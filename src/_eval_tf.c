/*  _eval_tf.c -- TensorFlow backend: graph-mode backward + TFE eager forward
 *
 *  Only compiled when EVAL_HAVE_TF=1 (TensorFlow C API detected by CMake).
 *
 *  Architecture:
 *    Forward pass: optionally replaced by TFE eager ops (GPU-capable)
 *    Backward pass: entire tape → TF_Graph → TF_AddGradients → TF_SessionRun
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include <tensorflow/c/c_api.h>
#include <tensorflow/c/eager/c_api.h>

#include "_eval_ad.h"

/* ================================================================
 * Helpers
 * ================================================================ */

static void tf_check(TF_Status *s, const char *where) {
    if (TF_GetCode(s) != TF_OK) {
        fprintf(stderr, "[TF] %s: %s\n", where, TF_Message(s));
    }
}

/* Create a scalar (0-d) float64 TF_Tensor from a double */
static TF_Tensor *tf_scalar_tensor(double v) {
    TF_Tensor *t = TF_AllocateTensor(TF_DOUBLE, NULL, 0, sizeof(double));
    *(double *)TF_TensorData(t) = v;
    return t;
}

/* Create a shaped float64 TF_Tensor from flat data */
static TF_Tensor *tf_tensor_from_data(const double *data, const int *shape,
                                       int ndim, int size) {
    int64_t dims[8];
    for (int i = 0; i < ndim; i++) dims[i] = shape[i];
    TF_Tensor *t = TF_AllocateTensor(TF_DOUBLE, dims, ndim,
                                      size * sizeof(double));
    memcpy(TF_TensorData(t), data, size * sizeof(double));
    return t;
}

/* ================================================================
 * Graph-mode backward pass
 *
 * Translates the AD tape into a TF graph, calls TF_AddGradients,
 * and runs a single TF_SessionRun to compute all parameter gradients.
 * ================================================================ */

/* Helper: add a TF const node — dtype inferred from tensor */
static TF_Output tf_add_const(TF_Graph *g, TF_Status *s,
                               const char *name, TF_Tensor *val) {
    TF_OperationDescription *desc = TF_NewOperation(g, "Const", name);
    TF_SetAttrTensor(desc, "value", val, s);
    TF_SetAttrType(desc, "dtype", TF_TensorType(val));
    TF_Operation *op = TF_FinishOperation(desc, s);
    TF_Output out = {op, 0};
    return out;
}

/* Helper: add a Placeholder node */
static TF_Output tf_add_placeholder(TF_Graph *g, TF_Status *s,
                                     const char *name) {
    TF_OperationDescription *desc = TF_NewOperation(g, "Placeholder", name);
    TF_SetAttrType(desc, "dtype", TF_DOUBLE);
    TF_Operation *op = TF_FinishOperation(desc, s);
    TF_Output out = {op, 0};
    return out;
}

/* Helper: add a unary op node */
static TF_Output tf_add_unary(TF_Graph *g, TF_Status *s,
                               const char *op_type, const char *name,
                               TF_Output input) {
    TF_OperationDescription *desc = TF_NewOperation(g, op_type, name);
    TF_AddInput(desc, input);
    TF_Operation *op = TF_FinishOperation(desc, s);
    TF_Output out = {op, 0};
    return out;
}

/* Helper: add a binary op node */
static TF_Output tf_add_binary(TF_Graph *g, TF_Status *s,
                                const char *op_type, const char *name,
                                TF_Output x, TF_Output y) {
    TF_OperationDescription *desc = TF_NewOperation(g, op_type, name);
    TF_AddInput(desc, x);
    TF_AddInput(desc, y);
    TF_Operation *op = TF_FinishOperation(desc, s);
    TF_Output out = {op, 0};
    return out;
}

/* Map ADOpKind to TF op name for unary ops */
static const char *tf_unary_op_name(ADOpKind op) {
    switch (op) {
    case AD_OP_NEG:     return "Neg";
    case AD_OP_SIN:     return "Sin";
    case AD_OP_COS:     return "Cos";
    case AD_OP_TAN:     return "Tan";
    case AD_OP_EXP:     return "Exp";
    case AD_OP_LOG:     return "Log";
    case AD_OP_SQRT:    return "Sqrt";
    case AD_OP_TANH:    return "Tanh";
    case AD_OP_SIGMOID: return "Sigmoid";
    case AD_OP_RELU:    return "Relu";
    case AD_OP_ABS:     return "Abs";
    default:            return NULL;
    }
}

/* Map ADOpKind to TF op name for binary ops */
static const char *tf_binary_op_name(ADOpKind op) {
    switch (op) {
    case AD_OP_ADD: return "AddV2";
    case AD_OP_SUB: return "Sub";
    case AD_OP_MUL: return "Mul";
    case AD_OP_DIV: return "RealDiv";
    case AD_OP_POW: return "Pow";
    default:        return NULL;
    }
}

static void tf_backward(ADTape *t, int from_idx) {
    if (from_idx < 0 || from_idx >= t->count) return;

    /* Mark which leaves are reachable from the output (for gradient targets) */
    char *reachable = (char *)calloc(from_idx + 1, 1);
    reachable[from_idx] = 1;
    for (int i = from_idx; i >= 0; i--) {
        if (!reachable[i]) continue;
        TapeEntry *e = &t->entries[i];
        if (e->arg0 >= 0) reachable[e->arg0] = 1;
        if (e->arg1 >= 0) reachable[e->arg1] = 1;
    }

    TF_Status *status = TF_NewStatus();
    TF_Graph *graph = TF_NewGraph();

    /* Parallel arrays: one TF_Output per tape entry */
    TF_Output *outputs = (TF_Output *)calloc(from_idx + 1, sizeof(TF_Output));

    /* Track which tape entries are params for gradient targets */
    int *param_indices = (int *)malloc((from_idx + 1) * sizeof(int));
    int n_params = 0;

    /* Track Placeholder feeds */
    TF_Output *feed_outputs = (TF_Output *)malloc((from_idx + 1) * sizeof(TF_Output));
    TF_Tensor **feed_tensors = (TF_Tensor **)malloc((from_idx + 1) * sizeof(TF_Tensor *));
    int n_feeds = 0;

    char name_buf[64];
    int graph_ok = 1;  /* set to 0 on any TF op failure */

    /* Walk tape and build TF graph nodes */
    for (int i = 0; i <= from_idx && graph_ok; i++) {
        TapeEntry *e = &t->entries[i];
        snprintf(name_buf, sizeof(name_buf), "n%d", i);

        if (e->op == AD_OP_LEAF) {
            int is_param;
            if (e->is_tensor) {
                /* Tensor: param if tgrad buffer was allocated */
                is_param = (e->tgrad != NULL);
            } else {
                /* Scalar: param only if reachable from output
                 * (unreachable constants like "2.0" become Const nodes) */
                is_param = reachable[i];
            }

            if (is_param) {
                /* Param: use Placeholder so we can feed current values */
                outputs[i] = tf_add_placeholder(graph, status, name_buf);
                if (TF_GetCode(status) != TF_OK) { graph_ok = 0; break; }

                /* Prepare feed */
                TF_Tensor *ft;
                if (e->is_tensor) {
                    ft = tf_tensor_from_data(e->tdata, e->shape, e->ndim, e->size);
                } else {
                    ft = tf_scalar_tensor(e->value);
                }
                feed_outputs[n_feeds] = outputs[i];
                feed_tensors[n_feeds] = ft;
                n_feeds++;

                param_indices[n_params++] = i;
            } else {
                /* Constant: bake value into graph */
                TF_Tensor *ct;
                if (e->is_tensor) {
                    ct = tf_tensor_from_data(e->tdata, e->shape,
                                              e->ndim, e->size);
                } else {
                    ct = tf_scalar_tensor(e->value);
                }
                outputs[i] = tf_add_const(graph, status, name_buf, ct);
                TF_DeleteTensor(ct);
                if (TF_GetCode(status) != TF_OK) { graph_ok = 0; break; }
            }
            continue;
        }

        /* Non-leaf: map op to TF node */
        TF_Output a0_out = outputs[e->arg0];
        const char *uname = tf_unary_op_name(e->op);
        const char *bname = tf_binary_op_name(e->op);

        if (bname) {
            /* Binary op */
            TF_Output a1_out = outputs[e->arg1];
            outputs[i] = tf_add_binary(graph, status, bname, name_buf,
                                        a0_out, a1_out);
        } else if (uname) {
            /* Unary op */
            outputs[i] = tf_add_unary(graph, status, uname, name_buf, a0_out);
        } else if (e->op == AD_OP_MATMUL) {
            TF_Output a1_out = outputs[e->arg1];
            TF_OperationDescription *desc = TF_NewOperation(graph, "MatMul", name_buf);
            TF_AddInput(desc, a0_out);
            TF_AddInput(desc, a1_out);
            TF_Operation *op = TF_FinishOperation(desc, status);
            outputs[i] = (TF_Output){op, 0};
        } else if (e->op == AD_OP_TRANSPOSE) {
            /* Transpose with perm=[1,0] */
            char perm_name[64];
            snprintf(perm_name, sizeof(perm_name), "perm%d", i);
            int32_t perm_data[2] = {1, 0};
            int64_t perm_dims[1] = {2};
            TF_Tensor *perm_t = TF_AllocateTensor(TF_INT32, perm_dims, 1,
                                                    2 * sizeof(int32_t));
            memcpy(TF_TensorData(perm_t), perm_data, 2 * sizeof(int32_t));
            TF_Output perm_out = tf_add_const(graph, status, perm_name, perm_t);
            TF_DeleteTensor(perm_t);
            if (TF_GetCode(status) != TF_OK) { graph_ok = 0; break; }

            TF_OperationDescription *desc = TF_NewOperation(graph, "Transpose", name_buf);
            TF_AddInput(desc, a0_out);
            TF_AddInput(desc, perm_out);
            TF_Operation *op = TF_FinishOperation(desc, status);
            outputs[i] = (TF_Output){op, 0};
        } else if (e->op == AD_OP_SUM || e->op == AD_OP_MEAN) {
            /* Reduce over all axes */
            const char *reduce_op = (e->op == AD_OP_SUM) ? "Sum" : "Mean";
            /* Create reduction_indices const (int32) — need to know input rank */
            TapeEntry *inp = &t->entries[e->arg0];
            int rank = inp->is_tensor ? inp->ndim : 0;
            char axes_name[64];
            snprintf(axes_name, sizeof(axes_name), "axes%d", i);
            int64_t axes_dims[1] = {rank};
            TF_Tensor *axes_t = TF_AllocateTensor(TF_INT32, axes_dims, 1,
                                                    rank * sizeof(int32_t));
            {
                int32_t *axes_ptr = (int32_t *)TF_TensorData(axes_t);
                for (int j = 0; j < rank; j++) axes_ptr[j] = j;
            }
            TF_Output axes_out = tf_add_const(graph, status, axes_name, axes_t);
            TF_DeleteTensor(axes_t);
            if (TF_GetCode(status) != TF_OK) { graph_ok = 0; break; }

            TF_OperationDescription *desc = TF_NewOperation(graph, reduce_op, name_buf);
            TF_AddInput(desc, a0_out);
            TF_AddInput(desc, axes_out);
            TF_Operation *op = TF_FinishOperation(desc, status);
            outputs[i] = (TF_Output){op, 0};
        } else {
            /* Unsupported op — fall back */
            graph_ok = 0;
            break;
        }

        /* Check status after every op creation */
        if (TF_GetCode(status) != TF_OK) { graph_ok = 0; break; }
    }

    /* If graph construction failed, fall back to native backward */
    if (!graph_ok) {
        free(reachable);
        free(outputs);
        free(param_indices);
        for (int f = 0; f < n_feeds; f++) TF_DeleteTensor(feed_tensors[f]);
        free(feed_outputs);
        free(feed_tensors);
        TF_DeleteGraph(graph);
        TF_DeleteStatus(status);
        ad_native_backward(t, from_idx);
        return;
    }

    if (n_params == 0) {
        /* No params to differentiate — nothing to do */
        free(reachable);
        free(outputs);
        free(param_indices);
        for (int f = 0; f < n_feeds; f++) TF_DeleteTensor(feed_tensors[f]);
        free(feed_outputs);
        free(feed_tensors);
        TF_DeleteGraph(graph);
        TF_DeleteStatus(status);
        return;
    }

    /* Compute gradients: dy/dx for each param */
    TF_Output y_output = outputs[from_idx];
    TF_Output *x_outputs = (TF_Output *)malloc(n_params * sizeof(TF_Output));
    for (int i = 0; i < n_params; i++) {
        x_outputs[i] = outputs[param_indices[i]];
    }

    TF_Output *grad_outputs = (TF_Output *)malloc(n_params * sizeof(TF_Output));
    TF_AddGradients(graph, &y_output, 1, x_outputs, n_params,
                    NULL, status, grad_outputs);
    if (TF_GetCode(status) != TF_OK) {
        fprintf(stderr, "[TF] AddGradients failed: %s — falling back to native\n",
                TF_Message(status));
        free(outputs);
        free(param_indices);
        free(x_outputs);
        free(grad_outputs);
        for (int f = 0; f < n_feeds; f++) TF_DeleteTensor(feed_tensors[f]);
        free(feed_outputs);
        free(feed_tensors);
        TF_DeleteGraph(graph);
        TF_DeleteStatus(status);
        ad_native_backward(t, from_idx);
        return;
    }

    /* Create session and run */
    TF_SessionOptions *opts = TF_NewSessionOptions();
    TF_Session *sess = TF_NewSession(graph, opts, status);
    tf_check(status, "NewSession");
    TF_DeleteSessionOptions(opts);

    /* Fetch gradient outputs */
    TF_Tensor **fetch_results = (TF_Tensor **)calloc(n_params, sizeof(TF_Tensor *));

    TF_SessionRun(sess,
                  NULL,                           /* run options */
                  feed_outputs, feed_tensors, n_feeds,  /* feeds */
                  grad_outputs, fetch_results, n_params, /* fetches */
                  NULL, 0,                        /* targets */
                  NULL,                           /* run metadata */
                  status);

    if (TF_GetCode(status) != TF_OK) {
        fprintf(stderr, "[TF] SessionRun failed: %s — falling back to native\n",
                TF_Message(status));
        TF_CloseSession(sess, status);
        TF_DeleteSession(sess, status);
        free(outputs);
        free(param_indices);
        free(x_outputs);
        free(grad_outputs);
        for (int f = 0; f < n_feeds; f++) TF_DeleteTensor(feed_tensors[f]);
        free(feed_outputs);
        free(feed_tensors);
        free(fetch_results);
        TF_DeleteGraph(graph);
        TF_DeleteStatus(status);
        ad_native_backward(t, from_idx);
        return;
    }

    /* Copy gradient data back to tape entries */
    for (int i = 0; i < n_params; i++) {
        int tape_idx = param_indices[i];
        TapeEntry *e = &t->entries[tape_idx];
        TF_Tensor *gt = fetch_results[i];
        if (!gt) continue;

        const double *gdata = (const double *)TF_TensorData(gt);
        int gsize = (int)(TF_TensorByteSize(gt) / sizeof(double));

        if (e->is_tensor) {
            if (!e->tgrad) {
                e->tgrad = (double *)calloc(e->size, sizeof(double));
            }
            int copy_size = gsize < e->size ? gsize : e->size;
            for (int k = 0; k < copy_size; k++)
                e->tgrad[k] += gdata[k];
        } else {
            /* Scalar gradient */
            if (gsize >= 1)
                e->grad += gdata[0];
        }
    }

    /* Seed the output gradient (for consistency with native backward) */
    TapeEntry *out_e = &t->entries[from_idx];
    if (out_e->is_tensor) {
        if (!out_e->tgrad) {
            out_e->tgrad = (double *)calloc(out_e->size, sizeof(double));
        }
        for (int i = 0; i < out_e->size; i++) out_e->tgrad[i] = 1.0;
    } else {
        out_e->grad = 1.0;
    }

    /* Cleanup */
    for (int i = 0; i < n_params; i++) {
        if (fetch_results[i]) TF_DeleteTensor(fetch_results[i]);
    }
    for (int f = 0; f < n_feeds; f++) TF_DeleteTensor(feed_tensors[f]);
    TF_CloseSession(sess, status);
    TF_DeleteSession(sess, status);
    free(reachable);
    free(outputs);
    free(param_indices);
    free(x_outputs);
    free(grad_outputs);
    free(feed_outputs);
    free(feed_tensors);
    free(fetch_results);
    TF_DeleteGraph(graph);
    TF_DeleteStatus(status);
}

/* ================================================================
 * TFE eager forward ops
 *
 * Replace native C tensor ops with TFE eager execution for GPU
 * acceleration on larger tensors.  Falls back to native C for
 * tensors smaller than TF_SIZE_THRESHOLD elements.
 * ================================================================ */

#define TF_SIZE_THRESHOLD 64

/* Thread-local TFE context — lazily initialized */
#ifdef _WIN32
static __declspec(thread) TFE_Context *tfe_ctx = NULL;
#else
static __thread TFE_Context *tfe_ctx = NULL;
#endif

static TFE_Context *get_tfe_ctx(void) {
    if (tfe_ctx) return tfe_ctx;
    TF_Status *s = TF_NewStatus();
    TFE_ContextOptions *opts = TFE_NewContextOptions();
    tfe_ctx = TFE_NewContext(opts, s);
    if (TF_GetCode(s) != TF_OK) {
        fprintf(stderr, "[TFE] context init failed: %s\n", TF_Message(s));
        tfe_ctx = NULL;
    }
    TFE_DeleteContextOptions(opts);
    TF_DeleteStatus(s);
    return tfe_ctx;
}

/* Create a TFE_TensorHandle from flat double data + shape */
static TFE_TensorHandle *make_handle(const double *data, const int *shape,
                                      int ndim, int size) {
    int64_t dims[8];
    for (int i = 0; i < ndim; i++) dims[i] = shape[i];
    TF_Tensor *t = TF_AllocateTensor(TF_DOUBLE, dims, ndim,
                                      size * sizeof(double));
    memcpy(TF_TensorData(t), data, size * sizeof(double));
    TF_Status *s = TF_NewStatus();
    TFE_TensorHandle *h = TFE_NewTensorHandle(t, s);
    TF_DeleteTensor(t);
    TF_DeleteStatus(s);
    return h;
}

/* Read a TFE_TensorHandle back to flat double data */
static void read_handle(TFE_TensorHandle *h, double *out, int size) {
    TF_Status *s = TF_NewStatus();
    TF_Tensor *t = TFE_TensorHandleResolve(h, s);
    if (TF_GetCode(s) == TF_OK && t) {
        int available = (int)(TF_TensorByteSize(t) / sizeof(double));
        int copy = available < size ? available : size;
        memcpy(out, TF_TensorData(t), copy * sizeof(double));
        TF_DeleteTensor(t);
    }
    TF_DeleteStatus(s);
}

/* Execute a TFE op with 1 input, 1 output */
static TFE_TensorHandle *tfe_exec1(const char *op_name, TFE_TensorHandle *a) {
    TFE_Context *ctx = get_tfe_ctx();
    if (!ctx) return NULL;
    TF_Status *s = TF_NewStatus();
    TFE_Op *op = TFE_NewOp(ctx, op_name, s);
    if (TF_GetCode(s) != TF_OK) { TF_DeleteStatus(s); return NULL; }
    TFE_OpSetAttrType(op, "T", TF_DOUBLE);
    TFE_OpAddInputList(op, &a, 1, s);
    TFE_TensorHandle *result = NULL;
    int n_ret = 1;
    TFE_Execute(op, &result, &n_ret, s);
    TFE_DeleteOp(op);
    if (TF_GetCode(s) != TF_OK) {
        TF_DeleteStatus(s);
        return NULL;
    }
    TF_DeleteStatus(s);
    return result;
}

/* Execute a TFE op with 2 inputs, 1 output */
static TFE_TensorHandle *tfe_exec2(const char *op_name,
                                    TFE_TensorHandle *a,
                                    TFE_TensorHandle *b) {
    TFE_Context *ctx = get_tfe_ctx();
    if (!ctx) return NULL;
    TF_Status *s = TF_NewStatus();
    TFE_Op *op = TFE_NewOp(ctx, op_name, s);
    if (TF_GetCode(s) != TF_OK) { TF_DeleteStatus(s); return NULL; }
    TFE_OpSetAttrType(op, "T", TF_DOUBLE);
    TFE_OpAddInputList(op, &a, 1, s);
    TFE_OpAddInputList(op, &b, 1, s);
    TFE_TensorHandle *result = NULL;
    int n_ret = 1;
    TFE_Execute(op, &result, &n_ret, s);
    TFE_DeleteOp(op);
    if (TF_GetCode(s) != TF_OK) {
        TF_DeleteStatus(s);
        return NULL;
    }
    TF_DeleteStatus(s);
    return result;
}

/* ---- TFE matmul ---- */

static void tf_matmul(const double *a, const double *b, double *c,
                       int m, int k, int n) {
    if (m * k < TF_SIZE_THRESHOLD && k * n < TF_SIZE_THRESHOLD) {
        ad_native_matmul(a, b, c, m, k, n);
        return;
    }

    int shape_a[2] = {m, k};
    int shape_b[2] = {k, n};
    TFE_TensorHandle *ha = make_handle(a, shape_a, 2, m * k);
    TFE_TensorHandle *hb = make_handle(b, shape_b, 2, k * n);

    TFE_TensorHandle *hc = tfe_exec2("MatMul", ha, hb);

    if (hc) {
        read_handle(hc, c, m * n);
        TFE_DeleteTensorHandle(hc);
    } else {
        /* Fallback */
        ad_native_matmul(a, b, c, m, k, n);
    }

    TFE_DeleteTensorHandle(ha);
    TFE_DeleteTensorHandle(hb);
}

/* ---- TFE element-wise binary ---- */

static void tf_binary_ew(const double *a, int sa, const double *b, int sb,
                          double *out, int sout, ADOpKind op) {
    if (sout < TF_SIZE_THRESHOLD) {
        ad_native_binary_ew(a, sa, b, sb, out, sout, op);
        return;
    }

    const char *op_name = tf_binary_op_name(op);
    if (!op_name) {
        ad_native_binary_ew(a, sa, b, sb, out, sout, op);
        return;
    }

    /* For TFE, tensors need matching shapes or proper broadcasting.
     * Use 1-d shapes: sa and sb elements. TF handles broadcasting. */
    int shape_a[1] = {sa};
    int shape_b[1] = {sb};
    TFE_TensorHandle *ha = make_handle(a, shape_a, 1, sa);
    TFE_TensorHandle *hb = make_handle(b, shape_b, 1, sb);

    TFE_TensorHandle *hr = tfe_exec2(op_name, ha, hb);

    if (hr) {
        read_handle(hr, out, sout);
        TFE_DeleteTensorHandle(hr);
    } else {
        ad_native_binary_ew(a, sa, b, sb, out, sout, op);
    }

    TFE_DeleteTensorHandle(ha);
    TFE_DeleteTensorHandle(hb);
}

/* ---- TFE element-wise unary ---- */

static void tf_unary_ew(const double *a, double *out, int size, ADOpKind op) {
    if (size < TF_SIZE_THRESHOLD) {
        ad_native_unary_ew(a, out, size, op);
        return;
    }

    const char *op_name = tf_unary_op_name(op);
    if (!op_name) {
        ad_native_unary_ew(a, out, size, op);
        return;
    }

    int shape[1] = {size};
    TFE_TensorHandle *ha = make_handle(a, shape, 1, size);

    TFE_TensorHandle *hr = tfe_exec1(op_name, ha);

    if (hr) {
        read_handle(hr, out, size);
        TFE_DeleteTensorHandle(hr);
    } else {
        ad_native_unary_ew(a, out, size, op);
    }

    TFE_DeleteTensorHandle(ha);
}

/* ---- TFE transpose ---- */

static void tf_transpose(const double *a, double *b, int m, int n) {
    if (m * n < TF_SIZE_THRESHOLD) {
        ad_native_transpose(a, b, m, n);
        return;
    }

    TFE_Context *ctx = get_tfe_ctx();
    if (!ctx) {
        ad_native_transpose(a, b, m, n);
        return;
    }

    int shape_a[2] = {m, n};
    TFE_TensorHandle *ha = make_handle(a, shape_a, 2, m * n);

    /* Create perm tensor [1, 0] */
    int32_t perm_data[2] = {1, 0};
    int64_t perm_dims[1] = {2};
    TF_Tensor *perm_t = TF_AllocateTensor(TF_INT32, perm_dims, 1,
                                            2 * sizeof(int32_t));
    memcpy(TF_TensorData(perm_t), perm_data, 2 * sizeof(int32_t));
    TF_Status *s = TF_NewStatus();
    TFE_TensorHandle *hp = TFE_NewTensorHandle(perm_t, s);
    TF_DeleteTensor(perm_t);

    /* Execute Transpose */
    TFE_Op *op = TFE_NewOp(ctx, "Transpose", s);
    TFE_OpSetAttrType(op, "T", TF_DOUBLE);
    TFE_OpSetAttrType(op, "Tperm", TF_INT32);
    TFE_OpAddInputList(op, &ha, 1, s);
    TFE_OpAddInputList(op, &hp, 1, s);
    TFE_TensorHandle *hr = NULL;
    int n_ret = 1;
    TFE_Execute(op, &hr, &n_ret, s);
    TFE_DeleteOp(op);

    if (TF_GetCode(s) == TF_OK && hr) {
        read_handle(hr, b, m * n);
        TFE_DeleteTensorHandle(hr);
    } else {
        ad_native_transpose(a, b, m, n);
    }

    TFE_DeleteTensorHandle(ha);
    TFE_DeleteTensorHandle(hp);
    TF_DeleteStatus(s);
}

/* ================================================================
 * Init: replace dispatch table entries with TF versions
 * ================================================================ */

void eval_tf_init(void) {
    /* Replace backward with graph-mode version */
    ad_tensor_ops.backward  = tf_backward;
    /* Replace forward ops with TFE eager versions */
    ad_tensor_ops.matmul    = tf_matmul;
    ad_tensor_ops.binary_ew = tf_binary_ew;
    ad_tensor_ops.unary_ew  = tf_unary_ew;
    ad_tensor_ops.transpose = tf_transpose;
}
