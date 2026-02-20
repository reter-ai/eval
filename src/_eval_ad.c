/*  _eval_ad.c -- Automatic differentiation: Var, Dual, Tensor, Tape  */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <chibi/eval.h>
#include "_eval_ad.h"

/* ================================================================
 * Global tape (thread-local in pool workers, global in main)
 * ================================================================ */

#ifdef _WIN32
static __declspec(thread) ADTape tape_storage = {NULL, 0, 0, 1};
#else
static __thread ADTape tape_storage = {NULL, 0, 0, 1};
#endif

ADTape *ad_global_tape(void) {
    return &tape_storage;
}

sexp_tag_t ad_var_type_tag = 0;
sexp_tag_t ad_dual_type_tag = 0;
sexp_tag_t ad_tensor_type_tag = 0;

/* ================================================================
 * Tape operations
 * ================================================================ */

static int tape_push(ADTape *t, TapeEntry e) {
    if (t->count >= t->capacity) {
        int newcap = t->capacity < 64 ? 64 : t->capacity * 2;
        TapeEntry *ne = (TapeEntry *)realloc(t->entries, newcap * sizeof(TapeEntry));
        if (!ne) return -1;
        t->entries = ne;
        t->capacity = newcap;
    }
    t->entries[t->count] = e;
    return t->count++;
}

static void tape_zero_grad(ADTape *t) {
    for (int i = 0; i < t->count; i++) {
        t->entries[i].grad = 0.0;
        if (t->entries[i].tgrad) {
            memset(t->entries[i].tgrad, 0,
                   t->entries[i].size * sizeof(double));
        }
    }
}

static void tape_reset(ADTape *t) {
    for (int i = 0; i < t->count; i++) {
        free(t->entries[i].tdata);
        free(t->entries[i].tgrad);
        free(t->entries[i].shape);
    }
    free(t->entries);
    t->entries = NULL;
    t->count = 0;
    t->capacity = 0;
    t->enabled = 1;
}

/* ================================================================
 * Native tensor operations
 * ================================================================ */

static int shape_total(const int *shape, int ndim) {
    int total = 1;
    for (int i = 0; i < ndim; i++) total *= shape[i];
    return total;
}

static int *shape_dup(const int *shape, int ndim) {
    int *s = (int *)malloc(ndim * sizeof(int));
    memcpy(s, shape, ndim * sizeof(int));
    return s;
}

static double *tensor_alloc(int size) {
    double *d = (double *)calloc(size, sizeof(double));
    return d;
}

/* ================================================================
 * Dispatch table — defaults to native C, replaced by TF if available
 * ================================================================ */

ADTensorOps ad_tensor_ops;

/* Element-wise binary ops with broadcasting.
 * Simple: if sizes match, element-wise; if one is scalar (size 1),
 * broadcast. */
void ad_native_binary_ew(const double *a, int sa,
                         const double *b, int sb,
                         double *out, int sout,
                         ADOpKind op) {
    for (int i = 0; i < sout; i++) {
        double va = a[i % sa];
        double vb = b[i % sb];
        switch (op) {
        case AD_OP_ADD: out[i] = va + vb; break;
        case AD_OP_SUB: out[i] = va - vb; break;
        case AD_OP_MUL: out[i] = va * vb; break;
        case AD_OP_DIV: out[i] = va / vb; break;
        case AD_OP_POW: out[i] = pow(va, vb); break;
        default: out[i] = 0; break;
        }
    }
}

/* Matrix multiply: A[m,k] @ B[k,n] -> C[m,n] */
void ad_native_matmul(const double *a, const double *b, double *c,
                      int m, int k, int n) {
    memset(c, 0, m * n * sizeof(double));
    for (int i = 0; i < m; i++)
        for (int j = 0; j < n; j++)
            for (int p = 0; p < k; p++)
                c[i * n + j] += a[i * k + p] * b[p * n + j];
}

/* Transpose: A[m,n] -> B[n,m] */
void ad_native_transpose(const double *a, double *b, int m, int n) {
    for (int i = 0; i < m; i++)
        for (int j = 0; j < n; j++)
            b[j * m + i] = a[i * n + j];
}

/* Unary element-wise */
void ad_native_unary_ew(const double *a, double *out, int size, ADOpKind op) {
    for (int i = 0; i < size; i++) {
        double v = a[i];
        switch (op) {
        case AD_OP_NEG:     out[i] = -v; break;
        case AD_OP_SIN:     out[i] = sin(v); break;
        case AD_OP_COS:     out[i] = cos(v); break;
        case AD_OP_TAN:     out[i] = tan(v); break;
        case AD_OP_EXP:     out[i] = exp(v); break;
        case AD_OP_LOG:     out[i] = log(v); break;
        case AD_OP_SQRT:    out[i] = sqrt(v); break;
        case AD_OP_TANH:    out[i] = tanh(v); break;
        case AD_OP_SIGMOID: { double s = 1.0 / (1.0 + exp(-v)); out[i] = s; } break;
        case AD_OP_RELU:    out[i] = v > 0 ? v : 0; break;
        case AD_OP_ABS:     out[i] = fabs(v); break;
        default:            out[i] = v; break;
        }
    }
}

/* ================================================================
 * Backward pass
 * ================================================================ */

void ad_native_backward(ADTape *t, int from_idx) {
    if (from_idx < 0 || from_idx >= t->count) return;

    TapeEntry *e = &t->entries[from_idx];

    /* Seed gradient */
    if (e->is_tensor) {
        if (!e->tgrad) {
            e->tgrad = tensor_alloc(e->size);
        }
        for (int i = 0; i < e->size; i++) e->tgrad[i] = 1.0;
    } else {
        e->grad = 1.0;
    }

    /* Reverse sweep */
    for (int i = from_idx; i >= 0; i--) {
        TapeEntry *cur = &t->entries[i];
        if (!cur->is_tensor && cur->grad == 0.0) continue;
        if (cur->is_tensor && (!cur->tgrad || cur->op == AD_OP_LEAF)) {
            /* Leaf tensors keep their gradient */
            if (cur->op != AD_OP_LEAF) continue;
            if (!cur->tgrad) continue;
            /* Check if all zeros */
            int all_zero = 1;
            for (int k = 0; k < cur->size; k++) {
                if (cur->tgrad[k] != 0.0) { all_zero = 0; break; }
            }
            if (all_zero) continue;
        }

        int a0 = cur->arg0, a1 = cur->arg1;

        if (!cur->is_tensor) {
            /* Scalar backward */
            double g = cur->grad;
            switch (cur->op) {
            case AD_OP_LEAF: break;
            case AD_OP_ADD:
                if (a0 >= 0) t->entries[a0].grad += g;
                if (a1 >= 0) t->entries[a1].grad += g;
                break;
            case AD_OP_SUB:
                if (a0 >= 0) t->entries[a0].grad += g;
                if (a1 >= 0) t->entries[a1].grad -= g;
                break;
            case AD_OP_MUL:
                if (a0 >= 0) t->entries[a0].grad += g * cur->cached1;
                if (a1 >= 0) t->entries[a1].grad += g * cur->cached0;
                break;
            case AD_OP_DIV:
                if (a0 >= 0) t->entries[a0].grad += g / cur->cached1;
                if (a1 >= 0) t->entries[a1].grad -= g * cur->cached0 / (cur->cached1 * cur->cached1);
                break;
            case AD_OP_POW:
                /* d/da(a^b) = b * a^(b-1), d/db(a^b) = a^b * ln(a) */
                if (a0 >= 0) t->entries[a0].grad += g * cur->cached1 * pow(cur->cached0, cur->cached1 - 1.0);
                if (a1 >= 0 && cur->cached0 > 0) t->entries[a1].grad += g * cur->value * log(cur->cached0);
                break;
            case AD_OP_NEG:
                if (a0 >= 0) t->entries[a0].grad -= g;
                break;
            case AD_OP_SIN:
                if (a0 >= 0) t->entries[a0].grad += g * cos(t->entries[a0].value);
                break;
            case AD_OP_COS:
                if (a0 >= 0) t->entries[a0].grad -= g * sin(t->entries[a0].value);
                break;
            case AD_OP_TAN:
                if (a0 >= 0) { double c = cos(t->entries[a0].value); t->entries[a0].grad += g / (c * c); }
                break;
            case AD_OP_EXP:
                if (a0 >= 0) t->entries[a0].grad += g * cur->value;
                break;
            case AD_OP_LOG:
                if (a0 >= 0) t->entries[a0].grad += g / t->entries[a0].value;
                break;
            case AD_OP_SQRT:
                if (a0 >= 0) t->entries[a0].grad += g / (2.0 * cur->value);
                break;
            case AD_OP_TANH:
                if (a0 >= 0) { double th = cur->value; t->entries[a0].grad += g * (1.0 - th * th); }
                break;
            case AD_OP_SIGMOID:
                if (a0 >= 0) { double s = cur->value; t->entries[a0].grad += g * s * (1.0 - s); }
                break;
            case AD_OP_RELU:
                if (a0 >= 0) t->entries[a0].grad += t->entries[a0].value > 0 ? g : 0;
                break;
            case AD_OP_ABS:
                if (a0 >= 0) t->entries[a0].grad += t->entries[a0].value >= 0 ? g : -g;
                break;
            default: break;
            }
        } else {
            /* Tensor backward */
            double *g = cur->tgrad;
            if (!g) continue;

            switch (cur->op) {
            case AD_OP_LEAF: break;
            case AD_OP_ADD:
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    int s0 = t->entries[a0].size;
                    for (int k = 0; k < s0; k++)
                        t->entries[a0].tgrad[k] += g[k % cur->size];
                }
                if (a1 >= 0 && t->entries[a1].tgrad) {
                    int s1 = t->entries[a1].size;
                    for (int k = 0; k < s1; k++)
                        t->entries[a1].tgrad[k] += g[k % cur->size];
                }
                break;
            case AD_OP_SUB:
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    int s0 = t->entries[a0].size;
                    for (int k = 0; k < s0; k++)
                        t->entries[a0].tgrad[k] += g[k % cur->size];
                }
                if (a1 >= 0 && t->entries[a1].tgrad) {
                    int s1 = t->entries[a1].size;
                    for (int k = 0; k < s1; k++)
                        t->entries[a1].tgrad[k] -= g[k % cur->size];
                }
                break;
            case AD_OP_MUL:
                /* Element-wise: d/da = g * b, d/db = g * a */
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    TapeEntry *e1 = &t->entries[a1];
                    int s0 = t->entries[a0].size;
                    for (int k = 0; k < s0; k++)
                        t->entries[a0].tgrad[k] += g[k % cur->size] * e1->tdata[k % e1->size];
                }
                if (a1 >= 0 && t->entries[a1].tgrad) {
                    TapeEntry *e0 = &t->entries[a0];
                    int s1 = t->entries[a1].size;
                    for (int k = 0; k < s1; k++)
                        t->entries[a1].tgrad[k] += g[k % cur->size] * e0->tdata[k % e0->size];
                }
                break;
            case AD_OP_DIV:
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    TapeEntry *e1 = &t->entries[a1];
                    int s0 = t->entries[a0].size;
                    for (int k = 0; k < s0; k++)
                        t->entries[a0].tgrad[k] += g[k % cur->size] / e1->tdata[k % e1->size];
                }
                if (a1 >= 0 && t->entries[a1].tgrad) {
                    TapeEntry *e0 = &t->entries[a0];
                    TapeEntry *e1 = &t->entries[a1];
                    int s1 = t->entries[a1].size;
                    for (int k = 0; k < s1; k++) {
                        double bv = e1->tdata[k % e1->size];
                        t->entries[a1].tgrad[k] -= g[k % cur->size] * e0->tdata[k % e0->size] / (bv * bv);
                    }
                }
                break;
            case AD_OP_MATMUL: {
                /* C = A @ B, dA = g @ B^T, dB = A^T @ g */
                TapeEntry *ea = &t->entries[a0];
                TapeEntry *eb = &t->entries[a1];
                int m = ea->shape[0], k = ea->shape[1], n = eb->shape[1];
                if (a0 >= 0 && ea->tgrad) {
                    /* dA = g @ B^T */
                    double *bt = tensor_alloc(k * n);
                    ad_tensor_ops.transpose(eb->tdata, bt, k, n);
                    double *da = tensor_alloc(m * k);
                    ad_tensor_ops.matmul(g, bt, da, m, n, k);
                    for (int j = 0; j < m * k; j++) ea->tgrad[j] += da[j];
                    free(bt);
                    free(da);
                }
                if (a1 >= 0 && eb->tgrad) {
                    /* dB = A^T @ g */
                    double *at = tensor_alloc(k * m);
                    ad_tensor_ops.transpose(ea->tdata, at, m, k);
                    double *db = tensor_alloc(k * n);
                    ad_tensor_ops.matmul(at, g, db, k, m, n);
                    for (int j = 0; j < k * n; j++) eb->tgrad[j] += db[j];
                    free(at);
                    free(db);
                }
                break;
            }
            case AD_OP_NEG:
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    for (int k = 0; k < t->entries[a0].size; k++)
                        t->entries[a0].tgrad[k] -= g[k];
                }
                break;
            case AD_OP_SIN:
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    TapeEntry *e0 = &t->entries[a0];
                    for (int k = 0; k < e0->size; k++)
                        e0->tgrad[k] += g[k] * cos(e0->tdata[k]);
                }
                break;
            case AD_OP_COS:
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    TapeEntry *e0 = &t->entries[a0];
                    for (int k = 0; k < e0->size; k++)
                        e0->tgrad[k] -= g[k] * sin(e0->tdata[k]);
                }
                break;
            case AD_OP_EXP:
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    for (int k = 0; k < t->entries[a0].size; k++)
                        t->entries[a0].tgrad[k] += g[k] * cur->tdata[k];
                }
                break;
            case AD_OP_LOG:
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    TapeEntry *e0 = &t->entries[a0];
                    for (int k = 0; k < e0->size; k++)
                        e0->tgrad[k] += g[k] / e0->tdata[k];
                }
                break;
            case AD_OP_SQRT:
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    for (int k = 0; k < t->entries[a0].size; k++)
                        t->entries[a0].tgrad[k] += g[k] / (2.0 * cur->tdata[k]);
                }
                break;
            case AD_OP_TANH:
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    for (int k = 0; k < t->entries[a0].size; k++) {
                        double th = cur->tdata[k];
                        t->entries[a0].tgrad[k] += g[k] * (1.0 - th * th);
                    }
                }
                break;
            case AD_OP_SIGMOID:
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    for (int k = 0; k < t->entries[a0].size; k++) {
                        double s = cur->tdata[k];
                        t->entries[a0].tgrad[k] += g[k] * s * (1.0 - s);
                    }
                }
                break;
            case AD_OP_RELU:
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    TapeEntry *e0 = &t->entries[a0];
                    for (int k = 0; k < e0->size; k++)
                        e0->tgrad[k] += e0->tdata[k] > 0 ? g[k] : 0;
                }
                break;
            case AD_OP_SUM:
                /* g is scalar (size 1), broadcast to input shape */
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    double gv = g[0];
                    for (int k = 0; k < t->entries[a0].size; k++)
                        t->entries[a0].tgrad[k] += gv;
                }
                break;
            case AD_OP_MEAN:
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    int n = t->entries[a0].size;
                    double gv = g[0] / n;
                    for (int k = 0; k < n; k++)
                        t->entries[a0].tgrad[k] += gv;
                }
                break;
            case AD_OP_TRANSPOSE: {
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    TapeEntry *e0 = &t->entries[a0];
                    int m = e0->shape[0], n = e0->shape[1];
                    /* cur is n x m, g is n x m, need to transpose g back to m x n */
                    double *gt = tensor_alloc(m * n);
                    ad_tensor_ops.transpose(g, gt, n, m);
                    for (int k = 0; k < m * n; k++)
                        e0->tgrad[k] += gt[k];
                    free(gt);
                }
                break;
            }
            default: break;
            }
        }
    }
}

/* ================================================================
 * Type registration
 * ================================================================ */

static sexp var_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    return SEXP_VOID;
}

static sexp tensor_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    return SEXP_VOID;
}

void register_ad_types(sexp ctx) {
    /* Initialize dispatch table with native C defaults */
    ad_tensor_ops.binary_ew = ad_native_binary_ew;
    ad_tensor_ops.matmul    = ad_native_matmul;
    ad_tensor_ops.transpose = ad_native_transpose;
    ad_tensor_ops.unary_ew  = ad_native_unary_ew;
    ad_tensor_ops.backward  = ad_native_backward;

#ifdef EVAL_HAVE_TF
    eval_tf_init();
#endif

    sexp_gc_var2(name, type);
    sexp_gc_preserve2(ctx, name, type);

    /* Var type: wraps tape index (stored as int in cpointer value) */
    name = sexp_c_string(ctx, "ad-var", -1);
    type = sexp_register_c_type(ctx, name, var_finalize);
    if (sexp_typep(type)) {
        ad_var_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }

    /* Dual type: 2 GC-traced slots (val, dot) — supports nested duals */
    {
        sexp_gc_var2(slots, sym);
        sexp_gc_preserve2(ctx, slots, sym);
        sym = sexp_intern(ctx, "dot", -1);
        slots = sexp_cons(ctx, sym, SEXP_NULL);
        sym = sexp_intern(ctx, "val", -1);
        slots = sexp_cons(ctx, sym, slots);
        name = sexp_c_string(ctx, "ad-dual", -1);
        type = sexp_register_simple_type(ctx, name, SEXP_FALSE, slots);
        if (sexp_typep(type)) {
            ad_dual_type_tag = sexp_type_tag(type);
            sexp_preserve_object(ctx, type);
        }
        sexp_gc_release2(ctx);
    }

    /* Tensor type: wraps tape index */
    name = sexp_c_string(ctx, "ad-tensor", -1);
    type = sexp_register_c_type(ctx, name, tensor_finalize);
    if (sexp_typep(type)) {
        ad_tensor_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }

    sexp_gc_release2(ctx);
}

/* ================================================================
 * Var wrapping: cpointer value stores the tape index as (void*)(intptr_t)
 * ================================================================ */

static sexp wrap_var(sexp ctx, int tape_idx) {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);
    result = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), ad_var_type_tag);
    sexp_cpointer_value(result) = (void *)(intptr_t)tape_idx;
    sexp_cpointer_length(result) = 0;
    sexp_gc_release1(ctx);
    return result;
}

static int unwrap_var(sexp x) {
    return (int)(intptr_t)sexp_cpointer_value(x);
}

/* Dual: 2-slot simple type — val and dot can be any sexp (including nested duals) */

static sexp wrap_dual_sexp(sexp ctx, sexp val, sexp dot) {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);
    result = sexp_alloc_type(ctx, pair, ad_dual_type_tag);
    sexp_slot_ref(result, 0) = val;
    sexp_slot_ref(result, 1) = dot;
    sexp_gc_release1(ctx);
    return result;
}

static sexp dual_val(sexp x) { return sexp_slot_ref(x, 0); }
static sexp dual_dot(sexp x) { return sexp_slot_ref(x, 1); }

/* Tensor wrapping */
static sexp wrap_tensor(sexp ctx, int tape_idx) {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);
    result = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), ad_tensor_type_tag);
    sexp_cpointer_value(result) = (void *)(intptr_t)tape_idx;
    sexp_cpointer_length(result) = 0;
    sexp_gc_release1(ctx);
    return result;
}

static int unwrap_tensor(sexp x) {
    return (int)(intptr_t)sexp_cpointer_value(x);
}

/* ================================================================
 * Helper: extract scalar value from sexp (number, var, or tensor)
 * ================================================================ */

static double sexp_to_double(sexp x) {
    if (sexp_flonump(x)) return sexp_flonum_value(x);
    if (sexp_fixnump(x)) return (double)sexp_unbox_fixnum(x);
    if (ad_varp(x)) {
        ADTape *t = ad_global_tape();
        int idx = unwrap_var(x);
        return t->entries[idx].value;
    }
    return 0.0;
}

/* ================================================================
 * Bridge: Scalar Var operations
 * ================================================================ */

/* __ad_make_var__(value) -> Var */
static sexp bridge_ad_make_var(sexp ctx, sexp self, sexp_sint_t n, sexp val) {
    double v = sexp_to_double(val);
    ADTape *t = ad_global_tape();
    TapeEntry e = {0};
    e.op = AD_OP_LEAF;
    e.arg0 = e.arg1 = -1;
    e.value = v;
    e.grad = 0.0;
    int idx = tape_push(t, e);
    return wrap_var(ctx, idx);
}

/* __ad_var_p__(x) -> bool */
static sexp bridge_ad_var_p(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    return ad_varp(x) ? SEXP_TRUE : SEXP_FALSE;
}

/* __ad_var_value__(var) -> flonum */
static sexp bridge_ad_var_value(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    if (!ad_varp(x)) return sexp_make_flonum(ctx, 0.0);
    ADTape *t = ad_global_tape();
    int idx = unwrap_var(x);
    return sexp_make_flonum(ctx, t->entries[idx].value);
}

/* __ad_var_grad__(var) -> flonum */
static sexp bridge_ad_var_grad(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    if (!ad_varp(x)) return sexp_make_flonum(ctx, 0.0);
    ADTape *t = ad_global_tape();
    int idx = unwrap_var(x);
    return sexp_make_flonum(ctx, t->entries[idx].grad);
}

/* __ad_var_set__(var, val) -> void */
static sexp bridge_ad_var_set(sexp ctx, sexp self, sexp_sint_t n,
                               sexp var, sexp val) {
    if (!ad_varp(var)) return SEXP_VOID;
    ADTape *t = ad_global_tape();
    int idx = unwrap_var(var);
    t->entries[idx].value = sexp_to_double(val);
    return SEXP_VOID;
}

/* __ad_binary__(a, b, op_int) -> Var
 * a and b are Vars (or will be ensured by scheme layer) */
static sexp bridge_ad_binary(sexp ctx, sexp self, sexp_sint_t n,
                              sexp a, sexp b, sexp op_s) {
    ADTape *t = ad_global_tape();
    int op = (int)sexp_unbox_fixnum(op_s);

    /* Get tape indices, creating leaf if needed */
    int ia, ib;
    double va, vb;
    if (ad_varp(a)) {
        ia = unwrap_var(a);
        va = t->entries[ia].value;
    } else {
        va = sexp_to_double(a);
        TapeEntry e = {0};
        e.op = AD_OP_LEAF;
        e.arg0 = e.arg1 = -1;
        e.value = va;
        ia = tape_push(t, e);
    }
    if (ad_varp(b)) {
        ib = unwrap_var(b);
        vb = t->entries[ib].value;
    } else {
        vb = sexp_to_double(b);
        TapeEntry e = {0};
        e.op = AD_OP_LEAF;
        e.arg0 = e.arg1 = -1;
        e.value = vb;
        ib = tape_push(t, e);
    }

    TapeEntry ne = {0};
    ne.op = (ADOpKind)op;
    ne.arg0 = ia;
    ne.arg1 = ib;
    ne.cached0 = va;
    ne.cached1 = vb;

    switch (ne.op) {
    case AD_OP_ADD: ne.value = va + vb; break;
    case AD_OP_SUB: ne.value = va - vb; break;
    case AD_OP_MUL: ne.value = va * vb; break;
    case AD_OP_DIV: ne.value = va / vb; break;
    case AD_OP_POW: ne.value = pow(va, vb); break;
    default: ne.value = 0; break;
    }

    int idx = tape_push(t, ne);
    return wrap_var(ctx, idx);
}

/* __ad_unary__(a, op_int) -> Var */
static sexp bridge_ad_unary(sexp ctx, sexp self, sexp_sint_t n,
                             sexp a, sexp op_s) {
    ADTape *t = ad_global_tape();
    int op = (int)sexp_unbox_fixnum(op_s);

    int ia;
    double va;
    if (ad_varp(a)) {
        ia = unwrap_var(a);
        va = t->entries[ia].value;
    } else {
        va = sexp_to_double(a);
        TapeEntry e = {0};
        e.op = AD_OP_LEAF;
        e.arg0 = e.arg1 = -1;
        e.value = va;
        ia = tape_push(t, e);
    }

    TapeEntry ne = {0};
    ne.op = (ADOpKind)op;
    ne.arg0 = ia;
    ne.arg1 = -1;

    switch (ne.op) {
    case AD_OP_NEG:     ne.value = -va; break;
    case AD_OP_SIN:     ne.value = sin(va); break;
    case AD_OP_COS:     ne.value = cos(va); break;
    case AD_OP_TAN:     ne.value = tan(va); break;
    case AD_OP_EXP:     ne.value = exp(va); break;
    case AD_OP_LOG:     ne.value = log(va); break;
    case AD_OP_SQRT:    ne.value = sqrt(va); break;
    case AD_OP_TANH:    ne.value = tanh(va); break;
    case AD_OP_SIGMOID: ne.value = 1.0 / (1.0 + exp(-va)); break;
    case AD_OP_RELU:    ne.value = va > 0 ? va : 0; break;
    case AD_OP_ABS:     ne.value = fabs(va); break;
    default:            ne.value = va; break;
    }

    int idx = tape_push(t, ne);
    return wrap_var(ctx, idx);
}

/* ================================================================
 * Bridge: Tensor operations
 * ================================================================ */

/* Helper: flatten nested list to double array, infer shape */
static int flatten_nested_list(sexp ctx, sexp lst, double **out_data,
                                int **out_shape, int *out_ndim) {
    /* Determine depth: keep going into car until we hit a non-pair */
    int ndim = 0;
    sexp probe = lst;
    int shape_buf[8];
    while (sexp_pairp(probe)) {
        int count = 0;
        sexp p = probe;
        while (sexp_pairp(p)) { count++; p = sexp_cdr(p); }
        if (ndim >= 8) return -1;
        shape_buf[ndim++] = count;
        probe = sexp_car(probe);
    }

    /* Calculate total size */
    int total = 1;
    for (int i = 0; i < ndim; i++) total *= shape_buf[i];

    *out_ndim = ndim;
    *out_shape = shape_dup(shape_buf, ndim);
    *out_data = (double *)malloc(total * sizeof(double));

    /* Flatten recursively */
    int idx = 0;
    /* For 1D: iterate the list */
    /* For 2D: iterate rows, then cols */
    /* Generic: recursive flatten */
    if (ndim == 1) {
        sexp p = lst;
        while (sexp_pairp(p)) {
            sexp v = sexp_car(p);
            (*out_data)[idx++] = sexp_to_double(v);
            p = sexp_cdr(p);
        }
    } else if (ndim == 2) {
        sexp row = lst;
        while (sexp_pairp(row)) {
            sexp col = sexp_car(row);
            while (sexp_pairp(col)) {
                (*out_data)[idx++] = sexp_to_double(sexp_car(col));
                col = sexp_cdr(col);
            }
            row = sexp_cdr(row);
        }
    } else {
        /* Fallback: just flatten everything we can find */
        /* Simple DFS */
        sexp stack[256];
        int sp = 0;
        stack[sp++] = lst;
        while (sp > 0) {
            sexp cur = stack[--sp];
            if (sexp_pairp(cur)) {
                /* Push in reverse so we process left-to-right */
                sexp items[256];
                int ni = 0;
                sexp p = cur;
                while (sexp_pairp(p) && ni < 256) {
                    items[ni++] = sexp_car(p);
                    p = sexp_cdr(p);
                }
                for (int i = ni - 1; i >= 0; i--) {
                    if (sp < 256) stack[sp++] = items[i];
                }
            } else {
                if (idx < total) (*out_data)[idx++] = sexp_to_double(cur);
            }
        }
    }

    return total;
}

/* Unflatten data to nested sexp list based on shape */
static sexp unflatten_to_list(sexp ctx, const double *data, const int *shape,
                               int ndim, int *pos) {
    if (ndim == 0) {
        return sexp_make_flonum(ctx, data[(*pos)++]);
    }
    if (ndim == 1) {
        sexp result = SEXP_NULL;
        sexp_gc_var1(tmp);
        sexp_gc_preserve1(ctx, tmp);
        /* Build in reverse */
        for (int i = shape[0] - 1; i >= 0; i--) {
            int save_pos = *pos;
            /* We need to compute the position for element i */
            /* Actually let's just build forward then reverse... */
            (void)save_pos;
        }
        /* Build forward with tail tracking */
        sexp head = SEXP_NULL, tail = SEXP_NULL;
        for (int i = 0; i < shape[0]; i++) {
            tmp = sexp_cons(ctx, sexp_make_flonum(ctx, data[(*pos)++]), SEXP_NULL);
            if (sexp_nullp(head)) {
                head = tmp;
                tail = tmp;
            } else {
                sexp_cdr(tail) = tmp;
                tail = tmp;
            }
        }
        sexp_gc_release1(ctx);
        return head;
    }

    sexp_gc_var2(row, cell);
    sexp_gc_preserve2(ctx, row, cell);
    sexp head = SEXP_NULL, tail = SEXP_NULL;
    for (int i = 0; i < shape[0]; i++) {
        row = unflatten_to_list(ctx, data, shape + 1, ndim - 1, pos);
        cell = sexp_cons(ctx, row, SEXP_NULL);
        if (sexp_nullp(head)) {
            head = cell;
            tail = cell;
        } else {
            sexp_cdr(tail) = cell;
            tail = cell;
        }
    }
    sexp_gc_release2(ctx);
    return head;
}

/* __ad_make_tensor__(data_list, is_param) -> Tensor */
static sexp bridge_ad_make_tensor(sexp ctx, sexp self, sexp_sint_t n,
                                   sexp data_list, sexp is_param) {
    double *data = NULL;
    int *shape = NULL;
    int ndim = 0;

    int total = flatten_nested_list(ctx, data_list, &data, &shape, &ndim);
    if (total < 0)
        return sexp_user_exception(ctx, self, "tensor: invalid data", data_list);

    ADTape *t = ad_global_tape();
    TapeEntry e = {0};
    e.op = AD_OP_LEAF;
    e.arg0 = e.arg1 = -1;
    e.is_tensor = 1;
    e.tdata = data;
    e.shape = shape;
    e.ndim = ndim;
    e.size = total;
    /* If is_param, allocate gradient buffer */
    if (is_param != SEXP_FALSE) {
        e.tgrad = tensor_alloc(total);
    }
    int idx = tape_push(t, e);
    return wrap_tensor(ctx, idx);
}

/* __ad_tensor_p__(x) -> bool */
static sexp bridge_ad_tensor_p(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    return ad_tensorp(x) ? SEXP_TRUE : SEXP_FALSE;
}

/* __ad_tensor_data__(tensor) -> nested list */
static sexp bridge_ad_tensor_data(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    if (!ad_tensorp(x)) return SEXP_NULL;
    ADTape *t = ad_global_tape();
    int idx = unwrap_tensor(x);
    TapeEntry *e = &t->entries[idx];
    int pos = 0;
    return unflatten_to_list(ctx, e->tdata, e->shape, e->ndim, &pos);
}

/* __ad_tensor_grad__(tensor) -> nested list or #f */
static sexp bridge_ad_tensor_grad(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    if (!ad_tensorp(x)) return SEXP_FALSE;
    ADTape *t = ad_global_tape();
    int idx = unwrap_tensor(x);
    TapeEntry *e = &t->entries[idx];
    if (!e->tgrad) return SEXP_FALSE;
    int pos = 0;
    return unflatten_to_list(ctx, e->tgrad, e->shape, e->ndim, &pos);
}

/* __ad_tensor_shape__(tensor) -> list of ints */
static sexp bridge_ad_tensor_shape(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    if (!ad_tensorp(x)) return SEXP_NULL;
    ADTape *t = ad_global_tape();
    int idx = unwrap_tensor(x);
    TapeEntry *e = &t->entries[idx];
    sexp result = SEXP_NULL;
    for (int i = e->ndim - 1; i >= 0; i--)
        result = sexp_cons(ctx, sexp_make_fixnum(e->shape[i]), result);
    return result;
}

/* __ad_tensor_item__(tensor) -> scalar (for 0-d or single-element) */
static sexp bridge_ad_tensor_item(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    if (!ad_tensorp(x)) return sexp_make_flonum(ctx, 0.0);
    ADTape *t = ad_global_tape();
    int idx = unwrap_tensor(x);
    TapeEntry *e = &t->entries[idx];
    if (e->size != 1)
        return sexp_user_exception(ctx, self, "tensor->item: not a scalar tensor", x);
    return sexp_make_flonum(ctx, e->tdata[0]);
}

/* __ad_tensor_binary__(a, b, op_int) -> Tensor */
static sexp bridge_ad_tensor_binary(sexp ctx, sexp self, sexp_sint_t n,
                                     sexp a, sexp b, sexp op_s) {
    ADTape *t = ad_global_tape();
    int op = (int)sexp_unbox_fixnum(op_s);
    int ia, ib;
    TapeEntry *ea, *eb;

    /* Ensure a is on tape */
    if (ad_tensorp(a)) {
        ia = unwrap_tensor(a);
    } else if (ad_varp(a)) {
        /* Scalar var -> 1-element tensor */
        int vidx = unwrap_var(a);
        double va = t->entries[vidx].value;
        double *d = tensor_alloc(1);
        d[0] = va;
        int *s = (int *)malloc(sizeof(int));
        s[0] = 1;
        TapeEntry e = {0};
        e.op = AD_OP_LEAF; e.arg0 = e.arg1 = -1;
        e.is_tensor = 1; e.tdata = d; e.tgrad = tensor_alloc(1);
        e.shape = s; e.ndim = 1; e.size = 1;
        ia = tape_push(t, e);
    } else {
        double va = sexp_to_double(a);
        double *d = tensor_alloc(1);
        d[0] = va;
        int *s = (int *)malloc(sizeof(int));
        s[0] = 1;
        TapeEntry e = {0};
        e.op = AD_OP_LEAF; e.arg0 = e.arg1 = -1;
        e.is_tensor = 1; e.tdata = d;
        e.shape = s; e.ndim = 1; e.size = 1;
        ia = tape_push(t, e);
    }
    ea = &t->entries[ia];

    /* Ensure b is on tape */
    if (ad_tensorp(b)) {
        ib = unwrap_tensor(b);
    } else if (ad_varp(b)) {
        int vidx = unwrap_var(b);
        double vb = t->entries[vidx].value;
        double *d = tensor_alloc(1);
        d[0] = vb;
        int *s = (int *)malloc(sizeof(int));
        s[0] = 1;
        TapeEntry e = {0};
        e.op = AD_OP_LEAF; e.arg0 = e.arg1 = -1;
        e.is_tensor = 1; e.tdata = d; e.tgrad = tensor_alloc(1);
        e.shape = s; e.ndim = 1; e.size = 1;
        ib = tape_push(t, e);
    } else {
        double vb = sexp_to_double(b);
        double *d = tensor_alloc(1);
        d[0] = vb;
        int *s = (int *)malloc(sizeof(int));
        s[0] = 1;
        TapeEntry e = {0};
        e.op = AD_OP_LEAF; e.arg0 = e.arg1 = -1;
        e.is_tensor = 1; e.tdata = d;
        e.shape = s; e.ndim = 1; e.size = 1;
        ib = tape_push(t, e);
    }
    /* Re-fetch after potential realloc */
    ea = &t->entries[ia];
    eb = &t->entries[ib];

    TapeEntry ne = {0};
    ne.op = (ADOpKind)op;
    ne.arg0 = ia;
    ne.arg1 = ib;
    ne.is_tensor = 1;

    if (op == AD_OP_MATMUL) {
        /* Matrix multiply: A[m,k] @ B[k,n] -> C[m,n] */
        if (ea->ndim < 2 || eb->ndim < 2)
            return sexp_user_exception(ctx, self, "matmul: need 2D tensors", a);
        int m = ea->shape[0], k = ea->shape[1], n2 = eb->shape[1];
        if (k != eb->shape[0])
            return sexp_user_exception(ctx, self, "matmul: shape mismatch", a);
        ne.size = m * n2;
        ne.ndim = 2;
        ne.shape = (int *)malloc(2 * sizeof(int));
        ne.shape[0] = m; ne.shape[1] = n2;
        ne.tdata = tensor_alloc(ne.size);
        ne.tgrad = tensor_alloc(ne.size);
        ad_tensor_ops.matmul(ea->tdata, eb->tdata, ne.tdata, m, k, n2);
    } else {
        /* Element-wise with broadcasting */
        int sout = ea->size >= eb->size ? ea->size : eb->size;
        ne.size = sout;
        if (ea->size >= eb->size) {
            ne.ndim = ea->ndim;
            ne.shape = shape_dup(ea->shape, ea->ndim);
        } else {
            ne.ndim = eb->ndim;
            ne.shape = shape_dup(eb->shape, eb->ndim);
        }
        ne.tdata = tensor_alloc(sout);
        ne.tgrad = tensor_alloc(sout);
        ad_tensor_ops.binary_ew(ea->tdata, ea->size, eb->tdata, eb->size,
                                ne.tdata, sout, (ADOpKind)op);
    }

    int idx = tape_push(t, ne);
    return wrap_tensor(ctx, idx);
}

/* __ad_tensor_unary__(tensor, op_int) -> Tensor */
static sexp bridge_ad_tensor_unary(sexp ctx, sexp self, sexp_sint_t n,
                                    sexp a, sexp op_s) {
    ADTape *t = ad_global_tape();
    int op = (int)sexp_unbox_fixnum(op_s);

    int ia;
    if (ad_tensorp(a)) {
        ia = unwrap_tensor(a);
    } else {
        return sexp_user_exception(ctx, self, "tensor_unary: expected tensor", a);
    }
    TapeEntry *ea = &t->entries[ia];

    if (op == AD_OP_TRANSPOSE) {
        if (ea->ndim != 2)
            return sexp_user_exception(ctx, self, "transpose: need 2D tensor", a);
        int m = ea->shape[0], nn = ea->shape[1];
        TapeEntry ne = {0};
        ne.op = AD_OP_TRANSPOSE;
        ne.arg0 = ia; ne.arg1 = -1;
        ne.is_tensor = 1;
        ne.ndim = 2;
        ne.shape = (int *)malloc(2 * sizeof(int));
        ne.shape[0] = nn; ne.shape[1] = m;
        ne.size = m * nn;
        ne.tdata = tensor_alloc(ne.size);
        ne.tgrad = tensor_alloc(ne.size);
        ad_tensor_ops.transpose(ea->tdata, ne.tdata, m, nn);
        int idx = tape_push(t, ne);
        return wrap_tensor(ctx, idx);
    }

    /* Element-wise unary */
    TapeEntry ne = {0};
    ne.op = (ADOpKind)op;
    ne.arg0 = ia; ne.arg1 = -1;
    ne.is_tensor = 1;
    ne.ndim = ea->ndim;
    ne.shape = shape_dup(ea->shape, ea->ndim);
    ne.size = ea->size;
    ne.tdata = tensor_alloc(ne.size);
    ne.tgrad = tensor_alloc(ne.size);
    ad_tensor_ops.unary_ew(ea->tdata, ne.tdata, ne.size, (ADOpKind)op);
    int idx = tape_push(t, ne);
    return wrap_tensor(ctx, idx);
}

/* __ad_tensor_reduce__(tensor, op_int) -> Tensor (scalar) */
static sexp bridge_ad_tensor_reduce(sexp ctx, sexp self, sexp_sint_t n,
                                     sexp a, sexp op_s) {
    ADTape *t = ad_global_tape();
    int op = (int)sexp_unbox_fixnum(op_s);

    int ia;
    if (ad_tensorp(a)) {
        ia = unwrap_tensor(a);
    } else {
        return sexp_user_exception(ctx, self, "tensor_reduce: expected tensor", a);
    }
    TapeEntry *ea = &t->entries[ia];

    double result = 0;
    if (op == AD_OP_SUM) {
        for (int i = 0; i < ea->size; i++) result += ea->tdata[i];
    } else if (op == AD_OP_MEAN) {
        for (int i = 0; i < ea->size; i++) result += ea->tdata[i];
        result /= ea->size;
    }

    TapeEntry ne = {0};
    ne.op = (ADOpKind)op;
    ne.arg0 = ia; ne.arg1 = -1;
    ne.is_tensor = 1;
    ne.ndim = 1;
    ne.shape = (int *)malloc(sizeof(int));
    ne.shape[0] = 1;
    ne.size = 1;
    ne.tdata = tensor_alloc(1);
    ne.tdata[0] = result;
    ne.tgrad = tensor_alloc(1);
    int idx = tape_push(t, ne);
    return wrap_tensor(ctx, idx);
}

/* ================================================================
 * Bridge: Dual number operations
 * ================================================================ */

/* __ad_make_dual__(val, dot) -> Dual — val/dot can be any sexp */
static sexp bridge_ad_make_dual(sexp ctx, sexp self, sexp_sint_t n,
                                 sexp val, sexp dot) {
    return wrap_dual_sexp(ctx, val, dot);
}

/* __ad_dual_p__(x) -> bool */
static sexp bridge_ad_dual_p(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    return ad_dualp(x) ? SEXP_TRUE : SEXP_FALSE;
}

/* __ad_dual_val__(dual) -> sexp (the primal value) */
static sexp bridge_ad_dual_val(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    if (!ad_dualp(x)) return x;
    return dual_val(x);
}

/* __ad_dual_dot__(dual) -> sexp (the tangent) */
static sexp bridge_ad_dual_dot(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    if (!ad_dualp(x)) return sexp_make_flonum(ctx, 0.0);
    return dual_dot(x);
}

/* ================================================================
 * Bridge: Tape control
 * ================================================================ */

/* __ad_backward__(node) -> void — reverse sweep from node */
static sexp bridge_ad_backward(sexp ctx, sexp self, sexp_sint_t n, sexp node) {
    ADTape *t = ad_global_tape();
    int idx;
    if (ad_varp(node)) {
        idx = unwrap_var(node);
    } else if (ad_tensorp(node)) {
        idx = unwrap_tensor(node);
    } else {
        return SEXP_VOID; /* Not a tracked node */
    }
    ad_tensor_ops.backward(t, idx);
    return SEXP_VOID;
}

/* __ad_zero_grad__() -> void */
static sexp bridge_ad_zero_grad(sexp ctx, sexp self, sexp_sint_t n) {
    tape_zero_grad(ad_global_tape());
    return SEXP_VOID;
}

/* __ad_tape_reset__() -> void */
static sexp bridge_ad_tape_reset(sexp ctx, sexp self, sexp_sint_t n) {
    tape_reset(ad_global_tape());
    return SEXP_VOID;
}

/* __ad_set_enabled__(bool) -> void */
static sexp bridge_ad_set_enabled(sexp ctx, sexp self, sexp_sint_t n, sexp val) {
    ad_global_tape()->enabled = (val != SEXP_FALSE) ? 1 : 0;
    return SEXP_VOID;
}

/* ================================================================
 * Registration
 * ================================================================ */

void register_ad_bridge_functions(sexp ctx, sexp env) {
    /* Scalar Var */
    sexp_define_foreign(ctx, env, "__ad_make_var__", 1, bridge_ad_make_var);
    sexp_define_foreign(ctx, env, "__ad_var_p__", 1, bridge_ad_var_p);
    sexp_define_foreign(ctx, env, "__ad_var_value__", 1, bridge_ad_var_value);
    sexp_define_foreign(ctx, env, "__ad_var_grad__", 1, bridge_ad_var_grad);
    sexp_define_foreign(ctx, env, "__ad_var_set__", 2, bridge_ad_var_set);
    sexp_define_foreign(ctx, env, "__ad_binary__", 3, bridge_ad_binary);
    sexp_define_foreign(ctx, env, "__ad_unary__", 2, bridge_ad_unary);

    /* Tensor */
    sexp_define_foreign(ctx, env, "__ad_make_tensor__", 2, bridge_ad_make_tensor);
    sexp_define_foreign(ctx, env, "__ad_tensor_p__", 1, bridge_ad_tensor_p);
    sexp_define_foreign(ctx, env, "__ad_tensor_data__", 1, bridge_ad_tensor_data);
    sexp_define_foreign(ctx, env, "__ad_tensor_grad__", 1, bridge_ad_tensor_grad);
    sexp_define_foreign(ctx, env, "__ad_tensor_shape__", 1, bridge_ad_tensor_shape);
    sexp_define_foreign(ctx, env, "__ad_tensor_item__", 1, bridge_ad_tensor_item);
    sexp_define_foreign(ctx, env, "__ad_tensor_binary__", 3, bridge_ad_tensor_binary);
    sexp_define_foreign(ctx, env, "__ad_tensor_unary__", 2, bridge_ad_tensor_unary);
    sexp_define_foreign(ctx, env, "__ad_tensor_reduce__", 2, bridge_ad_tensor_reduce);

    /* Dual */
    sexp_define_foreign(ctx, env, "__ad_make_dual__", 2, bridge_ad_make_dual);
    sexp_define_foreign(ctx, env, "__ad_dual_p__", 1, bridge_ad_dual_p);
    sexp_define_foreign(ctx, env, "__ad_dual_val__", 1, bridge_ad_dual_val);
    sexp_define_foreign(ctx, env, "__ad_dual_dot__", 1, bridge_ad_dual_dot);

    /* Tape control */
    sexp_define_foreign(ctx, env, "__ad_backward__", 1, bridge_ad_backward);
    sexp_define_foreign(ctx, env, "__ad_zero_grad__", 0, bridge_ad_zero_grad);
    sexp_define_foreign(ctx, env, "__ad_tape_reset__", 0, bridge_ad_tape_reset);
    sexp_define_foreign(ctx, env, "__ad_set_enabled__", 1, bridge_ad_set_enabled);

    /* Op-kind constants for Scheme layer */
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_LEAF", -1), sexp_make_fixnum(AD_OP_LEAF));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_ADD", -1), sexp_make_fixnum(AD_OP_ADD));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_SUB", -1), sexp_make_fixnum(AD_OP_SUB));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_MUL", -1), sexp_make_fixnum(AD_OP_MUL));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_DIV", -1), sexp_make_fixnum(AD_OP_DIV));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_POW", -1), sexp_make_fixnum(AD_OP_POW));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_NEG", -1), sexp_make_fixnum(AD_OP_NEG));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_SIN", -1), sexp_make_fixnum(AD_OP_SIN));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_COS", -1), sexp_make_fixnum(AD_OP_COS));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_TAN", -1), sexp_make_fixnum(AD_OP_TAN));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_EXP", -1), sexp_make_fixnum(AD_OP_EXP));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_LOG", -1), sexp_make_fixnum(AD_OP_LOG));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_SQRT", -1), sexp_make_fixnum(AD_OP_SQRT));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_TANH", -1), sexp_make_fixnum(AD_OP_TANH));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_SIGMOID", -1), sexp_make_fixnum(AD_OP_SIGMOID));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_RELU", -1), sexp_make_fixnum(AD_OP_RELU));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_ABS", -1), sexp_make_fixnum(AD_OP_ABS));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_MATMUL", -1), sexp_make_fixnum(AD_OP_MATMUL));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_SUM", -1), sexp_make_fixnum(AD_OP_SUM));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_MEAN", -1), sexp_make_fixnum(AD_OP_MEAN));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_TRANSPOSE", -1), sexp_make_fixnum(AD_OP_TRANSPOSE));
}
