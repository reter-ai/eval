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
        free(t->entries[i].aux);
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
 * im2col / col2im — helpers for Conv2D
 * ================================================================ */

/* Unfold one batch element [C_in, H, W] into column matrix
 * [C_in*kH*kW, H_out*W_out] for im2col+GEMM convolution. */
static void ad_im2col(const double *input, double *col,
                       int C_in, int H, int W,
                       int kH, int kW, int H_out, int W_out,
                       int stride_h, int stride_w,
                       int pad_h, int pad_w,
                       int dil_h, int dil_w) {
    int col_rows = C_in * kH * kW;
    int col_cols = H_out * W_out;
    for (int c = 0; c < C_in; c++) {
        for (int kh = 0; kh < kH; kh++) {
            for (int kw = 0; kw < kW; kw++) {
                int row = c * kH * kW + kh * kW + kw;
                for (int oh = 0; oh < H_out; oh++) {
                    for (int ow = 0; ow < W_out; ow++) {
                        int ih = oh * stride_h + kh * dil_h - pad_h;
                        int iw = ow * stride_w + kw * dil_w - pad_w;
                        int col_idx = row * col_cols + oh * W_out + ow;
                        if (ih >= 0 && ih < H && iw >= 0 && iw < W)
                            col[col_idx] = input[c * H * W + ih * W + iw];
                        else
                            col[col_idx] = 0.0;
                    }
                }
            }
        }
    }
    (void)col_rows;
}

/* Inverse scatter-add from [C_in*kH*kW, H_out*W_out] back to
 * [C_in, H, W] gradient. Same loop as im2col but accumulates. */
static void ad_col2im(const double *col, double *d_input,
                       int C_in, int H, int W,
                       int kH, int kW, int H_out, int W_out,
                       int stride_h, int stride_w,
                       int pad_h, int pad_w,
                       int dil_h, int dil_w) {
    int col_cols = H_out * W_out;
    for (int c = 0; c < C_in; c++) {
        for (int kh = 0; kh < kH; kh++) {
            for (int kw = 0; kw < kW; kw++) {
                int row = c * kH * kW + kh * kW + kw;
                for (int oh = 0; oh < H_out; oh++) {
                    for (int ow = 0; ow < W_out; ow++) {
                        int ih = oh * stride_h + kh * dil_h - pad_h;
                        int iw = ow * stride_w + kw * dil_w - pad_w;
                        if (ih >= 0 && ih < H && iw >= 0 && iw < W) {
                            int col_idx = row * col_cols + oh * W_out + ow;
                            d_input[c * H * W + ih * W + iw] += col[col_idx];
                        }
                    }
                }
            }
        }
    }
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
        case AD_OP_GELU: {
            /* GELU: 0.5 * x * (1 + tanh(sqrt(2/pi) * (x + 0.044715 * x^3))) */
            double x3 = v * v * v;
            double inner = 0.7978845608028654 * (v + 0.044715 * x3);
            double th = tanh(inner);
            out[i] = 0.5 * v * (1.0 + th);
            break;
        }
        case AD_OP_SILU: {
            /* SiLU: x * sigmoid(x) */
            double s = 1.0 / (1.0 + exp(-v));
            out[i] = v * s;
            break;
        }
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
            case AD_OP_GELU:
                if (a0 >= 0) {
                    double x = t->entries[a0].value;
                    double x3 = x * x * x;
                    double inner = 0.7978845608028654 * (x + 0.044715 * x3);
                    double th = tanh(inner);
                    double sech2 = 1.0 - th * th;
                    double d_inner = 0.7978845608028654 * (1.0 + 0.134145 * x * x);
                    t->entries[a0].grad += g * (0.5 * (1.0 + th) + 0.5 * x * sech2 * d_inner);
                }
                break;
            case AD_OP_SILU:
                if (a0 >= 0) {
                    double x = t->entries[a0].value;
                    double s = 1.0 / (1.0 + exp(-x));
                    t->entries[a0].grad += g * (s + x * s * (1.0 - s));
                }
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
            case AD_OP_GELU:
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    TapeEntry *e0 = &t->entries[a0];
                    for (int k = 0; k < e0->size; k++) {
                        double x = e0->tdata[k];
                        double x3 = x * x * x;
                        double inner = 0.7978845608028654 * (x + 0.044715 * x3);
                        double th = tanh(inner);
                        double sech2 = 1.0 - th * th;
                        double d_inner = 0.7978845608028654 * (1.0 + 0.134145 * x * x);
                        e0->tgrad[k] += g[k] * (0.5 * (1.0 + th) + 0.5 * x * sech2 * d_inner);
                    }
                }
                break;
            case AD_OP_SILU:
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    TapeEntry *e0 = &t->entries[a0];
                    for (int k = 0; k < e0->size; k++) {
                        double x = e0->tdata[k];
                        double s = 1.0 / (1.0 + exp(-x));
                        e0->tgrad[k] += g[k] * (s + x * s * (1.0 - s));
                    }
                }
                break;
            case AD_OP_SOFTMAX: {
                /* Backward: s_i * (g_i - sum_j(g_j * s_j)) per slice */
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    TapeEntry *e0 = &t->entries[a0];
                    int axis = cur->aux ? cur->aux[0] : (cur->ndim - 1);
                    int outer = 1, axis_size = cur->shape[axis], inner = 1;
                    for (int d = 0; d < axis; d++) outer *= cur->shape[d];
                    for (int d = axis + 1; d < cur->ndim; d++) inner *= cur->shape[d];
                    for (int o = 0; o < outer; o++) {
                        for (int j = 0; j < inner; j++) {
                            /* Compute dot = sum_k(g_k * s_k) for this slice */
                            double dot = 0.0;
                            for (int a = 0; a < axis_size; a++) {
                                int idx = (o * axis_size + a) * inner + j;
                                dot += g[idx] * cur->tdata[idx];
                            }
                            for (int a = 0; a < axis_size; a++) {
                                int idx = (o * axis_size + a) * inner + j;
                                e0->tgrad[idx] += cur->tdata[idx] * (g[idx] - dot);
                            }
                        }
                    }
                }
                break;
            }
            case AD_OP_SUM_AXIS: {
                /* Backward: broadcast gradient back along the reduced axis */
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    TapeEntry *e0 = &t->entries[a0];
                    int axis = cur->aux ? cur->aux[0] : 0;
                    int outer = 1, axis_size = e0->shape[axis], inner = 1;
                    for (int d = 0; d < axis; d++) outer *= e0->shape[d];
                    for (int d = axis + 1; d < e0->ndim; d++) inner *= e0->shape[d];
                    for (int o = 0; o < outer; o++) {
                        for (int j = 0; j < inner; j++) {
                            double gv = g[o * inner + j];
                            for (int a = 0; a < axis_size; a++) {
                                e0->tgrad[(o * axis_size + a) * inner + j] += gv;
                            }
                        }
                    }
                }
                break;
            }
            case AD_OP_MEAN_AXIS: {
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    TapeEntry *e0 = &t->entries[a0];
                    int axis = cur->aux ? cur->aux[0] : 0;
                    int outer = 1, axis_size = e0->shape[axis], inner = 1;
                    for (int d = 0; d < axis; d++) outer *= e0->shape[d];
                    for (int d = axis + 1; d < e0->ndim; d++) inner *= e0->shape[d];
                    double inv_n = 1.0 / axis_size;
                    for (int o = 0; o < outer; o++) {
                        for (int j = 0; j < inner; j++) {
                            double gv = g[o * inner + j] * inv_n;
                            for (int a = 0; a < axis_size; a++) {
                                e0->tgrad[(o * axis_size + a) * inner + j] += gv;
                            }
                        }
                    }
                }
                break;
            }
            case AD_OP_RESHAPE: {
                /* Backward: copy gradient flat (same layout, just different shape) */
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    for (int k = 0; k < t->entries[a0].size; k++)
                        t->entries[a0].tgrad[k] += g[k];
                }
                break;
            }
            case AD_OP_SLICE: {
                /* Backward: scatter gradient back to original positions */
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    TapeEntry *e0 = &t->entries[a0];
                    int ndim = e0->ndim;
                    int *begin = cur->aux;
                    int *size = cur->aux + ndim;
                    /* Iterate over output (slice) elements */
                    int out_size = cur->size;
                    for (int flat = 0; flat < out_size; flat++) {
                        /* Convert flat index to multi-dim in output shape */
                        int rem = flat;
                        int src_flat = 0;
                        int src_stride = 1;
                        /* Compute strides for input */
                        int strides[8];
                        strides[ndim - 1] = 1;
                        for (int d = ndim - 2; d >= 0; d--)
                            strides[d] = strides[d + 1] * e0->shape[d + 1];
                        /* Map output flat index to input flat index */
                        src_flat = 0;
                        for (int d = 0; d < ndim; d++) {
                            int out_stride = 1;
                            for (int dd = d + 1; dd < ndim; dd++) out_stride *= size[dd];
                            int coord = rem / out_stride;
                            rem %= out_stride;
                            src_flat += (begin[d] + coord) * strides[d];
                        }
                        e0->tgrad[src_flat] += g[flat];
                    }
                }
                break;
            }
            case AD_OP_CONCAT: {
                /* Backward: slice gradient into chunks for each input */
                if (!cur->aux) break;
                int axis = cur->aux[0];
                int n_inputs = cur->aux[1];
                int outer = 1, inner = 1;
                for (int d = 0; d < axis; d++) outer *= cur->shape[d];
                for (int d = axis + 1; d < cur->ndim; d++) inner *= cur->shape[d];
                int offset = 0;
                for (int inp = 0; inp < n_inputs; inp++) {
                    int ti = cur->aux[2 + inp];
                    if (ti >= 0 && t->entries[ti].tgrad) {
                        TapeEntry *ei = &t->entries[ti];
                        int ai_size = ei->shape[axis];
                        for (int o = 0; o < outer; o++) {
                            for (int a = 0; a < ai_size; a++) {
                                for (int j = 0; j < inner; j++) {
                                    int src_idx = (o * cur->shape[axis] + offset + a) * inner + j;
                                    int dst_idx = (o * ai_size + a) * inner + j;
                                    ei->tgrad[dst_idx] += g[src_idx];
                                }
                            }
                        }
                        offset += ai_size;
                    }
                }
                break;
            }
            case AD_OP_GATHER: {
                /* Backward: scatter-add gradient to indexed positions */
                if (a0 >= 0 && t->entries[a0].tgrad) {
                    TapeEntry *e0 = &t->entries[a0];
                    int axis = cur->aux[0];
                    int n_idx = cur->aux[1];
                    int outer = 1, inner = 1;
                    for (int d = 0; d < axis; d++) outer *= e0->shape[d];
                    for (int d = axis + 1; d < e0->ndim; d++) inner *= e0->shape[d];
                    for (int o = 0; o < outer; o++) {
                        for (int ii = 0; ii < n_idx; ii++) {
                            int src_a = cur->aux[2 + ii];
                            for (int j = 0; j < inner; j++) {
                                int g_idx = (o * n_idx + ii) * inner + j;
                                int s_idx = (o * e0->shape[axis] + src_a) * inner + j;
                                e0->tgrad[s_idx] += g[g_idx];
                            }
                        }
                    }
                }
                break;
            }
            case AD_OP_LAYER_NORM: {
                /* Backward for layer norm: grad flows to input, gamma, beta */
                if (!cur->aux) break;
                TapeEntry *input_e = &t->entries[a0];
                TapeEntry *gamma_e = (a1 >= 0) ? &t->entries[a1] : NULL;
                int beta_idx = cur->aux[0];
                TapeEntry *beta_e = (beta_idx >= 0) ? &t->entries[beta_idx] : NULL;
                double eps;
                memcpy(&eps, &cur->aux[1], sizeof(double));

                int last_dim = input_e->shape[input_e->ndim - 1];
                int n_slices = input_e->size / last_dim;

                for (int s = 0; s < n_slices; s++) {
                    const double *x_row = input_e->tdata + s * last_dim;
                    const double *g_row = g + s * last_dim;

                    /* Recompute mean, var for this slice */
                    double mean_v = 0.0;
                    for (int j = 0; j < last_dim; j++) mean_v += x_row[j];
                    mean_v /= last_dim;
                    double var_v = 0.0;
                    for (int j = 0; j < last_dim; j++) {
                        double d = x_row[j] - mean_v;
                        var_v += d * d;
                    }
                    var_v /= last_dim;
                    double inv_std = 1.0 / sqrt(var_v + eps);

                    /* Compute x_hat and accumulate */
                    double sum_dxhat = 0.0, sum_dxhat_xhat = 0.0;
                    for (int j = 0; j < last_dim; j++) {
                        double x_hat = (x_row[j] - mean_v) * inv_std;
                        double dxhat = g_row[j] * (gamma_e ? gamma_e->tdata[j] : 1.0);
                        sum_dxhat += dxhat;
                        sum_dxhat_xhat += dxhat * x_hat;
                    }

                    /* dx */
                    if (input_e->tgrad) {
                        for (int j = 0; j < last_dim; j++) {
                            double x_hat = (x_row[j] - mean_v) * inv_std;
                            double dxhat = g_row[j] * (gamma_e ? gamma_e->tdata[j] : 1.0);
                            input_e->tgrad[s * last_dim + j] +=
                                inv_std / last_dim * (last_dim * dxhat - sum_dxhat - x_hat * sum_dxhat_xhat);
                        }
                    }
                    /* dgamma */
                    if (gamma_e && gamma_e->tgrad) {
                        for (int j = 0; j < last_dim; j++) {
                            double x_hat = (x_row[j] - mean_v) * inv_std;
                            gamma_e->tgrad[j] += g_row[j] * x_hat;
                        }
                    }
                    /* dbeta */
                    if (beta_e && beta_e->tgrad) {
                        for (int j = 0; j < last_dim; j++) {
                            beta_e->tgrad[j] += g_row[j];
                        }
                    }
                }
                break;
            }
            case AD_OP_WHERE: {
                /* Backward: da += cond ? g : 0, db += cond ? 0 : g */
                TapeEntry *cond_e = &t->entries[a0];
                if (a1 >= 0 && t->entries[a1].tgrad) {
                    for (int k = 0; k < cur->size; k++) {
                        double c = cond_e->tdata[k % cond_e->size];
                        t->entries[a1].tgrad[k % t->entries[a1].size] += (c != 0.0) ? g[k] : 0.0;
                    }
                }
                if (cur->aux) {
                    int b_idx = cur->aux[0];
                    if (b_idx >= 0 && t->entries[b_idx].tgrad) {
                        TapeEntry *eb = &t->entries[b_idx];
                        for (int k = 0; k < cur->size; k++) {
                            double c = cond_e->tdata[k % cond_e->size];
                            eb->tgrad[k % eb->size] += (c == 0.0) ? g[k] : 0.0;
                        }
                    }
                }
                break;
            }
            case AD_OP_BATCH_MATMUL: {
                /* Batched: dA = dC @ B^T, dB = A^T @ dC per batch slice */
                if (a0 < 0 || a1 < 0) break;
                TapeEntry *ea = &t->entries[a0];
                TapeEntry *eb = &t->entries[a1];
                int nd = ea->ndim;
                int m = ea->shape[nd - 2], kk = ea->shape[nd - 1];
                int n2 = eb->shape[nd - 1];
                int batch = ea->size / (m * kk);
                for (int b = 0; b < batch; b++) {
                    const double *g_slice = g + b * m * n2;
                    if (ea->tgrad) {
                        double *bt = tensor_alloc(kk * n2);
                        ad_tensor_ops.transpose(eb->tdata + b * kk * n2, bt, kk, n2);
                        double *da = tensor_alloc(m * kk);
                        ad_tensor_ops.matmul(g_slice, bt, da, m, n2, kk);
                        for (int j = 0; j < m * kk; j++) ea->tgrad[b * m * kk + j] += da[j];
                        free(bt); free(da);
                    }
                    if (eb->tgrad) {
                        double *at = tensor_alloc(kk * m);
                        ad_tensor_ops.transpose(ea->tdata + b * m * kk, at, m, kk);
                        double *db = tensor_alloc(kk * n2);
                        ad_tensor_ops.matmul(at, g_slice, db, kk, m, n2);
                        for (int j = 0; j < kk * n2; j++) eb->tgrad[b * kk * n2 + j] += db[j];
                        free(at); free(db);
                    }
                }
                break;
            }
            case AD_OP_CONV2D: {
                /* Backward for Conv2D: dinput via col2im, dkernel via matmul */
                if (a0 < 0 || a1 < 0) break;
                TapeEntry *inp_e = &t->entries[a0];
                TapeEntry *ker_e = &t->entries[a1];
                int N = inp_e->shape[0], C_in = inp_e->shape[1];
                int H = inp_e->shape[2], W = inp_e->shape[3];
                int C_out = ker_e->shape[0];
                int kH = ker_e->shape[2], kW = ker_e->shape[3];
                int stride_h = cur->aux[0], stride_w = cur->aux[1];
                int pad_h = cur->aux[2], pad_w = cur->aux[3];
                int dil_h = cur->aux[4], dil_w = cur->aux[5];
                int H_out = (H + 2 * pad_h - dil_h * (kH - 1) - 1) / stride_h + 1;
                int W_out = (W + 2 * pad_w - dil_w * (kW - 1) - 1) / stride_w + 1;
                int col_rows = C_in * kH * kW;
                int col_cols = H_out * W_out;
                int inp_spatial = C_in * H * W;
                int out_spatial = C_out * H_out * W_out;
                for (int nn = 0; nn < N; nn++) {
                    const double *g_n = g + nn * out_spatial;
                    /* Recompute im2col for this batch element */
                    double *col = tensor_alloc(col_rows * col_cols);
                    ad_im2col(inp_e->tdata + nn * inp_spatial, col,
                              C_in, H, W, kH, kW, H_out, W_out,
                              stride_h, stride_w, pad_h, pad_w, dil_h, dil_w);
                    /* dkernel += g_n @ col^T */
                    if (ker_e->tgrad) {
                        double *col_T = tensor_alloc(col_cols * col_rows);
                        ad_tensor_ops.transpose(col, col_T, col_rows, col_cols);
                        double *dk = tensor_alloc(C_out * col_rows);
                        ad_tensor_ops.matmul(g_n, col_T, dk, C_out, col_cols, col_rows);
                        for (int j = 0; j < C_out * col_rows; j++)
                            ker_e->tgrad[j] += dk[j];
                        free(col_T); free(dk);
                    }
                    /* dinput: dcol = kernel^T @ g_n, then col2im */
                    if (inp_e->tgrad) {
                        double *ker_T = tensor_alloc(col_rows * C_out);
                        ad_tensor_ops.transpose(ker_e->tdata, ker_T, C_out, col_rows);
                        double *dcol = tensor_alloc(col_rows * col_cols);
                        ad_tensor_ops.matmul(ker_T, g_n, dcol, col_rows, C_out, col_cols);
                        ad_col2im(dcol, inp_e->tgrad + nn * inp_spatial,
                                  C_in, H, W, kH, kW, H_out, W_out,
                                  stride_h, stride_w, pad_h, pad_w, dil_h, dil_w);
                        free(ker_T); free(dcol);
                    }
                    free(col);
                }
                break;
            }
            case AD_OP_MAX_POOL2D: {
                /* Backward: route gradient to argmax positions */
                if (a0 < 0) break;
                TapeEntry *inp_e = &t->entries[a0];
                int N = inp_e->shape[0], C = inp_e->shape[1];
                int H = inp_e->shape[2], W = inp_e->shape[3];
                int kH = cur->aux[0], kW = cur->aux[1];
                int stride_h = cur->aux[2], stride_w = cur->aux[3];
                int pad_h = cur->aux[4], pad_w = cur->aux[5];
                int H_out = (H + 2 * pad_h - kH) / stride_h + 1;
                int W_out = (W + 2 * pad_w - kW) / stride_w + 1;
                if (!inp_e->tgrad) break;
                for (int nn = 0; nn < N; nn++) {
                    for (int c = 0; c < C; c++) {
                        for (int oh = 0; oh < H_out; oh++) {
                            for (int ow = 0; ow < W_out; ow++) {
                                /* Recompute argmax */
                                double best = -1e308;
                                int best_ih = 0, best_iw = 0;
                                for (int kh = 0; kh < kH; kh++) {
                                    for (int kw = 0; kw < kW; kw++) {
                                        int ih = oh * stride_h + kh - pad_h;
                                        int iw = ow * stride_w + kw - pad_w;
                                        if (ih >= 0 && ih < H && iw >= 0 && iw < W) {
                                            double v = inp_e->tdata[((nn * C + c) * H + ih) * W + iw];
                                            if (v > best) {
                                                best = v;
                                                best_ih = ih;
                                                best_iw = iw;
                                            }
                                        }
                                    }
                                }
                                int g_idx = ((nn * C + c) * H_out + oh) * W_out + ow;
                                inp_e->tgrad[((nn * C + c) * H + best_ih) * W + best_iw] += g[g_idx];
                            }
                        }
                    }
                }
                break;
            }
            case AD_OP_AVG_POOL2D: {
                /* Backward: distribute gradient equally to valid window positions */
                if (a0 < 0) break;
                TapeEntry *inp_e = &t->entries[a0];
                int N = inp_e->shape[0], C = inp_e->shape[1];
                int H = inp_e->shape[2], W = inp_e->shape[3];
                int kH = cur->aux[0], kW = cur->aux[1];
                int stride_h = cur->aux[2], stride_w = cur->aux[3];
                int pad_h = cur->aux[4], pad_w = cur->aux[5];
                int H_out = (H + 2 * pad_h - kH) / stride_h + 1;
                int W_out = (W + 2 * pad_w - kW) / stride_w + 1;
                if (!inp_e->tgrad) break;
                for (int nn = 0; nn < N; nn++) {
                    for (int c = 0; c < C; c++) {
                        for (int oh = 0; oh < H_out; oh++) {
                            for (int ow = 0; ow < W_out; ow++) {
                                /* Count valid positions */
                                int count = 0;
                                for (int kh = 0; kh < kH; kh++) {
                                    for (int kw = 0; kw < kW; kw++) {
                                        int ih = oh * stride_h + kh - pad_h;
                                        int iw = ow * stride_w + kw - pad_w;
                                        if (ih >= 0 && ih < H && iw >= 0 && iw < W)
                                            count++;
                                    }
                                }
                                int g_idx = ((nn * C + c) * H_out + oh) * W_out + ow;
                                double gv = g[g_idx] / count;
                                for (int kh = 0; kh < kH; kh++) {
                                    for (int kw = 0; kw < kW; kw++) {
                                        int ih = oh * stride_h + kh - pad_h;
                                        int iw = ow * stride_w + kw - pad_w;
                                        if (ih >= 0 && ih < H && iw >= 0 && iw < W)
                                            inp_e->tgrad[((nn * C + c) * H + ih) * W + iw] += gv;
                                    }
                                }
                            }
                        }
                    }
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
    /* Delay-loaded on Windows: catch missing tensorflow.dll gracefully */
#if defined(_WIN32) && defined(_MSC_VER)
    __try {
        eval_tf_init();
    } __except(EXCEPTION_EXECUTE_HANDLER) {
        /* tensorflow.dll not found — keep native C ops */
    }
#else
    eval_tf_init();
#endif
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
    case AD_OP_GELU: {
        double x3 = va * va * va;
        double inner = 0.7978845608028654 * (va + 0.044715 * x3);
        ne.value = 0.5 * va * (1.0 + tanh(inner));
        break;
    }
    case AD_OP_SILU: {
        double s = 1.0 / (1.0 + exp(-va));
        ne.value = va * s;
        break;
    }
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
 * Bridge: Softmax (axis-aware)
 * ================================================================ */

/* __ad_softmax__(tensor, axis) -> Tensor */
static sexp bridge_ad_softmax(sexp ctx, sexp self, sexp_sint_t n,
                               sexp a, sexp axis_s) {
    ADTape *t = ad_global_tape();
    if (!ad_tensorp(a))
        return sexp_user_exception(ctx, self, "softmax: expected tensor", a);
    int ia = unwrap_tensor(a);
    TapeEntry *ea = &t->entries[ia];
    int axis = (int)sexp_unbox_fixnum(axis_s);
    if (axis < 0) axis += ea->ndim;

    int outer = 1, axis_size = ea->shape[axis], inner = 1;
    for (int d = 0; d < axis; d++) outer *= ea->shape[d];
    for (int d = axis + 1; d < ea->ndim; d++) inner *= ea->shape[d];

    TapeEntry ne = {0};
    ne.op = AD_OP_SOFTMAX;
    ne.arg0 = ia; ne.arg1 = -1;
    ne.is_tensor = 1;
    ne.ndim = ea->ndim;
    ne.shape = shape_dup(ea->shape, ea->ndim);
    ne.size = ea->size;
    ne.tdata = tensor_alloc(ne.size);
    ne.tgrad = tensor_alloc(ne.size);
    ne.naux = 1;
    ne.aux = (int *)malloc(sizeof(int));
    ne.aux[0] = axis;

    /* Forward: per-slice exp(x - max) / sum(exp(x - max)) */
    for (int o = 0; o < outer; o++) {
        for (int j = 0; j < inner; j++) {
            /* Find max for numerical stability */
            double mx = -1e308;
            for (int ai = 0; ai < axis_size; ai++) {
                int idx = (o * axis_size + ai) * inner + j;
                if (ea->tdata[idx] > mx) mx = ea->tdata[idx];
            }
            double sum_exp = 0.0;
            for (int ai = 0; ai < axis_size; ai++) {
                int idx = (o * axis_size + ai) * inner + j;
                double e = exp(ea->tdata[idx] - mx);
                ne.tdata[idx] = e;
                sum_exp += e;
            }
            for (int ai = 0; ai < axis_size; ai++) {
                int idx = (o * axis_size + ai) * inner + j;
                ne.tdata[idx] /= sum_exp;
            }
        }
    }

    int idx = tape_push(t, ne);
    return wrap_tensor(ctx, idx);
}

/* ================================================================
 * Bridge: Axis-aware sum/mean
 * ================================================================ */

/* __ad_sum_axis__(tensor, axis) -> Tensor */
static sexp bridge_ad_sum_axis(sexp ctx, sexp self, sexp_sint_t n,
                                sexp a, sexp axis_s) {
    ADTape *t = ad_global_tape();
    if (!ad_tensorp(a))
        return sexp_user_exception(ctx, self, "sum_axis: expected tensor", a);
    int ia = unwrap_tensor(a);
    TapeEntry *ea = &t->entries[ia];
    int axis = (int)sexp_unbox_fixnum(axis_s);
    if (axis < 0) axis += ea->ndim;

    int outer = 1, axis_size = ea->shape[axis], inner = 1;
    for (int d = 0; d < axis; d++) outer *= ea->shape[d];
    for (int d = axis + 1; d < ea->ndim; d++) inner *= ea->shape[d];

    /* Output shape: input shape with axis removed */
    int out_ndim = ea->ndim - 1;
    if (out_ndim < 1) out_ndim = 1;
    int *out_shape = (int *)malloc(out_ndim * sizeof(int));
    if (ea->ndim == 1) {
        out_shape[0] = 1;
    } else {
        int si = 0;
        for (int d = 0; d < ea->ndim; d++)
            if (d != axis) out_shape[si++] = ea->shape[d];
    }
    int out_size = outer * inner;
    if (out_size < 1) out_size = 1;

    TapeEntry ne = {0};
    ne.op = AD_OP_SUM_AXIS;
    ne.arg0 = ia; ne.arg1 = -1;
    ne.is_tensor = 1;
    ne.ndim = out_ndim;
    ne.shape = out_shape;
    ne.size = out_size;
    ne.tdata = tensor_alloc(out_size);
    ne.tgrad = tensor_alloc(out_size);
    ne.naux = 1;
    ne.aux = (int *)malloc(sizeof(int));
    ne.aux[0] = axis;

    for (int o = 0; o < outer; o++) {
        for (int j = 0; j < inner; j++) {
            double s = 0.0;
            for (int ai = 0; ai < axis_size; ai++)
                s += ea->tdata[(o * axis_size + ai) * inner + j];
            ne.tdata[o * inner + j] = s;
        }
    }

    int idx = tape_push(t, ne);
    return wrap_tensor(ctx, idx);
}

/* __ad_mean_axis__(tensor, axis) -> Tensor */
static sexp bridge_ad_mean_axis(sexp ctx, sexp self, sexp_sint_t n,
                                 sexp a, sexp axis_s) {
    ADTape *t = ad_global_tape();
    if (!ad_tensorp(a))
        return sexp_user_exception(ctx, self, "mean_axis: expected tensor", a);
    int ia = unwrap_tensor(a);
    TapeEntry *ea = &t->entries[ia];
    int axis = (int)sexp_unbox_fixnum(axis_s);
    if (axis < 0) axis += ea->ndim;

    int outer = 1, axis_size = ea->shape[axis], inner = 1;
    for (int d = 0; d < axis; d++) outer *= ea->shape[d];
    for (int d = axis + 1; d < ea->ndim; d++) inner *= ea->shape[d];

    int out_ndim = ea->ndim - 1;
    if (out_ndim < 1) out_ndim = 1;
    int *out_shape = (int *)malloc(out_ndim * sizeof(int));
    if (ea->ndim == 1) {
        out_shape[0] = 1;
    } else {
        int si = 0;
        for (int d = 0; d < ea->ndim; d++)
            if (d != axis) out_shape[si++] = ea->shape[d];
    }
    int out_size = outer * inner;
    if (out_size < 1) out_size = 1;

    TapeEntry ne = {0};
    ne.op = AD_OP_MEAN_AXIS;
    ne.arg0 = ia; ne.arg1 = -1;
    ne.is_tensor = 1;
    ne.ndim = out_ndim;
    ne.shape = out_shape;
    ne.size = out_size;
    ne.tdata = tensor_alloc(out_size);
    ne.tgrad = tensor_alloc(out_size);
    ne.naux = 1;
    ne.aux = (int *)malloc(sizeof(int));
    ne.aux[0] = axis;

    double inv_n = 1.0 / axis_size;
    for (int o = 0; o < outer; o++) {
        for (int j = 0; j < inner; j++) {
            double s = 0.0;
            for (int ai = 0; ai < axis_size; ai++)
                s += ea->tdata[(o * axis_size + ai) * inner + j];
            ne.tdata[o * inner + j] = s * inv_n;
        }
    }

    int idx = tape_push(t, ne);
    return wrap_tensor(ctx, idx);
}

/* ================================================================
 * Bridge: Reshape
 * ================================================================ */

/* __ad_reshape__(tensor, shape_list) -> Tensor */
static sexp bridge_ad_reshape(sexp ctx, sexp self, sexp_sint_t n,
                               sexp a, sexp shape_list) {
    ADTape *t = ad_global_tape();
    if (!ad_tensorp(a))
        return sexp_user_exception(ctx, self, "reshape: expected tensor", a);
    int ia = unwrap_tensor(a);
    TapeEntry *ea = &t->entries[ia];

    /* Parse new shape from list */
    int new_ndim = 0;
    int new_shape[8];
    sexp p = shape_list;
    while (sexp_pairp(p) && new_ndim < 8) {
        new_shape[new_ndim++] = (int)sexp_unbox_fixnum(sexp_car(p));
        p = sexp_cdr(p);
    }
    int new_size = shape_total(new_shape, new_ndim);
    if (new_size != ea->size)
        return sexp_user_exception(ctx, self, "reshape: size mismatch", a);

    TapeEntry ne = {0};
    ne.op = AD_OP_RESHAPE;
    ne.arg0 = ia; ne.arg1 = -1;
    ne.is_tensor = 1;
    ne.ndim = new_ndim;
    ne.shape = shape_dup(new_shape, new_ndim);
    ne.size = new_size;
    ne.tdata = tensor_alloc(new_size);
    memcpy(ne.tdata, ea->tdata, new_size * sizeof(double));
    ne.tgrad = tensor_alloc(new_size);
    /* Store original shape in aux for backward */
    ne.naux = ea->ndim;
    ne.aux = (int *)malloc(ea->ndim * sizeof(int));
    memcpy(ne.aux, ea->shape, ea->ndim * sizeof(int));

    int idx = tape_push(t, ne);
    return wrap_tensor(ctx, idx);
}

/* ================================================================
 * Bridge: Slice
 * ================================================================ */

/* __ad_slice__(tensor, begin_list, size_list) -> Tensor */
static sexp bridge_ad_slice(sexp ctx, sexp self, sexp_sint_t n,
                             sexp a, sexp begin_list, sexp size_list) {
    ADTape *t = ad_global_tape();
    if (!ad_tensorp(a))
        return sexp_user_exception(ctx, self, "slice: expected tensor", a);
    int ia = unwrap_tensor(a);
    TapeEntry *ea = &t->entries[ia];
    int ndim = ea->ndim;

    int begin[8], sz[8];
    sexp p = begin_list;
    for (int d = 0; d < ndim && sexp_pairp(p); d++) {
        begin[d] = (int)sexp_unbox_fixnum(sexp_car(p));
        p = sexp_cdr(p);
    }
    p = size_list;
    for (int d = 0; d < ndim && sexp_pairp(p); d++) {
        sz[d] = (int)sexp_unbox_fixnum(sexp_car(p));
        p = sexp_cdr(p);
    }

    int out_size = shape_total(sz, ndim);

    TapeEntry ne = {0};
    ne.op = AD_OP_SLICE;
    ne.arg0 = ia; ne.arg1 = -1;
    ne.is_tensor = 1;
    ne.ndim = ndim;
    ne.shape = shape_dup(sz, ndim);
    ne.size = out_size;
    ne.tdata = tensor_alloc(out_size);
    ne.tgrad = tensor_alloc(out_size);
    ne.naux = 2 * ndim;
    ne.aux = (int *)malloc(2 * ndim * sizeof(int));
    memcpy(ne.aux, begin, ndim * sizeof(int));
    memcpy(ne.aux + ndim, sz, ndim * sizeof(int));

    /* Forward: stride-based copy */
    int in_strides[8], out_strides[8];
    in_strides[ndim - 1] = 1;
    out_strides[ndim - 1] = 1;
    for (int d = ndim - 2; d >= 0; d--) {
        in_strides[d] = in_strides[d + 1] * ea->shape[d + 1];
        out_strides[d] = out_strides[d + 1] * sz[d + 1];
    }

    for (int flat = 0; flat < out_size; flat++) {
        int rem = flat;
        int src_flat = 0;
        for (int d = 0; d < ndim; d++) {
            int coord = rem / out_strides[d];
            rem %= out_strides[d];
            src_flat += (begin[d] + coord) * in_strides[d];
        }
        ne.tdata[flat] = ea->tdata[src_flat];
    }

    int idx = tape_push(t, ne);
    return wrap_tensor(ctx, idx);
}

/* ================================================================
 * Bridge: Concat
 * ================================================================ */

/* __ad_concat__(tensor_list, axis) -> Tensor */
static sexp bridge_ad_concat(sexp ctx, sexp self, sexp_sint_t n,
                              sexp tensor_list, sexp axis_s) {
    ADTape *t = ad_global_tape();
    int axis = (int)sexp_unbox_fixnum(axis_s);

    /* Count inputs and collect tape indices */
    int n_inputs = 0;
    int tape_indices[64];
    sexp p = tensor_list;
    while (sexp_pairp(p) && n_inputs < 64) {
        sexp ti = sexp_car(p);
        if (!ad_tensorp(ti))
            return sexp_user_exception(ctx, self, "concat: expected tensor list", ti);
        tape_indices[n_inputs++] = unwrap_tensor(ti);
        p = sexp_cdr(p);
    }
    if (n_inputs < 1)
        return sexp_user_exception(ctx, self, "concat: empty list", tensor_list);

    TapeEntry *e0 = &t->entries[tape_indices[0]];
    int ndim = e0->ndim;
    if (axis < 0) axis += ndim;

    /* Compute output shape: sum along axis */
    int out_shape[8];
    memcpy(out_shape, e0->shape, ndim * sizeof(int));
    int total_axis = e0->shape[axis];
    for (int i = 1; i < n_inputs; i++) {
        total_axis += t->entries[tape_indices[i]].shape[axis];
    }
    out_shape[axis] = total_axis;
    int out_size = shape_total(out_shape, ndim);

    int outer = 1, inner = 1;
    for (int d = 0; d < axis; d++) outer *= out_shape[d];
    for (int d = axis + 1; d < ndim; d++) inner *= out_shape[d];

    TapeEntry ne = {0};
    ne.op = AD_OP_CONCAT;
    ne.arg0 = tape_indices[0]; ne.arg1 = -1;
    ne.is_tensor = 1;
    ne.ndim = ndim;
    ne.shape = shape_dup(out_shape, ndim);
    ne.size = out_size;
    ne.tdata = tensor_alloc(out_size);
    ne.tgrad = tensor_alloc(out_size);
    ne.naux = 2 + n_inputs;
    ne.aux = (int *)malloc(ne.naux * sizeof(int));
    ne.aux[0] = axis;
    ne.aux[1] = n_inputs;
    for (int i = 0; i < n_inputs; i++) ne.aux[2 + i] = tape_indices[i];

    /* Forward: copy data along axis */
    int offset = 0;
    for (int inp = 0; inp < n_inputs; inp++) {
        TapeEntry *ei = &t->entries[tape_indices[inp]];
        int ai_size = ei->shape[axis];
        for (int o = 0; o < outer; o++) {
            for (int a = 0; a < ai_size; a++) {
                for (int j = 0; j < inner; j++) {
                    int dst_idx = (o * total_axis + offset + a) * inner + j;
                    int src_idx = (o * ai_size + a) * inner + j;
                    ne.tdata[dst_idx] = ei->tdata[src_idx];
                }
            }
        }
        offset += ai_size;
    }

    int idx = tape_push(t, ne);
    return wrap_tensor(ctx, idx);
}

/* ================================================================
 * Bridge: Gather
 * ================================================================ */

/* __ad_gather__(tensor, indices_list, axis) -> Tensor */
static sexp bridge_ad_gather(sexp ctx, sexp self, sexp_sint_t n,
                              sexp a, sexp indices_list, sexp axis_s) {
    ADTape *t = ad_global_tape();
    if (!ad_tensorp(a))
        return sexp_user_exception(ctx, self, "gather: expected tensor", a);
    int ia = unwrap_tensor(a);
    TapeEntry *ea = &t->entries[ia];
    int axis = (int)sexp_unbox_fixnum(axis_s);
    if (axis < 0) axis += ea->ndim;

    /* Parse indices */
    int n_idx = 0;
    int indices[512];
    sexp p = indices_list;
    while (sexp_pairp(p) && n_idx < 512) {
        indices[n_idx++] = (int)sexp_unbox_fixnum(sexp_car(p));
        p = sexp_cdr(p);
    }

    int outer = 1, inner = 1;
    for (int d = 0; d < axis; d++) outer *= ea->shape[d];
    for (int d = axis + 1; d < ea->ndim; d++) inner *= ea->shape[d];

    /* Output shape: replace axis dim with n_idx */
    int out_shape[8];
    memcpy(out_shape, ea->shape, ea->ndim * sizeof(int));
    out_shape[axis] = n_idx;
    int out_size = shape_total(out_shape, ea->ndim);

    TapeEntry ne = {0};
    ne.op = AD_OP_GATHER;
    ne.arg0 = ia; ne.arg1 = -1;
    ne.is_tensor = 1;
    ne.ndim = ea->ndim;
    ne.shape = shape_dup(out_shape, ea->ndim);
    ne.size = out_size;
    ne.tdata = tensor_alloc(out_size);
    ne.tgrad = tensor_alloc(out_size);
    ne.naux = 2 + n_idx;
    ne.aux = (int *)malloc(ne.naux * sizeof(int));
    ne.aux[0] = axis;
    ne.aux[1] = n_idx;
    for (int i = 0; i < n_idx; i++) ne.aux[2 + i] = indices[i];

    /* Forward: index-select along axis */
    for (int o = 0; o < outer; o++) {
        for (int ii = 0; ii < n_idx; ii++) {
            int src_a = indices[ii];
            for (int j = 0; j < inner; j++) {
                int dst = (o * n_idx + ii) * inner + j;
                int src = (o * ea->shape[axis] + src_a) * inner + j;
                ne.tdata[dst] = ea->tdata[src];
            }
        }
    }

    int idx = tape_push(t, ne);
    return wrap_tensor(ctx, idx);
}

/* ================================================================
 * Bridge: Layer Norm
 * ================================================================ */

/* __ad_layer_norm__(input, gamma, beta, eps) -> Tensor */
static sexp bridge_ad_layer_norm(sexp ctx, sexp self, sexp_sint_t n,
                                  sexp input, sexp gamma, sexp beta, sexp eps_s) {
    ADTape *t = ad_global_tape();
    if (!ad_tensorp(input))
        return sexp_user_exception(ctx, self, "layer_norm: expected tensor", input);
    int i_in = unwrap_tensor(input);
    TapeEntry *ei = &t->entries[i_in];

    int i_gamma = -1;
    if (ad_tensorp(gamma)) i_gamma = unwrap_tensor(gamma);

    int i_beta = -1;
    if (ad_tensorp(beta)) i_beta = unwrap_tensor(beta);

    double eps = 1e-5;
    if (sexp_flonump(eps_s)) eps = sexp_flonum_value(eps_s);
    else if (sexp_fixnump(eps_s)) eps = (double)sexp_unbox_fixnum(eps_s);

    int last_dim = ei->shape[ei->ndim - 1];
    int n_slices = ei->size / last_dim;

    TapeEntry ne = {0};
    ne.op = AD_OP_LAYER_NORM;
    ne.arg0 = i_in;
    ne.arg1 = i_gamma;
    ne.is_tensor = 1;
    ne.ndim = ei->ndim;
    ne.shape = shape_dup(ei->shape, ei->ndim);
    ne.size = ei->size;
    ne.tdata = tensor_alloc(ne.size);
    ne.tgrad = tensor_alloc(ne.size);
    /* aux: [beta_tape_idx, eps_lo, eps_hi] — eps as double via memcpy */
    ne.naux = 3;
    ne.aux = (int *)malloc(3 * sizeof(int));
    ne.aux[0] = i_beta;
    memcpy(&ne.aux[1], &eps, sizeof(double));

    TapeEntry *eg = (i_gamma >= 0) ? &t->entries[i_gamma] : NULL;
    TapeEntry *eb = (i_beta >= 0) ? &t->entries[i_beta] : NULL;

    /* Forward: (x - mean) / sqrt(var + eps) * gamma + beta */
    for (int s = 0; s < n_slices; s++) {
        const double *x_row = ei->tdata + s * last_dim;
        double *y_row = ne.tdata + s * last_dim;
        double mean_v = 0.0;
        for (int j = 0; j < last_dim; j++) mean_v += x_row[j];
        mean_v /= last_dim;
        double var_v = 0.0;
        for (int j = 0; j < last_dim; j++) {
            double d = x_row[j] - mean_v;
            var_v += d * d;
        }
        var_v /= last_dim;
        double inv_std = 1.0 / sqrt(var_v + eps);
        for (int j = 0; j < last_dim; j++) {
            double x_hat = (x_row[j] - mean_v) * inv_std;
            double g_val = eg ? eg->tdata[j] : 1.0;
            double b_val = eb ? eb->tdata[j] : 0.0;
            y_row[j] = x_hat * g_val + b_val;
        }
    }

    int idx = tape_push(t, ne);
    return wrap_tensor(ctx, idx);
}

/* ================================================================
 * Bridge: Where (conditional select)
 * ================================================================ */

/* __ad_where__(cond, a, b) -> Tensor */
static sexp bridge_ad_where(sexp ctx, sexp self, sexp_sint_t n,
                             sexp cond, sexp a, sexp b) {
    ADTape *t = ad_global_tape();
    if (!ad_tensorp(cond))
        return sexp_user_exception(ctx, self, "where: expected tensor for cond", cond);
    int i_cond = unwrap_tensor(cond);

    int i_a;
    if (ad_tensorp(a)) {
        i_a = unwrap_tensor(a);
    } else {
        double va = sexp_to_double(a);
        double *d = tensor_alloc(1);
        d[0] = va;
        int *s = (int *)malloc(sizeof(int));
        s[0] = 1;
        TapeEntry e = {0};
        e.op = AD_OP_LEAF; e.arg0 = e.arg1 = -1;
        e.is_tensor = 1; e.tdata = d; e.tgrad = tensor_alloc(1);
        e.shape = s; e.ndim = 1; e.size = 1;
        i_a = tape_push(t, e);
    }

    int i_b;
    if (ad_tensorp(b)) {
        i_b = unwrap_tensor(b);
    } else {
        double vb = sexp_to_double(b);
        double *d = tensor_alloc(1);
        d[0] = vb;
        int *s = (int *)malloc(sizeof(int));
        s[0] = 1;
        TapeEntry e = {0};
        e.op = AD_OP_LEAF; e.arg0 = e.arg1 = -1;
        e.is_tensor = 1; e.tdata = d; e.tgrad = tensor_alloc(1);
        e.shape = s; e.ndim = 1; e.size = 1;
        i_b = tape_push(t, e);
    }

    /* Re-fetch after potential realloc */
    TapeEntry *ec = &t->entries[i_cond];
    TapeEntry *ea = &t->entries[i_a];
    TapeEntry *eb_e = &t->entries[i_b];

    /* Output shape: max of all sizes */
    int out_size = ec->size;
    if (ea->size > out_size) out_size = ea->size;
    if (eb_e->size > out_size) out_size = eb_e->size;

    int out_ndim;
    int *out_shape;
    if (ec->size == out_size) { out_ndim = ec->ndim; out_shape = shape_dup(ec->shape, ec->ndim); }
    else if (ea->size == out_size) { out_ndim = ea->ndim; out_shape = shape_dup(ea->shape, ea->ndim); }
    else { out_ndim = eb_e->ndim; out_shape = shape_dup(eb_e->shape, eb_e->ndim); }

    TapeEntry ne = {0};
    ne.op = AD_OP_WHERE;
    ne.arg0 = i_cond;
    ne.arg1 = i_a;
    ne.is_tensor = 1;
    ne.ndim = out_ndim;
    ne.shape = out_shape;
    ne.size = out_size;
    ne.tdata = tensor_alloc(out_size);
    ne.tgrad = tensor_alloc(out_size);
    ne.naux = 1;
    ne.aux = (int *)malloc(sizeof(int));
    ne.aux[0] = i_b;

    /* Forward: cond != 0 ? a : b */
    for (int k = 0; k < out_size; k++) {
        double cv = ec->tdata[k % ec->size];
        ne.tdata[k] = (cv != 0.0) ? ea->tdata[k % ea->size] : eb_e->tdata[k % eb_e->size];
    }

    int idx = tape_push(t, ne);
    return wrap_tensor(ctx, idx);
}

/* ================================================================
 * Bridge: Batch MatMul
 * ================================================================ */

/* __ad_batch_matmul__(a, b) -> Tensor */
static sexp bridge_ad_batch_matmul(sexp ctx, sexp self, sexp_sint_t n,
                                    sexp a, sexp b) {
    ADTape *t = ad_global_tape();
    if (!ad_tensorp(a) || !ad_tensorp(b))
        return sexp_user_exception(ctx, self, "batch_matmul: expected tensors", a);
    int ia = unwrap_tensor(a);
    int ib = unwrap_tensor(b);
    TapeEntry *ea = &t->entries[ia];
    TapeEntry *eb = &t->entries[ib];

    if (ea->ndim < 3 || eb->ndim < 3)
        return sexp_user_exception(ctx, self, "batch_matmul: need 3D+ tensors", a);

    int nd = ea->ndim;
    int m = ea->shape[nd - 2], kk = ea->shape[nd - 1];
    int n2 = eb->shape[nd - 1];
    if (kk != eb->shape[nd - 2])
        return sexp_user_exception(ctx, self, "batch_matmul: shape mismatch", a);

    /* Compute batch dims */
    int batch = 1;
    for (int d = 0; d < nd - 2; d++) batch *= ea->shape[d];

    int out_size = batch * m * n2;
    int *out_shape = (int *)malloc(nd * sizeof(int));
    for (int d = 0; d < nd - 2; d++) out_shape[d] = ea->shape[d];
    out_shape[nd - 2] = m;
    out_shape[nd - 1] = n2;

    TapeEntry ne = {0};
    ne.op = AD_OP_BATCH_MATMUL;
    ne.arg0 = ia;
    ne.arg1 = ib;
    ne.is_tensor = 1;
    ne.ndim = nd;
    ne.shape = out_shape;
    ne.size = out_size;
    ne.tdata = tensor_alloc(out_size);
    ne.tgrad = tensor_alloc(out_size);

    /* Forward: per-batch matmul */
    for (int bi = 0; bi < batch; bi++) {
        ad_tensor_ops.matmul(ea->tdata + bi * m * kk,
                             eb->tdata + bi * kk * n2,
                             ne.tdata + bi * m * n2,
                             m, kk, n2);
    }

    int idx = tape_push(t, ne);
    return wrap_tensor(ctx, idx);
}

/* ================================================================
 * Bridge: Conv2D
 * ================================================================ */

/* Helper to extract int from sexp (fixnum) */
static int sexp_to_int(sexp x) {
    if (sexp_fixnump(x)) return (int)sexp_unbox_fixnum(x);
    if (sexp_flonump(x)) return (int)sexp_flonum_value(x);
    return 0;
}

/* __ad_conv2d__(input, kernel, config_list) -> Tensor
 * config_list = (stride_h stride_w pad_h pad_w dil_h dil_w) */
static sexp bridge_ad_conv2d(sexp ctx, sexp self, sexp_sint_t n,
                              sexp input, sexp kernel, sexp config) {
    ADTape *t = ad_global_tape();
    if (!ad_tensorp(input))
        return sexp_user_exception(ctx, self, "conv2d: expected tensor for input", input);
    if (!ad_tensorp(kernel))
        return sexp_user_exception(ctx, self, "conv2d: expected tensor for kernel", kernel);

    int i_in = unwrap_tensor(input);
    int i_ker = unwrap_tensor(kernel);
    TapeEntry *ei = &t->entries[i_in];
    TapeEntry *ek = &t->entries[i_ker];

    if (ei->ndim != 4)
        return sexp_user_exception(ctx, self, "conv2d: input must be 4D [N,C,H,W]", input);
    if (ek->ndim != 4)
        return sexp_user_exception(ctx, self, "conv2d: kernel must be 4D [Cout,Cin,kH,kW]", kernel);

    /* Parse config list */
    int cfg[6] = {1, 1, 0, 0, 1, 1};
    sexp p = config;
    for (int j = 0; j < 6 && sexp_pairp(p); j++) {
        cfg[j] = sexp_to_int(sexp_car(p));
        p = sexp_cdr(p);
    }
    int stride_h = cfg[0], stride_w = cfg[1];
    int pad_h = cfg[2], pad_w = cfg[3];
    int dil_h = cfg[4], dil_w = cfg[5];

    int N = ei->shape[0], C_in = ei->shape[1];
    int H = ei->shape[2], W = ei->shape[3];
    int C_out = ek->shape[0];
    int kH = ek->shape[2], kW = ek->shape[3];

    if (ek->shape[1] != C_in)
        return sexp_user_exception(ctx, self, "conv2d: kernel C_in mismatch", kernel);

    int H_out = (H + 2 * pad_h - dil_h * (kH - 1) - 1) / stride_h + 1;
    int W_out = (W + 2 * pad_w - dil_w * (kW - 1) - 1) / stride_w + 1;

    if (H_out <= 0 || W_out <= 0)
        return sexp_user_exception(ctx, self, "conv2d: invalid output dimensions", input);

    int out_size = N * C_out * H_out * W_out;
    int *out_shape = (int *)malloc(4 * sizeof(int));
    out_shape[0] = N; out_shape[1] = C_out;
    out_shape[2] = H_out; out_shape[3] = W_out;

    TapeEntry ne = {0};
    ne.op = AD_OP_CONV2D;
    ne.arg0 = i_in;
    ne.arg1 = i_ker;
    ne.is_tensor = 1;
    ne.ndim = 4;
    ne.shape = out_shape;
    ne.size = out_size;
    ne.tdata = tensor_alloc(out_size);
    ne.tgrad = tensor_alloc(out_size);
    ne.naux = 6;
    ne.aux = (int *)malloc(6 * sizeof(int));
    ne.aux[0] = stride_h; ne.aux[1] = stride_w;
    ne.aux[2] = pad_h; ne.aux[3] = pad_w;
    ne.aux[4] = dil_h; ne.aux[5] = dil_w;

    /* Forward: per batch element, im2col then GEMM */
    int col_rows = C_in * kH * kW;
    int col_cols = H_out * W_out;
    int inp_spatial = C_in * H * W;
    int out_spatial = C_out * H_out * W_out;

    for (int nn = 0; nn < N; nn++) {
        double *col = tensor_alloc(col_rows * col_cols);
        ad_im2col(ei->tdata + nn * inp_spatial, col,
                  C_in, H, W, kH, kW, H_out, W_out,
                  stride_h, stride_w, pad_h, pad_w, dil_h, dil_w);
        /* kernel [C_out, C_in*kH*kW] @ col [C_in*kH*kW, H_out*W_out]
         * = out [C_out, H_out*W_out] */
        ad_tensor_ops.matmul(ek->tdata, col, ne.tdata + nn * out_spatial,
                             C_out, col_rows, col_cols);
        free(col);
    }

    int idx = tape_push(t, ne);
    return wrap_tensor(ctx, idx);
}

/* ================================================================
 * Bridge: MaxPool2D
 * ================================================================ */

/* __ad_max_pool2d__(input, config_list) -> Tensor
 * config_list = (kH kW stride_h stride_w pad_h pad_w) */
static sexp bridge_ad_max_pool2d(sexp ctx, sexp self, sexp_sint_t n,
                                  sexp input, sexp config) {
    ADTape *t = ad_global_tape();
    if (!ad_tensorp(input))
        return sexp_user_exception(ctx, self, "max_pool2d: expected tensor", input);

    int i_in = unwrap_tensor(input);
    TapeEntry *ei = &t->entries[i_in];

    if (ei->ndim != 4)
        return sexp_user_exception(ctx, self, "max_pool2d: input must be 4D [N,C,H,W]", input);

    /* Parse config list */
    int cfg[6] = {2, 2, 2, 2, 0, 0};
    sexp p = config;
    for (int j = 0; j < 6 && sexp_pairp(p); j++) {
        cfg[j] = sexp_to_int(sexp_car(p));
        p = sexp_cdr(p);
    }
    int kH = cfg[0], kW = cfg[1];
    int stride_h = cfg[2], stride_w = cfg[3];
    int pad_h = cfg[4], pad_w = cfg[5];

    int N = ei->shape[0], C = ei->shape[1];
    int H = ei->shape[2], W = ei->shape[3];
    int H_out = (H + 2 * pad_h - kH) / stride_h + 1;
    int W_out = (W + 2 * pad_w - kW) / stride_w + 1;

    if (H_out <= 0 || W_out <= 0)
        return sexp_user_exception(ctx, self, "max_pool2d: invalid output dimensions", input);

    int out_size = N * C * H_out * W_out;
    int *out_shape = (int *)malloc(4 * sizeof(int));
    out_shape[0] = N; out_shape[1] = C;
    out_shape[2] = H_out; out_shape[3] = W_out;

    TapeEntry ne = {0};
    ne.op = AD_OP_MAX_POOL2D;
    ne.arg0 = i_in;
    ne.arg1 = -1;
    ne.is_tensor = 1;
    ne.ndim = 4;
    ne.shape = out_shape;
    ne.size = out_size;
    ne.tdata = tensor_alloc(out_size);
    ne.tgrad = tensor_alloc(out_size);
    ne.naux = 6;
    ne.aux = (int *)malloc(6 * sizeof(int));
    ne.aux[0] = kH; ne.aux[1] = kW;
    ne.aux[2] = stride_h; ne.aux[3] = stride_w;
    ne.aux[4] = pad_h; ne.aux[5] = pad_w;

    /* Forward: max over each pooling window */
    for (int nn = 0; nn < N; nn++) {
        for (int c = 0; c < C; c++) {
            for (int oh = 0; oh < H_out; oh++) {
                for (int ow = 0; ow < W_out; ow++) {
                    double best = -1e308;
                    for (int kh = 0; kh < kH; kh++) {
                        for (int kw = 0; kw < kW; kw++) {
                            int ih = oh * stride_h + kh - pad_h;
                            int iw = ow * stride_w + kw - pad_w;
                            if (ih >= 0 && ih < H && iw >= 0 && iw < W) {
                                double v = ei->tdata[((nn * C + c) * H + ih) * W + iw];
                                if (v > best) best = v;
                            }
                        }
                    }
                    ne.tdata[((nn * C + c) * H_out + oh) * W_out + ow] = best;
                }
            }
        }
    }

    int idx = tape_push(t, ne);
    return wrap_tensor(ctx, idx);
}

/* ================================================================
 * Bridge: AvgPool2D
 * ================================================================ */

/* __ad_avg_pool2d__(input, config_list) -> Tensor
 * config_list = (kH kW stride_h stride_w pad_h pad_w) */
static sexp bridge_ad_avg_pool2d(sexp ctx, sexp self, sexp_sint_t n,
                                  sexp input, sexp config) {
    ADTape *t = ad_global_tape();
    if (!ad_tensorp(input))
        return sexp_user_exception(ctx, self, "avg_pool2d: expected tensor", input);

    int i_in = unwrap_tensor(input);
    TapeEntry *ei = &t->entries[i_in];

    if (ei->ndim != 4)
        return sexp_user_exception(ctx, self, "avg_pool2d: input must be 4D [N,C,H,W]", input);

    /* Parse config list */
    int cfg[6] = {2, 2, 2, 2, 0, 0};
    sexp p = config;
    for (int j = 0; j < 6 && sexp_pairp(p); j++) {
        cfg[j] = sexp_to_int(sexp_car(p));
        p = sexp_cdr(p);
    }
    int kH = cfg[0], kW = cfg[1];
    int stride_h = cfg[2], stride_w = cfg[3];
    int pad_h = cfg[4], pad_w = cfg[5];

    int N = ei->shape[0], C = ei->shape[1];
    int H = ei->shape[2], W = ei->shape[3];
    int H_out = (H + 2 * pad_h - kH) / stride_h + 1;
    int W_out = (W + 2 * pad_w - kW) / stride_w + 1;

    if (H_out <= 0 || W_out <= 0)
        return sexp_user_exception(ctx, self, "avg_pool2d: invalid output dimensions", input);

    int out_size = N * C * H_out * W_out;
    int *out_shape = (int *)malloc(4 * sizeof(int));
    out_shape[0] = N; out_shape[1] = C;
    out_shape[2] = H_out; out_shape[3] = W_out;

    TapeEntry ne = {0};
    ne.op = AD_OP_AVG_POOL2D;
    ne.arg0 = i_in;
    ne.arg1 = -1;
    ne.is_tensor = 1;
    ne.ndim = 4;
    ne.shape = out_shape;
    ne.size = out_size;
    ne.tdata = tensor_alloc(out_size);
    ne.tgrad = tensor_alloc(out_size);
    ne.naux = 6;
    ne.aux = (int *)malloc(6 * sizeof(int));
    ne.aux[0] = kH; ne.aux[1] = kW;
    ne.aux[2] = stride_h; ne.aux[3] = stride_w;
    ne.aux[4] = pad_h; ne.aux[5] = pad_w;

    /* Forward: average over valid window positions */
    for (int nn = 0; nn < N; nn++) {
        for (int c = 0; c < C; c++) {
            for (int oh = 0; oh < H_out; oh++) {
                for (int ow = 0; ow < W_out; ow++) {
                    double sum_val = 0.0;
                    int count = 0;
                    for (int kh = 0; kh < kH; kh++) {
                        for (int kw = 0; kw < kW; kw++) {
                            int ih = oh * stride_h + kh - pad_h;
                            int iw = ow * stride_w + kw - pad_w;
                            if (ih >= 0 && ih < H && iw >= 0 && iw < W) {
                                sum_val += ei->tdata[((nn * C + c) * H + ih) * W + iw];
                                count++;
                            }
                        }
                    }
                    ne.tdata[((nn * C + c) * H_out + oh) * W_out + ow] =
                        count > 0 ? sum_val / count : 0.0;
                }
            }
        }
    }

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

    /* New tensor ops */
    sexp_define_foreign(ctx, env, "__ad_softmax__", 2, bridge_ad_softmax);
    sexp_define_foreign(ctx, env, "__ad_sum_axis__", 2, bridge_ad_sum_axis);
    sexp_define_foreign(ctx, env, "__ad_mean_axis__", 2, bridge_ad_mean_axis);
    sexp_define_foreign(ctx, env, "__ad_reshape__", 2, bridge_ad_reshape);
    sexp_define_foreign(ctx, env, "__ad_slice__", 3, bridge_ad_slice);
    sexp_define_foreign(ctx, env, "__ad_concat__", 2, bridge_ad_concat);
    sexp_define_foreign(ctx, env, "__ad_gather__", 3, bridge_ad_gather);
    sexp_define_foreign(ctx, env, "__ad_layer_norm__", 4, bridge_ad_layer_norm);
    sexp_define_foreign(ctx, env, "__ad_where__", 3, bridge_ad_where);
    sexp_define_foreign(ctx, env, "__ad_batch_matmul__", 2, bridge_ad_batch_matmul);
    sexp_define_foreign(ctx, env, "__ad_conv2d__", 3, bridge_ad_conv2d);
    sexp_define_foreign(ctx, env, "__ad_max_pool2d__", 2, bridge_ad_max_pool2d);
    sexp_define_foreign(ctx, env, "__ad_avg_pool2d__", 2, bridge_ad_avg_pool2d);

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
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_SOFTMAX", -1), sexp_make_fixnum(AD_OP_SOFTMAX));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_GELU", -1), sexp_make_fixnum(AD_OP_GELU));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_SILU", -1), sexp_make_fixnum(AD_OP_SILU));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_SUM_AXIS", -1), sexp_make_fixnum(AD_OP_SUM_AXIS));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_MEAN_AXIS", -1), sexp_make_fixnum(AD_OP_MEAN_AXIS));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_RESHAPE", -1), sexp_make_fixnum(AD_OP_RESHAPE));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_SLICE", -1), sexp_make_fixnum(AD_OP_SLICE));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_CONCAT", -1), sexp_make_fixnum(AD_OP_CONCAT));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_GATHER", -1), sexp_make_fixnum(AD_OP_GATHER));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_LAYER_NORM", -1), sexp_make_fixnum(AD_OP_LAYER_NORM));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_WHERE", -1), sexp_make_fixnum(AD_OP_WHERE));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_BATCH_MATMUL", -1), sexp_make_fixnum(AD_OP_BATCH_MATMUL));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_CONV2D", -1), sexp_make_fixnum(AD_OP_CONV2D));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_MAX_POOL2D", -1), sexp_make_fixnum(AD_OP_MAX_POOL2D));
    sexp_env_define(ctx, env, sexp_intern(ctx, "AD_OP_AVG_POOL2D", -1), sexp_make_fixnum(AD_OP_AVG_POOL2D));
}
