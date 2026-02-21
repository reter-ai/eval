/*  _eval_ad.h -- Automatic differentiation types and declarations  */

#ifndef EVAL_AD_H
#define EVAL_AD_H

#include <chibi/eval.h>

/* ================================================================
 * Op kinds for the unified tape
 * ================================================================ */

typedef enum {
    AD_OP_LEAF = 0,
    AD_OP_ADD, AD_OP_SUB, AD_OP_MUL, AD_OP_DIV, AD_OP_POW, AD_OP_NEG,
    AD_OP_SIN, AD_OP_COS, AD_OP_TAN, AD_OP_EXP, AD_OP_LOG, AD_OP_SQRT,
    AD_OP_TANH, AD_OP_SIGMOID, AD_OP_RELU, AD_OP_ABS,
    AD_OP_MATMUL, AD_OP_SUM, AD_OP_MEAN, AD_OP_TRANSPOSE,
    AD_OP_SOFTMAX,        /* 21 — axis-aware fused softmax */
    AD_OP_GELU,           /* 22 — element-wise GELU activation */
    AD_OP_SILU,           /* 23 — element-wise SiLU/Swish activation */
    AD_OP_SUM_AXIS,       /* 24 — sum along specific axis */
    AD_OP_MEAN_AXIS,      /* 25 — mean along specific axis */
    AD_OP_RESHAPE,        /* 26 — reshape tensor (same data, new shape) */
    AD_OP_SLICE,          /* 27 — extract subtensor by begin+size */
    AD_OP_CONCAT,         /* 28 — concatenate along axis (N inputs) */
    AD_OP_GATHER,         /* 29 — index into tensor along axis (embeddings) */
    AD_OP_LAYER_NORM,     /* 30 — fused layer normalization */
    AD_OP_WHERE,          /* 31 — conditional select (masking) */
    AD_OP_BATCH_MATMUL,   /* 32 — 3D+ batched matrix multiply */
    AD_OP_CONV2D,         /* 33 — 2D convolution (NCHW, im2col+GEMM) */
    AD_OP_MAX_POOL2D,     /* 34 — 2D max pooling (NCHW) */
    AD_OP_AVG_POOL2D      /* 35 — 2D average pooling (NCHW) */
} ADOpKind;

/* ================================================================
 * Tape entry — unified for scalars and tensors
 * ================================================================ */

typedef struct {
    ADOpKind op;
    int arg0, arg1;         /* tape indices (-1 = none) */
    int is_tensor;          /* 0 = scalar, 1 = tensor */
    /* Scalar fields */
    double value, grad;
    double cached0, cached1; /* cached values for backward */
    /* Auxiliary parameters (axis, shape, extra tape indices) */
    int *aux;               /* auxiliary int params */
    int naux;               /* number of entries in aux */
    /* Tensor fields (NULL for scalars) */
    double *tdata;          /* tensor data (flat, row-major) */
    double *tgrad;          /* tensor gradient (same shape) */
    int *shape;             /* shape array */
    int ndim;               /* number of dimensions */
    int size;               /* total elements */
} TapeEntry;

typedef struct {
    TapeEntry *entries;
    int count, capacity;
    int enabled;            /* 1 = recording, 0 = no_grad */
} ADTape;

/* ================================================================
 * Global tape and type tags
 * ================================================================ */

extern ADTape *ad_global_tape(void);

/* Type tags (set during registration) */
extern sexp_tag_t ad_var_type_tag;
extern sexp_tag_t ad_dual_type_tag;
extern sexp_tag_t ad_tensor_type_tag;

/* ================================================================
 * Type predicates
 * ================================================================ */

#define ad_varp(x)    (sexp_pointerp(x) && sexp_pointer_tag(x) == ad_var_type_tag)
#define ad_dualp(x)   (sexp_pointerp(x) && sexp_pointer_tag(x) == ad_dual_type_tag)
#define ad_tensorp(x)  (sexp_pointerp(x) && sexp_pointer_tag(x) == ad_tensor_type_tag)

/* ================================================================
 * Tensor op dispatch table — swappable backend (native C / TensorFlow)
 * ================================================================ */

typedef struct {
    void (*binary_ew)(const double *a, int sa, const double *b, int sb,
                      double *out, int sout, ADOpKind op);
    void (*matmul)(const double *a, const double *b, double *c, int m, int k, int n);
    void (*transpose)(const double *a, double *b, int m, int n);
    void (*unary_ew)(const double *a, double *out, int size, ADOpKind op);
    void (*backward)(ADTape *t, int from_idx);  /* whole-graph backward */
} ADTensorOps;

extern ADTensorOps ad_tensor_ops;

/* Native C implementations (default backend) */
void ad_native_binary_ew(const double *a, int sa, const double *b, int sb,
                         double *out, int sout, ADOpKind op);
void ad_native_matmul(const double *a, const double *b, double *c, int m, int k, int n);
void ad_native_transpose(const double *a, double *b, int m, int n);
void ad_native_unary_ew(const double *a, double *out, int size, ADOpKind op);
void ad_native_backward(ADTape *t, int from_idx);

#ifdef EVAL_HAVE_TF
void eval_tf_init(void);
#endif

/* ================================================================
 * Registration functions (called from _chibi_context.c / _eval_pool.c)
 * ================================================================ */

void register_ad_types(sexp ctx);
void register_ad_bridge_functions(sexp ctx, sexp env);

#endif /* EVAL_AD_H */
