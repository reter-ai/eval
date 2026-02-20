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
    AD_OP_MATMUL, AD_OP_SUM, AD_OP_MEAN, AD_OP_TRANSPOSE
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
