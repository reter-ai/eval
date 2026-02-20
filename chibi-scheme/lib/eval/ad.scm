;; eval/ad.scm -- Automatic differentiation: operator overloading, grad, math, SGD
;; Loaded after collection-oo.scm. Chains onto previous __send__ dispatch.

;; ================================================================
;; Save original arithmetic (fast path for plain numbers)
;; ================================================================

(define __prim_+__ +)
(define __prim_-__ -)
(define __prim_*__ *)
(define __prim_/__ /)
(define __prim_expt__ expt)
(define __prim_<__ <)
(define __prim_>__ >)
(define __prim_=__ =)

;; ================================================================
;; Ensure-var: wrap plain number as a Var leaf on tape
;; ================================================================

(define (ensure-var x)
  (if (__ad_var_p__ x) x (__ad_make_var__ (exact->inexact x))))

;; ================================================================
;; Dual number arithmetic — val/dot can be any sexp (nested duals)
;; Uses __ad_*__ dispatch functions (NOT +,-,*,/ directly) because
;; chibi inlines primitives at compile time before set! overrides them.
;; Mutual recursion with __ad_*__ works via top-level define.
;; ================================================================

(define (__dv__ x) (if (__ad_dual_p__ x) (__ad_dual_val__ x) x))
(define (__dd__ x) (if (__ad_dual_p__ x) (__ad_dual_dot__ x) 0.0))

(define (dual-add a b)
  (let ((av (__dv__ a)) (ad (__dd__ a))
        (bv (__dv__ b)) (bd (__dd__ b)))
    (__ad_make_dual__ (__ad_add__ av bv)
                      (__ad_add__ ad bd))))

(define (dual-sub a b)
  (let ((av (__dv__ a)) (ad (__dd__ a))
        (bv (__dv__ b)) (bd (__dd__ b)))
    (__ad_make_dual__ (__ad_sub__ av bv)
                      (__ad_sub__ ad bd))))

(define (dual-mul a b)
  (let ((av (__dv__ a)) (ad (__dd__ a))
        (bv (__dv__ b)) (bd (__dd__ b)))
    (__ad_make_dual__ (__ad_mul__ av bv)
                      (__ad_add__ (__ad_mul__ ad bv) (__ad_mul__ av bd)))))

(define (dual-div a b)
  (let ((av (__dv__ a)) (ad (__dd__ a))
        (bv (__dv__ b)) (bd (__dd__ b)))
    (__ad_make_dual__ (__ad_div__ av bv)
                      (__ad_div__ (__ad_sub__ (__ad_mul__ ad bv) (__ad_mul__ av bd))
                                  (__ad_mul__ bv bv)))))

(define (dual-pow a b)
  (let ((av (__dv__ a)) (ad (__dd__ a))
        (bv (__dv__ b)) (bd (__dd__ b)))
    (let ((val (__ad_pow__ av bv)))
      (__ad_make_dual__ val
        (__ad_add__ (__ad_mul__ (__ad_mul__ bv (__ad_pow__ av (__ad_sub__ bv 1.0))) ad)
                    (if (and (number? av) (__prim_>__ av 0.0))
                        (__ad_mul__ (__ad_mul__ val (log av)) bd)
                        0.0))))))

;; ================================================================
;; Dispatch helpers for binary ops
;; ================================================================

(define (__ad_add__ a b)
  (cond
    ((and (number? a) (number? b)) (__prim_+__ a b))
    ((__ad_dual_p__ a) (dual-add a b))
    ((__ad_dual_p__ b) (dual-add a b))
    ((__ad_tensor_p__ a) (__ad_tensor_binary__ a b AD_OP_ADD))
    ((__ad_tensor_p__ b) (__ad_tensor_binary__ a b AD_OP_ADD))
    ((__ad_var_p__ a) (__ad_binary__ a b AD_OP_ADD))
    ((__ad_var_p__ b) (__ad_binary__ a b AD_OP_ADD))
    (else (__prim_+__ a b))))

(define (__ad_sub__ a b)
  (cond
    ((and (number? a) (number? b)) (__prim_-__ a b))
    ((__ad_dual_p__ a) (dual-sub a b))
    ((__ad_dual_p__ b) (dual-sub a b))
    ((__ad_tensor_p__ a) (__ad_tensor_binary__ a b AD_OP_SUB))
    ((__ad_tensor_p__ b) (__ad_tensor_binary__ a b AD_OP_SUB))
    ((__ad_var_p__ a) (__ad_binary__ a b AD_OP_SUB))
    ((__ad_var_p__ b) (__ad_binary__ a b AD_OP_SUB))
    (else (__prim_-__ a b))))

(define (__ad_mul__ a b)
  (cond
    ((and (number? a) (number? b)) (__prim_*__ a b))
    ((__ad_dual_p__ a) (dual-mul a b))
    ((__ad_dual_p__ b) (dual-mul a b))
    ((__ad_tensor_p__ a) (__ad_tensor_binary__ a b AD_OP_MUL))
    ((__ad_tensor_p__ b) (__ad_tensor_binary__ a b AD_OP_MUL))
    ((__ad_var_p__ a) (__ad_binary__ a b AD_OP_MUL))
    ((__ad_var_p__ b) (__ad_binary__ a b AD_OP_MUL))
    (else (__prim_*__ a b))))

(define (__ad_div__ a b)
  (cond
    ((and (number? a) (number? b)) (__prim_/__ a b))
    ((__ad_dual_p__ a) (dual-div a b))
    ((__ad_dual_p__ b) (dual-div a b))
    ((__ad_tensor_p__ a) (__ad_tensor_binary__ a b AD_OP_DIV))
    ((__ad_tensor_p__ b) (__ad_tensor_binary__ a b AD_OP_DIV))
    ((__ad_var_p__ a) (__ad_binary__ a b AD_OP_DIV))
    ((__ad_var_p__ b) (__ad_binary__ a b AD_OP_DIV))
    (else (__prim_/__ a b))))

(define (__ad_pow__ a b)
  (cond
    ((and (number? a) (number? b)) (__prim_expt__ a b))
    ((__ad_dual_p__ a) (dual-pow a b))
    ((__ad_dual_p__ b) (dual-pow a b))
    ((__ad_tensor_p__ a) (__ad_tensor_binary__ a b AD_OP_POW))
    ((__ad_tensor_p__ b) (__ad_tensor_binary__ a b AD_OP_POW))
    ((__ad_var_p__ a) (__ad_binary__ a b AD_OP_POW))
    ((__ad_var_p__ b) (__ad_binary__ a b AD_OP_POW))
    (else (__prim_expt__ a b))))

(define (__ad_neg__ a)
  (cond
    ((number? a) (__prim_-__ 0 a))
    ((__ad_dual_p__ a)
     (__ad_make_dual__ (__ad_neg__ (__ad_dual_val__ a))
                       (__ad_neg__ (__ad_dual_dot__ a))))
    ((__ad_tensor_p__ a) (__ad_tensor_unary__ a AD_OP_NEG))
    ((__ad_var_p__ a) (__ad_unary__ a AD_OP_NEG))
    (else (__prim_-__ 0 a))))

;; ================================================================
;; Replace global arithmetic operators
;; ================================================================

;; Variadic + via fold
(set! + (lambda args
  (if (null? args) 0
      (let loop ((rest (cdr args)) (acc (car args)))
        (if (null? rest) acc
            (loop (cdr rest) (__ad_add__ acc (car rest))))))))

;; Variadic - : unary negation or binary subtraction
(set! - (lambda (a . rest)
  (if (null? rest)
      (__ad_neg__ a)
      (let loop ((rest rest) (acc a))
        (if (null? rest) acc
            (loop (cdr rest) (__ad_sub__ acc (car rest))))))))

;; Variadic *
(set! * (lambda args
  (if (null? args) 1
      (let loop ((rest (cdr args)) (acc (car args)))
        (if (null? rest) acc
            (loop (cdr rest) (__ad_mul__ acc (car rest))))))))

;; Binary /
(set! / (lambda (a . rest)
  (if (null? rest)
      (__ad_div__ 1 a)
      (let loop ((rest rest) (acc a))
        (if (null? rest) acc
            (loop (cdr rest) (__ad_div__ acc (car rest))))))))

;; expt
(set! expt (lambda (a b) (__ad_pow__ a b)))

;; ================================================================
;; Comparison operators — pass through to primitives for AD types
;; (compare by value, not by gradient)
;; ================================================================

(define (__ad_value_of__ x)
  (cond ((__ad_var_p__ x) (__ad_var_value__ x))
        ((__ad_dual_p__ x) (__ad_value_of__ (__ad_dual_val__ x)))
        ((__ad_tensor_p__ x) (__ad_tensor_item__ x))
        (else x)))

(set! < (lambda (a b) (__prim_<__ (__ad_value_of__ a) (__ad_value_of__ b))))
(set! > (lambda (a b) (__prim_>__ (__ad_value_of__ a) (__ad_value_of__ b))))
(set! = (lambda (a b) (__prim_=__ (__ad_value_of__ a) (__ad_value_of__ b))))

;; ================================================================
;; Core API
;; ================================================================

(define (Param v)
  (cond ((__ad_tensor_p__ v) (__ad_make_tensor__ (__ad_tensor_data__ v) #t))
        ((pair? v) (__ad_make_tensor__ v #t))
        (else (__ad_make_var__ (exact->inexact v)))))

(define (tensor data)
  (__ad_make_tensor__ data #f))

(define (backward v) (__ad_backward__ v))
(define (zero_grad) (__ad_zero_grad__))
(define (tape_reset) (__ad_tape_reset__))

(define (value_of x) (__ad_value_of__ x))

(define (matmul a b) (__ad_tensor_binary__ a b AD_OP_MATMUL))

;; ================================================================
;; grad(f) — forward-mode via dual numbers (composable)
;; ================================================================

(define (grad f)
  (lambda (x)
    (let* ((xv (if (and (number? x) (exact? x)) (exact->inexact x) x))
           (result (f (__ad_make_dual__ xv 1.0))))
      (if (__ad_dual_p__ result)
          (__ad_dual_dot__ result)
          0.0))))

;; ================================================================
;; Math functions — dispatch on type
;; ================================================================

(define __prim_sin__ (lambda (x) (if (inexact? x) (flsin x) (flsin (exact->inexact x)))))
(define __prim_cos__ (lambda (x) (if (inexact? x) (flcos x) (flcos (exact->inexact x)))))
(define __prim_tan__ (lambda (x) (if (inexact? x) (fltan x) (fltan (exact->inexact x)))))
(define __prim_exp__ (lambda (x) (if (inexact? x) (flexp x) (flexp (exact->inexact x)))))
(define __prim_log__ (lambda (x) (if (inexact? x) (fllog x) (fllog (exact->inexact x)))))
(define __prim_sqrt__ (lambda (x) (if (inexact? x) (flsqrt x) (flsqrt (exact->inexact x)))))

(define (sin x)
  (cond ((number? x) (__prim_sin__ x))
        ((__ad_var_p__ x) (__ad_unary__ x AD_OP_SIN))
        ((__ad_tensor_p__ x) (__ad_tensor_unary__ x AD_OP_SIN))
        ((__ad_dual_p__ x)
         (let ((v (__dv__ x)) (d (__dd__ x)))
           (__ad_make_dual__ (sin v) (__ad_mul__ d (cos v)))))
        (else (__prim_sin__ x))))

(define (cos x)
  (cond ((number? x) (__prim_cos__ x))
        ((__ad_var_p__ x) (__ad_unary__ x AD_OP_COS))
        ((__ad_tensor_p__ x) (__ad_tensor_unary__ x AD_OP_COS))
        ((__ad_dual_p__ x)
         (let ((v (__dv__ x)) (d (__dd__ x)))
           (__ad_make_dual__ (cos v) (__ad_mul__ (__ad_neg__ d) (sin v)))))
        (else (__prim_cos__ x))))

(define (tan x)
  (cond ((number? x) (__prim_tan__ x))
        ((__ad_var_p__ x) (__ad_unary__ x AD_OP_TAN))
        ((__ad_tensor_p__ x) (__ad_tensor_unary__ x AD_OP_TAN))
        ((__ad_dual_p__ x)
         (let ((v (__dv__ x)) (d (__dd__ x)))
           (let ((c (cos v)))
             (__ad_make_dual__ (tan v) (__ad_div__ d (__ad_mul__ c c))))))
        (else (__prim_tan__ x))))

(define (exp x)
  (cond ((number? x) (__prim_exp__ x))
        ((__ad_var_p__ x) (__ad_unary__ x AD_OP_EXP))
        ((__ad_tensor_p__ x) (__ad_tensor_unary__ x AD_OP_EXP))
        ((__ad_dual_p__ x)
         (let ((v (__dv__ x)) (d (__dd__ x)))
           (let ((ev (exp v)))
             (__ad_make_dual__ ev (__ad_mul__ d ev)))))
        (else (__prim_exp__ x))))

(define (log x)
  (cond ((number? x) (__prim_log__ x))
        ((__ad_var_p__ x) (__ad_unary__ x AD_OP_LOG))
        ((__ad_tensor_p__ x) (__ad_tensor_unary__ x AD_OP_LOG))
        ((__ad_dual_p__ x)
         (let ((v (__dv__ x)) (d (__dd__ x)))
           (__ad_make_dual__ (log v) (__ad_div__ d v))))
        (else (__prim_log__ x))))

(define (sqrt x)
  (cond ((number? x) (__prim_sqrt__ x))
        ((__ad_var_p__ x) (__ad_unary__ x AD_OP_SQRT))
        ((__ad_tensor_p__ x) (__ad_tensor_unary__ x AD_OP_SQRT))
        ((__ad_dual_p__ x)
         (let ((v (__dv__ x)) (d (__dd__ x)))
           (let ((sv (sqrt v)))
             (__ad_make_dual__ sv (__ad_div__ d (__ad_mul__ 2.0 sv))))))
        (else (__prim_sqrt__ x))))

(define (tanh x)
  (cond ((number? x) (let ((ex (__prim_exp__ x)) (enx (__prim_exp__ (__prim_-__ 0 x))))
                       (__prim_/__ (__prim_-__ ex enx) (__prim_+__ ex enx))))
        ((__ad_var_p__ x) (__ad_unary__ x AD_OP_TANH))
        ((__ad_tensor_p__ x) (__ad_tensor_unary__ x AD_OP_TANH))
        ((__ad_dual_p__ x)
         (let ((v (__dv__ x)) (d (__dd__ x)))
           (let ((tv (tanh v)))
             (__ad_make_dual__ tv (__ad_mul__ d (__ad_sub__ 1.0 (__ad_mul__ tv tv)))))))
        (else (tanh (exact->inexact x)))))

(define (sigmoid x)
  (cond ((number? x) (__prim_/__ 1.0 (__prim_+__ 1.0 (__prim_exp__ (__prim_-__ 0 x)))))
        ((__ad_var_p__ x) (__ad_unary__ x AD_OP_SIGMOID))
        ((__ad_tensor_p__ x) (__ad_tensor_unary__ x AD_OP_SIGMOID))
        ((__ad_dual_p__ x)
         (let ((v (__dv__ x)) (d (__dd__ x)))
           (let ((sv (sigmoid v)))
             (__ad_make_dual__ sv (__ad_mul__ d (__ad_mul__ sv (__ad_sub__ 1.0 sv)))))))
        (else (sigmoid (exact->inexact x)))))

(define (relu x)
  (cond ((number? x) (if (__prim_>__ x 0) x 0))
        ((__ad_var_p__ x) (__ad_unary__ x AD_OP_RELU))
        ((__ad_tensor_p__ x) (__ad_tensor_unary__ x AD_OP_RELU))
        ((__ad_dual_p__ x)
         (let ((v (__dv__ x)) (d (__dd__ x)))
           (if (__prim_>__ (value_of v) 0)
               (__ad_make_dual__ v d)
               (__ad_make_dual__ 0.0 0.0))))
        (else (relu (exact->inexact x)))))

;; Tensor reductions
(define (sum x)
  (cond ((__ad_tensor_p__ x) (__ad_tensor_reduce__ x AD_OP_SUM))
        ((pair? x) (apply + x))
        (else x)))

(define (mean x)
  (cond ((__ad_tensor_p__ x) (__ad_tensor_reduce__ x AD_OP_MEAN))
        ((pair? x) (/ (apply + x) (length x)))
        (else x)))

(define (transpose x)
  (if (__ad_tensor_p__ x)
      (__ad_tensor_unary__ x AD_OP_TRANSPOSE)
      x))

;; ================================================================
;; abs — override for AD support
;; ================================================================
(define __prim_abs__ abs)
(set! abs (lambda (x)
  (cond ((number? x) (__prim_abs__ x))
        ((__ad_var_p__ x) (__ad_unary__ x AD_OP_ABS))
        ((__ad_tensor_p__ x) (__ad_tensor_unary__ x AD_OP_ABS))
        (else (__prim_abs__ x)))))

;; ================================================================
;; no_grad block
;; ================================================================

(define (no_grad thunk)
  (__ad_set_enabled__ #f)
  (let ((result (thunk)))
    (__ad_set_enabled__ #t)
    result))

;; ================================================================
;; SGD Optimizer
;; ================================================================

(define (SGD params lr)
  (lambda (__msg__)
    (cond
      ((eq? __msg__ 'step)
       (lambda ()
         (let loop ((ps params))
           (if (null? ps) #t
               (let ((p (car ps)))
                 (cond
                   ((__ad_var_p__ p)
                    (__ad_var_set__ p (- (__ad_var_value__ p)
                                        (* lr (__ad_var_grad__ p)))))
                   ;; Tensor params: update element-wise via data/grad access
                   ;; (simple for now — full implementation would need C-level)
                   )
                 (loop (cdr ps)))))))
      ((eq? __msg__ 'zero_grad)
       (lambda () (__ad_zero_grad__)))
      ((eq? __msg__ 'params) params)
      ((eq? __msg__ 'lr) lr)
      (else (error "SGD: unknown method" __msg__)))))

;; ================================================================
;; __send__ dispatch for Var and Tensor
;; ================================================================

(let ((__prev-send__ __send__))
  (set! __send__
    (lambda (obj msg)
      (cond
        ((__ad_var_p__ obj)
         (cond ((eq? msg 'grad)    (__ad_var_grad__ obj))
               ((eq? msg 'val)     (__ad_var_value__ obj))
               ((eq? msg 'value)   (__ad_var_value__ obj))
               ((eq? msg 'set)     (lambda (v) (__ad_var_set__ obj v)))
               ((eq? msg '__type__) '__var__)
               (else (error "Var: unknown message" msg))))
        ((__ad_tensor_p__ obj)
         (cond ((eq? msg 'grad)    (__ad_tensor_grad__ obj))
               ((eq? msg 'shape)   (__ad_tensor_shape__ obj))
               ((eq? msg 'data)    (__ad_tensor_data__ obj))
               ((eq? msg 'item)    (__ad_tensor_item__ obj))
               ((eq? msg 'T)       (transpose obj))
               ((eq? msg '__type__) '__tensor__)
               (else (error "Tensor: unknown message" msg))))
        (else (__prev-send__ obj msg))))))
