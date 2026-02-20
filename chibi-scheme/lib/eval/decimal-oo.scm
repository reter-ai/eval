;; eval/decimal-oo.scm -- OO wrapper for Decimal type
;; Loaded after datetime-oo.scm. Chains onto previous __send__ dispatch.

;; ================================================================
;; Decimal constructor
;; ================================================================

(define (Decimal val . rest)
  (cond
    ((string? val)   (%decimal-from-string val))
    ((integer? val)  (%decimal-from-number val))
    ((real? val)     (%decimal-from-number val))
    ((%decimal? val) val)  ;; identity
    (else (error "Decimal: expected string or number" val))))

;; ================================================================
;; Decimal __send__ dispatch
;; ================================================================

(define (__decimal-send__ obj msg)
  (cond
    ;; Properties
    ((eq? msg 'mantissa) (%decimal-mantissa obj))
    ((eq? msg 'scale)    (%decimal-scale obj))
    ;; Arithmetic methods (auto-coerce non-decimal args)
    ((eq? msg 'add)  (lambda (o) (%decimal-add obj (if (%decimal? o) o (Decimal o)))))
    ((eq? msg 'sub)  (lambda (o) (%decimal-sub obj (if (%decimal? o) o (Decimal o)))))
    ((eq? msg 'mul)  (lambda (o) (%decimal-mul obj (if (%decimal? o) o (Decimal o)))))
    ((eq? msg 'div)  (lambda (o . prec) (%decimal-div obj (if (%decimal? o) o (Decimal o))
                                          (if (pair? prec) (car prec) 28))))
    ((eq? msg 'negate) (lambda () (%decimal-negate obj)))
    ((eq? msg 'abs)    (lambda () (%decimal-abs obj)))
    ;; Comparison
    ((eq? msg 'lt)  (lambda (o) (< (%decimal-compare obj (if (%decimal? o) o (Decimal o))) 0)))
    ((eq? msg 'gt)  (lambda (o) (> (%decimal-compare obj (if (%decimal? o) o (Decimal o))) 0)))
    ((eq? msg 'eq)  (lambda (o) (= (%decimal-compare obj (if (%decimal? o) o (Decimal o))) 0)))
    ((eq? msg 'lte) (lambda (o) (<= (%decimal-compare obj (if (%decimal? o) o (Decimal o))) 0)))
    ((eq? msg 'gte) (lambda (o) (>= (%decimal-compare obj (if (%decimal? o) o (Decimal o))) 0)))
    ;; Rounding
    ((eq? msg 'round)    (lambda (places) (%decimal-round obj places 0)))
    ((eq? msg 'ceil)     (lambda (places) (%decimal-round obj places 1)))
    ((eq? msg 'floor)    (lambda (places) (%decimal-round obj places 2)))
    ((eq? msg 'truncate) (lambda (places) (%decimal-round obj places 3)))
    ;; Conversion
    ((eq? msg 'to_string) (lambda () (%decimal-to-string obj)))
    ((eq? msg 'to_number) (lambda () (%decimal-to-number obj)))
    ((eq? msg 'format)    (lambda (places) (%decimal-to-string (%decimal-round obj places 0))))
    (else (error "Decimal: unknown method" msg))))

;; ================================================================
;; Chain onto previous __send__
;; ================================================================

(let ((__prev-send__ __send__))
  (set! __send__
    (lambda (obj msg)
      (cond
        ((%decimal? obj) (__decimal-send__ obj msg))
        (else (__prev-send__ obj msg))))))
