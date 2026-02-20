;; eval/money-oo.scm -- Money type wrapping Decimal + currency
;; Loaded after decimal-oo.scm. Chains onto previous __send__ dispatch.
;; Money is a pure Scheme closure (no C type needed).

(define __money_tag__ (list 'money))

(define (__money? x)
  (and (procedure? x)
       (eq? (x '__type__) __money_tag__)))

(define (Money amount currency)
  (let ((d (if (%decimal? amount) amount (Decimal amount))))
    (lambda (msg)
      (cond
        ((eq? msg '__type__)  __money_tag__)
        ((eq? msg 'amount)    d)
        ((eq? msg 'currency)  currency)
        ((eq? msg 'add)
         (lambda (other)
           (if (not (string=? (other 'currency) currency))
               (error "Money: cannot add different currencies"
                      currency (other 'currency))
               (Money (%decimal-add d (other 'amount)) currency))))
        ((eq? msg 'sub)
         (lambda (other)
           (if (not (string=? (other 'currency) currency))
               (error "Money: cannot subtract different currencies"
                      currency (other 'currency))
               (Money (%decimal-sub d (other 'amount)) currency))))
        ((eq? msg 'mul)
         (lambda (factor)
           (Money (%decimal-mul d (if (%decimal? factor) factor (Decimal factor)))
                  currency)))
        ((eq? msg 'div)
         (lambda (divisor . prec)
           (Money (%decimal-div d
                    (if (%decimal? divisor) divisor (Decimal divisor))
                    (if (pair? prec) (car prec) 2))
                  currency)))
        ((eq? msg 'format)
         (lambda ()
           (%decimal-to-string (%decimal-round d 2 0))))
        ((eq? msg 'round)
         (lambda (places)
           (Money (%decimal-round d places 0) currency)))
        ((eq? msg 'lt)
         (lambda (o) (< (%decimal-compare d (o 'amount)) 0)))
        ((eq? msg 'gt)
         (lambda (o) (> (%decimal-compare d (o 'amount)) 0)))
        ((eq? msg 'eq)
         (lambda (o) (= (%decimal-compare d (o 'amount)) 0)))
        (else (error "Money: unknown method" msg))))))

;; ================================================================
;; Chain onto previous __send__
;; Money is a closure, so check __money? before falling through.
;; Since Money objects are procedures, we intercept in __send__.
;; ================================================================

(let ((__prev-send__ __send__))
  (set! __send__
    (lambda (obj msg)
      (if (and (procedure? obj)
               ;; Quick check: try to call with __type__ and see if it's money
               ;; We need a safe way to detect money closures
               (let ((r #f))
                 (guard (exn (#t #f))
                   (set! r (obj '__type__))
                   (eq? r __money_tag__))))
          (obj msg)
          (__prev-send__ obj msg)))))
