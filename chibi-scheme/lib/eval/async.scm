;; eval/async.scm -- Async/await runtime: promise type backed by mutex+condvar

(define (__make-promise__)
  (let ((m (make-mutex)) (cv (make-condition-variable))
        (resolved #f) (value #f) (err #f))
    (lambda (__msg__)
      (cond
        ((eq? __msg__ '__resolve__)
         (lambda (v)
           (mutex-lock! m)
           (set! resolved #t) (set! value v)
           (condition-variable-broadcast! cv)
           (mutex-unlock! m)))
        ((eq? __msg__ '__reject__)
         (lambda (e)
           (mutex-lock! m)
           (set! resolved #t) (set! err e)
           (condition-variable-broadcast! cv)
           (mutex-unlock! m)))
        ((eq? __msg__ '__await__)
         (mutex-lock! m)
         (let loop ()
           (if resolved
               (begin (mutex-unlock! m)
                      (if err (raise err) value))
               (begin (mutex-unlock! m cv)
                      (mutex-lock! m)
                      (loop)))))
        ((eq? __msg__ 'ready?)
         (mutex-lock! m)
         (let ((r resolved)) (mutex-unlock! m) r))
        ((eq? __msg__ '__type__) '__promise__)
        (else (error "promise: unknown message" __msg__))))))

;; Pool target for `parallel async` (set via set_async_pool / AsyncPool)
(define __async_pool__ #f)

;; async expr — always green thread (cooperative, shared state)
(define (__async_dispatch__ thunk)
  (let ((p (__make-promise__)))
    (thread-start! (make-thread (lambda ()
      (protect (__async_e__
        (else ((p '__reject__) __async_e__)))
        ((p '__resolve__) (thunk))))))
    p))

;; parallel async expr — always OS thread via __async_pool__
(define (__parallel_async_dispatch__ thunk)
  (if __async_pool__
      ((__async_pool__ 'submit) thunk)
      (error "parallel async requires a pool -- use AsyncPool(n) or set_async_pool(pool)")))

(define (__promise-resolve!__ p val)
  (protect (e (else ((p '__reject__) e)))
    ((p '__resolve__) val)))

(define (promise? v)
  (and (procedure? v)
       (protect (e (else #f))
         (eq? (v '__type__) '__promise__))))

;; __await__: dispatches on type -- promise, OO future, or raw cpointer future
(define (__await__ x)
  (cond
    ((and (procedure? x)
          (protect (e (else #f)) (eq? (x '__type__) '__promise__)))
     (x '__await__))
    ((and (procedure? x)
          (protect (e (else #f)) (eq? (x '__type__) '__future__)))
     (x '__await__))
    (else (future-result x))))
