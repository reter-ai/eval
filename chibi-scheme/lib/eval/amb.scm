;; eval/amb.scm -- Nondeterministic choice with backtracking
;; Uses continuations and syntax-rules macro for lazy evaluation.

(define __amb_fail__
  (lambda () (error "amb: no more choices")))

(define-syntax amb
  (syntax-rules ()
    ((amb) (__amb_fail__))
    ((amb x) x)
    ((amb x rest ...)
     (let ((saved-fail __amb_fail__))
       (call-with-current-continuation
         (lambda (k)
           (set! __amb_fail__
             (lambda ()
               (set! __amb_fail__ saved-fail)
               (k (amb rest ...))))
           x))))))

(define (require pred)
  (if (not pred) (__amb_fail__)))

(define (amb-collect thunk)
  (let ((results '()))
    (let ((saved-fail __amb_fail__))
      (call-with-current-continuation
        (lambda (exit)
          (set! __amb_fail__
            (lambda ()
              (set! __amb_fail__ saved-fail)
              (exit (reverse results))))
          (let loop ()
            (let ((val (thunk)))
              (set! results (cons val results))
              (__amb_fail__))))))))

(define amb_collect amb-collect)
