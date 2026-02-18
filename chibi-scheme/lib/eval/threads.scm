;; eval/threads.scm -- SRFI-18 green thread wrappers

(define thread-yield! yield!)

(define (thread-join! thread . o)
  (let ((timeout (and (pair? o) (car o))))
    (let lp ()
      (cond
       ((%thread-join! thread timeout)
        (if (%thread-exception? thread)
            (raise (%thread-end-result thread))
            (%thread-end-result thread)))
       (else
        (thread-yield!)
        (cond
         ((and timeout (thread-timeout?))
          (if (and (pair? o) (pair? (cdr o)))
              (cadr o)
              (error "timed out waiting for thread" thread)))
         (else (lp))))))))

(define (thread-terminate! thread)
  (if (%thread-terminate! thread) (thread-yield!)))

(define (thread-sleep! timeout)
  (%thread-sleep! timeout) (thread-yield!))

(define (mutex-lock! mutex . o)
  (let ((timeout (and (pair? o) (car o)))
        (thread (if (and (pair? o) (pair? (cdr o))) (cadr o) #t)))
    (cond
     ((%mutex-lock! mutex timeout thread))
     (else
      (thread-yield!)
      (if (thread-timeout?) #f
          (mutex-lock! mutex timeout thread))))))

(define (mutex-unlock! mutex . o)
  (let ((condvar (and (pair? o) (car o)))
        (timeout (if (and (pair? o) (pair? (cdr o))) (cadr o) #f)))
    (cond
     ((%mutex-unlock! mutex condvar timeout))
     (else (thread-yield!) (not (thread-timeout?))))))
