;; test.scm -- Lightweight test framework for Eval
;; Loaded at context init (no module system needed).
;; Uses bridge display/newline for output.

(define %test-pass 0)
(define %test-fail 0)
(define %test-error 0)
(define %test-group-name #f)

(define (test-begin name)
  (set! %test-pass 0)
  (set! %test-fail 0)
  (set! %test-error 0)
  (set! %test-group-name name)
  (display "-- ")
  (display name)
  (display " -------")
  (newline))

(define (test-end)
  (let ((fails (+ %test-fail %test-error)))
    (if %test-group-name
        (begin
          (display "-- ")
          (display %test-pass) (display " pass, ")
          (display %test-fail) (display " fail, ")
          (display %test-error) (display " error")
          (newline)))
    (set! %test-group-name #f)
    fails))

(define (%test-display-name name)
  (if name (begin (display ": ") (display name))))

(define (test . args)
  (cond
    ((= (length args) 2)
     (%test-check #f (car args) (cadr args)))
    ((= (length args) 3)
     (%test-check (car args) (cadr args) (car (cdr (cdr args)))))
    (else
     (display "  ERROR: test requires 2 or 3 arguments")
     (newline)
     (set! %test-error (+ %test-error 1)))))

(define (%test-check name expected actual)
  (if (%test-equal? expected actual)
      (begin
        (set! %test-pass (+ %test-pass 1))
        (display "  PASS")
        (%test-display-name name)
        (newline))
      (begin
        (set! %test-fail (+ %test-fail 1))
        (display "  FAIL")
        (%test-display-name name)
        (newline)
        (display "    expected: ") (display expected) (newline)
        (display "    actual:   ") (display actual) (newline))))

(define (%test-equal? a b)
  (or (equal? a b)
      (and (number? a) (number? b)
           (or (inexact? a) (inexact? b))
           (< (abs (- a b)) 1e-10))))

(define (test-assert . args)
  (cond
    ((= (length args) 1)
     (%test-assert-check #f (car args)))
    ((= (length args) 2)
     (%test-assert-check (car args) (cadr args)))
    (else
     (display "  ERROR: test-assert requires 1 or 2 arguments")
     (newline)
     (set! %test-error (+ %test-error 1)))))

(define (%test-assert-check name value)
  (if value
      (begin
        (set! %test-pass (+ %test-pass 1))
        (display "  PASS")
        (%test-display-name name)
        (newline))
      (begin
        (set! %test-fail (+ %test-fail 1))
        (display "  FAIL")
        (%test-display-name name)
        (newline)
        (display "    value was false")
        (newline))))

(define (test-error . args)
  (cond
    ((= (length args) 1)
     (%test-error-check #f (car args)))
    ((= (length args) 2)
     (%test-error-check (car args) (cadr args)))
    (else
     (display "  ERROR: test-error requires 1 or 2 arguments")
     (newline)
     (set! %test-error (+ %test-error 1)))))

(define (%test-error-check name thunk)
  (let ((result
         (call-with-current-continuation
           (lambda (k)
             (with-exception-handler
               (lambda (exn) (k 'caught))
               thunk)))))
    (if (eq? result 'caught)
        (begin
          (set! %test-pass (+ %test-pass 1))
          (display "  PASS")
          (%test-display-name name)
          (newline))
        (begin
          (set! %test-fail (+ %test-fail 1))
          (display "  FAIL")
          (%test-display-name name)
          (newline)
          (display "    expected error but got none")
          (newline)))))

(define (test-group name thunk)
  (test-begin name)
  (call-with-current-continuation
    (lambda (k)
      (with-exception-handler
        (lambda (exn)
          (set! %test-error (+ %test-error 1))
          (display "  ERROR: uncaught exception: ")
          (display (if (exception? exn)
                       (exception-message exn)
                       exn))
          (newline)
          (k #f))
        thunk)))
  (test-end))
