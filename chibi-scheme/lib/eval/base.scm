;; eval/base.scm -- Pre-extras standard definitions
;; Loaded before extras.scm in both Python and worker contexts.

;; Error object aliases (extras.scm expects these)
(define error-object? exception?)
(define error-object-message exception-message)

;; Byte I/O — pure Scheme via char I/O
(define (read-u8 . o)
  (let ((ch (if (pair? o) (read-char (car o)) (read-char))))
    (if (eof-object? ch) ch (char->integer ch))))
(define (write-u8 byte . o)
  (if (pair? o)
    (write-char (integer->char byte) (car o))
    (write-char (integer->char byte))))
(define (peek-u8 . o)
  (let ((ch (if (pair? o) (peek-char (car o)) (peek-char))))
    (if (eof-object? ch) ch (char->integer ch))))

;; String functions
(define (string-map proc str)
  (list->string (map proc (string->list str))))
(define (string-for-each proc str)
  (for-each proc (string->list str)))

;; I/O functions
(define (write-string str . o)
  (let ((port (if (pair? o) (car o) (current-output-port))))
    (let lp ((i 0))
      (if (< i (string-length str))
          (begin (write-char (string-ref str i) port)
                 (lp (+ i 1)))))))
(define (read-line . o)
  (let ((port (if (pair? o) (car o) (current-input-port))))
    (let lp ((res '()))
      (let ((ch (read-char port)))
        (cond ((eof-object? ch)
               (if (null? res) ch (list->string (reverse res))))
              ((eqv? ch #\newline)
               (list->string (reverse res)))
              ((eqv? ch #\return)
               (let ((next (peek-char port)))
                 (if (eqv? next #\newline) (read-char port))
                 (list->string (reverse res))))
              (else (lp (cons ch res))))))))
(define (read-string n . o)
  (let ((port (if (pair? o) (car o) (current-input-port))))
    (let lp ((i 0) (res '()))
      (if (>= i n)
          (list->string (reverse res))
          (let ((ch (read-char port)))
            (if (eof-object? ch)
                (if (null? res) ch (list->string (reverse res)))
                (lp (+ i 1) (cons ch res))))))))

;; Core aliases
(define callcc call-with-current-continuation)

;; OO dispatch: procedure? → direct call (backward compat), extended later for strings
(define (__send__ obj msg) (obj msg))
