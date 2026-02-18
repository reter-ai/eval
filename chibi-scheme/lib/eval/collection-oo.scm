;; eval/collection-oo.scm -- OO methods for lists and vectors via __send__
;; Loaded after string-oo.scm. Chains onto previous __send__ dispatch.

;; Helper: join list of strings with separator
(define (__list-join__ lst sep)
  (if (null? lst) ""
      (if (null? (cdr lst)) (car lst)
          (string-concatenate
            (cons (car lst)
                  (let loop ((rest (cdr lst)))
                    (if (null? rest) '()
                        (cons sep (cons (car rest) (loop (cdr rest)))))))))))

;; Helper: apply a list operation on a vector, returning a vector
(define (__vector-via-list__ fn obj)
  (list->vector (fn (vector->list obj))))

;; List dispatch
(define (__list-send__ obj msg)
  (cond
    ;; Properties
    ((eq? msg 'length)     (length obj))
    ((eq? msg 'empty?)     (null? obj))
    ((eq? msg 'first)      (car obj))
    ((eq? msg 'second)     (cadr obj))
    ((eq? msg 'third)      (car (cdr (cdr obj))))
    ((eq? msg 'rest)       (cdr obj))
    ((eq? msg 'last)       (last obj))
    ;; Methods
    ((eq? msg 'ref)        (lambda (i) (ref obj i)))
    ((eq? msg 'slice)      (lambda (start end) (slice obj start end)))
    ((eq? msg 'map)        (lambda (fn) (map fn obj)))
    ((eq? msg 'filter)     (lambda (fn) (filter fn obj)))
    ((eq? msg 'reject)     (lambda (fn) (remove fn obj)))
    ((eq? msg 'fold)       (lambda (fn init) (fold fn init obj)))
    ((eq? msg 'fold_right) (lambda (fn init) (fold-right fn init obj)))
    ((eq? msg 'for_each)   (lambda (fn) (for-each fn obj)))
    ((eq? msg 'flat_map)   (lambda (fn) (append-map fn obj)))
    ((eq? msg 'any)        (lambda (pred) (if (any pred obj) #t #f)))
    ((eq? msg 'every)      (lambda (pred) (if (every pred obj) #t #f)))
    ((eq? msg 'find)       (lambda (pred) (find pred obj)))
    ((eq? msg 'count)      (lambda (pred) (count pred obj)))
    ((eq? msg 'index_of)   (lambda (pred) (list-index pred obj)))
    ((eq? msg 'contains)   (lambda (x) (if (member x obj) #t #f)))
    ((eq? msg 'reverse)    (lambda () (reverse obj)))
    ((eq? msg 'append)     (lambda (other) (append obj other)))
    ((eq? msg 'flatten)    (lambda () (apply append obj)))
    ((eq? msg 'take)       (lambda (n) (take obj n)))
    ((eq? msg 'drop)       (lambda (n) (drop obj n)))
    ((eq? msg 'take_while) (lambda (pred) (take-while pred obj)))
    ((eq? msg 'drop_while) (lambda (pred) (drop-while pred obj)))
    ((eq? msg 'sort)       (lambda (cmp) (sort obj cmp)))
    ((eq? msg 'unique)     (lambda () (delete-duplicates obj)))
    ((eq? msg 'partition)  (lambda (pred)
                             (let-values (((yes no) (partition pred obj)))
                               (list yes no))))
    ((eq? msg 'zip)        (lambda (other) (zip obj other)))
    ((eq? msg 'join)       (lambda (sep) (__list-join__ obj sep)))
    ((eq? msg 'to_vector)  (lambda () (list->vector obj)))
    ((eq? msg 'copy)       (lambda () (list-copy obj)))
    (else (error "unknown list method" msg))))

;; Vector dispatch
(define (__vector-send__ obj msg)
  (cond
    ;; Properties
    ((eq? msg 'length)     (vector-length obj))
    ((eq? msg 'empty?)     (= (vector-length obj) 0))
    ((eq? msg 'first)      (vector-ref obj 0))
    ((eq? msg 'second)     (vector-ref obj 1))
    ((eq? msg 'third)      (vector-ref obj 2))
    ((eq? msg 'last)       (vector-ref obj (- (vector-length obj) 1)))
    ((eq? msg 'rest)       (vector-copy obj 1))
    ;; Methods
    ((eq? msg 'ref)        (lambda (i) (ref obj i)))
    ((eq? msg 'slice)      (lambda (start end) (slice obj start end)))
    ((eq? msg 'map)        (lambda (fn) (vector-map fn obj)))
    ((eq? msg 'for_each)   (lambda (fn) (vector-for-each fn obj)))
    ((eq? msg 'fold)       (lambda (fn init) (fold fn init (vector->list obj))))
    ((eq? msg 'any)        (lambda (pred) (if (any pred (vector->list obj)) #t #f)))
    ((eq? msg 'every)      (lambda (pred) (if (every pred (vector->list obj)) #t #f)))
    ((eq? msg 'find)       (lambda (pred) (find pred (vector->list obj))))
    ((eq? msg 'count)      (lambda (pred) (count pred (vector->list obj))))
    ((eq? msg 'index_of)   (lambda (pred) (list-index pred (vector->list obj))))
    ((eq? msg 'contains)   (lambda (x) (if (member x (vector->list obj)) #t #f)))
    ((eq? msg 'filter)     (lambda (fn) (__vector-via-list__ (lambda (l) (filter fn l)) obj)))
    ((eq? msg 'reject)     (lambda (fn) (__vector-via-list__ (lambda (l) (remove fn l)) obj)))
    ((eq? msg 'reverse)    (lambda () (__vector-via-list__ reverse obj)))
    ((eq? msg 'sort)       (lambda (cmp) (__vector-via-list__ (lambda (l) (sort l cmp)) obj)))
    ((eq? msg 'unique)     (lambda () (__vector-via-list__ delete-duplicates obj)))
    ((eq? msg 'append)     (lambda (other) (vector-append obj other)))
    ((eq? msg 'to_list)    (lambda () (vector->list obj)))
    ((eq? msg 'copy)       (lambda () (vector-copy obj)))
    ((eq? msg 'set)        (lambda (i val) (vector-set! obj i val)))
    (else (error "unknown vector method" msg))))

;; Chain onto previous __send__
(let ((__prev-send__ __send__))
  (set! __send__
    (lambda (obj msg)
      (cond
        ((pair? obj)   (__list-send__ obj msg))
        ((vector? obj) (__vector-send__ obj msg))
        (else (__prev-send__ obj msg))))))
