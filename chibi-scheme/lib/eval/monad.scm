;; eval/monad.scm -- Category theory toolkit for Eval
;; Maybe, Either, Validation, Writer, Reader, State, Monoid,
;; Traversable, Natural Transformations, Lenses, combinators.

;; ===== Core dispatch =====

(define (monad-bind m f) ((__send__ m 'bind) f))
(define (monad-then m next) ((__send__ m 'bind) (lambda (_) next)))

;; ===== Maybe: Some(x) / None =====

(define (Some x)
  (lambda (__msg__)
    (cond
      ((eq? __msg__ 'value)      x)
      ((eq? __msg__ 'is_some)    #t)
      ((eq? __msg__ 'is_none)    #f)
      ((eq? __msg__ 'unwrap)     x)
      ((eq? __msg__ '__type__)   'Some)
      ((eq? __msg__ 'map)        (lambda (f) (Some (f x))))
      ((eq? __msg__ 'bind)       (lambda (f) (f x)))
      ((eq? __msg__ 'flat_map)   (lambda (f) (f x)))
      ((eq? __msg__ 'apply)      (lambda (m) ((__send__ m 'map) x)))
      ((eq? __msg__ 'unwrap_or)  (lambda (d) x))
      ((eq? __msg__ 'or_else)    (lambda (alt) (Some x)))
      ((eq? __msg__ 'filter)     (lambda (pred) (if (pred x) (Some x) None)))
      ((eq? __msg__ 'pure)       (lambda (v) (Some v)))
      ((eq? __msg__ 'mappend)    (lambda (other)
                                   (if (__send__ other 'is_some)
                                       (Some (mappend x (__send__ other 'value)))
                                       (Some x))))
      ((eq? __msg__ 'match)      (lambda (on_some on_none) (on_some x)))
      ((eq? __msg__ 'to_list)    (lambda () (list x)))
      ((eq? __msg__ 'to_result)  (lambda (err) (Ok x)))
      (else (error "Maybe: unknown message" __msg__)))))

(define None
  (lambda (__msg__)
    (cond
      ((eq? __msg__ 'value)      (error "None has no value"))
      ((eq? __msg__ 'is_some)    #f)
      ((eq? __msg__ 'is_none)    #t)
      ((eq? __msg__ 'unwrap)     (error "cannot unwrap None"))
      ((eq? __msg__ '__type__)   'None)
      ((eq? __msg__ 'map)        (lambda (f) None))
      ((eq? __msg__ 'bind)       (lambda (f) None))
      ((eq? __msg__ 'flat_map)   (lambda (f) None))
      ((eq? __msg__ 'apply)      (lambda (m) None))
      ((eq? __msg__ 'unwrap_or)  (lambda (d) d))
      ((eq? __msg__ 'or_else)    (lambda (alt) (alt)))
      ((eq? __msg__ 'filter)     (lambda (pred) None))
      ((eq? __msg__ 'pure)       (lambda (v) (Some v)))
      ((eq? __msg__ 'mappend)    (lambda (other) other))
      ((eq? __msg__ 'match)      (lambda (on_some on_none) (on_none)))
      ((eq? __msg__ 'to_list)    (lambda () '()))
      ((eq? __msg__ 'to_result)  (lambda (err) (Err err)))
      (else (error "Maybe: unknown message" __msg__)))))

;; ===== Either: Ok(x) / Err(e) =====

(define (Ok x)
  (lambda (__msg__)
    (cond
      ((eq? __msg__ 'value)      x)
      ((eq? __msg__ 'error)      (error "Ok has no error"))
      ((eq? __msg__ 'is_ok)      #t)
      ((eq? __msg__ 'is_err)     #f)
      ((eq? __msg__ 'unwrap)     x)
      ((eq? __msg__ '__type__)   'Ok)
      ((eq? __msg__ 'map)        (lambda (f) (Ok (f x))))
      ((eq? __msg__ 'map_err)    (lambda (f) (Ok x)))
      ((eq? __msg__ 'bind)       (lambda (f) (f x)))
      ((eq? __msg__ 'flat_map)   (lambda (f) (f x)))
      ((eq? __msg__ 'apply)      (lambda (r) ((__send__ r 'map) x)))
      ((eq? __msg__ 'unwrap_or)  (lambda (d) x))
      ((eq? __msg__ 'or_else)    (lambda (alt) (Ok x)))
      ((eq? __msg__ 'pure)       (lambda (v) (Ok v)))
      ((eq? __msg__ 'match)      (lambda (on_ok on_err) (on_ok x)))
      ((eq? __msg__ 'to_maybe)   (lambda () (Some x)))
      (else (error "Either: unknown message" __msg__)))))

(define (Err e)
  (lambda (__msg__)
    (cond
      ((eq? __msg__ 'value)      (error "Err has no value" e))
      ((eq? __msg__ 'error)      e)
      ((eq? __msg__ 'is_ok)      #f)
      ((eq? __msg__ 'is_err)     #t)
      ((eq? __msg__ 'unwrap)     (error "cannot unwrap Err" e))
      ((eq? __msg__ '__type__)   'Err)
      ((eq? __msg__ 'map)        (lambda (f) (Err e)))
      ((eq? __msg__ 'map_err)    (lambda (f) (Err (f e))))
      ((eq? __msg__ 'bind)       (lambda (f) (Err e)))
      ((eq? __msg__ 'flat_map)   (lambda (f) (Err e)))
      ((eq? __msg__ 'apply)      (lambda (r) (Err e)))
      ((eq? __msg__ 'unwrap_or)  (lambda (d) d))
      ((eq? __msg__ 'or_else)    (lambda (alt) (alt e)))
      ((eq? __msg__ 'pure)       (lambda (v) (Ok v)))
      ((eq? __msg__ 'match)      (lambda (on_ok on_err) (on_err e)))
      ((eq? __msg__ 'to_maybe)   (lambda () None))
      (else (error "Either: unknown message" __msg__)))))

;; ===== Validation: Valid(x) / Invalid(errors) =====

(define (Valid x)
  (lambda (__msg__)
    (cond
      ((eq? __msg__ 'value)      x)
      ((eq? __msg__ 'errors)     (error "Valid has no errors"))
      ((eq? __msg__ 'is_valid)   #t)
      ((eq? __msg__ 'is_invalid) #f)
      ((eq? __msg__ '__type__)   'Valid)
      ((eq? __msg__ 'map)        (lambda (f) (Valid (f x))))
      ((eq? __msg__ 'bind)       (lambda (f) (f x)))
      ((eq? __msg__ 'apply)      (lambda (v)
                                   (if (__send__ v 'is_valid)
                                       (Valid (x (__send__ v 'value)))
                                       v)))
      ((eq? __msg__ 'pure)       (lambda (v) (Valid v)))
      ((eq? __msg__ 'to_result)  (lambda () (Ok x)))
      ((eq? __msg__ 'match)      (lambda (on_valid on_invalid) (on_valid x)))
      (else (error "Validation: unknown message" __msg__)))))

(define (Invalid errors)
  (lambda (__msg__)
    (cond
      ((eq? __msg__ 'value)      (error "Invalid has no value" errors))
      ((eq? __msg__ 'errors)     errors)
      ((eq? __msg__ 'is_valid)   #f)
      ((eq? __msg__ 'is_invalid) #t)
      ((eq? __msg__ '__type__)   'Invalid)
      ((eq? __msg__ 'map)        (lambda (f) (Invalid errors)))
      ((eq? __msg__ 'bind)       (lambda (f) (Invalid errors)))
      ((eq? __msg__ 'apply)      (lambda (v)
                                   (if (__send__ v 'is_invalid)
                                       (Invalid (append errors (__send__ v 'errors)))
                                       (Invalid errors))))
      ((eq? __msg__ 'pure)       (lambda (v) (Valid v)))
      ((eq? __msg__ 'to_result)  (lambda () (Err errors)))
      ((eq? __msg__ 'match)      (lambda (on_valid on_invalid) (on_invalid errors)))
      (else (error "Validation: unknown message" __msg__)))))

(define (validate_all validations)
  (let loop ((vs validations) (vals '()) (errs '()))
    (if (null? vs)
        (if (null? errs)
            (Valid (reverse vals))
            (Invalid errs))
        (if (__send__ (car vs) 'is_valid)
            (loop (cdr vs) (cons (__send__ (car vs) 'value) vals) errs)
            (loop (cdr vs) vals (append errs (__send__ (car vs) 'errors)))))))

(define (lift_v2 f v1 v2)
  (let ((curried (lambda (a) (lambda (b) (f a b)))))
    ((__send__ ((__send__ (Valid curried) 'apply) v1) 'apply) v2)))

(define (lift_v3 f v1 v2 v3)
  (let ((curried (lambda (a) (lambda (b) (lambda (c) (f a b c))))))
    ((__send__ ((__send__ ((__send__ (Valid curried) 'apply) v1) 'apply) v2) 'apply) v3)))

;; ===== Writer Monad =====

(define (Writer value log)
  (lambda (__msg__)
    (cond
      ((eq? __msg__ 'value)    value)
      ((eq? __msg__ 'log)      log)
      ((eq? __msg__ '__type__) 'Writer)
      ((eq? __msg__ 'run)      (lambda () (list value log)))
      ((eq? __msg__ 'map)      (lambda (f) (Writer (f value) log)))
      ((eq? __msg__ 'bind)     (lambda (f)
                                 (let ((result (f value)))
                                   (Writer (__send__ result 'value)
                                           (append log (__send__ result 'log))))))
      ((eq? __msg__ 'pure)     (lambda (v) (Writer v '())))
      (else (error "Writer: unknown message" __msg__)))))

(define (tell msg) (Writer '() (list msg)))

;; ===== Reader Monad =====

(define (Reader f)
  (lambda (__msg__)
    (cond
      ((eq? __msg__ '__type__) 'Reader)
      ((eq? __msg__ 'run)      f)
      ((eq? __msg__ 'map)      (lambda (g) (Reader (lambda (env) (g (f env))))))
      ((eq? __msg__ 'bind)     (lambda (g)
                                 (Reader (lambda (env)
                                   (let ((a (f env)))
                                     ((__send__ (g a) 'run) env))))))
      ((eq? __msg__ 'pure)     (lambda (v) (Reader (lambda (env) v))))
      (else (error "Reader: unknown message" __msg__)))))

(define (ask) (Reader (lambda (env) env)))
(define (asks proj) (Reader (lambda (env) (proj env))))
(define (run_reader r env) ((__send__ r 'run) env))

;; ===== State Monad =====

(define (State f)
  (lambda (__msg__)
    (cond
      ((eq? __msg__ '__type__) 'State)
      ((eq? __msg__ 'run)      f)
      ((eq? __msg__ 'map)      (lambda (g)
                                 (State (lambda (s)
                                   (let ((res (f s)))
                                     (list (g (car res)) (cadr res)))))))
      ((eq? __msg__ 'bind)     (lambda (g)
                                 (State (lambda (s)
                                   (let ((res (f s)))
                                     ((__send__ (g (car res)) 'run) (cadr res)))))))
      ((eq? __msg__ 'pure)     (lambda (v) (State (lambda (s) (list v s)))))
      (else (error "State: unknown message" __msg__)))))

(define (get_state) (State (lambda (s) (list s s))))
(define (put_state new_s) (State (lambda (_) (list '() new_s))))
(define (modify_state f) (State (lambda (s) (list '() (f s)))))
(define (run_state st s) ((__send__ st 'run) s))

;; ===== Monoid =====

(define (mappend a b)
  (cond
    ((string? a) (string-append a b))
    ((pair? a)   (append a b))
    ((null? a)   b)
    ((number? a) (+ a b))
    ((vector? a) (list->vector (append (vector->list a) (vector->list b))))
    ((procedure? a) ((__send__ a 'mappend) b))
    (else (error "mappend: not a monoid" a))))

(define (mempty type)
  (cond
    ((eq? type 'string) "")
    ((eq? type 'list)   '())
    ((eq? type 'number) 0)
    (else (error "mempty: unknown type" type))))

(define (mconcat type lst)
  (fold (lambda (x acc) (mappend acc x)) (mempty type) lst))

;; ===== Traversable =====

(define (sequence ms)
  (if (null? ms) (error "sequence: empty list")
      (let ((pure-fn (__send__ (car ms) 'pure)))
        (let loop ((rest ms))
          (if (null? rest) (pure-fn '())
              (monad-bind (car rest)
                (lambda (x)
                  (monad-bind (loop (cdr rest))
                    (lambda (xs) (pure-fn (cons x xs)))))))))))

(define (traverse f xs)
  (sequence (map f xs)))

;; ===== Natural Transformations =====

(define (maybe_to_list m)
  (if (__send__ m 'is_some) (list (__send__ m 'value)) '()))

(define (maybe_to_result err_msg m)
  (if (__send__ m 'is_some) (Ok (__send__ m 'value)) (Err err_msg)))

(define (result_to_maybe r)
  (if (__send__ r 'is_ok) (Some (__send__ r 'value)) None))

(define (list_to_maybe lst)
  (if (null? lst) None (Some (car lst))))

(define (validation_to_result v)
  ((__send__ v 'to_result)))

;; ===== Lenses =====

(define (Lens getter setter)
  (lambda (__msg__)
    (cond
      ((eq? __msg__ '__type__) 'Lens)
      ((eq? __msg__ 'get)      getter)
      ((eq? __msg__ 'set)      setter)
      ((eq? __msg__ 'over)     (lambda (f obj) (setter obj (f (getter obj)))))
      ((eq? __msg__ 'compose)  (lambda (inner)
                                 (Lens
                                   (lambda (obj) ((__send__ inner 'get) (getter obj)))
                                   (lambda (obj val)
                                     (setter obj
                                       ((__send__ inner 'set) (getter obj) val))))))
      (else (error "Lens: unknown message" __msg__)))))

(define (dict_lens key)
  (Lens
    (lambda (d) (hash-table-ref d key))
    (lambda (d val)
      (let ((new-d (__make_eval_dict__ (hash-table->alist d))))
        (hash-table-set! new-d key val)
        new-d))))

(define (index_lens i)
  (Lens
    (lambda (lst) (list-ref lst i))
    (lambda (lst val)
      (let loop ((l lst) (n 0))
        (if (null? l) '()
            (if (= n i) (cons val (cdr l))
                (cons (car l) (loop (cdr l) (+ n 1)))))))))

;; ===== Combinators =====

(define (from_nullable x) (if (null? x) None (Some x)))

(define (from_try thunk)
  (guard (exn (#t (Err exn)))
    (Ok (thunk))))

(define (maybe default f m)
  (if (__send__ m 'is_some) (f (__send__ m 'value)) default))

(define (either on_ok on_err r)
  (if (__send__ r 'is_ok) (on_ok (__send__ r 'value)) (on_err (__send__ r 'error))))

(define (compose f g) (lambda (x) (f (g x))))
(define (flip f) (lambda (a b) (f b a)))
(define (const x) (lambda (_) x))
(define (fish f g) (lambda (x) (monad-bind (f x) g)))
