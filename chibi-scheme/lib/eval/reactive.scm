;; eval/reactive.scm -- Reactive runtime: Signal, Computed, Effect, batch, dispose

;; --- ID generator (no gensym in chibi) ---
(define __reactive-id-counter__ 0)
(define (__reactive-gensym__ prefix)
  (set! __reactive-id-counter__ (+ __reactive-id-counter__ 1))
  (string-append prefix (number->string __reactive-id-counter__)))

;; --- Auto-tracking parameter (thread-local via make-parameter) ---
(define __current-observer__ (make-parameter #f))

;; --- Scope tracking ---
(define __reactive-current-scope__ #f)

;; --- Batch state ---
(define __reactive-batch-depth__ 0)
(define __reactive-batch-queue__ '())

;; --- Notify: mark downstream dirty and schedule effects ---
(define (__reactive-notify__ observers)
  (for-each
    (lambda (node)
      (node '__mark-dirty!__))
    observers))

;; --- Schedule an effect for batched flush (dedup by id) ---
(define (__reactive-schedule-effect__ node)
  (let ((nid (node 'id)))
    (if (not (find (lambda (pair) (equal? (cdr pair) node))
                   __reactive-batch-queue__))
      (let ((level (node 'level)))
        (set! __reactive-batch-queue__
          (cons (cons level node) __reactive-batch-queue__))))))

;; --- Flush: sort by level, run each ---
(define (__reactive-flush__)
  (let loop ()
    (if (null? __reactive-batch-queue__) (if #f #f)
      (let ((q (sort __reactive-batch-queue__
                 (lambda (a b) (< (car a) (car b))))))
        (set! __reactive-batch-queue__ '())
        (for-each
          (lambda (pair)
            (let ((node (cdr pair)))
              (node '__run!__)))
          q)
        (if (not (null? __reactive-batch-queue__))
            (loop))))))

;; --- Signal ---
(define (Signal initial-value . rest)
  (let ((value initial-value)
        (eq-fn (if (null? rest) equal? (car rest)))
        (observers (make-hash-table))
        (id (__reactive-gensym__ "sig")))
    (letrec
      ((notify!
        (lambda ()
          (let ((obs (hash-table-values observers)))
            (if (null? obs) (if #f #f)
              (begin
                (if (= __reactive-batch-depth__ 0)
                  (begin
                    (set! __reactive-batch-depth__ 1)
                    (__reactive-notify__ obs)
                    (set! __reactive-batch-depth__ 0)
                    (__reactive-flush__))
                  (__reactive-notify__ obs)))))))
       (self
        (lambda args
          (if (null? args)
            (begin
              (let ((obs (__current-observer__)))
                (if obs ((obs 'register-source) self)))
              value)
            (let ((msg (car args)))
              (cond
                ((eq? msg 'set)
                 (lambda (new-val)
                   (if (not (eq-fn value new-val))
                     (begin (set! value new-val)
                            (notify!)))))
                ((eq? msg 'update)
                 (lambda (fn)
                   (let ((new-val (fn value)))
                     (if (not (eq-fn value new-val))
                       (begin (set! value new-val)
                              (notify!))))))
                ((eq? msg 'peek) value)
                ((eq? msg 'add-observer)
                 (lambda (obs-id node)
                   (hash-table-set! observers obs-id node)))
                ((eq? msg 'remove-observer)
                 (lambda (obs-id)
                   (hash-table-delete! observers obs-id)))
                ((eq? msg 'observers)
                 (hash-table-values observers))
                ((eq? msg 'level) 0)
                ((eq? msg 'id) id)
                ((eq? msg '__type__) '__signal__)
                ((eq? msg 'close) (lambda ()
                  (for-each (lambda (k) (hash-table-delete! observers k))
                    (hash-table-keys observers))))
                ((eq? msg 'dispose) (lambda ()
                  (for-each (lambda (k) (hash-table-delete! observers k))
                    (hash-table-keys observers))))
                (else (error "Signal: unknown message" msg))))))))
      (if __reactive-current-scope__
        ((__reactive-current-scope__ 'register-child) self))
      self)))

;; --- Computed ---
(define (Computed fn . rest)
  (let ((value #f)
        (eq-fn (if (null? rest) equal? (car rest)))
        (dirty #t)
        (computing #f)
        (disposed #f)
        (sources (make-hash-table))
        (observers (make-hash-table))
        (id (__reactive-gensym__ "comp"))
        (level 1))
    (letrec
      ((unsubscribe-all!
        (lambda ()
          (for-each
            (lambda (src)
              ((src 'remove-observer) id))
            (hash-table-values sources))
          (let ((ht (make-hash-table)))
            (set! sources ht))))
       (recompute!
        (lambda ()
          (if computing (error "Computed: circular dependency" id))
          (set! computing #t)
          (unsubscribe-all!)
          (set! level 1)
          (let ((new-val
                 (protect (e (else
                   (set! computing #f)
                   (set! dirty #t)
                   (raise e)))
                   (parameterize ((__current-observer__ self))
                     (fn)))))
            (set! computing #f)
            (set! dirty #f)
            (let ((changed (not (eq-fn value new-val))))
              (set! value new-val)
              changed))))
       (self
        (lambda args
          (if (null? args)
            (begin
              (if (and dirty (not disposed))
                (recompute!))
              (let ((obs (__current-observer__)))
                (if obs ((obs 'register-source) self)))
              value)
            (let ((msg (car args)))
              (cond
                ((eq? msg 'register-source)
                 (lambda (src)
                   (let ((src-id (src 'id)))
                     (if (not (hash-table-exists? sources src-id))
                       (begin
                         (hash-table-set! sources src-id src)
                         ((src 'add-observer) id self)
                         (let ((sl (src 'level)))
                           (if (>= sl level)
                             (set! level (+ sl 1)))))))))
                ((eq? msg '__mark-dirty!__)
                 (if (not dirty)
                   (begin
                     (set! dirty #t)
                     (__reactive-schedule-effect__ self))))
                ((eq? msg '__run!__)
                 (if (and dirty (not disposed))
                   (let ((changed (recompute!)))
                     (if changed
                       (__reactive-notify__
                         (hash-table-values observers))))))
                ((eq? msg 'add-observer)
                 (lambda (obs-id node)
                   (hash-table-set! observers obs-id node)))
                ((eq? msg 'remove-observer)
                 (lambda (obs-id)
                   (hash-table-delete! observers obs-id)))
                ((eq? msg 'level) level)
                ((eq? msg 'id) id)
                ((eq? msg '__type__) '__computed__)
                ((eq? msg 'peek)
                 (if (and dirty (not disposed))
                   (recompute!))
                 value)
                ((eq? msg 'dirty?) dirty)
                ((eq? msg 'close) (lambda ()
                  (set! disposed #t)
                  (unsubscribe-all!)))
                ((eq? msg 'dispose) (lambda ()
                  (set! disposed #t)
                  (unsubscribe-all!)))
                (else
                  (error "Computed: unknown message" msg))))))))
      (protect (e (else (set! computing #f) (set! dirty #t)))
        (recompute!))
      (if __reactive-current-scope__
        ((__reactive-current-scope__ 'register-child) self))
      self)))

;; --- Effect ---
(define (Effect fn)
  (let ((cleanup #f)
        (disposed #f)
        (sources (make-hash-table))
        (id (__reactive-gensym__ "eff"))
        (level 1))
    (letrec
      ((unsubscribe-all!
        (lambda ()
          (for-each
            (lambda (src)
              ((src 'remove-observer) id))
            (hash-table-values sources))
          (let ((ht (make-hash-table)))
            (set! sources ht))))
       (run!
        (lambda ()
          (if disposed (if #f #f)
            (begin
              (if (procedure? cleanup)
                (protect (e (else (if #f #f))) (cleanup)))
              (set! cleanup #f)
              (unsubscribe-all!)
              (set! level 1)
              (let ((result
                     (parameterize ((__current-observer__ self))
                       (fn))))
                (if (procedure? result)
                  (set! cleanup result)))))))
       (self
        (lambda args
          (if (null? args)
            (if #f #f)
            (let ((msg (car args)))
              (cond
                ((eq? msg 'register-source)
                 (lambda (src)
                   (let ((src-id (src 'id)))
                     (if (not (hash-table-exists? sources src-id))
                       (begin
                         (hash-table-set! sources src-id src)
                         ((src 'add-observer) id self)
                         (let ((sl (src 'level)))
                           (if (>= sl level)
                             (set! level (+ sl 1)))))))))
                ((eq? msg '__mark-dirty!__)
                 (__reactive-schedule-effect__ self))
                ((eq? msg '__run!__)
                 (if (not disposed) (run!)))
                ((eq? msg 'level) level)
                ((eq? msg 'id) id)
                ((eq? msg '__type__) '__effect__)
                ((eq? msg 'close) (lambda ()
                  (set! disposed #t)
                  (if (procedure? cleanup)
                    (protect (e (else (if #f #f))) (cleanup)))
                  (set! cleanup #f)
                  (unsubscribe-all!)))
                ((eq? msg 'dispose) (lambda ()
                  (set! disposed #t)
                  (if (procedure? cleanup)
                    (protect (e (else (if #f #f))) (cleanup)))
                  (set! cleanup #f)
                  (unsubscribe-all!)))
                (else
                  (error "Effect: unknown message" msg))))))))
      (run!)
      (if __reactive-current-scope__
        ((__reactive-current-scope__ 'register-child) self))
      self)))

;; --- batch ---
(define (batch fn)
  (set! __reactive-batch-depth__
    (+ __reactive-batch-depth__ 1))
  (let ((result
         (protect (e (else
           (set! __reactive-batch-depth__
             (- __reactive-batch-depth__ 1))
           (if (= __reactive-batch-depth__ 0)
             (__reactive-flush__))
           (raise e)))
           (fn))))
    (set! __reactive-batch-depth__
      (- __reactive-batch-depth__ 1))
    (if (= __reactive-batch-depth__ 0)
      (__reactive-flush__))
    result))

;; --- dispose ---
(define (dispose node)
  ((node 'dispose)))

;; --- Type predicates ---
(define (signal? v)
  (and (procedure? v)
       (protect (e (else #f))
         (eq? (v '__type__) '__signal__))))
(define (computed? v)
  (and (procedure? v)
       (protect (e (else #f))
         (eq? (v '__type__) '__computed__))))
(define (effect? v)
  (and (procedure? v)
       (protect (e (else #f))
         (eq? (v '__type__) '__effect__))))

;; --- untracked: suppress dependency tracking ---
(define (untracked fn)
  (parameterize ((__current-observer__ #f))
    (fn)))

;; --- derived: shorthand computed from one signal ---
(define (derived src fn)
  (Computed (lambda () (fn (src)))))

;; --- readonly: read-only view of a signal/computed ---
(define (readonly src)
  (let ((id (__reactive-gensym__ "ro")))
    (lambda args
      (if (null? args)
        (src)
        (let ((msg (car args)))
          (cond
            ((eq? msg 'peek) (src 'peek))
            ((eq? msg 'level) (src 'level))
            ((eq? msg 'id) (src 'id))
            ((eq? msg 'add-observer)
             (lambda (obs-id node)
               ((src 'add-observer) obs-id node)))
            ((eq? msg 'remove-observer)
             (lambda (obs-id)
               ((src 'remove-observer) obs-id)))
            ((eq? msg '__type__) '__readonly__)
            ((eq? msg 'set) (error "readonly: cannot set"))
            ((eq? msg 'update) (error "readonly: cannot update"))
            ((eq? msg 'dispose) (error "readonly: cannot dispose"))
            ((eq? msg 'close) (error "readonly: cannot close"))
            (else (error "readonly: unknown message" msg))))))))

;; --- readonly? predicate ---
(define (readonly? v)
  (and (procedure? v)
       (protect (e (else #f))
         (eq? (v '__type__) '__readonly__))))

;; --- watch: run fn(new, old) on changes ---
(define (watch src fn)
  (let ((prev (src 'peek))
        (first #t))
    (Effect (lambda ()
      (let ((curr (src)))
        (if first
          (set! first #f)
          (let ((old prev))
            (set! prev curr)
            (fn curr old))))))))

;; --- on: effect with explicit dependency list ---
(define (on deps fn)
  (Effect (lambda ()
    (let ((vals (map (lambda (d) (d)) deps)))
      (parameterize ((__current-observer__ #f))
        (apply fn vals))))))

;; --- reduce: fold over signal changes ---
(define (reduce src fn initial)
  (let ((acc (Signal initial)))
    (watch src (lambda (new-val old-val)
      ((acc 'set) (fn (acc 'peek) new-val))))
    acc))

;; --- scope: reactive ownership scope ---
(define (scope fn)
  (let ((children '())
        (id (__reactive-gensym__ "scope"))
        (disposed #f)
        (prev-scope __reactive-current-scope__))
    (letrec
      ((self
        (lambda args
          (if (null? args)
            children
            (let ((msg (car args)))
              (cond
                ((eq? msg 'register-child)
                 (lambda (child)
                   (if (not disposed)
                     (set! children (cons child children)))))
                ((eq? msg 'dispose) (lambda ()
                  (if (not disposed)
                    (begin
                      (set! disposed #t)
                      (for-each (lambda (child)
                        (protect (e (else (if #f #f)))
                          ((child 'dispose))))
                        children)
                      (set! children '())))))
                ((eq? msg 'close) (lambda ()
                  ((self 'dispose))))
                ((eq? msg '__type__) '__scope__)
                ((eq? msg 'id) id)
                (else (error "scope: unknown message" msg))))))))
      (set! __reactive-current-scope__ self)
      (protect (e (else
        (set! __reactive-current-scope__ prev-scope)
        (raise e)))
        (fn))
      (set! __reactive-current-scope__ prev-scope)
      self)))

;; --- scope? predicate ---
(define (scope? v)
  (and (procedure? v)
       (protect (e (else #f))
         (eq? (v '__type__) '__scope__))))

;; --- WritableComputed: two-way computed ---
(define (WritableComputed getter setter)
  (let ((inner (Computed getter)))
    (lambda args
      (if (null? args)
        (inner)
        (let ((msg (car args)))
          (cond
            ((eq? msg 'set)
             (lambda (new-val) (setter new-val)))
            ((eq? msg 'update)
             (lambda (fn) (setter (fn (inner 'peek)))))
            ((eq? msg '__type__) '__writable-computed__)
            (else (inner msg))))))))

;; --- writable_computed? predicate ---
(define (writable_computed? v)
  (and (procedure? v)
       (protect (e (else #f))
         (eq? (v '__type__) '__writable-computed__))))

;; --- combine: merge N signals into one computed ---
(define (combine sources fn)
  (Computed (lambda ()
    (apply fn (map (lambda (s) (s)) sources)))))

;; --- select: fine-grained slice with optional custom equality ---
(define (select src selector . rest)
  (let ((eq-fn (if (null? rest) equal? (car rest))))
    (Computed (lambda () (selector (src))) eq-fn)))

;; --- prev: previous value signal ---
(define (prev src . rest)
  (let ((initial (if (null? rest) #f (car rest))))
    (let ((p (Signal initial)))
      (watch src (lambda (new-val old-val)
        ((p 'set) old-val)))
      (readonly p))))

;; --- trace: debug logging ---
(define (trace src label)
  (watch src (lambda (new-val old-val)
    (display (string-append "[" label "] "))
    (display old-val)
    (display " -> ")
    (display new-val)
    (newline)))
  src)

;; --- resource: async data loading primitive ---
(define (resource source-or-fetcher . rest)
  (let* ((has-source (not (null? rest)))
         (src (if has-source source-or-fetcher #f))
         (fetcher (if has-source (car rest) source-or-fetcher))
         (initial (if (and has-source (pair? (cdr rest)))
                    (cadr rest) #f))
         (value-sig (Signal initial))
         (loading-sig (Signal #t))
         (error-sig (Signal #f))
         (version 0)
         (current-thread #f)
         (watcher #f)
         (id (__reactive-gensym__ "res")))
    (letrec
      ((do-fetch
        (lambda (arg)
          (set! version (+ version 1))
          (let ((my-version version))
            ((loading-sig 'set) #t)
            ((error-sig 'set) #f)
            (let ((t (make-thread (lambda ()
                       (protect (e (else
                         (if (= my-version version)
                           (begin
                             ((error-sig 'set) e)
                             ((loading-sig 'set) #f)))))
                         (let ((result (if has-source
                                         (fetcher arg)
                                         (fetcher))))
                           (if (= my-version version)
                             (begin
                               ((value-sig 'set) result)
                               ((loading-sig 'set) #f)))))))))
              (set! current-thread t)
              (thread-start! t)))))
       (self
        (lambda args
          (if (null? args)
            (value-sig)
            (let ((msg (car args)))
              (cond
                ((eq? msg 'loading) (loading-sig))
                ((eq? msg 'error) (error-sig))
                ((eq? msg 'settle) (lambda ()
                  (if current-thread
                    (protect (e (else (if #f #f)))
                      (thread-join! current-thread)))
                  (thread-yield!)))
                ((eq? msg 'refetch) (lambda ()
                  (if has-source
                    (do-fetch (src 'peek))
                    (do-fetch #f))))
                ((eq? msg 'mutate) (lambda (v)
                  ((value-sig 'set) v)))
                ((eq? msg 'peek) (value-sig 'peek))
                ((eq? msg 'level) (value-sig 'level))
                ((eq? msg 'id) id)
                ((eq? msg 'add-observer)
                 (lambda (obs-id node)
                   ((value-sig 'add-observer) obs-id node)))
                ((eq? msg 'remove-observer)
                 (lambda (obs-id)
                   ((value-sig 'remove-observer) obs-id)))
                ((eq? msg '__type__) '__resource__)
                ((eq? msg 'dispose) (lambda ()
                  (set! version (+ version 1))
                  (if watcher (dispose watcher))))
                ((eq? msg 'close) (lambda ()
                  ((self 'dispose))))
                (else (error "resource: unknown message" msg))))))))
      (if has-source
        (begin
          (do-fetch (src 'peek))
          (set! watcher
            (watch src (lambda (new-val old-val)
              (do-fetch new-val)))))
        (do-fetch #f))
      (if __reactive-current-scope__
        ((__reactive-current-scope__ 'register-child) self))
      self)))

;; --- resource? predicate ---
(define (resource? v)
  (and (procedure? v)
       (protect (e (else #f))
         (eq? (v '__type__) '__resource__))))
