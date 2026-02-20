;; eval/logic.scm -- microKanren engine + Prolog database runtime

;; === Logic variable representation ===
;; A logic variable is #(var <id>)
(define (var id) (vector 'var id))
(define (var? x) (and (vector? x) (= (vector-length x) 2) (eq? (vector-ref x 0) 'var)))
(define (var=? x y) (= (vector-ref x 1) (vector-ref y 1)))

;; === Substitution & State ===
;; Substitution: alist ((var . val) ...)
;; State: (substitution . counter)
(define empty-state '(() . 0))

;; walk: resolve variable through substitution chain
(define (walk u s)
  (if (var? u)
    (let ((b (assoc u s (lambda (a b) (var=? a b)))))
      (if b (walk (cdr b) s) u))
    u))

;; unify: extend substitution or return #f
(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (cons (cons u v) s))
      ((var? v) (cons (cons v u) s))
      ((and (pair? u) (pair? v))
       (let ((s2 (unify (car u) (car v) s)))
         (and s2 (unify (cdr u) (cdr v) s2))))
      ((and (vector? u) (vector? v) (= (vector-length u) (vector-length v)))
       (let loop ((i 0) (s s))
         (if (= i (vector-length u)) s
           (let ((s2 (unify (vector-ref u i) (vector-ref v i) s)))
             (and s2 (loop (+ i 1) s2))))))
      ((equal? u v) s)
      (else #f))))

;; === Goal type ===
;; Goal = state -> stream-of-states
;; Stream: () | (state . stream) | (lambda () stream)  [thunk for fairness]

;; === (unification goal constructor) ===
(define (logic_eq u v)
  (lambda (state)
    (let ((s (unify u v (car state))))
      (if s (list (cons s (cdr state))) '()))))

;; call-fresh: introduce new logic variable
(define (call-fresh f)
  (lambda (state)
    (let ((c (cdr state)))
      ((f (var c)) (cons (car state) (+ c 1))))))

;; disj/conj: goal combinators
(define (disj g1 g2) (lambda (st) (mplus (g1 st) (g2 st))))
(define (conj g1 g2) (lambda (st) (logic-bind (g1 st) g2)))

;; Stream ops with interleaving for fairness
(define (mplus s1 s2)
  (cond ((null? s1) s2)
        ((procedure? s1) (lambda () (mplus s2 (s1))))
        (else (cons (car s1) (mplus (cdr s1) s2)))))

(define (logic-bind s g)
  (cond ((null? s) '())
        ((procedure? s) (lambda () (logic-bind (s) g)))
        (else (mplus (g (car s)) (logic-bind (cdr s) g)))))

;; === Higher-level interface ===

;; Reification: extract human-readable results
(define (walk* v s)
  (let ((v (walk v s)))
    (cond ((var? v) v)
          ((pair? v) (cons (walk* (car v) s) (walk* (cdr v) s)))
          (else v))))

(define (reify-s v s)
  (let ((v (walk v s)))
    (cond ((var? v) (cons (cons v (reify-name (length s))) s))
          ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
          (else s))))

(define (reify-name n)
  (string->symbol (string-append "_." (number->string n))))

(define (reify-1st state)
  (let ((v (walk* (var 0) (car state))))
    (walk* v (reify-s v '()))))

;; Stream helpers
(define (pull s) (if (procedure? s) (pull (s)) s))

(define (take-n n s)
  (let ((s (pull s)))
    (cond ((and n (<= n 0)) '())
          ((null? s) '())
          (else (cons (car s) (take-n (and n (- n 1)) (cdr s)))))))

;; Variadic conj/disj
(define (logic_conj_list goals)
  (if (null? goals) (lambda (st) (list st))
    (let loop ((gs goals))
      (if (null? (cdr gs)) (car gs)
        (conj (car gs) (loop (cdr gs)))))))

(define (logic_disj_list goals)
  (if (null? goals) (lambda (st) '())
    (let loop ((gs goals))
      (if (null? (cdr gs)) (car gs)
        (disj (car gs) (loop (cdr gs)))))))

;; conde: each clause is a list of goals
(define (logic_conde . clauses)
  (apply logic_disj_list
    (list (map (lambda (c) (logic_conj_list c)) clauses))))

;; run: main entry point
;; n = #f means all, n = number means first n
;; f is (lambda (q) goal) where q will be bound to (var 0)
(define (logic_run n f)
  (let* ((goal (call-fresh f))
         (results (pull (goal empty-state))))
    (map reify-1st (take-n n results))))

;; guard: use Eval predicate as goal
(define (logic_guard pred)
  (lambda (state)
    (if (pred) (list state) '())))

;; member relation
(define (logic_member x lst)
  (lambda (state)
    (let ((lst-val (walk lst (car state))))
      (if (pair? lst-val)
        ((disj
           (logic_eq x (car lst-val))
           (logic_member x (cdr lst-val)))
         state)
        '()))))

;; === Prolog-style database ===
(define __logic_db__ '())

(define (__logic_db_ref__ name)
  (let ((entry (assq name __logic_db__)))
    (if entry (cdr entry) '())))

(define (__logic_db_set!__ name entries)
  (let ((entry (assq name __logic_db__)))
    (if entry
      (set-cdr! entry entries)
      (set! __logic_db__ (cons (cons name entries) __logic_db__)))))

(define (logic_assert_fact name . args)
  (let ((existing (__logic_db_ref__ name)))
    (__logic_db_set!__ name
      (append existing (list (cons 'fact args))))))

(define (logic_assert_rule name arity body-fn)
  (let ((existing (__logic_db_ref__ name)))
    (__logic_db_set!__ name
      (append existing (list (list 'rule arity body-fn))))))

(define (logic_retract name)
  (set! __logic_db__ (filter (lambda (e) (not (eq? (car e) name))) __logic_db__)))

(define (logic_clear_db)
  (set! __logic_db__ '()))

;; Query a relation by name -- produces a goal
(define (logic_query_rel name args)
  (let ((entries (__logic_db_ref__ name)))
    (if (null? entries)
      (lambda (st) '())
      (logic_disj_list
        (map (lambda (entry)
               (if (eq? (car entry) 'fact)
                 ;; Fact: unify each arg positionally
                 (let ((fact-args (cdr entry)))
                   (logic_conj_list
                     (map (lambda (pair) (logic_eq (car pair) (cdr pair)))
                          (map cons args fact-args))))
                 ;; Rule: create fresh vars, call body
                 (let ((arity (cadr entry))
                       (body-fn (caddr entry)))
                   (lambda (state)
                     ;; Allocate fresh vars for rule params
                     (let loop ((i 0) (vars '()) (c (cdr state)))
                       (if (= i arity)
                         (let* ((vars-rev (reverse vars))
                                (new-state (cons (car state) c))
                                ;; Unify caller args with fresh rule vars
                                (unify-goal (logic_conj_list
                                              (map (lambda (pair) (logic_eq (car pair) (cdr pair)))
                                                   (map cons args vars-rev))))
                                ;; Then run rule body with those vars
                                (body-goal (apply body-fn vars-rev))
                                (combined (conj unify-goal body-goal)))
                           (combined new-state))
                         (loop (+ i 1) (cons (var c) vars) (+ c 1))))))))
             entries)))))

;; High-level query: collect all solutions as list of alists
(define (logic_run_query name args)
  ;; Find which args are logic vars (symbols starting with ?)
  ;; and which are concrete values
  (let* ((indexed (let loop ((a args) (i 0) (acc '()))
                    (if (null? a) (reverse acc)
                      (loop (cdr a) (+ i 1)
                            (cons (cons i (car a)) acc)))))
         ;; All args become fresh vars in the query
         (goal (lambda (q)
                 (lambda (state)
                   (let* ((c (cdr state))
                          ;; Create fresh vars for all positions
                          (result (let loop ((idx indexed) (vs '()) (c c))
                                   (if (null? idx)
                                     (cons (reverse vs) c)
                                     (loop (cdr idx)
                                           (cons (var c) vs)
                                           (+ c 1)))))
                          (fresh-vars (car result))
                          (new-c (cdr result))
                          (new-state (cons (car state) new-c))
                          ;; Bind concrete args to fresh vars
                          (bind-goals
                            (filter (lambda (g) g)
                              (map (lambda (pair fv)
                                     (let ((val (cdr pair)))
                                       ;; If arg is a var? already (from ?x syntax),
                                       ;; skip binding -- it stays free
                                       (if (var? val) #f
                                         (logic_eq fv val))))
                                   indexed fresh-vars)))
                          ;; Query the relation
                          (rel-goal (logic_query_rel name fresh-vars))
                          ;; Collect results: pack all free vars into q
                          (free-pairs
                            (filter (lambda (p) p)
                              (map (lambda (pair fv)
                                     (if (var? (cdr pair))
                                       (cons (cdr pair) fv) ;; (original-var . fresh-var)
                                       #f))
                                   indexed fresh-vars)))
                          ;; Build result as list of pairs
                          (result-goal
                            (if (null? free-pairs)
                              (logic_eq q #t)
                              (logic_eq q
                                (map (lambda (fp)
                                       (cons (car fp) (cdr fp)))
                                     free-pairs)))))
                     ((logic_conj_list
                        (append bind-goals (list rel-goal result-goal)))
                      new-state))))))
    (logic_run #f goal)))

;; findall: extract specific variable values
(define (logic_findall_var vname rname args)
  ;; Find position of vname in args (it's the arg that matches)
  (let* ((pos (let loop ((a args) (i 0))
               (if (null? a) #f
                 (if (and (var? (car a)))
                   (loop (cdr a) (+ i 1))
                   (loop (cdr a) (+ i 1))))))
         ;; Simple approach: run query and extract the relevant variable
         (goal (lambda (q)
                 (lambda (state)
                   (let* ((c (cdr state))
                          (fresh-vars (let loop ((a args) (vs '()) (c c))
                                       (if (null? a) (cons (reverse vs) c)
                                         (loop (cdr a) (cons (var c) vs) (+ c 1)))))
                          (fvs (car fresh-vars))
                          (new-c (cdr fresh-vars))
                          (new-state (cons (car state) new-c))
                          ;; Bind non-var args
                          (bind-goals
                            (let loop ((a args) (f fvs) (acc '()))
                              (if (null? a) (reverse acc)
                                (if (var? (car a))
                                  (loop (cdr a) (cdr f) acc)
                                  (loop (cdr a) (cdr f)
                                        (cons (logic_eq (car f) (car a)) acc))))))
                          ;; Find the fresh var for the target position
                          (target-fv (let loop ((a args) (f fvs))
                                       (if (null? a) #f
                                         (if (var? (car a))
                                           (car f)
                                           (loop (cdr a) (cdr f))))))
                          (rel-goal (logic_query_rel rname fvs))
                          (result-goal (if target-fv
                                        (logic_eq q target-fv)
                                        (logic_eq q #t))))
                     ((logic_conj_list
                        (append bind-goals (list rel-goal result-goal)))
                      new-state))))))
    (logic_run #f goal)))
