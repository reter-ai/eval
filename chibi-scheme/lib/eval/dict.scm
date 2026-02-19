;; eval/dict.scm -- Dict runtime: raw hash-table with bracket indexing

;; Constructor: returns raw hash-table (SRFI-69)
(define (__make_eval_dict__ pairs)
  (let ((ht (make-hash-table)))
    (for-each (lambda (p) (hash-table-set! ht (car p) (cdr p))) pairs)
    ht))

;; Predicate: hash-table?
(define dict? hash-table?)

;; Wrap ref for bracket indexing: d["name"] → hash-table-ref/default
(let ((__c-ref__ ref))
  (set! ref
    (lambda (obj idx)
      (if (hash-table? obj)
          (hash-table-ref/default obj
            (if (string? idx) (string->symbol idx) idx) #f)
          (__c-ref__ obj idx)))))
