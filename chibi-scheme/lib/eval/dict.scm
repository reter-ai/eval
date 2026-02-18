;; eval/dict.scm -- Dict runtime: closure wrapping a hash table

(define (__make_eval_dict__ pairs)
  (let ((ht (make-hash-table)))
    (for-each (lambda (p) (hash-table-set! ht (car p) (cdr p))) pairs)
    (lambda (__msg__)
      (cond
        ((eq? __msg__ 'get)
         (lambda (k) (hash-table-ref/default ht
           (if (string? k) (string->symbol k) k) #f)))
        ((eq? __msg__ 'set)
         (lambda (k v) (hash-table-set! ht
           (if (string? k) (string->symbol k) k) v)))
        ((eq? __msg__ 'delete)
         (lambda (k) (hash-table-delete! ht
           (if (string? k) (string->symbol k) k))))
        ((eq? __msg__ 'keys)
         (lambda () (hash-table-keys ht)))
        ((eq? __msg__ 'values)
         (lambda () (hash-table-values ht)))
        ((eq? __msg__ 'has?)
         (lambda (k) (hash-table-exists? ht
           (if (string? k) (string->symbol k) k))))
        ((eq? __msg__ 'size)
         (lambda () (hash-table-size ht)))
        ((eq? __msg__ 'to_list)
         (lambda () (hash-table->alist ht)))
        ((eq? __msg__ '__type__) '__dict__)
        ((hash-table-exists? ht __msg__)
         (hash-table-ref ht __msg__))
        (else #f)))))

(define (dict? v)
  (and (procedure? v)
       (protect (e (else #f))
         (eq? (v '__type__) '__dict__))))
