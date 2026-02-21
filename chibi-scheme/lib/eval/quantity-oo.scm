;; eval/quantity-oo.scm -- Quantity type: dimensional analysis + unit conversion
;; Loaded after ad.scm. Chains onto __ad_*__ dispatch and __send__.
;; Replaces money-oo.scm; Money becomes an alias for Qty with currency dimension.

;; ================================================================
;; Unit registry
;; ================================================================

(define __unit_registry__ (make-hash-table))

;; Registry entry: (dim-map scale base-umap)
;;   dim-map:   sorted alist of (dimension-symbol . exponent)
;;   scale:     number — conversion factor to base units
;;   base-umap: sorted alist of (base-unit-symbol . exponent)

(define (make-unit-entry dim-map scale base-umap)
  (list dim-map scale base-umap))
(define (unit-entry-dim entry)  (car entry))
(define (unit-entry-scale entry) (cadr entry))
(define (unit-entry-umap entry)  (caddr entry))

;; Register a name (and aliases) in the registry
(define (__register-unit__ name entry . aliases)
  (hash-table-set! __unit_registry__ name entry)
  (let loop ((a aliases))
    (if (pair? a)
        (begin
          (hash-table-set! __unit_registry__ (car a) entry)
          (loop (cdr a))))))

;; ================================================================
;; define-dimension: register a base dimension + its base unit
;; ================================================================

(define (define-dimension dim-name base-unit-name . aliases)
  (let* ((dim-sym (string->symbol dim-name))
         (dim-map (list (cons dim-sym 1)))
         (unit-sym (string->symbol base-unit-name))
         (base-umap (list (cons unit-sym 1)))
         (entry (make-unit-entry dim-map 1 base-umap)))
    (apply __register-unit__ base-unit-name entry aliases)))

;; ================================================================
;; define-unit: register a derived unit with scale relative to a ref unit
;; ================================================================

(define (define-unit name scale ref-unit . aliases)
  (let* ((ref-entry (hash-table-ref __unit_registry__ ref-unit))
         (ref-scale (unit-entry-scale ref-entry))
         (total-scale (* scale ref-scale))
         (dim-map (unit-entry-dim ref-entry))
         (base-umap (unit-entry-umap ref-entry))
         (entry (make-unit-entry dim-map total-scale base-umap)))
    (apply __register-unit__ name entry aliases)))

;; ================================================================
;; define-compound-unit: register a unit from component base units
;; ================================================================

(define (define-compound-unit name scale components . aliases)
  ;; components: alist of ((unit-name-string . exponent) ...)
  (let loop ((cs components) (dim-acc '()) (umap-acc '()) (scale-acc scale))
    (if (null? cs)
        (let ((entry (make-unit-entry (umap-normalize dim-acc)
                                       scale-acc
                                       (umap-normalize umap-acc))))
          (apply __register-unit__ name entry aliases))
        (let* ((c (car cs))
               (uname (if (symbol? (car c)) (symbol->string (car c)) (car c)))
               (exp (cdr c))
               (ref-entry (hash-table-ref __unit_registry__ uname))
               (ref-dim (unit-entry-dim ref-entry))
               (ref-scale (unit-entry-scale ref-entry))
               (ref-umap (unit-entry-umap ref-entry)))
          (loop (cdr cs)
                (umap-merge-add dim-acc (umap-scale ref-dim exp))
                (umap-merge-add umap-acc (umap-scale ref-umap exp))
                (* scale-acc (expt ref-scale exp)))))))

;; ================================================================
;; Unit-map arithmetic (sorted alists of (symbol . integer))
;; ================================================================

(define (umap-normalize u)
  ;; Remove zero-exponent entries
  (filter (lambda (p) (not (= (cdr p) 0))) u))

(define (umap-merge-add u1 u2)
  ;; Merge two umaps, adding exponents for matching symbols
  (cond
    ((null? u1) u2)
    ((null? u2) u1)
    (else
     (let loop ((a u1) (b u2) (acc '()))
       (cond
         ((and (null? a) (null? b)) (reverse acc))
         ((null? a) (append (reverse acc) b))
         ((null? b) (append (reverse acc) a))
         (else
          (let ((sa (car (car a))) (sb (car (car b))))
            (cond
              ((symbol<? sa sb)
               (loop (cdr a) b (cons (car a) acc)))
              ((symbol<? sb sa)
               (loop a (cdr b) (cons (car b) acc)))
              (else
               (let ((sum (+ (cdr (car a)) (cdr (car b)))))
                 (if (= sum 0)
                     (loop (cdr a) (cdr b) acc)
                     (loop (cdr a) (cdr b) (cons (cons sa sum) acc)))))))))))))

(define (umap-scale u n)
  ;; Multiply all exponents by n
  (map (lambda (p) (cons (car p) (* (cdr p) n))) u))

(define (umap-multiply u1 u2) (umap-normalize (umap-merge-add u1 u2)))
(define (umap-divide u1 u2)   (umap-normalize (umap-merge-add u1 (umap-scale u2 -1))))
(define (umap-power u n)      (umap-normalize (umap-scale u n)))

(define (umap-equal? u1 u2)
  (equal? (umap-normalize u1) (umap-normalize u2)))

(define (umap-dimensionless? u)
  (null? (umap-normalize u)))

;; symbol<? for sorting
(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

;; ================================================================
;; Unit-map display: "kg*m/s^2"
;; ================================================================

(define (umap->string u)
  (let* ((norm (umap-normalize u))
         (pos (filter (lambda (p) (> (cdr p) 0)) norm))
         (neg (filter (lambda (p) (< (cdr p) 0)) norm)))
    (if (null? norm) ""
        (let ((num-str (umap-part->string pos))
              (den-str (umap-part->string
                        (map (lambda (p) (cons (car p) (- (cdr p)))) neg))))
          (cond
            ((and (string=? num-str "") (string=? den-str "")) "")
            ((string=? den-str "") num-str)
            ((string=? num-str "") (string-append "1/" den-str))
            (else (string-append num-str "/" den-str)))))))

(define (umap-part->string units)
  (let loop ((us units) (acc ""))
    (if (null? us) acc
        (let* ((p (car us))
               (s (symbol->string (car p)))
               (e (cdr p))
               (part (if (= e 1) s (string-append s "^" (number->string e)))))
          (loop (cdr us)
                (if (string=? acc "") part (string-append acc "*" part)))))))

;; ================================================================
;; Unit string parser: "m/s^2" -> umap, also returns scale
;; Parse: unit_expr = term (('*'|'/') term)*
;;        term = NAME ('^' INT)?
;; ================================================================

(define __unit_parse_cache__ (make-hash-table))

(define (parse-unit-string str)
  ;; Returns (scale . umap) relative to base units
  (let ((cached (hash-table-ref/default __unit_parse_cache__ str #f)))
    (if cached cached
        (let ((result (__parse-unit-string__ str)))
          (hash-table-set! __unit_parse_cache__ str result)
          result))))

(define (__parse-unit-string__ str)
  ;; Tokenize by * and /
  (let* ((tokens (__tokenize-unit__ str))
         (result (__parse-tokens__ tokens)))
    result))

(define (__tokenize-unit__ str)
  ;; Returns list of (sign . name^exp) tokens
  ;; sign: 1 for multiply, -1 for divide
  (let ((len (string-length str)))
    (let loop ((i 0) (sign 1) (tokens '()))
      (if (>= i len)
          (reverse tokens)
          (let ((ch (string-ref str i)))
            (cond
              ((or (char=? ch #\space) (char=? ch #\tab))
               (loop (+ i 1) sign tokens))
              ((char=? ch #\*)
               (loop (+ i 1) 1 tokens))
              ((char=? ch #\/)
               (loop (+ i 1) -1 tokens))
              ;; Parse name with optional ^exp
              (else
               (let name-loop ((j i) (name-chars '()))
                 (if (or (>= j len)
                         (char=? (string-ref str j) #\*)
                         (char=? (string-ref str j) #\/)
                         (char=? (string-ref str j) #\space))
                     ;; End of token
                     (let* ((tok (list->string (reverse name-chars)))
                            (parsed (__parse-name-exp__ tok)))
                       (loop j 1
                             (cons (cons sign parsed) tokens)))
                     (name-loop (+ j 1)
                                (cons (string-ref str j) name-chars)))))))))))

(define (__parse-name-exp__ tok)
  ;; Parse "name^exp" or "name" -> (name . exp)
  (let ((hat-pos (__string-index__ tok #\^)))
    (if hat-pos
        (cons (substring tok 0 hat-pos)
              (string->number (substring tok (+ hat-pos 1) (string-length tok))))
        (cons tok 1))))

(define (__string-index__ str ch)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (cond ((>= i len) #f)
            ((char=? (string-ref str i) ch) i)
            (else (loop (+ i 1)))))))

(define (__parse-tokens__ tokens)
  ;; tokens: list of (sign . (name . exp))
  ;; Returns (total-scale . combined-umap)
  ;; umap uses the unit's own name symbol (not base expansion)
  (let loop ((ts tokens) (scale 1) (umap '()))
    (if (null? ts)
        (cons scale (umap-normalize umap))
        (let* ((t (car ts))
               (sign (car t))
               (name (cadr t))
               (raw-exp (cddr t))
               (exp (* sign raw-exp))
               (entry (hash-table-ref/default __unit_registry__ name #f)))
          (if (not entry)
              (error "Qty: unknown unit" name)
              (let* ((e-scale (unit-entry-scale entry))
                     (self-umap (list (cons (string->symbol name) 1))))
                (loop (cdr ts)
                      (* scale (expt e-scale exp))
                      (umap-merge-add umap (umap-scale self-umap exp)))))))))

;; ================================================================
;; Resolve a unit string/symbol to (scale . umap)
;; ================================================================

(define (resolve-unit u)
  (let ((name (if (symbol? u) (symbol->string u) u)))
    ;; Try direct registry lookup first (single unit)
    (let ((entry (hash-table-ref/default __unit_registry__ name #f)))
      (if entry
          ;; Use the unit's own name as the umap symbol (not base expansion)
          (cons (unit-entry-scale entry) (list (cons (string->symbol name) 1)))
          ;; Otherwise parse as compound expression
          (parse-unit-string name)))))

;; ================================================================
;; Currency detection: dimension starts with "currency_"
;; ================================================================

(define (dimension-is-currency? scale-umap)
  ;; scale-umap is (scale . umap) from resolve-unit
  ;; Check if the unit's dimension starts with "currency_"
  (let ((umap (cdr scale-umap)))
    (and (pair? umap)
         (null? (cdr umap))
         (= (cdar umap) 1)
         (let* ((unit-name (symbol->string (caar umap)))
                (entry (hash-table-ref/default __unit_registry__ unit-name #f)))
           (and entry
                (let ((dim (unit-entry-dim entry)))
                  (and (pair? dim)
                       (null? (cdr dim))
                       (let ((dim-str (symbol->string (caar dim))))
                         (and (>= (string-length dim-str) 9)
                              (string=? (substring dim-str 0 9) "currency_"))))))))))

;; ================================================================
;; Qty constructor
;; ================================================================

(define (Qty magnitude unit)
  (let* ((uname (cond ((symbol? unit) (symbol->string unit))
                       ((string? unit) unit)
                       (else (error "Qty: unit must be string or symbol" unit))))
         (resolved (resolve-unit uname))   ;; (scale . umap)
         (scale (car resolved))
         (umap  (cdr resolved))
         (mag   (if (dimension-is-currency? resolved)
                    (if (%decimal? magnitude) magnitude (Decimal magnitude))
                    magnitude)))
    ;; Store magnitude in the unit given, not scaled to base
    (%qty-make mag umap)))

;; ================================================================
;; User-facing unit definition functions (Eval syntax)
;; ================================================================

(define (define_base_unit name . aliases)
  (apply define-dimension name name aliases))

(define (define_unit name scale ref-unit . aliases)
  (apply define-unit name scale ref-unit aliases))

;; ================================================================
;; Dimension map lookup from umap
;; ================================================================

(define (umap->dim-map umap)
  ;; Look up each base unit's dimension and combine
  (let loop ((u umap) (dim-acc '()))
    (if (null? u) (umap-normalize dim-acc)
        (let* ((p (car u))
               (unit-sym (car p))
               (exp (cdr p))
               (unit-name (symbol->string unit-sym))
               (entry (hash-table-ref/default __unit_registry__ unit-name #f)))
          (if entry
              (loop (cdr u)
                    (umap-merge-add dim-acc (umap-scale (unit-entry-dim entry) exp)))
              ;; Unknown base unit — use unit itself as dimension
              (loop (cdr u)
                    (umap-merge-add dim-acc (list (cons unit-sym exp)))))))))

;; ================================================================
;; Conversion: convert quantity to target units
;; ================================================================

(define (__qty-convert__ q target-str)
  (let* ((src-umap (%qty-umap q))
         (src-mag  (%qty-magnitude q))
         (target-resolved (resolve-unit target-str))
         (target-scale (car target-resolved))
         (target-umap  (cdr target-resolved))
         ;; Check dimensional compatibility via dimension maps
         (src-dim (umap->dim-map src-umap))
         (target-dim (umap->dim-map target-umap)))
    (if (not (equal? src-dim target-dim))
        (error "Qty: incompatible dimensions for conversion"
               (umap->string src-umap) (umap->string target-umap))
        ;; Compute: src in base units, then to target units
        ;; src_base = src_mag * src_scale
        ;; target_mag = src_base / target_scale
        (let* ((src-scale (__umap-total-scale__ src-umap))
               (base-mag (__mag-mul__ src-mag src-scale))
               (new-mag  (__mag-div__ base-mag target-scale)))
          (%qty-make new-mag target-umap)))))

(define (__umap-total-scale__ umap)
  ;; Compute the combined scale factor for a umap
  (let loop ((u umap) (scale 1))
    (if (null? u) scale
        (let* ((p (car u))
               (unit-name (symbol->string (car p)))
               (exp (cdr p))
               (entry (hash-table-ref/default __unit_registry__ unit-name #f)))
          (if entry
              (loop (cdr u) (* scale (expt (unit-entry-scale entry) exp)))
              (loop (cdr u) scale))))))

(define (__qty-to-base__ q)
  ;; Convert to SI base units
  (let* ((src-umap (%qty-umap q))
         (src-mag  (%qty-magnitude q))
         (src-scale (__umap-total-scale__ src-umap))
         (base-mag (__mag-mul__ src-mag src-scale))
         ;; Expand umap to base units
         (base-umap (__expand-to-base-umap__ src-umap)))
    (%qty-make base-mag base-umap)))

(define (__expand-to-base-umap__ umap)
  (let loop ((u umap) (acc '()))
    (if (null? u) (umap-normalize acc)
        (let* ((p (car u))
               (unit-name (symbol->string (car p)))
               (exp (cdr p))
               (entry (hash-table-ref/default __unit_registry__ unit-name #f)))
          (if (and entry (not (equal? (unit-entry-umap entry) (list p))))
              ;; This unit expands to something else
              (loop (cdr u)
                    (umap-merge-add acc (umap-scale (unit-entry-umap entry) exp)))
              ;; Already a base unit
              (loop (cdr u) (umap-merge-add acc (list p))))))))

;; ================================================================
;; Magnitude arithmetic helpers (Decimal-aware)
;; ================================================================

(define (__mag-add__ a b)
  (cond
    ((and (%decimal? a) (%decimal? b)) (%decimal-add a b))
    ((%decimal? a) (%decimal-add a (Decimal b)))
    ((%decimal? b) (%decimal-add (Decimal a) b))
    (else (+ a b))))

(define (__mag-sub__ a b)
  (cond
    ((and (%decimal? a) (%decimal? b)) (%decimal-sub a b))
    ((%decimal? a) (%decimal-sub a (Decimal b)))
    ((%decimal? b) (%decimal-sub (Decimal a) b))
    (else (- a b))))

(define (__mag-mul__ a b)
  (cond
    ((and (%decimal? a) (%decimal? b)) (%decimal-mul a b))
    ((%decimal? a) (%decimal-mul a (if (%decimal? b) b (Decimal b))))
    ((%decimal? b) (%decimal-mul (Decimal a) b))
    (else (* a b))))

(define (__mag-div__ a b)
  (cond
    ((and (%decimal? a) (%decimal? b)) (%decimal-div a b 28))
    ((%decimal? a) (%decimal-div a (if (%decimal? b) b (Decimal b)) 28))
    ((%decimal? b) (%decimal-div (Decimal a) b 28))
    (else (/ a b))))

(define (__mag-negate__ a)
  (if (%decimal? a) (%decimal-negate a) (- a)))

(define (__mag-compare__ a b)
  (cond
    ((and (%decimal? a) (%decimal? b)) (%decimal-compare a b))
    ((%decimal? a) (%decimal-compare a (Decimal b)))
    ((%decimal? b) (%decimal-compare (Decimal a) b))
    (else (cond ((< a b) -1) ((> a b) 1) (else 0)))))

;; ================================================================
;; Quantity arithmetic
;; ================================================================

(define (__qty_add__ a b)
  (cond
    ((and (%qty? a) (%qty? b))
     (let ((ua (%qty-umap a)) (ub (%qty-umap b)))
       (if (not (umap-equal? ua ub))
           ;; Try converting b to a's units if same dimension
           (let ((dim-a (umap->dim-map ua))
                 (dim-b (umap->dim-map ub)))
             (if (not (equal? dim-a dim-b))
                 (error "Qty: incompatible dimensions"
                        (umap->string ua) (umap->string ub))
                 ;; Convert b to a's units
                 (let* ((scale-a (__umap-total-scale__ ua))
                        (scale-b (__umap-total-scale__ ub))
                        (b-in-a (__mag-div__ (__mag-mul__ (%qty-magnitude b) scale-b) scale-a)))
                   (%qty-make (__mag-add__ (%qty-magnitude a) b-in-a) ua))))
           ;; Same units — just add magnitudes
           (%qty-make (__mag-add__ (%qty-magnitude a) (%qty-magnitude b)) ua))))
    ((%qty? a)
     (if (umap-dimensionless? (%qty-umap a))
         (%qty-make (__mag-add__ (%qty-magnitude a) b) '())
         (error "Qty: cannot add number to dimensional quantity")))
    ((%qty? b)
     (if (umap-dimensionless? (%qty-umap b))
         (%qty-make (__mag-add__ a (%qty-magnitude b)) '())
         (error "Qty: cannot add dimensional quantity to number")))
    (else (error "Qty: unexpected add arguments" a b))))

(define (__qty_sub__ a b)
  (cond
    ((and (%qty? a) (%qty? b))
     (let ((ua (%qty-umap a)) (ub (%qty-umap b)))
       (if (not (umap-equal? ua ub))
           (let ((dim-a (umap->dim-map ua))
                 (dim-b (umap->dim-map ub)))
             (if (not (equal? dim-a dim-b))
                 (error "Qty: incompatible dimensions"
                        (umap->string ua) (umap->string ub))
                 (let* ((scale-a (__umap-total-scale__ ua))
                        (scale-b (__umap-total-scale__ ub))
                        (b-in-a (__mag-div__ (__mag-mul__ (%qty-magnitude b) scale-b) scale-a)))
                   (%qty-make (__mag-sub__ (%qty-magnitude a) b-in-a) ua))))
           (%qty-make (__mag-sub__ (%qty-magnitude a) (%qty-magnitude b)) ua))))
    ((%qty? a)
     (if (umap-dimensionless? (%qty-umap a))
         (%qty-make (__mag-sub__ (%qty-magnitude a) b) '())
         (error "Qty: cannot subtract number from dimensional quantity")))
    ((%qty? b)
     (if (umap-dimensionless? (%qty-umap b))
         (%qty-make (__mag-sub__ a (%qty-magnitude b)) '())
         (error "Qty: cannot subtract dimensional quantity from number")))
    (else (error "Qty: unexpected sub arguments" a b))))

(define (__qty_mul__ a b)
  (cond
    ((and (%qty? a) (%qty? b))
     (let* ((new-umap (umap-multiply (%qty-umap a) (%qty-umap b)))
            (new-mag  (__mag-mul__ (%qty-magnitude a) (%qty-magnitude b))))
       (if (null? new-umap)
           new-mag  ;; dimensionless result -> plain number
           (%qty-make new-mag new-umap))))
    ((and (%qty? a) (or (number? b) (%decimal? b)))
     (%qty-make (__mag-mul__ (%qty-magnitude a) b) (%qty-umap a)))
    ((and (or (number? a) (%decimal? a)) (%qty? b))
     (%qty-make (__mag-mul__ a (%qty-magnitude b)) (%qty-umap b)))
    (else (error "Qty: unexpected mul arguments" a b))))

(define (__qty_div__ a b)
  (cond
    ((and (%qty? a) (%qty? b))
     (let* ((new-umap (umap-divide (%qty-umap a) (%qty-umap b)))
            (new-mag  (__mag-div__ (%qty-magnitude a) (%qty-magnitude b))))
       (if (null? new-umap)
           new-mag  ;; dimensionless result -> plain number
           (%qty-make new-mag new-umap))))
    ((and (%qty? a) (or (number? b) (%decimal? b)))
     (%qty-make (__mag-div__ (%qty-magnitude a) b) (%qty-umap a)))
    ((and (or (number? a) (%decimal? a)) (%qty? b))
     (%qty-make (__mag-div__ a (%qty-magnitude b))
                (umap-scale (%qty-umap b) -1)))
    (else (error "Qty: unexpected div arguments" a b))))

(define (__qty_neg__ a)
  (%qty-make (__mag-negate__ (%qty-magnitude a)) (%qty-umap a)))

(define (__qty_pow__ a b)
  (cond
    ((and (%qty? a) (number? b))
     (let* ((new-umap (umap-power (%qty-umap a) b))
            (new-mag  (expt (%qty-magnitude a) b)))
       (if (null? new-umap)
           new-mag
           (%qty-make new-mag new-umap))))
    ((and (%qty? a) (%qty? b))
     (error "Qty: exponent cannot be a dimensional quantity"))
    (else (error "Qty: unexpected pow arguments" a b))))

;; ================================================================
;; Comparison operators for quantities
;; ================================================================

(define (__qty_lt__ a b)
  (cond
    ((and (%qty? a) (%qty? b))
     (let ((ua (%qty-umap a)) (ub (%qty-umap b)))
       (if (umap-equal? ua ub)
           (< (__mag-compare__ (%qty-magnitude a) (%qty-magnitude b)) 0)
           (let ((dim-a (umap->dim-map ua)) (dim-b (umap->dim-map ub)))
             (if (not (equal? dim-a dim-b))
                 (error "Qty: incompatible dimensions for comparison"
                        (umap->string ua) (umap->string ub))
                 (let* ((scale-a (__umap-total-scale__ ua))
                        (scale-b (__umap-total-scale__ ub))
                        (a-base (__mag-mul__ (%qty-magnitude a) scale-a))
                        (b-base (__mag-mul__ (%qty-magnitude b) scale-b)))
                   (< (__mag-compare__ a-base b-base) 0)))))))
    (else (error "Qty: cannot compare quantity with non-quantity"))))

(define (__qty_gt__ a b)
  (cond
    ((and (%qty? a) (%qty? b))
     (let ((ua (%qty-umap a)) (ub (%qty-umap b)))
       (if (umap-equal? ua ub)
           (> (__mag-compare__ (%qty-magnitude a) (%qty-magnitude b)) 0)
           (let ((dim-a (umap->dim-map ua)) (dim-b (umap->dim-map ub)))
             (if (not (equal? dim-a dim-b))
                 (error "Qty: incompatible dimensions for comparison"
                        (umap->string ua) (umap->string ub))
                 (let* ((scale-a (__umap-total-scale__ ua))
                        (scale-b (__umap-total-scale__ ub))
                        (a-base (__mag-mul__ (%qty-magnitude a) scale-a))
                        (b-base (__mag-mul__ (%qty-magnitude b) scale-b)))
                   (> (__mag-compare__ a-base b-base) 0)))))))
    (else (error "Qty: cannot compare quantity with non-quantity"))))

(define (__qty_eq__ a b)
  (cond
    ((and (%qty? a) (%qty? b))
     (let ((ua (%qty-umap a)) (ub (%qty-umap b)))
       (if (umap-equal? ua ub)
           (= (__mag-compare__ (%qty-magnitude a) (%qty-magnitude b)) 0)
           (let ((dim-a (umap->dim-map ua)) (dim-b (umap->dim-map ub)))
             (if (not (equal? dim-a dim-b))
                 #f  ;; different dimensions are never equal
                 (let* ((scale-a (__umap-total-scale__ ua))
                        (scale-b (__umap-total-scale__ ub))
                        (a-base (__mag-mul__ (%qty-magnitude a) scale-a))
                        (b-base (__mag-mul__ (%qty-magnitude b) scale-b)))
                   (= (__mag-compare__ a-base b-base) 0)))))))
    (else #f)))

;; ================================================================
;; Operator chaining: wrap ad.scm's dispatch functions
;; ================================================================

(let ((__orig_ad_add__ __ad_add__)
      (__orig_ad_sub__ __ad_sub__)
      (__orig_ad_mul__ __ad_mul__)
      (__orig_ad_div__ __ad_div__)
      (__orig_ad_pow__ __ad_pow__)
      (__orig_ad_neg__ __ad_neg__))

  (set! __ad_add__ (lambda (a b)
    (if (or (%qty? a) (%qty? b))
        (__qty_add__ a b)
        (__orig_ad_add__ a b))))

  (set! __ad_sub__ (lambda (a b)
    (if (or (%qty? a) (%qty? b))
        (__qty_sub__ a b)
        (__orig_ad_sub__ a b))))

  (set! __ad_mul__ (lambda (a b)
    (if (or (%qty? a) (%qty? b))
        (__qty_mul__ a b)
        (__orig_ad_mul__ a b))))

  (set! __ad_div__ (lambda (a b)
    (if (or (%qty? a) (%qty? b))
        (__qty_div__ a b)
        (__orig_ad_div__ a b))))

  (set! __ad_pow__ (lambda (a b)
    (if (or (%qty? a) (%qty? b))
        (__qty_pow__ a b)
        (__orig_ad_pow__ a b))))

  (set! __ad_neg__ (lambda (a)
    (if (%qty? a)
        (__qty_neg__ a)
        (__orig_ad_neg__ a)))))

;; Wrap comparison operators
(let ((__prev_<__ <) (__prev_>__ >) (__prev_=__ =))
  (set! < (lambda (a b)
    (if (or (%qty? a) (%qty? b))
        (__qty_lt__ a b)
        (__prev_<__ a b))))
  (set! > (lambda (a b)
    (if (or (%qty? a) (%qty? b))
        (__qty_gt__ a b)
        (__prev_>__ a b))))
  (set! = (lambda (a b)
    (if (or (%qty? a) (%qty? b))
        (__qty_eq__ a b)
        (__prev_=__ a b)))))

;; ================================================================
;; __send__ dispatch for Qty
;; ================================================================

(define (__qty-send__ obj msg)
  (cond
    ;; Properties
    ((eq? msg 'value)     (%qty-magnitude obj))
    ((eq? msg 'amount)    (%qty-magnitude obj))  ;; Money compat
    ((eq? msg 'magnitude) (%qty-magnitude obj))
    ((eq? msg 'units)     (umap->string (%qty-umap obj)))
    ((eq? msg 'dimension) (umap->string (umap->dim-map (%qty-umap obj))))
    ;; Conversion
    ((eq? msg 'to)
     (lambda (target) (__qty-convert__ obj target)))
    ((eq? msg 'to_base)
     (lambda () (__qty-to-base__ obj)))
    ;; Format
    ((eq? msg 'format)
     (lambda ()
       (let* ((mag (%qty-magnitude obj))
              (ustr (umap->string (%qty-umap obj)))
              (mag-str (if (%decimal? mag)
                           (%decimal-to-string mag)
                           (number->string mag))))
         (if (string=? ustr "")
             mag-str
             (string-append mag-str " " ustr)))))
    ;; Rounding
    ((eq? msg 'round)
     (lambda (n)
       (let ((mag (%qty-magnitude obj)))
         (%qty-make (if (%decimal? mag)
                        (%decimal-round mag n 0)
                        (let ((f (expt 10 n)))
                          (/ (round (* mag f)) f)))
                    (%qty-umap obj)))))
    ;; Currency-specific
    ((eq? msg 'currency)
     (let ((umap (%qty-umap obj)))
       (if (and (pair? umap) (null? (cdr umap)) (= (cdar umap) 1))
           (let* ((unit-name (symbol->string (caar umap)))
                  (entry (hash-table-ref/default __unit_registry__ unit-name #f)))
             (if (and entry
                      (let ((dim (unit-entry-dim entry)))
                        (and (pair? dim) (null? (cdr dim))
                             (let ((ds (symbol->string (caar dim))))
                               (and (>= (string-length ds) 9)
                                    (string=? (substring ds 0 9) "currency_"))))))
                 (caar umap)  ;; Return the currency symbol (e.g., USD)
                 (error "Qty: not a currency quantity")))
           (error "Qty: not a currency quantity"))))
    ;; Arithmetic methods (for backward compat with Money)
    ((eq? msg 'add)  (lambda (o) (+ obj o)))
    ((eq? msg 'sub)  (lambda (o) (- obj o)))
    ((eq? msg 'mul)  (lambda (o) (* obj o)))
    ((eq? msg 'div)  (lambda (o) (/ obj o)))
    ;; Type tag
    ((eq? msg '__type__) 'quantity)
    (else (error "Qty: unknown method" msg))))

;; Chain onto previous __send__
(let ((__prev-send__ __send__))
  (set! __send__
    (lambda (obj msg)
      (cond
        ((%qty? obj) (__qty-send__ obj msg))
        (else (__prev-send__ obj msg))))))

;; ================================================================
;; Money backward-compatibility alias
;; ================================================================

(define Money Qty)
