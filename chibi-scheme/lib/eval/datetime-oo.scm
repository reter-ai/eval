;; eval/datetime-oo.scm -- OO wrappers for DateTime, Date, TimeDelta
;; Loaded after collection-oo.scm. Chains onto previous __send__ dispatch.

;; ================================================================
;; DateTime constructor with static methods
;; ================================================================

(define DateTime
  (let ((statics (make-hash-table)))
    (hash-table-set! statics 'now       (lambda () (%datetime-now)))
    (hash-table-set! statics 'utc_now   (lambda () (%datetime-utc-now)))
    (hash-table-set! statics 'from_epoch (lambda (s) (%datetime-from-epoch s)))
    (hash-table-set! statics 'parse     (lambda (s) (%datetime-parse s)))
    (lambda args
      (if (and (pair? args) (null? (cdr args)) (symbol? (car args))
               (hash-table-exists? statics (car args)))
          (hash-table-ref statics (car args))
          (apply %make-datetime args)))))

;; ================================================================
;; Date constructor with static methods
;; ================================================================

(define Date
  (let ((statics (make-hash-table)))
    (hash-table-set! statics 'today (lambda () (%date-today)))
    (lambda args
      (if (and (pair? args) (null? (cdr args)) (symbol? (car args))
               (hash-table-exists? statics (car args)))
          (hash-table-ref statics (car args))
          (apply %make-date args)))))

;; ================================================================
;; TimeDelta convenience constructor
;; ================================================================

(define (TimeDelta . args)
  (cond
    ((= (length args) 1)
     (%make-timedelta (car args)))
    ((= (length args) 4)
     (let ((d (list-ref args 0)) (h (list-ref args 1))
           (m (list-ref args 2)) (s (list-ref args 3)))
       (%make-timedelta (+ (* d 86400) (* h 3600) (* m 60) s))))
    (else (error "TimeDelta: expected 1 or 4 arguments"))))

;; ================================================================
;; DateTime __send__ dispatch
;; ================================================================

(define (__datetime-send__ obj msg)
  (cond
    ;; Properties
    ((eq? msg 'year)    (%datetime-year obj))
    ((eq? msg 'month)   (%datetime-month obj))
    ((eq? msg 'day)     (%datetime-day obj))
    ((eq? msg 'hour)    (%datetime-hour obj))
    ((eq? msg 'minute)  (%datetime-minute obj))
    ((eq? msg 'second)  (%datetime-second obj))
    ((eq? msg 'epoch)   (%datetime-epoch obj))
    ((eq? msg 'offset)  (%datetime-offset obj))
    ;; Methods
    ((eq? msg 'format)    (lambda (fmt) (%datetime-format obj fmt)))
    ((eq? msg 'iso)       (lambda () (%datetime-to-iso obj)))
    ((eq? msg 'add)       (lambda (td) (%datetime-add-seconds obj (%timedelta-seconds td))))
    ((eq? msg 'sub)       (lambda (other)
                            (if (%timedelta? other)
                                (%datetime-add-seconds obj (- (%timedelta-seconds other)))
                                (%datetime-diff obj other))))
    ((eq? msg 'to_utc)    (lambda () (%datetime-to-utc obj)))
    ((eq? msg 'to_offset) (lambda (off) (%datetime-to-offset obj off)))
    ((eq? msg 'to_date)   (lambda () (%datetime-to-date obj)))
    ((eq? msg 'lt)   (lambda (o) (< (%datetime-compare obj o) 0)))
    ((eq? msg 'gt)   (lambda (o) (> (%datetime-compare obj o) 0)))
    ((eq? msg 'eq)   (lambda (o) (= (%datetime-compare obj o) 0)))
    ((eq? msg 'lte)  (lambda (o) (<= (%datetime-compare obj o) 0)))
    ((eq? msg 'gte)  (lambda (o) (>= (%datetime-compare obj o) 0)))
    (else (error "DateTime: unknown method" msg))))

;; ================================================================
;; Date __send__ dispatch
;; ================================================================

(define (__date-send__ obj msg)
  (cond
    ;; Properties
    ((eq? msg 'year)    (%date-year obj))
    ((eq? msg 'month)   (%date-month obj))
    ((eq? msg 'day)     (%date-day obj))
    ;; Methods
    ((eq? msg 'format)    (lambda (fmt) (%date-format obj fmt)))
    ((eq? msg 'to_datetime) (lambda () (%date-to-datetime obj)))
    ((eq? msg 'add_days)  (lambda (n) (%date-add-days obj n)))
    ((eq? msg 'diff)      (lambda (other) (%date-diff obj other)))
    ((eq? msg 'lt)   (lambda (o) (< (%date-compare obj o) 0)))
    ((eq? msg 'gt)   (lambda (o) (> (%date-compare obj o) 0)))
    ((eq? msg 'eq)   (lambda (o) (= (%date-compare obj o) 0)))
    ((eq? msg 'lte)  (lambda (o) (<= (%date-compare obj o) 0)))
    ((eq? msg 'gte)  (lambda (o) (>= (%date-compare obj o) 0)))
    (else (error "Date: unknown method" msg))))

;; ================================================================
;; TimeDelta __send__ dispatch
;; ================================================================

(define (__timedelta-send__ obj msg)
  (cond
    ;; Properties
    ((eq? msg 'total_seconds) (%timedelta-seconds obj))
    ((eq? msg 'seconds)       (%timedelta-seconds obj))
    ((eq? msg 'days)          (%timedelta-days obj))
    ((eq? msg 'hours)         (%timedelta-hours obj))
    ((eq? msg 'minutes)       (%timedelta-minutes obj))
    ;; Methods
    ((eq? msg 'add)     (lambda (other) (%timedelta-add obj other)))
    ((eq? msg 'negate)  (lambda () (%timedelta-negate obj)))
    ((eq? msg 'lt)   (lambda (o) (< (%timedelta-compare obj o) 0)))
    ((eq? msg 'gt)   (lambda (o) (> (%timedelta-compare obj o) 0)))
    ((eq? msg 'eq)   (lambda (o) (= (%timedelta-compare obj o) 0)))
    ((eq? msg 'lte)  (lambda (o) (<= (%timedelta-compare obj o) 0)))
    ((eq? msg 'gte)  (lambda (o) (>= (%timedelta-compare obj o) 0)))
    (else (error "TimeDelta: unknown method" msg))))

;; ================================================================
;; Chain onto previous __send__
;; ================================================================

(let ((__prev-send__ __send__))
  (set! __send__
    (lambda (obj msg)
      (cond
        ((%datetime? obj)  (__datetime-send__ obj msg))
        ((%date? obj)      (__date-send__ obj msg))
        ((%timedelta? obj) (__timedelta-send__ obj msg))
        (else (__prev-send__ obj msg))))))
