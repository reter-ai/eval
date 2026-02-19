;; eval/grammar-rt.scm -- Grammar and Parser OO wrappers
;;
;; Grammar("lark EBNF") returns an object with:
;;   ->compile() -- compile grammar to a native parser
;;
;; The compiled Parser object supports:
;;   ->parse("input text") -- parse input, returns nested list AST

(define (Grammar text)
  (let ((raw (__grammar_create__ text)))
    (lambda (__msg__)
      (cond
        ((eq? __msg__ 'compile)
         (lambda ()
           (let ((parser-raw (__grammar_compile__ raw)))
             ;; Wrap parser C object in lambda for -> dispatch
             (lambda (__msg2__)
               (cond
                 ((eq? __msg2__ 'parse)
                  (lambda (input) (__parser_parse__ parser-raw input)))
                 (else (error "Parser: unknown method" __msg2__)))))))
        (else (error "Grammar: unknown method" __msg__))))))
