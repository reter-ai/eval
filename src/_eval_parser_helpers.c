/*  _eval_parser_helpers.c -- Helper functions for building sexp trees  */

#include "_eval_parser_helpers.h"
#include "_eval_lexer.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Lemon-generated parser API (defined in eval_grammar.c) */
void *ParseAlloc(void *(*mallocProc)(size_t));
void  Parse(void *yyp, int yymajor, EvalToken yyminor, EvalParserState *state);
void  ParseFree(void *p, void (*freeProc)(void*));

/* Convenience: intern a symbol from C string */
sexp ps_intern(sexp ctx, const char *name) {
    return sexp_intern(ctx, name, -1);
}

/* Map operator token type to Scheme symbol for op-as-value */
sexp ps_intern_op(sexp ctx, int token_type) {
    const char *name;
    switch (token_type) {
    case TOK_PLUS:    name = "+"; break;
    case TOK_MINUS:   name = "-"; break;
    case TOK_STAR:    name = "*"; break;
    case TOK_SLASH:   name = "/"; break;
    case TOK_PERCENT: name = "modulo"; break;
    case TOK_STARSTAR:name = "expt"; break;
    case TOK_EQEQ:    name = "equal?"; break;
    case TOK_EQQ:     name = "eq?"; break;
    case TOK_LT:      name = "<"; break;
    case TOK_GT:      name = ">"; break;
    case TOK_LTE:     name = "<="; break;
    case TOK_GTE:     name = ">="; break;
    case TOK_BANG:    name = "not"; break;
    case TOK_BITAND:  name = "bitwise-and"; break;
    case TOK_BITOR:   name = "bitwise-ior"; break;
    case TOK_BITNOT:  name = "bitwise-not"; break;
    case TOK_SHL:     name = "arithmetic-shift"; break;
    case TOK_SHR:     name = "shift-right"; break;
    case TOK_CONCAT:  name = "string-append"; break;
    default:          name = "+"; break;  /* shouldn't happen */
    }
    return sexp_intern(ctx, name, -1);
}

/* Build (set! name (op name val)) */
sexp ps_set_op(sexp ctx, const char *op, sexp name, sexp val) {
    sexp_gc_var3(tmp, inner, result);
    sexp_gc_preserve3(ctx, tmp, inner, result);
    inner = sexp_list3(ctx, ps_intern(ctx, op), name, val);
    result = sexp_list3(ctx, ps_intern(ctx, "set!"), name, inner);
    sexp_gc_release3(ctx);
    return result;
}

/* Build (set! name (+ name 1)) or (set! name (- name 1)) */
sexp ps_incr(sexp ctx, sexp name, int delta) {
    sexp_gc_var3(one, inner, result);
    sexp_gc_preserve3(ctx, one, inner, result);
    one = sexp_make_fixnum(1);
    if (delta > 0)
        inner = sexp_list3(ctx, ps_intern(ctx, "+"), name, one);
    else
        inner = sexp_list3(ctx, ps_intern(ctx, "-"), name, one);
    result = sexp_list3(ctx, ps_intern(ctx, "set!"), name, inner);
    sexp_gc_release3(ctx);
    return result;
}

/* Build (arithmetic-shift a (- b)) for right shift */
sexp ps_shift_right(sexp ctx, sexp left, sexp right) {
    sexp_gc_var2(neg, result);
    sexp_gc_preserve2(ctx, neg, result);
    neg = sexp_list2(ctx, ps_intern(ctx, "-"), right);
    result = sexp_list3(ctx, ps_intern(ctx, "arithmetic-shift"), left, neg);
    sexp_gc_release2(ctx);
    return result;
}

/* Check if an sexp tree contains a reference to the __return__ symbol.
   Used to decide whether a function body needs call/cc wrapping. */
static int ps_uses_return(sexp ctx, sexp expr) {
    if (!sexp_pairp(expr)) {
        return sexp_symbolp(expr) && expr == ps_intern(ctx, "__return__");
    }
    /* Don't descend into nested lambda (they have their own __return__) */
    if (sexp_car(expr) == ps_intern(ctx, "lambda"))
        return 0;
    /* Check car and cdr */
    if (ps_uses_return(ctx, sexp_car(expr)))
        return 1;
    return ps_uses_return(ctx, sexp_cdr(expr));
}

/* Build function lambda.
   If body uses 'return', wraps in call/cc for early return support.
   Otherwise builds a plain lambda to preserve tail call optimization. */
sexp ps_make_function(sexp ctx, sexp params, sexp body) {
    if (ps_uses_return(ctx, body)) {
        sexp return_sym = ps_intern(ctx, "__return__");
        sexp inner_lambda = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                                       sexp_list1(ctx, return_sym), body);
        sexp callcc = sexp_list2(ctx, ps_intern(ctx, "call-with-current-continuation"),
                                 inner_lambda);
        return sexp_list3(ctx, ps_intern(ctx, "lambda"), params, callcc);
    }
    return sexp_list3(ctx, ps_intern(ctx, "lambda"), params, body);
}

/* Build (lambda (a . rest) body), with optional call/cc for return */
sexp ps_make_function_rest(sexp ctx, sexp params, sexp rest, sexp body) {
    /* Build dotted pair params list: (a b . rest) */
    sexp dotted;
    if (sexp_nullp(params)) {
        dotted = rest;
    } else {
        dotted = params;
        sexp p = params;
        while (!sexp_nullp(sexp_cdr(p)) && sexp_pairp(sexp_cdr(p)))
            p = sexp_cdr(p);
        sexp_cdr(p) = rest;
    }
    if (ps_uses_return(ctx, body)) {
        sexp return_sym = ps_intern(ctx, "__return__");
        sexp inner_lambda = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                                       sexp_list1(ctx, return_sym), body);
        sexp callcc = sexp_list2(ctx, ps_intern(ctx, "call-with-current-continuation"),
                                 inner_lambda);
        return sexp_list3(ctx, ps_intern(ctx, "lambda"), dotted, callcc);
    }
    return sexp_list3(ctx, ps_intern(ctx, "lambda"), dotted, body);
}

/* Build while loop:
   (call/cc (lambda (__break__)
     (letrec ((__loop__ (lambda ()
       (if cond (begin body (__loop__))))))
       (__loop__)))) */
sexp ps_make_while(sexp ctx, sexp cond, sexp body) {
    /*  (call-with-current-continuation (lambda (__break__)
          (letrec ((__loop__ (lambda () (if cond (begin body (__loop__))))))
            (__loop__)))) */
    sexp loop_sym = ps_intern(ctx, "__loop__");
    sexp break_sym = ps_intern(ctx, "__break__");
    sexp loop_call = sexp_list1(ctx, loop_sym);

    /* Build (begin body... (__loop__)), splicing body if it's a begin. */
    sexp lambda_body;
    if (sexp_pairp(body) && sexp_car(body) == ps_intern(ctx, "begin")) {
        /* Splice: (begin e1 e2 ...) + (__loop__) → (begin e1 e2 ... (__loop__)) */
        lambda_body = sexp_cons(ctx, ps_intern(ctx, "begin"), SEXP_NULL);
        sexp tail = lambda_body;
        sexp p = sexp_cdr(body);
        while (sexp_pairp(p)) {
            sexp_cdr(tail) = sexp_cons(ctx, sexp_car(p), SEXP_NULL);
            tail = sexp_cdr(tail);
            p = sexp_cdr(p);
        }
        sexp_cdr(tail) = sexp_cons(ctx, loop_call, SEXP_NULL);
    } else {
        lambda_body = sexp_list3(ctx, ps_intern(ctx, "begin"), body, loop_call);
    }
    sexp if_form = sexp_list3(ctx, ps_intern(ctx, "if"), cond, lambda_body);
    sexp lambda_form = sexp_list3(ctx, ps_intern(ctx, "lambda"), SEXP_NULL, if_form);
    sexp binding = sexp_list1(ctx, sexp_list2(ctx, loop_sym, lambda_form));
    sexp letrec_form = sexp_list3(ctx, ps_intern(ctx, "letrec"), binding, loop_call);

    /* Wrap with call/cc for break */
    sexp break_lambda = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                                   sexp_list1(ctx, break_sym), letrec_form);
    return sexp_list2(ctx, ps_intern(ctx, "call-with-current-continuation"),
                      break_lambda);
}

/* Build for loop:
   (begin init
     (call/cc (lambda (__break__)
       (letrec ((__loop__ (lambda ()
         (if cond (begin body step (__loop__))))))
         (__loop__))))) */
sexp ps_make_for(sexp ctx, sexp init, sexp cond, sexp step, sexp body) {
    sexp loop_sym = ps_intern(ctx, "__loop__");
    sexp break_sym = ps_intern(ctx, "__break__");
    sexp loop_call = sexp_list1(ctx, loop_sym);

    /* (begin body... step (__loop__)), splicing body if it's a begin. */
    sexp inner_body;
    if (sexp_pairp(body) && sexp_car(body) == ps_intern(ctx, "begin")) {
        inner_body = sexp_cons(ctx, ps_intern(ctx, "begin"), SEXP_NULL);
        sexp tail = inner_body;
        sexp p = sexp_cdr(body);
        while (sexp_pairp(p)) {
            sexp_cdr(tail) = sexp_cons(ctx, sexp_car(p), SEXP_NULL);
            tail = sexp_cdr(tail);
            p = sexp_cdr(p);
        }
        sexp_cdr(tail) = sexp_cons(ctx, step,
                           sexp_cons(ctx, loop_call, SEXP_NULL));
    } else {
        inner_body = sexp_cons(ctx, ps_intern(ctx, "begin"),
                        sexp_cons(ctx, body,
                          sexp_cons(ctx, step,
                            sexp_cons(ctx, loop_call, SEXP_NULL))));
    }

    sexp if_form = sexp_list3(ctx, ps_intern(ctx, "if"), cond, inner_body);
    sexp lambda_form = sexp_list3(ctx, ps_intern(ctx, "lambda"), SEXP_NULL, if_form);
    sexp binding = sexp_list1(ctx, sexp_list2(ctx, loop_sym, lambda_form));
    sexp letrec_form = sexp_list3(ctx, ps_intern(ctx, "letrec"), binding, loop_call);

    /* Wrap with call/cc for break */
    sexp break_lambda = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                                   sexp_list1(ctx, break_sym), letrec_form);
    sexp callcc = sexp_list2(ctx, ps_intern(ctx, "call-with-current-continuation"),
                             break_lambda);

    /* If init is (define var val), scope it to the loop with let */
    if (sexp_pairp(init) && sexp_car(init) == ps_intern(ctx, "define")) {
        sexp var = sexp_cadr(init);
        sexp val = sexp_caddr(init);
        sexp binding = sexp_list1(ctx, sexp_list2(ctx, var, val));
        return sexp_list3(ctx, ps_intern(ctx, "let"), binding, callcc);
    }
    return sexp_list3(ctx, ps_intern(ctx, "begin"), init, callcc);
}

/* Build do-until loop:
   (call/cc (lambda (__break__)
     (letrec ((__loop__ (lambda ()
       (begin body (if (not cond) (__loop__))))))
       (__loop__)))) */
sexp ps_make_do_until(sexp ctx, sexp body, sexp cond) {
    sexp loop_sym = ps_intern(ctx, "__loop__");
    sexp break_sym = ps_intern(ctx, "__break__");
    sexp loop_call = sexp_list1(ctx, loop_sym);

    sexp not_cond = sexp_list2(ctx, ps_intern(ctx, "not"), cond);
    sexp if_form = sexp_list3(ctx, ps_intern(ctx, "if"), not_cond, loop_call);
    sexp inner = sexp_list3(ctx, ps_intern(ctx, "begin"), body, if_form);
    sexp lambda_form = sexp_list3(ctx, ps_intern(ctx, "lambda"), SEXP_NULL, inner);
    sexp binding = sexp_list1(ctx, sexp_list2(ctx, loop_sym, lambda_form));
    sexp letrec_form = sexp_list3(ctx, ps_intern(ctx, "letrec"), binding, loop_call);

    /* Wrap with call/cc for break */
    sexp break_lambda = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                                   sexp_list1(ctx, break_sym), letrec_form);
    return sexp_list2(ctx, ps_intern(ctx, "call-with-current-continuation"),
                      break_lambda);
}

/* Build do-while loop:
   (call/cc (lambda (__break__)
     (letrec ((__loop__ (lambda ()
       (begin body (if cond (__loop__))))))
       (__loop__)))) */
sexp ps_make_do_while(sexp ctx, sexp body, sexp cond) {
    sexp loop_sym = ps_intern(ctx, "__loop__");
    sexp break_sym = ps_intern(ctx, "__break__");
    sexp loop_call = sexp_list1(ctx, loop_sym);

    sexp if_form = sexp_list3(ctx, ps_intern(ctx, "if"), cond, loop_call);
    sexp inner = sexp_list3(ctx, ps_intern(ctx, "begin"), body, if_form);
    sexp lambda_form = sexp_list3(ctx, ps_intern(ctx, "lambda"), SEXP_NULL, inner);
    sexp binding = sexp_list1(ctx, sexp_list2(ctx, loop_sym, lambda_form));
    sexp letrec_form = sexp_list3(ctx, ps_intern(ctx, "letrec"), binding, loop_call);

    /* Wrap with call/cc for break */
    sexp break_lambda = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                                   sexp_list1(ctx, break_sym), letrec_form);
    return sexp_list2(ctx, ps_intern(ctx, "call-with-current-continuation"),
                      break_lambda);
}

/* break -> (__break__ '()) — calls the continuation captured by enclosing loop */
sexp ps_make_break(sexp ctx) {
    return sexp_list2(ctx, ps_intern(ctx, "__break__"),
                      sexp_list2(ctx, ps_intern(ctx, "quote"), SEXP_NULL));
}

/* return expr -> (__return__ expr) — calls the continuation captured by function */
sexp ps_make_return(sexp ctx, sexp expr) {
    return sexp_list2(ctx, ps_intern(ctx, "__return__"), expr);
}

/* Build constructor:
   (lambda (args) (begin
     (define __name__ 'Unnamed)
     (define __supers__ '())
     body
     self)) */
sexp ps_make_constructor(sexp ctx, sexp params, sexp body) {
    sexp_gc_var5(def_name, def_supers, self_sym, inner, result);
    sexp_gc_preserve5(ctx, def_name, def_supers, self_sym, inner, result);

    def_name = sexp_list3(ctx, ps_intern(ctx, "define"),
                          ps_intern(ctx, "__name__"),
                          sexp_list2(ctx, ps_intern(ctx, "quote"),
                                     ps_intern(ctx, "Unnamed")));
    def_supers = sexp_list3(ctx, ps_intern(ctx, "define"),
                            ps_intern(ctx, "__supers__"),
                            sexp_list2(ctx, ps_intern(ctx, "quote"), SEXP_NULL));
    self_sym = ps_intern(ctx, "self");

    /* (begin def_name def_supers (define self body) self) */
    sexp def_self = sexp_list3(ctx, ps_intern(ctx, "define"), self_sym, body);
    inner = sexp_cons(ctx, ps_intern(ctx, "begin"),
              sexp_cons(ctx, def_name,
                sexp_cons(ctx, def_supers,
                  sexp_cons(ctx, def_self,
                    sexp_cons(ctx, self_sym, SEXP_NULL)))));

    result = sexp_list3(ctx, ps_intern(ctx, "lambda"), params, inner);
    sexp_gc_release5(ctx);
    return result;
}

/* Build interface:
   (define self
     (lambda (__msg__)
       (cond
         ((eq? __msg__ 'entry1) expr1)
         ((eq? __msg__ 'entry2) expr2)
         ...
         (else (search-supertypes __supers__ __msg__))))) */
sexp ps_make_interface(sexp ctx, sexp entries) {
    sexp_gc_var5(msg_sym, clauses, cond_form, lambda_form, define_form);
    sexp_gc_preserve5(ctx, msg_sym, clauses, cond_form, lambda_form, define_form);

    msg_sym = ps_intern(ctx, "__msg__");

    /* Build cond clauses from entries (list of (name . expr) pairs) */
    clauses = SEXP_NULL;
    sexp p = entries;
    while (sexp_pairp(p)) {
        sexp entry = sexp_car(p);
        sexp name = sexp_car(entry);
        sexp expr = sexp_cdr(entry);
        sexp test = sexp_list3(ctx, ps_intern(ctx, "eq?"), msg_sym,
                               sexp_list2(ctx, ps_intern(ctx, "quote"), name));
        sexp clause = sexp_list2(ctx, test, expr);
        clauses = sexp_cons(ctx, clause, clauses);
        p = sexp_cdr(p);
    }

    /* Reverse clauses */
    sexp reversed = SEXP_NULL;
    while (sexp_pairp(clauses)) {
        reversed = sexp_cons(ctx, sexp_car(clauses), reversed);
        clauses = sexp_cdr(clauses);
    }
    clauses = reversed;

    /* Append else clause: (else (if (null? __supers__) (error "unknown message") ((car __supers__) __msg__))) */
    sexp supers_sym = ps_intern(ctx, "__supers__");
    sexp else_body = sexp_cons(ctx, ps_intern(ctx, "if"),
        sexp_list3(ctx,
            sexp_list2(ctx, ps_intern(ctx, "null?"), supers_sym),
            sexp_list2(ctx, ps_intern(ctx, "error"), sexp_c_string(ctx, "unknown message", -1)),
            sexp_list2(ctx, sexp_list2(ctx, ps_intern(ctx, "car"), supers_sym), msg_sym)));
    sexp else_clause = sexp_list2(ctx, ps_intern(ctx, "else"), else_body);
    clauses = ps_append(ctx, clauses, else_clause);

    /* Build cond form */
    cond_form = sexp_cons(ctx, ps_intern(ctx, "cond"), clauses);

    /* (lambda (__msg__) cond_form) */
    lambda_form = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                             sexp_list1(ctx, msg_sym), cond_form);

    /* Return just the lambda — the constructor wraps it as (define self ...) */
    define_form = lambda_form;

    sexp_gc_release5(ctx);
    return define_form;
}

/* Build super with __abstract_ok__ flag management:
   (begin
     (set! __abstract_ok__ #t)
     (set! __supers__ (append __supers__ (list expr)))
     (set! __abstract_ok__ #f)) */
sexp ps_make_super(sexp ctx, sexp expr) {
    sexp_gc_var2(list_expr, append_expr);
    sexp_gc_preserve2(ctx, list_expr, append_expr);

    sexp abs_sym = ps_intern(ctx, "__abstract_ok__");
    sexp set_true = sexp_list3(ctx, ps_intern(ctx, "set!"), abs_sym, SEXP_TRUE);
    sexp set_false = sexp_list3(ctx, ps_intern(ctx, "set!"), abs_sym, SEXP_FALSE);

    list_expr = sexp_list2(ctx, ps_intern(ctx, "list"), expr);
    append_expr = sexp_list3(ctx, ps_intern(ctx, "append"),
                             ps_intern(ctx, "__supers__"), list_expr);
    sexp set_supers = sexp_list3(ctx, ps_intern(ctx, "set!"),
                                  ps_intern(ctx, "__supers__"), append_expr);

    sexp result = sexp_cons(ctx, ps_intern(ctx, "begin"),
                   sexp_cons(ctx, set_true,
                     sexp_cons(ctx, set_supers,
                       sexp_cons(ctx, set_false, SEXP_NULL))));
    sexp_gc_release2(ctx);
    return result;
}

/* Build a static wrapper around a constructor (or standalone for statics-only).
   Generates:
     (let* ((__statics__ (let ((ht (make-hash-table)))
                           (hash-table-set! ht 'key1 val1) ...
                           ht))
            (__ctor__ <ctor-lambda>))      ; omitted if ctor == SEXP_FALSE
       (lambda __args__
         (if (and (pair? __args__) (null? (cdr __args__)) (symbol? (car __args__)))
           (if (hash-table-exists? __statics__ (car __args__))
             (hash-table-ref __statics__ (car __args__))
             <else-single-arg>)
           <else-normal-call>)))
*/
static sexp ps_make_static_wrapper(sexp ctx, sexp ctor, sexp statics) {
    sexp s_ht        = ps_intern(ctx, "ht");
    sexp s_statics   = ps_intern(ctx, "__statics__");
    sexp s_ctor      = ps_intern(ctx, "__ctor__");
    sexp s_args      = ps_intern(ctx, "__args__");
    sexp s_inst      = ps_intern(ctx, "__inst__");
    sexp s_msg       = ps_intern(ctx, "__msg__");
    sexp s_mkht      = ps_intern(ctx, "make-hash-table");
    sexp s_htset     = ps_intern(ctx, "hash-table-set!");
    sexp s_htexists  = ps_intern(ctx, "hash-table-exists?");
    sexp s_htref     = ps_intern(ctx, "hash-table-ref");
    sexp s_pair      = ps_intern(ctx, "pair?");
    sexp s_null      = ps_intern(ctx, "null?");
    sexp s_symbolp   = ps_intern(ctx, "symbol?");
    sexp s_car       = ps_intern(ctx, "car");
    sexp s_cdr       = ps_intern(ctx, "cdr");
    sexp s_apply     = ps_intern(ctx, "apply");
    sexp s_if        = ps_intern(ctx, "if");
    sexp s_and       = ps_intern(ctx, "and");
    sexp s_let       = ps_intern(ctx, "let");
    sexp s_letstar   = ps_intern(ctx, "let*");
    sexp s_lambda    = ps_intern(ctx, "lambda");
    sexp s_begin     = ps_intern(ctx, "begin");
    sexp s_error     = ps_intern(ctx, "error");
    sexp s_quote     = ps_intern(ctx, "quote");

    /* --- Build __statics__ initializer:
       (let ((ht (make-hash-table)))
         (hash-table-set! ht 'k1 v1)
         ...
         ht)  */
    sexp mkht_call = sexp_list1(ctx, s_mkht);
    sexp ht_binding = sexp_list1(ctx, sexp_list2(ctx, s_ht, mkht_call));

    /* Build list of (hash-table-set! ht 'key val) forms */
    sexp set_forms = SEXP_NULL;
    sexp p = statics;
    while (sexp_pairp(p)) {
        sexp entry = sexp_car(p);
        sexp key = sexp_car(entry);
        sexp val = sexp_cdr(entry);
        sexp quoted_key = sexp_list2(ctx, s_quote, key);
        sexp set_call = sexp_cons(ctx, s_htset,
                            sexp_cons(ctx, s_ht,
                                sexp_cons(ctx, quoted_key,
                                    sexp_cons(ctx, val, SEXP_NULL))));
        set_forms = sexp_cons(ctx, set_call, set_forms);
        p = sexp_cdr(p);
    }
    /* Reverse set_forms */
    sexp rev_sets = SEXP_NULL;
    while (sexp_pairp(set_forms)) {
        rev_sets = sexp_cons(ctx, sexp_car(set_forms), rev_sets);
        set_forms = sexp_cdr(set_forms);
    }

    /* (begin set1 set2 ... ht) or just (begin ht) if no entries */
    sexp ht_body;
    if (sexp_nullp(rev_sets)) {
        ht_body = s_ht;
    } else {
        ht_body = sexp_cons(ctx, s_begin, rev_sets);
        /* Append ht at end */
        sexp tail = ht_body;
        while (sexp_pairp(sexp_cdr(tail)))
            tail = sexp_cdr(tail);
        sexp_cdr(tail) = sexp_cons(ctx, s_ht, SEXP_NULL);
    }

    sexp statics_init = sexp_list3(ctx, s_let, ht_binding, ht_body);

    /* --- Build let* bindings --- */
    sexp letstar_bindings;
    if (ctor == SEXP_FALSE) {
        /* Statics-only: just __statics__ */
        letstar_bindings = sexp_list1(ctx, sexp_list2(ctx, s_statics, statics_init));
    } else {
        /* Full: __statics__ and __ctor__ */
        letstar_bindings = sexp_list2(ctx,
            sexp_list2(ctx, s_statics, statics_init),
            sexp_list2(ctx, s_ctor, ctor));
    }

    /* --- Common subexpressions --- */
    sexp car_args = sexp_list2(ctx, s_car, s_args);
    sexp cdr_args = sexp_list2(ctx, s_cdr, s_args);
    sexp ht_exists = sexp_list3(ctx, s_htexists, s_statics, car_args);
    sexp ht_get    = sexp_list3(ctx, s_htref, s_statics, car_args);

    /* --- Single-symbol-arg test:
       (and (pair? __args__) (null? (cdr __args__)) (symbol? (car __args__))) */
    sexp test_single = sexp_cons(ctx, s_and,
        sexp_cons(ctx, sexp_list2(ctx, s_pair, s_args),
            sexp_cons(ctx, sexp_list2(ctx, s_null, cdr_args),
                sexp_cons(ctx, sexp_list2(ctx, s_symbolp, car_args),
                    SEXP_NULL))));

    /* --- Else for single-arg (hash miss): --- */
    sexp else_single;
    if (ctor == SEXP_FALSE) {
        /* statics-only: error */
        else_single = sexp_list2(ctx, s_error,
            sexp_c_string(ctx, "static-only class: unknown key", -1));
    } else {
        /* try as 1-arg constructor call */
        else_single = sexp_list3(ctx, s_apply, s_ctor, s_args);
    }

    /* (if (hash-table-exists? ...) (hash-table-ref ...) else_single) */
    sexp single_branch = sexp_cons(ctx, s_if,
        sexp_cons(ctx, ht_exists,
            sexp_cons(ctx, ht_get,
                sexp_cons(ctx, else_single, SEXP_NULL))));

    /* --- Normal-call branch (multi-arg or zero-arg): --- */
    sexp normal_branch;
    if (ctor == SEXP_FALSE) {
        /* statics-only: error */
        normal_branch = sexp_list2(ctx, s_error,
            sexp_c_string(ctx, "static-only class: not callable", -1));
    } else {
        /* (let ((__inst__ (apply __ctor__ __args__)))
             (lambda (__msg__)
               (if (hash-table-exists? __statics__ __msg__)
                 (hash-table-ref __statics__ __msg__)
                 (__inst__ __msg__)))) */
        sexp apply_ctor = sexp_list3(ctx, s_apply, s_ctor, s_args);
        sexp inst_binding = sexp_list1(ctx, sexp_list2(ctx, s_inst, apply_ctor));

        sexp ht_exists_msg = sexp_list3(ctx, s_htexists, s_statics, s_msg);
        sexp ht_get_msg = sexp_list3(ctx, s_htref, s_statics, s_msg);
        sexp inst_dispatch = sexp_list2(ctx, s_inst, s_msg);

        sexp inner_if = sexp_cons(ctx, s_if,
            sexp_cons(ctx, ht_exists_msg,
                sexp_cons(ctx, ht_get_msg,
                    sexp_cons(ctx, inst_dispatch, SEXP_NULL))));

        sexp inner_lambda = sexp_list3(ctx, s_lambda,
            sexp_list1(ctx, s_msg), inner_if);

        normal_branch = sexp_list3(ctx, s_let, inst_binding, inner_lambda);
    }

    /* --- Outer if --- */
    sexp outer_if = sexp_cons(ctx, s_if,
        sexp_cons(ctx, test_single,
            sexp_cons(ctx, single_branch,
                sexp_cons(ctx, normal_branch, SEXP_NULL))));

    /* --- Outer lambda with rest param: (lambda __args__ outer_if) --- */
    sexp outer_lambda = sexp_list3(ctx, s_lambda, s_args, outer_if);

    /* --- Wrap in let* --- */
    return sexp_list3(ctx, s_letstar, letstar_bindings, outer_lambda);
}

/* Build constructor with static methods/values.
   iface_entries: list of (name . expr) pairs for the interface, or SEXP_NULL for statics-only. */
sexp ps_make_constructor_static(sexp ctx, sexp params, sexp statics, sexp iface_entries) {
    sexp ctor;
    if (sexp_nullp(iface_entries)) {
        ctor = SEXP_FALSE;  /* statics-only */
    } else {
        sexp body = ps_make_interface(ctx, iface_entries);
        ctor = ps_make_constructor(ctx, params, body);
    }
    return ps_make_static_wrapper(ctx, ctor, statics);
}

/* Build abstract method: (lambda __args__ (error "abstract method" "name")) */
sexp ps_make_abstract_method(sexp ctx, const char *name, int length) {
    char buf[256];
    int n = length < 255 ? length : 255;
    memcpy(buf, name, n);
    buf[n] = '\0';

    sexp args_sym = ps_intern(ctx, "__args__");
    sexp err_call = sexp_list3(ctx, ps_intern(ctx, "error"),
                               sexp_c_string(ctx, "abstract method", -1),
                               sexp_c_string(ctx, buf, n));
    return sexp_list3(ctx, ps_intern(ctx, "lambda"), args_sym, err_call);
}

/* Build abstract constructor: wraps normal constructor in a guard lambda.
   (let ((__inner_ctor__ <normal-constructor>))
     (lambda __args__
       (if (not __abstract_ok__) (error "cannot instantiate abstract class"))
       (set! __abstract_ok__ #f)
       (apply __inner_ctor__ __args__))) */
sexp ps_make_abstract_constructor(sexp ctx, sexp params, sexp body) {
    /* Build normal constructor first */
    sexp normal_ctor = ps_make_constructor(ctx, params, body);

    sexp abs_sym = ps_intern(ctx, "__abstract_ok__");
    sexp inner_sym = ps_intern(ctx, "__inner_ctor__");
    sexp args_sym = ps_intern(ctx, "__args__");

    sexp not_abs = sexp_list2(ctx, ps_intern(ctx, "not"), abs_sym);
    sexp err = sexp_list2(ctx, ps_intern(ctx, "error"),
                          sexp_c_string(ctx, "cannot instantiate abstract class", -1));
    sexp guard = sexp_list3(ctx, ps_intern(ctx, "if"), not_abs, err);
    sexp reset = sexp_list3(ctx, ps_intern(ctx, "set!"), abs_sym, SEXP_FALSE);
    sexp apply_call = sexp_list3(ctx, ps_intern(ctx, "apply"), inner_sym, args_sym);

    /* (lambda __args__ guard reset apply_call) — rest-param lambda */
    sexp wrapper = sexp_cons(ctx, ps_intern(ctx, "lambda"),
                     sexp_cons(ctx, args_sym,
                       sexp_cons(ctx, guard,
                         sexp_cons(ctx, reset,
                           sexp_cons(ctx, apply_call, SEXP_NULL)))));

    /* (let ((__inner_ctor__ normal_ctor)) wrapper) */
    sexp binding = sexp_list1(ctx, sexp_list2(ctx, inner_sym, normal_ctor));
    return sexp_list3(ctx, ps_intern(ctx, "let"), binding, wrapper);
}

/* Build abstract constructor with static wrapper */
sexp ps_make_abstract_constructor_static(sexp ctx, sexp params, sexp statics, sexp iface_entries) {
    sexp ctor;
    if (sexp_nullp(iface_entries)) {
        ctor = SEXP_FALSE;
    } else {
        sexp body = ps_make_interface(ctx, iface_entries);
        ctor = ps_make_abstract_constructor(ctx, params, body);
    }
    return ps_make_static_wrapper(ctx, ctor, statics);
}

/* Build let, let-star, or letrec with bindings and body */
sexp ps_make_let(sexp ctx, const char *kind, sexp bindings, sexp body) {
    return sexp_list3(ctx, ps_intern(ctx, kind), bindings, body);
}

/* Build (guard (var (#t handler)) body) */
/* Build (protect (var (else handler)) body) — using chibi's 'protect' macro */
sexp ps_make_try_catch(sexp ctx, sexp body, sexp var, sexp handler) {
    /* (protect (var (else handler)) body) */
    sexp else_clause = sexp_list2(ctx, ps_intern(ctx, "else"), handler);
    sexp protect_args = sexp_list2(ctx, var, else_clause);
    return sexp_list3(ctx, ps_intern(ctx, "protect"), protect_args, body);
}

/* Build (protect (var (test1 h1) (test2 h2) ...) body) */
sexp ps_make_try_catch_multi(sexp ctx, sexp body, sexp var, sexp clauses) {
    sexp protect_args = sexp_cons(ctx, var, clauses);
    return sexp_list3(ctx, ps_intern(ctx, "protect"), protect_args, body);
}

/* Helper: wrap an expression in (dynamic-wind (lambda () #f) (lambda () expr) (lambda () cleanup)) */
static sexp wrap_dynamic_wind(sexp ctx, sexp expr, sexp cleanup) {
    sexp dw = ps_intern(ctx, "dynamic-wind");
    sexp lam = ps_intern(ctx, "lambda");
    sexp before = sexp_list3(ctx, lam, SEXP_NULL, SEXP_FALSE);
    sexp thunk  = sexp_list3(ctx, lam, SEXP_NULL, expr);
    sexp after  = sexp_list3(ctx, lam, SEXP_NULL, cleanup);
    /* (dynamic-wind before thunk after) — no sexp_list4, build manually */
    return sexp_cons(ctx, dw,
               sexp_cons(ctx, before,
                   sexp_cons(ctx, thunk,
                       sexp_cons(ctx, after, SEXP_NULL))));
}

/*  with(r1 = e1, r2 = e2) body  →  nested let + dynamic-wind
    Each resource gets its own dynamic-wind that calls (r 'close) on exit.
    Inner resources are closed before outer ones. */
sexp ps_make_with(sexp ctx, sexp bindings, sexp body) {
    /* bindings is a list of (name value) pairs.
       Process from right to left: innermost binding wraps body first. */
    sexp pairs[64];  /* max 64 bindings */
    int n = 0;
    sexp p = bindings;
    while (sexp_pairp(p) && n < 64) {
        pairs[n++] = sexp_car(p);
        p = sexp_cdr(p);
    }

    sexp result = body;
    sexp lam = ps_intern(ctx, "lambda");
    sexp dw = ps_intern(ctx, "dynamic-wind");
    sexp close_sym = sexp_list2(ctx, ps_intern(ctx, "quote"),
                                ps_intern(ctx, "close"));

    for (int i = n - 1; i >= 0; i--) {
        sexp pair = pairs[i];
        sexp name = sexp_car(pair);
        sexp init = sexp_cadr(pair);

        /* ((name 'close)) — dispatch then call the returned function */
        sexp close_dispatch = sexp_list2(ctx, name, close_sym);
        sexp close_call = sexp_list1(ctx, close_dispatch);
        /* (lambda () #f) */
        sexp before = sexp_list3(ctx, lam, SEXP_NULL, SEXP_FALSE);
        /* (lambda () result) */
        sexp thunk = sexp_list3(ctx, lam, SEXP_NULL, result);
        /* (lambda () (name 'close)) */
        sexp after = sexp_list3(ctx, lam, SEXP_NULL, close_call);
        /* (dynamic-wind before thunk after) */
        sexp dw_call = sexp_cons(ctx, dw,
                           sexp_cons(ctx, before,
                               sexp_cons(ctx, thunk,
                                   sexp_cons(ctx, after, SEXP_NULL))));
        /* (let ((name init)) dw_call) */
        sexp binding = sexp_list1(ctx, sexp_list2(ctx, name, init));
        result = sexp_list3(ctx, ps_intern(ctx, "let"), binding, dw_call);
    }

    return result;
}

/*  async expr  →  (__async_dispatch__ (lambda () expr))  — always green thread */
sexp ps_make_async(sexp ctx, sexp expr) {
    sexp thunk = sexp_list3(ctx, ps_intern(ctx, "lambda"), SEXP_NULL, expr);
    return sexp_list2(ctx, ps_intern(ctx, "__async_dispatch__"), thunk);
}

/*  parallel async expr  →  (__parallel_async_dispatch__ (lambda () expr))  — always OS thread */
sexp ps_make_parallel_async(sexp ctx, sexp expr) {
    sexp thunk = sexp_list3(ctx, ps_intern(ctx, "lambda"), SEXP_NULL, expr);
    return sexp_list2(ctx, ps_intern(ctx, "__parallel_async_dispatch__"), thunk);
}

sexp ps_make_try_catch_finally(sexp ctx, sexp body, sexp var, sexp handler, sexp cleanup) {
    sexp try_catch = ps_make_try_catch(ctx, body, var, handler);
    return wrap_dynamic_wind(ctx, try_catch, cleanup);
}

sexp ps_make_try_catch_multi_finally(sexp ctx, sexp body, sexp var, sexp clauses, sexp cleanup) {
    sexp try_catch = ps_make_try_catch_multi(ctx, body, var, clauses);
    return wrap_dynamic_wind(ctx, try_catch, cleanup);
}

sexp ps_make_try_finally(sexp ctx, sexp body, sexp cleanup) {
    return wrap_dynamic_wind(ctx, body, cleanup);
}

/* Build (call-with-values (lambda () expr) (lambda params body)) */
sexp ps_make_receive(sexp ctx, sexp params, sexp expr, sexp body) {
    sexp_gc_var3(producer, consumer, result);
    sexp_gc_preserve3(ctx, producer, consumer, result);

    producer = sexp_list3(ctx, ps_intern(ctx, "lambda"), SEXP_NULL, expr);
    consumer = sexp_list3(ctx, ps_intern(ctx, "lambda"), params, body);
    result = sexp_list3(ctx, ps_intern(ctx, "call-with-values"), producer, consumer);

    sexp_gc_release3(ctx);
    return result;
}

/*  Record implementation — vector-backed objects with -> access:
    record Point(x, y) generates:
    (begin
      (define (Point x y)
        (let ((__rec_vec__ (vector 'Point x y)))
          (lambda (__msg__)
            (cond
              ((eq? __msg__ 'x) (vector-ref __rec_vec__ 1))
              ((eq? __msg__ 'y) (vector-ref __rec_vec__ 2))
              ((eq? __msg__ '__type__) 'Point)
              (else (error "unknown field" __msg__))))))
      (define (Point? v__)
        (and (procedure? v__)
             (protect (e__ (else #f))
               (eq? (v__ '__type__) 'Point))))) */
sexp ps_make_record(sexp ctx, sexp name, sexp fields) {
    sexp sym_str = sexp_symbol_to_string(ctx, name);
    const char *nstr = sexp_string_data(sym_str);
    char buf[256];

    sexp quoted_name = sexp_list2(ctx, ps_intern(ctx, "quote"), name);
    sexp msg_sym = ps_intern(ctx, "__msg__");
    sexp vec_sym = ps_intern(ctx, "__rec_vec__");

    /* --- Build dispatch cond clauses for each field --- */
    sexp clauses = SEXP_NULL;
    sexp p = fields;
    int idx = 1;
    while (sexp_pairp(p)) {
        sexp field = sexp_car(p);
        sexp test = sexp_list3(ctx, ps_intern(ctx, "eq?"), msg_sym,
                               sexp_list2(ctx, ps_intern(ctx, "quote"), field));
        sexp vref = sexp_list3(ctx, ps_intern(ctx, "vector-ref"),
                               vec_sym, sexp_make_fixnum(idx));
        clauses = sexp_cons(ctx, sexp_list2(ctx, test, vref), clauses);
        p = sexp_cdr(p);
        idx++;
    }

    /* Reverse clauses to preserve field order */
    sexp reversed = SEXP_NULL;
    while (sexp_pairp(clauses)) {
        reversed = sexp_cons(ctx, sexp_car(clauses), reversed);
        clauses = sexp_cdr(clauses);
    }
    clauses = reversed;

    /* __type__ clause: ((eq? __msg__ '__type__) 'Name) */
    sexp type_test = sexp_list3(ctx, ps_intern(ctx, "eq?"), msg_sym,
        sexp_list2(ctx, ps_intern(ctx, "quote"), ps_intern(ctx, "__type__")));
    clauses = ps_append(ctx, clauses,
        sexp_list2(ctx, type_test, quoted_name));

    /* else clause: (else (error "unknown field" __msg__)) */
    clauses = ps_append(ctx, clauses,
        sexp_list2(ctx, ps_intern(ctx, "else"),
            sexp_list3(ctx, ps_intern(ctx, "error"),
                       sexp_c_string(ctx, "unknown field", -1), msg_sym)));

    /* cond form */
    sexp cond_form = sexp_cons(ctx, ps_intern(ctx, "cond"), clauses);

    /* dispatch lambda: (lambda (__msg__) cond_form) */
    sexp lambda_form = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                                  sexp_list1(ctx, msg_sym), cond_form);

    /* vector creation: (vector 'Name f1 f2 ...) */
    sexp vec_call = sexp_cons(ctx, ps_intern(ctx, "vector"),
                              sexp_cons(ctx, quoted_name, fields));

    /* let binding: (let ((__rec_vec__ vec_call)) lambda_form) */
    sexp let_form = sexp_list3(ctx, ps_intern(ctx, "let"),
        sexp_list1(ctx, sexp_list2(ctx, vec_sym, vec_call)),
        lambda_form);

    /* constructor: (define (Name f1 f2 ...) let_form) */
    sexp ctor_def = sexp_list3(ctx, ps_intern(ctx, "define"),
                               sexp_cons(ctx, name, fields), let_form);

    /* --- Predicate: (define (Name? v__) (and (procedure? v__) (protect ...))) --- */
    snprintf(buf, sizeof(buf), "%s?", nstr);
    sexp pred_sym = ps_intern(ctx, buf);
    sexp v_sym = ps_intern(ctx, "v__");
    sexp e_sym = ps_intern(ctx, "e__");

    sexp proc_check = sexp_list2(ctx, ps_intern(ctx, "procedure?"), v_sym);
    sexp type_call = sexp_list2(ctx, v_sym,
        sexp_list2(ctx, ps_intern(ctx, "quote"), ps_intern(ctx, "__type__")));
    sexp eq_check = sexp_list3(ctx, ps_intern(ctx, "eq?"), type_call, quoted_name);
    sexp protect_form = sexp_list3(ctx, ps_intern(ctx, "protect"),
        sexp_list2(ctx, e_sym,
            sexp_list2(ctx, ps_intern(ctx, "else"), SEXP_FALSE)),
        eq_check);
    sexp and_form = sexp_list3(ctx, ps_intern(ctx, "and"), proc_check, protect_form);
    sexp pred_def = sexp_list3(ctx, ps_intern(ctx, "define"),
                               sexp_list2(ctx, pred_sym, v_sym), and_form);

    /* (begin ctor_def pred_def) */
    return sexp_cons(ctx, ps_intern(ctx, "begin"),
                     sexp_list2(ctx, ctor_def, pred_def));
}

/* Build a begin form: if a is already a begin, extend it; otherwise create new */
sexp ps_sexp_begin(sexp ctx, sexp a, sexp b) {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    if (sexp_pairp(a) && sexp_car(a) == ps_intern(ctx, "begin")) {
        /* Append b to existing begin */
        sexp p = a;
        while (sexp_pairp(sexp_cdr(p)) && sexp_pairp(sexp_cdr(sexp_cdr(p))))
            p = sexp_cdr(p);
        /* Now p points to second-to-last element */
        if (sexp_pairp(sexp_cdr(p)))
            p = sexp_cdr(p);
        sexp_cdr(p) = sexp_cons(ctx, b, SEXP_NULL);
        result = a;
    } else {
        result = sexp_list3(ctx, ps_intern(ctx, "begin"), a, b);
    }

    sexp_gc_release1(ctx);
    return result;
}

/* Append element to end of list */
sexp ps_append(sexp ctx, sexp list, sexp elem) {
    sexp_gc_var1(new_elem);
    sexp_gc_preserve1(ctx, new_elem);
    new_elem = sexp_cons(ctx, elem, SEXP_NULL);

    if (sexp_nullp(list)) {
        sexp_gc_release1(ctx);
        return new_elem;
    }

    sexp p = list;
    while (sexp_pairp(sexp_cdr(p)))
        p = sexp_cdr(p);
    sexp_cdr(p) = new_elem;

    sexp_gc_release1(ctx);
    return list;
}

/* Build a sexp identifier from token text */
sexp ps_make_ident(sexp ctx, const char *start, int length) {
    return sexp_intern(ctx, start, length);
}

/* Quote a symbol: (quote sym) */
sexp ps_sexp_quote(sexp ctx, sexp sym) {
    return sexp_list2(ctx, ps_intern(ctx, "quote"), sym);
}

/* Make a string sexp from token text (handles escape sequences) */
sexp ps_make_string(sexp ctx, const char *start, int length) {
    /* Allocate buffer for unescaped string */
    char *buf = (char *)malloc(length + 1);
    if (!buf) return sexp_c_string(ctx, start, length);

    int j = 0;
    for (int i = 0; i < length; i++) {
        if (start[i] == '\\' && i + 1 < length) {
            i++;
            switch (start[i]) {
            case 'n': buf[j++] = '\n'; break;
            case 't': buf[j++] = '\t'; break;
            case 'r': buf[j++] = '\r'; break;
            case '\\': buf[j++] = '\\'; break;
            case '"': buf[j++] = '"'; break;
            case '0': buf[j++] = '\0'; break;
            default: buf[j++] = start[i]; break;
            }
        } else {
            buf[j++] = start[i];
        }
    }
    buf[j] = '\0';

    sexp result = sexp_c_string(ctx, buf, j);
    free(buf);
    return result;
}

/* Build a cond clause: (test expr) */
sexp ps_make_cond_clause(sexp ctx, sexp test, sexp expr) {
    return sexp_list2(ctx, test, expr);
}

/* Build a case clause: ((datums...) expr) */
sexp ps_make_case_clause(sexp ctx, sexp datums, sexp expr) {
    return sexp_list2(ctx, datums, expr);
}

/* include("a.eval", "b.eval") →
   (begin (eval-include "a.eval") (eval-include "b.eval"))
   Single file: just (eval-include "a.eval") */
sexp ps_make_include(sexp ctx, sexp string_list) {
    sexp inc_sym = ps_intern(ctx, "eval-include");

    /* Single file — no begin wrapper needed */
    if (sexp_pairp(string_list) && sexp_nullp(sexp_cdr(string_list)))
        return sexp_list2(ctx, inc_sym, sexp_car(string_list));

    /* Multiple files — wrap in begin */
    sexp_gc_var2(result, cur);
    sexp_gc_preserve2(ctx, result, cur);
    result = SEXP_NULL;
    for (cur = string_list; sexp_pairp(cur); cur = sexp_cdr(cur)) {
        sexp call = sexp_list2(ctx, inc_sym, sexp_car(cur));
        result = sexp_cons(ctx, call, result);
    }
    result = sexp_cons(ctx, ps_intern(ctx, "begin"), sexp_nreverse(ctx, result));
    sexp_gc_release2(ctx);
    return result;
}

/* dict(k1: v1, k2: v2) → (__make_eval_dict__ (list (cons 'k1 v1) (cons 'k2 v2)))
   dict() → (__make_eval_dict__ '()) */
sexp ps_make_dict(sexp ctx, sexp entries) {
    sexp dict_fn = ps_intern(ctx, "__make_eval_dict__");

    if (sexp_nullp(entries) || entries == SEXP_NULL) {
        return sexp_list2(ctx, dict_fn,
            sexp_list2(ctx, ps_intern(ctx, "quote"), SEXP_NULL));
    }

    /* Build (list (cons 'k1 v1) (cons 'k2 v2) ...) */
    sexp items = SEXP_NULL;
    sexp p = entries;
    while (sexp_pairp(p)) {
        sexp entry = sexp_car(p);
        sexp key = sexp_car(entry);    /* symbol or expr */
        sexp val = sexp_cdr(entry);    /* expr */
        sexp key_form;
        if (sexp_symbolp(key)) {
            key_form = sexp_list2(ctx, ps_intern(ctx, "quote"), key);
        } else {
            key_form = key;
        }
        sexp cons_form = sexp_list3(ctx, ps_intern(ctx, "cons"), key_form, val);
        items = sexp_cons(ctx, cons_form, items);
        p = sexp_cdr(p);
    }

    /* Reverse to preserve order */
    sexp reversed = SEXP_NULL;
    while (sexp_pairp(items)) {
        reversed = sexp_cons(ctx, sexp_car(items), reversed);
        items = sexp_cdr(items);
    }

    sexp list_form = sexp_cons(ctx, ps_intern(ctx, "list"), reversed);
    return sexp_list2(ctx, dict_fn, list_form);
}

/* Build dict comprehension: wraps list comprehension result in __make_eval_dict__ */
sexp ps_make_dict_comp(sexp ctx, sexp body, sexp clauses) {
    sexp list_expr = ps_make_list_comp(ctx, body, clauses);
    return sexp_list2(ctx, ps_intern(ctx, "__make_eval_dict__"), list_expr);
}

/* Build (define-library (name...) body-forms...)
   The body is a begin form; unwrap it into separate library declarations. */
sexp ps_make_library(sexp ctx, sexp name, sexp body) {
    sexp lib_sym = ps_intern(ctx, "define-library");
    sexp result = sexp_list2(ctx, lib_sym, name);
    /* Unwrap (begin form1 form2 ...) into (define-library (name) form1 form2 ...) */
    if (sexp_pairp(body) && sexp_car(body) == ps_intern(ctx, "begin")) {
        sexp p = sexp_cdr(body);
        sexp tail = result;
        while (sexp_pairp(tail) && sexp_pairp(sexp_cdr(tail)))
            tail = sexp_cdr(tail);
        sexp_cdr(tail) = p;
    } else {
        result = sexp_list3(ctx, lib_sym, name, body);
    }
    return result;
}

/* Make a begin-with-defines safe for expression context.
   Handles interleaved defines by nesting letrec forms to preserve order
   and allow mutual recursion:
     (begin e1 (define x v1) e2 (define y v2) e3)
   → (begin e1 (letrec ((x v1)) (begin e2 (letrec ((y v2)) e3))))
   If the expression is not a begin with defines, returns it unchanged. */
sexp ps_expr_safe(sexp ctx, sexp expr) {
    if (!sexp_pairp(expr))
        return expr;

    /* Bare (define var val) in expression context → (set! var val) */
    if (sexp_car(expr) == ps_intern(ctx, "define"))  {
        sexp_car(expr) = ps_intern(ctx, "set!");
        return expr;
    }

    /* Only transform (begin ...) forms below */
    if (sexp_car(expr) != ps_intern(ctx, "begin"))
        return expr;

    sexp with_stmt_sym = ps_intern(ctx, "__with_stmt__");
    sexp begin_sym = ps_intern(ctx, "begin");

    /* First pass: process __with_stmt__ markers from right to left.
       Each with_stmt wraps everything after it in let + dynamic-wind. */
    {
        /* Collect stmts into an array for right-to-left processing */
        sexp stmts[256];
        int nstmts = 0;
        sexp p = sexp_cdr(expr);
        while (sexp_pairp(p) && nstmts < 256) {
            stmts[nstmts++] = sexp_car(p);
            p = sexp_cdr(p);
        }

        /* Scan right-to-left for __with_stmt__ markers */
        int has_with = 0;
        for (int i = 0; i < nstmts; i++) {
            if (sexp_pairp(stmts[i]) && sexp_car(stmts[i]) == with_stmt_sym) {
                has_with = 1;
                break;
            }
        }

        if (has_with) {
            /* Process from the last with_stmt backward */
            /* Build the tail (everything after the last statement) */
            sexp tail = SEXP_VOID;

            for (int i = nstmts - 1; i >= 0; i--) {
                if (sexp_pairp(stmts[i]) && sexp_car(stmts[i]) == with_stmt_sym) {
                    /* This is a with_stmt — wrap tail in let+dynamic-wind */
                    sexp bindings = sexp_cadr(stmts[i]);  /* the bindings list */

                    /* Build body from stmts[i+1..end] if tail is VOID,
                       otherwise tail already includes them */
                    sexp body = tail;
                    if (body == SEXP_VOID) {
                        /* No statements after this with — body is void */
                        body = SEXP_VOID;
                    }

                    /* Wrap body with with-bindings (reuse ps_make_with) */
                    tail = ps_make_with(ctx, bindings, body);
                } else {
                    /* Regular statement — prepend to tail as (begin stmt tail) */
                    if (tail == SEXP_VOID) {
                        tail = stmts[i];
                    } else {
                        tail = sexp_list3(ctx, begin_sym, stmts[i], tail);
                    }
                }
            }

            /* Recurse to handle defines in the result */
            if (sexp_pairp(tail) && sexp_car(tail) == begin_sym)
                return ps_expr_safe(ctx, tail);
            return tail;
        }
    }

    sexp define_sym = ps_intern(ctx, "define");
    sexp letstar_sym = ps_intern(ctx, "letrec");

    /* Check if any element is a define */
    int has_define = 0;
    sexp p = sexp_cdr(expr);
    while (sexp_pairp(p)) {
        sexp e = sexp_car(p);
        if (sexp_pairp(e) && sexp_car(e) == define_sym) {
            has_define = 1;
            break;
        }
        p = sexp_cdr(p);
    }
    if (!has_define)
        return expr;

    sexp_gc_var5(prefix, bindings, binding, rest_expr, result);
    sexp_gc_preserve5(ctx, prefix, bindings, binding, rest_expr, result);

    p = sexp_cdr(expr);

    /* Collect non-define prefix expressions */
    prefix = SEXP_NULL;
    sexp prefix_tail = SEXP_NULL;
    while (sexp_pairp(p)) {
        sexp e = sexp_car(p);
        if (sexp_pairp(e) && sexp_car(e) == define_sym
            && sexp_pairp(sexp_cdr(e)) && sexp_pairp(sexp_cdr(sexp_cdr(e))))
            break;
        sexp cell = sexp_cons(ctx, e, SEXP_NULL);
        if (sexp_nullp(prefix)) {
            prefix = cell;
            prefix_tail = cell;
        } else {
            sexp_cdr(prefix_tail) = cell;
            prefix_tail = cell;
        }
        p = sexp_cdr(p);
    }

    /* Collect consecutive defines as let* bindings */
    bindings = SEXP_NULL;
    sexp bindings_tail = SEXP_NULL;
    while (sexp_pairp(p)) {
        sexp e = sexp_car(p);
        if (!(sexp_pairp(e) && sexp_car(e) == define_sym
              && sexp_pairp(sexp_cdr(e)) && sexp_pairp(sexp_cdr(sexp_cdr(e)))))
            break;
        sexp name = sexp_car(sexp_cdr(e));
        sexp val = sexp_car(sexp_cdr(sexp_cdr(e)));
        binding = sexp_list2(ctx, name, val);
        sexp cell = sexp_cons(ctx, binding, SEXP_NULL);
        if (sexp_nullp(bindings)) {
            bindings = cell;
            bindings_tail = cell;
        } else {
            sexp_cdr(bindings_tail) = cell;
            bindings_tail = cell;
        }
        p = sexp_cdr(p);
    }

    /* Build rest body — recurse for any further defines */
    if (!sexp_pairp(p)) {
        rest_expr = SEXP_VOID;
    } else if (sexp_nullp(sexp_cdr(p))) {
        rest_expr = sexp_car(p);
    } else {
        rest_expr = sexp_cons(ctx, begin_sym, p);
        rest_expr = ps_expr_safe(ctx, rest_expr);
    }

    /* Build (let* (bindings...) rest_expr) */
    result = sexp_list3(ctx, letstar_sym, bindings, rest_expr);

    /* If there's a prefix, wrap as (begin prefix... let_form) */
    if (!sexp_nullp(prefix)) {
        sexp_cdr(prefix_tail) = sexp_cons(ctx, result, SEXP_NULL);
        result = sexp_cons(ctx, begin_sym, prefix);
    }

    sexp_gc_release5(ctx);
    return result;
}

/* Build list comprehension:
   [body for x in xs]            → (__comp_map__ (lambda (x) body) xs)
   [body for x in xs if p]       → (__comp_map__ (lambda (x) body) (__comp_filter__ (lambda (x) p) xs))
   [body for x in xs for y in ys] → (__comp_append_map__ (lambda (x) (__comp_map__ ...)) xs)
   Uses polymorphic helpers that accept both list and generator sources.
   Clauses list: each element is (var . iter) for for-clauses, (#f . cond) for if-clauses. */
sexp ps_make_list_comp(sexp ctx, sexp body, sexp clauses) {
    /* Step 1: Attach if-conditions to preceding for-clause iterators
       via filter wrapping. Build list of (var . effective_iter) pairs. */
    sexp for_groups = SEXP_NULL;
    sexp current_var = SEXP_FALSE, current_iter = SEXP_FALSE;
    sexp p = clauses;
    while (sexp_pairp(p)) {
        sexp clause = sexp_car(p);
        if (sexp_car(clause) != SEXP_FALSE) {
            /* for-clause: push previous group, start new */
            if (current_var != SEXP_FALSE)
                for_groups = sexp_cons(ctx,
                    sexp_cons(ctx, current_var, current_iter), for_groups);
            current_var = sexp_car(clause);
            current_iter = sexp_cdr(clause);
        } else {
            /* if-clause: wrap current_iter in __comp_filter__ */
            sexp lam = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                           sexp_list1(ctx, current_var), sexp_cdr(clause));
            current_iter = sexp_list3(ctx, ps_intern(ctx, "__comp_filter__"),
                               lam, current_iter);
        }
        p = sexp_cdr(p);
    }
    if (current_var != SEXP_FALSE)
        for_groups = sexp_cons(ctx,
            sexp_cons(ctx, current_var, current_iter), for_groups);

    /* for_groups is reversed: innermost first.
       Step 2: Build nested __comp_map__/__comp_append_map__ from innermost to outermost. */
    sexp result = body;
    int first = 1;
    p = for_groups;
    while (sexp_pairp(p)) {
        sexp var = sexp_car(sexp_car(p));
        sexp iter = sexp_cdr(sexp_car(p));
        sexp lam = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                       sexp_list1(ctx, var), result);
        if (first) {
            result = sexp_list3(ctx, ps_intern(ctx, "__comp_map__"), lam, iter);
            first = 0;
        } else {
            result = sexp_list3(ctx, ps_intern(ctx, "__comp_append_map__"), lam, iter);
        }
        p = sexp_cdr(p);
    }
    return result;
}

/* Build generator function:
   (lambda (params) (make-coroutine-generator (lambda (__yield__)
     (call-with-current-continuation (lambda (__return__) body))))) */
sexp ps_make_generator(sexp ctx, sexp params, sexp body) {
    sexp yield_sym = ps_intern(ctx, "__yield__");
    sexp return_sym = ps_intern(ctx, "__return__");
    /* (call-with-current-continuation (lambda (__return__) body)) */
    sexp ret_lambda = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                          sexp_list1(ctx, return_sym), body);
    sexp callcc = sexp_list2(ctx, ps_intern(ctx, "call-with-current-continuation"),
                             ret_lambda);
    /* (make-coroutine-generator (lambda (__yield__) callcc)) */
    sexp yield_lambda = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                            sexp_list1(ctx, yield_sym), callcc);
    sexp gen = sexp_list2(ctx, ps_intern(ctx, "make-coroutine-generator"),
                          yield_lambda);
    return sexp_list3(ctx, ps_intern(ctx, "lambda"), params, gen);
}

/* Build generator function with rest params */
sexp ps_make_generator_rest(sexp ctx, sexp params, sexp rest, sexp body) {
    sexp dotted;
    if (sexp_nullp(params)) {
        dotted = rest;
    } else {
        dotted = params;
        sexp p = params;
        while (!sexp_nullp(sexp_cdr(p)) && sexp_pairp(sexp_cdr(p)))
            p = sexp_cdr(p);
        sexp_cdr(p) = rest;
    }
    sexp yield_sym = ps_intern(ctx, "__yield__");
    sexp return_sym = ps_intern(ctx, "__return__");
    sexp ret_lambda = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                          sexp_list1(ctx, return_sym), body);
    sexp callcc = sexp_list2(ctx, ps_intern(ctx, "call-with-current-continuation"),
                             ret_lambda);
    sexp yield_lambda = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                            sexp_list1(ctx, yield_sym), callcc);
    sexp gen = sexp_list2(ctx, ps_intern(ctx, "make-coroutine-generator"),
                          yield_lambda);
    return sexp_list3(ctx, ps_intern(ctx, "lambda"), dotted, gen);
}

/* Build generator comprehension:
   (make-coroutine-generator (lambda (__yield__)
     (__gen_for_each__ (lambda (x) (__yield__ body)) xs))) */
sexp ps_make_gen_comp(sexp ctx, sexp body, sexp clauses) {
    sexp yield_sym = ps_intern(ctx, "__yield__");
    sexp gfe = ps_intern(ctx, "__gen_for_each__");

    /* Collect clauses into array for reverse processing */
    sexp arr[64];
    int n = 0;
    sexp p = clauses;
    while (sexp_pairp(p) && n < 64) {
        arr[n++] = sexp_car(p);
        p = sexp_cdr(p);
    }

    /* Build from innermost to outermost */
    sexp result = sexp_list2(ctx, yield_sym, body);  /* (__yield__ body) */
    for (int i = n - 1; i >= 0; i--) {
        sexp clause = arr[i];
        if (sexp_car(clause) == SEXP_FALSE) {
            /* if-clause: (if cond result) */
            result = sexp_list3(ctx, ps_intern(ctx, "if"), sexp_cdr(clause), result);
        } else {
            /* for-clause: (__gen_for_each__ (lambda (var) result) iter) */
            sexp lam = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                           sexp_list1(ctx, sexp_car(clause)), result);
            result = sexp_list3(ctx, gfe, lam, sexp_cdr(clause));
        }
    }
    /* (make-coroutine-generator (lambda (__yield__) result)) */
    sexp inner = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                     sexp_list1(ctx, yield_sym), result);
    return sexp_list2(ctx, ps_intern(ctx, "make-coroutine-generator"), inner);
}

/* F-string text: like ps_make_string but also folds {{ → { and }} → } */
sexp ps_make_fstr_text(sexp ctx, const char *start, int length) {
    char *buf = (char *)malloc(length + 1);
    if (!buf) return sexp_c_string(ctx, start, length);

    int j = 0;
    for (int i = 0; i < length; i++) {
        if (start[i] == '{' && i + 1 < length && start[i + 1] == '{') {
            buf[j++] = '{';
            i++; /* skip second { */
        } else if (start[i] == '}' && i + 1 < length && start[i + 1] == '}') {
            buf[j++] = '}';
            i++; /* skip second } */
        } else if (start[i] == '\\' && i + 1 < length) {
            i++;
            switch (start[i]) {
            case 'n': buf[j++] = '\n'; break;
            case 't': buf[j++] = '\t'; break;
            case 'r': buf[j++] = '\r'; break;
            case '\\': buf[j++] = '\\'; break;
            case '"': buf[j++] = '"'; break;
            case '0': buf[j++] = '\0'; break;
            default: buf[j++] = start[i]; break;
            }
        } else {
            buf[j++] = start[i];
        }
    }
    buf[j] = '\0';

    sexp result = sexp_c_string(ctx, buf, j);
    free(buf);
    return result;
}

/* Build f-string: (string-append "start_text" (__tostr__ e1) "mid_text" (__tostr__ e2) ... "end_text")
   body is a list where odd elements are wrapped exprs and even elements are mid-text strings.
   The body list structure from the parser is:
     single expr:   (e1)
     with mids:     (e1 mid_text1 e2 mid_text2 e3 ...) */
sexp ps_fstr_build(sexp ctx, const char *s_start, int s_len,
                   sexp body,
                   const char *e_start, int e_len) {
    sexp tostr_sym = ps_intern(ctx, "__tostr__");
    sexp sa_sym = ps_intern(ctx, "string-append");

    /* Collect all parts into a list */
    sexp parts = SEXP_NULL;
    sexp parts_tail = SEXP_NULL;

    /* Helper to append a part */
    #define FSTR_APPEND(val) do { \
        sexp _cell = sexp_cons(ctx, (val), SEXP_NULL); \
        if (sexp_nullp(parts)) { parts = _cell; parts_tail = _cell; } \
        else { sexp_cdr(parts_tail) = _cell; parts_tail = _cell; } \
    } while(0)

    /* Start text */
    if (s_len > 0) {
        sexp s = ps_make_fstr_text(ctx, s_start, s_len);
        FSTR_APPEND(s);
    }

    /* Process body list: exprs are wrapped with __tostr__, strings are mid-text */
    sexp p = body;
    while (sexp_pairp(p)) {
        sexp elem = sexp_car(p);
        if (sexp_stringp(elem)) {
            /* Mid-text segment (only if non-empty) */
            if (sexp_string_size(elem) > 0)
                FSTR_APPEND(elem);
        } else {
            /* Expression: wrap with __tostr__ */
            sexp wrapped = sexp_list2(ctx, tostr_sym, elem);
            FSTR_APPEND(wrapped);
        }
        p = sexp_cdr(p);
    }

    /* End text */
    if (e_len > 0) {
        sexp s = ps_make_fstr_text(ctx, e_start, e_len);
        FSTR_APPEND(s);
    }

    #undef FSTR_APPEND

    /* If only one part, return it directly */
    if (sexp_pairp(parts) && sexp_nullp(sexp_cdr(parts)))
        return sexp_car(parts);

    /* If no parts at all (shouldn't happen), return empty string */
    if (sexp_nullp(parts))
        return sexp_c_string(ctx, "", 0);

    /* (string-append part1 part2 ...) */
    return sexp_cons(ctx, sa_sym, parts);
}

/* Append mid-text + wrapped expr to f-string body list.
   L is the existing body list, M is the mid token, E is the new expr. */
sexp ps_fstr_mid(sexp ctx, sexp list, const char *m_start, int m_len, sexp expr) {
    sexp result = list;
    /* Append mid-text (as a string sexp) */
    if (m_len > 0) {
        sexp mid_str = ps_make_fstr_text(ctx, m_start, m_len);
        result = ps_append(ctx, result, mid_str);
    }
    /* Append the expression */
    result = ps_append(ctx, result, expr);
    return result;
}

/* Build (ref obj index) */
sexp ps_make_ref(sexp ctx, sexp obj, sexp index) {
    return sexp_list3(ctx, ps_intern(ctx, "ref"), obj, index);
}

/* Build (slice obj start end) */
sexp ps_make_slice(sexp ctx, sexp obj, sexp start, sexp end) {
    return sexp_cons(ctx, ps_intern(ctx, "slice"),
               sexp_cons(ctx, obj,
                   sexp_cons(ctx, start,
                       sexp_cons(ctx, end, SEXP_NULL))));
}

/* ===== Logic programming helpers ===== */

/* Helper: build (conj g1 (conj g2 (conj g3 ...))) from list */
static sexp ps_logic_conj_chain(sexp ctx, sexp goals) {
    if (!sexp_pairp(goals)) return goals;
    if (!sexp_pairp(sexp_cdr(goals))) return sexp_car(goals);
    return sexp_list3(ctx, ps_intern(ctx, "conj"),
                      sexp_car(goals),
                      ps_logic_conj_chain(ctx, sexp_cdr(goals)));
}

/* fresh(?x, ?y) { g1, g2 }
   -> (call-fresh (lambda (x) (call-fresh (lambda (y) (conj g1 g2))))) */
sexp ps_make_fresh(sexp ctx, sexp vars, sexp goals) {
    sexp body = ps_logic_conj_chain(ctx, goals);
    /* Reverse vars to wrap inside-out */
    sexp rev = SEXP_NULL;
    for (sexp p = vars; sexp_pairp(p); p = sexp_cdr(p))
        rev = sexp_cons(ctx, sexp_car(p), rev);
    sexp result = body;
    for (sexp p = rev; sexp_pairp(p); p = sexp_cdr(p)) {
        sexp lam = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                              sexp_list1(ctx, sexp_car(p)), result);
        result = sexp_list2(ctx, ps_intern(ctx, "call-fresh"), lam);
    }
    return result;
}

/* run(n, ?q) { g1, g2 }
   -> (logic_run n (lambda (q) (conj g1 g2)))
   n = SEXP_FALSE means all; also handle * symbol (from OPVAL) as "all" */
sexp ps_make_run(sexp ctx, sexp n, const char *qname, int qlen, sexp goals) {
    /* run(*, ?q) -- * in value position becomes the symbol '*' via OPVAL */
    if (sexp_symbolp(n)) {
        const char *s = sexp_string_data(sexp_symbol_to_string(ctx, n));
        if (s && s[0] == '*' && s[1] == '\0')
            n = SEXP_FALSE;
    }
    sexp q = ps_make_ident(ctx, qname, qlen);
    sexp body = ps_logic_conj_chain(ctx, goals);
    sexp lam = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                          sexp_list1(ctx, q), body);
    return sexp_list3(ctx, ps_intern(ctx, "logic_run"), n, lam);
}

/* conde { {g1,g2}, {g3,g4} }
   -> (logic_conde (list g1 g2) (list g3 g4)) */
sexp ps_make_conde(sexp ctx, sexp clauses) {
    sexp result = SEXP_NULL;
    for (sexp p = clauses; sexp_pairp(p); p = sexp_cdr(p)) {
        sexp clause = sexp_car(p);
        sexp wrapped = sexp_cons(ctx, ps_intern(ctx, "list"), clause);
        result = sexp_pairp(result) ? ps_append(ctx, result, wrapped)
                                    : sexp_list1(ctx, wrapped);
    }
    return sexp_cons(ctx, ps_intern(ctx, "logic_conde"), result);
}

/* fact parent("tom", "bob");
   -> (begin (logic_assert_fact 'parent "tom" "bob")
             (define parent (lambda __args__ (logic_query_rel 'parent __args__)))) */
sexp ps_make_fact(sexp ctx, const char *name, int nlen, sexp args) {
    sexp sym = sexp_list2(ctx, ps_intern(ctx, "quote"),
                          ps_make_ident(ctx, name, nlen));
    sexp assert_call = sexp_cons(ctx, ps_intern(ctx, "logic_assert_fact"),
                                 sexp_cons(ctx, sym, args));
    sexp ident = ps_make_ident(ctx, name, nlen);
    sexp args_sym = ps_intern(ctx, "__args__");
    sexp rel_call = sexp_list3(ctx, ps_intern(ctx, "logic_query_rel"),
                               sym, args_sym);
    sexp lam = sexp_list3(ctx, ps_intern(ctx, "lambda"), args_sym, rel_call);
    sexp def = sexp_list3(ctx, ps_intern(ctx, "define"), ident, lam);
    return sexp_list3(ctx, ps_intern(ctx, "begin"), assert_call, def);
}


/* rule ancestor(?x, ?y) :- parent(?x, ?y);
   -> (begin (logic_assert_rule 'ancestor 2 (lambda (x y) (parent x y)))
             (define ancestor (lambda __args__ (logic_query_rel 'ancestor __args__))))
   For extra vars not in head, use fresh() in body:
     rule ancestor(?x, ?y) :- fresh(?z) { parent(?x, ?z), ancestor(?z, ?y) }; */
sexp ps_make_rule(sexp ctx, const char *name, int nlen, sexp params, sexp goals) {
    sexp sym = sexp_list2(ctx, ps_intern(ctx, "quote"),
                          ps_make_ident(ctx, name, nlen));
    int arity = 0;
    for (sexp p = params; sexp_pairp(p); p = sexp_cdr(p)) arity++;

    sexp body = ps_logic_conj_chain(ctx, goals);
    sexp lam = sexp_list3(ctx, ps_intern(ctx, "lambda"), params, body);
    sexp assert_call = sexp_cons(ctx, ps_intern(ctx, "logic_assert_rule"),
             sexp_cons(ctx, sym,
               sexp_cons(ctx, sexp_make_fixnum(arity),
                 sexp_cons(ctx, lam, SEXP_NULL))));
    sexp ident = ps_make_ident(ctx, name, nlen);
    sexp args_sym = ps_intern(ctx, "__args__");
    sexp rel_call = sexp_list3(ctx, ps_intern(ctx, "logic_query_rel"),
                               sym, args_sym);
    sexp def_lam = sexp_list3(ctx, ps_intern(ctx, "lambda"), args_sym, rel_call);
    sexp def = sexp_list3(ctx, ps_intern(ctx, "define"), ident, def_lam);
    return sexp_list3(ctx, ps_intern(ctx, "begin"), assert_call, def);
}

/* Helper: check if sexp is (quote sym) form */
static int is_quoted_sym(sexp x) {
    return sexp_pairp(x) && sexp_symbolp(sexp_car(x)) &&
           sexp_pairp(sexp_cdr(x)) && sexp_symbolp(sexp_cadr(x)) &&
           !sexp_pairp(sexp_cddr(x));
}

/* query parent(?x, "bob")
   Compiles to: run(*, ?__q__) { fresh(?x) { parent(?x, "bob"), ?__q__ === ?x } }
   Or with multiple free vars:
   query parent(?x, ?y) → run(*, ?__q__) { fresh(?x, ?y) { parent(?x, ?y), ?__q__ === [?x, ?y] } } */
sexp ps_make_query(sexp ctx, const char *name, int nlen, sexp args) {
    /* Collect free vars (quoted symbols) and build call args */
    sexp free_vars = SEXP_NULL;
    sexp call_args = SEXP_NULL;
    for (sexp p = args; sexp_pairp(p); p = sexp_cdr(p)) {
        sexp a = sexp_car(p);
        if (is_quoted_sym(a)) {
            sexp var_sym = sexp_cadr(a);
            free_vars = ps_append(ctx, sexp_pairp(free_vars) ? free_vars : SEXP_NULL, var_sym);
            call_args = ps_append(ctx, sexp_pairp(call_args) ? call_args : SEXP_NULL, var_sym);
        } else {
            call_args = ps_append(ctx, sexp_pairp(call_args) ? call_args : SEXP_NULL, a);
        }
    }
    if (!sexp_pairp(free_vars)) free_vars = SEXP_NULL;
    if (!sexp_pairp(call_args)) call_args = SEXP_NULL;

    sexp rel_name = ps_make_ident(ctx, name, nlen);
    /* Build: (rel_name arg1 arg2 ...) — function call as goal */
    sexp rel_call = sexp_cons(ctx, rel_name, call_args);

    sexp q = ps_intern(ctx, "__q__");
    /* Build result: if one free var, q === var; if multiple, q === [vars] */
    sexp result_expr;
    if (sexp_pairp(free_vars) && !sexp_pairp(sexp_cdr(free_vars))) {
        result_expr = sexp_car(free_vars);
    } else {
        result_expr = sexp_cons(ctx, ps_intern(ctx, "list"), free_vars);
    }
    sexp eq_goal = sexp_list3(ctx, ps_intern(ctx, "logic_eq"), q, result_expr);
    sexp body = sexp_list3(ctx, ps_intern(ctx, "conj"), rel_call, eq_goal);

    /* Wrap in fresh for free vars */
    if (sexp_pairp(free_vars)) {
        body = ps_make_fresh(ctx, free_vars, sexp_list1(ctx, body));
    }

    /* Wrap in logic_run */
    sexp lam = sexp_list3(ctx, ps_intern(ctx, "lambda"), sexp_list1(ctx, q), body);
    return sexp_list3(ctx, ps_intern(ctx, "logic_run"), SEXP_FALSE, lam);
}

/* findall(?x, parent(?x, "bob"))
   Compiles to: run(*, ?x) { parent(?x, "bob") }
   The target var ?x must appear in the relation args */
sexp ps_make_findall(sexp ctx, const char *vname, int vlen,
                     const char *rname, int rlen, sexp args) {
    sexp target = ps_make_ident(ctx, vname, vlen);

    /* Collect all free vars from args */
    sexp free_vars = SEXP_NULL;
    sexp call_args = SEXP_NULL;
    for (sexp p = args; sexp_pairp(p); p = sexp_cdr(p)) {
        sexp a = sexp_car(p);
        if (is_quoted_sym(a)) {
            sexp var_sym = sexp_cadr(a);
            free_vars = ps_append(ctx, sexp_pairp(free_vars) ? free_vars : SEXP_NULL, var_sym);
            call_args = ps_append(ctx, sexp_pairp(call_args) ? call_args : SEXP_NULL, var_sym);
        } else {
            call_args = ps_append(ctx, sexp_pairp(call_args) ? call_args : SEXP_NULL, a);
        }
    }
    if (!sexp_pairp(free_vars)) free_vars = SEXP_NULL;
    if (!sexp_pairp(call_args)) call_args = SEXP_NULL;

    sexp rel_name = ps_make_ident(ctx, rname, rlen);
    sexp rel_call = sexp_cons(ctx, rel_name, call_args);

    /* result = target var, so q === target_var */
    sexp q = ps_intern(ctx, "__q__");
    sexp eq_goal = sexp_list3(ctx, ps_intern(ctx, "logic_eq"), q, target);
    sexp body = sexp_list3(ctx, ps_intern(ctx, "conj"), rel_call, eq_goal);

    /* Remove target from free_vars for fresh (target is q) */
    sexp other_vars = SEXP_NULL;
    for (sexp p = free_vars; sexp_pairp(p); p = sexp_cdr(p)) {
        if (sexp_car(p) != target)
            other_vars = ps_append(ctx, sexp_pairp(other_vars) ? other_vars : SEXP_NULL, sexp_car(p));
    }

    /* Wrap in fresh for non-target free vars */
    if (sexp_pairp(other_vars)) {
        body = ps_make_fresh(ctx, other_vars, sexp_list1(ctx, body));
    }

    /* For findall, target IS q */
    sexp lam = sexp_list3(ctx, ps_intern(ctx, "lambda"), sexp_list1(ctx, target), body);
    return sexp_list3(ctx, ps_intern(ctx, "logic_run"), SEXP_FALSE, lam);
}

/* guard(expr) -> (logic_guard (lambda () expr)) */
sexp ps_make_logic_guard(sexp ctx, sexp expr) {
    sexp thunk = sexp_list3(ctx, ps_intern(ctx, "lambda"), SEXP_NULL, expr);
    return sexp_list2(ctx, ps_intern(ctx, "logic_guard"), thunk);
}

/* ===== Rete (forward-chaining) helpers ===== */

/* Tag a pattern variable: ?x -> '__var_x (a quoted symbol) */
sexp ps_make_rete_var(sexp ctx, const char *name, int len) {
    char buf[256];
    snprintf(buf, sizeof(buf), "__var_%.*s", len, name);
    return sexp_intern(ctx, buf, -1);
}

/* Collect unique variable names from patterns.
 * Each pattern is (relation arg1 arg2 ...) where args may be __var_X symbols.
 * Returns a list of plain variable name symbols (x, y, z) in discovery order. */
static sexp rete_collect_vars(sexp ctx, sexp patterns) {
    sexp result = SEXP_NULL;
    for (sexp p = patterns; sexp_pairp(p); p = sexp_cdr(p)) {
        sexp pattern = sexp_car(p);
        /* skip relation name */
        for (sexp a = sexp_cdr(pattern); sexp_pairp(a); a = sexp_cdr(a)) {
            sexp arg = sexp_car(a);
            if (sexp_symbolp(arg)) {
                const char *s = sexp_string_data(sexp_symbol_to_string(ctx, arg));
                if (strncmp(s, "__var_", 6) == 0) {
                    const char *vname = s + 6;
                    sexp vsym = sexp_intern(ctx, vname, -1);
                    /* Check uniqueness */
                    int found = 0;
                    for (sexp r = result; sexp_pairp(r); r = sexp_cdr(r)) {
                        if (sexp_car(r) == vsym) { found = 1; break; }
                    }
                    if (!found) {
                        if (result == SEXP_NULL)
                            result = sexp_list1(ctx, vsym);
                        else
                            result = ps_append(ctx, result, vsym);
                    }
                }
            }
        }
    }
    return result;
}

/* Build the __rete_add_rule__ call from patterns + body.
 *
 * whenever parent(?x, ?y), parent(?y, ?z) { body }
 * ->
 * (__rete_add_rule__
 *   (list (list 'parent '__var_x '__var_y) (list 'parent '__var_y '__var_z))
 *   '(x y z)
 *   (lambda (x y z) body))
 */
sexp ps_make_whenever(sexp ctx, sexp patterns, sexp body) {
    /* 1. Collect unique variable names */
    sexp var_names = rete_collect_vars(ctx, patterns);

    /* 2. Build conditions list expression:
     *    (list (list 'relation '__var_x ...) ...) */
    sexp conds_elems = SEXP_NULL;
    for (sexp p = patterns; sexp_pairp(p); p = sexp_cdr(p)) {
        sexp pattern = sexp_car(p);  /* (relation arg1 arg2 ...) */
        /* Build (list 'relation arg1 arg2 ...) */
        sexp elems = SEXP_NULL;
        for (sexp a = pattern; sexp_pairp(a); a = sexp_cdr(a)) {
            sexp arg = sexp_car(a);
            sexp quoted;
            if (sexp_symbolp(arg)) {
                /* Quote the symbol */
                quoted = sexp_list2(ctx, ps_intern(ctx, "quote"), arg);
            } else {
                /* Constant expression: evaluate at rule-add time */
                quoted = arg;
            }
            if (elems == SEXP_NULL)
                elems = sexp_list1(ctx, quoted);
            else
                elems = ps_append(ctx, elems, quoted);
        }
        sexp list_call = sexp_cons(ctx, ps_intern(ctx, "list"), elems);

        if (conds_elems == SEXP_NULL)
            conds_elems = sexp_list1(ctx, list_call);
        else
            conds_elems = ps_append(ctx, conds_elems, list_call);
    }
    sexp conds_list = sexp_cons(ctx, ps_intern(ctx, "list"), conds_elems);

    /* 3. Build var_names quoted list: '(x y z) */
    sexp var_names_quoted = sexp_list2(ctx, ps_intern(ctx, "quote"), var_names);

    /* 4. Build lambda: (lambda (x y z) body) */
    sexp lam = sexp_list3(ctx, ps_intern(ctx, "lambda"), var_names, body);

    /* 5. Return: (__rete_add_rule__ conditions var-names lambda) */
    return sexp_cons(ctx, ps_intern(ctx, "__rete_add_rule__"),
               sexp_cons(ctx, conds_list,
                   sexp_cons(ctx, var_names_quoted,
                       sexp_cons(ctx, lam, SEXP_NULL))));
}

/* ===== Lemon parser wrapper ===== */

sexp eval_parse(sexp ctx, sexp env, const char *source,
                char **error_msg, int *error_line, int *error_col) {
    EvalLexer lexer;
    EvalToken token;
    EvalParserState state;

    memset(&state, 0, sizeof(state));
    state.ctx = ctx;
    state.env = env;
    state.result = SEXP_VOID;
    state.has_error = 0;
    state.error_msg = NULL;

    eval_lexer_init(&lexer, source);

    void *parser = ParseAlloc(malloc);
    if (!parser) {
        if (error_msg) *error_msg = strdup("Failed to allocate parser");
        if (error_line) *error_line = 0;
        if (error_col) *error_col = 0;
        return SEXP_VOID;
    }

    /* Feed tokens to the lemon parser */
    while (!lexer.has_error && !state.has_error) {
        if (eval_lexer_next(&lexer, &token) != 0) {
            /* Lexer error */
            if (error_msg) *error_msg = lexer.error_msg ? strdup(lexer.error_msg) : strdup("Lexer error");
            if (error_line) *error_line = lexer.line;
            if (error_col) *error_col = lexer.col;
            ParseFree(parser, free);
            eval_lexer_free(&lexer);
            return SEXP_VOID;
        }

        if (token.type == TOK_EOF) {
            /* Signal end-of-input to lemon (token code 0) */
            Parse(parser, 0, token, &state);
            break;
        }

        Parse(parser, token.type, token, &state);
    }

    ParseFree(parser, free);

    if (state.has_error) {
        if (error_msg) *error_msg = state.error_msg ? strdup(state.error_msg) : strdup("Parse error");
        if (error_line) *error_line = state.error_line;
        if (error_col) *error_col = state.error_col;
        if (state.error_msg) free(state.error_msg);
        eval_lexer_free(&lexer);
        return SEXP_VOID;
    }

    if (state.error_msg) free(state.error_msg);
    eval_lexer_free(&lexer);
    return state.result;
}
