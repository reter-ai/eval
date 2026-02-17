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

/* Build (set! __supers__ (append __supers__ (list expr))) */
sexp ps_make_super(sexp ctx, sexp expr) {
    sexp_gc_var2(list_expr, append_expr);
    sexp_gc_preserve2(ctx, list_expr, append_expr);

    list_expr = sexp_list2(ctx, ps_intern(ctx, "list"), expr);
    append_expr = sexp_list3(ctx, ps_intern(ctx, "append"),
                             ps_intern(ctx, "__supers__"), list_expr);
    sexp result = sexp_list3(ctx, ps_intern(ctx, "set!"),
                              ps_intern(ctx, "__supers__"), append_expr);
    sexp_gc_release2(ctx);
    return result;
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

/*  async expr  →
    (let ((p (__make-promise__)))
      (thread-start! (make-thread (lambda ()
        (protect (__async_e__
          (else ((p '__reject__) __async_e__)))
          ((p '__resolve__) expr)))))
      p) */
sexp ps_make_async(sexp ctx, sexp expr) {
    sexp p = ps_intern(ctx, "__async_p__");
    sexp e = ps_intern(ctx, "__async_e__");
    sexp make_prom = sexp_list1(ctx, ps_intern(ctx, "__make-promise__"));
    /* ((p '__resolve__) expr) */
    sexp q_resolve = sexp_list2(ctx, ps_intern(ctx, "quote"), ps_intern(ctx, "__resolve__"));
    sexp get_resolve = sexp_list2(ctx, p, q_resolve);
    sexp do_resolve = sexp_list2(ctx, get_resolve, expr);
    /* ((p '__reject__) __async_e__) */
    sexp q_reject = sexp_list2(ctx, ps_intern(ctx, "quote"), ps_intern(ctx, "__reject__"));
    sexp get_reject = sexp_list2(ctx, p, q_reject);
    sexp do_reject = sexp_list2(ctx, get_reject, e);
    /* (else ((p '__reject__) e)) */
    sexp else_clause = sexp_list2(ctx, ps_intern(ctx, "else"), do_reject);
    /* (protect (e (else ...)) ((p '__resolve__) expr)) */
    sexp protect_args = sexp_list2(ctx, e, else_clause);
    sexp protect_form = sexp_list3(ctx, ps_intern(ctx, "protect"), protect_args, do_resolve);
    /* (lambda () protect_form) */
    sexp thunk = sexp_list3(ctx, ps_intern(ctx, "lambda"), SEXP_NULL, protect_form);
    sexp mk_thread = sexp_list2(ctx, ps_intern(ctx, "make-thread"), thunk);
    sexp start = sexp_list2(ctx, ps_intern(ctx, "thread-start!"), mk_thread);
    sexp body = sexp_list3(ctx, ps_intern(ctx, "begin"), start, p);
    sexp binding = sexp_list1(ctx, sexp_list2(ctx, p, make_prom));
    return sexp_list3(ctx, ps_intern(ctx, "let"), binding, body);
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
        sexp key = sexp_car(entry);    /* symbol */
        sexp val = sexp_cdr(entry);    /* expr */
        sexp quoted_key = sexp_list2(ctx, ps_intern(ctx, "quote"), key);
        sexp cons_form = sexp_list3(ctx, ps_intern(ctx, "cons"), quoted_key, val);
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
