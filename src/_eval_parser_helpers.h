/*  _eval_parser_helpers.h -- Helper functions for building sexp trees  */

#ifndef EVAL_PARSER_HELPERS_H
#define EVAL_PARSER_HELPERS_H

#include <chibi/eval.h>

/* Parser state passed to Lemon as extra_argument */
typedef struct {
    sexp ctx;
    sexp env;
    sexp result;
    char *error_msg;
    int error_line;
    int error_col;
    int has_error;
} EvalParserState;

/* Intern a symbol */
sexp ps_intern(sexp ctx, const char *name);

/* Build (set! name (op name val)) for compound assignment */
sexp ps_set_op(sexp ctx, const char *op, sexp name, sexp val);

/* Build (set! name (+ name 1)) or (set! name (- name 1)) */
sexp ps_incr(sexp ctx, sexp name, int delta);

/* Build (arithmetic-shift a (- b)) for right shift */
sexp ps_shift_right(sexp ctx, sexp left, sexp right);

/* Build (lambda (params) (*catch 'return body)) */
sexp ps_make_function(sexp ctx, sexp params, sexp body);

/* Build (lambda (a . rest) (*catch 'return body)) with rest param */
sexp ps_make_function_rest(sexp ctx, sexp params, sexp rest, sexp body);

/* Build while loop using letrec */
sexp ps_make_while(sexp ctx, sexp cond, sexp body);

/* Build for loop using letrec */
sexp ps_make_for(sexp ctx, sexp init, sexp cond, sexp step, sexp body);

/* Build do-until loop using letrec */
sexp ps_make_do_until(sexp ctx, sexp body, sexp cond);

/* Build do-while loop using letrec */
sexp ps_make_do_while(sexp ctx, sexp body, sexp cond);

/* Build (*throw 'break '()) */
sexp ps_make_break(sexp ctx);

/* Build (*throw 'return expr) */
sexp ps_make_return(sexp ctx, sexp expr);

/* Build constructor pattern */
sexp ps_make_constructor(sexp ctx, sexp params, sexp body);

/* Build interface (cond dispatch) */
sexp ps_make_interface(sexp ctx, sexp entries);

/* Build constructor with static methods/values.
   iface_entries is a list of (name . expr) pairs, or SEXP_NULL for statics-only. */
sexp ps_make_constructor_static(sexp ctx, sexp params, sexp statics, sexp iface_entries);

/* Build abstract method: (lambda __args__ (error "abstract method" "name")) */
sexp ps_make_abstract_method(sexp ctx, const char *name, int length);

/* Build abstract constructor (blocks direct instantiation via __abstract_ok__ flag) */
sexp ps_make_abstract_constructor(sexp ctx, sexp params, sexp body);

/* Build abstract constructor with static wrapper */
sexp ps_make_abstract_constructor_static(sexp ctx, sexp params, sexp statics, sexp iface_entries);

/* Build (set! supers (append supers (list expr))) */
sexp ps_make_super(sexp ctx, sexp expr);

/* Build let, let-star, or letrec with bindings and body */
sexp ps_make_let(sexp ctx, const char *kind, sexp bindings, sexp body);

/* Build (guard (var (#t handler)) body) */
sexp ps_make_try_catch(sexp ctx, sexp body, sexp var, sexp handler);

/* Build (guard (var (test1 handler1) ...) body) for multi-clause catch */
sexp ps_make_try_catch_multi(sexp ctx, sexp body, sexp var, sexp clauses);

/* try/catch/finally — wraps try/catch in dynamic-wind for cleanup */
sexp ps_make_try_catch_finally(sexp ctx, sexp body, sexp var, sexp handler, sexp cleanup);

/* try/catch(multi)/finally */
sexp ps_make_try_catch_multi_finally(sexp ctx, sexp body, sexp var, sexp clauses, sexp cleanup);

/* try/finally (no catch) — guaranteed cleanup via dynamic-wind */
sexp ps_make_try_finally(sexp ctx, sexp body, sexp cleanup);

/* with(r = expr) body — RAII: calls r->close() on scope exit via dynamic-wind */
sexp ps_make_with(sexp ctx, sexp bindings, sexp body);

/* async expr — spawn green thread, return promise */
sexp ps_make_async(sexp ctx, sexp expr);

/* parallel async expr — dispatch to OS thread pool, return promise */
sexp ps_make_parallel_async(sexp ctx, sexp expr);

/* Build (call-with-values (lambda () expr) (lambda params body)) */
sexp ps_make_receive(sexp ctx, sexp params, sexp expr, sexp body);

/* Build (define-record-type ...) */
sexp ps_make_record(sexp ctx, sexp name, sexp fields);

/* Build a begin form from two expressions */
sexp ps_sexp_begin(sexp ctx, sexp a, sexp b);

/* Append element to end of list */
sexp ps_append(sexp ctx, sexp list, sexp elem);

/* Build a sexp list from token identifiers */
sexp ps_make_ident(sexp ctx, const char *start, int length);

/* Quote a symbol */
sexp ps_sexp_quote(sexp ctx, sexp sym);

/* Make a string sexp from token */
sexp ps_make_string(sexp ctx, const char *start, int length);

/* Build cond clause */
sexp ps_make_cond_clause(sexp ctx, sexp test, sexp expr);

/* Build case clause */
sexp ps_make_case_clause(sexp ctx, sexp datums, sexp expr);

/* Build (define-library (name...) body-forms...) */
sexp ps_make_library(sexp ctx, sexp name, sexp body);

/* Make a begin-with-defines safe for expression context by converting
   leading (define x v) forms into (let* ((x v) ...) remaining-body...) */
sexp ps_expr_safe(sexp ctx, sexp expr);

/* Map an operator token type to its Scheme symbol (for op-as-value) */
sexp ps_intern_op(sexp ctx, int token_type);
sexp ps_make_include(sexp ctx, sexp string_list);
sexp ps_make_dict(sexp ctx, sexp entries);
sexp ps_make_dict_comp(sexp ctx, sexp body, sexp clauses);

/* Build list comprehension from body expr and clause list */
sexp ps_make_list_comp(sexp ctx, sexp body, sexp clauses);

/* Build generator function: (lambda (params) (make-coroutine-generator ...)) */
sexp ps_make_generator(sexp ctx, sexp params, sexp body);

/* Build generator function with rest params */
sexp ps_make_generator_rest(sexp ctx, sexp params, sexp rest, sexp body);

/* Build generator comprehension: (make-coroutine-generator (lambda (__yield__) ...)) */
sexp ps_make_gen_comp(sexp ctx, sexp body, sexp clauses);

/* Build (ref obj index) for indexing */
sexp ps_make_ref(sexp ctx, sexp obj, sexp index);

/* Build (slice obj start end) for slicing */
sexp ps_make_slice(sexp ctx, sexp obj, sexp start, sexp end);

/* Parse Eval source code into a chibi sexp.
   Returns the parsed sexp, or SEXP_VOID on error.
   On error, sets *error_msg (caller must free), *error_line, *error_col. */
sexp eval_parse(sexp ctx, sexp env, const char *source,
                char **error_msg, int *error_line, int *error_col);

#endif /* EVAL_PARSER_HELPERS_H */
