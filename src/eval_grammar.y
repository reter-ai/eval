/* eval_grammar.y -- Lemon LALR grammar for the Eval language */

%token_type { EvalToken }
%extra_argument { EvalParserState *state }
%token_prefix TOK_
%syntax_error {
    state->has_error = 1;
    state->error_line = TOKEN.line;
    state->error_col = TOKEN.col;
    if (state->error_msg) free(state->error_msg);
    state->error_msg = (char*)malloc(256);
    if (state->error_msg) {
        snprintf(state->error_msg, 256,
                 "Syntax error at line %d, col %d near '%.*s'",
                 TOKEN.line, TOKEN.col, TOKEN.length > 30 ? 30 : TOKEN.length,
                 TOKEN.start ? TOKEN.start : "EOF");
    }
}

%include {
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <chibi/eval.h>
#include "_eval_lexer.h"
#include "_eval_parser_helpers.h"

#define ctx (state->ctx)
#define env (state->env)
}

/* Operator precedence (low to high).
   ARGLIST_PREC is a pseudo-token at the lowest level, used to annotate
   'arglist_inner ::= expr' so that in LALR-merged states the shift for
   RPAREN/COMMA (delimiters) wins over the reduce to arglist_inner. */
%nonassoc ARGLIST_PREC.
%right ASSIGN PLUS_ASSIGN MINUS_ASSIGN.
%nonassoc IF_WITHOUT_ELSE.
%nonassoc ELSE.
%right RETURN_PREC SUPER_PREC.
%left OR.
%left AND.
%left BITOR.
%left BITAND.
%left EQEQ EQQ BANGEQ.
%left LT GT LTE GTE.
%left SHL SHR.
%left PLUS MINUS.
%left STAR SLASH PERCENT.
%right STARSTAR.
%right UMINUS BANG BITNOT BANGBANG QUOTE_PREC.
%left LPAREN ARROW PLUSPLUS MINUSMINUS.
%nonassoc RPAREN COMMA DOTDOT RBRACKET RBRACE SEMICOLON COLON.
/* Additional keyword tokens recognized by the lexer.
   Declaring them here makes lemon generate their TOK_ defines. */
%token LIBRARY EXPORT INCLUDE MACRO SYNTAX_RULES OPVAL TEST_GROUP DEFINE IN.

/* Non-terminal types - all are sexp */
%type program { sexp }
%type stmt_list { sexp }
%type stmt { sexp }
%type expr { sexp }
%type arglist { sexp }
%type arglist_inner { sexp }
%type params { sexp }
%type params_inner { sexp }
%type bindings { sexp }
%type ident_list { sexp }
%type list_items { sexp }
%type iface_entries { sexp }
%type iface_entry { sexp }
%type cond_clauses { sexp }
%type case_clauses { sexp }
%type datum_list { sexp }
%type catch_clauses { sexp }
%type field_list { sexp }
%type dotpair_rest { sexp }
%type string_list { sexp }
%type sr_clauses { sexp }
%type sr_pattern { sexp }

/* ===== PROGRAM ===== */

program ::= stmt_list(L). { state->result = L; }
program ::= . { state->result = SEXP_VOID; }

/* ===== STATEMENT LIST ===== */

stmt_list(A) ::= stmt(S). { A = S; }
stmt_list(A) ::= stmt_list(B) stmt(S). { A = ps_sexp_begin(ctx, B, S); }

/* ===== STATEMENT ===== */

stmt(A) ::= expr(E) SEMICOLON. { A = E; }

/* ===== EXPRESSIONS ===== */

/* --- Assignment / Definition --- */
expr(A) ::= IDENT(N) ASSIGN expr(E). {
    A = sexp_list3(ctx, ps_intern(ctx, "set!"),
        ps_make_ident(ctx, N.start, N.length), E);
}
/* define x = 10;  →  (define x 10) */
expr(A) ::= DEFINE IDENT(N) ASSIGN expr(E). {
    A = sexp_list3(ctx, ps_intern(ctx, "define"),
        ps_make_ident(ctx, N.start, N.length), E);
}
expr(A) ::= IDENT(N) PLUS_ASSIGN expr(E). {
    A = ps_set_op(ctx, "+", ps_make_ident(ctx, N.start, N.length), E);
}
expr(A) ::= IDENT(N) MINUS_ASSIGN expr(E). {
    A = ps_set_op(ctx, "-", ps_make_ident(ctx, N.start, N.length), E);
}

/* --- Logical --- */
expr(A) ::= expr(L) OR expr(R). {
    A = sexp_list3(ctx, ps_intern(ctx, "or"), L, R);
}
expr(A) ::= expr(L) AND expr(R). {
    A = sexp_list3(ctx, ps_intern(ctx, "and"), L, R);
}

/* --- Bitwise --- */
expr(A) ::= expr(L) BITOR expr(R). {
    A = sexp_list3(ctx, ps_intern(ctx, "bitwise-ior"), L, R);
}
expr(A) ::= expr(L) BITAND expr(R). {
    A = sexp_list3(ctx, ps_intern(ctx, "bitwise-and"), L, R);
}
expr(A) ::= expr(L) SHL expr(R). {
    A = sexp_list3(ctx, ps_intern(ctx, "arithmetic-shift"), L, R);
}
expr(A) ::= expr(L) SHR expr(R). {
    A = ps_shift_right(ctx, L, R);
}

/* --- Comparison --- */
expr(A) ::= expr(L) EQEQ expr(R). {
    A = sexp_list3(ctx, ps_intern(ctx, "equal?"), L, R);
}
expr(A) ::= expr(L) EQQ expr(R). {
    A = sexp_list3(ctx, ps_intern(ctx, "eq?"), L, R);
}
expr(A) ::= expr(L) BANGEQ expr(R). {
    A = sexp_list2(ctx, ps_intern(ctx, "not"),
        sexp_list3(ctx, ps_intern(ctx, "equal?"), L, R));
}
expr(A) ::= expr(L) LT expr(R). {
    A = sexp_list3(ctx, ps_intern(ctx, "<"), L, R);
}
expr(A) ::= expr(L) GT expr(R). {
    A = sexp_list3(ctx, ps_intern(ctx, ">"), L, R);
}
expr(A) ::= expr(L) LTE expr(R). {
    A = sexp_list3(ctx, ps_intern(ctx, "<="), L, R);
}
expr(A) ::= expr(L) GTE expr(R). {
    A = sexp_list3(ctx, ps_intern(ctx, ">="), L, R);
}

/* --- Arithmetic --- */
expr(A) ::= expr(L) PLUS expr(R). {
    A = sexp_list3(ctx, ps_intern(ctx, "+"), L, R);
}
expr(A) ::= expr(L) MINUS expr(R). {
    A = sexp_list3(ctx, ps_intern(ctx, "-"), L, R);
}
expr(A) ::= expr(L) STAR expr(R). {
    A = sexp_list3(ctx, ps_intern(ctx, "*"), L, R);
}
expr(A) ::= expr(L) SLASH expr(R). {
    A = sexp_list3(ctx, ps_intern(ctx, "/"), L, R);
}
expr(A) ::= expr(L) PERCENT expr(R). {
    A = sexp_list3(ctx, ps_intern(ctx, "modulo"), L, R);
}
expr(A) ::= expr(L) STARSTAR expr(R). {
    A = sexp_list3(ctx, ps_intern(ctx, "expt"), L, R);
}

/* --- Unary --- */
expr(A) ::= PLUS expr(E). [UMINUS] {
    /* Unary +: identity (just return the expression) */
    A = E;
}
expr(A) ::= MINUS expr(E). [UMINUS] {
    A = sexp_list2(ctx, ps_intern(ctx, "-"), E);
}
expr(A) ::= BANG expr(E). {
    A = sexp_list2(ctx, ps_intern(ctx, "not"), E);
}
expr(A) ::= BITNOT expr(E). {
    A = sexp_list2(ctx, ps_intern(ctx, "bitwise-not"), E);
}
expr(A) ::= BANGBANG expr(E). {
    /* Compile-time eval: just eval the expression */
    A = sexp_eval(ctx, E, env);
}

/* --- Postfix: function call --- */
expr(A) ::= expr(E) LPAREN arglist(L) RPAREN. {
    A = sexp_cons(ctx, E, L);
}

/* --- Postfix: member access --- */
expr(A) ::= expr(E) ARROW IDENT(M). {
    A = sexp_list2(ctx, E,
        sexp_list2(ctx, ps_intern(ctx, "quote"),
                   ps_make_ident(ctx, M.start, M.length)));
}

/* --- Postfix: increment/decrement --- */
expr(A) ::= IDENT(N) PLUSPLUS. {
    A = ps_incr(ctx, ps_make_ident(ctx, N.start, N.length), 1);
}
expr(A) ::= IDENT(N) MINUSMINUS. {
    A = ps_incr(ctx, ps_make_ident(ctx, N.start, N.length), -1);
}

/* --- Primary: literals --- */
expr(A) ::= INT(V). {
    A = sexp_make_fixnum(V.value.int_val);
}
expr(A) ::= FLOAT(V). {
    A = sexp_make_flonum(ctx, V.value.float_val);
}
expr(A) ::= STRING(V). {
    A = ps_make_string(ctx, V.start, V.length);
}
expr(A) ::= IDENT(V). {
    A = ps_make_ident(ctx, V.start, V.length);
}
expr(A) ::= TRUE. {
    A = SEXP_TRUE;
}
expr(A) ::= FALSE. {
    A = SEXP_FALSE;
}
expr(A) ::= NIL. {
    A = sexp_list2(ctx, ps_intern(ctx, "quote"), SEXP_NULL);
}

/* --- Operator as value: bare +, *, etc. in value position --- */
expr(A) ::= OPVAL(V). {
    A = ps_intern_op(ctx, (int)V.value.int_val);
}

/* --- Quote --- */
expr(A) ::= QUOTE expr(E). [QUOTE_PREC] {
    A = sexp_list2(ctx, ps_intern(ctx, "quote"), E);
}
/* '() — quoted empty list (special rule to avoid LALR conflict with arglist) */
expr(A) ::= QUOTE LPAREN RPAREN. [QUOTE_PREC] {
    A = sexp_list2(ctx, ps_intern(ctx, "quote"), SEXP_NULL);
}

/* --- Grouping --- */
expr(A) ::= LPAREN expr(E) RPAREN. {
    A = E;
}

/* --- Dotted pair: (a .. c) or (a, b .. c) --- */
/* Split into two rules to avoid LALR conflict with grouping (expr).
   After LPAREN expr, the next token (DOTDOT vs COMMA vs RPAREN) decides. */
expr(A) ::= LPAREN expr(E) DOTDOT expr(R) RPAREN. {
    /* (a .. c) -> (cons a c) */
    A = sexp_list3(ctx, ps_intern(ctx, "cons"), E, R);
}
expr(A) ::= LPAREN expr(E) COMMA dotpair_rest(L) DOTDOT expr(R) RPAREN. {
    /* (a, b, ... .. c) -> (cons a (cons b (... c))) */
    /* First reverse L, then fold right building nested cons */
    sexp rev = SEXP_NULL;
    sexp p = L;
    while (sexp_pairp(p)) {
        rev = sexp_cons(ctx, sexp_car(p), rev);
        p = sexp_cdr(p);
    }
    /* Now fold: start with R, prepend each element of rev */
    A = R;
    p = rev;
    while (sexp_pairp(p)) {
        A = sexp_list3(ctx, ps_intern(ctx, "cons"), sexp_car(p), A);
        p = sexp_cdr(p);
    }
    /* Wrap outermost: (cons E ...) */
    A = sexp_list3(ctx, ps_intern(ctx, "cons"), E, A);
}

/* --- Block --- */
expr(A) ::= LBRACE stmt_list(L) RBRACE. {
    if (sexp_pairp(L) && sexp_car(L) == ps_intern(ctx, "begin")) {
        A = ps_expr_safe(ctx, L);
    } else {
        A = ps_expr_safe(ctx, sexp_list2(ctx, ps_intern(ctx, "begin"), L));
    }
}
/* Block with trailing expr (no semicolon on last): { a; b; c } */
expr(A) ::= LBRACE stmt_list(L) expr(E) RBRACE. {
    A = ps_expr_safe(ctx, ps_sexp_begin(ctx, L, E));
}
/* Block with single expr (no semicolon): { expr } */
expr(A) ::= LBRACE expr(E) RBRACE. {
    A = ps_expr_safe(ctx, sexp_list2(ctx, ps_intern(ctx, "begin"), E));
}
expr(A) ::= LBRACE RBRACE. {
    A = SEXP_VOID;
}

/* --- List literal: [a, b, c] --- */
expr(A) ::= LBRACKET list_items(L) RBRACKET. {
    A = sexp_cons(ctx, ps_intern(ctx, "list"), L);
}
expr(A) ::= LBRACKET RBRACKET. {
    A = sexp_list1(ctx, ps_intern(ctx, "list"));
}

/* --- Vector literal: #[a, b, c] --- */
expr(A) ::= HASH_LBRACKET list_items(L) RBRACKET. {
    A = sexp_cons(ctx, ps_intern(ctx, "vector"), L);
}
expr(A) ::= HASH_LBRACKET RBRACKET. {
    A = sexp_list1(ctx, ps_intern(ctx, "vector"));
}

/* --- Function --- */
expr(A) ::= FUNCTION LPAREN params(P) RPAREN expr(B). [ARGLIST_PREC] {
    A = ps_make_function(ctx, P, B);
}
expr(A) ::= FUNCTION LPAREN params_inner(P) DOTDOT IDENT(R) RPAREN expr(B). [ARGLIST_PREC] {
    A = ps_make_function_rest(ctx, P, ps_make_ident(ctx, R.start, R.length), B);
}
expr(A) ::= FUNCTION LPAREN DOTDOT IDENT(R) RPAREN expr(B). [ARGLIST_PREC] {
    A = ps_make_function_rest(ctx, SEXP_NULL, ps_make_ident(ctx, R.start, R.length), B);
}

/* --- If --- */
expr(A) ::= IF LPAREN expr(C) RPAREN expr(T). [IF_WITHOUT_ELSE] {
    A = sexp_list3(ctx, ps_intern(ctx, "if"), C, T);
}
expr(A) ::= IF LPAREN expr(C) RPAREN expr(T) ELSE expr(E). [ARGLIST_PREC] {
    A = sexp_cons(ctx, ps_intern(ctx, "if"),
          sexp_cons(ctx, C,
            sexp_cons(ctx, T,
              sexp_cons(ctx, E, SEXP_NULL))));
}

/* --- Loops --- */
expr(A) ::= WHILE LPAREN expr(C) RPAREN expr(B). [ARGLIST_PREC] {
    A = ps_make_while(ctx, C, B);
}
expr(A) ::= FOR LPAREN expr(I) COMMA expr(C) COMMA expr(S) RPAREN expr(B). [ARGLIST_PREC] {
    A = ps_make_for(ctx, I, C, S, B);
}
/* for(let j = 0, ...) — j is loop-scoped */
expr(A) ::= FOR LPAREN LET IDENT(N) ASSIGN expr(I) COMMA expr(C) COMMA expr(S) RPAREN expr(B). [ARGLIST_PREC] {
    sexp init = sexp_list3(ctx, ps_intern(ctx, "define"),
        ps_make_ident(ctx, N.start, N.length), I);
    A = ps_make_for(ctx, init, C, S, B);
}
expr(A) ::= DO expr(B) UNTIL LPAREN expr(C) RPAREN. {
    A = ps_make_do_until(ctx, B, C);
}
/* for(let x in collection) body -- for-each loop */
expr(A) ::= FOR LPAREN LET IDENT(V) IN expr(L) RPAREN expr(B). [ARGLIST_PREC] {
    sexp var = ps_make_ident(ctx, V.start, V.length);
    sexp lam = sexp_list3(ctx, ps_intern(ctx, "lambda"),
                   sexp_list1(ctx, var), B);
    A = sexp_list3(ctx, ps_intern(ctx, "for-each"), lam, L);
}
expr(A) ::= BREAK. {
    A = ps_make_break(ctx);
}
expr(A) ::= RETURN expr(E). [RETURN_PREC] {
    A = ps_make_return(ctx, E);
}

/* --- OOP --- */
expr(A) ::= CONSTRUCTOR LPAREN params(P) RPAREN expr(B). [ARGLIST_PREC] {
    A = ps_make_constructor(ctx, P, B);
}
expr(A) ::= INTERFACE LPAREN iface_entries(E) RPAREN. {
    A = ps_make_interface(ctx, E);
}
expr(A) ::= SUPER expr(E). [SUPER_PREC] {
    A = ps_make_super(ctx, E);
}

/* ===== R7RS EXTENSIONS ===== */

/* --- Let / Let* / Letrec --- */
expr(A) ::= LET LPAREN bindings(B) RPAREN expr(E). [ARGLIST_PREC] {
    A = ps_make_let(ctx, "let", B, E);
}
expr(A) ::= LETSTAR LPAREN bindings(B) RPAREN expr(E). [ARGLIST_PREC] {
    A = ps_make_let(ctx, "let*", B, E);
}
expr(A) ::= LETREC LPAREN bindings(B) RPAREN expr(E). [ARGLIST_PREC] {
    A = ps_make_let(ctx, "letrec", B, E);
}

/* --- Try/Catch --- */
expr(A) ::= TRY expr(B) CATCH LPAREN IDENT(E) RPAREN expr(H). [ARGLIST_PREC] {
    A = ps_make_try_catch(ctx, B, ps_make_ident(ctx, E.start, E.length), H);
}
expr(A) ::= TRY expr(B) CATCH LPAREN IDENT(E) COMMA catch_clauses(C) RPAREN. {
    A = ps_make_try_catch_multi(ctx, B, ps_make_ident(ctx, E.start, E.length), C);
}

/* --- Import --- */
expr(A) ::= IMPORT LPAREN ident_list(L) RPAREN. {
    A = sexp_cons(ctx, ps_intern(ctx, "import"),
                  sexp_list1(ctx, L));
}

/* --- Cond --- */
expr(A) ::= COND LPAREN cond_clauses(C) RPAREN. {
    A = sexp_cons(ctx, ps_intern(ctx, "cond"), C);
}

/* --- Case --- */
expr(A) ::= CASE LPAREN expr(K) COMMA case_clauses(C) RPAREN. {
    A = sexp_cons(ctx, ps_intern(ctx, "case"),
                  sexp_cons(ctx, K, C));
}

/* --- When / Unless --- */
expr(A) ::= WHEN LPAREN expr(C) RPAREN expr(B). [ARGLIST_PREC] {
    /* when(cond) body  ->  (if cond body) */
    A = sexp_list3(ctx, ps_intern(ctx, "if"), C, B);
}
expr(A) ::= UNLESS LPAREN expr(C) RPAREN expr(B). [ARGLIST_PREC] {
    /* unless(cond) body  ->  (if (not cond) body) */
    A = sexp_list3(ctx, ps_intern(ctx, "if"),
        sexp_list2(ctx, ps_intern(ctx, "not"), C), B);
}

/* --- Values / Receive --- */
expr(A) ::= VALUES LPAREN arglist(L) RPAREN. {
    A = sexp_cons(ctx, ps_intern(ctx, "values"), L);
}
expr(A) ::= RECEIVE LPAREN params(P) RPAREN FROM expr(E) expr(B). [ARGLIST_PREC] {
    A = ps_make_receive(ctx, P, E, B);
}

/* --- Export --- */
expr(A) ::= EXPORT LPAREN ident_list(L) RPAREN. {
    A = sexp_cons(ctx, ps_intern(ctx, "export"), L);
}

/* --- Include --- */
expr(A) ::= INCLUDE LPAREN string_list(L) RPAREN. {
    A = sexp_cons(ctx, ps_intern(ctx, "include"), L);
}

/* --- Library --- */
/* library(scheme, base) { export(...); body... }
   -> (define-library (scheme base) export-and-body-forms...) */
expr(A) ::= LIBRARY LPAREN ident_list(N) RPAREN LBRACE stmt_list(B) RBRACE. {
    A = ps_make_library(ctx, N, B);
}
expr(A) ::= LIBRARY LPAREN ident_list(N) RPAREN LBRACE RBRACE. {
    A = sexp_list2(ctx, ps_intern(ctx, "define-library"), N);
}

/* --- Macro (define-syntax) --- */
/* macro name expr  ->  (define-syntax name expr) */
expr(A) ::= MACRO IDENT(N) expr(E). [RETURN_PREC] {
    A = sexp_list3(ctx, ps_intern(ctx, "define-syntax"),
        ps_make_ident(ctx, N.start, N.length), E);
}

/* --- Syntax-rules literal --- */
/* syntax_rules(lit, ...) { (pattern): template, ... }
   -> (syntax-rules (lit ...) (pattern template) ...) */
expr(A) ::= SYNTAX_RULES LPAREN ident_list(L) RPAREN LBRACE sr_clauses(C) RBRACE. {
    A = sexp_cons(ctx, ps_intern(ctx, "syntax-rules"),
          sexp_cons(ctx, L, C));
}
expr(A) ::= SYNTAX_RULES LPAREN RPAREN LBRACE sr_clauses(C) RBRACE. {
    A = sexp_cons(ctx, ps_intern(ctx, "syntax-rules"),
          sexp_cons(ctx, SEXP_NULL, C));
}

/* --- Record --- */
expr(A) ::= RECORD IDENT(N) LPAREN field_list(F) RPAREN. {
    A = ps_make_record(ctx, ps_make_ident(ctx, N.start, N.length), F);
}

/* --- Test group --- */
expr(A) ::= TEST_GROUP LPAREN expr(N) RPAREN expr(B). [ARGLIST_PREC] {
    /* test_group("name") body  →  (test-group name (lambda () body)) */
    A = sexp_list3(ctx, ps_intern(ctx, "test-group"), N,
                   sexp_list3(ctx, ps_intern(ctx, "lambda"), SEXP_NULL, B));
}

/* ===== HELPER RULES ===== */

/* Argument list (possibly empty) */
arglist(A) ::= . { A = SEXP_NULL; }
arglist(A) ::= arglist_inner(L). { A = L; }

arglist_inner(A) ::= expr(E). [ARGLIST_PREC] { A = sexp_list1(ctx, E); }
arglist_inner(A) ::= arglist_inner(L) COMMA expr(E). {
    A = ps_append(ctx, L, E);
}

/* Parameter list (possibly empty) */
params(A) ::= . { A = SEXP_NULL; }
params(A) ::= params_inner(L). { A = L; }

params_inner(A) ::= IDENT(N). {
    A = sexp_list1(ctx, ps_make_ident(ctx, N.start, N.length));
}
params_inner(A) ::= params_inner(L) COMMA IDENT(N). {
    A = ps_append(ctx, L, ps_make_ident(ctx, N.start, N.length));
}

/* Bindings for let, let-star, letrec: x = 1, y = 2 */
bindings(A) ::= IDENT(N) ASSIGN expr(E). {
    A = sexp_list1(ctx,
        sexp_list2(ctx, ps_make_ident(ctx, N.start, N.length), E));
}
bindings(A) ::= bindings(L) COMMA IDENT(N) ASSIGN expr(E). {
    A = ps_append(ctx, L,
        sexp_list2(ctx, ps_make_ident(ctx, N.start, N.length), E));
}

/* Identifier list for imports: scheme, base */
ident_list(A) ::= IDENT(N). {
    A = sexp_list1(ctx, ps_make_ident(ctx, N.start, N.length));
}
ident_list(A) ::= ident_list(L) COMMA IDENT(N). {
    A = ps_append(ctx, L, ps_make_ident(ctx, N.start, N.length));
}

/* List items: a, b, c */
list_items(A) ::= expr(E). { A = sexp_list1(ctx, E); }
list_items(A) ::= list_items(L) COMMA expr(E). {
    A = ps_append(ctx, L, E);
}

/* Interface entries: name: expr, name: expr */
iface_entries(A) ::= iface_entry(E). { A = sexp_list1(ctx, E); }
iface_entries(A) ::= iface_entries(L) COMMA iface_entry(E). {
    A = ps_append(ctx, L, E);
}
iface_entry(A) ::= IDENT(N) COLON expr(E). {
    A = sexp_cons(ctx, ps_make_ident(ctx, N.start, N.length), E);
}

/* Cond clauses: test: expr, test: expr, else: expr */
cond_clauses(A) ::= expr(T) COLON expr(E). {
    A = sexp_list1(ctx, ps_make_cond_clause(ctx, T, E));
}
cond_clauses(A) ::= cond_clauses(L) COMMA expr(T) COLON expr(E). {
    A = ps_append(ctx, L, ps_make_cond_clause(ctx, T, E));
}
cond_clauses(A) ::= cond_clauses(L) COMMA ELSE COLON expr(E). {
    A = ps_append(ctx, L,
        ps_make_cond_clause(ctx, ps_intern(ctx, "else"), E));
}

/* Case clauses */
case_clauses(A) ::= LPAREN datum_list(D) RPAREN COLON expr(E). {
    A = sexp_list1(ctx, ps_make_case_clause(ctx, D, E));
}
case_clauses(A) ::= case_clauses(L) COMMA LPAREN datum_list(D) RPAREN COLON expr(E). {
    A = ps_append(ctx, L, ps_make_case_clause(ctx, D, E));
}
case_clauses(A) ::= case_clauses(L) COMMA ELSE COLON expr(E). {
    A = ps_append(ctx, L,
        ps_make_case_clause(ctx, ps_intern(ctx, "else"), E));
}

datum_list(A) ::= expr(E). { A = sexp_list1(ctx, E); }
datum_list(A) ::= datum_list(L) COMMA expr(E). {
    A = ps_append(ctx, L, E);
}

/* Catch clauses for multi-clause try/catch */
catch_clauses(A) ::= expr(T) COLON expr(H). {
    A = sexp_list1(ctx, sexp_list2(ctx, T, H));
}
catch_clauses(A) ::= catch_clauses(L) COMMA expr(T) COLON expr(H). {
    A = ps_append(ctx, L, sexp_list2(ctx, T, H));
}

/* Field list for record */
field_list(A) ::= IDENT(N). {
    A = sexp_list1(ctx, ps_make_ident(ctx, N.start, N.length));
}
field_list(A) ::= field_list(L) COMMA IDENT(N). {
    A = ps_append(ctx, L, ps_make_ident(ctx, N.start, N.length));
}

/* Dotted pair rest elements: b or b, c or b, c, d ... */
dotpair_rest(A) ::= expr(E). { A = sexp_list1(ctx, E); }
dotpair_rest(A) ::= dotpair_rest(L) COMMA expr(E). {
    A = ps_append(ctx, L, E);
}

/* String list for include: "file1", "file2" */
string_list(A) ::= STRING(S). {
    A = sexp_list1(ctx, ps_make_string(ctx, S.start, S.length));
}
string_list(A) ::= string_list(L) COMMA STRING(S). {
    A = ps_append(ctx, L, ps_make_string(ctx, S.start, S.length));
}

/* Syntax-rules clauses: (pattern): template, ... */
sr_clauses(A) ::= sr_pattern(P) COLON expr(T). {
    A = sexp_list1(ctx, sexp_list2(ctx, P, T));
}
sr_clauses(A) ::= sr_clauses(L) COMMA sr_pattern(P) COLON expr(T). {
    A = ps_append(ctx, L, sexp_list2(ctx, P, T));
}

/* Syntax-rules pattern: (items...) */
sr_pattern(A) ::= LPAREN arglist(L) RPAREN. { A = L; }
