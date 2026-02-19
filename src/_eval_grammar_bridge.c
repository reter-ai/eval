/*
 * _eval_grammar_bridge.c — Bridge between Lark grammar system and eval language.
 *
 * Provides:
 *   - Grammar type (wraps LarkGrammar*)
 *   - Parser type (wraps JIT-compiled parse function)
 *   - Bridge functions: __grammar_create__, __grammar_compile__, __parser_parse__
 *   - ast_to_sexp: converts C AstNode* tree to eval nested lists
 *
 * Registration: types must be registered in the same order in both the
 * main Python context (_chibi_context.c) and worker threads (_eval_pool.c).
 */

#include "_lark_parser.h"
#include "_lark_codegen.h"
#include "_eval_lemon_lib.h"
#include "_eval_re2c_lib.h"

#ifdef EVAL_HAVE_JIT
#include "_eval_jit.h"
#endif

/* chibi-scheme headers */
#include "chibi/eval.h"
#include "chibi/sexp.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* ================================================================
 * Type tags (module-level, set during registration)
 * ================================================================ */

static sexp_tag_t grammar_type_tag = 0;
static sexp_tag_t parser_type_tag = 0;

/* ================================================================
 * Grammar type: wraps LarkGrammar*
 * ================================================================ */

static sexp grammar_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    LarkGrammar* g = (LarkGrammar*)sexp_cpointer_value(obj);
    if (g) {
        lark_grammar_free(g);
        sexp_cpointer_value(obj) = NULL;
    }
    return SEXP_VOID;
}

void register_grammar_type(sexp ctx) {
    sexp_gc_var2(name, type);
    sexp_gc_preserve2(ctx, name, type);

    name = sexp_c_string(ctx, "eval-grammar", -1);
    type = sexp_register_c_type(ctx, name, grammar_finalize);

    if (sexp_typep(type)) {
        grammar_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }

    sexp_gc_release2(ctx);
}

static sexp wrap_grammar(sexp ctx, LarkGrammar* g) {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), grammar_type_tag);
    sexp_cpointer_value(result) = (void*)g;
    sexp_cpointer_length(result) = 0;

    sexp_gc_release1(ctx);
    return result;
}

static LarkGrammar* unwrap_grammar(sexp x) {
    if (sexp_pointerp(x) && sexp_pointer_tag(x) == grammar_type_tag)
        return (LarkGrammar*)sexp_cpointer_value(x);
    return NULL;
}

/* ================================================================
 * Parser type: wraps JIT-compiled parse function
 * ================================================================ */

/* Function pointer type for the JIT'd parse function */
typedef void* (*parse_fn_t)(const char* input, int length,
                             char* error_buf, int error_buf_size,
                             void** arena_out);

typedef struct {
    parse_fn_t parse_fn;     /* JIT'd parse function */
    char func_prefix[64];    /* for debugging */
} EvalParser;

static sexp parser_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    EvalParser* p = (EvalParser*)sexp_cpointer_value(obj);
    if (p) {
        free(p);
        sexp_cpointer_value(obj) = NULL;
    }
    return SEXP_VOID;
}

void register_parser_type(sexp ctx) {
    sexp_gc_var2(name, type);
    sexp_gc_preserve2(ctx, name, type);

    name = sexp_c_string(ctx, "eval-parser", -1);
    type = sexp_register_c_type(ctx, name, parser_finalize);

    if (sexp_typep(type)) {
        parser_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }

    sexp_gc_release2(ctx);
}

static sexp wrap_parser(sexp ctx, EvalParser* p) {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), parser_type_tag);
    sexp_cpointer_value(result) = (void*)p;
    sexp_cpointer_length(result) = 0;

    sexp_gc_release1(ctx);
    return result;
}

static EvalParser* unwrap_parser(sexp x) {
    if (sexp_pointerp(x) && sexp_pointer_tag(x) == parser_type_tag)
        return (EvalParser*)sexp_cpointer_value(x);
    return NULL;
}

/* ================================================================
 * Global JIT instance (lazy init, lives for program lifetime)
 * ================================================================ */

static int _grammar_counter = 0;

#ifdef EVAL_HAVE_JIT
static EvalJIT* _global_jit = NULL;

static EvalJIT* get_global_jit(void) {
    if (!_global_jit) {
        _global_jit = eval_jit_create();
    }
    return _global_jit;
}
#endif

/* ================================================================
 * AstNode → sexp conversion
 *
 * AstNode with value (leaf/token): → ("TOKEN_NAME" "text")
 * AstNode without value (rule):    → ("rule_name" child1 child2 ...)
 * ================================================================ */

/* Forward decl — struct must match the JIT'd code's AstNode layout */
typedef struct AstNodeBridge {
    const char* name;
    const char* value;
    int num_children;
    struct AstNodeBridge** children;
} AstNodeBridge;

static sexp ast_to_sexp(sexp ctx, AstNodeBridge* node) {
    if (!node) return SEXP_NULL;

    sexp_gc_var4(result, name_sexp, val_sexp, child);
    sexp_gc_preserve4(ctx, result, name_sexp, val_sexp, child);

    /* Node name as string */
    name_sexp = node->name
        ? sexp_c_string(ctx, node->name, -1)
        : sexp_c_string(ctx, "?", -1);

    if (node->value) {
        /* Leaf node (token): ("TOKEN_NAME" "text") */
        val_sexp = sexp_c_string(ctx, node->value, -1);
        result = sexp_cons(ctx, val_sexp, SEXP_NULL);
        result = sexp_cons(ctx, name_sexp, result);
    } else {
        /* Rule node: ("rule_name" child1 child2 ...) */
        /* Build children list right-to-left */
        result = SEXP_NULL;
        for (int i = node->num_children - 1; i >= 0; i--) {
            child = ast_to_sexp(ctx, node->children[i]);
            result = sexp_cons(ctx, child, result);
        }
        result = sexp_cons(ctx, name_sexp, result);
    }

    sexp_gc_release4(ctx);
    return result;
}

/* ================================================================
 * Bridge functions
 * ================================================================ */

/* __grammar_create__(text) → grammar object
 * Parses a Lark EBNF grammar string into a LarkGrammar. */
sexp bridge_grammar_create(sexp ctx, sexp self, sexp_sint_t n, sexp text) {
    if (!sexp_stringp(text))
        return sexp_user_exception(ctx, self, "Grammar: expected string argument", text);

    const char* src = sexp_string_data(text);
    char* error_msg = NULL;
    LarkGrammar* g = lark_parse_grammar(src, &error_msg);

    if (!g) {
        sexp err = sexp_user_exception(ctx, self,
            error_msg ? error_msg : "Grammar: parse error", text);
        free(error_msg);
        return err;
    }
    free(error_msg);

    return wrap_grammar(ctx, g);
}

/* __grammar_compile__(grammar) → parser object
 * Full pipeline: LarkGrammar → codegen → lemon → lexer → combine → JIT → Parser */
sexp bridge_grammar_compile(sexp ctx, sexp self, sexp_sint_t n, sexp grammar_obj) {
    LarkGrammar* g = unwrap_grammar(grammar_obj);
    if (!g)
        return sexp_user_exception(ctx, self, "compile: expected grammar object", grammar_obj);

#ifndef EVAL_HAVE_JIT
    return sexp_user_exception(ctx, self,
        "compile: LLVM JIT not available (build with -DBUILD_GRAMMAR_JIT=ON)", SEXP_FALSE);
#else
    EvalJIT* jit = get_global_jit();
    if (!jit)
        return sexp_user_exception(ctx, self,
            "compile: failed to initialize JIT engine", SEXP_FALSE);

    /* Generate unique prefix for this grammar */
    char parser_prefix[64], func_prefix[64];
    int id = _grammar_counter++;
    snprintf(parser_prefix, sizeof(parser_prefix), "Grammar%d", id);
    snprintf(func_prefix, sizeof(func_prefix), "grammar_%d", id);

    /* Step 0: Desugar parenthesized groups into synthetic rules */
    lark_desugar_groups(g);

    /* Step 1: Generate lemon .y from grammar */
    char* y_source = lark_generate_lemon_y(g, parser_prefix);
    if (!y_source)
        return sexp_user_exception(ctx, self,
            "compile: failed to generate lemon grammar", SEXP_FALSE);

    /* Step 2: Compile .y to C parser code */
    LemonOutput* lemon_out = lemon_lib_compile(y_source);
    free(y_source);

    if (!lemon_out || !lemon_out->success) {
        const char* msg = (lemon_out && lemon_out->error_msg)
            ? lemon_out->error_msg : "lemon compilation failed";
        sexp err = sexp_user_exception(ctx, self, msg, SEXP_FALSE);
        if (lemon_out) lemon_lib_free(lemon_out);
        return err;
    }

    /* Step 3: Generate lexer C code via re2c DFA compilation */
    char* re_source = lark_generate_re2c_re(g, func_prefix);
    if (!re_source) {
        lemon_lib_free(lemon_out);
        return sexp_user_exception(ctx, self,
            "compile: failed to generate re2c input", SEXP_FALSE);
    }

    Re2cOutput* re2c_out = re2c_lib_compile(re_source);
    free(re_source);

    if (!re2c_out || !re2c_out->success) {
        const char* msg = (re2c_out && re2c_out->error_msg)
            ? re2c_out->error_msg : "re2c lexer compilation failed";
        sexp err = sexp_user_exception(ctx, self, msg, SEXP_FALSE);
        if (re2c_out) re2c_lib_free(re2c_out);
        lemon_lib_free(lemon_out);
        return err;
    }

    char* lexer_c = re2c_out->c_code;
    re2c_out->c_code = NULL; /* take ownership */
    re2c_lib_free(re2c_out);

    /* Step 4: Generate combined C source */
    char* combined = lark_generate_combined_c(g,
        lemon_out->c_code, lemon_out->h_code, lexer_c, func_prefix);
    lemon_lib_free(lemon_out);
    free(lexer_c);

    if (!combined)
        return sexp_user_exception(ctx, self,
            "compile: failed to generate combined C source", SEXP_FALSE);

    /* Step 5: JIT compile to native code */
    char parse_sym[128];
    snprintf(parse_sym, sizeof(parse_sym), "%s_parse", func_prefix);

    char* jit_error = NULL;
    void* parse_ptr = eval_jit_compile(jit, combined, parse_sym, &jit_error);
    free(combined);

    if (!parse_ptr) {
        sexp err = sexp_user_exception(ctx, self,
            jit_error ? jit_error : "JIT compilation failed", SEXP_FALSE);
        free(jit_error);
        return err;
    }
    free(jit_error);

    /* Step 6: Wrap in EvalParser */
    EvalParser* ep = (EvalParser*)malloc(sizeof(EvalParser));
    if (!ep)
        return sexp_user_exception(ctx, self, "compile: out of memory", SEXP_FALSE);
    ep->parse_fn = (parse_fn_t)parse_ptr;
    strncpy(ep->func_prefix, func_prefix, sizeof(ep->func_prefix) - 1);
    ep->func_prefix[sizeof(ep->func_prefix) - 1] = 0;

    return wrap_parser(ctx, ep);
#endif /* EVAL_HAVE_JIT */
}

/* __parser_parse__(parser, input) → nested list AST
 * Calls JIT'd parse function, converts AstNode tree to sexp. */
sexp bridge_parser_parse(sexp ctx, sexp self, sexp_sint_t n,
                          sexp parser_obj, sexp input) {
    EvalParser* ep = unwrap_parser(parser_obj);
    if (!ep)
        return sexp_user_exception(ctx, self, "parse: expected parser object", parser_obj);

    if (!sexp_stringp(input))
        return sexp_user_exception(ctx, self, "parse: expected string input", input);

    const char* src = sexp_string_data(input);
    int len = (int)sexp_string_size(input);

    char error_buf[256] = {0};
    void* arena = NULL;

    /* Call JIT'd parse function */
    AstNodeBridge* ast = (AstNodeBridge*)ep->parse_fn(src, len,
        error_buf, sizeof(error_buf), &arena);

    if (!ast) {
        if (arena) free(arena);
        return sexp_user_exception(ctx, self,
            error_buf[0] ? error_buf : "parse error", input);
    }

    /* Convert AstNode tree to sexp nested lists */
    sexp result = ast_to_sexp(ctx, ast);

    /* Free the arena */
    if (arena) free(arena);

    return result;
}

/* ================================================================
 * Registration helper — call from register_bridge_functions_c
 * ================================================================ */

void register_grammar_bridge_functions(sexp ctx, sexp env) {
    sexp_define_foreign(ctx, env, "__grammar_create__", 1, bridge_grammar_create);
    sexp_define_foreign(ctx, env, "__grammar_compile__", 1, bridge_grammar_compile);
    sexp_define_foreign(ctx, env, "__parser_parse__", 2, bridge_parser_parse);
}
