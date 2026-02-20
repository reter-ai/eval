/*  _eval_rete.c -- Rete forward-chaining engine for Eval
 *
 *  Implements the alpha/beta network for "whenever" rules.
 *  Facts asserted via logic_assert_fact propagate through the network
 *  and fire matching rule actions with variable bindings.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <chibi/eval.h>
#include "_eval_rete.h"

/* ---- Helpers ---- */

#define VAR_PREFIX "__var_"
#define VAR_PREFIX_LEN 6

/* Get the C string data of a symbol (uses ctx for inline symbol decode) */
static const char *sym_cstr(sexp ctx, sexp sym) {
    if (sexp_lsymbolp(sym))
        return sexp_lsymbol_data(sym);
    /* Inline (Huffman) symbol: decode via sexp_symbol_to_string */
    sexp s = sexp_symbol_to_string(ctx, sym);
    return sexp_stringp(s) ? sexp_string_data(s) : "";
}

static int is_rete_var_ctx(sexp ctx, sexp x) {
    if (!sexp_symbolp(x)) return 0;
    const char *s = sym_cstr(ctx, x);
    return strncmp(s, VAR_PREFIX, VAR_PREFIX_LEN) == 0;
}

/* Extract variable name from __var_x -> "x" */
static const char *rete_var_name_ctx(sexp ctx, sexp x) {
    return sym_cstr(ctx, x) + VAR_PREFIX_LEN;
}

static char *str_dup(const char *s) {
    size_t len = strlen(s);
    char *d = (char *)malloc(len + 1);
    if (d) memcpy(d, s, len + 1);
    return d;
}

/* Get nth element of a Scheme list */
static sexp list_ref(sexp lst, int n) {
    for (int i = 0; i < n && sexp_pairp(lst); i++)
        lst = sexp_cdr(lst);
    return sexp_pairp(lst) ? sexp_car(lst) : SEXP_FALSE;
}

static int list_length(sexp lst) {
    int n = 0;
    for (; sexp_pairp(lst); lst = sexp_cdr(lst)) n++;
    return n;
}

/* Compare two sexp values for equality (used in constant tests) */
static int sexp_equal(sexp ctx, sexp a, sexp b) {
    if (a == b) return 1;
    if (sexp_fixnump(a) && sexp_fixnump(b))
        return sexp_unbox_fixnum(a) == sexp_unbox_fixnum(b);
    if (sexp_stringp(a) && sexp_stringp(b))
        return strcmp(sexp_string_data(a), sexp_string_data(b)) == 0;
    if (sexp_symbolp(a) && sexp_symbolp(b))
        return strcmp(sym_cstr(ctx, a), sym_cstr(ctx, b)) == 0;
    if (sexp_flonump(a) && sexp_flonump(b))
        return sexp_flonum_value(a) == sexp_flonum_value(b);
    return 0;
}

/* Grow a dynamic array */
#define GROW_ARRAY(arr, count, cap, type) do { \
    if ((count) >= (cap)) { \
        (cap) = (cap) ? (cap) * 2 : 4; \
        (arr) = (type *)realloc((arr), sizeof(type) * (cap)); \
    } \
} while(0)

/* ---- GC preservation ---- */

static void gc_preserve(ReteEngine *engine, sexp val) {
    if (val && val != SEXP_NULL && val != SEXP_FALSE && val != SEXP_TRUE && val != SEXP_VOID) {
        engine->gc_preserve = sexp_cons(engine->ctx, val, engine->gc_preserve);
    }
}

/* ---- Token management ---- */

static ReteToken *token_create(int pattern_count) {
    ReteToken *t = (ReteToken *)calloc(1, sizeof(ReteToken));
    t->pattern_count = pattern_count;
    t->pattern_facts = (sexp *)calloc(pattern_count, sizeof(sexp));
    return t;
}

static ReteToken *token_extend(ReteToken *parent, sexp new_fact) {
    int new_count = parent ? parent->pattern_count + 1 : 1;
    ReteToken *t = token_create(new_count);
    if (parent) {
        memcpy(t->pattern_facts, parent->pattern_facts,
               parent->pattern_count * sizeof(sexp));
    }
    t->pattern_facts[new_count - 1] = new_fact;
    return t;
}

static void token_free(ReteToken *t) {
    if (t) {
        free(t->pattern_facts);
        free(t);
    }
}

static void token_list_free(ReteToken *head) {
    while (head) {
        ReteToken *next = head->next;
        token_free(head);
        head = next;
    }
}

/* ---- Alpha node ---- */

static AlphaNode *alpha_create(const char *relation, int num_args) {
    AlphaNode *a = (AlphaNode *)calloc(1, sizeof(AlphaNode));
    a->relation = str_dup(relation);
    a->num_args = num_args;
    a->const_tests = (sexp *)calloc(num_args, sizeof(sexp));
    for (int i = 0; i < num_args; i++)
        a->const_tests[i] = SEXP_FALSE;  /* no test */
    return a;
}

static void alpha_free(AlphaNode *a) {
    if (a) {
        free(a->relation);
        free(a->const_tests);
        free(a->facts);
        free(a->children);
        free(a);
    }
}

/* Check if a fact passes all constant tests in an alpha node */
static int alpha_test(ReteEngine *engine, AlphaNode *alpha, sexp args) {
    sexp a = args;
    for (int i = 0; i < alpha->num_args; i++) {
        if (!sexp_pairp(a)) return 0;
        if (alpha->const_tests[i] != SEXP_FALSE) {
            if (!sexp_equal(engine->ctx, sexp_car(a), alpha->const_tests[i]))
                return 0;
        }
        a = sexp_cdr(a);
    }
    return 1;
}

static void alpha_store_fact(AlphaNode *alpha, sexp args) {
    GROW_ARRAY(alpha->facts, alpha->fact_count, alpha->fact_cap, sexp);
    alpha->facts[alpha->fact_count++] = args;
}

/* ---- Beta node ---- */

static BetaNode *beta_create(int type) {
    BetaNode *b = (BetaNode *)calloc(1, sizeof(BetaNode));
    b->node_type = type;
    return b;
}

static void beta_free(BetaNode *b) {
    if (b) {
        free(b->join_tests);
        token_list_free(b->tokens);
        free(b->children);
        free(b->var_locs);
        free(b);
    }
}

static void beta_add_child(BetaNode *parent, BetaNode *child) {
    GROW_ARRAY(parent->children, parent->child_count, parent->child_cap, BetaNode *);
    parent->children[parent->child_count++] = child;
}

static void alpha_add_child(AlphaNode *alpha, BetaNode *child) {
    GROW_ARRAY(alpha->children, alpha->child_count, alpha->child_cap, BetaNode *);
    alpha->children[alpha->child_count++] = child;
}

/* ---- Join tests ---- */

/* Check if token + new_fact satisfy all join tests for a beta node */
static int check_join(ReteEngine *engine, BetaNode *beta,
                      ReteToken *token, sexp new_fact_args) {
    for (int i = 0; i < beta->join_test_count; i++) {
        JoinTest *jt = &beta->join_tests[i];

        /* Value from the new alpha fact */
        sexp alpha_val = list_ref(new_fact_args, jt->alpha_field);

        /* Value from an earlier pattern in the token */
        sexp earlier_fact = token->pattern_facts[jt->token_pattern_idx];
        sexp token_val = list_ref(earlier_fact, jt->token_field);

        if (!sexp_equal(engine->ctx, alpha_val, token_val))
            return 0;
    }
    return 1;
}

/* ---- Forward declarations ---- */
static void beta_right_activate(ReteEngine *engine, BetaNode *beta,
                                ReteToken *token, sexp new_fact_args);

/* ---- Terminal: fire rule action ---- */

static void terminal_fire(ReteEngine *engine, BetaNode *terminal, ReteToken *token) {
    sexp ctx = engine->ctx;
    int nargs = terminal->var_count;

    /* Build argument list from token using var_locs */
    sexp args_list = SEXP_NULL;
    sexp_gc_var1(tmp);
    sexp_gc_preserve1(ctx, tmp);

    /* Build in reverse so cons produces correct order */
    for (int i = nargs - 1; i >= 0; i--) {
        VarLocation *vl = &terminal->var_locs[i];
        sexp fact_args = token->pattern_facts[vl->pattern_idx];
        sexp val = list_ref(fact_args, vl->field_idx);
        args_list = sexp_cons(ctx, val, args_list);
    }

    /* Call action with the extracted bindings */
    tmp = sexp_apply(ctx, terminal->action, args_list);
    (void)tmp;

    sexp_gc_release1(ctx);
}

/* ---- Right-activation: new fact enters from alpha side ---- */

static void beta_right_activate(ReteEngine *engine, BetaNode *beta,
                                ReteToken *parent_token, sexp new_fact_args) {
    /* Create extended token */
    ReteToken *new_token = token_extend(parent_token, new_fact_args);

    /* Store in beta memory */
    new_token->next = beta->tokens;
    beta->tokens = new_token;

    if (beta->node_type == BETA_TERMINAL) {
        terminal_fire(engine, beta, new_token);
        return;
    }

    /* Propagate to child beta nodes */
    for (int c = 0; c < beta->child_count; c++) {
        BetaNode *child = beta->children[c];
        AlphaNode *child_alpha = child->alpha;

        /* Left-activate: try joining new_token with each fact in child's alpha memory */
        for (int f = 0; f < child_alpha->fact_count; f++) {
            sexp alpha_fact = child_alpha->facts[f];
            if (check_join(engine, child, new_token, alpha_fact)) {
                beta_right_activate(engine, child, new_token, alpha_fact);
            }
        }
    }
}

/* ---- Engine creation/destruction ---- */

ReteEngine *rete_create(sexp ctx) {
    ReteEngine *engine = (ReteEngine *)calloc(1, sizeof(ReteEngine));
    engine->ctx = ctx;
    engine->beta_root = beta_create(BETA_ROOT);
    engine->gc_preserve = SEXP_NULL;
    /* Preserve the gc_preserve list itself in the context */
    sexp_gc_var1(tmp);
    sexp_gc_preserve1(ctx, tmp);
    tmp = engine->gc_preserve;
    sexp_gc_release1(ctx);
    return engine;
}

static void alpha_list_free(AlphaNode *head) {
    while (head) {
        AlphaNode *next = head->next;
        alpha_free(head);
        head = next;
    }
}

static void beta_tree_free(BetaNode *node) {
    if (!node) return;
    for (int i = 0; i < node->child_count; i++)
        beta_tree_free(node->children[i]);
    beta_free(node);
}

void rete_destroy(ReteEngine *engine) {
    if (!engine) return;
    alpha_list_free(engine->alpha_nodes);
    beta_tree_free(engine->beta_root);
    free(engine);
}

/* ---- Find or create alpha node ---- */

static AlphaNode *find_or_create_alpha(ReteEngine *engine,
                                       const char *relation, int num_args,
                                       sexp *const_tests) {
    /* Look for existing match */
    for (AlphaNode *a = engine->alpha_nodes; a; a = a->next) {
        if (strcmp(a->relation, relation) == 0 && a->num_args == num_args) {
            int match = 1;
            for (int i = 0; i < num_args; i++) {
                int a_has = (a->const_tests[i] != SEXP_FALSE);
                int b_has = (const_tests[i] != SEXP_FALSE);
                if (a_has != b_has) { match = 0; break; }
                if (a_has && !sexp_equal(engine->ctx, a->const_tests[i], const_tests[i])) {
                    match = 0; break;
                }
            }
            if (match) return a;
        }
    }

    /* Create new */
    AlphaNode *a = alpha_create(relation, num_args);
    for (int i = 0; i < num_args; i++) {
        a->const_tests[i] = const_tests[i];
        if (const_tests[i] != SEXP_FALSE)
            gc_preserve(engine, const_tests[i]);
    }
    a->next = engine->alpha_nodes;
    engine->alpha_nodes = a;
    return a;
}

/* ---- Compile conditions into variable info ---- */

typedef struct {
    char name[64];
    int id;               /* unique id among all vars in the rule */
    int first_pattern;    /* first pattern where this var appears */
    int first_field;      /* field in that pattern */
} VarInfo;

static int find_var(VarInfo *vars, int count, const char *name) {
    for (int i = 0; i < count; i++)
        if (strcmp(vars[i].name, name) == 0) return i;
    return -1;
}

/* ---- Add a forward rule ---- */

void rete_add_rule(ReteEngine *engine, sexp conditions, sexp var_names, sexp action) {
    sexp ctx = engine->ctx;
    int nconds = list_length(conditions);
    if (nconds == 0) return;

    gc_preserve(engine, action);
    gc_preserve(engine, conditions);
    gc_preserve(engine, var_names);

    /* 1. Parse all conditions, collect variable info */
    VarInfo vars[64];
    int var_count = 0;

    /* Also build per-condition data */
    typedef struct {
        const char *relation;
        int num_args;
        int *arg_is_var;      /* 1=var, 0=const */
        int *arg_var_id;      /* var index or -1 */
        sexp *arg_const;      /* const value or SEXP_FALSE */
    } CondInfo;

    CondInfo conds[32];
    sexp cond = conditions;
    for (int ci = 0; ci < nconds && sexp_pairp(cond); ci++, cond = sexp_cdr(cond)) {
        sexp pattern = sexp_car(cond);
        /* pattern = (relation arg1 arg2 ...) */
        sexp rel_sym = sexp_car(pattern);
        const char *relation = sexp_symbolp(rel_sym) ? sym_cstr(ctx, rel_sym) :
                               sexp_stringp(rel_sym) ? sexp_string_data(rel_sym) : "?";
        sexp args = sexp_cdr(pattern);
        int nargs = list_length(args);

        conds[ci].relation = relation;
        conds[ci].num_args = nargs;
        conds[ci].arg_is_var = (int *)calloc(nargs, sizeof(int));
        conds[ci].arg_var_id = (int *)calloc(nargs, sizeof(int));
        conds[ci].arg_const = (sexp *)calloc(nargs, sizeof(sexp));

        sexp a = args;
        for (int ai = 0; ai < nargs && sexp_pairp(a); ai++, a = sexp_cdr(a)) {
            sexp arg = sexp_car(a);
            if (is_rete_var_ctx(ctx, arg)) {
                const char *vname = rete_var_name_ctx(ctx, arg);
                int vid = find_var(vars, var_count, vname);
                if (vid < 0) {
                    vid = var_count;
                    strncpy(vars[vid].name, vname, 63);
                    vars[vid].name[63] = '\0';
                    vars[vid].id = vid;
                    vars[vid].first_pattern = ci;
                    vars[vid].first_field = ai;
                    var_count++;
                }
                conds[ci].arg_is_var[ai] = 1;
                conds[ci].arg_var_id[ai] = vid;
                conds[ci].arg_const[ai] = SEXP_FALSE;
            } else {
                conds[ci].arg_is_var[ai] = 0;
                conds[ci].arg_var_id[ai] = -1;
                conds[ci].arg_const[ai] = arg;
            }
        }
    }

    /* 2. Build alpha/beta network */
    BetaNode *prev_beta = engine->beta_root;

    AlphaNode *alphas[32];

    for (int ci = 0; ci < nconds; ci++) {
        /* Find/create alpha node with constant tests */
        sexp *const_tests = (sexp *)calloc(conds[ci].num_args, sizeof(sexp));
        for (int ai = 0; ai < conds[ci].num_args; ai++) {
            const_tests[ai] = conds[ci].arg_is_var[ai] ? SEXP_FALSE : conds[ci].arg_const[ai];
        }
        AlphaNode *alpha = find_or_create_alpha(engine,
            conds[ci].relation, conds[ci].num_args, const_tests);
        free(const_tests);
        alphas[ci] = alpha;

        /* Create beta join node (or terminal for last condition) */
        int is_last = (ci == nconds - 1);
        BetaNode *beta = beta_create(is_last ? BETA_TERMINAL : BETA_JOIN);
        beta->parent = prev_beta;
        beta->alpha = alpha;

        /* Compute join tests: find variables that appeared in earlier conditions */
        JoinTest join_buf[64];
        int jcount = 0;
        for (int ai = 0; ai < conds[ci].num_args; ai++) {
            if (!conds[ci].arg_is_var[ai]) continue;
            int vid = conds[ci].arg_var_id[ai];
            /* Check if this var appeared in any earlier condition */
            if (vars[vid].first_pattern < ci) {
                join_buf[jcount].alpha_field = ai;
                join_buf[jcount].token_pattern_idx = vars[vid].first_pattern;
                join_buf[jcount].token_field = vars[vid].first_field;
                jcount++;
            }
        }
        if (jcount > 0) {
            beta->join_tests = (JoinTest *)malloc(sizeof(JoinTest) * jcount);
            memcpy(beta->join_tests, join_buf, sizeof(JoinTest) * jcount);
            beta->join_test_count = jcount;
        }

        /* Terminal: set up action and var locations */
        if (is_last) {
            beta->action = action;
            beta->var_count = var_count;
            beta->var_locs = (VarLocation *)calloc(var_count, sizeof(VarLocation));

            /* Map variables in the order they appear in var_names */
            sexp vn = var_names;
            for (int vi = 0; vi < var_count && sexp_pairp(vn); vi++, vn = sexp_cdr(vn)) {
                sexp name_sym = sexp_car(vn);
                const char *name = sexp_symbolp(name_sym) ? sym_cstr(ctx, name_sym) : "";
                int vid = find_var(vars, var_count, name);
                if (vid >= 0) {
                    beta->var_locs[vi].pattern_idx = vars[vid].first_pattern;
                    beta->var_locs[vi].field_idx = vars[vid].first_field;
                } else {
                    beta->var_locs[vi].pattern_idx = 0;
                    beta->var_locs[vi].field_idx = 0;
                }
            }
        }

        /* Link into network */
        beta_add_child(prev_beta, beta);
        alpha_add_child(alpha, beta);

        prev_beta = beta;
    }

    engine->rule_count++;

    /* 3. Populate alpha memories from existing Scheme DB facts.
     * When rules are added after facts, alpha nodes are empty.
     * Query __logic_db_ref__ for each unique relation to seed alpha memories. */
    for (int ci = 0; ci < nconds; ci++) {
        AlphaNode *alpha = alphas[ci];
        if (alpha->fact_count > 0) continue;  /* already populated */

        /* Call (__logic_db_ref__ 'relation) to get existing DB entries */
        sexp rel_sym = sexp_intern(ctx, conds[ci].relation, -1);
        sexp db_ref = sexp_intern(ctx, "__logic_db_ref__", -1);
        sexp call = sexp_list2(ctx, db_ref,
                      sexp_list2(ctx, sexp_intern(ctx, "quote", -1), rel_sym));
        sexp entries = sexp_eval(ctx, call, sexp_context_env(ctx));

        /* entries is a list of (fact arg1 arg2 ...) or (rule ...) */
        for (sexp e = entries; sexp_pairp(e); e = sexp_cdr(e)) {
            sexp entry = sexp_car(e);
            if (sexp_pairp(entry) && sexp_symbolp(sexp_car(entry))) {
                const char *tag = sym_cstr(ctx, sexp_car(entry));
                if (strcmp(tag, "fact") == 0) {
                    sexp fact_args = sexp_cdr(entry);
                    if (alpha_test(engine, alpha, fact_args)) {
                        alpha_store_fact(alpha, fact_args);
                        gc_preserve(engine, fact_args);
                    }
                }
            }
        }
    }

    /* 4. Right-activate with existing facts through the new beta chain */
    BetaNode *first_beta = engine->beta_root->children[engine->beta_root->child_count - 1];
    AlphaNode *first_alpha = alphas[0];

    ReteToken dummy;
    memset(&dummy, 0, sizeof(dummy));

    for (int f = 0; f < first_alpha->fact_count; f++) {
        sexp fact = first_alpha->facts[f];
        beta_right_activate(engine, first_beta, &dummy, fact);
    }

    /* Clean up condition info */
    for (int ci = 0; ci < nconds; ci++) {
        free(conds[ci].arg_is_var);
        free(conds[ci].arg_var_id);
        free(conds[ci].arg_const);
    }
}

/* ---- Propagate a fact ---- */

void rete_propagate(ReteEngine *engine, const char *relation, sexp args) {
    if (!engine || !relation) return;

    gc_preserve(engine, args);

    /* Find all alpha nodes matching this relation */
    for (AlphaNode *alpha = engine->alpha_nodes; alpha; alpha = alpha->next) {
        if (strcmp(alpha->relation, relation) != 0) continue;
        if (alpha->num_args != list_length(args)) continue;
        if (!alpha_test(engine, alpha, args)) continue;

        /* Add to alpha memory */
        alpha_store_fact(alpha, args);

        /* Right-activate child beta nodes */
        for (int c = 0; c < alpha->child_count; c++) {
            BetaNode *beta = alpha->children[c];
            BetaNode *parent = beta->parent;

            if (parent->node_type == BETA_ROOT) {
                /* First condition: no parent tokens to join against.
                 * Use a dummy empty token. */
                ReteToken dummy;
                memset(&dummy, 0, sizeof(dummy));
                beta_right_activate(engine, beta, &dummy, args);
            } else {
                /* Join with existing tokens in parent's beta memory */
                for (ReteToken *tok = parent->tokens; tok; tok = tok->next) {
                    if (check_join(engine, beta, tok, args)) {
                        beta_right_activate(engine, beta, tok, args);
                    }
                }
            }
        }
    }
}

/* ---- Reset ---- */

void rete_reset(ReteEngine *engine) {
    if (!engine) return;
    alpha_list_free(engine->alpha_nodes);
    engine->alpha_nodes = NULL;
    beta_tree_free(engine->beta_root);
    engine->beta_root = beta_create(BETA_ROOT);
    engine->gc_preserve = SEXP_NULL;
    engine->rule_count = 0;
}

/* ======== Bridge functions ======== */

/* Global engine pointer, stored per-context via a Scheme global variable.
 * We use a simple approach: store the engine pointer as a fixnum-encoded
 * address in a global variable __rete_engine__. */

static ReteEngine *get_engine(sexp ctx) {
    sexp env = sexp_context_env(ctx);
    sexp cell = sexp_env_cell(ctx, env, sexp_intern(ctx, "__rete_engine__", -1), 0);
    if (!cell || cell == SEXP_FALSE) return NULL;
    sexp val = sexp_cdr(cell);
    if (val == SEXP_FALSE || val == SEXP_NULL || val == SEXP_VOID) return NULL;
    /* Decode pointer from two fixnums: (lo . hi) */
    if (sexp_pairp(val)) {
        uintptr_t lo = (uintptr_t)sexp_unbox_fixnum(sexp_car(val));
        uintptr_t hi = (uintptr_t)sexp_unbox_fixnum(sexp_cdr(val));
        return (ReteEngine *)(lo | (hi << 32));
    }
    return NULL;
}

static void set_engine(sexp ctx, ReteEngine *engine) {
    sexp env = sexp_context_env(ctx);
    /* Encode pointer as pair of two fixnums for 64-bit safety */
    uintptr_t ptr = (uintptr_t)engine;
    sexp lo = sexp_make_fixnum((sexp_sint_t)(ptr & 0xFFFFFFFF));
    sexp hi = sexp_make_fixnum((sexp_sint_t)((ptr >> 32) & 0xFFFFFFFF));
    sexp val = sexp_cons(ctx, lo, hi);
    sexp sym = sexp_intern(ctx, "__rete_engine__", -1);
    sexp_env_define(ctx, env, sym, val);
}

static ReteEngine *ensure_engine(sexp ctx) {
    ReteEngine *engine = get_engine(ctx);
    if (!engine) {
        engine = rete_create(ctx);
        set_engine(ctx, engine);
    }
    return engine;
}

/* __rete_init__() -> void */
static sexp bridge_rete_init(sexp ctx, sexp self, sexp_sint_t n) {
    ensure_engine(ctx);
    return SEXP_VOID;
}

/* __rete_add_rule__(conditions, var_names, action) */
static sexp bridge_rete_add_rule(sexp ctx, sexp self, sexp_sint_t n,
                                 sexp conditions, sexp var_names, sexp action) {
    ReteEngine *engine = ensure_engine(ctx);
    rete_add_rule(engine, conditions, var_names, action);
    return SEXP_VOID;
}

/* __rete_propagate__(relation_sym, args) */
static sexp bridge_rete_propagate(sexp ctx, sexp self, sexp_sint_t n,
                                  sexp relation, sexp args) {
    ReteEngine *engine = get_engine(ctx);
    if (!engine) return SEXP_VOID;  /* no-op if no rules registered */
    if (engine->rule_count == 0) return SEXP_VOID;

    const char *rel = NULL;
    if (sexp_symbolp(relation))
        rel = sym_cstr(ctx, relation);
    else if (sexp_stringp(relation))
        rel = sexp_string_data(relation);
    else
        return SEXP_VOID;

    rete_propagate(engine, rel, args);
    return SEXP_VOID;
}

/* __rete_reset__() */
static sexp bridge_rete_reset(sexp ctx, sexp self, sexp_sint_t n) {
    ReteEngine *engine = get_engine(ctx);
    if (engine) rete_reset(engine);
    return SEXP_VOID;
}

/* __rete_rule_count__() */
static sexp bridge_rete_rule_count(sexp ctx, sexp self, sexp_sint_t n) {
    ReteEngine *engine = get_engine(ctx);
    return sexp_make_fixnum(engine ? engine->rule_count : 0);
}

/* ---- Registration ---- */

void register_rete_bridge_functions(sexp ctx, sexp env) {
    sexp_define_foreign(ctx, env, "__rete_init__", 0, bridge_rete_init);
    sexp_define_foreign(ctx, env, "__rete_add_rule__", 3, bridge_rete_add_rule);
    sexp_define_foreign(ctx, env, "__rete_propagate__", 2, bridge_rete_propagate);
    sexp_define_foreign(ctx, env, "__rete_reset__", 0, bridge_rete_reset);
    sexp_define_foreign(ctx, env, "__rete_rule_count__", 0, bridge_rete_rule_count);
}
