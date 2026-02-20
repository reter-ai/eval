/*  _eval_rete.h -- Rete forward-chaining engine for Eval
 *
 *  Complements the existing backward-chaining miniKanren system.
 *  When facts are asserted, they propagate through an alpha/beta
 *  network and fire matching "whenever" rules.
 */

#ifndef EVAL_RETE_H
#define EVAL_RETE_H

#include <chibi/eval.h>

/* ---- Alpha node: filters facts by relation + constant tests ---- */
typedef struct AlphaNode {
    char *relation;           /* e.g. "parent" */
    int num_args;
    sexp *const_tests;        /* array[num_args]: NULL=any, else required constant */

    /* Alpha memory: facts that passed all tests */
    sexp *facts;              /* dynamic array of (arg0 arg1 ...) lists */
    int fact_count, fact_cap;

    /* Child beta nodes (right-activated on new facts) */
    struct BetaNode **children;
    int child_count, child_cap;

    struct AlphaNode *next;   /* chain in engine */
} AlphaNode;

/* ---- Join test: variable equality across patterns ---- */
typedef struct {
    int alpha_field;          /* field in the new (alpha) fact */
    int token_pattern_idx;    /* which earlier pattern in the token */
    int token_field;          /* which field in that pattern */
} JoinTest;

/* ---- Token: partial match through beta network ---- */
typedef struct ReteToken {
    sexp *pattern_facts;      /* array: one fact-arg-list per condition matched */
    int pattern_count;
    struct ReteToken *next;
} ReteToken;

/* ---- Variable location for extracting bindings at terminal ---- */
typedef struct {
    int pattern_idx;          /* which pattern */
    int field_idx;            /* which field in that pattern */
} VarLocation;

/* ---- Beta node types ---- */
enum { BETA_ROOT = 0, BETA_JOIN = 1, BETA_TERMINAL = 2 };

/* ---- Beta node: joins alpha memory with parent tokens ---- */
typedef struct BetaNode {
    int node_type;

    struct BetaNode *parent;
    AlphaNode *alpha;         /* alpha memory to join against */

    JoinTest *join_tests;
    int join_test_count;

    ReteToken *tokens;        /* beta memory (partial matches) */

    struct BetaNode **children;
    int child_count, child_cap;

    /* Terminal node fields */
    sexp action;              /* Scheme lambda to call */
    VarLocation *var_locs;    /* for each variable: pattern+field to extract */
    int var_count;
} BetaNode;

/* ---- Engine (one per context) ---- */
typedef struct ReteEngine {
    AlphaNode *alpha_nodes;   /* linked list of all alpha nodes */
    BetaNode *beta_root;      /* dummy root node */
    sexp gc_preserve;         /* GC preservation list for sexp values */
    sexp ctx;                 /* chibi context (for calling actions) */
    int rule_count;
    int propagating;          /* re-entrancy guard (for chaining) */
} ReteEngine;

/* ---- Public API ---- */

/* Create/destroy engine */
ReteEngine *rete_create(sexp ctx);
void rete_destroy(ReteEngine *engine);

/* Add a forward rule.
 * conditions: Scheme list of (relation arg1 arg2 ...) where args are
 *   either '__var_X symbols (pattern vars) or constant values.
 * var_names: Scheme list of symbols naming the variables.
 * action: Scheme lambda taking var_count arguments. */
void rete_add_rule(ReteEngine *engine, sexp conditions, sexp var_names, sexp action);

/* Propagate a new fact through the network.
 * relation: C string (relation name).
 * args: Scheme list of argument values. */
void rete_propagate(ReteEngine *engine, const char *relation, sexp args);

/* Reset: clear all rules and memories */
void rete_reset(ReteEngine *engine);

/* ---- Bridge function registration ---- */
void register_rete_bridge_functions(sexp ctx, sexp env);

#endif /* EVAL_RETE_H */
