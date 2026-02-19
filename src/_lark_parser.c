/*
 * _lark_parser.c — Lark grammar data structures and helpers.
 *
 * Provides construction/destruction of LarkGrammar and the
 * lark_parse_grammar() entry point that drives the lemon-generated parser.
 */
#include "_lark_parser.h"
#include "_lark_lexer.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Forward declarations for lemon-generated parser (from lark_grammar.c) */
void* LarkParseAlloc(void* (*allocProc)(size_t));
void  LarkParse(void* parser, int token, LarkToken value, LarkParserState* state);
void  LarkParseFree(void* parser, void (*freeProc)(void*));

/* ---- Grammar construction ---- */

LarkGrammar* lark_grammar_new(void) {
    LarkGrammar* g = (LarkGrammar*)calloc(1, sizeof(LarkGrammar));
    return g;
}

LarkRule* lark_grammar_add_rule(LarkGrammar* g, const char* name, int is_inline) {
    /* Check if rule already exists (multiple alternatives add to same rule) */
    for (int i = 0; i < g->num_rules; i++) {
        if (strcmp(g->rules[i].name, name) == 0) {
            return &g->rules[i];
        }
    }
    if (g->num_rules >= g->rules_alloc) {
        g->rules_alloc = g->rules_alloc ? g->rules_alloc * 2 : 16;
        g->rules = (LarkRule*)realloc(g->rules, g->rules_alloc * sizeof(LarkRule));
    }
    LarkRule* r = &g->rules[g->num_rules++];
    memset(r, 0, sizeof(LarkRule));
    r->name = strdup(name);
    r->is_inline = is_inline;
    if (!g->start_rule) {
        g->start_rule = strdup(name);
    }
    return r;
}

LarkAlternative* lark_rule_add_alt(LarkRule* r) {
    if (r->num_alts >= r->alts_alloc) {
        r->alts_alloc = r->alts_alloc ? r->alts_alloc * 2 : 8;
        r->alts = (LarkAlternative*)realloc(r->alts, r->alts_alloc * sizeof(LarkAlternative));
    }
    LarkAlternative* alt = &r->alts[r->num_alts++];
    memset(alt, 0, sizeof(LarkAlternative));
    return alt;
}

LarkItem* lark_alt_add_item(LarkAlternative* alt, LarkItemType type,
                             const char* value, LarkModifier mod) {
    if (alt->num_items >= alt->items_alloc) {
        alt->items_alloc = alt->items_alloc ? alt->items_alloc * 2 : 8;
        alt->items = (LarkItem*)realloc(alt->items, alt->items_alloc * sizeof(LarkItem));
    }
    LarkItem* item = &alt->items[alt->num_items++];
    memset(item, 0, sizeof(LarkItem));
    item->type = type;
    item->value = value ? strdup(value) : NULL;
    item->modifier = mod;
    return item;
}

void lark_grammar_add_terminal(LarkGrammar* g, const char* name,
                                const char* pattern, int is_regex, int priority) {
    if (g->num_terminals >= g->terminals_alloc) {
        g->terminals_alloc = g->terminals_alloc ? g->terminals_alloc * 2 : 16;
        g->terminals = (LarkTerminal*)realloc(g->terminals,
            g->terminals_alloc * sizeof(LarkTerminal));
    }
    LarkTerminal* t = &g->terminals[g->num_terminals++];
    t->name = strdup(name);
    t->pattern = strdup(pattern);
    t->is_regex = is_regex;
    t->priority = priority;
}

void lark_grammar_add_ignore(LarkGrammar* g, const char* pattern) {
    if (g->num_ignore >= g->ignore_alloc) {
        g->ignore_alloc = g->ignore_alloc ? g->ignore_alloc * 2 : 8;
        g->ignore_patterns = (char**)realloc(g->ignore_patterns,
            g->ignore_alloc * sizeof(char*));
    }
    g->ignore_patterns[g->num_ignore++] = strdup(pattern);
}

/* ---- Grammar destruction ---- */

static void free_alt(LarkAlternative* alt) {
    for (int i = 0; i < alt->num_items; i++) {
        free(alt->items[i].value);
        if (alt->items[i].group_alts) {
            for (int j = 0; j < alt->items[i].num_group_alts; j++) {
                free_alt(&alt->items[i].group_alts[j]);
            }
            free(alt->items[i].group_alts);
        }
    }
    free(alt->items);
    free(alt->alias);
}

void lark_grammar_free(LarkGrammar* g) {
    if (!g) return;
    for (int i = 0; i < g->num_rules; i++) {
        free(g->rules[i].name);
        for (int j = 0; j < g->rules[i].num_alts; j++) {
            free_alt(&g->rules[i].alts[j]);
        }
        free(g->rules[i].alts);
    }
    free(g->rules);
    for (int i = 0; i < g->num_terminals; i++) {
        free(g->terminals[i].name);
        free(g->terminals[i].pattern);
    }
    free(g->terminals);
    for (int i = 0; i < g->num_ignore; i++) {
        free(g->ignore_patterns[i]);
    }
    free(g->ignore_patterns);
    free(g->start_rule);
    free(g);
}

/* ---- Parse entry point ---- */

LarkGrammar* lark_parse_grammar(const char* source, char** error_msg) {
    LarkLexer lexer;
    LarkToken token;
    LarkParserState state;
    void* parser;
    int tok;

    if (!source) {
        if (error_msg) *error_msg = strdup("NULL source");
        return NULL;
    }

    /* Initialize parser state */
    memset(&state, 0, sizeof(state));
    state.grammar = lark_grammar_new();
    if (!state.grammar) {
        if (error_msg) *error_msg = strdup("Out of memory");
        return NULL;
    }

    /* Initialize lexer */
    lark_lexer_init(&lexer, source);

    /* Create lemon parser */
    parser = LarkParseAlloc(malloc);
    if (!parser) {
        lark_grammar_free(state.grammar);
        if (error_msg) *error_msg = strdup("Out of memory");
        return NULL;
    }

    /* Feed tokens to parser */
    while ((tok = lark_lexer_next(&lexer, &token)) != 0) {
        if (tok == -1) {
            /* Lexer error */
            state.has_error = 1;
            snprintf(state.error_msg, sizeof(state.error_msg),
                     "Lexer error at line %d: %s", lexer.line,
                     lexer.error_msg ? lexer.error_msg : "unknown");
            break;
        }
        LarkParse(parser, tok, token, &state);
        if (state.has_error) break;
    }

    /* Send EOF */
    if (!state.has_error) {
        memset(&token, 0, sizeof(token));
        LarkParse(parser, 0, token, &state);
    }

    LarkParseFree(parser, free);
    lark_lexer_free(&lexer);

    if (state.has_error) {
        if (error_msg) *error_msg = strdup(state.error_msg);
        lark_grammar_free(state.grammar);
        return NULL;
    }

    return state.grammar;
}
