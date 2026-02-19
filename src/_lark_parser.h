/*
 * _lark_parser.h — Lark EBNF grammar data structures.
 *
 * Represents a parsed Lark grammar: rules, terminals, ignore patterns.
 * Populated by the lemon-generated Lark parser (lark_grammar.y).
 */
#ifndef LARK_PARSER_H
#define LARK_PARSER_H

#ifdef __cplusplus
extern "C" {
#endif

/* ---- Items within a rule alternative ---- */

typedef enum {
    LARK_ITEM_RULE,      /* reference to another rule (lowercase name) */
    LARK_ITEM_TERM,      /* reference to a terminal (UPPERCASE name) */
    LARK_ITEM_LITERAL,   /* inline string literal "+" */
    LARK_ITEM_GROUP      /* parenthesized group (a | b) */
} LarkItemType;

typedef enum {
    LARK_MOD_NONE,       /* no modifier */
    LARK_MOD_OPT,        /* ? (zero or one) */
    LARK_MOD_STAR,       /* * (zero or more) */
    LARK_MOD_PLUS        /* + (one or more) */
} LarkModifier;

typedef struct LarkAlternative LarkAlternative;

typedef struct LarkItem {
    LarkItemType type;
    char* value;                    /* rule/term name or literal text */
    LarkModifier modifier;
    LarkAlternative* group_alts;    /* for LARK_ITEM_GROUP */
    int num_group_alts;
} LarkItem;

/* ---- Alternatives and rules ---- */

struct LarkAlternative {
    char* alias;            /* "add" from ->, or NULL */
    LarkItem* items;
    int num_items;
    int items_alloc;
};

typedef struct LarkRule {
    char* name;
    int is_inline;          /* ?rule */
    LarkAlternative* alts;
    int num_alts;
    int alts_alloc;
} LarkRule;

/* ---- Terminals ---- */

typedef struct LarkTerminal {
    char* name;             /* "NUMBER" */
    char* pattern;          /* "[0-9]+" (stripped of /.../ delimiters) */
    int is_regex;           /* 1 for /.../, 0 for "..." string literal */
    int priority;           /* from NAME.N: syntax */
} LarkTerminal;

/* ---- Complete grammar ---- */

typedef struct LarkGrammar {
    LarkRule* rules;
    int num_rules;
    int rules_alloc;

    LarkTerminal* terminals;
    int num_terminals;
    int terminals_alloc;

    char** ignore_patterns;
    int num_ignore;
    int ignore_alloc;

    char* start_rule;       /* name of the first rule */
} LarkGrammar;

/* ---- Parser state (passed as %extra_argument) ---- */

typedef struct LarkParserState {
    LarkGrammar* grammar;
    char error_msg[512];
    int error_line;
    int has_error;
} LarkParserState;

/* ---- API ---- */

/* Parse a Lark grammar string into a LarkGrammar struct.
 * Returns NULL on error (error message in *error_msg if provided). */
LarkGrammar* lark_parse_grammar(const char* source, char** error_msg);

/* Free a LarkGrammar and all its contents. */
void lark_grammar_free(LarkGrammar* g);

/* ---- Helper functions used by parser actions ---- */

LarkGrammar* lark_grammar_new(void);
LarkRule* lark_grammar_add_rule(LarkGrammar* g, const char* name, int is_inline);
LarkAlternative* lark_rule_add_alt(LarkRule* r);
LarkItem* lark_alt_add_item(LarkAlternative* alt, LarkItemType type,
                             const char* value, LarkModifier mod);
void lark_grammar_add_terminal(LarkGrammar* g, const char* name,
                                const char* pattern, int is_regex, int priority);
void lark_grammar_add_ignore(LarkGrammar* g, const char* pattern);

#ifdef __cplusplus
}
#endif

#endif /* LARK_PARSER_H */
