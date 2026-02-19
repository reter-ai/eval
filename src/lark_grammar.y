/*
 * lark_grammar.y — Lemon grammar for parsing Lark EBNF format.
 *
 * Parses Lark-style grammar definitions into LarkGrammar structs.
 * Generated symbols: LarkParseAlloc, LarkParse, LarkParseFree.
 *
 * Key LALR trick: rule_header reduces when "NAME :" is seen,
 * creating the rule BEFORE alternatives are processed. Then
 * pipe_start reduces on "|" to create new alternatives.
 * Items add to the current alternative via static _ca pointer.
 */

%name LarkParse
%token_type { LarkToken }
%default_type { int }
%extra_argument { LarkParserState* state }
%token_prefix LTOK_

%include {
#include "_lark_parser.h"
#include "_lark_lexer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define G (state->grammar)

/* Current rule/alternative being built (set by reduce actions) */
static LarkRule* _cr;
static LarkAlternative* _ca;

/* Group save/restore stack for parenthesized groups like (A | B)* */
#define MAX_GROUP_DEPTH 8
static struct { LarkRule* cr; LarkAlternative* ca; } _grp_stk[MAX_GROUP_DEPTH];
static int _grp_depth = 0;
static LarkRule _grp_tmp[MAX_GROUP_DEPTH];

/* Terminal body: accumulated pattern for terminal alternatives */
typedef struct {
    char* pattern;
    int is_regex;
} TermBody;

/* Escape regex metacharacters in a literal string */
static char* regex_escape_literal(const char* lit) {
    int len = (int)strlen(lit);
    char* out = (char*)malloc(len * 2 + 1);
    int j = 0;
    for (int i = 0; i < len; i++) {
        switch (lit[i]) {
            case '.': case '*': case '+': case '?': case '(':
            case ')': case '[': case ']': case '{': case '}':
            case '|': case '^': case '$': case '\\':
                out[j++] = '\\';
                /* fall through */
            default:
                out[j++] = lit[i];
        }
    }
    out[j] = '\0';
    return out;
}

/* Combine two patterns into regex alternation: (left|right) */
static char* combine_patterns(const char* left, int left_is_regex,
                               const char* right, int right_is_regex) {
    char* l = left_is_regex ? strdup(left) : regex_escape_literal(left);
    char* r = right_is_regex ? strdup(right) : regex_escape_literal(right);
    int llen = (int)strlen(l);
    int rlen = (int)strlen(r);
    char* result = (char*)malloc(llen + rlen + 4);
    result[0] = '(';
    memcpy(result + 1, l, llen);
    result[1 + llen] = '|';
    memcpy(result + 2 + llen, r, rlen);
    result[2 + llen + rlen] = ')';
    result[3 + llen + rlen] = '\0';
    free(l);
    free(r);
    return result;
}
}

%syntax_error {
    state->has_error = 1;
    snprintf(state->error_msg, sizeof(state->error_msg),
             "Lark syntax error at line %d", TOKEN.line);
}

/* ---- Tokens ---- */
%token RULE_NAME TERM_NAME STRING REGEX ARROW COLON PIPE.
%token QUESTION STAR PLUS LPAREN RPAREN DOT NUMBER.
%token PERCENT_IGNORE NEWLINE TILDE.

/* ====================== Grammar Structure ====================== */

grammar ::= lines.

lines ::= lines line.
lines ::= .

line ::= rule_def NEWLINE.
line ::= term_def NEWLINE.
line ::= ignore_def NEWLINE.
line ::= NEWLINE.
/* Last definition may lack trailing NEWLINE (EOF) */
line ::= rule_def.
line ::= term_def.
line ::= ignore_def.

/* ====================== Rule Definitions ====================== */

/* rule_header reduces on "name :" — creates the rule and first alt */
rule_def ::= rule_header alts.

rule_header ::= RULE_NAME(N) COLON. {
    _cr = lark_grammar_add_rule(G, N.str_value, 0);
    _ca = lark_rule_add_alt(_cr);
}

rule_header ::= QUESTION RULE_NAME(N) COLON. {
    _cr = lark_grammar_add_rule(G, N.str_value, 1);
    _ca = lark_rule_add_alt(_cr);
}

/* ====================== Alternatives ====================== */

alts ::= alt.
alts ::= alts pipe_start alt.

/* pipe_start creates a new alternative for the current rule */
pipe_start ::= PIPE. {
    if (_cr) _ca = lark_rule_add_alt(_cr);
}

/* An alternative is a sequence of items, optionally followed by -> alias */
alt ::= items.
alt ::= items ARROW RULE_NAME(N). {
    if (_ca) {
        free(_ca->alias);
        _ca->alias = N.str_value ? strdup(N.str_value) : NULL;
    }
}
/* Empty alternative: pipe_start already created the alt with 0 items. */
alt ::= .

/* ====================== Items ====================== */

items ::= items item.
items ::= item.

/* Each item is an atom with an optional EBNF modifier */
item ::= atom_base.
item ::= atom_base QUESTION. {
    if (_ca && _ca->num_items > 0)
        _ca->items[_ca->num_items - 1].modifier = LARK_MOD_OPT;
}
item ::= atom_base STAR. {
    if (_ca && _ca->num_items > 0)
        _ca->items[_ca->num_items - 1].modifier = LARK_MOD_STAR;
}
item ::= atom_base PLUS. {
    if (_ca && _ca->num_items > 0)
        _ca->items[_ca->num_items - 1].modifier = LARK_MOD_PLUS;
}

/* ====================== Atoms ====================== */

/* atom_base adds an item to the current alternative */
atom_base ::= RULE_NAME(N). {
    if (_ca) lark_alt_add_item(_ca, LARK_ITEM_RULE, N.str_value, LARK_MOD_NONE);
}

atom_base ::= TERM_NAME(N). {
    if (_ca) lark_alt_add_item(_ca, LARK_ITEM_TERM, N.str_value, LARK_MOD_NONE);
}

atom_base ::= STRING(S). {
    if (_ca) lark_alt_add_item(_ca, LARK_ITEM_LITERAL, S.str_value, LARK_MOD_NONE);
}

/* Grouping: (...) — save/restore context so alts build on a temp rule.
 * group_start is a mid-rule action that pushes context onto _grp_stk. */
group_start ::= LPAREN. {
    _grp_stk[_grp_depth].cr = _cr;
    _grp_stk[_grp_depth].ca = _ca;
    memset(&_grp_tmp[_grp_depth], 0, sizeof(LarkRule));
    _cr = &_grp_tmp[_grp_depth];
    _ca = lark_rule_add_alt(_cr);
    _grp_depth++;
}

atom_base ::= group_start alts RPAREN. {
    _grp_depth--;
    LarkRule* tmp = &_grp_tmp[_grp_depth];
    _cr = _grp_stk[_grp_depth].cr;
    _ca = _grp_stk[_grp_depth].ca;
    if (_ca) {
        LarkItem* grp = lark_alt_add_item(_ca, LARK_ITEM_GROUP, NULL, LARK_MOD_NONE);
        grp->group_alts = tmp->alts;
        grp->num_group_alts = tmp->num_alts;
    }
}

/* ====================== Terminal Definitions ====================== */

%type term_body { TermBody }
%destructor term_body { free($$.pattern); }

term_def ::= TERM_NAME(N) COLON term_body(B). {
    lark_grammar_add_terminal(G, N.str_value, B.pattern, B.is_regex, 0);
    free(B.pattern);
}

term_def ::= TERM_NAME(N) DOT NUMBER(P) COLON term_body(B). {
    int prio = 0;
    if (P.start) {
        const char* pp;
        for (pp = P.start; pp < P.start + P.length; pp++)
            prio = prio * 10 + (*pp - '0');
    }
    lark_grammar_add_terminal(G, N.str_value, B.pattern, B.is_regex, prio);
    free(B.pattern);
}

/* Single pattern */
term_body(A) ::= REGEX(R). {
    A.pattern = R.str_value;
    R.str_value = NULL;
    A.is_regex = 1;
}

term_body(A) ::= STRING(S). {
    A.pattern = S.str_value;
    S.str_value = NULL;
    A.is_regex = 0;
}

/* Terminal alternatives: VALUE: /pat1/ | /pat2/ | "lit" */
term_body(A) ::= term_body(L) PIPE REGEX(R). {
    A.pattern = combine_patterns(L.pattern, L.is_regex, R.str_value, 1);
    A.is_regex = 1;
    free(L.pattern);
    free(R.str_value);
}

term_body(A) ::= term_body(L) PIPE STRING(S). {
    A.pattern = combine_patterns(L.pattern, L.is_regex, S.str_value, 0);
    A.is_regex = 1;
    free(L.pattern);
    free(S.str_value);
}

/* ====================== Ignore Directive ====================== */

ignore_def ::= PERCENT_IGNORE REGEX(R). {
    lark_grammar_add_ignore(G, R.str_value);
}

ignore_def ::= PERCENT_IGNORE STRING(S). {
    lark_grammar_add_ignore(G, S.str_value);
}
