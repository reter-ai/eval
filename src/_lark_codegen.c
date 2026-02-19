/*
 * _lark_codegen.c — Code generation from LarkGrammar → C source.
 *
 * Converts parsed Lark grammar into:
 *   1. Lemon .y string (BNF rules + AST-building actions)
 *   2. Lexer C code (longest-match token scanner)
 *   3. Combined C source for JIT compilation
 *
 * EBNF → BNF desugaring:
 *   item?  → xrule_opt_N : item | ;
 *   item*  → xrule_star_N : xrule_star_N item | ;
 *   item+  → xrule_plus_N : xrule_plus_N item | item ;
 *   (a|b)  → __group_N : a | b ;
 */
#include "_lark_codegen.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

/* ---- String builder ---- */
typedef struct {
    char* data;
    int used;
    int alloc;
} SBuf;

static void sb_init(SBuf* sb) {
    sb->alloc = 8192;
    sb->data = (char*)malloc(sb->alloc);
    sb->data[0] = 0;
    sb->used = 0;
}

static void sb_ensure(SBuf* sb, int extra) {
    if (sb->used + extra + 1 > sb->alloc) {
        while (sb->used + extra + 1 > sb->alloc) sb->alloc *= 2;
        sb->data = (char*)realloc(sb->data, sb->alloc);
    }
}

static void sb_append(SBuf* sb, const char* s) {
    int len = (int)strlen(s);
    sb_ensure(sb, len);
    memcpy(sb->data + sb->used, s, len + 1);
    sb->used += len;
}

static void sb_printf(SBuf* sb, const char* fmt, ...) {
    char buf[4096];
    va_list ap;
    va_start(ap, fmt);
    int n = vsnprintf(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    if (n > 0 && n < (int)sizeof(buf)) {
        sb_ensure(sb, n);
        memcpy(sb->data + sb->used, buf, n + 1);
        sb->used += n;
    }
}

static char* sb_detach(SBuf* sb) {
    char* s = sb->data;
    sb->data = NULL;
    sb->used = sb->alloc = 0;
    return s;
}

/* ---- Helper: generate unique name for EBNF desugaring ---- */
static int _desugar_counter = 0;

static void reset_desugar_counter(void) {
    _desugar_counter = 0;
}

/* ---- Collect all anonymous literals and assign token names ---- */

typedef struct {
    char* literal;     /* e.g. "+" */
    char* token_name;  /* e.g. "ANON_PLUS" or "ANON_0" */
} AnonLiteral;

static AnonLiteral* _anon_lits = NULL;
static int _num_anon = 0;
static int _anon_alloc = 0;

static void reset_anon_lits(void) {
    for (int i = 0; i < _num_anon; i++) {
        free(_anon_lits[i].literal);
        free(_anon_lits[i].token_name);
    }
    free(_anon_lits);
    _anon_lits = NULL;
    _num_anon = 0;
    _anon_alloc = 0;
}

static const char* get_anon_token_name(const char* literal) {
    /* Check if already registered */
    for (int i = 0; i < _num_anon; i++) {
        if (strcmp(_anon_lits[i].literal, literal) == 0)
            return _anon_lits[i].token_name;
    }
    /* Create new */
    if (_num_anon >= _anon_alloc) {
        _anon_alloc = _anon_alloc ? _anon_alloc * 2 : 16;
        _anon_lits = (AnonLiteral*)realloc(_anon_lits, _anon_alloc * sizeof(AnonLiteral));
    }
    char name[64];
    /* Try to make a readable name */
    if (strlen(literal) == 1 && isalnum((unsigned char)literal[0])) {
        snprintf(name, sizeof(name), "ANON_%c", literal[0]);
    } else {
        snprintf(name, sizeof(name), "ANON_%d", _num_anon);
    }
    _anon_lits[_num_anon].literal = strdup(literal);
    _anon_lits[_num_anon].token_name = strdup(name);
    _num_anon++;
    return _anon_lits[_num_anon - 1].token_name;
}

/* Scan grammar for all anonymous literals (after group desugaring,
 * so no need to recurse into group_alts) */
static void collect_anon_literals(LarkGrammar* g) {
    for (int r = 0; r < g->num_rules; r++) {
        for (int a = 0; a < g->rules[r].num_alts; a++) {
            LarkAlternative* alt = &g->rules[r].alts[a];
            for (int i = 0; i < alt->num_items; i++) {
                if (alt->items[i].type == LARK_ITEM_LITERAL) {
                    get_anon_token_name(alt->items[i].value);
                }
            }
        }
    }
}

/* ---- Group desugaring: (A | B) → synthetic inline rule __grp_N ---- */

static int _group_rule_counter = 0;

static void desugar_groups_in_alt(LarkGrammar* g, LarkAlternative* alt) {
    for (int i = 0; i < alt->num_items; i++) {
        LarkItem* item = &alt->items[i];
        if (item->type == LARK_ITEM_GROUP && item->group_alts) {
            char name[64];
            snprintf(name, sizeof(name), "__grp_%d", _group_rule_counter++);
            /* Add synthetic inline rule */
            LarkRule* rule = lark_grammar_add_rule(g, name, 1);
            rule->alts = item->group_alts;
            rule->num_alts = item->num_group_alts;
            rule->alts_alloc = item->num_group_alts;
            /* Recursively desugar nested groups in the new rule.
             * Re-fetch item pointer since realloc may have moved g->rules
             * which doesn't affect alt (it's in a different allocation),
             * but the rule's alts are the group_alts we just transferred. */
            for (int j = 0; j < rule->num_alts; j++)
                desugar_groups_in_alt(g, &rule->alts[j]);
            /* Replace group item with rule reference.
             * Must re-derive item pointer — alt->items is stable (no realloc here)
             * but be safe. */
            item = &alt->items[i];
            item->type = LARK_ITEM_RULE;
            item->value = strdup(name);
            item->group_alts = NULL;
            item->num_group_alts = 0;
            /* Modifier preserved — e.g. (A|B)* keeps LARK_MOD_STAR */
        }
    }
}

void lark_desugar_groups(LarkGrammar* g) {
    _group_rule_counter = 0;
    int orig_count = g->num_rules;
    for (int r = 0; r < orig_count; r++)
        for (int a = 0; a < g->rules[r].num_alts; a++)
            desugar_groups_in_alt(g, &g->rules[r].alts[a]);
}

/* ---- Get the lemon token name for an item ---- */
static const char* item_token_name(LarkItem* item) {
    switch (item->type) {
        case LARK_ITEM_TERM: return item->value;
        case LARK_ITEM_LITERAL: return get_anon_token_name(item->value);
        case LARK_ITEM_RULE: return item->value;
        default: return "UNKNOWN";
    }
}

/* ---- EBNF desugaring: emit helper rules ---- */
static void desugar_item(SBuf* sb, LarkItem* item, const char* base_name) {
    if (item->modifier == LARK_MOD_NONE) return;

    char helper_name[128];
    int id = _desugar_counter++;

    switch (item->modifier) {
        case LARK_MOD_OPT:
            snprintf(helper_name, sizeof(helper_name), "x%s_opt_%d", base_name, id);
            sb_printf(sb, "%s(A) ::= %s(B). { A = ast_rule(s, \"list\", 1, B); }\n",
                      helper_name, item_token_name(item));
            sb_printf(sb, "%s(A) ::= . { A = ast_rule(s, \"list\", 0); }\n",
                      helper_name);
            break;
        case LARK_MOD_STAR:
            snprintf(helper_name, sizeof(helper_name), "x%s_star_%d", base_name, id);
            sb_printf(sb, "%s(A) ::= %s(B) %s(C). { A = ast_append(s, B, C); }\n",
                      helper_name, helper_name, item_token_name(item));
            sb_printf(sb, "%s(A) ::= . { A = ast_rule(s, \"list\", 0); }\n",
                      helper_name);
            break;
        case LARK_MOD_PLUS:
            snprintf(helper_name, sizeof(helper_name), "x%s_plus_%d", base_name, id);
            sb_printf(sb, "%s(A) ::= %s(B) %s(C). { A = ast_append(s, B, C); }\n",
                      helper_name, helper_name, item_token_name(item));
            sb_printf(sb, "%s(A) ::= %s(B). { A = ast_rule(s, \"list\", 1, B); }\n",
                      helper_name, item_token_name(item));
            break;
        default:
            break;
    }
}

/* ---- Generate lemon .y ---- */

char* lark_generate_lemon_y(LarkGrammar* grammar, const char* parser_prefix) {
    SBuf sb;
    int r, a, i;

    sb_init(&sb);
    reset_desugar_counter();
    reset_anon_lits();
    collect_anon_literals(grammar);

    /* Header — no %name: each grammar gets its own JIT dylib so no symbol clash.
     * The parse function is prefixed externally by the combined C wrapper. */
    sb_append(&sb, "%token_type { AstNode* }\n");
    sb_append(&sb, "%default_type { AstNode* }\n");
    sb_printf(&sb, "%%extra_argument { GrammarParseState* s }\n");
    sb_printf(&sb, "%%token_prefix %s_TOK_\n\n", parser_prefix);

    /* Include: AST helpers */
    sb_append(&sb, "%include {\n");
    sb_append(&sb, "/* Forward declarations for AST helpers */\n");
    sb_append(&sb, "typedef struct AstNode AstNode;\n");
    sb_append(&sb, "typedef struct GrammarParseState GrammarParseState;\n");
    sb_append(&sb, "extern AstNode* ast_token(GrammarParseState* s, const char* name, AstNode* tok);\n");
    sb_append(&sb, "extern AstNode* ast_rule(GrammarParseState* s, const char* name, int n, ...);\n");
    sb_append(&sb, "extern AstNode* ast_append(GrammarParseState* s, AstNode* list, AstNode* item);\n");
    sb_append(&sb, "}\n\n");

    /* Syntax error handler */
    sb_append(&sb, "%syntax_error {\n");
    sb_append(&sb, "    s->has_error = 1;\n");
    sb_append(&sb, "}\n\n");

    /* Token declarations */
    sb_append(&sb, "/* Terminals */\n");
    for (i = 0; i < grammar->num_terminals; i++) {
        sb_printf(&sb, "%%token %s.\n", grammar->terminals[i].name);
    }
    /* Anonymous literals */
    for (i = 0; i < _num_anon; i++) {
        sb_printf(&sb, "%%token %s. /* \"%s\" */\n",
                  _anon_lits[i].token_name, _anon_lits[i].literal);
    }
    sb_append(&sb, "\n");

    /* First pass: emit EBNF desugaring helper rules */
    for (r = 0; r < grammar->num_rules; r++) {
        LarkRule* rule = &grammar->rules[r];
        for (a = 0; a < rule->num_alts; a++) {
            LarkAlternative* alt = &rule->alts[a];
            for (i = 0; i < alt->num_items; i++) {
                if (alt->items[i].modifier != LARK_MOD_NONE) {
                    desugar_item(&sb, &alt->items[i], rule->name);
                }
            }
        }
    }

    /* Emit wrapper start rule: lkstart ::= <first_rule>.
     * Lemon requires the start symbol to not appear on any RHS. */
    if (grammar->num_rules > 0) {
        sb_printf(&sb, "lkstart(A) ::= %s(B). {\n", grammar->rules[0].name);
        sb_append(&sb, "    A = B;\n");
        sb_append(&sb, "    s->result = A;\n");
        sb_append(&sb, "}\n");
    }

    /* Second pass: emit main rules.
     * Use a mirror counter to generate the same helper names as the first pass. */
    int desugar_ref = 0;
    for (r = 0; r < grammar->num_rules; r++) {
        LarkRule* rule = &grammar->rules[r];

        for (a = 0; a < rule->num_alts; a++) {
            LarkAlternative* alt = &rule->alts[a];

            /* Rule name */
            const char* node_name = alt->alias ? alt->alias : rule->name;
            int is_inline = rule->is_inline;

            /* LHS */
            sb_printf(&sb, "%s(A) ::=", rule->name);

            /* RHS items */
            int nchildren = 0;
            char child_names[64][32];

            for (i = 0; i < alt->num_items; i++) {
                LarkItem* item = &alt->items[i];
                char sym_buf[128];

                if (item->modifier != LARK_MOD_NONE) {
                    /* Reference the desugared helper rule name */
                    const char* mod_str =
                        item->modifier == LARK_MOD_OPT  ? "opt" :
                        item->modifier == LARK_MOD_STAR ? "star" : "plus";
                    snprintf(sym_buf, sizeof(sym_buf), "x%s_%s_%d",
                             rule->name, mod_str, desugar_ref++);
                } else {
                    snprintf(sym_buf, sizeof(sym_buf), "%s",
                             item_token_name(item));
                }

                snprintf(child_names[nchildren], 32, "C%d", nchildren);
                sb_printf(&sb, " %s(%s)", sym_buf, child_names[nchildren]);
                nchildren++;
            }

            sb_append(&sb, ". {\n");

            /* Reduce action: build AST node */
            if (alt->num_items == 0) {
                /* Empty alternative */
                sb_printf(&sb, "    A = ast_rule(s, \"%s\", 0);\n", node_name);
            } else if (is_inline && alt->num_items == 1 && !alt->alias) {
                /* Inline rule with single child: pass through */
                sb_printf(&sb, "    A = %s;\n", child_names[0]);
            } else {
                /* Build rule node */
                sb_printf(&sb, "    A = ast_rule(s, \"%s\", %d",
                          node_name, nchildren);
                for (i = 0; i < nchildren; i++) {
                    sb_printf(&sb, ", %s", child_names[i]);
                }
                sb_append(&sb, ");\n");
            }

            sb_append(&sb, "}\n");
        }
    }

    return sb_detach(&sb);
}

/* ---- Generate re2c .re input ---- */

/* Convert Lark regex shorthand to re2c pattern.
 * re2c doesn't support \d, \w, \s — expand them to character classes.
 * Returns malloc'd string. Caller frees. */
static char* lark_to_re2c_regex(const char* pat) {
    SBuf sb;
    sb_init(&sb);

    while (*pat) {
        if (*pat == '\\' && pat[1]) {
            pat++;
            switch (*pat) {
                case 'd': sb_append(&sb, "[0-9]"); break;
                case 'D': sb_append(&sb, "[^0-9]"); break;
                case 'w': sb_append(&sb, "[a-zA-Z0-9_]"); break;
                case 'W': sb_append(&sb, "[^a-zA-Z0-9_]"); break;
                case 's': sb_append(&sb, "[ \\t\\n\\r]"); break;
                case 'S': sb_append(&sb, "[^ \\t\\n\\r]"); break;
                case 'n': sb_append(&sb, "\\n"); break;
                case 't': sb_append(&sb, "\\t"); break;
                case 'r': sb_append(&sb, "\\r"); break;
                case '\\': sb_append(&sb, "\\\\"); break;
                default: {
                    /* Pass through other escapes */
                    char tmp[3] = {'\\', *pat, 0};
                    sb_append(&sb, tmp);
                    break;
                }
            }
            pat++;
        } else if (*pat == '.') {
            /* Lark '.' = any char. re2c '.' = any except newline.
             * Use [^\x00] for "any byte except NUL" (our EOF sentinel). */
            sb_append(&sb, "[^\\x00]");
            pat++;
        } else {
            char tmp[2] = {*pat, 0};
            sb_append(&sb, tmp);
            pat++;
        }
    }

    return sb_detach(&sb);
}

/* Emit a re2c string literal pattern from a Lark literal value.
 * Escapes special characters for re2c "..." syntax. */
static void emit_re2c_string_literal(SBuf* sb, const char* lit) {
    sb_append(sb, "\"");
    while (*lit) {
        switch (*lit) {
            case '"':  sb_append(sb, "\\\""); break;
            case '\\': sb_append(sb, "\\\\"); break;
            case '\n': sb_append(sb, "\\n"); break;
            case '\t': sb_append(sb, "\\t"); break;
            case '\r': sb_append(sb, "\\r"); break;
            default: {
                if ((unsigned char)*lit < 32) {
                    sb_printf(sb, "\\x%02x", (unsigned char)*lit);
                } else {
                    char tmp[2] = {*lit, 0};
                    sb_append(sb, tmp);
                }
            }
        }
        lit++;
    }
    sb_append(sb, "\"");
}

/* Convert a Lark ignore pattern to re2c regex.
 * Common patterns: \s+ → [ \t\n\r]+, /.../ → content as-is */
static char* lark_ignore_to_re2c(const char* pat) {
    /* Common shorthands */
    if (strcmp(pat, "\\s+") == 0)
        return strdup("[ \\t\\n\\r]+");
    if (strcmp(pat, "[ \\t\\n\\r]+") == 0 || strcmp(pat, "[ \\t\\n]+") == 0)
        return strdup("[ \\t\\n\\r]+");

    /* General case: convert through our regex converter */
    return lark_to_re2c_regex(pat);
}

char* lark_generate_re2c_re(LarkGrammar* grammar, const char* func_prefix) {
    SBuf sb;
    int i;

    sb_init(&sb);

    /* Lexer state struct */
    sb_printf(&sb, "typedef struct {\n");
    sb_printf(&sb, "    const char* cursor;\n");
    sb_printf(&sb, "    const char* end;\n");
    sb_printf(&sb, "    const char* start;\n");
    sb_printf(&sb, "    int line;\n");
    sb_printf(&sb, "} %s_Lexer;\n\n", func_prefix);

    /* Token type enum */
    sb_printf(&sb, "enum {\n");
    sb_printf(&sb, "    %s_TOK_EOF = 0,\n", func_prefix);
    for (i = 0; i < grammar->num_terminals; i++) {
        sb_printf(&sb, "    %s_TOK_%s = %d,\n",
                  func_prefix, grammar->terminals[i].name, i + 1);
    }
    for (i = 0; i < _num_anon; i++) {
        sb_printf(&sb, "    %s_TOK_%s = %d,\n",
                  func_prefix, _anon_lits[i].token_name,
                  grammar->num_terminals + i + 1);
    }
    sb_printf(&sb, "};\n\n");

    /* Token name lookup table */
    sb_printf(&sb, "static const char* %s_tok_names[] = {\n", func_prefix);
    sb_printf(&sb, "    \"EOF\",\n");
    for (i = 0; i < grammar->num_terminals; i++) {
        sb_printf(&sb, "    \"%s\",\n", grammar->terminals[i].name);
    }
    for (i = 0; i < _num_anon; i++) {
        sb_printf(&sb, "    \"%s\",\n", _anon_lits[i].token_name);
    }
    sb_printf(&sb, "};\n\n");

    /* TOKEN macro — used in re2c actions */
    sb_printf(&sb, "#define TOKEN(type) { \\\n");
    sb_printf(&sb, "    int _len = (int)(YYCURSOR - lex->start); \\\n");
    sb_printf(&sb, "    AstNode* _node = ast_token(s, %s_tok_names[type], 0); \\\n", func_prefix);
    sb_printf(&sb, "    _node->value = arena_strndup(&s->arena, lex->start, _len); \\\n");
    sb_printf(&sb, "    lex->cursor = YYCURSOR; \\\n");
    sb_printf(&sb, "    *tok_type = type; \\\n");
    sb_printf(&sb, "    return _node; \\\n");
    sb_printf(&sb, "}\n\n");

    /* SKIP macro — for ignored patterns */
    sb_printf(&sb, "#define SKIP() { \\\n");
    sb_printf(&sb, "    for (const char* _p = lex->start; _p < YYCURSOR; _p++) \\\n");
    sb_printf(&sb, "        if (*_p == '\\n') lex->line++; \\\n");
    sb_printf(&sb, "    lex->cursor = YYCURSOR; \\\n");
    sb_printf(&sb, "    goto loop; \\\n");
    sb_printf(&sb, "}\n\n");

    /* Next token function */
    sb_printf(&sb, "static AstNode* %s_next_token(%s_Lexer* lex, int* tok_type,\n",
              func_prefix, func_prefix);
    sb_printf(&sb, "                               GrammarParseState* s) {\n");
    sb_append(&sb, "    const char* YYCURSOR = lex->cursor;\n");
    sb_append(&sb, "    const char* YYMARKER;\n");
    sb_append(&sb, "    const char* YYLIMIT = lex->end;\n\n");

    sb_append(&sb, "loop:\n");
    sb_append(&sb, "    lex->start = YYCURSOR;\n\n");

    /* Begin re2c block */
    sb_append(&sb, "    /*!re2c\n");
    sb_append(&sb, "        re2c:define:YYCTYPE = char;\n");
    sb_append(&sb, "        re2c:yyfill:enable = 0;\n");
    sb_append(&sb, "        re2c:eof = 0;\n\n");

    /* Ignore patterns (whitespace etc.) — emit first so they're skipped */
    for (i = 0; i < grammar->num_ignore; i++) {
        char* re2c_pat = lark_ignore_to_re2c(grammar->ignore_patterns[i]);
        sb_printf(&sb, "        %s { SKIP() }\n", re2c_pat);
        free(re2c_pat);
    }
    if (grammar->num_ignore > 0) sb_append(&sb, "\n");

    /* Anonymous string literals — exact match, high priority (listed first) */
    for (i = 0; i < _num_anon; i++) {
        sb_append(&sb, "        ");
        emit_re2c_string_literal(&sb, _anon_lits[i].literal);
        sb_printf(&sb, " { TOKEN(%s_TOK_%s) }\n",
                  func_prefix, _anon_lits[i].token_name);
    }
    if (_num_anon > 0) sb_append(&sb, "\n");

    /* Named terminal patterns */
    for (i = 0; i < grammar->num_terminals; i++) {
        LarkTerminal* t = &grammar->terminals[i];
        if (t->is_regex) {
            char* re2c_pat = lark_to_re2c_regex(t->pattern);
            sb_printf(&sb, "        %s { TOKEN(%s_TOK_%s) }\n",
                      re2c_pat, func_prefix, t->name);
            free(re2c_pat);
        } else {
            /* String terminal — wrap in quotes */
            sb_append(&sb, "        ");
            emit_re2c_string_literal(&sb, t->pattern);
            sb_printf(&sb, " { TOKEN(%s_TOK_%s) }\n",
                      func_prefix, t->name);
        }
    }
    sb_append(&sb, "\n");

    /* EOF rule */
    sb_printf(&sb, "        $ { *tok_type = 0; lex->cursor = YYCURSOR; return 0; }\n");

    /* Default error rule */
    sb_printf(&sb, "        * { *tok_type = -1; lex->cursor = YYCURSOR; return 0; }\n");

    /* End re2c block */
    sb_append(&sb, "    */\n");
    sb_append(&sb, "}\n\n");

    /* Undefine macros */
    sb_append(&sb, "#undef TOKEN\n");
    sb_append(&sb, "#undef SKIP\n");

    return sb_detach(&sb);
}

/* ---- AST template string ---- */

static const char* AST_TEMPLATE =
"/* ---- AST Template ---- */\n"
"/* Freestanding declarations — no system headers needed for JIT */\n"
"#define NDEBUG 1\n"
"#define assert(x) ((void)0)\n"
"typedef void FILE;\n"
"typedef unsigned long long size_t;\n"
"extern void* malloc(size_t);\n"
"extern void* realloc(void*, size_t);\n"
"extern void  free(void*);\n"
"extern void* memset(void*, int, size_t);\n"
"extern void* memcpy(void*, const void*, size_t);\n"
"extern int   memcmp(const void*, const void*, size_t);\n"
"extern size_t strlen(const char*);\n"
"extern char* strncpy(char*, const char*, size_t);\n"
"extern int   strcmp(const char*, const char*);\n"
"extern int   strncmp(const char*, const char*, size_t);\n"
"extern int fprintf(void*, const char*, ...);\n"
"typedef __builtin_va_list va_list;\n"
"#define va_start(ap, param) __builtin_va_start(ap, param)\n"
"#define va_arg(ap, type) __builtin_va_arg(ap, type)\n"
"#define va_end(ap) __builtin_va_end(ap)\n"
"\n"
"typedef struct AstArena {\n"
"    char* base;\n"
"    size_t used, capacity;\n"
"} AstArena;\n"
"\n"
"typedef struct AstNode {\n"
"    const char* name;\n"
"    const char* value;\n"
"    int num_children;\n"
"    struct AstNode** children;\n"
"} AstNode;\n"
"\n"
"typedef struct GrammarParseState {\n"
"    AstArena arena;\n"
"    AstNode* result;\n"
"    char error_msg[256];\n"
"    int error_line, error_col;\n"
"    int has_error;\n"
"} GrammarParseState;\n"
"\n"
"static void* arena_alloc(AstArena* a, size_t size) {\n"
"    size = (size + 7) & ~7;\n"
"    if (a->used + size > a->capacity) {\n"
"        size_t newcap = a->capacity ? a->capacity * 2 : 4096;\n"
"        while (a->used + size > newcap) newcap *= 2;\n"
"        a->base = (char*)realloc(a->base, newcap);\n"
"        a->capacity = newcap;\n"
"    }\n"
"    void* p = a->base + a->used;\n"
"    a->used += size;\n"
"    return p;\n"
"}\n"
"\n"
"static char* arena_strndup(AstArena* a, const char* s, int len) {\n"
"    char* p = (char*)arena_alloc(a, len + 1);\n"
"    memcpy(p, s, len);\n"
"    p[len] = 0;\n"
"    return p;\n"
"}\n"
"\n"
"static AstNode* ast_token(GrammarParseState* s, const char* name, AstNode* tok) {\n"
"    AstNode* n = (AstNode*)arena_alloc(&s->arena, sizeof(AstNode));\n"
"    n->name = name;\n"
"    n->value = tok ? tok->value : 0;\n"
"    n->num_children = 0;\n"
"    n->children = 0;\n"
"    return n;\n"
"}\n"
"\n"
"static AstNode* ast_rule(GrammarParseState* s, const char* name, int n, ...) {\n"
"    AstNode* node = (AstNode*)arena_alloc(&s->arena, sizeof(AstNode));\n"
"    node->name = name;\n"
"    node->value = 0;\n"
"    node->num_children = n;\n"
"    node->children = n > 0 ? (AstNode**)arena_alloc(&s->arena, n * sizeof(AstNode*)) : 0;\n"
"    va_list ap;\n"
"    va_start(ap, n);\n"
"    for (int i = 0; i < n; i++) node->children[i] = va_arg(ap, AstNode*);\n"
"    va_end(ap);\n"
"    return node;\n"
"}\n"
"\n"
"static AstNode* ast_append(GrammarParseState* s, AstNode* list, AstNode* item) {\n"
"    int new_n = list->num_children + 1;\n"
"    AstNode** new_children = (AstNode**)arena_alloc(&s->arena, new_n * sizeof(AstNode*));\n"
"    for (int i = 0; i < list->num_children; i++) new_children[i] = list->children[i];\n"
"    new_children[new_n - 1] = item;\n"
"    list->children = new_children;\n"
"    list->num_children = new_n;\n"
"    return list;\n"
"}\n"
"\n";

/* ---- Generate combined C source ---- */

char* lark_generate_combined_c(LarkGrammar* grammar,
                                const char* parser_c,
                                const char* parser_h,
                                const char* lexer_c,
                                const char* func_prefix) {
    SBuf sb;
    sb_init(&sb);

    sb_append(&sb, "/* Auto-generated grammar parser — do not edit */\n\n");

    /* 1. AST template */
    sb_append(&sb, AST_TEMPLATE);

    /* 2. Parser header (token defines) */
    if (parser_h) {
        sb_append(&sb, "/* ---- Token definitions ---- */\n");
        sb_append(&sb, parser_h);
        sb_append(&sb, "\n");
    }

    /* 3. Lexer */
    sb_append(&sb, "/* ---- Lexer ---- */\n");
    sb_append(&sb, lexer_c);
    sb_append(&sb, "\n");

    /* 4. Parser — strip #include lines (we provide freestanding decls) */
    sb_append(&sb, "/* ---- Parser ---- */\n");
    {
        const char* p = parser_c;
        while (*p) {
            const char* line_end = p;
            while (*line_end && *line_end != '\n') line_end++;
            int line_len = (int)(line_end - p);
            /* Skip #include <...> lines */
            if (line_len > 8 && strncmp(p, "#include", 8) == 0) {
                /* skip this line */
            } else {
                sb_ensure(&sb, line_len + 1);
                memcpy(sb.data + sb.used, p, line_len);
                sb.used += line_len;
                sb.data[sb.used] = 0;
            }
            if (*line_end == '\n') {
                sb_append(&sb, "\n");
                p = line_end + 1;
            } else {
                break;
            }
        }
    }
    sb_append(&sb, "\n");

    /* 5. Glue: parse function */
    sb_append(&sb, "/* ---- Parse entry point ---- */\n");
    sb_printf(&sb, "AstNode* %s_parse(const char* input, int length,\n", func_prefix);
    sb_printf(&sb, "                   char* error_buf, int error_buf_size,\n");
    sb_printf(&sb, "                   void** arena_out) {\n");
    sb_append(&sb, "    GrammarParseState state;\n");
    sb_append(&sb, "    memset(&state, 0, sizeof(state));\n\n");

    /* Init lexer */
    sb_printf(&sb, "    %s_Lexer lex;\n", func_prefix);
    sb_append(&sb, "    lex.cursor = input;\n");
    sb_append(&sb, "    lex.end = input + length;\n");
    sb_append(&sb, "    lex.start = input;\n");
    sb_append(&sb, "    lex.line = 1;\n\n");

    /* Init parser */
    sb_append(&sb, "    void* parser = ParseAlloc(malloc);\n");
    sb_append(&sb, "    if (!parser) return 0;\n\n");

    /* Feed tokens */
    sb_append(&sb, "    int tok_type;\n");
    sb_append(&sb, "    AstNode* tok_val;\n");
    sb_printf(&sb, "    while ((tok_val = %s_next_token(&lex, &tok_type, &state)) != 0\n",
              func_prefix);
    sb_append(&sb, "           || tok_type > 0) {\n");
    sb_append(&sb, "        if (tok_type < 0) { state.has_error = 1; break; }\n");
    sb_append(&sb, "        Parse(parser, tok_type, tok_val, &state);\n");
    sb_append(&sb, "        if (state.has_error) break;\n");
    sb_append(&sb, "    }\n\n");

    /* Send EOF */
    sb_append(&sb, "    if (!state.has_error) {\n");
    sb_append(&sb, "        Parse(parser, 0, 0, &state);\n");
    sb_append(&sb, "    }\n\n");

    sb_append(&sb, "    ParseFree(parser, free);\n");
    sb_append(&sb, "\n");
    sb_append(&sb, "    if (state.has_error) {\n");
    sb_append(&sb, "        if (error_buf && error_buf_size > 0)\n");
    sb_append(&sb, "            strncpy(error_buf, state.error_msg, error_buf_size - 1);\n");
    sb_append(&sb, "        free(state.arena.base);\n");
    sb_append(&sb, "        if (arena_out) *arena_out = 0;\n");
    sb_append(&sb, "        return 0;\n");
    sb_append(&sb, "    }\n");
    sb_append(&sb, "    if (arena_out) *arena_out = state.arena.base;\n");
    sb_append(&sb, "    return state.result;\n");
    sb_append(&sb, "}\n\n");

    /* Free function */
    sb_printf(&sb, "void %s_free(AstNode* root) {\n", func_prefix);
    sb_append(&sb, "    /* Arena-allocated — nothing to free per-node.\n");
    sb_append(&sb, "     * The arena itself is freed by the caller. */\n");
    sb_append(&sb, "}\n");

    return sb_detach(&sb);
}
