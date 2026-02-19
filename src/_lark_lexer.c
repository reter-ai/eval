/*
 * _lark_lexer.c — Tokenizer for Lark EBNF grammar format.
 *
 * Lark format rules:
 * - Lowercase names are rules: start, expr, term
 * - UPPERCASE names are terminals: NUMBER, PLUS
 * - Strings "..." or '...' are inline literals
 * - /.../ are regex patterns
 * - -> alias names alternatives
 * - ? before rule name means inline
 * - %, *, + are EBNF modifiers
 * - %ignore /pattern/ ignores whitespace etc.
 * - Newlines are significant (end rules)
 * - Indentation/| continues previous rule
 * - Comments: // or #
 */
#include "_lark_lexer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

void lark_lexer_init(LarkLexer* lex, const char* source) {
    memset(lex, 0, sizeof(LarkLexer));
    lex->source = source;
    lex->current = source;
    lex->line = 1;
    lex->col = 1;
    lex->at_line_start = 1;
}

void lark_lexer_free(LarkLexer* lex) {
    free(lex->error_msg);
    lex->error_msg = NULL;
}

static int is_rule_char(char c) {
    return (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_';
}

static int is_term_char(char c) {
    return (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_';
}

/* Make a token referencing the source */
static void make_token(LarkToken* t, const char* start, int len, int line) {
    t->start = start;
    t->length = len;
    t->line = line;
    t->str_value = NULL;
}

/* Make a token with a heap-allocated string value */
static void make_str_token(LarkToken* t, const char* start, int len,
                            int line, const char* str, int str_len) {
    t->start = start;
    t->length = len;
    t->line = line;
    t->str_value = (char*)malloc(str_len + 1);
    memcpy(t->str_value, str, str_len);
    t->str_value[str_len] = 0;
}

int lark_lexer_next(LarkLexer* lex, LarkToken* token) {
    const char* p;
    memset(token, 0, sizeof(LarkToken));

restart:
    p = lex->current;

    /* Skip horizontal whitespace (NOT newlines) */
    while (*p == ' ' || *p == '\t') p++;
    lex->current = p;

    if (*p == 0) return 0;  /* EOF */

    /* Skip comments */
    if ((*p == '/' && p[1] == '/') || *p == '#') {
        while (*p && *p != '\n') p++;
        lex->current = p;
        goto restart;
    }

    /* Newline — significant token */
    if (*p == '\n') {
        /* Collapse multiple newlines and blank lines */
        while (*p == '\n' || *p == '\r') {
            if (*p == '\n') lex->line++;
            p++;
            /* Skip whitespace on next line */
            while (*p == ' ' || *p == '\t') p++;
            /* Skip comment lines */
            if ((*p == '/' && p[1] == '/') || *p == '#') {
                while (*p && *p != '\n') p++;
                continue;
            }
        }
        lex->current = p;
        lex->at_line_start = 1;

        /* If next line starts with | it's a continuation — emit PIPE instead */
        if (*p == '|') {
            lex->current = p + 1;
            lex->at_line_start = 0;
            make_token(token, p, 1, lex->line);
            return LTOK_PIPE;
        }

        /* If at EOF, don't emit trailing NEWLINE */
        if (*p == 0) return 0;

        make_token(token, p - 1, 1, lex->line);
        return LTOK_NEWLINE;
    }

    if (*p == '\r') {
        p++;
        lex->current = p;
        goto restart;
    }

    lex->at_line_start = 0;

    /* Two-character tokens */
    if (*p == '-' && p[1] == '>') {
        lex->current = p + 2;
        make_token(token, p, 2, lex->line);
        return LTOK_ARROW;
    }

    /* Single character tokens */
    switch (*p) {
        case ':':
            lex->current = p + 1;
            make_token(token, p, 1, lex->line);
            return LTOK_COLON;
        case '|':
            lex->current = p + 1;
            make_token(token, p, 1, lex->line);
            return LTOK_PIPE;
        case '?':
            lex->current = p + 1;
            make_token(token, p, 1, lex->line);
            return LTOK_QUESTION;
        case '*':
            lex->current = p + 1;
            make_token(token, p, 1, lex->line);
            return LTOK_STAR;
        case '+':
            lex->current = p + 1;
            make_token(token, p, 1, lex->line);
            return LTOK_PLUS;
        case '(':
            lex->current = p + 1;
            make_token(token, p, 1, lex->line);
            return LTOK_LPAREN;
        case ')':
            lex->current = p + 1;
            make_token(token, p, 1, lex->line);
            return LTOK_RPAREN;
        case '.':
            lex->current = p + 1;
            make_token(token, p, 1, lex->line);
            return LTOK_DOT;
        case '~':
            lex->current = p + 1;
            make_token(token, p, 1, lex->line);
            return LTOK_TILDE;
    }

    /* String literal "..." or '...' */
    if (*p == '"' || *p == '\'') {
        char delim = *p;
        const char* start = p;
        p++;
        const char* content_start = p;
        while (*p && *p != delim && *p != '\n') {
            if (*p == '\\' && p[1]) p++;
            p++;
        }
        int content_len = (int)(p - content_start);
        if (*p == delim) p++;
        lex->current = p;
        make_str_token(token, start, (int)(p - start), lex->line,
                       content_start, content_len);
        return LTOK_STRING;
    }

    /* Regex /.../ — handles [...] character classes where / is literal */
    if (*p == '/') {
        const char* start = p;
        p++;
        const char* content_start = p;
        int in_bracket = 0;
        while (*p && *p != '\n') {
            if (*p == '\\' && p[1]) { p += 2; continue; }
            if (*p == '[') { in_bracket = 1; p++; continue; }
            if (*p == ']') { in_bracket = 0; p++; continue; }
            if (*p == '/' && !in_bracket) break;
            p++;
        }
        int content_len = (int)(p - content_start);
        if (*p == '/') p++;
        lex->current = p;
        make_str_token(token, start, (int)(p - start), lex->line,
                       content_start, content_len);
        return LTOK_REGEX;
    }

    /* %ignore directive */
    if (*p == '%') {
        if (strncmp(p, "%ignore", 7) == 0 && !isalnum((unsigned char)p[7])) {
            lex->current = p + 7;
            make_token(token, p, 7, lex->line);
            return LTOK_PERCENT_IGNORE;
        }
        /* Skip other % directives (declare, import) for now */
        while (*p && *p != '\n') p++;
        lex->current = p;
        goto restart;
    }

    /* Number (for priority: NUMBER.2) */
    if (*p >= '0' && *p <= '9') {
        const char* start = p;
        while (*p >= '0' && *p <= '9') p++;
        lex->current = p;
        make_token(token, start, (int)(p - start), lex->line);
        return LTOK_NUMBER;
    }

    /* UPPERCASE name (terminal) */
    if (*p >= 'A' && *p <= 'Z') {
        const char* start = p;
        while (is_term_char(*p)) p++;
        lex->current = p;
        make_str_token(token, start, (int)(p - start), lex->line,
                       start, (int)(p - start));
        return LTOK_TERM_NAME;
    }

    /* lowercase name (rule) or _name */
    if ((*p >= 'a' && *p <= 'z') || *p == '_') {
        const char* start = p;
        while (is_rule_char(*p)) p++;
        lex->current = p;
        make_str_token(token, start, (int)(p - start), lex->line,
                       start, (int)(p - start));
        return LTOK_RULE_NAME;
    }

    /* Unknown character — error */
    lex->error_msg = (char*)malloc(64);
    snprintf(lex->error_msg, 64, "Unexpected character '%c' (0x%02x)",
             *p, (unsigned char)*p);
    lex->current = p + 1;
    return -1;
}
