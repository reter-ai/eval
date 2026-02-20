/*  _eval_lexer.c -- Hand-written tokenizer for Eval language  */

#include "_eval_lexer.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

void eval_lexer_init(EvalLexer *lexer, const char *source) {
    lexer->source = source;
    lexer->current = source;
    lexer->line = 1;
    lexer->col = 1;
    lexer->error_msg = NULL;
    lexer->has_error = 0;
    lexer->prev_ends_expr = 0;
    lexer->fstr_mode = 0;
    lexer->fstr_nesting = 0;
    memset(lexer->fstr_brace_stack, 0, sizeof(lexer->fstr_brace_stack));
}

void eval_lexer_free(EvalLexer *lexer) {
    if (lexer->error_msg) {
        free(lexer->error_msg);
        lexer->error_msg = NULL;
    }
}

static void lexer_error(EvalLexer *lexer, const char *msg) {
    if (lexer->error_msg) free(lexer->error_msg);
    lexer->error_msg = (char *)malloc(256);
    if (lexer->error_msg) {
        snprintf(lexer->error_msg, 256, "Lexer error at line %d, col %d: %s",
                 lexer->line, lexer->col, msg);
    }
    lexer->has_error = 1;
}

static char peek(EvalLexer *lexer) {
    return *lexer->current;
}

static char peek_next(EvalLexer *lexer) {
    if (*lexer->current == '\0') return '\0';
    return *(lexer->current + 1);
}

static char advance(EvalLexer *lexer) {
    char c = *lexer->current;
    if (c == '\n') {
        lexer->line++;
        lexer->col = 1;
    } else {
        lexer->col++;
    }
    lexer->current++;
    return c;
}

static void skip_whitespace_and_comments(EvalLexer *lexer) {
    for (;;) {
        char c = peek(lexer);
        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
            advance(lexer);
        } else if (c == '/' && peek_next(lexer) == '/') {
            /* Line comment */
            while (peek(lexer) != '\0' && peek(lexer) != '\n')
                advance(lexer);
        } else if (c == '/' && peek_next(lexer) == '*') {
            /* Block comment */
            advance(lexer); advance(lexer);
            while (peek(lexer) != '\0') {
                if (peek(lexer) == '*' && peek_next(lexer) == '/') {
                    advance(lexer); advance(lexer);
                    break;
                }
                advance(lexer);
            }
        } else {
            break;
        }
    }
}

static int is_ident_start(char c) {
    return isalpha((unsigned char)c) || c == '_' || c == '$';
}

static int is_ident_char(char c) {
    return isalnum((unsigned char)c) || c == '_' || c == '?' || c == '!';
}

/* Keyword lookup table */
typedef struct {
    const char *name;
    int token;
} Keyword;

static const Keyword keywords[] = {
    {"function",    TOK_FUNCTION},
    {"constructor", TOK_CONSTRUCTOR},
    {"if",          TOK_IF},
    {"else",        TOK_ELSE},
    {"for",         TOK_FOR},
    {"while",       TOK_WHILE},
    {"in",          TOK_IN},
    {"do",          TOK_DO},
    {"until",       TOK_UNTIL},
    {"return",      TOK_RETURN},
    {"break",       TOK_BREAK},
    {"interface",   TOK_INTERFACE},
    {"super",       TOK_SUPER},
    {"let",         TOK_LET},
    {"let*",        TOK_LETSTAR},
    {"letrec",      TOK_LETREC},
    {"try",         TOK_TRY},
    {"catch",       TOK_CATCH},
    {"finally",     TOK_FINALLY},
    {"with",        TOK_WITH},
    {"async",       TOK_ASYNC},
    {"await",       TOK_AWAIT},
    {"import",      TOK_IMPORT},
    {"cond",        TOK_COND},
    {"case",        TOK_CASE},
    {"when",        TOK_WHEN},
    {"unless",      TOK_UNLESS},
    {"values",      TOK_VALUES},
    {"receive",     TOK_RECEIVE},
    {"from",        TOK_FROM},
    {"record",      TOK_RECORD},
    {"dict",        TOK_DICT},
    {"library",     TOK_LIBRARY},
    {"export",      TOK_EXPORT},
    {"include",     TOK_INCLUDE},
    {"macro",       TOK_MACRO},
    {"syntax_rules",TOK_SYNTAX_RULES},
    {"test_group",  TOK_TEST_GROUP},
    {"define",      TOK_DEFINE},
    {"static",      TOK_STATIC},
    {"abstract",    TOK_ABSTRACT},
    {"yield",       TOK_YIELD},
    {"generator",   TOK_GENERATOR},
    {"parallel",    TOK_PARALLEL},
    {"fresh",       TOK_FRESH},
    {"conde",       TOK_CONDE},
    {"run",         TOK_RUN},
    {"fact",        TOK_FACT},
    {"rule",        TOK_RULE},
    {"true",        TOK_TRUE},
    {"false",       TOK_FALSE},
    {"nil",         TOK_NIL},
    {NULL, 0}
};

static int lookup_keyword(const char *start, int length) {
    for (const Keyword *kw = keywords; kw->name != NULL; kw++) {
        int klen = (int)strlen(kw->name);
        if (klen == length && memcmp(start, kw->name, length) == 0) {
            return kw->token;
        }
    }
    return TOK_IDENT;
}

static int lex_number(EvalLexer *lexer, EvalToken *token) {
    const char *start = lexer->current;
    int is_float = 0;

    if (peek(lexer) == '0' && (peek_next(lexer) == 'x' || peek_next(lexer) == 'X')) {
        /* Hex */
        advance(lexer); advance(lexer);
        while (isxdigit((unsigned char)peek(lexer))) advance(lexer);
        token->type = TOK_INT;
        token->start = start;
        token->length = (int)(lexer->current - start);
        token->value.int_val = strtoll(start, NULL, 16);
        return 0;
    }

    while (isdigit((unsigned char)peek(lexer))) advance(lexer);

    if (peek(lexer) == '.' && isdigit((unsigned char)peek_next(lexer))) {
        is_float = 1;
        advance(lexer); /* consume '.' */
        while (isdigit((unsigned char)peek(lexer))) advance(lexer);
    }

    if (peek(lexer) == 'e' || peek(lexer) == 'E') {
        is_float = 1;
        advance(lexer);
        if (peek(lexer) == '+' || peek(lexer) == '-') advance(lexer);
        while (isdigit((unsigned char)peek(lexer))) advance(lexer);
    }

    token->start = start;
    token->length = (int)(lexer->current - start);

    if (is_float) {
        token->type = TOK_FLOAT;
        token->value.float_val = strtod(start, NULL);
    } else {
        token->type = TOK_INT;
        token->value.int_val = strtoll(start, NULL, 10);
    }
    return 0;
}

static int lex_string(EvalLexer *lexer, EvalToken *token) {
    const char *start = lexer->current;
    advance(lexer); /* consume opening quote */

    /* Triple-quoted raw string: """...""" — no escape processing */
    if (peek(lexer) == '"' && peek_next(lexer) == '"') {
        advance(lexer); /* second " */
        advance(lexer); /* third " */
        const char *content_start = lexer->current;
        while (peek(lexer) != '\0') {
            if (peek(lexer) == '"'
                && *(lexer->current + 1) == '"'
                && *(lexer->current + 2) == '"') {
                token->type = TOK_RAW_STRING;
                token->start = content_start;
                token->length = (int)(lexer->current - content_start);
                advance(lexer); advance(lexer); advance(lexer);
                return 0;
            }
            advance(lexer);
        }
        lexer_error(lexer, "unterminated triple-quoted string");
        return -1;
    }

    /* Regular single-quoted string */
    while (peek(lexer) != '\0' && peek(lexer) != '"') {
        if (peek(lexer) == '\\') {
            advance(lexer); /* consume backslash */
            if (peek(lexer) == '\0') {
                lexer_error(lexer, "unterminated string escape");
                return -1;
            }
        }
        advance(lexer);
    }

    if (peek(lexer) == '\0') {
        lexer_error(lexer, "unterminated string");
        return -1;
    }

    advance(lexer); /* consume closing quote */
    token->type = TOK_STRING;
    token->start = start + 1; /* skip opening quote */
    token->length = (int)(lexer->current - start - 2); /* exclude both quotes */
    return 0;
}

/* Backtick-quoted identifier: `scheme-name` → raw Scheme identifier */
static int lex_backtick_ident(EvalLexer *lexer, EvalToken *token) {
    const char *start = lexer->current; /* points past opening backtick */

    while (peek(lexer) != '`' && peek(lexer) != '\0' && peek(lexer) != '\n')
        advance(lexer);

    if (peek(lexer) != '`') {
        lexer_error(lexer, "unterminated backtick identifier");
        return -1;
    }

    token->start = start;
    token->length = (int)(lexer->current - start);
    advance(lexer); /* consume closing backtick */
    token->type = TOK_IDENT; /* never a keyword */
    return 0;
}

static int lex_ident(EvalLexer *lexer, EvalToken *token) {
    const char *start = lexer->current;

    while (is_ident_char(peek(lexer))) advance(lexer);

    /* Check for let* specially */
    if (peek(lexer) == '*' && (lexer->current - start) == 3 &&
        memcmp(start, "let", 3) == 0) {
        advance(lexer); /* consume the * */
    }

    int length = (int)(lexer->current - start);
    token->start = start;
    token->length = length;
    token->type = lookup_keyword(start, length);
    return 0;
}

/* Check if next non-whitespace is an expression terminator or a
   continuation keyword (until, else, catch, finally, while).
   Used to disambiguate ++ (PLUSPLUS vs CONCAT) and unary ops vs OPVAL. */
static int peek_is_expr_terminator(EvalLexer *lexer) {
    const char *p = lexer->current;
    while (*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n') p++;
    char c = *p;
    if (c == ',' || c == ')' || c == ']' || c == '}' ||
        c == ';' || c == ':' || c == '\0')
        return 1;
    /* Continuation keywords that follow an expression but can't start one.
       Without this, "i++ until(...)" converts ++ to CONCAT. */
#define KW_MATCH(s, n) (strncmp(p, s, n) == 0 && \
    p[n] != '_' && (p[n] < 'A' || (p[n] > 'Z' && p[n] < 'a') || p[n] > 'z') \
    && (p[n] < '0' || p[n] > '9'))
    if (c == 'u' && KW_MATCH("until", 5))   return 1;
    if (c == 'e' && KW_MATCH("else", 4))    return 1;
    if (c == 'c' && KW_MATCH("catch", 5))   return 1;
    if (c == 'f' && KW_MATCH("finally", 7)) return 1;
    if (c == 'w' && KW_MATCH("while", 5))   return 1;
#undef KW_MATCH
    return 0;
}

/* Convert operators to OPVAL when in value position, update prev_ends_expr */
static void lexer_post_process(EvalLexer *lexer, EvalToken *token) {
    /* In value position (prev token doesn't end an expr), convert
       operator tokens to OPVAL so the parser sees them as values. */
    if (!lexer->prev_ends_expr) {
        switch (token->type) {
        /* Unambiguous operators — always OPVAL in value position */
        case TOK_STAR:
        case TOK_SLASH:
        case TOK_PERCENT:
        case TOK_STARSTAR:
        case TOK_EQEQ:
        case TOK_EQQ:
        case TOK_LT:
        case TOK_GT:
        case TOK_LTE:
        case TOK_GTE:
        case TOK_BITAND:
        case TOK_BITOR:
        case TOK_SHL:
        case TOK_SHR:
        case TOK_CONCAT:
            token->value.int_val = token->type;
            token->type = TOK_OPVAL;
            break;
        case TOK_PLUSPLUS:
            token->value.int_val = TOK_CONCAT;
            token->type = TOK_OPVAL;
            break;
        /* Ambiguous: also has unary form. Only OPVAL if followed by
           a token that can't start an expression (comma, paren, etc.) */
        case TOK_PLUS:
        case TOK_MINUS:
        case TOK_BANG:
        case TOK_BITNOT:
            if (peek_is_expr_terminator(lexer)) {
                token->value.int_val = token->type;
                token->type = TOK_OPVAL;
            }
            break;
        }
    }

    /* ++ disambiguation: in binary position (prev ends expr) and followed
       by an expression-starting token, convert PLUSPLUS → CONCAT. */
    if (lexer->prev_ends_expr && token->type == TOK_PLUSPLUS) {
        if (!peek_is_expr_terminator(lexer)) {
            token->type = TOK_CONCAT;
        }
    }

    /* Track whether this token can end an expression */
    switch (token->type) {
    case TOK_IDENT: case TOK_INT: case TOK_FLOAT: case TOK_STRING:
    case TOK_TRUE: case TOK_FALSE: case TOK_NIL:
    case TOK_RPAREN: case TOK_RBRACKET: case TOK_RBRACE:
    case TOK_PLUSPLUS: case TOK_MINUSMINUS:
    case TOK_OPVAL:
    case TOK_FSTR_END:
    case TOK_LOGICVAR:
        lexer->prev_ends_expr = 1;
        break;
    default:
        lexer->prev_ends_expr = 0;
        break;
    }
}

/* Scan f-string text segment. Called when fstr_mode is 1 (first) or 2 (mid).
   Scans until { (start of interpolation), " (end of f-string), or end of input.
   Token start/length point to raw text (including {{ and }} escapes). */
static int lex_fstring_text(EvalLexer *lexer, EvalToken *token) {
    token->line = lexer->line;
    token->col = lexer->col;
    const char *start = lexer->current;

    while (peek(lexer) != '\0') {
        char c = peek(lexer);

        if (c == '{' && peek_next(lexer) == '{') {
            /* Escaped brace {{ — include both chars in text */
            advance(lexer);
            advance(lexer);
            continue;
        }

        if (c == '}' && peek_next(lexer) == '}') {
            /* Escaped brace }} — include both chars in text */
            advance(lexer);
            advance(lexer);
            continue;
        }

        if (c == '{') {
            /* Start of interpolation — end text segment */
            token->start = start;
            token->length = (int)(lexer->current - start);
            if (lexer->fstr_mode == 1) {
                token->type = TOK_FSTR_START;
            } else {
                token->type = TOK_FSTR_MID;
            }
            advance(lexer); /* consume '{' */
            lexer->fstr_mode = 3; /* switch to expr mode */
            lexer->fstr_brace_stack[lexer->fstr_nesting - 1] = 1;
            lexer_post_process(lexer, token);
            return 0;
        }

        if (c == '"') {
            /* End of f-string */
            token->start = start;
            token->length = (int)(lexer->current - start);
            advance(lexer); /* consume closing '"' */
            if (lexer->fstr_mode == 1) {
                /* No interpolation: emit FSTR_START, then FSTR_END on next call */
                token->type = TOK_FSTR_START;
                lexer->fstr_mode = 4; /* pending FSTR_END */
            } else {
                /* End after interpolation(s) */
                token->type = TOK_FSTR_END;
                lexer->fstr_nesting--;
                lexer->fstr_mode = lexer->fstr_nesting > 0 ? 3 : 0;
            }
            lexer_post_process(lexer, token);
            return 0;
        }

        if (c == '\\') {
            /* Escape sequence — skip backslash + next char */
            advance(lexer);
            if (peek(lexer) != '\0') advance(lexer);
            continue;
        }

        advance(lexer);
    }

    lexer_error(lexer, "unterminated f-string");
    return -1;
}

int eval_lexer_next(EvalLexer *lexer, EvalToken *token) {
    /* Pending FSTR_END: no-interpolation f-string needs a closing token */
    if (lexer->fstr_mode == 4) {
        token->line = lexer->line;
        token->col = lexer->col;
        token->start = lexer->current;
        token->length = 0;
        token->value.int_val = 0;
        token->type = TOK_FSTR_END;
        lexer->fstr_nesting--;
        lexer->fstr_mode = lexer->fstr_nesting > 0 ? 3 : 0;
        lexer_post_process(lexer, token);
        return 0;
    }

    /* If we're in f-string text mode, scan text first */
    if (lexer->fstr_mode == 1 || lexer->fstr_mode == 2) {
        return lex_fstring_text(lexer, token);
    }

    skip_whitespace_and_comments(lexer);

    token->line = lexer->line;
    token->col = lexer->col;
    token->start = lexer->current;
    token->length = 1;
    token->value.int_val = 0;

    char c = peek(lexer);

    if (c == '\0') {
        token->type = TOK_EOF;
        lexer_post_process(lexer, token);
        return 0;
    }

    /* Numbers */
    if (isdigit((unsigned char)c)) {
        int r = lex_number(lexer, token);
        if (r == 0) lexer_post_process(lexer, token);
        return r;
    }

    /* Strings */
    if (c == '"') {
        int r = lex_string(lexer, token);
        if (r == 0) lexer_post_process(lexer, token);
        return r;
    }

    /* Backtick-quoted identifiers: `scheme-name` */
    if (c == '`') {
        advance(lexer); /* consume opening backtick */
        int r = lex_backtick_ident(lexer, token);
        if (r == 0) lexer_post_process(lexer, token);
        return r;
    }

    /* F-string: f"..." */
    if (c == 'f' && *(lexer->current + 1) == '"') {
        advance(lexer); /* consume 'f' */
        advance(lexer); /* consume '"' */
        if (lexer->fstr_nesting >= FSTR_MAX_NESTING) {
            lexer_error(lexer, "f-string nesting too deep");
            return -1;
        }
        lexer->fstr_mode = 1; /* text-first */
        lexer->fstr_nesting++;
        lexer->fstr_brace_stack[lexer->fstr_nesting - 1] = 0;
        return lex_fstring_text(lexer, token);
    }

    /* Identifiers and keywords */
    if (is_ident_start(c)) {
        int r = lex_ident(lexer, token);
        if (r == 0) lexer_post_process(lexer, token);
        return r;
    }

    /* Operators and delimiters */
    advance(lexer);

    switch (c) {
    case '(':
        token->type = TOK_LPAREN;
        break;
    case ')':
        token->type = TOK_RPAREN;
        break;
    case '{':
        if (lexer->fstr_mode == 3)
            lexer->fstr_brace_stack[lexer->fstr_nesting - 1]++;
        token->type = TOK_LBRACE;
        break;
    case '}':
        if (lexer->fstr_mode == 3) {
            lexer->fstr_brace_stack[lexer->fstr_nesting - 1]--;
            if (lexer->fstr_brace_stack[lexer->fstr_nesting - 1] == 0) {
                /* End of interpolation expression — switch to text-mid */
                lexer->fstr_mode = 2;
                return lex_fstring_text(lexer, token);
            }
        }
        token->type = TOK_RBRACE;
        break;
    case '[':
        token->type = TOK_LBRACKET;
        break;
    case ']':
        token->type = TOK_RBRACKET;
        break;
    case ',':
        token->type = TOK_COMMA;
        break;
    case ';':
        token->type = TOK_SEMICOLON;
        break;
    case '\'':
        token->type = TOK_QUOTE;
        break;
    case '~':
        token->type = TOK_BITNOT;
        break;

    case '#':
        if (peek(lexer) == '[') {
            advance(lexer);
            token->type = TOK_HASH_LBRACKET;
            token->length = 2;
        } else {
            lexer_error(lexer, "unexpected '#'");
            return -1;
        }
        break;

    case ':':
        if (peek(lexer) == '-') {
            advance(lexer);
            token->type = TOK_COLONMINUS;
            token->length = 2;
        } else {
            token->type = TOK_COLON;
        }
        break;

    case '=':
        if (peek(lexer) == '=' && peek_next(lexer) == '=') {
            advance(lexer); advance(lexer);
            token->type = TOK_UNIFY;
            token->length = 3;
        } else if (peek(lexer) == '=') {
            advance(lexer);
            token->type = TOK_EQEQ;
            token->length = 2;
        } else if (peek(lexer) == '?') {
            advance(lexer);
            token->type = TOK_EQQ;
            token->length = 2;
        } else {
            token->type = TOK_ASSIGN;
        }
        break;

    case '+':
        if (peek(lexer) == '+') {
            advance(lexer);
            token->type = TOK_PLUSPLUS;
            token->length = 2;
        } else if (peek(lexer) == '=') {
            advance(lexer);
            token->type = TOK_PLUS_ASSIGN;
            token->length = 2;
        } else {
            token->type = TOK_PLUS;
        }
        break;

    case '-':
        if (peek(lexer) == '-') {
            advance(lexer);
            token->type = TOK_MINUSMINUS;
            token->length = 2;
        } else if (peek(lexer) == '=') {
            advance(lexer);
            token->type = TOK_MINUS_ASSIGN;
            token->length = 2;
        } else if (peek(lexer) == '>') {
            advance(lexer);
            token->type = TOK_ARROW;
            token->length = 2;
        } else {
            token->type = TOK_MINUS;
        }
        break;

    case '*':
        if (peek(lexer) == '*') {
            advance(lexer);
            token->type = TOK_STARSTAR;
            token->length = 2;
        } else {
            token->type = TOK_STAR;
        }
        break;

    case '/':
        token->type = TOK_SLASH;
        break;

    case '%':
        token->type = TOK_PERCENT;
        break;

    case '<':
        if (peek(lexer) == '=') {
            advance(lexer);
            token->type = TOK_LTE;
            token->length = 2;
        } else if (peek(lexer) == '<') {
            advance(lexer);
            token->type = TOK_SHL;
            token->length = 2;
        } else {
            token->type = TOK_LT;
        }
        break;

    case '>':
        if (peek(lexer) == '=') {
            advance(lexer);
            token->type = TOK_GTE;
            token->length = 2;
        } else if (peek(lexer) == '>') {
            advance(lexer);
            token->type = TOK_SHR;
            token->length = 2;
        } else {
            token->type = TOK_GT;
        }
        break;

    case '&':
        if (peek(lexer) == '&') {
            advance(lexer);
            token->type = TOK_AND;
            token->length = 2;
        } else {
            token->type = TOK_BITAND;
        }
        break;

    case '|':
        if (peek(lexer) == '|') {
            advance(lexer);
            token->type = TOK_OR;
            token->length = 2;
        } else {
            token->type = TOK_BITOR;
        }
        break;

    case '!':
        if (peek(lexer) == '=') {
            advance(lexer);
            token->type = TOK_BANGEQ;
            token->length = 2;
        } else if (peek(lexer) == '!') {
            advance(lexer);
            token->type = TOK_BANGBANG;
            token->length = 2;
        } else {
            token->type = TOK_BANG;
        }
        break;

    case '.':
        if (peek(lexer) == '.') {
            advance(lexer);
            token->type = TOK_DOTDOT;
            token->length = 2;
        } else if (isdigit((unsigned char)peek(lexer))) {
            /* .5 style float */
            lexer->current--; /* back up */
            lexer->col--;
            int r = lex_number(lexer, token);
            if (r == 0) lexer_post_process(lexer, token);
            return r;
        } else {
            lexer_error(lexer, "unexpected '.'");
            return -1;
        }
        break;

    case '?':
        if (is_ident_start(peek(lexer))) {
            /* Logic variable: ?x, ?name */
            const char *lv_start = lexer->current - 1; /* include the ? */
            while (is_ident_char(peek(lexer))) advance(lexer);
            token->start = lv_start;
            token->length = (int)(lexer->current - lv_start);
            token->type = TOK_LOGICVAR;
        } else {
            lexer_error(lexer, "expected identifier after '?'");
            return -1;
        }
        break;

    default:
        lexer_error(lexer, "unexpected character");
        return -1;
    }

    lexer_post_process(lexer, token);
    return 0;
}

const char *eval_token_name(int type) {
    /* Use a switch since token values are lemon-assigned and may change. */
    switch (type) {
    case TOK_EOF:           return "EOF";
    case TOK_ASSIGN:        return "=";
    case TOK_PLUS_ASSIGN:   return "+=";
    case TOK_MINUS_ASSIGN:  return "-=";
    case TOK_ELSE:          return "else";
    case TOK_OR:            return "||";
    case TOK_AND:           return "&&";
    case TOK_BITOR:         return "|";
    case TOK_BITAND:        return "&";
    case TOK_EQEQ:          return "==";
    case TOK_EQQ:           return "=?";
    case TOK_BANGEQ:        return "!=";
    case TOK_LT:            return "<";
    case TOK_GT:            return ">";
    case TOK_LTE:           return "<=";
    case TOK_GTE:           return ">=";
    case TOK_SHL:           return "<<";
    case TOK_SHR:           return ">>";
    case TOK_PLUS:          return "+";
    case TOK_MINUS:         return "-";
    case TOK_STAR:          return "*";
    case TOK_SLASH:         return "/";
    case TOK_PERCENT:       return "%";
    case TOK_STARSTAR:      return "**";
    case TOK_BANG:          return "!";
    case TOK_BITNOT:        return "~";
    case TOK_BANGBANG:      return "!!";
    case TOK_LPAREN:        return "(";
    case TOK_RPAREN:        return ")";
    case TOK_ARROW:         return "->";
    case TOK_PLUSPLUS:      return "++";
    case TOK_CONCAT:        return "++";
    case TOK_MINUSMINUS:    return "--";
    case TOK_SEMICOLON:     return ";";
    case TOK_IDENT:         return "IDENT";
    case TOK_INT:           return "INT";
    case TOK_FLOAT:         return "FLOAT";
    case TOK_STRING:        return "STRING";
    case TOK_TRUE:          return "true";
    case TOK_FALSE:         return "false";
    case TOK_NIL:           return "nil";
    case TOK_QUOTE:         return "'";
    case TOK_DOTDOT:        return "..";
    case TOK_LBRACE:        return "{";
    case TOK_RBRACE:        return "}";
    case TOK_LBRACKET:      return "[";
    case TOK_RBRACKET:      return "]";
    case TOK_HASH_LBRACKET: return "#[";
    case TOK_COMMA:         return ",";
    case TOK_COLON:         return ":";
    case TOK_FUNCTION:      return "function";
    case TOK_CONSTRUCTOR:   return "constructor";
    case TOK_IF:            return "if";
    case TOK_WHILE:         return "while";
    case TOK_FOR:           return "for";
    case TOK_DO:            return "do";
    case TOK_UNTIL:         return "until";
    case TOK_RETURN:        return "return";
    case TOK_BREAK:         return "break";
    case TOK_INTERFACE:     return "interface";
    case TOK_SUPER:         return "super";
    case TOK_LET:           return "let";
    case TOK_LETSTAR:       return "let*";
    case TOK_LETREC:        return "letrec";
    case TOK_TRY:           return "try";
    case TOK_CATCH:         return "catch";
    case TOK_FINALLY:       return "finally";
    case TOK_WITH:          return "with";
    case TOK_ASYNC:         return "async";
    case TOK_AWAIT:         return "await";
    case TOK_IMPORT:        return "import";
    case TOK_COND:          return "cond";
    case TOK_CASE:          return "case";
    case TOK_WHEN:          return "when";
    case TOK_UNLESS:        return "unless";
    case TOK_VALUES:        return "values";
    case TOK_RECEIVE:       return "receive";
    case TOK_FROM:          return "from";
    case TOK_RECORD:        return "record";
    case TOK_DICT:          return "dict";
    case TOK_LIBRARY:       return "library";
    case TOK_EXPORT:        return "export";
    case TOK_INCLUDE:       return "include";
    case TOK_MACRO:         return "macro";
    case TOK_SYNTAX_RULES:  return "syntax_rules";
    case TOK_TEST_GROUP:    return "test_group";
    case TOK_DEFINE:        return "define";
    case TOK_OPVAL:         return "OPVAL";
    case TOK_ABSTRACT:      return "abstract";
    case TOK_YIELD:         return "yield";
    case TOK_GENERATOR:     return "generator";
    case TOK_FSTR_START:    return "FSTR_START";
    case TOK_FSTR_MID:      return "FSTR_MID";
    case TOK_FSTR_END:      return "FSTR_END";
    case TOK_LOGICVAR:      return "?var";
    case TOK_UNIFY:         return "===";
    case TOK_COLONMINUS:    return ":-";
    case TOK_FRESH:         return "fresh";
    case TOK_CONDE:         return "conde";
    case TOK_RUN:           return "run";
    case TOK_FACT:          return "fact";
    case TOK_RULE:          return "rule";
    case TOK_QUERY:         return "query";
    case TOK_FINDALL:       return "findall";
    default:                return "UNKNOWN";
    }
}
