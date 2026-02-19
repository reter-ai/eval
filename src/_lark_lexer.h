/*
 * _lark_lexer.h — Tokenizer for Lark EBNF grammar format.
 *
 * Produces tokens for the lemon-generated Lark parser (lark_grammar.y).
 * Lark format is line-oriented: rules end at newlines, continuation
 * by indentation or | at line start.
 */
#ifndef LARK_LEXER_H
#define LARK_LEXER_H

#ifdef __cplusplus
extern "C" {
#endif

/* Token value — passed as %token_type to lemon */
typedef struct LarkToken {
    const char* start;      /* pointer into source */
    int length;
    int line;
    char* str_value;        /* heap-allocated string (for STRING, REGEX content) */
} LarkToken;

/* Lexer state */
typedef struct LarkLexer {
    const char* source;
    const char* current;
    int line;
    int col;
    char* error_msg;
    int at_line_start;      /* true if we're at the beginning of a line */
} LarkLexer;

/* Token types — must match lark_grammar.h LTOK_ constants.
 * We define them here as well so the lexer can reference them. */
#define LTOK_RULE_NAME       1
#define LTOK_TERM_NAME       2
#define LTOK_STRING          3
#define LTOK_REGEX           4
#define LTOK_ARROW           5
#define LTOK_COLON           6
#define LTOK_PIPE            7
#define LTOK_QUESTION        8
#define LTOK_STAR            9
#define LTOK_PLUS           10
#define LTOK_LPAREN         11
#define LTOK_RPAREN         12
#define LTOK_DOT            13
#define LTOK_NUMBER         14
#define LTOK_PERCENT_IGNORE 15
#define LTOK_NEWLINE        16
#define LTOK_TILDE          17

void lark_lexer_init(LarkLexer* lex, const char* source);
int  lark_lexer_next(LarkLexer* lex, LarkToken* token);
void lark_lexer_free(LarkLexer* lex);

#ifdef __cplusplus
}
#endif

#endif /* LARK_LEXER_H */
