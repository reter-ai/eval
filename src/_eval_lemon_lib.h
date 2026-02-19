/*
 * _eval_lemon_lib.h — Lemon LALR(1) parser generator as an in-process library.
 *
 * Compiles .y grammar strings to C parser code entirely in memory.
 * No temp files — all I/O uses in-memory buffers.
 */
#ifndef EVAL_LEMON_LIB_H
#define EVAL_LEMON_LIB_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    char* c_code;        /* Generated parser C code (malloc'd, caller frees) */
    char* h_code;        /* Generated header (malloc'd, caller frees) */
    char* error_msg;     /* Error message if !success (malloc'd, caller frees) */
    int success;         /* 1 on success, 0 on failure */
} LemonOutput;

/* Compile a .y grammar string to C parser code.
 *
 * grammar_y_text: Complete lemon grammar (.y format) as a NUL-terminated string.
 *
 * The template (lempar.c) is embedded — no external file needed.
 * Returns a LemonOutput struct. Caller must free with lemon_lib_free(). */
LemonOutput* lemon_lib_compile(const char* grammar_y_text);

/* Free a LemonOutput and all its strings. */
void lemon_lib_free(LemonOutput* out);

/* Set a custom lempar.c template text (optional).
 * If not called, uses the built-in embedded template.
 * The text must remain valid until after lemon_lib_compile() returns. */
void lemon_lib_set_template(const char* template_text, int template_len);

#ifdef __cplusplus
}
#endif

#endif /* EVAL_LEMON_LIB_H */
