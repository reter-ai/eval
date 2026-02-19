/*
 * _eval_re2c_lib.h — re2c lexer generator as an in-process library.
 *
 * Compiles re2c .re format strings to C lexer code entirely in memory.
 */
#ifndef EVAL_RE2C_LIB_H
#define EVAL_RE2C_LIB_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    char* c_code;        /* Generated lexer C code (malloc'd, caller frees) */
    char* error_msg;     /* Error message if !success (malloc'd, caller frees) */
    int success;         /* 1 on success, 0 on failure */
} Re2cOutput;

/* Compile a .re format string to C lexer code.
 *
 * re_source: The re2c source with re2c marker blocks.
 *
 * Returns a Re2cOutput struct. Caller must free with re2c_lib_free(). */
Re2cOutput* re2c_lib_compile(const char* re_source);

/* Free a Re2cOutput and all its strings. */
void re2c_lib_free(Re2cOutput* out);

#ifdef __cplusplus
}
#endif

#endif /* EVAL_RE2C_LIB_H */
