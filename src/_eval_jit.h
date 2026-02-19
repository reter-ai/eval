/*
 * _eval_jit.h — LLVM ORC JIT wrapper for runtime C compilation.
 *
 * Compiles C source strings to native code in-memory using Clang + LLVM ORC JIT.
 * No temp files, no external processes — everything happens in-process.
 */
#ifndef EVAL_JIT_H
#define EVAL_JIT_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct EvalJIT EvalJIT;

/* Create JIT engine. Call once at startup.
 * Returns NULL on failure (LLVM init error). */
EvalJIT* eval_jit_create(void);

/* Compile C source string to native code and look up a symbol.
 *
 * jit:          JIT engine from eval_jit_create()
 * c_source:     Complete C source code string
 * symbol_name:  Exported function to look up (e.g. "grammar_parse")
 * error:        On failure, *error is set to a malloc'd error message (caller frees)
 *
 * Returns function pointer on success, NULL on failure.
 * Each call creates a new JIT dylib — multiple compilations don't clash. */
void* eval_jit_compile(EvalJIT* jit, const char* c_source,
                       const char* symbol_name, char** error);

/* Look up an additional symbol from a previous compilation.
 * Returns function pointer or NULL. */
void* eval_jit_lookup(EvalJIT* jit, const char* symbol_name, char** error);

/* Destroy JIT engine and free all compiled code. */
void eval_jit_destroy(EvalJIT* jit);

#ifdef __cplusplus
}
#endif

#endif /* EVAL_JIT_H */
