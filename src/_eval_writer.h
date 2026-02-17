/*  _eval_writer.h -- Eval-syntax serializer  */

#ifndef EVAL_WRITER_H
#define EVAL_WRITER_H

#ifndef EVAL_STANDALONE
#include <Python.h>
#endif
#include <chibi/eval.h>

/* Write a sexp in Eval notation to a Python string. Returns new reference. */
#ifndef EVAL_STANDALONE
PyObject *eval_write_to_string(sexp ctx, sexp x);
#endif

/* Write a sexp in Eval notation to a C string buffer.
   Returns number of bytes written (not including NUL).
   If buf is NULL, returns required size. */
int eval_write_to_buf(sexp ctx, sexp x, char *buf, int size);

#endif /* EVAL_WRITER_H */
