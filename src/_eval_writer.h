/*  _eval_writer.h -- Eval-syntax serializer  */

#ifndef EVAL_WRITER_H
#define EVAL_WRITER_H

#include <Python.h>
#include <chibi/eval.h>

/* Write a sexp in Eval notation to a Python string. Returns new reference. */
PyObject *eval_write_to_string(sexp ctx, sexp x);

/* Write a sexp in Eval notation to a C string buffer.
   Returns number of bytes written (not including NUL).
   If buf is NULL, returns required size. */
int eval_write_to_buf(sexp ctx, sexp x, char *buf, int size);

#endif /* EVAL_WRITER_H */
