/*  _chibi_convert.h -- sexp <-> PyObject conversion  */

#ifndef CHIBI_CONVERT_H
#define CHIBI_CONVERT_H

#include <Python.h>
#include <chibi/eval.h>

/* Convert a chibi sexp to a Python object. Returns new reference. */
PyObject *sexp_to_pyobject(sexp ctx, sexp x);

/* Convert a Python object to a chibi sexp. */
sexp pyobject_to_sexp(sexp ctx, PyObject *obj);

/* The python-object sexp type tag (set during init) */
extern sexp_tag_t pyobject_type_tag;

/* The ChibiSexp Python type (defined in _chibi_sexp.c) */
extern PyTypeObject ChibiSexpType;

#endif /* CHIBI_CONVERT_H */
