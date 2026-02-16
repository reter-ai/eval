/*  _chibi_pyobject_type.h -- Custom <python-object> sexp type  */

#ifndef CHIBI_PYOBJECT_TYPE_H
#define CHIBI_PYOBJECT_TYPE_H

#include <Python.h>
#include <chibi/eval.h>

/* Register the <python-object> type with chibi. Call once at init. */
void register_pyobject_type(sexp ctx);

/* Wrap a PyObject* as a chibi sexp. Calls Py_INCREF. */
sexp wrap_pyobject(sexp ctx, PyObject *obj);

/* Unwrap a python-object sexp to get the PyObject*. */
PyObject *unwrap_pyobject(sexp x);

/* Check if sexp is a python-object */
int sexp_pyobjectp(sexp x);

#endif /* CHIBI_PYOBJECT_TYPE_H */
