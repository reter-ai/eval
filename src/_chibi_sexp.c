/*  _chibi_sexp.c -- ChibiSexp Python type (wraps raw sexp)  */

#include <Python.h>
#include <chibi/eval.h>
#include "_chibi_convert.h"
#include "_eval_writer.h"

/* ChibiSexp object: wraps a raw sexp value */
typedef struct {
    PyObject_HEAD
    sexp value;
    PyObject *context;  /* ref to ChibiContext to prevent GC */
} ChibiSexpObject;

/* Forward declaration */
static PyObject *ChibiSexp_new(PyTypeObject *type, PyObject *args, PyObject *kwds);
static void ChibiSexp_dealloc(ChibiSexpObject *self);
static PyObject *ChibiSexp_repr(ChibiSexpObject *self);
static PyObject *ChibiSexp_str(ChibiSexpObject *self);
static PyObject *ChibiSexp_call(ChibiSexpObject *self, PyObject *args, PyObject *kwds);

/* Get the context's sexp ctx */
extern sexp ChibiContext_get_ctx(PyObject *context);

static PyObject *ChibiSexp_new(PyTypeObject *type, PyObject *args, PyObject *kwds) {
    ChibiSexpObject *self = (ChibiSexpObject *)type->tp_alloc(type, 0);
    if (self) {
        self->value = SEXP_VOID;
        self->context = NULL;
    }
    return (PyObject *)self;
}

static void ChibiSexp_dealloc(ChibiSexpObject *self) {
    if (self->context && self->value != SEXP_VOID) {
        sexp ctx = ChibiContext_get_ctx(self->context);
        if (ctx) {
            sexp_release_object(ctx, self->value);
        }
    }
    Py_XDECREF(self->context);
    Py_TYPE(self)->tp_free((PyObject *)self);
}

static PyObject *ChibiSexp_repr(ChibiSexpObject *self) {
    if (!self->context) return PyUnicode_FromString("<ChibiSexp: detached>");
    sexp ctx = ChibiContext_get_ctx(self->context);
    if (!ctx) return PyUnicode_FromString("<ChibiSexp: invalid context>");
    return eval_write_to_string(ctx, self->value);
}

static PyObject *ChibiSexp_str(ChibiSexpObject *self) {
    return ChibiSexp_repr(self);
}

static PyObject *ChibiSexp_call(ChibiSexpObject *self, PyObject *args, PyObject *kwds) {
    if (!self->context) {
        PyErr_SetString(PyExc_RuntimeError, "ChibiSexp has no context");
        return NULL;
    }

    sexp ctx = ChibiContext_get_ctx(self->context);
    if (!ctx) {
        PyErr_SetString(PyExc_RuntimeError, "Invalid context");
        return NULL;
    }

    if (!sexp_procedurep(self->value) && !sexp_opcodep(self->value)) {
        PyErr_SetString(PyExc_TypeError, "ChibiSexp is not callable");
        return NULL;
    }

    /* Convert args */
    Py_ssize_t nargs = PyTuple_Size(args);
    sexp_gc_var2(sargs, result);
    sexp_gc_preserve2(ctx, sargs, result);

    sargs = SEXP_NULL;
    for (Py_ssize_t i = nargs - 1; i >= 0; i--) {
        sexp arg = pyobject_to_sexp(ctx, PyTuple_GetItem(args, i));
        sargs = sexp_cons(ctx, arg, sargs);
    }

    result = sexp_apply(ctx, self->value, sargs);
    sexp_gc_release2(ctx);

    if (sexp_exceptionp(result)) {
        sexp msg = sexp_exception_message(result);
        if (sexp_stringp(msg))
            PyErr_SetString(PyExc_RuntimeError, sexp_string_data(msg));
        else
            PyErr_SetString(PyExc_RuntimeError, "Scheme exception");
        return NULL;
    }

    return sexp_to_pyobject(ctx, result);
}

/* Convert to Python value */
static PyObject *ChibiSexp_to_python(ChibiSexpObject *self, PyObject *Py_UNUSED(args)) {
    if (!self->context) {
        Py_RETURN_NONE;
    }
    sexp ctx = ChibiContext_get_ctx(self->context);
    if (!ctx) {
        Py_RETURN_NONE;
    }
    return sexp_to_pyobject(ctx, self->value);
}

static PyMethodDef ChibiSexp_methods[] = {
    {"to_python", (PyCFunction)ChibiSexp_to_python, METH_NOARGS,
     "Convert sexp to Python value"},
    {NULL}
};

PyTypeObject ChibiSexpType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "chibi_eval._chibi.ChibiSexp",
    .tp_doc = "Wraps a raw Scheme s-expression",
    .tp_basicsize = sizeof(ChibiSexpObject),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_new = ChibiSexp_new,
    .tp_dealloc = (destructor)ChibiSexp_dealloc,
    .tp_repr = (reprfunc)ChibiSexp_repr,
    .tp_str = (reprfunc)ChibiSexp_str,
    .tp_call = (ternaryfunc)ChibiSexp_call,
    .tp_methods = ChibiSexp_methods,
};

/* Helper: create a ChibiSexp wrapping a value, with GC preservation */
PyObject *ChibiSexp_wrap(sexp ctx, sexp value, PyObject *context) {
    ChibiSexpObject *obj = (ChibiSexpObject *)ChibiSexpType.tp_alloc(&ChibiSexpType, 0);
    if (!obj) return NULL;

    obj->value = value;
    sexp_preserve_object(ctx, value);

    Py_INCREF(context);
    obj->context = context;

    return (PyObject *)obj;
}
