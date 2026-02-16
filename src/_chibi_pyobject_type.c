/*  _chibi_pyobject_type.c -- Custom <python-object> sexp type  */

#include "_chibi_pyobject_type.h"

sexp_tag_t pyobject_type_tag = 0;

/* Stored globally; set in register_pyobject_type */
static sexp pyobject_type_obj = SEXP_FALSE;

/* Weak ref to prevent GC */
static sexp pyobject_type_preserved = SEXP_FALSE;

/* Finalizer: called when chibi GCs the python-object sexp */
static sexp pyobject_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    if (sexp_pointerp(obj) && sexp_pointer_tag(obj) == pyobject_type_tag) {
        PyObject *pyobj = (PyObject *)sexp_cpointer_value(obj);
        if (pyobj) {
            Py_DECREF(pyobj);
            sexp_cpointer_value(obj) = NULL;
        }
    }
    return SEXP_VOID;
}

void register_pyobject_type(sexp ctx) {
    sexp_gc_var2(name, type);
    sexp_gc_preserve2(ctx, name, type);

    name = sexp_c_string(ctx, "python-object", -1);

    /* Use sexp_register_c_type which handles all the boilerplate fields */
    type = sexp_register_c_type(ctx, name, pyobject_finalize);

    if (sexp_typep(type)) {
        pyobject_type_tag = sexp_type_tag(type);
        pyobject_type_obj = type;
        sexp_preserve_object(ctx, type);
    }

    sexp_gc_release2(ctx);
}

sexp wrap_pyobject(sexp ctx, PyObject *obj) {
    if (!obj) return SEXP_VOID;

    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    Py_INCREF(obj);
    result = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), pyobject_type_tag);
    sexp_cpointer_value(result) = (void *)obj;
    sexp_cpointer_length(result) = 0;

    sexp_gc_release1(ctx);
    return result;
}

PyObject *unwrap_pyobject(sexp x) {
    if (sexp_pointerp(x) && sexp_pointer_tag(x) == pyobject_type_tag) {
        return (PyObject *)sexp_cpointer_value(x);
    }
    return NULL;
}

int sexp_pyobjectp(sexp x) {
    return sexp_pointerp(x) && sexp_pointer_tag(x) == pyobject_type_tag;
}
