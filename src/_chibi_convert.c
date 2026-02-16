/*  _chibi_convert.c -- sexp <-> PyObject conversion  */

#include "_chibi_convert.h"
#include "_chibi_pyobject_type.h"

/* Forward declaration for ChibiSexp (defined in _chibi_sexp.c) */
extern PyTypeObject ChibiSexpType;

typedef struct {
    PyObject_HEAD
    sexp value;
    PyObject *context;  /* reference to ChibiContext to prevent GC */
} ChibiSexpObject;

/* Convert a chibi sexp to a Python object */
PyObject *sexp_to_pyobject(sexp ctx, sexp x) {
    /* void / unspecified -> None */
    if (x == SEXP_VOID || x == SEXP_UNDEF) {
        Py_RETURN_NONE;
    }

    /* null -> None */
    if (sexp_nullp(x)) {
        /* Return empty list */
        return PyList_New(0);
    }

    /* boolean */
    if (x == SEXP_TRUE) {
        Py_RETURN_TRUE;
    }
    if (x == SEXP_FALSE) {
        Py_RETURN_FALSE;
    }

    /* fixnum -> int */
    if (sexp_fixnump(x)) {
        return PyLong_FromLongLong(sexp_unbox_fixnum(x));
    }

    /* flonum -> float */
    if (sexp_flonump(x)) {
        return PyFloat_FromDouble(sexp_flonum_value(x));
    }

    /* bignum -> int */
#if SEXP_USE_BIGNUMS
    if (sexp_bignump(x)) {
        /* Convert via writing to a string port */
        sexp_gc_var2(out, str);
        sexp_gc_preserve2(ctx, out, str);
        out = sexp_open_output_string(ctx);
        sexp_write_bignum(ctx, x, out, 10);
        str = sexp_get_output_string(ctx, out);
        PyObject *result;
        if (sexp_stringp(str)) {
            result = PyLong_FromString(sexp_string_data(str), NULL, 10);
        } else {
            result = PyLong_FromLong(0);
        }
        sexp_gc_release2(ctx);
        return result;
    }
#endif

    /* string -> str */
    if (sexp_stringp(x)) {
        return PyUnicode_FromStringAndSize(sexp_string_data(x),
                                           sexp_string_size(x));
    }

    /* symbol -> str */
    if (sexp_symbolp(x)) {
        sexp sym_str = sexp_symbol_to_string(ctx, x);
        return PyUnicode_FromStringAndSize(sexp_string_data(sym_str),
                                           sexp_string_size(sym_str));
    }

    /* char -> str (single character) */
    if (sexp_charp(x)) {
        char c = sexp_unbox_character(x);
        return PyUnicode_FromStringAndSize(&c, 1);
    }

    /* bytes/bytevector -> bytes */
    if (sexp_bytesp(x)) {
        return PyBytes_FromStringAndSize(
            (const char *)sexp_bytes_data(x),
            sexp_bytes_length(x));
    }

    /* python-object -> unwrap */
    if (sexp_pyobjectp(x)) {
        PyObject *obj = unwrap_pyobject(x);
        if (obj) {
            Py_INCREF(obj);
            return obj;
        }
        Py_RETURN_NONE;
    }

    /* pair (list) -> list */
    if (sexp_pairp(x)) {
        /* Check if it's a proper list */
        sexp p = x;
        int len = 0;
        int is_proper = 1;
        while (sexp_pairp(p)) {
            len++;
            p = sexp_cdr(p);
        }
        if (!sexp_nullp(p)) is_proper = 0;

        if (is_proper) {
            PyObject *list = PyList_New(len);
            if (!list) return NULL;
            p = x;
            for (int i = 0; i < len; i++) {
                PyObject *elem = sexp_to_pyobject(ctx, sexp_car(p));
                if (!elem) {
                    Py_DECREF(list);
                    return NULL;
                }
                PyList_SET_ITEM(list, i, elem);
                p = sexp_cdr(p);
            }
            return list;
        } else {
            /* Dotted pair -> tuple of (list_items..., tail) */
            PyObject *items = PyList_New(0);
            p = x;
            while (sexp_pairp(p)) {
                PyObject *elem = sexp_to_pyobject(ctx, sexp_car(p));
                PyList_Append(items, elem);
                Py_DECREF(elem);
                p = sexp_cdr(p);
            }
            PyObject *tail = sexp_to_pyobject(ctx, p);
            PyList_Append(items, tail);
            Py_DECREF(tail);
            PyObject *result = PyList_AsTuple(items);
            Py_DECREF(items);
            return result;
        }
    }

    /* vector -> list */
    if (sexp_vectorp(x)) {
        int len = sexp_vector_length(x);
        PyObject *list = PyList_New(len);
        if (!list) return NULL;
        for (int i = 0; i < len; i++) {
            PyObject *elem = sexp_to_pyobject(ctx, sexp_vector_ref(x, sexp_make_fixnum(i)));
            if (!elem) {
                Py_DECREF(list);
                return NULL;
            }
            PyList_SET_ITEM(list, i, elem);
        }
        return list;
    }

    /* exception -> raise as Python exception */
    if (sexp_exceptionp(x)) {
        sexp msg = sexp_exception_message(x);
        if (sexp_stringp(msg)) {
            PyErr_SetString(PyExc_RuntimeError, sexp_string_data(msg));
        } else {
            PyErr_SetString(PyExc_RuntimeError, "Scheme exception");
        }
        return NULL;
    }

    /* For procedures and other types, return as ChibiSexp wrapper */
    /* We can't create ChibiSexp here without context reference,
       so return a string description */
    if (sexp_procedurep(x) || sexp_opcodep(x)) {
        return PyUnicode_FromString("<function>");
    }

    /* Default: return string representation */
    return PyUnicode_FromString("<scheme-object>");
}

/* Convert a Python object to a chibi sexp */
sexp pyobject_to_sexp(sexp ctx, PyObject *obj) {
    if (obj == NULL || obj == Py_None) {
        return SEXP_VOID;
    }

    /* bool (must check before int since bool is subclass of int) */
    if (PyBool_Check(obj)) {
        return obj == Py_True ? SEXP_TRUE : SEXP_FALSE;
    }

    /* int -> fixnum or bignum */
    if (PyLong_Check(obj)) {
        int overflow;
        long long val = PyLong_AsLongLongAndOverflow(obj, &overflow);
        if (overflow == 0 && !PyErr_Occurred()) {
            if (val >= SEXP_MIN_FIXNUM && val <= SEXP_MAX_FIXNUM) {
                return sexp_make_fixnum(val);
            }
        }
        PyErr_Clear();
        /* For large integers, convert via string */
        PyObject *str = PyObject_Str(obj);
        if (str) {
            const char *s = PyUnicode_AsUTF8(str);
            sexp result = sexp_eval_string(ctx, s, -1, NULL);
            Py_DECREF(str);
            return result;
        }
        return sexp_make_fixnum(0);
    }

    /* float -> flonum */
    if (PyFloat_Check(obj)) {
        return sexp_make_flonum(ctx, PyFloat_AsDouble(obj));
    }

    /* str -> string */
    if (PyUnicode_Check(obj)) {
        Py_ssize_t size;
        const char *data = PyUnicode_AsUTF8AndSize(obj, &size);
        if (data) {
            return sexp_c_string(ctx, data, size);
        }
        return sexp_c_string(ctx, "", 0);
    }

    /* bytes -> bytevector */
    if (PyBytes_Check(obj)) {
        Py_ssize_t size = PyBytes_Size(obj);
        const char *data = PyBytes_AsString(obj);
        sexp bv = sexp_make_bytes(ctx, sexp_make_fixnum(size), SEXP_UNDEF);
        if (sexp_bytesp(bv)) {
            memcpy(sexp_bytes_data(bv), data, size);
        }
        return bv;
    }

    /* ChibiSexp -> extract raw sexp */
    if (Py_TYPE(obj) == &ChibiSexpType) {
        ChibiSexpObject *sobj = (ChibiSexpObject *)obj;
        return sobj->value;
    }

    /* list -> scheme list */
    if (PyList_Check(obj)) {
        Py_ssize_t size = PyList_Size(obj);
        sexp result = SEXP_NULL;
        sexp_gc_var1(tmp);
        sexp_gc_preserve1(ctx, tmp);
        /* Build list in reverse */
        for (Py_ssize_t i = size - 1; i >= 0; i--) {
            tmp = pyobject_to_sexp(ctx, PyList_GetItem(obj, i));
            result = sexp_cons(ctx, tmp, result);
        }
        sexp_gc_release1(ctx);
        return result;
    }

    /* tuple -> scheme list */
    if (PyTuple_Check(obj)) {
        Py_ssize_t size = PyTuple_Size(obj);
        sexp result = SEXP_NULL;
        sexp_gc_var1(tmp);
        sexp_gc_preserve1(ctx, tmp);
        for (Py_ssize_t i = size - 1; i >= 0; i--) {
            tmp = pyobject_to_sexp(ctx, PyTuple_GetItem(obj, i));
            result = sexp_cons(ctx, tmp, result);
        }
        sexp_gc_release1(ctx);
        return result;
    }

    /* dict -> alist */
    if (PyDict_Check(obj)) {
        sexp result = SEXP_NULL;
        sexp_gc_var2(key_sexp, val_sexp);
        sexp_gc_preserve2(ctx, key_sexp, val_sexp);
        PyObject *key, *value;
        Py_ssize_t pos = 0;
        while (PyDict_Next(obj, &pos, &key, &value)) {
            key_sexp = pyobject_to_sexp(ctx, key);
            val_sexp = pyobject_to_sexp(ctx, value);
            result = sexp_cons(ctx, sexp_cons(ctx, key_sexp, val_sexp), result);
        }
        sexp_gc_release2(ctx);
        return result;
    }

    /* Everything else: wrap as python-object */
    return wrap_pyobject(ctx, obj);
}
