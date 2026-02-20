/*  _chibi_bridge.c -- Built-in bridge functions for Eval<->Python  */

#include "_chibi_bridge.h"
#include "_chibi_convert.h"
#include "_chibi_pyobject_type.h"
#include "_eval_writer.h"

/* === Bridge function implementations === */

/* py_import(name) -> import a Python module */
static sexp bridge_py_import(sexp ctx, sexp self, sexp_sint_t n, sexp name) {
    if (!sexp_stringp(name)) {
        return sexp_user_exception(ctx, self, "py_import: expected string", name);
    }
    PyObject *mod = PyImport_ImportModule(sexp_string_data(name));
    if (!mod) {
        PyObject *type, *value, *tb;
        PyErr_Fetch(&type, &value, &tb);
        const char *msg = "import failed";
        if (value) {
            PyObject *str = PyObject_Str(value);
            if (str) msg = PyUnicode_AsUTF8(str);
            /* Note: str leaks here, acceptable for error path */
        }
        Py_XDECREF(type); Py_XDECREF(value); Py_XDECREF(tb);
        return sexp_user_exception(ctx, self, msg, name);
    }
    sexp result = wrap_pyobject(ctx, mod);
    Py_DECREF(mod);
    return result;
}

/* py_getattr(obj, attr) -> get attribute */
static sexp bridge_py_getattr(sexp ctx, sexp self, sexp_sint_t n,
                               sexp obj, sexp attr) {
    if (!sexp_pyobjectp(obj)) {
        return sexp_user_exception(ctx, self, "py_getattr: expected python object", obj);
    }
    if (!sexp_stringp(attr)) {
        return sexp_user_exception(ctx, self, "py_getattr: expected string attr", attr);
    }
    PyObject *pyobj = unwrap_pyobject(obj);
    PyObject *result = PyObject_GetAttrString(pyobj, sexp_string_data(attr));
    if (!result) {
        PyErr_Clear();
        return sexp_user_exception(ctx, self, "attribute not found", attr);
    }
    sexp s = pyobject_to_sexp(ctx, result);
    Py_DECREF(result);
    return s;
}

/* py_setattr(obj, attr, val) -> set attribute */
static sexp bridge_py_setattr(sexp ctx, sexp self, sexp_sint_t n,
                               sexp obj, sexp attr, sexp val) {
    if (!sexp_pyobjectp(obj)) {
        return sexp_user_exception(ctx, self, "py_setattr: expected python object", obj);
    }
    if (!sexp_stringp(attr)) {
        return sexp_user_exception(ctx, self, "py_setattr: expected string attr", attr);
    }
    PyObject *pyobj = unwrap_pyobject(obj);
    PyObject *pyval = sexp_to_pyobject(ctx, val);
    if (!pyval) return sexp_user_exception(ctx, self, "conversion failed", val);
    int r = PyObject_SetAttrString(pyobj, sexp_string_data(attr), pyval);
    Py_DECREF(pyval);
    if (r < 0) {
        PyErr_Clear();
        return sexp_user_exception(ctx, self, "setattr failed", attr);
    }
    return SEXP_VOID;
}

/* py_call(callable, args...) -> call Python callable */
static sexp bridge_py_call(sexp ctx, sexp self, sexp_sint_t n, sexp args) {
    if (!sexp_pairp(args)) {
        return sexp_user_exception(ctx, self, "py_call: expected callable", SEXP_NULL);
    }
    sexp callable_sexp = sexp_car(args);
    sexp rest = sexp_cdr(args);

    PyObject *callable = NULL;
    if (sexp_pyobjectp(callable_sexp)) {
        callable = unwrap_pyobject(callable_sexp);
    } else {
        return sexp_user_exception(ctx, self, "py_call: not a python callable", callable_sexp);
    }

    /* Count args */
    int nargs = 0;
    sexp p = rest;
    while (sexp_pairp(p)) { nargs++; p = sexp_cdr(p); }

    PyObject *pyargs = PyTuple_New(nargs);
    p = rest;
    for (int i = 0; i < nargs; i++) {
        PyObject *arg = sexp_to_pyobject(ctx, sexp_car(p));
        if (!arg) {
            Py_DECREF(pyargs);
            PyErr_Clear();
            return sexp_user_exception(ctx, self, "arg conversion failed", sexp_car(p));
        }
        PyTuple_SET_ITEM(pyargs, i, arg);
        p = sexp_cdr(p);
    }

    PyObject *result = PyObject_Call(callable, pyargs, NULL);
    Py_DECREF(pyargs);

    if (!result) {
        PyObject *type, *value, *tb;
        PyErr_Fetch(&type, &value, &tb);
        const char *msg = "Python call failed";
        if (value) {
            PyObject *str = PyObject_Str(value);
            if (str) msg = PyUnicode_AsUTF8(str);
        }
        Py_XDECREF(type); Py_XDECREF(value); Py_XDECREF(tb);
        return sexp_user_exception(ctx, self, msg, SEXP_NULL);
    }

    sexp s = pyobject_to_sexp(ctx, result);
    Py_DECREF(result);
    return s;
}

/* py_method(obj, name, args...) -> getattr + call shorthand */
static sexp bridge_py_method(sexp ctx, sexp self, sexp_sint_t n, sexp args) {
    if (!sexp_pairp(args) || !sexp_pairp(sexp_cdr(args))) {
        return sexp_user_exception(ctx, self, "py_method: expected obj and method name", SEXP_NULL);
    }
    sexp obj = sexp_car(args);
    sexp name_sexp = sexp_car(sexp_cdr(args));
    sexp rest = sexp_cdr(sexp_cdr(args));

    if (!sexp_pyobjectp(obj)) {
        return sexp_user_exception(ctx, self, "py_method: expected python object", obj);
    }
    if (!sexp_stringp(name_sexp)) {
        return sexp_user_exception(ctx, self, "py_method: expected string name", name_sexp);
    }

    PyObject *pyobj = unwrap_pyobject(obj);
    PyObject *method = PyObject_GetAttrString(pyobj, sexp_string_data(name_sexp));
    if (!method) {
        PyErr_Clear();
        return sexp_user_exception(ctx, self, "method not found", name_sexp);
    }

    /* Count remaining args */
    int nargs = 0;
    sexp p = rest;
    while (sexp_pairp(p)) { nargs++; p = sexp_cdr(p); }

    PyObject *pyargs = PyTuple_New(nargs);
    p = rest;
    for (int i = 0; i < nargs; i++) {
        PyObject *arg = sexp_to_pyobject(ctx, sexp_car(p));
        if (!arg) {
            Py_DECREF(pyargs);
            Py_DECREF(method);
            PyErr_Clear();
            return sexp_user_exception(ctx, self, "arg conversion failed", sexp_car(p));
        }
        PyTuple_SET_ITEM(pyargs, i, arg);
        p = sexp_cdr(p);
    }

    PyObject *result = PyObject_Call(method, pyargs, NULL);
    Py_DECREF(pyargs);
    Py_DECREF(method);

    if (!result) {
        PyObject *type, *value, *tb;
        PyErr_Fetch(&type, &value, &tb);
        const char *msg = "Python method call failed";
        if (value) {
            PyObject *str = PyObject_Str(value);
            if (str) msg = PyUnicode_AsUTF8(str);
        }
        Py_XDECREF(type); Py_XDECREF(value); Py_XDECREF(tb);
        return sexp_user_exception(ctx, self, msg, SEXP_NULL);
    }

    sexp s = pyobject_to_sexp(ctx, result);
    Py_DECREF(result);
    return s;
}

/* py_eval(code) -> evaluate Python expression */
static sexp bridge_py_eval(sexp ctx, sexp self, sexp_sint_t n, sexp code) {
    if (!sexp_stringp(code)) {
        return sexp_user_exception(ctx, self, "py_eval: expected string", code);
    }

    PyObject *main_mod = PyImport_AddModule("__main__");
    PyObject *main_dict = PyModule_GetDict(main_mod);
    PyObject *result = PyRun_String(sexp_string_data(code),
                                    Py_eval_input, main_dict, main_dict);
    if (!result) {
        PyErr_Clear();
        return sexp_user_exception(ctx, self, "Python eval failed", code);
    }

    sexp s = pyobject_to_sexp(ctx, result);
    Py_DECREF(result);
    return s;
}

/* py_object_p(x) -> check if wrapped Python object */
static sexp bridge_py_object_p(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    return sexp_pyobjectp(x) ? SEXP_TRUE : SEXP_FALSE;
}

/* newline() -> print bare newline to stdout */
/* Write a value to a chibi output port using display semantics */
static void display_to_port(sexp ctx, sexp x, sexp port) {
    if (sexp_stringp(x)) {
        sexp_write_string_n(ctx, sexp_string_data(x), sexp_string_size(x), port);
    } else if (sexp_charp(x)) {
        sexp_write_char(ctx, sexp_unbox_character(x), port);
    } else {
        sexp_write(ctx, x, port);
    }
}

/* Write a value to Python's sys.stdout using display semantics */
static void display_to_py_stdout(sexp ctx, sexp x) {
    PyObject *py_stdout = PySys_GetObject("stdout");
    if (!py_stdout) return;
    if (sexp_stringp(x)) {
        PyObject *str = PyUnicode_FromStringAndSize(
            sexp_string_data(x), sexp_string_size(x));
        if (str) {
            PyObject_CallMethod(py_stdout, "write", "O", str);
            Py_DECREF(str);
        }
    } else {
        PyObject *str = eval_write_to_string(ctx, x);
        if (str) {
            PyObject_CallMethod(py_stdout, "write", "O", str);
            Py_DECREF(str);
        }
    }
}

/* newline([port]) -> write newline to port or Python stdout */
static sexp bridge_newline(sexp ctx, sexp self, sexp_sint_t n, sexp args) {
    if (sexp_pairp(args) && sexp_oportp(sexp_car(args))) {
        sexp_write_char(ctx, '\n', sexp_car(args));
        return SEXP_VOID;
    }
    PyObject *py_stdout = PySys_GetObject("stdout");
    if (py_stdout) {
        PyObject *nl = PyUnicode_FromString("\n");
        PyObject_CallMethod(py_stdout, "write", "O", nl);
        Py_DECREF(nl);
    }
    return SEXP_VOID;
}

/* display(x [, port]) -> display to port or Python stdout */
static sexp bridge_display(sexp ctx, sexp self, sexp_sint_t n, sexp args) {
    if (!sexp_pairp(args)) return SEXP_VOID;
    sexp x = sexp_car(args);
    sexp rest = sexp_cdr(args);
    if (sexp_pairp(rest) && sexp_oportp(sexp_car(rest))) {
        display_to_port(ctx, x, sexp_car(rest));
        return SEXP_VOID;
    }
    display_to_py_stdout(ctx, x);
    return SEXP_VOID;
}

/* print(x) -> display + newline to Python stdout */
static sexp bridge_print(sexp ctx, sexp self, sexp_sint_t n, sexp args) {
    if (!sexp_pairp(args)) return SEXP_VOID;
    display_to_py_stdout(ctx, sexp_car(args));
    PyObject *py_stdout = PySys_GetObject("stdout");
    if (py_stdout) {
        PyObject *nl = PyUnicode_FromString("\n");
        PyObject_CallMethod(py_stdout, "write", "O", nl);
        Py_DECREF(nl);
    }
    return SEXP_VOID;
}

/* === Bitwise operations === */

static sexp bridge_bitwise_and(sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b) {
    if (!sexp_fixnump(a) || !sexp_fixnump(b))
        return sexp_user_exception(ctx, self, "bitwise-and: expected integers", a);
    return sexp_make_fixnum(sexp_unbox_fixnum(a) & sexp_unbox_fixnum(b));
}

static sexp bridge_bitwise_ior(sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b) {
    if (!sexp_fixnump(a) || !sexp_fixnump(b))
        return sexp_user_exception(ctx, self, "bitwise-ior: expected integers", a);
    return sexp_make_fixnum(sexp_unbox_fixnum(a) | sexp_unbox_fixnum(b));
}

static sexp bridge_bitwise_xor(sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b) {
    if (!sexp_fixnump(a) || !sexp_fixnump(b))
        return sexp_user_exception(ctx, self, "bitwise-xor: expected integers", a);
    return sexp_make_fixnum(sexp_unbox_fixnum(a) ^ sexp_unbox_fixnum(b));
}

static sexp bridge_bitwise_not(sexp ctx, sexp self, sexp_sint_t n, sexp a) {
    if (!sexp_fixnump(a))
        return sexp_user_exception(ctx, self, "bitwise-not: expected integer", a);
    return sexp_make_fixnum(~sexp_unbox_fixnum(a));
}

static sexp bridge_arithmetic_shift(sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b) {
    if (!sexp_fixnump(a) || !sexp_fixnump(b))
        return sexp_user_exception(ctx, self, "arithmetic-shift: expected integers", a);
    sexp_sint_t val = sexp_unbox_fixnum(a);
    sexp_sint_t shift = sexp_unbox_fixnum(b);
    if (shift >= 0)
        return sexp_make_fixnum(val << shift);
    else
        return sexp_make_fixnum(val >> (-shift));
}

static sexp bridge_shift_right(sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b) {
    if (!sexp_fixnump(a) || !sexp_fixnump(b))
        return sexp_user_exception(ctx, self, "shift-right: expected integers", a);
    sexp_sint_t val = sexp_unbox_fixnum(a);
    sexp_sint_t shift = sexp_unbox_fixnum(b);
    return sexp_make_fixnum(val >> shift);
}

/* === Generic indexing: ref(obj, idx) === */

static sexp bridge_ref(sexp ctx, sexp self, sexp_sint_t n, sexp obj, sexp idx) {
    if (!sexp_fixnump(idx))
        return sexp_user_exception(ctx, self, "ref: expected integer index", idx);
    sexp_sint_t i = sexp_unbox_fixnum(idx);

    if (sexp_vectorp(obj)) {
        sexp_sint_t len = (sexp_sint_t)sexp_vector_length(obj);
        if (i < 0) i += len;
        if (i < 0 || i >= len)
            return sexp_user_exception(ctx, self, "ref: index out of range", idx);
        return sexp_vector_data(obj)[i];
    } else if (sexp_stringp(obj)) {
        sexp_sint_t len = (sexp_sint_t)sexp_string_size(obj);
        if (i < 0) i += len;
        if (i < 0 || i >= len)
            return sexp_user_exception(ctx, self, "ref: index out of range", idx);
        return sexp_make_character((unsigned char)sexp_string_data(obj)[i]);
    } else if (sexp_pairp(obj)) {
        /* Compute list length and handle negative indices */
        sexp_sint_t len = 0;
        sexp p = obj;
        while (sexp_pairp(p)) { len++; p = sexp_cdr(p); }
        if (i < 0) i += len;
        if (i < 0 || i >= len)
            return sexp_user_exception(ctx, self, "ref: index out of range", idx);
        p = obj;
        for (sexp_sint_t k = 0; k < i; k++) p = sexp_cdr(p);
        return sexp_car(p);
    }
    return sexp_user_exception(ctx, self, "ref: expected list, vector, or string", obj);
}

/* === Generic slicing: slice(obj, start, end) === */

static sexp bridge_slice(sexp ctx, sexp self, sexp_sint_t n,
                          sexp obj, sexp start_s, sexp end_s) {
    sexp_sint_t len;

    /* Determine length */
    if (sexp_vectorp(obj)) {
        len = (sexp_sint_t)sexp_vector_length(obj);
    } else if (sexp_stringp(obj)) {
        len = (sexp_sint_t)sexp_string_size(obj);
    } else if (sexp_pairp(obj) || sexp_nullp(obj)) {
        len = 0;
        sexp p = obj;
        while (sexp_pairp(p)) { len++; p = sexp_cdr(p); }
    } else {
        return sexp_user_exception(ctx, self, "slice: expected list, vector, or string", obj);
    }

    /* Resolve start */
    sexp_sint_t start = 0;
    if (start_s != SEXP_FALSE) {
        if (!sexp_fixnump(start_s))
            return sexp_user_exception(ctx, self, "slice: expected integer start", start_s);
        start = sexp_unbox_fixnum(start_s);
        if (start < 0) start += len;
    }

    /* Resolve end */
    sexp_sint_t end = len;
    if (end_s != SEXP_FALSE) {
        if (!sexp_fixnump(end_s))
            return sexp_user_exception(ctx, self, "slice: expected integer end", end_s);
        end = sexp_unbox_fixnum(end_s);
        if (end < 0) end += len;
    }

    /* Clamp */
    if (start < 0) start = 0;
    if (end > len) end = len;
    if (start > end) start = end;

    if (sexp_vectorp(obj)) {
        sexp_sint_t new_len = end - start;
        sexp vec = sexp_make_vector(ctx, sexp_make_fixnum(new_len), SEXP_VOID);
        for (sexp_sint_t k = 0; k < new_len; k++)
            sexp_vector_data(vec)[k] = sexp_vector_data(obj)[start + k];
        return vec;
    } else if (sexp_stringp(obj)) {
        const char *data = sexp_string_data(obj);
        return sexp_c_string(ctx, data + start, end - start);
    } else {
        /* List: skip to start, collect until end */
        sexp p = obj;
        for (sexp_sint_t k = 0; k < start && sexp_pairp(p); k++)
            p = sexp_cdr(p);
        sexp result = SEXP_NULL;
        sexp tail = SEXP_NULL;
        for (sexp_sint_t k = start; k < end && sexp_pairp(p); k++) {
            sexp cell = sexp_cons(ctx, sexp_car(p), SEXP_NULL);
            if (sexp_nullp(result)) {
                result = cell;
                tail = cell;
            } else {
                sexp_cdr(tail) = cell;
                tail = cell;
            }
            p = sexp_cdr(p);
        }
        return result;
    }
}

/* __tostr__(x) -> fast value-to-string conversion for f-string interpolation */
static sexp bridge_tostr(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    if (sexp_stringp(x)) return x;
    if (sexp_fixnump(x)) {
        char buf[32];
        snprintf(buf, sizeof(buf), "%lld", (long long)sexp_unbox_fixnum(x));
        return sexp_c_string(ctx, buf, -1);
    }
    if (sexp_flonump(x)) {
        char buf[64];
        snprintf(buf, sizeof(buf), "%g", sexp_flonum_value(x));
        return sexp_c_string(ctx, buf, -1);
    }
    if (sexp_booleanp(x)) {
        return x == SEXP_TRUE ? sexp_c_string(ctx, "true", 4)
                              : sexp_c_string(ctx, "false", 5);
    }
    if (x == SEXP_NULL) {
        return sexp_c_string(ctx, "nil", 3);
    }
    if (sexp_symbolp(x)) {
        sexp str = sexp_symbol_to_string(ctx, x);
        return sexp_stringp(str) ? str : sexp_c_string(ctx, "?", 1);
    }
    if (sexp_charp(x)) {
        char buf[2];
        buf[0] = (char)sexp_unbox_character(x);
        buf[1] = '\0';
        return sexp_c_string(ctx, buf, 1);
    }
    /* DateTime/Date/TimeDelta → string */
    {
        extern sexp datetime_tostr(sexp ctx, sexp x);
        sexp r = datetime_tostr(ctx, x);
        if (r != SEXP_FALSE) return r;
    }
    /* Decimal → string */
    {
        extern sexp decimal_tostr(sexp ctx, sexp x);
        sexp r = decimal_tostr(ctx, x);
        if (r != SEXP_FALSE) return r;
    }
    /* Fallback: write-to-string (write semantics) */
    sexp str = sexp_write_to_string(ctx, x);
    return sexp_stringp(str) ? str : sexp_c_string(ctx, "?", 1);
}

/* === Registration === */

/* op("+"") → look up the Scheme procedure for an Eval operator symbol.
   Maps Eval operator names to their Scheme equivalents. */
static sexp bridge_op(sexp ctx, sexp self, sexp_sint_t n, sexp name) {
    if (!sexp_stringp(name))
        return sexp_user_exception(ctx, self, "op: expected string", name);

    const char *s = sexp_string_data(name);
    const char *scheme_name = NULL;

    /* Map Eval operator names to Scheme procedure names */
    if (strcmp(s, "+") == 0) scheme_name = "+";
    else if (strcmp(s, "-") == 0) scheme_name = "-";
    else if (strcmp(s, "*") == 0) scheme_name = "*";
    else if (strcmp(s, "/") == 0) scheme_name = "/";
    else if (strcmp(s, "%") == 0) scheme_name = "modulo";
    else if (strcmp(s, "**") == 0) scheme_name = "expt";
    else if (strcmp(s, "==") == 0) scheme_name = "equal?";
    else if (strcmp(s, "=?") == 0) scheme_name = "eq?";
    else if (strcmp(s, "<") == 0) scheme_name = "<";
    else if (strcmp(s, ">") == 0) scheme_name = ">";
    else if (strcmp(s, "<=") == 0) scheme_name = "<=";
    else if (strcmp(s, ">=") == 0) scheme_name = ">=";
    else if (strcmp(s, "!") == 0) scheme_name = "not";
    else if (strcmp(s, "&") == 0) scheme_name = "bitwise-and";
    else if (strcmp(s, "|") == 0) scheme_name = "bitwise-ior";
    else if (strcmp(s, "~") == 0) scheme_name = "bitwise-not";
    else if (strcmp(s, "<<") == 0) scheme_name = "arithmetic-shift";
    else if (strcmp(s, ">>") == 0) scheme_name = "shift-right";
    else {
        /* Try as a direct Scheme name (e.g. "modulo", "expt") */
        scheme_name = s;
    }

    sexp sym = sexp_intern(ctx, scheme_name, -1);
    sexp val = sexp_env_ref(ctx, sexp_context_env(ctx), sym, SEXP_VOID);
    if (val == SEXP_VOID)
        return sexp_user_exception(ctx, self, "op: unknown operator", name);
    return val;
}

/* exception-message — accessor for exception message field */
static sexp bridge_exception_message(sexp ctx, sexp self, sexp_sint_t n, sexp exn) {
    if (sexp_exceptionp(exn)) {
        sexp msg = sexp_exception_message(exn);
        return msg ? msg : sexp_c_string(ctx, "", -1);
    }
    return sexp_c_string(ctx, "", -1);
}

/* current-second — high-resolution wall-clock time in seconds */
#ifdef _WIN32
#include <windows.h>
static sexp bridge_current_second(sexp ctx, sexp self, sexp_sint_t n) {
    LARGE_INTEGER freq, count;
    QueryPerformanceFrequency(&freq);
    QueryPerformanceCounter(&count);
    return sexp_make_flonum(ctx, (double)count.QuadPart / (double)freq.QuadPart);
}
#else
#include <sys/time.h>
static sexp bridge_current_second(sexp ctx, sexp self, sexp_sint_t n) {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return sexp_make_flonum(ctx, tv.tv_sec + tv.tv_usec / 1000000.0);
}
#endif

/* Evaluate a Scheme expression string in an isolated eval context.
 * This creates a fresh sexp_eval_string call, so any call/cc captured
 * inside will have a clean continuation chain (no references to the
 * caller's bytecode/channels). */
sexp bridge_eval_scheme(sexp ctx, sexp self, sexp_sint_t n, sexp s) {
    if (!sexp_stringp(s))
        return sexp_type_exception(ctx, self, SEXP_STRING, s);
    return sexp_eval_string(ctx, sexp_string_data(s), sexp_string_size(s),
                            sexp_context_env(ctx));
}

void register_bridge_functions(sexp ctx, sexp env) {
    sexp_define_foreign(ctx, env, "py-import", 1, bridge_py_import);
    sexp_define_foreign(ctx, env, "py-getattr", 2, bridge_py_getattr);
    sexp_define_foreign(ctx, env, "py-setattr", 3, bridge_py_setattr);
    sexp_define_foreign_proc_rest(ctx, env, "py-call", 0, bridge_py_call);
    sexp_define_foreign_proc_rest(ctx, env, "py-method", 0, bridge_py_method);
    sexp_define_foreign(ctx, env, "py-eval", 1, bridge_py_eval);
    sexp_define_foreign(ctx, env, "py-object?", 1, bridge_py_object_p);
    sexp_define_foreign_proc_rest(ctx, env, "display", 0, bridge_display);
    sexp_define_foreign_proc_rest(ctx, env, "print", 0, bridge_print);
    sexp_define_foreign_proc_rest(ctx, env, "newline", 0, bridge_newline);

    /* Register with underscore aliases (Eval-friendly names) */
    sexp_define_foreign(ctx, env, "py_import", 1, bridge_py_import);
    sexp_define_foreign(ctx, env, "py_getattr", 2, bridge_py_getattr);
    sexp_define_foreign(ctx, env, "py_setattr", 3, bridge_py_setattr);
    sexp_define_foreign_proc_rest(ctx, env, "py_call", 0, bridge_py_call);
    sexp_define_foreign_proc_rest(ctx, env, "py_method", 0, bridge_py_method);
    sexp_define_foreign(ctx, env, "py_eval", 1, bridge_py_eval);
    sexp_define_foreign(ctx, env, "py_object_p", 1, bridge_py_object_p);

    /* Operator-as-value: op("+") → procedure for + */
    sexp_define_foreign(ctx, env, "op", 1, bridge_op);

    /* Bitwise operations */
    sexp_define_foreign(ctx, env, "bitwise-and", 2, bridge_bitwise_and);
    sexp_define_foreign(ctx, env, "bitwise-ior", 2, bridge_bitwise_ior);
    sexp_define_foreign(ctx, env, "bitwise-xor", 2, bridge_bitwise_xor);
    sexp_define_foreign(ctx, env, "bitwise-not", 1, bridge_bitwise_not);
    sexp_define_foreign(ctx, env, "arithmetic-shift", 2, bridge_arithmetic_shift);
    sexp_define_foreign(ctx, env, "shift-right", 2, bridge_shift_right);

    /* Exception accessor — exception-message is not an opcode in chibi */
    sexp_define_foreign(ctx, env, "exception-message", 1, bridge_exception_message);

    /* Timing */
    sexp_define_foreign(ctx, env, "current-second", 0, bridge_current_second);
    sexp_define_foreign(ctx, env, "current_second", 0, bridge_current_second);

    /* Evaluate Scheme expression string in isolated eval context */
    sexp_define_foreign(ctx, env, "eval-scheme", 1, bridge_eval_scheme);
    sexp_define_foreign(ctx, env, "eval_scheme", 1, bridge_eval_scheme);

    /* F-string tostr */
    sexp_define_foreign(ctx, env, "__tostr__", 1, bridge_tostr);

    /* Indexing and slicing */
    sexp_define_foreign(ctx, env, "ref", 2, bridge_ref);
    sexp_define_foreign(ctx, env, "slice", 3, bridge_slice);

    /* Continuation serialization (defined in _chibi_serialize.c) */
    extern sexp bridge_serialize_continuation(sexp, sexp, sexp_sint_t, sexp);
    extern sexp bridge_deserialize_continuation(sexp, sexp, sexp_sint_t, sexp);
    sexp_define_foreign(ctx, env, "serialize-continuation", 1,
                        bridge_serialize_continuation);
    sexp_define_foreign(ctx, env, "deserialize-continuation", 1,
                        bridge_deserialize_continuation);
    sexp_define_foreign(ctx, env, "serialize_continuation", 1,
                        bridge_serialize_continuation);
    sexp_define_foreign(ctx, env, "deserialize_continuation", 1,
                        bridge_deserialize_continuation);

}
