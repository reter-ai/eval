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
static sexp bridge_newline(sexp ctx, sexp self, sexp_sint_t n) {
    PyObject *py_stdout = PySys_GetObject("stdout");
    if (py_stdout) {
        PyObject *nl = PyUnicode_FromString("\n");
        PyObject_CallMethod(py_stdout, "write", "O", nl);
        Py_DECREF(nl);
    }
    return SEXP_VOID;
}

/* display(x) -> print to stdout (display semantics: no quotes on strings) */
static sexp bridge_display(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    PyObject *py_stdout = PySys_GetObject("stdout");
    if (!py_stdout) return SEXP_VOID;
    if (sexp_stringp(x)) {
        /* Display semantics: strings without quotes */
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
    return SEXP_VOID;
}

/* print(x) -> display + newline */
static sexp bridge_print(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    bridge_display(ctx, self, n, x);
    bridge_newline(ctx, self, 0);
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
    sexp_define_foreign(ctx, env, "display", 1, bridge_display);
    sexp_define_foreign(ctx, env, "print", 1, bridge_print);
    sexp_define_foreign(ctx, env, "newline", 0, bridge_newline);

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
