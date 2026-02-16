/*  _chibi_context.c -- ChibiContext Python type  */

#include <Python.h>
#include <chibi/eval.h>
#include "_eval_parser_helpers.h"
#include "_chibi_convert.h"
#include "_chibi_pyobject_type.h"
#include "_chibi_bridge.h"
#include "_eval_writer.h"
#include "_chibi_serialize.h"

/* The ChibiSexp type and wrapper function from _chibi_sexp.c */
extern PyTypeObject ChibiSexpType;
extern PyObject *ChibiSexp_wrap(sexp ctx, sexp value, PyObject *context);

/* === ChibiContext Python type === */

typedef struct {
    PyObject_HEAD
    sexp ctx;
    sexp env;
    int initialized;
} ChibiContextObject;

/* Get ctx from a ChibiContext (used by ChibiSexp) */
sexp ChibiContext_get_ctx(PyObject *context) {
    if (context && Py_TYPE(context)->tp_name &&
        strcmp(Py_TYPE(context)->tp_name, "chibi_eval._chibi.ChibiContext") == 0) {
        return ((ChibiContextObject *)context)->ctx;
    }
    return NULL;
}

/* Trampoline for Python callable registered as Scheme function */
typedef struct {
    PyObject *callable;
    int arity;
} TrampolineData;

static sexp trampoline_call(sexp ctx, sexp self, sexp_sint_t n, sexp arg0) {
    sexp data_sexp = sexp_opcode_data(self);
    if (!sexp_cpointerp(data_sexp)) {
        return sexp_user_exception(ctx, self, "invalid trampoline data", SEXP_NULL);
    }
    TrampolineData *td = (TrampolineData *)sexp_cpointer_value(data_sexp);
    if (!td || !td->callable) {
        return sexp_user_exception(ctx, self, "trampoline: no callable", SEXP_NULL);
    }

    /* Collect arguments based on arity */
    PyObject *pyargs;
    if (td->arity < 0) {
        /* Variadic: arg0 is the args list */
        int nargs = 0;
        sexp p = arg0;
        while (sexp_pairp(p)) { nargs++; p = sexp_cdr(p); }
        pyargs = PyTuple_New(nargs);
        p = arg0;
        for (int i = 0; i < nargs; i++) {
            PyObject *a = sexp_to_pyobject(ctx, sexp_car(p));
            if (!a) { Py_DECREF(pyargs); PyErr_Clear(); return SEXP_VOID; }
            PyTuple_SET_ITEM(pyargs, i, a);
            p = sexp_cdr(p);
        }
    } else if (td->arity == 0) {
        pyargs = PyTuple_New(0);
    } else if (td->arity == 1) {
        pyargs = PyTuple_New(1);
        PyObject *a = sexp_to_pyobject(ctx, arg0);
        if (!a) { Py_DECREF(pyargs); PyErr_Clear(); return SEXP_VOID; }
        PyTuple_SET_ITEM(pyargs, 0, a);
    } else {
        /* For arity > 1, arg0 is the first arg, rest are from stack */
        /* In practice, sexp_define_foreign only gives us arg0 */
        /* For multi-arg, we need sexp_define_foreign with correct arity */
        pyargs = PyTuple_New(1);
        PyObject *a = sexp_to_pyobject(ctx, arg0);
        if (!a) { Py_DECREF(pyargs); PyErr_Clear(); return SEXP_VOID; }
        PyTuple_SET_ITEM(pyargs, 0, a);
    }

    PyObject *result = PyObject_Call(td->callable, pyargs, NULL);
    Py_DECREF(pyargs);

    if (!result) {
        PyObject *type, *value, *tb;
        PyErr_Fetch(&type, &value, &tb);
        const char *msg = "Python function raised exception";
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

/* Multi-arg trampolines */
static sexp trampoline_call2(sexp ctx, sexp self, sexp_sint_t n,
                              sexp a1, sexp a2) {
    sexp data_sexp = sexp_opcode_data(self);
    if (!sexp_cpointerp(data_sexp)) {
        return sexp_user_exception(ctx, self, "invalid trampoline data", SEXP_NULL);
    }
    TrampolineData *td = (TrampolineData *)sexp_cpointer_value(data_sexp);

    PyObject *pyargs = PyTuple_New(2);
    PyObject *pa1 = sexp_to_pyobject(ctx, a1);
    PyObject *pa2 = sexp_to_pyobject(ctx, a2);
    if (!pa1 || !pa2) {
        Py_XDECREF(pa1); Py_XDECREF(pa2); Py_DECREF(pyargs);
        PyErr_Clear();
        return SEXP_VOID;
    }
    PyTuple_SET_ITEM(pyargs, 0, pa1);
    PyTuple_SET_ITEM(pyargs, 1, pa2);

    PyObject *result = PyObject_Call(td->callable, pyargs, NULL);
    Py_DECREF(pyargs);

    if (!result) { PyErr_Clear(); return SEXP_VOID; }
    sexp s = pyobject_to_sexp(ctx, result);
    Py_DECREF(result);
    return s;
}

static sexp trampoline_call3(sexp ctx, sexp self, sexp_sint_t n,
                              sexp a1, sexp a2, sexp a3) {
    sexp data_sexp = sexp_opcode_data(self);
    if (!sexp_cpointerp(data_sexp)) {
        return sexp_user_exception(ctx, self, "invalid trampoline data", SEXP_NULL);
    }
    TrampolineData *td = (TrampolineData *)sexp_cpointer_value(data_sexp);

    PyObject *pyargs = PyTuple_New(3);
    PyObject *pa1 = sexp_to_pyobject(ctx, a1);
    PyObject *pa2 = sexp_to_pyobject(ctx, a2);
    PyObject *pa3 = sexp_to_pyobject(ctx, a3);
    if (!pa1 || !pa2 || !pa3) {
        Py_XDECREF(pa1); Py_XDECREF(pa2); Py_XDECREF(pa3);
        Py_DECREF(pyargs);
        PyErr_Clear();
        return SEXP_VOID;
    }
    PyTuple_SET_ITEM(pyargs, 0, pa1);
    PyTuple_SET_ITEM(pyargs, 1, pa2);
    PyTuple_SET_ITEM(pyargs, 2, pa3);

    PyObject *result = PyObject_Call(td->callable, pyargs, NULL);
    Py_DECREF(pyargs);

    if (!result) { PyErr_Clear(); return SEXP_VOID; }
    sexp s = pyobject_to_sexp(ctx, result);
    Py_DECREF(result);
    return s;
}

/* Variadic trampoline */
static sexp trampoline_call_rest(sexp ctx, sexp self, sexp_sint_t n, sexp args) {
    return trampoline_call(ctx, self, n, args);
}

/* === ChibiContext methods === */

static int ChibiContext_init(ChibiContextObject *self, PyObject *args, PyObject *kwds) {
    static char *kwlist[] = {"heap_size", "max_heap_size", NULL};
    unsigned long heap_size = 0, max_heap_size = 0;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "|kk", kwlist,
                                      &heap_size, &max_heap_size))
        return -1;

    sexp_scheme_init();

    self->ctx = sexp_make_eval_context(NULL, NULL, NULL,
                                        heap_size, max_heap_size);
    if (!self->ctx) {
        PyErr_SetString(PyExc_RuntimeError, "Failed to create chibi context");
        return -1;
    }

    /* Set up module path BEFORE loading standard env (init-7.scm needs to be found).
     * We look for the chibi-scheme lib dir in multiple locations:
     *   1. chibi_eval/_scheme_lib (installed package)
     *   2. Relative to chibi_eval/__init__.py: ../chibi-scheme/lib (dev / editable)
     *   3. CHIBI_MODULE_PATH env var (user override)
     */
    {
        sexp_gc_var1(path_sexp);
        sexp_gc_preserve1(self->ctx, path_sexp);

        PyObject *chibi_eval_mod = PyImport_ImportModule("chibi_eval");
        if (chibi_eval_mod) {
            PyObject *file_attr = PyObject_GetAttrString(chibi_eval_mod, "__file__");
            if (file_attr) {
                const char *init_path = PyUnicode_AsUTF8(file_attr);
                if (init_path) {
                    char pkg_dir[1024];
                    strncpy(pkg_dir, init_path, sizeof(pkg_dir) - 1);
                    pkg_dir[sizeof(pkg_dir) - 1] = '\0';
                    char *last_sep = strrchr(pkg_dir, '/');
                    if (!last_sep) last_sep = strrchr(pkg_dir, '\\');
                    if (last_sep) {
                        *last_sep = '\0';
                        char full_path[1200];

                        /* Try _scheme_lib under the package */
                        snprintf(full_path, sizeof(full_path), "%s/_scheme_lib", pkg_dir);
                        path_sexp = sexp_c_string(self->ctx, full_path, -1);
                        sexp_add_module_directory(self->ctx, path_sexp, SEXP_TRUE);

                        /* Also try chibi-scheme/lib relative to parent dir (dev install) */
                        snprintf(full_path, sizeof(full_path), "%s/../chibi-scheme/lib", pkg_dir);
                        path_sexp = sexp_c_string(self->ctx, full_path, -1);
                        sexp_add_module_directory(self->ctx, path_sexp, SEXP_TRUE);
                    }
                }
                Py_DECREF(file_attr);
            }
            Py_DECREF(chibi_eval_mod);
        }
        PyErr_Clear();

        sexp_gc_release1(self->ctx);
    }

    self->env = sexp_context_env(self->ctx);

    /* Load standard environment (this loads init-7.scm which defines list, modulo, etc.) */
    sexp_load_standard_env(self->ctx, self->env, SEXP_SEVEN);
    /* Pass NULL for ports — we handle I/O through Python's sys.stdout/stderr */
    sexp_load_standard_ports(self->ctx, self->env, NULL, NULL, NULL, 0);

    /* Register python-object type */
    register_pyobject_type(self->ctx);

    /* Register bridge functions */
    register_bridge_functions(self->ctx, self->env);

    /* Import (scheme base) via the meta-environment — this adds:
     * error-object?, error-object-message, square, boolean=?, symbol=?,
     * string-map, string-for-each, vector-map, vector-for-each,
     * vector-append, vector->string, string->vector, call-with-port,
     * eof-object, read-line, read-string, write-string, read-u8, peek-u8,
     * write-u8, make-parameter, parameterize, etc. */
    {
        sexp meta = sexp_global(self->ctx, SEXP_G_META_ENV);
        if (!meta || !sexp_envp(meta))
            meta = sexp_context_env(self->ctx);

        sexp_gc_var2(mod_env, tmp);
        sexp_gc_preserve2(self->ctx, mod_env, tmp);

        sexp_gc_release2(self->ctx);
    }

    /* === Pre-define functions that extras.scm and other files depend on === */

    /* Aliases that extras.scm expects (normally set by module rename) */
    sexp_eval_string(self->ctx,
        "(define error-object? exception?)", -1, self->env);

    /* error-object-message via C bridge (exception-message registered in bridge) */
    sexp_eval_string(self->ctx,
        "(define error-object-message exception-message)", -1, self->env);

    /* Byte I/O — pure Scheme via char I/O (needed before extras.scm) */
    sexp_eval_string(self->ctx,
        "(define (read-u8 . o)"
        "  (let ((ch (if (pair? o) (read-char (car o)) (read-char))))"
        "    (if (eof-object? ch) ch (char->integer ch))))", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define (write-u8 byte . o)"
        "  (if (pair? o)"
        "    (write-char (integer->char byte) (car o))"
        "    (write-char (integer->char byte))))", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define (peek-u8 . o)"
        "  (let ((ch (if (pair? o) (peek-char (car o)) (peek-char))))"
        "    (if (eof-object? ch) ch (char->integer ch))))", -1, self->env);

    /* String functions from (chibi string) — pure Scheme implementations */
    sexp_eval_string(self->ctx,
        "(define (string-map proc str)"
        "  (list->string (map proc (string->list str))))", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define (string-for-each proc str)"
        "  (for-each proc (string->list str)))", -1, self->env);

    /* I/O functions — pure Scheme implementations */
    sexp_eval_string(self->ctx,
        "(define (write-string str . o)"
        "  (let ((port (if (pair? o) (car o) (current-output-port))))"
        "    (let lp ((i 0))"
        "      (if (< i (string-length str))"
        "          (begin (write-char (string-ref str i) port)"
        "                 (lp (+ i 1)))))))", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define (read-line . o)"
        "  (let ((port (if (pair? o) (car o) (current-input-port))))"
        "    (let lp ((res '()))"
        "      (let ((ch (read-char port)))"
        "        (cond ((eof-object? ch)"
        "               (if (null? res) ch (list->string (reverse res))))"
        "              ((eqv? ch #\\newline)"
        "               (list->string (reverse res)))"
        "              ((eqv? ch #\\return)"
        "               (let ((next (peek-char port)))"
        "                 (if (eqv? next #\\newline) (read-char port))"
        "                 (list->string (reverse res))))"
        "              (else (lp (cons ch res))))))))", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define (read-string n . o)"
        "  (let ((port (if (pair? o) (car o) (current-input-port))))"
        "    (let lp ((i 0) (res '()))"
        "      (if (>= i n)"
        "          (list->string (reverse res))"
        "          (let ((ch (read-char port)))"
        "            (if (eof-object? ch)"
        "                (if (null? res) ch (list->string (reverse res)))"
        "                (lp (+ i 1) (cons ch res))))))))", -1, self->env);

    /* Core aliases */
    sexp_eval_string(self->ctx,
        "(define callcc call-with-current-continuation)", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define (filter pred lst)"
        "  (cond ((null? lst) '())"
        "        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))"
        "        (else (filter pred (cdr lst)))))", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define (fold proc init lst)"
        "  (if (null? lst) init"
        "      (fold proc (proc (car lst) init) (cdr lst))))", -1, self->env);

    /* === Now load library files that may reference the above === */
    {
        /* Load scheme/extras.scm — square, boolean=?, symbol=?,
         * call/cc, vector-append, vector-map, etc. */
        sexp_load_module_file(self->ctx, "scheme/extras.scm", self->env);

        /* Note: SRFI-39 and SRFI-95 .scm files depend on C extensions
         * that aren't available with SEXP_USE_DL=0. Pure Scheme implementations
         * are provided below instead. */

        /* After loading, the context env may have shifted.
         * Update self->env to the current context env. */
        self->env = sexp_context_env(self->ctx);
    }

    /* make-parameter — pure Scheme closure-based implementation */
    sexp_eval_string(self->ctx,
        "(define (make-parameter init . o)"
        "  (let ((value (if (and (pair? o) (car o)) ((car o) init) init))"
        "        (converter (and (pair? o) (car o))))"
        "    (lambda args"
        "      (cond ((null? args) value)"
        "            (else"
        "             (set! value (if converter (converter (car args)) (car args)))"
        "             value)))))", -1, self->env);

    /* sort — pure Scheme merge sort */
    sexp_eval_string(self->ctx,
        "(define (sort lst less)"
        "  (define (merge a b)"
        "    (cond ((null? a) b)"
        "          ((null? b) a)"
        "          ((less (car a) (car b))"
        "           (cons (car a) (merge (cdr a) b)))"
        "          (else"
        "           (cons (car b) (merge a (cdr b))))))"
        "  (define (msort lst n)"
        "    (if (<= n 1)"
        "        (if (= n 1) (list (car lst)) '())"
        "        (let* ((mid (quotient n 2))"
        "               (left (msort lst mid))"
        "               (rest (list-tail lst mid))"
        "               (right (msort rest (- n mid))))"
        "          (merge left right))))"
        "  (if (vector? lst)"
        "      (list->vector (msort (vector->list lst) (vector-length lst)))"
        "      (msort lst (length lst))))", -1, self->env);

    self->initialized = 1;
    return 0;
}

static void ChibiContext_dealloc(ChibiContextObject *self) {
    if (self->ctx) {
        sexp_destroy_context(self->ctx);
        self->ctx = NULL;
    }
    Py_TYPE(self)->tp_free((PyObject *)self);
}

/* eval(code) -> parse Eval code, evaluate, return Python object */
static PyObject *ChibiContext_eval(ChibiContextObject *self, PyObject *args) {
    const char *code;
    if (!PyArg_ParseTuple(args, "s", &code))
        return NULL;

    if (!self->initialized) {
        PyErr_SetString(PyExc_RuntimeError, "Context not initialized");
        return NULL;
    }

    /* Parse Eval code */
    char *error_msg = NULL;
    int error_line = 0, error_col = 0;

    sexp_gc_var2(parsed, result);
    sexp_gc_preserve2(self->ctx, parsed, result);

    parsed = eval_parse(self->ctx, self->env, code,
                        &error_msg, &error_line, &error_col);

    if (error_msg) {
        PyObject *err_type = PyExc_SyntaxError;
        /* Import EvalSyntaxError from chibi_eval if available */
        PyObject *mod = PyImport_ImportModule("chibi_eval");
        if (mod) {
            PyObject *exc = PyObject_GetAttrString(mod, "EvalSyntaxError");
            if (exc) { err_type = exc; }
            Py_DECREF(mod);
        }
        PyErr_Clear();
        PyErr_SetString(err_type, error_msg);
        if (err_type != PyExc_SyntaxError) Py_DECREF(err_type);
        free(error_msg);
        sexp_gc_release2(self->ctx);
        return NULL;
    }

    if (parsed == SEXP_VOID) {
        sexp_gc_release2(self->ctx);
        Py_RETURN_NONE;
    }

    /* Evaluate */
    result = sexp_eval(self->ctx, parsed, self->env);

    sexp_gc_release2(self->ctx);

    if (sexp_exceptionp(result)) {
        sexp msg = sexp_exception_message(result);
        if (sexp_stringp(msg))
            PyErr_SetString(PyExc_RuntimeError, sexp_string_data(msg));
        else
            PyErr_SetString(PyExc_RuntimeError, "Scheme evaluation error");
        return NULL;
    }

    return sexp_to_pyobject(self->ctx, result);
}

/* eval_raw(code) -> parse + eval, return ChibiSexp */
static PyObject *ChibiContext_eval_raw(ChibiContextObject *self, PyObject *args) {
    const char *code;
    if (!PyArg_ParseTuple(args, "s", &code))
        return NULL;

    if (!self->initialized) {
        PyErr_SetString(PyExc_RuntimeError, "Context not initialized");
        return NULL;
    }

    char *error_msg = NULL;
    int error_line = 0, error_col = 0;

    sexp_gc_var2(parsed, result);
    sexp_gc_preserve2(self->ctx, parsed, result);

    parsed = eval_parse(self->ctx, self->env, code,
                        &error_msg, &error_line, &error_col);

    if (error_msg) {
        PyErr_SetString(PyExc_SyntaxError, error_msg);
        free(error_msg);
        sexp_gc_release2(self->ctx);
        return NULL;
    }

    if (parsed == SEXP_VOID) {
        sexp_gc_release2(self->ctx);
        return ChibiSexp_wrap(self->ctx, SEXP_VOID, (PyObject *)self);
    }

    result = sexp_eval(self->ctx, parsed, self->env);
    sexp_gc_release2(self->ctx);

    if (sexp_exceptionp(result)) {
        sexp msg = sexp_exception_message(result);
        if (sexp_stringp(msg))
            PyErr_SetString(PyExc_RuntimeError, sexp_string_data(msg));
        else
            PyErr_SetString(PyExc_RuntimeError, "Scheme evaluation error");
        return NULL;
    }

    return ChibiSexp_wrap(self->ctx, result, (PyObject *)self);
}

/* apply(proc, *args) */
static PyObject *ChibiContext_apply(ChibiContextObject *self, PyObject *args) {
    PyObject *proc_obj;
    PyObject *apply_args;
    if (!PyArg_ParseTuple(args, "OO", &proc_obj, &apply_args))
        return NULL;

    sexp_gc_var3(proc, sargs, result);
    sexp_gc_preserve3(self->ctx, proc, sargs, result);

    proc = pyobject_to_sexp(self->ctx, proc_obj);
    sargs = pyobject_to_sexp(self->ctx, apply_args);
    result = sexp_apply(self->ctx, proc, sargs);

    sexp_gc_release3(self->ctx);

    if (sexp_exceptionp(result)) {
        sexp msg = sexp_exception_message(result);
        if (sexp_stringp(msg))
            PyErr_SetString(PyExc_RuntimeError, sexp_string_data(msg));
        else
            PyErr_SetString(PyExc_RuntimeError, "apply failed");
        return NULL;
    }

    return sexp_to_pyobject(self->ctx, result);
}

/* define(name, value) */
static PyObject *ChibiContext_define(ChibiContextObject *self, PyObject *args) {
    const char *name;
    PyObject *value;
    if (!PyArg_ParseTuple(args, "sO", &name, &value))
        return NULL;

    sexp_gc_var2(sym, val);
    sexp_gc_preserve2(self->ctx, sym, val);

    sym = sexp_intern(self->ctx, name, -1);
    val = pyobject_to_sexp(self->ctx, value);
    sexp_env_define(self->ctx, self->env, sym, val);

    sexp_gc_release2(self->ctx);
    Py_RETURN_NONE;
}

/* define_function(name, func, arity) */
static PyObject *ChibiContext_define_function(ChibiContextObject *self, PyObject *args) {
    const char *name;
    PyObject *func;
    int arity = -1;
    if (!PyArg_ParseTuple(args, "sO|i", &name, &func, &arity))
        return NULL;

    if (!PyCallable_Check(func)) {
        PyErr_SetString(PyExc_TypeError, "func must be callable");
        return NULL;
    }

    /* Create trampoline data */
    TrampolineData *td = (TrampolineData *)malloc(sizeof(TrampolineData));
    td->callable = func;
    Py_INCREF(func);
    td->arity = arity;

    /* Create cpointer to hold the trampoline data */
    sexp_gc_var1(data_sexp);
    sexp_gc_preserve1(self->ctx, data_sexp);

    data_sexp = sexp_make_cpointer(self->ctx, SEXP_CPOINTER, td, SEXP_FALSE, 0);

    /* Register based on arity */
    if (arity < 0) {
        /* Variadic */
        sexp_define_foreign_proc_aux(self->ctx, self->env, name,
                                      0, SEXP_PROC_VARIADIC,
                                      name, (sexp_proc1)trampoline_call_rest,
                                      data_sexp);
    } else if (arity == 0) {
        sexp_define_foreign_aux(self->ctx, self->env, name,
                                0, 0, name, (sexp_proc1)trampoline_call,
                                data_sexp);
    } else if (arity == 1) {
        sexp_define_foreign_aux(self->ctx, self->env, name,
                                1, 0, name, (sexp_proc1)trampoline_call,
                                data_sexp);
    } else if (arity == 2) {
        sexp_define_foreign_aux(self->ctx, self->env, name,
                                2, 0, name, (sexp_proc1)trampoline_call2,
                                data_sexp);
    } else if (arity == 3) {
        sexp_define_foreign_aux(self->ctx, self->env, name,
                                3, 0, name, (sexp_proc1)trampoline_call3,
                                data_sexp);
    } else {
        /* For higher arities, use variadic */
        sexp_define_foreign_proc_aux(self->ctx, self->env, name,
                                      0, SEXP_PROC_VARIADIC,
                                      name, (sexp_proc1)trampoline_call_rest,
                                      data_sexp);
    }

    sexp_gc_release1(self->ctx);
    Py_RETURN_NONE;
}

/* lookup(name) */
static PyObject *ChibiContext_lookup(ChibiContextObject *self, PyObject *args) {
    const char *name;
    if (!PyArg_ParseTuple(args, "s", &name))
        return NULL;

    sexp_gc_var2(sym, val);
    sexp_gc_preserve2(self->ctx, sym, val);

    sym = sexp_intern(self->ctx, name, -1);
    val = sexp_env_ref(self->ctx, self->env, sym, SEXP_VOID);

    sexp_gc_release2(self->ctx);

    if (val == SEXP_VOID) {
        PyErr_Format(PyExc_KeyError, "undefined variable: %s", name);
        return NULL;
    }

    return sexp_to_pyobject(self->ctx, val);
}

/* __getitem__(name) */
static PyObject *ChibiContext_getitem(ChibiContextObject *self, PyObject *key) {
    if (!PyUnicode_Check(key)) {
        PyErr_SetString(PyExc_TypeError, "key must be a string");
        return NULL;
    }
    const char *name = PyUnicode_AsUTF8(key);
    PyObject *args = Py_BuildValue("(s)", name);
    PyObject *result = ChibiContext_lookup(self, args);
    Py_DECREF(args);
    return result;
}

/* __setitem__(name, value) */
static int ChibiContext_setitem(ChibiContextObject *self, PyObject *key, PyObject *value) {
    if (!PyUnicode_Check(key)) {
        PyErr_SetString(PyExc_TypeError, "key must be a string");
        return -1;
    }
    const char *name = PyUnicode_AsUTF8(key);
    PyObject *args = Py_BuildValue("(sO)", name, value);
    PyObject *result = ChibiContext_define(self, args);
    Py_DECREF(args);
    if (!result) return -1;
    Py_DECREF(result);
    return 0;
}

static PyMappingMethods ChibiContext_mapping = {
    .mp_subscript = (binaryfunc)ChibiContext_getitem,
    .mp_ass_subscript = (objobjargproc)ChibiContext_setitem,
};

/* ChibiSexpObject struct (mirrors _chibi_sexp.c) for field access */
typedef struct {
    PyObject_HEAD
    sexp value;
    PyObject *context;
} ChibiSexpObject;

/* serialize_continuation(sexp) -> bytes */
static PyObject *ChibiContext_serialize_continuation(ChibiContextObject *self, PyObject *args) {
    PyObject *sexp_obj;
    if (!PyArg_ParseTuple(args, "O", &sexp_obj))
        return NULL;

    if (!self->initialized) {
        PyErr_SetString(PyExc_RuntimeError, "Context not initialized");
        return NULL;
    }

    /* sexp_obj must be a ChibiSexp */
    if (Py_TYPE(sexp_obj) != &ChibiSexpType) {
        PyErr_SetString(PyExc_TypeError,
                        "serialize_continuation: argument must be a ChibiSexp");
        return NULL;
    }
    ChibiSexpObject *sobj = (ChibiSexpObject *)sexp_obj;
    return chibi_serialize_continuation(self->ctx, self->env, sobj->value);
}

/* deserialize_continuation(bytes) -> ChibiSexp */
static PyObject *ChibiContext_deserialize_continuation(ChibiContextObject *self, PyObject *args) {
    const unsigned char *data;
    Py_ssize_t len;
    if (!PyArg_ParseTuple(args, "y#", &data, &len))
        return NULL;

    if (!self->initialized) {
        PyErr_SetString(PyExc_RuntimeError, "Context not initialized");
        return NULL;
    }

    sexp result = chibi_deserialize_continuation(self->ctx, self->env, data, len);
    if (result == SEXP_FALSE && PyErr_Occurred())
        return NULL;

    return ChibiSexp_wrap(self->ctx, result, (PyObject *)self);
}

static PyMethodDef ChibiContext_methods[] = {
    {"eval", (PyCFunction)ChibiContext_eval, METH_VARARGS,
     "Evaluate Eval code and return Python result"},
    {"eval_raw", (PyCFunction)ChibiContext_eval_raw, METH_VARARGS,
     "Evaluate Eval code and return ChibiSexp"},
    {"apply", (PyCFunction)ChibiContext_apply, METH_VARARGS,
     "Apply a procedure to arguments"},
    {"define", (PyCFunction)ChibiContext_define, METH_VARARGS,
     "Define a variable in the environment"},
    {"define_function", (PyCFunction)ChibiContext_define_function, METH_VARARGS,
     "Register a Python callable as a Scheme function"},
    {"lookup", (PyCFunction)ChibiContext_lookup, METH_VARARGS,
     "Look up a variable value"},
    {"serialize_continuation", (PyCFunction)ChibiContext_serialize_continuation,
     METH_VARARGS, "Serialize a continuation to bytes"},
    {"deserialize_continuation", (PyCFunction)ChibiContext_deserialize_continuation,
     METH_VARARGS, "Deserialize a continuation from bytes"},
    {NULL}
};

PyTypeObject ChibiContextType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "chibi_eval._chibi.ChibiContext",
    .tp_doc = "Chibi-scheme evaluation context with Eval language parser",
    .tp_basicsize = sizeof(ChibiContextObject),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_new = PyType_GenericNew,
    .tp_init = (initproc)ChibiContext_init,
    .tp_dealloc = (destructor)ChibiContext_dealloc,
    .tp_methods = ChibiContext_methods,
    .tp_as_mapping = &ChibiContext_mapping,
};
