/*  _chibi_context.c -- ChibiContext Python type  */

#include <Python.h>
#include <chibi/eval.h>
#include "_eval_parser_helpers.h"
#include "_chibi_convert.h"
#include "_chibi_pyobject_type.h"
#include "_chibi_bridge.h"
#include "_eval_writer.h"
#include "_chibi_serialize.h"
#include "_eval_pool.h"

/* The ChibiSexp type and wrapper function from _chibi_sexp.c */
extern PyTypeObject ChibiSexpType;
extern PyObject *ChibiSexp_wrap(sexp ctx, sexp value, PyObject *context);

/* Pool/channel registration (defined in _eval_pool.c) */
extern void register_channel_type(sexp ctx);
extern void register_pool_type(sexp ctx);
extern void register_pool_eval_functions(sexp ctx, sexp env);

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

    /* .scm files are loaded from embedded data via patched sexp_load_module_file.
     * No module path setup needed. */

    self->env = sexp_context_env(self->ctx);

    /* Load standard environment (this loads init-7.scm which defines list, modulo, etc.) */
    sexp_load_standard_env(self->ctx, self->env, SEXP_SEVEN);
    /* Pass NULL for ports — we handle I/O through Python's sys.stdout/stderr */
    sexp_load_standard_ports(self->ctx, self->env, NULL, NULL, NULL, 0);

    /* Register python-object type */
    register_pyobject_type(self->ctx);

    /* Register channel type (must be same order as workers for tag consistency) */
    register_channel_type(self->ctx);

    /* Register pool type (must be same order as workers for tag consistency) */
    register_pool_type(self->ctx);

    /* Register bridge functions */
    register_bridge_functions(self->ctx, self->env);

    /* Register pool/channel/future functions for pure-Eval use */
    register_pool_eval_functions(self->ctx, self->env);

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

    /* Base definitions: error-object aliases, byte I/O, string/IO fns, callcc */
    sexp_load_module_file(self->ctx, "eval/base.scm", self->env);
    self->env = sexp_context_env(self->ctx);

    /* filter and fold needed before extras.scm (workers get these from srfi/1) */
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

        /* Load scheme/test.scm — lightweight test framework */
        sexp_load_module_file(self->ctx, "scheme/test.scm", self->env);

        /* After loading, the context env may have shifted.
         * Update self->env to the current context env. */
        self->env = sexp_context_env(self->ctx);
    }

    /* Initialize all statically compiled chibi libraries.
     * Must be after extras.scm (provides define-record-type for types.scm). */
    {
        extern void eval_init_all_libs(sexp ctx, sexp env);
        eval_init_all_libs(self->ctx, self->env);
        self->env = sexp_context_env(self->ctx);
    }

    /* make-parameter and sort are now provided by srfi/39/syntax.scm
     * and srfi/95/sort.scm loaded in eval_init_all_libs. */

    /* SRFI-18 green thread wrappers */
    sexp_load_module_file(self->ctx, "eval/threads.scm", self->env);
    self->env = sexp_context_env(self->ctx);

    /* Underscore aliases for all libraries */
    sexp_load_module_file(self->ctx, "eval/aliases.scm", self->env);
    self->env = sexp_context_env(self->ctx);

    /* Dict runtime */
    sexp_load_module_file(self->ctx, "eval/dict.scm", self->env);
    self->env = sexp_context_env(self->ctx);

    /* Async/await runtime */
    sexp_load_module_file(self->ctx, "eval/async.scm", self->env);
    self->env = sexp_context_env(self->ctx);

    /* OO wrappers: Channel-wrap, Future-wrap, Pool (Eval syntax) */
    {
        extern void eval_load_eval_file(sexp ctx, sexp env, const char *path);

        eval_load_eval_file(self->ctx, self->env, "eval/threadpool.eval");
        self->env = sexp_context_env(self->ctx);

        /* Reactive runtime: Signal, Computed, Effect, batch, dispose */
        sexp_load_module_file(self->ctx, "eval/reactive.scm", self->env);
        self->env = sexp_context_env(self->ctx);

        /* OO networking: TcpSocket, TcpClient, TcpServer, HttpClient */
        eval_load_eval_file(self->ctx, self->env, "eval/net-oo.eval");
        self->env = sexp_context_env(self->ctx);
    }

    /* OO string methods: "hello"->upper(), etc. */
    sexp_load_module_file(self->ctx, "eval/string-oo.scm", self->env);
    self->env = sexp_context_env(self->ctx);

    /* OO list/vector methods: [1,2,3]->map(...), #[1,2]->length, etc. */
    sexp_load_module_file(self->ctx, "eval/collection-oo.scm", self->env);
    self->env = sexp_context_env(self->ctx);

    /* Abstract class support: global flag checked by abstract constructors */
    sexp_eval_string(self->ctx, "(define __abstract_ok__ #f)", -1, self->env);

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
