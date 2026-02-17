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

/* eval_set_module_path2 is defined in _eval_pool.c but not in header */
extern void eval_set_module_path2(const char *path);

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

                        /* Cache for worker threads */
                        eval_set_module_path(full_path);

                        /* Also try chibi-scheme/lib relative to parent dir (dev install) */
                        snprintf(full_path, sizeof(full_path), "%s/../chibi-scheme/lib", pkg_dir);
                        path_sexp = sexp_c_string(self->ctx, full_path, -1);
                        sexp_add_module_directory(self->ctx, path_sexp, SEXP_TRUE);

                        /* Cache for worker threads */
                        eval_set_module_path2(full_path);
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

    /* SRFI-18 green thread wrappers (simplified, no timeval dependency) */
    sexp_eval_string(self->ctx,
        "(define thread-yield! yield!)", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define (thread-join! thread . o)"
        "  (let ((timeout (and (pair? o) (car o))))"
        "    (let lp ()"
        "      (cond"
        "       ((%thread-join! thread timeout)"
        "        (if (%thread-exception? thread)"
        "            (raise (%thread-end-result thread))"
        "            (%thread-end-result thread)))"
        "       (else"
        "        (thread-yield!)"
        "        (cond"
        "         ((and timeout (thread-timeout?))"
        "          (if (and (pair? o) (pair? (cdr o)))"
        "              (cadr o)"
        "              (error \"timed out waiting for thread\" thread)))"
        "         (else (lp))))))))", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define (thread-terminate! thread)"
        "  (if (%thread-terminate! thread) (thread-yield!)))", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define (thread-sleep! timeout)"
        "  (%thread-sleep! timeout) (thread-yield!))", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define (mutex-lock! mutex . o)"
        "  (let ((timeout (and (pair? o) (car o)))"
        "        (thread (if (and (pair? o) (pair? (cdr o))) (cadr o) #t)))"
        "    (cond"
        "     ((%mutex-lock! mutex timeout thread))"
        "     (else"
        "      (thread-yield!)"
        "      (if (thread-timeout?) #f"
        "          (mutex-lock! mutex timeout thread))))))", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define (mutex-unlock! mutex . o)"
        "  (let ((condvar (and (pair? o) (car o)))"
        "        (timeout (if (and (pair? o) (pair? (cdr o))) (cadr o) #f)))"
        "    (cond"
        "     ((%mutex-unlock! mutex condvar timeout))"
        "     (else (thread-yield!) (not (thread-timeout?))))))", -1, self->env);

    /* Eval underscore aliases for SRFI-18 threads */
    sexp_eval_string(self->ctx,
        "(begin"
        " (define make_thread make-thread)"
        " (define thread_start thread-start!)"
        " (define thread_yield thread-yield!)"
        " (define thread_join thread-join!)"
        " (define thread_sleep thread-sleep!)"
        " (define thread_terminate thread-terminate!)"
        " (define current_thread current-thread)"
        " (define make_mutex make-mutex)"
        " (define mutex_lock mutex-lock!)"
        " (define mutex_unlock mutex-unlock!)"
        " (define make_condvar make-condition-variable)"
        " (define condvar_signal condition-variable-signal!)"
        " (define condvar_broadcast condition-variable-broadcast!))",
        -1, self->env);

    /* Eval underscore aliases for other new libraries */
    sexp_eval_string(self->ctx,
        "(begin"
        " (define random_integer random-integer)"
        " (define random_real random-real)"
        " (define json_read json-read)"
        " (define json_write json-write)"
        " (define get_env get-environment-variable)"
        " (define current_clock_second current-clock-second)"
        " (define bit_and bit-and)"
        " (define bit_ior bit-ior)"
        " (define bit_xor bit-xor)"
        " (define bit_count bit-count)"
        " (define arithmetic_shift arithmetic-shift)"
        " (define integer_length integer-length)"
        " (define object_cmp object-cmp)"
        " (define heap_stats heap-stats)"
        /* srfi/69 hash table aliases */
        " (define make_hash_table make-hash-table)"
        " (define hash_table_ref hash-table-ref)"
        " (define hash_table_set hash-table-set!)"
        " (define hash_table_delete hash-table-delete!)"
        " (define hash_table_exists hash-table-exists?)"
        " (define hash_table_keys hash-table-keys)"
        " (define hash_table_values hash-table-values)"
        " (define hash_table_size hash-table-size)"
        " (define hash_table_to_alist hash-table->alist)"
        /* srfi/1 list library aliases */
        " (define take_while take-while)"
        " (define drop_while drop-while)"
        " (define list_index list-index)"
        " (define delete_duplicates delete-duplicates)"
        " (define alist_cons alist-cons)"
        " (define alist_copy alist-copy)"
        " (define alist_delete alist-delete)"
        " (define append_map append-map)"
        " (define filter_map filter-map)"
        " (define fold_right fold-right)"
        " (define take_right take-right)"
        " (define drop_right drop-right)"
        " (define split_at split-at)"
        " (define last_pair last-pair)"
        " (define circular_list circular-list)"
        " (define is_proper_list proper-list?)"
        " (define is_dotted_list dotted-list?)"
        /* math/prime aliases */
        " (define is_prime prime?)"
        " (define is_probable_prime probable-prime?)"
        " (define random_prime random-prime)"
        " (define nth_prime nth-prime)"
        " (define prime_above prime-above)"
        " (define prime_below prime-below))",
        -1, self->env);

    /* Test framework underscore aliases for Eval syntax */
    sexp_eval_string(self->ctx,
        "(define test_begin test-begin)", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define test_end test-end)", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define test_assert test-assert)", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define test_error test-error)", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define test_group test-group)", -1, self->env);

    /* Dict runtime: __make_eval_dict__ creates a closure wrapping a hash table */
    sexp_eval_string(self->ctx,
        "(define (__make_eval_dict__ pairs)"
        "  (let ((ht (make-hash-table)))"
        "    (for-each (lambda (p) (hash-table-set! ht (car p) (cdr p))) pairs)"
        "    (lambda (__msg__)"
        "      (cond"
        "        ((eq? __msg__ 'get)"
        "         (lambda (k) (hash-table-ref/default ht"
        "           (if (string? k) (string->symbol k) k) #f)))"
        "        ((eq? __msg__ 'set)"
        "         (lambda (k v) (hash-table-set! ht"
        "           (if (string? k) (string->symbol k) k) v)))"
        "        ((eq? __msg__ 'delete)"
        "         (lambda (k) (hash-table-delete! ht"
        "           (if (string? k) (string->symbol k) k))))"
        "        ((eq? __msg__ 'keys)"
        "         (lambda () (hash-table-keys ht)))"
        "        ((eq? __msg__ 'values)"
        "         (lambda () (hash-table-values ht)))"
        "        ((eq? __msg__ 'has?)"
        "         (lambda (k) (hash-table-exists? ht"
        "           (if (string? k) (string->symbol k) k))))"
        "        ((eq? __msg__ 'size)"
        "         (lambda () (hash-table-size ht)))"
        "        ((eq? __msg__ 'to_list)"
        "         (lambda () (hash-table->alist ht)))"
        "        ((eq? __msg__ '__type__) '__dict__)"
        "        ((hash-table-exists? ht __msg__)"
        "         (hash-table-ref ht __msg__))"
        "        (else #f)))))", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define (dict? v)"
        "  (and (procedure? v)"
        "       (protect (e (else #f))"
        "         (eq? (v '__type__) '__dict__))))", -1, self->env);

    /* Async/await runtime: promise type backed by mutex+condvar */
    sexp_eval_string(self->ctx,
        "(define (__make-promise__)"
        "  (let ((m (make-mutex)) (cv (make-condition-variable))"
        "        (resolved #f) (value #f) (err #f))"
        "    (lambda (__msg__)"
        "      (cond"
        "        ((eq? __msg__ '__resolve__)"
        "         (lambda (v)"
        "           (mutex-lock! m)"
        "           (set! resolved #t) (set! value v)"
        "           (condition-variable-broadcast! cv)"
        "           (mutex-unlock! m)))"
        "        ((eq? __msg__ '__reject__)"
        "         (lambda (e)"
        "           (mutex-lock! m)"
        "           (set! resolved #t) (set! err e)"
        "           (condition-variable-broadcast! cv)"
        "           (mutex-unlock! m)))"
        "        ((eq? __msg__ '__await__)"
        "         (mutex-lock! m)"
        "         (let loop ()"
        "           (if resolved"
        "               (begin (mutex-unlock! m)"
        "                      (if err (raise err) value))"
        "               (begin (mutex-unlock! m cv)"
        "                      (mutex-lock! m)"
        "                      (loop)))))"
        "        ((eq? __msg__ 'ready?)"
        "         (mutex-lock! m)"
        "         (let ((r resolved)) (mutex-unlock! m) r))"
        "        ((eq? __msg__ '__type__) '__promise__)"
        "        (else (error \"promise: unknown message\" __msg__))))))", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define (__promise-resolve!__ p val)"
        "  (protect (e (else ((p '__reject__) e)))"
        "    ((p '__resolve__) val)))", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define (promise? v)"
        "  (and (procedure? v)"
        "       (protect (e (else #f))"
        "         (eq? (v '__type__) '__promise__))))", -1, self->env);
    sexp_eval_string(self->ctx,
        "(define (__await__ x)"
        "  (cond"
        "    ((and (procedure? x)"
        "          (protect (e (else #f)) (eq? (x '__type__) '__promise__)))"
        "     (x '__await__))"
        "    (else (future-result x))))", -1, self->env);

    /* OO wrapper: Channel-wrap — wraps raw channel cpointer */
    sexp_eval_string(self->ctx,
        "(define (Channel-wrap raw)"
        "  (lambda (__msg__)"
        "    (cond"
        "      ((eq? __msg__ 'send)"
        "       (lambda (val) (channel-send raw val)))"
        "      ((eq? __msg__ 'recv)"
        "       (lambda () (channel-recv raw)))"
        "      ((eq? __msg__ 'try_recv)"
        "       (lambda () (channel-try-recv raw)))"
        "      ((eq? __msg__ 'close)"
        "       (lambda () (channel-close raw)))"
        "      ((eq? __msg__ '__type__) '__channel__)"
        "      ((eq? __msg__ '__raw__) raw)"
        "      (else (error \"Channel: unknown message\" __msg__)))))",
        -1, self->env);

    /* OO wrapper: Future-wrap — wraps raw cpointer future */
    sexp_eval_string(self->ctx,
        "(define (Future-wrap raw)"
        "  (lambda (__msg__)"
        "    (cond"
        "      ((eq? __msg__ 'result)"
        "       (lambda () (future-result raw)))"
        "      ((eq? __msg__ 'ready?)"
        "       (future-ready? raw))"
        "      ((eq? __msg__ '__await__)"
        "       (future-result raw))"
        "      ((eq? __msg__ '__type__) '__future__)"
        "      ((eq? __msg__ '__raw__) raw)"
        "      (else (error \"Future: unknown message\" __msg__)))))",
        -1, self->env);

    /* OO wrapper: Pool(n) — wraps raw pool with -> access and RAII close */
    sexp_eval_string(self->ctx,
        "(define (Pool n)"
        "  (let ((raw (make-pool n)) (alive #t))"
        "    (lambda (__msg__)"
        "      (cond"
        "        ((eq? __msg__ 'submit)"
        "         (lambda (code) (Future-wrap (pool-submit raw code))))"
        "        ((eq? __msg__ 'apply)"
        "         (lambda (fn args) (Future-wrap (pool-apply raw fn args))))"
        "        ((eq? __msg__ 'channel)"
        "         (lambda (name) (Channel-wrap (pool-channel raw name))))"
        "        ((eq? __msg__ 'shutdown)"
        "         (lambda () (if alive (begin (set! alive #f)"
        "           (pool-shutdown raw)))))"
        "        ((eq? __msg__ 'close)"
        "         (lambda () (if alive (begin (set! alive #f)"
        "           (pool-shutdown raw)))))"
        "        ((eq? __msg__ '__type__) '__pool__)"
        "        ((eq? __msg__ '__raw__) raw)"
        "        (else (error \"Pool: unknown message\" __msg__))))))",
        -1, self->env);

    /* Update __await__ to handle OO Future objects too */
    sexp_eval_string(self->ctx,
        "(let ((orig-await __await__))"
        "  (set! __await__"
        "    (lambda (x)"
        "      (cond"
        "        ((and (procedure? x)"
        "              (protect (e (else #f)) (eq? (x '__type__) '__promise__)))"
        "         (x '__await__))"
        "        ((and (procedure? x)"
        "              (protect (e (else #f)) (eq? (x '__type__) '__future__)))"
        "         (x '__await__))"
        "        (else (future-result x))))))",
        -1, self->env);

    /* Reactive runtime: Signal, Computed, Effect, batch, dispose */
    {
        extern void eval_reactive_runtime(sexp ctx, sexp env);
        eval_reactive_runtime(self->ctx, self->env);
    }

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
