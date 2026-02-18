/*  _eval_lib_init.c -- central init for all statically-compiled chibi libs  */

#include <chibi/eval.h>

#ifdef _WIN32
#include <winsock2.h>
/* Ensure Winsock is initialized once per process.
 * Required before any getaddrinfo/socket/connect/etc. calls. */
static void eval_ensure_winsock(void) {
    static int initialized = 0;
    if (!initialized) {
        WSADATA wsa;
        WSAStartup(MAKEWORD(2, 2), &wsa);
        initialized = 1;
    }
}
#endif

/* Forward declarations for all renamed init functions */
extern sexp sexp_init_scheme_time(sexp ctx, sexp self, sexp_sint_t n, sexp env);
extern sexp sexp_init_chibi_json(sexp ctx, sexp self, sexp_sint_t n, sexp env);
extern sexp sexp_init_chibi_ast(sexp ctx, sexp self, sexp_sint_t n, sexp env);
extern sexp sexp_init_chibi_weak(sexp ctx, sexp self, sexp_sint_t n, sexp env);
extern sexp sexp_init_chibi_heap_stats(sexp ctx, sexp self, sexp_sint_t n, sexp env);
extern sexp sexp_init_chibi_disasm(sexp ctx, sexp self, sexp_sint_t n, sexp env);
extern sexp sexp_init_chibi_optimize_profile(sexp ctx, sexp self, sexp_sint_t n, sexp env);
extern sexp sexp_init_chibi_optimize_rest(sexp ctx, sexp self, sexp_sint_t n, sexp env);
extern sexp sexp_init_srfi18_threads(sexp ctx, sexp self, sexp_sint_t n, sexp env);
extern sexp sexp_init_srfi27_rand(sexp ctx, sexp self, sexp_sint_t n, sexp env);
extern sexp sexp_init_srfi39_param(sexp ctx, sexp self, sexp_sint_t n, sexp env);
extern sexp sexp_init_srfi69_hash(sexp ctx, sexp self, sexp_sint_t n, sexp env);
extern sexp sexp_init_srfi95_qsort(sexp ctx, sexp self, sexp_sint_t n, sexp env);
extern sexp sexp_init_srfi98_env(sexp ctx, sexp self, sexp_sint_t n, sexp env);
extern sexp sexp_init_srfi151_bit(sexp ctx, sexp self, sexp_sint_t n, sexp env);

#ifdef EVAL_HAVE_STUB_LIBS
/* Stub-generated library init functions (from _eval_stub_libs.c) */
extern sexp sexp_init_chibi_filesystem(sexp ctx, sexp self, sexp_sint_t n, sexp env,
                                        const char *version, const sexp_abi_identifier_t abi);
extern sexp sexp_init_chibi_io(sexp ctx, sexp self, sexp_sint_t n, sexp env,
                                const char *version, const sexp_abi_identifier_t abi);
extern sexp sexp_init_chibi_net(sexp ctx, sexp self, sexp_sint_t n, sexp env,
                                 const char *version, const sexp_abi_identifier_t abi);
extern sexp sexp_init_chibi_time(sexp ctx, sexp self, sexp_sint_t n, sexp env,
                                  const char *version, const sexp_abi_identifier_t abi);
extern sexp sexp_init_chibi_crypto(sexp ctx, sexp self, sexp_sint_t n, sexp env,
                                    const char *version, const sexp_abi_identifier_t abi);
extern sexp sexp_init_scheme_bytevector(sexp ctx, sexp self, sexp_sint_t n, sexp env,
                                         const char *version, const sexp_abi_identifier_t abi);
extern sexp sexp_init_srfi144_math(sexp ctx, sexp self, sexp_sint_t n, sexp env,
                                    const char *version, const sexp_abi_identifier_t abi);
extern sexp sexp_init_srfi160_uvprims(sexp ctx, sexp self, sexp_sint_t n, sexp env,
                                       const char *version, const sexp_abi_identifier_t abi);
#ifdef _WIN32
extern sexp sexp_init_chibi_win32_process(sexp ctx, sexp self, sexp_sint_t n, sexp env,
                                           const char *version, const sexp_abi_identifier_t abi);
#endif
#endif /* EVAL_HAVE_STUB_LIBS */

/* Register a simple record type with named fields, predicate,
 * getters, optional setters, and a zero-arg constructor. */
static void register_record_type(sexp ctx, sexp env,
                                  const char *type_name,
                                  const char *pred_name,
                                  const char *ctor_name,
                                  const char **field_names,
                                  const char **getter_names,
                                  const char **setter_names,
                                  int nfields) {
    sexp_gc_var5(name, slots, type, proc, sym);
    sexp_gc_preserve5(ctx, name, slots, type, proc, sym);

    /* Build slots list of symbols */
    slots = SEXP_NULL;
    for (int i = nfields - 1; i >= 0; i--) {
        sym = sexp_intern(ctx, field_names[i], -1);
        slots = sexp_cons(ctx, sym, slots);
    }

    /* Register type */
    name = sexp_c_string(ctx, type_name, -1);
    type = sexp_register_simple_type(ctx, name, SEXP_FALSE, slots);

    /* Bind type name in env */
    sexp_env_define(ctx, env, sexp_intern(ctx, type_name, -1), type);

    /* Predicate */
    name = sexp_c_string(ctx, pred_name, -1);
    proc = sexp_make_type_predicate(ctx, name, type);
    sexp_env_define(ctx, env, sexp_intern(ctx, pred_name, -1), proc);

    /* Getters and setters */
    for (int i = 0; i < nfields; i++) {
        sexp offset = sexp_make_fixnum(i);
        if (getter_names[i]) {
            name = sexp_c_string(ctx, getter_names[i], -1);
            proc = sexp_make_getter(ctx, name, type, offset);
            sexp_env_define(ctx, env, sexp_intern(ctx, getter_names[i], -1), proc);
        }
        if (setter_names && setter_names[i]) {
            name = sexp_c_string(ctx, setter_names[i], -1);
            proc = sexp_make_setter(ctx, name, type, offset);
            sexp_env_define(ctx, env, sexp_intern(ctx, setter_names[i], -1), proc);
        }
    }

    /* Constructor (zero-arg allocator) */
    name = sexp_c_string(ctx, ctor_name, -1);
    proc = sexp_make_constructor(ctx, name, type);
    sexp_env_define(ctx, env, sexp_intern(ctx, ctor_name, -1), proc);

    sexp_gc_release5(ctx);
}

void eval_init_all_libs(sexp ctx, sexp env) {
#ifdef _WIN32
    eval_ensure_winsock();
#endif

    /* SRFI-18 Mutex type (replaces srfi/18/types.scm Mutex definition).
     * sexp_make_constructor creates a zero-arg allocator; we bind it as
     * %%alloc-mutex and define the field-setting %make-mutex in Scheme. */
    {
        const char *fields[]  = {"name", "specific", "thread", "lock"};
        const char *getters[] = {"mutex-name", "mutex-specific", "%mutex-thread", "%mutex-lock"};
        const char *setters[] = {NULL, "mutex-specific-set!", "%mutex-thread-set!", "%mutex-lock-set!"};
        register_record_type(ctx, env, "Mutex", "mutex?", "%%alloc-mutex",
                             fields, getters, setters, 4);
    }
    sexp_eval_string(ctx,
        "(define (%make-mutex name specific thread lock)"
        "  (let ((m (%%alloc-mutex)))"
        "    (slot-set! Mutex m 0 name)"
        "    (slot-set! Mutex m 1 specific)"
        "    (slot-set! Mutex m 2 thread)"
        "    (slot-set! Mutex m 3 lock)"
        "    m))", -1, env);
    sexp_eval_string(ctx,
        "(define (make-mutex . o)"
        "  (%make-mutex (and (pair? o) (car o)) #f #f #f))", -1, env);

    /* SRFI-18 Condition-Variable type */
    {
        const char *fields[]  = {"name", "specific", "threads"};
        const char *getters[] = {"condition-variable-name", "condition-variable-specific",
                                 "%condition-variable-threads"};
        const char *setters[] = {NULL, "condition-variable-specific-set!",
                                 "%condition-variable-threads-set!"};
        register_record_type(ctx, env, "Condition-Variable", "condition-variable?",
                             "%%alloc-condvar", fields, getters, setters, 3);
    }
    sexp_eval_string(ctx,
        "(define (%make-condition-variable name specific threads)"
        "  (let ((cv (%%alloc-condvar)))"
        "    (slot-set! Condition-Variable cv 0 name)"
        "    (slot-set! Condition-Variable cv 1 specific)"
        "    (slot-set! Condition-Variable cv 2 threads)"
        "    cv))", -1, env);
    sexp_eval_string(ctx,
        "(define (make-condition-variable . o)"
        "  (%make-condition-variable (and (pair? o) (car o)) #f #f))", -1, env);

    sexp_init_srfi18_threads(ctx, SEXP_FALSE, 0, env);

    /* SRFI-9: define-record-type macro.  Must be loaded before any .scm
     * files that use it (srfi/69/type.scm, chibi/json.scm, etc.). */
    sexp_load_module_file(ctx, "srfi/9.scm", env);

    /* All others are order-independent */
    sexp_init_scheme_time(ctx, SEXP_FALSE, 0, env);
    sexp_init_chibi_json(ctx, SEXP_FALSE, 0, env);
    sexp_init_chibi_ast(ctx, SEXP_FALSE, 0, env);

    /* SRFI-18 interface: thread-yield!, thread-join!, thread-sleep!,
     * mutex-lock!, mutex-unlock!, current-time, etc.
     * Depends on: srfi18_threads (%thread-join!, %mutex-lock!, yield!),
     *             scheme_time (get-time-of-day, make-timeval, timeval-seconds),
     *             chibi_ast (%thread-interrupt!). */
    sexp_load_module_file(ctx, "srfi/18/interface.scm", env);

    sexp_init_chibi_weak(ctx, SEXP_FALSE, 0, env);
    sexp_init_chibi_heap_stats(ctx, SEXP_FALSE, 0, env);
    sexp_init_chibi_disasm(ctx, SEXP_FALSE, 0, env);
    sexp_init_chibi_optimize_profile(ctx, SEXP_FALSE, 0, env);
    sexp_init_chibi_optimize_rest(ctx, SEXP_FALSE, 0, env);
    sexp_init_srfi151_bit(ctx, SEXP_FALSE, 0, env);
    sexp_init_srfi27_rand(ctx, SEXP_FALSE, 0, env);
    sexp_init_srfi39_param(ctx, SEXP_FALSE, 0, env);
    sexp_init_srfi69_hash(ctx, SEXP_FALSE, 0, env);
    sexp_init_srfi95_qsort(ctx, SEXP_FALSE, 0, env);
    sexp_init_srfi98_env(ctx, SEXP_FALSE, 0, env);

    /* ================================================================
     * Layer 1: Scheme wrappers for C library primitives.
     * Order matters: srfi/69/type before interface, etc.
     * Must load misc-macros first for when/unless/guard used everywhere.
     * ================================================================ */
    /* when/unless/guard macros from R7RS.
     * Inlined from scheme/misc-macros.scm to avoid let-syntax/splicing warnings
     * from the let-syntax/letrec-syntax redefinitions in that file.
     * Each define-syntax must be a separate sexp_eval_string call. */
    sexp_eval_string(ctx,
        "(define-syntax when"
        "  (syntax-rules ()"
        "    ((when test . body) (if test (begin . body)))))",
        -1, env);
    sexp_eval_string(ctx,
        "(define-syntax unless"
        "  (syntax-rules ()"
        "    ((unless test . body) (when (not test) . body))))",
        -1, env);
    sexp_eval_string(ctx,
        "(define-syntax guard-aux"
        "  (syntax-rules (else =>)"
        "    ((guard-aux reraise (else result1 result2 ...))"
        "     (begin result1 result2 ...))"
        "    ((guard-aux reraise (test => result))"
        "     (let ((temp test))"
        "       (if temp (result temp) reraise)))"
        "    ((guard-aux reraise (test => result) clause1 clause2 ...)"
        "     (let ((temp test))"
        "       (if temp (result temp) (guard-aux reraise clause1 clause2 ...))))"
        "    ((guard-aux reraise (test))"
        "     (or test reraise))"
        "    ((guard-aux reraise (test) clause1 clause2 ...)"
        "     (or test (guard-aux reraise clause1 clause2 ...)))"
        "    ((guard-aux reraise (test result1 result2 ...))"
        "     (if test (begin result1 result2 ...) reraise))"
        "    ((guard-aux reraise (test result1 result2 ...) clause1 clause2 ...)"
        "     (if test"
        "         (begin result1 result2 ...)"
        "         (guard-aux reraise clause1 clause2 ...)))))",
        -1, env);
    sexp_eval_string(ctx,
        "(define-syntax guard"
        "  (syntax-rules ()"
        "    ((guard (var clause ...) e1 e2 ...)"
        "     ((call-with-current-continuation"
        "       (lambda (guard-k)"
        "         (with-exception-handler"
        "          (lambda (condition)"
        "            ((call-with-current-continuation"
        "              (lambda (handler-k)"
        "                (guard-k"
        "                 (lambda ()"
        "                   (let ((var condition))"
        "                     (guard-aux (handler-k (lambda ()"
        "                                             (raise-continuable condition)))"
        "                                clause ...))))))))"
        "          (lambda ()"
        "            (let ((res (let () e1 e2 ...)))"
        "              (guard-k (lambda () res)))))))))))",
        -1, env);
    sexp_load_module_file(ctx, "srfi/27/constructors.scm", env);
    sexp_load_module_file(ctx, "srfi/69/type.scm", env);
    sexp_load_module_file(ctx, "srfi/69/interface.scm", env);
    sexp_load_module_file(ctx, "srfi/95/sort.scm", env);
    sexp_load_module_file(ctx, "srfi/151/bitwise.scm", env);
    sexp_load_module_file(ctx, "chibi/ast.scm", env);
    sexp_load_module_file(ctx, "chibi/json.scm", env);
    sexp_load_module_file(ctx, "srfi/39/syntax.scm", env);

    /* ================================================================
     * Layer 2: R7RS scheme/ base extensions.
     * ================================================================ */
    sexp_load_module_file(ctx, "scheme/cxr.scm", env);       /* caaar..cddddr */
    sexp_load_module_file(ctx, "scheme/inexact.scm", env);    /* nan?, finite?, infinite? */

    /* scheme/division needs copy-exactness2 from cond-expand in .sld */
    sexp_eval_string(ctx,
        "(define-syntax copy-exactness2"
        "  (syntax-rules ()"
        "    ((copy-exactness2 src1 src2 expr) expr)))", -1, env);
    sexp_load_module_file(ctx, "scheme/division.scm", env);

    /* ================================================================
     * Layer 3: Inline SRFIs (small macro definitions from .sld files).
     * ================================================================ */

    /* SRFI-2: and-let* */
    sexp_eval_string(ctx,
        "(define-syntax and-let*"
        "  (syntax-rules ()"
        "    ((and-let* ()) #t)"
        "    ((and-let* () . body) (let () . body))"
        "    ((and-let* ((var expr))) expr)"
        "    ((and-let* ((expr))) expr)"
        "    ((and-let* (expr)) expr)"
        "    ((and-let* ((var expr) . rest) . body)"
        "     (let ((var expr)) (and var (and-let* rest . body))))"
        "    ((and-let* ((expr) . rest) . body)"
        "     (and expr (and-let* rest . body)))"
        "    ((and-let* (expr . rest) . body)"
        "     (let ((tmp expr)) (and tmp (and-let* rest . body))))))",
        -1, env);

    /* SRFI-8: receive */
    sexp_eval_string(ctx,
        "(define-syntax receive"
        "  (syntax-rules ()"
        "    ((receive params expr . body)"
        "     (call-with-values (lambda () expr) (lambda params . body)))))",
        -1, env);

    /* SRFI-11: let-values, let*-values */
    sexp_eval_string(ctx,
        "(define-syntax let*-values"
        "  (syntax-rules ()"
        "    ((let*-values () . body) (let () . body))"
        "    ((let*-values (((a) expr) . rest) . body)"
        "     (let ((a expr)) (let*-values rest . body)))"
        "    ((let*-values ((params expr) . rest) . body)"
        "     (call-with-values (lambda () expr)"
        "       (lambda params (let*-values rest . body))))))",
        -1, env);
    sexp_eval_string(ctx,
        "(define-syntax let-values"
        "  (syntax-rules ()"
        "    ((let-values () . body) (let () . body))"
        "    ((let-values (\"step\") (binds ...) bind expr maps () () . body)"
        "     (let*-values (binds ... (bind expr)) (let maps . body)))"
        "    ((let-values (\"step\") (binds ...) bind old-expr maps () ((params expr) . rest) . body)"
        "     (let-values (\"step\") (binds ... (bind old-expr)) () expr maps params rest . body))"
        "    ((let-values (\"step\") binds (bind ...) expr (maps ...) (x . y) rest . body)"
        "     (let-values (\"step\") binds (bind ... tmp) expr (maps ... (x tmp)) y rest . body))"
        "    ((let-values (\"step\") binds (bind ...) expr (maps ...) x rest . body)"
        "     (let-values (\"step\") binds (bind ... . tmp) expr (maps ... (x tmp)) () rest . body))"
        "    ((let-values ((params expr) . rest) . body)"
        "     (let-values (\"step\") () () expr () params rest . body))))",
        -1, env);

    /* SRFI-16: case-lambda */
    sexp_eval_string(ctx,
        "(define-syntax %case"
        "  (syntax-rules ()"
        "    ((%case args len n p ((params ...) . body) . rest)"
        "     (if (= len (length '(params ...)))"
        "         (apply (lambda (params ...) . body) args)"
        "         (%case args len 0 () . rest)))"
        "    ((%case args len n (p ...) ((x . y) . body) . rest)"
        "     (%case args len (+ n 1) (p ... x) (y . body) . rest))"
        "    ((%case args len n (p ...) (y . body) . rest)"
        "     (if (>= len n)"
        "         (apply (lambda (p ... . y) . body) args)"
        "         (%case args len 0 () . rest)))"
        "    ((%case args len n p)"
        "     (error \"case-lambda: no cases matched\"))))",
        -1, env);
    sexp_eval_string(ctx,
        "(define-syntax case-lambda"
        "  (syntax-rules ()"
        "    ((case-lambda . clauses)"
        "     (lambda args (let ((len (length* args)))"
        "       (%case args len 0 () . clauses))))))",
        -1, env);

    /* SRFI-26: cut, cute */
    sexp_eval_string(ctx,
        "(define-syntax %cut"
        "  (syntax-rules (<> <...>)"
        "    ((%cut e? params args) (lambda params args))"
        "    ((%cut e? (params ...) (args ...) <> . rest)"
        "     (%cut e? (params ... tmp) (args ... tmp) . rest))"
        "    ((%cut e? (params ...) (args ...) <...>)"
        "     (%cut e? (params ... . tmp) (apply args ... tmp)))"
        "    ((%cut e? (params ...) (args ...) <...> . rest)"
        "     (error \"cut: non-terminal <...>\"))"
        "    ((%cut #t (params ...) (args ...) x . rest)"
        "     (let ((tmp x)) (%cut #t (params ...) (args ... tmp) . rest)))"
        "    ((%cut #f (params ...) (args ...) x . rest)"
        "     (%cut #f (params ...) (args ... x) . rest))))",
        -1, env);
    sexp_eval_string(ctx,
        "(define-syntax cut"
        "  (syntax-rules () ((cut args ...) (%cut #f () () args ...))))",
        -1, env);
    sexp_eval_string(ctx,
        "(define-syntax cute"
        "  (syntax-rules () ((cute args ...) (%cut #t () () args ...))))",
        -1, env);

    /* ================================================================
     * Layer 4: SRFI-1 list library (9 files, order matters).
     * predicates → selectors → misc → constructors → fold →
     * search → deletion → alists → lset
     * ================================================================ */
    sexp_load_module_file(ctx, "srfi/1/predicates.scm", env);
    sexp_load_module_file(ctx, "srfi/1/selectors.scm", env);
    sexp_load_module_file(ctx, "srfi/1/misc.scm", env);
    sexp_load_module_file(ctx, "srfi/1/constructors.scm", env);
    sexp_load_module_file(ctx, "srfi/1/fold.scm", env);
    sexp_load_module_file(ctx, "srfi/1/search.scm", env);
    sexp_load_module_file(ctx, "srfi/1/deletion.scm", env);
    sexp_load_module_file(ctx, "srfi/1/alists.scm", env);
    sexp_load_module_file(ctx, "srfi/1/lset.scm", env);

    /* ================================================================
     * Layer 5: Utility libraries (no complex deps).
     * ================================================================ */
    sexp_load_module_file(ctx, "chibi/optional.scm", env);    /* let-optionals, let-keywords */
    sexp_load_module_file(ctx, "chibi/generic.scm", env);     /* define-generic, define-method */
    sexp_load_module_file(ctx, "chibi/equiv.scm", env);       /* equiv? (needs srfi/69) */
    sexp_load_module_file(ctx, "chibi/bytevector.scm", env);  /* u16/u32 ref, integer<->bv */

    /* ================================================================
     * Layer 6+7: Pattern matching, iteration, AST optimizer.
     * Disabled: match.scm (~1.6s) and loop.scm (~1.3s) are expensive
     * macro-heavy files only needed by the optimizer.  The optimizer's
     * rest.scm has bugs (replace-param returns #f for lambda/app nodes,
     * segfaults with supplied rest args).  Disabling the whole stack
     * saves ~4.5s per context init with no functional impact.
     * ================================================================ */
    /* sexp_load_module_file(ctx, "chibi/match/match.scm", env); */
    /* sexp_load_module_file(ctx, "chibi/loop/loop.scm", env); */
    /* sexp_load_module_file(ctx, "chibi/optimize.scm", env); */
    /* sexp_load_module_file(ctx, "chibi/optimize/profile.scm", env); */
    /* sexp_load_module_file(ctx, "chibi/optimize/rest.scm", env); */

    /* ================================================================
     * Layer 8: Math utilities (depends on srfi/151, srfi/27, srfi/1).
     * ================================================================ */
    sexp_load_module_file(ctx, "chibi/math/prime.scm", env);

    /* ================================================================
     * Stub-generated libraries (chibi-ffi compiled).
     * Only available after `make ffi` generates the .c files.
     * ================================================================ */
#ifdef EVAL_HAVE_STUB_LIBS
    sexp_init_chibi_filesystem(ctx, SEXP_FALSE, 0, env, "", SEXP_ABI_IDENTIFIER);
    sexp_init_chibi_io(ctx, SEXP_FALSE, 0, env, "", SEXP_ABI_IDENTIFIER);
    sexp_init_chibi_net(ctx, SEXP_FALSE, 0, env, "", SEXP_ABI_IDENTIFIER);
    sexp_init_chibi_time(ctx, SEXP_FALSE, 0, env, "", SEXP_ABI_IDENTIFIER);
    sexp_init_chibi_crypto(ctx, SEXP_FALSE, 0, env, "", SEXP_ABI_IDENTIFIER);
    sexp_init_scheme_bytevector(ctx, SEXP_FALSE, 0, env, "", SEXP_ABI_IDENTIFIER);
    sexp_init_srfi144_math(ctx, SEXP_FALSE, 0, env, "", SEXP_ABI_IDENTIFIER);
    sexp_init_srfi160_uvprims(ctx, SEXP_FALSE, 0, env, "", SEXP_ABI_IDENTIFIER);
#ifdef _WIN32
    sexp_init_chibi_win32_process(ctx, SEXP_FALSE, 0, env, "", SEXP_ABI_IDENTIFIER);

    /* Windows shims for filesystem functions guarded by (not windows) in stub.
     * get-file-descriptor-status / set-file-descriptor-status! use our fcntl
     * compat from _chibi_win_compat.h (ioctlsocket for non-blocking). */
    sexp_eval_string(ctx,
        "(define open/non-block 4)"  /* O_NONBLOCK = 0x0004 */
        "(define (get-file-descriptor-status fd)"
        "  0)"  /* No meaningful flags on Windows */
        "(define (set-file-descriptor-status! fd flags)"
        "  #t)",  /* fcntl compat in accept.c handles non-blocking */
        -1, env);
#endif

    /* ================================================================
     * Scheme wrappers for stub-generated libraries.
     * Load order: iset base → char-set base → string → io → filesystem → net
     * ================================================================ */

    /* (chibi iset base): Integer-Set record type, iset-contains?, make-iset */
    sexp_load_module_file(ctx, "chibi/iset/base.scm", env);

    /* (chibi char-set base): Char-Set = Integer-Set, char-set?, char-set-contains?
     * Inlined from char-set/base.sld (we don't process .sld files). */
    sexp_eval_string(ctx,
        "(define Char-Set Integer-Set)"
        "(define char-set? iset?)"
        "(define (char-set-contains? cset ch)"
        "  (iset-contains? cset (char->integer ch)))"
        "(define-syntax immutable-char-set"
        "  (syntax-rules () ((immutable-char-set cs) cs)))",
        -1, env);

    /* (chibi string): cursor-oriented string library.
     * Load string.scm first (string-fold, string-find, etc.),
     * then inline string-for-each and string-map from string.sld. */
    sexp_load_module_file(ctx, "chibi/string.scm", env);
    sexp_eval_string(ctx,
        "(define (string-for-each proc str . los)"
        "  (if (null? los)"
        "      (string-fold (lambda (ch a) (proc ch)) #f str)"
        "      (let ((los (cons str los)))"
        "        (let lp ((is (map string-cursor-start los)))"
        "          (cond"
        "           ((any (lambda (str i)"
        "                   (string-cursor>=? i (string-cursor-end str)))"
        "                 los is))"
        "           (else"
        "            (apply proc (map string-cursor-ref los is))"
        "            (lp (map string-cursor-next los is))))))))",
        -1, env);
    sexp_eval_string(ctx,
        "(define (string-map proc str . los)"
        "  (call-with-output-string"
        "    (lambda (out)"
        "      (apply string-for-each"
        "             (lambda args (write-char (apply proc args) out))"
        "             str los))))",
        -1, env);

    /* (chibi io): string->utf8, utf8->string, read-line, write-string, etc. */
    sexp_load_module_file(ctx, "chibi/io/io.scm", env);

    /* (chibi filesystem): file-exists?, directory-files, etc. */
    sexp_load_module_file(ctx, "chibi/filesystem.scm", env);

    /* (chibi net): make-listener-socket, get-address-info, send, receive, etc. */
    sexp_load_module_file(ctx, "chibi/net.scm", env);

    /* (srfi 144): flonum operations */
    sexp_load_module_file(ctx, "srfi/144/flonum.scm", env);

    /* (srfi 160): typed uniform vector constructors/converters.
     * Load base.scm (typed wrappers) instead of uvector.scm (derived ops).
     * uvector.scm is designed for per-type module scoping and overwrites
     * R7RS vector-map/vector-copy/etc. in a flat environment. */
    sexp_load_module_file(ctx, "srfi/160/base.scm", env);

    /* ----------------------------------------------------------------
     * Eval-friendly underscore aliases for chibi/net + filesystem functions.
     * Eval's parser treats `-` as subtraction, so hyphenated names must
     * be accessed via backticks.  These aliases provide underscore forms.
     * ---------------------------------------------------------------- */
    {
        static const char *aliases[][2] = {
            /* net.scm high-level API */
            {"make_listener_socket",   "make-listener-socket"},
            {"get_address_info",       "get-address-info"},
            {"make_address_info",      "make-address-info"},
            {"open_net_io",            "open-net-io"},
            {"with_net_io",            "with-net-io"},
            {"send_non_blocking",      "send/non-blocking"},
            {"receive_non_blocking",   "receive/non-blocking"},
            /* "receive" is a keyword in Eval (SRFI-8), so alias as "recv" */
            {"recv",                   "receive"},
            /* net C stub: address-info accessors */
            {"address_info_family",    "address-info-family"},
            {"address_info_socket_type","address-info-socket-type"},
            {"address_info_protocol",  "address-info-protocol"},
            {"address_info_address",   "address-info-address"},
            {"address_info_address_length","address-info-address-length"},
            {"address_info_next",      "address-info-next"},
            {"address_info_flags",     "address-info-flags"},
            {"address_info_canonname", "address-info-canonname"},
            /* net C stub: socket constants */
            {"address_family_unspecified","address-family/unspecified"},
            {"address_family_inet",    "address-family/inet"},
            {"address_family_inet6",   "address-family/inet6"},
            {"address_family_unix",    "address-family/unix"},
            {"socket_type_stream",     "socket-type/stream"},
            {"socket_type_datagram",   "socket-type/datagram"},
            {"socket_type_raw",        "socket-type/raw"},
            {"ip_proto_ip",            "ip-proto/ip"},
            {"ip_proto_tcp",           "ip-proto/tcp"},
            {"ip_proto_udp",           "ip-proto/udp"},
            {"socket_opt_reuseaddr",   "socket-opt/reuseaddr"},
            /* net C stub: sockaddr */
            {"make_sockaddr",          "make-sockaddr"},
            {"sockaddr_name",          "sockaddr-name"},
            {"sockaddr_port",          "sockaddr-port"},
            /* net C stub: socket options */
            {"set_socket_option",      "set-socket-option!"},
            {"get_socket_option",      "get-socket-option"},
            {"get_peer_name",          "get-peer-name"},
            /* filesystem C stub */
            {"close_file_descriptor",  "close-file-descriptor"},
            {"open_input_file_descriptor","open-input-file-descriptor"},
            {"open_output_file_descriptor","open-output-file-descriptor"},
            /* filesystem.scm high-level */
            {"file_exists",            "file-exists?"},
            {"file_directory",         "file-directory?"},
            {"file_regular",           "file-regular?"},
            {"file_size",              "file-size"},
            {"directory_files",        "directory-files"},
            {"directory_fold",         "directory-fold"},
            {"create_directory_star",  "create-directory*"},
            {"delete_file",            "delete-file"},
            {"delete_file_hierarchy",  "delete-file-hierarchy"},
            {"with_directory",         "with-directory"},
            {"current_directory",      "current-directory"},
            {"change_directory",       "change-directory"},
            {"file_modification_time", "file-modification-time"},
            {"file_access_time",       "file-access-time"},
            {"read_link",              "read-link"},
            /* io.scm */
            {"string_to_utf8",         "string->utf8"},
            {"utf8_to_string",         "utf8->string"},
            {"read_line",              "read-line"},
            {"read_string",            "read-string"},
            {"write_string",           "write-string"},
            {"write_line",             "write-line"},
            {"port_to_string",         "port->string"},
            {"file_to_string",         "file->string"},
            {"file_to_bytevector",     "file->bytevector"},
            /* string.scm */
            {"string_null",            "string-null?"},
            {"string_any",             "string-any"},
            {"string_every",           "string-every"},
            {"string_find",            "string-find"},
            {"string_find_right",      "string-find-right"},
            {"string_split",           "string-split"},
            {"string_join",            "string-join"},
            {"string_trim",            "string-trim"},
            {"string_trim_left",       "string-trim-left"},
            {"string_trim_right",      "string-trim-right"},
            {"string_contains",        "string-contains"},
            {"string_count",           "string-count"},
            {"string_prefix",          "string-prefix?"},
            {"string_suffix",          "string-suffix?"},
            {"string_fold",            "string-fold"},
            {"string_for_each",        "string-for-each"},
            {"string_map",             "string-map"},
            {"string_downcase_ascii",  "string-downcase-ascii"},
            {"string_upcase_ascii",    "string-upcase-ascii"},
            {NULL, NULL}
        };
        for (int i = 0; aliases[i][0]; i++) {
            sexp sym = sexp_intern(ctx, aliases[i][1], -1);
            sexp val = sexp_env_ref(ctx, env, sym, SEXP_VOID);
            if (val != SEXP_VOID)
                sexp_env_define(ctx, env,
                    sexp_intern(ctx, aliases[i][0], -1), val);
        }
    }
#endif /* EVAL_HAVE_STUB_LIBS */

    /* ----------------------------------------------------------------
     * Eval-friendly underscore aliases for SRFI-18 (threads, mutexes,
     * condition variables, time).  Always available (not stub-dependent).
     * ---------------------------------------------------------------- */
    {
        static const char *aliases[][2] = {
            /* threads */
            {"make_thread",            "make-thread"},
            {"thread_start",           "thread-start!"},
            {"thread_join",            "thread-join!"},
            {"thread_yield",           "thread-yield!"},
            {"thread_sleep",           "thread-sleep!"},
            {"thread_terminate",       "thread-terminate!"},
            {"thread_name",            "thread-name"},
            {"current_thread",         "current-thread"},
            {"thread_specific",        "thread-specific"},
            {"thread_specific_set",    "thread-specific-set!"},
            /* mutexes */
            {"make_mutex",             "make-mutex"},
            {"mutex_lock",             "mutex-lock!"},
            {"mutex_unlock",           "mutex-unlock!"},
            {"mutex_name",             "mutex-name"},
            {"mutex_specific",         "mutex-specific"},
            {"mutex_specific_set",     "mutex-specific-set!"},
            /* condition variables */
            {"make_condvar",           "make-condition-variable"},
            {"condvar_signal",         "condition-variable-signal!"},
            {"condvar_broadcast",      "condition-variable-broadcast!"},
            /* time */
            {"current_time",           "current-time"},
            {"time_to_seconds",        "time->seconds"},
            {"seconds_to_time",        "seconds->time"},
            {NULL, NULL}
        };
        for (int i = 0; aliases[i][0]; i++) {
            sexp sym = sexp_intern(ctx, aliases[i][1], -1);
            sexp val = sexp_env_ref(ctx, env, sym, SEXP_VOID);
            if (val != SEXP_VOID)
                sexp_env_define(ctx, env,
                    sexp_intern(ctx, aliases[i][0], -1), val);
        }
    }
}
