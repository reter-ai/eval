/*  eval_main.c -- Standalone Eval CLI executable
 *
 *  Usage:
 *    eval [options] [file.eval]
 *
 *  Options:
 *    -e <expr>   Evaluate expression and print result
 *    -I <dir>    Add module search directory
 *    -V          Print version
 *    -h          Print usage
 *    (no file)   Read from stdin
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <chibi/eval.h>

#ifdef _WIN32
#include <io.h>
#define isatty _isatty
#define fileno _fileno
#else
#include <unistd.h>
#endif
#include "_eval_parser_helpers.h"
#include "_eval_writer.h"

/* From _eval_pool.c (non-static under EVAL_STANDALONE) */
extern void register_pyobject_type(sexp ctx);
extern void register_channel_type(sexp ctx);
extern void register_pool_type(sexp ctx);
extern void register_bridge_functions_c(sexp ctx, sexp env);
extern void register_pool_eval_functions(sexp ctx, sexp env);
extern void eval_standard_aliases(sexp ctx, sexp env);
extern void eval_oo_wrappers(sexp ctx, sexp env);

/* From _eval_lib_init.c */
extern void eval_init_all_libs(sexp ctx, sexp env);

/* Saved Scheme-native display/newline (from init-7.scm) before bridge override */
static sexp g_scheme_display = SEXP_FALSE;
static sexp g_scheme_newline = SEXP_FALSE;

/* ================================================================
 * File / stdin reading
 * ================================================================ */

static char *read_file(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    if (size < 0) { fclose(f); return NULL; }
    char *buf = (char *)malloc(size + 1);
    if (!buf) { fclose(f); return NULL; }
    size_t nread = fread(buf, 1, size, f);
    fclose(f);
    buf[nread] = '\0';
    return buf;
}

static char *read_stdin_all(void) {
    size_t cap = 4096, len = 0;
    char *buf = (char *)malloc(cap);
    if (!buf) return NULL;
    while (!feof(stdin)) {
        if (len + 1024 > cap) {
            cap *= 2;
            char *tmp = (char *)realloc(buf, cap);
            if (!tmp) { free(buf); return NULL; }
            buf = tmp;
        }
        size_t n = fread(buf + len, 1, 1024, stdin);
        len += n;
        if (n == 0) break;
    }
    buf[len] = '\0';
    return buf;
}

/* ================================================================
 * REPL support
 * ================================================================ */

typedef struct {
    char *data;
    size_t len;
    size_t cap;
} ReplBuf;

static void replbuf_init(ReplBuf *rb) {
    rb->cap = 1024;
    rb->data = (char *)malloc(rb->cap);
    rb->data[0] = '\0';
    rb->len = 0;
}

static void replbuf_append(ReplBuf *rb, const char *s) {
    size_t slen = strlen(s);
    if (rb->len + slen + 1 > rb->cap) {
        while (rb->len + slen + 1 > rb->cap)
            rb->cap *= 2;
        rb->data = (char *)realloc(rb->data, rb->cap);
    }
    memcpy(rb->data + rb->len, s, slen + 1);
    rb->len += slen;
}

static void replbuf_clear(ReplBuf *rb) {
    rb->data[0] = '\0';
    rb->len = 0;
}

static void replbuf_free(ReplBuf *rb) {
    free(rb->data);
    rb->data = NULL;
    rb->len = rb->cap = 0;
}

/* Returns 1 if brackets are unbalanced (need more input). */
static int repl_needs_more(const char *s) {
    int depth = 0;
    int in_block_comment = 0;
    for (const char *p = s; *p; p++) {
        if (in_block_comment) {
            if (p[0] == '*' && p[1] == '/') {
                in_block_comment = 0;
                p++;
            }
            continue;
        }
        if (p[0] == '/' && p[1] == '/') {
            /* line comment — skip to end of line */
            while (*p && *p != '\n') p++;
            if (!*p) break;
            continue;
        }
        if (p[0] == '/' && p[1] == '*') {
            in_block_comment = 1;
            p++;
            continue;
        }
        if (p[0] == '"') {
            p++;
            while (*p && *p != '"') {
                if (*p == '\\' && p[1]) p++;
                p++;
            }
            if (!*p) break;
            continue;
        }
        if (*p == '(' || *p == '[' || *p == '{') depth++;
        else if (*p == ')' || *p == ']' || *p == '}') depth--;
    }
    return (depth > 0) || in_block_comment;
}

static int ends_with_semicolon(const char *s, size_t len) {
    while (len > 0 && (s[len-1] == ' ' || s[len-1] == '\t' ||
                       s[len-1] == '\n' || s[len-1] == '\r'))
        len--;
    return len > 0 && s[len-1] == ';';
}

/* Forward declarations */
static int run_code(sexp ctx, const char *source, int print_result);
static int run_scheme_file(sexp ctx, const char *path);

static void run_repl(sexp ctx) {
    ReplBuf rb;
    replbuf_init(&rb);
    char line[4096];

    printf("eval 0.1.0 - interactive mode (type 'exit' or 'quit' to leave)\n");

    for (;;) {
        printf("%s", rb.len == 0 ? "> " : "... ");
        fflush(stdout);

        if (!fgets(line, sizeof(line), stdin))
            break;  /* EOF (Ctrl+D / Ctrl+Z) */

        /* On fresh input, check for exit/quit/blank */
        if (rb.len == 0) {
            /* Strip leading whitespace for command check */
            const char *trimmed = line;
            while (*trimmed == ' ' || *trimmed == '\t') trimmed++;

            /* Blank line */
            if (*trimmed == '\n' || *trimmed == '\r' || *trimmed == '\0')
                continue;

            /* exit / quit commands */
            if (strncmp(trimmed, "exit", 4) == 0 &&
                (trimmed[4] == '\n' || trimmed[4] == '\r' ||
                 trimmed[4] == '\0' || trimmed[4] == ';'))
                break;
            if (strncmp(trimmed, "quit", 4) == 0 &&
                (trimmed[4] == '\n' || trimmed[4] == '\r' ||
                 trimmed[4] == '\0' || trimmed[4] == ';'))
                break;
        }

        replbuf_append(&rb, line);

        if (repl_needs_more(rb.data))
            continue;

        /* Auto-append semicolon if needed */
        if (!ends_with_semicolon(rb.data, rb.len))
            replbuf_append(&rb, ";");

        run_code(ctx, rb.data, 1);
        replbuf_clear(&rb);
    }

    printf("\n");
    replbuf_free(&rb);
}

/* ================================================================
 * Context initialization
 * ================================================================ */

static sexp init_context(const char *extra_module_dir) {
    sexp_scheme_init();

    sexp ctx = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);
    sexp env = sexp_context_env(ctx);

    /* .scm files are loaded from embedded data via patched sexp_load_module_file.
     * Only add extra module dir if user specified -I. */
    if (extra_module_dir) {
        sexp_gc_var1(p);
        sexp_gc_preserve1(ctx, p);
        p = sexp_c_string(ctx, extra_module_dir, -1);
        sexp_add_module_directory(ctx, p, SEXP_TRUE);
        sexp_gc_release1(ctx);
    }

    sexp_load_standard_env(ctx, env, SEXP_SEVEN);
    sexp_load_standard_ports(ctx, env, stdin, stdout, stderr, 1);

    /* Save Scheme-native display/newline before bridge overrides them */
    {
        sexp_gc_var2(sym, cell);
        sexp_gc_preserve2(ctx, sym, cell);
        sym = sexp_intern(ctx, "display", -1);
        cell = sexp_env_cell(ctx, env, sym, 0);
        if (cell && sexp_pairp(cell)) {
            g_scheme_display = sexp_cdr(cell);
            sexp_preserve_object(ctx, g_scheme_display);
        }
        sym = sexp_intern(ctx, "newline", -1);
        cell = sexp_env_cell(ctx, env, sym, 0);
        if (cell && sexp_pairp(cell)) {
            g_scheme_newline = sexp_cdr(cell);
            sexp_preserve_object(ctx, g_scheme_newline);
        }
        sexp_gc_release2(ctx);
    }

    /* Register types (same order as Python context) */
    register_pyobject_type(ctx);
    register_channel_type(ctx);
    register_pool_type(ctx);
    {
        extern void register_grammar_type(sexp ctx);
        extern void register_parser_type(sexp ctx);
        register_grammar_type(ctx);
        register_parser_type(ctx);
    }
#ifdef EVAL_HAVE_CAPNP
    {
        extern void register_capnp_schema_type(sexp ctx);
        extern void register_capnp_reader_type(sexp ctx);
        register_capnp_schema_type(ctx);
        register_capnp_reader_type(ctx);
    }
#endif

    /* Register C bridge functions */
    register_bridge_functions_c(ctx, env);
    {
        extern void register_grammar_bridge_functions(sexp ctx, sexp env);
        register_grammar_bridge_functions(ctx, env);
    }
#ifdef EVAL_HAVE_CAPNP
    {
        extern void register_capnp_bridge_functions(sexp ctx, sexp env);
        register_capnp_bridge_functions(ctx, env);
    }
#endif

    /* Load scheme extras, test framework */
    sexp_load_module_file(ctx, "scheme/extras.scm", env);
    sexp_load_module_file(ctx, "scheme/test.scm", env);
    env = sexp_context_env(ctx);

    /* Statically compiled chibi libraries */
    eval_init_all_libs(ctx, env);
    env = sexp_context_env(ctx);

    /* Pool functions (make-pool, pool-submit, etc.) — must be before aliases
     * which define OO wrappers referencing pool-submit/pool-apply */
    register_pool_eval_functions(ctx, env);

    /* Standard Scheme aliases */
    eval_standard_aliases(ctx, env);

    /* OO wrappers for Pool/Channel/Future (need pool-submit arity 2) */
    eval_oo_wrappers(ctx, env);

    /* Reactive runtime: Signal, Computed, Effect, batch, dispose */
    {
        extern void eval_reactive_runtime(sexp ctx, sexp env);
        eval_reactive_runtime(ctx, env);
    }

    /* OO networking: TcpSocket, TcpClient, TcpServer, HttpClient */
    {
        extern void eval_load_eval_file(sexp ctx, sexp env, const char *path);
        eval_load_eval_file(ctx, sexp_context_env(ctx), "eval/net-oo.eval");
    }

    /* OO string methods: "hello"->upper(), etc. */
    sexp_load_module_file(ctx, "eval/string-oo.scm", sexp_context_env(ctx));

    /* OO list/vector methods: [1,2,3]->map(...), #[1,2]->length, etc. */
    sexp_load_module_file(ctx, "eval/collection-oo.scm", sexp_context_env(ctx));

    return ctx;
}

/* ================================================================
 * Parse and execute
 * ================================================================ */

static int run_code(sexp ctx, const char *source, int print_result) {
    sexp env = sexp_context_env(ctx);
    char *error_msg = NULL;
    int error_line = 0, error_col = 0;

    sexp_gc_var2(parsed, result);
    sexp_gc_preserve2(ctx, parsed, result);

    parsed = eval_parse(ctx, env, source, &error_msg, &error_line, &error_col);
    if (error_msg) {
        fprintf(stderr, "parse error at line %d, col %d: %s\n",
                error_line, error_col, error_msg);
        free(error_msg);
        sexp_gc_release2(ctx);
        return 1;
    }

    if (parsed == SEXP_VOID) {
        sexp_gc_release2(ctx);
        return 0;
    }

    result = sexp_eval(ctx, parsed, env);
    if (sexp_exceptionp(result)) {
        sexp msg = sexp_exception_message(result);
        if (sexp_stringp(msg))
            fprintf(stderr, "error: %s\n", sexp_string_data(msg));
        else
            fprintf(stderr, "error: unknown exception\n");
        sexp_gc_release2(ctx);
        return 1;
    }

    if (print_result && result != SEXP_VOID && result != SEXP_UNDEF) {
        char buf[4096];
        eval_write_to_buf(ctx, result, buf, sizeof(buf));
        printf("%s\n", buf);
    }

    sexp_gc_release2(ctx);
    return 0;
}

/* ================================================================
 * Scheme file execution (for .scm/.ss/.sld files)
 * ================================================================ */

static int has_scheme_extension(const char *path) {
    const char *dot = strrchr(path, '.');
    if (!dot) return 0;
    return (strcmp(dot, ".scm") == 0 ||
            strcmp(dot, ".ss") == 0 ||
            strcmp(dot, ".sld") == 0);
}

/* Detect if a file is Scheme by extension or shebang line */
static int is_scheme_file(const char *path) {
    if (has_scheme_extension(path)) return 1;
    /* Check for shebang or leading '(' / ';' */
    FILE *f = fopen(path, "rb");
    if (!f) return 0;
    int c1 = fgetc(f);
    int c2 = fgetc(f);
    fclose(f);
    if (c1 == '#' && c2 == '!') return 1;  /* shebang */
    if (c1 == '(' || c1 == ';') return 1;  /* s-expression or comment */
    return 0;
}

/* Restore Scheme-native display/newline so port redirection works */
static void restore_scheme_io(sexp ctx) {
    sexp env = sexp_context_env(ctx);
    sexp_gc_var2(sym, cell);
    sexp_gc_preserve2(ctx, sym, cell);
    if (g_scheme_display != SEXP_FALSE) {
        sym = sexp_intern(ctx, "display", -1);
        cell = sexp_env_cell(ctx, env, sym, 0);
        if (cell && sexp_pairp(cell))
            sexp_cdr(cell) = g_scheme_display;
    }
    if (g_scheme_newline != SEXP_FALSE) {
        sym = sexp_intern(ctx, "newline", -1);
        cell = sexp_env_cell(ctx, env, sym, 0);
        if (cell && sexp_pairp(cell))
            sexp_cdr(cell) = g_scheme_newline;
    }
    sexp_gc_release2(ctx);
}

static int run_scheme_file(sexp ctx, const char *path) {
    sexp env = sexp_context_env(ctx);
    sexp_gc_var3(filepath, result, tmp);
    sexp_gc_preserve3(ctx, filepath, result, tmp);

    restore_scheme_io(ctx);

    filepath = sexp_c_string(ctx, path, -1);
    result = sexp_load(ctx, filepath, env);

    if (sexp_exceptionp(result)) {
        sexp msg = sexp_exception_message(result);
        if (sexp_stringp(msg))
            fprintf(stderr, "error: %s\n", sexp_string_data(msg));
        else
            fprintf(stderr, "error: exception while loading '%s'\n", path);
        sexp_gc_release3(ctx);
        return 1;
    }

    sexp_gc_release3(ctx);
    return 0;
}

static sexp get_meta_env(sexp ctx) {
    sexp me = sexp_global(ctx, SEXP_G_META_ENV);
    return sexp_envp(me) ? me : sexp_context_env(ctx);
}

/* Set (command-line) parameter for Scheme scripts */
static void set_command_line(sexp ctx, int argc, char **argv, int file_idx) {
    sexp_gc_var3(args, sym, tmp);
    sexp_gc_preserve3(ctx, args, sym, tmp);
    args = SEXP_NULL;
    for (int j = argc - 1; j >= file_idx; j--) {
        tmp = sexp_c_string(ctx, argv[j], -1);
        args = sexp_cons(ctx, tmp, args);
    }
    if (args == SEXP_NULL) {
        tmp = sexp_c_string(ctx, "", -1);
        args = sexp_cons(ctx, tmp, args);
    }
    sym = sexp_intern(ctx, "command-line", -1);
    sexp_set_parameter(ctx, get_meta_env(ctx), sym, args);
    sexp_gc_release3(ctx);
}

/* ================================================================
 * CLI
 * ================================================================ */

static void print_usage(const char *prog) {
    printf("Usage: %s [options] [file.eval | file.scm] [args...]\n\n", prog);
    printf("Options:\n");
    printf("  -e <expr>   Evaluate expression and print result\n");
    printf("  -I <dir>    Add module search directory\n");
    printf("  -V          Print version\n");
    printf("  -h          Print this help\n\n");
    printf("Runs .eval files as Eval language, .scm files as Scheme.\n");
    printf("Files starting with #! are also treated as Scheme.\n");
    printf("If no file is given, starts an interactive REPL.\n");
    printf("Piped input is read and executed non-interactively.\n");
}

int main(int argc, char **argv) {
    const char *expr = NULL;
    const char *file = NULL;
    const char *extra_module_dir = NULL;
    int file_idx = 0;  /* argv index of the file argument */

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            print_usage(argv[0]);
            return 0;
        } else if (strcmp(argv[i], "-V") == 0 || strcmp(argv[i], "--version") == 0) {
            printf("eval 0.1.0 (chibi-scheme embedded)\n");
            return 0;
        } else if (strcmp(argv[i], "-e") == 0) {
            if (++i >= argc) {
                fprintf(stderr, "error: -e requires an argument\n");
                return 1;
            }
            expr = argv[i];
        } else if (strcmp(argv[i], "-I") == 0) {
            if (++i >= argc) {
                fprintf(stderr, "error: -I requires an argument\n");
                return 1;
            }
            extra_module_dir = argv[i];
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "error: unknown option '%s'\n", argv[i]);
            return 1;
        } else {
            file = argv[i];
            file_idx = i;
            break;  /* remaining args are for the script */
        }
    }

    sexp ctx = init_context(extra_module_dir);
    int rc = 0;

    if (expr) {
        /* Append semicolon if not present (Eval grammar requires it) */
        size_t elen = strlen(expr);
        if (elen > 0 && expr[elen - 1] != ';') {
            char *expr_with_semi = (char *)malloc(elen + 2);
            memcpy(expr_with_semi, expr, elen);
            expr_with_semi[elen] = ';';
            expr_with_semi[elen + 1] = '\0';
            rc = run_code(ctx, expr_with_semi, 1);
            free(expr_with_semi);
        } else {
            rc = run_code(ctx, expr, 1);
        }
    } else if (file) {
        if (is_scheme_file(file)) {
            /* Scheme file — load directly via chibi's sexp_load */
            set_command_line(ctx, argc, argv, file_idx);
            rc = run_scheme_file(ctx, file);
        } else {
            /* Eval file — parse infix syntax then evaluate */
            char *source = read_file(file);
            if (!source) {
                fprintf(stderr, "error: cannot open '%s'\n", file);
                sexp_destroy_context(ctx);
                return 1;
            }
            rc = run_code(ctx, source, 0);
            free(source);
        }
    } else {
        if (isatty(fileno(stdin))) {
            run_repl(ctx);
        } else {
            char *source = read_stdin_all();
            if (!source) {
                fprintf(stderr, "error: failed to read stdin\n");
                sexp_destroy_context(ctx);
                return 1;
            }
            rc = run_code(ctx, source, 0);
            free(source);
        }
    }

    sexp_destroy_context(ctx);
    return rc;
}
