/*  _eval_pool.c -- C-level thread pool with channels for Eval  */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <chibi/eval.h>
#include "_eval_thread.h"
#include "_eval_pool.h"
#include "_eval_parser_helpers.h"

#ifdef EVAL_STANDALONE
/* Standalone pyobject type stub — allocates a type tag with no-op finalizer */
sexp_tag_t pyobject_type_tag = 0;
static sexp standalone_pyobject_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    return SEXP_VOID;
}
void register_pyobject_type(sexp ctx) {
    sexp_gc_var2(name, type);
    sexp_gc_preserve2(ctx, name, type);
    name = sexp_c_string(ctx, "python-object", -1);
    type = sexp_register_c_type(ctx, name, standalone_pyobject_finalize);
    if (sexp_typep(type)) {
        pyobject_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }
    sexp_gc_release2(ctx);
}
int sexp_pyobjectp(sexp x) {
    return sexp_pointerp(x) && sexp_pointer_tag(x) == pyobject_type_tag;
}
/* Standalone bridge_eval_scheme — pure C, no Python */
sexp bridge_eval_scheme(sexp ctx, sexp self, sexp_sint_t n, sexp s) {
    if (!sexp_stringp(s))
        return sexp_type_exception(ctx, self, SEXP_STRING, s);
    return sexp_eval_string(ctx, sexp_string_data(s), sexp_string_size(s),
                            sexp_context_env(ctx));
}
#else
/* Forward declaration for pyobject_type registration (defined in _chibi_pyobject_type.c) */
extern void register_pyobject_type(sexp ctx);
#endif

/* Forward declarations for binary serialization (defined in _chibi_serialize.c) */
extern int ser_sexp_to_buf(sexp ctx, sexp env, sexp val,
                           unsigned char **out_data, size_t *out_len,
                           char *err_msg, size_t err_len);
extern sexp deser_sexp_from_buf(sexp ctx, sexp env,
                                const unsigned char *data, size_t len,
                                char *err_msg, size_t err_len);

/* Binary format magic for auto-detection */
#define SER_MAGIC_BYTES "CCHI"

/* ================================================================
 * Channel type registration (per-context, no Python dependency)
 * ================================================================ */

static sexp_tag_t channel_type_tag = 0;

static sexp channel_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    /* Channels are owned by the pool; don't free here */
    return SEXP_VOID;
}

void register_channel_type(sexp ctx) {
    sexp_gc_var2(name, type);
    sexp_gc_preserve2(ctx, name, type);

    name = sexp_c_string(ctx, "eval-channel", -1);
    type = sexp_register_c_type(ctx, name, channel_finalize);

    if (sexp_typep(type)) {
        channel_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }

    sexp_gc_release2(ctx);
}

static sexp wrap_channel(sexp ctx, EvalChannel *ch) {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), channel_type_tag);
    sexp_cpointer_value(result) = (void *)ch;
    sexp_cpointer_length(result) = 0;

    sexp_gc_release1(ctx);
    return result;
}

static EvalChannel *unwrap_channel(sexp x) {
    if (sexp_pointerp(x) && sexp_pointer_tag(x) == channel_type_tag)
        return (EvalChannel *)sexp_cpointer_value(x);
    return NULL;
}

static int sexp_channelp(sexp x) {
    return sexp_pointerp(x) && sexp_pointer_tag(x) == channel_type_tag;
}

/* ================================================================
 * Pool type registration (per-context, no Python dependency)
 * ================================================================ */

static sexp_tag_t pool_type_tag = 0;

static sexp pool_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    /* Pools are explicitly destroyed via pool_shutdown; don't free here */
    return SEXP_VOID;
}

void register_pool_type(sexp ctx) {
    sexp_gc_var2(name, type);
    sexp_gc_preserve2(ctx, name, type);

    name = sexp_c_string(ctx, "eval-pool", -1);
    type = sexp_register_c_type(ctx, name, pool_finalize);

    if (sexp_typep(type)) {
        pool_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }

    sexp_gc_release2(ctx);
}

static sexp wrap_pool(sexp ctx, EvalPool *pool) {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), pool_type_tag);
    sexp_cpointer_value(result) = (void *)pool;
    sexp_cpointer_length(result) = 0;

    sexp_gc_release1(ctx);
    return result;
}

static EvalPool *unwrap_pool(sexp x) {
    if (sexp_pointerp(x) && sexp_pointer_tag(x) == pool_type_tag)
        return (EvalPool *)sexp_cpointer_value(x);
    return NULL;
}

/* ================================================================
 * EvalChannel
 * ================================================================ */

typedef struct ChannelItem {
    char *data;
    size_t len;
    struct ChannelItem *next;
} ChannelItem;

struct EvalChannel {
    eval_mutex_t mutex;
    eval_cond_t cond;
    ChannelItem *head, *tail;
    int closed;
};

static EvalChannel *eval_channel_create(void) {
    EvalChannel *ch = (EvalChannel *)calloc(1, sizeof(EvalChannel));
    eval_mutex_init(&ch->mutex);
    eval_cond_init(&ch->cond);
    ch->head = ch->tail = NULL;
    ch->closed = 0;
    return ch;
}

static void eval_channel_destroy(EvalChannel *ch) {
    if (!ch) return;
    eval_mutex_lock(&ch->mutex);
    ChannelItem *it = ch->head;
    while (it) {
        ChannelItem *next = it->next;
        free(it->data);
        free(it);
        it = next;
    }
    eval_mutex_unlock(&ch->mutex);
    eval_mutex_destroy(&ch->mutex);
    eval_cond_destroy(&ch->cond);
    free(ch);
}

void eval_channel_send(EvalChannel *ch, const char *data, size_t len) {
    ChannelItem *item = (ChannelItem *)malloc(sizeof(ChannelItem));
    item->data = (char *)malloc(len + 1);
    memcpy(item->data, data, len);
    item->data[len] = '\0';
    item->len = len;
    item->next = NULL;

    eval_mutex_lock(&ch->mutex);
    if (ch->tail) {
        ch->tail->next = item;
        ch->tail = item;
    } else {
        ch->head = ch->tail = item;
    }
    eval_cond_signal(&ch->cond);
    eval_mutex_unlock(&ch->mutex);
}

char *eval_channel_recv(EvalChannel *ch, size_t *out_len) {
    eval_mutex_lock(&ch->mutex);
    while (!ch->head && !ch->closed) {
        eval_cond_wait(&ch->cond, &ch->mutex);
    }
    if (!ch->head) {
        /* closed and empty */
        eval_mutex_unlock(&ch->mutex);
        if (out_len) *out_len = 0;
        return NULL;
    }
    ChannelItem *item = ch->head;
    ch->head = item->next;
    if (!ch->head) ch->tail = NULL;
    eval_mutex_unlock(&ch->mutex);

    char *data = item->data;
    if (out_len) *out_len = item->len;
    free(item);
    return data;
}

int eval_channel_try_recv(EvalChannel *ch, char **data, size_t *out_len) {
    eval_mutex_lock(&ch->mutex);
    if (!ch->head) {
        eval_mutex_unlock(&ch->mutex);
        return 0;
    }
    ChannelItem *item = ch->head;
    ch->head = item->next;
    if (!ch->head) ch->tail = NULL;
    eval_mutex_unlock(&ch->mutex);

    *data = item->data;
    if (out_len) *out_len = item->len;
    free(item);
    return 1;
}

void eval_channel_close(EvalChannel *ch) {
    eval_mutex_lock(&ch->mutex);
    ch->closed = 1;
    eval_cond_broadcast(&ch->cond);
    eval_mutex_unlock(&ch->mutex);
}

/* Convert binary data to text for Python consumption.
 * If the data starts with "CCHI" magic, deserialize and convert to text.
 * Otherwise return as-is (already text). */
static char *binary_to_text(const char *buf, size_t len, size_t *out_len) {
    if (len >= 4 && memcmp(buf, SER_MAGIC_BYTES, 4) == 0) {
        sexp ctx = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);
        sexp env = sexp_context_env(ctx);
        sexp_load_standard_env(ctx, env, SEXP_SEVEN);

        char err_msg[256] = {0};
        sexp val = deser_sexp_from_buf(ctx, env,
            (const unsigned char *)buf, len, err_msg, sizeof(err_msg));

        char *result;
        if (val != SEXP_FALSE || !err_msg[0]) {
            sexp str = sexp_write_to_string(ctx, val);
            result = sexp_stringp(str) ? strdup(sexp_string_data(str))
                                       : strdup("#<void>");
        } else {
            result = strdup("#<void>");
        }
        sexp_destroy_context(ctx);
        if (out_len) *out_len = strlen(result);
        return result;
    }
    /* Already text — duplicate so caller always owns */
    char *copy = (char *)malloc(len + 1);
    memcpy(copy, buf, len);
    copy[len] = '\0';
    if (out_len) *out_len = len;
    return copy;
}

char *eval_channel_recv_text(EvalChannel *ch, size_t *out_len) {
    size_t len;
    char *buf = eval_channel_recv(ch, &len);
    if (!buf) {
        if (out_len) *out_len = 0;
        return NULL;
    }
    char *result = binary_to_text(buf, len, out_len);
    free(buf);
    return result;
}

int eval_channel_try_recv_text(EvalChannel *ch, char **data, size_t *out_len) {
    char *raw = NULL;
    size_t len = 0;
    int got = eval_channel_try_recv(ch, &raw, &len);
    if (!got) return 0;
    *data = binary_to_text(raw, len, out_len);
    free(raw);
    return 1;
}

/* ================================================================
 * EvalFuture
 * ================================================================ */

struct EvalFuture {
    eval_mutex_t mutex;
    eval_cond_t cond;
    int completed;
    unsigned char *result;   /* binary serialized, or NULL */
    size_t result_len;       /* length of result data */
    char *result_text;       /* text representation for Python, or NULL */
    char *error;             /* error message, or NULL */
    volatile long refcount;  /* 2 = worker + caller; freed when 0 */
};

static EvalFuture *eval_future_create(void) {
    EvalFuture *f = (EvalFuture *)calloc(1, sizeof(EvalFuture));
    eval_mutex_init(&f->mutex);
    eval_cond_init(&f->cond);
    f->refcount = 2;  /* worker + caller */
    return f;
}

static void eval_future_destroy(EvalFuture *f) {
    eval_mutex_destroy(&f->mutex);
    eval_cond_destroy(&f->cond);
    free(f->result);
    free(f->result_text);
    free(f->error);
    free(f);
}

static void eval_future_decref(EvalFuture *f) {
#ifdef _WIN32
    long prev = InterlockedDecrement(&f->refcount);
#else
    long prev = __sync_sub_and_fetch(&f->refcount, 1);
#endif
    if (prev == 0)
        eval_future_destroy(f);
}

static void eval_future_complete(EvalFuture *f,
    const unsigned char *result, size_t result_len,
    const char *result_text, const char *error)
{
    eval_mutex_lock(&f->mutex);
    f->completed = 1;
    if (result && result_len > 0) {
        f->result = (unsigned char *)malloc(result_len);
        memcpy(f->result, result, result_len);
        f->result_len = result_len;
    }
    if (result_text) f->result_text = strdup(result_text);
    if (error) f->error = strdup(error);
    eval_cond_signal(&f->cond);
    eval_mutex_unlock(&f->mutex);
    eval_future_decref(f);  /* worker done with future */
}

int eval_future_wait(EvalFuture *f, unsigned char **result,
                     size_t *result_len, char **error) {
    eval_mutex_lock(&f->mutex);
    while (!f->completed) {
        eval_cond_wait(&f->cond, &f->mutex);
    }
    if (result) {
        if (f->result && f->result_len > 0) {
            *result = (unsigned char *)malloc(f->result_len);
            memcpy(*result, f->result, f->result_len);
        } else {
            *result = NULL;
        }
    }
    if (result_len) *result_len = f->result_len;
    if (error) *error = f->error ? strdup(f->error) : NULL;
    int ok = (f->error == NULL);
    eval_mutex_unlock(&f->mutex);
    return ok;
}

int eval_future_wait_text(EvalFuture *f, char **result, char **error) {
    eval_mutex_lock(&f->mutex);
    while (!f->completed) {
        eval_cond_wait(&f->cond, &f->mutex);
    }
    if (result) *result = f->result_text ? strdup(f->result_text) : NULL;
    if (error)  *error  = f->error ? strdup(f->error) : NULL;
    int ok = (f->error == NULL);
    eval_mutex_unlock(&f->mutex);
    return ok;
}

void eval_future_free(EvalFuture *f) {
    if (!f) return;
    eval_future_decref(f);  /* caller done with future */
}

int eval_future_ready(EvalFuture *f) {
    eval_mutex_lock(&f->mutex);
    int done = f->completed;
    eval_mutex_unlock(&f->mutex);
    return done;
}

/* ================================================================
 * EvalPool
 * ================================================================ */

typedef struct WorkItem {
    char *code;              /* Eval code string, or NULL */
    unsigned char *binary;   /* serialized (fn . args), or NULL */
    size_t binary_len;
    EvalFuture *future;
    struct WorkItem *next;
} WorkItem;

typedef struct ChannelEntry {
    char *name;
    EvalChannel *ch;
    struct ChannelEntry *next;
} ChannelEntry;

struct EvalPool {
    int num_workers;
    eval_thread_t *threads;
    eval_mutex_t work_mutex;
    eval_cond_t work_cond;
    WorkItem *work_head, *work_tail;
    int shutdown;
    eval_mutex_t chan_mutex;   /* protects channel list */
    ChannelEntry *channels;
    char *prelude_code;
};

typedef struct {
    EvalPool *pool;
    int worker_id;
} WorkerArg;

/* ================================================================
 * Bridge functions for workers (pure C, no Python)
 * ================================================================ */

/* Write a value to a chibi output port using display semantics */
static void display_to_port_c(sexp ctx, sexp x, sexp port) {
    if (sexp_stringp(x)) {
        sexp_write_string_n(ctx, sexp_string_data(x), sexp_string_size(x), port);
    } else if (sexp_charp(x)) {
        sexp_write_char(ctx, sexp_unbox_character(x), port);
    } else {
        sexp_write(ctx, x, port);
    }
}

/* display(x [, port]) - write to port or stdout */
static sexp bridge_display_c(sexp ctx, sexp self, sexp_sint_t n, sexp args) {
    if (!sexp_pairp(args)) return SEXP_VOID;
    sexp x = sexp_car(args);
    sexp rest = sexp_cdr(args);
    if (sexp_pairp(rest) && sexp_oportp(sexp_car(rest))) {
        display_to_port_c(ctx, x, sexp_car(rest));
        return SEXP_VOID;
    }
    if (sexp_stringp(x)) {
        fwrite(sexp_string_data(x), 1, sexp_string_size(x), stdout);
    } else {
        sexp str = sexp_write_to_string(ctx, x);
        if (sexp_stringp(str))
            fwrite(sexp_string_data(str), 1, sexp_string_size(str), stdout);
    }
    return SEXP_VOID;
}

/* print(x) - display + newline */
static sexp bridge_print_c(sexp ctx, sexp self, sexp_sint_t n, sexp args) {
    if (!sexp_pairp(args)) return SEXP_VOID;
    sexp x = sexp_car(args);
    if (sexp_stringp(x)) {
        fwrite(sexp_string_data(x), 1, sexp_string_size(x), stdout);
    } else {
        sexp str = sexp_write_to_string(ctx, x);
        if (sexp_stringp(str))
            fwrite(sexp_string_data(str), 1, sexp_string_size(str), stdout);
    }
    fputc('\n', stdout);
    fflush(stdout);
    return SEXP_VOID;
}

/* newline([port]) */
static sexp bridge_newline_c(sexp ctx, sexp self, sexp_sint_t n, sexp args) {
    if (sexp_pairp(args) && sexp_oportp(sexp_car(args))) {
        sexp_write_char(ctx, '\n', sexp_car(args));
        return SEXP_VOID;
    }
    fputc('\n', stdout);
    fflush(stdout);
    return SEXP_VOID;
}

/* eval-include(filename) — include with extension-based dispatch:
   .eval files are parsed with the Eval parser; everything else uses Scheme include. */
static sexp bridge_eval_include(sexp ctx, sexp self, sexp_sint_t n, sexp filename) {
    if (!sexp_stringp(filename))
        return sexp_type_exception(ctx, self, SEXP_STRING, filename);

    const char *name = sexp_string_data(filename);
    sexp env = sexp_context_env(ctx);

    /* Try to resolve file via chibi module path search */
    char *resolved = sexp_find_module_file_raw(ctx, name);
    const char *path = resolved ? resolved : name;

    /* Check extension */
    size_t len = strlen(path);
    int is_eval = (len >= 5 &&
                   path[len-5] == '.' &&
                   path[len-4] == 'e' &&
                   path[len-3] == 'v' &&
                   path[len-2] == 'a' &&
                   path[len-1] == 'l');

    if (is_eval) {
        /* Read file */
        FILE *f = fopen(path, "rb");
        if (!f) {
            if (resolved) free(resolved);
            return sexp_user_exception(ctx, self, "cannot open file", filename);
        }
        fseek(f, 0, SEEK_END);
        long size = ftell(f);
        fseek(f, 0, SEEK_SET);
        if (size < 0) {
            fclose(f);
            if (resolved) free(resolved);
            return sexp_user_exception(ctx, self, "cannot read file", filename);
        }
        char *source = (char *)malloc(size + 1);
        size_t nread = fread(source, 1, size, f);
        fclose(f);
        source[nread] = '\0';

        /* Parse as Eval */
        char *error_msg = NULL;
        int error_line = 0, error_col = 0;
        sexp_gc_var2(parsed, result);
        sexp_gc_preserve2(ctx, parsed, result);

        parsed = eval_parse(ctx, env, source, &error_msg, &error_line, &error_col);
        free(source);
        if (resolved) free(resolved);

        if (error_msg) {
            /* Build error message with filename context */
            char errbuf[512];
            snprintf(errbuf, sizeof(errbuf), "%s:%d:%d: %s",
                     name, error_line, error_col, error_msg);
            free(error_msg);
            sexp_gc_release2(ctx);
            return sexp_user_exception(ctx, self, errbuf, SEXP_NULL);
        }

        if (parsed == SEXP_VOID) {
            sexp_gc_release2(ctx);
            return SEXP_VOID;
        }

        result = sexp_eval(ctx, parsed, env);
        sexp_gc_release2(ctx);
        return result;
    } else {
        /* Scheme file — use chibi's loader */
        sexp result = sexp_load_module_file(ctx, path, env);
        if (resolved) free(resolved);
        return result;
    }
}

/* channel_send(ch, val) */
static sexp bridge_channel_send(sexp ctx, sexp self, sexp_sint_t n,
                                 sexp ch, sexp val) {
    EvalChannel *c = unwrap_channel(ch);
    if (!c) return sexp_user_exception(ctx, self, "channel_send: not a channel", ch);

    unsigned char *buf = NULL;
    size_t buf_len = 0;
    char err_msg[256] = {0};
    if (ser_sexp_to_buf(ctx, sexp_context_env(ctx), val,
                        &buf, &buf_len, err_msg, sizeof(err_msg)) != 0)
        return sexp_user_exception(ctx, self, err_msg, val);
    eval_channel_send(c, (char *)buf, buf_len);
    free(buf);
    return SEXP_VOID;
}

/* channel_recv(ch) -> val (blocks) */
static sexp bridge_channel_recv(sexp ctx, sexp self, sexp_sint_t n, sexp ch) {
    EvalChannel *c = unwrap_channel(ch);
    if (!c) return sexp_user_exception(ctx, self, "channel_recv: not a channel", ch);

    size_t len;
    char *buf = eval_channel_recv(c, &len);
    if (!buf) return SEXP_EOF;  /* channel closed */

    sexp result;
    if (len >= 4 && memcmp(buf, SER_MAGIC_BYTES, 4) == 0) {
        /* Binary format */
        char err_msg[256] = {0};
        result = deser_sexp_from_buf(ctx, sexp_context_env(ctx),
            (const unsigned char *)buf, len, err_msg, sizeof(err_msg));
        if (result == SEXP_FALSE && err_msg[0]) {
            free(buf);
            return sexp_user_exception(ctx, self, err_msg, SEXP_NULL);
        }
    } else {
        /* Text format (from Python-side send) */
        result = sexp_read_from_string(ctx, buf, (sexp_sint_t)len);
    }
    free(buf);
    return result;
}

/* channel_close(ch) */
static sexp bridge_channel_close(sexp ctx, sexp self, sexp_sint_t n, sexp ch) {
    EvalChannel *c = unwrap_channel(ch);
    if (!c) return sexp_user_exception(ctx, self, "channel_close: not a channel", ch);
    eval_channel_close(c);
    return SEXP_VOID;
}

/* channel_try_recv(ch) -> (value) list or #f (non-blocking) */
static sexp bridge_channel_try_recv(sexp ctx, sexp self, sexp_sint_t n, sexp ch) {
    EvalChannel *c = unwrap_channel(ch);
    if (!c) return sexp_user_exception(ctx, self, "channel_try_recv: not a channel", ch);

    char *data = NULL;
    size_t len = 0;
    int got = eval_channel_try_recv(c, &data, &len);
    if (!got) return SEXP_FALSE;

    sexp_gc_var2(val, result);
    sexp_gc_preserve2(ctx, val, result);

    if (len >= 4 && memcmp(data, SER_MAGIC_BYTES, 4) == 0) {
        char err_msg[256] = {0};
        val = deser_sexp_from_buf(ctx, sexp_context_env(ctx),
            (const unsigned char *)data, len, err_msg, sizeof(err_msg));
    } else {
        val = sexp_read_from_string(ctx, data, (sexp_sint_t)len);
    }
    free(data);
    result = sexp_cons(ctx, val, SEXP_NULL);

    sexp_gc_release2(ctx);
    return result;
}

/* make_channel("name") - create or get named channel on pool */
static sexp bridge_make_channel(sexp ctx, sexp self, sexp_sint_t n, sexp name_sexp) {
    if (!sexp_stringp(name_sexp))
        return sexp_user_exception(ctx, self, "make_channel: expected string name", name_sexp);

    /* Get pool pointer from __pool__ */
    sexp pool_sym = sexp_intern(ctx, "__pool__", -1);
    sexp pool_sexp = sexp_env_ref(ctx, sexp_context_env(ctx), pool_sym, SEXP_VOID);
    if (pool_sexp == SEXP_VOID || !sexp_cpointerp(pool_sexp))
        return sexp_user_exception(ctx, self, "make_channel: no pool", SEXP_NULL);

    EvalPool *pool = (EvalPool *)sexp_cpointer_value(pool_sexp);
    const char *name = sexp_string_data(name_sexp);

    EvalChannel *ch = eval_pool_channel(pool, name);
    return wrap_channel(ctx, ch);
}

/* pool_submit(code) - submit work to pool, return future */
static sexp bridge_pool_submit(sexp ctx, sexp self, sexp_sint_t n, sexp code) {
    if (!sexp_stringp(code))
        return sexp_user_exception(ctx, self, "pool_submit: expected string", code);

    sexp pool_sym = sexp_intern(ctx, "__pool__", -1);
    sexp pool_sexp = sexp_env_ref(ctx, sexp_context_env(ctx), pool_sym, SEXP_VOID);
    if (pool_sexp == SEXP_VOID || !sexp_cpointerp(pool_sexp))
        return sexp_user_exception(ctx, self, "pool_submit: no pool", SEXP_NULL);

    EvalPool *pool = (EvalPool *)sexp_cpointer_value(pool_sexp);
    EvalFuture *f = eval_pool_submit(pool, sexp_string_data(code));
    return sexp_make_cpointer(ctx, SEXP_CPOINTER, f, SEXP_FALSE, 0);
}

/* pool_result(future) - block until done, return result */
static sexp bridge_pool_result(sexp ctx, sexp self, sexp_sint_t n, sexp fut) {
    if (!sexp_cpointerp(fut))
        return sexp_user_exception(ctx, self, "pool_result: not a future", fut);

    EvalFuture *f = (EvalFuture *)sexp_cpointer_value(fut);
    unsigned char *result = NULL;
    size_t result_len = 0;
    char *error = NULL;
    eval_future_wait(f, &result, &result_len, &error);

    if (error) {
        sexp exc = sexp_user_exception(ctx, self, error, SEXP_NULL);
        free(error);
        free(result);
        return exc;
    }

    sexp val = SEXP_VOID;
    if (result && result_len > 0) {
        char err_msg[256] = {0};
        val = deser_sexp_from_buf(ctx, sexp_context_env(ctx),
                                  result, result_len, err_msg, sizeof(err_msg));
        free(result);
        if (val == SEXP_FALSE && err_msg[0])
            return sexp_user_exception(ctx, self, err_msg, SEXP_NULL);
    } else {
        free(result);
    }
    return val;
}

/* future_ready?(future) -> #t or #f (non-blocking) */
static sexp bridge_future_ready(sexp ctx, sexp self, sexp_sint_t n, sexp fut) {
    if (!sexp_cpointerp(fut))
        return sexp_user_exception(ctx, self, "future_ready?: not a future", fut);
    EvalFuture *f = (EvalFuture *)sexp_cpointer_value(fut);
    return eval_future_ready(f) ? SEXP_TRUE : SEXP_FALSE;
}

/* make_pool(num_workers) -> pool */
static sexp bridge_make_pool(sexp ctx, sexp self, sexp_sint_t n, sexp num_workers) {
    if (!sexp_fixnump(num_workers))
        return sexp_user_exception(ctx, self, "make_pool: expected integer", num_workers);
    int nw = (int)sexp_unbox_fixnum(num_workers);
    if (nw < 1) nw = 1;
    EvalPool *pool = eval_pool_create(nw, NULL);
    return wrap_pool(ctx, pool);
}

/* pool_submit(pool, code) -> future (explicit pool, arity 2) */
static sexp bridge_pool_submit_explicit(sexp ctx, sexp self, sexp_sint_t n,
                                         sexp pool_sexp, sexp code) {
    EvalPool *pool = unwrap_pool(pool_sexp);
    if (!pool) return sexp_user_exception(ctx, self, "pool_submit: not a pool", pool_sexp);
    if (!sexp_stringp(code))
        return sexp_user_exception(ctx, self, "pool_submit: expected string", code);
    EvalFuture *f = eval_pool_submit(pool, sexp_string_data(code));
    return sexp_make_cpointer(ctx, SEXP_CPOINTER, f, SEXP_FALSE, 0);
}

/* pool_channel(pool, name) -> channel (explicit pool, arity 2) */
static sexp bridge_pool_channel_explicit(sexp ctx, sexp self, sexp_sint_t n,
                                          sexp pool_sexp, sexp name) {
    EvalPool *pool = unwrap_pool(pool_sexp);
    if (!pool) return sexp_user_exception(ctx, self, "pool_channel: not a pool", pool_sexp);
    if (!sexp_stringp(name))
        return sexp_user_exception(ctx, self, "pool_channel: expected string name", name);
    EvalChannel *ch = eval_pool_channel(pool, sexp_string_data(name));
    return wrap_channel(ctx, ch);
}

/* pool_shutdown(pool) */
static sexp bridge_pool_shutdown(sexp ctx, sexp self, sexp_sint_t n, sexp pool_sexp) {
    EvalPool *pool = unwrap_pool(pool_sexp);
    if (!pool) return sexp_user_exception(ctx, self, "pool_shutdown: not a pool", pool_sexp);
    eval_pool_destroy(pool);
    sexp_cpointer_value(pool_sexp) = NULL;
    return SEXP_VOID;
}

/* Bitwise operations (pure C, shared with workers) */
static sexp bridge_bitwise_and_c(sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b) {
    if (!sexp_fixnump(a) || !sexp_fixnump(b))
        return sexp_user_exception(ctx, self, "bitwise-and: expected integers", a);
    return sexp_make_fixnum(sexp_unbox_fixnum(a) & sexp_unbox_fixnum(b));
}

static sexp bridge_bitwise_ior_c(sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b) {
    if (!sexp_fixnump(a) || !sexp_fixnump(b))
        return sexp_user_exception(ctx, self, "bitwise-ior: expected integers", a);
    return sexp_make_fixnum(sexp_unbox_fixnum(a) | sexp_unbox_fixnum(b));
}

static sexp bridge_bitwise_xor_c(sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b) {
    if (!sexp_fixnump(a) || !sexp_fixnump(b))
        return sexp_user_exception(ctx, self, "bitwise-xor: expected integers", a);
    return sexp_make_fixnum(sexp_unbox_fixnum(a) ^ sexp_unbox_fixnum(b));
}

static sexp bridge_bitwise_not_c(sexp ctx, sexp self, sexp_sint_t n, sexp a) {
    if (!sexp_fixnump(a))
        return sexp_user_exception(ctx, self, "bitwise-not: expected integer", a);
    return sexp_make_fixnum(~sexp_unbox_fixnum(a));
}

static sexp bridge_arithmetic_shift_c(sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b) {
    if (!sexp_fixnump(a) || !sexp_fixnump(b))
        return sexp_user_exception(ctx, self, "arithmetic-shift: expected integers", a);
    sexp_sint_t val = sexp_unbox_fixnum(a);
    sexp_sint_t shift = sexp_unbox_fixnum(b);
    return sexp_make_fixnum(shift >= 0 ? val << shift : val >> (-shift));
}

static sexp bridge_shift_right_c(sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b) {
    if (!sexp_fixnump(a) || !sexp_fixnump(b))
        return sexp_user_exception(ctx, self, "shift-right: expected integers", a);
    return sexp_make_fixnum(sexp_unbox_fixnum(a) >> sexp_unbox_fixnum(b));
}

/* op("+")/bridge_op for workers */
static sexp bridge_op_c(sexp ctx, sexp self, sexp_sint_t n, sexp name) {
    if (!sexp_stringp(name))
        return sexp_user_exception(ctx, self, "op: expected string", name);

    const char *s = sexp_string_data(name);
    const char *scheme_name = NULL;

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
    else scheme_name = s;

    sexp sym = sexp_intern(ctx, scheme_name, -1);
    sexp val = sexp_env_ref(ctx, sexp_context_env(ctx), sym, SEXP_VOID);
    if (val == SEXP_VOID)
        return sexp_user_exception(ctx, self, "op: unknown operator", name);
    return val;
}

/* exception-message */
static sexp bridge_exception_message_c(sexp ctx, sexp self, sexp_sint_t n, sexp exn) {
    if (sexp_exceptionp(exn)) {
        sexp msg = sexp_exception_message(exn);
        return msg ? msg : sexp_c_string(ctx, "", -1);
    }
    return sexp_c_string(ctx, "", -1);
}

/* current-second — high-resolution wall-clock time in seconds */
#ifdef _WIN32
static sexp bridge_current_second_c(sexp ctx, sexp self, sexp_sint_t n) {
    LARGE_INTEGER freq, count;
    QueryPerformanceFrequency(&freq);
    QueryPerformanceCounter(&count);
    return sexp_make_flonum(ctx, (double)count.QuadPart / (double)freq.QuadPart);
}
#else
#include <sys/time.h>
static sexp bridge_current_second_c(sexp ctx, sexp self, sexp_sint_t n) {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return sexp_make_flonum(ctx, tv.tv_sec + tv.tv_usec / 1000000.0);
}
#endif

/* pool_apply(fn, args) - serialize and submit to pool (worker context, 2 args) */
static sexp bridge_pool_apply(sexp ctx, sexp self, sexp_sint_t n,
                               sexp fn, sexp args) {
    if (!sexp_procedurep(fn) && !sexp_opcodep(fn))
        return sexp_user_exception(ctx, self, "pool_apply: expected procedure", fn);
    if (!sexp_pairp(args) && args != SEXP_NULL)
        return sexp_user_exception(ctx, self, "pool_apply: expected list or null", args);

    sexp pool_sym = sexp_intern(ctx, "__pool__", -1);
    sexp pool_sexp = sexp_env_ref(ctx, sexp_context_env(ctx), pool_sym, SEXP_VOID);
    if (pool_sexp == SEXP_VOID || !sexp_cpointerp(pool_sexp))
        return sexp_user_exception(ctx, self, "pool_apply: no pool", SEXP_NULL);
    EvalPool *pool = (EvalPool *)sexp_cpointer_value(pool_sexp);

    sexp_gc_var1(blob);
    sexp_gc_preserve1(ctx, blob);
    blob = sexp_cons(ctx, fn, args);

    unsigned char *buf = NULL;
    size_t buf_len = 0;
    char err_msg[256] = {0};
    if (ser_sexp_to_buf(ctx, sexp_context_env(ctx), blob,
                        &buf, &buf_len, err_msg, sizeof(err_msg)) != 0) {
        sexp_gc_release1(ctx);
        return sexp_user_exception(ctx, self, err_msg, fn);
    }

    EvalFuture *f = eval_pool_apply(pool, buf, buf_len);
    free(buf);
    sexp_gc_release1(ctx);
    return sexp_make_cpointer(ctx, SEXP_CPOINTER, f, SEXP_FALSE, 0);
}

/* pool_apply(pool, fn, args) - serialize and submit (main context, 3 args) */
static sexp bridge_pool_apply_explicit(sexp ctx, sexp self, sexp_sint_t n,
                                        sexp pool_sexp, sexp fn, sexp args) {
    EvalPool *pool = unwrap_pool(pool_sexp);
    if (!pool) return sexp_user_exception(ctx, self, "pool_apply: not a pool", pool_sexp);
    if (!sexp_procedurep(fn) && !sexp_opcodep(fn))
        return sexp_user_exception(ctx, self, "pool_apply: expected procedure", fn);
    if (!sexp_pairp(args) && args != SEXP_NULL)
        return sexp_user_exception(ctx, self, "pool_apply: expected list or null", args);

    sexp_gc_var1(blob);
    sexp_gc_preserve1(ctx, blob);
    blob = sexp_cons(ctx, fn, args);

    unsigned char *buf = NULL;
    size_t buf_len = 0;
    char err_msg[256] = {0};
    if (ser_sexp_to_buf(ctx, sexp_context_env(ctx), blob,
                        &buf, &buf_len, err_msg, sizeof(err_msg)) != 0) {
        sexp_gc_release1(ctx);
        return sexp_user_exception(ctx, self, err_msg, fn);
    }

    EvalFuture *f = eval_pool_apply(pool, buf, buf_len);
    free(buf);
    sexp_gc_release1(ctx);
    return sexp_make_cpointer(ctx, SEXP_CPOINTER, f, SEXP_FALSE, 0);
}

/* Register all pure-C bridge functions in a worker context */
#ifdef EVAL_STANDALONE
void register_bridge_functions_c(sexp ctx, sexp env) {
#else
static void register_bridge_functions_c(sexp ctx, sexp env) {
#endif
    /* I/O */
    sexp_define_foreign_proc_rest(ctx, env, "display", 0, bridge_display_c);
    sexp_define_foreign_proc_rest(ctx, env, "print", 0, bridge_print_c);
    sexp_define_foreign_proc_rest(ctx, env, "newline", 0, bridge_newline_c);

    /* Include */
    sexp_define_foreign(ctx, env, "eval-include", 1, bridge_eval_include);

    /* Channels */
    sexp_define_foreign(ctx, env, "channel-send", 2, bridge_channel_send);
    sexp_define_foreign(ctx, env, "channel_send", 2, bridge_channel_send);
    sexp_define_foreign(ctx, env, "channel-recv", 1, bridge_channel_recv);
    sexp_define_foreign(ctx, env, "channel_recv", 1, bridge_channel_recv);
    sexp_define_foreign(ctx, env, "channel-close", 1, bridge_channel_close);
    sexp_define_foreign(ctx, env, "channel_close", 1, bridge_channel_close);
    sexp_define_foreign(ctx, env, "make-channel", 1, bridge_make_channel);
    sexp_define_foreign(ctx, env, "make_channel", 1, bridge_make_channel);

    /* Pool */
    sexp_define_foreign(ctx, env, "pool-submit", 1, bridge_pool_submit);
    sexp_define_foreign(ctx, env, "pool_submit", 1, bridge_pool_submit);
    sexp_define_foreign(ctx, env, "pool-result", 1, bridge_pool_result);
    sexp_define_foreign(ctx, env, "pool_result", 1, bridge_pool_result);
    sexp_define_foreign(ctx, env, "future-result", 1, bridge_pool_result);
    sexp_define_foreign(ctx, env, "future_result", 1, bridge_pool_result);

    /* Pool apply (2-arg, implicit pool) */
    sexp_define_foreign(ctx, env, "pool-apply", 2, bridge_pool_apply);
    sexp_define_foreign(ctx, env, "pool_apply", 2, bridge_pool_apply);

    /* Non-blocking */
    sexp_define_foreign(ctx, env, "channel-try-recv", 1, bridge_channel_try_recv);
    sexp_define_foreign(ctx, env, "channel_try_recv", 1, bridge_channel_try_recv);
    sexp_define_foreign(ctx, env, "future-ready?", 1, bridge_future_ready);
    sexp_define_foreign(ctx, env, "future_ready?", 1, bridge_future_ready);

    /* Bitwise */
    sexp_define_foreign(ctx, env, "bitwise-and", 2, bridge_bitwise_and_c);
    sexp_define_foreign(ctx, env, "bitwise-ior", 2, bridge_bitwise_ior_c);
    sexp_define_foreign(ctx, env, "bitwise-xor", 2, bridge_bitwise_xor_c);
    sexp_define_foreign(ctx, env, "bitwise-not", 1, bridge_bitwise_not_c);
    sexp_define_foreign(ctx, env, "arithmetic-shift", 2, bridge_arithmetic_shift_c);
    sexp_define_foreign(ctx, env, "shift-right", 2, bridge_shift_right_c);

    /* Operators and exceptions */
    sexp_define_foreign(ctx, env, "op", 1, bridge_op_c);
    sexp_define_foreign(ctx, env, "exception-message", 1, bridge_exception_message_c);

    /* Timing */
    sexp_define_foreign(ctx, env, "current-second", 0, bridge_current_second_c);
    sexp_define_foreign(ctx, env, "current_second", 0, bridge_current_second_c);

    /* Continuation serialization (defined in _chibi_serialize.c) */
    {
        extern sexp bridge_serialize_continuation(sexp, sexp, sexp_sint_t, sexp);
        extern sexp bridge_deserialize_continuation(sexp, sexp, sexp_sint_t, sexp);
        sexp_define_foreign(ctx, env, "serialize-continuation", 1,
                            bridge_serialize_continuation);
        sexp_define_foreign(ctx, env, "serialize_continuation", 1,
                            bridge_serialize_continuation);
        sexp_define_foreign(ctx, env, "deserialize-continuation", 1,
                            bridge_deserialize_continuation);
        sexp_define_foreign(ctx, env, "deserialize_continuation", 1,
                            bridge_deserialize_continuation);
    }

    /* Evaluate Scheme string in isolated eval context */
    {
#ifndef EVAL_STANDALONE
        extern sexp bridge_eval_scheme(sexp, sexp, sexp_sint_t, sexp);
#endif
        sexp_define_foreign(ctx, env, "eval-scheme", 1, bridge_eval_scheme);
        sexp_define_foreign(ctx, env, "eval_scheme", 1, bridge_eval_scheme);
    }

}

/* Register pool/channel/future functions for the main (Python) context.
 * These take explicit pool arguments instead of looking up __pool__. */
void register_pool_eval_functions(sexp ctx, sexp env) {
    /* Pool lifecycle */
    sexp_define_foreign(ctx, env, "make-pool", 1, bridge_make_pool);
    sexp_define_foreign(ctx, env, "make_pool", 1, bridge_make_pool);
    sexp_define_foreign(ctx, env, "pool-shutdown", 1, bridge_pool_shutdown);
    sexp_define_foreign(ctx, env, "pool_shutdown", 1, bridge_pool_shutdown);

    /* Pool operations (2-arg, explicit pool) */
    sexp_define_foreign(ctx, env, "pool-submit", 2, bridge_pool_submit_explicit);
    sexp_define_foreign(ctx, env, "pool_submit", 2, bridge_pool_submit_explicit);
    sexp_define_foreign(ctx, env, "pool-channel", 2, bridge_pool_channel_explicit);
    sexp_define_foreign(ctx, env, "pool_channel", 2, bridge_pool_channel_explicit);

    /* Pool apply (3-arg, explicit pool) */
    sexp_define_foreign(ctx, env, "pool-apply", 3, bridge_pool_apply_explicit);
    sexp_define_foreign(ctx, env, "pool_apply", 3, bridge_pool_apply_explicit);

    /* Channel operations */
    sexp_define_foreign(ctx, env, "channel-send", 2, bridge_channel_send);
    sexp_define_foreign(ctx, env, "channel_send", 2, bridge_channel_send);
    sexp_define_foreign(ctx, env, "channel-recv", 1, bridge_channel_recv);
    sexp_define_foreign(ctx, env, "channel_recv", 1, bridge_channel_recv);
    sexp_define_foreign(ctx, env, "channel-close", 1, bridge_channel_close);
    sexp_define_foreign(ctx, env, "channel_close", 1, bridge_channel_close);
    sexp_define_foreign(ctx, env, "channel-try-recv", 1, bridge_channel_try_recv);
    sexp_define_foreign(ctx, env, "channel_try_recv", 1, bridge_channel_try_recv);

    /* Future operations */
    sexp_define_foreign(ctx, env, "future-result", 1, bridge_pool_result);
    sexp_define_foreign(ctx, env, "future_result", 1, bridge_pool_result);
    sexp_define_foreign(ctx, env, "future-ready?", 1, bridge_future_ready);
    sexp_define_foreign(ctx, env, "future_ready?", 1, bridge_future_ready);
}

/* Standard scheme aliases loaded in both Python and worker contexts */
#ifdef EVAL_STANDALONE
void eval_standard_aliases(sexp ctx, sexp env) {
#else
static void eval_standard_aliases(sexp ctx, sexp env) {
#endif
    sexp_eval_string(ctx,
        "(define error-object? exception?)", -1, env);
    sexp_eval_string(ctx,
        "(define error-object-message exception-message)", -1, env);

    /* Byte I/O */
    sexp_eval_string(ctx,
        "(define (read-u8 . o)"
        "  (let ((ch (if (pair? o) (read-char (car o)) (read-char))))"
        "    (if (eof-object? ch) ch (char->integer ch))))", -1, env);
    sexp_eval_string(ctx,
        "(define (write-u8 byte . o)"
        "  (if (pair? o)"
        "    (write-char (integer->char byte) (car o))"
        "    (write-char (integer->char byte))))", -1, env);
    sexp_eval_string(ctx,
        "(define (peek-u8 . o)"
        "  (let ((ch (if (pair? o) (peek-char (car o)) (peek-char))))"
        "    (if (eof-object? ch) ch (char->integer ch))))", -1, env);

    /* String functions */
    sexp_eval_string(ctx,
        "(define (string-map proc str)"
        "  (list->string (map proc (string->list str))))", -1, env);
    sexp_eval_string(ctx,
        "(define (string-for-each proc str)"
        "  (for-each proc (string->list str)))", -1, env);

    /* I/O functions */
    sexp_eval_string(ctx,
        "(define (write-string str . o)"
        "  (let ((port (if (pair? o) (car o) (current-output-port))))"
        "    (let lp ((i 0))"
        "      (if (< i (string-length str))"
        "          (begin (write-char (string-ref str i) port)"
        "                 (lp (+ i 1)))))))", -1, env);
    sexp_eval_string(ctx,
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
        "              (else (lp (cons ch res))))))))", -1, env);
    sexp_eval_string(ctx,
        "(define (read-string n . o)"
        "  (let ((port (if (pair? o) (car o) (current-input-port))))"
        "    (let lp ((i 0) (res '()))"
        "      (if (>= i n)"
        "          (list->string (reverse res))"
        "          (let ((ch (read-char port)))"
        "            (if (eof-object? ch)"
        "                (if (null? res) ch (list->string (reverse res)))"
        "                (lp (+ i 1) (cons ch res))))))))", -1, env);

    /* Core aliases */
    sexp_eval_string(ctx,
        "(define callcc call-with-current-continuation)", -1, env);

    /* filter, fold, make-parameter, sort are now provided by srfi/1
     * and srfi/39/syntax.scm, srfi/95/sort.scm via eval_init_all_libs. */

    /* SRFI-18 green thread wrappers */
    sexp_eval_string(ctx,
        "(define thread-yield! yield!)", -1, env);
    sexp_eval_string(ctx,
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
        "         (else (lp))))))))", -1, env);
    sexp_eval_string(ctx,
        "(define (thread-terminate! thread)"
        "  (if (%thread-terminate! thread) (thread-yield!)))", -1, env);
    sexp_eval_string(ctx,
        "(define (thread-sleep! timeout)"
        "  (%thread-sleep! timeout) (thread-yield!))", -1, env);
    sexp_eval_string(ctx,
        "(define (mutex-lock! mutex . o)"
        "  (let ((timeout (and (pair? o) (car o)))"
        "        (thread (if (and (pair? o) (pair? (cdr o))) (cadr o) #t)))"
        "    (cond"
        "     ((%mutex-lock! mutex timeout thread))"
        "     (else"
        "      (thread-yield!)"
        "      (if (thread-timeout?) #f"
        "          (mutex-lock! mutex timeout thread))))))", -1, env);
    sexp_eval_string(ctx,
        "(define (mutex-unlock! mutex . o)"
        "  (let ((condvar (and (pair? o) (car o)))"
        "        (timeout (if (and (pair? o) (pair? (cdr o))) (cadr o) #f)))"
        "    (cond"
        "     ((%mutex-unlock! mutex condvar timeout))"
        "     (else (thread-yield!) (not (thread-timeout?))))))", -1, env);

    /* Eval underscore aliases for SRFI-18 threads */
    sexp_eval_string(ctx,
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
        -1, env);

    /* Eval underscore aliases for other new libraries */
    sexp_eval_string(ctx,
        "(begin"
        " (define random_integer random-integer)"
        " (define random_real random-real)"
        " (define json_read json-read)"
        " (define json_write json-write)"
        " (define get_env get-environment-variable)"
        " (define current_clock_second current-clock-second)"
        " (define hash_table_cell hash-table-cell)"
        " (define bit_and bit-and)"
        " (define bit_ior bit-ior)"
        " (define bit_xor bit-xor)"
        " (define bit_count bit-count)"
        " (define arithmetic_shift arithmetic-shift)"
        " (define integer_length integer-length)"
        " (define object_cmp object-cmp)"
        " (define heap_stats heap-stats))",
        -1, env);

    /* Test framework underscore aliases */
    sexp_eval_string(ctx,
        "(define test_begin test-begin)", -1, env);
    sexp_eval_string(ctx,
        "(define test_end test-end)", -1, env);
    sexp_eval_string(ctx,
        "(define test_assert test-assert)", -1, env);
    sexp_eval_string(ctx,
        "(define test_error test-error)", -1, env);
    sexp_eval_string(ctx,
        "(define test_group test-group)", -1, env);

    /* Dict runtime: __make_eval_dict__ creates a closure wrapping a hash table */
    sexp_eval_string(ctx,
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
        "        (else #f)))))", -1, env);
    sexp_eval_string(ctx,
        "(define (dict? v)"
        "  (and (procedure? v)"
        "       (protect (e (else #f))"
        "         (eq? (v '__type__) '__dict__))))", -1, env);

    /* Async/await runtime: promise type backed by mutex+condvar */
    sexp_eval_string(ctx,
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
        "        (else (error \"promise: unknown message\" __msg__))))))", -1, env);
    sexp_eval_string(ctx,
        "(define (__promise-resolve!__ p val)"
        "  (protect (e (else ((p '__reject__) e)))"
        "    ((p '__resolve__) val)))", -1, env);
    sexp_eval_string(ctx,
        "(define (promise? v)"
        "  (and (procedure? v)"
        "       (protect (e (else #f))"
        "         (eq? (v '__type__) '__promise__))))", -1, env);
    /* __await__: dispatches on type — promise or pool future (cpointer) */
    sexp_eval_string(ctx,
        "(define (__await__ x)"
        "  (cond"
        "    ((and (procedure? x)"
        "          (protect (e (else #f)) (eq? (x '__type__) '__promise__)))"
        "     (x '__await__))"
        "    (else (future-result x))))", -1, env);

}

/* OO wrappers for Pool, Channel, Future — only for main context
 * (workers don't have make-pool and use different pool-submit arity) */
#ifdef EVAL_STANDALONE
void eval_oo_wrappers(sexp ctx, sexp env) {
#else
static void eval_oo_wrappers(sexp ctx, sexp env) {
#endif
    /* OO wrapper: Channel-wrap — wraps raw channel cpointer */
    sexp_eval_string(ctx,
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
        -1, env);

    /* OO wrapper: Future-wrap — wraps raw cpointer future */
    sexp_eval_string(ctx,
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
        -1, env);

    /* OO wrapper: Pool(n) — wraps raw pool with -> access and RAII close */
    sexp_eval_string(ctx,
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
        -1, env);

    /* Update __await__ to handle OO Future objects too */
    sexp_eval_string(ctx,
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
        -1, env);
}

/* ================================================================
 * Reactive runtime: Signal, Computed, Effect, batch, dispose
 * ================================================================ */

void eval_reactive_runtime(sexp ctx, sexp env) {

    /* --- ID generator (no gensym in chibi) --- */
    sexp_eval_string(ctx,
        "(define __reactive-id-counter__ 0)", -1, env);
    sexp_eval_string(ctx,
        "(define (__reactive-gensym__ prefix)"
        "  (set! __reactive-id-counter__ (+ __reactive-id-counter__ 1))"
        "  (string-append prefix (number->string __reactive-id-counter__)))",
        -1, env);

    /* --- Auto-tracking parameter (thread-local via make-parameter) --- */
    sexp_eval_string(ctx,
        "(define __current-observer__ (make-parameter #f))", -1, env);

    /* --- Scope tracking --- */
    sexp_eval_string(ctx,
        "(define __reactive-current-scope__ #f)", -1, env);

    /* --- Batch state --- */
    sexp_eval_string(ctx,
        "(define __reactive-batch-depth__ 0)", -1, env);
    sexp_eval_string(ctx,
        "(define __reactive-batch-queue__ '())", -1, env);

    /* --- Notify: mark downstream dirty and schedule effects --- */
    sexp_eval_string(ctx,
        "(define (__reactive-notify__ observers)"
        "  (for-each"
        "    (lambda (node)"
        "      (node '__mark-dirty!__))"
        "    observers))", -1, env);

    /* --- Schedule an effect for batched flush (dedup by id) --- */
    sexp_eval_string(ctx,
        "(define (__reactive-schedule-effect__ node)"
        "  (let ((nid (node 'id)))"
        "    (if (not (find (lambda (pair) (equal? (cdr pair) node))"
        "                   __reactive-batch-queue__))"
        "      (let ((level (node 'level)))"
        "        (set! __reactive-batch-queue__"
        "          (cons (cons level node) __reactive-batch-queue__))))))",
        -1, env);

    /* --- Flush: sort by level, run each --- */
    sexp_eval_string(ctx,
        "(define (__reactive-flush__)"
        "  (let loop ()"
        "    (if (null? __reactive-batch-queue__) (if #f #f)"
        "      (let ((q (sort __reactive-batch-queue__"
        "                 (lambda (a b) (< (car a) (car b))))))"
        "        (set! __reactive-batch-queue__ '())"
        "        (for-each"
        "          (lambda (pair)"
        "            (let ((node (cdr pair)))"
        "              (node '__run!__)))"
        "          q)"
        "        (if (not (null? __reactive-batch-queue__))"
        "            (loop))))))", -1, env);

    /* --- Signal --- */
    sexp_eval_string(ctx,
        "(define (Signal initial-value . rest)"
        "  (let ((value initial-value)"
        "        (eq-fn (if (null? rest) equal? (car rest)))"
        "        (observers (make-hash-table))"
        "        (id (__reactive-gensym__ \"sig\")))"
        "    (letrec"
        "      ((notify!"
        "        (lambda ()"
        "          (let ((obs (hash-table-values observers)))"
        "            (if (null? obs) (if #f #f)"
        "              (begin"
        "                (if (= __reactive-batch-depth__ 0)"
        "                  (begin"
        "                    (set! __reactive-batch-depth__ 1)"
        "                    (__reactive-notify__ obs)"
        "                    (set! __reactive-batch-depth__ 0)"
        "                    (__reactive-flush__))"
        "                  (__reactive-notify__ obs)))))))"
        "       (self"
        "        (lambda args"
        "          (if (null? args)"
        "            (begin"
        "              (let ((obs (__current-observer__)))"
        "                (if obs ((obs 'register-source) self)))"
        "              value)"
        "            (let ((msg (car args)))"
        "              (cond"
        "                ((eq? msg 'set)"
        "                 (lambda (new-val)"
        "                   (if (not (eq-fn value new-val))"
        "                     (begin (set! value new-val)"
        "                            (notify!)))))"
        "                ((eq? msg 'update)"
        "                 (lambda (fn)"
        "                   (let ((new-val (fn value)))"
        "                     (if (not (eq-fn value new-val))"
        "                       (begin (set! value new-val)"
        "                              (notify!))))))"
        "                ((eq? msg 'peek) value)"
        "                ((eq? msg 'add-observer)"
        "                 (lambda (obs-id node)"
        "                   (hash-table-set! observers obs-id node)))"
        "                ((eq? msg 'remove-observer)"
        "                 (lambda (obs-id)"
        "                   (hash-table-delete! observers obs-id)))"
        "                ((eq? msg 'observers)"
        "                 (hash-table-values observers))"
        "                ((eq? msg 'level) 0)"
        "                ((eq? msg 'id) id)"
        "                ((eq? msg '__type__) '__signal__)"
        "                ((eq? msg 'close) (lambda ()"
        "                  (for-each (lambda (k) (hash-table-delete! observers k))"
        "                    (hash-table-keys observers))))"
        "                ((eq? msg 'dispose) (lambda ()"
        "                  (for-each (lambda (k) (hash-table-delete! observers k))"
        "                    (hash-table-keys observers))))"
        "                (else (error \"Signal: unknown message\" msg))))))))"
        "      (if __reactive-current-scope__"
        "        ((__reactive-current-scope__ 'register-child) self))"
        "      self)))", -1, env);

    /* --- Computed --- */
    sexp_eval_string(ctx,
        "(define (Computed fn . rest)"
        "  (let ((value #f)"
        "        (eq-fn (if (null? rest) equal? (car rest)))"
        "        (dirty #t)"
        "        (computing #f)"
        "        (disposed #f)"
        "        (sources (make-hash-table))"
        "        (observers (make-hash-table))"
        "        (id (__reactive-gensym__ \"comp\"))"
        "        (level 1))"
        "    (letrec"
        "      ((unsubscribe-all!"
        "        (lambda ()"
        "          (for-each"
        "            (lambda (src)"
        "              ((src 'remove-observer) id))"
        "            (hash-table-values sources))"
        "          (let ((ht (make-hash-table)))"
        "            (set! sources ht))))"
        "       (recompute!"
        "        (lambda ()"
        "          (if computing (error \"Computed: circular dependency\" id))"
        "          (set! computing #t)"
        "          (unsubscribe-all!)"
        "          (set! level 1)"
        "          (let ((new-val"
        "                 (protect (e (else"
        "                   (set! computing #f)"
        "                   (set! dirty #t)"
        "                   (raise e)))"
        "                   (parameterize ((__current-observer__ self))"
        "                     (fn)))))"
        "            (set! computing #f)"
        "            (set! dirty #f)"
        "            (let ((changed (not (eq-fn value new-val))))"
        "              (set! value new-val)"
        "              changed))))"
        "       (self"
        "        (lambda args"
        "          (if (null? args)"
        "            (begin"
        "              (if (and dirty (not disposed))"
        "                (recompute!))"
        "              (let ((obs (__current-observer__)))"
        "                (if obs ((obs 'register-source) self)))"
        "              value)"
        "            (let ((msg (car args)))"
        "              (cond"
        "                ((eq? msg 'register-source)"
        "                 (lambda (src)"
        "                   (let ((src-id (src 'id)))"
        "                     (if (not (hash-table-exists? sources src-id))"
        "                       (begin"
        "                         (hash-table-set! sources src-id src)"
        "                         ((src 'add-observer) id self)"
        "                         (let ((sl (src 'level)))"
        "                           (if (>= sl level)"
        "                             (set! level (+ sl 1)))))))))"
        "                ((eq? msg '__mark-dirty!__)"
        "                 (if (not dirty)"
        "                   (begin"
        "                     (set! dirty #t)"
        "                     (__reactive-schedule-effect__ self))))"
        "                ((eq? msg '__run!__)"
        "                 (if (and dirty (not disposed))"
        "                   (let ((changed (recompute!)))"
        "                     (if changed"
        "                       (__reactive-notify__"
        "                         (hash-table-values observers))))))"
        "                ((eq? msg 'add-observer)"
        "                 (lambda (obs-id node)"
        "                   (hash-table-set! observers obs-id node)))"
        "                ((eq? msg 'remove-observer)"
        "                 (lambda (obs-id)"
        "                   (hash-table-delete! observers obs-id)))"
        "                ((eq? msg 'level) level)"
        "                ((eq? msg 'id) id)"
        "                ((eq? msg '__type__) '__computed__)"
        "                ((eq? msg 'peek)"
        "                 (if (and dirty (not disposed))"
        "                   (recompute!))"
        "                 value)"
        "                ((eq? msg 'dirty?) dirty)"
        "                ((eq? msg 'close) (lambda ()"
        "                  (set! disposed #t)"
        "                  (unsubscribe-all!)))"
        "                ((eq? msg 'dispose) (lambda ()"
        "                  (set! disposed #t)"
        "                  (unsubscribe-all!)))"
        "                (else"
        "                  (error \"Computed: unknown message\" msg))))))))"
        "      (protect (e (else (set! computing #f) (set! dirty #t)))"
        "        (recompute!))"
        "      (if __reactive-current-scope__"
        "        ((__reactive-current-scope__ 'register-child) self))"
        "      self)))", -1, env);

    /* --- Effect --- */
    sexp_eval_string(ctx,
        "(define (Effect fn)"
        "  (let ((cleanup #f)"
        "        (disposed #f)"
        "        (sources (make-hash-table))"
        "        (id (__reactive-gensym__ \"eff\"))"
        "        (level 1))"
        "    (letrec"
        "      ((unsubscribe-all!"
        "        (lambda ()"
        "          (for-each"
        "            (lambda (src)"
        "              ((src 'remove-observer) id))"
        "            (hash-table-values sources))"
        "          (let ((ht (make-hash-table)))"
        "            (set! sources ht))))"
        "       (run!"
        "        (lambda ()"
        "          (if disposed (if #f #f)"
        "            (begin"
        "              (if (procedure? cleanup)"
        "                (protect (e (else (if #f #f))) (cleanup)))"
        "              (set! cleanup #f)"
        "              (unsubscribe-all!)"
        "              (set! level 1)"
        "              (let ((result"
        "                     (parameterize ((__current-observer__ self))"
        "                       (fn))))"
        "                (if (procedure? result)"
        "                  (set! cleanup result)))))))"
        "       (self"
        "        (lambda args"
        "          (if (null? args)"
        "            (if #f #f)"
        "            (let ((msg (car args)))"
        "              (cond"
        "                ((eq? msg 'register-source)"
        "                 (lambda (src)"
        "                   (let ((src-id (src 'id)))"
        "                     (if (not (hash-table-exists? sources src-id))"
        "                       (begin"
        "                         (hash-table-set! sources src-id src)"
        "                         ((src 'add-observer) id self)"
        "                         (let ((sl (src 'level)))"
        "                           (if (>= sl level)"
        "                             (set! level (+ sl 1)))))))))"
        "                ((eq? msg '__mark-dirty!__)"
        "                 (__reactive-schedule-effect__ self))"
        "                ((eq? msg '__run!__)"
        "                 (if (not disposed) (run!)))"
        "                ((eq? msg 'level) level)"
        "                ((eq? msg 'id) id)"
        "                ((eq? msg '__type__) '__effect__)"
        "                ((eq? msg 'close) (lambda ()"
        "                  (set! disposed #t)"
        "                  (if (procedure? cleanup)"
        "                    (protect (e (else (if #f #f))) (cleanup)))"
        "                  (set! cleanup #f)"
        "                  (unsubscribe-all!)))"
        "                ((eq? msg 'dispose) (lambda ()"
        "                  (set! disposed #t)"
        "                  (if (procedure? cleanup)"
        "                    (protect (e (else (if #f #f))) (cleanup)))"
        "                  (set! cleanup #f)"
        "                  (unsubscribe-all!)))"
        "                (else"
        "                  (error \"Effect: unknown message\" msg))))))))"
        "      (run!)"
        "      (if __reactive-current-scope__"
        "        ((__reactive-current-scope__ 'register-child) self))"
        "      self)))", -1, env);

    /* --- batch --- */
    sexp_eval_string(ctx,
        "(define (batch fn)"
        "  (set! __reactive-batch-depth__"
        "    (+ __reactive-batch-depth__ 1))"
        "  (let ((result"
        "         (protect (e (else"
        "           (set! __reactive-batch-depth__"
        "             (- __reactive-batch-depth__ 1))"
        "           (if (= __reactive-batch-depth__ 0)"
        "             (__reactive-flush__))"
        "           (raise e)))"
        "           (fn))))"
        "    (set! __reactive-batch-depth__"
        "      (- __reactive-batch-depth__ 1))"
        "    (if (= __reactive-batch-depth__ 0)"
        "      (__reactive-flush__))"
        "    result))", -1, env);

    /* --- dispose --- */
    sexp_eval_string(ctx,
        "(define (dispose node)"
        "  ((node 'dispose)))", -1, env);

    /* --- Type predicates --- */
    sexp_eval_string(ctx,
        "(define (signal? v)"
        "  (and (procedure? v)"
        "       (protect (e (else #f))"
        "         (eq? (v '__type__) '__signal__))))", -1, env);
    sexp_eval_string(ctx,
        "(define (computed? v)"
        "  (and (procedure? v)"
        "       (protect (e (else #f))"
        "         (eq? (v '__type__) '__computed__))))", -1, env);
    sexp_eval_string(ctx,
        "(define (effect? v)"
        "  (and (procedure? v)"
        "       (protect (e (else #f))"
        "         (eq? (v '__type__) '__effect__))))", -1, env);

    /* --- untracked: suppress dependency tracking --- */
    sexp_eval_string(ctx,
        "(define (untracked fn)"
        "  (parameterize ((__current-observer__ #f))"
        "    (fn)))", -1, env);

    /* --- derived: shorthand computed from one signal --- */
    sexp_eval_string(ctx,
        "(define (derived src fn)"
        "  (Computed (lambda () (fn (src)))))", -1, env);

    /* --- readonly: read-only view of a signal/computed --- */
    sexp_eval_string(ctx,
        "(define (readonly src)"
        "  (let ((id (__reactive-gensym__ \"ro\")))"
        "    (lambda args"
        "      (if (null? args)"
        "        (src)"
        "        (let ((msg (car args)))"
        "          (cond"
        "            ((eq? msg 'peek) (src 'peek))"
        "            ((eq? msg 'level) (src 'level))"
        "            ((eq? msg 'id) (src 'id))"
        "            ((eq? msg 'add-observer)"
        "             (lambda (obs-id node)"
        "               ((src 'add-observer) obs-id node)))"
        "            ((eq? msg 'remove-observer)"
        "             (lambda (obs-id)"
        "               ((src 'remove-observer) obs-id)))"
        "            ((eq? msg '__type__) '__readonly__)"
        "            ((eq? msg 'set) (error \"readonly: cannot set\"))"
        "            ((eq? msg 'update) (error \"readonly: cannot update\"))"
        "            ((eq? msg 'dispose) (error \"readonly: cannot dispose\"))"
        "            ((eq? msg 'close) (error \"readonly: cannot close\"))"
        "            (else (error \"readonly: unknown message\" msg))))))))",
        -1, env);

    /* --- readonly? predicate --- */
    sexp_eval_string(ctx,
        "(define (readonly? v)"
        "  (and (procedure? v)"
        "       (protect (e (else #f))"
        "         (eq? (v '__type__) '__readonly__))))", -1, env);

    /* --- watch: run fn(new, old) on changes --- */
    sexp_eval_string(ctx,
        "(define (watch src fn)"
        "  (let ((prev (src 'peek))"
        "        (first #t))"
        "    (Effect (lambda ()"
        "      (let ((curr (src)))"
        "        (if first"
        "          (set! first #f)"
        "          (let ((old prev))"
        "            (set! prev curr)"
        "            (fn curr old))))))))", -1, env);

    /* --- on: effect with explicit dependency list --- */
    sexp_eval_string(ctx,
        "(define (on deps fn)"
        "  (Effect (lambda ()"
        "    (let ((vals (map (lambda (d) (d)) deps)))"
        "      (parameterize ((__current-observer__ #f))"
        "        (apply fn vals))))))", -1, env);

    /* --- reduce: fold over signal changes --- */
    sexp_eval_string(ctx,
        "(define (reduce src fn initial)"
        "  (let ((acc (Signal initial)))"
        "    (watch src (lambda (new-val old-val)"
        "      ((acc 'set) (fn (acc 'peek) new-val))))"
        "    acc))", -1, env);

    /* --- scope: reactive ownership scope --- */
    sexp_eval_string(ctx,
        "(define (scope fn)"
        "  (let ((children '())"
        "        (id (__reactive-gensym__ \"scope\"))"
        "        (disposed #f)"
        "        (prev-scope __reactive-current-scope__))"
        "    (letrec"
        "      ((self"
        "        (lambda args"
        "          (if (null? args)"
        "            children"
        "            (let ((msg (car args)))"
        "              (cond"
        "                ((eq? msg 'register-child)"
        "                 (lambda (child)"
        "                   (if (not disposed)"
        "                     (set! children (cons child children)))))"
        "                ((eq? msg 'dispose) (lambda ()"
        "                  (if (not disposed)"
        "                    (begin"
        "                      (set! disposed #t)"
        "                      (for-each (lambda (child)"
        "                        (protect (e (else (if #f #f)))"
        "                          ((child 'dispose))))"
        "                        children)"
        "                      (set! children '())))))"
        "                ((eq? msg 'close) (lambda ()"
        "                  ((self 'dispose))))"
        "                ((eq? msg '__type__) '__scope__)"
        "                ((eq? msg 'id) id)"
        "                (else (error \"scope: unknown message\" msg))))))))"
        "      (set! __reactive-current-scope__ self)"
        "      (protect (e (else"
        "        (set! __reactive-current-scope__ prev-scope)"
        "        (raise e)))"
        "        (fn))"
        "      (set! __reactive-current-scope__ prev-scope)"
        "      self)))", -1, env);

    /* --- scope? predicate --- */
    sexp_eval_string(ctx,
        "(define (scope? v)"
        "  (and (procedure? v)"
        "       (protect (e (else #f))"
        "         (eq? (v '__type__) '__scope__))))", -1, env);

    /* --- WritableComputed: two-way computed --- */
    sexp_eval_string(ctx,
        "(define (WritableComputed getter setter)"
        "  (let ((inner (Computed getter)))"
        "    (lambda args"
        "      (if (null? args)"
        "        (inner)"
        "        (let ((msg (car args)))"
        "          (cond"
        "            ((eq? msg 'set)"
        "             (lambda (new-val) (setter new-val)))"
        "            ((eq? msg 'update)"
        "             (lambda (fn) (setter (fn (inner 'peek)))))"
        "            ((eq? msg '__type__) '__writable-computed__)"
        "            (else (inner msg))))))))", -1, env);

    /* --- writable_computed? predicate --- */
    sexp_eval_string(ctx,
        "(define (writable_computed? v)"
        "  (and (procedure? v)"
        "       (protect (e (else #f))"
        "         (eq? (v '__type__) '__writable-computed__))))", -1, env);

    /* --- combine: merge N signals into one computed --- */
    sexp_eval_string(ctx,
        "(define (combine sources fn)"
        "  (Computed (lambda ()"
        "    (apply fn (map (lambda (s) (s)) sources)))))", -1, env);

    /* --- select: fine-grained slice with optional custom equality --- */
    sexp_eval_string(ctx,
        "(define (select src selector . rest)"
        "  (let ((eq-fn (if (null? rest) equal? (car rest))))"
        "    (Computed (lambda () (selector (src))) eq-fn)))", -1, env);

    /* --- prev: previous value signal --- */
    sexp_eval_string(ctx,
        "(define (prev src . rest)"
        "  (let ((initial (if (null? rest) #f (car rest))))"
        "    (let ((p (Signal initial)))"
        "      (watch src (lambda (new-val old-val)"
        "        ((p 'set) old-val)))"
        "      (readonly p))))", -1, env);

    /* --- trace: debug logging --- */
    sexp_eval_string(ctx,
        "(define (trace src label)"
        "  (watch src (lambda (new-val old-val)"
        "    (display (string-append \"[\" label \"] \"))"
        "    (display old-val)"
        "    (display \" -> \")"
        "    (display new-val)"
        "    (newline)))"
        "  src)", -1, env);

    /* --- resource: async data loading primitive --- */
    sexp_eval_string(ctx,
        "(define (resource source-or-fetcher . rest)"
        "  (let* ((has-source (not (null? rest)))"
        "         (src (if has-source source-or-fetcher #f))"
        "         (fetcher (if has-source (car rest) source-or-fetcher))"
        "         (initial (if (and has-source (pair? (cdr rest)))"
        "                    (cadr rest) #f))"
        "         (value-sig (Signal initial))"
        "         (loading-sig (Signal #t))"
        "         (error-sig (Signal #f))"
        "         (version 0)"
        "         (current-thread #f)"
        "         (watcher #f)"
        "         (id (__reactive-gensym__ \"res\")))"
        "    (letrec"
        "      ((do-fetch"
        "        (lambda (arg)"
        "          (set! version (+ version 1))"
        "          (let ((my-version version))"
        "            ((loading-sig 'set) #t)"
        "            ((error-sig 'set) #f)"
        "            (let ((t (make-thread (lambda ()"
        "                       (protect (e (else"
        "                         (if (= my-version version)"
        "                           (begin"
        "                             ((error-sig 'set) e)"
        "                             ((loading-sig 'set) #f)))))"
        "                         (let ((result (if has-source"
        "                                         (fetcher arg)"
        "                                         (fetcher))))"
        "                           (if (= my-version version)"
        "                             (begin"
        "                               ((value-sig 'set) result)"
        "                               ((loading-sig 'set) #f)))))))))"
        "              (set! current-thread t)"
        "              (thread-start! t)))))"
        "       (self"
        "        (lambda args"
        "          (if (null? args)"
        "            (value-sig)"
        "            (let ((msg (car args)))"
        "              (cond"
        "                ((eq? msg 'loading) (loading-sig))"
        "                ((eq? msg 'error) (error-sig))"
        "                ((eq? msg 'settle) (lambda ()"
        "                  (if current-thread"
        "                    (protect (e (else (if #f #f)))"
        "                      (thread-join! current-thread)))"
        "                  (thread-yield!)))"
        "                ((eq? msg 'refetch) (lambda ()"
        "                  (if has-source"
        "                    (do-fetch (src 'peek))"
        "                    (do-fetch #f))))"
        "                ((eq? msg 'mutate) (lambda (v)"
        "                  ((value-sig 'set) v)))"
        "                ((eq? msg 'peek) (value-sig 'peek))"
        "                ((eq? msg 'level) (value-sig 'level))"
        "                ((eq? msg 'id) id)"
        "                ((eq? msg 'add-observer)"
        "                 (lambda (obs-id node)"
        "                   ((value-sig 'add-observer) obs-id node)))"
        "                ((eq? msg 'remove-observer)"
        "                 (lambda (obs-id)"
        "                   ((value-sig 'remove-observer) obs-id)))"
        "                ((eq? msg '__type__) '__resource__)"
        "                ((eq? msg 'dispose) (lambda ()"
        "                  (set! version (+ version 1))"
        "                  (if watcher (dispose watcher))))"
        "                ((eq? msg 'close) (lambda ()"
        "                  ((self 'dispose))))"
        "                (else (error \"resource: unknown message\" msg))))))))"
        "      (if has-source"
        "        (begin"
        "          (do-fetch (src 'peek))"
        "          (set! watcher"
        "            (watch src (lambda (new-val old-val)"
        "              (do-fetch new-val)))))"
        "        (do-fetch #f))"
        "      (if __reactive-current-scope__"
        "        ((__reactive-current-scope__ 'register-child) self))"
        "      self)))", -1, env);

    /* --- resource? predicate --- */
    sexp_eval_string(ctx,
        "(define (resource? v)"
        "  (and (procedure? v)"
        "       (protect (e (else #f))"
        "         (eq? (v '__type__) '__resource__))))", -1, env);

}

/* Parse and evaluate Eval code in a worker context */
static sexp eval_parse_and_run(sexp ctx, sexp env, const char *code) {
    char *error_msg = NULL;
    int error_line = 0, error_col = 0;

    sexp_gc_var2(parsed, result);
    sexp_gc_preserve2(ctx, parsed, result);

    parsed = eval_parse(ctx, env, code, &error_msg, &error_line, &error_col);

    if (error_msg) {
        sexp err = sexp_user_exception(ctx, SEXP_FALSE, error_msg, SEXP_NULL);
        free(error_msg);
        sexp_gc_release2(ctx);
        return err;
    }

    if (parsed == SEXP_VOID) {
        sexp_gc_release2(ctx);
        return SEXP_VOID;
    }

    result = sexp_eval(ctx, parsed, env);
    sexp_gc_release2(ctx);
    return result;
}

/* Inject all named channels from pool into worker env */
static void inject_channels(sexp ctx, sexp env, EvalPool *pool) {
    eval_mutex_lock(&pool->chan_mutex);
    for (ChannelEntry *e = pool->channels; e; e = e->next) {
        sexp sym = sexp_intern(ctx, e->name, -1);
        if (sexp_env_ref(ctx, env, sym, SEXP_VOID) == SEXP_VOID) {
            sexp ch_sexp = wrap_channel(ctx, e->ch);
            sexp_env_define(ctx, env, sym, ch_sexp);
        }
    }
    eval_mutex_unlock(&pool->chan_mutex);
}

/* ================================================================
 * Worker thread
 * ================================================================ */

static EVAL_THREAD_FUNC worker_main(void *arg) {
    WorkerArg *wa = (WorkerArg *)arg;
    EvalPool *pool = wa->pool;
    free(wa);

    /* 1. Create independent root context */
    sexp ctx = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);
    sexp env = sexp_context_env(ctx);

    /* 2. Load standard env (.scm files loaded from embedded data) */
    sexp_load_standard_env(ctx, env, SEXP_SEVEN);
    sexp_load_standard_ports(ctx, env, NULL, NULL, NULL, 0);

    /* 3. Register types (same order as Python context for tag consistency) */
    register_pyobject_type(ctx);
    register_channel_type(ctx);
    register_pool_type(ctx);

    /* 4. Register C-only bridge functions */
    register_bridge_functions_c(ctx, env);

    /* 5. Load scheme extras, test framework */
    sexp_load_module_file(ctx, "scheme/extras.scm", env);
    sexp_load_module_file(ctx, "scheme/test.scm", env);
    env = sexp_context_env(ctx);

    /* 6. Initialize all statically compiled chibi libraries
     * (after extras.scm so define-record-type is available for types.scm) */
    {
        extern void eval_init_all_libs(sexp ctx, sexp env);
        eval_init_all_libs(ctx, env);
        env = sexp_context_env(ctx);
    }

    /* 7. Standard aliases (make-parameter, sort wrappers, thread wrappers, etc.) */
    eval_standard_aliases(ctx, env);

    /* 7. Store pool pointer as __pool__ in env */
    {
        sexp pool_sexp = sexp_make_cpointer(ctx, SEXP_CPOINTER, pool, SEXP_FALSE, 0);
        sexp_env_define(ctx, env, sexp_intern(ctx, "__pool__", -1), pool_sexp);
    }

    /* 8. Load prelude if any */
    eval_mutex_lock(&pool->work_mutex);
    char *prelude = pool->prelude_code ? strdup(pool->prelude_code) : NULL;
    eval_mutex_unlock(&pool->work_mutex);

    if (prelude) {
        eval_parse_and_run(ctx, env, prelude);
        free(prelude);
    }

    /* 9. Inject initial channels */
    inject_channels(ctx, env, pool);

    /* 10. Worker loop */
    while (1) {
        eval_mutex_lock(&pool->work_mutex);
        while (!pool->work_head && !pool->shutdown) {
            eval_cond_wait(&pool->work_cond, &pool->work_mutex);
        }
        if (pool->shutdown && !pool->work_head) {
            eval_mutex_unlock(&pool->work_mutex);
            break;
        }

        /* Dequeue work item */
        WorkItem *item = pool->work_head;
        pool->work_head = item->next;
        if (!pool->work_head) pool->work_tail = NULL;
        eval_mutex_unlock(&pool->work_mutex);

        /* Re-inject any new channels */
        inject_channels(ctx, env, pool);

        /* Execute */
        sexp result;
        if (item->binary && item->binary_len > 0) {
            /* Binary work item: deserialize (fn . args) and apply */
            char err_msg[256] = {0};
            sexp blob = deser_sexp_from_buf(ctx, env,
                item->binary, item->binary_len, err_msg, sizeof(err_msg));
            if (blob == SEXP_FALSE && err_msg[0]) {
                result = sexp_user_exception(ctx, SEXP_FALSE, err_msg, SEXP_NULL);
            } else {
                sexp_gc_var2(fn, args);
                sexp_gc_preserve2(ctx, fn, args);
                fn = sexp_car(blob);
                args = sexp_cdr(blob);
                result = sexp_apply(ctx, fn, args);
                sexp_gc_release2(ctx);
            }
        } else {
            result = eval_parse_and_run(ctx, env, item->code);
        }

        /* Complete future */
        if (sexp_exceptionp(result)) {
            sexp msg = sexp_exception_message(result);
            const char *err = sexp_stringp(msg) ? sexp_string_data(msg) : "evaluation error";
            eval_future_complete(item->future, NULL, 0, NULL, err);
        } else if (result == SEXP_VOID) {
            eval_future_complete(item->future, NULL, 0, "#<void>", NULL);
        } else {
            /* Binary serialization for C-level consumers */
            unsigned char *bin_buf = NULL;
            size_t bin_len = 0;
            char ser_err[256] = {0};
            int ser_ok = ser_sexp_to_buf(ctx, env, result,
                                         &bin_buf, &bin_len,
                                         ser_err, sizeof(ser_err));

            /* Text representation for Python consumers */
            const char *text = "#<void>";
            sexp str = sexp_write_to_string(ctx, result);
            if (sexp_stringp(str))
                text = sexp_string_data(str);

            if (ser_ok == 0) {
                eval_future_complete(item->future, bin_buf, bin_len, text, NULL);
                free(bin_buf);
            } else {
                /* Binary serialization failed (e.g. C pointer);
                 * fall back to text-only result */
                eval_future_complete(item->future, NULL, 0, text, NULL);
            }
        }

        free(item->code);
        free(item->binary);
        free(item);
    }

    sexp_destroy_context(ctx);
    EVAL_THREAD_RETURN;
}

/* ================================================================
 * Pool public API
 * ================================================================ */

EvalPool *eval_pool_create(int num_workers, const char *module_path) {
    (void)module_path;  /* .scm files loaded from embedded data */

    /* Ensure scheme is initialized (thread-safe: call once) */
    sexp_scheme_init();

    EvalPool *pool = (EvalPool *)calloc(1, sizeof(EvalPool));
    pool->num_workers = num_workers;
    pool->threads = (eval_thread_t *)calloc(num_workers, sizeof(eval_thread_t));
    eval_mutex_init(&pool->work_mutex);
    eval_cond_init(&pool->work_cond);
    eval_mutex_init(&pool->chan_mutex);

    /* Start worker threads */
    for (int i = 0; i < num_workers; i++) {
        WorkerArg *wa = (WorkerArg *)malloc(sizeof(WorkerArg));
        wa->pool = pool;
        wa->worker_id = i;
#ifdef _WIN32
        eval_thread_create(&pool->threads[i],
                           (eval_thread_func_t)worker_main, wa);
#else
        eval_thread_create(&pool->threads[i],
                           (eval_thread_func_t)worker_main, wa);
#endif
    }

    return pool;
}

void eval_pool_destroy(EvalPool *pool) {
    if (!pool) return;

    /* Signal shutdown */
    eval_mutex_lock(&pool->work_mutex);
    pool->shutdown = 1;
    eval_cond_broadcast(&pool->work_cond);
    eval_mutex_unlock(&pool->work_mutex);

    /* Join all workers */
    for (int i = 0; i < pool->num_workers; i++) {
        eval_thread_join(pool->threads[i]);
    }
    free(pool->threads);

    /* Free remaining work items */
    WorkItem *item = pool->work_head;
    while (item) {
        WorkItem *next = item->next;
        free(item->code);
        free(item->binary);
        /* Don't free futures - caller owns them */
        free(item);
        item = next;
    }

    /* Free channels */
    ChannelEntry *ce = pool->channels;
    while (ce) {
        ChannelEntry *next = ce->next;
        eval_channel_destroy(ce->ch);
        free(ce->name);
        free(ce);
        ce = next;
    }

    eval_mutex_destroy(&pool->work_mutex);
    eval_cond_destroy(&pool->work_cond);
    eval_mutex_destroy(&pool->chan_mutex);
    free(pool->prelude_code);
    free(pool);
}

void eval_pool_load_prelude(EvalPool *pool, const char *code) {
    eval_mutex_lock(&pool->work_mutex);
    free(pool->prelude_code);
    pool->prelude_code = code ? strdup(code) : NULL;
    eval_mutex_unlock(&pool->work_mutex);
}

EvalFuture *eval_pool_submit(EvalPool *pool, const char *code) {
    EvalFuture *f = eval_future_create();

    WorkItem *item = (WorkItem *)calloc(1, sizeof(WorkItem));
    item->code = strdup(code);
    item->future = f;

    eval_mutex_lock(&pool->work_mutex);
    if (pool->work_tail) {
        pool->work_tail->next = item;
        pool->work_tail = item;
    } else {
        pool->work_head = pool->work_tail = item;
    }
    eval_cond_signal(&pool->work_cond);
    eval_mutex_unlock(&pool->work_mutex);

    return f;
}

EvalFuture *eval_pool_apply(EvalPool *pool,
                            const unsigned char *data, size_t len) {
    EvalFuture *f = eval_future_create();

    WorkItem *item = (WorkItem *)calloc(1, sizeof(WorkItem));
    item->binary = (unsigned char *)malloc(len);
    memcpy(item->binary, data, len);
    item->binary_len = len;
    item->future = f;

    eval_mutex_lock(&pool->work_mutex);
    if (pool->work_tail) {
        pool->work_tail->next = item;
        pool->work_tail = item;
    } else {
        pool->work_head = pool->work_tail = item;
    }
    eval_cond_signal(&pool->work_cond);
    eval_mutex_unlock(&pool->work_mutex);

    return f;
}

EvalChannel *eval_pool_channel(EvalPool *pool, const char *name) {
    eval_mutex_lock(&pool->chan_mutex);

    /* Look for existing channel */
    for (ChannelEntry *e = pool->channels; e; e = e->next) {
        if (strcmp(e->name, name) == 0) {
            eval_mutex_unlock(&pool->chan_mutex);
            return e->ch;
        }
    }

    /* Create new channel */
    EvalChannel *ch = eval_channel_create();
    ChannelEntry *entry = (ChannelEntry *)malloc(sizeof(ChannelEntry));
    entry->name = strdup(name);
    entry->ch = ch;
    entry->next = pool->channels;
    pool->channels = entry;

    eval_mutex_unlock(&pool->chan_mutex);
    return ch;
}
