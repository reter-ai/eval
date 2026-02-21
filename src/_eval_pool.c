/*  _eval_pool.c -- C-level thread pool with channels for Eval  */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <chibi/eval.h>
#include "_eval_thread.h"
#include "_eval_pool.h"
#include "_eval_parser_helpers.h"
#include "eval_embedded_scm.h"

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
                        &buf, &buf_len, err_msg, sizeof(err_msg)) != 0) {
        return sexp_user_exception(ctx, self, err_msg, val);
    }
    eval_channel_send(c, (char *)buf, buf_len);
    free(buf);
    return SEXP_VOID;
}

/* Decode a raw channel buffer into a sexp value */
static sexp channel_decode_buf(sexp ctx, sexp self, char *buf, size_t len) {
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

/* %channel-recv(ch) -> val or #f (raw, green-thread aware)
 *
 * When green threads exist: non-blocking try_recv.  Returns #f and
 * sets waitp if nothing available (like %mutex-lock!).
 * Scheme wrapper retries with thread-yield! in between.
 * When no green threads: OS-level blocking recv. */
static sexp bridge_channel_recv(sexp ctx, sexp self, sexp_sint_t n, sexp ch) {
    EvalChannel *c = unwrap_channel(ch);
    if (!c) return sexp_user_exception(ctx, self, "channel_recv: not a channel", ch);

#if SEXP_USE_GREEN_THREADS
    {
        sexp front  = sexp_global(ctx, SEXP_G_THREADS_FRONT);
        sexp paused = sexp_global(ctx, SEXP_G_THREADS_PAUSED);
        if (sexp_pairp(front) || sexp_pairp(paused)) {
            /* Green threads active: non-blocking try */
            char *data = NULL;
            size_t len = 0;
            if (eval_channel_try_recv(c, &data, &len))
                return channel_decode_buf(ctx, self, data, len);
            if (c->closed)
                return SEXP_EOF;
            /* Nothing available — return #f so wrapper can yield and retry */
            return SEXP_FALSE;
        }
    }
#endif

    /* No green threads — use efficient OS-level blocking recv */
    size_t len;
    char *buf = eval_channel_recv(c, &len);
    if (!buf) return SEXP_EOF;  /* channel closed */
    return channel_decode_buf(ctx, self, buf, len);
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
    else if (strcmp(s, "@") == 0) scheme_name = "matmul";
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

/* === Generic indexing: ref(obj, idx) === */
static sexp bridge_ref_c(sexp ctx, sexp self, sexp_sint_t n, sexp obj, sexp idx) {
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
static sexp bridge_slice_c(sexp ctx, sexp self, sexp_sint_t n,
                            sexp obj, sexp start_s, sexp end_s) {
    sexp_sint_t len;

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

    sexp_sint_t start = 0;
    if (start_s != SEXP_FALSE) {
        if (!sexp_fixnump(start_s))
            return sexp_user_exception(ctx, self, "slice: expected integer start", start_s);
        start = sexp_unbox_fixnum(start_s);
        if (start < 0) start += len;
    }

    sexp_sint_t end = len;
    if (end_s != SEXP_FALSE) {
        if (!sexp_fixnump(end_s))
            return sexp_user_exception(ctx, self, "slice: expected integer end", end_s);
        end = sexp_unbox_fixnum(end_s);
        if (end < 0) end += len;
    }

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

/* __tostr__(x) -> fast value-to-string for f-string interpolation (pure C) */
static sexp bridge_tostr_c(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
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
    sexp str = sexp_write_to_string(ctx, x);
    return sexp_stringp(str) ? str : sexp_c_string(ctx, "?", 1);
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
    sexp_define_foreign(ctx, env, "%channel-recv", 1, bridge_channel_recv);
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

    /* F-string tostr */
    sexp_define_foreign(ctx, env, "__tostr__", 1, bridge_tostr_c);

    /* Indexing and slicing */
    sexp_define_foreign(ctx, env, "ref", 2, bridge_ref_c);
    sexp_define_foreign(ctx, env, "slice", 3, bridge_slice_c);

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
    sexp_define_foreign(ctx, env, "%channel-recv", 1, bridge_channel_recv);
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

/* Standard scheme aliases loaded in Python, standalone, and worker contexts */
void eval_standard_aliases(sexp ctx, sexp env) {
    /* Base definitions: error-object aliases, byte I/O, string/IO fns, callcc */
    sexp_load_module_file(ctx, "eval/base.scm", env);
    env = sexp_context_env(ctx);

    /* SRFI-18 green thread wrappers */
    sexp_load_module_file(ctx, "eval/threads.scm", env);
    env = sexp_context_env(ctx);

    /* Underscore aliases for all libraries */
    sexp_load_module_file(ctx, "eval/aliases.scm", env);
    env = sexp_context_env(ctx);

    /* Dict runtime */
    sexp_load_module_file(ctx, "eval/dict.scm", env);
    env = sexp_context_env(ctx);

    /* Async/await runtime */
    sexp_load_module_file(ctx, "eval/async.scm", env);
    env = sexp_context_env(ctx);

    /* Generator runtime */
    sexp_load_module_file(ctx, "eval/generator.scm", env);
    env = sexp_context_env(ctx);

    /* Logic programming runtime */
    sexp_load_module_file(ctx, "eval/logic.scm", env);
    env = sexp_context_env(ctx);

    /* Amb: nondeterministic choice with backtracking */
    sexp_load_module_file(ctx, "eval/amb.scm", env);
    env = sexp_context_env(ctx);

    /* OO string methods: "hello"->upper(), etc. */
    sexp_load_module_file(ctx, "eval/string-oo.scm", env);
    env = sexp_context_env(ctx);

    /* OO list/vector methods: [1,2,3]->map(...), #[1,2]->length, etc. */
    sexp_load_module_file(ctx, "eval/collection-oo.scm", env);
    env = sexp_context_env(ctx);

    /* Automatic differentiation: operator overloading, grad, math, SGD */
    sexp_load_module_file(ctx, "eval/ad.scm", env);
    env = sexp_context_env(ctx);

    /* OO DateTime/Date/TimeDelta methods */
    sexp_load_module_file(ctx, "eval/datetime-oo.scm", env);
    env = sexp_context_env(ctx);

    /* OO Decimal methods */
    sexp_load_module_file(ctx, "eval/decimal-oo.scm", env);
    env = sexp_context_env(ctx);

    /* Quantity type: dimensional analysis, unit conversion (replaces Money) */
    sexp_load_module_file(ctx, "eval/quantity-oo.scm", env);
    env = sexp_context_env(ctx);

    /* SI + currency unit definitions */
    sexp_load_module_file(ctx, "eval/units-defs.scm", env);
    env = sexp_context_env(ctx);

    /* Grammar/Parser OO wrappers */
    sexp_load_module_file(ctx, "eval/grammar-rt.scm", env);
    env = sexp_context_env(ctx);

    /* Cap'n Proto OO wrappers */
#ifdef EVAL_HAVE_CAPNP
    sexp_load_module_file(ctx, "eval/capnp-rt.scm", env);
    env = sexp_context_env(ctx);
#endif

    /* Category theory: Maybe, Either, Validation, Writer, Reader, State, Monoid, Lenses */
    sexp_load_module_file(ctx, "eval/monad.scm", env);
    env = sexp_context_env(ctx);

    /* Abstract class support: global flag checked by abstract constructors */
    sexp_eval_string(ctx, "(define __abstract_ok__ #f)", -1, env);

    /* Green-thread-aware channel-recv: retries with thread-yield! like
     * mutex-lock!.  Must come after threads.scm which defines thread-yield!.
     * Overrides the opcode binding so user code (and deserialized thunks)
     * gets the cooperative version. */
    sexp_eval_string(ctx,
        "(define (channel-recv ch)"
        "  (let ((r (%channel-recv ch)))"
        "    (cond"
        "      ((eof-object? r) r)"
        "      (r r)"
        "      (else (thread-yield!)"
        "            (channel-recv ch)))))", -1, env);
    sexp_eval_string(ctx,
        "(define channel_recv channel-recv)", -1, env);

}

/* Load an embedded .eval file: look up by path, parse as Eval, evaluate */
static sexp eval_parse_and_run(sexp ctx, sexp env, const char *code);
void eval_load_eval_file(sexp ctx, sexp env, const char *path);

/* OO wrappers for Pool, Channel, Future — only for main context
 * (workers don't have make-pool and use different pool-submit arity) */
#ifdef EVAL_STANDALONE
void eval_oo_wrappers(sexp ctx, sexp env) {
#else
static void eval_oo_wrappers(sexp ctx, sexp env) {
#endif
    eval_load_eval_file(ctx, env, "eval/threadpool.eval");
}

/* ================================================================
 * Reactive runtime: Signal, Computed, Effect, batch, dispose
 * Now loaded from eval/reactive.scm embedded file.
 * ================================================================ */

void eval_reactive_runtime(sexp ctx, sexp env) {
    sexp_load_module_file(ctx, "eval/reactive.scm", env);
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

/* Load an embedded .eval file: look up by path, parse as Eval, evaluate */
void eval_load_eval_file(sexp ctx, sexp env, const char *path) {
    const char *code = embedded_find_scm(path);
    if (!code) {
        fprintf(stderr, "eval_load_eval_file: not found: %s\n", path);
        return;
    }
    sexp result = eval_parse_and_run(ctx, env, code);
    if (sexp_exceptionp(result)) {
        sexp_print_exception(ctx, result,
            sexp_env_ref(ctx, env, sexp_intern(ctx, "current-error-port", -1),
                         SEXP_FALSE));
    }
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
    {
        extern void register_concurrent_types(sexp ctx);
        register_concurrent_types(ctx);
    }
    {
        extern void register_datetime_types(sexp ctx);
        register_datetime_types(ctx);
    }
    {
        extern void register_decimal_type(sexp ctx, sexp env);
        register_decimal_type(ctx, env);
    }
    {
        extern void register_quantity_type(sexp ctx, sexp env);
        register_quantity_type(ctx, env);
    }
    {
        extern void register_ad_types(sexp ctx);
        register_ad_types(ctx);
    }

    /* 4. Register C-only bridge functions */
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
    {
        extern void register_concurrent_bridge_functions(sexp ctx, sexp env);
        register_concurrent_bridge_functions(ctx, env);
    }
    {
        extern void register_rete_bridge_functions(sexp ctx, sexp env);
        register_rete_bridge_functions(ctx, env);
    }
    {
        extern void register_datetime_bridge_functions(sexp ctx, sexp env);
        register_datetime_bridge_functions(ctx, env);
    }
    {
        extern void register_decimal_bridge_functions(sexp ctx, sexp env);
        register_decimal_bridge_functions(ctx, env);
    }
    {
        extern void register_quantity_bridge_functions(sexp ctx, sexp env);
        register_quantity_bridge_functions(ctx, env);
    }
    {
        extern void register_ad_bridge_functions(sexp ctx, sexp env);
        register_ad_bridge_functions(ctx, env);
    }

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
    env = sexp_context_env(ctx);

    /* 7b. Concurrent containers (loaded as eval code from embedded data) */
    {
        extern void eval_load_eval_file(sexp ctx, sexp env, const char *path);
        eval_load_eval_file(ctx, env, "eval/concurrent.eval");
        env = sexp_context_env(ctx);

        /* Variadic constructor wrappers */
        sexp_eval_string(ctx,
            "(define __ConcurrentDict_1 ConcurrentDict)", -1, env);
        sexp_eval_string(ctx,
            "(define (ConcurrentDict . args)"
            "  (__ConcurrentDict_1 (if (pair? args) (car args) #f)))", -1, env);

        sexp_eval_string(ctx,
            "(define __ConcurrentQueue_2 ConcurrentQueue)", -1, env);
        sexp_eval_string(ctx,
            "(define (ConcurrentQueue . args)"
            "  (let ((name (if (and (pair? args) (string? (car args))) (car args) #f))"
            "        (rest (if (and (pair? args) (string? (car args))) (cdr args) args)))"
            "    (__ConcurrentQueue_2 name (if (pair? rest) (car rest) 0))))", -1, env);

        sexp_eval_string(ctx,
            "(define __ConcurrentStack_1 ConcurrentStack)", -1, env);
        sexp_eval_string(ctx,
            "(define (ConcurrentStack . args)"
            "  (__ConcurrentStack_1 (if (pair? args) (car args) #f)))", -1, env);

        sexp_eval_string(ctx,
            "(define __ConcurrentList_1 ConcurrentList)", -1, env);
        sexp_eval_string(ctx,
            "(define (ConcurrentList . args)"
            "  (__ConcurrentList_1 (if (pair? args) (car args) #f)))", -1, env);
        env = sexp_context_env(ctx);
    }

    /* 7c. Green-thread-aware channel-recv wrapper (needs thread-yield! from aliases) */
    sexp_eval_string(ctx,
        "(define (channel-recv ch)"
        "  (let ((r (%channel-recv ch)))"
        "    (cond"
        "      ((eof-object? r) r)"
        "      (r r)"
        "      (else (thread-yield!)"
        "            (channel-recv ch)))))", -1, env);
    sexp_eval_string(ctx,
        "(define channel_recv channel-recv)", -1, env);
    env = sexp_context_env(ctx);

    /* 7c. Store pool pointer as __pool__ in env */
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
