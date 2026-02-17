/*  _eval_pool.c -- C-level thread pool with channels for Eval  */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <chibi/eval.h>
#include "_eval_thread.h"
#include "_eval_pool.h"
#include "_eval_parser_helpers.h"

/* Forward declaration for pyobject_type registration (defined in _chibi_pyobject_type.c) */
extern void register_pyobject_type(sexp ctx);

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
 * Module path cache
 * ================================================================ */

static char *cached_module_path = NULL;
static char *cached_module_path2 = NULL;  /* dev install path */

void eval_set_module_path(const char *path) {
    free(cached_module_path);
    cached_module_path = path ? strdup(path) : NULL;
}

void eval_set_module_path2(const char *path) {
    free(cached_module_path2);
    cached_module_path2 = path ? strdup(path) : NULL;
}

const char *eval_get_module_path(void) {
    return cached_module_path;
}

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

/* display(x) - write to stdout */
static sexp bridge_display_c(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
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
static sexp bridge_print_c(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    bridge_display_c(ctx, self, n, x);
    fputc('\n', stdout);
    fflush(stdout);
    return SEXP_VOID;
}

/* newline() */
static sexp bridge_newline_c(sexp ctx, sexp self, sexp_sint_t n) {
    fputc('\n', stdout);
    fflush(stdout);
    return SEXP_VOID;
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
static void register_bridge_functions_c(sexp ctx, sexp env) {
    /* I/O */
    sexp_define_foreign(ctx, env, "display", 1, bridge_display_c);
    sexp_define_foreign(ctx, env, "print", 1, bridge_print_c);
    sexp_define_foreign(ctx, env, "newline", 0, bridge_newline_c);

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

    /* Evaluate Scheme string in isolated eval context (defined in _chibi_bridge.c) */
    {
        extern sexp bridge_eval_scheme(sexp, sexp, sexp_sint_t, sexp);
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
static void eval_standard_aliases(sexp ctx, sexp env) {
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

    /* 2. Set module paths, load standard env */
    {
        sexp_gc_var1(path_sexp);
        sexp_gc_preserve1(ctx, path_sexp);

        if (cached_module_path) {
            path_sexp = sexp_c_string(ctx, cached_module_path, -1);
            sexp_add_module_directory(ctx, path_sexp, SEXP_TRUE);
        }
        if (cached_module_path2) {
            path_sexp = sexp_c_string(ctx, cached_module_path2, -1);
            sexp_add_module_directory(ctx, path_sexp, SEXP_TRUE);
        }

        sexp_gc_release1(ctx);
    }

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
    if (module_path)
        eval_set_module_path(module_path);

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
