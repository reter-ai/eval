/*  _eval_concurrent.c -- Thread-safe concurrent containers for Eval
 *
 *  Architecture: C bridge functions are atomic from the green thread
 *  scheduler (no yield mid-call). OS-level mutexes protect against
 *  real parallelism. Values stored as serialized strings so they can
 *  be shared across isolated worker heaps.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <chibi/eval.h>
#include "_eval_thread.h"
#include "_eval_concurrent.h"

/* ================================================================
 * Global named container registry
 * ================================================================ */

enum { REGISTRY_DICT = 0, REGISTRY_QUEUE, REGISTRY_STACK, REGISTRY_LIST };

typedef struct RegistryEntry {
    char *name;
    int type;
    void *ptr;
    struct RegistryEntry *next;
} RegistryEntry;

static eval_mutex_t g_registry_mutex;
static int g_registry_inited = 0;
static RegistryEntry *g_registry = NULL;

static void registry_ensure_init(void) {
    if (!g_registry_inited) {
        eval_mutex_init(&g_registry_mutex);
        g_registry_inited = 1;
    }
}

static void *registry_find_locked(const char *name, int type) {
    for (RegistryEntry *e = g_registry; e; e = e->next)
        if (e->type == type && strcmp(e->name, name) == 0)
            return e->ptr;
    return NULL;
}

static void registry_insert_locked(const char *name, int type, void *ptr) {
    RegistryEntry *e = (RegistryEntry *)malloc(sizeof(RegistryEntry));
    e->name = strdup(name);
    e->type = type;
    e->ptr = ptr;
    e->next = g_registry;
    g_registry = e;
}

void concurrent_registry_cleanup(void) {
    if (!g_registry_inited) return;
    eval_mutex_lock(&g_registry_mutex);
    RegistryEntry *e = g_registry;
    while (e) {
        RegistryEntry *next = e->next;
        free(e->name);
        free(e);
        e = next;
    }
    g_registry = NULL;
    eval_mutex_unlock(&g_registry_mutex);
}

/* ================================================================
 * ConcurrentDict
 * ================================================================ */

#define DICT_INIT_BUCKETS 32

typedef struct DictEntry {
    char *key;
    char *val;
    size_t vlen;
    struct DictEntry *next;
} DictEntry;

struct ConcurrentDict {
    eval_mutex_t mutex;
    DictEntry **buckets;
    int nbuckets;
    int count;
    int closed;
};

static unsigned int fnv1a(const char *key) {
    unsigned int h = 2166136261u;
    for (; *key; key++) {
        h ^= (unsigned char)*key;
        h *= 16777619u;
    }
    return h;
}

static void dict_resize(ConcurrentDict *d) {
    if (d->count < (int)(d->nbuckets * 0.75))
        return;
    int new_nb = d->nbuckets * 2;
    DictEntry **new_b = (DictEntry **)calloc(new_nb, sizeof(DictEntry *));
    for (int i = 0; i < d->nbuckets; i++) {
        DictEntry *e = d->buckets[i];
        while (e) {
            DictEntry *next = e->next;
            unsigned int h = fnv1a(e->key) % new_nb;
            e->next = new_b[h];
            new_b[h] = e;
            e = next;
        }
    }
    free(d->buckets);
    d->buckets = new_b;
    d->nbuckets = new_nb;
}

ConcurrentDict *cdict_create(const char *name) {
    registry_ensure_init();

    if (name) {
        eval_mutex_lock(&g_registry_mutex);
        void *existing = registry_find_locked(name, REGISTRY_DICT);
        if (existing) {
            eval_mutex_unlock(&g_registry_mutex);
            return (ConcurrentDict *)existing;
        }
    }

    ConcurrentDict *d = (ConcurrentDict *)calloc(1, sizeof(ConcurrentDict));
    eval_mutex_init(&d->mutex);
    d->nbuckets = DICT_INIT_BUCKETS;
    d->buckets = (DictEntry **)calloc(d->nbuckets, sizeof(DictEntry *));

    if (name) {
        registry_insert_locked(name, REGISTRY_DICT, d);
        eval_mutex_unlock(&g_registry_mutex);
    }
    return d;
}

void cdict_set(ConcurrentDict *d, const char *key, const char *val, size_t vlen) {
    eval_mutex_lock(&d->mutex);
    unsigned int h = fnv1a(key) % d->nbuckets;

    for (DictEntry *e = d->buckets[h]; e; e = e->next) {
        if (strcmp(e->key, key) == 0) {
            free(e->val);
            e->val = (char *)malloc(vlen + 1);
            memcpy(e->val, val, vlen);
            e->val[vlen] = '\0';
            e->vlen = vlen;
            eval_mutex_unlock(&d->mutex);
            return;
        }
    }

    DictEntry *e = (DictEntry *)malloc(sizeof(DictEntry));
    e->key = strdup(key);
    e->val = (char *)malloc(vlen + 1);
    memcpy(e->val, val, vlen);
    e->val[vlen] = '\0';
    e->vlen = vlen;
    e->next = d->buckets[h];
    d->buckets[h] = e;
    d->count++;
    dict_resize(d);
    eval_mutex_unlock(&d->mutex);
}

int cdict_get(ConcurrentDict *d, const char *key, char **out, size_t *olen) {
    eval_mutex_lock(&d->mutex);
    unsigned int h = fnv1a(key) % d->nbuckets;
    for (DictEntry *e = d->buckets[h]; e; e = e->next) {
        if (strcmp(e->key, key) == 0) {
            *out = (char *)malloc(e->vlen + 1);
            memcpy(*out, e->val, e->vlen);
            (*out)[e->vlen] = '\0';
            if (olen) *olen = e->vlen;
            eval_mutex_unlock(&d->mutex);
            return 1;
        }
    }
    eval_mutex_unlock(&d->mutex);
    return 0;
}

int cdict_delete(ConcurrentDict *d, const char *key) {
    eval_mutex_lock(&d->mutex);
    unsigned int h = fnv1a(key) % d->nbuckets;
    DictEntry **pp = &d->buckets[h];
    while (*pp) {
        if (strcmp((*pp)->key, key) == 0) {
            DictEntry *doomed = *pp;
            *pp = doomed->next;
            free(doomed->key);
            free(doomed->val);
            free(doomed);
            d->count--;
            eval_mutex_unlock(&d->mutex);
            return 1;
        }
        pp = &(*pp)->next;
    }
    eval_mutex_unlock(&d->mutex);
    return 0;
}

int cdict_has(ConcurrentDict *d, const char *key) {
    eval_mutex_lock(&d->mutex);
    unsigned int h = fnv1a(key) % d->nbuckets;
    for (DictEntry *e = d->buckets[h]; e; e = e->next) {
        if (strcmp(e->key, key) == 0) {
            eval_mutex_unlock(&d->mutex);
            return 1;
        }
    }
    eval_mutex_unlock(&d->mutex);
    return 0;
}

int cdict_size(ConcurrentDict *d) {
    eval_mutex_lock(&d->mutex);
    int n = d->count;
    eval_mutex_unlock(&d->mutex);
    return n;
}

int cdict_keys(ConcurrentDict *d, char ***out, int *count) {
    eval_mutex_lock(&d->mutex);
    int n = d->count;
    char **keys = (char **)malloc(n * sizeof(char *));
    int idx = 0;
    for (int i = 0; i < d->nbuckets && idx < n; i++) {
        for (DictEntry *e = d->buckets[i]; e && idx < n; e = e->next)
            keys[idx++] = strdup(e->key);
    }
    eval_mutex_unlock(&d->mutex);
    *out = keys;
    *count = idx;
    return 1;
}

int cdict_entries(ConcurrentDict *d, char ***okeys, char ***ovals,
                  size_t **olens, int *count) {
    eval_mutex_lock(&d->mutex);
    int n = d->count;
    char **keys = (char **)malloc(n * sizeof(char *));
    char **vals = (char **)malloc(n * sizeof(char *));
    size_t *lens = (size_t *)malloc(n * sizeof(size_t));
    int idx = 0;
    for (int i = 0; i < d->nbuckets && idx < n; i++) {
        for (DictEntry *e = d->buckets[i]; e && idx < n; e = e->next) {
            keys[idx] = strdup(e->key);
            vals[idx] = (char *)malloc(e->vlen + 1);
            memcpy(vals[idx], e->val, e->vlen);
            vals[idx][e->vlen] = '\0';
            lens[idx] = e->vlen;
            idx++;
        }
    }
    eval_mutex_unlock(&d->mutex);
    *okeys = keys;
    *ovals = vals;
    *olens = lens;
    *count = idx;
    return 1;
}

void cdict_clear(ConcurrentDict *d) {
    eval_mutex_lock(&d->mutex);
    for (int i = 0; i < d->nbuckets; i++) {
        DictEntry *e = d->buckets[i];
        while (e) {
            DictEntry *next = e->next;
            free(e->key);
            free(e->val);
            free(e);
            e = next;
        }
        d->buckets[i] = NULL;
    }
    d->count = 0;
    eval_mutex_unlock(&d->mutex);
}

void cdict_close(ConcurrentDict *d) {
    eval_mutex_lock(&d->mutex);
    d->closed = 1;
    eval_mutex_unlock(&d->mutex);
}

/* ================================================================
 * ConcurrentQueue
 * ================================================================ */

typedef struct QueueNode {
    char *data;
    size_t len;
    struct QueueNode *next;
} QueueNode;

struct ConcurrentQueue {
    eval_mutex_t mutex;
    eval_cond_t not_empty;
    eval_cond_t not_full;
    QueueNode *head, *tail;
    int count, capacity, closed;
};

ConcurrentQueue *cqueue_create(const char *name, int capacity) {
    registry_ensure_init();

    if (name) {
        eval_mutex_lock(&g_registry_mutex);
        void *existing = registry_find_locked(name, REGISTRY_QUEUE);
        if (existing) {
            eval_mutex_unlock(&g_registry_mutex);
            return (ConcurrentQueue *)existing;
        }
    }

    ConcurrentQueue *q = (ConcurrentQueue *)calloc(1, sizeof(ConcurrentQueue));
    eval_mutex_init(&q->mutex);
    eval_cond_init(&q->not_empty);
    eval_cond_init(&q->not_full);
    q->capacity = capacity;  /* 0 = unbounded */

    if (name) {
        registry_insert_locked(name, REGISTRY_QUEUE, q);
        eval_mutex_unlock(&g_registry_mutex);
    }
    return q;
}

/* Returns 1 if pushed, 0 if full (bounded queue) */
int cqueue_push(ConcurrentQueue *q, const char *val, size_t len) {
    eval_mutex_lock(&q->mutex);
    if (q->closed) {
        eval_mutex_unlock(&q->mutex);
        return 0;
    }
    if (q->capacity > 0 && q->count >= q->capacity) {
        eval_mutex_unlock(&q->mutex);
        return 0;
    }

    QueueNode *node = (QueueNode *)malloc(sizeof(QueueNode));
    node->data = (char *)malloc(len + 1);
    memcpy(node->data, val, len);
    node->data[len] = '\0';
    node->len = len;
    node->next = NULL;

    if (q->tail) {
        q->tail->next = node;
        q->tail = node;
    } else {
        q->head = q->tail = node;
    }
    q->count++;
    eval_cond_signal(&q->not_empty);
    eval_mutex_unlock(&q->mutex);
    return 1;
}

/* OS-blocking pop. Returns 1 if got data, 0 if closed+empty */
int cqueue_pop(ConcurrentQueue *q, char **out, size_t *olen) {
    eval_mutex_lock(&q->mutex);
    while (!q->head && !q->closed) {
        eval_cond_wait(&q->not_empty, &q->mutex);
    }
    if (!q->head) {
        eval_mutex_unlock(&q->mutex);
        return 0;
    }
    QueueNode *node = q->head;
    q->head = node->next;
    if (!q->head) q->tail = NULL;
    q->count--;
    eval_cond_signal(&q->not_full);
    eval_mutex_unlock(&q->mutex);

    *out = node->data;
    if (olen) *olen = node->len;
    free(node);
    return 1;
}

int cqueue_try_pop(ConcurrentQueue *q, char **out, size_t *olen) {
    eval_mutex_lock(&q->mutex);
    if (!q->head) {
        eval_mutex_unlock(&q->mutex);
        return 0;
    }
    QueueNode *node = q->head;
    q->head = node->next;
    if (!q->head) q->tail = NULL;
    q->count--;
    eval_cond_signal(&q->not_full);
    eval_mutex_unlock(&q->mutex);

    *out = node->data;
    if (olen) *olen = node->len;
    free(node);
    return 1;
}

int cqueue_size(ConcurrentQueue *q) {
    eval_mutex_lock(&q->mutex);
    int n = q->count;
    eval_mutex_unlock(&q->mutex);
    return n;
}

void cqueue_close(ConcurrentQueue *q) {
    eval_mutex_lock(&q->mutex);
    q->closed = 1;
    eval_cond_broadcast(&q->not_empty);
    eval_cond_broadcast(&q->not_full);
    eval_mutex_unlock(&q->mutex);
}

/* ================================================================
 * ConcurrentStack
 * ================================================================ */

typedef struct StackNode {
    char *data;
    size_t len;
    struct StackNode *next;
} StackNode;

struct ConcurrentStack {
    eval_mutex_t mutex;
    eval_cond_t not_empty;
    StackNode *top;
    int count, closed;
};

ConcurrentStack *cstack_create(const char *name) {
    registry_ensure_init();

    if (name) {
        eval_mutex_lock(&g_registry_mutex);
        void *existing = registry_find_locked(name, REGISTRY_STACK);
        if (existing) {
            eval_mutex_unlock(&g_registry_mutex);
            return (ConcurrentStack *)existing;
        }
    }

    ConcurrentStack *s = (ConcurrentStack *)calloc(1, sizeof(ConcurrentStack));
    eval_mutex_init(&s->mutex);
    eval_cond_init(&s->not_empty);

    if (name) {
        registry_insert_locked(name, REGISTRY_STACK, s);
        eval_mutex_unlock(&g_registry_mutex);
    }
    return s;
}

void cstack_push(ConcurrentStack *s, const char *val, size_t len) {
    StackNode *node = (StackNode *)malloc(sizeof(StackNode));
    node->data = (char *)malloc(len + 1);
    memcpy(node->data, val, len);
    node->data[len] = '\0';
    node->len = len;

    eval_mutex_lock(&s->mutex);
    node->next = s->top;
    s->top = node;
    s->count++;
    eval_cond_signal(&s->not_empty);
    eval_mutex_unlock(&s->mutex);
}

int cstack_pop(ConcurrentStack *s, char **out, size_t *olen) {
    eval_mutex_lock(&s->mutex);
    while (!s->top && !s->closed) {
        eval_cond_wait(&s->not_empty, &s->mutex);
    }
    if (!s->top) {
        eval_mutex_unlock(&s->mutex);
        return 0;
    }
    StackNode *node = s->top;
    s->top = node->next;
    s->count--;
    eval_mutex_unlock(&s->mutex);

    *out = node->data;
    if (olen) *olen = node->len;
    free(node);
    return 1;
}

int cstack_try_pop(ConcurrentStack *s, char **out, size_t *olen) {
    eval_mutex_lock(&s->mutex);
    if (!s->top) {
        eval_mutex_unlock(&s->mutex);
        return 0;
    }
    StackNode *node = s->top;
    s->top = node->next;
    s->count--;
    eval_mutex_unlock(&s->mutex);

    *out = node->data;
    if (olen) *olen = node->len;
    free(node);
    return 1;
}

int cstack_peek(ConcurrentStack *s, char **out, size_t *olen) {
    eval_mutex_lock(&s->mutex);
    if (!s->top) {
        eval_mutex_unlock(&s->mutex);
        return 0;
    }
    *out = (char *)malloc(s->top->len + 1);
    memcpy(*out, s->top->data, s->top->len);
    (*out)[s->top->len] = '\0';
    if (olen) *olen = s->top->len;
    eval_mutex_unlock(&s->mutex);
    return 1;
}

int cstack_size(ConcurrentStack *s) {
    eval_mutex_lock(&s->mutex);
    int n = s->count;
    eval_mutex_unlock(&s->mutex);
    return n;
}

void cstack_close(ConcurrentStack *s) {
    eval_mutex_lock(&s->mutex);
    s->closed = 1;
    eval_cond_broadcast(&s->not_empty);
    eval_mutex_unlock(&s->mutex);
}

/* ================================================================
 * ConcurrentList
 * ================================================================ */

typedef struct { char *data; size_t len; } ListItem;

struct ConcurrentList {
    eval_mutex_t mutex;
    ListItem *items;
    int count, capacity, closed;
};

#define LIST_INIT_CAP 16

ConcurrentList *clist_create(const char *name) {
    registry_ensure_init();

    if (name) {
        eval_mutex_lock(&g_registry_mutex);
        void *existing = registry_find_locked(name, REGISTRY_LIST);
        if (existing) {
            eval_mutex_unlock(&g_registry_mutex);
            return (ConcurrentList *)existing;
        }
    }

    ConcurrentList *l = (ConcurrentList *)calloc(1, sizeof(ConcurrentList));
    eval_mutex_init(&l->mutex);
    l->capacity = LIST_INIT_CAP;
    l->items = (ListItem *)calloc(l->capacity, sizeof(ListItem));

    if (name) {
        registry_insert_locked(name, REGISTRY_LIST, l);
        eval_mutex_unlock(&g_registry_mutex);
    }
    return l;
}

static void list_grow(ConcurrentList *l) {
    if (l->count < l->capacity) return;
    l->capacity *= 2;
    l->items = (ListItem *)realloc(l->items, l->capacity * sizeof(ListItem));
}

void clist_append(ConcurrentList *l, const char *val, size_t len) {
    eval_mutex_lock(&l->mutex);
    list_grow(l);
    l->items[l->count].data = (char *)malloc(len + 1);
    memcpy(l->items[l->count].data, val, len);
    l->items[l->count].data[len] = '\0';
    l->items[l->count].len = len;
    l->count++;
    eval_mutex_unlock(&l->mutex);
}

void clist_prepend(ConcurrentList *l, const char *val, size_t len) {
    eval_mutex_lock(&l->mutex);
    list_grow(l);
    memmove(&l->items[1], &l->items[0], l->count * sizeof(ListItem));
    l->items[0].data = (char *)malloc(len + 1);
    memcpy(l->items[0].data, val, len);
    l->items[0].data[len] = '\0';
    l->items[0].len = len;
    l->count++;
    eval_mutex_unlock(&l->mutex);
}

int clist_ref(ConcurrentList *l, int idx, char **out, size_t *olen) {
    eval_mutex_lock(&l->mutex);
    if (idx < 0) idx += l->count;
    if (idx < 0 || idx >= l->count) {
        eval_mutex_unlock(&l->mutex);
        return 0;
    }
    *out = (char *)malloc(l->items[idx].len + 1);
    memcpy(*out, l->items[idx].data, l->items[idx].len);
    (*out)[l->items[idx].len] = '\0';
    if (olen) *olen = l->items[idx].len;
    eval_mutex_unlock(&l->mutex);
    return 1;
}

int clist_set(ConcurrentList *l, int idx, const char *val, size_t len) {
    eval_mutex_lock(&l->mutex);
    if (idx < 0) idx += l->count;
    if (idx < 0 || idx >= l->count) {
        eval_mutex_unlock(&l->mutex);
        return 0;
    }
    free(l->items[idx].data);
    l->items[idx].data = (char *)malloc(len + 1);
    memcpy(l->items[idx].data, val, len);
    l->items[idx].data[len] = '\0';
    l->items[idx].len = len;
    eval_mutex_unlock(&l->mutex);
    return 1;
}

int clist_remove(ConcurrentList *l, int idx) {
    eval_mutex_lock(&l->mutex);
    if (idx < 0) idx += l->count;
    if (idx < 0 || idx >= l->count) {
        eval_mutex_unlock(&l->mutex);
        return 0;
    }
    free(l->items[idx].data);
    memmove(&l->items[idx], &l->items[idx + 1],
            (l->count - idx - 1) * sizeof(ListItem));
    l->count--;
    eval_mutex_unlock(&l->mutex);
    return 1;
}

int clist_size(ConcurrentList *l) {
    eval_mutex_lock(&l->mutex);
    int n = l->count;
    eval_mutex_unlock(&l->mutex);
    return n;
}

int clist_to_array(ConcurrentList *l, char ***out, size_t **olens, int *count) {
    eval_mutex_lock(&l->mutex);
    int n = l->count;
    char **arr = (char **)malloc(n * sizeof(char *));
    size_t *lens = (size_t *)malloc(n * sizeof(size_t));
    for (int i = 0; i < n; i++) {
        arr[i] = (char *)malloc(l->items[i].len + 1);
        memcpy(arr[i], l->items[i].data, l->items[i].len);
        arr[i][l->items[i].len] = '\0';
        lens[i] = l->items[i].len;
    }
    eval_mutex_unlock(&l->mutex);
    *out = arr;
    *olens = lens;
    *count = n;
    return 1;
}

void clist_clear(ConcurrentList *l) {
    eval_mutex_lock(&l->mutex);
    for (int i = 0; i < l->count; i++)
        free(l->items[i].data);
    l->count = 0;
    eval_mutex_unlock(&l->mutex);
}

void clist_close(ConcurrentList *l) {
    eval_mutex_lock(&l->mutex);
    l->closed = 1;
    eval_mutex_unlock(&l->mutex);
}

/* ================================================================
 * Chibi type registration
 * ================================================================ */

static sexp_uint_t cdict_type_tag = 0;
static sexp_uint_t cqueue_type_tag = 0;
static sexp_uint_t cstack_type_tag = 0;
static sexp_uint_t clist_type_tag = 0;

static sexp cdict_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    return SEXP_VOID;
}
static sexp cqueue_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    return SEXP_VOID;
}
static sexp cstack_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    return SEXP_VOID;
}
static sexp clist_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    return SEXP_VOID;
}

void register_concurrent_types(sexp ctx) {
    sexp_gc_var2(name, type);
    sexp_gc_preserve2(ctx, name, type);

    name = sexp_c_string(ctx, "concurrent-dict", -1);
    type = sexp_register_c_type(ctx, name, cdict_finalize);
    if (sexp_typep(type)) {
        cdict_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }

    name = sexp_c_string(ctx, "concurrent-queue", -1);
    type = sexp_register_c_type(ctx, name, cqueue_finalize);
    if (sexp_typep(type)) {
        cqueue_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }

    name = sexp_c_string(ctx, "concurrent-stack", -1);
    type = sexp_register_c_type(ctx, name, cstack_finalize);
    if (sexp_typep(type)) {
        cstack_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }

    name = sexp_c_string(ctx, "concurrent-list", -1);
    type = sexp_register_c_type(ctx, name, clist_finalize);
    if (sexp_typep(type)) {
        clist_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }

    sexp_gc_release2(ctx);
}

/* Wrap / unwrap helpers */

static sexp wrap_cdict(sexp ctx, ConcurrentDict *d) {
    sexp_gc_var1(r);
    sexp_gc_preserve1(ctx, r);
    r = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), cdict_type_tag);
    sexp_cpointer_value(r) = (void *)d;
    sexp_cpointer_length(r) = 0;
    sexp_gc_release1(ctx);
    return r;
}

static ConcurrentDict *unwrap_cdict(sexp x) {
    if (sexp_pointerp(x) && sexp_pointer_tag(x) == cdict_type_tag)
        return (ConcurrentDict *)sexp_cpointer_value(x);
    return NULL;
}

static sexp wrap_cqueue(sexp ctx, ConcurrentQueue *q) {
    sexp_gc_var1(r);
    sexp_gc_preserve1(ctx, r);
    r = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), cqueue_type_tag);
    sexp_cpointer_value(r) = (void *)q;
    sexp_cpointer_length(r) = 0;
    sexp_gc_release1(ctx);
    return r;
}

static ConcurrentQueue *unwrap_cqueue(sexp x) {
    if (sexp_pointerp(x) && sexp_pointer_tag(x) == cqueue_type_tag)
        return (ConcurrentQueue *)sexp_cpointer_value(x);
    return NULL;
}

static sexp wrap_cstack(sexp ctx, ConcurrentStack *s) {
    sexp_gc_var1(r);
    sexp_gc_preserve1(ctx, r);
    r = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), cstack_type_tag);
    sexp_cpointer_value(r) = (void *)s;
    sexp_cpointer_length(r) = 0;
    sexp_gc_release1(ctx);
    return r;
}

static ConcurrentStack *unwrap_cstack(sexp x) {
    if (sexp_pointerp(x) && sexp_pointer_tag(x) == cstack_type_tag)
        return (ConcurrentStack *)sexp_cpointer_value(x);
    return NULL;
}

static sexp wrap_clist(sexp ctx, ConcurrentList *l) {
    sexp_gc_var1(r);
    sexp_gc_preserve1(ctx, r);
    r = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), clist_type_tag);
    sexp_cpointer_value(r) = (void *)l;
    sexp_cpointer_length(r) = 0;
    sexp_gc_release1(ctx);
    return r;
}

static ConcurrentList *unwrap_clist(sexp x) {
    if (sexp_pointerp(x) && sexp_pointer_tag(x) == clist_type_tag)
        return (ConcurrentList *)sexp_cpointer_value(x);
    return NULL;
}

/* ================================================================
 * Serialization helpers
 * ================================================================ */

/* Serialize sexp value to malloc'd string. Returns 1 on success. */
static int ser_value(sexp ctx, sexp val, char **out, size_t *olen) {
    sexp_gc_var1(str);
    sexp_gc_preserve1(ctx, str);
    str = sexp_write_to_string(ctx, val);
    if (!sexp_stringp(str)) {
        sexp_gc_release1(ctx);
        return 0;
    }
    size_t len = sexp_string_size(str);
    *out = (char *)malloc(len + 1);
    memcpy(*out, sexp_string_data(str), len);
    (*out)[len] = '\0';
    if (olen) *olen = len;
    sexp_gc_release1(ctx);
    return 1;
}

/* Deserialize string to sexp value */
static sexp deser_value(sexp ctx, const char *data, size_t len) {
    return sexp_read_from_string(ctx, data, (sexp_sint_t)len);
}

/* ================================================================
 * Bridge functions — ConcurrentDict
 * ================================================================ */

/* %make-cdict(name-or-false) */
static sexp bridge_make_cdict(sexp ctx, sexp self, sexp_sint_t n, sexp name_s) {
    const char *name = NULL;
    if (sexp_stringp(name_s))
        name = sexp_string_data(name_s);
    ConcurrentDict *d = cdict_create(name);
    return wrap_cdict(ctx, d);
}

/* %cdict-set!(dict, key, val) */
static sexp bridge_cdict_set(sexp ctx, sexp self, sexp_sint_t n,
                              sexp dict_s, sexp key_s, sexp val_s) {
    ConcurrentDict *d = unwrap_cdict(dict_s);
    if (!d) return sexp_user_exception(ctx, self, "cdict-set!: not a dict", dict_s);
    if (!sexp_stringp(key_s))
        return sexp_user_exception(ctx, self, "cdict-set!: key must be string", key_s);

    char *vdata;
    size_t vlen;
    if (!ser_value(ctx, val_s, &vdata, &vlen))
        return sexp_user_exception(ctx, self, "cdict-set!: cannot serialize", val_s);

    cdict_set(d, sexp_string_data(key_s), vdata, vlen);
    free(vdata);
    return SEXP_VOID;
}

/* %cdict-ref(dict, key) -> (#t . value) or #f */
static sexp bridge_cdict_ref(sexp ctx, sexp self, sexp_sint_t n,
                              sexp dict_s, sexp key_s) {
    ConcurrentDict *d = unwrap_cdict(dict_s);
    if (!d) return sexp_user_exception(ctx, self, "cdict-ref: not a dict", dict_s);
    if (!sexp_stringp(key_s))
        return sexp_user_exception(ctx, self, "cdict-ref: key must be string", key_s);

    char *vdata;
    size_t vlen;
    if (!cdict_get(d, sexp_string_data(key_s), &vdata, &vlen))
        return SEXP_FALSE;

    sexp_gc_var2(val, result);
    sexp_gc_preserve2(ctx, val, result);
    val = deser_value(ctx, vdata, vlen);
    free(vdata);
    result = sexp_cons(ctx, SEXP_TRUE, val);
    sexp_gc_release2(ctx);
    return result;
}

/* %cdict-delete!(dict, key) -> #t/#f */
static sexp bridge_cdict_delete(sexp ctx, sexp self, sexp_sint_t n,
                                 sexp dict_s, sexp key_s) {
    ConcurrentDict *d = unwrap_cdict(dict_s);
    if (!d) return sexp_user_exception(ctx, self, "cdict-delete!: not a dict", dict_s);
    if (!sexp_stringp(key_s))
        return sexp_user_exception(ctx, self, "cdict-delete!: key must be string", key_s);
    return cdict_delete(d, sexp_string_data(key_s)) ? SEXP_TRUE : SEXP_FALSE;
}

/* %cdict-has?(dict, key) -> #t/#f */
static sexp bridge_cdict_has(sexp ctx, sexp self, sexp_sint_t n,
                              sexp dict_s, sexp key_s) {
    ConcurrentDict *d = unwrap_cdict(dict_s);
    if (!d) return sexp_user_exception(ctx, self, "cdict-has?: not a dict", dict_s);
    if (!sexp_stringp(key_s))
        return sexp_user_exception(ctx, self, "cdict-has?: key must be string", key_s);
    return cdict_has(d, sexp_string_data(key_s)) ? SEXP_TRUE : SEXP_FALSE;
}

/* %cdict-size(dict) -> fixnum */
static sexp bridge_cdict_size(sexp ctx, sexp self, sexp_sint_t n, sexp dict_s) {
    ConcurrentDict *d = unwrap_cdict(dict_s);
    if (!d) return sexp_user_exception(ctx, self, "cdict-size: not a dict", dict_s);
    return sexp_make_fixnum(cdict_size(d));
}

/* %cdict-keys(dict) -> list of strings */
static sexp bridge_cdict_keys(sexp ctx, sexp self, sexp_sint_t n, sexp dict_s) {
    ConcurrentDict *d = unwrap_cdict(dict_s);
    if (!d) return sexp_user_exception(ctx, self, "cdict-keys: not a dict", dict_s);

    char **keys;
    int count;
    cdict_keys(d, &keys, &count);

    sexp_gc_var2(result, s);
    sexp_gc_preserve2(ctx, result, s);
    result = SEXP_NULL;
    for (int i = count - 1; i >= 0; i--) {
        s = sexp_c_string(ctx, keys[i], -1);
        result = sexp_cons(ctx, s, result);
        free(keys[i]);
    }
    free(keys);
    sexp_gc_release2(ctx);
    return result;
}

/* %cdict-values(dict) -> list of values */
static sexp bridge_cdict_values(sexp ctx, sexp self, sexp_sint_t n, sexp dict_s) {
    ConcurrentDict *d = unwrap_cdict(dict_s);
    if (!d) return sexp_user_exception(ctx, self, "cdict-values: not a dict", dict_s);

    char **keys, **vals;
    size_t *lens;
    int count;
    cdict_entries(d, &keys, &vals, &lens, &count);

    sexp_gc_var2(result, v);
    sexp_gc_preserve2(ctx, result, v);
    result = SEXP_NULL;
    for (int i = count - 1; i >= 0; i--) {
        v = deser_value(ctx, vals[i], lens[i]);
        result = sexp_cons(ctx, v, result);
        free(keys[i]);
        free(vals[i]);
    }
    free(keys);
    free(vals);
    free(lens);
    sexp_gc_release2(ctx);
    return result;
}

/* %cdict-entries(dict) -> list of (key . value) pairs */
static sexp bridge_cdict_entries(sexp ctx, sexp self, sexp_sint_t n, sexp dict_s) {
    ConcurrentDict *d = unwrap_cdict(dict_s);
    if (!d) return sexp_user_exception(ctx, self, "cdict-entries: not a dict", dict_s);

    char **keys, **vals;
    size_t *lens;
    int count;
    cdict_entries(d, &keys, &vals, &lens, &count);

    sexp_gc_var4(result, k, v, pair);
    sexp_gc_preserve4(ctx, result, k, v, pair);
    result = SEXP_NULL;
    for (int i = count - 1; i >= 0; i--) {
        k = sexp_c_string(ctx, keys[i], -1);
        v = deser_value(ctx, vals[i], lens[i]);
        pair = sexp_cons(ctx, k, v);
        result = sexp_cons(ctx, pair, result);
        free(keys[i]);
        free(vals[i]);
    }
    free(keys);
    free(vals);
    free(lens);
    sexp_gc_release4(ctx);
    return result;
}

/* %cdict-clear!(dict) */
static sexp bridge_cdict_clear(sexp ctx, sexp self, sexp_sint_t n, sexp dict_s) {
    ConcurrentDict *d = unwrap_cdict(dict_s);
    if (!d) return sexp_user_exception(ctx, self, "cdict-clear!: not a dict", dict_s);
    cdict_clear(d);
    return SEXP_VOID;
}

/* %cdict-close(dict) */
static sexp bridge_cdict_close(sexp ctx, sexp self, sexp_sint_t n, sexp dict_s) {
    ConcurrentDict *d = unwrap_cdict(dict_s);
    if (!d) return sexp_user_exception(ctx, self, "cdict-close: not a dict", dict_s);
    cdict_close(d);
    return SEXP_VOID;
}

/* ================================================================
 * Bridge functions — ConcurrentQueue
 * ================================================================ */

/* %make-cqueue(name-or-false, capacity) */
static sexp bridge_make_cqueue(sexp ctx, sexp self, sexp_sint_t n,
                                sexp name_s, sexp cap_s) {
    const char *name = NULL;
    if (sexp_stringp(name_s))
        name = sexp_string_data(name_s);
    int cap = sexp_fixnump(cap_s) ? (int)sexp_unbox_fixnum(cap_s) : 0;
    ConcurrentQueue *q = cqueue_create(name, cap);
    return wrap_cqueue(ctx, q);
}

/* %cqueue-push!(queue, val) -> #t if pushed, #f if full
 * Green-thread-aware for bounded queues. */
static sexp bridge_cqueue_push(sexp ctx, sexp self, sexp_sint_t n,
                                sexp q_s, sexp val_s) {
    ConcurrentQueue *q = unwrap_cqueue(q_s);
    if (!q) return sexp_user_exception(ctx, self, "cqueue-push!: not a queue", q_s);

    char *vdata;
    size_t vlen;
    if (!ser_value(ctx, val_s, &vdata, &vlen))
        return sexp_user_exception(ctx, self, "cqueue-push!: cannot serialize", val_s);

    int ok = cqueue_push(q, vdata, vlen);
    free(vdata);
    return ok ? SEXP_TRUE : SEXP_FALSE;
}

/* %cqueue-try-push(queue, val) -> #t/#f (non-blocking, same as push) */
static sexp bridge_cqueue_try_push(sexp ctx, sexp self, sexp_sint_t n,
                                    sexp q_s, sexp val_s) {
    return bridge_cqueue_push(ctx, self, n, q_s, val_s);
}

/* %cqueue-pop(queue) -> (#t . value) if data, #f if empty (retry), eof if closed
 * Green-thread-aware: uses try_pop when green threads are active. */
static sexp bridge_cqueue_pop(sexp ctx, sexp self, sexp_sint_t n, sexp q_s) {
    ConcurrentQueue *q = unwrap_cqueue(q_s);
    if (!q) return sexp_user_exception(ctx, self, "cqueue-pop: not a queue", q_s);

#if SEXP_USE_GREEN_THREADS
    {
        sexp front = sexp_global(ctx, SEXP_G_THREADS_FRONT);
        sexp paused = sexp_global(ctx, SEXP_G_THREADS_PAUSED);
        if (sexp_pairp(front) || sexp_pairp(paused)) {
            char *data;
            size_t len;
            if (cqueue_try_pop(q, &data, &len)) {
                sexp_gc_var2(val, result);
                sexp_gc_preserve2(ctx, val, result);
                val = deser_value(ctx, data, len);
                free(data);
                result = sexp_cons(ctx, SEXP_TRUE, val);
                sexp_gc_release2(ctx);
                return result;
            }
            if (q->closed) return SEXP_EOF;
            return SEXP_FALSE;  /* retry with yield */
        }
    }
#endif

    /* No green threads: OS-level blocking */
    char *data;
    size_t len;
    if (cqueue_pop(q, &data, &len)) {
        sexp_gc_var2(val, result);
        sexp_gc_preserve2(ctx, val, result);
        val = deser_value(ctx, data, len);
        free(data);
        result = sexp_cons(ctx, SEXP_TRUE, val);
        sexp_gc_release2(ctx);
        return result;
    }
    return SEXP_EOF;  /* closed */
}

/* %cqueue-try-pop(queue) -> (#t . value) or #f */
static sexp bridge_cqueue_try_pop(sexp ctx, sexp self, sexp_sint_t n, sexp q_s) {
    ConcurrentQueue *q = unwrap_cqueue(q_s);
    if (!q) return sexp_user_exception(ctx, self, "cqueue-try-pop: not a queue", q_s);

    char *data;
    size_t len;
    if (!cqueue_try_pop(q, &data, &len))
        return SEXP_FALSE;

    sexp_gc_var2(val, result);
    sexp_gc_preserve2(ctx, val, result);
    val = deser_value(ctx, data, len);
    free(data);
    result = sexp_cons(ctx, SEXP_TRUE, val);
    sexp_gc_release2(ctx);
    return result;
}

/* %cqueue-size(queue) -> fixnum */
static sexp bridge_cqueue_size(sexp ctx, sexp self, sexp_sint_t n, sexp q_s) {
    ConcurrentQueue *q = unwrap_cqueue(q_s);
    if (!q) return sexp_user_exception(ctx, self, "cqueue-size: not a queue", q_s);
    return sexp_make_fixnum(cqueue_size(q));
}

/* %cqueue-close(queue) */
static sexp bridge_cqueue_close(sexp ctx, sexp self, sexp_sint_t n, sexp q_s) {
    ConcurrentQueue *q = unwrap_cqueue(q_s);
    if (!q) return sexp_user_exception(ctx, self, "cqueue-close: not a queue", q_s);
    cqueue_close(q);
    return SEXP_VOID;
}

/* ================================================================
 * Bridge functions — ConcurrentStack
 * ================================================================ */

/* %make-cstack(name-or-false) */
static sexp bridge_make_cstack(sexp ctx, sexp self, sexp_sint_t n, sexp name_s) {
    const char *name = NULL;
    if (sexp_stringp(name_s))
        name = sexp_string_data(name_s);
    ConcurrentStack *s = cstack_create(name);
    return wrap_cstack(ctx, s);
}

/* %cstack-push!(stack, val) */
static sexp bridge_cstack_push(sexp ctx, sexp self, sexp_sint_t n,
                                sexp s_s, sexp val_s) {
    ConcurrentStack *s = unwrap_cstack(s_s);
    if (!s) return sexp_user_exception(ctx, self, "cstack-push!: not a stack", s_s);

    char *vdata;
    size_t vlen;
    if (!ser_value(ctx, val_s, &vdata, &vlen))
        return sexp_user_exception(ctx, self, "cstack-push!: cannot serialize", val_s);

    cstack_push(s, vdata, vlen);
    free(vdata);
    return SEXP_VOID;
}

/* %cstack-pop(stack) -> (#t . value) if data, #f if empty (retry), eof if closed */
static sexp bridge_cstack_pop(sexp ctx, sexp self, sexp_sint_t n, sexp s_s) {
    ConcurrentStack *s = unwrap_cstack(s_s);
    if (!s) return sexp_user_exception(ctx, self, "cstack-pop: not a stack", s_s);

#if SEXP_USE_GREEN_THREADS
    {
        sexp front = sexp_global(ctx, SEXP_G_THREADS_FRONT);
        sexp paused = sexp_global(ctx, SEXP_G_THREADS_PAUSED);
        if (sexp_pairp(front) || sexp_pairp(paused)) {
            char *data;
            size_t len;
            if (cstack_try_pop(s, &data, &len)) {
                sexp_gc_var2(val, result);
                sexp_gc_preserve2(ctx, val, result);
                val = deser_value(ctx, data, len);
                free(data);
                result = sexp_cons(ctx, SEXP_TRUE, val);
                sexp_gc_release2(ctx);
                return result;
            }
            if (s->closed) return SEXP_EOF;
            return SEXP_FALSE;
        }
    }
#endif

    char *data;
    size_t len;
    if (cstack_pop(s, &data, &len)) {
        sexp_gc_var2(val, result);
        sexp_gc_preserve2(ctx, val, result);
        val = deser_value(ctx, data, len);
        free(data);
        result = sexp_cons(ctx, SEXP_TRUE, val);
        sexp_gc_release2(ctx);
        return result;
    }
    return SEXP_EOF;
}

/* %cstack-try-pop(stack) -> (#t . value) or #f */
static sexp bridge_cstack_try_pop(sexp ctx, sexp self, sexp_sint_t n, sexp s_s) {
    ConcurrentStack *s = unwrap_cstack(s_s);
    if (!s) return sexp_user_exception(ctx, self, "cstack-try-pop: not a stack", s_s);

    char *data;
    size_t len;
    if (!cstack_try_pop(s, &data, &len))
        return SEXP_FALSE;

    sexp_gc_var2(val, result);
    sexp_gc_preserve2(ctx, val, result);
    val = deser_value(ctx, data, len);
    free(data);
    result = sexp_cons(ctx, SEXP_TRUE, val);
    sexp_gc_release2(ctx);
    return result;
}

/* %cstack-peek(stack) -> (#t . value) or #f */
static sexp bridge_cstack_peek(sexp ctx, sexp self, sexp_sint_t n, sexp s_s) {
    ConcurrentStack *s = unwrap_cstack(s_s);
    if (!s) return sexp_user_exception(ctx, self, "cstack-peek: not a stack", s_s);

    char *data;
    size_t len;
    if (!cstack_peek(s, &data, &len))
        return SEXP_FALSE;

    sexp_gc_var2(val, result);
    sexp_gc_preserve2(ctx, val, result);
    val = deser_value(ctx, data, len);
    free(data);
    result = sexp_cons(ctx, SEXP_TRUE, val);
    sexp_gc_release2(ctx);
    return result;
}

/* %cstack-size(stack) -> fixnum */
static sexp bridge_cstack_size(sexp ctx, sexp self, sexp_sint_t n, sexp s_s) {
    ConcurrentStack *s = unwrap_cstack(s_s);
    if (!s) return sexp_user_exception(ctx, self, "cstack-size: not a stack", s_s);
    return sexp_make_fixnum(cstack_size(s));
}

/* %cstack-close(stack) */
static sexp bridge_cstack_close(sexp ctx, sexp self, sexp_sint_t n, sexp s_s) {
    ConcurrentStack *s = unwrap_cstack(s_s);
    if (!s) return sexp_user_exception(ctx, self, "cstack-close: not a stack", s_s);
    cstack_close(s);
    return SEXP_VOID;
}

/* ================================================================
 * Bridge functions — ConcurrentList
 * ================================================================ */

/* %make-clist(name-or-false) */
static sexp bridge_make_clist(sexp ctx, sexp self, sexp_sint_t n, sexp name_s) {
    const char *name = NULL;
    if (sexp_stringp(name_s))
        name = sexp_string_data(name_s);
    ConcurrentList *l = clist_create(name);
    return wrap_clist(ctx, l);
}

/* %clist-append!(list, val) */
static sexp bridge_clist_append(sexp ctx, sexp self, sexp_sint_t n,
                                 sexp l_s, sexp val_s) {
    ConcurrentList *l = unwrap_clist(l_s);
    if (!l) return sexp_user_exception(ctx, self, "clist-append!: not a list", l_s);

    char *vdata;
    size_t vlen;
    if (!ser_value(ctx, val_s, &vdata, &vlen))
        return sexp_user_exception(ctx, self, "clist-append!: cannot serialize", val_s);

    clist_append(l, vdata, vlen);
    free(vdata);
    return SEXP_VOID;
}

/* %clist-prepend!(list, val) */
static sexp bridge_clist_prepend(sexp ctx, sexp self, sexp_sint_t n,
                                  sexp l_s, sexp val_s) {
    ConcurrentList *l = unwrap_clist(l_s);
    if (!l) return sexp_user_exception(ctx, self, "clist-prepend!: not a list", l_s);

    char *vdata;
    size_t vlen;
    if (!ser_value(ctx, val_s, &vdata, &vlen))
        return sexp_user_exception(ctx, self, "clist-prepend!: cannot serialize", val_s);

    clist_prepend(l, vdata, vlen);
    free(vdata);
    return SEXP_VOID;
}

/* %clist-ref(list, idx) -> value (error if out of bounds) */
static sexp bridge_clist_ref(sexp ctx, sexp self, sexp_sint_t n,
                              sexp l_s, sexp idx_s) {
    ConcurrentList *l = unwrap_clist(l_s);
    if (!l) return sexp_user_exception(ctx, self, "clist-ref: not a list", l_s);
    if (!sexp_fixnump(idx_s))
        return sexp_user_exception(ctx, self, "clist-ref: index must be integer", idx_s);

    char *data;
    size_t len;
    if (!clist_ref(l, (int)sexp_unbox_fixnum(idx_s), &data, &len))
        return sexp_user_exception(ctx, self, "clist-ref: index out of range", idx_s);

    sexp_gc_var1(val);
    sexp_gc_preserve1(ctx, val);
    val = deser_value(ctx, data, len);
    free(data);
    sexp_gc_release1(ctx);
    return val;
}

/* %clist-set!(list, idx, val) */
static sexp bridge_clist_set(sexp ctx, sexp self, sexp_sint_t n,
                              sexp l_s, sexp idx_s, sexp val_s) {
    ConcurrentList *l = unwrap_clist(l_s);
    if (!l) return sexp_user_exception(ctx, self, "clist-set!: not a list", l_s);
    if (!sexp_fixnump(idx_s))
        return sexp_user_exception(ctx, self, "clist-set!: index must be integer", idx_s);

    char *vdata;
    size_t vlen;
    if (!ser_value(ctx, val_s, &vdata, &vlen))
        return sexp_user_exception(ctx, self, "clist-set!: cannot serialize", val_s);

    int ok = clist_set(l, (int)sexp_unbox_fixnum(idx_s), vdata, vlen);
    free(vdata);
    if (!ok)
        return sexp_user_exception(ctx, self, "clist-set!: index out of range", idx_s);
    return SEXP_VOID;
}

/* %clist-remove!(list, idx) */
static sexp bridge_clist_remove(sexp ctx, sexp self, sexp_sint_t n,
                                 sexp l_s, sexp idx_s) {
    ConcurrentList *l = unwrap_clist(l_s);
    if (!l) return sexp_user_exception(ctx, self, "clist-remove!: not a list", l_s);
    if (!sexp_fixnump(idx_s))
        return sexp_user_exception(ctx, self, "clist-remove!: index must be integer", idx_s);

    if (!clist_remove(l, (int)sexp_unbox_fixnum(idx_s)))
        return sexp_user_exception(ctx, self, "clist-remove!: index out of range", idx_s);
    return SEXP_VOID;
}

/* %clist-size(list) -> fixnum */
static sexp bridge_clist_size(sexp ctx, sexp self, sexp_sint_t n, sexp l_s) {
    ConcurrentList *l = unwrap_clist(l_s);
    if (!l) return sexp_user_exception(ctx, self, "clist-size: not a list", l_s);
    return sexp_make_fixnum(clist_size(l));
}

/* %clist-to-list(list) -> scheme list snapshot */
static sexp bridge_clist_to_list(sexp ctx, sexp self, sexp_sint_t n, sexp l_s) {
    ConcurrentList *l = unwrap_clist(l_s);
    if (!l) return sexp_user_exception(ctx, self, "clist-to-list: not a list", l_s);

    char **arr;
    size_t *lens;
    int count;
    clist_to_array(l, &arr, &lens, &count);

    sexp_gc_var2(result, v);
    sexp_gc_preserve2(ctx, result, v);
    result = SEXP_NULL;
    for (int i = count - 1; i >= 0; i--) {
        v = deser_value(ctx, arr[i], lens[i]);
        result = sexp_cons(ctx, v, result);
        free(arr[i]);
    }
    free(arr);
    free(lens);
    sexp_gc_release2(ctx);
    return result;
}

/* %clist-clear!(list) */
static sexp bridge_clist_clear(sexp ctx, sexp self, sexp_sint_t n, sexp l_s) {
    ConcurrentList *l = unwrap_clist(l_s);
    if (!l) return sexp_user_exception(ctx, self, "clist-clear!: not a list", l_s);
    clist_clear(l);
    return SEXP_VOID;
}

/* %clist-close(list) */
static sexp bridge_clist_close(sexp ctx, sexp self, sexp_sint_t n, sexp l_s) {
    ConcurrentList *l = unwrap_clist(l_s);
    if (!l) return sexp_user_exception(ctx, self, "clist-close: not a list", l_s);
    clist_close(l);
    return SEXP_VOID;
}

/* ================================================================
 * Registration
 * ================================================================ */

void register_concurrent_bridge_functions(sexp ctx, sexp env) {
    /* Dict */
    sexp_define_foreign(ctx, env, "%make-cdict", 1, bridge_make_cdict);
    sexp_define_foreign(ctx, env, "%cdict-set!", 3, bridge_cdict_set);
    sexp_define_foreign(ctx, env, "%cdict-ref", 2, bridge_cdict_ref);
    sexp_define_foreign(ctx, env, "%cdict-delete!", 2, bridge_cdict_delete);
    sexp_define_foreign(ctx, env, "%cdict-has?", 2, bridge_cdict_has);
    sexp_define_foreign(ctx, env, "%cdict-size", 1, bridge_cdict_size);
    sexp_define_foreign(ctx, env, "%cdict-keys", 1, bridge_cdict_keys);
    sexp_define_foreign(ctx, env, "%cdict-values", 1, bridge_cdict_values);
    sexp_define_foreign(ctx, env, "%cdict-entries", 1, bridge_cdict_entries);
    sexp_define_foreign(ctx, env, "%cdict-clear!", 1, bridge_cdict_clear);
    sexp_define_foreign(ctx, env, "%cdict-close", 1, bridge_cdict_close);

    /* Queue */
    sexp_define_foreign(ctx, env, "%make-cqueue", 2, bridge_make_cqueue);
    sexp_define_foreign(ctx, env, "%cqueue-push!", 2, bridge_cqueue_push);
    sexp_define_foreign(ctx, env, "%cqueue-try-push", 2, bridge_cqueue_try_push);
    sexp_define_foreign(ctx, env, "%cqueue-pop", 1, bridge_cqueue_pop);
    sexp_define_foreign(ctx, env, "%cqueue-try-pop", 1, bridge_cqueue_try_pop);
    sexp_define_foreign(ctx, env, "%cqueue-size", 1, bridge_cqueue_size);
    sexp_define_foreign(ctx, env, "%cqueue-close", 1, bridge_cqueue_close);

    /* Stack */
    sexp_define_foreign(ctx, env, "%make-cstack", 1, bridge_make_cstack);
    sexp_define_foreign(ctx, env, "%cstack-push!", 2, bridge_cstack_push);
    sexp_define_foreign(ctx, env, "%cstack-pop", 1, bridge_cstack_pop);
    sexp_define_foreign(ctx, env, "%cstack-try-pop", 1, bridge_cstack_try_pop);
    sexp_define_foreign(ctx, env, "%cstack-peek", 1, bridge_cstack_peek);
    sexp_define_foreign(ctx, env, "%cstack-size", 1, bridge_cstack_size);
    sexp_define_foreign(ctx, env, "%cstack-close", 1, bridge_cstack_close);

    /* List */
    sexp_define_foreign(ctx, env, "%make-clist", 1, bridge_make_clist);
    sexp_define_foreign(ctx, env, "%clist-append!", 2, bridge_clist_append);
    sexp_define_foreign(ctx, env, "%clist-prepend!", 2, bridge_clist_prepend);
    sexp_define_foreign(ctx, env, "%clist-ref", 2, bridge_clist_ref);
    sexp_define_foreign(ctx, env, "%clist-set!", 3, bridge_clist_set);
    sexp_define_foreign(ctx, env, "%clist-remove!", 2, bridge_clist_remove);
    sexp_define_foreign(ctx, env, "%clist-size", 1, bridge_clist_size);
    sexp_define_foreign(ctx, env, "%clist-to-list", 1, bridge_clist_to_list);
    sexp_define_foreign(ctx, env, "%clist-clear!", 1, bridge_clist_clear);
    sexp_define_foreign(ctx, env, "%clist-close", 1, bridge_clist_close);
}
