/*  _eval_concurrent.h -- Thread-safe concurrent containers for Eval
 *
 *  ConcurrentDict, ConcurrentQueue, ConcurrentStack, ConcurrentList
 *  All containers use OS-level mutexes for thread safety.
 *  Values are serialized (sexp_write_to_string / sexp_read_from_string)
 *  so they can be shared across isolated worker heaps.
 *  Named containers use a global registry for cross-worker sharing.
 */

#ifndef EVAL_CONCURRENT_H
#define EVAL_CONCURRENT_H

#include <stddef.h>
#include <chibi/eval.h>

typedef struct ConcurrentDict  ConcurrentDict;
typedef struct ConcurrentQueue ConcurrentQueue;
typedef struct ConcurrentStack ConcurrentStack;
typedef struct ConcurrentList  ConcurrentList;

/* --- Dict --- */
ConcurrentDict *cdict_create(const char *name);  /* NULL = anonymous */
void cdict_set(ConcurrentDict *d, const char *key, const char *val, size_t vlen);
int  cdict_get(ConcurrentDict *d, const char *key, char **out, size_t *olen);
int  cdict_delete(ConcurrentDict *d, const char *key);
int  cdict_has(ConcurrentDict *d, const char *key);
int  cdict_size(ConcurrentDict *d);
int  cdict_keys(ConcurrentDict *d, char ***out, int *count);
int  cdict_entries(ConcurrentDict *d, char ***okeys, char ***ovals,
                   size_t **olens, int *count);
void cdict_clear(ConcurrentDict *d);
void cdict_close(ConcurrentDict *d);

/* --- Queue --- */
ConcurrentQueue *cqueue_create(const char *name, int capacity);
int  cqueue_push(ConcurrentQueue *q, const char *val, size_t len);
int  cqueue_pop(ConcurrentQueue *q, char **out, size_t *olen);
int  cqueue_try_pop(ConcurrentQueue *q, char **out, size_t *olen);
int  cqueue_size(ConcurrentQueue *q);
void cqueue_close(ConcurrentQueue *q);

/* --- Stack --- */
ConcurrentStack *cstack_create(const char *name);
void cstack_push(ConcurrentStack *s, const char *val, size_t len);
int  cstack_pop(ConcurrentStack *s, char **out, size_t *olen);
int  cstack_try_pop(ConcurrentStack *s, char **out, size_t *olen);
int  cstack_peek(ConcurrentStack *s, char **out, size_t *olen);
int  cstack_size(ConcurrentStack *s);
void cstack_close(ConcurrentStack *s);

/* --- List --- */
ConcurrentList *clist_create(const char *name);
void clist_append(ConcurrentList *l, const char *val, size_t len);
void clist_prepend(ConcurrentList *l, const char *val, size_t len);
int  clist_ref(ConcurrentList *l, int idx, char **out, size_t *olen);
int  clist_set(ConcurrentList *l, int idx, const char *val, size_t len);
int  clist_remove(ConcurrentList *l, int idx);
int  clist_size(ConcurrentList *l);
int  clist_to_array(ConcurrentList *l, char ***out, size_t **olens, int *count);
void clist_clear(ConcurrentList *l);
void clist_close(ConcurrentList *l);

/* --- Registry cleanup --- */
void concurrent_registry_cleanup(void);

/* --- Chibi bridge registration --- */
void register_concurrent_types(sexp ctx);
void register_concurrent_bridge_functions(sexp ctx, sexp env);

#endif /* EVAL_CONCURRENT_H */
