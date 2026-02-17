/*  _eval_pool.h -- C-level thread pool with channels for Eval  */

#ifndef EVAL_POOL_H
#define EVAL_POOL_H

#include <stddef.h>

#ifdef _WIN32
  #ifdef BUILDING_EVAL_POOL
    #define EVAL_POOL_API __declspec(dllexport)
  #else
    #define EVAL_POOL_API __declspec(dllimport)
  #endif
#else
  #define EVAL_POOL_API __attribute__((visibility("default")))
#endif

typedef struct EvalPool EvalPool;
typedef struct EvalChannel EvalChannel;
typedef struct EvalFuture EvalFuture;

/* Pool lifecycle */
EVAL_POOL_API EvalPool   *eval_pool_create(int num_workers, const char *module_path);
EVAL_POOL_API void        eval_pool_destroy(EvalPool *pool);
EVAL_POOL_API void        eval_pool_load_prelude(EvalPool *pool, const char *code);

/* Submit work -> get future */
EVAL_POOL_API EvalFuture *eval_pool_submit(EvalPool *pool, const char *code);
EVAL_POOL_API EvalFuture *eval_pool_apply(EvalPool *pool,
                                           const unsigned char *data, size_t len);
EVAL_POOL_API int         eval_future_wait(EvalFuture *f, unsigned char **result,
                                           size_t *result_len, char **error);
EVAL_POOL_API int         eval_future_wait_text(EvalFuture *f, char **result, char **error);
EVAL_POOL_API int         eval_future_ready(EvalFuture *f);
EVAL_POOL_API void        eval_future_free(EvalFuture *f);

/* Named channels (shared across all workers) */
EVAL_POOL_API EvalChannel *eval_pool_channel(EvalPool *pool, const char *name);

/* Channel operations */
EVAL_POOL_API void   eval_channel_send(EvalChannel *ch, const char *data, size_t len);
EVAL_POOL_API char  *eval_channel_recv(EvalChannel *ch, size_t *out_len);  /* blocks */
EVAL_POOL_API int    eval_channel_try_recv(EvalChannel *ch, char **data, size_t *out_len);
EVAL_POOL_API void   eval_channel_close(EvalChannel *ch);

/* Text-converting channel operations (auto-detect binary, for Python) */
EVAL_POOL_API char  *eval_channel_recv_text(EvalChannel *ch, size_t *out_len);
EVAL_POOL_API int    eval_channel_try_recv_text(EvalChannel *ch, char **data, size_t *out_len);

/* Module path cache (set once from Python, used by workers) */
EVAL_POOL_API void        eval_set_module_path(const char *path);
EVAL_POOL_API const char *eval_get_module_path(void);

#endif /* EVAL_POOL_H */
