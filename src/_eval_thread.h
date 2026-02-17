/*  _eval_thread.h -- Cross-platform threading primitives  */

#ifndef EVAL_THREAD_H
#define EVAL_THREAD_H

#ifdef _WIN32

#include <windows.h>
#include <process.h>

typedef CRITICAL_SECTION eval_mutex_t;
typedef CONDITION_VARIABLE eval_cond_t;
typedef HANDLE eval_thread_t;

#define eval_mutex_init(m)     InitializeCriticalSection(m)
#define eval_mutex_destroy(m)  DeleteCriticalSection(m)
#define eval_mutex_lock(m)     EnterCriticalSection(m)
#define eval_mutex_unlock(m)   LeaveCriticalSection(m)

#define eval_cond_init(c)      InitializeConditionVariable(c)
#define eval_cond_destroy(c)   ((void)0)
#define eval_cond_wait(c, m)   SleepConditionVariableCS(c, m, INFINITE)
#define eval_cond_signal(c)    WakeConditionVariable(c)
#define eval_cond_broadcast(c) WakeAllConditionVariable(c)

typedef unsigned (__stdcall *eval_thread_func_t)(void *);

static inline int eval_thread_create(eval_thread_t *t, eval_thread_func_t func, void *arg) {
    *t = (HANDLE)_beginthreadex(NULL, 0, func, arg, 0, NULL);
    return (*t == 0) ? -1 : 0;
}

static inline void eval_thread_join(eval_thread_t t) {
    WaitForSingleObject(t, INFINITE);
    CloseHandle(t);
}

#define EVAL_THREAD_FUNC  unsigned __stdcall
#define EVAL_THREAD_RETURN  return 0

#else /* POSIX */

#include <pthread.h>

typedef pthread_mutex_t eval_mutex_t;
typedef pthread_cond_t eval_cond_t;
typedef pthread_t eval_thread_t;

#define eval_mutex_init(m)     pthread_mutex_init(m, NULL)
#define eval_mutex_destroy(m)  pthread_mutex_destroy(m)
#define eval_mutex_lock(m)     pthread_mutex_lock(m)
#define eval_mutex_unlock(m)   pthread_mutex_unlock(m)

#define eval_cond_init(c)      pthread_cond_init(c, NULL)
#define eval_cond_destroy(c)   pthread_cond_destroy(c)
#define eval_cond_wait(c, m)   pthread_cond_wait(c, m)
#define eval_cond_signal(c)    pthread_cond_signal(c)
#define eval_cond_broadcast(c) pthread_cond_broadcast(c)

typedef void *(*eval_thread_func_t)(void *);

static inline int eval_thread_create(eval_thread_t *t, eval_thread_func_t func, void *arg) {
    return pthread_create(t, NULL, func, arg);
}

static inline void eval_thread_join(eval_thread_t t) {
    pthread_join(t, NULL);
}

#define EVAL_THREAD_FUNC  void *
#define EVAL_THREAD_RETURN  return NULL

#endif /* _WIN32 */

#endif /* EVAL_THREAD_H */
