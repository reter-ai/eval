# Synchronization Primitives

Eval provides OO wrappers for common synchronization patterns: **Mutex**, **Monitor**, **ReadWriteLock**, and **Semaphore**. All support RAII via `with` — locks are released automatically on scope exit, even on error.

These build on the low-level SRFI-18 primitives (`make_mutex`, `mutex_lock`, `mutex_unlock`, `make_condvar`, `condvar_signal`, `condvar_broadcast`) described in [THREADS.md](THREADS.md).

## Quick start

```
// Mutex: automatic lock/unlock
define m = Mutex();
with(guard = m->lock()) {
    // critical section — lock released on scope exit
};

// Monitor: mutex + condition variable (wait/pulse)
define mon = Monitor();
with(guard = mon->enter()) {
    while(!ready) mon->wait();
    // consume data
};

// ReadWriteLock: multiple readers, exclusive writer
define rwl = ReadWriteLock();
with(guard = rwl->read_lock()) { /* read shared data */ };
with(guard = rwl->write_lock()) { /* write shared data */ };

// Semaphore: limit concurrency
define sem = Semaphore(3);
with(guard = sem->acquire()) {
    // at most 3 concurrent
};
```

## Mutex

`Mutex()` creates a mutual exclusion lock. Calling `lock()` acquires it and returns a guard object whose `close()` releases the lock. Use with `with` for automatic unlock:

```
define m = Mutex();

// RAII lock/unlock
with(guard = m->lock()) {
    // only one thread can be here at a time
    counter += 1;
};
// lock released here, even on error
```

### try_lock

Non-blocking acquire — returns a guard on success, `false` if the lock is held:

```
define result = m->try_lock();
if(result)
    with(guard = result) {
        // acquired
    }
else
    display("lock busy");
```

### API

| Method | Description |
|--------|-------------|
| `Mutex()` | Create a new mutex |
| `m->lock()` | Block until acquired, return RAII guard |
| `m->try_lock()` | Non-blocking — return guard or `false` |
| `m->unlock()` | Manual unlock (prefer RAII) |
| `m->__raw__` | Underlying SRFI-18 mutex |
| `m->__type__` | `'__mutex__` |

### Thread-safe counter

```
define counter = 0;
define m = Mutex();

define inc = function() {
    for(let i = 0, i < 100, i++) {
        with(guard = m->lock()) {
            counter += 1;
        };
    };
};

define t1 = make_thread(inc);
define t2 = make_thread(inc);
thread_start(t1); thread_start(t2);
thread_join(t1); thread_join(t2);
counter;    // => 200
```

## Monitor

`Monitor()` combines a mutex and a condition variable into a single object, following C#/Java monitor semantics. Use it for wait/signal patterns like producer-consumer:

```
define mon = Monitor();
define data = false;
define ready = false;

// Consumer thread
define consumer = make_thread(function() {
    with(guard = mon->enter()) {
        while(!ready) mon->wait();
        display(data);
    };
});

// Producer thread
define producer = make_thread(function() {
    with(guard = mon->enter()) {
        data = "hello";
        ready = true;
        mon->pulse();
    };
});

thread_start(consumer);
thread_start(producer);
thread_join(consumer);    // prints "hello"
thread_join(producer);
```

### How wait/pulse works

`mon->wait()` does two things atomically:
1. Releases the monitor lock (so other threads can enter)
2. Blocks until another thread calls `pulse()` or `pulse_all()`

After being signaled, `wait()` re-acquires the lock before returning. The caller is always inside the critical section:

```
// Consumer:
with(guard = mon->enter()) {       // acquire lock
    while(!condition)
        mon->wait();               // release lock, block, re-acquire lock
    // lock is held here — safe to read shared state
};                                 // guard->close() releases lock
```

**Always use `while(!condition)` around `wait()`**, never bare `if`. Spurious wakeups are possible, and another thread may change the condition between `pulse()` and the waiter re-acquiring the lock.

### Timed wait

`wait_timeout(seconds)` returns `true` if signaled, `false` on timeout. Either way, the lock is re-acquired:

```
with(guard = mon->enter()) {
    define signaled = mon->wait_timeout(5.0);
    if(!signaled) display("timed out");
};
```

### API

| Method | Description |
|--------|-------------|
| `Monitor()` | Create a new monitor |
| `mon->enter()` | Lock the monitor, return RAII guard |
| `mon->exit()` | Manual unlock (prefer RAII) |
| `mon->wait()` | Release lock, wait for signal, re-acquire lock |
| `mon->wait_timeout(secs)` | Timed wait — returns `true` (signaled) or `false` (timeout) |
| `mon->pulse()` | Wake one waiting thread |
| `mon->pulse_all()` | Wake all waiting threads |
| `mon->__type__` | `'__monitor__` |

### Execution trace

```
Consumer:                           Producer:
  mon->enter()
    mutex_lock(m) -> guard
  ready==false, mon->wait()
    mutex_unlock(m, cv) -> BLOCKED    mon->enter()
                                        mutex_lock(m) -> guard
                                      data = "hello"
                                      ready = true
                                      mon->pulse()
                                        condvar_signal(cv) -> wakes consumer
    [consumer wakes]                  guard->close()
    mutex_lock(m) -> BLOCKED            mutex_unlock(m) -> unblocks consumer
    mutex_lock(m) -> acquired
  ready==true, display(data)
  guard->close()
    mutex_unlock(m)
```

## ReadWriteLock

`ReadWriteLock()` allows multiple concurrent readers or one exclusive writer. Writer preference prevents writer starvation:

```
define rwl = ReadWriteLock();

// Multiple readers can hold read_lock simultaneously
with(guard = rwl->read_lock()) {
    // read shared data
};

// Only one writer at a time, blocks until all readers release
with(guard = rwl->write_lock()) {
    // write shared data
};
```

### Writer preference

When a writer is waiting, new readers are blocked until the writer finishes. This prevents writer starvation in read-heavy workloads:

```
// Thread 1: holds read lock
// Thread 2: waiting for write lock  <-- blocks new readers
// Thread 3: tries read_lock()       <-- blocked (writer waiting)
// Thread 1 releases read lock
// Thread 2 gets write lock           <-- writer goes first
// Thread 2 releases write lock
// Thread 3 gets read lock            <-- readers proceed
```

### API

| Method | Description |
|--------|-------------|
| `ReadWriteLock()` | Create a new read-write lock |
| `rwl->read_lock()` | Acquire read lock, return RAII guard |
| `rwl->write_lock()` | Acquire write lock (exclusive), return RAII guard |
| `rwl->__type__` | `'__rw_lock__` |

### Concurrent readers, exclusive writer

```
define rwl = ReadWriteLock();
define data = 0;

// Multiple readers run concurrently
define reader = function(id) {
    with(guard = rwl->read_lock()) {
        define val = data;
        // ... process val ...
    };
};

// Writer gets exclusive access
define writer = function() {
    with(guard = rwl->write_lock()) {
        data = data + 1;
    };
};

define threads = [];
for(let i = 0, i < 5, i++) {
    define t = make_thread(reader);
    threads = cons(t, threads);
    thread_start(t);
};
define tw = make_thread(writer);
thread_start(tw);
`for-each`(thread_join, threads);
thread_join(tw);
```

## Semaphore

`Semaphore(n)` creates a counting semaphore that allows at most `n` concurrent holders:

```
// Allow at most 3 concurrent operations
define sem = Semaphore(3);

define worker = function(id) {
    with(guard = sem->acquire()) {
        // at most 3 workers here simultaneously
        display(id);
        thread_yield();
    };
};
```

### try_acquire

Non-blocking — returns a guard on success, `false` if the semaphore count is zero:

```
define result = sem->try_acquire();
if(result)
    with(guard = result) {
        // acquired a slot
    }
else
    display("no slots available");
```

### Manual release

For cases where RAII doesn't fit, `release()` increments the count manually:

```
sem->acquire();    // blocks if count is 0
// ... work ...
sem->release();    // increment count, wake a waiter
```

### API

| Method | Description |
|--------|-------------|
| `Semaphore(n)` | Create a semaphore with count `n` |
| `sem->acquire()` | Decrement count (block if 0), return RAII guard |
| `sem->try_acquire()` | Non-blocking — return guard or `false` |
| `sem->release()` | Increment count, wake one waiter |
| `sem->get_count()` | Current count |
| `sem->__type__` | `'__semaphore__` |

## RAII pattern

All four types return guard objects from their locking methods. Guards have a `close()` method that releases the lock. The `with` statement calls `close()` automatically when the block exits — whether normally or via error:

```
// Expression form — guard scoped to the with-block
with(guard = m->lock()) {
    // critical section
};

// Statement form — guard scoped to the enclosing block
{
    with(guard = m->lock());
    // everything after this is in the critical section
    do_work();
    more_work();
};
// guard->close() called here
```

This is the same `with` / RAII pattern used by `TcpSocket`, `TcpServer`, `HttpClient`, `Pool`, signals, and effects. Any object with a `close()` method works.

### Nested locks

Multiple resources close in reverse order:

```
define m1 = Mutex();
define m2 = Mutex();

with(g1 = m1->lock()) {
    with(g2 = m2->lock()) {
        // both locks held
    };
    // m2 released, m1 still held
};
// m1 released
```

Or with statement form:

```
{
    with(g1 = m1->lock());
    with(g2 = m2->lock());
    // both locks held
};
// g2->close() first, then g1->close()
```

## Patterns

### Thread-safe queue

```
define Queue = function() {
    define mon = Monitor();
    define items = [];

    interface(
        push: function(item) {
            with(guard = mon->enter()) {
                items = append(items, [item]);
                mon->pulse();
            };
        },
        pop: function() {
            with(guard = mon->enter()) {
                while(null?(items)) mon->wait();
                define item = car(items);
                items = cdr(items);
                item;
            };
        },
        size: function() length(items),
        __type__: '__queue__
    );
};

define q = Queue();

// Producer
define t1 = make_thread(function() {
    for(let i = 0, i < 5, i++) q->push(i);
});

// Consumer
define t2 = make_thread(function() {
    for(let i = 0, i < 5, i++) display(q->pop());
});

thread_start(t1);
thread_start(t2);
thread_join(t1);
thread_join(t2);
// prints: 01234
```

### Connection pool

```
define ConnectionPool = function(max_conns, factory) {
    define sem = Semaphore(max_conns);
    define mon = Monitor();
    define available = [];

    define get_conn = function() {
        with(guard = mon->enter()) {
            if(null?(available))
                factory()
            else {
                define conn = car(available);
                available = cdr(available);
                conn;
            };
        };
    };

    interface(
        acquire: function() {
            with(slot = sem->acquire()) {
                get_conn();
            };
        },
        release: function(conn) {
            with(guard = mon->enter()) {
                available = cons(conn, available);
            };
            sem->release();
        },
        __type__: '__conn_pool__
    );
};
```

### Cached value with read-write lock

```
define CachedValue = function(initial) {
    define rwl = ReadWriteLock();
    define value = initial;

    interface(
        get: function() {
            with(guard = rwl->read_lock()) {
                value;
            };
        },
        set: function(v) {
            with(guard = rwl->write_lock()) {
                value = v;
            };
        },
        update: function(f) {
            with(guard = rwl->write_lock()) {
                value = f(value);
            };
        },
        __type__: '__cached_value__
    );
};
```

## Low-level primitives

The OO wrappers are built on these SRFI-18 functions:

| Function | Description |
|----------|-------------|
| `make_mutex()` | Create a raw SRFI-18 mutex |
| `mutex_lock(m)` | Acquire mutex (blocks if held) |
| `mutex_lock(m, timeout)` | Timed acquire — `0` for non-blocking |
| `mutex_unlock(m)` | Release mutex |
| `mutex_unlock(m, cv)` | Release mutex and wait on condition variable (atomic) |
| `mutex_unlock(m, cv, timeout)` | Timed wait — returns `false` on timeout |
| `make_condvar()` | Create a condition variable |
| `condvar_signal(cv)` | Wake one waiting thread |
| `condvar_broadcast(cv)` | Wake all waiting threads |

The key primitive for `Monitor->wait()` is `mutex_unlock(m, cv)` — SRFI-18's `mutex-unlock!` with a condvar argument atomically releases the mutex and blocks on the condvar. After being signaled, the thread wakes but does NOT hold the lock — `mutex_lock(m)` must be called explicitly to re-acquire.

## See also

- [CONCURRENT.md](CONCURRENT.md) — Thread-safe containers: ConcurrentDict, ConcurrentQueue, ConcurrentStack, ConcurrentList
- [TASKS.md](TASKS.md) — TaskPool: scalable task execution with OS threads + green threads
- [THREADS.md](THREADS.md) — Green threads, mutexes, condition variables, continuations
- [ASYNC.md](ASYNC.md) — Async/await, thread pools, channels, pipelines
- [NETWORKING.md](NETWORKING.md) — TCP sockets, HTTP, OO wrappers with green threads
- [GREEN_THREADS.md](GREEN_THREADS.md) — Deep dive into chibi-scheme green thread internals
- [BINARY.md](BINARY.md) — Cap'n Proto binary serialization for efficient cross-thread data exchange
