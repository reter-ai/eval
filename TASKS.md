# TaskPool: Scalable Task Execution

`TaskPool` combines OS-level thread pools with green threads to give you **true parallelism** with **concurrent task execution** within each worker. Submit closures directly — no code strings needed — and collect results via promises.

```
with(pool = TaskPool(4)) {
    define results = pool->map([1, 2, 3, 4, 5, 6, 7, 8], function(x) x * x);
    results;    // => [1, 4, 9, 16, 25, 36, 49, 64]
};
```

## Why TaskPool over Pool?

| | `Pool` | `TaskPool` |
|---|---|---|
| **Submit** | Code strings or `pool_apply` | Closures directly |
| **Results** | Futures (separate API) | Promises (unified `await`) |
| **Per-worker concurrency** | One task at a time | Green threads — many concurrent tasks |
| **Convenience** | Low-level building block | `submit`, `run`, `map`, `drain` |
| **Communication** | Channels + futures | Same, plus promise-based results |

Use `Pool` when you need raw control or are running long-lived worker loops. Use `TaskPool` when you have many independent tasks to distribute.

## Quick Start

```
// Create a pool with 4 OS workers
define pool = TaskPool(4);

// Submit a closure — returns a promise
define p = pool->submit(function() 42);
await(p);    // => 42

// run() = submit + await
pool->run(function() 6 * 7);    // => 42

// Map over a list — distributes work round-robin across workers
pool->map([1, 2, 3, 4], function(x) x * x);    // => [1, 4, 9, 16]

pool->shutdown();
```

## RAII with `with`

TaskPool supports automatic shutdown via `with`, just like Pool, sockets, and locks:

```
// Expression form — pool shuts down after the block
define result = with(pool = TaskPool(2)) {
    pool->run(function() 99);
};
result;    // => 99

// Statement form — pool shuts down at enclosing block end
{
    with(pool = TaskPool(2));
    define a = pool->submit(function() 10);
    define b = pool->submit(function() 20);
    await(a) + await(b);    // => 30
};
```

## Submitting Closures

Unlike `Pool->submit` which takes code strings, `TaskPool->submit` takes actual closures. Closures are binary-serialized to the worker, executed there, and the result serialized back:

```
{
    with(pool = TaskPool(2));

    // Closures with captured variables
    define base = 1000;
    define add_base = function(x) x + base;
    pool->run(function() add_base(42));    // => 1042

    // Recursive functions
    define fib = function(n) if(n <= 1) n else fib(n - 1) + fib(n - 2);
    define a = pool->submit(function() fib(15));
    define b = pool->submit(function() fib(16));
    await(a);    // => 610
    await(b);    // => 987
};
```

## Parallel Map

`map(list, fn)` distributes work round-robin across workers and collects results in order:

```
{
    with(pool = TaskPool(4));

    // 8 items across 4 workers:
    //   Worker 0: items 0, 4
    //   Worker 1: items 1, 5
    //   Worker 2: items 2, 6
    //   Worker 3: items 3, 7
    pool->map([1, 2, 3, 4, 5, 6, 7, 8], function(x) x * 10);
    // => [10, 20, 30, 40, 50, 60, 70, 80]
};
```

## Error Propagation

If a task throws, the error is captured in the promise. `await` re-raises it:

```
{
    with(pool = TaskPool(2));
    define p = pool->submit(function() error("boom"));

    try {
        await(p);
    } catch(e) {
        display(e);    // => "boom"
    };
};
```

The pool keeps running — subsequent tasks work fine after an error.

## Drain

`drain()` blocks until all pending tasks complete:

```
{
    with(pool = TaskPool(2));
    pool->submit(function() expensive_work_1());
    pool->submit(function() expensive_work_2());
    pool->submit(function() expensive_work_3());
    pool->drain();
    pool->pending_count();    // => 0
};
```

## Inter-Worker Channels

TaskPool exposes the underlying Pool's channel system for custom communication between workers:

```
{
    with(pool = TaskPool(1));
    define ch = pool->channel("__data");

    // Worker sends a message through the channel
    define p = pool->submit(function() {
        channel_send(__data, "hello from worker");
        "sent";
    });

    await(p);         // => "sent"
    ch->recv();       // => "hello from worker"
};
```

Inside workers, channels are accessible as variables named after the channel. The `channel_send` / `channel_recv` / `channel_try_recv` functions work across the OS thread boundary via binary serialization.

## Green Threads Within Workers

Each worker runs an event loop that spawns a green thread per task. This means multiple tasks on the same worker can run concurrently — they yield to each other via the cooperative scheduler:

```
{
    with(pool = TaskPool(1));    // single worker
    define ch = pool->channel("__sync");

    // Task A blocks on channel_recv — yields to other green threads
    define pa = pool->submit(function() {
        define msg = channel_recv(__sync);
        msg + 100;
    });

    // Task B sends — unblocks Task A
    define pb = pool->submit(function() {
        channel_send(__sync, 42);
        "done";
    });

    await(pb);    // => "done"
    await(pa);    // => 142
};
```

Both tasks run on the same OS thread. Task A blocks on `channel_recv`, the green thread scheduler switches to Task B, which sends the value. Task A wakes up and completes. Without green threads, this would deadlock.

## API Reference

### Constructor

| | Description |
|---|---|
| `TaskPool(n)` | Create a pool with `n` OS worker threads |

### Methods

| Method | Description |
|---|---|
| `pool->submit(thunk)` | Submit a zero-arg closure, return a promise |
| `pool->run(thunk)` | Submit and await — returns the result directly |
| `pool->map(list, fn)` | Apply `fn` to each element in parallel, return results list |
| `pool->drain()` | Block until all pending tasks complete |
| `pool->shutdown()` | Drain, stop workers, clean up |
| `pool->close()` | Alias for `shutdown()` (used by `with`) |
| `pool->channel(name)` | Get or create a named channel for inter-worker communication |
| `pool->size` | Number of OS workers |
| `pool->pending_count()` | Number of tasks not yet completed |

### Promises

`submit` returns a promise — the same type used by `async`:

| | Description |
|---|---|
| `await(p)` | Block until resolved, return value (or re-raise error) |
| `promise?(p)` | Type check |
| `p->ready?` | Non-blocking boolean check |

## Architecture

```
TaskPool(n)
  |
  +-- Pool(n)                      N OS worker threads
  +-- task_ch[0..n-1]              per-worker channels (round-robin)
  +-- result_ch                    shared result channel (workers -> main)
  +-- pending: hash_table          maps task_id -> promise
  +-- collector green thread       reads result_ch, resolves promises

submit(thunk):
  main context                     worker i
  -----------                      --------
  id = next_id++
  pending[id] = make_promise()
  channel_send(task_ch[robin], (id . thunk))
                                   msg = channel_try_recv(task_ch[i])
                                   thread_start(green_thread {
                                       val = thunk()
                                       channel_send(result_ch, [id, ok, val])
                                   })
  collector:
    msg = result_ch.try_recv()
    promise.__resolve__(val)       <-- await(promise) unblocks
```

Each worker runs a polling loop: `channel_try_recv` + `thread_yield`. This non-blocking pattern lets green threads within the same worker cooperate — a blocking `channel_recv` would freeze the entire OS thread.

## Patterns

### Parallel computation with collection

```
define fib = function(n) if(n <= 1) n else fib(n - 1) + fib(n - 2);

with(pool = TaskPool(4)) {
    define results = pool->map(
        [20, 21, 22, 23, 24, 25],
        function(n) fib(n)
    );
    results;    // => [6765, 10946, 17711, 28657, 46368, 75025]
};
```

### Fire-and-forget with drain

```
with(pool = TaskPool(2)) {
    for(let i = 0, i < 10, i++)
        pool->submit(function() process_item(i));
    pool->drain();    // wait for all 10 to finish
};
```

### Fan-out / fan-in

```
with(pool = TaskPool(4)) {
    // Fan out: submit many tasks
    define promises = [];
    for(let i = 0, i < 8, i++) {
        let(x = i) {
            promises = cons(pool->submit(function() x * x), promises);
        };
    };

    // Fan in: collect all results
    define results = map(function(p) await(p), `reverse`(promises));
    fold(+, 0, results);    // => 140 (0+1+4+9+16+25+36+49)
};
```

## Hybrid Async: `parallel async`

`async` always spawns a green thread. `parallel async` always dispatches to an OS thread pool. Use both in the same scope for hybrid concurrency:

```
define fib = function(n) if(n <= 1) n else fib(n - 1) + fib(n - 2);

with(ap = AsyncPool(4)) {
    define a = async light_io();              // green thread (shared state)
    define b = parallel async fib(30);        // OS thread (true parallelism)
    define c = parallel async fib(31);        // OS thread (true parallelism)
    [await(a), await(b), await(c)];           // all return the same promise type
};
```

`parallel async` requires a pool — set one up with `AsyncPool` (RAII) or `set_async_pool` (manual). Without a pool, it raises an error.

### AsyncPool API

| | Description |
|---|---|
| `AsyncPool(n)` | Create TaskPool with `n` workers, set as `parallel async` target |
| `ap->pool` | The underlying TaskPool |
| `ap->submit(thunk)` | Submit directly (delegates to pool) |
| `ap->run(thunk)` | Submit and await |
| `ap->map(list, fn)` | Parallel map |
| `ap->drain()` | Wait for all pending tasks |
| `ap->size` | Number of workers |
| `ap->pending_count()` | Pending task count |
| `ap->close()` | Drain, restore previous pool, shutdown |

### Dispatch helpers

| | Description |
|---|---|
| `set_async_pool(pool)` | Set the `parallel async` dispatch target (`false` to clear) |
| `get_async_pool()` | Get the current dispatch target |

## See also

- [CONCURRENT.md](CONCURRENT.md) — Thread-safe containers: ConcurrentDict, ConcurrentQueue, ConcurrentStack, ConcurrentList
- [ASYNC.md](ASYNC.md) — Async/await, basic thread pools, channels, pipelines
- [THREADS.md](THREADS.md) — Green threads, mutexes, condition variables
- [MULTITHREADING.md](MULTITHREADING.md) — OO synchronization: Mutex, Monitor, ReadWriteLock, Semaphore
- [GREEN_THREADS.md](GREEN_THREADS.md) — Deep dive into chibi-scheme green thread internals
