# Green Threads, Continuations, and Thread Pools in Eval

Eval has three layers of concurrency, each building on the one below:

1. **Continuations** — first-class, serializable control flow
2. **Green threads** — cooperative multitasking within a single VM
3. **Thread pools** — true OS-level parallelism across isolated VMs

## Continuations

A continuation captures "the rest of the computation" at any point. `callcc` (call-with-current-continuation) gives you a function that, when called, jumps back to where it was captured:

```
// Basic: capture and immediately invoke
callcc(function(k) k(42));    // => 42

// Early exit from a computation
callcc(function(exit) {
    define x = 10;
    exit(x);       // jumps out immediately
    x + 100;       // never reached
});
// => 10
```

### Storing Continuations

Continuations are first-class values — store them in variables, pass them to functions, call them later:

```
define saved = false;
define result = callcc(function(k) { saved = k; "first"; });
result;    // => "first"

// Call the saved continuation — jumps back, result becomes "second"
saved("second");
result;    // => "second"
```

### Early Exit from Loops

Use `callcc` to break out of deeply nested computations:

```
define result = callcc(function(exit) {
    define i = 0;
    while(i < 100) {
        if(i == 5) exit(i);    // jump out with value 5
        i++;
    };
    999;
});
result;    // => 5
```

### Return Statement

Eval's `return` keyword is built on continuations. Every `function` body is wrapped in a continuation, and `return` invokes it:

```
define abs = function(x) {
    if(x >= 0) return x;
    return 0 - x;
};
abs(-7);    // => 7

define classify = function(n) {
    if(n < 0) return "negative";
    if(n == 0) return "zero";
    return "positive";
};
classify(-1);    // => "negative"
```

### Continuation Serialization

Continuations can be serialized to bytes and restored later — even in a different process or on a different machine:

```python
# Python API
from chibi_eval import Eval

e = Eval()
e.eval("""
    define saved = false;
    define result = callcc(function(k) { saved = k; "initial"; });
""")

# Serialize the continuation to bytes
data = e.serialize_continuation(e["saved"])

# Later, restore in a fresh interpreter
e2 = Eval()
k = e2.deserialize_continuation(data)
e2.eval("k(\"resumed\");")    # jumps back, result becomes "resumed"
```

This enables:
- **Checkpointing** — save program state, resume after crash
- **Migration** — move a computation between machines
- **Time travel** — snapshot and replay execution

## Green Threads (SRFI-18)

Green threads are lightweight cooperative threads that share a single OS thread. The chibi-scheme VM interleaves their execution using fuel-based scheduling — each thread runs for a quantum of VM instructions, then yields to the next.

### Basic Usage

```
define t = make_thread(function() {
    define sum = 0;
    for(let i = 0, i < 10, i++) sum += i;
    sum;
});
thread_start(t);
thread_join(t);    // => 45
```

### Thread API

| Function | Description |
|----------|-------------|
| `make_thread(fn)` | Create a thread (not started) |
| `make_thread(fn, name)` | Create a named thread |
| `thread_start(t)` | Start a thread |
| `thread_join(t)` | Wait for completion, return result |
| `thread_yield()` | Voluntarily yield to other threads |
| `thread_sleep(seconds)` | Sleep for a duration |
| `current_thread()` | Get the current thread object |

### Multiple Threads

Launch several threads and collect their results:

```
define t1 = make_thread(function() 10);
define t2 = make_thread(function() 20);
define t3 = make_thread(function() 30);
thread_start(t1);
thread_start(t2);
thread_start(t3);
thread_join(t1) + thread_join(t2) + thread_join(t3);    // => 60
```

### Shared State

Green threads share memory, so they can communicate through mutable variables:

```
define result = 0;
define t = make_thread(function() { result = 42; });
thread_start(t);
thread_join(t);
result;    // => 42
```

### Closures in Threads

Threads capture their environment — closures work naturally:

```
define a = 5;
define b = 10;
define t = make_thread(function() a + b);
thread_start(t);
thread_join(t);    // => 15
```

### Mutex Synchronization

When multiple threads mutate shared state, use mutexes to prevent races:

```
define counter = 0;
define m = make_mutex();

define inc = function() {
    for(let i = 0, i < 50, i++) {
        mutex_lock(m);
        counter = counter + 1;
        mutex_unlock(m);
    };
};

define t1 = make_thread(inc);
define t2 = make_thread(inc);
thread_start(t1);
thread_start(t2);
thread_join(t1);
thread_join(t2);
counter;    // => 100
```

| Function | Description |
|----------|-------------|
| `make_mutex()` | Create a mutex |
| `mutex_lock(m)` | Acquire the mutex (blocks if held) |
| `mutex_unlock(m)` | Release the mutex |

### Condition Variables

For thread signaling and waiting:

```
define cv = make_condvar();
condvar_signal(cv);       // wake one waiting thread
condvar_broadcast(cv);    // wake all waiting threads
```

| Function | Description |
|----------|-------------|
| `make_condvar()` | Create a condition variable |
| `condvar_signal(cv)` | Wake one waiting thread |
| `condvar_broadcast(cv)` | Wake all waiting threads |

### Nested Threads

Threads can spawn more threads:

```
define t = make_thread(function() {
    define t2 = make_thread(function() 42);
    thread_start(t2);
    thread_join(t2);
});
thread_start(t);
thread_join(t);    // => 42
```

### Cross-Thread Continuations

Continuations work across thread boundaries — capture in one thread, invoke in another:

```
define k = callcc(function(k) k);
if(`procedure?`(k)) {
    define t = make_thread(function() k(42));
    thread_start(t);
    thread_join(t);
};
k;    // => 42
```

### Error Handling in Threads

Threads have their own error context — use `try/catch` inside the thread:

```
define t = make_thread(function() {
    try {
        error("thread error");
    } catch(e) {
        "caught";
    };
});
thread_start(t);
thread_join(t);    // => "caught"
```

## Async / Await

`async` is syntactic sugar over green threads. It spawns a thread and wraps the result in a promise:

```
define p = async {
    define x = 10;
    define y = 20;
    x + y;
};
await(p);    // => 30
```

See [ASYNC.md](ASYNC.md) for the full async programming guide.

## Thread Pools

Thread pools provide true OS-level parallelism. Each worker runs an independent chibi-scheme VM on a real OS thread. Workers don't share memory — they communicate through channels and futures.

### Creating a Pool

```
// OO API with RAII
define result = with(pool = Pool(4)) {
    define f = pool->submit("6 * 7;");
    await(f);
};
result;    // => 42

// Low-level API
define pool = make_pool(4);
define f = pool_submit(pool, "6 * 7;");
future_result(f);    // => 42
pool_shutdown(pool);
```

### Submitting Closures

`pool_apply` serializes a closure and its arguments in binary, sends them to a worker, and returns the result:

```
define square = function(x) x * x;
define make_adder = function(n) function(x) n + x;
define add100 = make_adder(100);

define pool = make_pool(2);
future_result(pool_apply(pool, square, [7]));     // => 49
future_result(pool_apply(pool, add100, [23]));    // => 123
pool_shutdown(pool);
```

Workers can return closures back — they're deserialized in the receiving context:

```
define multiplier = function(n) function(x) n * x;

define pool = make_pool(2);
define triple = future_result(pool_apply(pool, multiplier, [3]));
triple(10);    // => 30
pool_shutdown(pool);
```

### Channels

Named channels allow bidirectional communication:

```
{
    with(pool = Pool(2));
    with(ch = pool->channel("data"));

    // Send from main, receive in worker
    ch->send(42);
    define f = pool->submit("define x = channel_recv(data); x * 2;");
    await(f);    // => 84

    // Send from worker, receive in main
    pool->submit("channel_send(data, 99);");
    ch->recv();    // => 99
};
```

### Closures Across Threads via Channels

Closures can be sent through channels in binary form:

```
{
    with(pool = Pool(2));
    define fn_ch = pool_channel(pool, "fn_ch");
    define out_ch = pool_channel(pool, "out_ch");

    // Worker A: create a closure, send it
    pool_submit(pool, "
        define make_adder = function(n) function(x) n + x;
        define add100 = make_adder(100);
        channel_send(fn_ch, add100);
    ");

    // Worker B: receive the closure, apply it
    pool_submit(pool, "
        define fn = channel_recv(fn_ch);
        channel_send(out_ch, fn(23));
    ");

    channel_recv(out_ch);    // => 123
};
```

### Continuations Across Threads

Continuations can be serialized, sent through channels, and invoked in a different worker:

```
{
    with(pool = Pool(2));
    define k_ch = pool_channel(pool, "k_ch");

    // Worker A: capture a continuation that computes x * x
    pool_submit(pool, "
        define k = eval_scheme(\"
            (let ((x (call-with-current-continuation (lambda (k) k))))
              (if (procedure? x) x (* x x)))
        \");
        channel_send(k_ch, k);
    ");

    // Worker B: receive and invoke the continuation
    define f = pool_submit(pool, "
        define k = channel_recv(k_ch);
        k(7);
    ");
    future_result(f);    // => 49
};
```

### Green Threads Inside Workers

Each pool worker can run its own green threads:

```
{
    with(pool = Pool(2));
    define out = pool_channel(pool, "out");

    pool_submit(pool, "
        define counter = 0;
        define m = make_mutex();

        define inc = function() {
            for(let i = 0, i < 50, i++) {
                mutex_lock(m);
                counter = counter + 1;
                mutex_unlock(m);
            };
        };

        define t1 = make_thread(inc);
        define t2 = make_thread(inc);
        thread_start(t1);
        thread_start(t2);
        thread_join(t1);
        thread_join(t2);
        channel_send(out, counter);
    ");

    channel_recv(out);    // => 100
};
```

### Multi-Stage Pipeline with Green Threads

Combine all three layers — green threads generate data inside workers, channels connect the stages:

```
{
    with(pool = Pool(3));
    define pipe_in  = pool_channel(pool, "pipe_in");
    define pipe_mid = pool_channel(pool, "pipe_mid");
    define pipe_out = pool_channel(pool, "pipe_out");

    // Stage 1: green threads generate values
    pool_submit(pool, "
        define gen = function(start, count) {
            for(let i = 0, i < count, i++) {
                channel_send(pipe_in, start + i);
            };
        };
        define t1 = make_thread(function() gen(1, 3));
        define t2 = make_thread(function() gen(10, 3));
        thread_start(t1);
        thread_start(t2);
        thread_join(t1);
        thread_join(t2);
        channel_send(pipe_in, \"done\");
    ");

    // Stage 2: transform — square each value
    pool_submit(pool, "
        define val = channel_recv(pipe_in);
        while(val != \"done\") {
            channel_send(pipe_mid, val * val);
            val = channel_recv(pipe_in);
        };
        channel_send(pipe_mid, \"done\");
    ");

    // Stage 3: collect results
    pool_submit(pool, "
        define results = [];
        define val = channel_recv(pipe_mid);
        while(val != \"done\") {
            results = append(results, [val]);
            val = channel_recv(pipe_mid);
        };
        channel_send(pipe_out, sort(results, <));
    ");

    channel_recv(pipe_out);    // => [1, 4, 9, 100, 121, 144]
};
```

## Comparison

| Feature | Continuations | Green Threads | Thread Pool |
|---------|--------------|---------------|-------------|
| **Level** | Control flow | Cooperative concurrency | OS parallelism |
| **Memory** | Shared | Shared | Isolated per worker |
| **Overhead** | Zero (just a jump) | Very low (context switch) | Higher (serialization) |
| **Parallelism** | No | No (single OS thread) | Yes (multiple cores) |
| **Communication** | Direct (shared state) | Direct (shared state) | Channels / futures |
| **Serializable** | Yes (to bytes) | No | Closures via `pool_apply` |
| **Use case** | Early exit, coroutines | I/O concurrency, scheduling | CPU-bound computation |

## Putting It Together

Here's a pattern that uses all three layers — continuations for control flow, green threads for concurrency within workers, and thread pools for parallelism:

```
// A function using return (continuation-based) and callcc for early exit
define process_batch = function(items) {
    if(length(items) == 0) return [];
    callcc(function(abort) {
        define results = [];
        for(let item in items) {
            if(item < 0) abort(results);    // abort on negative
            results = append(results, [item * item]);
        };
        results;
    });
};

// Run it across multiple workers with green threads
{
    with(pool = Pool(4));
    define out = pool_channel(pool, "out");

    // Each worker spawns green threads to process batches
    pool_submit(pool, "
        define t1 = make_thread(function() {
            channel_send(out, process_batch([1, 2, 3]));
        });
        define t2 = make_thread(function() {
            channel_send(out, process_batch([4, 5, -1, 6]));
        });
        thread_start(t1);
        thread_start(t2);
        thread_join(t1);
        thread_join(t2);
    ");

    // But wait — process_batch uses return and callcc, which were defined
    // in the main context. Workers need their own copy:
    pool_apply(pool, process_batch, [[10, 20, 30]]);
    // The closure is serialized and sent to the worker — it just works!

    channel_recv(out);    // => [1, 4, 9]
    channel_recv(out);    // => [16, 25] (aborted at -1)
};
```

## See also

- [NETWORKING.md](NETWORKING.md) — TCP sockets and HTTP with OO wrappers (`TcpSocket`, `TcpServer`, `HttpClient`) that integrate with green threads
- [ASYNC.md](ASYNC.md) — Async/await, thread pools, channels, pipelines
