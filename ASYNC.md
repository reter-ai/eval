# Async Programming in Eval

Eval provides two complementary concurrency models: **async/await** for lightweight cooperative concurrency within a single VM, and **thread pools** for true OS-level parallelism across isolated VMs. Both use the same `await` function, so you can freely mix them.

## Async: Green Thread Promises

`async` spawns a green thread and returns a promise. The expression runs cooperatively — it shares memory with the main thread and yields control at natural suspension points (function calls, loops).

```
define p = async 2 + 3;
await(p);              // => 5
```

The async body can be any expression — a simple value, a function call, or a block:

```
// Simple expression
define a = async 42;

// Function call
define double = function(x) x * 2;
define b = async double(21);

// Block with local state
define c = async {
    define x = 10;
    define y = 20;
    x + y;
};

await(a);    // => 42
await(b);    // => 42
await(c);    // => 30
```

## Concurrent Tasks

Multiple async expressions run concurrently as green threads. The VM interleaves their execution:

```
define fib = function(n) if(n <= 1) n else fib(n - 1) + fib(n - 2);

// Launch three computations concurrently
define a = async fib(10);
define b = async fib(12);
define c = async fib(15);

// Collect results — each await blocks only until its promise resolves
[await(a), await(b), await(c)];    // => [55, 144, 610]
```

Since green threads share memory, they can communicate through mutable variables:

```
define counter = 0;

define p1 = async { counter += 1; };
define p2 = async { counter += 1; };
define p3 = async { counter += 1; };

await(p1);
await(p2);
await(p3);
counter;    // => 3
```

## Error Propagation

If an async expression throws, the error is captured in the promise. Calling `await` re-raises it in the caller's context:

```
define p = async { error("boom"); };

try {
    await(p);
} catch(e) {
    display(e);    // => "boom"
};
```

This means error handling composes naturally — wrap `await` in `try/catch` just like synchronous code.

## Promise API

Promises are first-class objects with a simple API:

```
define p = async 42;

promise?(p);     // => true
promise?(42);    // => false

// Non-blocking check — has the promise resolved?
p->ready?;       // => true (if the green thread has finished)

// Blocking wait
await(p);        // => 42
```

## Nested Async

Async expressions can spawn further async tasks:

```
define result = async {
    define inner = async 42;
    await(inner) + 8;
};
await(result);    // => 50
```

## Async in Loops

Launch many tasks in a loop and collect results:

```
define promises = [];
for(let i = 0, i < 5, i++) {
    let(val = i * i) {
        promises = cons(async val, promises);
    };
};

define results = map(function(p) await(p), `reverse`(promises));
results;    // => [0, 1, 4, 9, 16]
```

## Thread Pool: True Parallelism

For CPU-bound work, use a **thread pool**. Each worker runs an independent chibi-scheme VM on a real OS thread. Workers don't share memory — they communicate through channels and futures.

### OO Pool API

`Pool(n)` creates a pool with `n` worker threads. Use `with` for automatic shutdown:

```
// Expression-form RAII — pool shuts down after the block
define result = with(pool = Pool(4)) {
    define f1 = pool->submit("10 * 10;");
    define f2 = pool->submit("20 + 5;");
    await(f1) + await(f2);
};
result;    // => 125

// Statement-form RAII — pool shuts down at block end
{
    with(pool = Pool(2));
    define f = pool->submit("6 * 7;");
    f->result();              // => 42
    f->ready?;                // => true
};

// Manual lifecycle
define p = Pool(2);
define f = p->submit("1 + 2;");
await(f);                     // => 3
p->shutdown();
```

### Submitting Work

Workers execute code strings — each submission gets a fresh environment:

```
{
    with(pool = Pool(4));

    // Simple expressions
    define a = pool->submit("2 ** 10;");
    define b = pool->submit("fold(+, 0, [1, 2, 3, 4, 5]);");

    // Functions defined inside the worker
    define c = pool->submit("
        define fib = function(n)
            if(n <= 1) n
            else fib(n - 1) + fib(n - 2);
        fib(20);
    ");

    await(a);    // => 1024
    await(b);    // => 15
    await(c);    // => 6765
};
```

### Futures

Every `pool->submit()` returns a future:

| Method | Description |
|---|---|
| `await(f)` | Block until result (unified — works with both promises and futures) |
| `f->result()` | Block until result (OO syntax) |
| `f->ready?` | Non-blocking boolean check |

### pool_apply: Send Functions to Workers

Instead of code strings, send actual closures to workers using `pool_apply`. The function and arguments are binary-serialized:

```
define square = function(x) x * x;

{
    with(pool = Pool(4));

    // Apply a function in a worker
    define f = pool_apply(pool, square, [7]);
    future_result(f);    // => 49

    // Closures with captured state work too
    define make_adder = function(n) function(x) n + x;
    define add100 = make_adder(100);
    future_result(pool_apply(pool, add100, [23]));    // => 123

    // Workers can return closures back
    define multiplier = function(n) function(x) n * x;
    define triple = future_result(pool_apply(pool, multiplier, [3]));
    triple(10);    // => 30
};
```

### Channels

Named channels allow bidirectional communication between the main context and workers:

```
{
    with(pool = Pool(2));
    with(ch = pool->channel("data"));

    // Main → Worker
    ch->send("hello");
    pool->submit("
        define msg = channel_recv(data);
        channel_send(data, msg ++ \" world\");
    ");
    ch->recv();    // => "hello world"

    // Non-blocking receive
    ch->try_recv();    // => false (empty)
    ch->send(42);
    ch->try_recv();    // => [42] (value wrapped in list)
};
```

| Method | Description |
|---|---|
| `ch->send(val)` | Send value to channel |
| `ch->recv()` | Blocking receive |
| `ch->try_recv()` | Non-blocking — returns `[value]` or `false` |
| `ch->close()` | Close channel |

### Producer-Consumer Pipeline

Chain workers together through channels to build processing pipelines:

```
{
    with(pool = Pool(3));
    define input  = pool_channel(pool, "input");
    define output = pool_channel(pool, "output");

    // Producer: generate squares
    pool_submit(pool, "
        define i = 0;
        while(i < 5) {
            i++;
            channel_send(input, i * i);
        };
    ");

    // Consumer: sum them up
    pool_submit(pool, "
        define total = 0;
        define count = 0;
        while(count < 5) {
            total += channel_recv(input);
            count++;
        };
        channel_send(output, total);
    ");

    channel_recv(output);    // => 55 (1 + 4 + 9 + 16 + 25)
};
```

### Worker Chain Pipeline

Multi-stage pipelines where each worker transforms and forwards:

```
{
    with(pool = Pool(3));
    define step1 = pool_channel(pool, "step1");
    define step2 = pool_channel(pool, "step2");
    define result = pool_channel(pool, "result");

    // Stage 1: add 10
    pool_submit(pool, "
        define x = channel_recv(step1);
        channel_send(step2, x + 10);
    ");

    // Stage 2: multiply by 3
    pool_submit(pool, "
        define x = channel_recv(step2);
        channel_send(result, x * 3);
    ");

    channel_send(step1, 5);
    channel_recv(result);    // => 45 — (5 + 10) * 3
};
```

### Error Handling

Worker errors propagate through futures:

```
{
    with(pool = Pool(1));

    // Syntax error
    define f1 = pool->submit("not valid code!!!");
    try { await(f1); } catch(e) display("syntax error caught");

    // Runtime error
    define f2 = pool->submit("error(\"boom\");");
    try { await(f2); } catch(e) display("runtime error caught");

    // Pool recovers — next submission works fine
    define f3 = pool->submit("42;");
    await(f3);    // => 42
};
```

## Unified Await

`await` works with both green-thread promises and thread pool futures — you don't need to care which one you have:

```
// Green thread promise
define p = async 42;
await(p);                           // => 42

// Thread pool future
define pool = make_pool(2);
define f = pool_submit(pool, "6 * 7;");
await(f);                           // => 42
pool_shutdown(pool);
```

## When to Use What

| | `async` | Thread pool |
|---|---|---|
| **Mechanism** | Green thread (cooperative) | OS thread (true parallelism) |
| **Context** | Shared — can access local variables | Isolated — code runs in separate VM |
| **Communication** | Shared mutable state | Channels and futures |
| **Overhead** | Very low (continuation switch) | Higher (serialization, thread creation) |
| **Best for** | Concurrent I/O, structured concurrency | CPU-bound parallelism |

**Use `async`** when you need lightweight concurrency with shared state — multiple tasks that cooperatively share the same VM. Works naturally with networking: `TcpClient`, `TcpServer`, and `HttpClient` all yield to the green thread scheduler on blocking operations.

**Use thread pools** when you need true parallelism for CPU-bound work — each worker runs on its own OS thread with its own VM, achieving real speedup on multi-core machines.

**Mix both** when you need both: use the thread pool for heavy computation, and `async` within each worker or the main thread for lightweight concurrent coordination. For example, a `TcpServer` can dispatch CPU-bound work to a thread pool while handling I/O with green threads.

### Proving True Parallelism

Thread pools use real OS threads, so CPU-bound work runs faster in parallel:

```
define fib_code = "define fib = function(n) if(n <= 1) n else fib(n-1) + fib(n-2); fib(28);";

// Serial: 4 tasks on 1 worker
define pool1 = make_pool(1);
define t0 = current_second();
future_result(pool_submit(pool1, fib_code));
future_result(pool_submit(pool1, fib_code));
future_result(pool_submit(pool1, fib_code));
future_result(pool_submit(pool1, fib_code));
define serial_time = current_second() - t0;
pool_shutdown(pool1);

// Parallel: 4 tasks on 4 workers
define pool4 = make_pool(4);
define t1 = current_second();
define fa = pool_submit(pool4, fib_code);
define fb = pool_submit(pool4, fib_code);
define fc = pool_submit(pool4, fib_code);
define fd = pool_submit(pool4, fib_code);
future_result(fa); future_result(fb);
future_result(fc); future_result(fd);
define parallel_time = current_second() - t1;
pool_shutdown(pool4);

// parallel_time < serial_time * 0.8 — real speedup!
```

## Low-Level Pool API

The OO wrappers (`Pool`, `->submit`, `->channel`) are built on top of these functions:

| Function | Description |
|----------|-------------|
| `make_pool(n)` | Create a pool with `n` worker threads |
| `pool_submit(pool, code)` | Submit code string, returns a future |
| `pool_apply(pool, fn, args)` | Submit a closure with args (binary serialized) |
| `pool_channel(pool, name)` | Get or create a named channel |
| `future_result(f)` | Block until done, return result |
| `future_ready?(f)` | Non-blocking check |
| `channel_send(ch, val)` | Send a value |
| `channel_recv(ch)` | Blocking receive |
| `channel_try_recv(ch)` | Non-blocking: `[value]` or `false` |
| `channel_close(ch)` | Close a channel |
| `pool_shutdown(pool)` | Shut down all workers |
