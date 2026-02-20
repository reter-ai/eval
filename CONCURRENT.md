# Concurrent Containers

Thread-safe data structures that work across **green threads** and **OS thread pool workers**: **ConcurrentDict**, **ConcurrentQueue**, **ConcurrentStack**, and **ConcurrentList**. All support RAII via `with`, named sharing across workers, and green-thread-aware blocking.

## Quick start

```
// Dict — thread-safe key-value store
define d = ConcurrentDict();
d->set("x", 42);
d->get("x");          // => 42
d->has?("x");         // => true

// Queue — FIFO, blocks on empty (green-thread-aware)
define q = ConcurrentQueue();
q->push(1); q->push(2); q->push(3);
q->pop();              // => 1 (FIFO)

// Bounded queue — blocks on full
define bq = ConcurrentQueue(10);
bq->push("item");

// Stack — LIFO, blocks on empty
define s = ConcurrentStack();
s->push(1); s->push(2); s->push(3);
s->pop();              // => 3 (LIFO)
s->peek();             // => 2 (no removal)

// List — concurrent dynamic array
define l = ConcurrentList();
l->append(10); l->append(20);
l->ref(0);             // => 10
l->ref(-1);            // => 20 (negative indexing)

// RAII — auto-close on scope exit
with(d = ConcurrentDict()) {
    d->set("key", "value");
};
```

## Named containers: cross-worker sharing

Pass a name string to share the same container across OS thread pool workers. Named containers use a global registry — any context that creates a container with the same name gets a handle to the same underlying data:

```
// Main context
define counts = ConcurrentDict("counts");
counts->set("total", 0);

// Workers see the same dict
with(pool = TaskPool(4)) {
    pool->map([1, 2, 3, 4], function(x) {
        define d = ConcurrentDict("counts");
        d->set(`number->string`(x), x * x);
    });
};

counts->size();        // => 5 (total + 4 worker entries)
counts->get("3");      // => 9
```

Without a name (or with `false`), the container is anonymous — only accessible through the variable that holds it. Anonymous containers work with green threads (shared memory) but not across pool workers (isolated heaps).

## ConcurrentDict

A thread-safe hash map with string keys and arbitrary values.

```
define d = ConcurrentDict();           // anonymous
define d = ConcurrentDict("shared");   // named (cross-worker)

d->set("key", 42);
d->get("key");         // => 42
d->get("missing");     // => false
d->has?("key");        // => true
d->delete("key");      // => true
d->size();             // => 0
d->empty?();           // => true

d->set("a", 1);
d->set("b", 2);
d->keys();             // => ("a" "b")
d->vals();             // => (1 2)
d->entries();          // => (("a" . 1) ("b" . 2))
d->clear();            // remove all entries
d->close();            // release resources
```

### API

| Method | Description |
|--------|-------------|
| `ConcurrentDict()` | Create anonymous dict |
| `ConcurrentDict("name")` | Create or retrieve named dict |
| `d->set(key, val)` | Set a key-value pair |
| `d->get(key)` | Get value, or `false` if missing |
| `d->has?(key)` | Check if key exists |
| `d->delete(key)` | Remove a key |
| `d->size()` | Number of entries |
| `d->empty?()` | True if size is 0 |
| `d->keys()` | List of all keys |
| `d->vals()` | List of all values |
| `d->entries()` | List of `(key . value)` pairs |
| `d->clear()` | Remove all entries |
| `d->close()` | Release resources |
| `d->__type__` | `'__concurrent_dict__` |

## ConcurrentQueue

A thread-safe FIFO queue. Optionally bounded (capacity limit). Blocking operations are green-thread-aware — they yield instead of blocking the OS thread.

```
define q = ConcurrentQueue();          // unbounded
define q = ConcurrentQueue(100);       // bounded, capacity 100
define q = ConcurrentQueue("shared");  // named, unbounded
define q = ConcurrentQueue("shared", 100); // named + bounded

q->push(1);
q->push(2);
q->push(3);
q->pop();              // => 1 (FIFO order)
q->pop();              // => 2
q->size();             // => 1
```

### Blocking vs non-blocking

`push` and `pop` are green-thread-aware: when called from a context with active green threads, they yield and retry instead of blocking the OS thread. This prevents deadlocks in cooperative concurrency:

```
// Blocking — yields to other green threads if empty
q->pop();

// Non-blocking — returns false immediately if empty
q->try_pop();          // => value or false

// Blocking push (bounded queue) — yields if full
q->push(item);

// Non-blocking push — returns false if full
q->try_push(item);     // => true or false
```

### Bounded queue

A bounded queue rejects pushes when full:

```
define q = ConcurrentQueue(2);
q->try_push(1);        // => true
q->try_push(2);        // => true
q->try_push(3);        // => false (full)
q->pop();              // => 1
q->try_push(3);        // => true (space available)
```

### API

| Method | Description |
|--------|-------------|
| `ConcurrentQueue()` | Unbounded anonymous queue |
| `ConcurrentQueue(capacity)` | Bounded anonymous queue |
| `ConcurrentQueue("name")` | Named unbounded queue |
| `ConcurrentQueue("name", capacity)` | Named bounded queue |
| `q->push(item)` | Enqueue (blocks if bounded and full) |
| `q->pop()` | Dequeue (blocks if empty) |
| `q->try_push(item)` | Non-blocking enqueue — `true` or `false` |
| `q->try_pop()` | Non-blocking dequeue — value or `false` |
| `q->size()` | Number of items |
| `q->empty?()` | True if size is 0 |
| `q->close()` | Close the queue (pop on closed queue raises error) |
| `q->__type__` | `'__concurrent_queue__` |

### Producer-consumer with green threads

```
define q = ConcurrentQueue();

define producer = make_thread(function() {
    for(let i = 0, i < 5, i++) q->push(i);
    q->close();
});

define consumer = make_thread(function() {
    define results = [];
    try {
        while(true) {
            results = append(results, [q->pop()]);
        };
    } catch(e) results;
});

thread_start(producer);
thread_start(consumer);
thread_join(producer);
thread_join(consumer);    // => [0, 1, 2, 3, 4]
```

## ConcurrentStack

A thread-safe LIFO stack. Blocking `pop` is green-thread-aware.

```
define s = ConcurrentStack();          // anonymous
define s = ConcurrentStack("shared");  // named (cross-worker)

s->push(1);
s->push(2);
s->push(3);
s->pop();              // => 3 (LIFO order)
s->peek();             // => 2 (no removal)
s->size();             // => 2
```

### API

| Method | Description |
|--------|-------------|
| `ConcurrentStack()` | Create anonymous stack |
| `ConcurrentStack("name")` | Create or retrieve named stack |
| `s->push(val)` | Push onto top |
| `s->pop()` | Pop from top (blocks if empty) |
| `s->try_pop()` | Non-blocking pop — value or `false` |
| `s->peek()` | Read top without removing — value or `false` |
| `s->size()` | Number of items |
| `s->empty?()` | True if size is 0 |
| `s->close()` | Close the stack |
| `s->__type__` | `'__concurrent_stack__` |

## ConcurrentList

A thread-safe dynamic array with random access. Supports negative indexing (Python-style).

```
define l = ConcurrentList();           // anonymous
define l = ConcurrentList("shared");   // named (cross-worker)

l->append(10);
l->append(20);
l->append(30);
l->ref(0);             // => 10
l->ref(-1);            // => 30 (last element)
l->set(1, 99);
l->ref(1);             // => 99

l->prepend(5);         // insert at front
l->remove(2);          // remove by index

define snap = l->to_list();  // snapshot as Scheme list
l->clear();
l->empty?();           // => true
```

### API

| Method | Description |
|--------|-------------|
| `ConcurrentList()` | Create anonymous list |
| `ConcurrentList("name")` | Create or retrieve named list |
| `l->append(val)` | Add to end |
| `l->prepend(val)` | Add to front |
| `l->ref(idx)` | Read element (negative = from end) |
| `l->set(idx, val)` | Write element |
| `l->remove(idx)` | Remove by index |
| `l->size()` | Number of elements |
| `l->empty?()` | True if size is 0 |
| `l->to_list()` | Snapshot as a Scheme list |
| `l->clear()` | Remove all elements |
| `l->close()` | Release resources |
| `l->__type__` | `'__concurrent_list__` |

## RAII with `with`

All four container types have a `close()` method, so they integrate with the `with` RAII pattern. Resources are released automatically when the block exits:

```
with(d = ConcurrentDict()) {
    d->set("key", "value");
    d->get("key");
};
// d is closed here

with(q = ConcurrentQueue(10)) {
    q->push("task");
    q->pop();
};
// q is closed here
```

This is the same `with` pattern used by Pool, TaskPool, Mutex, TcpSocket, and other RAII-enabled types.

## Cross-worker sharing

Named containers are the primary mechanism for sharing mutable state between OS thread pool workers. Each worker has an isolated chibi-scheme heap, but named containers live in shared C memory protected by OS mutexes.

### Shared counter with TaskPool

```
with(pool = TaskPool(4)) {
    define d = ConcurrentDict("results");

    pool->map([1, 2, 3, 4, 5, 6, 7, 8], function(x) {
        define d = ConcurrentDict("results");
        d->set(`number->string`(x), x * x);
    });

    d->size();    // => 8
    d->get("5");  // => 25
};
```

### Work queue with Pool

```
{
    with(pool = Pool(2));
    define q = ConcurrentQueue("work");
    define results = ConcurrentDict("results");

    // Enqueue work from main
    q->push("task-1");
    q->push("task-2");
    q->push("done");
    q->push("done");

    // Workers process the queue
    pool->submit("
        define q = ConcurrentQueue(\"work\");
        define r = ConcurrentDict(\"results\");
        define task = q->pop();
        while(task != \"done\") {
            r->set(task, true);
            task = q->pop();
        };
    ");
    pool->submit("
        define q = ConcurrentQueue(\"work\");
        define r = ConcurrentDict(\"results\");
        define task = q->pop();
        while(task != \"done\") {
            r->set(task, true);
            task = q->pop();
        };
    ");

    // Wait and check
    `thread-sleep!`(0.1);
    results->size();    // => 2
};
```

## Green thread integration

Blocking operations (`pop` on empty queue/stack, `push` on full bounded queue) are cooperative — they yield to other green threads instead of blocking the OS thread:

```
// Without green threads: OS-level blocking (condvar wait)
// With green threads: yield + retry loop

define q = ConcurrentQueue();

// Consumer blocks (yields) until producer pushes data
define consumer = make_thread(function() q->pop());

// Producer pushes after a delay
define producer = make_thread(function() {
    thread_yield();
    q->push(42);
});

thread_start(consumer);
thread_start(producer);
thread_join(producer);
thread_join(consumer);    // => 42
```

This means concurrent containers can be used in TaskPool workers where green threads handle multiple tasks per OS thread — blocking one task doesn't block the entire worker.

## How it works

```
                         Named Registry
                         (global, mutex-protected)
                              |
              +-------+-------+-------+
              |       |       |       |
           CDict   CQueue  CStack  CList
           (mutex)  (mutex  (mutex  (mutex)
                    +cond)  +cond)

  Green thread context:          Pool worker context:
  ┌──────────────────┐          ┌──────────────────┐
  │  Eval code       │          │  Eval code       │
  │  d->set(k, v)    │          │  d->set(k, v)    │
  │       │          │          │       │          │
  │  Bridge fn       │          │  Bridge fn       │
  │  serialize val   │          │  serialize val   │
  │  lock mutex      │          │  lock mutex      │
  │  store bytes     │          │  store bytes     │
  │  unlock mutex    │          │  unlock mutex    │
  └──────────────────┘          └──────────────────┘
           │                             │
           └─────── Same C struct ───────┘
```

- **C bridge functions** are atomic from the green thread scheduler (no yield mid-call), so a single mutex gives both green-thread safety and OS-thread safety.
- **Values are serialized** to strings (`sexp_write_to_string` / `sexp_read_from_string`) so they can cross isolated worker heaps. Any value that can be `write`-serialized works: numbers, strings, booleans, lists, vectors, symbols.
- **Named containers** use a global registry protected by its own mutex. Creating a container with a name that already exists returns the existing one.
- **Blocking operations** detect green thread context: if other green threads are running, they return a retry signal and let the Eval wrapper yield and retry; otherwise they use OS condvar blocking.

## See also

- [MULTITHREADING.md](MULTITHREADING.md) — Synchronization primitives: Mutex, Monitor, ReadWriteLock, Semaphore
- [TASKS.md](TASKS.md) — TaskPool: scalable task execution with OS threads + green threads
- [THREADS.md](THREADS.md) — Green threads, mutexes, condition variables, continuations
- [ASYNC.md](ASYNC.md) — Async/await, thread pools, channels, pipelines
