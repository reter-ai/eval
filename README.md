# chibi-eval

An expressive infix language with first-class functions, continuations, reactive programming, green threads, and a thread pool — powered by [chibi-scheme](https://github.com/ashinn/chibi-scheme).

Available as a **standalone CLI** (single self-contained binary, no dependencies) or as a **Python library** with seamless interop.

```
$ eval -e "map(function(x) x * x, [1, 2, 3, 4, 5])"
(1 4 9 16 25)
```

```python
from chibi_eval import Eval

e = Eval()
e.eval("""
    define factorial = function(n)
        if(n <= 1) 1
        else n * factorial(n - 1);

    print(factorial(20));
""")
# => 2432902008176640000
```

## Features

- **Infix syntax** — familiar C-style expressions with `define`, `let`, `if/else`, `while`, `for`, `{ blocks }`, `"""raw triple-quoted strings"""`
- **First-class functions** — closures, higher-order functions, `map`, `filter`, `fold`
- **Generators** — lazy sequences with `yield`, generator comprehensions, infinite streams, lazy pipelines
- **Reactive programming** — signals, computed values, effects, scopes, async resources
- **Continuations** — `callcc`, serializable to bytes for checkpointing and migration
- **Green threads** — cooperative multitasking with fuel-based VM scheduling
- **Synchronization** — OO wrappers: Mutex, Monitor, ReadWriteLock, Semaphore with RAII lock management
- **Thread pool** — true OS-level parallelism with worker threads, futures, and channels
- **String methods** — OO-style `"hello"->upper()`, `->trim()`, `->split(",")`, `->contains()`, chaining
- **Collection methods** — OO-style `[1,2,3]->map(f)`, `->filter()`, `->sort(<)`, `->join(",")`, `#[1,2,3]->length`
- **Networking** — TCP sockets, HTTP client/server with OO wrappers, RAII, and reactive signals
- **Standalone CLI** — single binary, all scheme files embedded, runs anywhere with no dependencies
- **Python interop** — call Python from Eval, call Eval from Python, share data bidirectionally
- **Full numeric tower** — integers, floats, bignums, rationals
- **Binary serialization** — Cap'n Proto zero-copy serialization with runtime schema parsing, wire-compatible with any language
- **Scheme power** — access any chibi-scheme primitive via backtick identifiers

## Installation

### Standalone CLI

Download a prebuilt binary from [Releases](../../releases) — single file, no dependencies:

| Platform | Download |
|----------|----------|
| Linux x86_64 | `eval-linux-x86_64.tar.gz` |
| macOS ARM (Apple Silicon) | `eval-macos-arm64.tar.gz` |
| macOS x86_64 (Intel) | `eval-macos-x86_64.tar.gz` |
| Windows x86_64 | `eval-windows-x86_64.zip` |

Or build from source (requires a C compiler — GCC, Clang, or MSVC):

```bash
cmake -B build -DBUILD_PYTHON_MODULE=OFF -DCMAKE_BUILD_TYPE=Release
cmake --build build --config Release
# Binary is at build/eval (or build/Release/eval.exe on Windows)
```

### Python library

```bash
pip install chibi-eval
```

Build from source (requires Python 3.10+ and a C compiler):

```bash
pip install -e .
```

## CLI usage

```bash
# Interactive REPL (just run with no arguments)
eval
> 2 ** 10
1024
> define double = function(x) x * 2;
> double(21)
42
> exit

# Run a file
eval program.eval

# Evaluate an expression
eval -e "2 ** 10"          # => 1024
eval -e "fold(+, 0, [1, 2, 3, 4, 5])"   # => 15

# Read from stdin
echo 'display("hello"); newline();' | eval

# Options
eval -V              # print version
eval -h              # print help
eval -I ./mylibs     # add module search directory
```

## Python quick start

```python
from chibi_eval import Eval

e = Eval()

# Arithmetic
e.eval("2 ** 10;")          # => 1024

# Variables
e.eval("define x = 42;")
e["x"]                       # => 42
e["greeting"] = "hello"
e.eval('greeting;')          # => 'hello'

# Functions
e.eval("""
    define double = function(x) x * 2;
    double(21);
""")                         # => 42

# Lists and higher-order functions
e.eval("map(function(x) x * x, [1, 2, 3, 4, 5]);")
                             # => [1, 4, 9, 16, 25]

# Call Eval functions from Python
e.eval("define add = function(a, b) a + b;")
e.call("add", 10, 20)       # => 30

# Call Python functions from Eval
import math
e.define_function("py_sqrt", math.sqrt, 1)
e.eval("py_sqrt(144);")     # => 12.0
```

## Language reference

### Variables and assignment

```
define x = 42;        // create a new variable
x = 100;              // mutate an existing variable
x += 10;              // compound assignment (also -=, *=, /=)
x++;                  // increment
x--;                  // decrement

// Block-scoped locals (internal defines → letrec)
{
    define a = 10;
    define b = 20;
    a + b;            // => 30
};
```

### Operators

| Category   | Operators                          |
|------------|------------------------------------|
| Arithmetic | `+`  `-`  `*`  `/`  `%`  `**`     |
| String     | `++` (concatenation)               |
| Comparison | `==`  `!=`  `<`  `>`  `<=`  `>=`  |
| Logical    | `&&`  `\|\|`  `!`                  |
| Bitwise    | `&`  `\|`  `~`  `<<`  `>>`        |
| Indexing   | `expr[i]`  `expr[s:e]` (slicing)   |

Operators are first-class values — pass them to higher-order functions:

```
fold(+, 0, [1, 2, 3, 4, 5]);    // => 15
apply(*, [3, 4, 5]);             // => 60
fold(++, "", ["a", "b", "c"]);   // => "abc"
define ops = [+, -, *];
car(ops);                        // => + (the function)
```

### Functions

```
// Simple
define square = function(x) x * x;

// Block body
define clamp = function(x, lo, hi) {
    if(x < lo) lo
    else if(x > hi) hi
    else x;
};

// Early return
define find_first = function(lst, pred) {
    for(let x in lst) {
        when(pred(x)) return x;
    };
    false;
};

// Closures
define make_counter = function(start) {
    define n = start;
    function() { n += 1; n; };
};
```

### Control flow

```
// If-else
if(x > 0) "positive" else "non-positive";

// Blocks (internal defines → letrec)
{
    define a = 10;
    define b = 20;
    a + b;
};

// When / Unless
when(debug) print("trace");
unless(valid) error("invalid input");

// Cond (multi-way branch)
cond(
    x < 0:   "negative",
    x == 0:  "zero",
    else:    "positive"
);

// Case (match by value)
case(color,
    ("red"):    "stop",
    ("yellow"): "caution",
    ("green"):  "go",
    else:       "unknown"
);
```

### Loops

```
// While
define i = 0; define total = 0;
while(i < 10) { total += i; i++; };

// For
define sum = 0;
for(let j = 0, j < 5, j++) sum += j;

// For-each (iterate over a collection)
for(let x in [1, 2, 3]) display(x);    // prints 123

define total = 0;
for(let x in [10, 20, 30]) { total += x; };
total;                                   // => 60

// Do-until
define k = 0;
do k++ until(k >= 10);

// Break
while(true) {
    if(done) break;
    process();
};
```

### Let bindings

```
let(a = 1, b = 2) a + b;              // parallel binding
let*(x = 10, y = x + 5) y;            // sequential binding
letrec(f = function(n)                // recursive binding
    if(n <= 1) 1 else n * f(n - 1)
) f(10);
```

### Data structures

```
// Lists
define xs = [1, 2, 3, 4, 5];
car(xs);                  // => 1
cdr(xs);                  // => [2, 3, 4, 5]
cons(0, xs);              // => [0, 1, 2, 3, 4, 5]

// Indexing (lists, vectors, strings) — brackets or arrows
xs[0];                    // => 1
xs->first;                // => 1 (same thing)
xs[-1];                   // => 5 (negative = from end)
xs->last;                 // => 5 (same thing)
xs[1:3];                  // => [2, 3] (slice, exclusive end)
xs[:2];                   // => [1, 2]
xs[1:];                   // => [2, 3, 4, 5]
xs->rest;                 // => [2, 3, 4, 5] (same as xs[1:])

// Dotted pairs
define pair = (1 .. 2);          // => (1 . 2)
(1, 2, 3 .. 4);           // => (1 2 3 . 4)

// Vectors (random access)
define v = #[10, 20, 30];
v[1];                     // => 20

// Strings
"hello"[1];               // => #\e
"hello"[1:3];             // => "el"

// Records
record Point(x, y);
define p = make_Point(3, 4);
Point_x(p);               // => 3
Point?(p);                 // => true
```

### String methods

Strings support OO-style methods via `->`:

```
"hello"->upper();                    // => "HELLO"
"  hi  "->trim();                    // => "hi"
"hello world"->contains("world");    // => true
"hello world"->replace("world", "there");  // => "hello there"
"a,b,c"->split(",");                // => ["a", "b", "c"]
","->join(["a", "b", "c"]);         // => "a,b,c"
"hello"->length;                     // => 5

// Chaining
"  Hello World  "->trim()->upper();  // => "HELLO WORLD"
```

See [STRINGS.md](STRINGS.md) for the full list of string properties and methods.

### List and vector methods

Lists and vectors also support OO-style methods via `->`:

```
[3, 1, 2]->sort(<);                           // => [1, 2, 3]
[1, 2, 3, 4]->filter(function(x) x > 2);      // => [3, 4]
[1, 2, 3]->map(function(x) x * 2);            // => [2, 4, 6]
["a", "b", "c"]->join(",");                     // => "a,b,c"
[1, 2, 3]->contains(2);                        // => true
[1, 2, 3]->length;                              // => 3

#[1, 2, 3]->map(function(x) x * x);           // => #[1, 4, 9]
#[1, 2, 3]->to_list()->filter(function(x) x > 1);  // => [2, 3]

// Chaining
[3, 1, 4, 2]->sort(<)->take(2)->map(function(x) x * 10);  // => [10, 20]
```

See [LISTS.md](LISTS.md) and [VECTORS.md](VECTORS.md) for the full reference.

### Higher-order functions

```
map(function(x) x * 2, [1, 2, 3]);           // => [2, 4, 6]
filter(function(x) x > 2, [1, 2, 3, 4, 5]);  // => [3, 4, 5]
fold(+, 0, [1, 2, 3, 4, 5]);                  // => 15
apply(+, [3, 4]);                              // => 7
```

### Generators

Lazy sequences with `yield` — values are computed on demand:

```
define count = generator(n) {
    for(let i = 0, i < n, i++) yield i;
};
collect(count(5));  // => [0, 1, 2, 3, 4]

// Generator comprehension (lazy)
define doubled = (x * 2 for x in [1, 2, 3, 4, 5]);
collect(doubled);   // => [2, 4, 6, 8, 10]

// Lazy pipeline — no intermediate lists
define g1 = (x * 2 for x in count(100));
define g2 = (x + 1 for x in g1);
g2();  // => 1  (only computes first value)

// All comprehension types accept generators as sources
[x * x for x in count(5)];        // => [0, 1, 4, 9, 16]
dict(x: x * x for x in count(3)); // dict with 0:0, 1:1, 2:4
```

See [GENERATORS.md](GENERATORS.md) for the full guide including infinite generators, early termination, and compilation details.

### Error handling

```
try
    risky_operation()
catch(err)
    print("Error: " ++ err);
```

### Continuations

```
// Capture and invoke
callcc(function(k) k(42));       // => 42

// Save a continuation for later
e.eval("""
    define saved = false;
    define result = callcc(function(k) { saved = k; "first"; });
""")
e.eval('result;')                // => "first"
e.eval('saved("second");')      // => "second"
```

### Object-oriented programming

Eval supports OOP through closures and the constructor/interface pattern, with abstract methods and abstract classes:

```
define Point = constructor(x, y)
    interface(
        x: x,
        y: y,
        dist: function() py_sqrt(x * x + y * y)
    );

define p = Point(3, 4);
p->x;                // => 3
p->dist();           // => 5.0

// Inheritance with super
define Point3D = constructor(x, y, z) {
    super Point(x, y);
    interface(
        z: z,
        dist: function() py_sqrt(x*x + y*y + z*z)
    );
};

define p3 = Point3D(1, 2, 3);
p3->x;               // => 1 (inherited from Point)
p3->z;               // => 3
```

### Abstract methods and classes

Declare methods that must be overridden, or prevent direct instantiation of base classes:

```
// Abstract method — errors if called without override
define Shape = constructor()
    interface(area: abstract, name: function() "shape");
Shape()->area();           // ERROR: "abstract method: area"
Shape()->name();           // "shape" (works fine)

// Abstract class — prevents direct instantiation
define Shape = abstract constructor()
    interface(area: abstract, name: function() "shape");
Shape();                   // ERROR: "cannot instantiate abstract class"

// Subclass overrides abstract methods
define Circle = constructor(r) {
    super Shape();
    interface(area: function() 3.14159 * r * r);
};
Circle(5)->area();         // => 78.53975
Circle(5)->name();         // => "shape" (inherited)

// Abstract + static
define Shape = abstract constructor()
    static(types: ["circle", "rect"])
    interface(area: abstract);
Shape->types;              // ["circle", "rect"] (statics still accessible)
```

### Multiple return values

```
// Return multiple values
receive(a, b) from values(1, 2)
    a + b;                    // => 3
```

### Compile-time evaluation

```
// !! evaluates an expression at parse time
define x = !!(2 + 3);        // x is 5, computed during parsing
```

### Include

```
// Include Scheme source files
include("library.scm");
```

### Accessing Scheme primitives

Use backtick identifiers to call any chibi-scheme function:

```
"hello" ++ " " ++ "world";                // => "hello world" (or use backtick:)
`string-append`("hello", " ", "world");   // => "hello world"
`number->string`(42);                      // => "42"
`list->vector`([1, 2, 3]);                // => #[1, 2, 3]
`exact->inexact`(1);                       // => 1.0
```

## Python API

### `Eval(heap_size=0, max_heap_size=0)`

Create an interpreter instance. Optional heap size parameters control memory allocation.

### `eval(code: str) -> object`

Evaluate Eval code, return the result converted to a Python object.

### `eval_raw(code: str) -> ChibiSexp`

Evaluate Eval code, return the raw S-expression (for continuations, etc.).

### `e[name]` / `e[name] = value`

Get or set variables. Python lists, ints, floats, strings, and bools convert automatically.

### `define_function(name, func, arity=-1)`

Register a Python callable. Use `arity=-1` for variadic functions.

### `call(proc_name, *args) -> object`

Call a named Eval procedure with Python arguments.

### `load(filename)`

Load and evaluate an `.eval` source file.

### `serialize_continuation(k) -> bytes`

Serialize a captured continuation to bytes.

### `deserialize_continuation(data) -> ChibiSexp`

Restore a continuation from bytes. The continuation is callable.

## Continuations

First-class, serializable continuations — capture "the rest of the computation" and resume later:

```
// Early exit
callcc(function(exit) {
    define i = 0;
    while(i < 100) {
        if(i == 5) exit(i);
        i++;
    };
});
// => 5

// Save and resume
define saved = false;
define result = callcc(function(k) { saved = k; "first"; });
saved("second");    // result is now "second"
```

Continuations can be serialized to bytes and restored in a different process:

```python
e = Eval()
e.eval("define k = callcc(function(k) k);")
data = e.serialize_continuation(e["k"])

e2 = Eval()
k = e2.deserialize_continuation(data)
e2.eval('k("resumed");')    # resumes where captured
```

## Green threads and async/await

Cooperative green threads with fuel-based VM scheduling, plus async/await sugar:

```
// Lightweight async tasks
define a = async fib(10);
define b = async fib(12);
[await(a), await(b)];    // => [55, 144]

// SRFI-18 threads with shared state
define counter = 0;
define m = make_mutex();
define inc = function() {
    for(let i = 0, i < 50, i++) {
        mutex_lock(m);
        counter += 1;
        mutex_unlock(m);
    };
};
define t1 = make_thread(inc);
define t2 = make_thread(inc);
thread_start(t1); thread_start(t2);
thread_join(t1); thread_join(t2);
counter;    // => 100

// OO mutex with RAII — lock released automatically
define m2 = Mutex();
with(guard = m2->lock()) {
    // critical section
};
```

See [`examples/green_threads/`](examples/green_threads/) for complete demos, [THREADS.md](THREADS.md) for the full green threads and continuations guide, [MULTITHREADING.md](MULTITHREADING.md) for OO synchronization wrappers, [ASYNC.md](ASYNC.md) for the async programming guide, [NETWORKING.md](NETWORKING.md) for the networking guide, and [LISTS.md](LISTS.md)/[VECTORS.md](VECTORS.md) for collection methods.

## Reactive programming

Fine-grained reactivity with automatic dependency tracking — signals hold state, computed values derive from signals, and effects run side effects when dependencies change:

```
define count = Signal(0);
define doubled = Computed(function() count() * 2);

Effect(function() {
    display("doubled is ");
    display(doubled());
    newline();
});
// prints: doubled is 0

count->set(5);
// prints: doubled is 10
```

### Signals, Computed, Effects

```
define name = Signal("world");
define greeting = Computed(function() "Hello, " ++ name() ++ "!");

greeting();          // => "Hello, world!"
name->set("Eval");
greeting();          // => "Hello, Eval!"

// Effects react to changes automatically
Effect(function() print(greeting()));

// Batch multiple updates into a single flush
batch(function() {
    count->set(1);
    name->set("test");
});
// Effects fire once after all updates
```

### Derived utilities

```
// Watch: callback on changes with old/new values
define w = watch(count, function(new_val, old_val) {
    print("count: " ++ `number->string`(old_val) ++ " -> " ++ `number->string`(new_val));
});

// Readonly: expose a signal without write access
define public_count = readonly(count);
public_count();          // => 0  (reading works)
public_count->set(5);    // ERROR: readonly: cannot set

// Reduce: accumulate over signal changes
define total = reduce(count, function(acc, v) acc + v, 0);

// Scope: ownership group — dispose all children at once
define sc = scope(function() {
    define a = Signal(1);
    define b = Computed(function() a() * 2);
    Effect(function() display(b()));
});
dispose(sc);    // disposes a, b, and the effect
```

### Custom equality, combine, select, prev

```
// Custom equality — only propagate when truly different
define pos = Signal([0, 0], function(a, b) car(a) == car(b) && cadr(a) == cadr(b));

// Combine multiple signals
define full = combine([first, last], function(f, l) f ++ " " ++ l);

// Select a slice with fine-grained updates
define x = select(point, function(p) car(p));

// Track previous value
define p = prev(count);
```

### Async resource

Fetch data asynchronously with green threads — re-fetches when the source changes:

```
define userId = Signal(1);
define user = resource(userId, function(id) load_user(id));

user->settle();          // wait for fetch
user();                  // => user data
user->loading;           // => false

userId->set(2);          // triggers re-fetch
user->settle();
user();                  // => new user data
```

See [REACTIVE.md](REACTIVE.md) for the full reactive programming guide, [GENERATORS.md](GENERATORS.md) for generators and lazy sequences, and [INTRO.md](INTRO.md) for the complete language reference.

## Networking

TCP sockets and HTTP with OO wrappers, RAII cleanup, and reactive signals:

```
// TCP client
with(sock = TcpClient("example.com", 80)) {
    sock->send("GET / HTTP/1.0\r\nHost: example.com\r\n\r\n");
    display(sock->recv(4096));
};

// HTTP client
with(client = HttpClient("example.com", 80)) {
    define result = client->get("/");
    display(car(cdr(result)));    // response body
};

// TCP server with reactive connection tracking
define server = TcpServer(8080, function(sock, addr, port) {
    define data = sock->recv(4096);
    sock->send("HTTP/1.0 200 OK\r\nContent-Length: 2\r\n\r\nOK");
});
server->connections;    // reactive Signal
server->requests;       // reactive Signal
server->run();
```

All socket operations integrate with green threads — they yield automatically when they would block. See [NETWORKING.md](NETWORKING.md) for the full guide.

## Thread pool

True OS-level parallelism with worker threads, each running an independent chibi-scheme VM. Workers communicate through channels and futures. Closures can be serialized across thread boundaries.

### Pure-Eval API (OO)

```
// Pool with RAII — auto-shutdown on scope exit
define result = with(pool = Pool(4)) {
    define f1 = pool->submit("10 * 10;");
    define f2 = pool->submit("20 + 5;");
    await(f1) + await(f2);
};
result;    // => 125

// Channels
{
    with(pool = Pool(2));
    with(ch = pool->channel("data"));
    ch->send("hello");
    ch->recv();    // => "hello"
};

// Send closures (binary-serialized)
define square = function(x) x * x;
define pool = make_pool(2);
future_result(pool_apply(pool, square, [7]));    // => 49
pool_shutdown(pool);
```

### Python API

```python
from chibi_eval import EvalPool

with EvalPool(workers=4) as pool:
    f = pool.submit("fold(+, 0, [1, 2, 3, 4, 5]);")
    print(f.result())  # => 15

    ch = pool.channel("data")
    pool.submit('channel_send(data, 42);')
    print(ch.recv())  # => 42
```

### `EvalPool(workers=None, prelude=None)`

Create a thread pool. Defaults to `os.cpu_count()` workers. Each worker gets an independent Eval interpreter.

### `pool.submit(code) -> EvalFuture`

Submit Eval code for async execution. Returns a future.

### `pool.map(codes) -> list`

Submit multiple code strings and collect all results.

### `pool.channel(name) -> EvalChannel`

Get or create a named channel shared across all workers.

### `future.result()`

Block until the worker finishes, then return the result. Raises `EvalError` on failure.

### `channel.send(value)` / `channel.recv()` / `channel.try_recv()`

Send or receive values through a channel. `recv()` blocks until a value is available. `try_recv()` returns `(ok, value)` without blocking — `ok` is `True` if a value was received.

See [ASYNC.md](ASYNC.md) for the full async programming guide, [THREADS.md](THREADS.md) for the complete threads and continuations guide, [MULTITHREADING.md](MULTITHREADING.md) for OO synchronization wrappers, [GENERATORS.md](GENERATORS.md) for generators and lazy sequences, [NETWORKING.md](NETWORKING.md) for TCP/HTTP networking, [FILESYS.md](FILESYS.md) for filesystem operations, [LISTS.md](LISTS.md)/[VECTORS.md](VECTORS.md) for collection methods, [BINARY.md](BINARY.md) for Cap'n Proto binary serialization, [GRAMMARS.md](GRAMMARS.md) for runtime parser generation, and [TESTS.md](TESTS.md) for the built-in test framework.

## Binary serialization

Efficient binary serialization using [Cap'n Proto](https://capnproto.org/)'s dynamic API — write a schema string, build and read messages with zero-copy performance:

```javascript
define s = Schema("""
  @0xdbb9ad1f14bf0b36;
  struct Person {
    name   @0 :Text;
    age    @1 :UInt32;
    active @2 :Bool;
  }
""");

// Serialize to binary bytevector
define msg = s->Person->build("name", "Alice", "age", 30, "active", true);

// Deserialize (zero-copy)
define r = s->Person->read(msg);
r->name;      // "Alice"
r->age;       // 30
r->active;    // true

// Save to file and memory-map for instant zero-copy access
s->Person->save(msg, "person.bin");
define result = with(r = s->Person->mmap("person.bin")) {
    r->name    // reads directly from mmap'd memory — no deserialization
};
// r is automatically closed here, file is unmapped
```

Full Cap'n Proto schema syntax supported — all integer widths, floats, booleans, text, data, lists, enums, multiple structs per schema. Messages are wire-compatible with any Cap'n Proto implementation (C++, Rust, Go, Java, Python, etc.). Memory-mapped files give instant access to large datasets with zero parsing overhead.

See [BINARY.md](BINARY.md) for the full guide including all supported types, memory-mapped files, RAII cleanup, and wire compatibility details.

## Grammar JIT

Runtime parser generation — write a grammar in Lark EBNF, compile it to a native parser via LLVM JIT:

```javascript
define calc = Grammar("""
    start: expr
    ?expr: expr "+" term -> add | term
    ?term: NUMBER
    NUMBER: /[0-9]+/
    %ignore /\s+/
""");
define p = calc->compile();
p->parse("1 + 2 + 3");
// ("add" ("add" ("NUMBER" "1") ...) ...)
```

See [GRAMMARS.md](GRAMMARS.md) for the full guide including the compilation pipeline, grammar format, and examples.

## Scheme-to-Eval transpiler

The included `scm2eval` tool converts Scheme source files to Eval syntax:

```bash
# Single file
python tools/scm2eval.py input.scm -o output.eval

# Entire directory tree
python tools/scm2eval.py --dir chibi-scheme/lib/scheme/ -o chibi_eval/_lib/scheme/
```

## Examples

| Example | Description |
|---------|-------------|
| [`examples/bank/`](examples/bank/) | OOP with closures: accounts, savings, checking, bank manager |
| [`examples/green_threads/`](examples/green_threads/) | Cooperative scheduling, continuation serialization, producer-consumer pipelines |
| [`examples/objects/`](examples/objects/) | SICP-inspired object patterns: message passing, delegation, polymorphism |
| [`examples/http_server.eval`](examples/http_server.eval) | HTTP server with routing, reactive state, green threads |
| [`examples/http_client.eval`](examples/http_client.eval) | HTTP client: low-level helpers + OO `HttpClient` wrapper |
| [`examples/file_processor.eval`](examples/file_processor.eval) | Reactive file scanner with indexing, signals, computed values |

Run examples with Python:

```bash
python examples/bank/test_bank.py
python examples/green_threads/run_showcase.py
python examples/objects/test_object_model.py
```

Run the test suite with the standalone CLI:

```bash
eval tests/eval/test_arithmetic.eval
eval tests/eval/test_functions.eval
eval tests/eval/test_green_threads.eval
eval tests/eval/test_pool.eval
```

## Testing

Python tests:

```bash
pip install -e ".[test]"
pytest tests/ -v
```

Standalone tests (all `.eval` files under `tests/eval/`):

```bash
for f in tests/eval/test_*.eval; do eval "$f"; done
```

### Built-in test framework

Eval includes a lightweight test framework for writing tests in Eval itself:

```javascript
// Test group with automatic summary
test_group("arithmetic") {
    test(4, 2 + 2);                          // unnamed: compare expected vs actual
    test("addition", 4, 2 + 2);              // named
    test_assert(5 > 0);                       // truthy check
    test_assert("positive", 5 > 0);          // named truthy check
    test_error(function() 1 / 0);            // expects error (thunk)
    test_error("div-zero", function() 1 / 0); // named error check
};
// Output:
// -- arithmetic -------
//   PASS
//   PASS: addition
//   PASS
//   PASS: positive
//   PASS
//   PASS: div-zero
// -- 6 pass, 0 fail, 0 error

// Standalone tests (no group)
test(42, 6 * 7);

// Manual begin/end for fine-grained control
test_begin("suite");
test(4, 2 + 2);
test(4, 2 + 3);  // FAIL
define failures = test_end();  // prints summary, returns fail+error count
```

## How it works

```
Eval source  ──→  Lexer  ──→  Lemon LALR parser  ──→  S-expression AST  ──→  chibi-scheme VM
    x + 1;         tokens        eval_grammar.y         (+ x 1)              bytecode execution
```

1. The **lexer** (`_eval_lexer.c`) tokenizes infix Eval syntax
2. The **Lemon parser** (`eval_grammar.y`) builds S-expression ASTs using LALR parsing
3. **chibi-scheme** compiles S-expressions to bytecode and executes on its VM
4. The **bridge layer** connects Scheme I/O and functions to the host (Python or standalone CLI)

The entire chibi-scheme runtime is statically linked. The standalone binary embeds all 480 scheme library files — no external dependencies at runtime.

## License

MIT License. See [LICENSE](LICENSE).

Third-party components are included under their own licenses. See [THIRD-PARTY-NOTICES](THIRD-PARTY-NOTICES) for details:
- **chibi-scheme** — BSD-3-Clause
- **Lemon parser generator** — Public Domain
- **re2c** — Public Domain
- **LLVM/Clang** (optional) — Apache 2.0 with LLVM Exceptions
- **Cap'n Proto** (optional) — MIT License
