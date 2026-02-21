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

- **Infix syntax** — familiar C-style expressions with `define`, `let`, `if/else`, `while`, `for`, `{ blocks }`, `"""raw triple-quoted strings"""`, `f"interpolated {strings}"`
- **First-class functions** — closures, higher-order functions, `map`, `filter`, `fold`
- **Generators** — lazy sequences with `yield`, generator comprehensions, infinite streams, lazy pipelines
- **Reactive programming** — signals, computed values, effects, scopes, async resources
- **Continuations** — `callcc`, serializable to bytes for checkpointing and migration
- **Green threads** — cooperative multitasking with fuel-based VM scheduling
- **Synchronization** — OO wrappers: Mutex, Monitor, ReadWriteLock, Semaphore with RAII lock management
- **Concurrent containers** — thread-safe ConcurrentDict, ConcurrentQueue, ConcurrentStack, ConcurrentList with cross-worker sharing
- **Thread pool** — true OS-level parallelism with worker threads, futures, and channels
- **Task pool** — scalable task execution: OS thread pool + green threads, submit closures, promises
- **Hybrid async** — `async` for green threads, `parallel async` for OS threads — mix both in the same scope
- **F-strings** — `f"hello {name}, you are {age} years old"` — interpolated strings with auto type conversion
- **String methods** — OO-style `"hello"->upper()`, `->trim()`, `->split(",")`, `->contains()`, chaining
- **Collection methods** — OO-style `[1,2,3]->map(f)`, `->filter()`, `->sort(<)`, `->join(",")`, `#[1,2,3]->length`
- **Networking** — TCP sockets, HTTP client/server with OO wrappers, RAII, and reactive signals
- **Standalone CLI** — single binary, all scheme files embedded, runs anywhere with no dependencies
- **Python interop** — call Python from Eval, call Eval from Python, share data bidirectionally
- **Quantities & Units** — `Qty(100, "km") / Qty(2, "hr")` with dimensional analysis, unit conversion, SI units, currency (`Qty("19.99", "USD")` with exact Decimal arithmetic)
- **Decimal** — arbitrary-precision `Decimal("0.1")->add(Decimal("0.2"))` is exactly `0.3`
- **DateTime & Date** — `DateTime->now()`, `DateTime->parse("2026-02-20T10:30:00Z")`, timezone conversion, `Date->today()`, `TimeDelta` arithmetic
- **Full numeric tower** — integers, floats, bignums, rationals
- **Nondeterministic choice** — `amb` operator with backtracking search, `require` constraints, `amb_collect` for all solutions
- **Logic programming** — miniKanren + Prolog-style: `fact`, `rule`, `run`, `fresh`, `conde`, `===` unification, interleaving search
- **Forward-chaining rules** — Rete algorithm: `whenever` rules fire automatically when matching facts are asserted, with multi-condition joins and chaining
- **Category theory** — Maybe/Either/Validation types, Writer/Reader/State monads, `|>` pipe, `>>=` bind, `<>` mappend, `mdo` do-notation, Traversable, lenses, Kleisli composition
- **Differentiable programming** — forward-mode (dual numbers) and reverse-mode (tape-based) AD with 35 ops: arithmetic, activations (relu, sigmoid, gelu, silu), softmax, axis reductions, reshape, slice, concat, gather, layer norm, where, batch matmul, conv2d, max/avg pooling — full transformer and CNN training with optional TensorFlow graph-mode GPU acceleration
- **Binary serialization** — Cap'n Proto zero-copy serialization with runtime schema parsing, wire-compatible with any language
- **Columnar data** — Apache Arrow tables with filter, sort, group-by, join, and CSV/Parquet/IPC file I/O via `Table(dict(x: [1,2,3]))->filter("x", >, 1)`
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
| Monadic    | `\|>` (pipe)  `>>=` (bind)  `<>` (mappend) |
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

### F-strings

Interpolated strings with embedded expressions — prefix with `f` and use `{expr}`:

```
define name = "Alice";
define age = 30;
f"Hello, {name}! You are {age} years old.";
// => "Hello, Alice! You are 30 years old."

f"sum: {1 + 2 + 3}"               // => "sum: 6"
f"upper: {name->upper()}"          // => "upper: ALICE"
f"n={42}, b={true}"                // => "n=42, b=true"
f"braces: {{ and }}"               // => "braces: { and }"
```

Expressions are auto-converted to strings. Supports escape sequences, nested f-strings, and arbitrary expressions inside `{}`. See [FSTRINGS.md](FSTRINGS.md) for the full guide.

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
// Lightweight async tasks (green threads by default)
define a = async fib(10);
define b = async fib(12);
[await(a), await(b)];    // => [55, 144]

// Hybrid: green threads + OS threads in the same scope
with(ap = AsyncPool(4)) {
    define a = async light_io();              // green thread (shared state)
    define b = parallel async fib(30);        // OS thread (true parallelism)
    define c = parallel async fib(31);        // OS thread (true parallelism)
    [await(a), await(b), await(c)];           // same promise type, same await
};

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

See [`examples/green_threads/`](examples/green_threads/) for complete demos, [THREADS.md](THREADS.md) for the full green threads and continuations guide, [MULTITHREADING.md](MULTITHREADING.md) for OO synchronization wrappers, [CONCURRENT.md](CONCURRENT.md) for thread-safe containers, [ASYNC.md](ASYNC.md) for the async programming guide, [TASKS.md](TASKS.md) for the TaskPool guide, [NETWORKING.md](NETWORKING.md) for the networking guide, and [LISTS.md](LISTS.md)/[VECTORS.md](VECTORS.md) for collection methods.

## Concurrent containers

Thread-safe data structures that work across green threads and OS thread pool workers:

```
// Dict — thread-safe key-value store
define d = ConcurrentDict();
d->set("x", 42);
d->get("x");              // => 42

// Queue — FIFO with blocking pop
define q = ConcurrentQueue();
q->push(1); q->push(2);
q->pop();                  // => 1

// Stack — LIFO with blocking pop
define s = ConcurrentStack();
s->push(1); s->push(2);
s->pop();                  // => 2

// List — concurrent dynamic array
define l = ConcurrentList();
l->append(10); l->append(20);
l->ref(0);                 // => 10

// Named containers share across pool workers
with(pool = TaskPool(4)) {
    define d = ConcurrentDict("shared");
    d->set("total", 0);
    pool->map([1, 2, 3, 4], function(x) {
        define d = ConcurrentDict("shared");
        d->set(`number->string`(x), x * x);
    });
    d->size();             // => 5
};
```

All containers support RAII via `with`, non-blocking variants (`try_pop`, `try_push`), and green-thread-aware blocking. See [CONCURRENT.md](CONCURRENT.md) for the full guide.

## Quantities and units

Dimensional analysis with automatic unit tracking, conversion, and dimensional safety:

```
// Physical quantities with unit arithmetic
define d = Qty(100, "km");
define t = Qty(2, "hr");
define speed = d / t;                                  // Qty(50, "km/hr")
speed->to("m/s");                                      // Qty(13.889, "m/s")

// Dimensional safety
Qty(5, "m") + Qty(3, "m");                            // Qty(8, "m")
Qty(5, "m") + Qty(3, "s");                            // ERROR: incompatible dimensions

// Currency — auto-Decimal, cross-currency protection
define price = Qty("19.99", "USD");
price + Qty("1.50", "USD");                            // Qty(21.49, "USD")
price + Qty(5, "EUR");                                 // ERROR: incompatible dimensions
```

SI units (length, mass, time, etc.), derived units (newton, joule, watt), and 10 currencies built in. See [UNITS.md](UNITS.md) for the full guide.

## Decimals, dates, and time

Exact decimal arithmetic and date/time handling:

```
// Decimal: arbitrary precision, no floating-point errors
Decimal("0.1")->add(Decimal("0.2"))->to_string();   // "0.3" (exact!)
Decimal("10")->div(Decimal("3"), 6)->to_string();    // "3.333333"

// DateTime: timestamps with timezone support
define now = DateTime->now();
define dt = DateTime->parse("2026-02-20T10:30:00Z");
dt->format("%Y-%m-%d");                               // "2026-02-20"
dt->add(TimeDelta(3600))->iso();                       // +1 hour

// Date: calendar dates
Date(2026, 2, 20)->add_days(7)->format("%B %d, %Y");  // "February 27, 2026"
```

All types integrate with f-strings: `f"total: {Decimal("1.23")}"`. See [DECIMALSANDDATES.md](DECIMALSANDDATES.md) for the full guide.

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

### TaskPool: Scalable Task Execution

`TaskPool` combines the thread pool with green threads — submit closures directly (no code strings), distribute work round-robin, and collect results via promises:

```
with(pool = TaskPool(4)) {
    pool->map([1, 2, 3, 4, 5, 6, 7, 8], function(x) x * x);
    // => [1, 4, 9, 16, 25, 36, 49, 64]
};
```

Each worker runs green threads, so multiple tasks on the same worker execute concurrently. See [TASKS.md](TASKS.md) for the full guide.

See [ASYNC.md](ASYNC.md) for the full async programming guide, [TASKS.md](TASKS.md) for the TaskPool guide, [THREADS.md](THREADS.md) for the complete threads and continuations guide, [MULTITHREADING.md](MULTITHREADING.md) for OO synchronization wrappers, [CONCURRENT.md](CONCURRENT.md) for thread-safe containers, [GENERATORS.md](GENERATORS.md) for generators and lazy sequences, [FSTRINGS.md](FSTRINGS.md) for interpolated strings, [UNITS.md](UNITS.md) for quantities/units/currency, [DECIMALSANDDATES.md](DECIMALSANDDATES.md) for decimals/dates, [NETWORKING.md](NETWORKING.md) for TCP/HTTP networking, [FILESYS.md](FILESYS.md) for filesystem operations, [LISTS.md](LISTS.md)/[VECTORS.md](VECTORS.md) for collection methods, [CATEGORY.md](CATEGORY.md) for category theory (Maybe, Either, monads, lenses), [BINARY.md](BINARY.md) for Cap'n Proto binary serialization, [ARROW.md](ARROW.md) for Apache Arrow columnar data, [GRAMMARS.md](GRAMMARS.md) for runtime parser generation, [AMB.md](AMB.md) for nondeterministic choice, [PROLOG.md](PROLOG.md) for logic programming, [RETE.md](RETE.md) for forward-chaining rules, [TESTS.md](TESTS.md) for the built-in test framework, and [DIFFERENTIAL.md](DIFFERENTIAL.md) for differentiable programming (AD, tensors, TF backend).

## Category theory

Maybe, Either, Validation, Writer, Reader, State monads — with pipe, bind, mappend operators and do-notation:

```
// Pipe: left-to-right composition
5 |> Some;                                   // Some(5)
[3,1,2] |> function(xs) xs->sort(<);        // [1, 2, 3]

// Maybe: safe optional values
Some(5)->map(function(x) x * 2)->unwrap;    // 10
None->unwrap_or(0);                          // 0

// Bind: chain computations that may fail
Some(5) >>= function(x) Some(x + 1);        // Some(6)
None >>= function(x) Some(x + 1);           // None

// List monad: nondeterministic computation
[1,2,3] >>= function(x) [x, x*10];         // [1, 10, 2, 20, 3, 30]

// Mappend: monoidal combine
"hello" <> " " <> "world";                  // "hello world"
[1, 2] <> [3, 4];                            // [1, 2, 3, 4]
Some(3) <> Some(4);                          // Some(7)

// Do-notation with mdo
mdo {
  x <~ Some(5);
  y <~ Some(3);
  Some(x + y);
};
// => Some(8)

// Writer monad: computation with logging
define w = mdo {
  x <~ Writer(5, []);
  tell("computed x");
  y <~ Writer(x + 1, []);
  tell("computed y");
  Writer(x + y, []);
};
w->value;  // 11
w->log;    // ["computed x", "computed y"]

// State monad: stateful computation without mutation
define counter = mdo {
  modify_state(function(s) s + 1);
  modify_state(function(s) s + 1);
  x <~ get_state();
  State(function(s) [x, s]);
};
run_state(counter, 0);  // [2, 2]

// Validation: accumulate all errors
lift_v2(function(a, b) a,
  Invalid(["too short"]),
  Invalid(["missing @"])
)->errors;
// => ["too short", "missing @"]

// Traversable: flip layers
sequence([Some(1), Some(2), Some(3)])->unwrap;   // [1, 2, 3]
sequence([Some(1), None, Some(3)])->is_none;      // true

// Lenses: composable getters/setters
define second = index_lens(1);
second->get([10, 20, 30]);       // 20
second->set([10, 20, 30], 99);  // [10, 99, 30]
```

See [CATEGORY.md](CATEGORY.md) for the full guide including Reader monad, natural transformations, Kleisli composition, and lens composition.

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

## Arrow columnar data

[Apache Arrow](https://arrow.apache.org/) tables for analytics — create tables from dicts, filter/sort/group/join with vectorized compute, and read/write CSV, Parquet, and Arrow IPC:

```javascript
// Create a table from column vectors
define t = Table(dict(
    name:   ["Alice", "Bob", "Charlie", "Diana"],
    dept:   ["eng", "sales", "eng", "sales"],
    salary: [120, 80, 110, 90]
));

// Filter → sort → top result (chained operations)
t->filter("salary", >, 100)->sort("salary", "desc")->head(1)
  ->column("name")->ref(0);    // => "Alice"

// Group-by aggregation
t->group_by("dept")->mean("salary");    // mean salary per department

// Column stats
t->column("salary")->sum;     // => 400
t->column("salary")->mean;    // => 100.0

// File I/O — CSV, Parquet, Arrow IPC
t->to_parquet("data.parquet");
define t2 = read_parquet("data.parquet");

// Join two tables
define reviews = Table(dict(name: ["Alice", "Bob"], score: [92, 78]));
t->join(reviews, "name");    // inner join on name
```

Tables support `->filter`, `->sort`, `->head`, `->tail`, `->slice`, `->select`, `->drop`, `->rename`, `->join`, `->group_by`, and file I/O (`->to_csv`, `->to_parquet`, `->to_ipc`). Arrays (columns) support `->sum`, `->mean`, `->min`, `->max`, `->ref`, `->to_list`, `->unique`, and `->sort`.

See [ARROW.md](ARROW.md) for the full guide including all table/array operations, group-by aggregations, file format details, and the complete API reference.

## Differentiable programming

Forward-mode (dual numbers) and reverse-mode (tape-based) automatic differentiation with optional TensorFlow GPU acceleration:

```javascript
// Reverse-mode: tensors with automatic gradients
param W = [[0.1, 0.2], [0.3, 0.4]];
param x = [[1.0], [2.0]];
define y = W @ x;
define loss = sum(y);
backward(loss);
W->grad;           // => [[1.0, 2.0], [1.0, 2.0]]

// Forward-mode: composable higher-order derivatives
define f = function(x) x * x + 2.0 * x;
grad(f)(3.0);      // => 8.0  (first derivative)
grad(grad(f))(3.0) // => 2.0  (second derivative)

// Transformer building blocks — all differentiable
define scores = Q @ transpose(K) / sqrt(d_k);
define attn = softmax(scores);                    // axis-aware, numerically stable
define normed = layer_norm(input, gamma, beta);   // fused layer normalization
define activated = gelu(projected);                // GELU activation
define embedded = gather(embeddings, token_ids);   // differentiable embedding lookup

// CNN building blocks
define features = conv2d(input, kernel, 1, 1);    // stride=1, pad=1 (same)
define pooled = max_pool2d(features, 2);           // 2x2 max pooling
```

35 differentiable ops including arithmetic, activations (relu, sigmoid, gelu, silu), softmax, axis-aware sum/mean, reshape, slice, concat, gather, layer norm, where, batch matmul, conv2d, and max/avg pooling. When TensorFlow's C library is available, the backward pass compiles to a TF computation graph for op fusion and GPU acceleration — fully transparent to user code.

See [DIFFERENTIAL.md](DIFFERENTIAL.md) for the full guide including examples (linear regression, MLP, transformer attention, YOLO detection head, CNN, RNN), the TF backend architecture, and the complete op reference.

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

## Nondeterministic choice (amb)

`amb` picks from a set of alternatives and backtracks automatically when a constraint fails. Powered by continuations, with lazy evaluation of alternatives:

```
// Backtracking search
define x = amb(1, 2, 3, 4, 5);
define y = amb(1, 2, 3, 4, 5);
require(x + y == 7);
[x, y];    // => [2, 5]

// Collect all solutions
amb_collect(function() {
    define x = amb(1, 2, 3);
    define y = amb(1, 2, 3);
    require(x + y == 4);
    [x, y];
});
// => [[1, 3], [2, 2], [3, 1]]
```

`amb` is a macro — alternatives are lazily evaluated (untried branches never execute). `require(pred)` fails if `pred` is false, triggering backtracking. `amb()` with no arguments fails immediately.

See [AMB.md](AMB.md) for the full guide including examples, amb_collect, and comparison with logic programming.

## Logic programming

Built-in miniKanren + Prolog-style logic programming with unification, facts, rules, and interleaving search:

```
// Declare facts
fact parent("tom", "bob");
fact parent("bob", "ann");
fact parent("bob", "pat");

// Define recursive rules
rule ancestor(?x, ?y) :- parent(?x, ?y);
rule ancestor(?x, ?y) :- fresh(?z) { parent(?x, ?z), ancestor(?z, ?y) };

// Search for solutions
run(*, ?q) { ancestor(?q, "ann") };
// => ["bob", "tom"]

// Disjunction with conde
run(*, ?q) conde {
    { ?q === 1 },
    { ?q === 2 },
    { ?q === 3 }
};
// => [1, 2, 3]

// Fresh variables and structural unification
run(1, ?q) {
    fresh(?x, ?y) {
        ?x === "hello",
        ?y === "world",
        ?q === [?x, ?y]
    }
};
// => [["hello", "world"]]
```

Logic variables use `?` prefix (`?x`, `?name`). `===` is the unification operator (distinct from `==` equality). `run(n, ?q)` finds up to `n` solutions (or `*` for all). `fresh` introduces new logic variables. `conde` tries multiple alternatives with fair interleaving.

See [PROLOG.md](PROLOG.md) for the full guide including facts, rules, recursive relations, and the runtime API.

## Forward-chaining rules (Rete)

`whenever` defines rules that fire automatically when matching facts are asserted — the complement to backward-chaining queries:

```
// Single condition: fires on every new parent fact
whenever parent(?x, ?y) {
    display(f"{?x} is parent of {?y}");
    newline();
}

// Multi-condition join: fires when shared variable ?y matches across facts
whenever parent(?x, ?y), parent(?y, ?z) {
    display(f"{?x} is grandparent of {?z}");
    newline();
}

fact parent("tom", "bob");     // fires rule 1
fact parent("bob", "ann");     // fires rule 1 AND rule 2 (join on ?y="bob")
// => tom is parent of bob
// => bob is parent of ann
// => tom is grandparent of ann
```

Supports constant filtering (`whenever parent("tom", ?child) { ... }`), three+ condition joins, late rule activation (rules added after facts), and chaining (rule actions that assert new facts trigger further rules).

See [RETE.md](RETE.md) for the full guide including the Rete algorithm, joins, chaining, and management functions.

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
eval tests/eval/test_taskpool.eval
eval tests/eval/test_logic.eval
eval tests/eval/test_amb.eval
eval tests/eval/test_rete.eval
eval tests/eval/test_arrow.eval
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

The entire chibi-scheme runtime is statically linked. The standalone binary embeds all 498 scheme library files — no external dependencies at runtime.

## License

MIT License. See [LICENSE](LICENSE).

Third-party components are included under their own licenses. See [THIRD-PARTY-NOTICES](THIRD-PARTY-NOTICES) for details:
- **chibi-scheme** — BSD-3-Clause
- **Lemon parser generator** — Public Domain
- **re2c** — Public Domain
- **LLVM/Clang** (optional) — Apache 2.0 with LLVM Exceptions
- **Cap'n Proto** (optional) — MIT License
- **Apache Arrow** (optional) — Apache 2.0
