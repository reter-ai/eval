# chibi-eval

An expressive infix language with first-class functions, continuations, green threads, and a thread pool — powered by [chibi-scheme](https://github.com/ashinn/chibi-scheme).

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

- **Infix syntax** — familiar C-style expressions with `define`, `let`, `if/else`, `while`, `for`, `{ blocks }`
- **First-class functions** — closures, higher-order functions, `map`, `filter`, `fold`
- **Continuations** — `callcc`, serializable to bytes for checkpointing and migration
- **Green threads** — cooperative multitasking with fuel-based VM scheduling
- **Thread pool** — true OS-level parallelism with worker threads, futures, and channels
- **Standalone CLI** — single binary, all scheme files embedded, runs anywhere with no dependencies
- **Python interop** — call Python from Eval, call Eval from Python, share data bidirectionally
- **Full numeric tower** — integers, floats, bignums, rationals
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
| Comparison | `==`  `!=`  `<`  `>`  `<=`  `>=`  |
| Logical    | `&&`  `\|\|`  `!`                  |
| Bitwise    | `&`  `\|`  `~`  `<<`  `>>`        |

Operators are first-class values — pass them to higher-order functions:

```
fold(+, 0, [1, 2, 3, 4, 5]);    // => 15
apply(*, [3, 4, 5]);             // => 60
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

// Dotted pairs
define pair = (1 .. 2);          // => (1 . 2)
(1, 2, 3 .. 4);           // => (1 2 3 . 4)

// Vectors (random access)
define v = #[10, 20, 30];

// Records
record Point(x, y);
define p = make_Point(3, 4);
Point_x(p);               // => 3
Point?(p);                 // => true
```

### Higher-order functions

```
map(function(x) x * 2, [1, 2, 3]);           // => [2, 4, 6]
filter(function(x) x > 2, [1, 2, 3, 4, 5]);  // => [3, 4, 5]
fold(+, 0, [1, 2, 3, 4, 5]);                  // => 15
apply(+, [3, 4]);                              // => 7
```

### Error handling

```
try
    risky_operation()
catch(err)
    print(cat("Error: ", err));
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

Eval supports OOP through closures and the constructor/interface pattern:

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

## Green threads

Eval includes cooperative green threads powered by chibi-scheme's fuel-based VM scheduler. Threads yield control after a quantum of VM instructions, enabling concurrent programming with continuations:

```
// Cooperative scheduler
spawn(function() {
    define i = 0;
    while(i < 3) { print(cat("A:", i)); yield(); i++; };
});
spawn(function() {
    define i = 0;
    while(i < 3) { print(cat("B:", i)); yield(); i++; };
});
run();
// Output interleaved: A:0, B:0, A:1, B:1, A:2, B:2
```

Continuations can be frozen mid-execution, serialized to bytes, and resumed later — even in a different process:

```python
e = Eval()
e.eval("...")  # capture continuation mid-thread
data = e.serialize_continuation(e["frozen_k"])

# Later, possibly different process:
e2 = Eval()
k = e2.deserialize_continuation(data)
e2.eval("k();")  # resumes exactly where it left off
```

See [`examples/green_threads/`](examples/green_threads/) for complete demos.

## Thread pool

`EvalPool` provides true OS-level parallelism with a pool of worker threads, each running an independent chibi-scheme VM. Workers communicate through named channels.

```python
from chibi_eval import EvalPool

with EvalPool(workers=4) as pool:
    # Submit code for async execution — returns a future
    f = pool.submit("define fib = function(n) if(n <= 1) n else fib(n-1) + fib(n-2); fib(30);")
    print(f.result())  # => 832040

    # Map over multiple tasks in parallel
    results = pool.map([
        "2 ** 10;",
        "fold(+, 0, [1, 2, 3, 4, 5]);",
        "map(function(x) x * x, [1, 2, 3]);",
    ])
    print(results)  # => [1024, 15, [1, 4, 9]]
```

### Channels

Named channels allow workers to send and receive values. Data is automatically serialized across thread boundaries.

```python
with EvalPool(workers=3) as pool:
    ch = pool.channel("results")

    # Worker sends to channel
    pool.submit('channel_send(results, 42);')

    # Python receives from channel
    print(ch.recv())  # => 42

    # Python sends, worker receives
    input_ch = pool.channel("input")
    input_ch.send(100)
    f = pool.submit("define x = channel_recv(input); x * 2;")
    print(f.result())  # => 200
```

### Producer-consumer pipeline

```python
with EvalPool(workers=3) as pool:
    pipe = pool.channel("pipe")
    out = pool.channel("out")

    # Producer: generate values
    pool.submit("""
        define i = 0;
        while(i < 5) { i++; channel_send(pipe, i * i); };
    """)

    # Consumer: collect and sum
    pool.submit("""
        define total = 0; define count = 0; define val = 0;
        while(count < 5) { val = channel_recv(pipe); total += val; count++; };
        channel_send(out, total);
    """)

    print(out.recv())  # => 55 (1 + 4 + 9 + 16 + 25)
```

### Pure-Eval pool API

Pools can be created and used entirely from Eval code — no Python required:

```
define pool = make_pool(4);
define f = pool_submit(pool, "define fib = function(n) if(n <= 1) n else fib(n-1) + fib(n-2); fib(30);");
future_result(f);            // => 832040
pool_shutdown(pool);
```

Submit closures directly to workers with `pool_apply` — the function and arguments are binary-serialized across thread boundaries (no code strings):

```
define pool = make_pool(4);

define square = function(x) x * x;
define f = pool_apply(pool, square, [7]);
future_result(f);            // => 49

// Closures with captured state work too
define make_adder = function(n) function(x) n + x;
define add100 = make_adder(100);
future_result(pool_apply(pool, add100, [23]));   // => 123

// Workers can return closures back
define multiplier = function(n) function(x) n * x;
define triple = future_result(pool_apply(pool, multiplier, [3]));
triple(10);                  // => 30

pool_shutdown(pool);
```

Channels work across the main context and workers:

```
define pool = make_pool(2);
define ch = pool_channel(pool, "data");

// Send from main, receive in worker
channel_send(ch, 42);
define f = pool_submit(pool, "define x = channel_recv(data); x * 2;");
future_result(f);            // => 84

// Non-blocking receive
channel_try_recv(ch);        // => false (empty)
channel_send(ch, 99);
channel_try_recv(ch);        // => (99) — one-element list

// Check future without blocking
define f2 = pool_submit(pool, "1 + 1;");
future_ready?(f2);           // => true or false

pool_shutdown(pool);
```

| Function | Description |
|----------|-------------|
| `make_pool(n)` | Create a pool with `n` worker threads |
| `pool_submit(pool, code)` | Submit code string, returns a future |
| `pool_apply(pool, fn, args)` | Submit a function with args (binary serialized), returns a future |
| `pool_channel(pool, name)` | Get or create a named channel |
| `future_result(f)` | Block until done, return result |
| `future_ready?(f)` | Non-blocking: `true` if done, `false` otherwise |
| `channel_send(ch, val)` | Send a value to a channel |
| `channel_recv(ch)` | Blocking receive from a channel |
| `channel_try_recv(ch)` | Non-blocking: `(value)` list or `false` |
| `channel_close(ch)` | Close a channel |
| `pool_shutdown(pool)` | Shut down all workers and free the pool |

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

Send or receive values through a channel. `recv()` blocks until a value is available. `try_recv()` returns `(ok, value)` without blocking — `ok` is `True` if a value was received. From Eval code, use `channel_send(ch, value)`, `channel_recv(ch)`, and `channel_try_recv(ch)`.

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

chibi-scheme is included under the BSD-3-Clause license. See [THIRD-PARTY-NOTICES](THIRD-PARTY-NOTICES).
