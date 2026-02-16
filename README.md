# chibi-eval

An expressive infix language with first-class functions, continuations, green threads, and seamless Python interop — powered by [chibi-scheme](https://github.com/ashinn/chibi-scheme).

```python
from chibi_eval import Eval

e = Eval()

e.eval("""
    factorial := function(n)
        if(n <= 1) 1
        else n * factorial(n - 1);

    print(factorial(20));
""")
# => 2432902008176640000
```

## Features

- **Infix syntax** — familiar C-style expressions with `:=`, `if/else`, `while`, `for`, `{ blocks }`
- **First-class functions** — closures, higher-order functions, `map`, `filter`, `fold`
- **Continuations** — `callcc`, serializable to bytes for checkpointing and migration
- **Green threads** — cooperative multitasking with fuel-based VM scheduling
- **Python interop** — call Python from Eval, call Eval from Python, share data bidirectionally
- **Full numeric tower** — integers, floats, bignums, rationals
- **Scheme power** — access any chibi-scheme primitive via backtick identifiers

## Installation

```bash
pip install chibi-eval
```

### Build from source

Requires Python 3.10+ and a C compiler (MSVC, GCC, or Clang).

```bash
pip install -e .
```

## Quick start

```python
from chibi_eval import Eval

e = Eval()

# Arithmetic
e.eval("2 ** 10;")          # => 1024

# Variables
e.eval("x := 42;")
e["x"]                       # => 42
e["greeting"] = "hello"
e.eval('greeting;')          # => 'hello'

# Functions
e.eval("""
    double := function(x) x * 2;
    double(21);
""")                         # => 42

# Lists and higher-order functions
e.eval("map(function(x) x * x, [1, 2, 3, 4, 5]);")
                             # => [1, 4, 9, 16, 25]

# Call Eval functions from Python
e.eval("add := function(a, b) a + b;")
e.call("add", 10, 20)       # => 30

# Call Python functions from Eval
import math
e.define_function("py_sqrt", math.sqrt, 1)
e.eval("py_sqrt(144);")     # => 12.0
```

## Language reference

### Variables and assignment

```
x := 42;              // define
x = 100;              // assign
x += 10;              // compound assignment (also -=, *=, /=)
x++;                  // increment
x--;                  // decrement
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
ops := [+, -, *];
car(ops);                        // => + (the function)
```

### Functions

```
// Simple
square := function(x) x * x;

// Block body
clamp := function(x, lo, hi) {
    if(x < lo) lo
    else if(x > hi) hi
    else x;
};

// Early return
find_first := function(lst, pred) {
    for_each(function(x) {
        when(pred(x)) return x;
    }, lst);
    false;
};

// Closures
make_counter := function(start) {
    n := start;
    function() { n = n + 1; n; };
};
```

### Control flow

```
// If-else
if(x > 0) "positive" else "non-positive";

// Blocks
{
    a := 10;
    b := 20;
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
```

### Loops

```
// While
i := 0; total := 0;
while(i < 10) { total += i; i++; };

// For
sum := 0;
for(j := 0, j < 5, j++) sum += j;

// Do-until
k := 0;
do k++ until(k >= 10);

// Break
while(true) {
    if(done) break;
    process();
};
```

### Let bindings

```
let(a := 1, b := 2) a + b;            // parallel binding
let*(x := 10, y := x + 5) y;          // sequential binding
letrec(f := function(n)               // recursive binding
    if(n <= 1) 1 else n * f(n - 1)
) f(10);
```

### Data structures

```
// Lists
xs := [1, 2, 3, 4, 5];
car(xs);                  // => 1
cdr(xs);                  // => [2, 3, 4, 5]
cons(0, xs);              // => [0, 1, 2, 3, 4, 5]

// Vectors (random access)
v := #[10, 20, 30];

// Records
record Point(x, y);
p := make_Point(3, 4);
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
    saved := false;
    result := callcc(function(k) { saved = k; "first"; });
""")
e.eval('result;')                // => "first"
e.eval('saved("second");')      // => "second"
```

### Object-oriented programming

Eval supports OOP through closures and the constructor/interface pattern:

```
Point := constructor(x, y)
    interface(
        x: x,
        y: y,
        dist: function() py_sqrt(x * x + y * y)
    );

p := Point(3, 4);
p->x;                // => 3
p->dist();           // => 5.0
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
    i := 0;
    while(i < 3) { print(cat("A:", i)); yield(); i++; };
});
spawn(function() {
    i := 0;
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

Run any example:

```bash
python examples/bank/test_bank.py
python examples/green_threads/run_showcase.py
python examples/objects/test_object_model.py
```

## Testing

```bash
pip install -e ".[test]"
pytest tests/ -v
```

## How it works

```
Eval source  ──→  Lexer  ──→  Lemon LALR parser  ──→  S-expression AST  ──→  chibi-scheme VM
    x + 1;         tokens        eval_grammar.y         (+ x 1)              bytecode execution
```

1. The **lexer** (`_eval_lexer.c`) tokenizes infix Eval syntax
2. The **Lemon parser** (`eval_grammar.y`) builds S-expression ASTs using LALR parsing
3. **chibi-scheme** compiles S-expressions to bytecode and executes on its VM
4. The **bridge layer** (`_chibi_bridge.c`) connects Scheme I/O and functions to Python

The entire chibi-scheme runtime is statically linked — no external dependencies at runtime.

## License

MIT License. See [LICENSE](LICENSE).

chibi-scheme is included under the BSD-3-Clause license. See [THIRD-PARTY-NOTICES](THIRD-PARTY-NOTICES).
