# Generators

Generators are lazy sequences that produce values one at a time, on demand. They are built on chibi-scheme's `call/cc` continuations — each `yield` suspends the generator, and the next call resumes it. This makes them ideal for large or infinite sequences where you don't want to materialize the entire collection in memory.

## Generator functions

Use the `generator` keyword (like `function`) to define a generator. Use `yield` inside the body to produce values:

```
define count = generator(n) {
    for(let i = 0, i < n, i++)
        yield i;
};
```

Calling a generator function returns a **generator object** — a zero-argument function that produces the next value on each call:

```
define g = count(3);
g();              // => 0
g();              // => 1
g();              // => 2
g();              // => eof (end of sequence)
g();              // => eof (stays exhausted)
```

When the generator has no more values, it returns the eof object. Use `eof?` to test for it:

```
define g = count(2);
g();              // => 0
g();              // => 1
eof?(g());        // => true
```

### Parameters

Generator functions take parameters just like `function`:

```
// No parameters
define ones = generator() {
    while(true) yield 1;
};

// Multiple parameters
define range = generator(start, stop) {
    define i = start;
    while(i < stop) {
        yield i;
        i++;
    };
};
collect(range(3, 7));  // => [3, 4, 5, 6]

// Rest parameters
define gen_all = generator(..args) {
    for(let x in args) yield x;
};
collect(gen_all(10, 20, 30));  // => [10, 20, 30]
```

### Blocks and closures

Generator bodies can contain local state, loops, and closures — anything a `function` body can:

```
define fib = generator() {
    define a = 0;
    define b = 1;
    while(true) {
        yield a;
        define temp = a + b;
        a = b;
        b = temp;
    };
};

define g = fib();
g();  // => 0
g();  // => 1
g();  // => 1
g();  // => 2
g();  // => 3
g();  // => 5
```

### Early termination with `return`

`return` inside a generator exits it immediately. Subsequent calls return eof:

```
define take_positive = generator(xs) {
    for(let x in xs) {
        if(x < 0) return nil;
        yield x;
    };
};

collect(take_positive([1, 2, 3, -1, 4, 5]));  // => [1, 2, 3]
```

## Collecting values

`collect` materializes a generator into a list:

```
define squares = generator(n) {
    for(let i = 0, i < n, i++)
        yield i * i;
};

collect(squares(5));  // => [0, 1, 4, 9, 16]
collect(squares(0));  // => []
```

You can also manually drain a generator:

```
define g = count(3);
define result = [];
define v = g();
while(!eof?(v)) {
    result = append(result, [v]);
    v = g();
};
result;  // => [0, 1, 2]
```

## Generator comprehensions

Parenthesized comprehensions create lazy generators:

```
(expr for var in source)
(expr for var in source if condition)
```

Brackets `[...]` produce a list (eager). Parentheses `(...)` produce a generator (lazy):

```
[x * 2 for x in [1, 2, 3]];            // => [2, 4, 6]        (list, computed now)
(x * 2 for x in [1, 2, 3]);            // => <generator>       (lazy, computed on demand)
collect((x * 2 for x in [1, 2, 3]));   // => [2, 4, 6]        (materialized)
```

### Filtering

```
collect((x for x in [1, 2, 3, 4, 5] if x > 2));  // => [3, 4, 5]
collect((x * x for x in [1, 2, 3, 4] if x % 2 == 0));  // => [4, 16]
```

### Nested iteration

```
collect((x + y for x in [1, 2] for y in [10, 20]));
// => [11, 21, 12, 22]

collect(([x, y] for x in [1, 2] for y in ["a", "b"]));
// => [[1, "a"], [1, "b"], [2, "a"], [2, "b"]]
```

## Lazy pipelines

Generators can feed into other generators, creating lazy pipelines where no intermediate lists are created:

```
define g1 = (x * 2 for x in [1, 2, 3, 4, 5]);
define g2 = (x + 100 for x in g1);
collect(g2);  // => [102, 104, 106, 108, 110]
```

Only the values that are actually consumed get computed:

```
define counter = 0;
define lazy = ({ counter++; x * x } for x in [1, 2, 3, 4, 5]);
lazy();           // => 1   (counter is now 1)
lazy();           // => 4   (counter is now 2)
counter;          // => 2   (only 2 values computed so far)
```

### Generator function as source

Generator comprehensions and list comprehensions accept generator functions as sources:

```
define naturals = generator() {
    define i = 0;
    while(true) { yield i; i++; };
};

// Generator comprehension from generator source
define evens = (x * 2 for x in naturals());

// List comprehension from generator source
define squares = generator(n) {
    for(let i = 0, i < n, i++)
        yield i * i;
};
[x for x in squares(5)];             // => [0, 1, 4, 9, 16]
[x for x in squares(5) if x > 5];   // => [9, 16]
```

## Generators as comprehension sources

All comprehension types accept generators as their `in` source — not just lists:

### List comprehension from generator

```
define count = generator(n) {
    for(let i = 0, i < n, i++) yield i;
};
[x * 2 for x in count(5)];              // => [0, 2, 4, 6, 8]
[x * 2 for x in count(5) if x > 2];    // => [6, 8]
```

### Vector comprehension from generator

```
#[x * x for x in count(5)];             // => #[0, 1, 4, 9, 16]
```

### Dict comprehension from generator

```
dict(x: x * x for x in count(4));
// => dict with 0: 0, 1: 1, 2: 4, 3: 9
```

### Generator comprehension from generator

```
define g1 = (x * 2 for x in count(5));
define g2 = (x + 100 for x in g1);
collect(g2);  // => [100, 102, 104, 106, 108]
```

## Infinite generators

Since generators are lazy, they can represent infinite sequences:

```
define naturals = generator() {
    define i = 0;
    while(true) { yield i; i++; };
};

define ng = naturals();
ng();  // => 0
ng();  // => 1
ng();  // => 2
// ... never ends

// Take the first 5
define g = naturals();
define first5 = [];
for(let i = 0, i < 5, i++)
    first5 = append(first5, [g()]);
first5;  // => [0, 1, 2, 3, 4]
```

Infinite generators can feed into generator comprehensions for lazy transformation:

```
define squares = (x * x for x in naturals());
define s = squares();  // doesn't compute anything yet
s();  // => 0
s();  // => 1
s();  // => 4
s();  // => 9
```

**Warning**: Don't pass an infinite generator to `collect`, a list comprehension, or any other function that consumes all values — it will loop forever.

## Comparison with comprehensions

| Syntax | Type | Evaluation | Source types |
|--------|------|-----------|--------------|
| `[expr for x in src]` | list | Eager | list, generator |
| `#[expr for x in src]` | vector | Eager | list, generator |
| `dict(k: v for x in src)` | dict | Eager | list, generator |
| `(expr for x in src)` | generator | Lazy | list, generator |

The first three produce a concrete collection immediately. The last produces a generator that computes values on demand.

## How it works

### Generator functions

`generator(params) body` compiles to:

```scheme
(lambda (params)
  (make-coroutine-generator
    (lambda (__yield__)
      (call-with-current-continuation
        (lambda (__return__)
          body)))))
```

- `yield expr` compiles to `(__yield__ expr)` — suspends the coroutine and returns `expr` to the caller
- `return expr` compiles to `(__return__ expr)` — exits the coroutine via the continuation; `make-coroutine-generator` then returns eof on subsequent calls
- `make-coroutine-generator` (defined in `eval/generator.scm`) implements the SRFI-121 coroutine pattern using two continuations: one for the outer caller and one for the inner coroutine

### Generator comprehensions

`(body for x in xs)` compiles to:

```scheme
(make-coroutine-generator
  (lambda (__yield__)
    (__gen_for_each__ (lambda (x) (__yield__ body)) xs)))
```

`__gen_for_each__` is a polymorphic iterator that handles both list and generator sources.

### Comprehension sources

List, vector, and dict comprehensions use `__comp_map__`, `__comp_filter__`, and `__comp_append_map__` — polymorphic versions of `map`, `filter`, and `append-map` that accept both lists and generators as sources.

## API reference

| Function | Description |
|----------|-------------|
| `generator(params) body` | Define a generator function |
| `yield expr` | Produce a value (inside generator body) |
| `collect(gen)` | Materialize generator into a list |
| `eof?(val)` | Test if value is end-of-sequence |
| `(expr for x in src)` | Generator comprehension |
| `(expr for x in src if cond)` | Generator comprehension with filter |

## See also

- [INTRO.md](INTRO.md) — Language introduction with generator overview
- [LISTS.md](LISTS.md) — List methods (map, filter, etc.)
- [VECTORS.md](VECTORS.md) — Vector methods
- [DICTS.md](DICTS.md) — Dict methods and dict comprehensions
