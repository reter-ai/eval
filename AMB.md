# Nondeterministic Choice (amb)

Eval includes `amb`, a classic nondeterministic choice operator from the Scheme/Lisp tradition. `amb` picks from a set of alternatives and backtracks automatically when a constraint fails, trying the next option until a solution is found (or all choices are exhausted).

```
define x = amb(1, 2, 3, 4, 5);
define y = amb(1, 2, 3, 4, 5);
require(x + y == 7);
display(x); display(" "); display(y); newline();
// => 2 5
```

## Concepts

`amb` enables **backtracking search**: you make a nondeterministic choice, proceed with computation, and if a later constraint fails, execution automatically rewinds to the most recent choice point and tries the next alternative. This is powered by first-class continuations under the hood.

The key primitives:

| Form | Description |
|------|-------------|
| `amb(a, b, c, ...)` | Choose one of the alternatives |
| `amb()` | Fail immediately (no choices) |
| `require(pred)` | Fail unless `pred` is true |
| `amb_collect(thunk)` | Collect all solutions |

## Basic Usage

### Making Choices

`amb` returns one of its arguments. If later computation fails (via `require` or `amb()`), it backtracks and tries the next one:

```
amb(1, 2, 3);              // => 1 (first choice)

// With backtracking
define x = amb(1, 2, 3);
require(x > 1);
x;                          // => 2 (first that satisfies x > 1)
```

### Constraints with `require`

`require(pred)` succeeds if `pred` is true, otherwise backtracks:

```
define x = amb(1, 2, 3, 4, 5);
require(x % 2 == 0);
x;
// => 2 (first even number)
```

### Multiple Choice Points

When multiple `amb` calls are in scope, backtracking explores the full combinatorial space:

```
define x = amb(1, 2, 3);
define y = amb(1, 2, 3);
require(x + y == 4);
[x, y];
// => [1, 3] (first pair summing to 4)
```

### Failure

`amb()` with no arguments fails immediately, triggering backtracking. If there are no remaining choices anywhere, it raises an error:

```
amb();    // ERROR: "amb: no more choices"
```

## Lazy Evaluation

`amb` is implemented as a Scheme macro, so alternatives are **not evaluated** until they are actually tried. This means side effects or errors in untried branches never occur:

```
// Only the first alternative is evaluated
amb(1, 1/0);              // => 1 (no division-by-zero error)

// Error in second branch — never reached
amb("safe", error("boom"));  // => "safe"
```

This is essential for correctness: if `amb` were a regular function, all arguments would be evaluated eagerly, defeating the purpose of nondeterministic choice.

## Collecting All Solutions

`amb_collect` (or `amb-collect`) takes a zero-argument function and returns a list of all values that succeed:

```
amb_collect(function() {
    define x = amb(1, 2, 3);
    define y = amb(1, 2, 3);
    require(x + y == 4);
    [x, y];
});
// => [[1, 3], [2, 2], [3, 1]]
```

The thunk is called repeatedly, and each successful return value is collected. When all choices are exhausted, the list is returned.

## Examples

### Pythagorean Triples

Find integer right triangles with sides up to 20:

```
amb_collect(function() {
    define a = amb(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20);
    define b = amb(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20);
    define c = amb(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20);
    require(a <= b);
    require(b <= c);
    require(a * a + b * b == c * c);
    [a, b, c];
});
// => [[3, 4, 5], [5, 12, 13], [6, 8, 10], [8, 15, 17], [9, 12, 15], [12, 16, 20]]
```

### Constraint Satisfaction

Find digits where SEND + MORE = MONEY:

```
// Simplified: find a, b such that a * 10 + b == 42
define a = amb(1, 2, 3, 4, 5, 6, 7, 8, 9);
define b = amb(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
require(a * 10 + b == 42);
[a, b];
// => [4, 2]
```

### SAT-style Search

```
define a = amb(true, false);
define b = amb(true, false);
define c = amb(true, false);
require(a || b);
require(b || c);
require(!a || !c);
[a, b, c];
// => [true, true, false]
```

### Generating Permutations

```
amb_collect(function() {
    define a = amb(1, 2, 3);
    define b = amb(1, 2, 3);
    define c = amb(1, 2, 3);
    require(a != b);
    require(b != c);
    require(a != c);
    [a, b, c];
});
// => [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]
```

## How It Works

### Compilation

`amb` is a `define-syntax` macro (syntax-rules). The Eval parser compiles `amb(a, b, c)` to the s-expression `(amb a b c)`, and the macro expands it without evaluating the alternatives:

| Eval syntax | Expands to |
|---|---|
| `amb()` | `(__amb_fail__)` |
| `amb(x)` | `x` |
| `amb(x, y, z)` | `(let ((saved __amb_fail__)) (call/cc (lambda (k) (set! __amb_fail__ (lambda () (set! __amb_fail__ saved) (k (amb y z)))) x)))` |
| `require(pred)` | `(if (not pred) (__amb_fail__))` |
| `amb_collect(thunk)` | Function call (not a macro) |

### Backtracking Mechanism

`amb` uses a global failure continuation `__amb_fail__`:

1. **On `amb(a, b, c)`**: Save the current `__amb_fail__`, capture a continuation `k`, set `__amb_fail__` to a thunk that restores the saved failure and calls `k` with the next alternative. Return the first alternative.

2. **On `require(false)`**: Call `__amb_fail__`, which jumps back to the most recent `amb`'s continuation, trying the next alternative.

3. **On exhaustion**: When all alternatives of an `amb` are tried, the saved `__amb_fail__` from before that `amb` is restored. If no more choice points remain, the initial `__amb_fail__` raises an error.

### amb_collect

`amb_collect` works by:
1. Saving the current `__amb_fail__`
2. Capturing an exit continuation
3. Setting `__amb_fail__` to a thunk that restores and exits with the collected results
4. Repeatedly calling the thunk, accumulating each return value
5. On each return, forcing `__amb_fail__` to try the next alternative

## amb vs Logic Programming

Both `amb` and [logic programming](PROLOG.md) support search, but they differ:

| | `amb` | Logic programming |
|---|---|---|
| **Style** | Imperative — choose, compute, constrain | Declarative — state relations |
| **Variables** | Regular Eval variables | Logic variables (`?x`) |
| **Constraints** | `require(pred)` — any boolean | `===` — structural unification |
| **Search** | Depth-first (backtracking) | Interleaving (fair) |
| **Use case** | Puzzles, constraint satisfaction, enumeration | Relational queries, pattern matching |

Use `amb` when you want to write imperative code that "just tries everything". Use logic programming when you want to describe relationships declaratively.

## API Reference

### `amb(alternatives...)`

Macro. Choose one of the alternatives. If later computation fails, backtrack and try the next. With zero arguments, fail immediately.

### `require(pred)`

Function. If `pred` is false, trigger backtracking. Otherwise, continue.

### `amb_collect(thunk)` / `amb-collect(thunk)`

Function. Call `thunk` (a zero-argument function) repeatedly, collecting all values that succeed before all choices are exhausted. Returns a list.

## See Also

- [PROLOG.md](PROLOG.md) — Logic programming: facts, rules, unification, interleaving search
- [INTRO.md](INTRO.md) — Complete language reference
- [README.md](README.md) — Project overview and quick start
