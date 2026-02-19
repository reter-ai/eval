# Test Framework

Eval includes a lightweight built-in test framework for writing tests in Eval itself. No imports or modules needed — the test functions are available immediately.

## Quick Start

```
test_begin("Arithmetic");

test("addition", 4, 2 + 2);
test("multiply", 42, 6 * 7);
test_assert("positive", 5 > 0);
test_error("div by zero", function() 1 / 0);

test_end();
```

Output:

```
-- Arithmetic -------
  PASS: addition
  PASS: multiply
  PASS: positive
  PASS: div by zero
-- 4 pass, 0 fail, 0 error
```

## Test Functions

### `test` — check equality

Compares an expected value against an actual value using structural equality (`equal?`). Float comparisons use a tolerance of `1e-10`.

```
test(expected, actual)              // unnamed
test(name, expected, actual)        // named
```

On pass:

```
  PASS: name
```

On fail:

```
  FAIL: name
    expected: 30
    actual:   11
```

Examples:

```
test("add", 30, 10 + 20);
test("list eq", [1, 2, 3], [1, 2, 3]);
test("float", 3.14, 3.14);
test(42, 6 * 7);                   // unnamed — just expected and actual
```

### `test_assert` — check truthiness

Passes if the expression is truthy (anything other than `false`).

```
test_assert(expr)                   // unnamed
test_assert(name, expr)             // named
```

On fail:

```
  FAIL: name
    value was false
```

Examples:

```
test_assert("positive", 5 > 0);
test_assert("list not empty", !([1, 2]->empty?));
test_assert(10 > 5);               // unnamed
```

### `test_error` — check that an error is raised

Passes if calling the thunk raises an error. The thunk must be a zero-argument function.

```
test_error(thunk)                   // unnamed
test_error(name, thunk)             // named
```

On fail (no error was raised):

```
  FAIL: name
    expected error but got none
```

Examples:

```
test_error("div by zero", function() 1 / 0);
test_error("undefined var", function() undefined_xyz);
test_error("explicit", function() error("boom"));
test_error(function() 1 / 0);      // unnamed
```

## Grouping Tests

### `test_begin` / `test_end` — manual group

`test_begin(name)` starts a test group: prints a header and resets all counters to zero. `test_end()` prints the summary and returns the number of failures + errors (0 means all passed).

```
test_begin("Functions");

test("closure", 42, make_adder(32)(10));
test("recursive", 3628800, factorial(10));

test_end();
```

Output:

```
-- Functions -------
  PASS: closure
  PASS: recursive
-- 2 pass, 0 fail, 0 error
```

The return value of `test_end()` is the failure count. When `test_end()` is the last expression in a `.eval` file, it becomes the file's exit status — `0` for success, non-zero for failure. This is how the test runner detects failures.

### `test_group` — automatic group

`test_group` is a language keyword that combines `test_begin`, body execution, and `test_end` in one form. It also catches uncaught exceptions inside the group body, reporting them as errors instead of crashing the entire test file.

```
test_group("Arithmetic") {
    test("add", 4, 2 + 2);
    test("mul", 42, 6 * 7);
};
```

This is equivalent to:

```
test_begin("Arithmetic");
// ... body with exception handling ...
test_end();
```

If an uncaught exception occurs inside the group body, it is caught and reported:

```
-- Arithmetic -------
  PASS: add
  ERROR: uncaught exception: some error message
-- 1 pass, 0 fail, 1 error
```

`test_group` returns the failure count from `test_end()`.

## Writing Test Files

Test files are `.eval` files placed in `tests/eval/`. Each file follows this pattern:

```
test_begin("Group Name");

// setup
define double = function(x) x * 2;

// tests
test("simple", 42, double(21));
test("zero", 0, double(0));
test_assert("positive", double(5) > 0);
test_error("type error", function() double("hello"));

test_end();
```

The last expression must be `test_end()` — its return value (0 or non-zero) determines whether the file passes or fails.

### Naming conventions

- File names: `test_<topic>.eval` (e.g. `test_arithmetic.eval`, `test_functions.eval`)
- Group names: descriptive, capitalized (e.g. `"Arithmetic"`, `"Error Handling"`)
- Test names: short, lowercase, describing what is tested (e.g. `"add"`, `"closure"`, `"div by zero"`)

### Using setup code

Define helper functions and variables between `test_begin` and the tests. All Eval features are available — functions, closures, records, interfaces, etc.:

```
test_begin("Objects");

define Point = constructor(x, y)
    interface(
        x: function() x,
        y: function() y,
        dist: function() (x**2 + y**2) ** 0.5
    );

define p = Point(3, 4);
test("x field", 3, p->x());
test("y field", 4, p->y());
test("distance", 5.0, p->dist());

test_end();
```

### Testing with blocks

Use block bodies `{ ... }` for tests that need multi-step setup:

```
test("block scope", 30, {
    define a = 10;
    define b = 20;
    a + b;
});
```

### Organizing large test files

Use comments to separate sections visually:

```
test_begin("Collection OO Methods");

// ============================================================
// LIST PROPERTIES
// ============================================================
test("list length", 3, [1, 2, 3]->length);
test("list first", 1, [1, 2, 3]->first);

// ============================================================
// LIST METHODS - transformation
// ============================================================
test("list map", [2, 4, 6], [1, 2, 3]->map(function(x) x * 2));
test("list filter", [2, 4], [1, 2, 3, 4]->filter(function(x) x % 2 == 0));

test_end();
```

## Running Tests

### With pytest (Python)

The test runner in `tests/test_eval_files.py` automatically discovers all `.eval` files in `tests/eval/` and runs each one in a fresh Eval context:

```bash
# Run all tests
python -m pytest tests/ -x -q

# Run a specific test file
python -m pytest tests/test_eval_files.py -k "test_arithmetic"

# Include slow tests (thread pool, networking, async)
python -m pytest tests/ --runslow
```

Each `.eval` file is loaded via `Eval.load()`. The test passes if the return value is `0` (meaning `test_end()` reported zero failures). On failure, the captured output (PASS/FAIL lines) is shown in the pytest report.

### With the standalone CLI

```bash
# Run a single test file
eval tests/eval/test_arithmetic.eval

# Run all test files
for f in tests/eval/test_*.eval; do eval "$f"; done
```

The exit status is the return value of the last expression (`test_end()`), so standard shell conventions apply — `0` is success, non-zero is failure.

## Output Format

The test framework prints to stdout using `display` and `newline`. The output format is:

```
-- Group Name -------
  PASS: test name
  PASS: another test
  FAIL: broken test
    expected: 42
    actual:   0
  PASS
  ERROR: uncaught exception: something went wrong
-- 3 pass, 1 fail, 1 error
```

- Header: `-- Group Name -------`
- Each test: two-space indent, `PASS` or `FAIL`, optional `: name`
- Failure detail: four-space indent, `expected:` and `actual:` (for `test`) or `value was false` (for `test_assert`) or `expected error but got none` (for `test_error`)
- Summary: `-- N pass, M fail, K error`

## Float Comparison

The `test` function uses a tolerance of `1e-10` when comparing numbers where at least one is inexact (float). This avoids false failures from floating-point rounding:

```
test("float add", 4.0, 1.5 + 2.5);       // passes (exact match)
test("float mul", 6.25, 2.5 * 2.5);       // passes (within tolerance)
test_assert("mixed", 1 + 0.5 == 1.5);     // passes
```

## Reference

| Function | Arguments | Returns | Purpose |
|----------|-----------|---------|---------|
| `test_begin(name)` | name: string | void | Start group, reset counters, print header |
| `test_end()` | none | integer (failures + errors) | Print summary, return failure count |
| `test(expected, actual)` | 2 values | void | Check equality (unnamed) |
| `test(name, expected, actual)` | string + 2 values | void | Check equality (named) |
| `test_assert(expr)` | 1 value | void | Check truthiness (unnamed) |
| `test_assert(name, expr)` | string + 1 value | void | Check truthiness (named) |
| `test_error(thunk)` | 1 function | void | Check error raised (unnamed) |
| `test_error(name, thunk)` | string + 1 function | void | Check error raised (named) |
| `test_group(name) body` | name + body (keyword) | integer (failures + errors) | Group with exception handling |
