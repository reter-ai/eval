# F-Strings (Interpolated Strings)

F-strings provide string interpolation with embedded expressions. Prefix a string with `f` and use `{expr}` placeholders — each expression is evaluated and converted to a string automatically.

```
f"hello {name}"                    // => "hello world" (if name is "world")
f"sum: {1 + 2 + 3}"               // => "sum: 6"
f"item: {xs[0]}"                   // => "item: 10" (if xs is [10, 20, 30])
```

Without f-strings you would write:

```
"hello " ++ name
"sum: " ++ to_string(1 + 2 + 3)
"item: " ++ to_string(xs[0])
```

F-strings are shorter, more readable, and handle type conversion automatically.

## Syntax

An f-string starts with `f"` and ends with `"`. Between the quotes, text and `{expr}` placeholders alternate freely:

```
f"text"                            // no interpolation — same as "text"
f"{expr}"                          // expression only — converts to string
f"text {expr} more text"           // mixed
f"a {e1} b {e2} c {e3} d"         // multiple interpolations
f""                                // empty string
```

### Expressions

Any Eval expression can appear inside `{...}`, including arithmetic, function calls, method chains, indexing, blocks, and nested f-strings:

```
// Arithmetic
f"result: {x + y * 2}"

// Function calls
f"len: {length(items)}"

// Method chains
f"name: {name->trim()->upper()}"

// Indexing and slicing
f"first: {items[0]}, last: {items[-1]}"

// Blocks
f"val: {{ define a = 10; define b = 20; a + b }}"

// Ternary-style
f"sign: {if(x > 0) "positive" else "negative"}"

// Nested f-strings
f"outer {f"inner {x}"} done"
```

### Escape sequences

Text portions of f-strings support the same escape sequences as regular strings: `\n` `\t` `\r` `\\` `\"` `\0`.

```
f"line1\nline2"                    // newline between lines
f"tab\there"                       // tab character
f"quote: \""                       // literal double quote
```

### Escaped braces

Use `{{` and `}}` to include literal `{` and `}` characters:

```
f"{{ and }}"                       // => "{ and }"
f"set: {{{x}}}"                    // => "set: {42}" (if x is 42)
```

The first `{{` produces a literal `{`, then `{x}` is interpolation, then `}}` produces a literal `}`.

## Type conversion

Interpolated expressions are automatically converted to strings using `__tostr__`. The conversion uses display semantics (no quotes on strings, no hash on characters):

| Type | Example | Result |
|------|---------|--------|
| string | `f"{name}"` | `Alice` (no quotes) |
| integer | `f"{42}"` | `42` |
| float | `f"{3.14}"` | `3.14` |
| boolean | `f"{true}"` | `true` |
| nil | `f"{nil}"` | `nil` |
| symbol | `f"{'hello}"` | `hello` |
| character | `f"{'A'}"` | `A` |
| list | `f"{[1,2,3]}"` | `(1 2 3)` |
| DateTime | `f"{DateTime->now()}"` | `2026-02-20T10:30:00Z` |
| Date | `f"{Date->today()}"` | `2026-02-20` |
| Decimal | `f"{Decimal("1.23")}"` | `1.23` |
| other | `f"{obj}"` | write representation |

String values pass through without conversion — no redundant quoting.

### Explicit conversion

You can also call `__tostr__` directly if needed:

```
__tostr__(42)                      // => "42"
__tostr__(true)                    // => "true"
__tostr__([1, 2, 3])              // => "(1 2 3)"
```

## Compilation

F-strings compile to `(string-append ...)` with `(__tostr__ expr)` wrapping each interpolated expression:

| F-string | Compiles to |
|----------|-------------|
| `f"hello {name}"` | `(string-append "hello " (__tostr__ name))` |
| `f"a {x} b {y} c"` | `(string-append "a " (__tostr__ x) " b " (__tostr__ y) " c")` |
| `f"{x}"` | `(string-append (__tostr__ x))` |
| `f"plain text"` | `"plain text"` (optimized — no string-append) |
| `f""` | `""` |

Empty text segments are omitted from the output. An f-string with no interpolation is optimized to a plain string constant.

## Nesting

F-strings can be nested — an interpolation can contain another f-string:

```
define name = "world";
define greeting = f"hello {f"dear {name}"}!";
// => "hello dear world!"
```

Nesting works to arbitrary depth. The lexer tracks brace depth per nesting level, so `{` and `}` inside expressions (blocks, dicts, etc.) are handled correctly:

```
f"val: {{ define d = dict(a: 1); d->a }}"
// The inner { } are part of the block, not f-string delimiters
```

## Examples

### Basic usage

```
define name = "Alice";
define age = 30;
f"Hello, {name}! You are {age} years old.";
// => "Hello, Alice! You are 30 years old."
```

### Building messages

```
define items = [1, 2, 3, 4, 5];
f"Found {length(items)} items, sum = {fold(+, 0, items)}";
// => "Found 5 items, sum = 15"
```

### Formatted output

```
define scores = [("Alice", 95), ("Bob", 87), ("Carol", 92)];
for(let entry in scores) {
    define name = car(entry);
    define score = cdr(entry);
    print(f"{name}: {score}");
};
// Alice: 95
// Bob: 87
// Carol: 92
```

### With string methods

```
define raw = "  hello world  ";
f"cleaned: '{raw->trim()->upper()}'";
// => "cleaned: 'HELLO WORLD'"
```

### Conditional content

```
define n = 5;
f"n is {if(n % 2 == 0) "even" else "odd"}";
// => "n is odd"
```

### Error messages

```
define validate = function(x) {
    when(x < 0) error(f"expected non-negative, got {x}");
    x;
};
```

### With collections

```
define xs = [1, 2, 3];
f"list: {xs}, length: {length(xs)}, first: {car(xs)}";
// => "list: (1 2 3), length: 3, first: 1"
```

## Comparison with `++` concatenation

F-strings and `++` concatenation are interchangeable. F-strings are more concise for multi-part strings; `++` is fine for simple joins:

```
// These produce the same result:
f"hello {name}, age {age}"
"hello " ++ __tostr__(name) ++ ", age " ++ __tostr__(age)

// For simple joins, ++ is equally readable:
"hello " ++ name
f"hello {name}"
```

Note that `++` does not auto-convert types — both sides must be strings. F-strings handle conversion automatically, which avoids the common `to_string()` boilerplate.

## Comparison with other languages

| Language | Syntax | Notes |
|----------|--------|-------|
| **Eval** | `f"hello {name}"` | `{expr}`, escaped braces `{{ }}`, nesting |
| Python | `f"hello {name}"` | Same syntax, same escaping |
| JavaScript | `` `hello ${name}` `` | Template literals with `${}` |
| C# | `$"hello {name}"` | `$` prefix, `{}` interpolation |
| Kotlin | `"hello $name"` or `"hello ${expr}"` | `$` prefix for simple, `${}` for complex |
| Ruby | `"hello #{name}"` | `#{}` interpolation |

Eval's f-strings follow the Python convention exactly: `f` prefix, `{expr}` placeholders, `{{ }}` for literal braces.
