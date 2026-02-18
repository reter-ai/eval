# String Methods

Eval provides Python/JS-style OO string methods via the `->` operator. Every string supports property access and method calls that chain naturally:

```
"hello"->upper();                      // => "HELLO"
"  Hello World  "->trim()->upper();    // => "HELLO WORLD"
"a,b,c"->split(",")->length;           // => 3 (split returns list, then length)
```

The `->` operator dispatches by type: strings get string methods, lists get list methods (see [LISTS.md](LISTS.md)), vectors get vector methods (see [VECTORS.md](VECTORS.md)), and all other types (interfaces, constructors, records, dicts) work as before.

## Properties

Properties return a value directly, no parentheses needed:

```
"hello"->length;       // => 5
""->length;            // => 0
""->empty?;            // => true
"hello"->empty?;       // => false
```

| Property | Returns | Description |
|----------|---------|-------------|
| `->length` | integer | Number of characters |
| `->empty?` | boolean | True if length is 0 |

## Methods

Methods return a closure that you call with `()`. Most return a new string, enabling chaining.

### Case conversion

```
"hello"->upper();      // => "HELLO"
"HELLO"->lower();      // => "hello"
"Hello"->upper()->lower();  // => "hello"
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->upper()` | string | ASCII uppercase |
| `->lower()` | string | ASCII lowercase |

### Trimming

```
"  hi  "->trim();         // => "hi"
"  hi  "->trim_start();   // => "hi  "
"  hi  "->trim_end();     // => "  hi"
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->trim()` | string | Remove whitespace from both ends |
| `->trim_start()` | string | Remove whitespace from the start |
| `->trim_end()` | string | Remove whitespace from the end |

### Searching

```
"hello world"->contains("world");     // => true
"hello"->contains("xyz");             // => false

"hello"->starts_with("hel");          // => true
"hello"->ends_with("llo");            // => true

"hello"->index_of("ll");              // => 2
"hello"->index_of("xyz");             // => false
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->contains(s)` | boolean | True if `s` is a substring |
| `->starts_with(s)` | boolean | True if string starts with `s` |
| `->ends_with(s)` | boolean | True if string ends with `s` |
| `->index_of(s)` | integer or `false` | Index of first occurrence, or `false` |

### Replacing and splitting

```
"hello world"->replace("world", "there");   // => "hello there"
"aXbXc"->replace("X", "-");                 // => "a-b-c"

"a,b,c"->split(",");                        // => ["a", "b", "c"]
","->join(["a", "b", "c"]);                 // => "a,b,c"

// Round-trip
","->join("a,b,c"->split(","));             // => "a,b,c"
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->replace(old, new)` | string | Replace all occurrences of `old` with `new` |
| `->split(sep)` | list of strings | Split by separator substring |
| `->join(lst)` | string | Join list of strings using receiver as separator |

Note: `->join` is called on the separator string, not the list. This reads naturally: `","->join(items)`.

### Substrings and characters

```
"hello world"->slice(0, 5);       // => "hello"
"hello"->char_at(1);              // => #\e (character)
"abc"->chars();                   // => (#\a #\b #\c) (list of characters)
"abc"->reverse();                 // => "cba"
"ab"->repeat(3);                  // => "ababab"
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->slice(start, end)` | string | Substring from `start` to `end` (exclusive) |
| `->char_at(i)` | character | Character at index `i` |
| `->chars()` | list | List of characters |
| `->reverse()` | string | Reversed copy |
| `->repeat(n)` | string | String repeated `n` times |

### Higher-order methods

These methods take a function or predicate and operate over the characters:

```
// Map: transform each character
"hello"->map(function(ch) `char-upcase`(ch));   // => "HELLO"

// Fold: reduce over characters
"banana"->fold(function(ch, n)
    if(`string`(ch) == "a") n + 1 else n, 0);   // => 3

// Any / every: test characters
"abc3def"->any(function(ch) `char-numeric?`(ch));       // => true
"hello"->every(function(ch) `char-alphabetic?`(ch));    // => true

// Count: count matching characters
"a b c"->count(function(ch) `string`(ch) == " ");       // => 2

// Find: first matching character (or false)
"ab3cd"->find(function(ch) `char-numeric?`(ch));         // => #\3

// For-each: iterate (side effects only)
"abc"->for_each(function(ch) display(ch));               // prints: abc
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->map(fn)` | string | Transform each character with `fn` |
| `->fold(fn, init)` | any | Reduce over characters: `fn(char, acc)` |
| `->any(pred)` | boolean | True if any character satisfies `pred` |
| `->every(pred)` | boolean | True if all characters satisfy `pred` |
| `->count(pred)` | integer | Count characters satisfying `pred` |
| `->find(pred)` | char/false | First character satisfying `pred`, or `false` |
| `->for_each(fn)` | void | Call `fn` on each character |
| `->copy()` | string | Copy the string |

### Conversion

```
"42"->to_number();       // => 42
"3.14"->to_number();     // => 3.14
"hello"->to_symbol();    // => hello (symbol)
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->to_number()` | number | Parse as number |
| `->to_symbol()` | symbol | Convert to symbol |

## Chaining

Since most methods return a new string, calls chain naturally:

```
"  Hello World  "->trim()->upper();                  // => "HELLO WORLD"
"  hello  "->trim()->upper()->reverse();             // => "OLLEH"
","->join("a , b , c"->split(","));                  // => "a , b , c"

// Process a CSV-like line
define line = "  Alice , Bob , Carol  ";
define names = map(
    function(s) s->trim(),
    line->split(",")
);
// names => ["Alice", "Bob", "Carol"]

// Build a slug from a title
define title = "  Hello World  ";
define slug = title->trim()->lower()->replace(" ", "-");
// slug => "hello-world"
```

## Compatibility

The `->` operator uses a type-based dispatch function `__send__`. Strings, lists, vectors, and callable objects each get their own dispatch:

```
// Strings use string methods
"hello"->upper();        // => "HELLO"
"hello"->length;         // => 5

// Lists use list methods
[1, 2, 3]->map(function(x) x * 2);  // => [2, 4, 6]

// Vectors use vector methods
#[1, 2, 3]->length;     // => 3

// Interfaces still work
define Point = constructor(x, y)
    interface(x: function() x, y: function() y);
define p = Point(3, 4);
p->x();                  // => 3

// Records still work
record Vec(a, b);
define v = Vec(1, 2);
v->a;                    // => 1

// Dicts still work
define d = dict(name: "test");
d->name;                 // => "test"
```

See [LISTS.md](LISTS.md) and [VECTORS.md](VECTORS.md) for the collection method references.

## Complete reference

### Properties (no parentheses)

| Property | Type | Description |
|----------|------|-------------|
| `->length` | integer | String length |
| `->empty?` | boolean | True if empty |

### Methods (call with parentheses)

| Method | Returns | Description |
|--------|---------|-------------|
| `->upper()` | string | ASCII uppercase |
| `->lower()` | string | ASCII lowercase |
| `->trim()` | string | Trim whitespace from both ends |
| `->trim_start()` | string | Trim whitespace from start |
| `->trim_end()` | string | Trim whitespace from end |
| `->contains(s)` | boolean | Substring search |
| `->starts_with(s)` | boolean | Prefix check |
| `->ends_with(s)` | boolean | Suffix check |
| `->index_of(s)` | int/false | Find substring position |
| `->replace(old, new)` | string | Replace all occurrences |
| `->split(sep)` | list | Split by separator |
| `->join(lst)` | string | Join list with separator |
| `->slice(start, end)` | string | Substring extraction |
| `->char_at(i)` | char | Character at index |
| `->chars()` | list | List of characters |
| `->reverse()` | string | Reversed string |
| `->repeat(n)` | string | Repeat n times |
| `->to_number()` | number | Parse as number |
| `->to_symbol()` | symbol | Convert to symbol |
| `->map(fn)` | string | Transform each character |
| `->fold(fn, init)` | any | Reduce over characters |
| `->any(pred)` | boolean | Any character matches? |
| `->every(pred)` | boolean | All characters match? |
| `->count(pred)` | integer | Count matching characters |
| `->find(pred)` | char/false | First matching character |
| `->for_each(fn)` | void | Iterate over characters |
| `->copy()` | string | Copy the string |
