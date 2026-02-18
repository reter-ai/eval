# Indexing and Element Access

Eval provides two complementary ways to access elements in collections: **bracket indexing** (`xs[0]`, `xs[1:3]`) and **arrow properties** (`xs->first`, `xs->ref(1)`, `xs->rest`). They compile to different things but often produce the same result. This guide explains both, compares them, and shows when to use which.

## Bracket indexing: `xs[i]` and `xs[s:e]`

Python-style bracket syntax works on lists, vectors, and strings:

```
[10, 20, 30][0]          // => 10
[10, 20, 30][-1]         // => 30 (negative = from end)
#[10, 20, 30][1]         // => 20
"hello"[0]               // => #\h (character)
```

Slicing returns a new collection of the same type:

```
[1, 2, 3, 4, 5][1:3]    // => [2, 3]
[1, 2, 3][:2]            // => [1, 2]      (omit start = 0)
[1, 2, 3][1:]            // => [2, 3]      (omit end = length)
[1, 2, 3][:]             // => [1, 2, 3]   (full copy)
"hello"[1:3]             // => "el"
#[10, 20, 30, 40][1:3]  // => #[20, 30]
```

Brackets compile to the `ref` and `slice` bridge functions:

```
xs[i]      ->  (ref xs i)
xs[s:e]    ->  (slice xs s e)
```

## Arrow properties: `xs->first`, `xs->ref(i)`, etc.

OO-style access via the `->` operator, available on lists and vectors:

```
[10, 20, 30]->first      // => 10
[10, 20, 30]->second     // => 20
[10, 20, 30]->third      // => 30
[10, 20, 30]->last       // => 30
[10, 20, 30]->rest       // => [20, 30]
[10, 20, 30]->ref(1)     // => 20
[10, 20, 30]->slice(1,3) // => [20, 30]
```

Same on vectors:

```
#[10, 20, 30]->first     // => 10
#[10, 20, 30]->second    // => 20
#[10, 20, 30]->last      // => 30
#[10, 20, 30]->rest      // => #[20, 30]
#[10, 20, 30]->ref(1)    // => 20
```

Arrow access compiles to `__send__` dispatch, which calls the underlying Scheme operations (`car`, `cadr`, `cdr`, `ref`, etc.).

## The equivalence table

Every bracket operation has an arrow equivalent, and vice versa:

### Single element access

| Bracket | Arrow | Description |
|---------|-------|-------------|
| `xs[0]` | `xs->first` | First element |
| `xs[1]` | `xs->second` | Second element |
| `xs[2]` | `xs->third` | Third element |
| `xs[-1]` | `xs->last` | Last element |
| `xs[i]` | `xs->ref(i)` | Element at index `i` |
| `xs[-i]` | `xs->ref(-i)` | Element at index from end |

### Subsequences

| Bracket | Arrow | Description |
|---------|-------|-------------|
| `xs[1:]` | `xs->rest` | Everything but first |
| `xs[:n]` | `xs->take(n)` | First `n` elements |
| `xs[n:]` | `xs->drop(n)` | Skip first `n` elements |
| `xs[s:e]` | `xs->slice(s, e)` | Subrange from `s` to `e` |
| `xs[:]` | `xs->copy()` | Full copy |

### String equivalences

Strings support brackets and string arrow methods, but not all the same names:

| Bracket | Arrow | Description |
|---------|-------|-------------|
| `s[0]` | `s->char_at(0)` | Character at index |
| `s[-1]` | `s->char_at(-1)` | Character from end *(bracket only)* |
| `s[1:3]` | `s->slice(1, 3)` | Substring |
| `s->length` | `s->length` | String length *(arrow only)* |

Note: strings don't have `->first`/`->second`/`->rest` — they have their own method set (see [STRINGS.md](STRINGS.md)).

## When to use which

### Use brackets `[]` when:

**You have a computed index** — the index is a variable or expression:

```
define i = find_position(data);
data[i];
matrix[row][col];
```

**You're doing random access into a known position:**

```
define name = record[0];
define value = record[1];
```

**You want negative indexing from the end:**

```
xs[-1]       // last element
xs[-2]       // second to last
xs[-3:]      // last three elements
```

**You're slicing with computed bounds:**

```
data[start:end];
text[offset:offset + len];
```

**You're working with strings:**

```
"hello"[0]       // => #\h
"hello"[1:3]     // => "el"
```

### Use arrow `->` when:

**You're destructuring a known structure** — the code reads like field access:

```
define method = request->first;
define path = request->second;
define body = request->third;

// vs the bracket equivalent:
define method = request[0];
define path = request[1];
define body = request[2];
```

Arrow reads better here because `->first`, `->second`, `->third` communicate *meaning* — you're unpacking a fixed structure. Brackets with magic numbers require comments or mental indexing.

**You want to chain into further method calls:**

```
// Arrow chains naturally into methods
data->rest->filter(function(x) x > 0)->sort(<);

// Brackets can't chain into methods directly
data[1:]->filter(function(x) x > 0)->sort(<);  // also works! slicing returns a list
```

Both work for chaining, but arrow reads more uniformly in a method chain.

**You're accessing the head/tail of a list (functional style):**

```
// Pattern: process first, recurse on rest
define process = function(lst) {
    when(!null?(lst)) {
        handle(lst->first);
        process(lst->rest);
    };
};
```

This is the classic `car`/`cdr` pattern, but more readable.

**You want descriptive property access in OO-style code:**

```
response->first;          // status code — reads naturally
response->second;         // body — reads naturally
point->first;             // x coordinate
point->second;            // y coordinate
```

## Mixing both styles

Brackets and arrows compose freely. Use whichever is clearest at each point:

```
// Parse a CSV: split (string method), trim each (string method), take first 3 (list method)
"a, b, c, d"->split(",")
    ->map(function(s) s->trim())
    ->take(3);
// => ["a", "b", "c"]

// Matrix operations: brackets for random access, arrows for structure
define matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
matrix[1][2];                    // => 6 (row 1, col 2)
matrix->first;                   // => [1, 2, 3] (first row)
matrix->map(function(row) row->last);  // => [3, 6, 9] (last column)

// HTTP response: arrows for structure, brackets for dynamic access
define response = [200, "OK", ["Content-Type: text/html", "Connection: close"]];
response->first;                 // => 200 (status)
response->third->find(function(h) h->starts_with("Content"));
// => "Content-Type: text/html"
```

## Bracket vs arrow: how they compile

The two styles take different paths through the compiler:

```
xs[1]              ->  (ref xs 1)              // bridge function, works on list/vector/string
xs->second         ->  (__send__ xs 'second)   // OO dispatch, calls (cadr xs)
xs->ref(1)         ->  (__send__ xs 'ref)      // returns (lambda (i) (ref xs i)), then called with 1
```

For simple element access, brackets are slightly more direct (one function call vs two). In practice the difference is negligible — use whichever reads better.

## The `ref` and `slice` functions

Brackets are syntax sugar for `ref` and `slice`, which can also be called directly:

```
ref([10, 20, 30], 1)            // => 20
ref(#[10, 20, 30], -1)          // => 30
ref("hello", 0)                 // => #\h

slice([1, 2, 3, 4], 1, 3)      // => [2, 3]
slice("hello", 0, 3)            // => "hel"
slice(#[10, 20, 30], 1, false)  // => #[20, 30] (false = to end)
```

The `ref` function supports negative indices on all types. The `slice` function accepts `false` for omitted bounds (equivalent to `[:e]` and `[s:]`).

## Complete reference

### Bracket syntax

| Syntax | Compiles to | Description |
|--------|-------------|-------------|
| `xs[i]` | `(ref xs i)` | Element at index (negative ok) |
| `xs[s:e]` | `(slice xs s e)` | Subrange, exclusive end |
| `xs[:e]` | `(slice xs 0 e)` | From start to `e` |
| `xs[s:]` | `(slice xs s #f)` | From `s` to end |
| `xs[:]` | `(slice xs 0 #f)` | Full copy |

Works on: lists, vectors, strings.

### Arrow properties (no parentheses)

| Property | Lists | Vectors | Description |
|----------|-------|---------|-------------|
| `->first` | `(car xs)` | `(vector-ref xs 0)` | First element |
| `->second` | `(cadr xs)` | `(vector-ref xs 1)` | Second element |
| `->third` | `(caddr xs)` | `(vector-ref xs 2)` | Third element |
| `->last` | `(last xs)` | `(vector-ref xs (- len 1))` | Last element |
| `->rest` | `(cdr xs)` | `(vector-copy xs 1)` | All but first |
| `->length` | `(length xs)` | `(vector-length xs)` | Element count |
| `->empty?` | `(null? xs)` | `(= len 0)` | True if empty |

### Arrow methods (call with parentheses)

| Method | Equivalent bracket | Description |
|--------|--------------------|-------------|
| `->ref(i)` | `xs[i]` | Element at index |
| `->slice(s, e)` | `xs[s:e]` | Subrange |
| `->take(n)` | `xs[:n]` | First `n` elements |
| `->drop(n)` | `xs[n:]` | Skip first `n` elements |
| `->copy()` | `xs[:]` | Full copy |

See [LISTS.md](LISTS.md), [VECTORS.md](VECTORS.md), and [STRINGS.md](STRINGS.md) for the complete method references.
