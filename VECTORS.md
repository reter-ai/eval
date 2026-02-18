# Vector Methods

Eval provides Python/JS-style OO methods on vectors via the `->` operator. Vectors (`#[1, 2, 3]`) are fixed-size, random-access arrays — they support O(1) indexing and length, making them ideal for performance-critical collections:

```
#[3, 1, 2]->sort(<);                          // => #[1, 2, 3]
#[1, 2, 3]->map(function(x) x * 2);           // => #[2, 4, 6]
#[1, 2, 3]->to_list()->filter(function(x) x > 1);  // => [2, 3]
```

The `->` operator dispatches by type: vectors get vector methods, lists get list methods (see [LISTS.md](LISTS.md)), strings get string methods (see [STRINGS.md](STRINGS.md)), and all other types (interfaces, constructors, records, dicts) work as before.

## Properties

Properties return a value directly, no parentheses needed:

```
#[1, 2, 3]->length;       // => 3
#[]->length;               // => 0
#[1, 2]->empty?;           // => false
#[]->empty?;               // => true
#[10, 20, 30]->first;     // => 10
#[10, 20, 30]->second;    // => 20
#[10, 20, 30]->third;     // => 30
#[10, 20, 30]->last;      // => 30
#[10, 20, 30]->rest;      // => #[20, 30]
```

| Property | Returns | Description |
|----------|---------|-------------|
| `->length` | integer | Number of elements (O(1)) |
| `->empty?` | boolean | True if length is 0 |
| `->first` | any | First element — same as `v[0]` |
| `->second` | any | Second element — same as `v[1]` |
| `->third` | any | Third element — same as `v[2]` |
| `->last` | any | Last element — same as `v[-1]` |
| `->rest` | vector | All but first — same as `v[1:]` |

These properties mirror bracket indexing. See [INDEXING.md](INDEXING.md) for a full comparison.

## Methods

Methods return a closure that you call with `()`. Most return a new vector or value.

### Element access

```
#[10, 20, 30]->ref(1);             // => 20
#[10, 20, 30]->ref(-1);            // => 30 (negative = from end)
#[10, 20, 30, 40]->slice(1, 3);    // => #[20, 30]
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->ref(i)` | any | Element at index (supports negative) |
| `->slice(start, end)` | vector | Subvector from `start` to `end` (exclusive) |

### Transformation

```
#[1, 2, 3]->map(function(x) x * 2);           // => #[2, 4, 6]
#[1, 2, 3, 4]->filter(function(x) x > 2);     // => #[3, 4]
#[1, 2, 3, 4]->reject(function(x) x > 2);     // => #[1, 2]
#[1, 2, 3]->reverse();                          // => #[3, 2, 1]
#[1, 2, 2, 3, 1]->unique();                    // => #[1, 2, 3]
#[1, 2, 3]->copy();                             // => #[1, 2, 3]
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->map(fn)` | vector | Transform each element with `fn` |
| `->filter(fn)` | vector | Keep elements where `fn` returns true |
| `->reject(fn)` | vector | Remove elements where `fn` returns true |
| `->reverse()` | vector | Reversed copy |
| `->unique()` | vector | Remove duplicate elements |
| `->copy()` | vector | Shallow copy |

### Folding

```
#[1, 2, 3, 4]->fold(+, 0);      // => 10
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->fold(fn, init)` | any | Left fold: `fn(element, accumulator)` |

### Searching

```
#[1, 2, 3]->any(function(x) x > 2);       // => true
#[2, 4, 6]->every(function(x) x % 2 == 0); // => true
#[1, 2, 3, 4]->find(function(x) x > 2);   // => 3
#[1, 2, 3]->find(function(x) x > 10);     // => false
#[1, 2, 3, 4]->count(function(x) x > 2);  // => 2

#[10, 20, 30]->index_of(function(x) x == 20);  // => 1
#[10, 20, 30]->index_of(function(x) x == 99);  // => false

#[1, 2, 3]->contains(2);                   // => true
#[1, 2, 3]->contains(5);                   // => false
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->any(pred)` | boolean | True if any element satisfies `pred` |
| `->every(pred)` | boolean | True if all elements satisfy `pred` |
| `->find(pred)` | any/false | First matching element, or `false` |
| `->count(pred)` | integer | Number of elements satisfying `pred` |
| `->index_of(pred)` | integer/false | Index of first match, or `false` |
| `->contains(x)` | boolean | True if `x` is in the vector (by `equal?`) |

### Sorting and combining

```
#[3, 1, 2]->sort(<);                // => #[1, 2, 3]
#[3, 1, 2]->sort(>);                // => #[3, 2, 1]
#[1, 2]->append(#[3, 4]);           // => #[1, 2, 3, 4]
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->sort(cmp)` | vector | Sorted copy using comparator |
| `->append(other)` | vector | Concatenate two vectors |

### Side effects

```
define sum = 0;
#[1, 2, 3]->for_each(function(x) { sum = sum + x; });
sum;    // => 6
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->for_each(fn)` | void | Call `fn` on each element (side effects) |

### Mutation

Vectors support in-place mutation:

```
define v = #[10, 20, 30];
v->set(1, 99);
v->ref(1);          // => 99
v;                   // => #[10, 99, 30]
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->set(i, val)` | void | Mutate element at index `i` in-place |

### Conversion

```
#[1, 2, 3]->to_list();              // => [1, 2, 3]
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->to_list()` | list | Convert to list |

Converting to a list unlocks the full set of list methods (see [LISTS.md](LISTS.md)):

```
#[1, 2, 3]->to_list()->flat_map(function(x) [x, x]);
// => [1, 1, 2, 2, 3, 3]

#[1, 2, 3]->to_list()->join(",");
// => "1,2,3" — wait, join expects strings!
```

## Chaining

Methods that return vectors can be followed by more vector methods:

```
// Filter then map
#[1, 2, 3, 4]->filter(function(x) x % 2 == 0)->map(function(x) x * 2);
// => #[4, 8]

// Sort a vector
#[3, 1, 4, 1, 5]->sort(<)->unique();
// => #[1, 3, 4, 5]
```

Converting types changes the dispatch:

```
// Vector -> list -> string
#[1, 2, 3]->to_list()->map(function(x) `number->string`(x))->join(",");
// => "1,2,3"

// List -> vector
[1, 2, 3]->to_vector()->map(function(x) x * 2);
// => #[2, 4, 6]
```

## Vectors vs lists

| | Lists `[1, 2, 3]` | Vectors `#[1, 2, 3]` |
|---|---|---|
| **Indexing** | O(n) | O(1) |
| **Length** | O(n) | O(1) |
| **Prepend** | O(1) (`cons`) | O(n) (copy) |
| **Mutation** | No `->set` | `->set(i, val)` |
| **Literal** | `[1, 2, 3]` | `#[1, 2, 3]` |
| **Nature** | Linked list (pairs) | Contiguous array |
| **Methods** | 30+ (full SRFI-1 set) | 22 (core operations) |

Use lists for functional pipelines, recursive processing, and when you need `cons`/`car`/`cdr`. Use vectors when you need fast random access, in-place mutation, or compact storage.

## Compatibility

The `->` operator uses a type-based dispatch function `__send__`. Vectors, lists, strings, and callable objects each get their own dispatch:

```
// Vectors use vector methods
#[1, 2, 3]->map(function(x) x * 2);   // => #[2, 4, 6]

// Lists use list methods
[1, 2, 3]->sort(<);                    // => [1, 2, 3]

// Strings use string methods
"hello"->upper();                       // => "HELLO"

// Interfaces still work
define Point = constructor(x, y)
    interface(x: function() x, y: function() y);
Point(3, 4)->x();                       // => 3
```

## Complete reference

### Properties (no parentheses)

| Property | Type | Description | Bracket equivalent |
|----------|------|-------------|-------------------|
| `->length` | integer | Vector length (O(1)) | — |
| `->empty?` | boolean | True if empty | — |
| `->first` | any | First element | `v[0]` |
| `->second` | any | Second element | `v[1]` |
| `->third` | any | Third element | `v[2]` |
| `->last` | any | Last element | `v[-1]` |
| `->rest` | vector | All but first | `v[1:]` |

### Methods (call with parentheses)

| Method | Returns | Description |
|--------|---------|-------------|
| `->ref(i)` | any | Element at index |
| `->slice(start, end)` | vector | Subvector |
| `->map(fn)` | vector | Transform elements |
| `->filter(fn)` | vector | Keep matching elements |
| `->reject(fn)` | vector | Remove matching elements |
| `->fold(fn, init)` | any | Left fold |
| `->for_each(fn)` | void | Iterate (side effects) |
| `->any(pred)` | boolean | Any element matches? |
| `->every(pred)` | boolean | All elements match? |
| `->find(pred)` | any/false | First matching element |
| `->count(pred)` | integer | Count matching elements |
| `->index_of(pred)` | int/false | Index of first match |
| `->contains(x)` | boolean | Element membership |
| `->reverse()` | vector | Reversed copy |
| `->sort(cmp)` | vector | Sorted copy |
| `->unique()` | vector | Remove duplicates |
| `->append(other)` | vector | Concatenate vectors |
| `->to_list()` | list | Convert to list |
| `->copy()` | vector | Shallow copy |
| `->set(i, val)` | void | Mutate element in-place |
