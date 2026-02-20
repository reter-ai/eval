# List Methods

Eval provides Python/JS-style OO methods on lists via the `->` operator. Every list supports property access and method calls that chain naturally:

```
[3, 1, 2]->sort(<)->map(function(x) x * 2);   // => [2, 4, 6]
[1, 2, 3, 4]->filter(function(x) x % 2 == 0); // => [2, 4]
["a", "b", "c"]->join(",");                     // => "a,b,c"
```

The `->` operator dispatches by type: lists get list methods, vectors get vector methods (see [VECTORS.md](VECTORS.md)), strings get string methods (see [STRINGS.md](STRINGS.md)), dicts get dict methods (see [DICTS.md](DICTS.md)), and all other types (interfaces, constructors, records) work as before.

## Properties

Properties return a value directly, no parentheses needed:

```
[1, 2, 3]->length;        // => 3
[1, 2]->empty?;           // => false
[1, 2, 3]->first;         // => 1
[1, 2, 3]->second;        // => 2
[1, 2, 3]->third;         // => 3
[1, 2, 3]->last;          // => 3
[1, 2, 3]->rest;          // => [2, 3]
```

| Property | Returns | Description |
|----------|---------|-------------|
| `->length` | integer | Number of elements (O(n)) |
| `->empty?` | boolean | Always `false` for non-empty lists |
| `->first` | any | First element (`car`) — same as `xs[0]` |
| `->second` | any | Second element (`cadr`) — same as `xs[1]` |
| `->third` | any | Third element — same as `xs[2]` |
| `->last` | any | Last element — same as `xs[-1]` |
| `->rest` | list | All but first (`cdr`) — same as `xs[1:]` |

These properties mirror bracket indexing: `xs->first` is `xs[0]`, `xs->rest` is `xs[1:]`, etc. See [INDEXING.md](INDEXING.md) for a full comparison.

Note: `->` dispatches on `pair?`, so the empty list `[]` (which is `null?`, not `pair?`) cannot use these methods. Use `length([])` or `null?([])` directly for empty lists.

## Methods

Methods return a closure that you call with `()`. Most return a new list, enabling chaining.

### Element access

```
[10, 20, 30]->ref(1);              // => 20
[10, 20, 30]->ref(-1);             // => 30 (negative = from end)
[10, 20, 30, 40]->slice(1, 3);     // => [20, 30]
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->ref(i)` | any | Element at index (supports negative) |
| `->slice(start, end)` | list | Sublist from `start` to `end` (exclusive) |

### Transformation

```
[1, 2, 3]->map(function(x) x * 2);           // => [2, 4, 6]
[1, 2, 3, 4]->filter(function(x) x > 2);     // => [3, 4]
[1, 2, 3, 4]->reject(function(x) x > 2);     // => [1, 2]
[1, 2, 3]->reverse();                          // => [3, 2, 1]
[[1, 2], [3, 4]]->flatten();                   // => [1, 2, 3, 4]
[1, 2, 2, 3, 1]->unique();                    // => [1, 2, 3]
[1, 2, 3]->copy();                             // => [1, 2, 3]
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->map(fn)` | list | Transform each element with `fn` |
| `->filter(fn)` | list | Keep elements where `fn` returns true |
| `->reject(fn)` | list | Remove elements where `fn` returns true |
| `->filter_map(fn)` | list | Map + filter: keep non-false results of `fn` |
| `->reverse()` | list | Reversed copy |
| `->flatten()` | list | Flatten one level of nesting |
| `->unique()` | list | Remove duplicate elements |
| `->delete(x)` | list | Remove all occurrences of `x` (by `equal?`) |
| `->copy()` | list | Shallow copy |

```
// filter_map: map and filter in one pass
[1, 2, 3, 4]->filter_map(function(x) if(x > 2) x * 10 else false);
// => [30, 40]

// delete: remove all occurrences of a value
[1, 2, 3, 2, 1]->delete(2);    // => [1, 3, 1]
```

### Folding

```
[1, 2, 3, 4]->fold(+, 0);       // => 10
[1, 2, 3]->fold_right(
    function(x, acc) cons(x, acc), []);  // => [1, 2, 3]
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->fold(fn, init)` | any | Left fold: `fn(element, accumulator)` |
| `->fold_right(fn, init)` | any | Right fold: `fn(element, accumulator)` |
| `->reduce(fn)` | any | Left fold using first element as init (error on empty) |
| `->reduce_right(fn)` | any | Right fold using last element as init (error on empty) |

Note: `fold` follows SRFI-1 conventions where the combining function takes `(element, accumulator)`.

```
[1, 2, 3, 4, 5]->reduce(+);       // => 15 (no initial value needed)
[1, 2, 3, 4, 5]->reduce_right(+); // => 15
```

### Searching

```
[1, 2, 3]->any(function(x) x > 2);      // => true
[2, 4, 6]->every(function(x) x % 2 == 0); // => true
[1, 2, 3, 4]->find(function(x) x > 2);  // => 3
[1, 2, 3]->find(function(x) x > 10);    // => false
[1, 2, 3, 4]->count(function(x) x > 2); // => 2

[10, 20, 30]->index_of(function(x) x == 20);  // => 1
[10, 20, 30]->index_of(function(x) x == 99);  // => false

[1, 2, 3]->contains(2);                  // => true
[1, 2, 3]->contains(5);                  // => false
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->any(pred)` | boolean | True if any element satisfies `pred` |
| `->every(pred)` | boolean | True if all elements satisfy `pred` |
| `->find(pred)` | any/false | First matching element, or `false` |
| `->count(pred)` | integer | Number of elements satisfying `pred` |
| `->index_of(pred)` | integer/false | Index of first match, or `false` |
| `->contains(x)` | boolean | True if `x` is in the list (by `equal?`) |

Note: `->index_of` takes a predicate, not a value. To find the index of a specific value, use `->index_of(function(x) x == value)`.

### Taking and dropping

```
[1, 2, 3, 4]->take(2);                        // => [1, 2]
[1, 2, 3, 4]->drop(2);                        // => [3, 4]
[1, 2, 5, 3]->take_while(function(x) x < 4);  // => [1, 2]
[1, 2, 5, 3]->drop_while(function(x) x < 4);  // => [5, 3]
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->take(n)` | list | First `n` elements |
| `->drop(n)` | list | All but first `n` elements |
| `->take_while(pred)` | list | Leading elements while `pred` is true |
| `->drop_while(pred)` | list | Skip leading elements while `pred` is true |
| `->take_right(n)` | list | Last `n` elements |
| `->drop_right(n)` | list | All but last `n` elements |
| `->split_at(n)` | [list, list] | Split at position: `[first_n, rest]` |
| `->span(pred)` | [list, list] | Split where predicate becomes false: `[head, tail]` |

```
[1, 2, 3, 4, 5]->take_right(2);                         // => [4, 5]
[1, 2, 3, 4, 5]->drop_right(2);                         // => [1, 2, 3]
[1, 2, 3, 4, 5]->split_at(3);                           // => [[1, 2, 3], [4, 5]]
[1, 2, 3, 4, 5]->span(function(x) x < 3);              // => [[1, 2], [3, 4, 5]]
```

### Sorting and combining

```
[3, 1, 2]->sort(<);                   // => [1, 2, 3]
[3, 1, 2]->sort(>);                   // => [3, 2, 1]
[1, 2]->append([3, 4]);               // => [1, 2, 3, 4]
[1, 2]->zip(["a", "b"]);              // => [[1, "a"], [2, "b"]]
[1, 2, 3]->flat_map(function(x) [x, x]);  // => [1, 1, 2, 2, 3, 3]
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->sort(cmp)` | list | Sorted copy using comparator (e.g., `<`, `>`) |
| `->append(other)` | list | Concatenate two lists |
| `->zip(other)` | list | Pair up elements: `[[a1,b1], [a2,b2], ...]` |
| `->flat_map(fn)` | list | Map then flatten one level |

### Partitioning

```
define parts = [1, 2, 3, 4, 5]->partition(function(x) x % 2 == 0);
ref(parts, 0);    // => [2, 4]     (matching)
ref(parts, 1);    // => [1, 3, 5]  (non-matching)
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->partition(pred)` | list of two lists | Split into `[matches, non-matches]` |

### Side effects

```
define sum = 0;
[1, 2, 3]->for_each(function(x) { sum = sum + x; });
sum;    // => 6
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->for_each(fn)` | void | Call `fn` on each element (side effects) |

### Joining and conversion

```
["a", "b", "c"]->join(",");          // => "a,b,c"
["hello", "world"]->join(" ");       // => "hello world"
["a", "b", "c"]->join("");           // => "abc"
[1, 2, 3]->to_vector();              // => #[1, 2, 3]
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->join(sep)` | string | Join elements with separator string |
| `->to_vector()` | vector | Convert to vector |

Note: `->join(sep)` is the dual of `str->split(sep)`. Compare: `","->join(["a","b"])` (string join, separator is receiver) vs `["a","b"]->join(",")` (list join, separator is argument). Both return `"a,b"`.

## Chaining

Since most methods return a new list, calls chain naturally:

```
// Filter then map
[1, 2, 3, 4]->filter(function(x) x % 2 == 0)->map(function(x) x * 2);
// => [4, 8]

// Sort then take
[3, 1, 4, 2]->sort(<)->take(2);
// => [1, 2]

// Map to strings then join
[1, 2, 3]->map(function(x) `number->string`(x * 2))->join(",");
// => "2,4,6"

// Split a string, process the list, join back
"Alice, Bob, Carol"->split(",")
    ->map(function(s) s->trim()->upper())
    ->join(" & ");
// => "ALICE & BOB & CAROL"
```

Methods that return lists can be followed by more list methods. Methods that return a string (like `->join`) switch to string methods. Methods that return a vector (like `->to_vector`) switch to vector methods. The dispatch follows the result type automatically.

## Comparison: OO vs prefix style

List methods are OO sugar over the same underlying Scheme functions. Use whichever reads better:

```
// OO style — reads left-to-right, chains naturally
[1, 2, 3, 4]->filter(function(x) x > 2)->map(function(x) x * x);

// Prefix style — traditional, works with any function
map(function(x) x * x, filter(function(x) x > 2, [1, 2, 3, 4]));
```

The prefix functions (`map`, `filter`, `fold`, etc.) take the list as the **last** argument. The OO methods take it as the **receiver** (the object before `->`).

## Compatibility

The `->` operator uses a type-based dispatch function `__send__`. Lists, vectors, strings, and callable objects each get their own dispatch:

```
// Lists use list methods
[1, 2, 3]->map(function(x) x * 2);    // => [2, 4, 6]

// Vectors use vector methods
#[1, 2, 3]->length;                    // => 3

// Strings use string methods
"hello"->upper();                       // => "HELLO"

// Interfaces still work
define Point = constructor(x, y)
    interface(x: function() x, y: function() y);
define p = Point(3, 4);
p->x();                                 // => 3

// Records still work
record Vec(a, b);
define v = Vec(1, 2);
v->a;                                   // => 1
```

## Complete reference

### Properties (no parentheses)

| Property | Type | Description | Bracket equivalent |
|----------|------|-------------|-------------------|
| `->length` | integer | List length | — |
| `->empty?` | boolean | True if empty | — |
| `->first` | any | First element | `xs[0]` |
| `->second` | any | Second element | `xs[1]` |
| `->third` | any | Third element | `xs[2]` |
| `->last` | any | Last element | `xs[-1]` |
| `->rest` | list | All but first | `xs[1:]` |

### Methods (call with parentheses)

| Method | Returns | Description |
|--------|---------|-------------|
| `->ref(i)` | any | Element at index |
| `->slice(start, end)` | list | Sublist |
| `->map(fn)` | list | Transform elements |
| `->filter(fn)` | list | Keep matching elements |
| `->reject(fn)` | list | Remove matching elements |
| `->fold(fn, init)` | any | Left fold |
| `->fold_right(fn, init)` | any | Right fold |
| `->for_each(fn)` | void | Iterate (side effects) |
| `->flat_map(fn)` | list | Map then flatten |
| `->any(pred)` | boolean | Any element matches? |
| `->every(pred)` | boolean | All elements match? |
| `->find(pred)` | any/false | First matching element |
| `->count(pred)` | integer | Count matching elements |
| `->index_of(pred)` | int/false | Index of first match |
| `->contains(x)` | boolean | Element membership |
| `->reverse()` | list | Reversed copy |
| `->append(other)` | list | Concatenate lists |
| `->flatten()` | list | Flatten one level |
| `->take(n)` | list | First n elements |
| `->drop(n)` | list | Skip first n elements |
| `->take_while(pred)` | list | Take while true |
| `->drop_while(pred)` | list | Drop while true |
| `->sort(cmp)` | list | Sorted copy |
| `->unique()` | list | Remove duplicates |
| `->partition(pred)` | [list, list] | Split by predicate |
| `->zip(other)` | list | Pair up elements |
| `->join(sep)` | string | Join with separator |
| `->to_vector()` | vector | Convert to vector |
| `->copy()` | list | Shallow copy |
| `->take_right(n)` | list | Last n elements |
| `->drop_right(n)` | list | All but last n elements |
| `->split_at(n)` | [list, list] | Split at position |
| `->span(pred)` | [list, list] | Split where predicate changes |
| `->filter_map(fn)` | list | Map + filter (keep non-false) |
| `->reduce(fn)` | any | Left fold (no init, error if empty) |
| `->reduce_right(fn)` | any | Right fold (no init, error if empty) |
| `->delete(x)` | list | Remove all occurrences of x |
| `->bind(fn)` | list | Monadic bind (same as flat_map) |
| `->pure(v)` | list | Wrap value in list: `[v]` |
| `->mappend(other)` | list | Monoidal append (same as append) |
