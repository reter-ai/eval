# Dict Methods

Eval provides Python/JS-style OO methods on dicts via the `->` operator. Dicts are mutable hash-tables created with `dict(key: value, ...)`:

```
define d = dict(name: "Alice", age: 30, city: "NYC");
d->name;                     // => "Alice"
d->keys;                     // => (name age city)
d->filter(function(k, v) v != "NYC");  // => dict with name and age
```

Dicts support both arrow access (`d->name`) and bracket indexing (`d["name"]`). Keys are symbols internally; string keys are auto-converted.

## Creation

```
dict(x: 10, y: 20)          // key: value pairs
dict()                       // empty dict
```

The `dict(key: value, ...)` syntax uses the same `key: value` entries as `interface`. Keys are symbols.

## Predicate

```
dict?(d);         // => true
dict?(42);        // => false
dict?([1, 2]);    // => false
```

`dict?` is equivalent to `hash-table?`.

## Bracket indexing

Dicts support bracket indexing with string keys:

```
define d = dict(x: 10, y: 20);
d["x"];           // => 10
d["y"];           // => 20
d["missing"];     // => false
```

String keys are auto-converted to symbols. This is equivalent to `d->get("x")`.

Bracket indexing compiles to `ref`, which detects hash-tables and dispatches accordingly:

```
d["x"]     ->  (ref d "x")     // hash-table-ref/default with string→symbol conversion
xs[0]      ->  (ref xs 0)      // unchanged for lists/vectors/strings
```

## Properties

Properties return a value directly, no parentheses needed:

```
define d = dict(a: 1, b: 2, c: 3);
d->length;        // => 3
d->size;          // => 3
d->empty?;        // => false
d->keys;          // => (a b c)
d->values;        // => (1 2 3)
d->entries;       // => ((a . 1) (b . 2) (c . 3))
dict()->empty?;   // => true
```

| Property | Returns | Description |
|----------|---------|-------------|
| `->length` | integer | Number of entries |
| `->size` | integer | Alias for `length` |
| `->empty?` | boolean | True if no entries |
| `->keys` | list | List of keys (symbols) |
| `->values` | list | List of values |
| `->entries` | list | List of `(key . value)` pairs |

## Field access

Accessing a key that isn't a known property or method returns the field value:

```
define d = dict(name: "Alice", age: 30);
d->name;          // => "Alice"
d->age;           // => 30
d->missing;       // => false
```

This is equivalent to bracket indexing: `d->name` and `d["name"]` return the same value.

Note: if a field name conflicts with a method name (e.g., a field named `map`), the method takes priority. Use `d->get("map")` or `d["map"]` to access the field value.

## Methods

Methods return a closure that you call with `()`.

### Lookup and mutation

```
define d = dict(x: 10, y: 20);
d->get("x");              // => 10
d->get("missing");        // => false
d->get("missing", 99);   // => 99 (default value)
d->has?("x");             // => true
d->has?("z");             // => false

d->set("z", 30);          // adds z: 30
d->delete("y");           // removes y
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->get(key)` | any/false | Get value, `false` if missing |
| `->get(key, default)` | any | Get value with fallback |
| `->set(key, value)` | void | Add or overwrite entry (mutates) |
| `->delete(key)` | void | Remove entry (mutates) |
| `->has?(key)` | boolean | Check key existence |
| `->update(key, fn)` | void | Apply `fn` to current value, store result (mutates) |
| `->merge!(other)` | void | Merge other dict into this one (mutates) |
| `->clear()` | void | Remove all entries (mutates) |

```
define d = dict(score: 0);
d->update("score", function(v) v + 10);
d->score;                  // => 10
```

### Transformation

These methods return a **new dict**, leaving the original unchanged:

```
define d = dict(a: 1, b: 2, c: 3);

d->map(function(k, v) v * 10);
// => dict(a: 10, b: 20, c: 30)

d->filter(function(k, v) v > 1);
// => dict(b: 2, c: 3)

d->reject(function(k, v) v > 1);
// => dict(a: 1)

d->merge(dict(c: 99, d: 4));
// => dict(a: 1, b: 2, c: 99, d: 4)

d->copy();
// => dict(a: 1, b: 2, c: 3)   (independent copy)
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->map(fn)` | dict | Transform values: `fn(key, value)` returns new value |
| `->filter(fn)` | dict | Keep entries where `fn(key, value)` is true |
| `->reject(fn)` | dict | Remove entries where `fn(key, value)` is true |
| `->merge(other)` | dict | Combine two dicts (other's values override) |
| `->copy()` | dict | Shallow copy |

Note: `->map` transforms values only — keys are preserved. The callback receives `(key, value)` and returns the new value.

### Folding and iteration

```
define d = dict(a: 1, b: 2, c: 3);

d->fold(function(k, v, acc) acc + v, 0);
// => 6

d->for_each(function(k, v) display(k));
// prints: abc (side effects only)
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->fold(fn, init)` | any | Fold: `fn(key, value, accumulator)` |
| `->for_each(fn)` | void | Iterate: `fn(key, value)` for side effects |

### Searching

```
define d = dict(a: 1, b: 2, c: 3);

d->any(function(k, v) v > 2);      // => true
d->every(function(k, v) v > 0);    // => true
d->find(function(k, v) v == 2);    // => (b . 2)
d->find(function(k, v) v > 10);   // => false
d->count(function(k, v) v > 1);    // => 2
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->any(fn)` | boolean | True if any entry satisfies `fn(key, value)` |
| `->every(fn)` | boolean | True if all entries satisfy `fn(key, value)` |
| `->find(fn)` | pair/false | First `(key . value)` where `fn` is true, or `false` |
| `->count(fn)` | integer | Number of entries satisfying `fn(key, value)` |

### Conversion

```
define d = dict(x: 10);
d->to_list();              // => ((x . 10))
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->to_list()` | list | Convert to association list of `(key . value)` pairs |

## Chaining

Methods that return a new dict can be chained:

```
dict(a: 1, b: 2, c: 3, d: 4)
    ->filter(function(k, v) v > 1)
    ->map(function(k, v) v * 100);
// => dict(b: 200, c: 300, d: 400)
```

Methods that return a non-dict (like `->fold`, `->any`) end the dict chain.

## Three ways to access values

```
define d = dict(name: "Alice");

// 1. Arrow field access (symbol key)
d->name;                   // => "Alice"

// 2. Bracket indexing (string key)
d["name"];                 // => "Alice"

// 3. Get method (string key, with optional default)
d->get("name");            // => "Alice"
d->get("name", "unknown"); // => "Alice"
d->get("missing", "unknown"); // => "unknown"
```

All three return the same value for existing keys. For missing keys:
- Arrow and bracket return `false`
- `->get` returns `false` (or custom default if provided)

## Nested dicts

```
define config = dict(
    db: dict(host: "localhost", port: 5432),
    app: dict(debug: true, name: "myapp")
);

config->db->host;          // => "localhost"
config->app->name;         // => "myapp"
config["db"]["port"];      // => 5432
```

## Complete reference

### Properties (no parentheses)

| Property | Type | Description |
|----------|------|-------------|
| `->length` | integer | Number of entries |
| `->size` | integer | Number of entries (alias) |
| `->empty?` | boolean | True if no entries |
| `->keys` | list | All keys (symbols) |
| `->values` | list | All values |
| `->entries` | list | All `(key . value)` pairs |

### Methods (call with parentheses)

| Method | Returns | Description |
|--------|---------|-------------|
| `->get(key)` | any/false | Lookup by key |
| `->get(key, default)` | any | Lookup with fallback |
| `->set(key, value)` | void | Add/overwrite (mutates) |
| `->delete(key)` | void | Remove (mutates) |
| `->has?(key)` | boolean | Key exists? |
| `->update(key, fn)` | void | Update value in-place (mutates) |
| `->map(fn)` | dict | Transform values |
| `->filter(fn)` | dict | Keep matching entries |
| `->reject(fn)` | dict | Remove matching entries |
| `->fold(fn, init)` | any | Fold over entries |
| `->for_each(fn)` | void | Iterate for side effects |
| `->any(fn)` | boolean | Any entry matches? |
| `->every(fn)` | boolean | All entries match? |
| `->find(fn)` | pair/false | First matching entry |
| `->count(fn)` | integer | Count matching entries |
| `->merge(other)` | dict | Combine with other dict |
| `->merge!(other)` | void | Merge into this dict (mutates) |
| `->copy()` | dict | Shallow copy |
| `->clear()` | void | Remove all entries (mutates) |
| `->to_list()` | list | Convert to alist |
