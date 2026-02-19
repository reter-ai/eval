# Eval Language Introduction

Eval is an infix language that compiles to Scheme (chibi-scheme). It gives you familiar C-style syntax while retaining the full power of Scheme: first-class functions, continuations, proper tail calls, and a full numeric tower.

Every Eval expression compiles down to an s-expression. You can think of Eval as syntax sugar over Scheme.

## Comments

```
// line comment

/* block
   comment */
```

## Literals

```
42              // integer
0xFF            // hex integer
3.14            // float
1e-2            // scientific notation
"hello\n"       // string (escapes: \n \t \r \\ \" \0)
"""multi
line"""         // triple-quoted raw string (literal newlines, embedded "quotes", no escapes)
true            // boolean #t
false           // boolean #f
nil             // empty list '()
```

### nil semantics

`nil` is the empty list `()` — it is **not** a null/None/undefined value. In Eval (following Scheme):

- Only `false` is falsy. Everything else is truthy, **including `nil`**.
- `nil == []` is `true` — both are the empty list.
- `nil == false` is `false` — they are different values.
- `null?(nil)` is `true`, `null?([])` is `true`.

```
if(nil) "truthy" else "falsy";   // => "truthy"  (nil is NOT false)
nil == [];                        // => true
nil == false;                     // => false
null?(nil);                       // => true
```

There is no separate null type. Use `false` or `#f` when you need a falsy "nothing" value. Use `nil` or `[]` when you need an empty collection.

### Symbols

The single-quote operator creates symbols (quoted identifiers):

```
'hello          // the symbol hello
'()             // the empty list
```

### Collections

```
[1, 2, 3]       // list   -> (list 1 2 3)
[]               // empty list
#[1, 2, 3]      // vector -> (vector 1 2 3)
#[]              // empty vector
(1 .. 2)         // dotted pair -> (cons 1 2)   i.e. (1 . 2)
(1, 2, 3 .. 4)   // nested cons -> (1 2 3 . 4)
```

## Indexing and Slicing

Python-style bracket indexing and slicing works on lists, vectors, and strings:

### Indexing `expr[i]`

```
[10, 20, 30][0]         // => 10
[10, 20, 30][2]         // => 30
[10, 20, 30][-1]        // => 30 (negative = from end)
[10, 20, 30][-2]        // => 20

#[10, 20, 30][1]        // => 20 (vector)
"hello"[0]              // => #\h (string → character)
"hello"[-1]             // => #\o
```

Indexing compiles to `(ref expr i)`.

### Slicing `expr[s:e]`

Slicing returns a new list, vector, or string. The start is inclusive, the end is exclusive:

```
[1, 2, 3, 4, 5][1:3]   // => [2, 3]
[1, 2, 3][:2]           // => [1, 2]      (omitted start = 0)
[1, 2, 3][1:]           // => [2, 3]      (omitted end = length)
[1, 2, 3][:]            // => [1, 2, 3]   (full copy)

// Negative indices
[1, 2, 3, 4, 5][-3:-1]  // => [3, 4]

// String slicing returns a string
"hello"[1:3]             // => "el"
"hello"[:3]              // => "hel"
"hello"[2:]              // => "llo"

// Vector slicing returns a vector
#[10, 20, 30, 40][1:3]  // => #[20, 30]
```

Out-of-range slice bounds are clamped (no error), but out-of-range indexing raises an error.

Slicing compiles to `(slice expr start end)` where omitted bounds are `#f`.

### Arrow equivalents

Lists and vectors also support OO-style element access via `->`, which mirrors bracket indexing:

```
[10, 20, 30]->first      // same as [10, 20, 30][0]
[10, 20, 30]->second     // same as [10, 20, 30][1]
[10, 20, 30]->last       // same as [10, 20, 30][-1]
[10, 20, 30]->rest       // same as [10, 20, 30][1:]
[10, 20, 30]->ref(1)     // same as [10, 20, 30][1]
```

Use brackets for computed indices and slicing. Use arrows for named access (`->first`, `->second`, `->rest`) that reads like field names. See [INDEXING.md](INDEXING.md) for a detailed comparison.

### Chained indexing

Indexing is a postfix operator with the same precedence as function calls and `->`, so it chains naturally:

```
[[1, 2], [3, 4]][1][0]  // => 3
define matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
matrix[1][2]             // => 6
```

## Variables

```
define x = 42;       // create new variable    -> (define x 42)
x = 100;             // mutate existing        -> (set! x 100)
x += 5;              // compound add           -> (set! x (+ x 5))
x -= 3;              // compound subtract      -> (set! x (- x 3))
x++;                 // increment              -> (set! x (+ x 1))
x--;                 // decrement              -> (set! x (- x 1))
```

`define` creates a new binding. Plain `=` mutates an existing one (Scheme `set!`).

## Operators

### Arithmetic

| Syntax | Meaning | Scheme |
|--------|---------|--------|
| `a + b` | addition | `(+ a b)` |
| `a - b` | subtraction | `(- a b)` |
| `a * b` | multiplication | `(* a b)` |
| `a / b` | division | `(/ a b)` |
| `a % b` | modulo | `(modulo a b)` |
| `a ** b` | exponentiation | `(expt a b)` |
| `-a` | negation | `(- a)` |

### Comparison

| Syntax | Meaning | Scheme |
|--------|---------|--------|
| `a == b` | structural equality | `(equal? a b)` |
| `a != b` | not equal | `(not (equal? a b))` |
| `a =? b` | identity (pointer) equality | `(eq? a b)` |
| `a < b` | less than | `(< a b)` |
| `a > b` | greater than | `(> a b)` |
| `a <= b` | less or equal | `(<= a b)` |
| `a >= b` | greater or equal | `(>= a b)` |

`==` compares by value (deep structural equality): `"abc" == "abc"` is `true`, `[1, 2] == [1, 2]` is `true`. Use `=?` only when you need identity comparison (same object), which is mainly useful for symbols and booleans.

### Logical

| Syntax | Meaning | Scheme |
|--------|---------|--------|
| `a && b` | logical and (short-circuit) | `(and a b)` |
| `a \|\| b` | logical or (short-circuit) | `(or a b)` |
| `!a` | logical not | `(not a)` |

### Bitwise

| Syntax | Meaning | Scheme |
|--------|---------|--------|
| `a & b` | bitwise and | `(bitwise-and a b)` |
| `a \| b` | bitwise or | `(bitwise-ior a b)` |
| `~a` | bitwise not | `(bitwise-not a)` |
| `a << b` | shift left | `(arithmetic-shift a b)` |
| `a >> b` | shift right | `(arithmetic-shift a (- b))` |

### String

| Syntax | Meaning | Scheme |
|--------|---------|--------|
| `a ++ b` | string concatenation | `(string-append a b)` |

The `++` operator is contextually distinguished from the postfix increment `x++`. When followed by an expression it's concatenation; when followed by `;`, `)`, etc. it's increment:

```
"hello" ++ " world";    // => "hello world"
x ++ y;                 // (string-append x y)
i++;                    // (set! i (+ i 1))
```

### Operators as values

Operators can be used as first-class values when they appear in value position (after `(`, `,`, etc.):

```
fold(+, 0, [1, 2, 3])           // => 6
map(-, [1, 2, 3])               // => [-1, -2, -3]
sort([3, 1, 2], <)              // => [1, 2, 3]
fold(++, "", ["a", "b", "c"])   // => "abc"
```

The string-based alternative `op("+")` also works.

### Precedence (low to high)

```
=  +=  -=              (right)
||                     (left)
&&                     (left)
|                      (left)
&                      (left)
==  =?  !=             (left)
<  >  <=  >=           (left)
<<  >>                 (left)
+  -  ++               (left)
*  /  %                (left)
**                     (right)
-  !  ~  '             (right, unary)
()  []  ->  ++  --     (left, postfix)
```

## Functions

```
// Simple — single expression body
define double = function(x) x * 2;

// Block body — multiple statements, last expression is the return value
define abs = function(x) {
    if(x < 0) return -x;
    x;
};

// No arguments
define greet = function() "hello";

// Variadic (rest parameter)
define all = function(..args) args;             // all(1,2,3) => [1,2,3]
define head_tail = function(h ..rest) [h, rest]; // head_tail(1,2,3) => [1, [2,3]]

// Immediate call
(function(x) x * x)(5);   // => 25
```

### Early return

`return` is available inside `function` bodies. The compiler wraps the body in `call-with-current-continuation` only when `return` is actually used, so there is no overhead when it isn't:

```
define find_first = function(lst, pred) {
    `for-each`(function(x) {
        when(pred(x)) return x;
    }, lst);
    false;
};
```

## Control Flow

### if / else

```
if(x > 0) "positive" else "non-positive"
if(debug) print("trace");
```

`if` is an expression and returns a value.

### when / unless

```
when(debug) print("trace");        // -> (if debug (print "trace"))
unless(valid) error("bad input");  // -> (if (not valid) (error "bad input"))
```

### cond (multi-way branch)

```
cond(
    x < 0:  "negative",
    x == 0: "zero",
    else:   "positive"
)
```

### case (value match)

```
case(color,
    ("red"):   "stop",
    ("green"): "go",
    else:      "caution"
)
```

## Loops

### while

```
define i = 0;
while(i < 10) {
    display(i); display(" ");
    i++;
};
```

### for

```
// C-style: for(init, condition, step) body
for(let i = 0, i < 5, i++) {
    print(i);
};

// `let` in the init position scopes the variable to the loop.
// Without let, use a pre-defined variable:
define j = 0;
for(j = 0, j < 5, j++) print(j);
```

### for-each (iterate a collection)

```
for(let x in [1, 2, 3]) print(x);
```

### do-until

```
define k = 0;
do k++ until(k >= 5);
```

### break

`break` exits the nearest enclosing `while`, `for`, or `do-until`:

```
define i = 0;
while(true) {
    if(i >= 10) break;
    i++;
};
```

## Blocks

Curly braces create a block. The last expression in the block is its value:

```
define result = {
    define a = 10;
    define b = 20;
    a + b
};
// result is 30
```

A trailing semicolon after the last expression is optional: `{ a; b; c }` and `{ a; b; c; }` are equivalent.

Defines inside blocks are scoped with `letrec`, so mutual recursion works:

```
{
    define even? = function(n) if(n == 0) true else odd?(n - 1);
    define odd? = function(n) if(n == 0) false else even?(n - 1);
    even?(10);
}
// => true
```

## Let Bindings

```
let(x = 1, y = 2) x + y              // => 3, parallel binding
let*(x = 1, y = x + 1) y             // => 2, sequential binding
letrec(f = function(n)                // recursive binding
    if(n <= 1) 1 else n * f(n - 1))
  f(10)
```

## Error Handling

### try / catch

```
try {
    1 / 0;
} catch(e) {
    display("caught: ");
    display(e);
    -1;
}
```

### Multi-clause catch

```
try expr catch(e,
    string?(e): display(e),
    pair?(e):   display(car(e))
)
```

### finally

`finally` guarantees a cleanup expression runs whether the body succeeds or throws:

```
// try/catch/finally — cleanup always runs
try {
    open_resource();
    do_work();
} catch(e) {
    handle_error(e);
} finally {
    close_resource();   // always runs
}
```

```
// try/finally (no catch) — error still propagates after cleanup
try {
    do_work();
} finally {
    close_resource();
}
```

The `finally` block does not affect the return value — the result is whatever the body (or catch) returned. `finally` is implemented via `dynamic-wind`, so it runs even if a continuation jumps out of the body.

```
define r = try { 42; } catch(e) { 0; } finally { 999; };
// r is 42, not 999 — finally runs for side effects only
```

### with (RAII)

`with` binds resources and guarantees their `close` method is called on scope exit, even on error:

```
with(f = open_file("data.txt")) {
    process(f);
}
// f->close() called automatically
```

Multiple resources — inner resources close first:

```
with(db = connect(), tx = db->begin()) {
    tx->execute("INSERT ...");
}
// tx->close() called first, then db->close()
```

Any object with a `close` method works (constructors, interfaces, dicts). The body's return value is preserved — `close` runs for side effects only.

Statement form — `with` as a declaration scopes cleanup to the enclosing block:

```
{
    define data = load_config();
    with(db = connect(data->host));
    with(tx = db->begin_transaction());
    tx->execute("INSERT ...");
    tx->execute("UPDATE ...");
    // tx->close() called first, then db->close() when block exits
}
```

This is equivalent to the expression form but avoids nesting. Each `with(...);` statement wraps everything after it in the same block with automatic cleanup.

## Values and Receive

For returning multiple values:

```
define swap = function(a, b) values(b, a);
receive(x, y) from swap(1, 2) [x, y];   // => [2, 1]
```

## Continuations

```
callcc(function(k) {
    k(42);
    // never reached
});
// => 42
```

Continuations are first-class and can be stored, serialized, and resumed.

## Object System: constructor / interface

Eval has a lightweight prototype-based object system built on closures.

### interface

`interface` creates a message-dispatch function. Each entry maps a name to a value:

```
define obj = interface(
    x: 10,
    y: 20,
    sum: function() x + y
);

obj->x;       // => 10
obj->sum();   // => 30
```

The `->` operator sends a message: `obj->x` compiles to `(__send__ obj 'x)`. Dispatch is type-based: lists get list methods (see [LISTS.md](LISTS.md)), vectors get vector methods (see [VECTORS.md](VECTORS.md)), strings get string methods (see [STRINGS.md](STRINGS.md)), dicts get dict methods (see [DICTS.md](DICTS.md)), and all other types (interfaces, constructors, records) are called directly.

Under the hood, `interface(...)` becomes:

```scheme
(lambda (__msg__)
  (cond
    ((eq? __msg__ 'x) 10)
    ((eq? __msg__ 'y) 20)
    ((eq? __msg__ 'sum) (lambda () (+ x y)))
    (else ... search supers ...)))
```

### constructor

`constructor` wraps `interface` in a factory function (like a class):

```
define Point = constructor(x, y)
    interface(
        x: function() x,
        y: function() y,
        dist: function() (x**2 + y**2) ** 0.5
    );

define p = Point(3, 4);
p->x();      // => 3
p->dist();   // => 5.0
```

Constructor body can be a block with local state:

```
define Counter = constructor(init) {
    define count = init;
    interface(
        get: function() count,
        inc: function() { count += 1; count; }
    );
};

define c = Counter(0);
c->inc();       // => 1
c->inc();       // => 2
c->get();       // => 2
```

### Inheritance with super

`super` adds a parent object to the lookup chain. When a message isn't found in the current interface, it's forwarded to parents:

```
define Animal = constructor(name)
    interface(
        name: function() name,
        speak: function() "..."
    );

define Dog = constructor(name) {
    super Animal(name);
    interface(
        speak: function() "woof!"
    );
};

define d = Dog("Rex");
d->speak();   // => "woof!"     (own method)
d->name();    // => "Rex"       (inherited from Animal)
```

### Virtual methods (polymorphism)

Method dispatch is virtual by default. A list of mixed types responds to the same message with different behavior:

```
define Animal = constructor(name)
    interface(name: function() name, speak: function() "...");

define Dog = constructor(name) {
    super Animal(name);
    interface(speak: function() "woof");
};

define Cat = constructor(name) {
    super Animal(name);
    interface(speak: function() "meow");
};

define animals = [Dog("Rex"), Cat("Whiskers"), Animal("???")];
`for-each`(function(a) {
    display(a->name()); display(" says "); display(a->speak()); newline();
}, animals);
// Rex says woof
// Whiskers says meow
// ??? says ...
```

Multi-level inheritance works too. Methods resolve by walking up the super chain:

```
define Puppy = constructor(name) {
    super Dog(name);
    interface(speak: function() "yip!");
};

define p = Puppy("Tiny");
p->speak();   // => "yip!"   (own, overrides Dog)
p->name();    // => "Tiny"   (inherited from Animal, through Dog)
```

If a child doesn't override a method, the parent's version is used:

```
define GuideDog = constructor(name) {
    super Dog(name);
    interface(trained: function() true);
};

define gd = GuideDog("Lassie");
gd->speak();     // => "woof"   (from Dog)
gd->name();      // => "Lassie" (from Animal)
gd->trained();   // => true     (own)
```

Calling an unknown message raises an error:

```
try gd->fly() catch(e) display("no such method");
// => no such method
```

### How it works

- `constructor(params) body` compiles to a lambda that defines `__name__`, `__supers__`, and `self` internally, then returns `self`.
- `interface(...)` compiles to a lambda that dispatches on a quoted symbol (`__msg__`).
- `super Parent(args)` appends `Parent(args)` to the `__supers__` list.
- When a message isn't found, the interface's `else` clause searches `__supers__` by calling `((car __supers__) __msg__)`.
- `area: abstract` in an interface compiles to `(lambda __args__ (error "abstract method" "area"))` — a placeholder that throws.
- `abstract constructor(...)` wraps the normal constructor in a guard that checks `__abstract_ok__`. Direct calls error; `super` sets the flag before calling the parent.

### Abstract methods

Use `abstract` in an interface entry to declare a method that must be overridden by subclasses. An abstract method is a placeholder that throws an error if called:

```
define Shape = constructor()
    interface(
        area: abstract,
        name: function() "shape"
    );

Shape()->name();     // => "shape" (works fine)
Shape()->area();     // ERROR: "abstract method: area"
```

Subclasses override abstract methods by providing their own implementation. The child's interface entry matches first, so the parent's abstract placeholder is never reached:

```
define Circle = constructor(r) {
    super Shape();
    interface(area: function() 3.14159 * r * r);
};

Circle(5)->area();   // => 78.53975
Circle(5)->name();   // => "shape" (inherited)
```

### Abstract classes

Prefix `constructor` with `abstract` to prevent direct instantiation. Only subclasses (via `super`) can create instances:

```
define Animal = abstract constructor()
    interface(
        speak: abstract,
        kind: function() "animal"
    );

Animal();            // ERROR: "cannot instantiate abstract class"

define Dog = constructor() {
    super Animal();
    interface(speak: function() "woof");
};

Dog()->speak();      // => "woof"
Dog()->kind();       // => "animal" (inherited)
```

The mechanism uses a global flag `__abstract_ok__` that `super` sets before calling the parent constructor. Direct calls leave the flag unset, triggering the error.

### Abstract + static

Abstract constructors work with `static` for class-level properties accessible without instantiation:

```
define Shape = abstract constructor()
    static(types: ["circle", "rect"])
    interface(area: abstract);

Shape->types;        // => ["circle", "rect"] (statics accessible)
Shape();             // ERROR: "cannot instantiate abstract class"

define Circle = constructor(r) {
    super Shape();
    interface(area: function() 3.14159 * r * r);
};
Circle(5)->area();   // => 78.53975
```

### Chained abstract inheritance

Abstract classes can inherit from other abstract classes. Each level can declare its own abstract methods:

```
define A = abstract constructor()
    interface(foo: abstract, base: function() "A");

define B = abstract constructor() {
    super A();
    interface(bar: abstract);
};

define C = constructor() {
    super B();
    interface(
        foo: function() "foo-impl",
        bar: function() "bar-impl"
    );
};

C()->foo();          // => "foo-impl"
C()->bar();          // => "bar-impl"
C()->base();         // => "A" (inherited through chain)
A();                 // ERROR
B();                 // ERROR
```

## Records

`record` defines a simple data type backed by a vector, with `->` access like OOP objects:

```
record Point(x, y);

define p = Point(3, 4);
p->x;            // => 3
p->y;            // => 4
Point?(p);       // => true
Point?(42);      // => false
```

Records are immutable data carriers. Access uses the same `->` syntax as constructors, but there are no methods or inheritance — just field access and a type predicate.

### constructor/interface vs record

| Feature | `constructor` / `interface` | `record` |
|---------|---------------------------|----------|
| **Nature** | Closure-based objects | Tagged vectors |
| **Access** | `obj->field` | `obj->field` |
| **Mutation** | Closures over mutable state | Immutable fields |
| **Inheritance** | `super` keyword | None |
| **Abstract** | `abstract` methods and classes | None |
| **Methods** | Interface entries can be functions | None (data only) |
| **Type check** | Not built-in | `Type?(obj)` predicate |
| **Use case** | Behavior, encapsulation, polymorphism | Simple data aggregates, tagged unions |

Use `record` for plain data carriers (like a struct). Use `constructor`/`interface` when you need behavior, encapsulation, or inheritance.

## Dictionaries

`dict` creates a mutable hash-table-backed dictionary with `->` access and bracket indexing:

```
define d = dict(name: "Alice", age: 30, city: "NYC");
d->name;         // => "Alice"
d["name"];       // => "Alice" (bracket indexing)
d->age;          // => 30
```

The `dict(key: value, ...)` syntax uses the same `key: value` entries as `interface`. Keys are symbols internally; string keys are auto-converted.

### Properties (no parentheses)

```
d->length;                // => 3
d->size;                  // => 3 (alias)
d->empty?;                // => false
d->keys;                  // => (name age city)
d->values;                // => ("Alice" 30 "NYC")
d->entries;               // => ((name . "Alice") (age . 30) (city . "NYC"))
```

### Methods

```
d->get("name");           // dynamic get by string key => "Alice"
d->get("missing");        // => false (not found)
d->get("missing", "?");   // => "?" (with default)
d->set("email", "a@b");   // add or overwrite a key
d->delete("city");        // remove a key
d->has?("name");          // => true
d->to_list();             // => association list ((key . value) ...)
```

### Iteration and transformation

```
define nums = dict(a: 1, b: 2, c: 3);
nums->map(function(k, v) v * 10);              // => dict(a: 10, b: 20, c: 30)
nums->filter(function(k, v) v > 1);            // => dict(b: 2, c: 3)
nums->fold(function(k, v, acc) acc + v, 0);    // => 6
nums->any(function(k, v) v > 2);               // => true
nums->every(function(k, v) v > 0);             // => true
nums->find(function(k, v) v == 2);             // => (b . 2)
nums->count(function(k, v) v > 1);             // => 2
nums->merge(dict(d: 4));                        // => dict(a: 1, b: 2, c: 3, d: 4)
nums->copy();                                   // => independent copy
```

### Predicate

```
dict?(d);        // => true
dict?(42);       // => false
```

See [DICTS.md](DICTS.md) for the complete method reference.

## Comprehensions

List comprehensions provide concise syntax for transforming and filtering collections:

```
[x * 2 for x in [1, 2, 3]];              // => [2, 4, 6]
[x for x in [1, 2, 3, 4] if x > 2];     // => [3, 4]
[x + y for x in [1, 2] for y in [10, 20]]; // => [11, 21, 12, 22]
```

Vector comprehensions use `#[...]`:

```
#[x * 2 for x in [1, 2, 3]];             // => #[2, 4, 6]
```

Dict comprehensions build dictionaries from key:value expressions:

```
dict(x: x * 2 for x in [1, 2, 3]);       // => {1: 2, 2: 4, 3: 6}
dict(x: x * x for x in [1, 2, 3] if x > 1); // => {2: 4, 3: 9}
```

Or from pair-producing expressions:

```
dict(cons(x, x * 10) for x in [1, 2, 3]); // => {1: 10, 2: 20, 3: 30}
```

All comprehension types accept generators as their source (see Generators below):

```
define count = generator(n) { for(let i = 0, i < n, i++) yield i; };
[x * 2 for x in count(5)];               // => [0, 2, 4, 6, 8]
#[x * x for x in count(4)];              // => #[0, 1, 4, 9]
dict(x: x * x for x in count(3));        // dict with 0:0, 1:1, 2:4
```

## Generators

Generators are lazy sequences built on continuations. Use the `generator` keyword to define a generator function, and `yield` to produce values:

```
define count = generator(n) {
    for(let i = 0, i < n, i++)
        yield i;
};
define g = count(3);
g();              // => 0
g();              // => 1
g();              // => 2
eof?(g());        // => true
```

`collect()` materializes a generator into a list:

```
collect(count(5));  // => [0, 1, 2, 3, 4]
```

`return` in a generator terminates it early:

```
define take_pos = generator(xs) {
    for(let x in xs) {
        if(x < 0) return nil;
        yield x;
    };
};
collect(take_pos([1, 2, -1, 3]));  // => [1, 2]
```

Generators can be infinite — only values that are consumed get computed:

```
define naturals = generator() {
    define i = 0;
    while(true) { yield i; i++; };
};
define ng = naturals();
ng();  // => 0
ng();  // => 1
```

### Generator Comprehensions

Parenthesized comprehensions produce lazy generators (brackets `[...]` produce lists):

```
(x * 2 for x in [1, 2, 3]);           // lazy generator
collect((x * 2 for x in [1, 2, 3]));  // => [2, 4, 6]
```

With filter:

```
collect((x for x in [1, 2, 3, 4, 5] if x > 2));  // => [3, 4, 5]
```

Generators can chain — feeding one generator into another creates a lazy pipeline where no intermediate lists are created:

```
define g1 = (x * 2 for x in [1, 2, 3, 4, 5]);
define g2 = (x + 100 for x in g1);
collect(g2);  // => [102, 104, 106, 108, 110]
```

Generators work as sources for all comprehension types:

```
[x * 2 for x in count(5)];              // list from generator
#[x * x for x in count(4)];             // vector from generator
dict(x: x * x for x in count(3));       // dict from generator
```

See [GENERATORS.md](GENERATORS.md) for the complete guide including infinite generators, lazy pipelines, and compilation details.

## String Methods

Strings support OO-style methods via the `->` operator:

### Properties

```
"hello"->length;       // => 5
""->empty?;            // => true
```

### Case, trimming, searching

```
"hello"->upper();                     // => "HELLO"
"HELLO"->lower();                     // => "hello"
"  hi  "->trim();                     // => "hi"
"hello world"->contains("world");     // => true
"hello"->starts_with("hel");          // => true
"hello"->ends_with("llo");            // => true
"hello"->index_of("ll");             // => 2
```

### Replace, split, join

```
"hello world"->replace("world", "there");  // => "hello there"
"a,b,c"->split(",");                       // => ["a", "b", "c"]
","->join(["a", "b", "c"]);                // => "a,b,c"
```

### Substrings and conversion

```
"hello world"->slice(0, 5);     // => "hello"
"abc"->reverse();                // => "cba"
"ab"->repeat(3);                 // => "ababab"
"42"->to_number();               // => 42
```

### Chaining

Methods return strings, so calls chain naturally:

```
"  Hello World  "->trim()->upper();              // => "HELLO WORLD"
"  hello  "->trim()->upper()->reverse();          // => "OLLEH"
title->trim()->lower()->replace(" ", "-");         // slug from title
```

See [STRINGS.md](STRINGS.md) for the complete reference with all methods.

## List Methods

Lists support OO-style methods via the `->` operator:

### Properties

```
[1, 2, 3]->length;        // => 3
[1, 2, 3]->first;         // => 1
[1, 2, 3]->last;          // => 3
[1, 2]->empty?;           // => false
```

### Transformation, searching, sorting

```
[1, 2, 3]->map(function(x) x * 2);            // => [2, 4, 6]
[1, 2, 3, 4]->filter(function(x) x > 2);      // => [3, 4]
[1, 2, 3, 4]->reject(function(x) x <= 2);     // => [3, 4]
[3, 1, 2]->sort(<);                            // => [1, 2, 3]
[1, 2, 3, 4]->fold(+, 0);                     // => 10
[1, 2, 3]->any(function(x) x > 2);            // => true
[1, 2, 3]->find(function(x) x > 2);           // => 3
[1, 2, 3]->contains(2);                        // => true
[1, 2, 3]->reverse();                          // => [3, 2, 1]
[1, 2, 2, 3]->unique();                       // => [1, 2, 3]
```

### Taking, dropping, joining

```
[1, 2, 3, 4]->take(2);                         // => [1, 2]
[1, 2, 3, 4]->drop(2);                         // => [3, 4]
["a", "b", "c"]->join(",");                     // => "a,b,c"
[1, 2]->append([3, 4]);                        // => [1, 2, 3, 4]
[1, 2, 3]->to_vector();                        // => #[1, 2, 3]
```

### Chaining

```
[3, 1, 4, 2]->sort(<)->take(2)->map(function(x) x * 10);
// => [10, 20]

"Alice, Bob, Carol"->split(",")
    ->map(function(s) s->trim()->upper())
    ->join(" & ");
// => "ALICE & BOB & CAROL"
```

See [LISTS.md](LISTS.md) for the complete reference with all 30+ methods.

## Vector Methods

Vectors (`#[1, 2, 3]`) support OO-style methods similar to lists, with O(1) indexing and length:

```
#[1, 2, 3]->length;                            // => 3
#[]->empty?;                                    // => true
#[1, 2, 3]->map(function(x) x * 2);           // => #[2, 4, 6]
#[3, 1, 2]->sort(<);                           // => #[1, 2, 3]
#[1, 2, 3]->filter(function(x) x > 1);        // => #[2, 3]
#[1, 2, 3]->contains(2);                       // => true
#[1, 2, 3]->to_list();                         // => [1, 2, 3]

// In-place mutation
define v = #[10, 20, 30];
v->set(1, 99);
v->ref(1);                                      // => 99
```

Converting to a list unlocks the full list method set:

```
#[1, 2, 3]->to_list()->flat_map(function(x) [x, x]);
// => [1, 1, 2, 2, 3, 3]
```

See [VECTORS.md](VECTORS.md) for the complete reference with all 20+ methods.

## Include

Include other Eval files:

```
include("utils.eval");
include("shapes.eval", "transforms.eval");
```

If the filename ends with `.eval`, it is parsed as Eval. Otherwise it is loaded as Scheme. Files are resolved relative to the current working directory and any directories added with `-I`.

## Modules

### library / export / import

```
// In my_math.eval
library(my, math) {
    export(square, cube);
    define square = function(x) x * x;
    define cube = function(x) x * x * x;
};

// In main.eval
import(my, math);
square(5);   // => 25
```

## Macros

```
macro my_and syntax_rules() {
    (_, x):       x,
    (_, x, y):    if(x) y else false
};

my_and(true, 42);   // => 42
my_and(false, 42);  // => false
```

`macro name expr` compiles to `(define-syntax name expr)`. `syntax_rules` creates a pattern-matching transformer.

## Backtick Identifiers

Scheme names with hyphens or special characters can be used via backtick:

```
`string-append`("hello", " ", "world")   // => "hello world"
`char->integer`('A')                     // => 65
`for-each`(print, [1, 2, 3])
```

Any Eval identifier with underscores works for bridge functions that have underscore aliases (e.g., `string_append`), but backtick gives access to any Scheme name.

## Compile-Time Eval

The `!!` prefix evaluates an expression at parse time:

```
define pi = !!3.14159265358979;
```

## I/O

```
display(x);       // print value (no quotes on strings)
print(x);         // display + newline
newline();         // print newline
```

These are C bridge functions that write to stdout.

## Built-in Functions

### List operations (from Scheme)

```
car([1, 2, 3])       // => 1
cdr([1, 2, 3])       // => [2, 3]
cons(0, [1, 2, 3])   // => [0, 1, 2, 3]
length([1, 2, 3])    // => 3
append([1, 2], [3])  // => [1, 2, 3]
`reverse`([3, 2, 1]) // => [1, 2, 3]
list(1, 2, 3)        // => [1, 2, 3]
```

### Higher-order functions

```
map(function(x) x * 2, [1, 2, 3])                 // => [2, 4, 6]
filter(function(x) x > 2, [1, 2, 3, 4, 5])        // => [3, 4, 5]
fold(+, 0, [1, 2, 3, 4, 5])                       // => 15
apply(+, [1, 2, 3])                                // => 6
`for-each`(function(x) print(x), [1, 2, 3])
```

Note: `fold` follows SRFI-1 conventions where the combining function takes `(element, accumulator)`, not `(accumulator, element)`.

### Indexing and slicing functions

The `[]` syntax compiles to these functions, which can also be called directly:

```
ref([10, 20, 30], 1)            // => 20 (same as [10, 20, 30][1])
ref("hello", 0)                 // => #\h
ref(#[10, 20, 30], -1)          // => 30

slice([1, 2, 3, 4], 1, 3)       // => [2, 3]
slice("hello", 0, 3)            // => "hel"
slice(#[10, 20, 30], 1, false)  // => #[20, 30] (false = omit bound)
```

### Type predicates

```
number?(42)      // => true
string?("hi")    // => true
list?([1, 2])    // => true
pair?([1, 2])    // => true
vector?(#[1])    // => true
procedure?(+)    // => true
boolean?(true)   // => true
null?([])        // => true
symbol?('x)      // => true
```

### Sorting

```
sort([3, 1, 4, 1, 5], <)   // => [1, 1, 3, 4, 5]
sort([3, 1, 4, 1, 5], >)   // => [5, 4, 3, 1, 1]
```

### Hash tables

```
hash(42)                        // hash any value
`string-hash`("hello")         // hash a string
```

### JSON

```
define p = `open-output-string`();
json_write(#[1, "hello", true], p);
`get-output-string`(p);   // => "[1,\"hello\",true]"

define p2 = `open-input-string`("[1, 2, 3]");
json_read(p2);   // => #[1, 2, 3]
```

### Random numbers

```
random_integer(100)    // => integer in [0, 100)
random_real()          // => float in [0.0, 1.0)
```

### Time

```
current_clock_second()   // seconds since epoch (float)
current_second()         // alias
```

### Environment variables

```
get_env("PATH")          // => string or false
```

## Green Threads (SRFI-18)

```
define t = make_thread(function() {
    print("hello from thread");
});
thread_start(t);
thread_join(t);
```

Available functions: `make_thread`, `thread_start`, `thread_join`, `thread_yield`, `thread_sleep`, `thread_terminate`, `current_thread`, `make_mutex`, `mutex_lock`, `mutex_unlock`, `make_condvar`, `condvar_signal`, `condvar_broadcast`.

### OO Synchronization Wrappers

Eval provides OO wrappers with RAII for common synchronization patterns:

```
// Mutex — auto-unlock via with
define m = Mutex();
with(guard = m->lock()) {
    counter += 1;
};

// Monitor — wait/pulse (producer-consumer)
define mon = Monitor();
with(guard = mon->enter()) {
    while(!ready) mon->wait();
};

// ReadWriteLock — concurrent readers, exclusive writer
define rwl = ReadWriteLock();
with(guard = rwl->read_lock()) { /* read */ };
with(guard = rwl->write_lock()) { /* write */ };

// Semaphore — limit concurrency
define sem = Semaphore(3);
with(guard = sem->acquire()) { /* at most 3 */ };
```

See [MULTITHREADING.md](MULTITHREADING.md) for the full synchronization guide.

See [THREADS.md](THREADS.md) for a comprehensive guide to green threads, mutexes, condition variables, and cross-thread continuations.

## Async / Await

`async` spawns a green thread and returns a promise. `await` blocks until the promise resolves.

```
define p = async {
    expensive_computation();
};
define result = await(p);
```

### Concurrent tasks

```
define a = async fib(30);
define b = async fib(31);
define c = async fib(32);
// all three run concurrently as green threads
[await(a), await(b), await(c)];
```

### Error propagation

If an async expression throws, the error is captured in the promise and re-raised on `await`:

```
define p = async { error("boom"); };
try { await(p); } catch(e) display(e);   // catches "boom"
```

### Promise API

```
promise?(p);    // => true
p->ready?;      // => true if resolved (non-blocking check)
```

### Unified await

`await` works with both green-thread promises and thread pool futures:

```
// Green thread promise
define p = async 42;
await(p);                           // => 42

// Thread pool future
define pool = make_pool(2);
define f = pool_submit(pool, "6 * 7;");
await(f);                           // => 42
pool_shutdown(pool);
```

### async vs thread pool

| | `async expr` | Thread pool |
|---|---|---|
| **Mechanism** | Green thread (cooperative) | OS thread (true parallelism) |
| **Context** | Shared — can access local variables | Separate — code runs as string in isolated context |
| **Overhead** | Very low | Higher (serialization, thread creation) |
| **Use case** | Concurrent I/O, structured concurrency | CPU-bound parallelism |

See [ASYNC.md](ASYNC.md) for a comprehensive guide to async programming, thread pools, channels, and pipelines.

## Thread Pool

For true OS-level parallelism using worker threads. `Pool(n)` creates an OO pool
with `n` worker threads that supports `with` (RAII) for automatic shutdown:

```
// Pool with RAII — auto-shutdown on scope exit
define result = with(pool = Pool(4)) {
    define f1 = pool->submit("10 * 10;");
    define f2 = pool->submit("20 + 5;");
    await(f1) + await(f2);   // => 125
};

// Statement-form RAII
{
    with(pool = Pool(2));
    define f = pool->submit("6 * 7;");
    f->result();              // => 42 (blocks until done)
    f->ready?;                // => true (non-blocking check)
};                            // pool auto-shutdown here

// Manual lifecycle
define p = Pool(2);
define f = p->submit("1 + 2;");
await(f);                     // => 3
p->shutdown();                // explicit shutdown
p->shutdown();                // safe to call twice
```

### Pool Methods

| Method | Description |
|---|---|
| `pool->submit(code)` | Submit code string, returns OO Future |
| `pool->channel(name)` | Create named channel, returns OO Channel |
| `pool->shutdown()` | Shutdown pool (idempotent) |
| `pool->close()` | Alias for shutdown (for `with` RAII) |

### Futures

Futures returned by `pool->submit()` support both `->` methods and `await`:

| Method | Description |
|---|---|
| `f->result()` | Block until result (or re-raise error) |
| `f->ready?` | Non-blocking boolean check |
| `await(f)` | Unified await (works for both pool futures and async promises) |

### Channels

Cross-thread communication via named channels:

```
{
    with(pool = Pool(2));
    with(ch = pool->channel("data"));

    ch->send("hello");
    ch->send("world");
    ch->recv();               // => "hello"
    ch->recv();               // => "world"
    ch->try_recv();           // => false (empty)
};
```

| Method | Description |
|---|---|
| `ch->send(val)` | Send value to channel |
| `ch->recv()` | Blocking receive |
| `ch->try_recv()` | Non-blocking receive, returns `false` if empty |
| `ch->close()` | Close channel (for `with` RAII) |

### Low-level Pool API

The OO wrappers call these underlying functions:

```
define pool = make_pool(4);
define f = pool_submit(pool, "2 + 3;");
future_result(f);             // => 5
define ch = pool_channel(pool, "test");
channel_send(ch, 42);
channel_recv(ch);             // => 42
pool_shutdown(pool);
```

See [ASYNC.md](ASYNC.md) and [THREADS.md](THREADS.md) for comprehensive guides covering pool_apply, closures across threads, continuation exchange, and multi-stage pipelines.

## Reactive Programming

Fine-grained reactivity with automatic dependency tracking, similar to Solid.js or MobX. Signals hold mutable values, Computed values auto-track their dependencies and update lazily, Effects re-run when their dependencies change.

### Signal

A reactive cell that holds a mutable value:

```
define count = Signal(0);
count();                  // => 0 (read)
count->set(5);            // write
count();                  // => 5
count->update(function(v) v + 1);   // update via function
count();                  // => 6
count->peek;              // => 6 (read without tracking)
```

| Method | Description |
|---|---|
| `sig()` | Read value (auto-tracked) |
| `sig->set(val)` | Set new value |
| `sig->update(fn)` | Set to `fn(current)` |
| `sig->peek` | Read without tracking |

### Computed

A derived value that auto-tracks its dependencies and recomputes lazily when they change:

```
define a = Signal(2);
define b = Signal(3);
define sum = Computed(function() a() + b());
sum();      // => 5

a->set(10);
sum();      // => 13 (recomputed on read)
```

Computed values are cached — reading multiple times without changes doesn't recompute:

```
define calls = 0;
define doubled = Computed(function() {
    calls++;
    count() * 2;
});
doubled();   // computes
doubled();   // cached, no recompute
doubled();   // cached, no recompute
```

Computed chains propagate automatically:

```
define x = Signal(1);
define doubled = Computed(function() x() * 2);
define quadrupled = Computed(function() doubled() * 2);
quadrupled();   // => 4
x->set(3);
quadrupled();   // => 12
```

### Effect

A side effect that auto-tracks dependencies and re-runs when they change:

```
define name = Signal("Alice");
define eff = Effect(function() {
    print("Hello, " ++ name() ++ "!");
});
// prints: Hello, Alice!

name->set("Bob");
// prints: Hello, Bob!
```

If the effect function returns a procedure, it becomes the cleanup function that runs before the next execution:

```
define interval_id = Signal(0);
define eff = Effect(function() {
    define id = interval_id();
    print("started " ++ `number->string`(id));
    function() print("cleanup " ++ `number->string`(id));
});
// prints: started 0

interval_id->set(1);
// prints: cleanup 0
// prints: started 1
```

### batch

Batch multiple signal updates so effects fire only once with the final state:

```
define a = Signal(0);
define log = [];
define eff = Effect(function() {
    log = cons(a(), log);
});
// log = [0]

log = [];
batch(function() {
    a->set(1);
    a->set(2);
    a->set(3);
});
// log = [3] — effect fired once with final value
```

Batching prevents the diamond problem — when multiple paths lead to the same effect:

```
define A = Signal(1);
define B = Computed(function() A() * 2);
define C = Computed(function() A() + 10);
define log = [];
define D = Effect(function() {
    log = cons([B(), C()], log);
});

log = [];
A->set(5);
// log = [[10, 15]] — single effect run with consistent B and C
```

### dispose

Clean up a reactive node: unsubscribe from sources, run effect cleanup, stop future updates:

```
define s = Signal(0);
define eff = Effect(function() print(s()));
// prints: 0

dispose(eff);
s->set(99);
// nothing printed — effect is disposed
```

### RAII with `with`

Effects and other reactive nodes support `with` for automatic disposal:

```
define s = Signal(0);
with(e = Effect(function() print(s()))) {
    s->set(1);
};
// After with block, effect is automatically disposed
s->set(99);   // no effect — already disposed
```

### Type predicates

```
signal?(Signal(0));       // => true
computed?(Computed(function() 1));   // => true
effect?(Effect(function() 1));      // => true
```

### Auto-tracking

Dependencies are discovered automatically when reactive values are read inside `Computed` or `Effect`:

```
define flag = Signal(true);
define left = Signal("L");
define right = Signal("R");
define result = Computed(function()
    if(flag()) left() else right()
);
result();   // => "L" (tracks flag and left)

right->set("R2");
result();   // => "L" (right not tracked, no recompute)

flag->set(false);
result();   // => "R2" (now tracks flag and right)
```

### untracked

Suppress dependency tracking inside a function. Any signal reads within `untracked` are invisible to the enclosing Computed or Effect:

```
define a = Signal(1);
define b = Signal(2);
define c = Computed(function() a() + untracked(function() b()));
c();            // => 3

a->set(10);
c();            // => 12 (recomputed — a is tracked)

b->set(100);
c();            // => 12 (NOT recomputed — b is untracked)
```

### derived

Shorthand for creating a Computed from a single signal with a transform function:

```
define count = Signal(0);
define doubled = derived(count, function(v) v * 2);
doubled();          // => 0

count->set(5);
doubled();          // => 10
```

Equivalent to `Computed(function() fn(source()))`.

### readonly

Wraps a Signal or Computed to expose only read access. Blocks `set`, `update`, `dispose`, and `close`:

```
define _count = Signal(0);
define count = readonly(_count);

count();            // => 0 (reading works)
_count->set(5);
count();            // => 5 (tracks underlying signal)
count->peek;        // => 5 (peek works)

count->set(10);     // ERROR: readonly: cannot set
```

Readonly views can be used as dependencies in Computed and Effect — they pass through observer registration to the underlying source:

```
define inner = Signal(42);
define ro = readonly(inner);
define c = Computed(function() ro() + 1);
c();                // => 43

inner->set(10);
c();                // => 11 (tracked through readonly)
```

Type predicate: `readonly?(ro)` returns `true`.

### watch

Observe a signal and call a function with `(new_value, old_value)` on each change. Skips the initial value — only fires when the value actually changes:

```
define name = Signal("Alice");
define w = watch(name, function(new_val, old_val) {
    print("changed from " ++ old_val ++ " to " ++ new_val);
});
// (nothing printed — initial run is skipped)

name->set("Bob");
// prints: changed from Alice to Bob

name->set("Carol");
// prints: changed from Bob to Carol

dispose(w);
// stops watching
```

Returns an Effect that can be disposed to stop watching.

### on

Create an effect with an explicit dependency list. Only the listed signals are tracked; all reads inside the callback are untracked:

```
define a = Signal(1);
define b = Signal(2);
define c = Signal(100);

on([a, b], function(av, bv) {
    print(av + bv + c->peek);
});
// prints: 103

a->set(10);
// prints: 112

c->set(999);
// (nothing — c is not in the dependency list)
```

### reduce

Fold over signal changes, accumulating a value. Returns a Signal holding the accumulated result:

```
define clicks = Signal(0);
define total = reduce(clicks, function(acc, v) acc + v, 0);
total();            // => 0

clicks->set(5);
total();            // => 5  (0 + 5)

clicks->set(3);
total();            // => 8  (5 + 3)

clicks->set(10);
total();            // => 18 (8 + 10)
```

The accumulator starts at the initial value and only updates when the source changes (uses `watch` internally, so the initial value is skipped).

### scope

Group reactive nodes for bulk disposal. All Signals, Computeds, and Effects created inside the scope function are tracked and disposed together:

```
define handle = scope(function() {
    define x = Signal(1);
    define doubled = Computed(function() x() * 2);
    Effect(function() print(doubled()));
});
// prints: 2

dispose(handle);
// effect stopped, all nodes cleaned up
```

Works with `with` for automatic cleanup:

```
define s = Signal(0);
with(h = scope(function() {
    Effect(function() print(s()));
})) {
    s->set(1);
};
// After with block, scope is disposed — effect stops
```

Scopes nest — disposing an inner scope doesn't affect the outer:

```
define outer = scope(function() {
    Effect(function() print("outer"));
    define inner = scope(function() {
        Effect(function() print("inner"));
    });
    dispose(inner);  // only inner effect stops
});
```

Type predicate: `scope?(handle)` returns `true`.

### Custom equality

Signal and Computed accept an optional equality function as a second argument. The equality function determines when updates are skipped:

```
// Identity equality — notify even for same-content objects
define s = Signal([1, 2, 3], =?);
s->set([1, 2, 3]);   // notifies! (different object)

// Default (structural equality) would skip:
define s2 = Signal([1, 2, 3]);
s2->set([1, 2, 3]);  // skipped — same content

// Always-notify (no dedup)
define s3 = Signal(0, function(a, b) false);
s3->set(0);           // notifies even though value unchanged
```

For Computed, custom equality prevents downstream propagation when the derived value hasn't meaningfully changed:

```
define precise = Signal(3.14);
define rounded = Computed(
    function() `floor`(precise()),
    function(a, b) a == b
);

precise->set(3.99);
// rounded recomputes to 3, same as before — downstream NOT notified

precise->set(4.5);
// rounded is now 4, different — downstream notified
```

### WritableComputed

A two-way computed: reads derive from sources, writes dispatch through a setter:

```
define celsius = Signal(0);
define fahrenheit = WritableComputed(
    function() celsius() * 9 / 5 + 32,
    function(f) celsius->set((f - 32) * 5 / 9)
);

fahrenheit();           // => 32
fahrenheit->set(212);
celsius();              // => 100
fahrenheit();           // => 212
```

Type predicate: `writable_computed?(fahrenheit)` returns `true`.

### combine

Merge multiple signals into a single computed value:

```
define x = Signal(1);
define y = Signal(2);
define z = Signal(3);
define sum = combine([x, y, z], function(a, b, c) a + b + c);
sum();   // => 6

x->set(10);
sum();   // => 15
```

### select

Extract a slice from a signal, only propagating when the selected portion changes:

```
define state = Signal([10, 20, 30]);
define first = select(state, function(lst) car(lst));
first();   // => 10

state->set([10, 99, 99]);
// first is still 10 — no propagation

state->set([42, 99, 99]);
// first is now 42 — propagates
```

Accepts an optional custom equality as a third argument.

### prev

Signal that holds the previous value of a source (one step behind):

```
define count = Signal(0);
define prevCount = prev(count);

prevCount();     // => false (no previous yet)
count->set(1);
prevCount();     // => 0
count->set(5);
prevCount();     // => 1
```

Accepts an optional initial value: `prev(source, "none")`. Returns a readonly signal.

### trace

Attach debug logging to a signal — logs every change to stdout:

```
define count = Signal(0);
trace(count, "count");

count->set(5);    // prints: [count] 0 -> 5
count->set(10);   // prints: [count] 5 -> 10
```

Returns the source signal unchanged, so it can be inserted inline without affecting behavior.

### resource

Async data loading primitive that fetches data in a green thread:

```
// Without source — fetches once
define r = resource(function() fetch_data());
r->settle();       // wait for fetch to complete
r();               // => fetched value
r->loading;        // => false
r->error;          // => #f (or error object)

// With source — re-fetches when source changes
define id = Signal(1);
define user = resource(id, function(v) load_user(v));
user->settle();
user();            // => user for id 1

id->set(2);        // triggers re-fetch
user->settle();
user();            // => user for id 2
```

Messages: `loading` (bool signal), `error` (signal), `settle` (wait for fetch), `refetch` (force re-fetch), `mutate(v)` (set value directly), `peek`, `dispose`.

The optional third argument sets an initial value before the first fetch completes:

```
define r = resource(src, fetcher, default_value);
```

Stale fetches are automatically ignored — if the source changes while a fetch is in progress, only the latest result is used.

## Continuation Serialization

Continuations can be serialized to bytes and restored later:

```
define k = callcc(function(k) k);
define blob = serialize_continuation(k);
define k2 = deserialize_continuation(blob);
```

## Statements and Semicolons

Every top-level form is a statement and must end with `;`:

```
define x = 10;
display(x);
x + 1;
```

Inside blocks, the last expression's semicolon is optional:

```
{ define x = 10; x + 1 }    // ok, returns 11
{ define x = 10; x + 1; }   // also ok
```

## Networking

Eval provides OO wrappers for TCP sockets and HTTP with RAII cleanup and reactive integration. All socket operations are non-blocking with green threads.

### TcpClient / TcpSocket

`TcpClient` connects to a host:port and returns a `TcpSocket` with string-based send/recv:

```
with(sock = TcpClient("example.com", 80)) {
    sock->send("GET / HTTP/1.0\r\nHost: example.com\r\n\r\n");
    define response = sock->recv(4096);
    display(response);
};
// sock->close() called automatically
```

`TcpSocket` wraps a raw file descriptor:

```
define sock = TcpSocket(fd);
sock->send("hello");       // string or bytevector
sock->recv(4096);          // returns string or false (EOF)
sock->recv_bytes(4096);    // returns raw bytevector
sock->fd;                  // underlying file descriptor
sock->open?;               // true if not closed
sock->close();             // close (idempotent)
```

### TcpServer

`TcpServer` creates a listener with green-thread-per-connection dispatch:

```
define server = TcpServer(8080, function(sock, addr, port) {
    define data = sock->recv(4096);
    sock->send("HTTP/1.0 200 OK\r\nContent-Length: 2\r\n\r\nOK");
    // sock auto-closed when handler returns
});

server->run();      // blocking accept loop
server->start();    // or non-blocking (green thread)
server->stop();     // stop accepting
```

Reactive signals track connection state:

```
server->connections;   // Signal — current active connections
server->requests;      // Signal — total requests served

Effect(function() {
    display("active=" ++ `number->string`(server->connections()));
    newline();
});
```

### HttpClient

OO HTTP client with `get` and `post`, returning `[status, body]`:

```
with(client = HttpClient("example.com", 80)) {
    define result = client->get("/");
    define status = car(result);
    define body = car(cdr(result));
    display(body);
};
```

See [NETWORKING.md](NETWORKING.md) for the full networking guide including low-level socket functions, DNS resolution, socket options, and constants.

## Further reading

- [GENERATORS.md](GENERATORS.md) — Generators, yield, generator comprehensions, lazy pipelines
- [INDEXING.md](INDEXING.md) — Bracket indexing vs arrow properties: `xs[0]` vs `xs->first`, when to use which
- [STRINGS.md](STRINGS.md) — String methods: upper, lower, trim, split, replace, contains, and more
- [LISTS.md](LISTS.md) — List methods: map, filter, sort, fold, join, take, drop, and more
- [VECTORS.md](VECTORS.md) — Vector methods: map, filter, sort, fold, set, to_list, and more
- [DICTS.md](DICTS.md) — Dict methods: get, set, map, filter, fold, merge, and more
- [NETWORKING.md](NETWORKING.md) — TCP sockets, HTTP client/server, OO wrappers, non-blocking I/O
- [FILESYS.md](FILESYS.md) — File I/O, directory operations, metadata, path utilities
- [ASYNC.md](ASYNC.md) — Async/await, thread pools, channels, pipelines
- [THREADS.md](THREADS.md) — Green threads, mutexes, condition variables, continuations
- [MULTITHREADING.md](MULTITHREADING.md) — OO synchronization: Mutex, Monitor, ReadWriteLock, Semaphore
- [REACTIVE.md](REACTIVE.md) — Signals, computed values, effects, scopes, resources
- [TESTS.md](TESTS.md) — Built-in test framework: test, test_assert, test_error, test_group
