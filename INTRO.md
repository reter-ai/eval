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

### Operators as values

Operators can be used as first-class values when they appear in value position (after `(`, `,`, etc.):

```
fold(+, 0, [1, 2, 3])           // => 6
map(-, [1, 2, 3])               // => [-1, -2, -3]
sort([3, 1, 2], <)              // => [1, 2, 3]
filter(function(x) x > 2, [1, 2, 3, 4])  // => [3, 4]
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
+  -                   (left)
*  /  %                (left)
**                     (right)
-  !  ~  '             (right, unary)
()  ->  ++  --         (left, postfix)
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

The `->` operator sends a message: `obj->x` compiles to `(obj 'x)`.

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
| **Methods** | Interface entries can be functions | None (data only) |
| **Type check** | Not built-in | `Type?(obj)` predicate |
| **Use case** | Behavior, encapsulation, polymorphism | Simple data aggregates, tagged unions |

Use `record` for plain data carriers (like a struct). Use `constructor`/`interface` when you need behavior, encapsulation, or inheritance.

## Dictionaries

`dict` creates a mutable hash-table-backed dictionary with `->` access:

```
define d = dict(name: "Alice", age: 30, city: "NYC");
d->name;         // => "Alice"
d->age;          // => 30
```

The `dict(key: value, ...)` syntax uses the same `key: value` entries as `interface`. Keys are symbols internally.

### Methods

```
d->get("name");           // dynamic get by string key => "Alice"
d->get("missing");        // => #f (not found)
d->set("email", "a@b");   // add or overwrite a key
d->delete("city");        // remove a key
d->has?("name");          // => #t
d->has?("city");          // => #f (after delete)
d->keys();                // => list of keys
d->values();              // => list of values
d->size();                // => number of entries
d->to_list();             // => association list ((key . value) ...)
```

### Predicate

```
dict?(d);        // => #t
dict?(42);       // => #f
```

### Empty dict

```
define empty = dict();
empty->size();   // => 0
```

### Field access vs method access

Direct field access (`d->name`) returns the value if the key exists, or `#f` if not. The `get`, `set`, `delete`, `keys`, `values`, `has?`, `size`, and `to_list` names are reserved as methods and cannot be used as field names.

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

## Thread Pool

For true OS-level parallelism using worker threads:

```
define pool = make_pool(4);    // 4 worker threads

// Submit a closure for async execution
define f = pool_submit(function() {
    fold(+, 0, map(function(x) x * x, [1, 2, 3, 4, 5]));
});

future_result(f);   // => 55 (blocks until done)
future_ready?(f);   // => true (non-blocking check)

pool_shutdown(pool);
```

### Channels

Cross-thread communication:

```
define ch = make_channel(10);   // buffered channel, capacity 10
channel_send(ch, "hello");
channel_recv(ch);               // => "hello"
channel_try_recv(ch);           // non-blocking, returns false if empty
channel_close(ch);
```

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
