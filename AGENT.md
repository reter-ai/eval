# Eval Language — Agent Reference

Eval is an infix-syntax language compiled to chibi-scheme. It combines familiar C-like syntax with Scheme's power: closures, continuations, macros, logic programming, reactive signals, and more.

## Running Code

```bash
eval -e "2 ** 10"        # evaluate expression, print result
eval program.eval         # run file
echo 'print(42);' | eval # read from stdin
eval                      # REPL (when stdin is a TTY)
eval -I ./libs file.eval  # add module search path
eval -V                   # version
```

File extension: `.eval`

## Python API

```python
from chibi_eval import Eval
e = Eval()
e.eval("2 ** 10;")          # => 1024
e["x"] = 42                 # set variable
e["x"]                      # => 42
e.call("func_name", arg)    # call Eval function
e.load("file.eval")         # load file
```

---

## Syntax Quick Reference

### Comments
```
// line comment
/* block comment */
```

### Literals

| Type | Example | Notes |
|------|---------|-------|
| Integer | `42`, `0xFF` | |
| Float | `3.14`, `1e-2` | |
| String | `"hello\n"` | escape sequences: `\n \t \r \\ \" \0` |
| Raw string | `"""multi\nline"""` | no escape processing, literal newlines OK |
| F-string | `f"hi {name}"` | interpolation, `{{`/`}}` for literal braces |
| Boolean | `true`, `false` | `false` is the ONLY falsy value |
| Nil | `nil` | empty list `()` — **truthy!** |
| Symbol | `'hello` | |
| List | `[1, 2, 3]` | linked list |
| Vector | `#[1, 2, 3]` | indexed array |
| Pair | `(1 .. 2)` | cons cell |

### Operators (by precedence, low to high)

| Precedence | Operators | Assoc |
|------------|-----------|-------|
| Assignment | `= += -= *= /=` | right |
| Logical OR | `\|\|` | left |
| Logical AND | `&&` | left |
| Bitwise OR | `\|` | left |
| Bitwise AND | `&` | left |
| Equality | `== != =?` | left |
| Comparison | `< > <= >=` | left |
| Shift | `<< >>` | left |
| Additive | `+ - ++`(concat) | left |
| Multiplicative | `* / %` | left |
| Exponent | `**` | right |
| Unary | `- ! ~ '` | right |
| Postfix | `() [] -> ++ --` | left |

- `++` between strings is concatenation: `"a" ++ "b"` => `"ab"`
- `=?` is identity (Scheme `eq?`), `==` is structural equality
- Operators are **first-class** in value position: `fold(+, 0, [1,2,3])` => `6`

### Variables & Assignment

```
define x = 42;       // create new binding
x = 100;             // mutate existing (set!)
x += 5;              // compound assignment
x++;                 // increment
x--;                 // decrement
```

`define` creates; `=` mutates. There is no standalone `let x = ...` statement.

### Functions

```
define double = function(x) x * 2;

define abs = function(x) {
    if(x < 0) return -x;
    x;
};

define greet = function() "hello";

// Variadic
define all = function(..args) args;

// Immediate call
(function(x) x * x)(5);
```

`return` uses call/cc — only allocated if `return` appears in body.

### Control Flow

```
// if/else (expression)
if(x > 0) "pos" else "non-pos"

// when/unless (no else)
when(debug) print("trace");
unless(valid) error("bad");

// cond (multi-way)
cond(
    x < 0:  "negative",
    x == 0: "zero",
    else:   "positive"
)

// case (value match)
case(color,
    ("red"):   "stop",
    ("green"): "go",
    else:      "unknown"
)
```

### Loops

```
// while
while(cond) { body; };

// C-style for
for(let i = 0, i < 5, i++) { body; };

// for-each
for(let x in collection) body;

// do-until
do expr until(cond);

// break exits innermost loop
while(true) { if(done) break; };
```

### Blocks

```
define result = {
    define a = 10;
    define b = 20;
    a + b
};  // => 30
```

Blocks are expressions. Internal `define`s compile to `letrec` (mutual recursion works). Trailing semicolon optional: `{ a; b; c }` and `{ a; b; c; }` are equivalent.

### Let Bindings

```
let(x = 1, y = 2) x + y           // parallel binding
let*(x = 10, y = x + 5) y         // sequential
letrec(f = function(n) ...) f(10)  // recursive
```

### Indexing & Slicing

```
[10, 20, 30][0]       // => 10
[10, 20, 30][-1]      // => 30 (from end)
"hello"[1]            // => #\e
[1,2,3,4,5][1:3]      // => [2, 3] (exclusive end)
[1,2,3][:2]           // => [1, 2]
[1,2,3][1:]           // => [2, 3]
"hello"[1:3]          // => "el"
#[10,20,30][1:3]      // => #[20, 30]
```

### Error Handling

```
// try/catch
try { risky(); } catch(e) { handle(e); }

// try/catch/finally
try { work(); } catch(e) { recover(e); } finally { cleanup(); }

// try/finally (error propagates)
try { work(); } finally { cleanup(); }

// Multi-clause catch
try expr catch(e,
    string?(e): handle_str(e),
    pair?(e):   handle_pair(e)
)

// with (RAII — auto-calls ->close())
with(f = open_file("data.txt")) { process(f); }
```

### Multiple Return Values

```
define swap = function(a, b) values(b, a);
receive(x, y) from swap(1, 2) [x, y];  // => [2, 1]
```

### Comprehensions

```
[x * 2 for x in [1,2,3]];                  // list: [2, 4, 6]
[x for x in range(10) if x > 5];           // filtered
[x+y for x in [1,2] for y in [10,20]];     // nested
#[x * 2 for x in [1,2,3]];                 // vector
dict(x: x*2 for x in [1,2,3]);             // dict
(x * 2 for x in [1,2,3]);                  // lazy generator
```

### Generators

```
define count = generator(n) {
    for(let i = 0, i < n, i++) yield i;
};
define g = count(3);
g();  // 0, 1, 2, then eof
collect(count(5));  // => [0, 1, 2, 3, 4]

// Infinite
define naturals = generator() {
    define i = 0;
    while(true) { yield i; i++; };
};
```

---

## Arrow Methods

Method dispatch via `->` is type-aware. Lists, strings, vectors, dicts, DateTime, Date, TimeDelta, Decimal, and Qty (Quantity) each have their own method set.

### List Methods

| Method | Example | Result |
|--------|---------|--------|
| `->first` | `[1,2,3]->first` | `1` |
| `->last` | `[1,2,3]->last` | `3` |
| `->rest` | `[1,2,3]->rest` | `[2,3]` |
| `->length` | `[1,2,3]->length` | `3` |
| `->empty?` | `[]->empty?` | `true` |
| `->map(f)` | `[1,2,3]->map(function(x) x*2)` | `[2,4,6]` |
| `->filter(f)` | `[1,2,3,4]->filter(function(x) x>2)` | `[3,4]` |
| `->reject(f)` | `[1,2,3,4]->reject(function(x) x>2)` | `[1,2]` |
| `->fold(f, init)` | `[1,2,3]->fold(+, 0)` | `6` |
| `->reduce(f)` | `[1,2,3,4,5]->reduce(+)` | `15` |
| `->find(f)` | `[1,2,3]->find(function(x) x>1)` | `2` |
| `->any(f)` | `[1,2,3]->any(function(x) x>2)` | `true` |
| `->every(f)` | `[2,4,6]->every(function(x) x%2==0)` | `true` |
| `->contains(v)` | `[1,2,3]->contains(2)` | `true` |
| `->count(f)` | `[1,2,3,4]->count(function(x) x>2)` | `2` |
| `->sort(cmp)` | `[3,1,2]->sort(<)` | `[1,2,3]` |
| `->reverse()` | `[1,2,3]->reverse()` | `[3,2,1]` |
| `->flatten()` | `[[1,2],[3]]->flatten()` | `[1,2,3]` |
| `->unique()` | `[1,2,2,3]->unique()` | `[1,2,3]` |
| `->take(n)` | `[1,2,3,4]->take(2)` | `[1,2]` |
| `->drop(n)` | `[1,2,3,4]->drop(2)` | `[3,4]` |
| `->take_while(f)` | `[1,2,5,3]->take_while(function(x) x<4)` | `[1,2]` |
| `->drop_while(f)` | `[1,2,5,3]->drop_while(function(x) x<4)` | `[5,3]` |
| `->append(lst)` | `[1,2]->append([3,4])` | `[1,2,3,4]` |
| `->zip(lst)` | `[1,2]->zip(["a","b"])` | `[[1,"a"],[2,"b"]]` |
| `->flat_map(f)` | `[1,2]->flat_map(function(x) [x,x])` | `[1,1,2,2]` |
| `->partition(f)` | `[1,2,3,4]->partition(function(x) x%2==0)` | `[[2,4],[1,3]]` |
| `->join(sep)` | `["a","b"]->join(",")` | `"a,b"` |
| `->to_vector()` | `[1,2,3]->to_vector()` | `#[1,2,3]` |
| `->for_each(f)` | side-effect iteration | |
| `->filter_map(f)` | map+filter (false = skip) | |
| `->index_of(f)` | `[10,20,30]->index_of(function(x) x==20)` | `1` |
| `->split_at(n)` | `[1,2,3,4]->split_at(2)` | `[[1,2],[3,4]]` |
| `->span(f)` | split at first failure | |
| `->take_right(n)` | from end | |
| `->drop_right(n)` | from end | |

### String Methods

| Method | Example | Result |
|--------|---------|--------|
| `->length` | `"hello"->length` | `5` |
| `->empty?` | `""->empty?` | `true` |
| `->upper()` | `"hello"->upper()` | `"HELLO"` |
| `->lower()` | `"HELLO"->lower()` | `"hello"` |
| `->trim()` | `"  hi  "->trim()` | `"hi"` |
| `->trim_start()` | leading whitespace | |
| `->trim_end()` | trailing whitespace | |
| `->contains(s)` | `"hello"->contains("ell")` | `true` |
| `->starts_with(s)` | `"hello"->starts_with("hel")` | `true` |
| `->ends_with(s)` | `"hello"->ends_with("llo")` | `true` |
| `->index_of(s)` | `"hello"->index_of("ll")` | `2` |
| `->replace(old,new)` | `"hi world"->replace("world","there")` | `"hi there"` |
| `->split(sep)` | `"a,b,c"->split(",")` | `["a","b","c"]` |
| `->join(lst)` | `","->join(["a","b"])` | `"a,b"` |
| `->slice(s,e)` | `"hello"->slice(0,3)` | `"hel"` |
| `->reverse()` | `"abc"->reverse()` | `"cba"` |
| `->repeat(n)` | `"ab"->repeat(3)` | `"ababab"` |
| `->chars()` | `"hi"->chars()` | `(#\h #\i)` |
| `->to_number()` | `"42"->to_number()` | `42` |
| `->to_symbol()` | `"x"->to_symbol()` | `x` (symbol) |
| `->map(f)` | per-character transform | |
| `->fold(f,init)` | per-character fold | |
| `->any(f)` | char predicate | |
| `->find(f)` | first matching char | |

### Vector Methods

| Method | Example | Result |
|--------|---------|--------|
| `->length` | `#[1,2,3]->length` | `3` |
| `->empty?` | `#[]->empty?` | `true` |
| `->first` | `#[10,20]->first` | `10` |
| `->last` | `#[10,20]->last` | `20` |
| `->rest` | `#[1,2,3]->rest` | `#[2,3]` |
| `->map(f)` | `#[1,2]->map(function(x) x*2)` | `#[2,4]` |
| `->filter(f)` | `#[1,2,3]->filter(function(x) x>1)` | `#[2,3]` |
| `->sort(cmp)` | `#[3,1,2]->sort(<)` | `#[1,2,3]` |
| `->reverse()` | `#[1,2,3]->reverse()` | `#[3,2,1]` |
| `->contains(v)` | `#[1,2,3]->contains(2)` | `true` |
| `->to_list()` | `#[1,2]->to_list()` | `[1,2]` |
| `->set(i, v)` | in-place mutation | |

### Dict Methods

```
define d = dict(name: "Alice", age: 30);
```

| Method | Example | Result |
|--------|---------|--------|
| `->length` / `->size` | `d->length` | `2` |
| `->empty?` | `d->empty?` | `false` |
| `->keys` | `d->keys` | `(name age)` |
| `->values` | `d->values` | `("Alice" 30)` |
| `->entries` | key-value alist | |
| `->get(k)` | `d->get("name")` | `"Alice"` |
| `->get(k, default)` | `d->get("x", 0)` | `0` |
| `->set(k, v)` | mutating add/update | |
| `->delete(k)` | remove key | |
| `->has?(k)` | `d->has?("name")` | `true` |
| `->map(f)` | `f(k,v)` transforms values | |
| `->filter(f)` | `f(k,v)` predicate | |
| `->fold(f, init)` | `f(k, v, acc)` | |
| `->for_each(f)` | `f(k,v)` side-effect | |
| `->merge(other)` | new dict | |
| `->merge!(other)` | mutating merge | |
| `->update(k, f)` | `f(old_val)` | |
| `->copy()` | shallow copy | |
| `->clear()` | remove all | |
| `->any(f)` | `f(k,v)` | |
| `->every(f)` | `f(k,v)` | |
| `->find(f)` | `f(k,v)` | |
| `->to_list()` | alist | |
| `d["key"]` | bracket access | |

---

## Paradigm Recipes

### Functional

```
// Map/filter/fold chains
[1,2,3,4,5]
    ->filter(function(x) x % 2 != 0)
    ->map(function(x) x * x)
    ->fold(+, 0);  // => 35

// Operators as values
sort([3,1,4,1,5], >);  // => [5,4,3,1,1]
fold(*, 1, [1,2,3,4]); // => 24

// Generators
define fibs = generator() {
    define a = 0; define b = 1;
    while(true) {
        yield a;
        define t = a + b;
        a = b; b = t;
    };
};
collect(fibs())->take(10);
```

### OOP (Constructor + Interface + Super)

```
define Animal = constructor(name)
    interface(
        name: function() name,
        speak: function() "..."
    );

define Dog = constructor(name) {
    super Animal(name);
    interface(speak: function() "woof!");
};

define d = Dog("Rex");
d->name();   // => "Rex"
d->speak();  // => "woof!"

// Abstract
define Shape = abstract constructor()
    interface(area: abstract);

define Circle = constructor(r) {
    super Shape();
    interface(area: function() 3.14159 * r * r);
};
Circle(5)->area();  // => 78.54
```

### Records

```
record Point(x, y);
define p = Point(3, 4);
p->x;        // => 3
Point?(p);   // => true
```

### Reactive (Signals)

```
define count = Signal(0);
define doubled = Computed(function() count() * 2);
Effect(function() print(f"count={count()}, doubled={doubled()}"));
// prints: count=0, doubled=0

count->set(5);
// prints: count=5, doubled=10

batch(function() {
    count->set(10);
    // effects deferred until batch ends
});

// Other signal utilities
define r = readonly(count);       // read-only view
define d = derived(count, function(v) v * 3);
define w = watch(count, function(new_val, old_val) { ... });
dispose(w);
```

### DateTime, Decimal & Qty

```
// DateTime: timestamps with timezone
DateTime->now()->format("%Y-%m-%d %H:%M");  // "2026-02-20 10:30"
DateTime->parse("2026-02-20T10:30:00Z")->year;  // 2026
DateTime(2026, 2, 20, 10, 0, 0, 0)->add(TimeDelta(3600))->hour;  // 11
dt1->sub(dt2)->days;  // difference in days

// Date: calendar dates
Date->today()->add_days(7)->format("%B %d");  // "February 27"

// TimeDelta: durations
TimeDelta(3600)->hours;           // 1
TimeDelta(1, 2, 30, 0)->total_seconds;  // 95400

// Decimal: exact arithmetic
Decimal("0.1")->add(Decimal("0.2"))->to_string();  // "0.3"
Decimal("10")->div(Decimal("3"), 4)->to_string();   // "3.3333"

// Qty (Quantity): dimensional analysis + unit conversion
Qty(100, "km") / Qty(2, "hr");          // Qty(50, "km/hr")
Qty(5, "m") + Qty(3, "m");              // Qty(8, "m")
Qty(5, "m") + Qty(3, "s");              // ERROR: incompatible dimensions
Qty(1, "mile")->to("km");               // Qty(1.609344, "km")
Qty(5, "m") ** 2;                        // Qty(25, "m^2")
Qty(10, "m") * 5;                        // Qty(50, "m")

// Currency (auto-Decimal, Money is alias for Qty)
Qty("19.99", "USD") + Qty("1.50", "USD");  // Qty(21.49, "USD")
Qty("19.99", "USD") * 3;                    // Qty(59.97, "USD")
Qty(5, "USD") + Qty(5, "EUR");              // ERROR: incompatible dimensions
Money("19.99", "USD") + Money("5", "USD");  // backward-compatible alias
```

### Logic Programming (miniKanren)

```
// Unification
run(1, ?q) { ?q === 5 };   // => [5]

// Relations
fact parent("tom", "bob");
fact parent("bob", "ann");

rule ancestor(?x, ?y) :- parent(?x, ?y);
rule ancestor(?x, ?y) :- fresh(?z) { parent(?x, ?z), ancestor(?z, ?y) };

run(*, ?q) { ancestor(?q, "ann") };  // => ["bob", "tom"]

// Disjunction
run(*, ?q) conde {
    { ?q === 1 },
    { ?q === 2 }
};  // => [1, 2]
```

### Forward-Chaining (Rete)

```
whenever parent(?x, ?y), parent(?y, ?z) {
    display(f"{?x} is grandparent of {?z}");
    newline();
};

fact parent("tom", "bob");
fact parent("bob", "ann");
// fires: tom is grandparent of ann
```

### Nondeterministic Choice (amb)

```
define x = amb(1, 2, 3, 4, 5);
require(x > 3);
x;  // => 4

// Collect all solutions
amb_collect(function() {
    define a = amb(1, 2, 3);
    define b = amb(1, 2, 3);
    require(a + b == 4);
    [a, b];
});  // => [[1,3], [2,2], [3,1]]
```

### Concurrency

```
// Green threads (async/await)
define a = async fib(30);
define b = async fib(32);
[await(a), await(b)];

// Thread pool (OS threads, isolated VMs)
with(pool = Pool(4)) {
    define f1 = pool->submit("10 * 10;");
    define f2 = pool->submit("20 + 5;");
    await(f1) + await(f2);
};

// Channels (cross-worker communication)
with(ch = pool->channel("data"));
ch->send("hello");
ch->recv();

// SRFI-18 threads (shared state)
define m = make_mutex();
define t = make_thread(function() {
    mutex_lock(m); counter += 1; mutex_unlock(m);
});
thread_start(t);
thread_join(t);

// OO synchronization
define m = Mutex();
with(guard = m->lock()) { counter += 1; };

define rw = ReadWriteLock();
with(guard = rw->read_lock()) { data; };
```

### Concurrent Containers

```
define d = ConcurrentDict();     // or ConcurrentDict("named") for cross-worker
d->set("key", 42);
d->get("key");

define q = ConcurrentQueue(100); // bounded
q->push(item);
q->pop();                        // blocking

define s = ConcurrentStack();
s->push(1); s->pop();
```

### Differentiable Programming (AD)

```
// Reverse-mode AD: param creates differentiable variables
param W = [[0.1, 0.2], [0.3, 0.4]];
param x = [[1.0], [2.0]];
define loss = sum(W @ x);
backward(loss);
W->grad;           // accumulated gradients

// Forward-mode: composable derivatives via dual numbers
define df = grad(function(x) x * x);
df(3.0);           // => 6.0

// Key ops (all differentiable with backward support)
softmax(logits);              // axis-aware, numerically stable
softmax(logits, 0);           // softmax along axis 0
gelu(x);                      // GELU activation
silu(x);                      // SiLU/Swish activation
sum(x, 1);                    // sum along axis 1
mean(x, 0);                   // mean along axis 0
reshape(x, [2, 3]);           // reshape tensor
tensor_slice(x, [0,1], [2,2]); // extract subtensor
concat([a, b, c], 0);         // concatenate along axis
gather(embeddings, [1,3,2]);   // embedding lookup
layer_norm(x, gamma, beta);   // layer normalization
where(mask, a, b);            // conditional select
batch_matmul(a, b);           // 3D+ batched matmul

// Tape control
tape_reset();                 // clear tape
zero_grad();                  // zero gradients
no_grad(function() ...);      // untracked computation

// Optimizer
define opt = SGD([W, x], 0.01);
opt->step();
opt->zero_grad();
```

### Grammar JIT

```
define calc = Grammar("""
    start: expr
    ?expr: expr "+" term -> add | term
    ?term: NUMBER
    NUMBER: /[0-9]+/
    %ignore /\\s+/
""");
define p = calc->compile();
p->parse("1 + 2 + 3");
// => ("add" ("add" ("NUMBER" "1") ("NUMBER" "2")) ("NUMBER" "3"))
```

Uses Lark EBNF syntax: lowercase rules, UPPERCASE terminals, `?` for inline, `->` for aliases, `%ignore` for whitespace.

### Binary Serialization

```
define s = Schema("""
    @0xdbb9ad1f14bf0b36;
    struct Person {
        name   @0 :Text;
        age    @1 :UInt32;
        active @2 :Bool;
    }
""");

define msg = s->Person->build("name", "Alice", "age", 30, "active", true);
define r = s->Person->read(msg);
r->name;    // => "Alice"
r->age;     // => 30

s->Person->save(msg, "person.bin");
with(r = s->Person->mmap("person.bin")) { r->name; };
```

### Testing

```
test_group("Arithmetic") {
    test("add", 4, 2 + 2);
    test("mul", 42, 6 * 7);
    test_assert("positive", 5 > 0);
    test_error("div0", function() 1 / 0);
};

// Or manual
test_begin("Suite");
test("case1", expected, actual);
test_end();  // returns failure count
```

---

## I/O & System

### File I/O

```
// Read entire file
define content = file_to_string("data.txt");
define bytes = file_to_bytevector("image.png");

// Write (auto-close)
call_with_output_file("out.txt", function(port) {
    write_string("hello\n", port);
});

// Read line-by-line
call_with_input_file("data.txt", function(port) {
    define line = read_line(port);
    // ...
});

// Append
define port = open_output_file_append("log.txt");
write_string("entry\n", port);
close_output_port(port);
```

### Directories

```
directory_files("src");                  // list entries
create_directory_star("path/to/nested"); // mkdir -p
delete_file("temp.txt");
delete_file_hierarchy("build");          // rm -rf
current_directory();
change_directory("/tmp");
file_exists("data.txt");
file_directory("src");
file_size("data.txt");
```

### Environment

```
get_env("PATH");            // => string or false
current_clock_second();     // epoch seconds (float)
random_integer(100);        // [0, 100)
random_real();              // [0.0, 1.0)
```

### Console I/O

```
display(x);    // print value (strings without quotes)
print(x);      // display + newline
newline();     // just newline
```

---

## Special Syntax

### Include & Modules

```
include("utils.eval");

library(my, math) {
    export(square, cube);
    define square = function(x) x * x;
    define cube = function(x) x * x * x;
};
import(my, math);
```

### Macros

```
macro my_and syntax_rules() {
    (_, x):    x,
    (_, x, y): if(x) y else false
};
```

### Scheme Interop

Most Scheme functions have OO arrow-method equivalents or underscore aliases. Prefer arrow methods; use backticks only for rare functions:

```
"hello" ++ " " ++ "world"                // string-append → ++ operator
"hello"->length                           // string-length → ->length
[1, 2, 3]->for_each(print)               // for-each → ->for_each
char_to_integer('A')                      // alias (no OO equivalent)
`some-rare-function`(args)                // backtick for uncommon names
```

### Compile-Time Evaluation

```
define x = !!(2 + 3);  // x is 5, computed at parse time
```

### Continuations

```
callcc(function(k) k(42));  // => 42

// Serialization (cross-process)
define blob = serialize_continuation(k);
define k2 = deserialize_continuation(blob);
```

---

## Built-in Functions

### Predicates
`number?` `string?` `list?` `pair?` `vector?` `procedure?` `boolean?` `null?` `symbol?` `eof?` `promise?`

### List Primitives
`car` `cdr` `cons` `list` `length` `append` `reverse` `sort`

### Higher-Order
```
map(f, lst)               // [f(x) for x in lst]
filter(f, lst)            // [x for x in lst if f(x)]
fold(f, init, lst)        // fold(+, 0, [1,2,3]) => 6  (element, acc)
apply(f, args_list)       // apply(+, [3, 4]) => 7
```

### Math
`abs` `min` `max` `floor` `ceiling` `round` `truncate` `sqrt` `expt` `modulo` `remainder` `gcd` `lcm`

### Conversion
```
"42"->to_number()              // string → number
"x"->to_symbol()               // string → symbol
"hi"->chars()                  // string → list of chars
[1,2,3]->to_vector()           // list → vector
#[1,2,3]->to_list()            // vector → list
number_to_string(42)           // number → string (alias)
symbol_to_string('abc)         // symbol → string (alias)
char_to_integer('A')           // char → integer (alias)
integer_to_char(65)            // integer → char (alias)
list_to_string(['A','B','C'])  // char list → string (alias)
```

### JSON
```
json_write(data, output_port);
json_read(input_port);
```

### Hash
```
hash(value);
string_hash("hello");
```

---

## Gotchas

1. **`nil` is truthy** — only `false` is falsy. Use `null?(x)` to test for empty list.
2. **`define` creates, `=` mutates** — `define x = 1;` then `x = 2;`. No standalone `let x = ...`.
3. **`fold` argument order** is `(element, accumulator)`, not `(acc, elem)`.
4. **`++` is string concatenation**, not increment. Increment is `x++` (postfix).
5. **`->` is method dispatch**, not a pointer or arrow function.
6. **Operators are first-class** in value position: `map(-, [1,2,3])` => `[-1,-2,-3]`.
7. **Triple-quoted strings are raw** — `\n` is literal backslash+n, not newline.
8. **Semicolons separate statements** in blocks; optional after the last one.
9. **Green threads share memory**; thread pool workers have isolated VMs (communicate via channels).
10. **Block `define`s use `letrec`** — mutual recursion works within a block.
11. **Scheme interop** — prefer arrow methods (`->length`, `->map`, `->to_number`) and operators (`++`). Underscore aliases exist for functions without OO equivalents (`char_to_integer`, `number_to_string`). Backticks only for rare cases.
12. **Signals must be called** `()` to read: `count()`, not `count`. Tracked automatically in `Computed`/`Effect`.
13. **`test_end()` returns failure count** — use as last expression for exit status.
14. **`amb` is lazy** — untried branches never execute.
15. **`with` auto-calls `->close()`** — use for RAII resource management.
