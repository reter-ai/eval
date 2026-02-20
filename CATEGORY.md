# Category Theory

Eval includes a comprehensive category theory toolkit — Maybe/Either/Validation types, Writer/Reader/State monads, Monoid with `<>`, pipe `|>` and bind `>>=` operators, `mdo` do-notation, Traversable, natural transformations, and lenses.

All types use Eval's closure-based message-dispatch pattern. They work automatically with `->` method calls.

## Operators

Three new operators for functional composition:

| Syntax | Name | Meaning | Scheme |
|--------|------|---------|--------|
| `x \|> f` | pipe | apply `f` to `x` | `(f x)` |
| `m >>= f` | bind | monadic bind | `(monad-bind m f)` |
| `a <> b` | mappend | monoidal combine | `(mappend a b)` |

### Pipe `|>`

Passes the left-hand value as the argument to the right-hand function. Reads left-to-right instead of inside-out:

```
5 |> Some;                            // Some(5)
[3,1,2] |> function(xs) xs->sort(<); // [1, 2, 3]

// Pipeline: much clearer than nested calls
[1, 2, 3, 4, 5]
  |> function(xs) xs->filter(function(x) x > 2)
  |> function(xs) xs->map(function(x) x * 10);
// => [30, 40, 50]
```

### Bind `>>=`

Monadic bind — chains computations that may fail, produce multiple results, or carry side effects:

```
Some(5) >>= function(x) Some(x + 1);     // Some(6)
None >>= function(x) Some(x + 1);        // None (short-circuits)
[1,2,3] >>= function(x) [x, x * 10];    // [1, 10, 2, 20, 3, 30]
```

### Mappend `<>`

Combines two monoidal values. Works on strings, lists, numbers, vectors, and any type that implements `->mappend`:

```
"hello" <> " " <> "world";   // "hello world"
[1, 2] <> [3, 4];            // [1, 2, 3, 4]
3 <> 4;                       // 7 (number monoid is addition)
Some(3) <> Some(4);           // Some(7)
Some(3) <> None;              // Some(3)
```

`<>` is also a first-class value — pass it to higher-order functions:

```
fold(<>, "", ["a", "b", "c"]);   // "abc"
fold(<>, 0, [1, 2, 3, 4, 5]);   // 15
```

## Do-notation: `mdo`

`mdo` provides Haskell-style do-notation for sequencing monadic computations. Inside an `mdo` block:

- `x <~ expr;` binds the result of a monadic expression to `x`
- `expr;` sequences a monadic expression (discards result)
- `define x = expr;` creates a local (non-monadic) binding
- The final expression is the return value (must be monadic)

```
// Maybe monad — short-circuits on None
mdo {
  x <~ Some(5);
  y <~ Some(3);
  Some(x + y);
};
// => Some(8)

mdo {
  x <~ Some(5);
  y <~ None;        // short-circuits here
  Some(x + y);
};
// => None

// List monad — nondeterministic computation
mdo {
  x <~ [1, 2, 3];
  y <~ [10, 20];
  [x + y];
};
// => [11, 21, 12, 22, 13, 23]

// Local bindings
mdo {
  define z = 10;
  x <~ Some(z + 5);
  Some(x * 2);
};
// => Some(30)
```

### Compilation

`mdo` desugars at parse time:

| Syntax | Compiles to |
|--------|-------------|
| `x <~ m; rest` | `(monad-bind m (lambda (x) rest))` |
| `m; rest` | `(monad-then m rest)` |
| `define x = v; rest` | `(let ((x v)) rest)` |

## Maybe: `Some` / `None`

Represents an optional value. `Some(x)` holds a value, `None` represents absence.

```
Some(42)->unwrap;             // 42
None->unwrap;                 // ERROR
Some(5)->unwrap_or(0);        // 5
None->unwrap_or(0);           // 0
```

### Properties

| Property | Some(x) | None |
|----------|---------|------|
| `->value` | `x` | error |
| `->is_some` | `true` | `false` |
| `->is_none` | `false` | `true` |
| `->unwrap` | `x` | error |

### Methods

| Method | Some(x) | None |
|--------|---------|------|
| `->map(f)` | `Some(f(x))` | `None` |
| `->bind(f)` | `f(x)` | `None` |
| `->flat_map(f)` | `f(x)` | `None` |
| `->filter(pred)` | `Some(x)` if `pred(x)`, else `None` | `None` |
| `->unwrap_or(default)` | `x` | `default` |
| `->or_else(alt)` | `Some(x)` | `alt()` (calls the thunk) |
| `->apply(m)` | maps `x` over `m` | `None` |
| `->pure(v)` | `Some(v)` | `Some(v)` |
| `->mappend(other)` | `Some(mappend(x, other.value))` | `other` |
| `->match(on_some, on_none)` | `on_some(x)` | `on_none()` |
| `->to_list()` | `[x]` | `[]` |
| `->to_result(err)` | `Ok(x)` | `Err(err)` |

### Examples

```
// Chaining with map
Some(5)->map(function(x) x * 2)->map(function(x) x + 1)->unwrap;
// => 11

// Chaining with bind
Some(5)->bind(function(x)
  if(x > 0) Some(x * 2)
  else None
)->unwrap;
// => 10

// Filtering
Some(5)->filter(function(x) x > 3);   // Some(5)
Some(1)->filter(function(x) x > 3);   // None

// Pattern matching
Some(42)->match(
  function(x) "got " ++ `number->string`(x),
  function() "nothing"
);
// => "got 42"
```

## Either: `Ok` / `Err`

Represents a computation that can succeed (`Ok(x)`) or fail (`Err(e)`). The error side carries information about what went wrong.

```
Ok(10)->map(function(v) v + 1)->unwrap;   // 11
Err("bad")->map(function(v) v + 1);       // Err("bad") — skips map
```

### Properties

| Property | Ok(x) | Err(e) |
|----------|-------|--------|
| `->value` | `x` | error |
| `->error` | error | `e` |
| `->is_ok` | `true` | `false` |
| `->is_err` | `false` | `true` |
| `->unwrap` | `x` | error |

### Methods

| Method | Ok(x) | Err(e) |
|--------|-------|--------|
| `->map(f)` | `Ok(f(x))` | `Err(e)` |
| `->map_err(f)` | `Ok(x)` | `Err(f(e))` |
| `->bind(f)` | `f(x)` | `Err(e)` |
| `->unwrap_or(default)` | `x` | `default` |
| `->or_else(alt)` | `Ok(x)` | `alt(e)` |
| `->match(on_ok, on_err)` | `on_ok(x)` | `on_err(e)` |
| `->to_maybe()` | `Some(x)` | `None` |

### `from_try`

Wraps a thunk in a try/catch, returning `Ok(result)` on success or `Err(exception)` on failure:

```
from_try(function() 10 / 2);   // Ok(5)
from_try(function() 1 / 0);    // Err(...)
```

## Validation: `Valid` / `Invalid`

Like Either but **accumulates errors** in applicative operations. Use this when you want to collect all validation failures, not just the first one.

```
Valid("Alice")->map(function(n) n ++ "!")->value;   // "Alice!"
Invalid(["too short"])->map(function(n) n ++ "!");  // Invalid(["too short"])
```

### Properties and methods

| Method | Valid(x) | Invalid(errors) |
|--------|----------|-----------------|
| `->value` | `x` | error |
| `->errors` | error | `errors` (list) |
| `->is_valid` | `true` | `false` |
| `->is_invalid` | `false` | `true` |
| `->map(f)` | `Valid(f(x))` | `Invalid(errors)` |
| `->bind(f)` | `f(x)` | `Invalid(errors)` |
| `->apply(v)` | applies wrapped function | accumulates errors |
| `->to_result()` | `Ok(x)` | `Err(errors)` |

### Error accumulation with `lift_v2` / `lift_v3`

`lift_v2(f, v1, v2)` applies a two-argument function to two validations, accumulating all errors:

```
define v1 = Valid("Alice");
define v2 = Invalid(["age must be positive"]);
define v3 = Invalid(["email required"]);

// Both validations fail — errors are combined
lift_v2(function(a, b) a, v2, v3)->errors;
// => ["age must be positive", "email required"]

// One succeeds, one fails
lift_v2(function(a, b) a, v1, v3)->errors;
// => ["email required"]

// Both succeed
lift_v2(function(a, b) a ++ " " ++ b, v1, Valid("Smith"))->value;
// => "Alice Smith"
```

### `validate_all`

Combines a list of validations into a single validation of a list:

```
validate_all([Valid(1), Valid(2), Valid(3)])->value;
// => [1, 2, 3]

validate_all([Valid(1), Invalid(["err1"]), Invalid(["err2"])])->errors;
// => ["err1", "err2"]
```

## Writer Monad

Pairs a computation result with an accumulated log (list). Useful for logging, auditing, or collecting side information without mutation.

```
Writer(5, [])->value;   // 5
Writer(5, [])->log;     // []
```

### Methods

| Method | Behavior |
|--------|----------|
| `->value` | the wrapped value |
| `->log` | the accumulated log (list) |
| `->map(f)` | transforms value, preserves log |
| `->bind(f)` | chains, appending logs |
| `->run()` | returns `[value, log]` pair |

### `tell`

Appends a message to the log with no value:

```
tell("step 1")->log;   // ["step 1"]
tell("step 1")->value;  // () (unit)
```

### Example with `mdo`

```
define w = mdo {
  x <~ Writer(5, []);
  tell("got x");
  y <~ Writer(x + 1, []);
  tell("got y");
  Writer(x + y, []);
};
w->value;   // 11
w->log;     // ["got x", "got y"]
```

## Reader Monad

Wraps a function `env -> value` for dependency injection. Pass configuration or context through a computation without threading it as an explicit argument.

### Construction

```
Reader(function(env) env + 1);   // wraps a function
ask();                            // Reader that returns the whole env
asks(function(e) e + 1);         // Reader that projects from env
```

### Methods

| Method | Behavior |
|--------|----------|
| `->run` | the wrapped function (call with env) |
| `->map(f)` | transforms the result |
| `->bind(f)` | chains readers |

### `run_reader(r, env)`

Runs a Reader with a given environment:

```
run_reader(asks(function(e) e * 2), 21);   // 42
```

### Example with `mdo`

```
define prog = mdo {
  x <~ Reader(function(e) e + 1);
  y <~ Reader(function(e) e + 2);
  Reader(function(e) x + y);
};
run_reader(prog, 10);   // 23 (x=11, y=12)
```

## State Monad

Wraps a function `state -> [value, new_state]` for stateful computation without mutation. Each step receives the current state and produces a new one.

### Construction

```
State(function(s) [s * 2, s + 1]);   // explicit state function
get_state();                          // State that returns current state as value
put_state(42);                        // State that replaces the state
modify_state(function(s) s + 1);     // State that transforms the state
```

### Methods

| Method | Behavior |
|--------|----------|
| `->run` | the wrapped function (call with initial state) |
| `->map(f)` | transforms the value, preserves state |
| `->bind(f)` | chains, threading state through |

### `run_state(st, initial)`

Runs a State computation with an initial state, returns `[value, final_state]`:

```
run_state(get_state(), 42);   // [42, 42]
run_state(put_state(10), 42); // [(), 10]
```

### Example with `mdo`

```
define counter = mdo {
  modify_state(function(s) s + 1);
  modify_state(function(s) s + 1);
  x <~ get_state();
  State(function(s) [x, s]);
};
run_state(counter, 0);   // [2, 2]
```

## Monoid

A monoid is a type with an associative binary operation (`mappend` / `<>`) and an identity element (`mempty`).

### Built-in monoid instances

| Type | `a <> b` | `mempty(type)` |
|------|----------|----------------|
| string | `string-append` | `""` |
| list | `append` | `[]` |
| number | `+` | `0` |
| vector | concatenation | n/a |
| Maybe | `Some(a.value <> b.value)` | n/a |
| any `->mappend` | delegates to method | n/a |

### Functions

| Function | Description |
|----------|-------------|
| `mappend(a, b)` | combine two values |
| `mempty(type)` | identity element for a type (`'string`, `'list`, `'number`) |
| `mconcat(type, list)` | fold a list with mappend |

```
mconcat('string, ["a", "b", "c"]);   // "abc"
mconcat('number, [1, 2, 3, 4, 5]);   // 15
mconcat('list, [[1], [2, 3], [4]]);   // [1, 2, 3, 4]
```

## List monad

Lists are monads too. `>>=` on a list is `flat_map` (bind each element, concatenate results):

```
[1, 2, 3] >>= function(x) [x, x * 10];
// => [1, 10, 2, 20, 3, 30]

// Nondeterministic computation with mdo
mdo {
  suit <~ ["hearts", "diamonds", "clubs", "spades"];
  rank <~ [1, 2, 3];
  [[rank, suit]];
};
// => [[1, "hearts"], [2, "hearts"], [3, "hearts"],
//     [1, "diamonds"], ...]
```

Lists also support `->bind(f)`, `->pure(v)`, and `->mappend(other)` via method dispatch.

## Traversable

Flip the layers of nested structures.

### `sequence(ms)`

Converts a list of monadic values into a monadic list of values:

```
sequence([Some(1), Some(2), Some(3)])->unwrap;
// => [1, 2, 3]

sequence([Some(1), None, Some(3)])->is_none;
// => true (None poisons the whole sequence)

sequence([[1, 2], [3, 4]]);
// => [[1, 3], [1, 4], [2, 3], [2, 4]]  (cartesian product)
```

### `traverse(f, xs)`

Maps a function over a list and sequences the results:

```
traverse(function(x) if(x > 0) Some(x) else None, [1, 2, 3])->unwrap;
// => [1, 2, 3]

traverse(function(x) if(x > 0) Some(x) else None, [1, -2, 3])->is_none;
// => true
```

## Natural transformations

Convert between functor types:

| Function | From | To |
|----------|------|----|
| `maybe_to_list(m)` | Maybe | List |
| `maybe_to_result(err, m)` | Maybe | Either |
| `result_to_maybe(r)` | Either | Maybe |
| `list_to_maybe(lst)` | List | Maybe |
| `validation_to_result(v)` | Validation | Either |

```
maybe_to_list(Some(5));            // [5]
maybe_to_list(None);               // []
result_to_maybe(Ok(5))->unwrap;    // 5
result_to_maybe(Err("x"))->is_none; // true
list_to_maybe([])->is_none;         // true
list_to_maybe([1, 2, 3])->unwrap;  // 1
```

## Lenses

Composable getters/setters for nested data. A lens focuses on a part of a structure, allowing you to read, update, or modify it without mutating the original.

### `Lens(getter, setter)`

Create a lens from a getter function and a setter function. The setter takes `(whole, new_part)` and returns a new whole:

```
define first_lens = Lens(
  function(lst) car(lst),
  function(lst, val) cons(val, cdr(lst))
);
first_lens->get([10, 20, 30]);                       // 10
first_lens->set([10, 20, 30], 99);                   // [99, 20, 30]
first_lens->over(function(x) x * 2, [10, 20, 30]);  // [20, 20, 30]
```

### Methods

| Method | Description |
|--------|-------------|
| `->get` | getter function — `lens->get(obj)` |
| `->set` | setter function — `lens->set(obj, val)` |
| `->over(f, obj)` | apply `f` to the focused part |
| `->compose(inner)` | compose with another lens for nested access |

### `index_lens(i)`

Focuses on the i-th element of a list:

```
define second = index_lens(1);
second->get([10, 20, 30]);        // 20
second->set([10, 20, 30], 99);   // [10, 99, 30]
```

### `dict_lens(key)`

Focuses on a key in a dict (hash-table). Returns a new dict on set/over (immutable update):

```
define name = dict_lens("name");
name->get(d);
name->set(d, "Bob");
name->over(function(n) n ++ "!", d);
```

### Lens composition

Compose lenses to access deeply nested data:

```
define outer = index_lens(1);
define inner = index_lens(0);
define nested = outer->compose(inner);

nested->get([[1, 2], [3, 4], [5, 6]]);      // 3
nested->set([[1, 2], [3, 4], [5, 6]], 99);  // [[1, 2], [99, 4], [5, 6]]
```

## Combinators

Utility functions for functional programming:

| Function | Description |
|----------|-------------|
| `monad_bind(m, f)` | same as `m >>= f` |
| `monad_then(m, next)` | bind discarding the result |
| `from_nullable(x)` | `None` if `x` is null, else `Some(x)` |
| `from_try(thunk)` | `Ok(result)` or `Err(exception)` |
| `maybe(default, f, m)` | extract from Maybe with default |
| `either(on_ok, on_err, r)` | extract from Either |
| `compose(f, g)` | `function(x) f(g(x))` |
| `flip(f)` | `function(a, b) f(b, a)` |
| `const(x)` | `function(_) x` |
| `fish(f, g)` | Kleisli composition `>=>` |

### Kleisli composition

`fish(f, g)` composes two monadic functions — the output of `f` is bound into `g`:

```
define safe_div = function(y) if(y == 0) None else Some(1/y);
define safe_sqrt = function(x) if(x < 0) None else Some(x ** 0.5);
define safe_op = fish(safe_div, safe_sqrt);

safe_op(4)->unwrap;    // 0.5  (1/4 = 0.25, sqrt(0.25) = 0.5)
safe_op(0)->is_none;   // true (division by zero)
```

## Complete example: validation pipeline

```
// Validate user input, accumulating all errors
define validate_name = function(n)
  if(n->length > 0) Valid(n)
  else Invalid(["name required"]);

define validate_age = function(a)
  if(a > 0 && a < 150) Valid(a)
  else Invalid(["age must be 1-149"]);

define validate_email = function(e)
  if(e->contains("@")) Valid(e)
  else Invalid(["invalid email"]);

define make_user = function(name, age)
  [name, age];

// All valid
lift_v2(make_user,
  validate_name("Alice"),
  validate_age(30)
)->value;
// => ["Alice", 30]

// Multiple failures
lift_v2(make_user,
  validate_name(""),
  validate_age(-5)
)->errors;
// => ["name required", "age must be 1-149"]
```

## Complete example: Writer logging

```
define safe_divide = function(a, b) {
  if(b == 0)
    Writer(0, ["division by zero"])
  else
    Writer(a / b, [f"divided {a} by {b}"]);
};

define computation = mdo {
  x <~ Writer(100, ["start"]);
  y <~ safe_divide(x, 4);
  z <~ safe_divide(y, 5);
  Writer(z, []);
};

computation->value;   // 5
computation->log;     // ["start", "divided 100 by 4", "divided 25 by 5"]
```

## Complete example: State counter

```
define push = function(x)
  State(function(stack) [nil, cons(x, stack)]);

define pop = State(function(stack) [car(stack), cdr(stack)]);

define program = mdo {
  push(10);
  push(20);
  push(30);
  x <~ pop;
  y <~ pop;
  State(function(s) [x + y, s]);
};

run_state(program, nil);
// => [50, (10)]  — popped 30 and 20, 10 remains on stack
```

## Further reading

- [INTRO.md](INTRO.md) — complete language reference
- [LISTS.md](LISTS.md) — list methods including `->bind`, `->pure`, `->mappend`
- [GENERATORS.md](GENERATORS.md) — lazy sequences and pipelines
- [AMB.md](AMB.md) — nondeterministic choice (alternative approach to list monad)
