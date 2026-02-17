# Reactive Programming in Eval

Eval has a built-in reactive system inspired by Solid.js and MobX. It gives you fine-grained reactivity with automatic dependency tracking: when data changes, everything that depends on it updates — automatically, efficiently, and in the right order.

This article walks through the entire reactive API, from core primitives to utility functions, with practical examples throughout.

## The Core Idea

Reactive programming inverts the usual control flow. Instead of manually wiring "when X changes, update Y", you declare relationships and the system figures out what to update:

```
define firstName = Signal("Alice");
define lastName = Signal("Smith");
define fullName = Computed(function() firstName() ++ " " ++ lastName());

fullName();   // => "Alice Smith"

firstName->set("Bob");
fullName();   // => "Bob Smith" — updated automatically
```

No event listeners. No callbacks. No manual subscriptions. You read `fullName()` and it always reflects the current state of `firstName` and `lastName`.

## Signals: Reactive State

A **Signal** is a container for a mutable value. It's the atom of reactivity — the source of truth that everything else derives from.

### Creating and Reading

```
define count = Signal(0);
count();          // => 0
```

Calling a signal with no arguments reads its value. This read is *tracked* — if you're inside a Computed or Effect, the system records that you depend on this signal.

### Writing

```
count->set(5);    // set to 5
count();          // => 5

count->update(function(v) v + 1);   // increment
count();          // => 6
```

`set` replaces the value. `update` applies a function to the current value. Both skip notification if the new value equals the old one (structural equality via `==`).

### Peeking

```
count->peek;      // => 6 (no tracking)
```

`peek` reads the value without registering a dependency. Useful when you need the value but don't want to trigger re-computation when it changes.

## Computed: Derived Values

A **Computed** is a value derived from other reactive sources. It auto-tracks which signals (or other computeds) it reads, and recomputes lazily when any of them change.

```
define celsius = Signal(0);
define fahrenheit = Computed(function() celsius() * 9 / 5 + 32);

fahrenheit();     // => 32
celsius->set(100);
fahrenheit();     // => 212
```

### Lazy and Cached

Computed values are lazy — they don't recompute until you read them. And they're cached — reading twice without any dependency changes returns the same value without recomputing:

```
define calls = 0;
define expensive = Computed(function() {
    calls++;
    celsius() * 2;
});

expensive();   // computes, calls = 1
expensive();   // cached, calls still 1
expensive();   // cached, calls still 1

celsius->set(50);
// not computed yet — lazy!
expensive();   // now recomputes, calls = 2
```

### Chains

Computed values can depend on other computed values, forming chains:

```
define base = Signal(10);
define doubled = Computed(function() base() * 2);
define quadrupled = Computed(function() doubled() * 2);

quadrupled();    // => 40
base->set(5);
quadrupled();    // => 20 — entire chain updates
```

### Conditional Dependencies

Dependencies are tracked dynamically on each evaluation. If a branch isn't taken, its signals aren't tracked:

```
define mode = Signal("simple");
define a = Signal(1);
define b = Signal(2);
define result = Computed(function()
    if(mode() == "simple") a() else a() + b()
);

result();        // => 1 (tracks mode, a — NOT b)
b->set(999);
result();        // => 1 (b change ignored — not a dependency)

mode->set("complex");
result();        // => 1000 (now tracks mode, a, AND b)
```

## Effects: Side Effects

An **Effect** runs a function for its side effects and re-runs it whenever its dependencies change.

```
define name = Signal("world");
define eff = Effect(function() {
    print("Hello, " ++ name() ++ "!");
});
// prints: Hello, world!

name->set("Eval");
// prints: Hello, Eval!
```

Effects run immediately when created, then re-run on each dependency change.

### Cleanup

If your effect function returns a procedure, that procedure becomes a **cleanup function**. It runs before the next execution and on disposal:

```
define url = Signal("/api/users");
define eff = Effect(function() {
    define endpoint = url();
    print("subscribing to " ++ endpoint);
    // return cleanup function
    function() print("unsubscribing from " ++ endpoint);
});
// prints: subscribing to /api/users

url->set("/api/posts");
// prints: unsubscribing from /api/users
// prints: subscribing to /api/posts
```

This pattern mirrors React's `useEffect` cleanup — perfect for subscriptions, timers, or any resource that needs teardown.

### Dispose

Stop an effect (or any reactive node) with `dispose`:

```
define s = Signal(0);
define log = [];
define eff = Effect(function() {
    log = cons(s(), log);
});
// log = [0]

s->set(1);
// log = [1, 0]

dispose(eff);
s->set(999);
// log unchanged — effect is gone
```

Disposal also runs the cleanup function if one exists.

### RAII with `with`

For scoped effects, use `with` to auto-dispose on scope exit:

```
define s = Signal(0);
with(e = Effect(function() print(s()))) {
    s->set(1);
    s->set(2);
};
// prints: 0, 1, 2

s->set(99);
// nothing — effect was disposed when `with` block ended
```

## Batching

When you update multiple signals, each update would normally trigger its own cascade of recomputations. **Batch** groups updates so everything flushes once at the end:

```
define a = Signal(0);
define b = Signal(0);
define log = [];
define eff = Effect(function() {
    log = cons(a() + b(), log);
});
log = [];

batch(function() {
    a->set(10);
    b->set(20);
});
// log = [30] — one effect run, not two
```

Without batching, you'd get `[30, 10]` — one run after `a` changes (reading stale `b`), then another after `b` changes.

### The Diamond Problem

Batching automatically solves the "diamond problem" where two paths lead to the same consumer:

```
//     price
//    /     \
// tax     discount
//    \     /
//    total (effect)

define price = Signal(100);
define tax = Computed(function() price() * 0.1);
define discount = Computed(function() price() * 0.05);
define eff = Effect(function() {
    print("total: " ++ `number->string`(price() + tax() - discount()));
});
// prints: total: 105

price->set(200);
// prints: total: 210 — ONE print, consistent values
```

The system uses topological ordering (by dependency level) to ensure that when an effect runs, all its dependencies are already up to date. No glitches, no double-fires.

### Nested Batches

Batches nest — only the outermost batch triggers a flush:

```
batch(function() {
    a->set(1);
    batch(function() {
        a->set(2);
        a->set(3);
    });
    // inner batch does NOT flush
    a->set(4);
});
// NOW everything flushes — effect sees 4
```

## Untracked: Opting Out of Tracking

Sometimes you want to read a signal inside a Computed or Effect without creating a dependency. Wrap the read in `untracked`:

```
define query = Signal("hello");
define config = Signal("default");

define results = Computed(function() {
    define q = query();
    define cfg = untracked(function() config());
    search(q, cfg);
});

// results recomputes when query changes
// results does NOT recompute when config changes
// but when query does change, it reads the current config
```

This is useful when you need a value for computation but don't want to subscribe to its changes.

## Derived: Quick One-Signal Transform

`derived` is a shortcut for creating a Computed that transforms a single signal:

```
define count = Signal(0);
define doubled = derived(count, function(v) v * 2);
define negated = derived(count, function(v) -v);
define label = derived(count, function(v) "count is " ++ `number->string`(v));

count->set(5);
doubled();    // => 10
negated();    // => -5
label();      // => "count is 5"
```

It's equivalent to `Computed(function() fn(source()))` but reads more declaratively.

## Readonly: Protecting State

`readonly` wraps a Signal (or Computed) to expose only the read side. Writes are blocked:

```
define _count = Signal(0);
define count = readonly(_count);

count();          // => 0 (reading works fine)
count->peek;      // => 0 (peeking works too)

count->set(5);    // ERROR: readonly: cannot set
count->update(function(v) v + 1);  // ERROR: readonly: cannot update
```

This is the "expose read, keep write private" pattern. A module can export the readonly view while keeping the writable signal internal:

```
// counter module
define _state = Signal(0);
define state = readonly(_state);

define increment = function() _state->update(function(v) v + 1);
define decrement = function() _state->update(function(v) v - 1);

// Consumers can read state() and use it in Computed/Effect,
// but they can't set it directly — only through increment/decrement.
```

Readonly views fully participate in the reactive graph — you can use them as dependencies in Computed and Effect:

```
define inner = Signal(42);
define ro = readonly(inner);
define c = Computed(function() ro() + 1);

c();            // => 43
inner->set(10);
c();            // => 11 (tracked through readonly)
```

Check with `readonly?(x)`.

## Watch: Observing Changes

`watch` calls a function with `(new_value, old_value)` each time a signal changes. Unlike Effect, it **skips the initial value** — it only fires on actual changes:

```
define temperature = Signal(20);
define w = watch(temperature, function(new_val, old_val) {
    print("Temperature changed from "
        ++ `number->string`(old_val)
        ++ " to "
        ++ `number->string`(new_val));
});
// (nothing printed — initial run is silent)

temperature->set(25);
// prints: Temperature changed from 20 to 25

temperature->set(30);
// prints: Temperature changed from 25 to 30
```

`watch` returns an Effect, so you can dispose it to stop watching:

```
dispose(w);
temperature->set(99);
// nothing — watcher is gone
```

### When to use watch vs Effect

| Use case | Tool |
|---|---|
| Run code whenever dependencies change (including initially) | `Effect` |
| React to a specific signal's changes with old/new values | `watch` |
| Log or debug value transitions | `watch` |
| Sync to external system on every change | `Effect` |

## On: Explicit Dependencies

`on` creates an effect with an explicit dependency list. Only the signals you list are tracked — any signals read inside the callback body are **not** tracked:

```
define search = Signal("");
define page = Signal(1);
define debugFlag = Signal(false);

on([search, page], function(query, pg) {
    // debugFlag is read but NOT tracked
    when(debugFlag->peek) print("debug: fetching");
    print("Fetching: " ++ query ++ " page " ++ `number->string`(pg));
});
// prints: Fetching:  page 1

search->set("eval reactive");
// prints: Fetching: eval reactive page 1

debugFlag->set(true);
// (nothing — debugFlag is not a dependency)

page->set(2);
// prints: debug: fetching
// prints: Fetching: eval reactive page 2
```

The callback receives the current values of the listed dependencies as arguments, in order.

### When to use on vs Effect

| Use case | Tool |
|---|---|
| Auto-track all reads | `Effect` |
| Control exactly which signals trigger re-runs | `on` |
| Avoid accidental dependencies | `on` |
| Read helper signals without subscribing | `on` |

## Reduce: Accumulating Over Time

`reduce` folds over signal changes, maintaining an accumulator. It returns a new Signal holding the accumulated result:

```
define clicks = Signal(0);
define totalClicks = reduce(clicks, function(acc, value) acc + value, 0);

totalClicks();     // => 0 (initial)

clicks->set(1);
totalClicks();     // => 1

clicks->set(5);
totalClicks();     // => 6

clicks->set(3);
totalClicks();     // => 9
```

The function receives `(accumulator, new_value)` and returns the new accumulator. The initial value is skipped (it uses `watch` internally).

### Examples

**Max tracker** — remember the highest value seen:

```
define readings = Signal(0);
define maxReading = reduce(readings,
    function(best, v) if(v > best) v else best, 0);

readings->set(5);
maxReading();     // => 5

readings->set(3);
maxReading();     // => 5 (3 is not higher)

readings->set(8);
maxReading();     // => 8
```

**History buffer** — keep the last N values:

```
define value = Signal(0);
define history = reduce(value, function(acc, v) {
    define extended = append(acc, [v]);
    if(length(extended) > 5)
        cdr(extended)
    else
        extended;
}, []);

value->set(1);
value->set(2);
value->set(3);
history();   // => [1, 2, 3]
```

**Change counter** — count how many times a signal changed:

```
define name = Signal("Alice");
define changeCount = reduce(name, function(acc, v) acc + 1, 0);

name->set("Bob");
name->set("Carol");
name->set("Dave");
changeCount();   // => 3
```

Since `reduce` returns a Signal, the result is itself reactive — you can use it in Computed, Effect, watch, or even another reduce.

## Scope: Ownership Groups

In any non-trivial application, you create many reactive nodes that belong together — signals, computeds, and effects that represent a single "component" or feature. `scope` groups them so you can dispose everything at once:

```
define s = Signal(0);
define handle = scope(function() {
    define doubled = Computed(function() s() * 2);
    define quadrupled = Computed(function() doubled() * 2);
    Effect(function() {
        print("value: " ++ `number->string`(quadrupled()));
    });
});
// prints: value: 0

s->set(5);
// prints: value: 20

dispose(handle);
s->set(99);
// nothing — all nodes in the scope are disposed
```

Every Signal, Computed, and Effect created inside the scope function is automatically registered. When the scope is disposed, all children are disposed in one shot.

### Scoped RAII

Scope works naturally with `with`:

```
define s = Signal("hello");
with(h = scope(function() {
    Effect(function() print(s()));
    watch(s, function(n, o) print("changed!"));
})) {
    s->set("world");
};
// After the with block, both effects are gone
s->set("ignored");
```

### Nested Scopes

Scopes can nest. Disposing an inner scope doesn't affect the outer:

```
define s = Signal(0);
define outer = scope(function() {
    Effect(function() print("outer: " ++ `number->string`(s())));

    define inner = scope(function() {
        Effect(function() print("inner: " ++ `number->string`(s())));
    });

    // Kill just the inner scope
    dispose(inner);
});

s->set(1);
// prints: outer: 1
// (inner is gone)

dispose(outer);
```

This enables hierarchical component patterns where each sub-component manages its own lifecycle.

## Custom Equality

By default, Signal and Computed use structural equality (`==` / `equal?`) to decide whether a value has changed. You can override this with a custom equality function.

### Signal with Custom Equality

```
// Identity equality — treat every new object as different
define items = Signal([1, 2, 3], =?);
items->set([1, 2, 3]);   // notifies! (different list object)

// Default would skip (same contents):
define items2 = Signal([1, 2, 3]);
items2->set([1, 2, 3]);  // skipped — equal? says same
```

Use cases:
- `=?` (identity) when you always want to notify on set, even with equal values
- A custom field comparator when only some fields matter
- `function(a, b) false` to always notify (no dedup)

### Computed with Custom Equality

Custom equality on Computed controls **downstream propagation**. If the computed recomputes but the equality function says the value hasn't changed, observers are not notified:

```
define price = Signal(9.99);
define displayPrice = Computed(
    function() `floor`(price()),
    function(a, b) a == b
);

// displayPrice starts at 9
price->set(9.50);
// displayPrice recomputes to 9 — same as before
// downstream effects do NOT re-run

price->set(10.50);
// displayPrice is now 10 — different
// downstream effects fire
```

This is powerful for performance: you can have a frequently-changing source but only propagate when the derived result meaningfully changes.

## WritableComputed: Two-Way Binding

A `WritableComputed` reads like a Computed but also accepts writes, dispatching them through a setter function:

```
define celsius = Signal(0);
define fahrenheit = WritableComputed(
    function() celsius() * 9 / 5 + 32,
    function(f) celsius->set((f - 32) * 5 / 9)
);

fahrenheit();           // => 32 (reading computes from celsius)

fahrenheit->set(212);   // writing goes through setter
celsius();              // => 100

celsius->set(0);
fahrenheit();           // => 32 (back to freezing)
```

This is the "two-way binding" pattern. The getter defines how to compute the derived value; the setter defines how to push writes back to the source signals. Downstream tracking works exactly like Computed:

```
define eff = Effect(function() {
    print("Temperature: " ++ `number->string`(fahrenheit()) ++ "F");
});
// prints: Temperature: 32F

celsius->set(100);
// prints: Temperature: 212F

fahrenheit->set(72);
// prints: Temperature: 72F
```

## Combine: Merging Multiple Signals

`combine` creates a Computed from a list of signals and a merge function:

```
define firstName = Signal("Alice");
define lastName = Signal("Smith");
define age = Signal(30);

define summary = combine([firstName, lastName, age],
    function(first, last, a)
        first ++ " " ++ last ++ ", age " ++ `number->string`(a));

summary();   // => "Alice Smith, age 30"

lastName->set("Johnson");
summary();   // => "Alice Johnson, age 30"
```

`combine` is like `derived` but for N sources. It's equivalent to writing `Computed(function() fn(s1(), s2(), ...))` but reads more declaratively when you have a known set of sources.

## Select: Fine-Grained Slices

`select` extracts a slice from a signal and only propagates when that specific slice changes:

```
define user = Signal(dict(name: "Alice", age: 30, email: "a@b.com"));
define userName = select(user, function(u) u->name);
define userAge = select(user, function(u) u->age);

// Update age only — userName doesn't propagate
user->set(dict(name: "Alice", age: 31, email: "a@b.com"));
// userName is still "Alice" — observers not notified
// userAge changed to 31 — observers notified
```

This is critical for performance with complex state objects. Without `select`, any change to the user signal would trigger ALL downstream computations. With `select`, only the relevant slice's consumers react.

Accepts an optional custom equality as a third argument:

```
define sel = select(source, selector_fn, custom_eq_fn);
```

## Prev: Previous Value

`prev` creates a read-only signal that holds the previous value of a source — always one step behind:

```
define position = Signal(0);
define lastPosition = prev(position);

lastPosition();     // => false (default initial)
position->set(10);
lastPosition();     // => 0
position->set(25);
lastPosition();     // => 10
```

With a custom initial value:

```
define count = Signal(0);
define prevCount = prev(count, -1);
prevCount();        // => -1 (custom initial, not false)
```

`prev` returns a `readonly` signal, so consumers can read and track it but not write to it.

### Computing Deltas

`prev` is particularly useful for computing changes:

```
define temperature = Signal(20);
define prevTemp = prev(temperature, 20);
define delta = Computed(function() temperature() - prevTemp());

delta();             // => 0
temperature->set(25);
delta();             // => 5
temperature->set(22);
delta();             // => -3
```

## Trace: Debug Logging

`trace` attaches a change logger to any signal. It prints `[label] old -> new` on every change:

```
define count = Signal(0);
trace(count, "count");

count->set(5);
// [count] 0 -> 5

count->set(10);
// [count] 5 -> 10
```

`trace` returns the source signal unchanged, so you can sprinkle it in without modifying behavior. Remove the `trace` call when you're done debugging.

## Type Predicates

Every reactive type has a predicate:

```
signal?(Signal(0));                        // => true
computed?(Computed(function() 1));         // => true
effect?(Effect(function() 1));             // => true
readonly?(readonly(Signal(0)));            // => true
scope?(scope(function() {}));              // => true
writable_computed?(WritableComputed(
    function() 1, function(v) v));         // => true
```

These work by checking the internal `__type__` message, so they're safe to call on any value — non-reactive values return `false`.

## Putting It Together

Here's a larger example that combines multiple reactive features — a simple reactive store pattern:

```
// === Reactive Store ===

// Private mutable state
define _items = Signal([]);
define _filter = Signal("");

// Public readonly views
define items = readonly(_items);
define filterText = readonly(_filter);

// Derived computations
define filteredItems = Computed(function() {
    define f = filterText();
    define all = items();
    if(f == "")
        all
    else
        filter(function(item) `string-contains`(item, f), all);
});

define itemCount = derived(filteredItems, function(xs) length(xs));

// Actions (the only way to mutate state)
define addItem = function(item) {
    _items->update(function(xs) append(xs, [item]));
};

define setFilter = function(f) {
    _filter->set(f);
};

// Change tracking
define changeLog = reduce(_items, function(acc, v)
    acc + 1, 0);

// UI effect
define ui = Effect(function() {
    print("Showing " ++ `number->string`(itemCount())
        ++ " items (filter: '"
        ++ filterText() ++ "')");
});
// prints: Showing 0 items (filter: '')

addItem("buy milk");
// prints: Showing 1 items (filter: '')

addItem("buy eggs");
addItem("walk dog");
// prints: Showing 2 items (filter: '')
// prints: Showing 3 items (filter: '')

setFilter("buy");
// prints: Showing 2 items (filter: 'buy')

changeLog();   // => 3 (three addItem calls)

// Cleanup
dispose(ui);
```

Key patterns at work:
- **Signals** hold the source-of-truth state
- **readonly** exposes read-only views to consumers
- **Computed** derives filtered list and count automatically
- **derived** creates the count as a simple transform
- **Effect** reacts to any change in the dependency graph
- **reduce** accumulates a change count over time
- **dispose** cleans up when done

The reactive graph handles all the wiring. Change a signal anywhere, and everything downstream updates — in the right order, at most once, with no manual plumbing.

### Component Pattern with Scope

The same store, but using `scope` for lifecycle management and the new utilities:

```
define app = scope(function() {
    // State
    define _items = Signal([]);
    define _filter = Signal("");

    // Public API
    define items = readonly(_items);
    define filterText = readonly(_filter);

    // Derived
    define filtered = Computed(function() {
        define f = filterText();
        define all = items();
        if(f == "")
            all
        else
            filter(function(item) `string-contains`(item, f), all);
    });

    define count = derived(filtered, function(xs) length(xs));

    // Track the previous filter for "undo" UX
    define prevFilter = prev(_filter, "");

    // Celsius/Fahrenheit display toggle
    define tempC = Signal(20);
    define tempF = WritableComputed(
        function() tempC() * 9 / 5 + 32,
        function(f) tempC->set((f - 32) * 5 / 9)
    );

    // Combine for status line
    define status = combine([count, filterText, tempF],
        function(n, f, temp)
            `number->string`(n) ++ " items"
            ++ if(f == "") "" else " (filter: " ++ f ++ ")"
            ++ " | " ++ `number->string`(temp) ++ "F"
    );

    // Debug during development
    trace(_items, "items");
    trace(_filter, "filter");

    // UI effect
    Effect(function() print(status()));

    // Actions
    define addItem = function(item) {
        _items->update(function(xs) append(xs, [item]));
    };
    define setFilter = function(f) _filter->set(f);

    addItem("buy milk");
    addItem("buy eggs");
    addItem("walk dog");
    setFilter("buy");
    tempF->set(72);
});

// When done, one call cleans up everything
dispose(app);
```

Every signal, computed, effect, watcher, and tracer created inside the scope is disposed with a single `dispose(app)`. No manual cleanup bookkeeping.
