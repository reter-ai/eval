# Forward-Chaining Rules (Rete)

Eval includes a forward-chaining rule engine based on the [Rete algorithm](https://en.wikipedia.org/wiki/Rete_algorithm). Unlike backward-chaining (`run`, `fresh`, `conde`) where you query for solutions, forward rules fire automatically when matching facts are asserted.

```
// Backward-chaining: "who is bob's parent?"
run(*, ?q) { parent(?q, "bob") };

// Forward-chaining: "when anyone becomes a parent, do this"
whenever parent(?x, ?y) {
    display(f"{?x} is parent of {?y}");
    newline();
}
fact parent("tom", "bob");   // fires the rule immediately
```

## `whenever` — Forward Rules

The `whenever` keyword defines rules that fire automatically when matching facts are asserted:

```
whenever parent(?x, ?y) {
    display(f"{?x} -> {?y}");
    newline();
}

fact parent("tom", "bob");    // prints: tom -> bob
fact parent("bob", "ann");    // prints: bob -> ann
```

The rule body executes once for each fact that matches the pattern. Pattern variables (`?x`, `?y`) are bound to the corresponding fact arguments.

### Multiple Conditions (Joins)

Rules can have multiple conditions separated by commas. The rule fires when all conditions are satisfied simultaneously, with shared variables acting as join constraints:

```
// Fires when two ancestor facts share a middle value
whenever ancestor(?x, ?y), ancestor(?y, ?z) {
    display(f"{?x} is grandparent of {?z}");
    newline();
}

fact ancestor("tom", "bob");   // no match yet (need second condition)
fact ancestor("bob", "ann");   // fires! ?y="bob" joins the two facts
// prints: tom is grandparent of ann
```

Three or more conditions work the same way:

```
whenever link(?a, ?b), link(?b, ?c), link(?c, ?d) {
    display(f"chain: {?a} -> {?d}");
    newline();
}

fact link("a", "b");
fact link("b", "c");
fact link("c", "d");    // fires: chain: a -> d
```

### Constant Filtering

Pattern arguments can be constants — only facts matching the constant fire the rule:

```
whenever parent("tom", ?child) {
    display(f"Tom's child: {?child}");
    newline();
}

fact parent("tom", "bob");    // fires: Tom's child: bob
fact parent("bob", "ann");    // does NOT fire (not "tom")
fact parent("tom", "sue");    // fires: Tom's child: sue
```

Constants and variables can be mixed freely across conditions:

```
whenever rel("start", ?x), rel(?x, ?end) {
    display(f"path: start -> {?end}");
    newline();
}

fact rel("start", "mid");
fact rel("mid", "goal");    // fires: path: start -> goal
```

### Late Rule Activation

Rules added after facts still fire — the engine replays existing facts through newly added rules:

```
fact early("a", "b");
fact early("b", "c");

// Rule added after facts — fires immediately on existing matches
whenever early(?x, ?y), early(?y, ?z) {
    display(f"chain: {?x} -> {?z}");
    newline();
}
// prints: chain: a -> c
```

### Chaining

Rules can assert new facts in their body, which trigger further rules:

```
whenever step1(?x) {
    logic_assert_fact('step2, ?x);
}

whenever step2(?x) {
    display(f"reached step2 with {?x}");
    newline();
}

fact step1("hello");
// step1 fires, asserts step2("hello"), which fires the second rule
// prints: reached step2 with hello
```

Note: Use `logic_assert_fact('name, args...)` instead of `fact name(...)` inside rule bodies, because the `fact` statement form includes a `define` which is not allowed inside lambda bodies.

### Multiple Rules

Multiple rules can watch the same relation — all matching rules fire independently:

```
define count1 = 0;
define count2 = 0;

whenever likes(?a, ?b) {
    count1 = count1 + 1;
}

whenever likes(?x, ?y) {
    count2 = count2 + 1;
}

fact likes("alice", "bob");    // both rules fire
fact likes("bob", "carol");   // both rules fire again
// count1 == 2, count2 == 2
```

## Coexistence with Backward-Chaining

Forward and backward chaining share the same fact database. When you assert a fact with `fact`, it is:
1. Stored in the miniKanren database (for backward-chaining queries)
2. Propagated through the Rete network (firing any matching forward rules)

Both systems work together seamlessly:

```
// Forward: log new facts as they arrive
whenever person(?name, ?age) {
    display(f"registered: {?name}");
    newline();
}

fact person("alice", 30);
fact person("bob", 25);
fact person("carol", 35);

// Backward: query the same facts
run(*, ?q) { fresh(?age) { person(?q, ?age) } };
// => ["alice", "bob", "carol"]
```

## Management Functions

| Function | Description |
|----------|-------------|
| `__rete_rule_count__()` | Number of registered forward rules |
| `__rete_reset__()` | Clear all forward rules and memories |

```
__rete_rule_count__();    // => number of active whenever rules
__rete_reset__();          // remove all rules, clear all memories
__rete_rule_count__();    // => 0
```

## How It Works

### The Rete Algorithm

The Rete algorithm builds a discrimination network that efficiently matches facts against rule patterns:

- **Alpha nodes** filter individual facts by relation name and constant field tests. Each unique pattern creates an alpha node that remembers all matching facts.
- **Beta nodes** join partial matches across multiple conditions. They test that shared variables have equal values across facts from different alpha nodes.
- **Tokens** represent partial matches flowing through the beta network. Each token holds one fact per matched condition.
- **Terminal nodes** fire the rule action when all conditions are matched, extracting variable bindings from the completed token.

### Compilation

`whenever` compiles to a call to `__rete_add_rule__`:

| Eval syntax | Compiles to |
|---|---|
| `whenever parent(?x, ?y) { body }` | `(__rete_add_rule__ conditions var-names (lambda (x y) body))` |

Pattern variables `?x` become tagged symbols `__var_x` that the Rete engine recognizes as variables (vs. constants).

### Performance

The Rete network is implemented in C for efficiency:
- O(1) alpha node lookup per relation (hashed by name)
- Constant field tests filter non-matching facts before reaching the beta network
- Join tests only check relevant variable bindings
- Facts already in memory when a new rule is added are replayed through the new rule

## See Also

- [PROLOG.md](PROLOG.md) — Backward-chaining logic programming: facts, rules, run, fresh, conde
- [AMB.md](AMB.md) — Nondeterministic choice: amb, require, backtracking search
- [INTRO.md](INTRO.md) — Complete language reference
- [README.md](README.md) — Project overview and quick start
