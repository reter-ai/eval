# Logic Programming

Eval includes built-in logic programming inspired by [miniKanren](http://minikanren.org/) and Prolog. Declare facts and rules, search for solutions with unification, and mix logic with regular Eval code.

```
// Find all pairs that satisfy the relation
fact parent("tom", "bob");
fact parent("bob", "ann");

run(*, ?q) {
    fresh(?x, ?y) {
        parent(?x, ?y),
        ?q === [?x, ?y]
    }
};
// => [["tom", "bob"], ["bob", "ann"]]
```

## Concepts

Logic programming works by **searching** for values that satisfy constraints. Instead of computing a result step by step, you describe relationships and ask the system to find all values that fit.

**Logic variables** (`?x`, `?q`) are unknowns. **Unification** (`===`) constrains them. **Goals** are constraints that either succeed or fail. The runtime explores all possible solutions.

## Logic Variables

Prefix an identifier with `?` to create a logic variable:

```
?x      // logic variable x
?name   // logic variable name
?q      // conventionally used as the query variable in run
```

Logic variables are scoped to their enclosing `run` or `fresh` block. Outside of logic contexts, `?x` is just a regular variable named `x`.

## Unification: `===`

The `===` operator unifies two values. It succeeds if the values can be made equal by binding logic variables:

```
run(1, ?q) { ?q === 5 };           // => [5]
run(1, ?q) { ?q === "hello" };     // => ["hello"]
run(1, ?q) { ?q === [1, 2, 3] };   // => [[1, 2, 3]]
```

Unification is bidirectional and works on nested structures:

```
run(1, ?q) {
    fresh(?x, ?y) {
        [?x, ?y] === [1, 2],
        ?q === [?y, ?x]
    }
};
// => [[2, 1]]
```

`===` is distinct from `==` (equality test). `==` returns a boolean; `===` is a logic goal that constrains variables.

## `run` — Search for Solutions

`run(n, ?q) { goals }` searches for up to `n` values of `?q` that satisfy all goals:

```
// Find 1 solution
run(1, ?q) { ?q === 42 };          // => [42]

// Find all solutions (use * for unlimited)
run(*, ?q) { ?q === 42 };          // => [42]

// Multiple goals are conjunctive (all must hold)
run(1, ?q) {
    fresh(?x) {
        ?x === 10,
        ?q === ?x
    }
};
// => [10]
```

The first argument controls how many solutions to collect:
- A number: `run(3, ?q) { ... }` returns at most 3
- `*`: `run(*, ?q) { ... }` returns all solutions

## `fresh` — Introduce New Variables

`fresh(?x, ?y, ...) { goals }` introduces fresh logic variables:

```
run(1, ?q) {
    fresh(?a, ?b) {
        ?a === "hello",
        ?b === "world",
        ?q === [?a, ?b]
    }
};
// => [["hello", "world"]]
```

Fresh variables are independent — each `fresh` creates new unknowns that are only constrained by the goals inside.

Nesting works:

```
run(1, ?q) {
    fresh(?a, ?b, ?c, ?d) {
        ?a === 1, ?b === 2, ?c === 3, ?d === 4,
        ?q === [[?a, ?b], [?c, ?d]]
    }
};
// => [[[1, 2], [3, 4]]]
```

## `conde` — Disjunction (OR)

`conde` tries multiple alternatives. Each clause is a set of goals in braces:

```
run(*, ?q) conde {
    { ?q === 1 },
    { ?q === 2 },
    { ?q === 3 }
};
// => [1, 2, 3]
```

Each clause is independent — variables bound in one clause don't affect others:

```
run(*, ?q) conde {
    { fresh(?x) { ?x === 10, ?q === ?x } },
    { fresh(?x) { ?x === 20, ?q === ?x } }
};
// => [10, 20]
```

`conde` uses interleaving search, so it handles infinite branches fairly.

## Limiting Results

Pass a number to `run` to limit how many solutions are returned:

```
run(2, ?q) conde {
    { ?q === 1 },
    { ?q === 2 },
    { ?q === 3 },
    { ?q === 4 }
};
// => [1, 2]
```

## Facts

Declare ground relations with `fact`:

```
fact parent("tom", "bob");
fact parent("bob", "ann");
fact parent("bob", "pat");
fact parent("tom", "liz");
```

Each `fact` statement does two things:
1. Asserts the fact to the logic database
2. Defines a callable relation function (e.g., `parent` becomes a function that returns a logic goal)

Query facts using `run`:

```
// Who are bob's parents?
run(*, ?q) { parent(?q, "bob") };
// => ["tom"]

// Who are bob's children?
run(*, ?q) { parent("bob", ?q) };
// => ["ann", "pat"]

// All parent-child pairs
run(*, ?q) {
    fresh(?x, ?y) {
        parent(?x, ?y),
        ?q === [?x, ?y]
    }
};
// => [["tom", "bob"], ["bob", "ann"], ["bob", "pat"], ["tom", "liz"]]
```

## Rules

Define derived relations with `rule`. A rule has a head and a body separated by `:-`:

```
rule ancestor(?x, ?y) :- parent(?x, ?y);
```

This says: `?x` is an ancestor of `?y` if `?x` is a parent of `?y`.

### Recursive Rules

Rules can be recursive. Use `fresh` for intermediate variables:

```
rule ancestor(?x, ?y) :- parent(?x, ?y);
rule ancestor(?x, ?y) :- fresh(?z) { parent(?x, ?z), ancestor(?z, ?y) };
```

Now query:

```
// All ancestors of ann
run(*, ?q) { ancestor(?q, "ann") };
// => ["bob", "tom"]

// All descendants of tom
run(*, ?q) { ancestor("tom", ?q) };
// => ["bob", "ann", "pat", "liz"]
```

### Multiple Rules

Multiple `rule` statements for the same relation name add alternatives (like multiple Prolog clauses):

```
rule sibling(?x, ?y) :- fresh(?p) { parent(?p, ?x), parent(?p, ?y) };

run(*, ?q) { sibling("ann", ?q) };
// => ["ann", "pat"]   (includes self — add a guard or !== to exclude)
```

### Body-Only Variables

Variables that appear in the rule body but not in the head must be introduced with `fresh`:

```
// ?z only appears in the body, so wrap in fresh
rule ancestor(?x, ?y) :- fresh(?z) { parent(?x, ?z), ancestor(?z, ?y) };
```

## Mixing Logic and Eval

Logic programming integrates naturally with regular Eval code.

### Using Results in Eval

`run` returns a regular Eval list, so you can use it in any expression:

```
define ancestors = run(*, ?q) { ancestor(?q, "ann") };
length(ancestors);        // => 2
car(ancestors);           // => "bob"

if(length(run(*, ?q) { parent(?q, "bob") }) > 0)
    display("Bob has parents");
```

### Logic Inside Control Flow

```
for(let person in ["ann", "pat", "liz"]) {
    define ancs = run(*, ?q) { ancestor(?q, person) };
    display(f"{person} has {length(ancs)} ancestors");
    newline();
};
```

### Building Facts Dynamically

```
define data = [["alice", "bob"], ["bob", "charlie"]];
for(let pair in data) {
    fact knows(car(pair), cadr(pair));
};

run(*, ?q) { knows("alice", ?q) };
// => ["bob"]
```

## How It Works

### Compilation

Logic constructs compile to Scheme function calls at parse time:

| Eval syntax | Compiles to |
|---|---|
| `?x` | The Scheme variable `x` |
| `a === b` | `(logic_eq a b)` |
| `fresh(?x, ?y) { G }` | `(call-fresh (lambda (x) (call-fresh (lambda (y) G))))` |
| `run(n, ?q) { G }` | `(logic_run n (lambda (q) G))` |
| `conde { {G1}, {G2} }` | `(logic_conde (list G1) (list G2))` |
| `fact name(a, b)` | `(logic_assert_fact 'name a b)` + function binding |
| `rule name(?x) :- G` | `(logic_assert_rule 'name 1 (lambda (x) G))` + function binding |

### Runtime Engine

The logic engine is a complete microKanren implementation in Scheme (`chibi-scheme/lib/eval/logic.scm`):

- **Logic variables**: `#(var id)` vectors with integer IDs
- **Substitution**: Association list mapping variables to values
- **State**: `(substitution . counter)` pair
- **Goals**: Functions from state to stream of states
- **Streams**: Lists with thunks for lazy interleaving (ensures fair search)
- **Unification**: Structural — works on pairs, vectors, and atomic values

### Search Strategy

The engine uses interleaving search (miniKanren-style), which alternates between branches of a disjunction. This means:

- Infinite branches don't starve other branches
- Results arrive in a fair interleaving order
- All reachable solutions will eventually be found

## API Reference

### Syntax

| Form | Description |
|------|-------------|
| `?x` | Logic variable |
| `a === b` | Unification goal |
| `run(n, ?q) { goals }` | Search for `n` solutions (`*` for all) |
| `fresh(?x, ...) { goals }` | Introduce fresh logic variables |
| `conde { {g1}, {g2}, ... }` | Disjunction — try each clause |
| `fact name(args...)` | Assert a ground fact |
| `rule name(?x, ...) :- goals` | Assert a derived rule |

### Runtime Functions

These Scheme functions are available if you need lower-level access:

| Function | Description |
|---|---|
| `logic_eq(a, b)` | Unification goal |
| `call-fresh(f)` | Introduce fresh variable: `f` receives the new var |
| `conj(g1, g2)` | Conjunction of two goals |
| `disj(g1, g2)` | Disjunction of two goals |
| `logic_run(n, f)` | Run query: `n` = count or `#f` for all, `f` = `(lambda (q) goal)` |
| `logic_conde(clause...)` | Each clause is a list of goals |
| `logic_guard(thunk)` | Use an Eval predicate as a goal (thunk returns boolean) |
| `logic_member(x, lst)` | Relational membership |
| `logic_assert_fact(name, args...)` | Assert fact to database |
| `logic_assert_rule(name, arity, body-fn)` | Assert rule to database |
| `logic_query_rel(name, args)` | Query relation by name |
| `logic_retract(name)` | Remove all entries for a relation |
| `logic_clear_db()` | Clear the entire logic database |

## Examples

### Family Tree

```
fact parent("tom", "bob");
fact parent("tom", "liz");
fact parent("bob", "ann");
fact parent("bob", "pat");

rule grandparent(?x, ?y) :- fresh(?z) { parent(?x, ?z), parent(?z, ?y) };
rule sibling(?x, ?y) :- fresh(?p) { parent(?p, ?x), parent(?p, ?y) };
rule ancestor(?x, ?y) :- parent(?x, ?y);
rule ancestor(?x, ?y) :- fresh(?z) { parent(?x, ?z), ancestor(?z, ?y) };

// Grandchildren of tom
run(*, ?q) { grandparent("tom", ?q) };
// => ["ann", "pat"]

// Siblings of ann
run(*, ?q) { sibling("ann", ?q) };
// => ["ann", "pat"]

// Full ancestor chain for ann
run(*, ?q) { ancestor(?q, "ann") };
// => ["bob", "tom"]
```

### Type Checking

```
fact typeof("x", "int");
fact typeof("y", "string");
fact typeof("z", "int");

// Find all int-typed variables
run(*, ?q) {
    fresh(?t) {
        typeof(?q, "int")
    }
};
// => ["x", "z"]
```

### Graph Reachability

```
fact edge("a", "b");
fact edge("b", "c");
fact edge("c", "d");
fact edge("a", "d");

rule path(?x, ?y) :- edge(?x, ?y);
rule path(?x, ?y) :- fresh(?z) { edge(?x, ?z), path(?z, ?y) };

// All nodes reachable from "a"
run(*, ?q) { path("a", ?q) };
// => ["b", "d", "c", "d", "c", "d"]
// (includes duplicates from multiple paths — filter with Eval if needed)
```

### Combining Logic with Computation

```
// Generate candidates with logic, filter with Eval
define candidates = run(*, ?q) conde {
    { ?q === 1 },
    { ?q === 2 },
    { ?q === 3 },
    { ?q === 4 },
    { ?q === 5 }
};
filter(function(x) x > 3, candidates);
// => [4, 5]
```

## See Also

- [RETE.md](RETE.md) — Forward-chaining rules: `whenever`, Rete algorithm, joins, chaining
- [AMB.md](AMB.md) — Nondeterministic choice: amb, require, backtracking search
- [INTRO.md](INTRO.md) — Complete language reference
- [README.md](README.md) — Project overview and quick start
- [TESTS.md](TESTS.md) — Built-in test framework
