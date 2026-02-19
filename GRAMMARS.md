# Grammar JIT: Runtime Parser Generation

Eval includes a built-in system for defining grammars at runtime and compiling
them to native parsers. You write a grammar in Lark EBNF notation, and the
system compiles it through a multi-stage pipeline entirely in memory:

```
Lark EBNF string
    --> Lemon LALR(1) parser tables
    --> re2c DFA lexer
    --> Combined C source
    --> LLVM JIT --> native machine code
```

The result is a parser that runs at native speed with no external tools,
no temporary files, and no interpreter overhead.

## Quick Start

```javascript
// Define a grammar using Lark EBNF (triple-quoted strings avoid escaping)
define g = Grammar("""
    start: expr
    ?expr: expr "+" term -> add
         | expr "-" term -> sub
         | term
    ?term: NUMBER
    NUMBER: /[0-9]+/
    %ignore /\s+/
""");

// Compile to native parser (one-time cost ~200ms)
define p = g->compile();

// Parse input strings (microseconds)
display(p->parse("1 + 2 + 3"));
// ("add" ("add" ("NUMBER" "1") ("ANON_0" "+") ("NUMBER" "2")) ("ANON_0" "+") ("NUMBER" "3"))
```

## API

### `Grammar(text)`

Creates a grammar object from a Lark EBNF string. Returns an object with
one method:

- `g->compile()` -- compile the grammar to a native parser

### `parser->parse(input)`

Parses an input string using the compiled grammar. Returns a nested list
representing the AST:

- **Token nodes** (leaves): `("TOKEN_NAME" "matched text")`
- **Rule nodes** (internal): `("rule_name" child1 child2 ...)`

If parsing fails, an error is raised with a description of the problem.

## AST Structure

The parse result is a nested list. Tokens are two-element lists (name + text),
rules are lists starting with the rule name followed by children:

```javascript
define g = Grammar("""
    start: assignment
    assignment: IDENT "=" NUMBER
    IDENT: /[a-zA-Z_][a-zA-Z0-9_]*/
    NUMBER: /[0-9]+/
    %ignore /\s+/
""");
define p = g->compile();
display(p->parse("x = 42"));
// ("start" ("assignment" ("IDENT" "x") ("ANON_0" "=") ("NUMBER" "42")))
```

String literals like `"="` that appear directly in rules are assigned
automatic token names (`ANON_0`, `ANON_1`, ...) and appear in the AST
with their matched text.

## Lark EBNF Grammar Format

The grammar format is based on [Lark](https://github.com/lark-parser/lark),
a Python parsing library. This implementation supports the core subset
described below.

### Rules

Rules have lowercase names and define the grammar structure. The first rule
is the start rule.

```
start: item_list
item_list: item
         | item_list "," item
item: NUMBER
```

Alternatives are separated by `|`. Continuation lines starting with `|` are
treated as additional alternatives for the previous rule.

### Terminals

Terminals have UPPERCASE names and define the tokens (lexer patterns).

**Regex terminals** use `/pattern/` syntax:

```
NUMBER: /[0-9]+/
IDENT:  /[a-zA-Z_][a-zA-Z0-9_]*/
STRING: /"[^"]*"/
```

**String terminals** use quoted strings:

```
PLUS: "+"
ARROW: "->"
KEYWORD_IF: "if"
```

Terminals can have multiple alternatives separated by `|`:

```
VALUE: /[a-z]+/ | /[0-9]+/
KEYWORD: "if" | "else" | "while"
TOKEN: "null" | /[0-9]+/ | /[a-z]+/
```

Terminals are matched by the lexer before parsing begins. Regex terminals
support standard regex syntax including character classes (`[a-z]`),
quantifiers (`+`, `*`, `?`), alternation (`a|b`), and the shorthands
`\d` (digits), `\w` (word chars), and `\s` (whitespace).

### Anonymous Literals

String literals that appear directly in rules (without being named as
terminals) are treated as anonymous tokens:

```
start: expr
?expr: expr "+" term -> add    // "+" is an anonymous literal
     | term
```

These are assigned names like `ANON_0`, `ANON_1`, etc. and appear in the
AST with their matched text.

### Aliases (`->`)

Aliases rename AST nodes for clarity:

```
?expr: expr "+" term -> add
     | expr "-" term -> sub
     | term
```

Without aliases, all alternatives would produce nodes named `expr`. With
aliases, you get `add` and `sub` nodes instead, making the AST easier to
process.

### Inline Rules (`?`)

Rules prefixed with `?` are inline rules. When an inline rule has a single
alternative that matches, its child is passed through directly instead of
being wrapped in an extra node:

```
?expr: expr "+" term -> add
     | term                    // single item: passes NUMBER through directly
?term: NUMBER
```

Parsing `"42"` produces `("NUMBER" "42")` rather than
`("expr" ("term" ("NUMBER" "42")))`.

### EBNF Modifiers

Items in rules can have quantifier modifiers:

| Modifier | Meaning       | Example         |
|----------|---------------|-----------------|
| `?`      | Zero or one   | `item?`         |
| `*`      | Zero or more  | `item*`         |
| `+`      | One or more   | `item+`         |

```
start: items
items: item+
item: NUMBER "," | NUMBER
```

These are desugared into helper rules internally (e.g., `item+` becomes
a left-recursive helper rule).

### `%ignore` Directive

The `%ignore` directive tells the lexer to skip patterns (typically
whitespace):

```
%ignore /\s+/           // skip whitespace
%ignore /\/\/.*/        // skip // comments
```

Without `%ignore`, every space in the input must be explicitly matched
by the grammar.

### Terminal Priority

Terminals can have numeric priority to resolve ambiguity:

```
KEYWORD: "if"
IDENT.1: /[a-zA-Z_]+/     // lower priority than KEYWORD
```

Higher-priority terminals are matched first. String literals always have
higher priority than regex patterns by default.

### Comments

Both `//` and `#` start line comments in the grammar:

```
start: expr    // this is a comment
# this is also a comment
?expr: NUMBER
```

## Examples

### Calculator

```javascript
define calc = Grammar("""
    start: expr
    ?expr: expr "+" term -> add
         | expr "-" term -> sub
         | term
    ?term: term "*" factor -> mul
         | term "/" factor -> div
         | factor
    ?factor: NUMBER
           | "(" expr ")"
    NUMBER: /[0-9]+/
    %ignore /\s+/
""");
define p = calc->compile();

display(p->parse("2 * (3 + 4)"));
// ("mul" ("NUMBER" "2") ("ANON_1" "*") ("add" ("NUMBER" "3") ("ANON_0" "+") ("NUMBER" "4")))
```

### Assignment Language

```javascript
define lang = Grammar("""
    start: statement
    statement: IDENT "=" expr
    ?expr: expr OP term -> binop
         | term
    ?term: NUMBER
         | IDENT
    IDENT: /[a-zA-Z_][a-zA-Z0-9_]*/
    NUMBER: /[0-9]+/
    OP: /[+\-*/]/
    %ignore /\s+/
""");
define p = lang->compile();

display(p->parse("result = x + 1"));
// ("start" ("statement" ("IDENT" "result") ("ANON_0" "=")
//     ("binop" ("IDENT" "x") ("OP" "+") ("NUMBER" "1"))))
```

### CSV-like Lists

```javascript
define csv = Grammar("""
    start: list
    list: item
        | list "," item
    item: NUMBER
    NUMBER: /[0-9]+/
    %ignore /\s+/
""");
define p = csv->compile();

display(p->parse("1, 2, 3"));
// ("start" ("list" ("list" ("item" ("NUMBER" "1")) ...) ...))
```

## Compilation Pipeline

Each stage runs fully in memory with no temporary files:

1. **Grammar Parsing** -- The Lark EBNF string is parsed into a
   `LarkGrammar` structure containing rules, terminals, and directives.

2. **Lemon Code Generation** -- The grammar is translated to a Lemon `.y`
   parser specification. EBNF modifiers (`?`, `*`, `+`) are desugared into
   helper rules. Reduce actions build AST nodes using an arena allocator.

3. **Lemon Compilation** -- The `.y` source is compiled to C parser tables
   by an embedded Lemon library (modified to use in-memory buffers instead
   of files).

4. **re2c Code Generation** -- Terminal patterns are translated to a re2c
   `.re` lexer specification with `\d`/`\w`/`\s` shorthand expansion and
   proper priority ordering (ignore patterns, then string literals, then
   regex terminals).

5. **re2c Compilation** -- The `.re` source is compiled to a DFA-based
   C lexer by an embedded re2c library (modified for in-memory I/O).

6. **Combined C Source** -- The parser tables, lexer, and a parse entry
   point are combined into a single self-contained C source file.

7. **LLVM JIT** -- Clang compiles the C source to LLVM IR, which is then
   JIT-compiled to native machine code. The resulting function pointer is
   called directly for parsing.

## Limitations

This implementation supports the core Lark grammar features. The following
are **not** supported:

- `%import` / `%declare` directives
- Tree transformers or visitor patterns
- Lookahead/lookbehind in regex (`(?=...)`, `(?<=...)`)
- Template rules or parameterized rules
- `@` decorators (`@v_args`, `@inline`)
- The `~` repetition operator (`item~3` for exact count)
- Earley or CYK parsing (only LALR(1))

The parser is LALR(1), which means it cannot handle all context-free
grammars. Ambiguous or non-LALR grammars will produce conflict errors
during compilation. If you get shift/reduce or reduce/reduce conflicts,
restructure your grammar to be unambiguous.
