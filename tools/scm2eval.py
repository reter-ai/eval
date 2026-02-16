#!/usr/bin/env python3
"""Scheme → Eval transpiler.

Converts .scm/.sld files into .eval/.evld Eval-syntax equivalents.

Usage:
    python tools/scm2eval.py FILE.scm                  # single file → stdout
    python tools/scm2eval.py FILE.scm -o FILE.eval      # single file → file
    python tools/scm2eval.py --dir chibi-scheme/lib/scheme/ -o chibi_eval/_lib/scheme/
"""

from __future__ import annotations
import argparse
import os
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Union


# ---------------------------------------------------------------------------
# S-expression reader
# ---------------------------------------------------------------------------

class ReaderError(Exception):
    pass


@dataclass
class Symbol:
    name: str
    def __repr__(self):
        return f"Symbol({self.name!r})"
    def __eq__(self, other):
        if isinstance(other, Symbol):
            return self.name == other.name
        if isinstance(other, str):
            return self.name == other
        return NotImplemented
    def __hash__(self):
        return hash(self.name)


class Dot:
    """Sentinel for dotted pair notation."""
    pass

DOT = Dot()


def read_all(text: str) -> list:
    """Read all s-expressions from text."""
    reader = SexpReader(text)
    results = []
    while True:
        reader.skip_whitespace_and_comments()
        if reader.at_end():
            break
        results.append(reader.read())
    return results


class SexpReader:
    def __init__(self, text: str):
        self.text = text
        self.pos = 0

    def at_end(self) -> bool:
        return self.pos >= len(self.text)

    def peek(self) -> str:
        if self.at_end():
            return ''
        return self.text[self.pos]

    def advance(self) -> str:
        ch = self.text[self.pos]
        self.pos += 1
        return ch

    def skip_whitespace_and_comments(self):
        while not self.at_end():
            ch = self.peek()
            if ch in ' \t\n\r':
                self.advance()
            elif ch == ';':
                # line comment
                while not self.at_end() and self.peek() != '\n':
                    self.advance()
            elif self.text[self.pos:self.pos+2] == '#|':
                # block comment
                self.pos += 2
                depth = 1
                while depth > 0 and not self.at_end():
                    if self.text[self.pos:self.pos+2] == '#|':
                        depth += 1
                        self.pos += 2
                    elif self.text[self.pos:self.pos+2] == '|#':
                        depth -= 1
                        self.pos += 2
                    else:
                        self.pos += 1
            elif self.text[self.pos:self.pos+2] == '#;':
                # datum comment — skip next datum
                self.pos += 2
                self.skip_whitespace_and_comments()
                self.read()  # read and discard
            else:
                break

    def read(self):
        self.skip_whitespace_and_comments()
        if self.at_end():
            raise ReaderError("unexpected end of input")

        ch = self.peek()

        if ch == '(':
            return self.read_list()
        elif ch == "'":
            self.advance()
            return [Symbol("quote"), self.read()]
        elif ch == '`':
            self.advance()
            return [Symbol("quasiquote"), self.read()]
        elif ch == ',':
            self.advance()
            if not self.at_end() and self.peek() == '@':
                self.advance()
                return [Symbol("unquote-splicing"), self.read()]
            return [Symbol("unquote"), self.read()]
        elif ch == '"':
            return self.read_string()
        elif ch == '#':
            return self.read_hash()
        else:
            return self.read_atom()

    def read_list(self):
        self.advance()  # consume '('
        items = []
        dot_seen = False
        cdr_value = None

        while True:
            self.skip_whitespace_and_comments()
            if self.at_end():
                raise ReaderError("unterminated list")
            if self.peek() == ')':
                self.advance()
                if dot_seen:
                    # Build dotted pair
                    result = cdr_value
                    for item in reversed(items):
                        result = [Symbol(".cons"), item, result]
                    return DottedList(items, cdr_value)
                return items

            item = self.read()
            if isinstance(item, Dot):
                dot_seen = True
                cdr_value = self.read()
            else:
                items.append(item)

    def read_string(self):
        self.advance()  # consume opening "
        result = []
        while not self.at_end():
            ch = self.advance()
            if ch == '"':
                return ''.join(result)
            elif ch == '\\':
                if self.at_end():
                    raise ReaderError("unterminated string escape")
                esc = self.advance()
                escapes = {'n': '\n', 't': '\t', 'r': '\r', '\\': '\\',
                           '"': '"', '0': '\0', 'a': '\a', 'b': '\b'}
                result.append(escapes.get(esc, esc))
            else:
                result.append(ch)
        raise ReaderError("unterminated string")

    def read_hash(self):
        self.advance()  # consume '#'
        if self.at_end():
            raise ReaderError("unexpected # at end")
        ch = self.peek()
        if ch == 't':
            self.advance()
            # Accept #t or #true
            if not self.at_end() and self.text[self.pos:self.pos+3] == 'rue':
                self.pos += 3
            return True
        elif ch == 'f':
            self.advance()
            # Accept #f or #false
            if not self.at_end() and self.text[self.pos:self.pos+4] == 'alse':
                self.pos += 4
            return False
        elif ch == '\\':
            # Character literal
            self.advance()
            if self.at_end():
                raise ReaderError("unexpected end in char literal")
            # Check for named characters
            start = self.pos
            if self.peek().isalpha():
                while not self.at_end() and self.peek().isalpha():
                    self.advance()
                name = self.text[start:self.pos]
                char_names = {
                    'space': ' ', 'newline': '\n', 'tab': '\t',
                    'return': '\r', 'nul': '\0', 'null': '\0',
                    'alarm': '\a', 'backspace': '\b', 'delete': '\x7f',
                    'escape': '\x1b',
                }
                if len(name) == 1:
                    return CharLiteral(name)
                return CharLiteral(char_names.get(name, name[0]))
            return CharLiteral(self.advance())
        elif ch == '(':
            # Vector literal #(...)
            lst = self.read_list()
            return VectorLiteral(lst if isinstance(lst, list) else lst.items)
        elif ch == 'u':
            # #u8(...) bytevector
            self.advance()  # u
            if not self.at_end() and self.peek() == '8':
                self.advance()  # 8
                lst = self.read_list()
                return BytevectorLiteral(lst if isinstance(lst, list) else [])
            return Symbol("#u")
        elif ch == ';':
            # datum comment handled in skip
            self.advance()
            self.skip_whitespace_and_comments()
            self.read()  # discard
            return self.read()
        elif ch == '|':
            # block comment handled in skip
            self.pos -= 1  # back to #
            self.skip_whitespace_and_comments()
            return self.read()
        else:
            # Unknown # syntax, read as symbol
            start = self.pos - 1  # include the #
            while not self.at_end() and self.peek() not in ' \t\n\r()':
                self.advance()
            return Symbol(self.text[start:self.pos])

    def read_atom(self):
        start = self.pos
        while not self.at_end() and self.peek() not in ' \t\n\r()";,':
            self.advance()
        token = self.text[start:self.pos]

        if not token:
            raise ReaderError(f"unexpected character: {self.peek()!r}")

        if token == '.':
            return DOT

        # Try numbers
        try:
            return int(token)
        except ValueError:
            pass
        try:
            return float(token)
        except ValueError:
            pass

        return Symbol(token)


@dataclass
class DottedList:
    items: list
    cdr: object


@dataclass
class CharLiteral:
    char: str


@dataclass
class VectorLiteral:
    items: list


@dataclass
class BytevectorLiteral:
    items: list


# ---------------------------------------------------------------------------
# Eval emitter
# ---------------------------------------------------------------------------

class EvalEmitter:
    """Converts parsed S-expressions into Eval syntax strings."""

    def __init__(self, indent_width: int = 2):
        self.indent_width = indent_width

    def emit(self, expr, indent: int = 0) -> str:
        """Convert a single s-expression to Eval syntax."""
        if isinstance(expr, list):
            return self.emit_form(expr, indent)
        elif isinstance(expr, DottedList):
            return self.emit_dotted(expr, indent)
        elif isinstance(expr, str):
            return self.emit_string(expr)
        elif isinstance(expr, bool):
            return "true" if expr else "false"
        elif isinstance(expr, int):
            return str(expr)
        elif isinstance(expr, float):
            return repr(expr)
        elif isinstance(expr, Symbol):
            return self.emit_symbol(expr.name)
        elif isinstance(expr, CharLiteral):
            return self.emit_char(expr.char)
        elif isinstance(expr, VectorLiteral):
            items = ", ".join(self.emit(x, indent) for x in expr.items)
            return f"#[{items}]"
        elif isinstance(expr, BytevectorLiteral):
            items = ", ".join(str(x) for x in expr.items)
            return f"bytes[{items}]"
        else:
            return repr(expr)

    def emit_string(self, s: str) -> str:
        escaped = s.replace("\\", "\\\\").replace('"', '\\"')
        escaped = escaped.replace("\n", "\\n").replace("\t", "\\t")
        escaped = escaped.replace("\r", "\\r")
        return f'"{escaped}"'

    def emit_char(self, ch: str) -> str:
        if ch == '\n':
            return r"'\n'"
        if ch == '\t':
            return r"'\t'"
        if ch == ' ':
            return r"' '"
        return f"'{ch}'"

    def _needs_backtick(self, name: str) -> bool:
        """Check if a Scheme identifier needs backtick quoting in Eval."""
        # Operators and special tokens are fine as-is
        preserve = {
            '+', '-', '*', '/', '<', '>', '<=', '>=', '=',
            '...', '=>', '_', '.',
        }
        if name in preserve:
            return False
        # If it contains characters that conflict with Eval operators,
        # it needs backtick quoting
        return '-' in name or '/' in name

    def emit_symbol(self, name: str) -> str:
        if self._needs_backtick(name):
            return f'`{name}`'
        return name

    def emit_dotted(self, dl: DottedList, indent: int) -> str:
        items = ", ".join(self.emit(x, indent) for x in dl.items)
        cdr = self.emit(dl.cdr, indent)
        return f"({items} .. {cdr})"

    def emit_form(self, form: list, indent: int) -> str:
        if not form:
            return "[]"

        head = form[0]

        # Check for known forms
        if isinstance(head, Symbol):
            name = head.name
            handler = self._form_handlers.get(name)
            if handler:
                return handler(self, form, indent)

        # Default: function call
        return self.emit_call(form, indent)

    def emit_call(self, form: list, indent: int) -> str:
        """Default: emit as function call f(arg1, arg2, ...)"""
        func = self.emit(form[0], indent)
        args = ", ".join(self.emit(x, indent) for x in form[1:])
        return f"{func}({args})"

    # --- Form handlers ---

    def emit_define(self, form: list, indent: int) -> str:
        """(define name expr) or (define (name params...) body)"""
        if len(form) < 3:
            return self.emit_call(form, indent)

        target = form[1]
        if isinstance(target, list) and len(target) >= 1:
            # (define (name params...) body...)
            name = self.emit(target[0], indent)
            params = self.emit_params(target[1:], indent)
            body = self.emit_body(form[2:], indent)
            return f"{name} := function({params}) {body}"
        elif isinstance(target, DottedList):
            # (define (name a b . rest) body...)
            name = self.emit(target.items[0], indent)
            params = self.emit_params_dotted(target.items[1:], target.cdr, indent)
            body = self.emit_body(form[2:], indent)
            return f"{name} := function({params}) {body}"
        else:
            # (define name expr)
            name = self.emit(target, indent)
            val = self.emit(form[2], indent)
            return f"{name} := {val}"

    def emit_set(self, form: list, indent: int) -> str:
        """(set! name expr)"""
        if len(form) != 3:
            return self.emit_call(form, indent)
        name = self.emit(form[1], indent)
        val = self.emit(form[2], indent)
        return f"{name} = {val}"

    def emit_lambda(self, form: list, indent: int) -> str:
        """(lambda (params...) body...)"""
        if len(form) < 3:
            return self.emit_call(form, indent)

        params_form = form[1]
        if isinstance(params_form, DottedList):
            params = self.emit_params_dotted(params_form.items, params_form.cdr, indent)
        elif isinstance(params_form, list):
            params = self.emit_params(params_form, indent)
        elif isinstance(params_form, Symbol):
            # (lambda args body) — rest args
            params = f".. {self.emit_symbol(params_form.name)}"
        else:
            params = self.emit(params_form, indent)

        body = self.emit_body(form[2:], indent)
        return f"function({params}) {body}"

    def emit_if(self, form: list, indent: int) -> str:
        """(if cond then else?)"""
        if len(form) == 4:
            cond = self.emit(form[1], indent)
            then = self.emit(form[2], indent)
            els = self.emit(form[3], indent)
            return f"if({cond}) {then} else {els}"
        elif len(form) == 3:
            cond = self.emit(form[1], indent)
            then = self.emit(form[2], indent)
            return f"if({cond}) {then}"
        return self.emit_call(form, indent)

    def emit_cond(self, form: list, indent: int) -> str:
        """(cond (test expr)... (else expr)?)"""
        clauses = []
        for clause in form[1:]:
            if isinstance(clause, list) and len(clause) >= 2:
                if isinstance(clause[0], Symbol) and clause[0].name == 'else':
                    body = self.emit_body(clause[1:], indent)
                    clauses.append(f"else: {body}")
                else:
                    test = self.emit(clause[0], indent)
                    body = self.emit_body(clause[1:], indent)
                    clauses.append(f"{test}: {body}")
        inner = ", ".join(clauses)
        return f"cond({inner})"

    def emit_case(self, form: list, indent: int) -> str:
        """(case key ((datum...) expr)... (else expr)?)"""
        if len(form) < 2:
            return self.emit_call(form, indent)
        key = self.emit(form[1], indent)
        clauses = []
        for clause in form[2:]:
            if isinstance(clause, list) and len(clause) >= 2:
                if isinstance(clause[0], Symbol) and clause[0].name == 'else':
                    body = self.emit_body(clause[1:], indent)
                    clauses.append(f"else: {body}")
                elif isinstance(clause[0], list):
                    datums = ", ".join(self.emit(d, indent) for d in clause[0])
                    body = self.emit_body(clause[1:], indent)
                    clauses.append(f"({datums}): {body}")
        inner = ", ".join(clauses)
        return f"case({key}, {inner})"

    def emit_let(self, form: list, indent: int, kind: str = "let") -> str:
        """(let ((name val)...) body...) or named let"""
        if len(form) < 3:
            return self.emit_call(form, indent)

        bindings_form = form[1]

        # Named let: (let name ((var init)...) body...)
        if isinstance(bindings_form, Symbol):
            if len(form) < 4:
                return self.emit_call(form, indent)
            name = self.emit_symbol(bindings_form.name)
            bindings = form[2]
            body = self.emit_body(form[3:], indent)
            bind_strs = []
            for b in bindings:
                if isinstance(b, list) and len(b) == 2:
                    bname = self.emit(b[0], indent)
                    bval = self.emit(b[1], indent)
                    bind_strs.append(f"{bname} := {bval}")
            binds = ", ".join(bind_strs)
            # Named let maps to letrec with function
            params = ", ".join(self.emit(b[0], indent) for b in bindings if isinstance(b, list))
            inits = ", ".join(self.emit(b[1], indent) for b in bindings if isinstance(b, list))
            return f"letrec({name} := function({params}) {body}) {name}({inits})"

        if not isinstance(bindings_form, list):
            return self.emit_call(form, indent)

        bind_strs = []
        for b in bindings_form:
            if isinstance(b, list) and len(b) == 2:
                bname = self.emit(b[0], indent)
                bval = self.emit(b[1], indent)
                bind_strs.append(f"{bname} := {bval}")
        binds = ", ".join(bind_strs)
        body = self.emit_body(form[2:], indent)

        eval_kind = {"let": "let", "let*": "let*", "letrec": "letrec",
                      "letrec*": "letrec"}
        kw = eval_kind.get(kind, kind)
        return f"{kw}({binds}) {body}"

    def emit_begin(self, form: list, indent: int) -> str:
        """(begin expr...)"""
        if len(form) == 1:
            return "{}"
        if len(form) == 2:
            return self.emit(form[1], indent)
        ni = indent + self.indent_width
        pad = " " * ni
        stmts = []
        for expr in form[1:]:
            stmts.append(f"{pad}{self.emit(expr, ni)};")
        inner = "\n".join(stmts)
        return "{\n" + inner + "\n" + " " * indent + "}"

    def emit_and(self, form: list, indent: int) -> str:
        if len(form) == 1:
            return "true"
        if len(form) == 2:
            return self.emit(form[1], indent)
        parts = [self.emit(x, indent) for x in form[1:]]
        return " && ".join(f"({p})" if " " in p else p for p in parts)

    def emit_or(self, form: list, indent: int) -> str:
        if len(form) == 1:
            return "false"
        if len(form) == 2:
            return self.emit(form[1], indent)
        parts = [self.emit(x, indent) for x in form[1:]]
        return " || ".join(f"({p})" if " " in p else p for p in parts)

    def emit_not(self, form: list, indent: int) -> str:
        if len(form) != 2:
            return self.emit_call(form, indent)
        inner = self.emit(form[1], indent)
        return f"!({inner})" if " " in inner else f"!{inner}"

    def emit_when(self, form: list, indent: int) -> str:
        cond = self.emit(form[1], indent)
        body = self.emit_body(form[2:], indent)
        return f"when({cond}) {body}"

    def emit_unless(self, form: list, indent: int) -> str:
        cond = self.emit(form[1], indent)
        body = self.emit_body(form[2:], indent)
        return f"unless({cond}) {body}"

    def emit_do(self, form: list, indent: int) -> str:
        """(do ((var init step)...) (test expr...) body...)
        → for(var := init, ..., test, var = step, ...) { body; }
        This is an approximation — Scheme's do is more general."""
        if len(form) < 3:
            return self.emit_call(form, indent)
        bindings = form[1]
        test_form = form[2]
        body = form[3:]

        # Emit as a while loop with init
        inits = []
        steps = []
        for b in bindings:
            if isinstance(b, list) and len(b) >= 2:
                var = self.emit(b[0], indent)
                init = self.emit(b[1], indent)
                inits.append(f"{var} := {init}")
                if len(b) >= 3:
                    step = self.emit(b[2], indent)
                    steps.append(f"{var} = {step}")

        test = self.emit(test_form[0], indent) if isinstance(test_form, list) and test_form else "false"
        result_exprs = test_form[1:] if isinstance(test_form, list) and len(test_form) > 1 else []

        ni = indent + self.indent_width
        pad = " " * ni
        lines = []
        for init in inits:
            lines.append(f"{init};")
        # while(!test) { body; steps; }
        body_strs = [f"{pad}{self.emit(b, ni)};" for b in body]
        step_strs = [f"{pad}{s};" for s in steps]
        all_body = "\n".join(body_strs + step_strs)
        lines.append(f"while(!({test})) {{\n{all_body}\n{' ' * indent}}};")
        if result_exprs:
            lines.append(self.emit_body(result_exprs, indent) + ";")
        return "\n".join(f"{' ' * indent}{l}" for l in lines)

    def emit_quote(self, form: list, indent: int) -> str:
        if len(form) != 2:
            return self.emit_call(form, indent)
        return f"'{self.emit_quoted(form[1])}"

    def emit_quoted(self, expr) -> str:
        """Emit a quoted datum."""
        if isinstance(expr, Symbol):
            if self._needs_backtick(expr.name):
                return f'`{expr.name}`'
            return expr.name
        if isinstance(expr, list):
            items = ", ".join(self.emit_quoted(x) for x in expr)
            return f"[{items}]"
        if isinstance(expr, str):
            return self.emit_string(expr)
        if isinstance(expr, bool):
            return "true" if expr else "false"
        if isinstance(expr, (int, float)):
            return str(expr)
        return repr(expr)

    def emit_quasiquote(self, form: list, indent: int) -> str:
        """(quasiquote expr) — best-effort, complex cases emit raw."""
        if len(form) != 2:
            return self.emit_call(form, indent)
        return self.emit_qq(form[1], indent)

    def emit_qq(self, expr, indent: int) -> str:
        if isinstance(expr, list):
            # Check for unquote/unquote-splicing
            if len(expr) == 2 and isinstance(expr[0], Symbol):
                if expr[0].name == 'unquote':
                    return self.emit(expr[1], indent)
                if expr[0].name == 'unquote-splicing':
                    return f"@{self.emit(expr[1], indent)}"
            # Reconstruct as list with possible splices
            parts = []
            for item in expr:
                if isinstance(item, list) and len(item) == 2 and isinstance(item[0], Symbol):
                    if item[0].name == 'unquote':
                        parts.append(self.emit(item[1], indent))
                        continue
                    if item[0].name == 'unquote-splicing':
                        parts.append(f"..{self.emit(item[1], indent)}")
                        continue
                parts.append(self.emit_qq(item, indent))
            return "[" + ", ".join(parts) + "]"
        return self.emit(expr, indent)

    def emit_values(self, form: list, indent: int) -> str:
        args = ", ".join(self.emit(x, indent) for x in form[1:])
        return f"values({args})"

    def emit_call_with_values(self, form: list, indent: int) -> str:
        """(call-with-values producer consumer)"""
        if len(form) != 3:
            return self.emit_call(form, indent)
        producer = form[1]
        consumer = form[2]
        # If producer is (lambda () expr) and consumer is (lambda (x y) body)
        if (isinstance(producer, list) and len(producer) >= 3 and
            isinstance(producer[0], Symbol) and producer[0].name == 'lambda' and
            isinstance(producer[1], list) and len(producer[1]) == 0):
            expr = self.emit_body(producer[2:], indent)
            if (isinstance(consumer, list) and len(consumer) >= 3 and
                isinstance(consumer[0], Symbol) and consumer[0].name == 'lambda'):
                params = self.emit_params(consumer[1] if isinstance(consumer[1], list) else [], indent)
                body = self.emit_body(consumer[2:], indent)
                return f"receive({params}) from {expr} {body}"
        # Fallback
        return self.emit_call(form, indent)

    def emit_guard(self, form: list, indent: int) -> str:
        """(guard (var clause...) body...)"""
        if len(form) < 3:
            return self.emit_call(form, indent)
        clauses_form = form[1]
        if not isinstance(clauses_form, list) or len(clauses_form) < 2:
            return self.emit_call(form, indent)
        var = self.emit(clauses_form[0], indent)
        body = self.emit_body(form[2:], indent)

        # Simple case: (guard (e (#t handler)) body) → try body catch(e) handler
        if len(clauses_form) == 2:
            clause = clauses_form[1]
            if isinstance(clause, list) and len(clause) == 2:
                handler = self.emit(clause[1], indent)
                return f"try {body} catch({var}) {handler}"

        # Multi-clause: try body catch(e, test1: h1, test2: h2)
        clause_strs = []
        for clause in clauses_form[1:]:
            if isinstance(clause, list) and len(clause) >= 2:
                if isinstance(clause[0], Symbol) and clause[0].name == 'else':
                    h = self.emit_body(clause[1:], indent)
                    clause_strs.append(f"else: {h}")
                elif clause[0] is True:
                    h = self.emit_body(clause[1:], indent)
                    clause_strs.append(f"true: {h}")
                else:
                    test = self.emit(clause[0], indent)
                    h = self.emit_body(clause[1:], indent)
                    clause_strs.append(f"{test}: {h}")
        inner = ", ".join(clause_strs)
        return f"try {body} catch({var}, {inner})"

    def emit_with_exception_handler(self, form: list, indent: int) -> str:
        return self.emit_call(form, indent)

    def emit_define_record_type(self, form: list, indent: int) -> str:
        """(define-record-type Name (make-Name fields...) pred? (field accessor)...)"""
        if len(form) < 4:
            return self.emit_call(form, indent)
        name = self.emit(form[1], indent)
        # Extract field names from accessor definitions
        fields = []
        for item in form[3:]:
            if isinstance(item, list) and len(item) >= 1:
                fields.append(self.emit(item[0], indent))
        field_str = ", ".join(fields)
        return f"record {name}({field_str})"

    def emit_define_library(self, form: list, indent: int) -> str:
        """(define-library (name parts...) decls...)"""
        if len(form) < 2:
            return self.emit_call(form, indent)

        lib_name = form[1]
        if isinstance(lib_name, list):
            name_parts = ", ".join(self.emit(x, indent) for x in lib_name)
        else:
            name_parts = self.emit(lib_name, indent)

        ni = indent + self.indent_width
        pad = " " * ni
        decl_strs = []
        for decl in form[2:]:
            if isinstance(decl, list) and len(decl) >= 1 and isinstance(decl[0], Symbol):
                dname = decl[0].name
                if dname == 'export':
                    exports = ", ".join(self.emit(x, indent) for x in decl[1:])
                    decl_strs.append(f"{pad}export({exports});")
                elif dname == 'import':
                    for imp in decl[1:]:
                        decl_strs.append(f"{pad}{self.emit_import_spec(imp, ni)};")
                elif dname == 'include':
                    for f in decl[1:]:
                        decl_strs.append(f"{pad}include({self.emit(f, ni)});")
                elif dname == 'include-ci':
                    for f in decl[1:]:
                        decl_strs.append(f"{pad}include_ci({self.emit(f, ni)});")
                elif dname == 'begin':
                    for expr in decl[1:]:
                        decl_strs.append(f"{pad}{self.emit(expr, ni)};")
                elif dname == 'cond-expand':
                    decl_strs.append(f"{pad}{self.emit_cond_expand(decl, ni)};")
                elif dname == 'alias-for':
                    if len(decl) >= 2 and isinstance(decl[1], list):
                        alias = ", ".join(self.emit(x, indent) for x in decl[1])
                        decl_strs.append(f"{pad}alias_for({alias});")
                else:
                    decl_strs.append(f"{pad}{self.emit(decl, ni)};")
            else:
                decl_strs.append(f"{pad}{self.emit(decl, ni)};")

        inner = "\n".join(decl_strs)
        return f"library({name_parts}) {{\n{inner}\n}}"

    def emit_import_spec(self, spec, indent: int) -> str:
        """Convert import spec to Eval syntax."""
        if isinstance(spec, list) and len(spec) >= 1:
            if isinstance(spec[0], Symbol):
                name = spec[0].name
                if name == 'only':
                    lib = self.emit_import_spec(spec[1], indent)
                    ids = ", ".join(self.emit(x, indent) for x in spec[2:])
                    return f"only({lib}, {ids})"
                elif name == 'except':
                    lib = self.emit_import_spec(spec[1], indent)
                    ids = ", ".join(self.emit(x, indent) for x in spec[2:])
                    return f"except({lib}, {ids})"
                elif name == 'rename':
                    lib = self.emit_import_spec(spec[1], indent)
                    renames = []
                    for pair in spec[2:]:
                        if isinstance(pair, list) and len(pair) == 2:
                            old = self.emit(pair[0], indent)
                            new = self.emit(pair[1], indent)
                            renames.append(f"{old}: {new}")
                        else:
                            # Single symbol rename pair (old new)
                            renames.append(self.emit(pair, indent))
                    r = ", ".join(renames)
                    return f"rename({lib}, {r})"
                elif name == 'prefix':
                    lib = self.emit_import_spec(spec[1], indent)
                    pfx = self.emit(spec[2], indent) if len(spec) > 2 else ""
                    return f"prefix({lib}, {pfx})"
            # Library name: (scheme base) → import(scheme, base)
            parts = ", ".join(self.emit(x, indent) for x in spec)
            return f"import({parts})"
        return self.emit(spec, indent)

    def emit_cond_expand(self, form: list, indent: int) -> str:
        """(cond-expand (feature body...) (else body...))"""
        clauses = []
        for clause in form[1:]:
            if isinstance(clause, list) and len(clause) >= 2:
                if isinstance(clause[0], Symbol) and clause[0].name == 'else':
                    body = "; ".join(self.emit(x, indent) for x in clause[1:])
                    clauses.append(f"else: {{ {body}; }}")
                else:
                    feat = self.emit(clause[0], indent)
                    body = "; ".join(self.emit(x, indent) for x in clause[1:])
                    clauses.append(f"{feat}: {{ {body}; }}")
        inner = ", ".join(clauses)
        return f"platform_cond({inner})"

    def emit_define_syntax(self, form: list, indent: int) -> str:
        """(define-syntax name transformer)"""
        if len(form) != 3:
            return self.emit_call(form, indent)
        name = self.emit(form[1], indent)
        transformer = form[2]
        if isinstance(transformer, list) and len(transformer) >= 1:
            if isinstance(transformer[0], Symbol) and transformer[0].name == 'syntax-rules':
                return f"macro {name}({self.emit_syntax_rules(transformer, indent)})"
            elif isinstance(transformer[0], Symbol) and transformer[0].name == 'er-macro-transformer':
                # Keep as raw — complex macro
                return f"/* er-macro: {name} */\n{self.emit_raw_scheme(form)}"
            elif isinstance(transformer[0], Symbol) and transformer[0].name == 'sc-macro-transformer':
                return f"/* sc-macro: {name} */\n{self.emit_raw_scheme(form)}"
        return f"macro {name}({self.emit(transformer, indent)})"

    def emit_syntax_rules(self, form: list, indent: int) -> str:
        """(syntax-rules (literals...) (pattern template)...)"""
        if len(form) < 2:
            return "syntax_rules()"
        literals = form[1]
        if isinstance(literals, list):
            lit_str = ", ".join(self.emit(x, indent) for x in literals)
        else:
            lit_str = self.emit(literals, indent)
        rules = []
        for rule in form[2:]:
            if isinstance(rule, list) and len(rule) == 2:
                pat = self.emit_raw_scheme(rule[0])
                tmpl = self.emit_raw_scheme(rule[1])
                rules.append(f"  ({pat}) => ({tmpl})")
        rules_str = ",\n".join(rules)
        return f"syntax_rules({lit_str},\n{rules_str})"

    def emit_raw_scheme(self, expr) -> str:
        """Emit raw Scheme notation for things we can't fully transpile."""
        if isinstance(expr, list):
            inner = " ".join(self.emit_raw_scheme(x) for x in expr)
            return f"({inner})"
        if isinstance(expr, DottedList):
            items = " ".join(self.emit_raw_scheme(x) for x in expr.items)
            cdr = self.emit_raw_scheme(expr.cdr)
            return f"({items} . {cdr})"
        if isinstance(expr, Symbol):
            return expr.name
        if isinstance(expr, str):
            return self.emit_string(expr)
        if isinstance(expr, bool):
            return "#t" if expr else "#f"
        if isinstance(expr, (int, float)):
            return str(expr)
        return repr(expr)

    def emit_import(self, form: list, indent: int) -> str:
        """(import (lib name)...)"""
        specs = []
        for spec in form[1:]:
            specs.append(self.emit_import_spec(spec, indent))
        return "; ".join(specs)

    # --- Binary operators ---

    def emit_binop(self, op: str):
        def handler(self, form: list, indent: int) -> str:
            if len(form) == 2:
                # Unary
                return f"{op}{self.emit(form[1], indent)}"
            if len(form) == 3:
                left = self.emit(form[1], indent)
                right = self.emit(form[2], indent)
                return f"{left} {op} {right}"
            # Multi-arg: (+ a b c) → a + b + c
            parts = [self.emit(x, indent) for x in form[1:]]
            return f" {op} ".join(parts)
        return handler

    def emit_expt(self, form: list, indent: int) -> str:
        if len(form) != 3:
            return self.emit_call(form, indent)
        left = self.emit(form[1], indent)
        right = self.emit(form[2], indent)
        return f"{left} ** {right}"

    def emit_modulo(self, form: list, indent: int) -> str:
        if len(form) != 3:
            return self.emit_call(form, indent)
        left = self.emit(form[1], indent)
        right = self.emit(form[2], indent)
        return f"{left} % {right}"

    def emit_comparison(self, op: str):
        def handler(self, form: list, indent: int) -> str:
            if len(form) != 3:
                return self.emit_call(form, indent)
            left = self.emit(form[1], indent)
            right = self.emit(form[2], indent)
            return f"{left} {op} {right}"
        return handler

    def emit_equal(self, form: list, indent: int) -> str:
        if len(form) != 3:
            return self.emit_call(form, indent)
        left = self.emit(form[1], indent)
        right = self.emit(form[2], indent)
        return f"{left} == {right}"

    def emit_eq(self, form: list, indent: int) -> str:
        if len(form) != 3:
            return self.emit_call(form, indent)
        left = self.emit(form[1], indent)
        right = self.emit(form[2], indent)
        return f"{left} =? {right}"

    # --- Bitwise ---

    def emit_bitwise_and(self, form: list, indent: int) -> str:
        if len(form) != 3:
            return self.emit_call(form, indent)
        left = self.emit(form[1], indent)
        right = self.emit(form[2], indent)
        return f"{left} & {right}"

    def emit_bitwise_ior(self, form: list, indent: int) -> str:
        if len(form) != 3:
            return self.emit_call(form, indent)
        left = self.emit(form[1], indent)
        right = self.emit(form[2], indent)
        return f"{left} | {right}"

    def emit_bitwise_not(self, form: list, indent: int) -> str:
        if len(form) != 2:
            return self.emit_call(form, indent)
        return f"~{self.emit(form[1], indent)}"

    def emit_arithmetic_shift(self, form: list, indent: int) -> str:
        if len(form) != 3:
            return self.emit_call(form, indent)
        val = self.emit(form[1], indent)
        shift = form[2]
        # Detect negative shift (right shift)
        if isinstance(shift, list) and len(shift) == 2:
            if isinstance(shift[0], Symbol) and shift[0].name == '-':
                amount = self.emit(shift[1], indent)
                return f"{val} >> {amount}"
        amount = self.emit(shift, indent)
        return f"{val} << {amount}"

    def emit_apply(self, form: list, indent: int) -> str:
        if len(form) < 3:
            return self.emit_call(form, indent)
        func = self.emit(form[1], indent)
        args = ", ".join(self.emit(x, indent) for x in form[2:])
        return f"apply({func}, {args})"

    def emit_list(self, form: list, indent: int) -> str:
        items = ", ".join(self.emit(x, indent) for x in form[1:])
        return f"[{items}]"

    def emit_vector(self, form: list, indent: int) -> str:
        items = ", ".join(self.emit(x, indent) for x in form[1:])
        return f"#[{items}]"

    def emit_cons(self, form: list, indent: int) -> str:
        if len(form) != 3:
            return self.emit_call(form, indent)
        return self.emit_call(form, indent)

    def emit_error(self, form: list, indent: int) -> str:
        args = ", ".join(self.emit(x, indent) for x in form[1:])
        return f"error({args})"

    # --- Helpers ---

    def emit_params(self, params: list, indent: int) -> str:
        return ", ".join(self.emit(p, indent) for p in params)

    def emit_params_dotted(self, fixed: list, rest, indent: int) -> str:
        parts = [self.emit(p, indent) for p in fixed]
        rest_name = self.emit(rest, indent) if not isinstance(rest, Symbol) else self.emit_symbol(rest.name)
        parts.append(f".. {rest_name}")
        return ", ".join(parts)

    def emit_body(self, exprs: list, indent: int) -> str:
        """Emit body expressions — single expr or block."""
        if len(exprs) == 1:
            return self.emit(exprs[0], indent)
        ni = indent + self.indent_width
        pad = " " * ni
        stmts = [f"{pad}{self.emit(e, ni)};" for e in exprs]
        return "{\n" + "\n".join(stmts) + "\n" + " " * indent + "}"

    # Form handler dispatch table
    _form_handlers = {
        'define': emit_define,
        'set!': emit_set,
        'lambda': emit_lambda,
        'if': emit_if,
        'cond': emit_cond,
        'case': emit_case,
        'let': lambda self, f, i: self.emit_let(f, i, 'let'),
        'let*': lambda self, f, i: self.emit_let(f, i, 'let*'),
        'letrec': lambda self, f, i: self.emit_let(f, i, 'letrec'),
        'letrec*': lambda self, f, i: self.emit_let(f, i, 'letrec'),
        'begin': emit_begin,
        'and': emit_and,
        'or': emit_or,
        'not': emit_not,
        'when': emit_when,
        'unless': emit_unless,
        'do': emit_do,
        'quote': emit_quote,
        'quasiquote': emit_quasiquote,
        'values': emit_values,
        'call-with-values': emit_call_with_values,
        'guard': emit_guard,
        'with-exception-handler': emit_with_exception_handler,
        'define-record-type': emit_define_record_type,
        'define-library': emit_define_library,
        'define-syntax': emit_define_syntax,
        'import': emit_import,
        # Arithmetic
        '+': emit_binop(None, '+'),
        '-': emit_binop(None, '-'),
        '*': emit_binop(None, '*'),
        '/': emit_binop(None, '/'),
        'expt': emit_expt,
        'modulo': emit_modulo,
        # Comparison
        '<': emit_comparison(None, '<'),
        '>': emit_comparison(None, '>'),
        '<=': emit_comparison(None, '<='),
        '>=': emit_comparison(None, '>='),
        'equal?': emit_equal,
        'eq?': emit_eq,
        '=': emit_comparison(None, '=='),
        # Bitwise
        'bitwise-and': emit_bitwise_and,
        'bitwise-ior': emit_bitwise_ior,
        'bitwise-not': emit_bitwise_not,
        'arithmetic-shift': emit_arithmetic_shift,
        # Other
        'apply': emit_apply,
        'list': emit_list,
        'vector': emit_vector,
        'cons': emit_cons,
        'error': emit_error,
    }


# ---------------------------------------------------------------------------
# File conversion
# ---------------------------------------------------------------------------

def convert_file(input_path: str) -> str:
    """Read a .scm/.sld file and return Eval-syntax equivalent."""
    with open(input_path, 'r', encoding='utf-8') as f:
        text = f.read()

    exprs = read_all(text)
    emitter = EvalEmitter()

    lines = []
    for expr in exprs:
        line = emitter.emit(expr)
        lines.append(line + ";")

    return "\n\n".join(lines) + "\n"


def output_extension(input_path: str) -> str:
    """Determine output extension: .scm→.eval, .sld→.evld"""
    if input_path.endswith('.sld'):
        return '.evld'
    return '.eval'


def convert_directory(input_dir: str, output_dir: str, recursive: bool = True):
    """Convert all .scm/.sld files in a directory."""
    input_path = Path(input_dir)
    output_path = Path(output_dir)
    count = 0
    errors = []

    patterns = ['**/*.scm', '**/*.sld'] if recursive else ['*.scm', '*.sld']

    for pattern in patterns:
        for src_file in input_path.glob(pattern):
            rel = src_file.relative_to(input_path)
            ext = output_extension(str(src_file))
            dst_file = output_path / rel.with_suffix(ext)
            dst_file.parent.mkdir(parents=True, exist_ok=True)

            try:
                result = convert_file(str(src_file))
                with open(dst_file, 'w', encoding='utf-8') as f:
                    f.write(result)
                count += 1
                print(f"  {rel} → {rel.with_suffix(ext)}", file=sys.stderr)
            except Exception as e:
                errors.append((str(rel), str(e)))
                print(f"  ERROR {rel}: {e}", file=sys.stderr)

    print(f"\nConverted {count} files, {len(errors)} errors.", file=sys.stderr)
    if errors:
        print("Errors:", file=sys.stderr)
        for path, err in errors:
            print(f"  {path}: {err}", file=sys.stderr)


def main():
    parser = argparse.ArgumentParser(
        description="Convert Scheme (.scm/.sld) to Eval (.eval/.evld) syntax"
    )
    parser.add_argument('input', nargs='?', help='Input file path')
    parser.add_argument('-o', '--output', help='Output file or directory')
    parser.add_argument('--dir', help='Convert entire directory')
    parser.add_argument('--no-recursive', action='store_true',
                        help='Do not recurse into subdirectories')

    args = parser.parse_args()

    if args.dir:
        if not args.output:
            print("Error: --dir requires -o OUTPUT_DIR", file=sys.stderr)
            sys.exit(1)
        convert_directory(args.dir, args.output, recursive=not args.no_recursive)
    elif args.input:
        result = convert_file(args.input)
        if args.output:
            Path(args.output).parent.mkdir(parents=True, exist_ok=True)
            with open(args.output, 'w', encoding='utf-8') as f:
                f.write(result)
            print(f"Written to {args.output}", file=sys.stderr)
        else:
            print(result)
    else:
        parser.print_help()
        sys.exit(1)


if __name__ == '__main__':
    main()
