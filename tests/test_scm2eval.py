"""Tests for the scm2eval transpiler."""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'tools'))

from scm2eval import read_all, EvalEmitter, Symbol, DottedList, VectorLiteral


def emit(text):
    """Helper: read scheme and emit eval."""
    exprs = read_all(text)
    emitter = EvalEmitter()
    return [emitter.emit(e) for e in exprs]


def emit1(text):
    """Helper: read scheme and emit eval (single expr)."""
    results = emit(text)
    assert len(results) == 1
    return results[0]


class TestReader:
    def test_int(self):
        assert read_all("42") == [42]

    def test_float(self):
        assert read_all("3.14") == [3.14]

    def test_string(self):
        assert read_all('"hello"') == ["hello"]

    def test_symbol(self):
        r = read_all("foo")
        assert len(r) == 1
        assert isinstance(r[0], Symbol)
        assert r[0].name == "foo"

    def test_list(self):
        r = read_all("(+ 1 2)")
        assert len(r) == 1
        assert isinstance(r[0], list)
        assert len(r[0]) == 3

    def test_nested(self):
        r = read_all("(define (f x) (* x x))")
        assert len(r) == 1

    def test_bool(self):
        assert read_all("#t #f") == [True, False]

    def test_quote(self):
        r = read_all("'foo")
        assert r == [[Symbol("quote"), Symbol("foo")]]

    def test_dotted(self):
        r = read_all("(a b . c)")
        assert len(r) == 1
        assert isinstance(r[0], DottedList)
        assert len(r[0].items) == 2

    def test_vector(self):
        r = read_all("#(1 2 3)")
        assert len(r) == 1
        assert isinstance(r[0], VectorLiteral)
        assert r[0].items == [1, 2, 3]

    def test_line_comment(self):
        r = read_all("; comment\n42")
        assert r == [42]

    def test_block_comment(self):
        r = read_all("#| block |# 42")
        assert r == [42]

    def test_datum_comment(self):
        r = read_all("#;(ignored) 42")
        assert r == [42]

    def test_multiple(self):
        r = read_all("1 2 3")
        assert r == [1, 2, 3]


class TestEmitArithmetic:
    def test_add(self):
        assert emit1("(+ 1 2)") == "1 + 2"

    def test_sub(self):
        assert emit1("(- 1 2)") == "1 - 2"

    def test_mul(self):
        assert emit1("(* 3 4)") == "3 * 4"

    def test_div(self):
        assert emit1("(/ 10 2)") == "10 / 2"

    def test_multi_arg(self):
        assert emit1("(+ 1 2 3)") == "1 + 2 + 3"

    def test_expt(self):
        assert emit1("(expt 2 10)") == "2 ** 10"

    def test_modulo(self):
        assert emit1("(modulo 10 3)") == "10 % 3"

    def test_unary_minus(self):
        assert emit1("(- x)") == "-x"


class TestEmitComparison:
    def test_lt(self):
        assert emit1("(< 1 2)") == "1 < 2"

    def test_gt(self):
        assert emit1("(> 2 1)") == "2 > 1"

    def test_lte(self):
        assert emit1("(<= 1 2)") == "1 <= 2"

    def test_equal(self):
        assert emit1("(equal? a b)") == "a == b"

    def test_eq(self):
        assert emit1("(eq? x y)") == "x =? y"


class TestEmitDefine:
    def test_simple(self):
        assert emit1("(define x 42)") == "x := 42"

    def test_function(self):
        assert emit1("(define (f x) (* x x))") == "f := function(x) x * x"

    def test_function_multi_body(self):
        r = emit1("(define (f x) (display x) (* x x))")
        assert "function(x)" in r
        assert "display(x)" in r

    def test_rest_param(self):
        r = emit1("(define (f x . rest) x)")
        assert ".. rest" in r


class TestEmitLambda:
    def test_simple(self):
        assert emit1("(lambda (x) x)") == "function(x) x"

    def test_multi_param(self):
        assert emit1("(lambda (x y) (+ x y))") == "function(x, y) x + y"


class TestEmitIf:
    def test_if_else(self):
        assert emit1("(if (< x 0) 0 x)") == "if(x < 0) 0 else x"

    def test_if_no_else(self):
        assert emit1("(if #t 42)") == "if(true) 42"


class TestEmitLet:
    def test_let(self):
        assert emit1("(let ((x 1) (y 2)) (+ x y))") == "let(x := 1, y := 2) x + y"

    def test_let_star(self):
        r = emit1("(let* ((x 1) (y (+ x 1))) y)")
        assert "let*(" in r
        assert "x := 1" in r

    def test_named_let(self):
        r = emit1("(let lp ((i 0)) (if (> i 5) i (lp (+ i 1))))")
        assert "letrec(" in r
        assert "lp" in r


class TestEmitLogical:
    def test_and(self):
        assert "&&" in emit1("(and a b)")

    def test_or(self):
        assert "||" in emit1("(or a b)")

    def test_not(self):
        assert emit1("(not x)") == "!x"


class TestEmitBitwise:
    def test_bitwise_and(self):
        assert emit1("(bitwise-and a b)") == "a & b"

    def test_bitwise_or(self):
        assert emit1("(bitwise-ior a b)") == "a | b"

    def test_bitwise_not(self):
        assert emit1("(bitwise-not x)") == "~x"

    def test_shift_left(self):
        assert emit1("(arithmetic-shift x 3)") == "x << 3"

    def test_shift_right(self):
        assert emit1("(arithmetic-shift x (- 3))") == "x >> 3"


class TestEmitControlFlow:
    def test_begin(self):
        r = emit1("(begin 1 2 3)")
        assert "{" in r

    def test_when(self):
        assert "when(" in emit1("(when #t 42)")

    def test_unless(self):
        assert "unless(" in emit1("(unless #f 42)")

    def test_cond(self):
        r = emit1("(cond (#t 1) (else 2))")
        assert "cond(" in r
        assert "else:" in r

    def test_case(self):
        r = emit1("(case x ((1 2) 'a) (else 'b))")
        assert "case(" in r


class TestEmitLiterals:
    def test_list(self):
        assert emit1("(list 1 2 3)") == "[1, 2, 3]"

    def test_vector(self):
        assert emit1("(vector 1 2 3)") == "#[1, 2, 3]"

    def test_bool(self):
        assert emit1("#t") == "true"
        assert emit1("#f") == "false"

    def test_string(self):
        assert emit1('"hello"') == '"hello"'

    def test_quote(self):
        assert emit1("'foo") == "'foo"


class TestEmitLibrary:
    def test_simple(self):
        r = emit1("(define-library (scheme write) (export write) (import (scheme base)))")
        assert "library(scheme, write)" in r
        assert "export(write)" in r
        assert "import(scheme, base)" in r

    def test_with_begin(self):
        r = emit1("(define-library (test) (begin (define x 42)))")
        assert "library(test)" in r
        assert "x := 42" in r


class TestEmitRecord:
    def test_simple(self):
        r = emit1("(define-record-type Point (make-Point x y) Point? (x Point-x) (y Point-y))")
        assert "record Point" in r


class TestEmitGuard:
    def test_simple(self):
        r = emit1("(guard (e (#t 999)) (error \"boom\"))")
        assert "try" in r
        assert "catch" in r


class TestSymbolConversion:
    def test_hyphen_to_backtick(self):
        assert emit1("string-append") == "`string-append`"

    def test_arrow_to_backtick(self):
        assert emit1("char->integer") == "`char->integer`"

    def test_preserve_operators(self):
        assert emit1("+") == "+"


class TestEmitMacro:
    def test_syntax_rules(self):
        r = emit1("(define-syntax my-when (syntax-rules () ((my-when test body) (if test body))))")
        assert "macro `my-when`" in r
        assert "syntax_rules" in r

    def test_er_macro(self):
        r = emit1("(define-syntax foo (er-macro-transformer (lambda (e r c) 42)))")
        assert "er-macro: foo" in r
