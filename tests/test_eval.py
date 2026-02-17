"""Tests for the chibi-eval package."""

import io
import sys
import pytest
from chibi_eval import Eval, EvalError, EvalSyntaxError


@pytest.fixture
def e():
    return Eval()


class TestArithmetic:
    def test_add(self, e):
        assert e.eval("10 + 20;") == 30

    def test_sub(self, e):
        assert e.eval("10 - 3;") == 7

    def test_mul(self, e):
        assert e.eval("6 * 7;") == 42

    def test_div(self, e):
        assert e.eval("10 / 2;") == 5

    def test_pow(self, e):
        assert e.eval("2 ** 10;") == 1024

    def test_mod(self, e):
        assert e.eval("10 % 3;") == 1

    def test_unary_minus(self, e):
        assert e.eval("-5;") == -5

    def test_precedence(self, e):
        assert e.eval("(10 + 20) * 3;") == 90
        assert e.eval("2 + 3 * 4;") == 14

    def test_nested(self, e):
        assert e.eval("((1 + 2) * (3 + 4));") == 21


class TestVariables:
    def test_define(self, e):
        e.eval("define x = 42;")
        assert e["x"] == 42

    def test_set(self, e):
        e.eval("define x = 42;")
        e.eval("x = 100;")
        assert e["x"] == 100

    def test_compound_assign(self, e):
        e.eval("define c = 10;")
        e.eval("c += 5;")
        assert e["c"] == 15
        e.eval("c -= 3;")
        assert e["c"] == 12

    def test_increment(self, e):
        e.eval("define n = 10;")
        e.eval("n++;")
        assert e["n"] == 11

    def test_decrement(self, e):
        e.eval("define n = 10;")
        e.eval("n--;")
        assert e["n"] == 9

    def test_setitem(self, e):
        e["x"] = 42
        assert e.eval("x;") == 42

    def test_setitem_list(self, e):
        e["data"] = [1, 2, 3]
        assert e.eval("data;") == [1, 2, 3]


class TestComparison:
    def test_lt(self, e):
        assert e.eval("1 < 2;") is True
        assert e.eval("2 < 1;") is False

    def test_gt(self, e):
        assert e.eval("2 > 1;") is True

    def test_lte(self, e):
        assert e.eval("1 <= 1;") is True

    def test_gte(self, e):
        assert e.eval("2 >= 2;") is True

    def test_eq(self, e):
        assert e.eval("1 == 1;") is True
        assert e.eval("1 == 2;") is False


class TestLogical:
    def test_and(self, e):
        assert e.eval("(1 < 2) && (3 > 1);") is True
        assert e.eval("(1 > 2) && (3 > 1);") is False

    def test_or(self, e):
        assert e.eval("(1 > 2) || (3 > 1);") is True
        assert e.eval("(1 > 2) || (3 < 1);") is False

    def test_not(self, e):
        assert e.eval("!(1 > 2);") is True
        assert e.eval("!true;") is False


class TestLiterals:
    def test_string(self, e):
        assert e.eval('"hello";') == "hello"

    def test_bool(self, e):
        assert e.eval("true;") is True
        assert e.eval("false;") is False

    def test_list(self, e):
        assert e.eval("[1, 2, 3];") == [1, 2, 3]

    def test_empty_list(self, e):
        assert e.eval("[];") == []

    def test_nested_list(self, e):
        assert e.eval("[1, [2, 3], 4];") == [1, [2, 3], 4]

    def test_vector(self, e):
        assert e.eval("#[1, 2, 3];") == [1, 2, 3]

    def test_quote(self, e):
        assert e.eval("'hello;") == "hello"


class TestControlFlow:
    def test_if_true(self, e):
        assert e.eval("if(true) 1 else 2;") == 1

    def test_if_false(self, e):
        assert e.eval("if(false) 1 else 2;") == 2

    def test_if_without_else(self, e):
        e.eval("define x = 0;")
        e.eval("if(true) x = 42;")
        assert e["x"] == 42

    def test_block(self, e):
        assert e.eval("{ define x = 10; x + 5; };") == 15

    def test_when(self, e):
        e.eval("define w = 0;")
        e.eval("when(true) w = 1;")
        assert e["w"] == 1

    def test_unless(self, e):
        e.eval("define w = 0;")
        e.eval("unless(false) w = 2;")
        assert e["w"] == 2

    def test_cond(self, e):
        assert e.eval("cond(false: 1, true: 2);") == 2


class TestFunctions:
    def test_simple(self, e):
        e.eval("define double = function(x) x * 2;")
        assert e.eval("double(21);") == 42

    def test_recursive(self, e):
        e.eval("define factorial = function(n) if(n <= 1) 1 else n * factorial(n - 1);")
        assert e.eval("factorial(10);") == 3628800

    def test_closure(self, e):
        e.eval("define make_adder = function(n) function(x) n + x;")
        assert e.eval("make_adder(10)(32);") == 42

    def test_return(self, e):
        e.eval("define f = function(x) { if(x > 0) return x * 10; return 0; };")
        assert e.eval("f(5);") == 50
        assert e.eval("f(0);") == 0

    def test_block_body(self, e):
        e.eval("define f = function(x) { define y = x * 2; y + 1; };")
        assert e.eval("f(10);") == 21

    def test_call_method(self, e):
        e.eval("define add = function(a, b) a + b;")
        assert e.call("add", 3, 4) == 7


class TestLetBindings:
    def test_let(self, e):
        assert e.eval("let(a = 1, b = 2) a + b;") == 3

    def test_let_star(self, e):
        assert e.eval("let*(x = 10, y = x + 5) y;") == 15

    def test_letrec(self, e):
        assert e.eval(
            "letrec(f = function(n) if(n <= 0) 0 else n + f(n - 1)) f(5);"
        ) == 15


class TestLoops:
    def test_while(self, e):
        e.eval("define i = 0; define total = 0;")
        e.eval("while(i < 10) { total += i; i++; };")
        assert e["total"] == 45

    def test_while_assign(self, e):
        """Test that = inside while mutates (doesn't shadow)."""
        e.eval("define x = 0;")
        e.eval("while(x < 3) x = x + 1;")
        assert e["x"] == 3

    def test_for(self, e):
        e.eval("define sum = 0;")
        e.eval("for(let j = 0, j < 5, j++) sum += j;")
        assert e["sum"] == 10

    def test_do_until(self, e):
        e.eval("define k = 0;")
        e.eval("do k++ until(k >= 5);")
        assert e["k"] == 5

    def test_break(self, e):
        e.eval("define n = 0;")
        e.eval("while(true) { if(n >= 3) break; n++; };")
        assert e["n"] == 3

    def test_break_in_for(self, e):
        e.eval("define total = 0;")
        e.eval("for(let i = 0, i < 100, i++) { if(i >= 5) break; total += i; };")
        assert e["total"] == 10


class TestHigherOrder:
    def test_map(self, e):
        assert e.eval("map(function(x) x * 2, [1, 2, 3]);") == [2, 4, 6]

    def test_filter(self, e):
        assert e.eval("filter(function(x) x > 2, [1, 2, 3, 4, 5]);") == [3, 4, 5]

    def test_apply(self, e):
        assert e.eval("apply(+, [3, 4]);") == 7

    def test_fold(self, e):
        assert e.eval("fold(+, 0, [1, 2, 3, 4, 5]);") == 15

    def test_callcc(self, e):
        assert e.eval("`call/cc`(function(k) k(42));") == 42


class TestPythonInterop:
    def test_define_function(self, e):
        e.define_function("py_sqrt", lambda x: x**0.5, 1)
        assert e.eval("py_sqrt(144);") == 12.0

    def test_py_import(self, e):
        result = e.eval('define os = py_import("os"); py_method(os, "getcwd");')
        assert isinstance(result, str)

    def test_py_eval(self, e):
        assert e.eval('py_eval("1 + 2");') == 3

    def test_py_getattr(self, e):
        e.eval('define math = py_import("math");')
        pi = e.eval('py_getattr(math, "pi");')
        assert abs(pi - 3.14159265) < 0.001

    def test_python_list(self, e):
        e["data"] = [1, 2, 3, 4, 5]
        assert e.eval("map(function(x) x * 2, data);") == [2, 4, 6, 8, 10]


class TestTryCatch:
    def test_basic(self, e):
        r = e.eval('try error("boom") catch(err) 999;')
        assert r == 999

    def test_block(self, e):
        r = e.eval('try { error("oops"); } catch(err) { 42; };')
        assert r == 42

    def test_no_error(self, e):
        r = e.eval("try 42 catch(err) 0;")
        assert r == 42


class TestRecords:
    def test_basic(self, e):
        e.eval("record Point(x, y);")
        e.eval("define p = Point(3, 4);")
        assert e.eval("p->x;") == 3
        assert e.eval("p->y;") == 4

    def test_predicate(self, e):
        e.eval("record Pair(a, b);")
        e.eval("define p = Pair(1, 2);")
        assert e.eval("Pair?(p);") is True
        assert e.eval("Pair?(42);") is False


class TestDict:
    def test_create_and_access(self, e):
        e.eval("define d = dict(x: 10, y: 20);")
        assert e.eval("d->x;") == 10
        assert e.eval("d->y;") == 20

    def test_predicate(self, e):
        e.eval("define d = dict(a: 1);")
        assert e.eval("dict?(d);") is True
        assert e.eval("dict?(42);") is False
        assert e.eval('dict?("hello");') is False

    def test_empty(self, e):
        e.eval("define d = dict();")
        assert e.eval("dict?(d);") is True
        assert e.eval("d->size();") == 0

    def test_get(self, e):
        e.eval('define d = dict(name: "Alice");')
        assert e.eval('d->get("name");') == "Alice"
        assert e.eval('d->get("missing");') is False

    def test_set(self, e):
        e.eval("define d = dict(a: 1);")
        e.eval('d->set("b", 2);')
        assert e.eval("d->b;") == 2
        e.eval('d->set("a", 99);')
        assert e.eval("d->a;") == 99

    def test_delete(self, e):
        e.eval("define d = dict(a: 1, b: 2);")
        e.eval('d->delete("a");')
        assert e.eval('d->has?("a");') is False
        assert e.eval('d->has?("b");') is True
        assert e.eval("d->size();") == 1

    def test_keys_values_size(self, e):
        e.eval("define d = dict(a: 1, b: 2, c: 3);")
        assert e.eval("d->size();") == 3
        assert e.eval("`length`(d->keys());") == 3
        assert e.eval("`length`(d->values());") == 3

    def test_nested(self, e):
        e.eval("define d = dict(inner: dict(v: 42));")
        assert e.eval("d->inner->v;") == 42

    def test_dict_from_function(self, e):
        e.eval("define mk = function(x, y) dict(x: x, y: y);")
        e.eval("define p = mk(3, 4);")
        assert e.eval("p->x;") == 3
        assert e.eval("p->y;") == 4


class TestOOP:
    def test_constructor_interface(self, e):
        e.eval(
            "define Point = constructor(x, y) "
            "interface(x: x, y: y, dist: function() (x**2 + y**2)**0.5);"
        )
        e.eval("define p = Point(3, 4);")
        assert e.eval("p->x;") == 3
        assert e.eval("p->y;") == 4
        assert e.eval("p->dist();") == 5.0


class TestBitwise:
    def test_and(self, e):
        assert e.eval("3 & 5;") == 1

    def test_or(self, e):
        assert e.eval("3 | 5;") == 7

    def test_not(self, e):
        assert e.eval("~0;") == -1

    def test_shift_left(self, e):
        assert e.eval("1 << 4;") == 16

    def test_shift_right(self, e):
        assert e.eval("16 >> 2;") == 4


class TestOp:
    def test_arithmetic(self, e):
        assert e.eval("apply(+, [3, 4]);") == 7
        assert e.eval("apply(-, [10, 3]);") == 7
        assert e.eval("apply(*, [6, 7]);") == 42
        assert e.eval("apply(/, [10, 2]);") == 5

    def test_comparison(self, e):
        assert e.eval("apply(<, [1, 2]);") is True
        assert e.eval("apply(>=, [2, 2]);") is True
        assert e.eval("apply(==, [42, 42]);") is True

    def test_higher_order(self, e):
        assert e.eval("fold(+, 0, [1, 2, 3, 4, 5]);") == 15
        assert e.eval("fold(*, 1, [1, 2, 3, 4, 5]);") == 120

    def test_in_list(self, e):
        e.eval("define ops = [+, -, *];")
        assert e.eval("apply(car(ops), [3, 4]);") == 7


class TestStringOps:
    def test_append(self, e):
        assert e.eval('`string-append`("hello", " ", "world");') == "hello world"

    def test_length(self, e):
        assert e.eval('`string-length`("hello");') == 5


class TestPrint:
    def test_int(self, e, capsys):
        e.eval("print(42);")
        assert capsys.readouterr().out.strip() == "42"

    def test_list(self, e, capsys):
        e.eval("print([1, 2, 3]);")
        assert capsys.readouterr().out.strip() == "[1, 2, 3]"

    def test_bool(self, e, capsys):
        e.eval("print(true);")
        assert capsys.readouterr().out.strip() == "true"

    def test_nested_list(self, e, capsys):
        e.eval("print([1, [2, 3], 4]);")
        assert capsys.readouterr().out.strip() == "[1, [2, 3], 4]"


class TestErrors:
    def test_syntax_error(self, e):
        with pytest.raises(EvalSyntaxError):
            e.eval("1 +;")

    def test_undefined_variable(self, e):
        with pytest.raises(EvalError):
            e.eval("undefined_xyz;")


class TestBacktickIdent:
    """Backtick-quoted identifiers for raw Scheme names."""

    def test_list_to_vector(self, e):
        assert e.eval('`list->vector`([1, 2, 3]);') == [1, 2, 3]

    def test_vector_to_list(self, e):
        assert e.eval('`vector->list`(#[10, 20]);') == [10, 20]

    def test_exact_to_inexact(self, e):
        assert e.eval('`exact->inexact`(42);') == 42.0

    def test_inexact_to_exact(self, e):
        assert e.eval('`inexact->exact`(3.0);') == 3

    def test_make_list(self, e):
        assert e.eval('`make-list`(3, 0);') == [0, 0, 0]

    def test_open_input_string(self, e):
        assert e.eval('`input-port?`(`open-input-string`("hello"));') is True

    def test_with_exception_handler(self, e):
        assert e.eval('`with-exception-handler`(function(e) 99, function() 42);') == 42

    def test_list_copy(self, e):
        assert e.eval('`list-copy`([1, 2, 3]);') == [1, 2, 3]

    def test_bytevector_ops(self, e):
        assert e.eval('`bytevector-length`(`make-bytevector`(5, 0));') == 5

    def test_dynamic_wind(self, e):
        assert e.eval('`dynamic-wind`(function() 0, function() 42, function() 0);') == 42

    def test_set_car(self, e):
        e.eval('define p = [1, 2, 3];')
        e.eval('`set-car!`(p, 99);')
        assert e.eval('car(p);') == 99

    def test_mixed_with_regular(self, e):
        """Backtick idents mix with regular Eval syntax."""
        assert e.eval('map(function(x) x * 2, `list-copy`([1, 2, 3]));') == [2, 4, 6]

    def test_string_to_list(self, e):
        result = e.eval('length(`string->list`("hello"));')
        assert result == 5

    def test_assign_to_var(self, e):
        """Backtick ident result can be assigned to a regular variable."""
        e.eval('define v = `list->vector`([10, 20, 30]);')
        assert e.eval('`vector-ref`(v, 2);') == 30


class TestGrammarFeatures:
    def test_values_receive(self, e):
        assert e.eval("receive(a, b) from values(1, 2) a + b;") == 3

    def test_dotted_pair(self, e):
        result = e.eval("(1 .. 2);")
        assert result == (1, 2)

    def test_dotted_pair_multi(self, e):
        result = e.eval("(1, 2 .. 3);")
        assert result == (1, 2, 3)

    def test_compile_time_eval(self, e):
        assert e.eval("!!(2 + 3);") == 5

    def test_super_dispatch(self, e):
        code = (
            "define Base = constructor() "
            "interface(greet: function() \"hello\");"
            "define Child = constructor() {"
            "  super Base();"
            "  interface(name: function() \"child\");"
            "};"
            "define c = Child();"
        )
        e.eval(code)
        assert e.eval('c->name();') == "child"
        assert e.eval('c->greet();') == "hello"

    def test_include_error(self, e):
        with pytest.raises((EvalError, RuntimeError)):
            e.eval('include("nonexistent_file_12345.scm");')

    def test_case(self, e):
        assert e.eval('case(2, (1): "a", (2): "b", else: "c");') == "b"

    def test_case_else(self, e):
        assert e.eval('case(99, (1): "a", (2): "b", else: "c");') == "c"


class TestGCStress:
    def test_many_defines(self, e):
        for i in range(500):
            e.eval(f"define tmp_{i} = {i};")
        assert e["tmp_499"] == 499

    def test_many_python_functions(self, e):
        for i in range(50):
            e.define_function(f"pyfn_{i}", lambda x, n=i: x + n, 1)
        assert e.eval("pyfn_25(10);") == 35


class TestTestFramework:
    def test_basic_pass(self, e, capsys):
        e.eval('test(4, 2 + 2);')
        assert "PASS" in capsys.readouterr().out

    def test_basic_fail(self, e, capsys):
        e.eval('test(4, 2 + 3);')
        out = capsys.readouterr().out
        assert "FAIL" in out

    def test_named(self, e, capsys):
        e.eval('test("addition", 4, 2 + 2);')
        out = capsys.readouterr().out
        assert "addition" in out
        assert "PASS" in out

    def test_assert_pass(self, e, capsys):
        e.eval('test_assert(5 > 0);')
        assert "PASS" in capsys.readouterr().out

    def test_assert_fail(self, e, capsys):
        e.eval('test_assert(5 < 0);')
        out = capsys.readouterr().out
        assert "FAIL" in out

    def test_error_pass(self, e, capsys):
        e.eval('test_error(function() 1 / 0);')
        assert "PASS" in capsys.readouterr().out

    def test_error_fail(self, e, capsys):
        e.eval('test_error(function() 42);')
        out = capsys.readouterr().out
        assert "FAIL" in out

    def test_group(self, e, capsys):
        e.eval('''
            test_group("math") {
                test(4, 2 + 2);
                test(9, 3 * 3);
            };
        ''')
        out = capsys.readouterr().out
        assert "math" in out
        assert "2 pass" in out

    def test_end_returns_failures(self, e):
        e.eval('test_begin("x");')
        e.eval('test(4, 2 + 2);')
        e.eval('test(4, 2 + 3);')
        result = e.eval('test_end();')
        assert result == 1  # 1 failure
