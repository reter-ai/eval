"""Test the previously-missing R7RS functions."""
import pytest
from chibi_eval import Eval


@pytest.fixture
def e():
    return Eval()


class TestSchemeBaseExtras:
    """Functions from (scheme base) extras."""

    def test_square(self, e):
        assert e.eval("`square`(5);") == 25

    def test_boolean_eq(self, e):
        assert e.eval("`boolean=?`(true, true);") == True
        assert e.eval("`boolean=?`(false, false);") == True
        assert e.eval("`boolean=?`(true, false);") == False

    def test_symbol_eq(self, e):
        assert e.eval("`symbol=?`('a, 'a);") == True
        assert e.eval("`symbol=?`('a, 'b);") == False

    def test_error_object_p(self, e):
        assert e.eval("`error-object?`(42);") == False

    def test_error_object_message(self, e):
        result = e.eval("""
            try {
                error("test error");
            } catch(ex) {
                `error-object-message`(ex);
            };
        """)
        assert result == "test error"

    def test_eof_object(self, e):
        result = e.eval("`eof-object?`(`eof-object`());")
        assert result == True

    def test_call_with_port(self, e):
        result = e.eval("""
            `call-with-port`(`open-input-string`("hello"), function(p)
                `read-char`(p)
            );
        """)
        assert result is not None


class TestStringExtras:
    """String functions from (scheme base) via (chibi string)."""

    def test_string_map(self, e):
        result = e.eval('`string-map`(`char-upcase`, "hello");')
        assert result == "HELLO"

    def test_string_for_each(self, e):
        result = e.eval("""
            define x = 0;
            `string-for-each`(function(c) { x += 1; }, "hello");
            x;
        """)
        assert result == 5


class TestVectorExtras:
    """Vector functions from (scheme base) extras."""

    def test_vector_map(self, e):
        result = e.eval("`vector-map`(function(x) x * 2, #[1, 2, 3]);")
        assert result == [2, 4, 6]

    def test_vector_for_each(self, e):
        result = e.eval("""
            define total = 0;
            `vector-for-each`(function(x) { total += x; }, #[10, 20, 30]);
            total;
        """)
        assert result == 60

    def test_vector_append(self, e):
        result = e.eval("`vector-append`(#[1, 2], #[3, 4]);")
        assert result == [1, 2, 3, 4]

    def test_vector_to_string(self, e):
        result = e.eval("""`vector->string`(`list->vector`(`string->list`("abc")));""")
        assert result == "abc"

    def test_string_to_vector(self, e):
        result = e.eval("""`string->vector`("abc");""")
        assert len(result) == 3


class TestIOExtras:
    """I/O functions — pure Scheme implementations."""

    def test_read_line(self, e):
        result = e.eval('`read-line`(`open-input-string`("hello world"));')
        assert result == "hello world"

    def test_read_string(self, e):
        result = e.eval('`read-string`(5, `open-input-string`("hello world"));')
        assert result == "hello"

    def test_write_string(self, e):
        result = e.eval("""
            let(p = `open-output-string`()) {
                `write-string`("hello", p);
                `get-output-string`(p);
            };
        """)
        assert result == "hello"

    def test_read_u8(self, e):
        # Use string port (bytevector ports need C extension)
        result = e.eval('`read-u8`(`open-input-string`("A"));')
        assert result == 65

    def test_peek_u8(self, e):
        result = e.eval("""
            let(p = `open-input-string`("AB")) {
                define x = `peek-u8`(p);
                define y = `read-u8`(p);
                x == y;
            };
        """)
        assert result == True

    def test_write_u8(self, e):
        result = e.eval("""
            let(p = `open-output-string`()) {
                `write-u8`(65, p);
                `write-u8`(66, p);
                `get-output-string`(p);
            };
        """)
        assert result == "AB"


class TestParameters:
    """make-parameter and parameterize from (srfi 39)."""

    def test_make_parameter(self, e):
        result = e.eval("""
            define p = `make-parameter`(42);
            p();
        """)
        assert result == 42


class TestSort:
    """Sort function from (srfi 95)."""

    def test_sort(self, e):
        result = e.eval("sort([3, 1, 4, 1, 5, 9, 2, 6], <);")
        assert result == [1, 1, 2, 3, 4, 5, 6, 9]


class TestCallCC:
    """call/cc should still work after extras import."""

    def test_callcc_alias(self, e):
        assert e.eval("callcc(function(k) k(42));") == 42

    def test_call_cc(self, e):
        assert e.eval("`call/cc`(function(k) k(99));") == 99
