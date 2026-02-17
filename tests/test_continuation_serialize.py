"""Tests for continuation serialization/deserialization."""

import pytest
from chibi_eval import Eval, EvalError, ChibiSexp


@pytest.fixture
def e():
    return Eval()


class TestBasicRoundTrip:
    """Test basic serialize -> deserialize -> resume."""

    def test_simple_continuation(self, e):
        """Capture a continuation and round-trip it."""
        k = e.eval_raw("callcc(function(k) k);")
        data = e.serialize_continuation(k)
        assert isinstance(data, bytes)
        assert len(data) > 0

        k2 = e.deserialize_continuation(data)
        assert isinstance(k2, ChibiSexp)

        # Resume with a value
        result = k2(42)
        assert result == 42

    def test_continuation_returns_string(self, e):
        k = e.eval_raw("callcc(function(k) k);")
        data = e.serialize_continuation(k)
        k2 = e.deserialize_continuation(data)
        result = k2("hello")
        assert result == "hello"

    def test_continuation_returns_list(self, e):
        k = e.eval_raw("callcc(function(k) k);")
        data = e.serialize_continuation(k)
        k2 = e.deserialize_continuation(data)
        result = k2([1, 2, 3])
        assert result == [1, 2, 3]

    def test_continuation_returns_boolean(self, e):
        k = e.eval_raw("callcc(function(k) k);")
        data = e.serialize_continuation(k)
        k2 = e.deserialize_continuation(data)
        assert k2(True) is True
        # Note: calling again with False on the same continuation
        # may not work since it was already resumed; create a fresh one
        k3 = e.deserialize_continuation(data)
        assert k3(False) is False


class TestCrossContext:
    """Test serialization in one Eval context and deserialization in another."""

    def test_cross_context_simple(self):
        e1 = Eval()
        k = e1.eval_raw("callcc(function(k) k);")
        data = e1.serialize_continuation(k)

        e2 = Eval()
        k2 = e2.deserialize_continuation(data)
        result = k2(99)
        assert result == 99

    def test_cross_context_with_arithmetic(self):
        e1 = Eval()
        # Continuation that captures some computation context
        k = e1.eval_raw("""
            callcc(function(k) {
                define x = 10;
                k;
            });
        """)
        data = e1.serialize_continuation(k)

        e2 = Eval()
        k2 = e2.deserialize_continuation(data)
        result = k2(42)
        assert result == 42


class TestClosures:
    """Test continuations that capture closure variables."""

    def test_continuation_in_closure(self, e):
        """Continuation captured inside a function that closes over variables."""
        k = e.eval_raw("""
            {
                define x = 100;
                callcc(function(k) k);
            };
        """)
        data = e.serialize_continuation(k)
        k2 = e.deserialize_continuation(data)
        result = k2(7)
        assert result == 7


class TestErrorCases:
    """Test error handling for invalid inputs."""

    def test_serialize_non_procedure(self, e):
        """Trying to serialize a non-procedure (e.g. fixnum) should raise."""
        val = e.eval_raw("42;")
        with pytest.raises(EvalError):
            e.serialize_continuation(val)

    def test_serialize_non_sexp(self, e):
        """Trying to serialize a non-ChibiSexp should raise."""
        with pytest.raises(EvalError):
            e.serialize_continuation(42)

    def test_deserialize_invalid_data(self, e):
        """Trying to deserialize garbage data should raise."""
        with pytest.raises(EvalError):
            e.deserialize_continuation(b"garbage data here")

    def test_deserialize_truncated(self, e):
        """Truncated valid data should raise."""
        k = e.eval_raw("callcc(function(k) k);")
        data = e.serialize_continuation(k)
        with pytest.raises(EvalError):
            e.deserialize_continuation(data[:10])

    def test_deserialize_empty(self, e):
        """Empty data should raise."""
        with pytest.raises(EvalError):
            e.deserialize_continuation(b"")

    def test_serialize_wrong_magic(self, e):
        """Data with wrong magic bytes should raise."""
        k = e.eval_raw("callcc(function(k) k);")
        data = e.serialize_continuation(k)
        bad_data = b"XXXX" + data[4:]
        with pytest.raises(EvalError):
            e.deserialize_continuation(bad_data)


class TestBinaryFormat:
    """Test the binary format structure."""

    def test_magic_bytes(self, e):
        k = e.eval_raw("callcc(function(k) k);")
        data = e.serialize_continuation(k)
        assert data[:4] == b"CCHI"

    def test_version(self, e):
        k = e.eval_raw("callcc(function(k) k);")
        data = e.serialize_continuation(k)
        version = int.from_bytes(data[4:8], byteorder='little')
        assert version == 1

    def test_pointer_size(self, e):
        import struct
        k = e.eval_raw("callcc(function(k) k);")
        data = e.serialize_continuation(k)
        ptr_size = int.from_bytes(data[8:12], byteorder='little')
        assert ptr_size == struct.calcsize("P")

    def test_deterministic(self, e):
        """Same continuation serialized twice should produce same bytes."""
        k = e.eval_raw("callcc(function(k) k);")
        data1 = e.serialize_continuation(k)
        data2 = e.serialize_continuation(k)
        assert data1 == data2


class TestSICPExamples:
    """Test with SICP-style examples using continuations."""

    def test_escape_continuation(self, e):
        """Test escape continuation pattern."""
        result = e.eval("""
            callcc(function(exit) {
                define x = 1;
                define y = 2;
                if(x + y == 3) exit(x + y);
                999;
            });
        """)
        assert result == 3

    def test_captured_continuation_serialize(self, e):
        """Capture a continuation, serialize it, deserialize, resume."""
        k = e.eval_raw("callcc(function(k) k);")
        data = e.serialize_continuation(k)

        # Deserialize and resume with a value
        k2 = e.deserialize_continuation(data)
        result = k2("second")
        assert result == "second"
