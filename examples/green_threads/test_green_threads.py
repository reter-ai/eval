"""Green threads + continuation serialization example.

Demonstrates:
  - Cooperative multitasking via continuations (call/cc)
  - Round-robin thread scheduling with yield
  - Freezing a thread mid-execution (capturing its continuation)
  - Serializing a frozen thread to bytes
  - Resuming the frozen thread in a different Eval context
"""

import os
import pytest
from chibi_eval import Eval, EvalError

HERE = os.path.dirname(os.path.abspath(__file__))


@pytest.fixture
def e():
    ev = Eval()
    ev.load(os.path.join(HERE, "scheduler.eval"))
    return ev


# =========================================================================
# Cooperative scheduling
# =========================================================================

class TestCooperativeScheduling:
    def test_two_threads_interleave(self, e):
        """Two threads yield back and forth."""
        e.eval("""
            log := [];
            spawn("A", function() {
                log = append(log, ["A1"]);
                yield("A");
                log = append(log, ["A2"]);
                yield("A");
                log = append(log, ["A3"]);
            });
            spawn("B", function() {
                log = append(log, ["B1"]);
                yield("B");
                log = append(log, ["B2"]);
            });
            run();
        """)
        assert e["log"] == ["A1", "B1", "A2", "B2", "A3"]

    def test_three_threads(self, e):
        """Three threads interleave in round-robin order."""
        e.eval("""
            log := [];
            spawn("A", function() {
                log = append(log, ["A"]);
                yield("A");
                log = append(log, ["A"]);
            });
            spawn("B", function() {
                log = append(log, ["B"]);
                yield("B");
                log = append(log, ["B"]);
            });
            spawn("C", function() {
                log = append(log, ["C"]);
                yield("C");
                log = append(log, ["C"]);
            });
            run();
        """)
        assert e["log"] == ["A", "B", "C", "A", "B", "C"]

    def test_no_yield_runs_sequentially(self, e):
        """Threads that don't yield run to completion in order."""
        e.eval("""
            log := [];
            spawn("A", function() {
                log = append(log, ["A1"]);
                log = append(log, ["A2"]);
            });
            spawn("B", function() {
                log = append(log, ["B1"]);
            });
            run();
        """)
        assert e["log"] == ["A1", "A2", "B1"]

    def test_single_thread_with_yields(self, e):
        """A single thread yielding just resumes itself."""
        e.eval("""
            log := [];
            spawn("solo", function() {
                log = append(log, [1]);
                yield("solo");
                log = append(log, [2]);
                yield("solo");
                log = append(log, [3]);
            });
            run();
        """)
        assert e["log"] == [1, 2, 3]

    def test_many_yields(self, e):
        """Threads with asymmetric yield counts."""
        e.eval("""
            log := [];
            spawn("A", function() {
                log = append(log, ["A1"]);
                yield("A");
                log = append(log, ["A2"]);
                yield("A");
                log = append(log, ["A3"]);
                yield("A");
                log = append(log, ["A4"]);
            });
            spawn("B", function() {
                log = append(log, ["B1"]);
                yield("B");
                log = append(log, ["B2"]);
            });
            run();
        """)
        assert e["log"] == ["A1", "B1", "A2", "B2", "A3", "A4"]


# =========================================================================
# Freeze + serialize
# =========================================================================

class TestFreezeAndSerialize:
    def test_freeze_returns_continuation(self, e):
        """freeze() returns the raw continuation on first call."""
        e.eval("k := freeze();")
        k = e.eval_raw("k;")
        # Calling k(42) resumes the callcc inside freeze, setting k := 42
        k(42)
        assert e["k"] == 42

    def test_freeze_serialize_roundtrip(self, e):
        """Freeze, serialize, deserialize in same context, resume."""
        e.eval("k := freeze();")
        k = e.eval_raw("k;")
        data = e.serialize_continuation(k)
        assert len(data) > 0

        k2 = e.deserialize_continuation(data)
        k2(99)
        assert e["k"] == 99

    def test_serialize_reusable_snapshot(self, e):
        """Same serialized bytes can resume in multiple contexts."""
        k = e.eval_raw("callcc(function(k) k);")
        data = e.serialize_continuation(k)

        results = []
        for i in range(5):
            ctx = Eval()
            k_restored = ctx.deserialize_continuation(data)
            results.append(k_restored(i * 10))

        assert results == [0, 10, 20, 30, 40]

    def test_freeze_mid_thread(self, e):
        """Freeze a thread mid-execution, serialize, resume."""
        e.eval("""
            log := [];
            frozen := false;

            spawn("worker", function() {
                log = append(log, ["step-1"]);
                yield("worker");
                log = append(log, ["step-2"]);
                frozen = freeze();
                // First time: frozen = continuation (truthy procedure)
                // Resume: frozen = whatever value was passed
            });

            spawn("helper", function() {
                log = append(log, ["helper"]);
            });

            run();
        """)
        log_before = e["log"]
        assert "step-1" in log_before
        assert "step-2" in log_before
        assert "helper" in log_before

        # Serialize the frozen continuation
        k = e.eval_raw("frozen;")
        data = e.serialize_continuation(k)
        assert len(data) > 0

        # Resume in same context — the continuation jumps back
        # into the thread, setting frozen := "woke-up"
        k2 = e.deserialize_continuation(data)
        k2("woke-up")
        assert e["frozen"] == "woke-up"

    def test_cross_context_continuation(self):
        """Serialize a continuation and resume in a fresh context."""
        e1 = Eval()
        k = e1.eval_raw("callcc(function(k) k);")
        data = e1.serialize_continuation(k)

        # Resume in a completely different context
        e2 = Eval()
        result = e2.deserialize_continuation(data)(42)
        assert result == 42

        # Same bytes, another context
        e3 = Eval()
        result2 = e3.deserialize_continuation(data)("hello")
        assert result2 == "hello"


# =========================================================================
# Dynamic wind (dk) correctness with green threads
# =========================================================================

class TestDynamicWind:
    def test_callcc_escape(self, e):
        """Escape continuation works correctly."""
        result = e.eval("""
            callcc(function(exit) {
                x := 10;
                y := 20;
                if(x + y == 30) exit("correct");
                "wrong";
            });
        """)
        assert result == "correct"

    def test_nested_callcc_serialize(self, e):
        """Nested continuations serialize and resume correctly."""
        e.eval("outer_k := false;")
        e.eval("""
            outer := callcc(function(k) {
                outer_k = k;
                "first";
            });
        """)
        assert e["outer"] == "first"

        k = e.eval_raw("outer_k;")
        data = e.serialize_continuation(k)
        k2 = e.deserialize_continuation(data)
        k2("second")
        assert e["outer"] == "second"

    def test_continuation_preserves_dk_identity(self):
        """The dynamic-wind point survives serialization (SER_CONTEXT_DK)."""
        e = Eval()
        # Capture, serialize, deserialize, resume — exercises the dk path
        k = e.eval_raw("callcc(function(k) k);")
        data = e.serialize_continuation(k)

        e2 = Eval()
        k2 = e2.deserialize_continuation(data)
        # If dk identity weren't preserved, this would crash with
        # "non procedure application" or "winding out of root!"
        assert k2(42) == 42


# =========================================================================
# Serialize/deserialize from within Eval
# =========================================================================

class TestEvalLevelSerialization:
    def test_serialize_deserialize_roundtrip(self, e):
        """serialize_continuation / deserialize_continuation from Eval code."""
        e.eval("""
            saved := false;
            val := callcc(function(k) {
                saved = k;
                "initial";
            });
        """)
        assert e["val"] == "initial"

        e.eval("""
            blob := serialize_continuation(saved);
            restored := deserialize_continuation(blob);
            restored("resumed");
        """)
        assert e["val"] == "resumed"

    def test_reuse_serialized_blob(self, e):
        """Same blob can be deserialized and resumed multiple times."""
        e.eval("""
            saved := false;
            val := callcc(function(k) {
                saved = k;
                0;
            });
        """)
        e.eval("blob := serialize_continuation(saved);")

        for word in ["one", "two", "three"]:
            e.eval(f"""
                k := deserialize_continuation(blob);
                k("{word}");
            """)
            assert e["val"] == word

    def test_serialize_freeze_from_eval(self, e):
        """Freeze, serialize, deserialize, resume — all from Eval."""
        e.eval("""
            saved := false;
            val := callcc(function(k) {
                saved = k;
                "before";
            });
        """)
        assert e["val"] == "before"

        e.eval("""
            blob := serialize_continuation(saved);
            k := deserialize_continuation(blob);
            k("after");
        """)
        assert e["val"] == "after"

    def test_serialize_frozen_thread(self, e):
        """Freeze a thread mid-scheduler, serialize, resume."""
        e.eval("""
            log := [];
            frozen := false;

            spawn("worker", function() {
                log = append(log, ["step-1"]);
                yield("worker");
                log = append(log, ["step-2"]);
                frozen = freeze();
                log = append(log, ["step-3"]);
            });

            spawn("helper", function() {
                log = append(log, ["helper"]);
            });

            run();
        """)
        assert "step-1" in e["log"]
        assert "step-2" in e["log"]
        assert "helper" in e["log"]

        # Serialize the frozen thread, deserialize, resume
        e.eval("""
            blob := serialize_continuation(frozen);
            k := deserialize_continuation(blob);
            k("resumed");
        """)
        assert e["frozen"] == "resumed"
        # step-3 appears again because the thread resumed past the freeze
        assert e["log"].count("step-3") == 2

    def test_serialize_error_non_procedure(self, e):
        """serialize_continuation on a non-procedure raises an error."""
        with pytest.raises(EvalError):
            e.eval("serialize_continuation(42);")

    def test_deserialize_error_non_bytevector(self, e):
        """deserialize_continuation on a non-bytevector raises an error."""
        with pytest.raises(EvalError):
            e.eval('deserialize_continuation("not bytes");')
