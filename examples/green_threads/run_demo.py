"""Green threads + continuation serialization demo.

Demonstrates:
  1. Cooperative multitasking via continuations (call/cc)
  2. Round-robin thread scheduling with yield
  3. Freezing a thread mid-execution (capturing its continuation)
  4. Serializing a frozen thread to bytes
  5. Resuming the frozen thread (same context & cross-context)
  6. Multiple resumes from the same serialized snapshot

Usage:
    python examples/green_threads/run_demo.py
"""

import os
from chibi_eval import Eval

HERE = os.path.dirname(os.path.abspath(__file__))


def make_eval():
    """Create an Eval instance with the scheduler loaded."""
    e = Eval()
    e.load(os.path.join(HERE, "green_threads_demo.eval"))
    return e


# ─────────────────────────────────────────────────────────────────
# Demo 1: Cooperative scheduling
# ─────────────────────────────────────────────────────────────────

print("=== Demo 1: Cooperative scheduling ===")
print("Two threads interleave via yield:\n")

e = make_eval()
e.eval("""
    log := [];
    spawn("counter", function() {
        i := 0;
        while(i < 3) {
            log = append(log, [`string-append`("counter: ", `number->string`(i))]);
            yield("counter");
            i = i + 1;
        };
    });
    spawn("greeter", function() {
        names := ["Alice", "Bob"];
        j := 0;
        while(j < length(names)) {
            log = append(log, [`string-append`("hello ", `list-ref`(names, j))]);
            yield("greeter");
            j = j + 1;
        };
    });
    run();
""")

for entry in e["log"]:
    print(f"  {entry}")


# ─────────────────────────────────────────────────────────────────
# Demo 2: Freeze + serialize + resume
# ─────────────────────────────────────────────────────────────────

print("\n=== Demo 2: Freeze + serialize + resume ===")
print("Worker freezes mid-execution, serialized to bytes, resumed:\n")

e = make_eval()
e.eval("""
    log := [];
    frozen := false;

    spawn("worker", function() {
        log = append(log, ["step-1"]);
        yield("worker");
        log = append(log, ["step-2"]);

        frozen = freeze();

        if(`procedure?`(frozen)) {
            log = append(log, ["** frozen at checkpoint **"]);
        } else {
            log = append(log, [`string-append`("** resumed with: ", frozen, " **")]);
        };

        log = append(log, ["step-3"]);
    });

    spawn("observer", function() {
        log = append(log, ["observer: watching"]);
        yield("observer");
        log = append(log, ["observer: done"]);
    });

    run();
""")

for entry in e["log"]:
    print(f"  {entry}")

# Serialize the frozen continuation
k = e.eval_raw("frozen;")
data = e.serialize_continuation(k)
print(f"\n  Serialized to {len(data)} bytes")

# Deserialize and resume in the same context
print("  Resuming in same context...")
k2 = e.deserialize_continuation(data)
k2("RESTORED")

print(f"  frozen = {e['frozen']!r}")
print(f"  log[-2:] = {e['log'][-2:]}")


# ─────────────────────────────────────────────────────────────────
# Demo 3: Cross-context resume
# ─────────────────────────────────────────────────────────────────

print("\n=== Demo 3: Cross-context resume ===")
print("Continuation serialized in one context, resumed in another:\n")

e1 = Eval()
k = e1.eval_raw("callcc(function(k) k);")
data = e1.serialize_continuation(k)
print(f"  Serialized continuation: {len(data)} bytes")

for val in [42, "hello", 99]:
    e2 = Eval()
    k2 = e2.deserialize_continuation(data)
    result = k2(val)
    print(f"  Resume with {val!r} -> {result!r}")


# ─────────────────────────────────────────────────────────────────
# Demo 4: Reusable snapshot (multiple resumes from same bytes)
# ─────────────────────────────────────────────────────────────────

print("\n=== Demo 4: Reusable snapshot ===")
print("Same serialized bytes, resumed 5 times:\n")

e = Eval()
k = e.eval_raw("callcc(function(k) k);")
data = e.serialize_continuation(k)

results = []
for i in range(5):
    ctx = Eval()
    k_restored = ctx.deserialize_continuation(data)
    results.append(k_restored(i * 10))

print(f"  Results: {results}")


# ─────────────────────────────────────────────────────────────────
# Demo 5: Serialize/deserialize from within Eval
# ─────────────────────────────────────────────────────────────────

print("\n=== Demo 5: Serialize/deserialize from Eval code ===")
print("Using serialize_continuation / deserialize_continuation builtins:\n")

e = Eval()
e.eval("""
    saved := false;
    val := callcc(function(k) {
        saved = k;
        "initial";
    });
""")
print(f"  val = {e['val']!r}")

e.eval("""
    blob := serialize_continuation(saved);
    restored := deserialize_continuation(blob);
    restored("resumed");
""")
print(f"  val = {e['val']!r}  (after resume)")

# Resume again with different value
e.eval("""
    restored2 := deserialize_continuation(blob);
    restored2("again");
""")
print(f"  val = {e['val']!r}  (after second resume)")

print("\n=== All demos complete ===")
