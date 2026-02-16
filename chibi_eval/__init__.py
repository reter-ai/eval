"""chibi-eval: Eval language interpreter with Python interop, powered by chibi-scheme."""

from chibi_eval._chibi import ChibiContext, ChibiSexp


class EvalError(Exception):
    """Base exception for Eval language errors."""
    pass


class EvalSyntaxError(EvalError):
    """Raised when Eval code has a syntax error."""
    pass


class Eval:
    """Eval language interpreter with Python interop.

    Usage:
        e = Eval()
        e.eval("x := 42;")
        print(e["x"])  # 42
    """

    def __init__(self, heap_size=0, max_heap_size=0):
        self._ctx = ChibiContext(heap_size=heap_size, max_heap_size=max_heap_size)

    def eval(self, code: str) -> object:
        """Evaluate Eval code and return the result as a Python object."""
        try:
            return self._ctx.eval(code)
        except SyntaxError as e:
            raise EvalSyntaxError(str(e)) from None
        except RuntimeError as e:
            raise EvalError(str(e)) from None

    def eval_raw(self, code: str) -> ChibiSexp:
        """Evaluate Eval code and return the result as a raw ChibiSexp."""
        try:
            return self._ctx.eval_raw(code)
        except SyntaxError as e:
            raise EvalSyntaxError(str(e)) from None
        except RuntimeError as e:
            raise EvalError(str(e)) from None

    def __getitem__(self, name: str) -> object:
        """Look up a variable value: e['x']"""
        try:
            return self._ctx.lookup(name)
        except KeyError:
            raise KeyError(name)

    def __setitem__(self, name: str, value):
        """Define a variable: e['x'] = 42"""
        self._ctx.define(name, value)

    def define_function(self, name: str, func, arity: int = -1):
        """Register a Python callable as an Eval function.

        Args:
            name: Function name in Eval
            func: Python callable
            arity: Number of arguments (-1 for variadic)
        """
        self._ctx.define_function(name, func, arity)

    def call(self, proc_name: str, *args) -> object:
        """Call a named Eval procedure with Python arguments."""
        proc = self._ctx.eval_raw(proc_name + ";")
        result = proc(*args)
        return result

    def load(self, filename: str):
        """Load and evaluate an Eval source file."""
        with open(filename, 'r') as f:
            code = f.read()
        return self.eval(code)

    def serialize_continuation(self, k: ChibiSexp) -> bytes:
        """Serialize a continuation to bytes.

        Args:
            k: A ChibiSexp continuation captured via callcc.

        Returns:
            bytes containing the serialized continuation.

        Raises:
            EvalError: If the continuation cannot be serialized.
        """
        try:
            return self._ctx.serialize_continuation(k)
        except (TypeError, ValueError, RuntimeError) as e:
            raise EvalError(str(e)) from None

    def deserialize_continuation(self, data: bytes) -> ChibiSexp:
        """Deserialize a continuation from bytes.

        Args:
            data: bytes previously returned by serialize_continuation.

        Returns:
            A callable ChibiSexp continuation.

        Raises:
            EvalError: If the data is invalid or globals cannot be resolved.
        """
        try:
            return self._ctx.deserialize_continuation(data)
        except (TypeError, ValueError, RuntimeError) as e:
            raise EvalError(str(e)) from None


__all__ = ['Eval', 'EvalError', 'EvalSyntaxError', 'ChibiContext', 'ChibiSexp']
