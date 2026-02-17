"""chibi-eval: Eval language interpreter with Python interop, powered by chibi-scheme."""

import ctypes
import os
import re
import sys
from concurrent.futures import Future
from threading import Thread

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
        e.eval("x = 42;")
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
        """Serialize a continuation to bytes."""
        try:
            return self._ctx.serialize_continuation(k)
        except (TypeError, ValueError, RuntimeError) as e:
            raise EvalError(str(e)) from None

    def deserialize_continuation(self, data: bytes) -> ChibiSexp:
        """Deserialize a continuation from bytes."""
        try:
            return self._ctx.deserialize_continuation(data)
        except (TypeError, ValueError, RuntimeError) as e:
            raise EvalError(str(e)) from None


# ================================================================
# Thread Pool
# ================================================================

def _get_pool_lib():
    """Load the C extension as a ctypes library for pool API access."""
    import chibi_eval._chibi as mod
    path = mod.__file__
    lib = ctypes.CDLL(path)

    # Pool lifecycle
    lib.eval_pool_create.argtypes = [ctypes.c_int, ctypes.c_char_p]
    lib.eval_pool_create.restype = ctypes.c_void_p
    lib.eval_pool_destroy.argtypes = [ctypes.c_void_p]
    lib.eval_pool_destroy.restype = None
    lib.eval_pool_load_prelude.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
    lib.eval_pool_load_prelude.restype = None

    # Submit
    lib.eval_pool_submit.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
    lib.eval_pool_submit.restype = ctypes.c_void_p
    lib.eval_future_wait_text.argtypes = [
        ctypes.c_void_p,
        ctypes.POINTER(ctypes.c_char_p),
        ctypes.POINTER(ctypes.c_char_p),
    ]
    lib.eval_future_wait_text.restype = ctypes.c_int
    lib.eval_future_free.argtypes = [ctypes.c_void_p]
    lib.eval_future_free.restype = None

    # Channels
    lib.eval_pool_channel.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
    lib.eval_pool_channel.restype = ctypes.c_void_p
    lib.eval_channel_send.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_size_t]
    lib.eval_channel_send.restype = None
    lib.eval_channel_recv.argtypes = [ctypes.c_void_p, ctypes.POINTER(ctypes.c_size_t)]
    lib.eval_channel_recv.restype = ctypes.c_char_p
    lib.eval_channel_recv_text.argtypes = [ctypes.c_void_p, ctypes.POINTER(ctypes.c_size_t)]
    lib.eval_channel_recv_text.restype = ctypes.c_char_p
    lib.eval_channel_close.argtypes = [ctypes.c_void_p]
    lib.eval_channel_close.restype = None

    return lib


def _parse_sexp_string(s):
    """Parse a sexp text representation back to a Python value."""
    if s is None:
        return None
    if isinstance(s, bytes):
        s = s.decode('utf-8', errors='replace')
    s = s.strip()
    if not s or s == '#<void>':
        return None
    if s == '#t':
        return True
    if s == '#f':
        return False
    # Integer
    try:
        return int(s)
    except ValueError:
        pass
    # Float
    try:
        return float(s)
    except ValueError:
        pass
    # String (quoted)
    if len(s) >= 2 and s[0] == '"' and s[-1] == '"':
        return s[1:-1].replace('\\"', '"').replace('\\\\', '\\')
    # List
    if s.startswith('(') and s.endswith(')'):
        return _parse_sexp_list(s)
    # Symbol or other
    return s


def _parse_sexp_list(s):
    """Parse a sexp list like (1 2 3) into a Python list."""
    inner = s[1:-1].strip()
    if not inner:
        return []
    result = []
    i = 0
    while i < len(inner):
        if inner[i] in ' \t\n\r':
            i += 1
            continue
        if inner[i] == '(':
            depth = 1
            j = i + 1
            while j < len(inner) and depth > 0:
                if inner[j] == '(':
                    depth += 1
                elif inner[j] == ')':
                    depth -= 1
                j += 1
            result.append(_parse_sexp_string(inner[i:j]))
            i = j
        elif inner[i] == '"':
            j = i + 1
            while j < len(inner):
                if inner[j] == '\\':
                    j += 2
                elif inner[j] == '"':
                    j += 1
                    break
                else:
                    j += 1
            result.append(_parse_sexp_string(inner[i:j]))
            i = j
        else:
            j = i
            while j < len(inner) and inner[j] not in ' \t\n\r()':
                j += 1
            result.append(_parse_sexp_string(inner[i:j]))
            i = j
    return result


class EvalFuture:
    """Wraps a C-level EvalFuture."""

    def __init__(self, lib, ptr):
        self._lib = lib
        self._ptr = ptr
        self._result = None
        self._error = None
        self._done = False

    def result(self, timeout=None):
        """Block until the result is available, then return it."""
        if self._done:
            if self._error:
                raise EvalError(self._error)
            return self._result

        result_p = ctypes.c_char_p()
        error_p = ctypes.c_char_p()
        self._lib.eval_future_wait_text(
            self._ptr,
            ctypes.byref(result_p),
            ctypes.byref(error_p),
        )
        self._done = True

        if error_p.value:
            self._error = error_p.value.decode('utf-8', errors='replace')
            raise EvalError(self._error)

        self._result = _parse_sexp_string(result_p.value)
        return self._result

    def __del__(self):
        if self._ptr and self._lib:
            self._lib.eval_future_free(self._ptr)
            self._ptr = None


class EvalChannel:
    """Wraps a C-level EvalChannel."""

    def __init__(self, lib, ptr, name):
        self._lib = lib
        self._ptr = ptr
        self.name = name

    def send(self, value):
        """Send a value through the channel (serialized as sexp text)."""
        if isinstance(value, str):
            data = f'"{value}"'.encode('utf-8')
        elif isinstance(value, bool):
            data = b'#t' if value else b'#f'
        elif isinstance(value, int):
            data = str(value).encode('utf-8')
        elif isinstance(value, float):
            data = str(value).encode('utf-8')
        elif isinstance(value, (list, tuple)):
            parts = []
            for item in value:
                if isinstance(item, str):
                    parts.append(f'"{item}"')
                elif isinstance(item, bool):
                    parts.append('#t' if item else '#f')
                else:
                    parts.append(str(item))
            data = ('(' + ' '.join(parts) + ')').encode('utf-8')
        else:
            data = str(value).encode('utf-8')
        self._lib.eval_channel_send(self._ptr, data, len(data))

    def recv(self):
        """Receive a value from the channel (blocks until available)."""
        out_len = ctypes.c_size_t()
        data = self._lib.eval_channel_recv_text(self._ptr, ctypes.byref(out_len))
        if data is None:
            return None  # channel closed
        return _parse_sexp_string(data)

    def close(self):
        """Close the channel."""
        self._lib.eval_channel_close(self._ptr)


class EvalPool:
    """C-level thread pool for parallel Eval execution.

    Each worker thread owns an independent chibi-scheme context.
    Communication between workers uses channels with sexp serialization.
    """

    def __init__(self, workers=None, prelude=None):
        if workers is None:
            workers = os.cpu_count() or 4

        # Ensure module paths are cached by creating a temporary Eval instance
        _tmp = Eval()
        del _tmp

        self._lib = _get_pool_lib()
        self._pool = self._lib.eval_pool_create(workers, None)
        self._channels = {}

        if prelude:
            self._lib.eval_pool_load_prelude(
                self._pool, prelude.encode('utf-8')
            )

    def submit(self, code: str) -> EvalFuture:
        """Submit Eval code for execution by a worker thread."""
        ptr = self._lib.eval_pool_submit(self._pool, code.encode('utf-8'))
        return EvalFuture(self._lib, ptr)

    def channel(self, name: str) -> EvalChannel:
        """Get or create a named channel shared across all workers."""
        if name in self._channels:
            return self._channels[name]
        ptr = self._lib.eval_pool_channel(self._pool, name.encode('utf-8'))
        ch = EvalChannel(self._lib, ptr, name)
        self._channels[name] = ch
        return ch

    def map(self, codes) -> list:
        """Submit multiple code strings and return all results."""
        futures = [self.submit(code) for code in codes]
        return [f.result() for f in futures]

    def shutdown(self):
        """Shut down the pool and wait for all workers to finish."""
        if self._pool:
            self._lib.eval_pool_destroy(self._pool)
            self._pool = None

    def __del__(self):
        self.shutdown()

    def __enter__(self):
        return self

    def __exit__(self, *args):
        self.shutdown()


__all__ = [
    'Eval', 'EvalError', 'EvalSyntaxError',
    'EvalPool', 'EvalChannel', 'EvalFuture',
    'ChibiContext', 'ChibiSexp',
]
