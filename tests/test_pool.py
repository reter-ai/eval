"""Tests for the C-level thread pool with channels."""

import time
import pytest
from chibi_eval import EvalPool, EvalError


class TestBasicSubmit:
    def test_simple_expression(self):
        with EvalPool(workers=2) as pool:
            f = pool.submit("1 + 2;")
            assert f.result() == 3

    def test_string_result(self):
        with EvalPool(workers=2) as pool:
            f = pool.submit('"hello";')
            assert f.result() == "hello"

    def test_boolean_result(self):
        with EvalPool(workers=2) as pool:
            f = pool.submit("3 > 2;")
            assert f.result() is True

    def test_list_result(self):
        with EvalPool(workers=2) as pool:
            f = pool.submit("[1, 2, 3];")
            assert f.result() == [1, 2, 3]

    def test_variable_binding(self):
        with EvalPool(workers=2) as pool:
            f = pool.submit("define x = 10; x * x;")
            assert f.result() == 100

    def test_function_definition(self):
        with EvalPool(workers=2) as pool:
            f = pool.submit("""
                define square = function(x) x * x;
                square(7);
            """)
            assert f.result() == 49

    def test_void_result(self):
        with EvalPool(workers=2) as pool:
            f = pool.submit("define x = 42;")
            # Void results should complete without error
            f.result()


class TestMap:
    def test_map_basic(self):
        with EvalPool(workers=4) as pool:
            results = pool.map([
                "10 + 1;",
                "20 + 2;",
                "30 + 3;",
                "40 + 4;",
            ])
            assert results == [11, 22, 33, 44]

    def test_map_computations(self):
        with EvalPool(workers=2) as pool:
            results = pool.map([
                "define fact = function(n) if(n <= 1) 1 else n * fact(n - 1); fact(10);",
                "define fib = function(n) if(n <= 1) n else fib(n-1) + fib(n-2); fib(10);",
            ])
            assert results == [3628800, 55]


class TestParallelSpeedup:
    def test_parallel_faster_than_serial(self):
        """Verify actual parallel execution by timing."""
        code = "define fib = function(n) if(n <= 1) n else fib(n-1) + fib(n-2); fib(28);"
        n_tasks = 4

        # Serial: run sequentially
        with EvalPool(workers=1) as pool:
            start = time.perf_counter()
            for _ in range(n_tasks):
                pool.submit(code).result()
            serial_time = time.perf_counter() - start

        # Parallel: run concurrently
        with EvalPool(workers=n_tasks) as pool:
            start = time.perf_counter()
            futures = [pool.submit(code) for _ in range(n_tasks)]
            for f in futures:
                f.result()
            parallel_time = time.perf_counter() - start

        # Parallel should be significantly faster
        assert parallel_time < serial_time * 0.8, (
            f"Parallel ({parallel_time:.2f}s) not faster enough than serial ({serial_time:.2f}s)"
        )


class TestChannel:
    def test_send_recv_integer(self):
        with EvalPool(workers=2) as pool:
            ch = pool.channel("test_int")
            pool.submit('channel_send(test_int, 42);')
            val = ch.recv()
            assert val == 42

    def test_send_recv_string(self):
        with EvalPool(workers=2) as pool:
            ch = pool.channel("test_str")
            pool.submit('channel_send(test_str, "hello world");')
            val = ch.recv()
            assert val == "hello world"

    def test_send_recv_list(self):
        with EvalPool(workers=2) as pool:
            ch = pool.channel("test_list")
            pool.submit('channel_send(test_list, [1, 2, 3]);')
            val = ch.recv()
            assert val == [1, 2, 3]

    def test_send_from_python(self):
        with EvalPool(workers=2) as pool:
            ch = pool.channel("from_py")
            ch.send(99)
            f = pool.submit("define x = channel_recv(from_py); x + 1;")
            assert f.result() == 100

    def test_multiple_values(self):
        with EvalPool(workers=2) as pool:
            ch = pool.channel("multi")
            pool.submit("""
                channel_send(multi, 1);
                channel_send(multi, 2);
                channel_send(multi, 3);
            """)
            vals = [ch.recv() for _ in range(3)]
            assert vals == [1, 2, 3]


class TestChannelPipeline:
    def test_producer_consumer(self):
        """Producer sends values, consumer receives and sums them."""
        with EvalPool(workers=3) as pool:
            input_ch = pool.channel("pipe_in")
            output_ch = pool.channel("pipe_out")

            # Producer: send numbers 1-5
            pool.submit("""
                define i = 0;
                while(i < 5) {
                    i++;
                    channel_send(pipe_in, i);
                };
            """)

            # Consumer: receive 5 values and sum them
            pool.submit("""
                define total = 0;
                define count = 0;
                define val = 0;
                while(count < 5) {
                    val = channel_recv(pipe_in);
                    total += val;
                    count++;
                };
                channel_send(pipe_out, total);
            """)

            result = output_ch.recv()
            assert result == 15  # 1+2+3+4+5


class TestErrorPropagation:
    def test_syntax_error(self):
        with EvalPool(workers=2) as pool:
            f = pool.submit("this is not valid eval code !!!")
            with pytest.raises(EvalError):
                f.result()

    def test_runtime_error(self):
        with EvalPool(workers=2) as pool:
            f = pool.submit('error("boom");')
            with pytest.raises(EvalError):
                f.result()

    def test_undefined_variable(self):
        with EvalPool(workers=2) as pool:
            f = pool.submit("undefined_var + 1;")
            with pytest.raises(EvalError):
                f.result()


class TestEvalPool:
    """Tests for the pure-Eval pool API (make_pool, pool_submit, etc.)."""

    def test_make_pool_submit(self):
        """Create pool from Eval, submit work, get result."""
        from chibi_eval import Eval
        e = Eval()
        result = e.eval("""
            define pool = make_pool(2);
            define f = pool_submit(pool, "1 + 2;");
            define r = future_result(f);
            pool_shutdown(pool);
            r;
        """)
        assert result == 3

    def test_pool_channel(self):
        """Create channel, send from main, recv in worker."""
        from chibi_eval import Eval
        e = Eval()
        result = e.eval("""
            define pool = make_pool(2);
            define ch = pool_channel(pool, "data");
            channel_send(ch, 42);
            define f = pool_submit(pool, "define x = channel_recv(data); x + 1;");
            define r = future_result(f);
            pool_shutdown(pool);
            r;
        """)
        assert result == 43

    def test_channel_try_recv_empty(self):
        """Non-blocking recv returns false when empty."""
        from chibi_eval import Eval
        e = Eval()
        result = e.eval("""
            define pool = make_pool(1);
            define ch = pool_channel(pool, "empty");
            define r = channel_try_recv(ch);
            pool_shutdown(pool);
            r;
        """)
        assert result is False

    def test_channel_try_recv_with_data(self):
        """Non-blocking recv returns (value) list when data available."""
        from chibi_eval import Eval
        e = Eval()
        result = e.eval("""
            define pool = make_pool(1);
            define ch = pool_channel(pool, "has_data");
            channel_send(ch, 99);
            define r = channel_try_recv(ch);
            pool_shutdown(pool);
            r;
        """)
        assert result == [99]

    def test_future_ready(self):
        """Check future readiness without blocking."""
        from chibi_eval import Eval
        e = Eval()
        result = e.eval("""
            define pool = make_pool(2);
            define f = pool_submit(pool, "1 + 1;");
            define r = future_result(f);
            define done = future_ready?(f);
            pool_shutdown(pool);
            done;
        """)
        assert result is True

    def test_pool_shutdown(self):
        """Explicit shutdown."""
        from chibi_eval import Eval
        e = Eval()
        # Should not raise
        e.eval("""
            define pool = make_pool(1);
            pool_shutdown(pool);
        """)

    def test_worker_to_main_channel(self):
        """Worker sends data back to main context via channel."""
        from chibi_eval import Eval
        e = Eval()
        result = e.eval("""
            define pool = make_pool(2);
            define ch = pool_channel(pool, "results");
            pool_submit(pool, "channel_send(results, 7 * 6);");
            define r = channel_recv(ch);
            pool_shutdown(pool);
            r;
        """)
        assert result == 42

    def test_multiple_submits(self):
        """Multiple submissions to the same pool."""
        from chibi_eval import Eval
        e = Eval()
        result = e.eval("""
            define pool = make_pool(2);
            define f1 = pool_submit(pool, "10 + 1;");
            define f2 = pool_submit(pool, "20 + 2;");
            define f3 = pool_submit(pool, "30 + 3;");
            define r = list(future_result(f1), future_result(f2), future_result(f3));
            pool_shutdown(pool);
            r;
        """)
        assert result == [11, 22, 33]


class TestPoolLifecycle:
    def test_context_manager(self):
        with EvalPool(workers=2) as pool:
            f = pool.submit("42;")
            assert f.result() == 42

    def test_multiple_pools(self):
        with EvalPool(workers=2) as pool1:
            with EvalPool(workers=2) as pool2:
                f1 = pool1.submit("1 + 1;")
                f2 = pool2.submit("2 + 2;")
                assert f1.result() == 2
                assert f2.result() == 4

    def test_many_submits(self):
        with EvalPool(workers=4) as pool:
            futures = [pool.submit(f"{i} * {i};") for i in range(20)]
            results = [f.result() for f in futures]
            expected = [i * i for i in range(20)]
            assert results == expected
