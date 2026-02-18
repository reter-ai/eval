"""SICP examples converted to Eval syntax and tested.

Every testable example from Structure and Interpretation of Computer Programs
(Abelson & Sussman) is converted from Scheme to Eval syntax and verified.
"""

import pytest
import math
from chibi_eval import Eval, EvalError



# ============================================================================
# Chapter 1.1 — The Elements of Programming
# ============================================================================

class TestSICP_1_1_Expressions:
    """1.1.1 Expressions"""

    def test_number(self, e):
        assert e.eval("486;") == 486

    def test_add(self, e):
        assert e.eval("137 + 349;") == 486

    def test_sub(self, e):
        assert e.eval("1000 - 334;") == 666

    def test_mul(self, e):
        assert e.eval("5 * 99;") == 495

    def test_div(self, e):
        assert e.eval("10 / 5;") == 2

    def test_add_float(self, e):
        assert abs(e.eval("2.7 + 10;") - 12.7) < 0.001

    def test_multi_add(self, e):
        # (+ 21 35 12 7) → 75 — multi-arg add
        assert e.eval("21 + 35 + 12 + 7;") == 75

    def test_multi_mul(self, e):
        assert e.eval("25 * 4 * 12;") == 1200

    def test_nested(self, e):
        # (+ (* 3 5) (- 10 6)) → 19
        assert e.eval("3 * 5 + (10 - 6);") == 19

    def test_complex_nested(self, e):
        # (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6)) → 57
        assert e.eval("3 * (2 * 4 + (3 + 5)) + ((10 - 7) + 6);") == 57


class TestSICP_1_1_Naming:
    """1.1.2 Naming and the Environment"""

    def test_define_size(self, e):
        e.eval("define size = 2;")
        assert e["size"] == 2
        assert e.eval("5 * size;") == 10

    def test_pi_radius(self, e):
        e.eval("define pi = 3.14159;")
        e.eval("define radius = 10;")
        result = e.eval("pi * (radius * radius);")
        assert abs(result - 314.159) < 0.01

    def test_circumference(self, e):
        e.eval("define pi = 3.14159;")
        e.eval("define radius = 10;")
        e.eval("define circumference = 2 * pi * radius;")
        assert abs(e["circumference"] - 62.8318) < 0.01


class TestSICP_1_1_CompoundProcedures:
    """1.1.4 Compound Procedures"""

    def test_square(self, e):
        e.eval("define square = function(x) x * x;")
        assert e.eval("square(21);") == 441
        assert e.eval("square(2 + 5);") == 49
        assert e.eval("square(square(3));") == 81

    def test_sum_of_squares(self, e):
        e.eval("define square = function(x) x * x;")
        e.eval("define sum_of_squares = function(x, y) square(x) + square(y);")
        assert e.eval("sum_of_squares(3, 4);") == 25

    def test_f_composition(self, e):
        e.eval("define square = function(x) x * x;")
        e.eval("define sum_of_squares = function(x, y) square(x) + square(y);")
        e.eval("define f = function(a) sum_of_squares(a + 1, a * 2);")
        assert e.eval("f(5);") == 136


class TestSICP_1_1_Conditionals:
    """1.1.6 Conditional Expressions and Predicates"""

    def test_abs_cond(self, e):
        e.eval("""define abs_val = function(x)
            cond(x > 0: x, x == 0: 0, x < 0: -x);""")
        assert e.eval("abs_val(5);") == 5
        assert e.eval("abs_val(0);") == 0
        assert e.eval("abs_val(-3);") == 3

    def test_abs_if(self, e):
        e.eval("define abs_val = function(x) if(x < 0) -x else x;")
        assert e.eval("abs_val(-5);") == 5
        assert e.eval("abs_val(3);") == 3

    def test_exercise_1_1(self, e):
        """Exercise 1.1 — sequence of expressions"""
        assert e.eval("10;") == 10
        assert e.eval("5 + 3 + 4;") == 12
        assert e.eval("9 - 1;") == 8
        assert e.eval("6 / 2;") == 3
        assert e.eval("2 * 4 + (4 - 6);") == 6

        e.eval("define a = 3;")
        e.eval("define b = a + 1;")
        assert e.eval("a + b + a * b;") == 19
        assert e.eval("a == b;") is False

        # (if (and (> b a) (< b (* a b))) b a) → 4
        assert e.eval("if((b > a) && (b < a * b)) b else a;") == 4

        # (cond ((= a 4) 6) ((= b 4) (+ 6 7 a)) (else 25)) → 16
        assert e.eval("cond(a == 4: 6, b == 4: 6 + 7 + a, else: 25);") == 16

        # (+ 2 (if (> b a) b a)) → 6
        assert e.eval("2 + if(b > a) b else a;") == 6

        # (* (cond ((> a b) a) ((< a b) b) (else -1)) (+ a 1)) → 16
        assert e.eval("cond(a > b: a, a < b: b, else: -1) * (a + 1);") == 16


class TestSICP_1_1_Sqrt:
    """1.1.7 Example: Square Roots by Newton's Method"""

    def test_sqrt_newton(self, e):
        e.eval("define square = function(x) x * x;")
        e.eval("define average = function(x, y) (x + y) / 2;")
        e.eval("define abs_val = function(x) if(x < 0) -x else x;")
        e.eval("""define good_enough = function(guess, x)
            abs_val(square(guess) - x) < 0.001;""")
        e.eval("""define improve = function(guess, x)
            average(guess, x / guess);""")
        e.eval("""define sqrt_iter = function(guess, x)
            if(good_enough(guess, x)) guess
            else sqrt_iter(improve(guess, x), x);""")
        e.eval("define my_sqrt = function(x) sqrt_iter(1.0, x);")

        result = e.eval("my_sqrt(9);")
        assert abs(result - 3.0) < 0.01

        result = e.eval("my_sqrt(137);")
        assert abs(result - math.sqrt(137)) < 0.01

        result = e.eval("square(my_sqrt(1000));")
        assert abs(result - 1000.0) < 0.01

    def test_sqrt_block_structure(self, e):
        """1.1.8 Block structure — sqrt with lexical scoping"""
        e.eval("define square = function(x) x * x;")
        e.eval("define average = function(x, y) (x + y) / 2;")
        e.eval("define abs_val = function(x) if(x < 0) -x else x;")
        # Using closures for block structure
        e.eval("""define my_sqrt = function(x) {
            define good_enough = function(guess)
                abs_val(square(guess) - x) < 0.001;
            define improve = function(guess)
                average(guess, x / guess);
            define sqrt_iter = function(guess)
                if(good_enough(guess)) guess
                else sqrt_iter(improve(guess));
            sqrt_iter(1.0);
        };""")
        assert abs(e.eval("my_sqrt(9);") - 3.0) < 0.01
        assert abs(e.eval("my_sqrt(2);") - math.sqrt(2)) < 0.01


# ============================================================================
# Chapter 1.2 — Procedures and the Processes They Generate
# ============================================================================

class TestSICP_1_2_Factorial:
    """1.2.1 Linear Recursion and Iteration"""

    def test_factorial_recursive(self, e):
        e.eval("""define factorial = function(n)
            if(n == 1) 1 else n * factorial(n - 1);""")
        assert e.eval("factorial(1);") == 1
        assert e.eval("factorial(6);") == 720
        assert e.eval("factorial(10);") == 3628800

    def test_factorial_iterative(self, e):
        e.eval("""define fact_iter = function(product, counter, max_count)
            if(counter > max_count) product
            else fact_iter(counter * product, counter + 1, max_count);""")
        e.eval("define factorial = function(n) fact_iter(1, 1, n);")
        assert e.eval("factorial(6);") == 720
        assert e.eval("factorial(10);") == 3628800


class TestSICP_1_2_Fibonacci:
    """1.2.2 Tree Recursion"""

    def test_fib_recursive(self, e):
        e.eval("""define fib = function(n)
            cond(n == 0: 0, n == 1: 1,
                 else: fib(n - 1) + fib(n - 2));""")
        assert e.eval("fib(0);") == 0
        assert e.eval("fib(1);") == 1
        assert e.eval("fib(5);") == 5
        assert e.eval("fib(10);") == 55

    def test_fib_iterative(self, e):
        e.eval("""define fib_iter = function(a, b, count)
            if(count == 0) b
            else fib_iter(a + b, a, count - 1);""")
        e.eval("define fib = function(n) fib_iter(1, 0, n);")
        assert e.eval("fib(0);") == 0
        assert e.eval("fib(1);") == 1
        assert e.eval("fib(10);") == 55
        assert e.eval("fib(20);") == 6765


class TestSICP_1_2_CountChange:
    """1.2.2 Counting change"""

    def test_count_change(self, e):
        e.eval("""define first_denomination = function(kinds)
            cond(kinds == 1: 1, kinds == 2: 5,
                 kinds == 3: 10, kinds == 4: 25,
                 kinds == 5: 50);""")
        e.eval("""define cc = function(amount, kinds)
            cond(amount == 0: 1,
                 (amount < 0) || (kinds == 0): 0,
                 else: cc(amount, kinds - 1) +
                       cc(amount - first_denomination(kinds), kinds));""")
        e.eval("define count_change = function(amount) cc(amount, 5);")
        assert e.eval("count_change(100);") == 292


class TestSICP_1_2_Ackermann:
    """Exercise 1.10 — Ackermann's function"""

    def test_ackermann(self, e):
        e.eval("""define A = function(x, y)
            cond(y == 0: 0,
                 x == 0: 2 * y,
                 y == 1: 2,
                 else: A(x - 1, A(x, y - 1)));""")
        assert e.eval("A(1, 10);") == 1024
        assert e.eval("A(2, 4);") == 65536
        assert e.eval("A(3, 3);") == 65536


class TestSICP_1_2_Exponentiation:
    """1.2.4 Exponentiation"""

    def test_expt_recursive(self, e):
        e.eval("""define my_expt = function(b, n)
            if(n == 0) 1 else b * my_expt(b, n - 1);""")
        assert e.eval("my_expt(2, 10);") == 1024
        assert e.eval("my_expt(3, 5);") == 243

    def test_expt_iterative(self, e):
        e.eval("""define expt_iter = function(b, counter, product)
            if(counter == 0) product
            else expt_iter(b, counter - 1, b * product);""")
        e.eval("define my_expt = function(b, n) expt_iter(b, n, 1);")
        assert e.eval("my_expt(2, 10);") == 1024

    def test_fast_expt(self, e):
        e.eval("define square = function(x) x * x;")
        e.eval("define even = function(n) n % 2 == 0;")
        e.eval("""define fast_expt = function(b, n)
            cond(n == 0: 1,
                 even(n): square(fast_expt(b, n / 2)),
                 else: b * fast_expt(b, n - 1));""")
        assert e.eval("fast_expt(2, 10);") == 1024
        assert e.eval("fast_expt(2, 20);") == 1048576
        assert e.eval("fast_expt(3, 10);") == 59049


class TestSICP_1_2_GCD:
    """1.2.5 Greatest Common Divisors"""

    def test_gcd(self, e):
        e.eval("""define my_gcd = function(a, b)
            if(b == 0) a else my_gcd(b, a % b);""")
        assert e.eval("my_gcd(206, 40);") == 2
        assert e.eval("my_gcd(16, 28);") == 4
        assert e.eval("my_gcd(48, 18);") == 6


class TestSICP_1_2_Primality:
    """1.2.6 Testing for Primality"""

    def test_smallest_divisor(self, e):
        e.eval("define square = function(x) x * x;")
        e.eval("define divides = function(a, b) b % a == 0;")
        e.eval("""define find_divisor = function(n, test)
            cond(square(test) > n: n,
                 divides(test, n): test,
                 else: find_divisor(n, test + 1));""")
        e.eval("define smallest_divisor = function(n) find_divisor(n, 2);")
        e.eval("define prime = function(n) n == smallest_divisor(n);")

        # Exercise 1.21
        assert e.eval("smallest_divisor(199);") == 199
        assert e.eval("smallest_divisor(1999);") == 1999
        assert e.eval("smallest_divisor(19999);") == 7

        assert e.eval("prime(199);") is True
        assert e.eval("prime(1999);") is True
        assert e.eval("prime(4);") is False
        assert e.eval("prime(15);") is False

    def test_expmod(self, e):
        e.eval("define square = function(x) x * x;")
        e.eval("define even = function(n) n % 2 == 0;")
        e.eval("""define expmod = function(base, exp, m)
            cond(exp == 0: 1,
                 even(exp): square(expmod(base, exp / 2, m)) % m,
                 else: (base * expmod(base, exp - 1, m)) % m);""")
        # Fermat's little theorem: a^p ≡ a (mod p) for prime p
        assert e.eval("expmod(2, 7, 7);") == 2
        assert e.eval("expmod(3, 7, 7);") == 3
        assert e.eval("expmod(2, 11, 11);") == 2


class TestSICP_1_2_Sine:
    """Exercise 1.15 — sine computation"""

    def test_sine(self, e):
        e.eval("define cube = function(x) x * x * x;")
        e.eval("define p = function(x) 3 * x - 4 * cube(x);")
        e.eval("define abs_val = function(x) if(x < 0) -x else x;")
        e.eval("""define sine = function(angle)
            if(!(abs_val(angle) > 0.1)) angle
            else p(sine(angle / 3.0));""")
        result = e.eval("sine(12.15);")
        assert abs(result - math.sin(12.15)) < 0.01


# ============================================================================
# Chapter 1.3 — Formulating Abstractions with Higher-Order Procedures
# ============================================================================

class TestSICP_1_3_SumAbstraction:
    """1.3.1 Procedures as Arguments"""

    def test_sum_integers(self, e):
        e.eval("""define sum_integers = function(a, b)
            if(a > b) 0 else a + sum_integers(a + 1, b);""")
        assert e.eval("sum_integers(1, 10);") == 55

    def test_sum_cubes(self, e):
        e.eval("define cube = function(x) x * x * x;")
        e.eval("""define sum_cubes = function(a, b)
            if(a > b) 0 else cube(a) + sum_cubes(a + 1, b);""")
        assert e.eval("sum_cubes(1, 10);") == 3025

    def test_higher_order_sum(self, e):
        """General sum with higher-order procedures"""
        e.eval("define cube = function(x) x * x * x;")
        e.eval("""define sum = function(term, a, next, b)
            if(a > b) 0
            else term(a) + sum(term, next(a), next, b);""")
        e.eval("define inc = function(n) n + 1;")
        e.eval("define identity = function(x) x;")

        # sum-cubes using sum
        e.eval("define sum_cubes = function(a, b) sum(cube, a, inc, b);")
        assert e.eval("sum_cubes(1, 10);") == 3025

        # sum-integers using sum
        e.eval("define sum_integers = function(a, b) sum(identity, a, inc, b);")
        assert e.eval("sum_integers(1, 10);") == 55

    def test_pi_sum(self, e):
        e.eval("""define sum = function(term, a, next, b)
            if(a > b) 0
            else term(a) + sum(term, next(a), next, b);""")
        e.eval("""define pi_sum = function(a, b) {
            define pi_term = function(x) 1.0 / (x * (x + 2));
            define pi_next = function(x) x + 4;
            sum(pi_term, a, pi_next, b);
        };""")
        result = e.eval("8 * pi_sum(1, 1000);")
        assert abs(result - math.pi) < 0.01

    def test_integral(self, e):
        e.eval("define cube = function(x) x * x * x;")
        e.eval("""define sum = function(term, a, next, b)
            if(a > b) 0
            else term(a) + sum(term, next(a), next, b);""")
        e.eval("""define integral = function(f, a, b, dx) {
            define add_dx = function(x) x + dx;
            sum(f, a + dx / 2.0, add_dx, b) * dx;
        };""")
        result = e.eval("integral(cube, 0, 1, 0.01);")
        assert abs(result - 0.25) < 0.01
        result = e.eval("integral(cube, 0, 1, 0.001);")
        assert abs(result - 0.25) < 0.001


class TestSICP_1_3_Lambda:
    """1.3.2 Constructing Procedures Using Lambda"""

    def test_lambda_call(self, e):
        e.eval("define square = function(x) x * x;")
        # ((lambda (x y z) (+ x y (square z))) 1 2 3) → 12
        result = e.eval("(function(x, y, z) x + y + square(z))(1, 2, 3);")
        assert result == 12

    def test_let_bindings(self, e):
        e.eval("define square = function(x) x * x;")
        # f(x,y) = x(1+xy)^2 + y(1-y) + (1+xy)(1-y)
        e.eval("""define f = function(x, y)
            let(a = 1 + x * y, b = 1 - y)
                x * square(a) + y * b + a * b;""")
        # f(1,2) = 1*(1+2)^2 + 2*(1-2) + (1+2)*(1-2) = 9 - 2 - 3 = 4
        assert e.eval("f(1, 2);") == 4

    def test_exercise_1_34(self, e):
        """Exercise 1.34"""
        e.eval("define square = function(x) x * x;")
        e.eval("define f = function(g) g(2);")
        assert e.eval("f(square);") == 4
        assert e.eval("f(function(z) z * (z + 1));") == 6


class TestSICP_1_3_FixedPoint:
    """1.3.3 Procedures as General Methods"""

    def test_fixed_point_cos(self, e):
        e.eval("define abs_val = function(x) if(x < 0) -x else x;")
        e.eval("define tolerance = 0.00001;")
        e.eval("""define fixed_point = function(f, first_guess) {
            define close_enough = function(v1, v2)
                abs_val(v1 - v2) < tolerance;
            define try_ = function(guess) {
                define next = f(guess);
                if(close_enough(guess, next)) next
                else try_(next);
            };
            try_(first_guess);
        };""")

        # cos fixed point ≈ 0.7390822985224023
        e.define_function("cos_fn", lambda x: math.cos(x), 1)
        result = e.eval("fixed_point(cos_fn, 1.0);")
        assert abs(result - 0.7390822985) < 0.0001

    def test_fixed_point_sqrt(self, e):
        e.eval("define abs_val = function(x) if(x < 0) -x else x;")
        e.eval("define average = function(x, y) (x + y) / 2;")
        e.eval("define tolerance = 0.00001;")
        e.eval("""define fixed_point = function(f, first_guess) {
            define close_enough = function(v1, v2)
                abs_val(v1 - v2) < tolerance;
            define try_ = function(guess) {
                define next = f(guess);
                if(close_enough(guess, next)) next
                else try_(next);
            };
            try_(first_guess);
        };""")

        # sqrt via average damping
        e.eval("""define my_sqrt = function(x)
            fixed_point(function(y) average(y, x / y), 1.0);""")
        result = e.eval("my_sqrt(2);")
        assert abs(result - math.sqrt(2)) < 0.001

    def test_golden_ratio(self, e):
        """Exercise 1.35 — golden ratio as fixed point of x → 1 + 1/x"""
        e.eval("define abs_val = function(x) if(x < 0) -x else x;")
        e.eval("define tolerance = 0.00001;")
        e.eval("""define fixed_point = function(f, first_guess) {
            define close_enough = function(v1, v2)
                abs_val(v1 - v2) < tolerance;
            define try_ = function(guess) {
                define next = f(guess);
                if(close_enough(guess, next)) next
                else try_(next);
            };
            try_(first_guess);
        };""")
        result = e.eval("fixed_point(function(x) 1 + 1 / x, 1.0);")
        golden = (1 + math.sqrt(5)) / 2
        assert abs(result - golden) < 0.001


class TestSICP_1_3_ReturnedProcedures:
    """1.3.4 Procedures as Returned Values"""

    def test_average_damp(self, e):
        e.eval("define square = function(x) x * x;")
        e.eval("define average = function(x, y) (x + y) / 2;")
        e.eval("""define average_damp = function(f)
            function(x) average(x, f(x));""")
        # ((average-damp square) 10) → 55
        assert e.eval("average_damp(square)(10);") == 55

    def test_derivative(self, e):
        e.eval("define cube = function(x) x * x * x;")
        e.eval("define dx = 0.00001;")
        e.eval("""define deriv = function(g)
            function(x) (g(x + dx) - g(x)) / dx;""")
        # derivative of x^3 at x=5 should be ~75
        result = e.eval("deriv(cube)(5);")
        assert abs(result - 75.0) < 0.01

    def test_newtons_method_sqrt(self, e):
        e.eval("define square = function(x) x * x;")
        e.eval("define abs_val = function(x) if(x < 0) -x else x;")
        e.eval("define average = function(x, y) (x + y) / 2;")
        e.eval("define dx = 0.00001;")
        e.eval("define tolerance = 0.00001;")
        e.eval("""define deriv = function(g)
            function(x) (g(x + dx) - g(x)) / dx;""")
        e.eval("""define fixed_point = function(f, first_guess) {
            define close_enough = function(v1, v2)
                abs_val(v1 - v2) < tolerance;
            define try_ = function(guess) {
                define next = f(guess);
                if(close_enough(guess, next)) next
                else try_(next);
            };
            try_(first_guess);
        };""")
        e.eval("""define newton_transform = function(g)
            function(x) x - g(x) / deriv(g)(x);""")
        e.eval("""define newtons_method = function(g, guess)
            fixed_point(newton_transform(g), guess);""")

        # sqrt via Newton's method: zero of y^2 - x
        e.eval("""define my_sqrt = function(x)
            newtons_method(function(y) square(y) - x, 1.0);""")
        result = e.eval("my_sqrt(2);")
        assert abs(result - math.sqrt(2)) < 0.001

    def test_compose(self, e):
        """Exercise 1.42 — function composition"""
        e.eval("define square = function(x) x * x;")
        e.eval("define inc = function(x) x + 1;")
        e.eval("""define compose = function(f, g)
            function(x) f(g(x));""")
        assert e.eval("compose(square, inc)(6);") == 49

    def test_repeated(self, e):
        """Exercise 1.43 — repeated application"""
        e.eval("define square = function(x) x * x;")
        e.eval("""define compose = function(f, g)
            function(x) f(g(x));""")
        e.eval("""define repeated = function(f, n)
            if(n == 1) f
            else compose(f, repeated(f, n - 1));""")
        # ((repeated square 2) 5) → 625
        assert e.eval("repeated(square, 2)(5);") == 625

    def test_cube_root(self, e):
        e.eval("define square = function(x) x * x;")
        e.eval("define abs_val = function(x) if(x < 0) -x else x;")
        e.eval("define average = function(x, y) (x + y) / 2;")
        e.eval("define tolerance = 0.00001;")
        e.eval("""define fixed_point = function(f, first_guess) {
            define close_enough = function(v1, v2)
                abs_val(v1 - v2) < tolerance;
            define try_ = function(guess) {
                define next = f(guess);
                if(close_enough(guess, next)) next
                else try_(next);
            };
            try_(first_guess);
        };""")
        e.eval("""define average_damp = function(f)
            function(x) average(x, f(x));""")
        e.eval("""define cube_root = function(x)
            fixed_point(average_damp(function(y) x / square(y)), 1.0);""")
        result = e.eval("cube_root(27);")
        assert abs(result - 3.0) < 0.001
        result = e.eval("cube_root(8);")
        assert abs(result - 2.0) < 0.001


# ============================================================================
# Chapter 2.1 — Introduction to Data Abstraction
# ============================================================================

class TestSICP_2_1_RationalNumbers:
    """2.1.1 Rational number arithmetic using cons/car/cdr"""

    def test_rational_arithmetic(self, e):
        # Implement rational numbers using pairs (lists)
        e.eval("define abs_val = function(x) if(x < 0) -x else x;")
        e.eval("""define my_gcd = function(a, b)
            if(b == 0) a else my_gcd(b, a % b);""")
        e.eval("""define make_rat = function(n, d) {
            define g = my_gcd(abs_val(n), abs_val(d));
            [n / g, d / g];
        };""")
        e.eval("define numer = function(x) car(x);")
        e.eval("define denom = function(x) car(cdr(x));")

        e.eval("""define add_rat = function(x, y)
            make_rat(numer(x) * denom(y) + numer(y) * denom(x),
                     denom(x) * denom(y));""")
        e.eval("""define mul_rat = function(x, y)
            make_rat(numer(x) * numer(y),
                     denom(x) * denom(y));""")

        # 1/2 + 1/3 = 5/6
        e.eval("define result = add_rat(make_rat(1, 2), make_rat(1, 3));")
        assert e.eval("numer(result);") == 5
        assert e.eval("denom(result);") == 6

        # 2/3 * 3/4 = 1/2
        e.eval("define result2 = mul_rat(make_rat(2, 3), make_rat(3, 4));")
        assert e.eval("numer(result2);") == 1
        assert e.eval("denom(result2);") == 2


class TestSICP_2_1_Pairs:
    """2.1.3 What is meant by data? — procedural pairs"""

    def test_church_pairs(self, e):
        """Church encoding of pairs (procedural representation)"""
        e.eval("""define my_cons = function(x, y)
            function(m) if(m == 0) x else y;""")
        e.eval("define my_car = function(z) z(0);")
        e.eval("define my_cdr = function(z) z(1);")

        e.eval("define p = my_cons(1, 2);")
        assert e.eval("my_car(p);") == 1
        assert e.eval("my_cdr(p);") == 2

        e.eval("define q = my_cons(3, my_cons(4, 5));")
        assert e.eval("my_car(q);") == 3
        assert e.eval("my_car(my_cdr(q));") == 4
        assert e.eval("my_cdr(my_cdr(q));") == 5


# ============================================================================
# Chapter 2.2 — Hierarchical Data (selected examples)
# ============================================================================

class TestSICP_2_2_ListOps:
    """2.2.1 Representing Sequences — list operations"""

    def test_list_ref(self, e):
        e.eval("""define list_ref = function(items, n)
            if(n == 0) car(items)
            else list_ref(cdr(items), n - 1);""")
        e.eval("define squares = [1, 4, 9, 16, 25];")
        assert e.eval("list_ref(squares, 3);") == 16

    def test_length(self, e):
        e.eval("""define my_length = function(items)
            if(null?(items)) 0
            else 1 + my_length(cdr(items));""")
        assert e.eval("my_length([1, 3, 5, 7]);") == 4

    def test_append(self, e):
        e.eval("""define my_append = function(list1, list2)
            if(null?(list1)) list2
            else cons(car(list1), my_append(cdr(list1), list2));""")
        result = e.eval("my_append([1, 2, 3], [4, 5, 6]);")
        assert result == [1, 2, 3, 4, 5, 6]

    def test_map(self, e):
        # Using built-in map
        e.eval("define square = function(x) x * x;")
        result = e.eval("map(square, [1, 2, 3, 4, 5]);")
        assert result == [1, 4, 9, 16, 25]

    def test_scale_list(self, e):
        e.eval("""define scale_list = function(items, factor)
            map(function(x) x * factor, items);""")
        result = e.eval("scale_list([1, 2, 3, 4, 5], 10);")
        assert result == [10, 20, 30, 40, 50]


class TestSICP_2_2_Trees:
    """2.2.2 Hierarchical Structures (trees)"""

    def test_count_leaves(self, e):
        e.eval("""define count_leaves = function(x)
            cond(null?(x): 0,
                 !(pair?(x)): 1,
                 else: count_leaves(car(x)) + count_leaves(cdr(x)));""")
        # (list 1 (list 2 3)) → proper scheme list
        # In Eval: [1, [2, 3]] is a flat Eval list, not a tree
        # For a tree we need nested cons cells
        assert e.eval("count_leaves([1, 2, 3, 4]);") == 4


class TestSICP_2_2_Accumulate:
    """2.2.3 Sequences as Conventional Interfaces"""

    def test_accumulate(self, e):
        e.eval("""define accumulate = function(op, initial, sequence)
            if(null?(sequence)) initial
            else op(car(sequence),
                    accumulate(op, initial, cdr(sequence)));""")
        assert e.eval('accumulate(+, 0, [1, 2, 3, 4, 5]);') == 15
        assert e.eval('accumulate(*, 1, [1, 2, 3, 4, 5]);') == 120


# ============================================================================
# Chapter 2.3 — Symbolic Data
# ============================================================================

class TestSICP_2_3_Symbolic:
    """2.3.1 Quotation / 2.3.3 Sets"""

    def test_memq(self, e):
        """memq — find symbol in list"""
        e.eval("""define memq = function(item, x)
            cond(null?(x): false,
                 item =? car(x): x,
                 else: memq(item, cdr(x)));""")
        result = e.eval("memq('apple, ['x, 'apple, 'banana]);")
        assert isinstance(result, list)
        assert result[0] == "apple"

    def test_equal(self, e):
        """Testing equality of lists"""
        e.eval("""define my_equal = function(a, b)
            cond(!(pair?(a)) && !(pair?(b)): a =? b,
                 pair?(a) && pair?(b):
                     my_equal(car(a), car(b)) && my_equal(cdr(a), cdr(b)),
                 else: false);""")
        assert e.eval("my_equal([1, 2, 3], [1, 2, 3]);") is True
        assert e.eval("my_equal([1, 2], [1, 3]);") is False


# ============================================================================
# Chapter 3.1 — Assignment and Local State
# ============================================================================

class TestSICP_3_1_Assignment:
    """3.1 Assignment and Local State"""

    def test_make_account(self, e):
        """Bank account with local state via closures"""
        e.eval("""define make_account = function(balance) {
            define withdraw = function(amount)
                if(balance >= amount) {
                    balance -= amount;
                    balance;
                } else error("Insufficient funds");
            define deposit = function(amount) {
                balance += amount;
                balance;
            };
            define dispatch = function(m)
                cond(m == 'withdraw: withdraw,
                     m == 'deposit: deposit,
                     else: error("Unknown request"));
            dispatch;
        };""")
        e.eval("define acc = make_account(100);")

        # withdraw 50 → 50
        assert e.eval("acc('withdraw)(50);") == 50
        # withdraw 60 → error (only 50 left)
        # deposit 40 → 90
        assert e.eval("acc('deposit)(40);") == 90
        # withdraw 60 → 30
        assert e.eval("acc('withdraw)(60);") == 30


class TestSICP_3_1_Accumulator:
    """Exercise 3.1 — Accumulator"""

    def test_make_accumulator(self, e):
        e.eval("""define make_accumulator = function(initial) {
            define total = initial;
            function(amount) {
                total += amount;
                total;
            };
        };""")
        e.eval("define acc = make_accumulator(5);")
        assert e.eval("acc(10);") == 15
        assert e.eval("acc(10);") == 25


# ============================================================================
# Additional SICP patterns
# ============================================================================

class TestSICP_Misc:
    """Miscellaneous SICP patterns"""

    def test_pascal_triangle(self, e):
        """Exercise 1.12 — Pascal's triangle"""
        e.eval("""define pascal = function(row, col)
            if(col == 0 || col == row) 1
            else pascal(row - 1, col - 1) + pascal(row - 1, col);""")
        assert e.eval("pascal(0, 0);") == 1
        assert e.eval("pascal(4, 2);") == 6
        assert e.eval("pascal(4, 1);") == 4

    def test_sum_of_squares_of_larger_two(self, e):
        """Exercise 1.3 — sum of squares of two larger numbers"""
        e.eval("define square = function(x) x * x;")
        e.eval("""define sum_sq_larger_two = function(a, b, c)
            cond(a <= b && a <= c: square(b) + square(c),
                 b <= a && b <= c: square(a) + square(c),
                 else: square(a) + square(b));""")
        assert e.eval("sum_sq_larger_two(1, 2, 3);") == 13
        assert e.eval("sum_sq_larger_two(3, 1, 2);") == 13
        assert e.eval("sum_sq_larger_two(3, 2, 1);") == 13

    def test_iterative_improve(self, e):
        """Exercise 1.46 — iterative improvement"""
        e.eval("define abs_val = function(x) if(x < 0) -x else x;")
        e.eval("""define iterative_improve = function(good_enough, improve)
            function(guess) {
                define try_ = function(g)
                    if(good_enough(g)) g
                    else try_(improve(g));
                try_(guess);
            };""")
        # sqrt using iterative_improve
        e.eval("""define my_sqrt = function(x)
            iterative_improve(
                function(guess) abs_val(guess * guess - x) < 0.001,
                function(guess) (guess + x / guess) / 2
            )(1.0);""")
        result = e.eval("my_sqrt(9);")
        assert abs(result - 3.0) < 0.01

    def test_exercise_1_11_recursive(self, e):
        """Exercise 1.11 — f(n) = n if n<3, else f(n-1) + 2f(n-2) + 3f(n-3)"""
        e.eval("""define f = function(n)
            if(n < 3) n
            else f(n - 1) + 2 * f(n - 2) + 3 * f(n - 3);""")
        assert e.eval("f(0);") == 0
        assert e.eval("f(1);") == 1
        assert e.eval("f(2);") == 2
        assert e.eval("f(3);") == 4
        assert e.eval("f(5);") == 25

    def test_exercise_1_11_iterative(self, e):
        """Exercise 1.11 — iterative version"""
        e.eval("""define f_iter = function(a, b, c, count)
            if(count == 0) a
            else f_iter(a + 2 * b + 3 * c, a, b, count - 1);""")
        e.eval("define f = function(n) if(n < 3) n else f_iter(2, 1, 0, n - 2);")
        assert e.eval("f(0);") == 0
        assert e.eval("f(3);") == 4
        assert e.eval("f(5);") == 25
        assert e.eval("f(10);") == 1892

    def test_product_higher_order(self, e):
        """Exercise 1.31 — product abstraction"""
        e.eval("""define product = function(term, a, next, b)
            if(a > b) 1
            else term(a) * product(term, next(a), next, b);""")
        e.eval("define identity = function(x) x;")
        e.eval("define inc = function(x) x + 1;")
        # factorial using product
        e.eval("define factorial = function(n) product(identity, 1, inc, n);")
        assert e.eval("factorial(5);") == 120
        assert e.eval("factorial(10);") == 3628800

    def test_cont_frac(self, e):
        """Exercise 1.37 — continued fractions"""
        e.eval("""define cont_frac = function(n, d, k) {
            define recur = function(i)
                if(i > k) 0
                else n(i) / (d(i) + recur(i + 1));
            recur(1);
        };""")
        # 1/φ ≈ 0.6180
        result = e.eval("cont_frac(function(i) 1.0, function(i) 1.0, 20);")
        golden_inv = 2.0 / (1 + math.sqrt(5))
        assert abs(result - golden_inv) < 0.0001

    def test_euler_e(self, e):
        """Exercise 1.38 — Euler's continued fraction for e"""
        e.eval("""define cont_frac = function(n, d, k) {
            define recur = function(i)
                if(i > k) 0
                else n(i) / (d(i) + recur(i + 1));
            recur(1);
        };""")
        # D_i: 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ...
        e.eval("""define euler_d = function(i)
            if(i % 3 == 2) ((i + 1) / 3) * 2
            else 1;""")
        result = e.eval("2 + cont_frac(function(i) 1.0, euler_d, 20);")
        assert abs(result - math.e) < 0.0001


# ============================================================================
# Chapter 2.2 — More Hierarchical Data examples
# ============================================================================

class TestSICP_2_2_MoreListOps:
    """2.2.1 Additional list operations"""

    def test_last_pair(self, e):
        """Exercise 2.17 — last-pair"""
        e.eval("""define last_pair = function(items)
            if(null?(cdr(items))) items
            else last_pair(cdr(items));""")
        result = e.eval("last_pair([23, 72, 149, 34]);")
        assert result == [34]

    def test_reverse(self, e):
        """Exercise 2.18 — reverse"""
        e.eval("""define my_reverse = function(items) {
            define iter = function(remaining, result)
                if(null?(remaining)) result
                else iter(cdr(remaining), cons(car(remaining), result));
            iter(items, []);
        };""")
        assert e.eval("my_reverse([1, 4, 9, 16, 25]);") == [25, 16, 9, 4, 1]
        assert e.eval("my_reverse([1, 2, 3]);") == [3, 2, 1]

    def test_length_iterative(self, e):
        """2.2.1 — iterative length"""
        e.eval("""define my_length = function(items) {
            define length_iter = function(a, count)
                if(null?(a)) count
                else length_iter(cdr(a), 1 + count);
            length_iter(items, 0);
        };""")
        assert e.eval("my_length([1, 3, 5, 7]);") == 4
        assert e.eval("my_length([]);") == 0

    def test_for_each(self, e):
        """Exercise 2.23 — for-each"""
        # We can test for-each by using it to accumulate into a variable
        e.eval("""define my_for_each = function(f, items)
            if(null?(items)) true
            else { f(car(items)); my_for_each(f, cdr(items)); };""")
        e.eval("define total = 0;")
        e.eval("my_for_each(function(x) total += x, [1, 2, 3, 4]);")
        assert e["total"] == 10


class TestSICP_2_2_SequenceOps:
    """2.2.3 Sequences as Conventional Interfaces"""

    def test_filter(self, e):
        """filter — select elements by predicate"""
        e.eval("""define my_filter = function(predicate, sequence)
            cond(null?(sequence): [],
                 predicate(car(sequence)):
                     cons(car(sequence),
                          my_filter(predicate, cdr(sequence))),
                 else: my_filter(predicate, cdr(sequence)));""")
        e.eval("define odd = function(n) n % 2 == 1;")
        result = e.eval("my_filter(odd, [1, 2, 3, 4, 5]);")
        assert result == [1, 3, 5]

    def test_enumerate_interval(self, e):
        """2.2.3 — enumerate-interval"""
        e.eval("""define enumerate_interval = function(low, high)
            if(low > high) []
            else cons(low, enumerate_interval(low + 1, high));""")
        assert e.eval("enumerate_interval(2, 7);") == [2, 3, 4, 5, 6, 7]
        assert e.eval("enumerate_interval(1, 5);") == [1, 2, 3, 4, 5]

    def test_accumulate_cons(self, e):
        """accumulate with cons — identity on lists"""
        e.eval("""define accumulate = function(op, initial, sequence)
            if(null?(sequence)) initial
            else op(car(sequence),
                    accumulate(op, initial, cdr(sequence)));""")
        result = e.eval("accumulate(cons, [], [1, 2, 3, 4, 5]);")
        assert result == [1, 2, 3, 4, 5]

    def test_flatmap(self, e):
        """2.2.3 — flatmap"""
        e.eval("""define accumulate = function(op, initial, sequence)
            if(null?(sequence)) initial
            else op(car(sequence),
                    accumulate(op, initial, cdr(sequence)));""")
        e.eval("""define my_append = function(list1, list2)
            if(null?(list1)) list2
            else cons(car(list1), my_append(cdr(list1), list2));""")
        e.eval("""define flatmap = function(proc, seq)
            accumulate(my_append, [], map(proc, seq));""")
        e.eval("""define enumerate_interval = function(low, high)
            if(low > high) []
            else cons(low, enumerate_interval(low + 1, high));""")

        # Generate pairs (i, j) for 1 <= j < i <= 4
        result = e.eval("""flatmap(
            function(i) map(function(j) [i, j],
                            enumerate_interval(1, i - 1)),
            enumerate_interval(1, 4));""")
        expected = [[2,1], [3,1], [3,2], [4,1], [4,2], [4,3]]
        assert result == expected

    def test_fold_left(self, e):
        """Exercise 2.38 — fold-left"""
        e.eval("""define fold_left = function(op, initial, sequence) {
            define iter = function(result, rest)
                if(null?(rest)) result
                else iter(op(result, car(rest)), cdr(rest));
            iter(initial, sequence);
        };""")
        # fold-left + 0 (1 2 3 4 5) → 15
        assert e.eval('fold_left(+, 0, [1, 2, 3, 4, 5]);') == 15
        # fold-left * 1 (1 2 3 4 5) → 120
        assert e.eval('fold_left(*, 1, [1, 2, 3, 4, 5]);') == 120

    def test_reverse_with_fold(self, e):
        """Exercise 2.39 — reverse via fold-left"""
        e.eval("""define fold_left = function(op, initial, sequence) {
            define iter = function(result, rest)
                if(null?(rest)) result
                else iter(op(result, car(rest)), cdr(rest));
            iter(initial, sequence);
        };""")
        e.eval("""define my_reverse = function(sequence)
            fold_left(function(x, y) cons(y, x), [], sequence);""")
        assert e.eval("my_reverse([1, 2, 3, 4, 5]);") == [5, 4, 3, 2, 1]

    def test_horner_eval(self, e):
        """Exercise 2.34 — Horner's rule for polynomial evaluation"""
        e.eval("""define accumulate = function(op, initial, sequence)
            if(null?(sequence)) initial
            else op(car(sequence),
                    accumulate(op, initial, cdr(sequence)));""")
        e.eval("""define horner_eval = function(x, coefficient_sequence)
            accumulate(
                function(this_coeff, higher_terms)
                    this_coeff + x * higher_terms,
                0,
                coefficient_sequence);""")
        # 1 + 3x + 5x^3 + x^5 at x=2
        # = 1 + 6 + 40 + 32 = 79
        assert e.eval("horner_eval(2, [1, 3, 0, 5, 0, 1]);") == 79

    def test_product_of_squares_of_odd(self, e):
        """2.2.3 — product-of-squares-of-odd-elements"""
        e.eval("""define my_filter = function(predicate, sequence)
            cond(null?(sequence): [],
                 predicate(car(sequence)):
                     cons(car(sequence),
                          my_filter(predicate, cdr(sequence))),
                 else: my_filter(predicate, cdr(sequence)));""")
        e.eval("""define accumulate = function(op, initial, sequence)
            if(null?(sequence)) initial
            else op(car(sequence),
                    accumulate(op, initial, cdr(sequence)));""")
        e.eval("define odd = function(n) n % 2 == 1;")
        e.eval("define square = function(x) x * x;")
        e.eval("""define product_of_squares_of_odd = function(sequence)
            accumulate(*, 1, map(square, my_filter(odd, sequence)));""")
        # odd elements of [1,2,3,4,5] = [1,3,5], squares = [1,9,25], product = 225
        assert e.eval("product_of_squares_of_odd([1, 2, 3, 4, 5]);") == 225


class TestSICP_2_2_ScaleTree:
    """2.2.2 — scale-tree using map"""

    def test_scale_list_with_map(self, e):
        """scale-list using map"""
        e.eval("""define scale_list = function(items, factor)
            map(function(x) x * factor, items);""")
        assert e.eval("scale_list([1, 2, 3, 4, 5], 10);") == [10, 20, 30, 40, 50]

    def test_square_list(self, e):
        """Exercise 2.21 — square-list using map"""
        e.eval("define square = function(x) x * x;")
        e.eval("define square_list = function(items) map(square, items);")
        assert e.eval("square_list([1, 2, 3, 4]);") == [1, 4, 9, 16]


class TestSICP_2_2_Remove:
    """2.2.3 — remove and permutations"""

    def test_remove(self, e):
        e.eval("""define my_filter = function(predicate, sequence)
            cond(null?(sequence): [],
                 predicate(car(sequence)):
                     cons(car(sequence),
                          my_filter(predicate, cdr(sequence))),
                 else: my_filter(predicate, cdr(sequence)));""")
        e.eval("""define my_remove = function(item, sequence)
            my_filter(function(x) !(x == item), sequence);""")
        assert e.eval("my_remove(3, [1, 2, 3, 4, 5]);") == [1, 2, 4, 5]
        assert e.eval("my_remove(1, [1, 2, 3]);") == [2, 3]


# ============================================================================
# Chapter 2.3 — More Symbolic Data
# ============================================================================

class TestSICP_2_3_Sets:
    """2.3.3 Sets as unordered lists"""

    def test_element_of_set(self, e):
        e.eval("""define element_of_set = function(x, set)
            cond(null?(set): false,
                 x == car(set): true,
                 else: element_of_set(x, cdr(set)));""")
        assert e.eval("element_of_set(3, [1, 2, 3, 4]);") is True
        assert e.eval("element_of_set(5, [1, 2, 3, 4]);") is False

    def test_adjoin_set(self, e):
        e.eval("""define element_of_set = function(x, set)
            cond(null?(set): false,
                 x == car(set): true,
                 else: element_of_set(x, cdr(set)));""")
        e.eval("""define adjoin_set = function(x, set)
            if(element_of_set(x, set)) set
            else cons(x, set);""")
        assert e.eval("adjoin_set(5, [1, 2, 3]);") == [5, 1, 2, 3]
        assert e.eval("adjoin_set(2, [1, 2, 3]);") == [1, 2, 3]

    def test_intersection_set(self, e):
        e.eval("""define element_of_set = function(x, set)
            cond(null?(set): false,
                 x == car(set): true,
                 else: element_of_set(x, cdr(set)));""")
        e.eval("""define intersection_set = function(set1, set2)
            cond(null?(set1) || null?(set2): [],
                 element_of_set(car(set1), set2):
                     cons(car(set1),
                          intersection_set(cdr(set1), set2)),
                 else: intersection_set(cdr(set1), set2));""")
        result = e.eval("intersection_set([1, 2, 3, 4], [2, 4, 6, 8]);")
        assert result == [2, 4]

    def test_union_set(self, e):
        """Exercise 2.59 — union-set"""
        e.eval("""define element_of_set = function(x, set)
            cond(null?(set): false,
                 x == car(set): true,
                 else: element_of_set(x, cdr(set)));""")
        e.eval("""define adjoin_set = function(x, set)
            if(element_of_set(x, set)) set
            else cons(x, set);""")
        e.eval("""define union_set = function(set1, set2)
            if(null?(set1)) set2
            else adjoin_set(car(set1), union_set(cdr(set1), set2));""")
        result = e.eval("union_set([1, 2, 3], [2, 3, 4]);")
        # Should contain 1, 2, 3, 4 (order may vary)
        assert sorted(result) == [1, 2, 3, 4]


class TestSICP_2_3_OrderedSets:
    """2.3.3 Sets as ordered lists"""

    def test_element_of_set_ordered(self, e):
        e.eval("""define element_of_set_ord = function(x, set)
            cond(null?(set): false,
                 x == car(set): true,
                 x < car(set): false,
                 else: element_of_set_ord(x, cdr(set)));""")
        assert e.eval("element_of_set_ord(3, [1, 2, 3, 4, 5]);") is True
        assert e.eval("element_of_set_ord(3, [1, 2, 4, 5, 6]);") is False

    def test_intersection_set_ordered(self, e):
        """2.3.3 O(n) intersection for ordered sets"""
        e.eval("""define intersection_set_ord = function(set1, set2)
            if(null?(set1) || null?(set2)) []
            else let(x1 = car(set1), x2 = car(set2))
                cond(x1 == x2: cons(x1, intersection_set_ord(cdr(set1), cdr(set2))),
                     x1 < x2: intersection_set_ord(cdr(set1), set2),
                     else: intersection_set_ord(set1, cdr(set2)));""")
        result = e.eval("intersection_set_ord([1, 3, 5, 7, 9], [2, 3, 5, 8, 9]);")
        assert result == [3, 5, 9]


class TestSICP_2_3_BinaryTrees:
    """2.3.3 Sets as binary trees"""

    def test_binary_tree_operations(self, e):
        """Binary tree entry/left/right/make-tree + element-of-set? + adjoin-set"""
        e.eval("define entry = function(tree) car(tree);")
        e.eval("define left_branch = function(tree) car(cdr(tree));")
        e.eval("define right_branch = function(tree) car(cdr(cdr(tree)));")
        e.eval("define make_tree = function(entry, left, right) [entry, left, right];")

        e.eval("""define element_of_set_tree = function(x, set)
            cond(null?(set): false,
                 x == entry(set): true,
                 x < entry(set): element_of_set_tree(x, left_branch(set)),
                 else: element_of_set_tree(x, right_branch(set)));""")

        e.eval("""define adjoin_set_tree = function(x, set)
            cond(null?(set): make_tree(x, [], []),
                 x == entry(set): set,
                 x < entry(set):
                     make_tree(entry(set),
                               adjoin_set_tree(x, left_branch(set)),
                               right_branch(set)),
                 else:
                     make_tree(entry(set),
                               left_branch(set),
                               adjoin_set_tree(x, right_branch(set))));""")

        # Build tree: 5, then add 3, 7, 1
        e.eval("define t = make_tree(5, [], []);")
        e.eval("t = adjoin_set_tree(3, t);")
        e.eval("t = adjoin_set_tree(7, t);")
        e.eval("t = adjoin_set_tree(1, t);")

        assert e.eval("element_of_set_tree(3, t);") is True
        assert e.eval("element_of_set_tree(7, t);") is True
        assert e.eval("element_of_set_tree(1, t);") is True
        assert e.eval("element_of_set_tree(4, t);") is False
        assert e.eval("entry(t);") == 5


# ============================================================================
# Chapter 3.1 — More Assignment examples
# ============================================================================

class TestSICP_3_1_MakeWithdraw:
    """3.1.1 — make-withdraw"""

    def test_make_withdraw(self, e):
        e.eval("""define make_withdraw = function(balance)
            function(amount)
                if(balance >= amount) {
                    balance -= amount;
                    balance;
                } else "Insufficient funds";""")
        e.eval("define W1 = make_withdraw(100);")
        e.eval("define W2 = make_withdraw(100);")

        assert e.eval("W1(50);") == 50
        assert e.eval("W2(70);") == 30
        # W1 has 50, W2 has 30
        assert e.eval("W1(40);") == 10

    def test_make_simplified_withdraw(self, e):
        """3.1.3 — make-simplified-withdraw (no insufficient check)"""
        e.eval("""define make_simplified_withdraw = function(balance)
            function(amount) {
                balance -= amount;
                balance;
            };""")
        e.eval("define W = make_simplified_withdraw(25);")
        assert e.eval("W(20);") == 5
        assert e.eval("W(10);") == -5

    def test_make_decrementer(self, e):
        """3.1.3 — make-decrementer (no mutation — pure)"""
        e.eval("""define make_decrementer = function(balance)
            function(amount) balance - amount;""")
        e.eval("define D = make_decrementer(25);")
        assert e.eval("D(20);") == 5
        assert e.eval("D(10);") == 15  # No state change


class TestSICP_3_1_MonteCarloGCD:
    """3.1.2 — Monte Carlo simulation (GCD-based pi estimate)"""

    def test_monte_carlo_framework(self, e):
        """Test the Monte Carlo framework with a simple experiment"""
        e.eval("""define monte_carlo = function(trials, experiment) {
            define iter = function(trials_remaining, trials_passed)
                cond(trials_remaining == 0:
                         trials_passed,
                     experiment():
                         iter(trials_remaining - 1, trials_passed + 1),
                     else:
                         iter(trials_remaining - 1, trials_passed));
            iter(trials, 0);
        };""")
        # Test with an always-true experiment
        assert e.eval("monte_carlo(100, function() true);") == 100
        # Test with an always-false experiment
        assert e.eval("monte_carlo(100, function() false);") == 0


class TestSICP_3_1_ImperativeFactorial:
    """3.1.3 — Imperative-style factorial"""

    def test_imperative_factorial(self, e):
        """Factorial with explicit mutation (imperative style)"""
        e.eval("""define factorial = function(n) {
            define product = 1;
            define counter = 1;
            define iter = function() {
                if(counter > n) product
                else {
                    product = counter * product;
                    counter += 1;
                    iter();
                };
            };
            iter();
        };""")
        assert e.eval("factorial(1);") == 1
        assert e.eval("factorial(5);") == 120
        assert e.eval("factorial(10);") == 3628800


# ============================================================================
# More exercises and patterns
# ============================================================================

class TestSICP_Exercise_2_20:
    """Exercise 2.20 — same-parity (requires variadic args)
    We test the concept using a list as input."""

    def test_same_parity(self, e):
        e.eval("""define same_parity = function(items) {
            define first = car(items);
            define parity = first % 2;
            define my_filter = function(lst)
                if(null?(lst)) []
                else if(car(lst) % 2 == parity)
                    cons(car(lst), my_filter(cdr(lst)))
                else my_filter(cdr(lst));
            my_filter(items);
        };""")
        assert e.eval("same_parity([1, 2, 3, 4, 5, 6, 7]);") == [1, 3, 5, 7]
        assert e.eval("same_parity([2, 3, 4, 5, 6, 7]);") == [2, 4, 6]


class TestSICP_Exercise_1_37_38:
    """Additional continued fraction tests"""

    def test_tan_cf(self, e):
        """Exercise 1.39 — Lambert's continued fraction for tan"""
        e.eval("""define cont_frac = function(n, d, k) {
            define recur = function(i)
                if(i > k) 0
                else n(i) / (d(i) + recur(i + 1));
            recur(1);
        };""")
        e.eval("""define tan_cf = function(x, k)
            cont_frac(
                function(i) if(i == 1) x else -(x * x),
                function(i) 2 * i - 1,
                k);""")
        result = e.eval("tan_cf(1.0, 20);")
        assert abs(result - math.tan(1.0)) < 0.0001


class TestSICP_Exercise_1_44:
    """Exercise 1.44 — smoothing a function"""

    def test_smooth(self, e):
        e.eval("define dx = 0.001;")
        e.eval("""define smooth = function(f)
            function(x) (f(x - dx) + f(x) + f(x + dx)) / 3;""")
        e.eval("define square = function(x) x * x;")
        # Smooth of square at 5 ≈ 25 (very close since dx is small)
        result = e.eval("smooth(square)(5);")
        assert abs(result - 25.0) < 0.01


class TestSICP_Exercise_1_29:
    """Exercise 1.29 — Simpson's rule for numerical integration"""

    def test_simpsons_rule(self, e):
        e.eval("define cube = function(x) x * x * x;")
        e.eval("""define sum = function(term, a, next, b)
            if(a > b) 0
            else term(a) + sum(term, next(a), next, b);""")
        e.eval("""define simpsons_rule = function(f, a, b, n) {
            define h = (b - a) / n;
            define yk = function(k) f(a + k * h);
            define coeff = function(k)
                cond(k == 0 || k == n: 1,
                     k % 2 == 1: 4,
                     else: 2);
            define term = function(k) coeff(k) * yk(k);
            define inc = function(k) k + 1;
            (h / 3) * sum(term, 0, inc, n);
        };""")
        # integral of x^3 from 0 to 1 = 0.25
        result = e.eval("simpsons_rule(cube, 0.0, 1.0, 100);")
        assert abs(result - 0.25) < 0.0001


class TestSICP_Exercise_2_19:
    """Exercise 2.19 — count-change with coin lists"""

    def test_cc_with_coin_lists(self, e):
        e.eval("define no_more = function(coins) null?(coins);")
        e.eval("define first_denomination = function(coins) car(coins);")
        e.eval("define except_first = function(coins) cdr(coins);")
        e.eval("""define cc = function(amount, coin_values)
            cond(amount == 0: 1,
                 (amount < 0) || no_more(coin_values): 0,
                 else: cc(amount, except_first(coin_values)) +
                       cc(amount - first_denomination(coin_values), coin_values));""")
        e.eval("define us_coins = [50, 25, 10, 5, 1];")
        assert e.eval("cc(100, us_coins);") == 292


class TestSICP_Exercise_1_16:
    """Exercise 1.16 — fast exponentiation (iterative)"""

    def test_fast_expt_iter(self, e):
        e.eval("define even = function(n) n % 2 == 0;")
        e.eval("""define fast_expt_iter = function(b, n, a)
            cond(n == 0: a,
                 even(n): fast_expt_iter(b * b, n / 2, a),
                 else: fast_expt_iter(b, n - 1, a * b));""")
        e.eval("define fast_expt = function(b, n) fast_expt_iter(b, n, 1);")
        assert e.eval("fast_expt(2, 10);") == 1024
        assert e.eval("fast_expt(3, 10);") == 59049
        assert e.eval("fast_expt(2, 0);") == 1


class TestSICP_Exercise_1_33:
    """Exercise 1.33 — filtered-accumulate"""

    def test_filtered_accumulate(self, e):
        e.eval("define square = function(x) x * x;")
        e.eval("define inc = function(n) n + 1;")
        e.eval("define divides = function(a, b) b % a == 0;")
        e.eval("""define find_divisor = function(n, test)
            cond(square(test) > n: n,
                 divides(test, n): test,
                 else: find_divisor(n, test + 1));""")
        e.eval("define smallest_divisor = function(n) find_divisor(n, 2);")
        e.eval("define prime = function(n) if(n < 2) false else n == smallest_divisor(n);")

        e.eval("""define filtered_accumulate = function(combiner, null_value, term, a, next, b, filter_fn)
            if(a > b) null_value
            else if(filter_fn(a))
                combiner(term(a), filtered_accumulate(combiner, null_value, term, next(a), next, b, filter_fn))
            else filtered_accumulate(combiner, null_value, term, next(a), next, b, filter_fn);""")

        # Sum of squares of primes from 2 to 10: 4+9+25+49 = 87
        result = e.eval('filtered_accumulate(+, 0, square, 2, inc, 10, prime);')
        assert result == 4 + 9 + 25 + 49  # 2^2 + 3^2 + 5^2 + 7^2 = 87


class TestSICP_Exercise_1_32:
    """Exercise 1.32 — accumulate (generalization of sum and product)"""

    def test_accumulate_general(self, e):
        e.eval("define inc = function(x) x + 1;")
        e.eval("define identity = function(x) x;")
        e.eval("""define accumulate = function(combiner, null_value, term, a, next, b)
            if(a > b) null_value
            else combiner(term(a), accumulate(combiner, null_value, term, next(a), next, b));""")
        # sum 1..10
        assert e.eval('accumulate(+, 0, identity, 1, inc, 10);') == 55
        # product 1..5 (factorial)
        assert e.eval('accumulate(*, 1, identity, 1, inc, 5);') == 120


# ============================================================================
# Chapter 2.2 — Additional sequence and tree operations
# ============================================================================

class TestSICP_2_2_EnumerateTree:
    """2.2.3 — enumerate-tree (flattening a tree)"""

    def test_enumerate_tree(self, e):
        e.eval("""define enumerate_tree = function(tree)
            cond(null?(tree): [],
                 !pair?(tree): [tree],
                 else: append(enumerate_tree(car(tree)),
                              enumerate_tree(cdr(tree))));""")
        assert e.eval("enumerate_tree([1, [2, [3, 4]], 5]);") == [1, 2, 3, 4, 5]
        assert e.eval("enumerate_tree([]);") == []
        assert e.eval("enumerate_tree([1]);") == [1]

    def test_count_leaves_with_accumulate(self, e):
        """Exercise 2.35 — count-leaves using accumulate"""
        e.eval("""define accumulate = function(op, initial, sequence)
            if(null?(sequence)) initial
            else op(car(sequence),
                    accumulate(op, initial, cdr(sequence)));""")
        e.eval("""define count_leaves = function(t)
            accumulate(+, 0, map(function(sub)
                if(pair?(sub)) count_leaves(sub) else 1, t));""")
        assert e.eval("count_leaves([[1, 2], [3, 4]]);") == 4
        assert e.eval("count_leaves([1, [2, [3, 4]], 5]);") == 5


class TestSICP_2_2_DeepReverse:
    """Exercise 2.27 — deep-reverse"""

    def test_deep_reverse(self, e):
        e.eval("""define deep_reverse = function(x)
            if(!pair?(x)) x
            else reverse(map(deep_reverse, x));""")
        assert e.eval("deep_reverse([[1, 2], [3, 4]]);") == [[4, 3], [2, 1]]
        assert e.eval("deep_reverse([1, 2, 3]);") == [3, 2, 1]
        assert e.eval("deep_reverse([]);") == []


class TestSICP_2_2_DotProduct:
    """2.2.3 — dot product and matrix operations (Exercise 2.37)"""

    def test_dot_product(self, e):
        e.eval("""define accumulate = function(op, initial, sequence)
            if(null?(sequence)) initial
            else op(car(sequence),
                    accumulate(op, initial, cdr(sequence)));""")
        e.eval("""define dot_product = function(v, w)
            accumulate(+, 0, map(*, v, w));""")
        assert e.eval("dot_product([1, 2, 3], [4, 5, 6]);") == 32

    def test_accumulate_n(self, e):
        """Exercise 2.36 — accumulate-n across sequences"""
        e.eval("""define accumulate = function(op, initial, sequence)
            if(null?(sequence)) initial
            else op(car(sequence),
                    accumulate(op, initial, cdr(sequence)));""")
        e.eval("""define accumulate_n = function(op, init, seqs)
            if(null?(car(seqs))) []
            else cons(accumulate(op, init, map(car, seqs)),
                      accumulate_n(op, init, map(cdr, seqs)));""")
        result = e.eval("accumulate_n(+, 0, [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]]);")

        assert result == [22, 26, 30]

    def test_matrix_vector(self, e):
        """Exercise 2.37 — matrix * vector"""
        e.eval("""define accumulate = function(op, initial, sequence)
            if(null?(sequence)) initial
            else op(car(sequence),
                    accumulate(op, initial, cdr(sequence)));""")
        e.eval("""define dot_product = function(v, w)
            accumulate(+, 0, map(*, v, w));""")
        e.eval("""define matrix_times_vector = function(m, v)
            map(function(row) dot_product(row, v), m);""")
        result = e.eval("matrix_times_vector([[1, 2], [3, 4]], [5, 6]);")
        assert result == [17, 39]

    def test_transpose(self, e):
        """Exercise 2.37 — matrix transpose"""
        e.eval("""define accumulate = function(op, initial, sequence)
            if(null?(sequence)) initial
            else op(car(sequence),
                    accumulate(op, initial, cdr(sequence)));""")
        e.eval("""define accumulate_n = function(op, init, seqs)
            if(null?(car(seqs))) []
            else cons(accumulate(op, init, map(car, seqs)),
                      accumulate_n(op, init, map(cdr, seqs)));""")
        e.eval("""define transpose = function(mat)
            accumulate_n(cons, [], mat);""")
        result = e.eval("transpose([[1, 2, 3], [4, 5, 6]]);")
        assert result == [[1, 4], [2, 5], [3, 6]]

    def test_matrix_multiply(self, e):
        """Exercise 2.37 — matrix * matrix"""
        e.eval("""define accumulate = function(op, initial, sequence)
            if(null?(sequence)) initial
            else op(car(sequence),
                    accumulate(op, initial, cdr(sequence)));""")
        e.eval("""define accumulate_n = function(op, init, seqs)
            if(null?(car(seqs))) []
            else cons(accumulate(op, init, map(car, seqs)),
                      accumulate_n(op, init, map(cdr, seqs)));""")
        e.eval("""define dot_product = function(v, w)
            accumulate(+, 0, map(*, v, w));""")
        e.eval("""define transpose = function(mat)
            accumulate_n(cons, [], mat);""")
        e.eval("""define matrix_times_matrix = function(m, n)
            let(cols = transpose(n))
                map(function(row) map(function(col) dot_product(row, col), cols), m);""")
        result = e.eval("matrix_times_matrix([[1, 2], [3, 4]], [[5, 6], [7, 8]]);")
        # [[1*5+2*7, 1*6+2*8], [3*5+4*7, 3*6+4*8]] = [[19, 22], [43, 50]]
        assert result == [[19, 22], [43, 50]]


class TestSICP_2_2_Permutations:
    """2.2.3 — Permutations using flatmap and remove"""

    def test_permutations(self, e):
        e.eval("""define accumulate = function(op, initial, sequence)
            if(null?(sequence)) initial
            else op(car(sequence),
                    accumulate(op, initial, cdr(sequence)));""")
        e.eval("""define flatmap = function(proc, seq)
            accumulate(append, [], map(proc, seq));""")
        e.eval("""define my_remove = function(item, sequence)
            filter(function(x) !(x == item), sequence);""")
        e.eval("""define permutations = function(s)
            if(null?(s)) [[]]
            else flatmap(
                function(x)
                    map(function(p) cons(x, p),
                        permutations(my_remove(x, s))),
                s);""")
        result = e.eval("permutations([1, 2, 3]);")
        assert len(result) == 6
        assert [1, 2, 3] in result
        assert [3, 2, 1] in result
        assert [2, 1, 3] in result

    def test_permutations_empty(self, e):
        e.eval("""define accumulate = function(op, initial, sequence)
            if(null?(sequence)) initial
            else op(car(sequence),
                    accumulate(op, initial, cdr(sequence)));""")
        e.eval("""define flatmap = function(proc, seq)
            accumulate(append, [], map(proc, seq));""")
        e.eval("""define my_remove = function(item, sequence)
            filter(function(x) !(x == item), sequence);""")
        e.eval("""define permutations = function(s)
            if(null?(s)) [[]]
            else flatmap(
                function(x)
                    map(function(p) cons(x, p),
                        permutations(my_remove(x, s))),
                s);""")
        assert e.eval("permutations([]);") == [[]]
        result = e.eval("permutations([1]);")
        assert result == [[1]]


class TestSICP_2_2_PrimeSumPairs:
    """2.2.3 — Prime sum pairs using nested mappings"""

    def test_prime_sum_pairs(self, e):
        e.eval("define square = function(x) x * x;")
        e.eval("""define find_divisor = function(n, test)
            cond(square(test) > n: n,
                 n % test == 0: test,
                 else: find_divisor(n, test + 1));""")
        e.eval("define smallest_divisor = function(n) find_divisor(n, 2);")
        e.eval("define prime = function(n) if(n < 2) false else n == smallest_divisor(n);")
        e.eval("""define accumulate = function(op, initial, sequence)
            if(null?(sequence)) initial
            else op(car(sequence),
                    accumulate(op, initial, cdr(sequence)));""")
        e.eval("""define flatmap = function(proc, seq)
            accumulate(append, [], map(proc, seq));""")
        e.eval("""define enumerate_interval = function(low, high)
            if(low > high) []
            else cons(low, enumerate_interval(low + 1, high));""")
        e.eval("""define prime_sum_pairs = function(n)
            filter(
                function(pair) prime(car(pair) + car(cdr(pair))),
                flatmap(
                    function(i)
                        map(function(j) [i, j],
                            enumerate_interval(1, i - 1)),
                    enumerate_interval(1, n)));""")
        result = e.eval("prime_sum_pairs(6);")
        # Verify some known prime-sum pairs
        for pair in result:
            assert pair[0] + pair[1] in [2, 3, 5, 7, 11, 13]  # must be prime


class TestSICP_2_2_EightQueens:
    """Exercise 2.42 — Eight queens puzzle"""

    def test_queens(self, e):
        e.eval("""define accumulate = function(op, initial, sequence)
            if(null?(sequence)) initial
            else op(car(sequence),
                    accumulate(op, initial, cdr(sequence)));""")
        e.eval("""define flatmap = function(proc, seq)
            accumulate(append, [], map(proc, seq));""")
        e.eval("""define enumerate_interval = function(low, high)
            if(low > high) []
            else cons(low, enumerate_interval(low + 1, high));""")
        # Positions are lists of row numbers, one per column
        e.eval("define empty_board = [];")
        e.eval("""define adjoin_position = function(new_row, k, rest_of_queens)
            cons(new_row, rest_of_queens);""")
        e.eval("""define safe = function(k, positions) {
            define new_row = car(positions);
            define rest = cdr(positions);
            define check = function(row_to_check, col_dist)
                if(null?(row_to_check)) true
                else let(r = car(row_to_check))
                    if(r == new_row) false
                    else if(abs(r - new_row) == col_dist) false
                    else check(cdr(row_to_check), col_dist + 1);
            check(rest, 1);
        };""")
        e.eval("""define queens = function(board_size) {
            define queen_cols = function(k)
                if(k == 0) [[]]
                else filter(
                    function(positions) safe(k, positions),
                    flatmap(
                        function(rest_of_queens)
                            map(function(new_row)
                                    adjoin_position(new_row, k, rest_of_queens),
                                enumerate_interval(1, board_size)),
                        queen_cols(k - 1)));
            queen_cols(board_size);
        };""")
        # The 4-queens problem has 2 solutions
        assert e.eval("length(queens(4));") == 2
        # The 5-queens problem has 10 solutions
        assert e.eval("length(queens(5));") == 10


# ============================================================================
# Chapter 2.3 — Symbolic differentiation & Huffman encoding
# ============================================================================

class TestSICP_2_3_SymbolicDiff:
    """2.3.2 — Symbolic differentiation"""

    def test_deriv_basic(self, e):
        """Basic symbolic differentiation with simplification"""
        # Predicates & selectors
        e.eval("define variable = function(x) symbol?(x);")
        e.eval("define same_variable = function(v1, v2) variable(v1) && variable(v2) && v1 =? v2;")
        e.eval("define eq_number = function(exp, num) number?(exp) && exp == num;")

        # Sum constructors with simplification (using strings as tags)
        e.eval("""define make_sum = function(a1, a2)
            cond(eq_number(a1, 0): a2,
                 eq_number(a2, 0): a1,
                 number?(a1) && number?(a2): a1 + a2,
                 else: ["+", a1, a2]);""")
        e.eval('define sum = function(e) car(e) == "+";')
        e.eval("define addend = function(e) car(cdr(e));")
        e.eval("define augend = function(e) car(cdr(cdr(e)));")

        # Product constructors with simplification
        e.eval("""define make_product = function(m1, m2)
            cond(eq_number(m1, 0) || eq_number(m2, 0): 0,
                 eq_number(m1, 1): m2,
                 eq_number(m2, 1): m1,
                 number?(m1) && number?(m2): m1 * m2,
                 else: ["*", m1, m2]);""")
        e.eval('define product = function(e) car(e) == "*";')
        e.eval("define multiplier = function(e) car(cdr(e));")
        e.eval("define multiplicand = function(e) car(cdr(cdr(e)));")

        # Derivative
        e.eval("""define deriv = function(exp, var)
            cond(number?(exp): 0,
                 variable(exp): if(same_variable(exp, var)) 1 else 0,
                 sum(exp): make_sum(deriv(addend(exp), var),
                                    deriv(augend(exp), var)),
                 product(exp): make_sum(
                     make_product(multiplier(exp),
                                  deriv(multiplicand(exp), var)),
                     make_product(deriv(multiplier(exp), var),
                                  multiplicand(exp))),
                 else: 'error);""")

        # d/dx (x + 3) = 1
        assert e.eval("""deriv(["+", 'x, 3], 'x);""") == 1
        # d/dx (x * y) = y
        assert e.eval("""deriv(["*", 'x, 'y], 'x);""") == e.eval("'y;")
        # d/dx (5 * x) = 5
        assert e.eval("""deriv(["*", 5, 'x], 'x);""") == 5
        # d/dx 42 = 0
        assert e.eval("deriv(42, 'x);") == 0

    def test_deriv_with_exponentiation(self, e):
        """Exercise 2.56 — differentiation with exponentiation"""
        e.eval("define variable = function(x) symbol?(x);")
        e.eval("define same_variable = function(v1, v2) variable(v1) && variable(v2) && v1 =? v2;")
        e.eval("define eq_number = function(exp, num) number?(exp) && exp == num;")
        e.eval("""define make_sum = function(a1, a2)
            cond(eq_number(a1, 0): a2,
                 eq_number(a2, 0): a1,
                 number?(a1) && number?(a2): a1 + a2,
                 else: ["+", a1, a2]);""")
        e.eval('define sum = function(e) car(e) == "+";')
        e.eval("define addend = function(e) car(cdr(e));")
        e.eval("define augend = function(e) car(cdr(cdr(e)));")
        e.eval("""define make_product = function(m1, m2)
            cond(eq_number(m1, 0) || eq_number(m2, 0): 0,
                 eq_number(m1, 1): m2,
                 eq_number(m2, 1): m1,
                 number?(m1) && number?(m2): m1 * m2,
                 else: ["*", m1, m2]);""")
        e.eval('define product = function(e) car(e) == "*";')
        e.eval("define multiplier = function(e) car(cdr(e));")
        e.eval("define multiplicand = function(e) car(cdr(cdr(e)));")

        # Exponentiation support
        e.eval('define exponentiation = function(e) car(e) == "**";')
        e.eval("define base = function(e) car(cdr(e));")
        e.eval("define exponent = function(e) car(cdr(cdr(e)));")
        e.eval("""define make_exponentiation = function(b, ex)
            cond(eq_number(ex, 0): 1,
                 eq_number(ex, 1): b,
                 else: ["**", b, ex]);""")

        e.eval("""define deriv = function(exp, var)
            cond(number?(exp): 0,
                 variable(exp): if(same_variable(exp, var)) 1 else 0,
                 sum(exp): make_sum(deriv(addend(exp), var),
                                    deriv(augend(exp), var)),
                 product(exp): make_sum(
                     make_product(multiplier(exp),
                                  deriv(multiplicand(exp), var)),
                     make_product(deriv(multiplier(exp), var),
                                  multiplicand(exp))),
                 exponentiation(exp): make_product(
                     make_product(exponent(exp),
                                  make_exponentiation(base(exp), exponent(exp) - 1)),
                     deriv(base(exp), var)),
                 else: 'error);""")

        # d/dx (x ** 3) = 3 * x^2  →  ["*", 3, ["**", 'x, 2]]
        result = e.eval("""deriv(["**", 'x, 3], 'x);""")
        assert result[0] == "*"
        assert result[1] == 3


class TestSICP_2_3_Huffman:
    """2.3.4 — Huffman encoding trees"""

    def test_huffman_full(self, e):
        """Build Huffman tree, encode, decode"""
        # Leaf constructors/selectors
        e.eval("define make_leaf = function(sym, weight) ['leaf, sym, weight];")
        e.eval("define leaf = function(obj) car(obj) =? 'leaf;")
        e.eval("define symbol_leaf = function(x) car(cdr(x));")
        e.eval("define weight_leaf = function(x) car(cdr(cdr(x)));")

        # Tree constructors/selectors
        e.eval("""define make_code_tree = function(left, right)
            [left, right,
             append(symbols_of(left), symbols_of(right)),
             weight_of(left) + weight_of(right)];""")
        e.eval("define left_branch = function(tree) car(tree);")
        e.eval("define right_branch = function(tree) car(cdr(tree));")
        e.eval("""define symbols_of = function(tree)
            if(leaf(tree)) [symbol_leaf(tree)]
            else car(cdr(cdr(tree)));""")
        e.eval("""define weight_of = function(tree)
            if(leaf(tree)) weight_leaf(tree)
            else car(cdr(cdr(cdr(tree))));""")

        # Decode
        e.eval("""define choose_branch = function(bit, branch)
            if(bit == 0) left_branch(branch)
            else right_branch(branch);""")
        e.eval("""define decode = function(bits, tree) {
            define decode_1 = function(bits, current_branch)
                if(null?(bits)) []
                else let(next = choose_branch(car(bits), current_branch))
                    if(leaf(next))
                        cons(symbol_leaf(next), decode_1(cdr(bits), tree))
                    else decode_1(cdr(bits), next);
            decode_1(bits, tree);
        };""")

        # Build the sample tree from Exercise 2.67
        e.eval("""define sample_tree = make_code_tree(
            make_leaf('A, 4),
            make_code_tree(
                make_leaf('B, 2),
                make_code_tree(
                    make_leaf('D, 1),
                    make_leaf('C, 1))));""")
        e.eval("define sample_message = [0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0];")

        # Decode
        decoded = e.eval("decode(sample_message, sample_tree);")
        a, b, c, d = e.eval("'A;"), e.eval("'B;"), e.eval("'C;"), e.eval("'D;")
        assert decoded == [a, d, a, b, b, c, a]

    def test_huffman_encode(self, e):
        """Exercise 2.68 — encode-symbol"""
        e.eval("define make_leaf = function(sym, weight) ['leaf, sym, weight];")
        e.eval("define leaf = function(obj) car(obj) =? 'leaf;")
        e.eval("define symbol_leaf = function(x) car(cdr(x));")
        e.eval("define weight_leaf = function(x) car(cdr(cdr(x)));")
        e.eval("""define make_code_tree = function(left, right)
            [left, right,
             append(symbols_of(left), symbols_of(right)),
             weight_of(left) + weight_of(right)];""")
        e.eval("define left_branch = function(tree) car(tree);")
        e.eval("define right_branch = function(tree) car(cdr(tree));")
        e.eval("""define symbols_of = function(tree)
            if(leaf(tree)) [symbol_leaf(tree)]
            else car(cdr(cdr(tree)));""")
        e.eval("""define weight_of = function(tree)
            if(leaf(tree)) weight_leaf(tree)
            else car(cdr(cdr(cdr(tree))));""")

        # element-of-set? for symbols
        e.eval("""define element_of = function(x, set)
            cond(null?(set): false,
                 x =? car(set): true,
                 else: element_of(x, cdr(set)));""")

        # encode-symbol
        e.eval("""define encode_symbol = function(sym, tree)
            if(leaf(tree)) []
            else if(element_of(sym, symbols_of(left_branch(tree))))
                cons(0, encode_symbol(sym, left_branch(tree)))
            else cons(1, encode_symbol(sym, right_branch(tree)));""")

        # encode
        e.eval("""define encode = function(message, tree)
            if(null?(message)) []
            else append(encode_symbol(car(message), tree),
                        encode(cdr(message), tree));""")

        # Sample tree: A=0, B=10, D=110, C=111
        e.eval("""define sample_tree = make_code_tree(
            make_leaf('A, 4),
            make_code_tree(
                make_leaf('B, 2),
                make_code_tree(
                    make_leaf('D, 1),
                    make_leaf('C, 1))));""")

        # Encode A D A B B C A
        result = e.eval("encode(['A, 'D, 'A, 'B, 'B, 'C, 'A], sample_tree);")
        assert result == [0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0]

    def test_generate_huffman_tree(self, e):
        """Exercise 2.69 — generate-huffman-tree from frequency pairs"""
        e.eval("define make_leaf = function(sym, weight) ['leaf, sym, weight];")
        e.eval("define leaf = function(obj) car(obj) =? 'leaf;")
        e.eval("define symbol_leaf = function(x) car(cdr(x));")
        e.eval("define weight_leaf = function(x) car(cdr(cdr(x)));")
        e.eval("""define make_code_tree = function(left, right)
            [left, right,
             append(symbols_of(left), symbols_of(right)),
             weight_of(left) + weight_of(right)];""")
        e.eval("define left_branch = function(tree) car(tree);")
        e.eval("define right_branch = function(tree) car(cdr(tree));")
        e.eval("""define symbols_of = function(tree)
            if(leaf(tree)) [symbol_leaf(tree)]
            else car(cdr(cdr(tree)));""")
        e.eval("""define weight_of = function(tree)
            if(leaf(tree)) weight_leaf(tree)
            else car(cdr(cdr(cdr(tree))));""")

        # adjoin-set ordered by weight
        e.eval("""define adjoin_set_w = function(x, set)
            cond(null?(set): [x],
                 weight_of(x) < weight_of(car(set)): cons(x, set),
                 else: cons(car(set), adjoin_set_w(x, cdr(set))));""")

        # make-leaf-set
        e.eval("""define make_leaf_set = function(pairs)
            if(null?(pairs)) []
            else let(pair = car(pairs))
                adjoin_set_w(make_leaf(car(pair), car(cdr(pair))),
                             make_leaf_set(cdr(pairs)));""")

        # successive-merge
        e.eval("""define successive_merge = function(set)
            if(null?(cdr(set))) car(set)
            else successive_merge(
                adjoin_set_w(
                    make_code_tree(car(set), car(cdr(set))),
                    cdr(cdr(set))));""")

        e.eval("""define generate_huffman_tree = function(pairs)
            successive_merge(make_leaf_set(pairs));""")

        # encode helper
        e.eval("""define element_of = function(x, set)
            cond(null?(set): false,
                 x =? car(set): true,
                 else: element_of(x, cdr(set)));""")
        e.eval("""define encode_symbol = function(sym, tree)
            if(leaf(tree)) []
            else if(element_of(sym, symbols_of(left_branch(tree))))
                cons(0, encode_symbol(sym, left_branch(tree)))
            else cons(1, encode_symbol(sym, right_branch(tree)));""")
        e.eval("""define encode = function(message, tree)
            if(null?(message)) []
            else append(encode_symbol(car(message), tree),
                        encode(cdr(message), tree));""")
        e.eval("""define choose_branch = function(bit, branch)
            if(bit == 0) left_branch(branch)
            else right_branch(branch);""")
        e.eval("""define decode = function(bits, tree) {
            define decode_1 = function(bits, current_branch)
                if(null?(bits)) []
                else let(next = choose_branch(car(bits), current_branch))
                    if(leaf(next))
                        cons(symbol_leaf(next), decode_1(cdr(bits), tree))
                    else decode_1(cdr(bits), next);
            decode_1(bits, tree);
        };""")

        # Generate tree from pairs and verify roundtrip
        e.eval("define tree = generate_huffman_tree([['A, 4], ['B, 2], ['C, 1], ['D, 1]]);")

        # Encode and decode should roundtrip
        e.eval("define msg = ['A, 'B, 'A, 'C, 'D, 'A];")
        e.eval("define encoded = encode(msg, tree);")
        decoded = e.eval("decode(encoded, tree);")
        original = e.eval("msg;")
        assert decoded == original

        # Total weight should be 8
        assert e.eval("weight_of(tree);") == 8


class TestSICP_2_3_TreeToList:
    """Exercise 2.63 — tree->list conversions"""

    def test_tree_to_list(self, e):
        """Two tree->list algorithms produce same result"""
        e.eval("define entry = function(tree) car(tree);")
        e.eval("define left_branch = function(tree) car(cdr(tree));")
        e.eval("define right_branch = function(tree) car(cdr(cdr(tree)));")
        e.eval("define make_tree = function(entry, left, right) [entry, left, right];")

        e.eval("""define tree_to_list_1 = function(tree)
            if(null?(tree)) []
            else append(tree_to_list_1(left_branch(tree)),
                        cons(entry(tree),
                             tree_to_list_1(right_branch(tree))));""")

        e.eval("""define tree_to_list_2 = function(tree) {
            define copy_to_list = function(tree, result_list)
                if(null?(tree)) result_list
                else copy_to_list(left_branch(tree),
                                  cons(entry(tree),
                                       copy_to_list(right_branch(tree),
                                                    result_list)));
            copy_to_list(tree, []);
        };""")

        # Build tree:       7
        #                 /   \
        #                3     9
        #               / \     \
        #              1   5    11
        e.eval("""define t = make_tree(7,
            make_tree(3, make_tree(1, [], []), make_tree(5, [], [])),
            make_tree(9, [], make_tree(11, [], [])));""")

        r1 = e.eval("tree_to_list_1(t);")
        r2 = e.eval("tree_to_list_2(t);")
        assert r1 == [1, 3, 5, 7, 9, 11]
        assert r2 == [1, 3, 5, 7, 9, 11]


class TestSICP_2_3_ListToTree:
    """Exercise 2.64 — list->tree (balanced BST construction)"""

    def test_list_to_tree(self, e):
        e.eval("define make_tree = function(entry, left, right) [entry, left, right];")
        e.eval("define entry = function(tree) car(tree);")
        e.eval("define left_branch = function(tree) car(cdr(tree));")
        e.eval("define right_branch = function(tree) car(cdr(cdr(tree)));")

        e.eval("""define partial_tree = function(elts, n)
            if(n == 0) cons([], elts)
            else let(left_size = quotient(n - 1, 2))
                let(left_result = partial_tree(elts, left_size))
                    let(left_tree = car(left_result),
                        non_left_elts = cdr(left_result),
                        right_size = n - (left_size + 1))
                        let(this_entry = car(non_left_elts),
                            right_result = partial_tree(cdr(non_left_elts), right_size))
                            let(right_tree = car(right_result),
                                remaining_elts = cdr(right_result))
                                cons(make_tree(this_entry, left_tree, right_tree),
                                     remaining_elts);""")

        e.eval("""define list_to_tree = function(elements)
            car(partial_tree(elements, length(elements)));""")

        # Convert to tree and verify
        e.eval("define t = list_to_tree([1, 3, 5, 7, 9, 11]);")
        assert e.eval("entry(t);") == 5
        assert e.eval("entry(left_branch(t));") == 1
        assert e.eval("entry(right_branch(t));") == 9

        # Convert back to sorted list
        e.eval("""define tree_to_list = function(tree)
            if(null?(tree)) []
            else append(tree_to_list(left_branch(tree)),
                        cons(entry(tree),
                             tree_to_list(right_branch(tree))));""")
        assert e.eval("tree_to_list(t);") == [1, 3, 5, 7, 9, 11]


# ============================================================================
# Chapter 3.3 — Pure-functional subset (no mutation needed)
# ============================================================================

class TestSICP_3_3_ProceduralPairs:
    """3.3.1 — Message-passing cons/car/cdr (pure, no set-car!/set-cdr!)"""

    def test_procedural_cons(self, e):
        e.eval("""define my_cons = function(x, y)
            function(m)
                cond(m =? 'car: x,
                     m =? 'cdr: y,
                     else: 'error);""")
        e.eval("define my_car = function(z) z('car);")
        e.eval("define my_cdr = function(z) z('cdr);")
        assert e.eval("my_car(my_cons(1, 2));") == 1
        assert e.eval("my_cdr(my_cons(1, 2));") == 2
        # Nested
        assert e.eval("my_car(my_cdr(my_cons(1, my_cons(2, 3))));") == 2


class TestSICP_3_3_Lookup:
    """3.3.3 — Pure functional table lookup with assoc"""

    def test_assoc(self, e):
        e.eval("""define my_assoc = function(key, records)
            cond(null?(records): false,
                 key == car(car(records)): car(records),
                 else: my_assoc(key, cdr(records)));""")
        e.eval("define records = [[1, 'a], [2, 'b], [3, 'c]];")
        assert e.eval("car(cdr(my_assoc(2, records)));") == e.eval("'b;")
        assert e.eval("my_assoc(99, records);") is False
