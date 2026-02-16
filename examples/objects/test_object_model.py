"""Complete Object Model for Eval — based on SICP Chapter 3.

Demonstrates the full SICP-inspired object system built on closures and
message-passing, using Eval's `constructor/interface` syntax sugar.

Object Model Hierarchy (from simple to complex):
    1. Closures with local state          — SICP §3.1
    2. Message-passing dispatch           — SICP §2.4.3, §3.1
    3. constructor/interface sugar        — Eval built-in
    4. Delegation (inheritance)           — manual composition
    5. Mutable objects                    — SICP §3.1
    6. Shared/joint state                 — SICP §3.1.3
    7. Procedural data (pairs, Church)    — SICP §2.1.3
"""

import pytest
import math
from chibi_eval import Eval, EvalError


@pytest.fixture
def e():
    return Eval()


# ============================================================================
# 1. Closures with Local State  (SICP §3.1.1)
# ============================================================================

class TestClosureState:
    """Objects as closures with mutable local state."""

    def test_make_withdraw(self, e):
        """SICP §3.1.1 — make-withdraw: withdrawal processor."""
        e.eval("""
            make_withdraw := function(balance)
                function(amount)
                    if(balance >= amount) {
                        balance = balance - amount;
                        balance;
                    } else error("Insufficient funds");
        """)
        e.eval("W1 := make_withdraw(100);")
        e.eval("W2 := make_withdraw(100);")

        assert e.eval("W1(50);") == 50
        assert e.eval("W2(70);") == 30
        assert e.eval("W1(40);") == 10
        with pytest.raises(EvalError, match="Insufficient funds"):
            e.eval("W2(40);")

    def test_make_accumulator(self, e):
        """SICP Exercise 3.1 — accumulator."""
        e.eval("""
            make_accumulator := function(initial) {
                total := initial;
                function(amount) {
                    total = total + amount;
                    total;
                };
            };
        """)
        e.eval("A := make_accumulator(5);")
        assert e.eval("A(10);") == 15
        assert e.eval("A(10);") == 25

    def test_make_counter(self, e):
        """Simple counter — closure pattern."""
        e.eval("""
            make_counter := function() {
                count := 0;
                function(msg)
                    cond(msg == 'inc:  { count = count + 1; count; },
                         msg == 'dec:  { count = count - 1; count; },
                         msg == 'get:  count,
                         msg == 'reset: { count = 0; count; },
                         else: error("Unknown message"));
            };
        """)
        e.eval("c := make_counter();")
        assert e.eval("c('get);") == 0
        assert e.eval("c('inc);") == 1
        assert e.eval("c('inc);") == 2
        assert e.eval("c('inc);") == 3
        assert e.eval("c('dec);") == 2
        assert e.eval("c('reset);") == 0


# ============================================================================
# 2. Message-Passing Dispatch  (SICP §2.4.3, §3.1.1)
# ============================================================================

class TestMessagePassing:
    """SICP-style message-passing objects via dispatch functions."""

    def test_make_account(self, e):
        """SICP §3.1.1 — bank account with deposit & withdraw."""
        e.eval("""
            make_account := function(balance) {
                withdraw := function(amount)
                    if(balance >= amount) {
                        balance = balance - amount;
                        balance;
                    } else error("Insufficient funds");
                deposit := function(amount) {
                    balance = balance + amount;
                    balance;
                };
                get_balance := function() balance;
                dispatch := function(m)
                    cond(m == 'withdraw: withdraw,
                         m == 'deposit:  deposit,
                         m == 'balance:  get_balance,
                         else: error("Unknown request"));
                dispatch;
            };
        """)
        e.eval("acc := make_account(100);")
        assert e.eval("acc('withdraw)(50);") == 50
        assert e.eval("acc('deposit)(40);") == 90
        assert e.eval("acc('balance)();") == 90
        assert e.eval("acc('withdraw)(60);") == 30

    def test_two_independent_accounts(self, e):
        """Two accounts maintain independent state."""
        e.eval("""
            make_account := function(balance) {
                withdraw := function(amount) {
                    balance = balance - amount; balance;
                };
                deposit := function(amount) {
                    balance = balance + amount; balance;
                };
                function(m)
                    cond(m == 'withdraw: withdraw,
                         m == 'deposit:  deposit,
                         m == 'balance:  function() balance);
            };
        """)
        e.eval("a1 := make_account(100);")
        e.eval("a2 := make_account(200);")
        e.eval("a1('withdraw)(30);")
        e.eval("a2('deposit)(50);")
        assert e.eval("a1('balance)();") == 70
        assert e.eval("a2('balance)();") == 250

    def test_make_monitored(self, e):
        """SICP Exercise 3.2 — monitored procedure wrapper."""
        e.eval("""
            make_monitored := function(f) {
                count := 0;
                function(x)
                    cond(x == 'how_many_calls: count,
                         x == 'reset_count: { count = 0; count; },
                         else: { count = count + 1; f(x); });
            };
        """)
        e.eval("s := make_monitored(function(x) x * x);")
        assert e.eval("s(10);") == 100
        assert e.eval("s(5);") == 25
        assert e.eval("s('how_many_calls);") == 2
        e.eval("s('reset_count);")
        assert e.eval("s('how_many_calls);") == 0

    def test_password_protected_account(self, e):
        """SICP Exercise 3.3 — password-protected bank account."""
        e.eval("""
            make_pw_account := function(balance, password) {
                withdraw := function(amount)
                    if(balance >= amount) {
                        balance = balance - amount;
                        balance;
                    } else error("Insufficient funds");
                deposit := function(amount) {
                    balance = balance + amount;
                    balance;
                };
                function(pw, m)
                    if(pw == password)
                        cond(m == 'withdraw: withdraw,
                             m == 'deposit:  deposit,
                             else: error("Unknown request"))
                    else error("Incorrect password");
            };
        """)
        e.eval("acc := make_pw_account(100, 'secret);")
        assert e.eval("acc('secret, 'withdraw)(40);") == 60
        assert e.eval("acc('secret, 'deposit)(20);") == 80
        with pytest.raises(EvalError, match="Incorrect password"):
            e.eval("acc('wrong, 'withdraw)(10);")

    def test_joint_account(self, e):
        """SICP Exercise 3.7 — joint accounts sharing state."""
        e.eval("""
            make_account := function(balance, password) {
                withdraw := function(amount)
                    if(balance >= amount) {
                        balance = balance - amount; balance;
                    } else error("Insufficient funds");
                deposit := function(amount) {
                    balance = balance + amount; balance;
                };
                check_password := function(pw) pw == password;
                dispatch := function(pw, m)
                    if(pw == password)
                        cond(m == 'withdraw: withdraw,
                             m == 'deposit:  deposit,
                             m == 'check_pw: check_password,
                             else: error("Unknown request"))
                    else error("Incorrect password");
                dispatch;
            };
        """)
        e.eval("""
            make_joint := function(account, old_pw, new_pw)
                function(pw, m)
                    if(pw == new_pw)
                        account(old_pw, m)
                    else error("Incorrect password");
        """)
        e.eval("peter_acc := make_account(100, 'peter_pw);")
        e.eval("paul_acc  := make_joint(peter_acc, 'peter_pw, 'paul_pw);")

        # Peter withdraws 20
        assert e.eval("peter_acc('peter_pw, 'withdraw)(20);") == 80
        # Paul deposits 50 — same account!
        assert e.eval("paul_acc('paul_pw, 'deposit)(50);") == 130
        # Peter sees the shared balance
        assert e.eval("peter_acc('peter_pw, 'deposit)(0);") == 130


# ============================================================================
# 3. Constructor / Interface Sugar  (Eval built-in)
# ============================================================================

class TestConstructorInterface:
    """Eval's `constructor ... interface(...)` syntax."""

    def test_point(self, e):
        """Basic immutable-data object."""
        e.eval("""
            Point := constructor(x, y)
                interface(
                    x: x,
                    y: y,
                    dist: function() (x**2 + y**2)**0.5
                );
        """)
        e.eval("p := Point(3, 4);")
        assert e.eval("p->x;") == 3
        assert e.eval("p->y;") == 4
        assert e.eval("p->dist();") == 5.0

    def test_point_operations(self, e):
        """Objects that return new objects (value semantics)."""
        e.eval("""
            Point := constructor(x, y)
                interface(
                    x: x,
                    y: y,
                    add:   function(other) Point(x + other->x, y + other->y),
                    scale: function(k) Point(x * k, y * k),
                    dist:  function() (x**2 + y**2)**0.5,
                    eq:    function(other) x == other->x && y == other->y
                );
        """)
        e.eval("p1 := Point(3, 4);")
        e.eval("p2 := Point(1, 2);")
        e.eval("p3 := p1->add(p2);")
        assert e.eval("p3->x;") == 4
        assert e.eval("p3->y;") == 6

        e.eval("p4 := p1->scale(2);")
        assert e.eval("p4->x;") == 6
        assert e.eval("p4->y;") == 8

        assert e.eval("p1->eq(Point(3, 4));") is True
        assert e.eval("p1->eq(p2);") is False

    def test_mutable_counter(self, e):
        """Constructor with internal mutable state."""
        e.eval("""
            Counter := constructor(init) {
                count := init;
                interface(
                    get:   function() count,
                    inc:   function() { count = count + 1; count; },
                    dec:   function() { count = count - 1; count; },
                    reset: function() { count = init; count; }
                );
            };
        """)
        e.eval("c := Counter(0);")
        assert e.eval("c->get();") == 0
        assert e.eval("c->inc();") == 1
        assert e.eval("c->inc();") == 2
        assert e.eval("c->inc();") == 3
        assert e.eval("c->dec();") == 2
        assert e.eval("c->reset();") == 0
        assert e.eval("c->get();") == 0

    def test_bank_account_constructor(self, e):
        """Bank account using constructor/interface."""
        e.eval("""
            Account := constructor(initial_balance) {
                balance := initial_balance;
                interface(
                    balance:  function() balance,
                    withdraw: function(amount)
                        if(balance >= amount) {
                            balance = balance - amount; balance;
                        } else error("Insufficient funds"),
                    deposit:  function(amount) {
                        balance = balance + amount; balance;
                    }
                );
            };
        """)
        e.eval("acc := Account(100);")
        assert e.eval("acc->balance();") == 100
        assert e.eval("acc->withdraw(30);") == 70
        assert e.eval("acc->deposit(50);") == 120
        assert e.eval("acc->balance();") == 120

    def test_stack(self, e):
        """Stack data structure as an object."""
        e.eval("""
            Stack := constructor() {
                items := [];
                interface(
                    push: function(x) { items = cons(x, items); x; },
                    pop:  function()
                        if(null?(items)) error("Stack empty")
                        else {
                            top := car(items);
                            items = cdr(items);
                            top;
                        },
                    peek: function()
                        if(null?(items)) error("Stack empty")
                        else car(items),
                    size:  function() length(items),
                    empty: function() null?(items)
                );
            };
        """)
        e.eval("s := Stack();")
        assert e.eval("s->empty();") is True
        e.eval("s->push(10);")
        e.eval("s->push(20);")
        e.eval("s->push(30);")
        assert e.eval("s->size();") == 3
        assert e.eval("s->peek();") == 30
        assert e.eval("s->pop();") == 30
        assert e.eval("s->pop();") == 20
        assert e.eval("s->size();") == 1

    def test_queue(self, e):
        """SICP §3.3.2 style queue via mutable pairs."""
        e.eval("""
            Queue := constructor() {
                front := [];
                rear  := [];
                interface(
                    empty: function() null?(front),
                    enqueue: function(item) {
                        new_pair := [item];
                        if(null?(front)) {
                            front = new_pair;
                            rear  = new_pair;
                        } else {
                            rear = append(rear, new_pair);
                            front = append(front, new_pair);
                        };
                        item;
                    },
                    dequeue: function()
                        if(null?(front)) error("Queue empty")
                        else {
                            item := car(front);
                            front = cdr(front);
                            item;
                        },
                    peek: function()
                        if(null?(front)) error("Queue empty")
                        else car(front),
                    size: function() length(front)
                );
            };
        """)
        e.eval("q := Queue();")
        assert e.eval("q->empty();") is True
        e.eval("q->enqueue(1);")
        e.eval("q->enqueue(2);")
        e.eval("q->enqueue(3);")
        assert e.eval("q->size();") == 3
        assert e.eval("q->dequeue();") == 1
        assert e.eval("q->dequeue();") == 2
        assert e.eval("q->peek();") == 3


# ============================================================================
# 4. Delegation / Inheritance
# ============================================================================

class TestDelegation:
    """Inheritance via manual delegation (composition over inheritance)."""

    def test_basic_delegation(self, e):
        """Child delegates unknown messages to parent."""
        e.eval("""
            Animal := constructor(n, sound)
                interface(
                    name:  n,
                    sound: sound,
                    speak: function() `string-append`(n, " says ", sound)
                );
        """)
        e.eval("""
            Pet := constructor(n, sound, owner) {
                parent := Animal(n, sound);
                interface(
                    owner: owner,
                    name:  function() parent->name,
                    speak: function() parent->speak(),
                    greet: function() `string-append`(n, " belongs to ", owner)
                );
            };
        """)
        e.eval('p := Pet("Buddy", "Woof", "Alice");')
        assert e.eval("p->owner;") == "Alice"
        assert e.eval("p->name();") == "Buddy"
        assert e.eval("p->speak();") == "Buddy says Woof"
        assert e.eval("p->greet();") == "Buddy belongs to Alice"

    def test_override_method(self, e):
        """Child can override parent behavior."""
        e.eval("""
            Shape := constructor(type)
                interface(
                    type: type,
                    area: function() 0,
                    describe: function() `string-append`("Shape: ", type)
                );
        """)
        e.eval("""
            Circle := constructor(r) {
                parent := Shape("circle");
                interface(
                    type:     function() parent->type,
                    radius:   r,
                    area:     function() 3.14159265 * r * r,
                    describe: function() `string-append`("Circle r=", `number->string`(r))
                );
            };
        """)
        e.eval("""
            Rectangle := constructor(w, h) {
                parent := Shape("rectangle");
                interface(
                    type:     function() parent->type,
                    width:    w,
                    height:   h,
                    area:     function() w * h,
                    describe: function() `string-append`(
                        "Rectangle ", `number->string`(w),
                        "x", `number->string`(h))
                );
            };
        """)
        e.eval("c := Circle(5);")
        e.eval("r := Rectangle(3, 4);")

        assert abs(e.eval("c->area();") - 78.5398) < 0.01
        assert e.eval("r->area();") == 12
        assert e.eval("c->type();") == "circle"
        assert e.eval("r->type();") == "rectangle"

    def test_polymorphic_dispatch(self, e):
        """Polymorphism: same message, different behavior."""
        e.eval("""
            Circle := constructor(r)
                interface(
                    area: function() 3.14159265 * r * r,
                    kind: "circle"
                );
        """)
        e.eval("""
            Square := constructor(s)
                interface(
                    area: function() s * s,
                    kind: "square"
                );
        """)
        e.eval("""
            Triangle := constructor(b, h)
                interface(
                    area: function() b * h / 2,
                    kind: "triangle"
                );
        """)
        e.eval("shapes := [Circle(5), Square(4), Triangle(6, 3)];")

        # Total area via map + fold
        e.eval("total := fold(+, 0, map(function(s) s->area(), shapes));")
        expected = math.pi * 25 + 16 + 9  # ~103.54
        assert abs(e.eval("total;") - (3.14159265 * 25 + 16 + 9)) < 0.01

    def test_two_level_inheritance(self, e):
        """Three-level delegation chain: Vehicle > Car > ElectricCar."""
        e.eval("""
            Vehicle := constructor(make, year)
                interface(
                    make: make,
                    year: year,
                    describe: function() `string-append`(make, " (", `number->string`(year), ")")
                );
        """)
        e.eval("""
            Car := constructor(make, year, doors) {
                parent := Vehicle(make, year);
                interface(
                    make:     function() parent->make,
                    year:     function() parent->year,
                    doors:    doors,
                    describe: function() `string-append`(parent->describe(), ", ", `number->string`(doors), " doors")
                );
            };
        """)
        e.eval("""
            ElectricCar := constructor(make, year, doors, range_km) {
                parent := Car(make, year, doors);
                interface(
                    make:     function() parent->make(),
                    year:     function() parent->year(),
                    doors:    function() parent->doors,
                    range_km: range_km,
                    describe: function() `string-append`(parent->describe(), ", range ", `number->string`(range_km), "km")
                );
            };
        """)
        e.eval('ev := ElectricCar("Tesla", 2024, 4, 500);')
        assert e.eval("ev->make();") == "Tesla"
        assert e.eval("ev->range_km;") == 500
        assert e.eval("ev->describe();") == "Tesla (2024), 4 doors, range 500km"


# ============================================================================
# 5. Advanced Object Patterns
# ============================================================================

class TestAdvancedPatterns:
    """More complex object patterns from SICP and beyond."""

    def test_make_rand(self, e):
        """SICP §3.1.2 — random number generator with reset.
        Uses a simple linear congruential generator."""
        e.eval("""
            make_rand := function(seed) {
                x := seed;
                function(msg)
                    cond(msg == 'generate: {
                             x = (x * 1103515245 + 12345) % (2 ** 31);
                             x;
                         },
                         msg == 'reset: function(new_seed) { x = new_seed; x; },
                         else: error("Unknown message"));
            };
        """)
        e.eval("rng := make_rand(42);")
        r1 = e.eval("rng('generate);")
        r2 = e.eval("rng('generate);")
        assert r1 != r2  # different values
        # Reset and verify reproducibility
        e.eval("rng('reset)(42);")
        assert e.eval("rng('generate);") == r1
        assert e.eval("rng('generate);") == r2

    def test_dispatch_table_pattern(self, e):
        """SICP §2.4.3 style: operation-type table for generic operations."""
        e.eval("""
            make_table := function() {
                records := [];
                lookup := function(key)
                    letrec(scan := function(items)
                        if(null?(items)) false
                        else if(car(car(items)) == key) car(cdr(car(items)))
                        else scan(cdr(items))
                    ) scan(records);
                insert := function(key, value) {
                    records = cons([key, value], records);
                    'ok;
                };
                function(m)
                    cond(m == 'lookup: lookup,
                         m == 'insert: insert,
                         else: error("Unknown table op"));
            };
        """)
        e.eval("t := make_table();")
        e.eval("t('insert)('x, 42);")
        e.eval("t('insert)('y, 99);")
        assert e.eval("t('lookup)('x);") == 42
        assert e.eval("t('lookup)('y);") == 99
        assert e.eval("t('lookup)('z);") is False

    def test_observer_pattern(self, e):
        """Observer/event pattern: objects notifying listeners."""
        e.eval("""
            EventEmitter := constructor() {
                listeners := [];
                interface(
                    on: function(callback) {
                        listeners = cons(callback, listeners);
                        'ok;
                    },
                    emit: function(value) {
                        `for-each`(function(cb) cb(value), listeners);
                        'ok;
                    }
                );
            };
        """)
        e.eval("emitter := EventEmitter();")
        e.eval("log := [];")
        e.eval("emitter->on(function(v) { log = append(log, [v]); });")
        e.eval("emitter->on(function(v) { log = append(log, [v * 10]); });")
        e.eval("emitter->emit(5);")
        # Both listeners fired (cons prepends, so second listener fires first)
        assert e.eval("log;") == [50, 5]

    def test_iterator_pattern(self, e):
        """Iterator: lazy sequence via closures."""
        e.eval("""
            range_iter := function(start, stop) {
                current := start;
                function(msg)
                    cond(msg == 'has_next: current < stop,
                         msg == 'next: if(current < stop) {
                             val := current;
                             current = current + 1;
                             val;
                         } else error("StopIteration"),
                         msg == 'reset: { current = start; 'ok; },
                         else: error("Unknown message"));
            };
        """)
        e.eval("it := range_iter(0, 5);")
        results = []
        for _ in range(5):
            assert e.eval("it('has_next);") is True
            results.append(e.eval("it('next);"))
        assert results == [0, 1, 2, 3, 4]
        assert e.eval("it('has_next);") is False

    def test_linked_list_object(self, e):
        """Linked list as objects with SICP-style cons/car/cdr."""
        e.eval("""
            Nil := constructor()
                interface(
                    null:   true,
                    length: function() 0,
                    to_list: function() []
                );
        """)
        e.eval("""
            Cons := constructor(hd, tl)
                interface(
                    null:   false,
                    head:   hd,
                    tail:   tl,
                    length: function() 1 + tl->length(),
                    to_list: function() cons(hd, tl->to_list())
                );
        """)
        e.eval("empty := Nil();")
        e.eval("lst := Cons(1, Cons(2, Cons(3, empty)));")
        assert e.eval("lst->head;") == 1
        assert e.eval("lst->tail->head;") == 2
        assert e.eval("lst->length();") == 3
        assert e.eval("lst->to_list();") == [1, 2, 3]
        assert e.eval("empty->null;") is True
        assert e.eval("lst->null;") is False


# ============================================================================
# 6. Procedural Data (SICP §2.1.3)
# ============================================================================

class TestProceduralData:
    """Data represented purely as procedures — SICP §2.1.3."""

    def test_cons_car_cdr(self, e):
        """Pairs as procedures."""
        e.eval("""
            my_cons := function(x, y)
                function(m)
                    cond(m == 0: x, m == 1: y,
                         else: error("Argument not 0 or 1"));
            my_car := function(z) z(0);
            my_cdr := function(z) z(1);
        """)
        e.eval("p := my_cons(1, 2);")
        assert e.eval("my_car(p);") == 1
        assert e.eval("my_cdr(p);") == 2

        # Nested pairs: ((1 . 2) . 3)
        e.eval("p2 := my_cons(my_cons(1, 2), 3);")
        assert e.eval("my_car(my_car(p2));") == 1
        assert e.eval("my_cdr(my_car(p2));") == 2
        assert e.eval("my_cdr(p2);") == 3

    def test_church_numerals(self, e):
        """SICP Exercise 2.6 — Church numerals."""
        e.eval("""
            zero  := function(f) function(x) x;
            add_1 := function(n) function(f) function(x) f(n(f)(x));
            one   := add_1(zero);
            two   := add_1(one);
            three := add_1(two);
            to_int := function(n) n(function(x) x + 1)(0);
        """)
        assert e.eval("to_int(zero);") == 0
        assert e.eval("to_int(one);") == 1
        assert e.eval("to_int(two);") == 2
        assert e.eval("to_int(three);") == 3

        # Church addition: add(m, n) = λf.λx. m(f)(n(f)(x))
        e.eval("church_add := function(m, n) function(f) function(x) m(f)(n(f)(x));")
        e.eval("five := church_add(two, three);")
        assert e.eval("to_int(five);") == 5

    def test_mutable_cons(self, e):
        """SICP §3.3.1 — mutable pairs via closures."""
        e.eval("""
            mcons := function(x, y) {
                set_x := function(v) { x = v; };
                set_y := function(v) { y = v; };
                function(m)
                    cond(m == 'car: x,
                         m == 'cdr: y,
                         m == 'set_car: set_x,
                         m == 'set_cdr: set_y,
                         else: error("Unknown op"));
            };
            mcar     := function(z) z('car);
            mcdr     := function(z) z('cdr);
            set_mcar := function(z, v) z('set_car)(v);
            set_mcdr := function(z, v) z('set_cdr)(v);
        """)
        e.eval("p := mcons(1, 2);")
        assert e.eval("mcar(p);") == 1
        assert e.eval("mcdr(p);") == 2
        e.eval("set_mcar(p, 10);")
        e.eval("set_mcdr(p, 20);")
        assert e.eval("mcar(p);") == 10
        assert e.eval("mcdr(p);") == 20


# ============================================================================
# 7. Complex SICP Examples
# ============================================================================

class TestSICPComplex:
    """Complete SICP examples combining multiple concepts."""

    def test_make_serializer(self, e):
        """SICP §3.4 style — serialized access (sequential execution)."""
        e.eval("""
            make_serializer := function() {
                locked := false;
                function(proc) function(.. args) {
                    while(locked) {};
                    locked = true;
                    result := apply(proc, args);
                    locked = false;
                    result;
                };
            };
        """)
        e.eval("ser := make_serializer();")
        e.eval("safe_add := ser(function(a, b) a + b);")
        assert e.eval("safe_add(3, 4);") == 7

    def test_rational_numbers(self, e):
        """SICP §2.1.1 — rational number arithmetic."""
        e.eval("""
            my_gcd := function(a, b)
                if(b == 0) a else my_gcd(b, a % b);
            abs_val := function(x) if(x < 0) -x else x;
        """)
        e.eval("""
            Rational := constructor(n, d) {
                g := my_gcd(abs_val(n), abs_val(d));
                numer := n / g;
                denom := d / g;
                interface(
                    numer: numer,
                    denom: denom,
                    add: function(other) Rational(
                        numer * other->denom + other->numer * denom,
                        denom * other->denom),
                    sub: function(other) Rational(
                        numer * other->denom - other->numer * denom,
                        denom * other->denom),
                    mul: function(other) Rational(
                        numer * other->numer,
                        denom * other->denom),
                    eq: function(other)
                        numer * other->denom == other->numer * denom,
                    to_float: function() numer / denom
                );
            };
        """)
        e.eval("r1 := Rational(1, 3);")
        e.eval("r2 := Rational(1, 6);")
        e.eval("r3 := r1->add(r2);")  # 1/3 + 1/6 = 1/2
        assert e.eval("r3->numer;") == 1
        assert e.eval("r3->denom;") == 2

        e.eval("r4 := r1->mul(r2);")  # 1/3 * 1/6 = 1/18
        assert e.eval("r4->numer;") == 1
        assert e.eval("r4->denom;") == 18

        assert e.eval("Rational(2, 4)->eq(Rational(1, 2));") is True

    def test_complex_numbers(self, e):
        """SICP §2.4.1 — rectangular complex numbers."""
        e.eval("""
            Complex := constructor(re, im)
                interface(
                    real: re,
                    imag: im,
                    magnitude: function() (re**2 + im**2)**0.5,
                    add: function(other) Complex(re + other->real, im + other->imag),
                    sub: function(other) Complex(re - other->real, im - other->imag),
                    mul: function(other) Complex(
                        re * other->real - im * other->imag,
                        re * other->imag + im * other->real),
                    eq: function(other) re == other->real && im == other->imag
                );
        """)
        e.eval("c1 := Complex(3, 4);")
        e.eval("c2 := Complex(1, 2);")

        assert e.eval("c1->magnitude();") == 5.0

        e.eval("c3 := c1->add(c2);")
        assert e.eval("c3->real;") == 4
        assert e.eval("c3->imag;") == 6

        e.eval("c4 := c1->mul(c2);")
        # (3+4i)(1+2i) = 3+6i+4i+8i² = 3+10i-8 = -5+10i
        assert e.eval("c4->real;") == -5
        assert e.eval("c4->imag;") == 10

    def test_evaluator_pattern(self, e):
        """Mini expression evaluator — data-directed dispatch on type tags."""
        e.eval("""
            make_num  := function(n) [n, 'num];
            make_add  := function(a, b) [a, b, 'add];
            make_mul  := function(a, b) [a, b, 'mul];
        """)
        e.eval("""
            eval_expr := function(expr) {
                tag := car(cdr(expr));
                if(tag == 'num) car(expr)
                else if(tag == 'add) {
                    tag2 := car(cdr(cdr(expr)));
                    if(tag2 == 'add)
                        eval_expr(car(expr)) + eval_expr(car(cdr(expr)))
                    else
                        eval_expr(car(expr)) + eval_expr(car(cdr(expr)));
                } else error("Unknown expr type");
            };
        """)
        # Simpler approach: tag at end
        e.eval("""
            Eval_expr := function(expr) {
                tag := last(expr);
                cond(
                    tag == 'num: car(expr),
                    tag == 'add: Eval_expr(car(expr)) + Eval_expr(car(cdr(expr))),
                    tag == 'mul: Eval_expr(car(expr)) * Eval_expr(car(cdr(expr))),
                    else: error("Unknown type")
                );
            };
            last := function(lst)
                if(null?(cdr(lst))) car(lst)
                else last(cdr(lst));
        """)
        # 2 + 3
        e.eval("expr := make_add(make_num(2), make_num(3));")
        assert e.eval("Eval_expr(expr);") == 5
        # 2 * (3 + 4) = 14
        e.eval("expr2 := make_mul(make_num(2), make_add(make_num(3), make_num(4)));")
        assert e.eval("Eval_expr(expr2);") == 14


# ============================================================================
# 8. Record Types  (Built-in)
# ============================================================================

class TestRecordTypes:
    """Eval's `record` declaration — lightweight struct-like types."""

    def test_basic_record(self, e):
        e.eval("record Point(x, y);")
        e.eval("p := make_Point(3, 4);")
        assert e.eval("Point_x(p);") == 3
        assert e.eval("Point_y(p);") == 4
        assert e.eval("Point?(p);") is True
        assert e.eval("Point?(42);") is False

    def test_record_vs_constructor(self, e):
        """Records are simpler but less powerful than constructor/interface."""
        e.eval("record Vec2(x, y);")
        e.eval("v := make_Vec2(10, 20);")
        assert e.eval("Vec2_x(v);") == 10
        assert e.eval("Vec2_y(v);") == 20

        # Records don't have methods — use standalone functions
        e.eval("vec2_add := function(a, b) make_Vec2(Vec2_x(a) + Vec2_x(b), Vec2_y(a) + Vec2_y(b));")
        e.eval("v2 := vec2_add(v, make_Vec2(1, 2));")
        assert e.eval("Vec2_x(v2);") == 11
        assert e.eval("Vec2_y(v2);") == 22


# ============================================================================
# 9. Python Interop with Objects
# ============================================================================

class TestPythonInterop:
    """Combining Eval objects with Python functions."""

    def test_python_method_in_interface(self, e):
        """Register Python functions as methods via define_function."""
        e.define_function("py_sqrt", lambda x: x**0.5, 1)
        e.eval("""
            Vector := constructor(x, y)
                interface(
                    x: x,
                    y: y,
                    length: function() py_sqrt(x**2 + y**2)
                );
        """)
        e.eval("v := Vector(3, 4);")
        assert e.eval("v->length();") == 5.0

    def test_eval_objects_from_python(self, e):
        """Create and interact with Eval objects from Python."""
        e.eval("""
            Pair := constructor(a, b)
                interface(
                    first:  a,
                    second: b,
                    sum:    function() a + b
                );
        """)
        # Create via eval, then access via eval
        e.eval("p := Pair(10, 20);")
        assert e.eval("p->first;") == 10
        assert e.eval("p->second;") == 20
        assert e.eval("p->sum();") == 30

        # Inject Python data into Eval objects
        e["data"] = [1, 2, 3, 4, 5]
        e.eval("""
            Stats := constructor(lst) {
                total := fold(+, 0, lst);
                n     := length(lst);
                interface(
                    sum:  function() total,
                    count: function() n,
                    mean: function() total / n
                );
            };
        """)
        e.eval("st := Stats(data);")
        assert e.eval("st->sum();") == 15
        assert e.eval("st->count();") == 5
        assert e.eval("st->mean();") == 3
