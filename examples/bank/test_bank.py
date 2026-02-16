"""Bank account hierarchy example — tests loading .eval files from Python.

Demonstrates:
  - Loading multiple .eval files that define OO hierarchies
  - constructor/interface-based OOP with delegation
  - Higher-order functions (fold, map, filter) over object collections
  - Closures capturing mutable state
  - Error handling for business rules
"""

import os
import pytest
from chibi_eval import Eval, EvalError

BANK_DIR = os.path.dirname(os.path.abspath(__file__))


@pytest.fixture
def e():
    ev = Eval()
    ev.load(os.path.join(BANK_DIR, "account.eval"))
    ev.load(os.path.join(BANK_DIR, "bank.eval"))
    return ev


# ============================================================================
# Account basics
# ============================================================================

class TestAccount:
    def test_create(self, e):
        e.eval('a := Account("Alice", 1000);')
        assert e.eval('a->owner();') == "Alice"
        assert e.eval('a->balance();') == 1000
        assert e.eval('a->type();') == "Account"

    def test_deposit(self, e):
        e.eval('a := Account("Alice", 1000);')
        assert e.eval('a->deposit(500);') == 1500
        assert e.eval('a->balance();') == 1500

    def test_withdraw(self, e):
        e.eval('a := Account("Alice", 1000);')
        assert e.eval('a->withdraw(300);') == 700
        assert e.eval('a->balance();') == 700

    def test_insufficient_funds(self, e):
        e.eval('a := Account("Alice", 100);')
        with pytest.raises(EvalError, match="insufficient funds"):
            e.eval('a->withdraw(200);')

    def test_negative_deposit(self, e):
        e.eval('a := Account("Alice", 100);')
        with pytest.raises(EvalError, match="amount must be positive"):
            e.eval('a->deposit(-50);')

    def test_history(self, e):
        e.eval('a := Account("Alice", 1000);')
        e.eval('a->deposit(200);')
        e.eval('a->withdraw(50);')
        hist = e.eval('a->history();')
        assert len(hist) == 2
        assert hist[0][0] == "deposit"
        assert hist[0][1] == 200
        assert hist[1][0] == "withdraw"
        assert hist[1][1] == 50

    def test_multiple_accounts_independent(self, e):
        e.eval('a := Account("Alice", 1000);')
        e.eval('b := Account("Bob", 500);')
        e.eval('a->withdraw(100);')
        assert e.eval('a->balance();') == 900
        assert e.eval('b->balance();') == 500


# ============================================================================
# SavingsAccount
# ============================================================================

class TestSavingsAccount:
    def test_create(self, e):
        e.eval('s := SavingsAccount("Alice", 1000, 0.05);')
        assert e.eval('s->owner();') == "Alice"
        assert e.eval('s->balance();') == 1000
        assert e.eval('s->type();') == "SavingsAccount"
        assert e.eval('s->rate();') == 0.05

    def test_deposit(self, e):
        e.eval('s := SavingsAccount("Alice", 1000, 0.05);')
        assert e.eval('s->deposit(500);') == 1500

    def test_accrue_interest(self, e):
        e.eval('s := SavingsAccount("Alice", 1000, 0.05);')
        interest = e.eval('s->accrue_interest();')
        assert interest == 50.0
        assert e.eval('s->balance();') == 1050.0

    def test_min_balance_enforced(self, e):
        e.eval('s := SavingsAccount("Alice", 500, 0.05);')
        # Can withdraw down to 100
        e.eval('s->withdraw(400);')
        assert e.eval('s->balance();') == 100
        # Cannot go below 100
        with pytest.raises(EvalError, match="minimum balance"):
            e.eval('s->withdraw(1);')

    def test_compound_interest(self, e):
        """Accrue interest twice — interest compounds."""
        e.eval('s := SavingsAccount("Alice", 1000, 0.10);')
        e.eval('s->accrue_interest();')  # 1000 * 0.10 = 100 → 1100
        assert e.eval('s->balance();') == 1100.0
        e.eval('s->accrue_interest();')  # 1100 * 0.10 = 110 → 1210
        assert e.eval('s->balance();') == 1210.0


# ============================================================================
# CheckingAccount
# ============================================================================

class TestCheckingAccount:
    def test_create(self, e):
        e.eval('c := CheckingAccount("Bob", 500, 200);')
        assert e.eval('c->owner();') == "Bob"
        assert e.eval('c->balance();') == 500
        assert e.eval('c->type();') == "CheckingAccount"
        assert e.eval('c->overdraft_limit();') == 200

    def test_normal_withdraw(self, e):
        e.eval('c := CheckingAccount("Bob", 500, 200);')
        e.eval('c->withdraw(300);')
        assert e.eval('c->balance();') == 200
        assert e.eval('c->overdraft_used();') == 0

    def test_overdraft(self, e):
        e.eval('c := CheckingAccount("Bob", 500, 200);')
        e.eval('c->withdraw(600);')
        assert e.eval('c->balance();') == -100
        assert e.eval('c->overdraft_used();') == 100

    def test_overdraft_limit(self, e):
        e.eval('c := CheckingAccount("Bob", 500, 200);')
        with pytest.raises(EvalError, match="exceeds overdraft limit"):
            e.eval('c->withdraw(800);')

    def test_deposit_repays_overdraft(self, e):
        e.eval('c := CheckingAccount("Bob", 500, 200);')
        e.eval('c->withdraw(600);')  # balance -100, overdraft 100
        e.eval('c->deposit(50);')     # repay 50 of overdraft
        assert e.eval('c->overdraft_used();') == 50
        assert e.eval('c->balance();') == -50

    def test_deposit_repays_overdraft_fully(self, e):
        e.eval('c := CheckingAccount("Bob", 500, 200);')
        e.eval('c->withdraw(600);')  # balance -100, overdraft 100
        e.eval('c->deposit(250);')    # repay 100 overdraft + 150 real deposit
        assert e.eval('c->overdraft_used();') == 0
        assert e.eval('c->balance();') == 150


# ============================================================================
# Bank manager
# ============================================================================

class TestBank:
    def test_create(self, e):
        e.eval('b := Bank("First National");')
        assert e.eval('b->name();') == "First National"
        assert e.eval('b->num_accounts();') == 0

    def test_open_accounts(self, e):
        e.eval('b := Bank("First National");')
        e.eval('id1 := b->open_account(Account("Alice", 1000));')
        e.eval('id2 := b->open_account(SavingsAccount("Bob", 2000, 0.05));')
        assert e.eval('id1;') == 1
        assert e.eval('id2;') == 2
        assert e.eval('b->num_accounts();') == 2

    def test_get_account(self, e):
        e.eval('b := Bank("First National");')
        e.eval('b->open_account(Account("Alice", 1000));')
        assert e.eval('b->get_account(1)->owner();') == "Alice"
        assert e.eval('b->get_account(1)->balance();') == 1000

    def test_total_assets(self, e):
        e.eval('b := Bank("First National");')
        e.eval('b->open_account(Account("Alice", 1000));')
        e.eval('b->open_account(Account("Bob", 2000));')
        e.eval('b->open_account(SavingsAccount("Carol", 3000, 0.05));')
        assert e.eval('b->total_assets();') == 6000

    def test_transfer(self, e):
        e.eval('b := Bank("First National");')
        e.eval('id1 := b->open_account(Account("Alice", 1000));')
        e.eval('id2 := b->open_account(Account("Bob", 500));')
        e.eval('b->transfer(id1, id2, 300);')
        assert e.eval('b->get_account(id1)->balance();') == 700
        assert e.eval('b->get_account(id2)->balance();') == 800
        # Total assets unchanged
        assert e.eval('b->total_assets();') == 1500

    def test_summary(self, e):
        e.eval('b := Bank("First National");')
        e.eval('b->open_account(Account("Alice", 1000));')
        e.eval('b->open_account(SavingsAccount("Bob", 2000, 0.05));')
        summary = e.eval('b->summary();')
        assert len(summary) == 2
        assert summary[0] == [1, "Alice", "Account", 1000]
        assert summary[1] == [2, "Bob", "SavingsAccount", 2000]

    def test_wealthy_accounts(self, e):
        e.eval('b := Bank("First National");')
        e.eval('b->open_account(Account("Alice", 500));')
        e.eval('b->open_account(Account("Bob", 2000));')
        e.eval('b->open_account(Account("Carol", 1500));')
        wealthy = e.eval('b->wealthy_accounts(1000);')
        assert len(wealthy) == 2

    def test_accrue_all_interest(self, e):
        e.eval('b := Bank("First National");')
        e.eval('b->open_account(Account("Alice", 1000));')
        e.eval('b->open_account(SavingsAccount("Bob", 2000, 0.05));')
        e.eval('b->open_account(SavingsAccount("Carol", 3000, 0.10));')
        e.eval('b->accrue_all_interest();')
        # Alice (Account) unchanged
        assert e.eval('b->get_account(1)->balance();') == 1000
        # Bob: 2000 * 0.05 = 100 → 2100
        assert e.eval('b->get_account(2)->balance();') == 2100.0
        # Carol: 3000 * 0.10 = 300 → 3300
        assert e.eval('b->get_account(3)->balance();') == 3300.0

    def test_account_not_found(self, e):
        e.eval('b := Bank("First National");')
        with pytest.raises(EvalError, match="account not found"):
            e.eval('b->get_account(99);')


# ============================================================================
# Cross-account scenarios
# ============================================================================

class TestCrossAccountScenarios:
    def test_transfer_standalone(self, e):
        """The standalone transfer function."""
        e.eval('a := Account("Alice", 1000);')
        e.eval('b := Account("Bob", 500);')
        e.eval('transfer(a, b, 250);')
        assert e.eval('a->balance();') == 750
        assert e.eval('b->balance();') == 750

    def test_savings_to_checking_transfer(self, e):
        """Transfer from savings to checking across account types."""
        e.eval('s := SavingsAccount("Alice", 1000, 0.05);')
        e.eval('c := CheckingAccount("Bob", 200, 500);')
        e.eval('transfer(s, c, 300);')
        assert e.eval('s->balance();') == 700
        assert e.eval('c->balance();') == 500

    def test_overdraft_then_deposit_sequence(self, e):
        """Checking: go into overdraft, then recover with deposits."""
        e.eval('c := CheckingAccount("Carol", 100, 300);')
        e.eval('c->withdraw(250);')  # -150 (overdraft 150)
        assert e.eval('c->balance();') == -150
        e.eval('c->deposit(100);')   # repay 100 of overdraft
        assert e.eval('c->balance();') == -50
        assert e.eval('c->overdraft_used();') == 50
        e.eval('c->deposit(200);')   # repay 50 + deposit 150
        assert e.eval('c->balance();') == 150
        assert e.eval('c->overdraft_used();') == 0

    def test_bank_with_mixed_accounts_and_operations(self, e):
        """Full scenario: open accounts, transact, accrue interest, summarize."""
        e.eval('b := Bank("Global Bank");')
        e.eval('id1 := b->open_account(SavingsAccount("Alice", 10000, 0.05));')
        e.eval('id2 := b->open_account(CheckingAccount("Bob", 2000, 1000));')
        e.eval('id3 := b->open_account(Account("Carol", 5000));')

        # Transactions
        e.eval('b->transfer(id3, id1, 2000);')  # Carol→Alice
        e.eval('b->transfer(id2, id3, 2500);')   # Bob→Carol (500 overdraft)

        assert e.eval('b->get_account(id1)->balance();') == 12000
        assert e.eval('b->get_account(id2)->balance();') == -500
        assert e.eval('b->get_account(id3)->balance();') == 5500

        # Total should be preserved (17000)
        assert e.eval('b->total_assets();') == 17000

        # Accrue interest on savings
        e.eval('b->accrue_all_interest();')
        # Alice: 12000 * 0.05 = 600 → 12600
        assert e.eval('b->get_account(id1)->balance();') == 12600.0

        # Total now includes interest
        assert e.eval('b->total_assets();') == 17600.0
