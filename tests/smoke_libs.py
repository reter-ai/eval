"""Smoke test for all loaded libraries."""
from chibi_eval import ChibiContext

ctx = ChibiContext()

# Basic
print('Basic:', ctx.eval('1 + 2;'))

# srfi/1 list library
print('fold:', ctx.eval('fold(+, 0, list(1,2,3,4,5));'))
print('filter:', ctx.eval('filter(function(x) x > 3, list(1,2,3,4,5));'))
print('take:', ctx.eval('take(list(1,2,3,4,5), 3);'))
print('drop:', ctx.eval('drop(list(1,2,3,4,5), 2);'))
print('iota:', ctx.eval('iota(5);'))
print('zip:', ctx.eval('zip(list(1,2,3), list("a","b","c"));'))
print('partition:', ctx.eval('`partition`(function(x) x > 3, list(1,5,2,4,3));'))
print('count:', ctx.eval('count(function(x) x > 3, list(1,5,2,4,3));'))
print('unfold:', ctx.eval('unfold(function(x) x > 5, function(x) x * x, function(x) x + 1, 1);'))
print('alist-cons:', ctx.eval('`alist-cons`("key", "val", list());'))
print('delete-dups:', ctx.eval('`delete-duplicates`(list(1,2,1,3,2,4));'))
print('lset-union:', ctx.eval('`lset-union`(`equal?`, list(1,2,3), list(2,3,4));'))
print('reduce:', ctx.eval('reduce(+, 0, list(1,2,3,4,5));'))
print('concatenate:', ctx.eval('concatenate(list(list(1,2), list(3,4), list(5)));'))
print('append-map:', ctx.eval('`append-map`(function(x) list(x, x * x), list(1,2,3));'))
print('xcons:', ctx.eval('xcons(1, 2);'))
print('last:', ctx.eval('last(list(1,2,3,4));'))

# srfi/69 hash tables
print('hash-table:', ctx.eval('''
    define h = `make-hash-table`();
    `hash-table-set!`(h, "key", 42);
    `hash-table-ref`(h, "key");
'''))

# Underscore aliases for hash tables
print('hash_table:', ctx.eval('''
    define ht = make_hash_table();
    hash_table_set(ht, "x", 99);
    hash_table_ref(ht, "x");
'''))
print('hash_table_keys:', ctx.eval('''
    define ht2 = make_hash_table();
    hash_table_set(ht2, "a", 1);
    hash_table_set(ht2, "b", 2);
    sort(hash_table_keys(ht2), `string<?`);
'''))

# srfi/95 sort
print('sort:', ctx.eval('sort(list(5,3,1,4,2), <);'))
print('sorted?:', ctx.eval('`sorted?`(list(1,2,3,4,5), <);'))

# scheme/cxr
print('caddr:', ctx.eval('caddr(list(1,2,3,4));'))
print('cadddr:', ctx.eval('cadddr(list(1,2,3,4,5));'))

# scheme/inexact
print('finite?:', ctx.eval('`finite?`(42);'))
print('infinite?:', ctx.eval('`infinite?`(1.0 / 0.0);'))

# scheme/division
print('ceiling-quotient:', ctx.eval('`ceiling-quotient`(7, 3);'))
print('euclidean-rem:', ctx.eval('`euclidean-remainder`(7, 3);'))

# prime
print('prime?:', ctx.eval('`prime?`(17);'))
print('nth-prime:', ctx.eval('`nth-prime`(10);'))
print('prime-above:', ctx.eval('`prime-above`(100);'))
print('factor:', ctx.eval('`factor`(360);'))
print('totient:', ctx.eval('`totient`(12);'))

# Underscore aliases
print('is_prime:', ctx.eval('is_prime(31);'))
print('random_int:', ctx.eval('random_integer(100) < 100;'))

# equiv?
print('equiv?:', ctx.eval('`equiv?`(list(1,2,3), list(1,2,3));'))

# chibi/bytevector
print('bv-u16:', ctx.eval('''
    define bv = `make-bytevector`(4, 0);
    `bytevector-u8-set!`(bv, 0, 0xAB);
    `bytevector-u8-set!`(bv, 1, 0xCD);
    `bytevector-u16-ref-le`(bv, 0);
'''))
print('int->bv:', ctx.eval('`integer->bytevector`(256);'))

# scheme/time
print('clock:', ctx.eval('`current-clock-second`() > 0;'))

# srfi/98 env
print('get-env:', ctx.eval('`string?`(`get-environment-variable`("PATH"));'))

# srfi/27 random
print('random:', ctx.eval('random_integer(1000);'))
print('random-real:', ctx.eval('`random-real`();'))

# chibi/json
print('json-read:', ctx.eval('''
    define port = `open-input-string`("[1,2,3]");
    `json-read`(port);
'''))

# define-record-type (macro — must be used via Scheme, not Eval syntax)
# The Hash-Table type from srfi/69 was defined via define-record-type, proving it works
print('record:', ctx.eval('`hash-table?`(`make-hash-table`());'))

# chibi/weak - make-ephemeron is available
print('weak:', ctx.eval('`ephemeron?`(`make-ephemeron`("key", "value"));'))

# srfi/151 bitwise
print('bit-and:', ctx.eval('`bit-and`(0xFF, 0x0F);'))
print('arith-shift:', ctx.eval('`arithmetic-shift`(1, 10);'))

# AST introspection
print('type-of:', ctx.eval('`type-of`(42);'))

print('\nAll smoke tests passed!')
