/*  _eval_decimal.c -- Arbitrary-precision Decimal type for Eval
 *
 *  Representation: value = mantissa × 10^(-scale)
 *    mantissa: fixnum or bignum (GC-traced via simple type slot 0)
 *    scale: non-negative fixnum (slot 1)
 *
 *  Example: Decimal("12.345") → mantissa=12345, scale=3
 *
 *  Uses sexp_register_simple_type with 2 GC-traced fields so the
 *  bignum mantissa is properly traced by the garbage collector.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <chibi/eval.h>
#include <chibi/bignum.h>

/* ================================================================
 * Type registration
 * ================================================================ */

static sexp decimal_type = SEXP_FALSE;
static sexp_uint_t decimal_type_tag = 0;

void register_decimal_type(sexp ctx, sexp env) {
    sexp_gc_var3(name, slots, sym);
    sexp_gc_preserve3(ctx, name, slots, sym);

    /* Build slots list: (mantissa scale) */
    sym = sexp_intern(ctx, "scale", -1);
    slots = sexp_cons(ctx, sym, SEXP_NULL);
    sym = sexp_intern(ctx, "mantissa", -1);
    slots = sexp_cons(ctx, sym, slots);

    name = sexp_c_string(ctx, "decimal", -1);
    decimal_type = sexp_register_simple_type(ctx, name, SEXP_FALSE, slots);

    if (sexp_typep(decimal_type)) {
        decimal_type_tag = sexp_type_tag(decimal_type);
        sexp_preserve_object(ctx, decimal_type);
    }

    sexp_gc_release3(ctx);
}

/* ================================================================
 * Helpers
 * ================================================================ */

static int is_decimal(sexp x) {
    return sexp_pointerp(x) && sexp_pointer_tag(x) == decimal_type_tag;
}

static sexp decimal_mantissa(sexp x) { return sexp_slot_ref(x, 0); }
static sexp decimal_scale(sexp x)    { return sexp_slot_ref(x, 1); }

static sexp make_decimal(sexp ctx, sexp mantissa, int scale) {
    sexp_gc_var1(d);
    sexp_gc_preserve1(ctx, d);
    d = sexp_alloc_type(ctx, pair, decimal_type_tag);
    sexp_slot_set(d, 0, mantissa);
    sexp_slot_set(d, 1, sexp_make_fixnum(scale));
    sexp_gc_release1(ctx);
    return d;
}

/* Compute 10^n as a fixnum/bignum */
static sexp pow10(sexp ctx, int n) {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);
    result = sexp_make_fixnum(1);
    for (int i = 0; i < n; i++)
        result = sexp_mul(ctx, result, sexp_make_fixnum(10));
    sexp_gc_release1(ctx);
    return result;
}

/* Normalize two decimals to the same scale (max of the two).
 * Modifies *a_man and *b_man, sets *out_sc. */
static void normalize_scales(sexp ctx, sexp a, sexp b,
                              sexp *out_a, sexp *out_b, int *out_sc) {
    int a_sc = (int)sexp_unbox_fixnum(decimal_scale(a));
    int b_sc = (int)sexp_unbox_fixnum(decimal_scale(b));
    sexp a_man = decimal_mantissa(a);
    sexp b_man = decimal_mantissa(b);

    if (a_sc == b_sc) {
        *out_a = a_man;
        *out_b = b_man;
        *out_sc = a_sc;
    } else if (a_sc < b_sc) {
        *out_a = sexp_mul(ctx, a_man, pow10(ctx, b_sc - a_sc));
        *out_b = b_man;
        *out_sc = b_sc;
    } else {
        *out_a = a_man;
        *out_b = sexp_mul(ctx, b_man, pow10(ctx, a_sc - b_sc));
        *out_sc = a_sc;
    }
}

/* ================================================================
 * Bridge functions — constructors
 * ================================================================ */

/* %make-decimal(mantissa, scale) */
static sexp bridge_make_decimal(sexp ctx, sexp self, sexp_sint_t n,
                                 sexp smant, sexp sscale) {
    int scale = (int)sexp_unbox_fixnum(sscale);
    return make_decimal(ctx, smant, scale);
}

/* %decimal-from-string(s) — parse "1.23", "-0.005", "100" */
static sexp bridge_decimal_from_string(sexp ctx, sexp self, sexp_sint_t n, sexp sstr) {
    if (!sexp_stringp(sstr))
        return sexp_user_exception(ctx, SEXP_FALSE, "expected string", sstr);

    const char *s = sexp_string_data(sstr);
    size_t len = sexp_string_size(sstr);

    /* Find the decimal point */
    const char *dot = NULL;
    for (size_t i = 0; i < len; i++) {
        if (s[i] == '.') { dot = &s[i]; break; }
    }

    int scale = 0;
    sexp_gc_var2(mantissa, snum);
    sexp_gc_preserve2(ctx, mantissa, snum);

    if (dot) {
        scale = (int)(len - (size_t)(dot - s) - 1);
        /* Build string without the dot */
        char *buf = (char *)malloc(len); /* len is enough (minus dot, plus NUL) */
        size_t j = 0;
        for (size_t i = 0; i < len; i++) {
            if (s[i] != '.') buf[j++] = s[i];
        }
        buf[j] = '\0';
        /* Parse via chibi's string->number */
        snum = sexp_c_string(ctx, buf, (sexp_sint_t)j);
        mantissa = sexp_string_to_number(ctx, snum, sexp_make_fixnum(10));
        free(buf);
    } else {
        /* No dot — integer */
        snum = sexp_c_string(ctx, s, (sexp_sint_t)len);
        mantissa = sexp_string_to_number(ctx, snum, sexp_make_fixnum(10));
    }

    if (!sexp_fixnump(mantissa) && !sexp_bignump(mantissa)) {
        sexp_gc_release2(ctx);
        return sexp_user_exception(ctx, SEXP_FALSE, "invalid decimal string", sstr);
    }

    sexp result = make_decimal(ctx, mantissa, scale);
    sexp_gc_release2(ctx);
    return result;
}

/* %decimal-from-number(n) — fixnum/flonum → Decimal */
static sexp bridge_decimal_from_number(sexp ctx, sexp self, sexp_sint_t n, sexp snum) {
    if (sexp_fixnump(snum) || sexp_bignump(snum)) {
        return make_decimal(ctx, snum, 0);
    }
    if (sexp_flonump(snum)) {
        /* Convert via string round-trip for exactness */
        char buf[64];
        snprintf(buf, sizeof(buf), "%.17g", sexp_flonum_value(snum));
        sexp_gc_var1(ss);
        sexp_gc_preserve1(ctx, ss);
        ss = sexp_c_string(ctx, buf, -1);
        sexp result = bridge_decimal_from_string(ctx, self, n, ss);
        sexp_gc_release1(ctx);
        return result;
    }
    return sexp_user_exception(ctx, SEXP_FALSE, "expected number", snum);
}

/* ================================================================
 * Bridge functions — arithmetic
 * ================================================================ */

/* %decimal-add(a, b) */
static sexp bridge_decimal_add(sexp ctx, sexp self, sexp_sint_t n, sexp sa, sexp sb) {
    if (!is_decimal(sa)) return sexp_user_exception(ctx, SEXP_FALSE, "expected decimal", sa);
    if (!is_decimal(sb)) return sexp_user_exception(ctx, SEXP_FALSE, "expected decimal", sb);

    sexp_gc_var2(am, bm);
    sexp_gc_preserve2(ctx, am, bm);
    int sc;
    normalize_scales(ctx, sa, sb, &am, &bm, &sc);
    sexp r_man = sexp_add(ctx, am, bm);
    sexp_gc_release2(ctx);
    return make_decimal(ctx, r_man, sc);
}

/* %decimal-sub(a, b) */
static sexp bridge_decimal_sub(sexp ctx, sexp self, sexp_sint_t n, sexp sa, sexp sb) {
    if (!is_decimal(sa)) return sexp_user_exception(ctx, SEXP_FALSE, "expected decimal", sa);
    if (!is_decimal(sb)) return sexp_user_exception(ctx, SEXP_FALSE, "expected decimal", sb);

    sexp_gc_var2(am, bm);
    sexp_gc_preserve2(ctx, am, bm);
    int sc;
    normalize_scales(ctx, sa, sb, &am, &bm, &sc);
    sexp r_man = sexp_sub(ctx, am, bm);
    sexp_gc_release2(ctx);
    return make_decimal(ctx, r_man, sc);
}

/* %decimal-mul(a, b) */
static sexp bridge_decimal_mul(sexp ctx, sexp self, sexp_sint_t n, sexp sa, sexp sb) {
    if (!is_decimal(sa)) return sexp_user_exception(ctx, SEXP_FALSE, "expected decimal", sa);
    if (!is_decimal(sb)) return sexp_user_exception(ctx, SEXP_FALSE, "expected decimal", sb);

    sexp a_man = decimal_mantissa(sa);
    sexp b_man = decimal_mantissa(sb);
    int a_sc = (int)sexp_unbox_fixnum(decimal_scale(sa));
    int b_sc = (int)sexp_unbox_fixnum(decimal_scale(sb));

    sexp r_man = sexp_mul(ctx, a_man, b_man);
    return make_decimal(ctx, r_man, a_sc + b_sc);
}

/* %decimal-div(a, b, precision) */
static sexp bridge_decimal_div(sexp ctx, sexp self, sexp_sint_t n,
                                sexp sa, sexp sb, sexp sprec) {
    if (!is_decimal(sa)) return sexp_user_exception(ctx, SEXP_FALSE, "expected decimal", sa);
    if (!is_decimal(sb)) return sexp_user_exception(ctx, SEXP_FALSE, "expected decimal", sb);

    int prec = (int)sexp_unbox_fixnum(sprec);

    sexp_gc_var4(am, bm, p, half);
    sexp_gc_preserve4(ctx, am, bm, p, half);

    int a_sc = (int)sexp_unbox_fixnum(decimal_scale(sa));
    int b_sc = (int)sexp_unbox_fixnum(decimal_scale(sb));
    am = decimal_mantissa(sa);
    bm = decimal_mantissa(sb);

    /* We want: result_mantissa = (a_man * 10^(prec + b_sc - a_sc)) / b_man
     * result_scale = prec */
    int shift = prec + b_sc - a_sc;
    if (shift > 0) {
        p = pow10(ctx, shift);
        am = sexp_mul(ctx, am, p);
    } else if (shift < 0) {
        p = pow10(ctx, -shift);
        bm = sexp_mul(ctx, bm, p);
    }

    /* Round half-up: add sign(a)*|b|/2 before dividing */
    half = sexp_quotient(ctx, bm, sexp_make_fixnum(2));

    /* Determine sign of numerator for rounding direction */
    int neg = sexp_negativep(am) ? 1 : 0;
    if (neg)
        am = sexp_sub(ctx, am, half);
    else
        am = sexp_add(ctx, am, half);

    sexp r_man = sexp_quotient(ctx, am, bm);
    sexp_gc_release4(ctx);
    return make_decimal(ctx, r_man, prec);
}

/* %decimal-negate(a) */
static sexp bridge_decimal_negate(sexp ctx, sexp self, sexp_sint_t n, sexp sa) {
    if (!is_decimal(sa)) return sexp_user_exception(ctx, SEXP_FALSE, "expected decimal", sa);

    sexp man = decimal_mantissa(sa);
    int sc = (int)sexp_unbox_fixnum(decimal_scale(sa));

    /* Negate: multiply by -1 (safe, doesn't mutate) */
    sexp neg = sexp_sub(ctx, sexp_make_fixnum(0), man);
    return make_decimal(ctx, neg, sc);
}

/* %decimal-abs(a) */
static sexp bridge_decimal_abs(sexp ctx, sexp self, sexp_sint_t n, sexp sa) {
    if (!is_decimal(sa)) return sexp_user_exception(ctx, SEXP_FALSE, "expected decimal", sa);

    sexp man = decimal_mantissa(sa);
    int sc = (int)sexp_unbox_fixnum(decimal_scale(sa));

    if (sexp_negativep(man)) {
        sexp neg = sexp_sub(ctx, sexp_make_fixnum(0), man);
        return make_decimal(ctx, neg, sc);
    }
    return sa;
}

/* %decimal-compare(a, b) → -1/0/1 */
static sexp bridge_decimal_compare(sexp ctx, sexp self, sexp_sint_t n, sexp sa, sexp sb) {
    if (!is_decimal(sa)) return sexp_user_exception(ctx, SEXP_FALSE, "expected decimal", sa);
    if (!is_decimal(sb)) return sexp_user_exception(ctx, SEXP_FALSE, "expected decimal", sb);

    sexp_gc_var2(am, bm);
    sexp_gc_preserve2(ctx, am, bm);
    int sc;
    normalize_scales(ctx, sa, sb, &am, &bm, &sc);
    sexp cmp = sexp_compare(ctx, am, bm);
    sexp_gc_release2(ctx);
    return cmp;
}

/* ================================================================
 * Bridge functions — rounding
 * ================================================================ */

/* %decimal-round(a, places, mode)
 * mode: 0=half-up, 1=ceil, 2=floor, 3=truncate */
static sexp bridge_decimal_round(sexp ctx, sexp self, sexp_sint_t n,
                                  sexp sa, sexp splaces, sexp smode) {
    if (!is_decimal(sa)) return sexp_user_exception(ctx, SEXP_FALSE, "expected decimal", sa);

    int places = (int)sexp_unbox_fixnum(splaces);
    int mode = (int)sexp_unbox_fixnum(smode);
    int cur_sc = (int)sexp_unbox_fixnum(decimal_scale(sa));
    sexp man = decimal_mantissa(sa);

    if (places >= cur_sc) {
        /* Already has enough precision, just adjust scale */
        if (places == cur_sc) return sa;
        sexp_gc_var1(p);
        sexp_gc_preserve1(ctx, p);
        p = pow10(ctx, places - cur_sc);
        sexp new_man = sexp_mul(ctx, man, p);
        sexp_gc_release1(ctx);
        return make_decimal(ctx, new_man, places);
    }

    /* Need to reduce scale */
    int shift = cur_sc - places;
    sexp_gc_var2(divisor, rem);
    sexp_gc_preserve2(ctx, divisor, rem);

    divisor = pow10(ctx, shift);
    sexp q = sexp_quotient(ctx, man, divisor);
    rem = sexp_remainder(ctx, man, divisor);

    switch (mode) {
    case 0: { /* half-up */
        sexp half = sexp_quotient(ctx, divisor, sexp_make_fixnum(2));
        if (sexp_negativep(man)) {
            /* For negative: round away from zero if |rem| >= half */
            sexp abs_rem = sexp_sub(ctx, sexp_make_fixnum(0), rem);
            if (sexp_negativep(rem)) abs_rem = sexp_sub(ctx, sexp_make_fixnum(0), rem);
            else abs_rem = rem;
            sexp cmp = sexp_compare(ctx, abs_rem, half);
            if (sexp_unbox_fixnum(cmp) >= 0)
                q = sexp_sub(ctx, q, sexp_make_fixnum(1));
        } else {
            sexp cmp = sexp_compare(ctx, rem, half);
            if (sexp_unbox_fixnum(cmp) >= 0)
                q = sexp_add(ctx, q, sexp_make_fixnum(1));
        }
        break;
    }
    case 1: /* ceil */
        /* If positive remainder, round up */
        if (!sexp_negativep(man) && !(sexp_fixnump(rem) && sexp_unbox_fixnum(rem) == 0))
            q = sexp_add(ctx, q, sexp_make_fixnum(1));
        break;
    case 2: /* floor */
        /* If negative remainder, round down */
        if (sexp_negativep(man) && !(sexp_fixnump(rem) && sexp_unbox_fixnum(rem) == 0))
            q = sexp_sub(ctx, q, sexp_make_fixnum(1));
        break;
    case 3: /* truncate — quotient is already truncated */
        break;
    }

    sexp_gc_release2(ctx);
    return make_decimal(ctx, q, places);
}

/* ================================================================
 * Bridge functions — conversion
 * ================================================================ */

/* %decimal-to-string(a) → "1.23" */
static sexp bridge_decimal_to_string(sexp ctx, sexp self, sexp_sint_t n, sexp sa) {
    if (!is_decimal(sa)) return sexp_user_exception(ctx, SEXP_FALSE, "expected decimal", sa);

    sexp man = decimal_mantissa(sa);
    int sc = (int)sexp_unbox_fixnum(decimal_scale(sa));

    /* Convert mantissa to string */
    sexp_gc_var1(man_str);
    sexp_gc_preserve1(ctx, man_str);
    man_str = sexp_write_to_string(ctx, man);
    if (!sexp_stringp(man_str)) {
        sexp_gc_release1(ctx);
        return sexp_user_exception(ctx, SEXP_FALSE, "decimal conversion error", sa);
    }

    const char *digits = sexp_string_data(man_str);
    size_t dlen = sexp_string_size(man_str);

    if (sc == 0) {
        sexp_gc_release1(ctx);
        return man_str;
    }

    /* Handle negative sign */
    int neg = (digits[0] == '-') ? 1 : 0;
    const char *abs_digits = digits + neg;
    size_t abs_len = dlen - neg;

    /* Build result with decimal point */
    size_t result_len = dlen + 2; /* room for dot and possible leading zeros */
    if ((int)abs_len <= sc)
        result_len += (sc - abs_len + 1); /* leading zeros: 0.00... */
    char *buf = (char *)malloc(result_len + 1);
    size_t pos = 0;

    if (neg) buf[pos++] = '-';

    if ((int)abs_len <= sc) {
        /* e.g. mantissa=5, scale=3 → "0.005" */
        buf[pos++] = '0';
        buf[pos++] = '.';
        for (int i = 0; i < sc - (int)abs_len; i++)
            buf[pos++] = '0';
        memcpy(buf + pos, abs_digits, abs_len);
        pos += abs_len;
    } else {
        /* Insert dot at position abs_len - sc */
        size_t int_part = abs_len - sc;
        memcpy(buf + pos, abs_digits, int_part);
        pos += int_part;
        buf[pos++] = '.';
        memcpy(buf + pos, abs_digits + int_part, sc);
        pos += sc;
    }
    buf[pos] = '\0';

    sexp result = sexp_c_string(ctx, buf, (sexp_sint_t)pos);
    free(buf);
    sexp_gc_release1(ctx);
    return result;
}

/* %decimal-to-number(a) → flonum */
static sexp bridge_decimal_to_number(sexp ctx, sexp self, sexp_sint_t n, sexp sa) {
    if (!is_decimal(sa)) return sexp_user_exception(ctx, SEXP_FALSE, "expected decimal", sa);

    /* Get string representation and parse as double */
    sexp str = bridge_decimal_to_string(ctx, self, n, sa);
    if (!sexp_stringp(str)) return str;
    double val = atof(sexp_string_data(str));
    return sexp_make_flonum(ctx, val);
}

/* %decimal-mantissa(a), %decimal-scale(a) */
static sexp bridge_decimal_mantissa(sexp ctx, sexp self, sexp_sint_t n, sexp sa) {
    if (!is_decimal(sa)) return sexp_user_exception(ctx, SEXP_FALSE, "expected decimal", sa);
    return decimal_mantissa(sa);
}
static sexp bridge_decimal_scale(sexp ctx, sexp self, sexp_sint_t n, sexp sa) {
    if (!is_decimal(sa)) return sexp_user_exception(ctx, SEXP_FALSE, "expected decimal", sa);
    return decimal_scale(sa);
}

/* %decimal?(x) */
static sexp bridge_decimal_p(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    return is_decimal(x) ? SEXP_TRUE : SEXP_FALSE;
}

/* ================================================================
 * tostr helper for bridge_tostr integration
 * ================================================================ */

sexp decimal_tostr(sexp ctx, sexp x) {
    if (!is_decimal(x)) return SEXP_FALSE;
    return bridge_decimal_to_string(ctx, SEXP_FALSE, 1, x);
}

/* ================================================================
 * Bridge registration
 * ================================================================ */

void register_decimal_bridge_functions(sexp ctx, sexp env) {
    sexp_define_foreign(ctx, env, "%make-decimal", 2, bridge_make_decimal);
    sexp_define_foreign(ctx, env, "%decimal-from-string", 1, bridge_decimal_from_string);
    sexp_define_foreign(ctx, env, "%decimal-from-number", 1, bridge_decimal_from_number);
    sexp_define_foreign(ctx, env, "%decimal-add", 2, bridge_decimal_add);
    sexp_define_foreign(ctx, env, "%decimal-sub", 2, bridge_decimal_sub);
    sexp_define_foreign(ctx, env, "%decimal-mul", 2, bridge_decimal_mul);
    sexp_define_foreign(ctx, env, "%decimal-div", 3, bridge_decimal_div);
    sexp_define_foreign(ctx, env, "%decimal-negate", 1, bridge_decimal_negate);
    sexp_define_foreign(ctx, env, "%decimal-abs", 1, bridge_decimal_abs);
    sexp_define_foreign(ctx, env, "%decimal-compare", 2, bridge_decimal_compare);
    sexp_define_foreign(ctx, env, "%decimal-round", 3, bridge_decimal_round);
    sexp_define_foreign(ctx, env, "%decimal-to-string", 1, bridge_decimal_to_string);
    sexp_define_foreign(ctx, env, "%decimal-to-number", 1, bridge_decimal_to_number);
    sexp_define_foreign(ctx, env, "%decimal-mantissa", 1, bridge_decimal_mantissa);
    sexp_define_foreign(ctx, env, "%decimal-scale", 1, bridge_decimal_scale);
    sexp_define_foreign(ctx, env, "%decimal?", 1, bridge_decimal_p);
}
