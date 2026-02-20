/*  _eval_datetime.c -- DateTime, Date, TimeDelta types for Eval
 *
 *  DateTime: epoch seconds (int64) + UTC offset in minutes
 *  Date:     year/month/day (civil date)
 *  TimeDelta: total seconds (int64)
 *
 *  All types use sexp_register_c_type + sexp_alloc_tagged + sexp_cpointer_value.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <chibi/eval.h>

#ifdef _WIN32
#include <windows.h>
#endif

/* ================================================================
 * Internal representations
 * ================================================================ */

typedef struct {
    int64_t epoch_sec;
    int32_t utc_offset_min;
} EvalDateTime;

typedef struct {
    int year, month, day;
} EvalDate;

typedef struct {
    int64_t total_seconds;
} EvalTimeDelta;

/* ================================================================
 * Type tags
 * ================================================================ */

static sexp_uint_t datetime_type_tag = 0;
static sexp_uint_t date_type_tag = 0;
static sexp_uint_t timedelta_type_tag = 0;

/* Finalizers (no-op, structs are malloc'd inside cpointer) */
static sexp datetime_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    EvalDateTime *dt = (EvalDateTime *)sexp_cpointer_value(obj);
    if (dt) { free(dt); sexp_cpointer_value(obj) = NULL; }
    return SEXP_VOID;
}
static sexp date_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    EvalDate *d = (EvalDate *)sexp_cpointer_value(obj);
    if (d) { free(d); sexp_cpointer_value(obj) = NULL; }
    return SEXP_VOID;
}
static sexp timedelta_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    EvalTimeDelta *td = (EvalTimeDelta *)sexp_cpointer_value(obj);
    if (td) { free(td); sexp_cpointer_value(obj) = NULL; }
    return SEXP_VOID;
}

/* ================================================================
 * Type registration
 * ================================================================ */

void register_datetime_types(sexp ctx) {
    sexp_gc_var2(name, type);
    sexp_gc_preserve2(ctx, name, type);

    name = sexp_c_string(ctx, "datetime", -1);
    type = sexp_register_c_type(ctx, name, datetime_finalize);
    if (sexp_typep(type)) {
        datetime_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }

    name = sexp_c_string(ctx, "date", -1);
    type = sexp_register_c_type(ctx, name, date_finalize);
    if (sexp_typep(type)) {
        date_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }

    name = sexp_c_string(ctx, "timedelta", -1);
    type = sexp_register_c_type(ctx, name, timedelta_finalize);
    if (sexp_typep(type)) {
        timedelta_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }

    sexp_gc_release2(ctx);
}

/* ================================================================
 * Wrap / unwrap helpers
 * ================================================================ */

static sexp wrap_datetime(sexp ctx, EvalDateTime *dt) {
    sexp_gc_var1(r);
    sexp_gc_preserve1(ctx, r);
    r = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), datetime_type_tag);
    sexp_cpointer_value(r) = (void *)dt;
    sexp_cpointer_length(r) = 0;
    sexp_gc_release1(ctx);
    return r;
}

static EvalDateTime *unwrap_datetime(sexp x) {
    if (sexp_pointerp(x) && sexp_pointer_tag(x) == datetime_type_tag)
        return (EvalDateTime *)sexp_cpointer_value(x);
    return NULL;
}

static sexp wrap_date(sexp ctx, EvalDate *d) {
    sexp_gc_var1(r);
    sexp_gc_preserve1(ctx, r);
    r = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), date_type_tag);
    sexp_cpointer_value(r) = (void *)d;
    sexp_cpointer_length(r) = 0;
    sexp_gc_release1(ctx);
    return r;
}

static EvalDate *unwrap_date(sexp x) {
    if (sexp_pointerp(x) && sexp_pointer_tag(x) == date_type_tag)
        return (EvalDate *)sexp_cpointer_value(x);
    return NULL;
}

static sexp wrap_timedelta(sexp ctx, EvalTimeDelta *td) {
    sexp_gc_var1(r);
    sexp_gc_preserve1(ctx, r);
    r = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), timedelta_type_tag);
    sexp_cpointer_value(r) = (void *)td;
    sexp_cpointer_length(r) = 0;
    sexp_gc_release1(ctx);
    return r;
}

static EvalTimeDelta *unwrap_timedelta(sexp x) {
    if (sexp_pointerp(x) && sexp_pointer_tag(x) == timedelta_type_tag)
        return (EvalTimeDelta *)sexp_cpointer_value(x);
    return NULL;
}

/* ================================================================
 * Platform helpers
 * ================================================================ */

/* mktime equivalent that interprets struct tm as UTC */
static time_t mkgm(struct tm *t) {
#ifdef _WIN32
    return _mkgmtime(t);
#else
    return timegm(t);
#endif
}

/* Thread-safe gmtime */
static struct tm *gmtime_safe(const time_t *t, struct tm *result) {
#ifdef _WIN32
    return gmtime_s(result, t) == 0 ? result : NULL;
#else
    return gmtime_r(t, result);
#endif
}

/* Thread-safe localtime */
static struct tm *localtime_safe(const time_t *t, struct tm *result) {
#ifdef _WIN32
    return localtime_s(result, t) == 0 ? result : NULL;
#else
    return localtime_r(t, result);
#endif
}

/* Compute struct tm from epoch_sec + utc_offset_min */
static int datetime_to_tm(EvalDateTime *dt, struct tm *out) {
    time_t adj = (time_t)(dt->epoch_sec + (int64_t)dt->utc_offset_min * 60);
    return gmtime_safe(&adj, out) != NULL;
}

/* ================================================================
 * DateTime bridge functions
 * ================================================================ */

/* %make-datetime(y, m, d, h, mi, s, offset_min) */
static sexp bridge_make_datetime(sexp ctx, sexp self, sexp_sint_t n,
        sexp sy, sexp sm, sexp sd, sexp sh, sexp smi, sexp ss, sexp soff) {
    int y = (int)sexp_unbox_fixnum(sy);
    int mo = (int)sexp_unbox_fixnum(sm);
    int d = (int)sexp_unbox_fixnum(sd);
    int h = (int)sexp_unbox_fixnum(sh);
    int mi = (int)sexp_unbox_fixnum(smi);
    int s = (int)sexp_unbox_fixnum(ss);
    int off = (int)sexp_unbox_fixnum(soff);

    struct tm t;
    memset(&t, 0, sizeof(t));
    t.tm_year = y - 1900;
    t.tm_mon = mo - 1;
    t.tm_mday = d;
    t.tm_hour = h;
    t.tm_min = mi;
    t.tm_sec = s;

    time_t epoch = mkgm(&t);
    if (epoch == (time_t)-1)
        return sexp_user_exception(ctx, SEXP_FALSE, "invalid datetime", sy);

    /* Subtract offset to get UTC epoch */
    EvalDateTime *dt = (EvalDateTime *)malloc(sizeof(EvalDateTime));
    dt->epoch_sec = (int64_t)epoch - (int64_t)off * 60;
    dt->utc_offset_min = off;
    return wrap_datetime(ctx, dt);
}

/* %datetime-now() */
static sexp bridge_datetime_now(sexp ctx, sexp self, sexp_sint_t n) {
    time_t now = time(NULL);
    struct tm local;
    localtime_safe(&now, &local);

    /* Compute UTC offset */
    struct tm utc;
    gmtime_safe(&now, &utc);
    time_t utc_epoch = mkgm(&utc);
    int offset_min = (int)((now - utc_epoch) / 60);

    EvalDateTime *dt = (EvalDateTime *)malloc(sizeof(EvalDateTime));
    dt->epoch_sec = (int64_t)now;
    dt->utc_offset_min = offset_min;
    return wrap_datetime(ctx, dt);
}

/* %datetime-utc-now() */
static sexp bridge_datetime_utc_now(sexp ctx, sexp self, sexp_sint_t n) {
    time_t now = time(NULL);
    EvalDateTime *dt = (EvalDateTime *)malloc(sizeof(EvalDateTime));
    dt->epoch_sec = (int64_t)now;
    dt->utc_offset_min = 0;
    return wrap_datetime(ctx, dt);
}

/* %datetime-from-epoch(seconds) */
static sexp bridge_datetime_from_epoch(sexp ctx, sexp self, sexp_sint_t n, sexp ssec) {
    int64_t sec;
    if (sexp_fixnump(ssec))
        sec = sexp_unbox_fixnum(ssec);
    else if (sexp_flonump(ssec))
        sec = (int64_t)sexp_flonum_value(ssec);
    else
        return sexp_user_exception(ctx, SEXP_FALSE, "expected number", ssec);

    EvalDateTime *dt = (EvalDateTime *)malloc(sizeof(EvalDateTime));
    dt->epoch_sec = sec;
    dt->utc_offset_min = 0;
    return wrap_datetime(ctx, dt);
}

/* %datetime-parse(string) — parse ISO 8601 subset: YYYY-MM-DDTHH:MM:SS[Z|±HH:MM] */
static sexp bridge_datetime_parse(sexp ctx, sexp self, sexp_sint_t n, sexp sstr) {
    if (!sexp_stringp(sstr))
        return sexp_user_exception(ctx, SEXP_FALSE, "expected string", sstr);

    const char *s = sexp_string_data(sstr);
    int y, mo, d, h, mi, sec;
    int off_h = 0, off_m = 0, off_sign = 1;
    int offset_min = 0;

    /* Parse date part */
    if (sscanf(s, "%d-%d-%d", &y, &mo, &d) != 3)
        return sexp_user_exception(ctx, SEXP_FALSE, "invalid ISO 8601 date", sstr);

    /* Find T or space separator */
    const char *tp = strchr(s, 'T');
    if (!tp) tp = strchr(s, 't');
    if (!tp) tp = strchr(s, ' ');

    h = mi = sec = 0;
    if (tp) {
        tp++;
        if (sscanf(tp, "%d:%d:%d", &h, &mi, &sec) < 2)
            return sexp_user_exception(ctx, SEXP_FALSE, "invalid ISO 8601 time", sstr);

        /* Parse timezone */
        const char *tz = tp;
        while (*tz && *tz != 'Z' && *tz != 'z' && *tz != '+' && *tz != '-')
            tz++;
        if (*tz == 'Z' || *tz == 'z') {
            offset_min = 0;
        } else if (*tz == '+' || *tz == '-') {
            off_sign = (*tz == '-') ? -1 : 1;
            tz++;
            if (sscanf(tz, "%d:%d", &off_h, &off_m) >= 1)
                offset_min = off_sign * (off_h * 60 + off_m);
        }
    }

    struct tm t;
    memset(&t, 0, sizeof(t));
    t.tm_year = y - 1900;
    t.tm_mon = mo - 1;
    t.tm_mday = d;
    t.tm_hour = h;
    t.tm_min = mi;
    t.tm_sec = sec;

    time_t epoch = mkgm(&t);
    if (epoch == (time_t)-1)
        return sexp_user_exception(ctx, SEXP_FALSE, "invalid datetime values", sstr);

    EvalDateTime *dt = (EvalDateTime *)malloc(sizeof(EvalDateTime));
    dt->epoch_sec = (int64_t)epoch - (int64_t)offset_min * 60;
    dt->utc_offset_min = offset_min;
    return wrap_datetime(ctx, dt);
}

/* Accessor helpers — compute struct tm from datetime, return a field */
#define DATETIME_ACCESSOR(name, field) \
static sexp bridge_datetime_##name(sexp ctx, sexp self, sexp_sint_t n, sexp sdt) { \
    EvalDateTime *dt = unwrap_datetime(sdt); \
    if (!dt) return sexp_user_exception(ctx, SEXP_FALSE, "expected datetime", sdt); \
    struct tm tm_val; \
    if (!datetime_to_tm(dt, &tm_val)) \
        return sexp_user_exception(ctx, SEXP_FALSE, "datetime conversion error", sdt); \
    return sexp_make_fixnum(field); \
}

DATETIME_ACCESSOR(year, tm_val.tm_year + 1900)
DATETIME_ACCESSOR(month, tm_val.tm_mon + 1)
DATETIME_ACCESSOR(day, tm_val.tm_mday)
DATETIME_ACCESSOR(hour, tm_val.tm_hour)
DATETIME_ACCESSOR(minute, tm_val.tm_min)
DATETIME_ACCESSOR(second, tm_val.tm_sec)

/* %datetime-epoch(dt) */
static sexp bridge_datetime_epoch(sexp ctx, sexp self, sexp_sint_t n, sexp sdt) {
    EvalDateTime *dt = unwrap_datetime(sdt);
    if (!dt) return sexp_user_exception(ctx, SEXP_FALSE, "expected datetime", sdt);
    /* Return as flonum for large values */
    if (dt->epoch_sec > SEXP_MAX_FIXNUM || dt->epoch_sec < SEXP_MIN_FIXNUM)
        return sexp_make_flonum(ctx, (double)dt->epoch_sec);
    return sexp_make_fixnum((sexp_sint_t)dt->epoch_sec);
}

/* %datetime-offset(dt) */
static sexp bridge_datetime_offset(sexp ctx, sexp self, sexp_sint_t n, sexp sdt) {
    EvalDateTime *dt = unwrap_datetime(sdt);
    if (!dt) return sexp_user_exception(ctx, SEXP_FALSE, "expected datetime", sdt);
    return sexp_make_fixnum(dt->utc_offset_min);
}

/* %datetime-format(dt, fmt) */
static sexp bridge_datetime_format(sexp ctx, sexp self, sexp_sint_t n, sexp sdt, sexp sfmt) {
    EvalDateTime *dt = unwrap_datetime(sdt);
    if (!dt) return sexp_user_exception(ctx, SEXP_FALSE, "expected datetime", sdt);
    if (!sexp_stringp(sfmt))
        return sexp_user_exception(ctx, SEXP_FALSE, "expected format string", sfmt);

    struct tm tm_val;
    if (!datetime_to_tm(dt, &tm_val))
        return sexp_user_exception(ctx, SEXP_FALSE, "datetime conversion error", sdt);

    char buf[256];
    size_t len = strftime(buf, sizeof(buf), sexp_string_data(sfmt), &tm_val);
    if (len == 0)
        return sexp_c_string(ctx, "", 0);
    return sexp_c_string(ctx, buf, (sexp_sint_t)len);
}

/* %datetime-to-iso(dt) */
static sexp bridge_datetime_to_iso(sexp ctx, sexp self, sexp_sint_t n, sexp sdt) {
    EvalDateTime *dt = unwrap_datetime(sdt);
    if (!dt) return sexp_user_exception(ctx, SEXP_FALSE, "expected datetime", sdt);

    struct tm tm_val;
    if (!datetime_to_tm(dt, &tm_val))
        return sexp_user_exception(ctx, SEXP_FALSE, "datetime conversion error", sdt);

    char buf[64];
    if (dt->utc_offset_min == 0) {
        snprintf(buf, sizeof(buf), "%04d-%02d-%02dT%02d:%02d:%02dZ",
                 tm_val.tm_year + 1900, tm_val.tm_mon + 1, tm_val.tm_mday,
                 tm_val.tm_hour, tm_val.tm_min, tm_val.tm_sec);
    } else {
        int off = dt->utc_offset_min;
        char sign = off >= 0 ? '+' : '-';
        if (off < 0) off = -off;
        snprintf(buf, sizeof(buf), "%04d-%02d-%02dT%02d:%02d:%02d%c%02d:%02d",
                 tm_val.tm_year + 1900, tm_val.tm_mon + 1, tm_val.tm_mday,
                 tm_val.tm_hour, tm_val.tm_min, tm_val.tm_sec,
                 sign, off / 60, off % 60);
    }
    return sexp_c_string(ctx, buf, -1);
}

/* %datetime-add-seconds(dt, n) */
static sexp bridge_datetime_add_seconds(sexp ctx, sexp self, sexp_sint_t n, sexp sdt, sexp ssec) {
    EvalDateTime *dt = unwrap_datetime(sdt);
    if (!dt) return sexp_user_exception(ctx, SEXP_FALSE, "expected datetime", sdt);

    int64_t sec;
    if (sexp_fixnump(ssec)) sec = sexp_unbox_fixnum(ssec);
    else if (sexp_flonump(ssec)) sec = (int64_t)sexp_flonum_value(ssec);
    else return sexp_user_exception(ctx, SEXP_FALSE, "expected number", ssec);

    EvalDateTime *r = (EvalDateTime *)malloc(sizeof(EvalDateTime));
    r->epoch_sec = dt->epoch_sec + sec;
    r->utc_offset_min = dt->utc_offset_min;
    return wrap_datetime(ctx, r);
}

/* %datetime-diff(dt1, dt2) → TimeDelta */
static sexp bridge_datetime_diff(sexp ctx, sexp self, sexp_sint_t n, sexp sdt1, sexp sdt2) {
    EvalDateTime *dt1 = unwrap_datetime(sdt1);
    EvalDateTime *dt2 = unwrap_datetime(sdt2);
    if (!dt1) return sexp_user_exception(ctx, SEXP_FALSE, "expected datetime", sdt1);
    if (!dt2) return sexp_user_exception(ctx, SEXP_FALSE, "expected datetime", sdt2);

    EvalTimeDelta *td = (EvalTimeDelta *)malloc(sizeof(EvalTimeDelta));
    td->total_seconds = dt1->epoch_sec - dt2->epoch_sec;
    return wrap_timedelta(ctx, td);
}

/* %datetime-compare(dt1, dt2) → -1/0/1 */
static sexp bridge_datetime_compare(sexp ctx, sexp self, sexp_sint_t n, sexp sdt1, sexp sdt2) {
    EvalDateTime *dt1 = unwrap_datetime(sdt1);
    EvalDateTime *dt2 = unwrap_datetime(sdt2);
    if (!dt1) return sexp_user_exception(ctx, SEXP_FALSE, "expected datetime", sdt1);
    if (!dt2) return sexp_user_exception(ctx, SEXP_FALSE, "expected datetime", sdt2);

    if (dt1->epoch_sec < dt2->epoch_sec) return sexp_make_fixnum(-1);
    if (dt1->epoch_sec > dt2->epoch_sec) return sexp_make_fixnum(1);
    return sexp_make_fixnum(0);
}

/* %datetime-to-utc(dt) */
static sexp bridge_datetime_to_utc(sexp ctx, sexp self, sexp_sint_t n, sexp sdt) {
    EvalDateTime *dt = unwrap_datetime(sdt);
    if (!dt) return sexp_user_exception(ctx, SEXP_FALSE, "expected datetime", sdt);

    EvalDateTime *r = (EvalDateTime *)malloc(sizeof(EvalDateTime));
    r->epoch_sec = dt->epoch_sec;
    r->utc_offset_min = 0;
    return wrap_datetime(ctx, r);
}

/* %datetime-to-offset(dt, offset_min) */
static sexp bridge_datetime_to_offset(sexp ctx, sexp self, sexp_sint_t n, sexp sdt, sexp soff) {
    EvalDateTime *dt = unwrap_datetime(sdt);
    if (!dt) return sexp_user_exception(ctx, SEXP_FALSE, "expected datetime", sdt);
    int off = (int)sexp_unbox_fixnum(soff);

    EvalDateTime *r = (EvalDateTime *)malloc(sizeof(EvalDateTime));
    r->epoch_sec = dt->epoch_sec;
    r->utc_offset_min = off;
    return wrap_datetime(ctx, r);
}

/* %datetime-to-date(dt) → Date */
static sexp bridge_datetime_to_date(sexp ctx, sexp self, sexp_sint_t n, sexp sdt) {
    EvalDateTime *dt = unwrap_datetime(sdt);
    if (!dt) return sexp_user_exception(ctx, SEXP_FALSE, "expected datetime", sdt);

    struct tm tm_val;
    if (!datetime_to_tm(dt, &tm_val))
        return sexp_user_exception(ctx, SEXP_FALSE, "datetime conversion error", sdt);

    EvalDate *d = (EvalDate *)malloc(sizeof(EvalDate));
    d->year = tm_val.tm_year + 1900;
    d->month = tm_val.tm_mon + 1;
    d->day = tm_val.tm_mday;
    return wrap_date(ctx, d);
}

/* %datetime?(x) */
static sexp bridge_datetime_p(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    return unwrap_datetime(x) ? SEXP_TRUE : SEXP_FALSE;
}

/* ================================================================
 * Date bridge functions
 * ================================================================ */

/* Helper: convert EvalDate to epoch seconds (midnight UTC) */
static int64_t date_to_epoch(EvalDate *d) {
    struct tm t;
    memset(&t, 0, sizeof(t));
    t.tm_year = d->year - 1900;
    t.tm_mon = d->month - 1;
    t.tm_mday = d->day;
    return (int64_t)mkgm(&t);
}

/* Helper: convert epoch to EvalDate */
static EvalDate *epoch_to_date(int64_t epoch) {
    time_t t = (time_t)epoch;
    struct tm tm_val;
    gmtime_safe(&t, &tm_val);
    EvalDate *d = (EvalDate *)malloc(sizeof(EvalDate));
    d->year = tm_val.tm_year + 1900;
    d->month = tm_val.tm_mon + 1;
    d->day = tm_val.tm_mday;
    return d;
}

/* %make-date(y, m, d) */
static sexp bridge_make_date(sexp ctx, sexp self, sexp_sint_t n,
        sexp sy, sexp sm, sexp sd) {
    EvalDate *d = (EvalDate *)malloc(sizeof(EvalDate));
    d->year = (int)sexp_unbox_fixnum(sy);
    d->month = (int)sexp_unbox_fixnum(sm);
    d->day = (int)sexp_unbox_fixnum(sd);
    return wrap_date(ctx, d);
}

/* %date-today() */
static sexp bridge_date_today(sexp ctx, sexp self, sexp_sint_t n) {
    time_t now = time(NULL);
    struct tm local;
    localtime_safe(&now, &local);

    EvalDate *d = (EvalDate *)malloc(sizeof(EvalDate));
    d->year = local.tm_year + 1900;
    d->month = local.tm_mon + 1;
    d->day = local.tm_mday;
    return wrap_date(ctx, d);
}

/* Date accessors */
static sexp bridge_date_year(sexp ctx, sexp self, sexp_sint_t n, sexp sd) {
    EvalDate *d = unwrap_date(sd);
    if (!d) return sexp_user_exception(ctx, SEXP_FALSE, "expected date", sd);
    return sexp_make_fixnum(d->year);
}
static sexp bridge_date_month(sexp ctx, sexp self, sexp_sint_t n, sexp sd) {
    EvalDate *d = unwrap_date(sd);
    if (!d) return sexp_user_exception(ctx, SEXP_FALSE, "expected date", sd);
    return sexp_make_fixnum(d->month);
}
static sexp bridge_date_day(sexp ctx, sexp self, sexp_sint_t n, sexp sd) {
    EvalDate *d = unwrap_date(sd);
    if (!d) return sexp_user_exception(ctx, SEXP_FALSE, "expected date", sd);
    return sexp_make_fixnum(d->day);
}

/* %date-to-datetime(d) → DateTime at midnight UTC */
static sexp bridge_date_to_datetime(sexp ctx, sexp self, sexp_sint_t n, sexp sd) {
    EvalDate *d = unwrap_date(sd);
    if (!d) return sexp_user_exception(ctx, SEXP_FALSE, "expected date", sd);

    EvalDateTime *dt = (EvalDateTime *)malloc(sizeof(EvalDateTime));
    dt->epoch_sec = date_to_epoch(d);
    dt->utc_offset_min = 0;
    return wrap_datetime(ctx, dt);
}

/* %date-diff(d1, d2) → TimeDelta */
static sexp bridge_date_diff(sexp ctx, sexp self, sexp_sint_t n, sexp sd1, sexp sd2) {
    EvalDate *d1 = unwrap_date(sd1);
    EvalDate *d2 = unwrap_date(sd2);
    if (!d1) return sexp_user_exception(ctx, SEXP_FALSE, "expected date", sd1);
    if (!d2) return sexp_user_exception(ctx, SEXP_FALSE, "expected date", sd2);

    EvalTimeDelta *td = (EvalTimeDelta *)malloc(sizeof(EvalTimeDelta));
    td->total_seconds = date_to_epoch(d1) - date_to_epoch(d2);
    return wrap_timedelta(ctx, td);
}

/* %date-add-days(d, n) */
static sexp bridge_date_add_days(sexp ctx, sexp self, sexp_sint_t n, sexp sd, sexp sn) {
    EvalDate *d = unwrap_date(sd);
    if (!d) return sexp_user_exception(ctx, SEXP_FALSE, "expected date", sd);

    int days = (int)sexp_unbox_fixnum(sn);
    int64_t epoch = date_to_epoch(d) + (int64_t)days * 86400;
    EvalDate *r = epoch_to_date(epoch);
    return wrap_date(ctx, r);
}

/* %date-compare(d1, d2) → -1/0/1 */
static sexp bridge_date_compare(sexp ctx, sexp self, sexp_sint_t n, sexp sd1, sexp sd2) {
    EvalDate *d1 = unwrap_date(sd1);
    EvalDate *d2 = unwrap_date(sd2);
    if (!d1) return sexp_user_exception(ctx, SEXP_FALSE, "expected date", sd1);
    if (!d2) return sexp_user_exception(ctx, SEXP_FALSE, "expected date", sd2);

    int64_t e1 = date_to_epoch(d1), e2 = date_to_epoch(d2);
    if (e1 < e2) return sexp_make_fixnum(-1);
    if (e1 > e2) return sexp_make_fixnum(1);
    return sexp_make_fixnum(0);
}

/* %date-format(d, fmt) */
static sexp bridge_date_format(sexp ctx, sexp self, sexp_sint_t n, sexp sd, sexp sfmt) {
    EvalDate *d = unwrap_date(sd);
    if (!d) return sexp_user_exception(ctx, SEXP_FALSE, "expected date", sd);
    if (!sexp_stringp(sfmt))
        return sexp_user_exception(ctx, SEXP_FALSE, "expected format string", sfmt);

    struct tm t;
    memset(&t, 0, sizeof(t));
    t.tm_year = d->year - 1900;
    t.tm_mon = d->month - 1;
    t.tm_mday = d->day;

    char buf[256];
    size_t len = strftime(buf, sizeof(buf), sexp_string_data(sfmt), &t);
    if (len == 0)
        return sexp_c_string(ctx, "", 0);
    return sexp_c_string(ctx, buf, (sexp_sint_t)len);
}

/* %date?(x) */
static sexp bridge_date_p(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    return unwrap_date(x) ? SEXP_TRUE : SEXP_FALSE;
}

/* ================================================================
 * TimeDelta bridge functions
 * ================================================================ */

/* %make-timedelta(total_seconds) */
static sexp bridge_make_timedelta(sexp ctx, sexp self, sexp_sint_t n, sexp ssec) {
    int64_t sec;
    if (sexp_fixnump(ssec)) sec = sexp_unbox_fixnum(ssec);
    else if (sexp_flonump(ssec)) sec = (int64_t)sexp_flonum_value(ssec);
    else return sexp_user_exception(ctx, SEXP_FALSE, "expected number", ssec);

    EvalTimeDelta *td = (EvalTimeDelta *)malloc(sizeof(EvalTimeDelta));
    td->total_seconds = sec;
    return wrap_timedelta(ctx, td);
}

/* %timedelta-seconds(td) → total seconds */
static sexp bridge_timedelta_seconds(sexp ctx, sexp self, sexp_sint_t n, sexp std) {
    EvalTimeDelta *td = unwrap_timedelta(std);
    if (!td) return sexp_user_exception(ctx, SEXP_FALSE, "expected timedelta", std);
    if (td->total_seconds > SEXP_MAX_FIXNUM || td->total_seconds < SEXP_MIN_FIXNUM)
        return sexp_make_flonum(ctx, (double)td->total_seconds);
    return sexp_make_fixnum((sexp_sint_t)td->total_seconds);
}

/* %timedelta-days(td) */
static sexp bridge_timedelta_days(sexp ctx, sexp self, sexp_sint_t n, sexp std) {
    EvalTimeDelta *td = unwrap_timedelta(std);
    if (!td) return sexp_user_exception(ctx, SEXP_FALSE, "expected timedelta", std);
    return sexp_make_fixnum((sexp_sint_t)(td->total_seconds / 86400));
}

/* %timedelta-hours(td) */
static sexp bridge_timedelta_hours(sexp ctx, sexp self, sexp_sint_t n, sexp std) {
    EvalTimeDelta *td = unwrap_timedelta(std);
    if (!td) return sexp_user_exception(ctx, SEXP_FALSE, "expected timedelta", std);
    return sexp_make_fixnum((sexp_sint_t)((td->total_seconds % 86400) / 3600));
}

/* %timedelta-minutes(td) */
static sexp bridge_timedelta_minutes(sexp ctx, sexp self, sexp_sint_t n, sexp std) {
    EvalTimeDelta *td = unwrap_timedelta(std);
    if (!td) return sexp_user_exception(ctx, SEXP_FALSE, "expected timedelta", std);
    return sexp_make_fixnum((sexp_sint_t)((td->total_seconds % 3600) / 60));
}

/* %timedelta-add(td1, td2) */
static sexp bridge_timedelta_add(sexp ctx, sexp self, sexp_sint_t n, sexp std1, sexp std2) {
    EvalTimeDelta *td1 = unwrap_timedelta(std1);
    EvalTimeDelta *td2 = unwrap_timedelta(std2);
    if (!td1) return sexp_user_exception(ctx, SEXP_FALSE, "expected timedelta", std1);
    if (!td2) return sexp_user_exception(ctx, SEXP_FALSE, "expected timedelta", std2);

    EvalTimeDelta *r = (EvalTimeDelta *)malloc(sizeof(EvalTimeDelta));
    r->total_seconds = td1->total_seconds + td2->total_seconds;
    return wrap_timedelta(ctx, r);
}

/* %timedelta-negate(td) */
static sexp bridge_timedelta_negate(sexp ctx, sexp self, sexp_sint_t n, sexp std) {
    EvalTimeDelta *td = unwrap_timedelta(std);
    if (!td) return sexp_user_exception(ctx, SEXP_FALSE, "expected timedelta", std);

    EvalTimeDelta *r = (EvalTimeDelta *)malloc(sizeof(EvalTimeDelta));
    r->total_seconds = -td->total_seconds;
    return wrap_timedelta(ctx, r);
}

/* %timedelta-compare(td1, td2) → -1/0/1 */
static sexp bridge_timedelta_compare(sexp ctx, sexp self, sexp_sint_t n, sexp std1, sexp std2) {
    EvalTimeDelta *td1 = unwrap_timedelta(std1);
    EvalTimeDelta *td2 = unwrap_timedelta(std2);
    if (!td1) return sexp_user_exception(ctx, SEXP_FALSE, "expected timedelta", std1);
    if (!td2) return sexp_user_exception(ctx, SEXP_FALSE, "expected timedelta", std2);

    if (td1->total_seconds < td2->total_seconds) return sexp_make_fixnum(-1);
    if (td1->total_seconds > td2->total_seconds) return sexp_make_fixnum(1);
    return sexp_make_fixnum(0);
}

/* %timedelta?(x) */
static sexp bridge_timedelta_p(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    return unwrap_timedelta(x) ? SEXP_TRUE : SEXP_FALSE;
}

/* ================================================================
 * tostr helper for bridge_tostr integration
 * ================================================================ */

sexp datetime_tostr(sexp ctx, sexp x) {
    EvalDateTime *dt = unwrap_datetime(x);
    if (dt) {
        struct tm tm_val;
        if (!datetime_to_tm(dt, &tm_val))
            return SEXP_FALSE;
        char buf[64];
        if (dt->utc_offset_min == 0) {
            snprintf(buf, sizeof(buf), "%04d-%02d-%02dT%02d:%02d:%02dZ",
                     tm_val.tm_year + 1900, tm_val.tm_mon + 1, tm_val.tm_mday,
                     tm_val.tm_hour, tm_val.tm_min, tm_val.tm_sec);
        } else {
            int off = dt->utc_offset_min;
            char sign = off >= 0 ? '+' : '-';
            if (off < 0) off = -off;
            snprintf(buf, sizeof(buf), "%04d-%02d-%02dT%02d:%02d:%02d%c%02d:%02d",
                     tm_val.tm_year + 1900, tm_val.tm_mon + 1, tm_val.tm_mday,
                     tm_val.tm_hour, tm_val.tm_min, tm_val.tm_sec,
                     sign, off / 60, off % 60);
        }
        return sexp_c_string(ctx, buf, -1);
    }

    EvalDate *d = unwrap_date(x);
    if (d) {
        char buf[32];
        snprintf(buf, sizeof(buf), "%04d-%02d-%02d", d->year, d->month, d->day);
        return sexp_c_string(ctx, buf, -1);
    }

    EvalTimeDelta *td = unwrap_timedelta(x);
    if (td) {
        char buf[64];
        int64_t s = td->total_seconds;
        if (s < 0) {
            snprintf(buf, sizeof(buf), "TimeDelta(-%lld)", (long long)(-s));
        } else {
            snprintf(buf, sizeof(buf), "TimeDelta(%lld)", (long long)s);
        }
        return sexp_c_string(ctx, buf, -1);
    }

    return SEXP_FALSE;
}

/* ================================================================
 * Bridge registration
 * ================================================================ */

void register_datetime_bridge_functions(sexp ctx, sexp env) {
    /* DateTime */
    sexp_define_foreign(ctx, env, "%make-datetime", 7, bridge_make_datetime);
    sexp_define_foreign(ctx, env, "%datetime-now", 0, bridge_datetime_now);
    sexp_define_foreign(ctx, env, "%datetime-utc-now", 0, bridge_datetime_utc_now);
    sexp_define_foreign(ctx, env, "%datetime-from-epoch", 1, bridge_datetime_from_epoch);
    sexp_define_foreign(ctx, env, "%datetime-parse", 1, bridge_datetime_parse);
    sexp_define_foreign(ctx, env, "%datetime-year", 1, bridge_datetime_year);
    sexp_define_foreign(ctx, env, "%datetime-month", 1, bridge_datetime_month);
    sexp_define_foreign(ctx, env, "%datetime-day", 1, bridge_datetime_day);
    sexp_define_foreign(ctx, env, "%datetime-hour", 1, bridge_datetime_hour);
    sexp_define_foreign(ctx, env, "%datetime-minute", 1, bridge_datetime_minute);
    sexp_define_foreign(ctx, env, "%datetime-second", 1, bridge_datetime_second);
    sexp_define_foreign(ctx, env, "%datetime-epoch", 1, bridge_datetime_epoch);
    sexp_define_foreign(ctx, env, "%datetime-offset", 1, bridge_datetime_offset);
    sexp_define_foreign(ctx, env, "%datetime-format", 2, bridge_datetime_format);
    sexp_define_foreign(ctx, env, "%datetime-to-iso", 1, bridge_datetime_to_iso);
    sexp_define_foreign(ctx, env, "%datetime-add-seconds", 2, bridge_datetime_add_seconds);
    sexp_define_foreign(ctx, env, "%datetime-diff", 2, bridge_datetime_diff);
    sexp_define_foreign(ctx, env, "%datetime-compare", 2, bridge_datetime_compare);
    sexp_define_foreign(ctx, env, "%datetime-to-utc", 1, bridge_datetime_to_utc);
    sexp_define_foreign(ctx, env, "%datetime-to-offset", 2, bridge_datetime_to_offset);
    sexp_define_foreign(ctx, env, "%datetime-to-date", 1, bridge_datetime_to_date);
    sexp_define_foreign(ctx, env, "%datetime?", 1, bridge_datetime_p);

    /* Date */
    sexp_define_foreign(ctx, env, "%make-date", 3, bridge_make_date);
    sexp_define_foreign(ctx, env, "%date-today", 0, bridge_date_today);
    sexp_define_foreign(ctx, env, "%date-year", 1, bridge_date_year);
    sexp_define_foreign(ctx, env, "%date-month", 1, bridge_date_month);
    sexp_define_foreign(ctx, env, "%date-day", 1, bridge_date_day);
    sexp_define_foreign(ctx, env, "%date-to-datetime", 1, bridge_date_to_datetime);
    sexp_define_foreign(ctx, env, "%date-diff", 2, bridge_date_diff);
    sexp_define_foreign(ctx, env, "%date-add-days", 2, bridge_date_add_days);
    sexp_define_foreign(ctx, env, "%date-compare", 2, bridge_date_compare);
    sexp_define_foreign(ctx, env, "%date-format", 2, bridge_date_format);
    sexp_define_foreign(ctx, env, "%date?", 1, bridge_date_p);

    /* TimeDelta */
    sexp_define_foreign(ctx, env, "%make-timedelta", 1, bridge_make_timedelta);
    sexp_define_foreign(ctx, env, "%timedelta-seconds", 1, bridge_timedelta_seconds);
    sexp_define_foreign(ctx, env, "%timedelta-days", 1, bridge_timedelta_days);
    sexp_define_foreign(ctx, env, "%timedelta-hours", 1, bridge_timedelta_hours);
    sexp_define_foreign(ctx, env, "%timedelta-minutes", 1, bridge_timedelta_minutes);
    sexp_define_foreign(ctx, env, "%timedelta-add", 2, bridge_timedelta_add);
    sexp_define_foreign(ctx, env, "%timedelta-negate", 1, bridge_timedelta_negate);
    sexp_define_foreign(ctx, env, "%timedelta-compare", 2, bridge_timedelta_compare);
    sexp_define_foreign(ctx, env, "%timedelta?", 1, bridge_timedelta_p);
}
