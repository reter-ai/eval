/*  _eval_writer.c -- Eval-syntax serializer  */

#include "_eval_writer.h"
#include "_chibi_pyobject_type.h"
#include <stdio.h>
#include <string.h>

/* Dynamic string buffer */
typedef struct {
    char *data;
    int size;
    int capacity;
} StrBuf;

static void strbuf_init(StrBuf *buf) {
    buf->capacity = 256;
    buf->data = (char *)malloc(buf->capacity);
    buf->data[0] = '\0';
    buf->size = 0;
}

static void strbuf_append(StrBuf *buf, const char *s, int len) {
    if (len < 0) len = (int)strlen(s);
    while (buf->size + len + 1 > buf->capacity) {
        buf->capacity *= 2;
        buf->data = (char *)realloc(buf->data, buf->capacity);
    }
    memcpy(buf->data + buf->size, s, len);
    buf->size += len;
    buf->data[buf->size] = '\0';
}

static void strbuf_append_char(StrBuf *buf, char c) {
    strbuf_append(buf, &c, 1);
}

static void strbuf_free(StrBuf *buf) {
    free(buf->data);
    buf->data = NULL;
    buf->size = 0;
    buf->capacity = 0;
}

/* Recursive writer with cycle detection via depth limit */
static void eval_write_recursive(sexp ctx, sexp x, StrBuf *buf, int depth) {
    if (depth > 100) {
        strbuf_append(buf, "...", -1);
        return;
    }

    /* void / unspecified */
    if (x == SEXP_VOID || x == SEXP_UNDEF) {
        return; /* nothing to write */
    }

    /* boolean */
    if (x == SEXP_TRUE) {
        strbuf_append(buf, "true", -1);
        return;
    }
    if (x == SEXP_FALSE) {
        strbuf_append(buf, "false", -1);
        return;
    }

    /* null */
    if (sexp_nullp(x)) {
        strbuf_append(buf, "[]", -1);
        return;
    }

    /* fixnum */
    if (sexp_fixnump(x)) {
        char tmp[64];
        snprintf(tmp, sizeof(tmp), "%lld",
                 (long long)sexp_unbox_fixnum(x));
        strbuf_append(buf, tmp, -1);
        return;
    }

    /* flonum */
    if (sexp_flonump(x)) {
        char tmp[64];
        double v = sexp_flonum_value(x);
        snprintf(tmp, sizeof(tmp), "%g", v);
        /* Ensure there's a decimal point */
        if (!strchr(tmp, '.') && !strchr(tmp, 'e') && !strchr(tmp, 'E')
            && !strchr(tmp, 'n') && !strchr(tmp, 'i')) {
            strncat(tmp, ".0", sizeof(tmp) - strlen(tmp) - 1);
        }
        strbuf_append(buf, tmp, -1);
        return;
    }

    /* bignum */
#if SEXP_USE_BIGNUMS
    if (sexp_bignump(x)) {
        sexp out = sexp_open_output_string(ctx);
        sexp_write_bignum(ctx, x, out, 10);
        sexp str = sexp_get_output_string(ctx, out);
        if (sexp_stringp(str))
            strbuf_append(buf, sexp_string_data(str), sexp_string_size(str));
        else
            strbuf_append(buf, "0", -1);
        return;
    }
#endif

    /* string */
    if (sexp_stringp(x)) {
        strbuf_append_char(buf, '"');
        const char *data = sexp_string_data(x);
        int len = sexp_string_size(x);
        for (int i = 0; i < len; i++) {
            switch (data[i]) {
            case '"':  strbuf_append(buf, "\\\"", 2); break;
            case '\\': strbuf_append(buf, "\\\\", 2); break;
            case '\n': strbuf_append(buf, "\\n", 2); break;
            case '\t': strbuf_append(buf, "\\t", 2); break;
            case '\r': strbuf_append(buf, "\\r", 2); break;
            default:   strbuf_append_char(buf, data[i]); break;
            }
        }
        strbuf_append_char(buf, '"');
        return;
    }

    /* symbol */
    if (sexp_symbolp(x)) {
        sexp sym_str = sexp_symbol_to_string(ctx, x);
        strbuf_append(buf, sexp_string_data(sym_str), sexp_string_size(sym_str));
        return;
    }

    /* char */
    if (sexp_charp(x)) {
        strbuf_append_char(buf, '\'');
        char c = sexp_unbox_character(x);
        switch (c) {
        case '\n': strbuf_append(buf, "\\n", 2); break;
        case '\t': strbuf_append(buf, "\\t", 2); break;
        case '\\': strbuf_append(buf, "\\\\", 2); break;
        case '\'': strbuf_append(buf, "\\'", 2); break;
        default: strbuf_append_char(buf, c); break;
        }
        strbuf_append_char(buf, '\'');
        return;
    }

    /* bytes/bytevector */
    if (sexp_bytesp(x)) {
        strbuf_append(buf, "bytes[", -1);
        int len = sexp_bytes_length(x);
        const unsigned char *data = (const unsigned char *)sexp_bytes_data(x);
        for (int i = 0; i < len; i++) {
            char tmp[8];
            snprintf(tmp, sizeof(tmp), "%d", data[i]);
            if (i > 0) strbuf_append(buf, ", ", 2);
            strbuf_append(buf, tmp, -1);
        }
        strbuf_append_char(buf, ']');
        return;
    }

    /* python-object */
    if (sexp_pyobjectp(x)) {
        PyObject *obj = unwrap_pyobject(x);
        if (obj) {
            strbuf_append(buf, "<py:", -1);
            PyObject *repr = PyObject_Repr(obj);
            if (repr) {
                const char *s = PyUnicode_AsUTF8(repr);
                if (s) strbuf_append(buf, s, -1);
                Py_DECREF(repr);
            } else {
                PyErr_Clear();
                strbuf_append(buf, "?", -1);
            }
            strbuf_append_char(buf, '>');
        } else {
            strbuf_append(buf, "<py:None>", -1);
        }
        return;
    }

    /* vector */
    if (sexp_vectorp(x)) {
        strbuf_append(buf, "#[", -1);
        int len = sexp_vector_length(x);
        for (int i = 0; i < len; i++) {
            if (i > 0) strbuf_append(buf, ", ", 2);
            eval_write_recursive(ctx, sexp_vector_ref(x, sexp_make_fixnum(i)),
                                 buf, depth + 1);
        }
        strbuf_append_char(buf, ']');
        return;
    }

    /* pair - check if proper list */
    if (sexp_pairp(x)) {
        sexp p = x;
        int is_proper = 1;
        while (sexp_pairp(p))
            p = sexp_cdr(p);
        if (!sexp_nullp(p))
            is_proper = 0;

        if (is_proper) {
            /* Proper list: [a, b, c] */
            strbuf_append_char(buf, '[');
            p = x;
            int first = 1;
            while (sexp_pairp(p)) {
                if (!first) strbuf_append(buf, ", ", 2);
                first = 0;
                eval_write_recursive(ctx, sexp_car(p), buf, depth + 1);
                p = sexp_cdr(p);
            }
            strbuf_append_char(buf, ']');
        } else {
            /* Dotted pair: (a, b .. c) */
            strbuf_append_char(buf, '(');
            p = x;
            int first = 1;
            while (sexp_pairp(p)) {
                if (!first) strbuf_append(buf, ", ", 2);
                first = 0;
                eval_write_recursive(ctx, sexp_car(p), buf, depth + 1);
                p = sexp_cdr(p);
            }
            strbuf_append(buf, " .. ", 4);
            eval_write_recursive(ctx, p, buf, depth + 1);
            strbuf_append_char(buf, ')');
        }
        return;
    }

    /* procedure */
    if (sexp_procedurep(x) || sexp_opcodep(x)) {
        strbuf_append(buf, "<function>", -1);
        return;
    }

    /* exception */
    if (sexp_exceptionp(x)) {
        strbuf_append(buf, "<error: ", -1);
        sexp msg = sexp_exception_message(x);
        if (sexp_stringp(msg))
            strbuf_append(buf, sexp_string_data(msg), sexp_string_size(msg));
        else
            strbuf_append(buf, "?", -1);
        strbuf_append_char(buf, '>');
        return;
    }

    /* Default */
    strbuf_append(buf, "<scheme-object>", -1);
}

/* Public API: Write sexp to Python string */
PyObject *eval_write_to_string(sexp ctx, sexp x) {
    StrBuf buf;
    strbuf_init(&buf);
    eval_write_recursive(ctx, x, &buf, 0);
    PyObject *result = PyUnicode_FromStringAndSize(buf.data, buf.size);
    strbuf_free(&buf);
    return result;
}

/* Public API: Write sexp to C buffer */
int eval_write_to_buf(sexp ctx, sexp x, char *outbuf, int size) {
    StrBuf buf;
    strbuf_init(&buf);
    eval_write_recursive(ctx, x, &buf, 0);
    int needed = buf.size;
    if (outbuf && size > 0) {
        int copy = (needed < size - 1) ? needed : size - 1;
        memcpy(outbuf, buf.data, copy);
        outbuf[copy] = '\0';
    }
    strbuf_free(&buf);
    return needed;
}
