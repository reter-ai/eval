/*  _chibi_serialize.c -- Continuation serialization/deserialization
 *
 *  Serializes chibi-scheme continuations (captured via call/cc) to a
 *  portable binary format, and deserializes them back.  The format is
 *  self-contained: all reachable sexp objects are written into an
 *  object table; global bindings and opcodes are recorded by name so
 *  they can be re-resolved in the target context.
 *
 *  Binary layout:
 *    HEADER (32 bytes)
 *    STRING TABLE
 *    OBJECT TABLE (index: offset, size, kind)
 *    OBJECT DATA
 */

#include "_chibi_serialize.h"
#include <string.h>
#include <stdlib.h>

/* Huffman tables for inline symbol decoding (GC-safe path) */
#if SEXP_USE_HUFF_SYMS
#include "chibi/sexp-hufftabdefs.h"
#endif

/* ================================================================
 *  Growable byte buffer
 * ================================================================ */

typedef struct {
    unsigned char *data;
    size_t len;
    size_t cap;
} Buffer;

static void buf_init(Buffer *b) {
    b->cap = 4096;
    b->data = (unsigned char *)malloc(b->cap);
    b->len = 0;
}

static void buf_ensure(Buffer *b, size_t extra) {
    while (b->len + extra > b->cap) {
        b->cap *= 2;
        b->data = (unsigned char *)realloc(b->data, b->cap);
    }
}

static void buf_write(Buffer *b, const void *src, size_t n) {
    buf_ensure(b, n);
    memcpy(b->data + b->len, src, n);
    b->len += n;
}

static void buf_write_u8(Buffer *b, uint8_t v) { buf_write(b, &v, 1); }
static void buf_write_u32(Buffer *b, uint32_t v) { buf_write(b, &v, 4); }
static void buf_write_u64(Buffer *b, uint64_t v) { buf_write(b, &v, 8); }
static void buf_write_f64(Buffer *b, double v) { buf_write(b, &v, 8); }

static void buf_free(Buffer *b) { free(b->data); b->data = NULL; }

/* ================================================================
 *  String table (deduplicates symbol/string names)
 * ================================================================ */

typedef struct {
    char **strs;
    uint32_t *lens;
    uint32_t count;
    uint32_t cap;
} StringTable;

static void strtab_init(StringTable *t) {
    t->cap = 256;
    t->strs = (char **)calloc(t->cap, sizeof(char *));
    t->lens = (uint32_t *)calloc(t->cap, sizeof(uint32_t));
    t->count = 0;
}

static uint32_t strtab_intern(StringTable *t, const char *s, uint32_t len) {
    for (uint32_t i = 0; i < t->count; i++) {
        if (t->lens[i] == len && memcmp(t->strs[i], s, len) == 0)
            return i;
    }
    if (t->count >= t->cap) {
        t->cap *= 2;
        t->strs = (char **)realloc(t->strs, t->cap * sizeof(char *));
        t->lens = (uint32_t *)realloc(t->lens, t->cap * sizeof(uint32_t));
    }
    uint32_t idx = t->count++;
    t->strs[idx] = (char *)malloc(len);
    memcpy(t->strs[idx], s, len);
    t->lens[idx] = len;
    return idx;
}

static void strtab_free(StringTable *t) {
    for (uint32_t i = 0; i < t->count; i++) free(t->strs[i]);
    free(t->strs);
    free(t->lens);
}

/* ================================================================
 *  sexp → ID hash table  (for graph-walk dedup / cycle detection)
 * ================================================================ */

typedef struct {
    sexp *keys;
    uint32_t *vals;
    uint32_t cap;
    uint32_t count;
} SexpMap;

static void map_init(SexpMap *m) {
    m->cap = 512;
    m->keys = (sexp *)calloc(m->cap, sizeof(sexp));
    m->vals = (uint32_t *)calloc(m->cap, sizeof(uint32_t));
    m->count = 0;
}

static uint32_t map_hash(sexp s, uint32_t cap) {
    return (uint32_t)((uintptr_t)s * 2654435761u) & (cap - 1);
}

static int map_get(SexpMap *m, sexp key, uint32_t *out) {
    uint32_t h = map_hash(key, m->cap);
    for (uint32_t i = 0; i < m->cap; i++) {
        uint32_t idx = (h + i) & (m->cap - 1);
        if (m->keys[idx] == NULL) return 0;
        if (m->keys[idx] == key) { *out = m->vals[idx]; return 1; }
    }
    return 0;
}

static void map_grow(SexpMap *m);

static void map_put(SexpMap *m, sexp key, uint32_t val) {
    if (m->count * 2 >= m->cap) map_grow(m);
    uint32_t h = map_hash(key, m->cap);
    for (uint32_t i = 0; i < m->cap; i++) {
        uint32_t idx = (h + i) & (m->cap - 1);
        if (m->keys[idx] == NULL || m->keys[idx] == key) {
            m->keys[idx] = key;
            m->vals[idx] = val;
            m->count++;
            return;
        }
    }
}

static void map_grow(SexpMap *m) {
    uint32_t old_cap = m->cap;
    sexp *old_keys = m->keys;
    uint32_t *old_vals = m->vals;
    m->cap *= 2;
    m->keys = (sexp *)calloc(m->cap, sizeof(sexp));
    m->vals = (uint32_t *)calloc(m->cap, sizeof(uint32_t));
    m->count = 0;
    for (uint32_t i = 0; i < old_cap; i++) {
        if (old_keys[i]) map_put(m, old_keys[i], old_vals[i]);
    }
    free(old_keys);
    free(old_vals);
}

static void map_free(SexpMap *m) {
    free(m->keys);
    free(m->vals);
}

/* ================================================================
 *  Serialization state
 * ================================================================ */

typedef struct {
    sexp ctx;
    sexp env;
    SexpMap visited;
    StringTable strtab;

    /* Object records: kind + serialized data */
    uint8_t *obj_kinds;
    Buffer *obj_bufs;    /* per-object data buffers */
    uint32_t obj_count;
    uint32_t obj_cap;

    int error;
    char error_msg[256];
} SerState;

static void ser_init(SerState *s, sexp ctx, sexp env) {
    s->ctx = ctx;
    s->env = env;
    map_init(&s->visited);
    strtab_init(&s->strtab);
    s->obj_cap = 256;
    s->obj_kinds = (uint8_t *)calloc(s->obj_cap, sizeof(uint8_t));
    s->obj_bufs = (Buffer *)calloc(s->obj_cap, sizeof(Buffer));
    s->obj_count = 0;
    s->error = 0;
    s->error_msg[0] = '\0';
}

static void ser_free(SerState *s) {
    map_free(&s->visited);
    strtab_free(&s->strtab);
    for (uint32_t i = 0; i < s->obj_count; i++)
        buf_free(&s->obj_bufs[i]);
    free(s->obj_kinds);
    free(s->obj_bufs);
}

static uint32_t ser_alloc_obj(SerState *s, uint8_t kind) {
    if (s->obj_count >= s->obj_cap) {
        s->obj_cap *= 2;
        s->obj_kinds = (uint8_t *)realloc(s->obj_kinds, s->obj_cap * sizeof(uint8_t));
        s->obj_bufs = (Buffer *)realloc(s->obj_bufs, s->obj_cap * sizeof(Buffer));
    }
    uint32_t id = s->obj_count++;  /* 0-based, but we use 1-based IDs externally */
    s->obj_kinds[id] = kind;
    buf_init(&s->obj_bufs[id]);
    return id + 1;  /* 1-based ID; 0 means null */
}

/* Forward declaration */
static uint32_t ser_walk(SerState *s, sexp x);

/* ================================================================
 *  GC-safe symbol name extraction
 * ================================================================ */

/* Decode a symbol (heap or inline) to a C string buffer WITHOUT any
 * chibi heap allocation.  Returns the length written (excluding NUL).
 * For heap symbols, copies from the symbol's string data.
 * For inline (Huffman) symbols, decodes the bits directly.
 * This is critical for GC safety: calling sexp_symbol_to_string_op
 * on an inline symbol triggers sexp_write_to_string which allocates
 * chibi heap memory, potentially triggering GC and invalidating any
 * sexp pointers read from our raw bytecode copy. */
static int sym_to_cstr(sexp sym, char *buf, int buf_size) {
    if (sexp_lsymbolp(sym)) {
        int len = (int)sexp_lsymbol_length(sym);
        if (len >= buf_size) len = buf_size - 1;
        memcpy(buf, sexp_lsymbol_data(sym), len);
        buf[len] = '\0';
        return len;
    }
#if SEXP_USE_HUFF_SYMS
    if (sexp_isymbolp(sym)) {
        sexp_uint_t c = ((sexp_uint_t)sym) >> SEXP_IMMEDIATE_BITS;
        int pos = 0;
        while (c && pos < buf_size - 1) {
            int res;
#include "chibi/sexp-unhuff.h"
            buf[pos++] = (char)res;
        }
        buf[pos] = '\0';
        return pos;
    }
#endif
    buf[0] = '\0';
    return 0;
}

/* ================================================================
 *  Graph walk: assign IDs and serialize each object
 * ================================================================ */

static void ser_error(SerState *s, const char *msg) {
    if (!s->error) {
        s->error = 1;
        strncpy(s->error_msg, msg, sizeof(s->error_msg) - 1);
        s->error_msg[sizeof(s->error_msg) - 1] = '\0';
    }
}

static uint32_t ser_walk(SerState *s, sexp x) {
    if (s->error) return 0;

    /* Null sexp pointer */
    if (!x) return 0;

    /* Immediates: fixnum, boolean, null, eof, void, undef, char */
    if (sexp_fixnump(x)) {
        uint32_t id = ser_alloc_obj(s, SER_FIXNUM);
        Buffer *b = &s->obj_bufs[id - 1];
        int64_t v = (int64_t)sexp_unbox_fixnum(x);
        buf_write_u64(b, (uint64_t)v);
        return id;
    }

    if (x == SEXP_FALSE) {
        uint32_t id = ser_alloc_obj(s, SER_IMMEDIATE);
        buf_write_u8(&s->obj_bufs[id - 1], SER_IMM_FALSE);
        return id;
    }
    if (x == SEXP_TRUE) {
        uint32_t id = ser_alloc_obj(s, SER_IMMEDIATE);
        buf_write_u8(&s->obj_bufs[id - 1], SER_IMM_TRUE);
        return id;
    }
    if (x == SEXP_NULL) {
        uint32_t id = ser_alloc_obj(s, SER_IMMEDIATE);
        buf_write_u8(&s->obj_bufs[id - 1], SER_IMM_NULL);
        return id;
    }
    if (x == SEXP_EOF) {
        uint32_t id = ser_alloc_obj(s, SER_IMMEDIATE);
        buf_write_u8(&s->obj_bufs[id - 1], SER_IMM_EOF);
        return id;
    }
    if (x == SEXP_VOID) {
        uint32_t id = ser_alloc_obj(s, SER_IMMEDIATE);
        buf_write_u8(&s->obj_bufs[id - 1], SER_IMM_VOID);
        return id;
    }
    if (x == SEXP_UNDEF) {
        uint32_t id = ser_alloc_obj(s, SER_IMMEDIATE);
        buf_write_u8(&s->obj_bufs[id - 1], SER_IMM_UNDEF);
        return id;
    }
    if (sexp_charp(x)) {
        uint32_t id = ser_alloc_obj(s, SER_CHAR);
        uint32_t cp = (uint32_t)sexp_unbox_character(x);
        buf_write_u32(&s->obj_bufs[id - 1], cp);
        return id;
    }

#if SEXP_USE_IMMEDIATE_FLONUMS
    if (sexp_flonump(x) && !sexp_pointerp(x)) {
        uint32_t id = ser_alloc_obj(s, SER_FLONUM);
        double v = (double)sexp_flonum_value(x);
        buf_write_f64(&s->obj_bufs[id - 1], v);
        return id;
    }
#endif

    /* Inline (Huffman) symbols — immediate values, not heap pointers */
#if SEXP_USE_HUFF_SYMS
    if (sexp_isymbolp(x)) {
        char nbuf[256];
        int nlen = sym_to_cstr(x, nbuf, sizeof(nbuf));
        uint32_t si = strtab_intern(&s->strtab, nbuf, (uint32_t)nlen);
        uint32_t id = ser_alloc_obj(s, SER_SYMBOL);
        buf_write_u32(&s->obj_bufs[id - 1], si);
        return id;
    }
#endif

    /* Non-pointer — shouldn't happen but be safe */
    if (!sexp_pointerp(x)) {
        /* Treat unknown immediates as void */
        uint32_t id = ser_alloc_obj(s, SER_IMMEDIATE);
        buf_write_u8(&s->obj_bufs[id - 1], SER_IMM_VOID);
        return id;
    }

    /* Check if already visited (handles cycles) */
    uint32_t existing;
    if (map_get(&s->visited, x, &existing))
        return existing;

    /* Context dk (dynamic-wind point): serialize as a special reference
     * so that on deserialization it resolves to the target context's dk,
     * preserving pointer identity for eq? checks in travel-to-point! */
    if (x == sexp_context_dk(s->ctx)) {
        uint32_t id = ser_alloc_obj(s, SER_CONTEXT_DK);
        map_put(&s->visited, x, id);
        return id;
    }

    /* Heap objects by tag */
    sexp_tag_t tag = sexp_pointer_tag(x);

    switch (tag) {
    case SEXP_SYMBOL: {
        char nbuf[256];
        int nlen = sym_to_cstr(x, nbuf, sizeof(nbuf));
        uint32_t si = strtab_intern(&s->strtab, nbuf, (uint32_t)nlen);
        uint32_t id = ser_alloc_obj(s, SER_SYMBOL);
        map_put(&s->visited, x, id);
        buf_write_u32(&s->obj_bufs[id - 1], si);
        return id;
    }

    case SEXP_STRING: {
        const char *data = sexp_string_data(x);
        uint32_t slen = (uint32_t)sexp_string_size(x);
        uint32_t si = strtab_intern(&s->strtab, data, slen);
        uint32_t id = ser_alloc_obj(s, SER_STRING);
        map_put(&s->visited, x, id);
        buf_write_u32(&s->obj_bufs[id - 1], si);
        return id;
    }

    case SEXP_PAIR: {
        /* Allocate ID first (cycle detection) */
        uint32_t id = ser_alloc_obj(s, SER_PAIR);
        map_put(&s->visited, x, id);
        uint32_t car_id = ser_walk(s, sexp_car(x));
        uint32_t cdr_id = ser_walk(s, sexp_cdr(x));
        buf_write_u32(&s->obj_bufs[id - 1], car_id);
        buf_write_u32(&s->obj_bufs[id - 1], cdr_id);
        return id;
    }

    case SEXP_VECTOR: {
        uint32_t len = (uint32_t)sexp_vector_length(x);
        uint32_t id = ser_alloc_obj(s, SER_VECTOR);
        map_put(&s->visited, x, id);
        buf_write_u32(&s->obj_bufs[id - 1], len);
        for (uint32_t i = 0; i < len; i++) {
            uint32_t eid = ser_walk(s, sexp_vector_data(x)[i]);
            buf_write_u32(&s->obj_bufs[id - 1], eid);
        }
        return id;
    }

    case SEXP_FLONUM: {
        uint32_t id = ser_alloc_obj(s, SER_FLONUM);
        map_put(&s->visited, x, id);
        double v = sexp_flonum_value(x);
        buf_write_f64(&s->obj_bufs[id - 1], v);
        return id;
    }

    case SEXP_BYTECODE: {
        /* Check if this is a RESUMECC bytecode (starts with OP_RESUMECC) */
        if (sexp_bytecode_length(x) >= 1 &&
            sexp_bytecode_data(x)[0] == SEXP_OP_RESUMECC) {
            uint32_t id = ser_alloc_obj(s, SER_RESUMECC_BC);
            map_put(&s->visited, x, id);
            return id;
        }

        uint32_t id = ser_alloc_obj(s, SER_BYTECODE);
        map_put(&s->visited, x, id);

        /* Walk sub-fields first */
        uint32_t name_id = ser_walk(s, sexp_bytecode_name(x));
        /* Skip the literals field entirely.  It serves two purposes
         * in the live VM:
         *  (a) GC rooting for objects embedded in the instruction
         *      stream (PUSH operands, MAKE_PROCEDURE bytecodes, etc.)
         *  (b) Error-reporting parent link: (name . parent-proc)
         * Neither is needed in serialized form — all instruction-stream
         * references are already captured by the patch table, and error
         * reporting works from the name field alone.  Serializing the
         * literals naively pulls in the entire closure graph (e.g.
         * __send__ → all OO method lambdas → 2700+ objects). */
        uint32_t lit_id = ser_walk(s, SEXP_FALSE);
        /* Skip source (may contain non-serializable compile artifacts) */
        uint32_t src_id = 0;
        uint32_t bc_len = (uint32_t)sexp_bytecode_length(x);
        uint32_t max_depth = (uint32_t)sexp_bytecode_max_depth(x);

        /* IMPORTANT: Do NOT cache a pointer to s->obj_bufs[id-1] here.
         * The embedded-ref loop below calls ser_walk / ser_alloc_obj
         * which may realloc s->obj_bufs, invalidating any cached
         * Buffer pointer.  Always use s->obj_bufs[id-1] directly. */
        buf_write_u32(&s->obj_bufs[id - 1], name_id);
        buf_write_u32(&s->obj_bufs[id - 1], lit_id);
        buf_write_u32(&s->obj_bufs[id - 1], src_id);
        buf_write_u32(&s->obj_bufs[id - 1], bc_len);
        buf_write_u32(&s->obj_bufs[id - 1], max_depth);

        /* Write raw instruction bytes, walking for embedded sexp pointers
         * and building a patch table. We zero out pointer slots in the
         * raw bytes and record them separately. */
        unsigned char *bc_data = sexp_bytecode_data(x);

        /* First pass: count patches */
        uint32_t patch_count = 0;
        for (uint32_t i = 0; i < bc_len; ) {
            unsigned char op = bc_data[i++];
            switch (op) {
            case SEXP_OP_FCALL0: case SEXP_OP_FCALL1:
            case SEXP_OP_FCALL2: case SEXP_OP_FCALL3:
            case SEXP_OP_FCALL4: case SEXP_OP_PUSH:
            case SEXP_OP_GLOBAL_REF: case SEXP_OP_GLOBAL_KNOWN_REF:
#if SEXP_USE_GREEN_THREADS
            case SEXP_OP_PARAMETER_REF:
#endif
#if SEXP_USE_EXTENDED_FCALL
            case SEXP_OP_FCALLN:
#endif
                patch_count++;
                /* fallthrough */
            case SEXP_OP_CALL: case SEXP_OP_TAIL_CALL:
            case SEXP_OP_JUMP: case SEXP_OP_JUMP_UNLESS:
            case SEXP_OP_STACK_REF: case SEXP_OP_CLOSURE_REF:
            case SEXP_OP_LOCAL_REF: case SEXP_OP_LOCAL_SET:
            case SEXP_OP_TYPEP:
#if SEXP_USE_RESERVE_OPCODE
            case SEXP_OP_RESERVE:
#endif
                i += sizeof(sexp); break;
            case SEXP_OP_MAKE: case SEXP_OP_SLOT_REF: case SEXP_OP_SLOT_SET:
                i += 2 * sizeof(sexp); break;
            case SEXP_OP_MAKE_PROCEDURE:
                patch_count++;  /* vec[2] */
                i += 3 * sizeof(sexp); break;
            default:
                break;
            }
        }

        /* Write raw bytes (with pointer slots zeroed) and build patches */
        /* Allocate temp buffer for modified bytes */
        unsigned char *raw = (unsigned char *)malloc(bc_len);
        memcpy(raw, bc_data, bc_len);

        /* Temp arrays for patch offsets and ref IDs */
        uint32_t *patch_offs = (uint32_t *)malloc(patch_count * sizeof(uint32_t));
        uint32_t *patch_refs = (uint32_t *)malloc(patch_count * sizeof(uint32_t));
        uint32_t pi = 0;

        for (uint32_t i = 0; i < bc_len; ) {
            unsigned char op = raw[i++];
            switch (op) {
            case SEXP_OP_FCALL0: case SEXP_OP_FCALL1:
            case SEXP_OP_FCALL2: case SEXP_OP_FCALL3:
            case SEXP_OP_FCALL4:
#if SEXP_USE_EXTENDED_FCALL
            case SEXP_OP_FCALLN:
#endif
            {
                /* FCALL operands embed callee info for stack traces
                 * (error reporting / debugging).  The actual function
                 * being called is on the stack, not in the operand.
                 * Skip serializing the operand to avoid pulling in
                 * the entire standard library's closure graph. */
                sexp *slot = (sexp *)(&raw[i]);
                sexp ref = *slot;
                uint32_t ref_id = 0;
                /* Only preserve env-cell refs (global bindings) and
                 * opcodes — these are cheap name-only references and
                 * enable the worker to produce meaningful errors. */
                if (ref && sexp_pointerp(ref)) {
                    if (sexp_pairp(ref) && sexp_symbolp(sexp_car(ref)) &&
                        sexp_env_cell(s->ctx, s->env,
                                      sexp_car(ref), 0) == ref) {
                        char nbuf[256];
                        int nlen = sym_to_cstr(sexp_car(ref), nbuf,
                                               sizeof(nbuf));
                        uint32_t si = strtab_intern(&s->strtab, nbuf,
                                                     (uint32_t)nlen);
                        uint32_t gid = ser_alloc_obj(s, SER_GLOBAL_REF);
                        buf_write_u32(&s->obj_bufs[gid - 1], si);
                        ref_id = gid;
                    } else if (sexp_opcodep(ref)) {
                        sexp name = sexp_opcode_name(ref);
                        if (name && sexp_stringp(name)) {
                            uint32_t si = strtab_intern(&s->strtab,
                                sexp_string_data(name),
                                (uint32_t)sexp_string_size(name));
                            uint32_t oid = ser_alloc_obj(s, SER_OPCODE_REF);
                            buf_write_u32(&s->obj_bufs[oid - 1], si);
                            ref_id = oid;
                        }
                    }
                    /* else: procedure/closure/other → skip (ref_id=0) */
                }
                patch_offs[pi] = i;
                patch_refs[pi] = ref_id;
                pi++;
                memset(&raw[i], 0, sizeof(sexp));
                i += sizeof(sexp);
                break;
            }
            case SEXP_OP_PUSH:
            case SEXP_OP_GLOBAL_REF: case SEXP_OP_GLOBAL_KNOWN_REF:
#if SEXP_USE_GREEN_THREADS
            case SEXP_OP_PARAMETER_REF:
#endif
            {
                sexp *slot = (sexp *)(&raw[i]);
                sexp ref = *slot;
                uint32_t ref_id = 0;
                if (ref && sexp_pointerp(ref)) {
                    /* GLOBAL_REF / GLOBAL_KNOWN_REF / PUSH: the
                     * compiler embeds an env cell (pair with symbol
                     * car).  Verify via sexp_env_cell to confirm it's
                     * a real env binding (PUSH can also embed non-env
                     * pairs).  Note: %dk is already handled above as
                     * SER_CONTEXT_DK, so inline symbols are safe to
                     * resolve by name.
                     *
                     * All three opcodes serialize as SER_GLOBAL_REF
                     * (resolved by name in the target context).  This
                     * ensures PUSH (set!) and GLOBAL_KNOWN_REF (read)
                     * share the same env cell. */
                    if (sexp_pairp(ref) && sexp_symbolp(sexp_car(ref)) &&
                        sexp_env_cell(s->ctx, s->env,
                                      sexp_car(ref), 0) == ref) {
                        char nbuf[256];
                        int nlen = sym_to_cstr(sexp_car(ref), nbuf,
                                               sizeof(nbuf));
                        uint32_t si = strtab_intern(&s->strtab, nbuf,
                                                     (uint32_t)nlen);
                        uint32_t gid = ser_alloc_obj(s, SER_GLOBAL_REF);
                        buf_write_u32(&s->obj_bufs[gid - 1], si);
                        ref_id = gid;
                    } else if (sexp_opcodep(ref)) {
                        /* Opcode: serialize by name */
                        sexp name = sexp_opcode_name(ref);
                        if (name && sexp_stringp(name)) {
                            uint32_t si = strtab_intern(&s->strtab,
                                sexp_string_data(name),
                                (uint32_t)sexp_string_size(name));
                            uint32_t oid = ser_alloc_obj(s, SER_OPCODE_REF);
                            buf_write_u32(&s->obj_bufs[oid - 1], si);
                            ref_id = oid;
                        } else {
                            ser_error(s, "opcode without name cannot be serialized");
                            ref_id = 0;
                        }
                    } else {
                        ref_id = ser_walk(s, ref);
                    }
                } else if (ref) {
                    /* Immediate value: fixnum, char, inline symbol,
                     * boolean, null, void, undef, eof */
                    ref_id = ser_walk(s, ref);
                }
                patch_offs[pi] = i;
                patch_refs[pi] = ref_id;
                pi++;
                /* Zero out the pointer slot in raw bytes */
                memset(&raw[i], 0, sizeof(sexp));
                i += sizeof(sexp);
                break;
            }
            case SEXP_OP_CALL: case SEXP_OP_TAIL_CALL:
            case SEXP_OP_JUMP: case SEXP_OP_JUMP_UNLESS:
            case SEXP_OP_STACK_REF: case SEXP_OP_CLOSURE_REF:
            case SEXP_OP_LOCAL_REF: case SEXP_OP_LOCAL_SET:
            case SEXP_OP_TYPEP:
#if SEXP_USE_RESERVE_OPCODE
            case SEXP_OP_RESERVE:
#endif
                i += sizeof(sexp); break;
            case SEXP_OP_MAKE: case SEXP_OP_SLOT_REF: case SEXP_OP_SLOT_SET:
                i += 2 * sizeof(sexp); break;
            case SEXP_OP_MAKE_PROCEDURE:
            {
                sexp *slot = (sexp *)(&raw[i + 2 * sizeof(sexp)]);
                sexp ref = *slot;
                uint32_t ref_id = 0;
                if (ref && sexp_pointerp(ref)) {
                    ref_id = ser_walk(s, ref);
                } else if (ref) {
                    ref_id = ser_walk(s, ref);
                }
                patch_offs[pi] = (uint32_t)(i + 2 * sizeof(sexp));
                patch_refs[pi] = ref_id;
                pi++;
                memset(&raw[i + 2 * sizeof(sexp)], 0, sizeof(sexp));
                i += 3 * sizeof(sexp);
                break;
            }
            default:
                break;
            }
        }

        /* Write: raw bytes, then patch count, then patches.
         * Re-read s->obj_bufs[id-1] each time (may have been
         * reallocated during the embedded-ref loop above). */
        buf_write(&s->obj_bufs[id - 1], raw, bc_len);
        buf_write_u32(&s->obj_bufs[id - 1], pi);
        for (uint32_t j = 0; j < pi; j++) {
            buf_write_u32(&s->obj_bufs[id - 1], patch_offs[j]);
            buf_write_u32(&s->obj_bufs[id - 1], patch_refs[j]);
        }

        free(raw);
        free(patch_offs);
        free(patch_refs);
        return id;
    }

    case SEXP_PROCEDURE: {
        uint32_t id = ser_alloc_obj(s, SER_PROCEDURE);
        map_put(&s->visited, x, id);

        uint32_t bc_id = ser_walk(s, sexp_procedure_code(x));
        uint32_t vars_id = ser_walk(s, sexp_procedure_vars(x));
        /* flags is a raw char (truncated tagged fixnum), num_args is a
         * raw short/int — NOT sexp fixnums.  Store as-is. */
        unsigned char raw_flags = (unsigned char)sexp_procedure_flags(x);
        sexp_proc_num_args_t raw_num_args = sexp_procedure_num_args(x);

        Buffer *b = &s->obj_bufs[id - 1];
        buf_write_u32(b, bc_id);
        buf_write_u32(b, vars_id);
        buf_write_u32(b, (uint32_t)raw_flags);
        buf_write_u32(b, (uint32_t)raw_num_args);
        return id;
    }

    case SEXP_OPCODE: {
        /* Opcodes serialize by name */
        sexp name = sexp_opcode_name(x);
        if (name && sexp_stringp(name)) {
            uint32_t si = strtab_intern(&s->strtab,
                sexp_string_data(name),
                (uint32_t)sexp_string_size(name));
            uint32_t id = ser_alloc_obj(s, SER_OPCODE_REF);
            map_put(&s->visited, x, id);
            buf_write_u32(&s->obj_bufs[id - 1], si);
            return id;
        }
        ser_error(s, "opcode without name cannot be serialized");
        return 0;
    }

    case SEXP_TYPE: {
        /* Serialize type descriptor by name — resolved by name in target context */
        sexp tname = sexp_type_name(x);
        if (!tname || !sexp_stringp(tname)) {
            ser_error(s, "type without name cannot be serialized");
            return 0;
        }
        uint32_t si = strtab_intern(&s->strtab,
            sexp_string_data(tname), (uint32_t)sexp_string_size(tname));
        uint32_t id = ser_alloc_obj(s, SER_TYPE_REF);
        map_put(&s->visited, x, id);
        buf_write_u32(&s->obj_bufs[id - 1], si);
        return id;
    }

    case SEXP_SYNCLO: {
        /* Syntactic closures are compile-time artifacts from macro
         * expansion.  They wrap (env, free-vars, expr).  For
         * serialization we only need the underlying expression —
         * the macro environment is irrelevant at runtime. */
        uint32_t eid = ser_walk(s, sexp_synclo_expr(x));
        map_put(&s->visited, x, eid);
        return eid;
    }

    default: {
        /* Check for python-object or port types — not serializable */
        if (sexp_cpointerp(x)) {
            ser_error(s, "cannot serialize C pointer / Python object in continuation");
            return 0;
        }
        /* For other unrecognized pointer types (env cells, etc.) */
        char msg[256];
        sexp tname = sexp_type_name_by_index(s->ctx, tag);
        snprintf(msg, sizeof(msg),
                 "cannot serialize sexp with tag %u (%s)", (unsigned)tag,
                 (tname && sexp_stringp(tname)) ? sexp_string_data(tname) : "?");
        ser_error(s, msg);
        return 0;
    }
    }
}

/* ================================================================
 *  Assemble the final binary
 * ================================================================ */

static void ser_assemble_buf(SerState *s, Buffer *out) {
    buf_init(out);

    /* ---- HEADER (32 bytes) ---- */
    buf_write(out, SER_MAGIC, SER_MAGIC_LEN);           /* 0-3: magic */
    buf_write_u32(out, SER_VERSION);                     /* 4-7: version */
    buf_write_u32(out, (uint32_t)sizeof(void *));        /* 8-11: pointer_size */
    buf_write_u32(out, s->obj_count);                    /* 12-15: object count */
    buf_write_u32(out, s->strtab.count);                 /* 16-19: string count */
    /* 20-31: reserved (3 x uint32_t) */
    buf_write_u32(out, 0);
    buf_write_u32(out, 0);
    buf_write_u32(out, 0);

    /* ---- STRING TABLE ---- */
    for (uint32_t i = 0; i < s->strtab.count; i++) {
        buf_write_u32(out, s->strtab.lens[i]);
        buf_write(out, s->strtab.strs[i], s->strtab.lens[i]);
    }

    /* ---- OBJECT TABLE (kind + data_offset + data_size) ---- */
    size_t obj_table_size = (size_t)s->obj_count * (1 + 4 + 4);
    size_t data_start = out->len + obj_table_size;
    (void)data_start;

    size_t data_offset = 0;
    for (uint32_t i = 0; i < s->obj_count; i++) {
        buf_write_u8(out, s->obj_kinds[i]);
        buf_write_u32(out, (uint32_t)data_offset);
        buf_write_u32(out, (uint32_t)s->obj_bufs[i].len);
        data_offset += s->obj_bufs[i].len;
    }

    /* ---- OBJECT DATA ---- */
    for (uint32_t i = 0; i < s->obj_count; i++) {
        buf_write(out, s->obj_bufs[i].data, s->obj_bufs[i].len);
    }
}

#ifndef EVAL_STANDALONE
static PyObject *ser_assemble(SerState *s) {
    Buffer out;
    ser_assemble_buf(s, &out);
    PyObject *result = PyBytes_FromStringAndSize((const char *)out.data, out.len);
    buf_free(&out);
    return result;
}
#endif

/* ================================================================
 *  Pure-C serialization API (no Python dependency)
 * ================================================================ */

int ser_sexp_to_buf(sexp ctx, sexp env, sexp val,
                    unsigned char **out_data, size_t *out_len,
                    char *err_msg, size_t err_len) {
    SerState s;
    ser_init(&s, ctx, env);

    uint32_t root_id = ser_walk(&s, val);
    if (s.error || root_id == 0) {
        if (err_msg) {
            const char *msg = s.error ? s.error_msg : "failed to serialize";
            strncpy(err_msg, msg, err_len - 1);
            err_msg[err_len - 1] = '\0';
        }
        ser_free(&s);
        return -1;
    }

    Buffer out;
    ser_assemble_buf(&s, &out);
    ser_free(&s);

    *out_data = out.data;  /* caller owns — hand over the buffer */
    *out_len = out.len;
    return 0;
}

/* ================================================================
 *  Public serialization API (Python wrappers)
 * ================================================================ */

#ifndef EVAL_STANDALONE
PyObject *chibi_serialize_continuation(sexp ctx, sexp env, sexp cont) {
    /* Validate: must be a procedure.
     * In chibi-scheme, user-visible continuations from call/cc are
     * wrapped in a regular lambda (via continuation->procedure in
     * init-7.scm) that handles dynamic-wind.  So we accept any
     * procedure, not just raw RESUMECC bytecodes. */
    if (!sexp_procedurep(cont)) {
        PyErr_SetString(PyExc_TypeError,
                        "serialize_continuation: argument is not a procedure");
        return NULL;
    }

    SerState s;
    ser_init(&s, ctx, env);

    /* Walk from root */
    uint32_t root_id = ser_walk(&s, cont);

    if (s.error) {
        PyErr_SetString(PyExc_ValueError, s.error_msg);
        ser_free(&s);
        return NULL;
    }

    if (root_id == 0) {
        PyErr_SetString(PyExc_ValueError,
                        "serialize_continuation: failed to serialize root");
        ser_free(&s);
        return NULL;
    }

    /* Root must be ID 1 (first object allocated).
     * If not (shouldn't happen since cont is the first thing walked),
     * we could reorder, but by construction it will be 1 for
     * non-immediate continuations. Actually, the procedure itself
     * might reference sub-objects first via its own walk.
     * But ser_walk for PROCEDURE allocates ID before recursing, so
     * root_id == 1 is guaranteed. */

    PyObject *result = ser_assemble(&s);
    ser_free(&s);
    return result;
}
#endif /* !EVAL_STANDALONE */

/* ================================================================
 *  Deserialization
 * ================================================================ */

typedef struct {
    sexp ctx;
    sexp env;
    const unsigned char *data;
    size_t data_len;

    /* Header fields */
    uint32_t version;
    uint32_t ptr_size;
    uint32_t obj_count;
    uint32_t str_count;

    /* String table */
    char **strs;
    uint32_t *str_lens;

    /* Object table */
    uint8_t *obj_kinds;
    uint32_t *obj_data_offs;  /* offset into data section */
    uint32_t *obj_data_lens;

    /* Data section start */
    size_t data_section_start;

    /* Reconstructed sexps (indexed by ID-1) */
    sexp *objs;

    int error;
    char error_msg[256];
} DeserState;

static void deser_error(DeserState *d, const char *msg) {
    if (!d->error) {
        d->error = 1;
        strncpy(d->error_msg, msg, sizeof(d->error_msg) - 1);
        d->error_msg[sizeof(d->error_msg) - 1] = '\0';
    }
}

static const unsigned char *deser_obj_data(DeserState *d, uint32_t id) {
    if (id == 0 || id > d->obj_count) return NULL;
    return d->data + d->data_section_start + d->obj_data_offs[id - 1];
}

static int deser_parse_header(DeserState *d) {
    if (d->data_len < SER_HEADER_SIZE) {
        deser_error(d, "data too short for header");
        return 0;
    }

    if (memcmp(d->data, SER_MAGIC, SER_MAGIC_LEN) != 0) {
        deser_error(d, "invalid magic bytes");
        return 0;
    }

    size_t pos = SER_MAGIC_LEN;
    memcpy(&d->version, d->data + pos, 4); pos += 4;
    memcpy(&d->ptr_size, d->data + pos, 4); pos += 4;
    memcpy(&d->obj_count, d->data + pos, 4); pos += 4;
    memcpy(&d->str_count, d->data + pos, 4); pos += 4;

    if (d->version != SER_VERSION) {
        deser_error(d, "unsupported serialization version");
        return 0;
    }
    if (d->ptr_size != sizeof(void *)) {
        deser_error(d, "pointer size mismatch (cross-architecture not supported)");
        return 0;
    }
    return 1;
}

static int deser_parse_strings(DeserState *d, size_t *pos) {
    d->strs = (char **)calloc(d->str_count, sizeof(char *));
    d->str_lens = (uint32_t *)calloc(d->str_count, sizeof(uint32_t));

    for (uint32_t i = 0; i < d->str_count; i++) {
        if (*pos + 4 > (size_t)d->data_len) {
            deser_error(d, "truncated string table");
            return 0;
        }
        uint32_t len;
        memcpy(&len, d->data + *pos, 4); *pos += 4;
        if (*pos + len > (size_t)d->data_len) {
            deser_error(d, "truncated string data");
            return 0;
        }
        d->strs[i] = (char *)malloc(len + 1);
        memcpy(d->strs[i], d->data + *pos, len);
        d->strs[i][len] = '\0';
        d->str_lens[i] = len;
        *pos += len;
    }
    return 1;
}

static int deser_parse_obj_table(DeserState *d, size_t *pos) {
    d->obj_kinds = (uint8_t *)calloc(d->obj_count, sizeof(uint8_t));
    d->obj_data_offs = (uint32_t *)calloc(d->obj_count, sizeof(uint32_t));
    d->obj_data_lens = (uint32_t *)calloc(d->obj_count, sizeof(uint32_t));

    for (uint32_t i = 0; i < d->obj_count; i++) {
        if (*pos + 9 > (size_t)d->data_len) {
            deser_error(d, "truncated object table");
            return 0;
        }
        d->obj_kinds[i] = d->data[*pos]; *pos += 1;
        memcpy(&d->obj_data_offs[i], d->data + *pos, 4); *pos += 4;
        memcpy(&d->obj_data_lens[i], d->data + *pos, 4); *pos += 4;
    }
    d->data_section_start = *pos;
    return 1;
}

/* Get a resolved sexp for an object ID */
static sexp deser_get(DeserState *d, uint32_t id) {
    if (id == 0) return SEXP_VOID;
    if (id > d->obj_count) {
        deser_error(d, "invalid object reference ID");
        return SEXP_VOID;
    }
    return d->objs[id - 1];
}

/* Pass 1: Allocate all objects */
static void deser_alloc(DeserState *d) {
    sexp_gc_var3(tmp1, tmp2, tmp3);
    sexp_gc_preserve3(d->ctx, tmp1, tmp2, tmp3);

    for (uint32_t i = 0; i < d->obj_count; i++) {
        if (d->error) break;
        const unsigned char *odata = d->data + d->data_section_start + d->obj_data_offs[i];
        uint32_t olen = d->obj_data_lens[i];

        switch (d->obj_kinds[i]) {
        case SER_FIXNUM: {
            if (olen < 8) { deser_error(d, "truncated fixnum"); break; }
            int64_t v;
            memcpy(&v, odata, 8);
            d->objs[i] = sexp_make_fixnum((sexp_sint_t)v);
            break;
        }
        case SER_IMMEDIATE: {
            if (olen < 1) { deser_error(d, "truncated immediate"); break; }
            switch (odata[0]) {
            case SER_IMM_FALSE: d->objs[i] = SEXP_FALSE; break;
            case SER_IMM_TRUE:  d->objs[i] = SEXP_TRUE; break;
            case SER_IMM_NULL:  d->objs[i] = SEXP_NULL; break;
            case SER_IMM_EOF:   d->objs[i] = SEXP_EOF; break;
            case SER_IMM_VOID:  d->objs[i] = SEXP_VOID; break;
            case SER_IMM_UNDEF: d->objs[i] = SEXP_UNDEF; break;
            default: d->objs[i] = SEXP_VOID; break;
            }
            break;
        }
        case SER_SYMBOL: {
            if (olen < 4) { deser_error(d, "truncated symbol"); break; }
            uint32_t si;
            memcpy(&si, odata, 4);
            if (si >= d->str_count) { deser_error(d, "invalid string index"); break; }
            d->objs[i] = sexp_intern(d->ctx, d->strs[si], (int)d->str_lens[si]);
            sexp_preserve_object(d->ctx, d->objs[i]);
            break;
        }
        case SER_STRING: {
            if (olen < 4) { deser_error(d, "truncated string"); break; }
            uint32_t si;
            memcpy(&si, odata, 4);
            if (si >= d->str_count) { deser_error(d, "invalid string index"); break; }
            d->objs[i] = sexp_c_string(d->ctx, d->strs[si], (int)d->str_lens[si]);
            sexp_preserve_object(d->ctx, d->objs[i]);
            break;
        }
        case SER_PAIR: {
            d->objs[i] = sexp_cons(d->ctx, SEXP_VOID, SEXP_VOID);
            sexp_preserve_object(d->ctx, d->objs[i]);
            break;
        }
        case SER_VECTOR: {
            if (olen < 4) { deser_error(d, "truncated vector"); break; }
            uint32_t len;
            memcpy(&len, odata, 4);
            d->objs[i] = sexp_make_vector(d->ctx, sexp_make_fixnum(len), SEXP_VOID);
            sexp_preserve_object(d->ctx, d->objs[i]);
            break;
        }
        case SER_FLONUM: {
            if (olen < 8) { deser_error(d, "truncated flonum"); break; }
            double v;
            memcpy(&v, odata, 8);
            d->objs[i] = sexp_make_flonum(d->ctx, v);
            sexp_preserve_object(d->ctx, d->objs[i]);
            break;
        }
        case SER_CHAR: {
            if (olen < 4) { deser_error(d, "truncated char"); break; }
            uint32_t cp;
            memcpy(&cp, odata, 4);
            d->objs[i] = sexp_make_character(cp);
            break;
        }
        case SER_BYTECODE: {
            if (olen < 20) { deser_error(d, "truncated bytecode header"); break; }
            uint32_t bc_len;
            memcpy(&bc_len, odata + 12, 4);
            d->objs[i] = sexp_alloc_bytecode(d->ctx, bc_len);
            if (!d->objs[i] || !sexp_pointerp(d->objs[i])) {
                deser_error(d, "failed to allocate bytecode");
                break;
            }
            sexp_bytecode_length(d->objs[i]) = bc_len;
            uint32_t max_depth;
            memcpy(&max_depth, odata + 16, 4);
            sexp_bytecode_max_depth(d->objs[i]) = max_depth;
            /* Copy raw instruction bytes */
            if (olen < 20 + bc_len) {
                deser_error(d, "truncated bytecode data");
                break;
            }
            memcpy(sexp_bytecode_data(d->objs[i]), odata + 20, bc_len);
            sexp_preserve_object(d->ctx, d->objs[i]);
            break;
        }
        case SER_PROCEDURE: {
            tmp1 = sexp_make_vector(d->ctx, SEXP_ZERO, SEXP_VOID);
            d->objs[i] = sexp_make_procedure(d->ctx, SEXP_ZERO, SEXP_ZERO,
                                              sexp_global(d->ctx, SEXP_G_RESUMECC_BYTECODE),
                                              tmp1);
            sexp_preserve_object(d->ctx, d->objs[i]);
            break;
        }
        case SER_GLOBAL_REF: {
            if (olen < 4) { deser_error(d, "truncated global ref"); break; }
            uint32_t si;
            memcpy(&si, odata, 4);
            if (si >= d->str_count) { deser_error(d, "invalid string index"); break; }
            /* Resolve global by name */
            tmp1 = sexp_intern(d->ctx, d->strs[si], (int)d->str_lens[si]);
            tmp2 = sexp_env_cell(d->ctx, d->env, tmp1, 0);
            if (!tmp2 || tmp2 == SEXP_FALSE) {
                char msg[256];
                snprintf(msg, sizeof(msg),
                         "cannot resolve global '%s' in target context",
                         d->strs[si]);
                deser_error(d, msg);
                break;
            }
            d->objs[i] = tmp2;  /* env cell (pair) */
            break;
        }
        case SER_GLOBAL_REF_VAL: {
            /* Resolve to env cell; if missing, create with VOID
             * and let the fixup phase fill in the captured value. */
            if (olen < 8) { deser_error(d, "truncated global ref+val"); break; }
            uint32_t si;
            memcpy(&si, odata, 4);
            if (si >= d->str_count) { deser_error(d, "invalid string index"); break; }
            tmp1 = sexp_intern(d->ctx, d->strs[si], (int)d->str_lens[si]);
            tmp2 = sexp_env_cell(d->ctx, d->env, tmp1, 0);
            if (!tmp2 || tmp2 == SEXP_FALSE) {
                /* Not found — create with VOID, fixup will set value */
                sexp_env_define(d->ctx, d->env, tmp1, SEXP_VOID);
                tmp2 = sexp_env_cell(d->ctx, d->env, tmp1, 0);
            }
            d->objs[i] = tmp2;
            break;
        }
        case SER_OPCODE_REF: {
            if (olen < 4) { deser_error(d, "truncated opcode ref"); break; }
            uint32_t si;
            memcpy(&si, odata, 4);
            if (si >= d->str_count) { deser_error(d, "invalid string index"); break; }
            tmp1 = sexp_intern(d->ctx, d->strs[si], (int)d->str_lens[si]);
            tmp2 = sexp_env_ref(d->ctx, d->env, tmp1, SEXP_FALSE);
            if (tmp2 == SEXP_FALSE || !sexp_opcodep(tmp2)) {
                char msg[256];
                snprintf(msg, sizeof(msg),
                         "cannot resolve opcode '%s' in target context",
                         d->strs[si]);
                deser_error(d, msg);
                break;
            }
            d->objs[i] = tmp2;
            break;
        }
        case SER_RESUMECC_BC: {
            d->objs[i] = sexp_global(d->ctx, SEXP_G_RESUMECC_BYTECODE);
            break;
        }
        case SER_CONTEXT_DK: {
            /* Resolve to the target context's dk — preserves pointer identity */
            d->objs[i] = sexp_context_dk(d->ctx);
            break;
        }
        case SER_TYPE_REF: {
            if (olen < 4) { deser_error(d, "truncated type ref"); break; }
            uint32_t si;
            memcpy(&si, odata, 4);
            if (si >= d->str_count) { deser_error(d, "invalid type name index"); break; }
            const char *tname = d->strs[si];
            uint32_t tlen = d->str_lens[si];
            /* Linear scan registered types by name */
            int num_types = sexp_context_num_types(d->ctx);
            sexp found = SEXP_FALSE;
            for (int ti = num_types - 1; ti >= 0; ti--) {
                sexp t = sexp_type_by_index(d->ctx, ti);
                if (t && sexp_typep(t)) {
                    sexp tn = sexp_type_name(t);
                    if (sexp_stringp(tn) &&
                        (uint32_t)sexp_string_size(tn) == tlen &&
                        memcmp(sexp_string_data(tn), tname, tlen) == 0) {
                        found = t;
                        break;
                    }
                }
            }
            if (found == SEXP_FALSE) {
                char msg[256];
                snprintf(msg, sizeof(msg),
                         "cannot resolve type '%.200s' in target context", tname);
                deser_error(d, msg);
                break;
            }
            d->objs[i] = found;
            break;
        }
        case SER_BYTEVECTOR:
        default:
            deser_error(d, "unsupported object kind during deserialization");
            break;
        }
    }

    sexp_gc_release3(d->ctx);
}

/* Pass 2: Fixup cross-references */
static void deser_fixup(DeserState *d) {
    for (uint32_t i = 0; i < d->obj_count; i++) {
        if (d->error) break;
        const unsigned char *odata = d->data + d->data_section_start + d->obj_data_offs[i];
        uint32_t olen = d->obj_data_lens[i];

        switch (d->obj_kinds[i]) {
        case SER_PAIR: {
            if (olen < 8) break;
            uint32_t car_id, cdr_id;
            memcpy(&car_id, odata, 4);
            memcpy(&cdr_id, odata + 4, 4);
            sexp_car(d->objs[i]) = deser_get(d, car_id);
            sexp_cdr(d->objs[i]) = deser_get(d, cdr_id);
            break;
        }
        case SER_VECTOR: {
            if (olen < 4) break;
            uint32_t len;
            memcpy(&len, odata, 4);
            for (uint32_t j = 0; j < len && (4 + (j + 1) * 4) <= olen; j++) {
                uint32_t eid;
                memcpy(&eid, odata + 4 + j * 4, 4);
                sexp_vector_data(d->objs[i])[j] = deser_get(d, eid);
            }
            break;
        }
        case SER_BYTECODE: {
            if (olen < 20) break;
            uint32_t name_id, lit_id, src_id, bc_len;
            memcpy(&name_id, odata, 4);
            memcpy(&lit_id, odata + 4, 4);
            memcpy(&src_id, odata + 8, 4);
            memcpy(&bc_len, odata + 12, 4);
            /* max_depth already set in alloc pass */

            sexp_bytecode_name(d->objs[i]) = deser_get(d, name_id);
            sexp_bytecode_literals(d->objs[i]) = deser_get(d, lit_id);
            sexp_bytecode_source(d->objs[i]) = deser_get(d, src_id);

            /* Apply patches: write resolved sexp pointers into instruction stream */
            size_t patch_table_start = 20 + bc_len;
            if (patch_table_start + 4 > olen) break;
            uint32_t patch_count;
            memcpy(&patch_count, odata + patch_table_start, 4);
            size_t ppos = patch_table_start + 4;
            unsigned char *bc_data_ptr = sexp_bytecode_data(d->objs[i]);
            for (uint32_t j = 0; j < patch_count; j++) {
                if (ppos + 8 > olen) { deser_error(d, "truncated patch table"); break; }
                uint32_t byte_off, ref_id;
                memcpy(&byte_off, odata + ppos, 4); ppos += 4;
                memcpy(&ref_id, odata + ppos, 4); ppos += 4;
                if (byte_off + sizeof(sexp) > bc_len) {
                    deser_error(d, "patch offset out of range");
                    break;
                }
                sexp resolved = deser_get(d, ref_id);
                sexp *slot = (sexp *)(&bc_data_ptr[byte_off]);
                *slot = resolved;
            }
            break;
        }
        case SER_PROCEDURE: {
            if (olen < 16) break;
            uint32_t bc_id, vars_id, flags, num_args;
            memcpy(&bc_id, odata, 4);
            memcpy(&vars_id, odata + 4, 4);
            memcpy(&flags, odata + 8, 4);
            memcpy(&num_args, odata + 12, 4);

            sexp_procedure_code(d->objs[i]) = deser_get(d, bc_id);
            sexp_procedure_vars(d->objs[i]) = deser_get(d, vars_id);
            /* flags is a raw char, num_args is a raw short/int — restore
             * directly without fixnum boxing. */
            sexp_procedure_flags(d->objs[i]) = (char)flags;
            sexp_procedure_num_args(d->objs[i]) = (sexp_proc_num_args_t)num_args;
            break;
        }
        case SER_GLOBAL_REF_VAL: {
            /* Legacy: only fill in captured value if the cell was
             * just created (value is VOID).  For existing env cells,
             * keep the current value to preserve mutations made
             * between serialization and resume. */
            if (olen < 8) break;
            if (sexp_cdr(d->objs[i]) == SEXP_VOID) {
                uint32_t val_id;
                memcpy(&val_id, odata + 4, 4);
                sexp_cdr(d->objs[i]) = deser_get(d, val_id);
            }
            break;
        }
        /* Other kinds need no fixup */
        default:
            break;
        }
    }
}

/* Pure-C deserialization (no Python dependency) */
sexp deser_sexp_from_buf(sexp ctx, sexp env,
                         const unsigned char *data, size_t len,
                         char *err_msg, size_t err_len) {
    DeserState d;
    memset(&d, 0, sizeof(d));
    d.ctx = ctx;
    d.env = env;
    d.data = data;
    d.data_len = len;

    /* Parse header */
    if (!deser_parse_header(&d)) goto fail;

    /* Parse string table */
    size_t pos = SER_HEADER_SIZE;
    if (!deser_parse_strings(&d, &pos)) goto fail;

    /* Parse object table */
    if (!deser_parse_obj_table(&d, &pos)) goto fail;

    /* Allocate output array */
    d.objs = (sexp *)calloc(d.obj_count, sizeof(sexp));

    /* Pass 1: Allocate */
    deser_alloc(&d);
    if (d.error) goto fail;

    /* Pass 2: Fixup */
    deser_fixup(&d);
    if (d.error) goto fail;

    /* Root object is ID 1 */
    {
        sexp result = d.objs[0];

        /* Release all preserved objects except root */
        for (uint32_t i = 1; i < d.obj_count; i++) {
            if (d.objs[i] && sexp_pointerp(d.objs[i]))
                sexp_release_object(ctx, d.objs[i]);
        }

        /* Cleanup */
        for (uint32_t i = 0; i < d.str_count; i++) free(d.strs[i]);
        free(d.strs);
        free(d.str_lens);
        free(d.obj_kinds);
        free(d.obj_data_offs);
        free(d.obj_data_lens);
        free(d.objs);

        return result;
    }

fail:
    {
        const char *msg = d.error ? d.error_msg : "deserialization failed";
        if (err_msg) {
            strncpy(err_msg, msg, err_len - 1);
            err_msg[err_len - 1] = '\0';
        }

        if (d.strs) {
            for (uint32_t i = 0; i < d.str_count; i++) free(d.strs[i]);
            free(d.strs);
        }
        free(d.str_lens);
        free(d.obj_kinds);
        free(d.obj_data_offs);
        free(d.obj_data_lens);
        if (d.objs) {
            for (uint32_t i = 0; i < d.obj_count; i++) {
                if (d.objs[i] && sexp_pointerp(d.objs[i]))
                    sexp_release_object(ctx, d.objs[i]);
            }
            free(d.objs);
        }

        return SEXP_FALSE;
    }
}

#ifndef EVAL_STANDALONE
sexp chibi_deserialize_continuation(sexp ctx, sexp env,
                                    const unsigned char *data,
                                    Py_ssize_t len) {
    DeserState d;
    memset(&d, 0, sizeof(d));
    d.ctx = ctx;
    d.env = env;
    d.data = data;
    d.data_len = len;

    /* Parse header */
    if (!deser_parse_header(&d)) goto fail;

    /* Parse string table */
    size_t pos = SER_HEADER_SIZE;
    if (!deser_parse_strings(&d, &pos)) goto fail;

    /* Parse object table */
    if (!deser_parse_obj_table(&d, &pos)) goto fail;

    /* Allocate output array */
    d.objs = (sexp *)calloc(d.obj_count, sizeof(sexp));

    /* Pass 1: Allocate */
    deser_alloc(&d);
    if (d.error) goto fail;

    /* Pass 2: Fixup */
    deser_fixup(&d);
    if (d.error) goto fail;

    /* Root object is ID 1 */
    sexp result = d.objs[0];

    /* Release all preserved objects except root */
    for (uint32_t i = 1; i < d.obj_count; i++) {
        if (d.objs[i] && sexp_pointerp(d.objs[i])) {
            sexp_release_object(ctx, d.objs[i]);
        }
    }

    /* Cleanup */
    for (uint32_t i = 0; i < d.str_count; i++) free(d.strs[i]);
    free(d.strs);
    free(d.str_lens);
    free(d.obj_kinds);
    free(d.obj_data_offs);
    free(d.obj_data_lens);
    free(d.objs);


    return result;

fail:
    {
        const char *msg = d.error ? d.error_msg : "deserialization failed";
        PyErr_SetString(PyExc_ValueError, msg);

        /* Cleanup */
        if (d.strs) {
            for (uint32_t i = 0; i < d.str_count; i++) free(d.strs[i]);
            free(d.strs);
        }
        free(d.str_lens);
        free(d.obj_kinds);
        free(d.obj_data_offs);
        free(d.obj_data_lens);
        if (d.objs) {
            for (uint32_t i = 0; i < d.obj_count; i++) {
                if (d.objs[i] && sexp_pointerp(d.objs[i]))
                    sexp_release_object(ctx, d.objs[i]);
            }
            free(d.objs);
        }
    
        return SEXP_FALSE;
    }
}
#endif /* !EVAL_STANDALONE */

/* ================================================================
 *  sexp-level API (callable from Eval as bridge functions)
 * ================================================================ */

/* serialize_continuation(proc) → bytevector */
sexp bridge_serialize_continuation(sexp ctx, sexp self, sexp_sint_t n, sexp cont) {
    if (!sexp_procedurep(cont)) {
        return sexp_user_exception(ctx, self,
            "serialize-continuation: not a procedure", cont);
    }

    sexp env = sexp_context_env(ctx);
    SerState s;
    ser_init(&s, ctx, env);

    uint32_t root_id = ser_walk(&s, cont);

    if (s.error) {
        sexp msg = sexp_c_string(ctx, s.error_msg, -1);
        ser_free(&s);
        return sexp_user_exception(ctx, self,
            "serialize-continuation failed", msg);
    }

    if (root_id == 0) {
        ser_free(&s);
        return sexp_user_exception(ctx, self,
            "serialize-continuation: failed to serialize root", cont);
    }

    Buffer out;
    ser_assemble_buf(&s, &out);
    ser_free(&s);

    sexp bv = sexp_make_bytes(ctx, sexp_make_fixnum(out.len), SEXP_ZERO);
    memcpy(sexp_bytes_data(bv), out.data, out.len);
    buf_free(&out);
    return bv;
}

/* deserialize_continuation(bytevector) → procedure */
sexp bridge_deserialize_continuation(sexp ctx, sexp self, sexp_sint_t n, sexp bv) {
    if (!sexp_bytesp(bv)) {
        return sexp_user_exception(ctx, self,
            "deserialize-continuation: not a bytevector", bv);
    }

    sexp env = sexp_context_env(ctx);
#ifndef EVAL_STANDALONE
    return chibi_deserialize_continuation(ctx, env,
        (const unsigned char *)sexp_bytes_data(bv),
        (Py_ssize_t)sexp_bytes_length(bv));
#else
    char err_msg[256] = {0};
    sexp result = deser_sexp_from_buf(ctx, env,
        (const unsigned char *)sexp_bytes_data(bv),
        (size_t)sexp_bytes_length(bv), err_msg, sizeof(err_msg));
    if (result == SEXP_FALSE && err_msg[0])
        return sexp_user_exception(ctx, self, err_msg, bv);
    return result;
#endif
}
