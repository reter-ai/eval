/*  _eval_quantity.c -- Quantity type for dimensional analysis
 *
 *  Quantity = (magnitude, unit-map)
 *    magnitude: fixnum/flonum or Decimal (slot 0, GC-traced)
 *    unit-map:  sorted alist of (base-unit-symbol . exponent) (slot 1, GC-traced)
 *
 *  Uses sexp_register_simple_type with 2 GC-traced fields.
 *  All unit logic lives in Scheme (quantity-oo.scm); C provides only
 *  the type tag and fast accessors for hot-path dispatch.
 */

#include <chibi/eval.h>

/* ================================================================
 * Type registration
 * ================================================================ */

static sexp quantity_type = SEXP_FALSE;
static sexp_uint_t quantity_type_tag = 0;

void register_quantity_type(sexp ctx, sexp env) {
    sexp_gc_var3(name, slots, sym);
    sexp_gc_preserve3(ctx, name, slots, sym);

    /* Build slots list: (magnitude umap) */
    sym = sexp_intern(ctx, "umap", -1);
    slots = sexp_cons(ctx, sym, SEXP_NULL);
    sym = sexp_intern(ctx, "magnitude", -1);
    slots = sexp_cons(ctx, sym, slots);

    name = sexp_c_string(ctx, "quantity", -1);
    quantity_type = sexp_register_simple_type(ctx, name, SEXP_FALSE, slots);

    if (sexp_typep(quantity_type)) {
        quantity_type_tag = sexp_type_tag(quantity_type);
        sexp_preserve_object(ctx, quantity_type);
    }

    sexp_gc_release3(ctx);
}

/* ================================================================
 * Helpers
 * ================================================================ */

static int is_quantity(sexp x) {
    return sexp_pointerp(x) && sexp_pointer_tag(x) == quantity_type_tag;
}

/* ================================================================
 * Bridge functions
 * ================================================================ */

/* %qty-make(magnitude, umap) -> Quantity */
static sexp bridge_qty_make(sexp ctx, sexp self, sexp_sint_t n,
                            sexp mag, sexp umap) {
    sexp_gc_var1(q);
    sexp_gc_preserve1(ctx, q);
    q = sexp_alloc_type(ctx, pair, quantity_type_tag);
    sexp_slot_set(q, 0, mag);
    sexp_slot_set(q, 1, umap);
    sexp_gc_release1(ctx);
    return q;
}

/* %qty?(x) -> boolean  -- fast tag check for operator dispatch */
static sexp bridge_qty_p(sexp ctx, sexp self, sexp_sint_t n, sexp x) {
    return is_quantity(x) ? SEXP_TRUE : SEXP_FALSE;
}

/* %qty-magnitude(q) -> value */
static sexp bridge_qty_magnitude(sexp ctx, sexp self, sexp_sint_t n, sexp q) {
    if (!is_quantity(q))
        return sexp_user_exception(ctx, SEXP_FALSE, "expected quantity", q);
    return sexp_slot_ref(q, 0);
}

/* %qty-umap(q) -> alist */
static sexp bridge_qty_umap(sexp ctx, sexp self, sexp_sint_t n, sexp q) {
    if (!is_quantity(q))
        return sexp_user_exception(ctx, SEXP_FALSE, "expected quantity", q);
    return sexp_slot_ref(q, 1);
}

/* ================================================================
 * Bridge registration
 * ================================================================ */

void register_quantity_bridge_functions(sexp ctx, sexp env) {
    sexp_define_foreign(ctx, env, "%qty-make", 2, bridge_qty_make);
    sexp_define_foreign(ctx, env, "%qty?", 1, bridge_qty_p);
    sexp_define_foreign(ctx, env, "%qty-magnitude", 1, bridge_qty_magnitude);
    sexp_define_foreign(ctx, env, "%qty-umap", 1, bridge_qty_umap);
}
