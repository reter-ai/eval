/*  _chibi_bridge.h -- Built-in bridge functions for Eval<->Python  */

#ifndef CHIBI_BRIDGE_H
#define CHIBI_BRIDGE_H

#include <Python.h>
#include <chibi/eval.h>

/* Register all bridge functions in the given environment */
void register_bridge_functions(sexp ctx, sexp env);

#endif /* CHIBI_BRIDGE_H */
