/*  _chibi_serialize.h -- Continuation serialization/deserialization  */
#ifndef _CHIBI_SERIALIZE_H
#define _CHIBI_SERIALIZE_H

#include <Python.h>
#include <chibi/eval.h>

/* Binary format magic and version */
#define SER_MAGIC       "CCHI"
#define SER_MAGIC_LEN   4
#define SER_VERSION     1
#define SER_HEADER_SIZE 32

/* Object kind tags */
enum ser_object_kind {
    SER_FIXNUM       = 1,
    SER_IMMEDIATE    = 2,
    SER_SYMBOL       = 3,
    SER_STRING       = 4,
    SER_PAIR         = 5,
    SER_VECTOR       = 6,
    SER_FLONUM       = 7,
    SER_CHAR         = 8,
    SER_BYTECODE     = 9,
    SER_PROCEDURE    = 10,
    SER_GLOBAL_REF   = 11,
    SER_OPCODE_REF   = 12,
    SER_RESUMECC_BC  = 13,
    SER_BYTEVECTOR   = 14,
    SER_ENV_CELL     = 15,
    SER_CONTEXT_DK   = 16,
    SER_GLOBAL_REF_VAL = 17,  /* name + captured value */
};

/* Immediate sub-kinds */
enum ser_immediate_kind {
    SER_IMM_FALSE = 0,
    SER_IMM_TRUE  = 1,
    SER_IMM_NULL  = 2,
    SER_IMM_EOF   = 3,
    SER_IMM_VOID  = 4,
    SER_IMM_UNDEF = 5,
};

/* Public API */
PyObject *chibi_serialize_continuation(sexp ctx, sexp env, sexp cont);
sexp chibi_deserialize_continuation(sexp ctx, sexp env,
                                    const unsigned char *data,
                                    Py_ssize_t len);

#endif /* _CHIBI_SERIALIZE_H */
