/*  eval_embedded_scm.h -- Embedded scheme files for standalone eval  */

#ifndef EVAL_EMBEDDED_SCM_H
#define EVAL_EMBEDDED_SCM_H

#include <stddef.h>

typedef struct {
    const char *path;       /* relative path, e.g. "init-7.scm" */
    const char *content;    /* file content (NUL-terminated) */
    size_t size;            /* size including NUL terminator */
} EmbeddedSchemeFile;

/* Table of all embedded files (NULL-terminated) */
extern const EmbeddedSchemeFile embedded_scheme_files[];
extern const int embedded_scheme_file_count;

/* Look up an embedded .scm file by relative path.
 * Returns the file content (NUL-terminated) or NULL if not found. */
const char *embedded_find_scm(const char *path);

#endif /* EVAL_EMBEDDED_SCM_H */
