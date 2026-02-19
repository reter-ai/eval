/*  eval_embedded_extract.c -- Embedded scheme file lookup  */

#include "eval_embedded_scm.h"
#include <string.h>

const char *embedded_find_scm(const char *path) {
    for (int i = 0; i < embedded_scheme_file_count; i++) {
        if (strcmp(embedded_scheme_files[i].path, path) == 0)
            return embedded_scheme_files[i].content;
    }
    return NULL;
}
