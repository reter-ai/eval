/* unistd.h -- Windows shim.
 * On Windows, usleep is provided by _chibi_win_compat.h
 * (which is included by chibi/sexp.h). This header exists so that
 * #include <unistd.h> in library .c files resolves without error. */
#pragma once
#ifdef _WIN32
#include <io.h>
/* Everything else provided by _chibi_win_compat.h via sexp.h */
#else
#include_next <unistd.h>
#endif
