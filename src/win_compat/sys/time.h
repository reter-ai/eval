/* sys/time.h -- Windows shim.
 * On Windows, gettimeofday/suseconds_t are provided by _chibi_win_compat.h
 * (which is included by chibi/sexp.h). This header exists so that
 * #include <sys/time.h> in library .c files resolves without error. */
#pragma once
#ifdef _WIN32
/* Everything already provided by _chibi_win_compat.h via sexp.h */
#else
#include_next <sys/time.h>
#endif
