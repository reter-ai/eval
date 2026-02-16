#ifndef _CHIBI_WIN_COMPAT_H
#define _CHIBI_WIN_COMPAT_H
#ifdef _WIN32

/* Stubs for POSIX non-blocking I/O APIs used by chibi green threads.
 * The green thread scheduler itself is pure C and works fine.
 * Only the I/O blocking helpers need these, and in our embedded
 * usage they can be no-ops (blocking I/O is acceptable). */

#include <io.h>    /* _fileno */

/* fcntl constants */
#ifndef F_GETFL
#define F_GETFL 3
#endif
#ifndef F_SETFL
#define F_SETFL 4
#endif
#ifndef O_NONBLOCK
#define O_NONBLOCK 0x0004
#endif

/* fcntl stub — always returns 0 (no flags) */
static inline int fcntl(int fd, int cmd, ...) {
    (void)fd; (void)cmd;
    return 0;
}

/* poll stub — always says "ready" */
#ifndef POLLIN
#define POLLIN 0x0001
struct pollfd { int fd; short events; short revents; };
#endif
static inline int poll(struct pollfd *fds, unsigned long nfds, int timeout) {
    (void)fds; (void)nfds; (void)timeout;
    return 1; /* always ready */
}

/* fileno — MSVC has _fileno */
#ifndef fileno
#define fileno _fileno
#endif

/* usleep — Windows has Sleep(ms), not usleep(us) */
static inline void usleep(unsigned int us) {
    Sleep(us / 1000);
}

#endif /* _WIN32 */
#endif /* _CHIBI_WIN_COMPAT_H */
