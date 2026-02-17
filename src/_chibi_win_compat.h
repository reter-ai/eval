#ifndef _CHIBI_WIN_COMPAT_H
#define _CHIBI_WIN_COMPAT_H
#ifdef _WIN32

/* Windows compatibility layer for POSIX APIs used by chibi green threads.
 * Provides: fcntl (with ioctlsocket for sockets), poll (via WSAPoll),
 * gettimeofday, usleep, fileno, suseconds_t, nfds_t.
 *
 * Note: sexp.h includes <winsock2.h> then <windows.h> before this file,
 * so those headers are already available. winsock2.h also defines
 * struct pollfd / WSAPOLLFD, so we don't redefine them. */

#include <io.h>    /* _fileno */

/* ---- fcntl constants ---- */
#ifndef F_GETFL
#define F_GETFL 3
#endif
#ifndef F_SETFL
#define F_SETFL 4
#endif
#ifndef O_NONBLOCK
#define O_NONBLOCK 0x0004
#endif

/* fcntl — sets non-blocking mode on sockets via ioctlsocket */
#include <stdarg.h>
static inline int fcntl(int fd, int cmd, ...) {
    if (cmd == F_SETFL) {
        va_list ap;
        va_start(ap, cmd);
        int flags = va_arg(ap, int);
        va_end(ap);
        if (flags & O_NONBLOCK) {
            u_long mode = 1;
            ioctlsocket((SOCKET)fd, FIONBIO, &mode);
        } else {
            u_long mode = 0;
            ioctlsocket((SOCKET)fd, FIONBIO, &mode);
        }
        return 0;
    }
    return 0;  /* F_GETFL returns 0 (no flags) */
}

/* ---- poll via WSAPoll ---- */
/* winsock2.h already defines POLLIN, POLLOUT, POLLERR, struct pollfd (WSAPOLLFD) */
#ifndef POLLIN
#define POLLIN   0x0001
#endif
#ifndef POLLOUT
#define POLLOUT  0x0010
#endif
#ifndef POLLERR
#define POLLERR  0x0008
#endif

typedef unsigned int nfds_t;

/* Use winsock2's WSAPOLLFD as struct pollfd (they're compatible) */
static inline int poll(struct pollfd *fds, nfds_t nfds, int timeout) {
    /* WSAPoll works directly with struct pollfd (which IS WSAPOLLFD) */
    int ret = WSAPoll((WSAPOLLFD *)fds, (ULONG)nfds, timeout);
    if (ret == SOCKET_ERROR) {
        /* Fallback: report all ready (for non-socket fds) */
        nfds_t i;
        for (i = 0; i < nfds; i++)
            fds[i].revents = fds[i].events;
        return (int)nfds;
    }
    return ret;
}

/* ---- gettimeofday ---- */
typedef long suseconds_t;

static inline int gettimeofday(struct timeval *tv, void *tz) {
    FILETIME ft;
    unsigned long long t;
    (void)tz;
    GetSystemTimeAsFileTime(&ft);
    t = ((unsigned long long)ft.dwHighDateTime << 32) | ft.dwLowDateTime;
    t -= 116444736000000000ULL;
    t /= 10;
    tv->tv_sec  = (long)(t / 1000000ULL);
    tv->tv_usec = (long)(t % 1000000ULL);
    return 0;
}

/* ---- fileno ---- */
#ifndef fileno
#define fileno _fileno
#endif

/* ---- usleep ---- */
static inline void usleep(unsigned int us) {
    Sleep(us / 1000 + (us % 1000 ? 1 : 0));
}

#endif /* _WIN32 */
#endif /* _CHIBI_WIN_COMPAT_H */
