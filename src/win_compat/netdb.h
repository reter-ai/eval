/* win_compat/netdb.h -- redirect to winsock2 (already included via sexp.h) */
#ifndef _WIN_COMPAT_NETDB_H
#define _WIN_COMPAT_NETDB_H
#include <ws2tcpip.h>  /* getaddrinfo, freeaddrinfo, etc. */
#endif
