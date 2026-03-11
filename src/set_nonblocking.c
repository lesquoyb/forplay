/*
 * set_nonblocking.c
 *
 * Portable C helper for setting socket non-blocking mode.
 *
 * On Linux/macOS, calling the variadic `ioctl()` from a Fortran
 * `bind(C)` interface is unreliable because gfortran does not emit
 * the correct calling-convention metadata for variadic functions.
 * This thin C wrapper calls `fcntl()` (POSIX) or `ioctlsocket()`
 * (Windows) natively, avoiding the variadic ABI issue entirely.
 *
 * Returns 0 on success, -1 on failure.
 */

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>

int c_set_nonblocking(int fd, int enable) {
    unsigned long mode = enable ? 1 : 0;
    return ioctlsocket((SOCKET)fd, FIONBIO, &mode);
}

/*
 * c_poll_connect — check the status of a non-blocking connect().
 *
 * Returns:
 *    1  = connected
 *    0  = still pending
 *   -1  = getsockopt itself failed
 *   -N  = connection failed with SO_ERROR == N  (N > 1)
 */
int c_poll_connect(int sockfd) {
    int err = 0;
    int len = sizeof(err);
    if (getsockopt(sockfd, SOL_SOCKET, SO_ERROR, (char *)&err, &len) != 0)
        return -1;
    if (err != 0)
        return -(err);
    /* SO_ERROR == 0 — confirm with getpeername */
    struct sockaddr_in peer;
    int plen = sizeof(peer);
    if (getpeername(sockfd, (struct sockaddr *)&peer, &plen) == 0)
        return 1;   /* connected */
    return 0;       /* still pending */
}

#else
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>

int c_set_nonblocking(int fd, int enable) {
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags == -1) return -1;
    if (enable)
        flags |= O_NONBLOCK;
    else
        flags &= ~O_NONBLOCK;
    return fcntl(fd, F_SETFL, flags);
}

/*
 * c_poll_connect — check the status of a non-blocking connect().
 *
 * Returns:
 *    1  = connected
 *    0  = still pending
 *   -1  = getsockopt itself failed
 *   -N  = connection failed with SO_ERROR == N  (N > 1)
 */
int c_poll_connect(int sockfd) {
    int err = 0;
    socklen_t len = sizeof(err);
    if (getsockopt(sockfd, SOL_SOCKET, SO_ERROR, &err, &len) != 0)
        return -1;
    if (err != 0)
        return -(err);
    /* SO_ERROR == 0 — confirm with getpeername */
    struct sockaddr_in peer;
    socklen_t plen = sizeof(peer);
    if (getpeername(sockfd, (struct sockaddr *)&peer, &plen) == 0)
        return 1;   /* connected */
    return 0;       /* still pending */
}

#endif
