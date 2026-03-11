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

int c_set_nonblocking(int fd, int enable) {
    unsigned long mode = enable ? 1 : 0;
    return ioctlsocket((SOCKET)fd, FIONBIO, &mode);
}
#else
#include <fcntl.h>

int c_set_nonblocking(int fd, int enable) {
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags == -1) return -1;
    if (enable)
        flags |= O_NONBLOCK;
    else
        flags &= ~O_NONBLOCK;
    return fcntl(fd, F_SETFL, flags);
}
#endif
