! network.f90
!
! Network module for hosting and joining games.
! Uses fortran-socket for all socket C bindings.

module network
    use iso_c_binding
    use socket_lib
    use socket_lib_constants
    implicit none
    private

    public :: net_init, net_cleanup
    public :: net_start_server, net_close
    public :: net_begin_connect, net_poll_connect
    public :: net_get_local_ip, net_get_public_ip
    public :: net_try_accept_nonblocking
    public :: net_send_bytes, net_recv_all, net_try_recv_bytes
    public :: net_send_msg, net_try_recv_msg

    ! Lobby message types
    integer(c_int8_t), parameter, public :: MSG_LIST_ROOMS   = 10
    integer(c_int8_t), parameter, public :: MSG_ROOM_LIST    = 11
    integer(c_int8_t), parameter, public :: MSG_CREATE_ROOM  = 12
    integer(c_int8_t), parameter, public :: MSG_JOIN_ROOM    = 13
    integer(c_int8_t), parameter, public :: MSG_LEAVE_ROOM   = 14
    integer(c_int8_t), parameter, public :: MSG_ROOM_UPDATE  = 15
    integer(c_int8_t), parameter, public :: MSG_CONFIG_CHANGE = 16
    integer(c_int8_t), parameter, public :: MSG_START_GAME   = 17
    integer(c_int8_t), parameter, public :: MSG_GAME_INIT    = 18
    integer(c_int8_t), parameter, public :: MSG_KICK         = 19

contains

    ! ==================================================================
    ! Init / cleanup
    ! ==================================================================
    subroutine net_init(status)
        integer, intent(out) :: status
        type(WSADATA), target :: wsa_data
        integer(c_int) :: res
        res = WSAStartup(int(Z'0202', c_int), c_loc(wsa_data))
        status = int(res)
    end subroutine

    subroutine net_cleanup()
        integer(c_int) :: res
        res = WSACleanup()
    end subroutine

    subroutine net_close(sockfd)
        integer(c_int), intent(in) :: sockfd
        integer(c_int) :: res
        if (is_windows) then
            res = closesocket(sockfd)
        else
            res = close(sockfd)
        end if
    end subroutine

    ! ==================================================================
    ! Start TCP server: socket → setsockopt → bind → listen
    ! ==================================================================
    subroutine net_start_server(port, server_fd, error_msg)
        integer(c_int16_t), intent(in) :: port
        integer(c_int), intent(out) :: server_fd
        character(len=*), intent(out) :: error_msg

        type(sockaddr_in), target :: addr
        integer(c_int) :: status
        integer(c_int), target :: optyes

        error_msg = ' '
        optyes = 1_c_int

        server_fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
        if (server_fd < 0) then
            error_msg = 'Error: socket creation failed'
            server_fd = -1
            return
        end if

        status = setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &
                            c_loc(optyes), int(c_sizeof(optyes), c_int))
        status = setsockopt(server_fd, IPPROTO_TCP, TCP_NODELAY, &
                            c_loc(optyes), int(c_sizeof(optyes), c_int))

        addr%sin_family = AF_INET
        addr%sin_port   = htons(port)
        addr%sin_addr   = htonl(INADDR_ANY)
        addr%sin_zero   = c_null_char

        status = bind(server_fd, c_loc(addr), int(c_sizeof(addr), c_int))
        if (status /= 0) then
            write(error_msg, '(A,I0)') 'Error: bind failed, code=', get_errno()
            call net_close(server_fd)
            server_fd = -1
            return
        end if

        status = listen(server_fd, 1)
        if (status /= 0) then
            write(error_msg, '(A,I0)') 'Error: listen failed, code=', get_errno()
            call net_close(server_fd)
            server_fd = -1
            return
        end if
    end subroutine

    ! ==================================================================
    ! Non-blocking accept — uses the lib's accept(fd, c_null_ptr, 0)
    ! then getpeername to retrieve client IP.
    ! ==================================================================
    subroutine net_try_accept_nonblocking(srv_fd, cli_fd, cli_ip, got_client)
        integer(c_int), intent(in)  :: srv_fd
        integer(c_int), intent(out) :: cli_fd
        character(len=*), intent(out) :: cli_ip
        logical, intent(out) :: got_client

        integer(c_int) :: res
        type(sockaddr_in), target :: peer_addr
        integer(c_int), target :: peer_addrlen
        type(c_ptr) :: ip_ptr
        character(kind=c_char), pointer :: ip_chars(:)
        integer :: i

        got_client = .false.
        cli_ip = ' '
        cli_fd = -1

        ! Set non-blocking
        res = set_nonblocking(srv_fd, 1_c_int)

        ! Try accept (like example: c_null_ptr, 0_c_int)
        cli_fd = accept(srv_fd, c_null_ptr, 0_c_int)

        ! Restore blocking
        res = set_nonblocking(srv_fd, 0_c_int)

        if (cli_fd >= 0) then
            got_client = .true.

            ! Set TCP_NODELAY on accepted socket
            block
                integer(c_int), target :: opt_on
                opt_on = 1_c_int
                res = setsockopt(cli_fd, IPPROTO_TCP, TCP_NODELAY, &
                                 c_loc(opt_on), int(c_sizeof(opt_on), c_int))
            end block

            ! Get peer IP via getpeername
            peer_addrlen = int(c_sizeof(peer_addr), c_int)
            res = getpeername(cli_fd, c_loc(peer_addr), c_loc(peer_addrlen))
            if (res == 0) then
                ip_ptr = inet_ntoa(peer_addr%sin_addr)
                if (c_associated(ip_ptr)) then
                    call c_f_pointer(ip_ptr, ip_chars, [16])
                    do i = 1, 16
                        if (ip_chars(i) == c_null_char) exit
                        cli_ip(i:i) = ip_chars(i)
                    end do
                end if
            end if
        end if
    end subroutine

    ! ==================================================================
    ! Connect to a remote server
    ! ==================================================================
    ! ==================================================================
    ! Begin a non-blocking connect. Returns the socket fd (>= 0) if
    ! the connect was initiated. The caller must then call
    ! net_poll_connect() each frame to check for completion.
    ! ==================================================================
    subroutine net_begin_connect(ip_str, port, sock_fd, error_msg)
        character(len=*), intent(in) :: ip_str
        integer(c_int16_t), intent(in) :: port
        integer(c_int), intent(out) :: sock_fd
        character(len=*), intent(out) :: error_msg

        type(sockaddr_in), target :: addr
        integer(c_int) :: status

        error_msg = ' '

        sock_fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
        if (sock_fd < 0) then
            error_msg = 'Error: socket creation failed'
            sock_fd = -1
            return
        end if

        addr%sin_family = AF_INET
        addr%sin_port   = htons(port)
        addr%sin_addr   = inet_addr(trim(ip_str) // c_null_char)

        if (addr%sin_addr == -1) then
            error_msg = 'Error: invalid IP address'
            call net_close(sock_fd)
            sock_fd = -1
            return
        end if

        addr%sin_zero = c_null_char

        ! Set non-blocking before connect
        status = set_nonblocking(sock_fd, 1_c_int)

        status = connect(sock_fd, c_loc(addr), int(c_sizeof(addr), c_int))
        if (status /= 0) then
            ! Non-blocking connect returns error immediately — that's expected
            ! On Windows: WSAEWOULDBLOCK (10035), on POSIX: EINPROGRESS (36/115)
            ! Any of these means "connection in progress"
            if (is_windows) then
                status = WSAGetLastError()
                if (status /= 10035) then
                    write(error_msg, '(A,I0)') 'Error: connect failed, code=', status
                    call net_close(sock_fd)
                    sock_fd = -1
                    return
                end if
            else
                status = get_errno()
                if (status /= 36 .and. status /= 115) then
                    write(error_msg, '(A,I0)') 'Error: connect failed, code=', status
                    call net_close(sock_fd)
                    sock_fd = -1
                    return
                end if
            end if
        end if
        ! Connection is in progress (or already completed) — caller polls
    end subroutine

    ! ==================================================================
    ! Poll a non-blocking connect for completion.
    !   result:  1 = connected,  0 = still pending,  -1 = failed
    ! On success, the socket is restored to blocking mode.
    ! ==================================================================
    subroutine net_poll_connect(sock_fd, result, error_msg)
        integer(c_int), intent(inout) :: sock_fd
        integer, intent(out) :: result
        character(len=*), intent(out) :: error_msg

        integer(c_int) :: res
        integer(c_int), target :: opt_on

        error_msg = ' '
        result = 0  ! pending

        ! Use the C helper which calls getsockopt(SO_ERROR) + getpeername
        ! natively, avoiding Fortran ↔ C interop issues with pointer
        ! arguments that caused failures on Linux.
        res = poll_connect(sock_fd)

        if (res == 1) then
            ! Connected! Restore blocking mode and set TCP_NODELAY
            res = set_nonblocking(sock_fd, 0_c_int)
            opt_on = 1_c_int
            res = setsockopt(sock_fd, IPPROTO_TCP, TCP_NODELAY, &
                             c_loc(opt_on), int(c_sizeof(opt_on), c_int))
            result = 1
        else if (res == 0) then
            ! Still pending
            result = 0
        else
            ! res < 0: error
            if (res == -1) then
                error_msg = 'Error: connection check failed'
            else
                write(error_msg, '(A,I0)') 'Error: connection failed, code=', -res
            end if
            call net_close(sock_fd)
            sock_fd = -1
            result = -1
        end if
    end subroutine

    ! ==================================================================
    ! Get local hostname
    ! ==================================================================
    subroutine net_get_local_ip(ip_str)
        character(len=*), intent(out) :: ip_str
        type(sockaddr_in), target :: addr, local_addr
        integer(c_int) :: udp_fd, res
        integer(c_int), target :: addrlen
        type(c_ptr) :: cstr_ptr
        character(len=1), pointer :: cstr(:)
        integer :: i

        ip_str = '127.0.0.1'

        ! Create a UDP socket
        udp_fd = socket(AF_INET, SOCK_DGRAM, 0_c_int)
        if (udp_fd < 0) return

        ! Connect to 8.8.8.8:53 (Google DNS) — no actual traffic is sent
        addr%sin_family = int(AF_INET, c_int16_t)
        addr%sin_port   = htons(53_c_int16_t)
        addr%sin_addr   = inet_addr('8.8.8.8' // c_null_char)
        addr%sin_zero   = transfer(0_c_int64_t, addr%sin_zero)

        res = connect(udp_fd, c_loc(addr), int(c_sizeof(addr), c_int))
        if (res < 0) then
            if (is_windows) then
                res = closesocket(udp_fd)
            else
                res = close(udp_fd)
            end if
            return
        end if

        ! Get local address assigned by the OS
        addrlen = int(c_sizeof(local_addr), c_int)
        res = getsockname(udp_fd, c_loc(local_addr), c_loc(addrlen))
        if (res == 0) then
            cstr_ptr = inet_ntoa(local_addr%sin_addr)
            if (c_associated(cstr_ptr)) then
                call c_f_pointer(cstr_ptr, cstr, [256])
                ip_str = ' '
                do i = 1, 256
                    if (cstr(i) == c_null_char) exit
                    ip_str(i:i) = cstr(i)
                end do
            end if
        end if

        if (is_windows) then
            res = closesocket(udp_fd)
        else
            res = close(udp_fd)
        end if
    end subroutine

    subroutine net_get_public_ip(ip_str)
        character(len=*), intent(out) :: ip_str
        character(len=512) :: tmp_file, line
        integer :: iu, ios
        logical :: exists

        ip_str = '?'
        tmp_file = 'forplay_pubip.tmp'

        ! Use curl to query public IP
        call execute_command_line( &
            'curl -s -m 3 https://api.ipify.org > ' // trim(tmp_file) // ' 2>&1', &
            wait=.true., exitstat=ios)
        if (ios /= 0) return

        inquire(file=trim(tmp_file), exist=exists)
        if (.not. exists) return

        open(newunit=iu, file=trim(tmp_file), status='old', iostat=ios)
        if (ios /= 0) return
        read(iu, '(A)', iostat=ios) line
        close(iu, status='delete')

        if (ios == 0 .and. len_trim(line) > 0 .and. len_trim(line) <= 45) then
            ip_str = trim(line)
        end if
    end subroutine

    ! ==================================================================
    ! Send helpers
    ! ==================================================================

    ! Send exactly nbytes. Returns 0 on success, -1 on failure.
    subroutine net_send_bytes(sockfd, buf, nbytes, status)
        integer(c_int), intent(in)  :: sockfd
        integer(c_int8_t), intent(in), target :: buf(*)
        integer(c_int), intent(in)  :: nbytes
        integer(c_int), intent(out) :: status
        integer(c_int) :: total, n

        total = 0
        do while (total < nbytes)
            n = send(sockfd, c_loc(buf(total + 1)), nbytes - total, 0_c_int)
            if (n <= 0) then
                status = -1
                return
            end if
            total = total + n
        end do
        status = 0
    end subroutine

    ! Blocking receive of exactly nbytes.
    subroutine net_recv_all(sockfd, buf, nbytes, status)
        integer(c_int), intent(in)  :: sockfd
        integer(c_int8_t), intent(out), target :: buf(*)
        integer(c_int), intent(in)  :: nbytes
        integer(c_int), intent(out) :: status
        integer(c_int) :: total, n

        total = 0
        do while (total < nbytes)
            n = recv(sockfd, c_loc(buf(total + 1)), nbytes - total, 0_c_int)
            if (n <= 0) then
                status = -1
                return
            end if
            total = total + n
        end do
        status = 0
    end subroutine

    ! Non-blocking receive attempt. received = bytes actually read (0 if none).
    subroutine net_try_recv_bytes(sockfd, buf, maxlen, received)
        integer(c_int), intent(in)  :: sockfd
        integer(c_int8_t), intent(out), target :: buf(*)
        integer(c_int), intent(in)  :: maxlen
        integer(c_int), intent(out) :: received

        integer(c_int) :: res

        received = 0

        res = set_nonblocking(sockfd, 1_c_int)

        received = recv(sockfd, c_loc(buf), maxlen, 0_c_int)
        if (received < 0) received = 0

        res = set_nonblocking(sockfd, 0_c_int)
    end subroutine

    ! ==================================================================
    ! Send a length-prefixed lobby message: [type:1][len_hi:1][len_lo:1][payload:N]
    ! Returns status: 0=success, -1=failure
    ! ==================================================================
    subroutine net_send_msg(sockfd, msg_type, payload, payload_len, status)
        integer(c_int), intent(in) :: sockfd
        integer(c_int8_t), intent(in) :: msg_type
        integer(c_int8_t), intent(in), target :: payload(*)
        integer, intent(in) :: payload_len
        integer(c_int), intent(out) :: status

        integer(c_int8_t), target :: header(3)

        header(1) = msg_type
        header(2) = int(ishft(payload_len, -8), c_int8_t)  ! len high byte
        header(3) = int(iand(payload_len, 255), c_int8_t)   ! len low byte

        call net_send_bytes(sockfd, header, 3_c_int, status)
        if (status /= 0) return

        if (payload_len > 0) then
            call net_send_bytes(sockfd, payload, int(payload_len, c_int), status)
        end if
    end subroutine

    ! ==================================================================
    ! Try to receive a lobby message (non-blocking, accumulating).
    !
    ! recv_buf / recv_pos: persistent accumulation buffer managed by caller.
    ! On success: msg_type, payload(1:payload_len) filled, recv_pos updated.
    ! got = .true. if a complete message was received.
    ! ==================================================================
    subroutine net_try_recv_msg(sockfd, recv_buf, recv_buf_size, recv_pos, &
                                 msg_type, payload, max_payload, payload_len, got)
        integer(c_int), intent(in)    :: sockfd
        integer(c_int8_t), intent(inout) :: recv_buf(*)
        integer, intent(in)           :: recv_buf_size
        integer, intent(inout)        :: recv_pos
        integer(c_int8_t), intent(out) :: msg_type
        integer(c_int8_t), intent(out) :: payload(*)
        integer, intent(in)           :: max_payload
        integer, intent(out)          :: payload_len
        logical, intent(out)          :: got

        integer(c_int8_t), target :: tmp(1024)
        integer(c_int) :: nbytes
        integer :: needed, plen, copy_len

        got = .false.
        msg_type = 0
        payload_len = 0

        ! Try to receive more data
        call net_try_recv_bytes(sockfd, tmp, int(min(1024, recv_buf_size - recv_pos), c_int), nbytes)
        if (nbytes > 0) then
            recv_buf(recv_pos+1 : recv_pos+nbytes) = tmp(1:nbytes)
            recv_pos = recv_pos + nbytes
        end if

        ! Need at least 3 bytes for header
        if (recv_pos < 3) return

        ! Parse header
        msg_type = recv_buf(1)
        plen = ior(ishft(iand(int(recv_buf(2)), 255), 8), iand(int(recv_buf(3)), 255))

        needed = 3 + plen
        if (recv_pos < needed) return

        ! Complete message available
        payload_len = min(plen, max_payload)
        if (payload_len > 0) then
            payload(1:payload_len) = recv_buf(4 : 3+payload_len)
        end if
        got = .true.

        ! Shift remaining data to front of buffer
        if (recv_pos > needed) then
            recv_buf(1 : recv_pos - needed) = recv_buf(needed+1 : recv_pos)
        end if
        recv_pos = recv_pos - needed
    end subroutine

end module network
