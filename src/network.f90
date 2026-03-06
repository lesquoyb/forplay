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
    public :: net_connect_to_server
    public :: net_get_local_ip, net_get_public_ip
    public :: net_try_accept_nonblocking
    public :: net_send_bytes, net_recv_all, net_try_recv_bytes

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
            error_msg = 'Erreur: creation du socket echouee'
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
            write(error_msg, '(A,I0)') 'Erreur: bind echoue, code=', get_errno()
            call net_close(server_fd)
            server_fd = -1
            return
        end if

        status = listen(server_fd, 1)
        if (status /= 0) then
            write(error_msg, '(A,I0)') 'Erreur: listen echoue, code=', get_errno()
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

        integer(c_long), target :: mode
        integer(c_int) :: res
        type(sockaddr_in), target :: peer_addr
        integer(c_int), target :: peer_addrlen
        type(in_addr) :: peer_in
        type(c_ptr) :: ip_ptr
        character(kind=c_char), pointer :: ip_chars(:)
        integer :: i

        got_client = .false.
        cli_ip = ' '
        cli_fd = -1

        ! Set non-blocking
        mode = 1_c_long
        res = ioctlsocket(srv_fd, FIONBIO, c_loc(mode))

        ! Try accept (like example: c_null_ptr, 0_c_int)
        cli_fd = accept(srv_fd, c_null_ptr, 0_c_int)

        ! Restore blocking
        mode = 0_c_long
        res = ioctlsocket(srv_fd, FIONBIO, c_loc(mode))

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
                peer_in%s_addr = peer_addr%sin_addr
                ip_ptr = inet_ntoa(peer_in)
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
    subroutine net_connect_to_server(ip_str, port, sock_fd, error_msg)
        character(len=*), intent(in) :: ip_str
        integer(c_int16_t), intent(in) :: port
        integer(c_int), intent(out) :: sock_fd
        character(len=*), intent(out) :: error_msg

        type(sockaddr_in), target :: addr
        integer(c_int) :: status

        error_msg = ' '

        sock_fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
        if (sock_fd < 0) then
            error_msg = 'Erreur: creation du socket echouee'
            sock_fd = -1
            return
        end if

        addr%sin_family = AF_INET
        addr%sin_port   = htons(port)
        addr%sin_addr   = inet_addr(trim(ip_str) // c_null_char)

        if (addr%sin_addr == -1) then
            error_msg = 'Erreur: adresse IP invalide'
            call net_close(sock_fd)
            sock_fd = -1
            return
        end if

        addr%sin_zero = c_null_char

        status = connect(sock_fd, c_loc(addr), int(c_sizeof(addr), c_int))
        if (status /= 0) then
            write(error_msg, '(A,I0)') 'Erreur: connexion echouee, code=', get_errno()
            call net_close(sock_fd)
            sock_fd = -1
            return
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
        type(in_addr) :: ia
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
            ia%s_addr = local_addr%sin_addr
            cstr_ptr = inet_ntoa(ia)
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

        integer(c_long), target :: mode
        integer(c_int) :: res

        received = 0

        mode = 1_c_long
        res = ioctlsocket(sockfd, FIONBIO, c_loc(mode))

        received = recv(sockfd, c_loc(buf), maxlen, 0_c_int)
        if (received < 0) received = 0

        mode = 0_c_long
        res = ioctlsocket(sockfd, FIONBIO, c_loc(mode))
    end subroutine

end module network
