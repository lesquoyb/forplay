! server_main.f90
!
! ForPlay Dedicated Server - Headless game host with rooms
!
! Players connect, browse rooms, create/join rooms.
! When a room has 2 players and the host starts, the game begins.
!
! Usage: forplay-server [port]  (default port: 12345)

program forplay_server
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: stderr => error_unit, stdout => output_unit
    use :: game_mod
    use :: network
    use :: server_mod
    implicit none

    integer, parameter :: DEFAULT_PORT = 12345
    integer, parameter :: MAX_CLIENTS  = 20
    integer, parameter :: RECV_BUF_SZ  = 4096

    ! --- Client state ---
    integer, parameter :: CLIENT_LOBBY = 0   ! in lobby, browsing rooms
    integer, parameter :: CLIENT_ROOM  = 1   ! in a room
    integer, parameter :: CLIENT_GAME  = 2   ! playing a game

    type :: client_type
        logical :: active = .false.
        integer(c_int) :: fd = -1
        character(len=64) :: ip = ' '
        character(len=32) :: name = ' '
        integer :: state = CLIENT_LOBBY
        integer :: room_idx = 0
        ! Lobby message recv buffer
        integer(c_int8_t) :: recv_buf(RECV_BUF_SZ)
        integer :: recv_pos = 0
    end type

    ! Server state
    integer(c_int) :: server_fd = -1
    character(len=256) :: local_ip, error_msg
    integer :: rc

    ! Clients
    type(client_type) :: clients(MAX_CLIENTS)

    ! Rooms
    type(room_type) :: rooms(MAX_ROOMS)

    ! Game states per room
    type(game_state) :: room_gs(MAX_ROOMS)

    ! Port from command line
    integer :: port
    character(len=32) :: port_arg

    ! --- Parse command line ---
    port = DEFAULT_PORT
    if (command_argument_count() >= 1) then
        call get_command_argument(1, port_arg)
        read(port_arg, *, err=10) port
        goto 11
    10  write(stderr, '(A)') 'Error: invalid port argument'
        stop 1
    11  continue
    end if

    ! --- Init ---
    call net_init(rc)
    if (rc /= 0) then
        write(stderr, '(A,I0)') 'Network init failed: ', rc
        stop 1
    end if

    call room_init(rooms)

    write(stdout, '(A)') '============================================'
    write(stdout, '(A)') '  ForPlay Dedicated Server (with rooms)'
    write(stdout, '(A)') '============================================'

    call server_start(int(port, c_int16_t), server_fd, local_ip, error_msg)
    if (server_fd < 0) then
        write(stderr, '(A,A)') 'Failed to start server: ', trim(error_msg)
        stop 1
    end if
    write(stdout, '(A,A)') 'Local IP: ', trim(local_ip)
    write(stdout, '(A,I0)') 'Listening on port: ', port
    write(stdout, '(A)') 'Waiting for connections...'

    ! --- Main event loop ---
    do
        ! Accept new connections
        call try_accept_new_client()

        ! Process each client
        call process_all_clients()

        call sleep_ms(10)
    end do

contains

    ! ==================================================================
    ! Accept a new client into the first free slot
    ! ==================================================================
    subroutine try_accept_new_client()
        integer(c_int) :: cli_fd
        character(len=256) :: cli_ip
        logical :: got_client
        integer :: i

        call server_try_accept(server_fd, cli_fd, cli_ip, got_client)
        if (.not. got_client) return

        ! Find free client slot
        do i = 1, MAX_CLIENTS
            if (.not. clients(i)%active) then
                clients(i)%active = .true.
                clients(i)%fd = cli_fd
                clients(i)%ip = trim(cli_ip)
                clients(i)%name = ' '
                clients(i)%state = CLIENT_LOBBY
                clients(i)%room_idx = 0
                clients(i)%recv_pos = 0
                write(stdout, '(A,I0,A,A)') 'Client ', i, ' connected from ', trim(cli_ip)
                return
            end if
        end do

        ! No free slots - close connection
        write(stdout, '(A)') 'Server full, rejecting connection'
        call net_close(cli_fd)
    end subroutine

    ! ==================================================================
    ! Process all active clients
    ! ==================================================================
    subroutine process_all_clients()
        integer :: i

        do i = 1, MAX_CLIENTS
            if (.not. clients(i)%active) cycle

            select case (clients(i)%state)
                case (CLIENT_LOBBY, CLIENT_ROOM)
                    call process_lobby_client(i)
                case (CLIENT_GAME)
                    call process_game_client(i)
            end select
        end do
    end subroutine

    ! ==================================================================
    ! Process a client in lobby/room state (lobby messages)
    ! ==================================================================
    subroutine process_lobby_client(ci)
        integer, intent(in) :: ci

        integer(c_int8_t) :: msg_type
        integer(c_int8_t) :: payload(1024)
        integer :: payload_len
        logical :: got

        call net_try_recv_msg(clients(ci)%fd, clients(ci)%recv_buf, RECV_BUF_SZ, &
                              clients(ci)%recv_pos, msg_type, payload, 1024, payload_len, got)

        if (.not. got) return

        select case (msg_type)
            case (MSG_LIST_ROOMS)
                call handle_list_rooms(ci)
            case (MSG_CREATE_ROOM)
                call handle_create_room(ci, payload, payload_len)
            case (MSG_JOIN_ROOM)
                call handle_join_room(ci, payload, payload_len)
            case (MSG_LEAVE_ROOM)
                call handle_leave_room(ci)
            case (MSG_CONFIG_CHANGE)
                call handle_config_change(ci, payload, payload_len)
            case (MSG_START_GAME)
                call handle_start_game(ci)
            case default
                ! Unknown message - disconnect
                write(stdout, '(A,I0,A,I0)') 'Client ', ci, ' sent unknown msg type: ', int(msg_type)
                call disconnect_client(ci)
        end select
    end subroutine

    ! ==================================================================
    ! Handle MSG_LIST_ROOMS: send room list back
    ! ==================================================================
    subroutine handle_list_rooms(ci)
        integer, intent(in) :: ci
        integer(c_int8_t), target :: buf(1024)
        integer :: nbytes
        integer(c_int) :: st

        call room_build_list_payload(rooms, buf, nbytes)
        call net_send_msg(clients(ci)%fd, MSG_ROOM_LIST, buf, nbytes, st)
    end subroutine

    ! ==================================================================
    ! Handle MSG_CREATE_ROOM: payload = [name_len:1][name:N]
    ! ==================================================================
    subroutine handle_create_room(ci, payload, plen)
        integer, intent(in) :: ci, plen
        integer(c_int8_t), intent(in) :: payload(:)
        integer :: nlen, idx
        character(len=MAX_ROOM_NAME) :: rname
        character(len=32) :: pname

        if (plen < 2) return

        ! Parse room name
        nlen = int(payload(1))
        if (nlen <= 0 .or. nlen > MAX_ROOM_NAME .or. plen < 1 + nlen) return
        rname = ' '
        rname(1:nlen) = transfer(payload(2:1+nlen), rname(1:nlen))

        ! Use player name from after room name, or IP as fallback
        if (plen > 1 + nlen) then
            block
                integer :: pname_len
                pname_len = int(payload(2 + nlen))
                pname = ' '
                if (pname_len > 0 .and. plen >= 2 + nlen + pname_len) then
                    pname(1:pname_len) = transfer(payload(3+nlen:2+nlen+pname_len), pname(1:pname_len))
                end if
            end block
        else
            pname = trim(clients(ci)%ip)
        end if
        clients(ci)%name = pname

        idx = room_create(rooms, trim(rname), clients(ci)%fd, trim(clients(ci)%ip), trim(pname))
        if (idx > 0) then
            clients(ci)%state = CLIENT_ROOM
            clients(ci)%room_idx = idx
            write(stdout, '(A,I0,A,A,A,I0)') 'Client ', ci, ' created room "', trim(rname), '", idx=', idx
            ! Send room update to the creator
            call send_room_update(idx)
        else
            write(stdout, '(A,I0,A)') 'Client ', ci, ' failed to create room (server full)'
        end if
    end subroutine

    ! ==================================================================
    ! Handle MSG_JOIN_ROOM: payload = [room_id:1][name_len:1][name:N]
    ! ==================================================================
    subroutine handle_join_room(ci, payload, plen)
        integer, intent(in) :: ci, plen
        integer(c_int8_t), intent(in) :: payload(:)
        integer :: rid, nlen
        character(len=32) :: pname
        logical :: ok

        if (plen < 1) return
        rid = int(payload(1))

        ! Parse player name
        pname = trim(clients(ci)%ip)
        if (plen >= 3) then
            nlen = int(payload(2))
            if (nlen > 0 .and. plen >= 2 + nlen) then
                pname = ' '
                pname(1:nlen) = transfer(payload(3:2+nlen), pname(1:nlen))
            end if
        end if
        clients(ci)%name = pname

        ok = room_join(rooms, rid, clients(ci)%fd, trim(clients(ci)%ip), trim(pname))
        if (ok) then
            clients(ci)%state = CLIENT_ROOM
            clients(ci)%room_idx = rid
            write(stdout, '(A,I0,A,I0)') 'Client ', ci, ' joined room ', rid
            call send_room_update(rid)
        else
            write(stdout, '(A,I0,A,I0)') 'Client ', ci, ' failed to join room ', rid
        end if
    end subroutine

    ! ==================================================================
    ! Handle MSG_LEAVE_ROOM
    ! ==================================================================
    subroutine handle_leave_room(ci)
        integer, intent(in) :: ci
        integer :: ridx

        ridx = clients(ci)%room_idx
        if (ridx > 0) then
            call room_leave(rooms, ridx, clients(ci)%fd)
            write(stdout, '(A,I0,A,I0)') 'Client ', ci, ' left room ', ridx
            ! Notify remaining players
            if (rooms(ridx)%active) call send_room_update(ridx)
        end if
        clients(ci)%state = CLIENT_LOBBY
        clients(ci)%room_idx = 0
    end subroutine

    ! ==================================================================
    ! Handle MSG_CONFIG_CHANGE: host changes config
    ! payload = [16 bytes server_config]
    ! ==================================================================
    subroutine handle_config_change(ci, payload, plen)
        integer, intent(in) :: ci, plen
        integer(c_int8_t), intent(in) :: payload(:)
        integer :: ridx

        if (plen < SERVER_CONFIG_BYTES) return
        ridx = clients(ci)%room_idx
        if (ridx < 1 .or. ridx > MAX_ROOMS) return
        if (.not. rooms(ridx)%active) return

        ! Only host can change config
        if (.not. is_room_host(ridx, clients(ci)%fd)) return

        call server_config_from_bytes(payload, rooms(ridx)%cfg)
        ! Broadcast update to all room members
        call send_room_update(ridx)
    end subroutine

    ! ==================================================================
    ! Handle MSG_START_GAME: host starts the game
    ! ==================================================================
    subroutine handle_start_game(ci)
        integer, intent(in) :: ci
        integer :: ridx
        integer(c_int) :: st
        integer(c_int8_t), target :: dummy(1)

        ridx = clients(ci)%room_idx
        if (ridx < 1 .or. ridx > MAX_ROOMS) return
        if (.not. rooms(ridx)%active) return
        if (.not. is_room_host(ridx, clients(ci)%fd)) return
        if (rooms(ridx)%num_players < 2) return

        rooms(ridx)%in_game = .true.

        ! Initialize game
        call server_init_game(room_gs(ridx), rooms(ridx)%cfg)

        write(stdout, '(A,I0,A,I0,A,I0)') 'Room ', ridx, ': game started, maze ', &
            room_gs(ridx)%maze%w, 'x', room_gs(ridx)%maze%h

        ! Send MSG_START_GAME to all room players (empty payload)
        block
            integer :: j
            do j = 1, rooms(ridx)%num_players
                call net_send_msg(rooms(ridx)%players(j)%fd, MSG_START_GAME, dummy, 0, st)
            end do
        end block

        ! Send game init to both players
        ! Player 1 (host) = hider by default (host_hides=true)
        call server_send_init_to_fd(rooms(ridx)%players(1)%fd, room_gs(ridx), .true., st)
        call server_send_init_to_fd(rooms(ridx)%players(2)%fd, room_gs(ridx), .true., st)

        ! Switch all room clients to game state
        block
            integer :: j, k
            do j = 1, rooms(ridx)%num_players
                do k = 1, MAX_CLIENTS
                    if (clients(k)%active .and. clients(k)%fd == rooms(ridx)%players(j)%fd) then
                        clients(k)%state = CLIENT_GAME
                        clients(k)%recv_pos = 0  ! reset recv buffer for game protocol
                        exit
                    end if
                end do
            end do
        end block
    end subroutine

    ! ==================================================================
    ! Process a client in game state (action relay)
    ! ==================================================================
    subroutine process_game_client(ci)
        integer, intent(in) :: ci
        integer :: atype, param, ridx, opp_fd
        logical :: got, moved
        integer :: role
        integer(c_int) :: st

        ridx = clients(ci)%room_idx
        if (ridx < 1 .or. .not. rooms(ridx)%active) return
        if (.not. rooms(ridx)%in_game) return

        call server_try_recv_action(clients(ci)%fd, atype, param, got)
        if (.not. got) return

        ! Find opponent fd and role
        call find_opponent_and_role(ridx, clients(ci)%fd, opp_fd, role)
        if (opp_fd < 0) return

        ! Quit: notify opponent, end room game
        if (atype == 8) then
            write(stdout, '(A,I0,A,I0)') 'Room ', ridx, ': player quit, role=', role
            call server_send_action(opp_fd, 8, 0)
            call end_room_game(ridx)
            return
        end if

        ! Replay (host only): reinit game, send action 9 + init to both
        if (atype == 9) then
            if (.not. is_room_host(ridx, clients(ci)%fd)) return
            write(stdout, '(A,I0,A)') 'Room ', ridx, ': host replaying'
            call server_init_game(room_gs(ridx), rooms(ridx)%cfg)
            call server_send_action(rooms(ridx)%players(1)%fd, 9, 0)
            call server_send_action(rooms(ridx)%players(2)%fd, 9, 0)
            call server_send_init_to_fd(rooms(ridx)%players(1)%fd, &
                                        room_gs(ridx), .true., st)
            call server_send_init_to_fd(rooms(ridx)%players(2)%fd, &
                                        room_gs(ridx), .true., st)
            return
        end if

        ! Back to room (host only): send action 10 to both, end game, send room update
        if (atype == 10) then
            if (.not. is_room_host(ridx, clients(ci)%fd)) return
            write(stdout, '(A,I0,A)') 'Room ', ridx, ': back to room'
            call server_send_action(rooms(ridx)%players(1)%fd, 10, 0)
            call server_send_action(rooms(ridx)%players(2)%fd, 10, 0)
            call end_room_game(ridx)
            call send_room_update(ridx)
            return
        end if

        ! Leave room from game (non-host): notify opponent, end game, remove from room
        if (atype == 11) then
            write(stdout, '(A,I0,A,I0)') 'Room ', ridx, ': non-host leaving, client=', ci
            call server_send_action(opp_fd, 8, 0)
            call end_room_game(ridx)
            call room_leave(rooms, ridx, clients(ci)%fd)
            clients(ci)%room_idx = 0
            clients(ci)%state = CLIENT_LOBBY
            if (rooms(ridx)%active) call send_room_update(ridx)
            return
        end if

        ! Don't process normal moves after game over
        if (room_gs(ridx)%game_over) return

        ! Normal action: relay and apply
        call server_send_action(opp_fd, atype, param)

        if (atype == 0) then
            moved = game_do_move(room_gs(ridx), role, param)
        else if (atype == 7) then
            call game_do_pass(room_gs(ridx))
        else
            call game_do_use_item(room_gs(ridx), role, atype, param)
        end if

        call game_end_turn(room_gs(ridx))

        if (room_gs(ridx)%game_over) then
            if (room_gs(ridx)%seeker_won) then
                write(stdout, '(A,I0,A)') 'Room ', ridx, ': seeker wins!'
            else
                write(stdout, '(A,I0,A)') 'Room ', ridx, ': hider wins!'
            end if
        end if
    end subroutine

    ! ==================================================================
    ! End the game in a room, return players to room state
    ! ==================================================================
    subroutine end_room_game(ridx)
        integer, intent(in) :: ridx
        integer :: j, k

        rooms(ridx)%in_game = .false.

        do j = 1, rooms(ridx)%num_players
            do k = 1, MAX_CLIENTS
                if (clients(k)%active .and. clients(k)%fd == rooms(ridx)%players(j)%fd) then
                    clients(k)%state = CLIENT_ROOM
                    clients(k)%recv_pos = 0
                    exit
                end if
            end do
        end do
    end subroutine

    ! ==================================================================
    ! Find the opponent fd and the player's role in a room
    ! ==================================================================
    subroutine find_opponent_and_role(ridx, my_fd, opp_fd, my_role)
        integer, intent(in) :: ridx
        integer(c_int), intent(in) :: my_fd
        integer(c_int), intent(out) :: opp_fd
        integer, intent(out) :: my_role

        opp_fd = -1
        my_role = ROLE_HIDER

        if (rooms(ridx)%num_players < 2) return

        if (rooms(ridx)%players(1)%fd == my_fd) then
            my_role = ROLE_HIDER   ! player 1 = hider
            opp_fd = rooms(ridx)%players(2)%fd
        else
            my_role = ROLE_SEEKER  ! player 2 = seeker
            opp_fd = rooms(ridx)%players(1)%fd
        end if
    end subroutine

    ! ==================================================================
    ! Send room update to all players in a room
    ! ==================================================================
    subroutine send_room_update(ridx)
        integer, intent(in) :: ridx
        integer(c_int8_t), target :: buf(512)
        integer :: nbytes, j
        integer(c_int) :: st

        if (.not. rooms(ridx)%active) return

        call room_build_update_payload(rooms(ridx), buf, nbytes)

        do j = 1, rooms(ridx)%num_players
            call net_send_msg(rooms(ridx)%players(j)%fd, MSG_ROOM_UPDATE, buf, nbytes, st)
        end do
    end subroutine

    ! ==================================================================
    ! Check if a fd is the host of a room
    ! ==================================================================
    function is_room_host(ridx, fd) result(is_host)
        integer, intent(in) :: ridx
        integer(c_int), intent(in) :: fd
        logical :: is_host
        integer :: j

        is_host = .false.
        do j = 1, rooms(ridx)%num_players
            if (rooms(ridx)%players(j)%fd == fd .and. rooms(ridx)%players(j)%is_host) then
                is_host = .true.
                return
            end if
        end do
    end function

    ! ==================================================================
    ! Disconnect a client cleanly
    ! ==================================================================
    subroutine disconnect_client(ci)
        integer, intent(in) :: ci
        integer :: ridx

        ridx = clients(ci)%room_idx
        if (ridx > 0 .and. rooms(ridx)%active) then
            ! If in game, notify opponent
            if (rooms(ridx)%in_game .and. clients(ci)%state == CLIENT_GAME) then
                block
                    integer(c_int) :: opp_fd
                    integer :: role
                    call find_opponent_and_role(ridx, clients(ci)%fd, opp_fd, role)
                    if (opp_fd >= 0) call server_send_action(opp_fd, 8, 0)
                    call end_room_game(ridx)
                end block
            end if
            call room_leave(rooms, ridx, clients(ci)%fd)
            if (rooms(ridx)%active) call send_room_update(ridx)
        end if

        if (clients(ci)%fd >= 0) then
            call net_close(clients(ci)%fd)
        end if

        write(stdout, '(A,I0,A)') 'Client ', ci, ' disconnected'
        clients(ci)%active = .false.
        clients(ci)%fd = -1
        clients(ci)%state = CLIENT_LOBBY
        clients(ci)%room_idx = 0
        clients(ci)%recv_pos = 0
    end subroutine

    ! ==================================================================
    ! Cross-platform millisecond sleep
    ! ==================================================================
    subroutine sleep_ms(ms)
        integer, intent(in) :: ms
        integer :: count_start, count_end, count_rate

        call system_clock(count_start, count_rate)
        do
            call system_clock(count_end)
            if (real(count_end - count_start) / real(count_rate) >= real(ms) / 1000.0) exit
        end do
    end subroutine

end program forplay_server
