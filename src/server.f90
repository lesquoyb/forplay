! server.f90
!
! Server module: encapsulates all game-hosting logic.
!
! This module can be used by:
!   - The player client (embedded hosting: player hosts + plays)
!   - A dedicated headless server program
!
! Responsibilities:
!   - Server lifecycle (start, stop, accept connections)
!   - Game configuration
!   - Game initialization (maze generation, item placement)
!   - Network protocol: serialize/parse game init packets
!   - Action relay between players

module server_mod
    use, intrinsic :: iso_c_binding
    use :: game_mod
    use :: network
    implicit none
    private

    ! --- Public API ---
    public :: server_config
    public :: server_config_defaults
    public :: server_start, server_stop
    public :: server_try_accept
    public :: server_init_game
    public :: server_build_init_packet
    public :: server_send_init_to_fd
    public :: server_parse_init_packet
    public :: server_restart_game
    public :: server_send_action, server_try_recv_action

    ! Room system
    public :: room_type, room_player_type
    public :: MAX_ROOMS, MAX_ROOM_PLAYERS, MAX_ROOM_NAME
    public :: room_init, room_create, room_join, room_leave, room_find
    public :: room_build_list_payload, room_build_update_payload
    public :: room_parse_list_payload, room_parse_update_payload
    public :: server_config_to_bytes, server_config_from_bytes
    public :: SERVER_CONFIG_BYTES

    ! Embedded server
    public :: emb_server_type, emb_client_type
    public :: EMB_MAX_CLIENTS
    public :: emb_server_init, emb_server_tick, emb_server_shutdown

    ! --- Limits ---
    integer, parameter :: MAX_ROOMS        = 10
    integer, parameter :: MAX_ROOM_PLAYERS = 6
    integer, parameter :: MAX_ROOM_NAME    = 32
    integer, parameter :: SERVER_CONFIG_BYTES = 17  ! serialised config size

    ! --- Game configuration (set by host before starting) ---
    type :: server_config
        integer :: maze_w      = 21
        integer :: maze_h      = 15
        integer :: min_dist    = 15
        integer :: item_counts(NUM_ITEM_TYPES) = [3, 2, 2, 1, 2]
        integer :: speed_h     = 2
        integer :: speed_s     = 1
        integer :: vision_h    = 1
        integer :: vision_s    = 2
        logical :: reveal_h    = .false.
        logical :: reveal_s    = .true.
        integer :: num_keys    = 1
        integer :: turn_timer  = 0    ! seconds, 0=disabled
        integer :: branch_pct  = 5   ! 0-100 (percent)
    end type

    ! --- Room player ---
    type :: room_player_type
        integer(c_int) :: fd = -1
        character(len=32) :: name = ' '
        character(len=64) :: ip   = ' '
        logical :: is_host = .false.
        integer :: team = TEAM_ESCAPE
    end type

    ! --- Room ---
    type :: room_type
        logical :: active = .false.
        integer :: id = 0
        character(len=MAX_ROOM_NAME) :: name = ' '
        type(room_player_type) :: players(MAX_ROOM_PLAYERS)
        integer :: num_players = 0
        type(server_config) :: cfg
        logical :: in_game = .false.
    end type

    ! --- Embedded server limits ---
    integer, parameter :: EMB_MAX_CLIENTS  = 8
    integer, parameter :: EMB_RECV_BUF_SZ  = 4096
    integer, parameter :: EMB_CLIENT_LOBBY = 0
    integer, parameter :: EMB_CLIENT_ROOM  = 1
    integer, parameter :: EMB_CLIENT_GAME  = 2

    ! --- Embedded server client ---
    type :: emb_client_type
        logical :: active = .false.
        integer(c_int) :: fd = -1
        character(len=64) :: ip = ' '
        character(len=32) :: name = ' '
        integer :: state = 0
        integer :: room_idx = 0
        integer(c_int8_t) :: recv_buf(EMB_RECV_BUF_SZ)
        integer :: recv_pos = 0
    end type

    ! --- Embedded server state ---
    type :: emb_server_type
        logical :: active = .false.
        integer(c_int) :: listen_fd = -1
        type(emb_client_type) :: clients(EMB_MAX_CLIENTS)
        type(room_type) :: rooms(MAX_ROOMS)
        type(game_state) :: room_gs(MAX_ROOMS)
    end type

contains

    ! ==================================================================
    ! Return a default config
    ! ==================================================================
    subroutine server_config_defaults(cfg)
        type(server_config), intent(out) :: cfg
        cfg%maze_w      = 21
        cfg%maze_h      = 15
        cfg%min_dist    = 15
        cfg%item_counts = [3, 2, 2, 1, 2]
        cfg%speed_h     = 2
        cfg%speed_s     = 1
        cfg%vision_h    = 1
        cfg%vision_s    = 2
        cfg%reveal_h    = .false.
        cfg%reveal_s    = .true.
        cfg%num_keys    = 1
        cfg%turn_timer  = 0
        cfg%branch_pct  = 5
    end subroutine

    ! ==================================================================
    ! Start the server: create socket, bind, listen.
    ! Returns server_fd >= 0 on success, -1 on failure.
    ! ==================================================================
    subroutine server_start(port, server_fd, local_ip, error_msg)
        integer(c_int16_t), intent(in) :: port
        integer(c_int), intent(out) :: server_fd
        character(len=*), intent(out) :: local_ip
        character(len=*), intent(out) :: error_msg

        call net_start_server(port, server_fd, error_msg)
        local_ip = ' '
        if (server_fd >= 0) call net_get_local_ip(local_ip)
    end subroutine

    ! ==================================================================
    ! Stop the server: close server socket.
    ! ==================================================================
    subroutine server_stop(server_fd)
        integer(c_int), intent(inout) :: server_fd
        if (server_fd >= 0) then
            call net_close(server_fd)
            server_fd = -1
        end if
    end subroutine

    ! ==================================================================
    ! Non-blocking accept of one client connection.
    ! ==================================================================
    subroutine server_try_accept(server_fd, client_fd, client_ip, got_client)
        integer(c_int), intent(in)  :: server_fd
        integer(c_int), intent(out) :: client_fd
        character(len=*), intent(out) :: client_ip
        logical, intent(out) :: got_client

        call net_try_accept_nonblocking(server_fd, client_fd, client_ip, got_client)
    end subroutine

    ! ==================================================================
    ! Initialize a game using the given config.
    ! Ensures maze dims are odd for proper generation.
    ! ==================================================================
    subroutine server_init_game(gs, cfg, num_players, player_teams)
        type(game_state), intent(out) :: gs
        type(server_config), intent(inout) :: cfg
        integer, intent(in) :: num_players
        integer, intent(in) :: player_teams(MAX_PLAYERS)

        ! Ensure maze dims are odd
        if (mod(cfg%maze_w, 2) == 0) cfg%maze_w = cfg%maze_w + 1
        if (mod(cfg%maze_h, 2) == 0) cfg%maze_h = cfg%maze_h + 1

        call game_init(gs, cfg%maze_w, cfg%maze_h, num_players, player_teams, &
                       cfg%num_keys, cfg%item_counts, cfg%min_dist, &
                       cfg%speed_h, cfg%speed_s, cfg%vision_h, cfg%vision_s, &
                       cfg%reveal_h, cfg%reveal_s, real(cfg%branch_pct) / 100.0)
    end subroutine

    ! ==================================================================
    ! Serialize the game state into a byte buffer (init packet).
    ! player_index: which player index (1..N) the receiver is.
    ! nbytes: actual number of bytes written.
    ! ==================================================================
    subroutine server_build_init_packet(gs, player_index, buf, nbytes)
        type(game_state), intent(in) :: gs
        integer, intent(in) :: player_index
        integer(c_int8_t), intent(out) :: buf(:)
        integer, intent(out) :: nbytes

        integer :: k, ix, iy, i

        k = 0
        ! Header: maze_w, maze_h, num_items, num_players, num_keys
        k = k+1; buf(k) = int(gs%maze%w, c_int8_t)
        k = k+1; buf(k) = int(gs%maze%h, c_int8_t)
        k = k+1; buf(k) = int(gs%num_items, c_int8_t)
        k = k+1; buf(k) = int(gs%num_players, c_int8_t)
        k = k+1; buf(k) = int(gs%num_keys, c_int8_t)

        ! Per-player stats: team, speed, vision, reveal
        do i = 1, gs%num_players
            k = k+1; buf(k) = int(gs%players(i)%team, c_int8_t)
            k = k+1; buf(k) = int(gs%players(i)%speed, c_int8_t)
            k = k+1; buf(k) = int(gs%players(i)%vision_radius, c_int8_t)
            k = k+1
            if (gs%players(i)%reveal) then; buf(k) = 1_c_int8_t
            else; buf(k) = 0_c_int8_t; end if
        end do

        ! My player index
        k = k+1; buf(k) = int(player_index, c_int8_t)

        ! Maze cells (row-major)
        do iy = 1, gs%maze%h
            do ix = 1, gs%maze%w
                k = k+1; buf(k) = int(gs%maze%cells(ix, iy), c_int8_t)
            end do
        end do

        ! Ground items (x, y, type)
        do i = 1, gs%num_items
            k = k+1; buf(k) = int(gs%items(i)%x, c_int8_t)
            k = k+1; buf(k) = int(gs%items(i)%y, c_int8_t)
            k = k+1; buf(k) = int(gs%items(i)%itype, c_int8_t)
        end do

        ! Player positions: x, y per player
        do i = 1, gs%num_players
            k = k+1; buf(k) = int(gs%players(i)%x, c_int8_t)
            k = k+1; buf(k) = int(gs%players(i)%y, c_int8_t)
        end do

        ! Key positions
        do i = 1, gs%num_keys
            k = k+1; buf(k) = int(gs%key_x(i), c_int8_t)
            k = k+1; buf(k) = int(gs%key_y(i), c_int8_t)
        end do

        nbytes = k
    end subroutine

    ! ==================================================================
    ! Send the init packet to a specific socket fd.
    ! ==================================================================
    subroutine server_send_init_to_fd(fd, gs, player_index, status)
        integer(c_int), intent(in) :: fd
        type(game_state), intent(in) :: gs
        integer, intent(in) :: player_index
        integer(c_int), intent(out) :: status

        integer(c_int8_t), target :: buf(4096)
        integer :: nbytes

        call server_build_init_packet(gs, player_index, buf, nbytes)
        call net_send_bytes(fd, buf, int(nbytes, c_int), status)
    end subroutine

    ! ==================================================================
    ! Parse a received init packet into a game_state.
    ! Returns player_index: the receiver's 1-based player index.
    ! Returns needed=0 on success, >0 if more bytes needed.
    ! ==================================================================
    subroutine server_parse_init_packet(recv_buf, buflen, gs, player_index, needed)
        integer(c_int8_t), intent(in) :: recv_buf(:)
        integer, intent(in) :: buflen
        type(game_state), intent(out) :: gs
        integer, intent(out) :: player_index
        integer, intent(out) :: needed

        integer :: k, mw, mh, ni, np, nk, ix, iy, i

        needed = 0
        if (buflen < 5) then
            needed = 5
            return
        end if

        mw = int(recv_buf(1))
        mh = int(recv_buf(2))
        ni = int(recv_buf(3))
        np = int(recv_buf(4))
        nk = int(recv_buf(5))

        ! Total: header(5) + per-player(np*4) + my_idx(1) + maze(mw*mh)
        !        + items(ni*3) + positions(np*2) + keys(nk*2)
        needed = 5 + np*4 + 1 + mw*mh + ni*3 + np*2 + nk*2
        if (buflen < needed) return

        ! Parse per-player stats
        k = 5
        gs%num_players = np
        do i = 1, np
            k = k+1; gs%players(i)%team          = int(recv_buf(k))
            k = k+1; gs%players(i)%speed         = int(recv_buf(k))
            k = k+1; gs%players(i)%vision_radius = int(recv_buf(k))
            k = k+1; gs%players(i)%reveal        = (recv_buf(k) /= 0)
            gs%players(i)%actions_left = gs%players(i)%speed
            gs%players(i)%num_items    = 0
            gs%players(i)%inventory    = ITEM_NONE
            gs%players(i)%captured     = .false.
            gs%players(i)%illuminate   = .false.
            gs%players(i)%class_id     = 0
        end do

        ! My player index
        k = k+1; player_index = int(recv_buf(k))

        ! Parse maze
        gs%maze%w = mw
        gs%maze%h = mh
        gs%maze%cells = 0
        do iy = 1, mh
            do ix = 1, mw
                k = k+1; gs%maze%cells(ix, iy) = int(recv_buf(k))
            end do
        end do

        ! Parse ground items
        gs%num_items = ni
        do i = 1, ni
            k = k+1; gs%items(i)%x     = int(recv_buf(k))
            k = k+1; gs%items(i)%y     = int(recv_buf(k))
            k = k+1; gs%items(i)%itype = int(recv_buf(k))
            gs%items(i)%active = .true.
        end do

        ! Parse player positions
        do i = 1, np
            k = k+1; gs%players(i)%x = int(recv_buf(k))
            k = k+1; gs%players(i)%y = int(recv_buf(k))
        end do

        ! Parse key positions
        gs%num_keys   = nk
        gs%keys_found = 0
        do i = 1, nk
            k = k+1; gs%key_x(i) = int(recv_buf(k))
            k = k+1; gs%key_y(i) = int(recv_buf(k))
            gs%key_active(i) = .true.
        end do

        ! Initialize state
        gs%visible     = .false.
        gs%visited     = .false.
        gs%item_seen   = .false.
        gs%last_seen_x = 0
        gs%last_seen_y = 0
        gs%has_seen    = .false.
        gs%current_player = 1
        gs%turn_number    = 1
        gs%game_over      = .false.
        gs%seekers_won    = .false.
        gs%input_state    = INPUT_MOVE
        gs%selected_slot  = 0
        gs%initialized    = .true.

        call game_compute_visibility(gs)
        needed = 0
    end subroutine

    ! ==================================================================
    ! Restart: re-init game and send new init to all players in room.
    ! ==================================================================
    subroutine server_restart_game(room, gs, cfg, player_teams, status)
        type(room_type), intent(in) :: room
        type(game_state), intent(out) :: gs
        type(server_config), intent(inout) :: cfg
        integer, intent(in) :: player_teams(MAX_PLAYERS)
        integer(c_int), intent(out) :: status
        integer :: i

        ! Send restart signal (action_type=9) to all
        do i = 1, room%num_players
            call server_send_action(room%players(i)%fd, 0, 9, 0)
        end do

        ! Generate new game
        call server_init_game(gs, cfg, room%num_players, player_teams)

        ! Send init to each player
        do i = 1, room%num_players
            call server_send_init_to_fd(room%players(i)%fd, gs, i, status)
        end do
    end subroutine

    ! ==================================================================
    ! Send a 3-byte action message: [pidx, action_type, param].
    ! ==================================================================
    subroutine server_send_action(fd, pidx, action_type, param)
        integer(c_int), intent(in) :: fd
        integer, intent(in) :: pidx, action_type, param

        integer(c_int8_t), target :: msg(3)
        integer(c_int) :: st

        msg(1) = int(pidx, c_int8_t)
        msg(2) = int(action_type, c_int8_t)
        msg(3) = int(param, c_int8_t)
        call net_send_bytes(fd, msg, 3_c_int, st)
    end subroutine

    ! ==================================================================
    ! Try to receive a 3-byte action (non-blocking).
    ! Returns got=.true. if an action was received.
    ! ==================================================================
    subroutine server_try_recv_action(fd, pidx, action_type, param, got, peer_closed)
        integer(c_int), intent(in) :: fd
        integer, intent(out) :: pidx, action_type, param
        logical, intent(out) :: got
        logical, intent(out), optional :: peer_closed

        integer(c_int8_t), target :: msg(3)
        integer(c_int) :: nbytes

        got = .false.
        if (present(peer_closed)) peer_closed = .false.
        call net_try_recv_bytes(fd, msg, 3_c_int, nbytes)
        if (nbytes == -1) then
            if (present(peer_closed)) peer_closed = .true.
            return
        end if
        if (nbytes >= 3) then
            pidx = int(msg(1))
            action_type = int(msg(2))
            param = int(msg(3))
            got = .true.
        end if
    end subroutine

    ! ==================================================================
    ! Room management
    ! ==================================================================

    ! Initialize all rooms to inactive
    subroutine room_init(rooms)
        type(room_type), intent(out) :: rooms(MAX_ROOMS)
        integer :: i
        do i = 1, MAX_ROOMS
            rooms(i)%active = .false.
            rooms(i)%id = i
            rooms(i)%num_players = 0
            rooms(i)%in_game = .false.
        end do
    end subroutine

    ! Create a new room. Returns room index (1..MAX_ROOMS) or 0 on failure.
    function room_create(rooms, name, host_fd, host_ip, host_name) result(idx)
        type(room_type), intent(inout) :: rooms(MAX_ROOMS)
        character(len=*), intent(in) :: name, host_ip, host_name
        integer(c_int), intent(in) :: host_fd
        integer :: idx, i

        idx = 0
        do i = 1, MAX_ROOMS
            if (.not. rooms(i)%active) then
                rooms(i)%active = .true.
                rooms(i)%name = name
                rooms(i)%num_players = 1
                rooms(i)%players(1)%fd = host_fd
                rooms(i)%players(1)%ip = host_ip
                if (len_trim(host_name) == 0 .or. trim(host_name) == 'Player') then
                    rooms(i)%players(1)%name = 'Player 1'
                else
                    rooms(i)%players(1)%name = host_name
                end if
                rooms(i)%players(1)%is_host = .true.
                rooms(i)%in_game = .false.
                call server_config_defaults(rooms(i)%cfg)
                idx = i
                return
            end if
        end do
    end function

    ! Join an existing room. Returns .true. on success.
    function room_join(rooms, room_idx, fd, ip, player_name) result(ok)
        type(room_type), intent(inout) :: rooms(MAX_ROOMS)
        integer, intent(in) :: room_idx
        integer(c_int), intent(in) :: fd
        character(len=*), intent(in) :: ip, player_name
        logical :: ok
        integer :: n

        ok = .false.
        if (room_idx < 1 .or. room_idx > MAX_ROOMS) return
        if (.not. rooms(room_idx)%active) return
        if (rooms(room_idx)%in_game) return
        n = rooms(room_idx)%num_players
        if (n >= MAX_ROOM_PLAYERS) return

        n = n + 1
        rooms(room_idx)%num_players = n
        rooms(room_idx)%players(n)%fd = fd
        rooms(room_idx)%players(n)%ip = ip
        if (len_trim(player_name) == 0 .or. trim(player_name) == 'Player') then
            write(rooms(room_idx)%players(n)%name, '(A,I0)') 'Player ', n
        else
            rooms(room_idx)%players(n)%name = player_name
        end if
        rooms(room_idx)%players(n)%is_host = .false.
        ok = .true.
    end function

    ! Leave a room. Closes room if empty.
    subroutine room_leave(rooms, room_idx, fd)
        type(room_type), intent(inout) :: rooms(MAX_ROOMS)
        integer, intent(in) :: room_idx
        integer(c_int), intent(in) :: fd
        integer :: i, j, n
        logical :: was_host

        if (room_idx < 1 .or. room_idx > MAX_ROOMS) return
        if (.not. rooms(room_idx)%active) return

        n = rooms(room_idx)%num_players
        was_host = .false.

        ! Find and remove the player
        do i = 1, n
            if (rooms(room_idx)%players(i)%fd == fd) then
                was_host = rooms(room_idx)%players(i)%is_host
                ! Shift remaining players down
                do j = i, n-1
                    rooms(room_idx)%players(j) = rooms(room_idx)%players(j+1)
                end do
                rooms(room_idx)%players(n)%fd = -1
                rooms(room_idx)%players(n)%name = ' '
                rooms(room_idx)%players(n)%is_host = .false.
                rooms(room_idx)%num_players = n - 1
                exit
            end if
        end do

        ! If empty, deactivate room
        if (rooms(room_idx)%num_players == 0) then
            rooms(room_idx)%active = .false.
            return
        end if

        ! If host left, promote first remaining player
        if (was_host) then
            rooms(room_idx)%players(1)%is_host = .true.
        end if
    end subroutine

    ! Find which room a client fd is in. Returns room_idx (0 if not in any).
    function room_find(rooms, fd) result(idx)
        type(room_type), intent(in) :: rooms(MAX_ROOMS)
        integer(c_int), intent(in) :: fd
        integer :: idx, i, j

        idx = 0
        do i = 1, MAX_ROOMS
            if (.not. rooms(i)%active) cycle
            do j = 1, rooms(i)%num_players
                if (rooms(i)%players(j)%fd == fd) then
                    idx = i
                    return
                end if
            end do
        end do
    end function

    ! ==================================================================
    ! Serialize server_config to bytes (fixed 16 bytes)
    ! ==================================================================
    subroutine server_config_to_bytes(cfg, buf, nbytes)
        type(server_config), intent(in) :: cfg
        integer(c_int8_t), intent(out) :: buf(:)
        integer, intent(out) :: nbytes
        integer :: k

        k = 0
        k=k+1; buf(k) = int(cfg%maze_w, c_int8_t)
        k=k+1; buf(k) = int(cfg%maze_h, c_int8_t)
        k=k+1; buf(k) = int(cfg%min_dist, c_int8_t)
        k=k+1; buf(k) = int(cfg%item_counts(1), c_int8_t)
        k=k+1; buf(k) = int(cfg%item_counts(2), c_int8_t)
        k=k+1; buf(k) = int(cfg%item_counts(3), c_int8_t)
        k=k+1; buf(k) = int(cfg%item_counts(4), c_int8_t)
        k=k+1; buf(k) = int(cfg%item_counts(5), c_int8_t)
        k=k+1; buf(k) = int(cfg%speed_h, c_int8_t)
        k=k+1; buf(k) = int(cfg%speed_s, c_int8_t)
        k=k+1; buf(k) = int(cfg%vision_h, c_int8_t)
        k=k+1; buf(k) = int(cfg%vision_s, c_int8_t)
        k=k+1; if (cfg%reveal_h) then; buf(k)=1; else; buf(k)=0; end if
        k=k+1; if (cfg%reveal_s) then; buf(k)=1; else; buf(k)=0; end if
        k=k+1; buf(k) = int(cfg%num_keys, c_int8_t)
        k=k+1; buf(k) = int(cfg%turn_timer, c_int8_t)
        k=k+1; buf(k) = int(cfg%branch_pct, c_int8_t)
        nbytes = k  ! 17
    end subroutine

    ! ==================================================================
    ! Deserialize server_config from bytes
    ! ==================================================================
    subroutine server_config_from_bytes(buf, cfg)
        integer(c_int8_t), intent(in) :: buf(:)
        type(server_config), intent(out) :: cfg
        integer :: k

        k = 0
        k=k+1; cfg%maze_w      = int(buf(k))
        k=k+1; cfg%maze_h      = int(buf(k))
        k=k+1; cfg%min_dist    = int(buf(k))
        k=k+1; cfg%item_counts(1) = int(buf(k))
        k=k+1; cfg%item_counts(2) = int(buf(k))
        k=k+1; cfg%item_counts(3) = int(buf(k))
        k=k+1; cfg%item_counts(4) = int(buf(k))
        k=k+1; cfg%item_counts(5) = int(buf(k))
        k=k+1; cfg%speed_h     = int(buf(k))
        k=k+1; cfg%speed_s     = int(buf(k))
        k=k+1; cfg%vision_h    = int(buf(k))
        k=k+1; cfg%vision_s    = int(buf(k))
        k=k+1; cfg%reveal_h    = (buf(k) /= 0)
        k=k+1; cfg%reveal_s    = (buf(k) /= 0)
        k=k+1; cfg%num_keys    = int(buf(k))
        k=k+1; cfg%turn_timer  = int(buf(k))
        k=k+1; cfg%branch_pct  = int(buf(k))
    end subroutine

    ! ==================================================================
    ! Build room list payload for MSG_ROOM_LIST
    ! Format: [num_rooms:1] then for each room:
    !   [room_id:1][name_len:1][name:N][num_players:1][in_game:1]
    ! ==================================================================
    subroutine room_build_list_payload(rooms, buf, nbytes)
        type(room_type), intent(in) :: rooms(MAX_ROOMS)
        integer(c_int8_t), intent(out) :: buf(:)
        integer, intent(out) :: nbytes
        integer :: i, k, nlen, nactive

        k = 0
        ! Count active rooms first
        nactive = 0
        do i = 1, MAX_ROOMS
            if (rooms(i)%active) nactive = nactive + 1
        end do

        k=k+1; buf(k) = int(nactive, c_int8_t)

        do i = 1, MAX_ROOMS
            if (.not. rooms(i)%active) cycle
            nlen = len_trim(rooms(i)%name)
            k=k+1; buf(k) = int(rooms(i)%id, c_int8_t)
            k=k+1; buf(k) = int(nlen, c_int8_t)
            buf(k+1:k+nlen) = transfer(rooms(i)%name(1:nlen), buf(1:nlen))
            k = k + nlen
            k=k+1; buf(k) = int(rooms(i)%num_players, c_int8_t)
            k=k+1; if (rooms(i)%in_game) then; buf(k)=1; else; buf(k)=0; end if
        end do
        nbytes = k
    end subroutine

    ! ==================================================================
    ! Parse room list payload (client side)
    ! Returns arrays of room info. max_rooms_out = size of output arrays.
    ! ==================================================================
    subroutine room_parse_list_payload(buf, buflen, &
            room_ids, room_names, room_nplayers, room_ingame, num_rooms_out)
        integer(c_int8_t), intent(in) :: buf(:)
        integer, intent(in) :: buflen
        integer, intent(out) :: room_ids(:)
        character(len=MAX_ROOM_NAME), intent(out) :: room_names(:)
        integer, intent(out) :: room_nplayers(:)
        logical, intent(out) :: room_ingame(:)
        integer, intent(out) :: num_rooms_out

        integer :: k, i, nlen, max_out

        max_out = size(room_ids)
        num_rooms_out = 0
        if (buflen < 1) return

        k = 1
        num_rooms_out = min(int(buf(k)), max_out)

        do i = 1, num_rooms_out
            k=k+1; room_ids(i) = int(buf(k))
            k=k+1; nlen = int(buf(k))
            room_names(i) = ' '
            if (nlen > 0 .and. nlen <= MAX_ROOM_NAME) then
                room_names(i)(1:nlen) = transfer(buf(k+1:k+nlen), room_names(i)(1:nlen))
            end if
            k = k + nlen
            k=k+1; room_nplayers(i) = int(buf(k))
            k=k+1; room_ingame(i) = (buf(k) /= 0)
        end do
    end subroutine

    ! ==================================================================
    ! Build room update payload for MSG_ROOM_UPDATE
    ! Format: [config:17][num_players:1] then for each player:
    !   [name_len:1][name:N][is_host:1][team:1]
    ! ==================================================================
    subroutine room_build_update_payload(room, buf, nbytes)
        type(room_type), intent(in) :: room
        integer(c_int8_t), intent(out) :: buf(:)
        integer, intent(out) :: nbytes
        integer :: k, i, nlen, cfg_bytes

        k = 0
        ! Config bytes
        call server_config_to_bytes(room%cfg, buf(k+1:), cfg_bytes)
        k = k + cfg_bytes

        ! Player list
        k=k+1; buf(k) = int(room%num_players, c_int8_t)
        do i = 1, room%num_players
            nlen = len_trim(room%players(i)%name)
            k=k+1; buf(k) = int(nlen, c_int8_t)
            if (nlen > 0) then
                buf(k+1:k+nlen) = transfer(room%players(i)%name(1:nlen), buf(1:nlen))
                k = k + nlen
            end if
            k=k+1; if (room%players(i)%is_host) then; buf(k)=1; else; buf(k)=0; end if
            k=k+1; buf(k) = int(room%players(i)%team, c_int8_t)
        end do
        nbytes = k
    end subroutine

    ! ==================================================================
    ! Parse room update payload (client side)
    ! ==================================================================
    subroutine room_parse_update_payload(buf, buflen, cfg, &
            player_names, player_is_host, player_teams, num_players_out)
        integer(c_int8_t), intent(in) :: buf(:)
        integer, intent(in) :: buflen
        type(server_config), intent(out) :: cfg
        character(len=32), intent(out) :: player_names(:)
        logical, intent(out) :: player_is_host(:)
        integer, intent(out) :: player_teams(:)
        integer, intent(out) :: num_players_out

        integer :: k, i, nlen, max_out

        max_out = size(player_names)
        num_players_out = 0

        if (buflen < SERVER_CONFIG_BYTES + 1) return

        call server_config_from_bytes(buf(1:), cfg)
        k = SERVER_CONFIG_BYTES

        k=k+1; num_players_out = min(int(buf(k)), max_out)
        do i = 1, num_players_out
            k=k+1; nlen = int(buf(k))
            player_names(i) = ' '
            if (nlen > 0 .and. nlen <= 32) then
                player_names(i)(1:nlen) = transfer(buf(k+1:k+nlen), player_names(i)(1:nlen))
                k = k + nlen
            end if
            k=k+1; player_is_host(i) = (buf(k) /= 0)
            k=k+1; player_teams(i) = int(buf(k))
        end do
    end subroutine

    ! ==================================================================
    ! Embedded server: initialize
    ! ==================================================================
    subroutine emb_server_init(srv, listen_fd)
        type(emb_server_type), intent(out) :: srv
        integer(c_int), intent(in) :: listen_fd
        integer :: i

        srv%active = .true.
        srv%listen_fd = listen_fd
        call room_init(srv%rooms)
        do i = 1, EMB_MAX_CLIENTS
            srv%clients(i)%active = .false.
            srv%clients(i)%fd = -1
            srv%clients(i)%recv_pos = 0
        end do
    end subroutine

    ! ==================================================================
    ! Embedded server: one tick (accept + process all clients)
    ! ==================================================================
    subroutine emb_server_tick(srv)
        type(emb_server_type), intent(inout) :: srv
        integer :: i

        if (.not. srv%active) return

        ! Accept new connections
        call emb_try_accept(srv)

        ! Process each client
        do i = 1, EMB_MAX_CLIENTS
            if (.not. srv%clients(i)%active) cycle
            select case (srv%clients(i)%state)
                case (EMB_CLIENT_LOBBY, EMB_CLIENT_ROOM)
                    call emb_process_lobby_client(srv, i)
                case (EMB_CLIENT_GAME)
                    call emb_process_game_client(srv, i)
            end select
        end do
    end subroutine

    ! ==================================================================
    ! Embedded server: shutdown and clean up
    ! ==================================================================
    subroutine emb_server_shutdown(srv)
        type(emb_server_type), intent(inout) :: srv
        integer :: i

        do i = 1, EMB_MAX_CLIENTS
            if (srv%clients(i)%active .and. srv%clients(i)%fd >= 0) then
                call net_close(srv%clients(i)%fd)
            end if
            srv%clients(i)%active = .false.
            srv%clients(i)%fd = -1
        end do

        if (srv%listen_fd >= 0) then
            call net_close(srv%listen_fd)
            srv%listen_fd = -1
        end if

        srv%active = .false.
    end subroutine

    ! ==================================================================
    ! Embedded server: accept a new client
    ! ==================================================================
    subroutine emb_try_accept(srv)
        type(emb_server_type), intent(inout) :: srv
        integer(c_int) :: cli_fd
        character(len=256) :: cli_ip
        logical :: got_client
        integer :: i

        call server_try_accept(srv%listen_fd, cli_fd, cli_ip, got_client)
        if (.not. got_client) return

        do i = 1, EMB_MAX_CLIENTS
            if (.not. srv%clients(i)%active) then
                srv%clients(i)%active = .true.
                srv%clients(i)%fd = cli_fd
                srv%clients(i)%ip = trim(cli_ip)
                srv%clients(i)%name = ' '
                srv%clients(i)%state = EMB_CLIENT_LOBBY
                srv%clients(i)%room_idx = 0
                srv%clients(i)%recv_pos = 0
                return
            end if
        end do

        ! No free slot
        call net_close(cli_fd)
    end subroutine

    ! ==================================================================
    ! Embedded server: process a lobby/room client
    ! ==================================================================
    subroutine emb_process_lobby_client(srv, ci)
        type(emb_server_type), intent(inout) :: srv
        integer, intent(in) :: ci

        integer(c_int8_t) :: msg_type
        integer(c_int8_t) :: payload(1024)
        integer :: payload_len
        logical :: got, closed

        call net_try_recv_msg(srv%clients(ci)%fd, srv%clients(ci)%recv_buf, &
                              EMB_RECV_BUF_SZ, srv%clients(ci)%recv_pos, &
                              msg_type, payload, 1024, payload_len, got, closed)

        if (closed) then
            call emb_disconnect_client(srv, ci)
            return
        end if
        if (.not. got) return

        select case (msg_type)
            case (MSG_LIST_ROOMS)
                call emb_handle_list_rooms(srv, ci)
            case (MSG_CREATE_ROOM)
                call emb_handle_create_room(srv, ci, payload, payload_len)
            case (MSG_JOIN_ROOM)
                call emb_handle_join_room(srv, ci, payload, payload_len)
            case (MSG_LEAVE_ROOM)
                call emb_handle_leave_room(srv, ci)
            case (MSG_CONFIG_CHANGE)
                call emb_handle_config_change(srv, ci, payload, payload_len)
            case (MSG_TEAM_CHANGE)
                call emb_handle_team_change(srv, ci, payload, payload_len)
            case (MSG_START_GAME)
                call emb_handle_start_game(srv, ci)
            case default
                ! Discard stale data (e.g. game-protocol bytes after transition)
                srv%clients(ci)%recv_pos = 0
        end select
    end subroutine

    ! ==================================================================
    ! Embedded server: handle MSG_LIST_ROOMS
    ! ==================================================================
    subroutine emb_handle_list_rooms(srv, ci)
        type(emb_server_type), intent(inout) :: srv
        integer, intent(in) :: ci
        integer(c_int8_t), target :: buf(1024)
        integer :: nbytes
        integer(c_int) :: st

        call room_build_list_payload(srv%rooms, buf, nbytes)
        call net_send_msg(srv%clients(ci)%fd, MSG_ROOM_LIST, buf, nbytes, st)
    end subroutine

    ! ==================================================================
    ! Embedded server: handle MSG_CREATE_ROOM
    ! ==================================================================
    subroutine emb_handle_create_room(srv, ci, payload, plen)
        type(emb_server_type), intent(inout) :: srv
        integer, intent(in) :: ci, plen
        integer(c_int8_t), intent(in) :: payload(:)
        integer :: nlen, idx
        character(len=MAX_ROOM_NAME) :: rname
        character(len=32) :: pname

        if (plen < 2) return

        nlen = int(payload(1))
        if (nlen <= 0 .or. nlen > MAX_ROOM_NAME .or. plen < 1 + nlen) return
        rname = ' '
        rname(1:nlen) = transfer(payload(2:1+nlen), rname(1:nlen))

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
            pname = trim(srv%clients(ci)%ip)
        end if
        srv%clients(ci)%name = pname

        idx = room_create(srv%rooms, trim(rname), srv%clients(ci)%fd, &
                          trim(srv%clients(ci)%ip), trim(pname))
        if (idx > 0) then
            srv%clients(ci)%state = EMB_CLIENT_ROOM
            srv%clients(ci)%room_idx = idx
            call emb_send_room_update(srv, idx)
        end if
    end subroutine

    ! ==================================================================
    ! Embedded server: handle MSG_JOIN_ROOM
    ! ==================================================================
    subroutine emb_handle_join_room(srv, ci, payload, plen)
        type(emb_server_type), intent(inout) :: srv
        integer, intent(in) :: ci, plen
        integer(c_int8_t), intent(in) :: payload(:)
        integer :: rid, nlen
        character(len=32) :: pname
        logical :: ok

        if (plen < 1) return
        rid = int(payload(1))

        pname = trim(srv%clients(ci)%ip)
        if (plen >= 3) then
            nlen = int(payload(2))
            if (nlen > 0 .and. plen >= 2 + nlen) then
                pname = ' '
                pname(1:nlen) = transfer(payload(3:2+nlen), pname(1:nlen))
            end if
        end if
        srv%clients(ci)%name = pname

        ok = room_join(srv%rooms, rid, srv%clients(ci)%fd, &
                       trim(srv%clients(ci)%ip), trim(pname))
        if (ok) then
            srv%clients(ci)%state = EMB_CLIENT_ROOM
            srv%clients(ci)%room_idx = rid
            call emb_send_room_update(srv, rid)
        end if
    end subroutine

    ! ==================================================================
    ! Embedded server: handle MSG_LEAVE_ROOM
    ! ==================================================================
    subroutine emb_handle_leave_room(srv, ci)
        type(emb_server_type), intent(inout) :: srv
        integer, intent(in) :: ci
        integer :: ridx

        ridx = srv%clients(ci)%room_idx
        if (ridx > 0) then
            call room_leave(srv%rooms, ridx, srv%clients(ci)%fd)
            if (srv%rooms(ridx)%active) call emb_send_room_update(srv, ridx)
        end if
        srv%clients(ci)%state = EMB_CLIENT_LOBBY
        srv%clients(ci)%room_idx = 0
    end subroutine

    ! ==================================================================
    ! Embedded server: handle MSG_CONFIG_CHANGE
    ! ==================================================================
    subroutine emb_handle_config_change(srv, ci, payload, plen)
        type(emb_server_type), intent(inout) :: srv
        integer, intent(in) :: ci, plen
        integer(c_int8_t), intent(in) :: payload(:)
        integer :: ridx

        if (plen < SERVER_CONFIG_BYTES) return
        ridx = srv%clients(ci)%room_idx
        if (ridx < 1 .or. ridx > MAX_ROOMS) return
        if (.not. srv%rooms(ridx)%active) return
        if (.not. emb_is_room_host(srv, ridx, srv%clients(ci)%fd)) return

        call server_config_from_bytes(payload, srv%rooms(ridx)%cfg)
        call emb_send_room_update(srv, ridx)
    end subroutine

    ! ==================================================================
    ! Embedded server: handle MSG_TEAM_CHANGE
    ! Payload: [player_slot (1-byte), new_team (1-byte)]
    ! ==================================================================
    subroutine emb_handle_team_change(srv, ci, payload, plen)
        type(emb_server_type), intent(inout) :: srv
        integer, intent(in) :: ci, plen
        integer(c_int8_t), intent(in) :: payload(:)
        integer :: ridx, slot, new_team

        if (plen < 2) return
        ridx = srv%clients(ci)%room_idx
        if (ridx < 1 .or. ridx > MAX_ROOMS) return
        if (.not. srv%rooms(ridx)%active) return
        if (.not. emb_is_room_host(srv, ridx, srv%clients(ci)%fd)) return

        slot = int(payload(1))
        new_team = int(payload(2))
        if (slot < 1 .or. slot > srv%rooms(ridx)%num_players) return
        if (new_team /= TEAM_ESCAPE .and. new_team /= TEAM_LABYRINTH) return

        srv%rooms(ridx)%players(slot)%team = new_team
        call emb_send_room_update(srv, ridx)
    end subroutine

    ! ==================================================================
    ! Embedded server: handle MSG_START_GAME
    ! ==================================================================
    subroutine emb_handle_start_game(srv, ci)
        type(emb_server_type), intent(inout) :: srv
        integer, intent(in) :: ci
        integer :: ridx, j, k
        integer(c_int) :: st
        integer(c_int8_t), target :: dummy(1)
        integer :: player_teams(MAX_PLAYERS)

        ridx = srv%clients(ci)%room_idx
        if (ridx < 1 .or. ridx > MAX_ROOMS) return
        if (.not. srv%rooms(ridx)%active) return
        if (.not. emb_is_room_host(srv, ridx, srv%clients(ci)%fd)) return
        if (srv%rooms(ridx)%num_players < 2) return

        srv%rooms(ridx)%in_game = .true.

        ! Build player_teams from room
        do j = 1, srv%rooms(ridx)%num_players
            player_teams(j) = srv%rooms(ridx)%players(j)%team
        end do

        call server_init_game(srv%room_gs(ridx), srv%rooms(ridx)%cfg, &
                              srv%rooms(ridx)%num_players, player_teams)

        ! Send MSG_START_GAME to all players
        do j = 1, srv%rooms(ridx)%num_players
            call net_send_msg(srv%rooms(ridx)%players(j)%fd, MSG_START_GAME, dummy, 0, st)
        end do

        ! Send game init to each player with their index
        do j = 1, srv%rooms(ridx)%num_players
            call server_send_init_to_fd(srv%rooms(ridx)%players(j)%fd, &
                                        srv%room_gs(ridx), j, st)
        end do

        ! Switch clients to game state
        do j = 1, srv%rooms(ridx)%num_players
            do k = 1, EMB_MAX_CLIENTS
                if (srv%clients(k)%active .and. &
                    srv%clients(k)%fd == srv%rooms(ridx)%players(j)%fd) then
                    srv%clients(k)%state = EMB_CLIENT_GAME
                    srv%clients(k)%recv_pos = 0
                    exit
                end if
            end do
        end do
    end subroutine

    ! ==================================================================
    ! Embedded server: process a game client (action relay)
    ! ==================================================================
    subroutine emb_process_game_client(srv, ci)
        type(emb_server_type), intent(inout) :: srv
        integer, intent(in) :: ci
        integer :: atype, param, ridx, pidx, recv_pidx, j
        integer(c_int) :: st
        logical :: got, moved, closed
        integer :: player_teams(MAX_PLAYERS)

        ridx = srv%clients(ci)%room_idx
        if (ridx < 1 .or. .not. srv%rooms(ridx)%active) return
        if (.not. srv%rooms(ridx)%in_game) return

        call server_try_recv_action(srv%clients(ci)%fd, recv_pidx, atype, param, got, closed)
        if (closed) then
            call emb_disconnect_client(srv, ci)
            return
        end if
        if (.not. got) return

        ! Find this client's player index
        pidx = emb_find_player_index(srv, ridx, srv%clients(ci)%fd)
        if (pidx < 1) return

        ! Quit: notify all others, end room game
        if (atype == 8) then
            call emb_broadcast_action(srv, ridx, srv%clients(ci)%fd, 0, 8, 0)
            call emb_end_room_game(srv, ridx)
            return
        end if

        ! Replay (host only): reinit game, send action 9 + init to all
        if (atype == 9) then
            if (.not. emb_is_room_host(srv, ridx, srv%clients(ci)%fd)) return
            do j = 1, srv%rooms(ridx)%num_players
                player_teams(j) = srv%rooms(ridx)%players(j)%team
            end do
            call server_init_game(srv%room_gs(ridx), srv%rooms(ridx)%cfg, &
                                  srv%rooms(ridx)%num_players, player_teams)
            do j = 1, srv%rooms(ridx)%num_players
                call server_send_action(srv%rooms(ridx)%players(j)%fd, 0, 9, 0)
                call server_send_init_to_fd(srv%rooms(ridx)%players(j)%fd, &
                                            srv%room_gs(ridx), j, st)
            end do
            return
        end if

        ! Back to room (host only): send action 10 to all, end game
        if (atype == 10) then
            if (.not. emb_is_room_host(srv, ridx, srv%clients(ci)%fd)) return
            do j = 1, srv%rooms(ridx)%num_players
                call server_send_action(srv%rooms(ridx)%players(j)%fd, 0, 10, 0)
            end do
            call emb_end_room_game(srv, ridx)
            call emb_send_room_update(srv, ridx)
            return
        end if

        ! Leave room from game (non-host): notify all others, end game, remove
        if (atype == 11) then
            call emb_broadcast_action(srv, ridx, srv%clients(ci)%fd, 0, 8, 0)
            call emb_end_room_game(srv, ridx)
            call room_leave(srv%rooms, ridx, srv%clients(ci)%fd)
            srv%clients(ci)%room_idx = 0
            srv%clients(ci)%state = EMB_CLIENT_LOBBY
            if (srv%rooms(ridx)%active) call emb_send_room_update(srv, ridx)
            return
        end if

        ! Don't process normal moves after game over
        if (srv%room_gs(ridx)%game_over) return

        ! Normal action: broadcast to all others and apply
        call emb_broadcast_action(srv, ridx, srv%clients(ci)%fd, pidx, atype, param)

        if (atype == 0) then
            moved = game_do_move(srv%room_gs(ridx), pidx, param)
        else if (atype == 7) then
            call game_do_pass(srv%room_gs(ridx))
        else
            call game_do_use_item(srv%room_gs(ridx), pidx, atype, param)
        end if

        call game_end_turn(srv%room_gs(ridx))
    end subroutine

    ! ==================================================================
    ! Embedded server: send room update to all players
    ! ==================================================================
    subroutine emb_send_room_update(srv, ridx)
        type(emb_server_type), intent(inout) :: srv
        integer, intent(in) :: ridx
        integer(c_int8_t), target :: buf(512)
        integer :: nbytes, j
        integer(c_int) :: st

        if (.not. srv%rooms(ridx)%active) return
        call room_build_update_payload(srv%rooms(ridx), buf, nbytes)
        do j = 1, srv%rooms(ridx)%num_players
            call net_send_msg(srv%rooms(ridx)%players(j)%fd, MSG_ROOM_UPDATE, buf, nbytes, st)
        end do
    end subroutine

    ! ==================================================================
    ! Embedded server: check if fd is room host
    ! ==================================================================
    function emb_is_room_host(srv, ridx, fd) result(is_host)
        type(emb_server_type), intent(in) :: srv
        integer, intent(in) :: ridx
        integer(c_int), intent(in) :: fd
        logical :: is_host
        integer :: j

        is_host = .false.
        do j = 1, srv%rooms(ridx)%num_players
            if (srv%rooms(ridx)%players(j)%fd == fd .and. &
                srv%rooms(ridx)%players(j)%is_host) then
                is_host = .true.
                return
            end if
        end do
    end function

    ! ==================================================================
    ! Embedded server: find player index by fd
    ! ==================================================================
    function emb_find_player_index(srv, ridx, fd) result(pidx)
        type(emb_server_type), intent(in) :: srv
        integer, intent(in) :: ridx
        integer(c_int), intent(in) :: fd
        integer :: pidx, j

        pidx = 0
        do j = 1, srv%rooms(ridx)%num_players
            if (srv%rooms(ridx)%players(j)%fd == fd) then
                pidx = j
                return
            end if
        end do
    end function

    ! ==================================================================
    ! Embedded server: broadcast action to all room players except sender
    ! ==================================================================
    subroutine emb_broadcast_action(srv, ridx, sender_fd, pidx, atype, param)
        type(emb_server_type), intent(in) :: srv
        integer, intent(in) :: ridx, pidx, atype, param
        integer(c_int), intent(in) :: sender_fd
        integer :: j

        do j = 1, srv%rooms(ridx)%num_players
            if (srv%rooms(ridx)%players(j)%fd /= sender_fd) then
                call server_send_action(srv%rooms(ridx)%players(j)%fd, pidx, atype, param)
            end if
        end do
    end subroutine

    ! ==================================================================
    ! Embedded server: end game in a room
    ! ==================================================================
    subroutine emb_end_room_game(srv, ridx)
        type(emb_server_type), intent(inout) :: srv
        integer, intent(in) :: ridx
        integer :: j, k

        srv%rooms(ridx)%in_game = .false.

        do j = 1, srv%rooms(ridx)%num_players
            do k = 1, EMB_MAX_CLIENTS
                if (srv%clients(k)%active .and. &
                    srv%clients(k)%fd == srv%rooms(ridx)%players(j)%fd) then
                    srv%clients(k)%state = EMB_CLIENT_ROOM
                    srv%clients(k)%recv_pos = 0
                    call net_drain_socket(srv%clients(k)%fd)
                    exit
                end if
            end do
        end do
    end subroutine

    ! ==================================================================
    ! Embedded server: disconnect a client
    ! ==================================================================
    subroutine emb_disconnect_client(srv, ci)
        type(emb_server_type), intent(inout) :: srv
        integer, intent(in) :: ci
        integer :: ridx

        ridx = srv%clients(ci)%room_idx
        if (ridx > 0 .and. srv%rooms(ridx)%active) then
            if (srv%rooms(ridx)%in_game .and. srv%clients(ci)%state == EMB_CLIENT_GAME) then
                ! Notify all other players this client quit
                call emb_broadcast_action(srv, ridx, srv%clients(ci)%fd, 0, 8, 0)
                call emb_end_room_game(srv, ridx)
            end if
            call room_leave(srv%rooms, ridx, srv%clients(ci)%fd)
            if (srv%rooms(ridx)%active) call emb_send_room_update(srv, ridx)
        end if

        if (srv%clients(ci)%fd >= 0) then
            call net_close(srv%clients(ci)%fd)
        end if

        srv%clients(ci)%active = .false.
        srv%clients(ci)%fd = -1
        srv%clients(ci)%state = EMB_CLIENT_LOBBY
        srv%clients(ci)%room_idx = 0
        srv%clients(ci)%recv_pos = 0
    end subroutine

end module server_mod
