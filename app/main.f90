! main.f90
!
! ForPlay - Asymmetric multiplayer maze game
!
! Pages:
!   PAGE_MENU  - Choose "Host" (host) or "Join" (join)
!   PAGE_HOST  - Shows IP/port, waits for client, launches game
!   PAGE_JOIN  - Enter IP/port, connect
!   PAGE_GAME  - The actual game

program forplay
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: stderr => error_unit
    use :: sdl2
    use :: sdl2_ttf
    use :: maze_mod
    use :: game_mod
    use :: network
    use :: server_mod
    implicit none

    ! --- Constants ---
    integer :: SCREEN_W      = 1024
    integer :: SCREEN_H      = 768
    integer, parameter :: TITLE_FONT_SZ = 48
    integer, parameter :: MAIN_FONT_SZ  = 24
    integer, parameter :: SML_FONT_SZ   = 14
    integer, parameter :: DEFAULT_PORT  = 12345
    integer, parameter :: HUD_H         = 90
    integer :: GAME_AREA_H   = 768 - 90  ! SCREEN_H - HUD_H
    integer, parameter :: DEFAULT_MAZE_W = 21
    integer, parameter :: DEFAULT_MAZE_H = 15
    integer, parameter :: DEFAULT_ITEM_DASH_COUNT       = 3
    integer, parameter :: DEFAULT_ITEM_VISION_COUNT     = 2
    integer, parameter :: DEFAULT_ITEM_ILLUMINATE_COUNT = 2
    integer, parameter :: DEFAULT_ITEM_SPEED_COUNT      = 1
    integer, parameter :: DEFAULT_ITEM_WALL_BREAK_COUNT  = 2
    integer, parameter :: DEFAULT_MIN_DIST  = 15
    integer, parameter :: DEFAULT_SPEED_H   = 2
    integer, parameter :: DEFAULT_SPEED_S   = 1
    integer, parameter :: DEFAULT_VISION_H  = 1
    integer, parameter :: DEFAULT_VISION_S  = 2
    integer, parameter :: WALL_THICK    = 2

    ! Page identifiers
    integer, parameter :: PAGE_MENU  = 0
    integer, parameter :: PAGE_HOST  = 1
    integer, parameter :: PAGE_JOIN  = 2
    integer, parameter :: PAGE_GAME  = 3
    integer, parameter :: PAGE_LOBBY = 4
    integer, parameter :: PAGE_ROOM  = 5

    ! Colors
    type(sdl_color) :: COL_WHITE, COL_BLACK, COL_GREEN, COL_RED
    type(sdl_color) :: COL_GRAY, COL_DARK, COL_YELLOW, COL_CYAN
    type(sdl_color) :: COL_ORANGE, COL_PURPLE, COL_DIM

    ! SDL handles
    type(c_ptr) :: main_window   = c_null_ptr
    type(c_ptr) :: main_renderer = c_null_ptr
    type(c_ptr) :: title_font    = c_null_ptr
    type(c_ptr) :: big_font      = c_null_ptr
    type(c_ptr) :: sml_font      = c_null_ptr
    integer :: sml_font_h, big_font_h  ! actual rendered line heights

    ! Application state
    integer :: current_page
    logical :: is_running
    type(sdl_event) :: event
    integer :: rc

    ! Network state
    integer(c_int) :: server_fd  = -1
    integer(c_int) :: client_fd  = -1
    integer(c_int) :: socket  = -1
    character(len=256) :: local_ip
    character(len=256) :: client_ip
    character(len=256) :: error_message
    logical :: client_connected
    logical :: waiting_for_client

    ! Join page state
    character(len=64)  :: input_ip
    character(len=16)  :: input_port
    integer :: active_field
    logical :: connecting
    integer(c_uint32_t) :: connect_start_tick  ! SDL tick when connect began
    integer, parameter :: CONNECT_TIMEOUT_MS = 5000  ! 5 second timeout

    ! Game state
    type(game_state) :: gs
    logical :: am_host           ! .true. = I am the network host
    integer :: player_index   ! 1-based index into gs%players
    integer(c_int) :: game_fd    ! socket for game communication
    logical :: game_ready        ! .true. once init data exchanged

    ! Game parameters (configurable by host before launch)
    type(server_config) :: cfg
    integer :: cfg_maze_w, cfg_maze_h, cfg_min_dist
    integer :: cfg_item_counts(NUM_ITEM_TYPES)
    integer :: cfg_speed_h, cfg_speed_s
    integer :: cfg_vision_h, cfg_vision_s
    logical :: cfg_reveal_h, cfg_reveal_s
    integer :: cfg_num_keys
    integer :: cfg_turn_timer
    integer :: cfg_branch_pct  ! branch probability as integer 0-100 (percent)

    ! Turn timer state
    integer :: game_turn_timer = 0         ! seconds per turn, 0=disabled (from config)
    integer(c_uint32_t) :: turn_start_tick = 0  ! SDL tick when current turn started

    ! Modal / end-game state
    logical :: show_quit_modal     ! confirmation dialog visible
    logical :: game_ended          ! game ended (quit or win), map revealed
    logical :: game_ended_by_quit  ! .true. if game ended by quit (not natural)
    character(len=128) :: endgame_msg  ! message to show on end screen

    ! Persistent buffer for receiving game init data
    integer(c_int8_t) :: recv_init_buf(2048)
    integer :: recv_init_pos

    ! Lobby message receive buffer
    integer(c_int8_t) :: lobby_recv_buf(4096)
    integer :: lobby_recv_pos

    ! Lobby state (room list)
    integer, parameter :: MAX_LOBBY_ROOMS = 10
    integer :: lobby_room_ids(MAX_LOBBY_ROOMS)
    character(len=32) :: lobby_room_names(MAX_LOBBY_ROOMS)
    integer :: lobby_room_nplayers(MAX_LOBBY_ROOMS)
    logical :: lobby_room_ingame(MAX_LOBBY_ROOMS)
    integer :: lobby_num_rooms
    logical :: lobby_creating       ! are we showing the create-room input?
    character(len=32) :: lobby_room_name_input
    character(len=32) :: lobby_player_name

    ! Room state (inside a room)
    integer :: room_id              ! which room we're in (server-side ID)
    type(server_config) :: room_cfg    ! current room config (from server)
    character(len=32) :: room_player_names(MAX_ROOM_PLAYERS)
    logical :: room_player_is_host(MAX_ROOM_PLAYERS)
    integer :: room_player_teams(MAX_ROOM_PLAYERS)
    integer :: room_num_players
    logical :: room_i_am_host
    integer :: room_teams_base_y = 0

    ! Embedded server (used when hosting via "Host a game")
    type(emb_server_type) :: emb_srv
    logical :: hosting_with_server = .false.

    ! SDL text input
    interface
        subroutine c_sdl_start_text_input() bind(C, name="SDL_StartTextInput")
        end subroutine
        subroutine c_sdl_stop_text_input() bind(C, name="SDL_StopTextInput")
        end subroutine
        function c_free_console() bind(C, name="FreeConsole") result(res)
            import :: c_int
            integer(c_int) :: res
        end function
    end interface

    ! Hide the console window on Windows
    rc = c_free_console()

    ! ---- Initialize colours ----
    COL_WHITE  = sdl_color(uint8(255), uint8(255), uint8(255), uint8(SDL_ALPHA_OPAQUE))
    COL_BLACK  = sdl_color(uint8(0),   uint8(0),   uint8(0),   uint8(SDL_ALPHA_OPAQUE))
    COL_GREEN  = sdl_color(uint8(0),   uint8(200), uint8(0),   uint8(SDL_ALPHA_OPAQUE))
    COL_RED    = sdl_color(uint8(220), uint8(50),  uint8(50),  uint8(SDL_ALPHA_OPAQUE))
    COL_GRAY   = sdl_color(uint8(180), uint8(180), uint8(180), uint8(SDL_ALPHA_OPAQUE))
    COL_DARK   = sdl_color(uint8(40),  uint8(40),  uint8(50),  uint8(SDL_ALPHA_OPAQUE))
    COL_YELLOW = sdl_color(uint8(255), uint8(220), uint8(50),  uint8(SDL_ALPHA_OPAQUE))
    COL_CYAN   = sdl_color(uint8(100), uint8(200), uint8(255), uint8(SDL_ALPHA_OPAQUE))
    COL_ORANGE = sdl_color(uint8(255), uint8(160), uint8(50),  uint8(SDL_ALPHA_OPAQUE))
    COL_PURPLE = sdl_color(uint8(180), uint8(100), uint8(255), uint8(SDL_ALPHA_OPAQUE))
    COL_DIM    = sdl_color(uint8(70),  uint8(70),  uint8(80),  uint8(SDL_ALPHA_OPAQUE))

    ! ---- Init SDL ----
    if (sdl_init(SDL_INIT_VIDEO) < 0) then
        write (stderr, '(A,A)') 'SDL Error: ', sdl_get_error(); stop
    end if
    if (ttf_init() < 0) then
        write (stderr, '(A,A)') 'TTF Error: ', sdl_get_error(); stop
    end if
    call net_init(rc)
    if (rc /= 0) then
        write (stderr, '(A,I0)') 'Network init failed: ', rc; stop
    end if

    main_window = sdl_create_window('ForPlay' // c_null_char, &
                    SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, &
                    SCREEN_W, SCREEN_H, ior(SDL_WINDOW_SHOWN, SDL_WINDOW_RESIZABLE))
    if (.not. c_associated(main_window)) stop 'Window creation failed'

    main_renderer = sdl_create_renderer(main_window, -1, SDL_RENDERER_ACCELERATED)
    if (.not. c_associated(main_renderer)) stop 'Renderer creation failed'

    ! Enable alpha blending so that draw calls with alpha < 255 blend
    ! against the existing framebuffer instead of overwriting the alpha
    ! channel.  Without this, Linux compositors (Wayland, picom, mutter …)
    ! see alpha < 255 and make parts of the window transparent.
    rc = sdl_set_render_draw_blend_mode(main_renderer, SDL_BLENDMODE_BLEND)

    title_font = load_font('resources/fonts/pixel musketeer/Pixel Musketeer.otf', TITLE_FONT_SZ)
    big_font   = load_font('resources/fonts/omega pixel biform/omega-pixel-biform.ttf', MAIN_FONT_SZ)
    sml_font   = load_font('resources/fonts/omega pixel biform/omega-pixel-biform.ttf', SML_FONT_SZ)
    ! Fallback to bundled DejaVu if custom fonts not found
    if (.not. c_associated(title_font)) title_font = try_load_font(TITLE_FONT_SZ)
    if (.not. c_associated(big_font))   big_font   = try_load_font(MAIN_FONT_SZ)
    if (.not. c_associated(sml_font))   sml_font   = try_load_font(SML_FONT_SZ)
    if (.not. c_associated(big_font)) stop 'No font found'
    sml_font_h = ttf_font_height(sml_font)
    big_font_h = ttf_font_height(big_font)

    ! ---- Init state ----
    current_page      = PAGE_MENU
    is_running        = .true.
    client_connected  = .false.
    waiting_for_client = .false.
    connecting        = .false.
    error_message     = ' '
    input_ip          = '127.0.0.1'
    input_port        = '12345'
    active_field      = 0
    local_ip          = ' '
    client_ip         = ' '
    am_host           = .false.
    player_index   = 1
    game_fd           = -1
    game_ready        = .false.
    gs%initialized    = .false.
    recv_init_pos     = 0
    cfg_maze_w        = DEFAULT_MAZE_W
    cfg_maze_h        = DEFAULT_MAZE_H
    cfg_item_counts(ITEM_DASH)          = DEFAULT_ITEM_DASH_COUNT
    cfg_item_counts(ITEM_VISION)        = DEFAULT_ITEM_VISION_COUNT
    cfg_item_counts(ITEM_ILLUMINATE)    = DEFAULT_ITEM_ILLUMINATE_COUNT
    cfg_item_counts(ITEM_SPEED)         = DEFAULT_ITEM_SPEED_COUNT
    cfg_item_counts(ITEM_WALL_BREAK)     = DEFAULT_ITEM_WALL_BREAK_COUNT
    cfg_min_dist      = DEFAULT_MIN_DIST
    cfg_speed_h       = DEFAULT_SPEED_H
    cfg_speed_s       = DEFAULT_SPEED_S
    cfg_vision_h      = DEFAULT_VISION_H
    cfg_vision_s      = DEFAULT_VISION_S
    cfg_reveal_h      = .false.
    cfg_reveal_s      = .true.
    cfg_num_keys      = 1
    cfg_turn_timer    = 0
    cfg_branch_pct    = 5
    show_quit_modal   = .false.
    game_ended        = .false.
    game_ended_by_quit = .false.
    endgame_msg       = ' '

    ! Lobby / room state
    lobby_recv_pos    = 0
    lobby_num_rooms   = 0
    lobby_creating    = .false.
    lobby_room_name_input = ' '
    lobby_player_name = 'Player'
    room_id        = 0
    room_num_players  = 0
    room_i_am_host    = .false.

    ! ---- Main loop ----
    do while (is_running)
        do while (sdl_poll_event(event) > 0)
            call handle_event(event)
        end do

        ! Tick the embedded server if active
        if (hosting_with_server) then
            call emb_server_tick(emb_srv)
        end if

        ! Async client connect (join page or self-connect during hosting)
        if (connecting) then
            call poll_connection()
        end if

        ! Lobby/room: poll for lobby messages
        if (current_page == PAGE_LOBBY .or. current_page == PAGE_ROOM) then
            call poll_lobby_messages()
        end if

        ! Game: always poll for opponent actions (quit can arrive at any time)
        if (current_page == PAGE_GAME .and. game_ready .and. &
            .not. game_ended) then
            call try_recv_opponent_action()
            ! Auto-pass when turn timer expires
            call check_turn_timer()
        end if

        ! Also check for opponent quit while in end-game or waiting
        if (current_page == PAGE_GAME .and. game_ready .and. &
            game_ended .and. .not. show_quit_modal) then
            call try_recv_end_message()
        end if

        ! Game: client waiting for init data
        if (current_page == PAGE_GAME .and. .not. game_ready &
            .and. .not. am_host) then
            call try_recv_game_init()
        end if

        call render()
        call sdl_delay(16)
    end do

    ! ---- Cleanup ----
    if (server_fd >= 0)  call net_close(server_fd)
    if (client_fd >= 0)  call net_close(client_fd)
    if (socket >= 0)  call net_close(socket)
    call net_cleanup()
    if (c_associated(title_font)) call ttf_close_font(title_font)
    if (c_associated(big_font)) call ttf_close_font(big_font)
    if (c_associated(sml_font)) call ttf_close_font(sml_font)
    call ttf_quit()
    call sdl_destroy_renderer(main_renderer)
    call sdl_destroy_window(main_window)
    call sdl_quit()

contains

    ! =====================================================================
    ! Helper: is it my turn?
    ! =====================================================================
    function is_my_turn() result(mine)
        logical :: mine
        mine = (gs%current_player == player_index)
    end function

    ! =====================================================================
    ! Helper: my team
    ! =====================================================================
    function my_team() result(t)
        integer :: t
        t = gs%players(player_index)%team
    end function

    ! =====================================================================
    ! Font loading
    ! =====================================================================
    pure function to_upper(str) result(res)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: res
        integer :: i, ic
        res = str
        do i = 1, len(str)
            ic = iachar(str(i:i))
            if (ic >= iachar('a') .and. ic <= iachar('z')) &
                res(i:i) = achar(ic - 32)
        end do
    end function

    function load_font(path, sz) result(f)
        character(len=*), intent(in) :: path
        integer, intent(in) :: sz
        type(c_ptr) :: f
        character(len=:), allocatable :: base_path
        character(len=512) :: fullpath

        ! Try relative to CWD first (works with fpm run from project root)
        f = ttf_open_font(trim(path) // c_null_char, sz)
        if (c_associated(f)) return

        ! Try relative to the executable location.
        ! With fpm the binary lives in build/<hash>/app/, so go up 3 levels.
        base_path = sdl_get_base_path()
        if (allocated(base_path)) then
            ! fonts next to executable (deployed layout)
            fullpath = base_path // path
            f = ttf_open_font(trim(fullpath) // c_null_char, sz)
            if (c_associated(f)) return

            ! fpm build layout: build/<hash>/app/  →  ../../.. = project root
            fullpath = base_path // '../../../' // path
            f = ttf_open_font(trim(fullpath) // c_null_char, sz)
            if (c_associated(f)) return

            ! one level up (in case run from build/ directly)
            fullpath = base_path // '../' // path
            f = ttf_open_font(trim(fullpath) // c_null_char, sz)
            if (c_associated(f)) return
        end if

        write (stderr, '(A,A)') 'Warning: font not found: ', trim(path)
    end function

    function try_load_font(sz) result(f)
        integer, intent(in) :: sz
        type(c_ptr) :: f
        character(len=256) :: paths(24)
        character(len=:), allocatable :: base_path
        integer :: i, n
        n = 0
        ! Bundled font (works everywhere) — relative to CWD
        n = n+1; paths(n) = 'resources/DejaVuSansMono.ttf'
        n = n+1; paths(n) = './resources/DejaVuSansMono.ttf'
        ! Bundled font relative to executable (fpm layout: up 3 levels)
        base_path = sdl_get_base_path()
        if (allocated(base_path)) then
            n = n+1; paths(n) = base_path // '../../../resources/DejaVuSansMono.ttf'
            n = n+1; paths(n) = base_path // 'resources/DejaVuSansMono.ttf'
        end if
        ! Windows fonts
        n = n+1; paths(n) = 'C:/Windows/Fonts/consola.ttf'
        n = n+1; paths(n) = 'C:/Windows/Fonts/arial.ttf'
        n = n+1; paths(n) = 'C:/Windows/Fonts/segoeui.ttf'
        n = n+1; paths(n) = 'C:/Windows/Fonts/cour.ttf'
        n = n+1; paths(n) = 'C:/Windows/Fonts/lucon.ttf'
        ! Linux fonts (Debian/Ubuntu)
        n = n+1; paths(n) = '/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf'
        n = n+1; paths(n) = '/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf'
        n = n+1; paths(n) = '/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf'
        n = n+1; paths(n) = '/usr/share/fonts/truetype/freefont/FreeMono.ttf'
        n = n+1; paths(n) = '/usr/share/fonts/truetype/ubuntu/UbuntuMono-R.ttf'
        ! Linux fonts (Fedora/Arch/openSUSE)
        n = n+1; paths(n) = '/usr/share/fonts/dejavu-sans-mono-fonts/DejaVuSansMono.ttf'
        n = n+1; paths(n) = '/usr/share/fonts/TTF/DejaVuSansMono.ttf'
        n = n+1; paths(n) = '/usr/share/fonts/dejavu/DejaVuSansMono.ttf'
        ! macOS fonts
        n = n+1; paths(n) = '/System/Library/Fonts/Supplemental/Courier New.ttf'
        n = n+1; paths(n) = '/System/Library/Fonts/Helvetica.ttc'
        n = n+1; paths(n) = '/System/Library/Fonts/SFNSMono.ttf'
        n = n+1; paths(n) = '/Library/Fonts/Arial.ttf'
        n = n+1; paths(n) = '/System/Library/Fonts/Supplemental/Arial.ttf'
        f = c_null_ptr
        do i = 1, n
            f = ttf_open_font(trim(paths(i)) // c_null_char, sz)
            if (c_associated(f)) return
        end do
    end function

    ! =====================================================================
    ! Event handling
    ! =====================================================================
    subroutine handle_event(ev)
        type(sdl_event), intent(in) :: ev
        select case (ev%type)
            case (SDL_QUITEVENT)
                is_running = .false.
            case (SDL_MOUSEBUTTONDOWN)
                if (ev%button%button == SDL_BUTTON_LEFT) &
                    call handle_click(int(ev%button%x), int(ev%button%y))
            case (SDL_KEYDOWN)
                call handle_keydown(ev%key%key_sym%sym)
            case (SDL_TEXTINPUT)
                if (current_page == PAGE_JOIN) &
                    call handle_text_input(ev%text%text)
                if (current_page == PAGE_LOBBY) &
                    call handle_lobby_text_input(ev%text%text)
            case (SDL_WINDOWEVENT)
                if (ev%window%event == SDL_WINDOWEVENT_SIZE_CHANGED) then
                    SCREEN_W    = ev%window%data1
                    SCREEN_H    = ev%window%data2
                    GAME_AREA_H = SCREEN_H - HUD_H
                end if
        end select
    end subroutine

    ! =====================================================================
    ! Click handling
    ! =====================================================================
    subroutine handle_click(mx, my)
        integer, intent(in) :: mx, my
        select case (current_page)
            case (PAGE_MENU);  call handle_menu_click(mx, my)
            case (PAGE_HOST);  call handle_host_click(mx, my)
            case (PAGE_JOIN);  call handle_join_click(mx, my)
            case (PAGE_GAME);  call handle_game_click(mx, my)
            case (PAGE_LOBBY); call handle_lobby_click(mx, my)
            case (PAGE_ROOM);  call handle_room_click(mx, my)
        end select
    end subroutine

    subroutine handle_menu_click(mx, my)
        integer, intent(in) :: mx, my
        if (mx>=SCREEN_W/2-150 .and. mx<=SCREEN_W/2+150 .and. my>=250 .and. my<=310) call start_hosting()
        if (mx>=SCREEN_W/2-150 .and. mx<=SCREEN_W/2+150 .and. my>=350 .and. my<=410) call start_joining()
    end subroutine

    subroutine handle_host_click(mx, my)
        integer, intent(in) :: mx, my
        integer :: py, total_items, ly, ry

        ! Simplified handling when hosting with embedded server
        if (hosting_with_server) then
            ! Cancel button: centered at (SCREEN_W/2-100, 340, 200, 40)
            if (mx >= SCREEN_W/2-100 .and. mx <= SCREEN_W/2+100 .and. &
                my >= 340 .and. my <= 380) then
                call go_back_to_menu()
            end if
            return
        end if

        ! Compute button Y to match render_host
        ! Left: General 4 rows at 224,250,276,302; Bonuses header+sep; 4 rows at 374,400,426,452
        ! Right: Hider 3 rows at 224,250,276; Seeker header+sep; 3 rows at 348,374,400
        ly = 530   ! left column final ly
        ry = 426   ! right column final ry
        py = max(ly, ry) + 10

        ! "Start game" button
        if (client_connected) then
            if (mx>=SCREEN_W/2-150 .and. mx<=SCREEN_W/2+150 .and. my>=py .and. my<=py+50) then
                call launch_game_as_host()
            end if
        end if
        ! "Back" button
        if (mx>=SCREEN_W/2-100 .and. mx<=SCREEN_W/2+100 .and. my>=py+60 .and. my<=py+100) call go_back_to_menu()

        total_items = sum(cfg_item_counts)

        ! ===== LEFT COLUMN (x_base=20): minus at 240, plus at 315 =====
        ! -- General block --
        ! Row: Width (y=224)
        if (my >= 211 .and. my <= 237) then
            if (mx >= 240 .and. mx <= 262) cfg_maze_w = max(7, cfg_maze_w - 2)
            if (mx >= 315 .and. mx <= 337) cfg_maze_w = min(MAZE_MAX_W, cfg_maze_w + 2)
        end if
        ! Row: Height (y=250)
        if (my >= 237 .and. my <= 263) then
            if (mx >= 240 .and. mx <= 262) cfg_maze_h = max(7, cfg_maze_h - 2)
            if (mx >= 315 .and. mx <= 337) cfg_maze_h = min(MAZE_MAX_H, cfg_maze_h + 2)
        end if
        ! Row: Min dist (y=276)
        if (my >= 263 .and. my <= 289) then
            if (mx >= 240 .and. mx <= 262) cfg_min_dist = max(1, cfg_min_dist - 1)
            if (mx >= 315 .and. mx <= 337) cfg_min_dist = min(99, cfg_min_dist + 1)
        end if
        ! Row: Branch % (y=302)
        if (my >= 289 .and. my <= 315) then
            if (mx >= 240 .and. mx <= 262) cfg_branch_pct = max(0, cfg_branch_pct - 1)
            if (mx >= 315 .and. mx <= 337) cfg_branch_pct = min(50, cfg_branch_pct + 1)
        end if
        ! Row: Num keys (y=328)
        if (my >= 315 .and. my <= 341) then
            if (mx >= 240 .and. mx <= 262) cfg_num_keys = max(1, cfg_num_keys - 1)
            if (mx >= 315 .and. mx <= 337) cfg_num_keys = min(MAX_KEYS, cfg_num_keys + 1)
        end if
        ! Row: Turn timer (y=354)
        if (my >= 341 .and. my <= 367) then
            if (mx >= 240 .and. mx <= 262) cfg_turn_timer = max(0, cfg_turn_timer - 5)
            if (mx >= 315 .and. mx <= 337) cfg_turn_timer = min(120, cfg_turn_timer + 5)
        end if

        ! -- Bonuses block --
        ! Row: Nb Dash (y=426)
        if (my >= 413 .and. my <= 439) then
            if (mx >= 240 .and. mx <= 262) cfg_item_counts(ITEM_DASH) = max(0, cfg_item_counts(ITEM_DASH) - 1)
            if (mx >= 315 .and. mx <= 337 .and. total_items < MAX_GROUND_ITEMS) &
                cfg_item_counts(ITEM_DASH) = cfg_item_counts(ITEM_DASH) + 1
        end if
        ! Row: Nb Vision (y=452)
        if (my >= 439 .and. my <= 465) then
            if (mx >= 240 .and. mx <= 262) cfg_item_counts(ITEM_VISION) = max(0, cfg_item_counts(ITEM_VISION) - 1)
            if (mx >= 315 .and. mx <= 337 .and. total_items < MAX_GROUND_ITEMS) &
                cfg_item_counts(ITEM_VISION) = cfg_item_counts(ITEM_VISION) + 1
        end if
        ! Row: Nb Light (y=478)
        if (my >= 465 .and. my <= 491) then
            if (mx >= 240 .and. mx <= 262) cfg_item_counts(ITEM_ILLUMINATE) = max(0, cfg_item_counts(ITEM_ILLUMINATE) - 1)
            if (mx >= 315 .and. mx <= 337 .and. total_items < MAX_GROUND_ITEMS) &
                cfg_item_counts(ITEM_ILLUMINATE) = cfg_item_counts(ITEM_ILLUMINATE) + 1
        end if
        ! Row: Nb Speed (y=504)
        if (my >= 491 .and. my <= 517) then
            if (mx >= 240 .and. mx <= 262) cfg_item_counts(ITEM_SPEED) = max(0, cfg_item_counts(ITEM_SPEED) - 1)
            if (mx >= 315 .and. mx <= 337 .and. total_items < MAX_GROUND_ITEMS) &
                cfg_item_counts(ITEM_SPEED) = cfg_item_counts(ITEM_SPEED) + 1
        end if
        ! Row: Nb Pickaxe (y=530)
        if (my >= 517 .and. my <= 543) then
            if (mx >= 240 .and. mx <= 262) cfg_item_counts(ITEM_WALL_BREAK) = max(0, cfg_item_counts(ITEM_WALL_BREAK) - 1)
            if (mx >= 315 .and. mx <= 337 .and. total_items < MAX_GROUND_ITEMS) &
                cfg_item_counts(ITEM_WALL_BREAK) = cfg_item_counts(ITEM_WALL_BREAK) + 1
        end if

        ! ===== RIGHT COLUMN (x_base=410): minus at 630, plus at 705 =====
        ! -- Hider block --
        ! Row: Speed (y=224)
        if (my >= 211 .and. my <= 237) then
            if (mx >= 630 .and. mx <= 652) cfg_speed_h = max(1, cfg_speed_h - 1)
            if (mx >= 705 .and. mx <= 727) cfg_speed_h = min(5, cfg_speed_h + 1)
        end if
        ! Row: Vision (y=250)
        if (my >= 237 .and. my <= 263) then
            if (mx >= 630 .and. mx <= 652) cfg_vision_h = max(1, cfg_vision_h - 1)
            if (mx >= 705 .and. mx <= 727) cfg_vision_h = min(10, cfg_vision_h + 1)
        end if
        ! Row: Maze revealed toggle (y=276), toggle at x=630..690
        if (my >= 263 .and. my <= 289) then
            if (mx >= 630 .and. mx <= 690) cfg_reveal_h = .not. cfg_reveal_h
        end if

        ! -- Seeker block --
        ! Row: Speed (y=348)
        if (my >= 335 .and. my <= 361) then
            if (mx >= 630 .and. mx <= 652) cfg_speed_s = max(1, cfg_speed_s - 1)
            if (mx >= 705 .and. mx <= 727) cfg_speed_s = min(5, cfg_speed_s + 1)
        end if
        ! Row: Vision (y=374)
        if (my >= 361 .and. my <= 387) then
            if (mx >= 630 .and. mx <= 652) cfg_vision_s = max(1, cfg_vision_s - 1)
            if (mx >= 705 .and. mx <= 727) cfg_vision_s = min(10, cfg_vision_s + 1)
        end if
        ! Row: Maze revealed toggle (y=400), toggle at x=630..690
        if (my >= 387 .and. my <= 413) then
            if (mx >= 630 .and. mx <= 690) cfg_reveal_s = .not. cfg_reveal_s
        end if
    end subroutine

    subroutine handle_join_click(mx, my)
        integer, intent(in) :: mx, my
        integer :: fx
        fx = SCREEN_W/2 - 175
        if (connecting) then
            if (mx>=SCREEN_W/2-100 .and. mx<=SCREEN_W/2+100 .and. my>=500 .and. my<=550) then
                if (socket >= 0) then
                    call net_close(socket); socket = -1
                end if
                connecting = .false.
                call go_back_to_menu()
            end if
            return
        end if
        if (mx>=fx .and. mx<=fx+350 .and. my>=220 .and. my<=260) then
            active_field = 0; call c_sdl_start_text_input()
        end if
        if (mx>=fx .and. mx<=fx+350 .and. my>=310 .and. my<=350) then
            active_field = 1; call c_sdl_start_text_input()
        end if
        if (mx>=SCREEN_W/2-150 .and. mx<=SCREEN_W/2+150 .and. my>=400 .and. my<=450) call attempt_connection()
        if (mx>=SCREEN_W/2-100 .and. mx<=SCREEN_W/2+100 .and. my>=500 .and. my<=550) call go_back_to_menu()
    end subroutine

    ! =====================================================================
    ! Keyboard
    ! =====================================================================
    subroutine handle_keydown(sym)
        integer(c_int32_t), intent(in) :: sym

        select case (current_page)
            case (PAGE_JOIN)
                call handle_join_keys(sym)
            case (PAGE_HOST)
                if (sym == SDLK_ESCAPE) call go_back_to_menu()
            case (PAGE_GAME)
                call handle_game_keys(sym)
            case (PAGE_LOBBY)
                call handle_lobby_keys(sym)
            case (PAGE_ROOM)
                call handle_room_keys(sym)
        end select
    end subroutine

    subroutine handle_join_keys(sym)
        integer(c_int32_t), intent(in) :: sym
        select case (sym)
            case (SDLK_BACKSPACE); call handle_backspace()
            case (SDLK_TAB)
                active_field = 1 - active_field
            case (SDLK_RETURN); call attempt_connection()
            case (SDLK_ESCAPE); call go_back_to_menu()
        end select
    end subroutine

    ! =====================================================================
    ! Game keyboard input
    ! =====================================================================
    subroutine handle_game_keys(sym)
        integer(c_int32_t), intent(in) :: sym
        integer :: d, slot, itype

        ! --- Quit confirmation modal ---
        if (show_quit_modal) then
            if (sym == SDLK_y .or. sym == SDLK_RETURN) then
                ! Confirmed quit
                show_quit_modal = .false.
                call end_game_with_quit()
            else
                ! Any other key cancels the modal
                show_quit_modal = .false.
            end if
            return
        end if

        ! --- End-game screen: buttons only ---
        if (game_ended) then
            if (sym == SDLK_ESCAPE) then
                call go_back_to_menu()
            end if
            return
        end if

        ! --- Escape opens quit confirmation ---
        if (sym == SDLK_ESCAPE) then
            show_quit_modal = .true.
            return
        end if

        if (gs%game_over .or. .not. game_ready) return
        if (.not. is_my_turn()) return

        ! --- Pass turn with Space ---
        if (sym == SDLK_SPACE) then
            call game_do_pass(gs)
            call send_action(7, 0)   ! action_type=7 means pass
            call game_end_turn(gs)
            call reset_turn_timer()
            if (gs%game_over .and. .not. game_ended) call handle_natural_game_over()
            return
        end if

        ! --- INPUT_MOVE state: arrows to move, 1-6 to select item ---
        if (gs%input_state == INPUT_MOVE) then
            d = key_to_dir(sym)
            if (d >= 0) then
                if (game_do_move(gs, player_index, d)) then
                    call send_action(0, d)   ! action_type = 0 (move)
                    call game_end_turn(gs)
                    call reset_turn_timer()
                    if (gs%game_over .and. .not. game_ended) call handle_natural_game_over()
                end if
                return
            end if

            ! Item selection: keys 1-6
            slot = key_to_slot(sym)
            if (slot >= 1 .and. slot <= 6) then
                call try_select_item(slot)
                return
            end if
        end if

        ! --- INPUT_ITEM_DIR state: arrows choose direction, Esc cancels ---
        if (gs%input_state == INPUT_ITEM_DIR) then
            d = key_to_dir(sym)
            if (d >= 0) then
                call game_do_use_item(gs, player_index, gs%selected_slot, d)
                call send_action(gs%selected_slot, d)  ! action = slot, param = dir
                gs%input_state = INPUT_MOVE
                call game_end_turn(gs)
                call reset_turn_timer()
                if (gs%game_over .and. .not. game_ended) call handle_natural_game_over()
                return
            end if
            ! Any other key cancels item selection
            gs%input_state = INPUT_MOVE
            gs%selected_slot = 0
        end if
    end subroutine

    function key_to_dir(sym) result(d)
        integer(c_int32_t), intent(in) :: sym
        integer :: d
        d = -1
        if (sym == SDLK_UP)    d = DIR_N
        if (sym == SDLK_RIGHT) d = DIR_E
        if (sym == SDLK_DOWN)  d = DIR_S
        if (sym == SDLK_LEFT)  d = DIR_W
    end function

    function key_to_slot(sym) result(s)
        integer(c_int32_t), intent(in) :: sym
        integer :: s
        s = -1
        if (sym == SDLK_1) s = 1
        if (sym == SDLK_2) s = 2
        if (sym == SDLK_3) s = 3
        if (sym == SDLK_4) s = 4
        if (sym == SDLK_5) s = 5
        if (sym == SDLK_6) s = 6
    end function

    subroutine try_select_item(slot)
        integer, intent(in) :: slot
        integer :: ni, itype

        ni = gs%players(player_index)%num_items
        if (slot > ni) return

        itype = gs%players(player_index)%inventory(slot)

        if (game_item_needs_direction(itype)) then
            gs%input_state   = INPUT_ITEM_DIR
            gs%selected_slot = slot
        else
            ! Use immediately (no direction needed)
            call game_do_use_item(gs, player_index, slot, 0)
            call send_action(slot, 0)  ! slot>0 means item use
            call game_end_turn(gs)
            call reset_turn_timer()
            if (gs%game_over .and. .not. game_ended) call handle_natural_game_over()
        end if
    end subroutine

    ! =====================================================================
    ! Text input helpers (join page)
    ! =====================================================================
    subroutine handle_backspace()
        integer :: l
        if (active_field == 0) then
            l = len_trim(input_ip);   if (l > 0) input_ip(l:l) = ' '
        else
            l = len_trim(input_port); if (l > 0) input_port(l:l) = ' '
        end if
    end subroutine

    subroutine handle_text_input(text_chars)
        character(kind=c_char), intent(in) :: text_chars(32)
        character(len=1) :: ch
        integer :: l
        ch = text_chars(1)
        if (iachar(ch) < 32 .or. iachar(ch) > 126) return
        if (active_field == 0) then
            l = len_trim(input_ip)
            if (l < 63) input_ip(l+1:l+1) = ch
        else
            if (iachar(ch)>=iachar('0') .and. iachar(ch)<=iachar('9')) then
                l = len_trim(input_port)
                if (l < 5) input_port(l+1:l+1) = ch
            end if
        end if
    end subroutine

    ! =====================================================================
    ! Sync UI config variables into the server_config struct
    ! =====================================================================
    subroutine sync_cfg_to_server_config()
        cfg%maze_w      = cfg_maze_w
        cfg%maze_h      = cfg_maze_h
        cfg%min_dist    = cfg_min_dist
        cfg%item_counts = cfg_item_counts
        cfg%speed_h     = cfg_speed_h
        cfg%speed_s     = cfg_speed_s
        cfg%vision_h    = cfg_vision_h
        cfg%vision_s    = cfg_vision_s
        cfg%reveal_h    = cfg_reveal_h
        cfg%reveal_s    = cfg_reveal_s
        cfg%num_keys    = cfg_num_keys
        cfg%turn_timer  = cfg_turn_timer
        cfg%branch_pct  = cfg_branch_pct
    end subroutine

    ! =====================================================================
    ! Page transitions
    ! =====================================================================
    subroutine start_hosting()
        integer(c_int16_t) :: pv
        character(len=256) :: err

        pv = int(DEFAULT_PORT, c_int16_t)
        call server_start(pv, server_fd, local_ip, err)
        if (server_fd < 0) then; error_message = err; return; end if

        ! Initialize the embedded server
        call emb_server_init(emb_srv, server_fd)
        hosting_with_server = .true.

        ! Self-connect to localhost
        call net_begin_connect('127.0.0.1', pv, socket, err)
        if (socket < 0) then
            error_message = err
            call emb_server_shutdown(emb_srv)
            hosting_with_server = .false.
            server_fd = -1
            return
        end if

        connecting = .true.
        connect_start_tick = sdl_get_ticks()
        error_message = ' '
        current_page = PAGE_HOST
    end subroutine

    subroutine start_joining()
        current_page  = PAGE_JOIN
        error_message = ' '
        active_field  = 0
        call c_sdl_start_text_input()
    end subroutine

    subroutine go_back_to_menu()
        ! Shut down embedded server if active (closes listen socket + all server-side client fds)
        if (hosting_with_server) then
            call emb_server_shutdown(emb_srv)
            hosting_with_server = .false.
            server_fd = -1  ! already closed by emb_server_shutdown
        end if
        if (server_fd >= 0) then; call net_close(server_fd); server_fd = -1; end if
        ! Close client sockets (game_fd may alias socket, avoid double-close)
        if (game_fd >= 0 .and. game_fd /= socket .and. game_fd /= client_fd) then
            call net_close(game_fd)
        end if
        game_fd = -1
        if (client_fd >= 0) then; call net_close(client_fd); client_fd = -1; end if
        if (socket >= 0) then; call net_close(socket); socket = -1; end if
        waiting_for_client = .false.
        client_connected   = .false.
        connecting         = .false.
        error_message      = ' '
        game_ready         = .false.
        gs%initialized     = .false.
        game_ended         = .false.
        game_ended_by_quit = .false.
        show_quit_modal    = .false.
        endgame_msg        = ' '
        current_page       = PAGE_MENU
        call c_sdl_stop_text_input()
    end subroutine

    subroutine attempt_connection()
        integer(c_int16_t) :: pv
        character(len=256) :: err
        integer :: pi
        if (len_trim(input_ip)==0 .or. len_trim(input_port)==0) then
            error_message = 'Please fill in all fields'; return
        end if
        read(input_port, *, err=99) pi
        pv = int(pi, c_int16_t)
        error_message = ' '
        call net_begin_connect(trim(input_ip), pv, socket, err)
        if (socket < 0) then; error_message = err; return; end if
        ! Connect started — poll each frame until done
        connecting = .true.
        connect_start_tick = sdl_get_ticks()
        return
    99  error_message = 'Error: invalid port'
    end subroutine

    ! Poll the async connect each frame
    subroutine poll_connection()
        integer :: res
        character(len=256) :: err
        integer(c_uint32_t) :: elapsed

        elapsed = sdl_get_ticks() - connect_start_tick

        call net_poll_connect(socket, res, err)

        if (res == 1) then
            ! Connected!
            connecting = .false.
            am_host    = .false.
            game_fd    = socket
            game_ready = .false.
            gs%initialized = .false.
            recv_init_pos = 0
            lobby_recv_pos = 0
            lobby_num_rooms = 0
            lobby_creating = .false.
            error_message = ' '
            call c_sdl_stop_text_input()

            if (hosting_with_server) then
                ! Auto-create a room and go to lobby (will switch to PAGE_ROOM on update)
                room_i_am_host = .true.
                call send_create_room('Game', trim(lobby_player_name))
                current_page = PAGE_LOBBY
                print *, '[HOST] self-connected, auto-creating room'
            else
                current_page = PAGE_LOBBY
                print *, '[CLIENT] connected, game_fd=', game_fd, ' entering lobby'
                call send_list_rooms_request()
            end if
        else if (res == -1) then
            ! Failed
            connecting = .false.
            error_message = err
        else
            ! Still pending — check timeout
            if (elapsed > CONNECT_TIMEOUT_MS) then
                connecting = .false.
                if (socket >= 0) then
                    call net_close(socket)
                    socket = -1
                end if
                error_message = 'Error: connection timed out'
            end if
        end if
    end subroutine

    subroutine check_for_client()
        logical :: got_one
        call server_try_accept(server_fd, client_fd, client_ip, got_one)
        if (got_one) then
            client_connected   = .true.
            waiting_for_client = .false.
            print *, '[HOST] client accepted, fd=', client_fd, ' ip=', trim(client_ip)
        end if
    end subroutine

    ! =====================================================================
    ! Game click handling (end-game buttons)
    ! =====================================================================
    subroutine handle_game_click(mx, my)
        integer, intent(in) :: mx, my
        integer :: hud_y
        if (.not. game_ended) return
        hud_y = GAME_AREA_H

        if (game_ended_by_quit) then
            ! Quit scenario: only "Back to room" or "Back to menu"
            ! "Back to room" button: (15, hud_y+45, 190, 35)
            if (mx >= 15 .and. mx <= 205 .and. &
                my >= hud_y+45 .and. my <= hud_y+80) then
                game_ended = .false.
                game_ready = .false.
                gs%initialized = .false.
                lobby_recv_pos = 0
                current_page = PAGE_ROOM
                return
            end if
            ! "Back to menu" button: (220, hud_y+45, 190, 35)
            if (mx >= 220 .and. mx <= 410 .and. &
                my >= hud_y+45 .and. my <= hud_y+80) then
                call go_back_to_menu()
                return
            end if
        else
            ! Natural game over: role-dependent buttons
            if (room_i_am_host) then
                ! Host: "Replay" button: (15, hud_y+45, 150, 35)
                if (mx >= 15 .and. mx <= 165 .and. &
                    my >= hud_y+45 .and. my <= hud_y+80) then
                    call send_action(9, 0)
                    return
                end if
                ! Host: "Back to room" button: (180, hud_y+45, 190, 35)
                if (mx >= 180 .and. mx <= 370 .and. &
                    my >= hud_y+45 .and. my <= hud_y+80) then
                    call send_action(10, 0)
                    return
                end if
            else
                ! Non-host: "Leave" button: (15, hud_y+45, 150, 35)
                if (mx >= 15 .and. mx <= 165 .and. &
                    my >= hud_y+45 .and. my <= hud_y+80) then
                    call send_action(11, 0)
                    game_ended = .false.
                    game_ready = .false.
                    gs%initialized = .false.
                    lobby_recv_pos = 0
                    lobby_num_rooms = 0
                    current_page = PAGE_LOBBY
                    call send_list_rooms_request()
                    return
                end if
            end if
        end if
    end subroutine

    ! =====================================================================
    ! End game: send quit message, reveal map
    ! =====================================================================
    subroutine end_game_with_quit()
        integer :: j
        ! Send quit action (action_type=8)
        call send_action(8, 0)

        ! Reveal whole map for all players
        do j = 1, gs%num_players
            gs%visible(1:gs%maze%w, 1:gs%maze%h, j) = .true.
            gs%visited(1:gs%maze%w, 1:gs%maze%h, j) = .true.
        end do
        gs%game_over = .true.
        game_ended   = .true.
        game_ended_by_quit = .true.
        endgame_msg  = 'Game abandoned'
    end subroutine

    ! =====================================================================
    ! Handle natural game over (seeker caught hider)
    ! =====================================================================
    subroutine handle_natural_game_over()
        integer :: j
        ! Reveal whole map for everyone
        do j = 1, gs%num_players
            gs%visible(1:gs%maze%w, 1:gs%maze%h, j) = .true.
            gs%visited(1:gs%maze%w, 1:gs%maze%h, j) = .true.
        end do
        game_ended = .true.
        game_ended_by_quit = .false.
        if (gs%seekers_won) then
            endgame_msg = 'The labyrinth team wins!'
        else
            endgame_msg = 'The escape team wins!'
        end if
    end subroutine

    ! =====================================================================
    ! Try receiving end/restart messages when in end-game state
    ! =====================================================================
    subroutine try_recv_end_message()
        integer :: atype, param, pidx
        logical :: got

        call server_try_recv_action(game_fd, pidx, atype, param, got)
        if (.not. got) return

        if (atype == 9) then
            ! Restart signal (replay) — reset and wait for new init
            recv_init_pos = 0
            game_ready    = .false.
            game_ended    = .false.
            game_ended_by_quit = .false.
            gs%initialized = .false.
        end if

        if (atype == 10) then
            ! Back to room — return to room page
            game_ended = .false.
            game_ready = .false.
            gs%initialized = .false.
            lobby_recv_pos = 0
            current_page = PAGE_ROOM
        end if

        if (atype == 8) then
            ! Opponent left during end-game — go back to room
            game_ended = .false.
            game_ready = .false.
            gs%initialized = .false.
            lobby_recv_pos = 0
            current_page = PAGE_ROOM
        end if
    end subroutine

    ! =====================================================================
    ! Restart game (host only): regenerate and send new init
    ! =====================================================================
    subroutine restart_game()
        integer(c_int) :: st
        integer :: player_teams(MAX_PLAYERS)
        if (.not. am_host) return

        call sync_cfg_to_server_config()

        ! Send restart signal to peer
        call server_send_action(game_fd, 0, 9, 0)

        ! Regenerate game (legacy 2-player: host=escape, peer=labyrinth)
        player_teams(1) = TEAM_ESCAPE
        player_teams(2) = TEAM_LABYRINTH
        call server_init_game(gs, cfg, 2, player_teams)

        ! Send new init to peer (player 2)
        call server_send_init_to_fd(game_fd, gs, 2, st)

        game_ready   = .true.
        game_ended   = .false.
        show_quit_modal = .false.
        game_turn_timer = cfg%turn_timer
        call reset_turn_timer()
        print *, '[HOST] game restarted'
    end subroutine

    ! =====================================================================
    ! Game initialisation & network protocol
    ! =====================================================================
    subroutine launch_game_as_host()
        integer(c_int) :: st
        integer :: player_teams(MAX_PLAYERS)
        ! Host generates maze and sends init data to client (legacy 2-player direct host)
        am_host = .true.
        player_index = 1
        game_fd = client_fd
        print *, '[HOST] launch_game_as_host: game_fd=', game_fd

        call sync_cfg_to_server_config()

        player_teams(1) = TEAM_ESCAPE
        player_teams(2) = TEAM_LABYRINTH
        call server_init_game(gs, cfg, 2, player_teams)
        print *, '[HOST] game_init done, maze=', gs%maze%w, 'x', gs%maze%h, &
                 ' items=', gs%num_items

        call server_send_init_to_fd(game_fd, gs, 2, st)
        print *, '[HOST] send_game_init: status=', st

        game_ready   = .true.
        game_turn_timer = cfg%turn_timer
        call reset_turn_timer()
        current_page = PAGE_GAME
        print *, '[HOST] game launched, page=PAGE_GAME'
    end subroutine

    ! Send full game state from host to client (uses server module)
    subroutine send_game_init()
        integer(c_int) :: st
        call server_send_init_to_fd(game_fd, gs, 2, st)
        print *, '[HOST] send_game_init: status=', st
    end subroutine

    ! Client: try to receive init data (non-blocking, accumulating)
    subroutine try_recv_game_init()
        integer(c_int8_t), target :: tmp(1024)
        integer(c_int) :: got
        integer :: needed

        ! Try to receive more data into persistent buffer
        call net_try_recv_bytes(game_fd, tmp, int(1024, c_int), got)
        if (got > 0) then
            if (recv_init_pos + got > 2048) got = 2048 - recv_init_pos
            recv_init_buf(recv_init_pos+1:recv_init_pos+got) = tmp(1:got)
            recv_init_pos = recv_init_pos + got
            print *, '[CLIENT] recv init: got', got, 'bytes, total=', recv_init_pos
        end if

        if (recv_init_pos < 10) return   ! not enough data yet

        ! Try parsing the accumulated buffer
        call server_parse_init_packet(recv_init_buf, recv_init_pos, gs, player_index, needed)
        if (needed > 0) then
            print *, '[CLIENT] need', needed, 'bytes, have', recv_init_pos
            return
        end if

        print *, '[CLIENT] full init received, I am player', player_index
        game_ready = .true.
        game_turn_timer = room_cfg%turn_timer
        call reset_turn_timer()
    end subroutine

    ! Send an action: 3 bytes [player_index, action_type, param]
    subroutine send_action(action_type, param)
        integer, intent(in) :: action_type, param
        call server_send_action(game_fd, player_index, action_type, param)
    end subroutine

    ! Reset the turn timer (call after every turn change and at game start)
    subroutine reset_turn_timer()
        turn_start_tick = sdl_get_ticks()
    end subroutine

    ! Check if turn timer has expired and auto-pass if so
    subroutine check_turn_timer()
        integer(c_uint32_t) :: elapsed_ms

        if (game_turn_timer <= 0) return
        if (.not. is_my_turn()) return
        if (gs%game_over) return
        if (show_quit_modal) return

        elapsed_ms = sdl_get_ticks() - turn_start_tick
        if (elapsed_ms >= int(game_turn_timer, c_uint32_t) * 1000_c_uint32_t) then
            ! Consume all remaining actions at once
            do while (is_my_turn() .and. .not. gs%game_over)
                call game_do_pass(gs)
                call send_action(7, 0)
                call game_end_turn(gs)
            end do
            call reset_turn_timer()
            if (gs%game_over .and. .not. game_ended) call handle_natural_game_over()
        end if
    end subroutine

    ! Try to receive another player's action (non-blocking)
    subroutine try_recv_opponent_action()
        integer :: atype, param, pidx, j
        logical :: moved, got

        call server_try_recv_action(game_fd, pidx, atype, param, got)
        if (.not. got) return

        if (atype == 0) then
            ! Move
            moved = game_do_move(gs, pidx, param)
        else if (atype == 7) then
            ! Pass turn
            call game_do_pass(gs)
        else if (atype == 8) then
            ! A player quit - reveal map and end game
            do j = 1, gs%num_players
                gs%visible(1:gs%maze%w, 1:gs%maze%h, j) = .true.
                gs%visited(1:gs%maze%w, 1:gs%maze%h, j) = .true.
            end do
            gs%game_over = .true.
            game_ended   = .true.
            game_ended_by_quit = .true.
            endgame_msg  = 'A player has quit'
            return
        else
            ! Item use: atype = slot, param = direction
            call game_do_use_item(gs, pidx, atype, param)
        end if

        call game_end_turn(gs)
        call reset_turn_timer()

        ! Check if game ended naturally
        if (gs%game_over .and. .not. game_ended) then
            call handle_natural_game_over()
        end if
    end subroutine

    ! =====================================================================
    ! Rendering
    ! =====================================================================
    subroutine render()
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(20), uint8(20), uint8(28), uint8(255))
        rc = sdl_render_clear(main_renderer)

        select case (current_page)
            case (PAGE_MENU);  call render_menu()
            case (PAGE_HOST);  call render_host()
            case (PAGE_JOIN);  call render_join()
            case (PAGE_GAME);  call render_game()
            case (PAGE_LOBBY); call render_lobby()
            case (PAGE_ROOM);  call render_room()
        end select

        call sdl_render_present(main_renderer)
    end subroutine

    ! --- Menu ---
    subroutine render_menu()
        call draw_text_centered('ForPlay', title_font, COL_CYAN, SCREEN_W/2, 80)
        call draw_text_centered('Choose an option', sml_font, COL_GRAY, &
                                SCREEN_W/2, 170)
        call draw_button(SCREEN_W/2 - 150, 250, 300, 60, 'Host a game', COL_GREEN)
        call draw_button(SCREEN_W/2 - 150, 350, 300, 60, 'Join a game', COL_CYAN)
        if (len_trim(error_message)>0) &
            call draw_text_centered(trim(error_message), sml_font, COL_RED, &
                                    SCREEN_W/2, 470)
    end subroutine

    ! --- Host ---
    subroutine render_host()
        character(len=256) :: info
        integer :: py, ly, ry

        ! Simplified UI when hosting with embedded server (brief self-connect)
        if (hosting_with_server) then
            call draw_text_centered('Starting server...', big_font, COL_CYAN, SCREEN_W/2, 200)
            write(info,'(A,A)') 'Local IP: ', trim(local_ip)
            call draw_text_centered(trim(info), sml_font, COL_WHITE, SCREEN_W/2, 260)
            call draw_button(SCREEN_W/2 - 100, 340, 200, 40, 'Cancel', COL_RED)
            return
        end if

        call draw_text_centered('Hosting', big_font, COL_CYAN, SCREEN_W/2, 20)
        write(info,'(A,A)') 'Local IP: ', trim(local_ip)
        call draw_text_centered(trim(info), sml_font, COL_WHITE, SCREEN_W/2, 64)
        write(info,'(A,I0)') 'Port: ', DEFAULT_PORT
        call draw_text_centered(trim(info), sml_font, COL_WHITE, SCREEN_W/2, 90)

        if (client_connected) then
            write(info,'(A,A)') 'Client: ', trim(client_ip)
            call draw_text_centered(trim(info), sml_font, COL_GREEN, SCREEN_W/2, 150)
        else
            call draw_text_centered('Waiting for a player...', sml_font, &
                                    COL_YELLOW, SCREEN_W/2, 150)
        end if

        ! ===== LEFT COLUMN =====
        ! --- General block ---
        call draw_text_at(sml_font, '--- General ---', COL_GRAY, 20, 180)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(80), uint8(80), uint8(100), uint8(255))
        rc = sdl_render_draw_line(main_renderer, 20, 200, 390, 200)

        ly = 224
        call draw_param_row_at(20, ly, 'Width',     cfg_maze_w);  ly = ly + 26
        call draw_param_row_at(20, ly, 'Height',    cfg_maze_h);  ly = ly + 26
        call draw_param_row_at(20, ly, 'Min dist.', cfg_min_dist); ly = ly + 26
        call draw_param_row_at(20, ly, 'Branch %',  cfg_branch_pct); ly = ly + 26
        call draw_param_row_at(20, ly, 'Num keys',  cfg_num_keys); ly = ly + 26
        call draw_param_row_at(20, ly, 'Turn timer', cfg_turn_timer); ly = ly + 26

        ! --- Bonuses block ---
        ly = ly + 8
        call draw_text_at(sml_font, '--- Bonuses ---', COL_GRAY, 20, ly)
        ly = ly + 20
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(80), uint8(80), uint8(100), uint8(255))
        rc = sdl_render_draw_line(main_renderer, 20, ly, 390, ly)
        ly = ly + 18
        call draw_param_row_at(20, ly, 'Nb Dash',   cfg_item_counts(ITEM_DASH));    ly = ly + 26
        call draw_param_row_at(20, ly, 'Nb Vision', cfg_item_counts(ITEM_VISION));  ly = ly + 26
        call draw_param_row_at(20, ly, 'Nb Light',  cfg_item_counts(ITEM_ILLUMINATE)); ly = ly + 26
        call draw_param_row_at(20, ly, 'Nb Speed',  cfg_item_counts(ITEM_SPEED));   ly = ly + 26
        call draw_param_row_at(20, ly, 'Nb Pickaxe', cfg_item_counts(ITEM_WALL_BREAK)); ly = ly + 26

        ! ===== RIGHT COLUMN =====
        ! --- Hider block ---
        call draw_text_at(sml_font, '--- Escape ---', COL_GRAY, 410, 180)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(80), uint8(80), uint8(100), uint8(255))
        rc = sdl_render_draw_line(main_renderer, 410, 200, 770, 200)

        ry = 224
        call draw_param_row_at(410, ry, 'Speed',  cfg_speed_h);  ry = ry + 26
        call draw_param_row_at(410, ry, 'Vision', cfg_vision_h); ry = ry + 26
        call draw_toggle_row_at(410, ry, 'Maze revealed', cfg_reveal_h); ry = ry + 26

        ! --- Labyrinth block ---
        ry = ry + 8
        call draw_text_at(sml_font, '--- Labyrinth ---', COL_GRAY, 410, ry)
        ry = ry + 20
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(80), uint8(80), uint8(100), uint8(255))
        rc = sdl_render_draw_line(main_renderer, 410, ry, 770, ry)
        ry = ry + 18
        call draw_param_row_at(410, ry, 'Speed',  cfg_speed_s);  ry = ry + 26
        call draw_param_row_at(410, ry, 'Vision', cfg_vision_s); ry = ry + 26
        call draw_toggle_row_at(410, ry, 'Maze revealed', cfg_reveal_s); ry = ry + 26

        ! --- Tooltip on hover ---
        call render_host_tooltip()

        ! Buttons
        py = max(ly, ry) + 10
        if (client_connected) then
            call draw_button(SCREEN_W/2 - 150, py, 300, 50, 'Start game', COL_GREEN)
        end if
        call draw_button(SCREEN_W/2 - 100, py + 60, 200, 40, 'Back', COL_RED)
    end subroutine

    subroutine draw_param_row_at(x_base, y, label, val)
        integer, intent(in) :: x_base, y, val
        character(len=*), intent(in) :: label
        character(len=16) :: vs
        type(sdl_rect) :: br
        integer :: lx, mx, vx, px

        ! Layout: label at x_base, minus at +220, value at +260, plus at +290
        lx = x_base
        mx = x_base + 220
        vx = x_base + 265
        px = x_base + 295

        call draw_text_at(sml_font, label, COL_WHITE, lx, y - sml_font_h/2)

        ! Minus button
        br = sdl_rect(mx, y - 9, 22, 18)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(60), uint8(40), uint8(40), uint8(255))
        rc = sdl_render_fill_rect(main_renderer, br)
        rc = sdl_set_render_draw_color(main_renderer, &
                COL_RED%r, COL_RED%g, COL_RED%b, uint8(255))
        rc = sdl_render_draw_rect(main_renderer, br)
        call draw_text_centered('-', sml_font, COL_RED, mx + 11, y - sml_font_h/2)

        ! Value
        write(vs, '(I0)') val
        call draw_text_centered(trim(vs), sml_font, COL_YELLOW, vx, y - sml_font_h/2)

        ! Plus button
        br = sdl_rect(px, y - 9, 22, 18)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(40), uint8(60), uint8(40), uint8(255))
        rc = sdl_render_fill_rect(main_renderer, br)
        rc = sdl_set_render_draw_color(main_renderer, &
                COL_GREEN%r, COL_GREEN%g, COL_GREEN%b, uint8(255))
        rc = sdl_render_draw_rect(main_renderer, br)
        call draw_text_centered('+', sml_font, COL_GREEN, px + 11, y - sml_font_h/2)
    end subroutine

    subroutine draw_toggle_row_at(x_base, y, label, val)
        integer, intent(in) :: x_base, y
        character(len=*), intent(in) :: label
        logical, intent(in) :: val
        type(sdl_rect) :: br
        integer :: lx, bx

        lx = x_base
        bx = x_base + 220

        call draw_text_at(sml_font, label, COL_WHITE, lx, y - sml_font_h/2)

        ! Toggle button (60 x 18)
        br = sdl_rect(bx, y - 9, 60, 18)
        if (val) then
            rc = sdl_set_render_draw_color(main_renderer, &
                    uint8(40), uint8(80), uint8(40), uint8(255))
            rc = sdl_render_fill_rect(main_renderer, br)
            rc = sdl_set_render_draw_color(main_renderer, &
                    COL_GREEN%r, COL_GREEN%g, COL_GREEN%b, uint8(255))
            rc = sdl_render_draw_rect(main_renderer, br)
            call draw_text_centered('Yes', sml_font, COL_GREEN, bx + 30, y - sml_font_h/2)
        else
            rc = sdl_set_render_draw_color(main_renderer, &
                    uint8(60), uint8(40), uint8(40), uint8(255))
            rc = sdl_render_fill_rect(main_renderer, br)
            rc = sdl_set_render_draw_color(main_renderer, &
                    COL_RED%r, COL_RED%g, COL_RED%b, uint8(255))
            rc = sdl_render_draw_rect(main_renderer, br)
            call draw_text_centered('No', sml_font, COL_RED, bx + 30, y - sml_font_h/2)
        end if
    end subroutine

    ! Tooltip for host parameters: show description when hovering over a row
    subroutine render_host_tooltip()
        integer(kind=c_int) :: mx_c, my_c
        integer(kind=c_uint32_t) :: btn_state
        integer :: mx, my
        character(len=80) :: tip
        type(sdl_rect) :: bgr
        type(sdl_surface), pointer :: surf
        type(c_ptr) :: tex
        integer :: tw

        btn_state = sdl_get_mouse_state(mx_c, my_c)
        mx = int(mx_c); my = int(my_c)

        tip = ' '

        ! --- Left column (x 20..390) ---
        if (mx >= 20 .and. mx <= 390) then
            ! General block: rows at y=224,250,276,302,328
            if (my >= 211 .and. my <= 237) tip = 'Number of columns (odd recommended)'
            if (my >= 237 .and. my <= 263) tip = 'Number of rows (odd recommended)'
            if (my >= 263 .and. my <= 289) tip = 'Min distance in cells between spawns'
            if (my >= 289 .and. my <= 315) tip = 'Extra wall removals (0=perfect maze, 50=very open)'
            if (my >= 315 .and. my <= 341) tip = 'Number of keys the escape team must find to win'
            if (my >= 341 .and. my <= 367) tip = 'Auto-pass after N seconds (0=disabled)'
            ! Bonuses block: rows at y=426,452,478,504,530
            if (my >= 413 .and. my <= 439) tip = 'Dash forward in a straight line, reveals path'
            if (my >= 439 .and. my <= 465) tip = 'Vision radius +1 to +3 (random, permanent)'
            if (my >= 465 .and. my <= 491) tip = 'Reveals the entire map for this turn'
            if (my >= 491 .and. my <= 517) tip = '+1 action per turn (permanent)'
            if (my >= 517 .and. my <= 543) tip = 'Break an adjacent wall in a given direction'
        end if

        ! --- Right column (x 410..770) ---
        if (mx >= 410 .and. mx <= 770) then
            ! Hider block: rows at y=224,250,276
            if (my >= 211 .and. my <= 237) tip = 'Actions per turn for the escape team'
            if (my >= 237 .and. my <= 263) tip = 'Vision radius at game start for the escape team'
            if (my >= 263 .and. my <= 289) tip = 'Escape team sees the full maze layout from start'
            ! Seeker block: rows at y=348,374,400
            if (my >= 335 .and. my <= 361) tip = 'Actions per turn for the labyrinth team'
            if (my >= 361 .and. my <= 387) tip = 'Vision radius at game start for the labyrinth team'
            if (my >= 387 .and. my <= 413) tip = 'Labyrinth team sees the full maze layout from start'
        end if

        if (len_trim(tip) == 0) return

        ! Draw tooltip background + text near mouse
        surf => ttf_render_text_solid(sml_font, to_upper(trim(tip))//c_null_char, COL_WHITE)
        if (.not. associated(surf)) return
        tw = surf%w
        bgr = sdl_rect(mx + 12, my - 24, tw + 10, 22)
        ! Keep tooltip on screen
        if (bgr%x + bgr%w > SCREEN_W) bgr%x = SCREEN_W - bgr%w - 4
        if (bgr%x < 0) bgr%x = 4
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(20), uint8(20), uint8(30), uint8(240))
        rc = sdl_render_fill_rect(main_renderer, bgr)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(120), uint8(120), uint8(140), uint8(255))
        rc = sdl_render_draw_rect(main_renderer, bgr)
        tex = sdl_create_texture_from_surface(main_renderer, surf)
        block
            type(sdl_rect) :: sr, dr
            sr = sdl_rect(0, 0, surf%w, surf%h)
            dr = sdl_rect(bgr%x + 5, bgr%y + 2, surf%w, surf%h)
            rc = sdl_render_copy(main_renderer, tex, sr, dr)
        end block
        call sdl_destroy_texture(tex)
        call sdl_free_surface(surf)
    end subroutine

    ! --- Join ---
    subroutine render_join()
        character(len=80) :: dt
        integer :: fx
        fx = SCREEN_W/2 - 175
        call draw_text_centered('Join a game', big_font, COL_CYAN, &
                                SCREEN_W/2, 60)
        call draw_text_at(sml_font, 'IP Address:', COL_WHITE, fx, 195)
        dt = ' '; if (len_trim(input_ip)>0) dt = trim(input_ip)
        call draw_input_field(fx, 220, 350, 40, trim(dt), active_field==0)
        call draw_text_at(sml_font, 'Port:', COL_WHITE, fx, 285)
        dt = ' '; if (len_trim(input_port)>0) dt = trim(input_port)
        call draw_input_field(fx, 310, 350, 40, trim(dt), active_field==1)
        if (connecting) then
            call draw_button(SCREEN_W/2 - 150, 400, 300, 50, 'Connecting...', COL_GRAY)
        else
            call draw_button(SCREEN_W/2 - 150, 400, 300, 50, 'Connect', COL_GREEN)
        end if
        if (len_trim(error_message)>0) &
            call draw_text_centered(trim(error_message), sml_font, COL_RED, &
                                    SCREEN_W/2, 470)
        call draw_button(SCREEN_W/2 - 100, 500, 200, 50, 'Back', COL_RED)
    end subroutine

    ! =====================================================================
    ! GAME PAGE RENDERING
    ! =====================================================================
    subroutine render_game()
        character(len=128) :: msg_text

        if (.not. game_ready) then
            call draw_text_centered('Waiting for game to start...', big_font, &
                                    COL_YELLOW, SCREEN_W/2, SCREEN_H/2 - 20)
            return
        end if

        ! Draw the maze
        call render_maze()

        ! Draw players
        call render_players()

        ! Draw HUD
        call render_hud()

        ! Item tooltip (HUD inventory + maze floor items)
        call render_game_tooltip()

        ! Draw turn / status overlay
        call render_status()

        ! Quit confirmation modal (overlay on top of everything)
        if (show_quit_modal) then
            call render_quit_modal()
        end if

        ! End-game screen
        if (game_ended) then
            call render_endgame_overlay()
        end if
    end subroutine

    ! -----------------------------------------------------------------
    ! Maze rendering
    ! -----------------------------------------------------------------
    subroutine render_maze()
        integer :: csz, ox, oy   ! cell size, offset x/y
        integer :: ix, iy, sx, sy, c
        logical :: is_vis, is_mem, show_cell, reveal
        integer :: i, pidx

        call get_maze_layout(csz, ox, oy)

        pidx = player_index
        reveal = gs%players(pidx)%reveal

        do iy = 1, gs%maze%h
            do ix = 1, gs%maze%w
                is_vis = gs%visible(ix, iy, pidx)
                is_mem = gs%visited(ix, iy, pidx)

                if (reveal) then
                    show_cell = .true.
                else
                    show_cell = is_vis .or. is_mem
                end if

                if (.not. show_cell) cycle

                sx = ox + (ix - 1) * csz
                sy = oy + (iy - 1) * csz
                c  = gs%maze%cells(ix, iy)

                call draw_cell_floor(sx, sy, csz, is_vis)

                if (reveal) then
                    call draw_cell_walls(sx, sy, csz, c, .true.)
                else
                    call draw_cell_walls(sx, sy, csz, c, is_vis)
                end if

                ! Draw ground items
                if (is_vis) then
                    do i = 1, gs%num_items
                        if (gs%items(i)%active .and. &
                            gs%items(i)%x == ix .and. gs%items(i)%y == iy) then
                            call draw_ground_item(sx, sy, csz, gs%items(i)%itype)
                        end if
                    end do
                    ! Draw keys if visible and active
                    do i = 1, gs%num_keys
                        if (gs%key_active(i) .and. gs%key_x(i) == ix .and. gs%key_y(i) == iy) then
                            call draw_key(sx, sy, csz)
                        end if
                    end do
                else
                    ! Not visible: show dimmed item if previously seen
                    do i = 1, gs%num_items
                        if (gs%items(i)%x == ix .and. gs%items(i)%y == iy) then
                            if (gs%item_seen(i, pidx)) &
                                call draw_ground_item_dimmed(sx, sy, csz, gs%items(i)%itype)
                        end if
                    end do
                end if
            end do
        end do
    end subroutine

    subroutine get_maze_layout(csz, ox, oy)
        integer, intent(out) :: csz, ox, oy
        integer :: csz_w, csz_h
        csz_w = (SCREEN_W - 20) / gs%maze%w
        csz_h = (GAME_AREA_H - 10) / gs%maze%h
        csz = min(csz_w, csz_h)
        if (csz < 8) csz = 8
        ox = (SCREEN_W - gs%maze%w * csz) / 2
        oy = (GAME_AREA_H - gs%maze%h * csz) / 2
    end subroutine

    subroutine draw_cell_floor(sx, sy, csz, bright)
        integer, intent(in) :: sx, sy, csz
        logical, intent(in) :: bright
        type(sdl_rect) :: r
        r = sdl_rect(sx + 1, sy + 1, csz - 1, csz - 1)
        if (bright) then
            rc = sdl_set_render_draw_color(main_renderer, &
                    uint8(45), uint8(45), uint8(55), uint8(255))
        else
            rc = sdl_set_render_draw_color(main_renderer, &
                    uint8(28), uint8(28), uint8(35), uint8(255))
        end if
        rc = sdl_render_fill_rect(main_renderer, r)
    end subroutine

    subroutine draw_cell_walls(sx, sy, csz, c, bright)
        integer, intent(in) :: sx, sy, csz, c
        logical, intent(in) :: bright
        type(sdl_rect) :: wr

        if (bright) then
            rc = sdl_set_render_draw_color(main_renderer, &
                    uint8(180), uint8(180), uint8(200), uint8(255))
        else
            rc = sdl_set_render_draw_color(main_renderer, &
                    uint8(60), uint8(60), uint8(75), uint8(255))
        end if

        ! North wall
        if (iand(c, WALL_N) /= 0) then
            wr = sdl_rect(sx, sy, csz + 1, WALL_THICK)
            rc = sdl_render_fill_rect(main_renderer, wr)
        end if
        ! South wall
        if (iand(c, WALL_S) /= 0) then
            wr = sdl_rect(sx, sy + csz - WALL_THICK + 1, csz + 1, WALL_THICK)
            rc = sdl_render_fill_rect(main_renderer, wr)
        end if
        ! West wall
        if (iand(c, WALL_W) /= 0) then
            wr = sdl_rect(sx, sy, WALL_THICK, csz + 1)
            rc = sdl_render_fill_rect(main_renderer, wr)
        end if
        ! East wall
        if (iand(c, WALL_E) /= 0) then
            wr = sdl_rect(sx + csz - WALL_THICK + 1, sy, WALL_THICK, csz + 1)
            rc = sdl_render_fill_rect(main_renderer, wr)
        end if
    end subroutine

    subroutine draw_ground_item(sx, sy, csz, itype)
        integer, intent(in) :: sx, sy, csz, itype
        type(sdl_rect) :: ir
        integer :: pad
        character(len=1) :: letter

        pad = csz / 4
        ir = sdl_rect(sx + pad, sy + pad, csz - 2*pad, csz - 2*pad)

        ! Background color per item type
        select case (itype)
            case (ITEM_DASH)
                rc = sdl_set_render_draw_color(main_renderer, &
                        uint8(255), uint8(140), uint8(30), uint8(200))
                letter = 'D'
            case (ITEM_VISION)
                rc = sdl_set_render_draw_color(main_renderer, &
                        uint8(160), uint8(80), uint8(255), uint8(200))
                letter = 'V'
            case (ITEM_ILLUMINATE)
                rc = sdl_set_render_draw_color(main_renderer, &
                        uint8(255), uint8(220), uint8(50), uint8(200))
                letter = 'L'
            case (ITEM_SPEED)
                rc = sdl_set_render_draw_color(main_renderer, &
                        uint8(50), uint8(220), uint8(130), uint8(200))
                letter = 'S'
            case (ITEM_WALL_BREAK)
                rc = sdl_set_render_draw_color(main_renderer, &
                        uint8(180), uint8(120), uint8(60), uint8(200))
                letter = 'P'
            case default
                return
        end select
        rc = sdl_render_fill_rect(main_renderer, ir)

        ! Draw letter (only if cell large enough)
        if (csz >= 20) then
            call draw_text_centered(letter, sml_font, COL_BLACK, &
                                    sx + csz/2, sy + csz/2 - sml_font_h/2)
        end if
    end subroutine

    ! Draw the key on the maze
    subroutine draw_key(sx, sy, csz)
        integer, intent(in) :: sx, sy, csz
        type(sdl_rect) :: ir
        integer :: pad
        pad = csz / 4
        ir = sdl_rect(sx + pad, sy + pad, csz - 2*pad, csz - 2*pad)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(255), uint8(215), uint8(0), uint8(230))
        rc = sdl_render_fill_rect(main_renderer, ir)
        ! Gold border
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(200), uint8(160), uint8(0), uint8(255))
        rc = sdl_render_draw_rect(main_renderer, ir)
        if (csz >= 20) then
            call draw_text_centered('K', sml_font, COL_BLACK, &
                                    sx + csz/2, sy + csz/2 - sml_font_h/2)
        end if
    end subroutine

    ! Ghost item: greyed-out square showing last-known item position
    subroutine draw_ghost_item(sx, sy, csz)
        integer, intent(in) :: sx, sy, csz
        type(sdl_rect) :: ir
        integer :: pad

        pad = csz / 4
        ir = sdl_rect(sx + pad, sy + pad, csz - 2*pad, csz - 2*pad)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(70), uint8(70), uint8(70), uint8(140))
        rc = sdl_render_fill_rect(main_renderer, ir)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(100), uint8(100), uint8(100), uint8(180))
        rc = sdl_render_draw_rect(main_renderer, ir)
    end subroutine

    ! Dimmed item: shows item type with reduced brightness in fog of war
    subroutine draw_ground_item_dimmed(sx, sy, csz, itype)
        integer, intent(in) :: sx, sy, csz, itype
        type(sdl_rect) :: ir
        integer :: pad
        character(len=1) :: letter

        pad = csz / 4
        ir = sdl_rect(sx + pad, sy + pad, csz - 2*pad, csz - 2*pad)

        ! Dimmed background color per item type (roughly half brightness)
        select case (itype)
            case (ITEM_DASH)
                rc = sdl_set_render_draw_color(main_renderer, &
                        uint8(120), uint8(65), uint8(15), uint8(140))
                letter = 'D'
            case (ITEM_VISION)
                rc = sdl_set_render_draw_color(main_renderer, &
                        uint8(80), uint8(40), uint8(120), uint8(140))
                letter = 'V'
            case (ITEM_ILLUMINATE)
                rc = sdl_set_render_draw_color(main_renderer, &
                        uint8(120), uint8(105), uint8(25), uint8(140))
                letter = 'L'
            case (ITEM_SPEED)
                rc = sdl_set_render_draw_color(main_renderer, &
                        uint8(25), uint8(105), uint8(60), uint8(140))
                letter = 'S'
            case (ITEM_WALL_BREAK)
                rc = sdl_set_render_draw_color(main_renderer, &
                        uint8(90), uint8(60), uint8(30), uint8(140))
                letter = 'P'
            case default
                return
        end select
        rc = sdl_render_fill_rect(main_renderer, ir)

        ! Border
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(100), uint8(100), uint8(100), uint8(180))
        rc = sdl_render_draw_rect(main_renderer, ir)

        ! Draw letter (dimmed)
        if (csz >= 20) then
            call draw_text_centered(letter, sml_font, COL_DIM, &
                                    sx + csz/2, sy + csz/2 - sml_font_h/2)
        end if
    end subroutine

    ! -----------------------------------------------------------------
    ! Player rendering
    ! -----------------------------------------------------------------
    subroutine render_players()
        integer :: csz, ox, oy, sx, sy, pad, i, mi
        type(sdl_rect) :: pr

        call get_maze_layout(csz, ox, oy)
        pad = csz / 5
        if (pad < 2) pad = 2
        mi = player_index

        do i = 1, gs%num_players
            if (gs%players(i)%captured) then
                ! Tombstone: always visible, drawn as dark cross
                sx = ox + (gs%players(i)%x - 1) * csz
                sy = oy + (gs%players(i)%y - 1) * csz
                pr = sdl_rect(sx + pad, sy + pad, csz - 2*pad, csz - 2*pad)
                rc = sdl_set_render_draw_color(main_renderer, &
                        uint8(90), uint8(90), uint8(90), uint8(180))
                rc = sdl_render_fill_rect(main_renderer, pr)
                if (csz >= 16) then
                    call draw_text_centered('X', sml_font, COL_DIM, &
                                            sx + csz/2, sy + csz/2 - sml_font_h/2)
                end if
                cycle
            end if

            if (i == mi) then
                ! Always draw myself
                sx = ox + (gs%players(i)%x - 1) * csz
                sy = oy + (gs%players(i)%y - 1) * csz
                pr = sdl_rect(sx + pad, sy + pad, csz - 2*pad, csz - 2*pad)
                if (gs%players(i)%team == TEAM_ESCAPE) then
                    rc = sdl_set_render_draw_color(main_renderer, &
                            uint8(220), uint8(50), uint8(50), uint8(255))
                else
                    rc = sdl_set_render_draw_color(main_renderer, &
                            uint8(50), uint8(120), uint8(255), uint8(255))
                end if
                rc = sdl_render_fill_rect(main_renderer, pr)
            else
                ! Other player: show if visible, else ghost
                if (gs%visible(gs%players(i)%x, gs%players(i)%y, mi)) then
                    sx = ox + (gs%players(i)%x - 1) * csz
                    sy = oy + (gs%players(i)%y - 1) * csz
                    pr = sdl_rect(sx + pad, sy + pad, csz - 2*pad, csz - 2*pad)
                    if (gs%players(i)%team == TEAM_ESCAPE) then
                        rc = sdl_set_render_draw_color(main_renderer, &
                                uint8(220), uint8(50), uint8(50), uint8(255))
                    else
                        rc = sdl_set_render_draw_color(main_renderer, &
                                uint8(50), uint8(120), uint8(255), uint8(255))
                    end if
                    rc = sdl_render_fill_rect(main_renderer, pr)
                else if (gs%has_seen(i, mi)) then
                    ! Ghost at last known position
                    sx = ox + (gs%last_seen_x(i, mi) - 1) * csz
                    sy = oy + (gs%last_seen_y(i, mi) - 1) * csz
                    pr = sdl_rect(sx + pad, sy + pad, csz - 2*pad, csz - 2*pad)
                    if (gs%players(i)%team == TEAM_ESCAPE) then
                        rc = sdl_set_render_draw_color(main_renderer, &
                                uint8(140), uint8(50), uint8(50), uint8(120))
                    else
                        rc = sdl_set_render_draw_color(main_renderer, &
                                uint8(50), uint8(80), uint8(140), uint8(120))
                    end if
                    rc = sdl_render_fill_rect(main_renderer, pr)
                end if
            end if
        end do
    end subroutine

    ! -----------------------------------------------------------------
    ! HUD rendering (bottom bar: inventory + info)
    ! -----------------------------------------------------------------
    subroutine render_hud()
        integer :: hud_y, i, bx, ni, itype, spd, act, mi
        type(sdl_rect) :: hr, ir
        character(len=64) :: label
        type(sdl_color) :: icol
        character(len=1) :: num_ch

        hud_y = GAME_AREA_H
        mi = player_index

        ! HUD background
        hr = sdl_rect(0, hud_y, SCREEN_W, HUD_H)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(30), uint8(30), uint8(40), uint8(255))
        rc = sdl_render_fill_rect(main_renderer, hr)

        ! Top border
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(100), uint8(100), uint8(120), uint8(255))
        rc = sdl_render_draw_line(main_renderer, 0, hud_y, SCREEN_W, hud_y)

        ! Show team
        if (my_team() == TEAM_ESCAPE) then
            call draw_text_at(sml_font, 'Escape team', COL_RED, 15, hud_y + 5)
        else
            call draw_text_at(sml_font, 'Labyrinth team', COL_CYAN, 15, hud_y + 5)
        end if

        ! Show inventory
        ni = gs%players(mi)%num_items

        call draw_text_at(sml_font, 'Items:', COL_GRAY, 15, hud_y + 30)
        bx = 110
        do i = 1, MAX_INVENTORY
            ir = sdl_rect(bx, hud_y + 28, 50, 28)
            if (i <= ni) then
                itype = gs%players(mi)%inventory(i)
                if (itype == ITEM_VISION .or. itype == ITEM_SPEED) cycle
                call item_color(itype, icol)
                rc = sdl_set_render_draw_color(main_renderer, &
                        icol%r, icol%g, icol%b, uint8(200))
                rc = sdl_render_fill_rect(main_renderer, ir)
                write(num_ch, '(I1)') i
                call draw_text_centered(num_ch, sml_font, COL_WHITE, &
                                        bx + 25, hud_y + 30)
                if (gs%input_state == INPUT_ITEM_DIR .and. gs%selected_slot == i) then
                    rc = sdl_set_render_draw_color(main_renderer, &
                            uint8(255), uint8(255), uint8(255), uint8(255))
                    rc = sdl_render_draw_rect(main_renderer, ir)
                end if
            else
                rc = sdl_set_render_draw_color(main_renderer, &
                        uint8(40), uint8(40), uint8(50), uint8(255))
                rc = sdl_render_fill_rect(main_renderer, ir)
            end if
            rc = sdl_set_render_draw_color(main_renderer, &
                    uint8(80), uint8(80), uint8(90), uint8(255))
            rc = sdl_render_draw_rect(main_renderer, ir)
            bx = bx + 56
        end do

        ! --- End-game HUD ---
        if (game_ended) then
            write(label, '(A,I0)') 'Turn: ', gs%turn_number
            call draw_text_at(sml_font, trim(label), COL_GRAY, 470, hud_y + 5)

            if (game_ended_by_quit) then
                call draw_button(15, hud_y + 45, 190, 35, 'Back to room', COL_CYAN)
                call draw_button(220, hud_y + 45, 190, 35, 'Back to menu', COL_RED)
            else
                if (room_i_am_host) then
                    call draw_button(15, hud_y + 45, 150, 35, 'Replay', COL_GREEN)
                    call draw_button(180, hud_y + 45, 190, 35, 'Back to room', COL_CYAN)
                else
                    call draw_button(15, hud_y + 45, 150, 35, 'Leave', COL_RED)
                    call draw_text_at(sml_font, 'Waiting for host...', &
                                      COL_DIM, 180, hud_y + 52)
                end if
            end if
            call draw_text_at(sml_font, 'Esc = Menu', COL_DIM, 470, hud_y + 52)
            return
        end if

        ! --- Normal HUD ---
        if (is_my_turn() .and. .not. gs%game_over) then
            spd = gs%players(mi)%speed
            act = gs%players(mi)%actions_left
            if (spd > 1) then
                write(label, '(A,I0,A,I0,A)') &
                    'YOUR TURN  (', act, '/', spd, ' actions)'
            else
                label = 'YOUR TURN'
            end if
            call draw_text_at(sml_font, trim(label), COL_GREEN, 15, hud_y + 62)
        else if (.not. gs%game_over) then
            write(label, '(A,I0,A)') 'Player ', gs%current_player, '''s turn...'
            call draw_text_at(sml_font, trim(label), COL_GRAY, 15, hud_y + 62)
        end if

        ! Turn timer countdown
        if (game_turn_timer > 0 .and. .not. gs%game_over) then
            block
                integer :: secs_left
                secs_left = game_turn_timer - &
                    int((sdl_get_ticks() - turn_start_tick) / 1000_c_uint32_t)
                if (secs_left < 0) secs_left = 0
                write(label, '(A,I0,A)') 'Timer: ', secs_left, 's'
                if (secs_left <= 3) then
                    call draw_text_at(sml_font, trim(label), COL_RED, 620, hud_y + 62)
                else
                    call draw_text_at(sml_font, trim(label), COL_YELLOW, 620, hud_y + 62)
                end if
            end block
        end if

        ! Vision radius + speed
        write(label, '(A,I0)') 'Vision: ', gs%players(mi)%vision_radius
        call draw_text_at(sml_font, trim(label), COL_GRAY, 470, hud_y + 30)
        if (gs%players(mi)%speed > 1) then
            write(label, '(A,I0)') 'Speed: ', gs%players(mi)%speed
            call draw_text_at(sml_font, trim(label), COL_GRAY, 620, hud_y + 30)
        end if

        ! Keys info (escape team objective)
        if (gs%num_keys > 0) then
            write(label, '(A,I0,A,I0)') 'Keys: ', gs%keys_found, '/', gs%num_keys
            call draw_text_at(sml_font, trim(label), COL_YELLOW, 470, hud_y + 5)
        end if

        ! Turn number
        write(label, '(A,I0)') 'Turn: ', gs%turn_number
        call draw_text_at(sml_font, trim(label), COL_GRAY, 620, hud_y + 5)

        ! Item direction hint
        if (gs%input_state == INPUT_ITEM_DIR) then
            call draw_text_at(sml_font, 'Choose direction (arrows)', &
                              COL_YELLOW, 470, hud_y + 55)
        else if (is_my_turn() .and. .not. gs%game_over) then
            call draw_text_at(sml_font, 'Space = pass', &
                              COL_DIM, 470, hud_y + 55)
        end if
    end subroutine

    subroutine item_color(itype, col)
        integer, intent(in) :: itype
        type(sdl_color), intent(out) :: col
        select case (itype)
            case (ITEM_DASH);       col = COL_ORANGE
            case (ITEM_VISION);     col = COL_PURPLE
            case (ITEM_ILLUMINATE); col = COL_YELLOW
            case (ITEM_SPEED);      col = COL_GREEN
            case (ITEM_WALL_BREAK); col = COL_ORANGE
            case default;           col = COL_GRAY
        end select
    end subroutine

    ! -----------------------------------------------------------------
    ! Tooltip for items: HUD inventory slots + maze floor items
    ! -----------------------------------------------------------------
    subroutine render_game_tooltip()
        integer(kind=c_int) :: mx_c, my_c
        integer(kind=c_uint32_t) :: btn_state
        integer :: mx, my, i, slot, itype, ni, mi
        integer :: csz, ox, oy, cell_x, cell_y
        character(len=80) :: tip
        type(sdl_surface), pointer :: surf
        type(c_ptr) :: tex
        type(sdl_rect) :: bgr
        integer :: tw

        btn_state = sdl_get_mouse_state(mx_c, my_c)
        mx = int(mx_c); my = int(my_c)
        tip = ' '
        mi = player_index

        ! --- Check HUD inventory slots ---
        if (my >= GAME_AREA_H + 28 .and. my <= GAME_AREA_H + 56) then
            ni = gs%players(mi)%num_items
            do slot = 1, ni
                if (mx >= 110 + (slot-1)*56 .and. mx < 110 + (slot-1)*56 + 50) then
                    itype = gs%players(mi)%inventory(slot)
                    tip = trim(game_item_name(itype)) // ': ' // &
                          trim(game_item_description(itype))
                    exit
                end if
            end do
        end if

        ! --- Check maze floor items ---
        if (len_trim(tip) == 0 .and. my < GAME_AREA_H) then
            call get_maze_layout(csz, ox, oy)
            if (csz > 0) then
                cell_x = (mx - ox) / csz + 1
                cell_y = (my - oy) / csz + 1
                if (cell_x >= 1 .and. cell_x <= gs%maze%w .and. &
                    cell_y >= 1 .and. cell_y <= gs%maze%h) then
                    if (gs%visible(cell_x, cell_y, mi)) then
                        do i = 1, gs%num_items
                            if (gs%items(i)%active .and. &
                                gs%items(i)%x == cell_x .and. &
                                gs%items(i)%y == cell_y) then
                                itype = gs%items(i)%itype
                                tip = trim(game_item_name(itype)) // ': ' // &
                                      trim(game_item_description(itype))
                                exit
                            end if
                        end do
                    else if (gs%visited(cell_x, cell_y, mi)) then
                        do i = 1, gs%num_items
                            if (gs%items(i)%x == cell_x .and. &
                                gs%items(i)%y == cell_y) then
                                if (gs%item_seen(i, mi)) then
                                    itype = gs%items(i)%itype
                                    tip = trim(game_item_name(itype)) // ': ' // &
                                          trim(game_item_description(itype))
                                    exit
                                end if
                            end if
                        end do
                    end if
                end if
            end if
        end if

        if (len_trim(tip) == 0) return

        ! Draw tooltip background + text near mouse
        surf => ttf_render_text_solid(sml_font, to_upper(trim(tip))//c_null_char, COL_WHITE)
        if (.not. associated(surf)) return
        tw = surf%w
        bgr = sdl_rect(mx + 12, my - 24, tw + 10, 22)
        ! Keep tooltip on screen
        if (bgr%x + bgr%w > SCREEN_W) bgr%x = SCREEN_W - bgr%w - 4
        if (bgr%x < 0) bgr%x = 4
        if (bgr%y < 0) bgr%y = my + 20
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(20), uint8(20), uint8(30), uint8(240))
        rc = sdl_render_fill_rect(main_renderer, bgr)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(120), uint8(120), uint8(140), uint8(255))
        rc = sdl_render_draw_rect(main_renderer, bgr)
        tex = sdl_create_texture_from_surface(main_renderer, surf)
        block
            type(sdl_rect) :: sr, dr
            sr = sdl_rect(0, 0, surf%w, surf%h)
            dr = sdl_rect(bgr%x + 5, bgr%y + 2, surf%w, surf%h)
            rc = sdl_render_copy(main_renderer, tex, sr, dr)
        end block
        call sdl_destroy_texture(tex)
        call sdl_free_surface(surf)
    end subroutine

    ! -----------------------------------------------------------------
    ! Status overlay (turn indicator only — game over handled separately)
    ! -----------------------------------------------------------------
    subroutine render_status()
        if (game_ended) return   ! handled by render_endgame_overlay

        ! Green border around the game area when it's your turn
        if (is_my_turn() .and. .not. gs%game_over) then
            rc = sdl_set_render_draw_color(main_renderer, &
                    uint8(0), uint8(200), uint8(0), uint8(255))
            rc = sdl_render_draw_rect(main_renderer, &
                    sdl_rect(0, 0, SCREEN_W, GAME_AREA_H))
            rc = sdl_render_draw_rect(main_renderer, &
                    sdl_rect(1, 1, SCREEN_W - 2, GAME_AREA_H - 2))
        end if
    end subroutine

    ! -----------------------------------------------------------------
    ! Quit confirmation modal
    ! -----------------------------------------------------------------
    subroutine render_quit_modal()
        type(sdl_rect) :: ovr
        integer :: ox, oy, ow, oh

        ow = 500; oh = 140
        ox = SCREEN_W/2 - ow/2
        oy = SCREEN_H/2 - oh/2

        ! Darken background
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(0), uint8(0), uint8(0), uint8(180))
        ovr = sdl_rect(ox, oy, ow, oh)
        rc = sdl_render_fill_rect(main_renderer, ovr)

        ! Border
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(200), uint8(100), uint8(100), uint8(255))
        rc = sdl_render_draw_rect(main_renderer, ovr)

        call draw_text_centered('Quit the game?', big_font, COL_YELLOW, &
                                SCREEN_W/2, oy + 15)
        call draw_text_centered('The map will be revealed to all players', &
                                sml_font, COL_GRAY, SCREEN_W/2, oy + 60)
        call draw_text_centered('[Y] Confirm   /   Any other key = Cancel', &
                                sml_font, COL_WHITE, SCREEN_W/2, oy + 100)
    end subroutine

    ! -----------------------------------------------------------------
    ! End-game: slim top banner only (buttons are in HUD)
    ! -----------------------------------------------------------------
    subroutine render_endgame_overlay()
        type(sdl_rect) :: ovr

        ! Slim banner at top
        ovr = sdl_rect(0, 0, SCREEN_W, 28)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(0), uint8(0), uint8(0), uint8(200))
        rc = sdl_render_fill_rect(main_renderer, ovr)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(200), uint8(200), uint8(100), uint8(255))
        rc = sdl_render_draw_line(main_renderer, 0, 27, SCREEN_W, 27)

        call draw_text_centered('GAME OVER  -  ' // trim(endgame_msg), &
                                sml_font, COL_YELLOW, SCREEN_W/2, 4)
    end subroutine

    ! =====================================================================
    ! LOBBY PAGE: room list, create, join, refresh
    ! =====================================================================

    ! --- Lobby message send helpers ---
    subroutine send_list_rooms_request()
        integer(c_int8_t), target :: dummy(1)
        integer(c_int) :: st
        call net_send_msg(game_fd, MSG_LIST_ROOMS, dummy, 0, st)
    end subroutine

    subroutine send_create_room(rname, pname)
        character(len=*), intent(in) :: rname, pname
        integer(c_int8_t), target :: buf(128)
        integer :: k, nlen, plen
        integer(c_int) :: st

        k = 0
        nlen = len_trim(rname)
        k=k+1; buf(k) = int(nlen, c_int8_t)
        buf(k+1:k+nlen) = transfer(rname(1:nlen), buf(1:nlen))
        k = k + nlen
        plen = len_trim(pname)
        k=k+1; buf(k) = int(plen, c_int8_t)
        if (plen > 0) then
            buf(k+1:k+plen) = transfer(pname(1:plen), buf(1:plen))
            k = k + plen
        end if
        call net_send_msg(game_fd, MSG_CREATE_ROOM, buf, k, st)
    end subroutine

    subroutine send_join_room(room_id, pname)
        integer, intent(in) :: room_id
        character(len=*), intent(in) :: pname
        integer(c_int8_t), target :: buf(64)
        integer :: k, plen
        integer(c_int) :: st

        k = 0
        k=k+1; buf(k) = int(room_id, c_int8_t)
        plen = len_trim(pname)
        k=k+1; buf(k) = int(plen, c_int8_t)
        if (plen > 0) then
            buf(k+1:k+plen) = transfer(pname(1:plen), buf(1:plen))
            k = k + plen
        end if
        call net_send_msg(game_fd, MSG_JOIN_ROOM, buf, k, st)
    end subroutine

    subroutine send_leave_room()
        integer(c_int8_t), target :: dummy(1)
        integer(c_int) :: st
        call net_send_msg(game_fd, MSG_LEAVE_ROOM, dummy, 0, st)
    end subroutine

    subroutine send_config_change()
        integer(c_int8_t), target :: buf(32)
        integer :: nbytes
        integer(c_int) :: st
        call server_config_to_bytes(room_cfg, buf, nbytes)
        call net_send_msg(game_fd, MSG_CONFIG_CHANGE, buf, nbytes, st)
    end subroutine

    subroutine send_team_change(slot, new_team)
        integer, intent(in) :: slot, new_team
        integer(c_int8_t), target :: buf(2)
        integer(c_int) :: st
        buf(1) = int(slot, c_int8_t)
        buf(2) = int(new_team, c_int8_t)
        call net_send_msg(game_fd, MSG_TEAM_CHANGE, buf, 2, st)
    end subroutine

    subroutine send_start_game_request()
        integer(c_int8_t), target :: dummy(1)
        integer(c_int) :: st
        call net_send_msg(game_fd, MSG_START_GAME, dummy, 0, st)
    end subroutine

    ! --- Poll lobby messages ---
    subroutine poll_lobby_messages()
        integer(c_int8_t) :: msg_type
        integer(c_int8_t) :: payload(2048)
        integer :: payload_len
        logical :: got

        call net_try_recv_msg(game_fd, lobby_recv_buf, 4096, lobby_recv_pos, &
                              msg_type, payload, 2048, payload_len, got)
        if (.not. got) return

        select case (msg_type)
            case (MSG_ROOM_LIST)
                call room_parse_list_payload(payload, payload_len, &
                    lobby_room_ids, lobby_room_names, lobby_room_nplayers, &
                    lobby_room_ingame, lobby_num_rooms)
                print *, '[LOBBY] received room list:', lobby_num_rooms, 'rooms'

            case (MSG_ROOM_UPDATE)
                call room_parse_update_payload(payload, payload_len, &
                    room_cfg, room_player_names, room_player_is_host, &
                    room_player_teams, room_num_players)
                print *, '[ROOM] update: ', room_num_players, ' players, host=', room_i_am_host
                ! If we're on the lobby page but got a room update, switch to room page
                if (current_page == PAGE_LOBBY) then
                    current_page = PAGE_ROOM
                end if

            case (MSG_START_GAME)
                ! Game is starting! Switch to game page and wait for init data
                print *, '[ROOM] game starting!'
                current_page = PAGE_GAME
                am_host = .false.   ! on dedicated server we're never HOST in the old sense
                game_ready = .false.
                gs%initialized = .false.
                game_ended = .false.
                game_ended_by_quit = .false.
                show_quit_modal = .false.
                recv_init_pos = 0
                ! Transfer any leftover lobby recv data to init buffer
                if (lobby_recv_pos > 0) then
                    recv_init_buf(1:lobby_recv_pos) = lobby_recv_buf(1:lobby_recv_pos)
                    recv_init_pos = lobby_recv_pos
                    lobby_recv_pos = 0
                end if

            case default
                print *, '[LOBBY] unknown message type:', int(msg_type)
        end select
    end subroutine

    ! --- Lobby click handling ---
    subroutine handle_lobby_click(mx, my)
        integer, intent(in) :: mx, my
        integer :: i, ry, ox

        ox = (SCREEN_W - 800) / 2

        if (lobby_creating) then
            ! "Create" confirm button
            if (mx>=SCREEN_W/2-150 .and. mx<=SCREEN_W/2+150 .and. my>=480 .and. my<=520) then
                if (len_trim(lobby_room_name_input) > 0) then
                    call send_create_room(trim(lobby_room_name_input), trim(lobby_player_name))
                    room_i_am_host = .true.
                    lobby_creating = .false.
                end if
                return
            end if
            ! "Cancel" button
            if (mx>=SCREEN_W/2-100 .and. mx<=SCREEN_W/2+100 .and. my>=530 .and. my<=565) then
                lobby_creating = .false.
                call c_sdl_stop_text_input()
                return
            end if
            ! Player name field
            if (mx>=SCREEN_W/2-150 .and. mx<=SCREEN_W/2+150 .and. my>=420 .and. my<=455) then
                active_field = 10  ! player name
                call c_sdl_start_text_input()
                return
            end if
            ! Room name field
            if (mx>=SCREEN_W/2-150 .and. mx<=SCREEN_W/2+150 .and. my>=360 .and. my<=395) then
                active_field = 11  ! room name
                call c_sdl_start_text_input()
                return
            end if
            return
        end if

        ! "Create room" button
        if (mx>=SCREEN_W/2-160 .and. mx<=SCREEN_W/2-10 .and. my>=160 .and. my<=200) then
            lobby_creating = .true.
            lobby_room_name_input = ' '
            active_field = 11  ! room name first
            call c_sdl_start_text_input()
            return
        end if

        ! "Refresh" button
        if (mx>=SCREEN_W/2+10 .and. mx<=SCREEN_W/2+160 .and. my>=160 .and. my<=200) then
            call send_list_rooms_request()
            return
        end if

        ! "Back" button
        if (mx>=SCREEN_W/2-100 .and. mx<=SCREEN_W/2+100 .and. my>=540 .and. my<=580) then
            call go_back_to_menu()
            return
        end if

        ! Room list: each room row is 45px tall starting at y=250
        do i = 1, lobby_num_rooms
            ry = 250 + (i-1) * 45
            ! "Join" button for this room
            if (mx>=620+ox .and. mx<=700+ox .and. my>=ry+5 .and. my<=ry+35) then
                if (.not. lobby_room_ingame(i) .and. lobby_room_nplayers(i) < MAX_ROOM_PLAYERS) then
                    call send_join_room(lobby_room_ids(i), trim(lobby_player_name))
                    room_i_am_host = .false.
                end if
                return
            end if
        end do
    end subroutine

    ! --- Lobby keyboard ---
    subroutine handle_lobby_keys(sym)
        integer(c_int32_t), intent(in) :: sym
        integer :: l

        if (sym == SDLK_ESCAPE) then
            if (lobby_creating) then
                lobby_creating = .false.
                call c_sdl_stop_text_input()
            else
                call go_back_to_menu()
            end if
            return
        end if

        if (.not. lobby_creating) return

        if (sym == SDLK_BACKSPACE) then
            if (active_field == 11) then
                l = len_trim(lobby_room_name_input)
                if (l > 0) lobby_room_name_input(l:l) = ' '
            else if (active_field == 10) then
                l = len_trim(lobby_player_name)
                if (l > 0) lobby_player_name(l:l) = ' '
            end if
        end if

        if (sym == SDLK_TAB) then
            if (active_field == 11) then
                active_field = 10
            else
                active_field = 11
            end if
        end if

        if (sym == SDLK_RETURN) then
            if (len_trim(lobby_room_name_input) > 0) then
                call send_create_room(trim(lobby_room_name_input), trim(lobby_player_name))
                room_i_am_host = .true.
                lobby_creating = .false.
                call c_sdl_stop_text_input()
            end if
        end if
    end subroutine

    ! --- Lobby text input ---
    subroutine handle_lobby_text_input(text_chars)
        character(kind=c_char), intent(in) :: text_chars(32)
        character(len=1) :: ch
        integer :: l

        ch = text_chars(1)
        if (iachar(ch) < 32 .or. iachar(ch) > 126) return

        if (active_field == 11) then
            l = len_trim(lobby_room_name_input)
            if (l < 30) lobby_room_name_input(l+1:l+1) = ch
        else if (active_field == 10) then
            l = len_trim(lobby_player_name)
            if (l < 30) lobby_player_name(l+1:l+1) = ch
        end if
    end subroutine

    ! --- Render lobby page ---
    subroutine render_lobby()
        integer :: i, ry, ox
        character(len=80) :: info

        ox = (SCREEN_W - 800) / 2

        call draw_text_centered('Server Lobby', big_font, COL_CYAN, SCREEN_W/2, 20)
        call draw_text_centered('Choose a room to join or create one', &
                                sml_font, COL_GRAY, SCREEN_W/2, 70)

        ! Player name display
        write(info, '(A,A)') 'Player: ', trim(lobby_player_name)
        call draw_text_at(sml_font, trim(info), COL_WHITE, 20 + ox, 110)

        if (lobby_creating) then
            ! --- Create room form ---
            call draw_text_centered('Create a Room', big_font, COL_YELLOW, SCREEN_W/2, 280)
            call draw_text_at(sml_font, 'Room name:', COL_WHITE, SCREEN_W/2 - 150, 340)
            call draw_input_field(SCREEN_W/2 - 150, 360, 300, 35, trim(lobby_room_name_input), active_field==11)
            call draw_text_at(sml_font, 'Your name:', COL_WHITE, SCREEN_W/2 - 150, 400)
            call draw_input_field(SCREEN_W/2 - 150, 420, 300, 35, trim(lobby_player_name), active_field==10)
            call draw_button(SCREEN_W/2 - 150, 480, 300, 40, 'Create', COL_GREEN)
            call draw_button(SCREEN_W/2 - 100, 530, 200, 35, 'Cancel', COL_RED)
        else
            ! --- Room list ---
            call draw_button(SCREEN_W/2 - 160, 160, 150, 40, 'Create room', COL_GREEN)
            call draw_button(SCREEN_W/2 + 10, 160, 150, 40, 'Refresh', COL_CYAN)

            ! Column headers
            call draw_text_at(sml_font, 'Room', COL_GRAY, 100 + ox, 225)
            call draw_text_at(sml_font, 'Players', COL_GRAY, 460 + ox, 225)
            call draw_text_at(sml_font, 'Status', COL_GRAY, 540 + ox, 225)

            if (lobby_num_rooms == 0) then
                call draw_text_centered('No rooms available', sml_font, &
                                        COL_DIM, SCREEN_W/2, 300)
            else
                do i = 1, lobby_num_rooms
                    ry = 250 + (i-1) * 45
                    ! Room name
                    call draw_text_at(sml_font, trim(lobby_room_names(i)), &
                                      COL_WHITE, 100 + ox, ry + 10)
                    ! Player count
                    write(info, '(I0,A,I0)') lobby_room_nplayers(i), '/', MAX_ROOM_PLAYERS
                    call draw_text_at(sml_font, trim(info), COL_YELLOW, 470 + ox, ry + 10)
                    ! Status
                    if (lobby_room_ingame(i)) then
                        call draw_text_at(sml_font, 'In game', COL_RED, 540 + ox, ry + 10)
                    else if (lobby_room_nplayers(i) >= MAX_ROOM_PLAYERS) then
                        call draw_text_at(sml_font, 'Full', COL_ORANGE, 540 + ox, ry + 10)
                    else
                        call draw_text_at(sml_font, 'Open', COL_GREEN, 540 + ox, ry + 10)
                        call draw_button(620 + ox, ry+5, 80, 30, 'Join', COL_GREEN)
                    end if

                    ! Separator line
                    rc = sdl_set_render_draw_color(main_renderer, &
                            uint8(60), uint8(60), uint8(70), uint8(255))
                    rc = sdl_render_draw_line(main_renderer, 80 + ox, ry+40, 720 + ox, ry+40)
                end do
            end if

            call draw_button(SCREEN_W/2 - 100, 540, 200, 40, 'Back', COL_RED)
        end if

        if (len_trim(error_message) > 0) &
            call draw_text_centered(trim(error_message), sml_font, COL_RED, &
                                    SCREEN_W/2, 580)
    end subroutine

    ! =====================================================================
    ! ROOM PAGE: config view, player list, start/leave
    ! =====================================================================

    ! --- Room click handling ---
    subroutine handle_room_click(mx, my)
        integer, intent(in) :: mx, my
        integer :: total_items

        ! "Leave" button (at bottom)
        if (mx>=SCREEN_W/2-100 .and. mx<=SCREEN_W/2+100 .and. my>=SCREEN_H-45 .and. my<=SCREEN_H-5) then
            call send_leave_room()
            room_id = 0
            if (hosting_with_server) then
                call go_back_to_menu()
            else
                current_page = PAGE_LOBBY
                call send_list_rooms_request()
            end if
            return
        end if

        ! "Start game" button (host only)
        if (room_i_am_host .and. room_num_players >= 2) then
            if (mx>=SCREEN_W/2-150 .and. mx<=SCREEN_W/2+150 .and. &
                my>=SCREEN_H-100 .and. my<=SCREEN_H-50) then
                call send_start_game_request()
                return
            end if
        end if

        ! Config editing (host only) - similar layout to host page
        if (.not. room_i_am_host) return

        ! ===== Player arrow buttons to swap team (check first) =====
        if (room_teams_base_y > 0 .and. my >= room_teams_base_y - 5) then
            block
                integer :: esc_n, lab_n, pi, row_y
                esc_n = 0
                lab_n = 0
                do pi = 1, room_num_players
                    if (room_player_teams(pi) == TEAM_ESCAPE) then
                        esc_n = esc_n + 1
                        row_y = room_teams_base_y + (esc_n - 1) * 28
                        if (mx >= 640 .and. mx <= 670 .and. &
                            my >= row_y - 2 .and. my <= row_y + 23) then
                            call send_team_change(pi, TEAM_LABYRINTH)
                            return
                        end if
                    else
                        lab_n = lab_n + 1
                        row_y = room_teams_base_y + (lab_n - 1) * 28
                        if (mx >= 700 .and. mx <= 730 .and. &
                            my >= row_y - 2 .and. my <= row_y + 23) then
                            call send_team_change(pi, TEAM_ESCAPE)
                            return
                        end if
                    end if
                end do
            end block
        end if

        total_items = sum(room_cfg%item_counts)

        ! ===== LEFT COLUMN (x_base=20): minus at 240, plus at 315 =====
        ! Row: Width (y=184)
        if (mx <= 400 .and. my >= 171 .and. my <= 197) then
            if (mx >= 240 .and. mx <= 262) room_cfg%maze_w = max(7, room_cfg%maze_w - 2)
            if (mx >= 315 .and. mx <= 337) room_cfg%maze_w = min(MAZE_MAX_W, room_cfg%maze_w + 2)
            call send_config_change(); return
        end if
        ! Row: Height (y=210)
        if (mx <= 400 .and. my >= 197 .and. my <= 223) then
            if (mx >= 240 .and. mx <= 262) room_cfg%maze_h = max(7, room_cfg%maze_h - 2)
            if (mx >= 315 .and. mx <= 337) room_cfg%maze_h = min(MAZE_MAX_H, room_cfg%maze_h + 2)
            call send_config_change(); return
        end if
        ! Row: Min dist (y=236)
        if (mx <= 400 .and. my >= 223 .and. my <= 249) then
            if (mx >= 240 .and. mx <= 262) room_cfg%min_dist = max(1, room_cfg%min_dist - 1)
            if (mx >= 315 .and. mx <= 337) room_cfg%min_dist = min(99, room_cfg%min_dist + 1)
            call send_config_change(); return
        end if
        ! Row: Branch % (y=262)
        if (mx <= 400 .and. my >= 249 .and. my <= 275) then
            if (mx >= 240 .and. mx <= 262) room_cfg%branch_pct = max(0, room_cfg%branch_pct - 1)
            if (mx >= 315 .and. mx <= 337) room_cfg%branch_pct = min(50, room_cfg%branch_pct + 1)
            call send_config_change(); return
        end if
        ! Row: Num keys (y=288)
        if (mx <= 400 .and. my >= 275 .and. my <= 301) then
            if (mx >= 240 .and. mx <= 262) room_cfg%num_keys = max(1, room_cfg%num_keys - 1)
            if (mx >= 315 .and. mx <= 337) room_cfg%num_keys = min(MAX_KEYS, room_cfg%num_keys + 1)
            call send_config_change(); return
        end if
        ! Row: Turn timer (y=314)
        if (mx <= 400 .and. my >= 301 .and. my <= 327) then
            if (mx >= 240 .and. mx <= 262) room_cfg%turn_timer = max(0, room_cfg%turn_timer - 5)
            if (mx >= 315 .and. mx <= 337) room_cfg%turn_timer = min(120, room_cfg%turn_timer + 5)
            call send_config_change(); return
        end if

        ! -- Bonuses block --
        ! Row: Nb Dash (y=386)
        if (mx <= 400 .and. my >= 373 .and. my <= 399) then
            if (mx >= 240 .and. mx <= 262) room_cfg%item_counts(ITEM_DASH) = max(0, room_cfg%item_counts(ITEM_DASH) - 1)
            if (mx >= 315 .and. mx <= 337 .and. total_items < MAX_GROUND_ITEMS) &
                room_cfg%item_counts(ITEM_DASH) = room_cfg%item_counts(ITEM_DASH) + 1
            call send_config_change(); return
        end if
        ! Row: Nb Vision (y=412)
        if (mx <= 400 .and. my >= 399 .and. my <= 425) then
            if (mx >= 240 .and. mx <= 262) room_cfg%item_counts(ITEM_VISION) = max(0, room_cfg%item_counts(ITEM_VISION) - 1)
            if (mx >= 315 .and. mx <= 337 .and. total_items < MAX_GROUND_ITEMS) &
                room_cfg%item_counts(ITEM_VISION) = room_cfg%item_counts(ITEM_VISION) + 1
            call send_config_change(); return
        end if
        ! Row: Nb Light (y=438)
        if (mx <= 400 .and. my >= 425 .and. my <= 451) then
            if (mx >= 240 .and. mx <= 262) room_cfg%item_counts(ITEM_ILLUMINATE) = max(0, room_cfg%item_counts(ITEM_ILLUMINATE) - 1)
            if (mx >= 315 .and. mx <= 337 .and. total_items < MAX_GROUND_ITEMS) &
                room_cfg%item_counts(ITEM_ILLUMINATE) = room_cfg%item_counts(ITEM_ILLUMINATE) + 1
            call send_config_change(); return
        end if
        ! Row: Nb Speed (y=464)
        if (mx <= 400 .and. my >= 451 .and. my <= 477) then
            if (mx >= 240 .and. mx <= 262) room_cfg%item_counts(ITEM_SPEED) = max(0, room_cfg%item_counts(ITEM_SPEED) - 1)
            if (mx >= 315 .and. mx <= 337 .and. total_items < MAX_GROUND_ITEMS) &
                room_cfg%item_counts(ITEM_SPEED) = room_cfg%item_counts(ITEM_SPEED) + 1
            call send_config_change(); return
        end if
        ! Row: Nb Pickaxe (y=490)
        if (mx <= 400 .and. my >= 477 .and. my <= 503) then
            if (mx >= 240 .and. mx <= 262) room_cfg%item_counts(ITEM_WALL_BREAK) = max(0, room_cfg%item_counts(ITEM_WALL_BREAK) - 1)
            if (mx >= 315 .and. mx <= 337 .and. total_items < MAX_GROUND_ITEMS) &
                room_cfg%item_counts(ITEM_WALL_BREAK) = room_cfg%item_counts(ITEM_WALL_BREAK) + 1
            call send_config_change(); return
        end if

        ! ===== RIGHT COLUMN (x_base=410): minus at 630, plus at 705 =====
        ! -- Hider block --
        ! Row: Speed (y=184)
        if (my >= 171 .and. my <= 197) then
            if (mx >= 630 .and. mx <= 652) room_cfg%speed_h = max(1, room_cfg%speed_h - 1)
            if (mx >= 705 .and. mx <= 727) room_cfg%speed_h = min(5, room_cfg%speed_h + 1)
            call send_config_change(); return
        end if
        ! Row: Vision (y=210)
        if (my >= 197 .and. my <= 223) then
            if (mx >= 630 .and. mx <= 652) room_cfg%vision_h = max(1, room_cfg%vision_h - 1)
            if (mx >= 705 .and. mx <= 727) room_cfg%vision_h = min(10, room_cfg%vision_h + 1)
            call send_config_change(); return
        end if
        ! Row: Maze revealed toggle (y=236)
        if (my >= 223 .and. my <= 249) then
            if (mx >= 630 .and. mx <= 690) then
                room_cfg%reveal_h = .not. room_cfg%reveal_h
                call send_config_change(); return
            end if
        end if

        ! -- Seeker block --
        ! Row: Speed (y=308)
        if (my >= 295 .and. my <= 321) then
            if (mx >= 630 .and. mx <= 652) room_cfg%speed_s = max(1, room_cfg%speed_s - 1)
            if (mx >= 705 .and. mx <= 727) room_cfg%speed_s = min(5, room_cfg%speed_s + 1)
            call send_config_change(); return
        end if
        ! Row: Vision (y=334)
        if (my >= 321 .and. my <= 347) then
            if (mx >= 630 .and. mx <= 652) room_cfg%vision_s = max(1, room_cfg%vision_s - 1)
            if (mx >= 705 .and. mx <= 727) room_cfg%vision_s = min(10, room_cfg%vision_s + 1)
            call send_config_change(); return
        end if
        ! Row: Maze revealed toggle (y=360)
        if (my >= 347 .and. my <= 373) then
            if (mx >= 630 .and. mx <= 690) then
                room_cfg%reveal_s = .not. room_cfg%reveal_s
                call send_config_change(); return
            end if
        end if
    end subroutine

    ! --- Room keyboard ---
    subroutine handle_room_keys(sym)
        integer(c_int32_t), intent(in) :: sym
        if (sym == SDLK_ESCAPE) then
            call send_leave_room()
            room_id = 0
            if (hosting_with_server) then
                call go_back_to_menu()
            else
                current_page = PAGE_LOBBY
                call send_list_rooms_request()
            end if
        end if
    end subroutine

    ! --- Render room page ---
    subroutine render_room()
        integer :: ly, ry, i
        character(len=80) :: info

        call draw_text_centered('Room', big_font, COL_CYAN, SCREEN_W/2, 10)

        ! ===== LEFT COLUMN: Config =====
        call draw_text_at(sml_font, '--- General ---', COL_GRAY, 20, 140)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(80), uint8(80), uint8(100), uint8(255))
        rc = sdl_render_draw_line(main_renderer, 20, 160, 390, 160)

        ly = 184
        call draw_param_row_at(20, ly, 'Width',     room_cfg%maze_w);  ly = ly + 26
        call draw_param_row_at(20, ly, 'Height',    room_cfg%maze_h);  ly = ly + 26
        call draw_param_row_at(20, ly, 'Min dist.', room_cfg%min_dist); ly = ly + 26
        call draw_param_row_at(20, ly, 'Branch %',  room_cfg%branch_pct); ly = ly + 26
        call draw_param_row_at(20, ly, 'Num keys',  room_cfg%num_keys); ly = ly + 26
        call draw_param_row_at(20, ly, 'Turn timer', room_cfg%turn_timer); ly = ly + 26

        ly = ly + 8
        call draw_text_at(sml_font, '--- Bonuses ---', COL_GRAY, 20, ly)
        ly = ly + 20
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(80), uint8(80), uint8(100), uint8(255))
        rc = sdl_render_draw_line(main_renderer, 20, ly, 390, ly)
        ly = ly + 18
        call draw_param_row_at(20, ly, 'Nb Dash',    room_cfg%item_counts(ITEM_DASH));    ly = ly + 26
        call draw_param_row_at(20, ly, 'Nb Vision',  room_cfg%item_counts(ITEM_VISION));  ly = ly + 26
        call draw_param_row_at(20, ly, 'Nb Light',   room_cfg%item_counts(ITEM_ILLUMINATE)); ly = ly + 26
        call draw_param_row_at(20, ly, 'Nb Speed',   room_cfg%item_counts(ITEM_SPEED));   ly = ly + 26
        call draw_param_row_at(20, ly, 'Nb Pickaxe', room_cfg%item_counts(ITEM_WALL_BREAK)); ly = ly + 26

        ! ===== RIGHT COLUMN: Hider/Seeker config =====
        call draw_text_at(sml_font, '--- Escape ---', COL_GRAY, 410, 140)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(80), uint8(80), uint8(100), uint8(255))
        rc = sdl_render_draw_line(main_renderer, 410, 160, 770, 160)

        ry = 184
        call draw_param_row_at(410, ry, 'Speed',  room_cfg%speed_h);  ry = ry + 26
        call draw_param_row_at(410, ry, 'Vision', room_cfg%vision_h); ry = ry + 26
        call draw_toggle_row_at(410, ry, 'Maze revealed', room_cfg%reveal_h); ry = ry + 26

        ry = ry + 8
        call draw_text_at(sml_font, '--- Labyrinth ---', COL_GRAY, 410, ry)
        ry = ry + 20
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(80), uint8(80), uint8(100), uint8(255))
        rc = sdl_render_draw_line(main_renderer, 410, ry, 770, ry)
        ry = ry + 18
        call draw_param_row_at(410, ry, 'Speed',  room_cfg%speed_s);  ry = ry + 26
        call draw_param_row_at(410, ry, 'Vision', room_cfg%vision_s); ry = ry + 26
        call draw_toggle_row_at(410, ry, 'Maze revealed', room_cfg%reveal_s); ry = ry + 26

        ! ===== Player Team Columns =====
        ry = ry + 20
        call draw_text_at(sml_font, '--- Escape Team ---', COL_RED, 410, ry)
        call draw_text_at(sml_font, '--- Labyrinth Team ---', COL_CYAN, 700, ry)
        ry = ry + 20
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(180), uint8(50), uint8(50), uint8(255))
        rc = sdl_render_draw_line(main_renderer, 410, ry, 680, ry)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(50), uint8(100), uint8(180), uint8(255))
        rc = sdl_render_draw_line(main_renderer, 700, ry, 1000, ry)

        room_teams_base_y = ry + 14
        block
            integer :: esc_y, lab_y
            esc_y = room_teams_base_y
            lab_y = room_teams_base_y
            do i = 1, room_num_players
                if (room_player_teams(i) == TEAM_ESCAPE) then
                    if (room_player_is_host(i)) then
                        write(info, '(A,A)') trim(room_player_names(i)), ' (host)'
                        call draw_text_at(sml_font, trim(info), COL_YELLOW, 420, esc_y)
                    else
                        call draw_text_at(sml_font, trim(room_player_names(i)), COL_WHITE, 420, esc_y)
                    end if
                    if (room_i_am_host) &
                        call draw_button(640, esc_y + 1, 30, 22, '>>', COL_YELLOW)
                    esc_y = esc_y + 28
                else
                    if (room_i_am_host) &
                        call draw_button(700, lab_y + 1, 30, 22, '<<', COL_YELLOW)
                    if (room_player_is_host(i)) then
                        write(info, '(A,A)') trim(room_player_names(i)), ' (host)'
                        call draw_text_at(sml_font, trim(info), COL_YELLOW, 735, lab_y)
                    else
                        call draw_text_at(sml_font, trim(room_player_names(i)), COL_WHITE, 735, lab_y)
                    end if
                    lab_y = lab_y + 28
                end if
            end do
        end block

        if (.not. room_i_am_host) then
            call draw_text_centered('Waiting for host to start...', sml_font, &
                                    COL_DIM, SCREEN_W/2, 50)
        end if

        if (room_i_am_host .and. room_num_players < 2) then
            call draw_text_centered('Waiting for another player...', sml_font, &
                                    COL_YELLOW, SCREEN_W/2, 50)
        end if

        ! Connection info when hosting
        if (hosting_with_server) then
            write(info, '(A,A,A,I0)') 'IP: ', trim(local_ip), '  Port: ', DEFAULT_PORT
            call draw_text_centered(trim(info), sml_font, COL_GRAY, SCREEN_W/2, 75)
        end if

        ! Start button (host only, 2+ players) at bottom
        if (room_i_am_host .and. room_num_players >= 2) then
            call draw_button(SCREEN_W/2 - 150, SCREEN_H - 100, 300, 50, 'Start game', COL_GREEN)
        end if

        ! Leave button at bottom
        call draw_button(SCREEN_W/2 - 100, SCREEN_H - 45, 200, 40, 'Leave', COL_RED)
    end subroutine

    function int_to_str(n) result(s)
        integer, intent(in) :: n
        character(len=16) :: s
        write(s, '(I0)') n
    end function

    ! =====================================================================
    ! Drawing primitives
    ! =====================================================================
    subroutine draw_text_at(fnt, str, color, x, y)
        type(c_ptr), intent(in) :: fnt
        character(len=*), intent(in) :: str
        type(sdl_color), intent(in) :: color
        integer, intent(in) :: x, y
        type(sdl_surface), pointer :: surf
        type(c_ptr) :: tex
        type(sdl_rect) :: sr, dr
        if (len_trim(str)==0) return
        surf => ttf_render_text_solid(fnt, to_upper(trim(str))//c_null_char, color)
        if (.not. associated(surf)) return
        tex = sdl_create_texture_from_surface(main_renderer, surf)
        sr = sdl_rect(0, 0, surf%w, surf%h)
        dr = sdl_rect(x, y, surf%w, surf%h)
        rc = sdl_render_copy(main_renderer, tex, sr, dr)
        call sdl_destroy_texture(tex)
        call sdl_free_surface(surf)
    end subroutine

    subroutine draw_text_centered(str, fnt, color, cx, y)
        character(len=*), intent(in) :: str
        type(c_ptr), intent(in) :: fnt
        type(sdl_color), intent(in) :: color
        integer, intent(in) :: cx, y
        type(sdl_surface), pointer :: surf
        type(c_ptr) :: tex
        type(sdl_rect) :: sr, dr
        integer :: x
        if (len_trim(str)==0) return
        surf => ttf_render_text_solid(fnt, to_upper(trim(str))//c_null_char, color)
        if (.not. associated(surf)) return
        tex = sdl_create_texture_from_surface(main_renderer, surf)
        x = cx - surf%w/2
        sr = sdl_rect(0, 0, surf%w, surf%h)
        dr = sdl_rect(x, y, surf%w, surf%h)
        rc = sdl_render_copy(main_renderer, tex, sr, dr)
        call sdl_destroy_texture(tex)
        call sdl_free_surface(surf)
    end subroutine

    subroutine draw_button(x, y, w, h, label, color)
        integer, intent(in) :: x, y, w, h
        character(len=*), intent(in) :: label
        type(sdl_color), intent(in) :: color
        type(sdl_rect) :: br
        br = sdl_rect(x, y, w, h)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(50), uint8(50), uint8(60), uint8(255))
        rc = sdl_render_fill_rect(main_renderer, br)
        rc = sdl_set_render_draw_color(main_renderer, &
                color%r, color%g, color%b, color%a)
        rc = sdl_render_draw_rect(main_renderer, br)
        br = sdl_rect(x+1, y+1, w-2, h-2)
        rc = sdl_render_draw_rect(main_renderer, br)
        call draw_text_centered(label, sml_font, color, x+w/2, y+(h-sml_font_h)/2)
    end subroutine

    subroutine draw_input_field(x, y, w, h, txt, is_active)
        integer, intent(in) :: x, y, w, h
        character(len=*), intent(in) :: txt
        logical, intent(in) :: is_active
        type(sdl_rect) :: fr
        fr = sdl_rect(x, y, w, h)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(20), uint8(20), uint8(30), uint8(255))
        rc = sdl_render_fill_rect(main_renderer, fr)
        if (is_active) then
            rc = sdl_set_render_draw_color(main_renderer, &
                    uint8(100), uint8(200), uint8(255), uint8(255))
        else
            rc = sdl_set_render_draw_color(main_renderer, &
                    uint8(100), uint8(100), uint8(120), uint8(255))
        end if
        rc = sdl_render_draw_rect(main_renderer, fr)
        if (len_trim(txt)>0) &
            call draw_text_at(sml_font, txt, COL_WHITE, x+8, y+(h-sml_font_h)/2)
        if (is_active) then
            block
                integer :: ticks, cx2
                type(sdl_rect) :: cr
                type(sdl_surface), pointer :: ms
                ticks = sdl_get_ticks()
                if (mod(ticks/500, 2)==0) then
                    cx2 = x + 8
                    if (len_trim(txt)>0) then
                        ms => ttf_render_text_solid(sml_font, &
                            to_upper(trim(txt))//c_null_char, COL_WHITE)
                        if (associated(ms)) then
                            cx2 = x + 8 + ms%w
                            call sdl_free_surface(ms)
                        end if
                    end if
                    cr = sdl_rect(cx2, y+6, 2, h-12)
                    rc = sdl_set_render_draw_color(main_renderer, &
                            uint8(255), uint8(255), uint8(255), uint8(255))
                    rc = sdl_render_fill_rect(main_renderer, cr)
                end if
            end block
        end if
    end subroutine

end program forplay
