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
    implicit none

    ! --- Constants ---
    integer, parameter :: SCREEN_W      = 800
    integer, parameter :: SCREEN_H      = 600
    integer, parameter :: TITLE_FONT_SZ = 48
    integer, parameter :: MAIN_FONT_SZ  = 24
    integer, parameter :: SML_FONT_SZ   = 14
    integer, parameter :: DEFAULT_PORT  = 12345
    integer, parameter :: HUD_H         = 90
    integer, parameter :: GAME_AREA_H   = SCREEN_H - HUD_H  ! 510
    integer, parameter :: DEFAULT_MAZE_W = 21
    integer, parameter :: DEFAULT_MAZE_H = 15
    integer, parameter :: DEFAULT_ITEM_DASH_COUNT       = 3
    integer, parameter :: DEFAULT_ITEM_VISION_COUNT     = 2
    integer, parameter :: DEFAULT_ITEM_ILLUMINATE_COUNT = 2
    integer, parameter :: DEFAULT_ITEM_SPEED_COUNT      = 1
    integer, parameter :: DEFAULT_MIN_DIST  = 15
    integer, parameter :: DEFAULT_SPEED_H   = 2
    integer, parameter :: DEFAULT_SPEED_S   = 1
    integer, parameter :: DEFAULT_VISION_H  = 1
    integer, parameter :: DEFAULT_VISION_S  = 2
    integer, parameter :: WALL_THICK    = 2

    ! Page identifiers
    integer, parameter :: PAGE_MENU = 0
    integer, parameter :: PAGE_HOST = 1
    integer, parameter :: PAGE_JOIN = 2
    integer, parameter :: PAGE_GAME = 3

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
    integer(c_int) :: my_socket  = -1
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
    logical :: am_hider          ! .true. = I play as the hider
    integer(c_int) :: game_fd    ! socket for game communication
    logical :: game_ready        ! .true. once init data exchanged

    ! Game parameters (configurable by host before launch)
    integer :: cfg_maze_w, cfg_maze_h, cfg_min_dist
    integer :: cfg_item_counts(NUM_ITEM_TYPES)
    integer :: cfg_speed_h, cfg_speed_s
    integer :: cfg_vision_h, cfg_vision_s
    logical :: cfg_reveal_h, cfg_reveal_s
    logical :: cfg_host_hides

    ! Modal / end-game state
    logical :: show_quit_modal     ! confirmation dialog visible
    logical :: game_ended          ! game ended (quit or win), map revealed
    character(len=128) :: endgame_msg  ! message to show on end screen

    ! Persistent buffer for receiving game init data
    integer(c_int8_t) :: recv_init_buf(2048)
    integer :: recv_init_pos

    ! SDL text input
    interface
        subroutine c_sdl_start_text_input() bind(C, name="SDL_StartTextInput")
        end subroutine
        subroutine c_sdl_stop_text_input() bind(C, name="SDL_StopTextInput")
        end subroutine
    end interface

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
                    SCREEN_W, SCREEN_H, SDL_WINDOW_SHOWN)
    if (.not. c_associated(main_window)) stop 'Window creation failed'

    main_renderer = sdl_create_renderer(main_window, -1, SDL_RENDERER_ACCELERATED)
    if (.not. c_associated(main_renderer)) stop 'Renderer creation failed'

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
    am_hider          = .false.
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
    cfg_min_dist      = DEFAULT_MIN_DIST
    cfg_speed_h       = DEFAULT_SPEED_H
    cfg_speed_s       = DEFAULT_SPEED_S
    cfg_vision_h      = DEFAULT_VISION_H
    cfg_vision_s      = DEFAULT_VISION_S
    cfg_reveal_h      = .false.
    cfg_reveal_s      = .true.
    cfg_host_hides    = .true.
    show_quit_modal   = .false.
    game_ended        = .false.
    endgame_msg       = ' '

    ! ---- Main loop ----
    do while (is_running)
        do while (sdl_poll_event(event) > 0)
            call handle_event(event)
        end do

        ! Async server accept
        if (current_page == PAGE_HOST .and. waiting_for_client &
            .and. .not. client_connected) then
            call check_for_client()
        end if

        ! Async client connect
        if (current_page == PAGE_JOIN .and. connecting) then
            call poll_connection()
        end if

        ! Game: always poll for opponent actions (quit can arrive at any time)
        if (current_page == PAGE_GAME .and. game_ready .and. &
            .not. game_ended) then
            call try_recv_opponent_action()
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
    if (my_socket >= 0)  call net_close(my_socket)
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
        if (am_hider) then
            mine = (gs%turn == 0)   ! hider's turn
        else
            mine = (gs%turn == 1)   ! seeker's turn
        end if
    end function

    ! =====================================================================
    ! Helper: my role
    ! =====================================================================
    function my_role() result(r)
        integer :: r
        if (am_hider) then
            r = ROLE_HIDER
        else
            r = ROLE_SEEKER
        end if
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
        f = ttf_open_font(trim(path) // c_null_char, sz)
    end function

    function try_load_font(sz) result(f)
        integer, intent(in) :: sz
        type(c_ptr) :: f
        character(len=256) :: paths(20)
        integer :: i
        ! Bundled font (works everywhere)
        paths(1) = 'resources/DejaVuSansMono.ttf'
        paths(2) = './resources/DejaVuSansMono.ttf'
        ! Windows fonts
        paths(3) = 'C:/Windows/Fonts/consola.ttf'
        paths(4) = 'C:/Windows/Fonts/arial.ttf'
        paths(5) = 'C:/Windows/Fonts/segoeui.ttf'
        paths(6) = 'C:/Windows/Fonts/cour.ttf'
        paths(7) = 'C:/Windows/Fonts/lucon.ttf'
        ! Linux fonts (Debian/Ubuntu)
        paths(8) = '/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf'
        paths(9) = '/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf'
        paths(10) = '/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf'
        paths(11) = '/usr/share/fonts/truetype/freefont/FreeMono.ttf'
        paths(12) = '/usr/share/fonts/truetype/ubuntu/UbuntuMono-R.ttf'
        ! Linux fonts (Fedora/Arch/openSUSE)
        paths(13) = '/usr/share/fonts/dejavu-sans-mono-fonts/DejaVuSansMono.ttf'
        paths(14) = '/usr/share/fonts/TTF/DejaVuSansMono.ttf'
        paths(15) = '/usr/share/fonts/dejavu/DejaVuSansMono.ttf'
        ! macOS fonts
        paths(16) = '/System/Library/Fonts/Supplemental/Courier New.ttf'
        paths(17) = '/System/Library/Fonts/Helvetica.ttc'
        paths(18) = '/System/Library/Fonts/SFNSMono.ttf'
        paths(19) = '/Library/Fonts/Arial.ttf'
        paths(20) = '/System/Library/Fonts/Supplemental/Arial.ttf'
        f = c_null_ptr
        do i = 1, 20
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
        end select
    end subroutine

    ! =====================================================================
    ! Click handling
    ! =====================================================================
    subroutine handle_click(mx, my)
        integer, intent(in) :: mx, my
        select case (current_page)
            case (PAGE_MENU); call handle_menu_click(mx, my)
            case (PAGE_HOST); call handle_host_click(mx, my)
            case (PAGE_JOIN); call handle_join_click(mx, my)
            case (PAGE_GAME); call handle_game_click(mx, my)
        end select
    end subroutine

    subroutine handle_menu_click(mx, my)
        integer, intent(in) :: mx, my
        if (mx>=250 .and. mx<=550 .and. my>=250 .and. my<=310) call start_hosting()
        if (mx>=250 .and. mx<=550 .and. my>=350 .and. my<=410) call start_joining()
    end subroutine

    subroutine handle_host_click(mx, my)
        integer, intent(in) :: mx, my
        integer :: py, total_items, ly, ry

        ! Compute button Y to match render_host
        ! Left: General 4 rows at 224,250,276,302; Bonuses header+sep; 4 rows at 374,400,426,452
        ! Right: Hider 3 rows at 224,250,276; Seeker header+sep; 3 rows at 348,374,400
        ly = 478   ! left column final ly
        ry = 426   ! right column final ry
        py = max(ly, ry) + 10

        ! "Start game" button
        if (client_connected) then
            if (mx>=250 .and. mx<=550 .and. my>=py .and. my<=py+50) then
                call launch_game_as_host()
            end if
        end if
        ! "Back" button
        if (mx>=300 .and. mx<=500 .and. my>=py+60 .and. my<=py+100) call go_back_to_menu()

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
        ! Row: Host hides toggle (y=302), toggle at x=240..300
        if (my >= 289 .and. my <= 315) then
            if (mx >= 240 .and. mx <= 300) cfg_host_hides = .not. cfg_host_hides
        end if

        ! -- Bonuses block --
        ! Row: Nb Dash (y=374)
        if (my >= 361 .and. my <= 387) then
            if (mx >= 240 .and. mx <= 262) cfg_item_counts(ITEM_DASH) = max(0, cfg_item_counts(ITEM_DASH) - 1)
            if (mx >= 315 .and. mx <= 337 .and. total_items < MAX_GROUND_ITEMS) &
                cfg_item_counts(ITEM_DASH) = cfg_item_counts(ITEM_DASH) + 1
        end if
        ! Row: Nb Vision (y=400)
        if (my >= 387 .and. my <= 413) then
            if (mx >= 240 .and. mx <= 262) cfg_item_counts(ITEM_VISION) = max(0, cfg_item_counts(ITEM_VISION) - 1)
            if (mx >= 315 .and. mx <= 337 .and. total_items < MAX_GROUND_ITEMS) &
                cfg_item_counts(ITEM_VISION) = cfg_item_counts(ITEM_VISION) + 1
        end if
        ! Row: Nb Light (y=426)
        if (my >= 413 .and. my <= 439) then
            if (mx >= 240 .and. mx <= 262) cfg_item_counts(ITEM_ILLUMINATE) = max(0, cfg_item_counts(ITEM_ILLUMINATE) - 1)
            if (mx >= 315 .and. mx <= 337 .and. total_items < MAX_GROUND_ITEMS) &
                cfg_item_counts(ITEM_ILLUMINATE) = cfg_item_counts(ITEM_ILLUMINATE) + 1
        end if
        ! Row: Nb Speed (y=452)
        if (my >= 439 .and. my <= 465) then
            if (mx >= 240 .and. mx <= 262) cfg_item_counts(ITEM_SPEED) = max(0, cfg_item_counts(ITEM_SPEED) - 1)
            if (mx >= 315 .and. mx <= 337 .and. total_items < MAX_GROUND_ITEMS) &
                cfg_item_counts(ITEM_SPEED) = cfg_item_counts(ITEM_SPEED) + 1
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
        if (connecting) then
            ! While connecting, only allow "Back" (which cancels)
            if (mx>=300 .and. mx<=500 .and. my>=500 .and. my<=550) then
                if (my_socket >= 0) then
                    call net_close(my_socket); my_socket = -1
                end if
                connecting = .false.
                call go_back_to_menu()
            end if
            return
        end if
        if (mx>=250 .and. mx<=600 .and. my>=220 .and. my<=260) then
            active_field = 0; call c_sdl_start_text_input()
        end if
        if (mx>=250 .and. mx<=600 .and. my>=310 .and. my<=350) then
            active_field = 1; call c_sdl_start_text_input()
        end if
        if (mx>=250 .and. mx<=550 .and. my>=400 .and. my<=450) call attempt_connection()
        if (mx>=300 .and. mx<=500 .and. my>=500 .and. my<=550) call go_back_to_menu()
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
            if (gs%game_over .and. .not. game_ended) call handle_natural_game_over()
            return
        end if

        ! --- INPUT_MOVE state: arrows to move, 1-6 to select item ---
        if (gs%input_state == INPUT_MOVE) then
            d = key_to_dir(sym)
            if (d >= 0) then
                if (game_do_move(gs, my_role(), d)) then
                    call send_action(0, d)   ! action_type = 0 (move)
                    call game_end_turn(gs)
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
                call game_do_use_item(gs, my_role(), gs%selected_slot, d)
                call send_action(gs%selected_slot, d)  ! action = slot, param = dir
                gs%input_state = INPUT_MOVE
                call game_end_turn(gs)
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

        if (am_hider) then
            ni = gs%hider%num_items
        else
            ni = gs%seeker%num_items
        end if
        if (slot > ni) return

        if (am_hider) then
            itype = gs%hider%inventory(slot)
        else
            itype = gs%seeker%inventory(slot)
        end if

        if (game_item_needs_direction(itype)) then
            gs%input_state   = INPUT_ITEM_DIR
            gs%selected_slot = slot
        else
            ! Use immediately (no direction needed)
            call game_do_use_item(gs, my_role(), slot, 0)
            call send_action(slot, 0)  ! slot>0 means item use
            call game_end_turn(gs)
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
    ! Page transitions
    ! =====================================================================
    subroutine start_hosting()
        integer(c_int16_t) :: pv
        character(len=256) :: err
        pv = int(DEFAULT_PORT, c_int16_t)
        call net_start_server(pv, server_fd, err)
        if (server_fd < 0) then; error_message = err; return; end if
        call net_get_local_ip(local_ip)
        client_connected  = .false.
        waiting_for_client = .true.
        client_ip         = ' '
        error_message     = ' '
        current_page      = PAGE_HOST
    end subroutine

    subroutine start_joining()
        current_page  = PAGE_JOIN
        error_message = ' '
        active_field  = 0
        call c_sdl_start_text_input()
    end subroutine

    subroutine go_back_to_menu()
        if (server_fd >= 0) then; call net_close(server_fd); server_fd = -1; end if
        if (client_fd >= 0) then; call net_close(client_fd); client_fd = -1; end if
        if (my_socket >= 0) then; call net_close(my_socket); my_socket = -1; end if
        waiting_for_client = .false.
        client_connected   = .false.
        connecting         = .false.
        error_message      = ' '
        game_ready         = .false.
        gs%initialized     = .false.
        game_ended         = .false.
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
        call net_begin_connect(trim(input_ip), pv, my_socket, err)
        if (my_socket < 0) then; error_message = err; return; end if
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

        call net_poll_connect(my_socket, res, err)

        if (res == 1) then
            ! Connected!
            connecting = .false.
            am_host    = .false.
            game_fd    = my_socket
            game_ready = .false.
            gs%initialized = .false.
            recv_init_pos = 0
            error_message = ' '
            current_page  = PAGE_GAME
            print *, '[CLIENT] connected, game_fd=', game_fd, ' waiting for init'
            call c_sdl_stop_text_input()
        else if (res == -1) then
            ! Failed
            connecting = .false.
            error_message = err
        else
            ! Still pending — check timeout
            if (elapsed > CONNECT_TIMEOUT_MS) then
                connecting = .false.
                if (my_socket >= 0) then
                    call net_close(my_socket)
                    my_socket = -1
                end if
                error_message = 'Error: connection timed out'
            end if
        end if
    end subroutine

    subroutine check_for_client()
        logical :: got_one
        call net_try_accept_nonblocking(server_fd, client_fd, client_ip, got_one)
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

        ! "Back to menu" button in HUD: (15, hud_y+45, 170, 35)
        if (mx >= 15 .and. mx <= 185 .and. &
            my >= hud_y+45 .and. my <= hud_y+80) then
            call go_back_to_menu()
            return
        end if

        ! "New game" button in HUD: (200, hud_y+45, 190, 35) — host only
        if (am_host) then
            if (mx >= 200 .and. mx <= 390 .and. &
                my >= hud_y+45 .and. my <= hud_y+80) then
                call restart_game()
                return
            end if
        end if
    end subroutine

    ! =====================================================================
    ! End game: send quit message, reveal map
    ! =====================================================================
    subroutine end_game_with_quit()
        ! Send quit action (action_type=8) to opponent
        call send_action(8, 0)

        ! Reveal whole map
        gs%h_visible(1:gs%maze%w, 1:gs%maze%h) = .true.
        gs%s_visible(1:gs%maze%w, 1:gs%maze%h) = .true.
        gs%h_visited(1:gs%maze%w, 1:gs%maze%h) = .true.
        gs%s_visited(1:gs%maze%w, 1:gs%maze%h) = .true.
        gs%game_over = .true.
        game_ended   = .true.
        endgame_msg  = 'Game abandoned'
    end subroutine

    ! =====================================================================
    ! Handle natural game over (seeker caught hider)
    ! =====================================================================
    subroutine handle_natural_game_over()
        ! Reveal whole map for everyone
        gs%h_visible(1:gs%maze%w, 1:gs%maze%h) = .true.
        gs%s_visible(1:gs%maze%w, 1:gs%maze%h) = .true.
        gs%h_visited(1:gs%maze%w, 1:gs%maze%h) = .true.
        gs%s_visited(1:gs%maze%w, 1:gs%maze%h) = .true.
        game_ended = .true.
        if (gs%seeker_won) then
            endgame_msg = 'The seeker wins!'
        else
            endgame_msg = 'The hider wins!'
        end if
    end subroutine

    ! =====================================================================
    ! Try receiving end/restart messages when in end-game state
    ! =====================================================================
    subroutine try_recv_end_message()
        integer(c_int8_t), target :: msg(2)
        integer(c_int) :: got

        call net_try_recv_bytes(game_fd, msg, 2_c_int, got)
        if (got < 2) return

        if (int(msg(1)) == 9) then
            ! Restart signal from host
            recv_init_pos = 0
            game_ready    = .false.
            game_ended    = .false.
            gs%initialized = .false.
        end if
    end subroutine

    ! =====================================================================
    ! Restart game (host only): regenerate and send new init
    ! =====================================================================
    subroutine restart_game()
        if (.not. am_host) return

        ! Send restart signal to client (action_type=9)
        call send_action(9, 0)

        ! Ensure maze dims are odd for nice generation
        if (mod(cfg_maze_w, 2) == 0) cfg_maze_w = cfg_maze_w + 1
        if (mod(cfg_maze_h, 2) == 0) cfg_maze_h = cfg_maze_h + 1

        call game_init(gs, cfg_maze_w, cfg_maze_h, cfg_item_counts, cfg_min_dist, &
                      cfg_speed_h, cfg_speed_s, cfg_vision_h, cfg_vision_s, &
                      cfg_reveal_h, cfg_reveal_s)
        call send_game_init()

        game_ready   = .true.
        game_ended   = .false.
        show_quit_modal = .false.
        print *, '[HOST] game restarted'
    end subroutine

    ! =====================================================================
    ! Game initialisation & network protocol
    ! =====================================================================
    subroutine launch_game_as_host()
        ! Host generates maze and sends init data to client
        am_host = .true.
        am_hider = cfg_host_hides
        game_fd = client_fd
        print *, '[HOST] launch_game_as_host: game_fd=', game_fd

        ! Ensure maze dims are odd for nice generation
        if (mod(cfg_maze_w, 2) == 0) cfg_maze_w = cfg_maze_w + 1
        if (mod(cfg_maze_h, 2) == 0) cfg_maze_h = cfg_maze_h + 1

        call game_init(gs, cfg_maze_w, cfg_maze_h, cfg_item_counts, cfg_min_dist, &
                      cfg_speed_h, cfg_speed_s, cfg_vision_h, cfg_vision_s, &
                      cfg_reveal_h, cfg_reveal_s)
        print *, '[HOST] game_init done, maze=', gs%maze%w, 'x', gs%maze%h, &
                 ' items=', gs%num_items
        call send_game_init()

        game_ready   = .true.
        current_page = PAGE_GAME
        print *, '[HOST] game launched, page=PAGE_GAME'
    end subroutine

    ! Send full game state from host to client (blocking)
    subroutine send_game_init()
        integer(c_int8_t), target :: buf(1024)
        integer :: k, ix, iy, i
        integer(c_int) :: st

        k = 0
        ! Header: maze_w, maze_h, num_items, speed_h, speed_s,
        !         vision_h, vision_s, reveal_h, reveal_s, host_hides  (10 bytes)
        k = k+1; buf(k) = int(gs%maze%w, c_int8_t)
        k = k+1; buf(k) = int(gs%maze%h, c_int8_t)
        k = k+1; buf(k) = int(gs%num_items, c_int8_t)
        k = k+1; buf(k) = int(gs%hider%speed, c_int8_t)
        k = k+1; buf(k) = int(gs%seeker%speed, c_int8_t)
        k = k+1; buf(k) = int(gs%hider%vision_radius, c_int8_t)
        k = k+1; buf(k) = int(gs%seeker%vision_radius, c_int8_t)
        if (gs%h_reveal) then; buf(k+1) = 1_c_int8_t; else; buf(k+1) = 0_c_int8_t; end if; k = k+1
        if (gs%s_reveal) then; buf(k+1) = 1_c_int8_t; else; buf(k+1) = 0_c_int8_t; end if; k = k+1
        if (am_hider) then; buf(k+1) = 1_c_int8_t; else; buf(k+1) = 0_c_int8_t; end if; k = k+1

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

        ! Player positions: hider_x, hider_y, seeker_x, seeker_y
        k = k+1; buf(k) = int(gs%hider%x, c_int8_t)
        k = k+1; buf(k) = int(gs%hider%y, c_int8_t)
        k = k+1; buf(k) = int(gs%seeker%x, c_int8_t)
        k = k+1; buf(k) = int(gs%seeker%y, c_int8_t)

        ! Key position
        k = k+1; buf(k) = int(gs%key_x, c_int8_t)
        k = k+1; buf(k) = int(gs%key_y, c_int8_t)

        call net_send_bytes(game_fd, buf, int(k, c_int), st)
        print *, '[HOST] send_game_init: sent', k, 'bytes, status=', st
    end subroutine

    ! Client: try to receive init data (non-blocking, accumulating)
    subroutine try_recv_game_init()
        integer(c_int8_t), target :: tmp(1024)
        integer(c_int) :: got
        integer :: k, mw, mh, ni, ix, iy, i, needed, sp_h, sp_s
        integer :: vis_h, vis_s
        logical :: rev_h, rev_s, host_hides

        ! Try to receive more data into persistent buffer
        call net_try_recv_bytes(game_fd, tmp, int(1024, c_int), got)
        if (got > 0) then
            if (recv_init_pos + got > 2048) got = 2048 - recv_init_pos
            recv_init_buf(recv_init_pos+1:recv_init_pos+got) = tmp(1:got)
            recv_init_pos = recv_init_pos + got
            print *, '[CLIENT] recv init: got', got, 'bytes, total=', recv_init_pos
        end if

        if (recv_init_pos < 10) return   ! not enough data yet (10-byte header)

        mw   = int(recv_init_buf(1))
        mh   = int(recv_init_buf(2))
        ni   = int(recv_init_buf(3))
        sp_h = int(recv_init_buf(4))
        sp_s = int(recv_init_buf(5))
        vis_h = int(recv_init_buf(6))
        vis_s = int(recv_init_buf(7))
        rev_h = (recv_init_buf(8) /= 0)
        rev_s = (recv_init_buf(9) /= 0)
        host_hides = (recv_init_buf(10) /= 0)
        needed = 10 + mw*mh + ni*3 + 4 + 2   ! +2 for key position

        if (recv_init_pos < needed) then
            print *, '[CLIENT] need', needed, 'bytes, have', recv_init_pos
            return
        end if
        print *, '[CLIENT] full init received:', needed, 'bytes'

        ! Parse maze
        k = 10
        gs%maze%w = mw
        gs%maze%h = mh
        gs%maze%cells = 0
        do iy = 1, mh
            do ix = 1, mw
                k = k+1; gs%maze%cells(ix, iy) = int(recv_init_buf(k))
            end do
        end do

        ! Parse ground items
        gs%num_items = ni
        do i = 1, ni
            k = k+1; gs%items(i)%x = int(recv_init_buf(k))
            k = k+1; gs%items(i)%y = int(recv_init_buf(k))
            k = k+1; gs%items(i)%itype = int(recv_init_buf(k))
            gs%items(i)%active = .true.
        end do

        ! Parse player positions
        k = k+1; gs%hider%x  = int(recv_init_buf(k))
        k = k+1; gs%hider%y  = int(recv_init_buf(k))
        k = k+1; gs%seeker%x = int(recv_init_buf(k))
        k = k+1; gs%seeker%y = int(recv_init_buf(k))

        ! Parse key position
        k = k+1; gs%key_x = int(recv_init_buf(k))
        k = k+1; gs%key_y = int(recv_init_buf(k))
        gs%key_active = .true.

        ! Set up seeker/hider defaults
        gs%hider%vision_radius  = vis_h
        gs%hider%num_items      = 0
        gs%hider%inventory      = ITEM_NONE
        gs%hider%speed          = sp_h
        gs%hider%actions_left   = sp_h
        gs%seeker%vision_radius = vis_s
        gs%seeker%num_items     = 0
        gs%seeker%inventory     = ITEM_NONE
        gs%seeker%speed         = sp_s
        gs%seeker%actions_left  = sp_s

        gs%h_visible       = .false.
        gs%s_visible       = .false.
        gs%h_visited       = .false.
        gs%s_visited       = .false.
        gs%h_item_seen     = .false.
        gs%s_item_seen     = .false.
        gs%h_last_opp_x    = 0
        gs%h_last_opp_y    = 0
        gs%s_last_opp_x    = 0
        gs%s_last_opp_y    = 0
        gs%h_opp_seen      = .false.
        gs%s_opp_seen      = .false.
        gs%h_illuminate    = .false.
        gs%s_illuminate    = .false.
        gs%h_reveal        = rev_h
        gs%s_reveal        = rev_s
        gs%turn            = 0
        gs%turn_number     = 1
        gs%game_over       = .false.
        gs%seeker_won      = .false.
        gs%input_state     = INPUT_MOVE
        gs%selected_slot   = 0
        gs%initialized     = .true.

        call game_compute_visibility(gs)
        ! Client role: opposite of host's choice
        am_hider = .not. host_hides
        game_ready = .true.
    end subroutine

    ! Send an action to the opponent: 2 bytes [action_type, param]
    ! action_type: 0 = move (param = direction)
    !              1-6 = use item from slot (param = direction or 0)
    subroutine send_action(action_type, param)
        integer, intent(in) :: action_type, param
        integer(c_int8_t), target :: msg(2)
        integer(c_int) :: st
        msg(1) = int(action_type, c_int8_t)
        msg(2) = int(param, c_int8_t)
        call net_send_bytes(game_fd, msg, 2_c_int, st)
    end subroutine

    ! Try to receive opponent's action (non-blocking)
    subroutine try_recv_opponent_action()
        integer(c_int8_t), target :: msg(2)
        integer(c_int) :: got
        integer :: atype, param, opp_role
        logical :: moved

        call net_try_recv_bytes(game_fd, msg, 2_c_int, got)
        if (got < 2) return

        atype = int(msg(1))
        param = int(msg(2))

        if (am_hider) then
            opp_role = ROLE_SEEKER
        else
            opp_role = ROLE_HIDER
        end if

        if (atype == 0) then
            ! Move
            moved = game_do_move(gs, opp_role, param)
        else if (atype == 7) then
            ! Pass turn
            call game_do_pass(gs)
        else if (atype == 8) then
            ! Opponent quit
            gs%h_visible(1:gs%maze%w, 1:gs%maze%h) = .true.
            gs%s_visible(1:gs%maze%w, 1:gs%maze%h) = .true.
            gs%h_visited(1:gs%maze%w, 1:gs%maze%h) = .true.
            gs%s_visited(1:gs%maze%w, 1:gs%maze%h) = .true.
            gs%game_over = .true.
            game_ended   = .true.
            endgame_msg  = 'Opponent has quit'
            return
        else
            ! Item use: atype = slot, param = direction
            call game_do_use_item(gs, opp_role, atype, param)
        end if

        call game_end_turn(gs)

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
            case (PAGE_MENU); call render_menu()
            case (PAGE_HOST); call render_host()
            case (PAGE_JOIN); call render_join()
            case (PAGE_GAME); call render_game()
        end select

        call sdl_render_present(main_renderer)
    end subroutine

    ! --- Menu ---
    subroutine render_menu()
        call draw_text_centered('ForPlay', title_font, COL_CYAN, SCREEN_W/2, 80)
        call draw_text_centered('Choose an option', sml_font, COL_GRAY, &
                                SCREEN_W/2, 170)
        call draw_button(250, 250, 300, 60, 'Host a game', COL_GREEN)
        call draw_button(250, 350, 300, 60, 'Join a game', COL_CYAN)
        if (len_trim(error_message)>0) &
            call draw_text_centered(trim(error_message), sml_font, COL_RED, &
                                    SCREEN_W/2, 470)
    end subroutine

    ! --- Host ---
    subroutine render_host()
        character(len=256) :: info
        integer :: py, ly, ry
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
        call draw_toggle_row_at(20, ly, 'Host hides', cfg_host_hides); ly = ly + 26

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

        ! ===== RIGHT COLUMN =====
        ! --- Hider block ---
        call draw_text_at(sml_font, '--- Hider ---', COL_GRAY, 410, 180)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(80), uint8(80), uint8(100), uint8(255))
        rc = sdl_render_draw_line(main_renderer, 410, 200, 770, 200)

        ry = 224
        call draw_param_row_at(410, ry, 'Speed',  cfg_speed_h);  ry = ry + 26
        call draw_param_row_at(410, ry, 'Vision', cfg_vision_h); ry = ry + 26
        call draw_toggle_row_at(410, ry, 'Maze revealed', cfg_reveal_h); ry = ry + 26

        ! --- Seeker block ---
        ry = ry + 8
        call draw_text_at(sml_font, '--- Seeker ---', COL_GRAY, 410, ry)
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
            call draw_button(250, py, 300, 50, 'Start game', COL_GREEN)
        end if
        call draw_button(300, py + 60, 200, 40, 'Back', COL_RED)
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
            ! General block: rows at y=224,250,276,302
            if (my >= 211 .and. my <= 237) tip = 'Number of columns (odd recommended)'
            if (my >= 237 .and. my <= 263) tip = 'Number of rows (odd recommended)'
            if (my >= 263 .and. my <= 289) tip = 'Min distance in cells between spawns'
            if (my >= 289 .and. my <= 315) tip = 'Host plays as the hider (Yes) or seeker (No)'
            ! Bonuses block: rows at y=374,400,426,452
            if (my >= 361 .and. my <= 387) tip = 'Dash forward in a straight line, reveals path'
            if (my >= 387 .and. my <= 413) tip = 'Vision radius +1 to +3 (random, permanent)'
            if (my >= 413 .and. my <= 439) tip = 'Reveals the entire map for this turn'
            if (my >= 439 .and. my <= 465) tip = '+1 action per turn (permanent)'
        end if

        ! --- Right column (x 410..770) ---
        if (mx >= 410 .and. mx <= 770) then
            ! Hider block: rows at y=224,250,276
            if (my >= 211 .and. my <= 237) tip = 'Actions per turn for the hider'
            if (my >= 237 .and. my <= 263) tip = 'Vision radius at game start for the hider'
            if (my >= 263 .and. my <= 289) tip = 'Hider sees the full maze layout from start'
            ! Seeker block: rows at y=348,374,400
            if (my >= 335 .and. my <= 361) tip = 'Actions per turn for the seeker'
            if (my >= 361 .and. my <= 387) tip = 'Vision radius at game start for the seeker'
            if (my >= 387 .and. my <= 413) tip = 'Seeker sees the full maze layout from start'
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
        call draw_text_centered('Join a game', big_font, COL_CYAN, &
                                SCREEN_W/2, 60)
        call draw_text_at(sml_font, 'IP Address:', COL_WHITE, 250, 195)
        dt = ' '; if (len_trim(input_ip)>0) dt = trim(input_ip)
        call draw_input_field(250, 220, 350, 40, trim(dt), active_field==0)
        call draw_text_at(sml_font, 'Port:', COL_WHITE, 250, 285)
        dt = ' '; if (len_trim(input_port)>0) dt = trim(input_port)
        call draw_input_field(250, 310, 350, 40, trim(dt), active_field==1)
        if (connecting) then
            call draw_button(250, 400, 300, 50, 'Connecting...', COL_GRAY)
        else
            call draw_button(250, 400, 300, 50, 'Connect', COL_GREEN)
        end if
        if (len_trim(error_message)>0) &
            call draw_text_centered(trim(error_message), sml_font, COL_RED, &
                                    SCREEN_W/2, 470)
        call draw_button(300, 500, 200, 50, 'Back', COL_RED)
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
        logical :: is_vis, is_mem, show_cell, my_reveal
        integer :: i

        call get_maze_layout(csz, ox, oy)

        ! Determine if the local player has maze reveal
        if (am_hider) then
            my_reveal = gs%h_reveal
        else
            my_reveal = gs%s_reveal
        end if

        do iy = 1, gs%maze%h
            do ix = 1, gs%maze%w
                ! Determine visibility for the current player's view
                if (am_hider) then
                    is_vis = gs%h_visible(ix, iy)
                    is_mem = gs%h_visited(ix, iy)
                else
                    is_vis = gs%s_visible(ix, iy)
                    is_mem = gs%s_visited(ix, iy)
                end if

                ! show_cell: if maze is revealed, always show walls
                if (my_reveal) then
                    show_cell = .true.
                else
                    show_cell = is_vis .or. is_mem
                end if

                if (.not. show_cell) cycle   ! unseen cell

                sx = ox + (ix - 1) * csz
                sy = oy + (iy - 1) * csz
                c  = gs%maze%cells(ix, iy)

                ! Draw floor
                call draw_cell_floor(sx, sy, csz, is_vis)

                ! Draw walls (bright if visible or if maze is revealed)
                if (my_reveal) then
                    call draw_cell_walls(sx, sy, csz, c, .true.)
                else
                    call draw_cell_walls(sx, sy, csz, c, is_vis)
                end if

                ! Draw ground items
                if (is_vis) then
                    ! Currently visible: show real state
                    do i = 1, gs%num_items
                        if (gs%items(i)%active .and. &
                            gs%items(i)%x == ix .and. gs%items(i)%y == iy) then
                            call draw_ground_item(sx, sy, csz, gs%items(i)%itype)
                        end if
                    end do
                    ! Draw key if visible and active
                    if (gs%key_active .and. gs%key_x == ix .and. gs%key_y == iy) then
                        call draw_key(sx, sy, csz)
                    end if
                else
                    ! Not visible: show dimmed item if previously seen
                    do i = 1, gs%num_items
                        if (gs%items(i)%x == ix .and. gs%items(i)%y == iy) then
                            if (am_hider) then
                                if (gs%h_item_seen(i)) &
                                    call draw_ground_item_dimmed(sx, sy, csz, gs%items(i)%itype)
                            else
                                if (gs%s_item_seen(i)) &
                                    call draw_ground_item_dimmed(sx, sy, csz, gs%items(i)%itype)
                            end if
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
        integer :: csz, ox, oy, sx, sy, pad
        type(sdl_rect) :: pr
        logical :: opp_vis

        call get_maze_layout(csz, ox, oy)
        pad = csz / 5
        if (pad < 2) pad = 2

        if (am_hider) then
            ! I'm the hider -- always draw myself (red)
            sx = ox + (gs%hider%x - 1) * csz
            sy = oy + (gs%hider%y - 1) * csz
            pr = sdl_rect(sx + pad, sy + pad, csz - 2*pad, csz - 2*pad)
            rc = sdl_set_render_draw_color(main_renderer, &
                    uint8(220), uint8(50), uint8(50), uint8(255))
            rc = sdl_render_fill_rect(main_renderer, pr)

            ! Draw seeker (opponent) -- real or ghost
            opp_vis = gs%h_visible(gs%seeker%x, gs%seeker%y)
            if (opp_vis) then
                sx = ox + (gs%seeker%x - 1) * csz
                sy = oy + (gs%seeker%y - 1) * csz
                pr = sdl_rect(sx + pad, sy + pad, csz - 2*pad, csz - 2*pad)
                rc = sdl_set_render_draw_color(main_renderer, &
                        uint8(50), uint8(120), uint8(255), uint8(255))
                rc = sdl_render_fill_rect(main_renderer, pr)
            else if (gs%h_opp_seen) then
                ! Ghost at last known position
                sx = ox + (gs%h_last_opp_x - 1) * csz
                sy = oy + (gs%h_last_opp_y - 1) * csz
                pr = sdl_rect(sx + pad, sy + pad, csz - 2*pad, csz - 2*pad)
                rc = sdl_set_render_draw_color(main_renderer, &
                        uint8(50), uint8(80), uint8(140), uint8(120))
                rc = sdl_render_fill_rect(main_renderer, pr)
            end if
        else
            ! I'm the seeker -- always draw myself (blue)
            sx = ox + (gs%seeker%x - 1) * csz
            sy = oy + (gs%seeker%y - 1) * csz
            pr = sdl_rect(sx + pad, sy + pad, csz - 2*pad, csz - 2*pad)
            rc = sdl_set_render_draw_color(main_renderer, &
                    uint8(50), uint8(120), uint8(255), uint8(255))
            rc = sdl_render_fill_rect(main_renderer, pr)

            ! Draw hider (opponent) -- real or ghost
            opp_vis = gs%s_visible(gs%hider%x, gs%hider%y)
            if (opp_vis) then
                sx = ox + (gs%hider%x - 1) * csz
                sy = oy + (gs%hider%y - 1) * csz
                pr = sdl_rect(sx + pad, sy + pad, csz - 2*pad, csz - 2*pad)
                rc = sdl_set_render_draw_color(main_renderer, &
                        uint8(220), uint8(50), uint8(50), uint8(255))
                rc = sdl_render_fill_rect(main_renderer, pr)
            else if (gs%s_opp_seen) then
                ! Ghost at last known position
                sx = ox + (gs%s_last_opp_x - 1) * csz
                sy = oy + (gs%s_last_opp_y - 1) * csz
                pr = sdl_rect(sx + pad, sy + pad, csz - 2*pad, csz - 2*pad)
                rc = sdl_set_render_draw_color(main_renderer, &
                        uint8(140), uint8(50), uint8(50), uint8(120))
                rc = sdl_render_fill_rect(main_renderer, pr)
            end if
        end if
    end subroutine

    ! -----------------------------------------------------------------
    ! HUD rendering (bottom bar: inventory + info)
    ! -----------------------------------------------------------------
    subroutine render_hud()
        integer :: hud_y, i, bx, ni, itype, spd, act
        type(sdl_rect) :: hr, ir
        character(len=64) :: label
        type(sdl_color) :: icol
        character(len=1) :: num_ch

        hud_y = GAME_AREA_H

        ! HUD background
        hr = sdl_rect(0, hud_y, SCREEN_W, HUD_H)
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(30), uint8(30), uint8(40), uint8(255))
        rc = sdl_render_fill_rect(main_renderer, hr)

        ! Top border
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(100), uint8(100), uint8(120), uint8(255))
        rc = sdl_render_draw_line(main_renderer, 0, hud_y, SCREEN_W, hud_y)

        ! Show role
        if (am_hider) then
            call draw_text_at(sml_font, 'Role: Hider', COL_RED, 15, hud_y + 5)
        else
            call draw_text_at(sml_font, 'Role: Seeker', COL_CYAN, 15, hud_y + 5)
        end if

        ! Show inventory
        if (am_hider) then
            ni = gs%hider%num_items
        else
            ni = gs%seeker%num_items
        end if

        call draw_text_at(sml_font, 'Items:', COL_GRAY, 15, hud_y + 30)
        bx = 110
        do i = 1, MAX_INVENTORY
            ! Draw slot
            ir = sdl_rect(bx, hud_y + 28, 50, 28)
            if (i <= ni) then
                if (am_hider) then
                    itype = gs%hider%inventory(i)
                else
                    itype = gs%seeker%inventory(i)
                end if
                if (itype == ITEM_VISION .or. itype == ITEM_SPEED) then
                    ! Skip drawing vision/speed (should never be present, but just in case)
                    cycle
                end if
                call item_color(itype, icol)
                rc = sdl_set_render_draw_color(main_renderer, &
                        icol%r, icol%g, icol%b, uint8(200))
                rc = sdl_render_fill_rect(main_renderer, ir)
                ! Item letter + slot number
                write(num_ch, '(I1)') i
                call draw_text_centered(num_ch, sml_font, COL_WHITE, &
                                        bx + 25, hud_y + 30)
                ! Highlight selected item slot
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
            ! Slot border
            rc = sdl_set_render_draw_color(main_renderer, &
                    uint8(80), uint8(80), uint8(90), uint8(255))
            rc = sdl_render_draw_rect(main_renderer, ir)
            bx = bx + 56
        end do

        ! --- End-game HUD: buttons replace normal right-side info ---
        if (game_ended) then
            ! Show turn count
            write(label, '(A,I0)') 'Turn: ', gs%turn_number
            call draw_text_at(sml_font, trim(label), COL_GRAY, 470, hud_y + 5)

            ! Buttons inside HUD
            call draw_button(15, hud_y + 45, 170, 35, 'Back to menu', COL_RED)
            if (am_host) then
                call draw_button(200, hud_y + 45, 190, 35, 'New game', COL_GREEN)
            else
                call draw_text_at(sml_font, 'Waiting for host...', &
                                  COL_DIM, 200, hud_y + 52)
            end if
            call draw_text_at(sml_font, 'Esc = Menu', COL_DIM, 470, hud_y + 52)
            return
        end if

        ! --- Normal HUD right-side info ---
        ! Turn indicator + actions remaining (bottom-left, under items)
        if (is_my_turn() .and. .not. gs%game_over) then
            if (am_hider) then
                spd = gs%hider%speed; act = gs%hider%actions_left
            else
                spd = gs%seeker%speed; act = gs%seeker%actions_left
            end if
            if (spd > 1) then
                write(label, '(A,I0,A,I0,A)') &
                    'YOUR TURN  (', act, '/', spd, ' actions)'
            else
                label = 'YOUR TURN'
            end if
            call draw_text_at(sml_font, trim(label), COL_GREEN, 15, hud_y + 62)
        else if (.not. gs%game_over) then
            call draw_text_at(sml_font, 'Opponent''s turn...', COL_GRAY, 15, hud_y + 62)
        end if

        ! Vision radius + speed
        if (am_hider) then
            write(label, '(A,I0)') 'Vision: ', gs%hider%vision_radius
            call draw_text_at(sml_font, trim(label), COL_GRAY, 470, hud_y + 30)
            if (gs%hider%speed > 1) then
                write(label, '(A,I0)') 'Speed: ', gs%hider%speed
                call draw_text_at(sml_font, trim(label), COL_GRAY, 620, hud_y + 30)
            end if
        else
            write(label, '(A,I0)') 'Vision: ', gs%seeker%vision_radius
            call draw_text_at(sml_font, trim(label), COL_GRAY, 470, hud_y + 30)
            if (gs%seeker%speed > 1) then
                write(label, '(A,I0)') 'Speed: ', gs%seeker%speed
                call draw_text_at(sml_font, trim(label), COL_GRAY, 620, hud_y + 30)
            end if
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
            case default;           col = COL_GRAY
        end select
    end subroutine

    ! -----------------------------------------------------------------
    ! Tooltip for items: HUD inventory slots + maze floor items
    ! -----------------------------------------------------------------
    subroutine render_game_tooltip()
        integer(kind=c_int) :: mx_c, my_c
        integer(kind=c_uint32_t) :: btn_state
        integer :: mx, my, i, slot, itype, ni
        integer :: csz, ox, oy, cell_x, cell_y
        character(len=80) :: tip
        type(sdl_surface), pointer :: surf
        type(c_ptr) :: tex
        type(sdl_rect) :: bgr
        integer :: tw

        btn_state = sdl_get_mouse_state(mx_c, my_c)
        mx = int(mx_c); my = int(my_c)
        tip = ' '

        ! --- Check HUD inventory slots ---
        ! Slots start at x=110, y=GAME_AREA_H+28, each 50 wide, 28 tall, spaced 56px
        if (my >= GAME_AREA_H + 28 .and. my <= GAME_AREA_H + 56) then
            if (am_hider) then
                ni = gs%hider%num_items
            else
                ni = gs%seeker%num_items
            end if
            do slot = 1, ni
                if (mx >= 110 + (slot-1)*56 .and. mx < 110 + (slot-1)*56 + 50) then
                    if (am_hider) then
                        itype = gs%hider%inventory(slot)
                    else
                        itype = gs%seeker%inventory(slot)
                    end if
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
                    ! Show tooltip for items in currently visible cells
                    if ((am_hider .and. gs%h_visible(cell_x, cell_y)) .or. &
                        (.not. am_hider .and. gs%s_visible(cell_x, cell_y))) then
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
                    ! Also show tooltip for remembered items in fog of war
                    else if ((am_hider .and. gs%h_visited(cell_x, cell_y)) .or. &
                             (.not. am_hider .and. gs%s_visited(cell_x, cell_y))) then
                        do i = 1, gs%num_items
                            if (gs%items(i)%x == cell_x .and. &
                                gs%items(i)%y == cell_y) then
                                if ((am_hider .and. gs%h_item_seen(i)) .or. &
                                    (.not. am_hider .and. gs%s_item_seen(i))) then
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

        ! Darken background
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(0), uint8(0), uint8(0), uint8(180))
        ovr = sdl_rect(150, 200, 500, 140)
        rc = sdl_render_fill_rect(main_renderer, ovr)

        ! Border
        rc = sdl_set_render_draw_color(main_renderer, &
                uint8(200), uint8(100), uint8(100), uint8(255))
        rc = sdl_render_draw_rect(main_renderer, ovr)

        call draw_text_centered('Quit the game?', big_font, COL_YELLOW, &
                                SCREEN_W/2, 215)
        call draw_text_centered('The map will be revealed to both players', &
                                sml_font, COL_GRAY, SCREEN_W/2, 260)
        call draw_text_centered('[Y] Confirm   /   Any other key = Cancel', &
                                sml_font, COL_WHITE, SCREEN_W/2, 300)
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
