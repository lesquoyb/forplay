! game.f90
!
! Game logic module for the asymmetric maze game.
!
! Roles:
!   ROLE_HIDER  - sees the full maze, tries to evade the seeker
!   ROLE_SEEKER - limited visibility (fog of war), must find the hider
!
! Turn structure:
!   Each turn, the active player can EITHER move one cell OR use an item.
!   Hider goes first, then seeker, alternating.
!
! Items (picked up from the ground):
!   ITEM_DASH       - dash in a straight line until hitting a wall; lights
!                     up the entire neighbourhood of the path traversed
!   ITEM_VISION     - permanently increases vision radius by +1 to +3
!   ITEM_ILLUMINATE - reveals the entire maze for the current turn

module game_mod
    use :: maze_mod
    implicit none
    private

    ! --- Public API ---
    public :: game_state, player_type, ground_item_type
    public :: ITEM_NONE, ITEM_DASH, ITEM_VISION, ITEM_ILLUMINATE, ITEM_SPEED
    public :: NUM_ITEM_TYPES, MAX_INVENTORY, MAX_GROUND_ITEMS
    public :: ROLE_HIDER, ROLE_SEEKER
    public :: INPUT_MOVE, INPUT_ITEM_DIR, INPUT_WAIT
    public :: game_init, game_do_move, game_do_use_item
    public :: game_compute_visibility, game_end_turn
    public :: game_check_win, game_item_needs_direction
    public :: game_item_name, game_item_description, game_do_pass

    ! --- Item types ---
    integer, parameter :: ITEM_NONE       = 0
    integer, parameter :: ITEM_DASH       = 1
    integer, parameter :: ITEM_VISION     = 2
    integer, parameter :: ITEM_ILLUMINATE = 3
    integer, parameter :: ITEM_SPEED      = 4
    integer, parameter :: NUM_ITEM_TYPES  = 4

    ! --- Player roles ---
    integer, parameter :: ROLE_HIDER  = 0
    integer, parameter :: ROLE_SEEKER = 1

    ! --- Input states ---
    integer, parameter :: INPUT_MOVE     = 0   ! normal: arrows move, 1-6 select item
    integer, parameter :: INPUT_ITEM_DIR = 1   ! waiting for direction for selected item
    integer, parameter :: INPUT_WAIT     = 2   ! not my turn, waiting for opponent

    ! --- Limits ---
    integer, parameter :: MAX_INVENTORY    = 6
    integer, parameter :: MAX_GROUND_ITEMS = 30

    ! --- Types ---
    type :: ground_item_type
        integer :: x      = 0
        integer :: y      = 0
        integer :: itype  = ITEM_NONE
        logical :: active = .false.
    end type

    type :: player_type
        integer :: x            = 0
        integer :: y            = 0
        integer :: inventory(MAX_INVENTORY) = ITEM_NONE
        integer :: num_items    = 0
        integer :: vision_radius = 1
        integer :: speed        = 1   ! actions per turn
        integer :: actions_left = 1   ! remaining actions this turn
    end type

    type :: game_state
        type(maze_type)        :: maze
        type(player_type)      :: hider
        type(player_type)      :: seeker
        ! Per-player visibility (current vision + memory)
        logical :: h_visible(MAZE_MAX_W, MAZE_MAX_H) = .false.
        logical :: s_visible(MAZE_MAX_W, MAZE_MAX_H) = .false.
        logical :: h_visited(MAZE_MAX_W, MAZE_MAX_H) = .false.
        logical :: s_visited(MAZE_MAX_W, MAZE_MAX_H) = .false.
        ! Per-player item memory (true while last seen active)
        logical :: h_item_seen(MAX_GROUND_ITEMS) = .false.
        logical :: s_item_seen(MAX_GROUND_ITEMS) = .false.
        ! Last-known opponent position (ghost tracking)
        integer :: h_last_opp_x = 0
        integer :: h_last_opp_y = 0
        integer :: s_last_opp_x = 0
        integer :: s_last_opp_y = 0
        logical :: h_opp_seen   = .false.
        logical :: s_opp_seen   = .false.
        ! Per-player illuminate
        logical :: h_illuminate = .false.
        logical :: s_illuminate = .false.
        ! General state
        integer :: turn         = 0      ! 0 = hider's turn, 1 = seeker's turn
        integer :: turn_number  = 1
        logical :: game_over    = .false.
        logical :: seeker_won   = .false.
        type(ground_item_type) :: items(MAX_GROUND_ITEMS)
        integer :: num_items    = 0
        integer :: input_state  = INPUT_MOVE
        integer :: selected_slot = 0     ! inventory slot for directional item
        logical :: initialized  = .false.
    end type

contains

    ! =========================================================================
    ! Initialize a new game: generate maze, place items and players.
    ! min_dist: minimum BFS distance between hider and seeker spawns.
    ! =========================================================================
    subroutine game_init(gs, maze_w, maze_h, item_counts, min_dist, speed_h, speed_s)
        type(game_state), intent(out) :: gs
        integer, intent(in) :: maze_w, maze_h, min_dist, speed_h, speed_s
        integer, intent(in) :: item_counts(NUM_ITEM_TYPES)

        integer :: i, j, x, y, itype, attempts, d
        real    :: rval
        logical :: occupied(MAZE_MAX_W, MAZE_MAX_H)
        integer :: hx, hy, sx, sy

        ! Generate the maze
        call random_seed()
        call maze_generate(gs%maze, maze_w, maze_h)

        ! Place hider at a random cell
        call random_number(rval); hx = 1 + int(rval * maze_w)
        if (hx > maze_w) hx = maze_w
        call random_number(rval); hy = 1 + int(rval * maze_h)
        if (hy > maze_h) hy = maze_h

        ! Place seeker ensuring minimum BFS distance
        attempts = 0
        do
            call random_number(rval); sx = 1 + int(rval * maze_w)
            if (sx > maze_w) sx = maze_w
            call random_number(rval); sy = 1 + int(rval * maze_h)
            if (sy > maze_h) sy = maze_h

            if (sx == hx .and. sy == hy) cycle

            d = maze_shortest_path(gs%maze, hx, hy, sx, sy)
            if (d >= min_dist) exit

            attempts = attempts + 1
            ! After many failures, regenerate the maze and hider spot
            if (attempts > 200) then
                call maze_generate(gs%maze, maze_w, maze_h)
                call random_number(rval); hx = 1 + int(rval * maze_w)
                if (hx > maze_w) hx = maze_w
                call random_number(rval); hy = 1 + int(rval * maze_h)
                if (hy > maze_h) hy = maze_h
                attempts = 0
            end if
        end do

        gs%hider%x = hx
        gs%hider%y = hy
        gs%hider%vision_radius = 2   ! limited vision for items/players
        gs%hider%num_items = 0
        gs%hider%inventory = ITEM_NONE
        gs%hider%speed = speed_h
        gs%hider%actions_left = speed_h

        gs%seeker%x = sx
        gs%seeker%y = sy
        gs%seeker%vision_radius = 1
        gs%seeker%num_items = 0
        gs%seeker%inventory = ITEM_NONE
        gs%seeker%speed = speed_s
        gs%seeker%actions_left = speed_s

        ! Track occupied cells (players + items)
        occupied = .false.
        occupied(gs%hider%x, gs%hider%y) = .true.
        occupied(gs%seeker%x, gs%seeker%y) = .true.

        ! Place items per type according to item_counts
        gs%num_items = 0
        do itype = 1, NUM_ITEM_TYPES
            do j = 1, item_counts(itype)
                if (gs%num_items >= MAX_GROUND_ITEMS) exit

                ! Find a random unoccupied cell
                do
                    call random_number(rval)
                    x = 1 + int(rval * maze_w)
                    if (x > maze_w) x = maze_w
                    call random_number(rval)
                    y = 1 + int(rval * maze_h)
                    if (y > maze_h) y = maze_h
                    if (.not. occupied(x, y)) exit
                end do

                gs%num_items = gs%num_items + 1
                gs%items(gs%num_items)%x     = x
                gs%items(gs%num_items)%y     = y
                gs%items(gs%num_items)%itype = itype
                gs%items(gs%num_items)%active = .true.
                occupied(x, y) = .true.
            end do
        end do

        ! Initialize per-player visibility and memory
        gs%h_visible = .false.; gs%s_visible = .false.
        gs%h_visited = .false.; gs%s_visited = .false.
        gs%h_item_seen = .false.; gs%s_item_seen = .false.
        gs%h_last_opp_x = 0; gs%h_last_opp_y = 0
        gs%s_last_opp_x = 0; gs%s_last_opp_y = 0
        gs%h_opp_seen = .false.; gs%s_opp_seen = .false.
        gs%h_illuminate = .false.; gs%s_illuminate = .false.
        gs%turn        = 0          ! hider goes first
        gs%turn_number = 1
        gs%game_over   = .false.
        gs%seeker_won  = .false.
        gs%input_state = INPUT_MOVE
        gs%selected_slot = 0
        gs%initialized = .true.

        call game_compute_visibility(gs)
    end subroutine

    ! =========================================================================
    ! Move the active player one cell in direction d.
    ! Returns .true. if the move was valid.
    ! =========================================================================
    function game_do_move(gs, role, d) result(moved)
        type(game_state), intent(inout) :: gs
        integer, intent(in) :: role, d
        logical :: moved

        integer :: dx, dy

        moved = .false.

        if (role == ROLE_HIDER) then
            if (.not. maze_can_move(gs%maze, gs%hider%x, gs%hider%y, d)) return
            call maze_move_delta(d, dx, dy)
            gs%hider%x = gs%hider%x + dx
            gs%hider%y = gs%hider%y + dy
            call pickup_items(gs, gs%hider)
        else
            if (.not. maze_can_move(gs%maze, gs%seeker%x, gs%seeker%y, d)) return
            call maze_move_delta(d, dx, dy)
            gs%seeker%x = gs%seeker%x + dx
            gs%seeker%y = gs%seeker%y + dy
            call pickup_items(gs, gs%seeker)
        end if

        moved = .true.
        call game_compute_visibility(gs)
        call game_check_win(gs)
    end function

    ! =========================================================================
    ! Use an item from the player's inventory.
    ! slot  = inventory index (1..num_items)
    ! d     = direction (for directional items like dash; ignored otherwise)
    ! =========================================================================
    subroutine game_do_use_item(gs, role, slot, d)
        type(game_state), intent(inout) :: gs
        integer, intent(in) :: role, slot, d

        integer :: itype, dx, dy, boost
        real    :: rval

        if (role == ROLE_HIDER) then
            if (slot < 1 .or. slot > gs%hider%num_items) return
            itype = gs%hider%inventory(slot)
        else
            if (slot < 1 .or. slot > gs%seeker%num_items) return
            itype = gs%seeker%inventory(slot)
        end if
        if (itype == ITEM_NONE) return

        select case (itype)

            case (ITEM_DASH)
                call maze_move_delta(d, dx, dy)
                if (role == ROLE_HIDER) then
                    do while (maze_can_move(gs%maze, gs%hider%x, gs%hider%y, d))
                        gs%hider%x = gs%hider%x + dx
                        gs%hider%y = gs%hider%y + dy
                        call reveal_area(gs, ROLE_HIDER, gs%hider%x, gs%hider%y, 1)
                        call pickup_items(gs, gs%hider)
                    end do
                else
                    do while (maze_can_move(gs%maze, gs%seeker%x, gs%seeker%y, d))
                        gs%seeker%x = gs%seeker%x + dx
                        gs%seeker%y = gs%seeker%y + dy
                        call reveal_area(gs, ROLE_SEEKER, gs%seeker%x, gs%seeker%y, 1)
                        call pickup_items(gs, gs%seeker)
                    end do
                end if

            case (ITEM_VISION)
                call random_number(rval)
                boost = 1 + int(rval * 3.0)
                if (boost > 3) boost = 3
                if (role == ROLE_HIDER) then
                    gs%hider%vision_radius = gs%hider%vision_radius + boost
                else
                    gs%seeker%vision_radius = gs%seeker%vision_radius + boost
                end if

            case (ITEM_ILLUMINATE)
                if (role == ROLE_HIDER) then
                    gs%h_illuminate = .true.
                else
                    gs%s_illuminate = .true.
                end if

            case (ITEM_SPEED)
                if (role == ROLE_HIDER) then
                    gs%hider%speed = gs%hider%speed + 1
                else
                    gs%seeker%speed = gs%seeker%speed + 1
                end if

        end select

        ! Remove item from inventory
        if (role == ROLE_HIDER) then
            call remove_item(gs%hider, slot)
        else
            call remove_item(gs%seeker, slot)
        end if

        call game_compute_visibility(gs)
        call game_check_win(gs)
    end subroutine

    ! =========================================================================
    ! Does the given item type need a direction? (e.g. dash)
    ! =========================================================================
    function game_item_needs_direction(itype) result(needs)
        integer, intent(in) :: itype
        logical :: needs
        needs = (itype == ITEM_DASH)
    end function

    ! =========================================================================
    ! Get a short name string for an item type.
    ! =========================================================================
    function game_item_name(itype) result(name)
        integer, intent(in) :: itype
        character(len=8) :: name
        select case (itype)
            case (ITEM_DASH);       name = 'Dash'
            case (ITEM_VISION);     name = 'Vision'
            case (ITEM_ILLUMINATE); name = 'Light'
            case (ITEM_SPEED);      name = 'Speed'
            case default;           name = '???'
        end select
    end function

    ! =========================================================================
    ! Get a detailed description for an item type (tooltip).
    ! =========================================================================
    function game_item_description(itype) result(desc)
        integer, intent(in) :: itype
        character(len=64) :: desc
        select case (itype)
            case (ITEM_DASH)
                desc = 'Dash forward in a straight line, reveals path'
            case (ITEM_VISION)
                desc = 'Vision radius +1 to +3 (random, permanent)'
            case (ITEM_ILLUMINATE)
                desc = 'Reveals the entire map for this turn'
            case (ITEM_SPEED)
                desc = '+1 action per turn (permanent)'
            case default
                desc = ' '
        end select
    end function

    ! =========================================================================
    ! Compute which cells are currently visible for BOTH players.
    ! Also updates opponent tracking and item memory.
    ! =========================================================================
    subroutine game_compute_visibility(gs)
        type(game_state), intent(inout) :: gs
        integer :: px, py, r, dx, dy, nx, ny, i, w, h

        w = gs%maze%w
        h = gs%maze%h

        ! --- Hider visibility ---
        gs%h_visible = .false.
        if (gs%h_illuminate) then
            gs%h_visible(1:w, 1:h) = .true.
            gs%h_visited(1:w, 1:h) = .true.
        else
            px = gs%hider%x; py = gs%hider%y
            r  = gs%hider%vision_radius
            do dy = -r, r
                do dx = -r, r
                    nx = px + dx; ny = py + dy
                    if (nx >= 1 .and. nx <= w .and. &
                        ny >= 1 .and. ny <= h) then
                        gs%h_visible(nx, ny) = .true.
                        gs%h_visited(nx, ny) = .true.
                    end if
                end do
            end do
        end if

        ! --- Seeker visibility ---
        gs%s_visible = .false.
        if (gs%s_illuminate) then
            gs%s_visible(1:w, 1:h) = .true.
            gs%s_visited(1:w, 1:h) = .true.
        else
            px = gs%seeker%x; py = gs%seeker%y
            r  = gs%seeker%vision_radius
            do dy = -r, r
                do dx = -r, r
                    nx = px + dx; ny = py + dy
                    if (nx >= 1 .and. nx <= w .and. &
                        ny >= 1 .and. ny <= h) then
                        gs%s_visible(nx, ny) = .true.
                        gs%s_visited(nx, ny) = .true.
                    end if
                end do
            end do
        end if

        ! --- Update opponent tracking ---
        ! Hider tracks seeker
        if (gs%h_visible(gs%seeker%x, gs%seeker%y)) then
            gs%h_last_opp_x = gs%seeker%x
            gs%h_last_opp_y = gs%seeker%y
            gs%h_opp_seen = .true.
        end if
        ! Seeker tracks hider
        if (gs%s_visible(gs%hider%x, gs%hider%y)) then
            gs%s_last_opp_x = gs%hider%x
            gs%s_last_opp_y = gs%hider%y
            gs%s_opp_seen = .true.
        end if

        ! --- Update per-player item memory ---
        do i = 1, gs%num_items
            if (gs%h_visible(gs%items(i)%x, gs%items(i)%y)) then
                gs%h_item_seen(i) = gs%items(i)%active
            end if
            if (gs%s_visible(gs%items(i)%x, gs%items(i)%y)) then
                gs%s_item_seen(i) = gs%items(i)%active
            end if
        end do
    end subroutine

    ! =========================================================================
    ! End the current turn: switch active player, reset per-turn effects.
    ! =========================================================================
    subroutine game_end_turn(gs)
        type(game_state), intent(inout) :: gs
        integer :: remaining

        ! Decrement actions remaining for current player
        if (gs%turn == 0) then
            gs%hider%actions_left = gs%hider%actions_left - 1
            remaining = gs%hider%actions_left
        else
            gs%seeker%actions_left = gs%seeker%actions_left - 1
            remaining = gs%seeker%actions_left
        end if

        if (remaining > 0) then
            ! Player still has actions this turn
            gs%input_state   = INPUT_MOVE
            gs%selected_slot = 0
            return
        end if

        ! No more actions: end the real turn
        ! Reset illuminate at end of turn
        if (gs%h_illuminate .or. gs%s_illuminate) then
            gs%h_illuminate = .false.
            gs%s_illuminate = .false.
            call game_compute_visibility(gs)
        end if

        ! Switch turn and give next player their actions
        if (gs%turn == 0) then
            gs%turn = 1
            gs%seeker%actions_left = gs%seeker%speed
        else
            gs%turn = 0
            gs%turn_number = gs%turn_number + 1
            gs%hider%actions_left = gs%hider%speed
        end if

        gs%input_state   = INPUT_MOVE
        gs%selected_slot = 0
    end subroutine

    ! =========================================================================
    ! Pass the turn (do nothing).
    ! =========================================================================
    subroutine game_do_pass(gs)
        type(game_state), intent(inout) :: gs
        ! Nothing happens, turn just ends
    end subroutine

    ! =========================================================================
    ! Check whether the seeker has caught the hider.
    ! =========================================================================
    subroutine game_check_win(gs)
        type(game_state), intent(inout) :: gs
        if (gs%hider%x == gs%seeker%x .and. gs%hider%y == gs%seeker%y) then
            gs%game_over  = .true.
            gs%seeker_won = .true.
        end if
    end subroutine

    ! =========================================================================
    ! Private helpers
    ! =========================================================================

    ! Pick up all ground items at the player's current position.
    subroutine pickup_items(gs, p)
        type(game_state), intent(inout) :: gs
        type(player_type), intent(inout) :: p
        integer :: i

        do i = 1, gs%num_items
            if (gs%items(i)%active .and. &
                gs%items(i)%x == p%x .and. gs%items(i)%y == p%y) then
                if (p%num_items < MAX_INVENTORY) then
                    p%num_items = p%num_items + 1
                    p%inventory(p%num_items) = gs%items(i)%itype
                    gs%items(i)%active = .false.
                end if
            end if
        end do
    end subroutine

    ! Reveal all cells within given radius of (cx, cy) for a specific role.
    subroutine reveal_area(gs, role, cx, cy, radius)
        type(game_state), intent(inout) :: gs
        integer, intent(in) :: role, cx, cy, radius
        integer :: dx, dy, nx, ny

        do dy = -radius, radius
            do dx = -radius, radius
                nx = cx + dx
                ny = cy + dy
                if (nx >= 1 .and. nx <= gs%maze%w .and. &
                    ny >= 1 .and. ny <= gs%maze%h) then
                    if (role == ROLE_HIDER) then
                        gs%h_visible(nx, ny) = .true.
                        gs%h_visited(nx, ny) = .true.
                    else
                        gs%s_visible(nx, ny) = .true.
                        gs%s_visited(nx, ny) = .true.
                    end if
                end if
            end do
        end do
    end subroutine

    ! Remove item at slot from inventory and shift remaining items down.
    subroutine remove_item(p, slot)
        type(player_type), intent(inout) :: p
        integer, intent(in) :: slot
        integer :: i

        if (slot < 1 .or. slot > p%num_items) return
        do i = slot, p%num_items - 1
            p%inventory(i) = p%inventory(i + 1)
        end do
        p%inventory(p%num_items) = ITEM_NONE
        p%num_items = p%num_items - 1
    end subroutine

end module game_mod
