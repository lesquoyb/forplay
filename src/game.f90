! game.f90
!
! Game logic module for the multiplayer maze game.
!
! Teams:
!   TEAM_ESCAPE    (0) - find all keys to win
!   TEAM_LABYRINTH (1) - capture all escape players to win
!
! Classes (per team, reserved for future expansion):
!   Currently one class per team: Hider (escape), Seeker (labyrinth).
!   player_type%class_id is always 0 for now.
!
! Turn structure:
!   Round-robin by player index: 1, 2, ..., N, repeat.
!   Captured players are skipped.
!   Each player has 'speed' actions per turn.
!
! Win conditions:
!   Escape wins:    all keys collected
!   Labyrinth wins: all escape players captured
!
! Capture:
!   A labyrinth player on the same cell as an uncaptured escape player
!   captures that escape player. Captured position shows a tombstone
!   visible to all players.

module game_mod
    use :: maze_mod
    implicit none
    private

    ! --- Public API ---
    public :: game_state, player_type, ground_item_type
    public :: ITEM_NONE, ITEM_DASH, ITEM_VISION, ITEM_ILLUMINATE, ITEM_SPEED, ITEM_WALL_BREAK
    public :: NUM_ITEM_TYPES, MAX_INVENTORY, MAX_GROUND_ITEMS
    public :: TEAM_ESCAPE, TEAM_LABYRINTH
    public :: ROLE_HIDER, ROLE_SEEKER
    public :: MAX_PLAYERS, MAX_KEYS
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
    integer, parameter :: ITEM_WALL_BREAK = 5
    integer, parameter :: NUM_ITEM_TYPES  = 5

    ! --- Teams ---
    integer, parameter :: TEAM_ESCAPE    = 0
    integer, parameter :: TEAM_LABYRINTH = 1

    ! Legacy role constants (equal to team values, one class per team)
    integer, parameter :: ROLE_HIDER  = 0
    integer, parameter :: ROLE_SEEKER = 1

    ! --- Input states ---
    integer, parameter :: INPUT_MOVE     = 0
    integer, parameter :: INPUT_ITEM_DIR = 1
    integer, parameter :: INPUT_WAIT     = 2

    ! --- Limits ---
    integer, parameter :: MAX_INVENTORY    = 6
    integer, parameter :: MAX_GROUND_ITEMS = 30
    integer, parameter :: MAX_PLAYERS      = 6
    integer, parameter :: MAX_KEYS         = 10

    ! --- Types ---
    type :: ground_item_type
        integer :: x      = 0
        integer :: y      = 0
        integer :: itype  = ITEM_NONE
        logical :: active = .false.
    end type

    type :: player_type
        integer :: x             = 0
        integer :: y             = 0
        integer :: inventory(MAX_INVENTORY) = ITEM_NONE
        integer :: num_items     = 0
        integer :: vision_radius = 1
        integer :: speed         = 1
        integer :: actions_left  = 1
        integer :: team          = TEAM_ESCAPE
        integer :: class_id      = 0       ! reserved for future class system
        logical :: captured      = .false.
        logical :: illuminate    = .false.  ! full map reveal for current turn
        logical :: reveal        = .false.  ! can always see maze walls
    end type

    type :: game_state
        type(maze_type)    :: maze
        type(player_type)  :: players(MAX_PLAYERS)
        integer :: num_players = 0

        ! Keys (escape team objective)
        integer :: key_x(MAX_KEYS)       = 0
        integer :: key_y(MAX_KEYS)       = 0
        logical :: key_active(MAX_KEYS)  = .false.
        integer :: num_keys    = 0
        integer :: keys_found  = 0

        ! Per-player visibility and memory
        logical :: visible(MAZE_MAX_W, MAZE_MAX_H, MAX_PLAYERS)  = .false.
        logical :: visited(MAZE_MAX_W, MAZE_MAX_H, MAX_PLAYERS)  = .false.
        logical :: item_seen(MAX_GROUND_ITEMS, MAX_PLAYERS)      = .false.

        ! Per-player tracking of others: indices are (target, observer)
        integer :: last_seen_x(MAX_PLAYERS, MAX_PLAYERS) = 0
        integer :: last_seen_y(MAX_PLAYERS, MAX_PLAYERS) = 0
        logical :: has_seen(MAX_PLAYERS, MAX_PLAYERS)    = .false.

        ! Turn system (round-robin by player index)
        integer :: current_player = 1
        integer :: turn_number    = 1
        logical :: game_over      = .false.
        logical :: seekers_won    = .false.

        ! Ground items
        type(ground_item_type) :: items(MAX_GROUND_ITEMS)
        integer :: num_items = 0

        ! Client-side input state
        integer :: input_state    = INPUT_MOVE
        integer :: selected_slot  = 0
        logical :: initialized    = .false.
    end type

contains

    ! =========================================================================
    ! Initialize a new game: generate maze, place players, keys, and items.
    ! player_teams: team for each player (indexed 1..num_players).
    ! Escape team placed first, then labyrinth with min_dist constraint.
    ! =========================================================================
    subroutine game_init(gs, maze_w, maze_h, num_players, player_teams, &
                         num_keys, item_counts, min_dist, &
                         speed_escape, speed_labyrinth, &
                         vision_escape, vision_labyrinth, &
                         reveal_escape, reveal_labyrinth, &
                         branch_prob)
        type(game_state), intent(out) :: gs
        integer, intent(in) :: maze_w, maze_h, num_players, min_dist
        integer, intent(in) :: player_teams(MAX_PLAYERS)
        integer, intent(in) :: num_keys
        integer, intent(in) :: item_counts(NUM_ITEM_TYPES)
        integer, intent(in) :: speed_escape, speed_labyrinth
        integer, intent(in) :: vision_escape, vision_labyrinth
        logical, intent(in) :: reveal_escape, reveal_labyrinth
        real, intent(in) :: branch_prob

        integer :: i, j, k, x, y, itype, attempts, d, gen_attempts
        real    :: rval
        logical :: occupied(MAZE_MAX_W, MAZE_MAX_H)
        logical :: ok

        call random_seed()
        gs%num_players = num_players

        ! Assign teams and per-player stats from team defaults
        do i = 1, num_players
            gs%players(i)%team = player_teams(i)
            gs%players(i)%class_id = 0
            gs%players(i)%captured = .false.
            gs%players(i)%illuminate = .false.
            gs%players(i)%num_items = 0
            gs%players(i)%inventory = ITEM_NONE
            if (player_teams(i) == TEAM_ESCAPE) then
                gs%players(i)%speed = speed_escape
                gs%players(i)%vision_radius = vision_escape
                gs%players(i)%reveal = reveal_escape
            else
                gs%players(i)%speed = speed_labyrinth
                gs%players(i)%vision_radius = vision_labyrinth
                gs%players(i)%reveal = reveal_labyrinth
            end if
            gs%players(i)%actions_left = gs%players(i)%speed
        end do

        ! Generate maze and place players (retry with new maze on failure)
        gen_attempts = 0
        gen_loop: do
            call maze_generate(gs%maze, maze_w, maze_h, branch_prob)
            occupied = .false.

            ! Phase 1: place escape team (no distance constraint)
            do i = 1, num_players
                if (player_teams(i) /= TEAM_ESCAPE) cycle
                do
                    call random_number(rval); x = 1 + int(rval * maze_w)
                    if (x > maze_w) x = maze_w
                    call random_number(rval); y = 1 + int(rval * maze_h)
                    if (y > maze_h) y = maze_h
                    if (.not. occupied(x, y)) exit
                end do
                gs%players(i)%x = x
                gs%players(i)%y = y
                occupied(x, y) = .true.
            end do

            ! Phase 2: place labyrinth team with min_dist from escape players
            ok = .true.
            lab_loop: do i = 1, num_players
                if (player_teams(i) /= TEAM_LABYRINTH) cycle lab_loop
                do attempts = 1, 500
                    call random_number(rval); x = 1 + int(rval * maze_w)
                    if (x > maze_w) x = maze_w
                    call random_number(rval); y = 1 + int(rval * maze_h)
                    if (y > maze_h) y = maze_h
                    if (occupied(x, y)) cycle

                    ! Check distance from all escape players
                    ok = .true.
                    do j = 1, num_players
                        if (player_teams(j) /= TEAM_ESCAPE) cycle
                        d = maze_shortest_path(gs%maze, x, y, &
                                               gs%players(j)%x, gs%players(j)%y)
                        if (d < min_dist) then
                            ok = .false.
                            exit
                        end if
                    end do
                    if (ok) then
                        gs%players(i)%x = x
                        gs%players(i)%y = y
                        occupied(x, y) = .true.
                        cycle lab_loop
                    end if
                end do
                ! Failed to place this labyrinth player
                ok = .false.
                exit lab_loop
            end do lab_loop

            if (ok) exit gen_loop

            gen_attempts = gen_attempts + 1
            if (gen_attempts > 50) then
                ! Fallback: place remaining labyrinth players anywhere
                do i = 1, num_players
                    if (player_teams(i) /= TEAM_LABYRINTH) cycle
                    if (gs%players(i)%x > 0) cycle
                    do
                        call random_number(rval); x = 1 + int(rval * maze_w)
                        if (x > maze_w) x = maze_w
                        call random_number(rval); y = 1 + int(rval * maze_h)
                        if (y > maze_h) y = maze_h
                        if (.not. occupied(x, y)) exit
                    end do
                    gs%players(i)%x = x
                    gs%players(i)%y = y
                    occupied(x, y) = .true.
                end do
                exit gen_loop
            end if
        end do gen_loop

        ! Place keys at minimum distance from escape players
        gs%num_keys = num_keys
        gs%keys_found = 0
        do k = 1, num_keys
            attempts = 0
            do
                call random_number(rval); x = 1 + int(rval * gs%maze%w)
                if (x > gs%maze%w) x = gs%maze%w
                call random_number(rval); y = 1 + int(rval * gs%maze%h)
                if (y > gs%maze%h) y = gs%maze%h
                if (occupied(x, y)) cycle

                ok = .true.
                do j = 1, num_players
                    if (player_teams(j) /= TEAM_ESCAPE) cycle
                    d = maze_shortest_path(gs%maze, x, y, &
                                           gs%players(j)%x, gs%players(j)%y)
                    if (d < min_dist) then
                        ok = .false.
                        exit
                    end if
                end do
                if (ok) exit

                attempts = attempts + 1
                if (attempts > 500) exit
            end do
            gs%key_x(k) = x
            gs%key_y(k) = y
            gs%key_active(k) = .true.
            occupied(x, y) = .true.
        end do

        ! Place ground items
        gs%num_items = 0
        do itype = 1, NUM_ITEM_TYPES
            do j = 1, item_counts(itype)
                if (gs%num_items >= MAX_GROUND_ITEMS) exit
                do
                    call random_number(rval)
                    x = 1 + int(rval * gs%maze%w)
                    if (x > gs%maze%w) x = gs%maze%w
                    call random_number(rval)
                    y = 1 + int(rval * gs%maze%h)
                    if (y > gs%maze%h) y = gs%maze%h
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

        ! Initialize state
        gs%visible    = .false.
        gs%visited    = .false.
        gs%item_seen  = .false.
        gs%last_seen_x = 0
        gs%last_seen_y = 0
        gs%has_seen   = .false.
        gs%current_player = 1
        gs%turn_number = 1
        gs%game_over   = .false.
        gs%seekers_won = .false.
        gs%input_state = INPUT_MOVE
        gs%selected_slot = 0
        gs%initialized = .true.

        call game_compute_visibility(gs)
    end subroutine

    ! =========================================================================
    ! Move player pidx one cell in direction d.
    ! Returns .true. if the move was valid.
    ! =========================================================================
    function game_do_move(gs, pidx, d) result(moved)
        type(game_state), intent(inout) :: gs
        integer, intent(in) :: pidx, d
        logical :: moved
        integer :: dx, dy

        moved = .false.
        if (pidx < 1 .or. pidx > gs%num_players) return
        if (gs%players(pidx)%captured) return
        if (.not. maze_can_move(gs%maze, gs%players(pidx)%x, &
                                gs%players(pidx)%y, d)) return

        call maze_move_delta(d, dx, dy)
        gs%players(pidx)%x = gs%players(pidx)%x + dx
        gs%players(pidx)%y = gs%players(pidx)%y + dy
        call pickup_items(gs, gs%players(pidx))

        moved = .true.
        call game_compute_visibility(gs)
        call game_check_win(gs)
    end function

    ! =========================================================================
    ! Use an item from player pidx's inventory.
    ! slot = inventory index (1..num_items), d = direction for dash/pickaxe.
    ! =========================================================================
    subroutine game_do_use_item(gs, pidx, slot, d)
        type(game_state), intent(inout) :: gs
        integer, intent(in) :: pidx, slot, d
        integer :: itype, dx, dy, boost
        real    :: rval

        if (pidx < 1 .or. pidx > gs%num_players) return
        if (gs%players(pidx)%captured) return
        if (slot < 1 .or. slot > gs%players(pidx)%num_items) return
        itype = gs%players(pidx)%inventory(slot)
        if (itype == ITEM_NONE) return

        select case (itype)
            case (ITEM_DASH)
                call maze_move_delta(d, dx, dy)
                do while (maze_can_move(gs%maze, gs%players(pidx)%x, &
                                        gs%players(pidx)%y, d))
                    gs%players(pidx)%x = gs%players(pidx)%x + dx
                    gs%players(pidx)%y = gs%players(pidx)%y + dy
                    call reveal_area(gs, pidx, gs%players(pidx)%x, &
                                     gs%players(pidx)%y, 1)
                    call pickup_items(gs, gs%players(pidx))
                end do

            case (ITEM_VISION)
                call random_number(rval)
                boost = 1 + int(rval * 3.0)
                if (boost > 3) boost = 3
                gs%players(pidx)%vision_radius = &
                    gs%players(pidx)%vision_radius + boost

            case (ITEM_ILLUMINATE)
                gs%players(pidx)%illuminate = .true.

            case (ITEM_SPEED)
                gs%players(pidx)%speed = gs%players(pidx)%speed + 1

            case (ITEM_WALL_BREAK)
                if (.not. maze_break_wall(gs%maze, gs%players(pidx)%x, &
                                          gs%players(pidx)%y, d)) return
        end select

        call remove_item(gs%players(pidx), slot)
        call game_compute_visibility(gs)
        call game_check_win(gs)
    end subroutine

    ! =========================================================================
    ! Does the given item type need a direction? (e.g. dash, pickaxe)
    ! =========================================================================
    function game_item_needs_direction(itype) result(needs)
        integer, intent(in) :: itype
        logical :: needs
        needs = (itype == ITEM_DASH .or. itype == ITEM_WALL_BREAK)
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
            case (ITEM_WALL_BREAK); name = 'Pickaxe'
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
            case (ITEM_WALL_BREAK)
                desc = 'Break an adjacent wall in a direction'
            case default
                desc = ' '
        end select
    end function

    ! =========================================================================
    ! Compute visibility for all active players.
    ! Updates opponent tracking and item memory.
    ! =========================================================================
    subroutine game_compute_visibility(gs)
        type(game_state), intent(inout) :: gs
        integer :: p, t, px, py, r, dx, dy, nx, ny, i, w, h

        w = gs%maze%w
        h = gs%maze%h

        do p = 1, gs%num_players
            if (gs%players(p)%captured) cycle

            ! Compute current vision
            gs%visible(:,:,p) = .false.
            if (gs%players(p)%illuminate) then
                gs%visible(1:w, 1:h, p) = .true.
                gs%visited(1:w, 1:h, p) = .true.
            else
                px = gs%players(p)%x
                py = gs%players(p)%y
                r  = gs%players(p)%vision_radius
                do dy = -r, r
                    do dx = -r, r
                        nx = px + dx; ny = py + dy
                        if (nx >= 1 .and. nx <= w .and. &
                            ny >= 1 .and. ny <= h) then
                            gs%visible(nx, ny, p) = .true.
                            gs%visited(nx, ny, p) = .true.
                        end if
                    end do
                end do
            end if

            ! Track other players
            do t = 1, gs%num_players
                if (t == p) cycle
                if (gs%players(t)%captured) then
                    ! Tombstone: always visible to everyone
                    gs%has_seen(t, p) = .true.
                    gs%last_seen_x(t, p) = gs%players(t)%x
                    gs%last_seen_y(t, p) = gs%players(t)%y
                else if (gs%visible(gs%players(t)%x, gs%players(t)%y, p)) then
                    gs%last_seen_x(t, p) = gs%players(t)%x
                    gs%last_seen_y(t, p) = gs%players(t)%y
                    gs%has_seen(t, p) = .true.
                else if (gs%has_seen(t, p)) then
                    ! Ghost: clear if we can see the ghost position but they moved
                    if (gs%visible(gs%last_seen_x(t,p), &
                                   gs%last_seen_y(t,p), p)) then
                        gs%has_seen(t, p) = .false.
                    end if
                end if
            end do

            ! Update item memory
            do i = 1, gs%num_items
                if (gs%visible(gs%items(i)%x, gs%items(i)%y, p)) then
                    gs%item_seen(i, p) = gs%items(i)%active
                end if
            end do
        end do
    end subroutine

    ! =========================================================================
    ! End the current turn: decrement actions, advance to next player.
    ! =========================================================================
    subroutine game_end_turn(gs)
        type(game_state), intent(inout) :: gs
        integer :: cp, next, i

        cp = gs%current_player
        gs%players(cp)%actions_left = gs%players(cp)%actions_left - 1

        if (gs%players(cp)%actions_left > 0) then
            gs%input_state   = INPUT_MOVE
            gs%selected_slot = 0
            return
        end if

        ! Full turn ends: clear illuminate for this player
        if (gs%players(cp)%illuminate) then
            gs%players(cp)%illuminate = .false.
            call game_compute_visibility(gs)
        end if

        ! Find next non-captured player (round-robin)
        next = cp
        do i = 1, gs%num_players
            next = mod(next, gs%num_players) + 1
            if (.not. gs%players(next)%captured) exit
        end do

        ! New round when wrapping back
        if (next <= cp) gs%turn_number = gs%turn_number + 1

        gs%current_player = next
        gs%players(next)%actions_left = gs%players(next)%speed
        gs%input_state   = INPUT_MOVE
        gs%selected_slot = 0
    end subroutine

    ! =========================================================================
    ! Pass the turn (do nothing, caller will call game_end_turn).
    ! =========================================================================
    subroutine game_do_pass(gs)
        type(game_state), intent(inout) :: gs
        gs%players(gs%current_player)%actions_left = 1
    end subroutine

    ! =========================================================================
    ! Check captures and key collection. Set game_over if done.
    ! =========================================================================
    subroutine game_check_win(gs)
        type(game_state), intent(inout) :: gs
        integer :: i, j
        logical :: all_captured, all_keys

        ! Capture: labyrinth player on same cell as uncaptured escape player
        do i = 1, gs%num_players
            if (gs%players(i)%team /= TEAM_LABYRINTH) cycle
            if (gs%players(i)%captured) cycle
            do j = 1, gs%num_players
                if (gs%players(j)%team /= TEAM_ESCAPE) cycle
                if (gs%players(j)%captured) cycle
                if (gs%players(i)%x == gs%players(j)%x .and. &
                    gs%players(i)%y == gs%players(j)%y) then
                    gs%players(j)%captured = .true.
                end if
            end do
        end do

        ! Key collection: escape player on active key
        do i = 1, gs%num_players
            if (gs%players(i)%team /= TEAM_ESCAPE) cycle
            if (gs%players(i)%captured) cycle
            do j = 1, gs%num_keys
                if (.not. gs%key_active(j)) cycle
                if (gs%players(i)%x == gs%key_x(j) .and. &
                    gs%players(i)%y == gs%key_y(j)) then
                    gs%key_active(j) = .false.
                    gs%keys_found = gs%keys_found + 1
                end if
            end do
        end do

        ! Win: all escape players captured
        all_captured = .true.
        do i = 1, gs%num_players
            if (gs%players(i)%team == TEAM_ESCAPE .and. &
                .not. gs%players(i)%captured) then
                all_captured = .false.
                exit
            end if
        end do

        ! Win: all keys found (need at least 1 key)
        all_keys = (gs%num_keys > 0 .and. gs%keys_found >= gs%num_keys)

        if (all_captured) then
            gs%game_over   = .true.
            gs%seekers_won = .true.
        else if (all_keys) then
            gs%game_over   = .true.
            gs%seekers_won = .false.
        end if
    end subroutine

    ! =========================================================================
    ! Private helpers
    ! =========================================================================

    ! Pick up all ground items at the player's current position.
    subroutine pickup_items(gs, p)
        type(game_state), intent(inout) :: gs
        type(player_type), intent(inout) :: p
        integer :: i, boost
        real    :: rval

        do i = 1, gs%num_items
            if (.not. gs%items(i)%active) cycle
            if (gs%items(i)%x /= p%x .or. gs%items(i)%y /= p%y) cycle

            select case (gs%items(i)%itype)
            case (ITEM_VISION)
                call random_number(rval)
                boost = 1 + int(rval * 3.0)
                if (boost > 3) boost = 3
                p%vision_radius = p%vision_radius + boost
                gs%items(i)%active = .false.
            case (ITEM_SPEED)
                p%speed = p%speed + 1
                p%actions_left = p%actions_left + 1
                gs%items(i)%active = .false.
            case default
                if (p%num_items < MAX_INVENTORY) then
                    p%num_items = p%num_items + 1
                    p%inventory(p%num_items) = gs%items(i)%itype
                    gs%items(i)%active = .false.
                end if
            end select
        end do
    end subroutine

    ! Reveal cells within radius of (cx, cy) for player pidx.
    subroutine reveal_area(gs, pidx, cx, cy, radius)
        type(game_state), intent(inout) :: gs
        integer, intent(in) :: pidx, cx, cy, radius
        integer :: dx, dy, nx, ny

        do dy = -radius, radius
            do dx = -radius, radius
                nx = cx + dx
                ny = cy + dy
                if (nx >= 1 .and. nx <= gs%maze%w .and. &
                    ny >= 1 .and. ny <= gs%maze%h) then
                    gs%visible(nx, ny, pidx) = .true.
                    gs%visited(nx, ny, pidx) = .true.
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
