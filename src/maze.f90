! maze.f90
!
! Maze generation module using recursive backtracking (DFS).
! Each cell stores a bitmask of its walls: WALL_N, WALL_E, WALL_S, WALL_W.

module maze_mod
    implicit none
    private

    public :: maze_type
    public :: WALL_N, WALL_E, WALL_S, WALL_W
    public :: DIR_N, DIR_E, DIR_S, DIR_W
    public :: MAZE_MAX_W, MAZE_MAX_H
    public :: maze_generate, maze_can_move, maze_move_delta
    public :: maze_shortest_path
    public :: maze_break_wall
    ! (removed duplicate public statement)
    ! =========================================================================
    ! Break the wall adjacent to (x, y) in direction d, if a wall exists.
    ! If there is no wall, nothing happens.
    ! Returns .true. if a wall was broken, .false. otherwise.
    ! =========================================================================
    ! (removed duplicate implementation)

    ! Wall bit flags
    integer, parameter :: WALL_N = 1   ! bit 0 - north wall
    integer, parameter :: WALL_E = 2   ! bit 1 - east wall
    integer, parameter :: WALL_S = 4   ! bit 2 - south wall
    integer, parameter :: WALL_W = 8   ! bit 3 - west wall

    ! Direction constants
    integer, parameter :: DIR_N = 0
    integer, parameter :: DIR_E = 1
    integer, parameter :: DIR_S = 2
    integer, parameter :: DIR_W = 3

    ! Maximum maze dimensions
    integer, parameter :: MAZE_MAX_W = 31
    integer, parameter :: MAZE_MAX_H = 23

    type :: maze_type
        integer :: w = 0
        integer :: h = 0
        integer :: cells(MAZE_MAX_W, MAZE_MAX_H) = 0
    end type

contains

    ! =========================================================================
    ! Break the wall adjacent to (x, y) in direction d, if a wall exists.
    ! If there is no wall, nothing happens.
    ! Returns .true. if a wall was broken, .false. otherwise.
    ! =========================================================================
    function maze_break_wall(m, x, y, d) result(broken)
        type(maze_type), intent(inout) :: m
        integer, intent(in) :: x, y, d
        logical :: broken
        integer :: nx, ny, wall_bit, opp_wall_bit

        broken = .false.
        select case (d)
            case (DIR_N)
                nx = x; ny = y - 1; wall_bit = WALL_N; opp_wall_bit = WALL_S
            case (DIR_E)
                nx = x + 1; ny = y; wall_bit = WALL_E; opp_wall_bit = WALL_W
            case (DIR_S)
                nx = x; ny = y + 1; wall_bit = WALL_S; opp_wall_bit = WALL_N
            case (DIR_W)
                nx = x - 1; ny = y; wall_bit = WALL_W; opp_wall_bit = WALL_E
            case default
                return
        end select

        if (nx < 1 .or. nx > m%w .or. ny < 1 .or. ny > m%h) return
        if (iand(m%cells(x, y), wall_bit) /= 0) then
            m%cells(x, y) = iand(m%cells(x, y), not(wall_bit))
            m%cells(nx, ny) = iand(m%cells(nx, ny), not(opp_wall_bit))
            broken = .true.
        end if
    end function

    ! =========================================================================
    ! Generate a random perfect maze using recursive backtracking (DFS).
    ! A perfect maze has exactly one path between any two cells.
    ! =========================================================================
    subroutine maze_generate(m, w, h, branch_prob)
        type(maze_type), intent(out) :: m
        integer, intent(in) :: w, h
        real, intent(in) :: branch_prob

        integer :: vis(MAZE_MAX_W, MAZE_MAX_H)
        integer :: stk_x(MAZE_MAX_W * MAZE_MAX_H)
        integer :: stk_y(MAZE_MAX_W * MAZE_MAX_H)
        integer :: sp, cx, cy, nx, ny
        integer :: dirs(4), nd, pick, d
        real    :: rval
        integer :: x, y, d2, nx2, ny2, wall_bit, opp_wall_bit

        m%w = w
        m%h = h
        m%cells = 0

        ! Initialize all cells with all 4 walls
        m%cells(1:w, 1:h) = ior(ior(WALL_N, WALL_E), ior(WALL_S, WALL_W))
        vis = 0

        ! Start at (1, 1)
        cx = 1; cy = 1
        vis(cx, cy) = 1
        sp = 1
        stk_x(1) = cx; stk_y(1) = cy

        do while (sp > 0)
            cx = stk_x(sp); cy = stk_y(sp)

            ! Collect unvisited neighbors
            nd = 0
            if (cy > 1) then
                if (vis(cx, cy - 1) == 0) then
                    nd = nd + 1; dirs(nd) = DIR_N
                end if
            end if
            if (cx < w) then
                if (vis(cx + 1, cy) == 0) then
                    nd = nd + 1; dirs(nd) = DIR_E
                end if
            end if
            if (cy < h) then
                if (vis(cx, cy + 1) == 0) then
                    nd = nd + 1; dirs(nd) = DIR_S
                end if
            end if
            if (cx > 1) then
                if (vis(cx - 1, cy) == 0) then
                    nd = nd + 1; dirs(nd) = DIR_W
                end if
            end if

            if (nd > 0) then
                ! Choose a random unvisited neighbor
                call random_number(rval)
                pick = 1 + int(rval * nd)
                if (pick > nd) pick = nd
                d = dirs(pick)

                ! Remove wall between current cell and chosen neighbor
                select case (d)
                    case (DIR_N)
                        nx = cx; ny = cy - 1
                        m%cells(cx, cy) = iand(m%cells(cx, cy), not(WALL_N))
                        m%cells(nx, ny) = iand(m%cells(nx, ny), not(WALL_S))
                    case (DIR_E)
                        nx = cx + 1; ny = cy
                        m%cells(cx, cy) = iand(m%cells(cx, cy), not(WALL_E))
                        m%cells(nx, ny) = iand(m%cells(nx, ny), not(WALL_W))
                    case (DIR_S)
                        nx = cx; ny = cy + 1
                        m%cells(cx, cy) = iand(m%cells(cx, cy), not(WALL_S))
                        m%cells(nx, ny) = iand(m%cells(nx, ny), not(WALL_N))
                    case (DIR_W)
                        nx = cx - 1; ny = cy
                        m%cells(cx, cy) = iand(m%cells(cx, cy), not(WALL_W))
                        m%cells(nx, ny) = iand(m%cells(nx, ny), not(WALL_E))
                end select

                vis(nx, ny) = 1
                sp = sp + 1
                stk_x(sp) = nx; stk_y(sp) = ny
            else
                sp = sp - 1
            end if
        end do
        ! Post-processing: randomly remove walls to add extra branches
        do y = 1, h
            do x = 1, w
                do d2 = 0, 3
                    select case (d2)
                        case (DIR_N)
                            nx2 = x; ny2 = y - 1; wall_bit = WALL_N; opp_wall_bit = WALL_S
                        case (DIR_E)
                            nx2 = x + 1; ny2 = y; wall_bit = WALL_E; opp_wall_bit = WALL_W
                        case (DIR_S)
                            nx2 = x; ny2 = y + 1; wall_bit = WALL_S; opp_wall_bit = WALL_N
                        case (DIR_W)
                            nx2 = x - 1; ny2 = y; wall_bit = WALL_W; opp_wall_bit = WALL_E
                        case default
                            cycle
                    end select
                    if (nx2 < 1 .or. nx2 > w .or. ny2 < 1 .or. ny2 > h) cycle
                    if (iand(m%cells(x, y), wall_bit) /= 0) then
                        call random_number(rval)
                        if (rval < branch_prob) then
                            m%cells(x, y) = iand(m%cells(x, y), not(wall_bit))
                            m%cells(nx2, ny2) = iand(m%cells(nx2, ny2), not(opp_wall_bit))
                        end if
                    end if
                end do
            end do
        end do
    end subroutine

    ! =========================================================================
    ! Check whether movement from (x, y) in direction d is possible (no wall).
    ! =========================================================================
    function maze_can_move(m, x, y, d) result(ok)
        type(maze_type), intent(in) :: m
        integer, intent(in) :: x, y, d
        logical :: ok
        integer :: wall_bit

        ok = .false.
        select case (d)
            case (DIR_N); wall_bit = WALL_N
            case (DIR_E); wall_bit = WALL_E
            case (DIR_S); wall_bit = WALL_S
            case (DIR_W); wall_bit = WALL_W
            case default; return
        end select

        if (x < 1 .or. x > m%w .or. y < 1 .or. y > m%h) return
        ok = (iand(m%cells(x, y), wall_bit) == 0)
    end function

    ! =========================================================================
    ! Get the (dx, dy) offset for a direction.
    ! =========================================================================
    subroutine maze_move_delta(d, dx, dy)
        integer, intent(in) :: d
        integer, intent(out) :: dx, dy

        dx = 0; dy = 0
        select case (d)
            case (DIR_N); dy = -1
            case (DIR_E); dx =  1
            case (DIR_S); dy =  1
            case (DIR_W); dx = -1
        end select
    end subroutine

    ! =========================================================================
    ! BFS shortest path distance between two cells in the maze.
    ! Returns the number of steps, or -1 if unreachable.
    ! =========================================================================
    function maze_shortest_path(m, x1, y1, x2, y2) result(dist)
        type(maze_type), intent(in) :: m
        integer, intent(in) :: x1, y1, x2, y2
        integer :: dist

        integer :: bfs_dist(MAZE_MAX_W, MAZE_MAX_H)
        integer :: qx(MAZE_MAX_W * MAZE_MAX_H), qy(MAZE_MAX_W * MAZE_MAX_H)
        integer :: head, tail, cx, cy, nx, ny, d, dx, dy

        dist = -1
        if (x1 == x2 .and. y1 == y2) then
            dist = 0
            return
        end if

        bfs_dist = -1
        bfs_dist(x1, y1) = 0
        head = 1; tail = 1
        qx(1) = x1; qy(1) = y1

        do while (head <= tail)
            cx = qx(head); cy = qy(head); head = head + 1

            do d = DIR_N, DIR_W
                if (.not. maze_can_move(m, cx, cy, d)) cycle
                call maze_move_delta(d, dx, dy)
                nx = cx + dx; ny = cy + dy

                if (bfs_dist(nx, ny) >= 0) cycle   ! already visited

                bfs_dist(nx, ny) = bfs_dist(cx, cy) + 1

                if (nx == x2 .and. ny == y2) then
                    dist = bfs_dist(nx, ny)
                    return
                end if

                tail = tail + 1
                qx(tail) = nx; qy(tail) = ny
            end do
        end do
    end function

end module maze_mod
