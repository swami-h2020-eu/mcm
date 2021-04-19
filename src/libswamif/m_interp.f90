! ---------------------------------------------------------------------
! – Project : SWAMI
! – Customer : N/A
! ---------------------------------------------------------------------
! – Author : Daniel Lubián Arenillas
! – Issue : 1.0
! – Date : 2021-03-31
! – Purpose : Module with interpolation functions
! - Component : m_interp
! ---------------------------------------------------------------------
! – © Copyright Deimos Space SLU, 2021
! – All rights reserved
! ---------------------------------------------------------------------

module m_interp
    implicit none

    logical, parameter, private :: DEBUG_INTERP = .false.
    logical, parameter, private :: B_DEBUG_STOP_INTERP = .false.
    integer, parameter, private :: IER_OUT_BOUNDS_LEFT = -1
    integer, parameter, private :: IER_OUT_BOUNDS_RIGHT = -2
    integer, parameter, private :: IER_DIMS_MISMATCH = -3

    private :: check_dimension_1d
    private :: check_dimension_4d
    private :: check_ier

    public :: find_nearest_node
    public :: find_bounds
    public :: linear_segment
    public :: log10_linear_segment
    public :: loge_linear_segment
    public :: interp1d_nearest
    public :: interp1d_linear
    public :: interp4d_nearest
    public :: interp4d_linear

contains

    subroutine check_ier(ier, message)
        ! Check error code, print message and stop (if configured)

        implicit none
        integer, intent(in) :: ier                      ! Error code
        character(*), intent(in), optional :: message   ! Message

        character(len=256) :: mess, description

        mess = ""
        description = ""
        if (present(message)) mess = message

        select case (ier)
        case (0)
            continue
        case (IER_OUT_BOUNDS_LEFT)
            description = "Out of bounds: left side"
        case (IER_OUT_BOUNDS_RIGHT)
            description = "Out of bounds: right side"
        case (IER_DIMS_MISMATCH)
            description = "Dimensions mismatch"
        case default
            description = "Unknown error code"
        end select

        if (ier /= 0) then
            write (*, '(A,I4,A)') "[ERROR] INTERP: error code ", ier, " -> "//trim(description)//" // "//trim(mess)
        end if

        if (B_DEBUG_STOP_INTERP) stop

    end subroutine check_ier

    subroutine find_nearest_node(axis, x, idx, ier)
        ! | Find the nearest index in 'axis' between those the value 'x' is located. Assumes a sorted axis.
        ! | Error codes (ier):
        ! - 0 if no problems
        ! - -1 if 'x' is out of bounds on the lower side
        ! - -2 if 'x' is out of bounds on the upper side

        implicit none
        real(8), intent(in) :: axis(:)          ! Array with nodes
        real(8), intent(in) :: x                ! Value to locate
        integer, intent(out) :: idx             ! Index in node array closest to x
        integer, intent(out), optional :: ier   ! Error code

        idx = minloc(abs(axis - x), dim=1)

        if (present(ier)) then
            ier = 0
            if (idx < lbound(axis, dim=1)) ier = IER_OUT_BOUNDS_LEFT
            if (idx > ubound(axis, dim=1)) ier = IER_OUT_BOUNDS_RIGHT
        end if

    end subroutine find_nearest_node

    subroutine find_bounds(axis, x, idx_left, idx_right, ier)
        ! | Find the left ('idx_left') and right ('idx_right') index in 'axis' between those the value 'x' is located. Assumes a sorted axis.
        ! | Error codes (ier):
        ! - 0 if no problems
        ! - -1 if 'x' is out of bounds on the lower side
        ! - -2 if 'x' is out of bounds on the upper side

        implicit none
        real(8), intent(in) :: axis(:)          ! Array with nodes
        real(8), intent(in) :: x                ! Value to locate
        integer, intent(out) :: idx_left        ! Index in node array left to x
        integer, intent(out) :: idx_right       ! Index in node array right to x
        integer, intent(out), optional :: ier   ! Error code
        integer :: aux

        aux = minloc(abs(axis - x), dim=1)

        if (x > axis(aux)) then
            idx_left = aux
        else if (x < axis(aux)) then
            idx_left = aux - 1
        else ! x == axis(aux)
            if (aux == lbound(axis, dim=1)) then
                idx_left = aux
            else if (aux == ubound(axis, dim=1)) then
                idx_left = aux - 1
            else
                idx_left = aux
            end if
        end if

        idx_right = idx_left + 1

        if (present(ier)) then
            ier = 0
            if (idx_left < lbound(axis, dim=1)) ier = IER_OUT_BOUNDS_LEFT
            if (idx_right > ubound(axis, dim=1)) ier = IER_OUT_BOUNDS_RIGHT
            if (ier /= 0) then
                write (*, "(A3,A4,A7,A15)") ">>>", "", "index", "value"
                write (*, "(A3,A4,I7,F15.4)") ">>>", "arg", aux, x
                write (*, "(A3,A4,I7,F15.4)") ">>>", "min", minloc(axis), minval(axis)
                write (*, "(A3,A4,I7,F15.4)") ">>>", "max", maxloc(axis), maxval(axis)
            end if
        end if

    end subroutine find_bounds

    pure real(8) function linear_segment(x0, x1, f0, f1, p)
        ! Linear interpolation formula. Given two nodes (x0, x1) and their values (f0, f1), interpolate the value for p

        implicit none
        real(8), intent(in) :: x0   ! Left node
        real(8), intent(in) :: x1   ! Right node
        real(8), intent(in) :: f0   ! Left value
        real(8), intent(in) :: f1   ! Right value
        real(8), intent(in) :: p    ! Value where to interpolate in nodes axis

        linear_segment = f0 + (p - x0)*(f1 - f0)/(x1 - x0)
    end function linear_segment

    pure real(8) function log10_linear_segment(x0, x1, f0, f1, p)
        ! Linear interpolation formula. Given two nodes (x0, x1) and their values (f0, f1), interpolate the value for p

        implicit none
        real(8), intent(in) :: x0   ! Left node
        real(8), intent(in) :: x1   ! Right node
        real(8), intent(in) :: f0   ! Left value
        real(8), intent(in) :: f1   ! Right value
        real(8), intent(in) :: p    ! Value where to interpolate in nodes axis

        log10_linear_segment = log10(f0) + (p - x0)*(log10(f1) - log10(f0))/(x1 - x0)
        log10_linear_segment = 10d0**log10_linear_segment
    end function log10_linear_segment

    pure real(8) function loge_linear_segment(x0, x1, f0, f1, p)
        ! Linear interpolation formula. Given two nodes (x0, x1) and their values (f0, f1), interpolate the value for p

        implicit none
        real(8), intent(in) :: x0   ! Left node
        real(8), intent(in) :: x1   ! Right node
        real(8), intent(in) :: f0   ! Left value
        real(8), intent(in) :: f1   ! Right value
        real(8), intent(in) :: p    ! Value where to interpolate in nodes axis

        loge_linear_segment = log(f0) + (p - x0)*(log(f1) - log(f0))/(x1 - x0)
        loge_linear_segment = exp(loge_linear_segment)
    end function loge_linear_segment

    subroutine check_dimension_1d(x, f, ier)
        ! Check if arrays x and f have the same dimensions (for one-dimensional arrays)
        implicit none
        real(8), intent(in) :: x(:), f(:)
        integer, intent(out), optional :: ier
        integer :: size_x, size_dim_x

        ier = 0
        size_x = size(x)
        size_dim_x = size(f)
        if (size_x /= size_dim_x) then
            ier = IER_DIMS_MISMATCH
!             write (*, 100) size_x, size_dim_x
! 100         FORMAT("check_dimension: Error in axis 1, mismatch in size: provided ", I3, " actual ", I3)
!             stop
        end if
    end subroutine check_dimension_1d

    subroutine check_dimension_4d(x, f, dim, ier)
        ! Check if arrays x and f have the same dimensions (for four-dimensional arrays)

        implicit none
        real(8), intent(in) :: x(:), f(:, :, :, :)
        integer, intent(in) :: dim
        integer, intent(out), optional :: ier
        integer :: size_x, size_dim_x

        ier = 0
        size_x = size(x)
        size_dim_x = size(f, dim=dim)

        if (DEBUG_INTERP) write (*, *) "variable range: ", minval(x), maxval(x)
        if (size_x /= size_dim_x) then
            ier = IER_DIMS_MISMATCH
!             write (*, 100) dim, size_x, size_dim_x
! 100         FORMAT("check_dimension: Error in axis ", I1, ", mismatch in size: provided ", I3, " actual ", I3)
!             stop
        end if
    end subroutine check_dimension_4d

    real(8) function interp1d_nearest(x, f, p)
        ! Nearest-value interpolation in 1D

        implicit none
        real(8), intent(in) :: x(:) ! Sorted array containing the nodes
        real(8), intent(in) :: f(:) ! Array containing the values of the function at the nodes
        real(8), intent(in) :: p    ! Coordinate of where to evaluate the interpolation

        integer :: i, ier

        call check_dimension_1d(x, f, ier)
        call check_ier(ier, "")
        call find_nearest_node(x, p, i, ier)
        call check_ier(ier, "")

        interp1d_nearest = f(i)
    end function interp1d_nearest

    real(8) function interp1d_linear(x, f, p, apply_log10)
        ! Linear interpolation in 1D

        implicit none
        real(8), intent(in) :: x(:)                     ! Sorted array containing the nodes
        real(8), intent(in) :: f(:)                     ! Array containing the values of the function at the nodes
        real(8), intent(in) :: p                        ! Coordinate of where to evaluate the interpolation
        logical, intent(in), optional :: apply_log10    ! Apply log10 to the axis when interpolating linearly

        integer :: i0, i1, ier
        logical :: b_log10

        call check_dimension_1d(x, f, ier)
        call check_ier(ier, "check dimensions")
        call find_bounds(x, p, i0, i1, ier)
        call check_ier(ier, "out of bounds!")

        if (present(apply_log10)) then
            b_log10 = apply_log10
        else
            b_log10 = .false.
        end if

        if (b_log10) then
            interp1d_linear = log10_linear_segment(x(i0), x(i1), f(i0), f(i1), p)
        else
            interp1d_linear = linear_segment(x(i0), x(i1), f(i0), f(i1), p)
        end if

    end function interp1d_linear

    real(8) function interp4d_nearest(x, y, z, t, f, p)
        ! Nearest-value interpolation in 4D

        implicit none
        real(8), intent(in) :: x(:)          ! Sorted array containing the nodes for axis 1
        real(8), intent(in) :: y(:)          ! Sorted array containing the nodes for axis 2
        real(8), intent(in) :: z(:)          ! Sorted array containing the nodes for axis 3
        real(8), intent(in) :: t(:)          ! Sorted array containing the nodes for axis 4
        real(8), intent(in) :: f(:, :, :, :) ! 4D array containing the value points for axis x, y, z, t
        real(8), intent(in) :: p(4)          ! Array containing coordinates (x, y, z, t) of the point where to evaluate the interpolator

        integer :: ix, iy, iz, it, ier

        ! find bounds in each axis
        call find_nearest_node(x, p(1), ix, ier)
        call check_ier(ier, "dimension x")
        call find_nearest_node(y, p(2), iy, ier)
        call check_ier(ier, "dimension y")
        call find_nearest_node(z, p(3), iz, ier)
        call check_ier(ier, "dimension z")
        call find_nearest_node(t, p(4), it, ier)
        call check_ier(ier, "dimension t")

        interp4d_nearest = f(ix, iy, iz, it)

    end function interp4d_nearest

    real(8) function interp4d_linear(x, y, z, t, f, p, apply_log10)
        ! | Linear interpolation in 4D, based on:
        ! | https://en.wikipedia.org/wiki/Linear_interpolation
        ! | https://en.wikipedia.org/wiki/Bilinear_interpolation
        ! | https://en.wikipedia.org/wiki/Trilinear_interpolation

        implicit none
        real(8), intent(in) :: x(:)                     ! Sorted array containing the nodes for axis 1
        real(8), intent(in) :: y(:)                     ! Sorted array containing the nodes for axis 2
        real(8), intent(in) :: z(:)                     ! Sorted array containing the nodes for axis 3
        real(8), intent(in) :: t(:)                     ! Sorted array containing the nodes for axis 4
        real(8), intent(in) :: f(:, :, :, :)            ! 4D array containing the value points for axis x, y, z, t
        real(8), intent(in) :: p(4)                     ! Array containing coordinates (x, y, z, t) of the point where to evaluate the interpolator
        logical, intent(in), optional :: apply_log10(4) ! Apply log10 to the axis when interpolating linearly

        real(8) :: f0000, f0001, f0010, f0011, &
                   f0100, f0101, f0110, f0111, &
                   f1000, f1001, f1010, f1011, &
                   f1100, f1101, f1110, f1111
        real(8) :: f000, f010, f001, f011, &
                   f100, f110, f101, f111
        real(8) :: f00, f01, f10, f11
        real(8) :: f0, f1
        integer :: ix(0:1), iy(0:1), iz(0:1), it(0:1)
        integer :: ier

        real(8) :: px, py, pz, pt
        logical :: axis_log10(4) = [.false., .false., .false., .false.]

        if (present(apply_log10)) axis_log10 = apply_log10

        ! check dimensions
        call check_dimension_4d(x, f, 1, ier)
        call check_ier(ier, "dimension x")
        call check_dimension_4d(y, f, 2, ier)
        call check_ier(ier, "dimension y")
        call check_dimension_4d(z, f, 3, ier)
        call check_ier(ier, "dimension z")
        call check_dimension_4d(t, f, 4, ier)
        call check_ier(ier, "dimension t")

        ! split data point
        px = p(1)
        py = p(2)
        pz = p(3)
        pt = p(4)

        ! find bounds in each axis
        call find_bounds(x, px, ix(0), ix(1), ier)
        call check_ier(ier, "dimension x")
        call find_bounds(y, py, iy(0), iy(1), ier)
        call check_ier(ier, "dimension y")
        call find_bounds(z, pz, iz(0), iz(1), ier)
        call check_ier(ier, "dimension z")
        call find_bounds(t, pt, it(0), it(1), ier)
        call check_ier(ier, "dimension t")

        if (DEBUG_INTERP) write (*, *) "ix ", ix
        if (DEBUG_INTERP) write (*, *) "iy ", iy
        if (DEBUG_INTERP) write (*, *) "iz ", iz
        if (DEBUG_INTERP) write (*, *) "it ", it

        ! get values for all vertices in the hypercube
        f0000 = f(ix(0), iy(0), iz(0), it(0))
        f0001 = f(ix(0), iy(0), iz(0), it(1))
        f0010 = f(ix(0), iy(0), iz(1), it(0))
        f0011 = f(ix(0), iy(0), iz(1), it(1))
        f0100 = f(ix(0), iy(1), iz(0), it(0))
        f0101 = f(ix(0), iy(1), iz(0), it(1))
        f0110 = f(ix(0), iy(1), iz(1), it(0))
        f0111 = f(ix(0), iy(1), iz(1), it(1))
        f1000 = f(ix(1), iy(0), iz(0), it(0))
        f1001 = f(ix(1), iy(0), iz(0), it(1))
        f1010 = f(ix(1), iy(0), iz(1), it(0))
        f1011 = f(ix(1), iy(0), iz(1), it(1))
        f1100 = f(ix(1), iy(1), iz(0), it(0))
        f1101 = f(ix(1), iy(1), iz(0), it(1))
        f1110 = f(ix(1), iy(1), iz(1), it(0))
        f1111 = f(ix(1), iy(1), iz(1), it(1))

        ! linear interpolation along x
        if (axis_log10(1)) then
            if (DEBUG_INTERP) write (*, *) "Interpolating log10 axis x with dimension ", size(x)
            f000 = log10_linear_segment(x(ix(0)), x(ix(1)), f0000, f1000, px)
            f001 = log10_linear_segment(x(ix(0)), x(ix(1)), f0001, f1001, px)
            f010 = log10_linear_segment(x(ix(0)), x(ix(1)), f0010, f1010, px)
            f011 = log10_linear_segment(x(ix(0)), x(ix(1)), f0011, f1011, px)
            f100 = log10_linear_segment(x(ix(0)), x(ix(1)), f0100, f1100, px)
            f101 = log10_linear_segment(x(ix(0)), x(ix(1)), f0101, f1101, px)
            f110 = log10_linear_segment(x(ix(0)), x(ix(1)), f0110, f1110, px)
            f111 = log10_linear_segment(x(ix(0)), x(ix(1)), f0111, f1111, px)
        else
            f000 = linear_segment(x(ix(0)), x(ix(1)), f0000, f1000, px)
            f001 = linear_segment(x(ix(0)), x(ix(1)), f0001, f1001, px)
            f010 = linear_segment(x(ix(0)), x(ix(1)), f0010, f1010, px)
            f011 = linear_segment(x(ix(0)), x(ix(1)), f0011, f1011, px)
            f100 = linear_segment(x(ix(0)), x(ix(1)), f0100, f1100, px)
            f101 = linear_segment(x(ix(0)), x(ix(1)), f0101, f1101, px)
            f110 = linear_segment(x(ix(0)), x(ix(1)), f0110, f1110, px)
            f111 = linear_segment(x(ix(0)), x(ix(1)), f0111, f1111, px)
        end if

        if (DEBUG_INTERP) write (*, *) "f000 ", f000
        if (DEBUG_INTERP) write (*, *) "f001 ", f001
        if (DEBUG_INTERP) write (*, *) "f010 ", f010
        if (DEBUG_INTERP) write (*, *) "f011 ", f011
        if (DEBUG_INTERP) write (*, *) "f100 ", f100
        if (DEBUG_INTERP) write (*, *) "f101 ", f101
        if (DEBUG_INTERP) write (*, *) "f110 ", f110
        if (DEBUG_INTERP) write (*, *) "f111 ", f111

        ! linear interpolation along y
        if (axis_log10(2)) then
            if (DEBUG_INTERP) write (*, *) "Interpolating log10 axis y with dimension ", size(y)
            f00 = log10_linear_segment(y(iy(0)), y(iy(1)), f000, f100, py)
            f01 = log10_linear_segment(y(iy(0)), y(iy(1)), f001, f101, py)
            f10 = log10_linear_segment(y(iy(0)), y(iy(1)), f010, f110, py)
            f11 = log10_linear_segment(y(iy(0)), y(iy(1)), f011, f111, py)
        else
            f00 = linear_segment(y(iy(0)), y(iy(1)), f000, f100, py)
            f01 = linear_segment(y(iy(0)), y(iy(1)), f001, f101, py)
            f10 = linear_segment(y(iy(0)), y(iy(1)), f010, f110, py)
            f11 = linear_segment(y(iy(0)), y(iy(1)), f011, f111, py)
        end if

        if (DEBUG_INTERP) write (*, *) "f00 ", f00
        if (DEBUG_INTERP) write (*, *) "f01 ", f01
        if (DEBUG_INTERP) write (*, *) "f10 ", f10
        if (DEBUG_INTERP) write (*, *) "f11 ", f11

        ! linear interpolation along z
        if (axis_log10(3)) then
            if (DEBUG_INTERP) write (*, *) "Interpolating log10 axis z with dimension ", size(z)
            f0 = log10_linear_segment(z(iz(0)), z(iz(1)), f00, f10, pz)
            f1 = log10_linear_segment(z(iz(0)), z(iz(1)), f01, f11, pz)
        else
            f0 = linear_segment(z(iz(0)), z(iz(1)), f00, f10, pz)
            f1 = linear_segment(z(iz(0)), z(iz(1)), f01, f11, pz)
        end if

        if (DEBUG_INTERP) write (*, *) "f0 ", f0
        if (DEBUG_INTERP) write (*, *) "f1 ", f1

        ! linear interpolation along t
        if (axis_log10(4)) then
            if (DEBUG_INTERP) write (*, *) "Interpolating log10 axis t with dimension ", size(t)
            interp4d_linear = log10_linear_segment(t(it(0)), t(it(1)), f0, f1, pt)
        else
            interp4d_linear = linear_segment(t(it(0)), t(it(1)), f0, f1, pt)
        end if

    end function interp4d_linear

end module m_interp
