! ---------------------------------------------------------------------
! – Project : SWAMI
! – Customer : N/A
! ---------------------------------------------------------------------
! – Author : Daniel Lubián Arenillas
! – Issue : 1.0
! – Date : 2021-03-31
! – Purpose : Handle UM datatables
! - Component : m_um
! ---------------------------------------------------------------------
! – MetOffice, Deimos Space SLU, 2021
! – All rights reserved
! ---------------------------------------------------------------------

module m_um

    use netcdf
    use m_interp, only: interp4d_linear, interp4d_nearest

    implicit none

    integer, parameter :: UM_FNAME_LENGTH = 256
    real(8), parameter :: KM2M = 1000.d0
    logical, parameter :: DEBUG_UM = .true.
    logical, parameter :: B_DEBUG_STOP_UM = .true.
    real(8), parameter :: UM_ALTI_LIMIT = 100d0
    real(8), parameter :: UM_ALTI_LIMIT_WINDS = 120d0
    character(len=*), parameter :: UM_NAME_TIME = "time" !> cftime.num2date(t0, "hours since 1970-01-01")
    character(len=*), parameter :: UM_NAME_REFTIME = "forecast_reference_time"
    character(len=*), parameter :: UM_NAME_LATI = "latitude"
    character(len=*), parameter :: UM_NAME_LOCT = "local_time"
    character(len=*), parameter :: UM_NAME_ALTI = "height"
    character(len=*), parameter :: UM_NAME_DENS = "air_density"
    character(len=*), parameter :: UM_NAME_TEMP = "air_temperature"
    character(len=*), parameter :: UM_NAME_XWIND = "x_wind"
    character(len=*), parameter :: UM_NAME_YWIND = "y_wind"
    character(len=*), parameter :: UM_VAR_TYPE_MEAN = "mean"
    character(len=*), parameter :: UM_VAR_TYPE_STD = "std"
    integer, parameter :: SOLAR_CYCLE_ACT_LOW = 1
    integer, parameter :: SOLAR_CYCLE_ACT_MEDIUM = 2
    integer, parameter :: SOLAR_CYCLE_ACT_HIGH = 3

    character(len=256) :: path_to_um_data

    private :: classify_solar_cycle
    private :: get_dimension_um
    private :: get_reference_date
    public :: check_ier_nc
    public :: convert_doy_to_um_time
    public :: get_um_filename
    public :: get_um_temp
    public :: get_um_dens
    public :: get_um_xwind
    public :: get_um_ywind
    public :: get_um_temp_standard_deviation
    public :: get_um_dens_standard_deviation
    public :: get_um_xwind_standard_deviation
    public :: get_um_ywind_standard_deviation
    public :: load_um_file, interpolate_um_var_linear, interpolate_um_var_nearest
    public :: t_um_dimension, t_um_variable

    type :: t_um_dimension
        ! UM dimension derived type
        character(len=20) :: name       ! Name
        integer :: id                   ! ID
        integer :: dimid                ! Dimension ID
        integer :: len                  ! Length of array
        real(8), allocatable :: data(:) ! Data array
    end type t_um_dimension

    type :: t_um_variable
        ! UM variable derived type
        character(len=20) :: name                ! Name
        integer :: id                            ! ID
        integer, dimension(4) :: shape           ! Shape of the data
        real(8), allocatable :: data(:, :, :, :) ! Data array
        type(t_um_dimension) :: time             ! time
        type(t_um_dimension) :: lati             ! latitude [0, 360)
        type(t_um_dimension) :: loct             ! local time [0, 24)
        type(t_um_dimension) :: alti             ! altitude [0, inf)
        type(t_um_dimension) :: dims(4)          ! Dimensions as an array
        real(8) :: reference_time
    end type t_um_variable

    logical :: file_is_loaded(3, 4, 2)
    type(t_um_variable) :: um_vars(3, 4, 2)

contains

    subroutine init_um(um_data_path)
        ! Initialise UM common variables

        implicit none
        character(len=*), intent(in) :: um_data_path

        integer, parameter :: solarcycles(3) = [SOLAR_CYCLE_ACT_LOW, SOLAR_CYCLE_ACT_MEDIUM, SOLAR_CYCLE_ACT_HIGH]
        character(len=*), parameter :: varnames(4) = [UM_NAME_TEMP, UM_NAME_DENS, UM_NAME_XWIND, UM_NAME_YWIND]
        character(len=*), parameter :: vartypes(2) = [UM_VAR_TYPE_MEAN, UM_VAR_TYPE_STD]

        integer :: i, j, k, ivn, ivt, iscc, code, ier
        type(t_um_variable) :: um_var

        path_to_um_data = trim(um_data_path)

        file_is_loaded(:, :, :) = .false.

        do i = 1, size(solarcycles)
            do j = 1, size(varnames)
                do k = 1, size(vartypes)
                    call tipify_um_var(varnames(j), vartypes(k), solarcycles(i), ivn, ivt, iscc, code)
                    call load_um_var(varnames(j), vartypes(k), solarcycles(i), um_var, code)
                    if (code /= 0) then
                        write(*, *) "ERROR: while tipifying UM var during initialization: ", &
                            varnames(j), vartypes(k), solarcycles(i)
                        ier = code
                        call exit(ier)
                    end if
                    if (code /= 0) then
                        write(*, *) "ERROR: while loading UM var during initialization: ", &
                            varnames(j), vartypes(k), solarcycles(i)
                        ier = code
                        call exit(ier)
                    end if

                    um_vars(iscc, ivn, ivt) = um_var
                    file_is_loaded(iscc, ivn, ivt) = .true.

                end do
            end do
        end do

    end subroutine init_um

    subroutine check_ier_nc(ier, message, exit_program)
        ! Used to provide clear error messages from netCDF

        ! Arguments
        integer, intent(in)                     :: ier              ! Error code from a netCDF function
        character(len=*), intent(in), optional  :: message          ! Message to print
        logical, intent(in), optional           :: exit_program     ! Stop after printing

        character(len=80)                       :: nc_explanation

        if (ier .ne. 0) then
            nc_explanation = nf90_strerror(ier)
            write (*, *) '> ERROR: ', ier, " - ", trim(nc_explanation)
            if (present(message)) then
                write (*, *) '>      : ', trim(message)
            end if
            if (present(exit_program)) then
                if (exit_program .or. B_DEBUG_STOP_UM) stop
            end if
        end if

    end subroutine check_ier_nc

    subroutine check_altitude(alti, limit)
        implicit none
        real(8), intent(in) :: alti, limit

        if (alti > limit) then
            write (*, "(A,F10.4,A,F10.4,A)") "UM ERROR: Altitude ", alti, " km above limit ", limit, " km. Stopping."
            stop
        end if
    end subroutine check_altitude

    subroutine get_dimension_um(name, fid, dim_um, ier)
        ! Get UM dimension fron a netCDF file

        ! Arguments
        character(*), intent(in) :: name                ! Name of the dimension
        integer, intent(in) ::  fid                     ! File unit id
        type(t_um_dimension), intent(out) :: dim_um     ! UM dimension type
        integer, intent(out) :: ier                     ! Error code

        ! Internal variables
        integer :: nc_status

        dim_um%name = name

        ! get id of variable
        nc_status = nf90_inq_dimid(fid, name, dim_um%dimid)
        call check_ier_nc(nc_status, "while inquiring dimid "//name)

        ! get length of array
        nc_status = nf90_inquire_dimension(fid, dim_um%dimid, len=dim_um%len)
        call check_ier_nc(nc_status, "while inquiring dimension "//name)

        ! allocate data
        allocate (dim_um%data(dim_um%len))

        ! get variable id
        nc_status = nf90_inq_varid(fid, dim_um%name, dim_um%id)
        call check_ier_nc(nc_status, "while inquiring variable id "//name)

        ! get variable data
        nc_status = nf90_get_var(fid, dim_um%id, dim_um%data)
        call check_ier_nc(nc_status, "while inquiring variable data "//name)

    end subroutine get_dimension_um

    subroutine get_reference_date(fid, time_varid, reference_time)
        ! Get the forecast reference time

        implicit none
        integer, intent(in) :: fid              ! netCDF file id
        integer, intent(in) :: time_varid       ! netCDF time variable id
        real(8), intent(out) ::  reference_time ! forecast reference time

        integer :: ier, id

        ! ier = nf90_get_att(fid, time_varid, UM_NAME_REFTIME, reference_time)
        ier = nf90_inq_varid(fid, UM_NAME_REFTIME, id)
        call check_ier_nc(ier, "while getting reference time varid ")
        ier = nf90_get_var(fid, id, reference_time)
        call check_ier_nc(ier, "while getting reference time value")

    end subroutine get_reference_date

    subroutine load_um_file(fname, var_name, um_var, ier)
        ! Load UM netCDF file as a UM variable

        ! Arguments
        character(*), intent(in) :: fname           ! Path to file
        character(*), intent(in) :: var_name        ! Variable name
        type(t_um_variable), intent(out) :: um_var  ! UM variable type data holder
        integer, intent(out) :: ier                 ! Error code

        ! Internal variables
        integer :: fid

        ! open file_in and set its id value in fid :
        ier = nf90_open(path=trim(fname), mode=nf90_nowrite, ncid=fid)
        call check_ier_nc(ier, "error while opening "//trim(fname))

        ! get dimensions/axis info
        call get_dimension_um(UM_NAME_TIME, fid, um_var%time, ier)
        call get_dimension_um(UM_NAME_LATI, fid, um_var%lati, ier)
        call get_dimension_um(UM_NAME_LOCT, fid, um_var%loct, ier)
        call get_dimension_um(UM_NAME_ALTI, fid, um_var%alti, ier)

        um_var%dims(um_var%time%dimid) = um_var%time
        um_var%dims(um_var%lati%dimid) = um_var%lati
        um_var%dims(um_var%loct%dimid) = um_var%loct
        um_var%dims(um_var%alti%dimid) = um_var%alti

        ! set variable attributes
        um_var%name = var_name
        um_var%shape = [um_var%dims(4)%len, &
                        um_var%dims(3)%len, &
                        um_var%dims(2)%len, &
                        um_var%dims(1)%len]

        ! allocate variable data
        allocate (um_var%data(um_var%dims(4)%len, &
                              um_var%dims(3)%len, &
                              um_var%dims(2)%len, &
                              um_var%dims(1)%len))

        ! get variable id
        ier = nf90_inq_varid(fid, um_var%name, um_var%id)
        call check_ier_nc(ier, "while inquiring variable id "//um_var%name)

        ! get variable data
        ier = nf90_get_var(fid, um_var%id, um_var%data)
        call check_ier_nc(ier, "while inquiring variable data "//um_var%name)

        ! get reference time
        call get_reference_date(fid, um_var%time%id, um_var%reference_time)

        ! close netcdf file:
        ier = nf90_close(fid)
        call check_ier_nc(ier, "error while closing "//trim(fname))

    end subroutine load_um_file

    subroutine interpolate_um_var_linear(um_var, alti, lati, loct, time, var_out, apply_log10)
        ! Interpolate linearly over the um_var at point (alti, lati, loct, time)

        implicit none
        type(t_um_variable), intent(in) :: um_var       ! UM variable
        real(8), intent(in) :: alti                     ! Altitude where to interpolate
        real(8), intent(in) :: lati                     ! Latitude where to interpolate
        real(8), intent(in) :: loct                     ! Local time where to interpolate
        real(8), intent(in) :: time                     ! Time where to interpolate
        real(8), intent(out) ::  var_out                ! Value at the coordinates after interpolation
        logical, intent(in), optional :: apply_log10(4) ! Axis where to apply log10 in interpolation

        real(8) :: point(4)
        logical :: axis_log10(4)

        if (present(apply_log10)) then
            axis_log10 = apply_log10
        else
            axis_log10 = [.false., .false., .false., .false.]
        end if

        point = [alti, loct, lati, time]

        var_out = interp4d_linear(um_var%dims(4)%data, & ! altitude
                                  um_var%dims(3)%data, & ! latitude
                                  um_var%dims(2)%data, & ! local time
                                  um_var%dims(1)%data, & ! time
                                  um_var%data, &
                                  point, &
                                  apply_log10=axis_log10)

    end subroutine interpolate_um_var_linear

    subroutine interpolate_um_var_nearest(um_var, alti, lati, loct, time, var_out)
        ! Interpolate by nearest value over the um_var at point (alti, lati, loct, time)

        implicit none
        type(t_um_variable), intent(in) :: um_var   ! UM variable
        real(8), intent(in) :: alti                 ! Altitude
        real(8), intent(in) :: lati                 ! Latitude
        real(8), intent(in) :: loct                 ! Local time
        real(8), intent(in) :: time                 ! Time
        real(8), intent(out) ::  var_out            ! Value

        real(8) :: point(4)

        point = [alti, loct, lati, time]

        var_out = interp4d_nearest(um_var%dims(4)%data, &
                                   um_var%dims(3)%data, &
                                   um_var%dims(2)%data, &
                                   um_var%dims(1)%data, &
                                   um_var%data, &
                                   point)
    end subroutine interpolate_um_var_nearest

    subroutine classify_solar_cycle(f107m, solar_cycle_class)
        ! Classify the set of space weather indices as a low/medium/high activity solar cycle

        implicit none
        real(8), intent(in) :: f107m                ! F10.7 average
        integer, intent(out) :: solar_cycle_class   ! Class of solar cycle

        if (f107m < 120d0) then
            solar_cycle_class = SOLAR_CYCLE_ACT_LOW
        else if (f107m > 160d0) then
            solar_cycle_class = SOLAR_CYCLE_ACT_HIGH
        else
            solar_cycle_class = SOLAR_CYCLE_ACT_MEDIUM
        end if

        ! solar_cycle_class = 1 ! low activity, 2008-2009
        ! solar_cycle_class = 2 ! medium activity, 2004
        ! solar_cycle_class = 3 ! high activity, 2002
    end subroutine classify_solar_cycle

    subroutine convert_doy_to_um_time(doy, solar_cycle_class, um_time)
        ! Convert day of year to UM time [WIP]

        implicit none
        real(8), intent(in) :: doy                  ! Day of year
        integer, intent(in) :: solar_cycle_class    ! Solar cycle class (to selec the file)
        real(8), intent(out) :: um_time             ! UM time

        real(8) :: hoy, um_ref, doy_aux

        if ((solar_cycle_class == SOLAR_CYCLE_ACT_LOW) .and. (doy < 166d0)) then
            doy_aux = doy + 365d0
        else if ((solar_cycle_class == SOLAR_CYCLE_ACT_MEDIUM) .and. (doy > 349d0)) then
            doy_aux = doy - 365d0
        else if ((solar_cycle_class == SOLAR_CYCLE_ACT_HIGH) .and. (doy > 348d0)) then
            doy_aux = doy - 365d0
        else
            doy_aux = doy
        end if

        um_time = doy_aux

    end subroutine convert_doy_to_um_time

    subroutine get_um_filename(var_name, var_type, solar_cycle_class, fname, ier)
        ! Generate filename for UM file

        implicit none
        character(*), intent(in) :: var_name       ! UM_NAME_TEMP, UM_NAME_DENS, UM_NAME_XWIND, UM_NAME_YWIND
        character(*), intent(in) :: var_type       ! UM_VAR_TYPE_MEAN, UM_VAR_TYPE_STD
        integer, intent(in) :: solar_cycle_class   ! 1, 2, 3
        character(*), intent(out) :: fname         ! Filename
        integer, intent(out) :: ier                ! Error code

        character(30):: pv, pt, py

        ier = 0

        ! solar cycle
        select case (solar_cycle_class)
        case (SOLAR_CYCLE_ACT_LOW)
            py = "2008-2009" ! low activity
        case (SOLAR_CYCLE_ACT_MEDIUM)
            py = "2004" ! medium activity
        case (SOLAR_CYCLE_ACT_HIGH)
            py = "2002" ! high activity
        case default
            py = ""
            ier = -1001
        end select

        ! variable
        select case (var_name)
        case (UM_NAME_TEMP)
            pv = "air-temperature"
        case (UM_NAME_DENS)
            pv = "air-density"
        case (UM_NAME_XWIND)
            pv = "x-wind"
        case (UM_NAME_YWIND)
            pv = "y-wind"
        case default
            pv = ""
            ier = -1002
        end select

        ! kind
        select case (var_type)
        case (UM_VAR_TYPE_MEAN)
            pt = "monthly-mean"
        case (UM_VAR_TYPE_STD)
            pt = "standard-deviation"
        case default
            pt = ""
            ier = -1003
        end select

        fname = trim(py)//"/"//trim(pv)//"_"//trim(pt)//".mcm.nc"

    end subroutine get_um_filename

    subroutine tipify_um_var(var_name, var_type, solar_cycle_class, ivarname, ivartype, iscc, ier)
        ! Convert variable name, type and solar cycle class to integer numbers to be used as indices

        implicit none
        character(*), intent(in) :: var_name        ! UM_NAME_TEMP, UM_NAME_DENS, UM_NAME_XWIND, UM_NAME_YWIND
        character(*), intent(in) :: var_type        ! UM_VAR_TYPE_MEAN, UM_VAR_TYPE_STD
        integer, intent(in) :: solar_cycle_class    ! 1, 2, 3
        integer, intent(out) :: ivarname            ! Index for variable name
        integer, intent(out) :: ivartype            ! Index for variable type
        integer, intent(out) :: iscc                ! Index for solar cycle class
        integer, intent(out) :: ier                 ! Error code

        ier = 0

        ! solar cycle
        select case (solar_cycle_class)
        case (SOLAR_CYCLE_ACT_LOW)
            iscc = 1 ! low activity
        case (SOLAR_CYCLE_ACT_MEDIUM)
            iscc = 2 ! medium activity
        case (SOLAR_CYCLE_ACT_HIGH)
            iscc = 3 ! high activity
        case default
            iscc = 0
            ier = -1001
        end select

        ! variable
        select case (var_name)
        case (UM_NAME_TEMP)
            ivarname = 1
        case (UM_NAME_DENS)
            ivarname = 2
        case (UM_NAME_XWIND)
            ivarname = 3
        case (UM_NAME_YWIND)
            ivarname = 4
        case default
            ivarname = 0
            ier = -1002
        end select

        ! kind
        select case (var_type)
        case (UM_VAR_TYPE_MEAN)
            ivartype = 1
        case (UM_VAR_TYPE_STD)
            ivartype = 2
        case default
            ivartype = 0
            ier = -1003
        end select

    end subroutine tipify_um_var

    subroutine load_um_var(var_name, var_type, solar_cycle_class, um_var, ier)
        ! Load UM variable into memory or retrieve from a previous load.

        implicit none
        character(*), intent(in) :: var_name       ! UM_NAME_TEMP, UM_NAME_DENS, UM_NAME_XWIND, UM_NAME_YWIND
        character(*), intent(in) :: var_type       ! UM_VAR_TYPE_MEAN, UM_VAR_TYPE_STD
        integer, intent(in) :: solar_cycle_class   ! 1, 2, 3
        type(t_um_variable), intent(out) :: um_var ! Filename
        integer, intent(out) :: ier                ! Error code

        integer :: code
        integer :: iscc, ivarname, ivartype
        character(len=UM_FNAME_LENGTH) :: um_fname

        ier = 0

        call tipify_um_var(var_name, var_type, solar_cycle_class, ivarname, ivartype, iscc, code)
        if (code /= 0) then
            write(*,*) "Error while tipifying UM var"
            ier = code
            return
        end if

        if (file_is_loaded(iscc, ivarname, ivartype)) then
            ! file is loaded, look for it
            um_var = um_vars(iscc, ivarname, ivartype)
        else
            ! generate the correct filename
            call get_um_filename(var_name, var_type, solar_cycle_class, um_fname, code)
            if (code /= 0) then
                write(*,*) "Error while generating UM fname"
                ier = code
                return
            end if
            um_fname = trim(path_to_um_data)//trim(um_fname)

            ! write(*, *) "INFO: UM var was not initiliased. Loading "//trim(um_fname)

            ! Load UM data from file
            call load_um_file(um_fname, var_name, um_var, code)
            if (code /= 0) then
                write(*,*) "Error while loading "//trim(um_fname)
                ier = code
                return
            end if

            file_is_loaded(iscc, ivarname, ivartype) = .true.

        end if

    end subroutine load_um_var

    subroutine get_um_var_std(var, varname, alti, lati, longi, loct, doy, f107, f107m, kps)
        ! Get standard deviation for a variable from UM tables, interpolating if necessary

        implicit none
        real(8), intent(out) :: var         ! Variable value
        character(*), intent(in) :: varname ! Variable name
        real(8), intent(in) :: alti         ! Altitude, in km [0-152]
        real(8), intent(in) :: lati         ! Latitude, in degrees [-90, 90]
        real(8), intent(in) :: longi        ! Longitude, in degrees [0, 360)
        real(8), intent(in) :: loct         ! Local time, in hours [0-24)
        real(8), intent(in) :: doy          ! Day of the year [0-366)
        real(8), intent(in) :: f107         ! Space weather index F10.7, instantaneous flux at (t - 24hr)
        real(8), intent(in) :: f107m        ! Space weather index F10.7, average flux at time
        real(8), intent(in) :: kps(2)       ! Space weather index: kp delayed by 3 hours (1st value), kp mean of last 24 hours (2nd value)

        real(8) :: um_time, alti_m
        integer :: solar_cycle_class, code
        character(len=UM_FNAME_LENGTH) :: um_fname
        type(t_um_variable) :: um_var

        ! prepare some variables
        alti_m = km2m*alti
        call classify_solar_cycle(f107m, solar_cycle_class)
        call convert_doy_to_um_time(doy, solar_cycle_class, um_time)

        ! Load UM data from memory
        call load_um_var(varname, UM_VAR_TYPE_STD, solar_cycle_class, um_var, code)
        if (code /= 0) then
            write(*,*) "Error while loading UM var"
            return
        end if

        ! interpolate
        call interpolate_um_var_nearest(um_var, alti_m, lati, loct, um_time, var)

    end subroutine get_um_var_std

    subroutine get_um_var_mean(var, varname, alti, lati, longi, loct, doy, f107, f107m, kps, apply_log10)
        ! Get monthly mean value for a variable from UM tables, interpolating if necessary

        implicit none
        real(8), intent(out) :: var                     ! Variable value
        character(*), intent(in) :: varname             ! Variable name
        real(8), intent(in) :: alti                     ! Altitude, in km [0-152]
        real(8), intent(in) :: lati                     ! Latitude, in degrees [-90, 90]
        real(8), intent(in) :: longi                    ! Longitude, in degrees [0, 360)
        real(8), intent(in) :: loct                     ! Local time, in hours [0-24)
        real(8), intent(in) :: doy                      ! Day of the year [0-366)
        real(8), intent(in) :: f107                     ! Space weather index F10.7, instantaneous flux at (t - 24hr)
        real(8), intent(in) :: f107m                    ! Space weather index F10.7, average flux at time
        real(8), intent(in) :: kps(2)                   ! Space weather index: kp delayed by 3 hours (1st value), kp mean of last 24 hours (2nd value)
        logical, intent(in), optional :: apply_log10(4) ! Axis where to apply log10 in interpolation

        real(8) :: um_time, alti_m
        integer :: solar_cycle_class, code
        character(len=UM_FNAME_LENGTH) :: um_fname
        type(t_um_variable) :: um_var
        logical :: interpolate_log10(4) = [.false., .false., .false., .false.] ! interpolate altitude with log10

        ! prepare some variables
        alti_m = km2m*alti
        call classify_solar_cycle(f107m, solar_cycle_class)
        call convert_doy_to_um_time(doy, solar_cycle_class, um_time)

        ! Load UM data from memory
        call load_um_var(varname, UM_VAR_TYPE_MEAN, solar_cycle_class, um_var, code)
        if (code /= 0) then
            write(*,*) "Error while loading UM var"
            return
        end if

        ! interpolate
        if (present(apply_log10)) then
            interpolate_log10 = apply_log10
        else
            interpolate_log10 = [.false., .false., .false., .false.]
        end if

        call interpolate_um_var_linear(um_var, alti_m, lati, loct, um_time, var, &
                                       apply_log10=interpolate_log10)

    end subroutine get_um_var_mean

    subroutine get_um_temp(temp, alti, lati, longi, loct, doy, f107, f107m, kps)
        ! Get air temperature from UM tables, interpolating if necessary

        implicit none
        real(8), intent(out) :: temp    ! Air temperature, in K
        real(8), intent(in) :: alti     ! Altitude, in km [0-100]
        real(8), intent(in) :: lati     ! Latitude, in degrees [-90, 90]
        real(8), intent(in) :: longi    ! Longitude, in degrees [0, 360)
        real(8), intent(in) :: loct     ! Local time, in hours [0-24)
        real(8), intent(in) :: doy      ! Day of the year [0-366)
        real(8), intent(in) :: f107     ! Space weather index F10.7, instantaneous flux at (t - 24hr)
        real(8), intent(in) :: f107m    ! Space weather index F10.7, average flux at time
        real(8), intent(in) :: kps(2)   ! Space weather index: kp delayed by 3 hours (1st value), kp mean of last 24 hours (2nd value)

        call check_altitude(alti, UM_ALTI_LIMIT)
        call get_um_var_mean(temp, UM_NAME_TEMP, alti, lati, longi, loct, &
                             doy, f107, f107m, kps, &
                             apply_log10=[.false., .false., .false., .false.])

    end subroutine get_um_temp

    subroutine get_um_dens(dens, alti, lati, longi, loct, doy, f107, f107m, kps)
        ! Get air density from UM tables, interpolating if necessary

        implicit none
        real(8), intent(out) :: dens   ! Air density, in g/cm^3
        real(8), intent(in) :: alti    ! Altitude, in km [0-100]
        real(8), intent(in) :: lati    ! Latitude, in degrees [-90, 90]
        real(8), intent(in) :: longi   ! Longitude, in degrees [0, 360)
        real(8), intent(in) :: loct    ! Local time, in hours [0-24)
        real(8), intent(in) :: doy     ! Day of the year [0-366)
        real(8), intent(in) :: f107    ! Space weather index F10.7, instantaneous flux at (t - 24hr)
        real(8), intent(in) :: f107m   ! Space weather index F10.7, average flux at time
        real(8), intent(in) :: kps(2)  ! Space weather index: kp delayed by 3 hours (1st value), kp mean of last 24 hours (2nd value)

        call check_altitude(alti, UM_ALTI_LIMIT)
        call get_um_var_mean(dens, UM_NAME_DENS, alti, lati, longi, loct, doy, &
                             f107, f107m, kps, &
                             apply_log10=[.true., .false., .false., .false.])

        ! Convert kg/m3 to g/cm3
        dens = dens*1d-3

    end subroutine get_um_dens

    subroutine get_um_dens_standard_deviation(std, alti, lati, longi, loct, doy, f107, f107m, kps)
        ! Get the standard deviation of the air density from UM tables, using the nearest value

        implicit none
        real(8), intent(out) :: std    ! Standard deviation of the density [g/cm3]
        real(8), intent(in) :: alti    ! Altitude, in km [0-100]
        real(8), intent(in) :: lati    ! Latitude, in degrees [-90, 90]
        real(8), intent(in) :: longi   ! Longitude, in degrees [0, 360)
        real(8), intent(in) :: loct    ! Local time, in hours [0-24)
        real(8), intent(in) :: doy     ! Day of the year [0-366)
        real(8), intent(in) :: f107    ! Space weather index F10.7, instantaneous flux at (t - 24hr)
        real(8), intent(in) :: f107m   ! Space weather index F10.7, average flux at time
        real(8), intent(in) :: kps(2)  ! Space weather index: kp delayed by 3 hours (1st value), kp mean of last 24 hours (2nd value)

        call check_altitude(alti, UM_ALTI_LIMIT)
        call get_um_var_std(std, UM_NAME_DENS, alti, lati, longi, loct, &
                            doy, f107, f107m, kps)

        ! Convert kg/m3 to g/cm3
        std = std*1d-3
    end subroutine get_um_dens_standard_deviation

    subroutine get_um_temp_standard_deviation(std, alti, lati, longi, loct, doy, f107, f107m, kps)
        ! Get the standard deviation of the air temperature from UM tables, using the nearest value

        implicit none
        real(8), intent(out) :: std    ! Standard deviation of the temperature [K]
        real(8), intent(in) :: alti    ! Altitude, in km [0-100]
        real(8), intent(in) :: lati    ! Latitude, in degrees [-90, 90]
        real(8), intent(in) :: longi   ! Longitude, in degrees [0, 360)
        real(8), intent(in) :: loct    ! Local time, in hours [0-24)
        real(8), intent(in) :: doy     ! Day of the year [0-366)
        real(8), intent(in) :: f107    ! Space weather index F10.7, instantaneous flux at (t - 24hr)
        real(8), intent(in) :: f107m   ! Space weather index F10.7, average flux at time
        real(8), intent(in) :: kps(2)  ! Space weather index: kp delayed by 3 hours (1st value), kp mean of last 24 hours (2nd value)

        call check_altitude(alti, UM_ALTI_LIMIT)
        call get_um_var_std(std, UM_NAME_TEMP, alti, lati, longi, loct, &
                            doy, f107, f107m, kps)

    end subroutine get_um_temp_standard_deviation

    subroutine get_um_xwind(xwind, alti, lati, longi, loct, doy, f107, f107m, kps)
        ! Get X wind from UM tables, interpolating if necessary

        implicit none
        real(8), intent(out) :: xwind  ! X wind, in m/s
        real(8), intent(in) :: alti    ! Altitude, in km [0-120]
        real(8), intent(in) :: lati    ! Latitude, in degrees [-90, 90]
        real(8), intent(in) :: longi   ! Longitude, in degrees [0, 360)
        real(8), intent(in) :: loct    ! Local time, in hours [0-24)
        real(8), intent(in) :: doy     ! Day of the year [0-366)
        real(8), intent(in) :: f107    ! Space weather index F10.7, instantaneous flux at (t - 24hr)
        real(8), intent(in) :: f107m   ! Space weather index F10.7, average flux at time
        real(8), intent(in) :: kps(2)  ! Space weather index: kp delayed by 3 hours (1st value), kp mean of last 24 hours (2nd value)

        call check_altitude(alti, UM_ALTI_LIMIT_WINDS)
        call get_um_var_mean(xwind, UM_NAME_XWIND, alti, lati, longi, loct, &
                             doy, f107, f107m, kps, &
                             apply_log10=[.false., .false., .false., .false.])

    end subroutine get_um_xwind

    subroutine get_um_ywind(ywind, alti, lati, longi, loct, doy, f107, f107m, kps)
        ! Get Y wind from UM tables, interpolating if necessary

        implicit none
        real(8), intent(out) :: ywind   ! Y wind, in m/s
        real(8), intent(in) :: alti     ! Altitude, in km [0-120]
        real(8), intent(in) :: lati     ! Latitude, in degrees [-90, 90]
        real(8), intent(in) :: longi    ! Longitude, in degrees [0, 360)
        real(8), intent(in) :: loct     ! Local time, in hours [0-24)
        real(8), intent(in) :: doy      ! Day of the year [0-366)
        real(8), intent(in) :: f107     ! Space weather index F10.7, instantaneous flux at (t - 24hr)
        real(8), intent(in) :: f107m    ! Space weather index F10.7, average flux at time
        real(8), intent(in) :: kps(2)   ! Space weather index: kp delayed by 3 hours (1st value), kp mean of last 24 hours (2nd value)

        call check_altitude(alti, UM_ALTI_LIMIT_WINDS)
        call get_um_var_mean(ywind, UM_NAME_YWIND, alti, lati, longi, loct, &
                             doy, f107, f107m, kps, &
                             apply_log10=[.false., .false., .false., .false.])

    end subroutine get_um_ywind

    subroutine get_um_xwind_standard_deviation(std, alti, lati, longi, loct, doy, f107, f107m, kps)
        ! Get the standard deviation of the X wind from UM tables, using the nearest value

        implicit none
        real(8), intent(out) :: std    ! Standard deviation of the X wind [m/s]
        real(8), intent(in) :: alti    ! Altitude, in km [0-120]
        real(8), intent(in) :: lati    ! Latitude, in degrees [-90, 90]
        real(8), intent(in) :: longi   ! Longitude, in degrees [0, 360)
        real(8), intent(in) :: loct    ! Local time, in hours [0-24)
        real(8), intent(in) :: doy     ! Day of the year [0-366)
        real(8), intent(in) :: f107    ! Space weather index F10.7, instantaneous flux at (t - 24hr)
        real(8), intent(in) :: f107m   ! Space weather index F10.7, average flux at time
        real(8), intent(in) :: kps(2)  ! Space weather index: kp delayed by 3 hours (1st value), kp mean of last 24 hours (2nd value)

        call check_altitude(alti, UM_ALTI_LIMIT_WINDS)
        call get_um_var_std(std, UM_NAME_XWIND, alti, lati, longi, loct, &
                            doy, f107, f107m, kps)

    end subroutine get_um_xwind_standard_deviation

    subroutine get_um_ywind_standard_deviation(std, alti, lati, longi, loct, doy, f107, f107m, kps)
        ! Get the standard deviation of the Y wind from UM tables, using the nearest value

        implicit none
        real(8), intent(out) :: std    ! Standard deviation of the Y wind [m/s]
        real(8), intent(in) :: alti    ! Altitude, in km [0-120]
        real(8), intent(in) :: lati    ! Latitude, in degrees [-90, 90]
        real(8), intent(in) :: longi   ! Longitude, in degrees [0, 360)
        real(8), intent(in) :: loct    ! Local time, in hours [0-24)
        real(8), intent(in) :: doy     ! Day of the year [0-366)
        real(8), intent(in) :: f107    ! Space weather index F10.7, instantaneous flux at (t - 24hr)
        real(8), intent(in) :: f107m   ! Space weather index F10.7, average flux at time
        real(8), intent(in) :: kps(2)  ! Space weather index: kp delayed by 3 hours (1st value), kp mean of last 24 hours (2nd value)

        call check_altitude(alti, UM_ALTI_LIMIT_WINDS)
        call get_um_var_std(std, UM_NAME_YWIND, alti, lati, longi, loct, &
                            doy, f107, f107m, kps)

    end subroutine get_um_ywind_standard_deviation

end module m_um
