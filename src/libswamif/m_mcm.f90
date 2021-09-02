! ---------------------------------------------------------------------
! – Project : SWAMI
! – Customer : N/A
! ---------------------------------------------------------------------
! – Author : Daniel Lubián Arenillas
! – Issue : 1.0
! – Date : 2021-03-31
! – Purpose : Provides main MCM functions
! - Component : m_mcm
! ---------------------------------------------------------------------
! – Deimos Space SLU, CNES, MetOffice, 2021
! – All rights reserved
! ---------------------------------------------------------------------

module m_mcm
    use m_um, only: init_um, get_um_dens, get_um_dens_standard_deviation, &
                    get_um_temp, get_um_temp_standard_deviation, &
                    get_um_xwind, get_um_xwind_standard_deviation, &
                    get_um_ywind, get_um_ywind_standard_deviation
    use m_dtm, only: get_dtm2020, DTM2020_DATA_FILENAME, init_dtm2020, get_dtm2020_dens_uncertainty, t_dtm2020_out
    use m_interp, only: interp1d_linear, loge_linear_segment, linear_segment

    implicit none

    real(8), parameter, private :: BLENDING_ALTI_RANGE_LOW = 100.0d0  ! Transition region: lower altitude
    real(8), parameter, private :: BLENDING_ALTI_RANGE_HIGH = 120.0d0  ! Transition region: higher altitude
    real(8), parameter, private :: PI = acos(-1d0)
    real(8), parameter, private :: HOUR2RAD = PI/12.d0
    real(8), parameter, public :: NONE = -999999d0

    public :: get_mcm_dens
    public :: get_mcm_temp
    public :: get_mcm
    public :: t_mcm_out

    type t_mcm_out
        real(8) :: dens         ! Total density (in gram/cm3)
        real(8) :: temp         ! Temperature at altitude (K)
        real(8) :: wmm          ! Mean molecular mass (in gram)
        real(8) :: d_H          ! Partial density of atomic hydrogen (in gram/cm3)
        real(8) :: d_He         ! Partial density of helium
        real(8) :: d_O          ! Partial density of atomic oxygen
        real(8) :: d_N2         ! Partial density of molecular nitrogen
        real(8) :: d_O2         ! Partial density of molecular oxygen
        real(8) :: d_N          ! Partial density of atomic nitrogen
        real(8) :: tinf         ! Exospheric temperature, in K
        real(8) :: dens_unc     ! Density uncertainty from DTM2020 (above 120 km), as a percentage
        real(8) :: dens_std     ! Standard deviation of the density (UM, below 100 km), in g/cm3
        real(8) :: temp_std     ! Standard deviation of the temperature (UM, below 100 km), in K
        real(8) :: xwind        ! Zonal wind, in m/s
        real(8) :: ywind        ! Meridional wind, m/s
        real(8) :: xwind_std    ! Standard deviation of zonal wind, in m/s
        real(8) :: ywind_std    ! Standard deviation of neridional wind, m/s
    end type

contains

    subroutine init_mcm(data_um, data_dtm)
        ! Initialise MCM model by loading UM and DTM data into memory

        implicit none
        character(*), intent(in) :: data_um     ! Path to UM files
        character(*), intent(in) :: data_dtm    ! Path directory where to find DTM2020 data file

        call init_dtm2020(trim(data_dtm)//trim(DTM2020_DATA_FILENAME))
        call init_um(trim(data_um))

    end subroutine init_mcm

    subroutine get_mcm_dens(dens, alti, lati, longi, loct, doy, f107, f107m, kps)
        ! | Get air density from the MOWA Climatological model, blended model of UM and DTM
        ! - Below 100km, UM tables are used.
        ! - Between 100 and 120, both models are combined using linear interpolation on the log(dens).
        ! - Higher than 120, only DTM is used.

        real(8), intent(out) :: dens                ! Air density, in g/cm3
        real(8), intent(in) :: alti                 ! Altitude, in km
        real(8), intent(in) :: lati                 ! Latitude, in degrees [-90,+90]
        real(8), intent(in) :: longi                 ! Longitude, in degrees [0, 360), east positive
        real(8), intent(in) :: loct                 ! Local time, in hours [0, 24)
        real(8), intent(in) :: doy                  ! Day of the year [0-366)
        real(8), intent(in) :: f107                 ! Space weather index F10.7, instantaneous flux at (t - 24hr)
        real(8), intent(in) :: f107m                ! Space weather index F10.7, average flux at time
        real(8), intent(in) :: kps(2)               ! Space weather index: kp delayed by 3 hours (1st value), kp mean of last 24 hours (2nd value)

        real(8) :: um = 0.0d0
        real(8) :: dtm = 0.0d0
        real(8) :: aux

        if (alti < BLENDING_ALTI_RANGE_LOW) then
            call get_um_dens(um, alti, lati, longi, loct, doy, f107, f107m, kps)
            dens = um

        else if (alti > BLENDING_ALTI_RANGE_HIGH) then
            call get_dtm2020(dtm, aux, alti, lati, longi, loct, doy, f107, f107m, kps)
            dens = dtm

        else
            call get_um_dens(um, BLENDING_ALTI_RANGE_LOW, lati, longi, loct, doy, f107, f107m, kps)
            call get_dtm2020(dtm, aux, BLENDING_ALTI_RANGE_HIGH, lati, longi, loct, doy, f107, f107m, kps)

            dens = loge_linear_segment(BLENDING_ALTI_RANGE_LOW, BLENDING_ALTI_RANGE_HIGH, um, dtm, alti)

        end if

    end subroutine get_mcm_dens

    subroutine get_mcm_temp(temp, alti, lati, longi, loct, doy, f107, f107m, kps)
        ! | Get air temperature from the MOWA Climatological model, blended model of UM and DTM
        ! - Below 100km, UM tables are used.
        ! - Between 100 and 120, both models are combined using linear interpolation.
        ! - Higher than 120, only DTM is used.

        real(8), intent(out) :: temp                ! Air temperature, in K
        real(8), intent(in) :: alti                 ! Altitude, in km
        real(8), intent(in) :: lati                 ! Latitude, in degrees [-90,+90]
        real(8), intent(in) :: longi                 ! Longitude, in degrees [0, 360), east positive
        real(8), intent(in) :: loct                 ! Local time, in hours [0, 24)
        real(8), intent(in) :: doy                  ! Day of the year [0-366)
        real(8), intent(in) :: f107                 ! Space weather index F10.7, instantaneous flux at (t - 24hr)
        real(8), intent(in) :: f107m                ! Space weather index F10.7, average flux at time
        real(8), intent(in) :: kps(2)               ! Space weather index: kp delayed by 3 hours (1st value), kp mean of last 24 hours (2nd value)

        real(8) :: um = 0.0d0
        real(8) :: dtm = 0.0d0
        real(8) :: aux

        if (alti < BLENDING_ALTI_RANGE_LOW) then
            call get_um_temp(um, alti, lati, longi, loct, doy, f107, f107m, kps)
            temp = um

        else if (alti > BLENDING_ALTI_RANGE_HIGH) then
            call get_dtm2020(aux, dtm, alti, lati, longi, loct, doy, f107, f107m, kps)
            temp = dtm

        else
            call get_um_temp(um, BLENDING_ALTI_RANGE_LOW, lati, longi, loct, doy, f107, f107m, kps)
            call get_dtm2020(aux, dtm, BLENDING_ALTI_RANGE_HIGH, lati, longi, loct, doy, f107, f107m, kps)

            temp = linear_segment(BLENDING_ALTI_RANGE_LOW, BLENDING_ALTI_RANGE_HIGH, um, dtm, alti)
        end if

    end subroutine get_mcm_temp

    subroutine get_mcm(mcm_out, alti, lati, longi, loct, doy, f107, f107m, kps, get_unc, get_winds, region)
        ! | Get all output values for MCM (slow):
        ! - dens : Total density (in gram/cm3)
        ! - temp : Temperature at altitude (K)
        ! - wmm : Mean molecular mass (in gram)
        ! - d_H : Partial density of atomic hydrogen (in gram/cm3)
        ! - d_He : Partial density of helium
        ! - d_O : Partial density of atomic oxygen
        ! - d_N2 : Partial density of molecular nitrogen
        ! - d_O2 : Partial density of molecular oxygen
        ! - d_N : Partial density of atomic nitrogen
        ! - tinf : Exospheric temperature, in K
        ! - dens_unc : Density uncertainty from DTM2020 (above 120 km), as a percentage
        ! - dens_std : Standard deviation of the density (UM, below 100 km), in g/cm3
        ! - temp_std : Standard deviation of the temperature (UM, below 100 km), in K
        ! - xwind : Zonal wind, in m/s
        ! - ywind : Meridional wind, m/s
        ! - xwind_std : Standard deviation of zonal wind, in m/s
        ! - ywind_std : Standard deviation of neridional wind, m/s

        implicit none
        type(t_mcm_out), intent(out) :: mcm_out     ! Full output of MCM
        real(8), intent(in) :: alti                 ! Altitude, in km [0, 1500]
        real(8), intent(in) :: lati                 ! Latitude, in degrees [-90, 90]
        real(8), intent(in) :: longi                ! Longitude, in degrees [0, 360)
        real(8), intent(in) :: loct                 ! Local time, in hours [0-24)
        real(8), intent(in) :: doy                  ! Day of the year [0-366)
        real(8), intent(in) :: f107                 ! Space weather index F10.7, instantaneous flux at (t - 24hr)
        real(8), intent(in) :: f107m                ! Space weather index F10.7, average flux at time
        real(8), intent(in) :: kps(2)               ! Space weather index: kp delayed by 3 hours (1st value), kp mean of last 24 hours (2nd value)
        logical, intent(in), optional :: get_unc    ! Get uncertainties and standard deviations (defaults to .false.)
        logical, intent(in), optional :: get_winds  ! Get winds (defaults to .false.)
        integer, intent(out), optional :: region    ! Region: 1 is UM, 2 is blending, 3 is DTM

        real(8) :: temp_um, dens_um, temp_dtm, dens_dtm, alti_um, alti_dtm
        type(t_dtm2020_out) :: dtm_out
        logical :: b_get_unc = .false.
        logical :: b_get_winds = .false.
        integer :: region__ = 0

        if (present(get_unc)) b_get_unc = get_unc
        if (present(get_winds)) b_get_winds = get_winds

        mcm_out%dens = NONE
        mcm_out%temp = NONE
        mcm_out%wmm = NONE
        mcm_out%d_H = NONE
        mcm_out%d_He = NONE
        mcm_out%d_O = NONE
        mcm_out%d_N2 = NONE
        mcm_out%d_O2 = NONE
        mcm_out%d_N = NONE
        mcm_out%tinf = NONE
        mcm_out%dens_unc = NONE
        mcm_out%dens_std = NONE
        mcm_out%temp_std = NONE
        mcm_out%xwind = NONE
        mcm_out%ywind = NONE
        mcm_out%xwind_std = NONE
        mcm_out%ywind_std = NONE

        ! UM part
        alti_um = min(alti, BLENDING_ALTI_RANGE_LOW)
        call get_um_temp(temp_um, alti_um, lati, longi, loct, doy, f107, f107m, kps)
        call get_um_dens(dens_um, alti_um, lati, longi, loct, doy, f107, f107m, kps)
        if (b_get_unc .and. (alti <= BLENDING_ALTI_RANGE_LOW)) then ! <= 100 km
            call get_um_temp_standard_deviation(mcm_out%temp_std, alti, lati, longi, loct, doy, f107, f107m, kps)
            call get_um_dens_standard_deviation(mcm_out%dens_std, alti, lati, longi, loct, doy, f107, f107m, kps)
        end if

        ! Winds
        if (b_get_winds .and. (alti <= BLENDING_ALTI_RANGE_HIGH)) then  ! <= 120 km
            call get_um_xwind(mcm_out%xwind, alti, lati, longi, loct, doy, f107, f107m, kps)
            call get_um_ywind(mcm_out%ywind, alti, lati, longi, loct, doy, f107, f107m, kps)
            if (b_get_unc .and. (alti <= BLENDING_ALTI_RANGE_HIGH)) then
                call get_um_xwind_standard_deviation(mcm_out%xwind_std, alti, lati, longi, loct, doy, f107, f107m, kps)
                call get_um_ywind_standard_deviation(mcm_out%ywind_std, alti, lati, longi, loct, doy, f107, f107m, kps)
            end if
        end if

        ! DTM2020 part
        alti_dtm = max(alti, BLENDING_ALTI_RANGE_HIGH)
        call get_dtm2020(dens_dtm, temp_dtm, alti_dtm, lati, longi, loct, doy, f107, f107m, kps, dtm_out=dtm_out)
        if (alti >= BLENDING_ALTI_RANGE_HIGH) then ! 120 km
            mcm_out%wmm = dtm_out%wmm
            mcm_out%d_H = dtm_out%d_H
            mcm_out%d_He = dtm_out%d_He
            mcm_out%d_O = dtm_out%d_O
            mcm_out%d_N2 = dtm_out%d_N2
            mcm_out%d_O2 = dtm_out%d_O2
            mcm_out%d_N = dtm_out%d_N
            mcm_out%tinf = dtm_out%tinf
            if (b_get_unc) then
                call get_dtm2020_dens_uncertainty(mcm_out%dens_unc, alti, lati, longi, loct, doy, f107, f107m, kps)
            end if
        end if

        ! Blending of temperature and density
        if (alti <= BLENDING_ALTI_RANGE_LOW) then ! 100 km
            mcm_out%temp = temp_um
            mcm_out%dens = dens_um
            region__ = 1
        else if (alti >= BLENDING_ALTI_RANGE_HIGH) then ! 120 km
            mcm_out%temp = temp_dtm
            mcm_out%dens = dens_dtm
            region__ = 3
        else
            mcm_out%temp = linear_segment(BLENDING_ALTI_RANGE_LOW, BLENDING_ALTI_RANGE_HIGH, temp_um, temp_dtm, alti)
            mcm_out%dens = loge_linear_segment(BLENDING_ALTI_RANGE_LOW, BLENDING_ALTI_RANGE_HIGH, dens_um, dens_dtm, alti)
            region__ = 2
        end if

        if (present(region)) region = region__

    end subroutine get_mcm

end module m_mcm
