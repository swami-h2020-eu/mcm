! ---------------------------------------------------------------------
! – Project : SWAMI
! – Customer : N/A
! ---------------------------------------------------------------------
! – Author : Daniel Lubián Arenillas
! – Issue : 1.0
! – Date : 2021-03-31
! – Purpose : Module to wrap DTM
! - Component : m_dtm
! ---------------------------------------------------------------------
! – CNES, Deimos Space SLU, 2021
! – All rights reserved
! ---------------------------------------------------------------------

module m_dtm
    implicit none

    real(8), private, parameter :: PI = acos(-1d0)
    real(8), private, parameter :: DEG2RAD = PI/180d0
    real(8), private, parameter :: HOUR2RAD = PI/12d0

    character(50), public:: DTM2020_DATA_FILENAME = "DTM_2020_F107_Kp.dat"

    private :: wrapper_dtm2020
    public :: get_dtm2020
    public :: get_dtm2020_dens_uncertainty
    public :: t_dtm2020_out

    external :: sigma_function, dtm3, lecdtm

    type t_dtm2020_out
        real(8) :: dens  ! total density (in gram/cm3)
        real(8) :: temp  ! temperature at altitude (K)
        real(8) :: wmm   ! mean molecular mass (in gram)
        real(8) :: d_H   ! partial density of atomic hydrogen (in gram/cm3)
        real(8) :: d_He  ! partial density of helium
        real(8) :: d_O   ! partial density of atomic oxygen
        real(8) :: d_N2  ! partial density of molecular nitrogen
        real(8) :: d_O2  ! partial density of molecular oxygen
        real(8) :: d_N   ! partial density of atomic nitrogen
        real(8) :: tinf  ! exospheric temperature
    end type

contains

    subroutine get_dtm2020(dens, temp, alti, lati, longi, loct, doy, f107, f107m, kps, dtm_out)
        ! Get air density and temperature from DTM model

        implicit none
        real(8), intent(out) :: dens                            ! Density (g/cm^3)
        real(8), intent(out) :: temp                            ! Temperature (K)
        real(8), intent(in) :: alti                             ! Altitude (km) [120-]
        real(8), intent(in) :: lati                             ! Latitude (deg) [-90, 90]
        real(8), intent(in) :: longi                            ! Longitude (deg) [0, 360]
        real(8), intent(in) :: loct                             ! Local time (h) [0, 24]
        real(8), intent(in) :: doy                              ! Day of the year [0-366]
        real(8), intent(in) :: f107                             ! Space weather index F10.7, instantaneous flux at (t - 24hr)
        real(8), intent(in) :: f107m                            ! Space weather index F10.7, average flux at time
        real(8), intent(in) :: kps(2)                           ! Space weather index: kp delayed by 3 hours (1st value), kp mean of last 24 hours (2nd value)
        type(t_dtm2020_out), intent(out), optional :: dtm_out   ! Other output values (partial densities, mean molecular weight)

        real(8) :: f(2), fbar(2), akp(4)
        type(t_dtm2020_out) :: dtm2020_out

        f = [f107, 0d0]
        fbar = [f107m, 0d0]
        akp = [kps(1), 0d0, kps(2), 0d0]

        call wrapper_dtm2020(doy=doy, f=f, fbar=fbar, akp=akp, &
                             alti=alti, loct=loct*HOUR2RAD, lati=lati*DEG2RAD, longi=longi*DEG2RAD, &
                             dtm_out=dtm2020_out)
        temp = dtm2020_out%temp
        dens = dtm2020_out%dens

        if (present(dtm_out)) then
            dtm_out = dtm2020_out
        end if

    end subroutine get_dtm2020

    subroutine get_dtm2020_dens_uncertainty(unc, alti, lati, longi, loct, doy, f107, f107m, kps)
        ! Get air density uncertainty from DTM model. It is the percentage of the density value at those coordinates

        real(8), intent(out) :: unc             ! Uncertainty of the density: returns 1-sigma value.
        real(8), intent(in) :: alti             ! Altitude (km) [120-]
        real(8), intent(in) :: lati             ! Latitude (deg) [-90, 90]
        real(8), intent(in) :: longi             ! Longitude (deg) [0, 360]
        real(8), intent(in) :: loct             ! Local time (h) [0, 24]
        real(8), intent(in) :: doy              ! Day of the year [0-366]
        real(8), intent(in) :: f107             ! Space weather index F10.7, instantaneous flux at (t - 24hr)
        real(8), intent(in) :: f107m            ! Space weather index F10.7, average flux at time
        real(8), intent(in) :: kps(2)           ! Space weather index: kp delayed by 3 hours (1st value), kp mean of last 24 hours (2nd value)

        real(4) :: unc4

        call sigma_function(real(lati), real(loct), real(doy), real(alti), real(f107m), real(kps(1)), unc4)

        unc = dble(unc4)

    end subroutine get_dtm2020_dens_uncertainty

    subroutine init_dtm2020(data_file)
        ! Initialise DTM model (load into memory)

        implicit none
        character(*), intent(in) :: data_file

        open (unit=42, file=trim(data_file))
        call lecdtm(42)
        close (42)

    end subroutine init_dtm2020

    subroutine wrapper_dtm2020(doy, f, fbar, akp, alti, loct, lati, longi, dtm_out)

        implicit none
        !
        !.. Parameters ..
        !
        !.. Formal Arguments ..
        real(8), intent(in) :: lati, alti, doy, loct, longi
        real(8), dimension(2), intent(in) :: f, fbar
        real(8), dimension(4), intent(in) :: akp
        type(t_dtm2020_out), intent(out) :: dtm_out

        real :: d(6), wmm, tinf, temp, dens

        call dtm3(real(doy), real(f), real(fbar), real(akp), &
                  real(alti), real(loct), real(lati), real(longi), &
                  temp, tinf, dens, d, wmm)

        dtm_out%temp = dble(temp)
        dtm_out%dens = dble(dens)
        dtm_out%wmm = dble(wmm)
        dtm_out%tinf = dble(tinf)
        dtm_out%d_H = dble(d(1))
        dtm_out%d_He = dble(d(2))
        dtm_out%d_O = dble(d(3))
        dtm_out%d_N2 = dble(d(4))
        dtm_out%d_O2 = dble(d(5))
        dtm_out%d_N = dble(d(6))

    end subroutine wrapper_dtm2020

end module m_dtm
