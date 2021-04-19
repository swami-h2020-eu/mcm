subroutine sigma_function(latitude,lhour,dayear,altitude,fpsolar,kpindex,stdev)

    ! last version updated : 05/01/2021
    ! Author : Claude Boniface (CNES)
    
    !----------------------------------!
    ! ONE-SIGMA ERROR EVALUATION MODULE!
    !----------------------------------!
    
    ! The model is well justified in the range [200 km - 500 km]. 
    ! However, strong assumptions must be added to extend the altitude range from 100 km to 1000 km due to unavailability of data:
    ! at lower altitudes (< 200 km), a worst-case is chosen by fixing the evolution of 1-sigma to their values at the altitude of 200 km,
    ! at higher altitudes (> 500 km), an optimistic case is assumed by keeping the evolution of 1-sigma for the altitude of 500 km.

    !     latitude= latitude (in deg, -90;90) 
    !     lhour   = local time (in hr, 0-24)
    !     dayear  = day of year (1-366)
    !     altitude= altitude (in km) greater than 120 km
    !     fpsolar = mean flux of last 81 days at t   =   fbar(1)
    !     kpindex = kp delayed by 3 hours = akp(1)

    !----------------------!
    ! DECLARATION VARIABLES!
    !----------------------!

    implicit none
    real :: latitude, lhour, dayear, altitude, fpsolar, kpindex, stdev ! argument parameter values
    real :: Copt_L, Copt_M, Copt_H ! projection coefficient in hyperplane equations
    real :: Res_L, Res_M, Res_H    ! direction coefficient in hyperplane equations
    real :: stdev_L, stdev_M, stdev_H  ! sigma values at different range of altitude
    ! sigma at low altitude (sigma < 200 km)
    real :: sigma_FP_L ! 1-sigma (Fp proxy)
    real :: sigma_KP_L ! 1-sigma (Kp index)
    real :: sigma_alat_L  !1-sigma in latitude
    real :: sigma_hl_L    !1-sigma in local hour
    real :: sigma_day_L   ! 1-sigma in day of year
    real :: sigma_alti_L  ! 1-sigma in altitude
    ! sigma at middle altitude (200 km <= sigma =< 500 km)
    real :: sigma_FP_M ! 1-sigma (Fp proxy)
    real :: sigma_KP_M ! 1-sigma (Kp index)
    real :: sigma_alat_M  !1-sigma in latitude
    real :: sigma_hl_M    !1-sigma in local hour
    real :: sigma_day_M   ! 1-sigma in day of year
    real :: sigma_alti_M  ! 1-sigma in altitude
    ! sigma at high altitude (sigma > 500 km)
    real :: sigma_FP_H ! 1-sigma (Fp proxy)
    real :: sigma_KP_H ! 1-sigma (Kp index)
    real :: sigma_alat_H  !1-sigma in latitude
    real :: sigma_hl_H    !1-sigma in local hour
    real :: sigma_day_H   ! 1-sigma in day of year
    real :: sigma_alti_H  ! 1-sigma in altitude

    !-----------------------!
    ! ONE-SIGMA CALCULATIONS!
    !-----------------------!

    altit : If(altitude.LT.200) then
    
        ! Global 1-sigma calculation by means of statistical binning of data (GOCE, CHAMP, GRACE and SWARM) and Hyperplan equation (6 dimensions) 
	! Polynomial extrapolation by fixing altitude value at 200 km

        Copt_L = 0.929
        Res_L = 112.79
        sigma_alat_L = std_alat_M(latitude)
        sigma_hl_L = std_hl_M(lhour)
        sigma_day_L = std_day_M(dayear)
        sigma_alti_L = std_alti_M(200.)
        sigma_FP_L = std_FP_M(fpsolar)
        sigma_KP_L = std_KP_M(kpindex)

        stdev = sigma_alat_L + sigma_hl_L + sigma_day_L + sigma_alti_L + sigma_FP_L + sigma_KP_L + Copt_L - Res_L
		

    ElseIf(altitude.GE.200.AND.altitude.LE.500) then
    
        ! Global 1-sigma calculation by means of statistical binning data (GOCE, CHAMP, GRACE and SWARM)) and Hyperplan equation (6 dimensions) 
	! Polynomial interpolation 

        Copt_M = 0.929
        Res_M = 112.79
        sigma_alat_M = std_alat_M(latitude)
        sigma_hl_M = std_hl_M(lhour)
        sigma_day_M = std_day_M(dayear)
        sigma_alti_M = std_alti_M(altitude)
        sigma_FP_M = std_FP_M(fpsolar)
        sigma_KP_M = std_KP_M(kpindex)

        stdev = sigma_alat_M + sigma_hl_M + sigma_day_M + sigma_alti_M + sigma_FP_M + sigma_KP_M + Copt_M - Res_M

  
    ElseIf(altitude.GT.500) then
    
        ! Global 1-sigma calculation by means of statistical binning data (GOCE, CHAMP, GRACE and SWARM) and Hyperplan equation (6 dimensions) 
	! Polynomial extrapolation by fixing altitude value at 500 km
    
    
        Copt_H = 0.929
        Res_H = 112.79
        sigma_alat_H = std_alat_M(latitude)
        sigma_hl_H = std_hl_M(lhour)
        sigma_day_H = std_day_M(dayear)
        sigma_alti_H = std_alti_M(500.)
        sigma_FP_H = std_FP_M(fpsolar)
        sigma_KP_H = std_KP_M(kpindex)
	
       
        stdev = sigma_alat_H + sigma_hl_H + sigma_day_H + sigma_alti_H + sigma_FP_H + sigma_KP_H + Copt_H - Res_H
	
	     
    Endif altit

contains

    !--------------------!
    ! ANALYTICAL FUNCTONS!
    !--------------------!

    ! General polynomial function

    function std_poly(x,n,a)
        integer :: h
        real :: std_poly
        integer, intent(in) :: n
        real, intent(in) :: x
        real, dimension(0:n) :: a
        h = 0
        std_poly = 0.
        Do h = 0,n
            std_poly = std_poly + a(h)*x**(h)
        enddo
    end function


    ! Solar Flux Fp
    !--------------

    function std_FP_M(x)
        real :: std_FP_M
        real, intent(in) :: x
        integer :: d
        real, dimension(0:1) :: c
        d = 1
        c(0) = 26.04
        c(1) = -3.36e-2
        std_FP_M = std_poly(x,d,c)
    end function std_FP_M

    ! Magnetic index Kp
    !------------------

    function std_KP_M(x)
        real :: std_KP_M
        real, intent(in) :: x
        integer :: d
        real, dimension(0:3) :: c
        d = 3
        c(0) = 22.47
        c(1) = 7.46e-1
        c(2) = -1.99e-1
        c(3) = 6.56e-2
        std_KP_M = std_poly(x,d,c)
    end function std_KP_M

    ! Latitude
    !---------

    function std_alat_M(x)
        real :: std_alat_M
        real, intent(in) :: x
        integer :: d
        real, dimension(0:2) :: c
        d = 2
        c(0) = 22.33
        c(1) = -7.19e-3
        c(2) = 4.72e-4
        std_alat_M = std_poly(x,d,c)
    end function std_alat_M

    ! Local hour
    !-----------

    function std_hl_M(x)
        real :: std_hl_M
        real, intent(in) :: x
        integer :: d
        real, dimension(0:3) :: c
        d = 3
        c(0) = 28.53
        c(1) = -3.95e-1
        c(2) = -5.64e-2
        c(3) = 3.15e-3
        std_hl_M = std_poly(x,d,c)
    end function std_hl_M

    ! Altitude
    !---------

    function std_alti_M(x)
        real :: std_alti_M
        real, intent(in) :: x
        integer :: d
        real, dimension(0:1) :: c
        d = 1
        c(0) = -9.73
        c(1) = 7.92e-2
        std_alti_M = std_poly(x,d,c)
    end function std_alti_M

    ! Day of year
    !------------
    ! Low solar flux regime

    function std_day_M(x)
        real :: std_day_M
        real, intent(in) :: x
        integer :: d
        real, dimension(0:1) :: c
        d = 1
        c(0) = 23.15
        c(1) = -3.91e-3
        std_day_M = std_poly(x,d,c)
    end function std_day_M


end subroutine
