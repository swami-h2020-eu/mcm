program winds
    use m_um, only: get_um_xwind, get_um_ywind, init_um

    implicit none

    ! Path where to find the UM netCDF files in folders 2002, 2004, 2008-2009
    character(*), parameter :: data_um = "/home/dala/swami/swami/data/um/"
    ! Path where to find the "DTM_2020_F107_Kp.dat" file
    character(*), parameter :: data_dtm = "/home/dala/swami/swami/data/"

    real(8) :: altitude = 60.0d0   ! km
    real(8) :: day_of_year = 53.0d0    ! days
    real(8) :: local_time = 12.0d0    ! hours
    real(8) :: latitude = 0d0       ! degrees
    real(8) :: longitude = 15d0      ! degrees
    real(8) :: f107 = 140d0     ! F10.7
    real(8) :: kps(2) = [1d0, 1d0]      ! Kp
    real(8) :: f107m

    real(8) :: x_wind, y_wind

    ! Init model (load path)
    call init_um(data_um)


    ! Solar cycle 1: f107m < 120
    print*, "Solar cycle 1: f107m < 120"
    f107m = 70d0
    call get_um_xwind(x_wind, altitude, latitude, longitude, local_time, day_of_year, f107, f107m, kps)
    call get_um_ywind(y_wind, altitude, latitude, longitude, local_time, day_of_year, f107, f107m, kps)
    print*, "f107m = ", f107m, "x_wind = ", x_wind, "y_wind = ", y_wind
    
    ! Solar cycle 2: 120 < f107m < 160
    print*, "Solar cycle 2: 120 < f107m < 160"
    f107m = 140d0
    call get_um_xwind(x_wind, altitude, latitude, longitude, local_time, day_of_year, f107, f107m, kps)
    call get_um_ywind(y_wind, altitude, latitude, longitude, local_time, day_of_year, f107, f107m, kps)
    print*, "f107m = ", f107m, "x_wind = ", x_wind, "y_wind = ", y_wind
    
    ! Solar cycle 3: f107m > 160
    print*, "Solar cycle 3: f107m > 160"
    f107m = 170d0
    call get_um_xwind(x_wind, altitude, latitude, longitude, local_time, day_of_year, f107, f107m, kps)
    call get_um_ywind(y_wind, altitude, latitude, longitude, local_time, day_of_year, f107, f107m, kps)
    print*, "f107m = ", f107m, "x_wind = ", x_wind, "y_wind = ", y_wind


end program winds