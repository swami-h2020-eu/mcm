program point

    use m_um
    use m_dtm
    use m_mcm

    implicit none

    ! Path where to find the UM netCDF files in folders 2002, 2004, 2008-2009
    character(*), parameter :: data_um = "/home/dala/swami/swami/data/um/"
    ! Path where to find the "DTM_2020_F107_Kp.dat" file
    character(*), parameter :: data_dtm = "/home/dala/swami/swami/data/"

    real(8) :: altitude = 150.0d0   ! km
    real(8) :: day_of_year = 53.0d0    ! days
    real(8) :: local_time = 12.0d0    ! hours
    real(8) :: latitude = 0d0       ! degrees
    real(8) :: longitude = 15d0      ! degrees
    real(8) :: f107 = 140d0     ! F10.7
    real(8) :: f107m = 139d0     ! F10.7 average
    real(8) :: kps(2) = [1d0, 1d0]      ! Kp

    real(8) :: temp, dens, std_dens, std_temp, unc

    ! Initialise/load the model
    call init_mcm(data_um, data_dtm)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! MCM

    ! Get the temperature (K) using the MCM model
    call get_mcm_temp(temp, altitude, latitude, longitude, local_time, day_of_year, &
                      f107, f107m, kps)

    ! Get the density (g/cm3) using the MCM model
    call get_mcm_dens(dens, altitude, latitude, longitude, local_time, day_of_year, &
                      f107, f107m, kps)

    print *, temp, dens

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! UM  (up to 152km)

    ! Get the temperature (K) using the UM model
    call get_um_temp(temp, altitude, latitude, longitude, local_time, day_of_year, &
                     f107, f107m, kps)

    ! Get the density (g/cm3) using the UM model
    call get_um_dens(dens, altitude, latitude, longitude, local_time, day_of_year, &
                     f107, f107m, kps)

    ! Standard deviation for density (g/cm3) and temperature (K)
    call get_um_dens_standard_deviation(std_dens, altitude, latitude, longitude, local_time, &
                                        day_of_year, f107, f107m, kps)
    call get_um_temp_standard_deviation(std_temp, altitude, latitude, longitude, local_time, &
                                        day_of_year, f107, f107m, kps)

    print *, temp, dens, std_dens, std_temp

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! DTM2020 (above 120 km)

    ! Get the temperature (K) density (g/cm3) using the DTM2020 model
    call get_dtm2020(dens, temp, altitude, latitude, longitude, local_time, day_of_year, &
                     f107, f107m, kps)

    ! Get the uncertainty (%) of the density
    call get_dtm2020_dens_uncertainty(unc, altitude, latitude, longitude, local_time, &
                                      day_of_year, f107, f107m, kps)

    print *, temp, dens, unc

end program point
