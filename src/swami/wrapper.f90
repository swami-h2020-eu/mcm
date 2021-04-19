! ---------------------------------------------------------------------
! – Project : SWAMI
! – Customer : N/A
! ---------------------------------------------------------------------
! – Author : Daniel Lubián Arenillas
! – Issue : 1.0
! – Date : 2021-03-31
! – Purpose : Single point program for the Python wrapper
! ---------------------------------------------------------------------
! – © Copyright Deimos Space SLU, 2021
! – All rights reserved
! ---------------------------------------------------------------------


program wrapper

    use m_um
    use m_dtm
    use m_mcm

    implicit none

    real(8) :: altitude
    real(8) :: day_of_year
    real(8) :: local_time
    real(8) :: latitude
    real(8) :: longitude
    real(8) :: f107
    real(8) :: f107m
    real(8) :: kps(2)
    ! Path where to find the UM netCDF files in folders 2002, 2004, 2008-2009
    character(len=4096) :: data_um
    ! Path where to find the "DTM_2020_F107_Kp.dat" file
    character(len=4096) :: data_dtm
    character(len=4096) :: output_file, input_file

    integer:: lun

    real(8) :: temp, dens, std_dens, std_temp, unc, kp1, kp2
    logical :: bMCM, bDTM, bUM, bUMstd, bDTMunc

    namelist /input/ altitude, day_of_year, local_time, latitude, longitude, &
        f107, f107m, kp1, kp2, &
        bMCM, bDTM, bUM, bUMstd, bDTMunc, &
        data_um, data_dtm, output_file

    call get_command_argument(1, input_file)

    open(file=input_file, newunit=lun, status="old", action="read")
    read(lun, nml=input)
    close(lun)

    kps = [kp1, kp2]

    ! Initialise/load the model
    call init_mcm(trim(data_um), trim(data_dtm))

    open (newunit=lun, file=output_file)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! MCM

    if (bMCM) then
        ! Get the temperature (K) using the MCM model
        call get_mcm_temp(temp, altitude, latitude, longitude, local_time, day_of_year, &
                          f107, f107m, kps)

        ! Get the density (g/cm3) using the MCM model
        call get_mcm_dens(dens, altitude, latitude, longitude, local_time, day_of_year, &
                          f107, f107m, kps)

        write (lun, *) "MCM:temp:", temp
        write (lun, *) "MCM:dens:", dens
    end if

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! UM  (up to 152km)
    if (bUM .and. (altitude < 150d0)) then
        ! Get the temperature (K) using the UM model
        call get_um_temp(temp, altitude, latitude, longitude, local_time, day_of_year, &
                         f107, f107m, kps)

        ! Get the density (g/cm3) using the UM model
        call get_um_dens(dens, altitude, latitude, longitude, local_time, day_of_year, &
                         f107, f107m, kps)
        write (lun, *) "UM:temp:", temp
        write (lun, *) "UM:dens:", dens
    end if

    if (bUMstd .and. (altitude < 150d0)) then
        ! Standard deviation for density (g/cm3) and temperature (K)
        call get_um_dens_standard_deviation(std_dens, altitude, latitude, longitude, local_time, &
                                            day_of_year, f107, f107m, kps)
        call get_um_temp_standard_deviation(std_temp, altitude, latitude, longitude, local_time, &
                                            day_of_year, f107, f107m, kps)

        write (lun, *) "UM:temp_std:", std_temp
        write (lun, *) "UM:dens_std:", std_dens
    end if

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! DTM2020 (above 120 km)

    if (bDTM .and. (altitude > 120d0)) then
        ! Get the temperature (K) density (g/cm3) using the DTM2020 model
        call get_dtm2020(dens, temp, altitude, latitude, longitude, local_time, day_of_year, &
                         f107, f107m, kps)
        write (lun, *) "DTM2020:temp:", temp
        write (lun, *) "DTM2020:dens:", dens
    end if
    if (bDTMunc .and. (altitude > 120d0)) then
        ! Get the uncertainty (%) of the density
        call get_dtm2020_dens_uncertainty(unc, altitude, latitude, longitude, local_time, &
                                          day_of_year, f107, f107m, kps)

        write (lun, *) "DTM2020:dens_unc:", unc
    end if

    close (lun)

end program wrapper
