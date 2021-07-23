! ---------------------------------------------------------------------
! – Project : SWAMI
! – Customer : N/A
! ---------------------------------------------------------------------
! – Author : Daniel Lubián Arenillas
! – Issue : 1.0
! – Date : 2021-03-31
! – Purpose : Single point program for the VSWMC
! ---------------------------------------------------------------------
! – Deimos Space SLU, CNES, MetOffice, 2021
! – All rights reserved
! ---------------------------------------------------------------------
!
! REQUIREMENTS (in Ubuntu 18.04)
!
! gfortran
! libnetcdff-dev: https://www.unidata.ucar.edu/software/netcdf/
!
! COMPILATION INSTRUCTIONS
!
! SRC=src/libswamif
! gfortran -c $SRC/dtm2020_F107_Kp-subr_MCM.f90 $SRC/dtm2020_sigma_function.f90 $SRC/m_dtm.f90 $SRC/m_interp.f90 $SRC/m_um.f90 $SRC/m_mcm.f90  `nf-config --fflags --flibs` -Wall -pedantic -Warray-bounds -fbacktrace
! gfortran -o swami_vswmc.x vswmc.f90 *.o `nf-config --fflags --flibs`
!
program wrapper

    use m_mcm, only: init_mcm, get_mcm, t_mcm_out

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
    real(8) :: kp1, kp2
    logical :: b_winds, b_unc_std
    type(t_mcm_out) :: res

    integer:: lun, ier


    namelist /input/ altitude, day_of_year, local_time, latitude, longitude, &
        f107, f107m, kp1, kp2, &
        b_unc_std, b_winds, &
        data_um, data_dtm, output_file

    call get_command_argument(1, input_file)
    if (trim(input_file) == "") then
        write(*, *) "Please provide an input file. Call './swami.x input_file.txt'"
        call exit(-1)
    end if

    open (file=input_file, newunit=lun, status="old", action="read", iostat=ier)
    if (ier /= 0) then
        write(*, *) "Error while opening input file: '", trim(input_file), "'. Error code is ", ier
    end if
    read (lun, nml=input)
    close (lun)

    kps = [kp1, kp2]

    ! Initialise/load the model
    call init_mcm(trim(data_um), trim(data_dtm))

    open (newunit=lun, file=output_file)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! MCM

    call get_mcm(mcm_out=res, &
                 alti=altitude, lati=latitude, longi=longitude, &
                 loct=local_time, doy=day_of_year, &
                 f107=f107, f107m=f107m, kps=kps, &
                 get_unc=b_unc_std, get_winds=b_winds)

    ! Write input values
    write (lun, 1000) "alti = ", altitude
    write (lun, 1000) "lati = ", latitude
    write (lun, 1000) "longi = ", longitude
    write (lun, 1000) "loct = ", local_time
    write (lun, 1000) "doy = ", day_of_year
    write (lun, 1000) "f107 = ", f107
    write (lun, 1000) "f107m = ", f107m
    write (lun, 1000) "kp1 = ", kp1
    write (lun, 1000) "kp2 = ", kp2

    ! Write output values
    write (lun, 1000) "dens = ", res%dens
    write (lun, 1000) "temp = ", res%temp
    write (lun, 1000) "wmm = ", res%wmm
    write (lun, 1000) "d_H = ", res%d_H
    write (lun, 1000) "d_He = ", res%d_He
    write (lun, 1000) "d_O = ", res%d_O
    write (lun, 1000) "d_N2 = ", res%d_N2
    write (lun, 1000) "d_O2 = ", res%d_O2
    write (lun, 1000) "d_N = ", res%d_N
    write (lun, 1000) "tinf = ", res%tinf
    write (lun, 1000) "dens_unc = ", res%dens_unc
    write (lun, 1000) "dens_std = ", res%dens_std
    write (lun, 1000) "temp_std = ", res%temp_std
    write (lun, 1000) "xwind = ", res%xwind
    write (lun, 1000) "ywind = ", res%ywind
    write (lun, 1000) "xwind_std = ", res%xwind_std
    write (lun, 1000) "ywind_std = ", res%ywind_std

1000 format(A, ES25.15)

    close (lun)

end program wrapper
