program altitude_profile

    use m_um
    use m_dtm
    use m_mcm

    implicit none

    character(*), parameter :: data_um = "/home/dala/swami/swami/data/um/"
    character(*), parameter :: data_dtm = "/home/dala/swami/swami/data/"

    integer :: i

    real(8) :: altitudes(51) = [(dble(i), i=0, 250, 5)]
    real(8) :: times(2) = [0, 180]
    real(8) :: local_times(5) = [(dble(i), i=0, 24, 6)]
    real(8) :: latitudes(7) = [(dble(i), i=-90, 90, 30)]
    real(8) :: f107(3) = [70d0, 140d0, 170d0]
    real(8) :: f107m(3) = [70d0, 140d0, 170d0]

    real(8) :: kps(2) = [1d0, 1d0]
    integer :: ih, it, ilt, ilat, isc
    character(len=100) :: fname
    real(8) :: temp, dens

    ! Initialise/load the model (calls lecdtm)
    call init_mcm(data_um, data_dtm)

    loop_sc: do isc = 1, size(f107)         ! solar cycle
    loop_t: do it = 1, size(times)           ! day of the year
    loop_lt: do ilt = 1, size(local_times)   ! local time
    loop_lat: do ilat = 1, size(latitudes)   ! latitude
        write (fname, '(A,I0.2,A,I0.2,A,I0.2,A,I0.2,A)') "profile_sc", isc, "_lt", ilt, "_lat", ilat, "_t", it, ".dat"
        open (unit=100, file="altitude_profile/"//"temp_"//fname)
        open (unit=101, file="altitude_profile/"//"dens_"//fname)
        write (*, *) "Opening file ", fname

        ! Save metadata in temperature file
        write (100, *) "# time       : ", times(it)
        write (100, *) "# local_times: ", local_times(ilt)
        write (100, *) "# latitudes  : ", latitudes(ilat)
        write (100, *) "# f10.7      : ", f107(isc)
        write (100, *) "# f10.7m     : ", f107m(isc)
        write (100, *) "# kp         : ", kps

        ! Save metadata in density file
        write (101, *) "# time       : ", times(it)
        write (101, *) "# local_times: ", local_times(ilt)
        write (101, *) "# latitudes  : ", latitudes(ilat)
        write (101, *) "# f10.7      : ", f107(isc)
        write (101, *) "# f10.7m     : ", f107m(isc)
        write (101, *) "# kp         : ", kps
        loop_h: do ih = 1, size(altitudes)

            ! MCM
            call get_mcm_temp(temp, altitudes(ih), latitudes(ilat), 0d0, local_times(ilt), times(it), &
                              f107(isc), f107m(isc), kps)
            call get_mcm_dens(dens, altitudes(ih), latitudes(ilat), 0d0, local_times(ilt), times(it), &
                              f107(isc), f107m(isc), kps)

            ! save results
            write (100, '(F15.6,F15.6)') altitudes(ih), temp
            write (101, '(F15.6,E15.6)') altitudes(ih), dens
        end do loop_h
        close (100)
        close (101)
    end do loop_lat
    end do loop_lt
    end do loop_t
    end do loop_sc

end program altitude_profile
