program map_altitude
    use m_mcm, only: get_mcm_dens, get_mcm_temp, init_mcm

    implicit none

    real(8) :: altitude(3) = [90, 140, 300]  ! km

    character(*), parameter :: data_um = "/home/dala/swami/swami/data/um/"
    character(*), parameter :: data_dtm = "/home/dala/swami/swami/data/"
    character(100) :: fname

    real(8) :: f107 = 100, f107m = 100, kps(2) = [1, 1]
    real(8) :: doy = 180, aux
    integer :: i, j, k
    real(8) :: lat(7) = [(dble(i), i=-90, 90, 30)]
    real(8) :: lt(5) = [(dble(i), i=0, 24, 6)]

    real(8) :: temp(size(lat), size(lt))
    real(8) :: dens(size(lat), size(lt))

    ! Load the model
    call init_mcm(data_um, data_dtm)

    do k = 1, size(altitude)
        write (fname, '(A,I0.2,A)') "map_", k, ".dat"

        open (unit=100, file="map_altitude/temp_"//trim(fname))
        open (unit=101, file="map_altitude/dens_"//trim(fname))

        print *, fname
        temp(:, :) = 0d0
        dens(:, :) = 0d0

        ! Save metadata in temperature file
        write (100, *) "# altitude : ", altitude(k)
        write (100, *) "# latitudes : ", lat
        write (100, *) "# local_times : ", lt
        write (100, *) "# f10.7 : ", f107
        write (100, *) "# f10.7m : ", f107m
        write (100, *) "# doy : ", doy
        write (100, *) "# kp : ", kps

        ! Save metadata in density file
        write (101, *) "# altitude : ", altitude(k)
        write (101, *) "# latitudes : ", lat
        write (101, *) "# local_times : ", lt
        write (101, *) "# f10.7 : ", f107
        write (101, *) "# f10.7m : ", f107m
        write (101, *) "# kp : ", kps
        write (101, *) "# doy : ", doy

        do i = 1, size(lat)     ! loop latitude
            do j = 1, size(lt)  ! loop local time
                call get_mcm_dens(aux, altitude(k), lat(i), 0d0, lt(j), doy, f107, f107m, kps)
                dens(i, j) = aux
                call get_mcm_temp(aux, altitude(k), lat(i), 0d0, lt(j), doy, f107, f107m, kps)
                temp(i, j) = aux
            end do
            write (100, *) temp(i, :)
            write (101, *) dens(i, :)
        end do
        close (100)
        close (101)

    end do

end program map_altitude
