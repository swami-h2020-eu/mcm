#! /usr/bin/env bash

SRC='../libswamif'

FLAGS='-Wall -pedantic -Warray-bounds -fbacktrace'
L_NETCDF=$(nf-config --fflags --flibs)  # -I/usr/include
                                        # -L/usr/lib -lnetcdff -Wl,-Bsymbolic-functions -Wl,-z,relro -Wl,-z,now -lnetcdf -lnetcdf
M_INTERP="$SRC/m_interp.f90"
M_UM="$M_INTERP $SRC/m_um.f90"
M_DTM="$SRC/dtm2020_sigma_function.f90 $SRC/dtm2020_F107_Kp-subr_MCM.f90 $SRC/m_dtm.f90"
M_MCM="$M_UM $M_DTM $SRC/m_mcm.f90"

gfortran wrapper.f90 $M_MCM $FLAGS $L_NETCDF -o swami.x && echo "Success!!"