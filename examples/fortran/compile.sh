#!/usr/bin/env bash

SRC='../../src/libswamif'
FLAGS='-Wall -pedantic -Warray-bounds -fbacktrace'
# FLAGS=''
L_NETCDF=$(nf-config --fflags --flibs)
M_INTERP="$SRC/m_interp.f90"
M_UM="$M_INTERP $SRC/m_um.f90"
M_DTM="$SRC/dtm2020_F107_Kp-subr_MCM.f90 $SRC/dtm2020_sigma_function.f90 $SRC/m_dtm.f90"
M_MCM="$M_UM $M_DTM $SRC/m_mcm.f90"

echo ">>> Delete old tests"
rm *.o
rm *.mod
rm *.x
rm *.log
rm *.test
rm *.dat
rm *.png


echo ">>> SINGLE POINT"
gfortran $M_MCM point.f90 -o point.x $FLAGS $L_NETCDF && echo "Compilation point succeded!"
# ./point.x


echo ">>> ALTITUDE PROFILE"
mkdir -p altitude_profile
gfortran $M_MCM altitude_profile.f90 -o altitude_profile.x $FLAGS $L_NETCDF && echo "Compilation altitude_profiles succeded!"
# ./altitude_profile.x

echo ">>> map ALTITUDE"
mkdir -p map_altitude
gfortran $M_MCM map_altitude.f90 -o map_altitude.x $FLAGS $L_NETCDF && echo "Compilation map_altitude succeded!"
# ./map_altitude.x

echo ">>> WINDS"
gfortran $M_MCM winds.f90 -o winds.x $FLAGS $L_NETCDF && echo "Compilation winds succeded!"
# ./winds.x