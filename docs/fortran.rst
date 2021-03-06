Fortran modules
===============

Compilation
-----------

Requirements
^^^^^^^^^^^^

Tested on Ubuntu 18.04 LTS

* `gfortran` (used version 7.5)
* `libnetcdf` for Fortran. More info at: https://www.unidata.ucar.edu/software/netcdf/docs-fortran/

These can be installed in Ubuntu using:


.. code-block:: bash

    sudo apt-get install gfortran
    sudo apt-get install libnetcdff-dev

How to compile
^^^^^^^^^^^^^^

Using gfortran and linking the netcdf library (which is the most tricky part).

This command will compile everything, but please look at both the `examples/build_examples.sh` and the `src/libswamif/build_tests.sh` scripts for more details.

.. code-block:: bash

    SRC='src/libswamif'

    gfortran -c $SRC/dtm2020_F107_Kp-subr_MCM.f90 $SRC/dtm2020_sigma_function.f90 \
        $SRC/m_dtm.f90 $SRC/m_interp.f90 $SRC/m_um.f90 $SRC/m_mcm.f90 \
        `nf-config --fflags --flibs` -Wall -pedantic -Warray-bounds -fbacktrace



A more complex setup would be:

.. code-block:: bash

    # path to the source code 
    SRC='src/libswamif'
    # flags for gfortran
    FLAGS='-Wall -pedantic -Warray-bounds -fbacktrace'
    # for the netcdff library
    L_NETCDF=$(nf-config --fflags --flibs)
    # for the interpolation module
    M_INTERP="$SRC/m_interp.f90"
    # for the UM module
    M_UM="$SRC/m_um.f90 $M_INTERP $L_NETCDF"
    # for the DTM module
    M_DTM="$SRC/dtm2020_F107_Kp-subr_MCM.f90 $SRC/dtm2020_sigma_function.f90 $SRC/m_dtm.f90"  
    # for the MCM module
    M_MCM="$M_UM $M_DTM $SRC/m_mcm.f90"

    gfortran $M_MCM


How to use
----------

In your fortran program, use the `use` statement to load the module. 
There are three modules with their most important functions.
More details on the units, the valid ranges and the definition in :ref:`Fortran interface`

It is important to call the `init_` subroutine at the beginning of the program.
It will load the required constants and keep them in memory throughtout the whole execution.


Fortran interface
-----------------


m_mcm: MOWA Climatological Model (MCM)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This module contains routines related to the MCM model, which combines the DTM (thermosphere) and the UM (lower atmosphere) to have a `MOdel of the Whole Atmosphere`

.. code-block:: fortran
    
    call init_mcm(path_to_data_um, path_to_data_dtm)
    call get_mcm_dens(dens, alti, lati, long, loct, doy, f107, f107m, kps(2))
    call get_mcm_temp(temp, alti, lati, long, loct, doy, f107, f107m, kps(2))
    call get_mcm(mcm_out, alti, lati, long, loct, doy, f107, f107m, kps(2))


.. f:autosubroutine:: m_mcm/init_mcm

.. f:autosubroutine:: m_mcm/get_mcm_dens

.. f:autosubroutine:: m_mcm/get_mcm_temp

.. f:autosubroutine:: m_mcm/get_mcm

Derived type ``t_mcm_out``

* **dens** *[real]*: Total density (in gram/cm3)
* **temp** *[real]*: Temperature at altitude (K)
* **wmm** *[real]*: Mean molecular mass (in gram)
* **d_H** *[real]*: Partial density of atomic hydrogen (in gram/cm3)
* **d_He** *[real]*: Partial density of helium
* **d_O** *[real]*: Partial density of atomic oxygen
* **d_N2** *[real]*: Partial density of molecular nitrogen
* **d_O2** *[real]*: Partial density of molecular oxygen
* **d_N** *[real]*: Partial density of atomic nitrogen
* **tinf** *[real]*: Exospheric temperature, in K
* **dens_unc** *[real]*: Density uncertainty from DTM2020 (above 120 km), as a percentage
* **dens_std** *[real]*: Standard deviation of the density (UM, below 100 km), in g/cm3
* **temp_std** *[real]*: Standard deviation of the temperature (UM, below 100 km), in K
* **xwind** *[real]*: Zonal wind, in m/s
* **ywind** *[real]*: Meridional wind, m/s
* **xwind_std** *[real]*: Standard deviation of zonal wind, in m/s
* **ywind_std** *[real]*: Standard deviation of neridional wind, m/s


m_dtm: Drag Temperature Model (DTM)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This module contains routines related to the DTM2020 model.

Go to :ref:`Drag Temperature Model (DTM2020)`) for some theoretical details about the model.

.. code-block:: fortran

    call init_dtm2020(path_to_data_dtm_file)
    call get_dtm2020(dens, temp, alti, lati, long, loct, doy, f107, f107m, kps(2))
    call get_dtm2020_dens_uncertainty(dens_unc, temp, alti, lati, long, loct, doy, f107, f107m, kps(2))


.. f:autosubroutine:: m_dtm/init_dtm2020


.. f:autosubroutine:: m_dtm/get_dtm2020


.. f:autosubroutine:: m_dtm/get_dtm2020_dens_uncertainty


Parameter ``DTM2020_DATA_FILENAME`` (*character*) is ``"DTM_2020_F107_Kp.dat"``

Derived type ``t_dtm_out``

* **dens** *[real]*: Total density (in gram/cm3)
* **temp** *[real]*: Temperature at altitude (K)
* **wmm** *[real]*: Mean molecular mass (in gram)
* **d_H** *[real]*: Partial density of atomic hydrogen (in gram/cm3)
* **d_He** *[real]*: Partial density of helium
* **d_O** *[real]*: Partial density of atomic oxygen
* **d_N2** *[real]*: Partial density of molecular nitrogen
* **d_O2** *[real]*: Partial density of molecular oxygen
* **d_N** *[real]*: Partial density of atomic nitrogen
* **tinf** *[real]*: Exospheric temperature, in K
* **dens_unc** *[real]*: Density uncertainty from DTM2020 (above 120 km), as a percentage


m_um: Unified Model (UM)
^^^^^^^^^^^^^^^^^^^^^^^^

This module contains routines related to the UM model.

Go to :ref:`Unified Model (UM)`) for some theoretical details about the model.

.. code-block:: fortran

    call init_um(path_to_data_um)
    call get_um_dens(dens, alti, lati, long, loct, doy, f107, f107m, kps(2))
    call get_um_temp(temp, alti, lati, long, loct, doy, f107, f107m, kps(2))
    call get_um_xwind(xwind, alti, lati, long, loct, doy, f107, f107m, kps(2))
    call get_um_ywind(ywind, alti, lati, long, loct, doy, f107, f107m, kps(2))
    call get_um_dens_standard_deviation(dens_std, alti, lati, long, loct, doy, f107, f107m, kps(2))
    call get_um_temp_standard_deviation(temp_std, alti, lati, long, loct, doy, f107, f107m, kps(2))
    call get_um_xwind_standard_deviation(xwind_std, alti, lati, long, loct, doy, f107, f107m, kps(2))
    call get_um_ywind_standard_deviation(ywind_std, alti, lati, long, loct, doy, f107, f107m, kps(2))


.. f:autosubroutine:: m_um/init_um


.. f:autosubroutine:: m_um/get_um_dens


.. f:autosubroutine:: m_um/get_um_temp


.. f:autosubroutine:: m_um/get_um_xwind


.. f:autosubroutine:: m_um/get_um_ywind


.. f:autosubroutine:: m_um/get_um_dens_standard_deviation


.. f:autosubroutine:: m_um/get_um_temp_standard_deviation


.. f:autosubroutine:: m_um/get_um_xwind_standard_deviation


.. f:autosubroutine:: m_um/get_um_ywind_standard_deviation


