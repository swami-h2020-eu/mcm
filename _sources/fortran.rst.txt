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

.. f:autotype:: m_mcm/t_mcm_out

.. py:class:: t_mcm_out

   .. py:attribute:: dens
      :type: float

      Total density (in gram/cm3)

   .. py:attribute:: temp
      :type: float
      
      Temperature at altitude (K)

   .. py:attribute:: wmm
      :type: float

      Mean molecular mass (in gram)
   .. py:attribute:: d_H
      :type: float

      Partial density of atomic hydrogen (in gram/cm3)
   .. py:attribute:: d_He
      :type: float

      Partial density of helium
   .. py:attribute:: d_O
      :type: float

      Partial density of atomic oxygen
   .. py:attribute:: d_N2
      :type: float

      Partial density of molecular nitrogen
   .. py:attribute:: d_O2
      :type: float

      Partial density of molecular oxygen
   .. py:attribute:: d_N
      :type: float

      Partial density of atomic nitrogen
   .. py:attribute:: tinf
      :type: float

      Exospheric temperature, in K
   .. py:attribute:: dens_unc
      :type: float

      Density uncertainty from DTM2020 (above 120 km), as a percentage
   .. py:attribute:: dens_std
      :type: float

      Standard deviation of the density (UM, below 100 km), in g/cm3
   .. py:attribute:: temp_std
      :type: float

      Standard deviation of the temperature (UM, below 100 km), in K
   .. py:attribute:: xwind
      :type: float

      Zonal wind, in m/s
   .. py:attribute:: ywind
      :type: float

      Meridional wind, m/s
   .. py:attribute:: xwind_std
      :type: float

      Standard deviation of zonal wind, in m/s
   .. py:attribute:: ywind_std
      :type: float

      Standard deviation of neridional wind, m/s


m_dtm: Drag Temperature Model (DTM)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This module contains routines related to the DTM2020 model.

Go to :ref:`Drag Temperature Model (DTM2020)`) for some theoretical details about the model.

.. code-block:: fortran

    call init_dtm2020(path_to_data_dtm_file)
    call get_dtm2020(dens, temp, alti, lati, long, loct, doy, f107, f107m, kps(2))
    call get_dtm2020_dens_uncertainty(dens_unc, temp, alti, lati, long, loct, doy, f107, f107m, kps(2))


.. f:autovariable:: m_dtm/DTM2020_DATA_FILENAME

.. py:data:: DTM2020_DATA_FILENAME
   :type: string
   :value: "DTM_2020_F107_Kp.dat"

.. f:autosubroutine:: m_dtm/init_dtm2020

.. f:autosubroutine:: m_dtm/get_dtm2020

.. f:autosubroutine:: m_dtm/get_dtm2020_dens_uncertainty

.. py:class:: t_mcm_out

      .. py:attribute:: dens
         :type: float
      
         Total density (in gram/cm3)

      .. py:attribute:: temp
         :type: float
      
         Temperature at altitude (K)

      .. py:attribute:: wmm
         :type: float
      
         Mean molecular mass (in gram)
      .. py:attribute:: d_H
         :type: float
      
         Partial density of atomic hydrogen (in gram/cm3)

      .. py:attribute:: d_He
         :type: float
      
         Partial density of helium
      .. py:attribute:: d_O
         :type: float
      
         Partial density of atomic oxygen

      .. py:attribute:: d_N2
         :type: float
      
         Partial density of molecular nitrogen

      .. py:attribute:: d_O2
         :type: float
      
         Partial density of molecular oxygen

      .. py:attribute:: d_N
         :type: float
      
         Partial density of atomic nitrogen

      .. py:attribute:: tinf
         :type: float
      
         Exospheric temperature, in K


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

