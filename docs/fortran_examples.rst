Fortran examples
================

.. note:: Compilation script below

Single point
------------

This program shows how to compute the temperature and the density for a single point using MCM, DTM and UM.
Also, it shows how to get the uncertainties.

Filename: examples/fortran/point.f90

.. literalinclude:: ../examples/fortran/point.f90
    :language: fortran
    :linenos:


Altitude profile
----------------

This program is useful to get the density and temperature profiles at several points.

Filename: examples/fortran/altitude_profile.f90

In examples/fortran it is included a Python script that can plot the generated .dat files.

.. literalinclude:: ../examples/fortran/altitude_profile.f90
    :language: fortran
    :linenos:


Map at altitude
---------------

This program is useful to get the density and temperature maps at different altitudes.

Filename: examples/fortran/map_altitude.f90

In examples/fortran it is included a Python script that can plot the generated .dat files.

.. literalinclude:: ../examples/fortran/map_altitude.f90
    :language: fortran
    :linenos:


UM Winds
--------

This example shows how to get the UM winds at a single point.

Filename: examples/fortran/winds.f90

.. literalinclude:: ../examples/fortran/winds.f90
    :language: fortran
    :linenos:


Compilation script
------------------

.. literalinclude:: ../examples/fortran/compile.sh
    :language: bash
    :linenos: