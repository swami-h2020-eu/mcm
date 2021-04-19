Unified Model (UM)
==================


Description of the model
------------------------

The UM is the Met Office weather and climate model. In most applications the
UM has an upper boundary at around 85 km in altitude, but here we use and
describe an extended UM which has an upper boundary at 152 km.
In order to run at these higher altitudes a number of changes to the UM have been
made. The radiation scheme has been altered to represent non local
thermodynamical equilibrium (LTE) longwave cooling and shortwave heating.
This leads to more accurate heating rates in the mesosphere and lower
thermosphere. The radiation scheme also been extended to include shorter
wavelengths in the extreme and far ultraviolet range (EUV and FUV). This results
in more accurate heating rates in the thermosphere and also provides photolysis
rates which can be used in future to drive exothermic heating via the UM
chemistry scheme. This feature in not available in the current UM version and so
here the exothermic heating is approximately represented by Newtonian relaxation
(“nudging”) to climatological temperatures in the lower thermosphere.

Three 1 year-long UM simulations were run to provide input data for the MCM:
Jan-Dec 2002 (solar maximum), Jan-Dec 2004 (solar median) and Jul 2008 – Jun
2009 (solar minimum). The UM output was regridded to a lower resolution of 10º
latitude x 15º longitude for processing. Then the mean and standard deviation of
the migrating tidal signal, and the mean and standard deviation of the total field
(minus any migrating tidal signal) were calculated for each month. These
calculations were carried out for neutral density, temperature, zonal wind and
meridional wind.

Subroutines for the UM part of the MCM model are available in the :ref:`m_um: Unified Model (UM)` module.


Solar cycle classification
--------------------------

Three years were chosen to represent three corresponding levels in terms of solar activity.

The driver to select between each year is the F10.7 flux 81-day average (`f107m` argument in the code) following this rule:

+---------------------+----------------------+-----------------------+
| Year                | Solar activity level | `f107m`               |
+---------------------+----------------------+-----------------------+
| Jan-Dec 2002        | High                 | > 160.0               |
+---------------------+----------------------+-----------------------+
| Jan-Dec 2004        | Medium               | between 120 and 160   |
+---------------------+----------------------+-----------------------+
| Jul 2008 – Jun 2009 | Low                  | < 120.0               |
+---------------------+----------------------+-----------------------+


Standard deviation
------------------

The UM model also provides the standard deviation of the temperature and the density through the :func:`m_um/get_um_temp_standard_deviation` and :func:`m_um/get_um_dens_standard_deviation` subroutines, respectively.


Winds
-----

X wind and Y wind denote zonal wind and meridional wind, respectively. 
Zonal wind is the horizontal wind in the east-west direction (eastward positive, westward negative) 
and meridional wind is the horizontal wind in  the north-south direction (northward positive, southward negative).

There are two subroutines available to retrieve them: :func:`m_um/get_um_xwind` and :func:`m_um/get_um_ywind`




.. admonition:: Contact information

   Dr. David Jackson (Met Office, UK)
   
   david.jackson [at] metoffice.gov.uk