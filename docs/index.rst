.. SWAMI documentation master file, created by
   sphinx-quickstart on Thu Oct 31 16:32:05 2019.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

SWAMI - MCM model
=================

SWAMI (http://swami-h2020.eu/) was a H2020 project with the purpose of developing a model of the whole atmosphere by means of blending the Unified Model (UM) from the MetOffice in the UK for the atmosphere (0 to 120 km) and the Drag Temperature Model (DTM2020) from the Centre National d'Études Spatiales (CNES) in France covering the thermosphere, from 120 to 1500 km.
The model, called MCM (MOWA Climatological Model), provides point-wise estimates of temperature, density and wind up to 120 km, and temperature, total and partial densities above 120 km. The drivers for the thermosphere model are the solar radio flux F10.7 and the planetary geomagnetic index Kp.

To be efficient and performant, the UM was averaged to a set of data tables that contain diurnal variability and cover three solar activity levels (in terms of F10.7). 
The DTM is included directly.
Both models are combined by means of interpolation in the blending altitude range (between 100 and 120 km) to create MCM.

The thermosphere model **DTM2020** is also available separately. Two versions exist: the operational version, which is used in MCM, and a more accurate research version. The drivers of the DTM2020 research model are the 30 cm solar radio flux F30 and the new hourly planetary geomagnetic index Hp60.

.. toctree::
   :maxdepth: 2
   :caption: Index

   SWAMI Website <http://swami-h2020.eu/>
   first-steps.rst
   um.rst
   dtm2020.rst
   fortran.rst
   fortran_examples.rst
   python.rst
   Python_example.ipynb


Acknowledgements
----------------

Contact information: http://swami-h2020.eu/contact-us/

.. |cnes_logo| image:: _static/cnes.png
   :scale: 80%
   :align: middle
.. |mo_logo| image:: _static/mo.png
   :scale: 70%
   :align: middle
.. |deimos_logo| image:: _static/deimos.jpg
   :scale: 80%
   :align: middle
.. |gfz_logo| image:: _static/gfz.png
   :scale: 70%
   :align: middle

+---------------------------------+----------------------------------------+
| |deimos_logo|                   | |cnes_logo|                            |
+---------------------------------+----------------------------------------+
| Deimos Space                    | CNES                                   |
+---------------------------------+----------------------------------------+
| Spain                           | France                                 |
+---------------------------------+----------------------------------------+
| http://www.deimos-space.com/en/ | https://cnes.fr/en                     |
+---------------------------------+----------------------------------------+
| |mo_logo|                       | |gfz_logo|                             |
+---------------------------------+----------------------------------------+
| MetOffice                       | GFZ-Postdam                            |
+---------------------------------+----------------------------------------+
| UK                              | Germany                                |
+---------------------------------+----------------------------------------+
| https://www.metoffice.gov.uk/   | https://www.gfz-potsdam.de/startseite/ |
+---------------------------------+----------------------------------------+


The `SWAMI <https://cordis.europa.eu/project/id/776287/en>`_ project has received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement No 776287. 

.. image:: _static/eu.jpg
   :scale: 30 %