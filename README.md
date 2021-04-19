# SWAMI MCM Model

SWAMI (http://swami-h2020.eu/) was a H2020 project with the purpose of developing a model of the whole atmosphere by means of blending the Unified Model (UM) from the MetOffice in the UK for the atmosphere (0 to 120 km) and the Drag Temperature Model (DTM2020) from the Centre National d'Études Spatiales (CNES) in France covering the thermosphere, from 120 to 1500 km. The model, called MCM (MOWA Climatological Model), provides point-wise estimates of temperature, density and wind up to 120 km, and temperature, total and partial densities above 120 km. The drivers for the thermosphere model are the solar radio flux F10.7 and the planetary geomagnetic index Kp.

To be efficient and performant, the UM was averaged to a set of data tables that contain diurnal variability and cover three solar activity levels (in terms of F10.7). The DTM is included directly. Both models are combined by means of interpolation in the blending altitude range (between 100 and 120 km) to create MCM.

The thermosphere model DTM2020 is also available separately. Two versions exist: the operational version, which is used in MCM, and a more accurate research version. The drivers of the DTM2020 research model are the 30 cm solar radio flux F30 and the new hourly planetary geomagnetic index Hp60.


## Documentation

The documentation is available at https://swami-h2020-eu.github.io/mcm/

It includes API reference, brief descriptions of the model, examples and compilation instructions

## How to use

Clone the repository or [download the code](https://github.com/swami-h2020-eu/mcm/archive/refs/heads/main.zip).

Tested in Ubuntu 18.04 LTS

Requirements:
* gfortran
* python >3.5
* libnetcdff-dev

### Fortran code

MCM model is under `src/libswamif`

Two versions of DTM2020 model are at `src/dtm2020`

Compilation instructions are at: https://swami-h2020-eu.github.io/mcm/fortran.html

### Python wrapper

* First, go to `src/swami` and compile the Fortran binary by running `./make_wrapper.sh`
* To create the package: `python3 setup.py sdist`
* To install: `pip3 install .`. This will install a Python package called `swami`

More detailed instructions at: https://swami-h2020-eu.github.io/mcm/python.html
