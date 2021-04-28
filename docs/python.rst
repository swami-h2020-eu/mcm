Python wrapper
==============

A Python wrapper has been developed and released to be able to make some quick plots and take a look at the results easily.

It consists on calling a compiled Fortran program, so the performance is far from good. 
Take that into account when using this Python wrapper.

Installation of the Python package
----------------------------------

System requirements (same as :ref:`Fortran modules` + the python part):

* Python >=3.6
* python-pip
* gfortran
* libnetcdff (more info at https://www.unidata.ucar.edu/software/netcdf/docs-fortran/)

To install the package, go to `src/swami` and execute in bash `make_wrapper.sh`.
It will generate a `swami.x` executable.

If you are using Ubuntu 18.04 64bits, the executable included in the distributed version should work.

Once that is done, install the python package in a virtual environment or in your preferred python distribution.

.. code-block:: bash

    python3 setup.py sdist
    pip install swami-1.0.tar.gz


Python interface
----------------

.. automodule:: swami
    :members: MCM,MCMOutput
