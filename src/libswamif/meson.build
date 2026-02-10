dtm_files = files(
  'dtm2020_sigma_function.f90',
  'dtm2020_F107_Kp-subr_MCM.f90',
  'm_dtm.f90',
  'm_interp.f90',
  'm_mcm.f90',
  'm_um.f90',
)

dtm_lib = shared_library(
    'dtm',
    dtm_files,
    dependencies: [netcdff_lib],
    install : true,
    install_dir: install_dir
)
