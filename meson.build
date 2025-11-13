project('swarmi', 'fortran',
  version : '1.0.1',
  license: 'MIT',
  meson_version: '>=0.64.0',
  default_options : ['warning_level=2'],
)

# Set up python
py_mod = import('python')
py = py_mod.find_installation(pure: false)
py_dep = py.dependency()

# Derive the package directory as installation path for the binary
install_dir = py.get_install_dir() / 'swami/'

# Find netcdf libs
netcdff_lib = dependency('netcdf', language: 'fortran')

# Step into subdirectories where the build happens.
subdir('src')

install_subdir(
    'data/',
    install_dir: install_dir
)

message('✔ Configuration for swami complete.')
message('Python version:      ' + py.version())
message('Install location:    ' + py.get_install_dir())
