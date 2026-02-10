swami_wrapper_files = files('wrapper.f90')

python_sources = files('__init__.py')

executable(
    'swami.x',
    swami_wrapper_files,
    dependencies: [netcdff_lib],
    link_with: [dtm_lib],
    install: true,
    install_dir: install_dir
    )

py.install_sources(
    python_sources,
    subdir: 'swami',
)
