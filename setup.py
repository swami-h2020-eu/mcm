from __future__ import absolute_import, division, print_function

from setuptools import setup

if __name__ == "__main__":

    with open("README.md") as f:
        long_description = f.read()

    setup(
        name="swami",
        version="1.0rc",
        author="SWAMI",
        author_email="swami@swami-h2020.eu",
        url="swami-h2020.eu",
        license="MIT",
        description="Space Weather Atmosphere Models and Indices",
        long_description=long_description,
        classifiers=[
            'Development Status :: 5 - Production/Stable',
            'Intended Audience :: Developers',
            'Natural Language :: English',
            'Programming Language :: Python :: 3.5',
            'Programming Language :: Python :: 3.6',
            'Programming Language :: Python :: 3.7',
            'Programming Language :: Python :: 3.8',
            'Programming Language :: Python :: 3.9',
        ],
        packages=["swami"],
        package_dir={"": "src", },
        python_requires='>=3.6',
        extras_require={
            "dev": ["pytest", "tox", "pylint", "coverage", "black"],
            "docs": ["sphinx",
                     "sphinx-rtd-theme",
                     "sphinx-fortran",
                     "nbsphinx",
                     "ipython"]
        },
        package_data={'swami': ['src/swami/data/*', 'src/swami/swami.x']},
        include_package_data=True,
        # data_files=[("data", "src/swami/data/*"), ]
    )
