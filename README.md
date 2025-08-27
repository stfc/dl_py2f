# DL_PY2F

# About

[`DL_PY2F`](https://github.com/stfc/dl_py2f) is an open-source library for the creation of modern interfaces and data structures in Python that can interoperate with existing scientific software written in Fortran and manipulate their data.
`DL_PY2F` is intended for use with Python-based software packages where data are managed using Python/NumPy and need to be accessed for computations performed by Fortran code. 
A great advantage of `DL_PY2F` for the application developers is that the attributes of the Python instance are conveniently retrieved by querying their names in a dictionary-like way on the Fortran side.
`DL_PY2F` also provides utility tools to facilitate initialisation and enhancement of the Python instances. 

## License

`DL_PY2F` is open source and released under [GNU Lesser General Public License v3.0](https://www.gnu.org/licenses/lgpl-3.0.en.html). It is available for download from the [repository](https://github.com/stfc/dl_py2f).

## Authors

`DL_PY2F` was created and implemented by You Lu at [STFC Scientific Computing](https://www.sc.stfc.ac.uk).

## Citing `DL_PY2F`

Applications using `DL_PY2F` should cite:

You Lu and Thomas W. Keal, *Journal of Open Source Software*, in preparation

## Project status

The Python-to-Fortran interoperability has been comprehensively tested using both GNU and Intel compilers.

:warning: **Warning:** However, the method for Fortran-to-Python interoperability is still undergoing testing and validation, and is currently limited to use with the GNU compiler gfortran, as the proprietary .mod file format used by the Intel compiler is not yet supported.

# Getting started

## Required libraries and tools


| Tools & Libraries             | Min. version | Note |
|:------------------------------|:-------------|:-----|
| gcc/gfortran                  | 7.3          |      |
| OR icc/ifort                  | 17           | :warning: |
| cmake                         | 3.16         |      |
| python3-dev                   | 3.8          |      |
| python3-numpy                 | 1.21.5       |      |

:bulb: The above package names are based on Ubuntu Linux. They may vary on other
       operating systems.

:warning: The Fortran-to-Python method does **NOT** yet work with the Intel compilers as Intel's proprietary
          .mod file format is unpublished and unsupported.

## Using and testing `DL_PY2F`

`DL_PY2F` is a mixed ABI/API library to be used by another software package. Here is a prototype package which is an example demonstrating in details how to compile and use `DL_PY2F`:

https://github.com/stfc/dl_py2f-example

The [example](https://github.com/stfc/dl_py2f-example) also works as a test bench for validating `DL_PY2F`. Please follow the instructions therein to compile and run it.

Apart from the source code on the [github repository](https://github.com/stfc/dl_py2f), `DL_PY2F` has also been published and deployed in a [launchpad.net PPA](https://launchpad.net/~dl-py2f/+archive/ubuntu/ppa) and can be installed systemwide using `apt`:

`sudo add-apt-repository ppa:dl-py2f/ppa`

`sudo apt update`

`sudo apt install dl-py2f`

:warning: Since Debian PPA does not allow underscore `_`, the package name published on [launchpad.net](https://launchpad.net/~dl-py2f/+archive/ubuntu/ppa) is **`dl-py2f`** but not `dl_py2f`.

:bulb: `sudo` access is required for installing system-wide packages using `apt`.

The use of system installed `DL_PY2F` is also demonstrated in the [example](https://github.com/stfc/dl_py2f-example) package.

For more information about the usage of `DL_PY2F` please also read the paper: [You Lu and Thomas W. Keal, *Journal of Open Source Software*, in preparation](https://joss.theoj.org/papers/).

# Miscellaneous

## Support

Please raise an Issue on the [project's page](https://github.com/stfc/dl_py2f) if you have a question or find an issue about the code.

## Contributing

Contributions are welcome in the form of Issue/PR on [github.com](https://github.com/stfc/dl_py2f).

## Acknowledgements

The `DL_PY2F` library was created during the redevelopment of [ChemShell](https://chemshell.org) as a Python-based package, which was funded by EPSRC under the grant [EP/K038419/1](https://gtr.ukri.org/projects?ref=EP/K038419/1). Ongoing support for the development of `DL_PY2F` as part of ChemShell is provided under EPSRC grants [EP/R001847/1](https://gtr.ukri.org/projects?ref=EP%2FR001847%2F1) and [EP/W014378/1](https://gtr.ukri.org/projects?ref=EP%2FW014378%2F1), and the [Computational Science Centre for Research Communities (CoSeC)](https://www.cosec.ac.uk), via the support provided to the [Materials Chemistry Consortium](https://mcc.hec.ac.uk). We acknowledge helpful discussions and suggestions for improvement from Paul Sherwood, Joseph Thacker, and Thomas Durrant.
