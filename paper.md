---
title: 'DL_PY2F: A library for Python-Fortran interoperability'
tags:
  - Python
  - Fortran
  - interoperability
authors:
  - name: You Lu
    orcid: 0000-0002-7524-4179
    corresponding: true
    equal-contrib: false
    affiliation: 1
  - name: Thomas W. Keal
    orcid: 0000-0001-8747-3975
    equal-contrib: false
    affiliation: 1
affiliations:
 - name: STFC Scientific Computing, Daresbury Laboratory, United Kingdom
   index: 1
date: 2 July 2025
bibliography: paper.bib
---

# Summary

Fortran is long established as one of the major programming languages for scientific software programs, but has only limited facilities for interoperating with other modern languages such as Python. `DL_PY2F` is an open-source library for the creation of modern interfaces and data structures in Python that can interoperate with existing scientific software written in Fortran and manipulate their data.

# Statement of need

`DL_PY2F` was created to facilitate the redevelopment of the computational chemistry environment `ChemShell` [@chemshell], which contains a number of Fortran modules and interfaces to external Fortran software. The redevelopment involved a new Python-based user interface, a modern Python software architecture, and NumPy-based core data structures. A key criterion for the redeveloped `Py-ChemShell` package [@lu2018; @lu2023] was ensuring the direct accessibility of its core Python data structures in the Fortran modules and interfaces. `DL_PY2F` achieves this interoperability using the Python `ctypes` and `numpy.ctypeslib` libraries and relevant features in the Fortran 2003 standard, in particular `iso_c_binding`. In `Py-ChemShell`, class instances and NumPy array data such as molecular coordinates, energy gradients, and Hessian matrices, are mapped to Fortran pointers via memory addresses by `DL_PY2F`. A simple Fortran 2003 interface is all that is required to access the data. As a result, developers benefit from a seamless Python user interface experience with native data objects, while direct access to data is possible for numerically intensive computing tasks in the Fortran code. 

While the `DL_PY2F` library is vital for the `Py-ChemShell` program, it has the potential to be applied in other situations in which Fortran code could benefit from integration into a wider Python software environment. Therefore, we have released `DL_PY2F` as an independent, general-purpose library for Python-Fortran interoperability.

## Python-to-Fortran interoperability

`DL_PY2F` is intended for use with Python-based software packages where data are managed using Python/NumPy and need to be accessed for computations performed by Fortran code. A pre-compiled shared object (dynamic library) containing a Fortran 2003 interface to the existing Fortran application is loaded using Python's `ctypes.CDLL`, as follows:

    import ctypes, dl_py2f
    libapp = ctypes.CDLL('/abc/edf/libapp.so')
    appRef  = dl_py2f.py2f(appObj)
    ierror = libapp.interface_app(appRef)

Here, `appObj` is an instance, typically created by the user at runtime, of the new package's Python class `App` which inherits from `ctypes.Structure`, for example:

    import ctypes, numpy
    from . import callback
    class App(ctypes.Structure):
        _kwargs = {
            'callback':callback.callback,
            'child'   :Child(),
            'coords'  :numpy.zeros(shape=(1000,3), dtype=numpy.float64),
            'npoints' :1000}

The instance's member attributes are declared in a Python dictionary `App._kwargs` (with default values) and these become visible to the Fortran application after `appObj` is read by the `dl_py2f.py2f` method. `DL_PY2F` supports a wide range of Python data types, with some illustrative examples given in the example above. Supported data types include 1- and 2-dimensional arrays (`numpy.ndarray`, `numpy.recarray`) and scalar values such as `int`, `float`, and `str` that are commonly needed in scientific computing. `dl_py2f.py2f` works recursively, so that `appObj` may contain unlimited levels of child instances (e.g., `appObj.child` in the above example). `DL_PY2F` provides utility tools to facilitate initialisation and enhancement of an instance. Please see the example application provided in the `DL_PY2F-example` [repository](https://github.com/stfc/dl_py2f-example) for further details. Callback functions to facilitate two-way data communication are also supported by `DL_PY2F`.

On the Fortran side, the `interface_app` function receives the passed-in `appPtr` – a pointer to the Python object – and bookkeeps it in a `dictType` instance `PyApp`, which is a linked list with support for child instances (e.g., `PyChild` in the code example):

    module AppModule
        use, intrinsic :: iso_c_binding
        use DL_PY2F, only: PyType, ptr2dict
        private
        abstract interface
            integer(c_long) function callback() bind(c)
                use iso_c_binding
            endfunction callback
        endinterface
        type(dictType)      , pointer, public :: PyApp
        procedure(callback) , pointer, public :: PyCallback
        contains
        function interface_app(appPtr) bind(c) result(ireturn)
            implicit none
            type(PyType)           , intent(in) :: appPtr
            type(c_funptr)                      :: pyfuncPtr
            type(PyType)  , pointer             :: childPtr
            real(kind=8)  , pointer             :: coords(:,:)
            integer                             :: npoints
            allocate(PyApp, source=ptr2dict(appPtr))
            call PyApp%get('child', childPtr)
            allocate(PyChild, source=ptr2dict(childPtr))
            allocate(coords(3,npoints))
            call PyApp%get('coords', coords)
            call PyApp%get('callback', pyfuncPtr)         
            call c_f_procpointer(pyfuncPtr, PyCallback)
            ! run the original application
            call my_app(npoints, coords)
            call PyApp%set('coords', coords)
            deallocate(PyApp, coords)
        endfunction interface_app
    endmodule AppModule

A great advantage of `DL_PY2F` for the application developers is that the attributes of the Python instance are conveniently retrieved by querying their names in a dictionary-like way. Two type-bound procedures `get` and `set` grant read and write access, respectively. For arrays, no copies are made because both operations act through memory addresses and values are thus changed in place and reflected on the Python side (if mutable). Callback functions can also be invoked, as follows:

    subroutine get_something(coords, npoints)
        use AppModule, only: PyApp, PyCallback
        real(kind=8)         , intent(in) :: coords(3,npoints)
        real(kind=8), pointer             :: buffer(:,:)
        call PyCallback()
        allocate(buffer(3,npoints))
        call PyApp%get('coords', buffer)
        coords = buffer
        deallocate(buffer)
    endsubroutine

`DL_PY2F`'s Python-to-Fortran interoperability has been comprehensively tested using both GNU and Intel compilers.

## Fortran-to-Python interoperability

While the Python-to-Fortran interoperability described above is recommended for new or redeveloped Python projects, other applications may benefit from interoperability using Python wrappers around their existing Fortran codes. For this `DL_PY2F` offers a second method based on analysis of the symbols in a pre-compiled shared object and parsing of the Fortran module files, which are assumed to be kept at compiletime. In this method Python's dot syntax may be used to access Fortran entities, for example, in a Python function invoked by the application at runtime:

    import dl_py2f
    libapp = dl_py2f.DL_DL('/abc/edf/libapp.so')
    libapp.moddir = '/abc/edf/modules'
    libapp.modules.my_mod.b.coords[1,2]  =  1.2345
    libapp.modules.my_mod.b.a[2,:].ibuff =  2025
    libapp.modules.my_mod.b.cbuff        = 'dl_py2f'

given that the original application's Fortran code contains:

    module my_mod
        implicit none
        type type_a
            integer :: ibuff
        endtype type_a
        type type_b
            type(type_a)                   :: a(5,6)
            character(len=15)              :: cbuff
            real(kind=8)     , allocatable :: coords(:,:)
        endtype type_b
        type(type_b) :: b
    endmodule my_mod

All Python attributes, including arrays of numbers and derived-type instances, are automatically instantiated as soon as an instance of class `dl_py2f.DL_DL` is created and a path to the module files is specified. In comparison with other Fortran-Python coupling libraries such as [F2PY](https://numpy.org/doc/stable/f2py/) and [gfort2py](https://github.com/rjfarmer/gfort2py), `DL_PY2F` does not provide compile tools which are only suitable for small pieces of Fortran code. Furthermore, `DL_PY2F` does not intervene in the Fortran applications' procedures, which normally involve a few parameters and are not compiled as exported symbols in shared objects. Instead, we focus on enabling modifications of application behaviour by manipulating computation data with inserted Python methods. The seamless access to Fortran data empowered by `DL_PY2F` will be particularly useful for machine-learning enhanced scientific computing, and is currently being trialled with the established computational chemistry codes `DL-FIND` [@kaestner2009] and `DL_POLY` [@devereux2025]. Note that this second method for Fortran-to-Python interoperability in `DL_PY2F` is still undergoing testing and validation, and is currently limited to use with the GNU compiler gfortran, as the proprietary .mod file format used by the Intel compiler [@green2024] is not yet supported.

# Obtaining `DL_PY2F`
`DL_PY2F` is an open-source library released under [GNU Lesser General Public License v3.0](https://www.gnu.org/licenses/lgpl-3.0.en.html). It is available for download from the [repository](https://github.com/stfc/dl_py2f). There is also a comprehensive [example](https://github.com/stfc/dl_py2f-example) demonstrating how to use `DL_PY2F` in an application project.

# Acknowledgements

The `DL_PY2F` library was created during the redevelopment of [ChemShell](https://chemshell.org) as a Python-based package, which was funded by EPSRC under the grant [EP/K038419/1](https://gtr.ukri.org/projects?ref=EP/K038419/1). Ongoing support for the development of `DL_PY2F` as part of ChemShell is provided under EPSRC grants [EP/R001847/1](https://gtr.ukri.org/projects?ref=EP%2FR001847%2F1) and [EP/W014378/1](https://gtr.ukri.org/projects?ref=EP%2FW014378%2F1), and the [Computational Science Centre for Research Communities (CoSeC)](https://www.cosec.ac.uk), via the support provided to the [Materials Chemistry Consortium](https://mcc.hec.ac.uk). We acknowledge helpful discussions and suggestions for improvement from Paul Sherwood, Joseph Thacker, and Thomas Durrant.

# References

---
nocite: '@*'
---
