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

Fortran is long established as one of the major programming languages for scientific software, but has only limited facilities for interoperating with other modern languages such as Python. `DL_PY2F` is an open-source library for the creation of modern interfaces and data structures in Python that can interoperate with existing scientific software written in Fortran and manipulate their data.

# Statement of need

`DL_PY2F` was created to facilitate the redevelopment of the computational chemistry environment `ChemShell` [@chemshell], which contains a number of Fortran modules and interfaces to external Fortran software. The redevelopment involved a new Python-based user interface, a modern Python software architecture, and [`NumPy`](https://numpy.org/)-based core data structures. A key criterion for the redeveloped `Py-ChemShell` package [@lu2018; @lu2023] was ensuring the direct accessibility of its core Python data structures in the Fortran modules and interfaces. `DL_PY2F` achieves this interoperability using the Python `ctypes` and `numpy.ctypeslib` libraries and relevant features in the Fortran 2003 standard, in particular `iso_c_binding`. In `Py-ChemShell`, class instances and `NumPy` array data such as molecular coordinates, energy gradients, and Hessian matrices, are mapped to Fortran pointers via memory addresses by `DL_PY2F`. A simple Fortran 2003 interface is all that is required to access the data. As a result, developers benefit from a seamless Python user interface experience with native data objects, while direct access to data is possible for numerically intensive computing tasks in the Fortran code. 

While the `DL_PY2F` library is vital for the `Py-ChemShell` program, it has the potential to be applied in other situations in which Fortran code could benefit from integration into a wider Python software environment. Therefore, we have released `DL_PY2F` as an independent, general-purpose library for Python&ndash;Fortran interoperability.

## Python-to-Fortran interoperability

`DL_PY2F` is intended for use with Python-based software packages where data are managed using Python/`NumPy` and need to be accessed for computations performed by Fortran code. At the ABI level, a pre-compiled shared object (dynamic library) containing a Fortran 2003 interface to the existing Fortran application is loaded using Python's `ctypes.CDLL`, as follows:

    import ctypes, dl_py2f
    libapp = ctypes.CDLL('/abc/def/libapp.so')
    ierror = libapp.interface_app(dl_py2f.py2f(appObj))

Here, `appObj` is an instance, typically created by the application's user at runtime, of the new package's Python class `App` which inherits from `ctypes.Structure`, for example:

    import ctypes, numpy
    from . import callback
    class App(ctypes.Structure):
        _kwargs = {
            'callback':callback.callback,
            'child'   :Child(),
            'coords'  :numpy.zeros(shape=(1000,3), dtype=numpy.float64),
            'npoints' :1000}

The instance's member attributes are declared in a Python dictionary `App._kwargs` (with default values) and these become visible to the Fortran application after `appObj` is read by the `dl_py2f.py2f` method provided at the API level. `DL_PY2F` supports a wide range of Python data types, with some illustrative examples given in the example above. Supported data types include 1- and 2-dimensional arrays (`numpy.ndarray`, `numpy.recarray`) and scalar values such as `int`, `float`, and `str` that are commonly needed in scientific computing. `dl_py2f.py2f` works recursively, so that `appObj` may contain unlimited levels of child instances (e.g., `appObj.child` in the above example). `DL_PY2F` provides utility tools to facilitate initialisation and enhancement of an instance. Please see the example application provided in the `DL_PY2F-example` [repository](https://github.com/stfc/dl_py2f-example) for further details. Callback functions to facilitate two-way data communication are also supported by `DL_PY2F`.

On the Fortran side, the `interface_app` function receives the passed-in `appPtr` – a pointer to the Python object – and bookkeeps it in a `dictType` instance `PyApp`, which is a linked list with support for child instances (e.g., `PyChild` in the code example):

    module AppModule
        use iso_c_binding
        use DL_PY2F, only: dictType, PyType, ptr2dict
        abstract interface
            integer(c_long) function callback() bind(c)
                use iso_c_binding
            endfunction callback
        endinterface
        type(dictType)     , pointer, public :: PyApp, PyChild
        procedure(callback), pointer, public :: PyCallback
        contains
        function interface_app(appPtr) bind(c) result(ireturn)
            implicit none
            type(PyType)           , intent(in) :: appPtr
            type(c_funptr)                      :: pyfuncPtr
            type(PyType)  , pointer             :: childPtr
            real(kind=8)  , pointer             :: coords(:,:)
            integer(c_long)                     :: ireturn
            allocate(PyApp, source=ptr2dict(appPtr))
            call PyApp%get('child', childPtr)
            allocate(PyChild, source=ptr2dict(childPtr))
            call PyApp%get('callback', pyfuncPtr)
            call c_f_procpointer(pyfuncPtr, PyCallback)
            call PyApp%get('coords', coords) ! writable handle to the NumPy array
            call my_app(coords)              ! run the application
            call PyApp%finalise()
            call PyChild%finalise()
            deallocate(PyApp, PyChild)
            nullify(coords)                  ! do not deallocate: Python owns the data
            ireturn = 0
        endfunction interface_app
    endmodule AppModule

A great advantage of `DL_PY2F` for application developers is that the attributes of the Python instance are conveniently retrieved by querying their names in a dictionary-like way. The type-bound accessor `get` moulds a handle of the target Python object with read and write (if mutable in Python) access. For arrays, no copies are made because access is via memory addresses; values are thus changed in place and reflected on the Python side. We also provide a safe mode in which the developer must make a copy first and explicitly use a `set` function to change the Python values:

    integer               :: npoints
    real(kind=8), pointer :: buffer(:,:)
    call PyApp%get('npoints', npoints)
    allocate(buffer(3,npoints))
    call PyApp%get('coords', buffer, readonly=.true.) ! a copy without write access
    call do_something(buffer)
    call PyApp%set('coords', buffer)
    deallocate(buffer)
 
Callback functions can also be invoked, as follows:

    subroutine get_something()
        use AppModule, only: PyCallback
        call PyCallback()
    endsubroutine

`DL_PY2F`'s Python-to-Fortran interoperability has been comprehensively tested using the GNU, Intel, Flang/Clang++, and NVIDIA HPC[^a] compilers.

## Fortran-to-Python interoperability

While the Python-to-Fortran interoperability described above is recommended for new or redeveloped Python projects, other applications may benefit from interoperability using Python wrappers around their existing Fortran codes. For this `DL_PY2F` offers an ABI-style second method based on analysis of the symbols in a pre-compiled shared object and parsing of the Fortran module files, which are assumed to be kept at compiletime. In this method, Python's dot syntax may be used to access Fortran entities, for example, in a Python function invoked by the application at runtime:

    import dl_py2f
    libapp = dl_py2f.DL_DL('/abc/def/libapp.so')
    libapp.moddir = '/abc/def/modules'
    libapp.modules.my_mod.b.coords[1,2]  = 1.2345
    libapp.modules.my_mod.b.a[2,:].ibuff = 2025

given that the original application's Fortran code contains:

    module my_mod
        type type_a
            integer :: ibuff
        endtype type_a
        type type_b
            type(type_a)                   :: a(5,6)
            real(kind=8)     , allocatable :: coords(:,:)
        endtype type_b
        type(type_b) :: b
    endmodule my_mod

All Python attributes, including arrays of numbers and derived-type instances, are automatically exposed as Python objects as soon as an instance of class `dl_py2f.DL_DL` is created and a path to the module files is specified. The seamless access to Fortran data empowered by `DL_PY2F` will be particularly useful for machine-learning enhanced scientific computing, and is currently being trialled with the established computational chemistry codes `CASTEP` [@clark2005], `DL-FIND` [@kaestner2009], and `DL_POLY` [@devereux2025]. Note that this second method for Fortran-to-Python interoperability in `DL_PY2F` is still undergoing testing and validation, and currently supports the GNU compiler gfortran, the Intel compiler ifx, the LLVM compiler Flang, and the NVIDIA HPC compiler nvfortran.

## Comparison with other tools

A number of tools have been developed to facilitate coupling of Python and Fortran code. A major category of these are interface generators, such as [`F2PY`](https://numpy.org/doc/stable/f2py/) [@peterson2009] and [`f90wrap`](https://github.com/jameskermode/f90wrap) [@kermode2020]. These tools process the Fortran source files and write out a wrapper layer, namely Python extension modules, plus, in the case of `f90wrap`, additional Fortran wrapper files, through which Python calls Fortran procedures and accesses Fortran data. The original sources are left untouched, although the generated wrapper layer must be compiled and maintained alongside the application. While `F2PY` on its own does not handle derived types, `f90wrap` extends it with support for modern Fortran features, including nested and allocatable derived types and zero-copy `NumPy` access to Fortran arrays, and since v0.3.0 it can alternatively generate a direct C layer without involving `F2PY`. By comparison, `DL_PY2F` generates no code at any stage and drives the whole Fortran application through a single entry point. Its Python-to-Fortran direction addresses a workflow the generator tools are not designed for, in which the primary data structures are created and owned in Python/`NumPy` and the Fortran side queries them by name at runtime. The application developer hand-writes this entry point as one small Fortran 2003 interface (such as `interface_app` above); no wrapper layer needs to be generated, built, or maintained. `DL_PY2F` also provides some capabilities beyond the generator tools, for example multi-dimensional arrays of derived types (see `b.a[2,:].ibuff` above), and callbacks held as ordinary object attributes, so that Fortran retrieves a Python callable by name at runtime just like any other datum. [`gfort2py`](https://github.com/rjfarmer/gfort2py) [@gfort2py] is an ABI-level runtime tool that works similarly to the Fortran-to-Python layer of `DL_PY2F`: it likewise operates directly on an existing shared library and its module files without amendment to the source code, but is restricted to the gfortran compiler, whereas the Fortran-to-Python layer of `DL_PY2F` supports four compiler families (GNU, Intel, LLVM, and NVIDIA HPC). An alternative route to realising Python-to-Fortran data binding would be to manually implement the mechanism based on the Python `ctypes` and `NumPy`'s `ctypeslib` modules. Such a challenging task might be indirectly assisted by tools such as [`CFFI`](https://github.com/cffi/cffi) which invokes a Fortran-bound C code/library at the ABI/API level or [`SWIG+Fortran`](https://github.com/swig-fortran/swig) which generates Fortran 2003 wrappers to existing C/C++ libraries that are then used by Python. `DL_PY2F` provides a more convenient solution by automating this complex mechanism.

# Obtaining `DL_PY2F`
`DL_PY2F` is an open-source library released under [GNU Lesser General Public License v3.0](https://www.gnu.org/licenses/lgpl-3.0.en.html). It is available for download from the [repository](https://github.com/stfc/dl_py2f), which also includes a comprehensive [reference manual](https://github.com/stfc/dl_py2f/blob/main/REFERENCE_MANUAL.md). There is also a comprehensive [example](https://github.com/stfc/dl_py2f-example) demonstrating how to embed `DL_PY2F` in an application project. `DL_PY2F` has been published and deployed in a [launchpad.net PPA](https://launchpad.net/~dl-py2f/+archive/ubuntu/ppa) and can be installed as a system package for Debian-based systems and is likewise available on [PyPI](https://pypi.org/project/dl-py2f). Please note that the PPA distribution works only with applications compiled with gfortran due to the pre-compiled Fortran module file we shipped.

# Acknowledgements

The `DL_PY2F` library was created during the redevelopment of [ChemShell](https://chemshell.org) as a Python-based package, which was funded by EPSRC under the grant [EP/K038419/1](https://gtr.ukri.org/projects?ref=EP/K038419/1). Ongoing support for the development of `DL_PY2F` as part of ChemShell is provided under EPSRC grants [EP/R001847/1](https://gtr.ukri.org/projects?ref=EP%2FR001847%2F1) and [EP/W014378/1](https://gtr.ukri.org/projects?ref=EP%2FW014378%2F1), and the [Computational Science Centre for Research Communities (CoSeC)](https://www.cosec.ac.uk), via the support provided to the [Materials Chemistry Consortium](https://mcc.hec.ac.uk). We acknowledge helpful discussions and suggestions for improvement from Paul Sherwood, Joseph Thacker, Thomas Durrant, and Maitrayee Singh.

# References

---
nocite: '@*'
---

[^a]: Currently, with NVIDIA HPC compilers, arrays must be retrieved and altered using the `get` and `set` methods, respectively, in the `readonly=.true.` mode due to a compiler bug in nvfortran.



