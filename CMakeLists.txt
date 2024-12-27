#  Copyright (C) 2017 The authors of DL_PY2F
#
#  This file is part of DL_PY2F.
#
#  DL_PY2F is free software: you can redistribute it and/or modify
#  it under the terms of the GNU Lesser General Public License as
#  published by the Free Software Foundation, either version 3 of the
#  License, or (at your option) any later version.
#
#  DL_PY2F is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with DL_PY2F.  If not, see
#  <http://www.gnu.org/licenses/>.

# you.lu@stfc.ac.uk
project(DL_PY2F
        VERSION       24.0
        DESCRIPTION  "A generic library that seamlessly glues Python and Fortran"
#        HOMEPAGE_URL "www.chemshell.org"
        LANGUAGES     CXX Fortran)

# paths
if("${DL_PY2F_BUILD_DIR}" STREQUAL "")
    set(DL_PY2F_BUILD_DIR ${CMAKE_CURRENT_LIST_DIR})
endif()
set(CMAKE_Fortran_MODULE_DIRECTORY ${DL_PY2F_BUILD_DIR}/modules/dl_py2f)
include_directories(${DL_PY2F_BUILD_DIR}/modules/dl_py2f)

# compile
add_library(dl_py2f SHARED dl_py2f.f90 dl_py2f.cpp)

# YL 27/03/2024: on some architectures we need this ${DL_PY2F_ARCH} which should be defined by the project's developer
if("${DL_PY2F_ARCH}" STREQUAL "cpe-gnu")
    # YL 10/04/2021: we must add -fcheck=no-bounds to prevent gfortran-10 checking the array bounds
    set_target_properties(dl_py2f PROPERTIES COMPILE_FLAGS "-fcheck=no-bounds")
elseif("${DL_PY2F_ARCH}" STREQUAL "cray")
    if(DEBUG)
        set_target_properties(dl_py2f PROPERTIES COMPILE_FLAGS "-dynamic -O0 -g -v")
    else()
        set_target_properties(dl_py2f PROPERTIES COMPILE_FLAGS "-dynamic -O0")
    endif()
endif()

# YL 18/03/2024: for C++
if(CMAKE_C_COMPILER_ID STREQUAL GNU)
#    add_compile_options($<$<COMPILE_LANG_AND_ID:CXX,GNU>:-fpermissive>)
    list(APPEND CMAKE_CXX_FLAGS -fpermissive)
endif()

# YL 25/11/2020: added export.map to meet special name mangling of Cray compilers
set_target_properties(dl_py2f PROPERTIES LINK_FLAGS "-Wl,--version-script=${CMAKE_CURRENT_LIST_DIR}/export.map")

target_include_directories(dl_py2f PRIVATE ${CMAKE_CURRENT_LIST_DIR})

# rules for `make clean`
set_property(DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
             APPEND
             PROPERTY ADDITIONAL_MAKE_CLEAN_FILES Makefile CMakeCache.txt cmake_install.cmake __pycache__ ${DL_PY2F_BUILD_DIR}/modules/dl_py2f
            )


