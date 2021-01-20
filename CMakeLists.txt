#  Copyright (C) 2017 The authors of Py-ChemShell
#
#  This file is part of Py-ChemShell.
#
#  Py-ChemShell is free software: you can redistribute it and/or modify
#  it under the terms of the GNU Lesser General Public License as
#  published by the Free Software Foundation, either version 3 of the
#  License, or (at your option) any later version.
#
#  Py-ChemShell is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with Py-ChemShell.  If not, see
#  <http://www.gnu.org/licenses/>.


# paths
if("${DL_PY2F_BUILD_DIR}" STREQUAL "")
    set(DL_PY2F_BUILD_DIR ${CMAKE_CURRENT_LIST_DIR})
endif()
set(CMAKE_Fortran_MODULE_DIRECTORY ${DL_PY2F_BUILD_DIR}/modules/dl_py2f)
include_directories(${DL_PY2F_BUILD_DIR}/modules/dl_py2f)

# compile
add_library(dl_py2f SHARED dl_py2f.f90)

# YL 25/11/2020: added export.map to meet special name mangling of Cray compilers
if(${CHEMSH_ARCH} STREQUAL "cray")
    if(DEBUG)
        set_target_properties(dl_py2f PROPERTIES COMPILE_FLAGS "-dynamic -O0 -g -v")
    else()
        set_target_properties(dl_py2f PROPERTIES COMPILE_FLAGS "-dynamic -O0")
    endif()
endif()
set_target_properties(dl_py2f PROPERTIES LINK_FLAGS "-Wl,--version-script=${CMAKE_CURRENT_LIST_DIR}/export.map")

# rules for `make clean`
set_property(DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
             APPEND
             PROPERTY ADDITIONAL_MAKE_CLEAN_FILES Makefile CMakeCache.txt cmake_install.cmake __pycache__ ${DL_PY2F_BUILD_DIR}/modules/dl_py2f
            )


