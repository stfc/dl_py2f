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
set(CMAKE_Fortran_MODULE_DIRECTORY ${CHEMSH_BUILD_DIR}/modules/py2f)

# source files
list(APPEND PY2F_FSOURCES py2f.f90
#                          py2f_dictionary.f90
#                          py2f_datatypes.f90
#                          py2f_objects.f90
)

include_directories(${CHEMSH_BUILD_DIR}/modules
                    ${CHEMSH_BUILD_DIR}/modules/py2f
                   )

add_library(py2f SHARED ${PY2F_FSOURCES})

#target_link_libraries(py2f chemsh)

if(DEBUG)
else()
    if(${CHEMSH_ARCH} STREQUAL "intel")
        set_target_properties(py2f PROPERTIES COMPILE_FLAGS "-diag-disable=8753")
    endif()
endif()


# rules of `make clean`
set(CURRENT_DIR ${CMAKE_CURRENT_LIST_DIR})
include(${CHEMSH_DIR}/chemsh/utils/cmake/clean.cmake)
