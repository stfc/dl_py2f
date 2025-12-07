cmake_minimum_required(VERSION 3.16)
cmake_policy(SET CMP0048 NEW)
project(DL_PY2F
        VERSION       1.1.1
        DESCRIPTION  "DL_PY2F: A library for Python-Fortran interoperability"
        LANGUAGES     CXX Fortran)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)
include_directories(${PROJECT_BINARY_DIR}/modules)
configure_file(libpath.py.in ${CMAKE_CURRENT_LIST_DIR}/libpath.py @ONLY)
add_library(dl_py2f SHARED dl_py2f.f90 dl_py2f.cpp)
if("${DL_PY2F_ARCH}" STREQUAL "cpe-gnu")
    set_target_properties(dl_py2f PROPERTIES COMPILE_FLAGS "-fcheck=no-bounds")
elseif("${DL_PY2F_ARCH}" STREQUAL "cray")
    if(DEBUG)
        set_target_properties(dl_py2f PROPERTIES COMPILE_FLAGS "-dynamic -O0 -g -v")
    else()
        set_target_properties(dl_py2f PROPERTIES COMPILE_FLAGS "-dynamic -O0")
    endif()
endif()
if(CMAKE_C_COMPILER_ID STREQUAL GNU)
    list(APPEND CMAKE_CXX_FLAGS -fpermissive)
endif()
set_target_properties(dl_py2f PROPERTIES PUBLIC_HEADER ${CMAKE_Fortran_MODULE_DIRECTORY}/dl_py2f.mod)
set_target_properties(dl_py2f PROPERTIES LINK_FLAGS "-Wl,--version-script=${CMAKE_CURRENT_LIST_DIR}/export.map")
target_include_directories(dl_py2f PRIVATE ${CMAKE_CURRENT_LIST_DIR})
set_property(DIRECTORY ${PROJECT_BINARY_DIR}
             APPEND
             PROPERTY ADDITIONAL_MAKE_CLEAN_FILES Makefile;CMakeCache.txt;cmake_install.cmake;__pycache__;${PROJECT_BINARY_DIR}/modules;${PROJECT_BINARY_DIR}/lib;CMakeFiles 
            )


# YL 11/08/2025: rules for installation (Debian only)

set(CMAKE_INSTALL_LIBDIR /lib/x86_64-linux-gnu)
set(CMAKE_INSTALL_INCLUDEDIR /usr/include/x86_64-linux-gnu)

install(TARGETS dl_py2f
        LIBRARY
            DESTINATION ${CMAKE_INSTALL_LIBDIR}
            COMPONENT Libraries
        PUBLIC_HEADER
            DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
            COMPONENT Headers)

install(FILES dl_f2py.py __init__.py
        DESTINATION /usr/lib/python3/dist-packages/dl_py2f)

install(FILES libdir_debian.py
        DESTINATION /usr/lib/python3/dist-packages/dl_py2f
        RENAME libpath.py)

install(DIRECTORY utils
        DESTINATION /usr/lib/python3/dist-packages/dl_py2f)
