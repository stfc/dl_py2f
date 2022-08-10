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

__author__ = 'You Lu <you.lu@ukri.stfc.org>'

from ctypes import RTLD_LOCAL, RTLD_GLOBAL

# YL 07/01/2021: same function as the one in `CHEMSH_ROOT/setup`
def importModule(modname, filepath, relative_dir='.', suppress_warning=True, submodules=None, **kwargs):
    '''Construct a module through its file location'''

    from importlib.util import spec_from_file_location, module_from_spec
    from warnings       import catch_warnings, resetwarnings, simplefilter
    from os.path        import abspath, dirname, join
    from sys            import path, stderr, stdout

    # so that we can do \`from . import *\` in the package (e.g., in tools/__init__.py)
    pkgdir = abspath(join(dirname(filepath), relative_dir))
    if pkgdir not in path:
        path.append(pkgdir)

    spec   = spec_from_file_location(modname, filepath, submodule_search_locations=submodules)
    module = module_from_spec(spec)

    # YL 26/02/2021: allowed additional attributes via keyword arguments
    #                it has to be before exec_module() which loads everything
    for k, v in kwargs.items():
        setattr(module, k, v)

    if suppress_warning:
        with catch_warnings(record=True) as w:
            simplefilter('ignore')
            spec.loader.exec_module(module)
        resetwarnings()
    else:
        spec.loader.exec_module(module)

    stderr.flush()
    stdout.flush()

    return module


def getSharedLibname(modname):
    '''Returns default shared library name (e.g., GAMESS_UK will be libgamess-uk.so) '''

    return 'lib' + modname.lower() + '.so'


def isLinked(modname, location):
    ''''''

    lib = getSharedLib(modname, location)

    # exemptions (always assumed linked in)
    if modname in [ 'parallel' ]:
        return True

    try:
        return bool(lib.islinked())
    except:
        return False


def getSharedLib(modname, location, mode=RTLD_GLOBAL, debug=0):
    '''Returns shared library (only linked-in by default)'''

    from ctypes import CDLL
    from os     import path

    libname = getSharedLibname(modname)

    # FIXME: improve
    try:
        soname = path.join(location, libname.replace('-', '_'))
        return CDLL(soname, mode=mode)
    except OSError as e:
        if 'undefined symbol' in str(e):
            raise
        if debug > 5:
            print(" >>> dl_py2f.utils.modutils.getSharedLib({}) failed to initialise CDLL:\n".format(modname), e)
        try:
            soname = path.join(location, libname.replace('_', '-'))
            return CDLL(soname, mode=mode)
        except:
            return


def getLinkedLib(modname, location, mode=RTLD_GLOBAL):
    '''Returns shared library (only linked-in by default)'''

    lib = getSharedLib(modname, location, mode=mode)

    if isLinked(modname, location):
        return lib
    else:
        return


