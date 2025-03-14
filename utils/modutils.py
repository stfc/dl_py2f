__author__ = 'You Lu <you.lu@ukri.stfc.org>'
from ctypes import CDLL, RTLD_LOCAL, RTLD_GLOBAL
from os     import path
def importModule(modname, filepath, relative_dir='.', suppress_warning=True, submodules=None, **kwargs):
    from importlib.util import spec_from_file_location, module_from_spec
    from warnings       import catch_warnings, resetwarnings, simplefilter
    from os.path        import abspath, dirname, join
    from sys            import path, stderr, stdout
    pkgdir = abspath(join(dirname(filepath), relative_dir))
    if pkgdir not in path:
        path.append(pkgdir)
    spec   = spec_from_file_location(modname, filepath, submodule_search_locations=submodules)
    module = module_from_spec(spec)
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
    return 'lib' + modname.lower() + '.so'
def isLinked(modname, location):
    lib = getSharedLib(modname, location)
    if modname in [ 'parallel' ]:
        return True
    try:
        return bool(lib.islinked())
    except:
        return False
def getSharedLib(modname, location, mode=RTLD_GLOBAL, debug=0):
    libname = getSharedLibname(modname)
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
def getLinkedLib(modname, location, mode=RTLD_GLOBAL, is_linked=False):
    lib = getSharedLib(modname, location, mode=mode)
    if isLinked(modname, location) or is_linked:
        return lib
    else:
        return
