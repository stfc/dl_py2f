#  Copyright (C) 2025 The authors of DL_PY2F
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
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with DL_PY2F. If not, see
#  <https://www.gnu.org/licenses/>.

# you.lu@stfc.ac.uk
__author__ = 'You Lu <you.lu@ukri.stfc.org>'
def __getEntry(arg, default):
    from . import iterutils
    if not arg:
        try:
            return iterutils.getFlattened((default,))
        except:
            return arg
    else:
        return iterutils.getFlattened((arg,))
def init(obj=None, inherit=(), init=(), priorities=(), inheritfrom=None):
    def __init(proc):
        from functools import wraps
        from .         import iterutils
        def __getObj():
            return obj
        @wraps(proc)
        def __wrapper(*args, **kwargs):
            obj = __getObj()
            if not obj:
                obj = args[0]
            def __getInheritfrom():
                if inheritfrom:
                    return iterutils.getFlattened((inheritfrom,))
                else:
                    return inheritfrom
            inheritAttrs(obj,
                         __getEntry(inherit, obj._inherit),
                         inheritfrom=__getInheritfrom())
            initAttrs(obj,
                      __getEntry(init      , obj._init),
                      __getEntry(priorities, obj._priorities))
            result = proc(*args, **kwargs)
            return result
        return __wrapper
    return __init
def inheritAttrs(obj, inherit, inheritfrom=None):
    from . import iterutils
    inheritSeqAttrs(obj,
                    inherit,
                    inheritfrom=inheritfrom)
    inheritDictAttrs(obj,
                     obj._init,
                     inheritfrom=inheritfrom)
def initAttrs(obj, init, priorities):
    initSeqAttrs(obj, init)
    initDictAttrs(obj, init, priorities=obj._priorities )
def inheritSeqAttrs(obj, attrs, inheritfrom=None):
    from . import iterutils, miscutils
    if not inheritfrom:
        inheritfrom = miscutils.getBaseClasses(obj.__class__)
    attrs = iterutils.getFlattened(['_inherit', '_init', list(attrs)])
    for baseCls in inheritfrom:
        it = iter(attrs)
        for i, attr in enumerate(it):
            try: 
                setattr(obj, attr, iterutils.getFlattened((getattr(baseCls,attr),)+(getattr(obj,attr),)))
            except:
                pass
def inheritDictAttrs(obj, attrs, inheritfrom=None):
    from .    import miscutils
    from copy import deepcopy
    if not inheritfrom:
        inheritfrom = miscutils.getBaseClasses(obj.__class__)
    for baseCls in inheritfrom:
        for attr in attrs:
            try:
                items = getattr(baseCls, attr).items()
                for key, val in items:
                    getattr(obj, attr).setdefault(key, deepcopy(val))
            except:
                continue
def initDictAttrs(obj, attrs, priorities=None):
    from copy import deepcopy
    items = getMergedItems(obj, attrs, order=priorities)
    for key, val in items:
        if key in getattr(obj, '_properties', []):
            addProperty(obj, key)
            continue
        try:
            setattr(obj, key, deepcopy(val))
            obj.__dict__.update()
        except (AttributeError, TypeError):
            setattr(obj, '_'+key, deepcopy(val))
        except:
            raise
def initSeqAttrs(obj, seqs):
    from . import iterutils
    for seq in seqs:
        val = getattr(obj, seq)
        if type(val) is dict:
            continue
        try:
            setattr(obj, seq, iterutils.getFlattened((val,)))
        except:
            setattr(obj, '_'+seq, iterutils.getFlattened((val,)))
def getMergedItems(obj, dicts, order=None):
    from . import dictutils
    items = []
    for attr in dicts:
        try:
            items += list(getattr(obj, attr).items())
        except:
            continue
    keys = dictutils.getSortedKeys(dict(items), order=order)
    return [ (key, dict(items)[key]) for key in keys ]
def checkType(obj, cls, info="", chksubcls=False):
    from numpy import ndarray, random, float64
    from .     import nputils
    if isinstance(obj, cls):
        if isinstance(obj, str):
            return obj
        if cls == list:
            if all(isinstance(i, type(obj[0])) for i in obj):
                return nputils.ndarray(obj)
            else:
                return obj
        if isinstance(obj, tuple):
            return ndarray(shape=obj, buffer=random.rand(obj[0]*obj[1]), dtype=float64)
        else:
            return obj
    elif cls is type(None):
        return obj
    elif isinstance(obj, cls.__bases__):
        return obj
    else:
        print(f"\n >>> DL_PY2F ERROR: Type {cls} expected for keyword {info} but {type(obj)} was given. Exiting...\n", flush=True)
        raise TypeError
assignByType = checkType
def getBaseType(obj):
    if hasattr(obj.__class__, '__base__'):
         try:
             return getBaseType(obj.__class__.__base__())
         except TypeError:
             return obj.__class__.__base__
    else:
         return obj.__class__
def getType(obj, list_modules=[], ndarrayislist=True):
    from numpy  import ndarray, float64, float32, int64, int32
    from types  import ModuleType, FunctionType
    if isinstance(obj, ndarray) and ndarrayislist:
        return list
    elif isinstance(obj, ndarray) and not ndarrayislist:
        return ndarray
    elif isinstance(obj, list):
        return list
    elif isinstance(obj, str):
        return str
    elif isinstance(obj, bytes):
        return bytes
    elif isinstance(obj, float64) or isinstance(obj, float64):
        return float
    elif isinstance(obj, int64) or isinstance(obj, int32):
        return int
    elif isinstance(obj, ModuleType):
        return ModuleType
    elif isinstance(obj, FunctionType):
        return FunctionType
    elif type(obj).__module__ in list_modules:
        return object
    else:
        return type(obj)
def getDeepCopy(obj):
    from copy  import deepcopy
    from numpy import copy as numpy_copy
    from numpy import ndarray, recarray
    from .     import nputils
    if isinstance(obj, nputils.RecArray):
        copied = recarray.copy(obj)
    elif isinstance(obj, ndarray):
        copied = numpy_copy(obj)
    else:
        copied = deepcopy(obj)
        if hasattr(obj, '__dict__'):
            for (name, attr) in [ (name, getattr(obj, name)) for name in obj.__dict__ if name not in {'_b_weakref_'} ]:
                setattr(copied, name, getDeepCopy(attr))
    return copied
def printTraceback(path):
    import os
    import sys
    import traceback
    print('\n >>> Failed to load module', path, flush=True)
    var = traceback.format_exc()
    print(var, flush=True)
    sys.exit(999)
def getModuleList(obj):
    from pkgutil import walk_packages, extend_path
    res = []
    def _recur(_path, _prefix): 
        for _loader, _name, _ispkg in walk_packages(_path, _prefix, onerror=printTraceback):
            pass
        try:
            for _loader, _name, _ispkg in walk_packages(_path, _prefix, onerror=printTraceback):
                pass
        except:
            print(" >>> ERROR: There is a bug found in module %s"%_name, flush=True)
            print(" >>> Use command `python3` to run your script for detailed reasons (requiring environment variables such as $PYTHONPATH)", flush=True)
            return
        for _loader, _name, _ispkg in walk_packages(_path, _prefix, onerror=printTraceback):
            _newpath = extend_path(_path, _name)
            _newlist = []
            try:
                _newlist.append(_newpath[1])
                _recur(_newlist, _name+'.')
            except:
                pass
            res.append(_name)
    _recur(obj.__path__, obj.__name__+'.')
    return set(res)
def getAttrType(obj, attr, list_modules=[]):
    try:
        return getType(getattr(obj, attr, list_modules=list_modules))
    except:
        return type(None)
def getAttr(obj, attrname, level=0):
    from . import strutils
    if not attrname:
        return obj
    try:
        return getattr(obj, attrname)
    except TypeError:
        return [ getAttr(obj, attr) for attr in attrname ]
    except AttributeError:
        return getAttr(getAttr(obj, 
                               strutils.split(attrname, '.')[0]),
                       strutils.split(attrname, '.', maxsplit=1)[1])
def type2dtype(obj, descr=False):
    from numpy import dtype, ndarray
    if descr:
        attr = 'descr'
    else:
        attr = 'str'
    selectcases = {
                    str    : lambda x: getattr(dtype(('S', len(obj)))    , attr),
                    int    : lambda x: getattr(dtype('int64')  , attr),
                    float  : lambda x: getattr(dtype('float64'), attr),
                    ndarray: lambda x: getattr(x.dtype, attr),
                    list   : lambda x: _getComplexDtype(x),
                    tuple  : lambda x: _getComplexDtype(x)
                  }
    def _getComplexDtype(array_seq):
        return [ ('', type2dtype(arr, descr=descr), (arr.shape[1],)) if arr.ndim>1 else
                 ('', type2dtype(arr, descr=descr),) for arr in array_seq ]
    return selectcases[type(obj)](obj)
def obj2dict(obj, list_modules=[]):
    from numpy import ndarray
    from .     import miscutils, nputils
    dictbuff = {}
    selectcases = {
                    bytes     : bytes.decode,
                    list      : miscutils.doNothing,
                    ndarray   : nputils.tolist,
                    str       : str.strip,
                    type(None): lambda x: None,
                    int       : int,
                    float     : float,
                    object    : obj2dict
                  }
    for key, val in obj._kwargs.items():
        dictbuff.update({key:selectcases[getType(getattr(obj,key,list_modules=list_modules), ndarrayislist=False)](getattr(obj, key))})
    return dictbuff
def setAttr(obj, attr, val, debug=0):
    from .         import nputils
    from .strutils import getRightmost
    if not attr:
        try:
            nputils.setField(obj, val, debug=debug)
            return
        except:
            return
    try:
        setattr(obj, attr, val)
    except TypeError:
        for key in attr:
            attrname = getRightmost(key)
            destin = getAttr(obj, getRightmost(key, pos=1))
            if hasattr(getattr(destin, attrname), 'size'):
                destin.resize(val.size)
                setattr(destin, attrname, val[attrname])
            else:
                raise AttributeError
def addProperty(obj, attr):
    mangled = '_'+attr
    setattr(obj, attr, property(lambda self: getattr(self, mangled),
                                lambda self, val: setattr(self, mangled, val)))
