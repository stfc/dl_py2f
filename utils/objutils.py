# -*- coding: utf-8 -*-

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

# to allow developers to override
# allowing either a string or a sequence of strings
def __getEntry(arg, default):
    '''Get the entry'''

    from . import iterutils

    if not arg:
        try:
            return iterutils.getFlattened((default,))
        except:
            return arg
    else:
        return iterutils.getFlattened((arg,))


# YL 15/08/2019: these tools should be provided with DL_PY2F
# init():
#     A. inheritAttrs
#         a. inheritSeqAttrs
#         b. inheritDictAttrs
#     B. initAttrs
#         a. initDictAttrs
#         b. initSeqAttrs
def init(obj=None, inherit=(), init=(), priorities=(), inheritfrom=None):
    '''Add attributes to a ChemShell <obj> or <func> and initialise with values provided in, for example, obj._kwargs'''

    def __init(proc):
        '''An inner wrapper to support arguments of @objutils.init()'''

        from functools import wraps
        from .         import iterutils

        # the wrapper function cannot access `obj` or other arguments of init()
        def __getObj():
            return obj
    
        @wraps(proc)
        def __wrapper(*args, **kwargs):
            '''A wrapper of the procedure to process'''

            obj = __getObj()
            if not obj:
                obj = args[0]
    
            # to allow developers to override
            # allowing either an obj or a sequence of objs
            def __getInheritfrom():
                if inheritfrom:
                    return iterutils.getFlattened((inheritfrom,))
                else:
                    return inheritfrom

            # update attributes according to the object's parent classe(s): either sequence (_init, _priorities, etc.) or <dict> (_kwargs, etc.)
            inheritAttrs(obj,
                         __getEntry(inherit, obj._inherit),
                         inheritfrom=__getInheritfrom())
        
            # initialise attributes
            initAttrs(obj,
                      __getEntry(init      , obj._init),
                      __getEntry(priorities, obj._priorities))

            # run the procedure
            result = proc(*args, **kwargs)

            return result
    
        return __wrapper

    return __init


def inheritAttrs(obj, inherit, inheritfrom=None):
    '''Complement the dicts ('_kwargs' and '_internals') with that of the parent class'''

    from . import iterutils

    # update sequences, such as '_toinits' and '_priorities'
    inheritSeqAttrs(obj,
                    inherit,
                    inheritfrom=inheritfrom)

    # update dict items in, for example, '_toinits' (i.e., '_internals', '_kwargs', etc.)
    inheritDictAttrs(obj,
                     obj._init,
                     inheritfrom=inheritfrom)


def initAttrs(obj, init, priorities):
    ''''''

    # this is the step to initialise obj according to sequence defaults (e.g., '_fields')
    initSeqAttrs(obj, init)

    # this is the step to initialise obj according to dict defaults (e.g., '_kwargs')
    initDictAttrs(obj, init, priorities=obj._priorities )


# it is difficult to implement this as a class-bound method
def inheritSeqAttrs(obj, attrs, inheritfrom=None):
    '''Complement the attributes according to those of the parent class(es)'''

    from . import iterutils, miscutils

    if not inheritfrom:
        inheritfrom = miscutils.getBaseClasses(obj.__class__)

    # always update the list of `_inherit` and `_init` first
    attrs = iterutils.getFlattened(['_inherit', '_init', list(attrs)])

    for baseCls in inheritfrom:

        # this is a dynamic loop: attr will be updated according to `_inherit` which is updated in the first place
        it = iter(attrs)
        for i, attr in enumerate(it):
            try: 
                setattr(obj, attr, iterutils.getFlattened((getattr(baseCls,attr),)+(getattr(obj,attr),)))
                # when attr is '_inherit' update attrs using obj._inherit
                # for example, `_inherit` of <class Fragment> only contains '_fields', but it should first inherit '_kwargs' and so from the parent <class Objects>
#                attrs = obj._inherit
            except:
                pass


# it is difficult to implement this as a class-bound method
# attrs: a <dict> or a list of <dict> objs
def inheritDictAttrs(obj, attrs, inheritfrom=None):
    '''Complement the attributes according to those of the parent class(es) by a given name of dict'''

    from .    import miscutils
    from copy import deepcopy

    if not inheritfrom:
        inheritfrom = miscutils.getBaseClasses(obj.__class__)

    # loop over base classes
    for baseCls in inheritfrom:
        for attr in attrs:
            # probe if dict-type
            try:
                items = getattr(baseCls, attr).items()
                for key, val in items:
                    # do NOT use utils.getDeepCopy(), because when val is numpy.ndarray, its 'flags.writeable' is set to False (haven't found a way to change this flag)
                    getattr(obj, attr).setdefault(key, deepcopy(val))
            except:
                continue


def initDictAttrs(obj, attrs, priorities=None):
    ''''''

    from copy import deepcopy

    # dict type will be checked at this step
    items = getMergedItems(obj, attrs, order=priorities)

    # loop over items
    for key, val in items:

        if key in getattr(obj, '_properties', []):
            addProperty(obj, key)
            continue

        # do NOT enquire with hasattr() since attributes with @property will be executed
        try:
            # using deepcopy can prevent NumPy arrays sharing the same data (they may still share the address)
            setattr(obj, key, deepcopy(val))
            obj.__dict__.update()
        # assign read-only attributes defined by @property to underscored names.
        # NOTE: define an @attr.setter in order to have an underscored attribute;
        #       if you want it to be read-only after initialised, do not define any @attr.setter
        except (AttributeError, TypeError):
            # using deepcopy can prevent NumPy arrays sharing the same data (they may still share the address)
            setattr(obj, '_'+key, deepcopy(val))
        except:
            raise


def initSeqAttrs(obj, seqs):
    ''''''
    from . import iterutils

    # loop over sequences ('_fields', '_internals', etc)
    for seq in seqs:
        val = getattr(obj, seq)

        # skip dict
        if type(val) is dict:
            continue
        
        # overwrite class-level definition with @property decorator and force to convert to a flattened sequence (tuple)
        try:
            setattr(obj, seq, iterutils.getFlattened((val,)))
        except:
            setattr(obj, '_'+seq, iterutils.getFlattened((val,)))


def getMergedItems(obj, dicts, order=None):
    '''Return a merged dict from the given list of dict names'''

    from . import dictutils

    items = []
    # attr is a dict containing initialising data (e.g., '_kwargs')
    for attr in dicts:
        # probe if dict-type
        try:
            items += list(getattr(obj, attr).items())
        except:
            continue

    keys = dictutils.getSortedKeys(dict(items), order=order)

    return [ (key, dict(items)[key]) for key in keys ]


def checkType(obj, cls, info="", chksubcls=False):
    '''Assign to given value/data when matching the required type'''

    from numpy import ndarray, random, float64
    from .     import nputils

    if isinstance(obj, cls):
        # <str>
        if isinstance(obj, str):
            # TWK: should not automatically lower case string values here
            # TWK: inputs may be case sensitive
            # return obj.lower()
            return obj

        # <list>: return an array containing given data, shape kept
        if cls == list:
            if all(isinstance(i, type(obj[0])) for i in obj):
                return nputils.ndarray(obj)
            # if elements are heterogeneous types, return the original list (otherwise numpy will cast the types!)
            else:
                return obj

        # <tuple>: return an emtpy array of given shape
        # use random.rand() to prevent automatic memory sharing
        if isinstance(obj, tuple):
            return ndarray(shape=obj, buffer=random.rand(obj[0]*obj[1]), dtype=float64)

        # <int>, <object>, etc.
        else:
            return obj

    # no restraint when default is None
    elif cls is type(None):
        return obj

    # if given value shares class parent with the required type
    elif isinstance(obj, cls.__bases__):
        return obj

    else:
        print(f"\n ChemShell >>> Type {cls} expected for keyword {info} but {type(obj)} was given. Exiting...\n")
        raise ChemShTypeError

assignByType = checkType


def getBaseType(obj):
    '''Recursively get an object's root base type'''

    if hasattr(obj.__class__, '__base__'):
         try:
             return getBaseType(obj.__class__.__base__())
         except TypeError:
             return obj.__class__.__base__

    else:
         return obj.__class__


def getType(obj, ndarrayislist=True):
    '''Return type'''

    from numpy  import ndarray, float64, float32, int64, int32
    from types  import ModuleType, FunctionType
    from chemsh import CHEMSH_MODS

    # numpy.ndarray treated as list
    if isinstance(obj, ndarray) and ndarrayislist:
        return list
    
    # numpy.ndarray not treated as list
    elif isinstance(obj, ndarray) and not ndarrayislist:
        return ndarray
    
    # list
    elif isinstance(obj, list):
        return list

    # str
    elif isinstance(obj, str):
        return str

    # bytes
    elif isinstance(obj, bytes):
        return bytes

    # float
    elif isinstance(obj, float64) or isinstance(obj, float64):
        return float

    # int
    elif isinstance(obj, int64) or isinstance(obj, int32):
        return int

    # module
    elif isinstance(obj, ModuleType):
        return ModuleType

    # function
    elif isinstance(obj, FunctionType):
        return FunctionType

    # ChemShell <objects>
    elif type(obj).__module__ in CHEMSH_MODS:
        return object

# YL: removed since not matched in any case
#   elif hasattr(type(obj), "__name__") and type(obj).__name__[:10] == "ndpointer_":
#       return list

    # anything else
    else:
        return type(obj)


# NOTE: if object is instantiated by __new__() rather than __init__(), some attributes may not be in __dict__
def getDeepCopy(obj):
    '''Return a deep copy of object. numpy.ndarray attributes are copied rather than referenced.'''
    
    from copy  import deepcopy
    from numpy import copy as numpy_copy
    from numpy import ndarray, recarray
    from .     import nputils

    # firstly check RecArray as it is a child class of <ndarray>
    if isinstance(obj, nputils.RecArray):
        copied = recarray.copy(obj)
    elif isinstance(obj, ndarray):
        copied = numpy_copy(obj)
        # IMPORTANT: protect the array! otherwise it may be stained by unknown reasons! (miserable bug of numpy!!!)
        # TODO: protect all arrays!
# YL 22/03/2018: commented out to check if it still holds
#        copied.flags.writeable = False
    else:
        copied = deepcopy(obj)
        if hasattr(obj, '__dict__'):
            # shall not copy ref of owner
            for (name, attr) in [ (name, getattr(obj, name)) for name in obj.__dict__ if name not in {'_b_weakref_'} ]:
                setattr(copied, name, getDeepCopy(attr))

    return copied


def onerror(path):
    ''' Catch module load errors from walk_packages '''
    import sys
    import traceback
    print('Failed to load module', path, flush=True)
    var = traceback.format_exc()
    print(var, flush=True)
    import os
    os.exit(999)

def getModuleList(obj):
    '''Get a list of all modules under the given object'''

    from pkgutil import walk_packages, extend_path

    res = []

    # recursive method
    def _recur(_path, _prefix): 

        for _loader, _name, _ispkg in walk_packages(_path, _prefix, onerror=onerror):
            pass
        # this is a bug checker to warn developers as no information is printed when chemsh.x detects Python errors
        try:
            for _loader, _name, _ispkg in walk_packages(_path, _prefix, onerror=onerror):
                pass
        except:
            print(" >>> ERROR: there is a bug found in module %s"%_name, flush=True)
            print(" >>> Use command `python3` to run your script for detailed reasons (requiring environment variables PYTHONPATH and CHEMSH_ARCH)", flush=True)
            # TODO: skip the faulty one and proceed with the rest healthy modules!
            return

        # walk packages
        for _loader, _name, _ispkg in walk_packages(_path, _prefix, onerror=onerror):

            # dig deeper in package
            # have to include pkg now as some modules were moved into __init__.py
            _newpath = extend_path(_path, _name)
            _newlist = []

            try:
                # extend_path() appends to a list if finds path with subdirectory
                _newlist.append(_newpath[1])
                _recur(_newlist, _name+'.')
            except:
                pass

            # get module name
            res.append(_name)

    _recur(obj.__path__, obj.__name__+'.')

    return set(res)


def getUserAttrs(obj):
    '''Get a list of all user attributes (not including internal ones)'''


# TODO: may not work! not in use so far
def getAttrType(obj, attr):
    '''Get attribute's type'''

    try:
        return getType(getattr(obj, attr))

    # if attribute does not exist, return NoneType
    except:
        return type(None)


def getAttr(obj, attrname, level=0):
    '''Recursively get attribute(s) from (dotted) name, e.g., aa.bb.cc'''

    from . import strutils

    # if invalid attrname, return `obj` itself
    if not attrname:
        return obj

    # direct getattr
    try:
        return getattr(obj, attrname)
        
    # attrname is a list of attribute names (attrname does not have attribute split)
    except TypeError:
        return [ getAttr(obj, attr) for attr in attrname ]

    # decompose nested attribute names, for example, 'aa.bb.cc.dd' to object `aa` and attribute name 'bb.cc.dd', and process with the method recursively
    except AttributeError:
        return getAttr(getAttr(obj, 
                               strutils.split(attrname, '.')[0]),
                       strutils.split(attrname, '.', maxsplit=1)[1])

#    # decompose nested attribute names, for example, 'aa.bb.cc.dd' to object `aa` and attribute name 'bb.cc.dd', and process with the method recursively
#    try:
#        return getAttr(getattr(obj, attrname.split('.')[0]), attrname.split('.', 1)[1])
#
#    # attrname is a list of attribute names (attrname does not have attribute split)
#    except AttributeError:
#        return [ getAttr(obj, attr) for attr in attrname ]
#
#    # direct getattr
#    except:
#        return getattr(obj, attrname)


# obj can be a sequence (e.g., a list) of numpy arrays
# result to return is not a dtype object, but can be used by dtype()
def type2dtype(obj, descr=False):
    '''Convert Python type to corresponding NumPy dtype'''

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

    # if obj is a list of objects (NumPy arrays), return a list of associated dtypes
    def _getComplexDtype(array_seq):
        return [ ('', type2dtype(arr, descr=descr), (arr.shape[1],)) if arr.ndim>1 else
                 ('', type2dtype(arr, descr=descr),) for arr in array_seq ]
        
    return selectcases[type(obj)](obj)


def obj2dict(obj):
    '''Recursively save information of a ChemShell object to dictionary'''

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
        dictbuff.update({key:selectcases[getType(getattr(obj,key), ndarrayislist=False)](getattr(obj, key))})

    return dictbuff


def setAttr(obj, attr, val, debug=0):
    ''''''

    from .         import nputils
    from .strutils import getRightmost

    # if attr is empty, directly assign value to obj
    if not attr:
        try:
            nputils.setField(obj, val, debug=debug)
            return
        except:
            return

    # first try to directly setattr
    try:
        setattr(obj, attr, val)
    # attr and val could be a sequence of attribute names and a structured array, respectively
    except TypeError:
        for key in attr:
            # get rightmost attr name: namely, 'cc' out of 'aa.bb.cc'
            attrname = getRightmost(key)
            # destination is object `abc` if key is 'abc.def' and `obj` for key 'abc'
            destin = getAttr(obj, getRightmost(key, pos=1))

            # resize only if destin.attr.size exists
            # when setAttr() is used by a file parser (e.g., punch parser), attr may be a tuple of (func(), converter()) and resize according to `val` does not make sense
            if hasattr(getattr(destin, attrname), 'size'):
                # resize destination array
                destin.resize(val.size)
                # assign values directly
                setattr(destin, attrname, val[attrname])

            # setAttr() does not know how to deal with the data and special treatment is needed outside the current routine
            else:
                raise AttributeError


           # try:
           # # val is a structred array: set fields based on names
           # except:
           #     nputils.setField(destin, val[attrname])



def addProperty(obj, attr):
    '''Make a @property and setter'''

    mangled = '_'+attr

    setattr(obj, attr, property(lambda self: getattr(self, mangled),
                                lambda self, val: setattr(self, mangled, val)))






