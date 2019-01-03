'''DL_PY2F is a library for direct coupling of Python/NumPy and Fortran'''

__author__     = [ 'You Lu', 'Thomas W. Keal' ]
__copyright__  = 'Copyright (C) 2018 The authors of Py-ChemShell'
__credits__    = [ 'You Lu', 'Thomas W. Keal' ]
__license__    = 'LGPLv3'
__version__    = '2018b'
__maintainer__ = __author__[0]
__email__      = 'you.lu@stfc.ac.uk'
__status__     = ''

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
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with Py-ChemShell. If not, see
#  <http://www.gnu.org/licenses/>.

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
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with DL_PY2F. If not, see
#  <http://www.gnu.org/licenses/>.


# change this name as your application needs, for example:
# from ctypes import Structure
# from numpy  import full, zeros
# class Fragment(Structure)
#    _attrs = {
#               'bqs'     : BQs(),
#               'coords'  : zeros(shape=(1,3),     dtype=float64),
#               'connmode':'covalent',
#               'frozen'  : zeros(shape=(1,) ,     dtype=int64),
#               'grid'    : None,
#               'names'   : full((1)         , '', dtype='S8'),
#               'natoms'  : 0,
#             }
__dictname = '_attrs'

def __getType(obj):
    '''Return type'''

    from numpy  import ndarray, float64, float32, int64, int32
    from types  import ModuleType, FunctionType

    # numpy.ndarray treated as list
    if isinstance(obj, ndarray):
        return list

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
    elif isinstance(obj, float64) or isinstance(obj, float32):
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

    # application <objects>
    elif hasattr(obj, __dictname) and type(obj).__module__ is not 'builtins':
        return object

    # anything else
    else:
        return type(obj)


def py2f(obj):
    '''Convert a Python object to <ctypes.Structure> (recursively)'''

    from ctypes import c_long, c_double, c_bool, c_char, c_wchar_p, c_char_p, c_void_p, addressof, pointer, POINTER, Structure
    from numpy  import asarray, ctypeslib
    from types  import ModuleType

    MAXLEN = 256
    ATTRLEN = 16

    # sort the attributes to fix the order
    dictbuff = getattr(obj, __dictname)
    _attrs = sorted(dictbuff.items())

    # add an attribute name to each attribute by concatenating " "*MAXLEN to the original name
    fields      = []
    initialiser = []

    def __list2ndp(foo):
        '''<list> to <numpy.ctypeslib.ndpointer>'''

        # <numpy.ndarray>
        if hasattr(foo, "dtype"):

            def _addNPArray(_ctype):
                '''Convert to uniform ndarray and add to fields and initialiser'''

                npptr = ctypeslib.ndpointer(_ctype)

                # only copy shape when obj is fully ready, otherwise ndpointer is destroyed
                if getattr(obj, key).size > 0:
                    npptr._shape_ = getattr(obj, key).shape

                # do NOT use ndarray.astype(_ctype) which does not work
                fields.append((key, npptr))
                initialiser.append(asarray(getattr(obj, key), dtype=_ctype).ctypes.data)

            selectcases = { 'int64'  : c_long,
                            'int32'  : c_long,
                            'float64': c_double,
                            'float32': c_double,
                            'bool'   : c_long,
                            'object' : c_long,
                            'bytes64': c_char_p,
                            'bytes32': c_char_p,
                            'str32'  : c_char_p,
                            'str64'  : c_char_p,
                            'str256' : c_char_p
                          }

            _addNPArray(selectcases[foo.dtype.name])

        # <tuple>/<list> of integer, will be converted to ndarray
        else:
            try:
                initialiser.append(asarray([i for i in getattr(obj, key)], dtype=c_long).ctypes.data)
                fields.append((key, ctypeslib.ndpointer(c_long)))
            except:
                print(" >>> WARNING: DL_PY2F cannot convert non-integer list/tuple `%s` of"%key, obj, "to NumPy ndarray")
                initialiser.append(None)
                fields.append((key, c_void_p))


    def __int2cint(foo):
        '''<int> to <ctypes.c_long>'''

        fields.append((key, POINTER(c_long)))
        initialiser.append(pointer(c_long(foo)))


    def __str2bytes(foo):
        '''<str> to <bytes>'''

        fields.append((key, c_char_p))
        # OBSOLETE: obj.path could be <module>
#        if isinstance(foo, ModuleType):
#            initialiser.append(c_char_p((foo.__name__ + " "*MAXLEN)[:MAXLEN].encode('ascii')))
#        else:
        initialiser.append(c_char_p((foo + " "*MAXLEN)[:MAXLEN].encode('ascii')))

    def __module2bytes(foo):
        '''<ModuleType> to <bytes>'''

        fields.append((key, c_char_p))
        initialiser.append(c_char_p((foo.__name__ + " "*MAXLEN)[:MAXLEN].encode('ascii')))


    # type of <object> foo must be ctypes.Structure or its offspring, otherwise TypeError is raised
    def __obj2ptr(foo):
        '''<object> to <ctypes.pointer> of <ctypes.Structure>, recursively'''

        bar = py2f(foo)
        fields.append((key, POINTER(type(bar))))
        initialiser.append(pointer(bar))


    # we currently do not support dict type so convert to None
    def __dict2None(foo):
        '''<dict> to c Null (<None>)'''

        print(" >>> DL_PY2F WARNING: unsupport <class dict> entry \""+key+"\" will be treated as None.")

        fields.append((key, c_void_p))
        initialiser.append(None)


    def __float2cdouble(foo):
        '''<float> to <c_double>'''

        fields.append((key, POINTER(c_double)))
        initialiser.append(pointer(c_double(foo)))


    def __bool2cbool(foo):
        '''<bool> to <c_bool>'''

        # c_bool must be passed as pointer, otherwise the item next to it is affected due to c_bool's short byte length
        fields.append((key, POINTER(c_bool)))
        initialiser.append(pointer(c_bool(foo)))


    def __None2ptr(foo):
        '''<None> to c Null'''

        fields.append((key, c_void_p))
        initialiser.append(None)


    selectcases = { 
                    bool      :__bool2cbool,
                    dict      :__dict2None,
                    float     :__float2cdouble,
                    int       :__int2cint,
                    list      :__list2ndp,
                    object    :__obj2ptr,
                    tuple     :__list2ndp,
                    str       :__str2bytes,
                    ModuleType:__module2bytes,
                    # there is no longer <NoneType> in <module> types
                    type(None):__None2ptr }


    for key, val in _attrs:

        # do NOT pass internal attributes
        if key.startswith('_'):
            continue

        # initialiser: A. type names as string byte by applying .encode()
        try:
            initialiser.append((__getType(getattr(obj, key)).__name__ + " "*ATTRLEN)[:ATTRLEN].encode('ascii'))
        except:
            initialiser.append((__getType(getattr(obj, key)).__class__.__name__ + " "*ATTRLEN)[:ATTRLEN].encode('ascii'))


        # initialiser: B. attribute names
        try:
            # in case there are synons
            initialiser.append((obj.synons[key.strip()] + " "*ATTRLEN)[:ATTRLEN].encode('ascii'))
        except:
            initialiser.append((key+' '*ATTRLEN)[:ATTRLEN].encode('ascii'))

        # initialiser: C. dtype name of array (has to be truncated by ATTRLEN!) or '' for scalar
        # initialiser: D. size of array or 0 for scalar
        # initialiser: E. shape of array (0 for scalar)
        if __getType(val) is list:
            abuff = asarray(getattr(obj, key))
            initialiser += [ (abuff.dtype.name+' '*ATTRLEN)[:ATTRLEN].encode('ascii'),
                             c_long(abuff.size),
                             c_long(abuff.shape[0]) ]
        else:
            initialiser += [ (' '*ATTRLEN)[:ATTRLEN].encode('ascii') ] + [ c_long(0) ]*2

        # initialiser: F. if the attribute belongs to the _master array
        initialiser.append(c_bool(__getType(val) is list and key in getattr(obj, '_fields', [])))

        # initialiser: G. attributes (by selecting methods from the above dict)
        try:
            selectcases[__getType(getattr(obj, key))](getattr(obj, key))
        except KeyError:
            print(" >>> DL_PY2F ERROR: type "+str(__getType(getattr(obj, key)))+" of entry \""+key+"\" not supported.\n")
        except:
            print(" >>> DL_PY2F ERROR: error processing entry \""+key+"\".\n")


    # labels for types, attributes, etc. (CStructType in objects.f90)
    # string names can be arbitrary and different from their Fortran counterparts,
    # but identical names are NOT allowed (so use "type"/"size"+name to avoid)
    fields = [ item for sublist in zip(zip([ 'type' + f[0]      for f in fields ], # fields: A. type names
                                           [ c_char*ATTRLEN for f in fields ]),
                                       zip([ f[0] + " "*ATTRLEN for f in fields ], # fields: B. attribute names
                                           [ c_char*ATTRLEN for f in fields ]),
                                       zip([ 'dtype' + f[0]     for f in fields ], # fields: C. NumPy dtype
                                           [ c_char*ATTRLEN for f in fields ]),
                                       zip([ 'size' + f[0]      for f in fields ], # fields: D. data size
                                           [ c_long         for f in fields ]),
                                       zip([ 'sizem' + f[0]     for f in fields ], # fields: E. data shape[0]
                                           [ c_long         for f in fields ]),
                                       zip([ 'isfield' + f[0]   for f in fields ], # fields: F. if in _fieds (namely if part of the _master array)
                                           [ c_bool         for f in fields ]),
                                       fields)                                     # fields: G. attributes
                    for item in sublist ]

    # number of attributes
    fields.insert(0, ('nattrs', c_long))
    initialiser.insert(0, c_long(len(_attrs)))

    # object name
    fields.insert(1, ('_class', c_char_p))
    initialiser.insert(1, c_char_p((obj.__module__ + '.' + obj.__class__.__name__ + " "*MAXLEN)[:MAXLEN].encode('ascii')))

    # object address
    fields.insert(2, ("_address", c_long))
    initialiser.insert(2, addressof(obj))

    # total width of arrays
    fields.insert(3, ('width', c_long))
    try:
        initialiser.insert(3, c_long(obj._width))
    except:
        initialiser.insert(3, c_long(0))


    # TODO: debugging print
    class CStruct(Structure):
        
        _fields_ = fields

        # has to be __new__() rather than __init__()?
        def __new__(cls, *args):
            ''''''
            inst = Structure.__new__(cls)
            return inst


    return CStruct(*initialiser)






