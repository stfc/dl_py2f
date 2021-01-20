'''DL_PY2F is a library for direct coupling of Python/NumPy and Fortran'''

__author__     = [ 'You Lu', 'Thomas W. Keal' ]
__copyright__  = 'Copyright (C) 2018 The authors of Py-ChemShell'
__credits__    = [ 'You Lu', 'Thomas W. Keal' ]
__license__    = 'LGPLv3'
__version__    = '19.0 (beta)'
__maintainer__ = __author__[0]
__email__      = 'you.lu@stfc.ac.uk'
__status__     = ''

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
    elif hasattr(obj, __dictname) and type(obj).__module__ != 'builtins':
        return object

    # anything else
    else:
        return type(obj)


def py2f(obj, debug=0, byref=False):
    '''Convert a Python object to <ctypes.Structure> (recursively)'''

    from ctypes import c_long, c_double, c_bool, c_char, c_wchar_p, c_char_p, c_void_p, addressof, pointer, POINTER, Structure
    from numpy  import asarray, ctypeslib, ma
    from types  import ModuleType
    from sys    import stdout

    if debug > 2:
        print("\n DL_PY2F: obj type = {}\n".format(type(obj)))
        stdout.flush()

    MAXLEN = 256
    ATTRLEN = 16

    # sort the attributes to fix the order
    dictbuff = getattr(obj, __dictname)
    _attrs = sorted(dictbuff.items())

    # add an attribute name to each attribute by concatenating " "*MAXLEN to the original name
    fbuff       = []
    initialiser = []

# YL 08/078/2019: failed to overload an attribute with a property() instance to get value from ctype-converted
#                 initialiser to enable %set() function for scalar values
#    def __scalar2property(obj, key):
#        ''''''
#
#        try:
#            setattr(obj, key, property(lambda self: getattr(getattr(self, '__'+key+'_ctype'), 'value')))
#        except:
#            pass

    def _appendInitAtoF(_initialiser, _obj, _key, _default_type):

        # YL 28/01/2020: allow numpy.ma.core.MaskedArray wrapper
        if isinstance(_obj, ma.core.MaskedArray):
            databuff = _obj.torecords()['_data'][_key]
        else:
            databuff = getattr(_obj, _key)

        # initialiser: A. type names as string byte by applying .encode()
        try:
            _initialiser.append((__getType(databuff).__name__ + " "*ATTRLEN)[:ATTRLEN].encode('ascii'))
        except:
            _initialiser.append((__getType(databuff).__class__.__name__ + " "*ATTRLEN)[:ATTRLEN].encode('ascii'))


        # initialiser: B. attribute names
        try:
            # in case there are synons
            _initialiser.append((_obj.synons[_key.strip()] + " "*ATTRLEN)[:ATTRLEN].encode('ascii'))
        except:
            _initialiser.append((_key+' '*ATTRLEN)[:ATTRLEN].encode('ascii'))

        # initialiser: C. dtype name of array (has to be truncated by ATTRLEN!) or '' for scalar
        # initialiser: D. size of array or 0 for scalar
        # initialiser: E. shape of array (0 for scalar)
        if _default_type is list:
            abuff = asarray(databuff)
            _initialiser += [ (abuff.dtype.name+' '*ATTRLEN)[:ATTRLEN].encode('ascii'),
                             c_long(abuff.size),
                             c_long(abuff.shape[0]) ]
        else:
            _initialiser += [ (' '*ATTRLEN)[:ATTRLEN].encode('ascii') ] + [ c_long(0) ]*2

        # initialiser: F. if the attribute belongs to the _master array
        if isinstance(_obj, ma.core.MaskedArray):
            _initialiser.append(c_bool(_default_type is list and _key in _obj.torecords()['_data'].dtype.fields))
        else:
            _initialiser.append(c_bool(_default_type is list and _key in getattr(_obj, '_fields', [])))

    def __list2ndp(obj, key, foo):
        '''<list> to <numpy.ctypeslib.ndpointer>'''

        # <numpy.ndarray>
        if hasattr(foo, "dtype"):

            # YL 16/07/2019: from NumPy 1.16, ctypeslib.ndpointer() no longer supports c_char_p but it didn't work at all anyway
            #                (and luckily array of characters hasn't been need by Fortran). so i'm changing all c_char_p to 'U8' here
            #                but haven't validated if it can work. and in fact here requires a dtype of NumPy rather than a ctypes type
            selectcases = { 'int64'  : c_long,
                            'int32'  : c_long,
                            'float64': c_double,
                            'float32': c_double,
                            'bool'   : c_long,
                            'object' : c_long,
                            'bytes64':'U8',
                            'bytes32':'U8',
                            'str32'  :'U8',
                            'str64'  :'U8',
                            'str256' :'U8',
                            'record' : foo.dtype,
                            'void'   : foo.dtype,
                          }

            # NumPy ndarray
            def _addNPArray(_ctype, _key, _val):
                '''Convert to uniform ndarray and add to fields (fbuff) and initialiser'''

                npptr = ctypeslib.ndpointer(_ctype)

                # only copy shape when obj is fully ready, otherwise ndpointer is destroyed
                if _val.size > 0:
                    npptr._shape_ = _val.shape

                # do NOT use ndarray.astype(_ctype) which does not work
                fbuff.append((_key, npptr))
                initialiser.append(asarray(_val, dtype=_ctype).ctypes.data)

            # NumPy recarray
            def _addRecArray(_ctype, _arr):
                '''Record array'''

                for field in _ctype.names:
                    # we skipped initialisers A-F
                    _appendInitAtoF(initialiser, foo, field, list)
                    _addNPArray(selectcases[_ctype.fields[field][0].name], field, _arr[field])

            # YL TODO 30/09/2020: currently this is only a place holder to let the code run but the Fortran
            #                     side isn't working at all! (we'll need another complicated parser to comprehend
            #                     this datatype but we may never need it in any use case)
            def _addFlexible(_ctype, _key, _val):
                '''Flexible datatype (name: 'voidxxx')'''

                npptr = ctypeslib.ndpointer(_ctype)

                # only copy shape when obj is fully ready, otherwise ndpointer is destroyed
                if _val.size > 0:
                    npptr._shape_ = _val.shape

                # do NOT use ndarray.astype(_ctype) which does not work
                fbuff.append((_key, npptr))
                initialiser.append(asarray(_val, dtype=_ctype).ctypes.data)


            try:
                _addNPArray(selectcases[foo.dtype.name], key, getattr(obj, key))
            except KeyError:
                if foo.dtype.name[:6] == 'record':
                    _addRecArray(selectcases[foo.dtype.name[:6]], getattr(obj, key))
                # to let entities of flexible datatype pass
                elif foo.dtype.name[:4] == 'void':
                    _addFlexible(selectcases[foo.dtype.name[:4]], key, getattr(obj, key))

        # <tuple>/<list> of integer, will be converted to ndarray
        else:
            try:
                initialiser.append(asarray([i for i in getattr(obj, key)], dtype=c_long).ctypes.data)
                fbuff.append((key, ctypeslib.ndpointer(c_long)))
            except:
                print(" >>> WARNING: DL_PY2F cannot convert non-integer list/tuple `%s` of"%key, obj, "to NumPy ndarray")
                initialiser.append(None)
                fbuff.append((key, c_void_p))


    def __int2cint(obj, key, foo):
        '''<int> to <ctypes.c_long>'''

        fbuff.append((key, POINTER(c_long)))
        initialiser.append(pointer(c_long(foo)))


    def __func2cfunptr(obj, key, foo):
        '''<class function> to <ctypes.CFUNCTYPE>'''

        from typing import get_type_hints

        # automatic function returned value detection: requiring type hinting

        selectcases = { float: c_double,
                        int  : c_long }

        # default returned type is int
        c_type = selectcases.get(get_type_hints(foo).get('return', int), c_long)

        # TODO: automatic function arguments detection: requiring type hinting
        fbuff.append((key, POINTER(CFUNCTYPE(c_type, c_void_p))))
        initialiser.append(pointer(CFUNCTYPE(c_type, c_void_p)(foo)))


    def __str2bytes(obj, key, foo):
        '''<str> to <bytes>'''

        fbuff.append((key, c_char_p))
        # OBSOLETE: obj.path could be <module>
#        if isinstance(foo, ModuleType):
#            initialiser.append(c_char_p((foo.__name__ + " "*MAXLEN)[:MAXLEN].encode('ascii')))
#        else:
        initialiser.append(c_char_p((foo + " "*MAXLEN)[:MAXLEN].encode('ascii')))


    def __module2bytes(obj, key, foo):
        '''<ModuleType> to <bytes>'''

        fbuff.append((key, c_char_p))
        initialiser.append(c_char_p((foo.__name__ + " "*MAXLEN)[:MAXLEN].encode('ascii')))


    # type of <object> foo must be ctypes.Structure or its offspring, otherwise TypeError is raised
    def __obj2ptr(obj, key, foo):
        '''<object> to <ctypes.pointer> of <ctypes.Structure>, recursively'''

        bar = py2f(foo)
        fbuff.append((key, POINTER(type(bar))))
        initialiser.append(pointer(bar))


    # we currently do not support dict type so convert to None
    def __dict2None(obj, key, foo):
        '''<dict> to c Null (<None>)'''

        print(" >>> DL_PY2F WARNING: unsupport <class dict> entry \""+key+"\" will be treated as None.")

        fbuff.append((key, c_void_p))
        initialiser.append(None)


    def __float2cdouble(obj, key, foo):
        '''<float> to <c_double>'''

        fbuff.append((key, POINTER(c_double)))
        initialiser.append(pointer(c_double(foo)))


    def __bool2cbool(obj, key, foo):
        '''<bool> to <c_bool>'''

        # c_bool must be passed as pointer, otherwise the item next to it is affected due to c_bool's short byte length
        fbuff.append((key, POINTER(c_bool)))
        initialiser.append(pointer(c_bool(foo)))


    def __None2ptr(obj, key, foo):
        '''<None> to c Null'''

        fbuff.append((key, c_void_p))
        initialiser.append(None)




    selectcases = { 
                    bool            :__bool2cbool,
                    dict            :__dict2None,
                    float           :__float2cdouble,
                    int             :__int2cint,
                    list            :__list2ndp,
                    object          :__obj2ptr,
                    tuple           :__list2ndp,
                    type(lambda x:x):__func2cfunptr,
                    str             :__str2bytes,
                    ModuleType      :__module2bytes,
                    # there is no longer <NoneType> in <module> types
                    type(None)      :__None2ptr }




    for key, default_val in _attrs:

        # do NOT pass "internal" attributes
        if key.startswith('_'):
            continue

        # record arrays need special treatment (see: __list2ndp())
        try:
            isRec = getattr(obj, key).dtype.name.startswith('record')
        except:
            isRec = False
        if not isRec:
            _appendInitAtoF(initialiser, obj, key, __getType(default_val))

        # initialiser: G. attributes (by selecting methods from the above dict)
        try:
            selectcases[__getType(getattr(obj, key))](obj, key, getattr(obj, key))
        except KeyError:
            print(" >>> DL_PY2F ERROR: type "+str(__getType(getattr(obj, key)))+" of entry \""+key+"\" not supported.\n")

    # labels for types, attributes, etc. (CStructType in objects.f90)
    # string names can be arbitrary and different from their Fortran counterparts,
    # but identical names are NOT allowed (so use "type"/"size"+name to avoid)
    fields = [ item for sublist in zip(zip([ 'type'    + f[0]   for f in fbuff ], # fields: A. type names
                                           [ c_char*ATTRLEN     for f in fbuff ]),
                                       zip([ f[0] + ' '*ATTRLEN for f in fbuff ], # fields: B. attribute names
                                           [ c_char*ATTRLEN     for f in fbuff ]),
                                       zip([ 'dtype'   + f[0]   for f in fbuff ], # fields: C. NumPy dtype
                                           [ c_char*ATTRLEN     for f in fbuff ]),
                                       zip([ 'size'    + f[0]   for f in fbuff ], # fields: D. data size
                                           [ c_long             for f in fbuff ]),
                                       zip([ 'sizem'   + f[0]   for f in fbuff ], # fields: E. data shape[0]
                                           [ c_long             for f in fbuff ]),
                                       zip([ 'isfield' + f[0]   for f in fbuff ], # fields: F. if in _fields (namely if part of the _master array)
                                           [ c_bool             for f in fbuff ]),
                                       fbuff)                                     # fields: G. attributes
                    for item in sublist ]

    if len(fields) != len(initialiser) or debug > 4:
        if len(fields) != len(initialiser):
            print(" >>> DL_PY2F WARNING: fields (len: {}) and initialiser (len: {}) mismatch:".format(len(fields),len(initialiser)))
        else:
            print(" >>> DL_PY2F DEBUG: data structure of PY2F object")
        print(" "*4+"-"*48)
        print("    {:20}  |  {}".format('field', 'initialiser'))
        for i in range(max(len(fields), len(initialiser))):
            try:
                if type(initialiser[i]) is bytes and fields[i][0].startswith('type'):
                    print(" "*4+"-"*48)
                print("    {:20}  |  {}".format(fields[i][0].strip(), initialiser[i]))
            except IndexError:
                try:
                    print("    {:20}  |  {}".format('', initialiser[i]))
                except:
                    print("    {:20}  |    ".format(fields[i][0].strip()))
        print(" "*4+"-"*48)
            #    print("### {<20s}  |  {<20s}".format(f[i].strip(), initialiser[i]))
            #except:
            #    print("### {}".format(initialiser[i]))

    stdout.flush()

    # number of attributes
    fields.insert(0, ('nattrs', c_long))
    initialiser.insert(0, c_long(int((len(fields)-1)/7)))

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

    # debugging
    if debug >= 5:
        print("\n >>> DL_PY2F DEBUG: Components of wrapped Python object", obj)
        print("\n     {:5}    {:32}    {:24} {}".format('index', 'field name', 'field type', 'value'))
        for i, v in enumerate(initialiser):
            print("     {:5d}    {:32.32}    {:24.24} {}".format(i, '\"'+fields[i][0]+'\"', fields[i][1].__name__, v))


    # TODO: debugging print
    class CStruct(Structure):
        
        _fields_ = fields

        # has to be __new__() rather than __init__()?
        def __new__(cls, *args):
            ''''''

            inst = Structure.__new__(cls)
            stdout.flush()
            return inst


    if not byref:
        return CStruct(*initialiser)
    else:
        from ctypes import byref as ctypes_byref
        return ctypes_byref(CStruct(*initialiser))






