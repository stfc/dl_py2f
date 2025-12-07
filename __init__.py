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
__author__     = [ 'You Lu', 'Thomas W. Keal' ]
__copyright__  = 'Copyright (C) 2025 The authors of DL_PY2F'
__credits__    = [ 'You Lu', 'Thomas W. Keal' ]
__license__    = 'LGPLv3'
__version__    = '25.0 (beta)'
__maintainer__ = __author__[0]
__email__      = 'you.lu@stfc.ac.uk'
__status__     = ''
__dictname = '_kwargs'
__arrname = '_master'
def __getType(obj):
    from numpy  import ndarray, float64, float32, int64, int32
    from types  import ModuleType, FunctionType
    if isinstance(obj, ndarray):
        return list
    elif isinstance(obj, list):
        return list
    elif isinstance(obj, str):
        return str
    elif isinstance(obj, bytes):
        return bytes
    elif isinstance(obj, float64) or isinstance(obj, float32):
        return float
    elif isinstance(obj, int64) or isinstance(obj, int32):
        return int
    elif isinstance(obj, ModuleType):
        return ModuleType
    elif isinstance(obj, FunctionType):
        return FunctionType
    elif hasattr(obj, __dictname) and type(obj).__module__ != 'builtins':
        return object
    else:
        return type(obj)
def py2f(obj, debug=0, byref=False):
    from ctypes import CFUNCTYPE, c_int, c_long, c_double, c_bool, c_char, c_wchar_p, c_char_p, c_void_p, addressof, pointer, POINTER, Structure
    from numpy  import array, asarray, ctypeslib, ma, str_
    from types  import ModuleType
    from sys    import stdout
    if debug > 2:
        print("\n DL_PY2F: obj type = {}\n".format(type(obj)))
        stdout.flush()
    MAXLEN = 256
    ATTRLEN = 32
    dictbuff = getattr(obj, __dictname)
    _kwargs = sorted(dictbuff.items())
    fbuff       = []
    initialiser = []
    def _appendInitAtoF(_initialiser, _obj, _key, _default_type):
        if isinstance(_obj, ma.core.MaskedArray):
            databuff = _obj.torecords()['_data'][_key]
        else:
            databuff = getattr(_obj, _key)
        try:
            _initialiser.append((__getType(databuff).__name__ + " "*ATTRLEN)[:ATTRLEN].encode('ascii'))
        except:
            _initialiser.append((__getType(databuff).__class__.__name__ + " "*ATTRLEN)[:ATTRLEN].encode('ascii'))
        try:
            _initialiser.append((_obj.synons[_key.strip()] + " "*ATTRLEN)[:ATTRLEN].encode('ascii'))
        except:
            _initialiser.append((_key+' '*ATTRLEN)[:ATTRLEN].encode('ascii'))
        if _default_type is list:
            abuff = asarray(databuff)
            if abuff.dtype.name.startswith('void'):
                size = abuff.size*len(abuff.dtype.names)
            else:
                size = abuff.size
            _initialiser += [ (abuff.dtype.name+' '*ATTRLEN)[:ATTRLEN].encode('ascii'),
                             c_long(size),
                             c_long(abuff.shape[0]) ]
        else:
            _initialiser += [ (' '*ATTRLEN)[:ATTRLEN].encode('ascii') ] + [ c_long(0) ]*2
        if isinstance(_obj, ma.core.MaskedArray):
            _initialiser.append(c_bool(_default_type is list and _key in _obj.torecords()['_data'].dtype.fields))
        else:
            _initialiser.append(c_bool(_default_type is list and _key in getattr(_obj, '_fields', [])))
    def __list2ndp(obj, key, foo):
        if hasattr(foo, "dtype"):
            selectcases = { 'int64'   : c_long,
                            'int32'   : c_int,
                            'float64' : c_double,
                            'float32' : c_double,
                            'bool'    : c_long,
                            'object'  : c_long,
                            'bytes64' :'U8',
                            'bytes32' :'U8',
                            'bytes128':'U8',
                            'str32'   :'U8',
                            'str64'   :'U8',
                            'str256'  :'U8',
                            'record'  : foo.dtype,
                            'void'    : foo.dtype,
                          }
            def _addNPArray(_ctype, _key, _val):
                npptr = ctypeslib.ndpointer(_ctype)
                if _val.size > 0:
                    npptr._shape_ = _val.shape
                fbuff.append((_key, npptr))
                initialiser.append(asarray(_val, dtype=_ctype).ctypes.data)
            def _addRecArray(_ctype, _arr):
                for field in _ctype.names:
                    _appendInitAtoF(initialiser, foo, field, list)
                    _addNPArray(selectcases[_ctype.fields[field][0].name], field, _arr[field])
            def _addFlexible(_ctype, _key, _val):
                npptr = ctypeslib.ndpointer(_ctype)
                if _val.size > 0:
                    npptr._shape_ = (_val.shape[0],len(_val.dtype.names))
                fbuff.append((_key, npptr))
                initialiser.append(asarray(_val, dtype=_ctype).ctypes.data)
            try:
                _addNPArray(selectcases[foo.dtype.name], key, getattr(obj, key))
            except KeyError:
                if foo.dtype.name[:6] == 'record':
                    _addRecArray(selectcases[foo.dtype.name[:6]], getattr(obj, key))
                elif foo.dtype.name[:4] == 'void':
                    _addFlexible(selectcases[foo.dtype.name[:4]], key, getattr(obj, key))
        else:
            try:
                initialiser.append(asarray([i for i in getattr(obj, key)], dtype=c_long).ctypes.data)
                fbuff.append((key, ctypeslib.ndpointer(c_long)))
            except ValueError:
                if all([type(i) in [str,str_] for i in getattr(obj, key)]):
                    fbuff.append((key, c_char_p))
                    initialiser[-6] = ('str'+' '*ATTRLEN)[:ATTRLEN].encode('ascii')
                    initialiser.append(c_char_p((';'.join(foo) + " "*MAXLEN)[:MAXLEN].encode('ascii')))
                else:
                    initialiser.append(None)
                    fbuff.append((key, c_void_p))
            except:
                if debug > 2:
                    print(" >>> DL_PY2F WARNING: cannot convert non-integer list/tuple `%s` of"%key, obj, "to NumPy ndarray")
                initialiser.append(None)
                fbuff.append((key, c_void_p))
    def __int2cint(obj, key, foo):
        fbuff.append((key, POINTER(c_long)))
        initialiser.append(pointer(c_long(foo)))
    def __func2cfunptr(obj, key, foo):
        from typing import get_type_hints
        selectcases = { float: c_double,
                        int  : c_long }
        c_type = selectcases.get(get_type_hints(foo).get('return', int), c_long)
        fbuff.append((key, POINTER(CFUNCTYPE(c_type, c_void_p))))
        initialiser.append(pointer(CFUNCTYPE(c_type, c_void_p)(foo)))
    def __str2bytes(obj, key, foo):
        fbuff.append((key, c_char_p))
        initialiser.append(c_char_p((foo + " "*MAXLEN)[:MAXLEN].encode('ascii')))
    def __module2bytes(obj, key, foo):
        fbuff.append((key, c_char_p))
        initialiser.append(c_char_p((foo.__name__ + " "*MAXLEN)[:MAXLEN].encode('ascii')))
    def __obj2ptr(obj, key, foo):
        bar = py2f(foo)
        fbuff.append((key, POINTER(type(bar))))
        initialiser.append(pointer(bar))
    def __dict2None(obj, key, foo):
        if debug > 2:
            print(" >>> DL_PY2F WARNING: unsupport <class dict> entry \""+key+"\" will be treated as None.")
        fbuff.append((key, c_void_p))
        initialiser.append(None)
    def __float2cdouble(obj, key, foo):
        fbuff.append((key, POINTER(c_double)))
        initialiser.append(pointer(c_double(foo)))
    def __bool2cbool(obj, key, foo):
        fbuff.append((key, POINTER(c_bool)))
        initialiser.append(pointer(c_bool(foo)))
    def __None2ptr(obj, key, foo):
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
                    type(None)      :__None2ptr }
    for key, default_val in _kwargs:
        if key.startswith('_'):
            continue
        try:
            isRec = getattr(obj, key).dtype.name.startswith('record')
        except:
            isRec = False
        if not isRec:
            _appendInitAtoF(initialiser, obj, key, __getType(default_val))
        try:
            selectcases[__getType(getattr(obj, key))](obj, key, getattr(obj, key))
        except KeyError:
            print(" >>> DL_PY2F ERROR: type "+str(__getType(getattr(obj, key)))+" of entry \""+key+"\" not supported.\n")
    fields = [ item for sublist in zip(zip([ 'type'    + f[0]   for f in fbuff ],
                                           [ c_char*ATTRLEN     for f in fbuff ]),
                                       zip([ f[0] + ' '*ATTRLEN for f in fbuff ],
                                           [ c_char*ATTRLEN     for f in fbuff ]),
                                       zip([ 'dtype'   + f[0]   for f in fbuff ],
                                           [ c_char*ATTRLEN     for f in fbuff ]),
                                       zip([ 'size'    + f[0]   for f in fbuff ],
                                           [ c_long             for f in fbuff ]),
                                       zip([ 'sizem'   + f[0]   for f in fbuff ],
                                           [ c_long             for f in fbuff ]),
                                       zip([ 'isfield' + f[0]   for f in fbuff ],
                                           [ c_bool             for f in fbuff ]),
                                       fbuff)
                    for item in sublist ]
    if len(fields) != len(initialiser) or debug > 4:
        if len(fields) != len(initialiser):
            print(" >>> DL_PY2F WARNING: fields (len: {}) and initialiser (len: {}) mismatch:".format(len(fields),len(initialiser)))
            print("    Python object:", obj)
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
    stdout.flush()
    fields.insert(0, ('nattrs', c_long))
    initialiser.insert(0, c_long(int((len(fields)-1)/7)))
    fields.insert(1, ('_class', c_char_p))
    initialiser.insert(1, c_char_p((obj.__module__ + '.' + obj.__class__.__name__ + " "*MAXLEN)[:MAXLEN].encode('ascii')))
    fields.insert(2, ('_address', c_long))
    initialiser.insert(2, addressof(obj))
    fields.insert(3, ('width', c_long))
    try:
        if hasattr(obj, __arrname):
            _master = getattr(obj, __arrname)
            width_list = [ _master[field].shape[1] if _master[field].ndim == 2 else
                           array(_master.dtype.fields[field][0].names).size for field in _master.dtype.fields ]
            _width = sum(width_list)
        initialiser.insert(3, c_long(_width))
    except:
        initialiser.insert(3, c_long(0))
    fields.insert(4, ('debug', POINTER(c_long)))
    initialiser.insert(4, pointer(c_long(debug)))
    stdout.flush()
    if debug >= 5:
        print("\n >>> DL_PY2F DEBUG: Components of wrapped Python object", obj)
        print("\n     {:5}    {:32}    {:24} {}".format('index', 'field name', 'field type', 'value'))
        for i, v in enumerate(initialiser):
            if not (i-5)%7:
                print()
            print("     {:5d}    {:32.32}    {:24.24} {}".format(i, '\"'+fields[i][0]+'\"', fields[i][1].__name__, v))
    class CStruct(Structure):
        _fields_ = fields
        def __new__(cls, *args):
            inst = Structure.__new__(cls)
            stdout.flush()
            return inst
    if not byref:
        return CStruct(*initialiser)
    else:
        from ctypes import byref as ctypes_byref
        from sys    import stdout
        try:
            return ctypes_byref(CStruct(*initialiser))
        except:
            print(" >>> DL_PY2F ERROR: obj =", obj)
            stdout.flush()
            raise
import os, sys
sys.path.append(os.path.dirname(__file__))
from .dl_f2py import DL_DL
from . import utils
