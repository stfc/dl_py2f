
# you.lu@stfc.ac.uk
# March 2024


import utils
from utils.colours import *
from ctypes import addressof, CDLL, memset, RTLD_GLOBAL, sizeof
from ctypes import c_bool, c_char_p, c_double, c_float, c_int, c_long, c_void_p, POINTER
from ctypes import c_byte, c_char, c_short, c_size_t, c_uint, c_ubyte, c_ulong, c_void_p, c_wchar
from _ctypes import _SimpleCData
import libpath
import intel
    

selectcases = { float    : c_double,
                c_double : c_double,
                c_float  : c_float,
                int      : c_int,
                c_int    : c_int,
               'CHARACTER1': c_char_p,
               'INTEGER4': c_int,
               'INTEGER8': c_long,
               'REAL4'   : c_float,
               'REAL8'   : c_double,
               'LOGICAL4': c_bool, # TODO logical of other lengths
              }

typestrs = { c_double : 'f8',
             c_float  : 'f4',
             c_long   : 'i8',
             c_int    : 'i4',
             c_bool   : 'b4',
           }


# NB: no method of this class can be of a name in all lower case! (not to be conflicting with Fortran components)
class DL_DT(_SimpleCData):
    '''Subclass of _ctypes._SimpleCData of which an instance is used for containing a comprehended Fortran derived-type object'''

    # this must be a single character string containing one of
    # 'cbBhHiIlLdfuzZqQPXOv?g' although 'O' has been taken by <class 'py_object'>
    _type_ = "O"

    def __new__(cls, address, dict_dt, derived_types, debug=False):
        ''''''

        # create an instance of <class CDLL>
        inst = _SimpleCData.__new__(cls)

        return inst


    # override the built-in __repr__() of <class 'py_object'>
    def __repr__(self):
        ''''''

        return f'<dl_f2py.DL_DT object at {self.__address__}>'


    def __init__(self, address, dict_dt, derived_types, debug=False):
        '''Initialiser of an instance'''

        from math      import prod
        from numpy     import array, empty
        from itertools import product
        from inspect   import stack

        if type(dict_dt) is not dict or not dict_dt.get('_is_derived', False):
            print('\n >>> DL_F2PY ERROR: Argument dict_dt must be a dict containing information of data structure of a Fortran derived type', flush=True)
            exit(999)

        # beginning of address
        setattr(self, '__address__', address)

        # to be used with the NumPy array interface
        class _wrapper():
            pass



        for k, v in dict_dt.items():
            if type(v) is dict:
                if '_offset' in v:
                    # arrays: do not check `dim in v`!
                    if '_ndims' in v:
                        # derived type
                        if v.get('_is_derived', False):
                            # deferred shape
                            if v.get('_is_deferred', False):
                                offset = v['_offset']
                                ndims = v['_ndims']
                                address = c_ulong.from_address(self.__address__+offset).value
                                # an array descriptor has 40+ndims*24 bytes; in Gfortran its source code is like this:
                                #     struct descriptor_dimension
                                #     {
                                #       ptrdiff_t stride;
                                #       ptrdiff_t lower_bound;
                                #       ptrdiff_t upper_bound;
                                #     };
                                #     
                                #     template <int Rank, typename Type>
                                #     struct descriptor {
                                #       Type *base_addr;
                                #       size_t offset;
                                #       ptrdiff_t dtype;
                                #       descriptor_dimension dim[Rank];
                                #     };
                                # where dtype comprises: size of the element type, element type, rank of the array
                                # see: https://thinkingeek.com/2017/01/14/gfortran-array-descriptor/
                                lbuff = [ c_long.from_address(self.__address__+offset+i*8).value for i in range(5+ndims*3) ]
#                                ibuff = [ c_int.from_address(self.__address__+offset+i*4).value for i in range(10+ndims*6) ]
                                # the last ndims sets of triples indicate the shape
                                lbounds = lbuff[6::3]
                                ubounds = lbuff[7::3]
                                shape = tuple(ubounds[x]-lbounds[x] for x in range(ndims))
                                # this allocatable/poiter array hasn't been allocated or is invalid
                                if any([x <= 0 for x in shape]) or not lbuff[0]:
                                    setattr(self, k, c_void_p(None))
                                    continue
                                shape = tuple(x+1 for x in shape)
                                # check if sizes match
                                if lbuff[2] != v['_size_chunk']:
                                    print(f'\n >>> WARNING: There may be something wrong with derived-type array {k}!')
                                    print(f'              Shape: {shape}')
                                # create a container instance with an array
                                value = DL_DT(self.__address__+offset, derived_types[v['_type']], derived_types, debug=debug)
                                setattr(value, '_derived_type', v['_type'])
                                value.__array__ = empty(shape=shape[::-1], dtype=object)
                                setattr(self, k, value)
                                # Fortran is column-major so that we loop over the rightmost index first which is what itertools.product() does
                                dims = [ range(x) for x in shape[::-1] ]
                                indices = product(*dims)
                                # set each element as an instance
                                for i, ind in enumerate(indices):
                                    addr = address + i*v['_size_chunk']
                                    value.__array__[ind] = DL_DT(addr, derived_types[v['_type']], derived_types, debug=debug)
                                    # name of derived type
                                    setattr(value.__array__[ind], '_derived_type', v['_type'])
                            # explicit shape
                            else:
                                # create a container instance with an array
                                value = DL_DT(self.__address__+v['_offset'], derived_types[v['_type']], derived_types, debug=debug)
                                value.__array__ = empty(shape=v['_dim'][::-1], dtype=object)
                                setattr(self, k, value)
                                # Fortran is column-major so that we loop over the rightmost index first which is what itertools.product() does
                                dims = [ range(x) for x in v['_dim'][::-1] ]
                                indices = product(*dims)
                                # set each element as an instance
                                for i, ind in enumerate(indices):
                                    addr = self.__address__+v['_offset']+i*v['_size_chunk']
                                    value.__array__[ind] = DL_DT(addr, derived_types[v['_type']], derived_types, debug=debug)

                        # standard type
                        else:
                            # deferred shape
                            if v.get('_is_deferred', False):
                                offset = v['_offset']
                                address = c_ulong.from_address(self.__address__+offset).value
                                ndims = v['_ndims']
                                # an array descriptor has 40+ndims*24 bytes (see the comment above)
                                lbuff = [ c_long.from_address(self.__address__+offset+i*8).value for i in range(5+ndims*3) ]
                                lbounds = lbuff[6::3]
                                ubounds = lbuff[7::3]
                                shape = tuple(ubounds[x]-lbounds[x]+1 for x in range(ndims))
                                # this allocatable/poiter array hasn't been allocated
                                if any([x <= 0 for x in shape]) or not lbuff[0]:
                                    setattr(self, k, c_void_p(None))
                                    continue
                                # check if sizes match
                                if lbuff[2] != sizeof(v['_type']):
                                    print(f'\n >>> WARNING: There may be something wrong with derived-type array {k}!')
                                size = lbuff[2]
                                if v['_type'] in typestrs and size:
                                    buff = _wrapper()
                                    typestr = typestrs[v['_type']]
                                    buff.__array_interface__ = {'shape':shape[::-1], 'data':(address, False), 'typestr': f'<{typestr}'}
                                    try:
                                        aview = array(buff, copy=False)
                                        setattr(self, k, aview)
                                    except:
                                        continue
                            # explicit shape
                            else:
                                if v['_type'] in typestrs:
                                    offset = v['_offset']
                                    address = self.__address__ + v['_offset']
                                    buff = _wrapper()
                                    typestr = typestrs[v['_type']]
                                    buff.__array_interface__ = {'shape':v['_dim'][::-1], 'data':(address, False), 'typestr': f'<{typestr}'}
                                    try:
                                        aview = array(buff, copy=False)
                                        setattr(self, k, aview)
                                    except:
                                        continue
                               

                    # scalar values
                    else:
                        # derived type
                        if v['_type'] in derived_types:
                            value = DL_DT(self.__address__+v['_offset'], derived_types[v['_type']], derived_types, debug=debug)
                            setattr(value, '_derived_type', v['_type'])
                            setattr(self, k, value)
                        # standard type
                        else:
                            # character
                            if v.get('_is_char', False):
#                                sbuff_getter = f'print("\\n    ### NAME = {_bC}{k}{CLR_} {self.__address__+v["_offset"]}"); return self._getStr({self.__address__+v["_offset"]}, {v.get("_length", 1)})'
                                # TODO
                                if v.get('_is_deferred', False):
                                    pass
                                else:
                                    # we can never get this dynamically at runtime but the only method is to save the address/length as an object attribute
                                    setattr(self, f'_DL_DT_CA_{k}', self.__address__+v['_offset'])
                                    setattr(self, f'_DL_DT_CL_{k}', v.get('_length', 1))
                                    sbuff_getter = f'return self._getStr(self._DL_DT_CA_{k}, self._DL_DT_CL_{k}, debug={debug})'
                                    sbuff_setter = f'self._setStr(self._DL_DT_CA_{k}, self._DL_DT_CL_{k}, value, debug={debug}); return 0'
#                                    setter_args = f'{self.__address__+v["_offset"]}, {v.get("_length", 1)}'
                            # other types
                            else:
                                if v.get('_is_pointer', False):
                                    # self._DL_DT_P_{k} is the pointer's address (not the target's address!)
                                    sbuff_getter = f'return self._getPtr(self._DL_DT_P_{k}, {v["_type"].__name__}, name="{k}", debug={debug})'
                                    # we can never get this dynamically at runtime but the only method is to save the address as an object attribute
                                    setattr(self, f'_DL_DT_P_{k}', self.__address__+v["_offset"])
                                    sbuff_setter = f'self._setPtr(self._DL_DT_P_{k}, {v["_type"].__name__}, value, name="{k}", debug={debug}); return 0'
                                else:
                                    sbuff_getter = ''
                                    sbuff_setter = ''
                                    # TODO type-bound procedures
                                    if v['_type'] == '_tb_procedure':
                                        continue
                                    value = v['_type'].from_address(self.__address__+v['_offset'])
                                    setter_args = f'self._DL_DT_{k}'
                                    setattr(self, '_DL_DT_'+k, value)
                            # this is a masterly knack to make the immutable scalar values mutable! ;P
                            sbuff = f'''
# acting as @property for _DL_DT_{k}
def _fget(self):
    {sbuff_getter}
    return self._DL_DT_{k}
'''
                            exec(sbuff)
                            sbuff = f'''
# acting as @_DL_DT_{k}.setter
def _fset(self, value):
    {sbuff_setter}
    # use the static methods of <class 'DL_DL'> to set values
    setters.get(type(self._DL_DT_{k}), DL_DL.setInt)({setter_args}, value)
'''
                            exec(sbuff)
                            # lambda won't work here because it cannot take variable name dynamically, for example:
                            # if we define `setattr(self, '_get_DL_DT_'+k, lambda self : getattr(self, '_DL_DT_'+k))`
                            # then `k` will remain holding the value of the last time it was defined (but not the
                            # runtime name)
                            setattr(self, '_get_DL_DT_'+k, locals()['_fget'])
                            setattr(self, '_set_DL_DT_'+k, locals()['_fset'])
                            # property must be defined to class rather than instance of class!
                            setattr(self.__class__, k, property(fget=getattr(self, '_get_DL_DT_'+k),
                                                                fset=getattr(self, '_set_DL_DT_'+k)))
                # no offset
                else:
                    pass


    def printBytes(self, dict_dt={}, name='', addr=0, increment=4, size_extra=0):
        ''''''

        if dict_dt:
            # print every character
            if dict_dt.get('_is_char', False):
                increment = 1
    
            print()
            if name:
                print(f' Name: {name}')
            print(' Type:', dict_dt['_type'])
            addr = self.__address__ + dict_dt['_offset']
            print(' Addr:', addr)
            print(' Incr:', increment, 'Bytes')
    
            # array
            if 'ndims' in dict_dt:
                ndims = dict_dt['_ndims']
                # we print only the array descriptor for an array with deferred shape
                if dict_dt.get('_is_deferred', False):
                    # an array descriptor has 40+ndims*24 bytes
                    size = 40+ndims*24
            # scalar
            else:
                if dict_dt.get('_is_char', False):
                    size = dict_dt.get('_length', 1)
                else:
                    # pointer
                    if dict_dt.get('_is_pointer', False):
                        size = 8
                    else:
                        size = 0

        else:
            if not addr:
                print('\n >>> ERROR: `addr` must be defined when dict is not given!')
                return
            size = 0

        for i in range(0, size+size_extra, increment):
            print(f'\n No. {int(i/4)}: Offset {i} Bytes', flush=True)
            ibuff  = c_int   .from_address(addr+i)
            lbuff  = c_long  .from_address(addr+i)
            ulbuff = c_ulong .from_address(addr+i)
            cbuff  = c_char  .from_address(addr+i)
            bbuff  = c_ubyte .from_address(addr+i)
            fbuff  = c_float .from_address(addr+i)
            dbuff  = c_double.from_address(addr+i)
            vbuff  = c_void_p.from_address(addr+i)
            wbuff  = c_wchar .from_address(addr+i)
            print(' '+'-'*70, flush=True)
            print('    address  =', addr+i)
            print('    c_int    =',  ibuff.value)
            print('    c_long   =',  lbuff.value)
            print('    c_ulong  =', ulbuff.value)
            print('    c_char   =',  cbuff.value)
            print('    c_ubyte  =',  bbuff.value)
            print('    c_float  =',  fbuff.value)
            print('    c_double =',  dbuff.value)
            print('    c_void_p =',  vbuff.value, flush=True)
            print(' '+'-'*70, flush=True)


    def _getStr(self, addr, length, debug=False):
        ''''''

        sbuff = ''

        for i in range(0, length, 1):
            sbuff += c_char.from_address(addr+i).value.decode()

        return sbuff


    def _setStr(self, addr, length, value, debug=False):
        '''Setter of string (Fortran character)'''

        DL_DL.setChar(addr, length, value)


    def _getPtr(self, addr, ctype, name='', debug=False):
        ''''''

        addr_target = c_ulong.from_address(addr).value
        if debug:
            self.printBytes(name=name, addr=addr, size_extra=8)
        if addr_target:
            value = ctype.from_address(addr_target)
            setattr(value, '_is_associated', True)
            return value
        else:
            value = c_void_p(None)
            setattr(value, '_is_associated', False)
            return value


    def _setPtr(self, addr, ctype, value, name='', debug=False):
        ''''''

        attr = self._getPtr(addr, ctype, debug=debug)

        if attr:
            setters.get(type(attr), DL_DL.setInt)(attr, value)
        else:
            print(f' >>> DL_F2PY ERROR: Pointer "{name}" in {self} is not allocated/associated')


    def setValue(self, name, value):
        ''''''

        # all names are case insensitive
        attr = getattr(self, name.strip().lower())

        # use the static methods of <class 'DL_DL'> to set values
        setters.get(type(attr), DL_DL.setInt)(attr, value)


    def __iter__(self):
        '''Make me iterable'''

        return self


    def __getitem__(self, index):
        '''Make me indexable'''

        if hasattr(self, '__array__'):
            return self.__array__[index]
        else:
            raise TypeError(f'{self.__repr__()} is not subscriptable')


    def __setitem__(self, index, value, **args):
        '''Throw out a warning because an element from an array of derived-type objects must be protected'''

        print(f' >>> DL_F2PY ERROR: Elements in array {self}')
        print( '                    cannot be replaced')
        print(f'                    (Fortran derived type: "{self._derived_type}", shape: {self.__shape__})')


    def __len__(self):
        '''Return the size of the current object if it is an array or 1 otherwise'''

        try:
            return self.__array__.size
        except:
            return 1


    @property
    def __shape__(self):
        '''Return the shape of the current instance if it is an array otherwise raise an error'''

        if hasattr(self, '__array__'):
            return self.__array__.shape
        else:
            raise AttributeError(f'{self.__repr__()} is scalar and has no shape')

#    def __next__(self):
#        ''''''
#
#        self.current += 1
#        if self.current < self.high:
#            return self.current
#        raise StopIteration



class DL_DL(CDLL):
    '''Daresbury Laboratory Dynamic Library'''

    __modules = []


    def __new__(cls, fullpath_to_lib, mode=RTLD_GLOBAL, debug=False):
        ''''''

        # create an instance of <class CDLL>
        inst = CDLL.__new__(cls)

        # initialise
        inst.__init__(fullpath_to_lib, mode=mode)

        # for later use
        inst._fullpath_to_lib = fullpath_to_lib

        # a dict with all data structures of parsed derived types
        inst._derived_types = {}

        # a dict of all declared instances of derived types
        inst._instances_derived_types = {}

        return inst


    # 30/12/2024: we must override CDLL.__getattr__() which looks up only a shared library's symbols (though sometimes works without this overriding)
    def __getattr__(self, name):
        ''''''

        try:
            if name.startswith('__') and name.endswith('__'):
                raise AttributeError(name)
            func = self.__getitem__(name)
            setattr(self, name, func)
            return func

        except:
            return object.__getattribute__(self, name)


    @property
    def lib_dl_py2f(self):
        '''Return a <class CDLL> instance of DL_PY2F dynamic library'''

# FIXME hardcoded for now
        try:
            return self._lib_dl_py2f
        except:
            self._lib_dl_py2f = utils.modutils.getLinkedLib('dl_py2f', libpath.libpath, is_linked=True)
            return self._lib_dl_py2f


    @property
    def symbols(self):

        self.lib_dl_py2f.getSymbols.argtypes = [ c_void_p, c_char_p ]
        self.lib_dl_py2f.getSymbols.restype = c_char_p
        bbuff = self.lib_dl_py2f.getSymbols(self.lib_dl_py2f.dl_f2py(), self._fullpath_to_lib.strip().encode())
        lbuff = bbuff.decode().split(';')
    
        return [ s for s in lbuff if s ]


    def searchSymbol(self, *keywords):
        ''''''

        symbols_all = self.symbols

        return [ s for s in symbols_all if all([ k.strip().lower() in s.strip().lower() for k in keywords ]) ]


    @property
    def modules(self):

        symbols_all = self.symbols
    
        lbuff = [ s.split('_MOD_')[0].lstrip('__') for s in symbols_all if '_MOD_' in s ]
        return sorted(list(set(lbuff)))


    @property
    def nsymbols(self):
        '''Number of symbols'''

        return len(self.symbols)


    def getModuleSymbols(self, module):
        ''''''

        symbols_all = self.symbols

        return [ s for s in symbols_all if module.strip().lower() in s.lower() ]


    def getSymbolOfModule(self, symbol, module):

        if module:
            symbols_module = self.getModuleSymbols(module)
            lbuff = [ s for s in symbols_module if s.lower().rstrip('_').endswith('_'+symbol.strip().lower()) ]
            if len(lbuff) == 0:
                print(f' >>> DL_PY2F ERROR: Module {module} does not have symbol "{symbol}"\n', flush=True)
                exit(999)
            elif len(lbuff) == 1:
                return lbuff[0]
            else:
                print(f' >>> DL_PY2F ERROR: Module {module} has more than one symbols containing string "{symbol}":')
                print( '                    ', ' '.join(lbuff))
                exit(999)
        else:
            return symbol


    def getValue(self, symbol, ctype=c_int, module='', return_ctype=False, debug=False):
        '''Get value(s) of a symbol'''

        lbuff = []
        if '%' in symbol:
            lbuff = symbol.split('%')
            symbol = lbuff[0]
        print("\n\n### getValue: symbol =", symbol, flush=True)

        # get the full symbol name
        fullname_symbol = self.getSymbolOfModule(symbol, module)

        _instances_derived_types = self._instances_derived_types.get(module, {})
        print("### _instances_derived_types =", _instances_derived_types)
        derived_type = _instances_derived_types.get(symbol, None)
        print("### derived_type =", derived_type)

        # choose a proper ctype
        ctype = selectcases.get(ctype, ctype)

        # get the object as a ctype instance
        entity = ctype.in_dll(self, fullname_symbol)
        print("### getValue: full symbol =", fullname_symbol, addressof(entity), flush=True)
        print("### getValue: nvar =", c_int.from_address(addressof(entity)), flush=True)
        for i in range(230):
            print("### getValue: nvarrr =", i*4, c_int.from_address(addressof(entity)+i*4), flush=True)
        # YL 23/023/2024: alternatively, the value can be retrieved by `c_double.from_address(addr)` given addr from `loc(entity)` in Fortran

        # ctype is defaulted to c_int if not specified which is enough for getting the address of a derived-type instance
        if derived_type in self._derived_types.get(module, {}):
            entity = DL_DT(addressof(entity), self._derived_types[module][derived_type], self._derived_types[module], debug=debug)
            print("### entity =", entity)
            for attr in lbuff[1:]:
                print("### attr =", attr)
                if '(' in attr and ')' in attr:
                    name = attr.split('(')[0].strip()
                    index = attr.split('(')[1].split(')')[0]
                    index = eval(f'({index},)')
                    # use the transpose because Python is row-major
                    index = tuple(i-1 for i in index)[::-1]
                    entity = getattr(entity, name)[index]
                else:
                    entity = getattr(entity, attr)
            else:
                return entity

            # we cannot return entity.value as an instance of <class 'DL_DT'>
            return entity

        if return_ctype:
            return entity
        else:
            return entity.value

    
    def setValue(self, symbol, value, module='', debug=False):
        '''Set value to a symbol'''

        symbol = self.getSymbolOfModule(symbol, module)
    
        entity = self.getValue(symbol, c_double, return_ctype=True, debug=debug)

        setters.get(type(entity), self.setInt)(entity, value)


    @staticmethod
    def setInt(entity, value):
        '''Rewrite the c_int value in place'''

        # represent the value to set as 8 bytes of hex numbers but interpreted as short integers (0-255)
        lbuff = utils.ctypeutils.int2intarray(value)

        # YL 23/03/2024: `addressof(entity)` is same as Fortran's `loc(entity)` but not same as Python's `id(entity)`!
        addr = addressof(entity)

        # write all the 8 bytes by offsetting the address each time
        for i in range(4):
            memset(addr+i, lbuff[i], 1)


    @staticmethod
    def setLong(entity, value):
        '''Rewrite the c_long value in place'''


    @staticmethod
    def setChar(addr, length, value):
        '''Rewrite the c_long value in place'''

        try:
            value = value.encode()
        except:
            pass

        for i in range(length):
            if i < len(value):
                # it's integer representation already by looping over bytes
                memset(addr+i, value[i], 1)
            else:
                # white space
                memset(addr+i, 32, 1)


    @staticmethod
    def setBool(entity, value):
        '''Rewrite the c_bool value in place'''

        # represent the value to set as 8 bytes of hex numbers but interpreted as short integers (0-255)
        lbuff = utils.ctypeutils.bool2intarray(value)

        # YL 23/03/2024: `addressof(entity)` is same as Fortran's `loc(entity)` but not same as Python's `id(entity)`!
        addr = addressof(entity)

        # in Python or C Boolean values are in 1 byte
        memset(addr, lbuff[0], 1)
        # but in Fortran it's 4 bytes by default, we hence must overwrite other 3 bytes
        for i in range(1,4):
            memset(addr+i, 0, 1)


    @staticmethod
    def setFloat(entity, value):
        ''''''

        print("### setFloat")


    @staticmethod
    def setDouble(entity, value):

        # represent the value to set as 8 bytes of hex numbers but interpreted as short integers (0-255)
        lbuff = utils.ctypeutils.double2intarray(value)

        # YL 23/03/2024: `addressof(entity)` is same as Fortran's `loc(entity)` but not same as Python's `id(entity)`!
        addr = addressof(entity)

        # write all the 8 bytes by offsetting the address each time
        for i in range(8):
            memset(addr+i, lbuff[i], 1)


    def parseAllMods(self, moddir, recursive=True, padding=True, debug=False):
        '''(Optionally recursively) parse all module files under a directory'''

        from os import path, sep, walk

        if recursive:
            for dir_full, subdirs, filenames in walk(moddir):
                for filename in filenames:
                    if debug:
                        print(f" >>> Parsing {_u_W_}{path.join(dir_full, filename)}{CLR_}")
                    self.parseMod(path.join(dir_full, filename), debug=debug)
        # TODO
        else:
            pass


    def parseMod(self, modpath, padding=True, debug=False):
        ''''''

        import gzip
        from time import time
        from math import prod
        from os   import path

        fortran_internals = [ '__vtab_', '__vtype_', '__def_init_', '__copy_', '__final_', '__convert' ]

        # the filename seems to be the only identity of the current module
        basename = utils.fileutils.getBaseName(path.basename(modpath))

        t0 = time()
        def _printSummary(_dbuff):
            print('\n '+'*'*72)
            print(f" {_bW}Summary of module{CLR_} '{_bB}{dbuff['_name']}{CLR_}'")
            print(' '+'*'*72)
            print(f" File path: {_u_W_}{path.abspath(modpath)}{CLR_}")
            print(f" {dbuff['_title']}")
            for _k, _v in _dbuff.items():
                if _k == '_title':
                    continue
                if type(_v) is dict:
                    print(f"\n Entry '{_bC}{_k}{CLR_}':")
                    len_keys = max([len(_) for _ in _v.keys()])
                    fmt_keys = f'     {_bY}' + '{_kk:<' + f'{len_keys}' + '}' + f'{CLR_}'
                    for _kk, _vv in _v.items():
                        if _kk.startswith('_'):
                            if type(_vv) is int:
                                fmt_vv = f': {_M}'+'{_vv}'+f'{CLR_}'
                            elif type(_vv) is bool:
                                fmt_vv = f': {_G}'+'{_vv}'+f'{CLR_}'
                            elif type(_vv) is str:
                                fmt_vv = f': {_C}'+"'{_vv}'"+f'{CLR_}'
                            elif isinstance(_SimpleCData, type(_vv)):
                                fmt_vv = f': {_R}'+'{_vv}'+f'{CLR_}'
                            else:
                                fmt_vv = ': {_vv}'
                            print(f'{fmt_keys+fmt_vv}'.format(_kk=_kk, _vv=_vv))
                    fmt_keys += ': {_vv}'
                    for _kk, _vv in _v.items():
                        if not _kk.startswith('_'):
                            if type(_vv) is dict:
                                print(f'{fmt_keys}'.format(_kk=_kk, _vv=''), end='')
                                for _iii, (_kkk,_vvv) in enumerate(_vv.items()):
                                    if _iii:
                                        print(' '*(len_keys+7), end='')
                                    if type(_vvv) is int:
                                        if _kkk == '_offset':
                                            print(f'''{_bC}{"'"+_kkk+"'":17s}{CLR_}: {_bM}{_vvv}{CLR_}''')
                                        else:
                                            print(f'''{_C}{"'"+_kkk+"'":17s}{CLR_}: {_M}{_vvv}{CLR_}''')
                                    elif type(_vvv) is bool:
                                        print(f'''{_C}{"'"+_kkk+"'":17s}{CLR_}: {_G}{_vvv}{CLR_}''')
                                    elif type(_vvv) is str:
                                        print(f'''{_C}{"'"+_kkk+"'":17s}{CLR_}: {_bY}{_vvv}{CLR_}''')
                                    elif type(_vvv) is tuple:
                                        print(f'''{_C}{"'"+_kkk+"'":17s}{CLR_}: {_vvv}''')
                                    elif isinstance(_SimpleCData, type(_vvv)):
                                        print(f'''{_C}{"'"+_kkk+"'":17s}{CLR_}: {_G}{_vvv}{CLR_}''')
                                    else:
                                        print(f'''{_C}{"'"+_kkk+"'":17s}{CLR_}: {_R}{_vvv}{CLR_}''')
                            else:
                                print(f'{fmt_keys}'.format(_kk=_kk, _vv=_vv))
                else:
                    print(f'\n Entry "{_k}": {_v}')
            print(f'\n >>> Total time used: {time()-t0} s\n')

        # result dict to return
        dbuff = {'_name':basename} # module-file-based dict, however we don't know which module from the declarations (unless we use the filename)
        
        # mapping between indices and entries
        indices2entries = {}
        # a temporary container for updating self._instances_derived_types later
        _instances_derived_types = {}

        try:
            fp = gzip.open(modpath)
            fp.read()
        except:

            intel.parseModIntel(modpath)
            return

        # open the Fortran module file as gzipped plain text and pretty-print the content in a local file
        with gzip.open(modpath) as mod, open(f'_dl_py2f_{basename}.txt', 'w') as fp:
            sbuff = ' '.join(mod.read().decode().replace('()', '').replace('\'\'', '').split())

            fp.write('DL_PY2F\nLegend to hierarchies:\n')
            fp.write('{[(⟨⟪⟫⟩)]}\n\n')

            is_level0  = True
            is_level1  = False
            is_level2  = False
            is_level3  = False
            is_level4  = False
            is_level5  = False
            is_type    = False
            is_padded  = False
            is_derived = False
            is_proc    = False
            is_var     = False
            is_intrin  = False
            is_done    = True
            depth  = 0
            offset = 0
            sbuff0  = ''
            sbuff1  = ''
            sbuff2  = ''
            sbuff3  = ''
            sbuff4  = ''
            sbuff5  = ''
            declare = ''
            sizes = []
            types2ids   = {}
            ids2types   = {}
            types2sizes = {'_tb_procedure':0}

            # loop character by character
            for ic, c in enumerate(sbuff):

                # here we only mark the current depth (level) of nested parenthese
                if c == '(':
                    depth += 1
                elif c == ')':
                    depth -= 1

                # we start from opening of the first parenthesis
                if c == '(':
                    if depth == 1:
                        if sbuff0:
                            dbuff.update({'_title':sbuff0})
                        is_level0 = False
                        is_level1 = True
                        fp.write('\n{')
                    elif depth == 2:
                        is_level1 = False
                        is_level2 = True
                        # save this name now which doesn't have to be the current module's name (see below)
                        try:
                            modname = sbuff1.replace('(','').replace(')','').strip().split()[1].strip("'")
                        except:
                            modname = ''
                        # there seems no way to know which the actual name of a module is (unless using the filename)
                        # so that we could build up a dict of mapping between entry and modname across multiple
                        # module files
                        try:
                            lsbuff1 = sbuff1.replace('(','').replace(')','').strip().split()
                            index = int(lsbuff1[0])
                            entry = lsbuff1[1].strip("'")
                            if len(lsbuff1) == 4:
                                module = lsbuff1[2].strip("'")
                            # dummy arguments don't have a module
                            elif len(lsbuff1) == 3:
                                module = ''
                                is_var = True
                        except:
                            # do not redefine the entry name before the entry is finished even if the it is incomprehensible
                            if is_done:
                                entry = sbuff1.replace('(','').replace(')','').strip()
                                sbuff1 = ''
                                module = ''

                        # exclude Fortran internal entries
                        is_internal = any([entry.startswith(x) for x in fortran_internals])

                        # in a module file real declarations of derived types are in title case; those
                        # with repeated name in all lower case are differed with indices
                        if entry.islower():
                            # do not rename unless it's finished
                            if entry in dbuff and is_done:
                                entry += f'_{index}'
                        # .istitle() shoudn't be used here which takes 'Aaa_bbb' as False
                        else:
                            entry = entry.lower()
                        # create a dict
                        if entry:
                            if is_done:
                                # we do not put Fortran internal items in the result dict
                                if is_internal:
                                    dict_entry = {}
                                else:
                                    dbuff.update({entry.lower():{'_index':index}})
                                    dict_entry = dbuff[entry.lower()]
                                if module:
                                    if module not in self.__modules:
                                        self.__modules += [ module ]
                                    dict_entry.update({'_module':module})
                            indices2entries.update({index:entry.lower()})
                            size = 0
                            # 4 or 8
                            stride = 0
                            sizes = []
                            procedures = {}
                            fp.write('\n    [')
                            if is_intrin:
                                is_type = True
                                # reset type description because it's another Level-2 block
                                declare = ''
                        # this is a block of declarations of all types explicited imported or defined
                        # in the current module
                        # NB: but implicitly used types are not delared here but in the next main block
                        else:
                            is_type = True
                            fp.write('\n    [')
                            continue
                    elif depth == 3:
                        is_level2 = False
                        is_level3 = True
                        # variable
                        fp.write('\n        (')
                    elif depth == 4:
                        is_level3 = False
                        # leave a mark for derived types
                        if 'DERIVED' in sbuff3:
                            dict_entry.update({'_is_derived':True})
                            # complement the mapping between types and IDs with implicitly used derived types
                            if index not in ids2types and not is_internal:
                                ids2types.update({index:entry})
                                types2ids.update({entry:index})
                            is_proc = False
                        elif 'PROCEDURE' in sbuff3:
                            is_proc = True
                        # do not overwrite variable entrie's description
                        else:
                            dict_entry.update({'_descr':sbuff3.replace('(','').replace(')','').strip()})
                        is_level4 = True
                        fp.write('\n            ⟨')
                        # do NOT remove this `continue`!
                        continue
                    # level -5 is where we add the actual data types
                    elif depth == 5:
                        # finished the current component's name
                        is_level4 = False
                        is_level5 = True
                        try:
                            name = sbuff4.split()[1].strip("'")
                            idx  = int(sbuff4.split()[0])
                        except:
                            name = sbuff4
                            idx  = None
                        # vaiables have no member
                        if is_var:
                            sbuff4 = ''
                        if sbuff4.strip():
                            dict_entry.update({name:{'_index':idx}})
                            if entry and not is_internal:
                                dbuff.update({entry:dict_entry})
                            name_member = name
                            dict_member = dict_entry[name_member]
                        sbuff4 = ''
                        fp.write('\n                ⟪')
                    else:
                        fp.write('\n                    <')

                # when the current parenthese is closed
                elif c == ')':
                    if depth == 0:
                        is_level1 = False
                        fp.write('\n}')
                    # Level 2 returning to Level 1
                    elif depth == 1:
                        # a Level-2 block of intrinsic procedure specifier followed by another
                        # Level-2 block of type that we cannot interrupt the current entry
                        if sbuff2.strip('()') and not module and entry.strip('()'):
                            if 'intrinsic' in sbuff2.strip('()'):
                                is_intrin = True
                                # do not start a new entry because the current one is not finished yet
                                is_done   = False
                                dict_entry.update({'_is_intrinsic':True})
                        if declare:
                            if is_intrin:
                                dict_entry.update({'_descr':'('+declare.strip('()')})
                                if 'PROCEDURE' in declare.strip('()').split(')')[0].split():
                                    is_proc = True
                                    # we've got what's needed for the current entry
                                    is_done = True
                                    dict_entry.update({'_is_proc':True})
                            else:
                                # type declarations
                                types2ids.update({declare.split()[0].strip("'"):int(declare.split()[2])})
                                ids2types.update({int(declare.split()[2]):declare.split()[0].strip("'")})
                        # reset Fortran module entry by returning to level -1
                        if entry:
                            # NB: these sizes and types2sizes contain padding which cannot be finally determined now!
                            types2sizes.update({entry:sum(sizes)})
                            if 'MODULE' in sbuff3:
                                dict_entry.update({'_is_module':True})
                        else:
                            is_type    = False
                        # 
                        sbuff1  = ''
                        # these need to be reset, too, for every new entry
                        sbuff3  = ''
                        sbuff2  = ''
                        declare = ''
                        procedures = {}
                        offset = 0
                        is_level1  = True
                        is_derived = False
                        is_var     = False
                        # update dict if there is one
                        try:
                            if is_done and entry and not is_internal:
                                dbuff.update({entry:dict_entry})
                        except:
                            pass
                        fp.write('\n    ]')
                    # level 3 returning to 2
                    elif depth == 2:
                            
                        is_lelve3 = False
                        # exclude derived-type entries from variables otherwise there're too many troubles
                        dt = sbuff3.replace('(','').replace(')','').split()
                        if dt[0] == 'DERIVED':
                           is_derived = True
                        if not is_derived:
                            # this is in fact alreay the 2nd Level-3 block about the variable
                            if is_var:
                                dt = sbuff3.replace('(','').replace(')','').split()
                                # we take only sensible type name because it could be array descriptor
                                if dt[0]+dt[1] in selectcases:
                                    dict_variable.update({'_type': selectcases[dt[0]+dt[1]]})
                            # variable and procedure
                            descr_var = sbuff3.split('(')[-1]
                            ldescr = descr_var.replace('(','').replace(')','').split()
                            if 'VARIABLE' in ldescr or '(VARIABLE' in ldescr:
                                is_var = True
                                dict_variable = {'_is_var': True}
                                # update the dict before sbuff3 is reset
                                dict_variable.update({'_descr': ' '.join(ldescr)})
                                if 'DUMMY' in ldescr:
                                    dict_variable.update({'_is_dummy': True})
                                if 'POINTER' in ldescr:
                                    dict_variable.update({'_is_pointer': True})
                                if 'IN' in ldescr:
                                    dict_variable.update({'_intent': 'in'})
                                elif 'OUT' in ldescr:
                                    dict_variable.update({'_intent': 'out'})
                                elif 'INOUT' in ldescr:
                                    dict_variable.update({'_intent': 'inout'})
                            if 'PROCEDURE' in ldescr:
                                is_proc = True
                                dict_procedure = {'_is_proc': True}
                                # update the dict before sbuff3 is reset
                                dict_procedure.update({'_descr': ' '.join(ldescr)})
                            # description and data type of a variable are defined in two level-3 blocks therefore
                            # we need to reset the first sbuff3 and use the 2nd block to determine data type
                            # NB: however, do NOT reset sbuff3 elsewhere!
                            if is_var:
                                sbuff3 = ''
                                # a variable has no member
                                name_member = ''
                            
                        fp.write('\n        )')
                        # valuable information could be found here like instance declarations!
                        try:
                            index_derived_type = int(sbuff3.partition('DERIVED')[2].split()[0])
                            # beware that index_derived_type may not have been added to <dict 'indices2entries'>
                            # hence we need to recheck at the bottom
                            tmp = indices2entries.get(index_derived_type, index_derived_type)
                            if not entry.startswith('__vtab'):
                                _instances_derived_types.update({entry:tmp})
                        except:
                            pass
                    # level 4 returning to 3
                    elif depth == 3:
                        # at -3 level we find the data type declarations
                        if sbuff5.strip():
                            # reset some parameters
                            is_padded = False
                            is_deferred_char = False
                            ndims = 0
                            dim = ()
                            # section 1 (mandatory): 1st value indicates the byte size
                            dt = sbuff5.split('(')[1].split(')')[0].split()

                            # TODO here are some ugly patched for handling uncommon situations that we don't use now
                            if len(dt) == 1:
                                # for a type-bound procedure it'd look, for example "PUBLIC OVERRIDABLE PASS GENERIC NO_PPC 0"
                                if 'OVERRIDABLE' in sbuff5.split('(')[2].split():
                                    name_member = dt[0].strip("'")
                                    ibuff = 0
                                    is_proc = True
                                    procedures.update({name_member:0})
                                ldescr = sbuff3.split('(')
                                if 'DERIVED' not in sbuff3:
                                    if any(['PROCEDURE' in x.split() for x in ldescr]):
                                        is_proc = True
                                    dt = ldescr[-1].replace('(','').replace(')','').split()
                                    ibuff = int(dt[1])
                                    
                                    
                            else:
                                ibuff = int(dt[1])
                                is_proc = False

                            # use this value for the total offset (in bytes)
                            size, size_derived = ibuff, size
                            # we cannot know the size of a derived-type instance until we have gone through
                            # all types definitions but we assume the size to be 8 because we need padding
                            if 'DERIVED' in dt[0]:
                                size = 8
                                is_derived = True
                                is_char = False
                            elif 'CHARACTER' in dt[0]:
                                is_char = True
                                is_derived = False
                                stride = 1
                                try:
                                    size = int(sbuff5.split(') (')[0].split('((')[1].split('))')[0].split()[-1].strip("'"))
                                    is_deferred_char = False
                                except:
                                    if 'DEFERRED_CL' in sbuff5.split(') (')[0].split():
                                        size = 1
                                        is_deferred_char = True
                            else:
                                is_derived = False
                                is_char = False
                            # record the maximum bitesize for aligning (padding)
                            if not is_char:
                                stride = size
                                # keep this record for the current derived type because we need deferred padding afterwards
                            # check stride > 0 because no padding for the first member
                            # no padding before character of explicit length
                            if padding and stride and size and not is_char:
                                # we add a padding memory if the total size cannot be natually asigned as the
                                # compilers behave by default (unless compiled with argument preventing memory padding)
                                residue = sum(sizes)%stride
                                if residue and size > residue:
                                    if debug and not is_internal:
                                        print(f" >>> {_bW}Padding{CLR_} {_bC}{entry}.{CLR_}{_bY}{name_member}{CLR_} by {_bM}{stride-residue}{CLR_} bytes:")
                                        print(f" >>>     Size: {_bM}{size}{CLR_}, Stride: {_bM}{stride}{CLR_}, Residue: {_bM}{residue}{CLR_}")
                                    # edit the most recent size so that the future padding will be correct
                                    pad = stride - residue
                                    sizes[-1] += pad
                                    is_padded = True

                            # section 1 (mandatory): 2nd is the Fortran type (one of INTEGER, REAL, LOGICAL, CHARACTER, COMPLEX, DERIVED)
                            # TODO: type-bound procedure
                            if is_proc:
                                dt = '_tb_procedure'

                            elif is_var:
                                dt = sbuff3.replace('(','').replace(')','').split()
                                dt = selectcases.get(dt[0]+dt[1], int(dt[1]))
                            else:
                                dt = selectcases.get(dt[0]+dt[1], int(dt[1]))
                            dt_dict = {'_type':dt}
                            dt_dict.update({'_stride':stride})
                            dt_dict.update({'_is_derived':is_derived})
                            dt_dict.update({'_is_padded':is_padded})
                            if is_padded:
                                dt_dict.update({'_padded':pad})

                            # section 2 (optional): array descriptor
                            #                       as of GFORTRAN module version '15', for example, for an array `integer :: a(8,9,10)`
                            #                       the descriptor would have such sturcture -
                            #                       ( 3 0 EXPLICIT (CONSTANT (INTEGER 4 0 0 0 INTEGER ) 0 '1' )
                            #                                      (CONSTANT (INTEGER 4 0 0 0 INTEGER ) 0 '8' )
                            #                                      (CONSTANT (INTEGER 4 0 0 0 INTEGER ) 0 '1' )
                            #                                      (CONSTANT (INTEGER 4 0 0 0 INTEGER ) 0 '9' )
                            #                                      (CONSTANT (INTEGER 4 0 0 0 INTEGER ) 0 '1' )
                            #                                      (CONSTANT (INTEGER 4 0 0 0 INTEGER ) 0 '10')
                            #                       )
                            try:
                                adesc = sbuff5.split(') (')[1].split()     # use .split() at the end to match whole word because there could be "ALWAYS_EXPLICIT"
                            # unused case
                            except:
                                adesc = []
                            # declared shape
                            if 'EXPLICIT' in adesc:
                                ndims = int(adesc[0])
                                adesc = sbuff5.split(')')[1:]
                                # keep the index within ndims because it'll look different when there is array initialiser
                                if is_char:
                                    adesc = sbuff5.split(') (')[1:]
                                    dim = tuple(int(x.strip(')').split()[-1].strip("'")) for (i,x) in enumerate(adesc[1::2]) if i < ndims)
                                else:
                                    dim = tuple(int(x.split()[1].strip("'")) for (i,x) in enumerate(adesc[3::4]) if i < ndims)

                                try:
                                    offset += sizes[-1]
                                # in case that 1st component is derived type
                                except IndexError:
                                    offset += size_derived
                                sizes += [size*prod(dim)]
                                dt_dict.update({'_ndims'      : ndims,
                                                '_is_deferred': False,
                                                '_dim'        : dim,
                                                '_offset'     : offset,
                                               })
                                if is_char:
                                    dt_dict.update({'_is_char': True,
                                                    '_is_deferred' : False,
                                                    '_length'      : size,
                                                   })

                            # deferred shape
                            elif 'DEFERRED' in adesc:
                                stride = 8
                                dt_dict.update({'_stride':stride})
                                ndims = int(adesc[0])
                                adesc = sbuff5.split(')')
                                #
                                if is_char:
                                    if is_deferred_char:
                                        dt_dict.update({'_is_deferred_char': True})
                                        # must recalculate the padding because above size was 1 and no padding was triggered
                                        residue = sum(sizes)%stride
                                        if padding and residue:
                                            if debug:
                                                print(f" >>> {_bW}Padding{CLR_} {_bC}{entry}.{CLR_}{_bY}{name_member}{CLR_} by {_bM}{stride-residue}{CLR_}:")
                                            # edit the most recent size so that the future padding will be correct
                                            sizes[-1] += stride - residue
                                            dt_dict.update({'_padded':stride-residue})
                                            is_padded = True
                                        else:
                                            is_padded = False
                                        dt_dict.update({'_is_padded':is_padded})
                                        # offset must be calculated with the padded size of the previous member
                                    offset += sizes[-1]
                                else:
                                    try:
                                        offset += sizes[-1]
                                    except IndexError:
                                        offset += size_derived
                                if 'DIMENSION POINTER' in adesc[2]:
                                    dt_dict.update({'_ndims'      : ndims,
                                                    '_is_deferred': True,
                                                    '_offset'     : offset,
                                                    'is_pointer'  : True})
                                elif 'ALLOCATABLE DIMENSION' in adesc[2]:
                                    dt_dict.update({'_ndims'      : ndims,
                                                    '_is_deferred': True,
                                                    '_offset'     : offset,
                                                    'is_pointer'  : False})
                                else:
                                    dt_dict.update({'_ndims'      : ndims,
                                                    '_offset'     : offset,
                                                    '_is_deferred': True})
                                # the size of array descriptor
                                sizes += [40+24*ndims]
                            # scalar
                            else:
                                if is_char:
                                    dt_dict.update({'_is_char'         : True,
                                                    '_length'          : size,
                                                    '_is_deferred_char': is_deferred_char,
                                                   })
                                    # character of deferred length
                                    if is_deferred_char:
                                        # this is a fixed number
                                        stride = 8
                                        dt_dict.update({'_stride':stride})
                                        # must recalculate the padding
                                        residue = sum(sizes)%stride
                                        if padding and residue:
                                            if debug:
                                                print(f" >>> {_bW}Padding{CLR_} {_bC}{entry}.{CLR_}{_bY}{name_member}{CLR_} by {_bM}{stride-residue}{CLR_}:")
                                            # edit the most recent size so that the future padding will be correct
                                            sizes[-1] += stride - residue
                                            is_padded = True
                                            dt_dict.update({'_is_padded':is_padded})
                                            dt_dict.update({'_padded':stride-residue})
                                        # offset must be calculated with the padded size of the previous member
                                        offset += sizes[-1]
                                        # attach the current size always in the end
                                        sizes += [8]
                                    else:
                                        # members after the first one
                                        try:
                                            offset += sizes[-1]
                                        # first member
                                        except:
                                            pass
                                        sizes += [size]
                                # TODO
                                elif is_proc:
                                    pass
                                else:
                                    try:
                                        if 'POINTER' in sbuff5.split(') (')[1].split(')')[0].split():
                                            dt_dict.update({'_is_pointer':True})
                                            # this is a fixed number
                                            size = 8
                                            stride = 8
                                            dt_dict.update({'_stride':stride})
                                            # must recalculate the padding
                                            residue = sum(sizes)%stride
                                            if padding and residue:
                                                if debug:
                                                    print(f" >>> {_bW}Padding{CLR_} {_bY}{entry}.{CLR_}{_bR}{name_member}{CLR_} by {_bM}{stride-residue}{CLR_}:")
                                                # edit the most recent size so that the future padding will be correct
                                                sizes[-1] += stride - residue
                                                is_padded = True
                                                dt_dict.update({'_is_padded':is_padded})
                                                dt_dict.update({'_padded':stride-residue})
                                    except:
                                        pass
                                    try:
                                        offset += sizes[-1]
                                    # 1st component of the current derived type
                                    except IndexError:
                                        offset += size_derived
                                    # in the end attach the current size
                                    sizes += [size]
                                if debug and not is_internal:
                                    print(f" >>> {_bC}{entry}.{CLR_}{_bY}{name_member}{CLR_} offset: {_bM}{offset}{CLR_}")
                                dt_dict.update({'_offset': offset})

                            # update the dict data
                            if name_member:
                                dict_member.update(dt_dict)
                                dict_entry.update({name_member:dict_member})
                            # type-bound procedures
                            if dict_entry.get('_is_derived', False):
                                dict_entry.update({'_tb_procedures':procedures})
                            
                            if is_var:
                                dbuff.update({entry:dict_variable})
                            elif is_proc:
                                dbuff.update({entry:dict_procedure})

                        fp.write('\n            ⟩')
                        # reset data type contents at the end of level -4
                        is_level5 = False
#                        if padding and is_padded:
#                            fp.write(f' Padded before "{name_member}" by {size-residue} bytes')
                        name_member = ''
                        sbuff5 = ''
                        sbuff4 = ''
                        dict_member = {}
                    elif depth == 4:
                        fp.write('\n                ⟫')
                    else:
                        fp.write('\n                    >')
                else:
                    fp.write(c)

                # complete the string buffers
                if is_level0:
                    sbuff0 += c
                if is_level1:
                    sbuff1 += c
                if is_level2:
                    sbuff2 += c
                if is_level3:
                    sbuff3 += c
                if is_level4:
                    sbuff4 += c
                if is_level5:
                    sbuff5 += c
                if is_type:
                    declare += c

        print("\n### modpath =", modpath)
        # complete deferred declarations
        for k, v in _instances_derived_types.items():
            print("### k, v =", k, v)
            if is_internal:
                continue
            if type(v) is int:
                tmp = indices2entries.get(v, None)
                if tmp:
                    _instances_derived_types.update({k:tmp})
        self._instances_derived_types.update({basename:_instances_derived_types})

        # save the information of derived types in the dict to return
        derived_types = {}
        for k, v in dbuff.items():
            if type(v) is dict:
                if v.get('_is_derived', False):
                    derived_types.update({k:v})
        self._derived_types.update({basename:derived_types})


        def _findDerivedTypes(self, _type):
            ''''''

            _size_type = types2sizes[_type]

            # skip GNU built-in
            for x in fortran_internals:
                if entry.startswith(x):
                    return 0

            # type-bound procedures
            if _type == '_tb_procedure':
                return 0

            for _k, _dict_subtype in dbuff[_type].items():
                if _k == '_tb_procedures':
                    continue
                if type(_dict_subtype) is dict:
                    # it's possible that the integer ID has been replaced by str name
                    _subtype = {int:ids2types.get(_dict_subtype['_type'], ''), str:_dict_subtype['_type']}.get(type(_dict_subtype['_type']), '')
                    if _subtype:
                        # replace index with type name
                        _dict_subtype['_type'] = _subtype
                        if '_dim' in _dict_subtype:
                            # NB: this size_array is that of _subtype rather than _type!
                            size_array = prod(_dict_subtype['_dim'])
                            ndims = _dict_subtype['_ndims']
                        else:
                            size_array = 1
                            ndims = 1
                        if debug:
                            print(f' >>>     {_R}Old size{CLR_} of type {_bC}{_type}{CLR_}:', _size_type)
                        # skip linked table
                        if _dict_subtype.get('_is_pointer', False) and not '_ndims' in _dict_subtype and _subtype == _type:
                            _size_subtype = 8
                        else:
                            # NB: _size_type is that of _type but not _subtype!
                            _size_subtype = _findDerivedTypes(self, _subtype)
                            _dict_subtype.update({'_size_chunk':_size_subtype})

                        # arrays of a deferred sizes were already taken care of
                        if _dict_subtype.get('_is_deferred', False):
                            size_array = 0
                        if _dict_subtype.get('_is_pointer', False) and not '_ndims' in _dict_subtype:
                            _size_subtype = 8
                            size_array = 0

                        # size previously assumed 8*size_array
                        _size_type += (_size_subtype-8)*size_array
                        if debug:
                            print(f' >>>     {_G}New size{CLR_} of type {_bC}{_type}{CLR_}:', _size_type)

            # pad the current derived type
            residue = _size_type%8
            if padding and residue and 8 > residue:
                _size_type += 8 - residue
                if debug:
                    print(f" >>>     Padded {_bC}{_type}{CLR_} by {8-residue} bytes to final size {_bM}{_size_type}{CLR_}")

            dbuff[_type].update({'_size_chunk':_size_type})
            return _size_type


        for k, v in dbuff.items():
            if type(v) is dict:
                size_derived = 0
                # this is the accumulated extra size
                accumulation = 0
                was_derived = False
                if v.get('_is_derived', False) and not k.startswith('__'):
                    for kk, vv in v.items():
                        if kk == '_tb_procedures':
                            continue
                        if type(vv) is dict:
                            tp = ids2types.get(vv['_type'], '')
                            if debug:
                                print(f' >>> {_R}Old offset{CLR_} of {_C}{k}{CLR_}.{_Y}{kk}{CLR_}: {_M}{vv["_offset"]}{CLR_}', f'(stride = {vv["_stride"]}, accumulation = {accumulation})')
                            # re-evaluate padding
                            offset_assumed = vv['_offset'] + accumulation - vv.get('_padded', 0)
                            if vv['_type'] == '_tb_procedure':
                                residue = 0
                            else:
                                # use a 0 here to check if 
                                residue = (offset_assumed)%vv.get('_stride',0)
                            if padding and residue:
                                if debug:
                                    print(f' >>> {_bM}Repadded{CLR_} {_bY}"{kk}"{CLR_} by 4 because {size_derived}+{vv["_offset"]} has residue {residue}')
                                    print(f' >>> otherwise offset would be {offset_assumed} (stride = {vv["_stride"]})')
                                pad = vv.get('_stride',8) - residue
                                vv['_offset'] += accumulation - vv.get('_padded', 0) + pad
                                accumulation += - vv.get('_padded', 0) + pad
                                if pad:
                                    vv.update({'_is_padded':True})
                                    vv.update({'_padded':pad})
                                else:
                                    vv.update({'_is_padded':False})
                                    vv.pop('padded', 0)
                            # remove it if the previous padding is no longer needed
                            else:
                                vv['_offset'] += accumulation - vv.get('_padded', 0)
                                accumulation -= vv.get('_padded', 0)
                                vv.update({'_is_padded':False})
                                vv.pop('padded', 0)
                            if debug:
                                print(f' >>> {_bG}New offset{CLR_} of {_bC}{k}{CLR_}.{_bY}{kk}{CLR_}: {_bM}{vv["_offset"]}{CLR_}', f'(accumulation = {accumulation})')
                            if tp and kk != '_def_init':
                                # replace index with type name which is deferred to determine
                                vv['_type'] = tp
                                # when the instance is an array of explicit shape
                                if '_dim' in vv:
                                    size_array = prod(vv['_dim'])
                                    ndims = vv['_ndims']
                                else:
                                    size_array = 1
                                    ndims = 1
                                # size previously assumed 8*size_array and _findDerivedTypes() returns a re-calculated size
                                size_old = types2sizes[tp]
                                # if this is a linked-table pointing to itself
                                if k == tp:
                                    size_new = size_old
                                else:
                                    size_new = _findDerivedTypes(self, tp)
                                    vv.update({'_size_chunk':size_new})
                                            
                                size_derived = size_new*size_array

                                # arrays of a deferred sizes were already taken care of
                                if vv.get('_is_deferred', False):
                                    ndims = vv['_ndims']
                                    size_derived = 0
                                    size_array = 0

                                if vv.get('_is_pointer', False) and not '_ndims' in vv:
                                    size_derived = 0
                                    size_array = 0
                                # offset also previously assmued 8*size_array must be deducted
                                accumulation += size_derived
                                accumulation -= 8*size_array
                                if debug:
                                    print(f' >>> {_bG}New size{CLR_} of {_bY}{k}{CLR_}.{_bC}{tp}{CLR_}: {_bM}{size_new}{CLR_} (size_derived = {size_derived}, accumulation = {accumulation})\n')
                                was_derived = True
                            else:
                                size_derived = 0
                                was_derived = False

        if debug:
            _printSummary(dbuff)

        return dbuff


    @property
    def derived_types(self):
         '''Return a list of all derived types which are already parsed in the current library'''

         return list(self._derived_types.keys())





# mapping of methods of setting values
setters = {
            c_float : DL_DL.setFloat,
            c_double: DL_DL.setDouble,
            c_int   : DL_DL.setInt,
            c_long  : DL_DL.setLong,
            c_bool  : DL_DL.setBool,
            str     : DL_DL.setChar,
          }

