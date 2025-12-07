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
__email__ = 'you.lu@stfc.ac.uk'
__author__ = f'You Lu <{__email__}>'
import os, utils
from importlib.resources import files
from ctypes    import addressof, CDLL, memset, RTLD_GLOBAL, sizeof
from ctypes    import c_bool, c_char_p, c_double, c_float, c_int, c_long, c_void_p, POINTER
from ctypes    import c_byte, c_char, c_short, c_size_t, c_uint, c_ubyte, c_ulong, c_void_p, c_wchar
from _ctypes   import _SimpleCData
from numpy     import array, dtype, empty, full, ndindex
from math      import ceil, prod
from itertools import product
from inspect   import stack
from weakref   import WeakValueDictionary
from time      import time
from .         import libpath
from .utils    import colours as _clr
_accessor_cache = {}
_DL_CACHE = WeakValueDictionary()
selectcases = { float      : c_double,
                c_double   : c_double,
                c_float    : c_float,
                int        : c_int,
                c_int      : c_int,
                c_long     : c_long,
                str        : c_char_p,
               'CHARACTER1': c_char_p,
               'INTEGER4'  : c_int,
               'INTEGER8'  : c_long,
               'REAL4'     : c_float,
               'REAL8'     : c_double,
               'LOGICAL1'  : c_bool, # TODO logical of other lengths
               'LOGICAL4'  : c_bool, # TODO logical of other lengths
               'LOGICAL8'  : c_bool, # TODO logical of other lengths
              }
typestrs = { c_double : 'f8',
             c_float  : 'f4',
             c_long   : 'i8',
             c_int    : 'i4',
             c_bool   : 'b4',
             c_char_p : 'S'
           }
class _DL_DT_Scalar:
    __slots__ = ('_parent', '_meta')
    def __init__(self, parent, meta):
        self._parent = parent
        self._meta   = meta
    def instantiate(self):
        addr        = self._parent.__address__ + self._meta['_offset']
        typename    = self._meta['_typename']
        dict_dt     = self._parent._derived_types[typename]
        caller_base = (self._parent._caller + '%' + self._meta['_name'] if self._parent._caller else self._meta['_name'])
        inst = DL_DT(addr,
                     dict_dt,
                     self._parent._derived_types,
                     caller=caller_base,
                     caller_type=typename,
                     return_ctype=self._parent._return_ctype,
                     debug=self._parent._debug)
        setattr(inst, '_derived_type', typename)
        return inst
class _DL_DT_Pointer:
    __slots__ = ('_parent', '_meta')
    def __init__(self, parent, meta):
        self._parent = parent
        self._meta   = meta
    def instantiate(self):
        offset   = self._meta['_offset']
        typename = self._meta['_typename']
        dict_dt  = self._parent._derived_types[typename]
        addr = c_ulong.from_address(self._parent.__address__ + offset).value
        if addr == 0:
            return
        if self._parent.lib_dl_py2f.associated(c_ulong(addr)):
            return
        caller      = self._parent._caller
        name_member = self._meta['_name']
        caller_base = (caller + '%' + name_member) if caller else name_member
        is_linked_list = False
        if caller:
            base = caller.split('%')[-1].split('(')[0]
            is_linked_list = (base == name_member)
            if is_linked_list and self._parent._debug:
                print(f' >>> NB: {_clr._bE}{caller_base}{_clr.CLR_} is a {_clr._bW}scalar linked list!{_clr.CLR_}')
        if self._parent.lib_dl_py2f.associated(c_ulong(addr)):
            if self._parent._debug:
                print(f' >>> WARNING: {_clr._R}{caller_base}{_clr.CLR_} at {_clr._uW}{hex(addr)}{_clr.CLR_} is likely to be a wild pointer!')
            return
        inst = DL_DT(addr,
                    dict_dt,
                    self._parent._derived_types,
                    caller=caller_base,
                    caller_type=typename,
                    return_ctype=self._parent._return_ctype,
                    debug=self._parent._debug)
        setattr(inst, '_derived_type', typename)
        if getattr(inst, '_return_immediately', False):
            if is_linked_list:
                self._parent._return_immediately = True
            else:
                if self._parent._debug:
                    print(f' >>> WARNING: {_clr._R}{caller_base}{_clr.CLR_} is likely to be an unassociated pointer to a linked list! (type: {_clr._Y}{typename}{_clr.CLR_})')
            return inst
        return inst
class _DL_DT_Array:
    __slots__ = ('_parent', '_meta', '_shape', '_typename', '_array')
    def __init__(self, parent, meta):
        self._parent   = parent
        self._meta     = meta
        self._shape    = tuple(meta['_shape'][::-1])
        self._typename = meta['_typename']
        self._array = None
    def __getitem__(self, ind):
        if not isinstance(ind, tuple):
            ind = (ind,)
        dims = self._shape
        if len(ind) != len(dims)    \
        or any(isinstance(x, slice) \
               or x is Ellipsis     \
               or x is None         \
               for x in ind):
            return _DL_DT_ArrayView(self, ind)
        norm = []
        for i, dim in zip(ind, dims):
            if isinstance(i, int):
                if i < 0:
                    i = dim + i
                if i < 0 or i >= dim:
                    raise IndexError(f'Index {i} out of bounds for dimension size {dim}')
            norm.append(i)
        ind = tuple(norm)
        return self.__getitem_int__(ind)
    def __getitem_int__(self, ind):
        if not isinstance(ind, tuple):
            ind = (ind,)
        dims = self._shape
        if len(ind) != len(dims):
            raise IndexError(f'Index {ind} has {len(ind)} dims, but array has {len(dims)} dims')
        norm = []
        for i, dim in zip(ind, dims):
            if isinstance(i, int):
                if i < 0:
                    i = dim + i
                if i < 0 or i >= dim:
                    raise IndexError(f'Index {i} out of bounds for dimension size {dim}')
            norm.append(i)
        ind = tuple(norm)
        flat = ind[0]
        for i, d in zip(ind[1:], dims[1:]):
            flat = flat*d + i
        offset     = self._meta['_offset']
        size_chunk = self._meta['_size_chunk']
        if self._meta['_kind'] == 'array_deferred':
            base = c_ulong.from_address(self._parent.__address__ + offset).value
            if base == 0:
                raise RuntimeError(f'Deferred array "{self._meta["_name"]}" is not allocated (NULL pointer).')
        else:
            base = self._parent.__address__ + offset
        addr = base + flat*size_chunk
        dict_dt = self._parent._derived_types[self._typename]
        base_name = self._meta['_name']
        if self._parent._caller:
            caller_base = self._parent._caller + '%' + base_name
        else:
            caller_base = base_name
        if self._parent._debug:
            caller = caller_base + '(' + ','.join(str(i+1) for i in ind[::-1]) + ')'
        else:
            caller = caller_base
        inst = DL_DT(addr,
                    dict_dt,
                    self._parent._derived_types,
                    caller=caller,
                    caller_type=self._typename,
                    return_ctype=self._parent._return_ctype,
                    debug=self._parent._debug)
        setattr(inst, '_derived_type', self._typename)
        return inst
    def __iter__(self):
        for i in range(len(self)):
            yield self[i]
    def __len__(self):
        return self._shape[0] if self._shape[0] else 0
    @property
    def __array__(self):
        if self._array is not None:
            return self._array
        abuff = empty(self._shape, dtype=object)
        for ind in ndindex(*self._shape):
            abuff[ind] = self[ind]
        self._array = abuff
        return abuff
    @property
    def __shape__(self):
        return self._shape
    @property
    def size(self):
        n = 1
        for d in self._shape:
            n *= d
        return n
    @property
    def shape(self):
        return self._shape
    @property
    def ndim(self):
        return len(self._shape)
    @property
    def dtype(self):
        return dtype(object)
    @property
    def T(self):
        return self.__array__.T
class _DL_DT_ArrayView:
    __slots__ = ('_parent', '_meta', '_slices', '_shape', '_is_scalar')
    def __init__(self, parent, slices):
        self._parent = parent
        self._meta   = parent._meta
        self._slices = self._normalise_slices(slices, parent._shape)
        shape = []
        shape_parent = parent._shape
        i_parent = 0
        for slc in self._slices:
            if slc is None:
                shape.append(1)
            elif isinstance(slc, slice):
                dim = shape_parent[i_parent]
                start, stop, step = slc.indices(dim)
                if step == 0:
                    raise ValueError("Slice step cannot be zero")
                length = max(0, ceil((stop-start)/step))
                shape.append(length)
                i_parent += 1
            elif isinstance(slc, int):
                i_parent += 1
            else:
                raise TypeError(f"Unsupported index type {type(slc)}")
        self._shape = tuple(shape)
        if len(self._shape) == 0:
            self._is_scalar = True
        elif len(self._shape) == 1 and self._shape[0] == 1:
            self._is_scalar = True
        else:
            self._is_scalar = False
    def _normalise_slices(self, slices, shape):
        ndims_parent = len(shape)
        if Ellipsis in slices:
            ind   = slices.index(Ellipsis)
            left  = slices[:ind]
            right = slices[ind+1:]
            def _consumes_dim(s):
                return s is not None and s is not Ellipsis
            used = sum(1 for s in left+right if _consumes_dim(s))
            missing = ndims_parent - used
            slices = left + (slice(None),)*missing + right
        if len(slices) < len(shape):
            slices = slices + (slice(None),) * (len(shape)-len(slices))
        norm = []
        i_parent = 0
        for slc in slices:
            if slc is None:
                norm.append(None)
                continue
            if i_parent >= ndims_parent:
                raise IndexError("Too many indices for array")
            dim = shape[i_parent]
            if isinstance(slc, int):
                if slc < 0:
                    slc = dim + slc
                if slc < 0 or slc >= dim:
                    raise IndexError(f'Index {slc} out of bounds for dimension size {dim}')
                norm.append(slc)
                i_parent += 1
            elif isinstance(slc, slice):
                norm.append(slc)
                i_parent += 1
            else:
                raise TypeError(f"Unsupported index type {type(slc)}")
        while i_parent < ndims_parent:
            norm.append(slice(None))
            i_parent += 1
        return tuple(norm)
    def __getattr__(self, attr):
        if self._is_scalar:
            return self.__getitem_int__(0).__getattribute__(attr)
        return super().__getattribute__(attr)
    def __setattr__(self, attr, value):
        if attr in [ '_parent', '_meta', '_slices', '_shape' ,'_is_scalar' ]:
            return object.__setattr__(self, attr, value)
        if getattr(self, '_is_scalar', False):
            return self.__getitem_int__(0).__setattr__(attr, value)
        return object.__setattr__(self, attr, value)
    def __getitem__(self, ind):
        if isinstance(ind, tuple):
            norm = []
            for i, dim in zip(ind, self._shape):
                if isinstance(i, int) and i < 0:
                    i = dim + i
                if isinstance(i, int) and (i < 0 or i >= dim):
                    raise IndexError(f'Index {i} out of bounds for dimension size {dim}')
                norm.append(i)
            ind = tuple(norm)
            return _DL_DT_ArrayView(self, ind)
        if isinstance(ind, slice):
            return _DL_DT_ArrayView(self, (ind,))
        if isinstance(ind, int):
            if ind < 0:
                ind = self._shape[0] + ind
            if ind < 0 or ind >= self._shape[0]:
                raise IndexError(f'Index {ind} out of bounds for dimension size {self._shape[0]}')
        return self.__getitem_int__(ind)
    def __getitem_int__(self, ind):
        if isinstance(ind, int):
            if ind < 0:
                ind = self._shape[0] + ind
            if ind < 0 or ind >= self._shape[0]:
                raise IndexError(f'Index {ind} out of bounds for dimension size {self._shape[0]}')
        parent = self._parent
        shape_parent = parent._shape
        i_parent = 0
        indices = []
        used = False
        for slc in self._slices:
            if slc is None:
                continue
            dim = shape_parent[i_parent]
            if isinstance(slc, slice):
                start, stop, step = slc.indices(dim)
                if not used:
                    i = start + ind*step
                    used = True
                else:
                    i = start
                indices.append(i)
                i_parent += 1
            elif isinstance(slc, int):
                indices.append(slc)
                i_parent += 1
            else:
                raise TypeError(f"Unsupported slice type {type(slc)}")
        return parent.__getitem_int__(tuple(indices))
    def __iter__(self):
        for i in range(len(self)):
            yield self[i]
    def __len__(self):
        return self._shape[0] if self._shape[0] else 0
    @property
    def __array__(self):
        abuff = empty(self._shape, dtype=object)
        for ind in ndindex(*self._shape):
            abuff[ind] = self[ind]
        return abuff
    @property
    def __shape__(self):
        return self._shape
    @property
    def size(self):
        n = 1
        for d in self._shape:
            n *= d
        return n
    @property
    def shape(self):
        return self._shape
    @property
    def ndim(self):
        return len(self._shape)
    @property
    def dtype(self):
        return dtype(object)
    @property
    def T(self):
        return self.__array__.T
class DL_DT(_SimpleCData):
    _type_ = 'O'
    class _wrapper():
        __slots__ = ('__array_interface__',)
    def __new__(cls,
                address      :int,
                dict_dt      :dict,
                derived_types:dict,
                caller       :str  = '',
                caller_type  :str  = '',
                return_ctype :bool = False,
                debug        :bool = False):
        key = (address, id(dict_dt))
        obj = _DL_CACHE.get(key)
        if obj is not None:
            return obj
        inst = _SimpleCData.__new__(cls)
        _DL_CACHE[key] = inst
        return inst
    def __repr__(self):
        return f'<dl_f2py.DL_DT object at {self.__address__}>'
    @property
    def lib_dl_py2f(self):
        try:
            return self._lib_dl_py2f
        except:
            self._lib_dl_py2f = utils.modutils.getLinkedLib('dl_py2f', files(__package__), is_linked=True) \
                             or utils.modutils.getLinkedLib('dl_py2f', libpath.libpath, is_linked=True)
            return self._lib_dl_py2f
    def __init__(self,
                 address      :int,
                 dict_dt      :dict,
                 derived_types:dict,
                 caller       :str  = '',
                 caller_type  :str  = '',
                 return_ctype :bool = False,
                 debug        :bool = False):
        if type(dict_dt) is not dict or not dict_dt.get('_is_derived', False):
            print('\n >>> DL_F2PY ERROR: Argument dict_dt must be a dict containing information of data structure of a Fortran derived type', flush=True)
            exit(999)
        self._dict_dt       = dict_dt
        self._derived_types = derived_types
        self._caller        = caller
        self._return_ctype  = return_ctype
        self._debug         = debug
        self._children      = {} 
        setattr(self, '__address__', address)
        icount = 0
        for k, v in dict_dt.items():
            if not isinstance(v, dict) or '_offset' not in v:
                continue
            icount += 1
#            if type(v) is dict:
#                if '_offset' in v:
            if '_ndims' in v:
                if v.get('_is_derived', False):
                    if v.get('_is_deferred', False):
                        offset = v['_offset']
                        ndims = v['_ndims']
                        address = c_ulong.from_address(self.__address__+offset).value
                        __address__ = self.__address__ + offset
                        lbuff = [ c_long.from_address(__address__+i*8).value for i in range(5+ndims*3) ]
                        lbounds = lbuff[6::3]
                        ubounds = lbuff[7::3]
                        shape = tuple(ubounds[x]-lbounds[x] for x in range(ndims))
                        if any([x <= 0 for x in shape]) or not lbuff[0]:
                            setattr(self, k, c_void_p(None))
                            continue
                        shape = tuple(x+1 for x in shape)
                        _is_allocated = True
                        if lbuff[2] != v['_size_chunk']:
                            _is_allocated = False
                            if debug:
                                print(f'{_clr._bR}\n >>> WARNING: = In {_clr.CLR_}{_clr._bY}{caller}{_clr.CLR_}{_clr._bR}, there may be something wrong with derived-type array {_clr._bY}{k}{_clr.CLR_}{_clr._bR}!{_clr.CLR_}')
                                print(f'{_clr._bR}              Shape: {shape}{_clr.CLR_}')
                            setattr(self, k, empty((), dtype=typestrs.get(v['_type'], 'i8')))
                            continue
                        if not _is_allocated:
                            shape = ()
                        dims = [ range(x) for x in shape[::-1] ]
                        indices = list(product(*dims))
                        indices.reverse()
                        is_linked_list = caller.split('%')[-1].split('(')[0] == k
                        is_allocated = True

                        if is_linked_list:
                            if debug:
                                print(f' >>> NB: {_clr._bE}{caller}%{k}{_clr.CLR_} is an {_clr._bW}array of linked list of a deferred shape{_clr.CLR_}! (type: {_clr._W_i}{v["_type"]}{_clr.CLR_})!')
                            for i, _ in enumerate(indices):
                                addr_base = address + (len(indices)-i-1)*v['_size_chunk']
                                llbuff = [ c_long.from_address(addr_base).value for i in range(5+ndims*3) ]
                                if llbuff[2] != v['_size_chunk']:
                                    is_allocated = False
                                    break
                                for j, ind in enumerate(indices):
                                    addr = addr_base + (len(indices)-j-1)*v['_size_chunk']
                                    llbuff = [ c_long.from_address(addr).value for i in range(5+ndims*3) ]
                                    if llbuff[2] != v['_size_chunk']:
                                        is_allocated = False
                                        break
                                if not is_allocated:
                                    break
                        if not is_allocated:
                              if debug:
                                  print(f' >>> NB: {_clr._bW}{caller+'%'+k}{_clr.CLR_} is {_clr._R_i}NOT ALLOCATED!{_clr.CLR_}')
                              value = None
                              continue
                        self._children[k] = { '_kind'      : 'array_deferred',
                                              '_name'      :  k,
                                              '_meta'      :  v,
                                              '_typename'  :  v['_type'],
                                              '_offset'    :  v['_offset'],
                                              '_shape'     :  shape,
                                              '_size_chunk':  v.get('_size_chunk'),
                                              '_dim'       :  v.get('_dim'),
                                              '_ndims'     :  v.get('_ndims', 0),
                                              '_is_pointer':  v.get('_is_pointer', False)}
                        continue
#                        value = DL_DT(self.__address__+offset,
#                                      derived_types[v['_type']],
#                                      derived_types,
#                                      caller=caller+'%'+k,
#                                      caller_type=v['_type'],
#                                      return_ctype=return_ctype,
#                                      debug=debug)
#                        setattr(value, '_derived_type', v['_type'])
#                        value.__array__ = empty(shape=shape[::-1], dtype=object)
#                        setattr(self, k, value)
#                        indices = product(*dims)
#                        if debug:
#                            for i, ind in enumerate(indices):
#                                lcallers += [ caller+'%'+k+'('+','.join([str(_+1) for _ in ind[::-1]])+')' ]
#                        else:
#                            lcallers = [ caller ]*len(list(product(*dims)))
#                        for i, ind in enumerate(indices):
#                            addr = address + i*v['_size_chunk']
#                            value.__array__[ind] = DL_DT(addr,
#                                                         derived_types[v['_type']],
#                                                         derived_types,
#                                                         caller=lcallers[i],
#                                                         caller_type=v['_type'],
#                                                         return_ctype=return_ctype,
#                                                         debug=debug)
#                            setattr(value.__array__[ind], '_derived_type', v['_type'])
                    else:
                        shape = tuple(v['_dim']) 
                        self._children[k] = { '_kind'      : 'array_fixed',
                                              '_name'      :  k,
                                              '_meta'      :  v,
                                              '_typename'  :  v['_type'],
                                              '_offset'    :  v['_offset'],
                                              '_shape'     :  shape,
                                              '_size_chunk':  v.get('_size_chunk'),
                                              '_ndims'     :  v.get('_ndims', 0),
                                              '_is_pointer':  v.get('_is_pointer', False)}
                        continue
#                        value = DL_DT(self.__address__ + v['_offset'],
#                                      derived_types[v['_type']],
#                                      derived_types,
#                                      caller=caller+'%'+k,
#                                      caller_type=v['_type'],
#                                      return_ctype=return_ctype,
#                                      debug=debug)
#                        value.__array__ = empty(shape=v['_dim'][::-1], dtype=object)
#                        setattr(self, k, value)
#                        dims = [ range(x) for x in v['_dim'][::-1] ]
#                        indices = product(*dims)
#                        lcallers = []
#                        if debug:
#                            for i, ind in enumerate(indices):
#                                lcallers += [ caller+'%'+k+'('+','.join([str(_+1) for _ in ind[::-1]])+')' ]
#                        else:
#                            lcallers = [ caller ]*len(list(product(*dims)))
#                        for i, ind in enumerate(indices):
#                            addr = self.__address__+v['_offset']+i*v['_size_chunk']
#                            value.__array__[ind] = DL_DT(addr,
#                                                   derived_types[v['_type']],
#                                                   derived_types,
#                                                   caller=lcallers[i],
#                                                   caller_type=v['_type'],
#                                                   return_ctype=return_ctype,
#                                                   debug=debug)
                else:
                    if v.get('_is_deferred', False):
                        offset = v['_offset']
                        address = c_ulong.from_address(self.__address__+offset).value
                        ndims = v['_ndims']
                        __address__ = self.__address__ + offset
                        lbuff = [ c_long.from_address(__address__+i*8).value for i in range(5+ndims*3) ]
                        lbounds = lbuff[6::3]
                        ubounds = lbuff[7::3]
                        shape = tuple(ubounds[x]-lbounds[x]+1 for x in range(ndims))
                        if any([x <= 0 for x in shape]) or not lbuff[0]:
                            setattr(self, k, c_void_p(None))
                            continue
                        if lbuff[2] != sizeof(v['_type']):
                            _is_allocated = False
                            if debug:
                                print(f'{_clr._bR}\n >>> WARNING: In {_clr.CLR_}{_clr._bY}{caller}{_clr.CLR_}{_clr._bR}, there may be something wrong with {v["_type"].__name__} array {_clr.CLR_}{_clr._bY}{k}!{_clr.CLR_}')
                                print(f'              Size is {_clr._bR}{lbuff[2]}{_clr.CLR_} which is supposed to be {_clr._bG}{sizeof(v["_type"])}{_clr.CLR_}')
                            setattr(self, k, empty((), dtype=typestrs.get(v['_type'], 'i8')))
                            continue
                        size = lbuff[2]
                        if v['_type'] in typestrs and size:
                            abuff = self._wrapper()
                            typestr = typestrs[v['_type']]
                            abuff.__array_interface__ = {'shape':shape[::-1], 'data':(address, False), 'typestr': f'<{typestr}'}
                            try:
                                aview = array(abuff, copy=False)
                                setattr(self, k, aview)
                            except:
                                continue
                    else:
                        if v['_type'] in typestrs:
                            offset = v['_offset']
                            address = self.__address__ + v['_offset']
                            abuff = self._wrapper()
                            typestr = typestrs[v['_type']]
                            abuff.__array_interface__ = {'shape':v['_dim'][::-1], 'data':(address, False), 'typestr': f'<{typestr}'}
                            try:
                                aview = array(abuff, copy=False)
                                setattr(self, k, aview)
                            except:
                                continue
            else:
                if v['_type'] in derived_types:
                    if v.get('_is_pointer', False):
                        is_linked_list = caller.split('%')[-1].split('(')[0] == k
                        if is_linked_list and debug:
                            print(f' >>> NB: {_clr._bE}{caller}%{k}{_clr.CLR_} is a {_clr._bW}scalar linked list!{_clr.CLR_}')
                        address = c_ulong.from_address(self.__address__+v['_offset']).value
                        if address:
                            if self.lib_dl_py2f.associated(c_ulong(address)):
                                if debug:
                                    print(f' >>> WARNING: {_clr._R}{caller}%{_clr.CLR_}{_clr._bR}{k}{_clr.CLR_} at ({_clr._uW}{hex(address)}{_clr.CLR_}) might be a wild pointer!')
                                value = None
                                setattr(self, k, value)
                                continue
                            else:
                                try:
                                    self._children[k] = { '_kind'      : 'pointer',
                                                          '_name'      :  k,
                                                          '_meta'      :  v,
                                                          '_typename'  :  v['_type'],
                                                          '_offset'    :  v['_offset'],
                                                          '_size_chunk':  v.get('_size_chunk'),
                                                          '_dim'       :  v.get('_dim'),
                                                          '_ndims'     :  v.get('_ndims', 0),
                                                          '_is_pointer':  v.get('_is_pointer', False)}
                                    continue
#                                    value = DL_DT(address,
#                                                  derived_types[v['_type']],
#                                                  derived_types,
#                                                  caller=caller+'%'+k,
#                                                  caller_type=v['_type'],
#                                                  return_ctype=return_ctype,
#                                                  debug=debug)
#                                    if value._return_immediately:
#                                        if is_linked_list:
#                                            self._return_immediately = True
#                                            return
#                                        else:
#                                            if debug:
#                                                print(f' >>> WARNING: {_clr._R}{caller}%{_clr.CLR_}{_clr._bR}{k}{_clr.CLR_} is likely to be an unassociated pointer to a linked list! (type: {_clr._Y}{v["_type"]}{_clr.CLR_})')
#                                    setattr(value, '_derived_type', v['_type'])
                                except RecursionError:
                                    value = None
                                    self._return_immediately = True
                                    return
                        else:
                            value = None
                            setattr(self, k, value)
                    else:
                        self._children[k] = { '_kind'      : 'scalar',
                                              '_name'      :  k,
                                              '_meta'      :  v,
                                              '_typename'  :  v['_type'],
                                              '_offset'    :  v['_offset'],
                                              '_size_chunk':  v.get('_size_chunk'),
                                              '_dim'       :  v.get('_dim'),
                                              '_ndims'     :  v.get('_ndims', 0),
                                              '_is_pointer':  v.get('_is_pointer', False)}
                        continue
#                        value = DL_DT(self.__address__ + v['_offset'],
#                                      derived_types[v['_type']],
#                                      derived_types,
#                                      caller=caller+'%'+k,
#                                      caller_type=v['_type'],
#                                      return_ctype=return_ctype,
#                                      debug=debug)
                    setattr(self, k, value)
                else:
                    if v.get('_is_char', False):
                        if v.get('_is_deferred', False):
                            pass
                        else:
                            setattr(self, f'_DL_DT_CA_{k}', self.__address__+v['_offset'])
                            setattr(self, f'_DL_DT_CL_{k}', v.get('_length', 1))
                            sbuff_getter = f'return self._getStr(self._DL_DT_CA_{k}, self._DL_DT_CL_{k}, debug={debug})'
                            sbuff_setter = f'self._setStr(self._DL_DT_CA_{k}, self._DL_DT_CL_{k}, value, debug={debug}); return 0'
                            setter_args = f'{self.__address__+v["_offset"]}, {v.get("_length", 1)}'
                    else:
                        if v.get('_is_pointer', False):
                            sbuff_getter = f'return self._getPtr(self._DL_DT_P_{k}, {v["_type"].__name__}, name="{k}", return_ctype={return_ctype}, debug={debug})'
                            setattr(self, f'_DL_DT_P_{k}', self.__address__+v['_offset'])
                            sbuff_setter = f'self._setPtr(self._DL_DT_P_{k}, {v["_type"].__name__}, value, name="{k}", debug={debug}); return 0'
                            setter_args = f'self._DL_DT_{k}'
                        else:
                            sbuff_getter = ''
                            sbuff_setter = ''
                            if v['_type'] == '_tb_procedure':
                                continue
                            value = v['_type'].from_address(self.__address__+v['_offset'])
                            setter_args = f'self._DL_DT_{k}'
                            setattr(self, '_DL_DT_'+k, value)
                    if return_ctype:
                        sbuff = f'''
def _fget(self):
    {sbuff_getter}
    return self._DL_DT_{k}
'''
                    else:
                        sbuff = f'''
def _fget(self):
    {sbuff_getter}
    return self._DL_DT_{k}.value
'''
                    exec(sbuff)
                    sbuff = f'''
def _fset(self, value):
    {sbuff_setter}
    setters.get(type(self._DL_DT_{k}), DL_DL.setInt)({setter_args}, value)
'''
                    exec(sbuff)
                    setattr(self, '_get_DL_DT_'+k, locals()['_fget'])
                    setattr(self, '_set_DL_DT_'+k, locals()['_fset'])
                    setattr(self.__class__, k, property(fget=getattr(self, '_get_DL_DT_'+k),
                                                        fset=getattr(self, '_set_DL_DT_'+k)))
#                else:
#                    pass
    def printBytes(self, dict_dt={}, name='', addr=0, increment=4, size_extra=0):
        if dict_dt:
            if dict_dt.get('_is_char', False):
                increment = 1
            print()
            if name:
                print(f' Name: {name}')
            print(' Type:', dict_dt['_type'])
            addr = self.__address__ + dict_dt['_offset']
            print(' Addr:', addr)
            print(' Incr:', increment, 'Bytes')
            if 'ndims' in dict_dt:
                ndims = dict_dt['_ndims']
                if dict_dt.get('_is_deferred', False):
                    size = 40+ndims*24
            else:
                if dict_dt.get('_is_char', False):
                    size = dict_dt.get('_length', 1)
                else:
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
        sbuff = ''
        for i in range(0, length, 1):
            sbuff += c_char.from_address(addr+i).value.decode()
        return sbuff
    def _setStr(self, addr, length, value, debug=False):
        DL_DL.setChar(addr, length, value)
    def _getPtr(self, addr, ctype, name='', return_ctype=False, debug=False):
        addr_target = c_ulong.from_address(addr).value
        if debug:
            self.printBytes(name=name, addr=addr, size_extra=8)
        if addr_target:
            value = ctype.from_address(addr_target)
            setattr(value, '_is_associated', True)
            if return_ctype:
                return value
            else:
                return value.value
        else:
            value = c_void_p(None)
            setattr(value, '_is_associated', False)
            return value
    def _setPtr(self, addr, ctype, value, name='', debug=False):
        attr = self._getPtr(addr, ctype, debug=debug)
        if attr:
            setters.get(type(attr), DL_DL.setInt)(attr, value)
        else:
            print(f' >>> DL_F2PY ERROR: Pointer "{name}" in {self} is not allocated/associated')
    def setValue(self, name, value):
        attr = getattr(self, name.strip().lower())
        setters.get(type(attr), DL_DL.setInt)(attr, value)
    def __iter__(self):
        return self
    def __getitem__(self, index):
        if hasattr(self, '__array__'):
            return self.__array__[index]
        else:
            raise TypeError(f'{self.__repr__()} is not subscriptable')
    def __getattr__(self, name):
        meta = self._children.get(name)
        if meta is None:
            raise AttributeError(name)
        kind = meta['_kind']
        if kind == 'scalar':
            inst = _DL_DT_Scalar(self, meta)
            obj = inst.instantiate()
            setattr(self, name, obj)
            return obj
        if kind == 'pointer':
            inst = _DL_DT_Pointer(self, meta)
            obj = inst.instantiate()
            setattr(self, name, obj)
            return obj
        if kind in [ 'array_fixed', 'array_deferred' ]:
            inst = _DL_DT_Array(self, meta)
            setattr(self, name, inst)
            return inst
        raise AttributeError(f'Unknown lazy kind {kind}')
    def __setitem__(self, index, value, **args):
        print(f' >>> DL_F2PY ERROR: Elements in array {self}')
        print( '                    cannot be replaced')
        print(f'                    (Fortran derived type: "{self._derived_type}", shape: {self.__shape__})')
    def __len__(self):
        try:
            return self.__array__.size
        except:
            raise AttributeError(f'{self.__repr__()} is scalar and has no length')
    @property
    def __shape__(self):
        if hasattr(self, '__array__'):
            return self.__array__.shape
        else:
            raise AttributeError(f'{self.__repr__()} is scalar and has no shape')
    @property
    def _return_immediately(self):
        try:
            return self.__return_immediately
        except:
            return False
    @_return_immediately.setter
    def _return_immediately(self, val:bool):
        self.__return_immediately = val
class DL_DL(CDLL):
    __modules = []
    class _DL_MOD(dict):
        def __init__(self, dl_dl, *args, **kwargs):
            object.__setattr__(self, '__dl_dl__', dl_dl)
            self.update(*args)
        def __getattr__(self, attr):
            return self.__dl_dl__.getValue(attr, module=self['_name'], return_ctype=self.__dl_dl__.return_ctype, debug=False)
        def __setattr__(self, attr, val):
            self.__dl_dl__.setValue(attr, val, module=self['_name'], debug=False)
    def __new__(cls, fullpath_to_lib, mode=RTLD_GLOBAL, debug=False):
        inst = CDLL.__new__(cls)
        inst.__init__(fullpath_to_lib, mode=mode)
        inst._fullpath_to_lib = fullpath_to_lib
        inst._derived_types = {}
        inst._instances_derived_types = {}
        return inst
    def __getattr__(self, name):
        if name in [ '_moddir', '_modules', '_lib_dl_py2f' ]:
            return object.__getattribute__(self, name)
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
        try:
            return self._lib_dl_py2f
        except:
            self._lib_dl_py2f = utils.modutils.getLinkedLib('dl_py2f', files(__package__), is_linked=True) \
                             or utils.modutils.getLinkedLib('dl_py2f', libpath.libpath, is_linked=True)
            return self._lib_dl_py2f
    @property
    def moddir(self):
        try:
            return self._moddir
        except:
            print('\n >>> ERROR: A directory containing Fortran module files has not been defined!\n')
            return []
    @moddir.setter
    def moddir(self, val):
        try:
            self._moddir
        except:
            self._moddir = []
        if os.path.isdir(val.strip()):
            if not val.strip() in self._moddir:
                self._moddir += [ val.strip() ]
        else:
            print(f'\n >>> ERROR: Directory {val.strip()} does not exist!')
    @property
    def return_ctype(self):
        try:
            return self._return_ctype
        except:
            return False
    @return_ctype.setter
    def return_ctype(self, val:bool):
        self._return_ctype = val
    @property
    def symbols(self):
        self.lib_dl_py2f.getSymbols.argtypes = [ c_void_p, c_char_p ]
        self.lib_dl_py2f.getSymbols.restype = c_char_p
        bbuff = self.lib_dl_py2f.getSymbols(self.lib_dl_py2f.dl_f2py(), self._fullpath_to_lib.strip().encode())
        lbuff = bbuff.decode().split(';')
        return [ s for s in lbuff if s and ('_MOD___copy_'       not in s
                                        and '_MOD___deallocate_' not in s
                                        and '_MOD___def_init_'   not in s
                                        and '_MOD___vtab_'       not in s
                                        and '_MOD___final_'      not in s) ]
    def searchSymbol(self, *keywords):
        symbols_all = self.symbols
        return [ s for s in symbols_all if all([ k.strip().lower() in s.strip().lower() for k in keywords ]) ]
    @property
    def modulenames(self):
        symbols_all = self.symbols
        lbuff = [ s.split('_MOD_')[0].lstrip('__') for s in symbols_all if '_MOD_' in s ]
        return sorted(list(set(lbuff)))
    @property
    def _to_parse_all(self):
        try:
            return self.__to_parse_all
        except:
            return True
    @_to_parse_all.setter
    def _to_parse_all(self, val):
         self.__to_parse_all = val
    @property
    def modules(self):
        try:
            if self._to_parse_all:
                for moddir in self.moddir:
                    self.parseAllModules(moddir, caller='modules')
            return self._modules
        except:
            self._modules = self._DL_MOD(self)
            if self._to_parse_all:
                for moddir in self.moddir:
                    self.parseAllModules(moddir, caller='modules')
            return self._modules
    @modules.setter
    def modules(self, val):
        try:
            overlap = set(self._modules.keys()) & (val.keys())
            if len(overlap):
                print(f'\n >>> WARNING: The following module instances will be overwritten:\n             ', ', '.join(overlap))
            self._modules.update(val)
        except:
            self._modules = self._DL_MOD(self)
            self._modules.update(val)
    @property
    def nsymbols(self):
        return len(self.symbols)
    def getModuleSymbols(self, module):
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
    def getValue(self, symbol, ctype=c_int, shape=(), module='', return_ctype=False, debug=False):
        symbol_original = symbol
        lbuff = []
        if '%' in symbol:
            lbuff = symbol.split('%')
            symbol = lbuff[0]
        fullname_symbol = self.getSymbolOfModule(symbol, module)
        _instances_derived_types = self._instances_derived_types.get(module, {})
        derived_type = _instances_derived_types.get(symbol, None)
        ctype = selectcases.get(ctype, ctype)
        try:
            mod = getattr(self.modules, module)
        except:
            for d in self.moddir:
                mod = self.parseModule(os.path.join(d, module+'.mod'), debug=debug)
                if mod != None:
                    break
        try:
            dbuff = mod.get(symbol, {})
        except UnboundLocalError:
            dbuff = {}
        ctype = dbuff.get('_type', ctype)
        shape = dbuff.get('_dim' , shape)
        entity = ctype.in_dll(self, fullname_symbol)
        if shape != ():
            if dbuff.get('_is_deferred', False):
                if dbuff.get('_is_char', False):
                    if not dbuff.get('_is_deferred_char', False):
                        lchar = dbuff.get('_length', 1)
                addr = c_ulong.in_dll(self, fullname_symbol).value
            else:
                if dbuff.get('_is_char', False):
                    if not dbuff.get('_is_deferred_char', False):
                        lchar = dbuff.get('_length', 1)
                addr = addressof(entity)
            abuff = DL_DT._wrapper()
            typestr = typestrs[ctype]
            if ctype == c_char_p:
                typestr += str(lchar)
            abuff.__array_interface__ = {'shape':shape[::-1], 'data':(addr, False), 'typestr': f'<{typestr}'}
            aview = array(abuff, copy=False)
            return aview
        accessor = None
        accessor_key = None
        if lbuff:
            accessor_key = (module, symbol_original)
            accessor = _accessor_cache.get(accessor_key)
        if derived_type in self._derived_types.get(module, {}):
            entity = DL_DT(addressof(entity),
                           self._derived_types[module][derived_type],
                           self._derived_types[module],
                           caller=fullname_symbol,
                           caller_type=derived_type,
                           return_ctype=return_ctype,
                           debug=debug)
            if lbuff:
                if accessor is None:
                    path = []
                    for attr in lbuff[1:]:
                        attr = attr.strip()
                        if '(' in attr and ')' in attr:
                            name = attr.split('(')[0].strip()
                            index = attr.split('(')[1].split(')')[0]
                            index = eval(f'({index},)')
                            index = tuple(i-1 for i in index)[::-1]
                            path.append(('indexed', name, index))
                        else:
                            path.append(('attr', attr))
                    def _accessor(obj, _path=tuple(path)):
                        res = obj
                        for kind, *spec in _path:
                            if kind == 'attr':
                                res = getattr(res, spec[0])
                            elif kind == 'indexed':
                                name, ind = spec
                                res = getattr(res, name)[ind]
                        return res
                    _accessor_cache[accessor_key] = _accessor
                    accessor = _accessor
                entity = accessor(entity)
            return entity
        if module.strip():
            mod = getattr(self.modules, module)
            if symbol in mod:
                try:
                    ctype = mod[symbol]['_type']
                except KeyError:
                    print(f' >>> ERROR: The data type of symbol {symbol} in module {module} is not defined! Please contact {_clr.__email__}.')
                if mod[symbol].get('_is_pointer', False):
                    if return_ctype:
                        return ctype.from_address(c_ulong.in_dll(self, fullname_symbol).value)
                    else:
                        return ctype.from_address(c_ulong.in_dll(self, fullname_symbol).value).value
                else:
                    entity = ctype.in_dll(self, fullname_symbol)
            else:
                print(f' >>> ERROR: Symbol {symbol} not found in module {module}!')
                return
        if return_ctype:
            return entity
        else:
            return entity.value
    def setValue(self, symbol, value, module='', debug=False):
        if module.strip():
            mod = getattr(self.modules, module)
            if symbol in mod:
                try:
                    ctype = mod[symbol]['_type']
                    entity = self.getValue(symbol, ctype, module=module, return_ctype=True, debug=debug)
                    setters.get(type(entity), self.setInt)(entity, value)
                except KeyError:
                    print(f' >>> ERROR: The data type of symbol {symbol} in module {module} is not defined! Please contact {_clr.__email__}.')
        return
    @staticmethod
    def setInt(entity, value):
        lbuff = utils.ctypeutils.int2intarray(value)
        addr = addressof(entity)
        for i in range(4):
            memset(addr+i, lbuff[i], 1)
    @staticmethod
    def setLong(entity, value):
        lbuff = utils.ctypeutils.long2intarray(value)
        addr = addressof(entity)
        for i in range(8):
            memset(addr+i, lbuff[i], 1)
    @staticmethod
    def setChar(addr, length, value):
        try:
            value = value.encode()
        except:
            pass
        for i in range(length):
            if i < len(value):
                memset(addr+i, value[i], 1)
            else:
                memset(addr+i, 32, 1)
    @staticmethod
    def setBool(entity, value):
        lbuff = utils.ctypeutils.bool2intarray(value)
        addr = addressof(entity)
        memset(addr, lbuff[0], 1)
        for i in range(1,4):
            memset(addr+i, 0, 1)
    @staticmethod
    def setFloat(entity, value):
        lbuff = utils.ctypeutils.float2intarray(value)
        addr = addressof(entity)
        for i in range(4):
            memset(addr+i, lbuff[i], 1)
    @staticmethod
    def setDouble(entity, value):
        lbuff = utils.ctypeutils.double2intarray(value)
        addr = addressof(entity)
        for i in range(8):
            memset(addr+i, lbuff[i], 1)
    def parseAllModules(self, moddir, recursive=True, padding=True, overwrite=False, caller='', debug=False):
        from os import path, sep, walk
        mods = self._DL_MOD(self)
        if caller == 'modules':
            items = self._modules.items()
            self._to_parse_all = False
        else:
            items = object.__getattribute__(self, 'modules').items()
            self._to_parse_all = False
        if recursive:
            for dir_full, subdirs, filenames in walk(moddir):
                for filename in filenames:
                    if debug:
                        print(f' >>> Parsing {_clr._uW_}{path.join(dir_full, filename)}{_clr.CLR_}')
                    filepath = path.join(dir_full, filename).strip()
                    basename = utils.fileutils.getBaseName(path.basename(filepath))
                    to_parse = True
                    for m, d in items:
                        f = d.get('_filename', '')
                        if filename == f and basename == m and not overwrite:
                            to_parse = False
                            continue
                    if to_parse:
                        mod = self.parseModule(filepath, debug=debug)
                        mods.update({mod['_name']:mod})
                        object.__setattr__(mods, mod['_name'], mod)
                        self._to_parse = True
        else:
            pass
        self.modules = mods
        return mods
    def parseModule(self, modpath, padding=True, debug=False):
        import gzip
        from time  import time
        from math  import prod
        from os    import path
        if not path.isfile(modpath):
            return
        fortran_internals = [ '__vtab_', '__vtype_', '__def_init_', '__copy_', '__final_', '__convert' ]
        basename = utils.fileutils.getBaseName(path.basename(modpath))
        def _printSummary(_dbuff):
            print('\n '+'*'*72)
            print(f" {_clr._bW}Summary of module{_clr.CLR_} '{_clr._bB_b}{dbuff['_name']}{_clr.CLR_}'")
            print(' '+'*'*72)
            print(f' File path: {_clr._u_W_}{path.abspath(modpath)}{_clr.CLR_}')
            print(f' {dbuff["_title"]}')
            for _k, _v in _dbuff.items():
                if _k == '_title':
                    continue
                if type(_v) is dict:
                    print(f'\n Entry "{_clr._bC}{_clr._k}{_clr.CLR_}":')
                    len_keys = max([len(_) for _ in _v.keys()])
                    fmt_keys = f'     {_clr._bY}' + '{_clr._kk:<' + f'{len_keys}' + '}' + f'{_clr.CLR_}'
                    for _kk, _vv in _v.items():
                        if _kk.startswith('_'):
                            if type(_vv) is int:
                                fmt_vv = f': {_clr._bM_b}'+'{_clr._vv}'+f'{_clr.CLR_}'
                            elif type(_vv) is bool:
                                fmt_vv = f': {_clr._G}'+'{_clr._vv}'+f'{_clr.CLR_}'
                            elif type(_vv) is str:
                                fmt_vv = f': {_clr._C}'+"'{_clr._vv}'"+f'{_clr.CLR_}'
                            elif isinstance(_SimpleCData, type(_vv)):
                                fmt_vv = f': {_clr._R}'+'{_clr._vv}'+f'{_clr.CLR_}'
                            else:
                                fmt_vv = ': {_clr._vv}'
                            print(f'{fmt_keys+fmt_vv}'.format(_kk=_kk, _vv=_vv))
                    fmt_keys += ': {_clr._vv}'
                    for _kk, _vv in _v.items():
                        if not _kk.startswith('_'):
                            if type(_vv) is dict:
                                print(f'{fmt_keys}'.format(_kk=_kk, _vv=''), end='')
                                for _iii, (_kkk,_vvv) in enumerate(_vv.items()):
                                    if _iii:
                                        print(' '*(len_keys+7), end='')
                                    if type(_vvv) is int:
                                        if _kkk == '_offset':
                                            print(f'''{_clr._bC}{"'"+_kkk+"'":17s}{_clr.CLR_}: {_clr._bY_r}{_clr._vvv}{_clr.CLR_}''')
                                        else:
                                            print(f'''{_clr._C}{"'"+_kkk+"'":17s}{_clr.CLR_}: {_clr._M}{_clr._vvv}{_clr.CLR_}''')
                                    elif type(_vvv) is bool:
                                        print(f'''{_clr._C}{"'"+_kkk+"'":17s}{_clr.CLR_}: {_clr._G}{_clr._vvv}{_clr.CLR_}''')
                                    elif type(_vvv) is str:
                                        print(f'''{_clr._C}{"'"+_kkk+"'":17s}{_clr.CLR_}: {_clr._bY}{_clr._vvv}{_clr.CLR_}''')
                                    elif type(_vvv) is tuple:
                                        print(f'''{_clr._C}{"'"+_kkk+"'":17s}{_clr.CLR_}: {_clr._vvv}''')
                                    elif isinstance(_SimpleCData, type(_vvv)):
                                        print(f'''{_clr._C}{"'"+_kkk+"'":17s}{_clr.CLR_}: {_clr._bGbE}{_clr._vvv}{_clr.CLR_}''')
                                    else:
                                        print(f'''{_clr._C}{"'"+_kkk+"'":17s}{_clr.CLR_}: {_clr._R}{_clr._vvv}{_clr.CLR_}''')
                            else:
                                print(f'{fmt_keys}'.format(_kk=_kk, _vv=_vv))
                else:
                    print(f'\n Entry "{_clr._k}": {_clr._v}')
            print(f'\n >>> Total time used: {time()-t0} s\n')
        dbuff = self._DL_MOD(self, {'_name':basename}) # module-file-based dict, however we don't know which module from the declarations (unless we use the filename)
        indices2entries = {}
        _instances_derived_types = {}
        try:
            fp = gzip.open(modpath)
            fp.read()
        except:
            return
        with gzip.open(modpath) as mod, open(f'._dl_py2f_{basename}.txt', 'w') as fp:
            sbuff = ' '.join(mod.read().decode().replace('()', '').replace('\'\'', '').split())
            fp.write('DL_PY2F\nLegend to hierarchies:\n')
            fp.write('{[(⟨⟪<“”>⟫⟩)]}\n\n')
            is_done    = True
            depth  = 0
            offset = 0
            sbuff0 = ''
            sbuff1 = ''
            sbuff2 = ''
            sbuff3 = ''
            sbuff4 = ''
            sbuff5 = ''
            sbuff6 = ''
            sbuff7 = ''
            sizes  = []
            types2ids   = {}
            ids2types   = {}
            types2sizes = {'_tb_procedure':0}
            for ic, c in enumerate(sbuff):
                if c == '(':
                    depth += 1
                elif c == ')':
                    depth -= 1
                if c == '(':
                    if depth == 1:
                        sbuff1 = ''
                        if sbuff0:
                            dbuff.update({'_title':sbuff0})
                        fp.write('\n{')
                    elif depth == 2:
                        sbuff2 = ''
                        self.__entry = ''
                        try:
                            modname = sbuff1.replace('(','').replace(')','').strip().split()[1].strip("'")
                        except:
                            modname = ''
                        try:
                            lsbuff1 = sbuff1.replace('(','').replace(')','').strip().split()
                            index = int(lsbuff1[0])
                            entry = lsbuff1[1].strip("'")
                            if len(lsbuff1) == 4:
                                module = lsbuff1[2].strip("'")
                            elif len(lsbuff1) == 3:
                                module = ''
                        except:
                            if is_done:
                                entry = sbuff1.replace('(','').replace(')','').strip()
                                module = ''
                        is_internal = any([entry.startswith(x) for x in fortran_internals])
                        is_internal += entry.lower().strip() in [ 'c_ptr', 'c_funptr' ]
                        if is_internal:
                            fp.write('\n    [')
                            continue
                        if entry.islower():
                            if entry in dbuff and is_done:
                                entry += f'_{index}'
                        else:
                            entry = entry.lower()
                        if entry:
                            self.__entry = entry
                            if is_done:
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
                            stride = 0
                            sizes = []
                            procedures = {}
                            fp.write('\n    [')
                        else:
                            fp.write('\n    [')
                    elif depth == 3:
                        sbuff3 = ''
                        if is_internal:
                            fp.write('\n        (')
                            continue
                        fp.write('\n        (')
                    elif depth == 4:
                        sbuff4 = ''
                        lbuff3 = sbuff3.split()
                        self.__printSbuff(sbuff3, sbuff=3, depth=4, entry='arr03_of_int', debug=debug)
                        if is_internal:
                            fp.write('\n            ⟨')
                            continue
                        if dict_entry.get('_is_const', False):
                            if sbuff3.strip() == 'ARRAY':
                                dict_entry.update({'_ndims':1})
                            if dict_entry.get('_ndims', 0):
                                try:
                                    dict_entry.update({'_ndims':int(sbuff3)})
                                    dict_entry.update({'_lbuff': []})
                                except:
                                    pass
                            if len(lbuff3) == 6:
                                if lbuff3[0] == 'CHARACTER' and lbuff3[-1] == 'CHARACTER':
                                    dict_entry.update({'_is_char':True})
                                    dict_entry.update({'_type':selectcases[lbuff3[0]+lbuff3[1]]})
                        else:
                            if len(lbuff3) == 6:
                                if lbuff3[0] == 'CHARACTER' and lbuff3[-1] == 'CHARACTER':
                                    dict_entry.update({'_is_char':True})
                                    dict_entry.update({'_type':selectcases[lbuff3[0]+lbuff3[1]]})
                            if dict_entry.get('_is_char', False):
                                if len(lbuff3) == 3:
                                    if lbuff3[-1] == 'EXPLICIT':
                                        dict_entry.update({'_ndims':int(lbuff3[0])})
                                        dict_entry.update({'_lbuff': []})
                            else:
                                if len(lbuff3) == 3:
                                    if lbuff3[-1] == 'EXPLICIT':
                                        dict_entry.update({'_ndims':int(lbuff3[0])})
                                        dict_entry.update({'_lbuff': []})
                        fp.write('\n            ⟨')
                    elif depth == 5:
                        sbuff5 = ''
                        lbuff4 = sbuff4.split()
                        self.__printSbuff(sbuff4, sbuff=4, depth=5, entry='arr03_of_int', debug=debug)
                        if is_internal:
                            fp.write('\n                ⟪')
                            continue
                        try:
                            name = lbuff4[1].strip("'")
                            idx  = int(lbuff4[0])
                        except:
                            name = sbuff4
                            idx  = None
                        if sbuff4.strip():
                            if entry and not is_internal:
                                dbuff.update({entry:dict_entry})
                            if len(name.split()) == 1:
                                if name.strip() not in [ 'UNKNOWN-ACCESS', 'CONSTANT', 'c_address' ]:
                                    name_member = name
                                    dict_entry.update({name_member:{'_index':idx}})
                                    dict_member = dict_entry[name_member]
                        try:
                            dict_member
                        except:
                            dict_entry.update({'_no_member':True})
                        fp.write('\n                ⟪')
                    elif depth == 6:
                        sbuff6 = ''
                        lbuff5 = sbuff5.split()
                        self.__printSbuff(sbuff5, sbuff=5, depth=6, entry='arr03_of_int', debug=debug)
                        if is_internal:
                            fp.write('\n                    <')
                            continue
                        if len(lbuff5) == 3:
                            if lbuff5[-1] == 'EXPLICIT':
                                dict_member.update({'_ndims':int(lbuff5[0])})
                                dict_member.update({'_lbuff': []})
                                dict_member.update({'_forced_pad':True})
                        if len(lbuff5) == 6:
                            if lbuff5[0] == 'CHARACTER' and lbuff5[-1] == 'CHARACTER':
                                dict_member.update({'_is_char':True})
                                dict_member.update({'_type':selectcases[lbuff5[0]+lbuff5[1]]})
                        fp.write('\n                    <')
                    else:
                        sbuff7 = ''
                        if is_internal:
                            fp.write('\n                        “')
                            continue
                        fp.write('\n                        “')
                elif c == ')':
                    if depth == 0:
                        fp.write('\n}')
                        sbuff1 = ''
                    elif depth == 1:
                        sbuff1 = ''
                        if is_internal:
                            fp.write('\n    ]')
                            continue
                        if entry:
                            types2sizes.update({entry:sum(sizes)})
                            if 'MODULE' in sbuff3:
                                dict_entry.update({'_is_module':True})
                        else:
                            is_type    = False
                        offset = 0
                        try:
                            if is_done and entry and not is_internal:
                                dbuff.update({entry:dict_entry})
                        except:
                            pass
                        fp.write('\n    ]')
                        sbuff2 = ''
                    elif depth == 2:
                        if is_internal:
                            sbuff3 = ''
                            fp.write('\n        )')
                            continue
                        sbuff2 = ''
                        lbuff3 = sbuff3.split()
                        self.__printSbuff(sbuff3, sbuff=3, depth=2, entry='arr03_of_int', debug=debug)
                        if 'PARAMETER' in lbuff3:
                            dict_entry.update({'_is_const':True})
                        if 'VARIABLE' in lbuff3:
                            dict_entry.update({'_is_var':True})
                            if lbuff3[-1] == 'POINTER':
                                dict_entry.update({'_is_pointer':True})
                            if lbuff3[-1] == 'TARGET':
                                dict_entry.update({'_is_target':True})
                        if dict_entry.get('_is_const', False) or dict_entry.get('_is_var', False):
                            if len(lbuff3) == 6:
                                if lbuff3[0] + lbuff3[1] in selectcases:
                                    dt = selectcases[lbuff3[0]+lbuff3[1]]
                                    dict_entry.update({'_type':dt})
                            if len(lbuff3) == 2 and lbuff3[1].startswith("'") and lbuff3[1].endswith("'"):
                                hexadecimal = lbuff3[1].strip("'")
                                value = self.hexadecimal2number(hexadecimal, dict_entry['_type'])
                                dict_entry.update({'_value':value})
                                object.__setattr__(dbuff, entry, value)
                            if dict_entry.get('_is_char', False):
                                if len(sbuff3.split("'")) == 3:
                                    dict_entry.update({'_length':int(lbuff3[1])})
                                    dict_entry.update({'_value':sbuff3.split("'")[-2][:dict_entry['_length']]})
                                    object.__setattr__(dbuff, entry, dict_entry['_value'])
                        if len(lbuff3) > 1:
                            if lbuff3[0] == 'DERIVED':
                                dict_entry.update({'_is_derived':True})
                                if not lbuff3[1].isdigit():
                                    if index not in ids2types and not is_internal:
                                        ids2types.update({index:entry})
                                        types2ids.update({entry:index})
                                        dict_entry.update({'_type_id':index})
                        if len(lbuff3) > 1:
                            if lbuff3[0]+lbuff3[1] in selectcases:
                                dt = selectcases[lbuff3[0]+lbuff3[1]]
                                dict_entry.update({'_type':dt})
                                types2sizes.update({dt:int(lbuff3[1])})
                            if 'DEFERRED' in lbuff3:
                                dict_entry.update({'_is_deferred':True})
                                dict_entry.update({'_ndims':int(lbuff3[0])})
                        if dict_entry.get('_is_const', True):
                            if dict_entry.get('_type', None) == c_bool:
                                if len(lbuff3) == 2:
                                    hexadecimal = lbuff3[-1]
                                    value = self.hexadecimal2number(hexadecimal, dict_entry['_type'])
                                    dict_entry.update({'_value':value})
                                    object.__setattr__(dbuff, entry, value)
                        fp.write('\n        )')
                        try:
                            index_derived_type = int(sbuff3.partition('DERIVED')[2].split()[0])
                            tmp = indices2entries.get(index_derived_type, index_derived_type)
                            if not entry.startswith('__vtab'):
                                _instances_derived_types.update({entry:tmp})
                        except:
                            pass
                        sbuff3 = ''
                    elif depth == 3:
                        sbuff3 = ''
                        lbuff4 = sbuff4.split()
                        self.__printSbuff(sbuff4, sbuff=4, depth=3, entry='arr03_of_int', debug=debug)
                        if is_internal:
                            sbuff4 = ''
                            fp.write('\n            ⟩')
                            continue
                        if dict_entry.get('_ndims', 0):
                            if dict_entry.get('_is_const', False):
                                if len(lbuff4) == dict_entry['_ndims']:
                                    shape = [ int(_.strip("'")) for _ in lbuff4 ][::-1]
                                    if all(shape):
                                        dict_entry.update({'_dim':tuple(shape)})
                                        dict_entry.update({'_value': full((len(dict_entry['_lbuff']),),
                                                                          dict_entry['_lbuff'],
                                                                          dtype=dict_entry['_type']).reshape(shape, order='C')})
                                        dict_entry.pop('_lbuff')
                                        object.__setattr__(dbuff, entry, dict_entry['_value'])
                            else:
                                if len(lbuff4) == 2 and lbuff4[1].startswith("'") and lbuff4[1].endswith("'"):
                                    dict_entry['_lbuff'] += [ int(lbuff4[-1].strip("'")) ]
                                    if not len(dict_entry['_lbuff'])%2:
                                        dict_entry.update({'_dim':tuple(dict_entry['_lbuff'][i*2+1]-dict_entry['_lbuff'][i*2]+1 for i in range(len(dict_entry['_lbuff'][::2])))})
                                        if len(dict_entry['_dim']) == dict_entry['_ndims']:
                                            dict_entry.pop('_lbuff')
                        try:
                            name_member
                        except UnboundLocalError:
                            name_member = ''
                        if not name_member.strip():
                            continue
                        if dict_member.get('_ndims', 0):
                            dict_member.update({'_forced_pad':True})
                            if dict_member.get('_is_deferred', False):
                                if dict_member.get('_is_char', False):
                                    residue = sum(sizes)%dict_member['_stride']
                                    if padding and residue:
                                        if debug > 1:
                                            print(f' >>> {_clr._bW}Padding{_clr.CLR_} {_clr._bC}{entry}%{_clr.CLR_}{_clr._bY}{name_member}{_clr.CLR_} by {_clr._bM}{dict_member["_stride"]-residue}{_clr.CLR_}:')
                                        pad = abs(dict_member['_stride'] - residue)
                                        sizes[-1] += pad
                                        dict_member.update({'_padded':pad})
                                        dict_member.update({'_is_padded':True})
                                    offset += sizes[-1]
                                else:
                                    residue = sum(sizes)%dict_member['_stride']
                                    if residue and dict_member['_size'] > residue:
                                        if debug > 1 and not is_internal:
                                            print(f' >>> {_clr._bW}Padding{_clr.CLR_} {_clr._bC}{entry}%{_clr.CLR_}{_clr._bY}{name_member}{_clr.CLR_} by {_clr._bM}{dict_member["_stride"]-residue}{_clr.CLR_} bytes:')
                                            print(f' >>>     Size: {_clr._bM}{dict_member["_size"]}{_clr.CLR_}, Stride: {_clr._bM}{dict_member["_stride"]}{_clr.CLR_}, Residue: {_clr._bM}{residue}{_clr.CLR_}')
                                        pad = abs(dict_member['_stride'] - residue)
                                        sizes[-1] += pad
                                        dict_member.update({'_is_padded':True})
                                        dict_member.update({'_padded':pad})
                                    try:
                                        offset += sizes[-1]
                                    except IndexError:
                                        offset += 0
                                dict_member.update({'_offset':offset})
                                sizes += [40+24*dict_member['_ndims']]
                            else:
                                if dict_member.get('_is_char', False):
                                    try:
                                        offset += sizes[-1]
                                    except IndexError:
                                        offset += 0
                                    sizes += [dict_member['_size']*prod(dict_member['_dim'])]
                                else:
                                    dict_member.update({'_forced_pad':True})
                                    if padding and dict_member['_stride'] and dict_member['_size']:
                                        residue = sum(sizes)%dict_member['_stride']
                                        if residue and dict_member['_size'] > residue:
                                            if debug > 1 and not is_internal:
                                                print(f' >>> {_clr._bW}Padding{_clr.CLR_} {_clr._bC}{entry}%{_clr.CLR_}{_clr._bY}{name_member}{_clr.CLR_} by {_clr._bM}{dict_member["_stride"]-residue}{_clr.CLR_} bytes:')
                                                print(f' >>>     Size: {_clr._bM}{dict_member["_size"]}{_clr.CLR_}, Stride: {_clr._bM}{dict_member["_stride"]}{_clr.CLR_}, Residue: {_clr._bM}{residue}{_clr.CLR_}')
                                            pad = abs(dict_member['_stride'] - residue)
                                            sizes[-1] += pad
                                            dict_member.update({'_is_padded':True})
                                            dict_member.update({'_padded':pad})
                                    try:
                                        offset += sizes[-1]
                                    except IndexError:
                                        offset += 0
                                    sizes += [dict_member['_size']*prod(dict_member['_dim'])]
                        else:
                            if dict_member.get('_is_char', False):
                                if dict_member.get('_is_deferred_char', False):
                                    residue = sum(sizes)%dict_member['_stride']
                                    if padding and residue:
                                        if debug > 1:
                                            print(f' >>> {_clr._bW}Padding{_clr.CLR_} {_clr._bC}{entry}.{_clr.CLR_}{_clr._bY}{name_member}{_clr.CLR_} by {_clr._bM}{stride-residue}{_clr.CLR_}:')
                                        pad = abs(dict_member['_stride'] - residue)
                                        sizes[-1] += pad
                                        dict_member.update({'_is_padded':True})
                                        dict_member.update({'_padded':pad})
                                    try:
                                        offset += sizes[-1]
                                    except:
                                        offset += 0
                                    sizes += [8]
                                else:
                                    try:
                                        offset += sizes[-1]
                                    except IndexError:
                                        offset += 0
                                    sizes += [dict_member['_size']]
                                dict_member.update({'_offset':offset})
                            else:
                                no_padding = False
                                if all([ s==4 for s in sizes ]):
                                    no_padding = True
                                if dict_member.get('_is_pointer', False):
                                    dict_member.update({'_stride':8})
                                    dict_member.update({'_size':8})
                                    residue = sum(sizes)%dict_member['_stride']
                                    if padding and residue and not no_padding:
                                        if debug > 1:
                                            print(f' >>> {_clr._bW}Padding{_clr.CLR_} {_clr._bY}{entry}.{_clr.CLR_}{_clr._bR}{name_member}{_clr.CLR_} by {_clr._bM}{stride-residue}{_clr.CLR_}:')
                                        pad = abs(dict_member['_stride'] - residue)
                                        sizes[-1] += pad
                                        dict_member.update({'_is_padded':True})
                                        dict_member.update({'_padded':pad})
                                else:
                                    if dict_member.get('_is_derived', False):
                                        dict_member.update({'_is_pointer':False})
                                        dict_member.update({'_forced_pad':True})
                                        dict_member.update({'_stride':8})
                                        dict_member.update({'_size':8})
                                        residue = sum(sizes)%dict_member['_stride']
                                        if padding and residue:
                                            pad = abs(dict_member['_stride'] - residue)
                                            sizes[-1] += pad
                                            dict_member.update({'_is_padded':True})
                                            dict_member.update({'_padded':pad})
                                            if debug > 1:
                                                print(f' >>> {_clr._bW}Padding{_clr.CLR_} {_clr._bY}{entry}%{_clr.CLR_}{_clr._bR}{name_member}{_clr.CLR_} by {_clr._bM}{pad}{_clr.CLR_} bytes:')
                                    else:
                                        residue = sum(sizes)%dict_member['_stride']
                                        if residue and dict_member['_size'] > residue and not all([ s==4 for s in sizes ]):
                                            pad = abs(dict_member['_stride'] - residue)
                                            sizes[-1] += pad
                                            dict_member.update({'_is_padded':True})
                                            dict_member.update({'_padded':pad})
                                            if debug > 1 and not is_internal:
                                                print(f' >>> {_clr._bW}Padding{_clr.CLR_} {_clr._bC}{entry}%{_clr.CLR_}{_clr._bY}{name_member}{_clr.CLR_} by {_clr._bM}{pad}{_clr.CLR_} bytes:')
                                                print(f' >>>     Size: {_clr._bM}{size}{_clr.CLR_}, Stride: {_clr._bM}{dict_member["_stride"]}{_clr.CLR_}, Residue: {_clr._bM}{residue}{_clr.CLR_}')
                                try:
                                    offset += sizes[-1]
                                except IndexError:
                                    offset += 0
                                sizes += [dict_member['_size']]
                                if debug > 1 and not is_internal:
                                    print(f' >>> {_clr._bC}{entry}.{_clr.CLR_}{_clr._bY}{name_member}{_clr.CLR_} offset: {_clr._bM}{offset}{_clr.CLR_}')
                        dict_member.update({'_offset': offset})
                        fp.write('\n            ⟩')
                        name_member = ''
                    elif depth == 4:
                        if is_internal:
                            sbuff5 = ''
                            fp.write('\n                ⟫')
                            continue
                        sbuff4 = ''
                        lbuff5 = sbuff5.split()
                        self.__printSbuff(sbuff5, sbuff=5, depth=4, entry='arr03_of_int', debug=debug)
                        if len(lbuff5) == 6:
                            if lbuff5[0]+lbuff5[1] in selectcases:
                                dt = selectcases[lbuff5[0]+lbuff5[1]]
                                dict_member.update({'_type':dt})
                                dict_member.update({'_nbytes':int(lbuff5[1])})
                                dict_member.update({'_size':int(lbuff5[1])})
                                dict_member.update({'_stride':int(lbuff5[1])})
                                types2sizes.update({dt:int(lbuff5[1])})
                            else:
                                if lbuff5[0] == 'DERIVED' and lbuff5[-1] == 'DERIVED':
                                    dict_member.update({'_type_id':int(lbuff5[1])})
                                    dict_member.update({'_is_derived':True})
                                    dict_member.update({'_forced_pad':True})
                                    dict_member.update({'_size':8})
                                    dict_member.update({'_stride':8})
                                if lbuff5[0] == 'CLASS' and lbuff5[-1] == 'CLASS':
                                    dict_member.update({'_size':8})
                                    dict_member.update({'_stride':8})
                        if dict_entry.get('_is_char', False):
                            if len(lbuff5) == 2:
                                if lbuff5[1].startswith("'") and lbuff5[1].endswith("'"):
                                    dict_entry.update({'_length':int(lbuff5[1].strip("'"))})
                        if dict_member.get('_is_char', False):
                            if sbuff5.strip() == 'DEFERRED_CL':
                                dict_member.update({'_is_deferred_char':True})
                                dict_member.update({'_stride':8})
                                dict_member.update({'_size':8})
                            if len(lbuff5) == 3:
                                if lbuff5[-1] == 'DEFERRED':
                                    dict_member.update({'_ndims':int(lbuff5[0])})
                                    dict_member.update({'_is_deferred':True})
                                    dict_member.update({'_forced_pad':True})
                                    dict_member.update({'_stride':8})
                            if dict_member.get('_is_deferred', False):
                                if lbuff5[-1] == 'POINTER':
                                    dict_member.update({'_is_pointer':True})
                            if not dict_member.get('_is_deferred_char', False):
                                if len(sbuff5.split("'")[0].split()) == 2:
                                    try:
                                        if int(int(lbuff5[1])) != dict_member.get('_length', -1):
                                            print(f'\n >>> WARNING: Character length {name_member} goes wrong!')
                                        dict_member.update({'_value':sbuff5.split("'")[-2][:dict_member['_length']]})
                                    except:
                                        pass
                        else:
                            if len(lbuff5):
                                if lbuff5[-1] == 'DEFERRED':
                                    dict_member.update({'_is_deferred':True})
                                    dict_member.update({'_ndims':int(lbuff5[0])})
                                if lbuff5[-1] == 'POINTER':
                                    dict_member.update({'_is_pointer':True})
                        sbuff5 = ''
                        fp.write('\n                ⟫')
                    elif depth == 5:
                        if is_internal:
                            sbuff6 = ''
                            fp.write('\n                    >')
                            continue
                        sbuff5 = ''
                        lbuff6 = sbuff6.split()
                        self.__printSbuff(sbuff6, sbuff=6, depth=5, entry='arr03_of_int', debug=debug)
                        if dict_entry.get('_is_const', False) and dict_entry.get('_ndims', 0):
                            dt = lbuff4
                            hexadecimal = sbuff6.split()[-1].strip("'")
                            value = self.hexadecimal2number(hexadecimal, dict_entry['_type'])
                            dict_entry['_lbuff'] += [ value ]
                        if dict_entry.get('_no_member', False):
                            dict_member = {}
                        if dict_member.get('_ndims', 0):
                            if len(lbuff6) == 2:
                                if lbuff6[0].startswith("'") and lbuff6[0].endswith("'"):
                                    if lbuff6[1].startswith("'") and lbuff6[1].endswith("'"):
                                        shape = (int(lbuff6[0].strip("'")),int(lbuff6[1].strip("'")))
                                        if shape != dict_member['_dim']:
                                            print(f'\n >>> WARNING: The explicit shape of array {name_member} goes wrong!')
                                else:
                                    if lbuff6[1].startswith("'") and lbuff6[1].endswith("'"):
                                        dict_member['_lbuff'] += [ int(lbuff6[1].strip("'")) ]
                                        if not len(dict_member['_lbuff'])%2:
                                            dict_member.update({'_dim':tuple(dict_member['_lbuff'][i*2+1]-dict_member['_lbuff'][i*2]+1 for i in range(len(dict_member['_lbuff'][::2])))})
                                            if len(dict_member['_dim']) == dict_member['_ndims']:
                                                dict_member.pop('_lbuff')
                        fp.write('\n                    >')
                        sbuff6 = ''
                    elif depth == 6:
                        if is_internal:
                            sbuff7 = ''
                            fp.write('\n                        ”')
                            continue
                        sbuff6 = ''
                        lbuff7 = sbuff7.split()
                        if dict_member.get('_is_char'):
                            if len(lbuff7) == 8 and lbuff7[-1].startswith("'") and lbuff7[-1].endswith("'"):
                                dict_member.update({'_length':int(lbuff7[-1].strip("'"))})
                                dict_member.update({'_size'  :int(lbuff7[-1].strip("'"))})
                        fp.write('\n                        ”')
                        sbuff7 = ''
                    else:
                        pass
                else:
                    fp.write(c)
                if c not in ['(', ')']:
                    if depth == 0:
                        sbuff0 += c
                    elif depth == 1:
                        sbuff1 += c
                    elif depth == 2:
                        sbuff2 += c
                    elif depth == 3:
                        sbuff3 += c
                    elif depth == 4:
                        sbuff4 += c
                    elif depth == 5:
                        sbuff5 += c
                    elif depth == 6:
                        sbuff6 += c
                    else:
                        sbuff7 += c
        for k, v in _instances_derived_types.items():
            if is_internal:
                continue
            if type(v) is int:
                tmp = indices2entries.get(v, None)
                if tmp:
                    _instances_derived_types.update({k:tmp})
        self._instances_derived_types.update({basename:_instances_derived_types})
        derived_types = {}
        for k, v in dbuff.items():
            if type(v) is dict:
                if v.get('_is_derived', False):
                    derived_types.update({k:v})
        self._derived_types.update({basename:derived_types})
        def _findDerivedTypes(self, _type, _padding):
            _size_type = types2sizes[_type]
            for x in fortran_internals:
                if entry.startswith(x):
                    return 0
            if _type == '_tb_procedure':
                return 0
            for _k, _dict_subtype in dbuff[_type].items():
                if _k == '_tb_procedures':
                    continue
                if type(_dict_subtype) is dict:
                    _subtype = ids2types.get(_dict_subtype.get('_type_id', -1), _dict_subtype.get('_type', ''))
                    _dict_subtype.update({'_type':_subtype})
                    if _subtype in dbuff:
                        if _dict_subtype['_type'] != _subtype:
                            _dict_subtype.update({'_type_id':_dict_subtype['_type']})
                        _dict_subtype['_type'] = _subtype
                        if '_dim' in _dict_subtype:
                            size_array = prod(_dict_subtype['_dim'])
                            ndims = _dict_subtype['_ndims']
                        else:
                            size_array = 1
                            ndims = 1
                        if debug > 1:
                            print(f' >>>     {_clr._R}Old size{_clr.CLR_} of type {_clr._bC}{_clr._type}{_clr.CLR_}:', _size_type)
                        if _subtype == _type:
                            if '_ndims' in _dict_subtype:
                                if _dict_subtype.get('_is_deferred', False):
                                    _size_subtype = 48 + _dict_subtype['_ndims']*24 
                                    _size_subtype = 40 + _dict_subtype['_ndims']*24 
                            else:
                                if _dict_subtype.get('_is_pointer', False):
                                    _size_subtype = 8
                        else:
                            _was_subtype_padded = _dict_subtype.get('_is_padded', False)
                            _size_subtype = _findDerivedTypes(self, _subtype, _was_subtype_padded)
                            if _dict_subtype.get('_is_padded', False) and _size_subtype == 4:
                                _dict_subtype.update({'_is_padded':False})
                                _size_type -= 4
                                types2sizes[_type] -= 4
                            _dict_subtype.update({'_size_chunk':_size_subtype})
                        if _dict_subtype.get('_is_deferred', False):
                            size_array = 0
                        if _dict_subtype.get('_is_pointer', False) and not '_ndims' in _dict_subtype:
                            _size_subtype = 8
                            size_array = 0
                        if _size_subtype:
                            _size_type += (_size_subtype-8)*size_array
                        if debug > 1:
                            print(f' >>>     {_clr._G}New size{_clr.CLR_} of type {_clr._bC}{_clr._type}{_clr.CLR_}:', _size_type)
            residue = _size_type%8
            if _size_type == 4:
                _padding = False
            if _padding and residue and 8 > residue:
                _size_type += 8 - residue
                if debug > 1:
                    print(f' >>>     Padded {_clr._bC}{_clr._type}{_clr.CLR_} by {8-residue} bytes to final size {_clr._bM}{_clr._size_type}{_clr.CLR_}')
            dbuff[_type].update({'_size_chunk':_size_type})
            return _size_type
        for k, v in dbuff.items():
            if type(v) is dict:
                tp = ids2types.get(types2ids.get('_type', v.get('_type_id', -1)), '')
                if tp:
                    v.update({'_type':tp})
                    size_new = _findDerivedTypes(self, tp, padding)
                    v.update({'_size_chunk':size_new})
                size_derived = 0
                accumulation = 0
                was_derived = False
                if v.get('_is_derived', False) and not k.startswith('__'):
                    for kk, vv in v.items():
                        if kk == '_tb_procedures':
                            continue
                        if type(vv) is dict:
                            tp = ids2types.get(types2ids.get('_type', vv.get('_type_id', -1)), '')
                            if tp:
                                vv.update({'_type':tp})
                            if debug > 1:
                                print(f' >>> {_clr._R}Old offset{_clr.CLR_} of {_clr._C}{k}{_clr.CLR_}%{_clr._Y}{kk}{_clr.CLR_}: {_clr._M}{vv["_offset"]}{_clr.CLR_}', f'(accumulation = {accumulation})')
                            offset_assumed = vv['_offset'] + accumulation - vv.get('_padded', 0)
                            if vv['_type'] == '_tb_procedure':
                                residue = 0
                            else:
                                stride = 8#vv.get('_stride_max', 8)
                                residue = (offset_assumed)%stride
                            if vv.get('_is_padded') and residue:
                                if debug > 1:
                                    print(f' >>> {_clr._bM}Repadded{_clr.CLR_} {_clr._bY}"{kk}"{_clr.CLR_} by 4 because {size_derived}+{vv["_offset"]} has residue {residue}')
                                    print(f' >>> otherwise offset would be {offset_assumed} (stride = {vv["_stride"]})')
                                pad = abs(vv.get('_stride', 8) - residue)
                                vv['_offset'] += accumulation - vv.get('_padded', 0) + pad
                                accumulation += - vv.get('_padded', 0) + pad
                                if pad:
                                    vv.update({'_is_padded':True})
                                    vv.update({'_padded':pad})
                                else:
                                    vv.update({'_is_padded':False})
                                    vv.pop('padded', 0)
                            else:
                                if not vv.get('_forced_pad', False):
                                    vv['_offset'] += accumulation - vv.get('_padded', 0)
                                    accumulation -= vv.get('_padded', 0)
                                    vv.update({'_is_padded':False})
                                    vv.pop('padded', 0)
                                else:
                                    vv['_offset'] += accumulation
                            if debug > 1:
                                print(f' >>> {_clr._bG}New offset{_clr.CLR_} of {_clr._bC}{k}{_clr.CLR_}%{_clr._bY}{kk}{_clr.CLR_}: {_clr._bM}{vv["_offset"]}{_clr.CLR_}', f'(accumulation = {accumulation})')
                            if tp and kk != '_def_init':
                                vv.update({'_type':tp})
                                if '_dim' in vv:
                                    size_array = prod(vv['_dim'])
                                    ndims = vv['_ndims']
                                else:
                                    size_array = 1
                                    ndims = 1
                                if tp in dbuff:
                                    size_new = _findDerivedTypes(self, tp, padding)
                                    vv.update({'_size_chunk':size_new})
                                else:
                                    continue
                                    size_new = vv.get('_size', 0)#types2sizes.get(tp, 0)
                                if k == tp:
                                    size_new = types2sizes[tp]
                                size_derived = size_new*size_array
                                if vv.get('_is_deferred', False):
                                    ndims = vv['_ndims']
                                    size_derived = 0
                                    size_array = 0
                                if vv.get('_is_pointer', False) and not '_ndims' in vv:
                                    size_derived = 0
                                    size_array = 0
                                accumulation += size_derived
                                accumulation -= 8*size_array
                                if debug > 1:
                                    print(f' >>> {_clr._bG}New size{_clr.CLR_} of {_clr._bY}{k}{_clr.CLR_}.{_clr._bC}{tp}{_clr.CLR_}: {_clr._bM}{size_new}{_clr.CLR_} (size_derived = {size_derived}, accumulation = {accumulation})\n')
                                was_derived = True
                            else:
                                size_derived = 0
                                was_derived = False
        if debug:
            _printSummary(dbuff)
        dbuff.update({'_filename':modpath})
        object.__setattr__(self.modules, dbuff['_name'], dbuff)
        return dbuff
    def __printSbuff(self, s, sbuff=-1, depth=-1, entry='', debug=False):
        ccode = f'{_clr._bE}'
        if depth == 0:
            ccode = f'{_clr._W}'
        elif depth == 1:
            ccode = f'{_clr._R}'
        elif depth == 2:
            ccode = f'{_clr._Y}'
        elif depth == 3:
            ccode = f'{_clr._bY}'
        elif depth == 4:
            ccode = f'{_clr._G}'
        elif depth == 5:
            ccode = f'{_clr._C}'
        elif depth == 6:
            ccode = f'{_clr._B}'
        elif depth == 7:
            ccode = f'{_clr._M}'
        if entry == self.__entry or not entry:
            if debug:
                if sbuff < depth:
                    print(f' >>> DEBUG: entering {sbuff} -> {depth}, sbuff{sbuff} = {ccode}{s}{_clr.CLR_}')
                else:
                    print(f' >>> DEBUG: leaving {sbuff} -> {depth}, sbuff{sbuff} = {ccode}{s}{_clr.CLR_}')
    @property
    def derived_types(self):
         return list(self._derived_types.keys())
    @staticmethod
    def hexadecimal2number(sbuff, tp, return_ctype=False):
        from numpy import float16, float32, float64, int32, int64
        ctypes2ptypes = { c_int   : int32,
                          c_long  : int64,
                          c_float : float32,
                          c_double: float64,
                          c_bool  : lambda x:bool(int(x)),
                        }
        try:
            mbuff, ebuff = sbuff.split('@')
        except:
            return ctypes2ptypes[tp](sbuff)
        sign = ctypes2ptypes[tp](1.0)
        if mbuff.strip().startswith('-'):
            sign = ctypes2ptypes[tp](-1.0)
        ebuff = int(ebuff)
        point = mbuff.index('.')
        mbuff = mbuff[point+1:]
        mbuff = mbuff.ljust(ebuff, '0')
        hexadecimal = mbuff[:ebuff]+'.'+mbuff[ebuff:]
        try:
            result = ctypes2ptypes[tp](int(hexadecimal.split('.')[0], 16))
        except ValueError:
            result = ctypes2ptypes[tp](0.0)
        mantissa = hexadecimal.split('.')[1]
        for i, d in enumerate(mantissa):
            diviser = ctypes2ptypes[tp](16.0)**ctypes2ptypes[tp](i+1)
            result += ctypes2ptypes[tp](int(d,16))/diviser
        result *= sign
        return result
        if return_ctype:
            return tp(result)
        else:
            return tp(result).value
setters = {
            c_float : DL_DL.setFloat,
            c_double: DL_DL.setDouble,
            c_int   : DL_DL.setInt,
            c_long  : DL_DL.setLong,
            c_bool  : DL_DL.setBool,
            str     : DL_DL.setChar,
          }
