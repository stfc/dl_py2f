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
import numpy
from . import iterutils
def ndarray(buffer, shape=None, dtype=None):
    if shape is None:
        shape = numpy.array(buffer).shape
    if dtype is None:
        dtype = numpy.array(buffer).dtype
    arr = numpy.ndarray(shape  = shape,
                        dtype  = dtype,
                        buffer = numpy.array(buffer))
    return numpy.ndarray(shape  = shape,
                         dtype  = dtype,
                         buffer = numpy.array(buffer))
def intarray(buffer, shape=None):
    if shape is None:
        shape = numpy.array(buffer).shape
    return numpy.ndarray(shape  = shape,
                         dtype  = numpy.int64,
                         buffer = numpy.array(numpy.int64(buffer)))
def indexarray(shape=None):
    if shape is None:
        shape = (1)
    return numpy.full(shape,
                      -1,
                      dtype=numpy.int64,
                     )
def setField(a, buffer, debug=0):
    if numpy.array(buffer).size != a.size and debug > 4:
        print('\n >>> WARNING: dl_py2f.utils.nputils.setField() failed to reshape input data of shape {} to {}'.format(numpy.array(buffer).shape, a.shape))
    a.setfield(numpy.array(buffer).reshape(a.shape), a.dtype)
def inplaceAdd(a, buffer):
    a.__iadd__(numpy.array(buffer).reshape(a.shape))
def inplaceMultiply(a, buffer):
    a.__imul__(buffer)
def map(a):
    if a.dtype.name == 'object':
        mapped = numpy.ndarray(shape=(a.shape[0], a[0].size))
        for i in range(a.shape[0]):
            mapped[i].setfield(a[i][:], dtype=a[0][0].dtype)
        return mapped
class RecArray(numpy.recarray):
    @property
    def defaults(self):
        return self._defaults
    @defaults.setter
    def defaults(self, val):
        self._defaults = val
    def __new__(cls, shape=None, dtype=None, buff=None, **kwargs):
        try:
            dtype = numpy.dtype(dtype)
            inst = numpy.recarray.__new__(cls, shape, dtype=dtype, buf=buff, **kwargs)
        except:
            buff = [ i[1] if isinstance(i[0], tuple) else
                     None for i in dtype ]
            dtype = [ i[0] if isinstance(i[0], tuple) else
                      i    if isinstance(i   , tuple) else
                      None for i in dtype ]
            inst = numpy.recarray.__new__(cls, shape, dtype=dtype, **kwargs)
            inst.defaults = dict(zip(inst.dtype.names, buff))
            inst.init()
        return inst
    def init(self):
        if not self.shape[0]:
            return
        for key, val in self.defaults.items():
            if hasattr(val, '__len__'):
                self.setField(val, field=key)
            else:
                self.field(key, val)
    def expand(self, N, axis=0):
        N = int(N)
        if N == self.size:
            return
        size = self.size
        self.resize(N, refcheck=False)
        if size:
            self[size:] = self[0]
        else:
            self.init()
    def setField(self, buff, field=None, factor=1.0):
        def _fill(_buff, _field):
            if hasattr(_buff, '__len__'):
                try:
                    self[_field].setfield(numpy.array(_buff).reshape(self[_field].shape), self[_field].dtype)
                except:
                    self[_field][:] = numpy.array(_buff)
            else:
                self[_field].setfield(numpy.full_like(self[_field], _buff).reshape(self[_field].shape), self[_field].dtype)
            if factor != 1.0:
                self[_field].__imul__(factor)
        if field:
            _fill(buff, field)
        elif not field and buff.dtype.fields:
            self.expand(buff.shape[0])
            for field in buff.dtype.fields.keys():
                _fill(buff[field], field)
        elif not field and not buff.dtype.fields:
            try:
                self[:] = buff[:]
            except ValueError:
                self.expand(len(buff[:]))
                for i in range(len(self[:])):
                    self[i] = buff[i,:] 
    def getDtype(self, field):
        return getattr(self, field).dtype.descr
class SymArray(numpy.ndarray):
    def __setitem__(self, indices, val):
        lbuff = [ i for i in indices[:-1] if isinstance(i, (int, list, tuple, numpy.ndarray)) ][::-1]
        lbuff.append(indices[-1])
        reversed = tuple(lbuff)
        super(SymArray, self).__setitem__(indices , val)
        super(SymArray, self).__setitem__(reversed, val)
def dtype2fmt(dt):
    from numpy import dtype, typecodes
    from .     import dictutils
    selectcases = {
                    typecodes['AllInteger']: 'd',
                    typecodes['AllFloat']  : 'f',
                    typecodes['Character'] : 's',
                    'U':                     's',
                    'S':                     's',
                  }
    return selectcases[dictutils.getMatchedKey(selectcases, dtype(dt).char)]
def tolist(arr):
    return bytes2str(arr).tolist()
def bytes2str(arr):
    from numpy import str as np_str
    if arr.dtype.kind == 'S':
        return arr.astype(np_str, copy=False)
    else:
        return arr
def getCountsA2InA1(a1, a2):
    from numpy import arange, array, bincount, isin, searchsorted, unique, zeros
    a1, a2 = array(a1), array(a2)
    if len(a2) != unique(a2).size:
        print("\n WARNING: getCountsA2InA1() must not contain repeat! Returning zeros array...\n")
        return zeros((a2.size,), dtype=int)
    masks_a1_in_a2 = isin(a1, a2)
    a1_in_a2 = a1[masks_a1_in_a2]
    sorter = a2.argsort()
    indices = sorter[searchsorted(a2, a1_in_a2, sorter=sorter)]
    return bincount(indices)
def getCountsA1InA2(a1, a2):
    from numpy import arange, array, bincount, isin, searchsorted, unique, zeros
    a1, a2 = array(a1), array(a2)
    if len(a2) != unique(a2).size:
        print("\n WARNING: getCountsA1InA2() must not contain repeat! Returning zeros array...\n")
        return zeros((a1.size,), dtype=int)
    counts = zeros((a1.size), dtype=int)
    masks_a1_in_a2 = isin(a1, a2)
    a1_in_a2 = a1[masks_a1_in_a2]
    unique_a1_in_a2 = unique(a1_in_a2)
    indices = searchsorted(unique_a1_in_a2, a1_in_a2)
    abuff = bincount(indices)
    counts[masks_a1_in_a2] = abuff[indices]
    return counts
def getIncrementsFromCounts(arr):
    if not len(arr):
        return numpy.zeros(arr.shape, dtype=int)
    unique_a, counts_a = numpy.unique(arr, return_counts=True)
    increments = numpy.concatenate(tuple(numpy.arange(c) for c in counts_a))
    sorter = arr.argsort().argsort()
    return increments[sorter]
def getGroupedIndices(arr):
    from numpy import arange, argsort, cumsum, diff, split
    sorter = argsort(arr)
    arr_sorted = arr[sorter]
    masks_leading = [True] + (arr_sorted[1:] != arr_sorted[:-1]).tolist()
    increments = diff(arange(arr.shape[0])[masks_leading])
    return split(sorter, cumsum(increments))
def getDTypeFromObj(obj, dictname='_kwargs'):
    kwargs = getattr(obj, dictname)
    fields = [ obj._synons[field] if field in obj._synons else
               field for field in iterutils.getFlattened((obj._fields,)) ]
    dtypes = [ [field, kwargs[field].dtype] if kwargs[field].ndim == 1 else
               [field, kwargs[field].dtype, kwargs[field].shape[1]] if kwargs[field].ndim == 2 else
               [field, kwargs[field].dtype, 0]
               for field in iterutils.getFlattened((obj._fields,)) ]
    dtypes = [ tuple([obj._synons[dtype[0]]]+dtype[1:]) if dtype[0] in obj._synons else
               tuple(dtype) for dtype in dtypes ]
    values = [ kwargs[field] for field in iterutils.getFlattened((obj._fields,)) ]
    return list(zip(dtypes, values))
