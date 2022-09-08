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

import numpy

def ndarray(buffer, shape=None, dtype=None):
    '''Create an instance of <numpy.ndarray> and initialise it with data contained in buffer (NOTE: shape kept!)'''

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
    '''Create an instance of <numpy.ndarray> for integers and initialise it with data contained in buffer (NOTE: shape kept!)'''

    if shape is None:
        shape = numpy.array(buffer).shape

    return numpy.ndarray(shape  = shape,
                         dtype  = numpy.int64,
                         buffer = numpy.array(numpy.int64(buffer)))


def indexarray(shape=None):
    '''Create an instance of <numpy.ndarray> for integers and fill with -1'''

    if shape is None:
        shape = (1)

    return numpy.full(shape,
                      -1,
                      dtype=numpy.int64,
                     )


def setField(a, buffer, debug=0):
    '''Set values of <numpy.ndarray> 'a', with memory address retained'''

    # YL 23/06/2020: print a bit debug information
    if numpy.array(buffer).size != a.size and debug > 4:
        print('\n >>> WARNING: dl_py2f.utils.nputils.setField() failed to reshape input data of shape {} to {}'.format(numpy.array(buffer).shape, a.shape))
    
    #a.setflags(write=True)
    a.setfield(numpy.array(buffer).reshape(a.shape), a.dtype)
    #a.setflags(write=True)


def inplaceAdd(a, buffer):
    '''Do in-place addition'''

#    a.flags.writeable = True
    a.__iadd__(numpy.array(buffer).reshape(a.shape))
#    a.flags.writeable = False


def inplaceMultiply(a, buffer):
    '''Do in-place multiplication'''

    a.__imul__(buffer)


def map(a):
    ''''''
    if a.dtype.name == 'object':

        mapped = numpy.ndarray(shape=(a.shape[0], a[0].size))

        for i in range(a.shape[0]):
            # does NOT work: mapped[:].setfield(a[:][:], dtype=a[0][0].dtype)
            mapped[i].setfield(a[i][:], dtype=a[0][0].dtype)

        return mapped


# strictly speaking, numpy.recarray and structred array are different things
# see: http://wiki.scipy.org/Cookbook/Recarray
class RecArray(numpy.recarray):
    '''Overloaded numpy.recarray'''

    @property
    def defaults(self):
        '''Default values for expanding when initial size is zero'''
        return self._defaults

    @defaults.setter
    def defaults(self, val):
        '''Setter of defaults'''
        self._defaults = val

    # tried __init__ but did not succeed
    def __new__(cls, shape=None, dtype=None, buff=None, **kwargs):
        ''''''

        # TODO: develop for more types of dtype
        # without initialiser
        try:
            dtype = numpy.dtype(dtype)
            inst = numpy.recarray.__new__(cls, shape, dtype=dtype, buf=buff, **kwargs)

        # with initialiser: e.g., dtype = [ (('abc', numpy.int64), 123) ] or [ (('abc', numpy.int64, 2), [1,2]) ]
        except:
            # fist fills then dtype because dtype will be redefined
            buff = [ i[1] if isinstance(i[0], tuple) else
                     None for i in dtype ]
            dtype = [ i[0] if isinstance(i[0], tuple) else
                      i    if isinstance(i   , tuple) else
                      None for i in dtype ]
            # no 'buf' but fill with values after creating
            inst = numpy.recarray.__new__(cls, shape, dtype=dtype, **kwargs)

            # save the default values that may be used for future expanding as by creation the array size could be zero (no way to initialise)
            inst.defaults = dict(zip(inst.dtype.names, buff))

            inst.init()

#        else:
#            print("\n\n >>> ChemShError: RecArray instantiation failed.\n\n")

        return inst

    def init(self):
        '''Initialise according to self.defaults'''

        # fill in initialising values if size is not zero
        # TODO: add type check
        if not self.shape[0]:
            return

        for key, val in self.defaults.items():
            # array
            if hasattr(val, '__len__'):
                self.setField(val, field=key)
            # scalar (may be None)
            else:
                self.field(key, val)


    def expand(self, N, axis=0):
        '''Expand array by N-folder using numpy.repeat'''

        # YL 16/08/2019: N may be numpy.integer
        N = int(N)
#        print("### nparray.RecArray: N =", N)
#        if not isinstance(N, int):
#            print("### nparray.RecArray.expand: N must be an integer")
#            raise TypeError

        if N == self.size:
            return

        # initial size before resizing
        size = self.size

        # use refcheck=False to avoid "ValueError: cannot resize an array references or is referenced"
        self.resize(N, refcheck=False)

        # if initial size > 0
        if size:
            # fill in the new array with previous values
            # YL 18/01/2018: self.repeat(N, axis=axis) was a bug that could cause serious out-of-memory problem because NxN array was created.
            #                repeat/tile is not necessary when an object's master array is expanded, because only few cases, e.g., frag.conn, require initial values
#            self.put(range(N), self.repeat(N, axis=axis))
            self[size:] = self[0]

        # if initial size is 0
        else:
            self.init()


    # YL: keep the conditional branching as flat as possible!
    def setField(self, buff, field=None, factor=1.0):
        '''Set values in place'''

        def _fill(_buff, _field):
            # array buffer
            if hasattr(_buff, '__len__'):
                # if buff fills the whole self[_field]
                try:
                    self[_field].setfield(numpy.array(_buff).reshape(self[_field].shape), self[_field].dtype)
                # if buff fills a row of self[_field]
                except:
                    self[_field][:] = numpy.array(_buff)
            # scalar buffer
            else:
                self[_field].setfield(numpy.full_like(self[_field], _buff).reshape(self[_field].shape), self[_field].dtype)

            if factor != 1.0:
                self[_field].__imul__(factor)

        # fill in a single field
        # do NOT resize at this level because it can be dangerous! instead, check the size at the I/O level
        if field:
            _fill(buff, field)

        # fill in all fields with a structured buff
        elif not field and buff.dtype.fields:
            self.expand(buff.shape[0])
            for field in buff.dtype.fields.keys():
                _fill(buff[field], field)

        # if buff not structured
        elif not field and not buff.dtype.fields:
            # fill row by row
            try:
                self[:] = buff[:]
            # when size does not match
            except ValueError:
                self.expand(len(buff[:]))
                for i in range(len(self[:])):
                    self[i] = buff[i,:] 


    def row2str(self):
        '''Convert row to string'''
    

# NOT IN USE
    def getDtype(self, field):
        '''Return dtype.descr'''
        return getattr(self, field).dtype.descr


class ChemShArray(numpy.ma.MaskedArray):
    ''''''


class SymArray(numpy.ndarray):
    '''Symmetric square matrix'''

    def __setitem__(self, indices, val):
        '''Overload the default setitem() method to realise automated symmetric assignment'''

	# make a reversed tuple of indices but remain slice(None, None, None) (for integer indices) or 1 (for list/array indices) as the last item
        lbuff = [ i for i in indices[:-1] if isinstance(i, (int, list, tuple, numpy.ndarray)) ][::-1]
        lbuff.append(indices[-1])
        reversed = tuple(lbuff)

        super(SymArray, self).__setitem__(indices , val)
        super(SymArray, self).__setitem__(reversed, val)


#def type2dtype(obj, dtype):
#    ''''''
#
#    selectcases = {
#                    str  :numpy.U,
#                    int  :numpy.int,
#                    float:numpy.float
#                  }


def dtype2fmt(dt):
    '''Find the corresponding Python print format for a NumPy dtype'''

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
    '''A wrapper of ndarray.tolist()'''

    # FIXME: always convert bytes array to string array
    return bytes2str(arr).tolist()


def bytes2str(arr):
    '''Convert bytes array to string array'''

    from numpy import str as np_str

    if arr.dtype.kind == 'S':
        return arr.astype(np_str, copy=False)
    else:
        return arr


# YL 15/02/2021
def getCountsA2InA1(a1, a2):
    '''Return an array (of same size as a2) of counts that a2 elements occur in a1 (a2 must not contain repeats!)'''

    from numpy import arange, array, bincount, in1d, searchsorted, unique, zeros

    a1, a2 = array(a1), array(a2)

    # check repeat in a2
    if len(a2) != unique(a2).size:
        print("\n WARNING: getCountsA2InA1() must not contain repeat! Returning zeros array...\n")
        return zeros((a2.size,), dtype=int)

    masks_a1_in_a2 = in1d(a1, a2)
    a1_in_a2 = a1[masks_a1_in_a2]

    # assuming no repeat in a2!
    sorter = a2.argsort()

    # so far we have construct an array that a2[indices]==a1_in_a2 elementwise
    indices = sorter[searchsorted(a2, a1_in_a2, sorter=sorter)]

    # count each sorting positions
    return bincount(indices)


# YL 15/02/2021
def getCountsA1InA2(a1, a2):
    '''Return an array (of same size as a1) of each a1 element's occuring frequency in a2 (a2 must not contain repeats!)'''

    from numpy import arange, array, bincount, in1d, searchsorted, unique, zeros

    a1, a2 = array(a1), array(a2)

    # check repeat in a2
    if len(a2) != unique(a2).size:
        print("\n WARNING: getCountsA1InA2() must not contain repeat! Returning zeros array...\n")
        return zeros((a1.size,), dtype=int)

    # start with an empty array of counts
    counts = zeros((a1.size), dtype=int)

    masks_a1_in_a2 = in1d(a1, a2)
    a1_in_a2 = a1[masks_a1_in_a2]

    # remove repeats in a1_in_a2
    unique_a1_in_a2 = unique(a1_in_a2)

#    # sort
#    sorter = unique_a1_in_a2.argsort()

    # remember the positions for restoring from the unique array
    # so far we have construct an array that unique_a1_in_a2[indices]==a1_in_a2 elementwise
    indices = searchsorted(unique_a1_in_a2, a1_in_a2)

    # this is a counts array of unique_a1_in_a2 occurring in a2
    # (the counts array of a1_in_a2 occurring in a2 is thus restored by abuff[indices])
    abuff = bincount(indices)

    # assign count values to where a1 elements appear in a2
    counts[masks_a1_in_a2] = abuff[indices]

    return counts
    

# YL 12/03/2021: for a use case see FragmentBase.connectIJs()
def getIncrementsFromCounts(arr):
    '''Return an array of positional increments according to the counts of given array's elements, for example
       [3504, 3504, 3508, 3509, 3508, 3504] will return [0, 1, 0, 0, 1, 2]
    '''

    if not len(arr):
        return numpy.zeros(arr.shape, dtype=int)

    # get the sorted counts
    unique_a, counts_a = numpy.unique(arr, return_counts=True)

    # construct a sorted array of increments
    increments = numpy.concatenate(tuple(numpy.arange(c) for c in counts_a))

    # restore the order with a sorter of sorter
    sorter = arr.argsort().argsort()

    return increments[sorter]







