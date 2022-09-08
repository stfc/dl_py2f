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

from . import miscutils

def getFlattenedIter(iter, sort=True):
    '''Return a flattened list, given a unsorted 1-level nested iterator'''

    from itertools   import chain
    from collections import abc, OrderedDict

    if not hasattr(iter, '__len__'):
        iter = [ iter ]

    # step 1: expand all iterables (excluding str!) and make non-iterables sublist;
    l = [ [ j for j in i ] if isinstance(i, abc.Iterable) and not isinstance(i, str) else
          [ i ] for i in iter ]

    # step 2: flatten with itertools.chain.from_iterable() and remove duplicates
    # do not try to sort, since the list can be unsortable (e.g., mixed strings and numbers)
    if sort:
        # set() removes repeats but reorders the sequence
        return list(set(chain.from_iterable(l)))
    else:
        return list(OrderedDict.fromkeys(chain.from_iterable(l)))


# YL 25/09/2020: added support for hybrid types
# YL could have been better implemented using more NumPy methods
# YL NB 27/11/2021: not suitable for large sequence because this function runs very slowly!
#@miscutils.time
def getFlattened(iter, unique=True):
    '''Recursively flatten a nested sequence (order and type kept)
       Examples:
           # (0, 98, 99, 100)
           getFlattened((arange(1), 98, [99,100]))
           # [0, 1, 2, 98, 99, 100]
           getFlattened(list((arange(3), 98, [99,100])))
           # [0, 1, 2, 98, 100, 3]
           getFlattened(list((arange(3), 98, [98,98,[100]], arange(4))))
           # [0, 1, 2, 98, 98, 98 100, 0, 1, 2, 3]
           getFlattened(list((arange(3), 98, [98,98,[100]], arange(4))), unique=False)
    '''

    from numpy    import array, dtype, setdiff1d, VisibleDeprecationWarning
    from warnings import filterwarnings

    # YL 15/12/2021: dictionary cannot be processed
    if type(iter) in [dict]:
        return iter

    # YL 20/08/2021: to improve the performance we directly return the result if the
    #                input iterable is already flattened, because array converted from
    #                uneven embedded list has a dtype 'O' (object); apply ravel() because, e.g.,
    #                array([[0],[1],[2],[3]]) also has a dtype 'int64'
    try:
        # YL 17/11/2021: to handle the warning issued by NumPy 19+
        #                "VisibleDeprecationWarning: Creating an ndarray from ragged nested sequences (which is a list-or-tuple of lists-or-tuples-or ndarrays with different lengths or shapes) is deprecated. If you meant to do this, you must specify 'dtype=object' when creating the ndarray"
        with filterwarnings('error', category=VisibleDeprecationWarning) as filter:
            try:
                abuff = array(iter)
            except:
                abuff = array(iter, dtype=object)
        # for cases other than ragged nested sequences 
        if abuff.dtype != dtype('O'):
            return abuff.ravel().tolist()
    # NB: array from embedded list of strings is not allowed in NumPy 1.14, e.g.,
    #     array(['_inherit', '_init', ['_fields']])
    except:
        pass

    filterwarnings('error')

    try:
        flattened = type(iter)()
        type_iter = type(iter)
    # YL 19/02/2021: numpy.ndarray() will fail due to missing of positional argument `shape`.
    #                we use list in this case
    except:
        flattened = list()
        type_iter = list

    for item in iter:
        if type(item) is type_iter:
            tmp = getFlattened(item, unique=unique)
            for t in tmp:
                # attach to the end and pop the existing one
                if t in flattened and unique:
                    flattened.remove(t)
                flattened += type_iter([t])
        else:
            # exclude duplicates
            if unique:
                try:
                    item = setdiff1d(item, flattened)
                # YL 20/10/2020: there will be a NumPy RuntimeWarning if `item` is unhashable
                except RuntimeWarning:
                    item = [item]
                try:
                    flattened += type_iter(type_iter(item))
                except:
                    flattened += type_iter([item])
            # not to exclude duplicates
            elif not unique:
                try:
                    flattened += type_iter(type_iter(item))
                except:
                    flattened += type_iter([item])

    filterwarnings('default')
    return flattened


# YL 25/09/2020: no longer in use
def getFlattenedOLD(iter, unique=True):
    '''Recursively flatten a nested sequence (order kept)'''

    flattened = type(iter)()

    for item in iter:
        if type(item) is type(iter):
            tmp = getFlattened(item)
            for t in tmp:
                # attach to the end and pop the existing one
                if t in flattened:
                    flattened.remove(t)
                flattened += type(iter)([t])
        else:
            # exclude duplicates
            if unique and item not in flattened:
                flattened += type(iter)([item])
            # not exclude duplicates
            elif not unique:
                flattened += type(iter)([item])
           
    return flattened


def findMapped(iter, mapping, case_sensitive=True):
    '''Translate an array/list of str/byte using the mapping dict'''

    from numpy import array, char, in1d

    iter = array(iter)

    # byte
    try:
        if not case_sensitive:
            iter = char.lower(char.decode(iter))
        else:
            iter = char.decode(iter)
    # str
    except:
        if not case_sensitive:
            iter = char.lower(iter)

    if mapping:
        if not case_sensitive:
            mapping_original, mapping = mapping, { k.lower():v for k,v in mapping.items() }
            keys_original = array(list(mapping_original.keys()))
        else:
            keys_original = array(list(mapping.keys()))
        keys   = array(list(mapping.keys()))
        mapped = keys_original[in1d(keys,iter)]

    else:
        mapped = array([])

    # return mapped keys
    return mapped


