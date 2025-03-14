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
from . import miscutils
def getFlattenedIter(iter, sort=True):
    from itertools   import chain
    from collections import abc, OrderedDict
    if not hasattr(iter, '__len__'):
        iter = [ iter ]
    l = [ [ j for j in i ] if isinstance(i, abc.Iterable) and not isinstance(i, str) else
          [ i ] for i in iter ]
    if sort:
        return list(set(chain.from_iterable(l)))
    else:
        return list(OrderedDict.fromkeys(chain.from_iterable(l)))
def getFlattened(iter, unique=True):
    from numpy    import array, dtype, setdiff1d, __version__
    from warnings import filterwarnings
    from numpy.lib import NumpyVersion
    if NumpyVersion(__version__) >= '2.0.0b1':
        from numpy.exceptions import VisibleDeprecationWarning
    else:
        from numpy import VisibleDeprecationWarning
    if type(iter) in [dict]:
        return iter
    try:
        with filterwarnings('error', category=VisibleDeprecationWarning) as filter:
            try:
                abuff = array(iter)
            except:
                abuff = array(iter, dtype=object)
        if abuff.dtype != dtype('O'):
            return abuff.ravel().tolist()
    except:
        pass
    filterwarnings('error')
    try:
        flattened = type(iter)()
        type_iter = type(iter)
    except:
        flattened = list()
        type_iter = list
    for item in iter:
        if type(item) is type_iter:
            tmp = getFlattened(item, unique=unique)
            for t in tmp:
                if t in flattened and unique:
                    flattened.remove(t)
                flattened += type_iter([t])
        else:
            if unique:
                try:
                    item = setdiff1d(item, flattened)
                except RuntimeWarning:
                    item = [item]
                try:
                    flattened += type_iter(type_iter(item))
                except:
                    flattened += type_iter([item])
            elif not unique:
                try:
                    flattened += type_iter(type_iter(item))
                except:
                    flattened += type_iter([item])
    filterwarnings('default')
    return flattened
def getFlattenedOLD(iter, unique=True):
    flattened = type(iter)()
    for item in iter:
        if type(item) is type(iter):
            tmp = getFlattened(item)
            for t in tmp:
                if t in flattened:
                    flattened.remove(t)
                flattened += type(iter)([t])
        else:
            if unique and item not in flattened:
                flattened += type(iter)([item])
            elif not unique:
                flattened += type(iter)([item])
    return flattened
def findMapped(iter, mapping, case_sensitive=True):
    from numpy import array, char, isin
    iter = array(iter)
    try:
        if not case_sensitive:
            iter = char.lower(char.decode(iter))
        else:
            iter = char.decode(iter)
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
        mapped = keys_original[isin(keys,iter)]
    else:
        mapped = array([])
    return mapped
