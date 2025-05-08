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
def getDictEntryOLD(dictObj, key1, key2=None):
    if key2:
        try:
            return dictObj[key1][key2]
        except:
            return None
    else:
        try:
            return dictObj[key1]
        except:
            return None
def getDictEntry(dictObj, *args):
    buff = dictObj
    for key in args:
        try:
            buff = buff.get(key)
            if buff == None:
                raise AttributeError
        except AttributeError:
            for val in list(dictObj.values()):
                try:
                    buff = getDictEntry(val, *args)
                    if buff == None:
                        continue
                    return buff
                except:
                    continue
    return buff
def hasDictEntry(dictObj, key1, key2=None):
    if key2:
        try:
            dictObj[key1][key2]
            return True
        except:
            return False
    else:
        try:
            dictObj[key1]
            return True
        except:
            return False
def getSortedKeys(dictObj, order=None):
    if order:
        return   [ key for key in order if key in dictObj ] \
               + [ key for key in dictObj if key not in order ]
    else:
        return sorted(list(dictObj.keys()))
def dict2obj(d, obj):
    from . import objutils
    for key in obj._kwargs.keys():
        setattr(obj, key, d[key])
def getMatchedKey(d, arg):
    l = [ key for key in d.keys() if arg in key ]
    if len(l) == 1:
        return l[0]
    elif len(l) > 1:
        return l
    else:
        return arg
def getValue(d, key, case_sensitive=True, raiseError=True):
    try:
        key = key.decode()
    except:
        pass
    try:
        if not case_sensitive:
            mapping = { k.lower():v for k,v in d.items() }
            return mapping[key.lower()]
        else:
            return mapping[key]
    except:
        if raiseError:
            raise
        else:
            return None
def isKeyOf(d, key, case_sensitive=True):
    from numpy import array, char
    try:
        key = key.decode()
    except:
        pass
    if not case_sensitive:
        keys = char.lower(array(list(d.keys())))
        return key.lower() in keys
    else:
        keys = array(list(d.keys()))
        return key in keys
def mergeDicts(d1, d2):
    from numpy import array, append
    for k, v in d2.items():
        if type(v) is dict:
            dbuff = d1.get(k, {})
            mergeDicts(dbuff, v)
            d1.update({k:dbuff})
        elif type(v) in [ str, float, int, bool ]:
            pass
        else:
            dt = v.dtype
            d1.update({k:append(d1.get(k, array([], dtype=dt)), v)})
