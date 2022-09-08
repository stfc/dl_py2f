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

def getDictEntryOLD(dictObj, key1, key2=None):
    '''Get entry from a multi-layer dictObj'''

    if key2:
        try:
            return dictObj[key1][key2]
        except:
            return None
#        return dictObj[key1][key2] if key2 in dictObj[key1] else None
    else:
        try:
            return dictObj[key1]
        except:
            return None
#        return dictObj[key1] if key1 in dictObj else None


# d = { 'a': 'A',
#       'b': { 'b1': 'B1',
#              'b2': { 'b21': 'B21',
#                      'b22': { 'b221': 'B221',
#                               'b222': 'B222',
#                             },
#                    }
#            },
#       'c': { 'c1': 'C1' }
#     }
# for example:
# getDictEntry(d, 'a'                 ), result: 'A'
# getDictEntry(d, 'b'  , 'b2'  , 'b21'), result: 'B21'
# getDictEntry(d, 'b2' , 'b21'        ), result: 'B21'
# getDictEntry(d, 'b22', 'b222'       ), result: 'B222'
# getDictEntry(d, 'b22', 'blabla'     ), result:  None
# getDictEntry(d, 'c1'                ), result: 'C1'
# getDictEntry(d, 'b2'                ), result: {'b21': 'B21', 'b22': {'b221': 'B221', 'b222': 'B222'}}
def getDictEntry(dictObj, *args):
    '''Get (recursively) the entry of a dictionary according to a single key or a series of keys'''

    # initialise buff
    buff = dictObj

    # loop over keys
    for key in args:
        try:
            # try to return val; return None if failed
            buff = buff.get(key)
            # dig into deeper levels
            if buff == None:
                raise AttributeError
        except AttributeError:
            # try recursively using the dict's values: this allows argument keys not starting from the topmost level
            for val in list(dictObj.values()):
                try:
                    buff = getDictEntry(val, *args)
                    # quit the current branch and explore more at the same level
                    if buff == None:
                        continue
                    return buff
                except:
                    continue

    return buff


# TODO expand to unlimited layers
def hasDictEntry(dictObj, key1, key2=None):
    '''Check entry from a multi-layer dictObj'''

    if key2:
        try:
            dictObj[key1][key2]
            return True
        except:
            return False
#        return True if key2 in dictObj[key1] else False
    else:
        try:
            dictObj[key1]
            return True
        except:
            return False
#        return True if key1 in dictObj else False



#class SelectCases(dictObj):
#    ''''''
#
#    def __setitem__(self, key, val):
#    print 'SET', key, val
#        dictObj.__setitem__(self, key, val)
#            setattr()

def getSortedKeys(dictObj, order=None):
    '''Return a list of keys by sorting the given dictObj'''

    # if order (a sequence such as tuple or list) in specified, use it to guide the sorting
    # example:
    #     dictObj = { 'a':1, 'b':2, 'c':3, 'd':4 }
    #     order = 'c', 'b'
    #     returned [ 'c', 'b', ... ]
    if order:
        return   [ key for key in order if key in dictObj ] \
               + [ key for key in dictObj if key not in order ]
    else:
        return sorted(list(dictObj.keys()))



#def getMerged(dictObj):


def dict2obj(d, obj):
    '''Recursively rebuild a ChemShell object with provided dictionary'''

    from . import objutils

    for key in obj._kwargs.keys():

        setattr(obj, key, d[key])



# TODO: unfinished...
def getDictEntry2(d, *args):
    ''''''

    

def getMatchedKey(d, arg):
    '''Return the key which contains fragment arg'''

    l = [ key for key in d.keys() if arg in key ]

    if len(l) == 1:
        return l[0]
    # if more than one match, return them as a list
    elif len(l) > 1:
        return l
    # if none matches, return arg
    else:
        return arg


def getValue(d, key, case_sensitive=True, raiseError=True):
    ''''''

    # byte
    try:
        key = key.decode()
    # str
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
    ''''''

    from numpy import array, char

    # byte
    try:
        key = key.decode()
    # str
    except:
        pass

    if not case_sensitive:
        keys = char.lower(array(list(d.keys())))
        return key.lower() in keys

    else:
        keys = array(list(d.keys()))
        return key in keys


def mergeDicts(d1, d2):
    '''Merge conterpart contents of d2 into d1, recursively'''

    from numpy import array, append

    for k, v in d2.items():
        if type(v) is dict:
            dbuff = d1.get(k, {})
            mergeDicts(dbuff, v)
            d1.update({k:dbuff})
        elif type(v) is str:
            pass
        # merge/update data: all valid data entries should be ndarrays only
        else:
            dt = v.dtype
            # always merge arrays; create an emtpy one if no such entry in d1
            d1.update({k:append(d1.get(k, array([], dtype=dt)), v)})


