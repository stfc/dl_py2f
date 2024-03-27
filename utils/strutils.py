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

def str2num(s, delimiter=' '):
    '''Convert a string to int/float or numpy array'''

    # a scalar value
    try:
        try:
            return int(s)
        except ValueError:
            return float(s)

    # array
    except ValueError:

         from numpy    import fromstring
         from warnings import catch_warnings, simplefilter

         # AJL,Aug2017: Commenting this out but is there a way to control verbosity so this
         # can be left in for debugging level of output?
         # YL 28/11/2022: as of Python 3.10.6 and Numpy 1.23.5, a DeprecationWarning is thrown out for non-digital text
         with catch_warnings(record=True) as w:
             simplefilter('error')
             try:
                 return fromstring(s, sep=delimiter)
             except:
                 return []


def importAsModule(s):
    '''Try to <str> as <module>: when fails return <str>'''

    from importlib import import_module

    try:
#        module = __import__(s, fromlist=s)
        module = import_module(s)
    except OSError:
        print("\n DL_PY2F ERROR: Library", s, "is not correctly compiled/linked.\n")
        quit(999)

    except ImportError:
        module = s

    return module


# YL: strings are immutable in Python! we cannot change them as arguments!
def getJoined(s, bar, newline=True):
    '''Concatenate string and return new'''

    s += bar
    if newline:
        s += '\n'

    return s


class Str(bytearray):
    '''Mutable byte array'''

    # YL: do NOT use __new__(), otherwise encoding must be specified when creating an instance
    def __init__(self, val, *args):
        '''Initialise'''

        self = bytearray(val, 'ascii')


#    def concat(self, bar, newline=True):
#        '''Concatenate string and return new'''
#   
#        self += bar.encode()
#        if newline:
#            self += '\n'.encode()


    def __iadd__(self, *args):
        '''Override operator +='''
        # Usage:
        #   ending with new line (default):
        #       self += 'blabla'
        #   ending without new line:
        #       self += 'blabla', 'nonewline'

        # one argument
        try:
            self.extend((args[0]+'\n').encode())
        # two arguments
        except:
            if args[0][1] in [ 'nonewline', 'nonl' ]:
                self.extend(args[0][0].encode())
            else:
                self.extend((args[0][0]+'\n').encode())
            
        return self


    def clear(self):
        ''''''

        del self[:]


    @property
    def s(self):
        '''Convert to string'''
        return self.decode()


def getRightmost(s, separator='.', pos=0, tolerant=False):
    '''Get the right most component of a string split by separator'''

    try:
        return s.rsplit(separator, pos+1)[-pos-1]
    except:
        # if tolerant, try next pos when exceeds the bound
        if tolerant:
            return getRightmost(s, separator=separator, pos=pos-1, tolerant=tolerant)
        else:
            return ''


def split(s, separator='.', maxsplit=-1, tolerant=False):
    '''String plitter with switch of tolerance'''

    if s.split(separator, maxsplit=maxsplit)[0] == s and not tolerant:
        return ['', '']
    return s.split(separator, maxsplit=maxsplit)


def bytes2str(b):
    '''Always return str'''

    try:
        return b.decode()
    except:
        return str(b)


def str2bytes(s):
    '''Always return bytes'''

    # if already bytes
    if issubclass(type(s), bytes):
        return s
    try:
        return s.encode()
    except:
        return str(s).encode()


def inOrEq(s, ref):
    '''Check if a given str equals to a str type ref or in a list/array of refs'''

    result = False

    # byte
    try:
        s = s.decode().lower()
    except:
        s = s.lower()

    # str ref
    try:
        if s == ref.lower():
            result = True
    # list/array of refs
    except AttributeError:
        refs = [ r.lower() for r in ref ]
        if s in refs:
            result = True

    return result


def isStr(s):
    '''Return True if the given entity is str-like type'''

    from numpy import str_, string_

    if type(s) in [ str, str_, string_ ]:
        return True
    else:
        return False



