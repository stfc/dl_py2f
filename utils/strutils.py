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
def str2num(s, delimiter=' '):
    try:
        try:
            return int(s)
        except ValueError:
            return float(s)
    except ValueError:
         from numpy    import fromstring
         from warnings import catch_warnings, simplefilter
         with catch_warnings(record=True) as w:
             simplefilter('error')
             try:
                 return fromstring(s, sep=delimiter)
             except:
                 return []
def importAsModule(s):
    from importlib import import_module
    try:
        module = import_module(s)
    except OSError:
        print("\n DL_PY2F ERROR: Library", s, "is not correctly compiled/linked.\n")
        quit(999)
    except ImportError:
        module = s
    return module
def getJoined(s, bar, newline=True):
    s += bar
    if newline:
        s += '\n'
    return s
class Str(bytearray):
    def __init__(self, val, *args):
        self = bytearray(val, 'ascii')
    def __iadd__(self, *args):
        try:
            self.extend((args[0]+'\n').encode())
        except:
            if args[0][1] in [ 'nonewline', 'nonl' ]:
                self.extend(args[0][0].encode())
            else:
                self.extend((args[0][0]+'\n').encode())
        return self
    def clear(self):
        del self[:]
    @property
    def s(self):
        return self.decode()
def getRightmost(s, separator='.', pos=0, tolerant=False):
    try:
        return s.rsplit(separator, pos+1)[-pos-1]
    except:
        if tolerant:
            return getRightmost(s, separator=separator, pos=pos-1, tolerant=tolerant)
        else:
            return ''
def split(s, separator='.', maxsplit=-1, tolerant=False):
    if s.split(separator, maxsplit=maxsplit)[0] == s and not tolerant:
        return ['', '']
    return s.split(separator, maxsplit=maxsplit)
def bytes2str(b):
    try:
        return b.decode()
    except:
        return str(b)
def str2bytes(s):
    if issubclass(type(s), bytes):
        return s
    try:
        return s.encode()
    except:
        return str(s).encode()
def inOrEq(s, ref):
    result = False
    try:
        s = s.decode().lower()
    except:
        s = s.lower()
    try:
        if s == ref.lower():
            result = True
    except AttributeError:
        refs = [ r.lower() for r in ref ]
        if s in refs:
            result = True
    return result
def isStr(s):
    from numpy import str_, string_
    if type(s) in [ str, str_, string_ ]:
        return True
    else:
        return False
