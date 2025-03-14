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
from struct import pack
def double2bytearray(d):
    return bytearray(pack('d', d))
def double2hexarray(d):
    ba = double2bytearray(d)
    return [ '0x%02x'%b for b in ba ]
def double2intarray(d):
    ha = double2hexarray(d)
    return [ int(h, base=16) for h in ha ]
def float2bytearray(f):
    return bytearray(pack('f', f))
def float2hexarray(f):
    ba = float2bytearray(f)
    return [ '0x%02x'%b for b in ba ]
def float2intarray(f):
    ha = float2hexarray(f)
    return [ int(h, base=16) for h in ha ]
def bool2bytearray(b):
    return bytearray(pack('?', b))
def bool2hexarray(b):
    ba = bool2bytearray(b)
    return [ '0x%02x'%b for b in ba ]
def bool2intarray(b):
    ha = bool2hexarray(b)
    return [ int(h, base=16) for h in ha ]
def int2bytearray(i):
    return bytearray(pack('i', i))
def int2hexarray(i):
    ba = int2bytearray(i)
    return [ '0x%02x'%b for b in ba ]
def int2intarray(i):
    ha = int2hexarray(i)
    return [ int(h, base=16) for h in ha ]
def long2bytearray(l):
    return bytearray(pack('l', l))
def long2hexarray(l):
    ba = long2bytearray(l)
    return [ '0x%02x'%b for b in ba ]
def long2intarray(l):
    ha = long2hexarray(l)
    return [ int(h, base=16) for h in ha ]
def char2bytearray(c):
    return bytearray(pack('c', c))
def char2hexarray(c):
    ba = char2bytearray(c)
    return [ '0x%02x'%b for b in ba ]
def char2intarray(c):
    ha = char2hexarray(c)
    return [ int(h, base=16) for h in ha ]
