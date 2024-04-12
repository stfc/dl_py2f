from struct import pack

def double2bytearray(d):
    '''Return a bytearray converted from the given double float (64-bit) number'''

    return bytearray(pack('d', d))


def double2hexarray(d):
    '''Return a string of hex representation of the given double float (64-bit) number'''

    ba = double2bytearray(d)

    return [ '0x%02x'%b for b in ba ]


def double2intarray(d):
    '''Return a list of small integers (0-255) interpreted from hex numbers of the given double (64-bit) float's bytes representation'''

    ha = double2hexarray(d)

    return [ int(h, base=16) for h in ha ]


def bool2bytearray(b):
    '''Return a bytearray converted from the given Boolean value (1 byte in Python)'''


    return bytearray(pack('?', b))


def bool2hexarray(b):
    '''Return a string of hex representation of the given Boolean value (1 byte)'''

    ba = bool2bytearray(b)

    # well, i know in Python or C it's only 1 byte...
    return [ '0x%02x'%b for b in ba ]


def bool2intarray(b):
    '''Return a list of small integers (0-255) interpreted from hex numbers of the given Boolean (1 byte) value's byte representation'''

    ha = bool2hexarray(b)

    # of course this is unnecessary because 1-byte hex array will be just [0] or [1]
    return [ int(h, base=16) for h in ha ]


def int2bytearray(i):
    '''Return a bytearray converted from the given integer (32-bit) number'''

    return bytearray(pack('i', i))


def int2hexarray(i):
    '''Return a string of hex representation of the given integer (32-bit) number'''

    ba = int2bytearray(i)

    return [ '0x%02x'%b for b in ba ]


def int2intarray(i):
    '''Return a list of small integers (0-255) interpreted from hex numbers of the given integer (32-bit) number's bytes representation'''

    ha = int2hexarray(i)

    return [ int(h, base=16) for h in ha ]


def char2bytearray(c):
    '''Return a bytearray converted from the given char (1 byte)'''

    return bytearray(pack('c', c))


def char2hexarray(c):
    '''Return a string of hex representation of the given char (1 byte)'''

    ba = char2bytearray(c)

    return [ '0x%02x'%b for b in ba ]


def char2intarray(c):
    '''Return a list of small integers (0-255) interpreted from hex numbers of the given char's (1 byte) bytes representation'''

    ha = char2hexarray(c)

    return [ int(h, base=16) for h in ha ]
