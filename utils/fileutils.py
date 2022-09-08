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

def file2text(filename, case='input', error_return='', preceding_newline=False, trailing_newline=False, prefix=''):
    '''Read contents of a text file into a <str> variable'''

    from os import path

    selectcases = {
                    'lower':str.lower,
                    'title':str.title,
                    'input':str
                  }

    # YL 23/11/2017: `filename` should not be blank otherwise "../" is wrongly returned
    filename0 = filename + ''

    # YL 13/08/2022: path prefix
    filename = path.join(prefix, filename)

    try:
        with open(filename, 'r') as fp:
            # get string and convert case
            strbuff = selectcases[case.lower()](fp.read())

        if preceding_newline:
            strbuff = '\n'+ strbuff

        if trailing_newline:
            strbuff = strbuff + '\n'
    
        return strbuff

    # return error_return or filename if failed
    except (FileNotFoundError, IsADirectoryError, OSError):

        # raises error if `error_return` is False or None
        if error_return in [ False, None ]:
            raise FileNotFoundError(' >>> ERROR: file %s not found'%filename) from None

        # return filename (without prefix) by default
        elif error_return in [ '' ]:
            return filename0

        # returns given argument value otherwise
        else:
            return error_return



def getBaseName(filename):
    '''Return the basename (stem)'''

    from os.path import splitext

    return splitext(filename)[0]


def getExtension(filename, lower=True):
    '''Return the extension name (without dot)'''

    from os import path

    filename = str(filename).lower()
    if path.splitext(filename)[1] != '':
        if lower:
            return path.splitext(filename)[1][1:].lower()
        else:
            return path.splitext(filename)[1][1:]
    else:
        return ''


# not in use
def getExtensionWithDot(filename):
    '''Return the extension (suffix) with dot'''

    from os.path import splitext

    if splitext(filename)[1]:
        return splitext(filename)[1]
    else:
        return None


def getOutName(filename):
    '''Return output filename according to given one: e.g., filename.in -> filename.out'''

    try:
        return getBaseName(filename) + ".out"
    except:
        raise IOError


# cannot be used within `for line in fp:` context where next(fp) is implicated
def joinContinuedLines(fp, continuator='\\'):
    '''Join continued lines'''

    from os import SEEK_CUR

    fp.seek(fp.tell())
    
    line = fp.readline()
    while line.rstrip().endswith(continuator):
        line = ' '.join([line.rstrip().rstrip(continuator), fp.readline()])

    return line


def rename(filename, newname):
    '''Rename a file'''

    from os import path, replace

    if path.isfile(filename):
        # os.rename() works only on UNIX and fails on Windows
        replace(filename, newname)


def archiveFile(filename, all=False, dir='.', copy=False, suffix='.OLD'):
    '''Rename a file to extension .OLD'''

    from os     import getcwd, listdir, path, sep
    from shutil import copyfile

    # support a list or tuple of filenames
    if type(filename) is tuple or type(filename) is list:
        for f in filename:
            archiveFile(f, all=all, dir=dir, suffix=suffix)

    # singular filename of string
    else:
        # new filename
        if not path.isdir(dir):
            archiveDir(dir)
        dest = path.join(getcwd(), dir, filename.split(sep)[-1]+suffix)
        if copy:
            copyfile(filename, dest)
        else:
            rename(filename, dest)


def archiveDir(dirname, suffix='.OLD'):
    '''Create a directory for archives'''

    from shutil import rmtree
    from os     import mkdir, replace

    # remove existing .OLD dir
    rmtree(dirname+suffix, ignore_errors=True)

    # rename with .OLD
    try:
        replace(dirname, dirname+suffix)
    except FileNotFoundError:
        pass

    # create
    mkdir(dirname)


## YL NB: not suitable for getting arrays of complex dtype
#def getArrayFromFile(filename, fmt, shape, regex=None, number_only=True):
#    '''Read contents of a text file into a <numpy.ndarray>'''
#
#    from numpy import zeros
#    from ..io  import file
#
#    abuff = zeros(shape)
#
#    # YL 30/09/2020: also get the returned data array
#    abuff = file.parse(filename, fmt, receiver=abuff, regex=regex, number_only=number_only)
#
#    return abuff


def getChoppedMasks(filename, section_key, closing_key='', return_start_lines=False):
    '''Chop a file into pieces according to the section head keyword and return a masks array marking the lines, for example:
           file content (11 lines):
               # title
               molecule
               Cl 0.0 0.0 0.0
               H  1.0 0.0 0.0
               end molecule

               molecule
               O  0.0 0.0 0.0
               H  1.0 0.0 0.0
               H -1.0 0.0 0.0
               end molecule
           this function returns masks of two sections each :
               [[False True  True  True  True  True  False False False False False]
                [False False False False False False True  True  True  True  True]]
    '''

    from numpy import arange, array, append, char, searchsorted

    with open(filename, 'r') as fp:

        # read in all lines
        lines = array(fp.readlines())
        nlines = lines.size
        linenums = arange(nlines)

        # find the lines that start with the given key
        linenums_mol = linenums[char.startswith(lines, section_key)]

        # TODO
        linenums_entries = linenums

        # find the positions of the key
        sorting_mol = searchsorted(linenums_entries, linenums_mol)

        # starting indices
        lindices_mol = linenums_mol
        # closing indices
        rindices_mol = linenums_entries[append(sorting_mol[1:]-1, linenums_entries.size-1)]

        # masks
        masks = (lindices_mol[:,None]<=linenums)*(rindices_mol[:,None]>=linenums)

    if return_start_lines:
        return masks, lindices_mol
    else:
        return masks


def write(filename, buff):
    '''Write out a file with given string buff'''

    try:
        with open(filename, 'w') as fp:
            fp.write(buff)
    except:
        raise IOError







