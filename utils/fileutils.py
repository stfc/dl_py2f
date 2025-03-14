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
def file2text(filename, case='input', error_return='', preceding_newline=False, trailing_newline=False, prefix=''):
    from os import path
    selectcases = {
                    'lower':str.lower,
                    'title':str.title,
                    'input':str
                  }
    filename0 = filename + ''
    filename = path.join(prefix, filename)
    try:
        with open(filename, 'r') as fp:
            strbuff = selectcases[case.lower()](fp.read())
        if preceding_newline:
            strbuff = '\n'+ strbuff
        if trailing_newline:
            strbuff = strbuff + '\n'
        return strbuff
    except (FileNotFoundError, IsADirectoryError, OSError):
        if error_return in [ False, None ]:
            raise FileNotFoundError(' >>> ERROR: file %s not found'%filename) from None
        elif error_return in [ '' ]:
            return filename0
        else:
            return error_return
def getBaseName(filename):
    from os.path import splitext
    return splitext(filename)[0]
def getExtension(filename, lower=True):
    from os import path
    filename = str(filename).lower()
    if path.splitext(filename)[1] != '':
        if lower:
            return path.splitext(filename)[1][1:].lower()
        else:
            return path.splitext(filename)[1][1:]
    else:
        return ''
def getExtensionWithDot(filename):
    from os.path import splitext
    if splitext(filename)[1]:
        return splitext(filename)[1]
    else:
        return None
def getOutName(filename):
    try:
        return getBaseName(filename) + ".out"
    except:
        raise IOError
def joinContinuedLines(fp, continuator='\\'):
    from os import SEEK_CUR
    fp.seek(fp.tell())
    line = fp.readline()
    while line.rstrip().endswith(continuator):
        line = ' '.join([line.rstrip().rstrip(continuator), fp.readline()])
    return line
def rename(filename, newname):
    from os import path, replace
    if path.isfile(filename):
        replace(filename, newname)
def archiveFile(filename, dir='.', copy=False, suffix='.OLD'):
    from os     import getcwd, listdir, path, sep
    from shutil import copyfile
    if type(filename) is tuple or type(filename) is list:
        for f in filename:
            archiveFile(f, dir=dir, suffix=suffix)
    elif type(filename) == str:
        if not path.isdir(dir):
            archiveDir(dir)
        dest = path.join(getcwd(), dir, filename.split(sep)[-1]+suffix)
        if copy:
            copyfile(filename, dest)
        else:
            rename(filename, dest)
def archiveDir(dirname, suffix='.OLD', mkdir=True):
    from shutil import rmtree
    from os     import mkdir as md
    from os     import replace
    rmtree(dirname+suffix, ignore_errors=True)
    try:
        replace(dirname, dirname+suffix)
    except FileNotFoundError:
        pass
    if mkdir:
        md(dirname)
def getChoppedMasks(filename, section_key, closing_key='', return_start_lines=False):
    from numpy import arange, array, append, char, searchsorted
    with open(filename, 'r') as fp:
        lines = array(fp.readlines())
        nlines = lines.size
        linenums = arange(nlines)
        linenums_mol = linenums[char.startswith(lines, section_key)]
        linenums_entries = linenums
        sorting_mol = searchsorted(linenums_entries, linenums_mol)
        lindices_mol = linenums_mol
        rindices_mol = linenums_entries[append(sorting_mol[1:]-1, linenums_entries.size-1)]
        masks = (lindices_mol[:,None]<=linenums)*(rindices_mol[:,None]>=linenums)
    if return_start_lines:
        return masks, lindices_mol
    else:
        return masks
def write(filename, buff):
    try:
        with open(filename, 'w') as fp:
            fp.write(buff)
    except:
        raise IOError
