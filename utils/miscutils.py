'''Miscellaneous utility tools'''

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

# NOTE: should NOT use unless debug
def addWeakRef(obj):
    '''Try to make all attribute objects remember their base object (recursively)'''
    from weakref import ref
    for attr in dir(obj):
        # do not check utils.getType to avoid initialising modules which could not be ready yet
        if not attr.startswith('_') and not callable(getattr(obj, attr)):
            try:
                setattr(getattr(obj, attr), '_b_weakref_', ref(obj))
                addWeakRef(getattr(obj, attr))
            except:
                pass


# NOTE: should NOT use unless debug
def setBaseObj(obj, base):
    from weakref import ref
    setattr(obj, '_b_weakref_', ref(base))


def getBaseClasses(cls, reverse=True):
    ''''''
    if reverse:
        try:
            return  getBaseClasses(cls.__bases__[0], reverse=reverse) + cls.__bases__
        except:
            return  cls.__bases__
    else:
        try:
            return  cls.__bases__ + getBaseClasses(cls.__bases__[0], reverse=reverse)
        except:
            return  cls.__bases__


# NOTE: should NOT use unless debug
def getBaseObj(obj):
    '''Return top-level of base object'''
    try:
        return getBaseObj(obj._b_weakref_.__call__())
    except:
        return obj._b_weakref_.__call__()


# NOTE: should NOT use unless debug
def hasBaseObj(obj):
    ''''''
    try:
        getBaseObj(obj)
        return True
    except:
        return False





# doesn't work in our case
#def str2class(str):
#    '''Convert a string to class; return None if failed'''
#
#    # in Python 3, reduce() is moved to functools
#    # TODO: may use for loop instead to save importing functools
#    import functools, sys
#    return functools.reduce(getattr, str.split("."), sys.modules[__name__])


# NOTE: should NOT use unless debug
def getCaller(levels=1):
    '''Get caller's name'''

    from inspect import stack

    sbuff = '\n'

    stacks = stack()

    sbuff += ' function     : {}()\n'.format(stacks[2].function)
    sbuff += ' filename     : {}\n'.format(stacks[2].filename)
    sbuff += ' line {:<5d}   : {}\n\n'.format(stacks[2].lineno, stacks[2].code_context[0].strip())
    for l in (range(3,3+levels+1)):
        try:
            sbuff += ' from function: {}()\n'.format(stacks[l].function)
            sbuff += ' filename     : {}\n'.format(stacks[l].filename)
            sbuff += ' line {:<5d}   : {}\n\n'.format(stacks[l].lineno, stacks[l].code_context[0].strip())
        except (IndexError, TypeError):
            continue

    return sbuff
#    return stack()[lvl+1][3]


def time(proc):
    '''Evaluate elapsed time of running a procedure'''

    from sys  import stdout
    from time import time

    # wrapper of measured procedure
    def wrapper(*args, **kwargs):

        tstart = time()
        result = proc(*args, **kwargs)
        telapsed = time() - tstart

        # if no mute=True
        if not kwargs.get('mute', False):
            print(" >>> Elapsed time of procedure %s: %.3f sec"%(proc.__module__+'.'+proc.__name__, telapsed))
            stdout.flush()

        return result

    return wrapper


def memusage(proc):
    '''Print the peak memory usage in MB'''

    from resource import getrusage, RUSAGE_SELF

    # wrapper of measured procedure
    def wrapper(*args, **kwargs):

        result = proc(*args, **kwargs)

        # TODO: KBytes on Linux and Bytes on OS X
        if not kwargs.get('mute', False):
            print(" >>> Peak memory used by procedure %s: %.3f MB"%(proc.__module__+'.'+proc.__name__, getrusage(RUSAGE_SELF).ru_maxrss/1024))

        return result

    return wrapper


def doNothing(*args):
    '''Dummy function of doing nothing'''

    return args


def getPrecision(fmt):
    '''Return the printing precison of a format string for float (e.g., '%f16.10f' gives 10)'''

    return int(fmt.split('.')[1].rstrip('f'))


def runSysCmd(syscmd, stdinp='', stdout='', stderr='', shell=True, check=True, exit_by_error=True, comm=False):
    '''Execute a system command'''

    import sys
    from subprocess import Popen, run
    from warnings   import catch_warnings, resetwarnings, simplefilter

    # YL 07/03/2021: enabled redirecting stderr to a file but not suitable to use `with open(...) as ...` here
    try:
        fp_o = open(stdout, 'w')
    except:
        fp_o = sys.stdout
    try:
        fp_e = open(stderr, 'w')
    except:
        fp_e = sys.stderr
    try:
        fp_i = open(stdinp, 'r')
    except:
        fp_i = None

    if shell:
        cmd_run = syscmd
    else:
        cmd_run = syscmd.split()

    if comm:
        with Popen(cmd_run, stdin=fp_i, stdout=fp_o, stderr=fp_e, shell=shell) as popen:
            outs, ierror = popen.communicate()

    else:
        # to suppress DeprecationWarning
        with catch_warnings() as w:
            simplefilter('ignore')
            sys.stderr.flush()
            sys.stdout.flush()
            completedproc = run(cmd_run,
                                shell=shell,
                                check=check,
                                stdout=fp_o,
                                stderr=fp_e)
            sys.stderr.flush()
            sys.stdout.flush()
        resetwarnings()

    # do not close sys.stdout!
    if fp_o != sys.stdout:
        fp_o.close()

    # do not close sys.stderr!
    if fp_e != sys.stderr:
        fp_e.close()

    # do not close sys.stderr!
    if fp_i and not fp_i.closed:
        fp_e.close()

    if (comm and ierror) or (not comm and completedproc.returncode):
        print(f"\n >>> Failed to run system command:\n {syscmd}\n")
        if stdout or stderr:
            print(' See files: {} {}'.format(stdout, stderr))
        if exit_by_error:
            exit(999)
        else:
            return 999
    else:
        print(f"\n >>> Successfully ran system command:\n {syscmd}\n")
        return 0


def isInteger(i):
    '''Return True if `i` is a Python or NumPy integer; False otherwise'''

    from numpy import int_

    if isinstance(i, int_) or isinstance(i, int):
        return True
    else:
        return False




