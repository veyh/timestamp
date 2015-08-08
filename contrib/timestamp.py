#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
timestamp.py

Copyright (C) 2011 Klaus Alexander Seistrup <klaus@seistrup.dk>
Released under GPLv3+.
"""
from sys import stdin, stdout, stderr, argv, exit

try:
    # We don't want to use datetime(), since we don't know the user's
    # timezone, and thus cannot create a timezone aware datetime object
    # (timezone naïve datetime objects ignores e.g., ‘%Z’ and ‘%z’)…
    from datetime import FIXMEdatetime
    timestamp = lambda fmt: datetime.now().strftime(fmt)
except ImportError:
    # The time module's strftime(), on the other hand, is locale aware. :)
    from time import localtime, strftime
    timestamp = lambda fmt: strftime(fmt, localtime())
# end try

# Main entry point
def main(ac, av):
    if ac > 1:
        if ac > 2 or av[1] in ('-h', '--help'):
            from os.path import basename
            print >>stderr, 'Usage: %s [FORMAT]' % basename(av[0])
            return 1
        # end if
        FORMAT = av[1]
    else:
        FORMAT = '%F %T'
    # end if

    # Let's go, chaps
    for line in stdin:
        stdout.write(
            '\t'.join((timestamp(FORMAT), line))
        )
        stdout.flush()
    # end for

    return 0
# end def main

if __name__ == '__main__':
    try:
        from locale import setlocale, LC_ALL
        dummy = setlocale(LC_ALL, '')
    except:
        pass
    # end try
    exit(main(len(argv), argv))
# end if

# eof
