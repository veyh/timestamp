#!/usr/bin/gawk -f
#######################################################################
# timestamp.awk
#
# Copyright (C) 2008-2011 Klaus Alexander Seistrup <klaus@seistrup.dk>
# Released under GPLv3+.
#######################################################################

BEGIN {
  # FIXME: How can I change this from the command line?
  FORMAT="%F %T"
}{
  print strftime(FORMAT) "\t" $0
}

# eof
