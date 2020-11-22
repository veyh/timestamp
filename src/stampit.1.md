STAMPIT(1)	"V0.3.2"

# NAME

stampit - prefix each line with an RFC 3339 compliant timestamp

# SYNOPSIS

*stampit* [_OPTIONS_] [_TEXT_ …]

# DESCRIPTION

*stampit* prefixes each line received on standard input with a
timestamp in the format _YYYY-MM-DDTHH:MM:SS.µµµµµµ±hh:mm_.

If *stampit* receives a SIGHUP and _FILENAME_ has been given, *stampit*
will reopen the file, something that can be useful when rotating logfiles
written to through  *stampit*.

The optional _TEXT_ will be stamped and written verbatim as the first
line before *stampit* starts reading from standard input.

# OPTIONS

*-h*, *--help*
	Print a short help text to standard error stream and exit

*-v*, *--version*
	Show version information and exit

*-c*, *--copyright*
	Show copying policy and exit

*-l*, *--localtime*
	Use local time rather than UTC

*-o* _OUTPUT_, *--output*=_OUTPUT_
	Path to output file

RFC 3339 timestamps are given with µs precision.

# EXAMPLES

$ (echo A; sleep 2s; echo B; sleep 3s; echo C) | stampit++
2020-11-20T14:45:13.612858+00:00	A
2020-11-20T14:45:15.615734+00:00	B
2020-11-20T14:45:18.621444+00:00	C

# AUTHOR

Klaus Alexander Seistrup <klaus@seistrup.dk>

# DEVELOPMENT

Please see git repository:

- https://github.com/kseistrup/timestamp/

# COPYRIGHT

Copyright ©2017-20, Klaus Alexander Seistrup.

# LICENSE

GNU General Public License, version 3 or later.

# SEE ALSO

*timestamp*(1),
*ts*(1) from the moreutils package,
*kill*(1) and *killall*(1) for how to send a SIGHUP to *stampit*,
and *logrotate*(8).
