TIMESTAMP(1)	"V0.3.0"

# NAME

timestamp - prefix each line with a timestamp

# SYNOPSIS

*timestamp* [_OPTIONS_] [_FILENAME_] [_TEXT_ [_TEXT_ …]]

# DESCRIPTION

*timestamp* prefixes each line received on standard input with a
timestamp in the format _YYYY-MM-DD HH:MM:SS_.

If _FILENAME_ is given, timestamp will write to _FILENAME_ instead
of standard output.

If *timestamp* receives a SIGHUP and _FILENAME_ has been given, *timestamp*
will reopen the file, something that can be useful when rotating logfiles
written to through  *timestamp*.

Any _TEXT_ after _FILENAME_ will be stamped and written verbatim as
the first line before *timestamp* starts reading from standard
input. Use the pseudo filename ‘_-_’ for standard output.

# OPTIONS

*-h*, *--help*
	Print a short help text to standard error stream and exit

*-v*, *--version*
	Show version information and exit

*-c*, *--copyright*
	Show copying policy and exit

*-u*, *--utc*
	Use UTC rather than local time

*-r*, *--rfc3339*
	Use RFC 3339 compliant timestamps

*-t*, *--tai64n*
	Use TAI64N timestamps

*-f* _FORMAT_, *--format*=_FORMAT_
	Datetime format (default: ’_%F %T_’)

Please see *strftime*(3) for possible conversion specifications.

# EXAMPLES

$ (echo A; sleep 2s; echo B; sleep 3s; echo C) | timestamp++
2020-11-20 10:29:44	A++
2020-11-20 10:29:46	B++
2020-11-20 10:29:49	C++
$ (echo A; sleep 2s; echo B; sleep 3s; echo C) | timestamp --tai64n++
@400000005fb78c9e339ac51c	A++
@400000005fb78ca033d3fc29	B++
@400000005fb78ca334742e9a	C

# AUTHORS

- Klaus Alexander Seistrup <klaus@seistrup.dk>++
C and scdoc sources, Makefile
- Adam Sjøgren <asjo@koldfront.dk>++
Debian stuff and original man pages

# DEVELOPMENT

Please see git repositories:

- https://koldfront.dk/git/timestamp/
- https://github.com/kseistrup/timestamp/

# COPYRIGHT

Copyright ©2011-20, Klaus Alexander Seistrup.

# LICENSE

GNU General Public License, version 3 or later.

# SEE ALSO

*strftime*(3) for date and time conversion specifications,
*ts*(1) from the moreutils package,
*kill*(1) and *killall*(1) for how to send a SIGHUP to *timestamp*,
and *logrotate*(8).
