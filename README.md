# Timestamp

Prefix each line with a timestamp

`timestamp` prefixes each line received on standard input with a
timestamp in the format `YYYY-MM-DD HH:MM:SS`.

## Usage

```txt
Usage: timestamp [OPTIONS] [FILENAME] [TEXT [TEXT …]]

positional arguments:
  FILENAME              optional output file
  TEXT                  optional startup message

optional arguments:
  -h, --help            show this help message and exit
  -v, --version         show version information and exit
  -c, --copyright       show copying policy and exit
  -u, --utc             use UTC rather than local time
  -r, --rfc3339         use RFC 3339 compliant timestamps
  -f FORMAT, --format FORMAT
                        datetime format (default: ‘%F %T’)

Please see strftime(3) for possible conversion specifications.

Any TEXT after FILENAME will be stamped and written verbatim as
the first line before timestamp starts reading from standard
input. Use the pseudo filename ‘-’ for standard output.
```

## Example

```sh
$ (echo A; sleep 2s; echo B; sleep 3s; echo C) | timestamp
2011-03-14 20:58:58     A
2011-03-14 20:59:00     B
2011-03-14 20:59:03     C
```

----

# Stampit

`stampit` is a special flavour of `timestamp` that

* uses RFC 3339 compliant timestamps with µs precision,
* does _not_ accept custom formats, but
* accepts text to be stamped as argument(s), and
* appends stamped lines to a file when the `--output` option is used.

This makes `stampit` a useful candidate for e.g.

* posting to a [twtxt.txt](https://github.com/buckket/twtxt) file, or
* logging one-line events to an ever-growing logfile.

## Usage

```txt
Usage: stampit [OPTIONS] [TEXT [TEXT …]]

positional arguments:
  TEXT                  optional text to timestamp

optional arguments:
  -h, --help            show this help message and exit
  -v, --version         show version information and exit
  -c, --copyright       show copying policy and exit
  -l, --localtime       use local time rather than UTC
  -o OUTPUT, --output OUTPUT
                        path of output file

If no TEXT is given on the command line, text is read
from standard input and stamped one line at a time.

RFC 3339 timestamps are given with µs precision.
```

## Example

```sh
$ stampit Frække frølår
2017-04-06T16:49:21.727686+00:00        Frække frølår
$ stampit -o twtxt.txt Frække frølår
$ echo Crème brûlée | stampit -o twtxt.txt
$ cat twtxt.txt
2017-04-06T16:50:47.250455+00:00        Frække frølår
2017-04-06T16:51:02.068170+00:00        Crème brûlée
```

:smile:
