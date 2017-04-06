# timestamp

Prefix each line with a timestamp

`timestamp` prefixes each line received on standard input with a
timestamp in the format `YYYY-MM-DD HH:MM:SS`.

## Example

```sh
$ (echo A; sleep 2s; echo B; sleep 3s; echo C) | timestamp
2011-03-14 20:58:58     A
2011-03-14 20:59:00     B
2011-03-14 20:59:03     C
```

# stampit

`stampit` is a special flavour of `timestamp` that

* uses RFC 3339 compliant timestamps with µs precision,
* does _not_ accept custom formats, but
* accepts text to be stamped as argument(s), and
* appends stamped lines to a file when the `--output` option is used.

This makes `stampit` a useful candidate for e.g.

* posting to a [twtxt.txt](https://github.com/buckket/twtxt) file, or
* logging one-line events to an ever-growing logfile.

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
