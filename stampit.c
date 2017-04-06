/*****************************************************************************
* Copyright ©2017 Klaus Alexander Seistrup <klaus@seistrup.dk>               *
*                                                                            *
* This program is free software; you can redistribute it and/or modify it    *
* under the terms of the GNU General Public License as published by the Free *
* Software Foundation; either version 3 of the License, or (at your option)  *
* any later version.                                                         *
*                                                                            *
* This program is distributed in the hope that it will be useful, but with-  *
* out any warranty; without even the implied warranty of merchantability or  *
* fitness for a particular purpose. See the GNU General Public License for   *
* more details.  <http://gplv3.fsf.org/>                                     *
*****************************************************************************/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include <locale.h>
#include <time.h>
#include <libgen.h>

#define PROGNAME "stampit"
#define VERSION  "0.1.3 (2017-04-06)"

#define USAGE \
"Usage: %s [OPTIONS] [TEXT [TEXT …]]\n" \
"\n" \
"positional arguments:\n" \
"  TEXT                  optional text to timestamp\n" \
"\n" \
"optional arguments:\n" \
"  -h, --help            show this help message and exit\n" \
"  -v, --version         show version information and exit\n" \
"  -c, --copyright       show copying policy and exit\n" \
"  -o OUTPUT, --output OUTPUT\n" \
"                        path of output file\n" \
"\n" \
"If no TEXT is given on the command line, text is read\n" \
"from standard input and stamped one line at a time.\n" \
"\n" \
"RFC 3339 timestamps are given in UTC with µs precision.\n"

static void
timestamp(void)
{
  int res, usec;
  time_t tnow = 0;
  struct timespec ts = {
    .tv_sec = 0,
    .tv_nsec = 0
  };
  struct tm *utc = NULL;

  res = clock_gettime(CLOCK_REALTIME, &ts);

  if (res != 0)
    {
      perror("Cannot get current time");
      exit(1);
    }

  tnow = ts.tv_sec;
  usec = (int) ((ts.tv_nsec + 500) / 1000);

  while (usec > 1000000)
    {
      usec -= 1000000;
      tnow += 1;
    }

  utc = gmtime(&tnow);

  if (utc == (struct tm *) NULL)
    exit(1);

  // YYYY-MM-DDThh:mm:ss.uuuuuu+00:00t
  // ----+----|----+----|----+----|---
  // = 32 chars for the stamp and 1 char for the TAB
  res = printf("%04d-%02d-%02dT%02d:%02d:%02d.%06d+00:00\t",
	       utc->tm_year + 1900,
	       utc->tm_mon + 1,
	       utc->tm_mday,
	       utc->tm_hour,
	       utc->tm_min,
	       utc->tm_sec,
	       usec);

  if (res != 33)
    exit(1);
}

static void
print(char *text, int end)
{
  if (fputs(text, stdout) == EOF)
    exit(1);
  if (putchar(end) == EOF)
    exit(1);
}

int
main(int argc, char **argv)
{
  int c, t;
  int opt, idx = 0;
  char *prog = basename(argv[0]);

  const struct option long_opts[] = {
    {"help",      0, NULL, (int) 'h'},
    {"version",   0, NULL, (int) 'v'},
    {"copyright", 0, NULL, (int) 'c'},
    {"output",    1, NULL, (int) 'o'},
    { NULL,       0, NULL,        0 }
  };

  (void) setlocale(LC_ALL, "");

  argv[0] = prog;
  opterr = 0;

  while ((opt = getopt_long(argc, argv, ":hvco:", long_opts, &idx)) != EOF)
    {
      switch (opt)
	{
	case 'h':
	  (void) fprintf(stderr, USAGE, prog);
	  return 0;
	  /*@fallthrough@*/
	case 'v':
	  (void) fprintf(stderr, "%s/%s\n", PROGNAME, VERSION);
	  return 0;
	  /*@fallthrough@*/
	case 'c':
	  (void) fputs("GNU General Public License v3+\n", stderr);
	  return 0;
	  /*@fallthrough@*/
	case 'o':
	  if (freopen(optarg, "a", stdout) == (FILE *) NULL)
	    {
	      perror(optarg);
	      return 1;
	    }
	  break;
	default:
	  if (optopt)
	    {
	      if (opt == (int) '?')
		(void) fprintf(stderr, "%s: Invalid option ‘%c’.\n", prog,
			       optopt);
	      else if (opt == (int) ':')
		(void) fprintf(stderr,
			       "%s: Option ‘%c’ requires an argument.\n",
			       prog, optopt);
	    }
	  else if (optind - 1 <= argc)
	    (void) fprintf(stderr, "%s: Unrecognized option ‘%s’.\n",
			   prog, argv[optind - 1]);
	  (void) fprintf(stderr,
			 "Try ‘%s --help’ for more information.\n", prog);
	  return 1;
	}
    }
  argv += optind;
  argc -= optind;

  if (setvbuf(stdout, NULL, (int) _IOLBF, 0) != 0)
    {
      perror("Cannot set line buffering");
      return 1;
    }

  if (argc > 0)
    {
      timestamp();

      // Python: print(' '.join(argv))
      for (idx = 0; idx < (argc - 1); ++idx)
	print(argv[idx], (int) ' ');

      print(argv[idx], (int) '\n');

      return 0;
    }

  c = getchar();

  if (c == EOF)
    return 0;

  if (ungetc(c, stdin) != c)
    return 1;

  timestamp();

  for (;;)
    {
      c = getchar();

      if (c == EOF)
	return 0;

      if (c == (int) '\n')
	{
	  if (putchar((int) '\n') == EOF)
	    return 1;

	  t = getchar();

	  if (t == EOF)
	    return 0;

	  if (ungetc(t, stdin) != t)
	    return 1;

	  timestamp();
	}
      else if (putchar(c) == EOF)
	return 1;
    }
}

// indent -gnu -npcs -i2
