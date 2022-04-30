/*****************************************************************************
* Copyright ©2017-20 Klaus Alexander Seistrup <klaus@seistrup.dk>            *
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
#include <signal.h>
#include <libgen.h>
#include <time.h>

#include "version.h"

#ifdef __USE_MISC
#define USAGE \
"Usage: %s [OPTIONS] [TEXT …]\n" \
"\n" \
"positional arguments:\n" \
"  TEXT                  optional text to timestamp\n" \
"\n" \
"optional arguments:\n" \
"  -h, --help            show this help message and exit\n" \
"  -v, --version         show version information and exit\n" \
"  -c, --copyright       show copying policy and exit\n" \
"  -l, --localtime       use local time rather than UTC\n" \
"  -o OUTPUT, --output OUTPUT\n" \
"                        path to output file\n" \
"\n" \
"If no TEXT is given on the command line, text is read\n" \
"from standard input and stamped one line at a time.\n" \
"\n" \
"RFC 3339 timestamps are given with µs precision.\n" \
"\n" \
"If stampit receives a SIGHUP and OUTPUT has been given,\n" \
"stampit will reopen the file.\n"
#else
#define USAGE \
"Usage: %s [OPTIONS] [TEXT …]\n" \
"\n" \
"positional arguments:\n" \
"  TEXT                  optional text to timestamp\n" \
"\n" \
"optional arguments:\n" \
"  -h, --help            show this help message and exit\n" \
"  -v, --version         show version information and exit\n" \
"  -c, --copyright       show copying policy and exit\n" \
"  -o OUTPUT, --output OUTPUT\n" \
"                        path to output file\n" \
"  -s SEPARATOR, --separator SEPARATOR\n" \
"                        separator to use, defaults to one space\n" \
"\n" \
"If no TEXT is given on the command line, text is read\n" \
"from standard input and stamped one line at a time.\n" \
"\n" \
"RFC 3339 timestamps are given in UTC with µs precision.\n" \
"\n" \
"If stampit receives a SIGHUP and OUTPUT has been given,\n" \
"stampit will reopen the file.\n"
#endif

char *filename = NULL;
char *separator = " ";
size_t separator_length = 1;

#ifdef __USE_MISC
int use_localtime = 0;
#endif

static void
timestamp(void)
{
   int res, usec;
   int hh = 0, mm = 0, ss = 0;
   time_t tnow = 0;
   struct timespec ts = {
      .tv_sec  = 0,
      .tv_nsec = 0
   };
   struct tm *tm = NULL;

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

#ifdef __USE_MISC
   if (use_localtime == 1)
     {
	tm = localtime(&tnow);
	ss = tm->tm_gmtoff;
	hh =  abs(ss) / 3600;
	mm = (abs(ss) % 3600) / 60;
     }
   else
#endif
      tm = gmtime(&tnow);

   if (tm == (struct tm *) NULL)
      exit(1);

   // YYYY-MM-DDThh:mm:ss.uuuuuu+00:00
   // ----+----|----+----|----+----|--
   // = 32 chars for the stamp and 1+ chars for the separator
   res = printf("%04d-%02d-%02dT%02d:%02d:%02d.%06d%c%02d:%02d%s",
		tm->tm_year + 1900,
		tm->tm_mon + 1,
		tm->tm_mday,
		tm->tm_hour,
		tm->tm_min,
		tm->tm_sec,
		usec,
		ss < 0 ? '-' : '+', hh, mm,
      separator);

   if (res != (32 + separator_length))
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

static void
open_out(int signum)
{
   if (filename != NULL)
     {
	if (freopen(filename, "a", stdout) == (FILE *) NULL)
	  {
	     perror(filename);
	     exit(1);
	  }
     }

   if (setvbuf(stdout, NULL, (int) _IOLBF, 0) != 0)
     {
	perror("Cannot set line buffering");
	exit(1);
     }
}

int
main(int argc, char **argv)
{
   int c, t;
   int opt, idx = 0;
   char *prog = basename(argv[0]);
#ifdef __USE_MISC
   char *short_opts = ":hvclo:s:";
#else
   char *short_opts = ":hvco:s:";
#endif

   const struct option long_opts[] = {
      {"help",      0, NULL, (int) 'h'},
      {"version",   0, NULL, (int) 'v'},
      {"copyright", 0, NULL, (int) 'c'},
      {"output",    1, NULL, (int) 'o'},
      {"separator", 1, NULL, (int) 's'},
      { NULL,       0, NULL,        0 }
   };

   (void) setlocale(LC_ALL, "");

   argv[0] = prog;
   opterr = 0;

   while ((opt = getopt_long(argc, argv, short_opts, long_opts, &idx)) != EOF)
     {
	switch (opt)
	  {
	  case 'h':
	     (void) fprintf(stderr, USAGE, prog);
	     return 0;
	     /*@fallthrough@ */
	  case 'v':
	     (void) fprintf(stderr, "%s/%s\n", STAMPIT, VERSION);
	     return 0;
	     /*@fallthrough@ */
	  case 'c':
	     (void) fputs("GNU General Public License v3+\n", stderr);
	     return 0;
	     /*@fallthrough@ */
	  case 'o':
	     filename = optarg;
	     break;
#ifdef __USE_MISC
	  case 'l':
	     use_localtime = 1;
	     break;
#endif
     case 's':
        separator = optarg;
        separator_length = strlen(separator);
        break;
	  default:
	     if (optopt)
	       {
		  if (opt == (int) '?')
		     (void) fprintf(stderr, "%s: Invalid option ‘%c’.\n",
				    prog, optopt);
		  else if (opt == (int) ':')
		     (void) fprintf(stderr,
				    "%s: Option ‘%c’ requires an argument.\n",
				    prog, optopt);
	       }
	     else if (optind - 1 <= argc)
		(void) fprintf(stderr, "%s: Unrecognized option ‘%s’.\n",
			       prog, argv[optind - 1]);
	     (void) fprintf(stderr,
			    "Try ‘%s --help’ for more information.\n",
			    prog);
	     return 1;
	  }
     }
   argv += optind;
   argc -= optind;

   if (signal(SIGHUP, open_out) == SIG_IGN)
      signal(SIGHUP, SIG_IGN);

   open_out(0);

   if (argc > 0)
     {
	timestamp();

	// Python: print(' '.join(argv))
	for (idx = 0; idx < (argc - 1); ++idx)
	   print(argv[idx], (int) ' ');

	print(argv[idx], (int) '\n');

	return 0;
     }

   if ((c = getchar()) == EOF)
      return 0;

   if (ungetc(c, stdin) != c)
      return 1;

   timestamp();

   for (;;)
     {
	if ((c = getchar()) == EOF)
	   return 0;

	if (c == (int) '\n')
	  {
	     if (putchar((int) '\n') == EOF)
		return 1;

	     if ((t = getchar()) == EOF)
		return 0;

	     if (ungetc(t, stdin) != t)
		return 1;

	     timestamp();
	  }
	else if (putchar(c) == EOF)
	   return 1;
     }
}

// indent -gnu -npcs -i3
