/* timestamp.c

   Copyright (C) 2011 Klaus Alexander Seistrup <klaus@seistrup.dk>.
   Released under GPLv3+.
*/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <getopt.h>
#include <locale.h>
#include <signal.h>
#include <libgen.h>

#define PROGNAME "timestamp"
#define VERSION  "0.1.1 (2017-04-07)"

#define USAGE \
"Usage: %s [OPTIONS] [FILENAME]\n" \
"\n" \
"positional arguments:\n" \
"  FILENAME              optional output file\n" \
"\n" \
"optional arguments:\n" \
"  -h, --help            show this help message and exit\n" \
"  -v, --version         show version information and exit\n" \
"  -c, --copyright       show copying policy and exit\n" \
"  -f FORMAT, --format FORMAT\n" \
"                        datetime format (default: ‘%%F %%T’)\n" \
"\n" \
"Please see strftime(3) for possible conversion specifications.\n"

#define BUFSIZE 128

char *filename;
FILE *out;

static void
print(char *text, int end)
{
  if (fputs(text, stdout) == EOF)
    exit(1);
  if (putchar(end) == EOF)
    exit(1);
}

static void
timestamp(char *fmt)
{
  time_t t = 0;
  struct tm *tm = NULL;
  static char buf[BUFSIZE];

  t = time(NULL);

  if (t == (time_t) -1)
    {
      perror("Cannot get current time");
      exit(1);
    }

  tm = localtime(&t);

  if (tm == (struct tm *) NULL)
    exit(1);

  if (strftime(buf, BUFSIZE, fmt, tm) == 0)
    {
      (void) fprintf(stderr,
		     "Resulting timestamp should be less than %d characters, please.\n",
		     BUFSIZE);
      exit(1);
    }

  print(buf, (int) '\t');
}

static void
open_out(int signum)
{
  if (filename != NULL)
    {
      if ((out = freopen(filename, "a", stdout)) == (FILE *) NULL)
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
  char *fmt = "%F %T";
  int opt, idx = 0;
  char *prog = basename(argv[0]);

  const struct option long_opts[] = {
    {"help",      0, NULL, (int) 'h'},
    {"version",   0, NULL, (int) 'v'},
    {"copyright", 0, NULL, (int) 'c'},
    {"format",    1, NULL, (int) 'f'},
    { NULL,       0, NULL,        0 }
  };

  (void) setlocale(LC_ALL, "");

  filename = NULL;
  argv[0] = prog;
  opterr = 0;

  while ((opt = getopt_long(argc, argv, ":hvcf:", long_opts, &idx)) != EOF)
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
	case 'f':
	  fmt = optarg;
	  break;
	default:
	  if (optopt)
	    {
	      if (opt == (int) '?')
		(void) fprintf(stderr,
			       "%s: Invalid option ‘%c’.\n",
			       prog, optopt);
	      else if (opt == (int) ':')
		(void) fprintf(stderr,
			       "%s: Option ‘%c’ requires an argument.\n",
			       prog, optopt);
	    }
	  else if (optind - 1 <= argc)
	    (void) fprintf(stderr,
			   "%s: Unrecognized option ‘%s’.\n",
			   prog, argv[optind - 1]);
	  (void) fprintf(stderr,
			 "Try ‘%s --help’ for more information.\n", prog);
	  return 1;
	}
    }
  argv += optind;
  argc -= optind;

  if (argc > 0)
    {
      filename = argv[0];
      --argc;
      ++argv;
    }

  if (signal(SIGHUP, open_out) == SIG_IGN)
    signal(SIGHUP, SIG_IGN);

  open_out(0);

  c = getchar();

  if (c == EOF)
    return 0;

  if (ungetc(c, stdin) != c)
    return 1;

  timestamp(fmt);

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

	  timestamp(fmt);
	}
      else if (putchar((int) c) == EOF)
	return 1;
    }
}

// indent -gnu -npcs -i2
