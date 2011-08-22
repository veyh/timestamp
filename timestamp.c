/* timestamp.c

   Copyright (C) 2011 Klaus Alexander Seistrup <klaus@seistrup.dk>.
   Released under GPLv3+.
*/

#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <string.h>
#include <locale.h>
#include <signal.h>

#define BUFSIZE 128

char *filename;
FILE *out;

void
timestamp(char *fmt)
{
    time_t t;
    size_t l;
    struct tm *tm;
    static char buf[BUFSIZE];

    t = time(NULL);

    if (t == (time_t) -1)
      {
	  perror("Couldn't get current time");
	  _exit(1);
      }

    tm = localtime(&t);

    if (tm == (struct tm *) NULL)
	_exit(-1);

    if ((l = strftime(buf, BUFSIZE, fmt, tm)) == 0)
      {
	  (void) fprintf(stderr,
			 "Resulting timestamp should be less than %d characters, please.\n",
			 BUFSIZE);
	  _exit(1);
      }

    if (fwrite(buf, 1, l, out) != l)
	_exit(1);

    if (fputc('\t', out) == EOF)
	_exit(1);
}

void
open_out(int signum)
{
    if (filename != NULL)
      {
	  if (out != NULL)
	    {
		fclose(out);
	    }
	  out = fopen(filename, "a");
	  if (out == NULL)
	    {
		perror("timestamp");
		_exit(10);
	    }
      }
    else
      {
	  out = stdout;
      }

    setlinebuf(out);
}

int
main(int argc, char **argv)
{
    int c, t;
    char *fmt = "%F %T";
    int used = 0;
    filename = NULL;

    (void) setlocale(LC_ALL, "");

    if (argc > 1)
      {
	  if ((strcmp(argv[1], "-h") == 0)
	      || (strcmp(argv[1], "--help") == 0))
	    {
		(void) puts("Usage: timestamp [-f FORMAT] [FILENAME]");
		return 0;
	    }
	  if ((strcmp(argv[1], "-f") == 0)
	      || (strcmp(argv[1], "--format") == 0))
	    {
		fmt = argv[2];
		used = 2;
	    }
	  if (argc > used)
	    {
		filename = argv[used + 1];
	    }
      }

    if (signal(SIGHUP, open_out) == SIG_IGN)
      {
	  signal(SIGHUP, SIG_IGN);
      }

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

	  if (c == '\n')
	    {
		t = getchar();

		if (fputc('\n', out) == EOF)
		    return 1;

		if (t == EOF)
		    return 0;

		if (ungetc(t, stdin) != t)
		    return 1;

		timestamp(fmt);
	    }
	  else if (fputc(c, out) == EOF)
	      return 1;
      }
}

// indent -gnu -npcs -i4
