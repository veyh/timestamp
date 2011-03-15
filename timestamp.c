/* timestamp.c

   Copyright (C) 2011 Klaus Alexander Seistrup <klaus@seistrup.dk>.
   Released under GPLv2+.
*/

#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <string.h>

#define BUFSIZE 128

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

    if (fwrite(buf, 1, l, stdout) != l)
	_exit(1);

    if (putchar('\t') == EOF)
	_exit(1);
}

int
main(int argc, char **argv)
{
    int c, t;
    char *fmt = "%F %T";

    if (argc > 1)
      {
	  if ((argc > 2) || (strcmp(argv[1], "-h") == 0)
	      || (strcmp(argv[1], "--help") == 0))
	    {
		(void) puts("Usage: timestamp [FORMAT]");
		return 0;
	    }
	  fmt = argv[1];
      }

    setlinebuf(stdout);

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

		if (putchar('\n') == EOF)
		    return 1;

		if (t == EOF)
		    return 0;

		if (ungetc(t, stdin) != t)
		    return 1;

		timestamp(fmt);
	    }
	  else if (putchar(c) == EOF)
	      return 1;
      }
}

// indent -gnu -npcs -i4
