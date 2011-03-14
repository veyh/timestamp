/* timestamp.c

   Copyright (C) 2011 Klaus Alexander Seistrup <klaus@seistrup.dk>.
   Released under GPLv2+.
*/

#include <stdio.h>
#include <time.h>
#include <unistd.h>

void
timestamp(void)
{
    time_t t;
    size_t l;
    struct tm *tm;
    char buf[22];

    t = time(NULL);

    if (t == (time_t) - 1)
      {
	  perror(NULL);
	  _exit(1);
      }

    tm = localtime(&t);

    if (tm == (struct tm *) NULL)
	_exit(-1);

    l = strftime(buf, 21, "%Y-%m-%d %H:%M:%S\t", tm);

    if (l == 0 || l > 20)
	_exit(1);

    if (fwrite(buf, 1, l, stdout) != l)
	_exit(1);
}

int
main(int argc, char **argv)
{
    int c = getchar();

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

	  if (c == '\n')
	    {
		int t = getchar();

		if (putchar('\n') == EOF)
		    return 1;

		if (t == EOF)
		    return 0;

		if (ungetc(t, stdin) != t)
		    return 1;

		timestamp();
	    }
	  else
	    {
		if (putchar(c) == EOF)
		    return 1;
	    }

      }
}
