#include <stdio.h>
#include <time.h>

void timestamp(void)
{
        time_t t;
        struct tm *tm;
        char buf[21];

        t = time(NULL);
        tm = localtime(&t);

        strftime(buf, 22, "%Y-%m-%d %H:%M:%S\t", tm);
        fwrite(buf, 1, 20, stdout);
}

int main(int argc, char **argv)
{
        int c = getchar();

        if (c == EOF)
                return 0;

        ungetc(c, stdin);

        timestamp();

        for (;;)
        {
                c = getchar();

                if (c == EOF)
                        return 0;
                if (c == '\n')
                {
                        int t = getchar();

                        putchar('\n');

                        if (t == EOF)
                                return 0;

                        ungetc(t, stdin);
                        timestamp();
                }
                else
                        putchar(c);

        }
}
