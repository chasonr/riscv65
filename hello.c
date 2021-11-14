#include <stdio.h>
#include <string.h>

int
main(void)
{
    const char *hello = "Hello world this is a much longer hello\n";
    fputs(hello, stdout);
    return 0;
}
