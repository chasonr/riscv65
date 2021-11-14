#include <string.h>
#include <unistd.h>

int
main(void)
{
    const char *hello = "Hello world this is a much longer hello\n";
    write(1, hello, strlen(hello));
    return 0;
}
