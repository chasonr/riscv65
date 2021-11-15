#include <stdio.h>
#include <string.h>
#include <sys/time.h>

int
main(void)
{
    struct timeval tv;

    memset(&tv, 0x55, sizeof(tv));
    gettimeofday(&tv, NULL);

    fputs("Hello 1\r\n", stdout);
    printf("Hello 2\r\n");
    printf("Hello tv=%lu%09lu %lu\r\n",
            (unsigned long)tv.tv_sec/1000000000,
            (unsigned long)tv.tv_sec%1000000000,
            tv.tv_usec);
    fputs("Hello 3\r\n", stdout);
    printf("Hello 4\r\n");
    time_t t = tv.tv_sec;
    printf("%s\r\n", asctime(gmtime(&t)));
    t = tv.tv_usec;
    printf("%s\r\n", asctime(gmtime(&t)));
    return 0;
}
