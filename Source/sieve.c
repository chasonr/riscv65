#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#ifndef __CC65__
#include <sys/time.h>
#endif

#define PSIZE 1000
unsigned primes[PSIZE];

int
main(void)
{
#ifndef __CC65__
    struct timeval tv1, tv2;
#endif
    unsigned num_primes;
    unsigned num;

#ifndef __CC65__
    gettimeofday(&tv1, NULL);
#endif

    write(1, "Begin\n", 6);
    FILE *fp = fopen("/primes.txt", "w");
    if (fp == NULL) {
        perror("primes.txt");
        return EXIT_FAILURE;
    }
    printf("fp=%p fileno=%d\n", fp, fileno(fp));

    num_primes = 0;
    num = 2;
    while (num_primes < PSIZE) {
        unsigned i;
        for (i = 0; i < num_primes; ++i) {
            if (num % primes[i] == 0) {
                break;
            }
        }
        if (i >= num_primes) {
            //printf("%u\r\n", num);
            if (fp) {
                errno = 0;
                fprintf(fp, "%u\n", num);
                if (errno != 0) {
                    fprintf(stderr, "%u\n", num);
                    perror("primes.txt");
                }
            }
            primes[num_primes++] = num;
        }
        ++num;
    }

#ifndef __CC65__
    gettimeofday(&tv2, NULL);
#endif

    printf("primes[%u]=%u\r\n", PSIZE-1, primes[PSIZE-1]);
    if (fp) {
        fprintf(fp, "primes[%u]=%u\r\n", PSIZE-1, primes[PSIZE-1]);
    }
#ifndef __CC65__
    printf("%lu seconds\r\n", (unsigned long)(tv2.tv_sec - tv1.tv_sec));
    if (fp) {
        fprintf(fp, "%lu seconds\r\n", (unsigned long)(tv2.tv_sec - tv1.tv_sec));
    }
#endif
    if (fp) {
        fclose(fp);
    }

    return 0;
}
