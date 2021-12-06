#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

int
main(void)
{
    static const char path[] = "/dir.1/dir.2/test.txt";
    int fd = open(path, 0, 0);
    int x = errno;
    printf("len=%u open returns: %d\r\n", (unsigned)strlen(path), fd);
    errno = x;

    if (fd >= 0) {
        char buf[100];
        ssize_t len;
        long count = 0;

        do {
            len = read(fd, buf, sizeof(buf));
            if (len < 0) {
                perror(path);
                break;
            }
            write(1, buf, len);
            count += len;
        } while (len != 0);
        printf("Read %ld bytes\r\n", count);
    } else {
        perror(path);
    }

    return 0;
}
