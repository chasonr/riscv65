#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

extern int _openat(int dirfd, const char *name, int flags, int mode);
extern int _faccessat(int dirfd, const char *file, int mode, int flags);
extern int _fstatat(int dirfd, const char *file, struct stat *st, int flags);

int
main(void)
{
    time_t tv1;
    time(&tv1);
    printf("time=%s\n", asctime(gmtime(&tv1)));

#if 0
    char str[256];

    strcpy(str, "unwritten");
    printf("Type something:\n");
    char *p = fgets(str, sizeof(str), stdin);
    printf("p=%p str=%p\n", p, str);
    if (p == NULL) {
        perror("fgets");
    }
    printf("input string is \"%s\"", str);
#endif

#if 0
    static const char path[] = "/dir.1/dir.2/test.txt";
    errno = 0;
    int rc = unlink(path);
    printf("unlink returns: %d errno=%d\n", rc, errno);

    FILE *fp = fopen("/deleteme.txt", "w");
    if (fp != NULL) {
        printf("Creating deleteme.txt\n");
        fprintf(fp, "This is a file to be deleted\n");
        fclose(fp);
        errno = 0;
        rc = unlink("/deleteme.txt");
        printf("unlink returns: %d errno=%d\n", rc, errno);
    }
#endif
#if 0
#if 0
    int x;
#endif
    int rc = chdir("/dir.1");
    printf("chdir returns: %d\n", rc);
    rc = chdir("dir.2");
    printf("chdir returns: %d\n", rc);
#endif

#if 0
    int fd = open("test.txt", O_RDONLY, 0);
    printf("len=%u open returns: %d\n", (unsigned)strlen(path), fd);
#endif
#if 1
    struct stat st;
    //memset(&st, 0, sizeof(st));
    chdir("/dir.1/dir.2");
    char buf[512];
    char *rcp = getcwd(buf, sizeof(buf));
    printf("getcwd returns: %p %s\n", rcp, rcp ? buf : "");
    int rc = access("test.txt", 0);
    printf("test.txt: rc = %d\n", rc);
    rcp = getcwd(buf, sizeof(buf));
    printf("getcwd returns: %p %s\n", rcp, rcp ? buf : "");
    errno = 0;
    rc = access("test.txt", 0);
    printf("test.txt: rc = %d errno = %d\n", rc, errno);
    errno = 0;
    rc = access("bogus.txt", 0);
    printf("bogus.txt: rc = %d errno = %d\n", rc, errno);
#endif
#if 0
    printf("st_dev = %ld\n", (long)st.st_dev);
    printf("sizeof(st_ino) = %lu\n", (unsigned long)sizeof(st.st_ino));
    printf("st_ino = %ld\n", (long)st.st_ino);
    printf("st_mode = %lo\n", (long)st.st_mode);
    printf("st_nlink = %ld\n", (long)st.st_nlink);
    printf("st_uid = %ld\n", (long)st.st_uid);
    printf("st_gid = %ld\n", (long)st.st_gid);
    printf("st_rdev = %ld\n", (long)st.st_rdev);
    printf("st_size = %ld\n", (long)st.st_size);
    printf("st_blksize = %ld\n", (long)st.st_blksize);
    printf("st_blocks = %ld\n", (long)st.st_blocks);
    printf("st_atim = %ld\n", (long)st.st_atim.tv_sec);
    printf("st_ctim = %ld\n", (long)st.st_ctim.tv_sec);
    printf("st_mtim = %ld\n", (long)st.st_mtim.tv_sec);
#endif
#if 0
    close(fd);
#endif

#if 0
    // Seek forward, from end
    long pos = lseek(fd, -60000, SEEK_END);
    x = errno;
    printf("pos=%ld\n", pos);
    errno = x;
    if (pos == -1) {
        perror("lseek");
    }

    char buf[20];
    ssize_t len = read(fd, buf, sizeof(buf));
    printf("Sample: %.*s\n", (int)len, buf);

    // Seek backward, from beginning
    pos = lseek(fd, 10000, SEEK_SET);
    x = errno;
    printf("pos=%ld\n", pos);
    errno = x;
    if (pos == -1) {
        perror("lseek");
    }

    len = read(fd, buf, sizeof(buf));
    printf("Sample: %.*s\n", (int)len, buf);

    // Seek forward, from current
    pos = lseek(fd, 10000, SEEK_CUR);
    x = errno;
    printf("pos=%ld\n", pos);
    errno = x;
    if (pos == -1) {
        perror("lseek");
    }

    len = read(fd, buf, sizeof(buf));
    printf("Sample: %.*s\n", (int)len, buf);

    int fd2;
    static const char path2[] = "/dir.1/dir.2/test-2.txt";
    fd2 = open(path2, O_RDWR | O_CREAT, 0644);
    x = errno;
    printf("fd2=%d\n", fd2);
    errno = x;
    if (fd2 < 0) {
        perror(path2);
    }
    close(fd2);

    // This is supposed to fail
    fd2 = open(path, O_RDWR | O_CREAT | O_EXCL, 0644);
    x = errno;
    printf("fd2=%d\n", fd2);
    errno = x;
    if (fd2 < 0) {
        perror(path);
    }
    close(fd2);

    // This should also fail: the file is already open
    fd2 = open(path, O_RDWR | O_CREAT | O_TRUNC, 0644);
    x = errno;
    printf("fd2=%d\n", fd2);
    errno = x;
    if (fd2 < 0) {
        perror(path);
    }
    close(fd2);

    // This should succeed
    fd2 = open(path, O_RDONLY, 0);
    x = errno;
    printf("fd2=%d\n", fd2);
    errno = x;
    if (fd2 < 0) {
        perror(path);
    }
    close(fd2);

    close(fd);

    // This should succeed, and truncate the file
    fd2 = open(path, O_RDWR | O_CREAT | O_TRUNC, 0644);
    x = errno;
    printf("fd2=%d\n", fd2);
    errno = x;
    if (fd2 < 0) {
        perror(path);
    }
    close(fd2);
#endif

#if 0
    if (fd >= 0) {
        char buf[1024];
        ssize_t len;
        long count = 0;

        time_t t1 = time(NULL);
        do {
            len = read(fd, buf, sizeof(buf));
            if (len < 0) {
                perror(path);
                break;
            }
            //write(1, buf, len);
            count += len;
        } while (len != 0);
        printf("Read %ld bytes\n", count);
        time_t t2 = time(NULL);
        printf("%ld seconds elapsed\n", (long)(t2-t1));
    } else {
        perror(path);
    }
#endif

    return 0;
}
