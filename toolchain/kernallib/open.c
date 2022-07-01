/* open.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_open(const char *name, unsigned char filenum, unsigned char device,
         unsigned char sec_addr)
{
    return syscall_4(CBM_OPEN, (long)name, filenum, device, sec_addr);
}
