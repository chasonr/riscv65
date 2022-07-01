/* chrout.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_chrout(unsigned char chr)
{
    return syscall_1(CBM_CHROUT, chr);
}
