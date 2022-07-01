/* close.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_close(unsigned char filenum)
{
    return syscall_1(CBM_CLOSE, filenum);
}
