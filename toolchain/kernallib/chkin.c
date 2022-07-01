/* chkin.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_chkin(unsigned char filenum)
{
    return syscall_1(CBM_CHKIN, filenum);
}
