/* chkout.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_chkout(unsigned char filenum)
{
    return syscall_1(CBM_CHKOUT, filenum);
}
