/* readst.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_readst(void)
{
    return syscall_0(CBM_READST);
}
