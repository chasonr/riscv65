/* unlsn.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_unlsn(void)
{
    return syscall_0(CBM_UNLSN);
}
