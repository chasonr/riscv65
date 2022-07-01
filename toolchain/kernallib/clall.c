/* clall.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_clall(void)
{
    return syscall_0(CBM_CLALL);
}
