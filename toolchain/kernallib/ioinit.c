/* ioinit.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_ioinit(void)
{
    return syscall_0(CBM_IOINIT);
}
