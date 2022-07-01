/* chrin.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_chrin(void)
{
    return syscall_0(CBM_CHRIN);
}
