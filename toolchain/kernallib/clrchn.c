/* clrchn.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_clrchn(void)
{
    return syscall_0(CBM_CLRCHN);
}
