/* acptr.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_acptr(void)
{
    return syscall_0(CBM_ACPTR);
}
