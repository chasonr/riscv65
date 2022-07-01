/* cint.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_cint(void)
{
    return syscall_0(CBM_CINT);
}
