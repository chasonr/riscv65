/* rdtim.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_rdtim(void)
{
    return syscall_0(CBM_RDTIM);
}
