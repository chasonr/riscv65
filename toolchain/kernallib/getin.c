/* getin.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_getin(void)
{
    return syscall_0(CBM_GETIN);
}
