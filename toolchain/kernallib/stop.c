/* stop.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_stop(void)
{
    return syscall_0(CBM_STOP);
}
