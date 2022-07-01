/* settim.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_settim(int time)
{
    return syscall_1(CBM_SETTIM, time);
}
