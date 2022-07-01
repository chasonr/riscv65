/* untlk.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_untlk(void)
{
    return syscall_0(CBM_UNTLK);
}
