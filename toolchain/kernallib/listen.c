/* listen.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_listen(unsigned char device)
{
    return syscall_1(CBM_LISTEN, device);
}
