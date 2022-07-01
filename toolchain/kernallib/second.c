/* second.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_second(unsigned char sec_addr)
{
    return syscall_1(CBM_SECOND, sec_addr);
}
