/* tksa.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_tksa(unsigned char sec_addr)
{
    return syscall_1(CBM_TKSA, sec_addr);
}
