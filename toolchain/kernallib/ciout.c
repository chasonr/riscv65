/* ciout.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_ciout(unsigned char chr)
{
    return syscall_1(CBM_CIOUT, chr);
}
