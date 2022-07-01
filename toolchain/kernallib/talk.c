/* talk.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_talk(unsigned char device)
{
    return syscall_1(CBM_TALK, device);
}
