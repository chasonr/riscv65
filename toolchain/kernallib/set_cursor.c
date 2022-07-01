/* set_cursor.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_set_cursor(unsigned char row, unsigned char col)
{
    return syscall_2(CBM_SET_CURSOR, row, col);
}
