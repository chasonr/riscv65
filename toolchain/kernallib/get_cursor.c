/* get_cursor.c */

#include "kernal.h"
#include "kernalapi.h"

int
CBM_get_cursor(unsigned char *row, unsigned char *col)
{
    register long syscall_id asm("a7") = CBM_GET_CURSOR;
    register long a0 asm("a0");
    register long a1 asm("a1");
    asm volatile("ecall" : "+r"(a0), "+r"(a1) : "r"(syscall_id));
    *row = a0;
    *col = a1;
    return 0;
}
