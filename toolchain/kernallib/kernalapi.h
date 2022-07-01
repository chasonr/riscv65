/* kernalapi.h -- system call numbers for Kernal access */

#ifndef KERNALAPI_H
#define KERNALAPI_H

#define CBM_ACPTR      0x7F000000
#define CBM_CHKIN      0x7F000001
#define CBM_CHKOUT     0x7F000002
#define CBM_CHRIN      0x7F000003
#define CBM_CHROUT     0x7F000004
#define CBM_CINT       0x7F000005
#define CBM_CIOUT      0x7F000006
#define CBM_CLALL      0x7F000007
#define CBM_CLOSE      0x7F000008
#define CBM_CLRCHN     0x7F000009
#define CBM_GETIN      0x7F00000A
#define CBM_IOINIT     0x7F00000B
#define CBM_LISTEN     0x7F00000C
#define CBM_OPEN       0x7F00000D
#define CBM_GET_CURSOR 0x7F00000E
#define CBM_SET_CURSOR 0x7F00000F
#define CBM_RDTIM      0x7F000010
#define CBM_READST     0x7F000011
#define CBM_SECOND     0x7F000012
#define CBM_SETTIM     0x7F000013
#define CBM_STOP       0x7F000014
#define CBM_TALK       0x7F000015
#define CBM_TKSA       0x7F000016
#define CBM_UNLSN      0x7F000017
#define CBM_UNTLK      0x7F000018

/* System call functions */

static inline long
syscall_0(long n)
{
    register long syscall_id asm("a7") = n;
    register long a0 asm("a0");
    asm volatile("ecall" : "+r"(a0) : "r"(syscall_id));
    return a0;
}

static inline long
syscall_1(long n, long a0_)
{
    register long syscall_id asm("a7") = n;
    register long a0 asm("a0") = a0_;
    asm volatile("ecall" : "+r"(a0) : "r"(syscall_id));
    return a0;
}

static inline long
syscall_2(long n, long a0_, long a1_)
{
    register long syscall_id asm("a7") = n;
    register long a0 asm("a0") = a0_;
    register long a1 asm("a1") = a1_;
    asm volatile("ecall" : "+r"(a0) : "r"(a1), "r"(syscall_id));
    return a0;
}

static inline long
syscall_4(long n, long a0_, long a1_, long a2_, long a3_)
{
    register long syscall_id asm("a7") = n;
    register long a0 asm("a0") = a0_;
    register long a1 asm("a1") = a1_;
    register long a2 asm("a2") = a2_;
    register long a3 asm("a3") = a3_;
    asm volatile("ecall" : "+r"(a0) : "r"(a1), "r"(a2), "r"(a3), "r"(syscall_id));
    return a0;
}

#endif
