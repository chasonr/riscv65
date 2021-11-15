; syscall.s -- implementation of system calls

; External parameters come from here
RISCV_BREAK = $07F4

.include "registers.inc"
.include "kernal.inc"
.include "errno.inc"
.include "reu.inc"

.import _RISCV_ireg_0
.import _RISCV_ireg_1
.import _RISCV_ireg_2
.import _RISCV_ireg_3
.importzp RISCV_fence

.bss

RISCV_break: .res 4
RISCV_min_break: .res 4

; Address and size for I/O
io_addr: .res 4
io_size: .res 4

; Structure for stat, fstat, lstat, fstatat
; Matches Newlib libgloss/riscv/kernel_stat.h
.struct kernel_stat
    st_dev     .dword 2
    st_ino     .dword 2
    st_mode    .dword
    st_nlink   .dword
    st_uid     .dword
    st_gid     .dword
    st_rdev    .dword 2
    __pad1     .dword 2
    st_size    .dword 2
    st_blksize .dword
    __pad2     .dword
    st_blocks  .dword 2
    st_atim    .dword 4 ; 8 bytes: tv_sec; 4 bytes: tv_nsec; 4 bytes: pad
    st_mtim    .dword 4
    st_ctim    .dword 4
    __pad3     .dword 2
.endstruct
kernel_stat_size = 128

; Structure for gettimeofday
; Matches struct timeval in Newlib newlib/libc/include/sys/_timeval.h
.struct timeval
    tv_sec  .dword 2
    tv_usec .dword
    pad     .dword
.endstruct
timeval_size = 16

; Transfer area for I/O
xfer_size = 16
io_xfer: .res 512

.segment "ZEROPAGE"
io_count: .res 1
io_index: .res 1
local_addr: .res 2

; U64 command interface

CMD_CONTROL        = $DF1C
CMD_STATUS         = $DF1C
CMD_DATA           = $DF1D
CMD_IDENTIFICATION = $DF1D
CMD_RESPONSE_DATA  = $DF1E
CMD_STATUS_DATA    = $DF1F

CMD_PUSH_CMD = $01
CMD_DATA_ACC = $02
CMD_ABORT    = $04
CMD_CLR_ERR  = $08

CMD_BUSY       = $01
CMD_ABORT_P    = $04
CMD_ERROR      = $08
CMD_STATE_MASK = $30
CMD_STATE_IDLE = $00
CMD_STATE_BUSY = $10
CMD_STATE_LAST = $20
CMD_STATE_MORE = $30
CMD_STAT_AV    = $40
CMD_DATA_AV    = $80

DOS_CMD_GET_TIME = $26

.code

; Initialize this system
.global _RISCV_syscall_init
.proc _RISCV_syscall_init

    ; Set the break
    lda RISCV_BREAK+0
    sta RISCV_break+0
    sta RISCV_min_break+0
    lda RISCV_BREAK+1
    sta RISCV_break+1
    sta RISCV_min_break+1
    lda RISCV_BREAK+2
    sta RISCV_break+2
    sta RISCV_min_break+2
    lda #1
    sta RISCV_break+3
    sta RISCV_min_break+3

    rts

.endproc

; SYS_faccessat    = bad_ecall
; SYS_openat       = bad_ecall

; Close the given file handle
; A0 = file handle
.global SYS_close
.proc SYS_close

    ; Ignore any attempt to close file 0, 1 or 2
    lda _RISCV_ireg_1+REG_a0
    ora _RISCV_ireg_2+REG_a0
    ora _RISCV_ireg_3+REG_a0
    bne close_file
    lda _RISCV_ireg_0+REG_a0
    cmp #3
    bcs close_file

        lda #0
        sta _RISCV_ireg_0+REG_a0
        sta _RISCV_ireg_1+REG_a0
        sta _RISCV_ireg_2+REG_a0
        sta _RISCV_ireg_3+REG_a0
        rts

    close_file:

    ; File I/O not yet implemented
    set_errno EBADF
    rts

.endproc

; SYS_lseek        = bad_ecall
; SYS_read         = bad_ecall

; Write to the given file handle
; A0 = file handle
; A1 = pointer to data
; A2 = length of data

.global SYS_write
.proc SYS_write

    ; Set up the transfer area
    lda _RISCV_ireg_0+REG_a1
    sta io_addr+0
    lda _RISCV_ireg_1+REG_a1
    sta io_addr+1
    lda _RISCV_ireg_2+REG_a1
    sta io_addr+2
    lda _RISCV_ireg_3+REG_a1
    sta io_addr+3
    lda _RISCV_ireg_0+REG_a2
    sta io_size+0
    lda _RISCV_ireg_1+REG_a2
    sta io_size+1
    lda _RISCV_ireg_2+REG_a2
    sta io_size+2
    lda _RISCV_ireg_3+REG_a2
    sta io_size+3
    jsr check_read
    bcc read_ok
        set_errno EFAULT
        rts
    read_ok:

    ; Check for special file handles 0, 1 and 2
    lda _RISCV_ireg_1+REG_a0
    ora _RISCV_ireg_2+REG_a0
    ora _RISCV_ireg_3+REG_a0
    bne check_file
    lda _RISCV_ireg_0+REG_a0
    bne check_output
        ; Handle 0 (standard input; not valid)
        set_errno EBADF
        rts
    check_output:
    cmp #3
    bcs check_file
        ; Handle 1 (standard output) or 2 (standard error)
        @write_loop:
            jsr read_io_block
            lda io_count
            beq @end_write_loop
            lda #0
            sta io_index
            @write_byte:
                ldx io_index
                ldy io_xfer,x
                lda ascii_to_pet,y
                jsr CHROUT
                inc io_index
            dec io_count
            bne @write_byte
        beq @write_loop
        @end_write_loop:
        lda _RISCV_ireg_0+REG_a2
        sta _RISCV_ireg_0+REG_a0
        lda _RISCV_ireg_1+REG_a2
        sta _RISCV_ireg_1+REG_a0
        lda _RISCV_ireg_2+REG_a2
        sta _RISCV_ireg_2+REG_a0
        lda _RISCV_ireg_3+REG_a2
        sta _RISCV_ireg_3+REG_a0
        rts
    check_file:

    ; File I/O not yet implemented
    set_errno EBADF
    rts

.endproc

; SYS_fstatat      = bad_ecall

; Return information about an open file handle
; A0 = file handle
; A1 = address of stat structure
;      This is struct kernel_stat in Newlib libgloss/riscv/kernel_stat.h
.global SYS_fstat
.proc SYS_fstat

    ; Set up the transfer area
    lda _RISCV_ireg_0+REG_a1
    sta io_addr+0
    lda _RISCV_ireg_1+REG_a1
    sta io_addr+1
    lda _RISCV_ireg_2+REG_a1
    sta io_addr+2
    lda _RISCV_ireg_3+REG_a1
    sta io_addr+3
    lda #kernel_stat_size
    sta io_size+0
    lda #0
    sta io_size+1
    sta io_size+2
    sta io_size+3
    jsr check_write
    bcc write_ok
        set_errno EFAULT
        rts
    write_ok:

    ; Clear the transfer area
    ldx #kernel_stat_size
    lda #0
    clear:
        sta io_xfer-1,x
    dex
    bne clear

    ; Check for special file handles 0, 1 and 2
    lda _RISCV_ireg_1+REG_a0
    ora _RISCV_ireg_2+REG_a0
    ora _RISCV_ireg_3+REG_a0
    bne check_file
    lda _RISCV_ireg_0+REG_a0
    bne check_output
        ; Handle 0 (standard input)
        lda #<020444            ; Character device, writable
        sta io_xfer+kernel_stat::st_mode+0
        lda #>020444
        sta io_xfer+kernel_stat::st_mode+1
        lda #0
        sta io_xfer+kernel_stat::st_mode+2
        sta io_xfer+kernel_stat::st_mode+3
        sta _RISCV_ireg_0+REG_a0
        sta _RISCV_ireg_1+REG_a0
        sta _RISCV_ireg_2+REG_a0
        sta _RISCV_ireg_3+REG_a0
        jmp write_io_xfer
    check_output:
    cmp #3
    bcs check_file
        ; Handle 1 (standard output) or 2 (standard error)
        lda #<020222            ; Character device, readable
        sta io_xfer+kernel_stat::st_mode+0
        lda #>020222
        sta io_xfer+kernel_stat::st_mode+1
        lda #0
        sta io_xfer+kernel_stat::st_mode+2
        sta io_xfer+kernel_stat::st_mode+3
        sta _RISCV_ireg_0+REG_a0
        sta _RISCV_ireg_1+REG_a0
        sta _RISCV_ireg_2+REG_a0
        sta _RISCV_ireg_3+REG_a0
        jmp write_io_xfer
    check_file:

    ; File I/O not yet implemented
    set_errno EBADF
    rts

.endproc

; Get time of day
; A0 == pointer to struct timeval that will receive the time
.global SYS_gettimeofday
.proc SYS_gettimeofday

    ; Set up the transfer area
    lda _RISCV_ireg_0+REG_a0
    sta io_addr+0
    lda _RISCV_ireg_1+REG_a0
    sta io_addr+1
    lda _RISCV_ireg_2+REG_a0
    sta io_addr+2
    lda _RISCV_ireg_3+REG_a0
    sta io_addr+3
    lda #timeval_size
    sta io_size+0
    lda #0
    sta io_size+1
    sta io_size+2
    sta io_size+3
    jsr check_write
    bcc write_ok
        set_errno EFAULT
        rts
    write_ok:

    ; Get time from the U64
    lda #1
    sta io_xfer+0
    lda #DOS_CMD_GET_TIME
    sta io_xfer+1
    lda #0
    sta io_xfer+2
    ldx #3
    jsr do_command

    ; DOS_CMD_GET_TIME always succeeds
    ; Response is yyyy/mm/dd hh:mm:ss
    ; Convert the components to integer form
    ; Year is 1980-2079

    lda io_xfer+0 ; 1000s of year
    and #$0F
    tax
    lda thousand_lo,x
    sta io_xfer+32

    lda io_xfer+1 ; 100s of year
    and #$0F
    tax
    clc
    lda io_xfer+32
    adc hundred_lo,x
    sta io_xfer+32

    lda io_xfer+2 ; 10s of year
    and #$0F
    tax
    lda io_xfer+3 ; 1s of year
    and #$0F
    clc
    adc ten,x
    ; the add never carries
    adc io_xfer+32
    sta io_xfer+32

    ; io_xfer+32 == year % 256

    ; Subtract 1970 from year to get years from epoch
    sec
    sbc #<1970
    tay

    ; Y == year - 1970

    ; Look up days from epoch for the year
    lda year_to_day_lo,y
    sta io_xfer+33
    lda year_to_day_hi,y
    sta io_xfer+34

    ; Convert month to integer

    lda io_xfer+5   ; 10s of month
    and #$0F
    tax
    lda io_xfer+6   ; 1s of month
    and #$0F
    clc
    adc ten,x
    tax

    ; X = month
    ; Look up days from start of year
    clc
    lda io_xfer+33
    adc month_to_day_lo-1,x
    sta io_xfer+33
    lda io_xfer+34
    adc month_to_day_hi-1,x
    sta io_xfer+34

    ; Account for February 29
    cpx #$03
    bcc end_leap        ; month is January or February
    lda io_xfer+32
    and #$03
    bne end_leap        ; year divides by 4
        inc io_xfer+33
        bne end_leap
        inc io_xfer+34
    end_leap:

    ; Convert day to integer

    lda io_xfer+8   ; 10s of day
    and #$0F
    tax
    lda io_xfer+9   ; 1s of day
    and #$0F
    clc
    adc ten,x
    sec
    sbc #1
    clc
    adc io_xfer+33
    sta io_xfer+33
    lda #0
    adc io_xfer+34
    sta io_xfer+34

    ; io_xfer+33,34 == days from epoch
    ; Convert to hours
    lda #0
    .repeat 3
        asl io_xfer+33
        rol io_xfer+34
        rol a
    .endrep
    sta io_xfer+35      ; io_xfer+33,34,35 == days*8
    lda io_xfer+33
    asl a
    sta io_xfer+36
    lda io_xfer+34
    rol a
    sta io_xfer+37
    lda io_xfer+35
    rol a
    sta io_xfer+38      ; io_xfer+36,37,38 == days*16
    clc
    lda io_xfer+33
    adc io_xfer+36
    sta io_xfer+33
    lda io_xfer+34
    adc io_xfer+37
    sta io_xfer+34
    lda io_xfer+35
    adc io_xfer+38
    sta io_xfer+35      ; io_xfer+33,34,35 = days*24

    ; Convert hour to integer

    lda io_xfer+11  ; 10s of hour
    and #$0F
    tax
    lda io_xfer+12  ; 1s of hour
    and #$0F
    clc
    adc ten,x
    clc
    adc io_xfer+33
    sta io_xfer+33
    lda #0
    adc io_xfer+34
    sta io_xfer+34
    lda #0
    adc io_xfer+35
    sta io_xfer+35

    ; io_xfer+33,34,35 == hours from epoch
    ; Convert to minutes

    lda io_xfer+33
    sta io_xfer+37
    lda io_xfer+34
    sta io_xfer+38
    lda io_xfer+35
    sta io_xfer+39      ; io_xfer+37,38,39 == hours
    lda #0
    .repeat 4
        asl io_xfer+33
        rol io_xfer+34
        rol io_xfer+35
        rol a
    .endrep
    sta io_xfer+36      ; io_xfer+33,34,35,36 == hours*16
    sec
    lda io_xfer+33
    sbc io_xfer+37
    sta io_xfer+33
    lda io_xfer+34
    sbc io_xfer+38
    sta io_xfer+34
    lda io_xfer+35
    sbc io_xfer+39
    sta io_xfer+35
    lda io_xfer+36
    sbc #0              ; io_xfer+33,34,35,A == hours*15
    .repeat 2
        asl io_xfer+33
        rol io_xfer+34
        rol io_xfer+35
        rol a
    .endrep
    sta io_xfer+36      ; io_xfer+33,34,35,36 == hours*60

    ; Convert minute to integer

    lda io_xfer+14  ; 10s of minute
    and #$0F
    tax
    lda io_xfer+15  ; 1s of minute
    and #$0F
    clc
    adc ten,x
    clc
    adc io_xfer+33
    sta io_xfer+33
    lda #0
    adc io_xfer+34
    sta io_xfer+34
    lda #0
    adc io_xfer+35
    sta io_xfer+35
    lda #0
    adc io_xfer+36
    sta io_xfer+36

    ; io_xfer+33,34,35,36 == minutes from epoch
    ; Convert to seconds

    lda io_xfer+33
    sta io_xfer+37
    lda io_xfer+34
    sta io_xfer+38
    lda io_xfer+35
    sta io_xfer+39
    lda io_xfer+36
    sta io_xfer+40      ; io_xfer+37,38,39,40 == minutes
    .repeat 4
        asl io_xfer+33
        rol io_xfer+34
        rol io_xfer+35
        rol a
    .endrep
    sta io_xfer+36      ; io_xfer+33,34,35,36 == minutes*16
    sec
    lda io_xfer+33
    sbc io_xfer+37
    sta io_xfer+33
    lda io_xfer+34
    sbc io_xfer+38
    sta io_xfer+34
    lda io_xfer+35
    sbc io_xfer+39
    sta io_xfer+35
    lda io_xfer+36
    sbc io_xfer+40      ; io_xfer+33,34,35,A == minutes*15
    .repeat 2
        asl io_xfer+33
        rol io_xfer+34
        rol io_xfer+35
        rol a
    .endrep
    sta io_xfer+36      ; io_xfer+33,34,35,36 == minutes*60

    ; Convert second to integer and write to io_xfer+0

    lda io_xfer+17  ; 10s of second
    and #$0F
    tax
    lda io_xfer+18  ; 1s of second
    and #$0F
    clc
    adc ten,x
    clc
    adc io_xfer+33
    sta io_xfer+0
    lda #0
    adc io_xfer+34
    sta io_xfer+1
    lda #0
    adc io_xfer+35
    sta io_xfer+2
    lda #0
    adc io_xfer+36
    sta io_xfer+3

    ; Fill out to a struct timeval
    ldx #11
    lda #0
    zero:
        sta io_xfer+4,x
    dex
    bpl zero

    jsr write_io_xfer

    lda #0
    sta _RISCV_ireg_0+REG_a0
    sta _RISCV_ireg_1+REG_a0
    sta _RISCV_ireg_2+REG_a0
    sta _RISCV_ireg_3+REG_a0
    rts

year_to_day_lo:
    .byte <(  0*365+ 0) ; 1970
    .byte <(  1*365+ 0) ; 1971
    .byte <(  2*365+ 0) ; 1972
    .byte <(  3*365+ 1) ; 1973
    .byte <(  4*365+ 1) ; 1974
    .byte <(  5*365+ 1) ; 1975
    .byte <(  6*365+ 1) ; 1976
    .byte <(  7*365+ 2) ; 1977
    .byte <(  8*365+ 2) ; 1978
    .byte <(  9*365+ 2) ; 1979
    .byte <( 10*365+ 2) ; 1980
    .byte <( 11*365+ 3) ; 1981
    .byte <( 12*365+ 3) ; 1982
    .byte <( 13*365+ 3) ; 1983
    .byte <( 14*365+ 3) ; 1984
    .byte <( 15*365+ 4) ; 1985
    .byte <( 16*365+ 4) ; 1986
    .byte <( 17*365+ 4) ; 1987
    .byte <( 18*365+ 4) ; 1988
    .byte <( 19*365+ 5) ; 1989
    .byte <( 20*365+ 5) ; 1990
    .byte <( 21*365+ 5) ; 1991
    .byte <( 22*365+ 5) ; 1992
    .byte <( 23*365+ 6) ; 1993
    .byte <( 24*365+ 6) ; 1994
    .byte <( 25*365+ 6) ; 1995
    .byte <( 26*365+ 6) ; 1996
    .byte <( 27*365+ 7) ; 1997
    .byte <( 28*365+ 7) ; 1998
    .byte <( 29*365+ 7) ; 1999
    .byte <( 30*365+ 7) ; 2000
    .byte <( 31*365+ 8) ; 2001
    .byte <( 32*365+ 8) ; 2002
    .byte <( 33*365+ 8) ; 2003
    .byte <( 34*365+ 8) ; 2004
    .byte <( 35*365+ 9) ; 2005
    .byte <( 36*365+ 9) ; 2006
    .byte <( 37*365+ 9) ; 2007
    .byte <( 38*365+ 9) ; 2008
    .byte <( 39*365+10) ; 2009
    .byte <( 40*365+10) ; 2010
    .byte <( 41*365+10) ; 2011
    .byte <( 42*365+10) ; 2012
    .byte <( 43*365+11) ; 2013
    .byte <( 44*365+11) ; 2014
    .byte <( 45*365+11) ; 2015
    .byte <( 46*365+11) ; 2016
    .byte <( 47*365+12) ; 2017
    .byte <( 48*365+12) ; 2018
    .byte <( 49*365+12) ; 2019
    .byte <( 50*365+12) ; 2020
    .byte <( 51*365+13) ; 2021
    .byte <( 52*365+13) ; 2022
    .byte <( 53*365+13) ; 2023
    .byte <( 54*365+13) ; 2024
    .byte <( 55*365+14) ; 2025
    .byte <( 56*365+14) ; 2026
    .byte <( 57*365+14) ; 2027
    .byte <( 58*365+14) ; 2028
    .byte <( 59*365+15) ; 2029
    .byte <( 60*365+15) ; 2030
    .byte <( 61*365+15) ; 2031
    .byte <( 62*365+15) ; 2032
    .byte <( 63*365+16) ; 2033
    .byte <( 64*365+16) ; 2034
    .byte <( 65*365+16) ; 2035
    .byte <( 66*365+16) ; 2036
    .byte <( 67*365+17) ; 2037
    .byte <( 68*365+17) ; 2038
    .byte <( 69*365+17) ; 2039
    .byte <( 70*365+17) ; 2040
    .byte <( 71*365+18) ; 2041
    .byte <( 72*365+18) ; 2042
    .byte <( 73*365+18) ; 2043
    .byte <( 74*365+18) ; 2044
    .byte <( 75*365+19) ; 2045
    .byte <( 76*365+19) ; 2046
    .byte <( 77*365+19) ; 2047
    .byte <( 78*365+19) ; 2048
    .byte <( 79*365+20) ; 2049
    .byte <( 80*365+20) ; 2050
    .byte <( 81*365+20) ; 2051
    .byte <( 82*365+20) ; 2052
    .byte <( 83*365+21) ; 2053
    .byte <( 84*365+21) ; 2054
    .byte <( 85*365+21) ; 2055
    .byte <( 86*365+21) ; 2056
    .byte <( 87*365+22) ; 2057
    .byte <( 88*365+22) ; 2058
    .byte <( 89*365+22) ; 2059
    .byte <( 90*365+22) ; 2060
    .byte <( 91*365+23) ; 2061
    .byte <( 92*365+23) ; 2062
    .byte <( 93*365+23) ; 2063
    .byte <( 94*365+23) ; 2064
    .byte <( 95*365+24) ; 2065
    .byte <( 96*365+24) ; 2066
    .byte <( 97*365+24) ; 2067
    .byte <( 98*365+24) ; 2068
    .byte <( 99*365+25) ; 2069
    .byte <(100*365+25) ; 2070
    .byte <(101*365+25) ; 2071
    .byte <(102*365+25) ; 2072
    .byte <(103*365+26) ; 2073
    .byte <(104*365+26) ; 2074
    .byte <(105*365+26) ; 2075
    .byte <(106*365+26) ; 2076
    .byte <(107*365+27) ; 2077
    .byte <(108*365+27) ; 2078
    .byte <(109*365+27) ; 2079
    .byte <(110*365+27) ; 2080
    .byte <(111*365+28) ; 2081
    .byte <(112*365+28) ; 2082
    .byte <(113*365+28) ; 2083
    .byte <(114*365+28) ; 2084
    .byte <(115*365+29) ; 2085
    .byte <(116*365+29) ; 2086
    .byte <(117*365+29) ; 2087
    .byte <(118*365+29) ; 2088
    .byte <(119*365+30) ; 2089
    .byte <(120*365+30) ; 2090
    .byte <(121*365+30) ; 2091
    .byte <(122*365+30) ; 2092
    .byte <(123*365+31) ; 2093
    .byte <(124*365+31) ; 2094
    .byte <(125*365+31) ; 2095
    .byte <(126*365+31) ; 2096
    .byte <(127*365+32) ; 2097
    .byte <(128*365+32) ; 2098
    .byte <(129*365+32) ; 2099

year_to_day_hi:
    .byte >(  0*365+ 0) ; 1970
    .byte >(  1*365+ 0) ; 1971
    .byte >(  2*365+ 0) ; 1972
    .byte >(  3*365+ 1) ; 1973
    .byte >(  4*365+ 1) ; 1974
    .byte >(  5*365+ 1) ; 1975
    .byte >(  6*365+ 1) ; 1976
    .byte >(  7*365+ 2) ; 1977
    .byte >(  8*365+ 2) ; 1978
    .byte >(  9*365+ 2) ; 1979
    .byte >( 10*365+ 2) ; 1980
    .byte >( 11*365+ 3) ; 1981
    .byte >( 12*365+ 3) ; 1982
    .byte >( 13*365+ 3) ; 1983
    .byte >( 14*365+ 3) ; 1984
    .byte >( 15*365+ 4) ; 1985
    .byte >( 16*365+ 4) ; 1986
    .byte >( 17*365+ 4) ; 1987
    .byte >( 18*365+ 4) ; 1988
    .byte >( 19*365+ 5) ; 1989
    .byte >( 20*365+ 5) ; 1990
    .byte >( 21*365+ 5) ; 1991
    .byte >( 22*365+ 5) ; 1992
    .byte >( 23*365+ 6) ; 1993
    .byte >( 24*365+ 6) ; 1994
    .byte >( 25*365+ 6) ; 1995
    .byte >( 26*365+ 6) ; 1996
    .byte >( 27*365+ 7) ; 1997
    .byte >( 28*365+ 7) ; 1998
    .byte >( 29*365+ 7) ; 1999
    .byte >( 30*365+ 7) ; 2000
    .byte >( 31*365+ 8) ; 2001
    .byte >( 32*365+ 8) ; 2002
    .byte >( 33*365+ 8) ; 2003
    .byte >( 34*365+ 8) ; 2004
    .byte >( 35*365+ 9) ; 2005
    .byte >( 36*365+ 9) ; 2006
    .byte >( 37*365+ 9) ; 2007
    .byte >( 38*365+ 9) ; 2008
    .byte >( 39*365+10) ; 2009
    .byte >( 40*365+10) ; 2010
    .byte >( 41*365+10) ; 2011
    .byte >( 42*365+10) ; 2012
    .byte >( 43*365+11) ; 2013
    .byte >( 44*365+11) ; 2014
    .byte >( 45*365+11) ; 2015
    .byte >( 46*365+11) ; 2016
    .byte >( 47*365+12) ; 2017
    .byte >( 48*365+12) ; 2018
    .byte >( 49*365+12) ; 2019
    .byte >( 50*365+12) ; 2020
    .byte >( 51*365+13) ; 2021
    .byte >( 52*365+13) ; 2022
    .byte >( 53*365+13) ; 2023
    .byte >( 54*365+13) ; 2024
    .byte >( 55*365+14) ; 2025
    .byte >( 56*365+14) ; 2026
    .byte >( 57*365+14) ; 2027
    .byte >( 58*365+14) ; 2028
    .byte >( 59*365+15) ; 2029
    .byte >( 60*365+15) ; 2030
    .byte >( 61*365+15) ; 2031
    .byte >( 62*365+15) ; 2032
    .byte >( 63*365+16) ; 2033
    .byte >( 64*365+16) ; 2034
    .byte >( 65*365+16) ; 2035
    .byte >( 66*365+16) ; 2036
    .byte >( 67*365+17) ; 2037
    .byte >( 68*365+17) ; 2038
    .byte >( 69*365+17) ; 2039
    .byte >( 70*365+17) ; 2040
    .byte >( 71*365+18) ; 2041
    .byte >( 72*365+18) ; 2042
    .byte >( 73*365+18) ; 2043
    .byte >( 74*365+18) ; 2044
    .byte >( 75*365+19) ; 2045
    .byte >( 76*365+19) ; 2046
    .byte >( 77*365+19) ; 2047
    .byte >( 78*365+19) ; 2048
    .byte >( 79*365+20) ; 2049
    .byte >( 80*365+20) ; 2050
    .byte >( 81*365+20) ; 2051
    .byte >( 82*365+20) ; 2052
    .byte >( 83*365+21) ; 2053
    .byte >( 84*365+21) ; 2054
    .byte >( 85*365+21) ; 2055
    .byte >( 86*365+21) ; 2056
    .byte >( 87*365+22) ; 2057
    .byte >( 88*365+22) ; 2058
    .byte >( 89*365+22) ; 2059
    .byte >( 90*365+22) ; 2060
    .byte >( 91*365+23) ; 2061
    .byte >( 92*365+23) ; 2062
    .byte >( 93*365+23) ; 2063
    .byte >( 94*365+23) ; 2064
    .byte >( 95*365+24) ; 2065
    .byte >( 96*365+24) ; 2066
    .byte >( 97*365+24) ; 2067
    .byte >( 98*365+24) ; 2068
    .byte >( 99*365+25) ; 2069
    .byte >(100*365+25) ; 2070
    .byte >(101*365+25) ; 2071
    .byte >(102*365+25) ; 2072
    .byte >(103*365+26) ; 2073
    .byte >(104*365+26) ; 2074
    .byte >(105*365+26) ; 2075
    .byte >(106*365+26) ; 2076
    .byte >(107*365+27) ; 2077
    .byte >(108*365+27) ; 2078
    .byte >(109*365+27) ; 2079
    .byte >(110*365+27) ; 2080
    .byte >(111*365+28) ; 2081
    .byte >(112*365+28) ; 2082
    .byte >(113*365+28) ; 2083
    .byte >(114*365+28) ; 2084
    .byte >(115*365+29) ; 2085
    .byte >(116*365+29) ; 2086
    .byte >(117*365+29) ; 2087
    .byte >(118*365+29) ; 2088
    .byte >(119*365+30) ; 2089
    .byte >(120*365+30) ; 2090
    .byte >(121*365+30) ; 2091
    .byte >(122*365+30) ; 2092
    .byte >(123*365+31) ; 2093
    .byte >(124*365+31) ; 2094
    .byte >(125*365+31) ; 2095
    .byte >(126*365+31) ; 2096
    .byte >(127*365+32) ; 2097
    .byte >(128*365+32) ; 2098
    .byte >(129*365+32) ; 2099

; Month to day of year
month_to_day_lo:
    .byte <0        ; January
    .byte <31       ; February
    .byte <59       ; March
    .byte <90       ; April
    .byte <120      ; May
    .byte <151      ; June
    .byte <181      ; July
    .byte <212      ; August
    .byte <243      ; September
    .byte <273      ; October
    .byte <304      ; November
    .byte <334      ; December

month_to_day_hi:
    .byte >0        ; January
    .byte >31       ; February
    .byte >59       ; March
    .byte >90       ; April
    .byte >120      ; May
    .byte >151      ; June
    .byte >181      ; July
    .byte >212      ; August
    .byte >243      ; September
    .byte >273      ; October
    .byte >304      ; November
    .byte >334      ; December

.endproc

.global SYS_brk
.proc SYS_brk

    ; If A0 == 0, query the current break; this always succeeds
    lda _RISCV_ireg_0+REG_a0
    ora _RISCV_ireg_1+REG_a0
    ora _RISCV_ireg_2+REG_a0
    ora _RISCV_ireg_3+REG_a0
    bne set_break

        lda RISCV_break+0
        sta _RISCV_ireg_0+REG_a0
        lda RISCV_break+1
        sta _RISCV_ireg_1+REG_a0
        lda RISCV_break+2
        sta _RISCV_ireg_2+REG_a0
        lda RISCV_break+3
        sta _RISCV_ireg_3+REG_a0
        rts

    set_break:

    ; A0 != 0 sets a new break
    ; The new break must be in space 1 (the REU)
    lda _RISCV_ireg_3+REG_a0
    cmp #1
    bne error

    ; The new break must not be less than the initial break
    lda _RISCV_ireg_0+REG_a0
    cmp RISCV_min_break+0
    lda _RISCV_ireg_1+REG_a0
    sbc RISCV_min_break+1
    lda _RISCV_ireg_2+REG_a0
    sbc RISCV_min_break+2
    lda _RISCV_ireg_3+REG_a0
    sbc RISCV_min_break+3
    bcc error

    ; The new break must be less than the current stack pointer
    lda _RISCV_ireg_0+REG_a0
    cmp _RISCV_ireg_0+REG_sp
    lda _RISCV_ireg_1+REG_a0
    sbc _RISCV_ireg_1+REG_sp
    lda _RISCV_ireg_2+REG_a0
    sbc _RISCV_ireg_2+REG_sp
    lda _RISCV_ireg_3+REG_a0
    sbc _RISCV_ireg_3+REG_sp
    bcs error

    ; Set the new break and return the current break
    lda _RISCV_ireg_0+REG_a0
    ldx RISCV_break+0
    stx _RISCV_ireg_0+REG_a0
    sta RISCV_break+0
    lda _RISCV_ireg_1+REG_a0
    ldx RISCV_break+1
    stx _RISCV_ireg_1+REG_a0
    sta RISCV_break+1
    lda _RISCV_ireg_2+REG_a0
    ldx RISCV_break+2
    stx _RISCV_ireg_2+REG_a0
    sta RISCV_break+2
    lda _RISCV_ireg_3+REG_a0
    ldx RISCV_break+3
    stx _RISCV_ireg_3+REG_a0
    sta RISCV_break+3
    rts

error:
    lda #$FF
    sta _RISCV_ireg_0+REG_a0
    sta _RISCV_ireg_1+REG_a0
    sta _RISCV_ireg_2+REG_a0
    sta _RISCV_ireg_3+REG_a0
    rts

.endproc

; SYS_open         = bad_ecall

; Create a hard link
; Newlib can call this, but the file system doesn't support it
.global SYS_link
.proc SYS_link

    set_errno ENOTSUP
    rts

.endproc

; SYS_unlink       = bad_ecall
; SYS_access       = bad_ecall
; SYS_stat         = bad_ecall
; SYS_lstat        = bad_ecall

; Check the configured address and size for valid write
; Set C if address and size are invalid
; Must be placed immediately before check_read so a branch will reach
.proc check_write

    ; Requirement is the same as check_read, except that the address must also
    ; be above the fence
    lda RISCV_fence
    cmp io_addr+2
    bcc check_read
    rts

.endproc

; Check the configured address and size for valid read
; Set C if address and size are invalid
.proc check_read

    ; I/O is to Space 1 (the REU) only
    ; The checks done here will not allow I/O to the last byte of the REU.
    ; This address is within the area used to build the parameters to main(),
    ; and is unlikely to be used for I/O.

    lda io_addr+3
    cmp #1
    bne bad_read

    ; I/O shall not exceed 16 MB
    lda io_size+3
    bne bad_read

    ; I/O shall not exceed the end of the REU
    clc
    lda io_addr+0
    adc io_size+0
    lda io_addr+1
    adc io_size+1
    lda io_addr+2
    adc io_size+2
    ; C set if the add overflows

    rts

bad_read:
    sec
    rts

.endproc

; Read next block into io_xfer
; Return size in io_count; update io_addr and io_size
.proc read_io_block

    ; Determine the size for this transfer
    lda io_size+1
    ora io_size+2
    beq check_size_0
        lda #xfer_size
        bne got_size
    check_size_0:
        lda io_size+0
        cmp #xfer_size
        bcc got_size
            lda #xfer_size
    got_size:
    sta io_count
    cmp #0
    beq end_read

    ; Space 1: RAM Expansion Unit
    sta reu_xfer_size_0
    set_reu_address io_addr
    set_local_address io_xfer
    lda #0
    sta reu_xfer_size_1
    do_reu_read

    ; Update io_addr and io_size
    clc
    lda io_addr+0
    adc io_count
    sta io_addr+0
    lda io_addr+1
    adc #0
    sta io_addr+1
    lda io_addr+2
    adc #0
    sta io_addr+2
    sec
    lda io_size+0
    sbc io_count
    sta io_size+0
    lda io_size+1
    sbc #0
    sta io_size+1
    lda io_size+2
    sbc #0
    sta io_size+2

end_read:
    rts

.endproc

; Pass the io_xfer area to the RISC-V target
.proc write_io_xfer

    set_reu_address io_addr
    set_xfer_size io_size
    set_local_address io_xfer
    do_reu_write
    rts

.endproc

; Pass command to the U64 firmware and receive the response
; Command is in io_xfer; its size is in X; X=0 for 256 bytes
; Return response in io_xfer+0, with size in X, and status in io_xfer+256,
; with size in Y
.proc do_command

    ; Send command
    ldy #0
    send_command:
        lda io_xfer,y
        sta CMD_DATA
        iny
    dex
    bne send_command
    lda #CMD_PUSH_CMD
    sta CMD_CONTROL

    ; Wait for completion
    wait:
        lda CMD_STATUS
        and #CMD_STATE_MASK
        cmp #CMD_STATE_BUSY
    beq wait

    ; Read data response
    ldx #0
    get_data:
        lda CMD_STATUS
        and #CMD_DATA_AV
        beq end_data
        lda CMD_RESPONSE_DATA
        sta io_xfer+0,x
    inx
    cpx #255
    bne get_data
    end_data:

    ; Read status response
    ldy #0
    get_status:
        lda CMD_STATUS
        and #CMD_STAT_AV
        beq end_status
        lda CMD_STATUS_DATA
        sta io_xfer+256,y
    iny
    cpy #255
    bne get_status
    end_status:

    ; Acknowledge the response
    lda #CMD_DATA_ACC
    sta CMD_CONTROL

    ; If either response is a string, null-terminate it
    lda #0
    sta io_xfer+0,x
    sta io_xfer+256,y

    rts

.endproc

.data
thousand_lo:
    .byte <0
    .byte <1000
    .byte <2000
    .byte <3000
    .byte <4000
    .byte <5000
    .byte <6000
    .byte <7000
    .byte <8000
    .byte <9000

thousand_hi:
    .byte >0
    .byte >1000
    .byte >2000
    .byte >3000
    .byte >4000
    .byte >5000
    .byte >6000
    .byte >7000
    .byte >8000
    .byte >9000

hundred_lo:
    .byte <0
    .byte <100
    .byte <200
    .byte <300
    .byte <400
    .byte <500
    .byte <600
    .byte <700
    .byte <800
    .byte <900

hundred_hi:
    .byte >0
    .byte >100
    .byte >200
    .byte >300
    .byte >400
    .byte >500
    .byte >600
    .byte >700
    .byte >800
    .byte >900

ten:
    .byte 0
    .byte 10
    .byte 20
    .byte 30
    .byte 40
    .byte 50
    .byte 60
    .byte 70
    .byte 80
    .byte 90

.segment "PAGEALIGN"
.align 256

ascii_to_pet:
    ; $00 to $40
    .repeat 65,i
        .byte i+0
    .endrep
    ; $41 to $5A
    .repeat 26,i
        .byte i+$C1
    .endrep
    ; $5B to $60
    .repeat 6,i
        .byte i+$5B
    .endrep
    ; $61 to $7A
    .repeat 26,i
        .byte i+$41
    .endrep
    ; $7B to $FF
    .repeat 133,i
        .byte i+$7B
    .endrep
