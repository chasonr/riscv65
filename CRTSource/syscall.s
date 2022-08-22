; syscall.s -- implement system calls via ECALL and EBREAK

.macpack longbranch
.include "syscall.inc"
.include "registers.inc"
.include "memory.inc"
.include "farcall.inc"
.include "error.inc"
.include "kernal.inc"
.include "errno.inc"
.include "reu.inc"
.include "stat.inc"

.proc RISCV_syscall_init

    ; Stub
    rts

.endproc

.proc RISCV_syscall_exit

    ; Stub
    rts

.endproc

.proc RISCV_EBREAK

    ; No EBREAK calls are currently defined
    jmp bad_ebreak

.endproc

.proc RISCV_ECALL

    ; ECALL selector is in A7 (x17)
    lda RISCV_ireg_3+REG_a7
    bpl :+
        ; Work in progress: provide for user-definable 6502 code at $C000
        ; User-defined syscalls are in range $80000000-$FFFFFFFF
        jmp $C000
    :

    ; ECALLs from libkernal.a are in range $7F000000-$7F0000FF
    cmp #$7F
    bne newlib_call

        ; Check bytes 1 and 2
        lda RISCV_ireg_1+REG_a7
        ora RISCV_ireg_2+REG_a7
        bne no_table

        ; Jump to appropriate Kernal system call
        ldx RISCV_ireg_0+REG_a7
        cpx #end_ecall_kernal-ecall_kernal_lo
        bcs no_table
        lda ecall_kernal_lo,x
        sta pointer1+0
        lda ecall_kernal_hi,x
        sta pointer1+1
        jmp (pointer1)

    newlib_call:
    ; ECALLs from Newlib have the two upper bytes equal to zero
    ora RISCV_ireg_2+REG_a7
    bne no_table

        ; Perform binary search on syscall_table
        lda #0
        sta pointer1+0      ; Lower bound
        lda #end_syscall_table-syscall_table
        sta pointer1+1      ; Upper bound

        search:
            ; Get the offset to the item to compare
            lda pointer1+0
            clc
            adc pointer1+1
            ror a           ; Catch the carry and divide by 2
            and #$FC        ; Offset is a multiple of 4
            tax
            ; Compare the system call number to the register
            sec
            lda syscall_table+0,x
            sbc RISCV_ireg_0+REG_a7
            sta pointer2+0
            lda syscall_table+1,x
            sbc RISCV_ireg_1+REG_a7
            ; If match, jump to the system call
            ora pointer2+0
            bne no_match
                lda syscall_table+2,x
                sta pointer2+0
                lda syscall_table+3,x
                sta pointer2+1
                jmp (pointer2)
            no_match:
            ; Otherwise, reset the boundaries to exclude the current record
            bcs lower
                ; The call number in the table is less than the one in the register
                inx
                inx
                inx
                inx
                stx pointer1+0
                bcc retry
            lower:
                ; The call number in the table is greater than the one in the register
                stx pointer1+1
        retry:
        lda pointer1+0
        cmp pointer1+1
        bcc search      ; while lower bound < upper bound
        ; else fall through to no_table

    no_table:
        jmp bad_ecall

.endproc

.rodata

; Kernal system calls
ecall_kernal_lo:
    .byte <CBM_acptr        ; 0x7F000000
    .byte <CBM_chkin        ; 0x7F000001
    .byte <CBM_chkout       ; 0x7F000002
    .byte <CBM_chrin        ; 0x7F000003
    .byte <CBM_chrout       ; 0x7F000004
    .byte <CBM_cint         ; 0x7F000005
    .byte <CBM_ciout        ; 0x7F000006
    .byte <CBM_clall        ; 0x7F000007
    .byte <CBM_close        ; 0x7F000008
    .byte <CBM_clrchn       ; 0x7F000009
    .byte <CBM_getin        ; 0x7F00000A
    .byte <CBM_ioinit       ; 0x7F00000B
    .byte <CBM_listen       ; 0x7F00000C
    .byte <CBM_open         ; 0x7F00000D
    .byte <CBM_get_cursor   ; 0x7F00000E
    .byte <CBM_set_cursor   ; 0x7F00000F
    .byte <CBM_rdtim        ; 0x7F000010
    .byte <CBM_readst       ; 0x7F000011
    .byte <CBM_second       ; 0x7F000012
    .byte <CBM_settim       ; 0x7F000013
    .byte <CBM_stop         ; 0x7F000014
    .byte <CBM_talk         ; 0x7F000015
    .byte <CBM_tksa         ; 0x7F000016
    .byte <CBM_unlsn        ; 0x7F000017
    .byte <CBM_untlk        ; 0x7F000018
end_ecall_kernal:
ecall_kernal_hi:
    .byte >CBM_acptr        ; 0x7F000000
    .byte >CBM_chkin        ; 0x7F000001
    .byte >CBM_chkout       ; 0x7F000002
    .byte >CBM_chrin        ; 0x7F000003
    .byte >CBM_chrout       ; 0x7F000004
    .byte >CBM_cint         ; 0x7F000005
    .byte >CBM_ciout        ; 0x7F000006
    .byte >CBM_clall        ; 0x7F000007
    .byte >CBM_close        ; 0x7F000008
    .byte >CBM_clrchn       ; 0x7F000009
    .byte >CBM_getin        ; 0x7F00000A
    .byte >CBM_ioinit       ; 0x7F00000B
    .byte >CBM_listen       ; 0x7F00000C
    .byte >CBM_open         ; 0x7F00000D
    .byte >CBM_get_cursor   ; 0x7F00000E
    .byte >CBM_set_cursor   ; 0x7F00000F
    .byte >CBM_rdtim        ; 0x7F000010
    .byte >CBM_readst       ; 0x7F000011
    .byte >CBM_second       ; 0x7F000012
    .byte >CBM_settim       ; 0x7F000013
    .byte >CBM_stop         ; 0x7F000014
    .byte >CBM_talk         ; 0x7F000015
    .byte >CBM_tksa         ; 0x7F000016
    .byte >CBM_unlsn        ; 0x7F000017
    .byte >CBM_untlk        ; 0x7F000018

; Table of system calls defined in or added to Newlib
; The indexes must be kept in numerical order, because a binary search finds
; the requested call
syscall_table:
    .word   17, SYS_getcwd
    .word   49, SYS_chdir
    .word   57, SYS_close
    .word   61, SYS_getdents
    .word   62, SYS_lseek
    .word   63, SYS_read
    .word   64, SYS_write
    .word   80, SYS_fstat
    .word   93, SYS_exit
    .word  169, SYS_gettimeofday
    .word  214, SYS_brk
    .word 1024, SYS_open
    .word 1025, SYS_link
    .word 1026, SYS_unlink
    .word 1030, SYS_mkdir
    .word 1033, SYS_access
    .word 1038, SYS_stat
    .word 1039, SYS_lstat
    .word 1152, SYS_rmdir
    .word 1153, SYS_rename
end_syscall_table:

.code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          POSIX-ish system calls                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Minimum handle number for file handle
first_file_handle = 3

; FIXME Stub out system calls not yet supported
SYS_getcwd       = bad_ecall
SYS_chdir        = bad_ecall
SYS_getdents     = bad_ecall
SYS_lseek        = bad_ecall
SYS_open         = bad_ecall
SYS_link         = bad_ecall
SYS_unlink       = bad_ecall
SYS_mkdir        = bad_ecall
SYS_access       = bad_ecall
SYS_stat         = bad_ecall
SYS_lstat        = bad_ecall
SYS_rmdir        = bad_ecall
SYS_rename       = bad_ecall

; Read from the given file handle
; A0 = file handle
; A1 = pointer to data
; A2 = length of data

.proc SYS_read

    ; Set up the transfer area
    lda RISCV_ireg_0+REG_a1
    sta longreg1+0
    lda RISCV_ireg_1+REG_a1
    sta longreg1+1
    lda RISCV_ireg_2+REG_a1
    sta longreg1+2
    lda RISCV_ireg_3+REG_a1
    sta longreg1+3
    lda RISCV_ireg_0+REG_a2
    sta longreg2+0
    lda RISCV_ireg_1+REG_a2
    sta longreg2+1
    lda RISCV_ireg_2+REG_a2
    sta longreg2+2
    lda RISCV_ireg_3+REG_a2
    sta longreg2+3
    jsr check_write
    bcc write_ok
        set_errno EFAULT
        rts
    write_ok:

    ; Check file handle
    jsr is_file_handle
    bcc do_tty_read
        ; Handle designates an open file
        jmp read_file
    do_tty_read:
        ; Handle is one of the special handles
        jmp read_tty

.endproc

; Read one of the special handles
; longreg1 and longreg2 are set and checked
.proc read_tty

    ; Can only read the keyboard
    lda RISCV_ireg_0+REG_a0
    bne bad_handle

        ; Handle 0 (standard input)
        ; Return size is 1, unless specified transfer size is 0
        lda #0
        sta RISCV_ireg_1+REG_a0
        sta RISCV_ireg_2+REG_a0
        sta RISCV_ireg_3+REG_a0

        ; Check for zero transfer size
        lda longreg2+0
        ora longreg2+1
        ora longreg2+2
        ora longreg2+3
        beq zero_size

        ; Call GETIN to get a single character
        ; TODO: Provide a means to access CHRIN for Kernal-style line input
        @key_loop:
            jsr GETIN
            cmp #0
        beq @key_loop
        tax
        lda petscii_to_ascii,x
        sta scratch_area+0

        ; Transfer to target memory
        lda #1
        sta RISCV_ireg_0+REG_a0
        set_reu_address longreg1
        set_local_address scratch_area
        jsr reu_write
        rts

    zero_size:
        lda #0
        sta RISCV_ireg_0+REG_a0
        rts

    bad_handle:
        ; Handle 1 (standard output) or 2 (standard error)
        ; These are not valid
        set_errno EBADF
        rts

.endproc

; Read from a file
; longreg1 and longreg2 are set and checked
.proc read_file

    ; FIXME Stub
    set_errno ENOTSUP
    rts

.endproc

; Write to the given file handle
; A0 = file handle
; A1 = pointer to data
; A2 = length of data

.proc SYS_write

    ; Set up the transfer area
    lda RISCV_ireg_0+REG_a1
    sta longreg1+0
    lda RISCV_ireg_1+REG_a1
    sta longreg1+1
    lda RISCV_ireg_2+REG_a1
    sta longreg1+2
    lda RISCV_ireg_3+REG_a1
    sta longreg1+3
    lda RISCV_ireg_0+REG_a2
    sta longreg2+0
    lda RISCV_ireg_1+REG_a2
    sta longreg2+1
    lda RISCV_ireg_2+REG_a2
    sta longreg2+2
    lda RISCV_ireg_3+REG_a2
    sta longreg2+3
    jsr check_read
    bcc read_ok
        set_errno EFAULT
        rts
    read_ok:

    ; Check file handle
    jsr is_file_handle
    bcc do_tty_write
        ; Handle designates an open file
        jsr write_file
        jmp flush_after_syscall
    do_tty_write:
        ; Handle is one of the special handles
        jmp write_tty

.endproc

; Write one of the special handles
; longreg1 and longreg2 are set and checked
.proc write_tty

    lda RISCV_ireg_0+REG_a0
    bne tty_ok
        ; Handle 0 (standard input; not valid)
        set_errno EBADF
        rts
    tty_ok:

    ; Handle 1 (standard output) or 2 (standard error)
    write_loop:
        jsr read_io_block
        lda pointer1+0
        beq end_write_loop
        ldx #0
        write_byte:
            ldy scratch_area,x
            lda ascii_to_petscii,y
            jsr CHROUT
        inx
        dec pointer1+0
        bne write_byte
    beq write_loop
    end_write_loop:
    lda RISCV_ireg_0+REG_a2
    sta RISCV_ireg_0+REG_a0
    lda RISCV_ireg_1+REG_a2
    sta RISCV_ireg_1+REG_a0
    lda RISCV_ireg_2+REG_a2
    sta RISCV_ireg_2+REG_a0
    lda RISCV_ireg_3+REG_a2
    sta RISCV_ireg_3+REG_a0
    rts

.endproc

; Read next block into scratch_area
; Address is in longreg1, and size is in longreg2
; Return size in pointer1+0; update longreg1 and longreg2
.proc read_io_block

    ; Maximum size to be read in one pass
    xfer_size = 32

    ; Determine the size for this transfer
    lda longreg2+1
    ora longreg2+2
    beq check_size_0
        lda #xfer_size
        bne got_size
    check_size_0:
        lda longreg2+0
        cmp #xfer_size
        bcc got_size
            lda #xfer_size
    got_size:
    sta pointer1+0
    cmp #0
    beq end_read

    ; Space 1: RAM Expansion Unit
    sta reu_xfer_size+0
    set_reu_address longreg1
    set_local_address scratch_area
    lda #0
    sta reu_xfer_size+1
    jsr reu_read

    ; Update longreg1 and longreg2
    clc
    lda longreg1+0
    adc pointer1+0
    sta longreg1+0
    lda longreg1+1
    adc #0
    sta longreg1+1
    lda longreg1+2
    adc #0
    sta longreg1+2
    sec
    lda longreg2+0
    sbc pointer1+0
    sta longreg2+0
    lda longreg2+1
    sbc #0
    sta longreg2+1
    lda longreg2+2
    sbc #0
    sta longreg2+2

end_read:
    rts

.endproc

; Write to a file
; longreg1 and longreg2 are set and checked
.proc write_file

    ; FIXME Stub
    set_errno ENOTSUP
    rts

.endproc

; Close a file handle
; A0 = file handle
.proc SYS_close

    ; Check file handle
    jsr is_file_handle
    bcc do_tty_close
        ; Handle designates an open file
        jmp close_file
    do_tty_close:
        ; Handle is one of the special handles
        ; Ignore attempts to close them
        lda #0
        sta RISCV_ireg_0+REG_a0
        sta RISCV_ireg_1+REG_a0
        sta RISCV_ireg_2+REG_a0
        sta RISCV_ireg_3+REG_a0
        rts

.endproc

; Close a file
; A0 = file handle
.proc close_file

    ; FIXME Stub
    set_errno ENOTSUP
    rts

.endproc

; Return information about an open file handle
; A0 = file handle
; A1 = address of stat structure
;      This is struct kernel_stat in Newlib libgloss/riscv/kernel_stat.h
.proc SYS_fstat

    ; Set up the transfer area
    lda RISCV_ireg_0+REG_a1
    sta longreg1+0
    lda RISCV_ireg_1+REG_a1
    sta longreg1+1
    lda RISCV_ireg_2+REG_a1
    sta longreg1+2
    lda RISCV_ireg_3+REG_a1
    sta longreg1+3
    lda #.sizeof(kernel_stat)
    sta longreg2+0
    lda #0
    sta longreg2+1
    sta longreg2+2
    sta longreg2+3
    jsr check_write
    bcc write_ok
        set_errno EFAULT
        rts
    write_ok:

    ; Clear the transfer area
    ldx #.sizeof(kernel_stat)
    lda #0
    clear:
        sta scratch_area-1,x
    dex
    bne clear

    ; Check for file handle
    jsr is_file_handle
    bcc do_tty_fstat
        ; Handle designates an open file
        jmp fstat_file
    do_tty_fstat:
        ; Handle is one of the special handles
        jmp fstat_tty

.endproc

; Do fstat on one of the special handles
; longreg1 and longreg2 are set and checked
; scratch_area is cleared
.proc fstat_tty

    lda #$FF
    ldx #11
    minus_one:
        sta scratch_area+kernel_stat::st_atim,x
        sta scratch_area+kernel_stat::st_ctim,x
        sta scratch_area+kernel_stat::st_mtim,x
    dex
    bpl minus_one

    lda RISCV_ireg_0+REG_a0
    bne fstat_output
        ; Handle 0 (standard input)
        lda #<%010000100100100  ; Character device, readable
        ldx #>%010000100100100
        bne done
    fstat_output:
        ; Handle 1 (standard output) or 2 (standard error)
        lda #<%010000010010010  ; Character device, writable
        ldx #>%010000010010010
    done:
    sta scratch_area+kernel_stat::st_mode+0
    stx scratch_area+kernel_stat::st_mode+1
    lda #0
    sta RISCV_ireg_0+REG_a0
    sta RISCV_ireg_1+REG_a0
    sta RISCV_ireg_2+REG_a0
    sta RISCV_ireg_3+REG_a0
    jmp write_io_xfer

.endproc

.proc fstat_file

    ; FIXME Stub
    set_errno ENOTSUP
    rts

.endproc

; Exit the program
; A0 contains the exit code; this is ignored
.proc SYS_exit

    jsr_far RISCV_exit_bank,RISCV_exit_entry
    ; Does not return

.endproc

; Get time of day
; A0 == pointer to struct timeval that will receive the time
.global SYS_gettimeofday
.proc SYS_gettimeofday

    ; Set up the transfer area
    lda RISCV_ireg_0+REG_a0
    sta longreg1+0
    lda RISCV_ireg_1+REG_a0
    sta longreg1+1
    lda RISCV_ireg_2+REG_a0
    sta longreg1+2
    lda RISCV_ireg_3+REG_a0
    sta longreg1+3
    lda #.sizeof(timeval)
    sta longreg2+0
    lda #0
    sta longreg2+1
    sta longreg2+2
    sta longreg2+3
    jsr check_write
    bcc write_ok
        set_errno EFAULT
        rts
    write_ok:

    ; Get time from the U64
    lda #<scratch_area
    sta pointer1+0
    lda #>scratch_area
    sta pointer1+1
    jsr_far dos_get_time_bank,dos_get_time_entry

    ; dos_get_time always succeeds
    ; Response is yyyy/mm/dd hh:mm:ss
    ; Convert the components to integer form
    ; Year is 1980-2079

    lda scratch_area+0 ; 1000s of year
    and #$0F
    tax
    lda thousand_lo,x
    sta syscall_dos_year+0
    lda thousand_hi,x
    sta syscall_dos_year+1

    lda scratch_area+1 ; 100s of year
    and #$0F
    tax
    clc
    lda syscall_dos_year+0
    adc hundred_lo,x
    sta syscall_dos_year+0
    lda syscall_dos_year+1
    adc hundred_hi,x
    sta syscall_dos_year+1

    lda scratch_area+2 ; 10s of year
    and #$0F
    tax
    lda scratch_area+3 ; 1s of year
    and #$0F
    clc
    adc ten,x
    ; the add never carries
    adc syscall_dos_year+0
    sta syscall_dos_year+0
    lda #0
    adc syscall_dos_year+1
    sta syscall_dos_year+1

    ; Convert month to integer

    lda scratch_area+5   ; 10s of month
    and #$0F
    tax
    lda scratch_area+6   ; 1s of month
    and #$0F
    clc
    adc ten,x
    sta syscall_dos_month

    ; Convert day to integer

    lda scratch_area+8   ; 10s of day
    and #$0F
    tax
    lda scratch_area+9   ; 1s of day
    and #$0F
    clc
    adc ten,x
    sta syscall_dos_day

    ; Convert hour to integer

    lda scratch_area+11  ; 10s of hour
    and #$0F
    tax
    lda scratch_area+12  ; 1s of hour
    and #$0F
    clc
    adc ten,x
    sta syscall_dos_hour

    ; Convert minute to integer

    lda scratch_area+14  ; 10s of minute
    and #$0F
    tax
    lda scratch_area+15  ; 1s of minute
    and #$0F
    clc
    adc ten,x
    sta syscall_dos_minute

    ; Convert second to integer

    lda scratch_area+17  ; 10s of second
    and #$0F
    tax
    lda scratch_area+18  ; 1s of second
    and #$0F
    clc
    adc ten,x
    sta syscall_dos_second

    ; Convert to Unix time
    jsr component_to_unix_time

    ; Transfer to the caller
    ldx #11
    @copy:
        lda syscall_unix_time,x
        sta scratch_area,x
    dex
    bpl @copy
    sta scratch_area+12
    sta scratch_area+13
    sta scratch_area+14
    sta scratch_area+15

    jsr write_io_xfer

    lda #0
    sta RISCV_ireg_0+REG_a0
    sta RISCV_ireg_1+REG_a0
    sta RISCV_ireg_2+REG_a0
    sta RISCV_ireg_3+REG_a0
    rts

.endproc

; If A0 == 0, query the current break
; Otherwise, set the break to A0
; Return A0 == -1 on error, otherwise A0 == the current or new break

.proc SYS_brk

    ; If A0 == 0, query the current break; this always succeeds
    lda RISCV_ireg_0+REG_a0
    ora RISCV_ireg_1+REG_a0
    ora RISCV_ireg_2+REG_a0
    ora RISCV_ireg_3+REG_a0
    bne set_break

        lda RISCV_break+0
        sta RISCV_ireg_0+REG_a0
        lda RISCV_break+1
        sta RISCV_ireg_1+REG_a0
        lda RISCV_break+2
        sta RISCV_ireg_2+REG_a0
        lda RISCV_break+3
        sta RISCV_ireg_3+REG_a0
        rts

    set_break:

    ; A0 != 0 sets a new break
    ; The new break must be in space 1 (the REU)
    lda RISCV_ireg_3+REG_a0
    cmp #1
    bne error

    ; The new break must not be less than the initial break
    lda RISCV_ireg_0+REG_a0
    cmp RISCV_min_break+0
    lda RISCV_ireg_1+REG_a0
    sbc RISCV_min_break+1
    lda RISCV_ireg_2+REG_a0
    sbc RISCV_min_break+2
    lda RISCV_ireg_3+REG_a0
    sbc RISCV_min_break+3
    bcc error

    ; The new break must be less than the current stack pointer
    lda RISCV_ireg_0+REG_a0
    cmp RISCV_ireg_0+REG_sp
    lda RISCV_ireg_1+REG_a0
    sbc RISCV_ireg_1+REG_sp
    lda RISCV_ireg_2+REG_a0
    sbc RISCV_ireg_2+REG_sp
    lda RISCV_ireg_3+REG_a0
    sbc RISCV_ireg_3+REG_sp
    bcs error

    ; Set the new break and return that same address
    lda RISCV_ireg_0+REG_a0
    sta RISCV_break+0
    lda RISCV_ireg_1+REG_a0
    sta RISCV_break+1
    lda RISCV_ireg_2+REG_a0
    sta RISCV_break+2
    lda RISCV_ireg_3+REG_a0
    sta RISCV_break+3
    ; leave A0 unchanged
    rts

error:
    lda #$FF
    sta RISCV_ireg_0+REG_a0
    sta RISCV_ireg_1+REG_a0
    sta RISCV_ireg_2+REG_a0
    sta RISCV_ireg_3+REG_a0
    rts

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Kernal calls                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Most kernal calls end in this, to return +A if success, -A if error
.macro return_A0

    bcs :+
        ; Return success
        sta RISCV_ireg_0+REG_a0
        lda #0
        sta RISCV_ireg_1+REG_a0
        sta RISCV_ireg_2+REG_a0
        sta RISCV_ireg_3+REG_a0
        rts
    :
        ; Return failure
        eor #$FF
        clc
        adc #0
        sta RISCV_ireg_0+REG_a0
        lda #$FF
        sta RISCV_ireg_1+REG_a0
        sta RISCV_ireg_2+REG_a0
        sta RISCV_ireg_3+REG_a0
        rts

.endmacro

; ACPTR; no input registers
.proc CBM_acptr
    jsr ACPTR
    return_A0
.endproc

; CHKIN; file number in A0
.proc CBM_chkin
    ldx RISCV_ireg_0+REG_a0
    jsr CHKIN
    return_A0
.endproc

; CHKOUT; file number in A0
.proc CBM_chkout
    ldx RISCV_ireg_0+REG_a0
    jsr CHKOUT
    return_A0
.endproc

; CHRIN
.proc CBM_chrin
    jsr CHRIN
    return_A0
.endproc

; CHROUT
.proc CBM_chrout
    lda RISCV_ireg_0+REG_a0
    jsr CHROUT
    return_A0
.endproc

; CINT
.proc CBM_cint
    jsr CINT
    return_A0
.endproc

; CIOUT; byte to transmit in A0
.proc CBM_ciout
    lda RISCV_ireg_0+REG_a0
    jsr CIOUT
    return_A0
.endproc

; CLALL
.proc CBM_clall
    jsr CLALL
    return_A0
.endproc

; CLOSE; file number in A0
.proc CBM_close
    lda RISCV_ireg_0+REG_a0
    jsr CLOSE
    return_A0
.endproc

; CLRCHN
.proc CBM_clrchn
    jsr CLRCHN
    return_A0
.endproc

; GETIN
.proc CBM_getin
    jsr GETIN
    return_A0
.endproc

; IOINIT
.proc CBM_ioinit
    jsr IOINIT
    return_A0
.endproc

; LISTEN; device number in A0
.proc CBM_listen
    lda RISCV_ireg_0+REG_a0
    jsr LISTEN
    return_A0
.endproc

; CBM_open
; Path is in A0
; File number is in A1
; Device number is in A2
; Secondary address is in A3
.proc CBM_open
    ; We have to read the path into 6502 memory space first.
    ; It may be too long, or the address may be invalid.
    lda RISCV_ireg_0+REG_a0
    sta longreg1+0
    lda RISCV_ireg_1+REG_a0
    sta longreg1+1
    lda RISCV_ireg_2+REG_a0
    sta longreg1+2
    lda RISCV_ireg_3+REG_a0
    sta longreg1+3
    jsr read_path
    bcc :+
        ; Failed to read the path
        sta RISCV_ireg_0+REG_a0
        lda #$FF
        sta RISCV_ireg_1+REG_a0
        sta RISCV_ireg_2+REG_a0
        sta RISCV_ireg_3+REG_a0
        rts
    :

    ; Determine the length of the name
    ; This cannot exceed 255 (even though syscall.s allows it)
    ldx #0
    name_length:
        lda syscall_path,x
        beq name_ok
    inx
    bne name_length
    ; If we exit this way, the name is too long
        lda #$100-ENAMETOOLONG
        sta RISCV_ireg_0+REG_a0
        lda #$FF
        sta RISCV_ireg_1+REG_a0
        sta RISCV_ireg_2+REG_a0
        sta RISCV_ireg_3+REG_a0
        rts
    name_ok:

    ; Set the name to open
    txa
    ldx #<longreg1
    ldy #>longreg1
    jsr SETNAM

    ; Set the file number, device number and secondary address
    lda RISCV_ireg_0+REG_a1
    ldx RISCV_ireg_0+REG_a2
    ldy RISCV_ireg_0+REG_a3
    jsr SETLFS

    jsr OPEN
    return_A0

.endproc

; PLOT with C set
; Return row in A0 and column in A1
.proc CBM_get_cursor
    sec
    jsr PLOT
    sty RISCV_ireg_0+REG_a0
    stx RISCV_ireg_0+REG_a1
    lda #0
    sta RISCV_ireg_1+REG_a0
    sta RISCV_ireg_2+REG_a0
    sta RISCV_ireg_3+REG_a0
    sta RISCV_ireg_1+REG_a1
    sta RISCV_ireg_2+REG_a2
    sta RISCV_ireg_3+REG_a3
    rts
.endproc

; PLOT with C clear
; Row in A0; coluimn in A1
.proc CBM_set_cursor
    ldy RISCV_ireg_0+REG_a0
    ldx RISCV_ireg_1+REG_a1
    clc
    jsr PLOT
    return_A0
.endproc

; RDTIM
.proc CBM_rdtim
    jsr RDTIM
    sta RISCV_ireg_0+REG_a0
    stx RISCV_ireg_1+REG_a0
    sty RISCV_ireg_2+REG_a0
    lda #0
    sta RISCV_ireg_3+REG_a0
    rts
.endproc

; READST
.proc CBM_readst
    jsr READST
    sta RISCV_ireg_0+REG_a0
    lda #0
    sta RISCV_ireg_1+REG_a0
    sta RISCV_ireg_2+REG_a0
    sta RISCV_ireg_3+REG_a0
    rts
.endproc

; SECOND; secondary address in A0
.proc CBM_second
    lda RISCV_ireg_0+REG_a0
    jsr SECOND
    return_A0
.endproc

; SETTIM; system time in A0
.proc CBM_settim
    lda RISCV_ireg_0+REG_a0
    ldx RISCV_ireg_1+REG_a0
    ldy RISCV_ireg_2+REG_a0
    jsr SETTIM
    return_A0
.endproc

; STOP
; Return -1 if stop key pressed; otherwise, the returned row in A
.proc CBM_stop
    jsr STOP
    bne :+
        lda #$FF
        sta RISCV_ireg_0+REG_a0
        sta RISCV_ireg_1+REG_a0
        sta RISCV_ireg_2+REG_a0
        sta RISCV_ireg_3+REG_a0
        rts
    :
        sta RISCV_ireg_0+REG_a0
        lda #0
        sta RISCV_ireg_1+REG_a0
        sta RISCV_ireg_2+REG_a0
        sta RISCV_ireg_3+REG_a0
        rts
.endproc

; TALK; device number in A0
.proc CBM_talk
    lda RISCV_ireg_0+REG_a0
    jsr TALK
    return_A0
.endproc

; TKSA; secondary address in A0
.proc CBM_tksa
    lda RISCV_ireg_0+REG_a0
    jsr TKSA
    return_A0
.endproc

; UNLSN
.proc CBM_unlsn
    jsr UNLSN
    return_A0
.endproc

; UNTLK
.proc CBM_untlk
    jsr UNTLK
    return_A0
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Various common routines                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; If A0 contains one of the special handles (stdin, stdout, stderr),
; return C clear.
; If A0 contains a possible file handle, return C set.

.proc is_file_handle

    lda RISCV_ireg_1+REG_a0
    lda RISCV_ireg_2+REG_a0
    lda RISCV_ireg_3+REG_a0
    bne file_handle

    lda RISCV_ireg_0+REG_a0
    cmp first_file_handle
    rts

    file_handle:
    sec
    rts

.endproc

; Check the configured address and size for valid write
; On entry: longreg1 contains the address
;           longreg2 contains the size
; Set C if address and size are invalid
; Must be placed immediately before check_read so a branch will reach
.proc check_write

    ; Requirement is the same as check_read, except that the address must also
    ; be above the fence
    lda longreg1+2
    cmp RISCV_fence
    bcs check_read
    sec
    rts

.endproc

; Check the configured address and size for valid read
; On entry: longreg1 contains the address
;           longreg2 contains the size
; Set C if address and size are invalid
.proc check_read

    ; I/O is to Space 1 (the REU) only
    lda longreg1+3
    cmp #1
    bne bad_read

    ; I/O shall not exceed 16 MB
    lda longreg2+3
    bne bad_read

    ; I/O shall not exceed the end of the REU
    clc
    lda longreg1+0
    adc longreg2+0
    lda longreg1+1
    adc longreg2+1
    lda longreg1+2
    adc longreg2+2
    ; C set if the add overflows
    bcc good_read

    ; Allow the size to extend to the end of the REU
    clc
    lda longreg1+0
    adc longreg2+0
    bne bad_read
    lda longreg1+1
    adc longreg2+1
    bne bad_read
    lda longreg1+2
    adc longreg2+2
    bne bad_read

good_read:
    clc
    rts

bad_read:
    sec
    rts

.endproc

; Pass the scratch area to the RISC-V target
; longreg1 contains the REU address
; longreg2 contains the size
.proc write_io_xfer

    set_reu_address longreg1
    set_xfer_size longreg2
    set_local_address scratch_area
    jsr reu_write
    rts

.endproc

; Read the path from the address at longreg1
; Return the path in syscall_path
; On error: set C and return -errno in A

.proc read_path

    ; Only address space 1 is valid
    lda longreg1+3
    cmp #$01
    bne bad_address

    ; Set the maximum length of the path
    set_xfer_size_imm 256

    ; If that would exceed the end of the REU, reduce the length to the
    ; remaining part of the REU and fill the transfer area with nonzero
    clc
    lda longreg1+1
    adc #1
    lda longreg1+2
    adc #0
    bcc size_ok
        ; C already set
        lda #0
        sbc longreg1+0
        sta reu_xfer_size+0
        lda #0              ; Address might be $01FFFF00
        sbc longreg1+1
        sta reu_xfer_size+1
        ldx reu_xfer_size+0
        beq size_ok
        lda #$FF
        fill:
            sta syscall_path,x
        inx
        bne fill
    size_ok:

    ; Set the transfer addresses
    set_reu_address longreg1
    set_local_address syscall_path

    ; Read the path
    jsr reu_read

    ; Did we get a null?
    ldx #0
    find_null:
        lda syscall_path,x
        beq path_ok
    inx
    bne find_null
    ; If we leave the loop by this path, the name is too long
    lda reu_xfer_size+1
    beq bad_address     ; Path ran off the end of the REU
    lda #$100-ENAMETOOLONG
    sec
    rts

path_ok:
    clc
    rts

bad_address:
    lda #$100-EFAULT
    sec
    rts

.endproc

; Flush the FAT after a system call that might alter it
; On entry, C is set if an error is pending, and -errno is in A;
; if that is the case, that error will be returned even if flush_fat succeeds

.proc flush_after_syscall

    ; FIXME Stub
    set_errno ENOTSUP
    rts

.endproc

; Convert component time to Unix time
.proc component_to_unix_time

    ; Year since 1970
    sec
    lda syscall_dos_year+0
    sbc #<1970
    tax
    lda syscall_dos_year+1
    sbc #>1970
    bcc @bad_time_1
    bne @bad_time_1
    cpx #2100-1970
    bcs @bad_time_1

    ; Convert to day
    lda year_to_day_lo,x
    sta syscall_unix_time+0
    lda year_to_day_hi,x
    sta syscall_unix_time+1

    ; Month
    ldx syscall_dos_month
    dex
    cpx #12
    bcs @bad_time_1
    ; Convert to day
    clc
    lda month_to_day_lo,x
    adc syscall_unix_time+0
    sta syscall_unix_time+0
    lda month_to_day_hi,x
    adc syscall_unix_time+1
    sta syscall_unix_time+1

    ; Check for month after February 29
    cpx #3
    bcc @end_leap_day ; January or February

    ; Check for leap year
    lda syscall_dos_year
    and #$03
    bne @end_leap_day ; Not leap year

        ; Add one day
        inc syscall_unix_time+0
        beq @end_leap_day
        inc syscall_unix_time+1

    @end_leap_day:

    ; Day of month
    lda syscall_dos_day
    sec
    sbc #1
    cmp #31
    bcc :+
    @bad_time_1:
        jmp @bad_time
    :
    clc
    adc syscall_unix_time+0
    sta syscall_unix_time+0
    lda #0
    adc syscall_unix_time+1
    sta syscall_unix_time+1
    ; syscall_unix_time[1:0] contains days from 1970

    ; Convert to hour
    lda syscall_unix_time+0
    asl a
    sta syscall_unix_time+2
    lda syscall_unix_time+1
    rol a
    sta syscall_unix_time+3
    lda #0
    rol a
    sta syscall_unix_time+4 ; syscall_unix_time[1:0] = days
                            ; syscall_unix_time[4:2] = days*2
    clc
    lda syscall_unix_time+0
    adc syscall_unix_time+2
    sta syscall_unix_time+2
    lda syscall_unix_time+1
    adc syscall_unix_time+3
    sta syscall_unix_time+3
    lda #0
    adc syscall_unix_time+4 ; A:syscall_unix_time[3:2] = days*3
    asl syscall_unix_time+2
    rol syscall_unix_time+3
    rol a           ; days*6
    asl syscall_unix_time+2
    rol syscall_unix_time+3
    rol a           ; days*12
    asl syscall_unix_time+2
    rol syscall_unix_time+3
    rol a           ; days*24
    sta syscall_unix_time+4 ; syscall_unix_time[4:2] = days*24

    ; Add hours from component time
    lda syscall_dos_hour
    cmp #24
    bcs @bad_time_1
    clc
    adc syscall_unix_time+2
    sta syscall_unix_time+0
    lda #0
    adc syscall_unix_time+3
    sta syscall_unix_time+1
    lda #0
    adc syscall_unix_time+4
    sta syscall_unix_time+2
    lda #0
    sta syscall_unix_time+3 ; syscall_unix_time[3:0] = hours since 1970

    ; Convert to minutes
    jsr unix_time_times_60

    ; Add minutes from component time
    lda syscall_dos_minute
    cmp #60
    bcs @bad_time
    clc
    adc syscall_unix_time+0
    sta syscall_unix_time+0
    lda #0
    adc syscall_unix_time+1
    sta syscall_unix_time+1
    lda #0
    adc syscall_unix_time+2
    sta syscall_unix_time+2
    lda #0
    adc syscall_unix_time+3
    sta syscall_unix_time+3 ; syscall_unix_time[3:0] = minutes since 1970

    ; Convert to seconds
    jsr unix_time_times_60

    ; Add seconds from component time
    lda syscall_dos_second
    cmp #60
    bcs @bad_time
    clc
    adc syscall_unix_time+0
    sta syscall_unix_time+0
    lda #0
    adc syscall_unix_time+1
    sta syscall_unix_time+1
    lda #0
    adc syscall_unix_time+2
    sta syscall_unix_time+2
    lda #0
    adc syscall_unix_time+3
    sta syscall_unix_time+3

    ; Zero out the rest of syscall_unix_time
    lda #0
    ldx #7
    @zero:
        sta syscall_unix_time+4,x
    dex
    bpl @zero
    rts

@bad_time:
    lda #$FF
    ldx #11
    @error:
        sta syscall_unix_time,x
    dex
    bpl @error
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

; Multiply syscall_unix_time[3:0] by 60
; Converts hours to minutes, or minutes to seconds

.proc unix_time_times_60

    ; A:syscall_unix_time[6:4] <- time * 2

    lda syscall_unix_time+0
    asl a
    sta syscall_unix_time+4
    lda syscall_unix_time+1
    rol a
    sta syscall_unix_time+5
    lda syscall_unix_time+2
    rol a
    sta syscall_unix_time+6
    lda syscall_unix_time+3
    rol a

    ; syscall_unix_time[7:4] <- time * 16

    ldx #3
    @shift1:
        asl syscall_unix_time+4
        rol syscall_unix_time+5
        rol syscall_unix_time+6
        rol a
    dex
    bne @shift1
    sta syscall_unix_time+7

    ; A:syscall_unix_time[2:0] <- time * 15

    sec
    lda syscall_unix_time+4
    sbc syscall_unix_time+0
    sta syscall_unix_time+0
    lda syscall_unix_time+5
    sbc syscall_unix_time+1
    sta syscall_unix_time+1
    lda syscall_unix_time+6
    sbc syscall_unix_time+2
    sta syscall_unix_time+2
    lda syscall_unix_time+7
    sbc syscall_unix_time+3

    ; syscall_unix_time[3:0] <- time * 60

    ldx #2
    @shift2:
        asl syscall_unix_time+0
        rol syscall_unix_time+1
        rol syscall_unix_time+2
        rol a
    dex
    bne @shift2
    sta syscall_unix_time+3 ; syscall_unix_time[3:0] = minutes*60

    rts

.endproc

.rodata

; Tables for decimal to binary conversion:

; Index times 1000

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

; Index times 100

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

; Index times 10

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

.code

; Dump registers on an unknown ECALL

.proc bad_ecall

    lda RISCV_ireg_0+REG_a7
    sta RISCV_address+0
    lda RISCV_ireg_1+REG_a7
    sta RISCV_address+1
    lda RISCV_ireg_2+REG_a7
    sta RISCV_address+2
    lda RISCV_ireg_3+REG_a7
    sta RISCV_address+3
    lda #ERR_bad_syscall
    sta pointer1+0
    jsr_far error_dump_bank,error_dump_entry
    jsr_far RISCV_exit_bank,RISCV_exit_entry ; does not return

.endproc

; Dump registers on an unknown EBREAK

.proc bad_ebreak

    lda RISCV_ireg_0+REG_a7
    sta RISCV_address+0
    lda RISCV_ireg_1+REG_a7
    sta RISCV_address+1
    lda RISCV_ireg_2+REG_a7
    sta RISCV_address+2
    lda RISCV_ireg_3+REG_a7
    sta RISCV_address+3
    lda #ERR_bad_ebreak
    sta pointer1+0
    jsr_far error_dump_bank,error_dump_entry
    jsr_far RISCV_exit_bank,RISCV_exit_entry ; does not return

.endproc
