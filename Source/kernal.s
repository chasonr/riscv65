; kernal.s

.include "errno.inc"
.include "kernal.inc"
.include "registers.inc"

.import _RISCV_ireg_0
.import _RISCV_ireg_1
.import _RISCV_ireg_2
.import _RISCV_ireg_3

; For CBM_open
.import io_addr
.import fs_path
.import read_path

; Most kernal calls end in this, to return +A if success, -A if error
.macro return_A0

    bcs :+
        ; Return success
        sta _RISCV_ireg_0+REG_a0
        lda #0
        sta _RISCV_ireg_1+REG_a0
        sta _RISCV_ireg_2+REG_a0
        sta _RISCV_ireg_3+REG_a0
        rts
    :
        ; Return failure
        eor #$FF
        clc
        adc #0
        sta _RISCV_ireg_0+REG_a0
        lda #$FF
        sta _RISCV_ireg_1+REG_a0
        sta _RISCV_ireg_2+REG_a0
        sta _RISCV_ireg_3+REG_a0
        rts

.endmacro

; ACPTR; no input registers
.global CBM_acptr
.proc CBM_acptr
    jsr ACPTR
    return_A0
.endproc

; CHKIN; file number in A0
.global CBM_chkin
.proc CBM_chkin
    ldx _RISCV_ireg_0+REG_a0
    jsr CHKIN
    return_A0
.endproc

; CHKOUT; file number in A0
.global CBM_chkout
.proc CBM_chkout
    ldx _RISCV_ireg_0+REG_a0
    jsr CHKOUT
    return_A0
.endproc

; CHRIN
.global CBM_chrin
.proc CBM_chrin
    jsr CHRIN
    return_A0
.endproc

; CHROUT
.global CBM_chrout
.proc CBM_chrout
    lda _RISCV_ireg_0+REG_a0
    jsr CHROUT
    return_A0
.endproc

; CINT
.global CBM_cint
.proc CBM_cint
    jsr CINT
    return_A0
.endproc

; CIOUT; byte to transmit in A0
.global CBM_ciout
.proc CBM_ciout
    lda _RISCV_ireg_0+REG_a0
    jsr CIOUT
    return_A0
.endproc

; CLALL
.global CBM_clall
.proc CBM_clall
    jsr CLALL
    return_A0
.endproc

; CLOSE; file number in A0
.global CBM_close
.proc CBM_close
    lda _RISCV_ireg_0+REG_a0
    jsr CLOSE
    return_A0
.endproc

; CLRCHN
.global CBM_clrchn
.proc CBM_clrchn
    jsr CLRCHN
    return_A0
.endproc

; GETIN
.global CBM_getin
.proc CBM_getin
    jsr GETIN
    return_A0
.endproc

; IOINIT
.global CBM_ioinit
.proc CBM_ioinit
    jsr IOINIT
    return_A0
.endproc

; LISTEN; device number in A0
.global CBM_listen
.proc CBM_listen
    lda _RISCV_ireg_0+REG_a0
    jsr LISTEN
    return_A0
.endproc

; CBM_open
; Path is in A0
; File number is in A1
; Device number is in A2
; Secondary address is in A3
.global CBM_open
.proc CBM_open
    ; We have to read the path into 6502 memory space first.
    ; It may be too long, or the address may be invalid.
    lda _RISCV_ireg_0+REG_a0
    sta io_addr+0
    lda _RISCV_ireg_1+REG_a0
    sta io_addr+1
    lda _RISCV_ireg_2+REG_a0
    sta io_addr+2
    lda _RISCV_ireg_3+REG_a0
    sta io_addr+3
    jsr read_path
    bcc :+
        ; Failed to read the path
        sta _RISCV_ireg_0+REG_a0
        lda #$FF
        sta _RISCV_ireg_1+REG_a0
        sta _RISCV_ireg_2+REG_a0
        sta _RISCV_ireg_3+REG_a0
        rts
    :

    ; Determine the length of the name
    ; This cannot exceed 255 (even though syscall.s allows it)
    ldx #0
    name_length:
        lda io_addr,x
        beq name_ok
    inx
    bne name_length
    ; If we exit this way, the name is too long
        lda #$100-ENAMETOOLONG
        sta _RISCV_ireg_0+REG_a0
        lda #$FF
        sta _RISCV_ireg_1+REG_a0
        sta _RISCV_ireg_2+REG_a0
        sta _RISCV_ireg_3+REG_a0
        rts
    name_ok:

    ; Set the name to open
    txa
    ldx #<io_addr
    ldy #>io_addr
    jsr SETNAM

    ; Set the file number, device number and secondary address
    lda _RISCV_ireg_0+REG_a1
    ldx _RISCV_ireg_0+REG_a2
    ldy _RISCV_ireg_0+REG_a3
    jsr SETLFS

    jsr OPEN
    return_A0

.endproc

; PLOT with C set
; Return row in A0 and column in A1
.global CBM_get_cursor
.proc CBM_get_cursor
    sec
    jsr PLOT
    sty _RISCV_ireg_0+REG_a0
    stx _RISCV_ireg_0+REG_a1
    lda #0
    sta _RISCV_ireg_1+REG_a0
    sta _RISCV_ireg_2+REG_a0
    sta _RISCV_ireg_3+REG_a0
    sta _RISCV_ireg_1+REG_a1
    sta _RISCV_ireg_2+REG_a2
    sta _RISCV_ireg_3+REG_a3
    rts
.endproc

; PLOT with C clear
; Row in A0; coluimn in A1
.global CBM_set_cursor
.proc CBM_set_cursor
    ldy _RISCV_ireg_0+REG_a0
    ldx _RISCV_ireg_1+REG_a1
    clc
    jsr PLOT
    return_A0
.endproc

; RDTIM
.global CBM_rdtim
.proc CBM_rdtim
    jsr RDTIM
    sta _RISCV_ireg_0+REG_a0
    stx _RISCV_ireg_1+REG_a0
    sty _RISCV_ireg_2+REG_a0
    lda #0
    sta _RISCV_ireg_3+REG_a0
    rts
.endproc

; READST
.global CBM_readst
.proc CBM_readst
    jsr READST
    sta _RISCV_ireg_0+REG_a0
    lda #0
    sta _RISCV_ireg_1+REG_a0
    sta _RISCV_ireg_2+REG_a0
    sta _RISCV_ireg_3+REG_a0
    rts
.endproc

; SECOND; secondary address in A0
.global CBM_second
.proc CBM_second
    lda _RISCV_ireg_0+REG_a0
    jsr SECOND
    return_A0
.endproc

; SETTIM; system time in A0
.global CBM_settim
.proc CBM_settim
    lda _RISCV_ireg_0+REG_a0
    ldx _RISCV_ireg_1+REG_a0
    ldy _RISCV_ireg_2+REG_a0
    jsr SETTIM
    return_A0
.endproc

; STOP
; Return -1 if stop key pressed; otherwise, the returned row in A
.global CBM_stop
.proc CBM_stop
    jsr STOP
    bne :+
        lda #$FF
        sta _RISCV_ireg_0+REG_a0
        sta _RISCV_ireg_1+REG_a0
        sta _RISCV_ireg_2+REG_a0
        sta _RISCV_ireg_3+REG_a0
        rts
    :
        sta _RISCV_ireg_0+REG_a0
        lda #0
        sta _RISCV_ireg_1+REG_a0
        sta _RISCV_ireg_2+REG_a0
        sta _RISCV_ireg_3+REG_a0
        rts
.endproc

; TALK; device number in A0
.global CBM_talk
.proc CBM_talk
    lda _RISCV_ireg_0+REG_a0
    jsr TALK
    return_A0
.endproc

; TKSA; secondary address in A0
.global CBM_tksa
.proc CBM_tksa
    lda _RISCV_ireg_0+REG_a0
    jsr TKSA
    return_A0
.endproc

; UNLSN
.global CBM_unlsn
.proc CBM_unlsn
    jsr UNLSN
    return_A0
.endproc

; UNTLK
.global CBM_untlk
.proc CBM_untlk
    jsr UNTLK
    return_A0
.endproc
