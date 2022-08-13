; reu.s -- wrapper functions for the REU

.include "memory.inc"
.include "reu.inc"
.include "reu-regs.inc"

; The Ultimate 64 REU does not advance its transfer address past any 512K
; boundary, emulating various extended REUs from third party vendors.
; For this reason, any transfer that might cross a 512K boundary must be
; split in two.
; It is easier to split at a 64K boundary.

; Copy reu_xfer_size bytes from local memory at reu_c64_address to REU memory
; at reu_xmem_address
.proc reu_read

    ldx #$91        ; start transfer immediately; no autoload; REU to main memory
    jmp reu_xfer

.endproc

.proc reu_write

    ldx #$90        ; start transfer immediately; no autoload; main memory to REU
    jmp reu_xfer

.endproc

; Fill reu_xfer_size bytes from local memory at reu_c64_address to REU memory
; at reu_xmem_address
.proc reu_fill
    lda #$80
    sta reu_address_control
    jsr reu_write
    lda #0
    sta reu_address_control
    rts

.endproc

; Local procedure to split the transfer if needed

.proc reu_xfer

    ; Does the transfer cross a 64K boundary?
    clc
    lda reu_xmem_address+0
    adc reu_xfer_size+0
    lda reu_xmem_address+1
    adc reu_xfer_size+1
    bcs two_transfers

        ; The transfer does not cross a 64K boundary
        lda reu_xfer_size+0
        ora reu_xfer_size+1
        beq :+
            lda reu_c64_address+0
            sta reu_c64_address_0
            lda reu_c64_address+1
            sta reu_c64_address_1
            lda reu_xmem_address+0
            sta reu_xmem_address_0
            lda reu_xmem_address+1
            sta reu_xmem_address_1
            lda reu_xmem_address+2
            sta reu_xmem_address_2
            lda reu_xfer_size+0
            sta reu_xfer_size_0
            lda reu_xfer_size+1
            sta reu_xfer_size_1
            stx reu_command
            bit reu_status
        :
        rts

    two_transfers:

        ; Perform the first part of the transfer
        sec
        lda #0
        sbc reu_xmem_address+0
        sta reu_partial_size+0
        lda #0
        sbc reu_xmem_address+1
        sta reu_partial_size+1

        lda reu_c64_address+0
        sta reu_c64_address_0
        lda reu_c64_address+1
        sta reu_c64_address_1
        lda reu_xmem_address+0
        sta reu_xmem_address_0
        lda reu_xmem_address+1
        sta reu_xmem_address_1
        ldy reu_xmem_address+2
        sty reu_xmem_address_2
        lda reu_partial_size+0
        sta reu_xfer_size_0
        lda reu_partial_size+1
        sta reu_xfer_size_1
        stx reu_command
        bit reu_status

        ; Perform the second part of the transfer
        ; If the transfer ends at the 64K boundary, the execution will use
        ; this path, and the second part will be zero bytes
        sec
        lda reu_xfer_size+0
        sbc reu_partial_size+0
        sta reu_partial_size+0
        lda reu_xfer_size+1
        sbc reu_partial_size+1
        sta reu_partial_size+1
        ora reu_partial_size+0
        beq :+
            iny
            sty reu_xmem_address_2
            lda reu_partial_size+0
            sta reu_xfer_size_0
            lda reu_partial_size+1
            sta reu_xfer_size_1
            stx reu_command
            bit reu_status
        :
        rts

.endproc
