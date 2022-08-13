; reu.inc -- addresses and macros to use the REU

.global reu_read
.global reu_write
.global reu_fill

; Macros to address the REU:

; Set the REU address
.macro set_reu_address address

    lda address+0
    sta reu_xmem_address+0
    lda address+1
    sta reu_xmem_address+1
    lda address+2
    sta reu_xmem_address+2

.endmacro

; Set the address in local memory
.macro set_local_address address

    lda #<address
    sta reu_c64_address+0
    lda #>address
    sta reu_c64_address+1

.endmacro

; Set the transfer size (immediate)
.macro set_xfer_size_imm size

    lda #<size
    sta reu_xfer_size+0
    lda #>size
    sta reu_xfer_size+1

.endmacro

; Set the transfer size (from memory)
.macro set_xfer_size size

    lda size+0
    sta reu_xfer_size+0
    lda size+1
    sta reu_xfer_size+1

.endmacro

; vim: syntax=asm
