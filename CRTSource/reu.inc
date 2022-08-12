; reu.inc -- addresses and macros to use the REU

; REU register locations
reu_status          = $DF00
reu_command         = $DF01
reu_c64_address_0   = $DF02
reu_c64_address_1   = $DF03
reu_xmem_address_0  = $DF04
reu_xmem_address_1  = $DF05
reu_xmem_address_2  = $DF06
reu_xfer_size_0     = $DF07
reu_xfer_size_1     = $DF08
reu_irq_mask        = $DF09
reu_address_control = $DF0A

; Macros to address the REU:

; Set the REU address
.macro set_reu_address address

    lda address+0
    sta reu_xmem_address_0
    lda address+1
    sta reu_xmem_address_1
    lda address+2
    sta reu_xmem_address_2

.endmacro

; Set the address in local memory
.macro set_local_address address

    lda #<address
    sta reu_c64_address_0
    lda #>address
    sta reu_c64_address_1

.endmacro

; Set the transfer size (immediate)
.macro set_xfer_size_imm size

    lda #<size
    sta reu_xfer_size_0
    lda #>size
    sta reu_xfer_size_1

.endmacro

; Set the transfer size (from memory)
.macro set_xfer_size size

    lda size+0
    sta reu_xfer_size_0
    lda size+1
    sta reu_xfer_size_1

.endmacro

; Perform the transfer
.macro do_reu_read

    lda #$91        ; start transfer immediately; no autoload; REU to main memory
    sta reu_command
    bit reu_status

.endmacro

.macro do_reu_write

    lda #$90        ; start transfer immediately; no autoload; REU to main memory
    sta reu_command
    bit reu_status

.endmacro

; vim: syntax=asm
