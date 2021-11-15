; vm-api.s

; External parameters come from here
RISCV_FENCE = $07F3

.include "registers.inc"
.include "memory.inc"
.include "kernal.inc"
.include "reu.inc"

.segment "ZEROPAGE"
.globalzp RISCV_fence
RISCV_fence: .res 1
local_address: .res 2

.bss
reg_index: .res 1
str_index: .res 1

.code

; Overall RISC-V machine address space:
; 00xxxxxx -- invalid; null pointer dereference
; 01xxxxxx -- main memory, resident in the REU device
; 02xxxxxx -- C64 text mode screen (1K)
; 03xxxxxx -- C64 graphics mode space (16K)
; 04xxxxxx -- C64 I/O space (4K)
; 05xxxxxx -- C64 character ROM (4K, read only)
; 06xxxxxx and up -- invalid
;
; The fence lies at adress 01ZZ0000 where ZZ is the byte in RISCV_fence.
; Addresses at least 01000000 and less than the fence are valid for instruction
; fetches. Addresses at least equal to the fence and not greater than 01FFFFFF
; are valid for writes. All addresses in main memory are valid for reads.
;
; Opcode fetches may only be performed in main memory. Reads and writes are
; valid in any of the spaces listed above.
;
; Opcode fetches, and reads and writes of 32 bits, must be aligned on a four
; byte boundary. Reads and writes of 16 bits must occur on an even address.
; Reads and writes of 8 bits may occur on any valid address.

.importzp _RISCV_pc
.importzp _RISCV_opcode
.importzp _RISCV_address
.importzp _RISCV_data
.import _RISCV_ireg_0
.import _RISCV_ireg_1
.import _RISCV_ireg_2
.import _RISCV_ireg_3
.import _RISCV_exit

; Initialize this system
.global _RISCV_api_init
.proc _RISCV_api_init

    ; Set the fence
    lda RISCV_FENCE
    sta RISCV_fence

    ; Set these registers once, rather than every time we fetch, read or write
    lda #0
    sta reu_irq_mask
    sta reu_address_control

    rts

.endproc

; Fetch an opcode from address _RISCV_pc into _RISCV_opcode
.global _RISCV_fetch
.proc _RISCV_fetch

    lda _RISCV_pc+3 ; Check for correct address space (main memory only)
    cmp #1
    bne bad_address
    lda _RISCV_pc+2 ; Check for address below the fence
    cmp RISCV_fence
    bcs bad_address
    lda _RISCV_pc+0 ; Check for alignment
    and #3
    bne bad_address

    set_reu_address _RISCV_pc
    set_local_address _RISCV_opcode
    set_xfer_size_imm 4
    do_reu_read

    rts

bad_address:
    lda _RISCV_pc+0
    sta _RISCV_address+0
    lda _RISCV_pc+1
    sta _RISCV_address+1
    lda _RISCV_pc+2
    sta _RISCV_address+2
    lda _RISCV_pc+3
    sta _RISCV_address+3
    ldx #<bad_fetch
    ldy #>bad_fetch
    jmp error_dump

bad_fetch:
    .asciiz "Invalid opcode fetch address: "

.endproc

; Fetch a byte from address _RISCV_address into _RISCV_data
.global _RISCV_read_8
.proc _RISCV_read_8

    ; No alignment requirement for 8 bit read

    ldx _RISCV_address+3 ; Select according to address space
    dex
    bne space_2
        ; Address space 1: main memory 
        ; The fence does not apply to reads
        set_reu_address _RISCV_address
        set_local_address _RISCV_data
        set_xfer_size_imm 1
        do_reu_read
        rts
    space_2:
    ; No space other than 1 allows this byte to be non-zero
    lda _RISCV_address+2
    bne bad_address
    ; These parts are common to spaces 2, 3, 4 and 5
    lda #0
    sta local_address+0
    ldy _RISCV_address+0
    lda _RISCV_address+1
    dex
    bne space_3
        ; Address space 2: C64 text mode screen
        cmp #>c64_text_size
        bcs bad_address
        clc
        adc #>c64_text_screen
        sta local_address+1
        lda (local_address),y
        sta _RISCV_data+0
        rts
    space_3:
    dex
    bne space_4
        ; Address space 3: C64 graphics mode space
        cmp #>c64_graph_size
        bcs bad_address
        clc
        adc #>c64_graph_screen
        sta local_address+1
        sei
        lda $00
        tax
        and #$F8    ; I/O and ROMs switched out
        sta $00
        lda (local_address),y
        sta _RISCV_data+0
        stx $00
        cli
        rts
    space_4:
    dex
    bne space_5
        ; Address space 4: C64 I/O space
        cmp #>c64_io_size
        bcs bad_address
        clc
        adc #>c64_io_space
        sta local_address+1
        lda (local_address),y
        sta _RISCV_data+0
        rts
    space_5:
    dex
    bne bad_address
        ; Address space 5: C64 character ROM
        cmp #>c64_character_size
        bcs bad_address
        clc
        adc #>c64_character_rom
        sta local_address+1
        sei
        lda $00
        tax
        ora #$04    ; Switch to character ROM
        sta $00
        lda (local_address),y
        sta _RISCV_data+0
        stx $00
        cli
        rts

bad_address:
    ldx #<bad_addr_str
    ldy #>bad_addr_str
    jmp error_dump

bad_addr_str:
    .asciiz "Invalid 8-bit read address: "

.endproc

.global _RISCV_read_16
.proc _RISCV_read_16

    ; Check address alignment
    lda _RISCV_address+0
    lsr a
    bcs bad_address_0

    ldx _RISCV_address+3 ; Select according to address space
    dex
    bne space_2
        ; Address space 1: main memory 
        ; The fence does not apply to reads
        set_reu_address _RISCV_address
        set_local_address _RISCV_data
        set_xfer_size_imm 2
        do_reu_read
        rts
    space_2:
    ; No space other than 1 allows this byte to be non-zero
    lda _RISCV_address+2
    bne bad_address
    ; These parts are common to spaces 2, 3, 4 and 5
    lda #0
    sta local_address+0
    ldy _RISCV_address+0
    lda _RISCV_address+1
    dex
    bne space_3
        ; Address space 2: C64 text mode screen
        cmp #>c64_text_size
        bcs bad_address
        clc
        adc #>c64_text_screen
        sta local_address+1
        lda (local_address),y
        sta _RISCV_data+0
        iny
        lda (local_address),y
        sta _RISCV_data+1
        rts
    ; Place this here to give relative branches a nearby target
    bad_address_0:
        jmp bad_address
    space_3:
    dex
    bne space_4
        ; Address space 3: C64 graphics mode space
        cmp #>c64_graph_size
        bcs bad_address
        clc
        adc #>c64_graph_screen
        sta local_address+1
        sei
        lda $00
        tax
        and #$F8    ; I/O and ROMs switched out
        sta $00
        lda (local_address),y
        sta _RISCV_data+0
        iny
        lda (local_address),y
        sta _RISCV_data+1
        stx $00
        cli
        rts
    space_4:
    dex
    bne space_5
        ; Address space 4: C64 I/O space
        cmp #>c64_io_size
        bcs bad_address
        clc
        adc #>c64_io_space
        sta local_address+1
        lda (local_address),y
        sta _RISCV_data+0
        iny
        lda (local_address),y
        sta _RISCV_data+1
        rts
    space_5:
    dex
    bne bad_address
        ; Address space 5: C64 character ROM
        cmp #>c64_character_size
        bcs bad_address
        clc
        adc #>c64_character_rom
        sta local_address+1
        sei
        lda $00
        tax
        ora #$04    ; Switch to character ROM
        sta $00
        lda (local_address),y
        sta _RISCV_data+0
        iny
        lda (local_address),y
        sta _RISCV_data+1
        stx $00
        cli
        rts

bad_address:
    ldx #<bad_addr_str
    ldy #>bad_addr_str
    jmp error_dump

bad_addr_str:
    .asciiz "Invalid 16-bit read address: "

.endproc

.global _RISCV_read_32
.proc _RISCV_read_32

    ; Check address alignment
    lda _RISCV_address+0
    and #$03
    bne bad_address_0

    ldx _RISCV_address+3 ; Select according to address space
    dex
    bne space_2
        ; Address space 1: main memory 
        ; The fence does not apply to reads
        set_reu_address _RISCV_address
        set_local_address _RISCV_data
        set_xfer_size_imm 4
        do_reu_read
        rts
    space_2:
    ; No space other than 1 allows this byte to be non-zero
    lda _RISCV_address+2
    bne bad_address_0
    ; These parts are common to spaces 2, 3, 4 and 5
    lda #0
    sta local_address+0
    ldy _RISCV_address+0
    lda _RISCV_address+1
    dex
    bne space_3
        ; Address space 2: C64 text mode screen
        cmp #>c64_text_size
        bcs bad_address_0
        clc
        adc #>c64_text_screen
        sta local_address+1
        lda (local_address),y
        sta _RISCV_data+0
        iny
        lda (local_address),y
        sta _RISCV_data+1
        iny
        lda (local_address),y
        sta _RISCV_data+2
        iny
        lda (local_address),y
        sta _RISCV_data+3
        rts
    ; Place this here to give relative branches a nearby target
    bad_address_0:
        jmp bad_address
    space_3:
    dex
    bne space_4
        ; Address space 3: C64 graphics mode space
        cmp #>c64_graph_size
        bcs bad_address
        clc
        adc #>c64_graph_screen
        sta local_address+1
        sei
        lda $00
        tax
        and #$F8    ; I/O and ROMs switched out
        sta $00
        lda (local_address),y
        sta _RISCV_data+0
        iny
        lda (local_address),y
        sta _RISCV_data+1
        iny
        lda (local_address),y
        sta _RISCV_data+2
        iny
        lda (local_address),y
        sta _RISCV_data+3
        stx $00
        cli
        rts
    space_4:
    dex
    bne space_5
        ; Address space 4: C64 I/O space
        cmp #>c64_io_size
        bcs bad_address
        clc
        adc #>c64_io_space
        sta local_address+1
        lda (local_address),y
        sta _RISCV_data+0
        iny
        lda (local_address),y
        sta _RISCV_data+1
        iny
        lda (local_address),y
        sta _RISCV_data+2
        iny
        lda (local_address),y
        sta _RISCV_data+3
        rts
    space_5:
    dex
    bne bad_address
        ; Address space 5: C64 character ROM
        cmp #>c64_character_size
        bcs bad_address
        clc
        adc #>c64_character_rom
        sta local_address+1
        sei
        lda $00
        tax
        ora #$04    ; Switch to character ROM
        sta $00
        lda (local_address),y
        sta _RISCV_data+0
        iny
        lda (local_address),y
        sta _RISCV_data+1
        iny
        lda (local_address),y
        sta _RISCV_data+2
        iny
        lda (local_address),y
        sta _RISCV_data+3
        stx $00
        cli
        rts

bad_address:
    ldx #<bad_addr_str
    ldy #>bad_addr_str
    jmp error_dump

bad_addr_str:
    .asciiz "Invalid 32-bit read address: "

.endproc

.global _RISCV_write_8
.proc _RISCV_write_8

    ; No alignment requirement for 8 bit write

    ldx _RISCV_address+3 ; Select according to address space
    dex
    bne space_2
        ; Address space 1: main memory 
        ; Check for valid write address
        lda _RISCV_address+2
        cmp RISCV_fence
        bcc bad_address
        set_reu_address _RISCV_address
        set_local_address _RISCV_data
        set_xfer_size_imm 1
        do_reu_write
        rts
    space_2:
    ; No space other than 1 allows this byte to be non-zero
    lda _RISCV_address+2
    bne bad_address
    ; These parts are common to spaces 2, 3 and 4
    lda #0
    sta local_address+0
    ldy _RISCV_address+0
    lda _RISCV_address+1
    dex
    bne space_3
        ; Address space 2: C64 text mode screen
        cmp #>c64_text_size
        bcs bad_address
        clc
        adc #>c64_text_screen
        sta local_address+1
        lda _RISCV_data+0
        sta (local_address),y
        rts
    space_3:
    dex
    bne space_4
        ; Address space 3: C64 graphics mode space
        cmp #>c64_graph_size
        bcs bad_address
        clc
        adc #>c64_graph_screen
        sta local_address+1
        sei
        lda $00
        tax
        and #$F8    ; I/O and ROMs switched out
        sta $00
        lda _RISCV_data+0
        sta (local_address),y
        stx $00
        cli
        rts
    space_4:
    dex
    bne bad_address
        ; Address space 4: C64 I/O space
        cmp #>c64_io_size
        bcs bad_address
        clc
        adc #>c64_io_space
        sta local_address+1
        lda _RISCV_data+0
        sta (local_address),y
        rts
        ; Address space 5 (C64 character ROM) is not valid for write

bad_address:
    ldx #<bad_addr_str
    ldy #>bad_addr_str
    jmp error_dump

bad_addr_str:
    .asciiz "Invalid 8-bit write address: "

.endproc

.global _RISCV_write_16
.proc _RISCV_write_16

    ; Check alignment of address
    lda _RISCV_address+0
    lsr a
    bcs bad_address_0

    ldx _RISCV_address+3 ; Select according to address space
    dex
    bne space_2
        ; Address space 1: main memory 
        ; Check for valid write address
        lda _RISCV_address+2
        cmp RISCV_fence
        bcc bad_address_0
        set_reu_address _RISCV_address
        set_local_address _RISCV_data
        set_xfer_size_imm 2
        do_reu_write
        rts
    space_2:
    ; No space other than 1 allows this byte to be non-zero
    lda _RISCV_address+2
    bne bad_address
    ; These parts are common to spaces 2, 3 and 4
    lda #0
    sta local_address+0
    ldy _RISCV_address+0
    lda _RISCV_address+1
    dex
    bne space_3
        ; Address space 2: C64 text mode screen
        cmp #>c64_text_size
        bcs bad_address
        clc
        adc #>c64_text_screen
        sta local_address+1
        lda _RISCV_data+0
        sta (local_address),y
        iny
        lda _RISCV_data+1
        sta (local_address),y
        rts
    ; Place this here to give relative branches a nearby target
    bad_address_0:
        jmp bad_address
    space_3:
    dex
    bne space_4
        ; Address space 3: C64 graphics mode space
        cmp #>c64_graph_size
        bcs bad_address
        clc
        adc #>c64_graph_screen
        sta local_address+1
        sei
        lda $00
        tax
        and #$F8    ; I/O and ROMs switched out
        sta $00
        lda _RISCV_data+0
        sta (local_address),y
        iny
        lda _RISCV_data+1
        sta (local_address),y
        stx $00
        cli
        rts
    space_4:
    dex
    bne bad_address
        ; Address space 4: C64 I/O space
        cmp #>c64_io_size
        bcs bad_address
        clc
        adc #>c64_io_space
        sta local_address+1
        lda _RISCV_data+0
        sta (local_address),y
        iny
        lda _RISCV_data+1
        sta (local_address),y
        rts
        ; Address space 5 (C64 character ROM) is not valid for write

bad_address:
    ldx #<bad_addr_str
    ldy #>bad_addr_str
    jmp error_dump

bad_addr_str:
    .asciiz "Invalid 16-bit write address: "

.endproc

.global _RISCV_write_32
.proc _RISCV_write_32

    ; Check alignment of address
    lda _RISCV_address+0
    and #$03
    bne bad_address_0

    ldx _RISCV_address+3 ; Select according to address space
    dex
    bne space_2
        ; Address space 1: main memory 
        ; Check for valid write address
        lda _RISCV_address+2
        cmp RISCV_fence
        bcc bad_address_0
        set_reu_address _RISCV_address
        set_local_address _RISCV_data
        set_xfer_size_imm 4
        do_reu_write
        rts
    space_2:
    ; No space other than 1 allows this byte to be non-zero
    lda _RISCV_address+2
    bne bad_address
    ; These parts are common to spaces 2, 3 and 4
    lda #0
    sta local_address+0
    ldy _RISCV_address+0
    lda _RISCV_address+1
    dex
    bne space_3
        ; Address space 2: C64 text mode screen
        cmp #>c64_text_size
        bcs bad_address
        clc
        adc #>c64_text_screen
        sta local_address+1
        lda _RISCV_data+0
        sta (local_address),y
        iny
        lda _RISCV_data+1
        sta (local_address),y
        iny
        lda _RISCV_data+2
        sta (local_address),y
        iny
        lda _RISCV_data+3
        sta (local_address),y
        rts
    ; Place this here to give relative branches a nearby target
    bad_address_0:
        jmp bad_address
    space_3:
    dex
    bne space_4
        ; Address space 3: C64 graphics mode space
        cmp #>c64_graph_size
        bcs bad_address
        clc
        adc #>c64_graph_screen
        sta local_address+1
        sei
        lda $00
        tax
        and #$F8    ; I/O and ROMs switched out
        sta $00
        lda _RISCV_data+0
        sta (local_address),y
        iny
        lda _RISCV_data+1
        sta (local_address),y
        iny
        lda _RISCV_data+2
        sta (local_address),y
        iny
        lda _RISCV_data+3
        sta (local_address),y
        stx $00
        cli
        rts
    space_4:
    dex
    bne bad_address
        ; Address space 4: C64 I/O space
        cmp #>c64_io_size
        bcs bad_address
        clc
        adc #>c64_io_space
        sta local_address+1
        lda _RISCV_data+0
        sta (local_address),y
        iny
        lda _RISCV_data+1
        sta (local_address),y
        iny
        lda _RISCV_data+2
        sta (local_address),y
        iny
        lda _RISCV_data+3
        sta (local_address),y
        rts
        ; Address space 5 (C64 character ROM) is not valid for write

bad_address:
    ldx #<bad_addr_str
    ldy #>bad_addr_str
    jmp error_dump

bad_addr_str:
    .asciiz "Invalid 32-bit write address: "

.endproc

.global _RISCV_invalid_opcode
.proc _RISCV_invalid_opcode

    lda _RISCV_opcode+0
    sta _RISCV_address+0
    lda _RISCV_opcode+1
    sta _RISCV_address+1
    lda _RISCV_opcode+2
    sta _RISCV_address+2
    lda _RISCV_opcode+3
    sta _RISCV_address+3
    ldx #<bad_opcode_str
    ldy #>bad_opcode_str
    jmp error_dump

bad_opcode_str:
    .asciiz "Invalid opcode: "

.endproc

.global _RISCV_ECALL
.proc _RISCV_ECALL

    ; ECALL selector is in A7 (x17)
    ; All defined ECALLs have the two upper bytes equal to zero
    lda _RISCV_ireg_2+REG_a7
    ora _RISCV_ireg_3+REG_a7
    bne no_table

    ; A <- A7 byte 0
    lda _RISCV_ireg_0+REG_a7

    ; Check A7 byte 1
    ldx _RISCV_ireg_1+REG_a7
    bne table_4
        asl a
        bcs @hi_bit
            sta @jump_0+1  ; self-modifying code
            @jump_0:
            jmp (ecall_00) ; actually ecall_00,a
        @hi_bit:
            sta @jump_1+1  ; self-modifying code
            @jump_1:
            jmp (ecall_00+256) ; actually ecall_00+256,a
    table_4:
    cpx #4
    bne table_7
        asl a
        bcs no_table
        sta @jump+1 ; self-modifying code
        @jump:
        jmp (ecall_04) ; actually ecall_04,a
    table_7:
    cpx #7
    bne no_table
        ; Only one defined ECALL here
        cmp #$DB
        bne no_table
        jmp SYS_getmainvars

    no_table:
        jmp bad_ecall

.endproc

; Dump registers on an unknown ECALL

.proc bad_ecall

    lda _RISCV_ireg_0+REG_a7
    sta _RISCV_address+0
    lda _RISCV_ireg_1+REG_a7
    sta _RISCV_address+1
    lda _RISCV_ireg_2+REG_a7
    sta _RISCV_address+2
    lda _RISCV_ireg_3+REG_a7
    sta _RISCV_address+3
    ldx #<bad_ecall_str
    ldy #>bad_ecall_str
    jmp error_dump

bad_ecall_str:
    .asciiz "Invalid ECALL selector: "

.endproc

.global _RISCV_EBREAK
.proc _RISCV_EBREAK

bad_ebreak:
    lda #0
    sta _RISCV_address+0
    sta _RISCV_address+1
    sta _RISCV_address+2
    sta _RISCV_address+3
    ldx #<bad_ebreak_str
    ldy #>bad_ebreak_str
    jmp error_dump

bad_ebreak_str:
    .asciiz "EBREAK instruction: "

.endproc

; Dump registers to the screen on error condition
; On entry: Y:X point to a string identifying the error
;           _RISCV_address contains the address or data that failed
; No return; the program exits from here

.proc error_dump

    ; Save the address for the title
    stx local_address+0
    sty local_address+1

    ; Switch the screen to text mode and clear it 
    jsr reset_screen

    ; Print the title
    jsr print_string
    lda _RISCV_address+3
    jsr print_byte
    lda _RISCV_address+2
    jsr print_byte
    lda _RISCV_address+1
    jsr print_byte
    lda _RISCV_address+0
    jsr print_byte
    lda #$0D
    jsr CHROUT
    lda #$0D
    jsr CHROUT

    ; Print the PC
    lda #<pc_str
    sta local_address+0
    lda #>pc_str
    sta local_address+1
    jsr print_string
    lda _RISCV_pc+3
    jsr print_byte
    lda _RISCV_pc+2
    jsr print_byte
    lda _RISCV_pc+1
    jsr print_byte
    lda _RISCV_pc+0
    jsr print_byte
    lda #$0D
    jsr CHROUT
    lda #$0D
    jsr CHROUT

    ; Print the registers
    lda #0
    sta reg_index
    @reg_print:

        ; Print the title for the register
        ldx reg_index
        lda reg_name_lo,x
        sta local_address+0
        lda reg_name_hi,x
        sta local_address+1
        jsr print_string
        ; Print the register value
        ldx reg_index
        lda _RISCV_ireg_3,x
        jsr print_byte
        ldx reg_index
        lda _RISCV_ireg_2,x
        jsr print_byte
        ldx reg_index
        lda _RISCV_ireg_1,x
        jsr print_byte
        ldx reg_index
        lda _RISCV_ireg_0,x
        jsr print_byte
        ; Arrange into two columns
        lda reg_index
        lsr a
        bcs @end_line
            ; Middle of column
            lda #$20
            jsr CHROUT
            lda #$20
        .byte $2C
        @end_line:
            lda #$0D
        jsr CHROUT

    inc reg_index
    lda reg_index
    cmp #32
    bcc @reg_print

    ; Print the final prompt
    lda #<end_prompt
    sta local_address+0
    lda #>end_prompt
    sta local_address+1
    jsr print_string

    ; Wait for key
    jsr wait_key
    ; Exit the program
    jmp _RISCV_exit

pc_str: .asciiz "PC = "
end_prompt: .byte $0D, "Press SPACE or ENTER to return to BASIC", $0D, 0

reg_name_lo:
    .byte <x0_name
    .byte <x1_name
    .byte <x2_name
    .byte <x3_name
    .byte <x4_name
    .byte <x5_name
    .byte <x6_name
    .byte <x7_name
    .byte <x8_name
    .byte <x9_name
    .byte <x10_name
    .byte <x11_name
    .byte <x12_name
    .byte <x13_name
    .byte <x14_name
    .byte <x15_name
    .byte <x16_name
    .byte <x17_name
    .byte <x18_name
    .byte <x19_name
    .byte <x20_name
    .byte <x21_name
    .byte <x22_name
    .byte <x23_name
    .byte <x24_name
    .byte <x25_name
    .byte <x26_name
    .byte <x27_name
    .byte <x28_name
    .byte <x29_name
    .byte <x30_name
    .byte <x31_name
reg_name_hi:
    .byte >x0_name
    .byte >x1_name
    .byte >x2_name
    .byte >x3_name
    .byte >x4_name
    .byte >x5_name
    .byte >x6_name
    .byte >x7_name
    .byte >x8_name
    .byte >x9_name
    .byte >x10_name
    .byte >x11_name
    .byte >x12_name
    .byte >x13_name
    .byte >x14_name
    .byte >x15_name
    .byte >x16_name
    .byte >x17_name
    .byte >x18_name
    .byte >x19_name
    .byte >x20_name
    .byte >x21_name
    .byte >x22_name
    .byte >x23_name
    .byte >x24_name
    .byte >x25_name
    .byte >x26_name
    .byte >x27_name
    .byte >x28_name
    .byte >x29_name
    .byte >x30_name
    .byte >x31_name

x0_name:  .asciiz "x0      = "
x1_name:  .asciiz "x1 /ra  = "
x2_name:  .asciiz "x2 /sp  = "
x3_name:  .asciiz "x3 /gp  = "
x4_name:  .asciiz "x4 /tp  = "
x5_name:  .asciiz "x5 /t0  = "
x6_name:  .asciiz "x6 /t1  = "
x7_name:  .asciiz "x7 /t2  = "
x8_name:  .asciiz "x8 /fp  = "
x9_name:  .asciiz "x9 /s1  = "
x10_name: .asciiz "x10/a0  = "
x11_name: .asciiz "x11/a1  = "
x12_name: .asciiz "x12/a2  = "
x13_name: .asciiz "x13/a3  = "
x14_name: .asciiz "x14/a4  = "
x15_name: .asciiz "x15/a5  = "
x16_name: .asciiz "x16/a6  = "
x17_name: .asciiz "x17/a7  = "
x18_name: .asciiz "x18/s2  = "
x19_name: .asciiz "x19/s3  = "
x20_name: .asciiz "x20/s4  = "
x21_name: .asciiz "x21/s5  = "
x22_name: .asciiz "x22/s6  = "
x23_name: .asciiz "x23/s7  = "
x24_name: .asciiz "x24/s8  = "
x25_name: .asciiz "x25/s9  = "
x26_name: .asciiz "x26/s10 = "
x27_name: .asciiz "x27/s11 = "
x28_name: .asciiz "x28/t3  = "
x29_name: .asciiz "x29/t4  = "
x30_name: .asciiz "x30/t5  = "
x31_name: .asciiz "x31/t6  = "

.endproc

; Print zero-terminated string at local_address
.proc print_string

    ldy #0
    sty str_index
    @title:
        ldy str_index
        lda (local_address),y
        beq @end_title
        jsr CHROUT
    inc str_index
    bne @title
    @end_title:
    rts

.endproc

; Print byte in A as hex
.proc print_byte

    pha
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda hexdigit,x
    jsr CHROUT
    pla
    and #$0F
    tax
    lda hexdigit,x
    jmp CHROUT

hexdigit: .byte "0123456789ABCDEF"

.endproc

; Low level I/O routines
; TODO: These are stubs to be rewritten when we're on real hardware

; Switch the screen to text mode and clear it
.proc reset_screen

    rts

.endproc

; Wait for a keypress
.proc wait_key

    wait:
        jsr GETIN
    cmp #0
    beq wait
    rts

.endproc

; ECALL address tables; must be page aligned
.segment "PAGEALIGN"
.align 256

; A7 byte 0 == 0

ecall_00:
    .word bad_ecall        ;   0
    .word bad_ecall        ;   1
    .word bad_ecall        ;   2
    .word bad_ecall        ;   3
    .word bad_ecall        ;   4
    .word bad_ecall        ;   5
    .word bad_ecall        ;   6
    .word bad_ecall        ;   7
    .word bad_ecall        ;   8
    .word bad_ecall        ;   9
    .word bad_ecall        ;  10
    .word bad_ecall        ;  11
    .word bad_ecall        ;  12
    .word bad_ecall        ;  13
    .word bad_ecall        ;  14
    .word bad_ecall        ;  15
    .word bad_ecall        ;  16
    .word SYS_getcwd       ;  17
    .word bad_ecall        ;  18
    .word bad_ecall        ;  19
    .word bad_ecall        ;  20
    .word bad_ecall        ;  21
    .word bad_ecall        ;  22
    .word SYS_dup          ;  23
    .word bad_ecall        ;  24
    .word SYS_fcntl        ;  25
    .word bad_ecall        ;  26
    .word bad_ecall        ;  27
    .word bad_ecall        ;  28
    .word bad_ecall        ;  29
    .word bad_ecall        ;  30
    .word bad_ecall        ;  31
    .word bad_ecall        ;  32
    .word bad_ecall        ;  33
    .word bad_ecall        ;  34
    .word bad_ecall        ;  35
    .word bad_ecall        ;  36
    .word bad_ecall        ;  37
    .word bad_ecall        ;  38
    .word bad_ecall        ;  39
    .word bad_ecall        ;  40
    .word bad_ecall        ;  41
    .word bad_ecall        ;  42
    .word bad_ecall        ;  43
    .word bad_ecall        ;  44
    .word bad_ecall        ;  45
    .word bad_ecall        ;  46
    .word bad_ecall        ;  47
    .word SYS_faccessat    ;  48 (supported)
    .word SYS_chdir        ;  49
    .word bad_ecall        ;  50
    .word bad_ecall        ;  51
    .word bad_ecall        ;  52
    .word bad_ecall        ;  53
    .word bad_ecall        ;  54
    .word bad_ecall        ;  55
    .word SYS_openat       ;  56 (supported)
    .word SYS_close        ;  57 (supported)
    .word bad_ecall        ;  58
    .word bad_ecall        ;  59
    .word bad_ecall        ;  60
    .word SYS_getdents     ;  61
    .word SYS_lseek        ;  62 (supported)
    .word SYS_read         ;  63 (supported)
    .word SYS_write        ;  64 (supported)
    .word bad_ecall        ;  65
    .word SYS_writev       ;  66
    .word SYS_pread        ;  67
    .word SYS_pwrite       ;  68
    .word bad_ecall        ;  69
    .word bad_ecall        ;  70
    .word bad_ecall        ;  71
    .word bad_ecall        ;  72
    .word bad_ecall        ;  73
    .word bad_ecall        ;  74
    .word bad_ecall        ;  75
    .word bad_ecall        ;  76
    .word bad_ecall        ;  77
    .word bad_ecall        ;  78
    .word SYS_fstatat      ;  79 (supported)
    .word SYS_fstat        ;  80 (supported)
    .word bad_ecall        ;  81
    .word bad_ecall        ;  82
    .word bad_ecall        ;  83
    .word bad_ecall        ;  84
    .word bad_ecall        ;  85
    .word bad_ecall        ;  86
    .word bad_ecall        ;  87
    .word bad_ecall        ;  88
    .word bad_ecall        ;  89
    .word bad_ecall        ;  90
    .word bad_ecall        ;  91
    .word bad_ecall        ;  92
    .word SYS_exit         ;  93 (supported)
    .word SYS_exit_group   ;  94
    .word bad_ecall        ;  95
    .word bad_ecall        ;  96
    .word bad_ecall        ;  97
    .word bad_ecall        ;  98
    .word bad_ecall        ;  99
    .word bad_ecall        ; 100
    .word bad_ecall        ; 101
    .word bad_ecall        ; 102
    .word bad_ecall        ; 103
    .word bad_ecall        ; 104
    .word bad_ecall        ; 105
    .word bad_ecall        ; 106
    .word bad_ecall        ; 107
    .word bad_ecall        ; 108
    .word bad_ecall        ; 109
    .word bad_ecall        ; 110
    .word bad_ecall        ; 111
    .word bad_ecall        ; 112
    .word bad_ecall        ; 113
    .word bad_ecall        ; 114
    .word bad_ecall        ; 115
    .word bad_ecall        ; 116
    .word bad_ecall        ; 117
    .word bad_ecall        ; 118
    .word bad_ecall        ; 119
    .word bad_ecall        ; 120
    .word bad_ecall        ; 121
    .word bad_ecall        ; 122
    .word bad_ecall        ; 123
    .word bad_ecall        ; 124
    .word bad_ecall        ; 125
    .word bad_ecall        ; 126
    .word bad_ecall        ; 127
    .word bad_ecall        ; 128
    .word SYS_kill         ; 129
    .word bad_ecall        ; 130
    .word bad_ecall        ; 131
    .word bad_ecall        ; 132
    .word bad_ecall        ; 133
    .word SYS_rt_sigaction ; 134
    .word bad_ecall        ; 135
    .word bad_ecall        ; 136
    .word bad_ecall        ; 137
    .word bad_ecall        ; 138
    .word bad_ecall        ; 139
    .word bad_ecall        ; 140
    .word bad_ecall        ; 141
    .word bad_ecall        ; 142
    .word bad_ecall        ; 143
    .word bad_ecall        ; 144
    .word bad_ecall        ; 145
    .word bad_ecall        ; 146
    .word bad_ecall        ; 147
    .word bad_ecall        ; 148
    .word bad_ecall        ; 149
    .word bad_ecall        ; 150
    .word bad_ecall        ; 151
    .word bad_ecall        ; 152
    .word SYS_times        ; 153
    .word bad_ecall        ; 154
    .word bad_ecall        ; 155
    .word bad_ecall        ; 156
    .word bad_ecall        ; 157
    .word bad_ecall        ; 158
    .word bad_ecall        ; 159
    .word SYS_uname        ; 160
    .word bad_ecall        ; 161
    .word bad_ecall        ; 162
    .word bad_ecall        ; 163
    .word bad_ecall        ; 164
    .word bad_ecall        ; 165
    .word bad_ecall        ; 166
    .word bad_ecall        ; 167
    .word bad_ecall        ; 168
    .word SYS_gettimeofday ; 169 (supported)
    .word bad_ecall        ; 170
    .word bad_ecall        ; 171
    .word SYS_getpid       ; 172
    .word bad_ecall        ; 173
    .word SYS_getuid       ; 174
    .word SYS_geteuid      ; 175
    .word SYS_getgid       ; 176
    .word SYS_getegid      ; 177
    .word bad_ecall        ; 178
    .word bad_ecall        ; 179
    .word bad_ecall        ; 180
    .word bad_ecall        ; 181
    .word bad_ecall        ; 182
    .word bad_ecall        ; 183
    .word bad_ecall        ; 184
    .word bad_ecall        ; 185
    .word bad_ecall        ; 186
    .word bad_ecall        ; 187
    .word bad_ecall        ; 188
    .word bad_ecall        ; 189
    .word bad_ecall        ; 190
    .word bad_ecall        ; 191
    .word bad_ecall        ; 192
    .word bad_ecall        ; 193
    .word bad_ecall        ; 194
    .word bad_ecall        ; 195
    .word bad_ecall        ; 196
    .word bad_ecall        ; 197
    .word bad_ecall        ; 198
    .word bad_ecall        ; 199
    .word bad_ecall        ; 200
    .word bad_ecall        ; 201
    .word bad_ecall        ; 202
    .word bad_ecall        ; 203
    .word bad_ecall        ; 204
    .word bad_ecall        ; 205
    .word bad_ecall        ; 206
    .word bad_ecall        ; 207
    .word bad_ecall        ; 208
    .word bad_ecall        ; 209
    .word bad_ecall        ; 210
    .word bad_ecall        ; 211
    .word bad_ecall        ; 212
    .word bad_ecall        ; 213
    .word SYS_brk          ; 214 (supported)
    .word SYS_munmap       ; 215
    .word SYS_mremap       ; 216
    .word bad_ecall        ; 217
    .word bad_ecall        ; 218
    .word bad_ecall        ; 219
    .word bad_ecall        ; 220
    .word bad_ecall        ; 221
    .word SYS_mmap         ; 222
    .word bad_ecall        ; 223
    .word bad_ecall        ; 224
    .word bad_ecall        ; 225
    .word bad_ecall        ; 226
    .word bad_ecall        ; 227
    .word bad_ecall        ; 228
    .word bad_ecall        ; 229
    .word bad_ecall        ; 230
    .word bad_ecall        ; 231
    .word bad_ecall        ; 232
    .word bad_ecall        ; 233
    .word bad_ecall        ; 234
    .word bad_ecall        ; 235
    .word bad_ecall        ; 236
    .word bad_ecall        ; 237
    .word bad_ecall        ; 238
    .word bad_ecall        ; 239
    .word bad_ecall        ; 240
    .word bad_ecall        ; 241
    .word bad_ecall        ; 242
    .word bad_ecall        ; 243
    .word bad_ecall        ; 244
    .word bad_ecall        ; 245
    .word bad_ecall        ; 246
    .word bad_ecall        ; 247
    .word bad_ecall        ; 248
    .word bad_ecall        ; 249
    .word bad_ecall        ; 250
    .word bad_ecall        ; 251
    .word bad_ecall        ; 252
    .word bad_ecall        ; 253
    .word bad_ecall        ; 254
    .word bad_ecall        ; 255

; A7 byte 0 == $04    

ecall_04:
    .word SYS_open         ; 1024 (supported)
    .word SYS_link         ; 1025 (supported)
    .word SYS_unlink       ; 1026 (supported)
    .word bad_ecall        ; 1027
    .word bad_ecall        ; 1028
    .word bad_ecall        ; 1029
    .word SYS_mkdir        ; 1030
    .word bad_ecall        ; 1031
    .word bad_ecall        ; 1032
    .word SYS_access       ; 1033 (supported)
    .word bad_ecall        ; 1034
    .word bad_ecall        ; 1035
    .word bad_ecall        ; 1036
    .word bad_ecall        ; 1037
    .word SYS_stat         ; 1038 (supported)
    .word SYS_lstat        ; 1039 (supported)
    .word bad_ecall        ; 1040
    .word bad_ecall        ; 1041
    .word bad_ecall        ; 1042
    .word bad_ecall        ; 1043
    .word bad_ecall        ; 1044
    .word bad_ecall        ; 1045
    .word bad_ecall        ; 1046
    .word bad_ecall        ; 1047
    .word bad_ecall        ; 1048
    .word bad_ecall        ; 1049
    .word bad_ecall        ; 1050
    .word bad_ecall        ; 1051
    .word bad_ecall        ; 1052
    .word bad_ecall        ; 1053
    .word bad_ecall        ; 1054
    .word bad_ecall        ; 1055
    .word bad_ecall        ; 1056
    .word bad_ecall        ; 1057
    .word bad_ecall        ; 1058
    .word bad_ecall        ; 1059
    .word bad_ecall        ; 1060
    .word bad_ecall        ; 1061
    .word SYS_time         ; 1062
    .word bad_ecall        ; 1063
    .word bad_ecall        ; 1064
    .word bad_ecall        ; 1065
    .word bad_ecall        ; 1066
    .word bad_ecall        ; 1067
    .word bad_ecall        ; 1068
    .word bad_ecall        ; 1069
    .word bad_ecall        ; 1070
    .word bad_ecall        ; 1071
    .word bad_ecall        ; 1072
    .word bad_ecall        ; 1073
    .word bad_ecall        ; 1074
    .word bad_ecall        ; 1075
    .word bad_ecall        ; 1076
    .word bad_ecall        ; 1077
    .word bad_ecall        ; 1078
    .word bad_ecall        ; 1079
    .word bad_ecall        ; 1080
    .word bad_ecall        ; 1081
    .word bad_ecall        ; 1082
    .word bad_ecall        ; 1083
    .word bad_ecall        ; 1084
    .word bad_ecall        ; 1085
    .word bad_ecall        ; 1086
    .word bad_ecall        ; 1087
    .word bad_ecall        ; 1088
    .word bad_ecall        ; 1089
    .word bad_ecall        ; 1090
    .word bad_ecall        ; 1091
    .word bad_ecall        ; 1092
    .word bad_ecall        ; 1093
    .word bad_ecall        ; 1094
    .word bad_ecall        ; 1095
    .word bad_ecall        ; 1096
    .word bad_ecall        ; 1097
    .word bad_ecall        ; 1098
    .word bad_ecall        ; 1099
    .word bad_ecall        ; 1100
    .word bad_ecall        ; 1101
    .word bad_ecall        ; 1102
    .word bad_ecall        ; 1103
    .word bad_ecall        ; 1104
    .word bad_ecall        ; 1105
    .word bad_ecall        ; 1106
    .word bad_ecall        ; 1107
    .word bad_ecall        ; 1108
    .word bad_ecall        ; 1109
    .word bad_ecall        ; 1110
    .word bad_ecall        ; 1111
    .word bad_ecall        ; 1112
    .word bad_ecall        ; 1113
    .word bad_ecall        ; 1114
    .word bad_ecall        ; 1115
    .word bad_ecall        ; 1116
    .word bad_ecall        ; 1117
    .word bad_ecall        ; 1118
    .word bad_ecall        ; 1119
    .word bad_ecall        ; 1120
    .word bad_ecall        ; 1121
    .word bad_ecall        ; 1122
    .word bad_ecall        ; 1123
    .word bad_ecall        ; 1124
    .word bad_ecall        ; 1125
    .word bad_ecall        ; 1126
    .word bad_ecall        ; 1127
    .word bad_ecall        ; 1128
    .word bad_ecall        ; 1129
    .word bad_ecall        ; 1130
    .word bad_ecall        ; 1131
    .word bad_ecall        ; 1132
    .word bad_ecall        ; 1133
    .word bad_ecall        ; 1134
    .word bad_ecall        ; 1135
    .word bad_ecall        ; 1136
    .word bad_ecall        ; 1137
    .word bad_ecall        ; 1138
    .word bad_ecall        ; 1139
    .word bad_ecall        ; 1140
    .word bad_ecall        ; 1141
    .word bad_ecall        ; 1142
    .word bad_ecall        ; 1143
    .word bad_ecall        ; 1144
    .word bad_ecall        ; 1145
    .word bad_ecall        ; 1146
    .word bad_ecall        ; 1147
    .word bad_ecall        ; 1148
    .word bad_ecall        ; 1149
    .word bad_ecall        ; 1150
    .word bad_ecall        ; 1151

; Unpopulated system calls

SYS_faccessat    = bad_ecall
SYS_openat       = bad_ecall
.import SYS_close
SYS_lseek        = bad_ecall
SYS_read         = bad_ecall
.import SYS_write
SYS_fstatat      = bad_ecall
.import SYS_fstat
SYS_exit         = _RISCV_exit
.import SYS_gettimeofday
.import SYS_brk
SYS_open         = bad_ecall
.import SYS_link
SYS_unlink       = bad_ecall
SYS_access       = bad_ecall
SYS_stat         = bad_ecall
SYS_lstat        = bad_ecall

; System calls not currently supported by Newlib

SYS_getcwd       = bad_ecall
SYS_dup          = bad_ecall
SYS_fcntl        = bad_ecall
SYS_chdir        = bad_ecall
SYS_getdents     = bad_ecall
SYS_writev       = bad_ecall
SYS_pread        = bad_ecall
SYS_pwrite       = bad_ecall
SYS_exit_group   = bad_ecall
SYS_kill         = bad_ecall
SYS_rt_sigaction = bad_ecall
SYS_times        = bad_ecall
SYS_uname        = bad_ecall
SYS_getpid       = bad_ecall
SYS_getuid       = bad_ecall
SYS_geteuid      = bad_ecall
SYS_getgid       = bad_ecall
SYS_getegid      = bad_ecall
SYS_munmap       = bad_ecall
SYS_mremap       = bad_ecall
SYS_mmap         = bad_ecall
SYS_mkdir        = bad_ecall
SYS_time         = bad_ecall
SYS_getmainvars  = bad_ecall
