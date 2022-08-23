; vm-api.s

.include "registers.inc"
.include "memory.inc"
.include "kernal.inc"
.include "reu-regs.inc"
.include "vm-api.inc"
.include "farcall.inc"
.include "riscv65.inc"
.include "error.inc"

; Location of text mode screen
c64_text_screen = $0400
c64_text_size = $0400
; Location of graphics mode screen
c64_graph_screen = $C000
c64_graph_size = $4000
; Location of I/O space
c64_io_space = $D000
c64_io_size = $1000
; Location of character ROM
c64_character_rom = $D000
c64_character_size = $1000

.code

; Overall RISC-V machine address space:
; 00xxxxxx -- invalid; null pointer dereference
; 01xxxxxx -- main memory, resident in the REU device
; 02xxxxxx -- C64 text mode screen (1K)
; 03xxxxxx -- C64 graphics mode space (32K)
; 04xxxxxx -- C64 I/O space (4K)
; 05xxxxxx -- C64 character ROM (4K, read only)
; 06xxxxxx and up -- invalid
;
; The fence lies at adress 01ZZ0000 where ZZ is the byte in RISCV_fence.
; Addresses at least 01000000 and less than the fence are valid for instruction
; fetches. Addresses at least equal to the fence and not greater than 01FFFFFF
; are valid for writes. All addresses in main memory are valid for reads.
;
; Opcode fetches may only be performed in main memory. Reads are valid in any
; of the spaces listed above. Writes are valid in spaces 1, 2, 3 and 4.
;
; Opcode fetches, and reads and writes of 32 bits, must be aligned on a four
; byte boundary. Reads and writes of 16 bits must occur on an even address.
; Reads and writes of 8 bits may occur on any valid address.

; Check the address in RISCV_pc
; Called from the RISCV_fetch macro
.global RISCV_check_address
.proc RISCV_check_address

    ; Reset the check flag
    lda #0
    sta RISCV_pc_check

    ; Check that we have a valid execution address

    lda RISCV_pc+3 ; Check for correct address space (main memory only)
    cmp #1
    bne bad_address
    lda RISCV_pc+2 ; Check for address below the fence
    cmp RISCV_fence
    bcs bad_address
    lda RISCV_pc+0 ; Check for alignment
    and #3
    bne bad_address

    ; We're OK
    rts

    ; Oops
bad_address:
    lda RISCV_pc+0
    sta RISCV_address+0
    lda RISCV_pc+1
    sta RISCV_address+1
    lda RISCV_pc+2
    sta RISCV_address+2
    lda RISCV_pc+3
    sta RISCV_address+3
    lda #ERR_bad_fetch
    sta pointer1+0
    jsr_far error_dump_bank,error_dump_entry
    jmp RISCV_exit

.endproc

; Fetch a byte from address RISCV_address into RISCV_data
.global RISCV_read_8
.proc RISCV_read_8

    ; No alignment requirement for 8 bit read

    ldy RISCV_address+3 ; Select according to address space
    dey
    bne space_2
        ; Address space 1: main memory 
        ; The fence does not apply to reads
        lda #1
        ldx #$91
        jmp reu_xfer
    space_2:
    ; No space other than 1 allows this byte to be non-zero
    lda RISCV_address+2
    bne bad_address
    ; This part is common to spaces 2, 3, 4 and 5
    ldx RISCV_address+0
    lda RISCV_address+1
    dey
    bne space_3
        ; Address space 2: C64 text mode screen
        cmp #>c64_text_size
        bcs bad_address
        clc
        adc #>c64_text_screen
        stx pointer1+0
        sta pointer1+1
        ldy #0
        lda (pointer1),y
        sta RISCV_data+0
        rts
    space_3:
    dey
    bne space_4
        ; Address space 3: C64 graphics mode space
        cmp #>c64_graph_size
        bcs bad_address
        clc
        adc #>c64_graph_screen
        tay
        lda #$00    ; I/O and ROMs switched out
        jmp far_read_8
    space_4:
    dey
    bne space_5
        ; Address space 4: C64 I/O space
        cmp #>c64_io_size
        bcs bad_address
        clc
        adc #>c64_io_space
        stx pointer1+0
        sta pointer1+1
        ldy #0
        lda (pointer1),y
        sta RISCV_data+0
        rts
    space_5:
    dey
    bne bad_address
        ; Address space 5: C64 character ROM
        cmp #>c64_character_size
        bcs bad_address
        clc
        adc #>c64_character_rom
        tay
        lda #$04    ; Switch to character ROM
        jmp far_read_8

bad_address:
    lda #ERR_bad_byte_read
    sta pointer1+0
    jsr_far error_dump_bank,error_dump_entry
    jmp RISCV_exit

.endproc

.global RISCV_read_16
.proc RISCV_read_16

    ; Check address alignment
    lda RISCV_address+0
    lsr a
    bcs bad_address

    ldy RISCV_address+3 ; Select according to address space
    dey
    bne space_2
        ; Address space 1: main memory 
        ; The fence does not apply to reads
        lda #2
        ldx #$91
        jmp reu_xfer
    space_2:
    ; No space other than 1 allows this byte to be non-zero
    lda RISCV_address+2
    bne bad_address
    ; This part is common to spaces 2, 3, 4 and 5
    ldx RISCV_address+0
    lda RISCV_address+1
    dey
    bne space_3
        ; Address space 2: C64 text mode screen
        cmp #>c64_text_size
        bcs bad_address
        clc
        adc #>c64_text_screen
        stx pointer1+0
        sta pointer1+1
        ldy #0
        lda (pointer1),y
        sta RISCV_data+0
        iny
        lda (pointer1),y
        sta RISCV_data+1
        rts
    space_3:
    dey
    bne space_4
        ; Address space 3: C64 graphics mode space
        cmp #>c64_graph_size
        bcs bad_address
        clc
        adc #>c64_graph_screen
        tay
        lda #$00    ; I/O and ROMs switched out
        jmp far_read_16
    space_4:
    dey
    bne space_5
        ; Address space 4: C64 I/O space
        cmp #>c64_io_size
        bcs bad_address
        clc
        adc #>c64_io_space
        stx pointer1+0
        sta pointer1+1
        ldy #0
        lda (pointer1),y
        sta RISCV_data+0
        iny
        lda (pointer1),y
        sta RISCV_data+1
        rts
    space_5:
    dey
    bne bad_address
        ; Address space 5: C64 character ROM
        cmp #>c64_character_size
        bcs bad_address
        clc
        adc #>c64_character_rom
        tay
        lda #$04    ; Switch to character ROM
        jmp far_read_16

bad_address:
    lda #ERR_bad_hword_read
    sta pointer1+0
    jsr_far error_dump_bank,error_dump_entry
    jmp RISCV_exit

.endproc

.global RISCV_read_32
.proc RISCV_read_32

    ; Check address alignment
    lda RISCV_address+0
    and #$03
    bne bad_address

    ldy RISCV_address+3 ; Select according to address space
    dey
    bne space_2
        ; Address space 1: main memory 
        ; The fence does not apply to reads
        lda #4
        ldx #$91
        jmp reu_xfer
    space_2:
    ; No space other than 1 allows this byte to be non-zero
    lda RISCV_address+2
    bne bad_address
    ; This part is common to spaces 2, 3, 4 and 5
    ldx RISCV_address+0
    lda RISCV_address+1
    dey
    bne space_3
        ; Address space 2: C64 text mode screen
        cmp #>c64_text_size
        bcs bad_address
        clc
        adc #>c64_text_screen
        stx pointer1+0
        sta pointer1+1
        ldy #0
        lda (pointer1),y
        sta RISCV_data+0
        iny
        lda (pointer1),y
        sta RISCV_data+1
        iny
        lda (pointer1),y
        sta RISCV_data+2
        iny
        lda (pointer1),y
        sta RISCV_data+3
        rts
    space_3:
    dey
    bne space_4
        ; Address space 3: C64 graphics mode space
        cmp #>c64_graph_size
        bcs bad_address
        clc
        adc #>c64_graph_screen
        tay
        lda #$00    ; I/O and ROMs switched out
        jmp far_read_32
    space_4:
    dey
    bne space_5
        ; Address space 4: C64 I/O space
        cmp #>c64_io_size
        bcs bad_address
        clc
        adc #>c64_io_space
        stx pointer1+0
        sta pointer1+1
        ldy #0
        lda (pointer1),y
        sta RISCV_data+0
        iny
        lda (pointer1),y
        sta RISCV_data+1
        iny
        lda (pointer1),y
        sta RISCV_data+2
        iny
        lda (pointer1),y
        sta RISCV_data+3
        rts
    space_5:
    dey
    bne bad_address
        ; Address space 5: C64 character ROM
        cmp #>c64_character_size
        bcs bad_address
        clc
        adc #>c64_character_rom
        tay
        lda #$04    ; Switch to character ROM
        jmp far_read_32

bad_address:
    lda #ERR_bad_dword_read
    sta pointer1+0
    jsr_far error_dump_bank,error_dump_entry
    jmp RISCV_exit

.endproc

.global RISCV_write_8
.proc RISCV_write_8

    ; No alignment requirement for 8 bit write

    ldy RISCV_address+3 ; Select according to address space
    dey
    bne space_2
        ; Address space 1: main memory 
        ; Check for valid write address
        lda RISCV_address+2
        cmp RISCV_fence
        bcc bad_address
        lda #1
        ldx #$90
        jmp reu_xfer
    space_2:
    ; No space other than 1 allows this byte to be non-zero
    lda RISCV_address+2
    bne bad_address
    ; This part is common to spaces 2, 3 and 4
    ldx RISCV_address+0
    lda RISCV_address+1
    dey
    bne space_3
        ; Address space 2: C64 text mode screen
        cmp #>c64_text_size
        bcs bad_address
        clc
        adc #>c64_text_screen
        stx pointer1+0
        sta pointer1+1
        ldy #0
        lda RISCV_data+0
        sta (pointer1),y
        rts
    space_3:
    dey
    bne space_4
        ; Address space 3: C64 graphics mode space
        cmp #>c64_graph_size
        bcs bad_address
        clc
        adc #>c64_graph_screen
        tay
        lda #$00    ; I/O and ROMs switched out
        jmp far_write_8
    space_4:
    dey
    bne bad_address
        ; Address space 4: C64 I/O space
        cmp #>c64_io_size
        bcs bad_address
        clc
        adc #>c64_io_space
        stx pointer1+0
        sta pointer1+1
        ldy #0
        lda RISCV_data+0
        sta (pointer1),y
        rts
        ; Address space 5 (C64 character ROM) is not valid for write

bad_address:
    lda #ERR_bad_byte_write
    sta pointer1+0
    jsr_far error_dump_bank,error_dump_entry
    jmp RISCV_exit

.endproc

.global RISCV_write_16
.proc RISCV_write_16

    ; Check alignment of address
    lda RISCV_address+0
    lsr a
    bcs bad_address

    ldy RISCV_address+3 ; Select according to address space
    dey
    bne space_2
        ; Address space 1: main memory 
        ; Check for valid write address
        lda RISCV_address+2
        cmp RISCV_fence
        bcc bad_address
        lda #2
        ldx #$90
        jmp reu_xfer
    space_2:
    ; No space other than 1 allows this byte to be non-zero
    lda RISCV_address+2
    bne bad_address
    ; This part is common to spaces 2, 3 and 4
    ldx RISCV_address+0
    lda RISCV_address+1
    dey
    bne space_3
        ; Address space 2: C64 text mode screen
        cmp #>c64_text_size
        bcs bad_address
        clc
        adc #>c64_text_screen
        stx pointer1+0
        sta pointer1+1
        ldy #0
        lda RISCV_data+0
        sta (pointer1),y
        iny
        lda RISCV_data+1
        sta (pointer1),y
        rts
    space_3:
    dey
    bne space_4
        ; Address space 3: C64 graphics mode space
        cmp #>c64_graph_size
        bcs bad_address
        clc
        adc #>c64_graph_screen
        tay
        lda #$00    ; I/O and ROMs switched out
        jmp far_write_16
    space_4:
    dey
    bne bad_address
        ; Address space 4: C64 I/O space
        cmp #>c64_io_size
        bcs bad_address
        clc
        adc #>c64_io_space
        stx pointer1+0
        sta pointer1+1
        ldy #0
        lda RISCV_data+0
        sta (pointer1),y
        iny
        lda RISCV_data+1
        sta (pointer1),y
        rts
        ; Address space 5 (C64 character ROM) is not valid for write

bad_address:
    lda #ERR_bad_hword_write
    sta pointer1+0
    jsr_far error_dump_bank,error_dump_entry
    jmp RISCV_exit

.endproc

.global RISCV_write_32
.proc RISCV_write_32

    ; Check alignment of address
    lda RISCV_address+0
    and #$03
    bne bad_address

    ldy RISCV_address+3 ; Select according to address space
    dey
    bne space_2
        ; Address space 1: main memory 
        ; Check for valid write address
        lda RISCV_address+2
        cmp RISCV_fence
        bcc bad_address
        lda #4
        ldx #$90
        jmp reu_xfer
    space_2:
    ; No space other than 1 allows this byte to be non-zero
    lda RISCV_address+2
    bne bad_address
    ; This part is common to spaces 2, 3 and 4
    ldx RISCV_address+0
    lda RISCV_address+1
    dey
    bne space_3
        ; Address space 2: C64 text mode screen
        cmp #>c64_text_size
        bcs bad_address
        clc
        adc #>c64_text_screen
        stx pointer1+0
        sta pointer1+1
        ldy #0
        lda RISCV_data+0
        sta (pointer1),y
        iny
        lda RISCV_data+1
        sta (pointer1),y
        iny
        lda RISCV_data+2
        sta (pointer1),y
        iny
        lda RISCV_data+3
        sta (pointer1),y
        rts
    space_3:
    dey
    bne space_4
        ; Address space 3: C64 graphics mode space
        cmp #>c64_graph_size
        bcs bad_address
        clc
        adc #>c64_graph_screen
        tay
        lda #$00    ; I/O and ROMs switched out
        jmp far_write_32
    space_4:
    dey
    bne bad_address
        ; Address space 4: C64 I/O space
        cmp #>c64_io_size
        bcs bad_address
        clc
        adc #>c64_io_space
        stx pointer1+0
        sta pointer1+1
        ldy #0
        lda RISCV_data+0
        sta (pointer1),y
        iny
        lda RISCV_data+1
        sta (pointer1),y
        iny
        lda RISCV_data+2
        sta (pointer1),y
        iny
        lda RISCV_data+3
        sta (pointer1),y
        rts
        ; Address space 5 (C64 character ROM) is not valid for write

bad_address:
    lda #ERR_bad_dword_write
    sta pointer1+0
    jsr_far error_dump_bank,error_dump_entry
    jmp RISCV_exit

.endproc

; Common routine to access the REU
; On entry: size in A; REU command in X ($91 for read, $90 for write)

.proc reu_xfer

    sta reu_xfer_size_0
    lda #0
    sta reu_xfer_size_1
    lda RISCV_address+0
    sta reu_xmem_address_0
    lda RISCV_address+1
    sta reu_xmem_address_1
    lda RISCV_address+2
    sta reu_xmem_address_2
    lda #<RISCV_data
    sta reu_c64_address_0
    lda #>RISCV_data
    sta reu_c64_address_1
    stx reu_command
    bit reu_status
    rts

.endproc

.global RISCV_invalid_opcode
.proc RISCV_invalid_opcode

    lda RISCV_opcode+0
    sta RISCV_address+0
    lda RISCV_opcode+1
    sta RISCV_address+1
    lda RISCV_opcode+2
    sta RISCV_address+2
    lda RISCV_opcode+3
    sta RISCV_address+3
    lda #ERR_bad_opcode
    sta pointer1+0
    jsr_far error_dump_bank,error_dump_entry
    jmp RISCV_exit

.endproc
