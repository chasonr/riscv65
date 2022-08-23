; init.s -- initialization and warm start routines
; Bank 0 must begin with this file

.include "memory.inc"
.include "kernal.inc"
.include "cmd.inc"
.include "reu.inc"
.include "reu-regs.inc"
.include "reu-load.inc"
.include "ultidos.inc"
.include "error.inc"
.include "farcall.inc"

.import browser

.code

; Entry points and signature
.word cold_start
.word nmi_entry
.byte $C3, $C2, $CD, $38, $30

; Entry jump table for Bank 0
jmp dos_init
jmp dos_open
jmp dos_close
jmp dos_seek
jmp dos_read
jmp dos_read_reu
jmp dos_write
jmp dos_write_reu
jmp dos_change_dir
jmp dos_open_dir
jmp dos_read_dir_first
jmp dos_read_dir_next
jmp dos_get_time
jmp error_dump

; Cold start enters here
.proc cold_start

    ; Write the bank switching thunks

    ldx #far_call_end_thunk - far_call_thunk
    copy_1:
        lda far_call_thunk-1,x
        sta far_call-1,x
    dex
    bne copy_1

    ldx #cold_start_end_thunk - cold_start_thunk
    copy_2:
        lda cold_start_thunk-1,x
        sta cold_start_jump-1,x
    dex
    bne copy_2

    ldx #warm_start_end_thunk - warm_start_thunk
    copy_3:
        lda warm_start_thunk-1,x
        sta warm_start_jump-1,x
    dex
    bne copy_3

    ldx #warm_start_return_end_thunk - warm_start_return_thunk
    copy_4:
        lda warm_start_return_thunk-1,x
        sta warm_start_return-1,x
    dex
    bne copy_4

    ldx #far_read_8_end_thunk - far_read_8_thunk
    copy_5:
        lda far_read_8_thunk-1,x
        sta far_read_8-1,x
    dex
    bne copy_5

    ldx #far_read_16_end_thunk - far_read_16_thunk
    copy_6:
        lda far_read_16_thunk-1,x
        sta far_read_16-1,x
    dex
    bne copy_6

    ldx #far_read_32_end_thunk - far_read_32_thunk
    copy_7:
        lda far_read_32_thunk-1,x
        sta far_read_32-1,x
    dex
    bne copy_7

    ldx #far_write_8_end_thunk - far_write_8_thunk
    copy_8:
        lda far_write_8_thunk-1,x
        sta far_write_8-1,x
    dex
    bne copy_8

    ldx #far_write_16_end_thunk - far_write_16_thunk
    copy_9:
        lda far_write_16_thunk-1,x
        sta far_write_16-1,x
    dex
    bne copy_9

    ldx #far_write_32_end_thunk - far_write_32_thunk
    copy_10:
        lda far_write_32_thunk-1,x
        sta far_write_32-1,x
    dex
    bne copy_10

    ; Copy the RISC-V interpreter into main memory
    ; This thunk needs to run only once, so use the scratch_area instead
    ; of allocating a dedicated area
    ldx #riscv_copy_end - riscv_copy
    copy_11:
        lda riscv_copy-1,x
        sta scratch_area-1,x
    dex
    bne copy_11
    jsr scratch_area

    ; Build the shift tables:
    ldx #0
    shift_loop:
        ; Left shift
        txa
        asl a
        asl a
        sta lshift2,x
        asl a
        sta lshift3,x
        asl a
        sta lshift4,x
        asl a
        sta lshift5,x
        asl a
        sta lshift6,x
        ; Unsigned right shift
        txa
        lsr a
        lsr a
        sta rshift2,x
        lsr a
        sta rshift3,x
        lsr a
        sta rshift4,x
        lsr a
        sta rshift5,x
        lsr a
        sta rshift6,x
        ; Signed right shift
        txa
        cmp #$80
        ror a
        cmp #$80
        ror a
        sta rsshift2,x
        cmp #$80
        ror a
        sta rsshift3,x
        cmp #$80
        ror a
        sta rsshift4,x
        cmp #$80
        ror a
        sta rsshift5,x
        cmp #$80
        ror a
        sta rsshift6,x
    inx
    bne shift_loop

    ; Populate petscii_to_screen
    ldx #0
    petscii_tbl:
        ldy rshift5,x
        txa
        and #$1F
        ora screen_map,y
        sta petscii_to_screen,x
    inx
    bne petscii_tbl

    ; Populate ascii_to_petscii and petscii_to_ascii
    ldx #0
    petscii_tbl_3:
        lda ascii_to_petscii_tbl,x
        sta ascii_to_petscii,x
        tay
        txa
        sta petscii_to_ascii,y
    inx
    bne petscii_tbl_3

    ; Populate ascii_to_screen
    ldx #0
    ascii_tbl:
        ldy ascii_to_petscii,x
        lda petscii_to_screen,y
        sta ascii_to_screen,x
    inx
    bpl ascii_tbl

    jmp warm_start

    ; Map from PETSCII to screen codes
screen_map:
    .byte $80   ; Lower control codes
    .byte $20   ; Symbols and digits
    .byte $00   ; Lower case
    .byte $C0   ; Upper case, alternate map
    .byte $A0   ; Upper control codes
    .byte $60   ; PETSCII symbols
    .byte $40   ; Upper case
    .byte $E0   ; PETSCII symbols, alternate map

.endproc

; Enter here on NMI from the RESTORE key; either directly or via
; warm_start_jump

.proc nmi_entry

    jsr STOP
    beq @warm_start
    jmp warm_start_return

    @warm_start:
    ldx #$FF
    txs
    jmp warm_start

.endproc

; Do warm start

.proc warm_start

    ; Calls to set up the Kernal
    jsr IOINIT
    jsr RAMTAS
    jsr RESTOR
    jsr CINT

    ; Set the lower case font
    lda #14
    jsr CHROUT
    lda #8
    jsr CHROUT

    ; The zero page is cleared at this point
    ; The current bank is bank 0
    lda #$80
    sta current_bank

    ; Clear memory areas
    lda #0
    sta RISCV_pc+0
    sta RISCV_pc+1
    sta RISCV_pc+2
    sta RISCV_pc+3
    ldx #32
    @clear:
        sta RISCV_ireg_0-1,x
        sta RISCV_ireg_1-1,x
        sta RISCV_ireg_2-1,x
        sta RISCV_ireg_3-1,x
    dex
    bne @clear

    ; Check for command interface
    lda CMD_IDENTIFICATION
    cmp #$C9
    beq :+
        jmp bad_platform
    :

    ; Check size of REU
    jsr reu_size
    cmp #24
    bcs :+
        jmp bad_platform
    :

    main_loop:
        ; Go to the browser
        jsr browser

        ; Load the target into the REU
        jsr reu_load

        ; Run the target
        jsr RISCV_emulator_entry

        ; Wait for a key
        :
            jsr GETIN
        cmp #0
        beq :-

    jmp main_loop

.endproc

; Query the size of the RAM Expansion Unit
; On return: A is the base-2 logarithm of the size, or 0 if there is no REU

.proc reu_size

    bytes55 = scratch_area + 0
    bytesAA = bytes55 + 8
    rbytes = bytesAA + 8
    log_addr = rbytes + 8

    jsr reu_present
    bne :+
        ; No REU is present
        lda #0
        rts
    :

    ; Build test patterns
    ldx #8
    @fill_1:
        lda #$55
        sta bytes55-1,x
        lda #$AA
        sta bytesAA-1,x
    dex
    bne @fill_1

    ; Main loop: assume 16M, and halve until we have the size or until
    ; less than 128K
    ; log_addr   assumed size
    ;    8           16M
    ;    7           8M
    ;    6           4M
    ;    5           2M
    ;    4           1M
    ;    3           512K
    ;    2           256K
    ;    1           128K
    lda #8
    sta log_addr
    main_loop:

        ; Write bytes55 to address zero
        lda #0
        sta reu_xmem_address+0
        sta reu_xmem_address+1
        sta reu_xmem_address+2
        set_local_address bytes55
        set_xfer_size_imm 8
        jsr reu_write

        ; Write bytesAA to the test address
        lda #0
        sta reu_xmem_address+0
        sta reu_xmem_address+1
        ldx log_addr
        lda test_addr_hi-1,x
        sta reu_xmem_address+2
        set_local_address bytesAA
        set_xfer_size_imm 8
        jsr reu_write

        ; Read back from the test address
        lda #0
        sta reu_xmem_address+0
        sta reu_xmem_address+1
        ldx log_addr
        lda test_addr_hi-1,x
        sta reu_xmem_address+2
        set_local_address rbytes
        set_xfer_size_imm 8
        jsr reu_read

        ; Should be the same as bytesAA
        ldx #8
        @cmp_1:
            lda rbytes-1,x
            cmp bytesAA-1,x
            bne no_match
        dex
        bne @cmp_1

        ; Read back from address 0
        lda #0
        sta reu_xmem_address+0
        sta reu_xmem_address+1
        sta reu_xmem_address+2
        set_local_address rbytes
        set_xfer_size_imm 8
        jsr reu_read

        ; Should be the same as bytes55
        ldx #8
        @cmp_2:
            lda rbytes-1,x
            cmp bytes55-1,x
            bne no_match
        dex
        bne @cmp_2

        ; Test is successful
        ; Get the index and convert to power of two
        lda log_addr
        clc
        adc #24-8
        rts

    ; Come here on any match failure
    no_match:
    dec log_addr
    beq :+
        jmp main_loop
    :

    ; No size checks out
    lda #0
    rts

; Test addresses
test_addr_hi: .byte $01, $02, $04, $08, $10, $20, $40, $80

.endproc

; Check for presence of an REU
; Return A nonzero if an REU is found

.proc reu_present

    ; Check for writability of reu_c64_address_0

    lda #$55
    sta reu_c64_address_0
    cmp reu_c64_address_0
    bne no_reu

    lda #$AA
    sta reu_c64_address_0
    cmp reu_c64_address_0
    bne no_reu

    ; REU is detected
    ; Set the address control register to 0, saving a step when we do a
    ; transfer
    lda #0
    sta reu_address_control
    ; Disable interrupts from the REU
    sta reu_irq_mask
    ; Indicate REU detected
    lda #1
    rts

    ; No REU is detected
no_reu:
    lda #0
    rts

.endproc

; Report unusable platform and bail

.proc bad_platform

    ldx #0
    @print:
        lda platform_error+0,x
        jsr CHROUT
    inx
    bne @print

scope_1:

    ldx #0
    @print:
        lda platform_error+$100,x
        beq @end_print
        jsr CHROUT
    inx
    bne @print
    @end_print:

    ; There's no way to disable the cartridge and quit to BASIC
    @halt:
    jmp @halt

platform_error:
.byte 147
      ;1234567890123456789012345678901234567890
.byte "Hardware requirements for this program:", 13
.byte "* You must be running a 1541-Ultimate", 13
.byte "  cartridge or Ultimate 64 board.", 13
.byte "* The command interface must be enabled."
.byte "* The REU must be enabled and set to", 13
.byte "  16 megabytes.", 13
.byte "All of these settings can be found in", 13
.byte "the device menu, under", 13
.byte "       C64 and Cartridge Settings", 13, 13
.byte "If you have an Ultimate 64 board, you", 13
.byte "should also set:", 13
.byte "* CPU Speed to 48 MHz", 13
.byte "* Badline Timing to Disabled", 13
.byte "These settings can be found in the", 13
.byte "device menu, under", 13
.byte "         U64 Specific Settings", 13, 13
.byte 0

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Thunks copied to RAM for bank switching:

; Far call; enter with the bank number in A, the high byte of the address in Y
; and the low byte of the address in X

.proc far_call_thunk

    ; Modify the JSR instruction.
    ; Careful: the target address must be that of the copy in RAM, not the
    ; copy in ROM.
    stx far_call + (@far_call-far_call_thunk) + 1
    sty far_call + (@far_call-far_call_thunk) + 2
    ; Save the current bank
    tax
    lda current_bank
    pha
    ; Switch to the new bank
    stx current_bank
    stx $DE00
    ; Run the target routine
    @far_call:
    jsr $FFFF
    ; Retrieve the previous bank
    pla
    sta current_bank
    sta $DE00

    rts

.endproc
far_call_end_thunk:

; Cold start; entered on hard reset
; No need to save the current bank

.proc cold_start_thunk

    lda #$80
    sta current_bank
    sta $DE00
    jmp ($8000)

.endproc
cold_start_end_thunk:

; Warm start; entered when RESTORE key pressed
; The Kernal has already saved A, X and Y, but we'll need to pop those before
; doing an RTI, if we're not going to respond to RESTORE

.proc warm_start_thunk

    ; Switch to bank 0 but leave current_bank set
    lda #$80
    sta $DE00
    jmp ($8002)

.endproc
warm_start_end_thunk:

; Refuse warm start (RUN/STOP not pressed)

.proc warm_start_return_thunk

    lda current_bank
    sta $DE00
    pla
    tay
    pla
    tax
    pla
    rti

.endproc
warm_start_return_end_thunk:

; Read byte from banked area
; On entry: A = 6510 bank setting; Y:X = address
; Return byte in RISCV_data

.proc far_read_8_thunk

    stx pointer1+0
    sty pointer1+1
    ldx $00
    ldy #0
    sei
    sta $00
    lda (pointer1),y
    sta RISCV_data+0
    stx $00
    cli
    rts

.endproc
far_read_8_end_thunk:

; Read 16-bit word from banked area
; On entry: A = 6510 bank setting; Y:X = address
; Return word in RISCV_data

.proc far_read_16_thunk

    stx pointer1+0
    sty pointer1+1
    ldx $00
    ldy #0
    sei
    sta $00
    lda (pointer1),y
    sta RISCV_data+0
    iny
    lda (pointer1),y
    sta RISCV_data+1
    stx $00
    cli
    rts

.endproc
far_read_16_end_thunk:

; Read 32-bit word from banked area
; On entry: A = 6510 bank setting; Y:X = address
; Return word in RISCV_data

.proc far_read_32_thunk

    stx pointer1+0
    sty pointer1+1
    ldx $00
    ldy #0
    sei
    sta $00
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
    stx $00
    cli
    rts

.endproc
far_read_32_end_thunk:

; Write byte to banked area
; On entry: A = 6510 bank setting; Y:X = address; byte in RISCV_data

.proc far_write_8_thunk

    stx pointer1+0
    sty pointer1+1
    ldx $00
    ldy #0
    sei
    sta $00
    lda RISCV_data+0
    sta (pointer1),y
    stx $00
    cli
    rts

.endproc
far_write_8_end_thunk:

; Write 16-bit word to banked area
; On entry: A = 6510 bank setting; Y:X = address; word in RISCV_data

.proc far_write_16_thunk

    stx pointer1+0
    sty pointer1+1
    ldx $00
    ldy #0
    sei
    sta $00
    lda RISCV_data+0
    sta (pointer1),y
    iny
    lda RISCV_data+1
    sta (pointer1),y
    stx $00
    cli
    rts

.endproc
far_write_16_end_thunk:

; Write 32-bit word to banked area
; On entry: A = 6510 bank setting; Y:X = address; word in RISCV_data

.proc far_write_32_thunk

    stx pointer1+0
    sty pointer1+1
    ldx $00
    ldy #0
    sei
    sta $00
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
    stx $00
    cli
    rts

.endproc
far_write_32_end_thunk:

; This copies the RISC-V interpreter into RAM

.proc riscv_copy

    ; Set the destination address
    lda #<RISCV_emulator_entry
    sta pointer1+0
    lda #>RISCV_emulator_entry
    sta pointer1+1
    ldy #0

    ; Copy RISCV_instruction_bank_1
    lda #RISCV_instruction_bank_1
    sta current_bank
    sta $DE00
    lda #<$8009
    sta pointer2+0
    lda #>$8009
    sta pointer2+1
    copy_1:
        lda (pointer2),y
        sta (pointer1),y
        inc pointer1+0
        bne :+
            inc pointer1+1
        :
        inc pointer2+0
        bne :+
            inc pointer2+1
        :
    lda pointer2+1
    cmp #$A0
    bcc copy_1

    ; Copy RISCV_instruction_bank_2
    lda #RISCV_instruction_bank_2
    sta current_bank
    sta $DE00
    lda #<$8009
    sta pointer2+0
    lda #>$8009
    sta pointer2+1
    copy_2:
        lda (pointer2),y
        sta (pointer1),y
        inc pointer1+0
        bne :+
            inc pointer1+1
        :
        inc pointer2+0
        bne :+
            inc pointer2+1
        :
    lda pointer2+1
    cmp #$A0
    bcc copy_2

    ; Return to the caller
    lda #0
    sta current_bank
    sta $DE00
    rts

.endproc
riscv_copy_end:

.rodata

; Table to convert ASCII to PETSCII for the screen
; This is based on the mapping used by cc65, but positions $A4, $AB, $AD, $B1,
; $B3 and $BF are changed to produce an invertible mapping
ascii_to_petscii_tbl:
    .byte $00, $01, $02, $03, $04, $05, $06, $07
    .byte $14, $09, $0D, $11, $93, $0A, $0E, $0F
    .byte $10, $0B, $12, $13, $08, $15, $16, $17
    .byte $18, $19, $1A, $1B, $1C, $1D, $1E, $1F
    .byte $20, $21, $22, $23, $24, $25, $26, $27
    .byte $28, $29, $2A, $2B, $2C, $2D, $2E, $2F
    .byte $30, $31, $32, $33, $34, $35, $36, $37
    .byte $38, $39, $3A, $3B, $3C, $3D, $3E, $3F
    .byte $40, $C1, $C2, $C3, $C4, $C5, $C6, $C7
    .byte $C8, $C9, $CA, $CB, $CC, $CD, $CE, $CF
    .byte $D0, $D1, $D2, $D3, $D4, $D5, $D6, $D7
    .byte $D8, $D9, $DA, $5B, $BF, $5D, $5E, $A4
    .byte $AD, $41, $42, $43, $44, $45, $46, $47
    .byte $48, $49, $4A, $4B, $4C, $4D, $4E, $4F
    .byte $50, $51, $52, $53, $54, $55, $56, $57
    .byte $58, $59, $5A, $B3, $DD, $AB, $B1, $DF
    .byte $80, $81, $82, $83, $84, $85, $86, $87
    .byte $88, $89, $8A, $8B, $8C, $8D, $8E, $8F
    .byte $90, $91, $92, $0C, $94, $95, $96, $97
    .byte $98, $99, $9A, $9B, $9C, $9D, $9E, $9F
    .byte $A0, $A1, $A2, $A3, $5F, $A5, $A6, $A7
    .byte $A8, $A9, $AA, $DC, $AC, $C0, $AE, $AF
    .byte $B0, $DE, $B2, $DB, $B4, $B5, $B6, $B7
    .byte $B8, $B9, $BA, $BB, $BC, $BD, $BE, $5C
    .byte $60, $61, $62, $63, $64, $65, $66, $67
    .byte $68, $69, $6A, $6B, $6C, $6D, $6E, $6F
    .byte $70, $71, $72, $73, $74, $75, $76, $77
    .byte $78, $79, $7A, $7B, $7C, $7D, $7E, $7F
    .byte $E0, $E1, $E2, $E3, $E4, $E5, $E6, $E7
    .byte $E8, $E9, $EA, $EB, $EC, $ED, $EE, $EF
    .byte $F0, $F1, $F2, $F3, $F4, $F5, $F6, $F7
    .byte $F8, $F9, $FA, $FB, $FC, $FD, $FE, $FF
