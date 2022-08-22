; error.s -- dump registers on error

.include "error.inc"
.include "farcall.inc"
.include "kernal.inc"
.include "memory.inc"

; Dump registers to the screen on error condition
; On entry: pointer1+0 contains a selector for an error message
;           RISCV_address contains the address or data that failed

.proc error_dump

    reg_index = scratch_area + 0

    ; Save the address for the title
    ldx pointer1+0
    lda message_lo,x
    sta pointer1+0
    lda message_hi,x
    sta pointer1+1

    ;;;;
    ; Wait for a key
    :
        jsr GETIN
    cmp #0
    beq :-
    ;;;;

    ; Switch the screen to text mode and clear it 
    jsr reset_screen

    ; Print the title
    jsr print_string
    lda RISCV_address+3
    jsr print_byte
    lda RISCV_address+2
    jsr print_byte
    lda RISCV_address+1
    jsr print_byte
    lda RISCV_address+0
    jsr print_byte
    lda #$0D
    jsr CHROUT
    lda #$0D
    jsr CHROUT

    ; Print the PC
    lda #<pc_str
    sta pointer1+0
    lda #>pc_str
    sta pointer1+1
    jsr print_string
    lda RISCV_pc+3
    jsr print_byte
    lda RISCV_pc+2
    jsr print_byte
    lda RISCV_pc+1
    jsr print_byte
    lda RISCV_pc+0
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
        sta pointer1+0
        lda reg_name_hi,x
        sta pointer1+1
        jsr print_string
        ; Print the register value
        ldx reg_index
        lda RISCV_ireg_3,x
        jsr print_byte
        ldx reg_index
        lda RISCV_ireg_2,x
        jsr print_byte
        ldx reg_index
        lda RISCV_ireg_1,x
        jsr print_byte
        ldx reg_index
        lda RISCV_ireg_0,x
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
    sta pointer1+0
    lda #>end_prompt
    sta pointer1+1
    jsr print_string

    ;;;;
    ; Wait for a key
    :
        jsr GETIN
    cmp #0
    beq :-
    ;;;;

    rts

pc_str: .asciiz "PC = "
end_prompt: .byte $0D, "Press SPACE or ENTER to return to main menu", $0D, 0

message_lo:
    .byte <message_0
    .byte <message_1
    .byte <message_2
    .byte <message_3
    .byte <message_4
    .byte <message_5
    .byte <message_6
    .byte <message_7
    .byte <message_8
    .byte <message_9

message_hi:
    .byte >message_0
    .byte >message_1
    .byte >message_2
    .byte >message_3
    .byte >message_4
    .byte >message_5
    .byte >message_6
    .byte >message_7
    .byte >message_8
    .byte >message_9

message_0: .asciiz "Invalid opcode fetch address: "
message_1: .asciiz "Invalid 8-bit read address: "
message_2: .asciiz "Invalid 16-bit read address: "
message_3: .asciiz "Invalid 32-bit read address: "
message_4: .asciiz "Invalid 8-bit write address: "
message_5: .asciiz "Invalid 16-bit write address: "
message_6: .asciiz "Invalid 32-bit write address: "
message_7: .asciiz "Invalid opcode: "
message_8: .asciiz "Invalid system call: "
message_9: .asciiz "Invalid EBREAK: "

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

; Reset the screen to text mode
.proc reset_screen

    jsr RESTOR
    jsr CINT
    ; Set the lower case font
    lda #14
    jsr CHROUT
    lda #8
    jsr CHROUT
    rts

.endproc

; Print zero-terminated string at pointer1
.proc print_string

    ldy #0
    @title:
        lda (pointer1),y
        beq @end_title
        jsr CHROUT
    iny
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
