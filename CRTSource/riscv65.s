; riscv65.s -- main dispatch for RISC-V instructions
; Supported extensions: M, Zifencei

.include "registers.inc"
.include "reu-regs.inc"
.include "memory.inc"
.include "farcall.inc"
.include "riscv65.inc"
.include "vm-api.inc"

; Fetch one opcode into RISCV_opcode
.macro RISCV_fetch

    ; If RISCV_pc_check is nonzero, either the PC has crossed a 64K boundary
    ; since the last check, or a branch instruction has been taken

    lda RISCV_pc_check
    beq :+
        jsr RISCV_check_address
    :

    ; Read one opcode
    ; Use the REU directly instead of going through reu.s. We need to avoid
    ; the performance hit, and alignment of the PC will prevent crossing a
    ; 512K boundary.

    lda RISCV_pc+0
    sta reu_xmem_address_0
    lda RISCV_pc+1
    sta reu_xmem_address_1
    lda RISCV_pc+2
    sta reu_xmem_address_2
    lda #<RISCV_opcode
    sta reu_c64_address_0
    lda #>RISCV_opcode
    sta reu_c64_address_1
    lda #4
    sta reu_xfer_size_0
    lda #0
    sta reu_xfer_size_1
    lda #$91
    sta reu_command
    bit reu_status

.endmacro

.code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                           Emulation entry point                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc RISCV_emulator

    ; Save the stack pointer
    tsx
    sta RISCV_saved_sp

    ; Set up the RISC-V state
    jsr RISCV_init

    ; Loop on RISC-V instructions until RISCV_exit called
    jmp RISCV_instruction

.endproc

; Jump here from anywhere to exit the emulation
.proc RISCV_exit

    ; Shut down the file system
    jsr_far RISCV_syscall_exit_bank,RISCV_syscall_exit_entry

    ; Reset the stack pointer
    ldx RISCV_saved_sp
    txs

    rts

.endproc

.proc RISCV_init

    ; Set the integer registers to zero, escept for the stack pointer
    lda #0
    ldx #31
    zero_loop:
        cpx #REG_sp
        beq :+
            sta RISCV_ireg_0,x
            sta RISCV_ireg_1,x
            sta RISCV_ireg_2,x
            sta RISCV_ireg_3,x
        :
    dex
    bpl zero_loop

    ; Set up the API
    jsr_far RISCV_syscall_init_bank,RISCV_syscall_init_entry

    rts

.endproc

RISCV_instruction:
    ; Fetch opcode
    RISCV_fetch

dispatch_instruction:
    ; Decode the lower seven bits
    lda RISCV_opcode+0      ; (3)
    asl a                   ; (2)
    tax                     ; (2)
    lda dispatch_main+0,x   ; (4)
    sta pointer1+0          ; (3)
    lda dispatch_main+1,x   ; (4)
    sta pointer1+1          ; (3)
    jmp (pointer1)          ; (5)
                            ; total: 26

.segment "ROPAGE"
.align 256

; Main opcode dispatch table

dispatch_main:
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word dispatch_LOAD
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode ; RV32F, RV32D
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word custom_0B
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word do_FENCE
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word dispatch_OP_IMM
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word do_AUIPC
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode ; RV64I: ADDIW, SSLIW, SRLIW, SRAIW
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word dispatch_STORE
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode ; RV32F, RV32D
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word custom_2B
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode ; RV32A: various
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word dispatch_OP
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word do_LUI
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode ; RV64I: ADDW, SUBW, SLLW, SRLW, SRAW; RV64M: MULW, DIVW, DIVUW, REMW, REMUW
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode ; RV32F, RV32D
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode ; RV32F, RV32D
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode ; RV32F, RV32D
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode ; RV32F, RV32D
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode ; RV32F, RV32D
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word custom_5B
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word dispatch_BRANCH
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word do_JALR
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word do_JAL
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word do_ECALL_EBREAK   ; Zicsr instructions are implemented here
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word custom_7B
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode
    .word invalid_opcode

.code

; Instruction routine ends by advancing or modifying PC and jumping back
; to RISCV_instruction.
; MULH, MULHSU, MULU, DIV and DIVU have their own return paths, to check
; for a matching MUL, REM or REMU.

; Advance the PC
.macro advance_pc size
    .local pc_done
    clc                 ; (2)
    lda RISCV_pc+0      ; (3)
    adc #size           ; (2)
    sta RISCV_pc+0      ; (3)
    bcc pc_done         ; (2) (3)
    inc RISCV_pc+1      ; (5) (0)
    bne pc_done         ; (2) (0)
    inc RISCV_pc_check  ; (5) (0)
    inc RISCV_pc+2      ; (5) (0)
    bne pc_done         ; (2) (0)
    inc RISCV_pc+3      ; (5) (0)
    pc_done:
.endmacro               ; total: min 13 max 36

; Extract the destination register (rd) index into X
; Z set if this index is zero
.macro get_rd_x
    lda RISCV_opcode+0   ; (3)
    asl a                ; (2)
    lda RISCV_opcode+1   ; (3)
    rol a                ; (2)
    and #$1F             ; (2)
    tax                  ; (2)
.endmacro                ; total: 14

; Extract the destination register (rd) index into Y
; Z set if this index is zero
.macro get_rd_y
    lda RISCV_opcode+0   ; (3)
    asl a                ; (2)
    lda RISCV_opcode+1   ; (3)
    rol a                ; (2)
    and #$1F             ; (2)
    tay                  ; (2)
.endmacro                ; total: 14

; Extract the RS1 field into X
.macro get_rs1_x
    lda RISCV_opcode+1  ; (3)
    asl a               ; (2)
    lda RISCV_opcode+2  ; (3)
    rol a               ; (2)
    and #$1F            ; (2)
    tax                 ; (2)
.endmacro               ; total: 14

; Extract the RS2 field into A
.macro get_rs2_a
    lda RISCV_opcode+3  ; (3)
    lsr a               ; (2)
    lda RISCV_opcode+2  ; (3)
    ror a               ; (2)
    tay                 ; (2)
    lda rshift3,y       ; (4)
.endmacro               ; total: 16

; Extract the rs1 field into X and the rs2 field into Y
.macro get_rs1_rs2
    get_rs1_x           ; (12)
    get_rs2_a           ; (16)
    tay                 ; ( 2)
.endmacro               ; total: 30

; Extract the immediate field into imm
; Only three bytes are written; the fourth byte is equal to the third
; This macro clobbers X
.macro get_imm12
    ldx RISCV_opcode+2  ; (3)
    lda rshift4,x       ; (4)
    ldx RISCV_opcode+3  ; (3)
    ora lshift4,x       ; (4)
    sta imm_0           ; (3)
    lda rsshift4,x      ; (4)
    sta imm_1           ; (3)
    tax                 ; (2)
    lda rsshift4,x      ; (4)
    sta imm_2           ; (3)
.endmacro               ; 33

; Extract the immediate field of an S-type instruction
; This macro clobbers X
.macro get_imm_S
    lda RISCV_opcode+0     ; (3)
    asl a                  ; (2)
    lda RISCV_opcode+1     ; (3)
    rol a                  ; (2)
    and #$1F               ; (2)
    ldx RISCV_opcode+3     ; (3)
    ora branch_lookup_30,x ; (4)
    sta imm_0              ; (3)
    lda rsshift4,x         ; (4)
    sta imm_1              ; (3)
    tax                    ; (2)
    lda rsshift4,x         ; (4)
    sta imm_2              ; (3)
.endmacro                  ; 38

; Unscramble the offset of a conditional branch and take the branch
.macro do_branch
    lda RISCV_opcode+1     ; A = ... ... ... ... b04 b03 b02 b01
    asl a                  ; A = ... ... ... b04 b03 b02 b01 ...
    and #$1E               ; A =   0   0   0 b04 b03 b02 b01   0
    ldy RISCV_opcode+3     ; Y = b12 b10 b09 b08 b07 b06 b05 ...
    ora branch_lookup_30,y ; A = b07 b06 b05 b04 b03 b02 b01   0
    clc
    adc RISCV_pc+0
    sta RISCV_pc+0

    ldx RISCV_opcode+0     ; X = b11 ... ... ... ... ... ... ...
    lda rshift4,x          ; A =   0   0   0   0 b11 ... ... ...
    and #$08               ; A =   0   0   0   0 b11   0   0   0
    ora branch_lookup_31,y ; A = b12 b12 b12 b12 b11 b10 b09 b08
    tay                    ; Y = b12 b12 b12 b12 b11 b10 b09 b08
    adc RISCV_pc+1
    sta RISCV_pc+1

    lda rsshift4,y         ; A = b12 b12 b12 b12 b12 b12 b12 b12
    tay                    ; Y = b12 b12 b12 b12 b12 b12 b12 b12
    adc RISCV_pc+2
    sta RISCV_pc+2

    tya                    ; A = b12 b12 b12 b12 b12 b12 b12 b12
    adc RISCV_pc+3
    sta RISCV_pc+3

    inc RISCV_pc_check
.endmacro

.proc dispatch_LOAD

    lda RISCV_opcode+1
    and #$70
    ora #table_LOAD
    tax
    lda dispatch_funct3+0,x
    sta pointer1+0
    lda dispatch_funct3+1,x
    sta pointer1+1

    get_imm12
    get_rs1_x

    clc
    lda RISCV_ireg_0,x
    adc imm_0
    sta RISCV_address+0
    lda RISCV_ireg_1,x
    adc imm_1
    sta RISCV_address+1
    lda RISCV_ireg_2,x
    adc imm_2
    sta RISCV_address+2
    lda RISCV_ireg_3,x
    adc imm_2 ; not imm_3
    sta RISCV_address+3

    jmp (pointer1)

.endproc

.proc do_LB

    jsr RISCV_read_8

    get_rd_x
    beq @end
        lda RISCV_data+0
        sta RISCV_ireg_0,x
        cmp #$80
        lda #0
        sbc #0
        eor #$FF
        sta RISCV_ireg_1,x
        sta RISCV_ireg_2,x
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_LBU

    jsr RISCV_read_8

    get_rd_x
    beq @end
        lda RISCV_data+0
        sta RISCV_ireg_0,x
        lda #0
        sta RISCV_ireg_1,x
        sta RISCV_ireg_2,x
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_LH

    jsr RISCV_read_16

    get_rd_x
    beq @end
        lda RISCV_data+0
        sta RISCV_ireg_0,x
        lda RISCV_data+1
        sta RISCV_ireg_1,x
        cmp #$80
        lda #0
        sbc #0
        eor #$FF
        sta RISCV_ireg_2,x
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_LHU

    jsr RISCV_read_16

    get_rd_x
    beq @end
        lda RISCV_data+0
        sta RISCV_ireg_0,x
        lda RISCV_data+1
        sta RISCV_ireg_1,x
        lda #0
        sta RISCV_ireg_2,x
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_LW

    jsr RISCV_read_32

    get_rd_x
    beq @end
        lda RISCV_data+0
        sta RISCV_ireg_0,x
        lda RISCV_data+1
        sta RISCV_ireg_1,x
        lda RISCV_data+2
        sta RISCV_ireg_2,x
        lda RISCV_data+3
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_FENCE

    ; funct3 must be 000 (FENCE) or 001 (FENCE.I)
    lda RISCV_opcode+1
    and #$60
    bne bad_opcode

    ; FENCE is a no-op

    advance_pc 4
    jmp RISCV_instruction

bad_opcode:
    jmp invalid_opcode

.endproc

.proc dispatch_OP_IMM

    lda RISCV_opcode+1
    and #$70
   ;ora #table_OP_IMM   ; equal to zero
    tax
    lda dispatch_funct3+0,x
    sta pointer1+0
    lda dispatch_funct3+1,x
    sta pointer1+1
    jmp (pointer1)

.endproc

.proc do_ADDI

    get_imm12
    get_rs1_x
    get_rd_y
    beq @end

        clc
        lda RISCV_ireg_0,x
        adc imm_0
        sta RISCV_ireg_0,y
        lda RISCV_ireg_1,x
        adc imm_1
        sta RISCV_ireg_1,y
        lda RISCV_ireg_2,x
        adc imm_2
        sta RISCV_ireg_2,y
        lda RISCV_ireg_3,x
        adc imm_2 ; not imm_3
        sta RISCV_ireg_3,y

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SLTI

    get_imm12
    get_rs1_x
    get_rd_y
    beq @end

        lda RISCV_ireg_0,x
        cmp imm_0
        lda RISCV_ireg_1,x
        sbc imm_1
        lda RISCV_ireg_2,x
        sbc imm_2
        lda RISCV_ireg_3,x
        sbc imm_2 ; not imm_3
        bvc @no_overflow
            eor #$80
        @no_overflow:

        rol a
        lda #0
        rol a

        sta RISCV_ireg_0,y
        lda #0
        sta RISCV_ireg_1,y
        sta RISCV_ireg_2,y
        sta RISCV_ireg_3,y

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SLTIU

    get_imm12
    get_rs1_x
    get_rd_y
    beq @end

        lda RISCV_ireg_0,x
        cmp imm_0
        lda RISCV_ireg_1,x
        sbc imm_1
        lda RISCV_ireg_2,x
        sbc imm_2
        lda RISCV_ireg_3,x
        sbc imm_2 ; not imm_3

        lda #0
        rol a
        eor #1

        sta RISCV_ireg_0,y
        lda #0
        sta RISCV_ireg_1,y
        sta RISCV_ireg_2,y
        sta RISCV_ireg_3,y

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_XORI

    get_imm12
    get_rs1_x
    get_rd_y
    beq @end

        lda RISCV_ireg_0,x
        eor imm_0
        sta RISCV_ireg_0,y
        lda RISCV_ireg_1,x
        eor imm_1
        sta RISCV_ireg_1,y
        lda RISCV_ireg_2,x
        eor imm_2
        sta RISCV_ireg_2,y
        lda RISCV_ireg_3,x
        eor imm_2 ; not imm_3
        sta RISCV_ireg_3,y

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_ORI

    get_imm12
    get_rs1_x
    get_rd_y
    beq @end

        lda RISCV_ireg_0,x
        ora imm_0
        sta RISCV_ireg_0,y
        lda RISCV_ireg_1,x
        ora imm_1
        sta RISCV_ireg_1,y
        lda RISCV_ireg_2,x
        ora imm_2
        sta RISCV_ireg_2,y
        lda RISCV_ireg_3,x
        ora imm_2 ; not imm_3
        sta RISCV_ireg_3,y

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_ANDI

    get_imm12
    get_rs1_x
    get_rd_y
    beq @end

        lda RISCV_ireg_0,x
        and imm_0
        sta RISCV_ireg_0,y
        lda RISCV_ireg_1,x
        and imm_1
        sta RISCV_ireg_1,y
        lda RISCV_ireg_2,x
        and imm_2
        sta RISCV_ireg_2,y
        lda RISCV_ireg_3,x
        and imm_2 ; not imm_3
        sta RISCV_ireg_3,y

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SLLI

    ; funct7 must be zero
    ; RV64I: funct7 reduced to 6 bits to make the shift distance 6 bits
    lda RISCV_opcode+3
    lsr a
    bne bad_opcode

    get_rs1_x
    lda RISCV_ireg_0,x
    sta imm_0
    lda RISCV_ireg_1,x
    sta imm_1
    lda RISCV_ireg_2,x
    sta imm_2
    lda RISCV_ireg_3,x
    sta imm_3

    get_rs2_a ; actually shift distance
    sta pointer1+0

    jsr_far shift_left_bank,shift_left_entry

    get_rd_x
    beq @end
        lda imm_0
        sta RISCV_ireg_0,x
        lda imm_1
        sta RISCV_ireg_1,x
        lda imm_2
        sta RISCV_ireg_2,x
        lda imm_3
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

bad_opcode:
    jmp invalid_opcode

.endproc

.proc do_SRLI_SRAI

    get_rs1_x
    lda RISCV_ireg_0,x
    sta imm_0
    lda RISCV_ireg_1,x
    sta imm_1
    lda RISCV_ireg_2,x
    sta imm_2
    lda RISCV_ireg_3,x
    sta imm_3

    ; funct7 determines whether we shift signed or unsigned
    ; RV64I: funct7 reduced to 6 bits to make the shift distance 6 bits
    get_rs2_a ; actually shift distance
    sta pointer1+0

    lda RISCV_opcode+3
    lsr a
    beq @do_unsigned
    cmp #$20
    beq @do_signed
    jmp invalid_opcode

    @do_unsigned:
        jsr_far shift_right_unsigned_bank,shift_right_unsigned_entry
        jmp @store
    @do_signed:
        jsr_far shift_right_signed_bank,shift_right_signed_entry

    @store:
    get_rd_x
    beq @end
        lda imm_0
        sta RISCV_ireg_0,x
        lda imm_1
        sta RISCV_ireg_1,x
        lda imm_2
        sta RISCV_ireg_2,x
        lda imm_3
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_AUIPC
    get_rd_x
    beq end_AUIPC
        lda RISCV_pc+0
        sta RISCV_ireg_0,x
        lda RISCV_opcode+1
        and #$F0
        clc
        adc RISCV_pc+1
        sta RISCV_ireg_1,x
        lda RISCV_opcode+2
        adc RISCV_pc+2
        sta RISCV_ireg_2,x
        lda RISCV_opcode+3
        adc RISCV_pc+3
        sta RISCV_ireg_3,x
    end_AUIPC:
    advance_pc 4
    jmp RISCV_instruction
.endproc

.proc dispatch_STORE

    lda RISCV_opcode+1
    and #$70
    ora #table_STORE
    tax
    lda dispatch_funct3+0,x
    sta pointer1+0
    lda dispatch_funct3+1,x
    sta pointer1+1

    get_imm_S
    get_rs1_x

    clc
    lda RISCV_ireg_0,x
    adc imm_0
    sta RISCV_address+0
    lda RISCV_ireg_1,x
    adc imm_1
    sta RISCV_address+1
    lda RISCV_ireg_2,x
    adc imm_2
    sta RISCV_address+2
    lda RISCV_ireg_3,x
    adc imm_2 ; not imm_3
    sta RISCV_address+3

    get_rs2_a
    tay
    lda RISCV_ireg_0,y
    sta RISCV_data+0

    jmp (pointer1)

.endproc

.proc do_SB

    jsr RISCV_write_8

    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SH

    lda RISCV_ireg_1,y
    sta RISCV_data+1

    jsr RISCV_write_16

    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SW

    lda RISCV_ireg_1,y
    sta RISCV_data+1
    lda RISCV_ireg_2,y
    sta RISCV_data+2
    lda RISCV_ireg_3,y
    sta RISCV_data+3

    jsr RISCV_write_32

    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc dispatch_OP

    ; Select according to funct3 and funct7

    lda RISCV_opcode+3
    lsr a               ; funct7
    tax
    lda RISCV_opcode+1
    and #$70            ; funct3
    ora OP_table,x
    tax
    lda dispatch_funct3+0,x
    sta pointer1+0
    lda dispatch_funct3+1,x
    sta pointer1+1
    get_rs1_rs2
    jmp (pointer1)

.endproc

.segment "ROPAGE"
.align 256

; Table selector used by dispatch_OP
OP_table:
    .byte table_OP_00
    .byte table_OP_01
    .repeat $1E
        .byte table_INVALID
    .endrep
    .byte table_OP_20
    .repeat $5F
        .byte table_INVALID
    .endrep

.code

.proc do_ADD

    clc
    lda RISCV_ireg_0,x
    adc RISCV_ireg_0,y
    sta imm_0
    lda RISCV_ireg_1,x
    adc RISCV_ireg_1,y
    sta imm_1
    lda RISCV_ireg_2,x
    adc RISCV_ireg_2,y
    sta imm_2
    lda RISCV_ireg_3,x
    adc RISCV_ireg_3,y
    sta imm_3

    get_rd_x
    beq @end
        lda imm_0
        sta RISCV_ireg_0,x
        lda imm_1
        sta RISCV_ireg_1,x
        lda imm_2
        sta RISCV_ireg_2,x
        lda imm_3
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SUB

    sec
    lda RISCV_ireg_0,x
    sbc RISCV_ireg_0,y
    sta imm_0
    lda RISCV_ireg_1,x
    sbc RISCV_ireg_1,y
    sta imm_1
    lda RISCV_ireg_2,x
    sbc RISCV_ireg_2,y
    sta imm_2
    lda RISCV_ireg_3,x
    sbc RISCV_ireg_3,y
    sta imm_3

    get_rd_x
    beq @end
        lda imm_0
        sta RISCV_ireg_0,x
        lda imm_1
        sta RISCV_ireg_1,x
        lda imm_2
        sta RISCV_ireg_2,x
        lda imm_3
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SLL

    lda RISCV_ireg_0,x
    sta imm_0
    lda RISCV_ireg_1,x
    sta imm_1
    lda RISCV_ireg_2,x
    sta imm_2
    lda RISCV_ireg_3,x
    sta imm_3

    lda RISCV_ireg_0,y
    and #$1F
    sta pointer1+0

    jsr_far shift_left_bank,shift_left_entry

    get_rd_x
    beq @end
        lda imm_0
        sta RISCV_ireg_0,x
        lda imm_1
        sta RISCV_ireg_1,x
        lda imm_2
        sta RISCV_ireg_2,x
        lda imm_3
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SLT

    lda RISCV_ireg_0,x
    cmp RISCV_ireg_0,y
    lda RISCV_ireg_1,x
    sbc RISCV_ireg_1,y
    lda RISCV_ireg_2,x
    sbc RISCV_ireg_2,y
    lda RISCV_ireg_3,x
    sbc RISCV_ireg_3,y
    bvc @no_overflow
        eor #$80
    @no_overflow:

    rol a
    lda #0
    rol a
    tay

    get_rd_x
    beq @end
        tya
        sta RISCV_ireg_0,x
        lda #0
        sta RISCV_ireg_1,x
        sta RISCV_ireg_2,x
        sta RISCV_ireg_3,x
    @end:

    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SLTU

    lda RISCV_ireg_0,x
    cmp RISCV_ireg_0,y
    lda RISCV_ireg_1,x
    sbc RISCV_ireg_1,y
    lda RISCV_ireg_2,x
    sbc RISCV_ireg_2,y
    lda RISCV_ireg_3,x
    sbc RISCV_ireg_3,y

    lda #0
    rol a
    eor #1
    tay

    get_rd_x
    beq @end
        tya
        sta RISCV_ireg_0,x
        lda #0
        sta RISCV_ireg_1,x
        sta RISCV_ireg_2,x
        sta RISCV_ireg_3,x
    @end:

    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_XOR

    lda RISCV_ireg_0,x
    eor RISCV_ireg_0,y
    sta imm_0
    lda RISCV_ireg_1,x
    eor RISCV_ireg_1,y
    sta imm_1
    lda RISCV_ireg_2,x
    eor RISCV_ireg_2,y
    sta imm_2
    lda RISCV_ireg_3,x
    eor RISCV_ireg_3,y
    sta imm_3

    get_rd_x
    beq @end
        lda imm_0
        sta RISCV_ireg_0,x
        lda imm_1
        sta RISCV_ireg_1,x
        lda imm_2
        sta RISCV_ireg_2,x
        lda imm_3
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SRL

    lda RISCV_ireg_0,x
    sta imm_0
    lda RISCV_ireg_1,x
    sta imm_1
    lda RISCV_ireg_2,x
    sta imm_2
    lda RISCV_ireg_3,x
    sta imm_3

    lda RISCV_ireg_0,y
    and #$1F
    sta pointer1+0

    jsr_far shift_right_unsigned_bank,shift_right_unsigned_entry

    get_rd_x
    beq @end
        lda imm_0
        sta RISCV_ireg_0,x
        lda imm_1
        sta RISCV_ireg_1,x
        lda imm_2
        sta RISCV_ireg_2,x
        lda imm_3
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SRA

    lda RISCV_ireg_0,x
    sta imm_0
    lda RISCV_ireg_1,x
    sta imm_1
    lda RISCV_ireg_2,x
    sta imm_2
    lda RISCV_ireg_3,x
    sta imm_3

    lda RISCV_ireg_0,y
    and #$1F
    sta pointer1+0

    jsr_far shift_right_signed_bank,shift_right_signed_entry

    get_rd_x
    beq @end
        lda imm_0
        sta RISCV_ireg_0,x
        lda imm_1
        sta RISCV_ireg_1,x
        lda imm_2
        sta RISCV_ireg_2,x
        lda imm_3
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_OR

    lda RISCV_ireg_0,x
    ora RISCV_ireg_0,y
    sta imm_0
    lda RISCV_ireg_1,x
    ora RISCV_ireg_1,y
    sta imm_1
    lda RISCV_ireg_2,x
    ora RISCV_ireg_2,y
    sta imm_2
    lda RISCV_ireg_3,x
    ora RISCV_ireg_3,y
    sta imm_3

    get_rd_x
    beq @end
        lda imm_0
        sta RISCV_ireg_0,x
        lda imm_1
        sta RISCV_ireg_1,x
        lda imm_2
        sta RISCV_ireg_2,x
        lda imm_3
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_AND

    lda RISCV_ireg_0,x
    and RISCV_ireg_0,y
    sta imm_0
    lda RISCV_ireg_1,x
    and RISCV_ireg_1,y
    sta imm_1
    lda RISCV_ireg_2,x
    and RISCV_ireg_2,y
    sta imm_2
    lda RISCV_ireg_3,x
    and RISCV_ireg_3,y
    sta imm_3

    get_rd_x
    beq @end
        lda imm_0
        sta RISCV_ireg_0,x
        lda imm_1
        sta RISCV_ireg_1,x
        lda imm_2
        sta RISCV_ireg_2,x
        lda imm_3
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_MUL

    lda RISCV_ireg_0,x
    sta mul_op1_0
    lda RISCV_ireg_1,x
    sta mul_op1_1
    lda RISCV_ireg_2,x
    sta mul_op1_2
    lda RISCV_ireg_3,x
    sta mul_op1_3

    lda RISCV_ireg_0,y
    sta mul_op2_0
    lda RISCV_ireg_1,y
    sta mul_op2_1
    lda RISCV_ireg_2,y
    sta mul_op2_2
    lda RISCV_ireg_3,y
    sta mul_op2_3

    jsr_far multiply_low_bank,multiply_low_entry

    get_rd_x
    beq @end

        lda mul_prod_0
        sta RISCV_ireg_0,x
        lda mul_prod_1
        sta RISCV_ireg_1,x
        lda mul_prod_2
        sta RISCV_ireg_2,x
        lda mul_prod_3
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_MULH

    stx rs1_reg
    sty rs2_reg

    lda RISCV_ireg_0,x
    sta mul_op1_0
    lda RISCV_ireg_1,x
    sta mul_op1_1
    lda RISCV_ireg_2,x
    sta mul_op1_2
    lda RISCV_ireg_3,x
    sta mul_op1_3

    lda RISCV_ireg_0,y
    sta mul_op2_0
    lda RISCV_ireg_1,y
    sta mul_op2_1
    lda RISCV_ireg_2,y
    sta mul_op2_2
    lda RISCV_ireg_3,y
    sta mul_op2_3

    jsr_far multiply_signed_bank,multiply_signed_entry

    get_rd_x
    stx rd_reg
    beq @end

        lda mul_prod_4
        sta RISCV_ireg_0,x
        lda mul_prod_5
        sta RISCV_ireg_1,x
        lda mul_prod_6
        sta RISCV_ireg_2,x
        lda mul_prod_7
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp end_MULH

.endproc

.proc do_MULHSU

    stx rs1_reg
    sty rs2_reg

    lda RISCV_ireg_0,x
    sta mul_op1_0
    lda RISCV_ireg_1,x
    sta mul_op1_1
    lda RISCV_ireg_2,x
    sta mul_op1_2
    lda RISCV_ireg_3,x
    sta mul_op1_3

    lda RISCV_ireg_0,y
    sta mul_op2_0
    lda RISCV_ireg_1,y
    sta mul_op2_1
    lda RISCV_ireg_2,y
    sta mul_op2_2
    lda RISCV_ireg_3,y
    sta mul_op2_3

    jsr_far multiply_signed_unsigned_bank,multiply_signed_unsigned_entry

    get_rd_x
    stx rd_reg
    beq @end

        lda mul_prod_4
        sta RISCV_ireg_0,x
        lda mul_prod_5
        sta RISCV_ireg_1,x
        lda mul_prod_6
        sta RISCV_ireg_2,x
        lda mul_prod_7
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp end_MULH

.endproc

.proc do_MULHU

    stx rs1_reg
    sty rs2_reg

    lda RISCV_ireg_0,x
    sta mul_op1_0
    lda RISCV_ireg_1,x
    sta mul_op1_1
    lda RISCV_ireg_2,x
    sta mul_op1_2
    lda RISCV_ireg_3,x
    sta mul_op1_3

    lda RISCV_ireg_0,y
    sta mul_op2_0
    lda RISCV_ireg_1,y
    sta mul_op2_1
    lda RISCV_ireg_2,y
    sta mul_op2_2
    lda RISCV_ireg_3,y
    sta mul_op2_3

    jsr_far multiply_unsigned_bank,multiply_unsigned_entry

    get_rd_x
    stx rd_reg
    beq @end

        lda mul_prod_4
        sta RISCV_ireg_0,x
        lda mul_prod_5
        sta RISCV_ireg_1,x
        lda mul_prod_6
        sta RISCV_ireg_2,x
        lda mul_prod_7
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp end_MULH

.endproc

; Process the end of a MULH, MULHSU or MULHU instruction
; Check the next instruction and, if it is a matching MUL, fill the register
; with the just-computed low word
.proc end_MULH
    ; If rd is the same as rs1 or rs2, we can't fuse the instruction; the
    ; just-completed instruction has clobbered its input
    lda rd_reg
    cmp rs1_reg
    beq end_instruction
    cmp rs2_reg
    bne :+
    end_instruction:
        jmp RISCV_instruction
    :

    ; Save the old opcode, modify funct3 to match the expected MUL
    ; and mask off rd
    ; The least significant byte will be $33 after masking off rd
    lda RISCV_opcode+1
    and #$80
    sta old_opcode+0
    lda RISCV_opcode+2
    sta old_opcode+1
    lda RISCV_opcode+3
    sta old_opcode+2

    ; Fetch the next opcode
    RISCV_fetch

    ; If the opcode matches the expected MUL, use the computed low word
    lda RISCV_opcode+0
    and #$7F
    cmp #$33
    bne do_instruction
    lda RISCV_opcode+1
    and #$F0
    cmp old_opcode+0
    bne do_instruction
    lda RISCV_opcode+2
    cmp old_opcode+1
    bne do_instruction
    lda RISCV_opcode+3
    cmp old_opcode+2
    bne do_instruction

    ; The opcode matches the expected MUL
    get_rd_x
    beq end_low_word
        lda mul_prod_0
        sta RISCV_ireg_0,x
        lda mul_prod_1
        sta RISCV_ireg_1,x
        lda mul_prod_2
        sta RISCV_ireg_2,x
        lda mul_prod_3
        sta RISCV_ireg_3,x
    end_low_word:
    advance_pc 4

    jmp RISCV_instruction

    ; Run the instruction that we just fetched
do_instruction:
    jmp dispatch_instruction

.endproc

.proc do_DIV

    stx rs1_reg
    sty rs2_reg

    lda RISCV_ireg_0,x
    sta div_op1_0
    lda RISCV_ireg_1,x
    sta div_op1_1
    lda RISCV_ireg_2,x
    sta div_op1_2
    lda RISCV_ireg_3,x
    sta div_op1_3

    lda RISCV_ireg_0,y
    sta div_op2_0
    lda RISCV_ireg_1,y
    sta div_op2_1
    lda RISCV_ireg_2,y
    sta div_op2_2
    lda RISCV_ireg_3,y
    sta div_op2_3

    jsr_far divide_signed_bank,divide_signed_entry

    get_rd_x
    stx rd_reg
    beq @end

        lda div_quo_0
        sta RISCV_ireg_0,x
        lda div_quo_1
        sta RISCV_ireg_1,x
        lda div_quo_2
        sta RISCV_ireg_2,x
        lda div_quo_3
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp end_DIV_DIVU

.endproc

.proc do_DIVU

    stx rs1_reg
    sty rs2_reg

    lda RISCV_ireg_0,x
    sta div_op1_0
    lda RISCV_ireg_1,x
    sta div_op1_1
    lda RISCV_ireg_2,x
    sta div_op1_2
    lda RISCV_ireg_3,x
    sta div_op1_3

    lda RISCV_ireg_0,y
    sta div_op2_0
    lda RISCV_ireg_1,y
    sta div_op2_1
    lda RISCV_ireg_2,y
    sta div_op2_2
    lda RISCV_ireg_3,y
    sta div_op2_3

    jsr_far divide_unsigned_bank,divide_unsigned_entry

    get_rd_x
    stx rd_reg
    beq @end

        lda div_quo_0
        sta RISCV_ireg_0,x
        lda div_quo_1
        sta RISCV_ireg_1,x
        lda div_quo_2
        sta RISCV_ireg_2,x
        lda div_quo_3
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp end_DIV_DIVU

.endproc

; Process the end of a DIV or DIVU instruction
; Check the next instruction and, if it is a matching REM or REMU, fill the
; register with the just-computed remainder
.proc end_DIV_DIVU
    ; If rd is the same as rs1 or rs2, we can't fuse the instruction; the
    ; just-completed instruction has clobbered its input
    lda rd_reg
    cmp rs1_reg
    beq end_instruction
    cmp rs2_reg
    bne :+
    end_instruction:
        jmp RISCV_instruction
    :

    ; Save the old opcode, modify funct3 to match the expected REM or REMU,
    ; and mask off rd
    ; The least significant byte will be $33 after masking off rd
    lda RISCV_opcode+1
    and #$F0
    ora #$20
    sta old_opcode+0
    lda RISCV_opcode+2
    sta old_opcode+1
    lda RISCV_opcode+3
    sta old_opcode+2

    ; Fetch the next opcode
    RISCV_fetch

    ; If the opcode matches the expected REM or REMU, use the computed remainder
    lda RISCV_opcode+0
    and #$7F
    cmp #$33
    bne do_instruction
    lda RISCV_opcode+1
    and #$F0
    cmp old_opcode+0
    bne do_instruction
    lda RISCV_opcode+2
    cmp old_opcode+1
    bne do_instruction
    lda RISCV_opcode+3
    cmp old_opcode+2
    bne do_instruction

    ; The opcode matches the expected REM or REMU
    get_rd_x
    beq end_remainder
        lda div_rem_0
        sta RISCV_ireg_0,x
        lda div_rem_1
        sta RISCV_ireg_1,x
        lda div_rem_2
        sta RISCV_ireg_2,x
        lda div_rem_3
        sta RISCV_ireg_3,x
    end_remainder:
    advance_pc 4

    jmp RISCV_instruction

    ; Run the instruction that we just fetched
do_instruction:
    jmp dispatch_instruction

.endproc

.proc do_REM

    lda RISCV_ireg_0,x
    sta div_op1_0
    lda RISCV_ireg_1,x
    sta div_op1_1
    lda RISCV_ireg_2,x
    sta div_op1_2
    lda RISCV_ireg_3,x
    sta div_op1_3

    lda RISCV_ireg_0,y
    sta div_op2_0
    lda RISCV_ireg_1,y
    sta div_op2_1
    lda RISCV_ireg_2,y
    sta div_op2_2
    lda RISCV_ireg_3,y
    sta div_op2_3

    jsr_far divide_signed_bank,divide_signed_entry

    get_rd_x
    beq @end

        lda div_rem_0
        sta RISCV_ireg_0,x
        lda div_rem_1
        sta RISCV_ireg_1,x
        lda div_rem_2
        sta RISCV_ireg_2,x
        lda div_rem_3
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_REMU

    lda RISCV_ireg_0,x
    sta div_op1_0
    lda RISCV_ireg_1,x
    sta div_op1_1
    lda RISCV_ireg_2,x
    sta div_op1_2
    lda RISCV_ireg_3,x
    sta div_op1_3

    lda RISCV_ireg_0,y
    sta div_op2_0
    lda RISCV_ireg_1,y
    sta div_op2_1
    lda RISCV_ireg_2,y
    sta div_op2_2
    lda RISCV_ireg_3,y
    sta div_op2_3

    jsr_far divide_unsigned_bank,divide_unsigned_entry

    get_rd_x
    beq @end

        lda div_rem_0
        sta RISCV_ireg_0,x
        lda div_rem_1
        sta RISCV_ireg_1,x
        lda div_rem_2
        sta RISCV_ireg_2,x
        lda div_rem_3
        sta RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_LUI
    get_rd_x
    beq end_LUI
        lda #0
        sta RISCV_ireg_0,x
        lda RISCV_opcode+1
        and #$F0
        sta RISCV_ireg_1,x
        lda RISCV_opcode+2
        sta RISCV_ireg_2,x
        lda RISCV_opcode+3
        sta RISCV_ireg_3,x
    end_LUI:
    advance_pc 4
    jmp RISCV_instruction
.endproc

.proc dispatch_BRANCH

    lda RISCV_opcode+1
    and #$70
    ora #table_BRANCH
    tax
    lda dispatch_funct3+0,x
    sta pointer1+0
    lda dispatch_funct3+1,x
    sta pointer1+1
    get_rs1_rs2
    jmp (pointer1)

.endproc

.proc do_BGEU

    lda RISCV_ireg_0,x ; rs1
    cmp RISCV_ireg_0,y ; rs2
    lda RISCV_ireg_1,x
    sbc RISCV_ireg_1,y
    lda RISCV_ireg_2,x
    sbc RISCV_ireg_2,y
    lda RISCV_ireg_3,x
    sbc RISCV_ireg_3,y
    bcs take_branch

skip_branch:
    advance_pc 4
    jmp RISCV_instruction

take_branch:
    do_branch
    jmp RISCV_instruction

.endproc

.proc do_BLTU

    lda RISCV_ireg_0,x ; rs1
    cmp RISCV_ireg_0,y ; rs2
    lda RISCV_ireg_1,x
    sbc RISCV_ireg_1,y
    lda RISCV_ireg_2,x
    sbc RISCV_ireg_2,y
    lda RISCV_ireg_3,x
    sbc RISCV_ireg_3,y
    bcc take_branch

skip_branch:
    advance_pc 4
    jmp RISCV_instruction

take_branch:
    do_branch
    jmp RISCV_instruction

.endproc

.proc do_BGE

    lda RISCV_ireg_0,x ; rs1
    cmp RISCV_ireg_0,y ; rs2
    lda RISCV_ireg_1,x
    sbc RISCV_ireg_1,y
    lda RISCV_ireg_2,x
    sbc RISCV_ireg_2,y
    lda RISCV_ireg_3,x
    sbc RISCV_ireg_3,y
    bpl positive

    bvs take_branch
skip_branch:
    advance_pc 4
    jmp RISCV_instruction

positive:
    bvs skip_branch
take_branch:
    do_branch
    jmp RISCV_instruction

.endproc

.proc do_BLT

    lda RISCV_ireg_0,x ; rs1
    cmp RISCV_ireg_0,y ; rs2
    lda RISCV_ireg_1,x
    sbc RISCV_ireg_1,y
    lda RISCV_ireg_2,x
    sbc RISCV_ireg_2,y
    lda RISCV_ireg_3,x
    sbc RISCV_ireg_3,y
    bmi negative

    bvs take_branch
skip_branch:
    advance_pc 4
    jmp RISCV_instruction

negative:
    bvs skip_branch
take_branch:
    do_branch
    jmp RISCV_instruction

.endproc

.proc do_BEQ

    lda RISCV_ireg_0,x ; rs1
    cmp RISCV_ireg_0,y ; rs2
    bne skip_branch
    lda RISCV_ireg_1,x
    cmp RISCV_ireg_1,y
    bne skip_branch
    lda RISCV_ireg_2,x
    cmp RISCV_ireg_2,y
    bne skip_branch
    lda RISCV_ireg_3,x
    cmp RISCV_ireg_3,y
    beq take_branch

skip_branch:
    advance_pc 4
    jmp RISCV_instruction

take_branch:
    do_branch
    jmp RISCV_instruction

.endproc

.proc do_BNE

    lda RISCV_ireg_0,x ; rs1
    cmp RISCV_ireg_0,y ; rs2
    bne take_branch
    lda RISCV_ireg_1,x
    cmp RISCV_ireg_1,y
    bne take_branch
    lda RISCV_ireg_2,x
    cmp RISCV_ireg_2,y
    bne take_branch
    lda RISCV_ireg_3,x
    cmp RISCV_ireg_3,y
    bne take_branch

skip_branch:
    advance_pc 4
    jmp RISCV_instruction

take_branch:
    do_branch
    jmp RISCV_instruction

.endproc

; Located here for a short BNE
bad_JALR:
    jmp invalid_opcode

.proc do_JALR

    ; funct3 must be 000
    lda RISCV_opcode+1
    and #$70
    bne bad_JALR

    ; Need to calculate the jump target first, then save the return address,
    ; in case of source and destination registers the same

    get_imm12
    get_rs1_x
    clc
    lda imm_0
    adc RISCV_ireg_0,x
    sta imm_0
    lda imm_1
    adc RISCV_ireg_1,x
    sta imm_1
    lda imm_2
    adc RISCV_ireg_2,x
    sta imm_3
    lda imm_2 ; not imm_3
    adc RISCV_ireg_3,x
    sta imm_2
    ; Role of imm_2 and imm_3 is reversed because imm_2 was needed at the end

    ; Save the return address (but not to x0)
    get_rd_x
    beq take_JALR
        clc
        lda RISCV_pc+0
        adc #4
        sta RISCV_ireg_0,x
        lda RISCV_pc+1
        adc #0
        sta RISCV_ireg_1,x
        lda RISCV_pc+2
        adc #0
        sta RISCV_ireg_2,x
        lda RISCV_pc+3
        adc #0
        sta RISCV_ireg_3,x
    take_JALR:

    ; Jump
    lda imm_0
    and #$FE
    sta RISCV_pc+0
    lda imm_1
    sta RISCV_pc+1
    lda imm_3   ; not imm_2
    sta RISCV_pc+2
    lda imm_2   ; not imm_3
    sta RISCV_pc+3
    inc RISCV_pc_check

    jmp RISCV_instruction

.endproc

.proc do_JAL
    get_rd_x
    beq take_JAL
        clc
        lda RISCV_pc+0
        adc #4
        sta RISCV_ireg_0,x
        lda RISCV_pc+1
        adc #0
        sta RISCV_ireg_1,x
        lda RISCV_pc+2
        adc #0
        sta RISCV_ireg_2,x
        lda RISCV_pc+3
        adc #0
        sta RISCV_ireg_3,x
    take_JAL:

    ; Bits of immediate value are scrambled
    ldx RISCV_opcode+2   ; (3) X = b03 b02 b01 b11 b19 b18 b17 b16
    ldy RISCV_opcode+3   ; (3) Y = b20 b10 b09 b08 b07 b06 b05 b04

    lda rshift4,x        ; (4) A =   0   0   0   0 b03 b02 b01 b11
    ora lshift4,y        ; (4) A = b07 b06 b05 b04 b03 b02 b01 b11
    and #$FE             ; (2) A = b07 b06 b05 b04 b03 b02 b01   0
    clc                  ; (2)
    adc RISCV_pc+0       ; (3)
    sta RISCV_pc+0       ; (3)

    lda RISCV_opcode+1   ; (3) A = b15 b14 b13 b12 xxx xxx xxx xxx
    and #$F0             ; (2) A = b15 b14 b13 b12   0   0   0   0
    ora jump_lookup_21,x ; (4) A = b15 b14 b13 b12 b11   0   0   0
    ora jump_lookup_31,y ; (4) A = b15 b14 b13 b12 b11 b10 b09 b08
    adc RISCV_pc+1       ; (3)
    sta RISCV_pc+1       ; (3)

    txa                  ; (2) A = b03 b02 b01 b11 b19 b18 b17 b16
    and #$0F             ; (2) A =   0   0   0   0 b19 b18 b17 b16
    ora jump_lookup_32,y ; (4) A = b20 b20 b20 b20 b19 b18 b17 b16
    tay                  ; (2) Y = b20 b20 b20 b20 b19 b18 b17 b16
    adc RISCV_pc+2       ; (3)
    sta RISCV_pc+2       ; (3)

    lda rsshift4,y       ; (4) A = b20 b20 b20 b20 b20 b20 b20 b20
    adc RISCV_pc+3       ; (3)
    sta RISCV_pc+3       ; (3)
    inc RISCV_pc_check   ; (5)
                         ; total: 74

    jmp RISCV_instruction
.endproc

.segment "ROPAGE"
.align 256

; JAL uses these tables to unshuffle the offset

; Opcode byte 2 to offset byte 1:
; xxx xxx xxx b11 xxx xxx xxx xxx =>   0   0   0   0 b11   0   0   0
jump_lookup_21:
.repeat 8
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08
.endrep

; Opcode byte 3 to offset byte 1:
; xxx b10 b09 b08 xxx xxx xxx xxx =>   0   0   0   0   0 b10 b09 b08
jump_lookup_31:
.repeat 2
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
    .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
    .byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
    .byte $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
    .byte $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
    .byte $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
    .byte $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.endrep

; Opcode byte 3 to offset byte 2:
; b20 xxx xxx xxx xxx xxx xxx xxx => b20 b20 b20 b20   0   0   0   0
jump_lookup_32:
.repeat 128
    .byte $00
.endrep
.repeat 128
    .byte $F0
.endrep

.code

.proc do_ECALL_EBREAK

    ; Zicsr extension uses this path
    ; Check for unimplemented opcodes
    lda RISCV_opcode+1
    ora RISCV_opcode+3
    bne bad_opcode
    lda RISCV_opcode+0
    cmp #$73
    bne bad_opcode

    lda RISCV_opcode+2
    bne check_EBREAK
    ; ECALL
    jsr_far RISCV_ECALL_bank,RISCV_ECALL_entry
    advance_pc 4
    jmp RISCV_instruction

check_EBREAK:
    cmp #$10
    bne bad_opcode
    ; EBREAK
    jsr_far RISCV_EBREAK_bank,RISCV_EBREAK_entry
    advance_pc 4
    jmp RISCV_instruction

bad_opcode:
    jmp invalid_opcode

.endproc

custom_0B = invalid_opcode
custom_2B = invalid_opcode
custom_5B = invalid_opcode
custom_7B = invalid_opcode

.proc invalid_opcode
    jmp RISCV_invalid_opcode
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Various 256 byte lookup tables.
; Tables are page aligned so that indexing does not add a cycle.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "ROPAGE"
.align 256

; Interleaved dispatch tables for opcodes dispatched via funct3
; funct3 appears in RISCV_opcode+1 bits 6-4. The rest is masked off via
; and #$70, and then an ora #$xx includes a constant to select the table.
; The constant may use bits 7, 3, 2 and 1, for sixteen possible tables; the
; RV32IM instruction set populates seven tables.  Bit 0 must be zero.
; Because ADDI is so frequently used, its table is given the constant zero.
;
; Selector $8E (table_INVALID) points only to invalid_opcode. This is used
; by dispatch_OP when funct7 does not match anything valid.

table_OP_IMM = $00
table_LOAD   = $02
table_STORE  = $04
table_OP_00  = $06
table_OP_01  = $08
table_OP_20  = $0a
table_BRANCH = $0c
;table_xxx = $0E
;table_xxx = $80
;table_xxx = $82
;table_xxx = $84
;table_xxx = $86
;table_xxx = $88
;table_xxx = $8A
;table_xxx = $8C
table_INVALID = $8E

dispatch_funct3:
    .word do_ADDI           ; funct3 = 0, selector = $00 (table_OP_IMM)
    .word do_LB             ; funct3 = 0, selector = $02 (table_LOAD)
    .word do_SB             ; funct3 = 0, selector = $04 (table_STORE)
    .word do_ADD            ; funct3 = 0, selector = $06 (table_OP_00)
    .word do_MUL            ; funct3 = 0, selector = $08 (table_OP_01)
    .word do_SUB            ; funct3 = 0, selector = $0A (table_OP_20)
    .word do_BEQ            ; funct3 = 0, selector = $0C (table_BRANCH)
    .word invalid_opcode    ; funct3 = 0, selector = $0E
    .word do_SLLI           ; funct3 = 1, selector = $00 (table_OP_IMM)
    .word do_LH             ; funct3 = 1, selector = $02 (table_LOAD)
    .word do_SH             ; funct3 = 1, selector = $04 (table_STORE)
    .word do_SLL            ; funct3 = 1, selector = $06 (table_OP_00)
    .word do_MULH           ; funct3 = 1, selector = $08 (table_OP_01)
    .word invalid_opcode    ; funct3 = 1, selector = $0A (table_OP_20)
    .word do_BNE            ; funct3 = 1, selector = $0C (table_BRANCH)
    .word invalid_opcode    ; funct3 = 1, selector = $0E
    .word do_SLTI           ; funct3 = 2, selector = $00 (table_OP_IMM)
    .word do_LW             ; funct3 = 2, selector = $02 (table_LOAD)
    .word do_SW             ; funct3 = 2, selector = $04 (table_STORE)
    .word do_SLT            ; funct3 = 2, selector = $06 (table_OP_00)
    .word do_MULHSU         ; funct3 = 2, selector = $08 (table_OP_01)
    .word invalid_opcode    ; funct3 = 2, selector = $0A (table_OP_20)
    .word invalid_opcode    ; funct3 = 2, selector = $0C (table_BRANCH)
    .word invalid_opcode    ; funct3 = 2, selector = $0E
    .word do_SLTIU          ; funct3 = 3, selector = $00 (table_OP_IMM)
    .word invalid_opcode    ; funct3 = 3, selector = $02 (table_LOAD)
    .word invalid_opcode    ; funct3 = 3, selector = $04 (table_STORE)
    .word do_SLTU           ; funct3 = 3, selector = $06 (table_OP_00)
    .word do_MULHU          ; funct3 = 3, selector = $08 (table_OP_01)
    .word invalid_opcode    ; funct3 = 3, selector = $0A (table_OP_20)
    .word invalid_opcode    ; funct3 = 3, selector = $0C (table_BRANCH)
    .word invalid_opcode    ; funct3 = 3, selector = $0E
    .word do_XORI           ; funct3 = 4, selector = $00 (table_OP_IMM)
    .word do_LBU            ; funct3 = 4, selector = $02 (table_LOAD)
    .word invalid_opcode    ; funct3 = 4, selector = $04 (table_STORE)
    .word do_XOR            ; funct3 = 4, selector = $06 (table_OP_00)
    .word do_DIV            ; funct3 = 4, selector = $08 (table_OP_01)
    .word invalid_opcode    ; funct3 = 4, selector = $0A (table_OP_20)
    .word do_BLT            ; funct3 = 4, selector = $0C (table_BRANCH)
    .word invalid_opcode    ; funct3 = 4, selector = $0E
    .word do_SRLI_SRAI      ; funct3 = 5, selector = $00 (table_OP_IMM)
    .word do_LHU            ; funct3 = 5, selector = $02 (table_LOAD)
    .word invalid_opcode    ; funct3 = 5, selector = $04 (table_STORE)
    .word do_SRL            ; funct3 = 5, selector = $06 (table_OP_00)
    .word do_DIVU           ; funct3 = 5, selector = $08 (table_OP_01)
    .word do_SRA            ; funct3 = 5, selector = $0A (table_OP_20)
    .word do_BGE            ; funct3 = 5, selector = $0C (table_BRANCH)
    .word invalid_opcode    ; funct3 = 5, selector = $0E
    .word do_ORI            ; funct3 = 6, selector = $00 (table_OP_IMM)
    .word invalid_opcode    ; funct3 = 6, selector = $02 (table_LOAD)
    .word invalid_opcode    ; funct3 = 6, selector = $04 (table_STORE)
    .word do_OR             ; funct3 = 6, selector = $06 (table_OP_00)
    .word do_REM            ; funct3 = 6, selector = $08 (table_OP_01)
    .word invalid_opcode    ; funct3 = 6, selector = $0A (table_OP_20)
    .word do_BLTU           ; funct3 = 6, selector = $0C (table_BRANCH)
    .word invalid_opcode    ; funct3 = 6, selector = $0E
    .word do_ANDI           ; funct3 = 7, selector = $00 (table_OP_IMM)
    .word invalid_opcode    ; funct3 = 7, selector = $02 (table_LOAD)
    .word invalid_opcode    ; funct3 = 7, selector = $04 (table_STORE)
    .word do_AND            ; funct3 = 7, selector = $06 (table_OP_00)
    .word do_REMU           ; funct3 = 7, selector = $08 (table_OP_01)
    .word invalid_opcode    ; funct3 = 7, selector = $0A (table_OP_20)
    .word do_BGEU           ; funct3 = 7, selector = $0C (table_BRANCH)
    .word invalid_opcode    ; funct3 = 7, selector = $0E
    .word invalid_opcode    ; funct3 = 0, selector = $80
    .word invalid_opcode    ; funct3 = 0, selector = $82
    .word invalid_opcode    ; funct3 = 0, selector = $84
    .word invalid_opcode    ; funct3 = 0, selector = $86
    .word invalid_opcode    ; funct3 = 0, selector = $88
    .word invalid_opcode    ; funct3 = 0, selector = $8A
    .word invalid_opcode    ; funct3 = 0, selector = $8C
    .word invalid_opcode    ; funct3 = 0, selector = $8E (table_INVALID)
    .word invalid_opcode    ; funct3 = 1, selector = $80
    .word invalid_opcode    ; funct3 = 1, selector = $82
    .word invalid_opcode    ; funct3 = 1, selector = $84
    .word invalid_opcode    ; funct3 = 1, selector = $86
    .word invalid_opcode    ; funct3 = 1, selector = $88
    .word invalid_opcode    ; funct3 = 1, selector = $8A
    .word invalid_opcode    ; funct3 = 1, selector = $8C
    .word invalid_opcode    ; funct3 = 1, selector = $8E (table_INVALID)
    .word invalid_opcode    ; funct3 = 2, selector = $80
    .word invalid_opcode    ; funct3 = 2, selector = $82
    .word invalid_opcode    ; funct3 = 2, selector = $84
    .word invalid_opcode    ; funct3 = 2, selector = $86
    .word invalid_opcode    ; funct3 = 2, selector = $88
    .word invalid_opcode    ; funct3 = 2, selector = $8A
    .word invalid_opcode    ; funct3 = 2, selector = $8C
    .word invalid_opcode    ; funct3 = 2, selector = $8E (table_INVALID)
    .word invalid_opcode    ; funct3 = 3, selector = $80
    .word invalid_opcode    ; funct3 = 3, selector = $82
    .word invalid_opcode    ; funct3 = 3, selector = $84
    .word invalid_opcode    ; funct3 = 3, selector = $86
    .word invalid_opcode    ; funct3 = 3, selector = $88
    .word invalid_opcode    ; funct3 = 3, selector = $8A
    .word invalid_opcode    ; funct3 = 3, selector = $8C
    .word invalid_opcode    ; funct3 = 3, selector = $8E (table_INVALID)
    .word invalid_opcode    ; funct3 = 4, selector = $80
    .word invalid_opcode    ; funct3 = 4, selector = $82
    .word invalid_opcode    ; funct3 = 4, selector = $84
    .word invalid_opcode    ; funct3 = 4, selector = $86
    .word invalid_opcode    ; funct3 = 4, selector = $88
    .word invalid_opcode    ; funct3 = 4, selector = $8A
    .word invalid_opcode    ; funct3 = 4, selector = $8C
    .word invalid_opcode    ; funct3 = 4, selector = $8E (table_INVALID)
    .word invalid_opcode    ; funct3 = 5, selector = $80
    .word invalid_opcode    ; funct3 = 5, selector = $82
    .word invalid_opcode    ; funct3 = 5, selector = $84
    .word invalid_opcode    ; funct3 = 5, selector = $86
    .word invalid_opcode    ; funct3 = 5, selector = $88
    .word invalid_opcode    ; funct3 = 5, selector = $8A
    .word invalid_opcode    ; funct3 = 5, selector = $8C
    .word invalid_opcode    ; funct3 = 5, selector = $8E (table_INVALID)
    .word invalid_opcode    ; funct3 = 6, selector = $80
    .word invalid_opcode    ; funct3 = 6, selector = $82
    .word invalid_opcode    ; funct3 = 6, selector = $84
    .word invalid_opcode    ; funct3 = 6, selector = $86
    .word invalid_opcode    ; funct3 = 6, selector = $88
    .word invalid_opcode    ; funct3 = 6, selector = $8A
    .word invalid_opcode    ; funct3 = 6, selector = $8C
    .word invalid_opcode    ; funct3 = 6, selector = $8E (table_INVALID)
    .word invalid_opcode    ; funct3 = 7, selector = $80
    .word invalid_opcode    ; funct3 = 7, selector = $82
    .word invalid_opcode    ; funct3 = 7, selector = $84
    .word invalid_opcode    ; funct3 = 7, selector = $86
    .word invalid_opcode    ; funct3 = 7, selector = $88
    .word invalid_opcode    ; funct3 = 7, selector = $8A
    .word invalid_opcode    ; funct3 = 7, selector = $8C
    .word invalid_opcode    ; funct3 = 7, selector = $8E (table_INVALID)

.segment "ROPAGE"
.align 256

; The do_branch macro uses these tables to unshuffle the offset

; Branch, opcode byte 3 to offset byte 0:
; xxx xxx xxx xxx b07 b06 b05 xxx => b07 b06 b05   0   0   0   0   0
; The get_imm_S macro also uses this
branch_lookup_30:
.repeat 16
    .byte $00,$00,$20,$20,$40,$40,$60,$60,$80,$80,$A0,$A0,$C0,$C0,$E0,$E0
.endrep

; Branch, opcode byte 3 to offset byte 1:
; b12 b10 b09 b08 xxx xxx xxx xxx => b12 b12 b12 b12   0 b10 b09 b08
branch_lookup_31:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
.byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
.byte $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
.byte $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
.byte $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
.byte $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte $F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0
.byte $F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1
.byte $F2,$F2,$F2,$F2,$F2,$F2,$F2,$F2,$F2,$F2,$F2,$F2,$F2,$F2,$F2,$F2
.byte $F3,$F3,$F3,$F3,$F3,$F3,$F3,$F3,$F3,$F3,$F3,$F3,$F3,$F3,$F3,$F3
.byte $F4,$F4,$F4,$F4,$F4,$F4,$F4,$F4,$F4,$F4,$F4,$F4,$F4,$F4,$F4,$F4
.byte $F5,$F5,$F5,$F5,$F5,$F5,$F5,$F5,$F5,$F5,$F5,$F5,$F5,$F5,$F5,$F5
.byte $F6,$F6,$F6,$F6,$F6,$F6,$F6,$F6,$F6,$F6,$F6,$F6,$F6,$F6,$F6,$F6
.byte $F7,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$F7
