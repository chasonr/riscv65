; riscv65.s
; Supported extensions: M, Zifencei

; Select supported extensions:
RV32M = 1
; Zifencei merely adds a single no-op instruction, and is always enabled

; Lower 3 bytes of initial PC are fetched from this address
RISCV_ENTRY = $07F0
; Lower 3 bytes of initial SP are fetched from this address
RISCV_STACK = $07F7

.include "registers.inc"

; Processor state includes the integer registers and the program counter
; Registers are stored in a 32-byte aligned segment, so that indexing does not
; cross a page boundary

.segment "REGISTERS"
.align 32

; Integer registers; stored as 32 low bytes, then 32 second bytes, then
; 32 third bytes and finally 32 high bytes
.global _RISCV_ireg_0
_RISCV_ireg_0: .res 32  ; Persistent
.global _RISCV_ireg_1
_RISCV_ireg_1: .res 32  ; Persistent
.global _RISCV_ireg_2
_RISCV_ireg_2: .res 32  ; Persistent
.global _RISCV_ireg_3
_RISCV_ireg_3: .res 32  ; Persistent

.segment "ZEROPAGE"

; Program counter register
.globalzp _RISCV_pc
_RISCV_pc: .res 4       ; Persistent

.if RV32M

; Multiply operands and result
mul_op1_0: .res 1
mul_op1_1: .res 1
mul_op1_2: .res 1
mul_op1_3: .res 1
mul_op2_0: .res 1
mul_op2_1: .res 1
mul_op2_2: .res 1
mul_op2_3: .res 1
mul_prod_0: .res 1
mul_prod_1: .res 1
mul_prod_2: .res 1
mul_prod_3: .res 1
mul_prod_4: .res 1
mul_prod_5: .res 1
mul_prod_6: .res 1
mul_prod_7: .res 1

; Divide operands and result
div_op1_0 = mul_op1_0
div_op1_1 = mul_op1_1
div_op1_2 = mul_op1_2
div_op1_3 = mul_op1_3
div_op2_0 = mul_op2_0
div_op2_1 = mul_op2_1
div_op2_2 = mul_op2_2
div_op2_3 = mul_op2_3
div_rem_0 = mul_prod_0
div_rem_1 = mul_prod_1
div_rem_2 = mul_prod_2
div_rem_3 = mul_prod_3
div_quo_0 = mul_prod_4
div_quo_1 = mul_prod_5
div_quo_2 = mul_prod_6
div_quo_3 = mul_prod_7

.endif ; RV32M

; Fetched opcode goes here
.globalzp _RISCV_opcode
_RISCV_opcode: .res 4

; Address for load or store
.globalzp _RISCV_address
_RISCV_address: .res 4

; Data for load or store
.globalzp _RISCV_data
_RISCV_data: .res 4

; Scratch areas:
; For multiply and divide
.if RV32M
div_op1_sign: .res 1
div_op2_sign: .res 1
muldiv_scratch2_4: .res 1
muldiv_scratch2_5: .res 1
; For fusion of MULH* with MUL and of DIV with REM
old_opcode: .res 3
rs1_reg: .res 1
rs2_reg: .res 1
rd_reg: .res 1
.endif ; RV32M
; For immediate value in opcode, and other purposes
imm_0: .res 1
imm_1: .res 1
imm_2: .res 1
imm_3: .res 1

.bss
; To save the BASIC part of the zero page
zp_size = $90 - 2
zp_save: .res zp_size

.code

.import _RISCV_api_init
.import _RISCV_syscall_init
.import _RISCV_fetch
.import _RISCV_read_8
.import _RISCV_read_16
.import _RISCV_read_32
.import _RISCV_write_8
.import _RISCV_write_16
.import _RISCV_write_32
.import _RISCV_ECALL
.import _RISCV_EBREAK
.import _RISCV_invalid_opcode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            PROGRAM ENTRY POINT                             ;
;                   This must be the first code assembled                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc RISCV_emulator

    ; Save the BASIC part of the zero page
    ldx #zp_size
    @zp_save:
        lda $02-1,x
        sta zp_save-1,x
    dex
    bne @zp_save

    ; Set up the RISC-V state
    jsr RISCV_init

    ; Loop on RISC-V instructions until _RISCV_exit called
    jmp RISCV_instruction

.endproc

; Jump here from anywhere to exit the program
.global _RISCV_exit
.proc _RISCV_exit

    ; Reset the stack pointer
    ; Assumes the structure of the BASIC interpreter
    ldx #$F7
    txs

    ; Restore the zero page
    ldx #zp_size
    zp_restore:
        lda zp_save-1,x
        sta $02-1,x
    dex
    bne zp_restore

    ; Set the normal memory map
    lda $01
    ora #$07
    sta $01

    rts

.endproc

.proc RISCV_init

    ; Set the integer registers to zero
    lda #0
    ldx #31
    zero_loop:
        sta _RISCV_ireg_0,x
        sta _RISCV_ireg_1,x
        sta _RISCV_ireg_2,x
        sta _RISCV_ireg_3,x
    dex
    bpl zero_loop

    ; Set the PC to $01000000+RISCV_ENTRY
    lda RISCV_ENTRY+0
    sta _RISCV_pc+0
    lda RISCV_ENTRY+1
    sta _RISCV_pc+1
    lda RISCV_ENTRY+2
    sta _RISCV_pc+2
    lda #1
    sta _RISCV_pc+3

    ; Set the stack pointer (x2) to $01000000+RISCV_STACK
    lda RISCV_STACK+0
    sta _RISCV_ireg_0+REG_sp
    lda RISCV_STACK+1
    sta _RISCV_ireg_1+REG_sp
    lda RISCV_STACK+2
    sta _RISCV_ireg_2+REG_sp
    lda #1
    sta _RISCV_ireg_3+REG_sp

    ; Set up the API
    jsr _RISCV_api_init
    jsr _RISCV_syscall_init

    rts

.endproc

RISCV_instruction:
    ; Fetch opcode
    jsr _RISCV_fetch

dispatch_instruction:
    ; Decode the lower seven bits
    lda _RISCV_opcode+0 ; (3)
    asl a               ; (2)
    sta jump+1          ; (4) self-modifying code here
    jump:
    jmp (dispatch_main) ; (5) actually (dispatch_main,a)
                        ; total: 14

    ; Instruction routine ends by advancing or modifying PC and jumping back
    ; to RISCV_instruction.
    ; MULH, MULHSU, MULU, DIV and DIVU have their own return paths, to check
    ; for a matching MUL, REM or REMU.

; Advance the PC
.macro advance_pc size
    .local pc_done
    clc                 ; (2)
    lda _RISCV_pc+0     ; (3)
    adc #size           ; (2)
    sta _RISCV_pc+0     ; (3)
    bcc pc_done         ; (2) (3)
    inc _RISCV_pc+1     ; (5) (0)
    bne pc_done         ; (2) (0)
    inc _RISCV_pc+2     ; (5) (0)
    bne pc_done         ; (2) (0)
    inc _RISCV_pc+3     ; (5) (0)
    pc_done:
.endmacro               ; total: min 13 max 31

; Extract the destination register (rd) index into X
; Z set if this index is zero
.macro get_rd_x
    lda _RISCV_opcode+0  ; (3)
    asl a                ; (2)
    lda _RISCV_opcode+1  ; (3)
    rol a                ; (2)
    and #$1F             ; (2)
    tax                  ; (2)
.endmacro                ; total: 14

; Extract the destination register (rd) index into Y
; Z set if this index is zero
.macro get_rd_y
    lda _RISCV_opcode+0  ; (3)
    asl a                ; (2)
    lda _RISCV_opcode+1  ; (3)
    rol a                ; (2)
    and #$1F             ; (2)
    tay                  ; (2)
.endmacro                ; total: 14

; Extract the RS1 field into X
.macro get_rs1_x
    lda _RISCV_opcode+1 ; (3)
    asl a               ; (2)
    lda _RISCV_opcode+2 ; (3)
    rol a               ; (2)
    and #$1F            ; (2)
    tax                 ; (2)
.endmacro               ; total: 14

; Extract the RS2 field into X
.macro get_rs2_x
    lda _RISCV_opcode+3 ; (3)
    lsr a               ; (2)
    lda _RISCV_opcode+2 ; (3)
    ror a               ; (2)
    tax                 ; (2)
    lda rshift3,x       ; (4)
    tax                 ; (2)
.endmacro               ; total: 18

; Extract the RS2 field into Y
.macro get_rs2_y
    lda _RISCV_opcode+3 ; (3)
    lsr a               ; (2)
    lda _RISCV_opcode+2 ; (3)
    ror a               ; (2)
    tay                 ; (2)
    lda rshift3,y       ; (4)
    tay                 ; (2)
.endmacro               ; total: 18

; Extract the rs1 field into X and the rs2 field into Y
.macro get_rs1_rs2
    get_rs1_x           ; (12)
    get_rs2_y           ; (18)
.endmacro               ; total: 30

; Extract the immediate field into imm
; Only three bytes are written; the fourth byte is equal to the third
; This macro clobbers X
.macro get_imm12
    ldx _RISCV_opcode+2 ; (3)
    lda rshift4,x       ; (4)
    ldx _RISCV_opcode+3 ; (3)
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
    lda _RISCV_opcode+0    ; (3)
    asl a                  ; (2)
    lda _RISCV_opcode+1    ; (3)
    rol a                  ; (2)
    and #$1F               ; (2)
    ldx _RISCV_opcode+3    ; (3)
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
    lda _RISCV_opcode+1    ; A = ... ... ... ... b04 b03 b02 b01
    asl a                  ; A = ... ... ... b04 b03 b02 b01 ...
    and #$1E               ; A =   0   0   0 b04 b03 b02 b01   0
    ldy _RISCV_opcode+3    ; Y = b12 b10 b09 b08 b07 b06 b05 ...
    ora branch_lookup_30,y ; A = b07 b06 b05 b04 b03 b02 b01   0
    clc
    adc _RISCV_pc+0
    sta _RISCV_pc+0

    ldx _RISCV_opcode+0    ; X = b11 ... ... ... ... ... ... ...
    lda branch_lookup_01,x ; A =   0   0   0   0 b11   0   0   0
    ora branch_lookup_31,y ; A = b12 b12 b12 b12 b11 b10 b09 b08
    tay                    ; Y = b12 b12 b12 b12 b11 b10 b09 b08
    adc _RISCV_pc+1
    sta _RISCV_pc+1

    lda rsshift4,y         ; A = b12 b12 b12 b12 b12 b12 b12 b12
    tay                    ; Y = b12 b12 b12 b12 b12 b12 b12 b12
    adc _RISCV_pc+2
    sta _RISCV_pc+2

    tya                    ; A = b12 b12 b12 b12 b12 b12 b12 b12
    adc _RISCV_pc+3
    sta _RISCV_pc+3
.endmacro

.proc dispatch_LOAD

    lda _RISCV_opcode+1
    and #$70
    ora #table_LOAD
    sta jump+1           ; self-modifying code here

    get_imm12
    get_rs1_x

    clc
    lda _RISCV_ireg_0,x
    adc imm_0
    sta _RISCV_address+0
    lda _RISCV_ireg_1,x
    adc imm_1
    sta _RISCV_address+1
    lda _RISCV_ireg_2,x
    adc imm_2
    sta _RISCV_address+2
    lda _RISCV_ireg_3,x
    adc imm_2 ; not imm_3
    sta _RISCV_address+3

    jump:
    jmp (dispatch_funct3)  ; actually an indexed jump

.endproc

.proc do_LB

    jsr _RISCV_read_8

    get_rd_x
    beq @end
        lda _RISCV_data+0
        sta _RISCV_ireg_0,x
        cmp #$80
        lda #0
        sbc #0
        eor #$FF
        sta _RISCV_ireg_1,x
        sta _RISCV_ireg_2,x
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_LBU

    jsr _RISCV_read_8

    get_rd_x
    beq @end
        lda _RISCV_data+0
        sta _RISCV_ireg_0,x
        lda #0
        sta _RISCV_ireg_1,x
        sta _RISCV_ireg_2,x
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_LH

    jsr _RISCV_read_16

    get_rd_x
    beq @end
        lda _RISCV_data+0
        sta _RISCV_ireg_0,x
        lda _RISCV_data+1
        sta _RISCV_ireg_1,x
        cmp #$80
        lda #0
        sbc #0
        eor #$FF
        sta _RISCV_ireg_2,x
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_LHU

    jsr _RISCV_read_16

    get_rd_x
    beq @end
        lda _RISCV_data+0
        sta _RISCV_ireg_0,x
        lda _RISCV_data+1
        sta _RISCV_ireg_1,x
        lda #0
        sta _RISCV_ireg_2,x
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_LW

    jsr _RISCV_read_32

    get_rd_x
    beq @end
        lda _RISCV_data+0
        sta _RISCV_ireg_0,x
        lda _RISCV_data+1
        sta _RISCV_ireg_1,x
        lda _RISCV_data+2
        sta _RISCV_ireg_2,x
        lda _RISCV_data+3
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_FENCE

    ; funct3 must be 000 (FENCE) or 001 (FENCE.I)
    lda _RISCV_opcode+1
    and #$60
    bne bad_opcode

    ; FENCE is a no-op

    advance_pc 4
    jmp RISCV_instruction

bad_opcode:
    jmp invalid_opcode

.endproc

.proc dispatch_OP_IMM

    lda _RISCV_opcode+1
    and #$70
   ;ora #table_OP_IMM   ; equal to zero
    sta jump+1          ; self-modifying code here
    jump:
    jmp (dispatch_funct3) ; actually (dispatch_funct3,a)

.endproc

.proc do_ADDI

    get_imm12
    get_rs1_x
    get_rd_y
    beq @end

        clc
        lda _RISCV_ireg_0,x
        adc imm_0
        sta _RISCV_ireg_0,y
        lda _RISCV_ireg_1,x
        adc imm_1
        sta _RISCV_ireg_1,y
        lda _RISCV_ireg_2,x
        adc imm_2
        sta _RISCV_ireg_2,y
        lda _RISCV_ireg_3,x
        adc imm_2 ; not imm_3
        sta _RISCV_ireg_3,y

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SLTI

    get_imm12
    get_rs1_x
    get_rd_y
    beq @end

        lda _RISCV_ireg_0,x
        cmp imm_0
        lda _RISCV_ireg_1,x
        sbc imm_1
        lda _RISCV_ireg_2,x
        sbc imm_2
        lda _RISCV_ireg_3,x
        sbc imm_2 ; not imm_3
        bvc @no_overflow
            eor #$80
        @no_overflow:

        rol a
        lda #0
        rol a

        sta _RISCV_ireg_0,y
        lda #0
        sta _RISCV_ireg_1,y
        sta _RISCV_ireg_2,y
        sta _RISCV_ireg_3,y

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SLTIU

    get_imm12
    get_rs1_x
    get_rd_y
    beq @end

        lda _RISCV_ireg_0,x
        cmp imm_0
        lda _RISCV_ireg_1,x
        sbc imm_1
        lda _RISCV_ireg_2,x
        sbc imm_2
        lda _RISCV_ireg_3,x
        sbc imm_2 ; not imm_3

        lda #0
        rol a
        eor #1

        sta _RISCV_ireg_0,y
        lda #0
        sta _RISCV_ireg_1,y
        sta _RISCV_ireg_2,y
        sta _RISCV_ireg_3,y

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_XORI

    get_imm12
    get_rs1_x
    get_rd_y
    beq @end

        lda _RISCV_ireg_0,x
        eor imm_0
        sta _RISCV_ireg_0,y
        lda _RISCV_ireg_1,x
        eor imm_1
        sta _RISCV_ireg_1,y
        lda _RISCV_ireg_2,x
        eor imm_2
        sta _RISCV_ireg_2,y
        lda _RISCV_ireg_3,x
        eor imm_2 ; not imm_3
        sta _RISCV_ireg_3,y

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_ORI

    get_imm12
    get_rs1_x
    get_rd_y
    beq @end

        lda _RISCV_ireg_0,x
        ora imm_0
        sta _RISCV_ireg_0,y
        lda _RISCV_ireg_1,x
        ora imm_1
        sta _RISCV_ireg_1,y
        lda _RISCV_ireg_2,x
        ora imm_2
        sta _RISCV_ireg_2,y
        lda _RISCV_ireg_3,x
        ora imm_2 ; not imm_3
        sta _RISCV_ireg_3,y

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_ANDI

    get_imm12
    get_rs1_x
    get_rd_y
    beq @end

        lda _RISCV_ireg_0,x
        and imm_0
        sta _RISCV_ireg_0,y
        lda _RISCV_ireg_1,x
        and imm_1
        sta _RISCV_ireg_1,y
        lda _RISCV_ireg_2,x
        and imm_2
        sta _RISCV_ireg_2,y
        lda _RISCV_ireg_3,x
        and imm_2 ; not imm_3
        sta _RISCV_ireg_3,y

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SLLI

    ; funct7 must be zero
    ; RV64I: funct7 reduced to 6 bits to make the shift distance 6 bits
    lda _RISCV_opcode+3
    lsr a
    bne bad_opcode

    get_rs1_x
    lda _RISCV_ireg_0,x
    sta imm_0
    lda _RISCV_ireg_1,x
    sta imm_1
    lda _RISCV_ireg_2,x
    sta imm_2
    lda _RISCV_ireg_3,x
    sta imm_3

    get_rs2_x ; actually shift distance

    jsr shift_left

    get_rd_x
    beq @end
        lda imm_0
        sta _RISCV_ireg_0,x
        lda imm_1
        sta _RISCV_ireg_1,x
        lda imm_2
        sta _RISCV_ireg_2,x
        lda imm_3
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

bad_opcode:
    jmp invalid_opcode

.endproc

.proc do_SRLI_SRAI

    get_rs1_x
    lda _RISCV_ireg_0,x
    sta imm_0
    lda _RISCV_ireg_1,x
    sta imm_1
    lda _RISCV_ireg_2,x
    sta imm_2
    lda _RISCV_ireg_3,x
    sta imm_3

    ; funct7 determines whether we shift signed or unsigned
    ; RV64I: funct7 reduced to 6 bits to make the shift distance 6 bits
    get_rs2_x ; actually shift distance

    lda _RISCV_opcode+3
    lsr a
    beq @do_unsigned
    cmp #$20
    beq @do_signed
    jmp invalid_opcode

    @do_unsigned:
        jsr shift_right_unsigned
        jmp @store
    @do_signed:
        jsr shift_right_signed

    @store:
    get_rd_x
    beq @end
        lda imm_0
        sta _RISCV_ireg_0,x
        lda imm_1
        sta _RISCV_ireg_1,x
        lda imm_2
        sta _RISCV_ireg_2,x
        lda imm_3
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_AUIPC
    get_rd_x
    beq end_AUIPC
        lda _RISCV_pc+0
        sta _RISCV_ireg_0,x
        lda _RISCV_opcode+1
        and #$F0
        clc
        adc _RISCV_pc+1
        sta _RISCV_ireg_1,x
        lda _RISCV_opcode+2
        adc _RISCV_pc+2
        sta _RISCV_ireg_2,x
        lda _RISCV_opcode+3
        adc _RISCV_pc+3
        sta _RISCV_ireg_3,x
    end_AUIPC:
    advance_pc 4
    jmp RISCV_instruction
.endproc

.proc dispatch_STORE

    lda _RISCV_opcode+1
    and #$70
    ora #table_STORE
    sta jump+1          ; self-modifying code here

    get_imm_S
    get_rs1_x

    clc
    lda _RISCV_ireg_0,x
    adc imm_0
    sta _RISCV_address+0
    lda _RISCV_ireg_1,x
    adc imm_1
    sta _RISCV_address+1
    lda _RISCV_ireg_2,x
    adc imm_2
    sta _RISCV_address+2
    lda _RISCV_ireg_3,x
    adc imm_2 ; not imm_3
    sta _RISCV_address+3

    get_rs2_y
    lda _RISCV_ireg_0,y
    sta _RISCV_data+0

    jump:
    jmp (dispatch_funct3)

.endproc

.proc do_SB

    jsr _RISCV_write_8

    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SH

    lda _RISCV_ireg_1,y
    sta _RISCV_data+1

    jsr _RISCV_write_16

    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SW

    lda _RISCV_ireg_1,y
    sta _RISCV_data+1
    lda _RISCV_ireg_2,y
    sta _RISCV_data+2
    lda _RISCV_ireg_3,y
    sta _RISCV_data+3

    jsr _RISCV_write_32

    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc dispatch_OP

    ; Select according to funct3 and funct7

    lda _RISCV_opcode+3
    lsr a               ; funct7
    tax
    lda _RISCV_opcode+1
    and #$70            ; funct3
    ora OP_table,x
    sta @jump+1         ; self-modifying code here
    get_rs1_rs2
    @jump:
    jmp (dispatch_funct3) ; actually (dispatch_funct3,a)

.endproc

.proc do_ADD

    clc
    lda _RISCV_ireg_0,x
    adc _RISCV_ireg_0,y
    sta imm_0
    lda _RISCV_ireg_1,x
    adc _RISCV_ireg_1,y
    sta imm_1
    lda _RISCV_ireg_2,x
    adc _RISCV_ireg_2,y
    sta imm_2
    lda _RISCV_ireg_3,x
    adc _RISCV_ireg_3,y
    sta imm_3

    get_rd_x
    beq @end
        lda imm_0
        sta _RISCV_ireg_0,x
        lda imm_1
        sta _RISCV_ireg_1,x
        lda imm_2
        sta _RISCV_ireg_2,x
        lda imm_3
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SUB

    sec
    lda _RISCV_ireg_0,x
    sbc _RISCV_ireg_0,y
    sta imm_0
    lda _RISCV_ireg_1,x
    sbc _RISCV_ireg_1,y
    sta imm_1
    lda _RISCV_ireg_2,x
    sbc _RISCV_ireg_2,y
    sta imm_2
    lda _RISCV_ireg_3,x
    sbc _RISCV_ireg_3,y
    sta imm_3

    get_rd_x
    beq @end
        lda imm_0
        sta _RISCV_ireg_0,x
        lda imm_1
        sta _RISCV_ireg_1,x
        lda imm_2
        sta _RISCV_ireg_2,x
        lda imm_3
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SLL

    lda _RISCV_ireg_0,x
    sta imm_0
    lda _RISCV_ireg_1,x
    sta imm_1
    lda _RISCV_ireg_2,x
    sta imm_2
    lda _RISCV_ireg_3,x
    sta imm_3

    lda _RISCV_ireg_0,y
    and #$1F
    tax

    jsr shift_left

    get_rd_x
    beq @end
        lda imm_0
        sta _RISCV_ireg_0,x
        lda imm_1
        sta _RISCV_ireg_1,x
        lda imm_2
        sta _RISCV_ireg_2,x
        lda imm_3
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SLT

    lda _RISCV_ireg_0,x
    cmp _RISCV_ireg_0,y
    lda _RISCV_ireg_1,x
    sbc _RISCV_ireg_1,y
    lda _RISCV_ireg_2,x
    sbc _RISCV_ireg_2,y
    lda _RISCV_ireg_3,x
    sbc _RISCV_ireg_3,y
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
        sta _RISCV_ireg_0,x
        lda #0
        sta _RISCV_ireg_1,x
        sta _RISCV_ireg_2,x
        sta _RISCV_ireg_3,x
    @end:

    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SLTU

    lda _RISCV_ireg_0,x
    cmp _RISCV_ireg_0,y
    lda _RISCV_ireg_1,x
    sbc _RISCV_ireg_1,y
    lda _RISCV_ireg_2,x
    sbc _RISCV_ireg_2,y
    lda _RISCV_ireg_3,x
    sbc _RISCV_ireg_3,y

    lda #0
    rol a
    eor #1
    tay

    get_rd_x
    beq @end
        tya
        sta _RISCV_ireg_0,x
        lda #0
        sta _RISCV_ireg_1,x
        sta _RISCV_ireg_2,x
        sta _RISCV_ireg_3,x
    @end:

    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_XOR

    lda _RISCV_ireg_0,x
    eor _RISCV_ireg_0,y
    sta imm_0
    lda _RISCV_ireg_1,x
    eor _RISCV_ireg_1,y
    sta imm_1
    lda _RISCV_ireg_2,x
    eor _RISCV_ireg_2,y
    sta imm_2
    lda _RISCV_ireg_3,x
    eor _RISCV_ireg_3,y
    sta imm_3

    get_rd_x
    beq @end
        lda imm_0
        sta _RISCV_ireg_0,x
        lda imm_1
        sta _RISCV_ireg_1,x
        lda imm_2
        sta _RISCV_ireg_2,x
        lda imm_3
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SRL

    lda _RISCV_ireg_0,x
    sta imm_0
    lda _RISCV_ireg_1,x
    sta imm_1
    lda _RISCV_ireg_2,x
    sta imm_2
    lda _RISCV_ireg_3,x
    sta imm_3

    lda _RISCV_ireg_0,y
    and #$1F
    tax

    jsr shift_right_unsigned

    get_rd_x
    beq @end
        lda imm_0
        sta _RISCV_ireg_0,x
        lda imm_1
        sta _RISCV_ireg_1,x
        lda imm_2
        sta _RISCV_ireg_2,x
        lda imm_3
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_SRA

    lda _RISCV_ireg_0,x
    sta imm_0
    lda _RISCV_ireg_1,x
    sta imm_1
    lda _RISCV_ireg_2,x
    sta imm_2
    lda _RISCV_ireg_3,x
    sta imm_3

    lda _RISCV_ireg_0,y
    and #$1F
    tax

    jsr shift_right_signed

    get_rd_x
    beq @end
        lda imm_0
        sta _RISCV_ireg_0,x
        lda imm_1
        sta _RISCV_ireg_1,x
        lda imm_2
        sta _RISCV_ireg_2,x
        lda imm_3
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_OR

    lda _RISCV_ireg_0,x
    ora _RISCV_ireg_0,y
    sta imm_0
    lda _RISCV_ireg_1,x
    ora _RISCV_ireg_1,y
    sta imm_1
    lda _RISCV_ireg_2,x
    ora _RISCV_ireg_2,y
    sta imm_2
    lda _RISCV_ireg_3,x
    ora _RISCV_ireg_3,y
    sta imm_3

    get_rd_x
    beq @end
        lda imm_0
        sta _RISCV_ireg_0,x
        lda imm_1
        sta _RISCV_ireg_1,x
        lda imm_2
        sta _RISCV_ireg_2,x
        lda imm_3
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_AND

    lda _RISCV_ireg_0,x
    and _RISCV_ireg_0,y
    sta imm_0
    lda _RISCV_ireg_1,x
    and _RISCV_ireg_1,y
    sta imm_1
    lda _RISCV_ireg_2,x
    and _RISCV_ireg_2,y
    sta imm_2
    lda _RISCV_ireg_3,x
    and _RISCV_ireg_3,y
    sta imm_3

    get_rd_x
    beq @end
        lda imm_0
        sta _RISCV_ireg_0,x
        lda imm_1
        sta _RISCV_ireg_1,x
        lda imm_2
        sta _RISCV_ireg_2,x
        lda imm_3
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.if RV32M

.proc do_MUL

    lda _RISCV_ireg_0,x
    sta mul_op1_0
    lda _RISCV_ireg_1,x
    sta mul_op1_1
    lda _RISCV_ireg_2,x
    sta mul_op1_2
    lda _RISCV_ireg_3,x
    sta mul_op1_3

    lda _RISCV_ireg_0,y
    sta mul_op2_0
    lda _RISCV_ireg_1,y
    sta mul_op2_1
    lda _RISCV_ireg_2,y
    sta mul_op2_2
    lda _RISCV_ireg_3,y
    sta mul_op2_3

    jsr multiply_low

    get_rd_x
    beq @end

        lda mul_prod_0
        sta _RISCV_ireg_0,x
        lda mul_prod_1
        sta _RISCV_ireg_1,x
        lda mul_prod_2
        sta _RISCV_ireg_2,x
        lda mul_prod_3
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_MULH

    stx rs1_reg
    sty rs2_reg

    lda _RISCV_ireg_0,x
    sta mul_op1_0
    lda _RISCV_ireg_1,x
    sta mul_op1_1
    lda _RISCV_ireg_2,x
    sta mul_op1_2
    lda _RISCV_ireg_3,x
    sta mul_op1_3

    lda _RISCV_ireg_0,y
    sta mul_op2_0
    lda _RISCV_ireg_1,y
    sta mul_op2_1
    lda _RISCV_ireg_2,y
    sta mul_op2_2
    lda _RISCV_ireg_3,y
    sta mul_op2_3

    jsr multiply_signed

    get_rd_x
    stx rd_reg
    beq @end

        lda mul_prod_4
        sta _RISCV_ireg_0,x
        lda mul_prod_5
        sta _RISCV_ireg_1,x
        lda mul_prod_6
        sta _RISCV_ireg_2,x
        lda mul_prod_7
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp end_MULH

.endproc

.proc do_MULHSU

    stx rs1_reg
    sty rs2_reg

    lda _RISCV_ireg_0,x
    sta mul_op1_0
    lda _RISCV_ireg_1,x
    sta mul_op1_1
    lda _RISCV_ireg_2,x
    sta mul_op1_2
    lda _RISCV_ireg_3,x
    sta mul_op1_3

    lda _RISCV_ireg_0,y
    sta mul_op2_0
    lda _RISCV_ireg_1,y
    sta mul_op2_1
    lda _RISCV_ireg_2,y
    sta mul_op2_2
    lda _RISCV_ireg_3,y
    sta mul_op2_3

    jsr multiply_signed_unsigned

    get_rd_x
    stx rd_reg
    beq @end

        lda mul_prod_4
        sta _RISCV_ireg_0,x
        lda mul_prod_5
        sta _RISCV_ireg_1,x
        lda mul_prod_6
        sta _RISCV_ireg_2,x
        lda mul_prod_7
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp end_MULH

.endproc

.proc do_MULHU

    stx rs1_reg
    sty rs2_reg

    lda _RISCV_ireg_0,x
    sta mul_op1_0
    lda _RISCV_ireg_1,x
    sta mul_op1_1
    lda _RISCV_ireg_2,x
    sta mul_op1_2
    lda _RISCV_ireg_3,x
    sta mul_op1_3

    lda _RISCV_ireg_0,y
    sta mul_op2_0
    lda _RISCV_ireg_1,y
    sta mul_op2_1
    lda _RISCV_ireg_2,y
    sta mul_op2_2
    lda _RISCV_ireg_3,y
    sta mul_op2_3

    jsr multiply_unsigned

    get_rd_x
    stx rd_reg
    beq @end

        lda mul_prod_4
        sta _RISCV_ireg_0,x
        lda mul_prod_5
        sta _RISCV_ireg_1,x
        lda mul_prod_6
        sta _RISCV_ireg_2,x
        lda mul_prod_7
        sta _RISCV_ireg_3,x

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
    beq end_instruction

    ; Save the old opcode, modify funct3 to match the expected MUL
    ; and mask off rd
    ; The least significant byte will be $33 after masking off rd
    lda _RISCV_opcode+1
    and #$80
    sta old_opcode+0
    lda _RISCV_opcode+2
    sta old_opcode+1
    lda _RISCV_opcode+3
    sta old_opcode+2

    ; Fetch the next opcode
    jsr _RISCV_fetch

    ; If the opcode matches the expected MUL, use the computed low word
    lda _RISCV_opcode+0
    and #$7F
    cmp #$33
    bne do_instruction
    lda _RISCV_opcode+1
    and #$F0
    cmp old_opcode+0
    bne do_instruction
    lda _RISCV_opcode+2
    cmp old_opcode+1
    bne do_instruction
    lda _RISCV_opcode+3
    cmp old_opcode+2
    bne do_instruction

    ; The opcode matches the expected MUL
    get_rd_x
    beq end_low_word
        lda mul_prod_0
        sta _RISCV_ireg_0,x
        lda mul_prod_1
        sta _RISCV_ireg_1,x
        lda mul_prod_2
        sta _RISCV_ireg_2,x
        lda mul_prod_3
        sta _RISCV_ireg_3,x
    end_low_word:
    advance_pc 4

end_instruction:
    jmp RISCV_instruction

    ; Run the instruction that we just fetched
do_instruction:
    jmp dispatch_instruction

.endproc

.proc do_DIV

    stx rs1_reg
    sty rs2_reg

    lda _RISCV_ireg_0,x
    sta div_op1_0
    lda _RISCV_ireg_1,x
    sta div_op1_1
    lda _RISCV_ireg_2,x
    sta div_op1_2
    lda _RISCV_ireg_3,x
    sta div_op1_3

    lda _RISCV_ireg_0,y
    sta div_op2_0
    lda _RISCV_ireg_1,y
    sta div_op2_1
    lda _RISCV_ireg_2,y
    sta div_op2_2
    lda _RISCV_ireg_3,y
    sta div_op2_3

    jsr divide_signed

    get_rd_x
    stx rd_reg
    beq @end

        lda div_quo_0
        sta _RISCV_ireg_0,x
        lda div_quo_1
        sta _RISCV_ireg_1,x
        lda div_quo_2
        sta _RISCV_ireg_2,x
        lda div_quo_3
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp end_DIV_DIVU

.endproc

.proc do_DIVU

    stx rs1_reg
    sty rs2_reg

    lda _RISCV_ireg_0,x
    sta div_op1_0
    lda _RISCV_ireg_1,x
    sta div_op1_1
    lda _RISCV_ireg_2,x
    sta div_op1_2
    lda _RISCV_ireg_3,x
    sta div_op1_3

    lda _RISCV_ireg_0,y
    sta div_op2_0
    lda _RISCV_ireg_1,y
    sta div_op2_1
    lda _RISCV_ireg_2,y
    sta div_op2_2
    lda _RISCV_ireg_3,y
    sta div_op2_3

    jsr divide_unsigned

    get_rd_x
    stx rd_reg
    beq @end

        lda div_quo_0
        sta _RISCV_ireg_0,x
        lda div_quo_1
        sta _RISCV_ireg_1,x
        lda div_quo_2
        sta _RISCV_ireg_2,x
        lda div_quo_3
        sta _RISCV_ireg_3,x

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
    beq end_instruction

    ; Save the old opcode, modify funct3 to match the expected REM or REMU,
    ; and mask off rd
    ; The least significant byte will be $33 after masking off rd
    lda _RISCV_opcode+1
    and #$F0
    ora #$20
    sta old_opcode+0
    lda _RISCV_opcode+2
    sta old_opcode+1
    lda _RISCV_opcode+3
    sta old_opcode+2

    ; Fetch the next opcode
    jsr _RISCV_fetch

    ; If the opcode matches the expected REM or REMU, use the computed remainder
    lda _RISCV_opcode+0
    and #$7F
    cmp #$33
    bne do_instruction
    lda _RISCV_opcode+1
    and #$F0
    cmp old_opcode+0
    bne do_instruction
    lda _RISCV_opcode+2
    cmp old_opcode+1
    bne do_instruction
    lda _RISCV_opcode+3
    cmp old_opcode+2
    bne do_instruction

    ; The opcode matches the expected REM or REMU
    get_rd_x
    beq end_remainder
        lda div_rem_0
        sta _RISCV_ireg_0,x
        lda div_rem_1
        sta _RISCV_ireg_1,x
        lda div_rem_2
        sta _RISCV_ireg_2,x
        lda div_rem_3
        sta _RISCV_ireg_3,x
    end_remainder:
    advance_pc 4

end_instruction:
    jmp RISCV_instruction

    ; Run the instruction that we just fetched
do_instruction:
    jmp dispatch_instruction

.endproc

.proc do_REM

    lda _RISCV_ireg_0,x
    sta div_op1_0
    lda _RISCV_ireg_1,x
    sta div_op1_1
    lda _RISCV_ireg_2,x
    sta div_op1_2
    lda _RISCV_ireg_3,x
    sta div_op1_3

    lda _RISCV_ireg_0,y
    sta div_op2_0
    lda _RISCV_ireg_1,y
    sta div_op2_1
    lda _RISCV_ireg_2,y
    sta div_op2_2
    lda _RISCV_ireg_3,y
    sta div_op2_3

    jsr divide_signed

    get_rd_x
    beq @end

        lda div_rem_0
        sta _RISCV_ireg_0,x
        lda div_rem_1
        sta _RISCV_ireg_1,x
        lda div_rem_2
        sta _RISCV_ireg_2,x
        lda div_rem_3
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.proc do_REMU

    lda _RISCV_ireg_0,x
    sta div_op1_0
    lda _RISCV_ireg_1,x
    sta div_op1_1
    lda _RISCV_ireg_2,x
    sta div_op1_2
    lda _RISCV_ireg_3,x
    sta div_op1_3

    lda _RISCV_ireg_0,y
    sta div_op2_0
    lda _RISCV_ireg_1,y
    sta div_op2_1
    lda _RISCV_ireg_2,y
    sta div_op2_2
    lda _RISCV_ireg_3,y
    sta div_op2_3

    jsr divide_unsigned

    get_rd_x
    beq @end

        lda div_rem_0
        sta _RISCV_ireg_0,x
        lda div_rem_1
        sta _RISCV_ireg_1,x
        lda div_rem_2
        sta _RISCV_ireg_2,x
        lda div_rem_3
        sta _RISCV_ireg_3,x

    @end:
    advance_pc 4
    jmp RISCV_instruction

.endproc

.else ; RV32M not supported

do_MUL = invalid_opcode
do_MULH = invalid_opcode
do_MULHSU = invalid_opcode
do_MULHU = invalid_opcode
do_DIV = invalid_opcode
do_DIVU = invalid_opcode
do_REM = invalid_opcode
do_REMU = invalid_opcode

.endif ; RV32M

.proc do_LUI
    get_rd_x
    beq end_LUI
        lda #0
        sta _RISCV_ireg_0,x
        lda _RISCV_opcode+1
        and #$F0
        sta _RISCV_ireg_1,x
        lda _RISCV_opcode+2
        sta _RISCV_ireg_2,x
        lda _RISCV_opcode+3
        sta _RISCV_ireg_3,x
    end_LUI:
    advance_pc 4
    jmp RISCV_instruction
.endproc

.proc dispatch_BRANCH

    lda _RISCV_opcode+1
    and #$70
    ora #table_BRANCH
    sta jump+1          ; self-modifying code here
    get_rs1_rs2
    jump:
    jmp (dispatch_funct3)

.endproc

.proc do_BGEU

    lda _RISCV_ireg_0,x ; rs1
    cmp _RISCV_ireg_0,y ; rs2
    lda _RISCV_ireg_1,x
    sbc _RISCV_ireg_1,y
    lda _RISCV_ireg_2,x
    sbc _RISCV_ireg_2,y
    lda _RISCV_ireg_3,x
    sbc _RISCV_ireg_3,y
    bcs take_branch

skip_branch:
    advance_pc 4
    jmp RISCV_instruction

take_branch:
    do_branch
    jmp RISCV_instruction

.endproc

.proc do_BLTU

    lda _RISCV_ireg_0,x ; rs1
    cmp _RISCV_ireg_0,y ; rs2
    lda _RISCV_ireg_1,x
    sbc _RISCV_ireg_1,y
    lda _RISCV_ireg_2,x
    sbc _RISCV_ireg_2,y
    lda _RISCV_ireg_3,x
    sbc _RISCV_ireg_3,y
    bcc take_branch

skip_branch:
    advance_pc 4
    jmp RISCV_instruction

take_branch:
    do_branch
    jmp RISCV_instruction

.endproc

.proc do_BGE

    lda _RISCV_ireg_0,x ; rs1
    cmp _RISCV_ireg_0,y ; rs2
    lda _RISCV_ireg_1,x
    sbc _RISCV_ireg_1,y
    lda _RISCV_ireg_2,x
    sbc _RISCV_ireg_2,y
    lda _RISCV_ireg_3,x
    sbc _RISCV_ireg_3,y
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

    lda _RISCV_ireg_0,x ; rs1
    cmp _RISCV_ireg_0,y ; rs2
    lda _RISCV_ireg_1,x
    sbc _RISCV_ireg_1,y
    lda _RISCV_ireg_2,x
    sbc _RISCV_ireg_2,y
    lda _RISCV_ireg_3,x
    sbc _RISCV_ireg_3,y
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

    lda _RISCV_ireg_0,x ; rs1
    cmp _RISCV_ireg_0,y ; rs2
    bne skip_branch
    lda _RISCV_ireg_1,x
    cmp _RISCV_ireg_1,y
    bne skip_branch
    lda _RISCV_ireg_2,x
    cmp _RISCV_ireg_2,y
    bne skip_branch
    lda _RISCV_ireg_3,x
    cmp _RISCV_ireg_3,y
    beq take_branch

skip_branch:
    advance_pc 4
    jmp RISCV_instruction

take_branch:
    do_branch
    jmp RISCV_instruction

.endproc

.proc do_BNE

    lda _RISCV_ireg_0,x ; rs1
    cmp _RISCV_ireg_0,y ; rs2
    bne take_branch
    lda _RISCV_ireg_1,x
    cmp _RISCV_ireg_1,y
    bne take_branch
    lda _RISCV_ireg_2,x
    cmp _RISCV_ireg_2,y
    bne take_branch
    lda _RISCV_ireg_3,x
    cmp _RISCV_ireg_3,y
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
    lda _RISCV_opcode+1
    and #$70
    bne bad_JALR

    ; Need to calculate the jump target first, then save the return address,
    ; in case of source and destination registers the same

    get_imm12
    get_rs1_x
    clc
    lda imm_0
    adc _RISCV_ireg_0,x
    sta imm_0
    lda imm_1
    adc _RISCV_ireg_1,x
    sta imm_1
    lda imm_2
    adc _RISCV_ireg_2,x
    sta imm_3
    lda imm_2 ; not imm_3
    adc _RISCV_ireg_3,x
    sta imm_2
    ; Role of imm_2 and imm_3 is reversed because imm_2 was needed at the end

    ; Save the return address (but not to x0)
    get_rd_x
    beq take_JALR
        clc
        lda _RISCV_pc+0
        adc #4
        sta _RISCV_ireg_0,x
        lda _RISCV_pc+1
        adc #0
        sta _RISCV_ireg_1,x
        lda _RISCV_pc+2
        adc #0
        sta _RISCV_ireg_2,x
        lda _RISCV_pc+3
        adc #0
        sta _RISCV_ireg_3,x
    take_JALR:

    ; Jump
    lda imm_0
    and #$FE
    sta _RISCV_pc+0
    lda imm_1
    sta _RISCV_pc+1
    lda imm_3   ; not imm_2
    sta _RISCV_pc+2
    lda imm_2   ; not imm_3
    sta _RISCV_pc+3

    jmp RISCV_instruction

.endproc

.proc do_JAL
    get_rd_x
    beq take_JAL
        clc
        lda _RISCV_pc+0
        adc #4
        sta _RISCV_ireg_0,x
        lda _RISCV_pc+1
        adc #0
        sta _RISCV_ireg_1,x
        lda _RISCV_pc+2
        adc #0
        sta _RISCV_ireg_2,x
        lda _RISCV_pc+3
        adc #0
        sta _RISCV_ireg_3,x
    take_JAL:

    ; Bits of immediate value are scrambled
    ldx _RISCV_opcode+2  ; (3) X = b03 b02 b01 b11 b19 b18 b17 b16
    ldy _RISCV_opcode+3  ; (3) Y = b20 b10 b09 b08 b07 b06 b05 b04

    lda jump_lookup_20,x ; (4) A =   0   0   0   0 b03 b02 b01   0
    ora lshift4,y        ; (4) A = b07 b06 b05 b04   0   0   0   0
    clc                  ; (2)
    adc _RISCV_pc+0      ; (3)
    sta _RISCV_pc+0      ; (3)

    lda _RISCV_opcode+1  ; (3) A = b15 b14 b13 b12 xxx xxx xxx xxx
    and #$F0             ; (2) A = b15 b14 b13 b12   0   0   0   0
    ora jump_lookup_21,x ; (4) A = b15 b14 b13 b12 b11   0   0   0
    ora jump_lookup_31,y ; (4) A = b15 b14 b13 b12 b11 b10 b09 b08
    adc _RISCV_pc+1      ; (3)
    sta _RISCV_pc+1      ; (3)

    txa                  ; (2) A = b03 b02 b01 b11 b19 b18 b17 b16
    and #$0F             ; (2) A =   0   0   0   0 b19 b18 b17 b16
    ora jump_lookup_32,y ; (2) A = b20 b20 b20 b20 b19 b18 b17 b16
    tay                  ; (2)
    adc _RISCV_pc+2      ; (3)
    sta _RISCV_pc+2      ; (3)

    lda rsshift4,y       ; (4) A = b20 b20 b20 b20 b20 b20 b20 b20
    adc _RISCV_pc+3      ; (3)
    sta _RISCV_pc+3      ; (3)
                         ; total: 65

    jmp RISCV_instruction
.endproc

.proc do_ECALL_EBREAK

    ; Zicsr extension uses this path
    ; Check for unimplemented opcodes
    lda _RISCV_opcode+1
    ora _RISCV_opcode+3
    bne bad_opcode
    lda _RISCV_opcode+0
    cmp #$73
    bne bad_opcode

    lda _RISCV_opcode+2
    bne check_EBREAK
    ; ECALL
    jsr _RISCV_ECALL
    advance_pc 4
    jmp RISCV_instruction

check_EBREAK:
    cmp #$10
    bne bad_opcode
    ; EBREAK
    jsr _RISCV_EBREAK
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
    jmp _RISCV_invalid_opcode
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                          32 bit shift algorithms                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Shift the contents of imm to the left
; X contains the number of bits to shift left
.proc shift_left

    txa
    asl a
    sta jump+1      ; self-modifying code here
    jump:
    jmp (sll_dispatch) ; actually (sll_dispatch,a)

.endproc

.proc sll_0
    rts             ;  0
.endproc

.proc sll_1
    asl imm_0       ; (5)
    rol imm_1       ; (5)
    rol imm_2       ; (5)
    rol imm_3       ; (5)
    rts             ; 20
.endproc

.proc sll_2
    asl imm_0       ; (5)
    rol imm_1       ; (5)
    rol imm_2       ; (5)
    rol imm_3       ; (5)
    asl imm_0       ; (5)
    rol imm_1       ; (5)
    rol imm_2       ; (5)
    rol imm_3       ; (5)
    rts             ; 40
.endproc

.proc sll_3
    ldy imm_0       ; (3)
    lda lshift3,y   ; (4)
    sta imm_0       ; (3)
    lda rshift5,y   ; (4)
    ldy imm_1       ; (3)
    ora lshift3,y   ; (4)
    sta imm_1       ; (3)
    lda rshift5,y   ; (4)
    ldy imm_2       ; (3)
    ora lshift3,y   ; (4)
    sta imm_2       ; (3)
    lda rshift5,y   ; (4)
    ldy imm_3       ; (3)
    ora lshift3,y   ; (4)
    sta imm_3       ; (3)
    rts             ; 52
.endproc

.proc sll_4
    ldy imm_0       ; (3)
    lda lshift4,y   ; (4)
    sta imm_0       ; (3)
    lda rshift4,y   ; (4)
    ldy imm_1       ; (3)
    ora lshift4,y   ; (4)
    sta imm_1       ; (3)
    lda rshift4,y   ; (4)
    ldy imm_2       ; (3)
    ora lshift4,y   ; (4)
    sta imm_2       ; (3)
    lda rshift4,y   ; (4)
    ldy imm_3       ; (3)
    ora lshift4,y   ; (4)
    sta imm_3       ; (3)
    rts             ; 52
.endproc

.proc sll_5
    ldy imm_0       ; (3)
    lda lshift5,y   ; (4)
    sta imm_0       ; (3)
    lda rshift3,y   ; (4)
    ldy imm_1       ; (3)
    ora lshift5,y   ; (4)
    sta imm_1       ; (3)
    lda rshift3,y   ; (4)
    ldy imm_2       ; (3)
    ora lshift5,y   ; (4)
    sta imm_2       ; (3)
    lda rshift3,y   ; (4)
    ldy imm_3       ; (3)
    ora lshift5,y   ; (4)
    sta imm_3       ; (3)
    rts             ; 52
.endproc

.proc sll_6
    ldy imm_0       ; (3)
    lda lshift6,y   ; (4)
    sta imm_0       ; (3)
    lda rshift2,y   ; (4)
    ldy imm_1       ; (3)
    ora lshift6,y   ; (4)
    sta imm_1       ; (3)
    lda rshift2,y   ; (4)
    ldy imm_2       ; (3)
    ora lshift6,y   ; (4)
    sta imm_2       ; (3)
    lda rshift2,y   ; (4)
    ldy imm_3       ; (3)
    ora lshift6,y   ; (4)
    sta imm_3       ; (3)
    rts             ; 52
.endproc

;b37 b36 b35 b34 b33 b32 b31 b30|b27 b26 b25 b24 b23 b22 b21 b20|b17 b16 b15 b14 b13 b12 b11 b10|b07 b06 b05 b04 b03 b02 b01 b00
;b30 b27 b26 b25 b24 b23 b22 b21|b20 b17 b16 b15 b14 b13 b12 b11|b10 b07 b06 b05 b04 b03 b02 b01|b00   0   0   0   0   0   0   0
.proc sll_7
    lda imm_3       ; (3) A=b37 b36 b35 b34 b33 b32 b31 b30  C=...
    lsr a           ; (2) A=  0 b37 b36 b35 b34 b33 b32 b31  C=b30
    lda imm_2       ; (3) A=b27 b26 b25 b24 b23 b22 b21 b20  C=b30
    ror a           ; (2) A=b30 b27 b26 b25 b24 b23 b22 b21  C=b20
    sta imm_3       ; (3)
    lda imm_1       ; (3) A=b17 b16 b15 b14 b13 b12 b11 b10  C=b20
    ror a           ; (2) A=b20 b17 b16 b15 b14 b13 b12 b11  C=b10
    sta imm_2       ; (3)
    lda imm_0       ; (3) A=b07 b06 b05 b04 b03 b02 b01 b00  C=b10
    ror a           ; (2) A=b10 b07 b06 b05 b04 b03 b02 b01  C=b00
    sta imm_1       ; (3)
    lda #0          ; (2) A=  0   0   0   0   0   0   0   0  C=b00
    ror a           ; (2) A=b00   0   0   0   0   0   0   0  C=  0
    sta imm_0       ; (3)
    rts             ; 36
.endproc

.proc sll_8
    lda imm_2       ; (3)
    sta imm_3       ; (3)
    lda imm_1       ; (3)
    sta imm_2       ; (3)
    lda imm_0       ; (3)
    sta imm_1       ; (3)
    lda #0          ; (2)
    sta imm_0       ; (3)
    rts             ; 23
.endproc

.proc sll_9
    ldx imm_2       ; (3)
    ldy imm_1       ; (3)
    lda imm_0       ; (3)
    asl a           ; (2)
    sta imm_1       ; (3)
    tya             ; (2)
    rol a           ; (2)
    sta imm_2       ; (3)
    txa             ; (2)
    rol a           ; (2)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_0       ; (3)
    rts             ; 33
.endproc

;b37 b36 b35 b34 b33 b32 b31 b30|b27 b26 b25 b24 b23 b22 b21 b20|b17 b16 b15 b14 b13 b12 b11 b10|b07 b06 b05 b04 b03 b02 b01 b00
;b25 b24 b23 b22 b21 b20 b17 b16|b15 b14 b13 b12 b11 b10 b07 b06|b05 b04 b03 b02 b01 b00   0   0|  0   0   0   0   0   0   0   0
.proc sll_10
    ldy imm_0       ; (3) A=... ... ... ... ... ... ... ...  X=... ... ... ... ... ... ... ...  Y=b07 b06 b05 b04 b03 b02 b01 b00
    lda lshift2,y   ; (4) A=b05 b04 b03 b02 b01 b00   0   0  X=... ... ... ... ... ... ... ...  Y=b07 b06 b05 b04 b03 b02 b01 b00
    ldx imm_1       ; (3) A=b05 b04 b03 b02 b01 b00   0   0  X=b17 b16 b15 b14 b13 b12 b11 b10  Y=b07 b06 b05 b04 b03 b02 b01 b00
    sta imm_1       ; (3)
    lda rshift6,y   ; (4) A=  0   0   0   0   0   0 b07 b06  X=b17 b16 b15 b14 b13 b12 b11 b10  Y=b07 b06 b05 b04 b03 b02 b01 b00
    ora lshift2,x   ; (4) A=b15 b14 b13 b12 b11 b10 b07 b06  X=b17 b16 b15 b14 b13 b12 b11 b10  Y=b07 b06 b05 b04 b03 b02 b01 b00
    ldy imm_2       ; (3) A=b15 b14 b13 b12 b11 b10 b07 b06  X=b17 b16 b15 b14 b13 b12 b11 b10  Y=b27 b26 b25 b24 b23 b22 b21 b20
    sta imm_2       ; (3)
    lda rshift6,x   ; (4) A=  0   0   0   0   0   0 b17 b16  X=b17 b16 b15 b14 b13 b12 b11 b10  Y=b27 b26 b25 b24 b23 b22 b21 b20
    ora lshift2,y   ; (4) A=b25 b24 b23 b22 b21 b20 b17 b16  X=b17 b16 b15 b14 b13 b12 b11 b10  Y=b27 b26 b25 b24 b23 b22 b21 b20
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_0       ; (3)
    rts             ; 43
.endproc

.proc sll_11
    ldy imm_0       ; (3)
    lda lshift3,y   ; (4)
    ldx imm_1       ; (3)
    sta imm_1       ; (3)
    lda rshift5,y   ; (4)
    ora lshift3,x   ; (4)
    ldy imm_2       ; (3)
    sta imm_2       ; (3)
    lda rshift5,x   ; (4)
    ora lshift3,y   ; (4)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_0       ; (3)
    rts             ; 43
.endproc

.proc sll_12
    ldy imm_0       ; (3)
    lda lshift4,y   ; (4)
    ldx imm_1       ; (3)
    sta imm_1       ; (3)
    lda rshift4,y   ; (4)
    ora lshift4,x   ; (4)
    ldy imm_2       ; (3)
    sta imm_2       ; (3)
    lda rshift4,x   ; (4)
    ora lshift4,y   ; (4)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_0       ; (3)
    rts             ; 43
.endproc

.proc sll_13
    ldy imm_0       ; (3)
    lda lshift5,y   ; (4)
    ldx imm_1       ; (3)
    sta imm_1       ; (3)
    lda rshift3,y   ; (4)
    ora lshift5,x   ; (4)
    ldy imm_2       ; (3)
    sta imm_2       ; (3)
    lda rshift3,x   ; (4)
    ora lshift5,y   ; (4)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_0       ; (3)
    rts             ; 43
.endproc

.proc sll_14
    ldy imm_0       ; (3)
    lda lshift6,y   ; (4)
    ldx imm_1       ; (3)
    sta imm_1       ; (3)
    lda rshift2,y   ; (4)
    ora lshift6,x   ; (4)
    ldy imm_2       ; (3)
    sta imm_2       ; (3)
    lda rshift2,x   ; (4)
    ora lshift6,y   ; (4)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_0       ; (3)
    rts             ; 43
.endproc

;b37 b36 b35 b34 b33 b32 b31 b30|b27 b26 b25 b24 b23 b22 b21 b20|b17 b16 b15 b14 b13 b12 b11 b10|b07 b06 b05 b04 b03 b02 b01 b00
;b20 b17 b16 b15 b14 b13 b12 b11|b10 b07 b06 b05 b04 b03 b02 b01|b00   0   0   0   0   0   0   0|  0   0   0   0   0   0   0   0
.proc sll_15
    lda imm_2       ; (3) A=b27 b26 b25 b24 b23 b22 b21 b20  C=...
    lsr a           ; (2) A=  0 b27 b26 b25 b24 b23 b22 b21  C=b20
    lda imm_1       ; (3) A=b17 b16 b15 b14 b13 b12 b11 b10  C=b20
    ror a           ; (2) A=b20 b17 b16 b15 b14 b13 b12 b11  C=b10
    sta imm_3       ; (3)
    lda imm_0       ; (3) A=b07 b06 b05 b04 b03 b02 b01 b00  C=b10
    ror a           ; (2) A=b10 b07 b06 b05 b04 b03 b02 b01  C=b00
    sta imm_2       ; (3)
    lda #0          ; (2) A=  0   0   0   0   0   0   0   0  C=b00
    sta imm_0       ; (3)
    ror a           ; (2) A=b00   0   0   0   0   0   0   0  C=  0
    sta imm_1       ; (3)
    rts             ; 31
.endproc

.proc sll_16
    lda imm_1       ; (3)
    sta imm_3       ; (3)
    lda imm_0       ; (3)
    sta imm_2       ; (3)
    lda #0          ; (2)
    sta imm_1       ; (3)
    sta imm_0       ; (3)
    rts             ; 20
.endproc

.proc sll_17
    lda imm_0       ; (3)
    asl a           ; (2)
    sta imm_2       ; (3)
    lda imm_1       ; (3)
    rol a           ; (2)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_1       ; (3)
    sta imm_0       ; (3)
    rts             ; 24
.endproc

.proc sll_18
    lda imm_0       ; (3)
    asl a           ; (2)
    rol imm_1       ; (5)
    asl a           ; (2)
    sta imm_2       ; (3)
    lda imm_1       ; (3)
    rol a           ; (2)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_1       ; (3)
    sta imm_0       ; (3)
    rts             ; 31
.endproc

.proc sll_19
    ldx imm_0       ; (3)
    lda lshift3,x   ; (4)
    sta imm_2       ; (3)
    lda rshift5,x   ; (4)
    ldx imm_1       ; (3)
    ora lshift3,x   ; (4)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_1       ; (3)
    sta imm_0       ; (3)
    rts             ; 32
.endproc

.proc sll_20
    ldx imm_0       ; (3)
    lda lshift4,x   ; (4)
    sta imm_2       ; (3)
    lda rshift4,x   ; (4)
    ldx imm_1       ; (3)
    ora lshift4,x   ; (4)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_1       ; (3)
    sta imm_0       ; (3)
    rts             ; 32
.endproc

.proc sll_21
    ldx imm_0       ; (3)
    lda lshift5,x   ; (4)
    sta imm_2       ; (3)
    lda rshift3,x   ; (4)
    ldx imm_1       ; (3)
    ora lshift5,x   ; (4)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_1       ; (3)
    sta imm_0       ; (3)
    rts             ; 32
.endproc

.proc sll_22
    ldx imm_0       ; (3)
    lda lshift6,x   ; (4)
    sta imm_2       ; (3)
    lda rshift2,x   ; (4)
    ldx imm_1       ; (3)
    ora lshift6,x   ; (4)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_1       ; (3)
    sta imm_0       ; (3)
    rts             ; 32
.endproc

.proc sll_23
    lda imm_1       ; (3)
    lsr a           ; (2)
    lda imm_0       ; (3)
    ror a           ; (2)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_1       ; (3)
    sta imm_0       ; (3)
    ror a           ; (2)
    sta imm_2       ; (3)
    rts             ; 26
.endproc

.proc sll_24
    lda imm_0       ; (3)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_2       ; (3)
    sta imm_1       ; (3)
    sta imm_0       ; (3)
    rts             ; 17
.endproc

.proc sll_25
    lda imm_0       ; (3)
    asl a           ; (2)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_2       ; (3)
    sta imm_1       ; (3)
    sta imm_0       ; (3)
    rts             ; 19
.endproc

.proc sll_26
    lda imm_0       ; (3)
    asl a           ; (2)
    asl a           ; (2)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_2       ; (3)
    sta imm_1       ; (3)
    sta imm_0       ; (3)
    rts             ; 21
.endproc

.proc sll_27
    ldx imm_0       ; (3)
    lda lshift3,x   ; (4)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_2       ; (3)
    sta imm_1       ; (3)
    sta imm_0       ; (3)
    rts             ; 21
.endproc

.proc sll_28
    ldx imm_0       ; (3)
    lda lshift4,x   ; (4)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_2       ; (3)
    sta imm_1       ; (3)
    sta imm_0       ; (3)
    rts             ; 21
.endproc

.proc sll_29
    ldx imm_0       ; (3)
    lda lshift5,x   ; (4)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_2       ; (3)
    sta imm_1       ; (3)
    sta imm_0       ; (3)
    rts             ; 21
.endproc

.proc sll_30
    ldx imm_0       ; (3)
    lda lshift6,x   ; (4)
    sta imm_3       ; (3)
    lda #0          ; (2)
    sta imm_2       ; (3)
    sta imm_1       ; (3)
    sta imm_0       ; (3)
    rts             ; 21
.endproc

.proc sll_31
    lda imm_0       ; (3)
    lsr a           ; (2)
    lda #0          ; (2)
    sta imm_2       ; (3)
    sta imm_1       ; (3)
    sta imm_0       ; (3)
    ror a           ; (2)
    sta imm_3       ; (3)
    rts             ; 21
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Shift the contents of imm to the right and fill with zeros
; X contains the number of bits to shift right
.proc shift_right_unsigned

    txa
    asl a
    sta jump+1      ; self-modifying code here
    jump:
    jmp (srl_dispatch) ; actually (srl_dispatch,a)

.endproc

.proc srl_0
    rts             ;  0
.endproc

.proc srl_1
    lsr imm_3       ; (5)
    ror imm_2       ; (5)
    ror imm_1       ; (5)
    ror imm_0       ; (5)
    rts             ; 20
.endproc

.proc srl_2
    lsr imm_3       ; (5)
    ror imm_2       ; (5)
    ror imm_1       ; (5)
    ror imm_0       ; (5)
    lsr imm_3       ; (5)
    ror imm_2       ; (5)
    ror imm_1       ; (5)
    ror imm_0       ; (5)
    rts             ; 40
.endproc

.proc srl_3
    ldy imm_3       ; (3)
    lda rshift3,y   ; (4)
    sta imm_3       ; (3)
    lda lshift5,y   ; (4)
    ldy imm_2       ; (3)
    ora rshift3,y   ; (4)
    sta imm_2       ; (3)
    lda lshift5,y   ; (4)
    ldy imm_1       ; (3)
    ora rshift3,y   ; (4)
    sta imm_1       ; (3)
    lda lshift5,y   ; (4)
    ldy imm_0       ; (3)
    ora rshift3,y   ; (4)
    sta imm_0       ; (3)
    rts             ; 52
.endproc

.proc srl_4
    ldy imm_3       ; (3)
    lda rshift4,y   ; (4)
    sta imm_3       ; (3)
    lda lshift4,y   ; (4)
    ldy imm_2       ; (3)
    ora rshift4,y   ; (4)
    sta imm_2       ; (3)
    lda lshift4,y   ; (4)
    ldy imm_1       ; (3)
    ora rshift4,y   ; (4)
    sta imm_1       ; (3)
    lda lshift4,y   ; (4)
    ldy imm_0       ; (3)
    ora rshift4,y   ; (4)
    sta imm_0       ; (3)
    rts             ; 52
.endproc

.proc srl_5
    ldy imm_3       ; (3)
    lda rshift5,y   ; (4)
    sta imm_3       ; (3)
    lda lshift3,y   ; (4)
    ldy imm_2       ; (3)
    ora rshift5,y   ; (4)
    sta imm_2       ; (3)
    lda lshift3,y   ; (4)
    ldy imm_1       ; (3)
    ora rshift5,y   ; (4)
    sta imm_1       ; (3)
    lda lshift3,y   ; (4)
    ldy imm_0       ; (3)
    ora rshift5,y   ; (4)
    sta imm_0       ; (3)
    rts             ; 52
.endproc

.proc srl_6
    ldy imm_3       ; (3)
    lda rshift6,y   ; (4)
    sta imm_3       ; (3)
    lda lshift2,y   ; (4)
    ldy imm_2       ; (3)
    ora rshift6,y   ; (4)
    sta imm_2       ; (3)
    lda lshift2,y   ; (4)
    ldy imm_1       ; (3)
    ora rshift6,y   ; (4)
    sta imm_1       ; (3)
    lda lshift2,y   ; (4)
    ldy imm_0       ; (3)
    ora rshift6,y   ; (4)
    sta imm_0       ; (3)
    rts             ; 52
.endproc

;b37 b36 b35 b34 b33 b32 b31 b30|b27 b26 b25 b24 b23 b22 b21 b20|b17 b16 b15 b14 b13 b12 b11 b10|b07 b06 b05 b04 b03 b02 b01 b00
;  0   0   0   0   0   0   0 b37|b36 b35 b34 b33 b32 b31 b30 b27|b26 b25 b24 b23 b22 b21 b20 b17|b16 b15 b14 b13 b12 b11 b10 b07
.proc srl_7
    lda imm_0       ; (3) C=...  A=b07 b06 b05 b04 b03 b02 b01 b00
    asl a           ; (2) C=b07  A=b06 b05 b04 b03 b02 b01 b00   0
    lda imm_1       ; (3) C=b07  A=b17 b16 b15 b14 b13 b12 b11 b10
    rol a           ; (2) C=b17  A=b16 b15 b14 b13 b12 b11 b10 b07
    sta imm_0       ; (3)
    lda imm_2       ; (3) C=b17  A=b27 b26 b25 b24 b23 b22 b21 b20
    rol a           ; (2) C=b27  A=b26 b25 b24 b23 b22 b21 b20 b17
    sta imm_1       ; (3)
    lda imm_3       ; (3) C=b27  A=b37 b36 b35 b34 b33 b32 b31 b30
    rol a           ; (2) C=b37  A=b36 b35 b34 b33 b32 b31 b30 b27
    sta imm_2       ; (3)
    lda #0          ; (2) C=b37  A=  0   0   0   0   0   0   0   0
    rol a           ; (2) C=  0  A=  0   0   0   0   0   0   0 b37
    sta imm_3       ; (3)
    rts             ; 36
.endproc

.proc srl_8
    lda imm_1       ; (3)
    sta imm_0       ; (3)
    lda imm_2       ; (3)
    sta imm_1       ; (3)
    lda imm_3       ; (3)
    sta imm_2       ; (3)
    lda #0          ; (2)
    sta imm_3       ; (3)
    rts             ; 23
.endproc

.proc srl_9
    ldx imm_1       ; (3)
    ldy imm_2       ; (3)
    lda imm_3       ; (3)
    lsr a           ; (2)
    sta imm_2       ; (3)
    tya             ; (2)
    ror a           ; (2)
    sta imm_1       ; (3)
    txa             ; (2)
    ror a           ; (2)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_3       ; (3)
    rts             ; 33
.endproc

;b37 b36 b35 b34 b33 b32 b31 b30|b27 b26 b25 b24 b23 b22 b21 b20|b17 b16 b15 b14 b13 b12 b11 b10|b07 b06 b05 b04 b03 b02 b01 b00
;  0   0   0   0   0   0   0   0|  0   0 b37 b36 b35 b34 b33 b32|b31 b30 b27 b26 b25 b24 b23 b22|b21 b20 b17 b16 b15 b14 b13 b12
.proc srl_10
    ldy imm_3       ; (3) A=... ... ... ... ... ... ... ...  X=... ... ... ... ... ... ... ...  Y=b37 b36 b35 b34 b33 b32 b31 b30
    lda rshift2,y   ; (4) A=  0   0 b37 b36 b35 b34 b33 b32  X=... ... ... ... ... ... ... ...  Y=b37 b36 b35 b34 b33 b32 b31 b30
    ldx imm_2       ; (3) A=  0   0 b37 b36 b35 b34 b33 b32  X=b27 b26 b25 b24 b23 b22 b21 b20  Y=b37 b36 b35 b34 b33 b32 b31 b30
    sta imm_2       ; (3)
    lda lshift6,y   ; (4) A=b31 b30   0   0   0   0   0   0  X=b27 b26 b25 b24 b23 b22 b21 b20  Y=b37 b36 b35 b34 b33 b32 b31 b30
    ora rshift2,x   ; (4) A=b31 b30 b27 b26 b25 b24 b23 b22  X=b27 b26 b25 b24 b23 b22 b21 b20  Y=b37 b36 b35 b34 b33 b32 b31 b30
    ldy imm_1       ; (3) A=b31 b30 b27 b26 b25 b24 b23 b22  X=b27 b26 b25 b24 b23 b22 b21 b20  Y=b17 b16 b15 b14 b13 b12 b11 b10
    sta imm_1       ; (3)
    lda lshift6,x   ; (4) A=b21 b20   0   0   0   0   0   0  X=b27 b26 b25 b24 b23 b22 b21 b20  Y=b17 b16 b15 b14 b13 b12 b11 b10
    ora rshift2,y   ; (4) A=b21 b20 b17 b16 b15 b14 b13 b12  X=b27 b26 b25 b24 b23 b22 b21 b20  Y=b17 b16 b15 b14 b13 b12 b11 b10
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_3       ; (3)
    rts             ; 43
.endproc

.proc srl_11
    ldy imm_3       ; (3)
    lda rshift3,y   ; (4)
    ldx imm_2       ; (3)
    sta imm_2       ; (3)
    lda lshift5,y   ; (4)
    ora rshift3,x   ; (4)
    ldy imm_1       ; (3)
    sta imm_1       ; (3)
    lda lshift5,x   ; (4)
    ora rshift3,y   ; (4)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_3       ; (3)
    rts             ; 43
.endproc

.proc srl_12
    ldy imm_3       ; (3)
    lda rshift4,y   ; (4)
    ldx imm_2       ; (3)
    sta imm_2       ; (3)
    lda lshift4,y   ; (4)
    ora rshift4,x   ; (4)
    ldy imm_1       ; (3)
    sta imm_1       ; (3)
    lda lshift4,x   ; (4)
    ora rshift4,y   ; (4)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_3       ; (3)
    rts             ; 43
.endproc

.proc srl_13
    ldy imm_3       ; (3)
    lda rshift5,y   ; (4)
    ldx imm_2       ; (3)
    sta imm_2       ; (3)
    lda lshift3,y   ; (4)
    ora rshift5,x   ; (4)
    ldy imm_1       ; (3)
    sta imm_1       ; (3)
    lda lshift3,x   ; (4)
    ora rshift5,y   ; (4)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_3       ; (3)
    rts             ; 43
.endproc

.proc srl_14
    ldy imm_3       ; (3)
    lda rshift6,y   ; (4)
    ldx imm_2       ; (3)
    sta imm_2       ; (3)
    lda lshift2,y   ; (4)
    ora rshift6,x   ; (4)
    ldy imm_1       ; (3)
    sta imm_1       ; (3)
    lda lshift2,x   ; (4)
    ora rshift6,y   ; (4)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_3       ; (3)
    rts             ; 43
.endproc

;b37 b36 b35 b34 b33 b32 b31 b30|b27 b26 b25 b24 b23 b22 b21 b20|b17 b16 b15 b14 b13 b12 b11 b10|b07 b06 b05 b04 b03 b02 b01 b00
;  0   0   0   0   0   0   0   0|  0   0   0   0   0   0   0 b37|b36 b35 b34 b33 b32 b31 b30 b27|b26 b25 b24 b23 b22 b21 b20 b17
.proc srl_15
    lda imm_1       ; (3) C=...  A=b17 b16 b15 b14 b13 b12 b11 b10
    asl a           ; (2) C=b17  A=b16 b15 b14 b13 b12 b11 b10   0
    lda imm_2       ; (3) C=b17  A=b27 b26 b25 b24 b23 b22 b21 b20
    rol a           ; (2) C=b27  A=b26 b25 b24 b23 b22 b21 b20 b17
    sta imm_0       ; (3)
    lda imm_3       ; (3) C=b27  A=b37 b36 b35 b34 b33 b32 b31 b30
    rol a           ; (2) C=b37  A=b36 b35 b34 b33 b32 b31 b30 b27
    sta imm_1       ; (3)
    lda #0          ; (2) C=b37  A=  0   0   0   0   0   0   0   0
    sta imm_3       ; (3)
    rol a           ; (2) C=  0  A=  0   0   0   0   0   0   0 b37
    sta imm_2       ; (3)
    rts             ; 31
.endproc

.proc srl_16
    lda imm_2       ; (3)
    sta imm_0       ; (3)
    lda imm_3       ; (3)
    sta imm_1       ; (3)
    lda #0          ; (2)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 20
.endproc

.proc srl_17
    lda imm_3       ; (3)
    lsr a           ; (2)
    sta imm_1       ; (3)
    lda imm_2       ; (3)
    ror a           ; (2)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 24
.endproc

.proc srl_18
    lda imm_3       ; (3)
    lsr a           ; (2)
    ror imm_2       ; (5)
    lsr a           ; (2)
    sta imm_1       ; (3)
    lda imm_2       ; (3)
    ror a           ; (2)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 31
.endproc

.proc srl_19
    ldx imm_3       ; (3)
    lda rshift3,x   ; (4)
    sta imm_1       ; (3)
    lda lshift5,x   ; (4)
    ldx imm_2       ; (3)
    ora rshift3,x   ; (4)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 32
.endproc

.proc srl_20
    ldx imm_3       ; (3)
    lda rshift4,x   ; (4)
    sta imm_1       ; (3)
    lda lshift4,x   ; (4)
    ldx imm_2       ; (3)
    ora rshift4,x   ; (4)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 32
.endproc

.proc srl_21
    ldx imm_3       ; (3)
    lda rshift5,x   ; (4)
    sta imm_1       ; (3)
    lda lshift3,x   ; (4)
    ldx imm_2       ; (3)
    ora rshift5,x   ; (4)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 32
.endproc

.proc srl_22
    ldx imm_3       ; (3)
    lda rshift6,x   ; (4)
    sta imm_1       ; (3)
    lda lshift2,x   ; (4)
    ldx imm_2       ; (3)
    ora rshift6,x   ; (4)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 32
.endproc

.proc srl_23
    lda imm_2       ; (3)
    asl a           ; (2)
    lda imm_3       ; (3)
    rol a           ; (2)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rol a           ; (2)
    sta imm_1       ; (3)
    rts             ; 26
.endproc

.proc srl_24
    lda imm_3       ; (3)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_1       ; (3)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 17
.endproc

.proc srl_25
    lda imm_3       ; (3)
    lsr a           ; (2)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_1       ; (3)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 19
.endproc

.proc srl_26
    lda imm_3       ; (3)
    lsr a           ; (2)
    lsr a           ; (2)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_1       ; (3)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 21
.endproc

.proc srl_27
    ldx imm_3       ; (3)
    lda rshift3,x   ; (4)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_1       ; (3)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 21
.endproc

.proc srl_28
    ldx imm_3       ; (3)
    lda rshift4,x   ; (4)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_1       ; (3)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 21
.endproc

.proc srl_29
    ldx imm_3       ; (3)
    lda rshift5,x   ; (4)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_1       ; (3)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 21
.endproc

.proc srl_30
    ldx imm_3       ; (3)
    lda rshift6,x   ; (4)
    sta imm_0       ; (3)
    lda #0          ; (2)
    sta imm_1       ; (3)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 21
.endproc

.proc srl_31
    lda imm_3       ; (3)
    asl a           ; (2)
    lda #0          ; (2)
    sta imm_1       ; (3)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rol a           ; (2)
    sta imm_0       ; (3)
    rts             ; 21
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Shift the contents of imm to the right and fill with the sign bit
; X contains the number of bits to shift right
.proc shift_right_signed

    txa
    asl a
    sta jump+1      ; self-modifying code here
    jump:
    jmp (sra_dispatch) ; actually (sra_dispatch,a)

.endproc

.proc sra_0
    rts             ;  0
.endproc

.proc sra_1
    lda imm_3       ; (3)
    cmp #$80        ; (2)
    ror a           ; (2)
    ror imm_2       ; (5)
    ror imm_1       ; (5)
    ror imm_0       ; (5)
    sta imm_3       ; (3)
    rts             ; 25
.endproc

.proc sra_2
    lda imm_3       ; (3)
    cmp #$80        ; (2)
    ror a           ; (2)
    ror imm_2       ; (5)
    ror imm_1       ; (5)
    ror imm_0       ; (5)
    cmp #$80        ; (2)
    ror a           ; (2)
    ror imm_2       ; (5)
    ror imm_1       ; (5)
    ror imm_0       ; (5)
    sta imm_3       ; (3)
    rts             ; 44
.endproc

.proc sra_3
    ldy imm_3       ; (3)
    lda rsshift3,y  ; (4)
    sta imm_3       ; (3)
    lda lshift5,y   ; (4)
    ldy imm_2       ; (3)
    ora rshift3,y   ; (4)
    sta imm_2       ; (3)
    lda lshift5,y   ; (4)
    ldy imm_1       ; (3)
    ora rshift3,y   ; (4)
    sta imm_1       ; (3)
    lda lshift5,y   ; (4)
    ldy imm_0       ; (3)
    ora rshift3,y   ; (4)
    sta imm_0       ; (3)
    rts             ; 52
.endproc

.proc sra_4
    ldy imm_3       ; (3)
    lda rsshift4,y  ; (4)
    sta imm_3       ; (3)
    lda lshift4,y   ; (4)
    ldy imm_2       ; (3)
    ora rshift4,y   ; (4)
    sta imm_2       ; (3)
    lda lshift4,y   ; (4)
    ldy imm_1       ; (3)
    ora rshift4,y   ; (4)
    sta imm_1       ; (3)
    lda lshift4,y   ; (4)
    ldy imm_0       ; (3)
    ora rshift4,y   ; (4)
    sta imm_0       ; (3)
    rts             ; 52
.endproc

.proc sra_5
    ldy imm_3       ; (3)
    lda rsshift5,y  ; (4)
    sta imm_3       ; (3)
    lda lshift3,y   ; (4)
    ldy imm_2       ; (3)
    ora rshift5,y   ; (4)
    sta imm_2       ; (3)
    lda lshift3,y   ; (4)
    ldy imm_1       ; (3)
    ora rshift5,y   ; (4)
    sta imm_1       ; (3)
    lda lshift3,y   ; (4)
    ldy imm_0       ; (3)
    ora rshift5,y   ; (4)
    sta imm_0       ; (3)
    rts             ; 52
.endproc

.proc sra_6
    ldy imm_3       ; (3)
    lda rsshift6,y  ; (4)
    sta imm_3       ; (3)
    lda lshift2,y   ; (4)
    ldy imm_2       ; (3)
    ora rshift6,y   ; (4)
    sta imm_2       ; (3)
    lda lshift2,y   ; (4)
    ldy imm_1       ; (3)
    ora rshift6,y   ; (4)
    sta imm_1       ; (3)
    lda lshift2,y   ; (4)
    ldy imm_0       ; (3)
    ora rshift6,y   ; (4)
    sta imm_0       ; (3)
    rts             ; 52
.endproc

;b37 b36 b35 b34 b33 b32 b31 b30|b27 b26 b25 b24 b23 b22 b21 b20|b17 b16 b15 b14 b13 b12 b11 b10|b07 b06 b05 b04 b03 b02 b01 b00
;b37 b37 b37 b37 b37 b37 b37 b37|b36 b35 b34 b33 b32 b31 b30 b27|b26 b25 b24 b23 b22 b21 b20 b17|b16 b15 b14 b13 b12 b11 b10 b07
.proc sra_7
    lda imm_0       ; (3) C=...  A=b07 b06 b05 b04 b03 b02 b01 b00
    asl a           ; (2) C=b07  A=b06 b05 b04 b03 b02 b01 b00   0
    lda imm_1       ; (3) C=b07  A=b17 b16 b15 b14 b13 b12 b11 b10
    rol a           ; (2) C=b17  A=b16 b15 b14 b13 b12 b11 b10 b07
    sta imm_0       ; (3)
    lda imm_2       ; (3) C=b17  A=b27 b26 b25 b24 b23 b22 b21 b20
    rol a           ; (2) C=b27  A=b26 b25 b24 b23 b22 b21 b20 b17
    sta imm_1       ; (3)
    lda imm_3       ; (3) C=b27  A=b37 b36 b35 b34 b33 b32 b31 b30
    rol a           ; (2) C=b37  A=b36 b35 b34 b33 b32 b31 b30 b27
    sta imm_2       ; (3)
    lda #0          ; (2) C=b37  A=  0   0   0   0   0   0   0   0
    sbc #0          ; (2) C=b37  A=~b37 * 8
    eor #$FF        ; (2) C=b37  A=b37 b37 b37 b37 b37 b37 b37 b37
    sta imm_3       ; (3)
    rts             ; 38
.endproc

.proc sra_8
    lda imm_1       ; (3)
    sta imm_0       ; (3)
    lda imm_2       ; (3)
    sta imm_1       ; (3)
    lda imm_3       ; (3)
    sta imm_2       ; (3)
    cmp #$80        ; (2)
    lda #0          ; (2)
    sbc #0          ; (2)
    eor #$FF        ; (2)
    sta imm_3       ; (3)
    rts             ; 29
.endproc

.proc sra_9
    ldx imm_1       ; (3)
    ldy imm_2       ; (3)
    lda imm_3       ; (3)
    cmp #$80        ; (2)
    ror a           ; (2)
    sta imm_2       ; (3)
    tya             ; (2)
    ror a           ; (2)
    sta imm_1       ; (3)
    txa             ; (2)
    ror a           ; (2)
    sta imm_0       ; (3)
    lda imm_2       ; (3)
    cmp #$80        ; (2)
    lda #0          ; (2)
    sbc #0          ; (2)
    eor #$FF        ; (2)
    sta imm_3       ; (3)
    rts             ; 44
.endproc

;b37 b36 b35 b34 b33 b32 b31 b30|b27 b26 b25 b24 b23 b22 b21 b20|b17 b16 b15 b14 b13 b12 b11 b10|b07 b06 b05 b04 b03 b02 b01 b00
;b37 b37 b37 b37 b37 b37 b37 b37|b37 b37 b37 b36 b35 b34 b33 b32|b31 b30 b27 b26 b25 b24 b23 b22|b21 b20 b17 b16 b15 b14 b13 b12
.proc sra_10
    ldy imm_3       ; (3) A=... ... ... ... ... ... ... ...  X=... ... ... ... ... ... ... ...  Y=b37 b36 b35 b34 b33 b32 b31 b30
    lda rsshift2,y  ; (4) A=b37 b37 b37 b36 b35 b34 b33 b32  X=... ... ... ... ... ... ... ...  Y=b37 b36 b35 b34 b33 b32 b31 b30
    ldx imm_2       ; (3) A=b37 b37 b37 b36 b35 b34 b33 b32  X=b27 b26 b25 b24 b23 b22 b21 b20  Y=b37 b36 b35 b34 b33 b32 b31 b30
    sta imm_2       ; (3)
    cmp #$80        ; (2)
    lda #0          ; (2)
    sbc #0          ; (2)
    eor #$FF        ; (2)
    sta imm_3       ; (3)
    lda lshift6,y   ; (4) A=b31 b30   0   0   0   0   0   0  X=b27 b26 b25 b24 b23 b22 b21 b20  Y=b37 b36 b35 b34 b33 b32 b31 b30
    ora rshift2,x   ; (4) A=b31 b30 b27 b26 b25 b24 b23 b22  X=b27 b26 b25 b24 b23 b22 b21 b20  Y=b37 b36 b35 b34 b33 b32 b31 b30
    ldy imm_1       ; (3) A=b31 b30 b27 b26 b25 b24 b23 b22  X=b27 b26 b25 b24 b23 b22 b21 b20  Y=b17 b16 b15 b14 b13 b12 b11 b10
    sta imm_1       ; (3)
    lda lshift6,x   ; (4) A=b21 b20   0   0   0   0   0   0  X=b27 b26 b25 b24 b23 b22 b21 b20  Y=b17 b16 b15 b14 b13 b12 b11 b10
    ora rshift2,y   ; (4) A=b21 b20 b17 b16 b15 b14 b13 b12  X=b27 b26 b25 b24 b23 b22 b21 b20  Y=b17 b16 b15 b14 b13 b12 b11 b10
    sta imm_0       ; (3)
    rts             ; 49
.endproc

.proc sra_11
    ldy imm_3       ; (3)
    lda rsshift3,y  ; (4)
    ldx imm_2       ; (3)
    sta imm_2       ; (3)
    cmp #$80        ; (2)
    lda #0          ; (2)
    sbc #0          ; (2)
    eor #$FF        ; (2)
    sta imm_3       ; (3)
    lda lshift5,y   ; (4)
    ora rshift3,x   ; (4)
    ldy imm_1       ; (3)
    sta imm_1       ; (3)
    lda lshift5,x   ; (4)
    ora rshift3,y   ; (4)
    sta imm_0       ; (3)
    rts             ; 49
.endproc

.proc sra_12
    ldy imm_3       ; (3)
    lda rsshift4,y  ; (4)
    ldx imm_2       ; (3)
    sta imm_2       ; (3)
    cmp #$80        ; (2)
    lda #0          ; (2)
    sbc #0          ; (2)
    eor #$FF        ; (2)
    sta imm_3       ; (3)
    lda lshift4,y   ; (4)
    ora rshift4,x   ; (4)
    ldy imm_1       ; (3)
    sta imm_1       ; (3)
    lda lshift4,x   ; (4)
    ora rshift4,y   ; (4)
    sta imm_0       ; (3)
    rts             ; 49
.endproc

.proc sra_13
    ldy imm_3       ; (3)
    lda rsshift5,y  ; (4)
    ldx imm_2       ; (3)
    sta imm_2       ; (3)
    cmp #$80        ; (2)
    lda #0          ; (2)
    sbc #0          ; (2)
    eor #$FF        ; (2)
    sta imm_3       ; (3)
    lda lshift3,y   ; (4)
    ora rshift5,x   ; (4)
    ldy imm_1       ; (3)
    sta imm_1       ; (3)
    lda lshift3,x   ; (4)
    ora rshift5,y   ; (4)
    sta imm_0       ; (3)
    rts             ; 49
.endproc

.proc sra_14
    ldy imm_3       ; (3)
    lda rsshift6,y  ; (4)
    ldx imm_2       ; (3)
    sta imm_2       ; (3)
    cmp #$80        ; (2)
    lda #0          ; (2)
    sbc #0          ; (2)
    eor #$FF        ; (2)
    sta imm_3       ; (3)
    lda lshift2,y   ; (4)
    ora rshift6,x   ; (4)
    ldy imm_1       ; (3)
    sta imm_1       ; (3)
    lda lshift2,x   ; (4)
    ora rshift6,y   ; (4)
    sta imm_0       ; (3)
    rts             ; 49
.endproc

;b37 b36 b35 b34 b33 b32 b31 b30|b27 b26 b25 b24 b23 b22 b21 b20|b17 b16 b15 b14 b13 b12 b11 b10|b07 b06 b05 b04 b03 b02 b01 b00
;b37 b37 b37 b37 b37 b37 b37 b37|b37 b37 b37 b37 b37 b37 b37 b37|b36 b35 b34 b33 b32 b31 b30 b27|b26 b25 b24 b23 b22 b21 b20 b17
.proc sra_15
    lda imm_1       ; (3) C=...  A=b17 b16 b15 b14 b13 b12 b11 b10
    asl a           ; (2) C=b17  A=b16 b15 b14 b13 b12 b11 b10   0
    lda imm_2       ; (3) C=b17  A=b27 b26 b25 b24 b23 b22 b21 b20
    rol a           ; (2) C=b27  A=b26 b25 b24 b23 b22 b21 b20 b17
    sta imm_0       ; (3)
    lda imm_3       ; (3) C=b27  A=b37 b36 b35 b34 b33 b32 b31 b30
    rol a           ; (2) C=b37  A=b36 b35 b34 b33 b32 b31 b30 b27
    sta imm_1       ; (3)
    lda #0          ; (2) C=b37  A=  0   0   0   0   0   0   0   0
    sbc #0          ; (2) C=b37  A=~b37 * 8
    eor #$FF        ; (2) C=b37  A=b37 b37 b37 b37 b37 b37 b37 b37
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 33
.endproc

.proc sra_16
    lda imm_2       ; (3)
    sta imm_0       ; (3)
    lda imm_3       ; (3)
    sta imm_1       ; (3)
    cmp #$80        ; (2)
    lda #0          ; (2)
    sbc #0          ; (2)
    eor #$FF        ; (2)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 26
.endproc

.proc sra_17
    lda imm_3       ; (3)
    cmp #$80        ; (2)
    ror a           ; (2)
    sta imm_1       ; (3)
    lda imm_2       ; (3)
    ror a           ; (2)
    sta imm_0       ; (3)
    lda imm_1       ; (3)
    cmp #$80        ; (2)
    lda #0          ; (2)
    sbc #0          ; (2)
    eor #$FF        ; (2)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 35
.endproc

.proc sra_18
    ldx imm_3       ; (3)
    ldy rsshift2,x  ; (4)
    sty imm_1       ; (3)
    lda lshift6,x   ; (4)
    ldx imm_2       ; (3)
    ora rshift2,x   ; (4)
    sta imm_0       ; (3)
    lda rsshift6,y  ; (4)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 34
.endproc

.proc sra_19
    ldx imm_3       ; (3)
    ldy rsshift3,x  ; (4)
    sty imm_1       ; (3)
    lda lshift5,x   ; (4)
    ldx imm_2       ; (3)
    ora rshift3,x   ; (4)
    sta imm_0       ; (3)
    lda rsshift5,y  ; (4)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 34
.endproc

.proc sra_20
    ldx imm_3       ; (3)
    ldy rsshift4,x  ; (4)
    sty imm_1       ; (3)
    lda lshift4,x   ; (4)
    ldx imm_2       ; (3)
    ora rshift4,x   ; (4)
    sta imm_0       ; (3)
    lda rsshift4,y  ; (4)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 34
.endproc

.proc sra_21
    ldx imm_3       ; (3)
    ldy rsshift5,x  ; (4)
    sty imm_1       ; (3)
    lda lshift3,x   ; (4)
    ldx imm_2       ; (3)
    ora rshift5,x   ; (4)
    sta imm_0       ; (3)
    lda rsshift3,y  ; (4)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 34
.endproc

.proc sra_22
    ldx imm_3       ; (3)
    ldy rsshift6,x  ; (4)
    sty imm_1       ; (3)
    lda lshift2,x   ; (4)
    ldx imm_2       ; (3)
    ora rshift6,x   ; (4)
    sta imm_0       ; (3)
    lda rsshift2,y  ; (4)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 34
.endproc

;b37 b36 b35 b34 b33 b32 b31 b30|b27 b26 b25 b24 b23 b22 b21 b20|b17 b16 b15 b14 b13 b12 b11 b10|b07 b06 b05 b04 b03 b02 b01 b00
;b37 b37 b37 b37 b37 b37 b37 b37|b37 b37 b37 b37 b37 b37 b37 b37|b37 b37 b37 b37 b37 b37 b37 b37|b36 b35 b34 b33 b32 b31 b30 b27
.proc sra_23
    lda imm_2       ; (3) C=...  A=b27 b26 b25 b24 b23 b22 b21 b20
    asl a           ; (2) C=b27  A=b26 b25 b24 b23 b22 b21 b20   0
    lda imm_3       ; (3) C=b27  A=b37 b36 b35 b34 b33 b32 b31 b30
    rol a           ; (2) C=b37  A=b36 b35 b34 b33 b32 b31 b30 b27
    sta imm_0       ; (3)
    lda #0          ; (2) C=b37  A=  0   0   0   0   0   0   0   0
    sbc #0          ; (2) C=b37  A=~b37 * 8
    eor #$FF        ; (2) C=b37  A=b37 b37 b37 b37 b37 b37 b37 b37
    sta imm_1       ; (3)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 28
.endproc

.proc sra_24
    lda imm_3       ; (3)
    sta imm_0       ; (3)
    cmp #$80        ; (2)
    lda #0          ; (2)
    sbc #0          ; (2)
    eor #$FF        ; (2)
    sta imm_1       ; (3)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 23
.endproc

.proc sra_25
    lda imm_3       ; (3)
    cmp #$80        ; (2)
    ror a           ; (2)
    sta imm_0       ; (3)
    cmp #$80        ; (2)
    lda #0          ; (2)
    sbc #0          ; (2)
    eor #$FF        ; (2)
    sta imm_1       ; (3)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 27
.endproc

.proc sra_26
    ldx imm_3       ; (3)
    ldy rsshift2,x  ; (4)
    sty imm_0       ; (3)
    lda rsshift6,y  ; (4)
    sta imm_1       ; (3)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 23
.endproc

.proc sra_27
    ldx imm_3       ; (3)
    ldy rsshift3,x  ; (4)
    sty imm_0       ; (3)
    lda rsshift5,y  ; (4)
    sta imm_1       ; (3)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 23
.endproc

.proc sra_28
    ldx imm_3       ; (3)
    ldy rsshift4,x  ; (4)
    sty imm_0       ; (3)
    lda rsshift4,y  ; (4)
    sta imm_1       ; (3)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 23
.endproc

.proc sra_29
    ldx imm_3       ; (3)
    ldy rsshift5,x  ; (4)
    sty imm_0       ; (3)
    lda rsshift3,y  ; (4)
    sta imm_1       ; (3)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 23
.endproc

.proc sra_30
    ldx imm_3       ; (3)
    ldy rsshift6,x  ; (4)
    sty imm_0       ; (3)
    lda rsshift2,y  ; (4)
    sta imm_1       ; (3)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 23
.endproc

.proc sra_31
    lda imm_3       ; (3)
    cmp #$80        ; (2)
    lda #0          ; (2)
    sbc #0          ; (2)
    eor #$FF        ; (2)
    sta imm_0       ; (3)
    sta imm_1       ; (3)
    sta imm_2       ; (3)
    sta imm_3       ; (3)
    rts             ; 23
.endproc

.if RV32M

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         32 bit multiply algorithms                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Multiply signed mul_op1 by signed mul_op2
; Return 64 bit result in mul_prod

.proc multiply_signed

    ; Count ones in mul_op1. If many, take the two's complement and use
    ; a*b = 2**32*a - a*(2**32-b)
    clc
    ldx mul_op1_0
    lda ones_count,x
    ldx mul_op1_1
    adc ones_count,x
    ; Adds don't overflow; it is not necessary to clear the carry flag
    ldx mul_op1_2
    adc ones_count,x
    ldx mul_op1_3
    adc ones_count,x
    ldy #0
    cmp #18
    bcc end_op1
    minus_op1:
        sec
        lda #0
        sbc mul_op1_0
        sta mul_op1_0
        lda #0
        sbc mul_op1_1
        sta mul_op1_1
        lda #0
        sbc mul_op1_2
        sta mul_op1_2
        lda #0
        sbc mul_op1_3
        sta mul_op1_3
        iny
    end_op1:
    sty muldiv_scratch2_5

    ; Extend mul_op2 to 48 bits
    lda mul_op2_3
    cmp #$80
    lda #0
    sbc #0
    eor #$FF
    sta muldiv_scratch2_4
    tay

    ; Clear the product
    lda #0
    sta mul_prod_0
    sta mul_prod_1
    sta mul_prod_2
    sta mul_prod_3
    sta mul_prod_4
    sta mul_prod_5
    sta mul_prod_6
    sta mul_prod_7

    ; We test bits 0, 8, 16 and 24 of muldiv_scratch1 and byte-shift
    ; muldiv_scratch2 as appropriate. By doing this, we only need to shift
    ; eight times, though we are multiplying 32 by 32 bits.

    ; On the first pass, mul_prod is 0, and we can skip the add

    ; muldiv_scratch1 bit 0, byte shift 0
    lsr mul_op1_0
    bcc @skip_add
        lda mul_op2_0
        sta mul_prod_0
        lda mul_op2_1
        sta mul_prod_1
        lda mul_op2_2
        sta mul_prod_2
        lda mul_op2_3
        sta mul_prod_3
        lda muldiv_scratch2_4
        sta mul_prod_4
        sta mul_prod_5
        sta mul_prod_6
        sta mul_prod_7
    @skip_add:

cheap_label_scope1:

    ldx #8
    bne enter_mul_loop
    mul_loop:

        ; muldiv_scratch1 bit 0, byte shift 0
        ; optimized above on first pass
        lsr mul_op1_0
        bcc @skip_add
            clc
            lda mul_prod_0
            adc mul_op2_0
            sta mul_prod_0
            lda mul_prod_1
            adc mul_op2_1
            sta mul_prod_1
            lda mul_prod_2
            adc mul_op2_2
            sta mul_prod_2
            lda mul_prod_3
            adc mul_op2_3
            sta mul_prod_3
            lda mul_prod_4
            adc muldiv_scratch2_4
            sta mul_prod_4
            tya
            adc mul_prod_5
            sta mul_prod_5
            tya
            adc mul_prod_6
            sta mul_prod_6
            tya
            adc mul_prod_7
            sta mul_prod_7
        @skip_add:

    enter_mul_loop:

        ; muldiv_scratch1 bit 8, byte shift 1
        lsr mul_op1_1
        bcc @skip_add
            clc
            lda mul_prod_1
            adc mul_op2_0
            sta mul_prod_1
            lda mul_prod_2
            adc mul_op2_1
            sta mul_prod_2
            lda mul_prod_3
            adc mul_op2_2
            sta mul_prod_3
            lda mul_prod_4
            adc mul_op2_3
            sta mul_prod_4
            lda mul_prod_5
            adc muldiv_scratch2_4
            sta mul_prod_5
            tya
            adc mul_prod_6
            sta mul_prod_6
            tya
            adc mul_prod_7
            sta mul_prod_7
        @skip_add:

cheap_label_scope2:

        ; muldiv_scratch1 bit 16, byte shift 2
        lsr mul_op1_2
        bcc @skip_add
            clc
            lda mul_prod_2
            adc mul_op2_0
            sta mul_prod_2
            lda mul_prod_3
            adc mul_op2_1
            sta mul_prod_3
            lda mul_prod_4
            adc mul_op2_2
            sta mul_prod_4
            lda mul_prod_5
            adc mul_op2_3
            sta mul_prod_5
            lda mul_prod_6
            adc muldiv_scratch2_4
            sta mul_prod_6
            tya
            adc mul_prod_7
            sta mul_prod_7
        @skip_add:

cheap_label_scope3:

        ; Exit the loop here on the eighth pass; we'll subtract muldiv_scratch2,
        ; because the place value of the bit in mul_op1 is negative
        dex
        beq end_mul_loop

        ; muldiv_scratch1 bit 24, byte shift 3
        lsr mul_op1_3
        bcc @skip_add
            clc
            lda mul_prod_3
            adc mul_op2_0
            sta mul_prod_3
            lda mul_prod_4
            adc mul_op2_1
            sta mul_prod_4
            lda mul_prod_5
            adc mul_op2_2
            sta mul_prod_5
            lda mul_prod_6
            adc mul_op2_3
            sta mul_prod_6
            lda mul_prod_7
            adc muldiv_scratch2_4
            sta mul_prod_7
        @skip_add:

    asl mul_op2_0
    rol mul_op2_1
    rol mul_op2_2
    rol mul_op2_3
    rol muldiv_scratch2_4

    jmp mul_loop
    end_mul_loop:

    ; Include sign bit and adjust product if needed
    lda muldiv_scratch2_5
    bne end_mul
        lsr mul_op1_3
        bcc sign_bit_1
            ; Few one bits, mul_op1 negative
            ; mul_prod <- mul_prod - (mul_op2 << 31)
            sec
            lda mul_prod_3
            sbc mul_op2_0
            sta mul_prod_3
            lda mul_prod_4
            sbc mul_op2_1
            sta mul_prod_4
            lda mul_prod_5
            sbc mul_op2_2
            sta mul_prod_5
            lda mul_prod_6
            sbc mul_op2_3
            sta mul_prod_6
            lda mul_prod_7
            sbc muldiv_scratch2_4
            sta mul_prod_7
        sign_bit_1:
        ; else:
        ;   Few one bits, mul_op1 positive
        ;   Leave mul_prod unchanged
        rts
    end_mul:
        lsr mul_op1_3
        bcc sign_bit_2
            ; Many one bits, mul_op1 positive (muldiv_scratch1 negative)
            ; mul_prod <- (mul_op2 << 31) - mul_prod
            sec
            lda #0
            sbc mul_prod_0
            sta mul_prod_0
            lda #0
            sbc mul_prod_1
            sta mul_prod_1
            lda #0
            sbc mul_prod_2
            sta mul_prod_2
            lda mul_op2_0
            sbc mul_prod_3
            sta mul_prod_3
            lda mul_op2_1
            sbc mul_prod_4
            sta mul_prod_4
            lda mul_op2_2
            sbc mul_prod_5
            sta mul_prod_5
            lda mul_op2_3
            sbc mul_prod_6
            sta mul_prod_6
            lda muldiv_scratch2_4
            sbc mul_prod_7
            sta mul_prod_7
            rts
        sign_bit_2:
            ; Many one bits, mul_op1 negative (muldiv_scratch1 positive)
            ; mul_prod <- (1 << 64) - mul_prod
            sec
            lda #0
            sbc mul_prod_0
            sta mul_prod_0
            lda #0
            sbc mul_prod_1
            sta mul_prod_1
            lda #0
            sbc mul_prod_2
            sta mul_prod_2
            lda #0
            sbc mul_prod_3
            sta mul_prod_3
            lda #0
            sbc mul_prod_4
            sta mul_prod_4
            lda #0
            sbc mul_prod_5
            sta mul_prod_5
            lda #0
            sbc mul_prod_6
            sta mul_prod_6
            lda #0
            sbc mul_prod_7
            sta mul_prod_7
            rts

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Multiply signed mul_op1 by unsigned mul_op2
; Return 64 bit result in mul_prod

.proc multiply_signed_unsigned

    ; Count ones in mul_op1. If many, take the two's complement and use
    ; a*b = 2**32*a - a*(2**32-b)
    clc
    ldx mul_op1_0
    lda ones_count,x
    ldx mul_op1_1
    adc ones_count,x
    ; Adds don't overflow; it is not necessary to clear the carry flag
    ldx mul_op1_2
    adc ones_count,x
    ldx mul_op1_3
    adc ones_count,x
    ldy #0
    cmp #18
    bcc end_op1
        sec
        lda #0
        sbc mul_op1_0
        sta mul_op1_0
        lda #0
        sbc mul_op1_1
        sta mul_op1_1
        lda #0
        sbc mul_op1_2
        sta mul_op1_2
        lda #0
        sbc mul_op1_3
        sta mul_op1_3
        iny
    end_op1:

    ; Extend mul_op2 to 40 bits
    lda #0
    sta muldiv_scratch2_4

    ; Clear the product
    sta mul_prod_0
    sta mul_prod_1
    sta mul_prod_2
    sta mul_prod_3
    sta mul_prod_4
    sta mul_prod_5
    sta mul_prod_6
    sta mul_prod_7

    ; We test bits 0, 8, 16 and 24 of muldiv_scratch1 and byte-shift
    ; muldiv_scratch2 as appropriate. By doing this, we only need to shift
    ; eight times, though we are multiplying 32 by 32 bits.

    ; muldiv_scratch1 bit 0, byte shift 0
    lsr mul_op1_0
    bcc @skip_add
        lda mul_op2_0
        sta mul_prod_0
        lda mul_op2_1
        sta mul_prod_1
        lda mul_op2_2
        sta mul_prod_2
        lda mul_op2_3
        sta mul_prod_3
    @skip_add:

    ldx #8
    bne enter_mul_loop
    mul_loop:

        asl mul_op2_0
        rol mul_op2_1
        rol mul_op2_2
        rol mul_op2_3
        rol muldiv_scratch2_4

        ; muldiv_scratch1 bit 0, byte shift 0
        lsr mul_op1_0
        bcc @skip_add
            clc
            lda mul_prod_0
            adc mul_op2_0
            sta mul_prod_0
            lda mul_prod_1
            adc mul_op2_1
            sta mul_prod_1
            lda mul_prod_2
            adc mul_op2_2
            sta mul_prod_2
            lda mul_prod_3
            adc mul_op2_3
            sta mul_prod_3
            lda mul_prod_4
            adc muldiv_scratch2_4
            sta mul_prod_4
            bcc @skip_add
            inc mul_prod_5
            bne @skip_add
            inc mul_prod_6
            bne @skip_add
            inc mul_prod_7
        @skip_add:

    enter_mul_loop:

        ; muldiv_scratch1 bit 8, byte shift 1
        lsr mul_op1_1
        bcc @skip_add
            clc
            lda mul_prod_1
            adc mul_op2_0
            sta mul_prod_1
            lda mul_prod_2
            adc mul_op2_1
            sta mul_prod_2
            lda mul_prod_3
            adc mul_op2_2
            sta mul_prod_3
            lda mul_prod_4
            adc mul_op2_3
            sta mul_prod_4
            lda mul_prod_5
            adc muldiv_scratch2_4
            sta mul_prod_5
            bcc @skip_add
            inc mul_prod_6
            bne @skip_add
            inc mul_prod_7
        @skip_add:

cheap_label_scope1:

        ; muldiv_scratch1 bit 16, byte shift 2
        lsr mul_op1_2
        bcc @skip_add
            clc
            lda mul_prod_2
            adc mul_op2_0
            sta mul_prod_2
            lda mul_prod_3
            adc mul_op2_1
            sta mul_prod_3
            lda mul_prod_4
            adc mul_op2_2
            sta mul_prod_4
            lda mul_prod_5
            adc mul_op2_3
            sta mul_prod_5
            lda mul_prod_6
            adc muldiv_scratch2_4
            sta mul_prod_6
            bcc @skip_add
            inc mul_prod_7
        @skip_add:

cheap_label_scope2:

        ; Exit the loop here on the eighth pass; we'll subtract muldiv_scratch2,
        ; because the place value of the bit in mul_op1 is negative
        dex
        beq end_mul_loop

        ; muldiv_scratch1 bit 24, byte shift 3
        lsr mul_op1_3
        bcc @skip_add
            clc
            lda mul_prod_3
            adc mul_op2_0
            sta mul_prod_3
            lda mul_prod_4
            adc mul_op2_1
            sta mul_prod_4
            lda mul_prod_5
            adc mul_op2_2
            sta mul_prod_5
            lda mul_prod_6
            adc mul_op2_3
            sta mul_prod_6
            lda mul_prod_7
            adc muldiv_scratch2_4
            sta mul_prod_7
        @skip_add:

    jmp mul_loop
    end_mul_loop:

    ; Include sign bit and adjust product if needed
    cpy #0
    bne end_mul
        lsr mul_op1_3
        bcc sign_bit_1
            ; Few one bits, mul_op1 negative
            ; mul_prod <- mul_prod - (mul_op2 << 31)
            sec
            lda mul_prod_3
            sbc mul_op2_0
            sta mul_prod_3
            lda mul_prod_4
            sbc mul_op2_1
            sta mul_prod_4
            lda mul_prod_5
            sbc mul_op2_2
            sta mul_prod_5
            lda mul_prod_6
            sbc mul_op2_3
            sta mul_prod_6
            lda mul_prod_7
            sbc muldiv_scratch2_4
            sta mul_prod_7
        sign_bit_1:
        ; else:
        ;   Few one bits, mul_op1 positive
        ;   Leave mul_prod unchanged
        rts
    end_mul:
        lsr mul_op1_3
        bcc sign_bit_2
            ; Many one bits, mul_op1 positive (muldiv_scratch1 negative)
            ; mul_prod <- (mul_op2 << 31) - mul_prod
            sec
            lda #0
            sbc mul_prod_0
            sta mul_prod_0
            lda #0
            sbc mul_prod_1
            sta mul_prod_1
            lda #0
            sbc mul_prod_2
            sta mul_prod_2
            lda mul_op2_0
            sbc mul_prod_3
            sta mul_prod_3
            lda mul_op2_1
            sbc mul_prod_4
            sta mul_prod_4
            lda mul_op2_2
            sbc mul_prod_5
            sta mul_prod_5
            lda mul_op2_3
            sbc mul_prod_6
            sta mul_prod_6
            lda muldiv_scratch2_4
            sbc mul_prod_7
            sta mul_prod_7
            rts
        sign_bit_2:
            ; Many one bits, mul_op1 negative (muldiv_scratch1 positive)
            ; mul_prod <- (1 << 64) - mul_prod
            sec
            lda #0
            sbc mul_prod_0
            sta mul_prod_0
            lda #0
            sbc mul_prod_1
            sta mul_prod_1
            lda #0
            sbc mul_prod_2
            sta mul_prod_2
            lda #0
            sbc mul_prod_3
            sta mul_prod_3
            lda #0
            sbc mul_prod_4
            sta mul_prod_4
            lda #0
            sbc mul_prod_5
            sta mul_prod_5
            lda #0
            sbc mul_prod_6
            sta mul_prod_6
            lda #0
            sbc mul_prod_7
            sta mul_prod_7
            rts

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Multiply unsigned mul_op1 by unsigned mul_op2
; Return 64 bit result in mul_prod

.proc multiply_unsigned

    ; Count ones in mul_op1. If many, take the two's complement and use
    ; a*b = 2**32*a - a*(2**32-b)
    clc
    ldx mul_op1_0
    lda ones_count,x
    ldx mul_op1_1
    adc ones_count,x
    ; Adds don't overflow; it is not necessary to clear the carry flag
    ldx mul_op1_2
    adc ones_count,x
    ldx mul_op1_3
    adc ones_count,x
    ldy #0
    cmp #18
    bcc end_op1
        sec
        lda #0
        sbc mul_op1_0
        sta mul_op1_0
        lda #0
        sbc mul_op1_1
        sta mul_op1_1
        lda #0
        sbc mul_op1_2
        sta mul_op1_2
        lda #0
        sbc mul_op1_3
        sta mul_op1_3
        iny
    end_op1:

    ; Extend mul_op2 to 40 bits
    lda #0
    sta muldiv_scratch2_4

    ; Clear the product
    sta mul_prod_0
    sta mul_prod_1
    sta mul_prod_2
    sta mul_prod_3
    sta mul_prod_4
    sta mul_prod_5
    sta mul_prod_6
    sta mul_prod_7

    ; We test bits 0, 8, 16 and 24 of muldiv_scratch1 and byte-shift
    ; muldiv_scratch2 as appropriate. By doing this, we only need to shift
    ; eight times, though we are multiplying 32 by 32 bits.

    ; muldiv_scratch1 bit 0, byte shift 0
    lsr mul_op1_0
    bcc @skip_add
        lda mul_op2_0
        sta mul_prod_0
        lda mul_op2_1
        sta mul_prod_1
        lda mul_op2_2
        sta mul_prod_2
        lda mul_op2_3
        sta mul_prod_3
    @skip_add:

    ldx #8
    bne enter_mul_loop
    mul_loop:

        asl mul_op2_0
        rol mul_op2_1
        rol mul_op2_2
        rol mul_op2_3
        rol muldiv_scratch2_4

        ; muldiv_scratch1 bit 0, byte shift 0
        lsr mul_op1_0
        bcc @skip_add
            clc
            lda mul_prod_0
            adc mul_op2_0
            sta mul_prod_0
            lda mul_prod_1
            adc mul_op2_1
            sta mul_prod_1
            lda mul_prod_2
            adc mul_op2_2
            sta mul_prod_2
            lda mul_prod_3
            adc mul_op2_3
            sta mul_prod_3
            lda mul_prod_4
            adc muldiv_scratch2_4
            sta mul_prod_4
            bcc @skip_add
            inc mul_prod_5
            bne @skip_add
            inc mul_prod_6
            bne @skip_add
            inc mul_prod_7
        @skip_add:

    enter_mul_loop:

        ; muldiv_scratch1 bit 8, byte shift 1
        lsr mul_op1_1
        bcc @skip_add
            clc
            lda mul_prod_1
            adc mul_op2_0
            sta mul_prod_1
            lda mul_prod_2
            adc mul_op2_1
            sta mul_prod_2
            lda mul_prod_3
            adc mul_op2_2
            sta mul_prod_3
            lda mul_prod_4
            adc mul_op2_3
            sta mul_prod_4
            lda mul_prod_5
            adc muldiv_scratch2_4
            sta mul_prod_5
            bcc @skip_add
            inc mul_prod_6
            bne @skip_add
            inc mul_prod_7
        @skip_add:

cheap_label_scope1:

        ; muldiv_scratch1 bit 16, byte shift 2
        lsr mul_op1_2
        bcc @skip_add
            clc
            lda mul_prod_2
            adc mul_op2_0
            sta mul_prod_2
            lda mul_prod_3
            adc mul_op2_1
            sta mul_prod_3
            lda mul_prod_4
            adc mul_op2_2
            sta mul_prod_4
            lda mul_prod_5
            adc mul_op2_3
            sta mul_prod_5
            lda mul_prod_6
            adc muldiv_scratch2_4
            sta mul_prod_6
            bcc @skip_add
            inc mul_prod_7
        @skip_add:

cheap_label_scope2:

        ; Exit the loop here on the eighth pass, and combine the last add with
        ; the post-multiply adjustment
        dex
        beq end_mul_loop

        ; muldiv_scratch1 bit 24, byte shift 3
        lsr mul_op1_3
        bcc @skip_add
            clc
            lda mul_prod_3
            adc mul_op2_0
            sta mul_prod_3
            lda mul_prod_4
            adc mul_op2_1
            sta mul_prod_4
            lda mul_prod_5
            adc mul_op2_2
            sta mul_prod_5
            lda mul_prod_6
            adc mul_op2_3
            sta mul_prod_6
            lda mul_prod_7
            adc muldiv_scratch2_4
            sta mul_prod_7
        @skip_add:

    jmp mul_loop
    end_mul_loop:

    ; Adjust product if needed
    ; mul_op2 is shifted left 31 bits; we may need to shift one more
    cpy #0
    bne end_mul
        lsr mul_op1_3
        bcc top_bit_1
            ; Few one bits, mul_op1 top bit set
            ; mul_prod <- mul_prod + (mul_op2 << 31)
            clc
            lda mul_prod_3
            adc mul_op2_0
            sta mul_prod_3
            lda mul_prod_4
            adc mul_op2_1
            sta mul_prod_4
            lda mul_prod_5
            adc mul_op2_2
            sta mul_prod_5
            lda mul_prod_6
            adc mul_op2_3
            sta mul_prod_6
            lda mul_prod_7
            adc muldiv_scratch2_4
            sta mul_prod_7
        top_bit_1:
        ; else:
        ;   Few one bits, mul_op1 top bit clear
        ;   Leave mul_prod unchanged
        rts
    end_mul:
        lsr mul_op1_3
        bcs top_bit_2
            ; Many one bits, mul_op1 top bit set (muldiv_scratch1 top bit clear)
            ; mul_prod <- (mul_op2 << 32) - mul_prod
            asl mul_op2_0
            rol mul_op2_1
            rol mul_op2_2
            rol mul_op2_3
            rol muldiv_scratch2_4
        top_bit_2:
        ; else:
        ;   Many one bits, mul_op1 top bit clear (muldiv_scratch1 top bit set)
        ;   mul_prod <- (mul_op2 << 31) - mul_prod
        sec
        lda #0
        sbc mul_prod_0
        sta mul_prod_0
        lda #0
        sbc mul_prod_1
        sta mul_prod_1
        lda #0
        sbc mul_prod_2
        sta mul_prod_2
        lda mul_op2_0
        sbc mul_prod_3
        sta mul_prod_3
        lda mul_op2_1
        sbc mul_prod_4
        sta mul_prod_4
        lda mul_op2_2
        sbc mul_prod_5
        sta mul_prod_5
        lda mul_op2_3
        sbc mul_prod_6
        sta mul_prod_6
        lda muldiv_scratch2_4
        sbc mul_prod_7
        sta mul_prod_7
        rts

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Multiply unsigned mul_op1 by unsigned mul_op2
; Return 32 bit result in mul_prod
; As multiply_unsigned, but returns only the lower 32 bits

.proc multiply_low

    ; Count ones in mul_op1. If many, take the two's complement and use
    ; a*b = 2**32*a - a*(2**32-b)
    clc
    ldx mul_op1_0
    lda ones_count,x
    ldx mul_op1_1
    adc ones_count,x
    ; Adds don't overflow; it is not necessary to clear the carry flag
    ldx mul_op1_2
    adc ones_count,x
    ldx mul_op1_3
    adc ones_count,x
    ldy #0
    cmp #18
    bcc end_op1
        sec
        lda #0
        sbc mul_op1_0
        sta mul_op1_0
        lda #0
        sbc mul_op1_1
        sta mul_op1_1
        lda #0
        sbc mul_op1_2
        sta mul_op1_2
        lda #0
        sbc mul_op1_3
        sta mul_op1_3
        iny
    end_op1:

    ; Clear the product
    lda #0
    sta mul_prod_0
    sta mul_prod_1
    sta mul_prod_2
    sta mul_prod_3

    ; We test bits 0, 8, 16 and 24 of muldiv_scratch1 and byte-shift
    ; muldiv_scratch2 as appropriate. By doing this, we only need to shift
    ; eight times, though we are multiplying 32 by 32 bits.

    ; muldiv_scratch1 bit 0, byte shift 0
    lsr mul_op1_0
    bcc @skip_add
        lda mul_op2_0
        sta mul_prod_0
        lda mul_op2_1
        sta mul_prod_1
        lda mul_op2_2
        sta mul_prod_2
        lda mul_op2_3
        sta mul_prod_3
    @skip_add:

    ldx #8
    bne enter_mul_loop
    mul_loop:

        asl mul_op2_0
        rol mul_op2_1
        rol mul_op2_2
        rol mul_op2_3

        ; muldiv_scratch1 bit 0, byte shift 0
        lsr mul_op1_0
        bcc @skip_add
            clc
            lda mul_prod_0
            adc mul_op2_0
            sta mul_prod_0
            lda mul_prod_1
            adc mul_op2_1
            sta mul_prod_1
            lda mul_prod_2
            adc mul_op2_2
            sta mul_prod_2
            lda mul_prod_3
            adc mul_op2_3
            sta mul_prod_3
        @skip_add:

    enter_mul_loop:

        ; muldiv_scratch1 bit 8, byte shift 1
        lsr mul_op1_1
        bcc @skip_add
            clc
            lda mul_prod_1
            adc mul_op2_0
            sta mul_prod_1
            lda mul_prod_2
            adc mul_op2_1
            sta mul_prod_2
            lda mul_prod_3
            adc mul_op2_2
            sta mul_prod_3
        @skip_add:

cheap_label_scope1:

        ; muldiv_scratch1 bit 16, byte shift 2
        lsr mul_op1_2
        bcc @skip_add
            clc
            lda mul_prod_2
            adc mul_op2_0
            sta mul_prod_2
            lda mul_prod_3
            adc mul_op2_1
            sta mul_prod_3
        @skip_add:

cheap_label_scope2:

        ; Exit the loop here on the eighth pass, and combine the last add with
        ; the post-multiply adjustment
        dex
        beq end_mul_loop

        ; muldiv_scratch1 bit 24, byte shift 3
        lsr mul_op1_3
        bcc @skip_add
            clc
            lda mul_prod_3
            adc mul_op2_0
            sta mul_prod_3
        @skip_add:

    jmp mul_loop
    end_mul_loop:

    ; Adjust product if needed
    ; mul_op2 is shifted left 31 bits; we may need to shift one more
    cpy #0
    bne end_mul
        lsr mul_op1_3
        bcc top_bit_1
            ; Few one bits, mul_op1 top bit set
            ; mul_prod <- mul_prod + (mul_op2 << 31)
            clc
            lda mul_prod_3
            adc mul_op2_0
            sta mul_prod_3
        top_bit_1:
        ; else:
        ;   Few one bits, mul_op1 top bit clear
        ;   Leave mul_prod unchanged
        rts
    end_mul:
        lsr mul_op1_3
        bcs top_bit_2
            ; Many one bits, mul_op1 top bit set (muldiv_scratch1 top bit clear)
            ; mul_prod <- (mul_op2 << 32) - mul_prod
            asl mul_op2_0
        top_bit_2:
            ; else:
            ; Many one bits, mul_op1 top bit clear (muldiv_scratch1 top bit set)
            ; mul_prod <- (mul_op2 << 31) - mul_prod
        sec
        lda #0
        sbc mul_prod_0
        sta mul_prod_0
        lda #0
        sbc mul_prod_1
        sta mul_prod_1
        lda #0
        sbc mul_prod_2
        sta mul_prod_2
        lda mul_op2_0
        sbc mul_prod_3
        sta mul_prod_3
        rts

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                          32 bit divide algorithms                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Divide signed operand in div_op1 by div_op2

.proc divide_signed

    ; Division by zero sets the quotient to $FFFFFFFF, which we need to leave
    ; alone
    lda div_op2_0
    ora div_op2_1
    ora div_op2_2
    ora div_op2_3
    beq zero_divide

    ; Set the absolute values of the operands, but keep the signs

    lda div_op1_3
    sta div_op1_sign
    bpl op1_end
        sec
        lda #0
        sbc div_op1_0
        sta div_op1_0
        lda #0
        sbc div_op1_1
        sta div_op1_1
        lda #0
        sbc div_op1_2
        sta div_op1_2
        lda #0
        sbc div_op1_3
        sta div_op1_3
    op1_end:

    lda div_op2_3
    sta div_op2_sign
    bpl op2_end
        sec
        lda #0
        sbc div_op2_0
        sta div_op2_0
        lda #0
        sbc div_op2_1
        sta div_op2_1
        lda #0
        sbc div_op2_2
        sta div_op2_2
        lda #0
        sbc div_op2_3
        sta div_op2_3
    op2_end:

    ; Do unsigned divide

    jsr divide_unsigned

    ; Set sign of remainder
    bit div_op1_sign
    bpl end_neg_rem
        sec
        lda #0
        sbc div_rem_0
        sta div_rem_0
        lda #0
        sbc div_rem_1
        sta div_rem_1
        lda #0
        sbc div_rem_2
        sta div_rem_2
        lda #0
        sbc div_rem_3
        sta div_rem_3
    end_neg_rem:

    ; Set sign of quotient

    lda div_op1_sign
    eor div_op2_sign
    bpl end_neg_quo
        sec
        lda #0
        sbc div_quo_0
        sta div_quo_0
        lda #0
        sbc div_quo_1
        sta div_quo_1
        lda #0
        sbc div_quo_2
        sta div_quo_2
        lda #0
        sbc div_quo_3
        sta div_quo_3
    end_neg_quo:

    rts

zero_divide:
    jmp divide_x0

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Divide unsigned operand in div_op1 by div_op2

.proc divide_unsigned

    ; Choose one of twelve optimized divide routines

    lda #0
    ldx div_op1_3
    cpx #1
    rol a
    ldx div_op1_2
    cpx #1
    rol a
    ldx div_op1_1
    cpx #1
    rol a
    ldx div_op1_0
    cpx #1
    rol a
    ldx div_op2_3
    cpx #1
    rol a
    ldx div_op2_2
    cpx #1
    rol a
    ldx div_op2_1
    cpx #1
    rol a
    ldx div_op2_0
    cpx #1
    rol a
    asl a
    bcs jump2
        sta jump_lo+1         ; self-modifying code here
        jump_lo:
        jmp (divide_dispatch) ; actually (divide_dispatch,a)
    jump2:
        sta jump_hi+1         ; self-modifying code here
        jump_hi:
        jmp (divide_dispatch+256) ; actually (divide_dispatch+256,a)

.endproc

.proc divide_x0

    ; Outcome of division by zero according to RISC-V
    lda #$FF
    sta div_quo_0
    sta div_quo_1
    sta div_quo_2
    sta div_quo_3
    lda div_op1_0
    sta div_rem_0
    lda div_op1_1
    sta div_rem_1
    lda div_op1_2
    sta div_rem_2
    lda div_op1_3
    sta div_rem_3
    rts

.endproc

.proc divide_small_large

    ; Divisor has more bytes than dividend

    lda #0
    sta div_quo_0
    sta div_quo_1
    sta div_quo_2
    sta div_quo_3
    lda div_op1_0
    sta div_rem_0
    lda div_op1_1
    sta div_rem_1
    lda div_op1_2
    sta div_rem_2
    lda div_op1_3
    sta div_rem_3
    rts

.endproc

; Divide one byte by one byte
.proc divide_11

    ; Initial quotient and remainder
    lda #0
    sta div_quo_0
    sta div_quo_1
    sta div_quo_2
    sta div_quo_3
    sta div_rem_0
    sta div_rem_1
    sta div_rem_2
    sta div_rem_3

    ; Dividend
    lda div_op1_0
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0

        ; Check for remainder greater than or equal to divisor
        sec
        lda div_rem_0
        sbc div_op2_0
        bcc @end_sub
            ; Update remainder
            sta div_rem_0
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0

    dex
    bne @div_loop

    rts

.endproc

; Divide two bytes by one byte
.proc divide_21

    ; Initial quotient and remainder
    lda #0
    sta div_quo_0
    sta div_quo_1
    sta div_quo_2
    sta div_quo_3
    sta div_rem_0
    sta div_rem_1
    sta div_rem_2
    sta div_rem_3

    ; Dividend byte 1
    lda div_op1_1
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0

        ; Check for remainder greater than or equal to divisor
        sec
        lda div_rem_0
        sbc div_op2_0
        bcc @end_sub
            ; Update remainder
            sta div_rem_0
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0

    dex
    bne @div_loop

cheap_label_scope1:

    ; Dividend byte 0
    lda div_op1_0
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0
        bcs @update_rem

        ; Check for remainder greater than or equal to divisor
        lda div_rem_0
        cmp div_op2_0
        bcc @end_sub
        @update_rem:
            ; Update remainder
            lda div_rem_0
            sbc div_op2_0
            sta div_rem_0
            sec
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0
        rol div_quo_1

    dex
    bne @div_loop

    rts

.endproc

; Divide three bytes by one byte
.proc divide_31

    ; Initial quotient and remainder
    lda #0
    sta div_quo_0
    sta div_quo_1
    sta div_quo_2
    sta div_quo_3
    sta div_rem_0
    sta div_rem_1
    sta div_rem_2
    sta div_rem_3

    ; Dividend byte 2
    lda div_op1_2
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0

        ; Check for remainder greater than or equal to divisor
        sec
        lda div_rem_0
        sbc div_op2_0
        bcc @end_sub
            ; Update remainder
            sta div_rem_0
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0

    dex
    bne @div_loop

cheap_label_scope1:

    ; Dividend byte 1
    lda div_op1_1
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0
        bcs @update_rem

        ; Check for remainder greater than or equal to divisor
        lda div_rem_0
        cmp div_op2_0
        bcc @end_sub
        @update_rem:
            ; Update remainder
            lda div_rem_0
            sbc div_op2_0
            sta div_rem_0
            sec
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0
        rol div_quo_1

    dex
    bne @div_loop

cheap_label_scope2:

    ; Dividend byte 0
    lda div_op1_0
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0
        bcs @update_rem

        ; Check for remainder greater than or equal to divisor
        lda div_rem_0
        cmp div_op2_0
        bcc @end_sub
        @update_rem:
            ; Update remainder
            lda div_rem_0
            sbc div_op2_0
            sta div_rem_0
            sec
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0
        rol div_quo_1
        rol div_quo_2

    dex
    bne @div_loop

    rts

.endproc

; Divide four bytes by one byte
.proc divide_41

    ; Initial quotient and remainder
    lda #0
    sta div_quo_0
    sta div_quo_1
    sta div_quo_2
    sta div_quo_3
    sta div_rem_0
    sta div_rem_1
    sta div_rem_2
    sta div_rem_3

    ; Dividend byte 3
    lda div_op1_3
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0

        ; Check for remainder greater than or equal to divisor
        sec
        lda div_rem_0
        sbc div_op2_0
        bcc @end_sub
            ; Update remainder
            sta div_rem_0
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0

    dex
    bne @div_loop

cheap_label_scope1:

    ; Dividend byte 2
    lda div_op1_2
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0
        bcs @update_rem

        ; Check for remainder greater than or equal to divisor
        lda div_rem_0
        cmp div_op2_0
        bcc @end_sub
        @update_rem:
            ; Update remainder
            lda div_rem_0
            sbc div_op2_0
            sta div_rem_0
            sec
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0
        rol div_quo_1

    dex
    bne @div_loop

cheap_label_scope2:

    ; Dividend byte 1
    lda div_op1_1
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0
        bcs @update_rem

        ; Check for remainder greater than or equal to divisor
        lda div_rem_0
        cmp div_op2_0
        bcc @end_sub
        @update_rem:
            ; Update remainder
            lda div_rem_0
            sbc div_op2_0
            sta div_rem_0
            sec
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0
        rol div_quo_1
        rol div_quo_2

    dex
    bne @div_loop

    ; Dividend byte 0
    lda div_op1_0
    sta muldiv_scratch2_4

cheap_label_scope3:

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0
        bcs @update_rem

        ; Check for remainder greater than or equal to divisor
        lda div_rem_0
        cmp div_op2_0
        bcc @end_sub
        @update_rem:
            ; Update remainder
            lda div_rem_0
            sbc div_op2_0
            sta div_rem_0
            sec
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0
        rol div_quo_1
        rol div_quo_2
        rol div_quo_3

    dex
    bne @div_loop

    rts

.endproc

; Divide two bytes by two bytes
.proc divide_22

    ; Initial quotient and remainder
    lda #0
    sta div_quo_0
    sta div_quo_1
    sta div_quo_2
    sta div_quo_3
    sta div_rem_1
    sta div_rem_2
    sta div_rem_3

    ; Dividend byte 1
    lda div_op1_1
    sta div_rem_0

    ; Dividend byte 0
    lda div_op1_0
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0
        rol div_rem_1

        ; Check for remainder greater than or equal to divisor
        lda div_rem_0
        cmp div_op2_0
        lda div_rem_1
        sbc div_op2_1
        bcc @end_sub
            ; Update remainder
            lda div_rem_0
            sbc div_op2_0
            sta div_rem_0
            lda div_rem_1
            sbc div_op2_1
            sta div_rem_1
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0

    dex
    bne @div_loop

    rts

.endproc

; Divide three bytes by two bytes
.proc divide_32

    ; Initial quotient and remainder
    lda #0
    sta div_quo_0
    sta div_quo_1
    sta div_quo_2
    sta div_quo_3
    sta div_rem_1
    sta div_rem_2
    sta div_rem_3

    ; Dividend byte 2
    lda div_op1_2
    sta div_rem_0

    ; Dividend byte 1
    lda div_op1_1
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0
        rol div_rem_1

        ; Check for remainder greater than or equal to divisor
        lda div_rem_0
        cmp div_op2_0
        lda div_rem_1
        sbc div_op2_1
        bcc @end_sub
            ; Update remainder
            lda div_rem_0
            sbc div_op2_0
            sta div_rem_0
            lda div_rem_1
            sbc div_op2_1
            sta div_rem_1
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0

    dex
    bne @div_loop

cheap_label_scope1:

    ; Dividend byte 0
    lda div_op1_0
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0
        rol div_rem_1
        bcs @update_rem

        ; Check for remainder greater than or equal to divisor
        lda div_rem_0
        cmp div_op2_0
        lda div_rem_1
        sbc div_op2_1
        bcc @end_sub
        @update_rem:
            ; Update remainder
            lda div_rem_0
            sbc div_op2_0
            sta div_rem_0
            lda div_rem_1
            sbc div_op2_1
            sta div_rem_1
            sec
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0
        rol div_quo_1

    dex
    bne @div_loop

    rts

.endproc

; Divide four bytes by two bytes
.proc divide_42

    ; Initial quotient and remainder
    lda #0
    sta div_quo_0
    sta div_quo_1
    sta div_quo_2
    sta div_quo_3
    sta div_rem_1
    sta div_rem_2
    sta div_rem_3

    ; Dividend byte 3
    lda div_op1_3
    sta div_rem_0

    ; Dividend byte 2
    lda div_op1_2
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0
        rol div_rem_1

        ; Check for remainder greater than or equal to divisor
        lda div_rem_0
        cmp div_op2_0
        lda div_rem_1
        sbc div_op2_1
        bcc @end_sub
            ; Update remainder
            lda div_rem_0
            sbc div_op2_0
            sta div_rem_0
            lda div_rem_1
            sbc div_op2_1
            sta div_rem_1
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0

    dex
    bne @div_loop

cheap_label_scope1:

    ; Dividend byte 1
    lda div_op1_1
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0
        rol div_rem_1
        bcs @update_rem

        ; Check for remainder greater than or equal to divisor
        lda div_rem_0
        cmp div_op2_0
        lda div_rem_1
        sbc div_op2_1
        bcc @end_sub
        @update_rem:
            ; Update remainder
            lda div_rem_0
            sbc div_op2_0
            sta div_rem_0
            lda div_rem_1
            sbc div_op2_1
            sta div_rem_1
            sec
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0
        rol div_quo_1

    dex
    bne @div_loop

cheap_label_scope2:

    ; Dividend byte 0
    lda div_op1_0
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0
        rol div_rem_1
        bcs @update_rem

        ; Check for remainder greater than or equal to divisor
        lda div_rem_0
        cmp div_op2_0
        lda div_rem_1
        sbc div_op2_1
        bcc @end_sub
        @update_rem:
            ; Update remainder
            lda div_rem_0
            sbc div_op2_0
            sta div_rem_0
            lda div_rem_1
            sbc div_op2_1
            sta div_rem_1
            sec
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0
        rol div_quo_1
        rol div_quo_2

    dex
    bne @div_loop

    rts

.endproc

; Divide three bytes by three bytes
.proc divide_33

    ; Initial quotient and remainder
    lda #0
    sta div_quo_0
    sta div_quo_1
    sta div_quo_2
    sta div_quo_3
    sta div_rem_2
    sta div_rem_3

    ; Dividend byte 2
    lda div_op1_2
    sta div_rem_1

    ; Dividend byte 1
    lda div_op1_1
    sta div_rem_0

    ; Dividend byte 0
    lda div_op1_0
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0
        rol div_rem_1
        rol div_rem_2

        ; Check for remainder greater than or equal to divisor
        lda div_rem_0
        cmp div_op2_0
        lda div_rem_1
        sbc div_op2_1
        lda div_rem_2
        sbc div_op2_2
        bcc @end_sub
            ; Update remainder
            lda div_rem_0
            sbc div_op2_0
            sta div_rem_0
            lda div_rem_1
            sbc div_op2_1
            sta div_rem_1
            lda div_rem_2
            sbc div_op2_2
            sta div_rem_2
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0

    dex
    bne @div_loop

    rts

.endproc

; Divide four bytes by three bytes
.proc divide_43

    ; Initial quotient and remainder
    lda #0
    sta div_quo_0
    sta div_quo_1
    sta div_quo_2
    sta div_quo_3
    sta div_rem_2
    sta div_rem_3

    ; Dividend byte 3
    lda div_op1_3
    sta div_rem_1

    ; Dividend byte 2
    lda div_op1_2
    sta div_rem_0

    ; Dividend byte 1
    lda div_op1_1
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0
        rol div_rem_1
        rol div_rem_2

        ; Check for remainder greater than or equal to divisor
        lda div_rem_0
        cmp div_op2_0
        lda div_rem_1
        sbc div_op2_1
        lda div_rem_2
        sbc div_op2_2
        bcc @end_sub
            ; Update remainder
            lda div_rem_0
            sbc div_op2_0
            sta div_rem_0
            lda div_rem_1
            sbc div_op2_1
            sta div_rem_1
            lda div_rem_2
            sbc div_op2_2
            sta div_rem_2
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0

    dex
    bne @div_loop

cheap_label_scope1:

    ; Dividend byte 0
    lda div_op1_0
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0
        rol div_rem_1
        rol div_rem_2
        bcs @update_rem

        ; Check for remainder greater than or equal to divisor
        lda div_rem_0
        cmp div_op2_0
        lda div_rem_1
        sbc div_op2_1
        lda div_rem_2
        sbc div_op2_2
        bcc @end_sub
        @update_rem:
            ; Update remainder
            lda div_rem_0
            sbc div_op2_0
            sta div_rem_0
            lda div_rem_1
            sbc div_op2_1
            sta div_rem_1
            lda div_rem_2
            sbc div_op2_2
            sta div_rem_2
            sec
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0
        rol div_quo_1

    dex
    bne @div_loop

    rts

.endproc

; Divide four bytes by four bytes
.proc divide_44

    ; Initial quotient and remainder
    lda #0
    sta div_quo_0
    sta div_quo_1
    sta div_quo_2
    sta div_quo_3
    sta div_rem_3

    ; Dividend byte 3
    lda div_op1_3
    sta div_rem_2

    ; Dividend byte 2
    lda div_op1_2
    sta div_rem_1

    ; Dividend byte 1
    lda div_op1_1
    sta div_rem_0

    ; Dividend byte 0
    lda div_op1_0
    sta muldiv_scratch2_4

    ldx #8
    @div_loop:

        ; Shift into remainder
        asl muldiv_scratch2_4
        rol div_rem_0
        rol div_rem_1
        rol div_rem_2
        rol div_rem_3

        ; Check for remainder greater than or equal to divisor
        lda div_rem_0
        cmp div_op2_0
        lda div_rem_1
        sbc div_op2_1
        lda div_rem_2
        sbc div_op2_2
        lda div_rem_3
        sbc div_op2_3
        bcc @end_sub
            ; Update remainder
            lda div_rem_0
            sbc div_op2_0
            sta div_rem_0
            lda div_rem_1
            sbc div_op2_1
            sta div_rem_1
            lda div_rem_2
            sbc div_op2_2
            sta div_rem_2
            lda div_rem_3
            sbc div_op2_3
            sta div_rem_3
        @end_sub:

        ; Shift carry into quotient
        rol div_quo_0

    dex
    bne @div_loop

    rts

.endproc

.endif ; RV32M

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Various 256 byte lookup tables.
; Address lookup tables must be page aligned, because they are read through a
; self-modifying code technique that assumes such alignment.
; Shift lookup tables are page aligned so that indexing does not add a cycle.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "PAGEALIGN"

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

; Interleaved dispatch tables for opcodes dispatched via funct3
; funct3 appears in _RISCV_opcode+1 bits 6-4. The rest is masked off via
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

    .align 256

; Dispatch table for shift_left
sll_dispatch:
    .repeat 4
        .word sll_0
        .word sll_1
        .word sll_2
        .word sll_3
        .word sll_4
        .word sll_5
        .word sll_6
        .word sll_7
        .word sll_8
        .word sll_9
        .word sll_10
        .word sll_11
        .word sll_12
        .word sll_13
        .word sll_14
        .word sll_15
        .word sll_16
        .word sll_17
        .word sll_18
        .word sll_19
        .word sll_20
        .word sll_21
        .word sll_22
        .word sll_23
        .word sll_24
        .word sll_25
        .word sll_26
        .word sll_27
        .word sll_28
        .word sll_29
        .word sll_30
        .word sll_31
    .endrep

; Dispatch table for shift_right_unsigned
srl_dispatch:
    .repeat 4
        .word srl_0
        .word srl_1
        .word srl_2
        .word srl_3
        .word srl_4
        .word srl_5
        .word srl_6
        .word srl_7
        .word srl_8
        .word srl_9
        .word srl_10
        .word srl_11
        .word srl_12
        .word srl_13
        .word srl_14
        .word srl_15
        .word srl_16
        .word srl_17
        .word srl_18
        .word srl_19
        .word srl_20
        .word srl_21
        .word srl_22
        .word srl_23
        .word srl_24
        .word srl_25
        .word srl_26
        .word srl_27
        .word srl_28
        .word srl_29
        .word srl_30
        .word srl_31
    .endrep

; Dispatch table for shift_right_signed
sra_dispatch:
    .repeat 4
        .word sra_0
        .word sra_1
        .word sra_2
        .word sra_3
        .word sra_4
        .word sra_5
        .word sra_6
        .word sra_7
        .word sra_8
        .word sra_9
        .word sra_10
        .word sra_11
        .word sra_12
        .word sra_13
        .word sra_14
        .word sra_15
        .word sra_16
        .word sra_17
        .word sra_18
        .word sra_19
        .word sra_20
        .word sra_21
        .word sra_22
        .word sra_23
        .word sra_24
        .word sra_25
        .word sra_26
        .word sra_27
        .word sra_28
        .word sra_29
        .word sra_30
        .word sra_31
    .endrep

.if RV32M

; Dispatch table for divide_common
divide_dispatch:
    .word divide_x0
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_x0
    .word divide_11
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_x0
    .word divide_21
    .word divide_22
    .word divide_22
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_x0
    .word divide_21
    .word divide_22
    .word divide_22
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_x0
    .word divide_31
    .word divide_32
    .word divide_32
    .word divide_33
    .word divide_33
    .word divide_33
    .word divide_33
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_x0
    .word divide_31
    .word divide_32
    .word divide_32
    .word divide_33
    .word divide_33
    .word divide_33
    .word divide_33
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_x0
    .word divide_31
    .word divide_32
    .word divide_32
    .word divide_33
    .word divide_33
    .word divide_33
    .word divide_33
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_x0
    .word divide_31
    .word divide_32
    .word divide_32
    .word divide_33
    .word divide_33
    .word divide_33
    .word divide_33
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_small_large
    .word divide_x0
    .word divide_41
    .word divide_42
    .word divide_42
    .word divide_43
    .word divide_43
    .word divide_43
    .word divide_43
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_x0
    .word divide_41
    .word divide_42
    .word divide_42
    .word divide_43
    .word divide_43
    .word divide_43
    .word divide_43
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_x0
    .word divide_41
    .word divide_42
    .word divide_42
    .word divide_43
    .word divide_43
    .word divide_43
    .word divide_43
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_x0
    .word divide_41
    .word divide_42
    .word divide_42
    .word divide_43
    .word divide_43
    .word divide_43
    .word divide_43
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_x0
    .word divide_41
    .word divide_42
    .word divide_42
    .word divide_43
    .word divide_43
    .word divide_43
    .word divide_43
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_x0
    .word divide_41
    .word divide_42
    .word divide_42
    .word divide_43
    .word divide_43
    .word divide_43
    .word divide_43
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_x0
    .word divide_41
    .word divide_42
    .word divide_42
    .word divide_43
    .word divide_43
    .word divide_43
    .word divide_43
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_x0
    .word divide_41
    .word divide_42
    .word divide_42
    .word divide_43
    .word divide_43
    .word divide_43
    .word divide_43
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44
    .word divide_44

.endif ; RV32M

; Shift tables:

; Left shift
lshift2:
.repeat 256,i
    .byte (i << 2) & $FF
.endrep

lshift3:
.repeat 256,i
    .byte (i << 3) & $FF
.endrep

lshift4:
.repeat 256,i
    .byte (i << 4) & $FF
.endrep

lshift5:
.repeat 256,i
    .byte (i << 5) & $FF
.endrep

lshift6:
.repeat 256,i
    .byte (i << 6) & $FF
.endrep

; Unsigned right shift
rshift2:
.repeat 256,i
    .byte i >> 2
.endrep

rshift3:
.repeat 256,i
    .byte i >> 3
.endrep

rshift4:
.repeat 256,i
    .byte i >> 4
.endrep

rshift5:
.repeat 256,i
    .byte i >> 5
.endrep

rshift6:
.repeat 256,i
    .byte i >> 6
.endrep

; Signed right shift
rsshift2:
.repeat 128,i
    .byte i >> 2
.endrep
.repeat 128,i
    .byte (i >> 2) | $E0
.endrep

rsshift3:
.repeat 128,i
    .byte i >> 3
.endrep
.repeat 128,i
    .byte (i >> 3) | $F0
.endrep

rsshift4:
.repeat 128,i
    .byte i >> 4
.endrep
.repeat 128,i
    .byte (i >> 4) | $F8
.endrep

rsshift5:
.repeat 128,i
    .byte i >> 5
.endrep
.repeat 128,i
    .byte (i >> 5) | $FC
.endrep

rsshift6:
.repeat 128,i
    .byte i >> 6
.endrep
.repeat 128,i
    .byte (i >> 6) | $FE
.endrep

; The do_branch macro uses these tables to unshuffle the offset

; Branch, opcode byte 0 to offset byte 1:
; b11 xxx xxx xxx xxx xxx xxx xxx =>   0   0   0   0 b11   0   0   0
branch_lookup_01:
.repeat 128
    .byte $00
.endrep
.repeat 128
    .byte $08
.endrep

; Branch, opcode byte 3 to offset byte 0:
; xxx xxx xxx xxx b07 b06 b05 xxx => b07 b06 b05   0   0   0   0   0
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

; JAL uses these tables to unshuffle the offset

; Opcode byte 2 to offset byte 0:
; b03 b02 b01 xxx xxx xxx xxx xxx =>   0   0   0   0 b03 b02 b01   0
jump_lookup_20:
.repeat 8,i
    .repeat 32
        .byte i*2
    .endrep
.endrep

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

.if RV32M
; Number of ones in a byte
; The multiply routines use this for an optimization
ones_count:
    .byte $00,$01,$01,$02,$01,$02,$02,$03,$01,$02,$02,$03,$02,$03,$03,$04
    .byte $01,$02,$02,$03,$02,$03,$03,$04,$02,$03,$03,$04,$03,$04,$04,$05
    .byte $01,$02,$02,$03,$02,$03,$03,$04,$02,$03,$03,$04,$03,$04,$04,$05
    .byte $02,$03,$03,$04,$03,$04,$04,$05,$03,$04,$04,$05,$04,$05,$05,$06
    .byte $01,$02,$02,$03,$02,$03,$03,$04,$02,$03,$03,$04,$03,$04,$04,$05
    .byte $02,$03,$03,$04,$03,$04,$04,$05,$03,$04,$04,$05,$04,$05,$05,$06
    .byte $02,$03,$03,$04,$03,$04,$04,$05,$03,$04,$04,$05,$04,$05,$05,$06
    .byte $03,$04,$04,$05,$04,$05,$05,$06,$04,$05,$05,$06,$05,$06,$06,$07
    .byte $01,$02,$02,$03,$02,$03,$03,$04,$02,$03,$03,$04,$03,$04,$04,$05
    .byte $02,$03,$03,$04,$03,$04,$04,$05,$03,$04,$04,$05,$04,$05,$05,$06
    .byte $02,$03,$03,$04,$03,$04,$04,$05,$03,$04,$04,$05,$04,$05,$05,$06
    .byte $03,$04,$04,$05,$04,$05,$05,$06,$04,$05,$05,$06,$05,$06,$06,$07
    .byte $02,$03,$03,$04,$03,$04,$04,$05,$03,$04,$04,$05,$04,$05,$05,$06
    .byte $03,$04,$04,$05,$04,$05,$05,$06,$04,$05,$05,$06,$05,$06,$06,$07
    .byte $03,$04,$04,$05,$04,$05,$05,$06,$04,$05,$05,$06,$05,$06,$06,$07
    .byte $04,$05,$05,$06,$05,$06,$06,$07,$05,$06,$06,$07,$06,$07,$07,$08
.endif ; RV32M
