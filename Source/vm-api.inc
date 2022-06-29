; vm-api.inc

.include "reu.inc"

.import RISCV_check_address

; Fetch one opcode into _RISCV_opcode
.macro RISCV_fetch

    ; If _RISCV_pc_check is nonzero, either the PC has crossed a 64K boundary
    ; since the last check, or a branch instruction has been taken

    lda _RISCV_pc_check
    beq :+
        jsr RISCV_check_address
    :

    ; Read one opcode

    set_reu_address _RISCV_pc
    set_local_address _RISCV_opcode
    set_xfer_size_imm 4
    do_reu_read

.endmacro
