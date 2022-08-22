; bank4.s -- Bank 4 entry points

.include "syscall.inc"

jmp RISCV_syscall_init
jmp RISCV_syscall_exit
jmp RISCV_ECALL
jmp RISCV_EBREAK
