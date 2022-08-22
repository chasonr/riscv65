; syscall.inc -- implement system calls via ECALL and EBREAK

.global RISCV_syscall_init
.global RISCV_syscall_exit
.global RISCV_ECALL
.global RISCV_EBREAK

; vim: syntax=asm
