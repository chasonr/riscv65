; All memory areas must be declared in memory.s. Global declarations in
; memory.s should be named as imports in memory.inc.

; The current bank number
.importzp current_bank

; Some scratch pointers
.importzp pointer1, pointer2, pointer3, pointer4

; The PC register
.import RISCV_pc

; Integer registers
.import RISCV_ireg_0,RISCV_ireg_1,RISCV_ireg_2,RISCV_ireg_3

; Floating point registers (not yet implemented)
; .import RISCV_freg_0, RISCV_freg_1, RISCV_freg_2, RISCV_freg_3
; .import RISCV_freg_4, RISCV_freg_5, RISCV_freg_6, RISCV_freg_7
; .import RISCV_freg_8, RISCV_freg_9, RISCV_freg_10, RISCV_freg_11
; .import RISCV_freg_12, RISCV_freg_13, RISCV_freg_14, RISCV_freg_15

; Bank switching
.import far_call
.import cold_start_jump
.import warm_start_jump
.import warm_start_return

; Scratch area, internal to various routines
.import scratch_area

; Shift tables
.import lshift2, lshift3, lshift4, lshift5, lshift6
.import rshift2, rshift3, rshift4, rshift5, rshift6
.import rsshift2, rsshift3, rsshift4, rsshift5, rsshift6
