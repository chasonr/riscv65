; All memory areas must be declared in this source file. That is, the segments
; .bss, "BSSPAGE" and .zeropage must appear in this source file, and no other.
;
; Declarations should be global and declared as imports in memory.inc.
; Any initialization should be done in init.s.
;
; This source file is linked to every bank, so that every bank has the same
; idea of how memory is organized. 

.zeropage

; Current bank number
.global current_bank
current_bank: .res 1

; Some scratch areas for passing parameters and return values
.global pointer1, pointer2, pointer3, pointer4
pointer1: .res 2
pointer2: .res 2
pointer3: .res 2
pointer4: .res 2
.global longreg1, longreg2
longreg1: .res 4
longreg2: .res 4

.bss

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RISC-V registers are placed first, for the benefit of target-supplied code.
; Integer and floating point registers are declared with one byte in each of
; several tables; e.g. integer register 5 occupies RISCV_ireg_0+5,
; RISCV_ireg_1+5, RISCV_ireg_2+5 and RISCV_ireg_3+5. This arrangement avoids
; having to shift indexes when accessing registers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Integer registers
.global RISCV_ireg_0,RISCV_ireg_1,RISCV_ireg_2,RISCV_ireg_3
RISCV_ireg_0: .res 32
RISCV_ireg_1: .res 32
RISCV_ireg_2: .res 32
RISCV_ireg_3: .res 32

; Floating point registers (not yet implemented)
; .global RISCV_freg_0, RISCV_freg_1, RISCV_freg_2, RISCV_freg_3
; .global RISCV_freg_4, RISCV_freg_5, RISCV_freg_6, RISCV_freg_7
; .global RISCV_freg_8, RISCV_freg_9, RISCV_freg_10, RISCV_freg_11
; .global RISCV_freg_12, RISCV_freg_13, RISCV_freg_14, RISCV_freg_15
; RISCV_freg_0: .res 32
; RISCV_freg_1: .res 32
; RISCV_freg_2: .res 32
; RISCV_freg_3: .res 32
; RISCV_freg_4: .res 32
; RISCV_freg_5: .res 32
; RISCV_freg_6: .res 32
; RISCV_freg_7: .res 32
; RISCV_freg_8: .res 32
; RISCV_freg_9: .res 32
; RISCV_freg_10: .res 32
; RISCV_freg_11: .res 32
; RISCV_freg_12: .res 32
; RISCV_freg_13: .res 32
; RISCV_freg_14: .res 32
; RISCV_freg_15: .res 32

; The PC register
.global RISCV_pc
RISCV_pc: .res 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bank switching areas. Bank 0 writes code here to be visible to all banks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.global far_call
far_call: .res 32
.global cold_start_jump
cold_start_jump: .res 16
.global warm_start_jump
warm_start_jump: .res 16
.global warm_start_return
warm_start_return: .res 16

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The file browser uses this area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BROWSER_MAX_PATH_LEN = 256

; Displayed directory, while the browser is active
; File to run, after the browser returns
.global browser_path
browser_path: .res BROWSER_MAX_PATH_LEN

; Current number of files displayed. The records reside in the REU.
.global browser_num_files
browser_num_files: .res 2

; Current state of the screen
.global browser_top_row
browser_top_row: .res 2
.global browser_cursor_pos
browser_cursor_pos: .res 2
.global browser_search_str
browser_search_str: .res 40

; Saved colors
.global browser_old_border_color
browser_old_border_color: .res 1
.global browser_old_background_color
browser_old_background_color: .res 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ultidos.s uses this area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Status strings from the command interface go here
.global ultidos_status
ultidos_status: .res 256

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Scratch area, internal to various routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.global scratch_area
scratch_area: .res 512

.segment "BSSPAGE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shift tables generated at cold start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Left shift
.global lshift2, lshift3, lshift4, lshift5, lshift6
lshift2: .res 256
lshift3: .res 256
lshift4: .res 256
lshift5: .res 256
lshift6: .res 256

; Unsigned right shift
.global rshift2, rshift3, rshift4, rshift5, rshift6
rshift2: .res 256
rshift3: .res 256
rshift4: .res 256
rshift5: .res 256
rshift6: .res 256

; Signed right shift
.global rsshift2, rsshift3, rsshift4, rsshift5, rsshift6
rsshift2: .res 256
rsshift3: .res 256
rsshift4: .res 256
rsshift5: .res 256
rsshift6: .res 256

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Character conversion tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.global ascii_to_petscii
ascii_to_petscii: .res 256
.global ascii_to_screen
ascii_to_screen: .res 256
.global petscii_to_screen
petscii_to_screen: .res 256
