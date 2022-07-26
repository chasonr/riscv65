; Each cartridge bank other than bank 0 must begin with
; -- this file
; -- followed by a jump table for its entry points.

.include "memory.inc"

.code

; Bank 0 places routines in RAM to route the cold start and warm start back
; to Bank 0
.word cold_start_jump
.word warm_start_jump
; The Kernal recognizes a cartridge by this signature, which must be present
; regardless of which bank is active
.byte $C3, $C2, $CD, $38, $30
