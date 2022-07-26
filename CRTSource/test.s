; test.s

.include "kernal.inc"

.code

; Entry point jump table

jmp far_call_test

.proc far_call_test

    ldx #0
    @print:
        lda got_there,x
        beq @end_print
        jsr CHROUT
    inx
    bne @print
    @end_print:
    rts

got_there: .byte "printing from a far call", 13, 0

.endproc
