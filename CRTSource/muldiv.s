; muldiv.s -- integer multiply, integer divide and shift

.include "registers.inc"
.include "reu-regs.inc"
.include "memory.inc"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         32 bit multiply algorithms                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.code

; Multiply signed mul_op1 by signed mul_op2
; Return 64 bit result in mul_prod

.global multiply_signed
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

    ; We test bits 0, 8, 16 and 24 of mul_op1 and byte-shift
    ; mul_op2 as appropriate. By doing this, we only need to shift
    ; eight times, though we are multiplying 32 by 32 bits.

    ; On the first pass, mul_prod is 0, and we can skip the add

    ; mul_op1 bit 0, byte shift 0
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

        ; mul_op1 bit 0, byte shift 0
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

        ; mul_op1 bit 8, byte shift 1
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

        ; mul_op1 bit 16, byte shift 2
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

        ; Exit the loop here on the eighth pass; we'll subtract mul_op2,
        ; because the place value of the bit in mul_op1 is negative
        dex
        beq end_mul_loop

        ; mul_op1 bit 24, byte shift 3
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
            ; Many one bits, mul_op1 positive (mul_op1 negative)
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
            ; Many one bits, mul_op1 negative (mul_op1 positive)
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

.global multiply_signed_unsigned
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

    ; We test bits 0, 8, 16 and 24 of mul_op1 and byte-shift
    ; mul_op2 as appropriate. By doing this, we only need to shift
    ; eight times, though we are multiplying 32 by 32 bits.

    ; mul_op1 bit 0, byte shift 0
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

        ; mul_op1 bit 0, byte shift 0
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

        ; mul_op1 bit 8, byte shift 1
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

        ; mul_op1 bit 16, byte shift 2
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

        ; Exit the loop here on the eighth pass; we'll subtract mul_op2,
        ; because the place value of the bit in mul_op1 is negative
        dex
        beq end_mul_loop

        ; mul_op1 bit 24, byte shift 3
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
            ; Many one bits, mul_op1 positive (original mul_op1 negative)
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
            ; Many one bits, mul_op1 negative (original mul_op1 positive)
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

.global multiply_unsigned
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

    ; We test bits 0, 8, 16 and 24 of mul_op1 and byte-shift
    ; mul_op2 as appropriate. By doing this, we only need to shift
    ; eight times, though we are multiplying 32 by 32 bits.

    ; mul_op1 bit 0, byte shift 0
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

        ; mul_op1 bit 0, byte shift 0
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

        ; mul_op1 bit 8, byte shift 1
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

        ; mul_op1 bit 16, byte shift 2
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

        ; mul_op1 bit 24, byte shift 3
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
            ; Many one bits, mul_op1 top bit set (original mul_op1 top bit clear)
            ; mul_prod <- (mul_op2 << 32) - mul_prod
            asl mul_op2_0
            rol mul_op2_1
            rol mul_op2_2
            rol mul_op2_3
            rol muldiv_scratch2_4
        top_bit_2:
        ; else:
        ;   Many one bits, mul_op1 top bit clear (original mul_op1 top bit set)
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

.global multiply_low
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

    ; We test bits 0, 8, 16 and 24 of mul_op1 and byte-shift
    ; mul_op2 as appropriate. By doing this, we only need to shift
    ; eight times, though we are multiplying 32 by 32 bits.

    ; mul_op1 bit 0, byte shift 0
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

        ; mul_op1 bit 0, byte shift 0
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

        ; mul_op1 bit 8, byte shift 1
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

        ; mul_op1 bit 16, byte shift 2
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

        ; mul_op1 bit 24, byte shift 3
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
            ; Many one bits, mul_op1 top bit set (original mul_op1 top bit clear)
            ; mul_prod <- (mul_op2 << 32) - mul_prod
            asl mul_op2_0
        top_bit_2:
            ; else:
            ; Many one bits, mul_op1 top bit clear (original mul_op1 top bit set)
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

.segment "ROPAGE"
.align 256

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                          32 bit divide algorithms                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.code

; Divide signed operand in div_op1 by div_op2

.global divide_signed
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

.global divide_unsigned
.proc divide_unsigned

    ; Choose one of twelve optimized divide routines
    lda div_op2_3
    beq op2_3_bytes

        lda div_op1_3
        bne div_44
        beq div_small_large

    op2_3_bytes:
    lda div_op2_2
    beq op2_2_bytes

        lda div_op1_3
        bne div_43
        lda div_op1_2
        bne div_33
        beq div_small_large

    op2_2_bytes:
    lda div_op2_1
    beq op2_1_bytes

        lda div_op1_3
        bne div_42
        lda div_op1_2
        bne div_32
        lda div_op1_1
        bne div_22
        beq div_small_large

    op2_1_bytes:
    lda div_op2_0
    beq op2_0_bytes

        lda div_op1_3
        bne div_41
        lda div_op1_2
        bne div_31
        lda div_op1_1
        bne div_21
        lda div_op1_0
        bne div_11
        beq div_small_large

    op2_0_bytes:
    jmp divide_x0

    div_44: jmp divide_44
    div_43: jmp divide_43
    div_42: jmp divide_42
    div_41: jmp divide_41
    div_33: jmp divide_33
    div_32: jmp divide_32
    div_31: jmp divide_31
    div_22: jmp divide_22
    div_21: jmp divide_21
    div_11: jmp divide_11
    div_small_large: jmp divide_small_large

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                          32 bit shift algorithms                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Shift the contents of imm to the left
; pointer1 contains the number of bits to shift left

.global shift_left
.proc shift_left

    lda pointer1+0
    and #$1F
    tax
    lda sll_dispatch_lo,x
    sta pointer1+0
    lda sll_dispatch_hi,x
    sta pointer1+1
    jmp (pointer1)

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
; pointer1 contains the number of bits to shift right

.global shift_right_unsigned
.proc shift_right_unsigned

    lda pointer1+0
    and #$1F
    tax
    lda srl_dispatch_lo,x
    sta pointer1+0
    lda srl_dispatch_hi,x
    sta pointer1+1
    jmp (pointer1)

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
; pointer1 contains the number of bits to shift right

.global shift_right_signed
.proc shift_right_signed

    lda pointer1+0
    and #$1F
    tax
    lda sra_dispatch_lo,x
    sta pointer1+0
    lda sra_dispatch_hi,x
    sta pointer1+1
    jmp (pointer1)

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

.segment "ROPAGE"
.align 256

; Dispatch table for shift_left
sll_dispatch_lo:
    .byte <sll_0
    .byte <sll_1
    .byte <sll_2
    .byte <sll_3
    .byte <sll_4
    .byte <sll_5
    .byte <sll_6
    .byte <sll_7
    .byte <sll_8
    .byte <sll_9
    .byte <sll_10
    .byte <sll_11
    .byte <sll_12
    .byte <sll_13
    .byte <sll_14
    .byte <sll_15
    .byte <sll_16
    .byte <sll_17
    .byte <sll_18
    .byte <sll_19
    .byte <sll_20
    .byte <sll_21
    .byte <sll_22
    .byte <sll_23
    .byte <sll_24
    .byte <sll_25
    .byte <sll_26
    .byte <sll_27
    .byte <sll_28
    .byte <sll_29
    .byte <sll_30
    .byte <sll_31

sll_dispatch_hi:
    .byte >sll_0
    .byte >sll_1
    .byte >sll_2
    .byte >sll_3
    .byte >sll_4
    .byte >sll_5
    .byte >sll_6
    .byte >sll_7
    .byte >sll_8
    .byte >sll_9
    .byte >sll_10
    .byte >sll_11
    .byte >sll_12
    .byte >sll_13
    .byte >sll_14
    .byte >sll_15
    .byte >sll_16
    .byte >sll_17
    .byte >sll_18
    .byte >sll_19
    .byte >sll_20
    .byte >sll_21
    .byte >sll_22
    .byte >sll_23
    .byte >sll_24
    .byte >sll_25
    .byte >sll_26
    .byte >sll_27
    .byte >sll_28
    .byte >sll_29
    .byte >sll_30
    .byte >sll_31

; Dispatch table for shift_right_unsigned
srl_dispatch_lo:
    .byte <srl_0
    .byte <srl_1
    .byte <srl_2
    .byte <srl_3
    .byte <srl_4
    .byte <srl_5
    .byte <srl_6
    .byte <srl_7
    .byte <srl_8
    .byte <srl_9
    .byte <srl_10
    .byte <srl_11
    .byte <srl_12
    .byte <srl_13
    .byte <srl_14
    .byte <srl_15
    .byte <srl_16
    .byte <srl_17
    .byte <srl_18
    .byte <srl_19
    .byte <srl_20
    .byte <srl_21
    .byte <srl_22
    .byte <srl_23
    .byte <srl_24
    .byte <srl_25
    .byte <srl_26
    .byte <srl_27
    .byte <srl_28
    .byte <srl_29
    .byte <srl_30
    .byte <srl_31

srl_dispatch_hi:
    .byte >srl_0
    .byte >srl_1
    .byte >srl_2
    .byte >srl_3
    .byte >srl_4
    .byte >srl_5
    .byte >srl_6
    .byte >srl_7
    .byte >srl_8
    .byte >srl_9
    .byte >srl_10
    .byte >srl_11
    .byte >srl_12
    .byte >srl_13
    .byte >srl_14
    .byte >srl_15
    .byte >srl_16
    .byte >srl_17
    .byte >srl_18
    .byte >srl_19
    .byte >srl_20
    .byte >srl_21
    .byte >srl_22
    .byte >srl_23
    .byte >srl_24
    .byte >srl_25
    .byte >srl_26
    .byte >srl_27
    .byte >srl_28
    .byte >srl_29
    .byte >srl_30
    .byte >srl_31

; Dispatch table for shift_right_signed
sra_dispatch_lo:
    .byte <sra_0
    .byte <sra_1
    .byte <sra_2
    .byte <sra_3
    .byte <sra_4
    .byte <sra_5
    .byte <sra_6
    .byte <sra_7
    .byte <sra_8
    .byte <sra_9
    .byte <sra_10
    .byte <sra_11
    .byte <sra_12
    .byte <sra_13
    .byte <sra_14
    .byte <sra_15
    .byte <sra_16
    .byte <sra_17
    .byte <sra_18
    .byte <sra_19
    .byte <sra_20
    .byte <sra_21
    .byte <sra_22
    .byte <sra_23
    .byte <sra_24
    .byte <sra_25
    .byte <sra_26
    .byte <sra_27
    .byte <sra_28
    .byte <sra_29
    .byte <sra_30
    .byte <sra_31

sra_dispatch_hi:
    .byte >sra_0
    .byte >sra_1
    .byte >sra_2
    .byte >sra_3
    .byte >sra_4
    .byte >sra_5
    .byte >sra_6
    .byte >sra_7
    .byte >sra_8
    .byte >sra_9
    .byte >sra_10
    .byte >sra_11
    .byte >sra_12
    .byte >sra_13
    .byte >sra_14
    .byte >sra_15
    .byte >sra_16
    .byte >sra_17
    .byte >sra_18
    .byte >sra_19
    .byte >sra_20
    .byte >sra_21
    .byte >sra_22
    .byte >sra_23
    .byte >sra_24
    .byte >sra_25
    .byte >sra_26
    .byte >sra_27
    .byte >sra_28
    .byte >sra_29
    .byte >sra_30
    .byte >sra_31

.align 256
