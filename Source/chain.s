; chain.s -- very short program to load and run riscv65

; Load from this device
DEVICE = 8

; Kernal entry points
LOAD = $FFD5
SETLFS = $FFBA
SETNAM = $FFBD

.org $02A7

    ; Set the device number, and the flag to load at the address in the file
    lda #0      ; dummy
    ldx #DEVICE
    ldy #1      ; load at address specified in file
    jsr SETLFS

    ; Set the name of the file to load
    lda #end_name-name-1
    ldx #<name
    ldy #>name
    jsr SETNAM

    lda #0
    ; Will load at address specified in file ($0880)
    jsr LOAD

    ; Enter the new program
    jmp $0880

name: .asciiz "RISCV65"
end_name:
