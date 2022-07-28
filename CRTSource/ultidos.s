; ultidos.c -- interface to UltiDOS on 1541-Ultimate and Ultimate 64

.include "memory.inc"
.include "ultidos.inc"

; Addresses for command interface
CMD_CONTROL        = $DF1C
CMD_STATUS         = $DF1C
CMD_DATA           = $DF1D
CMD_IDENTIFICATION = $DF1D
CMD_RESPONSE_DATA  = $DF1E
CMD_STATUS_DATA    = $DF1F

; CMD_CONTROL bits
CMD_PUSH_CMD = $01
CMD_DATA_ACC = $02
CMD_ABORT    = $04
CMD_CLR_ERR  = $08

; CMD_STATUS bits
CMD_BUSY       = $01
;CMD_DATA_ACC  = $02
CMD_ABORT_P    = $04
CMD_ERROR      = $08
CMD_STATE_MASK = $30
CMD_STATE_IDLE = $00
CMD_STATE_BUSY = $10
CMD_STATE_LAST = $20
CMD_STATE_MORE = $30
CMD_STAT_AV    = $40 ; bit CMD_STATUS sets V to this bit
CMD_DATA_AV    = $80 ; bit CMD_STATUS sets N to this bit

; UltiDOS commands
DOS_CMD_IDENTIFY       = $01
DOS_CMD_OPEN_FILE      = $02
DOS_CMD_CLOSE_FILE     = $03
DOS_CMD_READ_DATA      = $04
DOS_CMD_WRITE_DATA     = $05
DOS_CMD_FILE_SEEK      = $06
DOS_CMD_FILE_INFO      = $07
DOS_CMD_FILE_STAT      = $08
DOS_CMD_DELETE_FILE    = $09
DOS_CMD_RENAME_FILE    = $0A
DOS_CMD_COPY_FILE      = $0B
DOS_CMD_CHANGE_DIR     = $11
DOS_CMD_GET_PATH       = $12
DOS_CMD_OPEN_DIR       = $13
DOS_CMD_READ_DIR       = $14
DOS_CMD_CREATE_DIR     = $16
DOS_CMD_COPY_HOME_PATH = $17
DOS_CMD_LOAD_REU       = $21
DOS_CMD_SAVE_REU       = $22
DOS_CMD_MOUNT_DISK     = $23
DOS_CMD_UMOUNT_DISK    = $24
DOS_CMD_SWAP_DISK      = $25
DOS_CMD_GET_TIME       = $26
DOS_CMD_SET_TIME       = $27
DOS_CMD_ECHO           = $F0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; These functions may be called from other banks, and cannot pass parameters
; or return values in registers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Initialize the command interface
; Return 0 in pointer1 if the command interface is present

.global dos_init
.proc dos_init

    ; Make sure we have a command interface
    lda CMD_IDENTIFICATION
    cmp #$C9
    bne no_interface

    ; Pass clear-error and abort
    lda #CMD_CLR_ERR | CMD_ABORT
    sta CMD_CONTROL

    ; Wait for ready
    wait:
        lda CMD_STATUS
        and #CMD_ABORT_P
        bne wait

    lda #0
    sta pointer1+0
    sta pointer1+1
    rts

no_interface:
    lda #$FF
    sta pointer1+0
    sta pointer1+1
    rts

.endproc

; Open a file to the given target (1 or 2)
; pointer1 is set to the target (1 or 2)
; pointer2 is set to the address of the path.  The path ends with a zero byte.
; pointer3 is set to the mode.  The mode is FA_READ, FA_WRITE or
; FA_READ|FA_WRITE; FA_WRITE may be combined with FA_CREATE_NEW or
; FA_CREATE_ALWAYS.
; Status string is returned in ultidos status. pointer1 is 0 on success.
; pointer2 may be altered.

.global dos_open
.proc dos_open

    lda pointer1+0  ; target
    sta CMD_DATA
    lda #DOS_CMD_OPEN_FILE
    sta CMD_DATA
    lda pointer3+0  ; mode
    sta CMD_DATA
    ldy pointer2+0  ; path, low byte
    lda #0
    sta pointer2+0  ; pointer2+y points to a byte in the path
    path_loop:
        lda (pointer2),y ; byte from path
        beq end_path_loop
        sta CMD_DATA
    iny
    bne path_loop
    inc pointer2+1
    bne path_loop
    end_path_loop:

    jsr start_command
    jmp end_command

.endproc

; Close the file on the given target (1 or 2)
; pointer1 is set to the target (1 or 2)
; Status string is returned in ultidos status. pointer1 is 0 on success.

.global dos_close
.proc dos_close

    lda pointer1+0  ; target
    sta CMD_DATA
    lda #DOS_CMD_CLOSE_FILE
    sta CMD_DATA

    jsr start_command
    jmp end_command

.endproc

; Seek the file on the given target (1 or 2) to the given position.
; pointer1 is set to the target (1 or 2).
; longreg1 is set to the position.
; Status string is returned in ultidos status. pointer1 is 0 on success.

.global dos_seek
.proc dos_seek

    lda pointer1+0  ; target
    sta CMD_DATA
    lda #DOS_CMD_FILE_SEEK
    sta CMD_DATA
    lda longreg1+0  ; position
    sta CMD_DATA
    lda longreg1+1
    sta CMD_DATA
    lda longreg1+2
    sta CMD_DATA
    lda longreg1+3

    jsr start_command
    jmp end_command

.endproc

; Read from the file on the given target (1 or 2) to the given address
; pointer1 is set to the target (1 or 2).
; pointer2 is set to the address to receive the data.
; pointer3 is set to the length to read.
; Status string is returned in ultidos status. pointer1 is 0 on success.
; pointer3 returns the actual length read.
; pointer2 may be altered.

.global dos_read
.proc dos_read

    lda pointer1+0  ; target
    sta CMD_DATA
    lda #DOS_CMD_READ_DATA
    sta CMD_DATA
    lda pointer3+0  ; length
    sta CMD_DATA
    lda pointer3+1
    sta CMD_DATA

    jsr start_command

    ; Count total bytes read
    lda #0
    sta pointer3+0
    sta pointer3+1

    ; Set up pointer2 and y for fastest indexing
    ldy pointer2+0
    sta pointer2+0

    ; Outer loop runs once per 512-byte block
    read_outer:
        ; Set count that will reach zero when 512 bytes are read
        ldx #0
        lda #2
        sta pointer1+0
        ; Inner loop runs once per byte
        read_inner:
            ; Check for end of data
            bit CMD_STATUS
            bpl end_read
            ; Read one byte
            lda CMD_RESPONSE_DATA
            sta (pointer2),y
            iny
            bne :+
                inc pointer2+1
            :
            ; Count the bytes read
            inc pointer3+0
            bne :+
                inc pointer3+1
            :
        ; Count off 512 bytes
        inx
        bne read_inner
        dec pointer1+0
        bne read_inner
        ; Acknowledge after 512 bytes
        lda #CMD_DATA_ACC
        sta CMD_CONTROL
    bne read_outer

end_read:
    ; Get status and set pointer1
    jmp end_command

.endproc

; Read from the file on the given target (1 or 2) to the REU at the given
; address.
; pointer1 is set to the target (1 or 2).
; longreg1 is set to the REU address.
; longreg2 is set to the transfer size.
; Status string is returned in ultidos status. pointer1 is 0 on success.

.global dos_read_reu
.proc dos_read_reu

    lda pointer1+0  ; target
    sta CMD_DATA
    lda #DOS_CMD_LOAD_REU
    sta CMD_DATA
    lda longreg1+0  ; address
    sta CMD_DATA
    lda longreg1+1
    sta CMD_DATA
    lda longreg1+2
    sta CMD_DATA
    lda longreg1+3
    sta CMD_DATA
    lda longreg2+0  ; length
    sta CMD_DATA
    lda longreg2+1
    sta CMD_DATA
    lda longreg2+2
    sta CMD_DATA
    lda longreg2+3
    sta CMD_DATA

    jsr start_command
    ; TODO: The data string returns $<n> BYTES LOADED TO REU $<n>. Do we need this?
    jmp end_command

.endproc

; Write to the file on the given target (1 or 2) from the given address.
; pointer1 is set to the target (1 or 2).
; pointer2 is set to the address to provide the data.
; pointer3 is set to the length to write.
; Status string is returned in ultidos status. pointer1 is 0 on success.
; pointer2 and pointer3 may be altered.

.global dos_write
.proc dos_write

    ; end_command overwrites pointer1, so save the target in X
    ldx pointer1+0 ; target

    ; If writing 512 bytes or more, split into more than one command
    loop_write_512:
        lda pointer3+1
        sec
        sbc #2
        bcc end_write_512
        sta pointer3+1

        ; Write a block of 512 bytes:
        ; Write the command header
        stx CMD_DATA
        lda #DOS_CMD_WRITE_DATA
        sta CMD_DATA
        ; two dummy bytes afterwards
        sta CMD_DATA
        sta CMD_DATA

        ; Write 256 bytes
        ldy #0
        write_1:
            lda (pointer2),y
            sta CMD_DATA
        iny
        bne write_1
        inc pointer2+1

        ; Write 256 more bytes
        ldy #0
        write_2:
            lda (pointer2),y
            sta CMD_DATA
        iny
        bne write_2
        inc pointer2+1

        ; Run the command
        jsr start_command
        jsr end_command
        lda pointer1+0
        ora pointer1+1
        bne end_write ; command returned error

    jmp loop_write_512
    end_write_512:

    ; Write any remaining portion
    lda pointer3+0
    ora pointer3+1
    beq end_write

        ; Write the command header
        stx CMD_DATA
        lda #DOS_CMD_WRITE_DATA
        sta CMD_DATA
        ; two dummy bytes afterwards
        sta CMD_DATA
        sta CMD_DATA

        lda pointer3+1
        beq do_last_write

            ; Write 256 bytes
            ldy #0
            write_3:
                lda (pointer2),y
                sta CMD_DATA
            iny
            bne write_3
            inc pointer2+1

        do_last_write:
        lda pointer3+0
        beq do_write

            ; Write remaining block
            ldy #0
            write_4:
                lda (pointer2),y
                sta CMD_DATA
            iny
            cpy pointer3+0
            bne write_4

        do_write:
        jsr start_command
        jsr end_command

end_write:
    rts

.endproc

; Write to the file on the given target (1 or 2) from the REU at the given
; address.
; pointer1 is set to the target (1 or 2).
; longreg1 is set to the REU address.
; longreg2 is set to the transfer size.
; Status string is returned in ultidos status. pointer1 is 0 on success.

.global dos_write_reu
.proc dos_write_reu

    lda pointer1+0  ; target
    sta CMD_DATA
    lda #DOS_CMD_SAVE_REU
    sta CMD_DATA
    lda longreg1+0  ; address
    sta CMD_DATA
    lda longreg1+1
    sta CMD_DATA
    lda longreg1+2
    sta CMD_DATA
    lda longreg1+3
    sta CMD_DATA
    lda longreg2+0  ; length
    sta CMD_DATA
    lda longreg2+1
    sta CMD_DATA
    lda longreg2+2
    sta CMD_DATA
    lda longreg2+3
    sta CMD_DATA

    jsr start_command
    ; TODO: The data string returns $<n> BYTES SAVED FROM REU $<n>. Do we need this?
    jmp end_command

.endproc

; Change the current directory.
; pointer1 is set to the target (1 or 2).
; pointer2 points to the path. The path ends in a zero byte.
; Status string is returned in ultidos status. pointer1 is 0 on success.
; pointer2 may be altered.

.global dos_change_dir
.proc dos_change_dir

    lda pointer1+0  ; target
    sta CMD_DATA
    lda #DOS_CMD_CHANGE_DIR
    sta CMD_DATA

    ; Arrange pointer2 and y for fastest indexing
    ldy pointer2+0
    lda #0
    sta pointer2+0

    ; Pass the path to the command interface
    path_loop:
        lda (pointer2),y
        beq end_path_loop
        sta CMD_DATA
    iny
    bne path_loop
    inc pointer2+1
    bne path_loop
    end_path_loop:

    jsr start_command
    jmp end_command

.endproc

; Open the current directory.
; pointer1 is set to the target (1 or 2).
; Status string is returned in ultidos status. pointer1 is 0 on success.
; pointer1 is 1 if the directory is empty.

.global dos_open_dir
.proc dos_open_dir

    lda pointer1+0  ; target
    sta CMD_DATA
    lda #DOS_CMD_OPEN_DIR
    sta CMD_DATA

    jsr start_command
    jmp end_command

.endproc

; Read the first directory entry.
; pointer1 is set to the target (1 or 2).
; pointer2 points to the buffer that receives the path. This should be 256 bytes long.
; Status string is returned in ultidos status. pointer1 is 0 on success.
; pointer3 returns the file attributes.

.global dos_read_dir_first
.proc dos_read_dir_first

    lda pointer1+0  ; target
    sta CMD_DATA
    lda #DOS_CMD_READ_DIR
    sta CMD_DATA

    jsr start_command

    ; Set pointer3 to 0 and set an empty path in case of error
    ldy #0          ; path is initially empty
    sty pointer3+0
    sty pointer3+1

    ; Read the file attributes

    bit CMD_STATUS
    bpl end_read
    lda CMD_RESPONSE_DATA
    sta pointer3+0

    ; Read the directory entry
    read_loop:
        bit CMD_STATUS
        bpl end_read
        lda CMD_RESPONSE_DATA
        sta (pointer2),y
    iny
    cpy #255
    bne read_loop

end_read:
    ; Mark the end of the path
    lda #0
    sta (pointer2),y
    jmp end_command

.endproc

; Read next directory entry.
; This works on the target used for the last call to dos_read_dir_first.
; pointer2 points to the buffer that will receive the path. This should be 256 bytes long.
; pointer3 returns the file attributes.
; Return pointer1 != 0 if end of directory.

.global dos_read_dir_next
.proc dos_read_dir_next

    ; Wait for ready
    wait:
        lda CMD_STATUS
        and #CMD_STATE_MASK
        cmp #CMD_STATE_BUSY
        beq wait

    ; Set path empty and pointer3 == 0 in case of error
    ldy #0
    sty pointer3+0
    sty pointer3+1

    ; If idle, we're at the end of the list
    cmp #CMD_STATE_IDLE
    beq end_of_directory

    ; Read the file attributes
    bit CMD_STATUS
    bpl end_read
    lda CMD_RESPONSE_DATA
    sta pointer3+0

    ; Read the path
    read_loop:
        bit CMD_STATUS
        bpl end_read
        lda CMD_RESPONSE_DATA
        sta (pointer2),y
    iny
    cpy #255
    bne read_loop

end_read:

    ; Acknowledge the read
    lda #CMD_DATA_ACC
    sta CMD_CONTROL

    ; Mark the end of the string and indicate success
    lda #0
    sta (pointer2),y
    sta pointer1+0
    sta pointer1+1
    rts

end_of_directory:
    lda #$FF
    sta pointer1+0
    sta pointer1+1
    rts

.endproc

; Read the current time.
; pointer1 is ignored, and target 1 is used, because the time is the same for
; both targets.
; pointer2 points to a buffer to receive the time. This should have 20 bytes.
; Status string is returned in ultidos status. pointer1 is 0 on success.

.proc dos_get_time

    lda #1
    sta CMD_DATA
    lda #DOS_CMD_GET_TIME
    sta CMD_DATA
    lda #0
    sta CMD_DATA

    jsr start_command
    jmp end_command

.endproc

; Run a command that has been passed to the command interface

.proc start_command

    ; Start the command
    lda #CMD_PUSH_CMD
    sta CMD_CONTROL

    ; Wait for completion
    wait_loop:
        lda CMD_STATUS
        and #CMD_STATE_MASK
        cmp #CMD_STATE_BUSY
        beq wait_loop

    rts

.endproc

; Read status from the command interface, end the command and set pointer1
; to the numeric code that begins ultidos_status.
.proc end_command

    ; Read from the status channel
    ldx #0
    read_loop:

        bit CMD_STATUS
        bvc end_read_loop ; No more status
        lda CMD_STATUS_DATA
        sta ultidos_status,x

    inx
    cpx #255        ; Check for buffer full
    bne read_loop
    end_read_loop:
    ; Null terminate the status string
    lda #0
    sta ultidos_status,x

    ; End the command

    lda #CMD_DATA_ACC
    sta CMD_CONTROL

    ; Set pointer1 to the numeric code that begins ultidos_status.
    ; ultidos_status may be empty, indicating no error, or may begin with
    ; two digits indicating 00 (no error) or some other status.

    ; First digit
    lda ultidos_status+0
    beq return_ok ; String is empty; return 0 (OK)
    sec
    sbc #'0'
    cmp #10
    bcs return_unknown ; Not a digit; return -1 (unknown error)
    tax

    ; Second digit
    lda ultidos_status+1
    sec
    sbc #'0'
    cmp #10
    bcs return_unknown ; Not a digit; return -1 (unknown error)

    ; We have two digits
    clc
    adc ten,x
    sta pointer1+0
    lda #0
    sta pointer1+1
    rts

return_ok:
    lda #0
    sta pointer1+0
    sta pointer1+1
    rts

return_unknown:
    lda #$FF
    sta pointer1+0
    sta pointer1+1
    rts

.endproc

.rodata
ten: .byte 0, 10, 20, 30, 40, 50, 60, 70, 80, 90
