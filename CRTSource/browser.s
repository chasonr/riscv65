; browser.s -- Show a hierarchial display of the available files and allow
; the user to select one to run

.include "kernal.inc"
.include "memory.inc"
.include "reu.inc"
.include "ultidos.inc"

; Command interface target used by the browser
DIR_TARGET = 2

; Addresses for screen access
SCREEN_MEM = $0400
COLOR_MEM = $D800

; Screen colors
COLOR_BLACK      = $00
COLOR_WHITE      = $01
COLOR_RED        = $02
COLOR_CYAN       = $03
COLOR_VIOLET     = $04
COLOR_GREEN      = $05
COLOR_BLUE       = $06
COLOR_YELLOW     = $07
COLOR_ORANGE     = $08
COLOR_BROWN      = $09
COLOR_LIGHTRED   = $0A
COLOR_GRAY1      = $0B
COLOR_GRAY2      = $0C
COLOR_LIGHTGREEN = $0D
COLOR_LIGHTBLUE  = $0E
COLOR_GRAY3      = $0F

CH_CURS_UP    = 145
CH_CURS_DOWN  =  17
CH_CURS_LEFT  = 157
CH_CURS_RIGHT =  29
CH_F1         = 133
CH_F7         = 136
CH_ENTER      =  13

; Scratch area used for REU transfer

file_attr = scratch_area+0
file_name = file_attr+1

; Enter the file browser here

.global browser
.proc browser

    ; Set the screen colors

    lda $D020
    sta browser_old_border_color
    lda $D021
    sta browser_old_background_color
    lda #COLOR_GRAY1
    sta $D020
    lda #COLOR_BLACK
    sta $D021

    ; Set the initial path

    lda #'/'
    sta browser_path+0
    lda #0
    sta browser_path+1

    ; Load files from the root and display them

    jsr load_files
    jsr paint_screen

    ; enter directory: right (CH_CURS_RIGHT)
    ; leave directory: left (CH_CURS_LEFT)
    ; change selection: up (CH_CURS_UP), down (CH_CURS_DOWN)
    ; select: return (CH_ENTER)
    ; page up: F1 (CH_F1)
    ; page down: F7 (CH_F7)
    ;
    ; letters, digits (PETSCII): match names; ? wildcard

    main_loop:

        ; Wait for a key
        :
            jsr GETIN
        cmp #0
        beq :-

        ; If it's a PETSCII printable character, use it to match a file name
        tax
        lda rshift5,x
        cmp #1      ; $20 to $3F
        beq do_search
        cmp #2      ; $40 to $5F
        beq do_search
        cmp #6      ; $C0 to $DF
        bne clear_search
        do_search:
            txa
            jsr search
            jsr paint_directory
            jmp main_loop

        clear_search:

            ; Clear search string on cursor control etc.
            txa
            pha
            lda #0
            sta browser_search_str+0
            ; Clear any error messages
            jsr paint_directory
            pla

            ; Process control codes
            cmp #CH_CURS_UP
            bne :+
                jsr cursor_up
                jmp main_loop
            :
            cmp #CH_CURS_DOWN
            bne :+
                jsr cursor_down
                jmp main_loop
            :
            cmp #CH_CURS_LEFT
            bne :+
                jsr cursor_left
                jmp main_loop
            :
            cmp #CH_CURS_RIGHT
            bne :+
                jsr cursor_right
                jmp main_loop
            :
            cmp #CH_F1
            bne :+
                jsr page_up
                jmp main_loop
            :
            cmp #CH_F7
            bne :+
                jsr page_down
                jmp main_loop
            :
            cmp #CH_ENTER
            bne :+
                jsr select_file
                bcc end_main_loop
                jmp main_loop
            :
            jmp main_loop

    end_main_loop:

    ; Set the colors back to their previous settings
    lda browser_old_border_color
    sta $D020
    lda browser_old_background_color
    sta $D021

    ; Clear the screen
    lda #$93
    jsr CHROUT

    rts

.endproc

; Load the files from the current directory

.proc load_files

    ; Start off with an empty directory
    lda #0
    sta browser_num_files+0
    sta browser_num_files+1

    ; Ensure that we can open the directory
    lda #DIR_TARGET
    sta pointer1+0
    jsr dos_close

    ; Change to the current directory
    lda #DIR_TARGET
    sta pointer1+0
    lda #<browser_path
    sta pointer2+0
    lda #>browser_path
    sta pointer2+1
    jsr dos_change_dir
    lda pointer1+0
    cmp #10
    bcs file_error

    ; Open the current directory
    lda #DIR_TARGET
    sta pointer1+0
    jsr dos_open_dir
    lda pointer1+0
    cmp #10
    bcs file_error

    ; Begin reading the directory
    lda #DIR_TARGET
    sta pointer1+0
    lda #<file_name
    sta pointer2+0
    lda #>file_name
    sta pointer2+1
    jsr dos_read_dir_first
    lda pointer1+0
    cmp #10
    bcs file_error

    ; Save this entry and read the next one
    load_loop:

        ; Save the entry
        lda pointer3+0
        sta file_attr
        lda browser_num_files+0
        sta pointer1+0
        lda browser_num_files+1
        sta pointer1+1
        jsr save_dir_entry
        inc browser_num_files+0
        bne :+
            inc browser_num_files+1
        :

        ; Read another one
        lda #<file_name
        sta pointer2+0
        lda #>file_name
        sta pointer2+1
        jsr dos_read_dir_next

    lda pointer1+0
    ora pointer1+1
    beq load_loop

    ; Close the directory
    lda #DIR_TARGET
    sta pointer1+0
    jsr dos_close

    ; Reset the cursor, position and search string
    lda #0
    sta browser_top_row+0
    sta browser_top_row+1
    sta browser_cursor_pos+0
    sta browser_cursor_pos+1
    sta browser_search_str+0

    rts

file_error:

    ldx #<ultidos_status
    ldy #>ultidos_status
    jsr paint_error

    ; Close the directory
    lda #DIR_TARGET
    sta pointer1+0
    jsr dos_close

    rts

.endproc

; Write the directory entry from file_attr and file_name to the REU
; pointer1 contains the entry number

.proc save_dir_entry

    ; reu_xmem_address <- pointer1 * 257
    lda pointer1+0
    sta reu_xmem_address_0
    clc
    lda pointer1+0
    adc pointer1+1
    sta reu_xmem_address_1
    lda pointer1+1
    adc #0
    sta reu_xmem_address_2

    ; Retrieve the record from the REU
    set_local_address scratch_area
    set_xfer_size_imm 257
    do_reu_write

    rts

.endproc

; Load the directory entry from the REU into file_attr and file_name
; pointer1 contains the entry number

.proc load_dir_entry

    ; reu_xmem_address <- pointer1 * 257
    lda pointer1+0
    sta reu_xmem_address_0
    clc
    lda pointer1+0
    adc pointer1+1
    sta reu_xmem_address_1
    lda pointer1+1
    adc #0
    sta reu_xmem_address_2

    ; Retrieve the record from the REU
    set_local_address scratch_area
    set_xfer_size_imm 257
    do_reu_read

    rts

.endproc

; Add the character in A to the search string, and find the first match

.proc search

    ; Save the character
    tay

    ldx #0
    length_loop:
        lda browser_search_str,x
        beq end_length_loop
    inx
    cpx #40
    bcc length_loop
    ; If we exit this way, the search string is full
    rts

    end_length_loop:

    ; Add the character to the search string
    tya
    sta browser_search_str,x
    inx
    cpx #40
    bcs :+
        lda #0
        sta browser_search_str,x
    :

    ; Iterate 0 to browser_num_files
    lda #0
    sta pointer1+0
    sta pointer1+1
    beq check_match_loop
    match_loop:

        jsr load_dir_entry
        jsr search_match
        beq got_match

    inc pointer1+0
    bne :+
        inc pointer1+1
    :

    check_match_loop:
    lda pointer1+0
    cmp browser_num_files+0
    lda pointer1+1
    sbc browser_num_files+1
    bcc match_loop

    ; If we exit by this path, no match was found
    rts

    got_match:

    ; Set the cursor to the matching row

    sec
    lda pointer1+0
    sta browser_cursor_pos+0
    lda pointer1+1
    sta browser_cursor_pos+1
    jmp set_top_row

.endproc

; Return A == 0 if browser_search_str matches file_name
; browser_search_str is PETSCII; file_name is ASCII

.proc search_match

    ldx #0
    match_loop:

        ; Get character from browser_search_str
        lda browser_search_str,x
        beq got_match

        cmp #'?'
        bne do_match
            ; Wild card; any nonempty character in file_name matches
            lda file_name,x
            beq no_match
            bne next_match

        do_match:
            ; Convert to uppercase
            tay
            sec
            sbc #'a'
            cmp #26
            bcs :+
                clc
                adc #'A'
                tay
            :
            sty pointer4+0

            ; Get the character from file_name
            lda file_name,x
            beq no_match
            tay
            lda ascii_to_petscii,y
            ; Convert to uppercase
            tay
            sec
            sbc #'a'
            cmp #26
            bcs :+
                clc
                adc #'A'
                tay
            :
            ; Check for match
            cpy pointer4+0
            bne no_match

    next_match:
    inx
    cpx #40
    bcc match_loop

    got_match:
    lda #$00
    rts

    no_match:
    lda #$FF
    rts

.endproc

; Select the file at the cursor
; Return C clear if selected, or set if error

.proc select_file

    ; Display error if no file

    lda browser_num_files+0
    ora browser_num_files+1
    bne :+
        ldx #<no_file
        ldy #>no_file
        jsr paint_error
        sec
        rts
        no_file: .byte "No file", 0
    :

    ; Retrieve the record from the REU
    lda browser_cursor_pos+0
    sta pointer1+0
    lda browser_cursor_pos+1
    sta pointer1+1
    jsr load_dir_entry

    ; If the selection is a directory, enter it
    lda file_attr
    and #FA_DIRECTORY
    beq :+
        jsr cursor_right
        sec
        rts
    :

    ; Add the new component to the path and return C set or clear
    jmp add_to_path

.endproc

; Move up one row

.proc cursor_up

    ; Skip if already at the top
    lda browser_cursor_pos+0
    ora browser_cursor_pos+1
    beq end1

        ; browser_cursor_pos -= 1
        sec
        lda browser_cursor_pos+0
        sbc #1
        sta browser_cursor_pos+0
        lda browser_cursor_pos+1
        sbc #0
        sta browser_cursor_pos+1

        jmp set_top_row

    end1:
    rts

.endproc

; Move down one row

.proc cursor_down

    ; pointer1 <- browser_cursor_pos+1
    clc
    lda browser_cursor_pos+0
    adc #1
    sta pointer1+0
    lda browser_cursor_pos+1
    adc #0
    sta pointer1+1

    ; Skip if that is >= browser_num_files
    lda pointer1+0
    cmp browser_num_files+0
    lda pointer1+1
    sbc browser_num_files+1
    bcs end1

        ; Update the cursor position
        lda pointer1+0
        sta browser_cursor_pos+0
        lda pointer1+1
        sta browser_cursor_pos+1

        jmp set_top_row

    end1:
    rts

.endproc

; Set the top row after changing the cursor position

.proc set_top_row

    ; Set to cursor position minus eleven...

    sec
    lda browser_cursor_pos+0
    sbc #11
    sta browser_top_row+0
    lda browser_cursor_pos+1
    sbc #0
    sta browser_top_row+1

    ; ...but not less than zero
    bcs :+
        lda #0
        sta browser_top_row+0
        sta browser_top_row+1
        beq end
    :

    ; If fewer than 24 rows visible...

    sec
    lda browser_num_files+0
    sbc browser_top_row+0
    tax
    lda browser_num_files+1
    sbc browser_top_row+1
    bne end     ; at least 256 rows visible
    cpx #24
    bcs end     ; at least 24 rows visible

        ; ...set to number of files minus 24...

        sec
        lda browser_num_files+0
        sbc #24
        sta browser_top_row+0
        lda browser_num_files+1
        sbc #0
        sta browser_top_row+1

        ; ...but not less than zero

        bcs end
            lda #0
            sta browser_top_row+0
            sta browser_top_row+1

    end:
    jmp paint_screen

.endproc

; Move to the next higher directory

.proc cursor_left

    ; Get the length of the current path
    jsr get_path_length

    ; Trim any trailing slashes
    jsr trim_slashes

    ; Notify user if the resulting path is empty
    cpy #0
    bne path_ok

        ldx #<path_empty
        ldy #>path_empty
        jmp paint_error
        path_empty: .byte "Already at root directory", 0

    path_ok:

    ; Remove the last component of the path
    trim_loop:
        lda browser_path-1,y
        cmp #'/'
        beq end_trim_loop
    dey
    bne trim_loop
    end_trim_loop:

    ; Remove slashes before that component
    jsr trim_slashes

    ; Convert empty path to "/"

    cpy #0
    bne :+
        lda #'/'
        sta browser_path+0
        iny
    :

    ; Set new end of path
    lda #0
    sta browser_path,y

    jsr load_files
    jmp paint_screen

.endproc

; Enter selected directory

.proc cursor_right

    ; Error if no files
    lda browser_num_files+0
    ora browser_num_files+1
    bne got_entry

        ldx #<no_directory
        ldy #>no_directory
        jmp paint_error
        no_directory: .byte "No directory", 0

    got_entry:

    ; Retrieve the record from the REU
    lda browser_cursor_pos+0
    sta pointer1+0
    lda browser_cursor_pos+1
    sta pointer1+1
    jsr load_dir_entry

    ; The selection must be a directory
    lda file_attr
    and #FA_DIRECTORY
    bne got_directory

        ldx #<not_a_directory
        ldy #>not_a_directory
        jmp paint_error
        not_a_directory: .byte "Not a directory", 0

    got_directory:

    ; Add the new component to the path
    jsr add_to_path

    ; Length of existing path
    jsr get_path_length
    bcs end1

    jsr load_files
    jsr paint_screen

    end1:
    rts

.endproc

; Y <- length of browser_path

.proc get_path_length

    ldy #0
    length:
        lda browser_path,y
        beq end_length
    iny
    bne length

    end_length:
    rts

.endproc

; Given path length in Y, decrement until trailing slashes removed

.proc trim_slashes

    cpy #0
    beq end_slash_loop
    slash_loop:

        lda browser_path-1,y
        cmp #'/'
        bne end_slash_loop

    dey
    bne slash_loop
    end_slash_loop:

    rts

.endproc

; Add a new component to the path
; New component is in file_name
; Return C set if the new path is too long

.proc add_to_path

    ; Save length of path in case of error
    jsr get_path_length
    sty pointer1+0

    ; Trim any trailing slashes
    jsr trim_slashes

    ; Add one slash
    lda #'/'
    sta browser_path,y
    iny
    beq too_long

    ; Append the new component
    ldx #0
    append_loop:
        lda file_name,x
        beq done
        sta browser_path,y
    inx
    iny
    bne append_loop

    ; If we exit this way, the path is too long
    too_long:
        ; Restore the path to its saved length
        ldy pointer1+0
        lda #0
        sta browser_path,y
        ldx #<path_too_long
        ldy #>path_too_long
        jsr paint_error
        sec
        rts
        path_too_long: .byte "Path is too long", 0

    done:
    ; Set new length of path
    lda #0
    sta browser_path,y

    clc
    rts

.endproc

; Move up one page of 24 lines

.proc page_up

    ; browser_cursor_pos -= 24 but not less than 0
    sec
    lda browser_cursor_pos+0
    sbc #24
    sta browser_cursor_pos+0
    lda browser_cursor_pos+1
    sbc #0
    sta browser_cursor_pos+1
    bcs :+
        lda #0
        sta browser_cursor_pos+0
        sta browser_cursor_pos+1
    :

    jmp set_top_row

.endproc

; Move down one page of 24 lines

.proc page_down

    ; Skip if no files
    lda browser_num_files+0
    ora browser_num_files+1
    beq end1

        ; browser_cursor_pos += 24

        clc
        lda browser_cursor_pos+0
        adc #24
        sta browser_cursor_pos+0
        lda browser_cursor_pos+1
        adc #0
        sta browser_cursor_pos+1

        ; Don't go past the last row

        lda browser_cursor_pos+0
        cmp browser_num_files+0
        lda browser_cursor_pos+1
        sbc browser_num_files+1
        bcc :+
            sec
            lda browser_num_files+0
            sbc #1
            sta browser_cursor_pos+0
            lda browser_num_files+1
            sbc #0
            sta browser_cursor_pos+1
        :

        jmp set_top_row

    end1:
    rts

.endproc

; Refresh the screen

.proc paint_screen

    ; Inform the user if the directory is empty

    lda browser_num_files+0
    ora browser_num_files+1
    bne got_files

        ; The directory is empty

        lda #COLOR_RED
        ldy #39
        no_files_color:
            sta COLOR_MEM,y
        dey
        bpl no_files_color
        ldy #39
        no_files_text:
            lda no_files,y
            tax
            lda petscii_to_screen,x
            sta SCREEN_MEM,y
        dey
        bpl no_files_text

        lda #1
        sta pointer2+0
        bne print_blank_lines

        no_files: .byte "No files in this directory              "

    got_files:

        ; browser_top_row to pointer1
        lda browser_top_row+0
        sta pointer1+0
        lda browser_top_row+1
        sta pointer1+1
        ; pointer2 counts rows
        lda #0
        sta pointer2+0
        beq check_row_loop
        row_loop:

            jsr paint_filename

        inc pointer2+0 ; row count
        inc pointer1+0 ; current row
        bne :+
            inc pointer1+1
        :

        check_row_loop:
        ; Break loop if pointer1 >= browser_num_files or pointer2 >= 24
        lda pointer1+0
        cmp browser_num_files+0
        lda pointer1+1
        sbc browser_num_files+1
        bcs end_row_loop
        lda pointer2+0
        cmp #24
        bcc row_loop
        end_row_loop:

    print_blank_lines:

    ; Fill the rest of the screen with blank lines

    ldx pointer2+0
    jmp check_blank_loop
    blank_loop:

        jsr paint_blank

    inx
    check_blank_loop:
    cpx #24
    bcc blank_loop

    ; Paint the directory at the end
    jmp paint_directory

.endproc

; Display a single file name
; pointer1 contains the index of the file

.proc paint_filename

    ; Retrieve the REU record for the selected file
    jsr load_dir_entry

    ; Determine the row to paint

    sec
    lda pointer1+0
    sbc browser_top_row+0
    tax
    lda pointer1+1
    sbc browser_top_row+1
    bne end0
    cpx #24
    bcc :+
    end0:
        rts
    :

    lda screen_row_lo,x
    sta pointer4+0
    lda screen_row_hi,x
    ora #>COLOR_MEM
    sta pointer4+1

    ; Set the color of the row

    lda pointer1+0
    cmp browser_cursor_pos+0
    bne not_cursor
    lda pointer1+1
    cmp browser_cursor_pos+1
    bne not_cursor

        ; The current row is at the cursor position

        lda file_attr
        and #FA_DIRECTORY
        bne dir_at_cursor
        beq file_at_cursor

    not_cursor:

        ; The current row is not at the cursor position

        lda file_attr
        and #FA_DIRECTORY
        bne dir_not_at_cursor

    file_not_at_cursor:
        lda #COLOR_LIGHTBLUE
        .byte $2C
    dir_not_at_cursor:
        lda #COLOR_BROWN
        .byte $2C
    file_at_cursor:
        lda #COLOR_WHITE
        .byte $2C
    dir_at_cursor:
        lda #COLOR_ORANGE

    set_color:
    ldy #39
    color_loop:
        sta (pointer4),y
    dey
    bpl color_loop

    ; Point to screen memory
    lda pointer4+1
    and #$07
    ora #>SCREEN_MEM
    sta pointer4+1

    ; Paint up to 40 characters
    ldy #0
    name_loop:
        lda file_name,y
        beq pad_name
        tax
        lda ascii_to_screen,x
        sta (pointer4),y
    iny
    cpy #40
    bcc name_loop

    ; If we exit this way, the name has at least 40 characters

    lda file_name,y
    beq end1

        ; The name exceeds the width of the screen
        dey         ; to rightmost column (39)
        ldx #'>'
        lda petscii_to_screen,x
        sta (pointer4),y
        ; Point to color memory and set color to yellow
        lda pointer4+1
        and #$03
        ora #>COLOR_MEM
        sta pointer4+1
        lda #COLOR_YELLOW
        sta (pointer4),y

    end1:
    rts

    ; Branch here to pad the name with spaces
    pad_name:
        ldx #' '
        lda petscii_to_screen,x
        pad_loop:
            sta (pointer4),y
        iny
        cpy #40
        bcc pad_loop

        rts

.endproc

; Display an error message
; Y:X point to the message; it ends with a zero byte

.proc paint_error

    last_screen_row = SCREEN_MEM + 40*24
    last_color_row = COLOR_MEM + 40*24

    ; Save the address of the message

    stx pointer1+0
    sty pointer1+1

    ; Set color to red

    ldx #40
    lda #COLOR_RED
    colors:
        sta last_color_row-1,x
    dex
    bne colors

    ; Show the message

    ldy #0
    message:
        lda (pointer1),y
        beq end_message
        tax
        lda petscii_to_screen,x
        sta last_screen_row,y
    iny
    cpy #40
    bne message
    ; No fill needed if we exit by this path
    rts

    end_message:
    ldx #' '
    lda petscii_to_screen,x
    blank:
        sta last_screen_row,y
    iny
    cpy #40
    bne blank
    rts

.endproc

; Display a blank row
; Row number is in X
; Does not alter X

.proc paint_blank

    lda screen_row_lo,x
    sta pointer1+0
    lda screen_row_hi,x
    ora #>SCREEN_MEM
    sta pointer1+1
    ldy #' '
    lda petscii_to_screen,y
    ldy #39
    blank:
        sta (pointer1),y
    dey
    bpl blank

    rts

.endproc

; Display the current directory or search string

.proc paint_directory

    last_screen_row = SCREEN_MEM + 40*24
    last_color_row = COLOR_MEM + 40*24
    find_len = 6

    ; Set color to gray

    ldx #40
    lda #COLOR_GRAY2
    colors:
        sta last_color_row-1,x
    dex
    bne colors

    ; Is there a search string?
    lda browser_search_str+0
    beq show_directory

        ; Display "Find: "

        ldx #find_len
        find_str:
            lda find-1,x
            tay
            lda petscii_to_screen,y
            sta last_screen_row-1,x
        dex
        bne find_str

        ; Display search string
        ldx #0
        search_str:
            lda browser_search_str,x
            beq end_search_str
            tay
            lda petscii_to_screen,y
            sta last_screen_row+find_len,x
        inx
        cpx #40-find_len
        bcc search_str

        ; If we exit by this path, the search string may exceed the right margin
        lda browser_search_str,x
        beq end1

            ; Display '>' in yellow
            ldy #'>'
            lda petscii_to_screen,y
            sta last_screen_row+39
            lda #COLOR_YELLOW
            sta last_color_row+39
            rts

        end_search_str:
            ; We got the end of the search string and space remains on the screen
            ldy #' '
            lda petscii_to_screen,y
            blank1:
                sta last_screen_row+find_len,x
            inx
            cpx #40-find_len
            bcc blank1

        end1:
        rts

        find: .byte "Find: "

    show_directory:

        ; Will the current directory fit on the screen?
        ldx #0
        dir_len:
            lda browser_path,x
            beq end_path
        inx
        bne dir_len
        end_path:

        cpx #41
        bcs long_dir
            ; Directory is 40 characters or less

            ldx #0
            dir_loop1:
                lda browser_path,x
                beq end_dir_loop1
                tay
                lda ascii_to_screen,y
                sta last_screen_row,x
            inx
            bne dir_loop1
            end_dir_loop1:

            cpx #40
            bcs end2

            ldy #' '
            lda petscii_to_screen,y
            space_loop:
                sta last_screen_row,x
            inx
            cpx #40
            bcc space_loop

            end2:
            rts

        long_dir:
            ; Directory is longer than 40 characters
            txa
            sec
            sbc #39
            clc
            adc #<browser_path
            sta pointer1+0
            lda #0
            adc #>browser_path
            sta pointer1+1

            ; Last 39 characters of directory
            ldy #38
            dir_loop2:
                lda (pointer1),y
                tax
                lda ascii_to_screen,x
                sta last_screen_row+1,y
            dey
            bpl dir_loop2

            ; '<' in yellow
            ldy #'<'
            lda petscii_to_screen,y
            sta last_screen_row+0
            lda #COLOR_YELLOW
            sta last_color_row+0

            rts

.endproc

.rodata

; Lookup table for screen rows
screen_row_lo:
.repeat 25,i
    .byte <(40*i)
.endrep
screen_row_hi:
.repeat 25,i
    .byte >(40*i)
.endrep
