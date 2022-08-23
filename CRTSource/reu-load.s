; Load the RISC-V target into the REU and begin execution

.macpack longbranch
.include "farcall.inc"
.include "reu-load.inc"
.include "ultidos.inc"
.include "kernal.inc"
.include "reu.inc"
.include "memory.inc"
.include "registers.inc"

FATFS_TARGET = 1

.rodata

; A zero, for zero-filling with the REU
zero: .byte 0

; Types for interpretation of ELF binaries
; Descriptions of structure fields are from
; https://en.wikipedia.org/w/index.php?title=Executable_and_Linkable_Format&oldid=1053259331
.struct ELF_file_header
    e_ident     .res 16
    e_type      .word  ; Identifies object file type.
    e_machine   .word  ; Specifies target instruction set architecture. Some examples are:
    e_version   .dword ; Set to 1 for the original version of ELF.
    e_entry     .dword ; This is the memory address of the entry point from where the process starts executing. This field is either 32 or 64 bits long depending on the format defined earlier.
    e_phoff     .dword ; Points to the start of the program header table. It usually follows the file header immediately, making the offset 0x34 or 0x40 for 32- and 64-bit ELF executables, respectively.
    e_shoff     .dword ; Points to the start of the section header table.
    e_flags     .dword ; Interpretation of this field depends on the target architecture.
    e_ehsize    .word  ; Contains the size of this header, normally 64 Bytes for 64-bit and 52 Bytes for 32-bit format.
    e_phentsize .word  ; Contains the size of a program header table entry.
    e_phnum     .word  ; Contains the number of entries in the program header table.
    e_shentsize .word  ; Contains the size of a section header table entry.
    e_shnum     .word  ; Contains the number of entries in the section header table.
    e_shstrndx  .word  ; Contains index of the section header table entry that contains the section names.
.endstruct

.enum e_ident_index
    EI_MAG0       ; 0x7F followed by ELF(45 4c 46) in ASCII; these four bytes constitute the magic number.
    EI_MAG1
    EI_MAG2
    EI_MAG3
    EI_CLASS      ; This byte is set to either 1 or 2 to signify 32- or 64-bit format, respectively.
    EI_DATA       ; This byte is set to either 1 or 2 to signify little or big endianness, respectively.
    EI_VERSION    ; Set to 1 for the original and current version of ELF.
    EI_OSABI      ; Identifies the target operating system ABI.
    EI_ABIVERSION
    EI_PAD        ; Currently unused, should be filled with zeros.
.endenum

.struct ELF_program_header
    p_type   .dword ; Identifies the type of the segment.
    p_offset .dword ; Offset of the segment in the file image.
    p_vaddr  .dword ; Virtual address of the segment in memory.
    p_paddr  .dword ; Physical address of the segment in memory.
    p_filesz .dword ; Size in bytes of the segment in the file image. May be 0.
    p_memsz  .dword ; Size in bytes of the segment in memory. May be 0.
    p_flags  .dword ; Segment-dependent flags (position for 32-bit structure).
    p_align  .dword ; A positive, integral power of 2, with p_vaddr equating p_offset modulus p_align.
.endstruct

.code

; On entry: browser_path contains the path of a file to be opened as a FAT
; file system.
; reu_load opens the file system, looks for a file called "startup" in the
; root directory, loads it into the REU and then starts the RISC-V emulation.

.proc reu_load

    file_header = scratch_area + 0
    program_header = file_header + .sizeof(ELF_file_header)
    pgm_header_off = program_header + .sizeof(ELF_program_header)
    pgm_header_num = pgm_header_off + 4
    max_readonly = pgm_header_num + 2
    min_writable = max_readonly + 3
    end_addr = min_writable + 3
    zero_size = end_addr + 4
    base_name = zero_size + 3
    base_length = base_name + 1
    init_stack = base_length + 1

    ; Open the file as read and write
    lda #FATFS_TARGET
    sta pointer1+0
    lda #<browser_path
    sta pointer2+0
    lda #>browser_path
    sta pointer2+1
    lda #FA_READ|FA_WRITE
    sta pointer3+0
    jsr dos_open
    lda pointer1+0
    ora pointer1+1
    beq :+
        jmp report_error
    :

    ; Set it up as a FAT file system
    jsr_far fatfs_init_bank,fatfs_init_entry
    lda pointer1+0
    ora pointer1+1
    beq :+
        jmp report_error
    :

    ; Search for the file 
    jsr_far fatfs_search_startup_bank,fatfs_search_startup_entry
    lda pointer1+0
    ora pointer1+1
    beq :+
        jmp report_error
    :

    ; Check that it is a regular file
    lda fatfs_file_data+fatfs_filedata::attributes
    and #$10
    beq :+
        ldx #<is_a_directory
        ldy #>is_a_directory
        jmp message
    :

    ; Copy the file block so that file #0 accesses the file
    ldx #.sizeof(fatfs_filedata)
    @copy:
        lda fatfs_file_data-1,x
        sta fatfs_open_files-1,x
    dex
    bne @copy

    ; Read the file header
    lda #0
    sta pointer1+0
    sta pointer1+1
    lda #<file_header
    sta pointer2+0
    lda #>file_header
    sta pointer2+1
    lda #.sizeof(ELF_file_header)
    sta pointer3+0
    lda #0
    sta pointer3+1
    jsr_far fatfs_read_mem_bank,fatfs_read_mem_entry
    lda pointer1+0
    ora pointer1+1
    jne report_error

    ; Check for correct format
    ldx #<not_elf
    ldy #>not_elf
    lda file_header+ELF_file_header::e_ident+e_ident_index::EI_MAG0
    cmp #$7F
    bne message0
    lda file_header+ELF_file_header::e_ident+e_ident_index::EI_MAG1
    cmp #$45
    bne message0
    lda file_header+ELF_file_header::e_ident+e_ident_index::EI_MAG2
    cmp #$4C
    bne message0
    lda file_header+ELF_file_header::e_ident+e_ident_index::EI_MAG3
    cmp #$46
    bne message0

    ldx #<unsupported
    ldy #>unsupported
    lda file_header+ELF_file_header::e_ident+e_ident_index::EI_CLASS
    cmp #1      ; 32 bit
    bne message0
    lda file_header+ELF_file_header::e_ident+e_ident_index::EI_DATA
    cmp #1      ; Little endian
    bne message0
    lda file_header+ELF_file_header::e_type+0
    cmp #2      ; Executable
    bne message0
    lda file_header+ELF_file_header::e_type+1
    cmp #0
    bne message0
    lda file_header+ELF_file_header::e_machine+0
    cmp #$F3    ; RISC-V
    bne message0
    lda file_header+ELF_file_header::e_machine+1
    cmp #0
    bne message0
    lda file_header+ELF_file_header::e_version+0
    cmp #1      ; ELF version
    bne message0
    lda file_header+ELF_file_header::e_version+1
    cmp #0
    beq :+
    message0:
        jmp message
    :

    ; Get the program entry point and check that it is valid
    lda file_header+ELF_file_header::e_entry+0
    sta RISCV_pc+0
    lda file_header+ELF_file_header::e_entry+1
    sta RISCV_pc+1
    lda file_header+ELF_file_header::e_entry+2
    sta RISCV_pc+2
    lda file_header+ELF_file_header::e_entry+3
    sta RISCV_pc+3
    cmp #1
    beq :+
        ldx #<bad_entry
        ldy #>bad_entry
        jmp message
    :

    ; Limits of read-only and writable segments
    ldx #0
    stx RISCV_break+0   ; maximum address of all segments
    stx RISCV_break+1
    stx RISCV_break+2
    stx max_readonly+0  ; maximum address of read-only segments
    stx max_readonly+1
    stx max_readonly+2
    dex
    stx min_writable+0  ; minimum address of writable segments
    stx min_writable+1
    stx min_writable+2

    ; Offset to next program header
    lda file_header+ELF_file_header::e_phoff+0
    sta pgm_header_off+0
    lda file_header+ELF_file_header::e_phoff+1
    sta pgm_header_off+1
    lda file_header+ELF_file_header::e_phoff+2
    sta pgm_header_off+2
    lda file_header+ELF_file_header::e_phoff+3
    sta pgm_header_off+3

    ; Check that we have at least one header
    lda file_header+ELF_file_header::e_phnum+0
    ora file_header+ELF_file_header::e_phnum+1
    bne :+
        ldx #<no_data
        ldy #>no_data
        jmp message
    :

    ; Count of program headers; negative for easier looping
    sec
    lda #0
    sbc file_header+ELF_file_header::e_phnum+0
    sta pgm_header_num+0
    lda #0
    sbc file_header+ELF_file_header::e_phnum+1
    sta pgm_header_num+1

    header_loop:

        ; Seek to the header
        lda #0
        sta pointer1+0
        sta pointer1+1
        sta pointer2+0 ; SEEK_SET
        sta pointer2+1
        lda pgm_header_off+0
        sta longreg1+0
        lda pgm_header_off+1
        sta longreg1+1
        lda pgm_header_off+2
        sta longreg1+2
        lda pgm_header_off+3
        sta longreg1+3
        jsr_far fatfs_seek_bank,fatfs_seek_entry
        lda pointer1+0
        ora pointer1+1
        jne report_error

        ; Read the header
        lda #0
        sta pointer1+0
        sta pointer1+1
        lda #<program_header
        sta pointer2+0
        lda #>program_header
        sta pointer2+1
        lda #.sizeof(ELF_program_header)
        sta pointer3+0
        lda #0
        sta pointer3+1
        jsr_far fatfs_read_mem_bank,fatfs_read_mem_entry
        lda pointer1+0
        ora pointer1+1
        jne report_error

        ; Check for header type PT_RISCV_ATTRIBUTES
        ; TODO: This is where the executable declares the RISC-V extensions
        ; that it uses; we could check for any that are not supported.
        ; As it is, we just ignore this header.
        lda program_header+ELF_program_header::p_type+0
        cmp #$03
        bne :+
        lda program_header+ELF_program_header::p_type+1
        cmp #$00
        bne :+
        lda program_header+ELF_program_header::p_type+2
        cmp #$00
        bne :+
        lda program_header+ELF_program_header::p_type+3
        cmp #$70
        bne :+
        next_header0:
            jmp next_header
        :

        ; All other recognized header types have types <= 255
        lda program_header+ELF_program_header::p_type+1
        ora program_header+ELF_program_header::p_type+2
        ora program_header+ELF_program_header::p_type+3
        beq :+
        badhdr:
            ldx #<bad_header
            ldy #<bad_header
            jmp message
        :

        ; Ignore type PT_NULL
        lda program_header+ELF_program_header::p_type+0
        beq next_header0

        ; Error out on anything else except PT_LOAD
        cmp #1
        bne badhdr

        ; Ignore if memory size is 0
        lda program_header+ELF_program_header::p_memsz+0
        ora program_header+ELF_program_header::p_memsz+1
        ora program_header+ELF_program_header::p_memsz+2
        ora program_header+ELF_program_header::p_memsz+3
        beq next_header0

        ; Check that the segment loads to valid addresses
        lda program_header+ELF_program_header::p_vaddr+3
        cmp #1
        bne badaddr
        clc
        lda program_header+ELF_program_header::p_vaddr+0
        adc program_header+ELF_program_header::p_memsz+0
        sta end_addr+0
        lda program_header+ELF_program_header::p_vaddr+1
        adc program_header+ELF_program_header::p_memsz+1
        sta end_addr+1
        lda program_header+ELF_program_header::p_vaddr+2
        adc program_header+ELF_program_header::p_memsz+2
        sta end_addr+2
        lda program_header+ELF_program_header::p_vaddr+3
        adc program_header+ELF_program_header::p_memsz+3
        sta end_addr+3
        bcs badaddr
        lda end_addr+0
        cmp #$01
        lda end_addr+1
        cmp #$00
        lda end_addr+2
        cmp #$00
        lda end_addr+3
        cmp #$02
        bcc :+
        badaddr:
            ldx #<bad_address
            ldy #>bad_address
            jmp message
        :

        ; Check for valid write and executable flags
        ; Read flag is ignored; all areas are considered readable
        lda program_header+ELF_program_header::p_flags+0
        and #$03
        cmp #$01
        bcc check_writable
            ; Read-only area; we'll allow execution also
            ; if max_readonly < end_addr, set max_readonly to end_addr
            lda max_readonly+0
            cmp end_addr+0
            lda max_readonly+1
            sbc end_addr+1
            lda max_readonly+2
            sbc end_addr+2
            lda max_readonly+3
            sbc end_addr+3
            bcc :+
                lda end_addr+0
                sta max_readonly+0
                lda end_addr+1
                sta max_readonly+1
                lda end_addr+2
                sta max_readonly+2
                lda end_addr+3
                sta max_readonly+3
            :
            jmp set_break

        check_writable:
        bne bad_flags
            ; read and write, or write-only
            ; if p_addr < min_writable, set min_writable to p_addr
            lda program_header+ELF_program_header::p_vaddr+0
            cmp min_writable+0
            lda program_header+ELF_program_header::p_vaddr+1
            sbc min_writable+1
            lda program_header+ELF_program_header::p_vaddr+2
            sbc min_writable+2
            lda program_header+ELF_program_header::p_vaddr+3
            sbc min_writable+3
            bcc :+
                lda program_header+ELF_program_header::p_vaddr+0
                sta min_writable+0
                lda program_header+ELF_program_header::p_vaddr+1
                sta min_writable+1
                lda program_header+ELF_program_header::p_vaddr+2
                sta min_writable+2
                lda program_header+ELF_program_header::p_vaddr+3
                sta min_writable+3
            :
            jmp set_break

        bad_flags:
            ; Flags allow both write and execute
            ldx #<write_and_exec
            ldy #>write_and_exec
            jmp message

        set_break:

        ; Can't have memory size less than file size
        lda program_header+ELF_program_header::p_memsz+0
        cmp program_header+ELF_program_header::p_filesz+0
        lda program_header+ELF_program_header::p_memsz+1
        sbc program_header+ELF_program_header::p_filesz+1
        lda program_header+ELF_program_header::p_memsz+2
        sbc program_header+ELF_program_header::p_filesz+2
        lda program_header+ELF_program_header::p_memsz+3
        sbc program_header+ELF_program_header::p_filesz+3
        bcs :+
            ldx #<bad_mem_size
            ldy #>bad_mem_size
            jmp message
        :

        ; Set RISCV_break if it's less than the end of this segment
        lda RISCV_break+0
        cmp end_addr+0
        lda RISCV_break+1
        sbc end_addr+1
        lda RISCV_break+2
        sbc end_addr+2
        bcs :+
            lda end_addr+0
            sta RISCV_break+0
            lda end_addr+1
            sta RISCV_break+1
            lda end_addr+2
            sta RISCV_break+2
        :

        ; Seek to the location of the segment in memory
        lda #0
        sta pointer1+0
        sta pointer1+1
        sta pointer2+0 ; SEEK_SET
        sta pointer2+1
        lda program_header+ELF_program_header::p_offset+0
        sta longreg1+0
        lda program_header+ELF_program_header::p_offset+1
        sta longreg1+1
        lda program_header+ELF_program_header::p_offset+2
        sta longreg1+2
        lda program_header+ELF_program_header::p_offset+3
        sta longreg1+3
        jsr_far fatfs_seek_bank,fatfs_seek_entry
        lda pointer1+0
        ora pointer1+1
        jne report_error

        ; Read to the REU
        lda #0
        sta pointer1+0
        sta pointer1+1
        lda program_header+ELF_program_header::p_vaddr+0
        sta longreg1+0
        lda program_header+ELF_program_header::p_vaddr+1
        sta longreg1+1
        lda program_header+ELF_program_header::p_vaddr+2
        sta longreg1+2
        lda #0
        sta longreg1+3
        lda program_header+ELF_program_header::p_filesz+0
        sta longreg2+0
        lda program_header+ELF_program_header::p_filesz+1
        sta longreg2+1
        lda program_header+ELF_program_header::p_filesz+2
        sta longreg2+2
        lda #0
        sta longreg2+3
        jsr_far fatfs_read_bank,fatfs_read_entry
        lda pointer1+0
        ora pointer1+1
        jne report_error

        ; Zero-fill to the specified memory size
        clc
        lda program_header+ELF_program_header::p_vaddr+0
        adc program_header+ELF_program_header::p_filesz+0
        sta reu_xmem_address+0
        lda program_header+ELF_program_header::p_vaddr+1
        adc program_header+ELF_program_header::p_filesz+1
        sta reu_xmem_address+1
        lda program_header+ELF_program_header::p_vaddr+2
        adc program_header+ELF_program_header::p_filesz+2
        sta reu_xmem_address+2
        set_local_address zero
        sec
        lda end_addr+0
        sbc reu_xmem_address+0
        sta zero_size+0
        lda end_addr+1
        sbc reu_xmem_address+1
        sta zero_size+1
        lda end_addr+2
        sbc reu_xmem_address+2
        sta zero_size+2
        ora zero_size+1
        beq end_zero_loop
            sec
            lda #0
            sbc zero_size+1
            sta zero_size+1
            lda #0
            sbc zero_size+2
            sta zero_size+2
            set_xfer_size_imm 256
            zero_loop:
                jsr reu_fill
                inc reu_xmem_address+1
                bne :+
                inc reu_xmem_address+2
                :
            inc zero_size+1
            bne zero_loop
            inc zero_size+2
            bne zero_loop
        end_zero_loop:
        set_xfer_size zero_size
        jsr reu_fill

    next_header:

    ; Advance to next header
    clc
    lda pgm_header_off+0
    adc file_header+ELF_file_header::e_phentsize+0
    sta pgm_header_off+0
    lda pgm_header_off+1
    adc file_header+ELF_file_header::e_phentsize+1
    sta pgm_header_off+1
    bcc :+
        inc pgm_header_off+2
        bne :+
        inc pgm_header_off+3
    :

    ; Count until all headers processed
    inc pgm_header_num+0
    bne header_loop0
    inc pgm_header_num+1
    beq :+
    header_loop0:
        jmp header_loop
    :

    ; Check that at least one code segment was loaded
    lda max_readonly+0
    ora max_readonly+1
    ora max_readonly+2
    bne :+
        ldx #<no_code_seg
        ldy #>no_code_seg
        jmp message
    :

    ; Set the fence
    clc
    lda max_readonly+0
    adc #$FF
    lda max_readonly+1
    adc #$FF
    lda max_readonly+2
    adc #0
    sta RISCV_fence
    ; Check that the fence doesn't overlap a writable segment
    lda min_writable+2
    cmp RISCV_fence
    bcs :+
        ldx #<no_fence
        ldy #>no_fence
        jmp message
    :

    ; Determine the initial stack pointer:
    ; Find the image name without the directory
    ldx #0
    ldy #0
    name_loop:
        lda browser_path,x
        beq end_name_loop
        cmp #'/'
        bne :+
            txa
            tay
            iny
        :
    inx
    bne name_loop
    end_name_loop:
    sty base_name
    txa
    sec
    sbc base_name
    sta base_length
    inc base_length ; include the terminating '\0'

    ; Stack pointer is $02000000 - (base_length+17), rounded down to a multiple
    ; of 16
    ; 16 is 4 for argc, 4 for argv, 4 for argv[0] and 4 for argv[1]
    sec
    lda #$F0
    sbc base_length
    and #$F0
    sta RISCV_ireg_0+REG_sp
    lda #$FF
    sbc #0
    sta RISCV_ireg_1+REG_sp
    lda #$FF
    sbc #0
    sta RISCV_ireg_2+REG_sp
    lda #$01
    sbc #0
    sta RISCV_ireg_3+REG_sp

    ; Place argc
    lda #1
    sta init_stack+0
    lda #0
    sta init_stack+1
    sta init_stack+2
    sta init_stack+3
    ; Place argv
    clc
    lda RISCV_ireg_0+REG_sp
    adc #8
    sta init_stack+4
    lda RISCV_ireg_1+REG_sp
    adc #0
    sta init_stack+5
    lda RISCV_ireg_2+REG_sp
    adc #0
    sta init_stack+6
    lda RISCV_ireg_3+REG_sp
    adc #0
    sta init_stack+7
    ; Place argv[0]
    clc
    lda RISCV_ireg_0+REG_sp
    adc #16
    sta init_stack+8
    lda RISCV_ireg_1+REG_sp
    adc #0
    sta init_stack+9
    lda RISCV_ireg_2+REG_sp
    adc #0
    sta init_stack+10
    lda RISCV_ireg_3+REG_sp
    adc #0
    sta init_stack+11
    ; Place argv[1]
    lda #0
    sta init_stack+12
    sta init_stack+13
    sta init_stack+14
    sta init_stack+15
    ; Copy to REU
    lda RISCV_ireg_0+REG_sp
    sta reu_xmem_address+0
    lda RISCV_ireg_1+REG_sp
    sta reu_xmem_address+1
    lda RISCV_ireg_2+REG_sp
    sta reu_xmem_address+2
    set_local_address init_stack
    set_xfer_size_imm 16
    jsr reu_write
    ; Copy the name
    set_reu_address init_stack+8
    clc
    lda #<browser_path
    adc base_name
    sta reu_c64_address+0
    lda #>browser_path
    adc #0
    sta reu_c64_address+1
    lda base_length
    sta reu_xfer_size+0
    lda #0
    sta reu_xfer_size+1
    jsr reu_write

    lda #0
    sta fatfs_open_files+0
    sta pointer1+0
    sta pointer1+1
    rts

report_error:
    ldx #<ultidos_status
    ldy #>ultidos_status
message:
    stx pointer1+0
    sty pointer1+1
    ldy #0
    @loop:
        lda (pointer1),y
        beq @end_loop
        jsr CHROUT
    iny
    bne @loop
    @end_loop:
    lda #$0D
    jsr CHROUT

    lda #0
    sta fatfs_open_files+0
    lda #0
    sta pointer1+0
    sta pointer1+1
    rts

is_a_directory:
    .asciiz "Program is a directory"

not_elf:
    .asciiz "Not in ELF format"

unsupported:
    .asciiz "Unsupported system"

bad_entry:
    .asciiz "Program entry point out of range"

no_data:
    .asciiz "No data in the executable"

bad_header:
    .asciiz "Program header has unsupported type"

bad_address:
    .asciiz "Program header loads to invalid addresses"

write_and_exec:
    .asciiz "Program header allows both write and execute"

bad_mem_size:
    .asciiz "Program header has memory size less than file size"

no_code_seg:
    .asciiz "No code segment was loaded"

no_fence:
    .asciiz "Cannot place read-only limit"

.if 0
;;;;
check1: .byte "check 1", 13, 0
check2: .byte "check 2", 13, 0
check3: .byte "check 3", 13, 0
check4: .byte "check 4", 13, 0

print_string:
    lda pointer1+0
    pha
    lda pointer1+1
    pha
    stx pointer1+0
    sty pointer1+1
    ldy #0
    @print:
        lda (pointer1),y
        beq @end_print
        jsr CHROUT
    iny
    bne @print
    @end_print:
    pla
    sta pointer1+1
    pla
    sta pointer1+0
    rts

print_hex:
    pha
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda hexdigit,x
    jsr CHROUT
    pla
    and #$0F
    tax
    lda hexdigit,x
    jsr CHROUT
    rts

hexdigit: .byte "0123456789ABCDEF"
;;;;
.endif
.endproc
