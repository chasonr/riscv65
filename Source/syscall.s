; syscall.s -- implementation of system calls

; External parameters come from here
RISCV_BREAK = $07F4

.macpack longbranch
.include "registers.inc"
.include "kernal.inc"
.include "errno.inc"
.include "reu.inc"

.import _RISCV_ireg_0
.import _RISCV_ireg_1
.import _RISCV_ireg_2
.import _RISCV_ireg_3
.importzp RISCV_fence

.bss

RISCV_break: .res 4
RISCV_min_break: .res 4

; Address and size for I/O
io_addr: .res 4
io_size: .res 4

; Structure for stat, fstat, lstat, fstatat
; Matches Newlib libgloss/riscv/kernel_stat.h
.struct kernel_stat
    st_dev     .dword 2
    st_ino     .dword 2
    st_mode    .dword
    st_nlink   .dword
    st_uid     .dword
    st_gid     .dword
    st_rdev    .dword 2
    __pad1     .dword 2
    st_size    .dword 2
    st_blksize .dword
    __pad2     .dword
    st_blocks  .dword 2
    st_atim    .dword 4 ; 8 bytes: tv_sec; 4 bytes: tv_nsec; 4 bytes: pad
    st_mtim    .dword 4
    st_ctim    .dword 4
    __pad3     .dword 2
.endstruct
kernel_stat_size = 128

; Structure for gettimeofday
; Matches struct timeval in Newlib newlib/libc/include/sys/_timeval.h
.struct timeval
    tv_sec  .dword 2
    tv_usec .dword
    pad     .dword
.endstruct
timeval_size = 16

; Transfer area for I/O
xfer_size = 16      ; Size for transfer when doing character I/O
io_xfer: .res 512

; Read file paths into this area
filename_max = 1024 ; FILENAME_MAX from Newlib stdio.h
fs_path: .res filename_max+1

; File handle being accessed
file_handle: .res 1

; Path component being searched
path_part: .res 11

; Number of directory entries remaining in cluster
dir_size: .res 3

; Location of an empty directory entry where a new file can be created
new_dir_entry: .res 4

; Cluster currently being followed
cluster: .res 4

; Last cluster followed
last_cluster: .res 3

; Size to be read or written
rw_size: .res 3

; Structures for file access
; Operations in progress use file_data; open files are contained in open_files
.struct filedata
    ; Structure from the file system directory
    name       .byte 11
    attributes .byte  1
    case_flags .byte  1
    ctime_lo   .byte  1
    ctime      .word  1
    cdate      .word  1
    reserved   .word  1
    cluster_hi .word  1
    mtime      .word  1
    mdate      .word  1
    cluster_lo .word  1
    size       .dword 1

    ; Location of the directory entry
    dir_start  .dword 1  ; cluster where enclosing directory starts; 0 for root
    dir_entry  .dword 1  ; position of this directory entry
    lfn_entry  .dword 1  ; position of first LFN entry; equal to dir_entry if no LFN

    ; Mode for opening the file (read, write, append)
    open_flags  .byte 3

    ; Size as cluster and offset within cluster
    cluster_count .byte 3 ; not including partial cluster at the end
    cluster_partial .byte 3

    ; Current position as cluster and offset within cluster
    current_cluster .byte 3
    cluster_pos .byte 3
    cluster_ptr .byte 3
.endstruct
MAX_FILES = 16
file_data: .tag filedata
open_files: .tag filedata
open_files_1:
    filedata_size = open_files_1 - open_files
    .res filedata_size * (MAX_FILES-1)

.segment "RODATA"
open_files_lo:
    .repeat MAX_FILES,i
        .byte <(open_files + filedata_size*i)
    .endrep
open_files_hi:
    .repeat MAX_FILES,i
        .byte >(open_files + filedata_size*i)
    .endrep

.bss

; Flags for sys_open
O_ACCMODE   = $3
O_RDONLY    = 0
O_WRONLY    = 1
O_RDWR      = 2
O_APPEND    = $0008
O_CREAT     = $0200
O_TRUNC     = $0400
O_EXCL      = $0800
O_DIRECTORY = $200000

; Attribute flags
ATTR_READONLY  = $01
ATTR_HIDDEN    = $02
ATTR_SYSTEM    = $04
ATTR_VOLUME    = $08
ATTR_DIRECTORY = $10
ATTR_ARCHIVE   = $20

; File system data
; 3-byte quantities are in multiples of 256 bytes with the low byte being
; implicitly zero
fs_type:          .res 1 ; 0 (invalid), 12 (FAT12), 16 (FAT16), 32 (FAT32)
fs_sector_shift:  .res 1 ; Shift from sector size to 256
fs_cluster_shift: .res 1 ; Shift from cluster size to 256
fs_cluster_size:  .res 2 ; Cluster size in 256-byte blocks
fs_clusters:      .res 4 ; Number of clusters in file system
fs_root_dir_size: .res 2 ; Number of entries in root directory
fs_first_fat:     .res 3 ; 256-byte offset to first FAT
fs_second_fat:    .res 3 ; 256-byte offset to second FAT; == first_FAT if only one FAT
fs_root_dir:      .res 4 ; 256-byte offset to root directory (FAT12, FAT16)
                         ; or cluster where root directory starts (FAT32)
fs_cluster_base:  .res 3 ; Add this to shifted cluster number to get offset to cluster
fs_ext_flags:     .res 1 ; FAT32 extended flags
fs_info_sector:   .res 2 ; FAT32 info sector
fs_current_dir:   .res 4 ; Starting cluster of current directory; 0 for root on FAT12 or FAT16

.segment "ZEROPAGE"
io_count: .res 1
io_index: .res 1
local_addr: .res 2
path_ptr: .res 2

; U64 command interface

CMD_CONTROL        = $DF1C
CMD_STATUS         = $DF1C
CMD_DATA           = $DF1D
CMD_IDENTIFICATION = $DF1D
CMD_RESPONSE_DATA  = $DF1E
CMD_STATUS_DATA    = $DF1F

CMD_PUSH_CMD = $01
CMD_DATA_ACC = $02
CMD_ABORT    = $04
CMD_CLR_ERR  = $08

CMD_BUSY       = $01
CMD_ABORT_P    = $04
CMD_ERROR      = $08
CMD_STATE_MASK = $30
CMD_STATE_IDLE = $00
CMD_STATE_BUSY = $10
CMD_STATE_LAST = $20
CMD_STATE_MORE = $30
CMD_STAT_AV    = $40
CMD_DATA_AV    = $80

ULTIDOS_TARGET = $01

DOS_CMD_CLOSE_FILE = $03
DOS_CMD_READ_DATA  = $04
DOS_CMD_WRITE_DATA = $05
DOS_CMD_FILE_SEEK  = $06
DOS_CMD_LOAD_REU   = $21
DOS_CMD_GET_TIME   = $26

.code

; Initialize this system
.global _RISCV_syscall_init
.proc _RISCV_syscall_init

    ; Set the break
    lda RISCV_BREAK+0
    sta RISCV_break+0
    sta RISCV_min_break+0
    lda RISCV_BREAK+1
    sta RISCV_break+1
    sta RISCV_min_break+1
    lda RISCV_BREAK+2
    sta RISCV_break+2
    sta RISCV_min_break+2
    lda #1
    sta RISCV_break+3
    sta RISCV_min_break+3

    ; Get the file system layout
    jsr init_fs
    ;jsr dump_fs_params

    ; Mark files as closed
    ldx #MAX_FILES-1
    lda #0
    sta local_addr+0
    @mark_closed:
        ldy open_files_lo,x
        lda open_files_hi,x
        sta local_addr+1
        lda #0
        sta (local_addr),y
    dex
    bpl @mark_closed

    rts

.endproc

; Shut down all
.global _RISCV_syscall_exit
.proc _RISCV_syscall_exit

    ; Close the volume
    lda #ULTIDOS_TARGET
    sta CMD_DATA
    lda #DOS_CMD_CLOSE_FILE
    sta CMD_DATA
    jsr get_cmd_response

    rts

.endproc

; SYS_faccessat    = bad_ecall
; SYS_openat       = bad_ecall

; Close the given file handle
; A0 = file handle
.global SYS_close
.proc SYS_close

    ; File handles above 255 are not assigned
    lda _RISCV_ireg_1+REG_a0
    ora _RISCV_ireg_2+REG_a0
    ora _RISCV_ireg_3+REG_a0
    bne bad_file

    ; Ignore any attempt to close file 0, 1 or 2
    lda _RISCV_ireg_0+REG_a0
    sec
    sbc #3
    bcc file_closed

    ; A has index to open_files
    ; Check that it is valid 
    cmp #MAX_FILES
    bcs bad_file

        sta file_handle

        ; Build pointer to open_files
        tax
        lda open_files_lo,x
        sta local_addr+0
        lda open_files_hi,x
        sta local_addr+1

        ; Check that the file was open
        ldy #0
        lda (local_addr),y
        beq bad_file        ; File was not open

        ; If file was open for writing, update the directory entry
        ldy #filedata::open_flags+0
        lda (local_addr),y
        and #O_ACCMODE
        beq end_update
            jsr write_dir_entry
        end_update:
        sta _RISCV_ireg_0+REG_a0 ; always 0 if O_ACCMODE

        ; Mark the file as closed
        lda #0
        tay
        sta (local_addr),y
        ; Report error from write_dir_entry
        bcc :+
            lda #$FF
            sta _RISCV_ireg_1+REG_a0
            sta _RISCV_ireg_2+REG_a0
            sta _RISCV_ireg_3+REG_a0
            rts
        :

file_closed:
    lda #0
    sta _RISCV_ireg_0+REG_a0
    sta _RISCV_ireg_1+REG_a0
    sta _RISCV_ireg_2+REG_a0
    sta _RISCV_ireg_3+REG_a0
    rts

bad_file:
    set_errno EBADF
    rts

.endproc

; Seek to a position in a file
; A0 = file handle
; A1 = seek position
; A2 = 0 (SEEK_SET) to seek from beginning
;      1 (SEEK_CUR) to seek from current position
;      2 (SEEK_END) to seek from the end

.global SYS_lseek
.proc SYS_lseek

    ; Handle must refer to an open file
    lda _RISCV_ireg_1+REG_a0
    ora _RISCV_ireg_2+REG_a0
    ora _RISCV_ireg_3+REG_a0
    bne bad_file
    lda _RISCV_ireg_0+REG_a0
    sec
    sbc #3
    bcs file_ok_1
        set_errno ESPIPE
        rts
    file_ok_1:
    cmp #MAX_FILES
    bcs bad_file

    sta file_handle

    ; Build pointer to open_files
    tax
    lda open_files_lo,x
    sta local_addr+0
    lda open_files_hi,x
    sta local_addr+1

    ; Check that the file is open
    ldy #0
    lda (local_addr),y
    bne file_ok_2       ; Else file is not open
    bad_file:
        set_errno EBADF
        rts
    file_ok_2:

    ; Get the origin from which the seek shall be done
    lda _RISCV_ireg_1+REG_a2
    ora _RISCV_ireg_2+REG_a2
    ora _RISCV_ireg_3+REG_a2
    bne bad_param
    ldx _RISCV_ireg_0+REG_a2
    bne check_1
        ; 0 (SEEK_SET)
        lda #0
        sta lseek_location+0
        sta lseek_location+1
        sta lseek_location+2
        sta lseek_location+3
        beq have_origin
    check_1:
    dex
    bne check_2
        ; 1 (SEEK_CUR)
        ; Read current cluster number
        ldy #filedata::cluster_pos
        lda (local_addr),y
        sta lseek_location+1
        iny
        lda (local_addr),y
        sta lseek_location+2
        iny
        lda (local_addr),y
        ; Convert to byte offset
        ldx fs_cluster_shift
        beq @end_shift
        @shift:
            asl lseek_location+1
            rol lseek_location+2
            rol a
        dex
        bne @shift
        @end_shift:
        sta lseek_location+3
        ; Add the offset within the cluster
        ldy #filedata::cluster_ptr
        lda (local_addr),y
        sta lseek_location+0
        iny
        clc
        lda (local_addr),y
        adc lseek_location+1
        sta lseek_location+1
        iny
        lda (local_addr),y
        adc lseek_location+2
        sta lseek_location+2
        lda #0
        adc lseek_location+3
        sta lseek_location+3
        jmp have_origin
    check_2:
    dex
    bne bad_param
        ; 2 (SEEK_END)
        ldy #filedata::size
        lda (local_addr),y
        sta lseek_location+0
        iny
        lda (local_addr),y
        sta lseek_location+1
        iny
        lda (local_addr),y
        sta lseek_location+2
        iny
        lda (local_addr),y
        sta lseek_location+3
        jmp have_origin
    bad_param:
        set_errno EINVAL
        rts
    have_origin:

    ; Sign-extend the offset to 5 bytes.
    ; Treat the offset as signed and the position as unsigned.
    lda _RISCV_ireg_3+REG_a1
    asl a
    lda #0
    adc #$FF
    eor #$FF
    tax

    ; Add the offset to the position
    clc
    lda _RISCV_ireg_0+REG_a1
    adc lseek_location+0
    sta lseek_location+0
    lda _RISCV_ireg_1+REG_a1
    adc lseek_location+1
    sta lseek_location+1
    lda _RISCV_ireg_2+REG_a1
    adc lseek_location+2
    sta lseek_location+2
    lda _RISCV_ireg_3+REG_a1
    adc lseek_location+3
    sta lseek_location+3
    txa
    adc #0
    bne bad_param ; Add overflowed

    ; Does the seek location exceed the size of the file?
    ldy #filedata::size
    lda (local_addr),y
    cmp lseek_location+0
    iny
    lda (local_addr),y
    sbc lseek_location+1
    iny
    lda (local_addr),y
    sbc lseek_location+2
    iny
    lda (local_addr),y
    sbc lseek_location+3
    bcc bad_param ; Else seek beyond end of file

    ; Determine the cluster number of the seek position
    lda lseek_location+1
    sta lseek_cluster+0
    lda lseek_location+2
    sta lseek_cluster+1
    lda lseek_location+3
    ldx fs_cluster_shift
    beq @end_shift
    @shift:
        lsr a
        ror lseek_cluster+1
        ror lseek_cluster+0
    dex
    bne @shift
    @end_shift:
    sta lseek_cluster+2

    ; If the target cluster number is less than the current one, seek from the
    ; beginning. Otherwise, seek from the current cluster.

    ldy #filedata::cluster_pos
    lda lseek_cluster+0
    cmp (local_addr),y
    iny
    lda lseek_cluster+1
    sbc (local_addr),y
    iny
    lda lseek_cluster+2
    sbc (local_addr),y
    bcs seek_from_current
        ; Begin following the cluster chain from the start
        ldy #filedata::cluster_lo+0
        lda (local_addr),y
        sta cluster+0
        iny
        lda (local_addr),y
        sta cluster+1
        ldy #filedata::cluster_hi+0
        lda (local_addr),y
        sta cluster+2
        iny
        lda (local_addr),y
        sta cluster+3
        ; Negative of cluster pointer count
        sec
        lda #0
        sbc lseek_cluster+0
        sta lseek_count+0
        lda #0
        sbc lseek_cluster+1
        sta lseek_count+1
        lda #0
        sbc lseek_cluster+2
        sta lseek_count+2
    jmp have_start_cluster
    seek_from_current:
        ldy #filedata::current_cluster
        lda (local_addr),y
        sta cluster+0
        iny
        lda (local_addr),y
        sta cluster+1
        iny
        lda (local_addr),y
        sta cluster+2
        lda #0
        sta cluster+3
        ; Negative of cluster pointer count
        sec
        ldy #filedata::cluster_pos
        lda (local_addr),y
        sbc lseek_cluster+0
        sta lseek_count+0
        iny
        lda (local_addr),y
        sbc lseek_cluster+1
        sta lseek_count+1
        iny
        lda (local_addr),y
        sbc lseek_cluster+2
        sta lseek_count+2
    have_start_cluster:

    ; Follow the cluster chain from the current position
    lda lseek_count+0
    ora lseek_count+1
    ora lseek_count+2
    beq end_cluster_loop
    cluster_loop:

        jsr next_cluster
        bcs io_error

    ; lseek_count is negative, to simplify this count
    inc lseek_count+0
    bne cluster_loop
    inc lseek_count+1
    bne cluster_loop
    inc lseek_count+2
    bne cluster_loop
    end_cluster_loop:

    ; Set the current position
    ldy #filedata::current_cluster
    lda cluster+0
    sta (local_addr),y
    iny
    lda cluster+1
    sta (local_addr),y
    iny
    lda cluster+2
    sta (local_addr),y
    ldy #filedata::cluster_pos
    lda lseek_cluster+0
    sta (local_addr),y
    iny
    lda lseek_cluster+1
    sta (local_addr),y
    iny
    lda lseek_cluster+2
    sta (local_addr),y
    ldy fs_cluster_size+1
    ldx fs_cluster_size+0
    bne :+
        dey
    :
    dex
    tya
    and lseek_location+2
    ldy #filedata::cluster_ptr+2
    sta (local_addr),y
    dey
    txa
    and lseek_location+1
    sta (local_addr),y
    dey
    lda lseek_location+0
    sta (local_addr),y

    ; Return the new file position
    lda lseek_location+0
    sta _RISCV_ireg_0+REG_a0
    lda lseek_location+1
    sta _RISCV_ireg_1+REG_a0
    lda lseek_location+2
    sta _RISCV_ireg_2+REG_a0
    lda lseek_location+3
    sta _RISCV_ireg_3+REG_a0
    rts

io_error:
    ; error code in A
    sta _RISCV_ireg_0+REG_a0
    lda #$FF
    sta _RISCV_ireg_1+REG_a0
    sta _RISCV_ireg_2+REG_a0
    sta _RISCV_ireg_3+REG_a0
    rts

.endproc

.bss
lseek_count: .res 3
lseek_cluster: .res 3
lseek_location: .res 4

.code

; Read from the given file handle
; A0 = file handle
; A1 = pointer to data
; A2 = length of data

.global SYS_read
.proc SYS_read

    ; Set up the transfer area
    lda _RISCV_ireg_0+REG_a1
    sta io_addr+0
    lda _RISCV_ireg_1+REG_a1
    sta io_addr+1
    lda _RISCV_ireg_2+REG_a1
    sta io_addr+2
    lda _RISCV_ireg_3+REG_a1
    sta io_addr+3
    lda _RISCV_ireg_0+REG_a2
    sta io_size+0
    lda _RISCV_ireg_1+REG_a2
    sta io_size+1
    lda _RISCV_ireg_2+REG_a2
    sta io_size+2
    lda _RISCV_ireg_3+REG_a2
    sta io_size+3
    jsr check_write
    bcc write_ok
        set_errno EFAULT
        rts
    write_ok:

    ; Check for special file handles 0, 1 and 2
    lda _RISCV_ireg_1+REG_a0
    ora _RISCV_ireg_2+REG_a0
    ora _RISCV_ireg_3+REG_a0
    bne bad_file
    lda _RISCV_ireg_0+REG_a0
    bne check_output
    bad_file:
        ; Handle 0 (standard input; not yet implemented)
        set_errno EBADF
        rts
    check_output:
    sec
    sbc #3
    bcs check_file
        ; Handle 1 (standard output) or 2 (standard error)
        ; These are not valid
        set_errno EBADF
        rts
    check_file:
    ; A has index to open_files
    ; Check that it is valid 
    cmp #MAX_FILES
    bcs bad_file
        sta file_handle

        ; Build pointer to open_files
        tax
        lda open_files_lo,x
        sta local_addr+0
        lda open_files_hi,x
        sta local_addr+1

        ; Check that the file is open
        ldy #0
        lda (local_addr),y
        beq bad_file

        ; Check that the file is open for reading
        ldy #filedata::open_flags+0
        lda (local_addr),y
        and #O_ACCMODE
        cmp #O_WRONLY
        beq bad_file

        ; TODO: Handle directory specially

        ; Read loop
        jmp @end_read_loop
        @read_loop:

            ; Are we reading from the last cluster?
            ldy #filedata::cluster_count+0
            lda (local_addr),y
            ldy #filedata::cluster_pos+0
            cmp (local_addr),y
            bne @whole_cluster
            ldy #filedata::cluster_count+1
            lda (local_addr),y
            ldy #filedata::cluster_pos+1
            cmp (local_addr),y
            bne @whole_cluster
            ldy #filedata::cluster_count+2
            lda (local_addr),y
            ldy #filedata::cluster_pos+2
            cmp (local_addr),y
            bne @whole_cluster
                ; This is the last cluster.
                ; The maximum size is cluster_partial - cluster_ptr.
                sec
                ldy #filedata::cluster_partial+0
                lda (local_addr),y
                ldy #filedata::cluster_ptr+0
                sbc (local_addr),y
                sta rw_size+0
                ldy #filedata::cluster_partial+1
                lda (local_addr),y
                ldy #filedata::cluster_ptr+1
                sbc (local_addr),y
                sta rw_size+1
                ldy #filedata::cluster_partial+2
                lda (local_addr),y
                ldy #filedata::cluster_ptr+2
                sbc (local_addr),y
                sta rw_size+2
                ; If this is zero, we're at end of file
                ora rw_size+0
                ora rw_size+1
                beq :+
                    jmp @set_size   ; Read the next set of bytes
                :
                    jmp @end_read   ; Size is zero; this is EOF
            @whole_cluster:
                ; This is not the last cluster.
                ; The maximum size is fs_cluster_size - cluster_ptr.
                sec
                ldy #filedata::cluster_ptr+0
                lda #0
                sbc (local_addr),y
                sta rw_size+0
                iny
                lda fs_cluster_size+0
                sbc (local_addr),y
                sta rw_size+1
                iny
                lda fs_cluster_size+1
                sbc (local_addr),y
                sta rw_size+2
                ; If this is zero, advance to the next cluster
                ora rw_size+0
                ora rw_size+1
                bne @set_size
                    ; Get the next cluster number
                    ldy #filedata::current_cluster+0
                    lda (local_addr),y
                    sta cluster+0
                    iny
                    lda (local_addr),y
                    sta cluster+1
                    iny
                    lda (local_addr),y
                    sta cluster+2
                    jsr next_cluster
                    bcc @cluster_ok
                        ; Can produce EOF if the file size is an exact multiple
                        ; of the cluster size
                        cmp #$100-ENOENT
                        jeq @end_read
                        ; Otherwise return error
                        sta _RISCV_ireg_0+REG_a0
                        lda #$FF
                        sta _RISCV_ireg_1+REG_a0
                        sta _RISCV_ireg_2+REG_a0
                        sta _RISCV_ireg_3+REG_a0
                        rts
                    @cluster_ok:
                    ; Update the position
                    ldy #filedata::current_cluster+0
                    lda cluster+0
                    sta (local_addr),y
                    iny
                    lda cluster+1
                    sta (local_addr),y
                    iny
                    lda cluster+2
                    sta (local_addr),y
                    ldy #filedata::cluster_pos+0
                    ; There's no inc (xx),y; do this the long way
                    clc
                    lda (local_addr),y
                    adc #1
                    sta (local_addr),y
                    iny
                    lda (local_addr),y
                    adc #0
                    sta (local_addr),y
                    iny
                    lda (local_addr),y
                    adc #0
                    sta (local_addr),y
                    ldy #filedata::cluster_ptr+0
                    lda #0
                    sta (local_addr),y
                    iny
                    sta (local_addr),y
                    iny
                    sta (local_addr),y
                    ; Repeat the loop without having read any data
                    jeq @read_loop

            @set_size:
            ; Use the lesser of the size remaining or the maximum size
            lda io_size+0
            cmp rw_size+0
            lda io_size+1
            sbc rw_size+1
            lda io_size+2
            sbc rw_size+2
            bcs @do_read
                lda io_size+0
                sta rw_size+0
                lda io_size+1
                sta rw_size+1
                lda io_size+2
                sta rw_size+2
            @do_read:

            ; Seek to the position to be read:
            ; Current cluster number
            ldy #filedata::current_cluster
            lda (local_addr),y
            sta seek_location+1
            iny
            lda (local_addr),y
            sta seek_location+2
            iny
            lda (local_addr),y
            ldx fs_cluster_shift
            beq @end_shift
            @shift:
                asl seek_location+1
                rol seek_location+2
                rol a
            dex
            bne @shift
            @end_shift:
            sta seek_location+3
            ; Add fs_cluster_base
            clc
            lda fs_cluster_base+0
            adc seek_location+1
            sta seek_location+1
            lda fs_cluster_base+1
            adc seek_location+2
            sta seek_location+2
            lda fs_cluster_base+2
            adc seek_location+3
            sta seek_location+3
            ; Add cluster_ptr
            ldy #filedata::cluster_ptr
            lda (local_addr),y
            sta seek_location+0
            iny
            clc
            lda (local_addr),y
            adc seek_location+1
            sta seek_location+1
            iny
            lda (local_addr),y
            adc seek_location+2
            sta seek_location+2
            lda #0
            adc seek_location+3
            sta seek_location+3
            ; Seek
            jsr do_seek
            bcc @seek_ok
            @eio:
                set_errno EIO
                rts
            @seek_ok:

            ; Transfer directly from volume to REU
            lda #ULTIDOS_TARGET
            sta CMD_DATA
            lda #DOS_CMD_LOAD_REU
            sta CMD_DATA
            lda io_addr+0
            sta CMD_DATA
            lda io_addr+1
            sta CMD_DATA
            lda io_addr+2
            sta CMD_DATA
            lda #0
            sta CMD_DATA
            lda rw_size+0
            sta CMD_DATA
            lda rw_size+1
            sta CMD_DATA
            lda rw_size+2
            sta CMD_DATA
            lda #0
            sta CMD_DATA
            jsr get_cmd_response
            lda io_xfer+256
            cmp #'0'
            jne @eio

            ; Advance file position
            clc
            ldy #filedata::cluster_ptr
            lda rw_size+0
            adc (local_addr),y
            sta (local_addr),y
            iny
            lda rw_size+1
            adc (local_addr),y
            sta (local_addr),y
            iny
            lda rw_size+2
            adc (local_addr),y
            sta (local_addr),y

            ; Advance io_addr
            clc
            lda rw_size+0
            adc io_addr+0
            sta io_addr+0
            lda rw_size+1
            adc io_addr+1
            sta io_addr+1
            lda rw_size+2
            adc io_addr+2
            sta io_addr+2

            ; Advance io_size
            sec
            lda io_size+0
            sbc rw_size+0
            sta io_size+0
            lda io_size+1
            sbc rw_size+1
            sta io_size+1
            lda io_size+2
            sbc rw_size+2
            sta io_size+2

        ; Continue until read is complete
        @end_read_loop:
        lda io_size+0
        ora io_size+1
        ora io_size+2
        jne @read_loop
        @end_read:

        ; Return the number of bytes read
        sec
        lda _RISCV_ireg_0+REG_a2
        sbc io_size+0
        sta _RISCV_ireg_0+REG_a0
        lda _RISCV_ireg_1+REG_a2
        sbc io_size+1
        sta _RISCV_ireg_1+REG_a0
        lda _RISCV_ireg_2+REG_a2
        sbc io_size+2
        sta _RISCV_ireg_2+REG_a0
        lda #0
        sta _RISCV_ireg_3+REG_a0
        rts

.endproc

; Write to the given file handle
; A0 = file handle
; A1 = pointer to data
; A2 = length of data

.global SYS_write
.proc SYS_write

    ; Set up the transfer area
    lda _RISCV_ireg_0+REG_a1
    sta io_addr+0
    lda _RISCV_ireg_1+REG_a1
    sta io_addr+1
    lda _RISCV_ireg_2+REG_a1
    sta io_addr+2
    lda _RISCV_ireg_3+REG_a1
    sta io_addr+3
    lda _RISCV_ireg_0+REG_a2
    sta io_size+0
    lda _RISCV_ireg_1+REG_a2
    sta io_size+1
    lda _RISCV_ireg_2+REG_a2
    sta io_size+2
    lda _RISCV_ireg_3+REG_a2
    sta io_size+3
    jsr check_read
    bcc read_ok
        set_errno EFAULT
        rts
    read_ok:

    ; Check for special file handles 0, 1 and 2
    lda _RISCV_ireg_1+REG_a0
    ora _RISCV_ireg_2+REG_a0
    ora _RISCV_ireg_3+REG_a0
    bne check_file
    lda _RISCV_ireg_0+REG_a0
    bne check_output
        ; Handle 0 (standard input; not valid)
        set_errno EBADF
        rts
    check_output:
    cmp #3
    bcs check_file
        ; Handle 1 (standard output) or 2 (standard error)
        @write_loop:
            jsr read_io_block
            lda io_count
            beq @end_write_loop
            lda #0
            sta io_index
            @write_byte:
                ldx io_index
                ldy io_xfer,x
                lda ascii_to_pet,y
                jsr CHROUT
                inc io_index
            dec io_count
            bne @write_byte
        beq @write_loop
        @end_write_loop:
        lda _RISCV_ireg_0+REG_a2
        sta _RISCV_ireg_0+REG_a0
        lda _RISCV_ireg_1+REG_a2
        sta _RISCV_ireg_1+REG_a0
        lda _RISCV_ireg_2+REG_a2
        sta _RISCV_ireg_2+REG_a0
        lda _RISCV_ireg_3+REG_a2
        sta _RISCV_ireg_3+REG_a0
        rts
    check_file:

    ; File I/O not yet implemented
    set_errno EBADF
    rts

.endproc

; SYS_fstatat      = bad_ecall

; Return information about an open file handle
; A0 = file handle
; A1 = address of stat structure
;      This is struct kernel_stat in Newlib libgloss/riscv/kernel_stat.h
.global SYS_fstat
.proc SYS_fstat

    ; Set up the transfer area
    lda _RISCV_ireg_0+REG_a1
    sta io_addr+0
    lda _RISCV_ireg_1+REG_a1
    sta io_addr+1
    lda _RISCV_ireg_2+REG_a1
    sta io_addr+2
    lda _RISCV_ireg_3+REG_a1
    sta io_addr+3
    lda #kernel_stat_size
    sta io_size+0
    lda #0
    sta io_size+1
    sta io_size+2
    sta io_size+3
    jsr check_write
    bcc write_ok
        set_errno EFAULT
        rts
    write_ok:

    ; Clear the transfer area
    ldx #kernel_stat_size
    lda #0
    clear:
        sta io_xfer-1,x
    dex
    bne clear

    ; Check for special file handles 0, 1 and 2
    lda _RISCV_ireg_1+REG_a0
    ora _RISCV_ireg_2+REG_a0
    ora _RISCV_ireg_3+REG_a0
    bne check_file
    lda _RISCV_ireg_0+REG_a0
    bne check_output
        ; Handle 0 (standard input)
        lda #<020444            ; Character device, writable
        sta io_xfer+kernel_stat::st_mode+0
        lda #>020444
        sta io_xfer+kernel_stat::st_mode+1
        lda #0
        sta io_xfer+kernel_stat::st_mode+2
        sta io_xfer+kernel_stat::st_mode+3
        sta _RISCV_ireg_0+REG_a0
        sta _RISCV_ireg_1+REG_a0
        sta _RISCV_ireg_2+REG_a0
        sta _RISCV_ireg_3+REG_a0
        jmp write_io_xfer
    check_output:
    cmp #3
    bcs check_file
        ; Handle 1 (standard output) or 2 (standard error)
        lda #<020222            ; Character device, readable
        sta io_xfer+kernel_stat::st_mode+0
        lda #>020222
        sta io_xfer+kernel_stat::st_mode+1
        lda #0
        sta io_xfer+kernel_stat::st_mode+2
        sta io_xfer+kernel_stat::st_mode+3
        sta _RISCV_ireg_0+REG_a0
        sta _RISCV_ireg_1+REG_a0
        sta _RISCV_ireg_2+REG_a0
        sta _RISCV_ireg_3+REG_a0
        jmp write_io_xfer
    check_file:

    ; File I/O not yet implemented
    set_errno EBADF
    rts

.endproc

; Get time of day
; A0 == pointer to struct timeval that will receive the time
.global SYS_gettimeofday
.proc SYS_gettimeofday

    ; Set up the transfer area
    lda _RISCV_ireg_0+REG_a0
    sta io_addr+0
    lda _RISCV_ireg_1+REG_a0
    sta io_addr+1
    lda _RISCV_ireg_2+REG_a0
    sta io_addr+2
    lda _RISCV_ireg_3+REG_a0
    sta io_addr+3
    lda #timeval_size
    sta io_size+0
    lda #0
    sta io_size+1
    sta io_size+2
    sta io_size+3
    jsr check_write
    bcc write_ok
        set_errno EFAULT
        rts
    write_ok:

    ; Get time from the U64
    jsr get_time

    ; get_time always succeeds
    ; Response is yyyy/mm/dd hh:mm:ss
    ; Convert the components to integer form
    ; Year is 1980-2079

    lda io_xfer+0 ; 1000s of year
    and #$0F
    tax
    lda thousand_lo,x
    sta io_xfer+32

    lda io_xfer+1 ; 100s of year
    and #$0F
    tax
    clc
    lda io_xfer+32
    adc hundred_lo,x
    sta io_xfer+32

    lda io_xfer+2 ; 10s of year
    and #$0F
    tax
    lda io_xfer+3 ; 1s of year
    and #$0F
    clc
    adc ten,x
    ; the add never carries
    adc io_xfer+32
    sta io_xfer+32

    ; io_xfer+32 == year % 256

    ; Subtract 1970 from year to get years from epoch
    sec
    sbc #<1970
    tay

    ; Y == year - 1970

    ; Look up days from epoch for the year
    lda year_to_day_lo,y
    sta io_xfer+33
    lda year_to_day_hi,y
    sta io_xfer+34

    ; Convert month to integer

    lda io_xfer+5   ; 10s of month
    and #$0F
    tax
    lda io_xfer+6   ; 1s of month
    and #$0F
    clc
    adc ten,x
    tax

    ; X = month
    ; Look up days from start of year
    clc
    lda io_xfer+33
    adc month_to_day_lo-1,x
    sta io_xfer+33
    lda io_xfer+34
    adc month_to_day_hi-1,x
    sta io_xfer+34

    ; Account for February 29
    cpx #$03
    bcc end_leap        ; month is January or February
    lda io_xfer+32
    and #$03
    bne end_leap        ; year divides by 4
        inc io_xfer+33
        bne end_leap
        inc io_xfer+34
    end_leap:

    ; Convert day to integer

    lda io_xfer+8   ; 10s of day
    and #$0F
    tax
    lda io_xfer+9   ; 1s of day
    and #$0F
    clc
    adc ten,x
    sec
    sbc #1
    clc
    adc io_xfer+33
    sta io_xfer+33
    lda #0
    adc io_xfer+34
    sta io_xfer+34

    ; io_xfer+33,34 == days from epoch
    ; Convert to hours
    lda #0
    .repeat 3
        asl io_xfer+33
        rol io_xfer+34
        rol a
    .endrep
    sta io_xfer+35      ; io_xfer+33,34,35 == days*8
    lda io_xfer+33
    asl a
    sta io_xfer+36
    lda io_xfer+34
    rol a
    sta io_xfer+37
    lda io_xfer+35
    rol a
    sta io_xfer+38      ; io_xfer+36,37,38 == days*16
    clc
    lda io_xfer+33
    adc io_xfer+36
    sta io_xfer+33
    lda io_xfer+34
    adc io_xfer+37
    sta io_xfer+34
    lda io_xfer+35
    adc io_xfer+38
    sta io_xfer+35      ; io_xfer+33,34,35 = days*24

    ; Convert hour to integer

    lda io_xfer+11  ; 10s of hour
    and #$0F
    tax
    lda io_xfer+12  ; 1s of hour
    and #$0F
    clc
    adc ten,x
    clc
    adc io_xfer+33
    sta io_xfer+33
    lda #0
    adc io_xfer+34
    sta io_xfer+34
    lda #0
    adc io_xfer+35
    sta io_xfer+35

    ; io_xfer+33,34,35 == hours from epoch
    ; Convert to minutes

    lda io_xfer+33
    sta io_xfer+37
    lda io_xfer+34
    sta io_xfer+38
    lda io_xfer+35
    sta io_xfer+39      ; io_xfer+37,38,39 == hours
    lda #0
    .repeat 4
        asl io_xfer+33
        rol io_xfer+34
        rol io_xfer+35
        rol a
    .endrep
    sta io_xfer+36      ; io_xfer+33,34,35,36 == hours*16
    sec
    lda io_xfer+33
    sbc io_xfer+37
    sta io_xfer+33
    lda io_xfer+34
    sbc io_xfer+38
    sta io_xfer+34
    lda io_xfer+35
    sbc io_xfer+39
    sta io_xfer+35
    lda io_xfer+36
    sbc #0              ; io_xfer+33,34,35,A == hours*15
    .repeat 2
        asl io_xfer+33
        rol io_xfer+34
        rol io_xfer+35
        rol a
    .endrep
    sta io_xfer+36      ; io_xfer+33,34,35,36 == hours*60

    ; Convert minute to integer

    lda io_xfer+14  ; 10s of minute
    and #$0F
    tax
    lda io_xfer+15  ; 1s of minute
    and #$0F
    clc
    adc ten,x
    clc
    adc io_xfer+33
    sta io_xfer+33
    lda #0
    adc io_xfer+34
    sta io_xfer+34
    lda #0
    adc io_xfer+35
    sta io_xfer+35
    lda #0
    adc io_xfer+36
    sta io_xfer+36

    ; io_xfer+33,34,35,36 == minutes from epoch
    ; Convert to seconds

    lda io_xfer+33
    sta io_xfer+37
    lda io_xfer+34
    sta io_xfer+38
    lda io_xfer+35
    sta io_xfer+39
    lda io_xfer+36
    sta io_xfer+40      ; io_xfer+37,38,39,40 == minutes
    .repeat 4
        asl io_xfer+33
        rol io_xfer+34
        rol io_xfer+35
        rol a
    .endrep
    sta io_xfer+36      ; io_xfer+33,34,35,36 == minutes*16
    sec
    lda io_xfer+33
    sbc io_xfer+37
    sta io_xfer+33
    lda io_xfer+34
    sbc io_xfer+38
    sta io_xfer+34
    lda io_xfer+35
    sbc io_xfer+39
    sta io_xfer+35
    lda io_xfer+36
    sbc io_xfer+40      ; io_xfer+33,34,35,A == minutes*15
    .repeat 2
        asl io_xfer+33
        rol io_xfer+34
        rol io_xfer+35
        rol a
    .endrep
    sta io_xfer+36      ; io_xfer+33,34,35,36 == minutes*60

    ; Convert second to integer and write to io_xfer+0

    lda io_xfer+17  ; 10s of second
    and #$0F
    tax
    lda io_xfer+18  ; 1s of second
    and #$0F
    clc
    adc ten,x
    clc
    adc io_xfer+33
    sta io_xfer+0
    lda #0
    adc io_xfer+34
    sta io_xfer+1
    lda #0
    adc io_xfer+35
    sta io_xfer+2
    lda #0
    adc io_xfer+36
    sta io_xfer+3

    ; Fill out to a struct timeval
    ldx #11
    lda #0
    zero:
        sta io_xfer+4,x
    dex
    bpl zero

    jsr write_io_xfer

    lda #0
    sta _RISCV_ireg_0+REG_a0
    sta _RISCV_ireg_1+REG_a0
    sta _RISCV_ireg_2+REG_a0
    sta _RISCV_ireg_3+REG_a0
    rts

year_to_day_lo:
    .byte <(  0*365+ 0) ; 1970
    .byte <(  1*365+ 0) ; 1971
    .byte <(  2*365+ 0) ; 1972
    .byte <(  3*365+ 1) ; 1973
    .byte <(  4*365+ 1) ; 1974
    .byte <(  5*365+ 1) ; 1975
    .byte <(  6*365+ 1) ; 1976
    .byte <(  7*365+ 2) ; 1977
    .byte <(  8*365+ 2) ; 1978
    .byte <(  9*365+ 2) ; 1979
    .byte <( 10*365+ 2) ; 1980
    .byte <( 11*365+ 3) ; 1981
    .byte <( 12*365+ 3) ; 1982
    .byte <( 13*365+ 3) ; 1983
    .byte <( 14*365+ 3) ; 1984
    .byte <( 15*365+ 4) ; 1985
    .byte <( 16*365+ 4) ; 1986
    .byte <( 17*365+ 4) ; 1987
    .byte <( 18*365+ 4) ; 1988
    .byte <( 19*365+ 5) ; 1989
    .byte <( 20*365+ 5) ; 1990
    .byte <( 21*365+ 5) ; 1991
    .byte <( 22*365+ 5) ; 1992
    .byte <( 23*365+ 6) ; 1993
    .byte <( 24*365+ 6) ; 1994
    .byte <( 25*365+ 6) ; 1995
    .byte <( 26*365+ 6) ; 1996
    .byte <( 27*365+ 7) ; 1997
    .byte <( 28*365+ 7) ; 1998
    .byte <( 29*365+ 7) ; 1999
    .byte <( 30*365+ 7) ; 2000
    .byte <( 31*365+ 8) ; 2001
    .byte <( 32*365+ 8) ; 2002
    .byte <( 33*365+ 8) ; 2003
    .byte <( 34*365+ 8) ; 2004
    .byte <( 35*365+ 9) ; 2005
    .byte <( 36*365+ 9) ; 2006
    .byte <( 37*365+ 9) ; 2007
    .byte <( 38*365+ 9) ; 2008
    .byte <( 39*365+10) ; 2009
    .byte <( 40*365+10) ; 2010
    .byte <( 41*365+10) ; 2011
    .byte <( 42*365+10) ; 2012
    .byte <( 43*365+11) ; 2013
    .byte <( 44*365+11) ; 2014
    .byte <( 45*365+11) ; 2015
    .byte <( 46*365+11) ; 2016
    .byte <( 47*365+12) ; 2017
    .byte <( 48*365+12) ; 2018
    .byte <( 49*365+12) ; 2019
    .byte <( 50*365+12) ; 2020
    .byte <( 51*365+13) ; 2021
    .byte <( 52*365+13) ; 2022
    .byte <( 53*365+13) ; 2023
    .byte <( 54*365+13) ; 2024
    .byte <( 55*365+14) ; 2025
    .byte <( 56*365+14) ; 2026
    .byte <( 57*365+14) ; 2027
    .byte <( 58*365+14) ; 2028
    .byte <( 59*365+15) ; 2029
    .byte <( 60*365+15) ; 2030
    .byte <( 61*365+15) ; 2031
    .byte <( 62*365+15) ; 2032
    .byte <( 63*365+16) ; 2033
    .byte <( 64*365+16) ; 2034
    .byte <( 65*365+16) ; 2035
    .byte <( 66*365+16) ; 2036
    .byte <( 67*365+17) ; 2037
    .byte <( 68*365+17) ; 2038
    .byte <( 69*365+17) ; 2039
    .byte <( 70*365+17) ; 2040
    .byte <( 71*365+18) ; 2041
    .byte <( 72*365+18) ; 2042
    .byte <( 73*365+18) ; 2043
    .byte <( 74*365+18) ; 2044
    .byte <( 75*365+19) ; 2045
    .byte <( 76*365+19) ; 2046
    .byte <( 77*365+19) ; 2047
    .byte <( 78*365+19) ; 2048
    .byte <( 79*365+20) ; 2049
    .byte <( 80*365+20) ; 2050
    .byte <( 81*365+20) ; 2051
    .byte <( 82*365+20) ; 2052
    .byte <( 83*365+21) ; 2053
    .byte <( 84*365+21) ; 2054
    .byte <( 85*365+21) ; 2055
    .byte <( 86*365+21) ; 2056
    .byte <( 87*365+22) ; 2057
    .byte <( 88*365+22) ; 2058
    .byte <( 89*365+22) ; 2059
    .byte <( 90*365+22) ; 2060
    .byte <( 91*365+23) ; 2061
    .byte <( 92*365+23) ; 2062
    .byte <( 93*365+23) ; 2063
    .byte <( 94*365+23) ; 2064
    .byte <( 95*365+24) ; 2065
    .byte <( 96*365+24) ; 2066
    .byte <( 97*365+24) ; 2067
    .byte <( 98*365+24) ; 2068
    .byte <( 99*365+25) ; 2069
    .byte <(100*365+25) ; 2070
    .byte <(101*365+25) ; 2071
    .byte <(102*365+25) ; 2072
    .byte <(103*365+26) ; 2073
    .byte <(104*365+26) ; 2074
    .byte <(105*365+26) ; 2075
    .byte <(106*365+26) ; 2076
    .byte <(107*365+27) ; 2077
    .byte <(108*365+27) ; 2078
    .byte <(109*365+27) ; 2079
    .byte <(110*365+27) ; 2080
    .byte <(111*365+28) ; 2081
    .byte <(112*365+28) ; 2082
    .byte <(113*365+28) ; 2083
    .byte <(114*365+28) ; 2084
    .byte <(115*365+29) ; 2085
    .byte <(116*365+29) ; 2086
    .byte <(117*365+29) ; 2087
    .byte <(118*365+29) ; 2088
    .byte <(119*365+30) ; 2089
    .byte <(120*365+30) ; 2090
    .byte <(121*365+30) ; 2091
    .byte <(122*365+30) ; 2092
    .byte <(123*365+31) ; 2093
    .byte <(124*365+31) ; 2094
    .byte <(125*365+31) ; 2095
    .byte <(126*365+31) ; 2096
    .byte <(127*365+32) ; 2097
    .byte <(128*365+32) ; 2098
    .byte <(129*365+32) ; 2099

year_to_day_hi:
    .byte >(  0*365+ 0) ; 1970
    .byte >(  1*365+ 0) ; 1971
    .byte >(  2*365+ 0) ; 1972
    .byte >(  3*365+ 1) ; 1973
    .byte >(  4*365+ 1) ; 1974
    .byte >(  5*365+ 1) ; 1975
    .byte >(  6*365+ 1) ; 1976
    .byte >(  7*365+ 2) ; 1977
    .byte >(  8*365+ 2) ; 1978
    .byte >(  9*365+ 2) ; 1979
    .byte >( 10*365+ 2) ; 1980
    .byte >( 11*365+ 3) ; 1981
    .byte >( 12*365+ 3) ; 1982
    .byte >( 13*365+ 3) ; 1983
    .byte >( 14*365+ 3) ; 1984
    .byte >( 15*365+ 4) ; 1985
    .byte >( 16*365+ 4) ; 1986
    .byte >( 17*365+ 4) ; 1987
    .byte >( 18*365+ 4) ; 1988
    .byte >( 19*365+ 5) ; 1989
    .byte >( 20*365+ 5) ; 1990
    .byte >( 21*365+ 5) ; 1991
    .byte >( 22*365+ 5) ; 1992
    .byte >( 23*365+ 6) ; 1993
    .byte >( 24*365+ 6) ; 1994
    .byte >( 25*365+ 6) ; 1995
    .byte >( 26*365+ 6) ; 1996
    .byte >( 27*365+ 7) ; 1997
    .byte >( 28*365+ 7) ; 1998
    .byte >( 29*365+ 7) ; 1999
    .byte >( 30*365+ 7) ; 2000
    .byte >( 31*365+ 8) ; 2001
    .byte >( 32*365+ 8) ; 2002
    .byte >( 33*365+ 8) ; 2003
    .byte >( 34*365+ 8) ; 2004
    .byte >( 35*365+ 9) ; 2005
    .byte >( 36*365+ 9) ; 2006
    .byte >( 37*365+ 9) ; 2007
    .byte >( 38*365+ 9) ; 2008
    .byte >( 39*365+10) ; 2009
    .byte >( 40*365+10) ; 2010
    .byte >( 41*365+10) ; 2011
    .byte >( 42*365+10) ; 2012
    .byte >( 43*365+11) ; 2013
    .byte >( 44*365+11) ; 2014
    .byte >( 45*365+11) ; 2015
    .byte >( 46*365+11) ; 2016
    .byte >( 47*365+12) ; 2017
    .byte >( 48*365+12) ; 2018
    .byte >( 49*365+12) ; 2019
    .byte >( 50*365+12) ; 2020
    .byte >( 51*365+13) ; 2021
    .byte >( 52*365+13) ; 2022
    .byte >( 53*365+13) ; 2023
    .byte >( 54*365+13) ; 2024
    .byte >( 55*365+14) ; 2025
    .byte >( 56*365+14) ; 2026
    .byte >( 57*365+14) ; 2027
    .byte >( 58*365+14) ; 2028
    .byte >( 59*365+15) ; 2029
    .byte >( 60*365+15) ; 2030
    .byte >( 61*365+15) ; 2031
    .byte >( 62*365+15) ; 2032
    .byte >( 63*365+16) ; 2033
    .byte >( 64*365+16) ; 2034
    .byte >( 65*365+16) ; 2035
    .byte >( 66*365+16) ; 2036
    .byte >( 67*365+17) ; 2037
    .byte >( 68*365+17) ; 2038
    .byte >( 69*365+17) ; 2039
    .byte >( 70*365+17) ; 2040
    .byte >( 71*365+18) ; 2041
    .byte >( 72*365+18) ; 2042
    .byte >( 73*365+18) ; 2043
    .byte >( 74*365+18) ; 2044
    .byte >( 75*365+19) ; 2045
    .byte >( 76*365+19) ; 2046
    .byte >( 77*365+19) ; 2047
    .byte >( 78*365+19) ; 2048
    .byte >( 79*365+20) ; 2049
    .byte >( 80*365+20) ; 2050
    .byte >( 81*365+20) ; 2051
    .byte >( 82*365+20) ; 2052
    .byte >( 83*365+21) ; 2053
    .byte >( 84*365+21) ; 2054
    .byte >( 85*365+21) ; 2055
    .byte >( 86*365+21) ; 2056
    .byte >( 87*365+22) ; 2057
    .byte >( 88*365+22) ; 2058
    .byte >( 89*365+22) ; 2059
    .byte >( 90*365+22) ; 2060
    .byte >( 91*365+23) ; 2061
    .byte >( 92*365+23) ; 2062
    .byte >( 93*365+23) ; 2063
    .byte >( 94*365+23) ; 2064
    .byte >( 95*365+24) ; 2065
    .byte >( 96*365+24) ; 2066
    .byte >( 97*365+24) ; 2067
    .byte >( 98*365+24) ; 2068
    .byte >( 99*365+25) ; 2069
    .byte >(100*365+25) ; 2070
    .byte >(101*365+25) ; 2071
    .byte >(102*365+25) ; 2072
    .byte >(103*365+26) ; 2073
    .byte >(104*365+26) ; 2074
    .byte >(105*365+26) ; 2075
    .byte >(106*365+26) ; 2076
    .byte >(107*365+27) ; 2077
    .byte >(108*365+27) ; 2078
    .byte >(109*365+27) ; 2079
    .byte >(110*365+27) ; 2080
    .byte >(111*365+28) ; 2081
    .byte >(112*365+28) ; 2082
    .byte >(113*365+28) ; 2083
    .byte >(114*365+28) ; 2084
    .byte >(115*365+29) ; 2085
    .byte >(116*365+29) ; 2086
    .byte >(117*365+29) ; 2087
    .byte >(118*365+29) ; 2088
    .byte >(119*365+30) ; 2089
    .byte >(120*365+30) ; 2090
    .byte >(121*365+30) ; 2091
    .byte >(122*365+30) ; 2092
    .byte >(123*365+31) ; 2093
    .byte >(124*365+31) ; 2094
    .byte >(125*365+31) ; 2095
    .byte >(126*365+31) ; 2096
    .byte >(127*365+32) ; 2097
    .byte >(128*365+32) ; 2098
    .byte >(129*365+32) ; 2099

; Month to day of year
month_to_day_lo:
    .byte <0        ; January
    .byte <31       ; February
    .byte <59       ; March
    .byte <90       ; April
    .byte <120      ; May
    .byte <151      ; June
    .byte <181      ; July
    .byte <212      ; August
    .byte <243      ; September
    .byte <273      ; October
    .byte <304      ; November
    .byte <334      ; December

month_to_day_hi:
    .byte >0        ; January
    .byte >31       ; February
    .byte >59       ; March
    .byte >90       ; April
    .byte >120      ; May
    .byte >151      ; June
    .byte >181      ; July
    .byte >212      ; August
    .byte >243      ; September
    .byte >273      ; October
    .byte >304      ; November
    .byte >334      ; December

.endproc

.global SYS_brk
.proc SYS_brk

    ; If A0 == 0, query the current break; this always succeeds
    lda _RISCV_ireg_0+REG_a0
    ora _RISCV_ireg_1+REG_a0
    ora _RISCV_ireg_2+REG_a0
    ora _RISCV_ireg_3+REG_a0
    bne set_break

        lda RISCV_break+0
        sta _RISCV_ireg_0+REG_a0
        lda RISCV_break+1
        sta _RISCV_ireg_1+REG_a0
        lda RISCV_break+2
        sta _RISCV_ireg_2+REG_a0
        lda RISCV_break+3
        sta _RISCV_ireg_3+REG_a0
        rts

    set_break:

    ; A0 != 0 sets a new break
    ; The new break must be in space 1 (the REU)
    lda _RISCV_ireg_3+REG_a0
    cmp #1
    bne error

    ; The new break must not be less than the initial break
    lda _RISCV_ireg_0+REG_a0
    cmp RISCV_min_break+0
    lda _RISCV_ireg_1+REG_a0
    sbc RISCV_min_break+1
    lda _RISCV_ireg_2+REG_a0
    sbc RISCV_min_break+2
    lda _RISCV_ireg_3+REG_a0
    sbc RISCV_min_break+3
    bcc error

    ; The new break must be less than the current stack pointer
    lda _RISCV_ireg_0+REG_a0
    cmp _RISCV_ireg_0+REG_sp
    lda _RISCV_ireg_1+REG_a0
    sbc _RISCV_ireg_1+REG_sp
    lda _RISCV_ireg_2+REG_a0
    sbc _RISCV_ireg_2+REG_sp
    lda _RISCV_ireg_3+REG_a0
    sbc _RISCV_ireg_3+REG_sp
    bcs error

    ; Set the new break and return the current break
    lda _RISCV_ireg_0+REG_a0
    ldx RISCV_break+0
    stx _RISCV_ireg_0+REG_a0
    sta RISCV_break+0
    lda _RISCV_ireg_1+REG_a0
    ldx RISCV_break+1
    stx _RISCV_ireg_1+REG_a0
    sta RISCV_break+1
    lda _RISCV_ireg_2+REG_a0
    ldx RISCV_break+2
    stx _RISCV_ireg_2+REG_a0
    sta RISCV_break+2
    lda _RISCV_ireg_3+REG_a0
    ldx RISCV_break+3
    stx _RISCV_ireg_3+REG_a0
    sta RISCV_break+3
    rts

error:
    lda #$FF
    sta _RISCV_ireg_0+REG_a0
    sta _RISCV_ireg_1+REG_a0
    sta _RISCV_ireg_2+REG_a0
    sta _RISCV_ireg_3+REG_a0
    rts

.endproc

; Open a file
; Path is in A0
; Open flags are in A1:
;     0       (O_RDONLY)
;     1       (O_WRONLY)
;     2       (O_RDWR)
;     $0008   (O_APPEND)
;     $0200   (O_CREAT)
;     $0400   (O_TRUNC)
;     $0800   (O_EXCL)
;     $200000 (O_DIRECTORY)
; Creation mode is in A2
.global SYS_open
.proc SYS_open

    ; Check for invalid open flag
    lda _RISCV_ireg_0+REG_a1
    and #O_ACCMODE
    cmp #3
    bne mode_ok
        set_errno EINVAL
        rts
    mode_ok:

    ; Look for an unused file handle
    ldx #0
    stx local_addr+0
    find_handle:
        ldy open_files_lo,x
        lda open_files_hi,x
        sta local_addr+1
        lda (local_addr),y
        beq handle_ok
    inx
    cpx #MAX_FILES
    bcc find_handle
        set_errno ENFILE
        rts
    handle_ok:
    stx file_handle

    ; Set up the transfer area
    ; We don't know the length in advance. Check that we can read at least
    ; one byte. read_path will check that the path does not exceed readable
    ; space.
    lda _RISCV_ireg_0+REG_a0
    sta io_addr+0
    lda _RISCV_ireg_1+REG_a0
    sta io_addr+1
    lda _RISCV_ireg_2+REG_a0
    sta io_addr+2
    lda _RISCV_ireg_3+REG_a0
    sta io_addr+3
    lda #1
    sta io_size+0
    lda #0
    sta io_size+1
    sta io_size+2
    sta io_size+3
    jsr check_read
    bcc read_ok
        set_errno EFAULT
        rts
    read_ok:

    ; Read path into fs_path
    jsr read_path
    bcc path_ok
        sta _RISCV_ireg_0+REG_a0
        lda #$FF
        sta _RISCV_ireg_1+REG_a0
        sta _RISCV_ireg_2+REG_a0
        sta _RISCV_ireg_3+REG_a0
        rts
    path_ok:

    ; Begin traversal of path:
    ; Start at start of path string
    lda #<fs_path
    sta path_ptr+0
    lda #>fs_path
    sta path_ptr+1

    ; Start at current directory, or root if absolute path
    lda #0
    ldy #filedata_size-1
    zero_loop:
        sta file_data,y
    dey
    bpl zero_loop
    lda #ATTR_DIRECTORY
    sta file_data+filedata::attributes
    lda fs_current_dir+0
    sta file_data+filedata::cluster_lo+0
    lda fs_current_dir+1
    sta file_data+filedata::cluster_lo+1
    lda fs_current_dir+2
    sta file_data+filedata::cluster_hi+0
    lda fs_current_dir+3
    sta file_data+filedata::cluster_hi+1
    lda fs_path+0
    bne path_not_empty
    ; Empty path is not valid
        set_errno ENOENT
        rts
    path_not_empty:
    cmp #$2F
    beq root_dir
    cmp #$5C
    bne current_dir
    root_dir:
        ; Start search from root directory
        lda fs_type
        cmp #32
        beq fat32
            ; For FAT12 and FAT16, root directory has its own special area
            lda #0
            sta file_data+filedata::cluster_lo+0
            sta file_data+filedata::cluster_lo+1
            sta file_data+filedata::cluster_hi+0
            sta file_data+filedata::cluster_hi+1
            beq current_dir
        fat32:
            ; For FAT32, root directory starts at a cluster
            lda fs_root_dir+0
            sta file_data+filedata::cluster_lo+0
            lda fs_root_dir+1
            sta file_data+filedata::cluster_lo+1
            lda fs_root_dir+2
            sta file_data+filedata::cluster_hi+0
            lda fs_root_dir+3
            sta file_data+filedata::cluster_hi+1
    current_dir:

    path_loop:
        lda file_data+filedata::cluster_lo+0
        sta file_data+filedata::dir_start+0
        lda file_data+filedata::cluster_lo+1
        sta file_data+filedata::dir_start+1
        lda file_data+filedata::cluster_hi+0
        sta file_data+filedata::dir_start+2
        lda file_data+filedata::cluster_hi+1
        sta file_data+filedata::dir_start+3
        jsr get_path_part
        bcs end_path_loop
        jsr find_entry
    bcc path_loop
        ; If we exit the loop this way, we didn't find the file.
        ; If this was by ENOENT, O_CREAT is specified, and the search failed
        ; on the last component of the file, go to the file creation path.
        cmp #$100-ENOENT
        bne path_err        ; some other error
        lda _RISCV_ireg_1+REG_a1
        and #O_CREAT>>8
        beq path_err        ; no O_CREAT flag
        jsr is_last_component
        lda #$100-ENOENT
        bcs path_err
        jmp create_file
    path_err:
        sta _RISCV_ireg_0+REG_a0
        lda #$FF
        sta _RISCV_ireg_1+REG_a0
        sta _RISCV_ireg_2+REG_a0
        sta _RISCV_ireg_3+REG_a0
        rts
    end_path_loop:
    cmp #0
    bne path_err

    ;jsr dump_dir_entry

    ; Set open mode flags
    lda _RISCV_ireg_0+REG_a1
    sta file_data+filedata::open_flags+0
    lda _RISCV_ireg_1+REG_a1
    sta file_data+filedata::open_flags+1
    lda _RISCV_ireg_2+REG_a1
    sta file_data+filedata::open_flags+2

    ; If we proceed by this path, we found the file.
    ; If O_EXCL is specified, return an error.
    lda file_data+filedata::open_flags+1
    and #O_EXCL>>8
    beq :+
        set_errno EEXIST
        rts
    :

    ; Check the attribute bits for conflicts with the open flags
    lda file_data+filedata::attributes
    and #ATTR_VOLUME
    beq not_volume
        ; Can't open a volume label
        set_errno EACCES
        rts
    not_volume:
    lda file_data+filedata::attributes
    and #ATTR_DIRECTORY
    beq not_directory
        ; File is a directory. This is valid only of O_DIRECTORY | O_RDONLY.
        lda file_data+filedata::open_flags+2
        and #O_DIRECTORY >> 16
        bne ok_1
            ; Didn't specify O_DIRECTORY
            set_errno EISDIR
            rts
        ok_1:
        lda file_data+filedata::open_flags+0
        and #O_ACCMODE
        beq end_attr_check
            ; Can only open for reading
            set_errno EACCES
            rts
    not_directory:
        ; File is a regular file.
        lda file_data+filedata::open_flags+2
        and #O_DIRECTORY >> 16
        beq ok_2
            ; Requested a directory, got a regular file
            set_errno ENOTDIR
            rts
        ok_2:
        ; If read-only, check for open for writing
        lda file_data+filedata::attributes
        and #ATTR_READONLY
        beq end_attr_check
            lda file_data+filedata::open_flags+0
            and #O_ACCMODE
            beq end_attr_check
                ; Can only open for reading
                set_errno EACCES
                rts
    end_attr_check:

    ; Check for conflicts with other openings of the same file
    ldx #MAX_FILES-1
    share_loop:
        ; Establish a pointer to a record for an already-open file
        lda open_files_lo,x
        sta local_addr+0
        lda open_files_hi,x
        sta local_addr+1
        ; Check that the record contains an open file
        ldy #0
        lda (local_addr),y
        jeq next_share ; Not open file
        ; Check that the record refers to the same file as the one being opened
        ldy #filedata::dir_entry
        lda (local_addr),y
        cmp file_data,y
        bne next_share
        iny
        lda (local_addr),y
        cmp file_data,y
        bne next_share
        iny
        lda (local_addr),y
        cmp file_data,y
        bne next_share
        iny
        lda (local_addr),y
        cmp file_data,y
        bne next_share
        ; It's the same file. Both must be open read-only
        ldy #filedata::open_flags+0
        lda file_data,y
        ora (local_addr),y
        and #O_ACCMODE
        beq next_share
        bad_share:
            ; One is opened in a writable mode
            set_errno EACCES
            rts
    next_share:
    dex
    jpl share_loop

    ; Truncate the file, if O_TRUNC and a writing mode
    lda file_data+filedata::open_flags+0
    and #O_ACCMODE
    beq end_truncate ; Ignore O_TRUNC if O_RDONLY
    lda file_data+filedata::open_flags+1
    and #O_TRUNC>>8
    beq end_truncate
        lda file_data+filedata::cluster_lo+0
        sta cluster+0
        lda file_data+filedata::cluster_lo+1
        sta cluster+1
        lda file_data+filedata::cluster_hi+0
        sta cluster+2
        lda file_data+filedata::cluster_hi+1
        sta cluster+3
        jsr delete_cluster_chain
        lda #0
        sta file_data+filedata::cluster_lo+0
        sta file_data+filedata::cluster_lo+1
        sta file_data+filedata::cluster_hi+0
        sta file_data+filedata::cluster_hi+1
        sta file_data+filedata::size+0
        sta file_data+filedata::size+1
        sta file_data+filedata::size+2
        sta file_data+filedata::size+3
    end_truncate:

return_from_open:
    ; Set the number of bytes in the last cluster
    ldy fs_cluster_size+1   ; Y:X <- fs_cluster_size-1
    ldx fs_cluster_size+0
    bne :+
        dey
    :
    dex
    lda file_data+filedata::size+0
    sta file_data+filedata::cluster_partial+0
    txa
    and file_data+filedata::size+1
    sta file_data+filedata::cluster_partial+1
    tya
    and file_data+filedata::size+2
    sta file_data+filedata::cluster_partial+2

    ; Set the number of whole clusters in the file
    lda file_data+filedata::size+1
    sta file_data+filedata::cluster_count+0
    lda file_data+filedata::size+2
    sta file_data+filedata::cluster_count+1
    lda file_data+filedata::size+3
    ldx fs_cluster_shift
    beq @end_shift
    @shift:
        lsr a
        ror file_data+filedata::cluster_count+1
        ror file_data+filedata::cluster_count+0
    dex
    bne @shift
    @end_shift:
    sta file_data+filedata::cluster_count+2

    ; Set the initial file position to zero
    lda file_data+filedata::cluster_lo+0
    sta file_data+filedata::current_cluster+0
    lda file_data+filedata::cluster_lo+1
    sta file_data+filedata::current_cluster+1
    lda file_data+filedata::cluster_hi+0
    sta file_data+filedata::current_cluster+2
    lda #0
    sta file_data+filedata::cluster_pos+0
    sta file_data+filedata::cluster_pos+1
    sta file_data+filedata::cluster_pos+2
    sta file_data+filedata::cluster_ptr+0
    sta file_data+filedata::cluster_ptr+1
    sta file_data+filedata::cluster_ptr+2

    ; Copy file_data into the selected entry of open_files
    ldx file_handle
    lda open_files_lo,x
    sta local_addr+0
    lda open_files_hi,x
    sta local_addr+1
    ldy #filedata_size-1
    copy:
        lda file_data,y
        sta (local_addr),y
    dey
    bpl copy

    ; Return the file handle
    lda file_handle
    clc
    adc #3
    sta _RISCV_ireg_0+REG_a0
    lda #$00
    sta _RISCV_ireg_1+REG_a0
    sta _RISCV_ireg_2+REG_a0
    sta _RISCV_ireg_3+REG_a0
    rts

create_file:
    ; Come here to create the file.
    ; filedata::dir_start has the starting cluster of the enclosing directory,
    ; or 0 if that directory is the root on FAT12 or FAT16.
    ; new_dir_entry has the location of an empty directory entry, or 0 if none
    ; was found.
    ; path_part has the 8.3 name of the new file, formatted correctly for the
    ; new directory entry.

    ; Address of temporary file data block, for allocate_cluster, set_mtime and
    ; write_dir_entry
    lda #<file_data
    sta local_addr+0
    lda #>file_data
    sta local_addr+1

    ; Do we have a free directory entry available?
    lda new_dir_entry+0
    ora new_dir_entry+1
    ora new_dir_entry+2
    ora new_dir_entry+3
    jne have_dir_entry

        ; No free entry was found.
        ; If the directory is the FAT12/FAT16 root, we can't extend it.
        lda file_data+filedata::dir_start+0
        ora file_data+filedata::dir_start+1
        ora file_data+filedata::dir_start+2
        bne extend_dir
            set_errno ENOSPC
            rts
        extend_dir:

        ; last_cluster marks the cluster number that ends the directory, or 0
        ; if the directory is empty.
        ; Pass this to allocate_cluster so it can update the FAT to point to a
        ; new cluster.
        lda last_cluster+0
        sta cluster+0
        lda last_cluster+1
        sta cluster+1
        lda last_cluster+2
        sta cluster+2
        lda #0
        sta cluster+3

        ; Allocate a new cluster if possible
        jsr allocate_cluster
        bcc :+
            sta _RISCV_ireg_0+REG_a0
            lda #$FF
            sta _RISCV_ireg_1+REG_a0
            sta _RISCV_ireg_2+REG_a0
            sta _RISCV_ireg_3+REG_a0
            rts
        :

        ; Convert cluster number to byte position
        lda #0
        sta new_dir_entry+0
        lda cluster+0
        sta new_dir_entry+1
        lda cluster+1
        sta new_dir_entry+2
        lda cluster+2
        ldx fs_cluster_shift
        beq @end_shift
        @shift:
            asl new_dir_entry+1
            rol new_dir_entry+2
            rol a
        dex
        bpl @shift
        @end_shift:
        sta new_dir_entry+3
        clc
        lda fs_cluster_base+0
        adc new_dir_entry+1
        sta new_dir_entry+1
        lda fs_cluster_base+1
        adc new_dir_entry+2
        sta new_dir_entry+2
        lda fs_cluster_base+2
        adc new_dir_entry+3
        sta new_dir_entry+3

        ; Seek to the new cluster
        lda new_dir_entry+0
        sta seek_location+0
        lda new_dir_entry+1
        sta seek_location+1
        lda new_dir_entry+2
        sta seek_location+2
        lda new_dir_entry+3
        sta seek_location+3
        jsr do_seek
        bcc :+
            set_errno EIO
            rts
        :

        ; Fill the cluster with zeros:
        ; Set cluster size in 256-byte blocks
        ; Make negative to simplify the loop
        sec
        lda #0
        sbc fs_cluster_size+0
        sta dir_size+0
        lda #0
        sbc fs_cluster_size+1
        sta dir_size+1
        @fill_cluster:

            jsr begin_write
            lda #0
            tax                 ; write 256 zeros
            @fill_bytes:
                sta CMD_DATA
            dex
            bne @fill_bytes
            jsr end_write
            bcc :+
                set_errno EIO
                rts
            :

        inc dir_size+0
        bne @fill_cluster
        inc dir_size+1
        bne @fill_cluster

        ; new_dir_entry points to the newly allocated cluster.

    have_dir_entry:

    ; Fill in the file_data structure:

    ; name
    ldx #10
    copy_name:
        lda path_part,x
        sta file_data+filedata::name,x
    dex
    bpl copy_name

    ; attributes
    lda file_data+filedata::open_flags+2
    and #O_DIRECTORY >> 16
    bne directory
        ; Normal file.
        ; If the creation mode contains any write bit, make the file writable
        lda _RISCV_ireg_0+REG_a2
        and #%010010010
        beq @read_only
            ; Writable file
            lda #ATTR_ARCHIVE
            bne set_attr
        @read_only:
            ; Read-only
            lda #ATTR_ARCHIVE | ATTR_READONLY
            bne set_attr
    directory:
        lda #ATTR_DIRECTORY
    set_attr:
    sta file_data+filedata::attributes

    ; Creation and modification time
    jsr set_mtime
    sta file_data+filedata::ctime_lo ; odd second
    lda file_data+filedata::mtime+0
    sta file_data+filedata::ctime+0
    lda file_data+filedata::mtime+1
    sta file_data+filedata::ctime+1
    lda file_data+filedata::mdate+0
    sta file_data+filedata::cdate+0
    lda file_data+filedata::mdate+1
    sta file_data+filedata::cdate+1

    ; The file is initially empty
    lda #0
    sta file_data+filedata::case_flags
    sta file_data+filedata::reserved+0
    sta file_data+filedata::reserved+1
    sta file_data+filedata::cluster_hi+0
    sta file_data+filedata::cluster_hi+1
    sta file_data+filedata::cluster_lo+0
    sta file_data+filedata::cluster_lo+1
    sta file_data+filedata::size+0
    sta file_data+filedata::size+1
    sta file_data+filedata::size+2
    sta file_data+filedata::size+3

    ; Write the directory entry to the volume
    jsr write_dir_entry
    bcc :+
        sta _RISCV_ireg_0+REG_a0
        lda #$FF
        sta _RISCV_ireg_1+REG_a0
        sta _RISCV_ireg_2+REG_a0
        sta _RISCV_ireg_3+REG_a0
        rts
    :

    ; Location of the directory entry
    lda new_dir_entry+0
    sta file_data+filedata::dir_entry+0
    sta file_data+filedata::lfn_entry+0
    lda new_dir_entry+1
    sta file_data+filedata::dir_entry+1
    sta file_data+filedata::lfn_entry+1
    lda new_dir_entry+2
    sta file_data+filedata::dir_entry+2
    sta file_data+filedata::lfn_entry+2
    lda new_dir_entry+3
    sta file_data+filedata::dir_entry+3
    sta file_data+filedata::lfn_entry+3

    jmp return_from_open

.endproc

; Create a hard link
; Newlib can call this, but the file system doesn't support it
.global SYS_link
.proc SYS_link

    set_errno ENOTSUP
    rts

.endproc

; SYS_unlink       = bad_ecall
; SYS_access       = bad_ecall
; SYS_stat         = bad_ecall
; SYS_lstat        = bad_ecall

; Check the configured address and size for valid write
; Set C if address and size are invalid
; Must be placed immediately before check_read so a branch will reach
.proc check_write

    ; Requirement is the same as check_read, except that the address must also
    ; be above the fence
    lda RISCV_fence
    cmp io_addr+2
    bcc check_read
    rts

.endproc

; Check the configured address and size for valid read
; Set C if address and size are invalid
.proc check_read

    ; I/O is to Space 1 (the REU) only
    ; The checks done here will not allow I/O to the last byte of the REU.
    ; This address is within the area used to build the parameters to main(),
    ; and is unlikely to be used for I/O.

    lda io_addr+3
    cmp #1
    bne bad_read

    ; I/O shall not exceed 16 MB
    lda io_size+3
    bne bad_read

    ; I/O shall not exceed the end of the REU
    clc
    lda io_addr+0
    adc io_size+0
    lda io_addr+1
    adc io_size+1
    lda io_addr+2
    adc io_size+2
    ; C set if the add overflows

    rts

bad_read:
    sec
    rts

.endproc

; Read next block into io_xfer
; Return size in io_count; update io_addr and io_size
.proc read_io_block

    ; Determine the size for this transfer
    lda io_size+1
    ora io_size+2
    beq check_size_0
        lda #xfer_size
        bne got_size
    check_size_0:
        lda io_size+0
        cmp #xfer_size
        bcc got_size
            lda #xfer_size
    got_size:
    sta io_count
    cmp #0
    beq end_read

    ; Space 1: RAM Expansion Unit
    sta reu_xfer_size_0
    set_reu_address io_addr
    set_local_address io_xfer
    lda #0
    sta reu_xfer_size_1
    do_reu_read

    ; Update io_addr and io_size
    clc
    lda io_addr+0
    adc io_count
    sta io_addr+0
    lda io_addr+1
    adc #0
    sta io_addr+1
    lda io_addr+2
    adc #0
    sta io_addr+2
    sec
    lda io_size+0
    sbc io_count
    sta io_size+0
    lda io_size+1
    sbc #0
    sta io_size+1
    lda io_size+2
    sbc #0
    sta io_size+2

end_read:
    rts

.endproc

; Pass the io_xfer area to the RISC-V target
.proc write_io_xfer

    set_reu_address io_addr
    set_xfer_size io_size
    set_local_address io_xfer
    do_reu_write
    rts

.endproc

; Return response to command in io_xfer+0, with size in X, and status in
; io_xfer+256, with size in Y
.proc get_cmd_response

    lda #CMD_PUSH_CMD
    sta CMD_CONTROL

    ; Wait for completion
    wait:
        lda CMD_STATUS
        and #CMD_STATE_MASK
        cmp #CMD_STATE_BUSY
    beq wait

    ; Read data response
    ldx #0
    get_data:
        lda CMD_STATUS
        and #CMD_DATA_AV
        beq end_data
        lda CMD_RESPONSE_DATA
        sta io_xfer+0,x
    inx
    cpx #255
    bne get_data
    end_data:

    ; Read status response
    ldy #0
    get_status:
        lda CMD_STATUS
        and #CMD_STAT_AV
        beq end_status
        lda CMD_STATUS_DATA
        sta io_xfer+256,y
    iny
    cpy #255
    bne get_status
    end_status:

    ; Acknowledge the response
    lda #CMD_DATA_ACC
    sta CMD_CONTROL

    ; If either response is a string, null-terminate it
    lda #0
    sta io_xfer+0,x
    sta io_xfer+256,y

    rts

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                File system                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.proc init_fs

    .struct boot_record
        BS_jmpBoot      .byte 3  ;  0 (unused)
        BS_OEMName      .byte 8  ;  3 (unused)
        BPB_BytsPerSec  .word 1  ; 11
        BPB_SecPerClus  .byte 1  ; 13
        BPB_RsvdSecCnt  .word 1  ; 14
        BPB_NumFATs     .byte 1  ; 16
        BPB_RootEntCnt  .word 1  ; 17
        BPB_TotSec16    .word 1  ; 19
        BPB_Media       .byte 1  ; 21 (unused)
        BPB_FATSz16     .word 1  ; 22
        BPB_SecPerTrk   .word 1  ; 24 (unused)
        BPB_NumHeads    .word 1  ; 26 (unused)
        BPB_HiddSec     .dword 1 ; 28
        BPB_TotSec32    .dword 1 ; 32
        ; The following are defined only on FAT32
        BPB_FATSz32     .dword 1 ; 36
        BPB_ExtFlags    .word 1  ; 40
        BPB_FSVer       .word 1  ; 42
        BPB_RootClus    .dword 1 ; 44
        BPB_FSInfo      .word 1  ; 48
        BPB_BkBootSec   .word 1  ; 50 (unused)
        BPB_Reserved    .byte 12 ; 52 (unused)
    .endstruct

    ; Read the boot sector from the file system
    lda #0
    sta seek_location+0
    sta seek_location+1
    sta seek_location+2
    sta seek_location+3
    jsr do_seek
    bcs @bad_fs

    lda #128
    jsr read_bytes
    bcs @bad_fs

    ; Partitions are not supported, and so BPB_HiddSec must be zero
    lda io_xfer+boot_record::BPB_HiddSec+0
    ora io_xfer+boot_record::BPB_HiddSec+1
    ora io_xfer+boot_record::BPB_HiddSec+2
    ora io_xfer+boot_record::BPB_HiddSec+3
    bne @bad_fs

    ; Determine fs_sector_shift
    lda io_xfer+boot_record::BPB_BytsPerSec+0
    bne @bad_fs     ; sector size < 256 or not power of two
    lda io_xfer+boot_record::BPB_BytsPerSec+1
    beq @bad_fs     ; sector size is zero
    ldx #0
    @shift1:
        lsr a
        bcs @end_shift1
    inx
    bne @shift1
    @end_shift1:
    cmp #0
    bne @bad_fs     ; sector size is not a power of two
    stx fs_sector_shift

    ; Determine fs_cluster_shift
    lda io_xfer+boot_record::BPB_SecPerClus
    beq @bad_fs     ; cluster size is zero
    ; Leave X alone; this count is cumulative with the last one
    @shift2:
        lsr a
        bcs @end_shift2
    inx
    bne @shift2
    @end_shift2:
    cmp #0
    beq :+          ; cluster size must be a power of two
    @bad_fs:        ; placed here so branches will reach
        lda #0
        sta fs_type
        rts
    :
    stx fs_cluster_shift

    ; Determine fs_cluster_size
    lda io_xfer+boot_record::BPB_SecPerClus
    sta fs_cluster_size+0
    lda #0
    ldx fs_sector_shift
    beq @end_shift2a
    @shift2a:
        asl fs_cluster_size+0
        rol a
    dex
    bne @shift2a
    @end_shift2a:
    sta fs_cluster_size+1

    ; Determine fs_first_fat
    lda io_xfer+boot_record::BPB_RsvdSecCnt+0
    sta fs_first_fat+0
    lda io_xfer+boot_record::BPB_RsvdSecCnt+1
    sta fs_first_fat+1
    lda #0
    ldx fs_sector_shift
    beq @end_shift3
    @shift3:
        asl fs_first_fat+0
        rol fs_first_fat+1
        rol a
    dex
    bne @shift3
    @end_shift3:
    sta fs_first_fat+2

    ; Determine the number of 256-byte blocks per FAT
    ; Stash this in fs_root_dir for the nonce; we'll need it to determine
    ; that position and also fs_second_fat

    lda #0
    sta fs_root_dir+2
    sta fs_root_dir+3
    lda io_xfer+boot_record::BPB_FATSz16+0
    sta fs_root_dir+0
    lda io_xfer+boot_record::BPB_FATSz16+1
    sta fs_root_dir+1
    ora fs_root_dir+0
    bne @got_fat_size
        ; BPB_FATSz16 is zero; use BPB_FATSz32
        ; This is possible only on FAT32
        lda io_xfer+boot_record::BPB_FATSz32+0
        sta fs_root_dir+0
        lda io_xfer+boot_record::BPB_FATSz32+1
        sta fs_root_dir+1
        lda io_xfer+boot_record::BPB_FATSz32+2
        sta fs_root_dir+2
        lda io_xfer+boot_record::BPB_FATSz32+3
        bne @bad_fs         ; Too large (we won't be able to seek)
    @got_fat_size:
    lda fs_root_dir+2
    ldx fs_sector_shift
    beq @end_shift4
    @shift4:
        asl fs_root_dir+0
        rol fs_root_dir+1
        rol a
        bcs @bad_fs_1       ; Too large
    dex
    bne @shift4
    @end_shift4:
    sta fs_root_dir+2

    ; Check the number of FATs and determine fs_second_fat
    lda io_xfer+boot_record::BPB_NumFATs
    cmp #1
    beq @one_fat
    cmp #2
    bne @bad_fs_1
    @two_fats:
        ; Two FATs
        clc
        lda fs_first_fat+0
        adc fs_root_dir+0 ; actually FAT size in 256-byte blocks
        sta fs_second_fat+0
        lda fs_first_fat+1
        adc fs_root_dir+1
        sta fs_second_fat+1
        lda fs_first_fat+2
        adc fs_root_dir+2
        sta fs_second_fat+2
        bcs @bad_fs_1
    jmp @got_fats
    @one_fat:
        ; One FAT
        lda fs_first_fat+0
        sta fs_second_fat+0
        lda fs_first_fat+1
        sta fs_second_fat+1
        lda fs_first_fat+2
        sta fs_second_fat+2
    @got_fats:

    ; Determine location of root directory
    clc
    lda fs_root_dir+0
    adc fs_second_fat+0
    sta fs_root_dir+0
    lda fs_root_dir+1
    adc fs_second_fat+1
    sta fs_root_dir+1
    lda fs_root_dir+2
    adc fs_second_fat+2
    sta fs_root_dir+2
    bcc :+
    @bad_fs_1:
        lda #0
        sta fs_type
        rts
    :

    ; Determine number of entries in root directory
    ; Save a copy to fs_cluster_base for calculation of that parameter
    lda io_xfer+boot_record::BPB_RootEntCnt+0
    sta fs_root_dir_size+0
    sta fs_cluster_base+0
    lda io_xfer+boot_record::BPB_RootEntCnt+1
    sta fs_root_dir_size+1
    sta fs_cluster_base+1

    ; Determine base for cluster area:
    ; Start with the root directory size in bytes
    lda #0
    .repeat 3
        lsr fs_cluster_base+1
        ror fs_cluster_base+0
        ror a
    .endrep
    ; Round up to sector size
    ldx io_xfer+boot_record::BPB_BytsPerSec+1
    dex
    cmp #$01 ; set carry if A != 0
    txa
    adc fs_cluster_base+0
    sta fs_cluster_base+0
    lda #0
    adc fs_cluster_base+1
    sta fs_cluster_base+1
    txa
    eor #$FF
    and fs_cluster_base+0
    ; Add the base of the root directory
    clc
    adc fs_root_dir+0
    sta fs_cluster_base+0
    lda fs_cluster_base+1
    adc fs_root_dir+1
    sta fs_cluster_base+1
    lda #0
    adc fs_root_dir+2
    sta fs_cluster_base+2
    bcs @bad_fs_1

    ; The first cluster is cluster #2. Subtract twice the cluster size from
    ; the cluster base.

    ldx fs_cluster_shift
    sec
    lda fs_cluster_base+0
    sbc cluster_offset_lo,x
    sta fs_cluster_base+0
    lda fs_cluster_base+1
    sbc cluster_offset_hi,x
    sta fs_cluster_base+1
    lda fs_cluster_base+2
    sbc #0
    sta fs_cluster_base+2

    ; Determine the number of clusters
    lda #0
    sta fs_clusters+2
    sta fs_clusters+3
    lda io_xfer+boot_record::BPB_TotSec16+0
    sta fs_clusters+0
    lda io_xfer+boot_record::BPB_TotSec16+1
    sta fs_clusters+1
    ora fs_clusters+0
    bne @got_sectors
        ; 16 bit sector count is zero; use 32 bit sector count
        lda io_xfer+boot_record::BPB_TotSec32+0
        sta fs_clusters+0
        lda io_xfer+boot_record::BPB_TotSec32+1
        sta fs_clusters+1
        lda io_xfer+boot_record::BPB_TotSec32+2
        sta fs_clusters+2
        lda io_xfer+boot_record::BPB_TotSec32+3
        sta fs_clusters+3
    @got_sectors:

    ; Shift sector count to file system size in 256-byte blocks
    lda #0
    ldx fs_sector_shift
    beq @end_shift5
    @shift5:
        asl fs_clusters+0
        rol fs_clusters+1
        rol fs_clusters+2
        rol fs_clusters+3
        rol a
    dex
    bne @shift5
    @end_shift5:
    sta fs_clusters+3

    ; Subtract fs_cluster_base, giving the number of 256-byte blocks available
    ; for data
    sec
    lda fs_clusters+0
    sbc fs_cluster_base+0
    sta fs_clusters+0
    lda fs_clusters+1
    sbc fs_cluster_base+1
    sta fs_clusters+1
    lda fs_clusters+2
    sbc fs_cluster_base+2
    sta fs_clusters+2
    lda fs_clusters+3
    sbc #0

    ; Shift to get the number of clusters
    ldx fs_cluster_shift
    beq @end_shift6
    @shift6:
        lsr a
        ror fs_clusters+2
        ror fs_clusters+1
        ror fs_clusters+0
    dex
    bne @shift6
    @end_shift6:
    sta fs_clusters+3

    ; Set the file system type and FAT32-specific parameters
    lda #0
    sta fs_ext_flags
    sta fs_info_sector+0
    sta fs_info_sector+1
    sta fs_current_dir+0
    sta fs_current_dir+1
    sta fs_current_dir+2
    sta fs_current_dir+3
    lda fs_clusters+3
    ora fs_clusters+2
    bne @fat32          ; must be FAT32 if > 65535
    lda fs_clusters+0
    cmp #$F7
    lda fs_clusters+1
    sbc #$0F
    bcs @fat16
        lda #12
        sta fs_type
        rts
    @fat16:
    lda fs_clusters+0
    cmp #$F7
    lda fs_clusters+1
    sbc #$FF
    bcs @fat32
        lda #16
        sta fs_type
        rts
    @fat32:
        ; Check for version 0
        lda io_xfer+boot_record::BPB_FSVer+0
        ora io_xfer+boot_record::BPB_FSVer+1
        bne @bad_fs_2
            lda #32
            sta fs_type
            lda io_xfer+boot_record::BPB_ExtFlags
            sta fs_ext_flags
            lda io_xfer+boot_record::BPB_FSInfo+0
            sta fs_info_sector+0
            lda io_xfer+boot_record::BPB_FSInfo+1
            sta fs_info_sector+1
            lda io_xfer+boot_record::BPB_RootClus+0
            sta fs_root_dir+0
            sta fs_current_dir+0
            lda io_xfer+boot_record::BPB_RootClus+1
            sta fs_root_dir+1
            sta fs_current_dir+1
            lda io_xfer+boot_record::BPB_RootClus+2
            sta fs_root_dir+2
            sta fs_current_dir+2
            lda io_xfer+boot_record::BPB_RootClus+3
            sta fs_root_dir+3
            sta fs_current_dir+3
            rts
        @bad_fs_2:
            lda #0
            sta fs_type
            rts

cluster_offset_lo:
    .byte <$0002
    .byte <$0004
    .byte <$0008
    .byte <$0010
    .byte <$0020
    .byte <$0040
    .byte <$0080
    .byte <$0100
    .byte <$0200
    .byte <$0400
    .byte <$0800
    .byte <$1000
    .byte <$2000
    .byte <$4000
    .byte <$8000
cluster_offset_hi:
    .byte >$0002
    .byte >$0004
    .byte >$0008
    .byte >$0010
    .byte >$0020
    .byte >$0040
    .byte >$0080
    .byte >$0100
    .byte >$0200
    .byte >$0400
    .byte >$0800
    .byte >$1000
    .byte >$2000
    .byte >$4000
    .byte >$8000

.endproc

; Debug: dump the file system parameters
.if 0
.proc dump_fs_params

    ldx #<title_type
    ldy #>title_type
    jsr print_string
    lda fs_type
    jsr print_byte
    lda #$0D
    jsr CHROUT

    ldx #<title_sector_shift
    ldy #>title_sector_shift
    jsr print_string
    lda fs_sector_shift
    jsr print_byte
    lda #$0D
    jsr CHROUT

    ldx #<title_cluster_shift
    ldy #>title_cluster_shift
    jsr print_string
    lda fs_cluster_shift
    jsr print_byte
    lda #$0D
    jsr CHROUT

    ldx #<title_cluster_size
    ldy #>title_cluster_size
    jsr print_string
    lda fs_cluster_size+1
    jsr print_byte
    lda fs_cluster_size+0
    jsr print_byte
    lda #0
    jsr print_byte
    lda #$0D
    jsr CHROUT

    ldx #<title_clusters
    ldy #>title_clusters
    jsr print_string
    lda fs_clusters+3
    jsr print_byte
    lda fs_clusters+2
    jsr print_byte
    lda fs_clusters+1
    jsr print_byte
    lda fs_clusters+0
    jsr print_byte
    lda #$0D
    jsr CHROUT

    ldx #<title_root_dir_size
    ldy #>title_root_dir_size
    jsr print_string
    lda fs_root_dir_size+1
    jsr print_byte
    lda fs_root_dir_size+0
    jsr print_byte
    lda #$0D
    jsr CHROUT

    ldx #<title_first_fat
    ldy #>title_first_fat
    jsr print_string
    lda fs_first_fat+2
    jsr print_byte
    lda fs_first_fat+1
    jsr print_byte
    lda fs_first_fat+0
    jsr print_byte
    lda #0
    jsr print_byte
    lda #$0D
    jsr CHROUT

    ldx #<title_second_fat
    ldy #>title_second_fat
    jsr print_string
    lda fs_second_fat+2
    jsr print_byte
    lda fs_second_fat+1
    jsr print_byte
    lda fs_second_fat+0
    jsr print_byte
    lda #0
    jsr print_byte
    lda #$0D
    jsr CHROUT

    ldx #<title_root_dir
    ldy #>title_root_dir
    jsr print_string
    lda fs_type
    cmp #32
    beq @fat32
        lda fs_root_dir+2
        jsr print_byte
        lda fs_root_dir+1
        jsr print_byte
        lda fs_root_dir+0
        jsr print_byte
        lda #0
        jsr print_byte
    jmp @end_root_dir
    @fat32:
        lda fs_root_dir+3
        jsr print_byte
        lda fs_root_dir+2
        jsr print_byte
        lda fs_root_dir+1
        jsr print_byte
        lda fs_root_dir+0
        jsr print_byte
    @end_root_dir:
    lda #$0D
    jsr CHROUT

    ldx #<title_cluster_base
    ldy #>title_cluster_base
    jsr print_string
    lda fs_cluster_base+2
    jsr print_byte
    lda fs_cluster_base+1
    jsr print_byte
    lda fs_cluster_base+0
    jsr print_byte
    lda #0
    jsr print_byte
    lda #$0D
    jsr CHROUT

    ldx #<title_ext_flags
    ldy #>title_ext_flags
    jsr print_string
    lda fs_ext_flags
    jsr print_byte
    lda #$0D
    jsr CHROUT

    ldx #<title_info_sector
    ldy #>title_info_sector
    jsr print_string
    lda fs_info_sector+1
    jsr print_byte
    lda fs_info_sector+0
    jsr print_byte
    lda #$0D
    jsr CHROUT

    rts

title_type: .asciiz "fs_type="
title_sector_shift: .asciiz "fs_sector_shift="
title_cluster_shift: .asciiz "fs_cluster_shift="
title_cluster_size: .asciiz "fs_cluster_size="
title_clusters: .asciiz "fs_clusters="
title_root_dir_size: .asciiz "fs_root_dir_size="
title_first_fat: .asciiz "fs_first_fat="
title_second_fat: .asciiz "fs_second_fat="
title_root_dir: .asciiz "fs_root_dir="
title_cluster_base: .asciiz "fs_cluster_base="
title_ext_flags: .asciiz "fs_ext_flags="
title_info_sector: .asciiz "fs_info_sector="

.endproc
.endif

; Retrieve a path from the REU.
; io_addr contains the address. The length is not known in advance, but does
; not exceed filename_max+1 (else the path is invalid). The path is read into
; fs_path.
; Return C clear if OK. If error, C is set and -errno is in A.
; Possible errno values are ENAMETOOLONG and EFAULT.

.proc read_path

    ; Set size to filename_max+1
    lda #<(filename_max+1)
    sta io_size+0
    lda #>(filename_max+1)
    sta io_size+1

    ; If that would cross a 64K boundary, set size up to that boundary
    clc
    lda io_addr+0
    adc #<(filename_max+1)
    lda io_addr+1
    adc #>(filename_max+1)
    bcc @size_ok
        ; Get size up to the next 64K boundary
        sec
        lda #0
        sbc io_addr+0
        sta io_size+0
        lda #0
        sbc io_addr+1
        sta io_size+1
    @size_ok:

    ; Read from REU
    set_reu_address io_addr
    set_local_address fs_path
    set_xfer_size io_size
    do_reu_read

    ; If the path ends within the transfer size, we're done
    jsr path_ends
    bcs @read_next
        rts
    @read_next:

    ; Get the size for the second part of the read
    sec
    lda #<(filename_max+1)
    sbc io_size+0
    sta io_size+0
    lda #>(filename_max+1)
    sbc io_size+1
    sta io_size+1
    ora io_size+0
    bne @more_to_read
        ; Already read filename_max+1 bytes
        lda #$100-ENAMETOOLONG
        sec
        rts
    @more_to_read:
    ; Set the address for the next part
    ldx io_addr+2
    inx
    bne @addr_ok
        ; End of REU exceeded
        lda #$100-EFAULT
        sec
        rts
    @addr_ok:
    stx reu_xmem_address_2
    set_xfer_size io_size
    do_reu_read

    ; Set io_size to the full size of the transfer
    lda #<(filename_max+1)
    sta io_size+0
    lda #>(filename_max+1)
    sta io_size+1

    ; Check that the path ends within the transfer
    jsr path_ends
    bcc @path_ok
        lda #$100-ENAMETOOLONG
    @path_ok:
    rts

.endproc

; We have read a path, or part of a path, into fs_path
; The size read is io_size
; Check whether the path ends within the transfer
; Return C clear if end of path (a zero byte) is found; C set if not
.proc path_ends

    lda #0
    sta local_addr+0
    ldy #<fs_path
    lda #>fs_path
    sta local_addr+1    ; local_addr+1 and Y form the address
    lda io_size+1       ; Preserve io_size for two-part REU transfer
    pha

    ; Fail if io_size == 0
    ora io_size+0
    beq @fail

    ; Set io_size+1:X to -io_size
    ; We'll increment this and stop when we reach 0
    sec
    lda #0
    sbc io_size+0
    tax
    lda #0
    sbc io_size+1
    sta io_size+1

    @scan:
        lda (local_addr),y      ; Pass if a zero byte is at the address
        beq @pass
        iny                     ; Add one to the address
        bne :+
            inc local_addr+1
        :
    inx                         ; Subtract one from the count
    bne @scan
    inc io_size+1
    bne @scan
    ; Check fails if loop runs to completion

    @fail:
    sec
    pla
    sta io_size+1
    rts

    @pass:
    clc
    pla
    sta io_size+1
    rts

.endproc

; Retrieve current path component and convert to 8.3 format
; Return C clear if path component retrieved
; Return C set and A == 0 if end of path reached
; Return C set and A == -errno if invalid component found
.proc get_path_part

    ldy path_ptr+0
    lda #0
    sta path_ptr+0

    ; Skip path separators
    skip_separators:
        lda (path_ptr),y
        bne check_sep
            ; End of path
            sty path_ptr+0
            lda #0
            sec
            rts
        check_sep:
        cmp #$2F
        beq next_sep
        cmp #$5C
        bne start_path_part
    next_sep:
    iny
    bne skip_separators
    inc path_ptr+1
    bne skip_separators
    start_path_part:
    sty path_ptr+0

    ; Check for "." and ".." entries
    ldy #0
    lda (path_ptr),y
    cmp #$2E
    bne normal_name
        ; '.' found
        sta path_part+0
        iny     ; Y == 1
        lda (path_ptr),y
        beq dot_name
        cmp #$2E
        beq double_dot
        cmp #$2F
        beq dot_name
        cmp #$5C
        beq dot_name
        jmp bad_name
    double_dot:
        ; '..' found
        sta path_part+1
        iny     ; Y == 2
        lda (path_ptr),y
        beq dot_name
        cmp #$2F
        beq dot_name
        cmp #$5C
        beq dot_name
        jmp bad_name
    dot_name:
        ; "." or ".." in path_part; Y == 1 or 2
        lda #0
        sta path_part,y
        tya
        clc
        adc path_ptr+0
        sta path_ptr+0
        lda #0
        adc path_ptr+1
        sta path_ptr+1
        clc
        rts
    normal_name:

    ; Fill path_part with spaces
    ldx #10
    lda #$20
    fill_loop:
        sta path_part,x
    dex
    bpl fill_loop

    ; Get the base part of the name
    ldy path_ptr+0
    lda #0
    sta path_ptr+0
    ldx #0  ; fs_path offset
    name_loop:
        lda (path_ptr),y
        beq end_name
        cmp #$2F
        beq end_name
        cmp #$5C
        beq end_name
        cmp #$2E
        beq get_ext
        sta path_part,x
    iny
    bne :+
        inc path_ptr+1
    :
    inx
    cpx #8
    bcc name_loop

    ; Skip any characters up to '.' or end of component
    skip_name_loop:
        lda (path_ptr),y
        beq end_name
        cmp #$2F
        beq end_name
        cmp #$5C
        beq end_name
        cmp #$2E
        beq get_ext
        tax
        lda name_cvt,x
        beq bad_name
    iny
    bne skip_name_loop
    inc path_ptr+1
    bne skip_name_loop

    ; If a '.' was found, skip it and read the extension
    get_ext:
    iny
    bne :+
        inc path_ptr+1
    :
    ldx #8
    ext_loop:
        lda (path_ptr),y
        beq end_name
        cmp #$2F
        beq end_name
        cmp #$5C
        beq end_name
        sta path_part,x
    iny
    bne :+
        inc path_ptr+1
    :
    inx
    cpx #11
    bcc ext_loop

    ; Skip any characters up to end of component
    skip_ext_loop:
        lda (path_ptr),y
        beq end_name
        cmp #$2F
        beq end_name
        cmp #$5C
        beq end_name
        tax
        lda name_cvt,x
        beq bad_name
    iny
    bne skip_ext_loop
    inc path_ptr+1
    bne skip_ext_loop

end_name:
    sty path_ptr+0

    ; Convert the path component to match its occurrence on the volume
    ldx #10
    conv_loop:
        ldy path_part,x
        lda name_cvt,y
        beq bad_name
        sta path_part,x
    dex
    bpl conv_loop

    lda #0
    clc
    rts

bad_name:
    lda #$100-EINVAL
    sec
    rts

.endproc

; Determine whether path_ptr points to the end of the path
; Return C set if it does not
.proc is_last_component

    ; Look for next byte that is not '/' or '\'
    ldy path_ptr+0
    lda #0
    sta path_ptr+0
    scan:
        lda (path_ptr),y
        cmp #$2F
        beq @next
        cmp #$5C
        bne end_scan
    @next:
    iny
    bne scan
    inc path_ptr+1
    bne scan
    end_scan:
    sty path_ptr+0

    ; Set C if that byte is not zero
    cmp #$01
    rts

.endproc

; With file_data+filedata::dir_start and file_data+filedata::attributes set up,
; search directory for name matching path_part.
; If found, return C clear, entry in file_data, and dir_entry and lfn_entry set.
; If not found, return C set and -ENOENT in A.
; If I/O error, return C set and another errno in A.
.proc find_entry

    ; Must be seeking in a directory
    lda file_data+filedata::attributes
    and #ATTR_DIRECTORY
    bne dir_ok
        lda #$100-ENOTDIR
        sec
        rts
    dir_ok:

    ; This will record the location of an empty directory entry if we find one
    lda #0
    sta new_dir_entry+0
    sta new_dir_entry+1
    sta new_dir_entry+2
    sta new_dir_entry+3
    ; This marks the last cluster that was followed; if it is 0, the directory
    ; is empty
    sta last_cluster+0
    sta last_cluster+1
    sta last_cluster+2

    ; Determine seek location for the enclosing directory
    lda #0
    sta file_data+filedata::dir_entry+0
    sta file_data+filedata::lfn_entry+0
    lda file_data+filedata::dir_start+0
    ora file_data+filedata::dir_start+1
    ora file_data+filedata::dir_start+2
    ora file_data+filedata::dir_start+3
    bne dir_is_cluster_chain

        ; Enclosing directory is the root directory on a FAT12 or FAT16 volume.
        ; This directory is contiguous, and appears before the cluster area.
        lda fs_root_dir+0
        sta file_data+filedata::dir_entry+1
        sta file_data+filedata::lfn_entry+1
        lda fs_root_dir+1
        sta file_data+filedata::dir_entry+2
        sta file_data+filedata::lfn_entry+2
        lda fs_root_dir+2
        sta file_data+filedata::dir_entry+3
        sta file_data+filedata::lfn_entry+3
        ; Size to search is the entire directory
        ; Set as negative to simplify the loop
        sec
        lda #0
        sbc fs_root_dir_size+0
        sta dir_size+0
        lda #0
        sbc fs_root_dir_size+1
        sta dir_size+1

        ; Seek to start of root directory area
        lda file_data+filedata::dir_entry+0
        sta seek_location+0
        lda file_data+filedata::dir_entry+1
        sta seek_location+1
        lda file_data+filedata::dir_entry+2
        sta seek_location+2
        lda file_data+filedata::dir_entry+3
        sta seek_location+3
        jsr do_seek
        bcs @io_error

        @search_dir:
            ; Read one directory entry
            jsr read_dir_entry
            bcs @error
            beq @dir_match

        inc dir_size+0
        bne @search_dir
        inc dir_size+1
        bne @search_dir

        ; Return ENOENT
        lda #$100-ENOENT
        sec
        rts

    @io_error:
        ; Return EIO
        lda #$100-EIO
    @error:
        sec
        rts

    @dir_match:
        clc
        rts

    dir_is_cluster_chain:

        ; Enclosing directory is a cluster chain (root directory on FAT32, or
        ; any other directory on any volume).
        ; Set the starting cluster
        lda file_data+filedata::dir_start+0
        sta cluster+0
        lda file_data+filedata::dir_start+1
        sta cluster+1
        lda file_data+filedata::dir_start+2
        sta cluster+2

        ; If this is zero, the directory is empty
        ora cluster+0
        ora cluster+1
        bne @search_cluster
            lda #$100-ENOENT
            rts

        @search_cluster:
            ; Convert cluster number to byte position
            lda #0
            sta file_data+filedata::dir_entry+0
            lda cluster+0
            sta file_data+filedata::dir_entry+1
            lda cluster+1
            sta file_data+filedata::dir_entry+2
            lda cluster+2
            sta file_data+filedata::dir_entry+3
            ldx fs_cluster_shift
            beq @end_shift
            @shift:
                asl file_data+filedata::dir_entry+1
                rol file_data+filedata::dir_entry+2
                rol a
            dex
            bne @shift
            @end_shift:
            sta file_data+filedata::dir_entry+3
            ; Add the cluster base
            clc
            lda file_data+filedata::dir_entry+1
            adc fs_cluster_base+0
            sta file_data+filedata::dir_entry+1
            lda file_data+filedata::dir_entry+2
            adc fs_cluster_base+1
            sta file_data+filedata::dir_entry+2
            lda file_data+filedata::dir_entry+3
            adc fs_cluster_base+2
            sta file_data+filedata::dir_entry+3

            ; Size to search is one cluster
            ; fs_cluster_size is in 256 byte units; shift left by 3 to get count
            ; of 32-byte directory entries
            lda fs_cluster_size+0
            sta dir_size+0
            lda fs_cluster_size+1
            sta dir_size+1
            lda #0
            .repeat 3
                asl dir_size+0
                rol dir_size+1
                rol a
            .endrep
            sta dir_size+2
            ; Make it negative, to simplify the loop
            sec
            lda #0
            sbc dir_size+0
            sta dir_size+0
            lda #0
            sbc dir_size+1
            sta dir_size+1
            lda #0
            sbc dir_size+2
            sta dir_size+2

            ; If the last entry was not an LFN record, set the lfn_entry pointer
            lda file_data+filedata::attributes
            and #$3F
            cmp #$0F
            beq @lfn_1
                lda file_data+filedata::dir_entry+0
                sta file_data+filedata::lfn_entry+0
                lda file_data+filedata::dir_entry+1
                sta file_data+filedata::lfn_entry+1
                lda file_data+filedata::dir_entry+2
                sta file_data+filedata::lfn_entry+2
                lda file_data+filedata::dir_entry+3
                sta file_data+filedata::lfn_entry+3
            @lfn_1:

            ; Seek to start of cluster
            lda file_data+filedata::dir_entry+0
            sta seek_location+0
            lda file_data+filedata::dir_entry+1
            sta seek_location+1
            lda file_data+filedata::dir_entry+2
            sta seek_location+2
            lda file_data+filedata::dir_entry+3
            sta seek_location+3
            jsr do_seek
            bcs @io_error

            ; Search entries in the current cluster
            @search_dir:
                ; Read one directory entry
                jsr read_dir_entry
                bcs @error
                beq @dir_match

            inc dir_size+0
            bne @search_dir
            inc dir_size+1
            bne @search_dir
            inc dir_size+2
            bne @search_dir

            ; Get the next cluster

            lda cluster+0
            sta last_cluster+0
            lda cluster+1
            sta last_cluster+1
            lda cluster+2
            sta last_cluster+2
            jsr next_cluster
            jcc @search_cluster
            sec
            rts

        @io_error:
            lda #$100-EIO
        @error:
            sec
            rts

        @dir_match:
            clc
            rts

.endproc

; Read a directory entry and compare to the target name in path_part
; If match: C clear, A == 0, Z set
; If no match: C clear, A != 0, Z clear
; If error: C set, error in A
; new_dir_entry set if it isn't already and if the entry is empty
.proc read_dir_entry

    ; Read 32 bytes
    lda #32
    jsr read_bytes
    bcc io_ok
        lda #$100-EIO
        sec
        rts
    io_ok:

    ; Check for empty entry
    lda io_xfer+0
    bne not_end
        ; First byte is zero; this marks the end of the directory
        jsr set_new_dir_entry
        lda #$100-ENOENT
        sec
        rts
    not_end:
    cmp #$E5
    bne not_empty
        ; Deleted directory entry; never match, but continue searching
        jsr set_new_dir_entry
        jmp no_match
    not_empty:

    ; Is this an LFN entry?
    lda io_xfer+filedata::attributes
    and #$3F
    cmp #$0F
    bne not_lfn
        ; LFN entry. Never match on this.
        ; Set attributes byte to indicate LFN entry
        sta file_data+filedata::attributes
        ; Advance dir_entry but not lfn_entry
        clc
        lda file_data+filedata::dir_entry+0
        adc #32
        sta file_data+filedata::dir_entry+0
        bcc :+
        inc file_data+filedata::dir_entry+1
        bne :+
        inc file_data+filedata::dir_entry+2
        bne :+
        inc file_data+filedata::dir_entry+3
        :
        lda #$FF
        clc
        rts
    not_lfn:

    ; This is a matchable directory entry
    ; Compare to path_part
    ldx #10
    compare:
        lda io_xfer,x
        cmp path_part,x
        bne no_match
    dex
    bpl compare

    ; The entry matches. Copy to file_data
    ldx #31
    copy:
        lda io_xfer,x
        sta file_data,x
    dex
    bpl copy

    lda #0
    clc
    rts

    ; Come here if deleted directory entry or if name does not match
no_match:
    lda #0
    sta file_data+filedata::attributes ; not LFN entry and not directory
    clc
    lda file_data+filedata::dir_entry+0
    adc #32
    sta file_data+filedata::dir_entry+0
    bcc :+
    inc file_data+filedata::dir_entry+1
    bne :+
    inc file_data+filedata::dir_entry+2
    bne :+
    inc file_data+filedata::dir_entry+3
    :
    lda file_data+filedata::dir_entry+0
    sta file_data+filedata::lfn_entry+0
    lda file_data+filedata::dir_entry+1
    sta file_data+filedata::lfn_entry+1
    lda file_data+filedata::dir_entry+2
    sta file_data+filedata::lfn_entry+2
    lda file_data+filedata::dir_entry+3
    sta file_data+filedata::lfn_entry+3
    lda #$FF
    clc
    rts

.endproc

; Set new_dir_entry to the current directory entry, if it isn't already set
.proc set_new_dir_entry

    ; If new_dir_entry isn't zero, we already have an empty directory entry
    lda new_dir_entry+0
    ora new_dir_entry+1
    ora new_dir_entry+2
    ora new_dir_entry+3
    bne already_set

        lda file_data+filedata::dir_entry+0
        sta new_dir_entry+0
        lda file_data+filedata::dir_entry+1
        sta new_dir_entry+1
        lda file_data+filedata::dir_entry+2
        sta new_dir_entry+2
        lda file_data+filedata::dir_entry+3
        sta new_dir_entry+3

    already_set:
    rts

.endproc

; Given the current cluster number in "cluster", find the next cluster in
; the chain.
; If new cluster found: Return C clear.
; If end of chain: Return C set and ENOENT in A.
; If I/O error: Return C set and other error in A.
.proc next_cluster

    lda cluster+0
    sta cluster_num+0
    lda cluster+1
    sta cluster_num+1
    lda cluster+2
    sta cluster_num+2
    lda cluster+3
    sta cluster_num+3
    jsr read_fat_entry
    bcc :+
        rts
    :

    ; New cluster must be at least two
    lda fat_entry+0
    cmp #2
    lda fat_entry+1
    sbc #0
    lda fat_entry+2
    sbc #0
    lda fat_entry+3
    sbc #0
    bcc bad_cluster

    ; New cluster must be within the file system
    lda fat_entry+0
    cmp fs_clusters+0
    lda fat_entry+1
    sbc fs_clusters+1
    lda fat_entry+2
    sbc fs_clusters+2
    lda fat_entry+3
    and #$0F
    sbc fs_clusters+3
    bcs bad_cluster

    ; New cluster is found
    lda fat_entry+0
    sta cluster+0
    lda fat_entry+1
    sta cluster+1
    lda fat_entry+2
    sta cluster+2
    lda fat_entry+3
    and #$0F
    sta cluster+3
    clc
    rts

bad_cluster:
    lda #$100-ENOENT
    sec
    rts

.endproc

; Given the starting cluster number in "cluster", free that cluster and all
; others after it.
; Return C clear on success.
; On error, set C and return -errno in A.
.proc delete_cluster_chain

    ; Repeat until read_fat_entry returns an error
    delete_loop:
        ; Read the FAT entry
        lda cluster+0
        sta cluster_num+0
        lda cluster+1
        sta cluster_num+1
        lda cluster+2
        sta cluster_num+2
        lda cluster+3
        sta cluster_num+3
        jsr read_fat_entry
        bcs end_delete

        ; Copy to "cluster" for the next pass
        lda fat_entry+0
        sta cluster+0
        lda fat_entry+1
        sta cluster+1
        lda fat_entry+2
        sta cluster+2
        lda fat_entry+3
        and #$0F
        sta cluster+3

        ; Mark the current entry as free
        lda #0
        sta fat_entry+0
        sta fat_entry+1
        sta fat_entry+2
        sta fat_entry+3
        jsr write_fat_entry
    jcc delete_loop

    end_delete:
    ; ENOENT just means we reached the end of the chain
    cmp #$100-ENOENT
    bne real_error
        clc
        rts
    real_error:
        sec
        rts

.endproc

; Write the directory entry from the block at (local_addr)
; Return C clear if OK
; Return C set if error, and -errno in A
.proc write_dir_entry

    ; Update the modification time
    jsr set_mtime

    ; Seek to the location of the directory entry
    ldy #filedata::dir_entry
    lda (local_addr),y
    sta seek_location+0
    iny
    lda (local_addr),y
    sta seek_location+1
    iny
    lda (local_addr),y
    sta seek_location+2
    iny
    lda (local_addr),y
    sta seek_location+3
    jsr do_seek
    bcs error

    ; Do the write command
    jsr begin_write
    ldy #0
    @copy:
        lda (local_addr),y
        sta CMD_DATA
    iny
    cpy #32
    bcc @copy

    jsr end_write
    bcs error

    clc
    rts

error:
    lda #$100-EIO
    sec
    rts

.endproc

; Set the modification time for the directory entry at (local_addr)
; Never returns error
; Return A=100 if second is odd, else A=0; this can go in ctime_lo if we're
; creating the file
.proc set_mtime

    ; Query the current time
    jsr get_time

    ; get_time always succeeds
    ; Response is yyyy/mm/dd hh:mm:ss
    ; Convert the components to integer form

    lda io_xfer+0 ; 1000s of year
    and #$0F
    tax
    lda thousand_lo,x
    sta io_xfer+32

    lda io_xfer+1 ; 100s of year
    and #$0F
    tax
    clc
    lda io_xfer+32
    adc hundred_lo,x
    sta io_xfer+32

    lda io_xfer+2 ; 10s of year
    and #$0F
    tax
    lda io_xfer+3 ; 1s of year
    and #$0F
    clc
    adc ten,x
    ; the add never carries
    adc io_xfer+32

    ; io_xfer+32 == year % 256

    ; Subtract 1980 from year to get years from epoch
    sec
    sbc #<1980
    sta io_xfer+32

    ; io_xfer+32 = year - 1980

    ; Convert day to integer

    lda io_xfer+8   ; 10s of day
    and #$0F
    tax
    lda io_xfer+9   ; 1s of day
    and #$0F
    clc
    adc ten,x
    sta io_xfer+33

    ; io_xfer+33 = day

    ; Convert month to integer

    lda io_xfer+5   ; 10s of month
    and #$0F
    tax
    lda io_xfer+6   ; 1s of month
    and #$0F
    clc
    adc ten,x

    ; Form the low byte of mdate
    asl a
    asl a
    asl a
    asl a
    asl a   ; may carry
    ora io_xfer+33
    ldy #filedata::mdate+0
    sta (local_addr),y

    ; Form the high byte of mdate
    lda io_xfer+32
    rol a   ; catch the high bit of the month
    iny
    sta (local_addr),y

    ; Convert hour to integer

    lda io_xfer+11  ; 10s of hour
    and #$0F
    tax
    lda io_xfer+12  ; 1s of hour
    and #$0F
    clc
    adc ten,x
    sta io_xfer+32

    ; Convert second to integer

    lda io_xfer+17  ; 10s of second
    and #$0F
    tax
    lda io_xfer+18  ; 1s of second
    and #$0F
    clc
    adc ten,x
    lsr a           ; Two-second granularity
    sta io_xfer+33

    ; Convert minute to integer

    lda io_xfer+14  ; 10s of minute
    and #$0F
    tax
    lda io_xfer+15  ; 1s of minute
    and #$0F
    clc
    adc ten,x

    ; Form the low byte of mtime
    asl a
    asl a
    asl a
    rol io_xfer+32
    asl a
    rol io_xfer+32
    asl a ; may carry
    ora io_xfer+33
    ldy #filedata::mtime+0
    sta (local_addr),y

    ; Form the high byte of mtime
    lda io_xfer+32
    rol a ; Catch carry from above
    iny
    sta (local_addr),y

    ; Preserve the odd second for creation time
    lda io_xfer+18  ; 1s of second
    lsr a
    lda #0
    bcc :+
       lda #100
    :

    rts

.endproc

; Allocate a new cluster
; Find the first cluster after "cluster", if one exists
.proc allocate_cluster

    ; Scan the FAT twice: the first time from "cluster" if it is valid, and
    ; the second time from the beginning

    ; Is the starting cluster valid?
    lda cluster+0
    cmp #2
    lda cluster+1
    sbc #0
    lda cluster+2
    sbc #0
    lda cluster+3
    sbc #0
    bcc one_pass
    lda cluster+0
    cmp fs_clusters+0
    lda cluster+1
    sbc fs_clusters+1
    lda cluster+2
    sbc fs_clusters+2
    lda cluster+3
    sbc fs_clusters+3
    bcs one_pass

        ; Run two passes, with the first starting from "cluster"
        lda cluster+0
        sta cluster_num+0
        lda cluster+1
        sta cluster_num+1
        lda cluster+2
        sta cluster_num+2
        lda cluster+3
        sta cluster_num+3
        lda #2
        sta cluster_scans
        bne pass_loop

    one_pass:

        ; Run only one pass over the entire FAT
        lda #2
        sta cluster_num+0
        lda #0
        sta cluster_num+1
        sta cluster_num+2
        sta cluster_num+3
        lda #1
        sta cluster_scans

    pass_loop:

        cluster_loop:
            ; Read the current FAT entry
            jsr read_fat_entry
            bcs read_error  ; Error, or end of FAT
            lda fat_entry+3
            and #$0F
            ora fat_entry+0
            ora fat_entry+1
            ora fat_entry+2
            beq got_cluster ; Free entry found
        inc cluster_num+0
        bne cluster_loop
        inc cluster_num+1
        bne cluster_loop
        inc cluster_num+2
        bne cluster_loop
        inc cluster_num+3
        bne cluster_loop

    read_error:
        ; Return error if not ENOENT
        cmp #$100-ENOENT
        beq next_pass
            sec
            rts
    next_pass:
    dec cluster_scans
    jeq no_space
    lda #2
    sta cluster_num+0
    lda #0
    sta cluster_num+1
    sta cluster_num+2
    sta cluster_num+3
    beq pass_loop

got_cluster:
    ; cluster_num is the cluster to be newly allocated
    ; Mark that cluster as allocated
    lda #$F8        ; EOF marker = $FFFFFF8
    sta fat_entry+0
    lda #$FF
    sta fat_entry+1
    sta fat_entry+2
    lda #$0F
    sta fat_entry+3
    jsr write_fat_entry
    bcs error

    ; Link to the newly allocated cluster
    ; If "cluster" is 0, write to the file data block at (local_addr);
    ; otherwise, link from "cluster"
    lda cluster+0
    ora cluster+1
    ora cluster+2
    ora cluster+3
    beq link_from_dir
        ; Link from the FAT
        lda cluster_num+0
        sta fat_entry+0
        lda cluster_num+1
        sta fat_entry+1
        lda cluster_num+2
        sta fat_entry+2
        lda cluster_num+3
        sta fat_entry+3
        lda cluster+0
        sta cluster_num+0
        lda cluster+1
        sta cluster_num+1
        lda cluster+2
        sta cluster_num+2
        lda cluster+3
        sta cluster_num+3
        jsr write_fat_entry
        rts
    link_from_dir:
        ; Link from a directory entry
        ldy #filedata::cluster_lo+0
        lda fat_entry+0
        sta (local_addr),y
        iny
        lda fat_entry+1
        sta (local_addr),y
        ldy #filedata::cluster_hi+0
        lda fat_entry+2
        sta (local_addr),y
        iny
        lda fat_entry+3
        sta (local_addr),y
        clc
        rts

no_space:
    ; No free cluster was found
    lda #$100-ENOSPC
error:
    sec
    rts

.endproc

.bss

; Count of passes through allocate_cluster
cluster_scans: .res 1

; Input and output for read_fat_entry and write_fat_entry
cluster_num: .res 4
fat_entry: .res 4

.code

; Given the cluster number in cluster_num, read the FAT entry to fat_entry.
; If the FAT entry is read: return C clear and the data in fat_entry.
; If cluster_num is out of range: Return C set and ENOENT in A.
; If I/O error: Return C set and other error in A.
.proc read_fat_entry

    ; Check validity of cluster_num
    lda cluster_num+0
    cmp #2
    lda cluster_num+1
    sbc #0
    lda cluster_num+2
    sbc #0
    lda cluster_num+3
    sbc #0
    bcc bad_cluster

    lda cluster_num+0
    cmp fs_clusters+0
    lda cluster_num+1
    sbc fs_clusters+1
    lda cluster_num+2
    sbc fs_clusters+2
    lda cluster_num+3
    sbc fs_clusters+3
    bcc good_cluster

bad_cluster:
    lda #$100-ENOENT
    sec
    rts

good_cluster:

    ; Follow according to the file system type
    lda fs_type
    cmp #12
    beq fat_12
    cmp #16
    jeq fat_16
    cmp #32
    jeq fat_32
io_error:
    lda #$100-EIO
    sec
    rts

fat_12:
    ; cluster_num + (cluster_num >> 1)
    lda cluster_num+1
    lsr a
    sta seek_location+1
    lda cluster_num+0
    ror a
    sta seek_location+0
    clc
    lda cluster_num+0
    adc seek_location+0
    sta seek_location+0
    lda cluster_num+1
    adc seek_location+1
    sta seek_location+1
    lda #0
    sta seek_location+2
    sta seek_location+3

    ; Seek to location and read two bytes
    jsr add_first_fat
    jsr do_seek
    bcs io_error

    lda #2
    jsr read_bytes
    bcs io_error

    ; Select entry according to bit 0 of cluster number
    lda #0
    sta fat_entry+2
    lda cluster_num+0
    lsr a
    lda io_xfer+0
    sta fat_entry+0
    lda io_xfer+1
    bcs odd_cluster
        ; Even cluster number
        and #$0F
        sta fat_entry+1
        lda #0
        sta fat_entry+2
        sta fat_entry+3
        clc
        rts
    odd_cluster:
        ; Odd cluster number
        .repeat 4
            lsr a
            ror fat_entry+0
        .endrep
        sta fat_entry+1
        lda #0
        sta fat_entry+2
        sta fat_entry+3
        clc
        rts

fat_16:
    ; cluster_num << 1
    lda cluster_num+0
    asl a
    sta seek_location+0
    lda cluster_num+1
    rol a
    sta seek_location+1
    lda #0
    sta seek_location+3
    rol a
    sta seek_location+2

    ; Seek the location of the entry in the first FAT
    jsr add_first_fat
    jsr do_seek
    bcs @io_error

    ; Read two bytes
    lda #2
    jsr read_bytes
    bcs @io_error

    ; Return the FAT entry
    lda io_xfer+0
    sta fat_entry+0
    lda io_xfer+1
    sta fat_entry+1
    lda #0
    sta fat_entry+2
    sta fat_entry+3
    clc
    rts

@io_error:
    lda #$100-EIO
    sec
    rts

fat_32:
    ; cluster_num << 2
    lda cluster_num+0
    asl a
    sta seek_location+0
    lda cluster_num+1
    rol a
    sta seek_location+1
    lda cluster_num+2
    rol a
    sta seek_location+2
    lda cluster_num+3
    rol a
    asl seek_location+0
    rol seek_location+1
    rol seek_location+2
    rol a
    sta seek_location+3

    ; Add fs_first_fat or fs_second_fat
    lda fs_ext_flags
    cmp #$81
    bcs @second_fat
        jsr add_first_fat
        jmp @seek
    @second_fat:
        jsr add_second_fat
    @seek:

    ; Read from that position
    jsr do_seek
    bcs @io_error

    lda #4
    jsr read_bytes
    bcs @io_error

    ; Return the FAT entry
    lda io_xfer+0
    sta fat_entry+0
    lda io_xfer+1
    sta fat_entry+1
    lda io_xfer+2
    sta fat_entry+2
    lda io_xfer+3
    sta fat_entry+3
    rts

@io_error:
    lda #$100-EIO
    sec
    rts

.endproc

; Given the cluster number in cluster_num, write fat_entry to its FAT entry.
; If the FAT entry is ritten: return C clear.
; If cluster_num is out of range: Return C set and ENOENT in A.
; If I/O error: Return C set and other error in A.
.proc write_fat_entry

    ; Check validity of cluster_num
    lda cluster_num+0
    cmp #2
    lda cluster_num+1
    sbc #0
    lda cluster_num+2
    sbc #0
    lda cluster_num+3
    sbc #0
    bcc bad_cluster

    lda cluster_num+0
    cmp fs_clusters+0
    lda cluster_num+1
    sbc fs_clusters+1
    lda cluster_num+2
    sbc fs_clusters+2
    lda cluster_num+3
    sbc fs_clusters+3
    bcc good_cluster

bad_cluster:
    lda #$100-ENOENT
    sec
    rts

good_cluster:

    ; Follow according to the file system type
    lda fs_type
    cmp #12
    beq fat_12
    cmp #16
    jeq fat_16
    cmp #32
    jeq fat_32
    lda #$100-EIO
    sec
    rts

fat_12:
    ; cluster_num + (cluster_num >> 1)
    lda cluster_num+1
    lsr a
    sta seek_location+1
    lda cluster_num+0
    ror a
    sta seek_location+0
    clc
    lda cluster_num+0
    adc seek_location+0
    sta seek_location+0
    lda cluster_num+1
    adc seek_location+1
    sta seek_location+1
    lda #0
    sta seek_location+2
    sta seek_location+3

    ; Seek to location and read two bytes
    jsr add_first_fat
    jsr do_seek
    bcs @io_error

    lda #2
    jsr read_bytes
    bcs @io_error

    ; Update entry according to bit 0 of cluster number
    lda cluster_num+0
    lsr a
    bcs @odd_cluster
        ; Even cluster number
        lda fat_entry+0
        sta fat_temp+0
        lda fat_entry+1
        eor io_xfer+1
        and #$0F
        eor io_xfer+1
        sta fat_temp+1
        jmp @write
    @odd_cluster:
        ; Odd cluster number
        lda fat_entry+1
        sta fat_temp+1
        lda fat_entry+0
        .repeat 4
            asl a
            rol fat_temp+1
        .endrep
        eor io_xfer+0
        and #$0F
        eor io_xfer+0
        sta fat_temp+0

    @write:
    ; Seek back to the just-read location
    jsr do_seek
    bcs @io_error

    ; Write the FAT entry
    jsr begin_write
    lda fat_temp+0
    sta CMD_DATA
    lda fat_temp+1
    sta CMD_DATA
    jsr end_write
    bcs @io_error

    ; Seek to the second FAT
    jsr add_fat_delta
    jsr do_seek
    bcs @io_error

    ; Write the FAT entry
    jsr begin_write
    lda fat_temp+0
    sta CMD_DATA
    lda fat_temp+1
    sta CMD_DATA
    jsr end_write
    bcs @io_error

    ; C already clear
    rts

@io_error:
    lda #$100-EIO
    sec
    rts

fat_16:
    ; cluster_num << 1
    lda cluster_num+0
    asl a
    sta seek_location+0
    lda cluster_num+1
    rol a
    sta seek_location+1
    lda #0
    sta seek_location+3
    rol a
    sta seek_location+2

    ; Seek the location of the entry in the first FAT
    jsr add_first_fat
    jsr do_seek
    bcs @io_error

    ; Write two bytes
    jsr begin_write
    lda fat_entry+0
    sta CMD_DATA
    lda fat_entry+1
    sta CMD_DATA
    jsr end_write
    bcs @io_error

    ; Again for the second FAT
    jsr add_fat_delta
    jsr do_seek
    bcs @io_error

    ; Write two bytes
    jsr begin_write
    lda fat_entry+0
    sta CMD_DATA
    lda fat_entry+1
    sta CMD_DATA
    jsr end_write
    bcs @io_error

    ; C already clear
    rts

@io_error:
    lda #$100-EIO
    sec
    rts

fat_32:
    ; cluster_num << 2
    lda cluster_num+0
    asl a
    sta seek_location+0
    lda cluster_num+1
    rol a
    sta seek_location+1
    lda cluster_num+2
    rol a
    sta seek_location+2
    lda cluster_num+3
    rol a
    asl seek_location+0
    rol seek_location+1
    rol seek_location+2
    rol a
    sta seek_location+3

    ; Access both FATs if mirroring is enabled, else only one
    lda fs_ext_flags
    bmi no_mirror
        ; Write first FAT
        jsr add_first_fat
        jsr do_seek
        bcs @io_error
        lda #4
        jsr read_bytes
        bcs @io_error
        lda fat_entry+3
        eor io_xfer+3
        and #$0F
        eor io_xfer+3
        sta fat_temp+0
        jsr do_seek
        bcs @io_error
        jsr begin_write
        lda fat_entry+0
        sta CMD_DATA
        lda fat_entry+1
        sta CMD_DATA
        lda fat_entry+2
        sta CMD_DATA
        lda fat_temp+0
        sta CMD_DATA
        jsr end_write
        bcs @io_error

        ; Write second FAT
        jsr add_fat_delta
        jsr do_seek
        bcs @io_error
        jsr begin_write
        lda fat_entry+0
        sta CMD_DATA
        lda fat_entry+1
        sta CMD_DATA
        lda fat_entry+2
        sta CMD_DATA
        lda fat_temp+0
        sta CMD_DATA
        jsr end_write
        bcs @io_error

        ; C already clear
        rts

    @io_error:
        lda #$100-EIO
        sec
        rts

    no_mirror:
    cmp #$81
    bcs @second_fat
        ; Write first FAT
        jsr add_first_fat
        jsr do_seek
        bcs @io_error
        lda #4
        jsr read_bytes
        bcs @io_error
        lda fat_entry+3
        eor io_xfer+3
        and #$0F
        eor io_xfer+3
        sta fat_temp+0
        jsr do_seek
        bcs @io_error
        jsr begin_write
        lda fat_entry+0
        sta CMD_DATA
        lda fat_entry+1
        sta CMD_DATA
        lda fat_entry+2
        sta CMD_DATA
        lda fat_temp+0
        sta CMD_DATA
        jsr end_write
        bcs @io_error

        ; C already clear
        rts

    @second_fat:
        ; Write second FAT
        jsr add_second_fat
        jsr do_seek
        bcs @io_error
        lda #4
        jsr read_bytes
        bcs @io_error
        lda fat_entry+3
        eor io_xfer+3
        and #$0F
        eor io_xfer+3
        sta fat_temp+0
        jsr do_seek
        bcs @io_error
        jsr begin_write
        lda fat_entry+0
        sta CMD_DATA
        lda fat_entry+1
        sta CMD_DATA
        lda fat_entry+2
        sta CMD_DATA
        lda fat_temp+0
        sta CMD_DATA
        jsr end_write
        bcs @io_error

        ; C already clear
        rts

    @io_error:
        lda #$100-EIO
        sec
        rts

.endproc

.bss

; Temporary bytes used by write_fat_entry
fat_temp: .res 2

.code

; Add fs_first_fat to seek_location
.proc add_first_fat

    clc
    lda fs_first_fat+0
    adc seek_location+1
    sta seek_location+1
    lda fs_first_fat+1
    adc seek_location+2
    sta seek_location+2
    lda fs_first_fat+2
    adc seek_location+3
    sta seek_location+3

    rts

.endproc

; Add fs_second_fat to seek_location
.proc add_second_fat

    clc
    lda fs_second_fat+0
    adc seek_location+1
    sta seek_location+1
    lda fs_second_fat+1
    adc seek_location+2
    sta seek_location+2
    lda fs_second_fat+2
    adc seek_location+3
    sta seek_location+3

    rts

.endproc

; Given seek_location for first FAT, offset it to point to second FAT
.proc add_fat_delta

    sec
    lda seek_location+1
    sbc fs_first_fat+0
    sta seek_location+1
    lda seek_location+2
    sbc fs_first_fat+1
    sta seek_location+2
    lda seek_location+3
    sbc fs_first_fat+2
    sta seek_location+3

    clc
    lda fs_second_fat+0
    adc seek_location+1
    sta seek_location+1
    lda fs_second_fat+1
    adc seek_location+2
    sta seek_location+2
    lda fs_second_fat+2
    adc seek_location+3
    sta seek_location+3

    rts

.endproc

.bss

seek_location: .res 4

.code

; Seek to the position in seek_location
; Return C set if error
.proc do_seek

    lda #ULTIDOS_TARGET
    sta CMD_DATA
    lda #DOS_CMD_FILE_SEEK
    sta CMD_DATA
    lda seek_location+0
    sta CMD_DATA
    lda seek_location+1
    sta CMD_DATA
    lda seek_location+2
    sta CMD_DATA
    lda seek_location+3
    sta CMD_DATA
    jsr get_cmd_response
    lda io_xfer+256
    cmp #'0'
    bne io_error
    clc
    rts

io_error:
    sec
    rts

.endproc

; Read bytes from the volume
; A contains the number of bytes to read
; Bytes are returned in io_xfer
; Return C set if error
.proc read_bytes

    ldx #ULTIDOS_TARGET
    stx CMD_DATA
    ldx #DOS_CMD_READ_DATA
    stx CMD_DATA
    sta CMD_DATA
    lda #0
    sta CMD_DATA
    jsr get_cmd_response
    cpy #1
    rts

.endproc

; Begin a write command
; This function does not return an error
.proc begin_write

    lda #ULTIDOS_TARGET
    sta CMD_DATA
    lda #DOS_CMD_WRITE_DATA
    sta CMD_DATA
    lda #0
    sta CMD_DATA
    sta CMD_DATA
    rts

.endproc

; End a write command
; Return C set if error
.proc end_write

    jsr get_cmd_response
    lda io_xfer+256
    sec
    sbc #$30
    cmp #1
    rts

.endproc

; Get current time
; This function never returns an error
.proc get_time

    lda #ULTIDOS_TARGET
    sta CMD_DATA
    lda #DOS_CMD_GET_TIME
    sta CMD_DATA
    lda #0
    sta CMD_DATA
    jmp get_cmd_response

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.if 0
.proc dump_dir_entry

    ldx #<name_label
    ldy #>name_label
    jsr print_string
    .repeat 11,i
        lda file_data+filedata::name+i
        jsr CHROUT
    .endrep
    ldx #<name_tail
    ldy #>name_tail
    jsr print_string

    ldx #<attr_label
    ldy #>attr_label
    jsr print_string
    lda file_data+filedata::attributes
    jsr print_byte
    lda #$0D
    jsr CHROUT

    ldx #<cluster_label
    ldy #>cluster_label
    jsr print_string
    lda file_data+filedata::cluster_hi+1
    jsr print_byte
    lda file_data+filedata::cluster_hi+0
    jsr print_byte
    lda file_data+filedata::cluster_lo+1
    jsr print_byte
    lda file_data+filedata::cluster_lo+0
    jsr print_byte
    lda #$0D
    jsr CHROUT

    ldx #<size_label
    ldy #>size_label
    jsr print_string
    lda file_data+filedata::size+3
    jsr print_byte
    lda file_data+filedata::size+2
    jsr print_byte
    lda file_data+filedata::size+1
    jsr print_byte
    lda file_data+filedata::size+0
    jsr print_byte
    lda #$0D
    jsr CHROUT

    ldx #<dir_entry_label
    ldy #>dir_entry_label
    jsr print_string
    lda file_data+filedata::dir_entry+3
    jsr print_byte
    lda file_data+filedata::dir_entry+2
    jsr print_byte
    lda file_data+filedata::dir_entry+1
    jsr print_byte
    lda file_data+filedata::dir_entry+0
    jsr print_byte
    lda #$0D
    jsr CHROUT

    ldx #<lfn_entry_label
    ldy #>lfn_entry_label
    jsr print_string
    lda file_data+filedata::lfn_entry+3
    jsr print_byte
    lda file_data+filedata::lfn_entry+2
    jsr print_byte
    lda file_data+filedata::lfn_entry+1
    jsr print_byte
    lda file_data+filedata::lfn_entry+0
    jsr print_byte
    lda #$0D
    jsr CHROUT

    rts

name_label:
    .byte "name=", $22, 0
name_tail:
    .byte $22, $0D, 0
attr_label:
    .asciiz "attributes="
cluster_label:
    .asciiz "cluster="
size_label:
    .asciiz "size="
dir_entry_label:
    .asciiz "dir_entry="
lfn_entry_label:
    .asciiz "lfn_entry="

.endproc
.endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For debugging

.code

; Print string at Y:X
.proc print_string

    stx @print_addr+1
    sty @print_addr+2
    @print:
        ldy #0
        @print_addr:
        lda $FFFF
        beq @end_print
        jsr CHROUT
    inc @print_addr+1
    bne @print
    inc @print_addr+2
    bne @print
    @end_print:
    rts

.endproc

; Print byte in A as hex
.proc print_byte

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
    jmp CHROUT

hexdigit: .byte "0123456789ABCDEF"

.endproc

.data
thousand_lo:
    .byte <0
    .byte <1000
    .byte <2000
    .byte <3000
    .byte <4000
    .byte <5000
    .byte <6000
    .byte <7000
    .byte <8000
    .byte <9000

thousand_hi:
    .byte >0
    .byte >1000
    .byte >2000
    .byte >3000
    .byte >4000
    .byte >5000
    .byte >6000
    .byte >7000
    .byte >8000
    .byte >9000

hundred_lo:
    .byte <0
    .byte <100
    .byte <200
    .byte <300
    .byte <400
    .byte <500
    .byte <600
    .byte <700
    .byte <800
    .byte <900

hundred_hi:
    .byte >0
    .byte >100
    .byte >200
    .byte >300
    .byte >400
    .byte >500
    .byte >600
    .byte >700
    .byte >800
    .byte >900

ten:
    .byte 0
    .byte 10
    .byte 20
    .byte 30
    .byte 40
    .byte 50
    .byte 60
    .byte 70
    .byte 80
    .byte 90

.segment "PAGEALIGN"
.align 256

ascii_to_pet:
    .byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0D,$0B,$0C,$0A,$0E,$0F
    .byte $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1A,$1B,$1C,$1D,$1E,$1F
    .byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F
    .byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$3F
    .byte $40,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6A,$6B,$6C,$6D,$6E,$6F
    .byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7A,$5B,$5C,$5D,$5E,$5F
    .byte $60,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4A,$4B,$4C,$4D,$4E,$4F
    .byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5A,$7B,$7C,$7D,$7E,$7F
    .byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8A,$8B,$8C,$8D,$8E,$8F
    .byte $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9A,$9B,$9C,$9D,$9E,$9F
    .byte $A0,$A1,$A2,$A3,$A4,$A5,$A6,$A7,$A8,$A9,$AA,$AB,$AC,$AD,$AE,$AF
    .byte $B0,$B1,$B2,$B3,$B4,$B5,$B6,$B7,$B8,$B9,$BA,$BB,$BC,$BD,$BE,$BF
    .byte $C0,$C1,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$CB,$CC,$CD,$CE,$CF
    .byte $D0,$D1,$D2,$D3,$D4,$D5,$D6,$D7,$D8,$D9,$DA,$DB,$DC,$DD,$DE,$DF
    .byte $E0,$E1,$E2,$E3,$E4,$E5,$E6,$E7,$E8,$E9,$EA,$EB,$EC,$ED,$EE,$EF
    .byte $F0,$F1,$F2,$F3,$F4,$F5,$F6,$F7,$F8,$F9,$FA,$FB,$FC,$FD,$FE,$FF

; Conversion table for characters in short file names
; 0 indicates an invalid character
; Otherwise, convert lower to upper case and replace $E5 with $05
name_cvt:
    .byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    .byte   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    .byte $20,$21,  0,$23,$24,$25,$26,$27,$28,$29,  0,  0,  0,$2D,  0,  0
    .byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,  0,  0,  0,  0,  0,  0
    .byte $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4A,$4B,$4C,$4D,$4E,$4F
    .byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5A,  0,  0,  0,$5E,$5F
    .byte $60,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4A,$4B,$4C,$4D,$4E,$4F
    .byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5A,$7B,  0,$7D,$7E,  0
    .byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8A,$8B,$8C,$8D,$8E,$8F
    .byte $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9A,$9B,$9C,$9D,$9E,$9F
    .byte $A0,$A1,$A2,$A3,$A4,$A5,$A6,$A7,$A8,$A9,$AA,$AB,$AC,$AD,$AE,$AF
    .byte $B0,$B1,$B2,$B3,$B4,$B5,$B6,$B7,$B8,$B9,$BA,$BB,$BC,$BD,$BE,$BF
    .byte $C0,$C1,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$CB,$CC,$CD,$CE,$CF
    .byte $D0,$D1,$D2,$D3,$D4,$D5,$D6,$D7,$D8,$D9,$DA,$DB,$DC,$DD,$DE,$DF
    .byte $E0,$E1,$E2,$E3,$E4,$05,$E6,$E7,$E8,$E9,$EA,$EB,$EC,$ED,$EE,$EF
    .byte $F0,$F1,$F2,$F3,$F4,$F5,$F6,$F7,$F8,$F9,$FA,$FB,$FC,$FD,$FE,$FF
