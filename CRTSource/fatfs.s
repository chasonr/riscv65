; Implementation of FAT file system.
; Supports long file names and FAT12, FAT16 and FAT32.

.macpack longbranch
.include "memory.inc"
.include "farcall.inc"
.include "errno.inc"
.include "fatfs.inc"
;;;;
;.include "kernal.inc"
;;;;

; Use this UltiDOS target to access the file system
FATFS_TARGET = 1

; Structure of FAT32 information sector
.struct FAT32_FSInfo
    FSI_LeadSig    .byte 3
    FSI_Reserved1  .byte 480
    FSI_StrucSig   .byte 4
    FSI_Free_Count .byte 4
    FSI_Nxt_Free   .byte 4
    FSI_Reserved2  .byte 12
    FSI_TrailSig   .byte 4
.endstruct

; Long name record in directory
.struct long_name
    LDIR_Ord       .byte 1
    LDIR_Name1     .byte 10
    LDIR_Attr      .byte 1
    LDIR_Type      .byte 1
    LDIR_Chksum    .byte 1
    LDIR_Name2     .byte 12
    LDIR_FstClusLO .byte 2
    LDIR_Name3     .byte 4
.endstruct

; Attribute flags
ATTR_READONLY  = $01
ATTR_HIDDEN    = $02
ATTR_SYSTEM    = $04
ATTR_VOLUME    = $08
ATTR_DIRECTORY = $10
ATTR_ARCHIVE   = $20

.rodata

; Table of pointers to the open file records

open_files_lo:
    .repeat FATFS_MAX_FILES,i
        .byte <(fatfs_open_files + .sizeof(fatfs_filedata)*i)
    .endrep
open_files_hi:
    .repeat FATFS_MAX_FILES,i
        .byte >(fatfs_open_files + .sizeof(fatfs_filedata)*i)
    .endrep

.code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Initialize the file system                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Initialize a FAT file system from a file opened on target #1
; On return, pointer1 is 0 if successful; on failure, pointer1 is nonzero and
; ultidos_status contains an error message, converted to PETSCII

.proc fatfs_init

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

    ; Initialize the FAT cache
    jsr fat_init

    ; Mark all files as closed
    ldx #FATFS_MAX_FILES
    ldy #0
    @close:
        lda open_files_lo-1,x
        sta pointer1+0
        lda open_files_hi-1,x
        sta pointer1+1
        tya
        sta (pointer1),y
    dex
    bne @close

    ; Seek to the start of the file system
    lda #0
    sta longreg1+0
    sta longreg1+1
    sta longreg1+2
    sta longreg1+3
    jsr dos_seek
    bcc :+
        jmp petscii_error
    :

    ; Set a bad signature to check for successful read
    lda #0
    sta fatfs_data_sector+510
    sta fatfs_data_sector+511

    ; Read the boot sector from the file system
    lda #<fatfs_data_sector
    sta pointer2+0
    lda #>fatfs_data_sector
    sta pointer2+1
    lda #<512
    sta pointer3+0
    lda #>512
    sta pointer3+1
    jsr dos_read
    bcc :+
        jmp petscii_error
    :

    ; Check the signature at the end of the block
    lda fatfs_data_sector+510
    cmp #$55
    bne bad_fs_1
    lda fatfs_data_sector+511
    cmp #$AA
    bne bad_fs_1

    ; Partitions are not supported, and so BPB_HiddSec must be zero
    lda fatfs_data_sector+boot_record::BPB_HiddSec+0
    ora fatfs_data_sector+boot_record::BPB_HiddSec+1
    ora fatfs_data_sector+boot_record::BPB_HiddSec+2
    ora fatfs_data_sector+boot_record::BPB_HiddSec+3
    bne bad_fs_1

    ; Determine fatfs_sector_shift
    lda fatfs_data_sector+boot_record::BPB_BytsPerSec+0
    bne bad_fs_1    ; sector size < 256 or not power of two
    lda fatfs_data_sector+boot_record::BPB_BytsPerSec+1
    beq bad_fs_1    ; sector size is zero
    ldx #0
    @shift1:
        lsr a
        bcs @end_shift1
    inx
    bne @shift1
    @end_shift1:
    ; Sector size must be a power of two
    cmp #0
    bne bad_fs_1
    ; Valid sector sizes are 512, 1024, 2048 and 4096
    cpx #1
    bcc bad_fs_1
    cpx #5
    bcs bad_fs_1
    stx fatfs_sector_shift
    ; Sector size is 2**(fatfs_sector_shift+8)

    ; Determine fatfs_cluster_shift
    ; Leave X alone: this count is cumulative with the last one
    lda fatfs_data_sector+boot_record::BPB_SecPerClus
    beq bad_fs_1    ; cluster size is zero
    @shift2:
        lsr a
        bcs @end_shift2
    inx
    bne @shift2
    @end_shift2:
    cmp #0
    bne bad_fs_1    ; cluster size must be a power of two
    cpx #8
    bcc :+          ; cluster size must not be greater than 32768
    bad_fs_1:
        jmp invalid_file_system
    :
    stx fatfs_cluster_shift
    ; Cluster size is 2**(fatfs_cluster_shift+8)

    ; Determine fatfs_cluster_size
    lda bit_index,x
    sta fatfs_cluster_size
    ; Cluster size is fatfs_cluster_size * 256

    ; Determine fatfs_first_fat
    ; fatfs_first_fat <- BPB_RsvdSecCnt << fatfs_sector_shift
    lda fatfs_data_sector+boot_record::BPB_RsvdSecCnt+0
    sta fatfs_first_fat+0
    lda fatfs_data_sector+boot_record::BPB_RsvdSecCnt+1
    sta fatfs_first_fat+1
    lda #0
    ldx fatfs_sector_shift
    @shift3:
        asl fatfs_first_fat+0
        rol fatfs_first_fat+1
        rol a
    dex
    bne @shift3
    sta fatfs_first_fat+2
    ; Seek position of first FAT is fatfs_first_fat * 256

    ; Determine the number of 256-byte blocks per FAT
    lda #0
    sta fatfs_fat_size+2
    lda fatfs_data_sector+boot_record::BPB_FATSz16+0
    sta fatfs_fat_size+0
    lda fatfs_data_sector+boot_record::BPB_FATSz16+1
    sta fatfs_fat_size+1
    ora fatfs_fat_size+0
    bne @got_fat_size
        ; BPB_FATSz16 is zero; use BPB_FATSz32
        ; This is possible only on FAT32
        lda fatfs_data_sector+boot_record::BPB_FATSz32+0
        sta fatfs_fat_size+0
        lda fatfs_data_sector+boot_record::BPB_FATSz32+1
        sta fatfs_fat_size+1
        lda fatfs_data_sector+boot_record::BPB_FATSz32+2
        sta fatfs_fat_size+2
        lda fatfs_data_sector+boot_record::BPB_FATSz32+3
        bne bad_fs_1 ; too large
    @got_fat_size:
    ; fatfs_fat_size is the number of sectors per FAT
    ; Convert to number of 256-byte blocks
    lda fatfs_fat_size+2
    ldx fatfs_sector_shift
    @shift4:
        asl fatfs_fat_size+0
        rol fatfs_fat_size+1
        rol a
        bcs bad_fs_1  ; too large
    dex
    bne @shift4
    sta fatfs_fat_size+2
    ; Size of FAT is fatfs_fat_size * 256

    ; Check the number of FATs and determine fatfs_second_fat
    lda fatfs_data_sector+boot_record::BPB_NumFATs
    sta fatfs_num_fats
    cmp #1
    beq one_fat
    cmp #2
    beq two_fats
        jmp invalid_file_system
    two_fats:
        ; Two FATs
        clc
        lda fatfs_first_fat+0
        adc fatfs_fat_size+0
        sta fatfs_second_fat+0
        lda fatfs_first_fat+1
        adc fatfs_fat_size+1
        sta fatfs_second_fat+1
        lda fatfs_first_fat+2
        adc fatfs_fat_size+2
        sta fatfs_second_fat+2
        bcs bad_fs_2
        ; Seek position of second FAT is fatfs_second_fat * 256
    bcc got_fats
    one_fat:
        ; One FAT
        lda fatfs_first_fat+0
        sta fatfs_second_fat+0
        lda fatfs_first_fat+1
        sta fatfs_second_fat+1
        lda fatfs_first_fat+2
        sta fatfs_second_fat+2
        ; fatfs_second_fat == fatfs_first_fat
    got_fats:

    ; Determine location of root directory
    clc
    lda fatfs_fat_size+0
    adc fatfs_second_fat+0
    sta fatfs_root_dir+0
    lda fatfs_fat_size+1
    adc fatfs_second_fat+1
    sta fatfs_root_dir+1
    lda fatfs_fat_size+2
    adc fatfs_second_fat+2
    sta fatfs_root_dir+2
    bcc :+
    bad_fs_2:
        jmp invalid_file_system
    :

    ; Determine the number of entries in the root directory
    lda fatfs_data_sector+boot_record::BPB_RootEntCnt+0
    sta fatfs_root_dir_size+0
    sta fatfs_root_dir_bytes+0
    lda fatfs_data_sector+boot_record::BPB_RootEntCnt+1
    sta fatfs_root_dir_size+1
    sta fatfs_root_dir_bytes+1
    ; The number of entries in the root directory is fatfs_root_dir_size

    ; Determine the root directory size in bytes
    lda #0
    .repeat 3
        lsr fatfs_root_dir_bytes+1
        ror fatfs_root_dir_bytes+0
        ror a
    .endrep

    ; Round up to sector size
    ldx fatfs_data_sector+boot_record::BPB_BytsPerSec+1
    dex
    cmp #$01 ; set carry if A != 0
    txa
    adc fatfs_root_dir_bytes+0 ; Round up to multiple of 256
    sta fatfs_root_dir_bytes+0
    lda #0
    adc fatfs_root_dir_bytes+1
    sta fatfs_root_dir_bytes+1
    txa                     ; and then to sector size
    eor #$FF
    and fatfs_root_dir_bytes+0
    sta fatfs_root_dir_bytes+0
    ; The number of bytes in the root directory is fatfs_root_dir_bytes * 256

    ; Add the base of the root directory to get the offset to cluster #2
    clc
    lda fatfs_root_dir_bytes+0
    adc fatfs_root_dir+0
    sta fatfs_cluster_base+0
    lda fatfs_root_dir_bytes+1
    adc fatfs_root_dir+1
    sta fatfs_cluster_base+1
    lda #0
    adc fatfs_root_dir+2
    sta fatfs_cluster_base+2
    bcc :+
    bad_fs_3:
        jmp invalid_file_system
    :

    ; The first cluster is cluster #2. Subtract twice the cluster size from
    ; the cluster base.

    lda fatfs_cluster_size
    asl a   ; cluster size * 2 low byte
    tax
    lda #0
    rol a   ; cluster size * 2 high byte
    tay
    sec
    txa
    eor #$FF
    adc fatfs_cluster_base+0
    sta fatfs_cluster_base+0
    tya
    eor #$FF
    adc fatfs_cluster_base+1
    sta fatfs_cluster_base+1
    lda fatfs_cluster_base+2
    sbc #0
    sta fatfs_cluster_base+2

    ; Determine the size of the file system
    lda #0
    sta fatfs_fs_size+2
    lda fatfs_data_sector+boot_record::BPB_TotSec16+0
    sta fatfs_fs_size+0
    lda fatfs_data_sector+boot_record::BPB_TotSec16+1
    sta fatfs_fs_size+1
    ora fatfs_fs_size+0
    bne @got_sectors
        ; 16 bit sector count is zero; use 32 bit sector count
        lda fatfs_data_sector+boot_record::BPB_TotSec32+0
        sta fatfs_fs_size+0
        lda fatfs_data_sector+boot_record::BPB_TotSec32+1
        sta fatfs_fs_size+1
        lda fatfs_data_sector+boot_record::BPB_TotSec32+2
        sta fatfs_fs_size+2
        lda fatfs_data_sector+boot_record::BPB_TotSec32+3
        bne bad_fs_3
    @got_sectors:
    ; fatfs_fs_size contains the number of sectors

    ; Shift sector count to file system size in 256-byte blocks
    lda fatfs_fs_size+2
    ldx fatfs_sector_shift
    @shift5:
        asl fatfs_fs_size+0
        rol fatfs_fs_size+1
        rol a
        bcs bad_fs_3
    dex
    bne @shift5
    sta fatfs_fs_size+2
    ; fatfs_fs_size * 256 is the size of the file system

    ; Subtract fatfs_cluster_base, giving the number of 256-byte blocks available
    ; for data
    sec
    lda fatfs_fs_size+0
    sbc fatfs_cluster_base+0
    sta fatfs_clusters+0
    lda fatfs_fs_size+1
    sbc fatfs_cluster_base+1
    sta fatfs_clusters+1
    lda fatfs_fs_size+2
    sbc fatfs_cluster_base+2
    bcc bad_fs_4

    ; Shift to get the number of clusters
    ldx fatfs_cluster_shift
    @shift6:
        lsr a
        ror fatfs_clusters+1
        ror fatfs_clusters+0
    dex
    bne @shift6
    cmp #0
    beq :+
    bad_fs_4:
        jmp invalid_file_system
    :
    sta fatfs_clusters+2

    ; Set the current directory to the root
    lda #0
    sta fatfs_current_dir_num_components

    ; Set the file system type and FAT32-specific parameters
    lda #0
    sta fatfs_ext_flags
    sta fatfs_info_sector_pos+0
    sta fatfs_info_sector_pos+1
    sta fatfs_info_sector_pos+2
    lda fatfs_clusters+2
    bne @fat32          ; must be FAT32 if > 65535
    lda fatfs_clusters+0
    cmp #$F7
    lda fatfs_clusters+1
    sbc #$0F
    bcs @fat16
        lda #12
        sta fatfs_type
        jmp success
    @fat16:
    lda fatfs_clusters+0
    cmp #$F7
    lda fatfs_clusters+1
    sbc #$FF
    bcs @fat32
        lda #16
        sta fatfs_type
        jmp success
    @fat32:
        ; Check for version 0
        lda fatfs_data_sector+boot_record::BPB_FSVer+0
        ora fatfs_data_sector+boot_record::BPB_FSVer+1
        bne invalid_file_system

        lda #32
        sta fatfs_type
        lda fatfs_data_sector+boot_record::BPB_ExtFlags
        sta fatfs_ext_flags
        lda fatfs_data_sector+boot_record::BPB_FSInfo+0
        sta fatfs_info_sector_pos+0
        lda fatfs_data_sector+boot_record::BPB_FSInfo+1
        sta fatfs_info_sector_pos+1
        lda fatfs_data_sector+boot_record::BPB_RootClus+0
        sta fatfs_root_dir+0
        lda fatfs_data_sector+boot_record::BPB_RootClus+1
        sta fatfs_root_dir+1
        lda fatfs_data_sector+boot_record::BPB_RootClus+2
        sta fatfs_root_dir+2
        lda fatfs_data_sector+boot_record::BPB_RootClus+3
        bne invalid_file_system
        jsr init_info_sector
        bcs petscii_error

success:
    lda #0
    sta pointer1+0
    sta pointer1+1
    rts

invalid_file_system:
    ldx #<bad_fs
    ldy #>bad_fs
    jsr set_error

petscii_error:
    lda #$FF
    sta pointer1+0
    sta pointer1+1
    rts

bad_fs:
    .byte "Invalid file system", 0

bit_index:
    .byte $01, $02, $04, $08, $10, $20, $40, $80

.endproc

; For FAT32: Process the information sector if it is present
; Return C if error, and PETSCII error message in ultidos_status
; On success (C=0), fatfs_info_sector_pos is nonzero if the information sector
; is valid
.proc init_info_sector

    lda #0
    sta fatfs_free_changed
    ; Get the location of the information sector
    ; 0x0000 and 0xFFFF indicate that the sector is not present
    lda fatfs_info_sector_pos+0
    ora fatfs_info_sector_pos+1
    beq no_block_0
    lda fatfs_info_sector_pos+0
    and fatfs_info_sector_pos+1
    cmp #$FF
    bne :+
    no_block_0:
        jmp no_block
    :
    ; Shift to make a byte offset
    lda #0
    ldx fatfs_sector_shift
    beq @end_lshift
    @lshift:
        asl fatfs_info_sector_pos+0
        rol fatfs_info_sector_pos+1
        rol a
    dex
    bne @lshift
    @end_lshift:
    sta fatfs_info_sector_pos+2

    ; Seek to the information sector
    lda #0
    sta longreg1+0
    lda fatfs_info_sector_pos+0
    sta longreg1+1
    lda fatfs_info_sector_pos+1
    sta longreg1+2
    lda fatfs_info_sector_pos+2
    sta longreg1+3
    jsr dos_seek
    jcs error

    ; Set a bad signature to check for incomplete read
    lda #$FF
    sta fatfs_info_sector+FAT32_FSInfo::FSI_TrailSig+0
    sta fatfs_info_sector+FAT32_FSInfo::FSI_TrailSig+1
    sta fatfs_info_sector+FAT32_FSInfo::FSI_TrailSig+2
    sta fatfs_info_sector+FAT32_FSInfo::FSI_TrailSig+3

    ; Read the information sector
    lda #<fatfs_info_sector
    sta pointer2+0
    lda #>fatfs_info_sector
    sta pointer2+1
    lda #<512
    sta pointer3+0
    lda #>512
    sta pointer3+1
    jsr dos_read
    bcs error

    ; Check the first signature
    lda fatfs_info_sector+FAT32_FSInfo::FSI_LeadSig+0
    cmp #$52
    bne no_block
    lda fatfs_info_sector+FAT32_FSInfo::FSI_LeadSig+1
    cmp #$52
    bne no_block
    lda fatfs_info_sector+FAT32_FSInfo::FSI_LeadSig+2
    cmp #$61
    bne no_block
    lda fatfs_info_sector+FAT32_FSInfo::FSI_LeadSig+3
    cmp #$41
    bne no_block

    ; Check the second signature
    lda fatfs_data_sector+FAT32_FSInfo::FSI_StrucSig+0
    cmp #$72
    bne no_block
    lda fatfs_data_sector+FAT32_FSInfo::FSI_StrucSig+1
    cmp #$72
    bne no_block
    lda fatfs_data_sector+FAT32_FSInfo::FSI_StrucSig+2
    cmp #$41
    bne no_block
    lda fatfs_data_sector+FAT32_FSInfo::FSI_StrucSig+3
    cmp #$61
    bne no_block

    ; Check the third signature
    lda fatfs_data_sector+FAT32_FSInfo::FSI_TrailSig+0
    cmp #$00
    bne no_block
    lda fatfs_data_sector+FAT32_FSInfo::FSI_TrailSig+1
    cmp #$00
    bne no_block
    lda fatfs_data_sector+FAT32_FSInfo::FSI_TrailSig+2
    cmp #$55
    bne no_block
    lda fatfs_data_sector+FAT32_FSInfo::FSI_TrailSig+3
    cmp #$AA
    bne no_block

    clc
    rts

no_block:
    ; Indicate no information block
    lda #0
    sta fatfs_info_sector_pos+0
    sta fatfs_info_sector_pos+1
    sta fatfs_info_sector_pos+2
    clc
    rts

error:
    sec
    rts

.endproc

; Set error string in ultidos_status
; Y:X point to the zero-terminated PETSCII error string

.proc set_error

    stx pointer1+0
    sty pointer1+1
    ldy #0
    @err:
        lda (pointer1),y
        beq @end_err
        sta ultidos_status,y
    iny
    cpy #255
    bcc @err
    @end_err:
    lda #0
    sta ultidos_status,y
    rts

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Search for a file                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Entry point to search for the startup program
; The name "/startup" is best placed in this bank, where fatfs_search can
; find it
; See fatfs_search for return conditions

.proc fatfs_search_startup

    lda #<startup
    sta pointer1+0
    lda #>startup
    sta pointer1+1
    jmp fatfs_search

.endproc

.rodata

startup:
    ; This is "/startup"; but the .literal directive is not yet in a released
    ; version of ca65
    .byte '/', $73, $74, $61, $72, $74, $75, $70, $00

.code

; Entry point to search for a file
; pointer1 points to the path to search
; On success:
; * pointer1 returns 0
; * fatfs_file_data is populated with information about the file
; * fatfs_search_dir and fatfs_search_dir_num_components are populated with
;   the complete path
; On failure:
; * pointer1 returns -errno
; * If the file was not found (pointer1 == -ENOENT):
;   - pointer2 is nonzero if the search failed on the last component
;   - pointer3 points to the path component that failed
; * fatfs_file_data is populated with information about the directory in
;   which the search failed
; * fatfs_search_dir and fatfs_search_dir_num_components are populated with
;   the path so far as it was found

.proc fatfs_search

    ; Use longreg2 to advance through the file path, so pointer1 will be free
    ; to make calls to UltiDOS
    lda pointer1+0
    sta longreg2+0
    lda pointer1+1
    sta longreg2+1

    ; Clear fatfs_file_data
    ldy #.sizeof(fatfs_filedata)
    lda #0
    @clear_1:
        sta fatfs_file_data-1,y
    dey
    bne @clear_1

    ; Set the starting directory
    ; This is the root directory if the path begins with '/'; otherwise, it
    ; is the current directory

    ldy #0
    sty fatfs_search_dir_num_components ; set to root directory
    lda (longreg2),y
    cmp #'/'
    beq search_dir_set

        ; Path does not begin with '/'; start from the current directory
        lda fatfs_current_dir_num_components
        sta fatfs_search_dir_num_components
        ldy #.sizeof(fatfs_dir_entry)*FATFS_MAX_COMPONENTS
        @copy_1:
            lda fatfs_current_dir-1,y
            sta fatfs_search_dir-1,y
        dey
        bne @copy_1 

    search_dir_set:

    ; Populate fatfs_file_data with information about the starting directory

    jsr set_dir_from_search_dir
    bcc :+
        sta pointer1+0
        lda #$FF
        sta pointer1+1
        rts
    :

    ; Set current_file for routines that need it
    lda #<fatfs_file_data
    sta current_file+0
    lda #>fatfs_file_data
    sta current_file+1

    ; Main search loop

    path_loop:

        ; Skip slashes
        jsr skip_slashes

        ; Break loop at end of path
        ldy #0
        lda (longreg2),y
        jeq end_path_loop

        ; Must be a directory
        lda fatfs_file_data+fatfs_filedata::attributes
        and #ATTR_DIRECTORY
        jeq not_a_directory

        ; Go to the start of the directory
        jsr rewind_current_file

        ; Get length of path component
        ldy #0
        @comp_len:
            lda (longreg2),y
            beq @end_comp_len
            cmp #'/'
            beq @end_comp_len
        iny
        bne @comp_len
        jeq name_too_long
        @end_comp_len:
        sty fatfs_component_len

        ; Check for .
        cpy #1
        bne end_dot
            dey
            lda (longreg2),y
            iny
            cmp #'.'
            bne end_dot
                inc longreg2+0
                bne :+
                    inc longreg2+1
                :
                jmp path_loop
        end_dot:

        ; Check for ..
        cpy #2
        bne end_dotdot
            ldy #0
            lda (longreg2),y
            tax
            iny
            lda (longreg2),y
            iny
            cpx #'.'
            bne end_dotdot
            cmp #'.'
            bne end_dotdot
                lda fatfs_search_dir_num_components
                beq dotdot_done
                    dec fatfs_search_dir_num_components
                    jsr set_dir_from_search_dir
                    bcc :+
                        sta pointer1+0
                        lda #$FF
                        sta pointer1+1
                        rts
                    :
                dotdot_done:
                jmp path_loop
        end_dotdot:

        ; Check for path length exceeding FATFS_MAX_COMPONENTS
        lda fatfs_search_dir_num_components
        cmp #FATFS_MAX_COMPONENTS
        jcs name_too_long

        ; How many long name records encode this name?
        dey
        tya
        ldx #0
        @lfn_rec_count:
            inx
            sec
            sbc #13
        bcs @lfn_rec_count

        ; Set the starting sequence number
        txa
        ora #$40
        sta fatfs_start_seq

        ; Set the offset to the last segment of the name
        lda times_13-1,x
        sta fatfs_last_segment

        ; Loop over the directory entries
        dir_loop:

            ; Read one directory entry
            jsr seek_and_read_dir_entry
            jcs io_error

            ; The long file name match algorithm branches here on any failure,
            ; so the non-matching record can start another match.
            restart:

            ; Set lfn_entry
            lda fatfs_file_data+fatfs_filedata::dir_entry+0
            sta fatfs_file_data+fatfs_filedata::lfn_entry+0
            lda fatfs_file_data+fatfs_filedata::dir_entry+1
            sta fatfs_file_data+fatfs_filedata::lfn_entry+1
            lda fatfs_file_data+fatfs_filedata::dir_entry+2
            sta fatfs_file_data+fatfs_filedata::lfn_entry+2
            lda fatfs_file_data+fatfs_filedata::dir_entry+3
            sta fatfs_file_data+fatfs_filedata::lfn_entry+3

            ; Check for end marker
            lda fatfs_file_data+0
            jeq not_found

            ; Never match an erasure or a volume label; but a long name record
            ; is not a volume label
            cmp #$E5
            beq dir_loop        ; erasure
            lda fatfs_file_data+long_name::LDIR_Attr
            and #$0F
            cmp #$0F
            beq long_name_record
            and #ATTR_VOLUME
            bne dir_loop        ; volume label

            ; If the short name matches, we have a complete match
            jsr short_name_matches
            bne dir_loop
            jmp end_dir_loop

            ; We can start a long name match if
            ; * the record is a long name record and
            ; * its sequence number is equal to fatfs_start_seq and
            ; * it matches the last segment of the name.
            long_name_record:
            lda fatfs_file_data+long_name::LDIR_Ord
            cmp fatfs_start_seq
            bne dir_loop        ; sequence number incorrect
            ldy fatfs_last_segment
            jsr long_name_matches
            bne dir_loop        ; name does not match

            ; Set current sequence number, segment and checksum
            lda fatfs_start_seq
            and #$3F
            sta fatfs_sequence
            lda fatfs_last_segment
            sta fatfs_segment
            lda fatfs_file_data+long_name::LDIR_Chksum
            sta fatfs_checksum

            ; Read until short name record or match failure
            lfn_loop:

                ; Read the next directory record
                jsr seek_and_read_dir_entry
                jcs io_error

                ; Check for end marker
                lda fatfs_file_data+0
                jeq not_found

                ; Never match an erasure or a volume label; but a long name record
                ; is not a volume label
                cmp #$E5
                jeq dir_loop        ; erasure
                lda fatfs_file_data+long_name::LDIR_Attr
                and #$0F
                cmp #$0F
                beq long_name_record_2
                and #ATTR_VOLUME
                jne dir_loop        ; volume label

                    ; A short name record matches if
                    ; * the last sequence number is 1 and
                    ; * the checksum equals the checksum from the long name
                    ;   records.
                    ; Other short name records end the match with a negative
                    ; result.
                    lda fatfs_sequence
                    cmp #1
                    bne restart_1
                    jsr short_name_checksum
                    cmp fatfs_checksum
                    beq end_dir_loop
                restart_1:
                    jmp restart

                long_name_record_2:
                    ; A long name record continues the match if
                    ; * its sequence number is one less than the last one and
                    ; * its checksum matches that of the last record and
                    ; * it matches the next segment of the name.
                    dec fatfs_sequence
                    beq restart_1           ; sequence number 0 is invalid
                    lda fatfs_file_data+long_name::LDIR_Ord
                    cmp fatfs_sequence
                    bne restart_1           ; sequence number does not match
                    lda fatfs_file_data+long_name::LDIR_Chksum
                    cmp fatfs_checksum
                    bne restart_1           ; checksum does not match

                    sec
                    lda fatfs_segment
                    sbc #13
                    sta fatfs_segment
                    tay
                    jsr long_name_matches
                    bne restart_1           ; name does not match

                    ; Continue the loop to match long name records

            jmp lfn_loop

        jmp dir_loop
        end_dir_loop:

        ; Found the matching directory entry
        ; Add to the search path
        ldx fatfs_search_dir_num_components
        lda lshift3,x
        inx
        stx fatfs_search_dir_num_components
        tax
        lda fatfs_file_data+fatfs_filedata::lfn_entry+0
        sta fatfs_search_dir+fatfs_dir_entry::lfn_entry+0,x
        lda fatfs_file_data+fatfs_filedata::lfn_entry+1
        sta fatfs_search_dir+fatfs_dir_entry::lfn_entry+1,x
        lda fatfs_file_data+fatfs_filedata::lfn_entry+2
        sta fatfs_search_dir+fatfs_dir_entry::lfn_entry+2,x
        lda fatfs_file_data+fatfs_filedata::lfn_entry+3
        sta fatfs_search_dir+fatfs_dir_entry::lfn_entry+3,x
        lda fatfs_file_data+fatfs_filedata::dir_entry+0
        sta fatfs_search_dir+fatfs_dir_entry::dir_entry+0,x
        lda fatfs_file_data+fatfs_filedata::dir_entry+1
        sta fatfs_search_dir+fatfs_dir_entry::dir_entry+1,x
        lda fatfs_file_data+fatfs_filedata::dir_entry+2
        sta fatfs_search_dir+fatfs_dir_entry::dir_entry+2,x
        lda fatfs_file_data+fatfs_filedata::dir_entry+3
        sta fatfs_search_dir+fatfs_dir_entry::dir_entry+3,x

        ; Advance to the next path component
        jsr next_component

        ; Cycle back to path_loop to find the next entry

    jmp path_loop
    end_path_loop:

    ; Populate cluster_partial
    lda fatfs_file_data+fatfs_filedata::size+0
    sta fatfs_file_data+fatfs_filedata::cluster_partial+0
    ldx fatfs_cluster_size
    dex
    txa
    and fatfs_file_data+fatfs_filedata::size+1
    sta fatfs_file_data+fatfs_filedata::cluster_partial+1

    ; Populate cluster_count
    lda fatfs_file_data+fatfs_filedata::size+1
    sta fatfs_file_data+fatfs_filedata::cluster_count+0
    lda fatfs_file_data+fatfs_filedata::size+2
    sta fatfs_file_data+fatfs_filedata::cluster_count+1
    lda fatfs_file_data+fatfs_filedata::size+3
    ldx fatfs_cluster_shift
    @shift:
        lsr fatfs_file_data+fatfs_filedata::cluster_count+0
        ror fatfs_file_data+fatfs_filedata::cluster_count+1
        ror a
    dex
    bne @shift
    sta fatfs_file_data+fatfs_filedata::cluster_count+2

    jsr rewind_current_file

    ; Search is complete and the file is found
    lda #0
    sta pointer1+0
    sta pointer1+1
    rts

name_too_long:
    lda #$100-ENAMETOOLONG
    .byte $2C
not_a_directory:
    lda #$100-ENOTDIR
io_error:
    cmp #$100-ENOENT
    beq not_found
    sta pointer1+0
    lda #$FF
    sta pointer1+1
    lda #0
    sta pointer2+0
    sta pointer2+1
    rts

not_found:
    ; Indicate file not found
    lda #$100-ENOENT
    sta pointer1+0
    lda #$FF
    sta pointer1+1

    ; Set pointer3 to the failing path component
    lda longreg2+0
    sta pointer3+0
    lda longreg2+1
    sta pointer3+1

    ; Advance to the next component of the path
    jsr next_component

    ; Skip slashes
    jsr skip_slashes

    ; Indicate whether we've reached the end of the path
    ldx #0
    lda (longreg2),y
    bne :+
        dex
    :
    stx pointer2+0
    stx pointer2+1

    rts

times_13:
    .repeat 256/13,i
        .byte 13*i
    .endrepeat
.endproc

; Populate fatfs_file_data with the last directory in the search path
; On error, return C set and -errno in A

.proc set_dir_from_search_dir

    ; Mark as directory so the search doesn't reject the directory

    lda #ATTR_DIRECTORY
    sta fatfs_file_data+fatfs_filedata::attributes

    ldx fatfs_search_dir_num_components
    bne start_with_cluster

        ; Starting from the root directory
        lda fatfs_type
        cmp #32
        beq fat32_root

            ; Root directory on FAT12/16
            lda #$FF
            sta fatfs_file_data+fatfs_filedata::cluster_lo+0
            sta fatfs_file_data+fatfs_filedata::cluster_lo+1
            sta fatfs_file_data+fatfs_filedata::cluster_hi+0
            sta fatfs_file_data+fatfs_filedata::cluster_hi+1
            clc
            rts

        fat32_root:

            ; Root directory on FAT32
            lda fatfs_root_dir+0
            sta fatfs_file_data+fatfs_filedata::cluster_lo+0
            lda fatfs_root_dir+1
            sta fatfs_file_data+fatfs_filedata::cluster_lo+1
            lda fatfs_root_dir+2
            sta fatfs_file_data+fatfs_filedata::cluster_hi+0
            lda #0
            sta fatfs_file_data+fatfs_filedata::cluster_hi+1
            clc
            rts

    start_with_cluster:

        ; Starting from the last directory in the path
        lda lshift3,x
        tax
        lda fatfs_search_dir-.sizeof(fatfs_dir_entry)+fatfs_dir_entry::dir_entry+0,x
        sta longreg1+0
        lda fatfs_search_dir-.sizeof(fatfs_dir_entry)+fatfs_dir_entry::dir_entry+1,x
        sta longreg1+1
        lda fatfs_search_dir-.sizeof(fatfs_dir_entry)+fatfs_dir_entry::dir_entry+2,x
        sta longreg1+2
        lda fatfs_search_dir-.sizeof(fatfs_dir_entry)+fatfs_dir_entry::dir_entry+3,x
        sta longreg1+3
        jmp read_dir_entry

.endproc

; Seek to and read the next directory entry
; On error, return C set and -errno in A
; errno is ENOENT if end of directory

.proc seek_and_read_dir_entry

    ; Set the seek position for the directory in fatfs_file_data
    lda #<fatfs_file_data
    sta current_file+0
    lda #>fatfs_file_data
    sta current_file+1
    jsr set_seek_position
    bcs error

    ; Save the seek position for later use in the current-directory structure
    lda longreg1+0
    sta fatfs_file_data+fatfs_filedata::dir_entry+0
    lda longreg1+1
    sta fatfs_file_data+fatfs_filedata::dir_entry+1
    lda longreg1+2
    sta fatfs_file_data+fatfs_filedata::dir_entry+2
    lda longreg1+3
    sta fatfs_file_data+fatfs_filedata::dir_entry+3

    ; Read the directory entry
    jsr read_dir_entry
    bcs error

    ; Advance the file position
    clc
    lda fatfs_file_data+fatfs_filedata::cluster_ptr+0
    adc #32
    sta fatfs_file_data+fatfs_filedata::cluster_ptr+0
    bcc :+
    inc fatfs_file_data+fatfs_filedata::cluster_ptr+1
    bne :+
    inc fatfs_file_data+fatfs_filedata::cluster_ptr+2
    :
    rts

error:
    cmp #$100-ENOENT
    bne :+
        ldx #<not_found_error
        ldy #>not_found_error
        jsr set_error
    :
    sec
    rts

not_found_error: .asciiz "No such file or directory"

.endproc

; Read a directory entry
; longreg1 is the seek position of the directory entry
; On error, return C set and -errno in A
; errno is ENOENT if end of directory

.proc read_dir_entry

    jsr dos_seek
    bcs error

    lda #<fatfs_file_data
    sta pointer2+0
    lda #>fatfs_file_data
    sta pointer2+1
    lda #32
    sta pointer3+0
    lda #0
    sta pointer3+1
    jsr dos_read
    bcs error

    clc
    rts

error:
    sec
    rts

.endproc

; Advance the file at (current_file) to its next cluster
; Return C set and -errno in A on error
; errno is ENOENT if end of chain

.proc next_cluster_in_file

    ; Cluster number to longreg1
    ldy #fatfs_filedata::current_cluster+0
    lda (current_file),y
    sta longreg1+0
    iny
    lda (current_file),y
    sta longreg1+1
    iny
    lda (current_file),y
    sta longreg1+2
    iny
    lda (current_file),y
    sta longreg1+3

    ; Read the next entry

    jsr read_fat_entry
    bcs error

    ; Copy back to cluster number
    ldy #fatfs_filedata::current_cluster+0
    lda longreg1+0
    sta (current_file),y
    iny
    lda longreg1+1
    sta (current_file),y
    iny
    lda longreg1+2
    sta (current_file),y
    iny
    lda longreg1+3
    sta (current_file),y

    ; Advance position
    ; We can't use inc because no inc (zp),y
    ldy #fatfs_filedata::cluster_pos+0
    clc
    lda (current_file),y
    adc #1
    sta (current_file),y
    iny
    lda (current_file),y
    adc #0
    sta (current_file),y
    iny
    lda (current_file),y
    adc #0
    sta (current_file),y

    ; Reset offset
    ldy #fatfs_filedata::cluster_ptr+0
    lda #0
    sta (current_file),y
    iny
    sta (current_file),y
    iny
    sta (current_file),y

    clc
    rts

error:
    sec
    rts

.endproc

; Set the seek position for the file at (current_file)
; Return C set and -errno in A if error
; An error may result at end of file or when crossing a cluster boundary

.proc set_seek_position

    ldy #fatfs_filedata::current_cluster+0
    lda (current_file),y
    iny
    and (current_file),y
    iny
    and (current_file),y
    iny
    and (current_file),y
    cmp #$FF
    bne cluster_dir
        ; File is the FAT12/16 root directory
        ; Are we at the end?
        ldy #fatfs_filedata::cluster_ptr+1
        lda (current_file),y
        cmp fatfs_root_dir_bytes+0
        iny
        lda (current_file),y
        sbc fatfs_root_dir_bytes+1
        bcs end_of_file

        ; longreg1 <- fatfs_root_dir << 8
        lda fatfs_root_dir+0
        sta longreg1+1
        lda fatfs_root_dir+1
        sta longreg1+2
        lda fatfs_root_dir+2
        sta longreg1+3

        ; longreg1 += cluster_ptr
        ldy #fatfs_filedata::cluster_ptr+0
        clc
        lda (current_file),y
        sta longreg1+0
        iny
        lda longreg1+1
        adc (current_file),y
        sta longreg1+1
        iny
        lda longreg1+2
        adc (current_file),y
        sta longreg1+2
        bcc :+
            inc longreg1+3
        :

        rts

    cluster_dir:
        ; Directory is a cluster chain
        ; Are we at the end of the cluster?
        ldy #fatfs_filedata::cluster_ptr+1
        lda (current_file),y
        cmp fatfs_cluster_size
        bcc got_cluster

            ; Advance to the next cluster
            jsr next_cluster_in_file
            bcs error

        got_cluster:

        ; longreg1 <- current_cluster << (fatfs_cluster_shift+8)
        ldy #fatfs_filedata::current_cluster+0
        lda (current_file),y
        sta longreg1+1
        iny
        lda (current_file),y
        sta longreg1+2
        iny
        lda (current_file),y
        ldx fatfs_cluster_shift
        @shift:
            asl longreg1+1
            rol longreg1+2
            rol a
        dex
        bne @shift
        sta longreg1+3

        ; longreg1 += fatfs_cluster_base << 8
        clc
        lda longreg1+1
        adc fatfs_cluster_base+0
        sta longreg1+1
        lda longreg1+2
        adc fatfs_cluster_base+1
        sta longreg1+2
        lda longreg1+3
        adc fatfs_cluster_base+2
        sta longreg1+3

        ; longreg1 += cluster_ptr
        ldy #fatfs_filedata::cluster_ptr+0
        lda (current_file),y
        sta longreg1+0
        clc
        lda longreg1+1
        iny
        adc (current_file),y
        sta longreg1+1
        bcc :+
            inc longreg1+2
            bne :+
            inc longreg1+3
        :

        rts

end_of_file:
    lda #$100-ENOENT

error:
    sec
    rts


.endproc

; Advance longreg2 to the next component of the path

.proc next_component

    clc
    lda longreg2+0
    adc fatfs_component_len
    sta longreg2+0
    bcc :+
        inc longreg2+1
    :
    rts

.endproc

; Advance longreg2 to the next byte that is not a slash

.proc skip_slashes

    ldy #0
    @slash:
        lda (longreg2),y
        cmp #'/'
        bne @end_slash
    inc longreg2+0
    bne @slash
    inc longreg2+1
    bne @slash
    @end_slash:
    rts

.endproc

; Return Z set if the file name segment at longreg2+Y matches the long name
; record in fatfs_file_data

.proc long_name_matches

    name1 = scratch_area+0
    name2 = scratch_area+14

    ; Copy the name segment at longreg2+Y to name1
    ldx #0
    @copy1:
        lda (longreg2),y
        beq @end_copy1
        cmp #'/'
        beq @end_copy1
        sta name1,x
    iny
    inx
    cpx #13
    bcc @copy1
    @end_copy1:
    lda #0
    sta name1,x

    ; Copy the name segment in the record to name2
    ldx #0
    ldy #0
    @copy2a:
        ; The name is taken as ISO-8859-1; names on disk outside that character
        ; set never match
        lda fatfs_file_data+long_name::LDIR_Name1+1,x
        bne no_match
        lda fatfs_file_data+long_name::LDIR_Name1+0,x
        ; U+0000 marks the end of the name
        beq @end_copy2
        sta name2,y
    inx
    inx
    iny
    cpy #5
    bcc @copy2a
    ldx #0
    @copy2b:
        lda fatfs_file_data+long_name::LDIR_Name2+1,x
        bne no_match
        lda fatfs_file_data+long_name::LDIR_Name2+0,x
        beq @end_copy2
        sta name2,y
    inx
    inx
    iny
    cpy #11
    bcc @copy2b
    ldx #0
    @copy2c:
        lda fatfs_file_data+long_name::LDIR_Name3+1,x
        bne no_match
        lda fatfs_file_data+long_name::LDIR_Name3+0,x
        beq @end_copy2
        sta name2,y
    inx
    inx
    iny
    cpy #13
    bcc @copy2c
    @end_copy2:
    lda #0
    sta name2,y

    ; Convert to upper case
    ldx #12
    @upper:
        lda name1,x
        jsr to_upper
        sta name1,x
        lda name2,x
        jsr to_upper
        sta name2,x
    dex
    bpl @upper

    ; Compare name1 and name2
    ldx #0
    @compare:
        lda name1,x
        cmp name2,x
        bne no_match
    inx
    cmp #0
    bne @compare

match:
    lda #$00
    rts

no_match:
    lda #$FF
    rts

.endproc

; Return Z set if the file name component at longreg2 matches the short name
; record in fatfs_file_data

.proc short_name_matches

    name1 = scratch_area+0

    ; Fill name1 with spaces
    ldx #10
    lda #' '
    @spaces:
        sta name1,x
    dex
    bpl @spaces

    ; Copy name up to '.' or end
    ldy #0
    @copy1:
        lda (longreg2),y
        beq end_name
        cmp #'/'
        beq end_name
        cmp #'.'
        beq copy_ext
        sta name1,y
    iny
    cpy #8
    bcc @copy1

    ; Eight characters copied; next must be '.', '/' or end
    lda (longreg2),y
    beq end_name
    cmp #'/'
    beq end_name
    cmp #'.'
    bne no_match ; 9 or more characters before first '.'

copy_ext:
    ; Copy extension up to end
    iny
    ldx #0
    @copy2:
        lda (longreg2),y
        beq end_name
        cmp #'/'
        beq end_name
        cmp #'.'
        beq no_match ; two '.' characters
        sta name1+8,x
    iny
    inx
    cpx #3
    bcc @copy2
    bcs no_match ; 4 or more characters after '.'

end_name:

    ; Convert to upper case and compare to the record
    ldx #10
    @upper:
        lda name1,x
        jsr to_upper
        cmp fatfs_file_data,x
        bne no_match
    dex
    bpl @upper

match:
    lda #0
    rts

no_match:
    lda #$FF
    rts

.endproc

; Convert ISO 8859-1 character in A to upper case
; Does not alter X or Y

.proc to_upper

    ; Use numeric values to avoid conversion to PETSCII
    cmp #$61    ; 'a'
    bcc no_change
    cmp #$7B    ; 'z'+1
    bcc sub_32
    cmp #$E0    ; LATIN SMALL LETTER A WITH GRAVE
    bcc no_change
    cmp #$E7    ; LATIN SMALL LETTER O WITH DIAERESIS + 1
    bcc sub_32
    cmp #$F8    ; LATIN SMALL LETTER O WITH STROKE
    bcc no_change
    cmp #$FF    ; LATIN SMALL LETTER THORN + 1
    bcc sub_32

no_change:
    rts

sub_32:
    and #$D0
    rts

.endproc

; 

; From the short name record in fatfs_file_data, compute the checksum for
; the short name
; Return the checksum in A

.proc short_name_checksum

    ldx #0
    txa
    @loop:
        ; Rotate right
        lsr a
        bcc :+
            ora #$80
        :
        ; Add the byte from the name
        clc
        adc fatfs_file_data,x
    inx
    cpx #11
    bcc @loop

    rts

.endproc

; Set the file indicated by current_file to its first cluster

.proc current_file_to_first_cluster

    ; current_cluster <- cluster_hi:cluster_lo

    ldy #fatfs_filedata::cluster_lo+0
    lda (current_file),y
    tax
    iny
    lda (current_file),y
    ldy #fatfs_filedata::current_cluster+1
    sta (current_file),y
    txa
    dey
    sta (current_file),y
    ldy #fatfs_filedata::cluster_hi+0
    lda (current_file),y
    tax
    iny
    lda (current_file),y
    ldy #fatfs_filedata::current_cluster+3
    sta (current_file),y
    txa
    dey
    sta (current_file),y

    ; cluster_pos <- 0

    lda #0
    ldy #fatfs_filedata::cluster_pos+0
    sta (current_file),y
    iny
    sta (current_file),y
    iny
    sta (current_file),y

    rts

.endproc

; Rewind file indicated by current_file

.proc rewind_current_file

    jsr current_file_to_first_cluster

    ; cluster_ptr <- 0

    ldy #fatfs_filedata::cluster_ptr+0
    sta (current_file),y
    iny
    sta (current_file),y
    iny
    sta (current_file),y

    rts

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Query the current directory                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Read the current directory
; pointer1 points to an array of 256 bytes to receive the path
; On successful return: pointer1 is 0
; On error: pointer1 is set to -errno

.proc fatfs_get_current_dir

    ; dos_read uses pointer1, pointer2 and pointer3
    ; dos_seek uses longreg1 and pointer1
    ; Use pointer4 to point to the output buffer
    path_ptr = pointer4

    path_len = scratch_area+0
    path_count = scratch_area+1
    path_next = scratch_area+2

    ; path_ptr <- pointer1
    lda pointer1+0
    sta path_ptr+0
    lda pointer1+1
    sta path_ptr+1

    ; Check for the current directory being the root
    lda fatfs_current_dir_num_components
    bne :+
        ldy #0
        lda #'/'
        sta (path_ptr),y
        iny
        lda #0
        sta (path_ptr),y
        sta pointer1+0
        sta pointer1+1
        rts
    :

    ; TODO: first try retrieving the long names

do_short_name:

    ; path_count <- 0 and path_len <- 0
    lda #0
    sta path_count
    sta path_len

    short_name_loop:

        ; Seek to the short name record
        ldx path_count
        ldy lshift3,x
        lda fatfs_current_dir+fatfs_dir_entry::dir_entry+0,y
        sta longreg1+0
        lda fatfs_current_dir+fatfs_dir_entry::dir_entry+1,y
        sta longreg1+1
        lda fatfs_current_dir+fatfs_dir_entry::dir_entry+2,y
        sta longreg1+2
        lda fatfs_current_dir+fatfs_dir_entry::dir_entry+3,y
        sta longreg1+3
        jsr dos_seek
        bcs error

        ; Read into memory
        lda #<fatfs_file_data
        sta pointer2+0
        lda #>fatfs_file_data
        sta pointer2+1
        lda #32
        sta pointer3+0
        lda #0
        sta pointer3+1
        jsr dos_read
        bcs error

        ; Add a slash to the path
        ldy path_len
        lda #'/'
        sta (path_ptr),y
        iny

        ; If first byte is $05, convert to $E5; otherwise, pass as is
        lda fatfs_file_data+0
        cmp #$05
        bne :+
            lda #$E5
        :
        sta (path_ptr),y
        iny

        ; Pass seven more bytes as they are
        ldx #1
        @copy_name:
            lda fatfs_file_data,x
            sta (path_ptr),y
        iny
        inx
        cpx #8
        bcc @copy_name

        ; Trim any trailing spaces
        @trim_spaces_1:
            dey
            lda (path_ptr),y
            cmp #' '
        beq @trim_spaces_1
        iny

        ; Add a period to the path
        lda #'.'
        sta (path_ptr),y
        iny

        ; Pass three more bytes as they are
        @copy_ext:
            lda fatfs_file_data,x
            sta (path_ptr),y
        iny
        inx
        cpx #11
        bcc @copy_ext

        ; Trim any trailing spaces
        @trim_spaces_2:
            dey
            lda (path_ptr),y
            cmp #' '
        beq @trim_spaces_2
        ; Trim the period if that is the last character
        cmp #'.'
        beq :+
            iny
        :

        sty path_len

    inc path_count
    lda path_count
    cmp fatfs_current_dir_num_components
    jcc short_name_loop

end_path:
    ; Place a null character at the end and return success

    lda #0
    ldy path_len
    sta (path_ptr),y
    sta pointer1+0
    sta pointer1+1
    rts

error:
    lda #$100-EIO
    sta pointer1+0
    lda #$FF
    sta pointer1+1
    rts

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Seek to a given position                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Seek to a position
; pointer1 contains the index of the file block
; pointer2 contains 0 (SEEK_SET), 1 (SEEK_CUR) or 2 (SEEK_END)
; longreg1 contains the file offset
; On successful return: pointer1 is 0, and longreg1 contains the previous
;     file offset
; On error: pointer1 is set to -errno

.proc fatfs_seek

    ; Get the pointer to the file block
    jsr set_current_file
    bcc :+
    error:
        sta pointer1+0
        lda #$FF
        sta pointer1+1
        rts
    :

    ; fatfs_seek_offset <- longreg1
    ; Do this first, to clear longreg1 for set_seek_position
    lda longreg1+0
    sta fatfs_seek_offset+0
    lda longreg1+1
    sta fatfs_seek_offset+1
    lda longreg1+2
    sta fatfs_seek_offset+2
    lda longreg1+3
    sta fatfs_seek_offset+3

    ; Compute the target offset
    ldx pointer2+0
    bne check_1
        ; Offset is from the start of the file
        ; Consider the offset to be positive
        lda #0
        sta fatfs_seek_offset+4
        beq seek1

    check_1:
    dex
    bne check_2

        ; Offset is from the current position
        jsr set_seek_offset
        ; Consider the offset to be signed
        lda fatfs_seek_offset+3
        asl a
        lda #0
        sbc #0
        eor #$FF
        sta fatfs_seek_offset+4
        ; Consider the file position to be unsigned
        clc
        lda longreg1+0
        adc fatfs_seek_offset+0
        sta fatfs_seek_offset+0
        lda longreg1+1
        adc fatfs_seek_offset+1
        sta fatfs_seek_offset+1
        lda longreg1+2
        adc fatfs_seek_offset+2
        sta fatfs_seek_offset+2
        lda longreg1+3
        adc fatfs_seek_offset+3
        sta fatfs_seek_offset+3
        lda #0
        adc fatfs_seek_offset+4
        sta fatfs_seek_offset+4
        jmp seek1

    check_2:
    dex
    bne einval

        ; Offset is from the end of the file
        ; If the file is a directory, we don't have a length; we can't seek
        ; from the end
        ldy #fatfs_filedata::attributes
        lda (current_file),y
        and #ATTR_DIRECTORY
        bne einval

        ; Consider the offset to be negative
        clc
        ldy #fatfs_filedata::size
        lda (current_file),y
        adc fatfs_seek_offset+0
        sta fatfs_seek_offset+0
        iny
        lda (current_file),y
        adc fatfs_seek_offset+1
        sta fatfs_seek_offset+1
        iny
        lda (current_file),y
        adc fatfs_seek_offset+2
        sta fatfs_seek_offset+2
        iny
        lda (current_file),y
        adc fatfs_seek_offset+3
        sta fatfs_seek_offset+3
        lda #0
        adc #$FF
        sta fatfs_seek_offset+4

    seek1:

    ; Check that the seek location is betwen 0 and $FFFFFFFF
    lda fatfs_seek_offset+4
    bne einval

    ; If the file is a directory, the seek location must be a multiple of 32
    ldy #fatfs_filedata::attributes
    lda (current_file),y
    and #ATTR_DIRECTORY
    beq offset_ok_1
        lda fatfs_seek_offset+0
        and #$1F
        bne einval
    offset_ok_1:

    ; Is the file the FAT12/16 root directory?
    ldy #fatfs_filedata::cluster_lo
    lda (current_file),y
    iny
    and (current_file),y
    ldy #fatfs_filedata::cluster_hi
    and (current_file),y
    iny
    and (current_file),y
    cmp #$FF
    bne cluster_chain

        ; Check that the seek location is within the directory
        lda fatfs_seek_offset+3
        bne einval
        lda fatfs_root_dir_bytes+0
        cmp fatfs_seek_offset+1
        lda fatfs_root_dir_bytes+1
        sbc fatfs_seek_offset+2
        bcs :+
        einval:
            ; An invalid parameter was passed
            lda #$100-EINVAL
            sta pointer1+0
            lda #$FF
            sta pointer1+1
            rts
        :

        ; Set the offset and return the current offset
        ldy #fatfs_filedata::cluster_ptr
        lda (current_file),y
        sta longreg1+0
        lda fatfs_seek_offset+0
        sta (current_file),y
        iny
        lda fatfs_seek_offset+1
        lda (current_file),y
        sta longreg1+1
        sta (current_file),y
        iny
        lda fatfs_seek_offset+2
        lda (current_file),y
        sta longreg1+2
        sta (current_file),y
        lda #0
        sta longreg1+3

        jmp success

    cluster_chain:
        ; File is a cluster chain
        ; Check that the seek location is within the file
        ; This check is not available for directories
        ldy #fatfs_filedata::attributes
        lda (current_file),y
        and #ATTR_DIRECTORY
        bne offset_ok

            ldy #fatfs_filedata::size
            lda (current_file),y
            cmp fatfs_seek_offset+0
            iny
            lda (current_file),y
            sbc fatfs_seek_offset+1
            iny
            lda (current_file),y
            sbc fatfs_seek_offset+2
            iny
            lda (current_file),y
            sbc fatfs_seek_offset+3
            bcc einval

        offset_ok:

        ; Save the current position, for restore in case of error and to return
        ; the previous position
        ldy #fatfs_filedata::current_cluster
        lda (current_file),y
        sta longreg2+0
        iny
        lda (current_file),y
        sta longreg2+1
        iny
        lda (current_file),y
        sta longreg2+2
        iny
        lda (current_file),y
        sta longreg2+3
        ldy #fatfs_filedata::cluster_pos
        lda (current_file),y
        sta longreg1+1
        iny
        lda (current_file),y
        sta longreg1+2
        iny
        lda (current_file),y
        sta longreg1+3

        ; Get the cluster position to which we seek
        lda fatfs_seek_offset+1
        sta fatfs_seek_cluster_count+0
        lda fatfs_seek_offset+2
        sta fatfs_seek_cluster_count+1
        lda fatfs_seek_offset+3
        ldx fatfs_cluster_shift
        @shift:
            lsr a
            ror fatfs_seek_cluster_count+1
            ror fatfs_seek_cluster_count+0
        dex
        bne @shift
        sta fatfs_seek_cluster_count+2

        ; Is this less than the current position?

        lda fatfs_seek_cluster_count+0
        cmp longreg1+1
        lda fatfs_seek_cluster_count+1
        sbc longreg1+2
        lda fatfs_seek_cluster_count+2
        sbc longreg1+3
        bcc backwards

            ; New cluster is forwards or the same
            ; Reduce count to the number of clusters we have to step through
            sec
            lda fatfs_seek_cluster_count+0
            sbc longreg1+1
            sta fatfs_seek_cluster_count+0
            lda fatfs_seek_cluster_count+1
            sbc longreg1+2
            sta fatfs_seek_cluster_count+1
            lda fatfs_seek_cluster_count+2
            sbc longreg1+3
            sta fatfs_seek_cluster_count+2

        jmp seek2
        backwards:

            ; New cluster is less
            ; Begin step from the start
            jsr current_file_to_first_cluster

        seek2:

        ; Check the cluster count for zero
        lda fatfs_seek_cluster_count+0
        ora fatfs_seek_cluster_count+1
        ora fatfs_seek_cluster_count+2
        beq seek_done

        ; Negate the cluster count for easier looping
        sec
        lda #0
        sbc fatfs_seek_cluster_count+0
        sta fatfs_seek_cluster_count+0
        lda #0
        sbc fatfs_seek_cluster_count+1
        sta fatfs_seek_cluster_count+1
        lda #0
        sbc fatfs_seek_cluster_count+2
        sta fatfs_seek_cluster_count+2

        ; Loop until target cluster reached

        seek_loop:
            jsr next_cluster_in_file
            bcs restore_and_error
        inc fatfs_seek_cluster_count+0
        bne seek_loop
        inc fatfs_seek_cluster_count+1
        bne seek_loop
        inc fatfs_seek_cluster_count+2
        bne seek_loop

        seek_done:

        ; Set longreg1 to the previous position
        ldx fatfs_cluster_shift
        lda longreg1+3
        @shift:
            asl longreg1+1
            rol longreg1+2
            rol a
        dex
        bne @shift
        sta longreg1+3
        ldy #fatfs_filedata::cluster_ptr
        clc
        lda (current_file),y
        sta longreg1+0
        iny
        lda (current_file),y
        adc longreg1+1
        sta longreg1+1
        bcc :+
            inc longreg1+2
            bne :+
            inc longreg1+3
        :

        ; Set the offset
        ldy #fatfs_filedata::cluster_ptr
        lda fatfs_seek_offset+0
        sta (current_file),y
        iny
        ldx fatfs_cluster_size
        dex
        txa
        and fatfs_seek_offset+1
        sta (current_file),y
        iny
        lda #0
        sta (current_file),y

    success:
        ; Seek is complete
        lda #0
        sta pointer1+0
        sta pointer1+1
        rts

    restore_and_error:
        ; Error while seeking through the cluster chain
        sta pointer1+0
        lda #$FF
        sta pointer1+1

        ; Restore the file position
        ldy #fatfs_filedata::current_cluster
        lda longreg2+0
        sta (current_file),y
        iny
        lda longreg2+1
        sta (current_file),y
        iny
        lda longreg2+2
        sta (current_file),y
        iny
        lda longreg2+3
        sta (current_file),y
        ldy #fatfs_filedata::cluster_pos
        lda longreg1+1
        sta (current_file),y
        iny
        lda longreg1+2
        sta (current_file),y
        iny
        lda longreg1+3
        sta (current_file),y

        rts

.endproc

; Set fatfs_seek_offset to the current position of (current_file)
.proc set_seek_offset

    ; Is the file a root directory?
    ldy #fatfs_filedata::current_cluster
    lda (current_file),y
    iny
    and (current_file),y
    iny
    and (current_file),y
    iny
    and (current_file),y
    cmp #$FF
    bne cluster_chain

        ; Position is simply the offset into the root directory
        ldy #fatfs_filedata::cluster_pos
        lda (current_file),y
        sta fatfs_seek_offset+0
        iny
        lda (current_file),y
        sta fatfs_seek_offset+1
        iny
        lda (current_file),y
        sta fatfs_seek_offset+2
        rts

    cluster_chain:

        ; File is a cluster chain
        ; Set cluster_index << (fatfs_cluster_shift+8)

        ldy #fatfs_filedata::cluster_pos
        lda (current_file),y
        sta fatfs_seek_offset+1
        iny
        lda (current_file),y
        sta fatfs_seek_offset+2
        iny
        lda (current_file),y
        ldx fatfs_cluster_shift
        @shift:
            asl fatfs_seek_offset+1
            rol fatfs_seek_offset+2
            rol a
        dex
        bne @shift
        sta fatfs_seek_offset+3

        ; Add the cluster offset

        ldy #fatfs_filedata::cluster_ptr
        lda (current_file),y
        sta fatfs_seek_offset+0
        clc
        iny
        lda (current_file),y
        adc fatfs_seek_offset+1
        sta fatfs_seek_offset+1
        bcc :+
        inc fatfs_seek_offset+2
        bne :+
        inc fatfs_seek_offset+3
        :
        rts

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Read to main memory                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Read a file to main memory
; pointer1 contains the index of the file block
; pointer2 contains the address to receive the data
; pointer3 contains the size of the transfer
; On successful return: pointer1 is 0, and pointer3 contains the actual size
;     transferred
; On error: pointer1 contains -errno

.proc fatfs_read_mem

    ; Get the pointer to the file block
    jsr set_current_file
    bcc :+
    error:
        sta pointer1+0
        lda #$FF
        sta pointer1+1
        rts
    :

    ; fatfs_xfer_addr <- pointer2
    lda pointer2+0
    sta fatfs_xfer_addr+0
    lda pointer2+1
    sta fatfs_xfer_addr+1
    lda #0
    sta fatfs_xfer_addr+2
    sta fatfs_xfer_addr+3

    ; fatfs_remaining_size <- pointer3
    lda pointer3+0
    sta fatfs_remaining_size+0
    lda pointer3+1
    sta fatfs_remaining_size+1
    lda #0
    sta fatfs_remaining_size+2
    sta fatfs_remaining_size+3

    ; fatfs_total_size <- 0
    lda #0
    sta fatfs_total_size+0
    sta fatfs_total_size+1
    sta fatfs_total_size+2
    sta fatfs_total_size+3

    ; Repeat until end of file or fatfs_remaining_size == 0
    jmp check_xfer_loop
    xfer_loop:

        ; Set up read size
        jsr set_read_size
        bcc size_ok
            ; ENOENT means end of file; return other errors
            cmp #$100-ENOENT
            beq end_xfer_loop
            bne error
        size_ok:

        ; Seek to the position to be read
        jsr set_seek_position
        bcs error
        jsr dos_seek
        bcs error

        ; Read from the volume
        lda fatfs_xfer_addr+0
        sta pointer2+0
        lda fatfs_xfer_addr+1
        sta pointer2+1
        lda fatfs_xfer_size+0
        sta pointer3+0
        lda fatfs_xfer_size+1
        sta pointer3+1
        jsr dos_read
        bcs error

        ; Advance the positions and the sizes
        jsr advance_xfer

    check_xfer_loop:
    lda fatfs_remaining_size+0
    ora fatfs_remaining_size+1
    jne xfer_loop
    end_xfer_loop:

    ; Return the total size transferred
    lda fatfs_total_size+0
    sta pointer3+0
    lda fatfs_total_size+1
    sta pointer3+1

    ; Indicate success
    lda #0
    sta pointer1+0
    sta pointer1+1
    rts

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Read to the REU                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Read a file to the REU
; pointer1 contains the index of the file block
; longreg1 contains the address to receive the data
; longreg2 contains the size of the transfer
; On successful return: pointer1 is 0, and pointer3 contains the actual size
;     transferred
; On error: pointer1 contains -errno

.proc fatfs_read

    ; Get the pointer to the file block
    jsr set_current_file
    bcc :+
    error:
        sta pointer1+0
        lda #$FF
        sta pointer1+1
        rts
    :

    ; fatfs_xfer_addr <- longreg1
    lda longreg1+0
    sta fatfs_xfer_addr+0
    lda longreg1+1
    sta fatfs_xfer_addr+1
    lda longreg1+2
    sta fatfs_xfer_addr+2
    lda longreg1+3
    sta fatfs_xfer_addr+3

    ; fatfs_remaining_size <- longreg2
    lda longreg2+0
    sta fatfs_remaining_size+0
    lda longreg2+1
    sta fatfs_remaining_size+1
    lda longreg2+2
    sta fatfs_remaining_size+2
    lda longreg2+3
    sta fatfs_remaining_size+3

    ; fatfs_total_size <- 0
    lda #0
    sta fatfs_total_size+0
    sta fatfs_total_size+1
    sta fatfs_total_size+2
    sta fatfs_total_size+3

    ; Repeat until end of file or fatfs_remaining_size == 0
    jmp check_xfer_loop
    xfer_loop:

        ; Set up read size
        jsr set_read_size
        bcc size_ok
            ; ENOENT means end of file; return other errors
            cmp #$100-ENOENT
            beq end_xfer_loop
            bne error
        size_ok:

        ; Seek to the position to be read
        jsr set_seek_position
        bcs error
        jsr dos_seek
        error0:
        bcs error

        ; Read from the volume
        lda fatfs_xfer_addr+0
        sta longreg1+0
        lda fatfs_xfer_addr+1
        sta longreg1+1
        lda fatfs_xfer_addr+2
        sta longreg1+2
        lda fatfs_xfer_addr+3
        sta longreg1+3
        lda fatfs_xfer_size+0
        sta longreg2+0
        lda fatfs_xfer_size+1
        sta longreg2+1
        lda #0
        sta longreg2+2
        lda #0
        sta longreg2+3
        jsr dos_read_reu
        bcs error0

        ; Advance the positions and the sizes
        jsr advance_xfer

    check_xfer_loop:
    lda fatfs_remaining_size+0
    ora fatfs_remaining_size+1
    ora fatfs_remaining_size+2
    ora fatfs_remaining_size+3
    jne xfer_loop
    end_xfer_loop:

    ; Return the total size transferred
    lda fatfs_total_size+0
    sta pointer3+0
    lda fatfs_total_size+1
    sta pointer3+1

    ; Indicate success
    lda #0
    sta pointer1+0
    sta pointer1+1
    rts

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Common routines for read and write                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Set the size to read in one pass
; Shared between fatfs_read_mem and fatfs_read

.proc set_read_size

    ; Size remaining in the current cluster
    sec
    ldy #fatfs_filedata::cluster_ptr
    lda #0
    sbc (current_file),y
    sta fatfs_xfer_size+0
    iny
    lda fatfs_cluster_size
    sbc (current_file),y
    sta fatfs_xfer_size+1

    ; If that is zero, we need the next cluster
    ora fatfs_xfer_size+0
    bne check_eof
        ; Set transfer size to cluster size
        lda fatfs_cluster_size
        sta fatfs_xfer_size+1
        ; Advance to next cluster
        jsr next_cluster_in_file
        bcs error
    check_eof:

    ; Check for reading the last cluster
    ldy #fatfs_filedata::cluster_count+0
    lda (current_file),y
    ldy #fatfs_filedata::cluster_pos+0
    cmp (current_file),y
    ldy #fatfs_filedata::cluster_count+1
    lda (current_file),y
    ldy #fatfs_filedata::cluster_pos+1
    sbc (current_file),y
    ldy #fatfs_filedata::cluster_count+2
    lda (current_file),y
    ldy #fatfs_filedata::cluster_pos+2
    sbc (current_file),y
    bcs size_ok
        ; Set the size of the partial cluster at the end
        ldy #fatfs_filedata::cluster_partial
        lda (current_file),y
        sta fatfs_xfer_size+0
        iny
        lda (current_file),y
        sta fatfs_xfer_size+1
        ; Check for EOF
        ora fatfs_xfer_size+0
        beq end_xfer_loop
    size_ok:

    ; Check that this does not exceed the remaining size
    lda fatfs_remaining_size+2
    ora fatfs_remaining_size+3
    bne size_ok_2
    lda fatfs_xfer_size+0
    cmp fatfs_remaining_size+0
    lda fatfs_xfer_size+1
    sbc fatfs_remaining_size+1
    bcc size_ok_2
        lda fatfs_remaining_size+0
        sta fatfs_xfer_size+0
        lda fatfs_remaining_size+1
        sta fatfs_xfer_size+1
    size_ok_2:

    clc
    rts

end_xfer_loop:
    lda #$100-ENOENT
error:
    sec
    rts

.endproc

; Advance the transfer address, the sizes and the file position
; Shared among fatfs_read_mem, fatfs_read and fatfs_write

.proc advance_xfer

    ; Advance the address
    clc
    lda fatfs_xfer_addr+0
    adc fatfs_xfer_size+0
    sta fatfs_xfer_addr+0
    lda fatfs_xfer_addr+1
    adc fatfs_xfer_size+1
    sta fatfs_xfer_addr+1
    lda fatfs_xfer_addr+2
    adc #0
    sta fatfs_xfer_addr+2
    lda fatfs_xfer_addr+3
    adc #0
    sta fatfs_xfer_addr+3

    ; Advance the total_size
    clc
    lda fatfs_total_size+0
    adc fatfs_xfer_size+0
    sta fatfs_total_size+0
    lda fatfs_total_size+1
    adc fatfs_xfer_size+1
    sta fatfs_total_size+1
    lda fatfs_total_size+2
    adc #0
    sta fatfs_total_size+2
    lda fatfs_total_size+3
    adc #0
    sta fatfs_total_size+3

    ; Advance the file position
    clc
    ldy #fatfs_filedata::cluster_ptr
    lda (current_file),y
    adc fatfs_xfer_size+0
    sta (current_file),y
    iny
    lda (current_file),y
    adc fatfs_xfer_size+1
    sta (current_file),y

    ; Reduce the remaining size
    sec
    lda fatfs_remaining_size+0
    sbc fatfs_xfer_size+0
    sta fatfs_remaining_size+0
    lda fatfs_remaining_size+1
    sbc fatfs_xfer_size+1
    sta fatfs_remaining_size+1
    lda fatfs_remaining_size+2
    sbc #0
    sta fatfs_remaining_size+2
    lda fatfs_remaining_size+3
    sbc #0
    sta fatfs_remaining_size+3

    rts

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Access to the file allocation table                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Set up the FAT cache

.proc fat_init

    lda #0
    sta fatfs_fat_dirty
    lda #$FF
    sta fatfs_fat_begin+0
    sta fatfs_fat_begin+1
    sta fatfs_fat_begin+2
    sta fatfs_fat_begin+3
    rts

.endproc

; Given the cluster number in longreg1, read the FAT entry.
; If the FAT entry is read: return C clear and the data in longreg1.
; If cluster_num is out of range: Return C set and ENOENT in A.
; If I/O error: Return C set and other error in A.

.proc read_fat_entry

    ; Check validity of the cluster number
    lda longreg1+0
    cmp #2
    lda longreg1+1
    sbc #0
    lda longreg1+2
    sbc #0
    lda longreg1+3
    sbc #0
    bcc bad_cluster

    lda longreg1+0
    cmp fatfs_clusters+0
    lda longreg1+1
    sbc fatfs_clusters+1
    lda longreg1+2
    sbc fatfs_clusters+2
    lda longreg1+3
    sbc #0
    bcc good_cluster

bad_cluster:
    lda #$100-ENOENT
    sec
    rts

good_cluster:

    ; Follow according to the file system type
    lda fatfs_type
    cmp #12
    bne check_fat_16

        ; File system is FAT12

        ; cluster_num + (cluster_num >> 1)
        lda longreg1+1
        lsr a
        sta fatfs_fat_seek_pos+1
        lda longreg1+0
        ror a
        php         ; Save bit 0 as C
        sta fatfs_fat_seek_pos+0
        clc
        lda longreg1+0
        adc fatfs_fat_seek_pos+0
        sta fatfs_fat_seek_pos+0
        lda longreg1+1
        adc fatfs_fat_seek_pos+1
        sta fatfs_fat_seek_pos+1
        lda #0
        sta fatfs_fat_seek_pos+2
        sta fatfs_fat_seek_pos+3

        ; If the entry is not in memory, read it
        jsr read_fat_12_16
        bcc :+
            plp
            sec
            rts
        :

        ; Select entry according to bit 0 of cluster number
        lda #0
        sta longreg1+2
        sta longreg1+3
        plp         ; Retrieve saved bit 0
        bcs odd_cluster
            ; Even cluster number
            stx longreg1+0
            tya
            and #$0F
            sta longreg1+1
            clc
            rts
        odd_cluster:
            ; Odd cluster number
            lda rshift4,x
            ora lshift4,y
            sta longreg1+0
            lda rshift4,y
            sta longreg1+1
            clc
            rts

    check_fat_16:
    cmp #16
    bne check_fat_32

        ; File system is FAT16

        ; cluster_num << 1
        lda longreg1+0
        asl a
        sta fatfs_fat_seek_pos+0
        lda longreg1+1
        rol a
        sta fatfs_fat_seek_pos+1
        lda #0
        sta fatfs_fat_seek_pos+3
        rol a
        sta fatfs_fat_seek_pos+2

        ; Read this entry from the volume
        jsr read_fat_12_16
        bcc :+
            rts
        :

        ; Return the FAT entry
        stx longreg1+0
        sty longreg1+1
        lda #0
        sta longreg1+2
        sta longreg1+3
        clc
        rts

    check_fat_32:
    cmp #32
    bne io_error

        ; cluster_num << 2
        ldx longreg1+0
        lda lshift2,x
        sta fatfs_fat_seek_pos+0
        lda rshift6,x
        ldx longreg1+1
        ora lshift2,x
        sta fatfs_fat_seek_pos+1
        lda rshift6,x
        ldx longreg1+2
        ora lshift2,x
        sta fatfs_fat_seek_pos+2
        lda rshift6,x
        ldx longreg1+3
        ora lshift2,x
        sta fatfs_fat_seek_pos+3

        ; Read the entry from the volume
        jsr read_fat_32

        rts

    io_error:
        lda #$100-EIO
        sec
        rts

.endproc

; fatfs_fat_seek_pos contains the offset of a FAT entry on a FAT12 or FAT16
; volume. If that is not in memory, read it.
; Then return two bytes from that position: the first in X and the second in
; Y.

.proc read_fat_12_16

    ; Read the requested block

    jsr read_fat_cache
    bcs error

    ; Read two bytes

    sec
    lda fatfs_fat_seek_pos+0
    sbc fatfs_fat_begin+0
    tay
    lda fatfs_fat_cache+0,y
    tax
    lda fatfs_fat_cache+1,y
    tay
    clc
    rts

error:
    sec
    rts

.endproc

; fatfs_fat_seek_pos contains the offset of a FAT entry on a FAT32 volume.
; If that is not in memory, read it.
; Then return four bytes from that position in longreg1.

.proc read_fat_32

    ; Read the requested block

    jsr read_fat_cache
    bcs error

    ; Read four bytes

    sec
    lda fatfs_fat_seek_pos+0
    sbc fatfs_fat_begin+0
    tay
    lda fatfs_fat_cache+0,y
    sta longreg1+0
    lda fatfs_fat_cache+1,y
    sta longreg1+1
    lda fatfs_fat_cache+2,y
    sta longreg1+2
    lda fatfs_fat_cache+3,y
    sta longreg1+3
    clc
    rts

error:
    sec
    rts

.endproc

; Write the current FAT sector to the volume if it has been changed
; Return C set and -errno in A if error

.proc flush_fat_cache

    ; Has the FAT been changed?

    lda fatfs_fat_dirty
    bne :+
        clc
        rts
    :

    ; fatfs_num_fats  fatfs_ext_flags  first FAT  second FAT
    ;       1               X             yes        no
    ;       2               00            yes        yes
    ;       2               80            yes        no
    ;       2               81            no         yes

    lda fatfs_num_fats
    cmp #2
    bcs two_fats

        jsr write_first_fat
        bcs error
        bcc ok

    two_fats:
    lda fatfs_ext_flags
    bmi one_fat

        jsr write_first_fat
        bcs error
        jsr write_second_fat
        bcs error
        bcc ok

    one_fat:
    cmp #$81
    bcs second_fat

        jsr write_first_fat
        bcs error
        bcc ok

    second_fat:

        jsr write_second_fat
        bcs error

ok:
    lda #0
    sta fatfs_fat_dirty
    clc
    rts

error:
    sec
    rts

.endproc

; Write the cache to the first FAT

.proc write_first_fat

    ; Seek to the location to write
    clc
    lda fatfs_fat_begin+0
    sta longreg1+0
    lda fatfs_fat_begin+1
    adc fatfs_first_fat+0
    sta longreg1+1
    lda fatfs_fat_begin+2
    adc fatfs_first_fat+1
    sta longreg1+2
    lda fatfs_fat_begin+3
    adc fatfs_first_fat+2
    sta longreg1+3
    jsr dos_seek
    bcs error

    ; Write the cache
    lda #<fatfs_fat_cache
    sta pointer2+0
    lda #>fatfs_fat_cache
    sta pointer2+1
    lda #<256
    sta pointer3+0
    lda #>256
    sta pointer3+1
    jmp dos_write

error:
    sec
    rts

.endproc

; Write the cache to the second FAT

.proc write_second_fat

    ; Seek to the location to write
    clc
    lda fatfs_fat_begin+0
    sta longreg1+0
    lda fatfs_fat_begin+1
    adc fatfs_second_fat+0
    sta longreg1+1
    lda fatfs_fat_begin+2
    adc fatfs_second_fat+0
    sta longreg1+2
    lda fatfs_fat_begin+3
    adc fatfs_second_fat+0
    sta longreg1+3
    jsr dos_seek
    bcs error

    ; Write the cache
    lda #<fatfs_fat_cache
    sta pointer2+0
    lda #>fatfs_fat_cache
    sta pointer2+1
    lda #<256
    sta pointer3+0
    lda #>256
    sta pointer3+1
    jmp dos_write

error:
    sec
    rts

.endproc

; Read from the FAT into the cache
; Return C set and -errno in A if error

.proc read_fat_cache

    ; Is the requested position already in memory?
    ; Check for fatfs_fat_seek_pos - fatfs_fat_begin <= 254, to guarantee
    ; two bytes in memory
    ; If FAT32, four bytes are needed; but fatfs_fat_seek_pos is a multiple
    ; of four, so will be no greater than 252, and four bytes will be
    ; available

    lda fatfs_fat_begin+0
    and fatfs_fat_begin+1
    and fatfs_fat_begin+2
    and fatfs_fat_begin+3
    cmp #$FF
    beq need_to_read

    sec
    lda fatfs_fat_seek_pos+0
    sbc fatfs_fat_begin+0
    tax
    lda fatfs_fat_seek_pos+1
    sbc fatfs_fat_begin+1
    bne need_to_read
    lda fatfs_fat_seek_pos+2
    sbc fatfs_fat_begin+2
    bne need_to_read
    lda fatfs_fat_seek_pos+3
    sbc fatfs_fat_begin+3
    bne need_to_read
    cpx #255
    bcs need_to_read

        ; Already in memory
        clc
        rts

    need_to_read:

    ; Flush the cache
    jsr flush_fat_cache
    bcc :+
        rts
    :

    ; Convert offset to a 128-byte boundary
    ; We use a 128-byte boundary, but the cache is 256 bytes, to handle the
    ; case where a FAT12 entry crosses a 256-byte boundary

    lda fatfs_fat_seek_pos+0
    and #$80
    sta fatfs_fat_begin+0
    lda fatfs_fat_seek_pos+1
    sta fatfs_fat_begin+1
    lda fatfs_fat_seek_pos+2
    sta fatfs_fat_begin+2
    lda fatfs_fat_seek_pos+3
    sta fatfs_fat_begin+3

    ; Don't read past the end of the FAT
    lda fatfs_fat_begin+0
    cmp #0
    lda fatfs_fat_begin+1
    sbc fatfs_fat_size+0
    bne offset_ok
    lda fatfs_fat_begin+2
    sbc fatfs_fat_size+1
    bne offset_ok
    lda fatfs_fat_begin+3
    sbc fatfs_fat_size+2
    bne offset_ok
        sec
        lda #0
        sta fatfs_fat_begin+0
        lda fatfs_fat_size+0
        sbc #1
        sta fatfs_fat_begin+1
        lda fatfs_fat_size+1
        sbc #0
        sta fatfs_fat_begin+2
        lda fatfs_fat_size+2
        sbc #0
        sta fatfs_fat_begin+3
    offset_ok:

    ; Are we using the first or the second FAT?

    lda fatfs_ext_flags
    cmp #$81
    bcs second_fat

        ; Offset fatfs_fat_seek_pos with the first FAT
        clc
        lda fatfs_fat_begin+0
        sta longreg1+0
        lda fatfs_fat_begin+1
        adc fatfs_first_fat+0
        sta longreg1+1
        lda fatfs_fat_begin+2
        adc fatfs_first_fat+1
        sta longreg1+2
        lda fatfs_fat_begin+2
        adc fatfs_first_fat+2
        sta longreg1+3
        jmp got_position

    second_fat:

        ; Offset fatfs_fat_seek_pos with the second FAT
        clc
        lda fatfs_fat_begin+0
        sta longreg1+0
        lda fatfs_fat_begin+1
        adc fatfs_second_fat+0
        sta longreg1+1
        lda fatfs_fat_begin+2
        adc fatfs_second_fat+1
        sta longreg1+2
        lda fatfs_fat_begin+2
        adc fatfs_second_fat+2
        sta longreg1+3

    got_position:

    ; Seek to the location to read
    jsr dos_seek
    bcs error

    ; Read the cache
    lda #<fatfs_fat_cache
    sta pointer2+0
    lda #>fatfs_fat_cache
    sta pointer2+1
    lda #<256
    sta pointer3+0
    lda #>256
    sta pointer3+1
    jmp dos_read

error:
    sec
    rts

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Common routines                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Check pointer1 for a file number of an open file, and set current_file
; Return with C set and -errno in A if not a valid file

.proc set_current_file

    ; Check for pointer1 within bounds
    lda pointer1+1
    bne bad_file
    lda pointer1+0
    cmp #FATFS_MAX_FILES
    bcs bad_file

    ; Point to the file block
    tax
    lda open_files_lo,x
    sta current_file+0
    lda open_files_hi,x
    sta current_file+1

    ; Check that the file is open
    ldy #0
    lda (current_file),y
    beq bad_file

    clc
    rts

bad_file:
    lda #$100-EBADF
    sec
    rts

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Jump-off points to other banks                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Call dos_seek in the UltiDOS bank and set C if it returns an error

.proc dos_seek

    lda #FATFS_TARGET
    sta pointer1+0
    lda #dos_seek_bank
    ldx #<dos_seek_entry
    ldy #>dos_seek_entry
    jsr far_call
    lda pointer1+0
    cmp #10
    bcs error
    clc
    rts

error:
    lda #$100-EIO
    sec
    rts

.endproc

; Call dos_read in the UltiDOS bank and set C if it returns an error

.proc dos_read

    lda #FATFS_TARGET
    sta pointer1+0
    lda #dos_read_bank
    ldx #<dos_read_entry
    ldy #>dos_read_entry
    jsr far_call
    lda pointer1+0
    cmp #10
    bcs error
    clc
    rts

error:
    lda #$100-EIO
    sec
    rts

.endproc

; Call dos_read_reu in the UltiDOS bank and set C if it returns an error

.proc dos_read_reu

    lda #FATFS_TARGET
    sta pointer1+0
    lda #dos_read_reu_bank
    ldx #<dos_read_reu_entry
    ldy #>dos_read_reu_entry
    jsr far_call
    lda pointer1+0
    cmp #10
    bcs error
    clc
    rts

error:
    lda #$100-EIO
    sec
    rts

.endproc

; Call dos_write in the UltiDOS bank and set C if it returns an error

.proc dos_write

    lda #FATFS_TARGET
    sta pointer1+0
    lda #dos_write_bank
    ldx #<dos_write_entry
    ldy #>dos_write_entry
    jsr far_call
    lda pointer1+0
    cmp #10
    bcs error
    clc
    rts

error:
    lda #$100-EIO
    sec
    rts

.endproc
.if 0
;;;;
check1a: .byte "check 1A", 13, 0
check2a: .byte "check 2A", 13, 0
check3a: .byte "check 3A", 13, 0
check4a: .byte "check 4A", 13, 0
check5a: .byte "check 5A", 13, 0
check6a: .byte "check 6A", 13, 0
check7a: .byte "check 7A", 13, 0
check8a: .byte "check 8A", 13, 0

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

dump_dir_entry:
    ldy #0
    l1:
        lda fatfs_file_data,y
        jsr print_hex
    iny
    cpy #16
    bcc l1
    lda #13
    jsr CHROUT
    ldy #0
    l2:
        lda fatfs_file_data+16,y
        jsr print_hex
    iny
    cpy #16
    bcc l2
    lda #13
    jsr CHROUT
    rts
;;;;
.endif
