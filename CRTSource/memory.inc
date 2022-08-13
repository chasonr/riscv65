; All memory areas must be declared in memory.s. Global declarations in
; memory.s should be named as imports in memory.inc.

; The current bank number
.globalzp current_bank

; Some scratch areas for passing parameters and return values
.globalzp pointer1, pointer2, pointer3, pointer4
.globalzp longreg1, longreg2

; fatfs.s uses this to point to a fatfs_filedata record
.globalzp current_file

; The PC register
.globalzp RISCV_pc

; The fence, for memory protection
.globalzp RISCV_fence

; Integer registers
.global RISCV_ireg_0,RISCV_ireg_1,RISCV_ireg_2,RISCV_ireg_3

; Floating point registers (not yet implemented)
; .global RISCV_freg_0, RISCV_freg_1, RISCV_freg_2, RISCV_freg_3
; .global RISCV_freg_4, RISCV_freg_5, RISCV_freg_6, RISCV_freg_7
; .global RISCV_freg_8, RISCV_freg_9, RISCV_freg_10, RISCV_freg_11
; .global RISCV_freg_12, RISCV_freg_13, RISCV_freg_14, RISCV_freg_15

; More RISC-V related items

.global RISCV_break
.global RISCV_min_break

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bank switching areas. Bank 0 writes code here to be visible to all banks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.global far_call
.global cold_start_jump
.global warm_start_jump
.global warm_start_return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The file browser uses this area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.global browser_path
.global browser_num_files
.global browser_top_row
.global browser_cursor_pos
.global browser_search_str
.global browser_old_border_color
.global browser_old_background_color

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ultidos.s uses this area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Status strings from the command interface go here
.global ultidos_status

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fatfs.s uses this area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Structures for file access
; Operations in progress use fatfs_file_data
; Open files are contained in fatfs_open_files
.struct fatfs_filedata
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
    dir_entry  .byte 4   ; position of this directory entry
    lfn_entry  .byte 4   ; position of first LFN entry; equal to dir_entry if no LFN

    ; Mode for opening the file (read, write, append)
    open_flags  .byte 3

    ; Size as cluster and offset within cluster
    ; This is not valid for directories
    cluster_count .byte 3 ; not including partial cluster at the end
    cluster_partial .byte 2

    ; Current position as cluster and offset within cluster
    ; If current_cluster is $FFFFFFFF, the "cluster" is the FAT12/16 root directory,
    ; and three bytes of cluster_ptr are used; otherwise, only two bytes
    ; are used
    current_cluster .byte 4 ; Current cluster number as file system cluster number
    cluster_pos .byte 3     ; Current cluster number as count within cluster chain
    cluster_ptr .byte 3     ; Offset within cluster or root directory
.endstruct
FATFS_MAX_FILES = 16

.global fatfs_file_data
.global fatfs_open_files
.global fatfs_search_dir_num_components
.global fatfs_search_dir
.global fatfs_component_len

; Parameters describing the layout of the file system:

; Type of file system: 12, 16 or 32
.global fatfs_type

; Sector size is 2**(fatfs_sector_shift+8)
.global fatfs_sector_shift

; Cluster size is 2**(fatfs_cluster_shift+8)
.global fatfs_cluster_shift

; Cluster size is fatfs_cluster_size * 256
.global fatfs_cluster_size

; Number of FATS is fatfs_num_fats
.global fatfs_num_fats

; Seek position of first FAT is fatfs_first_fat * 256
.global fatfs_first_fat

; Seek position of second FAT is fatfs_second_fat * 256
.global fatfs_second_fat

; Size of one FAT is fatfs_fat_size * 256
.global fatfs_fat_size

; For FAT12 and FAT16: Seek position of root directory is fatfs_root_dir * 256
; For FAT32: First cluster of root directory is fatfs_root_dir
.global fatfs_root_dir

; For FAT12 and FAT16: Number of entries in root directory is fatfs_root_dir_size
; For FAT32: fatfs_root_dir_size is unused
.global fatfs_root_dir_size

; For FAT12 and FAT16: Size of root directory is fatfs_root_dir_bytes * 256
; For FAT32: fatfs_root_dir_bytes is unused
.global fatfs_root_dir_bytes

; Seek position of a cluster is
; ((cluster_num << fatfs_cluster_shift) + fatfs_cluster_base) * 256
.global fatfs_cluster_base

; Number of bytes in the file system is fatfs_fs_size * 256
.global fatfs_fs_size

; Valid cluster numbers are 2 <= cluster_num < fatfs_clusters
.global fatfs_clusters

; FAT32 extended flags
.global fatfs_ext_flags

; For FAT32: Seek position of the info sector is fatfs_info_sector_pos * 256
.global fatfs_info_sector_pos

; For FAT32: fatfs_free_changed is true if the free cluster count has changed
.global fatfs_free_changed

; fatfs_fat_dirty is nonzero if there are uncommitted writes in fatfs_fat_cache
.global fatfs_fat_dirty

; The seek position of the current sector in fatfs_fat_cache is fatfs_fat_begin
; If fatfs_fat_begin is zero, no FAT sector is cached
.global fatfs_fat_begin

; Seek position for FAT access
.global fatfs_fat_seek_pos

; Current directory:
FATFS_MAX_COMPONENTS = 16
.global fatfs_current_dir_num_components
.global fatfs_current_dir

; Directory currently being searched
.global fatfs_search_dir_num_components
.global fatfs_search_dir
; Length of path component being searched
.global fatfs_component_len
; Offset to last 13-byte segment of file name
.global fatfs_last_segment
; Sequence number matching last segment of file name
.global fatfs_start_seq
; Current name segment being matched
.global fatfs_segment
; Sequence number of last LFN record
.global fatfs_sequence
; Checksum of short file name
.global fatfs_checksum

; Boot sectors and directory entries are read and written here
.global fatfs_data_sector
; The FAT32 information sector is kept here
.global fatfs_info_sector
; 512 bytes of the FAT are cached here
.global fatfs_fat_cache

; Offset when seeking a file
.global fatfs_seek_offset

; Cluster count when seeking a file
.global fatfs_seek_cluster_count

; Current address when reading or writing a file
.global fatfs_xfer_addr

; Current size when reading or writing a file
.global fatfs_xfer_size

; Total size when reading or writing a file
.global fatfs_total_size

; Remaining size when reading or writing a file
.global fatfs_remaining_size

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reu.s uses this area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.global reu_c64_address
.global reu_xmem_address
.global reu_xfer_size
.global reu_partial_size

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Scratch area, internal to various routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.global scratch_area

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shift tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.global lshift2, lshift3, lshift4, lshift5, lshift6
.global rshift2, rshift3, rshift4, rshift5, rshift6
.global rsshift2, rsshift3, rsshift4, rsshift5, rsshift6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Character conversion tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.global ascii_to_petscii
.global ascii_to_screen
.global petscii_to_screen

; vim: syntax=asm
