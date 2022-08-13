; All memory areas must be declared in this source file. That is, the segments
; .bss, "BSSPAGE" and .zeropage must appear in this source file, and no other.
;
; Declarations should be global and declared as imports in memory.inc.
; Any initialization should be done in init.s.
;
; This source file is linked to every bank, so that every bank has the same
; idea of how memory is organized. 

.include "memory.inc"

.zeropage

; Current bank number
current_bank: .res 1

; Some scratch areas for passing parameters and return values
pointer1: .res 2
pointer2: .res 2
pointer3: .res 2
pointer4: .res 2
longreg1: .res 4
longreg2: .res 4

; fatfs.s uses this to point to a fatfs_filedata record
current_file: .res 2

; The PC register
RISCV_pc: .res 4

; The fence, for memory protection
RISCV_fence: .res 1

.bss

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RISC-V registers are placed first, for the benefit of target-supplied code.
; Integer and floating point registers are declared with one byte in each of
; several tables; e.g. integer register 5 occupies RISCV_ireg_0+5,
; RISCV_ireg_1+5, RISCV_ireg_2+5 and RISCV_ireg_3+5. This arrangement avoids
; having to shift indexes when accessing registers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Integer registers
RISCV_ireg_0: .res 32
RISCV_ireg_1: .res 32
RISCV_ireg_2: .res 32
RISCV_ireg_3: .res 32

; Floating point registers (not yet implemented)
; RISCV_freg_0: .res 32
; RISCV_freg_1: .res 32
; RISCV_freg_2: .res 32
; RISCV_freg_3: .res 32
; RISCV_freg_4: .res 32
; RISCV_freg_5: .res 32
; RISCV_freg_6: .res 32
; RISCV_freg_7: .res 32
; RISCV_freg_8: .res 32
; RISCV_freg_9: .res 32
; RISCV_freg_10: .res 32
; RISCV_freg_11: .res 32
; RISCV_freg_12: .res 32
; RISCV_freg_13: .res 32
; RISCV_freg_14: .res 32
; RISCV_freg_15: .res 32

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        More RISC-V related items                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RISCV_break: .res 3
RISCV_min_break: .res 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bank switching areas. Bank 0 writes code here to be visible to all banks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

far_call: .res 32
cold_start_jump: .res 16
warm_start_jump: .res 16
warm_start_return: .res 16

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The file browser uses this area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BROWSER_MAX_PATH_LEN = 256

; Displayed directory, while the browser is active
; File to run, after the browser returns
browser_path: .res BROWSER_MAX_PATH_LEN

; Current number of files displayed. The records reside in the REU.
browser_num_files: .res 2

; Current state of the screen
browser_top_row: .res 2
browser_cursor_pos: .res 2
browser_search_str: .res 40

; Saved colors
browser_old_border_color: .res 1
browser_old_background_color: .res 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ultidos.s uses this area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Status strings from the command interface go here
ultidos_status: .res 256

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fatfs.s uses this area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Parameters describing the layout of the file system:

; Type of file system: 12, 16 or 32
fatfs_type: .res 1

; Sector size is 2**(fatfs_sector_shift+8)
; Possible values: 1, 2, 3, 4
fatfs_sector_shift: .res 1

; Cluster size is 2**(fatfs_cluster_shift+8)
; Possible values: 1, 2, 3, 4, 5, 6, 7
fatfs_cluster_shift: .res 1

; Cluster size is fatfs_cluster_size * 256
; Possible values: 2, 4, 8, 16, 32, 64, 128
fatfs_cluster_size: .res 1

; Number of FATS is fatfs_num_fats
; Possible values: 1, 2
fatfs_num_fats: .res 1

; Seek position of first FAT is fatfs_first_fat * 256
fatfs_first_fat: .res 3

; Seek position of second FAT is fatfs_second_fat * 256
fatfs_second_fat: .res 3

; Size of one FAT is fatfs_fat_size * 256
fatfs_fat_size: .res 3

; For FAT12 and FAT16: Seek position of root directory is fatfs_root_dir * 256
; For FAT32: First cluster of root directory is fatfs_root_dir
fatfs_root_dir: .res 3

; For FAT12 and FAT16: Number of entries in root directory is fatfs_root_dir_size
; For FAT32: fatfs_root_dir_size is unused
fatfs_root_dir_size: .res 2

; For FAT12 and FAT16: Size of root directory is fatfs_root_dir_bytes * 256
; For FAT32: fatfs_root_dir_bytes is unused
fatfs_root_dir_bytes: .res 2

; Seek position of a cluster is
; ((cluster_num << fatfs_cluster_shift) + fatfs_cluster_base) * 256
fatfs_cluster_base: .res 3

; Number of bytes in the file system is fatfs_fs_size * 256
fatfs_fs_size: .res 3

; Valid cluster numbers are 2 <= cluster_num < fatfs_clusters
fatfs_clusters: .res 3

; FAT32 extended flags
fatfs_ext_flags: .res 1

; For FAT32: Seek position of the info sector is fatfs_info_sector_pos * 256
; Maximum value: $0FFFF0
fatfs_info_sector_pos: .res 3

; For FAT32: fatfs_free_changed is true if the free cluster count has changed
fatfs_free_changed: .res 1

; Current directory
fatfs_current_dir_num_components: .res 1
fatfs_current_dir: .res 8*FATFS_MAX_COMPONENTS

; Directory currently being searched
fatfs_search_dir_num_components: .res 1
fatfs_search_dir: .res 8*FATFS_MAX_COMPONENTS
; Length of path component being searched
fatfs_component_len: .res 1
; Offset to last 13-byte segment of file name
fatfs_last_segment: .res 1
; Sequence number matching last segment of file name
fatfs_start_seq: .res 1
; Current name segment being matched
fatfs_segment: .res 1
; Sequence number of last LFN record
fatfs_sequence: .res 1
; Checksum of short file name
fatfs_checksum: .res 1

; fatfs_fat_dirty is nonzero if there are uncommitted writes in fatfs_fat_cache
fatfs_fat_dirty: .res 1

; The seek position of the current sector in fatfs_fat_cache is fatfs_fat_begin
; If fatfs_fat_begin is zero, no FAT sector is cached
fatfs_fat_begin: .res 4

; Seek position for FAT access
fatfs_fat_seek_pos: .res 4

; Boot sectors and directory entries are read and written here
fatfs_data_sector: .res 512
; The FAT32 information sector is kept here
fatfs_info_sector: .res 512
; 256 bytes of the FAT are cached here
fatfs_fat_cache: .res 256

; Offset when seeking a file
fatfs_seek_offset: .res 5

; Cluster count when seeking a file
fatfs_seek_cluster_count: .res 3

; Current address when reading or writing a file
fatfs_xfer_addr: .res 4

; Current size when reading or writing a file
fatfs_xfer_size: .res 2

; Total size when reading or writing a file
fatfs_total_size: .res 4

; Remaining size when reading or writing a file
fatfs_remaining_size: .res 4

fatfs_file_data: .tag fatfs_filedata
fatfs_open_files: .res .sizeof(fatfs_filedata) * FATFS_MAX_FILES

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reu.s uses this area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

reu_c64_address: .res 2
reu_xmem_address: .res 3
reu_xfer_size: .res 2
reu_partial_size: .res 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Scratch area, internal to various routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
scratch_area: .res 512

.segment "BSSPAGE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shift tables generated at cold start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Left shift
lshift2: .res 256
lshift3: .res 256
lshift4: .res 256
lshift5: .res 256
lshift6: .res 256

; Unsigned right shift
rshift2: .res 256
rshift3: .res 256
rshift4: .res 256
rshift5: .res 256
rshift6: .res 256

; Signed right shift
rsshift2: .res 256
rsshift3: .res 256
rsshift4: .res 256
rsshift5: .res 256
rsshift6: .res 256

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Character conversion tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ascii_to_petscii: .res 256
ascii_to_screen: .res 256
petscii_to_screen: .res 256
