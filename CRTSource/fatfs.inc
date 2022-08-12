; Implementation of FAT file system.
; Supports long file names and FAT12, FAT16 and FAT32.

.global fatfs_init
.global fatfs_search_startup
.global fatfs_search
.global fatfs_seek
.global fatfs_read_mem
.global fatfs_read

; vim: syntax=asm
