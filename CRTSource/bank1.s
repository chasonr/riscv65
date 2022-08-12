; Jump table for bank 1

.include "fatfs.inc"

.code

jmp fatfs_init
jmp fatfs_search_startup
jmp fatfs_search
jmp fatfs_seek
jmp fatfs_read_mem
jmp fatfs_read
