; error.inc -- dump registers on error

.global error_dump

ERR_bad_fetch = 0
ERR_bad_byte_read = 1
ERR_bad_hword_read = 2
ERR_bad_dword_read = 3
ERR_bad_byte_write = 4
ERR_bad_hword_write = 5
ERR_bad_dword_write = 6
ERR_bad_opcode = 7
ERR_bad_syscall = 8
ERR_bad_ebreak = 9

; vim: syntax=asm
