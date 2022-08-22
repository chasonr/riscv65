; Far call entry points and macro

; Use this to perform a far call
; The far call uses all three CPU registers, so memory must be used to pass
; parameters and return values

.macro jsr_far bank,entry

    lda #bank
    ldx #<entry
    ldy #>entry
    jsr far_call

.endmacro

; Bank 0; jump table in init.s

dos_init_bank = 0
dos_init_entry = $8009 + 0*3

dos_open_bank = 0
dos_open_entry = $8009 + 1*3

dos_close_bank = 0
dos_close_entry = $8009 + 2*3

dos_seek_bank = 0
dos_seek_entry = $8009 + 3*3

dos_read_bank = 0
dos_read_entry = $8009 + 4*3

dos_read_reu_bank = 0
dos_read_reu_entry = $8009 + 5*3

dos_write_bank = 0
dos_write_entry = $8009 + 6*3

dos_write_reu_bank = 0
dos_write_reu_entry = $8009 + 7*3

dos_change_dir_bank = 0
dos_change_dir_entry = $8009 + 8*3

dos_open_dir_bank = 0
dos_open_dir_entry = $8009 + 9*3

dos_read_dir_first_bank = 0
dos_read_dir_first_entry = $8009 + 10*3

dos_read_dir_next_bank = 0
dos_read_dir_next_entry = $8009 + 11*3

dos_get_time_bank = 0
dos_get_time_entry = $8009 + 12*3

error_dump_bank = 0
error_dump_entry = $8009 + 13*3

; Bank 1; jump table in bank1.s

fatfs_init_bank = 1
fatfs_init_entry = $8009 + 0*3

fatfs_search_startup_bank = 1
fatfs_search_startup_entry = $8009 + 1*3

fatfs_search_bank = 1
fatfs_search_entry = $8009 + 2*3

fatfs_seek_bank = 1
fatfs_seek_entry = $8009 + 3*3

fatfs_read_mem_bank = 1
fatfs_read_mem_entry = $8009 + 4*3

fatfs_read_bank = 1
fatfs_read_entry = $8009 + 5*3

; Bank 2; jump table in bank2.s

RISCV_emulator_bank = 2
RISCV_emulator_entry = $8009 + 0*3

RISCV_exit_bank = 2
RISCV_exit_entry = $8009 + 1*3

; Bank 3; jump table in bank3.s

multiply_signed_bank = 3
multiply_signed_entry = $8009 + 0*3

multiply_signed_unsigned_bank = 3
multiply_signed_unsigned_entry = $8009 + 1*3

multiply_unsigned_bank = 3
multiply_unsigned_entry = $8009 + 2*3

multiply_low_bank = 3
multiply_low_entry = $8009 + 3*3

divide_signed_bank = 3
divide_signed_entry = $8009 + 4*3

divide_unsigned_bank = 3
divide_unsigned_entry = $8009 + 5*3

shift_left_bank = 3
shift_left_entry = $8009 + 6*3

shift_right_unsigned_bank = 3
shift_right_unsigned_entry = $8009 + 7*3

shift_right_signed_bank = 3
shift_right_signed_entry = $8009 + 8*3

; Bank 4; jump table in bank4.s

RISCV_syscall_init_bank = 4
RISCV_syscall_init_entry = $8009 + 0*3

RISCV_syscall_exit_bank = 4
RISCV_syscall_exit_entry = $8009 + 1*3

RISCV_ECALL_bank = 4
RISCV_ECALL_entry = $8009 + 2*3

RISCV_EBREAK_bank = 4
RISCV_EBREAK_entry = $8009 + 3*3

; vim: syntax=asm
