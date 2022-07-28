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

; vim: syntax=asm
