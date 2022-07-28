; ultidos.inc -- interface to UltiDOS on 1541-Ultimate and Ultimate 64

; Flags for dos_open mode
FA_READ          = $01
FA_WRITE         = $02
FA_CREATE_NEW    = $04
FA_CREATE_ALWAYS = $08

; Attributes returned when reading directory
; These are the same as FAT file systems
FA_READONLY  = $01
FA_HIDDEN    = $02
FA_SYSTEM    = $04
FA_VOLUME    = $08
FA_DIRECTORY = $10
FA_ARCHIVE   = $20

; vim: syntax=asm
