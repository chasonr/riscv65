# Makefile for the RISCV65 cartridge image

BANKS = \
bank0.bin \
bank1.bin

HOST_CC = gcc
HOST_CFLAGS = -Wall -O2

riscv65.crt : $(BANKS) crttool
	./crttool --output=riscv65.crt \
		--bank0=bank0.bin \
		--bank1=bank1.bin \
		--name='RISCV65 SYSTEM'

##############################################################################
#          Bank 0: initialization, file browser and program loader           #
##############################################################################

BANK0_OFILES = \
	init.o \
	browser.o \
	reu-load.o \
	ultidos.o \
	memory.o

bank0.bin: $(BANK0_OFILES) cartridge.cfg
	cl65 $(BANK0_OFILES) -C cartridge.cfg -o bank0.bin -vm --mapfile bank0.map

init.o: init.s memory.inc kernal.inc cmd.inc reu.inc reu-load.inc ultidos.inc
	ca65 init.s -o init.o -t c64 -l init.lst

browser.o: browser.s kernal.inc memory.inc reu.inc ultidos.inc
	ca65 browser.s -o browser.o -t c64 -l browser.lst

reu-load.o: reu-load.s farcall.inc reu-load.inc ultidos.inc kernal.inc \
		reu.inc memory.inc registers.inc
	ca65 reu-load.s -o reu-load.o -t c64 -l reu-load.lst

ultidos.o: ultidos.s memory.inc ultidos.inc
	ca65 ultidos.s -o ultidos.o -t c64 -l ultidos.lst

##############################################################################
#                          Bank 1: FAT file system                           #
##############################################################################

BANK1_OFILES = \
	crtheader.o \
	bank1.o \
	fatfs.o \
	memory.o 

bank1.bin: $(BANK1_OFILES) cartridge.cfg
	cl65 $(BANK1_OFILES) -C cartridge.cfg -o bank1.bin -vm --mapfile bank1.map

bank1.o: bank1.s fatfs.inc
	ca65 bank1.s -o bank1.o -t c64 -l bank1.lst

fatfs.o: fatfs.s memory.inc farcall.inc errno.inc fatfs.inc
	ca65 fatfs.s -o fatfs.o -t c64 -l fatfs.lst

##############################################################################
#                            Common to all banks                             #
##############################################################################

memory.o: memory.s memory.inc
	ca65 memory.s -o memory.o -t c64 -l memory.lst

crtheader.o: crtheader.s memory.inc
	ca65 crtheader.s -o crtheader.o -t c64 -l crtheader.lst

##############################################################################
#                               Cartridge tool                               #
##############################################################################

crttool: crttool.c
	$(HOST_CC) $(HOST_CFLAGS) crttool.c -o crttool

##############################################################################
#                                  Cleanup                                   #
##############################################################################

clean:
	rm -f *.o *.map *.lst bank*.bin riscv65.crt crttool
