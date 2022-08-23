# Makefile for the RISCV65 cartridge image

BANKS = \
bank0.bin \
bank1.bin \
bank2.bin \
bank3.bin \
bank4.bin

HOST_CC = gcc
HOST_CFLAGS = -Wall -O2

riscv65.crt : $(BANKS) crttool
	./crttool --output=riscv65.crt \
		--bank0=bank0.bin \
		--bank1=bank1.bin \
		--bank2=bank2.bin \
		--bank3=bank3.bin \
		--bank4=bank4.bin \
		--name='RISCV65 SYSTEM'

##############################################################################
#          Bank 0: initialization, file browser and program loader           #
##############################################################################

BANK0_OFILES = \
	init.o \
	browser.o \
	reu-load.o \
	ultidos.o \
	error.o \
	reu.o \
	memory.o

bank0.bin: $(BANK0_OFILES) cartridge.cfg
	cl65 $(BANK0_OFILES) -C cartridge.cfg -o bank0.bin -vm --mapfile bank0.map

init.o: init.s memory.inc kernal.inc cmd.inc reu.inc reu-regs.inc reu-load.inc ultidos.inc error.inc
	ca65 init.s -o init.o -t c64 -l init.lst

browser.o: browser.s kernal.inc memory.inc reu.inc ultidos.inc
	ca65 browser.s -o browser.o -t c64 -l browser.lst

reu-load.o: reu-load.s farcall.inc reu-load.inc ultidos.inc kernal.inc \
		reu.inc memory.inc registers.inc
	ca65 reu-load.s -o reu-load.o -t c64 -l reu-load.lst

error.o: error.s error.inc farcall.inc kernal.inc memory.inc
	ca65 error.s -o error.o -t c64 -l error.lst

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
#            Banks 2 and 3: Copy the RISC-V interpreter from here            #
##############################################################################

bank2.bin: riscv65.bin crtheader.bin
	cp crtheader.bin bank2.bin
	dd if=riscv65.bin of=bank2.bin conv=notrunc oflag=append bs=8183 skip=0 count=1

bank3.bin: riscv65.bin crtheader.bin
	cp crtheader.bin bank3.bin
	dd if=riscv65.bin of=bank3.bin conv=notrunc oflag=append bs=8183 skip=1 count=1

CRTHEADER_OFILES = \
	crtheader.o \
	memory.o

crtheader.bin: $(CRTHEADER_OFILES)
	cl65 $(CRTHEADER_OFILES) -C cartridge.cfg -o crtheader.bin

RISCV65_OFILES = \
	bank2.o \
	riscv65.o \
	vm-api.o \
	muldiv.o \
	memory.o

riscv65.bin: $(RISCV65_OFILES) riscv65.cfg
	cl65 $(RISCV65_OFILES) -C riscv65.cfg -o riscv65.bin -vm --mapfile riscv65.map

bank2.o: bank2.s riscv65.inc
	ca65 bank2.s -o bank2.o -t c64 -l bank2.lst

riscv65.o: riscv65.s registers.inc reu-regs.inc memory.inc farcall.inc riscv65.inc vm-api.inc
	ca65 riscv65.s -o riscv65.o -t c64 -l riscv65.lst

vm-api.o: vm-api.s registers.inc memory.inc kernal.inc reu-regs.inc vm-api.inc farcall.inc riscv65.inc error.inc
	ca65 vm-api.s -o vm-api.o -t c64 -l vm-api.lst

muldiv.o: muldiv.s registers.inc reu-regs.inc memory.inc
	ca65 muldiv.s -o muldiv.o -t c64 -l muldiv.lst

##############################################################################
#                            Bank 4: system calls                            #
##############################################################################

BANK4_OFILES = \
	crtheader.o \
	bank4.o \
	syscall.o \
	reu.o \
	memory.o

bank4.bin: $(BANK4_OFILES) cartridge.cfg
	cl65 $(BANK4_OFILES) -C cartridge.cfg -o bank4.bin -vm --mapfile bank4.map

bank4.o: bank4.s fatfs.inc
	ca65 bank4.s -o bank4.o -t c64 -l bank4.lst

syscall.o: syscall.s syscall.inc memory.inc
	ca65 syscall.s -o syscall.o -t c64 -l syscall.lst

##############################################################################
#                            Common to all banks                             #
##############################################################################

memory.o: memory.s memory.inc
	ca65 memory.s -o memory.o -t c64 -l memory.lst

crtheader.o: crtheader.s memory.inc
	ca65 crtheader.s -o crtheader.o -t c64 -l crtheader.lst

##############################################################################
#                        Common to two or more banks                         #
##############################################################################

reu.o: reu.s memory.inc reu.inc reu-regs.inc
	ca65 reu.s -o reu.o -t c64 -l reu.lst

##############################################################################
#                               Cartridge tool                               #
##############################################################################

crttool: crttool.c
	$(HOST_CC) $(HOST_CFLAGS) crttool.c -o crttool

##############################################################################
#                                  Cleanup                                   #
##############################################################################

clean:
	rm -f *.o *.map *.lst bank*.bin riscv65.bin crtheader.bin riscv65.crt crttool
