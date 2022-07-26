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
#                  Bank 0: initialization and file browser                   #
##############################################################################

BANK0_OFILES = \
	init.o \
	memory.o

bank0.bin: $(BANK0_OFILES) cartridge.cfg
	cl65 $(BANK0_OFILES) -C cartridge.cfg -o bank0.bin -vm --mapfile bank0.map

init.o: init.s memory.inc kernal.inc cmd.inc reu.inc
	ca65 init.s -o init.o -t c64 -l init.lst

##############################################################################
#                          Bank 1: test of far call                          #
##############################################################################

BANK1_OFILES = \
	crtheader.o \
	test.o \
	memory.o 

bank1.bin: $(BANK1_OFILES) cartridge.cfg
	cl65 $(BANK1_OFILES) -C cartridge.cfg -o bank1.bin -vm --mapfile bank1.map

test.o: test.s kernal.inc
	ca65 test.s -o test.o -t c64 -l test.lst

##############################################################################
#                            Common to all banks                             #
##############################################################################

memory.o: memory.s
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
