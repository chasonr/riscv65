# Makefile for RISCV65 project

all: riscv-vm hello reu-load.d64

# A D64 image containing reu-load and riscv65
reu-load.d64: riscv65.prg reu-load.prg
	c1541 -format reu-load,rl d64 reu-load.d64 \
		-write reu-load.prg reu-load \
		-write riscv65.prg riscv65

##############################################################################
#                 riscv65.prg, the RISC-V emulation runtime                  #
##############################################################################

riscv65.prg: riscv65.o vm-api.o syscall.o riscv65.cfg
	cl65 riscv65.o vm-api.o syscall.o -C riscv65.cfg -o riscv65.prg -vm --mapfile riscv65.map

riscv65.o: riscv65.s registers.inc
	ca65 riscv65.s -o riscv65.o -t c64 -l riscv65.lst

vm-api.o: vm-api.s registers.inc memory.inc kernal.inc reu.inc
	ca65 vm-api.s -o vm-api.o -t c64 -l vm-api.lst

syscall.o: syscall.s registers.inc kernal.inc errno.inc reu.inc
	ca65 syscall.s -o syscall.o -t c64 -l vm-api.lst

##############################################################################
#                          riscv-vm, a test program                          #
##############################################################################

riscv-vm: riscv-vm.o 6502emu.o
	gcc -c -Wall -g riscv-vm.o 6502emu.o -o riscv-vm

riscv-vm.o: riscv-vm.c 6502emu.h
	gcc -c -Wall -g $< -o $@

6502emu.o: 6502emu.c 6502emu.h riscv-vm.h
	gcc -c -Wall -g $< -o $@

##############################################################################
#         reu-load.prg, loads the RISC-V target and launches riscv65         #
##############################################################################

reu-load.prg: reu-load.o chain-prg.o reu-load.cfg
	cl65 reu-load.o chain-prg.o -C reu-load.cfg -o reu-load.prg -vm --mapfile reu-load.map

reu-load.o: reu-load.c chain-prg.h
	cl65 -c -O -Cl reu-load.c -o reu-load.o

chain-prg.o: chain-prg.c chain-prg.h
	cl65 -c -O -Cl chain-prg.c -o chain-prg.o

chain-prg.c: chain.bin
	echo '#include "chain-prg.h"' >$@
	echo 'unsigned char const chain_prg[] = {' >>$@
	od -t x1 -An chain.bin | sed -e 's%[0-9a-f][0-9a-f]%0x&,%g' >>$@
	echo '};' >>$@
	echo 'unsigned char const chain_prg_size = sizeof(chain_prg);' >>$@

chain.bin: chain.s
	cl65 -t none chain.s -o chain.bin

##############################################################################
#                           A sample RISC-V target                           #
##############################################################################

hello: hello.c
	riscv-elf-gcc -Wall -O2 hello.c -o hello

##############################################################################

clean:
	rm -f *.o riscv65.prg riscv65.map reu-load.prg reu-load.map *.lst riscv-vm hello
	rm -f chain.o chain.bin chain-prg.c
	rm -f reu-load.d64
