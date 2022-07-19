# Makefile for RISCV65 project

all: reu-load.d64 filesys.img

HOSTCC = gcc
HOSTCFLAGS = -Wall -O2

##############################################################################
#                 A D64 image containing reu-load and riscv65                #
##############################################################################

reu-load.d64: riscv65.prg reu-load.prg d64tool
	./d64tool --output=reu-load.d64 --name=reu-load --id=rl reu-load.prg riscv65.prg

##############################################################################
#                     d64tool, to generate the D64 image                     #
##############################################################################

d64tool: d64tool.c
	$(HOSTCC) $(HOSTCFLAGS) d64tool.c -o d64tool

##############################################################################
#                 riscv65.prg, the RISC-V emulation runtime                  #
##############################################################################

riscv65.prg: riscv65.o vm-api.o syscall.o kernal.o riscv65.cfg
	cl65 riscv65.o vm-api.o syscall.o kernal.o -C riscv65.cfg -o riscv65.prg -vm --mapfile riscv65.map

riscv65.o: riscv65.s reu.inc registers.inc vm-api.inc
	ca65 riscv65.s -o riscv65.o -t c64 -l riscv65.lst

vm-api.o: vm-api.s registers.inc memory.inc kernal.inc reu.inc
	ca65 vm-api.s -o vm-api.o -t c64 -l vm-api.lst

syscall.o: syscall.s registers.inc kernal.inc errno.inc reu.inc
	ca65 syscall.s -o syscall.o -t c64 -l syscall.lst

kernal.o: kernal.s errno.inc registers.inc kernal.inc
	ca65 kernal.s -o kernal.o -t c64 -l kernal.lst

##############################################################################
#         reu-load.prg, loads the RISC-V target and launches riscv65         #
##############################################################################

REU_LOAD_OFILES = \
	reu-load.o \
	chain-prg.o \
	reu.o \
	cmd.o \
	ultidos.o \

reu-load.prg: $(REU_LOAD_OFILES) reu-load.cfg
	cl65 $(REU_LOAD_OFILES) -C reu-load.cfg -o reu-load.prg -vm --mapfile reu-load.map

reu-load.o: reu-load.c chain-prg.h cmd.h reu.h ultidos.h
	cl65 -c -O -Cl reu-load.c -o reu-load.o

chain-prg.o: chain-prg.c chain-prg.h
	cl65 -c -O -Cl chain-prg.c -o chain-prg.o

reu.o: reu.c reu.h
	cl65 -c -O -Cl reu.c -o reu.o

cmd.o: cmd.c cmd.h
	cl65 -c -O -Cl cmd.c -o cmd.o

ultidos.o: ultidos.c cmd.h ultidos.h
	cl65 -c -O -Cl ultidos.c -o ultidos.o

chain-prg.c: chain.bin
	echo '#include "chain-prg.h"' >$@
	echo 'unsigned char const chain_prg[] = {' >>$@
	od -t x1 -An chain.bin | sed -e 's%[0-9a-f][0-9a-f]%0x&,%g' >>$@
	echo '};' >>$@
	echo 'unsigned char const chain_prg_size = sizeof(chain_prg);' >>$@

chain.bin: chain.s
	cl65 -t none chain.s -o chain.bin

##############################################################################
#             A DOS file system image containing a test program              #
##############################################################################

filesys.img: hello hello-vol.txt test.txt
	fatfsgen hello-vol.txt

##############################################################################
#                           A sample RISC-V target                           #
##############################################################################

hello: hello.c
	riscv-elf-gcc -Wall -O2 hello.c -o hello -lkernal

##############################################################################

clean:
	rm -f *.o riscv65.prg riscv65.map reu-load.prg reu-load.map *.lst hello
	rm -f chain.o chain.bin chain-prg.c
	rm -f d64tool reu-load.d64
	rm -f filesys.img
