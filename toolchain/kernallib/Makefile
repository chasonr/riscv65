# Makefile for libkernal.a

LIB = libkernal.a

OFILES = \
acptr.o chkin.o chkout.o chrin.o chrout.o ciout.o cint.o clall.o close.o \
clrchn.o getin.o ioinit.o listen.o open.o get_cursor.o set_cursor.o rdtim.o \
readst.o second.o settim.o stop.o talk.o tksa.o unlsn.o untlk.o

CC = riscv-elf-gcc
CFLAGS = -Wall -O2

$(LIB) : $(OFILES)
	@rm -f $(LIB)
	riscv-elf-ar -rcu $(LIB) $(OFILES)


acptr.o : acptr.c kernal.h kernalapi.h
chkin.o : chkin.c kernal.h kernalapi.h
chkout.o : chkout.c kernal.h kernalapi.h
chrin.o : chrin.c kernal.h kernalapi.h
chrout.o : chrout.c kernal.h kernalapi.h
ciout.o : ciout.c kernal.h kernalapi.h
cint.o : cint.c kernal.h kernalapi.h
clall.o : clall.c kernal.h kernalapi.h
close.o : close.c kernal.h kernalapi.h
clrchn.o : clrchn.c kernal.h kernalapi.h
getin.o : getin.c kernal.h kernalapi.h
ioinit.o : ioinit.c kernal.h kernalapi.h
listen.o : listen.c kernal.h kernalapi.h
open.o : open.c kernal.h kernalapi.h
get_cursor.o : get_cursor.c kernal.h kernalapi.h
set_cursor.o : set_cursor.c kernal.h kernalapi.h
rdtim.o : rdtim.c kernal.h kernalapi.h
readst.o : readst.c kernal.h kernalapi.h
second.o : second.c kernal.h kernalapi.h
settim.o : settim.c kernal.h kernalapi.h
stop.o : stop.c kernal.h kernalapi.h
talk.o : talk.c kernal.h kernalapi.h
tksa.o : tksa.c kernal.h kernalapi.h
unlsn.o : unlsn.c kernal.h kernalapi.h
untlk.o : untlk.c kernal.h kernalapi.h

clean:
	rm -f *.o $(LIB)
