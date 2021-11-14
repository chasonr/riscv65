# riscv65
RISC-V implementation on the Ultimate 64 board.

The idea is to trade away speed in return for program and data size. This project uses the Ultimate 64 board's 48 MHz setting and UltiDOS interface to access a file system and a 16 megabyte program space. The project can run on the 1541 Ultimate II+ cartridge, but may prove too slow to be useful.

A loader program loads a compiled RISC-V binary into the RAM Expansion Unit, and then loads a RISC-V interpreter to run it. The interpreter provides a set of system calls so the target can access files.
