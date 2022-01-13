# RISC-V 65

*RISC-V implementation on the* ***Ultimate 64*** *board.*

---

**⸢ [Building] ⸥**

---

## Concept

**The idea is to trade speed for program / data size.**

This project uses the **Ultimate 64** board's `48MHz` <br>
setting and **UltiDOS** interface to access a file <br>
system and a `16 megabyte` program space.

The project can run on the `1541 Ultimate II+` <br>
cartridge, *but may prove too slow to be useful*.

A loader program loads a compiled **RISC-V** <br>
binary into the **RAM Expansion Unit**, and <br>
then loads a **RISC-V** interpreter to run it.

The interpreter provides a set of system <br>
calls for the target to interact with  files.


<!----------------------------------------------------------------------------->

[Building]: toolchain/README.md
