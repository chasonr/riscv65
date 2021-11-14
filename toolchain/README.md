This directory contains the script needed to build the RISC-V toolchain and a patch for binutils so that the linker will link the binary as riscv65 requires.

You need these archives to build the toolchain:

* binutils-2.37.tar.xz
* gcc-11.2.0.tar.xz
* newlib-4.1.0.tar.gz

Building GCC requires the development packages for GMP, MPFR and MPC.

Once the toolchain is built, you will need to add $HOME/riscv-gcc to your PATH.
