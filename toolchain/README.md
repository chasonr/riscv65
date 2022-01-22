
# Toolchain

The [`build.sh`] script not only compiles the <br>
**RISC-V** toolchain but also patches for **binutils**, <br>
instructing the linker as **RISC-V 65** requires.

<br>

## Requirements

- `binutils-2.37.tar.xz`

- `newlib-4.1.0.tar.gz`

- `gcc-11.2.0.tar.xz`

    *GCC also requires these packages.*
    - `GMP`
    - `MPFR`
    - `MPC`

<br>

## Path

Once the toolchain has built, you will need <br>
to add `$HOME/riscv-gcc` to your **PATH**.


<!----------------------------------------------------------------------------->

[`build.sh`]: build.sh
