#!/bin/sh

PREFIX=$HOME/riscv-gcc
TARGET=riscv-elf

export PATH=$PREFIX/bin:$PATH
if [ "`uname`" == "Darwin" ]; then
    export C_INCLUDE_PATH=`brew --prefix`/include
    export CPLUS_INCLUDE_PATH=`brew --prefix`/include
    export LIBRARY_PATH=`brew --prefix`/lib
fi

BINUTILS_VERSION=2.37
GCC_VERSION=11.2.0
NEWLIB_VERSION=4.1.0

set -e

rm -rf build-binutils binutils-${BINUTILS_VERSION}
rm -rf build-gcc gcc-${GCC_VERSION}
rm -rf build-newlib newlib-${NEWLIB_VERSION}

tar xvf binutils-${BINUTILS_VERSION}.tar.xz
patch -p0 -i binutils.patch
mkdir build-binutils
cd build-binutils
../binutils-${BINUTILS_VERSION}/configure --prefix=$HOME/riscv-gcc --target=riscv-elf \
    --with-sysroot --disable-nls --disable-werror
make
make install
cd ..

tar xvf gcc-${GCC_VERSION}.tar.xz
if [ "`uname`" == "Darwin" ]; then
    patch -p0 -i mac-gcc.patch
fi
mkdir build-gcc
cd build-gcc
../gcc-${GCC_VERSION}/configure --target=riscv-elf --prefix=$HOME/riscv-gcc \
    --disable-nls --enable-languages=c,c++ --without-headers \
    --with-abi=ilp32 --with-arch=rv32im
make all-gcc all-target-libgcc
make install-gcc install-target-libgcc
cd ..

tar xvf newlib-${NEWLIB_VERSION}.tar.gz
mkdir build-newlib
cd build-newlib
../newlib-${NEWLIB_VERSION}/configure --prefix=$HOME/riscv-gcc --target=riscv-elf
make
make install
cd ..
