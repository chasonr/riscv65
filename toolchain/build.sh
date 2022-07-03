#!/bin/sh

PREFIX=$HOME/riscv-gcc
TARGET=riscv-elf

export PATH=$PREFIX/bin:$PATH
if [ "`uname`" = "Darwin" ]; then
    export C_INCLUDE_PATH=`brew --prefix`/include
    export CPLUS_INCLUDE_PATH=`brew --prefix`/include
    export LIBRARY_PATH=`brew --prefix`/lib
fi

BINUTILS_VERSION=2.38
GCC_VERSION=12.1.0
NEWLIB_VERSION=4.1.0

set -e

if [ "$(sha256sum binutils-${BINUTILS_VERSION}.tar.xz | awk '{print $1}')" != "e316477a914f567eccc34d5d29785b8b0f5a10208d36bbacedcc39048ecfe024" ]; then
    wget https://ftpmirror.gnu.org/binutils/binutils-${BINUTILS_VERSION}.tar.xz
fi
if [ "$(sha256sum gcc-${GCC_VERSION}.tar.xz | awk '{print $1}')" != "62fd634889f31c02b64af2c468f064b47ad1ca78411c45abe6ac4b5f8dd19c7b" ]; then
    wget https://ftpmirror.gnu.org/gcc/gcc-${GCC_VERSION}/gcc-${GCC_VERSION}.tar.xz
fi
if [ "$(sha256sum newlib-${NEWLIB_VERSION}.tar.gz | awk '{print $1}')" != "f296e372f51324224d387cc116dc37a6bd397198756746f93a2b02e9a5d40154" ]; then
    wget ftp://sourceware.org/pub/newlib/newlib-${NEWLIB_VERSION}.tar.gz
fi

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
if [ "`uname`" = "Darwin" ]; then
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
patch -p0 -i newlib.patch
mkdir build-newlib
cd build-newlib
../newlib-${NEWLIB_VERSION}/configure --prefix=$HOME/riscv-gcc \
    --target=riscv-elf \
    --enable-newlib-io-pos-args \
    --enable-newlib-io-c99-formats \
    --enable-newlib-io-long-long \
    --enable-newlib-io-long-double
make
make install
cd ..

# Build and install libkernal
make -C kernallib
cp -p kernallib/kernal.h ${PREFIX}/${TARGET}/include/
cp -p kernallib/libkernal.a ${PREFIX}/${TARGET}/lib/
