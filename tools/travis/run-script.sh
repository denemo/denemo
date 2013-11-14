#!/bin/bash
set -e

if [ "$COMPILER"x = "gcc"x ]; then
  export CC="gcc"
  export CXX="g++"
  ./autogen.sh
  ./configure
  make
  make dist
  sudo make install
  make -C tests check
elif [ "$COMPILER"x = "clang"x ]; then
  export CC="clang"
  export CXX="clang++"
  ./autogen.sh
  ./configure
  make
  make dist
  sudo make install
  make -C tests check
fi
