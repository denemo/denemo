#!/bin/bash
set -e

if [ "$COMPILER"x = "gcc"x ]; then
  export CC="gcc"
  export CXX="g++"
  ./autogen.sh
  ./configure
  make
elif [ "$COMPILER"x = "clang"x ]; then
  export CC="clang"
  export CXX="clang++"
  ./autogen.sh
  ./configure
  make
fi
