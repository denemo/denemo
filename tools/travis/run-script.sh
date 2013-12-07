#!/bin/bash
set -e

if [ "$TEST"x = "coverage"x ]; then
  CONFIGURE_FLAGS="--enable-gcov"
fi

if [ "$COMPILER"x = "gcc"x ]; then
  export CC="gcc"
  export CXX="g++"
elif [ "$COMPILER"x = "clang"x ]; then
  export CC="clang"
  export CXX="clang++"
fi

./autogen.sh
./configure --enable-debug --enable-silent-rules --disable-warnings $CONFIGURE_FLAGS

if [ "$TEST"x = "install"x ]; then
  make
  sudo make install
  denemo -n -a "(d-Quit)"
fi

if [ "$TEST"x = "integration"x ]; then
  make
  make distcheck
fi

if [ "$TEST"x = "coverage"x ]; then
  make
  make check
fi
