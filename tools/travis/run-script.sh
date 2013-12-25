#!/bin/bash
set -e

if [ "$TEST"x = "coverage"x ]; then
  CONFIGURE_FLAGS="--enable-gcov"
fi

if [ "$TEST"x = "doc"x ]; then
  CONFIGURE_FLAGS="--enable-gtk-doc"
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
make

if [ "$TEST"x = "install"x ]; then
  sudo make install
  denemo -n -a "(d-Quit)"
fi

if [ "$TEST"x = "integration"x ]; then
  make distcheck
fi

if [ "$TEST"x = "coverage"x ]; then
  make check
fi
