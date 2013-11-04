#!/bin/bash
set -e

if [ "$COMPILER"x = "gcc"x ]; then
  export CC="gcc"
  export CXX="g++"
  ./autogen.sh
  ./configure --enable-debug --enable-gcov
elif [ "$COMPILER"x = "clang"x ]; then
  export CC="clang"
  export CXX="clang++"
  ./autogen.sh
  ./configure --enable-debug
fi

if [ "$TEST"x = "integration"x ]; then
  make
  sudo make install
  make -C tests check
fi

if [ "$TEST"x = "dist"x ]; then
  ./autogen.sh
  ./configure
  make dist
  mkdir dist
  tar -xvzf denemo-*.tar.gz -C dist
  cd dist/*
  ./configure
  make
  sudo make install
  make -C tests check
fi
