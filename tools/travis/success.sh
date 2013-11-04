#!/bin/sh

if [ "$COMPILER"x = "gcc"x ]; then
  coveralls --build-root src --exclude intl --exclude tools --exclude docs --exclude libsmf --exclude libsffile -x .c -x .h
fi
