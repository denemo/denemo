#!/bin/sh

if [ "$COMPILER"x = "gcc"x ] && [ "$TEST"x = "integration"x ]; then
  coveralls --root .  --exclude intl --exclude docs --exclude libsmf --exclude libsffile -x .c -x .h
fi
