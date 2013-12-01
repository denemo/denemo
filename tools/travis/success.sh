#!/bin/sh

if [ "$TEST"x = "coverage"x ]; then
  coveralls --root .  --exclude intl --exclude docs --exclude libsmf --exclude libsffile -x .c -x .h
fi
