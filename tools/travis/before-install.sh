#!/bin/bash 
set -e

sudo apt-get -qq update

if [ "$TEST"x = "coverage"x ]; then
  sudo pip install cpp-coveralls --use-mirrors
fi
