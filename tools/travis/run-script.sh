#!/bin/bash
set -e

check_status()
{
  STATUS=`git status -s`
  if [ "$STATUS"x = ""x ]; then
     #Success
     return 0;
  else
     #Failure
     echo "ERROR: Directory is not clean. Please clean it up during the tests, or update .gitignore file"
     git status
     return 1;
  fi
}

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
mkdir bin && cd bin
if [ -n "$COMPILER" ]; then
  ../configure --enable-debug --enable-silent-rules --disable-warnings $CONFIGURE_FLAGS
  make
fi

if [ "$TEST"x = "install"x ]; then
  sudo make install
  denemo -n -a "(d-Quit)"
fi

if [ "$TEST"x = "integration"x ]; then
  make distcheck
  rm -rf denemo-*.tar.gz
fi

if [ "$TEST"x = "coverage"x ]; then
  make check
fi

if [ "$TEST"x = "cppcheck"x ]; then
  cd ..
  cppcheck --force --enable=unusedFunction --template '{file}:{line} — {severity} — {id} — {message}' -q -I include src tools
fi

#Should be the last command ran, it checks that the repository is clean after tests are ran.
check_status

