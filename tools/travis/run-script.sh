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

configure()
{
  ./autogen.sh
  mkdir bin && cd bin
  ../configure --enable-debug --enable-silent-rules --disable-warnings $1
  make
  cd ..
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

if [ -n "$COMPILER" ]; then
  configure "$CONFIGURE_FLAGS"
fi

if [ ${COVERITY_SCAN_BRANCH} == 1 ]; then
  exit 0;
fi

if [ "$TEST"x = "install"x ]; then
  cd bin
  sudo make install
  denemo -n -a "(d-Quit)"
fi

if [ "$TEST"x = "integration"x ]; then
  cd bin
  make distcheck
  rm -rf denemo-*.tar.gz
fi

if [ "$TEST"x = "coverage"x ]; then
  cd bin
  make check
fi

if [ "$TEST"x = "cppcheck"x ]; then
  cppcheck --force --enable=unusedFunction --template '{file}:{line} — {severity} — {id} — {message}' -q -I include src tools
fi

#Should be the last command ran, it checks that the repository is clean after tests are ran.
check_status
