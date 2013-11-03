#!/bin/sh
# This script updates the reference on the website.
# It builds the reference from the latest stable release and copy it somewhere
# accessible from the website.

REFERENCE_DIR="docs/reference/denemo"

if [ $# -lt 1 ]; then
  echo "Usage: ./update-doc.sh [DEST_DIR]"
  exit;
fi

cd $(pwd)/../..
git fetch -p origin

for tag in `git tag`; do
  git checkout -q $tag
  if [ -f $REFERENCE_DIR/Makefile.am ]; then
    ./autogen.sh
    ./configure --quiet --enable-silent-rules --enable-gtk-doc || continue
    make -C $REFERENCE_DIR || continue
    rm -rf $1/$tag
    mkdir -p $1/$tag
    cp -R $REFERENCE_DIR/html/* $1/$tag
    make -C $REFERENCE_DIR clean
  fi
done
git checkout -q master
