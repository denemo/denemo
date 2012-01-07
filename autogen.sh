#!/bin/sh
aclocal $ACLOCAL_FLAGS -I m4 -I /usr/share/aclocal && \
autoheader && \
libtoolize --force && \
automake --add-missing --gnu && \
autoconf

