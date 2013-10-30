#!/bin/sh
aclocal $ACLOCAL_FLAGS -I m4 -I /usr/share/aclocal && \
autoheader && \
libtoolize --force && \
intltoolize --copy --force --automake && \
automake --add-missing --gnu && \
autoconf
