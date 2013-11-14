#!/bin/sh
aclocal $ACLOCAL_FLAGS -I build -I /usr/share/aclocal && \
autoheader && \
libtoolize --force && \
intltoolize --copy --force --automake && \
automake --add-missing --gnu && \
autoconf
