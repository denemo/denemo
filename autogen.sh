#!/bin/sh
KERNEL=`uname`
if [ $KERNEL = 'Linux' ]
then 

aclocal $ACLOCAL_FLAGS -I m4 -I /usr/share/aclocal && \
autoheader && \
libtoolize --force && \
automake --add-missing --gnu && \
autoconf

fi 


if  [ $KERNEL = 'Darwin' ]
then

aclocal $ACLOCAL_FLAGS -I m4 -I /usr/share/aclocal && \
autoheader && \
glibtoolize  --force && \
automake --add-missing --gnu && \
autoconf

fi
