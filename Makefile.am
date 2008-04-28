SUBDIRS = m4 doc include keymaps plugins pixmaps po src \
	  fonts 


dist_pkgdata_DATA = denemo.conf denemorc.xml

EXTRA_DIST = config.rpath  mkinstalldirs  DESIGN GOALS @PACKAGE@.spec.in  \
	     DESIGN.lilypond README.lilypond

MAINTAINERCLEANFILES =  Makefile.in aclocal.m4 configure config.h.in \
	stamp-h.in \
	config.guess mkinstalldirs \
	missing install-sh config.sub \
	acinclude.m4 config.log \
	depcomp

dist-hook: @PACKAGE@.spec
	cp $(srcdir)/@PACKAGE@.spec $(distdir)

ACLOCAL_AMFLAGS = -I m4
