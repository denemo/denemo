SUBDIRS = m4 doc keymaps pixmaps po src \
	  fonts examples templates 


dist_sysconf_DATA = denemo.conf 

EXTRA_DIST = mkinstalldirs  doc/DESIGN doc/GOALS @PACKAGE@.spec.in  \
	     doc/DESIGN.lilypond README.lilypond include

MAINTAINERCLEANFILES =  Makefile.in aclocal.m4 configure config.h.in \
	stamp-h.in \
	config.guess mkinstalldirs \
	missing install-sh config.sub \
	acinclude.m4 config.log \
	depcomp

dist-hook: @PACKAGE@.spec
	cp $(srcdir)/@PACKAGE@.spec $(distdir)

ACLOCAL_AMFLAGS = -I m4
