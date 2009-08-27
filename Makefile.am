SUBDIRS = libsmf m4 doc actions pixmaps po src \
	  fonts examples templates 


dist_sysconf_DATA = denemo.conf 

EXTRA_DIST = actions/menus doc/DESIGN doc/GOALS @PACKAGE@.spec.in  \
	     doc/DESIGN.lilypond actions \
	     guile-1.8.pc fonts/LICENSE_OFL.txt

MAINTAINERCLEANFILES =  Makefile.in aclocal.m4 configure config.h.in \
	stamp-h.in \
	config.guess mkinstalldirs \
	missing install-sh config.sub \
	acinclude.m4 config.log \
	depcomp

install-data-hook:
	cp -r $(srcdir)/actions ${DESTDIR}/$(datadir)/${PACKAGE}/
	find $(DESTDIR)$(datadir)/${PACKAGE}/actions -type d -exec chmod 755 '{}' \;
dist-hook: @PACKAGE@.spec
	cp $(srcdir)/@PACKAGE@.spec $(distdir)

ACLOCAL_AMFLAGS = -I m4
