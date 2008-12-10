SUBDIRS = m4 doc actions pixmaps po src \
	  fonts examples templates 


dist_sysconf_DATA = denemo.conf 

EXTRA_DIST = mkinstalldirs  actions/menus doc/DESIGN doc/GOALS @PACKAGE@.spec.in  \
	     doc/DESIGN.lilypond README.lilypond include actions \
	     guile-1.8.pc

MAINTAINERCLEANFILES =  Makefile.in aclocal.m4 configure config.h.in \
	stamp-h.in \
	config.guess mkinstalldirs \
	missing install-sh config.sub \
	acinclude.m4 config.log \
	depcomp

install-data-hook:
	cp -r actions ${DESTDIR}/$(datadir)/${PACKAGE}/
	$(LN_S) $(DESTDIR)/$(datadir)/${PACKAGE}/actions/menus/MainMenu/EntryMenu/MIDI/CheckPitches \
	       	$(DESTDIR)/$(datadir)/${PACKAGE}/actions/menus/MainMenu/Educational/CheckPitches
	
	chmod -R 755 ${DESTDIR}/$(datadir)/${PACKAGE}/actions
dist-hook: @PACKAGE@.spec
	cp $(srcdir)/@PACKAGE@.spec $(distdir)

ACLOCAL_AMFLAGS = -I m4
