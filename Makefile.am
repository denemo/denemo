SUBDIRS = $(WHICH_SMF) libsffile m4 doc actions pixmaps po src \
	  fonts soundfonts 

INTLTOOL_FILES = intltool-extract.in \
                 intltool-merge.in \
                 intltool-update.in

dist_sysconf_DATA = denemo.conf 
bin_SCRIPTS = denemo_file_update.sh
EXTRA_DIST = actions/menus doc/DESIGN doc/GOALS @PACKAGE@.spec.in  \
	     doc/DESIGN.lilypond actions include \
	     guile-1.8.pc libsmf libsffile \
	     examples templates denemo_file_update.sh $(INTLTOOL_FILES)

MAINTAINERCLEANFILES =  Makefile.in aclocal.m4 configure config.h.in \
	stamp-h.in \
	config.guess mkinstalldirs \
	missing install-sh config.sub \
	acinclude.m4 config.log \
	depcomp \
  intltool-extract \
  intltool-merge \
  intltool-update \
  po/.intltool-merge-cache

install-data-hook:
	cp -r $(srcdir)/actions ${DESTDIR}/$(datadir)/${PACKAGE}/
	find $(DESTDIR)$(datadir)/${PACKAGE}/actions -type d -exec chmod 755 '{}' \;
	cp -r $(srcdir)/examples ${DESTDIR}/$(datadir)/${PACKAGE}/
	find $(DESTDIR)$(datadir)/${PACKAGE}/examples -type d -exec chmod 755 '{}' \;
	cp -r $(srcdir)/templates ${DESTDIR}/$(datadir)/${PACKAGE}/
	find $(DESTDIR)$(datadir)/${PACKAGE}/templates -type d -exec chmod 755 '{}' \;
dist-hook: @PACKAGE@.spec
	cp $(srcdir)/@PACKAGE@.spec $(distdir)

ACLOCAL_AMFLAGS = -I m4

DISTCLEANFILES = intltool-extract \
                 intltool-merge \
                 intltool-update \
                 po/.intltool-merge-cache