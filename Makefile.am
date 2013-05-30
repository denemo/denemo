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

potfile:
	@echo Generating POTFILES...
	@find src -name '*.c' -o -name '*.h' | sort >> po/POTFILES.in
	@find actions -name '*.xml' -o -name '*.scm' | sort >> po/POTFILES.in
	@find tools -name '*.c' -o -name '*.h' | sort >> po/POTFILES.skip

clean-potfile:
	@rm -f po/POTFILES.in
	@rm -f po/POTFILES.skip

all:potfile
check:potfile
clean:clean-potfile
dist-clean:clean-potfile

pot:potfile
	@echo Generating POT...
	$(MAKE) -C "$(srcdir)/po" "$(GETTEXT_PACKAGE).pot"
	cd po && ln -f -s "$(GETTEXT_PACKAGE).pot" "$(GETTEXT_PACKAGE)-$(VERSION).pot"

update-po:
	@echo Get po files from translationproject.org
	rsync -Lrtvz translationproject.org::tp/latest/denemo/ po

dist-hook: @PACKAGE@.spec pot
	cp $(srcdir)/@PACKAGE@.spec $(distdir)
	cp "$(srcdir)/po/$(GETTEXT_PACKAGE).pot" "$(distdir)/po/"

send-pot:dist
	echo "You can find in attachment the latest version of denemo.pot. This is an automatic message, but someone should answer you if you reply." | mailx -v -r denemo-devel@gnu.org -s "$(GETTEXT_PACKAGE)-$(VERSION).pot" -a $(GETTEXT_PACKAGE)-$(VERSION).tar.gz -a po/$(GETTEXT_PACKAGE)-$(VERSION).pot coordinator@translationproject.org 

ACLOCAL_AMFLAGS = -I m4

DISTCLEANFILES = intltool-extract \
                 intltool-merge \
                 intltool-update \
                 po/.intltool-merge-cache