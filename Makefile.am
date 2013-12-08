SUBDIRS = \
  tools \
  libs/libsffile \
  libs/libsmf \
  build \
  docs \
  pixmaps \
  po \
  src \
  fonts \
  soundfonts \
  ui \
  tests

bin_SCRIPTS = \
  denemo_file_update.sh

EXTRA_DIST = \
  include \
  actions \
  templates \
  examples \
  @PACKAGE@.spec.in \
  $(INTLTOOL_FILES) \
  docs/DESIGN \
  docs/GOALS \
  docs/DESIGN.lilypond \
  intltool-extract.in \
  intltool-merge.in \
  intltool-update.in \
  denemo_file_update.sh

MAINTAINERCLEANFILES = \
  Makefile.in \
  aclocal.m4 \
  configure \
  config.h.in \
  stamp-h.in \
  config.guess \
  mkinstalldirs \
  missing \
  install-sh \
  config.sub \
  acinclude.m4 \
  config.log \
  depcomp \
  intltool-extract \
  intltool-merge \
  intltool-update \
  po/.intltool-merge-cache \
  $(GETTEXT_PACKAGE)-$(VERSION).pot

DISTCLEANFILES = \
  intltool-extract \
  intltool-merge \
  intltool-update \
  po/.intltool-merge-cache \
  po/denemo.pot

ACLOCAL_AMFLAGS = -I build

install-data-hook:
	cp -r $(srcdir)/actions ${DESTDIR}/$(datadir)/${PACKAGE}/
	find $(DESTDIR)$(datadir)/${PACKAGE}/actions -type d -exec chmod 755 '{}' \;
	cp -r $(srcdir)/examples ${DESTDIR}/$(datadir)/${PACKAGE}/
	find $(DESTDIR)$(datadir)/${PACKAGE}/examples -type d -exec chmod 755 '{}' \;
	cp -r $(srcdir)/templates ${DESTDIR}/$(datadir)/${PACKAGE}/
	find $(DESTDIR)$(datadir)/${PACKAGE}/templates -type d -exec chmod 755 '{}' \;

uninstall-hook:
	rm -rf ${DESTDIR}/$(datadir)/${PACKAGE}/actions
	rm -rf ${DESTDIR}/$(datadir)/${PACKAGE}/examples
	rm -rf ${DESTDIR}/$(datadir)/${PACKAGE}/templates

potfile:
	@rm -f po/POTFILES.in
	@rm -f po/POTFILES.skip
	@echo Generating POTFILES...
	@find src -name '*.c' -o -name '*.h' ! -name 'pathconfig.h' | sort >> po/POTFILES.in
	@find actions -name '*.xml' -o -name '*.scm' | sort >> po/POTFILES.in
	@find tools -name '*.c' -o -name '*.h' | sort >> po/POTFILES.skip
	@find src -name 'pathconfig.h' | sort >> po/POTFILES.skip

pot:
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
	echo "You can find in attachment the latest version of denemo.pot. This is an automatic message, but someone should answer you if you reply." | mailx -v -S from=denemo-devel@gnu.org -s "$(GETTEXT_PACKAGE)-$(VERSION).pot" -a $(GETTEXT_PACKAGE)-$(VERSION).tar.gz -a po/$(GETTEXT_PACKAGE)-$(VERSION).pot coordinator@translationproject.org
