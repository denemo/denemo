@INTLTOOL_XML_RULE@
appdatadir = $(datarootdir)/appdata
appdata_DATA = $(appdata_in_files:.xml.in=.xml)
appdata_in_files = org.denemo.Denemo.appdata.xml.in

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

EXTRA_DIST = \
  include \
  actions \
  templates \
  examples \
  @PACKAGE@.spec.in \
  @PACKAGE@.spec \
  $(INTLTOOL_FILES) \
  docs/DESIGN \
  docs/GOALS \
  docs/DESIGN.lilypond \
  intltool-extract.in \
  intltool-merge.in \
  intltool-update.in \
  po/$(GETTEXT_PACKAGE).pot \
  org.denemo.Denemo.appdata.xml.in

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

CLEANFILES = \
  $(appdata_DATA)

ACLOCAL_AMFLAGS = -I build

install-data-hook:
	cp -r $(srcdir)/actions ${DESTDIR}/$(datadir)/${PACKAGE}/
	find $(DESTDIR)$(datadir)/${PACKAGE}/actions -type d -exec chmod 755 '{}' \;
	find $(DESTDIR)$(datadir)/${PACKAGE}/examples -type f \( -name Makefile -o -name Makefile.in \
	-o -name Makefile.am \) -print0 | xargs -0 rm -rf \;
	cp -r $(srcdir)/examples ${DESTDIR}/$(datadir)/${PACKAGE}/
	find $(DESTDIR)$(datadir)/${PACKAGE}/examples -type d -exec chmod 755 '{}' \;
	find $(DESTDIR)$(datadir)/${PACKAGE}/examples -type f \( -name Makefile -o -name Makefile.in \
	-o -name Makefile.am \) -print0 | xargs -0 rm -rf \;
	cp -r $(srcdir)/templates ${DESTDIR}/$(datadir)/${PACKAGE}/
	find $(DESTDIR)$(datadir)/${PACKAGE}/templates -type d -exec chmod 755 '{}' \;
	find $(DESTDIR)$(datadir)/${PACKAGE}/templates -type f \( -name Makefile -o -name Makefile.in \
	-o -name Makefile.am \) -print0 | xargs -0 rm -rf \;

uninstall-hook:
	rm -rf ${DESTDIR}/$(datadir)/${PACKAGE}/actions
	rm -rf ${DESTDIR}/$(datadir)/${PACKAGE}/examples
	rm -rf ${DESTDIR}/$(datadir)/${PACKAGE}/templates

potfile:
	@echo Generating POTFILES...
	@rm -f $(srcdir)/po/POTFILES.in
	@rm -f $(srcdir)/po/POTFILES.skip
	@cd $(srcdir) && find src -name '*.c' -o -name '*.h' ! -name 'pathconfig.h' | sort >> po/POTFILES.in
	@cd $(srcdir) && find actions -name '*.xml' -o -name '*.scm' | sort >> po/POTFILES.in
	@cd $(srcdir) && echo org.denemo.Denemo.appdata.xml.in >> po/POTFILES.in
	@cd $(srcdir) && find tools -name '*.c' -o -name '*.h' | sort >> po/POTFILES.skip
	@cd $(srcdir) && find src -name 'pathconfig.h' | sort >> po/POTFILES.skip
	@cd $(srcdir) && find src -name 'lilypond.lang' | sort >> po/POTFILES.skip
	@cp $(srcdir)/po/POTFILES.in $(builddir)/po/POTFILES.in.bak
	@cp $(srcdir)/po/POTFILES.skip $(builddir)/po/POTFILES.skip.bak
	@mv $(builddir)/po/POTFILES.in.bak $(builddir)/po/POTFILES.in
	@mv $(builddir)/po/POTFILES.skip.bak $(builddir)/po/POTFILES.skip

pot:
	@echo Generating POT...
	$(MAKE) -C "$(builddir)/po" "$(GETTEXT_PACKAGE).pot"
#	cp "$(builddir)/po/$(GETTEXT_PACKAGE).pot" "$(builddir)/po/$(GETTEXT_PACKAGE)-$(VERSION).pot"

po/$(GETTEXT_PACKAGE).pot:pot

update-po:
	@echo Get po files from translationproject.org
	rsync -Lrtvz translationproject.org::tp/latest/denemo/ po

send-pot:dist
	echo "You can find in attachment the latest version of denemo.pot. This is an automatic message, but someone should answer you if you reply." | mailx -v -S from=denemo-devel@gnu.org -s "$(GETTEXT_PACKAGE)-$(VERSION).pot" -a $(GETTEXT_PACKAGE)-$(VERSION).tar.gz -a po/$(GETTEXT_PACKAGE)-$(VERSION).pot coordinator@translationproject.org

update-samples:
	find . -name '*.denemo' -exec src/denemo -n -a "(d-Save \"{}\")(d-Quit)" '{}' \;

