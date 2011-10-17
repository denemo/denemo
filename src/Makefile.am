bin_PROGRAMS = denemo denemoclient
dist_pkgdata_DATA = denemoui.xml instruments.xml
denemo_SOURCES = articulations.c calculatepositions.c changenotehead.c \
	chordops.c clefdialog.c commandfuncs.c contexts.c draw.c \
	drawaccidentals.c drawclefs.c drawcursor.c drawkey.c drawdynamic.c \
	drawnotes.c drawselection.c drawstemdir.c drawtimesig.c drawtuplets.c \
	drawlyric.c dynamic.c drawfigure.c exportabc.c exportlilypond.c \
	exportxml.c file.c gcs.c \
	hairpin.c \
	help.c importxml.c importmusicxml.c importmidi.c kbd-custom.c kbd-interface.c \
	keyresponses.c keysigdialog.c figure.c 	main.c \
	measureops.c midi.c moveviewport.c mousing.c barline.c 	view.c http.c \
	mwidthdialog.c objops.c exportmidi.c 	instrumentname.c external.c screenshot.c\
	jackmidi.c jackmidi.h \
	fluid.c fluid.h \
	device_manager.c device_manager.h \
	playback.c drawfakechord.c fakechord.c \
	playbackprops.c prefdialog.c prefops.c	processstaffname.c \
	lyric.c scoreops.c scoreprops.c \
	selectops.c staffops.c \
	staffpropdialog.c drawbarline.c slurs.c \
	timedialog.c tomeasuredialog.c \
	tupletops.c utils.c graceops.c runsilent.c drawgrace.c	print.c \
	accwidths.h \
	articulations.h calculatepositions.h changenotehead.h \
	 chordops.h commandfuncs.h \
	contexts.h  dynamic.h dialogs.h draw.h \
	drawingprims.h \
	exportabc.h exportlilypond.h  exportxml.h file.h \
	gcs.h \
	figure.h barline.h view.h http.h hairpin.h  help.h importxml.h  importmusicxml.h importmidi.h \
	kbd-custom.h kbd-interface.h graceops.h \
	keyresponses.h  \
	measureops.h midi.h mousing.h  moveviewport.h notewidths.h \
	objops.h \
	playbackprops.h prefops.h processstaffname.h \
	redefs.h scoreops.h \
	selectops.h staffops.h slurs.h texteditors.h texteditors.c twoints.h  tupletops.h utils.h \
	exportxml.h \
	xmldefs.h exportmidi.h instrumentname.h  print.h \
	lyric.h playback.h runsilent.h binreloc.c  binreloc.h \
	bookmarks.c bookmarks.h scorewizard.c \
	external.h screenshot.h scorewizard.h fakechord.h \
	parseinstruments.c parseinstruments.h keyboard.c \
	pitchentry.c pitchrecog.c audiocapture.c pitchentry.h audio.h \
	drawlilydir.c lilydirectives.h lilydirectives.c keysigdialog.h \
	callbacks.h entries.h \
	scheme_cb.h scheme.h register_commands.h \
	pathconfig.h

localedir = $(datadir)/locale
INCLUDES = -I../intl -I$(top_srcdir)/intl -I$(top_srcdir)/include \
 $(SMF_INCLUDE) \
-I$(top_srcdir)/pixmaps \
-DLOCALEDIR=\"$(localedir)\" \
-DSYSCONFDIR=\"$(sysconfdir)/\" \
-DPKGDATADIR=\"$(pkgdatadir)/\" \
-DDATAROOTDIR=\"$(datarootdir)/\" \
-DPKGNAME=\"denemo\" 


denemo_LDADD = $(INTLLIBS) $(SMF_LDD)
AM_CPPFLAGS = $(BINRELOC_CFLAGS)

pathconfig.h:  $(top_builddir)/config.status
	-@rm pathconfig.tmp 
	@echo "Generating pathconfig.h..."
	@echo '#define DENEMO_LOAD_PATH "@denemo_load_path@"' >pathconfig.tmp
	@echo '#define DENEMO_BIN_PATH  "@denemo_bin_path@"' >>pathconfig.tmp
	@mv pathconfig.tmp $@	

denemoclient_SOURCES = denemo_client.c


noinst_HEADERS = parseinstruments.h keyboard.h

DISTCLEANFILES: pathconfig.h
