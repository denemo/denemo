bin_PROGRAMS = denemo
dist_pkgdata_DATA = denemoui.xml instruments.xml
denemo_SOURCES = articulations.c calculatepositions.c changenotehead.c \
	chordops.c clefdialog.c commandfuncs.c contexts.c draw.c \
	drawaccidentals.c drawclefs.c drawcursor.c drawkey.c drawdynamic.c \
	drawnotes.c drawselection.c drawstemdir.c drawtimesig.c drawtuplets.c \
	drawlyric.c dynamic.c drawfigure.c exportabc.c exportlilypond.c \
	exportxml.c file.c gcs.c \
	headerdialog.c hairpin.c \
	help.c importxml.c importmidi.c kbd-custom.c kbd-interface.c \
	keyresponses.c keysigdialog.c figure.c 	main.c \
	measureops.c midi.c moveviewport.c mousing.c barline.c 	view.c \
	mwidthdialog.c objops.c exportmidi.c 	instrumentname.c external.c \
	playback.c drawfakechord.c fakechord.c \
	playbackprops.c plugin.c prefdialog.c prefops.c	processstaffname.c \
	csoundplayback.c lyric.c scoreops.c scoreprops.c \
	selectops.c staffops.c \
	staffpropdialog.c drawbarline.c slurs.c \
	timedialog.c tomeasuredialog.c \
	tupletops.c utils.c graceops.c runsilent.c drawgrace.c	print.c \
	exportcsound.c lyparser.c  lylexer.c lyparserfuncs.c \
	accwidths.h \
	articulations.h calculatepositions.h changenotehead.h \
	 chordops.h commandfuncs.h \
	csoundplayback.h contexts.h  dynamic.h dialogs.h draw.h \
	drawingprims.h lyparser.h \
	exportabc.h exportlilypond.h  exportxml.h file.h \
	gcs.h \
	figure.h barline.h view.h hairpin.h  help.h importxml.h importmidi.h \
	kbd-custom.h kbd-interface.h graceops.h \
	keyresponses.h lyparserfuncs.h \
	measureops.h midi.h mousing.h  moveviewport.h notewidths.h \
	objops.h \
	playbackprops.h plugin.h prefops.h processstaffname.h \
	redefs.h scoreops.h \
	selectops.h staffops.h slurs.h twoints.h  tupletops.h utils.h \
	exportxml.h \
	xmldefs.h exportmidi.h instrumentname.h  print.h \
	exportcsound.h \
	lyric.h playback.h runsilent.h binreloc.c  binreloc.h \
	alsaplayback.c alsaplayback.h bookmarks.c bookmarks.h scorewizard.c \
	external.h scorewizard.h fakechord.h \
	parseinstruments.c parseinstruments.h keyboard.c \
	midiseq.h alsaseq.h alsaseq.c \
	pitchentry.c pitchrecog.c audiocapture.c pitchentry.h audio.h \
	drawlilydir.c lilydirectives.h lilydirectives.c keysigdialog.h \
	callbacks.h entries.h \
	transpose.c transpose.h \
	scheme.h scheme_cb.h register_commands.h

EXTRA_DIST = lylexer.l lyparser.y

localedir = $(datadir)/locale
INCLUDES = -I../intl -I$(top_srcdir)/intl -I$(top_srcdir)/include \
-I$(top_srcdir)/pixmaps \
-DLOCALEDIR=\"$(localedir)\" \
-DSYSCONFDIR=\"$(sysconfdir)/\" \
-DPKGDATADIR=\"$(pkgdatadir)/\"

denemo_LDADD = $(INTLLIBS) 
AM_CPPFLAGS = $(BINRELOC_CFLAGS)



lyparser.c lyparser.h: lyparser.y
	bison -d -ply $(srcdir)/lyparser.y 
	mv lyparser.tab.c lyparser.c
	mv lyparser.tab.h lyparser.h

lylexer.c: lylexer.l
	flex -Ply  -o$(srcdir)/lylexer.c $(srcdir)/lylexer.l  

MAINTAINERCLEANFILES = lylexer.c lyparser.c lyparser.h 

noinst_HEADERS = parseinstruments.h keyboard.h 
