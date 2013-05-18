bin_PROGRAMS = denemo
dist_pkgdata_DATA = denemoui.xml instruments.xml
denemo_SOURCES = denemo_types.c \
	calculatepositions.c changenotehead.c \
	chordops.c clefdialog.c commandfuncs.c contexts.c draw.c \
	drawaccidentals.c drawclefs.c drawcursor.c drawkey.c drawdynamic.c \
	drawnotes.c drawselection.c drawstemdir.c drawtimesig.c drawtuplets.c \
	drawlyric.c dynamic.c drawfigure.c exportabc.c exportlilypond.c \
	articulations.c articulations.h \
	exportxml.c file.c \
	hairpin.c \
	help.c importxml.c importmusicxml.c importmidi.c kbd-custom.c kbd-interface.c \
	keyresponses.c keysigdialog.c figure.c 	main.c \
	measureops.c moveviewport.c mousing.c barline.c 	view.c http.c \
	mwidthdialog.c objops.c exportmidi.c 	instrumentname.c external.c source.c  sourceaudio.c audiofile.c scorelayout.c\
	playback.c drawfakechord.c fakechord.c \
	playbackprops.c prefdialog.c prefops.c	processstaffname.c \
	lyric.c scoreops.c scoreprops.c \
	selectops.c staffops.c \
	staffpropdialog.c drawbarline.c slurs.c \
	timedialog.c tomeasuredialog.c \
	tupletops.c utils.c graceops.c runsilent.c drawgrace.c	print.c \
	accwidths.h \
	calculatepositions.h changenotehead.h \
	 chordops.h commandfuncs.h \
	contexts.h  dynamic.h dialogs.h draw.h \
	drawingprims.h \
	exportabc.h exportlilypond.h  exportxml.h file.h \
	figure.h barline.h view.h http.h hairpin.h  help.h importxml.h  importmusicxml.h importmidi.h \
	kbd-custom.h kbd-interface.h graceops.h \
	keyresponses.h  \
	measureops.h mousing.h  moveviewport.h notewidths.h \
	objops.h \
	playbackprops.h prefops.h processstaffname.h \
	redefs.h scoreops.h \
	selectops.h staffops.h slurs.h texteditors.h texteditors.c twoints.h  tupletops.h utils.h \
	exportxml.h \
	xmldefs.h exportmidi.h instrumentname.h  print.h \
	lyric.h playback.h runsilent.h binreloc.c  binreloc.h \
	bookmarks.c bookmarks.h \
	external.h source.h sourceaudio.h audiofile.h scorelayout.h fakechord.h \
	parseinstruments.c parseinstruments.h keyboard.c \
	pitchentry.c pitchrecog.c pitchentry.h audio.h \
	drawlilydir.c lilydirectives.h lilydirectives.c displayanimation.c displayanimation.h keysigdialog.h \
	generated/callbacks.h generated/entries.h \
	generated/scheme_cb.h generated/scheme.h generated/register_commands.h \
	pathconfig.h \
	midi.c audiocapture.c \
	audiointerface.h eventqueue.h dummybackend.h portaudiobackend.h \
	portmidibackend.h midi.h ringbuffer.h portaudioutil.h \
	fluid.h portmidiutil.h \
  keymapio.c keymapio.h

if WITH_X11
  denemo_SOURCES += screenshot.c screenshot.h
else
  denemo_SOURCES +=
endif

noinst_LIBRARIES = libaudiobackend.a
libaudiobackend_a_SOURCES = audiointerface.c audiointerface.h eventqueue.c eventqueue.h dummybackend.c dummybackend.h
libaudiobackend_a_CFLAGS = -W -Wall -Wno-unused-parameter $(PLATFORM_CFLAGS) 

libaudiobackend_a_SOURCES += jackbackend.c jackbackend.h jackutil.c jackutil.h
libaudiobackend_a_SOURCES += ringbuffer.c ringbuffer.h
libaudiobackend_a_SOURCES += portaudiobackend.c portaudiobackend.h portaudioutil.c portaudioutil.h
libaudiobackend_a_SOURCES += portmidibackend.c portmidibackend.h portmidiutil.c portmidiutil.h
libaudiobackend_a_SOURCES += alsabackend.c alsabackend.h
libaudiobackend_a_SOURCES += fluid.c fluid.h


AM_CPPFLAGS = -I../intl -I$(top_srcdir)/intl -I$(top_srcdir)/include \
 $(BINRELOC_CFLAGS) \
 $(SMF_INCLUDE) \
 $(PORTMIDI_INCLUDE) \
-I$(top_srcdir)/libsffile \
-I$(top_srcdir)/pixmaps \
-DPREFIX=\"$(prefix)\" \
-DBINDIR=\"$(exec_prefix)/bin\" \
-DLOCALEDIR=\"${LOCALEDIR}\"\
-DSYSCONFDIR=\"$(sysconfdir)/\" \
-DPKGDATADIR=\"$(pkgdatadir)/\" \
-DDATAROOTDIR=\"$(datarootdir)/\" \
-DPKGNAME=\"denemo\" 


denemo_LDADD = $(INTLLIBS) $(SMF_LDD) libaudiobackend.a ../libsffile/libsffile.a

pathconfig.h:  $(top_builddir)/config.status
	-@rm pathconfig.tmp 
	@echo "Generating pathconfig.h..."
	@echo '#define DENEMO_LOAD_PATH "@denemo_load_path@"' >pathconfig.tmp
	@echo '#define DENEMO_BIN_PATH  "@denemo_bin_path@"' >>pathconfig.tmp
	@mv pathconfig.tmp $@	



noinst_HEADERS = parseinstruments.h keyboard.h

DISTCLEANFILES: pathconfig.h
