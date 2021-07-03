bin_PROGRAMS = denemo
dist_pkgdata_DATA = instruments.xml lilypond.lang
denemo_SOURCES = \
  audio/audio.h \
  audio/audiocapture.c \
  audio/audiocapture.h \
  audio/instrumentname.c \
  audio/instrumentname.h \
  audio/midi.c \
  audio/midi.h \
  audio/midirecord.c \
  audio/midirecord.h \
  audio/parseinstruments.c \
  audio/parseinstruments.h \
  audio/pitchentry.c \
  audio/pitchentry.h \
  audio/pitchrecog.c \
  audio/pitchrecog.h \
  audio/playback.c \
  audio/playback.h \
  command/changenotehead.c \
  command/changenotehead.h \
  command/chord.c \
  command/chord.h \
  command/clef.c \
  command/clef.h \
  command/commandfuncs.c \
  command/commandfuncs.h \
  command/contexts.c \
  command/contexts.h \
  command/fakechord.c \
  command/fakechord.h \
  command/figure.c \
  command/figure.h \
  command/grace.c \
  command/grace.h \
  command/keyresponses.c \
  command/keyresponses.h \
  command/keysig.c \
  command/keysig.h \
  command/lilydirectives.c \
  command/lilydirectives.h \
  command/lyric.c \
  command/lyric.h \
  command/measure.c \
  command/measure.h \
  command/processstaffname.c \
  command/processstaffname.h \
  command/object.c \
  command/object.h \
  command/scorelayout.c \
  command/scorelayout.h \
  command/score.c \
  command/score.h \
  command/select.c \
  command/select.h \
  command/staff.c \
  command/staff.h \
  command/timesig.c \
  command/timesig.h \
  command/tuplet.c \
  command/tuplet.h \
  core/binreloc.c \
  core/binreloc.h \
  core/denemo_types.c \
  core/cache.c \
  core/cache.h \
  core/external.c \
  core/external.h \
  core/exportxml.c \
  core/exportxml.h \
  export/exportmusicxml.c \
  export/exportmusicxml.h \
  core/graphicseditor.c \
  core/graphicseditor.h \
  core/importxml.c \
  core/importxml.h \
  core/kbd-custom.c \
  core/kbd-custom.h \
  core/keyboard.c \
  core/keyboard.h \
  core/keymapio.c \
  core/keymapio.h \
  core/main.c \
  core/palettestorage.c \
  core/palettestorage.h \
  core/prefops.c \
  core/prefops.h \
  core/twoints.h \
  core/utils.c \
  core/utils.h \
  core/view.c \
  core/view.h \
  core/entries.h \
  display/accwidths.h \
  display/calculatepositions.c \
  display/calculatepositions.h \
  display/displayanimation.c \
  display/displayanimation.h \
  display/drawaccidentals.c \
  display/drawbarline.c \
  display/draw.c \
  display/drawclefs.c \
  display/drawcursor.c \
  display/drawdynamic.c \
  display/drawfakechord.c \
  display/drawfigure.c \
  display/draw.h \
  display/drawingprims.h \
  display/drawkey.c \
  display/drawlilydir.c \
  display/drawlyric.c \
  display/drawnotes.c \
  display/drawselection.c \
  display/drawstemdir.c \
  display/drawtimesig.c \
  display/drawtuplets.c \
  display/hairpin.c \
  display/hairpin.h \
  display/notewidths.h \
  display/slurs.c \
  display/slurs.h \
  export/audiofile.c \
  export/audiofile.h \
  export/exportabc.c \
  export/exportabc.h \
  export/exportlilypond.c \
  export/exportlilypond.h \
  export/exportmidi.c \
  export/exportmidi.h \
  export/file.c \
  export/file.h \
  export/guidedimportmidi.c \
  export/guidedimportmidi.h \
  export/importmidi.c \
  export/importmidi.h \
  export/importmusicxml.c \
  export/importmusicxml.h \
  export/print.c \
  export/print.h \
  export/xmldefs.h \
  scripting/scheme-callbacks.c \
  scripting/scheme-callbacks.h \
  scripting/scheme-identifiers.c \
  scripting/scheme-identifiers.h \
  scripting/scheme_cb.h \
  scripting/scheme.h \
  source/sourceaudio.c \
  source/sourceaudio.h \
  printview/svgview.h \
  printview/svgview.c \
  ui/clefdialog.c \
  ui/dialogs.h \
  ui/help.c \
  ui/help.h \
  ui/kbd-interface.c \
  ui/kbd-interface.h \
  ui/keysigdialog.c \
  ui/keysigdialog.h \
  ui/mousing.c \
  ui/mousing.h \
  ui/moveviewport.c \
  ui/moveviewport.h \
  ui/mwidthdialog.c \
  ui/palettes.c \
  ui/palettes.h \
  ui/virtualkeyboard.c \
  ui/virtualkeyboard.h \
  ui/playbackprops.c \
  ui/playbackprops.h \
  ui/prefdialog.c \
  ui/scoreprops.c \
  ui/staffpropdialog.c \
  ui/texteditors.c \
  ui/texteditors.h \
  ui/timedialog.c \
  ui/tomeasuredialog.c \
  ui/tupletdialog.c \
  ui/markup.c \
  ui/markup.h \
  core/menusystem.c \
  core/menusystem.h
  
nodist_denemo_SOURCES = pathconfig.h


if HAVE_EVINCE
  denemo_SOURCES += \
    source/source.c \
    source/source.h \
    source/proof.c \
    source/proof.h \
    printview/markupview.h \
    printview/markupview.c \
    printview/printview.h \
    printview/printview.c
endif

noinst_LIBRARIES = libaudiobackend.a
libaudiobackend_a_CFLAGS = -W -Wall -Wno-unused-parameter $(PLATFORM_CFLAGS) 
libaudiobackend_a_SOURCES = \
  audio/alsabackend.c \
  audio/alsabackend.h \
  audio/audiointerface.c \
  audio/audiointerface.h \
  audio/dummybackend.c \
  audio/dummybackend.h \
  audio/eventqueue.c \
  audio/eventqueue.h \
  audio/fluid.c \
  audio/fluid.h \
  audio/jackbackend.c \
  audio/jackbackend.h \
  audio/jackutil.c \
  audio/jackutil.h \
  audio/portaudiobackend.c \
  audio/portaudiobackend.h \
  audio/portaudioutil.c \
  audio/portaudioutil.h \
  audio/portmidibackend.c \
  audio/portmidibackend.h \
  audio/portmidiutil.c \
  audio/portmidiutil.h \
  audio/ringbuffer.c \
  audio/ringbuffer.h

AM_CPPFLAGS = \
   $(BINRELOC_CFLAGS) \
   $(PORTMIDI_INCLUDE) \
  -I$(top_srcdir)/intl \
  -I$(top_srcdir)/include \
  -I$(top_srcdir)/libs/libsffile \
  -I$(top_srcdir)/pixmaps \
  -DPREFIX=\"$(prefix)\" \
  -DBINDIR=\"$(exec_prefix)/bin\" \
  -DLOCALEDIR=\"${LOCALEDIR}\"\
  -DSYSCONFDIR=\"$(sysconfdir)/\" \
  -DPKGDATADIR=\"$(pkgdatadir)/\" \
  -DDATAROOTDIR=\"$(datarootdir)/\" \
  -DPKGNAME=\"denemo\" \
  -DG_LOG_DOMAIN=\"Denemo\"

denemo_LDADD = $(INTLLIBS) libaudiobackend.a -L$(top_builddir)/libs/libsffile -lsffile

if !HAVE_SMF
  AM_CPPFLAGS += -I$(top_srcdir)/libs/libsmf
  denemo_LDADD += -L$(top_builddir)/libs/libsmf -lsmf
endif

pathconfig.h:  $(top_builddir)/config.status
	-@rm pathconfig.tmp 
	@echo "Generating pathconfig.h..."
	@echo '#define DENEMO_LOAD_PATH "@denemo_load_path@"' >pathconfig.tmp
	@echo '#define DENEMO_BIN_PATH  "@denemo_bin_path@"' >>pathconfig.tmp
	@mv pathconfig.tmp $@	

noinst_HEADERS = \
  audio/parseinstruments.h \
  core/keyboard.h

DISTCLEANFILES: pathconfig.h
