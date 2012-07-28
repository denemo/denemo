noinst_HEADERS = pminternal.h pmutil.h portmidi.h porttime.h
noinst_LIBRARIES = libportmidi.a

libportmidi_a_SOURCES = pminternal.h pmutil.h pmwinmm.c portmidi.c porttime.c ptwinmm.c pmutil.c pmwin.c pmwinmm.h portmidi.h porttime.h
libportmidi_a_CFLAGS = $(GLIB_CFLAGS) -DWIN32 -D_WINDOWS -D_DEBUG -DPM_CHECK_ERRORS=1 -DDEBUG
