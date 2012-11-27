noinst_LIBRARIES = libpm_win.a
noinst_HEADERS = pmwinmm.h

libpm_win_a_CFLAGS = $(GLIB_CFLAGS) -I$(top_srcdir)/portmidi -I$(top_srcdir)/portmidi/pm_win -DWIN32 -D_WINDOWS -D_DEBUG -DPM_CHECK_ERRORS=0 -DDEBUG
libpm_win_a_SOURCES = pmwin.c pmwinmm.c
