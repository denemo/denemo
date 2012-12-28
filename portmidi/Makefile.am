noinst_HEADERS = pminternal.h pmutil.h portmidi.h porttime.h
noinst_LIBRARIES = libportmidi.a

libportmidi_a_SOURCES = pminternal.h pmutil.h portmidi.h porttime.h pmutil.c portmidi.c porttime.c


if LINUX
  libportmidi_a_LIBADD = pm_linux/libpm_linux.a
  libportmidi_a_CFLAGS = $(GLIB_CFLAGS) -I$(top_srcdir)/portmidi -I$(top_srcdir)/portmidi/pm_linux -DPMALSA -D_HAVE_PORTMIDI_ -D_DEBUG -DPM_CHECK_ERRORS=0 -DDEBUG
  SUBDIRS = pm_linux
endif

if MINGW
  libportmidi_a_LIBADD = pm_win/libpm_win.a
  libportmidi_a_CFLAGS = $(GLIB_CFLAGS) -I$(top_srcdir)/portmidi -I$(top_srcdir)/portmidi/pm_win -D_HAVE_PORTMIDI_ -DWIN32 -D_WINDOWS -D_DEBUG -DPM_CHECK_ERRORS=0 -DDEBUG
  SUBDIRS = pm_win
endif

if DARWIN
  libportmidi_a_LIBADD = pm_mac/libpm_mac.a
  libportmidi_a_CFLAGS = $(GLIB_CFLAGS) -I$(top_srcdir)/portmidi -I$(top_srcdir)/portmidi/pm_mac -D_HAVE_PORTMIDI_ -D_DEBUG -DPM_CHECK_ERRORS=0 -DDEBUG
  SUBDIRS = pm_mac
endif
