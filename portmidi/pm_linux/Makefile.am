noinst_LIBRARIES = libpm_linux.a
noinst_HEADERS = pmlinux.h pmlinuxalsa.h

dnl PORTMIDI_INCLUDE='-I$(top_srcdir)/portmidi -I$(top_srcdir)/portmidi/pm_win'
libpm_linux_a_CFLAGS = $(GLIB_CFLAGS) -I$(top_srcdir)/portmidi -I$(top_srcdir)/portmidi/pm_linux -DPMALSA -D_HAVE_PORTMIDI_ -D_DEBUG -DPM_CHECK_ERRORS=0 -DDEBUG
libpm_linux_a_SOURCES = finddefault.c pmlinux.c pmlinuxalsa.c

