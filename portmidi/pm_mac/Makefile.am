noinst_LIBRARIES = libpm_mac.a
noinst_HEADERS = pmmac.h pmmacosxcm.h readbinaryplist.h  

libpm_mac_a_CFLAGS = $(GLIB_CFLAGS) -I$(top_srcdir)/portmidi -I$(top_srcdir)/portmidi/pm_mac -D_HAVE_PORTMIDI_ -D_DEBUG -DPM_CHECK_ERRORS=0 -DDEBUG
libpm_mac_a_SOURCES = pmmac.h pmmacosxcm.h readbinaryplist.h finddefault.c pmmac.c pmmacosxcm.c readbinaryplist.c
