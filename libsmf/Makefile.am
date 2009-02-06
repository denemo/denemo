noinst_LIBRARIES = libsmf.a
libsmf_a_SOURCES = smf.h smf_private.h smf.c smf_decode.c smf_load.c smf_save.c smf_tempo.c
libsmf_a_CFLAGS = $(GLIB_CFLAGS) -DG_LOG_DOMAIN=\"libsmf\"

bin_PROGRAMS = smfsh
smfsh_SOURCES = smfsh.c
smfsh_CFLAGS = $(GLIB_CFLAGS) -DG_LOG_DOMAIN=\"smfsh\"
smfsh_LDADD = libsmf.a $(GLIB_LIBS) $(READLINE_LIBS)

