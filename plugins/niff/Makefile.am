INCLUDES = -I$(top_srcdir)/include

libdir = $(PACKAGE_PLUGIN_DIR)

lib_LTLIBRARIES = \
	libniff.la

libniff_la_SOURCES = chunks.c fcc.c register.c \
		     stbl.c tcbtable.c \
		     clt.c inherit.c  \
		     rfile.c stdcriff.c \
		     error.c names.c \
		     rwbytes.c storenif.c \
		     nfile.c \
		     rwniff.c table.c \
		     exportniff.h parse.c \
		     rwtypes.c tags.c
		     niffexporter.c

libniff_la_LDFLAGS = -module -version-info 1:0:0
libniff_la_LIBADD   = /usr/local/lib/niffio.a 
