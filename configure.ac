dnl Process this file with autoconf to produce a configure script.
AC_INIT([denemo], [2.2.11], [denemo-devel@gnu.org])
AC_CONFIG_SRCDIR([src/core/main.c])
AM_INIT_AUTOMAKE([tar-pax foreign dist-xz subdir-objects])
dnl AC_CANONICAL_SYSTEM
AC_CONFIG_HEADER(config.h)

AC_CONFIG_MACRO_DIR([build])
AM_MAINTAINER_MODE
AM_BINRELOC
dnl ALL_LINGUAS="es fr sv"
AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION([0.18])
AC_PROG_LN_S
AC_PROG_MKDIR_P
AM_PROG_CC_C_O

IT_PROG_INTLTOOL([0.35.0])
GETTEXT_PACKAGE=denemo
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED([GETTEXT_PACKAGE], ["$GETTEXT_PACKAGE"],
                   [The domain to use with gettext])
AM_GLIB_GNU_GETTEXT
LOCALEDIR=[${datadir}/locale]
AC_SUBST(LOCALEDIR)

dnl Checks for programs.
AC_PROG_CPP
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_YACC
AM_PROG_LEX
GUILE_FLAGS

AC_PROG_LIBTOOL

dnl Options for configuration
build_debug=no
AC_ARG_ENABLE(
  debug,
  AS_HELP_STRING([--enable-debug], [enable debug code @<:@default=no@:>@]),
  [
    if test "x$enableval" != "xno"; then
      build_debug=yes
      CFLAGS="-O0 -DDEBUG -g -Wall -Wextra -Wno-deprecated-declarations -Wno-sign-compare"
      CXXFLAGS="-O0 -DDEBUG -g -Wall -Wextra -Wno-deprecated-declarations -Wno-sign-compare"
    fi
  ])

AC_ARG_ENABLE(
  warnings,
  AS_HELP_STRING([--disable-warnings], [use warnings @<:@default=no@:>@]),
  [
    if test "x$enableval" = "xno"; then
      CFLAGS="$CFLAGS -w"
      CXXFLAGS="$CXXFLAGS -w"
    fi
  ])

dnl Enable color if available
AS_COMPILER_FLAG([-fdiagnostics-color=auto],
                 [CFLAGS="$CFLAGS -fdiagnostics-color=auto"])

AM_CONDITIONAL(ENABLE_GLIB_TEST, [test "x$enable_glibtest" = "xyes"])

AC_ARG_ENABLE(
  guile_1_8,
  AS_HELP_STRING([--enable-guile_1_8], [use GUILE_1_8 @<:@default=no@:>@]),
  [
    if test "x$enableval" != "xno"; then
      useguile_1_8=yes
      PKG_CHECK_MODULES(GUILE_1_8, guile-1.8 >= 1.8)
      LIBS="$LIBS $GUILE_1_8_LIBS"
      CFLAGS="$CFLAGS $GUILE_1_8_CFLAGS"
    fi
  ], [ useguile_1_8=no ])

AC_ARG_ENABLE(
  guile_2_0,
  AS_HELP_STRING([--enable-guile_2_0], [use GUILE_2_0 @<:@default=no@:>@]),
  [
    if test "x$enableval" != "xno"; then
      useguile_2_0=yes
      PKG_CHECK_MODULES(GUILE_2, guile-2.0 >= 2.0)
      LIBS="$LIBS $GUILE_2_LIBS"
      CFLAGS="$CFLAGS $GUILE_2_CFLAGS"
    fi
  ], [ useguile_2_0=no ])

if [test "x$useguile_1_8" = "xno"] && [test "x$useguile_2_0" = "xno"]; then
  PKG_CHECK_MODULES([GUILE_2], [guile-2.0], useguile_2_0=yes, useguile_2_0=no)
  LIBS="$LIBS $GUILE_2_LIBS"
  CFLAGS="$CFLAGS $GUILE_2_CFLAGS"
  if test "x$useguile_2_0" = "xno"; then
    PKG_CHECK_MODULES(GUILE_1_8, guile-1.8 >= 1.8)
    LIBS="$LIBS $GUILE_1_8_LIBS"
    CFLAGS="$CFLAGS $GUILE_1_8_CFLAGS"
    useguile_1_8=yes
  else
    useguile_2_0=yes
  fi
fi

PKG_CHECK_MODULES(GLIB, [glib-2.0 >= 2.30])
CFLAGS="$CFLAGS $GLIB_CFLAGS"
LIBS="$LIBS $GLIB_LIBS"

PKG_CHECK_MODULES(XML2, libxml-2.0 >= 2.3.10)
CFLAGS="$CFLAGS $XML2_CFLAGS"
LIBS="$LIBS $XML2_LIBS"

PKG_CHECK_MODULES(RSVG, librsvg-2.0 >= 2.0)
CFLAGS="$CFLAGS $RSVG_CFLAGS"
LIBS="$LIBS $RSVG_LIBS"

PKG_CHECK_MODULES(FONTCONFIG, fontconfig >= 2.2.0)
CFLAGS="$CFLAGS $FONTCONFIG_CFLAGS"
LIBS="$LIBS $FONTCONFIG_LIBS"

dnl The following is needed for g_thread_init
PKG_CHECK_MODULES(GTHREAD, gthread-2.0 >= 2.21.0)
CFLAGS="$CFLAGS $GTHREAD_LIBS"
LIBS="$LIBS $GTHREAD_LIBS"

dnl The following is needed for source audio files
PKG_CHECK_MODULES(SNDFILE, sndfile >= 1.0)
CFLAGS="$CFLAGS $SNDFILE_LIBS"
LIBS="$LIBS $SNDFILE_LIBS"

PKG_CHECK_MODULES(SMF, smf >= 1.3, havesmf=yes, havesmf=no)
AM_CONDITIONAL(HAVE_SMF, [test x$havesmf = xyes])
CFLAGS="$CFLAGS $SMF_CFLAGS"
LIBS="$LIBS $SMF_LIBS"

AC_ARG_ENABLE(
  jack,
  AS_HELP_STRING([--enable-jack], [use JACK @<:@default=no@:>@]),
  [
    if test "x$enableval" != "xno"; then
      usejack=yes
    fi
  ])
AM_CONDITIONAL(HAVE_JACK, [test x$usejack = xyes])

if test "x$usejack" = "xyes"; then
  PKG_CHECK_MODULES(JACK, jack >= 0.102.0)
  CFLAGS="$CFLAGS -D_HAVE_JACK_ $JACK_CFLAGS"
  LIBS="$LIBS -D_HAVE_JACK_ $JACK_LIBS"
fi

AC_ARG_ENABLE(
  evince,
  AS_HELP_STRING([--enable-evince], [use EVINCE @<:@default=yes@:>@]),
  [
    if test "x$enableval" == "xno"; then
      useevince=no
    else
      useevince=yes
    fi
  ], [ useevince=yes ])
AM_CONDITIONAL(HAVE_EVINCE, [test x$useevince = xyes])

AC_ARG_ENABLE(
  aubio,
  AS_HELP_STRING([--enable-aubio], [use AUBIO @<:@default=yes@:>@]),
  [
    if test "x$enableval" == "xno"; then
      useaubio=no
    else
      useaubio=yes
    fi
  ], [ useaubio=yes ])
AM_CONDITIONAL(HAVE_AUBIO, [test x$useaubio = xyes])
if test "x$useaubio" = "xyes"; then
      PKG_CHECK_MODULES(LIBAUBIO, aubio >= 0.4.0)
      CFLAGS="$CFLAGS $LIBAUBIO_CFLAGS -D_WITH_AUBIO_4_"
      LIBS="$LIBS $LIBAUBIO_LIBS"
fi
if test "x$useaubio" = "xno"; then
      CFLAGS="$CFLAGS -DDISABLE_AUBIO"
      LIBS="$LIBS -DDISABLE_AUBIO"
fi


AC_ARG_ENABLE(
  gtk2,
  AS_HELP_STRING([--enable-gtk2], [use GTK2 @<:@default=no@:>@]),
  [
    if test "x$enableval" != "xno"; then
      usegtk2=yes
      PKG_CHECK_MODULES(GTK2, gtk+-2.0 >= 2.0.3)
    fi
  ], [ usegtk2=no ])

AC_ARG_ENABLE(
  gtk3,
  AS_HELP_STRING([--enable-gtk3], [use GTK3 @<:@default=no@:>@]),
  [
    if test "x$enableval" != "xno"; then
      usegtk3=yes
      PKG_CHECK_MODULES(GTK3, gtk+-3.0 >= 3.0.0)
    fi
  ], [ usegtk3=no ])

if [test "x$usegtk2" = "xno"] && [test "x$usegtk3" = "xno"]; then
  PKG_CHECK_MODULES(GTK3, gtk+-3.0 >= 3.0.0, havegtk3=yes, havegtk3=no)
  if test "x$havegtk3" = "xno"; then
    PKG_CHECK_MODULES(GTK2, gtk+-2.0 >= 2.0.3)
    usegtk2=yes
  else
    usegtk3=yes
  fi
fi

if test "x$usegtk2" = "xyes"; then
  CFLAGS="$CFLAGS $GTK2_CFLAGS"
  LIBS="$LIBS $GTK2_LIBS"

  PKG_CHECK_MODULES(GTKSOURCEVIEW, gtksourceview-2.0 >= 2.0)
  CFLAGS="$CFLAGS $GTKSOURCEVIEW_CFLAGS"
  LIBS="$LIBS $GTKSOURCEVIEW_LIBS"

  if test "x$useevince" != "xno"; then
    PKG_CHECK_MODULES(EVINCE_2_0, evince-view-2.0 >= 2.0, have_evince2=yes, have_evince2=no)
    CFLAGS="$CFLAGS $EVINCE_2_0_CFLAGS"
    LIBS="$LIBS $EVINCE_2_0_LIBS"

    if test "x$have_evince2" = "xno"; then
      PKG_CHECK_MODULES(EVINCE_2_30, evince-view-2.30 >= 2.0, have_evince2_3=yes, have_evince2_3=no)
      CFLAGS="$CFLAGS $EVINCE_2_30_CFLAGS"
      LIBS="$LIBS $EVINCE_2_30_LIBS"
    fi

    if test "x$have_evince2_3" = "xno"; then
      PKG_CHECK_MODULES(EVINCE_2_32, evince-view-2.32 >= 2.0)
      CFLAGS="$CFLAGS $EVINCE_2_32_CFLAGS"
      LIBS="$LIBS $EVINCE_2_32_LIBS"
    fi
  fi
fi

if test "x$usegtk3" = "xyes"; then
  CFLAGS="$CFLAGS $GTK3_CFLAGS"
  LIBS="$LIBS $GTK3_LIBS"

  PKG_CHECK_MODULES(GTKSOURCEVIEW, gtksourceview-3.0 >= 3.0)
  CFLAGS="$CFLAGS $GTKSOURCEVIEW_CFLAGS"
  LIBS="$LIBS $GTKSOURCEVIEW_LIBS"

  if test "x$useevince" != "xno"; then
    PKG_CHECK_MODULES(EVINCE, evince-view-3.0 >= 3.0)
    CFLAGS="$CFLAGS $EVINCE_CFLAGS"
    LIBS="$LIBS $EVINCE_LIBS"
  fi
fi

if test "x$useevince" = "xyes"; then
  CFLAGS="$CFLAGS -DUSE_EVINCE"
  LIBS="$LIBS -DUSE_EVINCE"
fi

AC_ARG_ENABLE(
  alsa,
  AS_HELP_STRING([--enable-alsa], [use ALSA @<:@default=no@:>@]),
  [
    if test "x$enableval" != "xno"; then
      usealsa=yes
    fi
  ])
AM_CONDITIONAL(HAVE_ALSA, [test x$usealsa = xyes])

if test "x$usealsa" = "xyes"; then
  PKG_CHECK_MODULES(ALSA, alsa >= 1.0.0)
  CFLAGS="$CFLAGS $ALSA_CFLAGS -D_HAVE_ALSA_"
  LIBS="$LIBS $ALSA_LIBS"
fi


AC_ARG_ENABLE(
  fluidsynth,
  AS_HELP_STRING([--enable-fluidsynth], [use FLUIDSYNTH @<:@default=yes@:>@]),
  [
    if test "x$enableval" != "xno"; then
      usefluidsynth=yes
    fi
  ], [ usefluidsynth=yes ])
AM_CONDITIONAL(HAVE_FLUIDSYNTH, [test x$usefluidsynth = xyes])

if test "x$usefluidsynth" = "xyes"; then
  PKG_CHECK_MODULES(FLUIDSYNTH, fluidsynth >= 1.0.8)
  CFLAGS="$CFLAGS -D_HAVE_FLUIDSYNTH_ $FLUIDSYNTH_CFLAGS"
  LIBS="$LIBS -D_HAVE_FLUIDSYNTH_ $FLUIDSYNTH_LIBS"
fi

AC_ARG_ENABLE(
  rubberband,
  AS_HELP_STRING([--enable-rubberband], [use RUBBERBAND @<:@default=yes@:>@]),
  [
    if test "x$enableval" != "xno"; then
      userubberband=yes
    fi
  ], [ userubberband=yes ])
AM_CONDITIONAL(HAVE_RUBBERBAND, [test x$userubberband = xyes])

if test "x$userubberband" = "xyes"; then
  PKG_CHECK_MODULES(RUBBERBAND, rubberband >= 1.0.8)
  CFLAGS="$CFLAGS -D_HAVE_RUBBERBAND_ $FLUIDSYNTH_CFLAGS"
  LIBS="$LIBS -D_HAVE_RUBBERBAND_ $RUBBERBAND_LIBS"
fi



AC_ARG_ENABLE(
  portaudio,
  AS_HELP_STRING([--enable-portaudio], [use PORTAUDIO @<:@default=yes@:>@]),
  [
    if test "x$enableval" != "xno"; then
      useportaudio=yes
    fi
  ], [ useportaudio=yes ])
AM_CONDITIONAL(HAVE_PORTAUDIO, [test x$useportaudio = xyes])

if test "x$useportaudio" = "xyes"; then
  PKG_CHECK_MODULES(PORTAUDIO, portaudio-2.0 >= 19)
  CFLAGS="$CFLAGS -D_HAVE_PORTAUDIO_ $PORTAUDIO_CFLAGS"
  dnl CFLAGS="$CFLAGS $PORTAUDIO_CFLAGS"
  LIBS="$LIBS $PORTAUDIO_LIBS"

  PKG_CHECK_MODULES(FFTW, fftw3 >= 3.1.2)
  CFLAGS="$CFLAGS $FFTW_CFLAGS"
  LIBS="$LIBS $FFTW_LIBS"
fi

AC_ARG_ENABLE(
  portmidi,
  AS_HELP_STRING([--enable-portmidi], [use PORTMIDI @<:@default=yes@:>@]),
  [
    if test "x$enableval" != "xno"; then
      useportmidi=yes
    fi
  ], [ useportmidi=yes ])
AM_CONDITIONAL(HAVE_PORTMIDI, [test x$useportmidi = xyes])

if test "x$useportmidi" = "xyes"; then
  AC_CHECK_HEADER([portmidi.h], [],
  [
    AC_MSG_ERROR([Library 'portmidi' not found])
  ])
  PORTMIDI_CFLAGS="-D_HAVE_PORTMIDI_"

  AC_CHECK_LIB([portmidi],
               [Pm_Initialize],
               PORTMIDI_LIBS="-lportmidi $PORTMIDI_LIBS"
               )

  AC_CHECK_LIB([porttime],
               [Pt_Start],
               PORTMIDI_LIBS="-lporttime $PORTMIDI_LIBS",
               )

  if test "x$PORTMIDI_LIBS" = "x"; then
    AC_MSG_ERROR([Cannot link against portmidi nor porttime])
  fi

  CFLAGS="$CFLAGS $PORTMIDI_CFLAGS"
  LIBS="$LIBS $PORTMIDI_LIBS"
fi

AC_ARG_ENABLE(
  x11,
  AS_HELP_STRING([--enable-x11], [use X11 @<:@default=yes@:>@]),
  [
    if test "x$enableval" != "xno"; then
      enable_x11=yes
    fi
  ], [ enable_x11=yes ])
AM_CONDITIONAL(WITH_X11, [test x$enable_x11 = xyes])

if test "x$enable_x11" = "xyes"; then
  CFLAGS="$CFLAGS -D_HAVE_X11_"
  LIBS="$LIBS -D_HAVE_X11_"
fi

dnl Memory debugging
mem_debug=no
AC_ARG_ENABLE(
  mem,
  AS_HELP_STRING([--enable-mem], [enable memory debugging @<:@default=no@:>@]),
  [
    if test "x$enableval" != "xno"; then
      mem_debug=yes
    fi
  ])
if test "x$mem_debug" = "xyes"; then LDFLAGS="$LDFLAGS -lefence"; fi


dnl Build documentation
build_doc=no
AC_ARG_ENABLE(
  doc,
  AS_HELP_STRING(
    [--enable-doc],[enable documentation build @<:@default=no@:>@]),
  [
    if test "x$enableval" != "xno"; then
      build_doc=yes
    fi
  ])
AM_CONDITIONAL(DENEMO_BUILD_DOC,[test "$build_doc" = "yes"])

# check for gtk-doc
m4_ifdef([GTK_DOC_CHECK], [
GTK_DOC_CHECK([1.14],[--flavour no-tmpl])
],[
AM_CONDITIONAL([ENABLE_GTK_DOC], false)
])

# Test suite
GLIB_TESTS

dnl Check for coverage tool gcov
AC_TDD_GCOV

dnl Checks for header files.

AC_CHECK_HEADERS(sys/soundcard.h)
AC_CHECK_HEADERS(errno.h)
AC_CHECK_HEADERS(getopt.h sys/wait.h wait.h sys/time.h sys/resource.h)

AC_COMPILE_IFELSE(
  [AC_LANG_PROGRAM([#include <signal.h>], [int mysignal = SIGCHLD;])],
  [AC_DEFINE(HAVE_SIGCHLD, 1, [Define to 1 if SIGCHLD signal is available])])

AC_LINK_IFELSE(
  [AC_LANG_PROGRAM([#include <stdio.h>],
    [
      FILE * fp;
      flockfile (fp);
      ftrylockfile (fp);
      funlockfile (fp);
    ])],
  [AC_DEFINE(HAVE_FILE_LOCKS, 1, [Define to 1 if file locking is available])])

dnl Checks for typedefs, structures, and compiler characteristics.
dnl AC_HEADER_TIME

dnl Checks for library functions.

dnl Set PACKAGE_LOCALE_DIR in config.h.


if test "${prefix}" = "NONE"; then
  prefix=${ac_default_prefix}
  AC_SUBST(prefix)
fi

pkgdatadir=${datadir}/${PACKAGE}
PACKAGE_DATA_DIR=${datadir}/${PACKAGE}
AC_SUBST(pkgdatadir)
AC_SUBST(PACKAGE_DATA_DIR)

dnl
dnl Append the package name to sysconfdir.
dnl
sysconfdir=$sysconfdir/${PACKAGE}
AC_SUBST(sysconfdir)

dnl Set PACKAGE_SOURCE_DIR in config.h.
packagesrcdir=`cd $srcdir && pwd`
AC_DEFINE_UNQUOTED(
  PACKAGE_SOURCE_DIR, "${packagesrcdir}", [set package source dir])

dnl path magic for denemo guile modules
denemo_load_path=${PACKAGE_DATA_DIR}/modules
denemo_bin_path=${libdir}/${PACKAGE}/modules
AC_SUBST(denemo_load_path)
AC_SUBST(denemo_bin_path)

dnl Gettext stuff

AC_SUBST(CFLAGS)
AC_SUBST(CPPFLAGS)
AC_SUBST(LDFLAGS)


AC_OUTPUT(
[
  build/Makefile
  Makefile
  denemo.spec
  tools/Makefile
  src/Makefile
  pixmaps/Makefile
  docs/Makefile
  docs/transformations/Makefile
  docs/images/Makefile
  docs/reference/Makefile
  docs/reference/denemo/Makefile
  po/Makefile.in
  fonts/Makefile
  soundfonts/Makefile
  ui/Makefile
  tests/Makefile
  libs/libsffile/Makefile
  libs/libsmf/Makefile
])
