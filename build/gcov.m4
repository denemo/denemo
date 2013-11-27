# Checks for existence of coverage tools:
#  * gcov
# 
# Sets ac_cv_check_gcov to yes if tooling is present
# and reports the executables to the variables LCOV, GCOVR and GENHTML.
AC_DEFUN([AC_TDD_GCOV],
[
  AC_ARG_ENABLE(gcov,
  AS_HELP_STRING([--enable-gcov],
		 [enable coverage testing with gcov]),
  [use_gcov=$enableval], [use_gcov=no])

  AM_CONDITIONAL(HAVE_GCOV, test "x$use_gcov" = "xyes")

  if test "x$use_gcov" = "xyes"; then
  # we need gcc:
  if test "$GCC" != "yes"; then
    AC_MSG_ERROR([GCC is required for --enable-gcov])
  fi

  # Check if ccache is being used
  AC_CHECK_PROG(SHTOOL, shtool, shtool)
  if test "$SHTOOL"; then
    AS_CASE([`$SHTOOL path $CC`],
                [*ccache*], [gcc_ccache=yes],
                [gcc_ccache=no])
  fi

  if test "$gcc_ccache" = "yes" && (test -z "$CCACHE_DISABLE" || test "$CCACHE_DISABLE" != "1"); then
    AC_MSG_ERROR([ccache must be disabled when --enable-gcov option is used. You can disable ccache by setting environment variable CCACHE_DISABLE=1.])
  fi

  # Remove all optimization flags from CFLAGS
  changequote({,})
  CFLAGS=`echo "$CFLAGS" | $SED -e 's/-O[0-9]*//g'`
  changequote([,])

  # Add the special gcc flags
  CFLAGS="$CFLAGS --coverage"
  CXXFLAGS="$CXXFLAGS --coverage"
  LDFLAGS="$LDFLAGS --coverage"
fi
]) # AC_TDD_GCOV

