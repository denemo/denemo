#include "common.h"

void
g_test_print(const char *fmt, ...)
{
	va_list argp;
	va_start(argp, fmt);
#ifdef G_OS_WIN32
  vprintf(fmt, argp);
#else
  gchar* str = g_strconcat("\e[7m", fmt, "\e[27m", NULL);
	vprintf(str, argp);
  g_free(str);
#endif
	va_end(argp);
}