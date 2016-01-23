#include <stdio.h>
#include <glib.h>
#include <glib/gstdio.h>
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

gboolean
delete_if_exists(gchar* path){
  if(g_file_test(path, G_FILE_TEST_EXISTS)){
    if(g_file_test(path, G_FILE_TEST_IS_DIR)){
      GError *error = NULL;
      GDir* dir = g_dir_open(path, 0, &error);
      gchar* filename = NULL;
      gchar* child = NULL;
      while (filename = g_dir_read_name(dir)){
        child = g_build_filename(path, filename, NULL);
        delete_if_exists(child);
        g_free(child);
      }
   }
   if(g_remove(path) < 0)
     g_warning("Could not remove %s", path);
  }
}

