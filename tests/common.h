#ifndef __UTILS_H__
#define __UTILS_H__

#include <glib.h>

#define DENEMO "../src/denemo"
#define EXAMPLE_DIR "examples"
#define FIXTURES_DIR "fixtures"
#define TEMP_DIR "tmp"
#define REFERENCE_DIR "references"

void g_test_print(const char *fmt, ...);
gboolean delete_if_exists(gchar* path);

#endif
