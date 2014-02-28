#ifndef __SCHEME_IDENTIFIERS_H__
#define __SCHEME_IDENTIFIERS_H__

#include <denemo/denemo.h>

void install_scm_function (gint nbargs, gchar* tooltip, gchar * name, gpointer callback);
void create_scheme_identfiers (void);

#endif