
//#ifndef FAKECHORD_H

//#define FAKECHORD_H

#include <denemo/denemo.h>

void separate_fakechord_elements (gchar * fakechord, DenemoObject * curObj);

void fakechord_insert (GtkAction * action, DenemoScriptParam * param);
void delete_fakechords (GtkAction * action, DenemoScriptParam * param);
//#endif
