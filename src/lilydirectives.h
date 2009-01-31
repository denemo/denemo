/* lilydirectives.h
 *
 *
 * (c) 2000-2005 Adam Tee <eenajt@electeng.leeds.ac.uk>
 *
 */

#ifndef LILYDIRECTIVE_H

#define LILYDIRECTIVE_H

#include <denemo/denemo.h>


void
note_directive (GtkAction *action, gpointer param);
void
chord_directive (GtkAction *action, gpointer param);
void
standalone_directive (GtkAction *action, DenemoScriptParam *param);

#define DECL_PUT_FIELD(field) gboolean note_directive_put_##field(gchar *tag, gchar *value);
#define DECL_GET_FIELD(field) gchar *note_directive_get_##field(gchar *tag);
DECL_GET_FIELD(prefix)
DECL_GET_FIELD(postfix)
DECL_GET_FIELD(display)

DECL_PUT_FIELD(prefix)
DECL_PUT_FIELD(postfix)
DECL_PUT_FIELD(display)
#undef DECL_PUT_FIELD
#undef DECL_GET_FIELD
#endif
