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
edit_directive(GtkAction *action,  DenemoScriptParam *param);
void
note_directive (GtkAction *action, gpointer param);
void
chord_directive (GtkAction *action, gpointer param);
void
standalone_directive (GtkAction *action, DenemoScriptParam *param);

#define DECL_PUT_FIELD(what, field) gboolean what##_directive_put_##field(gchar *tag, gchar *value);
#define DECL_GET_FIELD(what, field) gchar *what##_directive_get_##field(gchar *tag);
#define DECL_GET_INT(what, field) gint what##_directive_get_##field(gchar *tag);
#define DECL_PUT_INT(what, field) gboolean what##_directive_get_##field(gchar *tag, gint value);

#define DECL_PUT_GRAPHIC(what) gboolean what##_directive_put_graphic(gchar *tag, gchar *value);
DECL_PUT_GRAPHIC(chord)
DECL_PUT_GRAPHIC(note)
DECL_PUT_GRAPHIC(standalone)


DECL_GET_FIELD(note, prefix)
DECL_GET_FIELD(note, postfix)
DECL_GET_FIELD(note, display)

DECL_PUT_FIELD(note, prefix)
DECL_PUT_FIELD(note, postfix)
DECL_PUT_FIELD(note, display)


DECL_GET_FIELD(chord, prefix)
DECL_GET_FIELD(chord, postfix)
DECL_GET_FIELD(chord, display)

DECL_PUT_FIELD(chord, prefix)
DECL_PUT_FIELD(chord, postfix)
DECL_PUT_FIELD(chord, display)

DECL_GET_FIELD(standalone, prefix)
DECL_GET_FIELD(standalone, postfix)
DECL_GET_FIELD(standalone, display)

DECL_PUT_FIELD(standalone, prefix)
DECL_PUT_FIELD(standalone, postfix)
DECL_PUT_FIELD(standalone, display)


DECL_GET_INT(note, minpixels)
DECL_GET_INT(chord, minpixels)
DECL_GET_INT(standalone, minpixels)
     //FIXME x,y,tx,ty,gx,gy

#undef DECL_PUT_FIELD
#undef DECL_GET_FIELD
#undef DECL_PUT_INT
#undef DECL_GET_INT
#endif
