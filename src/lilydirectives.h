/* lilydirectives.h
 *
 *
 * (c) 2000-2005 Adam Tee <eenajt@electeng.leeds.ac.uk>
 *
 */

#ifndef LILYDIRECTIVE_H

#define LILYDIRECTIVE_H

#include <denemo/denemo.h>

gchar * 
get_scoretitle(void);
gboolean 
text_edit_note_directive(gchar *tag);
void 
set_action_script_for_tag(gchar *tag, gchar *script);
void 
edit_object_directive(GtkAction *action,  DenemoScriptParam *param);
void 
edit_clef_directive(GtkAction *action,  DenemoScriptParam *param);
void 
edit_keysig_directive(GtkAction *action,  DenemoScriptParam *param);
void 
edit_timesig_directive(GtkAction *action,  DenemoScriptParam *param);
void 
edit_staff_directive(GtkAction *action,  DenemoScriptParam *param);
void 
edit_voice_directive(GtkAction *action,  DenemoScriptParam *param);
void 
edit_score_directive(GtkAction *action,  DenemoScriptParam *param);
void 
edit_movement_directive(GtkAction *action,  DenemoScriptParam *param);
void 
edit_object(GtkAction *action,  DenemoScriptParam *param);//FIXME move to ???
void
note_directive (GtkAction *action, DenemoScriptParam * param);
void
chord_directive (GtkAction *action, DenemoScriptParam * param);
void
standalone_directive (GtkAction *action, DenemoScriptParam *param);

void 
delete_object_directive(GtkAction *action,  DenemoScriptParam *param);


void
delete_directives (GList** pdirectives);

gboolean delete_chord_directive(gchar *tag);
gboolean delete_note_directive(gchar *tag);
gboolean delete_staff_directive(gchar *tag);
gboolean delete_voice_directive(gchar *tag);

#define DECL_PUT_FIELD(what, field) gboolean what##_directive_put_##field(gchar *tag, gchar *value);
#define DECL_GET_FIELD(what, field) gchar *what##_directive_get_##field(gchar *tag);
#define DECL_GET_INT(what, field) gint what##_directive_get_##field(gchar *tag);
#define DECL_PUT_INT(what, field) gboolean what##_directive_get_##field(gchar *tag, gint value);

#define DECL_PUT_GRAPHIC(what) gboolean what##_directive_put_graphic(gchar *tag, gchar *value);
#define DECL_PUT_GRAPHIC_WIDGET(what) gpointer what##_directive_put_graphic(gchar *tag, gchar *value);
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

     // block to copy
DECL_PUT_GRAPHIC(clef)
DECL_GET_FIELD(clef, prefix)
DECL_GET_FIELD(clef, postfix)
DECL_GET_FIELD(clef, display)
DECL_PUT_FIELD(clef, prefix)
DECL_PUT_FIELD(clef, postfix)
DECL_PUT_FIELD(clef, display)


     // end of block to copy


DECL_PUT_GRAPHIC_WIDGET(score)
DECL_PUT_GRAPHIC_WIDGET(staff)
DECL_PUT_GRAPHIC_WIDGET(voice)
DECL_PUT_GRAPHIC_WIDGET(scoreheader)
DECL_PUT_GRAPHIC_WIDGET(header)
DECL_PUT_GRAPHIC_WIDGET(paper)
DECL_PUT_GRAPHIC_WIDGET(layout)
DECL_PUT_GRAPHIC_WIDGET(movementcontrol)
     //FIXME x,y,tx,ty,gx,gy

#undef DECL_PUT_FIELD
#undef DECL_GET_FIELD
#undef DECL_PUT_INT
#undef DECL_GET_INT
#endif
