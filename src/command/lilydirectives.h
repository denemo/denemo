/* lilydirectives.h
 *
 *
 * (c) 2000-2005 Adam Tee <eenajt@electeng.leeds.ac.uk>
 *
 */

#ifndef LILYDIRECTIVE_H

#define LILYDIRECTIVE_H

#include <denemo/denemo.h>

DenemoDirective *get_note_directive_number (gint num);

gboolean compare_directive (DenemoDirective *d1, DenemoDirective *d2);
gboolean compare_directive_lists (GList *dlist1, GList *dlist2);
gchar *difference_of_directive_lists (GList *dlist1, GList *dlist2);
gchar *difference_of_directive (DenemoDirective *d1, DenemoDirective *d2);


gboolean unpopulate_menu (GtkWidget * menu);

gchar *get_scoretitle (void);
void widget_for_directive (DenemoDirective * directive, void fn ());
void widget_for_staff_directive (DenemoDirective * directive, GtkMenu * menu);
void widget_for_voice_directive (DenemoDirective * directive, GtkMenu * menu);
void widget_for_movementcontrol_directive (DenemoDirective * directive);
void widget_for_header_directive (DenemoDirective * directive);
void widget_for_layout_directive (DenemoDirective * directive);
gboolean text_edit_chord_directive (gchar * tag);
gboolean text_edit_note_directive (gchar * tag);
gboolean text_edit_clef_directive (gchar * tag);
gboolean text_edit_timesig_directive (gchar * tag);
gboolean text_edit_tuplet_directive (gchar * tag);
gboolean text_edit_stemdirective_directive (gchar * tag);
gboolean text_edit_keysig_directive (gchar * tag);
gboolean text_edit_standalone_directive (gchar * tag);

gboolean text_edit_score_directive (gchar * tag);// and all the others ... paper ...etc



void set_action_script_for_tag (gchar * tag, gchar * script);
gchar *get_action_script (gchar * name);

void edit_object_directive (DenemoAction * action, DenemoScriptParam * param);
void edit_clef_directive (DenemoAction * action, DenemoScriptParam * param);
void edit_keysig_directive (DenemoAction * action, DenemoScriptParam * param);
void edit_timesig_directive (DenemoAction * action, DenemoScriptParam * param);
void edit_staff_directive (DenemoAction * action, DenemoScriptParam * param);
void edit_voice_directive (DenemoAction * action, DenemoScriptParam * param);
void edit_score_directive (DenemoAction * action, DenemoScriptParam * param);
void edit_movement_directive (DenemoAction * action, DenemoScriptParam * param);
void edit_object_type (DenemoAction * action, DenemoScriptParam * param);       //FIXME move to ???

gboolean low_level_directive_edit (DenemoDirective *directive);


void standalone_directive (DenemoAction * action, DenemoScriptParam * param);
void put_standalone_directive (gchar *tag, gint pixelwidth);
void delete_chord_or_note_directive (DenemoAction * action, DenemoScriptParam * param);
gchar *get_editscript_filename (gchar * tag);

DenemoDirective *get_movementcontrol_directive (gchar * tag);
DenemoDirective *get_scoreheader_directive (gchar * tag);
DenemoDirective *get_paper_directive (gchar * tag);
DenemoDirective *get_layout_directive (gchar * tag);
DenemoDirective *get_header_directive (gchar * tag);
DenemoDirective *get_score_directive (gchar * tag);


DenemoDirective *get_staff_directive (gchar * tag);
DenemoDirective *get_voice_directive (gchar * tag);

DenemoDirective *get_clef_directive (gchar * tag);
DenemoDirective *get_keysig_directive (gchar * tag);
DenemoDirective *get_timesig_directive (gchar * tag);
DenemoDirective *get_tuplet_directive (gchar * tag);
DenemoDirective *get_stemdirective_directive (gchar * tag);
DenemoDirective *get_note_directive (gchar * tag);
DenemoDirective *get_chord_directive (gchar * tag);
DenemoDirective *get_object_directive (gchar * tag);


DenemoDirective *find_directive (GList * directives, gchar * tag);

gchar *get_nth_strict_note_tag (gint index);
const gchar *strict_note_directive_get_tag (gchar *tag);

void delete_directives (GList ** pdirectives);

gboolean delete_chord_directive (gchar * tag);
gboolean delete_note_directive (gchar * tag);
gboolean delete_staff_directive (gchar * tag);
gboolean delete_voice_directive (gchar * tag);

gboolean choose_tag_at_cursor (gchar **ptag);

gchar *get_script_for_directive (DenemoDirective* directive, gchar * what);

#define DECL_PUT_FIELD(what, field) gboolean what##_directive_put_##field(gchar *tag, gchar *value);
#define DECL_GET_FIELD(what, field) gchar *what##_directive_get_##field(gchar *tag);
#define DECL_GET_INT(what, field) gint what##_directive_get_##field(gchar *tag);
#define DECL_PUT_INT(what, field) gboolean what##_directive_put_##field(gchar *tag, gint value);

#define DECL_GET_TAG_FUNC(what) gchar * what##_directive_get_tag(gchar * tag);
#define DECL_TEXT_EDIT_DIRECTIVE(what) gboolean text_edit_##what##_directive(gchar * tag);

#define DECL_PUT_GRAPHIC(what) gboolean what##_directive_put_graphic(gchar *tag, gchar *value);

DECL_GET_TAG_FUNC (staff)
DECL_GET_TAG_FUNC (movementcontrol)

DECL_TEXT_EDIT_DIRECTIVE (header)
DECL_TEXT_EDIT_DIRECTIVE (layout)
DECL_TEXT_EDIT_DIRECTIVE (movementcontrol)
DECL_TEXT_EDIT_DIRECTIVE (paper)
DECL_TEXT_EDIT_DIRECTIVE (scoreheader)
DECL_TEXT_EDIT_DIRECTIVE (staff)
DECL_TEXT_EDIT_DIRECTIVE (voice)

DECL_PUT_GRAPHIC (chord)
DECL_PUT_GRAPHIC (note)
DECL_PUT_GRAPHIC (standalone)
DECL_GET_FIELD (note, prefix)
DECL_GET_FIELD (note, postfix)
DECL_GET_FIELD (note, display)
DECL_GET_FIELD (note, grob)
DECL_PUT_FIELD (note, prefix)
DECL_PUT_FIELD (note, postfix)
DECL_PUT_FIELD (note, display)
DECL_PUT_FIELD (note, grob)
DECL_GET_FIELD (chord, prefix)
DECL_GET_FIELD (chord, postfix)
DECL_GET_FIELD (chord, display)
DECL_GET_FIELD (chord, grob)
DECL_PUT_FIELD (chord, prefix)
DECL_PUT_FIELD (chord, postfix)
DECL_PUT_FIELD (chord, display)
DECL_PUT_FIELD (chord, grob)
DECL_GET_FIELD (standalone, prefix)
DECL_GET_FIELD (standalone, postfix)
DECL_GET_FIELD (standalone, display)
DECL_PUT_FIELD (standalone, prefix)
DECL_PUT_FIELD (standalone, postfix)
DECL_PUT_FIELD (standalone, display)
DECL_PUT_FIELD (standalone, grob) DECL_GET_INT (note, minpixels) DECL_GET_INT (chord, minpixels) DECL_GET_INT (standalone, minpixels)
  // block to copy
DECL_PUT_GRAPHIC (clef)
DECL_GET_FIELD (clef, prefix)
DECL_GET_FIELD (clef, postfix)
DECL_GET_FIELD (clef, display)
DECL_PUT_FIELD (clef, prefix)
DECL_PUT_FIELD (clef, postfix)
DECL_PUT_FIELD (clef, display)
  // end of block to copy

DECL_PUT_GRAPHIC (score)
DECL_GET_FIELD (score, prefix)
DECL_GET_FIELD (score, postfix)
DECL_GET_FIELD (score, display)
DECL_PUT_FIELD (score, prefix)
DECL_PUT_FIELD (score, postfix)
DECL_PUT_FIELD (score, display)

DECL_PUT_FIELD (header, postfix)
DECL_PUT_FIELD (header, display)
DECL_PUT_FIELD (paper, postfix)
DECL_PUT_FIELD (scoreheader, postfix) DECL_PUT_GRAPHIC (score) DECL_PUT_GRAPHIC (staff) DECL_PUT_GRAPHIC (voice) DECL_PUT_GRAPHIC (scoreheader) DECL_PUT_GRAPHIC (header) DECL_PUT_GRAPHIC (paper) DECL_PUT_GRAPHIC (layout) DECL_PUT_GRAPHIC (movementcontrol)
  //FIXME x,y,tx,ty,gx,gy
DECL_PUT_FIELD(staff, prefix)
DECL_PUT_INT(staff, override)
DECL_PUT_FIELD(staff, postfix)
DECL_PUT_GRAPHIC(timesig)
DECL_PUT_GRAPHIC(tuplet)
DECL_PUT_GRAPHIC(stemdirective)
DECL_PUT_GRAPHIC(keysig)
#undef DECL_PUT_FIELD
#undef DECL_GET_FIELD
#undef DECL_PUT_INT
#undef DECL_GET_INT
void widget_for_directive_menu (DenemoDirective * directive, void fn (), GtkMenu * menu);
DenemoDirective *get_next_directive_at_cursor (void);
void edit_system_directive (void);
DenemoDirective *get_note_directive (gchar * tag);
gboolean wrong_layout (DenemoDirective *d, guint id);//TRUE if layout id is not allowed for directive d
#endif
