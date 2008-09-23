/* commandfuncs.h
 * header file for functions invoked by user keypresses
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */


#ifndef COMMANDFUNCSH
#define COMMANDFUNCSH
#include <denemo/denemo.h> 

void
nudgerightward (DenemoGUI  *gui);

void
nudge_downward (DenemoGUI  *gui);

void
set_width_to_work_with (DenemoGUI  *gui);

void
adjustmeasurewidth (DenemoScore  *si, gint amount);

void
setcurrents (DenemoScore  *si);

void
adjuststaffheight (DenemoScore  *si, gint amount);

void
measureleft (DenemoGUI  *gui);

void
measureright (DenemoGUI  *gui);


gboolean
swapstaffs (GtkAction *action, gpointer param);
gboolean
splitstaffs (GtkAction *action, gpointer param);
gboolean
joinstaffs (GtkAction *action, gpointer param);

gboolean
swapmovements (GtkAction *action, gpointer param);

gboolean
staffup (DenemoGUI  *gui);

gboolean
staffdown (DenemoGUI  *gui);

gboolean
voiceup (DenemoGUI  *si);

gboolean
voicedown (DenemoGUI  *si);

void
cursorleft (DenemoGUI  *gui);

void
cursorright (DenemoGUI  *gui);

void
cursorup (DenemoGUI  *gui);

void
cursordown (DenemoGUI  *gui);

void
shiftcursor (DenemoGUI  *gui, gint note_value);

void
setenshift (DenemoScore * si, gint enshift);

void
changeduration (DenemoScore  *si, gint duration);

gboolean
tonechange (DenemoScore  *si, gboolean remove);

void
incrementenshift (DenemoGUI  *gui, gint direction);

void
change_stem_directive (DenemoScore *si, enum stemdirections amount);

void
changedots (DenemoScore  *si, gint amount);


void
appendmeasures (DenemoScore  *si, gint number);

void
insertclone (DenemoGUI  *gui);

void
toend (GtkAction *action, gpointer param);

void
tohome (GtkAction *action, gpointer param);


void
inserttuplet (DenemoScore  *si, tuplet_type type);

void
object_insert (DenemoGUI  *gui, DenemoObject *mudela_obj_new);

void
stem_directive_insert (GtkAction *action, gpointer param);

void
toggle_begin_slur (DenemoGUI *gui);

void
toggle_end_slur (DenemoGUI  *gui);

void insertgrace(DenemoGUI  *gui);

void
toggle_start_crescendo (DenemoGUI *gui);

void
toggle_end_crescendo (DenemoGUI *gui);

void
toggle_start_diminuendo (DenemoGUI *gui);

void
toggle_end_diminuendo (DenemoGUI *gui);

void insertion_point (DenemoScore  *si);

void
caution (DenemoScore  *si);

void displayhelper(DenemoGUI  *si);

gboolean auto_save_document_timeout(DenemoGUI *gui);



void delete_staff_current(GtkAction *action, gpointer param);
void delete_staff_before(GtkAction *action, gpointer param);
void delete_staff_after(GtkAction *action, gpointer param);

void
appendmeasurestoentirescore(DenemoScore *si, gint number);


void deletemeasure(DenemoGUI * gui);
void deletemeasureallstaffs(DenemoGUI * gui);
void deleteobject(DenemoGUI *gui);

void
beamandstemdirhelper (DenemoScore * si);

void 
nextrhythm(DenemoGUI *gui);
void 
insert_rhythm_pattern(DenemoGUI *gui);

#endif /*COMMANDFUNCSH */
