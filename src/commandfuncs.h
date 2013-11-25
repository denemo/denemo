/* commandfuncs.h
 * header file for functions invoked by user keypresses
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */


#ifndef COMMANDFUNCSH
#define COMMANDFUNCSH
#include <denemo/denemo.h>

void nudgerightward (DenemoGUI * gui);

void nudge_downward (DenemoGUI * gui);

void set_width_to_work_with (DenemoGUI * gui);

void adjustmeasurewidth (DenemoScore * si, gint amount);

void setcurrents (DenemoScore * si);

void adjuststaffheight (DenemoScore * si, gint amount);

void measureleft (GtkAction * action, DenemoScriptParam * param);

void measureright (GtkAction * action, DenemoScriptParam * param);

void movetomeasureleft (GtkAction * action, DenemoScriptParam * param);

void movetomeasureright (GtkAction * action, DenemoScriptParam * param);
void movetostart (GtkAction * action, DenemoScriptParam * param);
void movetoend (GtkAction * action, DenemoScriptParam * param);
gboolean swapstaffs (GtkAction * action, DenemoScriptParam * param);
gboolean splitstaffs (GtkAction * action, DenemoScriptParam * param);
gboolean joinstaffs (GtkAction * action, DenemoScriptParam * param);

gboolean swapmovements (GtkAction * action, DenemoScriptParam * param);

gboolean staffup (GtkAction * action, DenemoScriptParam * param);

gboolean staffdown (GtkAction * action, DenemoScriptParam * param);

gboolean movetostaffup (GtkAction * action, DenemoScriptParam * param);

gboolean movetostaffdown (GtkAction * action, DenemoScriptParam * param);

gboolean voiceup (GtkAction * action, DenemoScriptParam * param);

gboolean voicedown (GtkAction * action, DenemoScriptParam * param);

gboolean movetovoiceup (GtkAction * action, DenemoScriptParam * param);

gboolean movetovoicedown (GtkAction * action, DenemoScriptParam * param);



gboolean cursorleft (GtkAction * action, DenemoScriptParam * param);

gboolean cursorright (GtkAction * action, DenemoScriptParam * param);

gboolean movecursorleft (GtkAction * action, DenemoScriptParam * param);

gboolean movecursorright (GtkAction * action, DenemoScriptParam * param);

gboolean cursor_to_next_object (gboolean within_measure, gboolean stopping_at_empty);
gboolean cursor_to_prev_object (gboolean within_measure, gboolean stopping_at_empty);
gboolean cursor_to_next_selected_object (void);
gboolean cursor_to_prev_selected_object (void);
gboolean cursor_to_next_standalone_directive (void);
gboolean cursor_to_prev_standalone_directive (void);
gboolean cursor_to_next_standalone_in_measure (void);
gboolean cursor_to_prev_standalone_in_measure (void);

gboolean cursor_to_next_chord (void);
gboolean cursor_to_prev_chord (void);
gboolean cursor_to_next_chord_in_measure (void);
gboolean cursor_to_prev_chord_in_measure (void);
gboolean cursor_to_next_note (void);
gboolean cursor_to_prev_note (void);

void cursorup (GtkAction * action, DenemoScriptParam * param);

void cursordown (GtkAction * action, DenemoScriptParam * param);

void shiftcursor (DenemoGUI * gui, gint note_value);

void setenshift (DenemoScore * si, gint enshift);

void changeduration (DenemoScore * si, gint duration);

gboolean delete_chordnote (DenemoGUI * gui);

gboolean insert_chordnote (DenemoGUI * gui);

void incrementenshift (DenemoGUI * gui, gint direction);

void change_stem_directive (DenemoScore * si, enum stemdirections amount);

void changedots (DenemoScore * si, gint amount);


void appendmeasures (DenemoScore * si, gint number);

void insertclone (DenemoGUI * gui);

void toend (GtkAction * action, DenemoScriptParam * param);

void tohome (GtkAction * action, DenemoScriptParam * param);


void inserttuplet (DenemoScore * si, tuplet_type type);

void object_insert (DenemoGUI * gui, DenemoObject * mudela_obj_new);

void stem_directive_insert (GtkAction * action, DenemoScriptParam * param);

void toggle_begin_slur (GtkAction * action, DenemoScriptParam* param);

void toggle_end_slur (GtkAction * action, DenemoScriptParam* param);

void insertgrace (DenemoGUI * gui);

void toggle_start_crescendo (GtkAction * action, DenemoScriptParam* param);

void toggle_end_crescendo (GtkAction * action, DenemoScriptParam* param);

void toggle_start_diminuendo (GtkAction * action, DenemoScriptParam* param);

void toggle_end_diminuendo (GtkAction * action, DenemoScriptParam* param);

void insertion_point (DenemoScore * si);
void insertion_point_for_type (DenemoScore * si, DenemoObjType type);

void caution (DenemoScore * si);

void displayhelper (DenemoGUI * si);

gboolean auto_save_document_timeout (DenemoGUI * gui);



void delete_staff_current (GtkAction * action, DenemoScriptParam * param);
void delete_staff_before (GtkAction * action, DenemoScriptParam * param);
void delete_staff_after (GtkAction * action, DenemoScriptParam * param);

void appendmeasurestoentirescore (DenemoScore * si, gint number);

void insertmeasureafter (GtkAction* action, DenemoScriptParam* param);
void addmeasureafter (GtkAction* action, DenemoScriptParam* param);
void insertmeasurebefore (GtkAction* action, DenemoScriptParam* param);

void deletemeasure (GtkAction* action, DenemoScriptParam* param);
void deletemeasureallstaffs (GtkAction* action, DenemoScriptParam* param);
void deleteobject (GtkAction* action, DenemoScriptParam* param);


void insert_note_following_pattern (DenemoGUI * gui);

void beamandstemdirhelper (DenemoScore * si);

void nextrhythm (GtkAction* action, DenemoScriptParam* param);
void insert_rhythm_pattern (GtkAction* action, DenemoScriptParam* param);
void toggle_tie (GtkAction * action, DenemoScriptParam * param);
gint get_prevailing_duration (void);
void dnm_inserttuplet (DenemoGUI * gui, tuplet_type type);
gboolean next_editable_note (void);
#endif /*COMMANDFUNCSH */
