/* commandfuncs.h
 * header file for functions invoked by user keypresses
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */


#ifndef COMMANDFUNCSH
#define COMMANDFUNCSH
#include <denemo/denemo.h>

void nudgerightward (DenemoProject * gui);

void nudge_downward (DenemoProject * gui);

void set_width_to_work_with (DenemoProject * gui);

void adjustmeasurewidth (DenemoMovement * si, gint amount);

void setcurrents (DenemoMovement * si);

void adjuststaffheight (DenemoMovement * si, gint amount);

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
gboolean cursor_to_nth_note_height(gint n);
gboolean cursor_to_next_note_height (void);
void cursorup (GtkAction * action, DenemoScriptParam * param);

void cursordown (GtkAction * action, DenemoScriptParam * param);

void shiftcursor (DenemoProject * gui, gint note_value);

void setenshift (DenemoMovement * si, gint enshift);

void changeduration (DenemoMovement * si, gint duration);

gboolean delete_chordnote (DenemoProject * gui);

gboolean insert_chordnote (DenemoProject * gui);

void incrementenshift (DenemoProject * gui, gint direction);

void change_stem_directive (DenemoMovement * si, enum stemdirections amount);

void changedots (DenemoMovement * si, gint amount);


void appendmeasures (DenemoMovement * si, gint number);

void insertclone (DenemoProject * gui);

void toend (GtkAction * action, DenemoScriptParam * param);

void tohome (GtkAction * action, DenemoScriptParam * param);


void inserttuplet (DenemoMovement * si, tuplet_type type);

void object_insert (DenemoProject * gui, DenemoObject * mudela_obj_new);

void stem_directive_insert (GtkAction * action, DenemoScriptParam * param);

void toggle_begin_slur (GtkAction * action, DenemoScriptParam* param);

void toggle_end_slur (GtkAction * action, DenemoScriptParam* param);

void insertgrace (DenemoProject * gui);

void toggle_start_crescendo (GtkAction * action, DenemoScriptParam* param);

void toggle_end_crescendo (GtkAction * action, DenemoScriptParam* param);

void toggle_start_diminuendo (GtkAction * action, DenemoScriptParam* param);

void toggle_end_diminuendo (GtkAction * action, DenemoScriptParam* param);

void insertion_point (DenemoMovement * si);
void insertion_point_for_type (DenemoMovement * si, DenemoObjType type);

void caution (DenemoMovement * si);

void displayhelper (DenemoProject * si);

gboolean auto_save_document_timeout (DenemoProject * gui);



void delete_staff_current (GtkAction * action, DenemoScriptParam * param);
void delete_staff_before (GtkAction * action, DenemoScriptParam * param);
void delete_staff_after (GtkAction * action, DenemoScriptParam * param);

void appendmeasurestoentirescore (DenemoMovement * si, gint number);

void insertmeasureafter (GtkAction* action, DenemoScriptParam* param);
void addmeasureafter (GtkAction* action, DenemoScriptParam* param);
void insertmeasurebefore (GtkAction* action, DenemoScriptParam* param);

void deletemeasure (GtkAction* action, DenemoScriptParam* param);
void deletemeasureallstaffs (GtkAction* action, DenemoScriptParam* param);
void deleteobject (GtkAction* action, DenemoScriptParam* param);


void insert_note_following_pattern (DenemoProject * gui);

void beamandstemdirhelper (DenemoMovement * si);

void nextrhythm (GtkAction* action, DenemoScriptParam* param);
void insert_rhythm_pattern (GtkAction* action, DenemoScriptParam* param);
void toggle_tie (GtkAction * action, DenemoScriptParam * param);
gint get_prevailing_duration (void);
void dnm_inserttuplet (DenemoProject * gui, tuplet_type type);
gboolean next_editable_note (void);
gboolean insert_marked_midi_note (void);
#endif /*COMMANDFUNCSH */
