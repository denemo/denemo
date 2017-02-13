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

void measureleft (DenemoAction * action, DenemoScriptParam * param);

void measureright (DenemoAction * action, DenemoScriptParam * param);

void movetomeasureleft (DenemoAction * action, DenemoScriptParam * param);

void movetomeasureright (DenemoAction * action, DenemoScriptParam * param);
void movetostart (DenemoAction * action, DenemoScriptParam * param);
void movetoend (DenemoAction * action, DenemoScriptParam * param);
gboolean swapstaffs (DenemoAction * action, DenemoScriptParam * param);
gboolean splitstaffs (DenemoAction * action, DenemoScriptParam * param);
gboolean joinstaffs (DenemoAction * action, DenemoScriptParam * param);

gboolean swapmovements (DenemoAction * action, DenemoScriptParam * param);

gboolean staffup (DenemoAction * action, DenemoScriptParam * param);

gboolean staffdown (DenemoAction * action, DenemoScriptParam * param);

gboolean movetostaffup (DenemoAction * action, DenemoScriptParam * param);

gboolean movetostaffdown (DenemoAction * action, DenemoScriptParam * param);

gboolean voiceup (DenemoAction * action, DenemoScriptParam * param);

gboolean voicedown (DenemoAction * action, DenemoScriptParam * param);

gboolean movetovoiceup (DenemoAction * action, DenemoScriptParam * param);

gboolean movetovoicedown (DenemoAction * action, DenemoScriptParam * param);



gboolean cursorleft (DenemoAction * action, DenemoScriptParam * param);

gboolean cursorright (DenemoAction * action, DenemoScriptParam * param);

gboolean movecursorleft (DenemoAction * action, DenemoScriptParam * param);

gboolean movecursorright (DenemoAction * action, DenemoScriptParam * param);

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
void cursorup (DenemoAction * action, DenemoScriptParam * param);

void cursordown (DenemoAction * action, DenemoScriptParam * param);

void change_duration (gint duration);
void edit_or_append_pitch (gint note_value, gboolean absolute);
void insert_pitch (DenemoProject * gui, gint note_value);
void move_to_pitch (DenemoProject * gui, gint note_value);
void setenshift (DenemoMovement * si, gint enshift);

void changeduration (DenemoMovement * si, gint duration);

gboolean delete_chordnote (DenemoProject * gui);

gboolean insert_chordnote (DenemoProject * gui);

gboolean insert_or_delete_chordnote (gint enshift);

void incrementenshift (DenemoProject * gui, gint direction);

void change_stem_directive (DenemoMovement * si, enum stemdirections amount);

void changedots (DenemoMovement * si, gint amount);


void appendmeasures (DenemoMovement * si, gint number);

void insertclone (DenemoProject * gui);

void toend (DenemoAction * action, DenemoScriptParam * param);

void tohome (DenemoAction * action, DenemoScriptParam * param);


void inserttuplet (DenemoMovement * si, tuplet_type type);

void object_insert (DenemoProject * gui, DenemoObject * mudela_obj_new);

void stem_directive_insert (DenemoAction * action, DenemoScriptParam * param);

void toggle_begin_slur (DenemoAction * action, DenemoScriptParam* param);

void toggle_end_slur (DenemoAction * action, DenemoScriptParam* param);

void insertgrace (DenemoProject * gui);

void toggle_start_crescendo (DenemoAction * action, DenemoScriptParam* param);

void toggle_end_crescendo (DenemoAction * action, DenemoScriptParam* param);

void toggle_start_diminuendo (DenemoAction * action, DenemoScriptParam* param);

void toggle_end_diminuendo (DenemoAction * action, DenemoScriptParam* param);

void insertion_point (DenemoMovement * si);
void insertion_point_for_type (DenemoMovement * si, DenemoObjType type);

void caution (DenemoMovement * si);

void displayhelper (DenemoProject * si);

gboolean auto_save_document_timeout (DenemoProject * gui);



void delete_staff_current (DenemoAction * action, DenemoScriptParam * param);
void delete_staff_before (DenemoAction * action, DenemoScriptParam * param);
void delete_staff_after (DenemoAction * action, DenemoScriptParam * param);

void appendmeasurestoentirescore (DenemoMovement * si, gint number);

void insertmeasureafter (DenemoAction* action, DenemoScriptParam* param);
void addmeasureafter (DenemoAction* action, DenemoScriptParam* param);
void insertmeasurebefore (DenemoAction* action, DenemoScriptParam* param);

void deletemeasure (DenemoAction* action, DenemoScriptParam* param);
void deletemeasureallstaffs (DenemoAction* action, DenemoScriptParam* param);
void deleteobject (DenemoAction* action, DenemoScriptParam* param);


void insert_note_following_pattern (DenemoProject * gui);

void beamandstemdirhelper (DenemoMovement * si);

void nextrhythm (DenemoAction* action, DenemoScriptParam* param);
void insert_rhythm_pattern (DenemoAction* action, DenemoScriptParam* param);
void toggle_tie (DenemoAction * action, DenemoScriptParam * param);
gint get_prevailing_duration (void);
void dnm_inserttuplet (DenemoProject * gui, tuplet_type type);
gboolean next_editable_note (void);
gboolean next_insert_or_editable_note (void);
gboolean insert_marked_midi_note (void);
void insert_chord (GList *note_data, gint duration);


#endif /*COMMANDFUNCSH */
