/* keyresponses.h
 * function prototypes for responses to user keypresses
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */

#ifndef KEYRESPONSES_H
#define KEYRESPONSES_H

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include "command/commandfuncs.h"
#include <denemo/denemo.h>
#include "display/draw.h"
#include "command/select.h"
#include "command/chord.h"
#include "command/object.h"


gint scorearea_keypress_event (GtkWidget * widget, GdkEventKey * event);
gint scorearea_keyrelease_event (GtkWidget * widget, GdkEventKey * event);
gint scorearea_button_press (GtkWidget * widget, GdkEventButton * event);
gint scorearea_button_release (GtkWidget * widget, GdkEventButton * event);

gboolean intercept_scorearea_keypress (GdkEventKey * pevent);

void adjust_measure_less_width_key (DenemoAction* action, DenemoScriptParam *param);
void adjust_measure_more_width_key (DenemoAction* action, DenemoScriptParam *param);
void adjust_staff_less_height_key (DenemoAction* action, DenemoScriptParam *param);
void adjust_staff_more_height_key (DenemoAction* action, DenemoScriptParam *param);
void unset_selection_key (DenemoScriptParam *param);
void insert_chord_xkey (gint duration, DenemoScriptParam *param);
void change_or_append_A_key (DenemoAction* action, DenemoScriptParam *param);
void change_or_append_B_key (DenemoAction* action, DenemoScriptParam *param);
void change_or_append_C_key (DenemoAction* action, DenemoScriptParam *param);
void change_or_append_D_key (DenemoAction* action, DenemoScriptParam *param);
void change_or_append_E_key (DenemoAction* action, DenemoScriptParam *param);
void change_or_append_F_key (DenemoAction* action, DenemoScriptParam *param);
void change_or_append_G_key (DenemoAction* action, DenemoScriptParam *param);
void octave_up_key (DenemoAction* action, DenemoScriptParam *param);
void octave_down_key (DenemoAction* action, DenemoScriptParam *param);
void rest_toggle_key (DenemoAction* action, DenemoScriptParam *param);
void toggle_blank (DenemoAction* action, DenemoScriptParam *param);
void append_measure_key (DenemoAction* action, DenemoScriptParam *param);
void insert_measure_key (DenemoAction* action, DenemoScriptParam *param);


void insert_chord_0key (DenemoAction* action, DenemoScriptParam *param);
void insert_chord_1key (DenemoAction* action, DenemoScriptParam *param);
void insert_chord_2key (DenemoAction* action, DenemoScriptParam *param);
void insert_chord_3key (DenemoAction* action, DenemoScriptParam *param);
void insert_chord_4key (DenemoAction* action, DenemoScriptParam *param);
void insert_chord_5key (DenemoAction* action, DenemoScriptParam *param);
void insert_chord_6key (DenemoAction* action, DenemoScriptParam *param);
void insert_chord_7key (DenemoAction* action, DenemoScriptParam *param);
void insert_chord_8key (DenemoAction* action, DenemoScriptParam *param);
void insert_blankchord_xkey (gint duration, DenemoScriptParam *param);
void insert_blankchord_0key (DenemoAction* action, DenemoScriptParam *param);
void insert_blankchord_1key (DenemoAction* action, DenemoScriptParam *param);
void insert_blankchord_2key (DenemoAction* action, DenemoScriptParam *param);
void insert_blankchord_3key (DenemoAction* action, DenemoScriptParam *param);
void insert_blankchord_4key (DenemoAction* action, DenemoScriptParam *param);
void insert_blankchord_5key (DenemoAction* action, DenemoScriptParam *param);
void insert_blankchord_6key (DenemoAction* action, DenemoScriptParam *param);
void insert_blankchord_7key (DenemoAction* action, DenemoScriptParam *param);
void insert_blankchord_8key (DenemoAction* action, DenemoScriptParam *param);
void insert_rest_xkey(gint duration, DenemoScriptParam* param);
void insert_rest_0key (DenemoAction* action, DenemoScriptParam *param);
void insert_rest_1key (DenemoAction* action, DenemoScriptParam *param);
void insert_rest_2key (DenemoAction* action, DenemoScriptParam *param);
void insert_rest_3key (DenemoAction* action, DenemoScriptParam *param);
void insert_rest_4key (DenemoAction* action, DenemoScriptParam *param);
void insert_rest_5key (DenemoAction* action, DenemoScriptParam *param);
void insert_rest_6key (DenemoAction* action, DenemoScriptParam *param);
void insert_rest_7key (DenemoAction* action, DenemoScriptParam *param);
void insert_rest_8key (DenemoAction* action, DenemoScriptParam *param);

gboolean add_note_to_chord (DenemoAction* action, DenemoScriptParam *param);
gboolean delete_note_from_chord (DenemoAction* action, DenemoScriptParam *param);
void deletepreviousobject (DenemoAction* action, DenemoScriptParam *param);
void sharpen_key (DenemoAction* action, DenemoScriptParam *param);
void set_stem_up (DenemoAction* action, DenemoScriptParam *param);
void set_stem_down (DenemoAction* action, DenemoScriptParam *param);
void sharpen_note (DenemoAction* action, DenemoScriptParam *param);
void flatten_note (DenemoAction* action, DenemoScriptParam *param);
void pending_flatten (DenemoAction* action, DenemoScriptParam *param);
void pending_sharpen (DenemoAction* action, DenemoScriptParam *param);
void tie_notes_key (DenemoAction* action, DenemoScriptParam *param);
void add_dot_key (DenemoAction* action, DenemoScriptParam *param);
void remove_dot_key (DenemoAction* action, DenemoScriptParam *param);
void insert_clone_key (DenemoScriptParam *param);
void add_mordent (DenemoScriptParam *param);
void add_turn (DenemoScriptParam *param);
void set_grace (DenemoScriptParam *param);

void force_cautionary (DenemoAction* action, DenemoScriptParam *param);
void change_pitch (DenemoAction* action, DenemoScriptParam *param);
void add_coda (DenemoScriptParam *param);
void add_flageolet (DenemoScriptParam *param);
void add_open (DenemoScriptParam *param);
void add_prallmordent (DenemoScriptParam *param);
void add_prallprall (DenemoScriptParam *param);
void add_prall (DenemoScriptParam *param);
void add_reverseturn (DenemoScriptParam *param);
void add_segno (DenemoScriptParam *param);
void add_sforzato (DenemoScriptParam *param);
void add_stopped (DenemoScriptParam *param);
void add_thumb (DenemoScriptParam *param);
void add_trillelement (DenemoScriptParam *param);
void add_trill_element (DenemoScriptParam *param);
void add_upprall (DenemoScriptParam *param);
void add_arpeggio (DenemoScriptParam *param);
void default_mode (DenemoScriptParam *param);
void replace_mode (DenemoScriptParam *param);
void insert_mode (DenemoScriptParam *param);
void insert_opencloserepeat (DenemoScriptParam *param);
void insert_closerepeat (DenemoScriptParam *param);
void insert_openrepeat (DenemoScriptParam *param);
void insert_endbar (DenemoScriptParam *param);
void insert_doublebar (DenemoScriptParam *param);
void blank_mode (DenemoAction * action, DenemoScriptParam *param);



void append_measure_score (DenemoAction* action, DenemoScriptParam *param);

gchar *process_key_event (GdkEventKey * event, gchar * perform_command ());


void InsertRest(gint duration);
void InsertDur(gint duration);
void ChangeDur(gint duration);
void SetDur(gint duration);
//void Dur(gint duration);
void ChangeTo(gchar note);
void MoveTo(gchar note);
void Insert(gchar note);
void AddNote(gchar note);
void Add(gchar note);

void Dur0(DenemoAction *action, gpointer param);
void ChangeDur0(DenemoAction *action, gpointer param);
void InsertDur0(DenemoAction *action, gpointer param);
void InsertRest0(DenemoAction *action, gpointer param);
void SetDur0(DenemoAction *action, gpointer param);
void Dur1(DenemoAction *action, gpointer param);
void ChangeDur1(DenemoAction *action, gpointer param);
void InsertDur1(DenemoAction *action, gpointer param);
void InsertRest1(DenemoAction *action, gpointer param);
void SetDur1(DenemoAction *action, gpointer param);
void Dur2(DenemoAction *action, gpointer param);
void ChangeDur2(DenemoAction *action, gpointer param);
void InsertDur2(DenemoAction *action, gpointer param);
void InsertRest2(DenemoAction *action, gpointer param);
void SetDur2(DenemoAction *action, gpointer param);
void Dur3(DenemoAction *action, gpointer param);
void ChangeDur3(DenemoAction *action, gpointer param);
void InsertDur3(DenemoAction *action, gpointer param);
void InsertRest3(DenemoAction *action, gpointer param);
void SetDur3(DenemoAction *action, gpointer param);
void Dur4(DenemoAction *action, gpointer param);
void ChangeDur4(DenemoAction *action, gpointer param);
void InsertDur4(DenemoAction *action, gpointer param);
void InsertRest4(DenemoAction *action, gpointer param);
void SetDur4(DenemoAction *action, gpointer param);
void Dur5(DenemoAction *action, gpointer param);
void ChangeDur5(DenemoAction *action, gpointer param);
void InsertDur5(DenemoAction *action, gpointer param);
void InsertRest5(DenemoAction *action, gpointer param);
void SetDur5(DenemoAction *action, gpointer param);
void Dur6(DenemoAction *action, gpointer param);
void ChangeDur6(DenemoAction *action, gpointer param);
void InsertDur6(DenemoAction *action, gpointer param);
void InsertRest6(DenemoAction *action, gpointer param);
void SetDur6(DenemoAction *action, gpointer param);
void Dur7(DenemoAction *action, gpointer param);
void ChangeDur7(DenemoAction *action, gpointer param);
void InsertDur7(DenemoAction *action, gpointer param);
void InsertRest7(DenemoAction *action, gpointer param);
void SetDur7(DenemoAction *action, gpointer param);
void Dur8(DenemoAction *action, gpointer param);
void ChangeDur8(DenemoAction *action, gpointer param);
void InsertDur8(DenemoAction *action, gpointer param);
void InsertRest8(DenemoAction *action, gpointer param);
void SetDur8(DenemoAction *action, gpointer param);
void InsertA(DenemoAction *action, gpointer param);
void AddNoteA(DenemoAction *action, gpointer param);
void AddA(DenemoAction *action, gpointer param);
void ChangeToA(DenemoAction *action, gpointer param);
void MoveToA(DenemoAction *action, gpointer param);
void InsertB(DenemoAction *action, gpointer param);
void AddNoteB(DenemoAction *action, gpointer param);
void AddB(DenemoAction *action, gpointer param);
void ChangeToB(DenemoAction *action, gpointer param);
void MoveToB(DenemoAction *action, gpointer param);
void InsertC(DenemoAction *action, gpointer param);
void AddNoteC(DenemoAction *action, gpointer param);
void AddC(DenemoAction *action, gpointer param);
void ChangeToC(DenemoAction *action, gpointer param);
void MoveToC(DenemoAction *action, gpointer param);
void InsertD(DenemoAction *action, gpointer param);
void AddNoteD(DenemoAction *action, gpointer param);
void AddD(DenemoAction *action, gpointer param);
void ChangeToD(DenemoAction *action, gpointer param);
void MoveToD(DenemoAction *action, gpointer param);
void InsertE(DenemoAction *action, gpointer param);
void AddNoteE(DenemoAction *action, gpointer param);
void AddE(DenemoAction *action, gpointer param);
void ChangeToE(DenemoAction *action, gpointer param);
void MoveToE(DenemoAction *action, gpointer param);
void InsertF(DenemoAction *action, gpointer param);
void AddNoteF(DenemoAction *action, gpointer param);
void AddF(DenemoAction *action, gpointer param);
void ChangeToF(DenemoAction *action, gpointer param);
void MoveToF(DenemoAction *action, gpointer param);
void InsertG(DenemoAction *action, gpointer param);
void AddNoteG(DenemoAction *action, gpointer param);
void AddG(DenemoAction *action, gpointer param);
void ChangeToG(DenemoAction *action, gpointer param);
void MoveToG(DenemoAction *action, gpointer param);
#endif //KEYRESPONSES_H
