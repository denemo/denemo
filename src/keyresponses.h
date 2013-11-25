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
#include "commandfuncs.h"
#include <denemo/denemo.h>
#include "draw.h"
#include "selectops.h"
#include "chordops.h"
#include "objops.h"


gint scorearea_keypress_event (GtkWidget * widget, GdkEventKey * event);
gint scorearea_keyrelease_event (GtkWidget * widget, GdkEventKey * event);
gint scorearea_button_press (GtkWidget * widget, GdkEventButton * event);
gint scorearea_button_release (GtkWidget * widget, GdkEventButton * event);

gboolean intercept_scorearea_keypress (GdkEventKey * pevent);

void adjust_measure_less_width_key (GtkAction* action, DenemoScriptParam *param);
void adjust_measure_more_width_key (GtkAction* action, DenemoScriptParam *param);
void adjust_staff_less_height_key (GtkAction* action, DenemoScriptParam *param);
void adjust_staff_more_height_key (GtkAction* action, DenemoScriptParam *param);
void unset_selection_key (DenemoScriptParam *param);
void insert_chord_xkey (gint duration, DenemoScriptParam *param);
void insert_chord_key (DenemoScriptParam *param);
void go_to_key (gchar note, DenemoScriptParam *param);
void go_to_A_key (GtkAction* action, DenemoScriptParam *param);
void go_to_B_key (GtkAction* action, DenemoScriptParam *param);
void go_to_C_key (GtkAction* action, DenemoScriptParam *param);
void go_to_D_key (GtkAction* action, DenemoScriptParam *param);
void go_to_E_key (GtkAction* action, DenemoScriptParam *param);
void go_to_F_key (GtkAction* action, DenemoScriptParam *param);
void go_to_G_key (GtkAction* action, DenemoScriptParam *param);
void octave_up_key (GtkAction* action, DenemoScriptParam *param);
void octave_down_key (GtkAction* action, DenemoScriptParam *param);
void rest_toggle_key (GtkAction* action, DenemoScriptParam *param);
void toggle_blank (GtkAction* action, DenemoScriptParam *param);
void append_measure_key (GtkAction* action, DenemoScriptParam *param);
void insert_measure_key (GtkAction* action, DenemoScriptParam *param);


void insert_chord_0key (GtkAction* action, DenemoScriptParam *param);
void insert_chord_1key (GtkAction* action, DenemoScriptParam *param);
void insert_chord_2key (GtkAction* action, DenemoScriptParam *param);
void insert_chord_3key (GtkAction* action, DenemoScriptParam *param);
void insert_chord_4key (GtkAction* action, DenemoScriptParam *param);
void insert_chord_5key (GtkAction* action, DenemoScriptParam *param);
void insert_chord_6key (GtkAction* action, DenemoScriptParam *param);
void insert_chord_7key (GtkAction* action, DenemoScriptParam *param);
void insert_chord_8key (GtkAction* action, DenemoScriptParam *param);
void insert_blankchord_xkey (gint duration, DenemoScriptParam *param);
void insert_blankchord_0key (GtkAction* action, DenemoScriptParam *param);
void insert_blankchord_1key (GtkAction* action, DenemoScriptParam *param);
void insert_blankchord_2key (GtkAction* action, DenemoScriptParam *param);
void insert_blankchord_3key (GtkAction* action, DenemoScriptParam *param);
void insert_blankchord_4key (GtkAction* action, DenemoScriptParam *param);
void insert_blankchord_5key (GtkAction* action, DenemoScriptParam *param);
void insert_blankchord_6key (GtkAction* action, DenemoScriptParam *param);
void insert_blankchord_7key (GtkAction* action, DenemoScriptParam *param);
void insert_blankchord_8key (GtkAction* action, DenemoScriptParam *param);
void insert_rest_xkey(gint duration, DenemoScriptParam* param);
void insert_rest_0key (GtkAction* action, DenemoScriptParam *param);
void insert_rest_1key (GtkAction* action, DenemoScriptParam *param);
void insert_rest_2key (GtkAction* action, DenemoScriptParam *param);
void insert_rest_3key (GtkAction* action, DenemoScriptParam *param);
void insert_rest_4key (GtkAction* action, DenemoScriptParam *param);
void insert_rest_5key (GtkAction* action, DenemoScriptParam *param);
void insert_rest_6key (GtkAction* action, DenemoScriptParam *param);
void insert_rest_7key (GtkAction* action, DenemoScriptParam *param);
void insert_rest_8key (GtkAction* action, DenemoScriptParam *param);

void insert_duplet (GtkAction* action, DenemoScriptParam *param);
void insert_triplet (GtkAction* action, DenemoScriptParam *param);
void start_triplet (GtkAction* action, DenemoScriptParam *param);
void end_tuplet (GtkAction* action, DenemoScriptParam *param);



void insert_quadtuplet (GtkAction* action, DenemoScriptParam *param);
void insert_quintuplet (GtkAction* action, DenemoScriptParam *param);
void insert_sextuplet (GtkAction* action, DenemoScriptParam *param);
void insert_septuplet (GtkAction* action, DenemoScriptParam *param);
gboolean add_tone_key (GtkAction* action, DenemoScriptParam *param);
gboolean remove_tone_key (GtkAction* action, DenemoScriptParam *param);
void deletepreviousobject (GtkAction* action, DenemoScriptParam *param);
void sharpen_key (GtkAction* action, DenemoScriptParam *param);
void stem_up (GtkAction* action, DenemoScriptParam *param);
void stem_down (GtkAction* action, DenemoScriptParam *param);
void flatten_key (GtkAction* action, DenemoScriptParam *param);
void pending_flatten (GtkAction* action, DenemoScriptParam *param);
void pending_sharpen (GtkAction* action, DenemoScriptParam *param);
void tie_notes_key (GtkAction* action, DenemoScriptParam *param);
void add_dot_key (GtkAction* action, DenemoScriptParam *param);
void remove_dot_key (GtkAction* action, DenemoScriptParam *param);
void insert_clone_key (DenemoScriptParam *param);
void add_mordent (DenemoScriptParam *param);
void add_turn (DenemoScriptParam *param);
void set_grace (DenemoScriptParam *param);

void force_cautionary (GtkAction* action, DenemoScriptParam *param);
void change_pitch (GtkAction* action, DenemoScriptParam *param);
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
void blank_mode (GtkAction * action, DenemoScriptParam *param);
void newcleftreble (GtkAction* action, DenemoScriptParam *param);
void newclefbass (GtkAction* action, DenemoScriptParam *param);
void newclefg8 (GtkAction* action, DenemoScriptParam *param);
void newclefalto (GtkAction* action, DenemoScriptParam *param);
void newcleftenor (GtkAction* action, DenemoScriptParam *param);
void newclefsoprano (GtkAction* action, DenemoScriptParam *param);

void setcleftreble (GtkAction* action, DenemoScriptParam *param);
void setclefbass (GtkAction* action, DenemoScriptParam *param);
void setclefg8 (GtkAction* action, DenemoScriptParam *param);
void setclefalto (GtkAction* action, DenemoScriptParam *param);
void setcleftenor (GtkAction* action, DenemoScriptParam *param);
void setclefsoprano (GtkAction* action, DenemoScriptParam *param);

void newtimesig22 (GtkAction* action, DenemoScriptParam *param);
void newtimesig42 (GtkAction* action, DenemoScriptParam *param);
void newtimesig32 (GtkAction* action, DenemoScriptParam *param);
void newtimesig44 (GtkAction* action, DenemoScriptParam *param);
void newtimesig54 (GtkAction* action, DenemoScriptParam *param);
void newtimesig24 (GtkAction* action, DenemoScriptParam *param);
void newtimesig34 (GtkAction* action, DenemoScriptParam *param);
void newtimesig68 (GtkAction* action, DenemoScriptParam *param);
void newtimesig128 (GtkAction* action, DenemoScriptParam *param);
void newtimesig38 (GtkAction* action, DenemoScriptParam *param);
void newtimesig98 (GtkAction* action, DenemoScriptParam *param);
void newtimesig64 (GtkAction* action, DenemoScriptParam *param);


void settimesig22 (GtkAction* action, DenemoScriptParam *param);
void settimesig42 (GtkAction* action, DenemoScriptParam *param);
void settimesig32 (GtkAction* action, DenemoScriptParam *param);
void settimesig44 (GtkAction* action, DenemoScriptParam *param);
void settimesig54 (GtkAction* action, DenemoScriptParam *param);
void settimesig24 (GtkAction* action, DenemoScriptParam *param);
void settimesig34 (GtkAction* action, DenemoScriptParam *param);
void settimesig68 (GtkAction* action, DenemoScriptParam *param);
void settimesig128 (GtkAction* action, DenemoScriptParam *param);
void settimesig38 (GtkAction* action, DenemoScriptParam *param);
void settimesig98 (GtkAction* action, DenemoScriptParam *param);
void settimesig64 (GtkAction* action, DenemoScriptParam *param);

void newkeysigcmaj (GtkAction* action, DenemoScriptParam *param);
void newkeysiggmaj (GtkAction* action, DenemoScriptParam *param);
void newkeysigdmaj (GtkAction* action, DenemoScriptParam *param);
void newkeysigamaj (GtkAction* action, DenemoScriptParam *param);
void newkeysigemaj (GtkAction* action, DenemoScriptParam *param);
void newkeysigbmaj (GtkAction* action, DenemoScriptParam *param);
void newkeysigfsharpmaj (GtkAction* action, DenemoScriptParam *param);
void newkeysigcsharpmaj (GtkAction* action, DenemoScriptParam *param);
void newkeysigfmaj (GtkAction* action, DenemoScriptParam *param);
void newkeysigbflatmaj (GtkAction* action, DenemoScriptParam *param);
void newkeysigeflatmaj (GtkAction* action, DenemoScriptParam *param);
void newkeysigaflatmaj (GtkAction* action, DenemoScriptParam *param);
void newkeysigdflatmaj (GtkAction* action, DenemoScriptParam *param);
void newkeysiggflatmaj (GtkAction* action, DenemoScriptParam *param);
void newkeysigcflatmaj (GtkAction* action, DenemoScriptParam *param);

void newkeysigamin (GtkAction* action, DenemoScriptParam *param);
void newkeysigemin (GtkAction* action, DenemoScriptParam *param);
void newkeysigbmin (GtkAction* action, DenemoScriptParam *param);
void newkeysigfsharpmin (GtkAction* action, DenemoScriptParam *param);
void newkeysigcsharpmin (GtkAction* action, DenemoScriptParam *param);
void newkeysiggsharpmin (GtkAction* action, DenemoScriptParam *param);
void newkeysigdsharpmin (GtkAction* action, DenemoScriptParam *param);
void newkeysigasharpmin (GtkAction* action, DenemoScriptParam *param);
void newkeysigdmin (GtkAction* action, DenemoScriptParam *param);
void newkeysiggmin (GtkAction* action, DenemoScriptParam *param);
void newkeysigcmin (GtkAction* action, DenemoScriptParam *param);
void newkeysigfmin (GtkAction* action, DenemoScriptParam *param);
void newkeysigbflatmin (GtkAction* action, DenemoScriptParam *param);
void newkeysigeflatmin (GtkAction* action, DenemoScriptParam *param);
void newkeysigaflatmin (GtkAction* action, DenemoScriptParam *param);


void setkeysigcmaj (GtkAction* action, DenemoScriptParam *param);
void setkeysiggmaj (GtkAction* action, DenemoScriptParam *param);
void setkeysigdmaj (GtkAction* action, DenemoScriptParam *param);
void setkeysigamaj (GtkAction* action, DenemoScriptParam *param);
void setkeysigemaj (GtkAction* action, DenemoScriptParam *param);
void setkeysigbmaj (GtkAction* action, DenemoScriptParam *param);
void setkeysigfsharpmaj (GtkAction* action, DenemoScriptParam *param);
void setkeysigcsharpmaj (GtkAction* action, DenemoScriptParam *param);
void setkeysigfmaj (GtkAction* action, DenemoScriptParam *param);
void setkeysigbflatmaj (GtkAction* action, DenemoScriptParam *param);
void setkeysigeflatmaj (GtkAction* action, DenemoScriptParam *param);
void setkeysigaflatmaj (GtkAction* action, DenemoScriptParam *param);
void setkeysigdflatmaj (GtkAction* action, DenemoScriptParam *param);
void setkeysiggflatmaj (GtkAction* action, DenemoScriptParam *param);
void setkeysigcflatmaj (GtkAction* action, DenemoScriptParam *param);

void setkeysigamin (GtkAction* action, DenemoScriptParam *param);
void setkeysigemin (GtkAction* action, DenemoScriptParam *param);
void setkeysigbmin (GtkAction* action, DenemoScriptParam *param);
void setkeysigfsharpmin (GtkAction* action, DenemoScriptParam *param);
void setkeysigcsharpmin (GtkAction* action, DenemoScriptParam *param);
void setkeysiggsharpmin (GtkAction* action, DenemoScriptParam *param);
void setkeysigdsharpmin (GtkAction* action, DenemoScriptParam *param);
void setkeysigasharpmin (GtkAction* action, DenemoScriptParam *param);
void setkeysigdmin (GtkAction* action, DenemoScriptParam *param);
void setkeysiggmin (GtkAction* action, DenemoScriptParam *param);
void setkeysigcmin (GtkAction* action, DenemoScriptParam *param);
void setkeysigfmin (GtkAction* action, DenemoScriptParam *param);
void setkeysigbflatmin (GtkAction* action, DenemoScriptParam *param);
void setkeysigeflatmin (GtkAction* action, DenemoScriptParam *param);
void setkeysigaflatmin (GtkAction* action, DenemoScriptParam *param);


void append_measure_score (GtkAction* action, DenemoScriptParam *param);

gchar *process_key_event (GdkEventKey * event, gchar * perform_command ());


void InsertRest(gint duration);
void InsertDur(gint duration);
void ChangeDur(gint duration);
void SetDur(gint duration);
void Dur(gint duration);
void ChangeTo(gchar note);
void MoveTo(gchar note);
void Insert(gchar note);
void AddNote(gchar note);
void Add(gchar note);

void Dur0(GtkAction *action, gpointer param);
void ChangeDur0(GtkAction *action, gpointer param);
void InsertDur0(GtkAction *action, gpointer param);
void InsertRest0(GtkAction *action, gpointer param);
void SetDur0(GtkAction *action, gpointer param);
void Dur1(GtkAction *action, gpointer param);
void ChangeDur1(GtkAction *action, gpointer param);
void InsertDur1(GtkAction *action, gpointer param);
void InsertRest1(GtkAction *action, gpointer param);
void SetDur1(GtkAction *action, gpointer param);
void Dur2(GtkAction *action, gpointer param);
void ChangeDur2(GtkAction *action, gpointer param);
void InsertDur2(GtkAction *action, gpointer param);
void InsertRest2(GtkAction *action, gpointer param);
void SetDur2(GtkAction *action, gpointer param);
void Dur3(GtkAction *action, gpointer param);
void ChangeDur3(GtkAction *action, gpointer param);
void InsertDur3(GtkAction *action, gpointer param);
void InsertRest3(GtkAction *action, gpointer param);
void SetDur3(GtkAction *action, gpointer param);
void Dur4(GtkAction *action, gpointer param);
void ChangeDur4(GtkAction *action, gpointer param);
void InsertDur4(GtkAction *action, gpointer param);
void InsertRest4(GtkAction *action, gpointer param);
void SetDur4(GtkAction *action, gpointer param);
void Dur5(GtkAction *action, gpointer param);
void ChangeDur5(GtkAction *action, gpointer param);
void InsertDur5(GtkAction *action, gpointer param);
void InsertRest5(GtkAction *action, gpointer param);
void SetDur5(GtkAction *action, gpointer param);
void Dur6(GtkAction *action, gpointer param);
void ChangeDur6(GtkAction *action, gpointer param);
void InsertDur6(GtkAction *action, gpointer param);
void InsertRest6(GtkAction *action, gpointer param);
void SetDur6(GtkAction *action, gpointer param);
void Dur7(GtkAction *action, gpointer param);
void ChangeDur7(GtkAction *action, gpointer param);
void InsertDur7(GtkAction *action, gpointer param);
void InsertRest7(GtkAction *action, gpointer param);
void SetDur7(GtkAction *action, gpointer param);
void Dur8(GtkAction *action, gpointer param);
void ChangeDur8(GtkAction *action, gpointer param);
void InsertDur8(GtkAction *action, gpointer param);
void InsertRest8(GtkAction *action, gpointer param);
void SetDur8(GtkAction *action, gpointer param);
void InsertA(GtkAction *action, gpointer param);
void AddNoteA(GtkAction *action, gpointer param);
void AddA(GtkAction *action, gpointer param);
void ChangeToA(GtkAction *action, gpointer param);
void MoveToA(GtkAction *action, gpointer param);
void InsertB(GtkAction *action, gpointer param);
void AddNoteB(GtkAction *action, gpointer param);
void AddB(GtkAction *action, gpointer param);
void ChangeToB(GtkAction *action, gpointer param);
void MoveToB(GtkAction *action, gpointer param);
void InsertC(GtkAction *action, gpointer param);
void AddNoteC(GtkAction *action, gpointer param);
void AddC(GtkAction *action, gpointer param);
void ChangeToC(GtkAction *action, gpointer param);
void MoveToC(GtkAction *action, gpointer param);
void InsertD(GtkAction *action, gpointer param);
void AddNoteD(GtkAction *action, gpointer param);
void AddD(GtkAction *action, gpointer param);
void ChangeToD(GtkAction *action, gpointer param);
void MoveToD(GtkAction *action, gpointer param);
void InsertE(GtkAction *action, gpointer param);
void AddNoteE(GtkAction *action, gpointer param);
void AddE(GtkAction *action, gpointer param);
void ChangeToE(GtkAction *action, gpointer param);
void MoveToE(GtkAction *action, gpointer param);
void InsertF(GtkAction *action, gpointer param);
void AddNoteF(GtkAction *action, gpointer param);
void AddF(GtkAction *action, gpointer param);
void ChangeToF(GtkAction *action, gpointer param);
void MoveToF(GtkAction *action, gpointer param);
void InsertG(GtkAction *action, gpointer param);
void AddNoteG(GtkAction *action, gpointer param);
void AddG(GtkAction *action, gpointer param);
void ChangeToG(GtkAction *action, gpointer param);
void MoveToG(GtkAction *action, gpointer param);
#endif //KEYRESPONSES_H
