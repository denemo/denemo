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

void adjust_measure_less_width_key (DenemoScriptParam *param);
void adjust_measure_more_width_key (DenemoScriptParam *param);
void adjust_staff_less_height_key (DenemoScriptParam *param);
void adjust_staff_more_height_key (DenemoScriptParam *param);
void unset_selection_key (DenemoScriptParam *param);
void insert_chord_xkey (gint duration, DenemoScriptParam *param);
void insert_chord_key (DenemoScriptParam *param);
void go_to_key (gchar note, DenemoScriptParam *param);
void go_to_A_key (DenemoScriptParam *param);
void go_to_B_key (DenemoScriptParam *param);
void go_to_C_key (DenemoScriptParam *param);
void go_to_D_key (DenemoScriptParam *param);
void go_to_E_key (DenemoScriptParam *param);
void go_to_F_key (DenemoScriptParam *param);
void go_to_G_key (DenemoScriptParam *param);
void octave_up_key (DenemoScriptParam *param);
void octave_down_key (DenemoScriptParam *param);
void rest_toggle_key (DenemoScriptParam *param);
void toggle_blank (DenemoScriptParam *param);
void append_measure_key (DenemoScriptParam *param);
void insert_measure_key (DenemoScriptParam *param);


void insert_chord_0key (DenemoScriptParam *param);
void insert_chord_1key (DenemoScriptParam *param);
void insert_chord_2key (DenemoScriptParam *param);
void insert_chord_3key (DenemoScriptParam *param);
void insert_chord_4key (DenemoScriptParam *param);
void insert_chord_5key (DenemoScriptParam *param);
void insert_chord_6key (DenemoScriptParam *param);
void insert_chord_7key (DenemoScriptParam *param);
void insert_chord_8key (DenemoScriptParam *param);
void insert_blankchord_xkey (gint duration, DenemoScriptParam *param);
void insert_blankchord_0key (DenemoScriptParam *param);
void insert_blankchord_1key (DenemoScriptParam *param);
void insert_blankchord_2key (DenemoScriptParam *param);
void insert_blankchord_3key (DenemoScriptParam *param);
void insert_blankchord_4key (DenemoScriptParam *param);
void insert_blankchord_5key (DenemoScriptParam *param);
void insert_blankchord_6key (DenemoScriptParam *param);
void insert_blankchord_7key (DenemoScriptParam *param);
void insert_blankchord_8key (DenemoScriptParam *param);
void insert_rest_xkey(gint duration, DenemoScriptParam* param);
void insert_rest_0key (DenemoScriptParam *param);
void insert_rest_1key (DenemoScriptParam *param);
void insert_rest_2key (DenemoScriptParam *param);
void insert_rest_3key (DenemoScriptParam *param);
void insert_rest_4key (DenemoScriptParam *param);
void insert_rest_5key (DenemoScriptParam *param);
void insert_rest_6key (DenemoScriptParam *param);
void insert_rest_7key (DenemoScriptParam *param);
void insert_rest_8key (DenemoScriptParam *param);

void insert_duplet (DenemoScriptParam *param);
void insert_triplet (DenemoScriptParam *param);
void start_triplet (DenemoScriptParam *param);
void end_tuplet (DenemoScriptParam *param);



void insert_quadtuplet (DenemoScriptParam *param);
void insert_quintuplet (DenemoScriptParam *param);
void insert_sextuplet (DenemoScriptParam *param);
void insert_septuplet (DenemoScriptParam *param);
gboolean add_tone_key (DenemoScriptParam *param);
gboolean remove_tone_key (DenemoScriptParam *param);
void deletepreviousobject (DenemoScriptParam *param);
void sharpen_key (DenemoScriptParam *param);
void stem_up (DenemoScriptParam *param);
void stem_down (DenemoScriptParam *param);
void flatten_key (DenemoScriptParam *param);
void pending_flatten (DenemoScriptParam *param);
void pending_sharpen (DenemoScriptParam *param);
void tie_notes_key (DenemoScriptParam *param);
void add_dot_key (DenemoScriptParam *param);
void remove_dot_key (DenemoScriptParam *param);
void insert_clone_key (DenemoScriptParam *param);
void add_mordent (DenemoScriptParam *param);
void add_turn (DenemoScriptParam *param);
void set_grace (DenemoScriptParam *param);

void force_cautionary (DenemoScriptParam *param);
void change_pitch (DenemoScriptParam *param);
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
void newcleftreble (DenemoScriptParam *param);
void newclefbass (DenemoScriptParam *param);
void newclefg8 (DenemoScriptParam *param);
void newclefalto (DenemoScriptParam *param);
void newcleftenor (DenemoScriptParam *param);
void newclefsoprano (DenemoScriptParam *param);

void setcleftreble (DenemoScriptParam *param);
void setclefbass (DenemoScriptParam *param);
void setclefg8 (DenemoScriptParam *param);
void setclefalto (DenemoScriptParam *param);
void setcleftenor (DenemoScriptParam *param);
void setclefsoprano (DenemoScriptParam *param);

void newtimesig22 (DenemoScriptParam *param);
void newtimesig42 (DenemoScriptParam *param);
void newtimesig32 (DenemoScriptParam *param);
void newtimesig44 (DenemoScriptParam *param);
void newtimesig54 (DenemoScriptParam *param);
void newtimesig24 (DenemoScriptParam *param);
void newtimesig34 (DenemoScriptParam *param);
void newtimesig68 (DenemoScriptParam *param);
void newtimesig128 (DenemoScriptParam *param);
void newtimesig38 (DenemoScriptParam *param);
void newtimesig98 (DenemoScriptParam *param);
void newtimesig64 (DenemoScriptParam *param);


void settimesig22 (DenemoScriptParam *param);
void settimesig42 (DenemoScriptParam *param);
void settimesig32 (DenemoScriptParam *param);
void settimesig44 (DenemoScriptParam *param);
void settimesig54 (DenemoScriptParam *param);
void settimesig24 (DenemoScriptParam *param);
void settimesig34 (DenemoScriptParam *param);
void settimesig68 (DenemoScriptParam *param);
void settimesig128 (DenemoScriptParam *param);
void settimesig38 (DenemoScriptParam *param);
void settimesig98 (DenemoScriptParam *param);
void settimesig64 (DenemoScriptParam *param);

void newkeysigcmaj (DenemoScriptParam *param);
void newkeysiggmaj (DenemoScriptParam *param);
void newkeysigdmaj (DenemoScriptParam *param);
void newkeysigamaj (DenemoScriptParam *param);
void newkeysigemaj (DenemoScriptParam *param);
void newkeysigbmaj (DenemoScriptParam *param);
void newkeysigfsharpmaj (DenemoScriptParam *param);
void newkeysigcsharpmaj (DenemoScriptParam *param);
void newkeysigfmaj (DenemoScriptParam *param);
void newkeysigbflatmaj (DenemoScriptParam *param);
void newkeysigeflatmaj (DenemoScriptParam *param);
void newkeysigaflatmaj (DenemoScriptParam *param);
void newkeysigdflatmaj (DenemoScriptParam *param);
void newkeysiggflatmaj (DenemoScriptParam *param);
void newkeysigcflatmaj (DenemoScriptParam *param);

void newkeysigamin (DenemoScriptParam *param);
void newkeysigemin (DenemoScriptParam *param);
void newkeysigbmin (DenemoScriptParam *param);
void newkeysigfsharpmin (DenemoScriptParam *param);
void newkeysigcsharpmin (DenemoScriptParam *param);
void newkeysiggsharpmin (DenemoScriptParam *param);
void newkeysigdsharpmin (DenemoScriptParam *param);
void newkeysigasharpmin (DenemoScriptParam *param);
void newkeysigdmin (DenemoScriptParam *param);
void newkeysiggmin (DenemoScriptParam *param);
void newkeysigcmin (DenemoScriptParam *param);
void newkeysigfmin (DenemoScriptParam *param);
void newkeysigbflatmin (DenemoScriptParam *param);
void newkeysigeflatmin (DenemoScriptParam *param);
void newkeysigaflatmin (DenemoScriptParam *param);


void setkeysigcmaj (DenemoScriptParam *param);
void setkeysiggmaj (DenemoScriptParam *param);
void setkeysigdmaj (DenemoScriptParam *param);
void setkeysigamaj (DenemoScriptParam *param);
void setkeysigemaj (DenemoScriptParam *param);
void setkeysigbmaj (DenemoScriptParam *param);
void setkeysigfsharpmaj (DenemoScriptParam *param);
void setkeysigcsharpmaj (DenemoScriptParam *param);
void setkeysigfmaj (DenemoScriptParam *param);
void setkeysigbflatmaj (DenemoScriptParam *param);
void setkeysigeflatmaj (DenemoScriptParam *param);
void setkeysigaflatmaj (DenemoScriptParam *param);
void setkeysigdflatmaj (DenemoScriptParam *param);
void setkeysiggflatmaj (DenemoScriptParam *param);
void setkeysigcflatmaj (DenemoScriptParam *param);

void setkeysigamin (DenemoScriptParam *param);
void setkeysigemin (DenemoScriptParam *param);
void setkeysigbmin (DenemoScriptParam *param);
void setkeysigfsharpmin (DenemoScriptParam *param);
void setkeysigcsharpmin (DenemoScriptParam *param);
void setkeysiggsharpmin (DenemoScriptParam *param);
void setkeysigdsharpmin (DenemoScriptParam *param);
void setkeysigasharpmin (DenemoScriptParam *param);
void setkeysigdmin (DenemoScriptParam *param);
void setkeysiggmin (DenemoScriptParam *param);
void setkeysigcmin (DenemoScriptParam *param);
void setkeysigfmin (DenemoScriptParam *param);
void setkeysigbflatmin (DenemoScriptParam *param);
void setkeysigeflatmin (DenemoScriptParam *param);
void setkeysigaflatmin (DenemoScriptParam *param);


void append_measure_score (DenemoScriptParam *param);

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
