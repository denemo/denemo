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

void adjust_measure_less_width_key (DenemoGUI * gui);
void adjust_measure_more_width_key (DenemoGUI * gui);
void adjust_staff_less_height_key (DenemoGUI * gui);
void adjust_staff_more_height_key (DenemoGUI * gui);
void unset_selection_key (DenemoGUI * gui);
void insert_chord_xkey (gint duration, DenemoGUI * gui);
void insert_chord_key (DenemoGUI * gui);
void go_to_key (gchar note, DenemoGUI * gui);
void go_to_A_key (DenemoGUI * gui);
void go_to_B_key (DenemoGUI * gui);
void go_to_C_key (DenemoGUI * gui);
void go_to_D_key (DenemoGUI * gui);
void go_to_E_key (DenemoGUI * gui);
void go_to_F_key (DenemoGUI * gui);
void go_to_G_key (DenemoGUI * gui);
void octave_up_key (DenemoGUI * gui);
void octave_down_key (DenemoGUI * gui);
void rest_toggle_key (DenemoGUI * gui);
void toggle_blank (DenemoGUI * gui);
void append_measure_key (DenemoGUI * gui);
void insert_measure_key (DenemoGUI * gui);


void insert_chord_0key (DenemoGUI * gui);
void insert_chord_1key (DenemoGUI * gui);
void insert_chord_2key (DenemoGUI * gui);
void insert_chord_3key (DenemoGUI * gui);
void insert_chord_4key (DenemoGUI * gui);
void insert_chord_5key (DenemoGUI * gui);
void insert_chord_6key (DenemoGUI * gui);
void insert_chord_7key (DenemoGUI * gui);
void insert_chord_8key (DenemoGUI * gui);
void insert_blankchord_0key (DenemoGUI * gui);
void insert_blankchord_1key (DenemoGUI * gui);
void insert_blankchord_2key (DenemoGUI * gui);
void insert_blankchord_3key (DenemoGUI * gui);
void insert_blankchord_4key (DenemoGUI * gui);
void insert_blankchord_5key (DenemoGUI * gui);
void insert_blankchord_6key (DenemoGUI * gui);
void insert_blankchord_7key (DenemoGUI * gui);
void insert_blankchord_8key (DenemoGUI * gui);
void insert_rest_0key (DenemoGUI * gui);
void insert_rest_1key (DenemoGUI * gui);
void insert_rest_2key (DenemoGUI * gui);
void insert_rest_3key (DenemoGUI * gui);
void insert_rest_4key (DenemoGUI * gui);
void insert_rest_5key (DenemoGUI * gui);
void insert_rest_6key (DenemoGUI * gui);
void insert_rest_7key (DenemoGUI * gui);
void insert_rest_8key (DenemoGUI * gui);

void insert_duplet (DenemoGUI * gui);
void insert_triplet (DenemoGUI * gui);
void start_triplet (DenemoGUI * gui);
void end_tuplet (DenemoGUI * gui);



void insert_quadtuplet (DenemoGUI * gui);
void insert_quintuplet (DenemoGUI * gui);
void insert_sextuplet (DenemoGUI * gui);
void insert_septuplet (DenemoGUI * gui);
gboolean add_tone_key (DenemoGUI * gui);
gboolean remove_tone_key (DenemoGUI * gui);
void deletepreviousobject (DenemoGUI * gui);
void sharpen_key (DenemoGUI * gui);
void stem_up (DenemoGUI * gui);
void stem_down (DenemoGUI * gui);
void flatten_key (DenemoGUI * gui);
void pending_flatten (DenemoGUI * gui);
void pending_sharpen (DenemoGUI * gui);
void tie_notes_key (DenemoGUI * gui);
void add_dot_key (DenemoGUI * gui);
void remove_dot_key (DenemoGUI * gui);
void insert_clone_key (DenemoGUI * gui);
void add_mordent (DenemoGUI * gui);
void add_turn (DenemoGUI * gui);
void set_grace (DenemoGUI * gui);

void force_cautionary (DenemoGUI * gui);
void change_pitch (DenemoGUI * gui);
void add_coda (DenemoGUI * gui);
void add_flageolet (DenemoGUI * gui);
void add_open (DenemoGUI * gui);
void add_prallmordent (DenemoGUI * gui);
void add_prallprall (DenemoGUI * gui);
void add_prall (DenemoGUI * gui);
void add_reverseturn (DenemoGUI * gui);
void add_segno (DenemoGUI * gui);
void add_sforzato (DenemoGUI * gui);
void add_stopped (DenemoGUI * gui);
void add_thumb (DenemoGUI * gui);
void add_trillelement (DenemoGUI * gui);
void add_trill_element (DenemoGUI * gui);
void add_upprall (DenemoGUI * gui);
void add_arpeggio (DenemoGUI * gui);
void default_mode (DenemoGUI * gui);
void replace_mode (DenemoGUI * gui);
void insert_mode (DenemoGUI * gui);
void insert_opencloserepeat (DenemoGUI * gui);
void insert_closerepeat (DenemoGUI * gui);
void insert_openrepeat (DenemoGUI * gui);
void insert_endbar (DenemoGUI * gui);
void insert_doublebar (DenemoGUI * gui);
void blank_mode (GtkAction * action, DenemoGUI * gui);
void newcleftreble (DenemoGUI * gui);
void newclefbass (DenemoGUI * gui);
void newclefg8 (DenemoGUI * gui);
void newclefalto (DenemoGUI * gui);
void newcleftenor (DenemoGUI * gui);
void newclefsoprano (DenemoGUI * gui);

void setcleftreble (DenemoGUI * gui);
void setclefbass (DenemoGUI * gui);
void setclefg8 (DenemoGUI * gui);
void setclefalto (DenemoGUI * gui);
void setcleftenor (DenemoGUI * gui);
void setclefsoprano (DenemoGUI * gui);

void newtimesig22 (DenemoGUI * gui);
void newtimesig42 (DenemoGUI * gui);
void newtimesig32 (DenemoGUI * gui);
void newtimesig44 (DenemoGUI * gui);
void newtimesig54 (DenemoGUI * gui);
void newtimesig24 (DenemoGUI * gui);
void newtimesig34 (DenemoGUI * gui);
void newtimesig68 (DenemoGUI * gui);
void newtimesig128 (DenemoGUI * gui);
void newtimesig38 (DenemoGUI * gui);
void newtimesig98 (DenemoGUI * gui);
void newtimesig64 (DenemoGUI * gui);


void settimesig22 (DenemoGUI * gui);
void settimesig42 (DenemoGUI * gui);
void settimesig32 (DenemoGUI * gui);
void settimesig44 (DenemoGUI * gui);
void settimesig54 (DenemoGUI * gui);
void settimesig24 (DenemoGUI * gui);
void settimesig34 (DenemoGUI * gui);
void settimesig68 (DenemoGUI * gui);
void settimesig128 (DenemoGUI * gui);
void settimesig38 (DenemoGUI * gui);
void settimesig98 (DenemoGUI * gui);
void settimesig64 (DenemoGUI * gui);

void newkeysigcmaj (DenemoGUI * gui);
void newkeysiggmaj (DenemoGUI * gui);
void newkeysigdmaj (DenemoGUI * gui);
void newkeysigamaj (DenemoGUI * gui);
void newkeysigemaj (DenemoGUI * gui);
void newkeysigbmaj (DenemoGUI * gui);
void newkeysigfsharpmaj (DenemoGUI * gui);
void newkeysigcsharpmaj (DenemoGUI * gui);
void newkeysigfmaj (DenemoGUI * gui);
void newkeysigbflatmaj (DenemoGUI * gui);
void newkeysigeflatmaj (DenemoGUI * gui);
void newkeysigaflatmaj (DenemoGUI * gui);
void newkeysigdflatmaj (DenemoGUI * gui);
void newkeysiggflatmaj (DenemoGUI * gui);
void newkeysigcflatmaj (DenemoGUI * gui);

void newkeysigamin (DenemoGUI * gui);
void newkeysigemin (DenemoGUI * gui);
void newkeysigbmin (DenemoGUI * gui);
void newkeysigfsharpmin (DenemoGUI * gui);
void newkeysigcsharpmin (DenemoGUI * gui);
void newkeysiggsharpmin (DenemoGUI * gui);
void newkeysigdsharpmin (DenemoGUI * gui);
void newkeysigasharpmin (DenemoGUI * gui);
void newkeysigdmin (DenemoGUI * gui);
void newkeysiggmin (DenemoGUI * gui);
void newkeysigcmin (DenemoGUI * gui);
void newkeysigfmin (DenemoGUI * gui);
void newkeysigbflatmin (DenemoGUI * gui);
void newkeysigeflatmin (DenemoGUI * gui);
void newkeysigaflatmin (DenemoGUI * gui);


void setkeysigcmaj (DenemoGUI * gui);
void setkeysiggmaj (DenemoGUI * gui);
void setkeysigdmaj (DenemoGUI * gui);
void setkeysigamaj (DenemoGUI * gui);
void setkeysigemaj (DenemoGUI * gui);
void setkeysigbmaj (DenemoGUI * gui);
void setkeysigfsharpmaj (DenemoGUI * gui);
void setkeysigcsharpmaj (DenemoGUI * gui);
void setkeysigfmaj (DenemoGUI * gui);
void setkeysigbflatmaj (DenemoGUI * gui);
void setkeysigeflatmaj (DenemoGUI * gui);
void setkeysigaflatmaj (DenemoGUI * gui);
void setkeysigdflatmaj (DenemoGUI * gui);
void setkeysiggflatmaj (DenemoGUI * gui);
void setkeysigcflatmaj (DenemoGUI * gui);

void setkeysigamin (DenemoGUI * gui);
void setkeysigemin (DenemoGUI * gui);
void setkeysigbmin (DenemoGUI * gui);
void setkeysigfsharpmin (DenemoGUI * gui);
void setkeysigcsharpmin (DenemoGUI * gui);
void setkeysiggsharpmin (DenemoGUI * gui);
void setkeysigdsharpmin (DenemoGUI * gui);
void setkeysigasharpmin (DenemoGUI * gui);
void setkeysigdmin (DenemoGUI * gui);
void setkeysiggmin (DenemoGUI * gui);
void setkeysigcmin (DenemoGUI * gui);
void setkeysigfmin (DenemoGUI * gui);
void setkeysigbflatmin (DenemoGUI * gui);
void setkeysigeflatmin (DenemoGUI * gui);
void setkeysigaflatmin (DenemoGUI * gui);


void append_measure_score (DenemoGUI * gui);

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
