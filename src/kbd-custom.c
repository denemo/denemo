/* kbd-custom.cpp
 *  Low-level data structure routines and file I/O for customizing keyboard
 *  configuration.
 *  
 *  For Denemo, the GNU graphical music notation package
 *  (c) 2000-2005 
 *      Olivier Vermersch, Matthew Hiller, Adam Tee
 */

#include <stdio.h>
#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <glib.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <denemo/denemo.h>
#include "commandfuncs.h"
#include "kbd-custom.h"
#include "kbd-interface.h"
#include "keyresponses.h"
#include "prefops.h"
#include "selectops.h"
#include "utils.h"
#include "playback.h"
#include "keyboard.h"

static void
load_keymap_file_named (keymap * the_keymap, gchar *keymapfile, gchar *fallback);

struct name_action_and_function *denemo_commands;


gint denemo_commands_size;

gchar *kbd_categories[] = {
  KBD_CATEGORY_NAVIGATION,
  KBD_CATEGORY_EDIT,
  KBD_CATEGORY_NOTE_ENTRY,
  KBD_CATEGORY_REST_ENTRY,
  KBD_CATEGORY_MEASURE,
  KBD_CATEGORY_STAFF,
  KBD_CATEGORY_ARTICULATION,
  KBD_CATEGORY_PLAYBACK,
  KBD_CATEGORY_OTHER
};

gint kbd_categories_length = G_N_ELEMENTS (kbd_categories);

struct name_and_function unmenued_commands[] = {
  {KBD_CATEGORY_NAVIGATION, N_("CursorLeft"), (GtkFunction) cursorleft},
  {KBD_CATEGORY_NAVIGATION, N_("CursorDown"), (GtkFunction) cursordown},
  {KBD_CATEGORY_NAVIGATION, N_("CursorUp"), (GtkFunction) cursorup},
  {KBD_CATEGORY_NAVIGATION, N_("CursorRight"), (GtkFunction) cursorright},
  {KBD_CATEGORY_NAVIGATION, N_("StaffUp"), (GtkFunction) staffup},
  {KBD_CATEGORY_NAVIGATION, N_("StaffDown"), (GtkFunction) staffdown},
  {KBD_CATEGORY_NAVIGATION, N_("MeasureLeft"), (GtkFunction) measureleft},
  {KBD_CATEGORY_NAVIGATION, N_("MeasureRight"), (GtkFunction) measureright},
  {KBD_CATEGORY_NAVIGATION, N_("ToNearestA"), (GtkFunction) go_to_A_key},
  {KBD_CATEGORY_NAVIGATION, N_("ToNearestB"), (GtkFunction) go_to_B_key},
  {KBD_CATEGORY_NAVIGATION, N_("ToNearestC"), (GtkFunction) go_to_C_key},
  {KBD_CATEGORY_NAVIGATION, N_("ToNearestD"), (GtkFunction) go_to_D_key},
  {KBD_CATEGORY_NAVIGATION, N_("ToNearestE"), (GtkFunction) go_to_E_key},
  {KBD_CATEGORY_NAVIGATION, N_("ToNearestF"), (GtkFunction) go_to_F_key},
  {KBD_CATEGORY_NAVIGATION, N_("ToNearestG"), (GtkFunction) go_to_G_key},
  {KBD_CATEGORY_NAVIGATION, N_("OctaveUp"), (GtkFunction) octave_up_key},
  {KBD_CATEGORY_NAVIGATION, N_("OctaveDown"), (GtkFunction) octave_down_key},

  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertWholeNote"),
   (GtkFunction) insert_chord_0key},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertHalfNote"),
   (GtkFunction) insert_chord_1key},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertQuarterNote"),
   (GtkFunction) insert_chord_2key},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertEighthNote"),
   (GtkFunction) insert_chord_3key},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertSixteenthNote"),
   (GtkFunction) insert_chord_4key},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertThirtysecondNote"),
   (GtkFunction) insert_chord_5key},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertSixtyfourthNote"),
   (GtkFunction) insert_chord_6key},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertBlankWholeNote"),
   (GtkFunction) insert_blankchord_0key},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertBlankHalfNote"),
   (GtkFunction) insert_blankchord_1key},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertBlankQuarterNote"),
   (GtkFunction) insert_blankchord_2key},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertBlankEighthNote"),
   (GtkFunction) insert_blankchord_3key},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertBlankSixteenthNote"),
   (GtkFunction) insert_blankchord_4key},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertBlankThirtysecondNote"),
   (GtkFunction) insert_blankchord_5key},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertBlankSixtyfourthNote"),
   (GtkFunction) insert_blankchord_6key},
  {KBD_CATEGORY_EDIT, N_("ToggleRestMode"), (GtkFunction) rest_toggle_key},
  {KBD_CATEGORY_EDIT, N_("ToggleBlankMode"), (GtkFunction) toggle_blank},

  {KBD_CATEGORY_REST_ENTRY, N_("InsertWholeRest"),
   (GtkFunction) insert_rest_0key},
  {KBD_CATEGORY_REST_ENTRY, N_("InsertHalfRest"),
   (GtkFunction) insert_rest_1key},
  {KBD_CATEGORY_REST_ENTRY, N_("InsertQuarterRest"),
   (GtkFunction) insert_rest_2key},
  {KBD_CATEGORY_REST_ENTRY, N_("InsertEighthRest"),
   (GtkFunction) insert_rest_3key},
  {KBD_CATEGORY_REST_ENTRY, N_("InsertSixteenthRest"),
   (GtkFunction) insert_rest_4key},
  {KBD_CATEGORY_REST_ENTRY, N_("InsertThirtysecondRest"),
   (GtkFunction) insert_rest_5key},
  {KBD_CATEGORY_REST_ENTRY, N_("InsertSixtyfourthRest"),
   (GtkFunction) insert_rest_6key},


  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertDuplet"), (GtkFunction) insert_duplet},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertTriplet"),
   (GtkFunction) insert_triplet},
  {KBD_CATEGORY_NOTE_ENTRY, N_("StartTriplet"),
   (GtkFunction) start_triplet},
  {KBD_CATEGORY_NOTE_ENTRY, N_("EndTuplet"),
   (GtkFunction) end_tuplet},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertQuadtuplet"),
   (GtkFunction) insert_quadtuplet},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertQuintuplet"),
   (GtkFunction) insert_quintuplet},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertSextuplet"),
   (GtkFunction) insert_sextuplet},
  {KBD_CATEGORY_NOTE_ENTRY, N_("InsertSeptuplet"),
   (GtkFunction) insert_septuplet},
  {KBD_CATEGORY_NOTE_ENTRY, N_("AddTone"), (GtkFunction) add_tone_key},
  {KBD_CATEGORY_NOTE_ENTRY, N_("RemoveTone"), (GtkFunction) remove_tone_key},

  {KBD_CATEGORY_NOTE_ENTRY, N_("Sharpen/StemDown"),
   (GtkFunction) sharpen_key},
  {KBD_CATEGORY_NOTE_ENTRY, N_("Flatten/StemUp"), (GtkFunction) flatten_key},

  {KBD_CATEGORY_NOTE_ENTRY, N_("AddDot"), (GtkFunction) add_dot_key},
  {KBD_CATEGORY_NOTE_ENTRY, N_("RemoveDot"), (GtkFunction) remove_dot_key},

  {KBD_CATEGORY_ARTICULATION, N_("ToggleTie"), (GtkFunction) tie_notes_key},

  {KBD_CATEGORY_EDIT, N_("DeleteObject"), (GtkFunction) deleteobject},
  {KBD_CATEGORY_EDIT, N_("DeletePreviousObject"),
   (GtkFunction) deletepreviousobject},

  {KBD_CATEGORY_MEASURE, N_("InsertMeasure"),
   (GtkFunction) insert_measure_key},
  {KBD_CATEGORY_MEASURE, N_("AppendMeasure"),
   (GtkFunction) append_measure_key},
  {KBD_CATEGORY_MEASURE, N_("DeleteMeasure"), (GtkFunction) deletemeasure},
  {KBD_CATEGORY_MEASURE, N_("ShrinkMeasures"),
   (GtkFunction) adjust_measure_less_width_key},
  {KBD_CATEGORY_MEASURE, N_("WidenMeasures"),
   (GtkFunction) adjust_measure_more_width_key},

  {KBD_CATEGORY_STAFF, N_("ShorterStaffs"),
   (GtkFunction) adjust_staff_less_height_key},
  {KBD_CATEGORY_STAFF, N_("TallerStaffs"),
   (GtkFunction) adjust_staff_more_height_key},

  {KBD_CATEGORY_STAFF, N_("InsertTrebleClef"), (GtkFunction) newcleftreble},
  {KBD_CATEGORY_STAFF, N_("InsertBassClef"), (GtkFunction) newclefbass},
  {KBD_CATEGORY_STAFF, N_("Insertg8clef"), (GtkFunction) newclefg8},
  {KBD_CATEGORY_STAFF, N_("InsertAltoClef"), (GtkFunction) newclefalto},
  {KBD_CATEGORY_STAFF, N_("InsertTenorClef"), (GtkFunction) newcleftenor},
  {KBD_CATEGORY_STAFF, N_("InsertSopranoClef"), (GtkFunction) newclefsoprano},

  {KBD_CATEGORY_STAFF, N_("SetInitialTrebleClef"),
   (GtkFunction) setcleftreble},
  {KBD_CATEGORY_STAFF, N_("SetInitialBassClef"), (GtkFunction) setclefbass},
  {KBD_CATEGORY_STAFF, N_("SetInitialg8clef"), (GtkFunction) setclefg8},
  {KBD_CATEGORY_STAFF, N_("SetInitialAltoClef"), (GtkFunction) setclefalto},
  {KBD_CATEGORY_STAFF, N_("SetInitialTenorClef"), (GtkFunction) setcleftenor},
  {KBD_CATEGORY_STAFF, N_("SetInitialSopranoClef"),
   (GtkFunction) setclefsoprano},

  {KBD_CATEGORY_STAFF, N_("Insert22Time"), (GtkFunction) newtimesig22},
  {KBD_CATEGORY_STAFF, N_("Insert32Time"), (GtkFunction) newtimesig32},
  {KBD_CATEGORY_STAFF, N_("Insert42Time"), (GtkFunction) newtimesig42},
  {KBD_CATEGORY_STAFF, N_("Insert44Time"), (GtkFunction) newtimesig44},
  {KBD_CATEGORY_STAFF, N_("Insert34Time"), (GtkFunction) newtimesig34},
  {KBD_CATEGORY_STAFF, N_("Insert24Time"), (GtkFunction) newtimesig24},
  {KBD_CATEGORY_STAFF, N_("Insert64Time"), (GtkFunction) newtimesig64},
  {KBD_CATEGORY_STAFF, N_("Insert38Time"), (GtkFunction) newtimesig38},
  {KBD_CATEGORY_STAFF, N_("Insert68Time"), (GtkFunction) newtimesig68},
  {KBD_CATEGORY_STAFF, N_("Insert128Time"), (GtkFunction) newtimesig128},
  {KBD_CATEGORY_STAFF, N_("Insert98Time"), (GtkFunction) newtimesig98},
  {KBD_CATEGORY_STAFF, N_("Set22Time"), (GtkFunction) settimesig22},
  {KBD_CATEGORY_STAFF, N_("Set32Time"), (GtkFunction) settimesig32},
  {KBD_CATEGORY_STAFF, N_("Set42Time"), (GtkFunction) settimesig42},
  {KBD_CATEGORY_STAFF, N_("Set44Time"), (GtkFunction) settimesig44},
  {KBD_CATEGORY_STAFF, N_("Set34Time"), (GtkFunction) settimesig34},
  {KBD_CATEGORY_STAFF, N_("Set24Time"), (GtkFunction) settimesig24},
  {KBD_CATEGORY_STAFF, N_("Set64Time"), (GtkFunction) settimesig64},
  {KBD_CATEGORY_STAFF, N_("Set38Time"), (GtkFunction) settimesig38},
  {KBD_CATEGORY_STAFF, N_("Set68Time"), (GtkFunction) settimesig68},
  {KBD_CATEGORY_STAFF, N_("Set128Time"), (GtkFunction) settimesig128},
  {KBD_CATEGORY_STAFF, N_("Set98Time"), (GtkFunction) settimesig98},
  {KBD_CATEGORY_STAFF, N_("InsertCmaj"), (GtkFunction) newkeysigcmaj},
  {KBD_CATEGORY_STAFF, N_("InsertGmaj"), (GtkFunction) newkeysiggmaj},
  {KBD_CATEGORY_STAFF, N_("InsertDmaj"), (GtkFunction) newkeysigdmaj},
  {KBD_CATEGORY_STAFF, N_("InsertAmaj"), (GtkFunction) newkeysigamaj},
  {KBD_CATEGORY_STAFF, N_("InsertEmaj"), (GtkFunction) newkeysigemaj},
  {KBD_CATEGORY_STAFF, N_("InsertBmaj"), (GtkFunction) newkeysigbmaj},
  {KBD_CATEGORY_STAFF, N_("InsertFSharpmaj"),
   (GtkFunction) newkeysigfsharpmaj},
  {KBD_CATEGORY_STAFF, N_("InsertCSharpmaj"),
   (GtkFunction) newkeysigcsharpmaj},
  {KBD_CATEGORY_STAFF, N_("InsertFmaj"), (GtkFunction) newkeysigfmaj},
  {KBD_CATEGORY_STAFF, N_("InsertBflatmaj"), (GtkFunction) newkeysigbflatmaj},
  {KBD_CATEGORY_STAFF, N_("InsertEflatmaj"), (GtkFunction) newkeysigeflatmaj},
  {KBD_CATEGORY_STAFF, N_("InsertAflatmaj"), (GtkFunction) newkeysigaflatmaj},
  {KBD_CATEGORY_STAFF, N_("InsertDflatmaj"), (GtkFunction) newkeysigdflatmaj},
  {KBD_CATEGORY_STAFF, N_("InsertGflatmaj"), (GtkFunction) newkeysiggflatmaj},
  {KBD_CATEGORY_STAFF, N_("InsertCflatmaj"), (GtkFunction) newkeysigcflatmaj},
  {KBD_CATEGORY_STAFF, N_("InsertAmin"), (GtkFunction) newkeysigamin},
  {KBD_CATEGORY_STAFF, N_("InsertEmin"), (GtkFunction) newkeysigemin},
  {KBD_CATEGORY_STAFF, N_("InsertBmin"), (GtkFunction) newkeysigbmin},
  {KBD_CATEGORY_STAFF, N_("InsertFSharpmin"),
   (GtkFunction) newkeysigfsharpmin},
  {KBD_CATEGORY_STAFF, N_("InsertCSharpmin"),
   (GtkFunction) newkeysigcsharpmin},
  {KBD_CATEGORY_STAFF, N_("InsertGSharpmin"),
   (GtkFunction) newkeysiggsharpmin},
  {KBD_CATEGORY_STAFF, N_("InsertDSharpmin"),
   (GtkFunction) newkeysigdsharpmin},
  {KBD_CATEGORY_STAFF, N_("InsertASharpmin"),
   (GtkFunction) newkeysigasharpmin},
  {KBD_CATEGORY_STAFF, N_("InsertDmin"), (GtkFunction) newkeysigdmin},
  {KBD_CATEGORY_STAFF, N_("InsertGmin"), (GtkFunction) newkeysiggmin},
  {KBD_CATEGORY_STAFF, N_("InsertCmin"), (GtkFunction) newkeysigcmin},
  {KBD_CATEGORY_STAFF, N_("InsertFmin"), (GtkFunction) newkeysigfmin},
  {KBD_CATEGORY_STAFF, N_("InsertBflatmin"), (GtkFunction) newkeysigbflatmin},
  {KBD_CATEGORY_STAFF, N_("InsertEflatmin"), (GtkFunction) newkeysigeflatmin},
  {KBD_CATEGORY_STAFF, N_("InsertAflatmin"), (GtkFunction) newkeysigaflatmin},

  //Functions to Set Initial Key Sig
  {KBD_CATEGORY_STAFF, N_("SetInitialCmaj"), (GtkFunction) setkeysigcmaj},
  {KBD_CATEGORY_STAFF, N_("SetInitialGmaj"), (GtkFunction) setkeysiggmaj},
  {KBD_CATEGORY_STAFF, N_("SetInitialDmaj"), (GtkFunction) setkeysigdmaj},
  {KBD_CATEGORY_STAFF, N_("SetInitialAmaj"), (GtkFunction) setkeysigamaj},
  {KBD_CATEGORY_STAFF, N_("SetInitialEmaj"), (GtkFunction) setkeysigemaj},
  {KBD_CATEGORY_STAFF, N_("SetInitialBmaj"), (GtkFunction) setkeysigbmaj},
  {KBD_CATEGORY_STAFF, N_("SetInitialFSharpmaj"),
   (GtkFunction) setkeysigfsharpmaj},
  {KBD_CATEGORY_STAFF, N_("SetInitialCSharpmaj"),
   (GtkFunction) setkeysigcsharpmaj},
  {KBD_CATEGORY_STAFF, N_("SetInitialFmaj"), (GtkFunction) setkeysigfmaj},
  {KBD_CATEGORY_STAFF, N_("SetInitialBflatmaj"),
   (GtkFunction) setkeysigbflatmaj},
  {KBD_CATEGORY_STAFF, N_("SetInitialEflatmaj"),
   (GtkFunction) setkeysigeflatmaj},
  {KBD_CATEGORY_STAFF, N_("SetInitialAflatmaj"),
   (GtkFunction) setkeysigaflatmaj},
  {KBD_CATEGORY_STAFF, N_("SetInitialDflatmaj"),
   (GtkFunction) setkeysigdflatmaj},
  {KBD_CATEGORY_STAFF, N_("SetInitialGflatmaj"),
   (GtkFunction) setkeysiggflatmaj},
  {KBD_CATEGORY_STAFF, N_("SetInitialCflatmaj"),
   (GtkFunction) setkeysigcflatmaj},
  {KBD_CATEGORY_STAFF, N_("SetInitialAmin"), (GtkFunction) setkeysigamin},
  {KBD_CATEGORY_STAFF, N_("SetInitialEmin"), (GtkFunction) setkeysigemin},
  {KBD_CATEGORY_STAFF, N_("SetInitialBmin"), (GtkFunction) setkeysigbmin},
  {KBD_CATEGORY_STAFF, N_("SetInitialFSharpmin"),
   (GtkFunction) setkeysigfsharpmin},
  {KBD_CATEGORY_STAFF, N_("SetInitialCSharpmin"),
   (GtkFunction) setkeysigcsharpmin},
  {KBD_CATEGORY_STAFF, N_("SetInitialGSharpmin"),
   (GtkFunction) setkeysiggsharpmin},
  {KBD_CATEGORY_STAFF, N_("SetInitialDSharpmin"),
   (GtkFunction) setkeysigdsharpmin},
  {KBD_CATEGORY_STAFF, N_("SetInitialASharpmin"),
   (GtkFunction) setkeysigasharpmin},
  {KBD_CATEGORY_STAFF, N_("SetInitialDmin"), (GtkFunction) setkeysigdmin},
  {KBD_CATEGORY_STAFF, N_("SetInitialGmin"), (GtkFunction) setkeysiggmin},
  {KBD_CATEGORY_STAFF, N_("SetInitialCmin"), (GtkFunction) setkeysigcmin},
  {KBD_CATEGORY_STAFF, N_("SetInitialFmin"), (GtkFunction) setkeysigfmin},
  {KBD_CATEGORY_STAFF, N_("SetInitialBflatmin"),
   (GtkFunction) setkeysigbflatmin},
  {KBD_CATEGORY_STAFF, N_("SetInitialEflatmin"),
   (GtkFunction) setkeysigeflatmin},
  {KBD_CATEGORY_STAFF, N_("SetInitialAflatmin"),
   (GtkFunction) setkeysigaflatmin},


  {KBD_CATEGORY_EDIT, N_("SetMark"), (GtkFunction) set_mark},
  {KBD_CATEGORY_EDIT, N_("UnsetMark"), (GtkFunction) unset_mark},

  {KBD_CATEGORY_ARTICULATION, N_("ToggleBeginSlur"),
   (GtkFunction) toggle_begin_slur},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleEndSlur"),
   (GtkFunction) toggle_end_slur},

  {KBD_CATEGORY_ARTICULATION, N_("ToggleStartCrescendo"),
   (GtkFunction) toggle_start_crescendo},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleEndCrescendo"),
   (GtkFunction) toggle_end_crescendo},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleStartDiminuendo"),
   (GtkFunction) toggle_start_diminuendo},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleEndDiminuendo"),
   (GtkFunction) toggle_end_diminuendo},

  {KBD_CATEGORY_ARTICULATION, N_("ToggleAccent"), (GtkFunction) add_accent},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleFermata"), (GtkFunction) add_fermata},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleStaccato"),
   (GtkFunction) add_staccato},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleTenuto"), (GtkFunction) add_tenuto},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleTrill"), (GtkFunction) add_trill},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleTurn"), (GtkFunction) add_turn},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleMordent"), (GtkFunction) add_mordent},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleStaccatissimo"),
   (GtkFunction) add_staccatissimo},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleCoda"), (GtkFunction) add_coda},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleFlageolet"),
   (GtkFunction) add_flageolet},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleOpen"), (GtkFunction) add_open},
  {KBD_CATEGORY_ARTICULATION, N_("TogglePrallMordent"),
   (GtkFunction) add_prallmordent},
  {KBD_CATEGORY_ARTICULATION, N_("TogglePrallPrall"),
   (GtkFunction) add_prallprall},
  {KBD_CATEGORY_ARTICULATION, N_("TogglePrall"), (GtkFunction) add_prall},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleReverseTurn"),
   (GtkFunction) add_reverseturn},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleSegno"), (GtkFunction) add_segno},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleSforzato"),
   (GtkFunction) add_sforzato},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleStopped"), (GtkFunction) add_stopped},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleThumb"), (GtkFunction) add_thumb},
  /*{KBD_CATEGORY_ARTICULATION, N_("ToggleTrillElement"), (GtkFunction) add_trillelement},
     {KBD_CATEGORY_ARTICULATION, N_("ToggleTrill_Element"), (GtkFunction) add_trill_element}, */
  {KBD_CATEGORY_ARTICULATION, N_("ToggleUpprall"), (GtkFunction) add_upprall},
  {KBD_CATEGORY_ARTICULATION, N_("ToggleArpeggio"),
   (GtkFunction) add_arpeggio},
  {KBD_CATEGORY_ARTICULATION, N_("SetGrace"), (GtkFunction) set_grace},

  {KBD_CATEGORY_PLAYBACK, N_("PlayLocal"), (GtkFunction) playback_local},

  {KBD_CATEGORY_OTHER, N_("ForceCaution"), (GtkFunction) force_cautionary},

  {KBD_CATEGORY_NOTE_ENTRY, N_("ChangePitch"), (GtkFunction) change_pitch},
  {KBD_CATEGORY_OTHER, N_("DoubleBar"), (GtkFunction) insert_doublebar},
  {KBD_CATEGORY_OTHER, N_("EndBar"), (GtkFunction) insert_endbar},
  {KBD_CATEGORY_OTHER, N_("OpenRepeat"), (GtkFunction) insert_openrepeat},
  {KBD_CATEGORY_OTHER, N_("CloseRepeat"), (GtkFunction) insert_closerepeat},
  {KBD_CATEGORY_OTHER, N_("OpenCloseRepeat"),
   (GtkFunction) insert_opencloserepeat},
  {KBD_CATEGORY_OTHER, N_("InsertRhythm"),
   (GtkFunction) insert_rhythm_pattern},
  {KBD_CATEGORY_OTHER, N_("NextRhythm"),
   (GtkFunction) nextrhythm},
  {KBD_CATEGORY_MEASURE, N_("AppendMesauresToScore"),
   (GtkFunction) append_measure_score}


};
gint unmenued_commands_length = G_N_ELEMENTS (unmenued_commands);

/**
 * Warns user about old keymap file
 *
 */
static void
old_keymap_dialog ()
{
  GtkWidget *dialog;
  dialog = gtk_message_dialog_new (NULL,
				   (GtkDialogFlags)
				   (GTK_DIALOG_MODAL |
				    GTK_DIALOG_DESTROY_WITH_PARENT),
				   GTK_MESSAGE_WARNING,
				   GTK_BUTTONS_CLOSE,
				   _("Old keymap file found"));


  gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
					    _
					    ("From version 0.7.5, Denemo uses an xml file to store its key bindings. "
					     "I have found an old-style file, and am using that for now.\n\n"
					     "If you won't want to use an old version of Denemo in future, "
					     "please go to \"Edit,Set Keybindings\" and click \"OK and Save As Default\" "
					     "in order to avoid seeing this message again.\n\n"
					     "Thanks."));

  gtk_widget_show_all (dialog);
  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
}

/**
 * Warns user that there was no keymap available to load  
 *
 */
static void
no_map_dialog ()
{
  GtkWidget *dialog;
  dialog = gtk_message_dialog_new (NULL,
				   (GtkDialogFlags)
				   (GTK_DIALOG_MODAL |
				    GTK_DIALOG_DESTROY_WITH_PARENT),
				   GTK_MESSAGE_WARNING,
				   GTK_BUTTONS_CLOSE,
				   _
				   ("Keyboard shortcuts could not be found"));


  gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
					    _
					    ("No keymap file was found in either"
					     " the systemwide Denemo directory"
					     " or in .denemo directory within your "
					     "home directory. Please go to"
					     " Edit/Set Keybindings to construct a custom "
					     "interface or to load one from"
					     " an alternate keymap file."));

  gtk_widget_show_all (dialog);
  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
}


static keymap *allocate_keymap() {
  keymap *the_keymap = (keymap *) g_malloc (sizeof (keymap));
  gint i;
  gint n_unmenued_commands = (sizeof (unmenued_commands)
			      / sizeof (struct name_and_function));
  gchar *hold, **split;

  /* Allocate more than the necessary space for denemo_commands to start.  */
  denemo_commands = (struct name_action_and_function *)
    g_malloc (sizeof (struct name_action_and_function)
	      * (n_unmenued_commands + n_menu_items));
  for (i = 0; i < n_unmenued_commands; i++)
    {
      denemo_commands[i].name = unmenued_commands[i].name;
      denemo_commands[i].callback_action = -1;
      denemo_commands[i].func.nocallback = unmenued_commands[i].function;
    }
  denemo_commands_size = n_unmenued_commands;
  // TODO
  // Change this to use the GtkActionEntry data rather
  // than the GtkItemFactoryEntry

  for (i = 0; i < n_menu_items; i++)
    {
      if (menu_entries[i].name)
	{
	  /* The trickery with the strings eliminates spaces and underscores
	     from the command names as they appear in the keymap file and
	     in the keyboard customization interface.  */
	  hold = g_strdup (_(menu_entries[i].label));
	  hold = g_strdelimit (hold, " ", '_');
	  split = g_strsplit (hold, "_", 0);
	  g_free (hold);
	  denemo_commands[denemo_commands_size].name =
	    g_strjoinv (NULL, split);
	  g_strfreev (split);
	  denemo_commands[denemo_commands_size].callback_action = 0;
	  denemo_commands[denemo_commands_size].func.callback
	    = G_ACTIONCALLBACK (menu_entries[i].callback);
	  denemo_commands_size++;
	}
    }
  /* Now it's safe to shrink denemo_commands.  */
  denemo_commands =
    (struct name_action_and_function *) g_realloc (denemo_commands,
						   (sizeof
						    (struct
						     name_action_and_function)
						    * denemo_commands_size));

  the_keymap->commands =
    (GList **) g_malloc (sizeof (GList *) * denemo_commands_size);
  the_keymap->quick_lookup_hashes = g_hash_table_new (NULL, NULL);
  for (i = 0; i < denemo_commands_size; i++)
    the_keymap->commands[i] = NULL;

  return the_keymap;
}


/**
 * Create and Initialise a keymap structure
 * from user's standard keymap file (or system standard keymap file if none).
 *  @return The newly created keymap
 */
keymap *
init_keymap ()
{
  keymap *the_keymap = allocate_keymap();
  load_standard_keymap_file (the_keymap);
  return the_keymap;
}

/**
 * Create and Initialise a keymap structure from keymap file of name FILENAME
 * located in user standard place (or system standard place if none there).
 *  @return The newly created keymap
 */
keymap *
create_keymap (const gchar *filename)
{
  gchar *localrc = NULL;
  const gchar *dotdenemo = locatedotdenemo ();
  gchar *systemwide = g_build_filename (get_data_dir (), filename,
                                        NULL);
  keymap *the_keymap = allocate_keymap();
  if(dotdenemo)
    localrc = g_build_filename (dotdenemo, filename, NULL);
  load_keymap_file_named (the_keymap, localrc, systemwide);
  g_free(localrc);
  g_free(systemwide);
  return the_keymap;
}


/**
 * Utility function for clear_keymap 
 */
static gboolean
remove_it (gpointer key, gpointer value, gpointer user_data)
{
  return TRUE;
}

/**
 * Utility function for clear_keymap 
 */
static void
clear_hashtable (gpointer key, gpointer value, gpointer user_data)
{
  g_hash_table_destroy ((GHashTable *) value);
}

/**
 * Clears the keymap of all entries
 *
 */
void
clear_keymap (keymap * the_keymap)
{
  gint i;

  for (i = 0; i < denemo_commands_size; i++)
    if (the_keymap->commands[i])
      {
	g_list_foreach (the_keymap->commands[i], freeit, NULL);
	g_list_free (the_keymap->commands[i]);
	the_keymap->commands[i] = NULL;
      }
  g_hash_table_foreach (the_keymap->quick_lookup_hashes,
			(GHFunc) clear_hashtable, NULL);
  g_hash_table_foreach_remove (the_keymap->quick_lookup_hashes,
			       remove_it, NULL);
}

/**
 *  Search through keybindings for a specific binding
 *
 */
KeybindingInfo *
lookup_keybinding (keymap * the_keymap, gint keyval, gint state)
{
  GHashTable *quick_lookup_hash;

  if ((quick_lookup_hash
       = (GHashTable *) g_hash_table_lookup (the_keymap->quick_lookup_hashes,
					     GINT_TO_POINTER (MASK_FILTER
							      (state)))))
    return (KeybindingInfo *) g_hash_table_lookup (quick_lookup_hash,
						   GINT_TO_POINTER (keyval));
  else
    return NULL;
}

/**
 * Look up a key binding by name.
 * FIXME: This is inefficent. The keybinding structures needs a rework to be more usefull and robust.
 *
 * @param keymap
 * @param name
 * @returns the list of keybinding, or NULL if not found
 */
GList *
lookup_keybinding_by_name (keymap * keymap, const gchar * name)
{
  int i;
  int idx = -1;

  for (i = 0; i < denemo_commands_size; i++)
    {
      if (strcmp (denemo_commands[i].name, name) == 0)
	{
	  idx = i;
	  break;
	}
    }

  if (idx == -1)
    {
      g_warning ("Could not find keybinding '%s'\n", name);
      return NULL;
    }

  return keymap->commands[idx];
}

/**
 * Removes a keybinding from the keymap.  Note that it will not delete
 * a second-level hashtable from the_keymap if its size shrinks to 0.
 * Even though this mightn't be a bad idea, it doesn't really hurt to
 * keep them around either. 
 */

static void
remove_keybinding_helper (keymap * the_keymap,
			  GHashTable * quick_lookup_hash, KeybindingInfo * ki)
{
  g_hash_table_remove (quick_lookup_hash, GINT_TO_POINTER (ki->keyval));
  the_keymap->commands[ki->command_number]
    = g_list_remove (the_keymap->commands[ki->command_number], ki);
  g_free (ki);
}

/**
 * Removes a keybinding from the_keymap.  Wraps the helper function
 * more than anything else.  Note that there is a little bit of code
 * duplication between this and add_keybinding, though it's more
 * spread out in the latter. 
 */

void
remove_keybinding (keymap * the_keymap, gint keyval, gint state)
{
  GHashTable *quick_lookup_hash;
  KeybindingInfo *ki;

  if ((quick_lookup_hash
       = (GHashTable *) g_hash_table_lookup (the_keymap->quick_lookup_hashes,
					     GINT_TO_POINTER (state)))
      && (ki =
	  (KeybindingInfo *) g_hash_table_lookup (quick_lookup_hash,
						  GINT_TO_POINTER (keyval))))
    remove_keybinding_helper (the_keymap, quick_lookup_hash, ki);
}

/**
 * Adds a keybinding from the_keymap.  If the key was already bound,
 * this function removes the old binding and replaces it, returning
 * the number of the command this keybinding was attached to. Otherwise
 * returns -1. 
 */
gint
add_keybinding (keymap * the_keymap, gint keyval, gint state,
		gint command_number)
{
  KeybindingInfo *new_ki = (KeybindingInfo *)
    g_malloc (sizeof (KeybindingInfo));
  KeybindingInfo *ki_to_delete = NULL;
  GHashTable *quick_lookup_hash;
  gint ret = -1;

  state = MASK_FILTER (state);

  /* Initialize the info structure for the new keybinding */

  new_ki->keyval = keyval;
  new_ki->state = state;
  new_ki->command_number = command_number;
  new_ki->callback_action = denemo_commands[command_number].callback_action;
  new_ki->func = denemo_commands[command_number].func;
  /* works for either case of the union - nifty, no?  */

  /* Set quick_lookup_hash correctly, adding it to
     the_keymap->quick_lookup hashes if necessary. Also, if this key
     already has a binding, remove that binding. */
  if ((quick_lookup_hash
       = (GHashTable *) g_hash_table_lookup (the_keymap->quick_lookup_hashes,
					     GINT_TO_POINTER (state))))
    {
      /* Okay, there are commands with this set of bucky bits */
      if ((ki_to_delete
	   =
	   (KeybindingInfo *) g_hash_table_lookup (quick_lookup_hash,
						   GINT_TO_POINTER (keyval))))
	{
	  /* lo and behold, the command is already in the keymap.
	     Remove it before proceeding.  */
	  ret = ki_to_delete->command_number;
	  remove_keybinding_helper (the_keymap, quick_lookup_hash,
				    ki_to_delete);
	}
    }
  else
    {
      /* We need to create an appropriate quick_lookup_hash.  */
      quick_lookup_hash = g_hash_table_new (NULL, NULL);
      g_hash_table_insert (the_keymap->quick_lookup_hashes,
			   GINT_TO_POINTER (state), quick_lookup_hash);
    }

  /* We now know where the structures need to go, so let's put
     them there.  */

  g_hash_table_insert (quick_lookup_hash, GINT_TO_POINTER (keyval), new_ki);
  the_keymap->commands[command_number]
    = g_list_append (the_keymap->commands[command_number], new_ki);
  //if(ki_to_delete)
  //       g_free(ki_to_delete);
  return ret;
}

struct callbackdata
{
  keymap *the_keymap;
  GtkWidget *filesel;
};

/**
 * This function is a callback that is wrapper for
 * load_keymap_file 
 *FIXME note that non xml file support has been commented out
 */
void
load_keymap_from_dialog (GtkWidget * widget, struct callbackdata *cbdata)
{
  gchar *name = (gchar *)
    gtk_file_selection_get_filename (GTK_FILE_SELECTION (cbdata->filesel));
#if 0
  /* if(strcmp (name + strlen (name) - 4, ".xml") == 0) */
  load_xml_keymap (name, cbdata->the_keymap);
/*  else
  	load_keymap_file(name, cbdata->the_keymap);
  */
#else
  load_keymap_file_named(cbdata->the_keymap, NULL, name);
#endif
}

/**
 * Function for loading a keymap from an arbitrary place by way of
 * a user dialog.  Similar to file_open. called from kbd-interface.c:configure_keyboard_dialog_OLD
 */

void
load_keymap_dialog (GtkWidget * widget, keymap * the_keymap)
{
  GtkWidget *filesel;
  static struct callbackdata cbdata;
  static gchar *dotdenemo = NULL;

  if (!dotdenemo)
    dotdenemo = g_strconcat (locatedotdenemo (), "/", NULL);//FIXME G_DIR_SEPARATOR or g_build_filename
  filesel = gtk_file_selection_new (_("Load keymap"));
  gtk_file_selection_set_filename (GTK_FILE_SELECTION (filesel), dotdenemo);
  gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (filesel)->ok_button),
		      "clicked", GTK_SIGNAL_FUNC (load_keymap_from_dialog),
		      &cbdata);
  gtk_signal_connect_object (GTK_OBJECT
			     (GTK_FILE_SELECTION (filesel)->ok_button),
			     "clicked", GTK_SIGNAL_FUNC (gtk_widget_destroy),
			     GTK_OBJECT (filesel));
  gtk_signal_connect_object (GTK_OBJECT
			     (GTK_FILE_SELECTION (filesel)->cancel_button),
			     "clicked", GTK_SIGNAL_FUNC (gtk_widget_destroy),
			     GTK_OBJECT (filesel));

  cbdata.the_keymap = the_keymap;
  cbdata.filesel = filesel;
  gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
  gtk_window_set_position (GTK_WINDOW (filesel), GTK_WIN_POS_MOUSE);
  gtk_widget_show (filesel);
}

/**
 * Wrapper function to load keymap file
 * 
 */
void
load_standard_keymap_file_wrapper (GtkWidget * widget, keymap * the_keymap)
{
  load_standard_keymap_file (the_keymap);

}

/*
 * load_keymap_file_named: load a keymap file of the given name from one
 * of the two paths: the keymap file can be in xml or text format
 * param THE_KEYMAP must point to allocated memory for keymap.
 * param LOCALRC file to load (full path) or NULL
 * param SYSTEMWIDE fallback file (full path)

 */
static void
load_keymap_file_named (keymap * the_keymap, gchar *localrc, gchar *systemwide) {
/* 
  The plan:
     Load local file as xml 
  OR load local file as text and warn
  OR load system file as xml
  OR load system file as text (and warn?)
  OR warn
*/

  g_print ("Trying local file %s as xml...", localrc);
  if (load_xml_keymap (localrc, the_keymap) == -1)
    {
      g_print ("..no. As pre-0.7.5...");
      if (load_keymap_file (localrc, the_keymap) == FALSE)
	{
	  g_print ("..no.\nTrying systemwide file %s as xml...", systemwide);
	  if (load_xml_keymap (systemwide, the_keymap) == -1)
	    {
	      g_print ("..no. As pre-0.7.5...");
	      if (load_keymap_file (systemwide, the_keymap) == FALSE)
		{
		  g_print ("..no.\nNo useful keymaps found.\n");
		  no_map_dialog ();
		}
	      else
		{
		  g_print ("..ok.\n");
		  old_keymap_dialog ();
		}
	    }
	  else
	    g_print ("..ok.\n");
	}
      else
	{
	  g_print ("..ok.\n");
	  old_keymap_dialog ();
	}
    }
  else
    g_print ("..ok.\n");
}

/**
 * Load the either the local keymap 
 * or the global keymap of the standard name
 */
void
load_standard_keymap_file (keymap * the_keymap)
{
  gchar *localrc = NULL;
  const gchar *dotdenemo = locatedotdenemo ();
  gchar *systemwide = g_build_filename (get_data_dir (), "denemo.keymaprc",
                                        NULL);
  g_print ("systemwide = %s\n", systemwide);
  if(dotdenemo)
    localrc = g_build_filename (dotdenemo, "keymaprc", NULL);
  load_keymap_file_named (the_keymap, localrc, systemwide);
  g_free(localrc);
  g_free(systemwide);
}

static GScannerConfig scanner_config_template = {
  (" \t\r\n") /* cset_skip_characters */ ,
  (G_CSET_a_2_z "_0123456789/." G_CSET_A_2_Z) /* cset_identifier_first */ ,
  (G_CSET_a_2_z
   "_0123456789/."
   G_CSET_A_2_Z G_CSET_LATINS G_CSET_LATINC) /* cset_identifier_nth */ ,
  ("#\n") /* cpair_comment_single */ ,

  FALSE /* case_sensitive */ ,

  TRUE /* skip_comment_multi */ ,
  TRUE /* skip_comment_single */ ,
  TRUE /* scan_comment_multi */ ,
  TRUE /* scan_identifier */ ,
  TRUE /* scan_identifier_1char */ ,
  FALSE /* scan_identifier_NULL */ ,
  TRUE /* scan_symbols */ ,
  FALSE /* scan_binary */ ,
  FALSE /* scan_octal */ ,
  FALSE /* scan_float */ ,
  FALSE /* scan_hex */ ,
  FALSE /* scan_hex_dollar */ ,
  TRUE /* scan_string_sq */ ,
  TRUE /* scan_string_dq */ ,
  FALSE /* numbers_2_int */ ,
  FALSE /* int_2_float */ ,
  TRUE /* identifier_2_string */ ,
  TRUE /* char_2_token */ ,
  TRUE /* symbol_2_token */ ,
  FALSE				/* scope_0_fallback */
};

/**
 * This function loads a keymap file from file filename 
 * param FILENAME is full pathname of file to be loaded
 * return TRUE on success
 */
gboolean
load_keymap_file (gchar * filename, keymap * the_keymap)
{
  gint fd, keyval, state = 0, result;
  static GScanner *scanner = NULL;
  if(filename==NULL)
    return FALSE;
  if (!scanner)
    {
      int i;

      /* Create the scanner */
      scanner = g_scanner_new (&scanner_config_template);
      g_scanner_freeze_symbol_table (scanner);
      for (i = 0; i < denemo_commands_size; i++)
	g_scanner_add_symbol (scanner, _(denemo_commands[i].name),
			      GINT_TO_POINTER (i + G_TOKEN_LAST));
      g_scanner_thaw_symbol_table (scanner);
    }

  if ((fd = open (filename, O_RDONLY)) == -1)
    {
      g_warning (_("load_keymap_file : error opening %s : %s"), filename,
		 g_strerror (errno));
      return FALSE;
    }

  clear_keymap (the_keymap);
  g_scanner_input_file (scanner, fd);

  while ((result = g_scanner_get_next_token (scanner)) != G_TOKEN_EOF)
    {
      if (G_TOKEN_LAST <= result
	  && result < G_TOKEN_LAST + denemo_commands_size)
	/* We have a token for a named command type */
	while (g_scanner_peek_next_token (scanner) == G_TOKEN_STRING)
	  {
	    g_scanner_get_next_token (scanner);
	    keyval = gdk_keyval_from_name (scanner->value.v_string);
	    if (g_scanner_peek_next_token (scanner) == G_TOKEN_LEFT_PAREN)
	      {
		g_scanner_get_next_token (scanner);
		if (g_scanner_get_next_token (scanner) == G_TOKEN_STRING)
		  state = atoi (scanner->value.v_string);
		if (g_scanner_peek_next_token (scanner)
		    == G_TOKEN_RIGHT_PAREN)
		  g_scanner_get_next_token (scanner);
	      }
	    else
	      state = 0;
	    /* Okay. We've determined the keyval & event for this
	       keybinding, add it to the keymap, and overwrite what
	       the binding previously did.  */
	    add_keybinding (the_keymap, keyval, state, result - G_TOKEN_LAST);
	  }
      else
	{
	  g_warning (_("Unexpected token %s found in keymap file %s\n"),
		     scanner->value.v_string, filename);
	}
    }
  close (fd);
  return TRUE;
}

/**
 * Callback for saving the keymap to a given file
 *
 */
void
save_keymap_from_dialog (GtkWidget * widget, struct callbackdata *cbdata)
{
  save_xml_keymap ((gchar *)
		   gtk_file_selection_get_filename (GTK_FILE_SELECTION
						    (cbdata->filesel)),
		   cbdata->the_keymap);

}

/**
 * Function for saving a keymap to an arbitrary place by way of
 * a user dialog.  Similar to file_saveas. 
 */
void
save_keymap_dialog (GtkWidget * widget, keymap * the_keymap)
{
  GtkWidget *filesel;
  static gchar *dotdenemo = NULL;
  static struct callbackdata cbdata;

  if (!dotdenemo)
    dotdenemo = g_strconcat (locatedotdenemo (), "/", NULL);
  filesel = gtk_file_selection_new (_("Save keymap"));
  gtk_file_selection_set_filename (GTK_FILE_SELECTION (filesel), dotdenemo);
  gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (filesel)->ok_button),
		      "clicked", GTK_SIGNAL_FUNC (save_keymap_from_dialog),
		      &cbdata);
  gtk_signal_connect_object (GTK_OBJECT
			     (GTK_FILE_SELECTION (filesel)->ok_button),
			     "clicked", GTK_SIGNAL_FUNC (gtk_widget_destroy),
			     GTK_OBJECT (filesel));
  gtk_signal_connect_object (GTK_OBJECT
			     (GTK_FILE_SELECTION (filesel)->cancel_button),
			     "clicked", GTK_SIGNAL_FUNC (gtk_widget_destroy),
			     GTK_OBJECT (filesel));

  cbdata.the_keymap = the_keymap;
  cbdata.filesel = filesel;
  gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
  gtk_window_set_position (GTK_WINDOW (filesel), GTK_WIN_POS_MOUSE);
  gtk_widget_show (filesel);
}

/**
 * Wrapper function for saving the keymap to the standard place
 *
 */
void
save_standard_keymap_file_wrapper (GtkWidget * widget, DenemoGUI *gui)
{
  keymap * the_keymap = Denemo.prefs.the_keymap;
  if(the_keymap != Denemo.prefs.standard_keymap) {
    warningdialog("You are trying to overwrite your standard keymap with one designed for a particular mode.\nUse \"Save as Alternate Keymap File\" for this if you really mean it\n");
    return;
  }
    
  save_standard_keymap_file (the_keymap);
}

/**
 * Saves the keymap to the standard keymap file
 *
 */
void
save_standard_keymap_file (keymap * the_keymap)
{
  gchar *localrc = NULL;
  const gchar *dotdenemo = locatedotdenemo ();
  if(dotdenemo)
    localrc = g_build_filename (dotdenemo, "keymaprc", NULL);
  save_xml_keymap (localrc, the_keymap);
  g_free(localrc);
}

/**
 * g_list_foreach function invoked by save keymap file
 */
static void
write_keybinding_info (KeybindingInfo * ki, FILE * fp)
{
  fprintf (fp, "%s", gdk_keyval_name (ki->keyval));
  if (ki->state)
    fprintf (fp, "(%d)", ki->state);
  fprintf (fp, " ");
}

/**
 * This function saves the keymap to file filename 
 */
void
save_keymap_file (gchar * filename, keymap * the_keymap)
{
  FILE *fp;
  gint i;

  if ((fp = fopen (filename, "w")))
    {
      for (i = 0; i < denemo_commands_size; i++)
	{
	  fprintf (fp, "%s ", denemo_commands[i].name);
	  g_list_foreach (the_keymap->commands[i],
			  (GFunc) write_keybinding_info, fp);
	  fprintf (fp, "\n");
	}
      fclose (fp);
    }
  else
    g_warning (_("unable to write keymap file to %s\n"), filename);
}


/**
 * This function gets the caller a string useful for display
 */
void
set_state (gint state, gchar ** value)
{
  switch (state)
    {
    case 1:
      *value = "Shift+";
      break;
    case 4:
      *value = "Ctrl+";
      break;
    case 8:
      *value = "Alt+";
      break;
    case 5:
      *value = "Ctrl+Shift+";
      break;
    case 9:
      *value = "Alt+Shift+";
      break;
    case 12:
      *value = "Alt+Ctrl+";
      break;
    case 13:
      *value = "Alt+Ctrl+Shift+";
      break;
    }
}
