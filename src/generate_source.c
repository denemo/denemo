/* 
 * generate_source.c
 *
 * Program for generating source code from the old unmenued commands
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (C) 2007 Richard Shann
 *
 * License: this file may be used under the FSF GPL version 2
 */


#include <stdio.h>
#define N_

#define KBD_CATEGORY_NAVIGATION		0
#define KBD_CATEGORY_NOTE_ENTRY		1
#define KBD_CATEGORY_REST_ENTRY		2
#define KBD_CATEGORY_ARTICULATION	3
#define KBD_CATEGORY_EDIT		4
#define KBD_CATEGORY_MEASURE		5
#define KBD_CATEGORY_STAFF		6
#define KBD_CATEGORY_PLAYBACK		7
#define KBD_CATEGORY_OTHER		8     

char *catname[9] = {N_("Navigation"),	
		   N_("Note entry"),	
		   N_("Rest entry"),	
		   N_("Articulation"),	
		   N_("Edit"),		
		   N_("Measure"),		
		   N_("Staff"),		
		   N_("Playback"),		
		   N_("Other") };







struct name_and_function
{
  unsigned category;
  /** Command name */

  char *menu_label;
  char *tooltip;
  char *name;
  char* function;
};


struct name_and_function unmenued_commands[] = {
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("CursorLeft"), "cursorleft"},
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("CursorDown"), "cursordown"},
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("CursorUp"), "cursorup"},
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("CursorRight"), "cursorright"},
  {KBD_CATEGORY_NAVIGATION, NULL, "Go to the staff above",	N_("StaffUp"), "staffup"},
  {KBD_CATEGORY_NAVIGATION, NULL, "Go to the staff below",	N_("StaffDown"), "staffdown"},
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("MeasureLeft"), "measureleft"},
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("MeasureRight"), "measureright"},
  {KBD_CATEGORY_NAVIGATION, NULL, "Move the cursor to A",	N_("A"), "go_to_A_key"},
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("B"), "go_to_B_key"},
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("C"), "go_to_C_key"},
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("D"), "go_to_D_key"},
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("E"), "go_to_E_key"},
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("F"), "go_to_F_key"},
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("G"), "go_to_G_key"},
  {KBD_CATEGORY_NAVIGATION, NULL, "Octave Up",	N_("OctaveUp"), "octave_up_key"},
  {KBD_CATEGORY_NAVIGATION, NULL, "Octave Down",	N_("OctaveDown"), "octave_down_key"},

  {KBD_CATEGORY_NOTE_ENTRY,  "\"MUSIC_FONT(\"0\")\"", "Insert whole-note",	N_("WholeNote"), "insert_chord_0key"},
  {KBD_CATEGORY_NOTE_ENTRY, "\"MUSIC_FONT(\"1\")\"", "Insert half-note",	N_("HalfNote"), "insert_chord_1key"},
  {KBD_CATEGORY_NOTE_ENTRY, "\"MUSIC_FONT(\"2\")\"", "Insert quarter-note",	N_("QuarterNote"), "insert_chord_2key"},
  {KBD_CATEGORY_NOTE_ENTRY, "\"MUSIC_FONT(\"3\")\"", "Insert eighth-note",	N_("EighthNote"), "insert_chord_3key"},
  {KBD_CATEGORY_NOTE_ENTRY, "\"MUSIC_FONT(\"4\")\"", "Insert sixteenth-note",	N_("SixteenthNote"), "insert_chord_4key"},
  {KBD_CATEGORY_NOTE_ENTRY, "\"MUSIC_FONT(\"5\")\"", "Insert thirty-second-note",	N_("ThirtysecondNote"), "insert_chord_5key"},
  {KBD_CATEGORY_NOTE_ENTRY, "\"MUSIC_FONT(\"6\")\"", "Insert sixty-fourth-note",	N_("SixtyfourthNote"), "insert_chord_6key"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankWholeNote"), "insert_blankchord_0key"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankHalfNote"), "insert_blankchord_1key"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankQuarterNote"), "insert_blankchord_2key"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankEighthNote"), "insert_blankchord_3key"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankSixteenthNote"), "insert_blankchord_4key"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankThirtysecondNote"), "insert_blankchord_5key"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankSixtyfourthNote"), "insert_blankchord_6key"},
  {KBD_CATEGORY_EDIT, NULL, "No Tooltip yet",	N_("ToggleRestMode"), "rest_toggle_key"},
  {KBD_CATEGORY_EDIT, NULL, "No Tooltip yet",	N_("ToggleBlankMode"), "toggle_blank"},

  {KBD_CATEGORY_REST_ENTRY, "\"MUSIC_FONT(\"r\")\"", "Insert whole-note rest",  N_("InsertWholeRest"), "insert_rest_0key"},
  {KBD_CATEGORY_REST_ENTRY,  "\"MUSIC_FONT(\"s\")\"", "Insert half-note rest",	N_("InsertHalfRest"), "insert_rest_1key"},
  {KBD_CATEGORY_REST_ENTRY,  "\"MUSIC_FONT(\"t\")\"", "Insert quarter-note rest",	N_("InsertQuarterRest"), "insert_rest_2key"},
  {KBD_CATEGORY_REST_ENTRY,  "\"MUSIC_FONT(\"u\")\"", "Insert eighth-note rest",	N_("InsertEighthRest"), "insert_rest_3key"},
  {KBD_CATEGORY_REST_ENTRY,  "\"MUSIC_FONT(\"v\")\"", "Insert sixteenth-note rest",	N_("InsertSixteenthRest"), "insert_rest_4key"},
  {KBD_CATEGORY_REST_ENTRY,  "\"MUSIC_FONT(\"w\")\"", "Insert thirty-second note rest",	N_("InsertThirtysecondRest"), "insert_rest_5key"},
  {KBD_CATEGORY_REST_ENTRY,  "\"MUSIC_FONT(\"x\")\"", "Insert sixty-fourth note rest",	N_("InsertSixtyfourthRest"), "insert_rest_6key"},


  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertDuplet"), "insert_duplet"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertTriplet"), "insert_triplet"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("StartTriplet"), "start_triplet"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("EndTuplet"), "end_tuplet"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertQuadtuplet"), "insert_quadtuplet"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertQuintuplet"), "insert_quintuplet"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertSextuplet"), "insert_sextuplet"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertSeptuplet"), "insert_septuplet"},
  {KBD_CATEGORY_NOTE_ENTRY, "Add note", "Add a note to the current chord\\nThe cursor position determines which note to add",	N_("AddTone"), "add_tone_key"},
  {KBD_CATEGORY_NOTE_ENTRY, "Remove note", "Remove a note from the current chord",	N_("RemoveTone"), "remove_tone_key"},

  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("Sharpen/StemDown"), "sharpen_key"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("Flatten/StemUp"), "flatten_key"},

  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("AddDot"), "add_dot_key"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("RemoveDot"), "remove_dot_key"},

  {KBD_CATEGORY_ARTICULATION, "Tied note", "Inserts a duplicate of the current note, tied",	N_("InsertTiedNote"), "tie_notes_key"},

  {KBD_CATEGORY_EDIT, NULL, "No Tooltip yet",	N_("DeleteObject"), "deleteobject"},
  {KBD_CATEGORY_EDIT, NULL, "No Tooltip yet",	N_("DeletePreviousObject"), "deletepreviousobject"},

  {KBD_CATEGORY_MEASURE, NULL, "No Tooltip yet",	N_("InsertMeasure"), "insert_measure_key"},
  {KBD_CATEGORY_MEASURE, NULL, "No Tooltip yet",	N_("AppendMeasure"), "append_measure_key"},
  {KBD_CATEGORY_MEASURE, NULL, "Delete the current measure in this staff, leaving the staff short",	N_("DeleteMeasure"), "deletemeasure"},
  {KBD_CATEGORY_MEASURE, NULL, "Delete the current measure in all staffs",	N_("DeleteMeasureAllStaffs"), "deletemeasureallstaffs"},
  {KBD_CATEGORY_MEASURE, NULL, "No Tooltip yet",	N_("ShrinkMeasures"), "adjust_measure_less_width_key"},
  {KBD_CATEGORY_MEASURE, NULL, "No Tooltip yet",	N_("WidenMeasures"), "adjust_measure_more_width_key"},

  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("ShorterStaffs"), "adjust_staff_less_height_key"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("TallerStaffs"), "adjust_staff_more_height_key"},

  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertTrebleClef"), "newcleftreble"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertBassClef"), "newclefbass"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insertg8clef"), "newclefg8"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertAltoClef"), "newclefalto"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertTenorClef"), "newcleftenor"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertSopranoClef"), "newclefsoprano"},

  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialTrebleClef"), "setcleftreble"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialBassClef"), "setclefbass"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialg8clef"), "setclefg8"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialAltoClef"), "setclefalto"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialTenorClef"), "setcleftenor"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialSopranoClef"), "setclefsoprano"},

  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert22Time"), "newtimesig22"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert32Time"), "newtimesig32"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert42Time"), "newtimesig42"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert44Time"), "newtimesig44"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert34Time"), "newtimesig34"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert24Time"), "newtimesig24"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert64Time"), "newtimesig64"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert38Time"), "newtimesig38"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert68Time"), "newtimesig68"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert128Time"), "newtimesig128"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert98Time"), "newtimesig98"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set22Time"), "settimesig22"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set32Time"), "settimesig32"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set42Time"), "settimesig42"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set44Time"), "settimesig44"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set34Time"), "settimesig34"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set24Time"), "settimesig24"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set64Time"), "settimesig64"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set38Time"), "settimesig38"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set68Time"), "settimesig68"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set128Time"), "settimesig128"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set98Time"), "settimesig98"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertCmaj"), "newkeysigcmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertGmaj"), "newkeysiggmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertDmaj"), "newkeysigdmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertAmaj"), "newkeysigamaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertEmaj"), "newkeysigemaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertBmaj"), "newkeysigbmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertFSharpmaj"), "newkeysigfsharpmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertCSharpmaj"), "newkeysigcsharpmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertFmaj"), "newkeysigfmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertBflatmaj"), "newkeysigbflatmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertEflatmaj"), "newkeysigeflatmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertAflatmaj"), "newkeysigaflatmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertDflatmaj"), "newkeysigdflatmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertGflatmaj"), "newkeysiggflatmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertCflatmaj"), "newkeysigcflatmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertAmin"), "newkeysigamin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertEmin"), "newkeysigemin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertBmin"), "newkeysigbmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertFSharpmin"), "newkeysigfsharpmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertCSharpmin"), "newkeysigcsharpmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertGSharpmin"), "newkeysiggsharpmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertDSharpmin"), "newkeysigdsharpmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertASharpmin"), "newkeysigasharpmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertDmin"), "newkeysigdmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertGmin"), "newkeysiggmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertCmin"), "newkeysigcmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertFmin"), "newkeysigfmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertBflatmin"), "newkeysigbflatmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertEflatmin"), "newkeysigeflatmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertAflatmin"), "newkeysigaflatmin"},

  //Functions to Set Initial Key Sig
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialCmaj"), "setkeysigcmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialGmaj"), "setkeysiggmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialDmaj"), "setkeysigdmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialAmaj"), "setkeysigamaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialEmaj"), "setkeysigemaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialBmaj"), "setkeysigbmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialFSharpmaj"), "setkeysigfsharpmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialCSharpmaj"), "setkeysigcsharpmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialFmaj"), "setkeysigfmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialBflatmaj"), "setkeysigbflatmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialEflatmaj"), "setkeysigeflatmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialAflatmaj"), "setkeysigaflatmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialDflatmaj"), "setkeysigdflatmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialGflatmaj"), "setkeysiggflatmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialCflatmaj"), "setkeysigcflatmaj"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialAmin"), "setkeysigamin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialEmin"), "setkeysigemin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialBmin"), "setkeysigbmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialFSharpmin"), "setkeysigfsharpmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialCSharpmin"), "setkeysigcsharpmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialGSharpmin"), "setkeysiggsharpmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialDSharpmin"), "setkeysigdsharpmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialASharpmin"), "setkeysigasharpmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialDmin"), "setkeysigdmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialGmin"), "setkeysiggmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialCmin"), "setkeysigcmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialFmin"), "setkeysigfmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialBflatmin"), "setkeysigbflatmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialEflatmin"), "setkeysigeflatmin"},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialAflatmin"), "setkeysigaflatmin"},


  {KBD_CATEGORY_OTHER, NULL, "No Tooltip yet",	N_("SetMark"), "set_mark"},
  {KBD_CATEGORY_OTHER, NULL, "No Tooltip yet",	N_("UnsetMark"), "unset_mark"},

  {KBD_CATEGORY_ARTICULATION, NULL, "Insert/delete begin slur on this note",	N_("ToggleBeginSlur"), "toggle_begin_slur"},
  {KBD_CATEGORY_ARTICULATION, NULL, "Insert/delete end slur on this note",	N_("ToggleEndSlur"), "toggle_end_slur"},

  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleStartCrescendo"), "toggle_start_crescendo"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleEndCrescendo"), "toggle_end_crescendo"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleStartDiminuendo"), "toggle_start_diminuendo"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleEndDiminuendo"), "toggle_end_diminuendo"},

  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleAccent"), "add_accent"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleFermata"), "add_fermata"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleStaccato"), "add_staccato"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleTenuto"), "add_tenuto"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleTrill"), "add_trill"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleTurn"), "add_turn"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleMordent"), "add_mordent"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleStaccatissimo"), "add_staccatissimo"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleCoda"), "add_coda"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleFlageolet"), "add_flageolet"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleOpen"), "add_open"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("TogglePrallMordent"), "add_prallmordent"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("TogglePrallPrall"), "add_prallprall"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("TogglePrall"), "add_prall"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleReverseTurn"), "add_reverseturn"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleSegno"), "add_segno"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleSforzato"), "add_sforzato"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleStopped"), "add_stopped"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleThumb"), "add_thumb"},
  /*{KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleTrillElement"), "add_trillelement"},
     {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleTrill_Element"), "add_trill_element"}, */
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleUpprall"), "add_upprall"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleArpeggio"), "add_arpeggio"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("SetGrace"), "set_grace"},

  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ForceCaution"), "force_cautionary"},

  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("ChangePitch"), "change_pitch"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("DoubleBar"), "insert_doublebar"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("EndBar"), "insert_endbar"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("OpenRepeat"), "insert_openrepeat"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("CloseRepeat"), "insert_closerepeat"},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("OpenCloseRepeat"), "insert_opencloserepeat"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertRhythm"), "insert_rhythm_pattern"},
  {KBD_CATEGORY_OTHER, NULL, "Make next rhythm pattern\\nthe prevailing rhythm.\\nNotes entered will follow this pattern",	N_("NextRhythm"), "nextrhythm"},
  {KBD_CATEGORY_MEASURE, NULL, "No Tooltip yet",	N_("AppendMesauresToScore"), "append_measure_score"}


};






#define ni unmenued_commands[i].name
#define ii unmenued_commands[i].menu_label
#define ti unmenued_commands[i].tooltip
#define fi unmenued_commands[i].function
#define mi unmenued_commands[i].category
int main() {
  FILE *callbacks, *entries, *xml;
  callbacks = fopen("callbacks.h", "w");
  entries =  fopen("entries.h", "w");
  xml = fopen("xml.fragment", "w");
  if(!callbacks || !entries || !xml)
    return -1;
  fprintf(callbacks, "/******** generated automatically from generate_source. See generate_source.c */\n");
  fprintf(entries, "/******** generated automatically from generate_source. See generate_source.c */\n");

  int i,j;
  int n_unmenued_commands = (sizeof (unmenued_commands)
			   / sizeof (struct name_and_function));

  for(i=0;i<n_unmenued_commands;i++) {
    fprintf(callbacks, "/*%s %s*/\n",ni, fi);
    fprintf(callbacks, "static void %s_cb (GtkAction *a, DenemoGUI *gui) {\n"
	   "%s (gui);\ndisplayhelper (gui);\n%s}\n", fi, fi, mi==KBD_CATEGORY_NAVIGATION?"":"  score_status(gui, TRUE);\n");
    fprintf(entries,
  "{\"%s\", NULL, N_(\"%s\"), NULL,"
   "N_(\"%s\"),"
	    "G_CALLBACK (%s_cb)},\n",
	    ni, ii?ii:ni, ti?ti:ni,fi);
  }

  /* generate source for duration callbacks - these were intercepted when
     typed at the keyboard to set prevailing rhythm, so the callback has to
     include code for this */

  for(i=0;i<7;i++) {
    /* callbacks for mode independent duration actions InsertRest0,1,2... ChangeRest0,1,2... InsertDur,ChangeDur0,1,2... */
    fprintf(callbacks, 
"static void InsertRest%d(GtkWidget *menuitem, DenemoGUI *gui){\n"
"  highlight_rest(gui, %d);\n"
"  gint mode = gui->mode;\n"
"  gui->mode = INPUTINSERT|INPUTREST;\n"
"  insert_chord_%dkey(gui);\n"
"  gui->mode = mode;\n"
"  score_status(gui, TRUE);\n"
"  displayhelper(gui);\n"
"}\n"

"static void ChangeRest%d(GtkWidget *menuitem, DenemoGUI *gui){\n"
"  gint mode = gui->mode;\n"
"  gboolean appending = gui->si->cursor_appending;\n"
"  if(appending)\n"
"    cursorleft(gui);\n"
"  gui->mode = INPUTEDIT|INPUTREST;\n"
"  insert_chord_%dkey(gui);\n"
"  gui->mode = mode;\n"
"  if(appending)\n"
"    cursorright(gui);\n"
"  score_status(gui, TRUE);\n"
"  displayhelper(gui);\n"
"}\n"

"void InsertDur%d(GtkWidget *menuitem, DenemoGUI *gui){\n"
"  highlight_duration(gui, %d);\n"
"  gint mode = gui->mode;\n"
"  gui->mode = INPUTINSERT|INPUTNORMAL;\n"
"  insert_chord_%dkey(gui);\n"
"  gui->mode = mode;\n"
"  score_status(gui, TRUE);\n"
"  displayhelper(gui);\n"
"}\n"

"static void ChangeDur%d(GtkWidget *menuitem, DenemoGUI *gui){\n"
"  gint mode = gui->mode;\n"
"  gboolean appending = gui->si->cursor_appending;\n"
"  if(appending)\n"
"    cursorleft(gui);\n"
"  gui->mode = INPUTEDIT|INPUTNORMAL;\n"
"  insert_chord_%dkey(gui);\n"
"  gui->mode = mode;\n"
"  if(appending)\n"
"    cursorright(gui);\n"
"  displayhelper(gui);\n"
"}\n",i,i,i,i,i,i,i,i,i,i);


    /* callbacks for mode sensitive  duration actions, Dur0,1,2 ... */
    fprintf(callbacks, 
	    "static void Dur%d  (GtkWidget *w, DenemoGUI *gui) {\n"
	    " if(gui->mode&INPUTINSERT)\n"
	    "   highlight_duration(gui, %d);\n"
	    " else \n"
	    " if( (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))\n"
	    "   ChangeDur%d (w, gui);\n"
	    "else {\n"
	    " insert_chord_%dkey(gui);\n"
	    "  score_status(gui, TRUE);\n"
	    " displayhelper(gui);\n"
	    " }\n"
	    "}\n", i , i, i, i);

    /* menu_entries for the mode sensitive duration actions, Dur0,1,2 ... */
    fprintf(entries,
  "{\"%d\", \"NULL\", N_(MUSIC_FONT(\"%d\")), NULL, N_(\"In insert mode, changes prevailing rhythm to \"MUSIC_FONT(\"%d\")\"\\nIn edit mode changes the current note to \"MUSIC_FONT(\"%d\")\"\\n or appends a \"MUSIC_FONT(\"%d\")\" if no current note\\nIn classic mode inserts a \"MUSIC_FONT(\"%d\")\" at the cursor\"),\n"
	    "G_CALLBACK (Dur%d)},\n"

  "{\"Change%d\", \"NULL\", N_(MUSIC_FONT(\"%d\")), NULL, N_(\"Change current note to a \"MUSIC_FONT(\"%d\")),\n"
	    "G_CALLBACK (ChangeDur%d)},\n"
  "{\"ChangeRest%d\", NULL, N_(\"Change duration\"), NULL, N_(\"Change durtion of current rest\"),\n"
    "G_CALLBACK (ChangeRest%d)},\n"
 "{\"Insert%d\", NULL, N_(\"Insert a \"MUSIC_FONT(\"%d\")\"\"), NULL, N_(\"Inserts a \"MUSIC_FONT(\"%d\")\" at cursor position\\nSets prevailing rhythm to \"MUSIC_FONT(\"%d\")),\n"
  "G_CALLBACK (InsertDur%d)},\n"
 "{\"InsertRest%d\", NULL, N_(\"Insert a \"MUSIC_FONT(\"%d\")\"rest\"), NULL, N_(\"Inserts a rest at cursor position\\nSets prevailing rhythm to \"MUSIC_FONT(\"%d\")),\n"
	    "G_CALLBACK (InsertRest%d)},\n",
     i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i,i,i );
  }


  for(i=0;i<7;i++) 
    fprintf(xml, "<menuitem action=\"%d\"/>\n", i);
  for(i=0;i<7;i++) 
    fprintf(xml, "<menuitem action=\"Change%d\"/>\n", i);
  for(i=0;i<7;i++) 
    fprintf(xml, "<menuitem action=\"Insert%d\"/>\n", i);
  for(i=0;i<7;i++) 
    fprintf(xml, "<menuitem action=\"ChangeRest%d\"/>\n", i);
  for(i=0;i<7;i++) 
    fprintf(xml, "<menuitem action=\"InsertRest%d\"/>\n", i);
  
  for(i='A';i<='G';i++) 
    fprintf(xml, "<menuitem action=\"Insert%c\"/>\n", i);
  for(i='A';i<='G';i++) 
    fprintf(xml, "<menuitem action=\"ChangeTo%c\"/>\n", i);



  /* menu_entries for the mode    note name    */
  for(i='A';i<='G';i++) {
    fprintf(entries,
" {\"Insert%c\", NULL, N_(\"Insert %c\"), NULL, N_(\"Inserts note %c before note at cursor\\nCursor determines which octave\\nNote is inserted in the prevailing rhythm\"),\n"
"  G_CALLBACK (Insert%c)},\n"
"  {\"ChangeTo%c\", NULL, N_(\"Change current note to %c\"), NULL, N_(\"Changes current note to the %c nearest cursor or (if no current note) inserts the note %c\\nCursor determines which octave\\nNote is inserted in the prevailing rhythm\"),\n"
"   G_CALLBACK (ChangeTo%c)},\n"

    ,i ,i ,i ,i ,i ,i ,i ,i ,i);

  }

  for(i='A';i<='G';i++) {
    fprintf(callbacks,
"static void ChangeTo%c(GtkWidget *menuitem, DenemoGUI *gui){\n"
"  gboolean appending = gui->si->cursor_appending;\n"
"  if(appending)\n"
"    cursorleft(gui); \n"
"  gint mode = gui->mode;\n"
"  gui->mode = INPUTEDIT|INPUTNORMAL;\n"
"  go_to_%c_key(gui);\n"
"  gui->mode = mode;\n"
"  if(appending)\n"
"    cursorright(gui);\n"
"  score_status(gui, TRUE);\n"
"  displayhelper(gui);\n"
"}\n", i, i);
  }
  for(i='A';i<='G';i++) {

    fprintf(callbacks,
"static void Insert%c(GtkWidget *menuitem, DenemoGUI *gui){\n"
"  gint mode = gui->mode;\n"
"  gui->mode = INPUTINSERT|INPUTNORMAL;\n"
"  go_to_%c_key(gui);\n"
"  gui->mode = mode;\n"
"  score_status(gui, TRUE);\n"
"  displayhelper(gui);\n"
	    "}\n", i, i);
  }



#ifdef GENERATE_XML_FRAGMENT
  fprintf(xml, "<menu action=\"AllOther\">\n");
  for(j=0;j<9;j++){
    fprintf(xml, "<menu action=\"%s\">\n", catname[j]);
    for(i=0;i<n_unmenued_commands;i++) {
      if(mi == j) {
	fprintf(xml, "<menuitem action=\"%s\"/>\n", ni);
      }
      
    }
    fprintf(xml, "</menu>\n");
  }
  fprintf(xml, "</menu>\n");
#endif
  return 0;      
}
