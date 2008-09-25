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

 
#define KBD_CATEGORY_DIRECT		0x100    /* does not require a wrapper to the callback */    



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
  char *icon;
  //char *menu_label;
  char *tooltip;
  char *name;
  char* function;
  char *menu_label;
  char *initial_setting;/*of radio/check items */
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

  {KBD_CATEGORY_NOTE_ENTRY, NULL, "Insert whole-note",	N_("WholeNote"), "insert_chord_0key", "\"MUSIC_FONT(\"0\")\""},
  {KBD_CATEGORY_NOTE_ENTRY,NULL , "Insert half-note",	N_("HalfNote"), "insert_chord_1key", "\"MUSIC_FONT(\"1\")\""},
  {KBD_CATEGORY_NOTE_ENTRY,NULL, "Insert quarter-note",	N_("QuarterNote"), "insert_chord_2key", "\"MUSIC_FONT(\"2\")\""},
  {KBD_CATEGORY_NOTE_ENTRY,NULL, "Insert eighth-note",	N_("EighthNote"), "insert_chord_3key", "\"MUSIC_FONT(\"3\")\""},
  {KBD_CATEGORY_NOTE_ENTRY,NULL, "Insert sixteenth-note",	N_("SixteenthNote"), "insert_chord_4key", "\"MUSIC_FONT(\"4\")\""},
  {KBD_CATEGORY_NOTE_ENTRY,NULL, "Insert thirty-second-note",	N_("ThirtysecondNote"), "insert_chord_5key", "\"MUSIC_FONT(\"5\")\""},
  {KBD_CATEGORY_NOTE_ENTRY,NULL , "Insert sixty-fourth-note",	N_("SixtyfourthNote"), "insert_chord_6key", "\"MUSIC_FONT(\"6\")\""},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankWholeNote"), "insert_blankchord_0key"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankHalfNote"), "insert_blankchord_1key"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankQuarterNote"), "insert_blankchord_2key"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankEighthNote"), "insert_blankchord_3key"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankSixteenthNote"), "insert_blankchord_4key"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankThirtysecondNote"), "insert_blankchord_5key"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankSixtyfourthNote"), "insert_blankchord_6key"},
  {KBD_CATEGORY_EDIT, NULL, "No Tooltip yet",	N_("ToggleRestMode"), "rest_toggle_key"},
  {KBD_CATEGORY_EDIT, NULL, "No Tooltip yet",	N_("ToggleBlankMode"), "toggle_blank"},

  {KBD_CATEGORY_REST_ENTRY, NULL, "Insert whole-note rest",  N_("InsertWholeRest"), "insert_rest_0key","\"MUSIC_FONT(\"r\")\""},
  {KBD_CATEGORY_REST_ENTRY,  NULL, "Insert half-note rest",	N_("InsertHalfRest"), "insert_rest_1key","\"MUSIC_FONT(\"s\")\""},
  {KBD_CATEGORY_REST_ENTRY,  NULL, "Insert quarter-note rest",	N_("InsertQuarterRest"), "insert_rest_2key","\"MUSIC_FONT(\"t\")\""},
  {KBD_CATEGORY_REST_ENTRY,  NULL, "Insert eighth-note rest",	N_("InsertEighthRest"), "insert_rest_3key","\"MUSIC_FONT(\"u\")\""},
  {KBD_CATEGORY_REST_ENTRY,  NULL, "Insert sixteenth-note rest",	N_("InsertSixteenthRest"), "insert_rest_4key","\"MUSIC_FONT(\"v\")\""},
  {KBD_CATEGORY_REST_ENTRY,  NULL, "Insert thirty-second note rest",	N_("InsertThirtysecondRest"), "insert_rest_5key","\"MUSIC_FONT(\"w\")\""},
  {KBD_CATEGORY_REST_ENTRY,  NULL, "Insert sixty-fourth note rest",	N_("InsertSixtyfourthRest"), "insert_rest_6key","\"MUSIC_FONT(\"x\")\""},


  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertDuplet"), "insert_duplet"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertTriplet"), "insert_triplet"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("StartTriplet"), "start_triplet"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("EndTuplet"), "end_tuplet"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertQuadtuplet"), "insert_quadtuplet"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertQuintuplet"), "insert_quintuplet"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertSextuplet"), "insert_sextuplet"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertSeptuplet"), "insert_septuplet"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "Add a note to the current chord\\nThe cursor position determines which note to add",	N_("AddNoteToChord"), "add_tone_key","Add note"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "Remove a note from the current chord",	N_("RemoveNoteFromChord"), "remove_tone_key","Remove note"},

  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("Sharpen"), "sharpen_key", "Sharpen"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("Flatten"), "flatten_key", "Flatten"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("StemUp"), "stem_up", "StemUp"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("StemDown"), "stem_down", "StemDown"},

  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("AddDot"), "add_dot_key"},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("RemoveDot"), "remove_dot_key"},

  {KBD_CATEGORY_ARTICULATION,  NULL, "Inserts a duplicate of the current note, tied",	N_("InsertTiedNote"), "tie_notes_key","Tied note"},

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
  {KBD_CATEGORY_MEASURE, NULL, "No Tooltip yet",	N_("AppendMeasuresToScore"), "append_measure_score"},
  
  /* from view.c menu_entries[]  */
  {KBD_CATEGORY_DIRECT, NULL, "Creating, saving, loading, displaying and printing musical scores", N_("FileMenu"), NULL, "File"},
  {KBD_CATEGORY_DIRECT, NULL, "Creating, saving places in musical scores", N_("Bookmarks"), NULL, "Bookmarks"},
  {KBD_CATEGORY_DIRECT, NULL, "Different keyboard entry modes", N_("EntryMenu"), NULL, "Mode"},  {KBD_CATEGORY_DIRECT, NULL, "General editing commands", N_("EditMenu"), NULL, "Edit"}, 
 {KBD_CATEGORY_DIRECT, NULL, "Control which tools are to be shown", N_("ViewMenu"), NULL, "View"}, 

  {KBD_CATEGORY_DIRECT, NULL, "Staffs and voices", N_("StaffMenu"), NULL,"Staffs/Voices"},
  {KBD_CATEGORY_DIRECT, NULL, "Movements in a score", N_("MovementMenu"), NULL,"Movements"},
  {KBD_CATEGORY_DIRECT, NULL, "Help with denemo", N_("HelpMenu"), NULL, "Help"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Playing the music through midi file", N_("PlaybackMenu"), NULL, "Playback"}, 



  {KBD_CATEGORY_DIRECT, "GTK_STOCK_NEW", "Start a new musical score", N_("New"), "file_newwrapper"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Open a file containing a music score for editing", N_("Open"), "file_open_with_check"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Add staffs from a Denemo file", N_("AddStaffs"), "file_add_staffs"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Add movements from a Denemo file", N_("AddMovements"), "file_add_movements"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PROPERTIES", "Change properties of this movement", N_("MovementProps"), "movement_props_dialog"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Open a file containing a music score for editing in a separate working area (tab", N_("OpenNewWindow"), "openinnew"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_SAVE", "Save the score", N_("Save"), "file_savewrapper"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_SAVE_AS", "Save the score under a new name", N_("SaveAs"), "file_saveaswrapper"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Start a new score from a built-in template file", N_("OpenTemplate"), "system_template_open_with_check"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Start a new score from a built-in example", N_("OpenExample"), "system_example_open_with_check"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Start a new score from one of your own template files", N_("OpenMyTemplate"), "local_template_open_with_check"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_SAVE_AS", "Save the score as a template for re-use as a starting point for new scores", N_("SaveTemplate"), "template_save"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Create working area (tab with an empty score in it)", N_("NewWindow"), "newview", "New Tab"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert a new movement before the current one", N_("InsertMovementBefore"), "insert_movement_before"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert a new movement after the current one", N_("InsertMovementAfter"), "insert_movement_after"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_SAVE_AS", "Save Parts: each staff becomes a file in lilypond format", N_("SaveParts"), "file_savepartswrapper"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_SAVE_AS", "Export the score as a PDF document file", N_("ExportPDF"), "export_pdf_action"},

  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PROPERTIES", "Start up a wizard to create a new score. This allows you to set various properties of the score", N_("ConfigureScore"), "scorewizard"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PRINT_PREVIEW", "Displays the final finished score in your pdf viewer", N_("PrintPreview"), "printpreview_cb"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PRINT_PREVIEW", "Displays a musical excerpt in your image viewer", N_("PrintExcerptPreview"), "printexcerptpreview_cb"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PRINT", "Displays the final finished score in a pdf viewer. From this you can print the file using the print command of the viewer", N_("Print"), "printall_cb"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PRINT", "Displays the final finished score for the current part (that is current staff", N_("PrintPart"), "printpart_cb"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_CLOSE", "Close the current score. Other windows will stay open", N_("Close"), "close_gui_with_check"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_QUIT", "Quit the Denemo program", N_("Quit"), "closewrapper"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_UNDO", "Undo", N_("Undo"), "undowrapper"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_REDO", "Redo", N_("Redo"), "redowrapper"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Selecting stretches of notes", N_("Select"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Extend the selection", N_("ExtendSelect"), NULL}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_COPY", "Copy", N_("Copy"), "copywrapper"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_CUT", "Cut", N_("Cut"), "cutwrapper"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PASTE", "Paste the selected music", N_("Paste"), "pastewrapper"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PROPERTIES", "Change some of the properties of the current score. This will start up a dialog window", N_("ScoreProperties"), "score_properties_dialog"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Save the selected music. Not sure if this is working", N_("SaveSelection"), "saveselwrapper"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PREFERENCES", "Set and save your preferences for how Denemo operates on startup. Edit .denemo/denemorc for missing ones", N_("Preferences"), "preferences_change"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Set actions to take in response to keypresses", N_("KeyBindings"), NULL, "Customize Commands, Shortcuts ..."},
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_SAVE", "Save the current commands and keyboard shortcuts as the default", N_("SaveAccels"), "save_default_keymap_file_wrapper", "Save Command Set"}, 
  {KBD_CATEGORY_DIRECT, NULL, "View help, change and save keyboard shortcuts", "CommandManagement", "configure_keyboard_dialog", "Manage Command Set"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Load Plugins", N_("LoadPlugins"), "load_plugin"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Unload Plugins", N_("UnloadPlugins"), "unloadplugins"}, 
  {KBD_CATEGORY_DIRECT, NULL, "List the loaded plugins", N_("ListPlugins"), "list_loaded_plugins"}, 
  {KBD_CATEGORY_DIRECT, NULL, "List the available plugins", N_("ListAvailablePlugins"), "list_available_plugins"}, 

  {KBD_CATEGORY_DIRECT, NULL, "Swap this staff with the one higher up. Note this actually swaps voices.", N_("SwapStaffs"), "swapstaffs"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Split off the next voice as a separate staff", N_("SplitVoices"), "splitstaffs"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Merge this staff as a voice on the previous staff", N_("JoinVoices"), "joinstaffs"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Swap this movement with the one before", N_("SwapMovements"), "swapmovements"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Go to the higher numbered voice (or staff if highest voice number on staff", N_("VoiceUp"),
    "voiceup_cb"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Go to the lower numbered voice on this staff", N_("VoiceDown"), "voicedown_cb"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserts a new staff before the current staff", N_("AddBefore"), "newstaffbefore"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserts/Adds a new staff after the current staff", N_("AddAfter"), "dnm_newstaffafter"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserts a new staff at the top of the score", N_("AddInitial"), "newstaffinitial"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserts a new staff at the end of the score", N_("AddLast"), "newstafflast"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Deletes the staff before the current staff", N_("DeleteBefore"), "delete_staff_before"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Deletes the current staff", N_("DeleteStaff"), "delete_staff_current"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Deletes the staff after the current staff", N_("DeleteAfter"), "delete_staff_after"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Adds a new voice (part), to the current staff. It is tricky to switch between the voices. Suggest to use merge staffs", "AddVoice", "dnm_newstaffvoice"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Transpose the current staff", N_("TransposeStaff"), "staff_transposition"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PROPERTIES", "Change the properties of the current staff", N_("StaffProperties"), "staff_properties_change_cb"},
  {KBD_CATEGORY_DIRECT, NULL, "Insert", N_("InsertMenu"), NULL},  
  {KBD_CATEGORY_DIRECT, NULL, "Clef", N_("Clef"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Change the initial clef of the current staff", N_("InitialClef"), "clef_change_initial"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert a change of clef at the cursor", N_("InsertClef"), "clef_change_insert"}, 


  {KBD_CATEGORY_DIRECT, NULL, "insert change key signature or set the initial key", N_("Key"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Set the initial key signature of the current staff", N_("InitialKey"), "key_change_initial"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert a key change at the cursor position", N_("InsertKey"), "key_change_insert"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Manage the time signature changes and initial value", N_("TimeSig"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Set the initial time signature of the current staff", N_("InitialTimeSig"), "timesig_change_initial"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Edit/Insert a time signature change for the current measure", N_("InsertTimeSig"), "timesig_change_insert"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Change the type of notehead for the current note", N_("ChangeNotehead"), "set_notehead"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserts a stem neutral tag. Click on this tag and use Sharpen/StemUp etc commands to change stem direction", N_("InsertStem"), "stem_directive_insert"},
  {KBD_CATEGORY_DIRECT, NULL, "Add a lyric to current note. Beware: all previous notes must have lyrics for printing correctly", "EditLyric", "lyric_insert", "Insert/Edit Lyric"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Add a bass figure to the current note. Use | sign to split the duration of a note so as to have multiple figures on one note. See Lilypond docs for other notation", N_("EditFiguredBass"), "figure_insert", "Insert/Edit Figured Bass"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Allows chord symbols to be added to the current note. E.G.cis:dim7 for c-sharp diminished 7th. See Lilypond docs for notation", N_("EditChords"), "fakechord_insert"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserts a dynamic marking at the cursor position", N_("InsertDynamic"), "insert_dynamic"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert or edit a directive in the LilyPond music typesetting language. This can be used for extra spacing, transposing or almost anything. See LilyPond documentation for ideas.", N_("InsertLilyDirective"), "lily_directive_insert"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert or edit a LilyPond text to be post-fixed to the current note. This can be used for guitar fingerings, cautionary accidentals and much more. See LilyPond documentation.",N_("InsertLilyPostfix"), "lily_directive_postfix"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserts specialized barline at the cursor position. Mostly not working", N_("InsertBarline"), "insert_barline"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Getting around the score", N_("NavigationMenu"), NULL, "Navigation"},
  {KBD_CATEGORY_DIRECT, NULL, "Opens a dialog for going to a numbered measure", N_("GoToMeasure"), "tomeasurenum", "Go to Measure"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_GOTO_FIRST", "Go To Beginning", N_("GoToBeginning"), "tohome","Go To Beginning"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_GOTO_LAST", "Go To End", N_("GoToEnd"), "toend",  "Go To End"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Go to the next movement", N_("NextMovement"), "next_movement"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Go to the previous movement", N_("PreviousMovement"), "prev_movement"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Delete the current movement", N_("DeleteMovement"), "delete_movement"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Delete all bookmarks in current movement", N_("DeleteBookmarks"), "deletebookmarks"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_MEDIA_PLAY", "Play", N_("Play"), "ext_midi_playback"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_MEDIA_STOP", "Stop", N_("Stop"), "stop_midi_playback"}, 

  {KBD_CATEGORY_DIRECT, "GTK_STOCK_MEDIA_PLAY", "Play using CSound...", N_("PlayCSound"), "csoundplayback"}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PROPERTIES", "Allows you to specify properties used in playing back (midi and csound", N_("PlaybackProperties"), "playback_properties_change", "Playback Properties"}, 

  {KBD_CATEGORY_DIRECT, NULL, "Opens a browser on the user manual", N_("Help"), "browse_manual"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Gives the version number etc of this program", N_("About"), "about"}, 


  {KBD_CATEGORY_DIRECT, NULL, "Allows choosing extra commands/menu items from disk", N_("MoreMenu"), NULL, "More"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Allows choosing standard extra commands/menu items", N_("MoreCommands"), "morecommands", "More Commands"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Allows choosing extra commands/menu items from your own collection of extras", N_("MyCommands"), "mycommands", "My Commands"}, 



  {KBD_CATEGORY_DIRECT, NULL, "Bookmark the current cursor position", N_("AddBookmark"), "addbookmark", "Add Bookmark"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Go to a bookmarked point in the score", N_("GotoBookmark"), "gotobookmark"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Go to the next bookmarked point in the list", N_("NextBookmark"), "nextbookmark"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Go to the previous bookmarked point in the list", N_("PrevBookmark"), "prevbookmark"}, 
 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Open previously used files", N_("OpenRecent"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Toggle between current mode and edit mode", N_("ToggleEdit"), "toggle_edit_mode"},
  {KBD_CATEGORY_DIRECT, NULL, "Toggle between note entry and rest entry", N_("ToggleRest"),  "toggle_rest_mode"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Toggle between note entry and rhythm entry", N_("ToggleRhythm"),  "toggle_rhythm_mode"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Clear the list of pitches that overlay the notes", N_("ClearOverlay"),  "clear_overlay"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Copy selection as a rhythm pattern for notes to follow as they are entered", N_("CreateRhythm"), "create_rhythm_cb"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Delete the selected rhythm pattern", N_("DeleteRhythm"), "delete_rhythm_cb"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Moving the cursor and inserting notes or rests there", N_("ClassicModeNote"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Moving the cursor to the nearest ...", N_("SelectNote"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Actions for notes: inserting, deleting, etc.", N_("InsertModeNote"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserting the note ...", N_("InsertNote"), NULL, "Note Entry" }, 
  {KBD_CATEGORY_DIRECT, NULL, "Anything not previously covered", N_("AllOther"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Moving around the piece", N_("Navigation"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Entering notes", N_("NoteEntry"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Various expressive marks", N_("Articulation"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Editing", N_("Edit"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Manipulating measures", N_("Measure"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Commands for staffs", N_("Staff"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Playing the music through midi file", N_("Playback"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Changing the prevailing duration or rhythm pattern", N_("SelectDuration"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Appending, Changing, and deleting notes", N_("EditModeNote"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Changing the note at the cursor to the nearest ...", N_("EditNote"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Changing the duration of note at the cursor or appending a note of the given duration", N_("EditDuration"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Moving the cursor", N_("Cursor"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert/change clef Set initial clef", N_("ClefMenu"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Adding notes to make chords", N_("ChordMenu"), NULL, "Chords"}, 
  {KBD_CATEGORY_DIRECT, NULL, "Measures: adding, deleting, navigating etc", N_("MeasureMenu"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserting notes, measures, staffs, keysigs etc", N_("Insert"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert a Staff relative to current staff", N_("InsertStaff"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert a Movement relative to current movement", N_("InsertMovement"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserting notes of a given duration", N_("InsertDuration"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Changing properties of notes, measures, staffs, keysigs etc", N_("Change"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Modeless actions on notes/rests", N_("ModelessNote"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Actions for notes/rests", N_("NotesRests"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Modeless entry of rests", N_("RestEntry"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Changing the note at the cursor to the nearest ...", N_("ChangeNote"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Changes the duration of the current note", N_("ChangeDuration"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Changes the duration of the current rest", N_("ChangeRest"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Dynamics, staccato, slurs, ties and other expressive marks", N_("ExpressionMarks"), NULL, "Expression Marks"}, 
  {KBD_CATEGORY_DIRECT, NULL, "grace notes etc", N_("Ornaments"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Lyrics, chord symbols, figured basses etc", N_("Other"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Less used actions", N_("Others"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Customized LilyPond inserts. Store often-used inserts here labelled with what they do", N_("Favorites"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Add a custom LilyPond insert to favorites menu", N_("AddFavorite"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Entering triplets and other tuplets", N_("Tuplets"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Deleting notes, measures staffs keysigs etc", N_("Delete"), NULL}, 

};






#define ni unmenued_commands[i].name
#define ii unmenued_commands[i].icon
#define ml unmenued_commands[i].menu_label
#define ti unmenued_commands[i].tooltip
#define fi unmenued_commands[i].function
#define mi unmenued_commands[i].category
int main() {
  FILE *callbacks, *entries, *xml, *scheme, *scheme_cb, *register_commands;
  scheme_cb = fopen("scheme_cb.h", "w");
  scheme = fopen("scheme.h", "w");
  callbacks = fopen("callbacks.h", "w");
  entries =  fopen("entries.h", "w");
  register_commands = fopen("register_commands.h", "w");
  xml = fopen("xml.fragment", "w");
  if(!callbacks || !entries || !xml || !scheme || !scheme_cb || !register_commands)
    return -1;
  fprintf(callbacks, "/******** generated automatically from generate_source. See generate_source.c */\n");
  fprintf(entries, "/******** generated automatically from generate_source. See generate_source.c */\n");

  int i,j;
  int n_unmenued_commands = (sizeof (unmenued_commands)
			   / sizeof (struct name_and_function));

  fprintf(scheme, "gchar *text;\n");
  for(i=0;i<n_unmenued_commands;i++) {
    if (fi != NULL)
      if(mi!=KBD_CATEGORY_DIRECT) {
      fprintf(callbacks, "/*%s %s*/\n",ni, fi);
      fprintf(callbacks, "static void %s_cb (GtkAction *action, gpointer param) {\n"
	      "  DenemoGUI *gui = Denemo.gui;\n"
	      "%s (gui);\ndisplayhelper (gui);\n%s}\n", fi, fi, mi==KBD_CATEGORY_NAVIGATION?"":"  score_status(gui, TRUE);\n");

      }
    if (fi != NULL) {
      fprintf(scheme, "/*%s %s*/\n",ni, fi);
      //if(mi==KBD_CATEGORY_DIRECT)
	fprintf(scheme, "SCM scheme_%s(SCM optional);\ninstall_scm_function (\"d-%s\", scheme_%s);\n", ni, ni, ni);// for direct callback via (scheme_xxx)
      fprintf(scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, \"%s\")), \"scm\", (gpointer)1);\n", ni); //define a property "scm" on the action to mean it scheme can call the action.
      //fprintf(scheme, "text = g_strdup_printf(\"(define dnm_%s %%d)\\n\", (int)action_of_name(Denemo.commands, \"%s\"));\n", ni, ni);
      //fprintf(scheme, "(void)scm_c_eval_string(text);\ng_free(text);\n");// for callback via (denemo dnm_xxx)
      //if(mi==KBD_CATEGORY_DIRECT)
	fprintf(scheme_cb, "SCM scheme_%s (SCM optional) {\nSCM ret;\nGString *gstr=NULL;\nint length;\n   char *str=NULL;\nif(SCM_STRINGP(optional)){\nstr = gh_scm2newstr(optional, &length);\ngstr = g_string_new_len(str, length);\n  }\n%s%s (NULL, gstr);\nif(gstr) {\nret=scm_makfrom0str(gstr->str);\ng_string_free(gstr, TRUE);} else\nret=scm_makfrom0str(\"\");\nreturn ret;\n}\n", ni, fi, mi!=KBD_CATEGORY_DIRECT?"_cb":"");
	fprintf(register_commands, "register_command(Denemo.commands, gtk_action_group_get_action(action_group, \"%s\"), \"%s\", \"%s\", \"%s\", %s);\n",ni,ni, ml?ml:ni, ti?ti:ni,fi);
    }


    if (fi != NULL)
      fprintf(entries,
	      "{\"%s\", %s, N_(\"%s\"), NULL,"
	      "N_(\"%s\"),"
	      "G_CALLBACK (%s%s)},\n",
	      ni, ii?ii:"NULL",ml?ml:ni, ti?ti:ni,fi,  (mi==KBD_CATEGORY_DIRECT)?"":"_cb");
    else
     fprintf(entries,
	      "{\"%s\", %s, N_(\"%s\"), NULL,"
	      "N_(\"%s\")},\n",
	      ni, ii?ii:"NULL",ml?ml:ni, ti?ti:ni);
  }

  /* generate source for duration callbacks - these were intercepted when
     typed at the keyboard to set prevailing rhythm, so the callback has to
     include code for this */

  for(i=0;i<7;i++) {
    /* callbacks for mode independent duration actions InsertRest0,1,2... ChangeRest0,1,2... InsertDur,ChangeDur0,1,2... */
    fprintf(callbacks, 
"static void InsertRest%d(GtkAction *action, gpointer param){\n"
"  DenemoGUI *gui = Denemo.gui;\n"
"  highlight_rest(gui, %d);\n"
"  gint mode = gui->mode;\n"
"  gui->mode = INPUTINSERT|INPUTREST;\n"
"  insert_chord_%dkey(gui);\n"
"  gui->mode = mode;\n"
"  score_status(gui, TRUE);\n"
"  displayhelper(gui);\n"
"}\n"

"static void ChangeRest%d(GtkAction *action, gpointer param){\n"
"  DenemoGUI *gui = Denemo.gui;\n"
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

"void InsertDur%d(GtkAction *action, gpointer param){\n"
"  DenemoGUI *gui = Denemo.gui;\n"
"  highlight_duration(gui, %d);\n"
"  gint mode = gui->mode;\n"
"  gui->mode = INPUTINSERT|INPUTNORMAL;\n"
"  insert_chord_%dkey(gui);\n"
"  gui->mode = mode;\n"
"  score_status(gui, TRUE);\n"
"  displayhelper(gui);\n"
"}\n"

"static void ChangeDur%d(GtkAction *action, gpointer param){\n"
"  DenemoGUI *gui = Denemo.gui;\n"
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
	    "static void Dur%d  (GtkAction *action, gpointer param) {\n"
	    "  DenemoGUI *gui = Denemo.gui;\n"
	    " if(gui->mode&INPUTINSERT)\n"
	    "   highlight_duration(gui, %d);\n"
	    " else \n"
	    " if( (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))\n"
	    "   ChangeDur%d (action, param);\n"
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
"static void ChangeTo%c(GtkAction *action, gpointer param){\n"
"  DenemoGUI *gui = Denemo.gui;\n"
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
"static void Insert%c(GtkAction *action, gpointer param){\n"
"  DenemoGUI *gui = Denemo.gui;\n"
"  gint mode = gui->mode;\n"
"  gui->mode = INPUTINSERT|INPUTNORMAL;\n"
"  go_to_%c_key(gui);\n"
"  gui->mode = mode;\n"
"  score_status(gui, TRUE);\n"
"  displayhelper(gui);\n"
	    "}\n", i, i);
  }

  for(i='A';i<='G';i++) {
    fprintf(register_commands,
	    "register_command(Denemo.commands, gtk_action_group_get_action(action_group, \"Insert%c\"), \"Insert%c\", \"Insert %c\",\"Inserts note %c before note at cursor\\nCursor determines which octave\\nNote is inserted in the prevailing rhythm\",  Insert%c);\n", i,i,i,i,i);

  }

  for(i='A';i<='G';i++) {
    fprintf(register_commands,
	    "register_command(Denemo.commands, gtk_action_group_get_action(action_group, \"ChangeTo%c\"), \"ChangeTo%c\", N_(\"Change to %c\"),N_(\"Changes note at cursor to nearest note %c\\nRhythm is unchanged\"),  ChangeTo%c);\n", i,i,i,i,i);

      fprintf(scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, \"ChangeTo%c\")), \"scm\", (gpointer)1);\n", i); //define a property "scm" on the action to mean scheme can call the action.
      fprintf(scheme, "SCM scheme_ChangeTo%c(SCM optional);\ninstall_scm_function (\"d-ChangeTo%c\", scheme_ChangeTo%c);\n", i, i, i);// for direct callback via (scheme_xxx)
      fprintf(scheme_cb, "SCM scheme_ChangeTo%c (SCM optional) {\nChangeTo%c (NULL, NULL);\nreturn SCM_EOL;\n}\n", i,  i);
      fprintf(scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, \"Insert%c\")), \"scm\", (gpointer)1);\n", i); //define a property "scm" on the action to mean scheme can call the action.
      fprintf(scheme, "SCM scheme_Insert%c(SCM optional);\ninstall_scm_function (\"d-Insert%c\", scheme_Insert%c);\n", i, i, i);// for direct callback via (scheme_xxx)
      fprintf(scheme_cb, "SCM scheme_Insert%c (SCM optional) {\nInsert%c (NULL, NULL);\nreturn SCM_EOL;\n}\n", i,  i);



  }

  for(i=0;i<7;i++) {
    /* registering commands for mode independent duration actions InsertRest0,1,2... ChangeRest0,1,2... InsertDur,ChangeDur0,1,2... */
    fprintf(register_commands, 
	    "register_command(Denemo.commands, gtk_action_group_get_action(action_group, \"%d\"), \"%d\", N_(MUSIC_FONT(\"%d\")), N_(\"In insert mode, changes prevailing rhythm to \"MUSIC_FONT(\"%d\")\"\\nIn edit mode changes the current note to \"MUSIC_FONT(\"%d\")\"\\n or appends a \"MUSIC_FONT(\"%d\")\" if no current note\\nIn classic mode inserts a \"MUSIC_FONT(\"%d\")\" at the cursor\"), Dur%d);\n", i, i, i, i, i, i, i, i);

    fprintf(register_commands, 
	    "register_command(Denemo.commands, gtk_action_group_get_action(action_group, \"Change%d\"), \"Change%d\", N_(MUSIC_FONT(\"%d\")), N_(\"Change the current note to a \"MUSIC_FONT(\"%d\")), ChangeDur%d);\n", i, i, i, i, i);

    fprintf(register_commands, 
	    "register_command(Denemo.commands, gtk_action_group_get_action(action_group, \"Insert%d\"), \"Insert%d\", N_(MUSIC_FONT(\"%d\")), N_(\"Insert a \"MUSIC_FONT(\"%d\")), InsertDur%d);\n", i, i, i, i, i);

    fprintf(register_commands, 
	    "register_command(Denemo.commands, gtk_action_group_get_action(action_group, \"InsertRest%d\"), \"InsertRest%d\",  N_(\"Insert a \"MUSIC_FONT(\"%d\")\"rest\") ,  N_(\"Inserts a rest at cursor position\\nSets prevailing rhythm to \"MUSIC_FONT(\"%d\")), InsertRest%d);\n", i, i, i, i, i);

    fprintf(register_commands, 
	    "register_command(Denemo.commands, gtk_action_group_get_action(action_group, \"ChangeRest%d\"), \"ChangeRest%d\",  N_(\"Change a \"MUSIC_FONT(\"%d\")\"rest\") ,  N_(\"Changes a rest at cursor position\\nSets prevailing rhythm to \"MUSIC_FONT(\"%d\")), ChangeRest%d);\n", i, i, i, i, i);

      fprintf(scheme, "/*%d */\n", i);
      fprintf(scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, \"%d\")), \"scm\", (gpointer)1);\n", i); //define a property "scm" on the action to mean scheme can call the action.

      fprintf(scheme, "SCM scheme_%d(SCM optional);\ninstall_scm_function (\"d-%d\", scheme_%d);\n", i, i, i);// for direct callback via (scheme_xxx)
      fprintf(scheme_cb, "SCM scheme_%d (SCM optional) {\nDur%d (NULL, NULL);\nreturn SCM_EOL;\n}\n", i,  i);

      fprintf(scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, \"Insert%d\")), \"scm\", (gpointer)1);\n", i); //define a property "scm" on the action to mean scheme can call the action.
      fprintf(scheme, "SCM scheme_InsertDur%d(SCM optional);\ninstall_scm_function (\"d-Insert%d\", scheme_InsertDur%d);\n", i, i, i);// for direct callback via (scheme_xxx)
      fprintf(scheme_cb, "SCM scheme_InsertDur%d (SCM optional) {\nInsertDur%d (NULL, NULL);\nreturn SCM_EOL;\n}\n", i,  i);

      fprintf(scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, \"Change%d\")), \"scm\", (gpointer)1);\n", i); //define a property "scm" on the action to mean scheme can call the action.
      fprintf(scheme, "SCM scheme_ChangeDur%d(SCM optional);\ninstall_scm_function (\"d-Change%d\", scheme_ChangeRest%d);\n", i, i, i);// for direct callback via (scheme_xxx)
      fprintf(scheme_cb, "SCM scheme_ChangeDur%d (SCM optional) {\nChangeDur%d (NULL, NULL);\nreturn SCM_EOL;\n}\n", i,  i);

      fprintf(scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, \"InsertRest%d\")), \"scm\", (gpointer)1);\n", i); //define a property "scm" on the action to mean scheme can call the action.
      fprintf(scheme, "SCM scheme_InsertRest%d(SCM optional);\ninstall_scm_function (\"d-InsertRest%d\", scheme_InsertRest%d);\n", i, i, i);// for direct callback via (scheme_xxx)
      fprintf(scheme_cb, "SCM scheme_InsertRest%d (SCM optional) {\nInsertRest%d (NULL, NULL);\nreturn SCM_EOL;\n}\n", i,  i);

      fprintf(scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, \"ChangeRest%d\")), \"scm\", (gpointer)1);\n", i); //define a property "scm" on the action to mean scheme can call the action.
      fprintf(scheme, "SCM scheme_ChangeRest%d(SCM optional);\ninstall_scm_function (\"d-ChangeRest%d\", scheme_ChangeRest%d);\n", i, i, i);// for direct callback via (scheme_xxx)
      fprintf(scheme_cb, "SCM scheme_ChangeRest%d (SCM optional) {\nChangeRest%d (NULL, NULL);\nreturn SCM_EOL;\n}\n", i,  i);


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
