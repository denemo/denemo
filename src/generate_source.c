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
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("CursorLeft"), "cursorleft", N_("Cursor Left")},
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("CursorDown"), "cursordown", N_("Cursor Down")},
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("CursorUp"), "cursorup", N_("Cursor Up")},
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("CursorRight"), "cursorright", N_("Cursor Right")},
  {KBD_CATEGORY_NAVIGATION, NULL, "Go to the staff above",	N_("StaffUp"), "staffup", N_("Staff Up")},
  {KBD_CATEGORY_NAVIGATION, NULL, "Go to the staff below",	N_("StaffDown"), "staffdown", N_("Staff Down")},
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("MeasureLeft"), "measureleft", N_("Measure Left")},
  {KBD_CATEGORY_NAVIGATION, NULL, "No Tooltip yet",	N_("MeasureRight"), "measureright", N_("Measure Right")},
  {KBD_CATEGORY_NAVIGATION, NULL, "Action for note A (Insert, Edit or Move Cursor, depending on Mode)",	N_("A"), "go_to_A_key", N_("A")},
  {KBD_CATEGORY_NAVIGATION, NULL, "Action for note B (Insert, Edit or Move Cursor, depending on Mode)",	N_("B"), "go_to_B_key", N_("B")},
  {KBD_CATEGORY_NAVIGATION, NULL, "Action for note C (Insert, Edit or Move Cursor, depending on Mode)",	N_("C"), "go_to_C_key", N_("C")},
  {KBD_CATEGORY_NAVIGATION, NULL, "Action for note D (Insert, Edit or Move Cursor, depending on Mode)",	N_("D"), "go_to_D_key", N_("D")},
  {KBD_CATEGORY_NAVIGATION, NULL, "Action for note E (Insert, Edit or Move Cursor, depending on Mode)",	N_("E"), "go_to_E_key", N_("E")},
  {KBD_CATEGORY_NAVIGATION, NULL, "Action for note F (Insert, Edit or Move Cursor, depending on Mode)",	N_("F"), "go_to_F_key", N_("F")},
  {KBD_CATEGORY_NAVIGATION, NULL, "Action for note G (Insert, Edit or Move Cursor, depending on Mode)",	N_("G"), "go_to_G_key", N_("G")},
  {KBD_CATEGORY_NAVIGATION, NULL, "Octave Up",	N_("OctaveUp"), "octave_up_key", N_("Octave Up")},
  {KBD_CATEGORY_NAVIGATION, NULL, "Octave Down",	N_("OctaveDown"), "octave_down_key", N_("Octave Down")},

  {KBD_CATEGORY_NOTE_ENTRY, NULL, "Insert whole-note",	N_("WholeNote"), "insert_chord_0key", "\"MUSIC_FONT(\"0\")\"", N_("Insert Whole Note")},
  {KBD_CATEGORY_NOTE_ENTRY,NULL , "Insert half-note",	N_("HalfNote"), "insert_chord_1key", "\"MUSIC_FONT(\"1\")\"", N_("Insert Half Note")},
  {KBD_CATEGORY_NOTE_ENTRY,NULL, "Insert quarter-note",	N_("QuarterNote"), "insert_chord_2key", "\"MUSIC_FONT(\"2\")\"", N_("Insert Quarter Note")},
  {KBD_CATEGORY_NOTE_ENTRY,NULL, "Insert eighth-note",	N_("EighthNote"), "insert_chord_3key", "\"MUSIC_FONT(\"3\")\"", N_("Insert Eigth Note")},
  {KBD_CATEGORY_NOTE_ENTRY,NULL, "Insert sixteenth-note",	N_("SixteenthNote"), "insert_chord_4key", "\"MUSIC_FONT(\"4\")\"", N_("Insert Sixteenth Note")},
  {KBD_CATEGORY_NOTE_ENTRY,NULL, "Insert thirty-second-note",	N_("ThirtysecondNote"), "insert_chord_5key", "\"MUSIC_FONT(\"5\")\"", N_("Insert Thirty Second Note")},
  {KBD_CATEGORY_NOTE_ENTRY,NULL , "Insert sixty-fourth-note",	N_("SixtyfourthNote"), "insert_chord_6key", "\"MUSIC_FONT(\"6\")\"", N_("Insert Sixty Forth Note")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankWholeNote"), "insert_blankchord_0key", N_("Insert Blank Whole Note")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankHalfNote"), "insert_blankchord_1key", N_("Insert Blank Half Note")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankQuarterNote"), "insert_blankchord_2key", N_("Insert Blank Quarter Note")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankEighthNote"), "insert_blankchord_3key", N_("Insert Blank Eigth Note")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankSixteenthNote"), "insert_blankchord_4key", N_("Insert Blank Sixteenth Note")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankThirtysecondNote"), "insert_blankchord_5key", N_("Insert Blank Thirty Second Note")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertBlankSixtyfourthNote"), "insert_blankchord_6key", N_("Insert Blank Sixty Forth Note")},
  {KBD_CATEGORY_EDIT, NULL, "No Tooltip yet",	N_("ToggleRestMode"), "rest_toggle_key", N_("Toggle Rest Mode")},
  {KBD_CATEGORY_EDIT, NULL, "No Tooltip yet",	N_("ToggleBlankMode"), "toggle_blank", N_("Toggle Blank Mode")},

  {KBD_CATEGORY_REST_ENTRY, NULL, "Insert whole-note rest",  N_("InsertWholeRest"), "insert_rest_0key","\"MUSIC_FONT(\"r\")\"", N_("Insert Whole Rest")},
  {KBD_CATEGORY_REST_ENTRY,  NULL, "Insert half-note rest",	N_("InsertHalfRest"), "insert_rest_1key","\"MUSIC_FONT(\"s\")\"", N_("Insert Half Rest")},
  {KBD_CATEGORY_REST_ENTRY,  NULL, "Insert quarter-note rest",	N_("InsertQuarterRest"), "insert_rest_2key","\"MUSIC_FONT(\"t\")\"", N_("Insert Quarter Rest")},
  {KBD_CATEGORY_REST_ENTRY,  NULL, "Insert eighth-note rest",	N_("InsertEighthRest"), "insert_rest_3key","\"MUSIC_FONT(\"u\")\"", N_("Insert Eigth Rest")},
  {KBD_CATEGORY_REST_ENTRY,  NULL, "Insert sixteenth-note rest",	N_("InsertSixteenthRest"), "insert_rest_4key","\"MUSIC_FONT(\"v\")\"", N_("Insert Sixteenth Rest")},
  {KBD_CATEGORY_REST_ENTRY,  NULL, "Insert thirty-second note rest",	N_("InsertThirtysecondRest"), "insert_rest_5key","\"MUSIC_FONT(\"w\")\"", N_("Insert Thirty-second Rest")},
  {KBD_CATEGORY_REST_ENTRY,  NULL, "Insert sixty-fourth note rest",	N_("InsertSixtyfourthRest"), "insert_rest_6key","\"MUSIC_FONT(\"x\")\"", N_("Insert Sixty-forth Rest")},


  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertDuplet"), "insert_duplet", N_("Insert Duplet")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertTriplet"), "insert_triplet", N_("Insert Triplet")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("StartTriplet"), "start_triplet", N_("Start Triplet")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("EndTuplet"), "end_tuplet", N_("End Tuplet")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertQuadtuplet"), "insert_quadtuplet", N_("Insert Quadtuplet")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertQuintuplet"), "insert_quintuplet", N_("Insert Quintuplet")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertSextuplet"), "insert_sextuplet", N_("Insert Sextuplet")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertSeptuplet"), "insert_septuplet", N_("Insert Septuplet")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "Add a note to the current chord\\nThe cursor position determines which note to add",	N_("AddNoteToChord"), "add_tone_key","Add note", N_("Add Tone")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "Remove a note from the current chord",	N_("RemoveNoteFromChord"), "remove_tone_key","Remove note", N_("Remove Tone")},

  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("Sharpen"), "sharpen_key", "Sharpen", N_("Sharpen Note")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("Flatten"), "flatten_key", "Flatten", N_("Flatten Note")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("StemUp"), "stem_up", "StemUp", N_("Stem Up")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("StemDown"), "stem_down", "StemDown", N_("Stem Down")},

  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("AddDot"), "add_dot_key", N_("Add Dot")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("RemoveDot"), "remove_dot_key", N_("Remove Dot")},

  {KBD_CATEGORY_ARTICULATION,  NULL, "Inserts a duplicate of the current note, tied",	N_("InsertTiedNote"), "tie_notes_key","Tied note", N_("Insert Tied Note")},

  {KBD_CATEGORY_EDIT, NULL, "No Tooltip yet",	N_("DeleteObject"), "deleteobject", N_("Delete Object")},
  {KBD_CATEGORY_EDIT, NULL, "No Tooltip yet",	N_("DeletePreviousObject"), "deletepreviousobject", N_("Delete Previous Object")},

  {KBD_CATEGORY_MEASURE, NULL, "No Tooltip yet",	N_("InsertMeasure"), "insert_measure_key", N_("Insert Measure")},
  {KBD_CATEGORY_MEASURE, NULL, "No Tooltip yet",	N_("AppendMeasure"), "append_measure_key", N_("Append Measure")},
  {KBD_CATEGORY_MEASURE, NULL, "Delete the current measure in this staff, leaving the staff short",	N_("DeleteMeasure"), "deletemeasure", N_("Delete Measure")},
  {KBD_CATEGORY_MEASURE, NULL, "Delete the current measure in all staffs",	N_("DeleteMeasureAllStaffs"), "deletemeasureallstaffs", N_("Delete Measure All Staffs")},
  {KBD_CATEGORY_MEASURE, NULL, "No Tooltip yet",	N_("ShrinkMeasures"), "adjust_measure_less_width_key", N_("Shrink Measure")},
  {KBD_CATEGORY_MEASURE, NULL, "No Tooltip yet",	N_("WidenMeasures"), "adjust_measure_more_width_key", N_("Widen Measures")},

  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("ShorterStaffs"), "adjust_staff_less_height_key", N_("Shorter Staffs")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("TallerStaffs"), "adjust_staff_more_height_key", N_("Taller Staffs")},

  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertTrebleClef"), "newcleftreble", N_("New Treble Clef")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertBassClef"), "newclefbass", N_("New Bass Clef")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insertg8clef"), "newclefg8", N_("New G8 Clef")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertAltoClef"), "newclefalto", N_("New Alto Clef")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertTenorClef"), "newcleftenor", N_("New Tenor Clef")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertSopranoClef"), "newclefsoprano", N_("New Soprano Clef")},

  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialTrebleClef"), "setcleftreble", N_("Set Treble Clef")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialBassClef"), "setclefbass", N_("Set Bass Clef")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialg8clef"), "setclefg8", N_("Set G8 Clef")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialAltoClef"), "setclefalto", N_("Set Alto Clef")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialTenorClef"), "setcleftenor", N_("Set Tenor Clef")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialSopranoClef"), "setclefsoprano", N_("Set Soprano Clef")},

  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert22Time"), "newtimesig22", N_("Insert 2/2 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert32Time"), "newtimesig32", N_("Insert 3/2 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert42Time"), "newtimesig42", N_("Insert 4/2 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert44Time"), "newtimesig44", N_("Insert 4/4 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert34Time"), "newtimesig34", N_("Insert 3/4 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert24Time"), "newtimesig24", N_("Insert 2/4 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert64Time"), "newtimesig64", N_("Insert 6/4 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert38Time"), "newtimesig38", N_("Insert 3/8 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert68Time"), "newtimesig68", N_("Insert 6/8 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert128Time"), "newtimesig128", N_("Insert 12/8 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Insert98Time"), "newtimesig98", N_("Insert 9/8 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set22Time"), "settimesig22", N_("Set 2/2 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set32Time"), "settimesig32", N_("Set 3/2 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set42Time"), "settimesig42", N_("Set 4/2 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set44Time"), "settimesig44", N_("Set 4/4 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set34Time"), "settimesig34", N_("Set 3/4 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set24Time"), "settimesig24", N_("Set 2/4 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set64Time"), "settimesig64", N_("Set 6/4 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set38Time"), "settimesig38", N_("Set 3/8 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set68Time"), "settimesig68", N_("Set 6/8 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set128Time"), "settimesig128", N_("Set 12/8 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("Set98Time"), "settimesig98", N_("Set 9/8 Time")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertCmaj"), "newkeysigcmaj", N_("Insert Cmaj")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertGmaj"), "newkeysiggmaj", N_("Insert Gmaj")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertDmaj"), "newkeysigdmaj", N_("Insert Dmaj")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertAmaj"), "newkeysigamaj", N_("Insert Amaj")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertEmaj"), "newkeysigemaj", N_("Insert Emaj")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertBmaj"), "newkeysigbmaj", N_("Insert Bmaj")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertFSharpmaj"), "newkeysigfsharpmaj", N_("Insert F# Major")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertCSharpmaj"), "newkeysigcsharpmaj", N_("Insert C# Major")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertFmaj"), "newkeysigfmaj", N_("Insert F Major")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertBflatmaj"), "newkeysigbflatmaj", N_("Insert Bb Major")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertEflatmaj"), "newkeysigeflatmaj", N_("Insert Eb Major")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertAflatmaj"), "newkeysigaflatmaj", N_("Insert Ab Major")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertDflatmaj"), "newkeysigdflatmaj", N_("Insert Db Major")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertGflatmaj"), "newkeysiggflatmaj", N_("Insert Gb Major")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertCflatmaj"), "newkeysigcflatmaj", N_("Insert Cb Major")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertAmin"), "newkeysigamin", N_("Insert A Minor")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertEmin"), "newkeysigemin", N_("Insert E Minor")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertBmin"), "newkeysigbmin", N_("Insert B Minor")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertFSharpmin"), "newkeysigfsharpmin", N_("Insert F# Minor")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertCSharpmin"), "newkeysigcsharpmin", N_("Insert C# Minor")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertGSharpmin"), "newkeysiggsharpmin", N_("Insert G# Minor")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertDSharpmin"), "newkeysigdsharpmin", N_("Insert D# Minor")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertASharpmin"), "newkeysigasharpmin", N_("Insert A# Minor")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertDmin"), "newkeysigdmin", N_("Insert D Minor")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertGmin"), "newkeysiggmin", N_("Insert G Minor")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertCmin"), "newkeysigcmin", N_("Insert C Minor")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertFmin"), "newkeysigfmin", N_("Insert F Minor")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertBflatmin"), "newkeysigbflatmin", N_("Insert Bb Minor")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertEflatmin"), "newkeysigeflatmin", N_("Insert Eb Minor")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("InsertAflatmin"), "newkeysigaflatmin", N_("Insert Ab Minor")},

  //Functions to Set Initial Key Sig
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialCmaj"), "setkeysigcmaj", N_("Set Initial Keysig to C Major")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialGmaj"), "setkeysiggmaj", N_("Set Initial Keysig to G Major")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialDmaj"), "setkeysigdmaj", N_("Set D Major as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialAmaj"), "setkeysigamaj", N_("Set A Major as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialEmaj"), "setkeysigemaj", N_("Set E Major as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialBmaj"), "setkeysigbmaj", N_("Set B Major as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialFSharpmaj"), "setkeysigfsharpmaj", N_("Set F# Major as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialCSharpmaj"), "setkeysigcsharpmaj", N_("Set C# Major as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialFmaj"), "setkeysigfmaj", N_("Set F Major as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialBflatmaj"), "setkeysigbflatmaj", N_("Set Bb Major as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialEflatmaj"), "setkeysigeflatmaj", N_("Set Eb Major as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialAflatmaj"), "setkeysigaflatmaj", N_("Set Ab Major as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialDflatmaj"), "setkeysigdflatmaj", N_("Set Db Major as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialGflatmaj"), "setkeysiggflatmaj", N_("Set Gb Major as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialCflatmaj"), "setkeysigcflatmaj", N_("Set Cb Major as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialAmin"), "setkeysigamin", N_("Set A Minor as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialEmin"), "setkeysigemin", N_("Set E Minor as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialBmin"), "setkeysigbmin", N_("Set B Minor as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialFSharpmin"), "setkeysigfsharpmin", N_("Set F# Minor as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialCSharpmin"), "setkeysigcsharpmin", N_("Set C# Minor as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialGSharpmin"), "setkeysiggsharpmin", N_("Set G# Minor as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialDSharpmin"), "setkeysigdsharpmin", N_("Set D# Minor as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialASharpmin"), "setkeysigasharpmin", N_("Set A# Minor as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialDmin"), "setkeysigdmin", N_("Set D Minor as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialGmin"), "setkeysiggmin", N_("Set G Minor as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialCmin"), "setkeysigcmin", N_("Set C Minor as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialFmin"), "setkeysigfmin", N_("Set F Minor as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialBflatmin"), "setkeysigbflatmin", N_("Set Bb Minor as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialEflatmin"), "setkeysigeflatmin", N_("Set Eb Minor as Initial Keysig")},
  {KBD_CATEGORY_STAFF, NULL, "No Tooltip yet",	N_("SetInitialAflatmin"), "setkeysigaflatmin", N_("Set Ab Minor as Initial Keysig")},


  {KBD_CATEGORY_OTHER, NULL, "No Tooltip yet",	N_("SetMark"), "set_mark", N_("Set Mark")},
  {KBD_CATEGORY_OTHER, NULL, "No Tooltip yet",	N_("UnsetMark"), "unset_mark", N_("Unset Mark")},

  {KBD_CATEGORY_ARTICULATION, NULL, "Insert/delete begin slur on this note",	N_("ToggleBeginSlur"), "toggle_begin_slur", N_("Begin Slur")},
  {KBD_CATEGORY_ARTICULATION, NULL, "Insert/delete end slur on this note",	N_("ToggleEndSlur"), "toggle_end_slur", N_("End Slur")},

  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleStartCrescendo"), "toggle_start_crescendo", N_("Start Crescendo")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleEndCrescendo"), "toggle_end_crescendo", N_("End Crescendo")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleStartDiminuendo"), "toggle_start_diminuendo", N_("Start Diminuendo")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleEndDiminuendo"), "toggle_end_diminuendo", N_("End Diminuendo")},

  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleAccent"), "add_accent", N_("Add Accent")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleFermata"), "add_fermata", N_("Add Fermata")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleStaccato"), "add_staccato", N_("Add Staccato")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleTenuto"), "add_tenuto", N_("Add Tenuto")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleTrill"), "add_trill", N_("Add Trill")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleTurn"), "add_turn", N_("Add Turn")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleMordent"), "add_mordent", N_("Add Mordent")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleStaccatissimo"), "add_staccatissimo", N_("Add Staccatissimo")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleCoda"), "add_coda", N_("Add Coda")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleFlageolet"), "add_flageolet", N_("Add Flageolet")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleOpen"), "add_open", N_("Add Open")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("TogglePrallMordent"), "add_prallmordent", N_("Add Prall Mordent")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("TogglePrallPrall"), "add_prallprall", N_("Add Prall Prall")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("TogglePrall"), "add_prall", N_("Add Prall")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleReverseTurn"), "add_reverseturn", N_("Add Reverse Turn")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleSegno"), "add_segno", N_("Toggle Segno")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleSforzato"), "add_sforzato", N_("Toggle Sforzato")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleStopped"), "add_stopped", N_("Toggle Stopped")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleThumb"), "add_thumb", N_("Toggle Thumb")},
  /*{KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleTrillElement"), "add_trillelement", N_("Toggle Trill")},
     {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleTrill_Element"), "add_trill_element", N_("Add Trill")}, */
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleUpprall"), "add_upprall", N_("Toggle Up Prall")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ToggleArpeggio"), "add_arpeggio", N_("Add Apreggio")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("SetGrace"), "set_grace", N_("Set Grace")},

  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("ForceCaution"), "force_cautionary", N_("Force Cautionary Accidental")},

  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("ChangePitch"), "change_pitch", N_("Change Pitch")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("DoubleBar"), "insert_doublebar", N_("Insert Double Bar")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("EndBar"), "insert_endbar", N_("Insert Endbar")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("OpenRepeat"), "insert_openrepeat", N_("Insert Open Repeat")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("CloseRepeat"), "insert_closerepeat", N_("Insert Close Repeat")},
  {KBD_CATEGORY_ARTICULATION, NULL, "No Tooltip yet",	N_("OpenCloseRepeat"), "insert_opencloserepeat", N_("Insert Open Close Repeat")},
  {KBD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet",	N_("InsertRhythm"), "insert_rhythm_pattern", N_("Insert Rhythm")},
  {KBD_CATEGORY_OTHER, NULL, "Make next rhythm pattern\\nthe prevailing rhythm.\\nNotes entered will follow this pattern",	N_("NextRhythm"), "nextrhythm", N_("Next Rhythm")},
  {KBD_CATEGORY_MEASURE, NULL, "No Tooltip yet",	N_("AppendMeasuresToScore"), "append_measure_score", N_("Append Measure To Score")},
  
  /* from view.c menu_entries[]  */
  {KBD_CATEGORY_DIRECT, NULL, "Creating, saving, loading, displaying and printing musical scores", N_("FileMenu"), NULL, "File", N_("File Menu")},
  {KBD_CATEGORY_DIRECT, NULL, "Creating, saving places in musical scores", N_("Bookmarks"), NULL, "Bookmarks", N_("Bookmarks")},
  {KBD_CATEGORY_DIRECT, NULL, "Different keyboard entry modes", N_("EntryMenu"), NULL, "Mode", N_("Entry Menu")},  {KBD_CATEGORY_DIRECT, NULL, "General editing commands", N_("EditMenu"), NULL, "Edit", N_("Edit Menu")}, 
 {KBD_CATEGORY_DIRECT, NULL, "Control which tools are to be shown", N_("ViewMenu"), NULL, "View", N_("View Menu")}, 

  {KBD_CATEGORY_DIRECT, NULL, "Staffs and voices", N_("StaffMenu"), NULL,"Staffs/Voices", N_("Staff Menu")},
  {KBD_CATEGORY_DIRECT, NULL, "Movements in a score", N_("MovementMenu"), NULL,"Movements", N_("Movement Menu")},
  {KBD_CATEGORY_DIRECT, NULL, "Help with denemo", N_("HelpMenu"), NULL, "Help", N_("Help Menu")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Playing the music through midi file", N_("PlaybackMenu"), NULL, "Playback", N_("Playback Menu")}, 



  {KBD_CATEGORY_DIRECT, "GTK_STOCK_NEW", "Start a new musical score", N_("New"), "file_newwrapper", N_("New File")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Open a file containing a music score for editing", N_("Open"), "file_open_with_check", N_("Open")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Add staffs from a Denemo file", N_("AddStaffs"), "file_add_staffs", N_("Add Staffs")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Add movements from a Denemo file", N_("AddMovements"), "file_add_movements", N_("Add Movement")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PROPERTIES", "Change properties of this movement", N_("MovementProps"), "movement_props_dialog", N_("Change Properties")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Open a file containing a music score for editing in a separate working area (tab", N_("OpenNewWindow"), "openinnew", N_("Open In New")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_SAVE", "Save the score", N_("Save"), "file_savewrapper", N_("Save")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_SAVE_AS", "Save the score under a new name", N_("SaveAs"), "file_saveaswrapper", N_("Save As")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Start a new score from a built-in template file", N_("OpenTemplate"), "system_template_open_with_check", N_("Open Template")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Start a new score from a built-in example", N_("OpenExample"), "system_example_open_with_check", N_("Open Example")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Start a new score from one of your own template files", N_("OpenMyTemplate"), "local_template_open_with_check", N_("")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_SAVE_AS", "Save the score as a template for re-use as a starting point for new scores", N_("SaveTemplate"), "template_save", N_("Save Template")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Create working area (tab with an empty score in it)", N_("NewWindow"), "newview", N_("New Tab")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert a new movement before the current one", N_("InsertMovementBefore"), "insert_movement_before", N_("Insert Movement Before")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert a new movement after the current one", N_("InsertMovementAfter"), "insert_movement_after", N_("Insert Movement After")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_SAVE_AS", "Save Parts: each staff becomes a file in lilypond format", N_("SaveParts"), "file_savepartswrapper", N_("Save Parts")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_SAVE_AS", "Export the score as a PDF document file", N_("ExportPDF"), "export_pdf_action", N_("Export PDF")},

  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PROPERTIES", "Start up a wizard to create a new score. This allows you to set various properties of the score", N_("ConfigureScore"), "scorewizard", N_("Score Wizard")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PRINT_PREVIEW", "Displays the final finished score in your pdf viewer", N_("PrintPreview"), "printpreview_cb", N_("Print Preview")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PRINT_PREVIEW", "Displays a musical excerpt in your image viewer", N_("PrintExcerptPreview"), "printexcerptpreview_cb", N_("Print Excerpt")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PRINT", "Displays the final finished score in a pdf viewer. From this you can print the file using the print command of the viewer", N_("Print"), "printall_cb", N_("Print")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PRINT", "Displays the final finished score for the current part (that is current staff", N_("PrintPart"), "printpart_cb", N_("Print Part")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_CLOSE", "Close the current score. Other windows will stay open", N_("Close"), "close_gui_with_check", N_("Close Score")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_QUIT", "Quit the Denemo program", N_("Quit"), "closewrapper", N_("Quit")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_UNDO", "Undo", N_("Undo"), "undowrapper", N_("Undo")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_REDO", "Redo", N_("Redo"), "redowrapper", N_("Redo")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Selecting stretches of notes", N_("Select"), NULL, N_("Select")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Extend the selection", N_("ExtendSelect"), NULL, N_("Extend Selection")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_COPY", "Copy", N_("Copy"), "copywrapper", N_("Copy")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_CUT", "Cut", N_("Cut"), "cutwrapper", N_("Cut")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PASTE", "Paste the selected music", N_("Paste"), "pastewrapper", N_("Paste")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PROPERTIES", "Change some of the properties of the current score. This will start up a dialog window", N_("ScoreProperties"), "score_properties_dialog", N_("Score Properties")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Save the selected music. Not sure if this is working", N_("SaveSelection"), "saveselwrapper", N_("Save Selection")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PREFERENCES", "Set and save your preferences for how Denemo operates on startup. Edit .denemo/denemorc for missing ones", N_("Preferences"), "preferences_change", N_("Change Preferences")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Set actions to take in response to keypresses", N_("KeyBindings"), NULL, "Customize Commands, Shortcuts ...", N_("Customize Key Bindings")},
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_SAVE", "Save the current commands and keyboard shortcuts as the default", N_("SaveAccels"), "save_default_keymap_file_wrapper", "Save Command Set", N_("Save Command Set")}, 
  {KBD_CATEGORY_DIRECT, NULL, "View help, change and save keyboard shortcuts", "CommandManagement", "configure_keyboard_dialog", "Manage Command Set", N_("Manage Command Set")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Load Plugins", N_("LoadPlugins"), "load_plugin", N_("Load Plugins")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Unload Plugins", N_("UnloadPlugins"), "unloadplugins", N_("Unload Plugins")}, 
  {KBD_CATEGORY_DIRECT, NULL, "List the loaded plugins", N_("ListPlugins"), "list_loaded_plugins", N_("List Plugins")}, 
  {KBD_CATEGORY_DIRECT, NULL, "List the available plugins", N_("ListAvailablePlugins"), "list_available_plugins", N_("List Available Pluigins")}, 

  {KBD_CATEGORY_DIRECT, NULL, "Swap this staff with the one higher up. Note this actually swaps voices.", N_("SwapStaffs"), "swapstaffs", N_("Swap Staffs")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Split off the next voice as a separate staff", N_("SplitVoices"), "splitstaffs", N_("Split Voices")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Merge this staff as a voice on the previous staff", N_("JoinVoices"), "joinstaffs", N_("Join Voices")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Swap this movement with the one before", N_("SwapMovements"), "swapmovements", N_("Swap Movements")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Go to the higher numbered voice (or staff if highest voice number on staff", N_("VoiceUp"),
    "voiceup_cb", N_("Voice Up")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Go to the lower numbered voice on this staff", N_("VoiceDown"), "voicedown_cb", N_("Voice Down")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserts a new staff before the current staff", N_("AddBefore"), "newstaffbefore", N_("Add Staff Before")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserts/Adds a new staff after the current staff", N_("AddAfter"), "dnm_newstaffafter", N_("Add Staff After")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserts a new staff at the top of the score", N_("AddInitial"), "newstaffinitial", N_("Add Initial Staff")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserts a new staff at the end of the score", N_("AddLast"), "newstafflast", N_("Add Last Staff")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Deletes the staff before the current staff", N_("DeleteBefore"), "delete_staff_before", N_("Delete Staff Before")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Deletes the current staff", N_("DeleteStaff"), "delete_staff_current", N_("Delete Current Staff")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Deletes the staff after the current staff", N_("DeleteAfter"), "delete_staff_after", N_("Delete Staff After")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Adds a new voice (part), to the current staff. It is tricky to switch between the voices. Suggest to use merge staffs", "AddVoice", "dnm_newstaffvoice", N_("Add Voice")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Transpose the current staff", N_("TransposeStaff"), "staff_transposition", N_("Traspose Staff")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PROPERTIES", "Change the properties of the current staff", N_("StaffProperties"), "staff_properties_change_cb", N_("Staff Properties")},
  {KBD_CATEGORY_DIRECT, NULL, "Insert", N_("InsertMenu"), NULL},  
  {KBD_CATEGORY_DIRECT, NULL, "Clef", N_("Clef"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Change the initial clef of the current staff", N_("InitialClef"), "clef_change_initial", N_("Initial Clef")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert a change of clef at the cursor", N_("InsertClef"), "clef_change_insert", N_("Clef Change")}, 


  {KBD_CATEGORY_DIRECT, NULL, "insert change key signature or set the initial key", N_("Key"), NULL, N_("Keysig")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Set the initial key signature of the current staff", N_("InitialKey"), "key_change_initial", N_("Initial Key")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert a key change at the cursor position", N_("InsertKey"), "key_change_insert", N_("Insert Key Change")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Manage the time signature changes and initial value", N_("TimeSig"), NULL}, 
  {KBD_CATEGORY_DIRECT, NULL, "Set the initial time signature of the current staff", N_("InitialTimeSig"), "timesig_change_initial", N_("Inital Time Signature")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Edit/Insert a time signature change for the current measure", N_("InsertTimeSig"), "timesig_change_insert", N_("Insert Time Signature")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Change the type of notehead for the current note", N_("ChangeNotehead"), "set_notehead", N_("Set Notehead")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserts a stem neutral tag. Click on this tag and use Sharpen/StemUp etc commands to change stem direction", N_("InsertStem"), "stem_directive_insert", N_("Stem Directive")},
  {KBD_CATEGORY_DIRECT, NULL, "Add a lyric to current note. Beware: all previous notes must have lyrics for printing correctly", "EditLyric", "lyric_insert", "Insert/Edit Lyric", N_("Edit Lyrics")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Add a bass figure to the current note. Use | sign to split the duration of a note so as to have multiple figures on one note. See Lilypond docs for other notation", N_("EditFiguredBass"), "figure_insert", "Insert/Edit Figured Bass", N_("Edit Figured Bass")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Allows chord symbols to be added to the current note. E.G.cis:dim7 for c-sharp diminished 7th. See Lilypond docs for notation", N_("EditChords"), "fakechord_insert", N_("Edit Chords")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserts a dynamic marking at the cursor position", N_("InsertDynamic"), "insert_dynamic", N_("Insert Dynamics")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert or edit a directive in the LilyPond music typesetting language. This can be used for extra spacing, transposing or almost anything. See LilyPond documentation for ideas.", N_("InsertLilyDirective"), "lily_directive_insert", N_("Insert Lilypond")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert or edit a LilyPond text to be post-fixed to the current note. This can be used for guitar fingerings, cautionary accidentals and much more. See LilyPond documentation.",N_("InsertLilyPostfix"), "lily_directive_postfix", N_("Postfix Lilypond")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserts specialized barline at the cursor position. Mostly not working", N_("InsertBarline"), "insert_barline", N_("Insert Barline")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Getting around the score", N_("NavigationMenu"), NULL, "Navigation", N_("Navigation Menu")},
  {KBD_CATEGORY_DIRECT, NULL, "Opens a dialog for going to a numbered measure", N_("GoToMeasure"), "tomeasurenum", "Go to Measure", N_("Go To Measure")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_GOTO_FIRST", "Go To Beginning", N_("GoToBeginning"), "tohome","Go To Beginning", N_("Go To Beginning")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_GOTO_LAST", "Go To End", N_("GoToEnd"), "toend",  "Go To End", N_("Go To End")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Go to the next movement", N_("NextMovement"), "next_movement", N_("Next Movement")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Go to the previous movement", N_("PreviousMovement"), "prev_movement", N_("Previous Movement")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Delete the current movement", N_("DeleteMovement"), "delete_movement", N_("Delete Movement")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Delete all bookmarks in current movement", N_("DeleteBookmarks"), "deletebookmarks", N_("Delete Bookmarks")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_MEDIA_PLAY", "Play", N_("Play"), "ext_midi_playback", N_("Play")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_MEDIA_STOP", "Stop", N_("Stop"), "stop_midi_playback", N_("Stop")}, 

  {KBD_CATEGORY_DIRECT, "GTK_STOCK_MEDIA_PLAY", "Play using CSound...", N_("PlayCSound"), "csoundplayback", N_("Csound Playback")}, 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_PROPERTIES", "Allows you to specify properties used in playing back (midi and csound", N_("PlaybackProperties"), "playback_properties_change", "Playback Properties", N_("Playback Properties")}, 

  {KBD_CATEGORY_DIRECT, NULL, "Opens a browser on the user manual", N_("Help"), "browse_manual", N_("Browse Manual")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Gives the version number etc of this program", N_("About"), "about", N_("About")}, 


  {KBD_CATEGORY_DIRECT, NULL, "Allows choosing extra commands/menu items from disk", N_("MoreMenu"), NULL, N_("More")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Allows choosing standard extra commands/menu items", N_("MoreCommands"), "morecommands", N_("More Commands")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Allows choosing extra commands/menu items from your own collection of extras", N_("MyCommands"), "mycommands", N_("My Commands")}, 



  {KBD_CATEGORY_DIRECT, NULL, "Bookmark the current cursor position", N_("AddBookmark"), "addbookmark", "Add Bookmark", N_("Add Bookmarks")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Go to a bookmarked point in the score", N_("GotoBookmark"), "gotobookmark", N_("Goto Bookmarks")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Go to the next bookmarked point in the list", N_("NextBookmark"), "nextbookmark", N_("Next Bookmark")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Go to the previous bookmarked point in the list", N_("PrevBookmark"), "prevbookmark", N_("Prev Bookmark")}, 
 
  {KBD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Open previously used files", N_("OpenRecent"), NULL, N_("Open Recent")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Toggle between current mode and edit mode", N_("ToggleEdit"), "toggle_edit_mode", N_("Toggle Edit Mode")},
  {KBD_CATEGORY_DIRECT, NULL, "Toggle between note entry and rest entry", N_("ToggleRest"),  "toggle_rest_mode", N_("Toggle Rest Mode")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Toggle between note entry and rhythm entry", N_("ToggleRhythm"),  "toggle_rhythm_mode", N_("Toggle Rhythm Mode")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Clear the list of pitches that overlay the notes", N_("ClearOverlay"),  "clear_overlay", N_("Clear Overlay")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Copy selection as a rhythm pattern for notes to follow as they are entered", N_("CreateRhythm"), "create_rhythm_cb", N_("Create Rhythm")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Delete the selected rhythm pattern", N_("DeleteRhythm"), "delete_rhythm_cb", N_("Delete Rhythm")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Moving the cursor and inserting notes or rests there", N_("ClassicModeNote"), NULL, N_("Classic Mode")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Moving the cursor to the nearest ...", N_("SelectNote"), NULL, N_("Select Note")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Actions for notes: inserting, deleting, etc.", N_("InsertModeNote"), NULL, N_("Insert")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Actions to control the stem up/down", N_("StemControl"), NULL, N_("Stem Direction")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Different types of notehead", N_("NoteheadControl"), NULL, N_("Notehead Types")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Creating Tied Notes", N_("TiedNotes"), NULL, N_("Tied Notes")}, 



  {KBD_CATEGORY_DIRECT, NULL, "Inserting the note ...", N_("InsertNote"), NULL, "Note Entry" , N_("Note Entry")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Anything not previously covered", N_("AllOther"), NULL, N_("Other")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Moving around the piece", N_("Navigation"), NULL, N_("Navigation")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Entering notes", N_("NoteEntry"), NULL, N_("Note Entry")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Various expressive marks", N_("Articulation"), NULL, N_("Articulation")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Editing", N_("Edit"), NULL, N_("Edit")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Manipulating measures", N_("Measure"), NULL, N_("Measure")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Commands for staffs", N_("Staff"), NULL, N_("Staff")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Playing the music through midi file", N_("Playback"), NULL, N_("Playback")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Changing the prevailing duration or rhythm pattern", N_("SelectDuration"), NULL, N_("Select Duration")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Appending, Changing, and deleting notes", N_("EditModeNote"), NULL, N_("Edit")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Changing the note at the cursor to the nearest ...", N_("EditNote"), NULL, N_("Edit Note")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Changing the duration of note at the cursor or appending a note of the given duration", N_("EditDuration"), NULL, N_("Edit Duration")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Moving the cursor", N_("Cursor"), NULL, N_("Cursor")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert/change clef Set initial clef", N_("ClefMenu"), NULL, N_("Clef Menu")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Adding notes to make chords", N_("ChordMenu"), NULL, "Chords", N_("Chords")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Measures: adding, deleting, navigating etc", N_("MeasureMenu"), NULL, N_("Measure Menu")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserting notes, measures, staffs, keysigs etc", N_("Insert"), NULL, N_("Insert")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert a Staff relative to current staff", N_("InsertStaff"), NULL, N_("Insert Staff")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Insert a Movement relative to current movement", N_("InsertMovement"), NULL, N_("Insert Movement")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Inserting notes of a given duration", N_("InsertDuration"), NULL, N_("Insert Duration")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Changing properties of notes, measures, staffs, keysigs etc", N_("Change"), NULL, N_("Change")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Modeless actions on notes/rests", N_("ModelessNote"), NULL, N_("Notes/Durations")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Actions for notes/rests", N_("NotesRests"), NULL, N_("Notes/Rests")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Modeless entry of rests", N_("RestEntry"), NULL, N_("Rest Entry")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Changing the note at the cursor to the nearest ...", N_("ChangeNote"), NULL, N_("Change Note")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Changes the duration of the current note", N_("ChangeDuration"), NULL, N_("Change Duration")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Changes the duration of the current rest", N_("ChangeRest"), NULL, N_("Change Rest")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Dynamics, staccato, slurs, ties and other expressive marks", N_("ExpressionMarks"), NULL, "Expression Marks", N_("Expression Marks")}, 
  {KBD_CATEGORY_DIRECT, NULL, "grace notes etc", N_("Ornaments"), NULL, N_("Ornaments")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Lyrics, chord symbols, figured basses etc", N_("Other"), NULL, N_("Other")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Less used actions", N_("Others"), NULL, N_("Others")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Customized LilyPond inserts. Store often-used inserts here labelled with what they do", N_("Favorites"), NULL, N_("Favorites")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Add a custom LilyPond insert to favorites menu", N_("AddFavorite"), NULL, N_("Add Favorite")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Entering triplets and other tuplets", N_("Tuplets"), NULL, N_("Tuplets")}, 
  {KBD_CATEGORY_DIRECT, NULL, "Deleting notes, measures staffs keysigs etc", N_("Delete"), NULL, N_("Delete")}, 

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
