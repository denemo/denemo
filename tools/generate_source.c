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

#define NOTECHAR0 "\\xF0\\x9D\\x85\\x9D"
#define NOTECHAR1 "\\xF0\\x9D\\x85\\x9E"
#define NOTECHAR2 "\\xF0\\x9D\\x85\\x9F"
#define NOTECHAR3 "\\xF0\\x9D\\x85\\xA0"
#define NOTECHAR4 "\\xF0\\x9D\\x85\\xA1"
#define NOTECHAR5 "\\xF0\\x9D\\x85\\xA2"
#define NOTECHAR6 "\\xF0\\x9D\\x85\\xA3"
#define NOTECHAR7 "\\xF0\\x9D\\x85\\xA4"
#define NOTECHAR8 "\\xF0\\x9D\\x85\\xA5"


#define RESTCHAR0 "\\xF0\\x9D\\x84\\xBB"
#define RESTCHAR1 "\\xF0\\x9D\\x84\\xBC"
#define RESTCHAR2 "\\xF0\\x9D\\x84\\xBD"
#define RESTCHAR3 "\\xF0\\x9D\\x84\\xBE"
#define RESTCHAR4 "\\xF0\\x9D\\x84\\xBF"
#define RESTCHAR5 "\\xF0\\x9D\\x85\\x80"
#define RESTCHAR6 "\\xF0\\x9D\\x85\\x81"
#define RESTCHAR7 "\\xF0\\x9D\\x85\\x82"
#define RESTCHAR8 "\\xF0\\x9D\\x85\\x83"

#define REST0 "<span font_desc=\\\"Denemo\\\">"RESTCHAR0"</span>"
#define REST1 "<span font_desc=\\\"Denemo\\\">"RESTCHAR1"</span>"
#define REST2 "<span font_desc=\\\"Denemo\\\">"RESTCHAR2"</span>"
#define REST3 "<span font_desc=\\\"Denemo\\\">"RESTCHAR3"</span>"
#define REST4 "<span font_desc=\\\"Denemo\\\">"RESTCHAR4"</span>"
#define REST5 "<span font_desc=\\\"Denemo\\\">"RESTCHAR5"</span>"
#define REST6 "<span font_desc=\\\"Denemo\\\">"RESTCHAR6"</span>"
#define REST7 "<span font_desc=\\\"Denemo\\\">"RESTCHAR7"</span>"
#define REST8 "<span font_desc=\\\"Denemo\\\">"RESTCHAR8"</span>"


//char *NOTES[] = {NOTE0, NOTE1, NOTE2, NOTE3, NOTE4, NOTE5, NOTE6, NOTE7, NOTE8};
char *NOTECHARS[] = { NOTECHAR0, NOTECHAR1, NOTECHAR2, NOTECHAR3, NOTECHAR4, NOTECHAR5, NOTECHAR6, NOTECHAR7, NOTECHAR8 };

//char *RESTS[] = {REST0, REST1, REST2, REST3, REST4, REST5, REST6, REST7, REST8};
char *RESTCHARS[] = { RESTCHAR0, RESTCHAR1, RESTCHAR2, RESTCHAR3, RESTCHAR4, RESTCHAR5, RESTCHAR6, RESTCHAR7, RESTCHAR8 };



#define N_

#define CMD_CATEGORY_NAVIGATION		0
#define CMD_CATEGORY_NOTE_ENTRY		1
#define CMD_CATEGORY_REST_ENTRY		2
#define CMD_CATEGORY_ARTICULATION	3
#define CMD_CATEGORY_EDIT		4
#define CMD_CATEGORY_MEASURE		5
#define CMD_CATEGORY_STAFF		6
#define CMD_CATEGORY_PLAYBACK		7
#define CMD_CATEGORY_OTHER		8

#define CMD_CLASS(cat) (cat&0xFF)
#define CMD_CATEGORY_DIRECT		0x100   /* does not require a wrapper to the callback */
#define CMD_CATEGORY_BOOLEAN		0x200   /* call returns boolean */



char *catname[9] = { N_("Navigation"),
  N_("Note entry"),
  N_("Rest entry"),
  N_("Articulation"),
  N_("Edit"),
  N_("Measure"),
  N_("Staff"),
  N_("Playback"),
  N_("Other")
};







struct name_and_function
{
  unsigned category;
  /** Command name */
  char *icon;
  //char *menu_label;
  char *tooltip;
  char *name;
  char *function;
  char *menu_label;
  char *initial_setting;        /*of radio/check items */
};


struct name_and_function denemo_commands[] = {

  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Moves the cursor one object left, altering the selection if any", N_("CursorLeft"), "cursorleft", N_("Cursor Left")},
  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Moves the cursor one object left, without altering the selection", N_("MoveCursorLeft"), "movecursorleft", N_("Move Cursor Left")},
  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Moves the cursor one scale step down", N_("CursorDown"), "cursordown", N_("Cursor Down")},
  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Moves the cursor one scale step up", N_("CursorUp"), "cursorup", N_("Cursor Up")},
  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Moves the cursor one object right, altering the selection if any", N_("CursorRight"), "cursorright", N_("Cursor Right")},
  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Moves the cursor one object right, without altering the selection", N_("MoveCursorRight"), "movecursorright", N_("Move Cursor Right")},

  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_DIRECT, NULL, "Moves the cursor to the Mark without altering the selection", N_("GoToMark"), "goto_mark", N_("To Mark")},

  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_DIRECT, NULL, "Swaps the active end of the selection", N_("SwapPointAndMark"), "swap_point_and_mark", N_("Swap Ends of Selection")},



  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_DIRECT, NULL, "Moves the cursor to the first object in the selection without altering the selection. returns #f if no selection", N_("GoToSelectionStart"), "goto_selection_start", N_("To Selection Start")},

  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_DIRECT, NULL, "Pushes the current cursor position onto a stack", N_("PushPosition"), "PushPosition", N_("Push Position")},
  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_DIRECT, NULL, "Pops a position from the stack of cursor positions, moving the cursor there", N_("PopPosition"), "PopPosition", N_("Pop Position")},
  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_DIRECT, NULL, "Pops a position from the stack of cursor positions, pushes the current position, then moves the cursor to the popped position", N_("PopPushPosition"), "PopPushPosition", N_("Pop and Push Position")},


  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_DIRECT, NULL, "Hides/Shows menus, panes etc. The ones shown are those checked in the view menu.", N_("ToggleReduceToDrawingArea"), "ToggleReduceToDrawingArea", N_("Hide/Show Menus")},


  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Moves the cursor to the staff above, extending selection if any", N_("StaffUp"), "staffup", N_("Staff Up")},
  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Moves the cursor to the staff below, extending selection if any", N_("StaffDown"), "staffdown", N_("Staff Down")},

  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Moves the cursor to the staff above without altering selection", N_("MoveToStaffUp"), "movetostaffup", N_("Move to Staff Up")},
  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Moves the cursor to the staff below  without altering selection", N_("MoveToStaffDown"), "movetostaffdown", N_("Move to Staff Down")},



  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Moves the cursor to the first object in the next measure, extending selection if any", N_("MeasureLeft"), "measureleft", N_("Measure Left")},
  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Moves the cursor to the first object in the previous measure, extending selection if any", N_("MeasureRight"), "measureright", N_("Measure Right")},

  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Moves the cursor to the first object in the next measure leaving selection, if any, unchanged", N_("MoveToMeasureLeft"), "movetomeasureleft", N_("Move to Measure Left")},
  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Moves the cursor to the first object in the previous measure leaving selection, if any, unchanged", N_("MoveToMeasureRight"), "movetomeasureright", N_("Move to Measure Right")},



  {CMD_CATEGORY_NAVIGATION, NULL, "Changes the note the cursor is on to the note A.\\nIf the cursor is in the appending position, appends a note using the prevailing duration.", N_("A"), "go_to_A_key", N_("Change/Append A")},
  {CMD_CATEGORY_NAVIGATION, NULL, "Changes the note the cursor is on to the note B.\\nIf the cursor is in the appending position, appends a note using the prevailing duration.", N_("B"), "go_to_B_key", N_("Change/Append B")},
  {CMD_CATEGORY_NAVIGATION, NULL, "Changes the note the cursor is on to the note C.\\nIf the cursor is in the appending position, appends a note using the prevailing duration.", N_("C"), "go_to_C_key", N_("Change/Append C")},
  {CMD_CATEGORY_NAVIGATION, NULL, "Changes the note the cursor is on to the note D.\\nIf the cursor is in the appending position, appends a note using the prevailing duration.", N_("D"), "go_to_D_key", N_("Change/Append D")},
  {CMD_CATEGORY_NAVIGATION, NULL, "Changes the note the cursor is on to the note E.\\nIf the cursor is in the appending position, appends a note using the prevailing duration.", N_("E"), "go_to_E_key", N_("Change/Append E")},
  {CMD_CATEGORY_NAVIGATION, NULL, "Changes the note the cursor is on to the note F.\\nIf the cursor is in the appending position, appends a note using the prevailing duration.", N_("F"), "go_to_F_key", N_("Change/Append F")},
  {CMD_CATEGORY_NAVIGATION, NULL, "Changes the note the cursor is on to the note G.\\nIf the cursor is in the appending position, appends a note using the prevailing duration.", N_("G"), "go_to_G_key", N_("Change/Append G")},
  {CMD_CATEGORY_NAVIGATION, NULL, "Changes the note at the cursor to an octave higher", N_("OctaveUp"), "octave_up_key", N_("Octave Up")},
  {CMD_CATEGORY_NAVIGATION, NULL, "Changes the note at the cursor to an octave lower", N_("OctaveDown"), "octave_down_key", N_("Octave Down")},

  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert " NOTECHAR0, N_("WholeNote"), "insert_chord_0key", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert " NOTECHAR1, N_("HalfNote"), "insert_chord_1key", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert " NOTECHAR2, N_("QuarterNote"), "insert_chord_2key", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert " NOTECHAR3, N_("EighthNote"), "insert_chord_3key", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert " NOTECHAR4, N_("SixteenthNote"), "insert_chord_4key", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert " NOTECHAR5, N_("ThirtysecondNote"), "insert_chord_5key", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert " NOTECHAR6, N_("SixtyfourthNote"), "insert_chord_6key", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert " NOTECHAR7, N_("OneHundredTwentyEighthNote"), "insert_chord_7key", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert " NOTECHAR8, N_("TwoHundredFiftySixthNote"), "insert_chord_8key", NULL},

  {CMD_CATEGORY_REST_ENTRY, NULL, "Insert " RESTCHAR0 " rest", N_("InsertWholeRest"), "insert_rest_0key", "Insert a " REST0, NULL},
  {CMD_CATEGORY_REST_ENTRY, NULL, "Insert " RESTCHAR1 " rest", N_("InsertHalfRest"), "insert_rest_1key", "Insert a " REST1, NULL},
  {CMD_CATEGORY_REST_ENTRY, NULL, "Insert " RESTCHAR2 " rest", N_("InsertQuarterRest"), "insert_rest_2key", "Insert a " REST2, NULL},
  {CMD_CATEGORY_REST_ENTRY, NULL, "Insert " RESTCHAR3 " rest", N_("InsertEighthRest"), "insert_rest_3key", "Insert a " REST3, NULL},
  {CMD_CATEGORY_REST_ENTRY, NULL, "Insert " RESTCHAR4 " rest", N_("InsertSixteenthRest"), "insert_rest_4key", "Insert a " REST4, NULL},
  {CMD_CATEGORY_REST_ENTRY, NULL, "Insert " RESTCHAR5 " rest", N_("InsertThirtysecondRest"), "insert_rest_5key", "Insert a " REST5, NULL},
  {CMD_CATEGORY_REST_ENTRY, NULL, "Insert " RESTCHAR6 " rest", N_("InsertSixtyfourthRest"), "insert_rest_6key", "Insert a " REST6, NULL},

  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert a non-printing " RESTCHAR0 " rest", N_("InsertBlankWholeNote"), "insert_blankchord_0key", "Insert a " RESTCHAR0 " Spacer", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert a non-printing " RESTCHAR1 " rest", N_("InsertBlankHalfNote"), "insert_blankchord_1key", "Insert a " RESTCHAR1 " Spacer", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert a non-printing " RESTCHAR2 " rest", N_("InsertBlankQuarterNote"), "insert_blankchord_2key", "Insert a " RESTCHAR2 " Spacer", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert a non-printing " RESTCHAR3 " rest", N_("InsertBlankEighthNote"), "insert_blankchord_3key", "Insert a " RESTCHAR3 " Spacer", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert a non-printing " RESTCHAR4 " rest", N_("InsertBlankSixteenthNote"), "insert_blankchord_4key", "Insert a " RESTCHAR4 " Spacer", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert a non-printing " RESTCHAR5 " rest", N_("InsertBlankThirtysecondNote"), "insert_blankchord_5key", "Insert a " RESTCHAR5 " Spacer", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert a non-printing " RESTCHAR6 " rest", N_("InsertBlankSixtyfourthNote"), "insert_blankchord_6key", "Insert a " RESTCHAR6 " Spacer", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert a non-printing " RESTCHAR7 " rest", N_("InsertBlankOneHundredTwentyEighthNote"), "insert_blankchord_7key", "Insert a " RESTCHAR7 " Spacer", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Insert a non-printing " RESTCHAR8 " rest", N_("InsertBlankTwoHundredFiftySixthNote"), "insert_blankchord_8key", "Insert a " RESTCHAR8 " Spacer", NULL},




  {CMD_CATEGORY_EDIT, NULL, "No Tooltip yet", N_("ToggleRestMode"), "rest_toggle_key", N_("Toggle Rest Mode")},
  {CMD_CATEGORY_EDIT, NULL, "No Tooltip yet", N_("ToggleBlankMode"), "toggle_blank", N_("Toggle Blank Mode")},



  {CMD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet", N_("InsertDuplet"), "insert_duplet", N_("Insert Duplet")},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Inserts a Start Triplet object and an End Tuplet object and places the cursor between these two", N_("InsertTriplet"), "insert_triplet", N_("Insert Triplet")},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Inserts an Start Triplet object, which makes the notes following take 2/3 of their written duration. Later in this measure there should be an End Tuplet object.", N_("StartTriplet"), "start_triplet", N_("Start Triplet")},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Inserts an End Tuplet object, which terminates a tuplet started earlier in this measure.", N_("EndTuplet"), "end_tuplet", N_("End Tuplet")},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet", N_("InsertQuadtuplet"), "insert_quadtuplet", N_("Insert Quadruplet")},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet", N_("InsertQuintuplet"), "insert_quintuplet", N_("Insert Quintuplet")},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet", N_("InsertSextuplet"), "insert_sextuplet", N_("Insert Sextuplet")},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet", N_("InsertSeptuplet"), "insert_septuplet", N_("Insert Septuplet")},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Add a note to the current chord\\nThe cursor position determines which note to add", N_("AddNoteToChord"), "add_tone_key", "Add note", N_("Add Tone")},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Remove a note from the current chord, based on the cursor position", N_("RemoveNoteFromChord"), "remove_tone_key", "Remove note", N_("Remove Tone")},

  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Sharpen the note at the cursor", N_("Sharpen"), "sharpen_key", "Sharpen", N_("Sharpen Note")},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Flatten the note at the cursor", N_("Flatten"), "flatten_key", "Flatten", N_("Flatten Note")},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Increases the sharpness of the next entered note. The status bar shows the current state.", N_("PendingSharpen"), "pending_sharpen", "Sharpen Next Note", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Increases the flatness of the next entered note. The status bar shows the current state.", N_("PendingFlatten"), "pending_flatten", "Flatten Next Note", NULL},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Alters a StemNeutral object to stem up.", N_("StemUp"), "stem_up", "StemUp", N_("Stem Up")},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Alters a StemNeutral object to stem down.", N_("StemDown"), "stem_down", "StemDown", N_("Stem Down")},

  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Lengthen the chord, note or rest at the cursor by dotting it.", N_("AddDot"), "add_dot_key", N_("Add Dot")},
  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Reduce the dotting of the chord note or rest at the cursor.", N_("RemoveDot"), "remove_dot_key", N_("Remove Dot")},

  {CMD_CATEGORY_ARTICULATION, NULL, "Inserts a duplicate of the current note, tied", N_("InsertTiedNote"), "tie_notes_key", "Tied note", N_("Insert Tied Note")},
  {CMD_CATEGORY_ARTICULATION | CMD_CATEGORY_DIRECT, NULL, "Ties/unties the note at the cursor", N_("ToggleTie"), "toggle_tie", "Toggle Tie", N_("Toggle Tied Note")},

  {CMD_CATEGORY_EDIT, NULL, "Delete the object at the cursor", N_("DeleteObject"), "deleteobject", N_("Delete Object")},
  {CMD_CATEGORY_EDIT, NULL, "Delete to the left of the cursor.", N_("DeletePreviousObject"), "deletepreviousobject", N_("Delete Previous Object")},

  {CMD_CATEGORY_MEASURE, NULL, "Insert a blank measure before the current one (in all staffs)", N_("InsertMeasure"), "insert_measure_key", N_("Insert Measure Before")},
  {CMD_CATEGORY_MEASURE, NULL, "Insert a blank measure after the current one (in all staffs)", N_("AddMeasure"), "addmeasureafter", N_("Insert Measure After")},



  {CMD_CATEGORY_MEASURE, NULL, "Insert a blank measure before the current one (in current staff)", N_("InsertMeasureBefore"), "insertmeasurebefore", N_("Staff Insert Measure Before")},
  {CMD_CATEGORY_MEASURE, NULL, "Insert a blank measure in current staff after the current measure", N_("InsertMeasureAfter"), "insertmeasureafter", N_("Staff Insert Measure After")},


  {CMD_CATEGORY_MEASURE, NULL, "Append an empty measure at the end of the current staff", N_("AppendMeasure"), "append_measure_key", N_("Staff Append Measure")},
  {CMD_CATEGORY_MEASURE, NULL, "Delete the current measure in this staff, leaving the staff short", N_("DeleteMeasure"), "deletemeasure", N_("Staff Delete Measure")},
  {CMD_CATEGORY_MEASURE, NULL, "Delete the current measure in all staffs", N_("DeleteMeasureAllStaffs"), "deletemeasureallstaffs", N_("Delete Measure All Staffs")},
  {CMD_CATEGORY_MEASURE, NULL, "No Tooltip yet", N_("ShrinkMeasures"), "adjust_measure_less_width_key", N_("Shrink Measure")},
  {CMD_CATEGORY_MEASURE, NULL, "No Tooltip yet", N_("WidenMeasures"), "adjust_measure_more_width_key", N_("Widen Measures")},

  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("ShorterStaffs"), "adjust_staff_less_height_key", N_("Shorter Staffs")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("TallerStaffs"), "adjust_staff_more_height_key", N_("Taller Staffs")},

  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertTrebleClef"), "newcleftreble", N_("New Treble Clef")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertBassClef"), "newclefbass", N_("New Bass Clef")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Insertg8clef"), "newclefg8", N_("New G8 Clef")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertAltoClef"), "newclefalto", N_("New Alto Clef")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertTenorClef"), "newcleftenor", N_("New Tenor Clef")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertSopranoClef"), "newclefsoprano", N_("New Soprano Clef")},

  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialTrebleClef"), "setcleftreble", N_("Set Treble Clef")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialBassClef"), "setclefbass", N_("Set Bass Clef")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialg8clef"), "setclefg8", N_("Set G8 Clef")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialAltoClef"), "setclefalto", N_("Set Alto Clef")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialTenorClef"), "setcleftenor", N_("Set Tenor Clef")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialSopranoClef"), "setclefsoprano", N_("Set Soprano Clef")},

  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Insert22Time"), "newtimesig22", N_("Insert 2/2 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Insert32Time"), "newtimesig32", N_("Insert 3/2 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Insert42Time"), "newtimesig42", N_("Insert 4/2 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Insert44Time"), "newtimesig44", N_("Insert 4/4 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Insert34Time"), "newtimesig34", N_("Insert 3/4 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Insert24Time"), "newtimesig24", N_("Insert 2/4 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Insert64Time"), "newtimesig64", N_("Insert 6/4 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Insert38Time"), "newtimesig38", N_("Insert 3/8 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Insert68Time"), "newtimesig68", N_("Insert 6/8 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Insert128Time"), "newtimesig128", N_("Insert 12/8 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Insert98Time"), "newtimesig98", N_("Insert 9/8 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Set22Time"), "settimesig22", N_("Set 2/2 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Set32Time"), "settimesig32", N_("Set 3/2 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Set42Time"), "settimesig42", N_("Set 4/2 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Set44Time"), "settimesig44", N_("Set 4/4 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Set34Time"), "settimesig34", N_("Set 3/4 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Set24Time"), "settimesig24", N_("Set 2/4 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Set64Time"), "settimesig64", N_("Set 6/4 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Set38Time"), "settimesig38", N_("Set 3/8 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Set68Time"), "settimesig68", N_("Set 6/8 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Set128Time"), "settimesig128", N_("Set 12/8 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("Set98Time"), "settimesig98", N_("Set 9/8 Time")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertCmaj"), "newkeysigcmaj", N_("Insert Cmaj")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertGmaj"), "newkeysiggmaj", N_("Insert Gmaj")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertDmaj"), "newkeysigdmaj", N_("Insert Dmaj")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertAmaj"), "newkeysigamaj", N_("Insert Amaj")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertEmaj"), "newkeysigemaj", N_("Insert Emaj")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertBmaj"), "newkeysigbmaj", N_("Insert Bmaj")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertFSharpmaj"), "newkeysigfsharpmaj", N_("Insert F# Major")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertCSharpmaj"), "newkeysigcsharpmaj", N_("Insert C# Major")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertFmaj"), "newkeysigfmaj", N_("Insert F Major")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertBflatmaj"), "newkeysigbflatmaj", N_("Insert Bb Major")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertEflatmaj"), "newkeysigeflatmaj", N_("Insert Eb Major")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertAflatmaj"), "newkeysigaflatmaj", N_("Insert Ab Major")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertDflatmaj"), "newkeysigdflatmaj", N_("Insert Db Major")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertGflatmaj"), "newkeysiggflatmaj", N_("Insert Gb Major")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertCflatmaj"), "newkeysigcflatmaj", N_("Insert Cb Major")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertAmin"), "newkeysigamin", N_("Insert A Minor")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertEmin"), "newkeysigemin", N_("Insert E Minor")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertBmin"), "newkeysigbmin", N_("Insert B Minor")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertFSharpmin"), "newkeysigfsharpmin", N_("Insert F# Minor")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertCSharpmin"), "newkeysigcsharpmin", N_("Insert C# Minor")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertGSharpmin"), "newkeysiggsharpmin", N_("Insert G# Minor")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertDSharpmin"), "newkeysigdsharpmin", N_("Insert D# Minor")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertASharpmin"), "newkeysigasharpmin", N_("Insert A# Minor")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertDmin"), "newkeysigdmin", N_("Insert D Minor")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertGmin"), "newkeysiggmin", N_("Insert G Minor")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertCmin"), "newkeysigcmin", N_("Insert C Minor")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertFmin"), "newkeysigfmin", N_("Insert F Minor")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertBflatmin"), "newkeysigbflatmin", N_("Insert Bb Minor")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertEflatmin"), "newkeysigeflatmin", N_("Insert Eb Minor")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("InsertAflatmin"), "newkeysigaflatmin", N_("Insert Ab Minor")},

  //Functions to Set Initial Key Sig
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialCmaj"), "setkeysigcmaj", N_("Set Initial Keysig to C Major")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialGmaj"), "setkeysiggmaj", N_("Set Initial Keysig to G Major")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialDmaj"), "setkeysigdmaj", N_("Set D Major as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialAmaj"), "setkeysigamaj", N_("Set A Major as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialEmaj"), "setkeysigemaj", N_("Set E Major as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialBmaj"), "setkeysigbmaj", N_("Set B Major as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialFSharpmaj"), "setkeysigfsharpmaj", N_("Set F# Major as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialCSharpmaj"), "setkeysigcsharpmaj", N_("Set C# Major as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialFmaj"), "setkeysigfmaj", N_("Set F Major as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialBflatmaj"), "setkeysigbflatmaj", N_("Set Bb Major as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialEflatmaj"), "setkeysigeflatmaj", N_("Set Eb Major as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialAflatmaj"), "setkeysigaflatmaj", N_("Set Ab Major as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialDflatmaj"), "setkeysigdflatmaj", N_("Set Db Major as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialGflatmaj"), "setkeysiggflatmaj", N_("Set Gb Major as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialCflatmaj"), "setkeysigcflatmaj", N_("Set Cb Major as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialAmin"), "setkeysigamin", N_("Set A Minor as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialEmin"), "setkeysigemin", N_("Set E Minor as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialBmin"), "setkeysigbmin", N_("Set B Minor as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialFSharpmin"), "setkeysigfsharpmin", N_("Set F# Minor as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialCSharpmin"), "setkeysigcsharpmin", N_("Set C# Minor as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialGSharpmin"), "setkeysiggsharpmin", N_("Set G# Minor as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialDSharpmin"), "setkeysigdsharpmin", N_("Set D# Minor as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialASharpmin"), "setkeysigasharpmin", N_("Set A# Minor as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialDmin"), "setkeysigdmin", N_("Set D Minor as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialGmin"), "setkeysiggmin", N_("Set G Minor as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialCmin"), "setkeysigcmin", N_("Set C Minor as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialFmin"), "setkeysigfmin", N_("Set F Minor as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialBflatmin"), "setkeysigbflatmin", N_("Set Bb Minor as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialEflatmin"), "setkeysigeflatmin", N_("Set Eb Minor as Initial Keysig")},
  {CMD_CATEGORY_STAFF, NULL, "No Tooltip yet", N_("SetInitialAflatmin"), "setkeysigaflatmin", N_("Set Ab Minor as Initial Keysig")},


  {CMD_CATEGORY_NAVIGATION, NULL, "Sets the start point for a selection,\\nthe end point of the selection is unaltered", N_("SetMark"), "set_mark", N_("Set Mark")},
  {CMD_CATEGORY_NAVIGATION, NULL, "Gets rid of the selection.", N_("UnsetMark"), "unset_mark", N_("Unset Mark")},
  {CMD_CATEGORY_NAVIGATION, NULL, "Extends the selection to the current cursor position", N_("SetPoint"), "set_point", N_("Set Point")},
  {CMD_CATEGORY_ARTICULATION, NULL, "Insert/delete begin slur on this note", N_("ToggleBeginSlur"), "toggle_begin_slur", N_("Begin Slur")},
  {CMD_CATEGORY_ARTICULATION, NULL, "Insert/delete end slur on this note", N_("ToggleEndSlur"), "toggle_end_slur", N_("End Slur")},

  {CMD_CATEGORY_ARTICULATION, NULL, "Marks/Unmarks the chord or note at the cursor as the start of a crescendo.", N_("ToggleStartCrescendo"), "toggle_start_crescendo", N_("Start Crescendo (Off/On)")},
  {CMD_CATEGORY_ARTICULATION, NULL, "Marks/Unmarks the chord or note at the cursor as the end of a crescendo.", N_("ToggleEndCrescendo"), "toggle_end_crescendo", N_("End Crescendo (Off/On)")},
  {CMD_CATEGORY_ARTICULATION, NULL, "Marks/Unmarks the chord or note at the cursor as the start of a diminuendo.", N_("ToggleStartDiminuendo"), "toggle_start_diminuendo", N_("Start Diminuendo (Off/On)")},
  {CMD_CATEGORY_ARTICULATION, NULL, "Marks/Unmarks the chord or note at the cursor as the end of a diminuendo.", N_("ToggleEndDiminuendo"), "toggle_end_diminuendo", N_("End Diminuendo (Off/On)")},
  {CMD_CATEGORY_ARTICULATION | CMD_CATEGORY_DIRECT, NULL, "Makes the note at the cursor an appogiatura grace note, if it is one, makes it normal", N_("ToggleGrace"), "toggle_grace", N_("Grace Note Off/On")},
  {CMD_CATEGORY_ARTICULATION | CMD_CATEGORY_DIRECT, NULL, "Makes the note at the cursor an acciaccatura grace note, if it is one, makes it normal", N_("ToggleAcciaccatura"), "toggle_acciaccatura", N_("Acciaccatura Off/On")},

  {CMD_CATEGORY_ARTICULATION, NULL, "Give a cautionary accidental to the note at the cursor", N_("ForceCaution"), "force_cautionary", N_("Force Cautionary Accidental")},

  {CMD_CATEGORY_NOTE_ENTRY, NULL, "Changes the pitch of the note at the cursor to the cursor height", N_("ChangePitch"), "change_pitch", N_("Change Pitch")},

  {CMD_CATEGORY_NOTE_ENTRY, NULL, "No Tooltip yet", N_("InsertRhythm"), "insert_rhythm_pattern", N_("Insert Snippet")},
  {CMD_CATEGORY_OTHER, NULL, "Make next snippet\\nthe current snippet.\\nNotes entered will follow the rhythmic pattern of this snippet", N_("NextRhythm"), "nextrhythm", N_("Next Snippet")},
  {CMD_CATEGORY_MEASURE, NULL, "Appends a blank measure to every staff in this movement", N_("AppendMeasureAllStaffs"), "append_measure_score", N_("Append Measure All Staffs")},


  {CMD_CATEGORY_DIRECT, NULL, "Execute the scheme code from the scripting window", N_("ExecuteScheme"), "execute_scheme", N_("Execute Scheme")},



  /* from view.c menu_entries[]  */
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCreating, saving, loading, exporting, importing, typesetting and printing musical scores", N_("FileMenu"), NULL, "File", N_("File Menu")},
  {CMD_CATEGORY_DIRECT, NULL, "Creating, saving places in musical scores", N_("Bookmarks"), NULL, "Bookmarks", N_("Bookmarks")},
  {CMD_CATEGORY_DIRECT, NULL, "Different keyboard and MIDI entry modes", N_("ModeMenu"), NULL, "Mode"},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nGeneral editing commands", N_("EditMenu"), NULL, "Edit", N_("Edit Menu")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nControl which tools are to be shown", N_("ViewMenu"), NULL, "View", N_("View Menu")},

  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCommands that manipulate Staffs and voices on staffs.\\nCommands that apply to the current staff.", N_("StaffMenu"), NULL, "Staffs/Voices", N_("Staff Menu")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nThings that manipulate Voices, that is separately stemmed and beamed parts that will be typeset on the same staff.\\nKeep them displayed on separate staffs for editing purposes at least.\\nThe typesetter will amalgamate them for you.\\nSee also commands for hiding rests and moving notes and rests to avoid collisions in the Notes/Rests menu", N_("Voices"), NULL, "Voices", N_("Voices")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCommands that apply to the current movement in your score.\\nMovements in a score could be songs in a song book etc\\nAny piece of continuous music.", N_("MovementMenu"), NULL, "Movements", N_("Movement Menu")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nInformation and help for Denemo", N_("HelpMenu"), NULL, "Help", N_("Help Menu")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nChoose whether to get input audio (e.g. microphone) or MIDI controller (e.g. MIDI keyboard)\\nor just from the pc keyboard.", N_("InputMenu"), NULL, "Input", N_("Input Menu")},
  {CMD_CATEGORY_DIRECT, NULL, "Shifts the set of accidentals one step sharper", N_("SharpenEnharmonicSet"), "set_sharper", N_("Shift Accidentals Sharpwise")},
  {CMD_CATEGORY_DIRECT, NULL, "Shifts the set of accidentals one step flatter", N_("FlattenEnharmonicSet"), "set_flatter", N_("Shift Accidentals Flatwise")},
  {CMD_CATEGORY_DIRECT, NULL, "Music playback. Music is played between the playback start (green bar) and playback end (red bar).\\nThere are playback controls (See View->Playback Controls) which make it easy to set and re-set these and also to loop-play, choose the temperament to play in etc. Or use items in this menu to play the whole piece from the cursor to the end.", N_("PlaybackMenu"), NULL, "Playback", N_("Playback Menu")},



  {CMD_CATEGORY_DIRECT, "GTK_STOCK_NEW", "Start a new musical score", N_("New"), "file_newwrapper", "Empty Score"},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_NEW", "Start a new musical score for a named instrument/voice.", N_("NewScore"), "new_score_cb", N_("New")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Menu:\\nOpen a previously edited score, or a source file for transcription", N_("OpenMenu"), NULL, N_("Open")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Open a file containing a music score for editing", N_("Open"), "file_open_with_check", N_("Open")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Import a Lilypond file", N_("ImportLilypond"), "file_import_lilypond_with_check", N_("Import Lilypond")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Import a Midi file", N_("ImportMidi"), "file_import_midi_with_check", N_("Import Midi")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Import a MusicXml file", N_("ImportMusicXml"), "file_import_musicxml_with_check", N_("Import MusicXml")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Add staffs from a Denemo file", N_("AddStaffs"), "file_add_staffs", N_("Add Staffs")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Add movements from a Denemo file", N_("AddMovements"), "file_add_movements", N_("Add Movement")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_PROPERTIES", "Change properties of this movement", N_("MovementProps"), "movement_props_dialog", N_("Change Properties")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Open a file containing a music score for editing in a separate working area (tab)", N_("OpenNewWindow"), "openinnew", N_("Open In New")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_SAVE", "Save the score. The score is saved to disk in XML format.", N_("Save"), "file_savewrapper", N_("Save")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_SAVE_AS", "Save the score under a new name", N_("SaveAs"), "file_saveaswrapper", N_("Save As")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_SAVE_AS", "Save a copy of the score", N_("SaveCopy"), "file_copy_save", N_("Create Copy")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Start a new score from a built-in template file", N_("OpenTemplate"), "system_template_open_with_check", N_("Open Template")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Start a new score from a built-in example", N_("OpenExample"), "system_example_open_with_check", N_("Open Example")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Start a new score from one of your own template files", N_("OpenMyTemplate"), "local_template_open_with_check", N_("Open Custom Template")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_SAVE_AS", "Save the score as a template for re-use as a starting point for new scores", N_("SaveTemplate"), "template_save", N_("Save Template")},
  {CMD_CATEGORY_DIRECT, NULL, "Create working area (tab with an empty score in it)", N_("NewWindow"), "newview", N_("New Tab")},
  {CMD_CATEGORY_DIRECT, NULL, "Insert a new movement before the current one", N_("InsertMovementBefore"), "insert_movement_before", N_("Insert Movement Before")},
  {CMD_CATEGORY_DIRECT, NULL, "Insert a new movement after the current one", N_("InsertMovementAfter"), "insert_movement_after", N_("Insert Movement After")},
  {CMD_CATEGORY_DIRECT, NULL, "Create a new movement, usign any default template", N_("NewMovement"), "append_new_movement", N_("New Movement")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_SAVE", "Menu:\\nSave the score\\nBesides saving for later you can save the score for use as a template for future works, or save under a new name etc\\nThe score is saved to disk in XML format.", N_("SaveMenu"), NULL, N_("Save")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_SAVE_AS", "Save Parts: each staff becomes a file in lilypond format", N_("SaveParts"), "file_savepartswrapper", N_("Save Parts")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_SAVE_AS", "Export the score as a lilypond file", N_("ExportMUDELA"), "export_mudela_action", N_("Export Lilypond")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_SAVE_AS", "Export the score as a PDF document file", N_("ExportPDF"), "export_pdf_action", N_("Export PDF")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_SAVE_AS", "Export the score as a PNG image file", N_("ExportPNG"), "export_png_action", N_("Export PNG")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_SAVE_AS", "Export the score as a MIDI file", N_("ExportMIDI"), "export_midi_action", N_("Export MIDI")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_PRINT", "Menu:\\nCommands for typesetting and then printing the music.\\nA part, movement, full score or various reduced scores can be typeset.\\n See also the Score Layout view for further options.", N_("PrintMenu"), NULL, N_("Print")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_PRINT_PREVIEW", "Displays the final finished score in your PDF viewer", N_("PrintPreview"), "printpreview_cb", N_("Print Preview")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_PRINT_PREVIEW", "Typesets the score\\nIf you have a score layout selected it will use that\\notherwise all movements staffs and lyrics are typeset by default.\\nBe patient! It takes time to create a beautifully laid out score.\\nOnce complete you can view and then send to your printer or to a file as a .pdf document.", N_("PrintView"), "show_print_view", N_("Print Preview")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_PRINT", "Displays selected music from score in your pdf viewer", N_("PrintSelection"), "printselection_cb", N_("Print Selection")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_PRINT_PREVIEW", "Displays a musical excerpt in your image viewer", N_("PrintExcerptPreview"), "printexcerptpreview_cb", N_("Print Excerpt")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_PRINT", "Typesets the current movement and opens a print dialog", N_("PrintMovement"), "printmovement_cb", N_("Print Movement")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_PRINT", "Typesets the score using LilyPond and opens a print dialog", N_("Print"), "printall_cb", N_("Print")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_PRINT", "Typesets the current part (the one containing the cursor).", N_("PrintPart"), "printpart_cb", N_("Print Part")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_CLOSE", "Close the current score. Other scores (tabs) will stay open", N_("Close"), "close_gui_with_check", N_("Close Score")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_QUIT", "Quit the Denemo program - closes tabs one at a time.", N_("Quit"), "closewrapper", N_("Quit")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_UNDO", "Undoes one (more) step of your edits to the current score.", N_("Undo"), "undowrapper", N_("Undo")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_REDO", "Redoes the next of the steps you have Undone", N_("Redo"), "redowrapper", N_("Redo")},
  {CMD_CATEGORY_DIRECT, NULL, "Selecting stretches of notes", N_("Select"), NULL, N_("Select")},
  {CMD_CATEGORY_DIRECT, NULL, "Extend the selection", N_("ExtendSelect"), NULL, N_("Extend Selection")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_COPY", "Copy the music selected to the Denemo clipboard", N_("Copy"), "copywrapper", N_("Copy")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_CUT", "Cut the music selected to the Denemo clipboard", N_("Cut"), "cutwrapper", N_("Cut")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_PASTE", "Paste the Denemo clipboard into the score where the cursor is positioned", N_("Paste"), "pastewrapper", N_("Paste")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_PASTE", "Paste LilyPond notes from the text clipboard\\nThis will import music written as LilyPond syntax\\nYou open the LilyPond file in a texteditor, copy the stretch of notes (control-c command in your texteditor usually) and then use this command.", "PasteClipboard", "paste_clipboard", N_("Paste LilyPond notes")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_PROPERTIES", "Change some of the properties of the current score. This will start up a dialog window", N_("ScoreProperties"), "score_properties_dialog", N_("Score Properties")},
  {CMD_CATEGORY_DIRECT, NULL, "Save the selected music. Not sure if this is working", N_("SaveSelection"), "saveselwrapper", N_("Save Selection")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_PREFERENCES", "Set and save your preferences for how Denemo operates on startup.\\nAdvanced users can edit .denemo-XXXX/denemorc for missing ones", N_("Preferences"), "preferences_change", N_("Change Preferences")},
  {CMD_CATEGORY_DIRECT, NULL, "Set actions to take in response to keypresses", N_("KeyBindings"), NULL, "Customize Commands, Shortcuts ...", N_("Customize Key Bindings")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_SAVE", "Save the current commands and keyboard shortcuts as the default", N_("SaveAccels"), "save_default_keymap_file_wrapper", "Save Command Set", N_("Save Command Set")},
  {CMD_CATEGORY_DIRECT, NULL, "View help, change and save keyboard shortcuts", "CommandManagement", "configure_keyboard_dialog", "Manage Command Set", N_("Manage Command Set")},

  {CMD_CATEGORY_DIRECT, NULL, "Swap this staff with the one higher up.\\nBe aware that if you have inserted directives to move a voice to another staff\\nthese may need re-making.", N_("SwapStaffs"), "swapstaffs", N_("Swap Staffs")},
  {CMD_CATEGORY_DIRECT, NULL, "Split off the next voice as a separate staff", N_("SplitVoices"), "splitstaffs", N_("Split Voices")},
  {CMD_CATEGORY_DIRECT, NULL, "Merge this staff as a voice on the previous staff", N_("JoinVoices"), "joinstaffs", N_("Join Voices")},
  {CMD_CATEGORY_DIRECT, NULL, "Swap this movement with the one before", N_("SwapMovements"), "swapmovements", N_("Swap Movements")},
  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Go to the higher numbered voice on staff, extending selection if any", N_("VoiceUp"), "voiceup", N_("Voice Up")},
  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Go to the lower numbered voice on this staff, extending selection if any", N_("VoiceDown"), "voicedown", N_("Voice Down")},

  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Go to the higher numbered voice on staff without altering selection", N_("MoveToVoiceUp"), "movetovoiceup", N_("Move to Voice Up")},
  {CMD_CATEGORY_NAVIGATION | CMD_CATEGORY_BOOLEAN, NULL, "Go to the lower numbered voice on this staff without altering selection", N_("MoveToVoiceDown"), "movetovoicedown", N_("Move to Voice Down")},



  {CMD_CATEGORY_DIRECT, NULL, "Inserts a new staff before the current staff", N_("AddBefore"), "newstaffbefore", N_("Add Staff Before")},
  {CMD_CATEGORY_DIRECT, NULL, "Inserts/Adds a new staff after the current staff", N_("AddAfter"), "dnm_newstaffafter", N_("Add Staff After")},
  {CMD_CATEGORY_DIRECT, NULL, "Inserts a new staff at the top of the score", N_("AddInitial"), "newstaffinitial", N_("Add Initial Staff")},
  {CMD_CATEGORY_DIRECT, NULL, "Inserts a new staff at the end of the score", N_("AddLast"), "newstafflast", N_("Add Last Staff")},
  {CMD_CATEGORY_DIRECT, NULL, "Deletes the staff before the current staff", N_("DeleteBefore"), "delete_staff_before", N_("Delete Staff Before")},
  {CMD_CATEGORY_DIRECT, NULL, "Deletes the current staff", N_("DeleteStaff"), "delete_staff_current", N_("Delete Current Staff")},
  {CMD_CATEGORY_DIRECT, NULL, "Deletes the staff after the current staff", N_("DeleteAfter"), "delete_staff_after", N_("Delete Staff After")},
  {CMD_CATEGORY_DIRECT, NULL, "Adds a new voice (part), to the current staff. It is tricky to switch between the voices. Suggest to use merge staffs", "AddVoice", "dnm_newstaffvoice", N_("Add Voice")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_PROPERTIES", "Change the properties of the current staff", N_("StaffProperties"), "staff_properties_change_cb", N_("Staff Properties")},
  {CMD_CATEGORY_DIRECT, NULL, "Insert", N_("InsertMenu"), NULL},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nClef", N_("Clef"), NULL},
  {CMD_CATEGORY_DIRECT, NULL, "Change the initial clef of the current staff", N_("InitialClef"), "clef_change_initial", N_("Initial Clef")},
  {CMD_CATEGORY_DIRECT, NULL, "Insert/Edit a change of clef at the cursor", N_("InsertClef"), "clef_change_insert", N_("Clef Change")},


  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nSet the key signature or insert a change of key signature\\nSharpen or flatten a key signature or\\nhide it on printing", N_("Key"), NULL, N_("Keys")},
  {CMD_CATEGORY_DIRECT, NULL, "Set the initial key signature of the current staff", N_("InitialKey"), "key_change_initial", N_("Initial Key")},
  {CMD_CATEGORY_DIRECT, NULL, "Insert/Edit a key change at the cursor position", N_("InsertKey"), "key_change_insert", N_("Key Signature Change")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nSet the time signature, insert or edit a change of timesignature,\\nhide a time signature on printing.\\nControl whether to use numerical or traditional styles.", N_("TimeSig"), NULL, N_("Time Signatures")},
  {CMD_CATEGORY_DIRECT, NULL, "Set the initial time signature of the current staff", N_("InitialTimeSig"), "timesig_change_initial", N_("Inital Time Signature")},
  {CMD_CATEGORY_DIRECT, NULL, "Edit/Insert a time signature change for the current measure", N_("InsertTimeSig"), "timesig_change_insert", N_("Time Signature Change")},
  {CMD_CATEGORY_DIRECT, NULL, "Change the type of notehead for the current note", N_("ChangeNotehead"), "set_notehead", N_("Set Notehead")},
  {CMD_CATEGORY_DIRECT, NULL, "Inserts a stem neutral object. After this automatic stem directions are active. You can click on this tag and use Sharpen/StemUp etc commands to change stem direction", N_("InsertStem"), "stem_directive_insert", N_("Auto Stemming")},
  {CMD_CATEGORY_DIRECT, NULL, "Add a verse of lyrics", "AddVerse", "add_verse", "Add Lyric Verse", NULL},
  {CMD_CATEGORY_DIRECT, NULL, "Deletes current verse of lyrics from current voice", "DeleteVerse", "delete_verse", "Delete Verse", NULL},
  {CMD_CATEGORY_DIRECT, NULL, "Add a bass figure to the current note. Use | sign to split the duration of a note so as to have multiple figures on one note. See Lilypond docs for other notation", N_("EditFiguredBass"), "figure_insert", "Insert/Edit Figured Bass", N_("Edit Figured Bass")},
  {CMD_CATEGORY_DIRECT, NULL, "Delete the figured bass on the current staff", N_("DeleteFiguredBass"), "delete_figured_bass", "Delete Figures", N_("Delete Figures")},
  {CMD_CATEGORY_DIRECT, NULL, "Hide the figured bass on the current staff on printing", N_("HideFiguredBass"), "hide_figured_bass", "Hide Figures (Print)", N_("Hide Figures")},
  {CMD_CATEGORY_DIRECT, NULL, "Show the figured bass on the current staff on printing", N_("ShowFiguredBass"), "show_figured_bass", "Show Figures (Print)", N_("Show Figures")},
  {CMD_CATEGORY_DIRECT, NULL, "Allows chord symbols to be added to the current note. E.G.cis:dim7 for c-sharp diminished 7th. See Lilypond docs for notation", N_("EditChords"), "fakechord_insert", N_("Edit Chord Symbols")},
  {CMD_CATEGORY_DIRECT, NULL, "Inserts a dynamic marking at the cursor position", N_("InsertDynamic"), "insert_dynamic", N_("Insert Dynamics")},

  {CMD_CATEGORY_DIRECT, NULL, "Edit the object at the cursor.", N_("EditObject"), "edit_object", N_("Edit Object")},

  {CMD_CATEGORY_DIRECT, NULL, "Edit any directives attached to chord/note at cursor.", N_("EditDirective"), "edit_object_directive", N_("Edit Directives")},
  {CMD_CATEGORY_DIRECT, NULL, "Edit any directives attached to staff.", N_("EditStaffDirective"), "edit_staff_directive", N_("Edit Staff Directives")},
  {CMD_CATEGORY_DIRECT, NULL, "Edit any directives attached to voice.", N_("EditVoiceDirective"), "edit_voice_directive", N_("Edit Voice Directives")},
  {CMD_CATEGORY_DIRECT, NULL, "Edit any directives attached to score.", N_("EditScoreDirective"), "edit_score_directive", N_("Edit Score Directives")},
  {CMD_CATEGORY_DIRECT, NULL, "Edit any directives attached to movement.", N_("EditMovementDirective"), "edit_movement_directive", N_("Edit Movement Directives")},


  {CMD_CATEGORY_DIRECT, NULL, "Edit any directives attached to clef.", N_("EditClefDirective"), "edit_clef_directive", N_("Edit Clef Directives")},
  {CMD_CATEGORY_DIRECT, NULL, "Edit any directives attached to time signature.", N_("EditTimesigDirective"), "edit_timesig_directive", N_("Edit Time Signature Directives")},
  {CMD_CATEGORY_DIRECT, NULL, "Edit any directives attached to key signature.", N_("EditKeysigDirective"), "edit_keysig_directive", N_("Edit Key Signature Directives")},

  {CMD_CATEGORY_DIRECT, NULL, "Delete a directive attached to chord/note at cursor.", N_("DeleteDirective"), "delete_chord_or_note_directive", N_("Delete a Directive")},


  {CMD_CATEGORY_DIRECT, NULL, "Attach or edit attached LilyPond text to the note at the cursor. This can be used for guitar fingerings, cautionary accidentals and much more. See LilyPond documentation.", N_("AttachLilyToNote"), "note_directive", N_("Attach Lilypond to Note")},
  {CMD_CATEGORY_DIRECT, NULL, "Attach or edit attached LilyPond text to the chord at the cursor. This can be used for attaching and placing text and much more. See LilyPond documentation.", N_("AttachLilyToChord"), "chord_directive", N_("Attach Lilypond to Chord")},
  {CMD_CATEGORY_DIRECT, NULL, "Inserts specialized barline at the cursor position. Mostly not working", N_("InsertBarline"), "insert_barline", N_("Insert Barline")},
  {CMD_CATEGORY_DIRECT, NULL, "Moving the cursor within the current movement, setting bookmarks, seeking things in the score", N_("NavigationMenu"), NULL, "Navigation", N_("Navigation Menu")},
  {CMD_CATEGORY_DIRECT, NULL, "Opens a dialog for going to a numbered measure", N_("GoToMeasure"), "tomeasurenum", "Go to Measure", N_("Go To Measure")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_GOTO_FIRST", "Cursor to start of staff/voice, extending selection if any", N_("GoToBeginning"), "tohome", "Go to Beginning", N_("Mark to Staff/Voice Beginning")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_GOTO_LAST", "Cursor to end of staff/voice, extending selection if any", N_("GoToEnd"), "toend", "Go to End", N_("Mark to Staff/Voice End")},

  {CMD_CATEGORY_DIRECT, "GTK_STOCK_GOTO_FIRST", "Cursor to start of staff/voice, without extending selection if any", N_("MoveToBeginning"), "movetostart", "Move to Staff/Voice Beginning", N_("Move to Staff/Voice Beginning")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_GOTO_LAST", "Cursor to end of staff/voice, without extending selection if any", N_("MoveToEnd"), "movetoend", "Move to Staff/Voice End", N_("Move to Staff/Voice End")},




  {CMD_CATEGORY_DIRECT, NULL, "Go to the next movement", N_("NextMovement"), "next_movement", N_("Next Movement")},
  {CMD_CATEGORY_DIRECT, NULL, "Go to the previous movement", N_("PreviousMovement"), "prev_movement", N_("Previous Movement")},
  {CMD_CATEGORY_DIRECT, NULL, "Delete the current movement", N_("DeleteMovement"), "delete_movement", N_("Delete Movement")},


  {CMD_CATEGORY_DIRECT, "GTK_STOCK_MEDIA_PLAY", "Play", N_("Play"), "ext_midi_playback", N_("Play")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_MEDIA_STOP", "Stop", N_("Stop"), "stop_midi_playback", N_("Stop")},


  {CMD_CATEGORY_DIRECT, "GTK_STOCK_PROPERTIES", "Allows you to specify properties used in playing back (midi)", N_("PlaybackProperties"), "playback_properties_change", "Playback Properties", N_("Playback Properties")},

  {CMD_CATEGORY_DIRECT, NULL, "Opens a browser on the user manual", N_("Help"), "browse_manual", N_("Browse Manual")},
  {CMD_CATEGORY_DIRECT, NULL, "Gives the version number etc of this program", N_("About"), "about", N_("About")},


  {CMD_CATEGORY_DIRECT, NULL, "Allows choosing extra commands/menu items from disk", N_("MoreMenu"), NULL, N_("More")},
  {CMD_CATEGORY_DIRECT, NULL, "Allows choosing standard extra commands/menu items", N_("MoreCommands"), "morecommands", N_("More Commands")},
  {CMD_CATEGORY_DIRECT, NULL, "Allows choosing extra commands/menu items from your own collection of extras", N_("MyCommands"), "mycommands", N_("My Commands")},
  {CMD_CATEGORY_DIRECT, NULL, "Refreshes the set of commands available from Denemo.org.\\nUse More Commands after this has finished", N_("FetchCommands"), "fetchcommands", N_("Update Commands from Internet")},





  {CMD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Open previously used files", N_("OpenRecent"), NULL, N_("Open Recent")},
  {CMD_CATEGORY_DIRECT, "GTK_STOCK_OPEN", "Menu:\\nImport the supported file formats", N_("Import"), NULL, N_("Import File")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nExport the music to another file format.\\nThese file formats will not support all the features of the score.", N_("Export"), NULL, N_("Export As")},
  {CMD_CATEGORY_DIRECT, NULL, "Toggle between current mode and edit mode", N_("ToggleEdit"), "toggle_edit_mode", N_("Toggle Edit Mode")},
  {CMD_CATEGORY_DIRECT, NULL, "Toggle between note entry and rest entry", N_("ToggleRest"), "toggle_rest_mode", N_("Toggle Rest Mode")},
  {CMD_CATEGORY_DIRECT, NULL, "Toggle audible feedback on/off", N_("ToggleRhythm"), "toggle_rhythm_mode", N_("Toggle Audible Feedback")},
  {CMD_CATEGORY_DIRECT, NULL, "Clear the list of pitches that overlay the notes", N_("ClearOverlay"), "clear_overlay", N_("Clear Overlay")},
  {CMD_CATEGORY_DIRECT, NULL, "Copy selection as music snippet or rhythm pattern for notes to follow as they are entered", N_("CreateRhythm"), "create_rhythm_cb", N_("Create Snippet")},
  {CMD_CATEGORY_DIRECT, NULL, "Delete the selected music snippet/rhythm pattern", N_("DeleteRhythm"), "delete_rhythm_cb", N_("Delete Snippet")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nMoving the cursor and inserting notes or rests there", N_("ClassicModeNote"), NULL, N_("Classic Mode")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nMoving the cursor to the nearest ...", N_("SelectNote"), NULL, N_("Select Note")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCommands for notes: inserting, deleting, etc.", N_("InsertModeNote"), NULL, N_("Insert")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCommands to control the stem up/down", N_("StemControl"), NULL, N_("Stem Direction")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nDifferent types of notehead", N_("NoteheadControl"), NULL, N_("Notehead Types")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCreating Tied Notes\\nDo not confuse with slurs!!", N_("TiedNotes"), NULL, N_("Tied Notes")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nEffects that are only for the Denemo display,\\nnot affecting the printed page.", N_("DisplayEffects"), NULL, N_("Display Effects")},

  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nArticulations", N_("Articulations"), NULL, N_("Articulations")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nSlurs\\nDo not confuse with ties which in print can look the same\\nSlurs have little ticks at the end in the Denemo display.", N_("Slurs"), NULL, N_("Slurs")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nGrace Notes", N_("GraceNotes"), NULL, N_("Grace Notes")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCommands that affect the entire piece of music across all movements.", N_("Score"), NULL, N_("Score")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nInstruments", N_("Instruments"), NULL, N_("Instruments")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nMarkings to be applied to the note at the cursor.\\nIncludes articulations, string numbers, chord symbols, arbitrary text ...", N_("Markings"), NULL, N_("Markings")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nText and symbol Markings", N_("TextMarks"), NULL, N_("Text/Symbol")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nMarkings on scores for Instruments", "Strings", NULL, N_("Fingerings etc for Instruments")},

  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nInserting the note ...", N_("InsertNote"), NULL, "Note Insertion", N_("Note Insertion")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nLyrics consist of syllables which are placed under or above the notes of the voice they are attached to\\nYou can use the hyphen - and underscore _ to affect the placement\\nas well as slurs placed over notes. Spaces, tabs or new lines can serve to separate the syllables, it makes no difference which.", N_("Lyrics"), NULL, N_("Lyrics")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nDirectives tell Denemo to do something special with the object they are attached to\\nor at the point in the music they are placed\\nThis can be directing the typesetter to do something special\\nor issuing MIDI instructions to alter the playback\\nor even take an action, such as linking to source manuscript when clicked", N_("Directives"), NULL, N_("Directives")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nMoving the Denemo cursor around the piece", N_("Navigation"), NULL, N_("Navigation")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nEntering notes", N_("NoteEntry"), NULL, N_("Insert Note")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nMarks placed on notes and chords", N_("Articulation"), NULL, N_("Articulation")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nEditing", N_("Edit"), NULL, N_("Edit")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nManipulating measures\\nCommands that apply to the current measure", N_("Measure"), NULL, N_("Measure")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCommands for staffs", N_("Staff"), NULL, N_("Staff")},

  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nPlaying the music via the computer's soundcard.\\nAll or part of the piece can be played controlled by the green and red markers (start and stop playing)\\nUse the Playback Controls (from the View menu) to set/reset these and to loop play while editing the score", N_("Playback"), NULL, N_("Playback")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nChanging the prevailing duration or rhythm pattern", N_("SelectDuration"), NULL, N_("Select Duration")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nAppending, Changing, and deleting notes", N_("EditModeNote"), NULL, N_("Append/Edit")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nAppending or Editing notes", N_("EditNote"), NULL, N_("Append/Edit Note")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nAppending or Editing durations", N_("EditDuration"), NULL, N_("Append/Insert Duration")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nMoving the cursor", N_("Cursor"), NULL, N_("Cursor")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nMoving the cursor to note positions", N_("CursorToNote"), NULL, N_("Cursor to Note")},

  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nInsert/change clef, set initial clef, hide a clef on printing,\\nor display in a different clef", N_("ClefMenu"), NULL, N_("Clefs")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCommands for creating chords. Note that chord symbols and figured bass are under markings on notes.\\nSee Notes/Rests menu", N_("ChordMenu"), NULL, "Chords", N_("Chords")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nAdding notes to a note or chord at the cursor to make chords", N_("ChordNoteMenu"), NULL, "Add Note", N_("Add Note")},

  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nAdding Chord Symbols over music", N_("ChordSymbols"), NULL, "Chords Symbols", N_("Chord Symbols")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nAdding Figured Bass Figures", N_("FiguredBass"), NULL, "Figured Bass", N_("Figured Bass")},


  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nMeasures: adding, deleting, navigating etc", N_("MeasureMenu"), NULL, N_("Measures")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nInserting notes, measures, staffs, keysignatures etc", N_("Insert"), NULL, N_("Insert")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nInsert a new staff postioned relative to current staff", N_("InsertStaff"), NULL, N_("Add Staff")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nInsert a new movement positioned relative to current movement", N_("InsertMovement"), NULL, N_("Insert Movement")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nInserting notes of a given duration at cursor note height", N_("InsertDuration"), NULL, N_("Insert at Cursor")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nChanging properties of notes, measures, staffs, keysigs etc", N_("Change"), NULL, N_("Change")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nModeless actions on notes/rests", N_("ModelessNote"), NULL, N_("Notes/Durations")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCommands for notes/rests", N_("NotesRests"), NULL, N_("Notes/Rests")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nModeless entry of rests", N_("RestEntry"), NULL, N_("Rest Insertion")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nEducational Games", N_("Educational"), NULL, N_("Educational")},

  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nEditing directives", N_("EditDirectivesMenu"), NULL, N_("Edit Directive(s)")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nChanging the note at the cursor to the nearest ...", N_("ChangeNote"), NULL, N_("Change Note")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nChanges the duration of the current note", N_("ChangeDuration"), NULL, N_("Edit Duration")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nChanges the duration of the current rest", N_("ChangeRest"), NULL, N_("Change Rest")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nDynamics, staccato, slurs, ties and other expressive marks", N_("ExpressionMarks"), NULL, "Expression Marks", N_("Expression Marks")},
  //  {CMD_CATEGORY_DIRECT, NULL, "Markings above and below the music", N_("Markings"), NULL, "Markings", N_("Markings")},
  {CMD_CATEGORY_DIRECT, NULL, "Dynamic markings", N_("Dynamics"), NULL, "Dynamics", N_("Dynamics")},

  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\ngrace notes etc", N_("Ornaments"), NULL, N_("Ornaments")},
  {CMD_CATEGORY_DIRECT, NULL, "Lyrics, chord symbols, figured basses etc", N_("Other"), NULL, N_("Other")},
  {CMD_CATEGORY_DIRECT, NULL, "Less used actions", N_("Others"), NULL, N_("Others")},
  {CMD_CATEGORY_DIRECT, NULL, "Customized LilyPond inserts. Store often-used inserts here labelled with what they do", N_("Favorites"), NULL, N_("Favorites")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nAdd a custom LilyPond insert to favorites menu", N_("AddFavorite"), NULL, N_("Add Favorite")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nEntering triplets and other tuplets", N_("Tuplets"), NULL, N_("Tuplets")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nDeleting notes, measures staffs keysigs etc", N_("Delete"), NULL, N_("Delete")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nBeaming - controlling which notes are beamed together", N_("Beaming"), NULL, N_("Beaming")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nFor making directives apply only to one score layout or be omitted from one score layout", N_("Conditional Directives"), NULL, N_("Conditional Directives")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nFor affecting how MIDI in works. MIDI in refers to MIDI keyboards or other controllers connected to the computer. You may need to configure this using the Audio/MIDI section of the Preferences.", N_("MIDI"), NULL, N_("MIDI Keyboard")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nChoose between different ways of entering notes from the computer keyboard.", N_("Keyboard"), NULL, N_("PC Keyboard")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nChoose between different ways of using the mouse.", N_("Mouse"), NULL, N_("Mouse")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nTransposing the score when typesetting.", N_("PrintTranspositions"), NULL, N_("Print Transposed")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nInserting markers to start/stop transposition when typesetting. The markers affect only the music in the staff/voice they are placed in.", N_("Print Transpositions"), NULL, N_("Start/Stop Transposing")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nDifferent sorts of barline (repeat barlines, double bars, end of movement ...)", N_("Barlines"), NULL, N_("Inserting Barlines")},



  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nMoving the Denemo cursor forwards/backwards to find useful places in the score", N_("Seek"), NULL, N_("Seek")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nVarious titling schemes, (main title, movement titles, composer etc) and adding table of contents, critical commentary", N_("Titles"), NULL, N_("Titles")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCommands to affect the layout of the typeset score.", N_("PrintLayout"), NULL, N_("Print Layout")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCommands to embed images taken from original manuscripts etc into the score (one for each measure)", N_("CaptureScore"), NULL, N_("Capture Score")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCreating an Ossia staff, which appears with alternative interpretations for a bar or two on typesetting", N_("Ossia"), NULL, N_("Ossia")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCommands for setting how often bar numbers appear in the typeset version of this staff", N_("Numbering"), NULL, N_("Bar Numbering")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nVarious sorts of brace linking the staffs together. These may be nested, see the Score Layout view for a display of the staff groups created.", N_("StaffGroupings"), NULL, N_("Staff Groupings (Braces)")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nAdding notes to the chord at the cursor at various intervals above base note", N_("AddAboveBase"), NULL, N_("Add Note Above Base of Chord")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nAdding notes to the chord at the cursor at various intervals below the top-most note", N_("AddBelowTop"), NULL, N_("Add Note Below Top of Chord")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCommands to include other durations in a chord (instead of polyphony - this is specialized stuff!) The note at the cursor height is altered.", N_("ChangePrintDuration"), NULL, N_("Change Duration of Note")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nFingerings (numbers) to typeset near to notes", N_("Fingerings"), NULL, N_("Fingerings")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nAlters the note/chord at the cursor or the selection transposing it by the interval chosen. This alters the music in the score - see other transpose options for transposing the printed output, leaving the music untouched.", N_("Transpose"), NULL, N_("Transpose Music")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nMark the directive at the cursor as applying to just the current score layout, or to exclude the current layout. Use this for example to exclude redundant first and second time markings in parts which don't need them.", N_("Conditional-Directives"), NULL, N_("Make a Directive Conditional")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nTo insert random notes at cursor", N_("SingleRandomNote"), NULL, N_("Single Random Note")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCreating rhythms from a sequence of characters.", N_("GenerateRhythmFromString"), NULL, N_("Generate Rhythm from String")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nMethods of generating music of guided randomness", N_("NotationMagick"), NULL, N_("Notation Magick")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nZooming the Denemo display. Usually Control and mouse wheel are used for this.", N_("Zoom"), NULL, N_("Display Zoom")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nGames for testing your musical ear", N_("Aural Training"), NULL, N_("Aural Training")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nGames for testing your music reading ability. Notice there is (elsewhere) the Checking Pitches feature which lets you test your ability to play via Midi Controller.", N_("Note-Reading"), NULL, N_("Note Reading")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCommands to place titles etc at the start of the music (and each movement if needed). Do not mix with Book Titles.", N_("Simple Titles"), NULL, N_("Simple Titling")},
  {CMD_CATEGORY_DIRECT, NULL, "Menu:\\nCommands to create a title page, table of contents, commentaries, appendices etc.", N_("Book Titles"), NULL, N_("Book Titling")},



};

#define ni denemo_commands[i].name
#define ii denemo_commands[i].icon
#define ml denemo_commands[i].menu_label
#define ti denemo_commands[i].tooltip
#define fi denemo_commands[i].function
#define mi denemo_commands[i].category
int
main ()
{
  FILE *callbacks, *entries, *xml, *scheme, *scheme_cb, *register_commands;
  scheme_cb = fopen ("scheme_cb.h", "w");
  scheme = fopen ("scheme.h", "w");
  callbacks = fopen ("callbacks.h", "w");
  entries = fopen ("entries.h", "w");
  register_commands = fopen ("register_commands.h", "w");
  xml = fopen ("xml.fragment", "w");
  if (!callbacks || !entries || !xml || !scheme || !scheme_cb || !register_commands)
    return -1;
  fprintf (callbacks, "/******** generated automatically from generate_source. See generate_source.c */\n");
  fprintf (entries, "/******** generated automatically from generate_source. See generate_source.c */\n");

  int i, j;
  int n_denemo_commands = (sizeof (denemo_commands) / sizeof (struct name_and_function));

  for (i = 0; i < n_denemo_commands; i++)
    {
      if (fi != NULL)
        {
          if (!(mi & CMD_CATEGORY_DIRECT))
            {
              fprintf (callbacks, "/*%s %s*/\n", ni, fi);
        /*******************   create a callback for calling from a menuitem *******************/
              if (mi & CMD_CATEGORY_BOOLEAN)
                {
                  fprintf (callbacks, "static void %s_cb (GtkAction *action, DenemoScriptParam *param) {\n" "%s (param);\n%s}\n", fi, fi, (CMD_CLASS (mi) == CMD_CATEGORY_NAVIGATION) ? "gtk_widget_queue_draw(Denemo.scorearea);\n" : " displayhelper (Denemo.gui);\n score_status(gui, TRUE);\n");
                }
              else
                {
                  fprintf (callbacks, "static void %s_cb (GtkAction *action, gpointer param) {\n" "  DenemoGUI *gui = Denemo.gui;\n" "%s (gui);\n%s}\n", fi, fi, CMD_CLASS (mi) == CMD_CATEGORY_NAVIGATION ? "gtk_widget_queue_draw(Denemo.scorearea);\n" : "  displayhelper (gui);\nscore_status(gui, TRUE);\n");
                }
            }

      /*******************   create a procedure d-<name> in scheme to call scheme_<name>  *******************/
          fprintf (scheme, "/*%s %s*/\n", ni, fi);
          fprintf (scheme, "SCM scheme_%s(SCM optional);\ninstall_scm_function (\"d-%s\", scheme_%s);\n", ni, ni, ni);  // for direct callback via (scheme_xxx)
          fprintf (scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.map, \"%s\")), \"scm\", (gpointer)1);\n", ni);     //define a property "scm" on the action to mean it scheme can call the action.

      /*******************   create a callback scheme_<name> for calling from a scheme procedure d-<name>  *******************/
          fprintf (scheme_cb, "SCM scheme_%s (SCM optional) {\ngboolean query=FALSE;\nDenemoScriptParam param;\nGString *gstr=NULL;\nint length;\n   char *str=NULL;\nif(scm_is_string(optional)){\nstr = scm_to_locale_stringn(optional, (size_t *)&length);\ngstr = g_string_new_len(str, length);\nif(!strncmp(\"query\",str,5)) query = TRUE;\
          }\n\
         param.string = gstr;\n\
         param.status = FALSE;\n\
         \n%s%s (NULL, &param);\n\
         if(param.status && query) return scm_from_locale_string (gstr->str);\
         if(gstr) g_string_free(gstr, TRUE);\nreturn SCM_BOOL(param.status);\n}\n", ni, fi, !(mi & CMD_CATEGORY_DIRECT) ? "_cb" : "");
      /****************** install the command in the hash table of commands (keymap) **************/
          fprintf (register_commands, "register_command(Denemo.map, gtk_action_group_get_action(action_group, \"%s\"), \"%s\", _(\"%s\"), _(\"%s\"), %s);\n", ni, ni, ml ? ml : ni, ti ? ti : ni, fi);
      /****************** install the command as an action in the menu system **************************/
          fprintf (entries, "{\"%s\", %s, N_(\"%s\"), \"\"," "N_(\"%s\")," "G_CALLBACK (%s%s)},\n", ni, ii ? ii : "NULL", ml ? ml : ni, ti ? ti : ni, fi, (mi & CMD_CATEGORY_DIRECT) ? "" : "_cb");
        }
      else                      //no callback function - a menu rather than a menu item. It still needs to be added as an action in the menu system.
        fprintf (entries, "{\"%s\", %s, N_(\"%s\"), \"\"," "N_(\"%s\")},\n", ni, ii ? ii : "NULL", ml ? ml : ni, ti ? ti : ni);
    }

  /* generate source for duration callbacks - these were intercepted when
     typed at the keyboard to set prevailing rhythm, so the callback has to
     include code for this */
  //     for(i=0;i<9;i++) {

  //     }

  for (i = 0; i < 9; i++)
    {
      /* callbacks for mode independent duration actions InsertRest0,1,2... InsertDur,ChangeDur0,1,2... SetDur0,1,2... */
      fprintf (callbacks,
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
               "/* UNUSED\n"
               "static void Dummy%d(GtkAction *action, gpointer param){\n"
               "  DenemoGUI *gui = Denemo.gui;\n"
               "  gint mode = gui->mode;\n"
               "  gboolean appending = gui->si->cursor_appending;\n"
               "  if(appending)\n"
               "    movecursorleft(NULL);\n"
               "  gui->mode = INPUTEDIT|INPUTREST;\n"
               "  insert_chord_%dkey(gui);\n"
               "  gui->mode = mode;\n"
               "  if(appending)\n"
               "    movecursorright(NULL);\n"
               "  score_status(gui, TRUE);\n"
               "  displayhelper(gui);\n"
               "}\n"
               "*/\n"
               "static void InsertDur%d(GtkAction *action, gpointer param){\n"
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
               "  if(appending)\n" "    movecursorleft(NULL);\n" "  gui->mode = INPUTEDIT|INPUTNORMAL;\n" "  insert_chord_%dkey(gui);\n" "  gui->mode = mode;\n" "  if(appending)\n" "    movecursorright(NULL);\n" "  score_status(gui, TRUE);\n" "  displayhelper(gui);\n" "}\n" "static void SetDur%d(GtkAction *action, gpointer param){\n" "  DenemoGUI *gui = Denemo.gui;\n" "  highlight_duration(gui, %d);\n" "//  displayhelper(gui);\n" "}\n", i, i, i, i, i, i, i, i, i, i, i, i);


      /* callbacks for mode sensitive  duration actions, Dur0,1,2 ... */
      fprintf (callbacks, "static void Dur%d  (GtkAction *action, gpointer param) {\n" "  DenemoGUI *gui = Denemo.gui;\n" " if(gui->mode&INPUTINSERT)\n" "   highlight_duration(gui, %d);\n" " else \n" " if( !(gui->mode&INPUTRHYTHM) && (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))\n" "   ChangeDur%d (action, param);\n" "else {\n" " insert_chord_%dkey(gui);\n" "   highlight_duration(gui, %d);\n" "  score_status(gui, TRUE);\n" " displayhelper(gui);\n" " }\n" "}\n", i, i, i, i, i);

      // fprintf(entries, "\n#define NOTECHAR%d \"%s\"\n", i,  NOTECHARS[i]);
      // fprintf(entries, "\n#define RESTCHAR%d \"%s\"\n", i, RESTCHARS[i]);
      fprintf (entries, "\n#define NOTE%d \"%s\"\n", i, NOTECHARS[i]);
      fprintf (entries, "\n#define REST%d \"%s\"\n", i, RESTCHARS[i]);


      /* menu_entries for the mode sensitive duration actions, Dur0,1,2 ... */
      fprintf (entries, "{\"%d\", \"NULL\", NOTE%d, NULL, \"Inserts a note at the cursor with duration \"NOTE%d\", or \\n(if appending) appends this duration\\nIf MIDI-in is active the note will be pitchless (displays yellow, percussion-sounding)\\n - the MIDI keyboard will provide the pitch. Changes prevailing duration.\",\n" "G_CALLBACK (Dur%d)},\n" "{\"Change%d\", \"NULL\", NOTE%d, NULL, \"Change current note to a \"NOTE%d,\n" "G_CALLBACK (ChangeDur%d)},\n"
               //"{\"ChangeRest%d\", NULL, \"Change duration\", NULL, \"Change duration of current rest\",\n"
               // "G_CALLBACK (ChangeRest%d)},\n"
               "{\"Insert%d\", NULL, \"Insert a \"NOTE%d\"\", NULL, \"Inserts a \"NOTE%d\" at cursor position\\nSets prevailing duration to \"NOTE%d,\n"
               "G_CALLBACK (InsertDur%d)},\n" "{\"InsertRest%d\", NULL, \"Insert a \"REST%d\"rest\", NULL, \"Inserts a rest at cursor position\\nSets prevailing duration to \"NOTE%d,\n" "G_CALLBACK (InsertRest%d)},\n" "{\"Set%d\", NULL, \"Set Duration to \"NOTE%d\"\", NULL, \"Sets prevailing duration to \"NOTE%d\" (subsequent notes entered will have this duration)\", \n" "G_CALLBACK (SetDur%d)},\n"
               /* ,i, i, i */ , i
               , i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i);


    }


  for (i = 0; i < 9; i++)
    fprintf (xml, "<menuitem action=\"%d\"/>\n", i);
  for (i = 0; i < 9; i++)
    fprintf (xml, "<menuitem action=\"Change%d\"/>\n", i);
  for (i = 0; i < 9; i++)
    fprintf (xml, "<menuitem action=\"Insert%d\"/>\n", i);
  // for(i=0;i<9;i++) 
  //  fprintf(xml, "<menuitem action=\"ChangeRest%d\"/>\n", i);
  for (i = 0; i < 9; i++)
    fprintf (xml, "<menuitem action=\"InsertRest%d\"/>\n", i);

  for (i = 'A'; i <= 'G'; i++)
    fprintf (xml, "<menuitem action=\"Insert%c\"/>\n", i);
  for (i = 'A'; i <= 'G'; i++)
    fprintf (xml, "<menuitem action=\"ChangeTo%c\"/>\n", i);

  for (i = 0; i < 9; i++)
    fprintf (xml, "<menuitem action=\"Set%d\"/>\n", i);

  /* menu_entries for the mode    note name    */
  for (i = 'A'; i <= 'G'; i++)
    {
      fprintf (entries,
               " {\"Insert%c\", NULL, \"Insert %c\", NULL, \"Inserts note %c before note at cursor\\nCursor determines which octave\\nNote is inserted in the prevailing rhythm\",\n"
               "  G_CALLBACK (Insert%c)},\n"
               " {\"AddNote%c\", NULL, \"Insert %c After\", NULL, \"Inserts note %c after note at cursor\\nCursor determines which octave\\nNote is inserted in the prevailing rhythm\",\n"
               "  G_CALLBACK (AddNote%c)},\n"
               " {\"Add%c\", NULL, \"Add %c to Chord\", NULL, \"Adds note %c to chord at cursor\\nCursor determines which octave\",\n"
               "  G_CALLBACK (Add%c)},\n"
               "  {\"ChangeTo%c\", NULL, \"Change current note to %c\", NULL, \"Changes current note to the %c nearest cursor or (if no current note) inserts the note %c\\nCursor determines which octave\\nNote is inserted in the prevailing rhythm\",\n"
               "   G_CALLBACK (ChangeTo%c)},\n" "  {\"MoveTo%c\", NULL, \"Move cursor to step %c\", NULL, \"Moves the cursor to the %c nearest cursor\\nCurrent cursor position determines which octave.\",\n" "   G_CALLBACK (MoveTo%c)},\n", i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i);

    }

  for (i = 'A'; i <= 'G'; i++)
    {
      fprintf (callbacks, "static void ChangeTo%c(GtkAction *action, gpointer param){\n" "  DenemoGUI *gui = Denemo.gui;\n" "  gboolean appending = gui->si->cursor_appending;\n" "  if(appending)\n" "    movecursorleft(NULL); \n" "  gint mode = gui->mode;\n" "  gui->mode = INPUTEDIT|INPUTNORMAL;\n" "  go_to_%c_key(gui);\n" "  gui->mode = mode;\n" "  if(appending)\n" "    movecursorright(NULL);\n" "  score_status(gui, TRUE);\n" "  displayhelper(gui);\n" "}\n", i, i);
    }


  for (i = 'A'; i <= 'G'; i++)
    {
      fprintf (callbacks, "static void MoveTo%c(GtkAction *action, gpointer param){\n" "  DenemoGUI *gui = Denemo.gui;\n" "  gint mode = gui->mode;\n" "  gui->mode = INPUTCLASSIC|INPUTNORMAL;\n" "  go_to_%c_key(gui);\n" "  gui->mode = mode;\n" "  displayhelper(gui);\n" "}\n", i, i);
    }


  for (i = 'A'; i <= 'G'; i++)
    {

      fprintf (callbacks, "static void Insert%c(GtkAction *action, gpointer param){\n" "  DenemoGUI *gui = Denemo.gui;\n" "  gint mode = gui->mode;\n" "  gui->mode = INPUTINSERT|INPUTNORMAL;\n" "  go_to_%c_key(gui);\n" "  gui->mode = mode;\n" "  score_status(gui, TRUE);\n" "  displayhelper(gui);\n" "}\n", i, i);
    }
  for (i = 'A'; i <= 'G'; i++)
    {
      fprintf (callbacks, "static void AddNote%c(GtkAction *action, gpointer param){\n" "  DenemoGUI *gui = Denemo.gui;\n" "  movecursorright(NULL);\n" "  gint mode = gui->mode;\n" "  gui->mode = INPUTINSERT|INPUTNORMAL;\n" "  go_to_%c_key(gui);\n" "  gui->mode = mode;\n" "  movecursorleft(NULL);\n" "  score_status(gui, TRUE);\n" "  displayhelper(gui);\n" "}\n", i, i);
    }

  for (i = 'A'; i <= 'G'; i++)
    {

      fprintf (callbacks, "static void Add%c(GtkAction *action, gpointer param){\n" "  DenemoGUI *gui = Denemo.gui;\n" "  gint mode = gui->mode;\n" "  gui->mode = INPUTCLASSIC|INPUTNORMAL;\n" "  go_to_%c_key(gui);\n" "  add_tone_key(gui);\n" "  gui->mode = mode;\n" "  score_status(gui, TRUE);\n" "  displayhelper(gui);\n" "}\n", i, i);
    }

  for (i = 'A'; i <= 'G'; i++)
    {
      fprintf (register_commands, "register_command(Denemo.map, gtk_action_group_get_action(action_group, \"Insert%c\"), \"Insert%c\", _(\"Insert %c\"),_(\"Inserts note %c before note at cursor\\nCursor determines which octave\\nNote is inserted in the prevailing rhythm\"),  Insert%c);\n", i, i, i, i, i);
      fprintf (scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.map, \"Insert%c\")), \"scm\", (gpointer)1);\n", i);    //define a property "scm" on the action to mean scheme can call the action.
      fprintf (scheme, "SCM scheme_Insert%c(SCM optional);\ninstall_scm_function (\"d-Insert%c\", scheme_Insert%c);\n", i, i, i);       // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_Insert%c (SCM optional) {\nInsert%c (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);
      fprintf (register_commands, "register_command(Denemo.map, gtk_action_group_get_action(action_group, \"AddNote%c\"), \"AddNote%c\", _(\"Insert %c After\"),_(\"Inserts note %c after note at cursor\\nCursor determines which octave\\nNote is inserted in the prevailing rhythm\"),  AddNote%c);\n", i, i, i, i, i);
      fprintf (scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.map, \"AddNote%c\")), \"scm\", (gpointer)1);\n", i);   //define a property "scm" on the action to mean scheme can call the action.
      fprintf (scheme, "SCM scheme_AddNote%c(SCM optional);\ninstall_scm_function (\"d-AddNote%c\", scheme_AddNote%c);\n", i, i, i);    // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_AddNote%c (SCM optional) {\nAddNote%c (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);


      fprintf (register_commands, "register_command(Denemo.map, gtk_action_group_get_action(action_group, \"Add%c\"), \"Add%c\", _(\"Add %c\"),_(\"Adds note %c to the chord at cursor\\nCursor height determines which octave\"),  Add%c);\n", i, i, i, i, i);
      fprintf (scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.map, \"Add%c\")), \"scm\", (gpointer)1);\n", i);       //define a property "scm" on the action to mean scheme can call the action.
      fprintf (scheme, "SCM scheme_Add%c(SCM optional);\ninstall_scm_function (\"d-Add%c\", scheme_Add%c);\n", i, i, i);        // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_Add%c (SCM optional) {\nAdd%c (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);



      fprintf (register_commands, "register_command(Denemo.map, gtk_action_group_get_action(action_group, \"ChangeTo%c\"), \"ChangeTo%c\", _(\"Change to %c\"),_(\"Changes note at cursor to nearest note %c\\nRhythm is unchanged\"),  ChangeTo%c);\n", i, i, i, i, i);
      fprintf (scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.map, \"ChangeTo%c\")), \"scm\", (gpointer)1);\n", i);  //define a property "scm" on the action to mean scheme can call the action.
      fprintf (scheme, "SCM scheme_ChangeTo%c(SCM optional);\ninstall_scm_function (\"d-ChangeTo%c\", scheme_ChangeTo%c);\n", i, i, i); // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_ChangeTo%c (SCM optional) {\nChangeTo%c (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);


      fprintf (register_commands, "register_command(Denemo.map, gtk_action_group_get_action(action_group, \"MoveTo%c\"), \"MoveTo%c\", _(\"Move to %c\"),_(\"Moves cursor to nearest note %c\"),  MoveTo%c);\n", i, i, i, i, i);
      fprintf (scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.map, \"MoveTo%c\")), \"scm\", (gpointer)1);\n", i);    //define a property "scm" on the action to mean scheme can call the action.
      fprintf (scheme, "SCM scheme_MoveTo%c(SCM optional);\ninstall_scm_function (\"d-MoveTo%c\", scheme_MoveTo%c);\n", i, i, i);       // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_MoveTo%c (SCM optional) {\nMoveTo%c (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);

    }



  for (i = 0; i < 9; i++)
    {
      /* registering commands for mode independent duration actions InsertRest0,1,2... ChangeRest0,1,2... InsertDur,ChangeDur0,1,2...
       *
       * !!! FIXME what is ChangeRestn???? seems to be Changen ... now dropped. */

      fprintf (register_commands, "register_command(Denemo.map, gtk_action_group_get_action(action_group, \"%d\"), \"%d\", _(\"Insert/Append a %s\"), _(\"When appending, appends a %s \\nWith the cursor on a note inserts a %s  before the current note\\nIf MIDI-in is active, the note will be pitchless (displays yellow, percussion-sounding)\\n - the MIDI keyboard will provide the pitch. Changes prevailing duration.\"), Dur%d);\n", i, i, NOTECHARS[i], NOTECHARS[i], NOTECHARS[i], i);

      fprintf (register_commands, "register_command(Denemo.map, gtk_action_group_get_action(action_group, \"Change%d\"), \"Change%d\", _(\"Change to %s\"), _(\"Change the current note to a %s\"), ChangeDur%d);\n", i, i, NOTECHARS[i], NOTECHARS[i], i);

      fprintf (register_commands, "register_command(Denemo.map, gtk_action_group_get_action(action_group, \"Insert%d\"), \"Insert%d\", _(\"%s\"), _(\"Insert a %s\"), InsertDur%d);\n", i, i, NOTECHARS[i], NOTECHARS[i], i);

      fprintf (register_commands, "register_command(Denemo.map, gtk_action_group_get_action(action_group, \"InsertRest%d\"), \"InsertRest%d\",  _(\"Insert a %s\") ,  _(\"Inserts a rest at cursor position\\nSets prevailing rhythm to %s\"), InsertRest%d);\n", i, i, RESTCHARS[i], NOTECHARS[i], i);

      //  fprintf(register_commands, 
      //    "register_command(Denemo.map, gtk_action_group_get_action(action_group, \"ChangeRest%d\"), \"ChangeRest%d\",  _(\"Change a %s\") ,  _(\"Changes a rest at cursor position\\nSets prevailing rhythm to %s\"), ChangeRest%d);\n", i, i, RESTCHARS[i], NOTECHARS[i], i);

      fprintf (register_commands, "register_command(Denemo.map, gtk_action_group_get_action(action_group, \"Set%d\"), \"Set%d\", _(\"Set Prevailing Duration to %s\"), _(\"Set the prevailing duration to %s (subsequent notes entered will have this duration)\"), SetDur%d);\n", i, i, NOTECHARS[i], NOTECHARS[i], i);

      fprintf (register_commands, "\n#undef NOTE%d\n", i);
      fprintf (register_commands, "\n#undef REST%d\n", i);
      fprintf (register_commands, "\n#undef NOTECHAR%d\n", i);
      fprintf (register_commands, "\n#undef RESTCHAR%d\n", i);

      fprintf (scheme, "/*%d */\n", i);
      fprintf (scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.map, \"%d\")), \"scm\", (gpointer)1);\n", i);  //define a property "scm" on the action to mean scheme can call the action.

      fprintf (scheme, "SCM scheme_%d(SCM optional);\ninstall_scm_function (\"d-%d\", scheme_%d);\n", i, i, i); // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_%d (SCM optional) {\nDur%d (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);

      fprintf (scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.map, \"Insert%d\")), \"scm\", (gpointer)1);\n", i);    //define a property "scm" on the action to mean scheme can call the action.
      fprintf (scheme, "SCM scheme_InsertDur%d(SCM optional);\ninstall_scm_function (\"d-Insert%d\", scheme_InsertDur%d);\n", i, i, i); // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_InsertDur%d (SCM optional) {\nInsertDur%d (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);



      fprintf (scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.map, \"Change%d\")), \"scm\", (gpointer)1);\n", i);    //define a property "scm" on the action to mean scheme can call the action.
      fprintf (scheme, "SCM scheme_ChangeDur%d(SCM optional);\ninstall_scm_function (\"d-Change%d\", scheme_ChangeDur%d);\n", i, i, i); // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_ChangeDur%d (SCM optional) {\nChangeDur%d (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);

      fprintf (scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.map, \"Set%d\")), \"scm\", (gpointer)1);\n", i);       //define a property "scm" on the action to mean scheme can call the action.
      fprintf (scheme, "SCM scheme_SetDur%d(SCM optional);\ninstall_scm_function (\"d-Set%d\", scheme_SetDur%d);\n", i, i, i);  // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_SetDur%d (SCM optional) {\nSetDur%d (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);



      fprintf (scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.map, \"InsertRest%d\")), \"scm\", (gpointer)1);\n", i);        //define a property "scm" on the action to mean scheme can call the action.
      fprintf (scheme, "SCM scheme_InsertRest%d(SCM optional);\ninstall_scm_function (\"d-InsertRest%d\", scheme_InsertRest%d);\n", i, i, i);   // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_InsertRest%d (SCM optional) {\nInsertRest%d (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);

      //fprintf(scheme, "g_object_set_data(G_OBJECT(action_of_name(Denemo.map, \"ChangeRest%d\")), \"scm\", (gpointer)1);\n", i); //define a property "scm" on the action to mean scheme can call the action.
      // fprintf(scheme, "SCM scheme_ChangeRest%d(SCM optional);\ninstall_scm_function (\"d-ChangeRest%d\", scheme_ChangeRest%d);\n", i, i, i);// for direct callback via (scheme_xxx)
      // fprintf(scheme_cb, "SCM scheme_ChangeRest%d (SCM optional) {\nChangeRest%d (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i,  i);
    }



#ifdef GENERATE_XML_FRAGMENT
  fprintf (xml, "<menu action=\"AllOther\">\n");
  for (j = 0; j < 9; j++)
    {
      fprintf (xml, "<menu action=\"%s\">\n", catname[j]);
      for (i = 0; i < n_denemo_commands; i++)
        {
          if (mi == j)
            {
              fprintf (xml, "<menuitem action=\"%s\"/>\n", ni);
            }

        }
      fprintf (xml, "</menu>\n");
    }
  fprintf (xml, "</menu>\n");
#endif
  return 0;
}
