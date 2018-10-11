#include <libguile.h>
#include "scripting/scheme-identifiers.h"
#include "scripting/scheme-callbacks.h"
#include "core/view.h"

#ifdef DEVELOPER
static FILE *DEV_fp;
#define DEV_CODE  gint idx = lookup_command_from_name(Denemo.map, name+strlen(DENEMO_SCHEME_PREFIX));\
  gchar *tooltip =  (idx<0)? "To be documented":(gchar*)lookup_tooltip_from_idx(Denemo.map, idx);\
  if(!DEV_fp) DEV_fp = fopen("functions.xml", "w");
#endif

void
install_scm_function (gint nbargs, gchar* tooltip, gchar * name, gpointer callback)
{
#ifdef DEVELOPER
  DEV_CODE;
  if (DEV_fp)
    fprintf (DEV_fp, "<listitem>%s %i parameter: %s </listitem>\n", name, nbargs, tooltip);
#endif
  switch(nbargs){
    case 0:
      scm_c_define_gsubr (name, 0, 1, 0, callback);
      break;
    case 1:
      scm_c_define_gsubr (name, 1, 1, 0, callback);
      break;
    case 2:
      scm_c_define_gsubr (name, 2, 0, 0, callback);
      break;
    default:
      scm_c_define_gsubr (name, 0, nbargs, 0, callback);//use 3+ for more arguments, they will all be optional
      break;
  }

  gchar* helpname = g_strconcat("Help-", name, NULL);
  define_scheme_variable(helpname, tooltip, "Value is the help string of the variable");
  g_free(helpname);
}

#undef DEV_CODE

void
create_scheme_identfiers (void)
{

  /* test with
     (d-EditMode)
     (d-2)
     (d-PutNoteName "cis''")
   */

  /* create scheme functions d-<name> for all the menuitem callbacks of <name> that are not check/radio items
     The scheme functions are defined to take one optional parameter which by denemo convention will be a String type,
     not necessarily null terminated, which is then passed as a GString * to the callback routines (with the first parameter, the DenemoAction*, passed as NULL.
   */
#include "scripting/scheme.h"
  init_denemo_notenames ();

  install_scm_function (0, "Hides all the menus", DENEMO_SCHEME_PREFIX "HideMenus", scheme_hide_menus);
  install_scm_function (0, "Hides Score buttons or shows them if passed #f", DENEMO_SCHEME_PREFIX "HideButtons", scheme_hide_buttons);
  install_scm_function (0, "Removes Score buttons", DENEMO_SCHEME_PREFIX "DestroyButtons", scheme_destroy_buttons);
  install_scm_function (0, "Hides the Denemo.project or shows it if passed #f", DENEMO_SCHEME_PREFIX "HideWindow", scheme_hide_window);

  install_scm_function (1, "Takes the the name of a scripted command. Runs the script stored for that command. Scripts which invoke other scripted commands use this (implicitly?) ", DENEMO_SCHEME_PREFIX "ScriptCallback", scheme_script_callback);

  install_scm_function (1, "create a dialog with the options (null separated string) & return the one chosen, of #f if the user cancels, takes optional title", DENEMO_SCHEME_PREFIX "GetOption", scheme_get_option);
  /* test with (display (d-GetOption "this\0and\0that\0")) */
  install_scm_function (0, "Returns the text on the clipboard", DENEMO_SCHEME_PREFIX "GetTextSelection", scheme_get_text_selection);
  install_scm_function (0, "Returns the padding that has been set by dragging in the Print view window", DENEMO_SCHEME_PREFIX "GetPadding", scheme_get_padding);
  install_scm_function (0, "Deprecated - gets an integer from the user via a dialog", DENEMO_SCHEME_PREFIX "GetRelativeFontSize", scheme_get_relative_font_size);
  /* install the scheme functions for calling extra Denemo functions created for the scripting interface */
  install_scm_function (1, "Takes a command name. called by a script if it requires initialization the initialization script is expected to be in init.scm in the menupath of the command passed in.", DENEMO_SCHEME_PREFIX "InitializeScript", scheme_initialize_script);
  install_scm_function (1, " pass in a path (from below menus) to a command script. Loads the command from .denemo or system if it can be found. It is used at startup in .denemo files like ReadingNoteNames.denemo which executes (d-LoadCommand \"MainMenu/Educational/ReadingNoteNames\") to ensure that the command it needs is in the command set.", DENEMO_SCHEME_PREFIX "LoadCommand", scheme_load_command);

  install_scm_function (1, "Takes a string, a menu path (from below menus). It executes the command for that menu item. Returns #f for no menu item.", DENEMO_SCHEME_PREFIX "ActivateMenuItem", scheme_activate_menu_item);


  install_scm_function (0, "Returns the directory holding the user's preferences", DENEMO_SCHEME_PREFIX "LocateDotDenemo", scheme_locate_dotdenemo);
  install_scm_function (0, "Returns the name of the type of object at the cursor, or None if none. If not passed #f returns Appending if cursor is in appending after an object.", DENEMO_SCHEME_PREFIX "GetType", scheme_get_type);
  install_scm_function (0, "Returns the lilypond typesetting text for object at the cursor or #f if the object has not yet been typeset", DENEMO_SCHEME_PREFIX "GetLilyPond", scheme_get_lilypond);
  install_scm_function (0, "Re-computes the LilyPond output for the current score layout", DENEMO_SCHEME_PREFIX "RefreshLilyPond", scheme_refresh_lilypond);

  install_scm_function (0, "Returns a string numerator/denominator for a tuplet open object or #f if cursor not on a tuplet open", DENEMO_SCHEME_PREFIX "GetTuplet", scheme_get_tuplet);
  install_scm_function (0, "Set passed string as numerator/denominator for a tuplet open at cursor", DENEMO_SCHEME_PREFIX "SetTuplet", scheme_set_tuplet);

  install_scm_function (0, "Set passed 24 bit number as RGB color of background.", DENEMO_SCHEME_PREFIX "SetBackground", scheme_set_background);

  install_scm_function (2, "Takes a staff number m and a object number n. Returns the type of object at the (m, n)th position on the Denemo Clipboard or #f if none.", DENEMO_SCHEME_PREFIX "GetClipObjType", scheme_get_clip_obj_type);
  install_scm_function (1, "Takes a staff number m, Returns the number of objects in the mth staff on the Denemo Clipboard or #f if none.", DENEMO_SCHEME_PREFIX "GetClipObjects", scheme_get_clip_objects);


  install_scm_function (2, "Takes a staff number m and a object number n. Inserts the (m, n)th Denemo Object from Denemo Clipboard into the staff at the cursor position", DENEMO_SCHEME_PREFIX "PutClipObj", scheme_put_clip_obj);
  install_scm_function (0, "Clears the Denemo Music Clipboard", DENEMO_SCHEME_PREFIX "ClearClipboard", scheme_clear_clipboard);
  install_scm_function (0, "Gives the number of staffs in the Denemo Music Clipboard", DENEMO_SCHEME_PREFIX "GetStaffsInClipboard", scheme_get_staffs_in_clipboard);

  install_scm_function (0, "Gives the number of measures in the current staff", DENEMO_SCHEME_PREFIX "GetMeasuresInStaff", scheme_get_measures_in_staff);
  install_scm_function (0, "Lessens the display height for the current staff", DENEMO_SCHEME_PREFIX "ShortenStaffHeight", scheme_shorten_staff_height);
  install_scm_function (0, "Copies the staff properties from the staff above to the current staff", DENEMO_SCHEME_PREFIX "InheritStaffProperties", scheme_inherit_staff_properties);
  install_scm_function (0, "Sets the number of lines for the current staff", DENEMO_SCHEME_PREFIX "SetLinesInStaff", scheme_set_lines_in_staff);
  install_scm_function (0, "Sets the highest note playable for the current staff", DENEMO_SCHEME_PREFIX "SetStaffRangeHi", scheme_set_staff_range_hi);
  install_scm_function (0, "Sets the lowest note playable for the current staff", DENEMO_SCHEME_PREFIX "SetStaffRangeLo", scheme_set_staff_range_lo);
  install_scm_function (0, "Sets the range of notes playable for the current staff from the chord at the cursor.", DENEMO_SCHEME_PREFIX "SetStaffRange", scheme_set_staff_range);
  install_scm_function (0, "Sets the display color for the current staff", DENEMO_SCHEME_PREFIX "SetColorOfStaff", scheme_set_color_of_staff);
  install_scm_function (0, "Gives the number of staffs in the current movement", DENEMO_SCHEME_PREFIX "GetStaffsInMovement", scheme_get_staffs_in_movement);
  install_scm_function (0, "Gives the number of movements in the current score", DENEMO_SCHEME_PREFIX "GetMovementsInScore", scheme_get_movements_in_score);

  install_scm_function (0, "Makes the current staff a voice belonging to the staff above", DENEMO_SCHEME_PREFIX "StaffToVoice", scheme_staff_to_voice);

  install_scm_function (0, "Makes the current voice a independent staff", DENEMO_SCHEME_PREFIX "VoiceToStaff", scheme_voice_to_staff);
  install_scm_function (0, "Returns #f if the current staff is not a voice else true", DENEMO_SCHEME_PREFIX "IsVoice", scheme_is_voice);

  install_scm_function (0, "Adjusts the horizontal (x-) positioning of notes etc after paste", DENEMO_SCHEME_PREFIX "AdjustXes", scheme_adjust_xes);

  install_scm_function (0, "Turn highlighting of cursor off/on returning #t, or given a boolean parameter sets the highlighting returning the previous value", DENEMO_SCHEME_PREFIX "HighlightCursor", scheme_highlight_cursor);

  install_scm_function (0, "Returns #t if there is an object at the cursor which has any printing behavior it may have overridden", DENEMO_SCHEME_PREFIX "GetNonprinting", scheme_get_nonprinting);

  install_scm_function (0, "Sets the Non Printing attribute of a chord (or note/rest) at the cursor. For a rest this makes a non printing rest, for a note it makes it ia pure rhythm (which will not print, but can be assigned pitch, e.g. via a MIDI keyboard. Pass in #f to unset the attribute", DENEMO_SCHEME_PREFIX "SetNonprinting", scheme_set_nonprinting);

  install_scm_function (0, "Returns #t if there is a grace note/chord at cursor, else #f", DENEMO_SCHEME_PREFIX "IsGrace", scheme_is_grace);
  install_scm_function (0, "Returns #t if there is a tied note/chord at cursor, else #f", DENEMO_SCHEME_PREFIX "IsTied", scheme_is_tied);

  install_scm_function (0, "Returns #t if there is a chord with slur starting at cursor, else #f", DENEMO_SCHEME_PREFIX "IsSlurStart", scheme_is_slur_start);

  install_scm_function (0, "Returns #t if there is a chord with slur ending at cursor, else #f", DENEMO_SCHEME_PREFIX "IsSlurEnd", scheme_is_slur_end);

  install_scm_function (0, "Returns #t if there is a chord with crescendo starting at cursor, else #f", DENEMO_SCHEME_PREFIX "IsCrescStart", scheme_is_cresc_start);
  install_scm_function (0, "Returns #t if there is a chord with crescendo ending at cursor, else #f", DENEMO_SCHEME_PREFIX "IsCrescEnd", scheme_is_cresc_end);

  install_scm_function (0, "Returns #t if there is a chord with diminuendo starting at cursor, else #f", DENEMO_SCHEME_PREFIX "IsDimStart", scheme_is_dim_start);
  install_scm_function (0, "Returns #t if there is a chord with diminuendo ending at cursor, else #f", DENEMO_SCHEME_PREFIX "IsDimEnd", scheme_is_dim_end);



  install_scm_function (0, "Returns #t if the cursor is in the selection area, else #f", DENEMO_SCHEME_PREFIX "IsInSelection", scheme_is_in_selection);
  install_scm_function (0, "Returns #t if there is a selection, else #f", DENEMO_SCHEME_PREFIX "HasSelection", scheme_has_selection);

  install_scm_function (0, "Returns #t if the cursor is in the appending position, else #f", DENEMO_SCHEME_PREFIX "IsAppending", scheme_is_appending);

  install_scm_function (0, "Shifts the cursor up or down by the integer amount passed in", DENEMO_SCHEME_PREFIX "ShiftCursor", scheme_shift_cursor);


  install_scm_function (0, "Returns the Roman Numeral corresponding to the passed string", DENEMO_SCHEME_PREFIX "RomanNumeral", scheme_roman_numeral);



  install_scm_function (0, "Returns the movement number counting from 1", DENEMO_SCHEME_PREFIX "GetMovement", scheme_get_movement);
  install_scm_function (0, "Returns the LilyPond identifier for the current voice", DENEMO_SCHEME_PREFIX "GetVoiceIdentifier", scheme_get_voice_identifier);
  install_scm_function (0, "Returns the staff/voice number counting from 1", DENEMO_SCHEME_PREFIX "GetStaff", scheme_get_staff);
  install_scm_function (0, "With parameter #t or #f makes the staff hidden/visible in the display, returns the hidden status. Typesetting is unaffected", DENEMO_SCHEME_PREFIX "StaffHidden", scheme_staff_hidden);
  install_scm_function (0, "Returns the measure number counting from 1", DENEMO_SCHEME_PREFIX "GetMeasure", scheme_get_measure);
  install_scm_function (0, "Sets the display width of the object at the cursor to the value passed (in pixels)", DENEMO_SCHEME_PREFIX "SetObjectDisplayWidth", scheme_set_object_display_width);
  install_scm_function (0, "Returns the cursor horizontal position in current measure.\n 1 = first position in measure, n+1 is appending position where n is the number of objects in current measure", DENEMO_SCHEME_PREFIX "GetHorizontalPosition", scheme_get_horizontal_position);

  install_scm_function (0, "Returns the note name for the line or space where the cursor is", DENEMO_SCHEME_PREFIX "GetCursorNote", scheme_get_cursor_note);
  install_scm_function (0, "Returns the note name and octave in LilyPond notation for the line or space where the cursor is", DENEMO_SCHEME_PREFIX "GetCursorNoteWithOctave", scheme_get_cursor_note_with_octave);


  install_scm_function (0, "Prints out information about the object at the cursor", DENEMO_SCHEME_PREFIX "DebugObject", scheme_debug_object);
  install_scm_function (0, "Swaps the positions of the Denemo Display and the Denemo Source view", DENEMO_SCHEME_PREFIX "SwapDisplayAndSource", scheme_swap_display_and_source);
  install_scm_function (0, "Displays information about the object at the cursor position.", DENEMO_SCHEME_PREFIX "DisplayObject", scheme_display_object);
  install_scm_function (0, "Prints out the cumulative time spent editing this score.\nThe time counts any period between starting to edit and saving to disk\nThe time is accumulated over different editing sessions.", DENEMO_SCHEME_PREFIX "GetEditingTime", scheme_get_editing_time);
  install_scm_function (0, "Remove the user's customized buttons and other scheme startup stuff created by the user in actions/denemo.scm", DENEMO_SCHEME_PREFIX "DestroySchemeInit", scheme_destroy_scheme_init);



  install_scm_function (3, "Takes a number 1 or 2 and initial value and a message. Returns one or two note-names in LilyPond format or #f if the user cancels", DENEMO_SCHEME_PREFIX "GetNoteNamesFromUser", scheme_get_note_names_from_user);
  install_scm_function (3, "Takes a title, instruction and initial value. Returns user input #f if the user cancels", DENEMO_SCHEME_PREFIX "GetMultilineUserInput", scheme_get_multiline_user_input);
  install_scm_function (5, "Takes a title, instruction, prior lilypond context, post lilypond context and intial value. Returns markup from user which is checked in the given contexts #f if the user cancels", DENEMO_SCHEME_PREFIX "GetLilyPondSyntaxFromUser", scheme_get_lilypond_syntax_from_user);

  install_scm_function (0, "Returns the name of the (highest) note in any chord at the cursor position, or #f if none", DENEMO_SCHEME_PREFIX "GetNoteName", scheme_get_note_name);
  install_scm_function (0, "Insert a rest at the cursor in the prevailing duration, or if given a integer, in that duration, setting the prevailing duration. If MIDI in is active, the cursor stays on the rest after inserting it, else it moves right.", DENEMO_SCHEME_PREFIX "InsertRest", scheme_insert_rest);
  install_scm_function (1, "Takes a space separated list of notes in LilyPond syntax and inserts/appends a chord at the cursor in the prevailing duration, or if given a integer, in that duration, not setting the prevailing duration.", DENEMO_SCHEME_PREFIX "InsertChord", scheme_insert_chord);
  install_scm_function (0, "Insert rests at the cursor to the value of the one whole measure in the key signature and return the number of rests inserted", DENEMO_SCHEME_PREFIX "PutWholeMeasureRests", scheme_put_whole_measure_rests);
  install_scm_function (0, "Takes optional integer parameter n = 1..., returns LilyPond representation of the nth note of the chord at the cursor counting from the lowest, or #f if none", DENEMO_SCHEME_PREFIX "GetNote", scheme_get_note);
  install_scm_function (0, "Takes optional integer parameter n = 1..., returns the number of diatonic steps above the center-line of the staff of the  nth note (counting from the lowest) of the chord at the cursor, or #f if none", DENEMO_SCHEME_PREFIX "GetNoteStaffPosition", scheme_get_note_staff_position);
  install_scm_function (0, "Takes optional integer parameter n = 1..., returns LilyPond representation of the nth note of the chord at the cursor counting from the highest, or #f if none", DENEMO_SCHEME_PREFIX "GetNoteFromTop", scheme_get_note_from_top);
  install_scm_function (0, "Takes optional integer parameter n = 1..., returns MIDI key for the nth note of the chord at the cursor counting from the highest, or #f if none", DENEMO_SCHEME_PREFIX "GetNoteFromTopAsMidi", scheme_get_note_from_top_as_midi);
  install_scm_function (0, "Returns a space separated string of LilyPond notes for the chord at the cursor position or #f if none", DENEMO_SCHEME_PREFIX "GetNotes", scheme_get_notes);
  install_scm_function (0, "Returns LilyPond note at the cursor position or #f if none", DENEMO_SCHEME_PREFIX "GetNoteAtCursor", scheme_get_note_at_cursor);
  install_scm_function (0, "Returns the number of dots on the note at the cursor, or #f if no note", DENEMO_SCHEME_PREFIX "GetDots", scheme_get_dots);
  install_scm_function (0, "Returns the base duration of the note at the cursor number=0, 1, 2 for whole half quarter note etc, or #f if none", DENEMO_SCHEME_PREFIX "GetNoteBaseDuration", scheme_get_note_base_duration);
  install_scm_function (0, "Returns the duration in LilyPond syntax of the note at the cursor, or #f if none", DENEMO_SCHEME_PREFIX "GetNoteDuration", scheme_get_note_duration);

  install_scm_function (1, "Takes an integer, Sets the number of ticks (PPQN) for the object at the cursor, returns #f if none; if the object is a chord it is set undotted", DENEMO_SCHEME_PREFIX "SetDurationInTicks", scheme_set_duration_in_ticks);
  install_scm_function (1, "Takes an index, returns the time in seconds, time signature and tempo in seconds per quarter note of the index'th MIDI tempo event in the recorded MIDI stream.", DENEMO_SCHEME_PREFIX "GetRecordedMidiTempo", scheme_get_recorded_midi_tempo);
  install_scm_function (1, "Takes an track number 1,2 ..., makes that MIDI track of the loaded MIDI stream the current recorded track.", DENEMO_SCHEME_PREFIX "GetImportedMidiTrack", scheme_get_imported_midi_track);
  install_scm_function (0, "Delete the current imported/recorded MIDI track fails if playing, returning #f.", DENEMO_SCHEME_PREFIX "DeleteImportedMidi", scheme_delete_imported_midi);
  install_scm_function (0, "Returns the MIDI track number of the current imported track.", DENEMO_SCHEME_PREFIX "GetCurrentMidiTrack", scheme_get_current_midi_track);
  install_scm_function (0, "Returns the number of MIDI tracks of the loaded/recorded MIDI.", DENEMO_SCHEME_PREFIX "GetImportedMidiTracks", scheme_get_imported_midi_tracks);
  install_scm_function (0, "Returns the duration in seconds of the recorded MIDI track or #f if none", DENEMO_SCHEME_PREFIX "GetRecordedMidiDuration", scheme_get_recorded_midi_duration);

  install_scm_function (0, "Returns the number of ticks (PPQN) for the object at the cursor, or #f if none", DENEMO_SCHEME_PREFIX "GetDurationInTicks", scheme_get_duration_in_ticks);
  install_scm_function (0, "Returns the number of ticks (PPQN) for the chord without dots or tuplet effects at the cursor, or #f if not a chord. The value is -ve for special durations (i.e. non-standard notes)", DENEMO_SCHEME_PREFIX "GetBaseDurationInTicks", scheme_get_base_duration_in_ticks);

  install_scm_function (0, "Returns the tick count (PPQN) for the end of the object at the cursor, or #f if none", DENEMO_SCHEME_PREFIX "GetEndTick", scheme_get_end_tick);
  install_scm_function (0, "Returns the tick count (PPQN) for the start of the object at the cursor, or #f if none", DENEMO_SCHEME_PREFIX "GetStartTick", scheme_get_start_tick);

  install_scm_function (0, "Returns the measure number at cursor position.", DENEMO_SCHEME_PREFIX "GetMeasureNumber", scheme_get_measure_number);
  install_scm_function (0, "Returns the value set on the current measure by which the measure numbers in the Denemo Displayed should be offset from this point on.", DENEMO_SCHEME_PREFIX "GetMeasureNumberOffset", scheme_get_measure_number_offset);
  install_scm_function (1, "Takes a value to set on the current measure. The measure numbers in the Denemo Display will be offset from this point on by this amount. No effect on the typesetting.", DENEMO_SCHEME_PREFIX "SetMeasureNumberOffset", scheme_set_measure_number_offset);


  install_scm_function (0, "Takes LilyPond note name string. Moves the cursor to the line or space", DENEMO_SCHEME_PREFIX "CursorToNote", scheme_cursor_to_note);
  install_scm_function (1, "Takes a number 1 ... n. Moves the cursor to the nth note from the bottom of the chord at the cursor, returning #f if it fails.", DENEMO_SCHEME_PREFIX "CursorToNthNoteHeight", scheme_cursor_to_nth_note_height);
  install_scm_function (0, "If there are two notes at the cursor height it re-orders them and returns #t. If passed 'enquire it does not do the swapping.", DENEMO_SCHEME_PREFIX "SwapNotesAtCursorHeight", scheme_swap_notes_at_cursor_height);
  install_scm_function (0, "Moves the cursor up to the next higher note of the chord at the cursor, returning #f if it fails.", DENEMO_SCHEME_PREFIX "CursorToNextNoteHeight", scheme_cursor_to_next_note_height);

  install_scm_function (0, "Returns the prevailing key signature at the cursor", DENEMO_SCHEME_PREFIX "GetPrevailingKeysig", scheme_get_prevailing_keysig);
  install_scm_function (0, "Returns the name prevailing key signature at the cursor", DENEMO_SCHEME_PREFIX "GetPrevailingKeysigName", scheme_get_prevailing_keysig_name);
  install_scm_function (0, "Returns the prevailing time signature at the cursor", DENEMO_SCHEME_PREFIX "GetPrevailingTimesig", scheme_get_prevailing_timesig);
  install_scm_function (0, "Returns the prevailing clef at the cursor. Note that non-builtin clefs like drum are not handled yet.", DENEMO_SCHEME_PREFIX "GetPrevailingClef", scheme_get_prevailing_clef);

  install_scm_function (0, "Returns the LilyPond typesetting syntax for prevailing clef at the cursor.", DENEMO_SCHEME_PREFIX "GetPrevailingClefAsLilyPond", scheme_get_prevailing_clef_as_lilypond);
  install_scm_function (0, "Returns the LilyPond typesetting syntax for prevailing key signature at the cursor.", DENEMO_SCHEME_PREFIX "GetPrevailingKeysigAsLilyPond", scheme_get_prevailing_keysig_as_lilypond);
  install_scm_function (0, "Returns the LilyPond typesetting syntax for prevailing time signature at the cursor.", DENEMO_SCHEME_PREFIX "GetPrevailingTimesigAsLilyPond", scheme_get_prevailing_timesig_as_lilypond);


  install_scm_function (0, "Returns the prevailing duration, ie duration which will be used for the next inserted note, with a parameter 0 ... 8 sets the prevailing duration.", DENEMO_SCHEME_PREFIX "GetPrevailingDuration", scheme_get_prevailing_duration);

  //more work needed, see above install_scm_function (0, "Sets the prevailing keysignature at the cursor to the string of 7 steps passed. Each step can be -1, 0 or 1",DENEMO_SCHEME_PREFIX"SetPrevailingKeysig", scheme_set_prevailing_keysig);

  install_scm_function (0, "Makes the initial keysig sharper/flatter", DENEMO_SCHEME_PREFIX "IncrementInitialKeysig", scheme_increment_initial_keysig);
  install_scm_function (0, "Makes the keysig sharper/flatter, affects keysig change when cursor is on one or appending after one, otherwise affects initial keysig", DENEMO_SCHEME_PREFIX "IncrementKeysig", scheme_increment_keysig);
  install_scm_function (0, "Appends a new movement without copying staff structure.", DENEMO_SCHEME_PREFIX "AddMovement", scheme_add_movement);



  install_scm_function (0, "Takes a string of LilyPond note names. Replaces the notes of the chord at the cursor with these notes, preserving other attributes", DENEMO_SCHEME_PREFIX "ChangeChordNotes", scheme_change_chord_notes);
  install_scm_function (0, "Takes a LilyPond note name, and changes the note at the cursor to that note", DENEMO_SCHEME_PREFIX "PutNoteName", scheme_put_note_name);
  install_scm_function (0, "Takes a LilyPond note name, changes the note at the cursor to have the accidental passed in either LilyPond string or integer -2..+2. Returns #f if cursor is not on a note position.  ", DENEMO_SCHEME_PREFIX "SetAccidental", scheme_set_accidental);

  install_scm_function (0, "Inserts a rest at the cursor; either passed in duration or if none passed the prevailing duration.", DENEMO_SCHEME_PREFIX "PutRest", scheme_put_rest);
  install_scm_function (0, "Inserts a note at the cursor; either passed in duration or if none passed the prevailing duration.", DENEMO_SCHEME_PREFIX "PutNote", scheme_put_note);



  install_scm_function (0, "Takes a LilyPond note name, and adds that note to the chord", DENEMO_SCHEME_PREFIX "InsertNoteInChord", scheme_insert_note_in_chord);

  install_scm_function (0, "Moves the note at the cursor by the number of diatonic steps passed in", DENEMO_SCHEME_PREFIX "DiatonicShift", scheme_diatonic_shift);
  install_scm_function (0, "Moves the cursor to the object to the right returning #t if this was possible", DENEMO_SCHEME_PREFIX "NextObject", scheme_next_object);
  install_scm_function (0, "Moves the cursor to object to the left returning #t if the cursor moved", DENEMO_SCHEME_PREFIX "PrevObject", scheme_prev_object);
  install_scm_function (0, "Moves the cursor to the next object in the current measure, returning #f if there were no more objects to the left in the current measure", DENEMO_SCHEME_PREFIX "NextObjectInMeasure", scheme_next_object_in_measure);
  install_scm_function (0, "Moves the cursor to the previous object in the current measure, returning #f if the cursor was on the first object", DENEMO_SCHEME_PREFIX "PrevObjectInMeasure", scheme_prev_object_in_measure);
  install_scm_function (0, "Moves the cursor to the next object in the selection. Returns #t if the cursor moved", DENEMO_SCHEME_PREFIX "NextSelectedObject", scheme_next_selected_object);
  install_scm_function (0, "Moves the cursor to the previous object in the selection. Returns #t if the cursor moved", DENEMO_SCHEME_PREFIX "PrevSelectedObject", scheme_prev_selected_object);
  install_scm_function (0, "Moves the cursor the the next object of type CHORD in the current staff. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "NextChord", scheme_next_chord);
  install_scm_function (0, "Moves the cursor the the previous object of type CHORD in the current staff. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "PrevChord", scheme_prev_chord);

  install_scm_function (0, "Moves the cursor the the next object of type CHORD in the current measure. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "NextChordInMeasure", scheme_next_chord_in_measure);
  install_scm_function (0, "Moves the cursor the the previous object of type CHORD in the current measure. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "PrevChordInMeasure", scheme_prev_chord_in_measure);


  install_scm_function (0, "Moves the cursor the next object of type CHORD which is not a rest in the current staff. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "NextNote", scheme_next_note);
  install_scm_function (0, "Moves the cursor the previous object of type CHORD which is not a rest in the current staff. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "PrevNote", scheme_prev_note);

  install_scm_function (0, "Moves the view port (the Denemo Display) to the left, leaving the cursor on the same object as long as it is still in view.", DENEMO_SCHEME_PREFIX "ScrollLeft", scheme_scroll_left);
  install_scm_function (0, "Moves the view port (the Denemo Display) to the right, leaving the cursor on the same object as long as it is still in view.", DENEMO_SCHEME_PREFIX "ScrollRight", scheme_scroll_right);

  install_scm_function (0, "Creates a music Snippet comprising the object at the cursor Returns #f if not possible, otherwise an identifier for that snippet", DENEMO_SCHEME_PREFIX "CreateSnippetFromObject", scheme_create_snippet_from_object);

  install_scm_function (0, "Selects music Snippet from passed id Returns #f if not possible", DENEMO_SCHEME_PREFIX "SelectSnippet", scheme_select_snippet);

  install_scm_function (1, "Inserts music Snippet from passed id Returns #f if not possible, a second boolean parameter determines if the snippet becomes selected. ", DENEMO_SCHEME_PREFIX "InsertSnippet", scheme_insert_snippet);



  install_scm_function (0, "Moves the cursor the next object that is a Denemo Directive in the current staff. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "NextStandaloneDirective", scheme_next_standalone_directive);
  install_scm_function (0, "Moves the cursor the previous object that is a Denemo Directive in the current staff. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "PrevStandaloneDirective", scheme_prev_standalone_directive);
  install_scm_function (0, "Moves the cursor within the current measure to the next object that is a Denemo Directive in the current staff. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "NextStandaloneDirectiveInMeasure", scheme_next_standalone_directive_in_measure);
  install_scm_function (0, "Moves the cursor within the current measure to the previous object that is a Denemo Directive in the current staff. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "PrevStandaloneDirectiveInMeasure", scheme_prev_standalone_directive_in_measure);


  install_scm_function (0, "Enforces the treatment of the note at the cursor as a chord in LilyPond", DENEMO_SCHEME_PREFIX "Chordize", scheme_chordize);
  install_scm_function (0, "Takes xml representation of a preference and adds it to the Denemo preferences", DENEMO_SCHEME_PREFIX "SetPrefs", scheme_set_prefs);

  install_scm_function (0, "Takes string name of a boolean-valued preference and returns the current value. Non-existent prefs return #f, ensure that the preference name is correct before using.", DENEMO_SCHEME_PREFIX "GetBooleanPref", scheme_get_boolean_pref);
  install_scm_function (0, "Takes string name of an int-valued preference and returns the current value. Non-existent prefs return #f", DENEMO_SCHEME_PREFIX "GetIntPref", scheme_get_int_pref);
  install_scm_function (0, "Takes string name of a string-valued preference and returns the current value. Non-existent prefs return #f", DENEMO_SCHEME_PREFIX "GetStringPref", scheme_get_string_pref);

  install_scm_function (0, "Takes a script as a string, which will be stored. All the callbacks are called when the musical score is closed", DENEMO_SCHEME_PREFIX "AttachQuitCallback", scheme_attach_quit_callback);
  install_scm_function (0, "Removes a callback from the current musical score", DENEMO_SCHEME_PREFIX "DetachQuitCallback", scheme_detach_quit_callback);
  install_scm_function (0, "Returns DENEMO_INPUTMIDI, DENEMO_INPUTKEYBOARD, DENEMO_INPUTAUDIO depending on the source of input to Denemo.", DENEMO_SCHEME_PREFIX "GetInputSource", scheme_get_input_source);
  install_scm_function (0, "Pops up a menu given by the list of pairs in the argument. Each pair should be a label string and an expression, the expression for the chosen label is returned. Alternatively the label string can be replaced by a pair of strings, label . tooltip. The third syntax is just a list of string labels, the chosen string is returned.", DENEMO_SCHEME_PREFIX "PopupMenu", scheme_popup_menu);
  install_scm_function (1, "Pops up a dialog of check buttons given by the list of pairs in the argument and optional title argument. Each pair should be a label string and a boolean, the list is returned with booleans as chosen or #f returned of camce;;ed.", DENEMO_SCHEME_PREFIX "CheckBoxes", scheme_check_boxes);
  install_scm_function (0, "Returns a list of the target type and grob (if a directive). Target is set by clicking on the typeset version of the score at a link that LilyPond has inserted.", DENEMO_SCHEME_PREFIX "GetTargetInfo", scheme_get_target_info);
  install_scm_function (0, "Interactively sets a target (a click on a LilyPond link in the printview window) from the user ", DENEMO_SCHEME_PREFIX "GetNewTarget", scheme_get_new_target);
  install_scm_function (0, "Interactively sets a point in the printview window from the user", DENEMO_SCHEME_PREFIX "GetNewPoint", scheme_get_new_point);
  install_scm_function (0, "Interactively sets a reference point (a click on a point in the printview window) from the user showing a cross hairs prompt ", DENEMO_SCHEME_PREFIX "GetReferencePoint", scheme_get_reference_point);
  install_scm_function (0, "Interactively gets an offset from the user in the print view window. The offset is from the last clicked object in the print view window. Returns pair of numbers x is positive to the right, y is positive upwards.", DENEMO_SCHEME_PREFIX "GetOffset", scheme_get_offset);
  install_scm_function (0, "Interactively sets a control point for a curve in the print view window. Takes one parameter the number 1-4 of the control point to set.", DENEMO_SCHEME_PREFIX "GetControlPoint", scheme_get_control_point);
  install_scm_function (0, "Interactively gets a curve from the user in the print view window. Returns a list of pairs of numbers, the control points of the curve.", DENEMO_SCHEME_PREFIX "GetCurve", scheme_get_curve);
  install_scm_function (0, "Interactively gets two positions from the user in the print view window. Returns pair of pairs numbers.", DENEMO_SCHEME_PREFIX "GetPositions", scheme_get_positions);


  install_scm_function (4, "Move to given Movement, voice measure and object position. Takes 4 parameters integers starting from 1, use #f for no change. Returns #f if it fails", DENEMO_SCHEME_PREFIX "GoToPosition", scheme_goto_position);


  install_scm_function (5, "Takes a palette name, label, tooltip and script", DENEMO_SCHEME_PREFIX "CreatePaletteButton", scheme_create_palette_button);
  install_scm_function (4, "Takes a palette name, boolean, and limit", DENEMO_SCHEME_PREFIX "SetPaletteShape", scheme_set_palette_shape);
  install_scm_function (1, "Hides/Un-hides a palette. Pass a palette name (or #t to choose a palette) with second parameter #f hides the palette otherwise show.", DENEMO_SCHEME_PREFIX "ShowPalettes", scheme_show_palettes);
  install_scm_function (0, "Returns the current palette name. The palette status is not changed - it may be hidden. Pass a palette name to become the current palette or pass #t to choose a palette as the current palette.", DENEMO_SCHEME_PREFIX "SelectPalette", scheme_select_palette);
  install_scm_function (0, "Allows the user to type a label to activate a palette button.", DENEMO_SCHEME_PREFIX "ActivatePaletteButton", scheme_activate_palette_button);


  install_scm_function (4, "Takes up to three strings, title, prompt and initial value. Shows these to the user and returns the user's string. Fourth parameter makes the dialog not block waiting for input", DENEMO_SCHEME_PREFIX "GetUserInput", scheme_get_user_input);
  install_scm_function (4, "Takes up to three strings, title, prompt and initial value. Shows these to the user with a text editor for the user to return a string. Buttons are present to insert snippets which are bracketed with section characters. Fourth parameter makes the dialog not block waiting for input. Returns a pair comprising the user's text and formatted LilyPond syntax.", DENEMO_SCHEME_PREFIX "GetUserInputWithSnippets", scheme_get_user_input_with_snippets);
  install_scm_function (0, "Allows the user to select a font returns a string describing the font. Takes an optional title.", DENEMO_SCHEME_PREFIX "SelectFont", scheme_select_font);
  install_scm_function (0, "Allows the user to select a color returns a list of r g b values between 0-255.\nTakes an optional title.", DENEMO_SCHEME_PREFIX "SelectColor", scheme_select_color);
  install_scm_function (0, "Takes a message as a string. Pops up the message for the user to take note of as a warning", DENEMO_SCHEME_PREFIX "WarningDialog", scheme_warningdialog);
  install_scm_function (1, "Takes a message as a string amd a script. Pops up the message for the user to take note of as a informative message, runs the script when dismissed.", DENEMO_SCHEME_PREFIX "InfoWithHook", scheme_info_with_hook);
  install_scm_function (1, "Takes a message as a string and boolean noblock parameter. Pops up the message for the user to take note of as a informative message, blocks if noblock is #f", DENEMO_SCHEME_PREFIX "InfoDialog", scheme_infodialog);
  install_scm_function (0, "Takes a message as a string. Pops up the message inside of a pulsing progressbar", DENEMO_SCHEME_PREFIX "ProgressBar", scheme_progressbar);
  install_scm_function (0, "If running, Stops the ProgressBar.", DENEMO_SCHEME_PREFIX "ProgressBarStop", scheme_progressbar_stop);
  install_scm_function (0, "Typesets the score. Takes a script which will be called when Refresh is performed on the typeset window.", DENEMO_SCHEME_PREFIX "TypesetForScript", scheme_typeset_for_script);
  install_scm_function (0, "Prints from the PDF file generated by TypesetForScript.", DENEMO_SCHEME_PREFIX "PrintTypesetPDF", scheme_print_typeset_pdf);
  install_scm_function (2, "Displays the SVG file generated by LilyPond for playback. Takes a scale and a boolean (true if only the current part is to be typeset)", DENEMO_SCHEME_PREFIX "DisplayTypesetSvg", scheme_display_typeset_svg);
  install_scm_function (0, "Returns #t if continuous typsetting is in operation else #f", DENEMO_SCHEME_PREFIX "ContinuousTypesetting", scheme_continous_typsetting);


  install_scm_function (0, "Intercepts the next keypress and returns a string containing the character. Returns #f if keyboard interception was not possible.", DENEMO_SCHEME_PREFIX "GetChar", scheme_get_char);
  install_scm_function (0, "Intercepts the next keypress and returns a string containing the name of the keypress (the shortcut name). Returns #f if keyboard interception was not possible.  With parameter #f puts the last keypress back to be executed as normal.", DENEMO_SCHEME_PREFIX "GetKeypress", scheme_get_keypress);
  install_scm_function (0, "Returns the last keypress that successfully invoked a command.", DENEMO_SCHEME_PREFIX "GetCommandKeypress", scheme_get_command_keypress);

  install_scm_function (0, "Intercepts the next keypress and returns the name of the command invoked, before invoking the command. Returns #f if the keypress is not a shortcut for any command", DENEMO_SCHEME_PREFIX "GetCommand", scheme_get_command);
  install_scm_function (0, "Intercepts the next keyboard shortcut and returns the name of the command invoked, before invoking the command. Returns #f if the keypress(es) are not a shortcut for any command", DENEMO_SCHEME_PREFIX "GetCommandFromUser", scheme_get_command_from_user);
  install_scm_function (0, "Locks the standalone directive at the cursor so that it runs its delete action when deleted. The tag should be the name of a command that responds to the delete parameter.", DENEMO_SCHEME_PREFIX "LockDirective", scheme_lock_directive);

  install_scm_function (2, "Sets an \"action script\" on the directive of the given tag", DENEMO_SCHEME_PREFIX "SetDirectiveTagActionScript", scheme_set_action_script_for_tag);
  install_scm_function (1, "Inserts a Denemo Directive of the given tag, even if one already exists at the cursor, a pixel width can be passed as well", DENEMO_SCHEME_PREFIX "PutStandaloneDirective", scheme_put_standalone_directive);
  install_scm_function (1, "Changes the tag of the Denemo Directive at the cursor", DENEMO_SCHEME_PREFIX "DirectiveChangeTag", scheme_directive_change_tag);
  install_scm_function (0, "Choose a Denemo Directive at the cursor. Returns the tag or, if the directive is on a chord, a pair whose first element is the tag and second a boolean, true if the directive is on the note at the cursor, false if it is on the chord.", DENEMO_SCHEME_PREFIX "ChooseTagAtCursor", scheme_choose_tag_at_cursor);
  install_scm_function (2, "Takes a tag and a boolean type. Returns a script for cloning the Denemo Directive with the passed tag at the cursor. If type is true the Directive is on a note, no chord.", DENEMO_SCHEME_PREFIX "GetScriptForDirective", scheme_get_script_for_directive);

#define INSTALL_GET_TAG(what)\
  install_scm_function (0, "Takes a optional tag. Returns that tag if a "#what" directive exists at the cursor, else returns the tag of the first such directive at the cursor, or #f if none", DENEMO_SCHEME_PREFIX"DirectiveGetForTag"  "-" #what, scheme_##what##_directive_get_tag);
  INSTALL_GET_TAG (object);
  INSTALL_GET_TAG (standalone);
  INSTALL_GET_TAG (chord);
  INSTALL_GET_TAG (note);
  INSTALL_GET_TAG (staff);
  INSTALL_GET_TAG (voice);
  INSTALL_GET_TAG (score);
  INSTALL_GET_TAG (clef);
  INSTALL_GET_TAG (timesig);
  INSTALL_GET_TAG (tuplet);
  INSTALL_GET_TAG (stemdirective);
  INSTALL_GET_TAG (keysig);
  INSTALL_GET_TAG (scoreheader);
  INSTALL_GET_TAG (header);
  INSTALL_GET_TAG (paper);
  INSTALL_GET_TAG (layout);
  INSTALL_GET_TAG (movementcontrol);
#undef INSTALL_GET_TAG

#define INSTALL_GET_NTH_TAG(what)\
  install_scm_function (1, "Takes a number n. Returns the tag of the nth "#what" directive if it exists else returns #f if none", DENEMO_SCHEME_PREFIX"DirectiveGetNthTag"  "-" #what, scheme_##what##_directive_get_nth_tag);
  INSTALL_GET_NTH_TAG (chord);
  INSTALL_GET_NTH_TAG (note);
  INSTALL_GET_NTH_TAG (staff);
  INSTALL_GET_NTH_TAG (voice);
  INSTALL_GET_NTH_TAG (score);
  INSTALL_GET_NTH_TAG (clef);
  INSTALL_GET_NTH_TAG (timesig);
  INSTALL_GET_NTH_TAG (tuplet);
  INSTALL_GET_NTH_TAG (stemdirective);
  INSTALL_GET_NTH_TAG (keysig);
  INSTALL_GET_NTH_TAG (scoreheader);
  INSTALL_GET_NTH_TAG (header);
  INSTALL_GET_NTH_TAG (paper);
  INSTALL_GET_NTH_TAG (layout);
  INSTALL_GET_NTH_TAG (movementcontrol);
#undef INSTALL_GET_NTH_TAG
#define INSTALL_PRIORITIZE_TAG(what)\
  install_scm_function (1, "Takes a tag. The "#what" directive with that tag if it exists becomes the first to be processed; else returns #f if none", DENEMO_SCHEME_PREFIX"DirectivePrioritizeTag"  "-" #what, scheme_##what##_directive_prioritize_tag);
  INSTALL_PRIORITIZE_TAG (chord);
  INSTALL_PRIORITIZE_TAG (note);
  INSTALL_PRIORITIZE_TAG (staff);
  INSTALL_PRIORITIZE_TAG (voice);
  INSTALL_PRIORITIZE_TAG (score);
  INSTALL_PRIORITIZE_TAG (clef);
  INSTALL_PRIORITIZE_TAG (timesig);
  INSTALL_PRIORITIZE_TAG (tuplet);
  INSTALL_PRIORITIZE_TAG (stemdirective);
  INSTALL_PRIORITIZE_TAG (keysig);
  INSTALL_PRIORITIZE_TAG (scoreheader);
  INSTALL_PRIORITIZE_TAG (header);
  INSTALL_PRIORITIZE_TAG (paper);
  INSTALL_PRIORITIZE_TAG (layout);
  INSTALL_PRIORITIZE_TAG (movementcontrol);
#undef INSTALL_PRIORITIZE_TAG

  install_scm_function (0, "Offers a list of Score-wide or Movement-wide directives for editing\n", DENEMO_SCHEME_PREFIX"EditSystemDirective", scheme_edit_system_directive);
  install_scm_function (2, "Takes a directive type and a tag, displays the text editor for that directive (if any).\n", DENEMO_SCHEME_PREFIX"DisplayDirectiveTextEditor", scheme_display_directive_text_editor);
  install_scm_function (1, "Takes a number n. Returns the tag of the nth note directive if it exists at the cursor height else returns #f if none", DENEMO_SCHEME_PREFIX"DirectiveGetNthTagStrictNote", scheme_directive_get_nth_tag_strict_note);
  install_scm_function (1, "Takes a tag and returns #t if a note directive exists with that tag at the cursor height, with no argument returns the first tag on the note at cursor height, else returns #f", DENEMO_SCHEME_PREFIX"DirectiveGetForTagStrictNote", scheme_directive_get_for_tag_strict_note);



#define INSTALL_EDIT(what)\
  install_scm_function (1, "Deletes a "#what" directive of the passed in tag. Returns #f if not deleted", DENEMO_SCHEME_PREFIX"DirectiveDelete"  "-" #what, scheme_delete_##what##_directive); \
  install_scm_function (1, "Activates a "#what" directive widget of the passed in tag. Returns #f if not a button", DENEMO_SCHEME_PREFIX"DirectiveActivate"  "-" #what, scheme_activate_##what##_directive); \
  install_scm_function (1, "Takes a tag. Lets the user edit (by running the editscript named by the tag) a "#what" directive of the passed in tag. Returns #f if none", DENEMO_SCHEME_PREFIX"DirectiveTextEdit"  "-" #what, scheme_text_edit_##what##_directive);
  INSTALL_EDIT (note);
  INSTALL_EDIT (chord);
  INSTALL_EDIT (staff);
  INSTALL_EDIT (voice);
  INSTALL_EDIT (score);
  install_scm_function (0, "Start a low-level edit of the standalone directive at the cursor", DENEMO_SCHEME_PREFIX "DirectiveTextEdit-standalone", scheme_text_edit_standalone_directive);

  install_scm_function (1, NULL, DENEMO_SCHEME_PREFIX "DirectiveDelete-object", scheme_delete_object_directive);


#define INSTALL_PUT(what, field)\
 install_scm_function (2, "Writes the " #field" field (a string) of the " #what" directive with the passed int tag. Creates the directive of the given type and tag if it does not exist.",DENEMO_SCHEME_PREFIX"DirectivePut" "-" #what "-" #field, scheme_##what##_directive_put_##field);

#define INSTALL_GET(what, field)\
 install_scm_function (1, "Gets the value of the " #field" field (a string) of the " #what" directive with the passed tag.",DENEMO_SCHEME_PREFIX"DirectiveGet" "-" #what "-" #field, scheme_##what##_directive_get_##field);

#define INSTALL_PUT_ALLOW(what)\
 install_scm_function (2, "Takes a tag and a layout id; if there is a directive of the given tag then the directive will be honored when typesetting that layout other layouts will ignore the directive.",DENEMO_SCHEME_PREFIX"DirectivePut" "-" #what "-allow", scheme_##what##_directive_put_allow);
#define INSTALL_PUT_IGNORE(what)\
 install_scm_function (2, "Takes a tag and a layout id; if there is a directive of the given tag then the directive will be ignored when typesetting that layout, other layouts will honor the directive.",DENEMO_SCHEME_PREFIX"DirectivePut" "-" #what "-ignore", scheme_##what##_directive_put_ignore);

  INSTALL_GET (object, minpixels);
  INSTALL_PUT (object, minpixels);

  //block to repeat for new  directive fields

  INSTALL_GET (standalone, minpixels);
  INSTALL_GET (chord, minpixels);
  INSTALL_GET (note, minpixels);
  INSTALL_GET (staff, minpixels);
  INSTALL_GET (voice, minpixels);
  INSTALL_GET (score, minpixels);
  INSTALL_GET (clef, minpixels);
  INSTALL_GET (timesig, minpixels);
  INSTALL_GET (tuplet, minpixels);
  INSTALL_GET (stemdirective, minpixels);
  INSTALL_GET (keysig, minpixels);

  INSTALL_GET (scoreheader, minpixels);
  INSTALL_GET (header, minpixels);
  INSTALL_GET (paper, minpixels);
  INSTALL_GET (layout, minpixels);
  INSTALL_GET (movementcontrol, minpixels);

  INSTALL_PUT (standalone, minpixels);
  INSTALL_PUT (chord, minpixels);
  INSTALL_PUT (note, minpixels);
  INSTALL_PUT (staff, minpixels);
  INSTALL_PUT (voice, minpixels);
  INSTALL_PUT (score, minpixels);
  INSTALL_PUT (clef, minpixels);
  INSTALL_PUT (timesig, minpixels);
  INSTALL_PUT (tuplet, minpixels);
  INSTALL_PUT (stemdirective, minpixels);
  INSTALL_PUT (keysig, minpixels);


  INSTALL_PUT (scoreheader, minpixels);
  INSTALL_PUT (header, minpixels);
  INSTALL_PUT (paper, minpixels);
  INSTALL_PUT (layout, minpixels);
  INSTALL_PUT (movementcontrol, minpixels);

  //end block to repeat for new  directive fields
  INSTALL_GET (standalone, data);
  INSTALL_GET (chord, data);
  INSTALL_GET (note, data);
  INSTALL_GET (staff, data);
  INSTALL_GET (voice, data);
  INSTALL_GET (score, data);
  INSTALL_GET (clef, data);
  INSTALL_GET (timesig, data);
  INSTALL_GET (tuplet, data);
  INSTALL_GET (stemdirective, data);
  INSTALL_GET (keysig, data);

  INSTALL_GET (scoreheader, data);
  INSTALL_GET (header, data);
  INSTALL_GET (paper, data);
  INSTALL_GET (layout, data);
  INSTALL_GET (movementcontrol, data);

  INSTALL_PUT (standalone, data);
  INSTALL_PUT (chord, data);
  INSTALL_PUT (note, data);
  INSTALL_PUT (staff, data);
  INSTALL_PUT (voice, data);
  INSTALL_PUT (score, data);
  INSTALL_PUT (clef, data);
  INSTALL_PUT (timesig, data);
  INSTALL_PUT (tuplet, data);
  INSTALL_PUT (stemdirective, data);
  INSTALL_PUT (keysig, data);


  INSTALL_PUT (scoreheader, data);
  INSTALL_PUT (header, data);
  INSTALL_PUT (paper, data);
  INSTALL_PUT (layout, data);
  INSTALL_PUT (movementcontrol, data);

  INSTALL_GET (standalone, grob);
  INSTALL_GET (standalone, graphic_name);
  INSTALL_GET (chord, graphic_name);
  INSTALL_GET (note, graphic_name);
  INSTALL_GET (clef, graphic_name);
  INSTALL_GET (keysig, graphic_name);
  INSTALL_GET (timesig, graphic_name);
  INSTALL_GET (tuplet, graphic_name);
  INSTALL_GET (chord, grob);
  INSTALL_GET (note, grob);
  INSTALL_GET (staff, grob);
  INSTALL_GET (voice, grob);
  INSTALL_GET (score, grob);
  INSTALL_GET (clef, grob);
  INSTALL_GET (timesig, grob);
  INSTALL_GET (tuplet, grob);
  INSTALL_GET (stemdirective, grob);
  INSTALL_GET (keysig, grob);
  INSTALL_GET (standalone, grob);

  // INSTALL_GET(scoreheader, grob);
  // INSTALL_GET(header, grob);
  // INSTALL_GET(paper, grob);
  // INSTALL_GET(layout, grob);
  // INSTALL_GET(movementcontrol, grob);

  INSTALL_PUT (standalone, grob);
  INSTALL_PUT (chord, grob);
  INSTALL_PUT (note, grob);
  //INSTALL_PUT(staff, grob);
  //INSTALL_PUT(voice, grob);
  INSTALL_PUT (score, grob);
  INSTALL_PUT (clef, grob);
  INSTALL_PUT (timesig, grob);
  INSTALL_PUT (tuplet, grob);
  INSTALL_PUT (stemdirective, grob);
  INSTALL_PUT (keysig, grob);


  // INSTALL_PUT(scoreheader, grob);
  // INSTALL_PUT(header, grob);
  // INSTALL_PUT(paper, grob);
  // INSTALL_PUT(layout, grob);
  // INSTALL_PUT(movementcontrol, grob);
  //end of grob





  INSTALL_GET (standalone, midibytes);
  INSTALL_GET (chord, midibytes);
  INSTALL_GET (note, midibytes);
  INSTALL_GET (keysig, midibytes);
  INSTALL_GET (timesig, midibytes);
  INSTALL_GET (tuplet, midibytes);
  INSTALL_GET (clef, midibytes);
  INSTALL_GET (staff, midibytes);
  INSTALL_GET (voice, midibytes);
  INSTALL_GET (score, midibytes);
  INSTALL_GET (movementcontrol, midibytes);
  INSTALL_PUT (standalone, midibytes);
  INSTALL_PUT (chord, midibytes);
  INSTALL_PUT (note, midibytes);
  INSTALL_PUT (keysig, midibytes);
  INSTALL_PUT (timesig, midibytes);
  INSTALL_PUT (tuplet, midibytes);
  INSTALL_PUT (clef, midibytes);
  INSTALL_PUT (staff, midibytes);
  INSTALL_PUT (voice, midibytes);
  INSTALL_PUT (score, midibytes);
  INSTALL_PUT (movementcontrol, midibytes);





  INSTALL_GET (standalone, override);
  INSTALL_GET (chord, override);
  INSTALL_GET (note, override);
  INSTALL_GET (staff, override);
  INSTALL_GET (voice, override);
  INSTALL_GET (score, override);

  INSTALL_PUT (standalone, override);
  INSTALL_PUT (chord, override);
  INSTALL_PUT (note, override);
  INSTALL_PUT (staff, override);
  INSTALL_PUT (voice, override);
  INSTALL_PUT (score, override);


  //graphic
  INSTALL_PUT (note, graphic);
  //INSTALL_GET(note, graphic);

  INSTALL_PUT (chord, graphic);
  //INSTALL_GET(chord, graphic);

  INSTALL_PUT (standalone, graphic);
  //INSTALL_GET(standalone, graphic);


  INSTALL_PUT (staff, graphic);
  INSTALL_PUT (voice, graphic);

  INSTALL_PUT (score, graphic);
  //graphic



  INSTALL_PUT (chord, display);
  INSTALL_PUT (chord, prefix);
  INSTALL_PUT (chord, postfix);

  INSTALL_GET (chord, display);
  INSTALL_GET (chord, prefix);
  INSTALL_GET (chord, postfix);


  INSTALL_PUT (note, display);
  INSTALL_PUT (note, prefix);
  INSTALL_PUT (note, postfix);

  INSTALL_GET (note, display);
  INSTALL_GET (note, prefix);
  INSTALL_GET (note, postfix);

  INSTALL_PUT (standalone, display);
  INSTALL_PUT (standalone, prefix);
  INSTALL_PUT (standalone, postfix);

  INSTALL_GET (standalone, display);
  INSTALL_GET (standalone, prefix);
  INSTALL_GET (standalone, postfix);


  INSTALL_PUT (staff, display);
  INSTALL_PUT (staff, prefix);
  INSTALL_PUT (staff, postfix);

  INSTALL_GET (staff, display);
  INSTALL_GET (staff, prefix);
  INSTALL_GET (staff, postfix);

  INSTALL_PUT (voice, display);
  INSTALL_PUT (voice, prefix);
  INSTALL_PUT (voice, postfix);

  INSTALL_GET (voice, display);
  INSTALL_GET (voice, prefix);
  INSTALL_GET (voice, postfix);

  INSTALL_PUT (score, display);
  INSTALL_PUT (score, prefix);
  INSTALL_PUT (score, postfix);

  INSTALL_GET (score, display);
  INSTALL_GET (score, prefix);
  INSTALL_GET (score, postfix);


  INSTALL_GET (score, width);
  INSTALL_GET (score, height);



  //INSTALL_GET (score);
  INSTALL_GET (score, gx);
  INSTALL_GET (score, tx);
  INSTALL_PUT_ALLOW (score);
  INSTALL_PUT (score, gx);
  INSTALL_PUT (score, tx);

//  INSTALL_GET (score);
  INSTALL_GET (score, gy);
  INSTALL_GET (score, ty);
  INSTALL_PUT_IGNORE (score);
  INSTALL_PUT (score, gy);
  INSTALL_PUT (score, ty);




  INSTALL_PUT_ALLOW (note);
  //INSTALL_GET (note);
  INSTALL_PUT_ALLOW (chord);
  //INSTALL_GET (chord);
  INSTALL_PUT_IGNORE (note);
  //INSTALL_GET (note);
  INSTALL_PUT_IGNORE (chord);
  //INSTALL_GET (chord);

   INSTALL_PUT_ALLOW (staff);
   INSTALL_PUT_ALLOW (voice);
   INSTALL_PUT_IGNORE (staff);
   INSTALL_PUT_IGNORE (voice);



  INSTALL_PUT (note, tx);
  INSTALL_GET (note, tx);
  INSTALL_PUT (chord, tx);
  INSTALL_GET (chord, tx);
  INSTALL_PUT (note, ty);
  INSTALL_GET (note, ty);
  INSTALL_PUT (chord, ty);
  INSTALL_GET (chord, ty);



  INSTALL_PUT (note, gx);
  INSTALL_GET (note, gx);
  INSTALL_PUT (chord, gx);
  INSTALL_GET (chord, gx);
  INSTALL_PUT (note, gy);
  INSTALL_GET (note, gy);
  INSTALL_PUT (chord, gy);
  INSTALL_GET (chord, gy);


  INSTALL_PUT_ALLOW (standalone);
  //INSTALL_GET (standalone);
  INSTALL_PUT_IGNORE (standalone);
  //INSTALL_GET (standalone);

  INSTALL_PUT (standalone, tx);
  INSTALL_GET (standalone, tx);
  INSTALL_PUT (standalone, ty);
  INSTALL_GET (standalone, ty);

  INSTALL_PUT (standalone, gx);
  INSTALL_GET (standalone, gx);
  INSTALL_PUT (standalone, gy);
  INSTALL_GET (standalone, gy);




  INSTALL_GET (note, width);
  INSTALL_GET (chord, width);
  INSTALL_GET (standalone, width);
  INSTALL_GET (note, height);
  INSTALL_GET (chord, height);
  INSTALL_GET (standalone, height);



  //block to copy for new type of directive
  INSTALL_PUT (clef, display);
  INSTALL_PUT (clef, prefix);
  INSTALL_PUT (clef, postfix);
  INSTALL_PUT (clef, graphic);


  INSTALL_GET (clef, display);
  INSTALL_GET (clef, prefix);
  INSTALL_GET (clef, postfix);

  INSTALL_PUT_ALLOW (clef);
  INSTALL_PUT_IGNORE (clef);
  INSTALL_PUT (clef, tx);
  INSTALL_PUT (clef, ty);
  INSTALL_PUT (clef, gx);
  INSTALL_PUT (clef, gy);
  INSTALL_PUT (clef, override);
  //INSTALL_GET (clef);
  //INSTALL_GET (clef);
  INSTALL_GET (clef, tx);
  INSTALL_GET (clef, ty);
  INSTALL_GET (clef, gx);
  INSTALL_GET (clef, gy);
  INSTALL_GET (clef, override);
  INSTALL_GET (clef, width);
  INSTALL_GET (clef, height);
  INSTALL_EDIT (clef);
  // end of block to copy for new type of directive

  INSTALL_PUT (timesig, display);
  INSTALL_PUT (timesig, prefix);
  INSTALL_PUT (timesig, postfix);
  INSTALL_PUT (timesig, graphic);


  INSTALL_GET (timesig, display);
  INSTALL_GET (timesig, prefix);
  INSTALL_GET (timesig, postfix);

  INSTALL_PUT_ALLOW (timesig);
  INSTALL_PUT_IGNORE (timesig);
  INSTALL_PUT (timesig, tx);
  INSTALL_PUT (timesig, ty);
  INSTALL_PUT (timesig, gx);
  INSTALL_PUT (timesig, gy);
  INSTALL_PUT (timesig, override);
  //INSTALL_GET (timesig);
  //INSTALL_GET (timesig);
  INSTALL_GET (timesig, tx);
  INSTALL_GET (timesig, ty);
  INSTALL_GET (timesig, gx);
  INSTALL_GET (timesig, gy);
  INSTALL_GET (timesig, override);
  INSTALL_GET (timesig, width);
  INSTALL_GET (timesig, height);
  INSTALL_EDIT (timesig);

  INSTALL_PUT (tuplet, display);
  INSTALL_PUT (tuplet, prefix);
  INSTALL_PUT (tuplet, postfix);
  INSTALL_PUT (tuplet, graphic);


  INSTALL_GET (tuplet, display);
  INSTALL_GET (tuplet, prefix);
  INSTALL_GET (tuplet, postfix);

  INSTALL_PUT_ALLOW (tuplet);
  INSTALL_PUT_IGNORE (tuplet);
  INSTALL_PUT (tuplet, tx);
  INSTALL_PUT (tuplet, ty);
  INSTALL_PUT (tuplet, gx);
  INSTALL_PUT (tuplet, gy);
  INSTALL_PUT (tuplet, override);
  //INSTALL_GET (tuplet);
  //INSTALL_GET (tuplet);
  INSTALL_GET (tuplet, tx);
  INSTALL_GET (tuplet, ty);
  INSTALL_GET (tuplet, gx);
  INSTALL_GET (tuplet, gy);
  INSTALL_GET (tuplet, override);
  INSTALL_GET (tuplet, width);
  INSTALL_GET (tuplet, height);
  INSTALL_EDIT (tuplet);

  INSTALL_PUT (stemdirective, display);
  INSTALL_PUT (stemdirective, prefix);
  INSTALL_PUT (stemdirective, postfix);
  INSTALL_PUT (stemdirective, graphic);


  INSTALL_GET (stemdirective, display);
  INSTALL_GET (stemdirective, prefix);
  INSTALL_GET (stemdirective, postfix);

  INSTALL_PUT_ALLOW (stemdirective);
  INSTALL_PUT_IGNORE (stemdirective);
  INSTALL_PUT (stemdirective, tx);
  INSTALL_PUT (stemdirective, ty);
  INSTALL_PUT (stemdirective, gx);
  INSTALL_PUT (stemdirective, gy);
  INSTALL_PUT (stemdirective, override);
  //INSTALL_GET (stemdirective);
  //INSTALL_GET (stemdirective);
  INSTALL_GET (stemdirective, tx);
  INSTALL_GET (stemdirective, ty);
  INSTALL_GET (stemdirective, gx);
  INSTALL_GET (stemdirective, gy);
  INSTALL_GET (stemdirective, override);
  INSTALL_GET (stemdirective, width);
  INSTALL_GET (stemdirective, height);
  INSTALL_EDIT (stemdirective);

  INSTALL_PUT (keysig, display);
  INSTALL_PUT (keysig, prefix);
  INSTALL_PUT (keysig, postfix);
  INSTALL_PUT (keysig, graphic);


  INSTALL_GET (keysig, display);
  INSTALL_GET (keysig, prefix);
  INSTALL_GET (keysig, postfix);

  INSTALL_PUT_ALLOW (keysig);
  INSTALL_PUT_IGNORE (keysig);
  INSTALL_PUT (keysig, tx);
  INSTALL_PUT (keysig, ty);
  INSTALL_PUT (keysig, gx);
  INSTALL_PUT (keysig, gy);
  INSTALL_PUT (keysig, override);
  //INSTALL_GET (keysig);
  //INSTALL_GET (keysig);
  INSTALL_GET (keysig, tx);
  INSTALL_GET (keysig, ty);
  INSTALL_GET (keysig, gx);
  INSTALL_GET (keysig, gy);
  INSTALL_GET (keysig, override);
  INSTALL_GET (keysig, width);
  INSTALL_GET (keysig, height);
  INSTALL_EDIT (keysig);


  INSTALL_PUT (scoreheader, display);
  INSTALL_PUT (scoreheader, prefix);
  INSTALL_PUT (scoreheader, postfix);
  INSTALL_PUT (scoreheader, graphic);


  INSTALL_GET (scoreheader, display);
  INSTALL_GET (scoreheader, prefix);
  INSTALL_GET (scoreheader, postfix);

  INSTALL_PUT_ALLOW (scoreheader);
  INSTALL_PUT_IGNORE (scoreheader);
  INSTALL_PUT (scoreheader, tx);
  INSTALL_PUT (scoreheader, ty);
  INSTALL_PUT (scoreheader, gx);
  INSTALL_PUT (scoreheader, gy);
  INSTALL_PUT (scoreheader, override);
  //INSTALL_GET (scoreheader);
  //INSTALL_GET (scoreheader);
  INSTALL_GET (scoreheader, tx);
  INSTALL_GET (scoreheader, ty);
  INSTALL_GET (scoreheader, gx);
  INSTALL_GET (scoreheader, gy);
  INSTALL_GET (scoreheader, override);
  INSTALL_GET (scoreheader, width);
  INSTALL_GET (scoreheader, height);
  INSTALL_EDIT (scoreheader);


  INSTALL_PUT (header, display);
  INSTALL_PUT (header, prefix);
  INSTALL_PUT (header, postfix);
  INSTALL_PUT (header, graphic);


  INSTALL_GET (header, display);
  INSTALL_GET (header, prefix);
  INSTALL_GET (header, postfix);

  INSTALL_PUT_ALLOW (header);
  INSTALL_PUT_IGNORE (header);
  INSTALL_PUT (header, tx);
  INSTALL_PUT (header, ty);
  INSTALL_PUT (header, gx);
  INSTALL_PUT (header, gy);
  INSTALL_PUT (header, override);
  //INSTALL_GET (header);
  //INSTALL_GET (header);
  INSTALL_GET (header, tx);
  INSTALL_GET (header, ty);
  INSTALL_GET (header, gx);
  INSTALL_GET (header, gy);
  INSTALL_GET (header, override);
  INSTALL_GET (header, width);
  INSTALL_GET (header, height);
  INSTALL_EDIT (header);


  INSTALL_PUT (paper, display);
  INSTALL_PUT (paper, prefix);
  INSTALL_PUT (paper, postfix);
  INSTALL_PUT (paper, graphic);


  INSTALL_GET (paper, display);
  INSTALL_GET (paper, prefix);
  INSTALL_GET (paper, postfix);

  INSTALL_PUT_ALLOW (paper);
  INSTALL_PUT_IGNORE (paper);
  INSTALL_PUT (paper, tx);
  INSTALL_PUT (paper, ty);
  INSTALL_PUT (paper, gx);
  INSTALL_PUT (paper, gy);
  INSTALL_PUT (paper, override);
  //INSTALL_GET (paper);
  //INSTALL_GET (paper);
  INSTALL_GET (paper, tx);
  INSTALL_GET (paper, ty);
  INSTALL_GET (paper, gx);
  INSTALL_GET (paper, gy);
  INSTALL_GET (paper, override);
  INSTALL_GET (paper, width);
  INSTALL_GET (paper, height);
  INSTALL_EDIT (paper);


  INSTALL_PUT (layout, display);
  INSTALL_PUT (layout, prefix);
  INSTALL_PUT (layout, postfix);
  INSTALL_PUT (layout, graphic);


  INSTALL_GET (layout, display);
  INSTALL_GET (layout, prefix);
  INSTALL_GET (layout, postfix);

  INSTALL_PUT_ALLOW (layout);
  INSTALL_PUT_IGNORE (layout);
  INSTALL_PUT (layout, tx);
  INSTALL_PUT (layout, ty);
  INSTALL_PUT (layout, gx);
  INSTALL_PUT (layout, gy);
  INSTALL_PUT (layout, override);
  //INSTALL_GET (layout);
  //INSTALL_GET (layout);
  INSTALL_GET (layout, tx);
  INSTALL_GET (layout, ty);
  INSTALL_GET (layout, gx);
  INSTALL_GET (layout, gy);
  INSTALL_GET (layout, override);
  INSTALL_GET (layout, width);
  INSTALL_GET (layout, height);
  INSTALL_EDIT (layout);

  INSTALL_PUT (movementcontrol, display);
  INSTALL_PUT (movementcontrol, prefix);
  INSTALL_PUT (movementcontrol, postfix);
  INSTALL_PUT (movementcontrol, graphic);


  INSTALL_GET (movementcontrol, display);
  INSTALL_GET (movementcontrol, prefix);
  INSTALL_GET (movementcontrol, postfix);

  INSTALL_PUT_ALLOW (movementcontrol);
  INSTALL_PUT_IGNORE (movementcontrol);
  INSTALL_PUT (movementcontrol, tx);
  INSTALL_PUT (movementcontrol, ty);
  INSTALL_PUT (movementcontrol, gx);
  INSTALL_PUT (movementcontrol, gy);
  INSTALL_PUT (movementcontrol, override);
  //INSTALL_GET (movementcontrol);
  //INSTALL_GET (movementcontrol);
  INSTALL_GET (movementcontrol, tx);
  INSTALL_GET (movementcontrol, ty);
  INSTALL_GET (movementcontrol, gx);
  INSTALL_GET (movementcontrol, gy);
  INSTALL_GET (movementcontrol, override);
  INSTALL_GET (movementcontrol, width);
  INSTALL_GET (movementcontrol, height);
  INSTALL_EDIT (movementcontrol);


#undef INSTALL_EDIT
#undef EDIT_DELETE_FN_DEF
#undef INSTALL_PUT
#undef INSTALL_GET
#undef GETFUNC_DEF
#undef PUTFUNC_DEF

#undef INT_PUTFUNC_DEF
#undef INT_GETFUNC_DEF
#undef PUTGRAPHICFUNC_DEF




  /* test with (display (d-DirectivePut-note-display "LHfinger" "test")) after attaching a LH finger directive */
  /* test with (display (d-DirectivePut-note-minpixels "LHfinger" 80)) after attaching a LH finger directive */
  /* test with (display (d-DirectiveGet-note-minpixels "LHfinger")) after attaching a LH finger directive */

  /* test with (display (d-DirectiveGet-note-display "LHfinger")) after attaching a LH finger directive */
  install_scm_function (1, "The passed string is placed on the system clipboard", DENEMO_SCHEME_PREFIX "PutTextClipboard", scheme_put_text_clipboard);

  install_scm_function (0, "Asks the user for a user name which is returned", DENEMO_SCHEME_PREFIX "GetUserName", scheme_get_username);
  install_scm_function (0, "Asks the user for a password which is returned", DENEMO_SCHEME_PREFIX "GetPassword", scheme_get_password);

  install_scm_function (0, "Returns an integer value, a set of bitfields representing the keyboard state, e.g. GDK_SHIFT_MASK etc", DENEMO_SCHEME_PREFIX "GetKeyboardState", scheme_get_keyboard_state);
  install_scm_function (0, "Routes the MIDI in to MIDI out if it is not intercepted by d-GetMidi", DENEMO_SCHEME_PREFIX "SetMidiThru", scheme_set_midi_thru);

  install_scm_function (0, "Returns the ticks of the next event on the recorded MIDI track -ve if it is a NOTEOFF or #f if none. Advances to the next note.", DENEMO_SCHEME_PREFIX "GetRecordedMidiOnTick", scheme_get_recorded_midi_on_tick);

  install_scm_function (0, "Returns the LilyPond representation of the passed MIDI key number, using the current enharmonic set.", DENEMO_SCHEME_PREFIX "GetNoteForMidiKey", scheme_get_note_for_midi_key);



  install_scm_function (0, "Returns the ticks of the next event on the recorded MIDI track -ve if it is a NOTEOFF or #f if none", DENEMO_SCHEME_PREFIX "GetRecordedMidiNote", scheme_get_recorded_midi_note);

  install_scm_function (0, "Rewinds the recorded MIDI track returns #f if no MIDI track recorded", DENEMO_SCHEME_PREFIX "RewindRecordedMidi", scheme_rewind_recorded_midi);

  install_scm_function (0, "Intercepts a MIDI event and returns it as a 4 byte number", DENEMO_SCHEME_PREFIX "GetMidi", scheme_get_midi);

  install_scm_function (0, "Takes one bool parameter - MIDI events will be captured/not captured depending on the value passed in, returns previous value.", DENEMO_SCHEME_PREFIX "SetMidiCapture", scheme_set_midi_capture);

  install_scm_function (0, "Displays a vitural MIDI keyboard. Can take an integer number of octaves to use.", DENEMO_SCHEME_PREFIX "VirtualKeyboard", scheme_virtual_keyboard);

  install_scm_function (0, "Switches to playalong playback. When playing or recording playback will not advance beyond the cursor position unless then mouse is moved or the next note is played in via MIDI in.", DENEMO_SCHEME_PREFIX "TogglePlayAlong", scheme_toggle_playalong);
  install_scm_function (0, "Switches to mouse conducting playback. Playback will not advance beyond the cursor position unless then mouse is moved in the drawing area.", DENEMO_SCHEME_PREFIX "ToggleConduct", scheme_toggle_conduct);

  install_scm_function (1, "Starts playback and synchronously records from MIDI in. any script passed in is run at the end of the recording. The recording will play back with future play until deleted. The recording is not saved with the score - convert to notation first,", DENEMO_SCHEME_PREFIX "MidiRecord", scheme_midi_record);
  install_scm_function (0, "Computes durationss for recorded/imported MIDI notes based on tempo and timing of note off from previous note off or start.", DENEMO_SCHEME_PREFIX "ComputeMidiNoteDurations", scheme_compute_midi_note_durations);

  install_scm_function (0, "Gets the marked recorded midi note as LilyPond", DENEMO_SCHEME_PREFIX "GetMarkedMidiNote", scheme_get_marked_midi_note);
  install_scm_function (0, "Gets the time in seconds of marked recorded midi note or #f if none", DENEMO_SCHEME_PREFIX "GetMarkedMidiNoteSeconds", scheme_get_marked_midi_note_seconds);
  install_scm_function (1, "Advances the marked recorded midi note can take an integer for number of steps to advance, or #f to clear the mark. Returns #f if no more marks.", DENEMO_SCHEME_PREFIX "AdvanceMarkedMidi", scheme_advance_marked_midi);
  install_scm_function (0, "Inserts the marked recorded or imported MIDI note using the duration guessed from the note length. Returns #f if nothing marked.", DENEMO_SCHEME_PREFIX "InsertMarkedMidiNote", scheme_insert_marked_midi_note);

  install_scm_function (0, "Generates the MIDI timings for the music of the current movement. Returns TRUE if the MIDI was re-computed else FALSE (call was unnecessary).", DENEMO_SCHEME_PREFIX "CreateTimebase", scheme_create_timebase);



  install_scm_function (1, "Takes and int as MIDI data and simulates a midi event, avoiding capturing of midi by scripts. Value 0 is special and is received by scripts.", DENEMO_SCHEME_PREFIX "PutMidi", scheme_put_midi);
  install_scm_function (1, "Takes and int as MIDI data and sends it directly to the MIDI out backend", DENEMO_SCHEME_PREFIX "OutputMidi", scheme_output_midi);


  install_scm_function (1, "Takes a string of space-separated bytes. The $ char stands for the current channel. Sends the passed bytes to the MIDI out", DENEMO_SCHEME_PREFIX "OutputMidiBytes", scheme_output_midi_bytes);
  install_scm_function (1, "Deprecated - takes an integer which is decomposed into a MIDI note played for 100ms", DENEMO_SCHEME_PREFIX "PlayMidiKey", scheme_play_midikey);
  install_scm_function (1, "Takes a midi note key and plays it with next rhythm effect", DENEMO_SCHEME_PREFIX "PendingMidi", scheme_pending_midi);
  install_scm_function (4, "Takes midi key number, volume 0-255, duration in ms and channel 0-15 and plays the note on midi out.", DENEMO_SCHEME_PREFIX "PlayMidiNote", scheme_play_midi_note);

  install_scm_function (1, "Takes duration and executable scheme script. Executes the passed scheme code after the passed duration milliseconds", DENEMO_SCHEME_PREFIX "OneShotTimer", scheme_one_shot_timer);
  install_scm_function (1, "Takes a duration and scheme script, starts a timer that tries to execute the script after every duration ms. It returns a timer id which must be passed back to destroy the timer", DENEMO_SCHEME_PREFIX "Timer", scheme_timer);
  install_scm_function (0, "Takes a timer id and destroys the timer", DENEMO_SCHEME_PREFIX "KillTimer", scheme_kill_timer);

  install_scm_function (0, "Returns #f if the current staff has no figures (or will not print out figured bass. See d-ShowFiguredBass)", DENEMO_SCHEME_PREFIX "HasFigures", scheme_has_figures);
  install_scm_function (0, "Returns bass figure string for current note or rest)", DENEMO_SCHEME_PREFIX "GetBassFigure", scheme_get_bass_figure);

  install_scm_function (2, "Returns a string for the bass figure for the two MIDI keys passed in", DENEMO_SCHEME_PREFIX "BassFigure", scheme_bass_figure);


  install_scm_function (0, "returns #t if the passed list of MIDI keys fails the pitch spellcheck", DENEMO_SCHEME_PREFIX "SpellCheckMidiChord", scheme_spell_check_midi_chord);

  install_scm_function (0, "Gets the MIDI key number for the note-position where the cursor is", DENEMO_SCHEME_PREFIX "GetCursorNoteAsMidi", scheme_get_cursor_note_as_midi);
  install_scm_function (0, "Returns the MIDI key number for the note at the cursor, or 0 if none", DENEMO_SCHEME_PREFIX "GetNoteAsMidi", scheme_get_note_as_midi);
  install_scm_function (0, "Re-draws the Denemo display, which can have side effects on the data, updates status bar ... ", DENEMO_SCHEME_PREFIX "RefreshDisplay", scheme_refresh_display);
  install_scm_function (0, "Keeps the GUI alive during long scripts ", DENEMO_SCHEME_PREFIX "KeepAlive", scheme_keep_alive);
  install_scm_function (0, "Computes cached values (normally done while drawing)", DENEMO_SCHEME_PREFIX "RefreshCache", scheme_refresh_cache);
  install_scm_function (0, "Sets the status of the current musical score to saved, or unsaved if passed #f", DENEMO_SCHEME_PREFIX "SetSaved", scheme_set_saved);
  install_scm_function (0, "Gets the saved status of the current musical score", DENEMO_SCHEME_PREFIX "GetSaved", scheme_get_saved);
  install_scm_function (0, "Sets/Gets the changecount of the current musical score", DENEMO_SCHEME_PREFIX "Changecount", scheme_changecount);
  install_scm_function (0, "Returns #f if mark is not set", DENEMO_SCHEME_PREFIX "MarkStatus", scheme_mark_status);
  install_scm_function (0, "Takes a command name and returns the tooltip or #f if none", DENEMO_SCHEME_PREFIX "GetHelp", scheme_get_help);

  install_scm_function (0, "Takes a file name, loads keybindings from actions/menus returns #f if it fails", DENEMO_SCHEME_PREFIX "LoadKeybindings", scheme_load_keybindings);

  install_scm_function (0, "Takes a file name, saves keybindings from actions/menus returns #f if it fails", DENEMO_SCHEME_PREFIX "SaveKeybindings", scheme_save_keybindings);

  install_scm_function (0, "Clears all keybindings returns #t", DENEMO_SCHEME_PREFIX "ClearKeybindings", scheme_clear_keybindings);

  install_scm_function (0, "Takes a file name for xml format commandset, loads commands, returns #f if it fails", DENEMO_SCHEME_PREFIX "LoadCommandset", scheme_load_commandset);

  install_scm_function (0, "Takes a double or string and scales the display; return #f for invalid value else the value set. With no parameter returns the current value. ", DENEMO_SCHEME_PREFIX "Zoom", scheme_zoom);

  install_scm_function (0, "Takes a double or string and scales the tempo; returns the tempo set. With no parameter returns the current master tempo ", DENEMO_SCHEME_PREFIX "MasterTempo", scheme_master_tempo);

  install_scm_function (0, "Takes an integer or string number of beats (quarter notes) per minute as the tempo for the current movement; returns the tempo set ", DENEMO_SCHEME_PREFIX "MovementTempo", scheme_movement_tempo);


  install_scm_function (0, "Takes a double or string and scales the volume; returns the volume set ", DENEMO_SCHEME_PREFIX "MasterVolume", scheme_master_volume);
  install_scm_function (0, "Takes a double 0-1 and sets the staff master volume for the current staff, returns the value.\nWith no parameter returns the current value or minus the current value if staff is muted.\nPass #f to mute the current staff and #t to unmute, leaving master volume unchanged.", DENEMO_SCHEME_PREFIX "StaffMasterVolume", scheme_staff_master_volume);

  install_scm_function (0, "Takes a integer sets the enharmonic range to use 0 = E-flat to G-sharp ", DENEMO_SCHEME_PREFIX "SetEnharmonicPosition", scheme_set_enharmonic_position);


  install_scm_function (0, "Return a string of tuning bytes (offsets from 64) for MIDI tuning message", DENEMO_SCHEME_PREFIX "GetMidiTuning", scheme_get_midi_tuning);
  install_scm_function (0, "Return name of flattest degree of current temperament", DENEMO_SCHEME_PREFIX "GetFlattest", scheme_get_flattest);

  install_scm_function (0, "Return name of sharpest degree of current temperament", DENEMO_SCHEME_PREFIX "GetSharpest", scheme_get_sharpest);
  install_scm_function (0, "Return name of current temperament", DENEMO_SCHEME_PREFIX "GetTemperament", scheme_get_temperament);

  install_scm_function (0, "Rewind the MIDI generated for the current movement. Given a time in seconds it tries to rewind to there.", DENEMO_SCHEME_PREFIX "RewindMidi", scheme_rewind_midi);
  install_scm_function (0, "Takes an interval, returns a pair, a list of the next note-on events that occur within that interval and the time of these events.", DENEMO_SCHEME_PREFIX "NextMidiNotes", scheme_next_midi_notes);

  install_scm_function (0, "Restart midi play, cancelling any pause", DENEMO_SCHEME_PREFIX "RestartPlay", scheme_restart_play);
  install_scm_function (0, "Sets the passed staff number as the only staff that will play. Pass 0 or no argument for all staffs", DENEMO_SCHEME_PREFIX "StaffToPlay", scheme_staff_to_play);
  install_scm_function (0, "Return a number, the midi time in seconds for the start of the object at the cursor; return #f if none ", DENEMO_SCHEME_PREFIX "GetMidiOnTime", scheme_get_midi_on_time);
  install_scm_function (0, "Return a number, the midi time in seconds for the end of the object at the cursor; return #f if none ", DENEMO_SCHEME_PREFIX "GetMidiOffTime", scheme_get_midi_off_time);
  install_scm_function (0, "Set the MIDI in controller to listening mode. All signals are directed straight to the output. ", DENEMO_SCHEME_PREFIX "MidiInListening", scheme_midi_in_listening);
  install_scm_function (0, "Set the MIDI in controller to checking mode. The cursor will advance and the note sounded only if it is the (lowest) note at the cursor. ", DENEMO_SCHEME_PREFIX "MidiInChecking", scheme_midi_in_checking);
  install_scm_function (0, "Set the MIDI in controller to append/edit mode. The MIDI key sounded will be inserted in score, or appended if in appending position. MIDI signals can be filtered by scheme scripts in this mode. ", DENEMO_SCHEME_PREFIX "MidiInAppendEdit", scheme_midi_in_append_edit);

  install_scm_function (2, "Set start and/or end time for playback to the passed numbers/strings in seconds. Use #t if a value is not to be changed. Returns #f for bad parameters ", DENEMO_SCHEME_PREFIX "SetPlaybackInterval", scheme_set_playback_interval);

  install_scm_function (0, "Adjust start time for playback by passed number of seconds. Returns #f for bad parameter ", DENEMO_SCHEME_PREFIX "AdjustPlaybackStart", scheme_adjust_playback_start);

  install_scm_function (0, "Adjust end time for playback by passed number of seconds. Returns #f for bad parameter ", DENEMO_SCHEME_PREFIX "AdjustPlaybackEnd", scheme_adjust_playback_end);

  install_scm_function (0, "Pushes the Denemo clipboard (cut/copy buffer) onto a stack; Use d-PopClipboard to retrieve it.", DENEMO_SCHEME_PREFIX "PushClipboard", scheme_push_clipboard);

  install_scm_function (0, "Pops the Denemo clipboard (cut/copy buffer) from a stack created by d-PushClipboard. Returns #f if nothing on stack, else #t.", DENEMO_SCHEME_PREFIX "PopClipboard", scheme_pop_clipboard);

  install_scm_function (0, "Deletes all objects in the selection Returns #f if no selection else #t.", DENEMO_SCHEME_PREFIX "DeleteSelection", scheme_delete_selection);

  install_scm_function (0, "Sets the selection to be used for a thumbnail. Returns #f if no selection or selection not in first movement else #t.", DENEMO_SCHEME_PREFIX "SetThumbnailSelection", scheme_set_thumbnail_selection);

  install_scm_function (1, "Creates a thumbnail for the current score. With no argument it waits for the thumbnail to complete, freezing any display. With #t it generates the thumbnail asynchronously. It does not report on completion.", DENEMO_SCHEME_PREFIX "CreateThumbnail", scheme_create_thumbnail);

  install_scm_function (0, "Exits Denemo without saving history, prefs etc.", DENEMO_SCHEME_PREFIX "Exit", scheme_exit);

  install_scm_function (0, "Snapshots the current movement putting it in the undo queue returns #f if no snapshot was taken because of a guard", DENEMO_SCHEME_PREFIX "TakeSnapshot", scheme_take_snapshot);

  install_scm_function (0, "Creates the default layout.", DENEMO_SCHEME_PREFIX "SelectDefaultLayout", scheme_select_default_layout);
  install_scm_function (1, "Sets the pending layout id to the layout name passed - resets to no pending layout if no name passed. Conditional directives will apply depending on the pending layout when set.", DENEMO_SCHEME_PREFIX "SetPendingLayout", scheme_set_pending_layout);
  install_scm_function (1, "Creates a custom layout from the currently selected (standard). Uses the passed name for the new layout. Returns #f if nothing happened.", DENEMO_SCHEME_PREFIX "CreateLayout", scheme_create_layout);
  install_scm_function (1, "Deletes a custom layout of the passed name. Returns #f if no layout with passed name.", DENEMO_SCHEME_PREFIX "DeleteLayout", scheme_delete_layout);
  install_scm_function (0, "Returns the id of the currently selected score layout (see View->Score Layout). Returns #f if no layout is selected.", DENEMO_SCHEME_PREFIX "GetLayoutId", scheme_get_layout_id);
  install_scm_function (0, "Returns the id of a score layout for typesetting the part for the current staff. Returns #f if not a primary voice.", DENEMO_SCHEME_PREFIX "GetCurrentStaffLayoutId", scheme_get_current_staff_layout_id);
  install_scm_function (0, "Selects the score layout with the passed id. Returns #f if there is no such layout.", DENEMO_SCHEME_PREFIX "SelectLayoutId", scheme_select_layout_id);
  install_scm_function (0, "Generates LilyPond layout for the current part (ie staffs with the name of the staff with the cursor), all movements and staffs with that staff name are generated.", DENEMO_SCHEME_PREFIX "LilyPondForPart", scheme_lilypond_for_part);
  install_scm_function (0, "Typesets the current part (ie the staff with the cursor), all movements and staffs with that staff name are typeset.", DENEMO_SCHEME_PREFIX "TypesetPart", scheme_typeset_part);
  install_scm_function (0, "Converts the current score layout to editable LilyPond text. After this the score layout is only affected by editing the LilyPond syntax.", DENEMO_SCHEME_PREFIX "ReduceLayoutToLilyPond", scheme_reduce_layout_to_lilypond);
  install_scm_function (0, "Returns the name of the currently selected score layout (see View->Score Layout). Returns #f if no layout is selected.", DENEMO_SCHEME_PREFIX "GetLayoutName", scheme_get_layout_name);
  install_scm_function (0, "Selects the next score layout. If the current layout is the last, returns #f otherwise #t.", DENEMO_SCHEME_PREFIX "SelectNextLayout", scheme_select_next_layout);
  install_scm_function (0, "Selects the first score layout.", DENEMO_SCHEME_PREFIX "SelectFirstLayout", scheme_select_first_layout);
  install_scm_function (0, "Selects the next custom score layout. If the current layout is the last, returns #f otherwise #t.", DENEMO_SCHEME_PREFIX "SelectNextCustomLayout", scheme_select_next_custom_layout);
  install_scm_function (0, "Selects the first custom score layout.", DENEMO_SCHEME_PREFIX "SelectFirstCustomLayout", scheme_select_first_custom_layout);
  
  install_scm_function (0, "Returns the full path to the currently opened Denemo score or #f if it does not have a disk file yet.", DENEMO_SCHEME_PREFIX "GetFilename", scheme_get_filename);
  install_scm_function (0, "Selects the nth open score (i.e. tab) indexed from 0, returns the current tab", DENEMO_SCHEME_PREFIX "SelectTab", scheme_select_tab);
  install_scm_function (3, "Takes two tab indexes and optional move parameter. Compares the current objects from the cursors in the two passed scores (i.e. tabs). Returns a string describing the first difference, placing the cursors on the differing objects else returs #f. Unless move is #f it moves the cursor right before starting comparisons.", DENEMO_SCHEME_PREFIX "CompareObjects", scheme_compare_objects);
  install_scm_function (3, "Takes two tab indexes and optional move parameter. Compares the current staffs in the two passed scores (i.e. tabs) returning a description of the first difference or #f if they are the same. Moves on to staffs below unless move is #f", DENEMO_SCHEME_PREFIX "DifferenceOfStaffs", scheme_difference_of_staffs);
  install_scm_function (2, "Takes two tab indexes. Compares the current movement headers for the scores indexed (i.e. tabs) returning a description of the first difference or #f if they are the same.", DENEMO_SCHEME_PREFIX "DifferenceOfMovements", scheme_difference_of_movements);
  install_scm_function (2, "Takes two tab indexes. Compares the score headers for the scores indexed (i.e. tabs) returning a description of the first difference or #f if they are the same.", DENEMO_SCHEME_PREFIX "DifferenceOfProjects", scheme_difference_of_projects);
  install_scm_function (0, "Allows the display to unfreeze, but the user may cause a crash ... ", DENEMO_SCHEME_PREFIX "Wakeup", scheme_wakeup);

  install_scm_function (1, "Returns the directory component of the passed filename.", DENEMO_SCHEME_PREFIX "PathFromFilename", scheme_path_from_filename);
  install_scm_function (1, "Returns the #t if file passed in exists.", DENEMO_SCHEME_PREFIX "FileExists", scheme_file_exists);
  install_scm_function (1, "Returns the filename component of the passed path.", DENEMO_SCHEME_PREFIX "FilenameFromPath", scheme_filename_from_path);
  install_scm_function (3, "Gives dialog to choose a file. Takes a title, start directory and list of extensions. Returns a string or #f if user cancesl", DENEMO_SCHEME_PREFIX "ChooseFile", scheme_choose_file);
  install_scm_function (3, "Gives dialog to choose a directory. Takes a title, start directory and list of extensions. Returns a string or #f if user cancesl", DENEMO_SCHEME_PREFIX "ChooseDirectory", scheme_choose_directory);

  install_scm_function (0, "Follows a link to a source file of form string \"filename:x:y:page\". It opens the file and places a marker there. ", DENEMO_SCHEME_PREFIX "OpenSource", scheme_open_source);
  install_scm_function (3, "Takes an optional filename and optional new name. Opens an encapsulated postscript file for editing. Returns the filename (without extension) if successful.\nStarts the graphics editor on the passed in filename or one from a dialog.\nThe returned .eps file may not exist when this procedure returns, an editor is open on it. With no filename parameter allows the user to choose,\ncopying to the project directory or the users graphics templates (if a new name is given)", DENEMO_SCHEME_PREFIX "EditGraphics", scheme_edit_graphics);
  install_scm_function (0, "Opens a PDF file previously generated by Denemo which has proof reading annotations. The notes in the file can be clicked on to locate the music in the Denemo display", DENEMO_SCHEME_PREFIX "OpenProofReadFile", scheme_open_proofread_file);
#ifdef DISABLE_AUBIO
#else
  install_scm_function (0, "Converts the recorded audio to user chosen audio file.", DENEMO_SCHEME_PREFIX "ExportRecordedAudio", scheme_export_recorded_audio);
  install_scm_function (0, "Opens a source file for transcribing from. Links to this source file can be placed by shift-clicking on its contents", DENEMO_SCHEME_PREFIX "OpenSourceFile", scheme_open_source_file);

  install_scm_function (0, "Opens a source audio file for transcribing from. Returns the number of seconds of audio successfully opened or #f if failed. ", DENEMO_SCHEME_PREFIX "OpenSourceAudioFile", scheme_open_source_audio_file);
  install_scm_function (0, "Closes a source audio attached to the current movement.", DENEMO_SCHEME_PREFIX "CloseSourceAudio", scheme_close_source_audio);

  install_scm_function (0, "Plays audio allowing timings to be entered via keypresses if passed #t as parameter. ", DENEMO_SCHEME_PREFIX "StartAudioPlay", scheme_start_audio_play);
  install_scm_function (0, "Stops audio playback", DENEMO_SCHEME_PREFIX "StopAudioPlay", scheme_stop_audio_play);
  install_scm_function (0, "Takes a number of seconds to be used as lead-in for audio. If negative clips that much from the start of the audio. ", DENEMO_SCHEME_PREFIX "SetAudioLeadIn", scheme_set_audio_lead_in);
  install_scm_function (0, "returns #f if audio is not playing else #t", DENEMO_SCHEME_PREFIX "AudioIsPlaying", scheme_audio_is_playing);

  install_scm_function (0, "Returns the next in the list of timings registered by the user during audio play.", DENEMO_SCHEME_PREFIX "NextAudioTiming", scheme_next_audio_timing);
#endif




  install_scm_function (0, "Stop collecting undo information. Call DecreaseGuard when finished. Returns #f if already guarded, #t if this call is stopping the undo collection", DENEMO_SCHEME_PREFIX "IncreaseGuard", scheme_increase_guard);

  install_scm_function (0, "Drop one guard against collecting undo information. Returns #t if there are no more guards \n(undo information will be collected) \nor #f if there are still guards in place.", DENEMO_SCHEME_PREFIX "DecreaseGuard", scheme_decrease_guard);

  install_scm_function (0, "Undoes the actions performed by the script so far, starts another undo stage for the subsequent actions of the script. Note this command has the same name as the built-in Undo command, to override it when called from a script. Returns #t", DENEMO_SCHEME_PREFIX "Undo" /*sic */ , scheme_undo);
  install_scm_function (0, "Creates a new tab. Note this command has the same name as the built-in NewWindow command, to override it when called from a script. Returns #t", DENEMO_SCHEME_PREFIX "NewWindow" /*sic */ , scheme_new_window);

  install_scm_function (0, "Undo normally undoes all the actions performed by a script. This puts a stage at the point in a script where it is called, so that a user-invoked undo will stop at this point, continuing when a further undo is invoked. Returns #t", DENEMO_SCHEME_PREFIX "StageForUndo", scheme_stage_for_undo);

  install_scm_function (0, "return a string giving the latest step available for Undo", DENEMO_SCHEME_PREFIX "GetLastChange", scheme_get_last_change);

  install_scm_function (0, "Takes a command name and returns the menu path to that command or #f if none", DENEMO_SCHEME_PREFIX "GetMenuPath", scheme_get_menu_path);
  install_scm_function (0, "Takes a string and returns a string representing an MD5 checksum for the passed string.", DENEMO_SCHEME_PREFIX "GetChecksum", scheme_get_checksum);
  install_scm_function (0, "Sets the newbie status to the passed value", DENEMO_SCHEME_PREFIX "SetNewbie", scheme_set_newbie);
  install_scm_function (0, "Gets the current verse of the current staff or #f if none, with an integer parameter, gets the nth verse", DENEMO_SCHEME_PREFIX "GetVerse", scheme_get_verse);
  install_scm_function (0, "With a boolean parameter sets whether lyrics should be typeset for the current staff, else returns the current status.", DENEMO_SCHEME_PREFIX "TypesetLyricsForStaff", scheme_typeset_lyrics_for_staff);

  install_scm_function (0, "Gets the number of current verse of the current staff or #f if none. With an integer parameter sets the verse to that number.", DENEMO_SCHEME_PREFIX "GetVerseNumber", scheme_get_verse_number);
  install_scm_function (0, "Gets the number of lyric syllables in the current staff up to the cursor position.", DENEMO_SCHEME_PREFIX "SyllableCount", scheme_syllable_count);
  install_scm_function (0, "Moves the lyric cursor to match the current Denemo Cursor position (offset by an optional integer parameter), switching the keyboard input to the lyrics pane", DENEMO_SCHEME_PREFIX "SynchronizeLyricCursor", scheme_synchronize_lyric_cursor);
  install_scm_function (1, "Inserts passed text at the lyric cursor in the lyrics pane, returns #f if no verse at cursor", DENEMO_SCHEME_PREFIX "InsertTextInVerse", scheme_insert_text_in_verse);
  install_scm_function (0, "Puts the passed string as the current verse of the current staff", DENEMO_SCHEME_PREFIX "PutVerse", scheme_put_verse);
  install_scm_function (0, "Appends the passed string to the current verse of the current staff", DENEMO_SCHEME_PREFIX "AppendToVerse", scheme_append_to_verse);


  install_scm_function (0, "Takes a command name and returns and id for it or #f if no command of that name exists", DENEMO_SCHEME_PREFIX "GetId", scheme_get_id);

  install_scm_function (2, "Takes a command name or command id and binding name and sets that binding on that command returns the command id that previously had the binding or #f if none", DENEMO_SCHEME_PREFIX "AddKeybinding", scheme_add_keybinding);

  install_scm_function (0, "Takes a command name and returns the label for the menu item that executes the command or #f if none", DENEMO_SCHEME_PREFIX "GetLabel", scheme_get_label);
  install_scm_function (0, "Takes a non-built-in command name and returns position in the menu system for he command or #f if none", DENEMO_SCHEME_PREFIX "GetMenuPosition", scheme_get_menu_position);


  install_scm_function (0, "Returns the installed LilyPond version", DENEMO_SCHEME_PREFIX "GetLilyVersion", scheme_get_lily_version);
  install_scm_function (0, "Returns a boolean if the installed version of LilyPond is greater than or equal to the passed in version string", DENEMO_SCHEME_PREFIX "CheckLilyVersion", scheme_check_lily_version);



  install_scm_function (0, "Takes a string putting it on the scheme-controlled status bar as a list of active filters", DENEMO_SCHEME_PREFIX "InputFilterNames", scheme_input_filter_names);

  install_scm_function (0, "Takes a string putting the scheme controlled status bar; with no argument it hides this  status bar", DENEMO_SCHEME_PREFIX "WriteStatus", scheme_write_status);

  install_scm_function (1, "Display a debug message", DENEMO_SCHEME_PREFIX "Debug", scheme_log_debug);
  install_scm_function (1, "Display an info message", DENEMO_SCHEME_PREFIX "Info", scheme_log_info);
  install_scm_function (1, "Display a regular message", DENEMO_SCHEME_PREFIX "Message", scheme_log_message);
  install_scm_function (1, "Display a warning message", DENEMO_SCHEME_PREFIX "Warning", scheme_log_warning);
  install_scm_function (1, "Display a critical message", DENEMO_SCHEME_PREFIX "Critical", scheme_log_critical);
  install_scm_function (1, "Display an error message and abort", DENEMO_SCHEME_PREFIX "Error", scheme_log_error);

}
