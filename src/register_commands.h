register_command(Denemo.map, gtk_action_group_get_action(action_group, "CursorLeft"), "CursorLeft", "Cursor Left", "Moves the cursor one object left, altering the selection if any", cursorleft);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveCursorLeft"), "MoveCursorLeft", "Move Cursor Left", "Moves the cursor one object left, without altering the selection", movecursorleft);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "CursorDown"), "CursorDown", "Cursor Down", "Moves the cursor one scale step down", cursordown);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "CursorUp"), "CursorUp", "Cursor Up", "Moves the cursor one scale step up", cursorup);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "CursorRight"), "CursorRight", "Cursor Right", "Moves the cursor one object right, altering the selection if any", cursorright);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveCursorRight"), "MoveCursorRight", "Move Cursor Right", "Moves the cursor one object right, without altering the selection", movecursorright);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "GoToMark"), "GoToMark", "To Mark", "Moves the cursor to the Mark without altering the selection", goto_mark);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SwapPointAndMark"), "SwapPointAndMark", "Swap Ends of Selection", "Swaps the active end of the selection", swap_point_and_mark);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "GoToSelectionStart"), "GoToSelectionStart", "To Selection Start", "Moves the cursor to the first object in the selection without altering the selection. returns #f if no selection", goto_selection_start);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PushPosition"), "PushPosition", "Push Position", "Pushes the current cursor position onto a stack", PushPosition);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PopPosition"), "PopPosition", "Pop Position", "Pops a position from the stack of cursor positions, moving the cursor there", PopPosition);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PopPushPosition"), "PopPushPosition", "Pop and Push Position", "Pops a position from the stack of cursor positions, pushes the current position, then moves the cursor to the popped position", PopPushPosition);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleReduceToDrawingArea"), "ToggleReduceToDrawingArea", "Hide/Show Menus", "Hides/Shows menus, panes etc. The ones shown are those checked in the view menu.", ToggleReduceToDrawingArea);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StaffUp"), "StaffUp", "Staff Up", "Moves the cursor to the staff above, extending selection if any", staffup);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StaffDown"), "StaffDown", "Staff Down", "Moves the cursor to the staff below, extending selection if any", staffdown);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToStaffUp"), "MoveToStaffUp", "Move to Staff Up", "Moves the cursor to the staff above without altering selection", movetostaffup);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToStaffDown"), "MoveToStaffDown", "Move to Staff Down", "Moves the cursor to the staff below  without altering selection", movetostaffdown);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MeasureLeft"), "MeasureLeft", "Measure Left", "Moves the cursor to the first object in the next measure, extending selection if any", measureleft);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MeasureRight"), "MeasureRight", "Measure Right", "Moves the cursor to the first object in the previous measure, extending selection if any", measureright);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToMeasureLeft"), "MoveToMeasureLeft", "Move to Measure Left", "Moves the cursor to the first object in the next measure leaving selection, if any, unchanged", movetomeasureleft);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToMeasureRight"), "MoveToMeasureRight", "Move to Measure Right", "Moves the cursor to the first object in the previous measureleaving selection, if any, unchanged", movetomeasureright);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "A"), "A", "A", "Append/Edit  A", go_to_A_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "B"), "B", "B", "Append/Edit  B", go_to_B_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "C"), "C", "C", "Append/Edit  C", go_to_C_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "D"), "D", "D", "Append/Edit  D", go_to_D_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "E"), "E", "E", "Append/Edit  E", go_to_E_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "F"), "F", "F", "Append/Edit  F", go_to_F_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "G"), "G", "G", "Append/Edit  G", go_to_G_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OctaveUp"), "OctaveUp", "Octave Up", "Octave Up", octave_up_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OctaveDown"), "OctaveDown", "Octave Down", "Octave Down", octave_down_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "WholeNote"), "WholeNote", "WholeNote", "Insert \xF0\x9D\x85\x9D", insert_chord_0key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "HalfNote"), "HalfNote", "HalfNote", "Insert \xF0\x9D\x85\x9E", insert_chord_1key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "QuarterNote"), "QuarterNote", "QuarterNote", "Insert \xF0\x9D\x85\x9F", insert_chord_2key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EighthNote"), "EighthNote", "EighthNote", "Insert \xF0\x9D\x85\xA0", insert_chord_3key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SixteenthNote"), "SixteenthNote", "SixteenthNote", "Insert \xF0\x9D\x85\xA1", insert_chord_4key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ThirtysecondNote"), "ThirtysecondNote", "ThirtysecondNote", "Insert \xF0\x9D\x85\xA2", insert_chord_5key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SixtyfourthNote"), "SixtyfourthNote", "SixtyfourthNote", "Insert \xF0\x9D\x85\xA3", insert_chord_6key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OneHundredTwentyEighthNote"), "OneHundredTwentyEighthNote", "OneHundredTwentyEighthNote", "Insert \xF0\x9D\x85\xA4", insert_chord_7key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "TwoHundredFiftySixthNote"), "TwoHundredFiftySixthNote", "TwoHundredFiftySixthNote", "Insert \xF0\x9D\x85\xA5", insert_chord_8key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertWholeRest"), "InsertWholeRest", "<span font_desc=\"Denemo\">\xF0\x9D\x84\xBB</span>", "Insert \xF0\x9D\x84\xBB rest", insert_rest_0key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertHalfRest"), "InsertHalfRest", "<span font_desc=\"Denemo\">\xF0\x9D\x84\xBC</span>", "Insert \xF0\x9D\x84\xBC rest", insert_rest_1key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertQuarterRest"), "InsertQuarterRest", "<span font_desc=\"Denemo\">\xF0\x9D\x84\xBD</span>", "Insert \xF0\x9D\x84\xBD rest", insert_rest_2key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertEighthRest"), "InsertEighthRest", "<span font_desc=\"Denemo\">\xF0\x9D\x84\xBE</span>", "Insert \xF0\x9D\x84\xBE rest", insert_rest_3key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertSixteenthRest"), "InsertSixteenthRest", "<span font_desc=\"Denemo\">\xF0\x9D\x84\xBF</span>", "Insert \xF0\x9D\x84\xBF rest", insert_rest_4key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertThirtysecondRest"), "InsertThirtysecondRest", "<span font_desc=\"Denemo\">\xF0\x9D\x85\x80</span>", "Insert \xF0\x9D\x85\x80 rest", insert_rest_5key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertSixtyfourthRest"), "InsertSixtyfourthRest", "<span font_desc=\"Denemo\">\xF0\x9D\x85\x81</span>", "Insert \xF0\x9D\x85\x81 rest", insert_rest_6key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankWholeNote"), "InsertBlankWholeNote", "InsertBlankWholeNote", "Insert a non-printing \xF0\x9D\x84\xBB rest", insert_blankchord_0key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankHalfNote"), "InsertBlankHalfNote", "InsertBlankHalfNote", "Insert a non-printing \xF0\x9D\x84\xBC rest", insert_blankchord_1key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankQuarterNote"), "InsertBlankQuarterNote", "InsertBlankQuarterNote", "Insert a non-printing \xF0\x9D\x84\xBD rest", insert_blankchord_2key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankEighthNote"), "InsertBlankEighthNote", "InsertBlankEighthNote", "Insert a non-printing \xF0\x9D\x84\xBE rest", insert_blankchord_3key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankSixteenthNote"), "InsertBlankSixteenthNote", "InsertBlankSixteenthNote", "Insert a non-printing \xF0\x9D\x84\xBF rest", insert_blankchord_4key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankThirtysecondNote"), "InsertBlankThirtysecondNote", "InsertBlankThirtysecondNote", "Insert a non-printing \xF0\x9D\x85\x80 rest", insert_blankchord_5key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankSixtyfourthNote"), "InsertBlankSixtyfourthNote", "InsertBlankSixtyfourthNote", "Insert a non-printing \xF0\x9D\x85\x81 rest", insert_blankchord_6key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankOneHundredTwentyEighthNote"), "InsertBlankOneHundredTwentyEighthNote", "InsertBlankOneHundredTwentyEighthNote", "Insert a non-printing \xF0\x9D\x85\x82 rest", insert_blankchord_7key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankTwoHundredFiftySixthNote"), "InsertBlankTwoHundredFiftySixthNote", "InsertBlankTwoHundredFiftySixthNote", "Insert a non-printing \xF0\x9D\x85\x83 rest", insert_blankchord_8key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleRestMode"), "ToggleRestMode", "Toggle Rest Mode", "No Tooltip yet", rest_toggle_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleBlankMode"), "ToggleBlankMode", "Toggle Blank Mode", "No Tooltip yet", toggle_blank);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDuplet"), "InsertDuplet", "Insert Duplet", "No Tooltip yet", insert_duplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertTriplet"), "InsertTriplet", "Insert Triplet", "No Tooltip yet", insert_triplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StartTriplet"), "StartTriplet", "Start Triplet", "No Tooltip yet", start_triplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EndTuplet"), "EndTuplet", "End Tuplet", "No Tooltip yet", end_tuplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertQuadtuplet"), "InsertQuadtuplet", "Insert Quadtuplet", "No Tooltip yet", insert_quadtuplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertQuintuplet"), "InsertQuintuplet", "Insert Quintuplet", "No Tooltip yet", insert_quintuplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertSextuplet"), "InsertSextuplet", "Insert Sextuplet", "No Tooltip yet", insert_sextuplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertSeptuplet"), "InsertSeptuplet", "Insert Septuplet", "No Tooltip yet", insert_septuplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteToChord"), "AddNoteToChord", "Add note", "Add a note to the current chord\nThe cursor position determines which note to add", add_tone_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "RemoveNoteFromChord"), "RemoveNoteFromChord", "Remove note", "Remove a note from the current chord", remove_tone_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Sharpen"), "Sharpen", "Sharpen", "No Tooltip yet", sharpen_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Flatten"), "Flatten", "Flatten", "No Tooltip yet", flatten_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PendingSharpen"), "PendingSharpen", "Sharpen Next Note", "Increases the sharpness of the next entered note. The status bar shows the current state.", pending_sharpen);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PendingFlatten"), "PendingFlatten", "Flatten Next Note", "Increases the flatness of the next entered note. The status bar shows the current state.", pending_flatten);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StemUp"), "StemUp", "StemUp", "Alters a StemNeutral object to stem up.", stem_up);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StemDown"), "StemDown", "StemDown", "Alters a StemNeutral object to stem down.", stem_down);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddDot"), "AddDot", "Add Dot", "No Tooltip yet", add_dot_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "RemoveDot"), "RemoveDot", "Remove Dot", "No Tooltip yet", remove_dot_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertTiedNote"), "InsertTiedNote", "Tied note", "Inserts a duplicate of the current note, tied", tie_notes_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleTie"), "ToggleTie", "Toggle Tie", "Ties/unties the note at the cursor", toggle_tie);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteObject"), "DeleteObject", "Delete Object", "Delete the object at the cursor", deleteobject);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeletePreviousObject"), "DeletePreviousObject", "Delete Previous Object", "Delete to the left of the cursor.", deletepreviousobject);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertMeasure"), "InsertMeasure", "Insert Measure Before", "Insert a blank measure before the current one (in all staffs)", insert_measure_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddMeasure"), "AddMeasure", "Insert Measure After", "Insert a blank measure after the current one (in all staffs)", addmeasureafter);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertMeasureBefore"), "InsertMeasureBefore", "Staff Insert Measure Before", "Insert a blank measure before the current one (in current staff)", insertmeasurebefore);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertMeasureAfter"), "InsertMeasureAfter", "Staff Insert Measure After", "Insert a blank measure in current staff after the current measure", insertmeasureafter);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AppendMeasure"), "AppendMeasure", "Staff Append Measure", "No Tooltip yet", append_measure_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteMeasure"), "DeleteMeasure", "Staff Delete Measure", "Delete the current measure in this staff, leaving the staff short", deletemeasure);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteMeasureAllStaffs"), "DeleteMeasureAllStaffs", "Delete Measure All Staffs", "Delete the current measure in all staffs", deletemeasureallstaffs);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ShrinkMeasures"), "ShrinkMeasures", "Shrink Measure", "No Tooltip yet", adjust_measure_less_width_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "WidenMeasures"), "WidenMeasures", "Widen Measures", "No Tooltip yet", adjust_measure_more_width_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ShorterStaffs"), "ShorterStaffs", "Shorter Staffs", "No Tooltip yet", adjust_staff_less_height_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "TallerStaffs"), "TallerStaffs", "Taller Staffs", "No Tooltip yet", adjust_staff_more_height_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertTrebleClef"), "InsertTrebleClef", "New Treble Clef", "No Tooltip yet", newcleftreble);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBassClef"), "InsertBassClef", "New Bass Clef", "No Tooltip yet", newclefbass);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insertg8clef"), "Insertg8clef", "New G8 Clef", "No Tooltip yet", newclefg8);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertAltoClef"), "InsertAltoClef", "New Alto Clef", "No Tooltip yet", newclefalto);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertTenorClef"), "InsertTenorClef", "New Tenor Clef", "No Tooltip yet", newcleftenor);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertSopranoClef"), "InsertSopranoClef", "New Soprano Clef", "No Tooltip yet", newclefsoprano);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialTrebleClef"), "SetInitialTrebleClef", "Set Treble Clef", "No Tooltip yet", setcleftreble);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialBassClef"), "SetInitialBassClef", "Set Bass Clef", "No Tooltip yet", setclefbass);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialg8clef"), "SetInitialg8clef", "Set G8 Clef", "No Tooltip yet", setclefg8);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialAltoClef"), "SetInitialAltoClef", "Set Alto Clef", "No Tooltip yet", setclefalto);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialTenorClef"), "SetInitialTenorClef", "Set Tenor Clef", "No Tooltip yet", setcleftenor);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialSopranoClef"), "SetInitialSopranoClef", "Set Soprano Clef", "No Tooltip yet", setclefsoprano);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert22Time"), "Insert22Time", "Insert 2/2 Time", "No Tooltip yet", newtimesig22);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert32Time"), "Insert32Time", "Insert 3/2 Time", "No Tooltip yet", newtimesig32);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert42Time"), "Insert42Time", "Insert 4/2 Time", "No Tooltip yet", newtimesig42);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert44Time"), "Insert44Time", "Insert 4/4 Time", "No Tooltip yet", newtimesig44);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert34Time"), "Insert34Time", "Insert 3/4 Time", "No Tooltip yet", newtimesig34);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert24Time"), "Insert24Time", "Insert 2/4 Time", "No Tooltip yet", newtimesig24);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert64Time"), "Insert64Time", "Insert 6/4 Time", "No Tooltip yet", newtimesig64);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert38Time"), "Insert38Time", "Insert 3/8 Time", "No Tooltip yet", newtimesig38);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert68Time"), "Insert68Time", "Insert 6/8 Time", "No Tooltip yet", newtimesig68);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert128Time"), "Insert128Time", "Insert 12/8 Time", "No Tooltip yet", newtimesig128);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert98Time"), "Insert98Time", "Insert 9/8 Time", "No Tooltip yet", newtimesig98);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set22Time"), "Set22Time", "Set 2/2 Time", "No Tooltip yet", settimesig22);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set32Time"), "Set32Time", "Set 3/2 Time", "No Tooltip yet", settimesig32);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set42Time"), "Set42Time", "Set 4/2 Time", "No Tooltip yet", settimesig42);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set44Time"), "Set44Time", "Set 4/4 Time", "No Tooltip yet", settimesig44);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set34Time"), "Set34Time", "Set 3/4 Time", "No Tooltip yet", settimesig34);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set24Time"), "Set24Time", "Set 2/4 Time", "No Tooltip yet", settimesig24);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set64Time"), "Set64Time", "Set 6/4 Time", "No Tooltip yet", settimesig64);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set38Time"), "Set38Time", "Set 3/8 Time", "No Tooltip yet", settimesig38);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set68Time"), "Set68Time", "Set 6/8 Time", "No Tooltip yet", settimesig68);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set128Time"), "Set128Time", "Set 12/8 Time", "No Tooltip yet", settimesig128);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set98Time"), "Set98Time", "Set 9/8 Time", "No Tooltip yet", settimesig98);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertCmaj"), "InsertCmaj", "Insert Cmaj", "No Tooltip yet", newkeysigcmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertGmaj"), "InsertGmaj", "Insert Gmaj", "No Tooltip yet", newkeysiggmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDmaj"), "InsertDmaj", "Insert Dmaj", "No Tooltip yet", newkeysigdmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertAmaj"), "InsertAmaj", "Insert Amaj", "No Tooltip yet", newkeysigamaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertEmaj"), "InsertEmaj", "Insert Emaj", "No Tooltip yet", newkeysigemaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBmaj"), "InsertBmaj", "Insert Bmaj", "No Tooltip yet", newkeysigbmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertFSharpmaj"), "InsertFSharpmaj", "Insert F# Major", "No Tooltip yet", newkeysigfsharpmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertCSharpmaj"), "InsertCSharpmaj", "Insert C# Major", "No Tooltip yet", newkeysigcsharpmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertFmaj"), "InsertFmaj", "Insert F Major", "No Tooltip yet", newkeysigfmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBflatmaj"), "InsertBflatmaj", "Insert Bb Major", "No Tooltip yet", newkeysigbflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertEflatmaj"), "InsertEflatmaj", "Insert Eb Major", "No Tooltip yet", newkeysigeflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertAflatmaj"), "InsertAflatmaj", "Insert Ab Major", "No Tooltip yet", newkeysigaflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDflatmaj"), "InsertDflatmaj", "Insert Db Major", "No Tooltip yet", newkeysigdflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertGflatmaj"), "InsertGflatmaj", "Insert Gb Major", "No Tooltip yet", newkeysiggflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertCflatmaj"), "InsertCflatmaj", "Insert Cb Major", "No Tooltip yet", newkeysigcflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertAmin"), "InsertAmin", "Insert A Minor", "No Tooltip yet", newkeysigamin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertEmin"), "InsertEmin", "Insert E Minor", "No Tooltip yet", newkeysigemin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBmin"), "InsertBmin", "Insert B Minor", "No Tooltip yet", newkeysigbmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertFSharpmin"), "InsertFSharpmin", "Insert F# Minor", "No Tooltip yet", newkeysigfsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertCSharpmin"), "InsertCSharpmin", "Insert C# Minor", "No Tooltip yet", newkeysigcsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertGSharpmin"), "InsertGSharpmin", "Insert G# Minor", "No Tooltip yet", newkeysiggsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDSharpmin"), "InsertDSharpmin", "Insert D# Minor", "No Tooltip yet", newkeysigdsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertASharpmin"), "InsertASharpmin", "Insert A# Minor", "No Tooltip yet", newkeysigasharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDmin"), "InsertDmin", "Insert D Minor", "No Tooltip yet", newkeysigdmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertGmin"), "InsertGmin", "Insert G Minor", "No Tooltip yet", newkeysiggmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertCmin"), "InsertCmin", "Insert C Minor", "No Tooltip yet", newkeysigcmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertFmin"), "InsertFmin", "Insert F Minor", "No Tooltip yet", newkeysigfmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBflatmin"), "InsertBflatmin", "Insert Bb Minor", "No Tooltip yet", newkeysigbflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertEflatmin"), "InsertEflatmin", "Insert Eb Minor", "No Tooltip yet", newkeysigeflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertAflatmin"), "InsertAflatmin", "Insert Ab Minor", "No Tooltip yet", newkeysigaflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialCmaj"), "SetInitialCmaj", "Set Initial Keysig to C Major", "No Tooltip yet", setkeysigcmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialGmaj"), "SetInitialGmaj", "Set Initial Keysig to G Major", "No Tooltip yet", setkeysiggmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialDmaj"), "SetInitialDmaj", "Set D Major as Initial Keysig", "No Tooltip yet", setkeysigdmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialAmaj"), "SetInitialAmaj", "Set A Major as Initial Keysig", "No Tooltip yet", setkeysigamaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialEmaj"), "SetInitialEmaj", "Set E Major as Initial Keysig", "No Tooltip yet", setkeysigemaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialBmaj"), "SetInitialBmaj", "Set B Major as Initial Keysig", "No Tooltip yet", setkeysigbmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialFSharpmaj"), "SetInitialFSharpmaj", "Set F# Major as Initial Keysig", "No Tooltip yet", setkeysigfsharpmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialCSharpmaj"), "SetInitialCSharpmaj", "Set C# Major as Initial Keysig", "No Tooltip yet", setkeysigcsharpmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialFmaj"), "SetInitialFmaj", "Set F Major as Initial Keysig", "No Tooltip yet", setkeysigfmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialBflatmaj"), "SetInitialBflatmaj", "Set Bb Major as Initial Keysig", "No Tooltip yet", setkeysigbflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialEflatmaj"), "SetInitialEflatmaj", "Set Eb Major as Initial Keysig", "No Tooltip yet", setkeysigeflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialAflatmaj"), "SetInitialAflatmaj", "Set Ab Major as Initial Keysig", "No Tooltip yet", setkeysigaflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialDflatmaj"), "SetInitialDflatmaj", "Set Db Major as Initial Keysig", "No Tooltip yet", setkeysigdflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialGflatmaj"), "SetInitialGflatmaj", "Set Gb Major as Initial Keysig", "No Tooltip yet", setkeysiggflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialCflatmaj"), "SetInitialCflatmaj", "Set Cb Major as Initial Keysig", "No Tooltip yet", setkeysigcflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialAmin"), "SetInitialAmin", "Set A Minor as Initial Keysig", "No Tooltip yet", setkeysigamin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialEmin"), "SetInitialEmin", "Set E Minor as Initial Keysig", "No Tooltip yet", setkeysigemin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialBmin"), "SetInitialBmin", "Set B Minor as Initial Keysig", "No Tooltip yet", setkeysigbmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialFSharpmin"), "SetInitialFSharpmin", "Set F# Minor as Initial Keysig", "No Tooltip yet", setkeysigfsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialCSharpmin"), "SetInitialCSharpmin", "Set C# Minor as Initial Keysig", "No Tooltip yet", setkeysigcsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialGSharpmin"), "SetInitialGSharpmin", "Set G# Minor as Initial Keysig", "No Tooltip yet", setkeysiggsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialDSharpmin"), "SetInitialDSharpmin", "Set D# Minor as Initial Keysig", "No Tooltip yet", setkeysigdsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialASharpmin"), "SetInitialASharpmin", "Set A# Minor as Initial Keysig", "No Tooltip yet", setkeysigasharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialDmin"), "SetInitialDmin", "Set D Minor as Initial Keysig", "No Tooltip yet", setkeysigdmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialGmin"), "SetInitialGmin", "Set G Minor as Initial Keysig", "No Tooltip yet", setkeysiggmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialCmin"), "SetInitialCmin", "Set C Minor as Initial Keysig", "No Tooltip yet", setkeysigcmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialFmin"), "SetInitialFmin", "Set F Minor as Initial Keysig", "No Tooltip yet", setkeysigfmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialBflatmin"), "SetInitialBflatmin", "Set Bb Minor as Initial Keysig", "No Tooltip yet", setkeysigbflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialEflatmin"), "SetInitialEflatmin", "Set Eb Minor as Initial Keysig", "No Tooltip yet", setkeysigeflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialAflatmin"), "SetInitialAflatmin", "Set Ab Minor as Initial Keysig", "No Tooltip yet", setkeysigaflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetMark"), "SetMark", "Set Mark", "Sets the start point for a selection,\nthe end point of the selection is unaltered", set_mark);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "UnsetMark"), "UnsetMark", "Unset Mark", "Gets rid of the selection.", unset_mark);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetPoint"), "SetPoint", "Set Point", "Extends the selection to the current cursor position", set_point);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleBeginSlur"), "ToggleBeginSlur", "Begin Slur", "Insert/delete begin slur on this note", toggle_begin_slur);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleEndSlur"), "ToggleEndSlur", "End Slur", "Insert/delete end slur on this note", toggle_end_slur);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleStartCrescendo"), "ToggleStartCrescendo", "Start Crescendo", "No Tooltip yet", toggle_start_crescendo);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleEndCrescendo"), "ToggleEndCrescendo", "End Crescendo", "No Tooltip yet", toggle_end_crescendo);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleStartDiminuendo"), "ToggleStartDiminuendo", "Start Diminuendo", "No Tooltip yet", toggle_start_diminuendo);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleEndDiminuendo"), "ToggleEndDiminuendo", "End Diminuendo", "No Tooltip yet", toggle_end_diminuendo);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetGrace"), "SetGrace", "Set Grace", "No Tooltip yet", set_grace);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleGrace"), "ToggleGrace", "Grace Note Off/On", "Makes the note at the cursor a grace note, if it is one, makes it normal", toggle_grace);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ForceCaution"), "ForceCaution", "Force Cautionary Accidental", "No Tooltip yet", force_cautionary);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangePitch"), "ChangePitch", "Change Pitch", "No Tooltip yet", change_pitch);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRhythm"), "InsertRhythm", "Insert Snippet", "No Tooltip yet", insert_rhythm_pattern);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "NextRhythm"), "NextRhythm", "Next Snippet", "Make next snippet\nthe current snippet.\nNotes entered will follow the rhythmic pattern of this snippet", nextrhythm);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AppendMeasureAllStaffs"), "AppendMeasureAllStaffs", "Append Measure All Staffs", "Appends a blank measure to every staff in this movement", append_measure_score);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ExecuteScheme"), "ExecuteScheme", "Execute Scheme", "Execute the scheme code from the scripting window", execute_scheme);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SharpenEnharmonicSet"), "SharpenEnharmonicSet", "Shift Accidentals Sharpwise", "Shifts the set of accidentals one step sharper", set_sharper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "FlattenEnharmonicSet"), "FlattenEnharmonicSet", "Shift Accidentals Flatwise", "Shifts the set of accidentals one step flatter", set_flatter);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "New"), "New", "New File", "Start a new musical score", file_newwrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Open"), "Open", "Open", "Open a file containing a music score for editing", file_open_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ImportLilypond"), "ImportLilypond", "Import Lilypond", "Import a Lilypond file", file_import_lilypond_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ImportMidi"), "ImportMidi", "Import Midi", "Import a Midi file", file_import_midi_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ImportMusicXml"), "ImportMusicXml", "Import MusicXml", "Import a MusicXml file", file_import_musicxml_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddStaffs"), "AddStaffs", "Add Staffs", "Add staffs from a Denemo file", file_add_staffs);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddMovements"), "AddMovements", "Add Movement", "Add movements from a Denemo file", file_add_movements);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MovementProps"), "MovementProps", "Change Properties", "Change properties of this movement", movement_props_dialog);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OpenNewWindow"), "OpenNewWindow", "Open In New", "Open a file containing a music score for editing in a separate working area (tab", openinnew);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Save"), "Save", "Save", "Save the score", file_savewrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveAs"), "SaveAs", "Save As", "Save the score under a new name", file_saveaswrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveCopy"), "SaveCopy", "Create Copy", "Save a copy of the score", file_copy_save);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OpenTemplate"), "OpenTemplate", "Open Template", "Start a new score from a built-in template file", system_template_open_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OpenExample"), "OpenExample", "Open Example", "Start a new score from a built-in example", system_example_open_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OpenMyTemplate"), "OpenMyTemplate", "Open Custom Template", "Start a new score from one of your own template files", local_template_open_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveTemplate"), "SaveTemplate", "Save Template", "Save the score as a template for re-use as a starting point for new scores", template_save);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "NewWindow"), "NewWindow", "New Tab", "Create working area (tab with an empty score in it)", newview);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertMovementBefore"), "InsertMovementBefore", "Insert Movement Before", "Insert a new movement before the current one", insert_movement_before);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertMovementAfter"), "InsertMovementAfter", "Insert Movement After", "Insert a new movement after the current one", insert_movement_after);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "NewMovement"), "NewMovement", "New Movement", "Create a new movement, usign any default template", append_new_movement);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveParts"), "SaveParts", "Save Parts", "Save Parts: each staff becomes a file in lilypond format", file_savepartswrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ExportMUDELA"), "ExportMUDELA", "Export Lilypond", "Export the score as a lilypond file", export_mudela_action);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ExportPDF"), "ExportPDF", "Export PDF", "Export the score as a PDF document file", export_pdf_action);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ExportPNG"), "ExportPNG", "Export PNG", "Export the score as a PNG image file", export_png_action);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ExportMIDI"), "ExportMIDI", "Export MIDI", "Export the score as a MIDI file", export_midi_action);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PrintPreview"), "PrintPreview", "Print Preview", "Displays the final finished score in your pdf viewer", printpreview_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PrintView"), "PrintView", "Print Preview", "Displays the final finished score in the Print View window", show_print_view);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PrintSelection"), "PrintSelection", "Print Selection", "Displays selected music from score in your pdf viewer", printselection_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PrintExcerptPreview"), "PrintExcerptPreview", "Print Excerpt", "Displays a musical excerpt in your image viewer", printexcerptpreview_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Print"), "Print", "Print", "Displays the final finished score in a pdf viewer. From this you can print the file using the print command of the viewer", printall_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PrintPart"), "PrintPart", "Print Part", "Displays the final finished score for the current part (that is current staff", printpart_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Close"), "Close", "Close Score", "Close the current score. Other windows will stay open", close_gui_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Quit"), "Quit", "Quit", "Quit the Denemo program", closewrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Undo"), "Undo", "Undo", "Undoes one (more) step of your edits to the current score.", undowrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Redo"), "Redo", "Redo", "Redoes the next of the steps you have Undone", redowrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Copy"), "Copy", "Copy", "Copy", copywrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Cut"), "Cut", "Cut", "Cut", cutwrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Paste"), "Paste", "Paste", "Paste the selected music", pastewrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PasteClipboard"), "PasteClipboard", "Paste LilyPond notes", "Paste LilyPond notes from the text clipboard", paste_clipboard);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ScoreProperties"), "ScoreProperties", "Score Properties", "Change some of the properties of the current score. This will start up a dialog window", score_properties_dialog);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveSelection"), "SaveSelection", "Save Selection", "Save the selected music. Not sure if this is working", saveselwrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Preferences"), "Preferences", "Change Preferences", "Set and save your preferences for how Denemo operates on startup. Edit .denemo/denemorc for missing ones", preferences_change);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveAccels"), "SaveAccels", "Save Command Set", "Save the current commands and keyboard shortcuts as the default", save_default_keymap_file_wrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "CommandManagement"), "CommandManagement", "Manage Command Set", "View help, change and save keyboard shortcuts", configure_keyboard_dialog);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SwapStaffs"), "SwapStaffs", "Swap Staffs", "Swap this staff with the one higher up. Note this actually swaps voices.", swapstaffs);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SplitVoices"), "SplitVoices", "Split Voices", "Split off the next voice as a separate staff", splitstaffs);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "JoinVoices"), "JoinVoices", "Join Voices", "Merge this staff as a voice on the previous staff", joinstaffs);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SwapMovements"), "SwapMovements", "Swap Movements", "Swap this movement with the one before", swapmovements);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "VoiceUp"), "VoiceUp", "Voice Up", "Go to the higher numbered voice on staff, extending selection if any", voiceup);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "VoiceDown"), "VoiceDown", "Voice Down", "Go to the lower numbered voice on this staff, extending selection if any", voicedown);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToVoiceUp"), "MoveToVoiceUp", "Move to Voice Up", "Go to the higher numbered voice on staff without altering selection", movetovoiceup);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToVoiceDown"), "MoveToVoiceDown", "Move to Voice Down", "Go to the lower numbered voice on this staff without altering selection", movetovoicedown);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddBefore"), "AddBefore", "Add Staff Before", "Inserts a new staff before the current staff", newstaffbefore);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddAfter"), "AddAfter", "Add Staff After", "Inserts/Adds a new staff after the current staff", dnm_newstaffafter);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddInitial"), "AddInitial", "Add Initial Staff", "Inserts a new staff at the top of the score", newstaffinitial);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddLast"), "AddLast", "Add Last Staff", "Inserts a new staff at the end of the score", newstafflast);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteBefore"), "DeleteBefore", "Delete Staff Before", "Deletes the staff before the current staff", delete_staff_before);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteStaff"), "DeleteStaff", "Delete Current Staff", "Deletes the current staff", delete_staff_current);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteAfter"), "DeleteAfter", "Delete Staff After", "Deletes the staff after the current staff", delete_staff_after);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddVoice"), "AddVoice", "Add Voice", "Adds a new voice (part), to the current staff. It is tricky to switch between the voices. Suggest to use merge staffs", dnm_newstaffvoice);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StaffProperties"), "StaffProperties", "Staff Properties", "Change the properties of the current staff", staff_properties_change_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InitialClef"), "InitialClef", "Initial Clef", "Change the initial clef of the current staff", clef_change_initial);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertClef"), "InsertClef", "Clef Change", "Insert/Edit a change of clef at the cursor", clef_change_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InitialKey"), "InitialKey", "Initial Key", "Set the initial key signature of the current staff", key_change_initial);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertKey"), "InsertKey", "Key Signature Change", "Insert/Edit a key change at the cursor position", key_change_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InitialTimeSig"), "InitialTimeSig", "Inital Time Signature", "Set the initial time signature of the current staff", timesig_change_initial);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertTimeSig"), "InsertTimeSig", "Time Signature Change", "Edit/Insert a time signature change for the current measure", timesig_change_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeNotehead"), "ChangeNotehead", "Set Notehead", "Change the type of notehead for the current note", set_notehead);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertStem"), "InsertStem", "Auto Stemming", "Inserts a stem neutral object. After this automatic stem directions are active. You can click on this tag and use Sharpen/StemUp etc commands to change stem direction", stem_directive_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddVerse"), "AddVerse", "Add Lyric Verse", "Add a verse of lyrics", add_verse);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteVerse"), "DeleteVerse", "Delete Verse", "Deletes current verse of lyrics from current voice", delete_verse);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditFiguredBass"), "EditFiguredBass", "Insert/Edit Figured Bass", "Add a bass figure to the current note. Use | sign to split the duration of a note so as to have multiple figures on one note. See Lilypond docs for other notation", figure_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteFiguredBass"), "DeleteFiguredBass", "Delete Figures", "Delete the figured bass on the current staff", delete_figured_bass);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "HideFiguredBass"), "HideFiguredBass", "Hide Figures (Print)", "Hide the figured bass on the current staff on printing", hide_figured_bass);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ShowFiguredBass"), "ShowFiguredBass", "Show Figures (Print)", "Show the figured bass on the current staff on printing", show_figured_bass);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditChords"), "EditChords", "Edit Chord Symbols", "Allows chord symbols to be added to the current note. E.G.cis:dim7 for c-sharp diminished 7th. See Lilypond docs for notation", fakechord_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDynamic"), "InsertDynamic", "Insert Dynamics", "Inserts a dynamic marking at the cursor position", insert_dynamic);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditObject"), "EditObject", "Edit Object", "Edit the object at the cursor.", edit_object);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditDirective"), "EditDirective", "Edit Directives", "Edit any directives attached to chord/note at cursor.", edit_object_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditStaffDirective"), "EditStaffDirective", "Edit Staff Directives", "Edit any directives attached to staff.", edit_staff_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditVoiceDirective"), "EditVoiceDirective", "Edit Voice Directives", "Edit any directives attached to voice.", edit_voice_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditScoreDirective"), "EditScoreDirective", "Edit Score Directives", "Edit any directives attached to score.", edit_score_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditMovementDirective"), "EditMovementDirective", "Edit Movement Directives", "Edit any directives attached to movement.", edit_movement_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditClefDirective"), "EditClefDirective", "Edit Clef Directives", "Edit any directives attached to clef.", edit_clef_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditTimesigDirective"), "EditTimesigDirective", "Edit Time Signature Directives", "Edit any directives attached to time signature.", edit_timesig_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditKeysigDirective"), "EditKeysigDirective", "Edit Key Signature Directives", "Edit any directives attached to key signature.", edit_keysig_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteDirective"), "DeleteDirective", "Delete a Directive", "Delete a directive attached to chord/note at cursor.", delete_chord_or_note_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AttachLilyToNote"), "AttachLilyToNote", "Attach Lilypond to Note", "Attach or edit attached LilyPond text to the note at the cursor. This can be used for guitar fingerings, cautionary accidentals and much more. See LilyPond documentation.", note_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AttachLilyToChord"), "AttachLilyToChord", "Attach Lilypond to Chord", "Attach or edit attached LilyPond text to the chord at the cursor. This can be used for attaching and placing text and much more. See LilyPond documentation.", chord_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBarline"), "InsertBarline", "Insert Barline", "Inserts specialized barline at the cursor position. Mostly not working", insert_barline);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "GoToMeasure"), "GoToMeasure", "Go to Measure", "Opens a dialog for going to a numbered measure", tomeasurenum);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "GoToBeginning"), "GoToBeginning", "Go to Beginning", "Cursor to start of staff/voice, extending selection if any", tohome);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "GoToEnd"), "GoToEnd", "Go to End", "Cursor to end of staff/voice, extending selection if any", toend);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToBeginning"), "MoveToBeginning", "Move to Staff/Voice Beginning", "Cursor to start of staff/voice, without extending selection if any", movetostart);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToEnd"), "MoveToEnd", "Move to Staff/Voice End", "Cursor to end of staff/voice, without extending selection if any", movetoend);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "NextMovement"), "NextMovement", "Next Movement", "Go to the next movement", next_movement);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PreviousMovement"), "PreviousMovement", "Previous Movement", "Go to the previous movement", prev_movement);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteMovement"), "DeleteMovement", "Delete Movement", "Delete the current movement", delete_movement);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Play"), "Play", "Play", "Play", ext_midi_playback);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Stop"), "Stop", "Stop", "Stop", stop_midi_playback);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PlaybackProperties"), "PlaybackProperties", "Playback Properties", "Allows you to specify properties used in playing back (midi)", playback_properties_change);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Help"), "Help", "Browse Manual", "Opens a browser on the user manual", browse_manual);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "About"), "About", "About", "Gives the version number etc of this program", about);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoreCommands"), "MoreCommands", "More Commands", "Allows choosing standard extra commands/menu items", morecommands);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MyCommands"), "MyCommands", "My Commands", "Allows choosing extra commands/menu items from your own collection of extras", mycommands);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "FetchCommands"), "FetchCommands", "Update Commands from Internet", "Refreshes the set of commands available from Denemo.org.\nUse More Commands after this has finished", fetchcommands);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleEdit"), "ToggleEdit", "Toggle Edit Mode", "Toggle between current mode and edit mode", toggle_edit_mode);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleRest"), "ToggleRest", "Toggle Rest Mode", "Toggle between note entry and rest entry", toggle_rest_mode);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleRhythm"), "ToggleRhythm", "Toggle Audible Feedback", "Toggle audible feedback on/off", toggle_rhythm_mode);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ClearOverlay"), "ClearOverlay", "Clear Overlay", "Clear the list of pitches that overlay the notes", clear_overlay);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "CreateRhythm"), "CreateRhythm", "Create Snippet", "Copy selection as music snippet or rhythm pattern for notes to follow as they are entered", create_rhythm_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteRhythm"), "DeleteRhythm", "Delete Snippet", "Delete the selected music snippet/rhythm pattern", delete_rhythm_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertA"), "InsertA", "Insert A","Inserts note A before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm",  InsertA);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteA"), "AddNoteA", "Insert A After","Inserts note A after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm",  AddNoteA);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddA"), "AddA", "Add A","Adds note A to the chord at cursor\nCursor height determines which octave",  AddA);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToA"), "ChangeToA", "Change to A","Changes note at cursor to nearest note A\nRhythm is unchanged",  ChangeToA);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToA"), "MoveToA", "Move to A","Moves cursor to nearest note A",  MoveToA);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertB"), "InsertB", "Insert B","Inserts note B before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm",  InsertB);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteB"), "AddNoteB", "Insert B After","Inserts note B after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm",  AddNoteB);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddB"), "AddB", "Add B","Adds note B to the chord at cursor\nCursor height determines which octave",  AddB);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToB"), "ChangeToB", "Change to B","Changes note at cursor to nearest note B\nRhythm is unchanged",  ChangeToB);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToB"), "MoveToB", "Move to B","Moves cursor to nearest note B",  MoveToB);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertC"), "InsertC", "Insert C","Inserts note C before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm",  InsertC);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteC"), "AddNoteC", "Insert C After","Inserts note C after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm",  AddNoteC);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddC"), "AddC", "Add C","Adds note C to the chord at cursor\nCursor height determines which octave",  AddC);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToC"), "ChangeToC", "Change to C","Changes note at cursor to nearest note C\nRhythm is unchanged",  ChangeToC);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToC"), "MoveToC", "Move to C","Moves cursor to nearest note C",  MoveToC);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertD"), "InsertD", "Insert D","Inserts note D before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm",  InsertD);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteD"), "AddNoteD", "Insert D After","Inserts note D after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm",  AddNoteD);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddD"), "AddD", "Add D","Adds note D to the chord at cursor\nCursor height determines which octave",  AddD);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToD"), "ChangeToD", "Change to D","Changes note at cursor to nearest note D\nRhythm is unchanged",  ChangeToD);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToD"), "MoveToD", "Move to D","Moves cursor to nearest note D",  MoveToD);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertE"), "InsertE", "Insert E","Inserts note E before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm",  InsertE);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteE"), "AddNoteE", "Insert E After","Inserts note E after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm",  AddNoteE);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddE"), "AddE", "Add E","Adds note E to the chord at cursor\nCursor height determines which octave",  AddE);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToE"), "ChangeToE", "Change to E","Changes note at cursor to nearest note E\nRhythm is unchanged",  ChangeToE);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToE"), "MoveToE", "Move to E","Moves cursor to nearest note E",  MoveToE);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertF"), "InsertF", "Insert F","Inserts note F before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm",  InsertF);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteF"), "AddNoteF", "Insert F After","Inserts note F after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm",  AddNoteF);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddF"), "AddF", "Add F","Adds note F to the chord at cursor\nCursor height determines which octave",  AddF);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToF"), "ChangeToF", "Change to F","Changes note at cursor to nearest note F\nRhythm is unchanged",  ChangeToF);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToF"), "MoveToF", "Move to F","Moves cursor to nearest note F",  MoveToF);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertG"), "InsertG", "Insert G","Inserts note G before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm",  InsertG);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteG"), "AddNoteG", "Insert G After","Inserts note G after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm",  AddNoteG);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddG"), "AddG", "Add G","Adds note G to the chord at cursor\nCursor height determines which octave",  AddG);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToG"), "ChangeToG", "Change to G","Changes note at cursor to nearest note G\nRhythm is unchanged",  ChangeToG);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToG"), "MoveToG", "Move to G","Moves cursor to nearest note G",  MoveToG);
/* putting 0 things ..*/

#define NOTE0 "<span font_desc=\"Denemo\">\xF0\x9D\x85\x9D</span>"

#define REST0 "<span font_desc=\"Denemo\">\xF0\x9D\x84\xBB</span>"

#define NOTECHAR0 "\xF0\x9D\x85\x9D"

#define RESTCHAR0 "\xF0\x9D\x84\xBB"
register_command(Denemo.map, gtk_action_group_get_action(action_group, "0"), "0", NOTE0, "When appending, appends a "NOTECHAR0" \nWith the cursor on a note inserts a "NOTECHAR0"  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch", Dur0);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change0"), "Change0", NOTE0, "Change the current note to a "NOTECHAR0, ChangeDur0);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert0"), "Insert0", NOTE0, "Insert a "NOTECHAR0, InsertDur0);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest0"), "InsertRest0",  "Insert a "RESTCHAR0"" ,  "Inserts a rest at cursor position\nSets prevailing rhythm to "NOTECHAR0, InsertRest0);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest0"), "ChangeRest0",  "Change a "RESTCHAR0" " ,  "Changes a rest at cursor position\nSets prevailing rhythm to "NOTECHAR0, ChangeRest0);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set0"), "Set0", NOTE0, "Set the prevailing duration to "NOTECHAR0, SetDur0);

#undef NOTE0

#undef REST0

#undef NOTECHAR0

#undef RESTCHAR0
/* putting 1 things ..*/

#define NOTE1 "<span font_desc=\"Denemo\">\xF0\x9D\x85\x9E</span>"

#define REST1 "<span font_desc=\"Denemo\">\xF0\x9D\x84\xBC</span>"

#define NOTECHAR1 "\xF0\x9D\x85\x9E"

#define RESTCHAR1 "\xF0\x9D\x84\xBC"
register_command(Denemo.map, gtk_action_group_get_action(action_group, "1"), "1", NOTE1, "When appending, appends a "NOTECHAR1" \nWith the cursor on a note inserts a "NOTECHAR1"  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch", Dur1);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change1"), "Change1", NOTE1, "Change the current note to a "NOTECHAR1, ChangeDur1);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert1"), "Insert1", NOTE1, "Insert a "NOTECHAR1, InsertDur1);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest1"), "InsertRest1",  "Insert a "RESTCHAR1"" ,  "Inserts a rest at cursor position\nSets prevailing rhythm to "NOTECHAR1, InsertRest1);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest1"), "ChangeRest1",  "Change a "RESTCHAR1" " ,  "Changes a rest at cursor position\nSets prevailing rhythm to "NOTECHAR1, ChangeRest1);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set1"), "Set1", NOTE1, "Set the prevailing duration to "NOTECHAR1, SetDur1);

#undef NOTE1

#undef REST1

#undef NOTECHAR1

#undef RESTCHAR1
/* putting 2 things ..*/

#define NOTE2 "<span font_desc=\"Denemo\">\xF0\x9D\x85\x9F</span>"

#define REST2 "<span font_desc=\"Denemo\">\xF0\x9D\x84\xBD</span>"

#define NOTECHAR2 "\xF0\x9D\x85\x9F"

#define RESTCHAR2 "\xF0\x9D\x84\xBD"
register_command(Denemo.map, gtk_action_group_get_action(action_group, "2"), "2", NOTE2, "When appending, appends a "NOTECHAR2" \nWith the cursor on a note inserts a "NOTECHAR2"  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch", Dur2);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change2"), "Change2", NOTE2, "Change the current note to a "NOTECHAR2, ChangeDur2);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert2"), "Insert2", NOTE2, "Insert a "NOTECHAR2, InsertDur2);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest2"), "InsertRest2",  "Insert a "RESTCHAR2"" ,  "Inserts a rest at cursor position\nSets prevailing rhythm to "NOTECHAR2, InsertRest2);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest2"), "ChangeRest2",  "Change a "RESTCHAR2" " ,  "Changes a rest at cursor position\nSets prevailing rhythm to "NOTECHAR2, ChangeRest2);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set2"), "Set2", NOTE2, "Set the prevailing duration to "NOTECHAR2, SetDur2);

#undef NOTE2

#undef REST2

#undef NOTECHAR2

#undef RESTCHAR2
/* putting 3 things ..*/

#define NOTE3 "<span font_desc=\"Denemo\">\xF0\x9D\x85\xA0</span>"

#define REST3 "<span font_desc=\"Denemo\">\xF0\x9D\x84\xBE</span>"

#define NOTECHAR3 "\xF0\x9D\x85\xA0"

#define RESTCHAR3 "\xF0\x9D\x84\xBE"
register_command(Denemo.map, gtk_action_group_get_action(action_group, "3"), "3", NOTE3, "When appending, appends a "NOTECHAR3" \nWith the cursor on a note inserts a "NOTECHAR3"  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch", Dur3);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change3"), "Change3", NOTE3, "Change the current note to a "NOTECHAR3, ChangeDur3);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert3"), "Insert3", NOTE3, "Insert a "NOTECHAR3, InsertDur3);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest3"), "InsertRest3",  "Insert a "RESTCHAR3"" ,  "Inserts a rest at cursor position\nSets prevailing rhythm to "NOTECHAR3, InsertRest3);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest3"), "ChangeRest3",  "Change a "RESTCHAR3" " ,  "Changes a rest at cursor position\nSets prevailing rhythm to "NOTECHAR3, ChangeRest3);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set3"), "Set3", NOTE3, "Set the prevailing duration to "NOTECHAR3, SetDur3);

#undef NOTE3

#undef REST3

#undef NOTECHAR3

#undef RESTCHAR3
/* putting 4 things ..*/

#define NOTE4 "<span font_desc=\"Denemo\">\xF0\x9D\x85\xA1</span>"

#define REST4 "<span font_desc=\"Denemo\">\xF0\x9D\x84\xBF</span>"

#define NOTECHAR4 "\xF0\x9D\x85\xA1"

#define RESTCHAR4 "\xF0\x9D\x84\xBF"
register_command(Denemo.map, gtk_action_group_get_action(action_group, "4"), "4", NOTE4, "When appending, appends a "NOTECHAR4" \nWith the cursor on a note inserts a "NOTECHAR4"  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch", Dur4);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change4"), "Change4", NOTE4, "Change the current note to a "NOTECHAR4, ChangeDur4);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert4"), "Insert4", NOTE4, "Insert a "NOTECHAR4, InsertDur4);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest4"), "InsertRest4",  "Insert a "RESTCHAR4"" ,  "Inserts a rest at cursor position\nSets prevailing rhythm to "NOTECHAR4, InsertRest4);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest4"), "ChangeRest4",  "Change a "RESTCHAR4" " ,  "Changes a rest at cursor position\nSets prevailing rhythm to "NOTECHAR4, ChangeRest4);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set4"), "Set4", NOTE4, "Set the prevailing duration to "NOTECHAR4, SetDur4);

#undef NOTE4

#undef REST4

#undef NOTECHAR4

#undef RESTCHAR4
/* putting 5 things ..*/

#define NOTE5 "<span font_desc=\"Denemo\">\xF0\x9D\x85\xA2</span>"

#define REST5 "<span font_desc=\"Denemo\">\xF0\x9D\x85\x80</span>"

#define NOTECHAR5 "\xF0\x9D\x85\xA2"

#define RESTCHAR5 "\xF0\x9D\x85\x80"
register_command(Denemo.map, gtk_action_group_get_action(action_group, "5"), "5", NOTE5, "When appending, appends a "NOTECHAR5" \nWith the cursor on a note inserts a "NOTECHAR5"  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch", Dur5);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change5"), "Change5", NOTE5, "Change the current note to a "NOTECHAR5, ChangeDur5);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert5"), "Insert5", NOTE5, "Insert a "NOTECHAR5, InsertDur5);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest5"), "InsertRest5",  "Insert a "RESTCHAR5"" ,  "Inserts a rest at cursor position\nSets prevailing rhythm to "NOTECHAR5, InsertRest5);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest5"), "ChangeRest5",  "Change a "RESTCHAR5" " ,  "Changes a rest at cursor position\nSets prevailing rhythm to "NOTECHAR5, ChangeRest5);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set5"), "Set5", NOTE5, "Set the prevailing duration to "NOTECHAR5, SetDur5);

#undef NOTE5

#undef REST5

#undef NOTECHAR5

#undef RESTCHAR5
/* putting 6 things ..*/

#define NOTE6 "<span font_desc=\"Denemo\">\xF0\x9D\x85\xA3</span>"

#define REST6 "<span font_desc=\"Denemo\">\xF0\x9D\x85\x81</span>"

#define NOTECHAR6 "\xF0\x9D\x85\xA3"

#define RESTCHAR6 "\xF0\x9D\x85\x81"
register_command(Denemo.map, gtk_action_group_get_action(action_group, "6"), "6", NOTE6, "When appending, appends a "NOTECHAR6" \nWith the cursor on a note inserts a "NOTECHAR6"  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch", Dur6);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change6"), "Change6", NOTE6, "Change the current note to a "NOTECHAR6, ChangeDur6);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert6"), "Insert6", NOTE6, "Insert a "NOTECHAR6, InsertDur6);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest6"), "InsertRest6",  "Insert a "RESTCHAR6"" ,  "Inserts a rest at cursor position\nSets prevailing rhythm to "NOTECHAR6, InsertRest6);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest6"), "ChangeRest6",  "Change a "RESTCHAR6" " ,  "Changes a rest at cursor position\nSets prevailing rhythm to "NOTECHAR6, ChangeRest6);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set6"), "Set6", NOTE6, "Set the prevailing duration to "NOTECHAR6, SetDur6);

#undef NOTE6

#undef REST6

#undef NOTECHAR6

#undef RESTCHAR6
/* putting 7 things ..*/

#define NOTE7 "<span font_desc=\"Denemo\">\xF0\x9D\x85\xA4</span>"

#define REST7 "<span font_desc=\"Denemo\">\xF0\x9D\x85\x82</span>"

#define NOTECHAR7 "\xF0\x9D\x85\xA4"

#define RESTCHAR7 "\xF0\x9D\x85\x82"
register_command(Denemo.map, gtk_action_group_get_action(action_group, "7"), "7", NOTE7, "When appending, appends a "NOTECHAR7" \nWith the cursor on a note inserts a "NOTECHAR7"  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch", Dur7);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change7"), "Change7", NOTE7, "Change the current note to a "NOTECHAR7, ChangeDur7);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert7"), "Insert7", NOTE7, "Insert a "NOTECHAR7, InsertDur7);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest7"), "InsertRest7",  "Insert a "RESTCHAR7"" ,  "Inserts a rest at cursor position\nSets prevailing rhythm to "NOTECHAR7, InsertRest7);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest7"), "ChangeRest7",  "Change a "RESTCHAR7" " ,  "Changes a rest at cursor position\nSets prevailing rhythm to "NOTECHAR7, ChangeRest7);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set7"), "Set7", NOTE7, "Set the prevailing duration to "NOTECHAR7, SetDur7);

#undef NOTE7

#undef REST7

#undef NOTECHAR7

#undef RESTCHAR7
/* putting 8 things ..*/

#define NOTE8 "<span font_desc=\"Denemo\">\xF0\x9D\x85\xA5</span>"

#define REST8 "<span font_desc=\"Denemo\">\xF0\x9D\x85\x83</span>"

#define NOTECHAR8 "\xF0\x9D\x85\xA5"

#define RESTCHAR8 "\xF0\x9D\x85\x83"
register_command(Denemo.map, gtk_action_group_get_action(action_group, "8"), "8", NOTE8, "When appending, appends a "NOTECHAR8" \nWith the cursor on a note inserts a "NOTECHAR8"  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch", Dur8);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change8"), "Change8", NOTE8, "Change the current note to a "NOTECHAR8, ChangeDur8);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert8"), "Insert8", NOTE8, "Insert a "NOTECHAR8, InsertDur8);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest8"), "InsertRest8",  "Insert a "RESTCHAR8"" ,  "Inserts a rest at cursor position\nSets prevailing rhythm to "NOTECHAR8, InsertRest8);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest8"), "ChangeRest8",  "Change a "RESTCHAR8" " ,  "Changes a rest at cursor position\nSets prevailing rhythm to "NOTECHAR8, ChangeRest8);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set8"), "Set8", NOTE8, "Set the prevailing duration to "NOTECHAR8, SetDur8);

#undef NOTE8

#undef REST8

#undef NOTECHAR8

#undef RESTCHAR8
