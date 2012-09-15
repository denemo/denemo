register_command(Denemo.map, gtk_action_group_get_action(action_group, "CursorLeft"), "CursorLeft", N_("Cursor Left"), N_("Moves the cursor one object left, altering the selection if any"), cursorleft);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveCursorLeft"), "MoveCursorLeft", N_("Move Cursor Left"), N_("Moves the cursor one object left, without altering the selection"), movecursorleft);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "CursorDown"), "CursorDown", N_("Cursor Down"), N_("Moves the cursor one scale step down"), cursordown);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "CursorUp"), "CursorUp", N_("Cursor Up"), N_("Moves the cursor one scale step up"), cursorup);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "CursorRight"), "CursorRight", N_("Cursor Right"), N_("Moves the cursor one object right, altering the selection if any"), cursorright);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveCursorRight"), "MoveCursorRight", N_("Move Cursor Right"), N_("Moves the cursor one object right, without altering the selection"), movecursorright);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "GoToMark"), "GoToMark", N_("To Mark"), N_("Moves the cursor to the Mark without altering the selection"), goto_mark);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SwapPointAndMark"), "SwapPointAndMark", N_("Swap Ends of Selection"), N_("Swaps the active end of the selection"), swap_point_and_mark);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "GoToSelectionStart"), "GoToSelectionStart", N_("To Selection Start"), N_("Moves the cursor to the first object in the selection without altering the selection. returns #f if no selection"), goto_selection_start);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PushPosition"), "PushPosition", N_("Push Position"), N_("Pushes the current cursor position onto a stack"), PushPosition);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PopPosition"), "PopPosition", N_("Pop Position"), N_("Pops a position from the stack of cursor positions, moving the cursor there"), PopPosition);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PopPushPosition"), "PopPushPosition", N_("Pop and Push Position"), N_("Pops a position from the stack of cursor positions, pushes the current position, then moves the cursor to the popped position"), PopPushPosition);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleReduceToDrawingArea"), "ToggleReduceToDrawingArea", N_("Hide/Show Menus"), N_("Hides/Shows menus, panes etc. The ones shown are those checked in the view menu."), ToggleReduceToDrawingArea);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StaffUp"), "StaffUp", N_("Staff Up"), N_("Moves the cursor to the staff above, extending selection if any"), staffup);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StaffDown"), "StaffDown", N_("Staff Down"), N_("Moves the cursor to the staff below, extending selection if any"), staffdown);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToStaffUp"), "MoveToStaffUp", N_("Move to Staff Up"), N_("Moves the cursor to the staff above without altering selection"), movetostaffup);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToStaffDown"), "MoveToStaffDown", N_("Move to Staff Down"), N_("Moves the cursor to the staff below  without altering selection"), movetostaffdown);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MeasureLeft"), "MeasureLeft", N_("Measure Left"), N_("Moves the cursor to the first object in the next measure, extending selection if any"), measureleft);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MeasureRight"), "MeasureRight", N_("Measure Right"), N_("Moves the cursor to the first object in the previous measure, extending selection if any"), measureright);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToMeasureLeft"), "MoveToMeasureLeft", N_("Move to Measure Left"), N_("Moves the cursor to the first object in the next measure leaving selection, if any, unchanged"), movetomeasureleft);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToMeasureRight"), "MoveToMeasureRight", N_("Move to Measure Right"), N_("Moves the cursor to the first object in the previous measureleaving selection, if any, unchanged"), movetomeasureright);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "A"), "A", N_("A"), N_("Append/Edit  A"), go_to_A_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "B"), "B", N_("B"), N_("Append/Edit  B"), go_to_B_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "C"), "C", N_("C"), N_("Append/Edit  C"), go_to_C_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "D"), "D", N_("D"), N_("Append/Edit  D"), go_to_D_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "E"), "E", N_("E"), N_("Append/Edit  E"), go_to_E_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "F"), "F", N_("F"), N_("Append/Edit  F"), go_to_F_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "G"), "G", N_("G"), N_("Append/Edit  G"), go_to_G_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OctaveUp"), "OctaveUp", N_("Octave Up"), N_("Octave Up"), octave_up_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OctaveDown"), "OctaveDown", N_("Octave Down"), N_("Octave Down"), octave_down_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "WholeNote"), "WholeNote", N_("WholeNote"), N_("Insert \xF0\x9D\x85\x9D"), insert_chord_0key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "HalfNote"), "HalfNote", N_("HalfNote"), N_("Insert \xF0\x9D\x85\x9E"), insert_chord_1key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "QuarterNote"), "QuarterNote", N_("QuarterNote"), N_("Insert \xF0\x9D\x85\x9F"), insert_chord_2key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EighthNote"), "EighthNote", N_("EighthNote"), N_("Insert \xF0\x9D\x85\xA0"), insert_chord_3key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SixteenthNote"), "SixteenthNote", N_("SixteenthNote"), N_("Insert \xF0\x9D\x85\xA1"), insert_chord_4key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ThirtysecondNote"), "ThirtysecondNote", N_("ThirtysecondNote"), N_("Insert \xF0\x9D\x85\xA2"), insert_chord_5key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SixtyfourthNote"), "SixtyfourthNote", N_("SixtyfourthNote"), N_("Insert \xF0\x9D\x85\xA3"), insert_chord_6key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OneHundredTwentyEighthNote"), "OneHundredTwentyEighthNote", N_("OneHundredTwentyEighthNote"), N_("Insert \xF0\x9D\x85\xA4"), insert_chord_7key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "TwoHundredFiftySixthNote"), "TwoHundredFiftySixthNote", N_("TwoHundredFiftySixthNote"), N_("Insert \xF0\x9D\x85\xA5"), insert_chord_8key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertWholeRest"), "InsertWholeRest", N_("<span font_desc=\"Denemo\">\xF0\x9D\x84\xBB</span>"), N_("Insert \xF0\x9D\x84\xBB rest"), insert_rest_0key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertHalfRest"), "InsertHalfRest", N_("<span font_desc=\"Denemo\">\xF0\x9D\x84\xBC</span>"), N_("Insert \xF0\x9D\x84\xBC rest"), insert_rest_1key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertQuarterRest"), "InsertQuarterRest", N_("<span font_desc=\"Denemo\">\xF0\x9D\x84\xBD</span>"), N_("Insert \xF0\x9D\x84\xBD rest"), insert_rest_2key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertEighthRest"), "InsertEighthRest", N_("<span font_desc=\"Denemo\">\xF0\x9D\x84\xBE</span>"), N_("Insert \xF0\x9D\x84\xBE rest"), insert_rest_3key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertSixteenthRest"), "InsertSixteenthRest", N_("<span font_desc=\"Denemo\">\xF0\x9D\x84\xBF</span>"), N_("Insert \xF0\x9D\x84\xBF rest"), insert_rest_4key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertThirtysecondRest"), "InsertThirtysecondRest", N_("<span font_desc=\"Denemo\">\xF0\x9D\x85\x80</span>"), N_("Insert \xF0\x9D\x85\x80 rest"), insert_rest_5key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertSixtyfourthRest"), "InsertSixtyfourthRest", N_("<span font_desc=\"Denemo\">\xF0\x9D\x85\x81</span>"), N_("Insert \xF0\x9D\x85\x81 rest"), insert_rest_6key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankWholeNote"), "InsertBlankWholeNote", N_("InsertBlankWholeNote"), N_("Insert a non-printing \xF0\x9D\x84\xBB rest"), insert_blankchord_0key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankHalfNote"), "InsertBlankHalfNote", N_("InsertBlankHalfNote"), N_("Insert a non-printing \xF0\x9D\x84\xBC rest"), insert_blankchord_1key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankQuarterNote"), "InsertBlankQuarterNote", N_("InsertBlankQuarterNote"), N_("Insert a non-printing \xF0\x9D\x84\xBD rest"), insert_blankchord_2key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankEighthNote"), "InsertBlankEighthNote", N_("InsertBlankEighthNote"), N_("Insert a non-printing \xF0\x9D\x84\xBE rest"), insert_blankchord_3key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankSixteenthNote"), "InsertBlankSixteenthNote", N_("InsertBlankSixteenthNote"), N_("Insert a non-printing \xF0\x9D\x84\xBF rest"), insert_blankchord_4key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankThirtysecondNote"), "InsertBlankThirtysecondNote", N_("InsertBlankThirtysecondNote"), N_("Insert a non-printing \xF0\x9D\x85\x80 rest"), insert_blankchord_5key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankSixtyfourthNote"), "InsertBlankSixtyfourthNote", N_("InsertBlankSixtyfourthNote"), N_("Insert a non-printing \xF0\x9D\x85\x81 rest"), insert_blankchord_6key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankOneHundredTwentyEighthNote"), "InsertBlankOneHundredTwentyEighthNote", N_("InsertBlankOneHundredTwentyEighthNote"), N_("Insert a non-printing \xF0\x9D\x85\x82 rest"), insert_blankchord_7key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankTwoHundredFiftySixthNote"), "InsertBlankTwoHundredFiftySixthNote", N_("InsertBlankTwoHundredFiftySixthNote"), N_("Insert a non-printing \xF0\x9D\x85\x83 rest"), insert_blankchord_8key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleRestMode"), "ToggleRestMode", N_("Toggle Rest Mode"), N_("No Tooltip yet"), rest_toggle_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleBlankMode"), "ToggleBlankMode", N_("Toggle Blank Mode"), N_("No Tooltip yet"), toggle_blank);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDuplet"), "InsertDuplet", N_("Insert Duplet"), N_("No Tooltip yet"), insert_duplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertTriplet"), "InsertTriplet", N_("Insert Triplet"), N_("No Tooltip yet"), insert_triplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StartTriplet"), "StartTriplet", N_("Start Triplet"), N_("No Tooltip yet"), start_triplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EndTuplet"), "EndTuplet", N_("End Tuplet"), N_("No Tooltip yet"), end_tuplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertQuadtuplet"), "InsertQuadtuplet", N_("Insert Quadtuplet"), N_("No Tooltip yet"), insert_quadtuplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertQuintuplet"), "InsertQuintuplet", N_("Insert Quintuplet"), N_("No Tooltip yet"), insert_quintuplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertSextuplet"), "InsertSextuplet", N_("Insert Sextuplet"), N_("No Tooltip yet"), insert_sextuplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertSeptuplet"), "InsertSeptuplet", N_("Insert Septuplet"), N_("No Tooltip yet"), insert_septuplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteToChord"), "AddNoteToChord", N_("Add note"), N_("Add a note to the current chord\nThe cursor position determines which note to add"), add_tone_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "RemoveNoteFromChord"), "RemoveNoteFromChord", N_("Remove note"), N_("Remove a note from the current chord"), remove_tone_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Sharpen"), "Sharpen", N_("Sharpen"), N_("No Tooltip yet"), sharpen_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Flatten"), "Flatten", N_("Flatten"), N_("No Tooltip yet"), flatten_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PendingSharpen"), "PendingSharpen", N_("Sharpen Next Note"), N_("Increases the sharpness of the next entered note. The status bar shows the current state."), pending_sharpen);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PendingFlatten"), "PendingFlatten", N_("Flatten Next Note"), N_("Increases the flatness of the next entered note. The status bar shows the current state."), pending_flatten);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StemUp"), "StemUp", N_("StemUp"), N_("Alters a StemNeutral object to stem up."), stem_up);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StemDown"), "StemDown", N_("StemDown"), N_("Alters a StemNeutral object to stem down."), stem_down);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddDot"), "AddDot", N_("Add Dot"), N_("No Tooltip yet"), add_dot_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "RemoveDot"), "RemoveDot", N_("Remove Dot"), N_("No Tooltip yet"), remove_dot_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertTiedNote"), "InsertTiedNote", N_("Tied note"), N_("Inserts a duplicate of the current note, tied"), tie_notes_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleTie"), "ToggleTie", N_("Toggle Tie"), N_("Ties/unties the note at the cursor"), toggle_tie);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteObject"), "DeleteObject", N_("Delete Object"), N_("Delete the object at the cursor"), deleteobject);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeletePreviousObject"), "DeletePreviousObject", N_("Delete Previous Object"), N_("Delete to the left of the cursor."), deletepreviousobject);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertMeasure"), "InsertMeasure", N_("Insert Measure Before"), N_("Insert a blank measure before the current one (in all staffs)"), insert_measure_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddMeasure"), "AddMeasure", N_("Insert Measure After"), N_("Insert a blank measure after the current one (in all staffs)"), addmeasureafter);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertMeasureBefore"), "InsertMeasureBefore", N_("Staff Insert Measure Before"), N_("Insert a blank measure before the current one (in current staff)"), insertmeasurebefore);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertMeasureAfter"), "InsertMeasureAfter", N_("Staff Insert Measure After"), N_("Insert a blank measure in current staff after the current measure"), insertmeasureafter);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AppendMeasure"), "AppendMeasure", N_("Staff Append Measure"), N_("No Tooltip yet"), append_measure_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteMeasure"), "DeleteMeasure", N_("Staff Delete Measure"), N_("Delete the current measure in this staff, leaving the staff short"), deletemeasure);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteMeasureAllStaffs"), "DeleteMeasureAllStaffs", N_("Delete Measure All Staffs"), N_("Delete the current measure in all staffs"), deletemeasureallstaffs);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ShrinkMeasures"), "ShrinkMeasures", N_("Shrink Measure"), N_("No Tooltip yet"), adjust_measure_less_width_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "WidenMeasures"), "WidenMeasures", N_("Widen Measures"), N_("No Tooltip yet"), adjust_measure_more_width_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ShorterStaffs"), "ShorterStaffs", N_("Shorter Staffs"), N_("No Tooltip yet"), adjust_staff_less_height_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "TallerStaffs"), "TallerStaffs", N_("Taller Staffs"), N_("No Tooltip yet"), adjust_staff_more_height_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertTrebleClef"), "InsertTrebleClef", N_("New Treble Clef"), N_("No Tooltip yet"), newcleftreble);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBassClef"), "InsertBassClef", N_("New Bass Clef"), N_("No Tooltip yet"), newclefbass);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insertg8clef"), "Insertg8clef", N_("New G8 Clef"), N_("No Tooltip yet"), newclefg8);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertAltoClef"), "InsertAltoClef", N_("New Alto Clef"), N_("No Tooltip yet"), newclefalto);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertTenorClef"), "InsertTenorClef", N_("New Tenor Clef"), N_("No Tooltip yet"), newcleftenor);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertSopranoClef"), "InsertSopranoClef", N_("New Soprano Clef"), N_("No Tooltip yet"), newclefsoprano);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialTrebleClef"), "SetInitialTrebleClef", N_("Set Treble Clef"), N_("No Tooltip yet"), setcleftreble);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialBassClef"), "SetInitialBassClef", N_("Set Bass Clef"), N_("No Tooltip yet"), setclefbass);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialg8clef"), "SetInitialg8clef", N_("Set G8 Clef"), N_("No Tooltip yet"), setclefg8);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialAltoClef"), "SetInitialAltoClef", N_("Set Alto Clef"), N_("No Tooltip yet"), setclefalto);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialTenorClef"), "SetInitialTenorClef", N_("Set Tenor Clef"), N_("No Tooltip yet"), setcleftenor);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialSopranoClef"), "SetInitialSopranoClef", N_("Set Soprano Clef"), N_("No Tooltip yet"), setclefsoprano);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert22Time"), "Insert22Time", N_("Insert 2/2 Time"), N_("No Tooltip yet"), newtimesig22);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert32Time"), "Insert32Time", N_("Insert 3/2 Time"), N_("No Tooltip yet"), newtimesig32);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert42Time"), "Insert42Time", N_("Insert 4/2 Time"), N_("No Tooltip yet"), newtimesig42);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert44Time"), "Insert44Time", N_("Insert 4/4 Time"), N_("No Tooltip yet"), newtimesig44);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert34Time"), "Insert34Time", N_("Insert 3/4 Time"), N_("No Tooltip yet"), newtimesig34);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert24Time"), "Insert24Time", N_("Insert 2/4 Time"), N_("No Tooltip yet"), newtimesig24);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert64Time"), "Insert64Time", N_("Insert 6/4 Time"), N_("No Tooltip yet"), newtimesig64);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert38Time"), "Insert38Time", N_("Insert 3/8 Time"), N_("No Tooltip yet"), newtimesig38);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert68Time"), "Insert68Time", N_("Insert 6/8 Time"), N_("No Tooltip yet"), newtimesig68);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert128Time"), "Insert128Time", N_("Insert 12/8 Time"), N_("No Tooltip yet"), newtimesig128);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert98Time"), "Insert98Time", N_("Insert 9/8 Time"), N_("No Tooltip yet"), newtimesig98);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set22Time"), "Set22Time", N_("Set 2/2 Time"), N_("No Tooltip yet"), settimesig22);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set32Time"), "Set32Time", N_("Set 3/2 Time"), N_("No Tooltip yet"), settimesig32);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set42Time"), "Set42Time", N_("Set 4/2 Time"), N_("No Tooltip yet"), settimesig42);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set44Time"), "Set44Time", N_("Set 4/4 Time"), N_("No Tooltip yet"), settimesig44);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set34Time"), "Set34Time", N_("Set 3/4 Time"), N_("No Tooltip yet"), settimesig34);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set24Time"), "Set24Time", N_("Set 2/4 Time"), N_("No Tooltip yet"), settimesig24);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set64Time"), "Set64Time", N_("Set 6/4 Time"), N_("No Tooltip yet"), settimesig64);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set38Time"), "Set38Time", N_("Set 3/8 Time"), N_("No Tooltip yet"), settimesig38);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set68Time"), "Set68Time", N_("Set 6/8 Time"), N_("No Tooltip yet"), settimesig68);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set128Time"), "Set128Time", N_("Set 12/8 Time"), N_("No Tooltip yet"), settimesig128);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set98Time"), "Set98Time", N_("Set 9/8 Time"), N_("No Tooltip yet"), settimesig98);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertCmaj"), "InsertCmaj", N_("Insert Cmaj"), N_("No Tooltip yet"), newkeysigcmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertGmaj"), "InsertGmaj", N_("Insert Gmaj"), N_("No Tooltip yet"), newkeysiggmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDmaj"), "InsertDmaj", N_("Insert Dmaj"), N_("No Tooltip yet"), newkeysigdmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertAmaj"), "InsertAmaj", N_("Insert Amaj"), N_("No Tooltip yet"), newkeysigamaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertEmaj"), "InsertEmaj", N_("Insert Emaj"), N_("No Tooltip yet"), newkeysigemaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBmaj"), "InsertBmaj", N_("Insert Bmaj"), N_("No Tooltip yet"), newkeysigbmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertFSharpmaj"), "InsertFSharpmaj", N_("Insert F# Major"), N_("No Tooltip yet"), newkeysigfsharpmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertCSharpmaj"), "InsertCSharpmaj", N_("Insert C# Major"), N_("No Tooltip yet"), newkeysigcsharpmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertFmaj"), "InsertFmaj", N_("Insert F Major"), N_("No Tooltip yet"), newkeysigfmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBflatmaj"), "InsertBflatmaj", N_("Insert Bb Major"), N_("No Tooltip yet"), newkeysigbflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertEflatmaj"), "InsertEflatmaj", N_("Insert Eb Major"), N_("No Tooltip yet"), newkeysigeflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertAflatmaj"), "InsertAflatmaj", N_("Insert Ab Major"), N_("No Tooltip yet"), newkeysigaflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDflatmaj"), "InsertDflatmaj", N_("Insert Db Major"), N_("No Tooltip yet"), newkeysigdflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertGflatmaj"), "InsertGflatmaj", N_("Insert Gb Major"), N_("No Tooltip yet"), newkeysiggflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertCflatmaj"), "InsertCflatmaj", N_("Insert Cb Major"), N_("No Tooltip yet"), newkeysigcflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertAmin"), "InsertAmin", N_("Insert A Minor"), N_("No Tooltip yet"), newkeysigamin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertEmin"), "InsertEmin", N_("Insert E Minor"), N_("No Tooltip yet"), newkeysigemin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBmin"), "InsertBmin", N_("Insert B Minor"), N_("No Tooltip yet"), newkeysigbmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertFSharpmin"), "InsertFSharpmin", N_("Insert F# Minor"), N_("No Tooltip yet"), newkeysigfsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertCSharpmin"), "InsertCSharpmin", N_("Insert C# Minor"), N_("No Tooltip yet"), newkeysigcsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertGSharpmin"), "InsertGSharpmin", N_("Insert G# Minor"), N_("No Tooltip yet"), newkeysiggsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDSharpmin"), "InsertDSharpmin", N_("Insert D# Minor"), N_("No Tooltip yet"), newkeysigdsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertASharpmin"), "InsertASharpmin", N_("Insert A# Minor"), N_("No Tooltip yet"), newkeysigasharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDmin"), "InsertDmin", N_("Insert D Minor"), N_("No Tooltip yet"), newkeysigdmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertGmin"), "InsertGmin", N_("Insert G Minor"), N_("No Tooltip yet"), newkeysiggmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertCmin"), "InsertCmin", N_("Insert C Minor"), N_("No Tooltip yet"), newkeysigcmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertFmin"), "InsertFmin", N_("Insert F Minor"), N_("No Tooltip yet"), newkeysigfmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBflatmin"), "InsertBflatmin", N_("Insert Bb Minor"), N_("No Tooltip yet"), newkeysigbflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertEflatmin"), "InsertEflatmin", N_("Insert Eb Minor"), N_("No Tooltip yet"), newkeysigeflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertAflatmin"), "InsertAflatmin", N_("Insert Ab Minor"), N_("No Tooltip yet"), newkeysigaflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialCmaj"), "SetInitialCmaj", N_("Set Initial Keysig to C Major"), N_("No Tooltip yet"), setkeysigcmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialGmaj"), "SetInitialGmaj", N_("Set Initial Keysig to G Major"), N_("No Tooltip yet"), setkeysiggmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialDmaj"), "SetInitialDmaj", N_("Set D Major as Initial Keysig"), N_("No Tooltip yet"), setkeysigdmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialAmaj"), "SetInitialAmaj", N_("Set A Major as Initial Keysig"), N_("No Tooltip yet"), setkeysigamaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialEmaj"), "SetInitialEmaj", N_("Set E Major as Initial Keysig"), N_("No Tooltip yet"), setkeysigemaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialBmaj"), "SetInitialBmaj", N_("Set B Major as Initial Keysig"), N_("No Tooltip yet"), setkeysigbmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialFSharpmaj"), "SetInitialFSharpmaj", N_("Set F# Major as Initial Keysig"), N_("No Tooltip yet"), setkeysigfsharpmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialCSharpmaj"), "SetInitialCSharpmaj", N_("Set C# Major as Initial Keysig"), N_("No Tooltip yet"), setkeysigcsharpmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialFmaj"), "SetInitialFmaj", N_("Set F Major as Initial Keysig"), N_("No Tooltip yet"), setkeysigfmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialBflatmaj"), "SetInitialBflatmaj", N_("Set Bb Major as Initial Keysig"), N_("No Tooltip yet"), setkeysigbflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialEflatmaj"), "SetInitialEflatmaj", N_("Set Eb Major as Initial Keysig"), N_("No Tooltip yet"), setkeysigeflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialAflatmaj"), "SetInitialAflatmaj", N_("Set Ab Major as Initial Keysig"), N_("No Tooltip yet"), setkeysigaflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialDflatmaj"), "SetInitialDflatmaj", N_("Set Db Major as Initial Keysig"), N_("No Tooltip yet"), setkeysigdflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialGflatmaj"), "SetInitialGflatmaj", N_("Set Gb Major as Initial Keysig"), N_("No Tooltip yet"), setkeysiggflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialCflatmaj"), "SetInitialCflatmaj", N_("Set Cb Major as Initial Keysig"), N_("No Tooltip yet"), setkeysigcflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialAmin"), "SetInitialAmin", N_("Set A Minor as Initial Keysig"), N_("No Tooltip yet"), setkeysigamin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialEmin"), "SetInitialEmin", N_("Set E Minor as Initial Keysig"), N_("No Tooltip yet"), setkeysigemin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialBmin"), "SetInitialBmin", N_("Set B Minor as Initial Keysig"), N_("No Tooltip yet"), setkeysigbmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialFSharpmin"), "SetInitialFSharpmin", N_("Set F# Minor as Initial Keysig"), N_("No Tooltip yet"), setkeysigfsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialCSharpmin"), "SetInitialCSharpmin", N_("Set C# Minor as Initial Keysig"), N_("No Tooltip yet"), setkeysigcsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialGSharpmin"), "SetInitialGSharpmin", N_("Set G# Minor as Initial Keysig"), N_("No Tooltip yet"), setkeysiggsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialDSharpmin"), "SetInitialDSharpmin", N_("Set D# Minor as Initial Keysig"), N_("No Tooltip yet"), setkeysigdsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialASharpmin"), "SetInitialASharpmin", N_("Set A# Minor as Initial Keysig"), N_("No Tooltip yet"), setkeysigasharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialDmin"), "SetInitialDmin", N_("Set D Minor as Initial Keysig"), N_("No Tooltip yet"), setkeysigdmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialGmin"), "SetInitialGmin", N_("Set G Minor as Initial Keysig"), N_("No Tooltip yet"), setkeysiggmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialCmin"), "SetInitialCmin", N_("Set C Minor as Initial Keysig"), N_("No Tooltip yet"), setkeysigcmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialFmin"), "SetInitialFmin", N_("Set F Minor as Initial Keysig"), N_("No Tooltip yet"), setkeysigfmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialBflatmin"), "SetInitialBflatmin", N_("Set Bb Minor as Initial Keysig"), N_("No Tooltip yet"), setkeysigbflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialEflatmin"), "SetInitialEflatmin", N_("Set Eb Minor as Initial Keysig"), N_("No Tooltip yet"), setkeysigeflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialAflatmin"), "SetInitialAflatmin", N_("Set Ab Minor as Initial Keysig"), N_("No Tooltip yet"), setkeysigaflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetMark"), "SetMark", N_("Set Mark"), N_("Sets the start point for a selection,\nthe end point of the selection is unaltered"), set_mark);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "UnsetMark"), "UnsetMark", N_("Unset Mark"), N_("Gets rid of the selection."), unset_mark);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetPoint"), "SetPoint", N_("Set Point"), N_("Extends the selection to the current cursor position"), set_point);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleBeginSlur"), "ToggleBeginSlur", N_("Begin Slur"), N_("Insert/delete begin slur on this note"), toggle_begin_slur);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleEndSlur"), "ToggleEndSlur", N_("End Slur"), N_("Insert/delete end slur on this note"), toggle_end_slur);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleStartCrescendo"), "ToggleStartCrescendo", N_("Start Crescendo"), N_("No Tooltip yet"), toggle_start_crescendo);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleEndCrescendo"), "ToggleEndCrescendo", N_("End Crescendo"), N_("No Tooltip yet"), toggle_end_crescendo);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleStartDiminuendo"), "ToggleStartDiminuendo", N_("Start Diminuendo"), N_("No Tooltip yet"), toggle_start_diminuendo);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleEndDiminuendo"), "ToggleEndDiminuendo", N_("End Diminuendo"), N_("No Tooltip yet"), toggle_end_diminuendo);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleGrace"), "ToggleGrace", N_("Grace Note Off/On"), N_("Makes the note at the cursor a grace note, if it is one, makes it normal"), toggle_grace);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ForceCaution"), "ForceCaution", N_("Force Cautionary Accidental"), N_("No Tooltip yet"), force_cautionary);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangePitch"), "ChangePitch", N_("Change Pitch"), N_("No Tooltip yet"), change_pitch);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRhythm"), "InsertRhythm", N_("Insert Snippet"), N_("No Tooltip yet"), insert_rhythm_pattern);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "NextRhythm"), "NextRhythm", N_("Next Snippet"), N_("Make next snippet\nthe current snippet.\nNotes entered will follow the rhythmic pattern of this snippet"), nextrhythm);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AppendMeasureAllStaffs"), "AppendMeasureAllStaffs", N_("Append Measure All Staffs"), N_("Appends a blank measure to every staff in this movement"), append_measure_score);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ExecuteScheme"), "ExecuteScheme", N_("Execute Scheme"), N_("Execute the scheme code from the scripting window"), execute_scheme);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SharpenEnharmonicSet"), "SharpenEnharmonicSet", N_("Shift Accidentals Sharpwise"), N_("Shifts the set of accidentals one step sharper"), set_sharper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "FlattenEnharmonicSet"), "FlattenEnharmonicSet", N_("Shift Accidentals Flatwise"), N_("Shifts the set of accidentals one step flatter"), set_flatter);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "New"), "New", N_("Empty Score"), N_("Start a new musical score"), file_newwrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "NewScore"), "NewScore", N_("New"), N_("Start a new musical score for a named instrument/voice."), new_score_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Open"), "Open", N_("Open"), N_("Open a file containing a music score for editing"), file_open_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ImportLilypond"), "ImportLilypond", N_("Import Lilypond"), N_("Import a Lilypond file"), file_import_lilypond_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ImportMidi"), "ImportMidi", N_("Import Midi"), N_("Import a Midi file"), file_import_midi_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ImportMusicXml"), "ImportMusicXml", N_("Import MusicXml"), N_("Import a MusicXml file"), file_import_musicxml_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddStaffs"), "AddStaffs", N_("Add Staffs"), N_("Add staffs from a Denemo file"), file_add_staffs);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddMovements"), "AddMovements", N_("Add Movement"), N_("Add movements from a Denemo file"), file_add_movements);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MovementProps"), "MovementProps", N_("Change Properties"), N_("Change properties of this movement"), movement_props_dialog);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OpenNewWindow"), "OpenNewWindow", N_("Open In New"), N_("Open a file containing a music score for editing in a separate working area (tab"), openinnew);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Save"), "Save", N_("Save"), N_("Save the score. The score is saved to disk in XML format."), file_savewrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveAs"), "SaveAs", N_("Save As"), N_("Save the score under a new name"), file_saveaswrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveCopy"), "SaveCopy", N_("Create Copy"), N_("Save a copy of the score"), file_copy_save);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OpenTemplate"), "OpenTemplate", N_("Open Template"), N_("Start a new score from a built-in template file"), system_template_open_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OpenExample"), "OpenExample", N_("Open Example"), N_("Start a new score from a built-in example"), system_example_open_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OpenMyTemplate"), "OpenMyTemplate", N_("Open Custom Template"), N_("Start a new score from one of your own template files"), local_template_open_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveTemplate"), "SaveTemplate", N_("Save Template"), N_("Save the score as a template for re-use as a starting point for new scores"), template_save);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "NewWindow"), "NewWindow", N_("New Tab"), N_("Create working area (tab with an empty score in it)"), newview);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertMovementBefore"), "InsertMovementBefore", N_("Insert Movement Before"), N_("Insert a new movement before the current one"), insert_movement_before);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertMovementAfter"), "InsertMovementAfter", N_("Insert Movement After"), N_("Insert a new movement after the current one"), insert_movement_after);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "NewMovement"), "NewMovement", N_("New Movement"), N_("Create a new movement, usign any default template"), append_new_movement);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveParts"), "SaveParts", N_("Save Parts"), N_("Save Parts: each staff becomes a file in lilypond format"), file_savepartswrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ExportMUDELA"), "ExportMUDELA", N_("Export Lilypond"), N_("Export the score as a lilypond file"), export_mudela_action);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ExportPDF"), "ExportPDF", N_("Export PDF"), N_("Export the score as a PDF document file"), export_pdf_action);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ExportPNG"), "ExportPNG", N_("Export PNG"), N_("Export the score as a PNG image file"), export_png_action);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ExportMIDI"), "ExportMIDI", N_("Export MIDI"), N_("Export the score as a MIDI file"), export_midi_action);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PrintPreview"), "PrintPreview", N_("Print Preview"), N_("Displays the final finished score in your pd viewer"), printpreview_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PrintView"), "PrintView", N_("Print Preview"), N_("Typesets the score\nIf you have a score layout selected it will use that\notherwise all movements staffs and lyrics are typeset by default.\nBe patient! It takes time to create a beautifully laid out score.\nOnce complete you can view and then send to your printer or to a file as a .pdf document."), show_print_view);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PrintSelection"), "PrintSelection", N_("Print Selection"), N_("Displays selected music from score in your pdf viewer"), printselection_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PrintExcerptPreview"), "PrintExcerptPreview", N_("Print Excerpt"), N_("Displays a musical excerpt in your image viewer"), printexcerptpreview_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PrintMovement"), "PrintMovement", N_("Print Movement"), N_("Typesets the current movement and opens a print dialog"), printmovement_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Print"), "Print", N_("Print"), N_("Typesets the score using LilyPond and opens a print dialog"), printall_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PrintPart"), "PrintPart", N_("Print Part"), N_("Typesets the current part (the one containing the cursor)."), printpart_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Close"), "Close", N_("Close Score"), N_("Close the current score. Other scores (tabs) will stay open"), close_gui_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Quit"), "Quit", N_("Quit"), N_("Quit the Denemo program - closes tabs one at a time."), closewrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Undo"), "Undo", N_("Undo"), N_("Undoes one (more) step of your edits to the current score."), undowrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Redo"), "Redo", N_("Redo"), N_("Redoes the next of the steps you have Undone"), redowrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Copy"), "Copy", N_("Copy"), N_("Copy the music selected to the Denemo clipboard"), copywrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Cut"), "Cut", N_("Cut"), N_("Cut the music selected to the Denemo clipboard"), cutwrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Paste"), "Paste", N_("Paste"), N_("Paste the Denemo clipboard into the score where the cursor is positioned"), pastewrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PasteClipboard"), "PasteClipboard", N_("Paste LilyPond notes"), N_("Paste LilyPond notes from the text clipboard\nThis will import music written as LilyPond syntax\nYou open the LilyPond file in a texteditor, copy the stretch of notes (control-c command in your texteditor usually) and then use this command."), paste_clipboard);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ScoreProperties"), "ScoreProperties", N_("Score Properties"), N_("Change some of the properties of the current score. This will start up a dialog window"), score_properties_dialog);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveSelection"), "SaveSelection", N_("Save Selection"), N_("Save the selected music. Not sure if this is working"), saveselwrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Preferences"), "Preferences", N_("Change Preferences"), N_("Set and save your preferences for how Denemo operates on startup.\nAdvanced users can edit .denemo-XXXX/denemorc for missing ones"), preferences_change);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveAccels"), "SaveAccels", N_("Save Command Set"), N_("Save the current commands and keyboard shortcuts as the default"), save_default_keymap_file_wrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "CommandManagement"), "CommandManagement", N_("Manage Command Set"), N_("View help, change and save keyboard shortcuts"), configure_keyboard_dialog);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SwapStaffs"), "SwapStaffs", N_("Swap Staffs"), N_("Swap this staff with the one higher up.\nBe aware that if you have inserted directives to move a voice to another staff\nthese may need re-making."), swapstaffs);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SplitVoices"), "SplitVoices", N_("Split Voices"), N_("Split off the next voice as a separate staff"), splitstaffs);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "JoinVoices"), "JoinVoices", N_("Join Voices"), N_("Merge this staff as a voice on the previous staff"), joinstaffs);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SwapMovements"), "SwapMovements", N_("Swap Movements"), N_("Swap this movement with the one before"), swapmovements);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "VoiceUp"), "VoiceUp", N_("Voice Up"), N_("Go to the higher numbered voice on staff, extending selection if any"), voiceup);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "VoiceDown"), "VoiceDown", N_("Voice Down"), N_("Go to the lower numbered voice on this staff, extending selection if any"), voicedown);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToVoiceUp"), "MoveToVoiceUp", N_("Move to Voice Up"), N_("Go to the higher numbered voice on staff without altering selection"), movetovoiceup);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToVoiceDown"), "MoveToVoiceDown", N_("Move to Voice Down"), N_("Go to the lower numbered voice on this staff without altering selection"), movetovoicedown);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddBefore"), "AddBefore", N_("Add Staff Before"), N_("Inserts a new staff before the current staff"), newstaffbefore);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddAfter"), "AddAfter", N_("Add Staff After"), N_("Inserts/Adds a new staff after the current staff"), dnm_newstaffafter);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddInitial"), "AddInitial", N_("Add Initial Staff"), N_("Inserts a new staff at the top of the score"), newstaffinitial);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddLast"), "AddLast", N_("Add Last Staff"), N_("Inserts a new staff at the end of the score"), newstafflast);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteBefore"), "DeleteBefore", N_("Delete Staff Before"), N_("Deletes the staff before the current staff"), delete_staff_before);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteStaff"), "DeleteStaff", N_("Delete Current Staff"), N_("Deletes the current staff"), delete_staff_current);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteAfter"), "DeleteAfter", N_("Delete Staff After"), N_("Deletes the staff after the current staff"), delete_staff_after);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddVoice"), "AddVoice", N_("Add Voice"), N_("Adds a new voice (part), to the current staff. It is tricky to switch between the voices. Suggest to use merge staffs"), dnm_newstaffvoice);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StaffProperties"), "StaffProperties", N_("Staff Properties"), N_("Change the properties of the current staff"), staff_properties_change_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InitialClef"), "InitialClef", N_("Initial Clef"), N_("Change the initial clef of the current staff"), clef_change_initial);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertClef"), "InsertClef", N_("Clef Change"), N_("Insert/Edit a change of clef at the cursor"), clef_change_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InitialKey"), "InitialKey", N_("Initial Key"), N_("Set the initial key signature of the current staff"), key_change_initial);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertKey"), "InsertKey", N_("Key Signature Change"), N_("Insert/Edit a key change at the cursor position"), key_change_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InitialTimeSig"), "InitialTimeSig", N_("Inital Time Signature"), N_("Set the initial time signature of the current staff"), timesig_change_initial);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertTimeSig"), "InsertTimeSig", N_("Time Signature Change"), N_("Edit/Insert a time signature change for the current measure"), timesig_change_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeNotehead"), "ChangeNotehead", N_("Set Notehead"), N_("Change the type of notehead for the current note"), set_notehead);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertStem"), "InsertStem", N_("Auto Stemming"), N_("Inserts a stem neutral object. After this automatic stem directions are active. You can click on this tag and use Sharpen/StemUp etc commands to change stem direction"), stem_directive_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddVerse"), "AddVerse", N_("Add Lyric Verse"), N_("Add a verse of lyrics"), add_verse);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteVerse"), "DeleteVerse", N_("Delete Verse"), N_("Deletes current verse of lyrics from current voice"), delete_verse);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditFiguredBass"), "EditFiguredBass", N_("Insert/Edit Figured Bass"), N_("Add a bass figure to the current note. Use | sign to split the duration of a note so as to have multiple figures on one note. See Lilypond docs for other notation"), figure_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteFiguredBass"), "DeleteFiguredBass", N_("Delete Figures"), N_("Delete the figured bass on the current staff"), delete_figured_bass);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "HideFiguredBass"), "HideFiguredBass", N_("Hide Figures (Print)"), N_("Hide the figured bass on the current staff on printing"), hide_figured_bass);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ShowFiguredBass"), "ShowFiguredBass", N_("Show Figures (Print)"), N_("Show the figured bass on the current staff on printing"), show_figured_bass);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditChords"), "EditChords", N_("Edit Chord Symbols"), N_("Allows chord symbols to be added to the current note. E.G.cis:dim7 for c-sharp diminished 7th. See Lilypond docs for notation"), fakechord_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDynamic"), "InsertDynamic", N_("Insert Dynamics"), N_("Inserts a dynamic marking at the cursor position"), insert_dynamic);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditObject"), "EditObject", N_("Edit Object"), N_("Edit the object at the cursor."), edit_object);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditDirective"), "EditDirective", N_("Edit Directives"), N_("Edit any directives attached to chord/note at cursor."), edit_object_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditStaffDirective"), "EditStaffDirective", N_("Edit Staff Directives"), N_("Edit any directives attached to staff."), edit_staff_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditVoiceDirective"), "EditVoiceDirective", N_("Edit Voice Directives"), N_("Edit any directives attached to voice."), edit_voice_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditScoreDirective"), "EditScoreDirective", N_("Edit Score Directives"), N_("Edit any directives attached to score."), edit_score_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditMovementDirective"), "EditMovementDirective", N_("Edit Movement Directives"), N_("Edit any directives attached to movement."), edit_movement_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditClefDirective"), "EditClefDirective", N_("Edit Clef Directives"), N_("Edit any directives attached to clef."), edit_clef_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditTimesigDirective"), "EditTimesigDirective", N_("Edit Time Signature Directives"), N_("Edit any directives attached to time signature."), edit_timesig_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditKeysigDirective"), "EditKeysigDirective", N_("Edit Key Signature Directives"), N_("Edit any directives attached to key signature."), edit_keysig_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteDirective"), "DeleteDirective", N_("Delete a Directive"), N_("Delete a directive attached to chord/note at cursor."), delete_chord_or_note_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AttachLilyToNote"), "AttachLilyToNote", N_("Attach Lilypond to Note"), N_("Attach or edit attached LilyPond text to the note at the cursor. This can be used for guitar fingerings, cautionary accidentals and much more. See LilyPond documentation."), note_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AttachLilyToChord"), "AttachLilyToChord", N_("Attach Lilypond to Chord"), N_("Attach or edit attached LilyPond text to the chord at the cursor. This can be used for attaching and placing text and much more. See LilyPond documentation."), chord_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBarline"), "InsertBarline", N_("Insert Barline"), N_("Inserts specialized barline at the cursor position. Mostly not working"), insert_barline);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "GoToMeasure"), "GoToMeasure", N_("Go to Measure"), N_("Opens a dialog for going to a numbered measure"), tomeasurenum);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "GoToBeginning"), "GoToBeginning", N_("Go to Beginning"), N_("Cursor to start of staff/voice, extending selection if any"), tohome);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "GoToEnd"), "GoToEnd", N_("Go to End"), N_("Cursor to end of staff/voice, extending selection if any"), toend);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToBeginning"), "MoveToBeginning", N_("Move to Staff/Voice Beginning"), N_("Cursor to start of staff/voice, without extending selection if any"), movetostart);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToEnd"), "MoveToEnd", N_("Move to Staff/Voice End"), N_("Cursor to end of staff/voice, without extending selection if any"), movetoend);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "NextMovement"), "NextMovement", N_("Next Movement"), N_("Go to the next movement"), next_movement);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PreviousMovement"), "PreviousMovement", N_("Previous Movement"), N_("Go to the previous movement"), prev_movement);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteMovement"), "DeleteMovement", N_("Delete Movement"), N_("Delete the current movement"), delete_movement);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Play"), "Play", N_("Play"), N_("Play"), ext_midi_playback);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Stop"), "Stop", N_("Stop"), N_("Stop"), stop_midi_playback);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PlaybackProperties"), "PlaybackProperties", N_("Playback Properties"), N_("Allows you to specify properties used in playing back (midi)"), playback_properties_change);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Help"), "Help", N_("Browse Manual"), N_("Opens a browser on the user manual"), browse_manual);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "About"), "About", N_("About"), N_("Gives the version number etc of this program"), about);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoreCommands"), "MoreCommands", N_("More Commands"), N_("Allows choosing standard extra commands/menu items"), morecommands);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MyCommands"), "MyCommands", N_("My Commands"), N_("Allows choosing extra commands/menu items from your own collection of extras"), mycommands);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "FetchCommands"), "FetchCommands", N_("Update Commands from Internet"), N_("Refreshes the set of commands available from Denemo.org.\nUse More Commands after this has finished"), fetchcommands);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleEdit"), "ToggleEdit", N_("Toggle Edit Mode"), N_("Toggle between current mode and edit mode"), toggle_edit_mode);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleRest"), "ToggleRest", N_("Toggle Rest Mode"), N_("Toggle between note entry and rest entry"), toggle_rest_mode);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleRhythm"), "ToggleRhythm", N_("Toggle Audible Feedback"), N_("Toggle audible feedback on/off"), toggle_rhythm_mode);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ClearOverlay"), "ClearOverlay", N_("Clear Overlay"), N_("Clear the list of pitches that overlay the notes"), clear_overlay);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "CreateRhythm"), "CreateRhythm", N_("Create Snippet"), N_("Copy selection as music snippet or rhythm pattern for notes to follow as they are entered"), create_rhythm_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteRhythm"), "DeleteRhythm", N_("Delete Snippet"), N_("Delete the selected music snippet/rhythm pattern"), delete_rhythm_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertA"), "InsertA", N_("Insert A"),N_("Inserts note A before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  InsertA);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteA"), "AddNoteA", N_("Insert A After"),N_("Inserts note A after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  AddNoteA);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddA"), "AddA", N_("Add A"),N_("Adds note A to the chord at cursor\nCursor height determines which octave"),  AddA);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToA"), "ChangeToA", N_("Change to A"),N_("Changes note at cursor to nearest note A\nRhythm is unchanged"),  ChangeToA);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToA"), "MoveToA", N_("Move to A"),N_("Moves cursor to nearest note A"),  MoveToA);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertB"), "InsertB", N_("Insert B"),N_("Inserts note B before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  InsertB);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteB"), "AddNoteB", N_("Insert B After"),N_("Inserts note B after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  AddNoteB);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddB"), "AddB", N_("Add B"),N_("Adds note B to the chord at cursor\nCursor height determines which octave"),  AddB);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToB"), "ChangeToB", N_("Change to B"),N_("Changes note at cursor to nearest note B\nRhythm is unchanged"),  ChangeToB);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToB"), "MoveToB", N_("Move to B"),N_("Moves cursor to nearest note B"),  MoveToB);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertC"), "InsertC", N_("Insert C"),N_("Inserts note C before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  InsertC);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteC"), "AddNoteC", N_("Insert C After"),N_("Inserts note C after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  AddNoteC);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddC"), "AddC", N_("Add C"),N_("Adds note C to the chord at cursor\nCursor height determines which octave"),  AddC);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToC"), "ChangeToC", N_("Change to C"),N_("Changes note at cursor to nearest note C\nRhythm is unchanged"),  ChangeToC);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToC"), "MoveToC", N_("Move to C"),N_("Moves cursor to nearest note C"),  MoveToC);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertD"), "InsertD", N_("Insert D"),N_("Inserts note D before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  InsertD);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteD"), "AddNoteD", N_("Insert D After"),N_("Inserts note D after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  AddNoteD);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddD"), "AddD", N_("Add D"),N_("Adds note D to the chord at cursor\nCursor height determines which octave"),  AddD);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToD"), "ChangeToD", N_("Change to D"),N_("Changes note at cursor to nearest note D\nRhythm is unchanged"),  ChangeToD);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToD"), "MoveToD", N_("Move to D"),N_("Moves cursor to nearest note D"),  MoveToD);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertE"), "InsertE", N_("Insert E"),N_("Inserts note E before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  InsertE);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteE"), "AddNoteE", N_("Insert E After"),N_("Inserts note E after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  AddNoteE);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddE"), "AddE", N_("Add E"),N_("Adds note E to the chord at cursor\nCursor height determines which octave"),  AddE);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToE"), "ChangeToE", N_("Change to E"),N_("Changes note at cursor to nearest note E\nRhythm is unchanged"),  ChangeToE);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToE"), "MoveToE", N_("Move to E"),N_("Moves cursor to nearest note E"),  MoveToE);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertF"), "InsertF", N_("Insert F"),N_("Inserts note F before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  InsertF);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteF"), "AddNoteF", N_("Insert F After"),N_("Inserts note F after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  AddNoteF);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddF"), "AddF", N_("Add F"),N_("Adds note F to the chord at cursor\nCursor height determines which octave"),  AddF);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToF"), "ChangeToF", N_("Change to F"),N_("Changes note at cursor to nearest note F\nRhythm is unchanged"),  ChangeToF);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToF"), "MoveToF", N_("Move to F"),N_("Moves cursor to nearest note F"),  MoveToF);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertG"), "InsertG", N_("Insert G"),N_("Inserts note G before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  InsertG);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteG"), "AddNoteG", N_("Insert G After"),N_("Inserts note G after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  AddNoteG);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddG"), "AddG", N_("Add G"),N_("Adds note G to the chord at cursor\nCursor height determines which octave"),  AddG);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToG"), "ChangeToG", N_("Change to G"),N_("Changes note at cursor to nearest note G\nRhythm is unchanged"),  ChangeToG);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToG"), "MoveToG", N_("Move to G"),N_("Moves cursor to nearest note G"),  MoveToG);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "0"), "0", _("\xF0\x9D\x85\x9D"), _("When appending, appends a \xF0\x9D\x85\x9D \nWith the cursor on a note inserts a \xF0\x9D\x85\x9D  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch"), Dur0);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change0"), "Change0", _("\xF0\x9D\x85\x9D"), _("Change the current note to a \xF0\x9D\x85\x9D"), ChangeDur0);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert0"), "Insert0", _("\xF0\x9D\x85\x9D"), _("Insert a \xF0\x9D\x85\x9D"), InsertDur0);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest0"), "InsertRest0",  _("Insert a \xF0\x9D\x84\xBB") ,  _("Inserts a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\x9D"), InsertRest0);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest0"), "ChangeRest0",  _("Change a \xF0\x9D\x84\xBB") ,  _("Changes a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\x9D"), ChangeRest0);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set0"), "Set0", _("\xF0\x9D\x85\x9D"), _("Set the prevailing duration to \xF0\x9D\x85\x9D"), SetDur0);

#undef NOTE0

#undef REST0

#undef NOTECHAR0

#undef RESTCHAR0
register_command(Denemo.map, gtk_action_group_get_action(action_group, "1"), "1", _("\xF0\x9D\x85\x9E"), _("When appending, appends a \xF0\x9D\x85\x9E \nWith the cursor on a note inserts a \xF0\x9D\x85\x9E  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch"), Dur1);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change1"), "Change1", _("\xF0\x9D\x85\x9E"), _("Change the current note to a \xF0\x9D\x85\x9E"), ChangeDur1);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert1"), "Insert1", _("\xF0\x9D\x85\x9E"), _("Insert a \xF0\x9D\x85\x9E"), InsertDur1);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest1"), "InsertRest1",  _("Insert a \xF0\x9D\x84\xBC") ,  _("Inserts a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\x9E"), InsertRest1);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest1"), "ChangeRest1",  _("Change a \xF0\x9D\x84\xBC") ,  _("Changes a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\x9E"), ChangeRest1);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set1"), "Set1", _("\xF0\x9D\x85\x9E"), _("Set the prevailing duration to \xF0\x9D\x85\x9E"), SetDur1);

#undef NOTE1

#undef REST1

#undef NOTECHAR1

#undef RESTCHAR1
register_command(Denemo.map, gtk_action_group_get_action(action_group, "2"), "2", _("\xF0\x9D\x85\x9F"), _("When appending, appends a \xF0\x9D\x85\x9F \nWith the cursor on a note inserts a \xF0\x9D\x85\x9F  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch"), Dur2);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change2"), "Change2", _("\xF0\x9D\x85\x9F"), _("Change the current note to a \xF0\x9D\x85\x9F"), ChangeDur2);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert2"), "Insert2", _("\xF0\x9D\x85\x9F"), _("Insert a \xF0\x9D\x85\x9F"), InsertDur2);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest2"), "InsertRest2",  _("Insert a \xF0\x9D\x84\xBD") ,  _("Inserts a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\x9F"), InsertRest2);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest2"), "ChangeRest2",  _("Change a \xF0\x9D\x84\xBD") ,  _("Changes a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\x9F"), ChangeRest2);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set2"), "Set2", _("\xF0\x9D\x85\x9F"), _("Set the prevailing duration to \xF0\x9D\x85\x9F"), SetDur2);

#undef NOTE2

#undef REST2

#undef NOTECHAR2

#undef RESTCHAR2
register_command(Denemo.map, gtk_action_group_get_action(action_group, "3"), "3", _("\xF0\x9D\x85\xA0"), _("When appending, appends a \xF0\x9D\x85\xA0 \nWith the cursor on a note inserts a \xF0\x9D\x85\xA0  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch"), Dur3);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change3"), "Change3", _("\xF0\x9D\x85\xA0"), _("Change the current note to a \xF0\x9D\x85\xA0"), ChangeDur3);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert3"), "Insert3", _("\xF0\x9D\x85\xA0"), _("Insert a \xF0\x9D\x85\xA0"), InsertDur3);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest3"), "InsertRest3",  _("Insert a \xF0\x9D\x84\xBE") ,  _("Inserts a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\xA0"), InsertRest3);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest3"), "ChangeRest3",  _("Change a \xF0\x9D\x84\xBE") ,  _("Changes a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\xA0"), ChangeRest3);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set3"), "Set3", _("\xF0\x9D\x85\xA0"), _("Set the prevailing duration to \xF0\x9D\x85\xA0"), SetDur3);

#undef NOTE3

#undef REST3

#undef NOTECHAR3

#undef RESTCHAR3
register_command(Denemo.map, gtk_action_group_get_action(action_group, "4"), "4", _("\xF0\x9D\x85\xA1"), _("When appending, appends a \xF0\x9D\x85\xA1 \nWith the cursor on a note inserts a \xF0\x9D\x85\xA1  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch"), Dur4);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change4"), "Change4", _("\xF0\x9D\x85\xA1"), _("Change the current note to a \xF0\x9D\x85\xA1"), ChangeDur4);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert4"), "Insert4", _("\xF0\x9D\x85\xA1"), _("Insert a \xF0\x9D\x85\xA1"), InsertDur4);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest4"), "InsertRest4",  _("Insert a \xF0\x9D\x84\xBF") ,  _("Inserts a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\xA1"), InsertRest4);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest4"), "ChangeRest4",  _("Change a \xF0\x9D\x84\xBF") ,  _("Changes a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\xA1"), ChangeRest4);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set4"), "Set4", _("\xF0\x9D\x85\xA1"), _("Set the prevailing duration to \xF0\x9D\x85\xA1"), SetDur4);

#undef NOTE4

#undef REST4

#undef NOTECHAR4

#undef RESTCHAR4
register_command(Denemo.map, gtk_action_group_get_action(action_group, "5"), "5", _("\xF0\x9D\x85\xA2"), _("When appending, appends a \xF0\x9D\x85\xA2 \nWith the cursor on a note inserts a \xF0\x9D\x85\xA2  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch"), Dur5);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change5"), "Change5", _("\xF0\x9D\x85\xA2"), _("Change the current note to a \xF0\x9D\x85\xA2"), ChangeDur5);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert5"), "Insert5", _("\xF0\x9D\x85\xA2"), _("Insert a \xF0\x9D\x85\xA2"), InsertDur5);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest5"), "InsertRest5",  _("Insert a \xF0\x9D\x85\x80") ,  _("Inserts a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\xA2"), InsertRest5);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest5"), "ChangeRest5",  _("Change a \xF0\x9D\x85\x80") ,  _("Changes a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\xA2"), ChangeRest5);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set5"), "Set5", _("\xF0\x9D\x85\xA2"), _("Set the prevailing duration to \xF0\x9D\x85\xA2"), SetDur5);

#undef NOTE5

#undef REST5

#undef NOTECHAR5

#undef RESTCHAR5
register_command(Denemo.map, gtk_action_group_get_action(action_group, "6"), "6", _("\xF0\x9D\x85\xA3"), _("When appending, appends a \xF0\x9D\x85\xA3 \nWith the cursor on a note inserts a \xF0\x9D\x85\xA3  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch"), Dur6);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change6"), "Change6", _("\xF0\x9D\x85\xA3"), _("Change the current note to a \xF0\x9D\x85\xA3"), ChangeDur6);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert6"), "Insert6", _("\xF0\x9D\x85\xA3"), _("Insert a \xF0\x9D\x85\xA3"), InsertDur6);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest6"), "InsertRest6",  _("Insert a \xF0\x9D\x85\x81") ,  _("Inserts a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\xA3"), InsertRest6);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest6"), "ChangeRest6",  _("Change a \xF0\x9D\x85\x81") ,  _("Changes a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\xA3"), ChangeRest6);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set6"), "Set6", _("\xF0\x9D\x85\xA3"), _("Set the prevailing duration to \xF0\x9D\x85\xA3"), SetDur6);

#undef NOTE6

#undef REST6

#undef NOTECHAR6

#undef RESTCHAR6
register_command(Denemo.map, gtk_action_group_get_action(action_group, "7"), "7", _("\xF0\x9D\x85\xA4"), _("When appending, appends a \xF0\x9D\x85\xA4 \nWith the cursor on a note inserts a \xF0\x9D\x85\xA4  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch"), Dur7);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change7"), "Change7", _("\xF0\x9D\x85\xA4"), _("Change the current note to a \xF0\x9D\x85\xA4"), ChangeDur7);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert7"), "Insert7", _("\xF0\x9D\x85\xA4"), _("Insert a \xF0\x9D\x85\xA4"), InsertDur7);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest7"), "InsertRest7",  _("Insert a \xF0\x9D\x85\x82") ,  _("Inserts a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\xA4"), InsertRest7);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest7"), "ChangeRest7",  _("Change a \xF0\x9D\x85\x82") ,  _("Changes a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\xA4"), ChangeRest7);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set7"), "Set7", _("\xF0\x9D\x85\xA4"), _("Set the prevailing duration to \xF0\x9D\x85\xA4"), SetDur7);

#undef NOTE7

#undef REST7

#undef NOTECHAR7

#undef RESTCHAR7
register_command(Denemo.map, gtk_action_group_get_action(action_group, "8"), "8", _("\xF0\x9D\x85\xA5"), _("When appending, appends a \xF0\x9D\x85\xA5 \nWith the cursor on a note inserts a \xF0\x9D\x85\xA5  before the current note\nThe note will be pitchless (displays yellow, non-printing, percussion-sounding) if MIDI-in is active\n - the MIDI keyboard will provide the pitch"), Dur8);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Change8"), "Change8", _("\xF0\x9D\x85\xA5"), _("Change the current note to a \xF0\x9D\x85\xA5"), ChangeDur8);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert8"), "Insert8", _("\xF0\x9D\x85\xA5"), _("Insert a \xF0\x9D\x85\xA5"), InsertDur8);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRest8"), "InsertRest8",  _("Insert a \xF0\x9D\x85\x83") ,  _("Inserts a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\xA5"), InsertRest8);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeRest8"), "ChangeRest8",  _("Change a \xF0\x9D\x85\x83") ,  _("Changes a rest at cursor position\nSets prevailing rhythm to \xF0\x9D\x85\xA5"), ChangeRest8);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set8"), "Set8", _("\xF0\x9D\x85\xA5"), _("Set the prevailing duration to \xF0\x9D\x85\xA5"), SetDur8);

#undef NOTE8

#undef REST8

#undef NOTECHAR8

#undef RESTCHAR8
