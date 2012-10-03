register_command(Denemo.map, gtk_action_group_get_action(action_group, "CursorLeft"), "CursorLeft", _("Cursor Left"), _("Moves the cursor one object left, altering the selection if any"), cursorleft);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveCursorLeft"), "MoveCursorLeft", _("Move Cursor Left"), _("Moves the cursor one object left, without altering the selection"), movecursorleft);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "CursorDown"), "CursorDown", _("Cursor Down"), _("Moves the cursor one scale step down"), cursordown);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "CursorUp"), "CursorUp", _("Cursor Up"), _("Moves the cursor one scale step up"), cursorup);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "CursorRight"), "CursorRight", _("Cursor Right"), _("Moves the cursor one object right, altering the selection if any"), cursorright);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveCursorRight"), "MoveCursorRight", _("Move Cursor Right"), _("Moves the cursor one object right, without altering the selection"), movecursorright);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "GoToMark"), "GoToMark", _("To Mark"), _("Moves the cursor to the Mark without altering the selection"), goto_mark);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SwapPointAndMark"), "SwapPointAndMark", _("Swap Ends of Selection"), _("Swaps the active end of the selection"), swap_point_and_mark);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "GoToSelectionStart"), "GoToSelectionStart", _("To Selection Start"), _("Moves the cursor to the first object in the selection without altering the selection. returns #f if no selection"), goto_selection_start);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PushPosition"), "PushPosition", _("Push Position"), _("Pushes the current cursor position onto a stack"), PushPosition);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PopPosition"), "PopPosition", _("Pop Position"), _("Pops a position from the stack of cursor positions, moving the cursor there"), PopPosition);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PopPushPosition"), "PopPushPosition", _("Pop and Push Position"), _("Pops a position from the stack of cursor positions, pushes the current position, then moves the cursor to the popped position"), PopPushPosition);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleReduceToDrawingArea"), "ToggleReduceToDrawingArea", _("Hide/Show Menus"), _("Hides/Shows menus, panes etc. The ones shown are those checked in the view menu."), ToggleReduceToDrawingArea);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StaffUp"), "StaffUp", _("Staff Up"), _("Moves the cursor to the staff above, extending selection if any"), staffup);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StaffDown"), "StaffDown", _("Staff Down"), _("Moves the cursor to the staff below, extending selection if any"), staffdown);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToStaffUp"), "MoveToStaffUp", _("Move to Staff Up"), _("Moves the cursor to the staff above without altering selection"), movetostaffup);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToStaffDown"), "MoveToStaffDown", _("Move to Staff Down"), _("Moves the cursor to the staff below  without altering selection"), movetostaffdown);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MeasureLeft"), "MeasureLeft", _("Measure Left"), _("Moves the cursor to the first object in the next measure, extending selection if any"), measureleft);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MeasureRight"), "MeasureRight", _("Measure Right"), _("Moves the cursor to the first object in the previous measure, extending selection if any"), measureright);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToMeasureLeft"), "MoveToMeasureLeft", _("Move to Measure Left"), _("Moves the cursor to the first object in the next measure leaving selection, if any, unchanged"), movetomeasureleft);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToMeasureRight"), "MoveToMeasureRight", _("Move to Measure Right"), _("Moves the cursor to the first object in the previous measureleaving selection, if any, unchanged"), movetomeasureright);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "A"), "A", _("A"), _("Append/Edit  A"), go_to_A_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "B"), "B", _("B"), _("Append/Edit  B"), go_to_B_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "C"), "C", _("C"), _("Append/Edit  C"), go_to_C_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "D"), "D", _("D"), _("Append/Edit  D"), go_to_D_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "E"), "E", _("E"), _("Append/Edit  E"), go_to_E_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "F"), "F", _("F"), _("Append/Edit  F"), go_to_F_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "G"), "G", _("G"), _("Append/Edit  G"), go_to_G_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OctaveUp"), "OctaveUp", _("Octave Up"), _("Octave Up"), octave_up_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OctaveDown"), "OctaveDown", _("Octave Down"), _("Octave Down"), octave_down_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "WholeNote"), "WholeNote", _("WholeNote"), _("Insert \xF0\x9D\x85\x9D"), insert_chord_0key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "HalfNote"), "HalfNote", _("HalfNote"), _("Insert \xF0\x9D\x85\x9E"), insert_chord_1key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "QuarterNote"), "QuarterNote", _("QuarterNote"), _("Insert \xF0\x9D\x85\x9F"), insert_chord_2key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EighthNote"), "EighthNote", _("EighthNote"), _("Insert \xF0\x9D\x85\xA0"), insert_chord_3key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SixteenthNote"), "SixteenthNote", _("SixteenthNote"), _("Insert \xF0\x9D\x85\xA1"), insert_chord_4key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ThirtysecondNote"), "ThirtysecondNote", _("ThirtysecondNote"), _("Insert \xF0\x9D\x85\xA2"), insert_chord_5key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SixtyfourthNote"), "SixtyfourthNote", _("SixtyfourthNote"), _("Insert \xF0\x9D\x85\xA3"), insert_chord_6key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OneHundredTwentyEighthNote"), "OneHundredTwentyEighthNote", _("OneHundredTwentyEighthNote"), _("Insert \xF0\x9D\x85\xA4"), insert_chord_7key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "TwoHundredFiftySixthNote"), "TwoHundredFiftySixthNote", _("TwoHundredFiftySixthNote"), _("Insert \xF0\x9D\x85\xA5"), insert_chord_8key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertWholeRest"), "InsertWholeRest", _("<span font_desc=\"Denemo\">\xF0\x9D\x84\xBB</span>"), _("Insert \xF0\x9D\x84\xBB rest"), insert_rest_0key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertHalfRest"), "InsertHalfRest", _("<span font_desc=\"Denemo\">\xF0\x9D\x84\xBC</span>"), _("Insert \xF0\x9D\x84\xBC rest"), insert_rest_1key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertQuarterRest"), "InsertQuarterRest", _("<span font_desc=\"Denemo\">\xF0\x9D\x84\xBD</span>"), _("Insert \xF0\x9D\x84\xBD rest"), insert_rest_2key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertEighthRest"), "InsertEighthRest", _("<span font_desc=\"Denemo\">\xF0\x9D\x84\xBE</span>"), _("Insert \xF0\x9D\x84\xBE rest"), insert_rest_3key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertSixteenthRest"), "InsertSixteenthRest", _("<span font_desc=\"Denemo\">\xF0\x9D\x84\xBF</span>"), _("Insert \xF0\x9D\x84\xBF rest"), insert_rest_4key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertThirtysecondRest"), "InsertThirtysecondRest", _("<span font_desc=\"Denemo\">\xF0\x9D\x85\x80</span>"), _("Insert \xF0\x9D\x85\x80 rest"), insert_rest_5key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertSixtyfourthRest"), "InsertSixtyfourthRest", _("<span font_desc=\"Denemo\">\xF0\x9D\x85\x81</span>"), _("Insert \xF0\x9D\x85\x81 rest"), insert_rest_6key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankWholeNote"), "InsertBlankWholeNote", _("InsertBlankWholeNote"), _("Insert a non-printing \xF0\x9D\x84\xBB rest"), insert_blankchord_0key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankHalfNote"), "InsertBlankHalfNote", _("InsertBlankHalfNote"), _("Insert a non-printing \xF0\x9D\x84\xBC rest"), insert_blankchord_1key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankQuarterNote"), "InsertBlankQuarterNote", _("InsertBlankQuarterNote"), _("Insert a non-printing \xF0\x9D\x84\xBD rest"), insert_blankchord_2key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankEighthNote"), "InsertBlankEighthNote", _("InsertBlankEighthNote"), _("Insert a non-printing \xF0\x9D\x84\xBE rest"), insert_blankchord_3key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankSixteenthNote"), "InsertBlankSixteenthNote", _("InsertBlankSixteenthNote"), _("Insert a non-printing \xF0\x9D\x84\xBF rest"), insert_blankchord_4key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankThirtysecondNote"), "InsertBlankThirtysecondNote", _("InsertBlankThirtysecondNote"), _("Insert a non-printing \xF0\x9D\x85\x80 rest"), insert_blankchord_5key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankSixtyfourthNote"), "InsertBlankSixtyfourthNote", _("InsertBlankSixtyfourthNote"), _("Insert a non-printing \xF0\x9D\x85\x81 rest"), insert_blankchord_6key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankOneHundredTwentyEighthNote"), "InsertBlankOneHundredTwentyEighthNote", _("InsertBlankOneHundredTwentyEighthNote"), _("Insert a non-printing \xF0\x9D\x85\x82 rest"), insert_blankchord_7key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBlankTwoHundredFiftySixthNote"), "InsertBlankTwoHundredFiftySixthNote", _("InsertBlankTwoHundredFiftySixthNote"), _("Insert a non-printing \xF0\x9D\x85\x83 rest"), insert_blankchord_8key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleRestMode"), "ToggleRestMode", _("Toggle Rest Mode"), _("No Tooltip yet"), rest_toggle_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleBlankMode"), "ToggleBlankMode", _("Toggle Blank Mode"), _("No Tooltip yet"), toggle_blank);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDuplet"), "InsertDuplet", _("Insert Duplet"), _("No Tooltip yet"), insert_duplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertTriplet"), "InsertTriplet", _("Insert Triplet"), _("No Tooltip yet"), insert_triplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StartTriplet"), "StartTriplet", _("Start Triplet"), _("No Tooltip yet"), start_triplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EndTuplet"), "EndTuplet", _("End Tuplet"), _("No Tooltip yet"), end_tuplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertQuadtuplet"), "InsertQuadtuplet", _("Insert Quadtuplet"), _("No Tooltip yet"), insert_quadtuplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertQuintuplet"), "InsertQuintuplet", _("Insert Quintuplet"), _("No Tooltip yet"), insert_quintuplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertSextuplet"), "InsertSextuplet", _("Insert Sextuplet"), _("No Tooltip yet"), insert_sextuplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertSeptuplet"), "InsertSeptuplet", _("Insert Septuplet"), _("No Tooltip yet"), insert_septuplet);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteToChord"), "AddNoteToChord", _("Add note"), _("Add a note to the current chord\nThe cursor position determines which note to add"), add_tone_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "RemoveNoteFromChord"), "RemoveNoteFromChord", _("Remove note"), _("Remove a note from the current chord"), remove_tone_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Sharpen"), "Sharpen", _("Sharpen"), _("No Tooltip yet"), sharpen_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Flatten"), "Flatten", _("Flatten"), _("No Tooltip yet"), flatten_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PendingSharpen"), "PendingSharpen", _("Sharpen Next Note"), _("Increases the sharpness of the next entered note. The status bar shows the current state."), pending_sharpen);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PendingFlatten"), "PendingFlatten", _("Flatten Next Note"), _("Increases the flatness of the next entered note. The status bar shows the current state."), pending_flatten);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StemUp"), "StemUp", _("StemUp"), _("Alters a StemNeutral object to stem up."), stem_up);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StemDown"), "StemDown", _("StemDown"), _("Alters a StemNeutral object to stem down."), stem_down);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddDot"), "AddDot", _("Add Dot"), _("No Tooltip yet"), add_dot_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "RemoveDot"), "RemoveDot", _("Remove Dot"), _("No Tooltip yet"), remove_dot_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertTiedNote"), "InsertTiedNote", _("Tied note"), _("Inserts a duplicate of the current note, tied"), tie_notes_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleTie"), "ToggleTie", _("Toggle Tie"), _("Ties/unties the note at the cursor"), toggle_tie);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteObject"), "DeleteObject", _("Delete Object"), _("Delete the object at the cursor"), deleteobject);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeletePreviousObject"), "DeletePreviousObject", _("Delete Previous Object"), _("Delete to the left of the cursor."), deletepreviousobject);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertMeasure"), "InsertMeasure", _("Insert Measure Before"), _("Insert a blank measure before the current one (in all staffs)"), insert_measure_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddMeasure"), "AddMeasure", _("Insert Measure After"), _("Insert a blank measure after the current one (in all staffs)"), addmeasureafter);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertMeasureBefore"), "InsertMeasureBefore", _("Staff Insert Measure Before"), _("Insert a blank measure before the current one (in current staff)"), insertmeasurebefore);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertMeasureAfter"), "InsertMeasureAfter", _("Staff Insert Measure After"), _("Insert a blank measure in current staff after the current measure"), insertmeasureafter);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AppendMeasure"), "AppendMeasure", _("Staff Append Measure"), _("No Tooltip yet"), append_measure_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteMeasure"), "DeleteMeasure", _("Staff Delete Measure"), _("Delete the current measure in this staff, leaving the staff short"), deletemeasure);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteMeasureAllStaffs"), "DeleteMeasureAllStaffs", _("Delete Measure All Staffs"), _("Delete the current measure in all staffs"), deletemeasureallstaffs);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ShrinkMeasures"), "ShrinkMeasures", _("Shrink Measure"), _("No Tooltip yet"), adjust_measure_less_width_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "WidenMeasures"), "WidenMeasures", _("Widen Measures"), _("No Tooltip yet"), adjust_measure_more_width_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ShorterStaffs"), "ShorterStaffs", _("Shorter Staffs"), _("No Tooltip yet"), adjust_staff_less_height_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "TallerStaffs"), "TallerStaffs", _("Taller Staffs"), _("No Tooltip yet"), adjust_staff_more_height_key);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertTrebleClef"), "InsertTrebleClef", _("New Treble Clef"), _("No Tooltip yet"), newcleftreble);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBassClef"), "InsertBassClef", _("New Bass Clef"), _("No Tooltip yet"), newclefbass);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insertg8clef"), "Insertg8clef", _("New G8 Clef"), _("No Tooltip yet"), newclefg8);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertAltoClef"), "InsertAltoClef", _("New Alto Clef"), _("No Tooltip yet"), newclefalto);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertTenorClef"), "InsertTenorClef", _("New Tenor Clef"), _("No Tooltip yet"), newcleftenor);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertSopranoClef"), "InsertSopranoClef", _("New Soprano Clef"), _("No Tooltip yet"), newclefsoprano);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialTrebleClef"), "SetInitialTrebleClef", _("Set Treble Clef"), _("No Tooltip yet"), setcleftreble);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialBassClef"), "SetInitialBassClef", _("Set Bass Clef"), _("No Tooltip yet"), setclefbass);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialg8clef"), "SetInitialg8clef", _("Set G8 Clef"), _("No Tooltip yet"), setclefg8);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialAltoClef"), "SetInitialAltoClef", _("Set Alto Clef"), _("No Tooltip yet"), setclefalto);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialTenorClef"), "SetInitialTenorClef", _("Set Tenor Clef"), _("No Tooltip yet"), setcleftenor);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialSopranoClef"), "SetInitialSopranoClef", _("Set Soprano Clef"), _("No Tooltip yet"), setclefsoprano);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert22Time"), "Insert22Time", _("Insert 2/2 Time"), _("No Tooltip yet"), newtimesig22);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert32Time"), "Insert32Time", _("Insert 3/2 Time"), _("No Tooltip yet"), newtimesig32);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert42Time"), "Insert42Time", _("Insert 4/2 Time"), _("No Tooltip yet"), newtimesig42);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert44Time"), "Insert44Time", _("Insert 4/4 Time"), _("No Tooltip yet"), newtimesig44);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert34Time"), "Insert34Time", _("Insert 3/4 Time"), _("No Tooltip yet"), newtimesig34);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert24Time"), "Insert24Time", _("Insert 2/4 Time"), _("No Tooltip yet"), newtimesig24);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert64Time"), "Insert64Time", _("Insert 6/4 Time"), _("No Tooltip yet"), newtimesig64);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert38Time"), "Insert38Time", _("Insert 3/8 Time"), _("No Tooltip yet"), newtimesig38);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert68Time"), "Insert68Time", _("Insert 6/8 Time"), _("No Tooltip yet"), newtimesig68);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert128Time"), "Insert128Time", _("Insert 12/8 Time"), _("No Tooltip yet"), newtimesig128);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Insert98Time"), "Insert98Time", _("Insert 9/8 Time"), _("No Tooltip yet"), newtimesig98);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set22Time"), "Set22Time", _("Set 2/2 Time"), _("No Tooltip yet"), settimesig22);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set32Time"), "Set32Time", _("Set 3/2 Time"), _("No Tooltip yet"), settimesig32);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set42Time"), "Set42Time", _("Set 4/2 Time"), _("No Tooltip yet"), settimesig42);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set44Time"), "Set44Time", _("Set 4/4 Time"), _("No Tooltip yet"), settimesig44);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set34Time"), "Set34Time", _("Set 3/4 Time"), _("No Tooltip yet"), settimesig34);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set24Time"), "Set24Time", _("Set 2/4 Time"), _("No Tooltip yet"), settimesig24);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set64Time"), "Set64Time", _("Set 6/4 Time"), _("No Tooltip yet"), settimesig64);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set38Time"), "Set38Time", _("Set 3/8 Time"), _("No Tooltip yet"), settimesig38);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set68Time"), "Set68Time", _("Set 6/8 Time"), _("No Tooltip yet"), settimesig68);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set128Time"), "Set128Time", _("Set 12/8 Time"), _("No Tooltip yet"), settimesig128);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Set98Time"), "Set98Time", _("Set 9/8 Time"), _("No Tooltip yet"), settimesig98);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertCmaj"), "InsertCmaj", _("Insert Cmaj"), _("No Tooltip yet"), newkeysigcmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertGmaj"), "InsertGmaj", _("Insert Gmaj"), _("No Tooltip yet"), newkeysiggmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDmaj"), "InsertDmaj", _("Insert Dmaj"), _("No Tooltip yet"), newkeysigdmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertAmaj"), "InsertAmaj", _("Insert Amaj"), _("No Tooltip yet"), newkeysigamaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertEmaj"), "InsertEmaj", _("Insert Emaj"), _("No Tooltip yet"), newkeysigemaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBmaj"), "InsertBmaj", _("Insert Bmaj"), _("No Tooltip yet"), newkeysigbmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertFSharpmaj"), "InsertFSharpmaj", _("Insert F# Major"), _("No Tooltip yet"), newkeysigfsharpmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertCSharpmaj"), "InsertCSharpmaj", _("Insert C# Major"), _("No Tooltip yet"), newkeysigcsharpmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertFmaj"), "InsertFmaj", _("Insert F Major"), _("No Tooltip yet"), newkeysigfmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBflatmaj"), "InsertBflatmaj", _("Insert Bb Major"), _("No Tooltip yet"), newkeysigbflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertEflatmaj"), "InsertEflatmaj", _("Insert Eb Major"), _("No Tooltip yet"), newkeysigeflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertAflatmaj"), "InsertAflatmaj", _("Insert Ab Major"), _("No Tooltip yet"), newkeysigaflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDflatmaj"), "InsertDflatmaj", _("Insert Db Major"), _("No Tooltip yet"), newkeysigdflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertGflatmaj"), "InsertGflatmaj", _("Insert Gb Major"), _("No Tooltip yet"), newkeysiggflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertCflatmaj"), "InsertCflatmaj", _("Insert Cb Major"), _("No Tooltip yet"), newkeysigcflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertAmin"), "InsertAmin", _("Insert A Minor"), _("No Tooltip yet"), newkeysigamin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertEmin"), "InsertEmin", _("Insert E Minor"), _("No Tooltip yet"), newkeysigemin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBmin"), "InsertBmin", _("Insert B Minor"), _("No Tooltip yet"), newkeysigbmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertFSharpmin"), "InsertFSharpmin", _("Insert F# Minor"), _("No Tooltip yet"), newkeysigfsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertCSharpmin"), "InsertCSharpmin", _("Insert C# Minor"), _("No Tooltip yet"), newkeysigcsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertGSharpmin"), "InsertGSharpmin", _("Insert G# Minor"), _("No Tooltip yet"), newkeysiggsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDSharpmin"), "InsertDSharpmin", _("Insert D# Minor"), _("No Tooltip yet"), newkeysigdsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertASharpmin"), "InsertASharpmin", _("Insert A# Minor"), _("No Tooltip yet"), newkeysigasharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDmin"), "InsertDmin", _("Insert D Minor"), _("No Tooltip yet"), newkeysigdmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertGmin"), "InsertGmin", _("Insert G Minor"), _("No Tooltip yet"), newkeysiggmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertCmin"), "InsertCmin", _("Insert C Minor"), _("No Tooltip yet"), newkeysigcmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertFmin"), "InsertFmin", _("Insert F Minor"), _("No Tooltip yet"), newkeysigfmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBflatmin"), "InsertBflatmin", _("Insert Bb Minor"), _("No Tooltip yet"), newkeysigbflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertEflatmin"), "InsertEflatmin", _("Insert Eb Minor"), _("No Tooltip yet"), newkeysigeflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertAflatmin"), "InsertAflatmin", _("Insert Ab Minor"), _("No Tooltip yet"), newkeysigaflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialCmaj"), "SetInitialCmaj", _("Set Initial Keysig to C Major"), _("No Tooltip yet"), setkeysigcmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialGmaj"), "SetInitialGmaj", _("Set Initial Keysig to G Major"), _("No Tooltip yet"), setkeysiggmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialDmaj"), "SetInitialDmaj", _("Set D Major as Initial Keysig"), _("No Tooltip yet"), setkeysigdmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialAmaj"), "SetInitialAmaj", _("Set A Major as Initial Keysig"), _("No Tooltip yet"), setkeysigamaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialEmaj"), "SetInitialEmaj", _("Set E Major as Initial Keysig"), _("No Tooltip yet"), setkeysigemaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialBmaj"), "SetInitialBmaj", _("Set B Major as Initial Keysig"), _("No Tooltip yet"), setkeysigbmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialFSharpmaj"), "SetInitialFSharpmaj", _("Set F# Major as Initial Keysig"), _("No Tooltip yet"), setkeysigfsharpmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialCSharpmaj"), "SetInitialCSharpmaj", _("Set C# Major as Initial Keysig"), _("No Tooltip yet"), setkeysigcsharpmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialFmaj"), "SetInitialFmaj", _("Set F Major as Initial Keysig"), _("No Tooltip yet"), setkeysigfmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialBflatmaj"), "SetInitialBflatmaj", _("Set Bb Major as Initial Keysig"), _("No Tooltip yet"), setkeysigbflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialEflatmaj"), "SetInitialEflatmaj", _("Set Eb Major as Initial Keysig"), _("No Tooltip yet"), setkeysigeflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialAflatmaj"), "SetInitialAflatmaj", _("Set Ab Major as Initial Keysig"), _("No Tooltip yet"), setkeysigaflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialDflatmaj"), "SetInitialDflatmaj", _("Set Db Major as Initial Keysig"), _("No Tooltip yet"), setkeysigdflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialGflatmaj"), "SetInitialGflatmaj", _("Set Gb Major as Initial Keysig"), _("No Tooltip yet"), setkeysiggflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialCflatmaj"), "SetInitialCflatmaj", _("Set Cb Major as Initial Keysig"), _("No Tooltip yet"), setkeysigcflatmaj);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialAmin"), "SetInitialAmin", _("Set A Minor as Initial Keysig"), _("No Tooltip yet"), setkeysigamin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialEmin"), "SetInitialEmin", _("Set E Minor as Initial Keysig"), _("No Tooltip yet"), setkeysigemin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialBmin"), "SetInitialBmin", _("Set B Minor as Initial Keysig"), _("No Tooltip yet"), setkeysigbmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialFSharpmin"), "SetInitialFSharpmin", _("Set F# Minor as Initial Keysig"), _("No Tooltip yet"), setkeysigfsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialCSharpmin"), "SetInitialCSharpmin", _("Set C# Minor as Initial Keysig"), _("No Tooltip yet"), setkeysigcsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialGSharpmin"), "SetInitialGSharpmin", _("Set G# Minor as Initial Keysig"), _("No Tooltip yet"), setkeysiggsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialDSharpmin"), "SetInitialDSharpmin", _("Set D# Minor as Initial Keysig"), _("No Tooltip yet"), setkeysigdsharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialASharpmin"), "SetInitialASharpmin", _("Set A# Minor as Initial Keysig"), _("No Tooltip yet"), setkeysigasharpmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialDmin"), "SetInitialDmin", _("Set D Minor as Initial Keysig"), _("No Tooltip yet"), setkeysigdmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialGmin"), "SetInitialGmin", _("Set G Minor as Initial Keysig"), _("No Tooltip yet"), setkeysiggmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialCmin"), "SetInitialCmin", _("Set C Minor as Initial Keysig"), _("No Tooltip yet"), setkeysigcmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialFmin"), "SetInitialFmin", _("Set F Minor as Initial Keysig"), _("No Tooltip yet"), setkeysigfmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialBflatmin"), "SetInitialBflatmin", _("Set Bb Minor as Initial Keysig"), _("No Tooltip yet"), setkeysigbflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialEflatmin"), "SetInitialEflatmin", _("Set Eb Minor as Initial Keysig"), _("No Tooltip yet"), setkeysigeflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetInitialAflatmin"), "SetInitialAflatmin", _("Set Ab Minor as Initial Keysig"), _("No Tooltip yet"), setkeysigaflatmin);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetMark"), "SetMark", _("Set Mark"), _("Sets the start point for a selection,\nthe end point of the selection is unaltered"), set_mark);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "UnsetMark"), "UnsetMark", _("Unset Mark"), _("Gets rid of the selection."), unset_mark);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SetPoint"), "SetPoint", _("Set Point"), _("Extends the selection to the current cursor position"), set_point);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleBeginSlur"), "ToggleBeginSlur", _("Begin Slur"), _("Insert/delete begin slur on this note"), toggle_begin_slur);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleEndSlur"), "ToggleEndSlur", _("End Slur"), _("Insert/delete end slur on this note"), toggle_end_slur);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleStartCrescendo"), "ToggleStartCrescendo", _("Start Crescendo"), _("No Tooltip yet"), toggle_start_crescendo);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleEndCrescendo"), "ToggleEndCrescendo", _("End Crescendo"), _("No Tooltip yet"), toggle_end_crescendo);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleStartDiminuendo"), "ToggleStartDiminuendo", _("Start Diminuendo"), _("No Tooltip yet"), toggle_start_diminuendo);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleEndDiminuendo"), "ToggleEndDiminuendo", _("End Diminuendo"), _("No Tooltip yet"), toggle_end_diminuendo);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleGrace"), "ToggleGrace", _("Grace Note Off/On"), _("Makes the note at the cursor an appogiatura grace note, if it is one, makes it normal"), toggle_grace);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleAcciaccatura"), "ToggleAcciaccatura", _("Acciaccatura Off/On"), _("Makes the note at the cursor an acciaccatura grace note, if it is one, makes it normal"), toggle_acciaccatura);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ForceCaution"), "ForceCaution", _("Force Cautionary Accidental"), _("No Tooltip yet"), force_cautionary);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangePitch"), "ChangePitch", _("Change Pitch"), _("No Tooltip yet"), change_pitch);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertRhythm"), "InsertRhythm", _("Insert Snippet"), _("No Tooltip yet"), insert_rhythm_pattern);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "NextRhythm"), "NextRhythm", _("Next Snippet"), _("Make next snippet\nthe current snippet.\nNotes entered will follow the rhythmic pattern of this snippet"), nextrhythm);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AppendMeasureAllStaffs"), "AppendMeasureAllStaffs", _("Append Measure All Staffs"), _("Appends a blank measure to every staff in this movement"), append_measure_score);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ExecuteScheme"), "ExecuteScheme", _("Execute Scheme"), _("Execute the scheme code from the scripting window"), execute_scheme);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SharpenEnharmonicSet"), "SharpenEnharmonicSet", _("Shift Accidentals Sharpwise"), _("Shifts the set of accidentals one step sharper"), set_sharper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "FlattenEnharmonicSet"), "FlattenEnharmonicSet", _("Shift Accidentals Flatwise"), _("Shifts the set of accidentals one step flatter"), set_flatter);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "New"), "New", _("Empty Score"), _("Start a new musical score"), file_newwrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "NewScore"), "NewScore", _("New"), _("Start a new musical score for a named instrument/voice."), new_score_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Open"), "Open", _("Open"), _("Open a file containing a music score for editing"), file_open_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ImportLilypond"), "ImportLilypond", _("Import Lilypond"), _("Import a Lilypond file"), file_import_lilypond_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ImportMidi"), "ImportMidi", _("Import Midi"), _("Import a Midi file"), file_import_midi_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ImportMusicXml"), "ImportMusicXml", _("Import MusicXml"), _("Import a MusicXml file"), file_import_musicxml_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddStaffs"), "AddStaffs", _("Add Staffs"), _("Add staffs from a Denemo file"), file_add_staffs);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddMovements"), "AddMovements", _("Add Movement"), _("Add movements from a Denemo file"), file_add_movements);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MovementProps"), "MovementProps", _("Change Properties"), _("Change properties of this movement"), movement_props_dialog);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OpenNewWindow"), "OpenNewWindow", _("Open In New"), _("Open a file containing a music score for editing in a separate working area (tab"), openinnew);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Save"), "Save", _("Save"), _("Save the score. The score is saved to disk in XML format."), file_savewrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveAs"), "SaveAs", _("Save As"), _("Save the score under a new name"), file_saveaswrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveCopy"), "SaveCopy", _("Create Copy"), _("Save a copy of the score"), file_copy_save);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OpenTemplate"), "OpenTemplate", _("Open Template"), _("Start a new score from a built-in template file"), system_template_open_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OpenExample"), "OpenExample", _("Open Example"), _("Start a new score from a built-in example"), system_example_open_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "OpenMyTemplate"), "OpenMyTemplate", _("Open Custom Template"), _("Start a new score from one of your own template files"), local_template_open_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveTemplate"), "SaveTemplate", _("Save Template"), _("Save the score as a template for re-use as a starting point for new scores"), template_save);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "NewWindow"), "NewWindow", _("New Tab"), _("Create working area (tab with an empty score in it)"), newview);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertMovementBefore"), "InsertMovementBefore", _("Insert Movement Before"), _("Insert a new movement before the current one"), insert_movement_before);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertMovementAfter"), "InsertMovementAfter", _("Insert Movement After"), _("Insert a new movement after the current one"), insert_movement_after);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "NewMovement"), "NewMovement", _("New Movement"), _("Create a new movement, usign any default template"), append_new_movement);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveParts"), "SaveParts", _("Save Parts"), _("Save Parts: each staff becomes a file in lilypond format"), file_savepartswrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ExportMUDELA"), "ExportMUDELA", _("Export Lilypond"), _("Export the score as a lilypond file"), export_mudela_action);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ExportPDF"), "ExportPDF", _("Export PDF"), _("Export the score as a PDF document file"), export_pdf_action);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ExportPNG"), "ExportPNG", _("Export PNG"), _("Export the score as a PNG image file"), export_png_action);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ExportMIDI"), "ExportMIDI", _("Export MIDI"), _("Export the score as a MIDI file"), export_midi_action);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PrintPreview"), "PrintPreview", _("Print Preview"), _("Displays the final finished score in your pd viewer"), printpreview_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PrintView"), "PrintView", _("Print Preview"), _("Typesets the score\nIf you have a score layout selected it will use that\notherwise all movements staffs and lyrics are typeset by default.\nBe patient! It takes time to create a beautifully laid out score.\nOnce complete you can view and then send to your printer or to a file as a .pdf document."), show_print_view);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PrintSelection"), "PrintSelection", _("Print Selection"), _("Displays selected music from score in your pdf viewer"), printselection_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PrintExcerptPreview"), "PrintExcerptPreview", _("Print Excerpt"), _("Displays a musical excerpt in your image viewer"), printexcerptpreview_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PrintMovement"), "PrintMovement", _("Print Movement"), _("Typesets the current movement and opens a print dialog"), printmovement_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Print"), "Print", _("Print"), _("Typesets the score using LilyPond and opens a print dialog"), printall_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PrintPart"), "PrintPart", _("Print Part"), _("Typesets the current part (the one containing the cursor)."), printpart_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Close"), "Close", _("Close Score"), _("Close the current score. Other scores (tabs) will stay open"), close_gui_with_check);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Quit"), "Quit", _("Quit"), _("Quit the Denemo program - closes tabs one at a time."), closewrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Undo"), "Undo", _("Undo"), _("Undoes one (more) step of your edits to the current score."), undowrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Redo"), "Redo", _("Redo"), _("Redoes the next of the steps you have Undone"), redowrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Copy"), "Copy", _("Copy"), _("Copy the music selected to the Denemo clipboard"), copywrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Cut"), "Cut", _("Cut"), _("Cut the music selected to the Denemo clipboard"), cutwrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Paste"), "Paste", _("Paste"), _("Paste the Denemo clipboard into the score where the cursor is positioned"), pastewrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PasteClipboard"), "PasteClipboard", _("Paste LilyPond notes"), _("Paste LilyPond notes from the text clipboard\nThis will import music written as LilyPond syntax\nYou open the LilyPond file in a texteditor, copy the stretch of notes (control-c command in your texteditor usually) and then use this command."), paste_clipboard);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ScoreProperties"), "ScoreProperties", _("Score Properties"), _("Change some of the properties of the current score. This will start up a dialog window"), score_properties_dialog);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveSelection"), "SaveSelection", _("Save Selection"), _("Save the selected music. Not sure if this is working"), saveselwrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Preferences"), "Preferences", _("Change Preferences"), _("Set and save your preferences for how Denemo operates on startup.\nAdvanced users can edit .denemo-XXXX/denemorc for missing ones"), preferences_change);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SaveAccels"), "SaveAccels", _("Save Command Set"), _("Save the current commands and keyboard shortcuts as the default"), save_default_keymap_file_wrapper);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "CommandManagement"), "CommandManagement", _("Manage Command Set"), _("View help, change and save keyboard shortcuts"), configure_keyboard_dialog);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SwapStaffs"), "SwapStaffs", _("Swap Staffs"), _("Swap this staff with the one higher up.\nBe aware that if you have inserted directives to move a voice to another staff\nthese may need re-making."), swapstaffs);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SplitVoices"), "SplitVoices", _("Split Voices"), _("Split off the next voice as a separate staff"), splitstaffs);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "JoinVoices"), "JoinVoices", _("Join Voices"), _("Merge this staff as a voice on the previous staff"), joinstaffs);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "SwapMovements"), "SwapMovements", _("Swap Movements"), _("Swap this movement with the one before"), swapmovements);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "VoiceUp"), "VoiceUp", _("Voice Up"), _("Go to the higher numbered voice on staff, extending selection if any"), voiceup);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "VoiceDown"), "VoiceDown", _("Voice Down"), _("Go to the lower numbered voice on this staff, extending selection if any"), voicedown);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToVoiceUp"), "MoveToVoiceUp", _("Move to Voice Up"), _("Go to the higher numbered voice on staff without altering selection"), movetovoiceup);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToVoiceDown"), "MoveToVoiceDown", _("Move to Voice Down"), _("Go to the lower numbered voice on this staff without altering selection"), movetovoicedown);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddBefore"), "AddBefore", _("Add Staff Before"), _("Inserts a new staff before the current staff"), newstaffbefore);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddAfter"), "AddAfter", _("Add Staff After"), _("Inserts/Adds a new staff after the current staff"), dnm_newstaffafter);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddInitial"), "AddInitial", _("Add Initial Staff"), _("Inserts a new staff at the top of the score"), newstaffinitial);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddLast"), "AddLast", _("Add Last Staff"), _("Inserts a new staff at the end of the score"), newstafflast);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteBefore"), "DeleteBefore", _("Delete Staff Before"), _("Deletes the staff before the current staff"), delete_staff_before);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteStaff"), "DeleteStaff", _("Delete Current Staff"), _("Deletes the current staff"), delete_staff_current);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteAfter"), "DeleteAfter", _("Delete Staff After"), _("Deletes the staff after the current staff"), delete_staff_after);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddVoice"), "AddVoice", _("Add Voice"), _("Adds a new voice (part), to the current staff. It is tricky to switch between the voices. Suggest to use merge staffs"), dnm_newstaffvoice);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "StaffProperties"), "StaffProperties", _("Staff Properties"), _("Change the properties of the current staff"), staff_properties_change_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InitialClef"), "InitialClef", _("Initial Clef"), _("Change the initial clef of the current staff"), clef_change_initial);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertClef"), "InsertClef", _("Clef Change"), _("Insert/Edit a change of clef at the cursor"), clef_change_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InitialKey"), "InitialKey", _("Initial Key"), _("Set the initial key signature of the current staff"), key_change_initial);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertKey"), "InsertKey", _("Key Signature Change"), _("Insert/Edit a key change at the cursor position"), key_change_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InitialTimeSig"), "InitialTimeSig", _("Inital Time Signature"), _("Set the initial time signature of the current staff"), timesig_change_initial);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertTimeSig"), "InsertTimeSig", _("Time Signature Change"), _("Edit/Insert a time signature change for the current measure"), timesig_change_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeNotehead"), "ChangeNotehead", _("Set Notehead"), _("Change the type of notehead for the current note"), set_notehead);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertStem"), "InsertStem", _("Auto Stemming"), _("Inserts a stem neutral object. After this automatic stem directions are active. You can click on this tag and use Sharpen/StemUp etc commands to change stem direction"), stem_directive_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddVerse"), "AddVerse", _("Add Lyric Verse"), _("Add a verse of lyrics"), add_verse);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteVerse"), "DeleteVerse", _("Delete Verse"), _("Deletes current verse of lyrics from current voice"), delete_verse);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditFiguredBass"), "EditFiguredBass", _("Insert/Edit Figured Bass"), _("Add a bass figure to the current note. Use | sign to split the duration of a note so as to have multiple figures on one note. See Lilypond docs for other notation"), figure_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteFiguredBass"), "DeleteFiguredBass", _("Delete Figures"), _("Delete the figured bass on the current staff"), delete_figured_bass);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "HideFiguredBass"), "HideFiguredBass", _("Hide Figures (Print)"), _("Hide the figured bass on the current staff on printing"), hide_figured_bass);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ShowFiguredBass"), "ShowFiguredBass", _("Show Figures (Print)"), _("Show the figured bass on the current staff on printing"), show_figured_bass);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditChords"), "EditChords", _("Edit Chord Symbols"), _("Allows chord symbols to be added to the current note. E.G.cis:dim7 for c-sharp diminished 7th. See Lilypond docs for notation"), fakechord_insert);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertDynamic"), "InsertDynamic", _("Insert Dynamics"), _("Inserts a dynamic marking at the cursor position"), insert_dynamic);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditObject"), "EditObject", _("Edit Object"), _("Edit the object at the cursor."), edit_object);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditDirective"), "EditDirective", _("Edit Directives"), _("Edit any directives attached to chord/note at cursor."), edit_object_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditStaffDirective"), "EditStaffDirective", _("Edit Staff Directives"), _("Edit any directives attached to staff."), edit_staff_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditVoiceDirective"), "EditVoiceDirective", _("Edit Voice Directives"), _("Edit any directives attached to voice."), edit_voice_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditScoreDirective"), "EditScoreDirective", _("Edit Score Directives"), _("Edit any directives attached to score."), edit_score_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditMovementDirective"), "EditMovementDirective", _("Edit Movement Directives"), _("Edit any directives attached to movement."), edit_movement_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditClefDirective"), "EditClefDirective", _("Edit Clef Directives"), _("Edit any directives attached to clef."), edit_clef_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditTimesigDirective"), "EditTimesigDirective", _("Edit Time Signature Directives"), _("Edit any directives attached to time signature."), edit_timesig_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "EditKeysigDirective"), "EditKeysigDirective", _("Edit Key Signature Directives"), _("Edit any directives attached to key signature."), edit_keysig_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteDirective"), "DeleteDirective", _("Delete a Directive"), _("Delete a directive attached to chord/note at cursor."), delete_chord_or_note_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AttachLilyToNote"), "AttachLilyToNote", _("Attach Lilypond to Note"), _("Attach or edit attached LilyPond text to the note at the cursor. This can be used for guitar fingerings, cautionary accidentals and much more. See LilyPond documentation."), note_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AttachLilyToChord"), "AttachLilyToChord", _("Attach Lilypond to Chord"), _("Attach or edit attached LilyPond text to the chord at the cursor. This can be used for attaching and placing text and much more. See LilyPond documentation."), chord_directive);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertBarline"), "InsertBarline", _("Insert Barline"), _("Inserts specialized barline at the cursor position. Mostly not working"), insert_barline);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "GoToMeasure"), "GoToMeasure", _("Go to Measure"), _("Opens a dialog for going to a numbered measure"), tomeasurenum);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "GoToBeginning"), "GoToBeginning", _("Go to Beginning"), _("Cursor to start of staff/voice, extending selection if any"), tohome);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "GoToEnd"), "GoToEnd", _("Go to End"), _("Cursor to end of staff/voice, extending selection if any"), toend);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToBeginning"), "MoveToBeginning", _("Move to Staff/Voice Beginning"), _("Cursor to start of staff/voice, without extending selection if any"), movetostart);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToEnd"), "MoveToEnd", _("Move to Staff/Voice End"), _("Cursor to end of staff/voice, without extending selection if any"), movetoend);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "NextMovement"), "NextMovement", _("Next Movement"), _("Go to the next movement"), next_movement);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PreviousMovement"), "PreviousMovement", _("Previous Movement"), _("Go to the previous movement"), prev_movement);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteMovement"), "DeleteMovement", _("Delete Movement"), _("Delete the current movement"), delete_movement);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Play"), "Play", _("Play"), _("Play"), ext_midi_playback);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Stop"), "Stop", _("Stop"), _("Stop"), stop_midi_playback);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "PlaybackProperties"), "PlaybackProperties", _("Playback Properties"), _("Allows you to specify properties used in playing back (midi)"), playback_properties_change);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "Help"), "Help", _("Browse Manual"), _("Opens a browser on the user manual"), browse_manual);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "About"), "About", _("About"), _("Gives the version number etc of this program"), about);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoreCommands"), "MoreCommands", _("More Commands"), _("Allows choosing standard extra commands/menu items"), morecommands);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MyCommands"), "MyCommands", _("My Commands"), _("Allows choosing extra commands/menu items from your own collection of extras"), mycommands);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "FetchCommands"), "FetchCommands", _("Update Commands from Internet"), _("Refreshes the set of commands available from Denemo.org.\nUse More Commands after this has finished"), fetchcommands);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleEdit"), "ToggleEdit", _("Toggle Edit Mode"), _("Toggle between current mode and edit mode"), toggle_edit_mode);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleRest"), "ToggleRest", _("Toggle Rest Mode"), _("Toggle between note entry and rest entry"), toggle_rest_mode);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ToggleRhythm"), "ToggleRhythm", _("Toggle Audible Feedback"), _("Toggle audible feedback on/off"), toggle_rhythm_mode);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ClearOverlay"), "ClearOverlay", _("Clear Overlay"), _("Clear the list of pitches that overlay the notes"), clear_overlay);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "CreateRhythm"), "CreateRhythm", _("Create Snippet"), _("Copy selection as music snippet or rhythm pattern for notes to follow as they are entered"), create_rhythm_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "DeleteRhythm"), "DeleteRhythm", _("Delete Snippet"), _("Delete the selected music snippet/rhythm pattern"), delete_rhythm_cb);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertA"), "InsertA", _("Insert A"),_("Inserts note A before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  InsertA);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteA"), "AddNoteA", _("Insert A After"),_("Inserts note A after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  AddNoteA);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddA"), "AddA", _("Add A"),_("Adds note A to the chord at cursor\nCursor height determines which octave"),  AddA);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToA"), "ChangeToA", _("Change to A"),_("Changes note at cursor to nearest note A\nRhythm is unchanged"),  ChangeToA);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToA"), "MoveToA", _("Move to A"),_("Moves cursor to nearest note A"),  MoveToA);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertB"), "InsertB", _("Insert B"),_("Inserts note B before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  InsertB);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteB"), "AddNoteB", _("Insert B After"),_("Inserts note B after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  AddNoteB);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddB"), "AddB", _("Add B"),_("Adds note B to the chord at cursor\nCursor height determines which octave"),  AddB);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToB"), "ChangeToB", _("Change to B"),_("Changes note at cursor to nearest note B\nRhythm is unchanged"),  ChangeToB);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToB"), "MoveToB", _("Move to B"),_("Moves cursor to nearest note B"),  MoveToB);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertC"), "InsertC", _("Insert C"),_("Inserts note C before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  InsertC);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteC"), "AddNoteC", _("Insert C After"),_("Inserts note C after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  AddNoteC);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddC"), "AddC", _("Add C"),_("Adds note C to the chord at cursor\nCursor height determines which octave"),  AddC);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToC"), "ChangeToC", _("Change to C"),_("Changes note at cursor to nearest note C\nRhythm is unchanged"),  ChangeToC);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToC"), "MoveToC", _("Move to C"),_("Moves cursor to nearest note C"),  MoveToC);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertD"), "InsertD", _("Insert D"),_("Inserts note D before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  InsertD);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteD"), "AddNoteD", _("Insert D After"),_("Inserts note D after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  AddNoteD);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddD"), "AddD", _("Add D"),_("Adds note D to the chord at cursor\nCursor height determines which octave"),  AddD);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToD"), "ChangeToD", _("Change to D"),_("Changes note at cursor to nearest note D\nRhythm is unchanged"),  ChangeToD);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToD"), "MoveToD", _("Move to D"),_("Moves cursor to nearest note D"),  MoveToD);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertE"), "InsertE", _("Insert E"),_("Inserts note E before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  InsertE);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteE"), "AddNoteE", _("Insert E After"),_("Inserts note E after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  AddNoteE);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddE"), "AddE", _("Add E"),_("Adds note E to the chord at cursor\nCursor height determines which octave"),  AddE);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToE"), "ChangeToE", _("Change to E"),_("Changes note at cursor to nearest note E\nRhythm is unchanged"),  ChangeToE);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToE"), "MoveToE", _("Move to E"),_("Moves cursor to nearest note E"),  MoveToE);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertF"), "InsertF", _("Insert F"),_("Inserts note F before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  InsertF);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteF"), "AddNoteF", _("Insert F After"),_("Inserts note F after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  AddNoteF);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddF"), "AddF", _("Add F"),_("Adds note F to the chord at cursor\nCursor height determines which octave"),  AddF);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToF"), "ChangeToF", _("Change to F"),_("Changes note at cursor to nearest note F\nRhythm is unchanged"),  ChangeToF);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToF"), "MoveToF", _("Move to F"),_("Moves cursor to nearest note F"),  MoveToF);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "InsertG"), "InsertG", _("Insert G"),_("Inserts note G before note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  InsertG);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddNoteG"), "AddNoteG", _("Insert G After"),_("Inserts note G after note at cursor\nCursor determines which octave\nNote is inserted in the prevailing rhythm"),  AddNoteG);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "AddG"), "AddG", _("Add G"),_("Adds note G to the chord at cursor\nCursor height determines which octave"),  AddG);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "ChangeToG"), "ChangeToG", _("Change to G"),_("Changes note at cursor to nearest note G\nRhythm is unchanged"),  ChangeToG);
register_command(Denemo.map, gtk_action_group_get_action(action_group, "MoveToG"), "MoveToG", _("Move to G"),_("Moves cursor to nearest note G"),  MoveToG);
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
