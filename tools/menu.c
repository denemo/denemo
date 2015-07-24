struct name_and_function denemo_commands[] = {

  {NULL, "Moves the cursor one object left, altering the selection if any", N_("CursorLeft"), "cursorleft", N_("Selection Cursor Left")},
  {NULL, "Moves the cursor one object left, without altering the selection", N_("MoveCursorLeft"), "movecursorleft", N_("Move Cursor Left")},
  {NULL, "Moves the cursor one scale step down", N_("CursorDown"), "cursordown", N_("Cursor Down")},
  {NULL, "Moves the cursor one scale step up", N_("CursorUp"), "cursorup", N_("Cursor Up")},
  {NULL, "Moves the cursor one object right, altering the selection if any", N_("CursorRight"), "cursorright", N_("Selection Cursor Right")},
  {NULL, "Moves the cursor one object right, without altering the selection", N_("MoveCursorRight"), "movecursorright", N_("Move Cursor Right")},

  {NULL, "Moves the cursor to the Mark without altering the selection", N_("GoToMark"), "goto_mark", N_("To Mark")},

  {NULL, "Swaps the active end of the selection", N_("SwapPointAndMark"), "swap_point_and_mark", N_("Swap Ends of Selection")},



  {NULL, "Moves the cursor to the first object in the selection without altering the selection. returns #f if no selection", N_("GoToSelectionStart"), "goto_selection_start", N_("To Selection Start")},

  {NULL, "Pushes the current cursor position onto a stack", N_("PushPosition"), "PushPosition", N_("Push Position")},
  {NULL, "Pops a position from the stack of cursor positions, moving the cursor there", N_("PopPosition"), "PopPosition", N_("Pop Position")},
  {NULL, "Pops a position from the stack of cursor positions, pushes the current position, then moves the cursor to the popped position", N_("PopPushPosition"), "PopPushPosition", N_("Pop and Push Position")},


  {NULL, "Hides/Shows menus, panes etc. The ones shown are those checked in the view menu.", N_("ToggleReduceToDrawingArea"), "ToggleReduceToDrawingArea", N_("Hide/Show Menus")},


  {NULL, "Moves the cursor to the staff above, extending selection if any", N_("StaffUp"), "staffup", N_("Selection Staff Up")},
  {NULL, "Moves the cursor to the staff below, extending selection if any", N_("StaffDown"), "staffdown", N_("Selection Staff Down")},

  {NULL, "Moves the cursor to the staff above without altering selection. On the top staff it adds space above the staffs.", N_("MoveToStaffUp"), "movetostaffup", N_("Move to Staff Up")},
  {NULL, "Moves the cursor to the staff below without altering selection", N_("MoveToStaffDown"), "movetostaffdown", N_("Move to Staff Down")},



  {NULL, "Moves the cursor to the first object in the next measure, extending selection if any", N_("MeasureLeft"), "measureleft", N_("Measure Left")},
  {NULL, "Moves the cursor to the first object in the previous measure, extending selection if any", N_("MeasureRight"), "measureright", N_("Measure Right")},

  {NULL, "Moves the cursor to the first object in the next measure leaving selection, if any, unchanged", N_("MoveToMeasureLeft"), "movetomeasureleft", N_("Move to Measure Left")},
  {NULL, "Moves the cursor to the first object in the previous measure leaving selection, if any, unchanged", N_("MoveToMeasureRight"), "movetomeasureright", N_("Move to Measure Right")},



  {NULL, "Changes the note the cursor is on to the note A.\\nIf the cursor is in the appending position, appends a note using the prevailing duration.", N_("A"), "go_to_A_key", N_("Change/Append A")},
  {NULL, "Changes the note the cursor is on to the note B.\\nIf the cursor is in the appending position, appends a note using the prevailing duration.", N_("B"), "go_to_B_key", N_("Change/Append B")},
  {NULL, "Changes the note the cursor is on to the note C.\\nIf the cursor is in the appending position, appends a note using the prevailing duration.", N_("C"), "go_to_C_key", N_("Change/Append C")},
  {NULL, "Changes the note the cursor is on to the note D.\\nIf the cursor is in the appending position, appends a note using the prevailing duration.", N_("D"), "go_to_D_key", N_("Change/Append D")},
  {NULL, "Changes the note the cursor is on to the note E.\\nIf the cursor is in the appending position, appends a note using the prevailing duration.", N_("E"), "go_to_E_key", N_("Change/Append E")},
  {NULL, "Changes the note the cursor is on to the note F.\\nIf the cursor is in the appending position, appends a note using the prevailing duration.", N_("F"), "go_to_F_key", N_("Change/Append F")},
  {NULL, "Changes the note the cursor is on to the note G.\\nIf the cursor is in the appending position, appends a note using the prevailing duration.", N_("G"), "go_to_G_key", N_("Change/Append G")},
  {NULL, "Changes the note at the cursor to an octave higher", N_("OctaveUp"), "octave_up_key", N_("Octave Up")},
  {NULL, "Changes the note at the cursor to an octave lower", N_("OctaveDown"), "octave_down_key", N_("Octave Down")},

  {NULL, "Insert " NOTE0, N_("WholeNote"), "insert_chord_0key", NULL},
  {NULL, "Insert " NOTE1, N_("HalfNote"), "insert_chord_1key", NULL},
  {NULL, "Insert " NOTE2, N_("QuarterNote"), "insert_chord_2key", NULL},
  {NULL, "Insert " NOTE3, N_("EighthNote"), "insert_chord_3key", NULL},
  {NULL, "Insert " NOTE4, N_("SixteenthNote"), "insert_chord_4key", NULL},
  {NULL, "Insert " NOTE5, N_("ThirtysecondNote"), "insert_chord_5key", NULL},
  {NULL, "Insert " NOTE6, N_("SixtyfourthNote"), "insert_chord_6key", NULL},
  {NULL, "Insert " NOTE7, N_("OneHundredTwentyEighthNote"), "insert_chord_7key", NULL},
  {NULL, "Insert " NOTE8, N_("TwoHundredFiftySixthNote"), "insert_chord_8key", NULL},

  {NULL, "Insert " REST0 " rest", N_("InsertWholeRest"), "insert_rest_0key", "Insert a " REST0},
  {NULL, "Insert " REST1 " rest", N_("InsertHalfRest"), "insert_rest_1key", "Insert a " REST1},
  {NULL, "Insert " REST2 " rest", N_("InsertQuarterRest"), "insert_rest_2key", "Insert a " REST2},
  {NULL, "Insert " REST3 " rest", N_("InsertEighthRest"), "insert_rest_3key", "Insert a " REST3},
  {NULL, "Insert " REST4 " rest", N_("InsertSixteenthRest"), "insert_rest_4key", "Insert a " REST4},
  {NULL, "Insert " REST5 " rest", N_("InsertThirtysecondRest"), "insert_rest_5key", "Insert a " REST5},
  {NULL, "Insert " REST6 " rest", N_("InsertSixtyfourthRest"), "insert_rest_6key", "Insert a " REST6},

  {NULL, "Insert a non-printing " REST0 " rest", N_("InsertBlankWholeNote"), "insert_blankchord_0key", "Insert a " REST0 " Spacer"},
  {NULL, "Insert a non-printing " REST1 " rest", N_("InsertBlankHalfNote"), "insert_blankchord_1key", "Insert a " REST1 " Spacer"},
  {NULL, "Insert a non-printing " REST2 " rest", N_("InsertBlankQuarterNote"), "insert_blankchord_2key", "Insert a " REST2 " Spacer"},
  {NULL, "Insert a non-printing " REST3 " rest", N_("InsertBlankEighthNote"), "insert_blankchord_3key", "Insert a " REST3 " Spacer"},
  {NULL, "Insert a non-printing " REST4 " rest", N_("InsertBlankSixteenthNote"), "insert_blankchord_4key", "Insert a " REST4 " Spacer"},
  {NULL, "Insert a non-printing " REST5 " rest", N_("InsertBlankThirtysecondNote"), "insert_blankchord_5key", "Insert a " REST5 " Spacer"},
  {NULL, "Insert a non-printing " REST6 " rest", N_("InsertBlankSixtyfourthNote"), "insert_blankchord_6key", "Insert a " REST6 " Spacer"},
  {NULL, "Insert a non-printing " REST7 " rest", N_("InsertBlankOneHundredTwentyEighthNote"), "insert_blankchord_7key", "Insert a " REST7 " Spacer"},
  {NULL, "Insert a non-printing " REST8 " rest", N_("InsertBlankTwoHundredFiftySixthNote"), "insert_blankchord_8key", "Insert a " REST8 " Spacer"},




  {NULL, "No Tooltip yet", N_("ToggleRestMode"), "rest_toggle_key", N_("Toggle Rest Mode")},
  {NULL, "No Tooltip yet", N_("ToggleBlankMode"), "toggle_blank", N_("Toggle Blank Mode")},



  {NULL, "No Tooltip yet", N_("InsertDuplet"), "duplet_insert", N_("Insert Duplet")},
  {NULL, "Inserts a Start Triplet object and an End Tuplet object and places the cursor between these two", N_("InsertTriplet"), "triplet_insert", N_("Insert Triplet")},
  {NULL, "Inserts an Start Triplet object, which makes the notes following take 2/3 of their written duration. Later in this measure there should be an End Tuplet object.", N_("StartTriplet"), "triplet_start", N_("Start Triplet")},
  {NULL, "Inserts an End Tuplet object, which terminates a tuplet started earlier in this measure.", N_("EndTuplet"), "tuplet_end", N_("End Tuplet")},
  {NULL, "No Tooltip yet", N_("InsertQuadtuplet"), "insert_quadtuplet", N_("Insert Quadruplet")},
  {NULL, "No Tooltip yet", N_("InsertQuintuplet"), "quintuplet_insert", N_("Insert Quintuplet")},
  {NULL, "No Tooltip yet", N_("InsertSextuplet"), "sextuplet_insert", N_("Insert Sextuplet")},
  {NULL, "No Tooltip yet", N_("InsertSeptuplet"), "septuplet_insert", N_("Insert Septuplet")},
  {NULL, "Add a note to the current chord\\nThe cursor position determines which note to add", N_("AddNoteToChord"), "add_tone_key", "Add note"},
  {NULL, "Remove a note from the current chord, based on the cursor position", N_("RemoveNoteFromChord"), "remove_tone_key", "Remove note"},

  {NULL, "Sharpen the note at the cursor", N_("Sharpen"), "sharpen_key", "Sharpen"},
  {NULL, "Flatten the note at the cursor", N_("Flatten"), "flatten_key", "Flatten"},
  {NULL, "Increases the sharpness of the next entered note. The status bar shows the current state.", N_("PendingSharpen"), "pending_sharpen", "Sharpen Next Note"},
  {NULL, "Increases the flatness of the next entered note. The status bar shows the current state.", N_("PendingFlatten"), "pending_flatten", "Flatten Next Note"},
  {NULL, "Alters a StemNeutral object to stem up.", N_("StemUp"), "stem_up", "StemUp"},
  {NULL, "Alters a StemNeutral object to stem down.", N_("StemDown"), "stem_down", "StemDown"},

  {NULL, "Lengthen the chord, note or rest at the cursor by dotting it.", N_("AddDot"), "add_dot_key", N_("Add Dot")},
  {NULL, "Reduce the dotting of the chord note or rest at the cursor.", N_("RemoveDot"), "remove_dot_key", N_("Remove Dot")},

  {NULL, "Inserts a duplicate of the current note, tied", N_("InsertTiedNote"), "tie_notes_key", "Tied note"},
  {NULL, "Ties/unties the note at the cursor. The following note should be the same pitch.", N_("ToggleTie"), "toggle_tie", "Tie (Off/On)"},

  {NULL, "Delete the object at the cursor", N_("DeleteObject"), "deleteobject", N_("Delete Object")},
  {NULL, "Delete to the left of the cursor.", N_("DeletePreviousObject"), "deletepreviousobject", N_("Delete Previous Object")},

  {NULL, "Insert a blank measure before the current one (in all staffs)", N_("InsertMeasure"), "insert_measure_key", N_("Insert Measure Before")},
  {NULL, "Insert a blank measure after the current one (in all staffs)", N_("AddMeasure"), "addmeasureafter", N_("Insert Measure After")},



  {NULL, "Insert a blank measure before the current one (in current staff)", N_("InsertMeasureBefore"), "insertmeasurebefore", N_("Staff Insert Measure Before")},
  {NULL, "Insert a blank measure in current staff after the current measure", N_("InsertMeasureAfter"), "insertmeasureafter", N_("Staff Insert Measure After")},


  {NULL, "Append an empty measure at the end of the current staff", N_("AppendMeasure"), "append_measure_key", N_("Staff Append Measure")},
  {NULL, "Delete the current measure in this staff, leaving the staff short", N_("DeleteMeasure"), "deletemeasure", N_("Staff Delete Measure")},
  {NULL, "Delete the current measure in all staffs", N_("DeleteMeasureAllStaffs"), "deletemeasureallstaffs", N_("Delete Measure All Staffs")},
  {NULL, "No Tooltip yet", N_("ShrinkMeasures"), "adjust_measure_less_width_key", N_("Shrink Measure")},
  {NULL, "No Tooltip yet", N_("WidenMeasures"), "adjust_measure_more_width_key", N_("Widen Measures")},

  {NULL, "No Tooltip yet", N_("ShorterStaffs"), "adjust_staff_less_height_key", N_("Shorter Staffs")},
  {NULL, "No Tooltip yet", N_("TallerStaffs"), "adjust_staff_more_height_key", N_("Taller Staffs")},

  {NULL, "No Tooltip yet", N_("InsertTrebleClef"), "clef_new_treble", N_("New Treble Clef")},
  {NULL, "No Tooltip yet", N_("InsertBassClef"), "clef_new_bass", N_("New Bass Clef")},
  {NULL, "No Tooltip yet", N_("Insertg8clef"), "clef_new_g8", N_("New G8 Clef")},
  {NULL, "No Tooltip yet", N_("InsertAltoClef"), "clef_new_alto", N_("New Alto Clef")},
  {NULL, "No Tooltip yet", N_("InsertTenorClef"), "clef_new_tenor", N_("New Tenor Clef")},
  {NULL, "No Tooltip yet", N_("InsertSopranoClef"), "clef_new_soprano", N_("New Soprano Clef")},

  {NULL, "No Tooltip yet", N_("SetInitialTrebleClef"), "clef_set_treble", N_("Set Treble Clef")},
  {NULL, "No Tooltip yet", N_("SetInitialBassClef"), "clef_set_bass", N_("Set Bass Clef")},
  {NULL, "No Tooltip yet", N_("SetInitialg8clef"), "clef_set_g8", N_("Set G8 Clef")},
  {NULL, "No Tooltip yet", N_("SetInitialAltoClef"), "clef_set_alto", N_("Set Alto Clef")},
  {NULL, "No Tooltip yet", N_("SetInitialTenorClef"), "clef_set_tenor", N_("Set Tenor Clef")},
  {NULL, "No Tooltip yet", N_("SetInitialSopranoClef"), "clef_set_soprano", N_("Set Soprano Clef")},

  {NULL, "No Tooltip yet", N_("Insert22Time"), "newtimesig22", N_("Insert 2/2 Time")},
  {NULL, "No Tooltip yet", N_("Insert32Time"), "newtimesig32", N_("Insert 3/2 Time")},
  {NULL, "No Tooltip yet", N_("Insert42Time"), "newtimesig42", N_("Insert 4/2 Time")},
  {NULL, "No Tooltip yet", N_("Insert44Time"), "newtimesig44", N_("Insert 4/4 Time")},
  {NULL, "No Tooltip yet", N_("Insert34Time"), "newtimesig34", N_("Insert 3/4 Time")},
  {NULL, "No Tooltip yet", N_("Insert24Time"), "newtimesig24", N_("Insert 2/4 Time")},
  {NULL, "No Tooltip yet", N_("Insert64Time"), "newtimesig64", N_("Insert 6/4 Time")},
  {NULL, "No Tooltip yet", N_("Insert38Time"), "newtimesig38", N_("Insert 3/8 Time")},
  {NULL, "No Tooltip yet", N_("Insert68Time"), "newtimesig68", N_("Insert 6/8 Time")},
  {NULL, "No Tooltip yet", N_("Insert128Time"), "newtimesig128", N_("Insert 12/8 Time")},
  {NULL, "No Tooltip yet", N_("Insert98Time"), "newtimesig98", N_("Insert 9/8 Time")},
  {NULL, "No Tooltip yet", N_("Set22Time"), "settimesig22", N_("Set 2/2 Time")},
  {NULL, "No Tooltip yet", N_("Set32Time"), "settimesig32", N_("Set 3/2 Time")},
  {NULL, "No Tooltip yet", N_("Set42Time"), "settimesig42", N_("Set 4/2 Time")},
  {NULL, "No Tooltip yet", N_("Set44Time"), "settimesig44", N_("Set 4/4 Time")},
  {NULL, "No Tooltip yet", N_("Set34Time"), "settimesig34", N_("Set 3/4 Time")},
  {NULL, "No Tooltip yet", N_("Set24Time"), "settimesig24", N_("Set 2/4 Time")},
  {NULL, "No Tooltip yet", N_("Set64Time"), "settimesig64", N_("Set 6/4 Time")},
  {NULL, "No Tooltip yet", N_("Set38Time"), "settimesig38", N_("Set 3/8 Time")},
  {NULL, "No Tooltip yet", N_("Set68Time"), "settimesig68", N_("Set 6/8 Time")},
  {NULL, "No Tooltip yet", N_("Set128Time"), "settimesig128", N_("Set 12/8 Time")},
  {NULL, "No Tooltip yet", N_("Set98Time"), "settimesig98", N_("Set 9/8 Time")},
  {NULL, "No Tooltip yet", N_("InsertCmaj"), "keysig_new_cmaj", N_("Insert Cmaj")},
  {NULL, "No Tooltip yet", N_("InsertGmaj"), "keysig_new_gmaj", N_("Insert Gmaj")},
  {NULL, "No Tooltip yet", N_("InsertDmaj"), "keysig_new_dmaj", N_("Insert Dmaj")},
  {NULL, "No Tooltip yet", N_("InsertAmaj"), "keysig_new_amaj", N_("Insert Amaj")},
  {NULL, "No Tooltip yet", N_("InsertEmaj"), "keysig_new_emaj", N_("Insert Emaj")},
  {NULL, "No Tooltip yet", N_("InsertBmaj"), "keysig_new_bmaj", N_("Insert Bmaj")},
  {NULL, "No Tooltip yet", N_("InsertFSharpmaj"), "keysig_new_fsharpmaj", N_("Insert F# Major")},
  {NULL, "No Tooltip yet", N_("InsertCSharpmaj"), "keysig_new_csharpmaj", N_("Insert C# Major")},
  {NULL, "No Tooltip yet", N_("InsertFmaj"), "keysig_new_fmaj", N_("Insert F Major")},
  {NULL, "No Tooltip yet", N_("InsertBflatmaj"), "keysig_new_bflatmaj", N_("Insert Bb Major")},
  {NULL, "No Tooltip yet", N_("InsertEflatmaj"), "keysig_new_eflatmaj", N_("Insert Eb Major")},
  {NULL, "No Tooltip yet", N_("InsertAflatmaj"), "keysig_new_aflatmaj", N_("Insert Ab Major")},
  {NULL, "No Tooltip yet", N_("InsertDflatmaj"), "keysig_new_dflatmaj", N_("Insert Db Major")},
  {NULL, "No Tooltip yet", N_("InsertGflatmaj"), "keysig_new_gflatmaj", N_("Insert Gb Major")},
  {NULL, "No Tooltip yet", N_("InsertCflatmaj"), "keysig_new_cflatmaj", N_("Insert Cb Major")},
  {NULL, "No Tooltip yet", N_("InsertAmin"), "keysig_new_amin", N_("Insert A Minor")},
  {NULL, "No Tooltip yet", N_("InsertEmin"), "keysig_new_emin", N_("Insert E Minor")},
  {NULL, "No Tooltip yet", N_("InsertBmin"), "keysig_new_bmin", N_("Insert B Minor")},
  {NULL, "No Tooltip yet", N_("InsertFSharpmin"), "keysig_new_fsharpmin", N_("Insert F# Minor")},
  {NULL, "No Tooltip yet", N_("InsertCSharpmin"), "keysig_new_csharpmin", N_("Insert C# Minor")},
  {NULL, "No Tooltip yet", N_("InsertGSharpmin"), "keysig_new_gsharpmin", N_("Insert G# Minor")},
  {NULL, "No Tooltip yet", N_("InsertDSharpmin"), "keysig_new_dsharpmin", N_("Insert D# Minor")},
  {NULL, "No Tooltip yet", N_("InsertASharpmin"), "keysig_new_asharpmin", N_("Insert A# Minor")},
  {NULL, "No Tooltip yet", N_("InsertDmin"), "keysig_new_dmin", N_("Insert D Minor")},
  {NULL, "No Tooltip yet", N_("InsertGmin"), "keysig_new_gmin", N_("Insert G Minor")},
  {NULL, "No Tooltip yet", N_("InsertCmin"), "keysig_new_cmin", N_("Insert C Minor")},
  {NULL, "No Tooltip yet", N_("InsertFmin"), "keysig_new_fmin", N_("Insert F Minor")},
  {NULL, "No Tooltip yet", N_("InsertBflatmin"), "keysig_new_bflatmin", N_("Insert Bb Minor")},
  {NULL, "No Tooltip yet", N_("InsertEflatmin"), "keysig_new_eflatmin", N_("Insert Eb Minor")},
  {NULL, "No Tooltip yet", N_("InsertAflatmin"), "keysig_new_aflatmin", N_("Insert Ab Minor")},

  //Functions to Set Initial Key Sig
  {NULL, "No Tooltip yet", N_("SetInitialCmaj"), "keysig_set_cmaj", N_("Set Initial Keysig to C Major")},
  {NULL, "No Tooltip yet", N_("SetInitialGmaj"), "keysig_set_gmaj", N_("Set Initial Keysig to G Major")},
  {NULL, "No Tooltip yet", N_("SetInitialDmaj"), "keysig_set_dmaj", N_("Set D Major as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialAmaj"), "keysig_set_amaj", N_("Set A Major as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialEmaj"), "keysig_set_emaj", N_("Set E Major as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialBmaj"), "keysig_set_bmaj", N_("Set B Major as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialFSharpmaj"), "keysig_set_fsharpmaj", N_("Set F# Major as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialCSharpmaj"), "keysig_set_csharpmaj", N_("Set C# Major as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialFmaj"), "keysig_set_fmaj", N_("Set F Major as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialBflatmaj"), "keysig_set_bflatmaj", N_("Set Bb Major as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialEflatmaj"), "keysig_set_eflatmaj", N_("Set Eb Major as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialAflatmaj"), "keysig_set_aflatmaj", N_("Set Ab Major as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialDflatmaj"), "keysig_set_dflatmaj", N_("Set Db Major as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialGflatmaj"), "keysig_set_gflatmaj", N_("Set Gb Major as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialCflatmaj"), "keysig_set_cflatmaj", N_("Set Cb Major as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialAmin"), "keysig_set_amin", N_("Set A Minor as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialEmin"), "keysig_set_emin", N_("Set E Minor as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialBmin"), "keysig_set_bmin", N_("Set B Minor as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialFSharpmin"), "keysig_set_fsharpmin", N_("Set F# Minor as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialCSharpmin"), "keysig_set_csharpmin", N_("Set C# Minor as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialGSharpmin"), "keysig_set_gsharpmin", N_("Set G# Minor as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialDSharpmin"), "keysig_set_dsharpmin", N_("Set D# Minor as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialASharpmin"), "keysig_set_asharpmin", N_("Set A# Minor as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialDmin"), "keysig_set_dmin", N_("Set D Minor as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialGmin"), "keysig_set_gmin", N_("Set G Minor as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialCmin"), "keysig_set_cmin", N_("Set C Minor as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialFmin"), "keysig_set_fmin", N_("Set F Minor as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialBflatmin"), "keysig_set_bflatmin", N_("Set Bb Minor as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialEflatmin"), "keysig_set_eflatmin", N_("Set Eb Minor as Initial Keysig")},
  {NULL, "No Tooltip yet", N_("SetInitialAflatmin"), "keysig_set_aflatmin", N_("Set Ab Minor as Initial Keysig")},


  {NULL, "Sets the start point for a selection,\\nthe end point of the selection is unaltered", N_("SetMark"), "set_mark", N_("Set Mark")},
  {NULL, "Gets rid of the selection.", N_("UnsetMark"), "unset_mark", N_("Unset Mark")},
  {NULL, "Extends the selection to the current cursor position", N_("SetPoint"), "set_point", N_("Set Point")},
  {NULL, "Insert/delete begin slur on this note", N_("ToggleBeginSlur"), "toggle_begin_slur", N_("Begin Slur (Off/On)")},
  {NULL, "Insert/delete end slur on this note", N_("ToggleEndSlur"), "toggle_end_slur", N_("End Slur (Off/On)")},

  {NULL, "Marks/Unmarks the chord or note at the cursor as the start of a crescendo.", N_("ToggleStartCrescendo"), "toggle_start_crescendo", N_("Start Crescendo (Off/On)")},
  {NULL, "Marks/Unmarks the chord or note at the cursor as the end of a crescendo.", N_("ToggleEndCrescendo"), "toggle_end_crescendo", N_("End Crescendo (Off/On)")},
  {NULL, "Marks/Unmarks the chord or note at the cursor as the start of a diminuendo.", N_("ToggleStartDiminuendo"), "toggle_start_diminuendo", N_("Start Diminuendo (Off/On)")},
  {NULL, "Marks/Unmarks the chord or note at the cursor as the end of a diminuendo.", N_("ToggleEndDiminuendo"), "toggle_end_diminuendo", N_("End Diminuendo (Off/On)")},
  {NULL, "Makes the note at the cursor an appoggiatura grace note, if it is one, makes it normal", N_("ToggleGrace"), "toggle_grace", N_("Grace Note Off/On")},
  {NULL, "Makes the note at the cursor an acciaccatura grace note, if it is one, makes it normal", N_("ToggleAcciaccatura"), "toggle_acciaccatura", N_("Acciaccatura Off/On")},

  {NULL, "Give a cautionary accidental to the note at the cursor", N_("ForceCaution"), "force_cautionary", N_("Force Cautionary Accidental")},

  {NULL, "Changes the pitch of the note at the cursor to the cursor height", N_("ChangePitch"), "change_pitch", N_("Change Pitch")},

  {NULL, "Inserts the current music snippet, or if none is selected prompts for a number 1,2... for the snippet to insert.", N_("InsertRhythm"), "insert_rhythm_pattern", N_("Insert Snippet")},
  {NULL, "Make next snippet\\nthe current snippet.\\nNotes entered will follow the rhythmic pattern of this snippet", N_("NextRhythm"), "nextrhythm", N_("Next Snippet")},
  {NULL, "Appends a blank measure to every staff in this movement", N_("AppendMeasureAllStaffs"), "append_measure_score", N_("Append Measure All Staffs")},


  {NULL, "Execute the scheme code from the scripting window", N_("ExecuteScheme"), "execute_scheme", N_("Execute Scheme")},



  /* from view.c menu_entries[]  */
  {NULL, "Menu:\\nCreating, saving, loading, exporting, importing, typesetting and printing musical scores", N_("FileMenu"), NULL, "File"},
  {NULL, "Creating, saving places in musical scores", N_("Bookmarks"), NULL, "Bookmarks"},
  {NULL, "Different keyboard and MIDI entry modes", N_("ModeMenu"), NULL, "Mode"},
  {NULL, "Menu:\\nGeneral editing commands", N_("EditMenu"), NULL, "Edit"},
  {NULL, "Menu:\\nControl which tools are to be shown", N_("ViewMenu"), NULL, "View"},
  {NULL, "Menu:\\nHidden Items", N_("HiddenMenu"), NULL, "Hidden"},

  {NULL, "Menu:\\nCommands that manipulate Staffs and voices on staffs.\\nCommands that apply to the current staff.", N_("StaffMenu"), NULL, "Staffs/Voices"},
  {NULL, "Menu:\\nCommands for properties of the current staff.", N_("StaffPropertiesMenu"), NULL, "Staff Properties"},
  {NULL, "Menu:\\nCommands for properties of the current movement.", N_("MovementPropertiesMenu"), NULL, "Movement Properties"},
  {NULL, "Menu:\\nThings that manipulate Voices, that is separately stemmed and beamed parts that will be typeset on the same staff.\\nKeep them displayed on separate staffs for editing purposes at least.\\nThe typesetter will amalgamate them for you.\\nSee also commands for hiding rests and moving notes and rests to avoid collisions in the Notes/Rests menu", N_("Voices"), NULL, "Voices"},
  {NULL, "Menu:\\nCommands that apply to the current movement in your score.\\nMovements in a score could be songs in a song book etc\\nAny piece of continuous music.", N_("MovementMenu"), NULL, "Movements"},
  {NULL, "Menu:\\nInformation and help for Denemo", N_("HelpMenu"), NULL, "Help"},
  {NULL, "Menu:\\nChoose whether to get input audio (e.g. microphone) or MIDI controller (e.g. MIDI keyboard)\\nor just from the pc keyboard.", N_("InputMenu"), NULL, "Input"},
  {NULL, "Shifts the set of accidentals one step sharper", N_("SharpenEnharmonicSet"), "set_sharper", N_("Shift Accidentals Sharpwise")},
  {NULL, "Shifts the set of accidentals one step flatter", N_("FlattenEnharmonicSet"), "set_flatter", N_("Shift Accidentals Flatwise")},
  {NULL, "Music playback. Music is played between the playback start (green bar) and playback end (red bar).\\nThere are playback controls (See View->Playback Controls) which make it easy to set and re-set these and also to loop-play, choose the temperament to play in etc. Or use items in this menu to play the whole piece from the cursor to the end.", N_("PlaybackMenu"), NULL, "Playback"},



  {"GTK_STOCK_NEW", "Start a new musical score", N_("New"), "file_newwrapper", "Empty Score"},
  {"GTK_STOCK_NEW", "Start a new musical score for a named instrument/voice.", N_("NewScore"), "new_score_cb", N_("New")},
  {"GTK_STOCK_OPEN", "Menu:\\nOpen a previously edited score, or a source file for transcription", N_("OpenMenu"), NULL, N_("Open")},
  {"GTK_STOCK_OPEN", "Open a file containing a music score for editing", N_("Open"), "file_open_with_check", N_("Open")},
  {"GTK_STOCK_OPEN", "Import a Lilypond file", N_("ImportLilypond"), "file_import_lilypond_with_check", N_("Import Lilypond")},
  {"GTK_STOCK_OPEN", "Import a Midi file", N_("ImportMidi"), "file_import_midi_with_check", N_("Import Midi")},
  {"GTK_STOCK_OPEN", "Import a MusicXml file", N_("ImportMusicXml"), "file_import_musicxml_with_check", N_("Import MusicXml")},
  {"GTK_STOCK_OPEN", "Add staffs from a Denemo file", N_("AddStaffs"), "file_add_staffs", N_("Add Staffs")},
  {"GTK_STOCK_OPEN", "Add movements from a Denemo file", N_("AddMovements"), "file_add_movements", N_("Add Movement")},
  {"GTK_STOCK_PROPERTIES", "Change properties of this movement", N_("MovementProps"), "movement_props_dialog", N_("Change Properties")},
  {"GTK_STOCK_OPEN", "Open a file containing a music score for editing in a separate working area (tab)", N_("OpenNewWindow"), "openinnew", N_("Open In New")},
  {"GTK_STOCK_SAVE", "Save the score. The score is saved to disk in XML format.", N_("Save"), "file_savewrapper", N_("Save")},
  {"GTK_STOCK_SAVE_AS", "Save the score under a new name", N_("SaveAs"), "file_saveaswrapper", N_("Save As")},
  {"GTK_STOCK_SAVE_AS", "Save a copy of the score", N_("SaveCopy"), "file_copy_save", N_("Create Copy")},
  {"GTK_STOCK_OPEN", "Start a new score from a built-in template file", N_("OpenTemplate"), "system_template_open_with_check", N_("Open Template")},
  {"GTK_STOCK_OPEN", "Start a new score from a built-in example", N_("OpenExample"), "system_example_open_with_check", N_("Open Example")},
  {"GTK_STOCK_OPEN", "Start a new score from one of your own template files", N_("OpenMyTemplate"), "local_template_open_with_check", N_("Open Custom Template")},
  {"GTK_STOCK_SAVE_AS", "Save the score as a template for re-use as a starting point for new scores", N_("SaveTemplate"), "template_save", N_("Save Template")},
  {NULL, "Create working area (tab with an empty score in it)", N_("NewWindow"), "newview", N_("New Tab")},
  {NULL, "Insert a new movement before the current one", N_("InsertMovementBefore"), "insert_movement_before", N_("Insert Movement Before")},
  {NULL, "Insert a new movement after the current one", N_("InsertMovementAfter"), "insert_movement_after", N_("Insert Movement After")},
  {NULL, "Create a new movement, usign any default template", N_("NewMovement"), "append_new_movement", N_("New Movement")},
  {"GTK_STOCK_SAVE", "Menu:\\nSave the score\\nBesides saving for later you can save the score for use as a template for future works, or save under a new name etc\\nThe score is saved to disk in XML format.", N_("SaveMenu"), NULL, N_("Save")},
  {"GTK_STOCK_SAVE_AS", "Save Parts: each staff becomes a file in lilypond format", N_("SaveParts"), "file_savepartswrapper", N_("Save Parts")},
  {"GTK_STOCK_SAVE_AS", "Export the score as a lilypond file", N_("ExportMUDELA"), "export_mudela_action", N_("Export Lilypond")},
  {"GTK_STOCK_SAVE_AS", "Export the score as a PDF document file", N_("ExportPDF"), "export_pdf_action", N_("Export PDF")},
  {"GTK_STOCK_SAVE_AS", "Export the score as a PNG image file", N_("ExportPNG"), "export_png_action", N_("Export Score as PNG")},
  {"GTK_STOCK_SAVE_AS", "Export the score as a MIDI file", N_("ExportMIDI"), "export_midi_action", N_("Export MIDI")},
  {"GTK_STOCK_PRINT", "Menu:\\nCommands for typesetting and then printing the music.\\nA part, movement, full score or various reduced scores can be typeset.\\n See also the Score Layout view for further options.", N_("PrintMenu"), NULL, N_("Print")},
  {"GTK_STOCK_PRINT_PREVIEW", "Typesets the score\\nIf you have a score layout selected it will use that\\notherwise all movements staffs and lyrics are typeset by default.\\nBe patient! It takes time to create a beautifully laid out score.\\nOnce complete you can view and then send to your printer or to a file as a .pdf document.", N_("PrintView"), "show_print_view", N_("Print Preview")},
  {"GTK_STOCK_PRINT", "Displays selected music from score in your pdf viewer", N_("PrintSelection"), "printselection_cb", N_("Print Selection")},
  {"GTK_STOCK_PRINT_PREVIEW", "Displays a musical excerpt in your image viewer", N_("PrintExcerptPreview"), "printexcerptpreview_cb", N_("Export Selection as PNG")},
  {"GTK_STOCK_PRINT", "Typesets the current movement and opens a print dialog", N_("PrintMovement"), "printmovement_cb", N_("Print Movement")},
  {"GTK_STOCK_PRINT", "Typesets the score using LilyPond and opens a print dialog", N_("Print"), "printall_cb", N_("Print")},
  {"GTK_STOCK_PRINT", "Typesets the current part (the one containing the cursor).", N_("PrintPart"), "printpart_cb", N_("Print Part")},
  {"GTK_STOCK_CLOSE", "Close the current score. Other scores (tabs) will stay open", N_("Close"), "close_gui_with_check", N_("Close Score")},
  {"GTK_STOCK_QUIT", "Quit the Denemo program - closes tabs one at a time.", N_("Quit"), "closewrapper", N_("Quit")},
  {"GTK_STOCK_UNDO", "Undoes one (more) step of your edits to the current score.", N_("Undo"), "undowrapper", N_("Undo")},
  {"GTK_STOCK_REDO", "Redoes the next of the steps you have Undone", N_("Redo"), "redowrapper", N_("Redo")},
  {NULL, "Selecting stretches of notes", N_("Select"), NULL, N_("Select")},
  {NULL, "Extend the selection", N_("ExtendSelect"), NULL, N_("Extend Selection")},
  {"GTK_STOCK_COPY", "Copy the music selected to the Denemo clipboard", N_("Copy"), "copywrapper", N_("Copy")},
  {"GTK_STOCK_CUT", "Cut the music selected to the Denemo clipboard", N_("Cut"), "cutwrapper", N_("Cut")},
  {"GTK_STOCK_PASTE", "Paste the Denemo clipboard into the score where the cursor is positioned", N_("Paste"), "pastewrapper", N_("Paste")},
  {"GTK_STOCK_PASTE", "Paste LilyPond notes from the text clipboard\\nThis will import music written as LilyPond syntax\\nYou open the LilyPond file in a texteditor, copy the stretch of notes (control-c command in your texteditor usually) and then use this command.", "PasteClipboard", "paste_clipboard", N_("Paste LilyPond notes")},
  {"GTK_STOCK_PASTE", "Paste the text clipboard as a comment inserted at the cursor.", "PasteComment", "paste_comment", N_("Paste a Comment")},
  {"GTK_STOCK_PROPERTIES", "Change built-in properties of the current score. This will start up a dialog window", N_("ScoreProperties"), "score_properties_dialog", N_("Score Properties")},
  {NULL, "Save the selected music. Not sure if this is working", N_("SaveSelection"), "saveselwrapper", N_("Save Selection")},
  {"GTK_STOCK_PREFERENCES", "Set and save your preferences for how Denemo operates on startup.\\nAdvanced users can edit .denemo-XXXX/denemorc for missing ones", N_("Preferences"), "preferences_change", N_("Change Preferences")},
  {NULL, "Set actions to take in response to keypresses", N_("KeyBindings"), NULL, "Customize Commands, Shortcuts ..."},
  {"GTK_STOCK_SAVE", "Save the current commands and keyboard shortcuts as the default", N_("SaveAccels"), "save_default_keymap_file_wrapper", "Save Command Set"},
  {NULL, "View help, change and save keyboard shortcuts", "CommandManagement", "configure_keyboard_dialog", "Manage Command Set"},

  {NULL, "Swap this staff with the one higher up.\\nBe aware that if you have inserted directives to move a voice to another staff\\nthese may need re-making.", N_("SwapStaffs"), "swapstaffs", N_("Swap Staffs")},
  {NULL, "Split off the next voice as a separate staff", N_("SplitVoices"), "splitstaffs", N_("Split Voices")},
  {NULL, "Merge this staff as a voice on the previous staff", N_("JoinVoices"), "joinstaffs", N_("Join Voices")},
  {NULL, "Swap this movement with the one before", N_("SwapMovements"), "swapmovements", N_("Swap Movements")},
  {NULL, "Go to the higher numbered voice on staff, extending selection if any", N_("VoiceUp"), "voiceup", N_("Selection Voice Up")},
  {NULL, "Go to the lower numbered voice on this staff, extending selection if any", N_("VoiceDown"), "voicedown", N_("Selection Voice Down")},

  {NULL, "Go to the higher numbered voice on staff without altering selection", N_("MoveToVoiceUp"), "movetovoiceup", N_("Move to Voice Up")},
  {NULL, "Go to the lower numbered voice on this staff without altering selection", N_("MoveToVoiceDown"), "movetovoicedown", N_("Move to Voice Down")},



  {NULL, "Inserts a new staff before the current staff", N_("AddBefore"), "staff_new_before", N_("Add Staff Before")},
  {NULL, "Inserts/Adds a new staff after the current staff", N_("AddAfter"), "staff_new_after", N_("Add Staff After")},
  {NULL, "Inserts a new staff at the top of the score", N_("AddInitial"), "staff_new_initial", N_("Add Initial Staff")},
  {NULL, "Inserts a new staff at the end of the score", N_("AddLast"), "staff_new_last", N_("Add Last Staff")},
  {NULL, "Deletes the staff before the current staff", N_("DeleteBefore"), "delete_staff_before", N_("Delete Staff Before")},
  {NULL, "Deletes the current staff", N_("DeleteStaff"), "delete_staff_current", N_("Delete Current Staff")},
  {NULL, "Deletes the staff after the current staff", N_("DeleteAfter"), "delete_staff_after", N_("Delete Staff After")},
  {NULL, "Adds a new voice (part), to the current staff. It is tricky to switch between the voices. Suggest to use merge staffs", "AddVoice", "staff_new_voice", N_("Add Voice")},
  {"GTK_STOCK_PROPERTIES", "Change the built-in properties of the current staff", N_("StaffProperties"), "staff_properties_change_cb", N_("Built-in Staff Properties")},
  {NULL, "Insert", N_("InsertMenu"), NULL, NULL},
  {NULL, "Menu:\\nClef", N_("Clef"), NULL, NULL},
  {NULL, "Change the initial clef of the current staff", N_("InitialClef"), "clef_change_initial", N_("Initial Clef")},
  {NULL, "Insert/Edit a change of clef at the cursor", N_("InsertClef"), "clef_change_insert", N_("Clef Change")},


  {NULL, "Menu:\\nSet the key signature or insert a change of key signature\\nSharpen or flatten a key signature or\\nhide it on printing", N_("Key"), NULL, N_("Keys")},
  {NULL, "Set the initial key signature of the current staff", N_("InitialKey"), "key_change_initial", N_("Initial Key")},
  {NULL, "Insert/Edit a key change at the cursor position", N_("InsertKey"), "key_change_insert", N_("Key Signature Change")},
  {NULL, "Menu:\\nSet the time signature, insert or edit a change of timesignature,\\nhide a time signature on printing.\\nControl whether to use numerical or traditional styles.", N_("TimeSig"), NULL, N_("Time Signatures")},
  {NULL, "Set the initial time signature of the current staff", N_("InitialTimeSig"), "timesig_change_initial", N_("Inital Time Signature")},
  {NULL, "Edit/Insert a time signature change for the current measure", N_("InsertTimeSig"), "timesig_change_insert", N_("Time Signature Change")},
  {NULL, "Change the type of notehead for the current note", N_("ChangeNotehead"), "set_notehead", N_("Set Notehead")},
  {NULL, "Inserts a stem neutral object. After this automatic stem directions are active. You can click on this tag and use Sharpen/StemUp etc commands to change stem direction", N_("InsertStem"), "stem_directive_insert", N_("Auto Stemming")},
  {NULL, "Add a verse of lyrics", "AddVerse", "add_verse", "Add Lyric Verse"},
  {NULL, "Deletes current verse of lyrics from current voice", "DeleteVerse", "delete_verse", "Delete Verse"},
  {NULL, "Add a bass figure to the current note. Use | sign to split the duration of a note so as to have multiple figures on one note. See Lilypond docs for other notation", N_("EditFiguredBass"), "figure_insert", "Insert/Edit Figured Bass"},
  {NULL, "Delete the figured bass on the current staff", N_("DeleteFiguredBass"), "delete_figured_bass", "Delete Figures"},
  {NULL, "Delete the chord symbols on the current staff", N_("DeleteChordSymbols"), "delete_fakechords", "Delete Chord Symbols"},
  {NULL, "Hide the figured bass on the current staff on printing", N_("HideFiguredBass"), "hide_figured_bass", "Hide Figures (Print)"},
  {NULL, "Show the figured bass on the current staff on printing", N_("ShowFiguredBass"), "show_figured_bass", "Show Figures (Print)"},
  {NULL, "Allows chord symbols to be added to the current note. E.G.cis:dim7 for c-sharp diminished 7th. See Lilypond docs for notation", N_("EditChords"), "fakechord_insert", N_("Edit Chord Symbols")},

  {NULL, "Edit the object at the cursor.", N_("EditObject"), "edit_object", N_("Edit Object")},

  {NULL, "Edit any directives attached to chord/note at cursor.", N_("EditDirective"), "edit_object_directive", N_("Edit Directives")},
  {NULL, "Edit any directives attached to staff.", N_("EditStaffDirective"), "edit_staff_directive", N_("Edit Staff Directives")},
  {NULL, "Edit any directives attached to voice.", N_("EditVoiceDirective"), "edit_voice_directive", N_("Edit Voice Directives")},
  {NULL, "Edit any directives attached to score.", N_("EditScoreDirective"), "edit_score_directive", N_("Edit Score Directives")},
  {NULL, "Edit any directives attached to movement.", N_("EditMovementDirective"), "edit_movement_directive", N_("Edit Movement Directives")},


  {NULL, "Edit any directives attached to clef.", N_("EditClefDirective"), "edit_clef_directive", N_("Edit Clef Directives")},
  {NULL, "Edit any directives attached to time signature.", N_("EditTimesigDirective"), "edit_timesig_directive", N_("Edit Time Signature Directives")},
  {NULL, "Edit any directives attached to key signature.", N_("EditKeysigDirective"), "edit_keysig_directive", N_("Edit Key Signature Directives")},

  {NULL, "Delete a directive attached to chord/note at cursor.", N_("DeleteDirective"), "delete_chord_or_note_directive", N_("Delete a Directive")},


  {NULL, "Attach or edit attached LilyPond text to the note at the cursor. This can be used for guitar fingerings, cautionary accidentals and much more. See LilyPond documentation.", N_("AttachLilyToNote"), "note_directive", N_("Attach Lilypond to Note")},
  {NULL, "Attach or edit attached LilyPond text to the chord at the cursor. This can be used for attaching and placing text and much more. See LilyPond documentation.", N_("AttachLilyToChord"), "chord_directive", N_("Attach Lilypond to Chord")},
  {NULL, "Moving the cursor within the current movement, setting bookmarks, seeking things in the score", N_("NavigationMenu"), NULL, "Navigation"},
  {NULL, "Opens a dialog for going to a numbered measure", N_("GoToMeasure"), "tomeasurenum", "Go to Measure"},
  {"GTK_STOCK_GOTO_FIRST", "Cursor to start of staff/voice, extending selection if any", N_("GoToBeginning"), "tohome", "Go to Beginning"},
  {"GTK_STOCK_GOTO_LAST", "Cursor to end of staff/voice, extending selection if any", N_("GoToEnd"), "toend", "Go to End"},

  {"GTK_STOCK_GOTO_FIRST", "Cursor to start of staff/voice, without extending selection if any", N_("MoveToBeginning"), "movetostart", "Move to Staff/Voice Beginning"},
  {"GTK_STOCK_GOTO_LAST", "Cursor to end of staff/voice, without extending selection if any", N_("MoveToEnd"), "movetoend", "Move to Staff/Voice End"},




  {NULL, "Go to the next movement", N_("NextMovement"), "next_movement", N_("Next Movement")},
  {NULL, "Go to the previous movement", N_("PreviousMovement"), "prev_movement", N_("Previous Movement")},
  {NULL, "Delete the current movement", N_("DeleteMovement"), "delete_movement", N_("Delete Movement")},


  {"GTK_STOCK_MEDIA_PLAY", "Playback from start marker to end marker (Set these markers in the playback controls)", N_("Play"), "ext_midi_playback", N_("Play")},
  {"GTK_STOCK_MEDIA_STOP", "Stop Playback", N_("Stop"), "stop_midi_playback", N_("Stop")},


  {"GTK_STOCK_PROPERTIES", "Allows you to specify properties used in playing back (midi)", N_("PlaybackProperties"), "playback_properties_change", "Playback Properties"},

  {NULL, "Opens a browser on the user manual", N_("Help"), "browse_manual", N_("Browse Manual")},
  {NULL, "Gives the version number etc of this program", N_("About"), "about", N_("About")},
  {NULL, "Gives a list of shortcuts currently available. Explains how to set them too.", N_("Shortcuts"), "display_shortcuts", N_("Shortcuts")},


  {NULL, "Allows choosing extra commands/menu items from disk", N_("MoreMenu"), NULL, N_("More")},
  {NULL, "Allows choosing standard extra commands/menu items", N_("MoreCommands"), "morecommands", N_("More Commands")},
  {NULL, "Allows choosing extra commands/menu items from your own collection of extras", N_("MyCommands"), "mycommands", N_("My Commands")},
  {NULL, "Refreshes the set of commands available from Denemo.org.\\nUse More Commands after this has finished", N_("FetchCommands"), "fetchcommands", N_("Update Commands from Internet")},





  {"GTK_STOCK_OPEN", "Open previously used files", N_("OpenRecent"), NULL, N_("Open Recent")},
  {"GTK_STOCK_OPEN", "Menu:\\nImport the supported file formats", N_("Import"), NULL, N_("Import File")},
  {NULL, "Menu:\\nExport the music to another file format.\\nThese file formats will not support all the features of the score.", N_("Export"), NULL, N_("Export As")},
  {NULL, "Toggle between current mode and edit mode", N_("ToggleEdit"), "toggle_edit_mode", N_("Toggle Edit Mode")},
  {NULL, "Toggle between note entry and rest entry", N_("ToggleRest"), "toggle_rest_mode", N_("Toggle Rest Mode")},
  {NULL, "Toggle audible feedback on/off", N_("ToggleRhythm"), "toggle_rhythm_mode", N_("Toggle Audible Feedback")},
  {NULL, "Clear the list of pitches that overlay the notes", N_("ClearOverlay"), "clear_overlay", N_("Clear Overlay")},
  {NULL, "Copy selection as music snippet or rhythm pattern for notes to follow as they are entered", N_("CreateRhythm"), "create_rhythm_cb", N_("Create Snippet")},
  {NULL, "Delete the selected music snippet/rhythm pattern", N_("DeleteRhythm"), "delete_rhythm_cb", N_("Delete Snippet")},
  {NULL, "Menu:\\nMoving the cursor and inserting notes or rests there", N_("ClassicModeNote"), NULL, N_("Classic Mode")},
  {NULL, "Menu:\\nMoving the cursor to the nearest ...", N_("SelectNote"), NULL, N_("Select Note")},
  {NULL, "Menu:\\nCommands for notes: inserting, deleting, etc.", N_("InsertModeNote"), NULL, N_("Insert")},
  {NULL, "Menu:\\nCommands to control the stem up/down", N_("StemControl"), NULL, N_("Stem Direction")},
  {NULL, "Menu:\\nDifferent types of notehead", N_("NoteheadControl"), NULL, N_("Notehead Types")},
  {NULL, "Menu:\\nCreating Tied Notes\\nDo not confuse with slurs!!", N_("TiedNotes"), NULL, N_("Tied Notes")},
  {NULL, "Menu:\\nEffects that are only for the Denemo display,\\nnot affecting the printed page.", N_("DisplayEffects"), NULL, N_("Display Effects")},

  {NULL, "Menu:\\nArticulations", N_("Articulations"), NULL, N_("Articulations")},
  {NULL, "Menu:\\nSlurs\\nDo not confuse with ties which in print can look the same\\nSlurs have little ticks at the end in the Denemo display.", N_("Slurs"), NULL, N_("Slurs")},
  {NULL, "Menu:\\nGrace Notes", N_("GraceNotes"), NULL, N_("Grace Notes")},
  {NULL, "Menu:\\nCommands that affect the entire piece of music across all movements.", N_("Score"), NULL, N_("Score")},
  {NULL, "Menu:\\nInstruments", N_("Instruments"), NULL, N_("Instruments")},
  {NULL, "Menu:\\nMarkings to be applied to the note at the cursor.\\nIncludes articulations, string numbers, chord symbols, arbitrary text ...", N_("Markings"), NULL, N_("Markings")},
  {NULL, "Menu:\\nText and symbol Markings", N_("TextMarks"), NULL, N_("Text/Symbol")},
  {NULL, "Menu:\\nMarkings on scores for Instruments", "Strings", NULL, N_("Fingerings etc for Instruments")},

  {NULL, "Menu:\\nInserting the note ...", N_("InsertNote"), NULL, "Note Insertion"},
  {NULL, "Menu:\\nLyrics consist of syllables which are placed under or above the notes of the voice they are attached to\\nYou can use the hyphen - and underscore _ to affect the placement\\nas well as slurs placed over notes. Spaces, tabs or new lines can serve to separate the syllables, it makes no difference which.", N_("Lyrics"), NULL, N_("Lyrics")},
  {NULL, "Menu:\\nDirectives tell Denemo to do something special with the object they are attached to\\nor at the point in the music they are placed\\nThis can be directing the typesetter to do something special\\nor issuing MIDI instructions to alter the playback\\nor even take an action, such as linking to source manuscript when clicked", N_("Directives"), NULL, N_("Directives")},
  {NULL, "Menu:\\nMoving the Denemo cursor around the piece", N_("Navigation"), NULL, N_("Navigation")},
  {NULL, "Menu:\\nEntering notes", N_("NoteEntry"), NULL, N_("Insert Note")},
  {NULL, "Menu:\\nMarks placed on notes and chords", N_("Articulation"), NULL, N_("Articulation")},
  {NULL, "Menu:\\nEditing", N_("Edit"), NULL, N_("Edit")},
  {NULL, "Menu:\\nManipulating measures\\nCommands that apply to the current measure", N_("Measure"), NULL, N_("Measure")},
  {NULL, "Menu:\\nCommands for staffs", N_("Staff"), NULL, N_("Staff")},

  {NULL, "Menu:\\nPlaying the music via the computer's soundcard.\\nAll or part of the piece can be played controlled by the green and red markers (start and stop playing)\\nUse the Playback Controls (from the View menu) to set/reset these and to loop play while editing the score", N_("Playback"), NULL, N_("Playback")},
  {NULL, "Menu:\\nChanging the prevailing duration or rhythm pattern", N_("SelectDuration"), NULL, N_("Select Duration")},
  {NULL, "Menu:\\nAppending, Changing, and deleting notes", N_("EditModeNote"), NULL, N_("Append/Edit")},
  {NULL, "Menu:\\nAppending or Editing notes", N_("EditNote"), NULL, N_("Append/Edit Note")},
  {NULL, "Menu:\\nAppending or Editing durations", N_("EditDuration"), NULL, N_("Append/Insert Duration")},
  {NULL, "Menu:\\nMoving the cursor", N_("Cursor"), NULL, N_("Cursor")},
  {NULL, "Menu:\\nMoving the cursor to note positions", N_("CursorToNote"), NULL, N_("Cursor to Note")},

  {NULL, "Menu:\\nInsert/change clef, set initial clef, hide a clef on printing,\\nor display in a different clef", N_("ClefMenu"), NULL, N_("Clefs")},
  {NULL, "Menu:\\nCommands for creating chords. Note that chord symbols and figured bass are under markings on notes.\\nSee Notes/Rests menu", N_("ChordMenu"), NULL, "Chords"},
  {NULL, "Menu:\\nAdding notes to a note or chord at the cursor to make chords", N_("ChordNoteMenu"), NULL, "Add Note"},

  {NULL, "Menu:\\nAdding Chord Symbols over music", N_("ChordSymbols"), NULL, "Chords Symbols"},
  {NULL, "Menu:\\nAdding Figured Bass Figures", N_("FiguredBass"), NULL, "Figured Bass"},


  {NULL, "Menu:\\nMeasures: adding, deleting, navigating etc", N_("MeasureMenu"), NULL, N_("Measures")},
  {NULL, "Menu:\\nInserting notes, measures, staffs, keysignatures etc", N_("Insert"), NULL, N_("Insert")},
  {NULL, "Menu:\\nInsert a new staff postioned relative to current staff", N_("InsertStaff"), NULL, N_("Add Staff")},
  {NULL, "Menu:\\nInsert a new movement positioned relative to current movement", N_("InsertMovement"), NULL, N_("Insert Movement")},
  {NULL, "Menu:\\nInserting notes of a given duration at cursor note height", N_("InsertDuration"), NULL, N_("Insert at Cursor")},
  {NULL, "Menu:\\nChanging properties of notes, measures, staffs, keysigs etc", N_("Change"), NULL, N_("Change")},
  {NULL, "Menu:\\nModeless actions on notes/rests", N_("ModelessNote"), NULL, N_("Notes/Durations")},
  {NULL, "Menu:\\nCommands for notes/rests", N_("NotesRests"), NULL, N_("Notes/Rests")},
  {NULL, "Menu:\\nModeless entry of rests", N_("RestEntry"), NULL, N_("Rest Insertion")},
  {NULL, "Menu:\\nEducational Games", N_("Educational"), NULL, N_("Educational")},

  {NULL, "Menu:\\nEditing directives", N_("EditDirectivesMenu"), NULL, N_("Edit Directive(s)")},
  {NULL, "Menu:\\nChanging the note at the cursor to the nearest ...", N_("ChangeNote"), NULL, N_("Change Note")},
  {NULL, "Menu:\\nChanges the duration of the current note", N_("ChangeDuration"), NULL, N_("Edit Duration")},
  {NULL, "Menu:\\nChanges the duration of the current rest", N_("ChangeRest"), NULL, N_("Change Rest")},
  {NULL, "Menu:\\nDynamics, staccato, slurs, ties and other expressive marks", N_("ExpressionMarks"), NULL, "Expression Marks"},
  //  {NULL, "Markings above and below the music", N_("Markings"), NULL, "Markings", N_("Markings")},
  {NULL, "Dynamic markings", N_("Dynamics"), NULL, "Dynamics"},

  {NULL, "Menu:\\ngrace notes etc", N_("Ornaments"), NULL, N_("Ornaments")},
  {NULL, "Lyrics, chord symbols, figured basses etc", N_("Other"), NULL, N_("Other")},
  {NULL, "Less used actions", N_("Others"), NULL, N_("Others")},
  {NULL, "Customized LilyPond inserts. Store often-used inserts here labelled with what they do", N_("Favorites"), NULL, N_("Favorites")},
  {NULL, "Menu:\\nAdd a custom LilyPond insert to favorites menu", N_("AddFavorite"), NULL, N_("Add Favorite")},
  {NULL, "Menu:\\nEntering triplets and other tuplets", N_("Tuplets"), NULL, N_("Tuplets")},
  {NULL, "Menu:\\nDeleting notes, measures, staffs, key signatures as well as meta-objects such as imported MIDI or audio recordings.", N_("Delete"), NULL, N_("Delete")},
  {NULL, "Menu:\\nBeaming - controlling which notes are beamed together", N_("Beaming"), NULL, N_("Beaming")},
  {NULL, "Menu:\\nFor making directives apply only to one score layout or be omitted from one score layout", N_("Conditional Directives"), NULL, N_("Conditional Directives")},
  {NULL, "Menu:\\nFor issuing MIDI instructions during playback at the time indicated by the current cursor position.", N_("MIDI"), NULL, N_("MIDI")},
  {NULL, "Menu:\\nChoose between different ways of entering notes from the computer keyboard.", N_("Keyboard"), NULL, N_("PC Keyboard")},
  {NULL, "Menu:\\nChoose between different ways of using the mouse.", N_("Mouse"), NULL, N_("Mouse")},
  {NULL, "Menu:\\nTransposing the score when typesetting.", N_("PrintTranspositions"), NULL, N_("Print Transposed")},
  {NULL, "Menu:\\nInserting markers to start/stop transposition when typesetting. The markers affect only the music in the staff/voice they are placed in.", N_("Print Transpositions"), NULL, N_("Start/Stop Transposing")},
  {NULL, "Menu:\\nDifferent sorts of barline (repeat barlines, double bars, end of movement ...)", N_("Barlines"), NULL, N_("Inserting Barlines")},



  {NULL, "Menu:\\nMoving the Denemo cursor forwards/backwards to find useful places in the score", N_("Seek"), NULL, N_("Seek")},
  {NULL, "Menu:\\nVarious titling schemes, (main title, movement titles, composer etc) and adding table of contents, critical commentary", N_("Titles"), NULL, N_("Titles")},
  {NULL, "Menu:\\nCommands to affect the layout of the typeset score.", N_("PrintLayout"), NULL, N_("Print Layout")},
  {NULL, "Menu:\\nCommands to embed images taken from original manuscripts etc into the score (one for each measure)", N_("CaptureScore"), NULL, N_("Capture Score")},
  {NULL, "Menu:\\nCreating an Ossia staff, which appears with alternative interpretations for a bar or two on typesetting", N_("Ossia"), NULL, N_("Ossia")},
  {NULL, "Menu:\\nCommands for setting how often bar numbers appear in the typeset version of this staff", N_("Numbering"), NULL, N_("Bar Numbering")},
  {NULL, "Menu:\\nVarious sorts of brace linking the staffs together. These may be nested, see the Score Layout view for a display of the staff groups created.", N_("StaffGroupings"), NULL, N_("Staff Groupings (Braces)")},
  {NULL, "Menu:\\nAdding notes to the chord at the cursor at various intervals above base note", N_("AddAboveBase"), NULL, N_("Add Note Above Base of Chord")},
  {NULL, "Menu:\\nAdding notes to the chord at the cursor at various intervals below the top-most note", N_("AddBelowTop"), NULL, N_("Add Note Below Top of Chord")},
  {NULL, "Menu:\\nCommands to include other durations in a chord (instead of polyphony - this is specialized stuff!) The note at the cursor height is altered.", N_("ChangePrintDuration"), NULL, N_("Change Duration of Note")},
  {NULL, "Menu:\\nFingerings (numbers) to typeset near to notes", N_("Fingerings"), NULL, N_("Fingerings")},
  {NULL, "Menu:\\nAlters the note/chord at the cursor or the selection transposing it by the interval chosen. This alters the music in the score - see other transpose options for transposing the printed output, leaving the music untouched.", N_("Transpose"), NULL, N_("Transpose Music")},
  {NULL, "Menu:\\nMark the directive at the cursor as applying to just the current score layout, or to exclude the current layout. Use this for example to exclude redundant first and second time markings in parts which don't need them.", N_("Conditional-Directives"), NULL, N_("Make a Directive Conditional")},
  {NULL, "Menu:\\nTo insert random notes at cursor", N_("SingleRandomNote"), NULL, N_("Single Random Note")},
  {NULL, "Menu:\\nCreating rhythms from a sequence of characters.", N_("GenerateRhythmFromString"), NULL, N_("Generate Rhythm from String")},
  {NULL, "Menu:\\nMethods of generating music of guided randomness", N_("NotationMagick"), NULL, N_("Notation Magick")},
  {NULL, "Menu:\\nZooming the Denemo display. Usually Control and mouse wheel are used for this.", N_("Zoom"), NULL, N_("Display Zoom")},
  {NULL, "Menu:\\nGames for testing your musical ear", N_("Aural Training"), NULL, N_("Aural Training")},
  {NULL, "Menu:\\nGames for testing your music reading ability. Notice there is (elsewhere) the Checking Pitches feature which lets you test your ability to play via Midi Controller.", N_("Note-Reading"), NULL, N_("Note Reading")},
  {NULL, "Menu:\\nCommands to place titles etc at the start of the music (and each movement if needed). Do not mix with Book Titles.", N_("Simple Titles"), NULL, N_("Simple Titling")},
  {NULL, "Menu:\\nCommands to create a title page, table of contents, commentaries, appendices etc.", N_("Book Titles"), NULL, N_("Book Titling")},
  {NULL, "Menu:\\nCommands to manipulate the palettes of commands", N_("Palettes"), NULL, N_("Palettes")},
  {NULL, "Menu:\\nCommands to tell the typesetter to start or stop a particular effect. Use these around passages that are to be treated differently, or over which a marking should be placed.", N_("Spanning"), NULL, N_("Spanning")},
  {NULL, "Menu:\\nInstructions for the typesetter to do something at the cursor position.", N_("Typesetter"), NULL, N_("Typesetter")},
  {NULL, "Menu:\\nInstructions for the typesetter to combine two voices in various ways.", N_("Part-Combining"), NULL, N_("Part-Combining")},

  {NULL, "Menu:\\nCommands for working with the musical object hierarchy.", N_("ObjectMenu"), NULL, N_("Object Menu")},
  {NULL, "Menu:\\nThe top level commands for operating Denemo.", N_("MainMenu"), NULL, N_("Main Menu")},



};
