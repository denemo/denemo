/*CursorLeft cursorleft*/
SCM scheme_CursorLeft(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "CursorLeft", scheme_CursorLeft);
/*MoveCursorLeft movecursorleft*/
SCM scheme_MoveCursorLeft(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoveCursorLeft", scheme_MoveCursorLeft);
/*CursorDown cursordown*/
SCM scheme_CursorDown(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "CursorDown", scheme_CursorDown);
/*CursorUp cursorup*/
SCM scheme_CursorUp(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "CursorUp", scheme_CursorUp);
/*CursorRight cursorright*/
SCM scheme_CursorRight(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "CursorRight", scheme_CursorRight);
/*MoveCursorRight movecursorright*/
SCM scheme_MoveCursorRight(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoveCursorRight", scheme_MoveCursorRight);
/*GoToMark goto_mark*/
SCM scheme_GoToMark(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "GoToMark", scheme_GoToMark);
/*SwapPointAndMark swap_point_and_mark*/
SCM scheme_SwapPointAndMark(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SwapPointAndMark", scheme_SwapPointAndMark);
/*GoToSelectionStart goto_selection_start*/
SCM scheme_GoToSelectionStart(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "GoToSelectionStart", scheme_GoToSelectionStart);
/*PushPosition PushPosition*/
SCM scheme_PushPosition(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "PushPosition", scheme_PushPosition);
/*PopPosition PopPosition*/
SCM scheme_PopPosition(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "PopPosition", scheme_PopPosition);
/*PopPushPosition PopPushPosition*/
SCM scheme_PopPushPosition(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "PopPushPosition", scheme_PopPushPosition);
/*ToggleReduceToDrawingArea ToggleReduceToDrawingArea*/
SCM scheme_ToggleReduceToDrawingArea(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ToggleReduceToDrawingArea", scheme_ToggleReduceToDrawingArea);
/*StaffUp staffup*/
SCM scheme_StaffUp(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "StaffUp", scheme_StaffUp);
/*StaffDown staffdown*/
SCM scheme_StaffDown(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "StaffDown", scheme_StaffDown);
/*MoveToStaffUp movetostaffup*/
SCM scheme_MoveToStaffUp(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoveToStaffUp", scheme_MoveToStaffUp);
/*MoveToStaffDown movetostaffdown*/
SCM scheme_MoveToStaffDown(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoveToStaffDown", scheme_MoveToStaffDown);
/*MeasureLeft measureleft*/
SCM scheme_MeasureLeft(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MeasureLeft", scheme_MeasureLeft);
/*MeasureRight measureright*/
SCM scheme_MeasureRight(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MeasureRight", scheme_MeasureRight);
/*MoveToMeasureLeft movetomeasureleft*/
SCM scheme_MoveToMeasureLeft(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoveToMeasureLeft", scheme_MoveToMeasureLeft);
/*MoveToMeasureRight movetomeasureright*/
SCM scheme_MoveToMeasureRight(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoveToMeasureRight", scheme_MoveToMeasureRight);
/*A go_to_A_key*/
SCM scheme_A(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "A", scheme_A);
/*B go_to_B_key*/
SCM scheme_B(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "B", scheme_B);
/*C go_to_C_key*/
SCM scheme_C(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "C", scheme_C);
/*D go_to_D_key*/
SCM scheme_D(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "D", scheme_D);
/*E go_to_E_key*/
SCM scheme_E(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "E", scheme_E);
/*F go_to_F_key*/
SCM scheme_F(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "F", scheme_F);
/*G go_to_G_key*/
SCM scheme_G(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "G", scheme_G);
/*OctaveUp octave_up_key*/
SCM scheme_OctaveUp(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "OctaveUp", scheme_OctaveUp);
/*OctaveDown octave_down_key*/
SCM scheme_OctaveDown(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "OctaveDown", scheme_OctaveDown);
/*WholeNote insert_chord_0key*/
SCM scheme_WholeNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "WholeNote", scheme_WholeNote);
/*HalfNote insert_chord_1key*/
SCM scheme_HalfNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "HalfNote", scheme_HalfNote);
/*QuarterNote insert_chord_2key*/
SCM scheme_QuarterNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "QuarterNote", scheme_QuarterNote);
/*EighthNote insert_chord_3key*/
SCM scheme_EighthNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EighthNote", scheme_EighthNote);
/*SixteenthNote insert_chord_4key*/
SCM scheme_SixteenthNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SixteenthNote", scheme_SixteenthNote);
/*ThirtysecondNote insert_chord_5key*/
SCM scheme_ThirtysecondNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ThirtysecondNote", scheme_ThirtysecondNote);
/*SixtyfourthNote insert_chord_6key*/
SCM scheme_SixtyfourthNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SixtyfourthNote", scheme_SixtyfourthNote);
/*OneHundredTwentyEighthNote insert_chord_7key*/
SCM scheme_OneHundredTwentyEighthNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "OneHundredTwentyEighthNote", scheme_OneHundredTwentyEighthNote);
/*TwoHundredFiftySixthNote insert_chord_8key*/
SCM scheme_TwoHundredFiftySixthNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "TwoHundredFiftySixthNote", scheme_TwoHundredFiftySixthNote);
/*InsertWholeRest insert_rest_0key*/
SCM scheme_InsertWholeRest(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertWholeRest", scheme_InsertWholeRest);
/*InsertHalfRest insert_rest_1key*/
SCM scheme_InsertHalfRest(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertHalfRest", scheme_InsertHalfRest);
/*InsertQuarterRest insert_rest_2key*/
SCM scheme_InsertQuarterRest(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertQuarterRest", scheme_InsertQuarterRest);
/*InsertEighthRest insert_rest_3key*/
SCM scheme_InsertEighthRest(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertEighthRest", scheme_InsertEighthRest);
/*InsertSixteenthRest insert_rest_4key*/
SCM scheme_InsertSixteenthRest(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertSixteenthRest", scheme_InsertSixteenthRest);
/*InsertThirtysecondRest insert_rest_5key*/
SCM scheme_InsertThirtysecondRest(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertThirtysecondRest", scheme_InsertThirtysecondRest);
/*InsertSixtyfourthRest insert_rest_6key*/
SCM scheme_InsertSixtyfourthRest(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertSixtyfourthRest", scheme_InsertSixtyfourthRest);
/*InsertBlankWholeNote insert_blankchord_0key*/
SCM scheme_InsertBlankWholeNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertBlankWholeNote", scheme_InsertBlankWholeNote);
/*InsertBlankHalfNote insert_blankchord_1key*/
SCM scheme_InsertBlankHalfNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertBlankHalfNote", scheme_InsertBlankHalfNote);
/*InsertBlankQuarterNote insert_blankchord_2key*/
SCM scheme_InsertBlankQuarterNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertBlankQuarterNote", scheme_InsertBlankQuarterNote);
/*InsertBlankEighthNote insert_blankchord_3key*/
SCM scheme_InsertBlankEighthNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertBlankEighthNote", scheme_InsertBlankEighthNote);
/*InsertBlankSixteenthNote insert_blankchord_4key*/
SCM scheme_InsertBlankSixteenthNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertBlankSixteenthNote", scheme_InsertBlankSixteenthNote);
/*InsertBlankThirtysecondNote insert_blankchord_5key*/
SCM scheme_InsertBlankThirtysecondNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertBlankThirtysecondNote", scheme_InsertBlankThirtysecondNote);
/*InsertBlankSixtyfourthNote insert_blankchord_6key*/
SCM scheme_InsertBlankSixtyfourthNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertBlankSixtyfourthNote", scheme_InsertBlankSixtyfourthNote);
/*InsertBlankOneHundredTwentyEighthNote insert_blankchord_7key*/
SCM scheme_InsertBlankOneHundredTwentyEighthNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertBlankOneHundredTwentyEighthNote", scheme_InsertBlankOneHundredTwentyEighthNote);
/*InsertBlankTwoHundredFiftySixthNote insert_blankchord_8key*/
SCM scheme_InsertBlankTwoHundredFiftySixthNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertBlankTwoHundredFiftySixthNote", scheme_InsertBlankTwoHundredFiftySixthNote);
/*InsertDuplet duplet_insert*/
SCM scheme_InsertDuplet(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertDuplet", scheme_InsertDuplet);
/*InsertTriplet triplet_insert*/
SCM scheme_InsertTriplet(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertTriplet", scheme_InsertTriplet);
/*StartTriplet triplet_start*/
SCM scheme_StartTriplet(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "StartTriplet", scheme_StartTriplet);
/*EndTuplet tuplet_end*/
SCM scheme_EndTuplet(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EndTuplet", scheme_EndTuplet);
/*InsertQuadtuplet insert_quadtuplet*/
SCM scheme_InsertQuadtuplet(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertQuadtuplet", scheme_InsertQuadtuplet);
/*InsertQuintuplet quintuplet_insert*/
SCM scheme_InsertQuintuplet(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertQuintuplet", scheme_InsertQuintuplet);
/*InsertSextuplet sextuplet_insert*/
SCM scheme_InsertSextuplet(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertSextuplet", scheme_InsertSextuplet);
/*InsertSeptuplet septuplet_insert*/
SCM scheme_InsertSeptuplet(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertSeptuplet", scheme_InsertSeptuplet);
/*AddNoteToChord add_tone_key*/
SCM scheme_AddNoteToChord(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddNoteToChord", scheme_AddNoteToChord);
/*RemoveNoteFromChord remove_tone_key*/
SCM scheme_RemoveNoteFromChord(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "RemoveNoteFromChord", scheme_RemoveNoteFromChord);
/*Sharpen sharpen_key*/
SCM scheme_Sharpen(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Sharpen", scheme_Sharpen);
/*Flatten flatten_key*/
SCM scheme_Flatten(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Flatten", scheme_Flatten);
/*PendingSharpen pending_sharpen*/
SCM scheme_PendingSharpen(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "PendingSharpen", scheme_PendingSharpen);
/*PendingFlatten pending_flatten*/
SCM scheme_PendingFlatten(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "PendingFlatten", scheme_PendingFlatten);
/*StemUp stem_up*/
SCM scheme_StemUp(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "StemUp", scheme_StemUp);
/*StemDown stem_down*/
SCM scheme_StemDown(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "StemDown", scheme_StemDown);
/*AddDot add_dot_key*/
SCM scheme_AddDot(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddDot", scheme_AddDot);
/*RemoveDot remove_dot_key*/
SCM scheme_RemoveDot(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "RemoveDot", scheme_RemoveDot);
/*InsertTiedNote tie_notes_key*/
SCM scheme_InsertTiedNote(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertTiedNote", scheme_InsertTiedNote);
/*ToggleTie toggle_tie*/
SCM scheme_ToggleTie(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ToggleTie", scheme_ToggleTie);
/*DeleteObject deleteobject*/
SCM scheme_DeleteObject(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "DeleteObject", scheme_DeleteObject);
/*DeletePreviousObject deletepreviousobject*/
SCM scheme_DeletePreviousObject(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "DeletePreviousObject", scheme_DeletePreviousObject);
/*InsertMeasure insert_measure_key*/
SCM scheme_InsertMeasure(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertMeasure", scheme_InsertMeasure);
/*AddMeasure addmeasureafter*/
SCM scheme_AddMeasure(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddMeasure", scheme_AddMeasure);
/*InsertMeasureBefore insertmeasurebefore*/
SCM scheme_InsertMeasureBefore(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertMeasureBefore", scheme_InsertMeasureBefore);
/*InsertMeasureAfter insertmeasureafter*/
SCM scheme_InsertMeasureAfter(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertMeasureAfter", scheme_InsertMeasureAfter);
/*AppendMeasure append_measure_key*/
SCM scheme_AppendMeasure(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AppendMeasure", scheme_AppendMeasure);
/*DeleteMeasure deletemeasure*/
SCM scheme_DeleteMeasure(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "DeleteMeasure", scheme_DeleteMeasure);
/*DeleteMeasureAllStaffs deletemeasureallstaffs*/
SCM scheme_DeleteMeasureAllStaffs(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "DeleteMeasureAllStaffs", scheme_DeleteMeasureAllStaffs);
/*ShrinkMeasures adjust_measure_less_width_key*/
SCM scheme_ShrinkMeasures(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ShrinkMeasures", scheme_ShrinkMeasures);
/*WidenMeasures adjust_measure_more_width_key*/
SCM scheme_WidenMeasures(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "WidenMeasures", scheme_WidenMeasures);
/*ShorterStaffs adjust_staff_less_height_key*/
SCM scheme_ShorterStaffs(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ShorterStaffs", scheme_ShorterStaffs);
/*TallerStaffs adjust_staff_more_height_key*/
SCM scheme_TallerStaffs(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "TallerStaffs", scheme_TallerStaffs);
/*InsertTrebleClef clef_new_treble*/
SCM scheme_InsertTrebleClef(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertTrebleClef", scheme_InsertTrebleClef);
/*InsertBassClef clef_new_bass*/
SCM scheme_InsertBassClef(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertBassClef", scheme_InsertBassClef);
/*Insertg8clef clef_new_g8*/
SCM scheme_Insertg8clef(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insertg8clef", scheme_Insertg8clef);
/*InsertAltoClef clef_new_alto*/
SCM scheme_InsertAltoClef(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertAltoClef", scheme_InsertAltoClef);
/*InsertTenorClef clef_new_tenor*/
SCM scheme_InsertTenorClef(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertTenorClef", scheme_InsertTenorClef);
/*InsertSopranoClef clef_new_soprano*/
SCM scheme_InsertSopranoClef(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertSopranoClef", scheme_InsertSopranoClef);
/*SetInitialTrebleClef clef_set_treble*/
SCM scheme_SetInitialTrebleClef(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialTrebleClef", scheme_SetInitialTrebleClef);
/*SetInitialBassClef clef_set_bass*/
SCM scheme_SetInitialBassClef(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialBassClef", scheme_SetInitialBassClef);
/*SetInitialg8clef clef_set_g8*/
SCM scheme_SetInitialg8clef(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialg8clef", scheme_SetInitialg8clef);
/*SetInitialAltoClef clef_set_alto*/
SCM scheme_SetInitialAltoClef(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialAltoClef", scheme_SetInitialAltoClef);
/*SetInitialTenorClef clef_set_tenor*/
SCM scheme_SetInitialTenorClef(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialTenorClef", scheme_SetInitialTenorClef);
/*SetInitialSopranoClef clef_set_soprano*/
SCM scheme_SetInitialSopranoClef(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialSopranoClef", scheme_SetInitialSopranoClef);
/*Insert22Time newtimesig22*/
SCM scheme_Insert22Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert22Time", scheme_Insert22Time);
/*Insert32Time newtimesig32*/
SCM scheme_Insert32Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert32Time", scheme_Insert32Time);
/*Insert42Time newtimesig42*/
SCM scheme_Insert42Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert42Time", scheme_Insert42Time);
/*Insert44Time newtimesig44*/
SCM scheme_Insert44Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert44Time", scheme_Insert44Time);
/*Insert34Time newtimesig34*/
SCM scheme_Insert34Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert34Time", scheme_Insert34Time);
/*Insert24Time newtimesig24*/
SCM scheme_Insert24Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert24Time", scheme_Insert24Time);
/*Insert64Time newtimesig64*/
SCM scheme_Insert64Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert64Time", scheme_Insert64Time);
/*Insert38Time newtimesig38*/
SCM scheme_Insert38Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert38Time", scheme_Insert38Time);
/*Insert68Time newtimesig68*/
SCM scheme_Insert68Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert68Time", scheme_Insert68Time);
/*Insert128Time newtimesig128*/
SCM scheme_Insert128Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert128Time", scheme_Insert128Time);
/*Insert98Time newtimesig98*/
SCM scheme_Insert98Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert98Time", scheme_Insert98Time);
/*Set22Time settimesig22*/
SCM scheme_Set22Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set22Time", scheme_Set22Time);
/*Set32Time settimesig32*/
SCM scheme_Set32Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set32Time", scheme_Set32Time);
/*Set42Time settimesig42*/
SCM scheme_Set42Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set42Time", scheme_Set42Time);
/*Set44Time settimesig44*/
SCM scheme_Set44Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set44Time", scheme_Set44Time);
/*Set34Time settimesig34*/
SCM scheme_Set34Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set34Time", scheme_Set34Time);
/*Set24Time settimesig24*/
SCM scheme_Set24Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set24Time", scheme_Set24Time);
/*Set64Time settimesig64*/
SCM scheme_Set64Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set64Time", scheme_Set64Time);
/*Set38Time settimesig38*/
SCM scheme_Set38Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set38Time", scheme_Set38Time);
/*Set68Time settimesig68*/
SCM scheme_Set68Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set68Time", scheme_Set68Time);
/*Set128Time settimesig128*/
SCM scheme_Set128Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set128Time", scheme_Set128Time);
/*Set98Time settimesig98*/
SCM scheme_Set98Time(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set98Time", scheme_Set98Time);
/*InsertCmaj keysig_new_cmaj*/
SCM scheme_InsertCmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertCmaj", scheme_InsertCmaj);
/*InsertGmaj keysig_new_gmaj*/
SCM scheme_InsertGmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertGmaj", scheme_InsertGmaj);
/*InsertDmaj keysig_new_dmaj*/
SCM scheme_InsertDmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertDmaj", scheme_InsertDmaj);
/*InsertAmaj keysig_new_amaj*/
SCM scheme_InsertAmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertAmaj", scheme_InsertAmaj);
/*InsertEmaj keysig_new_emaj*/
SCM scheme_InsertEmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertEmaj", scheme_InsertEmaj);
/*InsertBmaj keysig_new_bmaj*/
SCM scheme_InsertBmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertBmaj", scheme_InsertBmaj);
/*InsertFSharpmaj keysig_new_fsharpmaj*/
SCM scheme_InsertFSharpmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertFSharpmaj", scheme_InsertFSharpmaj);
/*InsertCSharpmaj keysig_new_csharpmaj*/
SCM scheme_InsertCSharpmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertCSharpmaj", scheme_InsertCSharpmaj);
/*InsertFmaj keysig_new_fmaj*/
SCM scheme_InsertFmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertFmaj", scheme_InsertFmaj);
/*InsertBflatmaj keysig_new_bflatmaj*/
SCM scheme_InsertBflatmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertBflatmaj", scheme_InsertBflatmaj);
/*InsertEflatmaj keysig_new_eflatmaj*/
SCM scheme_InsertEflatmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertEflatmaj", scheme_InsertEflatmaj);
/*InsertAflatmaj keysig_new_aflatmaj*/
SCM scheme_InsertAflatmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertAflatmaj", scheme_InsertAflatmaj);
/*InsertDflatmaj keysig_new_dflatmaj*/
SCM scheme_InsertDflatmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertDflatmaj", scheme_InsertDflatmaj);
/*InsertGflatmaj keysig_new_gflatmaj*/
SCM scheme_InsertGflatmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertGflatmaj", scheme_InsertGflatmaj);
/*InsertCflatmaj keysig_new_cflatmaj*/
SCM scheme_InsertCflatmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertCflatmaj", scheme_InsertCflatmaj);
/*InsertAmin keysig_new_amin*/
SCM scheme_InsertAmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertAmin", scheme_InsertAmin);
/*InsertEmin keysig_new_emin*/
SCM scheme_InsertEmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertEmin", scheme_InsertEmin);
/*InsertBmin keysig_new_bmin*/
SCM scheme_InsertBmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertBmin", scheme_InsertBmin);
/*InsertFSharpmin keysig_new_fsharpmin*/
SCM scheme_InsertFSharpmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertFSharpmin", scheme_InsertFSharpmin);
/*InsertCSharpmin keysig_new_csharpmin*/
SCM scheme_InsertCSharpmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertCSharpmin", scheme_InsertCSharpmin);
/*InsertGSharpmin keysig_new_gsharpmin*/
SCM scheme_InsertGSharpmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertGSharpmin", scheme_InsertGSharpmin);
/*InsertDSharpmin keysig_new_dsharpmin*/
SCM scheme_InsertDSharpmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertDSharpmin", scheme_InsertDSharpmin);
/*InsertASharpmin keysig_new_asharpmin*/
SCM scheme_InsertASharpmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertASharpmin", scheme_InsertASharpmin);
/*InsertDmin keysig_new_dmin*/
SCM scheme_InsertDmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertDmin", scheme_InsertDmin);
/*InsertGmin keysig_new_gmin*/
SCM scheme_InsertGmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertGmin", scheme_InsertGmin);
/*InsertCmin keysig_new_cmin*/
SCM scheme_InsertCmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertCmin", scheme_InsertCmin);
/*InsertFmin keysig_new_fmin*/
SCM scheme_InsertFmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertFmin", scheme_InsertFmin);
/*InsertBflatmin keysig_new_bflatmin*/
SCM scheme_InsertBflatmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertBflatmin", scheme_InsertBflatmin);
/*InsertEflatmin keysig_new_eflatmin*/
SCM scheme_InsertEflatmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertEflatmin", scheme_InsertEflatmin);
/*InsertAflatmin keysig_new_aflatmin*/
SCM scheme_InsertAflatmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertAflatmin", scheme_InsertAflatmin);
/*SetInitialCmaj keysig_set_cmaj*/
SCM scheme_SetInitialCmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialCmaj", scheme_SetInitialCmaj);
/*SetInitialGmaj keysig_set_gmaj*/
SCM scheme_SetInitialGmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialGmaj", scheme_SetInitialGmaj);
/*SetInitialDmaj keysig_set_dmaj*/
SCM scheme_SetInitialDmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialDmaj", scheme_SetInitialDmaj);
/*SetInitialAmaj keysig_set_amaj*/
SCM scheme_SetInitialAmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialAmaj", scheme_SetInitialAmaj);
/*SetInitialEmaj keysig_set_emaj*/
SCM scheme_SetInitialEmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialEmaj", scheme_SetInitialEmaj);
/*SetInitialBmaj keysig_set_bmaj*/
SCM scheme_SetInitialBmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialBmaj", scheme_SetInitialBmaj);
/*SetInitialFSharpmaj keysig_set_fsharpmaj*/
SCM scheme_SetInitialFSharpmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialFSharpmaj", scheme_SetInitialFSharpmaj);
/*SetInitialCSharpmaj keysig_set_csharpmaj*/
SCM scheme_SetInitialCSharpmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialCSharpmaj", scheme_SetInitialCSharpmaj);
/*SetInitialFmaj keysig_set_fmaj*/
SCM scheme_SetInitialFmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialFmaj", scheme_SetInitialFmaj);
/*SetInitialBflatmaj keysig_set_bflatmaj*/
SCM scheme_SetInitialBflatmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialBflatmaj", scheme_SetInitialBflatmaj);
/*SetInitialEflatmaj keysig_set_eflatmaj*/
SCM scheme_SetInitialEflatmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialEflatmaj", scheme_SetInitialEflatmaj);
/*SetInitialAflatmaj keysig_set_aflatmaj*/
SCM scheme_SetInitialAflatmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialAflatmaj", scheme_SetInitialAflatmaj);
/*SetInitialDflatmaj keysig_set_dflatmaj*/
SCM scheme_SetInitialDflatmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialDflatmaj", scheme_SetInitialDflatmaj);
/*SetInitialGflatmaj keysig_set_gflatmaj*/
SCM scheme_SetInitialGflatmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialGflatmaj", scheme_SetInitialGflatmaj);
/*SetInitialCflatmaj keysig_set_cflatmaj*/
SCM scheme_SetInitialCflatmaj(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialCflatmaj", scheme_SetInitialCflatmaj);
/*SetInitialAmin keysig_set_amin*/
SCM scheme_SetInitialAmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialAmin", scheme_SetInitialAmin);
/*SetInitialEmin keysig_set_emin*/
SCM scheme_SetInitialEmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialEmin", scheme_SetInitialEmin);
/*SetInitialBmin keysig_set_bmin*/
SCM scheme_SetInitialBmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialBmin", scheme_SetInitialBmin);
/*SetInitialFSharpmin keysig_set_fsharpmin*/
SCM scheme_SetInitialFSharpmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialFSharpmin", scheme_SetInitialFSharpmin);
/*SetInitialCSharpmin keysig_set_csharpmin*/
SCM scheme_SetInitialCSharpmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialCSharpmin", scheme_SetInitialCSharpmin);
/*SetInitialGSharpmin keysig_set_gsharpmin*/
SCM scheme_SetInitialGSharpmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialGSharpmin", scheme_SetInitialGSharpmin);
/*SetInitialDSharpmin keysig_set_dsharpmin*/
SCM scheme_SetInitialDSharpmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialDSharpmin", scheme_SetInitialDSharpmin);
/*SetInitialASharpmin keysig_set_asharpmin*/
SCM scheme_SetInitialASharpmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialASharpmin", scheme_SetInitialASharpmin);
/*SetInitialDmin keysig_set_dmin*/
SCM scheme_SetInitialDmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialDmin", scheme_SetInitialDmin);
/*SetInitialGmin keysig_set_gmin*/
SCM scheme_SetInitialGmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialGmin", scheme_SetInitialGmin);
/*SetInitialCmin keysig_set_cmin*/
SCM scheme_SetInitialCmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialCmin", scheme_SetInitialCmin);
/*SetInitialFmin keysig_set_fmin*/
SCM scheme_SetInitialFmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialFmin", scheme_SetInitialFmin);
/*SetInitialBflatmin keysig_set_bflatmin*/
SCM scheme_SetInitialBflatmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialBflatmin", scheme_SetInitialBflatmin);
/*SetInitialEflatmin keysig_set_eflatmin*/
SCM scheme_SetInitialEflatmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialEflatmin", scheme_SetInitialEflatmin);
/*SetInitialAflatmin keysig_set_aflatmin*/
SCM scheme_SetInitialAflatmin(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetInitialAflatmin", scheme_SetInitialAflatmin);
/*SetMark set_mark*/
SCM scheme_SetMark(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetMark", scheme_SetMark);
/*UnsetMark unset_mark*/
SCM scheme_UnsetMark(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "UnsetMark", scheme_UnsetMark);
/*SetPoint set_point*/
SCM scheme_SetPoint(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SetPoint", scheme_SetPoint);
/*ToggleBeginSlur toggle_begin_slur*/
SCM scheme_ToggleBeginSlur(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ToggleBeginSlur", scheme_ToggleBeginSlur);
/*ToggleEndSlur toggle_end_slur*/
SCM scheme_ToggleEndSlur(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ToggleEndSlur", scheme_ToggleEndSlur);
/*ToggleStartCrescendo toggle_start_crescendo*/
SCM scheme_ToggleStartCrescendo(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ToggleStartCrescendo", scheme_ToggleStartCrescendo);
/*ToggleEndCrescendo toggle_end_crescendo*/
SCM scheme_ToggleEndCrescendo(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ToggleEndCrescendo", scheme_ToggleEndCrescendo);
/*ToggleStartDiminuendo toggle_start_diminuendo*/
SCM scheme_ToggleStartDiminuendo(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ToggleStartDiminuendo", scheme_ToggleStartDiminuendo);
/*ToggleEndDiminuendo toggle_end_diminuendo*/
SCM scheme_ToggleEndDiminuendo(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ToggleEndDiminuendo", scheme_ToggleEndDiminuendo);
/*ToggleGrace toggle_grace*/
SCM scheme_ToggleGrace(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ToggleGrace", scheme_ToggleGrace);
/*ToggleAcciaccatura toggle_acciaccatura*/
SCM scheme_ToggleAcciaccatura(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ToggleAcciaccatura", scheme_ToggleAcciaccatura);
/*ForceCaution force_cautionary*/
SCM scheme_ForceCaution(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ForceCaution", scheme_ForceCaution);
/*ChangePitch change_pitch*/
SCM scheme_ChangePitch(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ChangePitch", scheme_ChangePitch);
/*InsertRhythm insert_rhythm_pattern*/
SCM scheme_InsertRhythm(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertRhythm", scheme_InsertRhythm);
/*NextRhythm nextrhythm*/
SCM scheme_NextRhythm(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "NextRhythm", scheme_NextRhythm);
/*AppendMeasureAllStaffs append_measure_score*/
SCM scheme_AppendMeasureAllStaffs(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AppendMeasureAllStaffs", scheme_AppendMeasureAllStaffs);
/*ExecuteScheme execute_scheme*/
SCM scheme_ExecuteScheme(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ExecuteScheme", scheme_ExecuteScheme);
/*SharpenEnharmonicSet set_sharper*/
SCM scheme_SharpenEnharmonicSet(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SharpenEnharmonicSet", scheme_SharpenEnharmonicSet);
/*FlattenEnharmonicSet set_flatter*/
SCM scheme_FlattenEnharmonicSet(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "FlattenEnharmonicSet", scheme_FlattenEnharmonicSet);
/*New file_newwrapper*/
SCM scheme_New(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "New", scheme_New);
/*NewScore new_score_cb*/
SCM scheme_NewScore(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "NewScore", scheme_NewScore);
/*Open file_open_with_check*/
SCM scheme_Open(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Open", scheme_Open);
/*ImportLilypond file_import_lilypond_with_check*/
SCM scheme_ImportLilypond(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ImportLilypond", scheme_ImportLilypond);
/*ImportMidi file_import_midi_with_check*/
SCM scheme_ImportMidi(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ImportMidi", scheme_ImportMidi);
/*ImportMusicXml file_import_musicxml_with_check*/
SCM scheme_ImportMusicXml(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ImportMusicXml", scheme_ImportMusicXml);
/*AddStaffs file_add_staffs*/
SCM scheme_AddStaffs(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddStaffs", scheme_AddStaffs);
/*AddMovements file_add_movements*/
SCM scheme_AddMovements(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddMovements", scheme_AddMovements);
/*MovementProps movement_props_dialog*/
SCM scheme_MovementProps(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MovementProps", scheme_MovementProps);
/*OpenNewWindow openinnew*/
SCM scheme_OpenNewWindow(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "OpenNewWindow", scheme_OpenNewWindow);
/*Save file_savewrapper*/
SCM scheme_Save(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Save", scheme_Save);
/*SaveAs file_saveaswrapper*/
SCM scheme_SaveAs(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SaveAs", scheme_SaveAs);
/*SaveCopy file_copy_save*/
SCM scheme_SaveCopy(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SaveCopy", scheme_SaveCopy);
/*OpenTemplate system_template_open_with_check*/
SCM scheme_OpenTemplate(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "OpenTemplate", scheme_OpenTemplate);
/*OpenExample system_example_open_with_check*/
SCM scheme_OpenExample(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "OpenExample", scheme_OpenExample);
/*OpenMyTemplate local_template_open_with_check*/
SCM scheme_OpenMyTemplate(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "OpenMyTemplate", scheme_OpenMyTemplate);
/*SaveTemplate template_save*/
SCM scheme_SaveTemplate(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SaveTemplate", scheme_SaveTemplate);
/*NewWindow newview*/
SCM scheme_NewWindow(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "NewWindow", scheme_NewWindow);
/*InsertMovementBefore insert_movement_before*/
SCM scheme_InsertMovementBefore(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertMovementBefore", scheme_InsertMovementBefore);
/*InsertMovementAfter insert_movement_after*/
SCM scheme_InsertMovementAfter(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertMovementAfter", scheme_InsertMovementAfter);
/*NewMovement append_new_movement*/
SCM scheme_NewMovement(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "NewMovement", scheme_NewMovement);
/*SaveParts file_savepartswrapper*/
SCM scheme_SaveParts(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SaveParts", scheme_SaveParts);
/*ExportMUDELA export_mudela_action*/
SCM scheme_ExportMUDELA(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ExportMUDELA", scheme_ExportMUDELA);
/*ExportMusicXML export_musicXML_action*/
SCM scheme_ExportMusicXML(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ExportMusicXML", scheme_ExportMusicXML);
/*ExportPDF export_pdf_action*/
SCM scheme_ExportPDF(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ExportPDF", scheme_ExportPDF);
/*ExportPNG export_png_action*/
SCM scheme_ExportPNG(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ExportPNG", scheme_ExportPNG);
/*ExportMIDI export_midi_action*/
SCM scheme_ExportMIDI(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ExportMIDI", scheme_ExportMIDI);
/*PrintView show_print_view*/
SCM scheme_PrintView(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "PrintView", scheme_PrintView);
/*PrintSelection printselection_cb*/
SCM scheme_PrintSelection(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "PrintSelection", scheme_PrintSelection);
/*PrintExcerptPreview printexcerptpreview_cb*/
SCM scheme_PrintExcerptPreview(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "PrintExcerptPreview", scheme_PrintExcerptPreview);
/*PrintMovement printmovement_cb*/
SCM scheme_PrintMovement(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "PrintMovement", scheme_PrintMovement);
/*Print printall_cb*/
SCM scheme_Print(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Print", scheme_Print);
/*PrintPart printpart_cb*/
SCM scheme_PrintPart(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "PrintPart", scheme_PrintPart);
/*Close close_gui_with_check*/
SCM scheme_Close(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Close", scheme_Close);
/*Quit closewrapper*/
SCM scheme_Quit(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Quit", scheme_Quit);
/*Undo undowrapper*/
SCM scheme_Undo(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Undo", scheme_Undo);
/*Redo redowrapper*/
SCM scheme_Redo(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Redo", scheme_Redo);
/*Copy copywrapper*/
SCM scheme_Copy(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Copy", scheme_Copy);
/*Cut cutwrapper*/
SCM scheme_Cut(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Cut", scheme_Cut);
/*Paste pastewrapper*/
SCM scheme_Paste(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Paste", scheme_Paste);
/*PasteClipboard paste_clipboard*/
SCM scheme_PasteClipboard(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "PasteClipboard", scheme_PasteClipboard);
/*PasteComment paste_comment*/
SCM scheme_PasteComment(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "PasteComment", scheme_PasteComment);
/*ScoreProperties score_properties_dialog*/
SCM scheme_ScoreProperties(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ScoreProperties", scheme_ScoreProperties);
/*Preferences preferences_change*/
SCM scheme_Preferences(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Preferences", scheme_Preferences);
/*SaveAccels save_default_keymap_file_wrapper*/
SCM scheme_SaveAccels(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SaveAccels", scheme_SaveAccels);
/*CommandManagement configure_keyboard_dialog*/
SCM scheme_CommandManagement(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "CommandManagement", scheme_CommandManagement);
/*SwapStaffs swapstaffs*/
SCM scheme_SwapStaffs(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SwapStaffs", scheme_SwapStaffs);
/*SplitVoices splitstaffs*/
SCM scheme_SplitVoices(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SplitVoices", scheme_SplitVoices);
/*JoinVoices joinstaffs*/
SCM scheme_JoinVoices(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "JoinVoices", scheme_JoinVoices);
/*SwapMovements swapmovements*/
SCM scheme_SwapMovements(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "SwapMovements", scheme_SwapMovements);
/*VoiceUp voiceup*/
SCM scheme_VoiceUp(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "VoiceUp", scheme_VoiceUp);
/*VoiceDown voicedown*/
SCM scheme_VoiceDown(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "VoiceDown", scheme_VoiceDown);
/*MoveToVoiceUp movetovoiceup*/
SCM scheme_MoveToVoiceUp(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoveToVoiceUp", scheme_MoveToVoiceUp);
/*MoveToVoiceDown movetovoicedown*/
SCM scheme_MoveToVoiceDown(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoveToVoiceDown", scheme_MoveToVoiceDown);
/*AddBefore staff_new_before*/
SCM scheme_AddBefore(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddBefore", scheme_AddBefore);
/*AddAfter staff_new_after*/
SCM scheme_AddAfter(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddAfter", scheme_AddAfter);
/*AddInitial staff_new_initial*/
SCM scheme_AddInitial(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddInitial", scheme_AddInitial);
/*AddLast staff_new_last*/
SCM scheme_AddLast(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddLast", scheme_AddLast);
/*DeleteBefore delete_staff_before*/
SCM scheme_DeleteBefore(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "DeleteBefore", scheme_DeleteBefore);
/*DeleteStaff delete_staff_current*/
SCM scheme_DeleteStaff(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "DeleteStaff", scheme_DeleteStaff);
/*DeleteAfter delete_staff_after*/
SCM scheme_DeleteAfter(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "DeleteAfter", scheme_DeleteAfter);
/*AddVoice staff_new_voice*/
SCM scheme_AddVoice(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddVoice", scheme_AddVoice);
/*StaffProperties staff_properties_change_cb*/
SCM scheme_StaffProperties(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "StaffProperties", scheme_StaffProperties);
/*InitialClef clef_change_initial*/
SCM scheme_InitialClef(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InitialClef", scheme_InitialClef);
/*InsertClef clef_change_insert*/
SCM scheme_InsertClef(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertClef", scheme_InsertClef);
/*InitialKey key_change_initial*/
SCM scheme_InitialKey(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InitialKey", scheme_InitialKey);
/*InsertKey key_change_insert*/
SCM scheme_InsertKey(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertKey", scheme_InsertKey);
/*InitialTimeSig timesig_change_initial*/
SCM scheme_InitialTimeSig(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InitialTimeSig", scheme_InitialTimeSig);
/*InsertTimeSig timesig_change_insert*/
SCM scheme_InsertTimeSig(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertTimeSig", scheme_InsertTimeSig);
/*ChangeNotehead set_notehead*/
SCM scheme_ChangeNotehead(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ChangeNotehead", scheme_ChangeNotehead);
/*InsertStem stem_directive_insert*/
SCM scheme_InsertStem(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertStem", scheme_InsertStem);
/*AddVerse add_verse*/
SCM scheme_AddVerse(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddVerse", scheme_AddVerse);
/*DeleteVerse delete_verse*/
SCM scheme_DeleteVerse(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "DeleteVerse", scheme_DeleteVerse);
/*EditFiguredBass figure_insert*/
SCM scheme_EditFiguredBass(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EditFiguredBass", scheme_EditFiguredBass);
/*DeleteFiguredBass delete_figured_bass*/
SCM scheme_DeleteFiguredBass(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "DeleteFiguredBass", scheme_DeleteFiguredBass);
/*DeleteChordSymbols delete_fakechords*/
SCM scheme_DeleteChordSymbols(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "DeleteChordSymbols", scheme_DeleteChordSymbols);
/*HideFiguredBass hide_figured_bass*/
SCM scheme_HideFiguredBass(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "HideFiguredBass", scheme_HideFiguredBass);
/*ShowFiguredBass show_figured_bass*/
SCM scheme_ShowFiguredBass(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ShowFiguredBass", scheme_ShowFiguredBass);
/*EditChords fakechord_insert*/
SCM scheme_EditChords(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EditChords", scheme_EditChords);
/*EditObject edit_object_type*/
SCM scheme_EditObject(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EditObject", scheme_EditObject);
/*EditCursorObject edit_object*/
SCM scheme_EditCursorObject(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EditCursorObject", scheme_EditCursorObject);
/*EditScoreProperties edit_score_properties*/
SCM scheme_EditScoreProperties(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EditScoreProperties", scheme_EditScoreProperties);
/*EditMovementProperties edit_movement_properties*/
SCM scheme_EditMovementProperties(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EditMovementProperties", scheme_EditMovementProperties);
/*EditStaffProperties edit_staff_properties*/
SCM scheme_EditStaffProperties(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EditStaffProperties", scheme_EditStaffProperties);
/*EditVoiceProperties edit_voice_properties*/
SCM scheme_EditVoiceProperties(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EditVoiceProperties", scheme_EditVoiceProperties);
/*EditDirective edit_object_directive*/
SCM scheme_EditDirective(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EditDirective", scheme_EditDirective);
/*EditStaffDirective edit_staff_directive*/
SCM scheme_EditStaffDirective(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EditStaffDirective", scheme_EditStaffDirective);
/*EditVoiceDirective edit_voice_directive*/
SCM scheme_EditVoiceDirective(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EditVoiceDirective", scheme_EditVoiceDirective);
/*EditScoreDirective edit_score_directive*/
SCM scheme_EditScoreDirective(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EditScoreDirective", scheme_EditScoreDirective);
/*EditMovementDirective edit_movement_directive*/
SCM scheme_EditMovementDirective(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EditMovementDirective", scheme_EditMovementDirective);
/*EditClefDirective edit_clef_directive*/
SCM scheme_EditClefDirective(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EditClefDirective", scheme_EditClefDirective);
/*EditTimesigDirective edit_timesig_directive*/
SCM scheme_EditTimesigDirective(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EditTimesigDirective", scheme_EditTimesigDirective);
/*EditKeysigDirective edit_keysig_directive*/
SCM scheme_EditKeysigDirective(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "EditKeysigDirective", scheme_EditKeysigDirective);
/*DeleteDirective delete_chord_or_note_directive*/
SCM scheme_DeleteDirective(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "DeleteDirective", scheme_DeleteDirective);
/*GoToMeasure tomeasurenum*/
SCM scheme_GoToMeasure(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "GoToMeasure", scheme_GoToMeasure);
/*GoToBeginning tohome*/
SCM scheme_GoToBeginning(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "GoToBeginning", scheme_GoToBeginning);
/*GoToEnd toend*/
SCM scheme_GoToEnd(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "GoToEnd", scheme_GoToEnd);
/*MoveToBeginning movetostart*/
SCM scheme_MoveToBeginning(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoveToBeginning", scheme_MoveToBeginning);
/*MoveToEnd movetoend*/
SCM scheme_MoveToEnd(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoveToEnd", scheme_MoveToEnd);
/*NextMovement next_movement*/
SCM scheme_NextMovement(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "NextMovement", scheme_NextMovement);
/*PreviousMovement prev_movement*/
SCM scheme_PreviousMovement(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "PreviousMovement", scheme_PreviousMovement);
/*DeleteMovement delete_movement*/
SCM scheme_DeleteMovement(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "DeleteMovement", scheme_DeleteMovement);
/*Play ext_midi_playback*/
SCM scheme_Play(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Play", scheme_Play);
/*Stop stop_midi_playback*/
SCM scheme_Stop(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Stop", scheme_Stop);
/*PlaybackProperties playback_properties_change*/
SCM scheme_PlaybackProperties(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "PlaybackProperties", scheme_PlaybackProperties);
/*Help browse_manual*/
SCM scheme_Help(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Help", scheme_Help);
/*About about*/
SCM scheme_About(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "About", scheme_About);
/*Shortcuts display_shortcuts*/
SCM scheme_Shortcuts(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Shortcuts", scheme_Shortcuts);
/*MoreCommands morecommands*/
SCM scheme_MoreCommands(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoreCommands", scheme_MoreCommands);
/*MyCommands mycommands*/
SCM scheme_MyCommands(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MyCommands", scheme_MyCommands);
/*FetchCommands fetchcommands*/
SCM scheme_FetchCommands(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "FetchCommands", scheme_FetchCommands);

/*ClearOverlay clear_overlay*/
SCM scheme_ClearOverlay(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ClearOverlay", scheme_ClearOverlay);
/*CreateRhythm create_rhythm_cb*/
SCM scheme_CreateRhythm(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "CreateRhythm", scheme_CreateRhythm);
/*DeleteRhythm delete_rhythm_cb*/
SCM scheme_DeleteRhythm(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "DeleteRhythm", scheme_DeleteRhythm);
SCM scheme_InsertA(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertA", scheme_InsertA);
SCM scheme_AddNoteA(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddNoteA", scheme_AddNoteA);
SCM scheme_AddA(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddA", scheme_AddA);
SCM scheme_ChangeToA(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ChangeToA", scheme_ChangeToA);
SCM scheme_MoveToA(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoveToA", scheme_MoveToA);
SCM scheme_InsertB(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertB", scheme_InsertB);
SCM scheme_AddNoteB(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddNoteB", scheme_AddNoteB);
SCM scheme_AddB(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddB", scheme_AddB);
SCM scheme_ChangeToB(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ChangeToB", scheme_ChangeToB);
SCM scheme_MoveToB(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoveToB", scheme_MoveToB);
SCM scheme_InsertC(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertC", scheme_InsertC);
SCM scheme_AddNoteC(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddNoteC", scheme_AddNoteC);
SCM scheme_AddC(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddC", scheme_AddC);
SCM scheme_ChangeToC(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ChangeToC", scheme_ChangeToC);
SCM scheme_MoveToC(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoveToC", scheme_MoveToC);
SCM scheme_InsertD(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertD", scheme_InsertD);
SCM scheme_AddNoteD(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddNoteD", scheme_AddNoteD);
SCM scheme_AddD(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddD", scheme_AddD);
SCM scheme_ChangeToD(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ChangeToD", scheme_ChangeToD);
SCM scheme_MoveToD(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoveToD", scheme_MoveToD);
SCM scheme_InsertE(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertE", scheme_InsertE);
SCM scheme_AddNoteE(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddNoteE", scheme_AddNoteE);
SCM scheme_AddE(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddE", scheme_AddE);
SCM scheme_ChangeToE(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ChangeToE", scheme_ChangeToE);
SCM scheme_MoveToE(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoveToE", scheme_MoveToE);
SCM scheme_InsertF(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertF", scheme_InsertF);
SCM scheme_AddNoteF(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddNoteF", scheme_AddNoteF);
SCM scheme_AddF(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddF", scheme_AddF);
SCM scheme_ChangeToF(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ChangeToF", scheme_ChangeToF);
SCM scheme_MoveToF(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoveToF", scheme_MoveToF);
SCM scheme_InsertG(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertG", scheme_InsertG);
SCM scheme_AddNoteG(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddNoteG", scheme_AddNoteG);
SCM scheme_AddG(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "AddG", scheme_AddG);
SCM scheme_ChangeToG(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "ChangeToG", scheme_ChangeToG);
SCM scheme_MoveToG(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "MoveToG", scheme_MoveToG);
/*0 */
SCM scheme_0(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "0", scheme_0);
SCM scheme_InsertDur0(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert0", scheme_InsertDur0);
SCM scheme_ChangeDur0(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Change0", scheme_ChangeDur0);
SCM scheme_SetDur0(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set0", scheme_SetDur0);
SCM scheme_InsertRest0(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertRest0", scheme_InsertRest0);
/*1 */
SCM scheme_1(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "1", scheme_1);
SCM scheme_InsertDur1(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert1", scheme_InsertDur1);
SCM scheme_ChangeDur1(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Change1", scheme_ChangeDur1);
SCM scheme_SetDur1(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set1", scheme_SetDur1);
SCM scheme_InsertRest1(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertRest1", scheme_InsertRest1);
/*2 */
SCM scheme_2(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "2", scheme_2);
SCM scheme_InsertDur2(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert2", scheme_InsertDur2);
SCM scheme_ChangeDur2(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Change2", scheme_ChangeDur2);
SCM scheme_SetDur2(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set2", scheme_SetDur2);
SCM scheme_InsertRest2(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertRest2", scheme_InsertRest2);
/*3 */
SCM scheme_3(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "3", scheme_3);
SCM scheme_InsertDur3(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert3", scheme_InsertDur3);
SCM scheme_ChangeDur3(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Change3", scheme_ChangeDur3);
SCM scheme_SetDur3(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set3", scheme_SetDur3);
SCM scheme_InsertRest3(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertRest3", scheme_InsertRest3);
/*4 */
SCM scheme_4(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "4", scheme_4);
SCM scheme_InsertDur4(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert4", scheme_InsertDur4);
SCM scheme_ChangeDur4(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Change4", scheme_ChangeDur4);
SCM scheme_SetDur4(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set4", scheme_SetDur4);
SCM scheme_InsertRest4(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertRest4", scheme_InsertRest4);
/*5 */
SCM scheme_5(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "5", scheme_5);
SCM scheme_InsertDur5(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert5", scheme_InsertDur5);
SCM scheme_ChangeDur5(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Change5", scheme_ChangeDur5);
SCM scheme_SetDur5(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set5", scheme_SetDur5);
SCM scheme_InsertRest5(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertRest5", scheme_InsertRest5);
/*6 */
SCM scheme_6(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "6", scheme_6);
SCM scheme_InsertDur6(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert6", scheme_InsertDur6);
SCM scheme_ChangeDur6(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Change6", scheme_ChangeDur6);
SCM scheme_SetDur6(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set6", scheme_SetDur6);
SCM scheme_InsertRest6(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertRest6", scheme_InsertRest6);
/*7 */
SCM scheme_7(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "7", scheme_7);
SCM scheme_InsertDur7(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert7", scheme_InsertDur7);
SCM scheme_ChangeDur7(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Change7", scheme_ChangeDur7);
SCM scheme_SetDur7(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set7", scheme_SetDur7);
SCM scheme_InsertRest7(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertRest7", scheme_InsertRest7);
/*8 */
SCM scheme_8(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "8", scheme_8);
SCM scheme_InsertDur8(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Insert8", scheme_InsertDur8);
SCM scheme_ChangeDur8(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Change8", scheme_ChangeDur8);
SCM scheme_SetDur8(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "Set8", scheme_SetDur8);
SCM scheme_InsertRest8(SCM optional);
install_scm_function (0, NULL, DENEMO_SCHEME_PREFIX "InsertRest8", scheme_InsertRest8);
