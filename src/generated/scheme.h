/*CursorLeft cursorleft*/
SCM scheme_CursorLeft (SCM optional);
install_scm_function ("d-CursorLeft", scheme_CursorLeft);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "CursorLeft")), "scm", (gpointer) 1);
/*MoveCursorLeft movecursorleft*/
SCM scheme_MoveCursorLeft (SCM optional);
install_scm_function ("d-MoveCursorLeft", scheme_MoveCursorLeft);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoveCursorLeft")), "scm", (gpointer) 1);
/*CursorDown cursordown*/
SCM scheme_CursorDown (SCM optional);
install_scm_function ("d-CursorDown", scheme_CursorDown);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "CursorDown")), "scm", (gpointer) 1);
/*CursorUp cursorup*/
SCM scheme_CursorUp (SCM optional);
install_scm_function ("d-CursorUp", scheme_CursorUp);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "CursorUp")), "scm", (gpointer) 1);
/*CursorRight cursorright*/
SCM scheme_CursorRight (SCM optional);
install_scm_function ("d-CursorRight", scheme_CursorRight);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "CursorRight")), "scm", (gpointer) 1);
/*MoveCursorRight movecursorright*/
SCM scheme_MoveCursorRight (SCM optional);
install_scm_function ("d-MoveCursorRight", scheme_MoveCursorRight);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoveCursorRight")), "scm", (gpointer) 1);
/*GoToMark goto_mark*/
SCM scheme_GoToMark (SCM optional);
install_scm_function ("d-GoToMark", scheme_GoToMark);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "GoToMark")), "scm", (gpointer) 1);
/*SwapPointAndMark swap_point_and_mark*/
SCM scheme_SwapPointAndMark (SCM optional);
install_scm_function ("d-SwapPointAndMark", scheme_SwapPointAndMark);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SwapPointAndMark")), "scm", (gpointer) 1);
/*GoToSelectionStart goto_selection_start*/
SCM scheme_GoToSelectionStart (SCM optional);
install_scm_function ("d-GoToSelectionStart", scheme_GoToSelectionStart);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "GoToSelectionStart")), "scm", (gpointer) 1);
/*PushPosition PushPosition*/
SCM scheme_PushPosition (SCM optional);
install_scm_function ("d-PushPosition", scheme_PushPosition);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "PushPosition")), "scm", (gpointer) 1);
/*PopPosition PopPosition*/
SCM scheme_PopPosition (SCM optional);
install_scm_function ("d-PopPosition", scheme_PopPosition);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "PopPosition")), "scm", (gpointer) 1);
/*PopPushPosition PopPushPosition*/
SCM scheme_PopPushPosition (SCM optional);
install_scm_function ("d-PopPushPosition", scheme_PopPushPosition);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "PopPushPosition")), "scm", (gpointer) 1);
/*ToggleReduceToDrawingArea ToggleReduceToDrawingArea*/
SCM scheme_ToggleReduceToDrawingArea (SCM optional);
install_scm_function ("d-ToggleReduceToDrawingArea", scheme_ToggleReduceToDrawingArea);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ToggleReduceToDrawingArea")), "scm", (gpointer) 1);
/*StaffUp staffup*/
SCM scheme_StaffUp (SCM optional);
install_scm_function ("d-StaffUp", scheme_StaffUp);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "StaffUp")), "scm", (gpointer) 1);
/*StaffDown staffdown*/
SCM scheme_StaffDown (SCM optional);
install_scm_function ("d-StaffDown", scheme_StaffDown);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "StaffDown")), "scm", (gpointer) 1);
/*MoveToStaffUp movetostaffup*/
SCM scheme_MoveToStaffUp (SCM optional);
install_scm_function ("d-MoveToStaffUp", scheme_MoveToStaffUp);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoveToStaffUp")), "scm", (gpointer) 1);
/*MoveToStaffDown movetostaffdown*/
SCM scheme_MoveToStaffDown (SCM optional);
install_scm_function ("d-MoveToStaffDown", scheme_MoveToStaffDown);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoveToStaffDown")), "scm", (gpointer) 1);
/*MeasureLeft measureleft*/
SCM scheme_MeasureLeft (SCM optional);
install_scm_function ("d-MeasureLeft", scheme_MeasureLeft);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MeasureLeft")), "scm", (gpointer) 1);
/*MeasureRight measureright*/
SCM scheme_MeasureRight (SCM optional);
install_scm_function ("d-MeasureRight", scheme_MeasureRight);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MeasureRight")), "scm", (gpointer) 1);
/*MoveToMeasureLeft movetomeasureleft*/
SCM scheme_MoveToMeasureLeft (SCM optional);
install_scm_function ("d-MoveToMeasureLeft", scheme_MoveToMeasureLeft);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoveToMeasureLeft")), "scm", (gpointer) 1);
/*MoveToMeasureRight movetomeasureright*/
SCM scheme_MoveToMeasureRight (SCM optional);
install_scm_function ("d-MoveToMeasureRight", scheme_MoveToMeasureRight);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoveToMeasureRight")), "scm", (gpointer) 1);
/*A go_to_A_key*/
SCM scheme_A (SCM optional);
install_scm_function ("d-A", scheme_A);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "A")), "scm", (gpointer) 1);
/*B go_to_B_key*/
SCM scheme_B (SCM optional);
install_scm_function ("d-B", scheme_B);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "B")), "scm", (gpointer) 1);
/*C go_to_C_key*/
SCM scheme_C (SCM optional);
install_scm_function ("d-C", scheme_C);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "C")), "scm", (gpointer) 1);
/*D go_to_D_key*/
SCM scheme_D (SCM optional);
install_scm_function ("d-D", scheme_D);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "D")), "scm", (gpointer) 1);
/*E go_to_E_key*/
SCM scheme_E (SCM optional);
install_scm_function ("d-E", scheme_E);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "E")), "scm", (gpointer) 1);
/*F go_to_F_key*/
SCM scheme_F (SCM optional);
install_scm_function ("d-F", scheme_F);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "F")), "scm", (gpointer) 1);
/*G go_to_G_key*/
SCM scheme_G (SCM optional);
install_scm_function ("d-G", scheme_G);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "G")), "scm", (gpointer) 1);
/*OctaveUp octave_up_key*/
SCM scheme_OctaveUp (SCM optional);
install_scm_function ("d-OctaveUp", scheme_OctaveUp);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "OctaveUp")), "scm", (gpointer) 1);
/*OctaveDown octave_down_key*/
SCM scheme_OctaveDown (SCM optional);
install_scm_function ("d-OctaveDown", scheme_OctaveDown);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "OctaveDown")), "scm", (gpointer) 1);
/*WholeNote insert_chord_0key*/
SCM scheme_WholeNote (SCM optional);
install_scm_function ("d-WholeNote", scheme_WholeNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "WholeNote")), "scm", (gpointer) 1);
/*HalfNote insert_chord_1key*/
SCM scheme_HalfNote (SCM optional);
install_scm_function ("d-HalfNote", scheme_HalfNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "HalfNote")), "scm", (gpointer) 1);
/*QuarterNote insert_chord_2key*/
SCM scheme_QuarterNote (SCM optional);
install_scm_function ("d-QuarterNote", scheme_QuarterNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "QuarterNote")), "scm", (gpointer) 1);
/*EighthNote insert_chord_3key*/
SCM scheme_EighthNote (SCM optional);
install_scm_function ("d-EighthNote", scheme_EighthNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "EighthNote")), "scm", (gpointer) 1);
/*SixteenthNote insert_chord_4key*/
SCM scheme_SixteenthNote (SCM optional);
install_scm_function ("d-SixteenthNote", scheme_SixteenthNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SixteenthNote")), "scm", (gpointer) 1);
/*ThirtysecondNote insert_chord_5key*/
SCM scheme_ThirtysecondNote (SCM optional);
install_scm_function ("d-ThirtysecondNote", scheme_ThirtysecondNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ThirtysecondNote")), "scm", (gpointer) 1);
/*SixtyfourthNote insert_chord_6key*/
SCM scheme_SixtyfourthNote (SCM optional);
install_scm_function ("d-SixtyfourthNote", scheme_SixtyfourthNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SixtyfourthNote")), "scm", (gpointer) 1);
/*OneHundredTwentyEighthNote insert_chord_7key*/
SCM scheme_OneHundredTwentyEighthNote (SCM optional);
install_scm_function ("d-OneHundredTwentyEighthNote", scheme_OneHundredTwentyEighthNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "OneHundredTwentyEighthNote")), "scm", (gpointer) 1);
/*TwoHundredFiftySixthNote insert_chord_8key*/
SCM scheme_TwoHundredFiftySixthNote (SCM optional);
install_scm_function ("d-TwoHundredFiftySixthNote", scheme_TwoHundredFiftySixthNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "TwoHundredFiftySixthNote")), "scm", (gpointer) 1);
/*InsertWholeRest insert_rest_0key*/
SCM scheme_InsertWholeRest (SCM optional);
install_scm_function ("d-InsertWholeRest", scheme_InsertWholeRest);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertWholeRest")), "scm", (gpointer) 1);
/*InsertHalfRest insert_rest_1key*/
SCM scheme_InsertHalfRest (SCM optional);
install_scm_function ("d-InsertHalfRest", scheme_InsertHalfRest);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertHalfRest")), "scm", (gpointer) 1);
/*InsertQuarterRest insert_rest_2key*/
SCM scheme_InsertQuarterRest (SCM optional);
install_scm_function ("d-InsertQuarterRest", scheme_InsertQuarterRest);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertQuarterRest")), "scm", (gpointer) 1);
/*InsertEighthRest insert_rest_3key*/
SCM scheme_InsertEighthRest (SCM optional);
install_scm_function ("d-InsertEighthRest", scheme_InsertEighthRest);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertEighthRest")), "scm", (gpointer) 1);
/*InsertSixteenthRest insert_rest_4key*/
SCM scheme_InsertSixteenthRest (SCM optional);
install_scm_function ("d-InsertSixteenthRest", scheme_InsertSixteenthRest);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertSixteenthRest")), "scm", (gpointer) 1);
/*InsertThirtysecondRest insert_rest_5key*/
SCM scheme_InsertThirtysecondRest (SCM optional);
install_scm_function ("d-InsertThirtysecondRest", scheme_InsertThirtysecondRest);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertThirtysecondRest")), "scm", (gpointer) 1);
/*InsertSixtyfourthRest insert_rest_6key*/
SCM scheme_InsertSixtyfourthRest (SCM optional);
install_scm_function ("d-InsertSixtyfourthRest", scheme_InsertSixtyfourthRest);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertSixtyfourthRest")), "scm", (gpointer) 1);
/*InsertBlankWholeNote insert_blankchord_0key*/
SCM scheme_InsertBlankWholeNote (SCM optional);
install_scm_function ("d-InsertBlankWholeNote", scheme_InsertBlankWholeNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertBlankWholeNote")), "scm", (gpointer) 1);
/*InsertBlankHalfNote insert_blankchord_1key*/
SCM scheme_InsertBlankHalfNote (SCM optional);
install_scm_function ("d-InsertBlankHalfNote", scheme_InsertBlankHalfNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertBlankHalfNote")), "scm", (gpointer) 1);
/*InsertBlankQuarterNote insert_blankchord_2key*/
SCM scheme_InsertBlankQuarterNote (SCM optional);
install_scm_function ("d-InsertBlankQuarterNote", scheme_InsertBlankQuarterNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertBlankQuarterNote")), "scm", (gpointer) 1);
/*InsertBlankEighthNote insert_blankchord_3key*/
SCM scheme_InsertBlankEighthNote (SCM optional);
install_scm_function ("d-InsertBlankEighthNote", scheme_InsertBlankEighthNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertBlankEighthNote")), "scm", (gpointer) 1);
/*InsertBlankSixteenthNote insert_blankchord_4key*/
SCM scheme_InsertBlankSixteenthNote (SCM optional);
install_scm_function ("d-InsertBlankSixteenthNote", scheme_InsertBlankSixteenthNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertBlankSixteenthNote")), "scm", (gpointer) 1);
/*InsertBlankThirtysecondNote insert_blankchord_5key*/
SCM scheme_InsertBlankThirtysecondNote (SCM optional);
install_scm_function ("d-InsertBlankThirtysecondNote", scheme_InsertBlankThirtysecondNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertBlankThirtysecondNote")), "scm", (gpointer) 1);
/*InsertBlankSixtyfourthNote insert_blankchord_6key*/
SCM scheme_InsertBlankSixtyfourthNote (SCM optional);
install_scm_function ("d-InsertBlankSixtyfourthNote", scheme_InsertBlankSixtyfourthNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertBlankSixtyfourthNote")), "scm", (gpointer) 1);
/*InsertBlankOneHundredTwentyEighthNote insert_blankchord_7key*/
SCM scheme_InsertBlankOneHundredTwentyEighthNote (SCM optional);
install_scm_function ("d-InsertBlankOneHundredTwentyEighthNote", scheme_InsertBlankOneHundredTwentyEighthNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertBlankOneHundredTwentyEighthNote")), "scm", (gpointer) 1);
/*InsertBlankTwoHundredFiftySixthNote insert_blankchord_8key*/
SCM scheme_InsertBlankTwoHundredFiftySixthNote (SCM optional);
install_scm_function ("d-InsertBlankTwoHundredFiftySixthNote", scheme_InsertBlankTwoHundredFiftySixthNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertBlankTwoHundredFiftySixthNote")), "scm", (gpointer) 1);
/*ToggleRestMode rest_toggle_key*/
SCM scheme_ToggleRestMode (SCM optional);
install_scm_function ("d-ToggleRestMode", scheme_ToggleRestMode);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ToggleRestMode")), "scm", (gpointer) 1);
/*ToggleBlankMode toggle_blank*/
SCM scheme_ToggleBlankMode (SCM optional);
install_scm_function ("d-ToggleBlankMode", scheme_ToggleBlankMode);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ToggleBlankMode")), "scm", (gpointer) 1);
/*InsertDuplet insert_duplet*/
SCM scheme_InsertDuplet (SCM optional);
install_scm_function ("d-InsertDuplet", scheme_InsertDuplet);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertDuplet")), "scm", (gpointer) 1);
/*InsertTriplet insert_triplet*/
SCM scheme_InsertTriplet (SCM optional);
install_scm_function ("d-InsertTriplet", scheme_InsertTriplet);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertTriplet")), "scm", (gpointer) 1);
/*StartTriplet start_triplet*/
SCM scheme_StartTriplet (SCM optional);
install_scm_function ("d-StartTriplet", scheme_StartTriplet);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "StartTriplet")), "scm", (gpointer) 1);
/*EndTuplet end_tuplet*/
SCM scheme_EndTuplet (SCM optional);
install_scm_function ("d-EndTuplet", scheme_EndTuplet);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "EndTuplet")), "scm", (gpointer) 1);
/*InsertQuadtuplet insert_quadtuplet*/
SCM scheme_InsertQuadtuplet (SCM optional);
install_scm_function ("d-InsertQuadtuplet", scheme_InsertQuadtuplet);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertQuadtuplet")), "scm", (gpointer) 1);
/*InsertQuintuplet insert_quintuplet*/
SCM scheme_InsertQuintuplet (SCM optional);
install_scm_function ("d-InsertQuintuplet", scheme_InsertQuintuplet);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertQuintuplet")), "scm", (gpointer) 1);
/*InsertSextuplet insert_sextuplet*/
SCM scheme_InsertSextuplet (SCM optional);
install_scm_function ("d-InsertSextuplet", scheme_InsertSextuplet);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertSextuplet")), "scm", (gpointer) 1);
/*InsertSeptuplet insert_septuplet*/
SCM scheme_InsertSeptuplet (SCM optional);
install_scm_function ("d-InsertSeptuplet", scheme_InsertSeptuplet);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertSeptuplet")), "scm", (gpointer) 1);
/*AddNoteToChord add_tone_key*/
SCM scheme_AddNoteToChord (SCM optional);
install_scm_function ("d-AddNoteToChord", scheme_AddNoteToChord);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddNoteToChord")), "scm", (gpointer) 1);
/*RemoveNoteFromChord remove_tone_key*/
SCM scheme_RemoveNoteFromChord (SCM optional);
install_scm_function ("d-RemoveNoteFromChord", scheme_RemoveNoteFromChord);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "RemoveNoteFromChord")), "scm", (gpointer) 1);
/*Sharpen sharpen_key*/
SCM scheme_Sharpen (SCM optional);
install_scm_function ("d-Sharpen", scheme_Sharpen);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Sharpen")), "scm", (gpointer) 1);
/*Flatten flatten_key*/
SCM scheme_Flatten (SCM optional);
install_scm_function ("d-Flatten", scheme_Flatten);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Flatten")), "scm", (gpointer) 1);
/*PendingSharpen pending_sharpen*/
SCM scheme_PendingSharpen (SCM optional);
install_scm_function ("d-PendingSharpen", scheme_PendingSharpen);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "PendingSharpen")), "scm", (gpointer) 1);
/*PendingFlatten pending_flatten*/
SCM scheme_PendingFlatten (SCM optional);
install_scm_function ("d-PendingFlatten", scheme_PendingFlatten);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "PendingFlatten")), "scm", (gpointer) 1);
/*StemUp stem_up*/
SCM scheme_StemUp (SCM optional);
install_scm_function ("d-StemUp", scheme_StemUp);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "StemUp")), "scm", (gpointer) 1);
/*StemDown stem_down*/
SCM scheme_StemDown (SCM optional);
install_scm_function ("d-StemDown", scheme_StemDown);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "StemDown")), "scm", (gpointer) 1);
/*AddDot add_dot_key*/
SCM scheme_AddDot (SCM optional);
install_scm_function ("d-AddDot", scheme_AddDot);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddDot")), "scm", (gpointer) 1);
/*RemoveDot remove_dot_key*/
SCM scheme_RemoveDot (SCM optional);
install_scm_function ("d-RemoveDot", scheme_RemoveDot);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "RemoveDot")), "scm", (gpointer) 1);
/*InsertTiedNote tie_notes_key*/
SCM scheme_InsertTiedNote (SCM optional);
install_scm_function ("d-InsertTiedNote", scheme_InsertTiedNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertTiedNote")), "scm", (gpointer) 1);
/*ToggleTie toggle_tie*/
SCM scheme_ToggleTie (SCM optional);
install_scm_function ("d-ToggleTie", scheme_ToggleTie);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ToggleTie")), "scm", (gpointer) 1);
/*DeleteObject deleteobject*/
SCM scheme_DeleteObject (SCM optional);
install_scm_function ("d-DeleteObject", scheme_DeleteObject);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "DeleteObject")), "scm", (gpointer) 1);
/*DeletePreviousObject deletepreviousobject*/
SCM scheme_DeletePreviousObject (SCM optional);
install_scm_function ("d-DeletePreviousObject", scheme_DeletePreviousObject);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "DeletePreviousObject")), "scm", (gpointer) 1);
/*InsertMeasure insert_measure_key*/
SCM scheme_InsertMeasure (SCM optional);
install_scm_function ("d-InsertMeasure", scheme_InsertMeasure);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertMeasure")), "scm", (gpointer) 1);
/*AddMeasure addmeasureafter*/
SCM scheme_AddMeasure (SCM optional);
install_scm_function ("d-AddMeasure", scheme_AddMeasure);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddMeasure")), "scm", (gpointer) 1);
/*InsertMeasureBefore insertmeasurebefore*/
SCM scheme_InsertMeasureBefore (SCM optional);
install_scm_function ("d-InsertMeasureBefore", scheme_InsertMeasureBefore);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertMeasureBefore")), "scm", (gpointer) 1);
/*InsertMeasureAfter insertmeasureafter*/
SCM scheme_InsertMeasureAfter (SCM optional);
install_scm_function ("d-InsertMeasureAfter", scheme_InsertMeasureAfter);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertMeasureAfter")), "scm", (gpointer) 1);
/*AppendMeasure append_measure_key*/
SCM scheme_AppendMeasure (SCM optional);
install_scm_function ("d-AppendMeasure", scheme_AppendMeasure);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AppendMeasure")), "scm", (gpointer) 1);
/*DeleteMeasure deletemeasure*/
SCM scheme_DeleteMeasure (SCM optional);
install_scm_function ("d-DeleteMeasure", scheme_DeleteMeasure);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "DeleteMeasure")), "scm", (gpointer) 1);
/*DeleteMeasureAllStaffs deletemeasureallstaffs*/
SCM scheme_DeleteMeasureAllStaffs (SCM optional);
install_scm_function ("d-DeleteMeasureAllStaffs", scheme_DeleteMeasureAllStaffs);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "DeleteMeasureAllStaffs")), "scm", (gpointer) 1);
/*ShrinkMeasures adjust_measure_less_width_key*/
SCM scheme_ShrinkMeasures (SCM optional);
install_scm_function ("d-ShrinkMeasures", scheme_ShrinkMeasures);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ShrinkMeasures")), "scm", (gpointer) 1);
/*WidenMeasures adjust_measure_more_width_key*/
SCM scheme_WidenMeasures (SCM optional);
install_scm_function ("d-WidenMeasures", scheme_WidenMeasures);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "WidenMeasures")), "scm", (gpointer) 1);
/*ShorterStaffs adjust_staff_less_height_key*/
SCM scheme_ShorterStaffs (SCM optional);
install_scm_function ("d-ShorterStaffs", scheme_ShorterStaffs);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ShorterStaffs")), "scm", (gpointer) 1);
/*TallerStaffs adjust_staff_more_height_key*/
SCM scheme_TallerStaffs (SCM optional);
install_scm_function ("d-TallerStaffs", scheme_TallerStaffs);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "TallerStaffs")), "scm", (gpointer) 1);
/*InsertTrebleClef newcleftreble*/
SCM scheme_InsertTrebleClef (SCM optional);
install_scm_function ("d-InsertTrebleClef", scheme_InsertTrebleClef);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertTrebleClef")), "scm", (gpointer) 1);
/*InsertBassClef newclefbass*/
SCM scheme_InsertBassClef (SCM optional);
install_scm_function ("d-InsertBassClef", scheme_InsertBassClef);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertBassClef")), "scm", (gpointer) 1);
/*Insertg8clef newclefg8*/
SCM scheme_Insertg8clef (SCM optional);
install_scm_function ("d-Insertg8clef", scheme_Insertg8clef);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insertg8clef")), "scm", (gpointer) 1);
/*InsertAltoClef newclefalto*/
SCM scheme_InsertAltoClef (SCM optional);
install_scm_function ("d-InsertAltoClef", scheme_InsertAltoClef);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertAltoClef")), "scm", (gpointer) 1);
/*InsertTenorClef newcleftenor*/
SCM scheme_InsertTenorClef (SCM optional);
install_scm_function ("d-InsertTenorClef", scheme_InsertTenorClef);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertTenorClef")), "scm", (gpointer) 1);
/*InsertSopranoClef newclefsoprano*/
SCM scheme_InsertSopranoClef (SCM optional);
install_scm_function ("d-InsertSopranoClef", scheme_InsertSopranoClef);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertSopranoClef")), "scm", (gpointer) 1);
/*SetInitialTrebleClef setcleftreble*/
SCM scheme_SetInitialTrebleClef (SCM optional);
install_scm_function ("d-SetInitialTrebleClef", scheme_SetInitialTrebleClef);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialTrebleClef")), "scm", (gpointer) 1);
/*SetInitialBassClef setclefbass*/
SCM scheme_SetInitialBassClef (SCM optional);
install_scm_function ("d-SetInitialBassClef", scheme_SetInitialBassClef);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialBassClef")), "scm", (gpointer) 1);
/*SetInitialg8clef setclefg8*/
SCM scheme_SetInitialg8clef (SCM optional);
install_scm_function ("d-SetInitialg8clef", scheme_SetInitialg8clef);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialg8clef")), "scm", (gpointer) 1);
/*SetInitialAltoClef setclefalto*/
SCM scheme_SetInitialAltoClef (SCM optional);
install_scm_function ("d-SetInitialAltoClef", scheme_SetInitialAltoClef);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialAltoClef")), "scm", (gpointer) 1);
/*SetInitialTenorClef setcleftenor*/
SCM scheme_SetInitialTenorClef (SCM optional);
install_scm_function ("d-SetInitialTenorClef", scheme_SetInitialTenorClef);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialTenorClef")), "scm", (gpointer) 1);
/*SetInitialSopranoClef setclefsoprano*/
SCM scheme_SetInitialSopranoClef (SCM optional);
install_scm_function ("d-SetInitialSopranoClef", scheme_SetInitialSopranoClef);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialSopranoClef")), "scm", (gpointer) 1);
/*Insert22Time newtimesig22*/
SCM scheme_Insert22Time (SCM optional);
install_scm_function ("d-Insert22Time", scheme_Insert22Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert22Time")), "scm", (gpointer) 1);
/*Insert32Time newtimesig32*/
SCM scheme_Insert32Time (SCM optional);
install_scm_function ("d-Insert32Time", scheme_Insert32Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert32Time")), "scm", (gpointer) 1);
/*Insert42Time newtimesig42*/
SCM scheme_Insert42Time (SCM optional);
install_scm_function ("d-Insert42Time", scheme_Insert42Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert42Time")), "scm", (gpointer) 1);
/*Insert44Time newtimesig44*/
SCM scheme_Insert44Time (SCM optional);
install_scm_function ("d-Insert44Time", scheme_Insert44Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert44Time")), "scm", (gpointer) 1);
/*Insert34Time newtimesig34*/
SCM scheme_Insert34Time (SCM optional);
install_scm_function ("d-Insert34Time", scheme_Insert34Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert34Time")), "scm", (gpointer) 1);
/*Insert24Time newtimesig24*/
SCM scheme_Insert24Time (SCM optional);
install_scm_function ("d-Insert24Time", scheme_Insert24Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert24Time")), "scm", (gpointer) 1);
/*Insert64Time newtimesig64*/
SCM scheme_Insert64Time (SCM optional);
install_scm_function ("d-Insert64Time", scheme_Insert64Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert64Time")), "scm", (gpointer) 1);
/*Insert38Time newtimesig38*/
SCM scheme_Insert38Time (SCM optional);
install_scm_function ("d-Insert38Time", scheme_Insert38Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert38Time")), "scm", (gpointer) 1);
/*Insert68Time newtimesig68*/
SCM scheme_Insert68Time (SCM optional);
install_scm_function ("d-Insert68Time", scheme_Insert68Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert68Time")), "scm", (gpointer) 1);
/*Insert128Time newtimesig128*/
SCM scheme_Insert128Time (SCM optional);
install_scm_function ("d-Insert128Time", scheme_Insert128Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert128Time")), "scm", (gpointer) 1);
/*Insert98Time newtimesig98*/
SCM scheme_Insert98Time (SCM optional);
install_scm_function ("d-Insert98Time", scheme_Insert98Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert98Time")), "scm", (gpointer) 1);
/*Set22Time settimesig22*/
SCM scheme_Set22Time (SCM optional);
install_scm_function ("d-Set22Time", scheme_Set22Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set22Time")), "scm", (gpointer) 1);
/*Set32Time settimesig32*/
SCM scheme_Set32Time (SCM optional);
install_scm_function ("d-Set32Time", scheme_Set32Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set32Time")), "scm", (gpointer) 1);
/*Set42Time settimesig42*/
SCM scheme_Set42Time (SCM optional);
install_scm_function ("d-Set42Time", scheme_Set42Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set42Time")), "scm", (gpointer) 1);
/*Set44Time settimesig44*/
SCM scheme_Set44Time (SCM optional);
install_scm_function ("d-Set44Time", scheme_Set44Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set44Time")), "scm", (gpointer) 1);
/*Set34Time settimesig34*/
SCM scheme_Set34Time (SCM optional);
install_scm_function ("d-Set34Time", scheme_Set34Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set34Time")), "scm", (gpointer) 1);
/*Set24Time settimesig24*/
SCM scheme_Set24Time (SCM optional);
install_scm_function ("d-Set24Time", scheme_Set24Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set24Time")), "scm", (gpointer) 1);
/*Set64Time settimesig64*/
SCM scheme_Set64Time (SCM optional);
install_scm_function ("d-Set64Time", scheme_Set64Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set64Time")), "scm", (gpointer) 1);
/*Set38Time settimesig38*/
SCM scheme_Set38Time (SCM optional);
install_scm_function ("d-Set38Time", scheme_Set38Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set38Time")), "scm", (gpointer) 1);
/*Set68Time settimesig68*/
SCM scheme_Set68Time (SCM optional);
install_scm_function ("d-Set68Time", scheme_Set68Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set68Time")), "scm", (gpointer) 1);
/*Set128Time settimesig128*/
SCM scheme_Set128Time (SCM optional);
install_scm_function ("d-Set128Time", scheme_Set128Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set128Time")), "scm", (gpointer) 1);
/*Set98Time settimesig98*/
SCM scheme_Set98Time (SCM optional);
install_scm_function ("d-Set98Time", scheme_Set98Time);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set98Time")), "scm", (gpointer) 1);
/*InsertCmaj newkeysigcmaj*/
SCM scheme_InsertCmaj (SCM optional);
install_scm_function ("d-InsertCmaj", scheme_InsertCmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertCmaj")), "scm", (gpointer) 1);
/*InsertGmaj newkeysiggmaj*/
SCM scheme_InsertGmaj (SCM optional);
install_scm_function ("d-InsertGmaj", scheme_InsertGmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertGmaj")), "scm", (gpointer) 1);
/*InsertDmaj newkeysigdmaj*/
SCM scheme_InsertDmaj (SCM optional);
install_scm_function ("d-InsertDmaj", scheme_InsertDmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertDmaj")), "scm", (gpointer) 1);
/*InsertAmaj newkeysigamaj*/
SCM scheme_InsertAmaj (SCM optional);
install_scm_function ("d-InsertAmaj", scheme_InsertAmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertAmaj")), "scm", (gpointer) 1);
/*InsertEmaj newkeysigemaj*/
SCM scheme_InsertEmaj (SCM optional);
install_scm_function ("d-InsertEmaj", scheme_InsertEmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertEmaj")), "scm", (gpointer) 1);
/*InsertBmaj newkeysigbmaj*/
SCM scheme_InsertBmaj (SCM optional);
install_scm_function ("d-InsertBmaj", scheme_InsertBmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertBmaj")), "scm", (gpointer) 1);
/*InsertFSharpmaj newkeysigfsharpmaj*/
SCM scheme_InsertFSharpmaj (SCM optional);
install_scm_function ("d-InsertFSharpmaj", scheme_InsertFSharpmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertFSharpmaj")), "scm", (gpointer) 1);
/*InsertCSharpmaj newkeysigcsharpmaj*/
SCM scheme_InsertCSharpmaj (SCM optional);
install_scm_function ("d-InsertCSharpmaj", scheme_InsertCSharpmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertCSharpmaj")), "scm", (gpointer) 1);
/*InsertFmaj newkeysigfmaj*/
SCM scheme_InsertFmaj (SCM optional);
install_scm_function ("d-InsertFmaj", scheme_InsertFmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertFmaj")), "scm", (gpointer) 1);
/*InsertBflatmaj newkeysigbflatmaj*/
SCM scheme_InsertBflatmaj (SCM optional);
install_scm_function ("d-InsertBflatmaj", scheme_InsertBflatmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertBflatmaj")), "scm", (gpointer) 1);
/*InsertEflatmaj newkeysigeflatmaj*/
SCM scheme_InsertEflatmaj (SCM optional);
install_scm_function ("d-InsertEflatmaj", scheme_InsertEflatmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertEflatmaj")), "scm", (gpointer) 1);
/*InsertAflatmaj newkeysigaflatmaj*/
SCM scheme_InsertAflatmaj (SCM optional);
install_scm_function ("d-InsertAflatmaj", scheme_InsertAflatmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertAflatmaj")), "scm", (gpointer) 1);
/*InsertDflatmaj newkeysigdflatmaj*/
SCM scheme_InsertDflatmaj (SCM optional);
install_scm_function ("d-InsertDflatmaj", scheme_InsertDflatmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertDflatmaj")), "scm", (gpointer) 1);
/*InsertGflatmaj newkeysiggflatmaj*/
SCM scheme_InsertGflatmaj (SCM optional);
install_scm_function ("d-InsertGflatmaj", scheme_InsertGflatmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertGflatmaj")), "scm", (gpointer) 1);
/*InsertCflatmaj newkeysigcflatmaj*/
SCM scheme_InsertCflatmaj (SCM optional);
install_scm_function ("d-InsertCflatmaj", scheme_InsertCflatmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertCflatmaj")), "scm", (gpointer) 1);
/*InsertAmin newkeysigamin*/
SCM scheme_InsertAmin (SCM optional);
install_scm_function ("d-InsertAmin", scheme_InsertAmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertAmin")), "scm", (gpointer) 1);
/*InsertEmin newkeysigemin*/
SCM scheme_InsertEmin (SCM optional);
install_scm_function ("d-InsertEmin", scheme_InsertEmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertEmin")), "scm", (gpointer) 1);
/*InsertBmin newkeysigbmin*/
SCM scheme_InsertBmin (SCM optional);
install_scm_function ("d-InsertBmin", scheme_InsertBmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertBmin")), "scm", (gpointer) 1);
/*InsertFSharpmin newkeysigfsharpmin*/
SCM scheme_InsertFSharpmin (SCM optional);
install_scm_function ("d-InsertFSharpmin", scheme_InsertFSharpmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertFSharpmin")), "scm", (gpointer) 1);
/*InsertCSharpmin newkeysigcsharpmin*/
SCM scheme_InsertCSharpmin (SCM optional);
install_scm_function ("d-InsertCSharpmin", scheme_InsertCSharpmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertCSharpmin")), "scm", (gpointer) 1);
/*InsertGSharpmin newkeysiggsharpmin*/
SCM scheme_InsertGSharpmin (SCM optional);
install_scm_function ("d-InsertGSharpmin", scheme_InsertGSharpmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertGSharpmin")), "scm", (gpointer) 1);
/*InsertDSharpmin newkeysigdsharpmin*/
SCM scheme_InsertDSharpmin (SCM optional);
install_scm_function ("d-InsertDSharpmin", scheme_InsertDSharpmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertDSharpmin")), "scm", (gpointer) 1);
/*InsertASharpmin newkeysigasharpmin*/
SCM scheme_InsertASharpmin (SCM optional);
install_scm_function ("d-InsertASharpmin", scheme_InsertASharpmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertASharpmin")), "scm", (gpointer) 1);
/*InsertDmin newkeysigdmin*/
SCM scheme_InsertDmin (SCM optional);
install_scm_function ("d-InsertDmin", scheme_InsertDmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertDmin")), "scm", (gpointer) 1);
/*InsertGmin newkeysiggmin*/
SCM scheme_InsertGmin (SCM optional);
install_scm_function ("d-InsertGmin", scheme_InsertGmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertGmin")), "scm", (gpointer) 1);
/*InsertCmin newkeysigcmin*/
SCM scheme_InsertCmin (SCM optional);
install_scm_function ("d-InsertCmin", scheme_InsertCmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertCmin")), "scm", (gpointer) 1);
/*InsertFmin newkeysigfmin*/
SCM scheme_InsertFmin (SCM optional);
install_scm_function ("d-InsertFmin", scheme_InsertFmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertFmin")), "scm", (gpointer) 1);
/*InsertBflatmin newkeysigbflatmin*/
SCM scheme_InsertBflatmin (SCM optional);
install_scm_function ("d-InsertBflatmin", scheme_InsertBflatmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertBflatmin")), "scm", (gpointer) 1);
/*InsertEflatmin newkeysigeflatmin*/
SCM scheme_InsertEflatmin (SCM optional);
install_scm_function ("d-InsertEflatmin", scheme_InsertEflatmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertEflatmin")), "scm", (gpointer) 1);
/*InsertAflatmin newkeysigaflatmin*/
SCM scheme_InsertAflatmin (SCM optional);
install_scm_function ("d-InsertAflatmin", scheme_InsertAflatmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertAflatmin")), "scm", (gpointer) 1);
/*SetInitialCmaj setkeysigcmaj*/
SCM scheme_SetInitialCmaj (SCM optional);
install_scm_function ("d-SetInitialCmaj", scheme_SetInitialCmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialCmaj")), "scm", (gpointer) 1);
/*SetInitialGmaj setkeysiggmaj*/
SCM scheme_SetInitialGmaj (SCM optional);
install_scm_function ("d-SetInitialGmaj", scheme_SetInitialGmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialGmaj")), "scm", (gpointer) 1);
/*SetInitialDmaj setkeysigdmaj*/
SCM scheme_SetInitialDmaj (SCM optional);
install_scm_function ("d-SetInitialDmaj", scheme_SetInitialDmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialDmaj")), "scm", (gpointer) 1);
/*SetInitialAmaj setkeysigamaj*/
SCM scheme_SetInitialAmaj (SCM optional);
install_scm_function ("d-SetInitialAmaj", scheme_SetInitialAmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialAmaj")), "scm", (gpointer) 1);
/*SetInitialEmaj setkeysigemaj*/
SCM scheme_SetInitialEmaj (SCM optional);
install_scm_function ("d-SetInitialEmaj", scheme_SetInitialEmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialEmaj")), "scm", (gpointer) 1);
/*SetInitialBmaj setkeysigbmaj*/
SCM scheme_SetInitialBmaj (SCM optional);
install_scm_function ("d-SetInitialBmaj", scheme_SetInitialBmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialBmaj")), "scm", (gpointer) 1);
/*SetInitialFSharpmaj setkeysigfsharpmaj*/
SCM scheme_SetInitialFSharpmaj (SCM optional);
install_scm_function ("d-SetInitialFSharpmaj", scheme_SetInitialFSharpmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialFSharpmaj")), "scm", (gpointer) 1);
/*SetInitialCSharpmaj setkeysigcsharpmaj*/
SCM scheme_SetInitialCSharpmaj (SCM optional);
install_scm_function ("d-SetInitialCSharpmaj", scheme_SetInitialCSharpmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialCSharpmaj")), "scm", (gpointer) 1);
/*SetInitialFmaj setkeysigfmaj*/
SCM scheme_SetInitialFmaj (SCM optional);
install_scm_function ("d-SetInitialFmaj", scheme_SetInitialFmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialFmaj")), "scm", (gpointer) 1);
/*SetInitialBflatmaj setkeysigbflatmaj*/
SCM scheme_SetInitialBflatmaj (SCM optional);
install_scm_function ("d-SetInitialBflatmaj", scheme_SetInitialBflatmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialBflatmaj")), "scm", (gpointer) 1);
/*SetInitialEflatmaj setkeysigeflatmaj*/
SCM scheme_SetInitialEflatmaj (SCM optional);
install_scm_function ("d-SetInitialEflatmaj", scheme_SetInitialEflatmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialEflatmaj")), "scm", (gpointer) 1);
/*SetInitialAflatmaj setkeysigaflatmaj*/
SCM scheme_SetInitialAflatmaj (SCM optional);
install_scm_function ("d-SetInitialAflatmaj", scheme_SetInitialAflatmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialAflatmaj")), "scm", (gpointer) 1);
/*SetInitialDflatmaj setkeysigdflatmaj*/
SCM scheme_SetInitialDflatmaj (SCM optional);
install_scm_function ("d-SetInitialDflatmaj", scheme_SetInitialDflatmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialDflatmaj")), "scm", (gpointer) 1);
/*SetInitialGflatmaj setkeysiggflatmaj*/
SCM scheme_SetInitialGflatmaj (SCM optional);
install_scm_function ("d-SetInitialGflatmaj", scheme_SetInitialGflatmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialGflatmaj")), "scm", (gpointer) 1);
/*SetInitialCflatmaj setkeysigcflatmaj*/
SCM scheme_SetInitialCflatmaj (SCM optional);
install_scm_function ("d-SetInitialCflatmaj", scheme_SetInitialCflatmaj);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialCflatmaj")), "scm", (gpointer) 1);
/*SetInitialAmin setkeysigamin*/
SCM scheme_SetInitialAmin (SCM optional);
install_scm_function ("d-SetInitialAmin", scheme_SetInitialAmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialAmin")), "scm", (gpointer) 1);
/*SetInitialEmin setkeysigemin*/
SCM scheme_SetInitialEmin (SCM optional);
install_scm_function ("d-SetInitialEmin", scheme_SetInitialEmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialEmin")), "scm", (gpointer) 1);
/*SetInitialBmin setkeysigbmin*/
SCM scheme_SetInitialBmin (SCM optional);
install_scm_function ("d-SetInitialBmin", scheme_SetInitialBmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialBmin")), "scm", (gpointer) 1);
/*SetInitialFSharpmin setkeysigfsharpmin*/
SCM scheme_SetInitialFSharpmin (SCM optional);
install_scm_function ("d-SetInitialFSharpmin", scheme_SetInitialFSharpmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialFSharpmin")), "scm", (gpointer) 1);
/*SetInitialCSharpmin setkeysigcsharpmin*/
SCM scheme_SetInitialCSharpmin (SCM optional);
install_scm_function ("d-SetInitialCSharpmin", scheme_SetInitialCSharpmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialCSharpmin")), "scm", (gpointer) 1);
/*SetInitialGSharpmin setkeysiggsharpmin*/
SCM scheme_SetInitialGSharpmin (SCM optional);
install_scm_function ("d-SetInitialGSharpmin", scheme_SetInitialGSharpmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialGSharpmin")), "scm", (gpointer) 1);
/*SetInitialDSharpmin setkeysigdsharpmin*/
SCM scheme_SetInitialDSharpmin (SCM optional);
install_scm_function ("d-SetInitialDSharpmin", scheme_SetInitialDSharpmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialDSharpmin")), "scm", (gpointer) 1);
/*SetInitialASharpmin setkeysigasharpmin*/
SCM scheme_SetInitialASharpmin (SCM optional);
install_scm_function ("d-SetInitialASharpmin", scheme_SetInitialASharpmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialASharpmin")), "scm", (gpointer) 1);
/*SetInitialDmin setkeysigdmin*/
SCM scheme_SetInitialDmin (SCM optional);
install_scm_function ("d-SetInitialDmin", scheme_SetInitialDmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialDmin")), "scm", (gpointer) 1);
/*SetInitialGmin setkeysiggmin*/
SCM scheme_SetInitialGmin (SCM optional);
install_scm_function ("d-SetInitialGmin", scheme_SetInitialGmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialGmin")), "scm", (gpointer) 1);
/*SetInitialCmin setkeysigcmin*/
SCM scheme_SetInitialCmin (SCM optional);
install_scm_function ("d-SetInitialCmin", scheme_SetInitialCmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialCmin")), "scm", (gpointer) 1);
/*SetInitialFmin setkeysigfmin*/
SCM scheme_SetInitialFmin (SCM optional);
install_scm_function ("d-SetInitialFmin", scheme_SetInitialFmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialFmin")), "scm", (gpointer) 1);
/*SetInitialBflatmin setkeysigbflatmin*/
SCM scheme_SetInitialBflatmin (SCM optional);
install_scm_function ("d-SetInitialBflatmin", scheme_SetInitialBflatmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialBflatmin")), "scm", (gpointer) 1);
/*SetInitialEflatmin setkeysigeflatmin*/
SCM scheme_SetInitialEflatmin (SCM optional);
install_scm_function ("d-SetInitialEflatmin", scheme_SetInitialEflatmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialEflatmin")), "scm", (gpointer) 1);
/*SetInitialAflatmin setkeysigaflatmin*/
SCM scheme_SetInitialAflatmin (SCM optional);
install_scm_function ("d-SetInitialAflatmin", scheme_SetInitialAflatmin);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetInitialAflatmin")), "scm", (gpointer) 1);
/*SetMark set_mark*/
SCM scheme_SetMark (SCM optional);
install_scm_function ("d-SetMark", scheme_SetMark);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetMark")), "scm", (gpointer) 1);
/*UnsetMark unset_mark*/
SCM scheme_UnsetMark (SCM optional);
install_scm_function ("d-UnsetMark", scheme_UnsetMark);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "UnsetMark")), "scm", (gpointer) 1);
/*SetPoint set_point*/
SCM scheme_SetPoint (SCM optional);
install_scm_function ("d-SetPoint", scheme_SetPoint);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SetPoint")), "scm", (gpointer) 1);
/*ToggleBeginSlur toggle_begin_slur*/
SCM scheme_ToggleBeginSlur (SCM optional);
install_scm_function ("d-ToggleBeginSlur", scheme_ToggleBeginSlur);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ToggleBeginSlur")), "scm", (gpointer) 1);
/*ToggleEndSlur toggle_end_slur*/
SCM scheme_ToggleEndSlur (SCM optional);
install_scm_function ("d-ToggleEndSlur", scheme_ToggleEndSlur);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ToggleEndSlur")), "scm", (gpointer) 1);
/*ToggleStartCrescendo toggle_start_crescendo*/
SCM scheme_ToggleStartCrescendo (SCM optional);
install_scm_function ("d-ToggleStartCrescendo", scheme_ToggleStartCrescendo);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ToggleStartCrescendo")), "scm", (gpointer) 1);
/*ToggleEndCrescendo toggle_end_crescendo*/
SCM scheme_ToggleEndCrescendo (SCM optional);
install_scm_function ("d-ToggleEndCrescendo", scheme_ToggleEndCrescendo);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ToggleEndCrescendo")), "scm", (gpointer) 1);
/*ToggleStartDiminuendo toggle_start_diminuendo*/
SCM scheme_ToggleStartDiminuendo (SCM optional);
install_scm_function ("d-ToggleStartDiminuendo", scheme_ToggleStartDiminuendo);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ToggleStartDiminuendo")), "scm", (gpointer) 1);
/*ToggleEndDiminuendo toggle_end_diminuendo*/
SCM scheme_ToggleEndDiminuendo (SCM optional);
install_scm_function ("d-ToggleEndDiminuendo", scheme_ToggleEndDiminuendo);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ToggleEndDiminuendo")), "scm", (gpointer) 1);
/*ToggleGrace toggle_grace*/
SCM scheme_ToggleGrace (SCM optional);
install_scm_function ("d-ToggleGrace", scheme_ToggleGrace);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ToggleGrace")), "scm", (gpointer) 1);
/*ToggleAcciaccatura toggle_acciaccatura*/
SCM scheme_ToggleAcciaccatura (SCM optional);
install_scm_function ("d-ToggleAcciaccatura", scheme_ToggleAcciaccatura);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ToggleAcciaccatura")), "scm", (gpointer) 1);
/*ForceCaution force_cautionary*/
SCM scheme_ForceCaution (SCM optional);
install_scm_function ("d-ForceCaution", scheme_ForceCaution);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ForceCaution")), "scm", (gpointer) 1);
/*ChangePitch change_pitch*/
SCM scheme_ChangePitch (SCM optional);
install_scm_function ("d-ChangePitch", scheme_ChangePitch);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ChangePitch")), "scm", (gpointer) 1);
/*InsertRhythm insert_rhythm_pattern*/
SCM scheme_InsertRhythm (SCM optional);
install_scm_function ("d-InsertRhythm", scheme_InsertRhythm);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertRhythm")), "scm", (gpointer) 1);
/*NextRhythm nextrhythm*/
SCM scheme_NextRhythm (SCM optional);
install_scm_function ("d-NextRhythm", scheme_NextRhythm);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "NextRhythm")), "scm", (gpointer) 1);
/*AppendMeasureAllStaffs append_measure_score*/
SCM scheme_AppendMeasureAllStaffs (SCM optional);
install_scm_function ("d-AppendMeasureAllStaffs", scheme_AppendMeasureAllStaffs);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AppendMeasureAllStaffs")), "scm", (gpointer) 1);
/*ExecuteScheme execute_scheme*/
SCM scheme_ExecuteScheme (SCM optional);
install_scm_function ("d-ExecuteScheme", scheme_ExecuteScheme);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ExecuteScheme")), "scm", (gpointer) 1);
/*SharpenEnharmonicSet set_sharper*/
SCM scheme_SharpenEnharmonicSet (SCM optional);
install_scm_function ("d-SharpenEnharmonicSet", scheme_SharpenEnharmonicSet);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SharpenEnharmonicSet")), "scm", (gpointer) 1);
/*FlattenEnharmonicSet set_flatter*/
SCM scheme_FlattenEnharmonicSet (SCM optional);
install_scm_function ("d-FlattenEnharmonicSet", scheme_FlattenEnharmonicSet);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "FlattenEnharmonicSet")), "scm", (gpointer) 1);
/*New file_newwrapper*/
SCM scheme_New (SCM optional);
install_scm_function ("d-New", scheme_New);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "New")), "scm", (gpointer) 1);
/*NewScore new_score_cb*/
SCM scheme_NewScore (SCM optional);
install_scm_function ("d-NewScore", scheme_NewScore);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "NewScore")), "scm", (gpointer) 1);
/*Open file_open_with_check*/
SCM scheme_Open (SCM optional);
install_scm_function ("d-Open", scheme_Open);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Open")), "scm", (gpointer) 1);
/*ImportLilypond file_import_lilypond_with_check*/
SCM scheme_ImportLilypond (SCM optional);
install_scm_function ("d-ImportLilypond", scheme_ImportLilypond);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ImportLilypond")), "scm", (gpointer) 1);
/*ImportMidi file_import_midi_with_check*/
SCM scheme_ImportMidi (SCM optional);
install_scm_function ("d-ImportMidi", scheme_ImportMidi);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ImportMidi")), "scm", (gpointer) 1);
/*ImportMusicXml file_import_musicxml_with_check*/
SCM scheme_ImportMusicXml (SCM optional);
install_scm_function ("d-ImportMusicXml", scheme_ImportMusicXml);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ImportMusicXml")), "scm", (gpointer) 1);
/*AddStaffs file_add_staffs*/
SCM scheme_AddStaffs (SCM optional);
install_scm_function ("d-AddStaffs", scheme_AddStaffs);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddStaffs")), "scm", (gpointer) 1);
/*AddMovements file_add_movements*/
SCM scheme_AddMovements (SCM optional);
install_scm_function ("d-AddMovements", scheme_AddMovements);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddMovements")), "scm", (gpointer) 1);
/*MovementProps movement_props_dialog*/
SCM scheme_MovementProps (SCM optional);
install_scm_function ("d-MovementProps", scheme_MovementProps);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MovementProps")), "scm", (gpointer) 1);
/*OpenNewWindow openinnew*/
SCM scheme_OpenNewWindow (SCM optional);
install_scm_function ("d-OpenNewWindow", scheme_OpenNewWindow);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "OpenNewWindow")), "scm", (gpointer) 1);
/*Save file_savewrapper*/
SCM scheme_Save (SCM optional);
install_scm_function ("d-Save", scheme_Save);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Save")), "scm", (gpointer) 1);
/*SaveAs file_saveaswrapper*/
SCM scheme_SaveAs (SCM optional);
install_scm_function ("d-SaveAs", scheme_SaveAs);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SaveAs")), "scm", (gpointer) 1);
/*SaveCopy file_copy_save*/
SCM scheme_SaveCopy (SCM optional);
install_scm_function ("d-SaveCopy", scheme_SaveCopy);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SaveCopy")), "scm", (gpointer) 1);
/*OpenTemplate system_template_open_with_check*/
SCM scheme_OpenTemplate (SCM optional);
install_scm_function ("d-OpenTemplate", scheme_OpenTemplate);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "OpenTemplate")), "scm", (gpointer) 1);
/*OpenExample system_example_open_with_check*/
SCM scheme_OpenExample (SCM optional);
install_scm_function ("d-OpenExample", scheme_OpenExample);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "OpenExample")), "scm", (gpointer) 1);
/*OpenMyTemplate local_template_open_with_check*/
SCM scheme_OpenMyTemplate (SCM optional);
install_scm_function ("d-OpenMyTemplate", scheme_OpenMyTemplate);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "OpenMyTemplate")), "scm", (gpointer) 1);
/*SaveTemplate template_save*/
SCM scheme_SaveTemplate (SCM optional);
install_scm_function ("d-SaveTemplate", scheme_SaveTemplate);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SaveTemplate")), "scm", (gpointer) 1);
/*NewWindow newview*/
SCM scheme_NewWindow (SCM optional);
install_scm_function ("d-NewWindow", scheme_NewWindow);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "NewWindow")), "scm", (gpointer) 1);
/*InsertMovementBefore insert_movement_before*/
SCM scheme_InsertMovementBefore (SCM optional);
install_scm_function ("d-InsertMovementBefore", scheme_InsertMovementBefore);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertMovementBefore")), "scm", (gpointer) 1);
/*InsertMovementAfter insert_movement_after*/
SCM scheme_InsertMovementAfter (SCM optional);
install_scm_function ("d-InsertMovementAfter", scheme_InsertMovementAfter);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertMovementAfter")), "scm", (gpointer) 1);
/*NewMovement append_new_movement*/
SCM scheme_NewMovement (SCM optional);
install_scm_function ("d-NewMovement", scheme_NewMovement);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "NewMovement")), "scm", (gpointer) 1);
/*SaveParts file_savepartswrapper*/
SCM scheme_SaveParts (SCM optional);
install_scm_function ("d-SaveParts", scheme_SaveParts);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SaveParts")), "scm", (gpointer) 1);
/*ExportMUDELA export_mudela_action*/
SCM scheme_ExportMUDELA (SCM optional);
install_scm_function ("d-ExportMUDELA", scheme_ExportMUDELA);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ExportMUDELA")), "scm", (gpointer) 1);
/*ExportPDF export_pdf_action*/
SCM scheme_ExportPDF (SCM optional);
install_scm_function ("d-ExportPDF", scheme_ExportPDF);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ExportPDF")), "scm", (gpointer) 1);
/*ExportPNG export_png_action*/
SCM scheme_ExportPNG (SCM optional);
install_scm_function ("d-ExportPNG", scheme_ExportPNG);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ExportPNG")), "scm", (gpointer) 1);
/*ExportMIDI export_midi_action*/
SCM scheme_ExportMIDI (SCM optional);
install_scm_function ("d-ExportMIDI", scheme_ExportMIDI);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ExportMIDI")), "scm", (gpointer) 1);
/*PrintPreview printpreview_cb*/
SCM scheme_PrintPreview (SCM optional);
install_scm_function ("d-PrintPreview", scheme_PrintPreview);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "PrintPreview")), "scm", (gpointer) 1);
/*PrintView show_print_view*/
SCM scheme_PrintView (SCM optional);
install_scm_function ("d-PrintView", scheme_PrintView);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "PrintView")), "scm", (gpointer) 1);
/*PrintSelection printselection_cb*/
SCM scheme_PrintSelection (SCM optional);
install_scm_function ("d-PrintSelection", scheme_PrintSelection);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "PrintSelection")), "scm", (gpointer) 1);
/*PrintExcerptPreview printexcerptpreview_cb*/
SCM scheme_PrintExcerptPreview (SCM optional);
install_scm_function ("d-PrintExcerptPreview", scheme_PrintExcerptPreview);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "PrintExcerptPreview")), "scm", (gpointer) 1);
/*PrintMovement printmovement_cb*/
SCM scheme_PrintMovement (SCM optional);
install_scm_function ("d-PrintMovement", scheme_PrintMovement);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "PrintMovement")), "scm", (gpointer) 1);
/*Print printall_cb*/
SCM scheme_Print (SCM optional);
install_scm_function ("d-Print", scheme_Print);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Print")), "scm", (gpointer) 1);
/*PrintPart printpart_cb*/
SCM scheme_PrintPart (SCM optional);
install_scm_function ("d-PrintPart", scheme_PrintPart);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "PrintPart")), "scm", (gpointer) 1);
/*Close close_gui_with_check*/
SCM scheme_Close (SCM optional);
install_scm_function ("d-Close", scheme_Close);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Close")), "scm", (gpointer) 1);
/*Quit closewrapper*/
SCM scheme_Quit (SCM optional);
install_scm_function ("d-Quit", scheme_Quit);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Quit")), "scm", (gpointer) 1);
/*Undo undowrapper*/
SCM scheme_Undo (SCM optional);
install_scm_function ("d-Undo", scheme_Undo);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Undo")), "scm", (gpointer) 1);
/*Redo redowrapper*/
SCM scheme_Redo (SCM optional);
install_scm_function ("d-Redo", scheme_Redo);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Redo")), "scm", (gpointer) 1);
/*Copy copywrapper*/
SCM scheme_Copy (SCM optional);
install_scm_function ("d-Copy", scheme_Copy);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Copy")), "scm", (gpointer) 1);
/*Cut cutwrapper*/
SCM scheme_Cut (SCM optional);
install_scm_function ("d-Cut", scheme_Cut);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Cut")), "scm", (gpointer) 1);
/*Paste pastewrapper*/
SCM scheme_Paste (SCM optional);
install_scm_function ("d-Paste", scheme_Paste);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Paste")), "scm", (gpointer) 1);
/*PasteClipboard paste_clipboard*/
SCM scheme_PasteClipboard (SCM optional);
install_scm_function ("d-PasteClipboard", scheme_PasteClipboard);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "PasteClipboard")), "scm", (gpointer) 1);
/*ScoreProperties score_properties_dialog*/
SCM scheme_ScoreProperties (SCM optional);
install_scm_function ("d-ScoreProperties", scheme_ScoreProperties);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ScoreProperties")), "scm", (gpointer) 1);
/*SaveSelection saveselwrapper*/
SCM scheme_SaveSelection (SCM optional);
install_scm_function ("d-SaveSelection", scheme_SaveSelection);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SaveSelection")), "scm", (gpointer) 1);
/*Preferences preferences_change*/
SCM scheme_Preferences (SCM optional);
install_scm_function ("d-Preferences", scheme_Preferences);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Preferences")), "scm", (gpointer) 1);
/*SaveAccels save_default_keymap_file_wrapper*/
SCM scheme_SaveAccels (SCM optional);
install_scm_function ("d-SaveAccels", scheme_SaveAccels);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SaveAccels")), "scm", (gpointer) 1);
/*CommandManagement configure_keyboard_dialog*/
SCM scheme_CommandManagement (SCM optional);
install_scm_function ("d-CommandManagement", scheme_CommandManagement);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "CommandManagement")), "scm", (gpointer) 1);
/*SwapStaffs swapstaffs*/
SCM scheme_SwapStaffs (SCM optional);
install_scm_function ("d-SwapStaffs", scheme_SwapStaffs);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SwapStaffs")), "scm", (gpointer) 1);
/*SplitVoices splitstaffs*/
SCM scheme_SplitVoices (SCM optional);
install_scm_function ("d-SplitVoices", scheme_SplitVoices);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SplitVoices")), "scm", (gpointer) 1);
/*JoinVoices joinstaffs*/
SCM scheme_JoinVoices (SCM optional);
install_scm_function ("d-JoinVoices", scheme_JoinVoices);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "JoinVoices")), "scm", (gpointer) 1);
/*SwapMovements swapmovements*/
SCM scheme_SwapMovements (SCM optional);
install_scm_function ("d-SwapMovements", scheme_SwapMovements);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "SwapMovements")), "scm", (gpointer) 1);
/*VoiceUp voiceup*/
SCM scheme_VoiceUp (SCM optional);
install_scm_function ("d-VoiceUp", scheme_VoiceUp);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "VoiceUp")), "scm", (gpointer) 1);
/*VoiceDown voicedown*/
SCM scheme_VoiceDown (SCM optional);
install_scm_function ("d-VoiceDown", scheme_VoiceDown);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "VoiceDown")), "scm", (gpointer) 1);
/*MoveToVoiceUp movetovoiceup*/
SCM scheme_MoveToVoiceUp (SCM optional);
install_scm_function ("d-MoveToVoiceUp", scheme_MoveToVoiceUp);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoveToVoiceUp")), "scm", (gpointer) 1);
/*MoveToVoiceDown movetovoicedown*/
SCM scheme_MoveToVoiceDown (SCM optional);
install_scm_function ("d-MoveToVoiceDown", scheme_MoveToVoiceDown);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoveToVoiceDown")), "scm", (gpointer) 1);
/*AddBefore newstaffbefore*/
SCM scheme_AddBefore (SCM optional);
install_scm_function ("d-AddBefore", scheme_AddBefore);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddBefore")), "scm", (gpointer) 1);
/*AddAfter dnm_newstaffafter*/
SCM scheme_AddAfter (SCM optional);
install_scm_function ("d-AddAfter", scheme_AddAfter);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddAfter")), "scm", (gpointer) 1);
/*AddInitial newstaffinitial*/
SCM scheme_AddInitial (SCM optional);
install_scm_function ("d-AddInitial", scheme_AddInitial);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddInitial")), "scm", (gpointer) 1);
/*AddLast newstafflast*/
SCM scheme_AddLast (SCM optional);
install_scm_function ("d-AddLast", scheme_AddLast);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddLast")), "scm", (gpointer) 1);
/*DeleteBefore delete_staff_before*/
SCM scheme_DeleteBefore (SCM optional);
install_scm_function ("d-DeleteBefore", scheme_DeleteBefore);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "DeleteBefore")), "scm", (gpointer) 1);
/*DeleteStaff delete_staff_current*/
SCM scheme_DeleteStaff (SCM optional);
install_scm_function ("d-DeleteStaff", scheme_DeleteStaff);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "DeleteStaff")), "scm", (gpointer) 1);
/*DeleteAfter delete_staff_after*/
SCM scheme_DeleteAfter (SCM optional);
install_scm_function ("d-DeleteAfter", scheme_DeleteAfter);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "DeleteAfter")), "scm", (gpointer) 1);
/*AddVoice dnm_newstaffvoice*/
SCM scheme_AddVoice (SCM optional);
install_scm_function ("d-AddVoice", scheme_AddVoice);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddVoice")), "scm", (gpointer) 1);
/*StaffProperties staff_properties_change_cb*/
SCM scheme_StaffProperties (SCM optional);
install_scm_function ("d-StaffProperties", scheme_StaffProperties);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "StaffProperties")), "scm", (gpointer) 1);
/*InitialClef clef_change_initial*/
SCM scheme_InitialClef (SCM optional);
install_scm_function ("d-InitialClef", scheme_InitialClef);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InitialClef")), "scm", (gpointer) 1);
/*InsertClef clef_change_insert*/
SCM scheme_InsertClef (SCM optional);
install_scm_function ("d-InsertClef", scheme_InsertClef);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertClef")), "scm", (gpointer) 1);
/*InitialKey key_change_initial*/
SCM scheme_InitialKey (SCM optional);
install_scm_function ("d-InitialKey", scheme_InitialKey);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InitialKey")), "scm", (gpointer) 1);
/*InsertKey key_change_insert*/
SCM scheme_InsertKey (SCM optional);
install_scm_function ("d-InsertKey", scheme_InsertKey);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertKey")), "scm", (gpointer) 1);
/*InitialTimeSig timesig_change_initial*/
SCM scheme_InitialTimeSig (SCM optional);
install_scm_function ("d-InitialTimeSig", scheme_InitialTimeSig);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InitialTimeSig")), "scm", (gpointer) 1);
/*InsertTimeSig timesig_change_insert*/
SCM scheme_InsertTimeSig (SCM optional);
install_scm_function ("d-InsertTimeSig", scheme_InsertTimeSig);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertTimeSig")), "scm", (gpointer) 1);
/*ChangeNotehead set_notehead*/
SCM scheme_ChangeNotehead (SCM optional);
install_scm_function ("d-ChangeNotehead", scheme_ChangeNotehead);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ChangeNotehead")), "scm", (gpointer) 1);
/*InsertStem stem_directive_insert*/
SCM scheme_InsertStem (SCM optional);
install_scm_function ("d-InsertStem", scheme_InsertStem);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertStem")), "scm", (gpointer) 1);
/*AddVerse add_verse*/
SCM scheme_AddVerse (SCM optional);
install_scm_function ("d-AddVerse", scheme_AddVerse);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddVerse")), "scm", (gpointer) 1);
/*DeleteVerse delete_verse*/
SCM scheme_DeleteVerse (SCM optional);
install_scm_function ("d-DeleteVerse", scheme_DeleteVerse);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "DeleteVerse")), "scm", (gpointer) 1);
/*EditFiguredBass figure_insert*/
SCM scheme_EditFiguredBass (SCM optional);
install_scm_function ("d-EditFiguredBass", scheme_EditFiguredBass);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "EditFiguredBass")), "scm", (gpointer) 1);
/*DeleteFiguredBass delete_figured_bass*/
SCM scheme_DeleteFiguredBass (SCM optional);
install_scm_function ("d-DeleteFiguredBass", scheme_DeleteFiguredBass);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "DeleteFiguredBass")), "scm", (gpointer) 1);
/*HideFiguredBass hide_figured_bass*/
SCM scheme_HideFiguredBass (SCM optional);
install_scm_function ("d-HideFiguredBass", scheme_HideFiguredBass);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "HideFiguredBass")), "scm", (gpointer) 1);
/*ShowFiguredBass show_figured_bass*/
SCM scheme_ShowFiguredBass (SCM optional);
install_scm_function ("d-ShowFiguredBass", scheme_ShowFiguredBass);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ShowFiguredBass")), "scm", (gpointer) 1);
/*EditChords fakechord_insert*/
SCM scheme_EditChords (SCM optional);
install_scm_function ("d-EditChords", scheme_EditChords);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "EditChords")), "scm", (gpointer) 1);
/*InsertDynamic insert_dynamic*/
SCM scheme_InsertDynamic (SCM optional);
install_scm_function ("d-InsertDynamic", scheme_InsertDynamic);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertDynamic")), "scm", (gpointer) 1);
/*EditObject edit_object*/
SCM scheme_EditObject (SCM optional);
install_scm_function ("d-EditObject", scheme_EditObject);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "EditObject")), "scm", (gpointer) 1);
/*EditDirective edit_object_directive*/
SCM scheme_EditDirective (SCM optional);
install_scm_function ("d-EditDirective", scheme_EditDirective);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "EditDirective")), "scm", (gpointer) 1);
/*EditStaffDirective edit_staff_directive*/
SCM scheme_EditStaffDirective (SCM optional);
install_scm_function ("d-EditStaffDirective", scheme_EditStaffDirective);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "EditStaffDirective")), "scm", (gpointer) 1);
/*EditVoiceDirective edit_voice_directive*/
SCM scheme_EditVoiceDirective (SCM optional);
install_scm_function ("d-EditVoiceDirective", scheme_EditVoiceDirective);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "EditVoiceDirective")), "scm", (gpointer) 1);
/*EditScoreDirective edit_score_directive*/
SCM scheme_EditScoreDirective (SCM optional);
install_scm_function ("d-EditScoreDirective", scheme_EditScoreDirective);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "EditScoreDirective")), "scm", (gpointer) 1);
/*EditMovementDirective edit_movement_directive*/
SCM scheme_EditMovementDirective (SCM optional);
install_scm_function ("d-EditMovementDirective", scheme_EditMovementDirective);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "EditMovementDirective")), "scm", (gpointer) 1);
/*EditClefDirective edit_clef_directive*/
SCM scheme_EditClefDirective (SCM optional);
install_scm_function ("d-EditClefDirective", scheme_EditClefDirective);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "EditClefDirective")), "scm", (gpointer) 1);
/*EditTimesigDirective edit_timesig_directive*/
SCM scheme_EditTimesigDirective (SCM optional);
install_scm_function ("d-EditTimesigDirective", scheme_EditTimesigDirective);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "EditTimesigDirective")), "scm", (gpointer) 1);
/*EditKeysigDirective edit_keysig_directive*/
SCM scheme_EditKeysigDirective (SCM optional);
install_scm_function ("d-EditKeysigDirective", scheme_EditKeysigDirective);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "EditKeysigDirective")), "scm", (gpointer) 1);
/*DeleteDirective delete_chord_or_note_directive*/
SCM scheme_DeleteDirective (SCM optional);
install_scm_function ("d-DeleteDirective", scheme_DeleteDirective);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "DeleteDirective")), "scm", (gpointer) 1);
/*AttachLilyToNote note_directive*/
SCM scheme_AttachLilyToNote (SCM optional);
install_scm_function ("d-AttachLilyToNote", scheme_AttachLilyToNote);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AttachLilyToNote")), "scm", (gpointer) 1);
/*AttachLilyToChord chord_directive*/
SCM scheme_AttachLilyToChord (SCM optional);
install_scm_function ("d-AttachLilyToChord", scheme_AttachLilyToChord);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AttachLilyToChord")), "scm", (gpointer) 1);
/*InsertBarline insert_barline*/
SCM scheme_InsertBarline (SCM optional);
install_scm_function ("d-InsertBarline", scheme_InsertBarline);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertBarline")), "scm", (gpointer) 1);
/*GoToMeasure tomeasurenum*/
SCM scheme_GoToMeasure (SCM optional);
install_scm_function ("d-GoToMeasure", scheme_GoToMeasure);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "GoToMeasure")), "scm", (gpointer) 1);
/*GoToBeginning tohome*/
SCM scheme_GoToBeginning (SCM optional);
install_scm_function ("d-GoToBeginning", scheme_GoToBeginning);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "GoToBeginning")), "scm", (gpointer) 1);
/*GoToEnd toend*/
SCM scheme_GoToEnd (SCM optional);
install_scm_function ("d-GoToEnd", scheme_GoToEnd);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "GoToEnd")), "scm", (gpointer) 1);
/*MoveToBeginning movetostart*/
SCM scheme_MoveToBeginning (SCM optional);
install_scm_function ("d-MoveToBeginning", scheme_MoveToBeginning);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoveToBeginning")), "scm", (gpointer) 1);
/*MoveToEnd movetoend*/
SCM scheme_MoveToEnd (SCM optional);
install_scm_function ("d-MoveToEnd", scheme_MoveToEnd);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoveToEnd")), "scm", (gpointer) 1);
/*NextMovement next_movement*/
SCM scheme_NextMovement (SCM optional);
install_scm_function ("d-NextMovement", scheme_NextMovement);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "NextMovement")), "scm", (gpointer) 1);
/*PreviousMovement prev_movement*/
SCM scheme_PreviousMovement (SCM optional);
install_scm_function ("d-PreviousMovement", scheme_PreviousMovement);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "PreviousMovement")), "scm", (gpointer) 1);
/*DeleteMovement delete_movement*/
SCM scheme_DeleteMovement (SCM optional);
install_scm_function ("d-DeleteMovement", scheme_DeleteMovement);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "DeleteMovement")), "scm", (gpointer) 1);
/*Play ext_midi_playback*/
SCM scheme_Play (SCM optional);
install_scm_function ("d-Play", scheme_Play);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Play")), "scm", (gpointer) 1);
/*Stop stop_midi_playback*/
SCM scheme_Stop (SCM optional);
install_scm_function ("d-Stop", scheme_Stop);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Stop")), "scm", (gpointer) 1);
/*PlaybackProperties playback_properties_change*/
SCM scheme_PlaybackProperties (SCM optional);
install_scm_function ("d-PlaybackProperties", scheme_PlaybackProperties);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "PlaybackProperties")), "scm", (gpointer) 1);
/*Help browse_manual*/
SCM scheme_Help (SCM optional);
install_scm_function ("d-Help", scheme_Help);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Help")), "scm", (gpointer) 1);
/*About about*/
SCM scheme_About (SCM optional);
install_scm_function ("d-About", scheme_About);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "About")), "scm", (gpointer) 1);
/*MoreCommands morecommands*/
SCM scheme_MoreCommands (SCM optional);
install_scm_function ("d-MoreCommands", scheme_MoreCommands);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoreCommands")), "scm", (gpointer) 1);
/*MyCommands mycommands*/
SCM scheme_MyCommands (SCM optional);
install_scm_function ("d-MyCommands", scheme_MyCommands);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MyCommands")), "scm", (gpointer) 1);
/*FetchCommands fetchcommands*/
SCM scheme_FetchCommands (SCM optional);
install_scm_function ("d-FetchCommands", scheme_FetchCommands);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "FetchCommands")), "scm", (gpointer) 1);
/*ToggleEdit toggle_edit_mode*/
SCM scheme_ToggleEdit (SCM optional);
install_scm_function ("d-ToggleEdit", scheme_ToggleEdit);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ToggleEdit")), "scm", (gpointer) 1);
/*ToggleRest toggle_rest_mode*/
SCM scheme_ToggleRest (SCM optional);
install_scm_function ("d-ToggleRest", scheme_ToggleRest);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ToggleRest")), "scm", (gpointer) 1);
/*ToggleRhythm toggle_rhythm_mode*/
SCM scheme_ToggleRhythm (SCM optional);
install_scm_function ("d-ToggleRhythm", scheme_ToggleRhythm);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ToggleRhythm")), "scm", (gpointer) 1);
/*ClearOverlay clear_overlay*/
SCM scheme_ClearOverlay (SCM optional);
install_scm_function ("d-ClearOverlay", scheme_ClearOverlay);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ClearOverlay")), "scm", (gpointer) 1);
/*CreateRhythm create_rhythm_cb*/
SCM scheme_CreateRhythm (SCM optional);
install_scm_function ("d-CreateRhythm", scheme_CreateRhythm);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "CreateRhythm")), "scm", (gpointer) 1);
/*DeleteRhythm delete_rhythm_cb*/
SCM scheme_DeleteRhythm (SCM optional);
install_scm_function ("d-DeleteRhythm", scheme_DeleteRhythm);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "DeleteRhythm")), "scm", (gpointer) 1);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertA")), "scm", (gpointer) 1);
SCM scheme_InsertA (SCM optional);
install_scm_function ("d-InsertA", scheme_InsertA);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddNoteA")), "scm", (gpointer) 1);
SCM scheme_AddNoteA (SCM optional);
install_scm_function ("d-AddNoteA", scheme_AddNoteA);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddA")), "scm", (gpointer) 1);
SCM scheme_AddA (SCM optional);
install_scm_function ("d-AddA", scheme_AddA);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ChangeToA")), "scm", (gpointer) 1);
SCM scheme_ChangeToA (SCM optional);
install_scm_function ("d-ChangeToA", scheme_ChangeToA);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoveToA")), "scm", (gpointer) 1);
SCM scheme_MoveToA (SCM optional);
install_scm_function ("d-MoveToA", scheme_MoveToA);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertB")), "scm", (gpointer) 1);
SCM scheme_InsertB (SCM optional);
install_scm_function ("d-InsertB", scheme_InsertB);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddNoteB")), "scm", (gpointer) 1);
SCM scheme_AddNoteB (SCM optional);
install_scm_function ("d-AddNoteB", scheme_AddNoteB);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddB")), "scm", (gpointer) 1);
SCM scheme_AddB (SCM optional);
install_scm_function ("d-AddB", scheme_AddB);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ChangeToB")), "scm", (gpointer) 1);
SCM scheme_ChangeToB (SCM optional);
install_scm_function ("d-ChangeToB", scheme_ChangeToB);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoveToB")), "scm", (gpointer) 1);
SCM scheme_MoveToB (SCM optional);
install_scm_function ("d-MoveToB", scheme_MoveToB);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertC")), "scm", (gpointer) 1);
SCM scheme_InsertC (SCM optional);
install_scm_function ("d-InsertC", scheme_InsertC);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddNoteC")), "scm", (gpointer) 1);
SCM scheme_AddNoteC (SCM optional);
install_scm_function ("d-AddNoteC", scheme_AddNoteC);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddC")), "scm", (gpointer) 1);
SCM scheme_AddC (SCM optional);
install_scm_function ("d-AddC", scheme_AddC);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ChangeToC")), "scm", (gpointer) 1);
SCM scheme_ChangeToC (SCM optional);
install_scm_function ("d-ChangeToC", scheme_ChangeToC);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoveToC")), "scm", (gpointer) 1);
SCM scheme_MoveToC (SCM optional);
install_scm_function ("d-MoveToC", scheme_MoveToC);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertD")), "scm", (gpointer) 1);
SCM scheme_InsertD (SCM optional);
install_scm_function ("d-InsertD", scheme_InsertD);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddNoteD")), "scm", (gpointer) 1);
SCM scheme_AddNoteD (SCM optional);
install_scm_function ("d-AddNoteD", scheme_AddNoteD);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddD")), "scm", (gpointer) 1);
SCM scheme_AddD (SCM optional);
install_scm_function ("d-AddD", scheme_AddD);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ChangeToD")), "scm", (gpointer) 1);
SCM scheme_ChangeToD (SCM optional);
install_scm_function ("d-ChangeToD", scheme_ChangeToD);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoveToD")), "scm", (gpointer) 1);
SCM scheme_MoveToD (SCM optional);
install_scm_function ("d-MoveToD", scheme_MoveToD);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertE")), "scm", (gpointer) 1);
SCM scheme_InsertE (SCM optional);
install_scm_function ("d-InsertE", scheme_InsertE);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddNoteE")), "scm", (gpointer) 1);
SCM scheme_AddNoteE (SCM optional);
install_scm_function ("d-AddNoteE", scheme_AddNoteE);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddE")), "scm", (gpointer) 1);
SCM scheme_AddE (SCM optional);
install_scm_function ("d-AddE", scheme_AddE);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ChangeToE")), "scm", (gpointer) 1);
SCM scheme_ChangeToE (SCM optional);
install_scm_function ("d-ChangeToE", scheme_ChangeToE);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoveToE")), "scm", (gpointer) 1);
SCM scheme_MoveToE (SCM optional);
install_scm_function ("d-MoveToE", scheme_MoveToE);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertF")), "scm", (gpointer) 1);
SCM scheme_InsertF (SCM optional);
install_scm_function ("d-InsertF", scheme_InsertF);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddNoteF")), "scm", (gpointer) 1);
SCM scheme_AddNoteF (SCM optional);
install_scm_function ("d-AddNoteF", scheme_AddNoteF);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddF")), "scm", (gpointer) 1);
SCM scheme_AddF (SCM optional);
install_scm_function ("d-AddF", scheme_AddF);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ChangeToF")), "scm", (gpointer) 1);
SCM scheme_ChangeToF (SCM optional);
install_scm_function ("d-ChangeToF", scheme_ChangeToF);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoveToF")), "scm", (gpointer) 1);
SCM scheme_MoveToF (SCM optional);
install_scm_function ("d-MoveToF", scheme_MoveToF);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertG")), "scm", (gpointer) 1);
SCM scheme_InsertG (SCM optional);
install_scm_function ("d-InsertG", scheme_InsertG);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddNoteG")), "scm", (gpointer) 1);
SCM scheme_AddNoteG (SCM optional);
install_scm_function ("d-AddNoteG", scheme_AddNoteG);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "AddG")), "scm", (gpointer) 1);
SCM scheme_AddG (SCM optional);
install_scm_function ("d-AddG", scheme_AddG);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "ChangeToG")), "scm", (gpointer) 1);
SCM scheme_ChangeToG (SCM optional);
install_scm_function ("d-ChangeToG", scheme_ChangeToG);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "MoveToG")), "scm", (gpointer) 1);
SCM scheme_MoveToG (SCM optional);
install_scm_function ("d-MoveToG", scheme_MoveToG);
/*0 */
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "0")), "scm", (gpointer) 1);
SCM scheme_0 (SCM optional);
install_scm_function ("d-0", scheme_0);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert0")), "scm", (gpointer) 1);
SCM scheme_InsertDur0 (SCM optional);
install_scm_function ("d-Insert0", scheme_InsertDur0);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Change0")), "scm", (gpointer) 1);
SCM scheme_ChangeDur0 (SCM optional);
install_scm_function ("d-Change0", scheme_ChangeDur0);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set0")), "scm", (gpointer) 1);
SCM scheme_SetDur0 (SCM optional);
install_scm_function ("d-Set0", scheme_SetDur0);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertRest0")), "scm", (gpointer) 1);
SCM scheme_InsertRest0 (SCM optional);
install_scm_function ("d-InsertRest0", scheme_InsertRest0);
/*1 */
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "1")), "scm", (gpointer) 1);
SCM scheme_1 (SCM optional);
install_scm_function ("d-1", scheme_1);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert1")), "scm", (gpointer) 1);
SCM scheme_InsertDur1 (SCM optional);
install_scm_function ("d-Insert1", scheme_InsertDur1);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Change1")), "scm", (gpointer) 1);
SCM scheme_ChangeDur1 (SCM optional);
install_scm_function ("d-Change1", scheme_ChangeDur1);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set1")), "scm", (gpointer) 1);
SCM scheme_SetDur1 (SCM optional);
install_scm_function ("d-Set1", scheme_SetDur1);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertRest1")), "scm", (gpointer) 1);
SCM scheme_InsertRest1 (SCM optional);
install_scm_function ("d-InsertRest1", scheme_InsertRest1);
/*2 */
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "2")), "scm", (gpointer) 1);
SCM scheme_2 (SCM optional);
install_scm_function ("d-2", scheme_2);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert2")), "scm", (gpointer) 1);
SCM scheme_InsertDur2 (SCM optional);
install_scm_function ("d-Insert2", scheme_InsertDur2);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Change2")), "scm", (gpointer) 1);
SCM scheme_ChangeDur2 (SCM optional);
install_scm_function ("d-Change2", scheme_ChangeDur2);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set2")), "scm", (gpointer) 1);
SCM scheme_SetDur2 (SCM optional);
install_scm_function ("d-Set2", scheme_SetDur2);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertRest2")), "scm", (gpointer) 1);
SCM scheme_InsertRest2 (SCM optional);
install_scm_function ("d-InsertRest2", scheme_InsertRest2);
/*3 */
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "3")), "scm", (gpointer) 1);
SCM scheme_3 (SCM optional);
install_scm_function ("d-3", scheme_3);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert3")), "scm", (gpointer) 1);
SCM scheme_InsertDur3 (SCM optional);
install_scm_function ("d-Insert3", scheme_InsertDur3);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Change3")), "scm", (gpointer) 1);
SCM scheme_ChangeDur3 (SCM optional);
install_scm_function ("d-Change3", scheme_ChangeDur3);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set3")), "scm", (gpointer) 1);
SCM scheme_SetDur3 (SCM optional);
install_scm_function ("d-Set3", scheme_SetDur3);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertRest3")), "scm", (gpointer) 1);
SCM scheme_InsertRest3 (SCM optional);
install_scm_function ("d-InsertRest3", scheme_InsertRest3);
/*4 */
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "4")), "scm", (gpointer) 1);
SCM scheme_4 (SCM optional);
install_scm_function ("d-4", scheme_4);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert4")), "scm", (gpointer) 1);
SCM scheme_InsertDur4 (SCM optional);
install_scm_function ("d-Insert4", scheme_InsertDur4);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Change4")), "scm", (gpointer) 1);
SCM scheme_ChangeDur4 (SCM optional);
install_scm_function ("d-Change4", scheme_ChangeDur4);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set4")), "scm", (gpointer) 1);
SCM scheme_SetDur4 (SCM optional);
install_scm_function ("d-Set4", scheme_SetDur4);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertRest4")), "scm", (gpointer) 1);
SCM scheme_InsertRest4 (SCM optional);
install_scm_function ("d-InsertRest4", scheme_InsertRest4);
/*5 */
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "5")), "scm", (gpointer) 1);
SCM scheme_5 (SCM optional);
install_scm_function ("d-5", scheme_5);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert5")), "scm", (gpointer) 1);
SCM scheme_InsertDur5 (SCM optional);
install_scm_function ("d-Insert5", scheme_InsertDur5);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Change5")), "scm", (gpointer) 1);
SCM scheme_ChangeDur5 (SCM optional);
install_scm_function ("d-Change5", scheme_ChangeDur5);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set5")), "scm", (gpointer) 1);
SCM scheme_SetDur5 (SCM optional);
install_scm_function ("d-Set5", scheme_SetDur5);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertRest5")), "scm", (gpointer) 1);
SCM scheme_InsertRest5 (SCM optional);
install_scm_function ("d-InsertRest5", scheme_InsertRest5);
/*6 */
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "6")), "scm", (gpointer) 1);
SCM scheme_6 (SCM optional);
install_scm_function ("d-6", scheme_6);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert6")), "scm", (gpointer) 1);
SCM scheme_InsertDur6 (SCM optional);
install_scm_function ("d-Insert6", scheme_InsertDur6);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Change6")), "scm", (gpointer) 1);
SCM scheme_ChangeDur6 (SCM optional);
install_scm_function ("d-Change6", scheme_ChangeDur6);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set6")), "scm", (gpointer) 1);
SCM scheme_SetDur6 (SCM optional);
install_scm_function ("d-Set6", scheme_SetDur6);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertRest6")), "scm", (gpointer) 1);
SCM scheme_InsertRest6 (SCM optional);
install_scm_function ("d-InsertRest6", scheme_InsertRest6);
/*7 */
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "7")), "scm", (gpointer) 1);
SCM scheme_7 (SCM optional);
install_scm_function ("d-7", scheme_7);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert7")), "scm", (gpointer) 1);
SCM scheme_InsertDur7 (SCM optional);
install_scm_function ("d-Insert7", scheme_InsertDur7);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Change7")), "scm", (gpointer) 1);
SCM scheme_ChangeDur7 (SCM optional);
install_scm_function ("d-Change7", scheme_ChangeDur7);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set7")), "scm", (gpointer) 1);
SCM scheme_SetDur7 (SCM optional);
install_scm_function ("d-Set7", scheme_SetDur7);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertRest7")), "scm", (gpointer) 1);
SCM scheme_InsertRest7 (SCM optional);
install_scm_function ("d-InsertRest7", scheme_InsertRest7);
/*8 */
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "8")), "scm", (gpointer) 1);
SCM scheme_8 (SCM optional);
install_scm_function ("d-8", scheme_8);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Insert8")), "scm", (gpointer) 1);
SCM scheme_InsertDur8 (SCM optional);
install_scm_function ("d-Insert8", scheme_InsertDur8);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Change8")), "scm", (gpointer) 1);
SCM scheme_ChangeDur8 (SCM optional);
install_scm_function ("d-Change8", scheme_ChangeDur8);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "Set8")), "scm", (gpointer) 1);
SCM scheme_SetDur8 (SCM optional);
install_scm_function ("d-Set8", scheme_SetDur8);
g_object_set_data (G_OBJECT (action_of_name (Denemo.map, "InsertRest8")), "scm", (gpointer) 1);
SCM scheme_InsertRest8 (SCM optional);
install_scm_function ("d-InsertRest8", scheme_InsertRest8);
