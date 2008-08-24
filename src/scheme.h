gchar *text;
/*CursorLeft cursorleft*/
text = g_strdup_printf("(define dnm_CursorLeft %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "CursorLeft"));
(void)scm_c_eval_string(text);
g_free(text);
/*CursorDown cursordown*/
text = g_strdup_printf("(define dnm_CursorDown %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "CursorDown"));
(void)scm_c_eval_string(text);
g_free(text);
/*CursorUp cursorup*/
text = g_strdup_printf("(define dnm_CursorUp %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "CursorUp"));
(void)scm_c_eval_string(text);
g_free(text);
/*CursorRight cursorright*/
text = g_strdup_printf("(define dnm_CursorRight %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "CursorRight"));
(void)scm_c_eval_string(text);
g_free(text);
/*StaffUp staffup*/
text = g_strdup_printf("(define dnm_StaffUp %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "StaffUp"));
(void)scm_c_eval_string(text);
g_free(text);
/*StaffDown staffdown*/
text = g_strdup_printf("(define dnm_StaffDown %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "StaffDown"));
(void)scm_c_eval_string(text);
g_free(text);
/*MeasureLeft measureleft*/
text = g_strdup_printf("(define dnm_MeasureLeft %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "MeasureLeft"));
(void)scm_c_eval_string(text);
g_free(text);
/*MeasureRight measureright*/
text = g_strdup_printf("(define dnm_MeasureRight %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "MeasureRight"));
(void)scm_c_eval_string(text);
g_free(text);
/*A go_to_A_key*/
text = g_strdup_printf("(define dnm_A %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "A"));
(void)scm_c_eval_string(text);
g_free(text);
/*B go_to_B_key*/
text = g_strdup_printf("(define dnm_B %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "B"));
(void)scm_c_eval_string(text);
g_free(text);
/*C go_to_C_key*/
text = g_strdup_printf("(define dnm_C %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "C"));
(void)scm_c_eval_string(text);
g_free(text);
/*D go_to_D_key*/
text = g_strdup_printf("(define dnm_D %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "D"));
(void)scm_c_eval_string(text);
g_free(text);
/*E go_to_E_key*/
text = g_strdup_printf("(define dnm_E %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "E"));
(void)scm_c_eval_string(text);
g_free(text);
/*F go_to_F_key*/
text = g_strdup_printf("(define dnm_F %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "F"));
(void)scm_c_eval_string(text);
g_free(text);
/*G go_to_G_key*/
text = g_strdup_printf("(define dnm_G %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "G"));
(void)scm_c_eval_string(text);
g_free(text);
/*OctaveUp octave_up_key*/
text = g_strdup_printf("(define dnm_OctaveUp %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "OctaveUp"));
(void)scm_c_eval_string(text);
g_free(text);
/*OctaveDown octave_down_key*/
text = g_strdup_printf("(define dnm_OctaveDown %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "OctaveDown"));
(void)scm_c_eval_string(text);
g_free(text);
/*WholeNote insert_chord_0key*/
text = g_strdup_printf("(define dnm_WholeNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "WholeNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*HalfNote insert_chord_1key*/
text = g_strdup_printf("(define dnm_HalfNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "HalfNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*QuarterNote insert_chord_2key*/
text = g_strdup_printf("(define dnm_QuarterNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "QuarterNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*EighthNote insert_chord_3key*/
text = g_strdup_printf("(define dnm_EighthNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "EighthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*SixteenthNote insert_chord_4key*/
text = g_strdup_printf("(define dnm_SixteenthNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SixteenthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*ThirtysecondNote insert_chord_5key*/
text = g_strdup_printf("(define dnm_ThirtysecondNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ThirtysecondNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*SixtyfourthNote insert_chord_6key*/
text = g_strdup_printf("(define dnm_SixtyfourthNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SixtyfourthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankWholeNote insert_blankchord_0key*/
text = g_strdup_printf("(define dnm_InsertBlankWholeNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBlankWholeNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankHalfNote insert_blankchord_1key*/
text = g_strdup_printf("(define dnm_InsertBlankHalfNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBlankHalfNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankQuarterNote insert_blankchord_2key*/
text = g_strdup_printf("(define dnm_InsertBlankQuarterNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBlankQuarterNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankEighthNote insert_blankchord_3key*/
text = g_strdup_printf("(define dnm_InsertBlankEighthNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBlankEighthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankSixteenthNote insert_blankchord_4key*/
text = g_strdup_printf("(define dnm_InsertBlankSixteenthNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBlankSixteenthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankThirtysecondNote insert_blankchord_5key*/
text = g_strdup_printf("(define dnm_InsertBlankThirtysecondNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBlankThirtysecondNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankSixtyfourthNote insert_blankchord_6key*/
text = g_strdup_printf("(define dnm_InsertBlankSixtyfourthNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBlankSixtyfourthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleRestMode rest_toggle_key*/
text = g_strdup_printf("(define dnm_ToggleRestMode %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleRestMode"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleBlankMode toggle_blank*/
text = g_strdup_printf("(define dnm_ToggleBlankMode %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleBlankMode"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertWholeRest insert_rest_0key*/
text = g_strdup_printf("(define dnm_InsertWholeRest %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertWholeRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertHalfRest insert_rest_1key*/
text = g_strdup_printf("(define dnm_InsertHalfRest %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertHalfRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertQuarterRest insert_rest_2key*/
text = g_strdup_printf("(define dnm_InsertQuarterRest %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertQuarterRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertEighthRest insert_rest_3key*/
text = g_strdup_printf("(define dnm_InsertEighthRest %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertEighthRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertSixteenthRest insert_rest_4key*/
text = g_strdup_printf("(define dnm_InsertSixteenthRest %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertSixteenthRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertThirtysecondRest insert_rest_5key*/
text = g_strdup_printf("(define dnm_InsertThirtysecondRest %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertThirtysecondRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertSixtyfourthRest insert_rest_6key*/
text = g_strdup_printf("(define dnm_InsertSixtyfourthRest %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertSixtyfourthRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDuplet insert_duplet*/
text = g_strdup_printf("(define dnm_InsertDuplet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertDuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertTriplet insert_triplet*/
text = g_strdup_printf("(define dnm_InsertTriplet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertTriplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*StartTriplet start_triplet*/
text = g_strdup_printf("(define dnm_StartTriplet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "StartTriplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*EndTuplet end_tuplet*/
text = g_strdup_printf("(define dnm_EndTuplet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "EndTuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertQuadtuplet insert_quadtuplet*/
text = g_strdup_printf("(define dnm_InsertQuadtuplet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertQuadtuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertQuintuplet insert_quintuplet*/
text = g_strdup_printf("(define dnm_InsertQuintuplet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertQuintuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertSextuplet insert_sextuplet*/
text = g_strdup_printf("(define dnm_InsertSextuplet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertSextuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertSeptuplet insert_septuplet*/
text = g_strdup_printf("(define dnm_InsertSeptuplet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertSeptuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddTone add_tone_key*/
text = g_strdup_printf("(define dnm_AddTone %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddTone"));
(void)scm_c_eval_string(text);
g_free(text);
/*RemoveTone remove_tone_key*/
text = g_strdup_printf("(define dnm_RemoveTone %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "RemoveTone"));
(void)scm_c_eval_string(text);
g_free(text);
/*SharpenOrStemDown sharpen_key*/
text = g_strdup_printf("(define dnm_SharpenOrStemDown %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SharpenOrStemDown"));
(void)scm_c_eval_string(text);
g_free(text);
/*FlattenOrStemUp flatten_key*/
text = g_strdup_printf("(define dnm_FlattenOrStemUp %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "FlattenOrStemUp"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddDot add_dot_key*/
text = g_strdup_printf("(define dnm_AddDot %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddDot"));
(void)scm_c_eval_string(text);
g_free(text);
/*RemoveDot remove_dot_key*/
text = g_strdup_printf("(define dnm_RemoveDot %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "RemoveDot"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertTiedNote tie_notes_key*/
text = g_strdup_printf("(define dnm_InsertTiedNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertTiedNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteObject deleteobject*/
text = g_strdup_printf("(define dnm_DeleteObject %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteObject"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeletePreviousObject deletepreviousobject*/
text = g_strdup_printf("(define dnm_DeletePreviousObject %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeletePreviousObject"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertMeasure insert_measure_key*/
text = g_strdup_printf("(define dnm_InsertMeasure %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertMeasure"));
(void)scm_c_eval_string(text);
g_free(text);
/*AppendMeasure append_measure_key*/
text = g_strdup_printf("(define dnm_AppendMeasure %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AppendMeasure"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteMeasure deletemeasure*/
text = g_strdup_printf("(define dnm_DeleteMeasure %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteMeasure"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteMeasureAllStaffs deletemeasureallstaffs*/
text = g_strdup_printf("(define dnm_DeleteMeasureAllStaffs %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteMeasureAllStaffs"));
(void)scm_c_eval_string(text);
g_free(text);
/*ShrinkMeasures adjust_measure_less_width_key*/
text = g_strdup_printf("(define dnm_ShrinkMeasures %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ShrinkMeasures"));
(void)scm_c_eval_string(text);
g_free(text);
/*WidenMeasures adjust_measure_more_width_key*/
text = g_strdup_printf("(define dnm_WidenMeasures %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "WidenMeasures"));
(void)scm_c_eval_string(text);
g_free(text);
/*ShorterStaffs adjust_staff_less_height_key*/
text = g_strdup_printf("(define dnm_ShorterStaffs %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ShorterStaffs"));
(void)scm_c_eval_string(text);
g_free(text);
/*TallerStaffs adjust_staff_more_height_key*/
text = g_strdup_printf("(define dnm_TallerStaffs %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "TallerStaffs"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertTrebleClef newcleftreble*/
text = g_strdup_printf("(define dnm_InsertTrebleClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertTrebleClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBassClef newclefbass*/
text = g_strdup_printf("(define dnm_InsertBassClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBassClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insertg8clef newclefg8*/
text = g_strdup_printf("(define dnm_Insertg8clef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insertg8clef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertAltoClef newclefalto*/
text = g_strdup_printf("(define dnm_InsertAltoClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertAltoClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertTenorClef newcleftenor*/
text = g_strdup_printf("(define dnm_InsertTenorClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertTenorClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertSopranoClef newclefsoprano*/
text = g_strdup_printf("(define dnm_InsertSopranoClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertSopranoClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialTrebleClef setcleftreble*/
text = g_strdup_printf("(define dnm_SetInitialTrebleClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialTrebleClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialBassClef setclefbass*/
text = g_strdup_printf("(define dnm_SetInitialBassClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialBassClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialg8clef setclefg8*/
text = g_strdup_printf("(define dnm_SetInitialg8clef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialg8clef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialAltoClef setclefalto*/
text = g_strdup_printf("(define dnm_SetInitialAltoClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialAltoClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialTenorClef setcleftenor*/
text = g_strdup_printf("(define dnm_SetInitialTenorClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialTenorClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialSopranoClef setclefsoprano*/
text = g_strdup_printf("(define dnm_SetInitialSopranoClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialSopranoClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert22Time newtimesig22*/
text = g_strdup_printf("(define dnm_Insert22Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert22Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert32Time newtimesig32*/
text = g_strdup_printf("(define dnm_Insert32Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert32Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert42Time newtimesig42*/
text = g_strdup_printf("(define dnm_Insert42Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert42Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert44Time newtimesig44*/
text = g_strdup_printf("(define dnm_Insert44Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert44Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert34Time newtimesig34*/
text = g_strdup_printf("(define dnm_Insert34Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert34Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert24Time newtimesig24*/
text = g_strdup_printf("(define dnm_Insert24Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert24Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert64Time newtimesig64*/
text = g_strdup_printf("(define dnm_Insert64Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert64Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert38Time newtimesig38*/
text = g_strdup_printf("(define dnm_Insert38Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert38Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert68Time newtimesig68*/
text = g_strdup_printf("(define dnm_Insert68Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert68Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert128Time newtimesig128*/
text = g_strdup_printf("(define dnm_Insert128Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert128Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert98Time newtimesig98*/
text = g_strdup_printf("(define dnm_Insert98Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert98Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set22Time settimesig22*/
text = g_strdup_printf("(define dnm_Set22Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set22Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set32Time settimesig32*/
text = g_strdup_printf("(define dnm_Set32Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set32Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set42Time settimesig42*/
text = g_strdup_printf("(define dnm_Set42Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set42Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set44Time settimesig44*/
text = g_strdup_printf("(define dnm_Set44Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set44Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set34Time settimesig34*/
text = g_strdup_printf("(define dnm_Set34Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set34Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set24Time settimesig24*/
text = g_strdup_printf("(define dnm_Set24Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set24Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set64Time settimesig64*/
text = g_strdup_printf("(define dnm_Set64Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set64Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set38Time settimesig38*/
text = g_strdup_printf("(define dnm_Set38Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set38Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set68Time settimesig68*/
text = g_strdup_printf("(define dnm_Set68Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set68Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set128Time settimesig128*/
text = g_strdup_printf("(define dnm_Set128Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set128Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set98Time settimesig98*/
text = g_strdup_printf("(define dnm_Set98Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set98Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertCmaj newkeysigcmaj*/
text = g_strdup_printf("(define dnm_InsertCmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertCmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertGmaj newkeysiggmaj*/
text = g_strdup_printf("(define dnm_InsertGmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertGmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDmaj newkeysigdmaj*/
text = g_strdup_printf("(define dnm_InsertDmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertDmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertAmaj newkeysigamaj*/
text = g_strdup_printf("(define dnm_InsertAmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertAmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertEmaj newkeysigemaj*/
text = g_strdup_printf("(define dnm_InsertEmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertEmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBmaj newkeysigbmaj*/
text = g_strdup_printf("(define dnm_InsertBmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertFSharpmaj newkeysigfsharpmaj*/
text = g_strdup_printf("(define dnm_InsertFSharpmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertFSharpmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertCSharpmaj newkeysigcsharpmaj*/
text = g_strdup_printf("(define dnm_InsertCSharpmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertCSharpmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertFmaj newkeysigfmaj*/
text = g_strdup_printf("(define dnm_InsertFmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertFmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBflatmaj newkeysigbflatmaj*/
text = g_strdup_printf("(define dnm_InsertBflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertEflatmaj newkeysigeflatmaj*/
text = g_strdup_printf("(define dnm_InsertEflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertEflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertAflatmaj newkeysigaflatmaj*/
text = g_strdup_printf("(define dnm_InsertAflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertAflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDflatmaj newkeysigdflatmaj*/
text = g_strdup_printf("(define dnm_InsertDflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertDflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertGflatmaj newkeysiggflatmaj*/
text = g_strdup_printf("(define dnm_InsertGflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertGflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertCflatmaj newkeysigcflatmaj*/
text = g_strdup_printf("(define dnm_InsertCflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertCflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertAmin newkeysigamin*/
text = g_strdup_printf("(define dnm_InsertAmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertAmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertEmin newkeysigemin*/
text = g_strdup_printf("(define dnm_InsertEmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertEmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBmin newkeysigbmin*/
text = g_strdup_printf("(define dnm_InsertBmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertFSharpmin newkeysigfsharpmin*/
text = g_strdup_printf("(define dnm_InsertFSharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertFSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertCSharpmin newkeysigcsharpmin*/
text = g_strdup_printf("(define dnm_InsertCSharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertCSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertGSharpmin newkeysiggsharpmin*/
text = g_strdup_printf("(define dnm_InsertGSharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertGSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDSharpmin newkeysigdsharpmin*/
text = g_strdup_printf("(define dnm_InsertDSharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertDSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertASharpmin newkeysigasharpmin*/
text = g_strdup_printf("(define dnm_InsertASharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertASharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDmin newkeysigdmin*/
text = g_strdup_printf("(define dnm_InsertDmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertDmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertGmin newkeysiggmin*/
text = g_strdup_printf("(define dnm_InsertGmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertGmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertCmin newkeysigcmin*/
text = g_strdup_printf("(define dnm_InsertCmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertCmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertFmin newkeysigfmin*/
text = g_strdup_printf("(define dnm_InsertFmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertFmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBflatmin newkeysigbflatmin*/
text = g_strdup_printf("(define dnm_InsertBflatmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertEflatmin newkeysigeflatmin*/
text = g_strdup_printf("(define dnm_InsertEflatmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertEflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertAflatmin newkeysigaflatmin*/
text = g_strdup_printf("(define dnm_InsertAflatmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertAflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialCmaj setkeysigcmaj*/
text = g_strdup_printf("(define dnm_SetInitialCmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialCmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialGmaj setkeysiggmaj*/
text = g_strdup_printf("(define dnm_SetInitialGmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialGmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialDmaj setkeysigdmaj*/
text = g_strdup_printf("(define dnm_SetInitialDmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialDmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialAmaj setkeysigamaj*/
text = g_strdup_printf("(define dnm_SetInitialAmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialAmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialEmaj setkeysigemaj*/
text = g_strdup_printf("(define dnm_SetInitialEmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialEmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialBmaj setkeysigbmaj*/
text = g_strdup_printf("(define dnm_SetInitialBmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialBmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialFSharpmaj setkeysigfsharpmaj*/
text = g_strdup_printf("(define dnm_SetInitialFSharpmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialFSharpmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialCSharpmaj setkeysigcsharpmaj*/
text = g_strdup_printf("(define dnm_SetInitialCSharpmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialCSharpmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialFmaj setkeysigfmaj*/
text = g_strdup_printf("(define dnm_SetInitialFmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialFmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialBflatmaj setkeysigbflatmaj*/
text = g_strdup_printf("(define dnm_SetInitialBflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialBflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialEflatmaj setkeysigeflatmaj*/
text = g_strdup_printf("(define dnm_SetInitialEflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialEflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialAflatmaj setkeysigaflatmaj*/
text = g_strdup_printf("(define dnm_SetInitialAflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialAflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialDflatmaj setkeysigdflatmaj*/
text = g_strdup_printf("(define dnm_SetInitialDflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialDflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialGflatmaj setkeysiggflatmaj*/
text = g_strdup_printf("(define dnm_SetInitialGflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialGflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialCflatmaj setkeysigcflatmaj*/
text = g_strdup_printf("(define dnm_SetInitialCflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialCflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialAmin setkeysigamin*/
text = g_strdup_printf("(define dnm_SetInitialAmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialAmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialEmin setkeysigemin*/
text = g_strdup_printf("(define dnm_SetInitialEmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialEmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialBmin setkeysigbmin*/
text = g_strdup_printf("(define dnm_SetInitialBmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialBmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialFSharpmin setkeysigfsharpmin*/
text = g_strdup_printf("(define dnm_SetInitialFSharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialFSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialCSharpmin setkeysigcsharpmin*/
text = g_strdup_printf("(define dnm_SetInitialCSharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialCSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialGSharpmin setkeysiggsharpmin*/
text = g_strdup_printf("(define dnm_SetInitialGSharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialGSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialDSharpmin setkeysigdsharpmin*/
text = g_strdup_printf("(define dnm_SetInitialDSharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialDSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialASharpmin setkeysigasharpmin*/
text = g_strdup_printf("(define dnm_SetInitialASharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialASharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialDmin setkeysigdmin*/
text = g_strdup_printf("(define dnm_SetInitialDmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialDmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialGmin setkeysiggmin*/
text = g_strdup_printf("(define dnm_SetInitialGmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialGmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialCmin setkeysigcmin*/
text = g_strdup_printf("(define dnm_SetInitialCmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialCmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialFmin setkeysigfmin*/
text = g_strdup_printf("(define dnm_SetInitialFmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialFmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialBflatmin setkeysigbflatmin*/
text = g_strdup_printf("(define dnm_SetInitialBflatmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialBflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialEflatmin setkeysigeflatmin*/
text = g_strdup_printf("(define dnm_SetInitialEflatmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialEflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialAflatmin setkeysigaflatmin*/
text = g_strdup_printf("(define dnm_SetInitialAflatmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialAflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetMark set_mark*/
text = g_strdup_printf("(define dnm_SetMark %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetMark"));
(void)scm_c_eval_string(text);
g_free(text);
/*UnsetMark unset_mark*/
text = g_strdup_printf("(define dnm_UnsetMark %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "UnsetMark"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleBeginSlur toggle_begin_slur*/
text = g_strdup_printf("(define dnm_ToggleBeginSlur %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleBeginSlur"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleEndSlur toggle_end_slur*/
text = g_strdup_printf("(define dnm_ToggleEndSlur %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleEndSlur"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleStartCrescendo toggle_start_crescendo*/
text = g_strdup_printf("(define dnm_ToggleStartCrescendo %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleStartCrescendo"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleEndCrescendo toggle_end_crescendo*/
text = g_strdup_printf("(define dnm_ToggleEndCrescendo %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleEndCrescendo"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleStartDiminuendo toggle_start_diminuendo*/
text = g_strdup_printf("(define dnm_ToggleStartDiminuendo %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleStartDiminuendo"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleEndDiminuendo toggle_end_diminuendo*/
text = g_strdup_printf("(define dnm_ToggleEndDiminuendo %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleEndDiminuendo"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleAccent add_accent*/
text = g_strdup_printf("(define dnm_ToggleAccent %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleAccent"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleFermata add_fermata*/
text = g_strdup_printf("(define dnm_ToggleFermata %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleFermata"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleStaccato add_staccato*/
text = g_strdup_printf("(define dnm_ToggleStaccato %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleStaccato"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleTenuto add_tenuto*/
text = g_strdup_printf("(define dnm_ToggleTenuto %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleTenuto"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleTrill add_trill*/
text = g_strdup_printf("(define dnm_ToggleTrill %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleTrill"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleTurn add_turn*/
text = g_strdup_printf("(define dnm_ToggleTurn %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleTurn"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleMordent add_mordent*/
text = g_strdup_printf("(define dnm_ToggleMordent %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleMordent"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleStaccatissimo add_staccatissimo*/
text = g_strdup_printf("(define dnm_ToggleStaccatissimo %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleStaccatissimo"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleCoda add_coda*/
text = g_strdup_printf("(define dnm_ToggleCoda %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleCoda"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleFlageolet add_flageolet*/
text = g_strdup_printf("(define dnm_ToggleFlageolet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleFlageolet"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleOpen add_open*/
text = g_strdup_printf("(define dnm_ToggleOpen %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleOpen"));
(void)scm_c_eval_string(text);
g_free(text);
/*TogglePrallMordent add_prallmordent*/
text = g_strdup_printf("(define dnm_TogglePrallMordent %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "TogglePrallMordent"));
(void)scm_c_eval_string(text);
g_free(text);
/*TogglePrallPrall add_prallprall*/
text = g_strdup_printf("(define dnm_TogglePrallPrall %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "TogglePrallPrall"));
(void)scm_c_eval_string(text);
g_free(text);
/*TogglePrall add_prall*/
text = g_strdup_printf("(define dnm_TogglePrall %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "TogglePrall"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleReverseTurn add_reverseturn*/
text = g_strdup_printf("(define dnm_ToggleReverseTurn %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleReverseTurn"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleSegno add_segno*/
text = g_strdup_printf("(define dnm_ToggleSegno %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleSegno"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleSforzato add_sforzato*/
text = g_strdup_printf("(define dnm_ToggleSforzato %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleSforzato"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleStopped add_stopped*/
text = g_strdup_printf("(define dnm_ToggleStopped %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleStopped"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleThumb add_thumb*/
text = g_strdup_printf("(define dnm_ToggleThumb %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleThumb"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleUpprall add_upprall*/
text = g_strdup_printf("(define dnm_ToggleUpprall %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleUpprall"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleArpeggio add_arpeggio*/
text = g_strdup_printf("(define dnm_ToggleArpeggio %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleArpeggio"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetGrace set_grace*/
text = g_strdup_printf("(define dnm_SetGrace %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetGrace"));
(void)scm_c_eval_string(text);
g_free(text);
/*ForceCaution force_cautionary*/
text = g_strdup_printf("(define dnm_ForceCaution %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ForceCaution"));
(void)scm_c_eval_string(text);
g_free(text);
/*ChangePitch change_pitch*/
text = g_strdup_printf("(define dnm_ChangePitch %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ChangePitch"));
(void)scm_c_eval_string(text);
g_free(text);
/*DoubleBar insert_doublebar*/
text = g_strdup_printf("(define dnm_DoubleBar %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DoubleBar"));
(void)scm_c_eval_string(text);
g_free(text);
/*EndBar insert_endbar*/
text = g_strdup_printf("(define dnm_EndBar %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "EndBar"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenRepeat insert_openrepeat*/
text = g_strdup_printf("(define dnm_OpenRepeat %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "OpenRepeat"));
(void)scm_c_eval_string(text);
g_free(text);
/*CloseRepeat insert_closerepeat*/
text = g_strdup_printf("(define dnm_CloseRepeat %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "CloseRepeat"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenCloseRepeat insert_opencloserepeat*/
text = g_strdup_printf("(define dnm_OpenCloseRepeat %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "OpenCloseRepeat"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertRhythm insert_rhythm_pattern*/
text = g_strdup_printf("(define dnm_InsertRhythm %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertRhythm"));
(void)scm_c_eval_string(text);
g_free(text);
/*NextRhythm nextrhythm*/
text = g_strdup_printf("(define dnm_NextRhythm %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "NextRhythm"));
(void)scm_c_eval_string(text);
g_free(text);
/*AppendMesauresToScore append_measure_score*/
text = g_strdup_printf("(define dnm_AppendMesauresToScore %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AppendMesauresToScore"));
(void)scm_c_eval_string(text);
g_free(text);
/*New file_newwrapper*/
text = g_strdup_printf("(define dnm_New %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "New"));
(void)scm_c_eval_string(text);
g_free(text);
/*Open file_open_with_check*/
text = g_strdup_printf("(define dnm_Open %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Open"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddStaffs file_add_staffs*/
text = g_strdup_printf("(define dnm_AddStaffs %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddStaffs"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddMovements file_add_movements*/
text = g_strdup_printf("(define dnm_AddMovements %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddMovements"));
(void)scm_c_eval_string(text);
g_free(text);
/*MovementProps movement_props_dialog*/
text = g_strdup_printf("(define dnm_MovementProps %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "MovementProps"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenNewWindow openinnew*/
text = g_strdup_printf("(define dnm_OpenNewWindow %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "OpenNewWindow"));
(void)scm_c_eval_string(text);
g_free(text);
/*Save file_savewrapper*/
text = g_strdup_printf("(define dnm_Save %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Save"));
(void)scm_c_eval_string(text);
g_free(text);
/*SaveAs file_saveaswrapper*/
text = g_strdup_printf("(define dnm_SaveAs %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SaveAs"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenTemplate system_template_open_with_check*/
text = g_strdup_printf("(define dnm_OpenTemplate %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "OpenTemplate"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenExample system_example_open_with_check*/
text = g_strdup_printf("(define dnm_OpenExample %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "OpenExample"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenMyTemplate local_template_open_with_check*/
text = g_strdup_printf("(define dnm_OpenMyTemplate %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "OpenMyTemplate"));
(void)scm_c_eval_string(text);
g_free(text);
/*SaveTemplate template_save*/
text = g_strdup_printf("(define dnm_SaveTemplate %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SaveTemplate"));
(void)scm_c_eval_string(text);
g_free(text);
/*NewWindow newview*/
text = g_strdup_printf("(define dnm_NewWindow %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "NewWindow"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertMovementBefore insert_movement_before*/
text = g_strdup_printf("(define dnm_InsertMovementBefore %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertMovementBefore"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertMovementAfter insert_movement_after*/
text = g_strdup_printf("(define dnm_InsertMovementAfter %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertMovementAfter"));
(void)scm_c_eval_string(text);
g_free(text);
/*SaveParts file_savepartswrapper*/
text = g_strdup_printf("(define dnm_SaveParts %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SaveParts"));
(void)scm_c_eval_string(text);
g_free(text);
/*ExportPDF export_pdf_action*/
text = g_strdup_printf("(define dnm_ExportPDF %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ExportPDF"));
(void)scm_c_eval_string(text);
g_free(text);
/*ConfigureScore scorewizard*/
text = g_strdup_printf("(define dnm_ConfigureScore %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ConfigureScore"));
(void)scm_c_eval_string(text);
g_free(text);
/*PrintPreview printpreview_cb*/
text = g_strdup_printf("(define dnm_PrintPreview %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "PrintPreview"));
(void)scm_c_eval_string(text);
g_free(text);
/*PrintExcerptPreview printexcerptpreview_cb*/
text = g_strdup_printf("(define dnm_PrintExcerptPreview %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "PrintExcerptPreview"));
(void)scm_c_eval_string(text);
g_free(text);
/*Print printall_cb*/
text = g_strdup_printf("(define dnm_Print %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Print"));
(void)scm_c_eval_string(text);
g_free(text);
/*PrintPart printpart_cb*/
text = g_strdup_printf("(define dnm_PrintPart %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "PrintPart"));
(void)scm_c_eval_string(text);
g_free(text);
/*Close close_gui_with_check*/
text = g_strdup_printf("(define dnm_Close %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Close"));
(void)scm_c_eval_string(text);
g_free(text);
/*Quit closewrapper*/
text = g_strdup_printf("(define dnm_Quit %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Quit"));
(void)scm_c_eval_string(text);
g_free(text);
/*Undo undowrapper*/
text = g_strdup_printf("(define dnm_Undo %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Undo"));
(void)scm_c_eval_string(text);
g_free(text);
/*Redo redowrapper*/
text = g_strdup_printf("(define dnm_Redo %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Redo"));
(void)scm_c_eval_string(text);
g_free(text);
/*Copy copywrapper*/
text = g_strdup_printf("(define dnm_Copy %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Copy"));
(void)scm_c_eval_string(text);
g_free(text);
/*Cut cutwrapper*/
text = g_strdup_printf("(define dnm_Cut %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Cut"));
(void)scm_c_eval_string(text);
g_free(text);
/*Paste pastewrapper*/
text = g_strdup_printf("(define dnm_Paste %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Paste"));
(void)scm_c_eval_string(text);
g_free(text);
/*ScoreProperties score_properties_dialog*/
text = g_strdup_printf("(define dnm_ScoreProperties %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ScoreProperties"));
(void)scm_c_eval_string(text);
g_free(text);
/*SaveSelection saveselwrapper*/
text = g_strdup_printf("(define dnm_SaveSelection %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SaveSelection"));
(void)scm_c_eval_string(text);
g_free(text);
/*Preferences preferences_change*/
text = g_strdup_printf("(define dnm_Preferences %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Preferences"));
(void)scm_c_eval_string(text);
g_free(text);
/*SaveAccels save_default_keymap_file_wrapper*/
text = g_strdup_printf("(define dnm_SaveAccels %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SaveAccels"));
(void)scm_c_eval_string(text);
g_free(text);
/*Keyboard configure_keyboard_dialog*/
text = g_strdup_printf("(define dnm_Keyboard %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Keyboard"));
(void)scm_c_eval_string(text);
g_free(text);
/*LoadPlugins load_plugin*/
text = g_strdup_printf("(define dnm_LoadPlugins %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "LoadPlugins"));
(void)scm_c_eval_string(text);
g_free(text);
/*UnloadPlugins unloadplugins*/
text = g_strdup_printf("(define dnm_UnloadPlugins %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "UnloadPlugins"));
(void)scm_c_eval_string(text);
g_free(text);
/*ListPlugins list_loaded_plugins*/
text = g_strdup_printf("(define dnm_ListPlugins %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ListPlugins"));
(void)scm_c_eval_string(text);
g_free(text);
/*ListAvailablePlugins list_available_plugins*/
text = g_strdup_printf("(define dnm_ListAvailablePlugins %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ListAvailablePlugins"));
(void)scm_c_eval_string(text);
g_free(text);
/*SwapStaffs swapstaffs*/
text = g_strdup_printf("(define dnm_SwapStaffs %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SwapStaffs"));
(void)scm_c_eval_string(text);
g_free(text);
/*SplitVoices splitstaffs*/
text = g_strdup_printf("(define dnm_SplitVoices %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SplitVoices"));
(void)scm_c_eval_string(text);
g_free(text);
/*JoinVoices joinstaffs*/
text = g_strdup_printf("(define dnm_JoinVoices %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "JoinVoices"));
(void)scm_c_eval_string(text);
g_free(text);
/*SwapMovements swapmovements*/
text = g_strdup_printf("(define dnm_SwapMovements %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SwapMovements"));
(void)scm_c_eval_string(text);
g_free(text);
/*VoiceUp voiceup*/
text = g_strdup_printf("(define dnm_VoiceUp %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "VoiceUp"));
(void)scm_c_eval_string(text);
g_free(text);
/*VoiceDown voicedown*/
text = g_strdup_printf("(define dnm_VoiceDown %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "VoiceDown"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddBefore newstaffbefore*/
text = g_strdup_printf("(define dnm_AddBefore %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddBefore"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddAfter dnm_newstaffafter*/
text = g_strdup_printf("(define dnm_AddAfter %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddAfter"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddInitial newstaffinitial*/
text = g_strdup_printf("(define dnm_AddInitial %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddInitial"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddLast newstafflast*/
text = g_strdup_printf("(define dnm_AddLast %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddLast"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteBefore delete_staff_before*/
text = g_strdup_printf("(define dnm_DeleteBefore %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteBefore"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteStaff delete_staff_current*/
text = g_strdup_printf("(define dnm_DeleteStaff %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteStaff"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteAfter delete_staff_after*/
text = g_strdup_printf("(define dnm_DeleteAfter %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteAfter"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddVoice dnm_newstaffvoice*/
text = g_strdup_printf("(define dnm_AddVoice %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddVoice"));
(void)scm_c_eval_string(text);
g_free(text);
/*TransposeStaff staff_transposition*/
text = g_strdup_printf("(define dnm_TransposeStaff %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "TransposeStaff"));
(void)scm_c_eval_string(text);
g_free(text);
/*StaffProperties staff_properties_change_cb*/
text = g_strdup_printf("(define dnm_StaffProperties %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "StaffProperties"));
(void)scm_c_eval_string(text);
g_free(text);
/*InitialClef clef_change_initial*/
text = g_strdup_printf("(define dnm_InitialClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InitialClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertClef clef_change_insert*/
text = g_strdup_printf("(define dnm_InsertClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InitialKey key_change_initial*/
text = g_strdup_printf("(define dnm_InitialKey %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InitialKey"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertKey key_change_insert*/
text = g_strdup_printf("(define dnm_InsertKey %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertKey"));
(void)scm_c_eval_string(text);
g_free(text);
/*InitialTimeSig timesig_change_initial*/
text = g_strdup_printf("(define dnm_InitialTimeSig %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InitialTimeSig"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertTimeSig timesig_change_insert*/
text = g_strdup_printf("(define dnm_InsertTimeSig %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertTimeSig"));
(void)scm_c_eval_string(text);
g_free(text);
/*ChangeNotehead set_notehead*/
text = g_strdup_printf("(define dnm_ChangeNotehead %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ChangeNotehead"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertStem stem_directive_insert*/
text = g_strdup_printf("(define dnm_InsertStem %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertStem"));
(void)scm_c_eval_string(text);
g_free(text);
/*EditLyric lyric_insert*/
text = g_strdup_printf("(define dnm_EditLyric %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "EditLyric"));
(void)scm_c_eval_string(text);
g_free(text);
/*EditFiguredBass figure_insert*/
text = g_strdup_printf("(define dnm_EditFiguredBass %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "EditFiguredBass"));
(void)scm_c_eval_string(text);
g_free(text);
/*EditChords fakechord_insert*/
text = g_strdup_printf("(define dnm_EditChords %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "EditChords"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDynamic insert_dynamic*/
text = g_strdup_printf("(define dnm_InsertDynamic %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertDynamic"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertLilyDirective lily_directive_insert*/
text = g_strdup_printf("(define dnm_InsertLilyDirective %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertLilyDirective"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertLilyPostfix lily_directive_postfix*/
text = g_strdup_printf("(define dnm_InsertLilyPostfix %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertLilyPostfix"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBarline insert_barline*/
text = g_strdup_printf("(define dnm_InsertBarline %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBarline"));
(void)scm_c_eval_string(text);
g_free(text);
/*GoToMeasure tomeasurenum*/
text = g_strdup_printf("(define dnm_GoToMeasure %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "GoToMeasure"));
(void)scm_c_eval_string(text);
g_free(text);
/*GoToBeginning tohome*/
text = g_strdup_printf("(define dnm_GoToBeginning %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "GoToBeginning"));
(void)scm_c_eval_string(text);
g_free(text);
/*GoToEnd toend*/
text = g_strdup_printf("(define dnm_GoToEnd %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "GoToEnd"));
(void)scm_c_eval_string(text);
g_free(text);
/*NextMovement next_movement*/
text = g_strdup_printf("(define dnm_NextMovement %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "NextMovement"));
(void)scm_c_eval_string(text);
g_free(text);
/*PreviousMovement prev_movement*/
text = g_strdup_printf("(define dnm_PreviousMovement %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "PreviousMovement"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteMovement delete_movement*/
text = g_strdup_printf("(define dnm_DeleteMovement %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteMovement"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteBookmarks deletebookmarks*/
text = g_strdup_printf("(define dnm_DeleteBookmarks %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteBookmarks"));
(void)scm_c_eval_string(text);
g_free(text);
/*Play ext_midi_playback*/
text = g_strdup_printf("(define dnm_Play %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Play"));
(void)scm_c_eval_string(text);
g_free(text);
/*Stop stop_midi_playback*/
text = g_strdup_printf("(define dnm_Stop %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Stop"));
(void)scm_c_eval_string(text);
g_free(text);
/*PlayCSound dnm_csoundplayback*/
text = g_strdup_printf("(define dnm_PlayCSound %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "PlayCSound"));
(void)scm_c_eval_string(text);
g_free(text);
/*PlaybackProperties playback_properties_change*/
text = g_strdup_printf("(define dnm_PlaybackProperties %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "PlaybackProperties"));
(void)scm_c_eval_string(text);
g_free(text);
/*Help browse_manual*/
text = g_strdup_printf("(define dnm_Help %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Help"));
(void)scm_c_eval_string(text);
g_free(text);
/*About about*/
text = g_strdup_printf("(define dnm_About %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "About"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddBookmark addbookmark*/
text = g_strdup_printf("(define dnm_AddBookmark %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddBookmark"));
(void)scm_c_eval_string(text);
g_free(text);
/*GotoBookmark gotobookmark*/
text = g_strdup_printf("(define dnm_GotoBookmark %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "GotoBookmark"));
(void)scm_c_eval_string(text);
g_free(text);
/*NextBookmark nextbookmark*/
text = g_strdup_printf("(define dnm_NextBookmark %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "NextBookmark"));
(void)scm_c_eval_string(text);
g_free(text);
/*PrevBookmark prevbookmark*/
text = g_strdup_printf("(define dnm_PrevBookmark %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "PrevBookmark"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleEdit toggle_edit_mode*/
text = g_strdup_printf("(define dnm_ToggleEdit %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleEdit"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleRest toggle_rest_mode*/
text = g_strdup_printf("(define dnm_ToggleRest %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleRhythm toggle_rhythm_mode*/
text = g_strdup_printf("(define dnm_ToggleRhythm %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleRhythm"));
(void)scm_c_eval_string(text);
g_free(text);
/*ClearOverlay clear_overlay*/
text = g_strdup_printf("(define dnm_ClearOverlay %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ClearOverlay"));
(void)scm_c_eval_string(text);
g_free(text);
/*CreateRhythm create_rhythm_cb*/
text = g_strdup_printf("(define dnm_CreateRhythm %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "CreateRhythm"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteRhythm delete_rhythm_cb*/
text = g_strdup_printf("(define dnm_DeleteRhythm %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteRhythm"));
(void)scm_c_eval_string(text);
g_free(text);
