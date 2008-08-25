gchar *text;
/*CursorLeft cursorleft*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "CursorLeft")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_CursorLeft %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "CursorLeft"));
(void)scm_c_eval_string(text);
g_free(text);
/*CursorDown cursordown*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "CursorDown")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_CursorDown %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "CursorDown"));
(void)scm_c_eval_string(text);
g_free(text);
/*CursorUp cursorup*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "CursorUp")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_CursorUp %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "CursorUp"));
(void)scm_c_eval_string(text);
g_free(text);
/*CursorRight cursorright*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "CursorRight")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_CursorRight %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "CursorRight"));
(void)scm_c_eval_string(text);
g_free(text);
/*StaffUp staffup*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "StaffUp")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_StaffUp %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "StaffUp"));
(void)scm_c_eval_string(text);
g_free(text);
/*StaffDown staffdown*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "StaffDown")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_StaffDown %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "StaffDown"));
(void)scm_c_eval_string(text);
g_free(text);
/*MeasureLeft measureleft*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "MeasureLeft")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_MeasureLeft %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "MeasureLeft"));
(void)scm_c_eval_string(text);
g_free(text);
/*MeasureRight measureright*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "MeasureRight")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_MeasureRight %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "MeasureRight"));
(void)scm_c_eval_string(text);
g_free(text);
/*A go_to_A_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "A")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_A %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "A"));
(void)scm_c_eval_string(text);
g_free(text);
/*B go_to_B_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "B")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_B %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "B"));
(void)scm_c_eval_string(text);
g_free(text);
/*C go_to_C_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "C")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_C %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "C"));
(void)scm_c_eval_string(text);
g_free(text);
/*D go_to_D_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "D")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_D %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "D"));
(void)scm_c_eval_string(text);
g_free(text);
/*E go_to_E_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "E")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_E %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "E"));
(void)scm_c_eval_string(text);
g_free(text);
/*F go_to_F_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "F")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_F %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "F"));
(void)scm_c_eval_string(text);
g_free(text);
/*G go_to_G_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "G")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_G %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "G"));
(void)scm_c_eval_string(text);
g_free(text);
/*OctaveUp octave_up_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "OctaveUp")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_OctaveUp %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "OctaveUp"));
(void)scm_c_eval_string(text);
g_free(text);
/*OctaveDown octave_down_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "OctaveDown")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_OctaveDown %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "OctaveDown"));
(void)scm_c_eval_string(text);
g_free(text);
/*WholeNote insert_chord_0key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "WholeNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_WholeNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "WholeNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*HalfNote insert_chord_1key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "HalfNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_HalfNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "HalfNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*QuarterNote insert_chord_2key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "QuarterNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_QuarterNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "QuarterNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*EighthNote insert_chord_3key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "EighthNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_EighthNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "EighthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*SixteenthNote insert_chord_4key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SixteenthNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SixteenthNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SixteenthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*ThirtysecondNote insert_chord_5key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ThirtysecondNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ThirtysecondNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ThirtysecondNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*SixtyfourthNote insert_chord_6key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SixtyfourthNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SixtyfourthNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SixtyfourthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankWholeNote insert_blankchord_0key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertBlankWholeNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBlankWholeNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBlankWholeNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankHalfNote insert_blankchord_1key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertBlankHalfNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBlankHalfNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBlankHalfNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankQuarterNote insert_blankchord_2key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertBlankQuarterNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBlankQuarterNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBlankQuarterNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankEighthNote insert_blankchord_3key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertBlankEighthNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBlankEighthNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBlankEighthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankSixteenthNote insert_blankchord_4key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertBlankSixteenthNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBlankSixteenthNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBlankSixteenthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankThirtysecondNote insert_blankchord_5key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertBlankThirtysecondNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBlankThirtysecondNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBlankThirtysecondNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankSixtyfourthNote insert_blankchord_6key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertBlankSixtyfourthNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBlankSixtyfourthNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBlankSixtyfourthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleRestMode rest_toggle_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleRestMode")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleRestMode %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleRestMode"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleBlankMode toggle_blank*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleBlankMode")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleBlankMode %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleBlankMode"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertWholeRest insert_rest_0key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertWholeRest")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertWholeRest %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertWholeRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertHalfRest insert_rest_1key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertHalfRest")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertHalfRest %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertHalfRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertQuarterRest insert_rest_2key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertQuarterRest")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertQuarterRest %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertQuarterRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertEighthRest insert_rest_3key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertEighthRest")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertEighthRest %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertEighthRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertSixteenthRest insert_rest_4key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertSixteenthRest")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertSixteenthRest %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertSixteenthRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertThirtysecondRest insert_rest_5key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertThirtysecondRest")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertThirtysecondRest %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertThirtysecondRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertSixtyfourthRest insert_rest_6key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertSixtyfourthRest")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertSixtyfourthRest %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertSixtyfourthRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDuplet insert_duplet*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertDuplet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertDuplet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertDuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertTriplet insert_triplet*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertTriplet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertTriplet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertTriplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*StartTriplet start_triplet*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "StartTriplet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_StartTriplet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "StartTriplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*EndTuplet end_tuplet*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "EndTuplet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_EndTuplet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "EndTuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertQuadtuplet insert_quadtuplet*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertQuadtuplet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertQuadtuplet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertQuadtuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertQuintuplet insert_quintuplet*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertQuintuplet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertQuintuplet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertQuintuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertSextuplet insert_sextuplet*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertSextuplet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertSextuplet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertSextuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertSeptuplet insert_septuplet*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertSeptuplet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertSeptuplet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertSeptuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddTone add_tone_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "AddTone")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddTone %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddTone"));
(void)scm_c_eval_string(text);
g_free(text);
/*RemoveTone remove_tone_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "RemoveTone")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_RemoveTone %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "RemoveTone"));
(void)scm_c_eval_string(text);
g_free(text);
/*SharpenOrStemDown sharpen_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SharpenOrStemDown")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SharpenOrStemDown %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SharpenOrStemDown"));
(void)scm_c_eval_string(text);
g_free(text);
/*FlattenOrStemUp flatten_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "FlattenOrStemUp")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_FlattenOrStemUp %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "FlattenOrStemUp"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddDot add_dot_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "AddDot")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddDot %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddDot"));
(void)scm_c_eval_string(text);
g_free(text);
/*RemoveDot remove_dot_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "RemoveDot")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_RemoveDot %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "RemoveDot"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertTiedNote tie_notes_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertTiedNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertTiedNote %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertTiedNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteObject deleteobject*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "DeleteObject")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteObject %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteObject"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeletePreviousObject deletepreviousobject*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "DeletePreviousObject")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeletePreviousObject %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeletePreviousObject"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertMeasure insert_measure_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertMeasure")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertMeasure %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertMeasure"));
(void)scm_c_eval_string(text);
g_free(text);
/*AppendMeasure append_measure_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "AppendMeasure")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AppendMeasure %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AppendMeasure"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteMeasure deletemeasure*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "DeleteMeasure")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteMeasure %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteMeasure"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteMeasureAllStaffs deletemeasureallstaffs*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "DeleteMeasureAllStaffs")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteMeasureAllStaffs %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteMeasureAllStaffs"));
(void)scm_c_eval_string(text);
g_free(text);
/*ShrinkMeasures adjust_measure_less_width_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ShrinkMeasures")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ShrinkMeasures %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ShrinkMeasures"));
(void)scm_c_eval_string(text);
g_free(text);
/*WidenMeasures adjust_measure_more_width_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "WidenMeasures")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_WidenMeasures %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "WidenMeasures"));
(void)scm_c_eval_string(text);
g_free(text);
/*ShorterStaffs adjust_staff_less_height_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ShorterStaffs")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ShorterStaffs %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ShorterStaffs"));
(void)scm_c_eval_string(text);
g_free(text);
/*TallerStaffs adjust_staff_more_height_key*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "TallerStaffs")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_TallerStaffs %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "TallerStaffs"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertTrebleClef newcleftreble*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertTrebleClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertTrebleClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertTrebleClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBassClef newclefbass*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertBassClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBassClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBassClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insertg8clef newclefg8*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Insertg8clef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insertg8clef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insertg8clef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertAltoClef newclefalto*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertAltoClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertAltoClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertAltoClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertTenorClef newcleftenor*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertTenorClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertTenorClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertTenorClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertSopranoClef newclefsoprano*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertSopranoClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertSopranoClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertSopranoClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialTrebleClef setcleftreble*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialTrebleClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialTrebleClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialTrebleClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialBassClef setclefbass*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialBassClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialBassClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialBassClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialg8clef setclefg8*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialg8clef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialg8clef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialg8clef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialAltoClef setclefalto*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialAltoClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialAltoClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialAltoClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialTenorClef setcleftenor*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialTenorClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialTenorClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialTenorClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialSopranoClef setclefsoprano*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialSopranoClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialSopranoClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialSopranoClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert22Time newtimesig22*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Insert22Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert22Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert22Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert32Time newtimesig32*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Insert32Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert32Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert32Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert42Time newtimesig42*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Insert42Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert42Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert42Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert44Time newtimesig44*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Insert44Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert44Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert44Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert34Time newtimesig34*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Insert34Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert34Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert34Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert24Time newtimesig24*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Insert24Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert24Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert24Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert64Time newtimesig64*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Insert64Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert64Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert64Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert38Time newtimesig38*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Insert38Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert38Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert38Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert68Time newtimesig68*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Insert68Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert68Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert68Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert128Time newtimesig128*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Insert128Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert128Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert128Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert98Time newtimesig98*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Insert98Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert98Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Insert98Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set22Time settimesig22*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Set22Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set22Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set22Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set32Time settimesig32*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Set32Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set32Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set32Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set42Time settimesig42*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Set42Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set42Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set42Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set44Time settimesig44*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Set44Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set44Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set44Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set34Time settimesig34*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Set34Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set34Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set34Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set24Time settimesig24*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Set24Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set24Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set24Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set64Time settimesig64*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Set64Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set64Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set64Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set38Time settimesig38*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Set38Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set38Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set38Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set68Time settimesig68*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Set68Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set68Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set68Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set128Time settimesig128*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Set128Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set128Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set128Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set98Time settimesig98*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Set98Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set98Time %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Set98Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertCmaj newkeysigcmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertCmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertCmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertCmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertGmaj newkeysiggmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertGmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertGmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertGmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDmaj newkeysigdmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertDmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertDmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertDmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertAmaj newkeysigamaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertAmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertAmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertAmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertEmaj newkeysigemaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertEmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertEmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertEmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBmaj newkeysigbmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertBmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertFSharpmaj newkeysigfsharpmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertFSharpmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertFSharpmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertFSharpmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertCSharpmaj newkeysigcsharpmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertCSharpmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertCSharpmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertCSharpmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertFmaj newkeysigfmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertFmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertFmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertFmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBflatmaj newkeysigbflatmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertBflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertEflatmaj newkeysigeflatmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertEflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertEflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertEflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertAflatmaj newkeysigaflatmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertAflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertAflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertAflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDflatmaj newkeysigdflatmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertDflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertDflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertDflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertGflatmaj newkeysiggflatmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertGflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertGflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertGflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertCflatmaj newkeysigcflatmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertCflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertCflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertCflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertAmin newkeysigamin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertAmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertAmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertAmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertEmin newkeysigemin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertEmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertEmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertEmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBmin newkeysigbmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertBmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertFSharpmin newkeysigfsharpmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertFSharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertFSharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertFSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertCSharpmin newkeysigcsharpmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertCSharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertCSharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertCSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertGSharpmin newkeysiggsharpmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertGSharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertGSharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertGSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDSharpmin newkeysigdsharpmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertDSharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertDSharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertDSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertASharpmin newkeysigasharpmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertASharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertASharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertASharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDmin newkeysigdmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertDmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertDmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertDmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertGmin newkeysiggmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertGmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertGmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertGmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertCmin newkeysigcmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertCmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertCmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertCmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertFmin newkeysigfmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertFmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertFmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertFmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBflatmin newkeysigbflatmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertBflatmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBflatmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertEflatmin newkeysigeflatmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertEflatmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertEflatmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertEflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertAflatmin newkeysigaflatmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertAflatmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertAflatmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertAflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialCmaj setkeysigcmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialCmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialCmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialCmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialGmaj setkeysiggmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialGmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialGmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialGmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialDmaj setkeysigdmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialDmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialDmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialDmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialAmaj setkeysigamaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialAmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialAmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialAmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialEmaj setkeysigemaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialEmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialEmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialEmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialBmaj setkeysigbmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialBmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialBmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialBmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialFSharpmaj setkeysigfsharpmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialFSharpmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialFSharpmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialFSharpmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialCSharpmaj setkeysigcsharpmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialCSharpmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialCSharpmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialCSharpmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialFmaj setkeysigfmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialFmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialFmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialFmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialBflatmaj setkeysigbflatmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialBflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialBflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialBflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialEflatmaj setkeysigeflatmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialEflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialEflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialEflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialAflatmaj setkeysigaflatmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialAflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialAflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialAflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialDflatmaj setkeysigdflatmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialDflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialDflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialDflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialGflatmaj setkeysiggflatmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialGflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialGflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialGflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialCflatmaj setkeysigcflatmaj*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialCflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialCflatmaj %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialCflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialAmin setkeysigamin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialAmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialAmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialAmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialEmin setkeysigemin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialEmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialEmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialEmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialBmin setkeysigbmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialBmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialBmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialBmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialFSharpmin setkeysigfsharpmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialFSharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialFSharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialFSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialCSharpmin setkeysigcsharpmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialCSharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialCSharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialCSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialGSharpmin setkeysiggsharpmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialGSharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialGSharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialGSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialDSharpmin setkeysigdsharpmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialDSharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialDSharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialDSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialASharpmin setkeysigasharpmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialASharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialASharpmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialASharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialDmin setkeysigdmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialDmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialDmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialDmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialGmin setkeysiggmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialGmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialGmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialGmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialCmin setkeysigcmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialCmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialCmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialCmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialFmin setkeysigfmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialFmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialFmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialFmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialBflatmin setkeysigbflatmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialBflatmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialBflatmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialBflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialEflatmin setkeysigeflatmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialEflatmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialEflatmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialEflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialAflatmin setkeysigaflatmin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetInitialAflatmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialAflatmin %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetInitialAflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetMark set_mark*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetMark")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetMark %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetMark"));
(void)scm_c_eval_string(text);
g_free(text);
/*UnsetMark unset_mark*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "UnsetMark")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_UnsetMark %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "UnsetMark"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleBeginSlur toggle_begin_slur*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleBeginSlur")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleBeginSlur %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleBeginSlur"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleEndSlur toggle_end_slur*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleEndSlur")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleEndSlur %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleEndSlur"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleStartCrescendo toggle_start_crescendo*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleStartCrescendo")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleStartCrescendo %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleStartCrescendo"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleEndCrescendo toggle_end_crescendo*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleEndCrescendo")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleEndCrescendo %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleEndCrescendo"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleStartDiminuendo toggle_start_diminuendo*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleStartDiminuendo")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleStartDiminuendo %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleStartDiminuendo"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleEndDiminuendo toggle_end_diminuendo*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleEndDiminuendo")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleEndDiminuendo %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleEndDiminuendo"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleAccent add_accent*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleAccent")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleAccent %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleAccent"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleFermata add_fermata*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleFermata")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleFermata %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleFermata"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleStaccato add_staccato*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleStaccato")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleStaccato %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleStaccato"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleTenuto add_tenuto*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleTenuto")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleTenuto %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleTenuto"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleTrill add_trill*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleTrill")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleTrill %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleTrill"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleTurn add_turn*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleTurn")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleTurn %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleTurn"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleMordent add_mordent*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleMordent")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleMordent %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleMordent"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleStaccatissimo add_staccatissimo*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleStaccatissimo")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleStaccatissimo %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleStaccatissimo"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleCoda add_coda*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleCoda")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleCoda %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleCoda"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleFlageolet add_flageolet*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleFlageolet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleFlageolet %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleFlageolet"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleOpen add_open*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleOpen")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleOpen %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleOpen"));
(void)scm_c_eval_string(text);
g_free(text);
/*TogglePrallMordent add_prallmordent*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "TogglePrallMordent")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_TogglePrallMordent %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "TogglePrallMordent"));
(void)scm_c_eval_string(text);
g_free(text);
/*TogglePrallPrall add_prallprall*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "TogglePrallPrall")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_TogglePrallPrall %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "TogglePrallPrall"));
(void)scm_c_eval_string(text);
g_free(text);
/*TogglePrall add_prall*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "TogglePrall")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_TogglePrall %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "TogglePrall"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleReverseTurn add_reverseturn*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleReverseTurn")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleReverseTurn %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleReverseTurn"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleSegno add_segno*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleSegno")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleSegno %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleSegno"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleSforzato add_sforzato*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleSforzato")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleSforzato %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleSforzato"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleStopped add_stopped*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleStopped")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleStopped %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleStopped"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleThumb add_thumb*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleThumb")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleThumb %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleThumb"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleUpprall add_upprall*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleUpprall")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleUpprall %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleUpprall"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleArpeggio add_arpeggio*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleArpeggio")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleArpeggio %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleArpeggio"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetGrace set_grace*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SetGrace")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetGrace %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SetGrace"));
(void)scm_c_eval_string(text);
g_free(text);
/*ForceCaution force_cautionary*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ForceCaution")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ForceCaution %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ForceCaution"));
(void)scm_c_eval_string(text);
g_free(text);
/*ChangePitch change_pitch*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ChangePitch")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ChangePitch %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ChangePitch"));
(void)scm_c_eval_string(text);
g_free(text);
/*DoubleBar insert_doublebar*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "DoubleBar")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DoubleBar %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DoubleBar"));
(void)scm_c_eval_string(text);
g_free(text);
/*EndBar insert_endbar*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "EndBar")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_EndBar %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "EndBar"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenRepeat insert_openrepeat*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "OpenRepeat")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_OpenRepeat %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "OpenRepeat"));
(void)scm_c_eval_string(text);
g_free(text);
/*CloseRepeat insert_closerepeat*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "CloseRepeat")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_CloseRepeat %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "CloseRepeat"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenCloseRepeat insert_opencloserepeat*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "OpenCloseRepeat")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_OpenCloseRepeat %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "OpenCloseRepeat"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertRhythm insert_rhythm_pattern*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertRhythm")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertRhythm %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertRhythm"));
(void)scm_c_eval_string(text);
g_free(text);
/*NextRhythm nextrhythm*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "NextRhythm")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_NextRhythm %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "NextRhythm"));
(void)scm_c_eval_string(text);
g_free(text);
/*AppendMesauresToScore append_measure_score*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "AppendMesauresToScore")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AppendMesauresToScore %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AppendMesauresToScore"));
(void)scm_c_eval_string(text);
g_free(text);
/*New file_newwrapper*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "New")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_New %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "New"));
(void)scm_c_eval_string(text);
g_free(text);
/*Open file_open_with_check*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Open")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Open %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Open"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddStaffs file_add_staffs*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "AddStaffs")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddStaffs %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddStaffs"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddMovements file_add_movements*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "AddMovements")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddMovements %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddMovements"));
(void)scm_c_eval_string(text);
g_free(text);
/*MovementProps movement_props_dialog*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "MovementProps")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_MovementProps %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "MovementProps"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenNewWindow openinnew*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "OpenNewWindow")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_OpenNewWindow %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "OpenNewWindow"));
(void)scm_c_eval_string(text);
g_free(text);
/*Save file_savewrapper*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Save")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Save %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Save"));
(void)scm_c_eval_string(text);
g_free(text);
/*SaveAs file_saveaswrapper*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SaveAs")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SaveAs %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SaveAs"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenTemplate system_template_open_with_check*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "OpenTemplate")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_OpenTemplate %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "OpenTemplate"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenExample system_example_open_with_check*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "OpenExample")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_OpenExample %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "OpenExample"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenMyTemplate local_template_open_with_check*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "OpenMyTemplate")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_OpenMyTemplate %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "OpenMyTemplate"));
(void)scm_c_eval_string(text);
g_free(text);
/*SaveTemplate template_save*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SaveTemplate")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SaveTemplate %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SaveTemplate"));
(void)scm_c_eval_string(text);
g_free(text);
/*NewWindow newview*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "NewWindow")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_NewWindow %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "NewWindow"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertMovementBefore insert_movement_before*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertMovementBefore")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertMovementBefore %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertMovementBefore"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertMovementAfter insert_movement_after*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertMovementAfter")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertMovementAfter %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertMovementAfter"));
(void)scm_c_eval_string(text);
g_free(text);
/*SaveParts file_savepartswrapper*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SaveParts")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SaveParts %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SaveParts"));
(void)scm_c_eval_string(text);
g_free(text);
/*ExportPDF export_pdf_action*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ExportPDF")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ExportPDF %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ExportPDF"));
(void)scm_c_eval_string(text);
g_free(text);
/*ConfigureScore scorewizard*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ConfigureScore")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ConfigureScore %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ConfigureScore"));
(void)scm_c_eval_string(text);
g_free(text);
/*PrintPreview printpreview_cb*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "PrintPreview")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_PrintPreview %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "PrintPreview"));
(void)scm_c_eval_string(text);
g_free(text);
/*PrintExcerptPreview printexcerptpreview_cb*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "PrintExcerptPreview")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_PrintExcerptPreview %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "PrintExcerptPreview"));
(void)scm_c_eval_string(text);
g_free(text);
/*Print printall_cb*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Print")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Print %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Print"));
(void)scm_c_eval_string(text);
g_free(text);
/*PrintPart printpart_cb*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "PrintPart")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_PrintPart %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "PrintPart"));
(void)scm_c_eval_string(text);
g_free(text);
/*Close close_gui_with_check*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Close")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Close %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Close"));
(void)scm_c_eval_string(text);
g_free(text);
/*Quit closewrapper*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Quit")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Quit %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Quit"));
(void)scm_c_eval_string(text);
g_free(text);
/*Undo undowrapper*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Undo")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Undo %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Undo"));
(void)scm_c_eval_string(text);
g_free(text);
/*Redo redowrapper*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Redo")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Redo %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Redo"));
(void)scm_c_eval_string(text);
g_free(text);
/*Copy copywrapper*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Copy")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Copy %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Copy"));
(void)scm_c_eval_string(text);
g_free(text);
/*Cut cutwrapper*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Cut")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Cut %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Cut"));
(void)scm_c_eval_string(text);
g_free(text);
/*Paste pastewrapper*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Paste")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Paste %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Paste"));
(void)scm_c_eval_string(text);
g_free(text);
/*ScoreProperties score_properties_dialog*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ScoreProperties")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ScoreProperties %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ScoreProperties"));
(void)scm_c_eval_string(text);
g_free(text);
/*SaveSelection saveselwrapper*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SaveSelection")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SaveSelection %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SaveSelection"));
(void)scm_c_eval_string(text);
g_free(text);
/*Preferences preferences_change*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Preferences")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Preferences %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Preferences"));
(void)scm_c_eval_string(text);
g_free(text);
/*SaveAccels save_default_keymap_file_wrapper*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SaveAccels")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SaveAccels %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SaveAccels"));
(void)scm_c_eval_string(text);
g_free(text);
/*Keyboard configure_keyboard_dialog*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Keyboard")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Keyboard %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Keyboard"));
(void)scm_c_eval_string(text);
g_free(text);
/*LoadPlugins load_plugin*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "LoadPlugins")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_LoadPlugins %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "LoadPlugins"));
(void)scm_c_eval_string(text);
g_free(text);
/*UnloadPlugins unloadplugins*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "UnloadPlugins")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_UnloadPlugins %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "UnloadPlugins"));
(void)scm_c_eval_string(text);
g_free(text);
/*ListPlugins list_loaded_plugins*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ListPlugins")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ListPlugins %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ListPlugins"));
(void)scm_c_eval_string(text);
g_free(text);
/*ListAvailablePlugins list_available_plugins*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ListAvailablePlugins")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ListAvailablePlugins %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ListAvailablePlugins"));
(void)scm_c_eval_string(text);
g_free(text);
/*SwapStaffs swapstaffs*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SwapStaffs")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SwapStaffs %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SwapStaffs"));
(void)scm_c_eval_string(text);
g_free(text);
/*SplitVoices splitstaffs*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SplitVoices")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SplitVoices %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SplitVoices"));
(void)scm_c_eval_string(text);
g_free(text);
/*JoinVoices joinstaffs*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "JoinVoices")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_JoinVoices %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "JoinVoices"));
(void)scm_c_eval_string(text);
g_free(text);
/*SwapMovements swapmovements*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "SwapMovements")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SwapMovements %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "SwapMovements"));
(void)scm_c_eval_string(text);
g_free(text);
/*VoiceUp voiceup*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "VoiceUp")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_VoiceUp %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "VoiceUp"));
(void)scm_c_eval_string(text);
g_free(text);
/*VoiceDown voicedown*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "VoiceDown")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_VoiceDown %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "VoiceDown"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddBefore newstaffbefore*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "AddBefore")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddBefore %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddBefore"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddAfter dnm_newstaffafter*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "AddAfter")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddAfter %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddAfter"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddInitial newstaffinitial*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "AddInitial")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddInitial %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddInitial"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddLast newstafflast*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "AddLast")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddLast %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddLast"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteBefore delete_staff_before*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "DeleteBefore")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteBefore %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteBefore"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteStaff delete_staff_current*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "DeleteStaff")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteStaff %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteStaff"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteAfter delete_staff_after*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "DeleteAfter")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteAfter %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteAfter"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddVoice dnm_newstaffvoice*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "AddVoice")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddVoice %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddVoice"));
(void)scm_c_eval_string(text);
g_free(text);
/*TransposeStaff staff_transposition*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "TransposeStaff")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_TransposeStaff %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "TransposeStaff"));
(void)scm_c_eval_string(text);
g_free(text);
/*StaffProperties staff_properties_change_cb*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "StaffProperties")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_StaffProperties %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "StaffProperties"));
(void)scm_c_eval_string(text);
g_free(text);
/*InitialClef clef_change_initial*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InitialClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InitialClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InitialClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertClef clef_change_insert*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertClef %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InitialKey key_change_initial*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InitialKey")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InitialKey %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InitialKey"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertKey key_change_insert*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertKey")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertKey %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertKey"));
(void)scm_c_eval_string(text);
g_free(text);
/*InitialTimeSig timesig_change_initial*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InitialTimeSig")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InitialTimeSig %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InitialTimeSig"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertTimeSig timesig_change_insert*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertTimeSig")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertTimeSig %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertTimeSig"));
(void)scm_c_eval_string(text);
g_free(text);
/*ChangeNotehead set_notehead*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ChangeNotehead")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ChangeNotehead %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ChangeNotehead"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertStem stem_directive_insert*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertStem")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertStem %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertStem"));
(void)scm_c_eval_string(text);
g_free(text);
/*EditLyric lyric_insert*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "EditLyric")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_EditLyric %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "EditLyric"));
(void)scm_c_eval_string(text);
g_free(text);
/*EditFiguredBass figure_insert*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "EditFiguredBass")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_EditFiguredBass %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "EditFiguredBass"));
(void)scm_c_eval_string(text);
g_free(text);
/*EditChords fakechord_insert*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "EditChords")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_EditChords %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "EditChords"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDynamic insert_dynamic*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertDynamic")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertDynamic %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertDynamic"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertLilyDirective lily_directive_insert*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertLilyDirective")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertLilyDirective %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertLilyDirective"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertLilyPostfix lily_directive_postfix*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertLilyPostfix")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertLilyPostfix %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertLilyPostfix"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBarline insert_barline*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "InsertBarline")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBarline %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "InsertBarline"));
(void)scm_c_eval_string(text);
g_free(text);
/*GoToMeasure tomeasurenum*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "GoToMeasure")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_GoToMeasure %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "GoToMeasure"));
(void)scm_c_eval_string(text);
g_free(text);
/*GoToBeginning tohome*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "GoToBeginning")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_GoToBeginning %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "GoToBeginning"));
(void)scm_c_eval_string(text);
g_free(text);
/*GoToEnd toend*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "GoToEnd")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_GoToEnd %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "GoToEnd"));
(void)scm_c_eval_string(text);
g_free(text);
/*NextMovement next_movement*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "NextMovement")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_NextMovement %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "NextMovement"));
(void)scm_c_eval_string(text);
g_free(text);
/*PreviousMovement prev_movement*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "PreviousMovement")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_PreviousMovement %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "PreviousMovement"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteMovement delete_movement*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "DeleteMovement")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteMovement %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteMovement"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteBookmarks deletebookmarks*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "DeleteBookmarks")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteBookmarks %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteBookmarks"));
(void)scm_c_eval_string(text);
g_free(text);
/*Play ext_midi_playback*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Play")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Play %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Play"));
(void)scm_c_eval_string(text);
g_free(text);
/*Stop stop_midi_playback*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Stop")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Stop %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Stop"));
(void)scm_c_eval_string(text);
g_free(text);
/*PlayCSound dnm_csoundplayback*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "PlayCSound")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_PlayCSound %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "PlayCSound"));
(void)scm_c_eval_string(text);
g_free(text);
/*PlaybackProperties playback_properties_change*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "PlaybackProperties")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_PlaybackProperties %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "PlaybackProperties"));
(void)scm_c_eval_string(text);
g_free(text);
/*Help browse_manual*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "Help")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Help %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "Help"));
(void)scm_c_eval_string(text);
g_free(text);
/*About about*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "About")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_About %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "About"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddBookmark addbookmark*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "AddBookmark")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddBookmark %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "AddBookmark"));
(void)scm_c_eval_string(text);
g_free(text);
/*GotoBookmark gotobookmark*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "GotoBookmark")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_GotoBookmark %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "GotoBookmark"));
(void)scm_c_eval_string(text);
g_free(text);
/*NextBookmark nextbookmark*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "NextBookmark")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_NextBookmark %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "NextBookmark"));
(void)scm_c_eval_string(text);
g_free(text);
/*PrevBookmark prevbookmark*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "PrevBookmark")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_PrevBookmark %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "PrevBookmark"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleEdit toggle_edit_mode*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleEdit")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleEdit %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleEdit"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleRest toggle_rest_mode*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleRest")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleRest %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleRhythm toggle_rhythm_mode*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ToggleRhythm")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleRhythm %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ToggleRhythm"));
(void)scm_c_eval_string(text);
g_free(text);
/*ClearOverlay clear_overlay*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "ClearOverlay")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ClearOverlay %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "ClearOverlay"));
(void)scm_c_eval_string(text);
g_free(text);
/*CreateRhythm create_rhythm_cb*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "CreateRhythm")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_CreateRhythm %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "CreateRhythm"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteRhythm delete_rhythm_cb*/
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "DeleteRhythm")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteRhythm %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "DeleteRhythm"));
(void)scm_c_eval_string(text);
g_free(text);
/*0 */
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "0")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_0 %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "0"));
(void)scm_c_eval_string(text);
g_free(text);
/*1 */
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "1")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_1 %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "1"));
(void)scm_c_eval_string(text);
g_free(text);
/*2 */
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "2")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_2 %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "2"));
(void)scm_c_eval_string(text);
g_free(text);
/*3 */
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "3")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_3 %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "3"));
(void)scm_c_eval_string(text);
g_free(text);
/*4 */
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "4")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_4 %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "4"));
(void)scm_c_eval_string(text);
g_free(text);
/*5 */
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "5")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_5 %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "5"));
(void)scm_c_eval_string(text);
g_free(text);
/*6 */
g_object_set_data(G_OBJECT(action_of_name(Denemo.prefs.the_keymap, "6")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_6 %d)\n", (int)action_of_name(Denemo.prefs.the_keymap, "6"));
(void)scm_c_eval_string(text);
g_free(text);
