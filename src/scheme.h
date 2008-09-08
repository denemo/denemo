gchar *text;
/*CursorLeft cursorleft*/
SCM scheme_CursorLeft(SCM optional);
install_scm_function ("d-CursorLeft", scheme_CursorLeft);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "CursorLeft")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_CursorLeft %d)\n", (int)action_of_name(Denemo.commands, "CursorLeft"));
(void)scm_c_eval_string(text);
g_free(text);
/*CursorDown cursordown*/
SCM scheme_CursorDown(SCM optional);
install_scm_function ("d-CursorDown", scheme_CursorDown);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "CursorDown")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_CursorDown %d)\n", (int)action_of_name(Denemo.commands, "CursorDown"));
(void)scm_c_eval_string(text);
g_free(text);
/*CursorUp cursorup*/
SCM scheme_CursorUp(SCM optional);
install_scm_function ("d-CursorUp", scheme_CursorUp);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "CursorUp")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_CursorUp %d)\n", (int)action_of_name(Denemo.commands, "CursorUp"));
(void)scm_c_eval_string(text);
g_free(text);
/*CursorRight cursorright*/
SCM scheme_CursorRight(SCM optional);
install_scm_function ("d-CursorRight", scheme_CursorRight);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "CursorRight")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_CursorRight %d)\n", (int)action_of_name(Denemo.commands, "CursorRight"));
(void)scm_c_eval_string(text);
g_free(text);
/*StaffUp staffup*/
SCM scheme_StaffUp(SCM optional);
install_scm_function ("d-StaffUp", scheme_StaffUp);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "StaffUp")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_StaffUp %d)\n", (int)action_of_name(Denemo.commands, "StaffUp"));
(void)scm_c_eval_string(text);
g_free(text);
/*StaffDown staffdown*/
SCM scheme_StaffDown(SCM optional);
install_scm_function ("d-StaffDown", scheme_StaffDown);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "StaffDown")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_StaffDown %d)\n", (int)action_of_name(Denemo.commands, "StaffDown"));
(void)scm_c_eval_string(text);
g_free(text);
/*MeasureLeft measureleft*/
SCM scheme_MeasureLeft(SCM optional);
install_scm_function ("d-MeasureLeft", scheme_MeasureLeft);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "MeasureLeft")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_MeasureLeft %d)\n", (int)action_of_name(Denemo.commands, "MeasureLeft"));
(void)scm_c_eval_string(text);
g_free(text);
/*MeasureRight measureright*/
SCM scheme_MeasureRight(SCM optional);
install_scm_function ("d-MeasureRight", scheme_MeasureRight);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "MeasureRight")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_MeasureRight %d)\n", (int)action_of_name(Denemo.commands, "MeasureRight"));
(void)scm_c_eval_string(text);
g_free(text);
/*A go_to_A_key*/
SCM scheme_A(SCM optional);
install_scm_function ("d-A", scheme_A);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "A")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_A %d)\n", (int)action_of_name(Denemo.commands, "A"));
(void)scm_c_eval_string(text);
g_free(text);
/*B go_to_B_key*/
SCM scheme_B(SCM optional);
install_scm_function ("d-B", scheme_B);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "B")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_B %d)\n", (int)action_of_name(Denemo.commands, "B"));
(void)scm_c_eval_string(text);
g_free(text);
/*C go_to_C_key*/
SCM scheme_C(SCM optional);
install_scm_function ("d-C", scheme_C);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "C")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_C %d)\n", (int)action_of_name(Denemo.commands, "C"));
(void)scm_c_eval_string(text);
g_free(text);
/*D go_to_D_key*/
SCM scheme_D(SCM optional);
install_scm_function ("d-D", scheme_D);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "D")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_D %d)\n", (int)action_of_name(Denemo.commands, "D"));
(void)scm_c_eval_string(text);
g_free(text);
/*E go_to_E_key*/
SCM scheme_E(SCM optional);
install_scm_function ("d-E", scheme_E);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "E")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_E %d)\n", (int)action_of_name(Denemo.commands, "E"));
(void)scm_c_eval_string(text);
g_free(text);
/*F go_to_F_key*/
SCM scheme_F(SCM optional);
install_scm_function ("d-F", scheme_F);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "F")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_F %d)\n", (int)action_of_name(Denemo.commands, "F"));
(void)scm_c_eval_string(text);
g_free(text);
/*G go_to_G_key*/
SCM scheme_G(SCM optional);
install_scm_function ("d-G", scheme_G);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "G")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_G %d)\n", (int)action_of_name(Denemo.commands, "G"));
(void)scm_c_eval_string(text);
g_free(text);
/*OctaveUp octave_up_key*/
SCM scheme_OctaveUp(SCM optional);
install_scm_function ("d-OctaveUp", scheme_OctaveUp);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "OctaveUp")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_OctaveUp %d)\n", (int)action_of_name(Denemo.commands, "OctaveUp"));
(void)scm_c_eval_string(text);
g_free(text);
/*OctaveDown octave_down_key*/
SCM scheme_OctaveDown(SCM optional);
install_scm_function ("d-OctaveDown", scheme_OctaveDown);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "OctaveDown")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_OctaveDown %d)\n", (int)action_of_name(Denemo.commands, "OctaveDown"));
(void)scm_c_eval_string(text);
g_free(text);
/*WholeNote insert_chord_0key*/
SCM scheme_WholeNote(SCM optional);
install_scm_function ("d-WholeNote", scheme_WholeNote);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "WholeNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_WholeNote %d)\n", (int)action_of_name(Denemo.commands, "WholeNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*HalfNote insert_chord_1key*/
SCM scheme_HalfNote(SCM optional);
install_scm_function ("d-HalfNote", scheme_HalfNote);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "HalfNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_HalfNote %d)\n", (int)action_of_name(Denemo.commands, "HalfNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*QuarterNote insert_chord_2key*/
SCM scheme_QuarterNote(SCM optional);
install_scm_function ("d-QuarterNote", scheme_QuarterNote);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "QuarterNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_QuarterNote %d)\n", (int)action_of_name(Denemo.commands, "QuarterNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*EighthNote insert_chord_3key*/
SCM scheme_EighthNote(SCM optional);
install_scm_function ("d-EighthNote", scheme_EighthNote);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "EighthNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_EighthNote %d)\n", (int)action_of_name(Denemo.commands, "EighthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*SixteenthNote insert_chord_4key*/
SCM scheme_SixteenthNote(SCM optional);
install_scm_function ("d-SixteenthNote", scheme_SixteenthNote);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SixteenthNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SixteenthNote %d)\n", (int)action_of_name(Denemo.commands, "SixteenthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*ThirtysecondNote insert_chord_5key*/
SCM scheme_ThirtysecondNote(SCM optional);
install_scm_function ("d-ThirtysecondNote", scheme_ThirtysecondNote);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ThirtysecondNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ThirtysecondNote %d)\n", (int)action_of_name(Denemo.commands, "ThirtysecondNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*SixtyfourthNote insert_chord_6key*/
SCM scheme_SixtyfourthNote(SCM optional);
install_scm_function ("d-SixtyfourthNote", scheme_SixtyfourthNote);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SixtyfourthNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SixtyfourthNote %d)\n", (int)action_of_name(Denemo.commands, "SixtyfourthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankWholeNote insert_blankchord_0key*/
SCM scheme_InsertBlankWholeNote(SCM optional);
install_scm_function ("d-InsertBlankWholeNote", scheme_InsertBlankWholeNote);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertBlankWholeNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBlankWholeNote %d)\n", (int)action_of_name(Denemo.commands, "InsertBlankWholeNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankHalfNote insert_blankchord_1key*/
SCM scheme_InsertBlankHalfNote(SCM optional);
install_scm_function ("d-InsertBlankHalfNote", scheme_InsertBlankHalfNote);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertBlankHalfNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBlankHalfNote %d)\n", (int)action_of_name(Denemo.commands, "InsertBlankHalfNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankQuarterNote insert_blankchord_2key*/
SCM scheme_InsertBlankQuarterNote(SCM optional);
install_scm_function ("d-InsertBlankQuarterNote", scheme_InsertBlankQuarterNote);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertBlankQuarterNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBlankQuarterNote %d)\n", (int)action_of_name(Denemo.commands, "InsertBlankQuarterNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankEighthNote insert_blankchord_3key*/
SCM scheme_InsertBlankEighthNote(SCM optional);
install_scm_function ("d-InsertBlankEighthNote", scheme_InsertBlankEighthNote);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertBlankEighthNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBlankEighthNote %d)\n", (int)action_of_name(Denemo.commands, "InsertBlankEighthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankSixteenthNote insert_blankchord_4key*/
SCM scheme_InsertBlankSixteenthNote(SCM optional);
install_scm_function ("d-InsertBlankSixteenthNote", scheme_InsertBlankSixteenthNote);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertBlankSixteenthNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBlankSixteenthNote %d)\n", (int)action_of_name(Denemo.commands, "InsertBlankSixteenthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankThirtysecondNote insert_blankchord_5key*/
SCM scheme_InsertBlankThirtysecondNote(SCM optional);
install_scm_function ("d-InsertBlankThirtysecondNote", scheme_InsertBlankThirtysecondNote);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertBlankThirtysecondNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBlankThirtysecondNote %d)\n", (int)action_of_name(Denemo.commands, "InsertBlankThirtysecondNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBlankSixtyfourthNote insert_blankchord_6key*/
SCM scheme_InsertBlankSixtyfourthNote(SCM optional);
install_scm_function ("d-InsertBlankSixtyfourthNote", scheme_InsertBlankSixtyfourthNote);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertBlankSixtyfourthNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBlankSixtyfourthNote %d)\n", (int)action_of_name(Denemo.commands, "InsertBlankSixtyfourthNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleRestMode rest_toggle_key*/
SCM scheme_ToggleRestMode(SCM optional);
install_scm_function ("d-ToggleRestMode", scheme_ToggleRestMode);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleRestMode")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleRestMode %d)\n", (int)action_of_name(Denemo.commands, "ToggleRestMode"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleBlankMode toggle_blank*/
SCM scheme_ToggleBlankMode(SCM optional);
install_scm_function ("d-ToggleBlankMode", scheme_ToggleBlankMode);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleBlankMode")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleBlankMode %d)\n", (int)action_of_name(Denemo.commands, "ToggleBlankMode"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertWholeRest insert_rest_0key*/
SCM scheme_InsertWholeRest(SCM optional);
install_scm_function ("d-InsertWholeRest", scheme_InsertWholeRest);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertWholeRest")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertWholeRest %d)\n", (int)action_of_name(Denemo.commands, "InsertWholeRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertHalfRest insert_rest_1key*/
SCM scheme_InsertHalfRest(SCM optional);
install_scm_function ("d-InsertHalfRest", scheme_InsertHalfRest);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertHalfRest")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertHalfRest %d)\n", (int)action_of_name(Denemo.commands, "InsertHalfRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertQuarterRest insert_rest_2key*/
SCM scheme_InsertQuarterRest(SCM optional);
install_scm_function ("d-InsertQuarterRest", scheme_InsertQuarterRest);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertQuarterRest")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertQuarterRest %d)\n", (int)action_of_name(Denemo.commands, "InsertQuarterRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertEighthRest insert_rest_3key*/
SCM scheme_InsertEighthRest(SCM optional);
install_scm_function ("d-InsertEighthRest", scheme_InsertEighthRest);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertEighthRest")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertEighthRest %d)\n", (int)action_of_name(Denemo.commands, "InsertEighthRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertSixteenthRest insert_rest_4key*/
SCM scheme_InsertSixteenthRest(SCM optional);
install_scm_function ("d-InsertSixteenthRest", scheme_InsertSixteenthRest);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertSixteenthRest")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertSixteenthRest %d)\n", (int)action_of_name(Denemo.commands, "InsertSixteenthRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertThirtysecondRest insert_rest_5key*/
SCM scheme_InsertThirtysecondRest(SCM optional);
install_scm_function ("d-InsertThirtysecondRest", scheme_InsertThirtysecondRest);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertThirtysecondRest")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertThirtysecondRest %d)\n", (int)action_of_name(Denemo.commands, "InsertThirtysecondRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertSixtyfourthRest insert_rest_6key*/
SCM scheme_InsertSixtyfourthRest(SCM optional);
install_scm_function ("d-InsertSixtyfourthRest", scheme_InsertSixtyfourthRest);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertSixtyfourthRest")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertSixtyfourthRest %d)\n", (int)action_of_name(Denemo.commands, "InsertSixtyfourthRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDuplet insert_duplet*/
SCM scheme_InsertDuplet(SCM optional);
install_scm_function ("d-InsertDuplet", scheme_InsertDuplet);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertDuplet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertDuplet %d)\n", (int)action_of_name(Denemo.commands, "InsertDuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertTriplet insert_triplet*/
SCM scheme_InsertTriplet(SCM optional);
install_scm_function ("d-InsertTriplet", scheme_InsertTriplet);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertTriplet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertTriplet %d)\n", (int)action_of_name(Denemo.commands, "InsertTriplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*StartTriplet start_triplet*/
SCM scheme_StartTriplet(SCM optional);
install_scm_function ("d-StartTriplet", scheme_StartTriplet);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "StartTriplet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_StartTriplet %d)\n", (int)action_of_name(Denemo.commands, "StartTriplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*EndTuplet end_tuplet*/
SCM scheme_EndTuplet(SCM optional);
install_scm_function ("d-EndTuplet", scheme_EndTuplet);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "EndTuplet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_EndTuplet %d)\n", (int)action_of_name(Denemo.commands, "EndTuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertQuadtuplet insert_quadtuplet*/
SCM scheme_InsertQuadtuplet(SCM optional);
install_scm_function ("d-InsertQuadtuplet", scheme_InsertQuadtuplet);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertQuadtuplet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertQuadtuplet %d)\n", (int)action_of_name(Denemo.commands, "InsertQuadtuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertQuintuplet insert_quintuplet*/
SCM scheme_InsertQuintuplet(SCM optional);
install_scm_function ("d-InsertQuintuplet", scheme_InsertQuintuplet);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertQuintuplet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertQuintuplet %d)\n", (int)action_of_name(Denemo.commands, "InsertQuintuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertSextuplet insert_sextuplet*/
SCM scheme_InsertSextuplet(SCM optional);
install_scm_function ("d-InsertSextuplet", scheme_InsertSextuplet);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertSextuplet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertSextuplet %d)\n", (int)action_of_name(Denemo.commands, "InsertSextuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertSeptuplet insert_septuplet*/
SCM scheme_InsertSeptuplet(SCM optional);
install_scm_function ("d-InsertSeptuplet", scheme_InsertSeptuplet);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertSeptuplet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertSeptuplet %d)\n", (int)action_of_name(Denemo.commands, "InsertSeptuplet"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddNoteToChord add_tone_key*/
SCM scheme_AddNoteToChord(SCM optional);
install_scm_function ("d-AddNoteToChord", scheme_AddNoteToChord);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "AddNoteToChord")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddNoteToChord %d)\n", (int)action_of_name(Denemo.commands, "AddNoteToChord"));
(void)scm_c_eval_string(text);
g_free(text);
/*RemoveNoteFromChord remove_tone_key*/
SCM scheme_RemoveNoteFromChord(SCM optional);
install_scm_function ("d-RemoveNoteFromChord", scheme_RemoveNoteFromChord);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "RemoveNoteFromChord")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_RemoveNoteFromChord %d)\n", (int)action_of_name(Denemo.commands, "RemoveNoteFromChord"));
(void)scm_c_eval_string(text);
g_free(text);
/*SharpenOrStemDown sharpen_key*/
SCM scheme_SharpenOrStemDown(SCM optional);
install_scm_function ("d-SharpenOrStemDown", scheme_SharpenOrStemDown);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SharpenOrStemDown")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SharpenOrStemDown %d)\n", (int)action_of_name(Denemo.commands, "SharpenOrStemDown"));
(void)scm_c_eval_string(text);
g_free(text);
/*FlattenOrStemUp flatten_key*/
SCM scheme_FlattenOrStemUp(SCM optional);
install_scm_function ("d-FlattenOrStemUp", scheme_FlattenOrStemUp);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "FlattenOrStemUp")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_FlattenOrStemUp %d)\n", (int)action_of_name(Denemo.commands, "FlattenOrStemUp"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddDot add_dot_key*/
SCM scheme_AddDot(SCM optional);
install_scm_function ("d-AddDot", scheme_AddDot);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "AddDot")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddDot %d)\n", (int)action_of_name(Denemo.commands, "AddDot"));
(void)scm_c_eval_string(text);
g_free(text);
/*RemoveDot remove_dot_key*/
SCM scheme_RemoveDot(SCM optional);
install_scm_function ("d-RemoveDot", scheme_RemoveDot);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "RemoveDot")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_RemoveDot %d)\n", (int)action_of_name(Denemo.commands, "RemoveDot"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertTiedNote tie_notes_key*/
SCM scheme_InsertTiedNote(SCM optional);
install_scm_function ("d-InsertTiedNote", scheme_InsertTiedNote);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertTiedNote")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertTiedNote %d)\n", (int)action_of_name(Denemo.commands, "InsertTiedNote"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteObject deleteobject*/
SCM scheme_DeleteObject(SCM optional);
install_scm_function ("d-DeleteObject", scheme_DeleteObject);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "DeleteObject")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteObject %d)\n", (int)action_of_name(Denemo.commands, "DeleteObject"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeletePreviousObject deletepreviousobject*/
SCM scheme_DeletePreviousObject(SCM optional);
install_scm_function ("d-DeletePreviousObject", scheme_DeletePreviousObject);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "DeletePreviousObject")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeletePreviousObject %d)\n", (int)action_of_name(Denemo.commands, "DeletePreviousObject"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertMeasure insert_measure_key*/
SCM scheme_InsertMeasure(SCM optional);
install_scm_function ("d-InsertMeasure", scheme_InsertMeasure);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertMeasure")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertMeasure %d)\n", (int)action_of_name(Denemo.commands, "InsertMeasure"));
(void)scm_c_eval_string(text);
g_free(text);
/*AppendMeasure append_measure_key*/
SCM scheme_AppendMeasure(SCM optional);
install_scm_function ("d-AppendMeasure", scheme_AppendMeasure);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "AppendMeasure")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AppendMeasure %d)\n", (int)action_of_name(Denemo.commands, "AppendMeasure"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteMeasure deletemeasure*/
SCM scheme_DeleteMeasure(SCM optional);
install_scm_function ("d-DeleteMeasure", scheme_DeleteMeasure);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "DeleteMeasure")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteMeasure %d)\n", (int)action_of_name(Denemo.commands, "DeleteMeasure"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteMeasureAllStaffs deletemeasureallstaffs*/
SCM scheme_DeleteMeasureAllStaffs(SCM optional);
install_scm_function ("d-DeleteMeasureAllStaffs", scheme_DeleteMeasureAllStaffs);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "DeleteMeasureAllStaffs")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteMeasureAllStaffs %d)\n", (int)action_of_name(Denemo.commands, "DeleteMeasureAllStaffs"));
(void)scm_c_eval_string(text);
g_free(text);
/*ShrinkMeasures adjust_measure_less_width_key*/
SCM scheme_ShrinkMeasures(SCM optional);
install_scm_function ("d-ShrinkMeasures", scheme_ShrinkMeasures);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ShrinkMeasures")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ShrinkMeasures %d)\n", (int)action_of_name(Denemo.commands, "ShrinkMeasures"));
(void)scm_c_eval_string(text);
g_free(text);
/*WidenMeasures adjust_measure_more_width_key*/
SCM scheme_WidenMeasures(SCM optional);
install_scm_function ("d-WidenMeasures", scheme_WidenMeasures);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "WidenMeasures")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_WidenMeasures %d)\n", (int)action_of_name(Denemo.commands, "WidenMeasures"));
(void)scm_c_eval_string(text);
g_free(text);
/*ShorterStaffs adjust_staff_less_height_key*/
SCM scheme_ShorterStaffs(SCM optional);
install_scm_function ("d-ShorterStaffs", scheme_ShorterStaffs);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ShorterStaffs")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ShorterStaffs %d)\n", (int)action_of_name(Denemo.commands, "ShorterStaffs"));
(void)scm_c_eval_string(text);
g_free(text);
/*TallerStaffs adjust_staff_more_height_key*/
SCM scheme_TallerStaffs(SCM optional);
install_scm_function ("d-TallerStaffs", scheme_TallerStaffs);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "TallerStaffs")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_TallerStaffs %d)\n", (int)action_of_name(Denemo.commands, "TallerStaffs"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertTrebleClef newcleftreble*/
SCM scheme_InsertTrebleClef(SCM optional);
install_scm_function ("d-InsertTrebleClef", scheme_InsertTrebleClef);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertTrebleClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertTrebleClef %d)\n", (int)action_of_name(Denemo.commands, "InsertTrebleClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBassClef newclefbass*/
SCM scheme_InsertBassClef(SCM optional);
install_scm_function ("d-InsertBassClef", scheme_InsertBassClef);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertBassClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBassClef %d)\n", (int)action_of_name(Denemo.commands, "InsertBassClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insertg8clef newclefg8*/
SCM scheme_Insertg8clef(SCM optional);
install_scm_function ("d-Insertg8clef", scheme_Insertg8clef);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Insertg8clef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insertg8clef %d)\n", (int)action_of_name(Denemo.commands, "Insertg8clef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertAltoClef newclefalto*/
SCM scheme_InsertAltoClef(SCM optional);
install_scm_function ("d-InsertAltoClef", scheme_InsertAltoClef);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertAltoClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertAltoClef %d)\n", (int)action_of_name(Denemo.commands, "InsertAltoClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertTenorClef newcleftenor*/
SCM scheme_InsertTenorClef(SCM optional);
install_scm_function ("d-InsertTenorClef", scheme_InsertTenorClef);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertTenorClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertTenorClef %d)\n", (int)action_of_name(Denemo.commands, "InsertTenorClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertSopranoClef newclefsoprano*/
SCM scheme_InsertSopranoClef(SCM optional);
install_scm_function ("d-InsertSopranoClef", scheme_InsertSopranoClef);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertSopranoClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertSopranoClef %d)\n", (int)action_of_name(Denemo.commands, "InsertSopranoClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialTrebleClef setcleftreble*/
SCM scheme_SetInitialTrebleClef(SCM optional);
install_scm_function ("d-SetInitialTrebleClef", scheme_SetInitialTrebleClef);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialTrebleClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialTrebleClef %d)\n", (int)action_of_name(Denemo.commands, "SetInitialTrebleClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialBassClef setclefbass*/
SCM scheme_SetInitialBassClef(SCM optional);
install_scm_function ("d-SetInitialBassClef", scheme_SetInitialBassClef);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialBassClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialBassClef %d)\n", (int)action_of_name(Denemo.commands, "SetInitialBassClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialg8clef setclefg8*/
SCM scheme_SetInitialg8clef(SCM optional);
install_scm_function ("d-SetInitialg8clef", scheme_SetInitialg8clef);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialg8clef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialg8clef %d)\n", (int)action_of_name(Denemo.commands, "SetInitialg8clef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialAltoClef setclefalto*/
SCM scheme_SetInitialAltoClef(SCM optional);
install_scm_function ("d-SetInitialAltoClef", scheme_SetInitialAltoClef);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialAltoClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialAltoClef %d)\n", (int)action_of_name(Denemo.commands, "SetInitialAltoClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialTenorClef setcleftenor*/
SCM scheme_SetInitialTenorClef(SCM optional);
install_scm_function ("d-SetInitialTenorClef", scheme_SetInitialTenorClef);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialTenorClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialTenorClef %d)\n", (int)action_of_name(Denemo.commands, "SetInitialTenorClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialSopranoClef setclefsoprano*/
SCM scheme_SetInitialSopranoClef(SCM optional);
install_scm_function ("d-SetInitialSopranoClef", scheme_SetInitialSopranoClef);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialSopranoClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialSopranoClef %d)\n", (int)action_of_name(Denemo.commands, "SetInitialSopranoClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert22Time newtimesig22*/
SCM scheme_Insert22Time(SCM optional);
install_scm_function ("d-Insert22Time", scheme_Insert22Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Insert22Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert22Time %d)\n", (int)action_of_name(Denemo.commands, "Insert22Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert32Time newtimesig32*/
SCM scheme_Insert32Time(SCM optional);
install_scm_function ("d-Insert32Time", scheme_Insert32Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Insert32Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert32Time %d)\n", (int)action_of_name(Denemo.commands, "Insert32Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert42Time newtimesig42*/
SCM scheme_Insert42Time(SCM optional);
install_scm_function ("d-Insert42Time", scheme_Insert42Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Insert42Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert42Time %d)\n", (int)action_of_name(Denemo.commands, "Insert42Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert44Time newtimesig44*/
SCM scheme_Insert44Time(SCM optional);
install_scm_function ("d-Insert44Time", scheme_Insert44Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Insert44Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert44Time %d)\n", (int)action_of_name(Denemo.commands, "Insert44Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert34Time newtimesig34*/
SCM scheme_Insert34Time(SCM optional);
install_scm_function ("d-Insert34Time", scheme_Insert34Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Insert34Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert34Time %d)\n", (int)action_of_name(Denemo.commands, "Insert34Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert24Time newtimesig24*/
SCM scheme_Insert24Time(SCM optional);
install_scm_function ("d-Insert24Time", scheme_Insert24Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Insert24Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert24Time %d)\n", (int)action_of_name(Denemo.commands, "Insert24Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert64Time newtimesig64*/
SCM scheme_Insert64Time(SCM optional);
install_scm_function ("d-Insert64Time", scheme_Insert64Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Insert64Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert64Time %d)\n", (int)action_of_name(Denemo.commands, "Insert64Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert38Time newtimesig38*/
SCM scheme_Insert38Time(SCM optional);
install_scm_function ("d-Insert38Time", scheme_Insert38Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Insert38Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert38Time %d)\n", (int)action_of_name(Denemo.commands, "Insert38Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert68Time newtimesig68*/
SCM scheme_Insert68Time(SCM optional);
install_scm_function ("d-Insert68Time", scheme_Insert68Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Insert68Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert68Time %d)\n", (int)action_of_name(Denemo.commands, "Insert68Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert128Time newtimesig128*/
SCM scheme_Insert128Time(SCM optional);
install_scm_function ("d-Insert128Time", scheme_Insert128Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Insert128Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert128Time %d)\n", (int)action_of_name(Denemo.commands, "Insert128Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Insert98Time newtimesig98*/
SCM scheme_Insert98Time(SCM optional);
install_scm_function ("d-Insert98Time", scheme_Insert98Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Insert98Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Insert98Time %d)\n", (int)action_of_name(Denemo.commands, "Insert98Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set22Time settimesig22*/
SCM scheme_Set22Time(SCM optional);
install_scm_function ("d-Set22Time", scheme_Set22Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Set22Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set22Time %d)\n", (int)action_of_name(Denemo.commands, "Set22Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set32Time settimesig32*/
SCM scheme_Set32Time(SCM optional);
install_scm_function ("d-Set32Time", scheme_Set32Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Set32Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set32Time %d)\n", (int)action_of_name(Denemo.commands, "Set32Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set42Time settimesig42*/
SCM scheme_Set42Time(SCM optional);
install_scm_function ("d-Set42Time", scheme_Set42Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Set42Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set42Time %d)\n", (int)action_of_name(Denemo.commands, "Set42Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set44Time settimesig44*/
SCM scheme_Set44Time(SCM optional);
install_scm_function ("d-Set44Time", scheme_Set44Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Set44Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set44Time %d)\n", (int)action_of_name(Denemo.commands, "Set44Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set34Time settimesig34*/
SCM scheme_Set34Time(SCM optional);
install_scm_function ("d-Set34Time", scheme_Set34Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Set34Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set34Time %d)\n", (int)action_of_name(Denemo.commands, "Set34Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set24Time settimesig24*/
SCM scheme_Set24Time(SCM optional);
install_scm_function ("d-Set24Time", scheme_Set24Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Set24Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set24Time %d)\n", (int)action_of_name(Denemo.commands, "Set24Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set64Time settimesig64*/
SCM scheme_Set64Time(SCM optional);
install_scm_function ("d-Set64Time", scheme_Set64Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Set64Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set64Time %d)\n", (int)action_of_name(Denemo.commands, "Set64Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set38Time settimesig38*/
SCM scheme_Set38Time(SCM optional);
install_scm_function ("d-Set38Time", scheme_Set38Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Set38Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set38Time %d)\n", (int)action_of_name(Denemo.commands, "Set38Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set68Time settimesig68*/
SCM scheme_Set68Time(SCM optional);
install_scm_function ("d-Set68Time", scheme_Set68Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Set68Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set68Time %d)\n", (int)action_of_name(Denemo.commands, "Set68Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set128Time settimesig128*/
SCM scheme_Set128Time(SCM optional);
install_scm_function ("d-Set128Time", scheme_Set128Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Set128Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set128Time %d)\n", (int)action_of_name(Denemo.commands, "Set128Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*Set98Time settimesig98*/
SCM scheme_Set98Time(SCM optional);
install_scm_function ("d-Set98Time", scheme_Set98Time);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Set98Time")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Set98Time %d)\n", (int)action_of_name(Denemo.commands, "Set98Time"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertCmaj newkeysigcmaj*/
SCM scheme_InsertCmaj(SCM optional);
install_scm_function ("d-InsertCmaj", scheme_InsertCmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertCmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertCmaj %d)\n", (int)action_of_name(Denemo.commands, "InsertCmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertGmaj newkeysiggmaj*/
SCM scheme_InsertGmaj(SCM optional);
install_scm_function ("d-InsertGmaj", scheme_InsertGmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertGmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertGmaj %d)\n", (int)action_of_name(Denemo.commands, "InsertGmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDmaj newkeysigdmaj*/
SCM scheme_InsertDmaj(SCM optional);
install_scm_function ("d-InsertDmaj", scheme_InsertDmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertDmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertDmaj %d)\n", (int)action_of_name(Denemo.commands, "InsertDmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertAmaj newkeysigamaj*/
SCM scheme_InsertAmaj(SCM optional);
install_scm_function ("d-InsertAmaj", scheme_InsertAmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertAmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertAmaj %d)\n", (int)action_of_name(Denemo.commands, "InsertAmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertEmaj newkeysigemaj*/
SCM scheme_InsertEmaj(SCM optional);
install_scm_function ("d-InsertEmaj", scheme_InsertEmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertEmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertEmaj %d)\n", (int)action_of_name(Denemo.commands, "InsertEmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBmaj newkeysigbmaj*/
SCM scheme_InsertBmaj(SCM optional);
install_scm_function ("d-InsertBmaj", scheme_InsertBmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertBmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBmaj %d)\n", (int)action_of_name(Denemo.commands, "InsertBmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertFSharpmaj newkeysigfsharpmaj*/
SCM scheme_InsertFSharpmaj(SCM optional);
install_scm_function ("d-InsertFSharpmaj", scheme_InsertFSharpmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertFSharpmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertFSharpmaj %d)\n", (int)action_of_name(Denemo.commands, "InsertFSharpmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertCSharpmaj newkeysigcsharpmaj*/
SCM scheme_InsertCSharpmaj(SCM optional);
install_scm_function ("d-InsertCSharpmaj", scheme_InsertCSharpmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertCSharpmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertCSharpmaj %d)\n", (int)action_of_name(Denemo.commands, "InsertCSharpmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertFmaj newkeysigfmaj*/
SCM scheme_InsertFmaj(SCM optional);
install_scm_function ("d-InsertFmaj", scheme_InsertFmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertFmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertFmaj %d)\n", (int)action_of_name(Denemo.commands, "InsertFmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBflatmaj newkeysigbflatmaj*/
SCM scheme_InsertBflatmaj(SCM optional);
install_scm_function ("d-InsertBflatmaj", scheme_InsertBflatmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertBflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBflatmaj %d)\n", (int)action_of_name(Denemo.commands, "InsertBflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertEflatmaj newkeysigeflatmaj*/
SCM scheme_InsertEflatmaj(SCM optional);
install_scm_function ("d-InsertEflatmaj", scheme_InsertEflatmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertEflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertEflatmaj %d)\n", (int)action_of_name(Denemo.commands, "InsertEflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertAflatmaj newkeysigaflatmaj*/
SCM scheme_InsertAflatmaj(SCM optional);
install_scm_function ("d-InsertAflatmaj", scheme_InsertAflatmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertAflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertAflatmaj %d)\n", (int)action_of_name(Denemo.commands, "InsertAflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDflatmaj newkeysigdflatmaj*/
SCM scheme_InsertDflatmaj(SCM optional);
install_scm_function ("d-InsertDflatmaj", scheme_InsertDflatmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertDflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertDflatmaj %d)\n", (int)action_of_name(Denemo.commands, "InsertDflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertGflatmaj newkeysiggflatmaj*/
SCM scheme_InsertGflatmaj(SCM optional);
install_scm_function ("d-InsertGflatmaj", scheme_InsertGflatmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertGflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertGflatmaj %d)\n", (int)action_of_name(Denemo.commands, "InsertGflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertCflatmaj newkeysigcflatmaj*/
SCM scheme_InsertCflatmaj(SCM optional);
install_scm_function ("d-InsertCflatmaj", scheme_InsertCflatmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertCflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertCflatmaj %d)\n", (int)action_of_name(Denemo.commands, "InsertCflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertAmin newkeysigamin*/
SCM scheme_InsertAmin(SCM optional);
install_scm_function ("d-InsertAmin", scheme_InsertAmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertAmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertAmin %d)\n", (int)action_of_name(Denemo.commands, "InsertAmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertEmin newkeysigemin*/
SCM scheme_InsertEmin(SCM optional);
install_scm_function ("d-InsertEmin", scheme_InsertEmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertEmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertEmin %d)\n", (int)action_of_name(Denemo.commands, "InsertEmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBmin newkeysigbmin*/
SCM scheme_InsertBmin(SCM optional);
install_scm_function ("d-InsertBmin", scheme_InsertBmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertBmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBmin %d)\n", (int)action_of_name(Denemo.commands, "InsertBmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertFSharpmin newkeysigfsharpmin*/
SCM scheme_InsertFSharpmin(SCM optional);
install_scm_function ("d-InsertFSharpmin", scheme_InsertFSharpmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertFSharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertFSharpmin %d)\n", (int)action_of_name(Denemo.commands, "InsertFSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertCSharpmin newkeysigcsharpmin*/
SCM scheme_InsertCSharpmin(SCM optional);
install_scm_function ("d-InsertCSharpmin", scheme_InsertCSharpmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertCSharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertCSharpmin %d)\n", (int)action_of_name(Denemo.commands, "InsertCSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertGSharpmin newkeysiggsharpmin*/
SCM scheme_InsertGSharpmin(SCM optional);
install_scm_function ("d-InsertGSharpmin", scheme_InsertGSharpmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertGSharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertGSharpmin %d)\n", (int)action_of_name(Denemo.commands, "InsertGSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDSharpmin newkeysigdsharpmin*/
SCM scheme_InsertDSharpmin(SCM optional);
install_scm_function ("d-InsertDSharpmin", scheme_InsertDSharpmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertDSharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertDSharpmin %d)\n", (int)action_of_name(Denemo.commands, "InsertDSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertASharpmin newkeysigasharpmin*/
SCM scheme_InsertASharpmin(SCM optional);
install_scm_function ("d-InsertASharpmin", scheme_InsertASharpmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertASharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertASharpmin %d)\n", (int)action_of_name(Denemo.commands, "InsertASharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDmin newkeysigdmin*/
SCM scheme_InsertDmin(SCM optional);
install_scm_function ("d-InsertDmin", scheme_InsertDmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertDmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertDmin %d)\n", (int)action_of_name(Denemo.commands, "InsertDmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertGmin newkeysiggmin*/
SCM scheme_InsertGmin(SCM optional);
install_scm_function ("d-InsertGmin", scheme_InsertGmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertGmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertGmin %d)\n", (int)action_of_name(Denemo.commands, "InsertGmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertCmin newkeysigcmin*/
SCM scheme_InsertCmin(SCM optional);
install_scm_function ("d-InsertCmin", scheme_InsertCmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertCmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertCmin %d)\n", (int)action_of_name(Denemo.commands, "InsertCmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertFmin newkeysigfmin*/
SCM scheme_InsertFmin(SCM optional);
install_scm_function ("d-InsertFmin", scheme_InsertFmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertFmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertFmin %d)\n", (int)action_of_name(Denemo.commands, "InsertFmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBflatmin newkeysigbflatmin*/
SCM scheme_InsertBflatmin(SCM optional);
install_scm_function ("d-InsertBflatmin", scheme_InsertBflatmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertBflatmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBflatmin %d)\n", (int)action_of_name(Denemo.commands, "InsertBflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertEflatmin newkeysigeflatmin*/
SCM scheme_InsertEflatmin(SCM optional);
install_scm_function ("d-InsertEflatmin", scheme_InsertEflatmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertEflatmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertEflatmin %d)\n", (int)action_of_name(Denemo.commands, "InsertEflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertAflatmin newkeysigaflatmin*/
SCM scheme_InsertAflatmin(SCM optional);
install_scm_function ("d-InsertAflatmin", scheme_InsertAflatmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertAflatmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertAflatmin %d)\n", (int)action_of_name(Denemo.commands, "InsertAflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialCmaj setkeysigcmaj*/
SCM scheme_SetInitialCmaj(SCM optional);
install_scm_function ("d-SetInitialCmaj", scheme_SetInitialCmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialCmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialCmaj %d)\n", (int)action_of_name(Denemo.commands, "SetInitialCmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialGmaj setkeysiggmaj*/
SCM scheme_SetInitialGmaj(SCM optional);
install_scm_function ("d-SetInitialGmaj", scheme_SetInitialGmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialGmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialGmaj %d)\n", (int)action_of_name(Denemo.commands, "SetInitialGmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialDmaj setkeysigdmaj*/
SCM scheme_SetInitialDmaj(SCM optional);
install_scm_function ("d-SetInitialDmaj", scheme_SetInitialDmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialDmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialDmaj %d)\n", (int)action_of_name(Denemo.commands, "SetInitialDmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialAmaj setkeysigamaj*/
SCM scheme_SetInitialAmaj(SCM optional);
install_scm_function ("d-SetInitialAmaj", scheme_SetInitialAmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialAmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialAmaj %d)\n", (int)action_of_name(Denemo.commands, "SetInitialAmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialEmaj setkeysigemaj*/
SCM scheme_SetInitialEmaj(SCM optional);
install_scm_function ("d-SetInitialEmaj", scheme_SetInitialEmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialEmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialEmaj %d)\n", (int)action_of_name(Denemo.commands, "SetInitialEmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialBmaj setkeysigbmaj*/
SCM scheme_SetInitialBmaj(SCM optional);
install_scm_function ("d-SetInitialBmaj", scheme_SetInitialBmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialBmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialBmaj %d)\n", (int)action_of_name(Denemo.commands, "SetInitialBmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialFSharpmaj setkeysigfsharpmaj*/
SCM scheme_SetInitialFSharpmaj(SCM optional);
install_scm_function ("d-SetInitialFSharpmaj", scheme_SetInitialFSharpmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialFSharpmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialFSharpmaj %d)\n", (int)action_of_name(Denemo.commands, "SetInitialFSharpmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialCSharpmaj setkeysigcsharpmaj*/
SCM scheme_SetInitialCSharpmaj(SCM optional);
install_scm_function ("d-SetInitialCSharpmaj", scheme_SetInitialCSharpmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialCSharpmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialCSharpmaj %d)\n", (int)action_of_name(Denemo.commands, "SetInitialCSharpmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialFmaj setkeysigfmaj*/
SCM scheme_SetInitialFmaj(SCM optional);
install_scm_function ("d-SetInitialFmaj", scheme_SetInitialFmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialFmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialFmaj %d)\n", (int)action_of_name(Denemo.commands, "SetInitialFmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialBflatmaj setkeysigbflatmaj*/
SCM scheme_SetInitialBflatmaj(SCM optional);
install_scm_function ("d-SetInitialBflatmaj", scheme_SetInitialBflatmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialBflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialBflatmaj %d)\n", (int)action_of_name(Denemo.commands, "SetInitialBflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialEflatmaj setkeysigeflatmaj*/
SCM scheme_SetInitialEflatmaj(SCM optional);
install_scm_function ("d-SetInitialEflatmaj", scheme_SetInitialEflatmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialEflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialEflatmaj %d)\n", (int)action_of_name(Denemo.commands, "SetInitialEflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialAflatmaj setkeysigaflatmaj*/
SCM scheme_SetInitialAflatmaj(SCM optional);
install_scm_function ("d-SetInitialAflatmaj", scheme_SetInitialAflatmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialAflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialAflatmaj %d)\n", (int)action_of_name(Denemo.commands, "SetInitialAflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialDflatmaj setkeysigdflatmaj*/
SCM scheme_SetInitialDflatmaj(SCM optional);
install_scm_function ("d-SetInitialDflatmaj", scheme_SetInitialDflatmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialDflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialDflatmaj %d)\n", (int)action_of_name(Denemo.commands, "SetInitialDflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialGflatmaj setkeysiggflatmaj*/
SCM scheme_SetInitialGflatmaj(SCM optional);
install_scm_function ("d-SetInitialGflatmaj", scheme_SetInitialGflatmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialGflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialGflatmaj %d)\n", (int)action_of_name(Denemo.commands, "SetInitialGflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialCflatmaj setkeysigcflatmaj*/
SCM scheme_SetInitialCflatmaj(SCM optional);
install_scm_function ("d-SetInitialCflatmaj", scheme_SetInitialCflatmaj);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialCflatmaj")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialCflatmaj %d)\n", (int)action_of_name(Denemo.commands, "SetInitialCflatmaj"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialAmin setkeysigamin*/
SCM scheme_SetInitialAmin(SCM optional);
install_scm_function ("d-SetInitialAmin", scheme_SetInitialAmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialAmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialAmin %d)\n", (int)action_of_name(Denemo.commands, "SetInitialAmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialEmin setkeysigemin*/
SCM scheme_SetInitialEmin(SCM optional);
install_scm_function ("d-SetInitialEmin", scheme_SetInitialEmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialEmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialEmin %d)\n", (int)action_of_name(Denemo.commands, "SetInitialEmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialBmin setkeysigbmin*/
SCM scheme_SetInitialBmin(SCM optional);
install_scm_function ("d-SetInitialBmin", scheme_SetInitialBmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialBmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialBmin %d)\n", (int)action_of_name(Denemo.commands, "SetInitialBmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialFSharpmin setkeysigfsharpmin*/
SCM scheme_SetInitialFSharpmin(SCM optional);
install_scm_function ("d-SetInitialFSharpmin", scheme_SetInitialFSharpmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialFSharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialFSharpmin %d)\n", (int)action_of_name(Denemo.commands, "SetInitialFSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialCSharpmin setkeysigcsharpmin*/
SCM scheme_SetInitialCSharpmin(SCM optional);
install_scm_function ("d-SetInitialCSharpmin", scheme_SetInitialCSharpmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialCSharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialCSharpmin %d)\n", (int)action_of_name(Denemo.commands, "SetInitialCSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialGSharpmin setkeysiggsharpmin*/
SCM scheme_SetInitialGSharpmin(SCM optional);
install_scm_function ("d-SetInitialGSharpmin", scheme_SetInitialGSharpmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialGSharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialGSharpmin %d)\n", (int)action_of_name(Denemo.commands, "SetInitialGSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialDSharpmin setkeysigdsharpmin*/
SCM scheme_SetInitialDSharpmin(SCM optional);
install_scm_function ("d-SetInitialDSharpmin", scheme_SetInitialDSharpmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialDSharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialDSharpmin %d)\n", (int)action_of_name(Denemo.commands, "SetInitialDSharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialASharpmin setkeysigasharpmin*/
SCM scheme_SetInitialASharpmin(SCM optional);
install_scm_function ("d-SetInitialASharpmin", scheme_SetInitialASharpmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialASharpmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialASharpmin %d)\n", (int)action_of_name(Denemo.commands, "SetInitialASharpmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialDmin setkeysigdmin*/
SCM scheme_SetInitialDmin(SCM optional);
install_scm_function ("d-SetInitialDmin", scheme_SetInitialDmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialDmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialDmin %d)\n", (int)action_of_name(Denemo.commands, "SetInitialDmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialGmin setkeysiggmin*/
SCM scheme_SetInitialGmin(SCM optional);
install_scm_function ("d-SetInitialGmin", scheme_SetInitialGmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialGmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialGmin %d)\n", (int)action_of_name(Denemo.commands, "SetInitialGmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialCmin setkeysigcmin*/
SCM scheme_SetInitialCmin(SCM optional);
install_scm_function ("d-SetInitialCmin", scheme_SetInitialCmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialCmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialCmin %d)\n", (int)action_of_name(Denemo.commands, "SetInitialCmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialFmin setkeysigfmin*/
SCM scheme_SetInitialFmin(SCM optional);
install_scm_function ("d-SetInitialFmin", scheme_SetInitialFmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialFmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialFmin %d)\n", (int)action_of_name(Denemo.commands, "SetInitialFmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialBflatmin setkeysigbflatmin*/
SCM scheme_SetInitialBflatmin(SCM optional);
install_scm_function ("d-SetInitialBflatmin", scheme_SetInitialBflatmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialBflatmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialBflatmin %d)\n", (int)action_of_name(Denemo.commands, "SetInitialBflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialEflatmin setkeysigeflatmin*/
SCM scheme_SetInitialEflatmin(SCM optional);
install_scm_function ("d-SetInitialEflatmin", scheme_SetInitialEflatmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialEflatmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialEflatmin %d)\n", (int)action_of_name(Denemo.commands, "SetInitialEflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetInitialAflatmin setkeysigaflatmin*/
SCM scheme_SetInitialAflatmin(SCM optional);
install_scm_function ("d-SetInitialAflatmin", scheme_SetInitialAflatmin);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetInitialAflatmin")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetInitialAflatmin %d)\n", (int)action_of_name(Denemo.commands, "SetInitialAflatmin"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetMark set_mark*/
SCM scheme_SetMark(SCM optional);
install_scm_function ("d-SetMark", scheme_SetMark);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetMark")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetMark %d)\n", (int)action_of_name(Denemo.commands, "SetMark"));
(void)scm_c_eval_string(text);
g_free(text);
/*UnsetMark unset_mark*/
SCM scheme_UnsetMark(SCM optional);
install_scm_function ("d-UnsetMark", scheme_UnsetMark);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "UnsetMark")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_UnsetMark %d)\n", (int)action_of_name(Denemo.commands, "UnsetMark"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleBeginSlur toggle_begin_slur*/
SCM scheme_ToggleBeginSlur(SCM optional);
install_scm_function ("d-ToggleBeginSlur", scheme_ToggleBeginSlur);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleBeginSlur")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleBeginSlur %d)\n", (int)action_of_name(Denemo.commands, "ToggleBeginSlur"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleEndSlur toggle_end_slur*/
SCM scheme_ToggleEndSlur(SCM optional);
install_scm_function ("d-ToggleEndSlur", scheme_ToggleEndSlur);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleEndSlur")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleEndSlur %d)\n", (int)action_of_name(Denemo.commands, "ToggleEndSlur"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleStartCrescendo toggle_start_crescendo*/
SCM scheme_ToggleStartCrescendo(SCM optional);
install_scm_function ("d-ToggleStartCrescendo", scheme_ToggleStartCrescendo);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleStartCrescendo")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleStartCrescendo %d)\n", (int)action_of_name(Denemo.commands, "ToggleStartCrescendo"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleEndCrescendo toggle_end_crescendo*/
SCM scheme_ToggleEndCrescendo(SCM optional);
install_scm_function ("d-ToggleEndCrescendo", scheme_ToggleEndCrescendo);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleEndCrescendo")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleEndCrescendo %d)\n", (int)action_of_name(Denemo.commands, "ToggleEndCrescendo"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleStartDiminuendo toggle_start_diminuendo*/
SCM scheme_ToggleStartDiminuendo(SCM optional);
install_scm_function ("d-ToggleStartDiminuendo", scheme_ToggleStartDiminuendo);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleStartDiminuendo")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleStartDiminuendo %d)\n", (int)action_of_name(Denemo.commands, "ToggleStartDiminuendo"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleEndDiminuendo toggle_end_diminuendo*/
SCM scheme_ToggleEndDiminuendo(SCM optional);
install_scm_function ("d-ToggleEndDiminuendo", scheme_ToggleEndDiminuendo);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleEndDiminuendo")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleEndDiminuendo %d)\n", (int)action_of_name(Denemo.commands, "ToggleEndDiminuendo"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleAccent add_accent*/
SCM scheme_ToggleAccent(SCM optional);
install_scm_function ("d-ToggleAccent", scheme_ToggleAccent);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleAccent")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleAccent %d)\n", (int)action_of_name(Denemo.commands, "ToggleAccent"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleFermata add_fermata*/
SCM scheme_ToggleFermata(SCM optional);
install_scm_function ("d-ToggleFermata", scheme_ToggleFermata);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleFermata")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleFermata %d)\n", (int)action_of_name(Denemo.commands, "ToggleFermata"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleStaccato add_staccato*/
SCM scheme_ToggleStaccato(SCM optional);
install_scm_function ("d-ToggleStaccato", scheme_ToggleStaccato);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleStaccato")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleStaccato %d)\n", (int)action_of_name(Denemo.commands, "ToggleStaccato"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleTenuto add_tenuto*/
SCM scheme_ToggleTenuto(SCM optional);
install_scm_function ("d-ToggleTenuto", scheme_ToggleTenuto);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleTenuto")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleTenuto %d)\n", (int)action_of_name(Denemo.commands, "ToggleTenuto"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleTrill add_trill*/
SCM scheme_ToggleTrill(SCM optional);
install_scm_function ("d-ToggleTrill", scheme_ToggleTrill);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleTrill")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleTrill %d)\n", (int)action_of_name(Denemo.commands, "ToggleTrill"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleTurn add_turn*/
SCM scheme_ToggleTurn(SCM optional);
install_scm_function ("d-ToggleTurn", scheme_ToggleTurn);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleTurn")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleTurn %d)\n", (int)action_of_name(Denemo.commands, "ToggleTurn"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleMordent add_mordent*/
SCM scheme_ToggleMordent(SCM optional);
install_scm_function ("d-ToggleMordent", scheme_ToggleMordent);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleMordent")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleMordent %d)\n", (int)action_of_name(Denemo.commands, "ToggleMordent"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleStaccatissimo add_staccatissimo*/
SCM scheme_ToggleStaccatissimo(SCM optional);
install_scm_function ("d-ToggleStaccatissimo", scheme_ToggleStaccatissimo);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleStaccatissimo")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleStaccatissimo %d)\n", (int)action_of_name(Denemo.commands, "ToggleStaccatissimo"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleCoda add_coda*/
SCM scheme_ToggleCoda(SCM optional);
install_scm_function ("d-ToggleCoda", scheme_ToggleCoda);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleCoda")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleCoda %d)\n", (int)action_of_name(Denemo.commands, "ToggleCoda"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleFlageolet add_flageolet*/
SCM scheme_ToggleFlageolet(SCM optional);
install_scm_function ("d-ToggleFlageolet", scheme_ToggleFlageolet);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleFlageolet")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleFlageolet %d)\n", (int)action_of_name(Denemo.commands, "ToggleFlageolet"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleOpen add_open*/
SCM scheme_ToggleOpen(SCM optional);
install_scm_function ("d-ToggleOpen", scheme_ToggleOpen);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleOpen")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleOpen %d)\n", (int)action_of_name(Denemo.commands, "ToggleOpen"));
(void)scm_c_eval_string(text);
g_free(text);
/*TogglePrallMordent add_prallmordent*/
SCM scheme_TogglePrallMordent(SCM optional);
install_scm_function ("d-TogglePrallMordent", scheme_TogglePrallMordent);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "TogglePrallMordent")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_TogglePrallMordent %d)\n", (int)action_of_name(Denemo.commands, "TogglePrallMordent"));
(void)scm_c_eval_string(text);
g_free(text);
/*TogglePrallPrall add_prallprall*/
SCM scheme_TogglePrallPrall(SCM optional);
install_scm_function ("d-TogglePrallPrall", scheme_TogglePrallPrall);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "TogglePrallPrall")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_TogglePrallPrall %d)\n", (int)action_of_name(Denemo.commands, "TogglePrallPrall"));
(void)scm_c_eval_string(text);
g_free(text);
/*TogglePrall add_prall*/
SCM scheme_TogglePrall(SCM optional);
install_scm_function ("d-TogglePrall", scheme_TogglePrall);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "TogglePrall")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_TogglePrall %d)\n", (int)action_of_name(Denemo.commands, "TogglePrall"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleReverseTurn add_reverseturn*/
SCM scheme_ToggleReverseTurn(SCM optional);
install_scm_function ("d-ToggleReverseTurn", scheme_ToggleReverseTurn);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleReverseTurn")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleReverseTurn %d)\n", (int)action_of_name(Denemo.commands, "ToggleReverseTurn"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleSegno add_segno*/
SCM scheme_ToggleSegno(SCM optional);
install_scm_function ("d-ToggleSegno", scheme_ToggleSegno);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleSegno")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleSegno %d)\n", (int)action_of_name(Denemo.commands, "ToggleSegno"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleSforzato add_sforzato*/
SCM scheme_ToggleSforzato(SCM optional);
install_scm_function ("d-ToggleSforzato", scheme_ToggleSforzato);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleSforzato")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleSforzato %d)\n", (int)action_of_name(Denemo.commands, "ToggleSforzato"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleStopped add_stopped*/
SCM scheme_ToggleStopped(SCM optional);
install_scm_function ("d-ToggleStopped", scheme_ToggleStopped);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleStopped")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleStopped %d)\n", (int)action_of_name(Denemo.commands, "ToggleStopped"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleThumb add_thumb*/
SCM scheme_ToggleThumb(SCM optional);
install_scm_function ("d-ToggleThumb", scheme_ToggleThumb);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleThumb")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleThumb %d)\n", (int)action_of_name(Denemo.commands, "ToggleThumb"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleUpprall add_upprall*/
SCM scheme_ToggleUpprall(SCM optional);
install_scm_function ("d-ToggleUpprall", scheme_ToggleUpprall);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleUpprall")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleUpprall %d)\n", (int)action_of_name(Denemo.commands, "ToggleUpprall"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleArpeggio add_arpeggio*/
SCM scheme_ToggleArpeggio(SCM optional);
install_scm_function ("d-ToggleArpeggio", scheme_ToggleArpeggio);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleArpeggio")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleArpeggio %d)\n", (int)action_of_name(Denemo.commands, "ToggleArpeggio"));
(void)scm_c_eval_string(text);
g_free(text);
/*SetGrace set_grace*/
SCM scheme_SetGrace(SCM optional);
install_scm_function ("d-SetGrace", scheme_SetGrace);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SetGrace")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SetGrace %d)\n", (int)action_of_name(Denemo.commands, "SetGrace"));
(void)scm_c_eval_string(text);
g_free(text);
/*ForceCaution force_cautionary*/
SCM scheme_ForceCaution(SCM optional);
install_scm_function ("d-ForceCaution", scheme_ForceCaution);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ForceCaution")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ForceCaution %d)\n", (int)action_of_name(Denemo.commands, "ForceCaution"));
(void)scm_c_eval_string(text);
g_free(text);
/*ChangePitch change_pitch*/
SCM scheme_ChangePitch(SCM optional);
install_scm_function ("d-ChangePitch", scheme_ChangePitch);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ChangePitch")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ChangePitch %d)\n", (int)action_of_name(Denemo.commands, "ChangePitch"));
(void)scm_c_eval_string(text);
g_free(text);
/*DoubleBar insert_doublebar*/
SCM scheme_DoubleBar(SCM optional);
install_scm_function ("d-DoubleBar", scheme_DoubleBar);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "DoubleBar")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DoubleBar %d)\n", (int)action_of_name(Denemo.commands, "DoubleBar"));
(void)scm_c_eval_string(text);
g_free(text);
/*EndBar insert_endbar*/
SCM scheme_EndBar(SCM optional);
install_scm_function ("d-EndBar", scheme_EndBar);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "EndBar")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_EndBar %d)\n", (int)action_of_name(Denemo.commands, "EndBar"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenRepeat insert_openrepeat*/
SCM scheme_OpenRepeat(SCM optional);
install_scm_function ("d-OpenRepeat", scheme_OpenRepeat);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "OpenRepeat")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_OpenRepeat %d)\n", (int)action_of_name(Denemo.commands, "OpenRepeat"));
(void)scm_c_eval_string(text);
g_free(text);
/*CloseRepeat insert_closerepeat*/
SCM scheme_CloseRepeat(SCM optional);
install_scm_function ("d-CloseRepeat", scheme_CloseRepeat);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "CloseRepeat")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_CloseRepeat %d)\n", (int)action_of_name(Denemo.commands, "CloseRepeat"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenCloseRepeat insert_opencloserepeat*/
SCM scheme_OpenCloseRepeat(SCM optional);
install_scm_function ("d-OpenCloseRepeat", scheme_OpenCloseRepeat);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "OpenCloseRepeat")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_OpenCloseRepeat %d)\n", (int)action_of_name(Denemo.commands, "OpenCloseRepeat"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertRhythm insert_rhythm_pattern*/
SCM scheme_InsertRhythm(SCM optional);
install_scm_function ("d-InsertRhythm", scheme_InsertRhythm);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertRhythm")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertRhythm %d)\n", (int)action_of_name(Denemo.commands, "InsertRhythm"));
(void)scm_c_eval_string(text);
g_free(text);
/*NextRhythm nextrhythm*/
SCM scheme_NextRhythm(SCM optional);
install_scm_function ("d-NextRhythm", scheme_NextRhythm);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "NextRhythm")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_NextRhythm %d)\n", (int)action_of_name(Denemo.commands, "NextRhythm"));
(void)scm_c_eval_string(text);
g_free(text);
/*AppendMesauresToScore append_measure_score*/
SCM scheme_AppendMesauresToScore(SCM optional);
install_scm_function ("d-AppendMesauresToScore", scheme_AppendMesauresToScore);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "AppendMesauresToScore")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AppendMesauresToScore %d)\n", (int)action_of_name(Denemo.commands, "AppendMesauresToScore"));
(void)scm_c_eval_string(text);
g_free(text);
/*New file_newwrapper*/
SCM scheme_New(SCM optional);
install_scm_function ("d-New", scheme_New);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "New")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_New %d)\n", (int)action_of_name(Denemo.commands, "New"));
(void)scm_c_eval_string(text);
g_free(text);
/*Open file_open_with_check*/
SCM scheme_Open(SCM optional);
install_scm_function ("d-Open", scheme_Open);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Open")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Open %d)\n", (int)action_of_name(Denemo.commands, "Open"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddStaffs file_add_staffs*/
SCM scheme_AddStaffs(SCM optional);
install_scm_function ("d-AddStaffs", scheme_AddStaffs);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "AddStaffs")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddStaffs %d)\n", (int)action_of_name(Denemo.commands, "AddStaffs"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddMovements file_add_movements*/
SCM scheme_AddMovements(SCM optional);
install_scm_function ("d-AddMovements", scheme_AddMovements);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "AddMovements")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddMovements %d)\n", (int)action_of_name(Denemo.commands, "AddMovements"));
(void)scm_c_eval_string(text);
g_free(text);
/*MovementProps movement_props_dialog*/
SCM scheme_MovementProps(SCM optional);
install_scm_function ("d-MovementProps", scheme_MovementProps);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "MovementProps")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_MovementProps %d)\n", (int)action_of_name(Denemo.commands, "MovementProps"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenNewWindow openinnew*/
SCM scheme_OpenNewWindow(SCM optional);
install_scm_function ("d-OpenNewWindow", scheme_OpenNewWindow);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "OpenNewWindow")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_OpenNewWindow %d)\n", (int)action_of_name(Denemo.commands, "OpenNewWindow"));
(void)scm_c_eval_string(text);
g_free(text);
/*Save file_savewrapper*/
SCM scheme_Save(SCM optional);
install_scm_function ("d-Save", scheme_Save);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Save")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Save %d)\n", (int)action_of_name(Denemo.commands, "Save"));
(void)scm_c_eval_string(text);
g_free(text);
/*SaveAs file_saveaswrapper*/
SCM scheme_SaveAs(SCM optional);
install_scm_function ("d-SaveAs", scheme_SaveAs);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SaveAs")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SaveAs %d)\n", (int)action_of_name(Denemo.commands, "SaveAs"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenTemplate system_template_open_with_check*/
SCM scheme_OpenTemplate(SCM optional);
install_scm_function ("d-OpenTemplate", scheme_OpenTemplate);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "OpenTemplate")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_OpenTemplate %d)\n", (int)action_of_name(Denemo.commands, "OpenTemplate"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenExample system_example_open_with_check*/
SCM scheme_OpenExample(SCM optional);
install_scm_function ("d-OpenExample", scheme_OpenExample);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "OpenExample")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_OpenExample %d)\n", (int)action_of_name(Denemo.commands, "OpenExample"));
(void)scm_c_eval_string(text);
g_free(text);
/*OpenMyTemplate local_template_open_with_check*/
SCM scheme_OpenMyTemplate(SCM optional);
install_scm_function ("d-OpenMyTemplate", scheme_OpenMyTemplate);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "OpenMyTemplate")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_OpenMyTemplate %d)\n", (int)action_of_name(Denemo.commands, "OpenMyTemplate"));
(void)scm_c_eval_string(text);
g_free(text);
/*SaveTemplate template_save*/
SCM scheme_SaveTemplate(SCM optional);
install_scm_function ("d-SaveTemplate", scheme_SaveTemplate);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SaveTemplate")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SaveTemplate %d)\n", (int)action_of_name(Denemo.commands, "SaveTemplate"));
(void)scm_c_eval_string(text);
g_free(text);
/*NewWindow newview*/
SCM scheme_NewWindow(SCM optional);
install_scm_function ("d-NewWindow", scheme_NewWindow);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "NewWindow")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_NewWindow %d)\n", (int)action_of_name(Denemo.commands, "NewWindow"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertMovementBefore insert_movement_before*/
SCM scheme_InsertMovementBefore(SCM optional);
install_scm_function ("d-InsertMovementBefore", scheme_InsertMovementBefore);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertMovementBefore")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertMovementBefore %d)\n", (int)action_of_name(Denemo.commands, "InsertMovementBefore"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertMovementAfter insert_movement_after*/
SCM scheme_InsertMovementAfter(SCM optional);
install_scm_function ("d-InsertMovementAfter", scheme_InsertMovementAfter);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertMovementAfter")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertMovementAfter %d)\n", (int)action_of_name(Denemo.commands, "InsertMovementAfter"));
(void)scm_c_eval_string(text);
g_free(text);
/*SaveParts file_savepartswrapper*/
SCM scheme_SaveParts(SCM optional);
install_scm_function ("d-SaveParts", scheme_SaveParts);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SaveParts")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SaveParts %d)\n", (int)action_of_name(Denemo.commands, "SaveParts"));
(void)scm_c_eval_string(text);
g_free(text);
/*ExportPDF export_pdf_action*/
SCM scheme_ExportPDF(SCM optional);
install_scm_function ("d-ExportPDF", scheme_ExportPDF);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ExportPDF")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ExportPDF %d)\n", (int)action_of_name(Denemo.commands, "ExportPDF"));
(void)scm_c_eval_string(text);
g_free(text);
/*ConfigureScore scorewizard*/
SCM scheme_ConfigureScore(SCM optional);
install_scm_function ("d-ConfigureScore", scheme_ConfigureScore);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ConfigureScore")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ConfigureScore %d)\n", (int)action_of_name(Denemo.commands, "ConfigureScore"));
(void)scm_c_eval_string(text);
g_free(text);
/*PrintPreview printpreview_cb*/
SCM scheme_PrintPreview(SCM optional);
install_scm_function ("d-PrintPreview", scheme_PrintPreview);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "PrintPreview")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_PrintPreview %d)\n", (int)action_of_name(Denemo.commands, "PrintPreview"));
(void)scm_c_eval_string(text);
g_free(text);
/*PrintExcerptPreview printexcerptpreview_cb*/
SCM scheme_PrintExcerptPreview(SCM optional);
install_scm_function ("d-PrintExcerptPreview", scheme_PrintExcerptPreview);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "PrintExcerptPreview")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_PrintExcerptPreview %d)\n", (int)action_of_name(Denemo.commands, "PrintExcerptPreview"));
(void)scm_c_eval_string(text);
g_free(text);
/*Print printall_cb*/
SCM scheme_Print(SCM optional);
install_scm_function ("d-Print", scheme_Print);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Print")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Print %d)\n", (int)action_of_name(Denemo.commands, "Print"));
(void)scm_c_eval_string(text);
g_free(text);
/*PrintPart printpart_cb*/
SCM scheme_PrintPart(SCM optional);
install_scm_function ("d-PrintPart", scheme_PrintPart);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "PrintPart")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_PrintPart %d)\n", (int)action_of_name(Denemo.commands, "PrintPart"));
(void)scm_c_eval_string(text);
g_free(text);
/*Close close_gui_with_check*/
SCM scheme_Close(SCM optional);
install_scm_function ("d-Close", scheme_Close);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Close")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Close %d)\n", (int)action_of_name(Denemo.commands, "Close"));
(void)scm_c_eval_string(text);
g_free(text);
/*Quit closewrapper*/
SCM scheme_Quit(SCM optional);
install_scm_function ("d-Quit", scheme_Quit);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Quit")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Quit %d)\n", (int)action_of_name(Denemo.commands, "Quit"));
(void)scm_c_eval_string(text);
g_free(text);
/*Undo undowrapper*/
SCM scheme_Undo(SCM optional);
install_scm_function ("d-Undo", scheme_Undo);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Undo")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Undo %d)\n", (int)action_of_name(Denemo.commands, "Undo"));
(void)scm_c_eval_string(text);
g_free(text);
/*Redo redowrapper*/
SCM scheme_Redo(SCM optional);
install_scm_function ("d-Redo", scheme_Redo);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Redo")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Redo %d)\n", (int)action_of_name(Denemo.commands, "Redo"));
(void)scm_c_eval_string(text);
g_free(text);
/*Copy copywrapper*/
SCM scheme_Copy(SCM optional);
install_scm_function ("d-Copy", scheme_Copy);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Copy")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Copy %d)\n", (int)action_of_name(Denemo.commands, "Copy"));
(void)scm_c_eval_string(text);
g_free(text);
/*Cut cutwrapper*/
SCM scheme_Cut(SCM optional);
install_scm_function ("d-Cut", scheme_Cut);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Cut")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Cut %d)\n", (int)action_of_name(Denemo.commands, "Cut"));
(void)scm_c_eval_string(text);
g_free(text);
/*Paste pastewrapper*/
SCM scheme_Paste(SCM optional);
install_scm_function ("d-Paste", scheme_Paste);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Paste")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Paste %d)\n", (int)action_of_name(Denemo.commands, "Paste"));
(void)scm_c_eval_string(text);
g_free(text);
/*ScoreProperties score_properties_dialog*/
SCM scheme_ScoreProperties(SCM optional);
install_scm_function ("d-ScoreProperties", scheme_ScoreProperties);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ScoreProperties")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ScoreProperties %d)\n", (int)action_of_name(Denemo.commands, "ScoreProperties"));
(void)scm_c_eval_string(text);
g_free(text);
/*SaveSelection saveselwrapper*/
SCM scheme_SaveSelection(SCM optional);
install_scm_function ("d-SaveSelection", scheme_SaveSelection);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SaveSelection")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SaveSelection %d)\n", (int)action_of_name(Denemo.commands, "SaveSelection"));
(void)scm_c_eval_string(text);
g_free(text);
/*Preferences preferences_change*/
SCM scheme_Preferences(SCM optional);
install_scm_function ("d-Preferences", scheme_Preferences);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Preferences")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Preferences %d)\n", (int)action_of_name(Denemo.commands, "Preferences"));
(void)scm_c_eval_string(text);
g_free(text);
/*SaveAccels save_default_keymap_file_wrapper*/
SCM scheme_SaveAccels(SCM optional);
install_scm_function ("d-SaveAccels", scheme_SaveAccels);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SaveAccels")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SaveAccels %d)\n", (int)action_of_name(Denemo.commands, "SaveAccels"));
(void)scm_c_eval_string(text);
g_free(text);
/*CommandManagement configure_keyboard_dialog*/
SCM scheme_CommandManagement(SCM optional);
install_scm_function ("d-CommandManagement", scheme_CommandManagement);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "CommandManagement")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_CommandManagement %d)\n", (int)action_of_name(Denemo.commands, "CommandManagement"));
(void)scm_c_eval_string(text);
g_free(text);
/*LoadPlugins load_plugin*/
SCM scheme_LoadPlugins(SCM optional);
install_scm_function ("d-LoadPlugins", scheme_LoadPlugins);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "LoadPlugins")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_LoadPlugins %d)\n", (int)action_of_name(Denemo.commands, "LoadPlugins"));
(void)scm_c_eval_string(text);
g_free(text);
/*UnloadPlugins unloadplugins*/
SCM scheme_UnloadPlugins(SCM optional);
install_scm_function ("d-UnloadPlugins", scheme_UnloadPlugins);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "UnloadPlugins")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_UnloadPlugins %d)\n", (int)action_of_name(Denemo.commands, "UnloadPlugins"));
(void)scm_c_eval_string(text);
g_free(text);
/*ListPlugins list_loaded_plugins*/
SCM scheme_ListPlugins(SCM optional);
install_scm_function ("d-ListPlugins", scheme_ListPlugins);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ListPlugins")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ListPlugins %d)\n", (int)action_of_name(Denemo.commands, "ListPlugins"));
(void)scm_c_eval_string(text);
g_free(text);
/*ListAvailablePlugins list_available_plugins*/
SCM scheme_ListAvailablePlugins(SCM optional);
install_scm_function ("d-ListAvailablePlugins", scheme_ListAvailablePlugins);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ListAvailablePlugins")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ListAvailablePlugins %d)\n", (int)action_of_name(Denemo.commands, "ListAvailablePlugins"));
(void)scm_c_eval_string(text);
g_free(text);
/*SwapStaffs swapstaffs*/
SCM scheme_SwapStaffs(SCM optional);
install_scm_function ("d-SwapStaffs", scheme_SwapStaffs);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SwapStaffs")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SwapStaffs %d)\n", (int)action_of_name(Denemo.commands, "SwapStaffs"));
(void)scm_c_eval_string(text);
g_free(text);
/*SplitVoices splitstaffs*/
SCM scheme_SplitVoices(SCM optional);
install_scm_function ("d-SplitVoices", scheme_SplitVoices);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SplitVoices")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SplitVoices %d)\n", (int)action_of_name(Denemo.commands, "SplitVoices"));
(void)scm_c_eval_string(text);
g_free(text);
/*JoinVoices joinstaffs*/
SCM scheme_JoinVoices(SCM optional);
install_scm_function ("d-JoinVoices", scheme_JoinVoices);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "JoinVoices")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_JoinVoices %d)\n", (int)action_of_name(Denemo.commands, "JoinVoices"));
(void)scm_c_eval_string(text);
g_free(text);
/*SwapMovements swapmovements*/
SCM scheme_SwapMovements(SCM optional);
install_scm_function ("d-SwapMovements", scheme_SwapMovements);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "SwapMovements")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_SwapMovements %d)\n", (int)action_of_name(Denemo.commands, "SwapMovements"));
(void)scm_c_eval_string(text);
g_free(text);
/*VoiceUp voiceup_cb*/
SCM scheme_VoiceUp(SCM optional);
install_scm_function ("d-VoiceUp", scheme_VoiceUp);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "VoiceUp")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_VoiceUp %d)\n", (int)action_of_name(Denemo.commands, "VoiceUp"));
(void)scm_c_eval_string(text);
g_free(text);
/*VoiceDown voicedown_cb*/
SCM scheme_VoiceDown(SCM optional);
install_scm_function ("d-VoiceDown", scheme_VoiceDown);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "VoiceDown")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_VoiceDown %d)\n", (int)action_of_name(Denemo.commands, "VoiceDown"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddBefore newstaffbefore*/
SCM scheme_AddBefore(SCM optional);
install_scm_function ("d-AddBefore", scheme_AddBefore);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "AddBefore")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddBefore %d)\n", (int)action_of_name(Denemo.commands, "AddBefore"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddAfter dnm_newstaffafter*/
SCM scheme_AddAfter(SCM optional);
install_scm_function ("d-AddAfter", scheme_AddAfter);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "AddAfter")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddAfter %d)\n", (int)action_of_name(Denemo.commands, "AddAfter"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddInitial newstaffinitial*/
SCM scheme_AddInitial(SCM optional);
install_scm_function ("d-AddInitial", scheme_AddInitial);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "AddInitial")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddInitial %d)\n", (int)action_of_name(Denemo.commands, "AddInitial"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddLast newstafflast*/
SCM scheme_AddLast(SCM optional);
install_scm_function ("d-AddLast", scheme_AddLast);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "AddLast")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddLast %d)\n", (int)action_of_name(Denemo.commands, "AddLast"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteBefore delete_staff_before*/
SCM scheme_DeleteBefore(SCM optional);
install_scm_function ("d-DeleteBefore", scheme_DeleteBefore);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "DeleteBefore")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteBefore %d)\n", (int)action_of_name(Denemo.commands, "DeleteBefore"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteStaff delete_staff_current*/
SCM scheme_DeleteStaff(SCM optional);
install_scm_function ("d-DeleteStaff", scheme_DeleteStaff);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "DeleteStaff")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteStaff %d)\n", (int)action_of_name(Denemo.commands, "DeleteStaff"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteAfter delete_staff_after*/
SCM scheme_DeleteAfter(SCM optional);
install_scm_function ("d-DeleteAfter", scheme_DeleteAfter);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "DeleteAfter")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteAfter %d)\n", (int)action_of_name(Denemo.commands, "DeleteAfter"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddVoice dnm_newstaffvoice*/
SCM scheme_AddVoice(SCM optional);
install_scm_function ("d-AddVoice", scheme_AddVoice);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "AddVoice")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddVoice %d)\n", (int)action_of_name(Denemo.commands, "AddVoice"));
(void)scm_c_eval_string(text);
g_free(text);
/*TransposeStaff staff_transposition*/
SCM scheme_TransposeStaff(SCM optional);
install_scm_function ("d-TransposeStaff", scheme_TransposeStaff);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "TransposeStaff")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_TransposeStaff %d)\n", (int)action_of_name(Denemo.commands, "TransposeStaff"));
(void)scm_c_eval_string(text);
g_free(text);
/*StaffProperties staff_properties_change_cb*/
SCM scheme_StaffProperties(SCM optional);
install_scm_function ("d-StaffProperties", scheme_StaffProperties);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "StaffProperties")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_StaffProperties %d)\n", (int)action_of_name(Denemo.commands, "StaffProperties"));
(void)scm_c_eval_string(text);
g_free(text);
/*InitialClef clef_change_initial*/
SCM scheme_InitialClef(SCM optional);
install_scm_function ("d-InitialClef", scheme_InitialClef);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InitialClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InitialClef %d)\n", (int)action_of_name(Denemo.commands, "InitialClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertClef clef_change_insert*/
SCM scheme_InsertClef(SCM optional);
install_scm_function ("d-InsertClef", scheme_InsertClef);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertClef")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertClef %d)\n", (int)action_of_name(Denemo.commands, "InsertClef"));
(void)scm_c_eval_string(text);
g_free(text);
/*InitialKey key_change_initial*/
SCM scheme_InitialKey(SCM optional);
install_scm_function ("d-InitialKey", scheme_InitialKey);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InitialKey")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InitialKey %d)\n", (int)action_of_name(Denemo.commands, "InitialKey"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertKey key_change_insert*/
SCM scheme_InsertKey(SCM optional);
install_scm_function ("d-InsertKey", scheme_InsertKey);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertKey")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertKey %d)\n", (int)action_of_name(Denemo.commands, "InsertKey"));
(void)scm_c_eval_string(text);
g_free(text);
/*InitialTimeSig timesig_change_initial*/
SCM scheme_InitialTimeSig(SCM optional);
install_scm_function ("d-InitialTimeSig", scheme_InitialTimeSig);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InitialTimeSig")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InitialTimeSig %d)\n", (int)action_of_name(Denemo.commands, "InitialTimeSig"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertTimeSig timesig_change_insert*/
SCM scheme_InsertTimeSig(SCM optional);
install_scm_function ("d-InsertTimeSig", scheme_InsertTimeSig);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertTimeSig")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertTimeSig %d)\n", (int)action_of_name(Denemo.commands, "InsertTimeSig"));
(void)scm_c_eval_string(text);
g_free(text);
/*ChangeNotehead set_notehead*/
SCM scheme_ChangeNotehead(SCM optional);
install_scm_function ("d-ChangeNotehead", scheme_ChangeNotehead);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ChangeNotehead")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ChangeNotehead %d)\n", (int)action_of_name(Denemo.commands, "ChangeNotehead"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertStem stem_directive_insert*/
SCM scheme_InsertStem(SCM optional);
install_scm_function ("d-InsertStem", scheme_InsertStem);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertStem")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertStem %d)\n", (int)action_of_name(Denemo.commands, "InsertStem"));
(void)scm_c_eval_string(text);
g_free(text);
/*EditLyric lyric_insert*/
SCM scheme_EditLyric(SCM optional);
install_scm_function ("d-EditLyric", scheme_EditLyric);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "EditLyric")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_EditLyric %d)\n", (int)action_of_name(Denemo.commands, "EditLyric"));
(void)scm_c_eval_string(text);
g_free(text);
/*EditFiguredBass figure_insert*/
SCM scheme_EditFiguredBass(SCM optional);
install_scm_function ("d-EditFiguredBass", scheme_EditFiguredBass);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "EditFiguredBass")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_EditFiguredBass %d)\n", (int)action_of_name(Denemo.commands, "EditFiguredBass"));
(void)scm_c_eval_string(text);
g_free(text);
/*EditChords fakechord_insert*/
SCM scheme_EditChords(SCM optional);
install_scm_function ("d-EditChords", scheme_EditChords);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "EditChords")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_EditChords %d)\n", (int)action_of_name(Denemo.commands, "EditChords"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertDynamic insert_dynamic*/
SCM scheme_InsertDynamic(SCM optional);
install_scm_function ("d-InsertDynamic", scheme_InsertDynamic);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertDynamic")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertDynamic %d)\n", (int)action_of_name(Denemo.commands, "InsertDynamic"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertLilyDirective lily_directive_insert*/
SCM scheme_InsertLilyDirective(SCM optional);
install_scm_function ("d-InsertLilyDirective", scheme_InsertLilyDirective);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertLilyDirective")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertLilyDirective %d)\n", (int)action_of_name(Denemo.commands, "InsertLilyDirective"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertLilyPostfix lily_directive_postfix*/
SCM scheme_InsertLilyPostfix(SCM optional);
install_scm_function ("d-InsertLilyPostfix", scheme_InsertLilyPostfix);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertLilyPostfix")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertLilyPostfix %d)\n", (int)action_of_name(Denemo.commands, "InsertLilyPostfix"));
(void)scm_c_eval_string(text);
g_free(text);
/*InsertBarline insert_barline*/
SCM scheme_InsertBarline(SCM optional);
install_scm_function ("d-InsertBarline", scheme_InsertBarline);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "InsertBarline")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_InsertBarline %d)\n", (int)action_of_name(Denemo.commands, "InsertBarline"));
(void)scm_c_eval_string(text);
g_free(text);
/*GoToMeasure tomeasurenum*/
SCM scheme_GoToMeasure(SCM optional);
install_scm_function ("d-GoToMeasure", scheme_GoToMeasure);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "GoToMeasure")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_GoToMeasure %d)\n", (int)action_of_name(Denemo.commands, "GoToMeasure"));
(void)scm_c_eval_string(text);
g_free(text);
/*GoToBeginning tohome*/
SCM scheme_GoToBeginning(SCM optional);
install_scm_function ("d-GoToBeginning", scheme_GoToBeginning);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "GoToBeginning")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_GoToBeginning %d)\n", (int)action_of_name(Denemo.commands, "GoToBeginning"));
(void)scm_c_eval_string(text);
g_free(text);
/*GoToEnd toend*/
SCM scheme_GoToEnd(SCM optional);
install_scm_function ("d-GoToEnd", scheme_GoToEnd);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "GoToEnd")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_GoToEnd %d)\n", (int)action_of_name(Denemo.commands, "GoToEnd"));
(void)scm_c_eval_string(text);
g_free(text);
/*NextMovement next_movement*/
SCM scheme_NextMovement(SCM optional);
install_scm_function ("d-NextMovement", scheme_NextMovement);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "NextMovement")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_NextMovement %d)\n", (int)action_of_name(Denemo.commands, "NextMovement"));
(void)scm_c_eval_string(text);
g_free(text);
/*PreviousMovement prev_movement*/
SCM scheme_PreviousMovement(SCM optional);
install_scm_function ("d-PreviousMovement", scheme_PreviousMovement);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "PreviousMovement")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_PreviousMovement %d)\n", (int)action_of_name(Denemo.commands, "PreviousMovement"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteMovement delete_movement*/
SCM scheme_DeleteMovement(SCM optional);
install_scm_function ("d-DeleteMovement", scheme_DeleteMovement);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "DeleteMovement")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteMovement %d)\n", (int)action_of_name(Denemo.commands, "DeleteMovement"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteBookmarks deletebookmarks*/
SCM scheme_DeleteBookmarks(SCM optional);
install_scm_function ("d-DeleteBookmarks", scheme_DeleteBookmarks);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "DeleteBookmarks")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteBookmarks %d)\n", (int)action_of_name(Denemo.commands, "DeleteBookmarks"));
(void)scm_c_eval_string(text);
g_free(text);
/*Play ext_midi_playback*/
SCM scheme_Play(SCM optional);
install_scm_function ("d-Play", scheme_Play);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Play")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Play %d)\n", (int)action_of_name(Denemo.commands, "Play"));
(void)scm_c_eval_string(text);
g_free(text);
/*Stop stop_midi_playback*/
SCM scheme_Stop(SCM optional);
install_scm_function ("d-Stop", scheme_Stop);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Stop")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Stop %d)\n", (int)action_of_name(Denemo.commands, "Stop"));
(void)scm_c_eval_string(text);
g_free(text);
/*PlayCSound csoundplayback*/
SCM scheme_PlayCSound(SCM optional);
install_scm_function ("d-PlayCSound", scheme_PlayCSound);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "PlayCSound")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_PlayCSound %d)\n", (int)action_of_name(Denemo.commands, "PlayCSound"));
(void)scm_c_eval_string(text);
g_free(text);
/*PlaybackProperties playback_properties_change*/
SCM scheme_PlaybackProperties(SCM optional);
install_scm_function ("d-PlaybackProperties", scheme_PlaybackProperties);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "PlaybackProperties")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_PlaybackProperties %d)\n", (int)action_of_name(Denemo.commands, "PlaybackProperties"));
(void)scm_c_eval_string(text);
g_free(text);
/*Help browse_manual*/
SCM scheme_Help(SCM optional);
install_scm_function ("d-Help", scheme_Help);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "Help")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_Help %d)\n", (int)action_of_name(Denemo.commands, "Help"));
(void)scm_c_eval_string(text);
g_free(text);
/*About about*/
SCM scheme_About(SCM optional);
install_scm_function ("d-About", scheme_About);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "About")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_About %d)\n", (int)action_of_name(Denemo.commands, "About"));
(void)scm_c_eval_string(text);
g_free(text);
/*MoreCommands morecommands*/
SCM scheme_MoreCommands(SCM optional);
install_scm_function ("d-MoreCommands", scheme_MoreCommands);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "MoreCommands")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_MoreCommands %d)\n", (int)action_of_name(Denemo.commands, "MoreCommands"));
(void)scm_c_eval_string(text);
g_free(text);
/*MyCommands mycommands*/
SCM scheme_MyCommands(SCM optional);
install_scm_function ("d-MyCommands", scheme_MyCommands);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "MyCommands")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_MyCommands %d)\n", (int)action_of_name(Denemo.commands, "MyCommands"));
(void)scm_c_eval_string(text);
g_free(text);
/*AddBookmark addbookmark*/
SCM scheme_AddBookmark(SCM optional);
install_scm_function ("d-AddBookmark", scheme_AddBookmark);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "AddBookmark")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_AddBookmark %d)\n", (int)action_of_name(Denemo.commands, "AddBookmark"));
(void)scm_c_eval_string(text);
g_free(text);
/*GotoBookmark gotobookmark*/
SCM scheme_GotoBookmark(SCM optional);
install_scm_function ("d-GotoBookmark", scheme_GotoBookmark);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "GotoBookmark")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_GotoBookmark %d)\n", (int)action_of_name(Denemo.commands, "GotoBookmark"));
(void)scm_c_eval_string(text);
g_free(text);
/*NextBookmark nextbookmark*/
SCM scheme_NextBookmark(SCM optional);
install_scm_function ("d-NextBookmark", scheme_NextBookmark);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "NextBookmark")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_NextBookmark %d)\n", (int)action_of_name(Denemo.commands, "NextBookmark"));
(void)scm_c_eval_string(text);
g_free(text);
/*PrevBookmark prevbookmark*/
SCM scheme_PrevBookmark(SCM optional);
install_scm_function ("d-PrevBookmark", scheme_PrevBookmark);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "PrevBookmark")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_PrevBookmark %d)\n", (int)action_of_name(Denemo.commands, "PrevBookmark"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleEdit toggle_edit_mode*/
SCM scheme_ToggleEdit(SCM optional);
install_scm_function ("d-ToggleEdit", scheme_ToggleEdit);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleEdit")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleEdit %d)\n", (int)action_of_name(Denemo.commands, "ToggleEdit"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleRest toggle_rest_mode*/
SCM scheme_ToggleRest(SCM optional);
install_scm_function ("d-ToggleRest", scheme_ToggleRest);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleRest")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleRest %d)\n", (int)action_of_name(Denemo.commands, "ToggleRest"));
(void)scm_c_eval_string(text);
g_free(text);
/*ToggleRhythm toggle_rhythm_mode*/
SCM scheme_ToggleRhythm(SCM optional);
install_scm_function ("d-ToggleRhythm", scheme_ToggleRhythm);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ToggleRhythm")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ToggleRhythm %d)\n", (int)action_of_name(Denemo.commands, "ToggleRhythm"));
(void)scm_c_eval_string(text);
g_free(text);
/*ClearOverlay clear_overlay*/
SCM scheme_ClearOverlay(SCM optional);
install_scm_function ("d-ClearOverlay", scheme_ClearOverlay);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "ClearOverlay")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_ClearOverlay %d)\n", (int)action_of_name(Denemo.commands, "ClearOverlay"));
(void)scm_c_eval_string(text);
g_free(text);
/*CreateRhythm create_rhythm_cb*/
SCM scheme_CreateRhythm(SCM optional);
install_scm_function ("d-CreateRhythm", scheme_CreateRhythm);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "CreateRhythm")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_CreateRhythm %d)\n", (int)action_of_name(Denemo.commands, "CreateRhythm"));
(void)scm_c_eval_string(text);
g_free(text);
/*DeleteRhythm delete_rhythm_cb*/
SCM scheme_DeleteRhythm(SCM optional);
install_scm_function ("d-DeleteRhythm", scheme_DeleteRhythm);
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "DeleteRhythm")), "scm", (gpointer)1);
text = g_strdup_printf("(define dnm_DeleteRhythm %d)\n", (int)action_of_name(Denemo.commands, "DeleteRhythm"));
(void)scm_c_eval_string(text);
g_free(text);
SCM scheme_ChangeToA(SCM optional);
install_scm_function ("d-ChangeToA", scheme_ChangeToA);
SCM scheme_InsertA(SCM optional);
install_scm_function ("d-InsertA", scheme_InsertA);
SCM scheme_ChangeToB(SCM optional);
install_scm_function ("d-ChangeToB", scheme_ChangeToB);
SCM scheme_InsertB(SCM optional);
install_scm_function ("d-InsertB", scheme_InsertB);
SCM scheme_ChangeToC(SCM optional);
install_scm_function ("d-ChangeToC", scheme_ChangeToC);
SCM scheme_InsertC(SCM optional);
install_scm_function ("d-InsertC", scheme_InsertC);
SCM scheme_ChangeToD(SCM optional);
install_scm_function ("d-ChangeToD", scheme_ChangeToD);
SCM scheme_InsertD(SCM optional);
install_scm_function ("d-InsertD", scheme_InsertD);
SCM scheme_ChangeToE(SCM optional);
install_scm_function ("d-ChangeToE", scheme_ChangeToE);
SCM scheme_InsertE(SCM optional);
install_scm_function ("d-InsertE", scheme_InsertE);
SCM scheme_ChangeToF(SCM optional);
install_scm_function ("d-ChangeToF", scheme_ChangeToF);
SCM scheme_InsertF(SCM optional);
install_scm_function ("d-InsertF", scheme_InsertF);
SCM scheme_ChangeToG(SCM optional);
install_scm_function ("d-ChangeToG", scheme_ChangeToG);
SCM scheme_InsertG(SCM optional);
install_scm_function ("d-InsertG", scheme_InsertG);
/*0 */
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "0")), "scm", (gpointer)1);
SCM scheme_0(SCM optional);
install_scm_function ("d-0", scheme_0);
SCM scheme_InsertDur0(SCM optional);
install_scm_function ("d-InsertDur0", scheme_InsertRest0);
SCM scheme_ChangeDur0(SCM optional);
install_scm_function ("d-ChangeDur0", scheme_ChangeRest0);
SCM scheme_InsertRest0(SCM optional);
install_scm_function ("d-InsertRest0", scheme_InsertRest0);
SCM scheme_ChangeRest0(SCM optional);
install_scm_function ("d-ChangeRest0", scheme_ChangeRest0);
/*1 */
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "1")), "scm", (gpointer)1);
SCM scheme_1(SCM optional);
install_scm_function ("d-1", scheme_1);
SCM scheme_InsertDur1(SCM optional);
install_scm_function ("d-InsertDur1", scheme_InsertRest1);
SCM scheme_ChangeDur1(SCM optional);
install_scm_function ("d-ChangeDur1", scheme_ChangeRest1);
SCM scheme_InsertRest1(SCM optional);
install_scm_function ("d-InsertRest1", scheme_InsertRest1);
SCM scheme_ChangeRest1(SCM optional);
install_scm_function ("d-ChangeRest1", scheme_ChangeRest1);
/*2 */
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "2")), "scm", (gpointer)1);
SCM scheme_2(SCM optional);
install_scm_function ("d-2", scheme_2);
SCM scheme_InsertDur2(SCM optional);
install_scm_function ("d-InsertDur2", scheme_InsertRest2);
SCM scheme_ChangeDur2(SCM optional);
install_scm_function ("d-ChangeDur2", scheme_ChangeRest2);
SCM scheme_InsertRest2(SCM optional);
install_scm_function ("d-InsertRest2", scheme_InsertRest2);
SCM scheme_ChangeRest2(SCM optional);
install_scm_function ("d-ChangeRest2", scheme_ChangeRest2);
/*3 */
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "3")), "scm", (gpointer)1);
SCM scheme_3(SCM optional);
install_scm_function ("d-3", scheme_3);
SCM scheme_InsertDur3(SCM optional);
install_scm_function ("d-InsertDur3", scheme_InsertRest3);
SCM scheme_ChangeDur3(SCM optional);
install_scm_function ("d-ChangeDur3", scheme_ChangeRest3);
SCM scheme_InsertRest3(SCM optional);
install_scm_function ("d-InsertRest3", scheme_InsertRest3);
SCM scheme_ChangeRest3(SCM optional);
install_scm_function ("d-ChangeRest3", scheme_ChangeRest3);
/*4 */
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "4")), "scm", (gpointer)1);
SCM scheme_4(SCM optional);
install_scm_function ("d-4", scheme_4);
SCM scheme_InsertDur4(SCM optional);
install_scm_function ("d-InsertDur4", scheme_InsertRest4);
SCM scheme_ChangeDur4(SCM optional);
install_scm_function ("d-ChangeDur4", scheme_ChangeRest4);
SCM scheme_InsertRest4(SCM optional);
install_scm_function ("d-InsertRest4", scheme_InsertRest4);
SCM scheme_ChangeRest4(SCM optional);
install_scm_function ("d-ChangeRest4", scheme_ChangeRest4);
/*5 */
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "5")), "scm", (gpointer)1);
SCM scheme_5(SCM optional);
install_scm_function ("d-5", scheme_5);
SCM scheme_InsertDur5(SCM optional);
install_scm_function ("d-InsertDur5", scheme_InsertRest5);
SCM scheme_ChangeDur5(SCM optional);
install_scm_function ("d-ChangeDur5", scheme_ChangeRest5);
SCM scheme_InsertRest5(SCM optional);
install_scm_function ("d-InsertRest5", scheme_InsertRest5);
SCM scheme_ChangeRest5(SCM optional);
install_scm_function ("d-ChangeRest5", scheme_ChangeRest5);
/*6 */
g_object_set_data(G_OBJECT(action_of_name(Denemo.commands, "6")), "scm", (gpointer)1);
SCM scheme_6(SCM optional);
install_scm_function ("d-6", scheme_6);
SCM scheme_InsertDur6(SCM optional);
install_scm_function ("d-InsertDur6", scheme_InsertRest6);
SCM scheme_ChangeDur6(SCM optional);
install_scm_function ("d-ChangeDur6", scheme_ChangeRest6);
SCM scheme_InsertRest6(SCM optional);
install_scm_function ("d-InsertRest6", scheme_InsertRest6);
SCM scheme_ChangeRest6(SCM optional);
install_scm_function ("d-ChangeRest6", scheme_ChangeRest6);
