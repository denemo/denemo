/*CursorLeft cursorleft*/
scm_c_define_gsubr ("CursorLeft", 0, 0, 0, scheme_CursorLeft);
/*CursorDown cursordown*/
scm_c_define_gsubr ("CursorDown", 0, 0, 0, scheme_CursorDown);
/*CursorUp cursorup*/
scm_c_define_gsubr ("CursorUp", 0, 0, 0, scheme_CursorUp);
/*CursorRight cursorright*/
scm_c_define_gsubr ("CursorRight", 0, 0, 0, scheme_CursorRight);
/*StaffUp staffup*/
scm_c_define_gsubr ("StaffUp", 0, 0, 0, scheme_StaffUp);
/*StaffDown staffdown*/
scm_c_define_gsubr ("StaffDown", 0, 0, 0, scheme_StaffDown);
/*MeasureLeft measureleft*/
scm_c_define_gsubr ("MeasureLeft", 0, 0, 0, scheme_MeasureLeft);
/*MeasureRight measureright*/
scm_c_define_gsubr ("MeasureRight", 0, 0, 0, scheme_MeasureRight);
/*A go_to_A_key*/
scm_c_define_gsubr ("A", 0, 0, 0, scheme_A);
/*B go_to_B_key*/
scm_c_define_gsubr ("B", 0, 0, 0, scheme_B);
/*C go_to_C_key*/
scm_c_define_gsubr ("C", 0, 0, 0, scheme_C);
/*D go_to_D_key*/
scm_c_define_gsubr ("D", 0, 0, 0, scheme_D);
/*E go_to_E_key*/
scm_c_define_gsubr ("E", 0, 0, 0, scheme_E);
/*F go_to_F_key*/
scm_c_define_gsubr ("F", 0, 0, 0, scheme_F);
/*G go_to_G_key*/
scm_c_define_gsubr ("G", 0, 0, 0, scheme_G);
/*OctaveUp octave_up_key*/
scm_c_define_gsubr ("OctaveUp", 0, 0, 0, scheme_OctaveUp);
/*OctaveDown octave_down_key*/
scm_c_define_gsubr ("OctaveDown", 0, 0, 0, scheme_OctaveDown);
/*WholeNote insert_chord_0key*/
scm_c_define_gsubr ("WholeNote", 0, 0, 0, scheme_WholeNote);
/*HalfNote insert_chord_1key*/
scm_c_define_gsubr ("HalfNote", 0, 0, 0, scheme_HalfNote);
/*QuarterNote insert_chord_2key*/
scm_c_define_gsubr ("QuarterNote", 0, 0, 0, scheme_QuarterNote);
/*EighthNote insert_chord_3key*/
scm_c_define_gsubr ("EighthNote", 0, 0, 0, scheme_EighthNote);
/*SixteenthNote insert_chord_4key*/
scm_c_define_gsubr ("SixteenthNote", 0, 0, 0, scheme_SixteenthNote);
/*ThirtysecondNote insert_chord_5key*/
scm_c_define_gsubr ("ThirtysecondNote", 0, 0, 0, scheme_ThirtysecondNote);
/*SixtyfourthNote insert_chord_6key*/
scm_c_define_gsubr ("SixtyfourthNote", 0, 0, 0, scheme_SixtyfourthNote);
/*InsertBlankWholeNote insert_blankchord_0key*/
scm_c_define_gsubr ("InsertBlankWholeNote", 0, 0, 0, scheme_InsertBlankWholeNote);
/*InsertBlankHalfNote insert_blankchord_1key*/
scm_c_define_gsubr ("InsertBlankHalfNote", 0, 0, 0, scheme_InsertBlankHalfNote);
/*InsertBlankQuarterNote insert_blankchord_2key*/
scm_c_define_gsubr ("InsertBlankQuarterNote", 0, 0, 0, scheme_InsertBlankQuarterNote);
/*InsertBlankEighthNote insert_blankchord_3key*/
scm_c_define_gsubr ("InsertBlankEighthNote", 0, 0, 0, scheme_InsertBlankEighthNote);
/*InsertBlankSixteenthNote insert_blankchord_4key*/
scm_c_define_gsubr ("InsertBlankSixteenthNote", 0, 0, 0, scheme_InsertBlankSixteenthNote);
/*InsertBlankThirtysecondNote insert_blankchord_5key*/
scm_c_define_gsubr ("InsertBlankThirtysecondNote", 0, 0, 0, scheme_InsertBlankThirtysecondNote);
/*InsertBlankSixtyfourthNote insert_blankchord_6key*/
scm_c_define_gsubr ("InsertBlankSixtyfourthNote", 0, 0, 0, scheme_InsertBlankSixtyfourthNote);
/*ToggleRestMode rest_toggle_key*/
scm_c_define_gsubr ("ToggleRestMode", 0, 0, 0, scheme_ToggleRestMode);
/*ToggleBlankMode toggle_blank*/
scm_c_define_gsubr ("ToggleBlankMode", 0, 0, 0, scheme_ToggleBlankMode);
/*InsertWholeRest insert_rest_0key*/
scm_c_define_gsubr ("InsertWholeRest", 0, 0, 0, scheme_InsertWholeRest);
/*InsertHalfRest insert_rest_1key*/
scm_c_define_gsubr ("InsertHalfRest", 0, 0, 0, scheme_InsertHalfRest);
/*InsertQuarterRest insert_rest_2key*/
scm_c_define_gsubr ("InsertQuarterRest", 0, 0, 0, scheme_InsertQuarterRest);
/*InsertEighthRest insert_rest_3key*/
scm_c_define_gsubr ("InsertEighthRest", 0, 0, 0, scheme_InsertEighthRest);
/*InsertSixteenthRest insert_rest_4key*/
scm_c_define_gsubr ("InsertSixteenthRest", 0, 0, 0, scheme_InsertSixteenthRest);
/*InsertThirtysecondRest insert_rest_5key*/
scm_c_define_gsubr ("InsertThirtysecondRest", 0, 0, 0, scheme_InsertThirtysecondRest);
/*InsertSixtyfourthRest insert_rest_6key*/
scm_c_define_gsubr ("InsertSixtyfourthRest", 0, 0, 0, scheme_InsertSixtyfourthRest);
/*InsertDuplet insert_duplet*/
scm_c_define_gsubr ("InsertDuplet", 0, 0, 0, scheme_InsertDuplet);
/*InsertTriplet insert_triplet*/
scm_c_define_gsubr ("InsertTriplet", 0, 0, 0, scheme_InsertTriplet);
/*StartTriplet start_triplet*/
scm_c_define_gsubr ("StartTriplet", 0, 0, 0, scheme_StartTriplet);
/*EndTuplet end_tuplet*/
scm_c_define_gsubr ("EndTuplet", 0, 0, 0, scheme_EndTuplet);
/*InsertQuadtuplet insert_quadtuplet*/
scm_c_define_gsubr ("InsertQuadtuplet", 0, 0, 0, scheme_InsertQuadtuplet);
/*InsertQuintuplet insert_quintuplet*/
scm_c_define_gsubr ("InsertQuintuplet", 0, 0, 0, scheme_InsertQuintuplet);
/*InsertSextuplet insert_sextuplet*/
scm_c_define_gsubr ("InsertSextuplet", 0, 0, 0, scheme_InsertSextuplet);
/*InsertSeptuplet insert_septuplet*/
scm_c_define_gsubr ("InsertSeptuplet", 0, 0, 0, scheme_InsertSeptuplet);
/*AddTone add_tone_key*/
scm_c_define_gsubr ("AddTone", 0, 0, 0, scheme_AddTone);
/*RemoveTone remove_tone_key*/
scm_c_define_gsubr ("RemoveTone", 0, 0, 0, scheme_RemoveTone);
/*SharpenOrStemDown sharpen_key*/
scm_c_define_gsubr ("SharpenOrStemDown", 0, 0, 0, scheme_SharpenOrStemDown);
/*FlattenOrStemUp flatten_key*/
scm_c_define_gsubr ("FlattenOrStemUp", 0, 0, 0, scheme_FlattenOrStemUp);
/*AddDot add_dot_key*/
scm_c_define_gsubr ("AddDot", 0, 0, 0, scheme_AddDot);
/*RemoveDot remove_dot_key*/
scm_c_define_gsubr ("RemoveDot", 0, 0, 0, scheme_RemoveDot);
/*InsertTiedNote tie_notes_key*/
scm_c_define_gsubr ("InsertTiedNote", 0, 0, 0, scheme_InsertTiedNote);
/*DeleteObject deleteobject*/
scm_c_define_gsubr ("DeleteObject", 0, 0, 0, scheme_DeleteObject);
/*DeletePreviousObject deletepreviousobject*/
scm_c_define_gsubr ("DeletePreviousObject", 0, 0, 0, scheme_DeletePreviousObject);
/*InsertMeasure insert_measure_key*/
scm_c_define_gsubr ("InsertMeasure", 0, 0, 0, scheme_InsertMeasure);
/*AppendMeasure append_measure_key*/
scm_c_define_gsubr ("AppendMeasure", 0, 0, 0, scheme_AppendMeasure);
/*DeleteMeasure deletemeasure*/
scm_c_define_gsubr ("DeleteMeasure", 0, 0, 0, scheme_DeleteMeasure);
/*DeleteMeasureAllStaffs deletemeasureallstaffs*/
scm_c_define_gsubr ("DeleteMeasureAllStaffs", 0, 0, 0, scheme_DeleteMeasureAllStaffs);
/*ShrinkMeasures adjust_measure_less_width_key*/
scm_c_define_gsubr ("ShrinkMeasures", 0, 0, 0, scheme_ShrinkMeasures);
/*WidenMeasures adjust_measure_more_width_key*/
scm_c_define_gsubr ("WidenMeasures", 0, 0, 0, scheme_WidenMeasures);
/*ShorterStaffs adjust_staff_less_height_key*/
scm_c_define_gsubr ("ShorterStaffs", 0, 0, 0, scheme_ShorterStaffs);
/*TallerStaffs adjust_staff_more_height_key*/
scm_c_define_gsubr ("TallerStaffs", 0, 0, 0, scheme_TallerStaffs);
/*InsertTrebleClef newcleftreble*/
scm_c_define_gsubr ("InsertTrebleClef", 0, 0, 0, scheme_InsertTrebleClef);
/*InsertBassClef newclefbass*/
scm_c_define_gsubr ("InsertBassClef", 0, 0, 0, scheme_InsertBassClef);
/*Insertg8clef newclefg8*/
scm_c_define_gsubr ("Insertg8clef", 0, 0, 0, scheme_Insertg8clef);
/*InsertAltoClef newclefalto*/
scm_c_define_gsubr ("InsertAltoClef", 0, 0, 0, scheme_InsertAltoClef);
/*InsertTenorClef newcleftenor*/
scm_c_define_gsubr ("InsertTenorClef", 0, 0, 0, scheme_InsertTenorClef);
/*InsertSopranoClef newclefsoprano*/
scm_c_define_gsubr ("InsertSopranoClef", 0, 0, 0, scheme_InsertSopranoClef);
/*SetInitialTrebleClef setcleftreble*/
scm_c_define_gsubr ("SetInitialTrebleClef", 0, 0, 0, scheme_SetInitialTrebleClef);
/*SetInitialBassClef setclefbass*/
scm_c_define_gsubr ("SetInitialBassClef", 0, 0, 0, scheme_SetInitialBassClef);
/*SetInitialg8clef setclefg8*/
scm_c_define_gsubr ("SetInitialg8clef", 0, 0, 0, scheme_SetInitialg8clef);
/*SetInitialAltoClef setclefalto*/
scm_c_define_gsubr ("SetInitialAltoClef", 0, 0, 0, scheme_SetInitialAltoClef);
/*SetInitialTenorClef setcleftenor*/
scm_c_define_gsubr ("SetInitialTenorClef", 0, 0, 0, scheme_SetInitialTenorClef);
/*SetInitialSopranoClef setclefsoprano*/
scm_c_define_gsubr ("SetInitialSopranoClef", 0, 0, 0, scheme_SetInitialSopranoClef);
/*Insert22Time newtimesig22*/
scm_c_define_gsubr ("Insert22Time", 0, 0, 0, scheme_Insert22Time);
/*Insert32Time newtimesig32*/
scm_c_define_gsubr ("Insert32Time", 0, 0, 0, scheme_Insert32Time);
/*Insert42Time newtimesig42*/
scm_c_define_gsubr ("Insert42Time", 0, 0, 0, scheme_Insert42Time);
/*Insert44Time newtimesig44*/
scm_c_define_gsubr ("Insert44Time", 0, 0, 0, scheme_Insert44Time);
/*Insert34Time newtimesig34*/
scm_c_define_gsubr ("Insert34Time", 0, 0, 0, scheme_Insert34Time);
/*Insert24Time newtimesig24*/
scm_c_define_gsubr ("Insert24Time", 0, 0, 0, scheme_Insert24Time);
/*Insert64Time newtimesig64*/
scm_c_define_gsubr ("Insert64Time", 0, 0, 0, scheme_Insert64Time);
/*Insert38Time newtimesig38*/
scm_c_define_gsubr ("Insert38Time", 0, 0, 0, scheme_Insert38Time);
/*Insert68Time newtimesig68*/
scm_c_define_gsubr ("Insert68Time", 0, 0, 0, scheme_Insert68Time);
/*Insert128Time newtimesig128*/
scm_c_define_gsubr ("Insert128Time", 0, 0, 0, scheme_Insert128Time);
/*Insert98Time newtimesig98*/
scm_c_define_gsubr ("Insert98Time", 0, 0, 0, scheme_Insert98Time);
/*Set22Time settimesig22*/
scm_c_define_gsubr ("Set22Time", 0, 0, 0, scheme_Set22Time);
/*Set32Time settimesig32*/
scm_c_define_gsubr ("Set32Time", 0, 0, 0, scheme_Set32Time);
/*Set42Time settimesig42*/
scm_c_define_gsubr ("Set42Time", 0, 0, 0, scheme_Set42Time);
/*Set44Time settimesig44*/
scm_c_define_gsubr ("Set44Time", 0, 0, 0, scheme_Set44Time);
/*Set34Time settimesig34*/
scm_c_define_gsubr ("Set34Time", 0, 0, 0, scheme_Set34Time);
/*Set24Time settimesig24*/
scm_c_define_gsubr ("Set24Time", 0, 0, 0, scheme_Set24Time);
/*Set64Time settimesig64*/
scm_c_define_gsubr ("Set64Time", 0, 0, 0, scheme_Set64Time);
/*Set38Time settimesig38*/
scm_c_define_gsubr ("Set38Time", 0, 0, 0, scheme_Set38Time);
/*Set68Time settimesig68*/
scm_c_define_gsubr ("Set68Time", 0, 0, 0, scheme_Set68Time);
/*Set128Time settimesig128*/
scm_c_define_gsubr ("Set128Time", 0, 0, 0, scheme_Set128Time);
/*Set98Time settimesig98*/
scm_c_define_gsubr ("Set98Time", 0, 0, 0, scheme_Set98Time);
/*InsertCmaj newkeysigcmaj*/
scm_c_define_gsubr ("InsertCmaj", 0, 0, 0, scheme_InsertCmaj);
/*InsertGmaj newkeysiggmaj*/
scm_c_define_gsubr ("InsertGmaj", 0, 0, 0, scheme_InsertGmaj);
/*InsertDmaj newkeysigdmaj*/
scm_c_define_gsubr ("InsertDmaj", 0, 0, 0, scheme_InsertDmaj);
/*InsertAmaj newkeysigamaj*/
scm_c_define_gsubr ("InsertAmaj", 0, 0, 0, scheme_InsertAmaj);
/*InsertEmaj newkeysigemaj*/
scm_c_define_gsubr ("InsertEmaj", 0, 0, 0, scheme_InsertEmaj);
/*InsertBmaj newkeysigbmaj*/
scm_c_define_gsubr ("InsertBmaj", 0, 0, 0, scheme_InsertBmaj);
/*InsertFSharpmaj newkeysigfsharpmaj*/
scm_c_define_gsubr ("InsertFSharpmaj", 0, 0, 0, scheme_InsertFSharpmaj);
/*InsertCSharpmaj newkeysigcsharpmaj*/
scm_c_define_gsubr ("InsertCSharpmaj", 0, 0, 0, scheme_InsertCSharpmaj);
/*InsertFmaj newkeysigfmaj*/
scm_c_define_gsubr ("InsertFmaj", 0, 0, 0, scheme_InsertFmaj);
/*InsertBflatmaj newkeysigbflatmaj*/
scm_c_define_gsubr ("InsertBflatmaj", 0, 0, 0, scheme_InsertBflatmaj);
/*InsertEflatmaj newkeysigeflatmaj*/
scm_c_define_gsubr ("InsertEflatmaj", 0, 0, 0, scheme_InsertEflatmaj);
/*InsertAflatmaj newkeysigaflatmaj*/
scm_c_define_gsubr ("InsertAflatmaj", 0, 0, 0, scheme_InsertAflatmaj);
/*InsertDflatmaj newkeysigdflatmaj*/
scm_c_define_gsubr ("InsertDflatmaj", 0, 0, 0, scheme_InsertDflatmaj);
/*InsertGflatmaj newkeysiggflatmaj*/
scm_c_define_gsubr ("InsertGflatmaj", 0, 0, 0, scheme_InsertGflatmaj);
/*InsertCflatmaj newkeysigcflatmaj*/
scm_c_define_gsubr ("InsertCflatmaj", 0, 0, 0, scheme_InsertCflatmaj);
/*InsertAmin newkeysigamin*/
scm_c_define_gsubr ("InsertAmin", 0, 0, 0, scheme_InsertAmin);
/*InsertEmin newkeysigemin*/
scm_c_define_gsubr ("InsertEmin", 0, 0, 0, scheme_InsertEmin);
/*InsertBmin newkeysigbmin*/
scm_c_define_gsubr ("InsertBmin", 0, 0, 0, scheme_InsertBmin);
/*InsertFSharpmin newkeysigfsharpmin*/
scm_c_define_gsubr ("InsertFSharpmin", 0, 0, 0, scheme_InsertFSharpmin);
/*InsertCSharpmin newkeysigcsharpmin*/
scm_c_define_gsubr ("InsertCSharpmin", 0, 0, 0, scheme_InsertCSharpmin);
/*InsertGSharpmin newkeysiggsharpmin*/
scm_c_define_gsubr ("InsertGSharpmin", 0, 0, 0, scheme_InsertGSharpmin);
/*InsertDSharpmin newkeysigdsharpmin*/
scm_c_define_gsubr ("InsertDSharpmin", 0, 0, 0, scheme_InsertDSharpmin);
/*InsertASharpmin newkeysigasharpmin*/
scm_c_define_gsubr ("InsertASharpmin", 0, 0, 0, scheme_InsertASharpmin);
/*InsertDmin newkeysigdmin*/
scm_c_define_gsubr ("InsertDmin", 0, 0, 0, scheme_InsertDmin);
/*InsertGmin newkeysiggmin*/
scm_c_define_gsubr ("InsertGmin", 0, 0, 0, scheme_InsertGmin);
/*InsertCmin newkeysigcmin*/
scm_c_define_gsubr ("InsertCmin", 0, 0, 0, scheme_InsertCmin);
/*InsertFmin newkeysigfmin*/
scm_c_define_gsubr ("InsertFmin", 0, 0, 0, scheme_InsertFmin);
/*InsertBflatmin newkeysigbflatmin*/
scm_c_define_gsubr ("InsertBflatmin", 0, 0, 0, scheme_InsertBflatmin);
/*InsertEflatmin newkeysigeflatmin*/
scm_c_define_gsubr ("InsertEflatmin", 0, 0, 0, scheme_InsertEflatmin);
/*InsertAflatmin newkeysigaflatmin*/
scm_c_define_gsubr ("InsertAflatmin", 0, 0, 0, scheme_InsertAflatmin);
/*SetInitialCmaj setkeysigcmaj*/
scm_c_define_gsubr ("SetInitialCmaj", 0, 0, 0, scheme_SetInitialCmaj);
/*SetInitialGmaj setkeysiggmaj*/
scm_c_define_gsubr ("SetInitialGmaj", 0, 0, 0, scheme_SetInitialGmaj);
/*SetInitialDmaj setkeysigdmaj*/
scm_c_define_gsubr ("SetInitialDmaj", 0, 0, 0, scheme_SetInitialDmaj);
/*SetInitialAmaj setkeysigamaj*/
scm_c_define_gsubr ("SetInitialAmaj", 0, 0, 0, scheme_SetInitialAmaj);
/*SetInitialEmaj setkeysigemaj*/
scm_c_define_gsubr ("SetInitialEmaj", 0, 0, 0, scheme_SetInitialEmaj);
/*SetInitialBmaj setkeysigbmaj*/
scm_c_define_gsubr ("SetInitialBmaj", 0, 0, 0, scheme_SetInitialBmaj);
/*SetInitialFSharpmaj setkeysigfsharpmaj*/
scm_c_define_gsubr ("SetInitialFSharpmaj", 0, 0, 0, scheme_SetInitialFSharpmaj);
/*SetInitialCSharpmaj setkeysigcsharpmaj*/
scm_c_define_gsubr ("SetInitialCSharpmaj", 0, 0, 0, scheme_SetInitialCSharpmaj);
/*SetInitialFmaj setkeysigfmaj*/
scm_c_define_gsubr ("SetInitialFmaj", 0, 0, 0, scheme_SetInitialFmaj);
/*SetInitialBflatmaj setkeysigbflatmaj*/
scm_c_define_gsubr ("SetInitialBflatmaj", 0, 0, 0, scheme_SetInitialBflatmaj);
/*SetInitialEflatmaj setkeysigeflatmaj*/
scm_c_define_gsubr ("SetInitialEflatmaj", 0, 0, 0, scheme_SetInitialEflatmaj);
/*SetInitialAflatmaj setkeysigaflatmaj*/
scm_c_define_gsubr ("SetInitialAflatmaj", 0, 0, 0, scheme_SetInitialAflatmaj);
/*SetInitialDflatmaj setkeysigdflatmaj*/
scm_c_define_gsubr ("SetInitialDflatmaj", 0, 0, 0, scheme_SetInitialDflatmaj);
/*SetInitialGflatmaj setkeysiggflatmaj*/
scm_c_define_gsubr ("SetInitialGflatmaj", 0, 0, 0, scheme_SetInitialGflatmaj);
/*SetInitialCflatmaj setkeysigcflatmaj*/
scm_c_define_gsubr ("SetInitialCflatmaj", 0, 0, 0, scheme_SetInitialCflatmaj);
/*SetInitialAmin setkeysigamin*/
scm_c_define_gsubr ("SetInitialAmin", 0, 0, 0, scheme_SetInitialAmin);
/*SetInitialEmin setkeysigemin*/
scm_c_define_gsubr ("SetInitialEmin", 0, 0, 0, scheme_SetInitialEmin);
/*SetInitialBmin setkeysigbmin*/
scm_c_define_gsubr ("SetInitialBmin", 0, 0, 0, scheme_SetInitialBmin);
/*SetInitialFSharpmin setkeysigfsharpmin*/
scm_c_define_gsubr ("SetInitialFSharpmin", 0, 0, 0, scheme_SetInitialFSharpmin);
/*SetInitialCSharpmin setkeysigcsharpmin*/
scm_c_define_gsubr ("SetInitialCSharpmin", 0, 0, 0, scheme_SetInitialCSharpmin);
/*SetInitialGSharpmin setkeysiggsharpmin*/
scm_c_define_gsubr ("SetInitialGSharpmin", 0, 0, 0, scheme_SetInitialGSharpmin);
/*SetInitialDSharpmin setkeysigdsharpmin*/
scm_c_define_gsubr ("SetInitialDSharpmin", 0, 0, 0, scheme_SetInitialDSharpmin);
/*SetInitialASharpmin setkeysigasharpmin*/
scm_c_define_gsubr ("SetInitialASharpmin", 0, 0, 0, scheme_SetInitialASharpmin);
/*SetInitialDmin setkeysigdmin*/
scm_c_define_gsubr ("SetInitialDmin", 0, 0, 0, scheme_SetInitialDmin);
/*SetInitialGmin setkeysiggmin*/
scm_c_define_gsubr ("SetInitialGmin", 0, 0, 0, scheme_SetInitialGmin);
/*SetInitialCmin setkeysigcmin*/
scm_c_define_gsubr ("SetInitialCmin", 0, 0, 0, scheme_SetInitialCmin);
/*SetInitialFmin setkeysigfmin*/
scm_c_define_gsubr ("SetInitialFmin", 0, 0, 0, scheme_SetInitialFmin);
/*SetInitialBflatmin setkeysigbflatmin*/
scm_c_define_gsubr ("SetInitialBflatmin", 0, 0, 0, scheme_SetInitialBflatmin);
/*SetInitialEflatmin setkeysigeflatmin*/
scm_c_define_gsubr ("SetInitialEflatmin", 0, 0, 0, scheme_SetInitialEflatmin);
/*SetInitialAflatmin setkeysigaflatmin*/
scm_c_define_gsubr ("SetInitialAflatmin", 0, 0, 0, scheme_SetInitialAflatmin);
/*SetMark set_mark*/
scm_c_define_gsubr ("SetMark", 0, 0, 0, scheme_SetMark);
/*UnsetMark unset_mark*/
scm_c_define_gsubr ("UnsetMark", 0, 0, 0, scheme_UnsetMark);
/*ToggleBeginSlur toggle_begin_slur*/
scm_c_define_gsubr ("ToggleBeginSlur", 0, 0, 0, scheme_ToggleBeginSlur);
/*ToggleEndSlur toggle_end_slur*/
scm_c_define_gsubr ("ToggleEndSlur", 0, 0, 0, scheme_ToggleEndSlur);
/*ToggleStartCrescendo toggle_start_crescendo*/
scm_c_define_gsubr ("ToggleStartCrescendo", 0, 0, 0, scheme_ToggleStartCrescendo);
/*ToggleEndCrescendo toggle_end_crescendo*/
scm_c_define_gsubr ("ToggleEndCrescendo", 0, 0, 0, scheme_ToggleEndCrescendo);
/*ToggleStartDiminuendo toggle_start_diminuendo*/
scm_c_define_gsubr ("ToggleStartDiminuendo", 0, 0, 0, scheme_ToggleStartDiminuendo);
/*ToggleEndDiminuendo toggle_end_diminuendo*/
scm_c_define_gsubr ("ToggleEndDiminuendo", 0, 0, 0, scheme_ToggleEndDiminuendo);
/*ToggleAccent add_accent*/
scm_c_define_gsubr ("ToggleAccent", 0, 0, 0, scheme_ToggleAccent);
/*ToggleFermata add_fermata*/
scm_c_define_gsubr ("ToggleFermata", 0, 0, 0, scheme_ToggleFermata);
/*ToggleStaccato add_staccato*/
scm_c_define_gsubr ("ToggleStaccato", 0, 0, 0, scheme_ToggleStaccato);
/*ToggleTenuto add_tenuto*/
scm_c_define_gsubr ("ToggleTenuto", 0, 0, 0, scheme_ToggleTenuto);
/*ToggleTrill add_trill*/
scm_c_define_gsubr ("ToggleTrill", 0, 0, 0, scheme_ToggleTrill);
/*ToggleTurn add_turn*/
scm_c_define_gsubr ("ToggleTurn", 0, 0, 0, scheme_ToggleTurn);
/*ToggleMordent add_mordent*/
scm_c_define_gsubr ("ToggleMordent", 0, 0, 0, scheme_ToggleMordent);
/*ToggleStaccatissimo add_staccatissimo*/
scm_c_define_gsubr ("ToggleStaccatissimo", 0, 0, 0, scheme_ToggleStaccatissimo);
/*ToggleCoda add_coda*/
scm_c_define_gsubr ("ToggleCoda", 0, 0, 0, scheme_ToggleCoda);
/*ToggleFlageolet add_flageolet*/
scm_c_define_gsubr ("ToggleFlageolet", 0, 0, 0, scheme_ToggleFlageolet);
/*ToggleOpen add_open*/
scm_c_define_gsubr ("ToggleOpen", 0, 0, 0, scheme_ToggleOpen);
/*TogglePrallMordent add_prallmordent*/
scm_c_define_gsubr ("TogglePrallMordent", 0, 0, 0, scheme_TogglePrallMordent);
/*TogglePrallPrall add_prallprall*/
scm_c_define_gsubr ("TogglePrallPrall", 0, 0, 0, scheme_TogglePrallPrall);
/*TogglePrall add_prall*/
scm_c_define_gsubr ("TogglePrall", 0, 0, 0, scheme_TogglePrall);
/*ToggleReverseTurn add_reverseturn*/
scm_c_define_gsubr ("ToggleReverseTurn", 0, 0, 0, scheme_ToggleReverseTurn);
/*ToggleSegno add_segno*/
scm_c_define_gsubr ("ToggleSegno", 0, 0, 0, scheme_ToggleSegno);
/*ToggleSforzato add_sforzato*/
scm_c_define_gsubr ("ToggleSforzato", 0, 0, 0, scheme_ToggleSforzato);
/*ToggleStopped add_stopped*/
scm_c_define_gsubr ("ToggleStopped", 0, 0, 0, scheme_ToggleStopped);
/*ToggleThumb add_thumb*/
scm_c_define_gsubr ("ToggleThumb", 0, 0, 0, scheme_ToggleThumb);
/*ToggleUpprall add_upprall*/
scm_c_define_gsubr ("ToggleUpprall", 0, 0, 0, scheme_ToggleUpprall);
/*ToggleArpeggio add_arpeggio*/
scm_c_define_gsubr ("ToggleArpeggio", 0, 0, 0, scheme_ToggleArpeggio);
/*SetGrace set_grace*/
scm_c_define_gsubr ("SetGrace", 0, 0, 0, scheme_SetGrace);
/*ForceCaution force_cautionary*/
scm_c_define_gsubr ("ForceCaution", 0, 0, 0, scheme_ForceCaution);
/*ChangePitch change_pitch*/
scm_c_define_gsubr ("ChangePitch", 0, 0, 0, scheme_ChangePitch);
/*DoubleBar insert_doublebar*/
scm_c_define_gsubr ("DoubleBar", 0, 0, 0, scheme_DoubleBar);
/*EndBar insert_endbar*/
scm_c_define_gsubr ("EndBar", 0, 0, 0, scheme_EndBar);
/*OpenRepeat insert_openrepeat*/
scm_c_define_gsubr ("OpenRepeat", 0, 0, 0, scheme_OpenRepeat);
/*CloseRepeat insert_closerepeat*/
scm_c_define_gsubr ("CloseRepeat", 0, 0, 0, scheme_CloseRepeat);
/*OpenCloseRepeat insert_opencloserepeat*/
scm_c_define_gsubr ("OpenCloseRepeat", 0, 0, 0, scheme_OpenCloseRepeat);
/*InsertRhythm insert_rhythm_pattern*/
scm_c_define_gsubr ("InsertRhythm", 0, 0, 0, scheme_InsertRhythm);
/*NextRhythm nextrhythm*/
scm_c_define_gsubr ("NextRhythm", 0, 0, 0, scheme_NextRhythm);
/*AppendMesauresToScore append_measure_score*/
scm_c_define_gsubr ("AppendMesauresToScore", 0, 0, 0, scheme_AppendMesauresToScore);
/*New file_newwrapper*/
scm_c_define_gsubr ("New", 0, 0, 0, scheme_New);
/*Open file_open_with_check*/
scm_c_define_gsubr ("Open", 0, 0, 0, scheme_Open);
/*AddStaffs file_add_staffs*/
scm_c_define_gsubr ("AddStaffs", 0, 0, 0, scheme_AddStaffs);
/*AddMovements file_add_movements*/
scm_c_define_gsubr ("AddMovements", 0, 0, 0, scheme_AddMovements);
/*MovementProps movement_props_dialog*/
scm_c_define_gsubr ("MovementProps", 0, 0, 0, scheme_MovementProps);
/*OpenNewWindow openinnew*/
scm_c_define_gsubr ("OpenNewWindow", 0, 0, 0, scheme_OpenNewWindow);
/*Save file_savewrapper*/
scm_c_define_gsubr ("Save", 0, 0, 0, scheme_Save);
/*SaveAs file_saveaswrapper*/
scm_c_define_gsubr ("SaveAs", 0, 0, 0, scheme_SaveAs);
/*OpenTemplate system_template_open_with_check*/
scm_c_define_gsubr ("OpenTemplate", 0, 0, 0, scheme_OpenTemplate);
/*OpenExample system_example_open_with_check*/
scm_c_define_gsubr ("OpenExample", 0, 0, 0, scheme_OpenExample);
/*OpenMyTemplate local_template_open_with_check*/
scm_c_define_gsubr ("OpenMyTemplate", 0, 0, 0, scheme_OpenMyTemplate);
/*SaveTemplate template_save*/
scm_c_define_gsubr ("SaveTemplate", 0, 0, 0, scheme_SaveTemplate);
/*NewWindow newview*/
scm_c_define_gsubr ("NewWindow", 0, 0, 0, scheme_NewWindow);
/*InsertMovementBefore insert_movement_before*/
scm_c_define_gsubr ("InsertMovementBefore", 0, 0, 0, scheme_InsertMovementBefore);
/*InsertMovementAfter insert_movement_after*/
scm_c_define_gsubr ("InsertMovementAfter", 0, 0, 0, scheme_InsertMovementAfter);
/*SaveParts file_savepartswrapper*/
scm_c_define_gsubr ("SaveParts", 0, 0, 0, scheme_SaveParts);
/*ExportPDF export_pdf_action*/
scm_c_define_gsubr ("ExportPDF", 0, 0, 0, scheme_ExportPDF);
/*ConfigureScore scorewizard*/
scm_c_define_gsubr ("ConfigureScore", 0, 0, 0, scheme_ConfigureScore);
/*PrintPreview printpreview_cb*/
scm_c_define_gsubr ("PrintPreview", 0, 0, 0, scheme_PrintPreview);
/*PrintExcerptPreview printexcerptpreview_cb*/
scm_c_define_gsubr ("PrintExcerptPreview", 0, 0, 0, scheme_PrintExcerptPreview);
/*Print printall_cb*/
scm_c_define_gsubr ("Print", 0, 0, 0, scheme_Print);
/*PrintPart printpart_cb*/
scm_c_define_gsubr ("PrintPart", 0, 0, 0, scheme_PrintPart);
/*Close close_gui_with_check*/
scm_c_define_gsubr ("Close", 0, 0, 0, scheme_Close);
/*Quit closewrapper*/
scm_c_define_gsubr ("Quit", 0, 0, 0, scheme_Quit);
/*Undo undowrapper*/
scm_c_define_gsubr ("Undo", 0, 0, 0, scheme_Undo);
/*Redo redowrapper*/
scm_c_define_gsubr ("Redo", 0, 0, 0, scheme_Redo);
/*Copy copywrapper*/
scm_c_define_gsubr ("Copy", 0, 0, 0, scheme_Copy);
/*Cut cutwrapper*/
scm_c_define_gsubr ("Cut", 0, 0, 0, scheme_Cut);
/*Paste pastewrapper*/
scm_c_define_gsubr ("Paste", 0, 0, 0, scheme_Paste);
/*ScoreProperties score_properties_dialog*/
scm_c_define_gsubr ("ScoreProperties", 0, 0, 0, scheme_ScoreProperties);
/*SaveSelection saveselwrapper*/
scm_c_define_gsubr ("SaveSelection", 0, 0, 0, scheme_SaveSelection);
/*Preferences preferences_change*/
scm_c_define_gsubr ("Preferences", 0, 0, 0, scheme_Preferences);
/*SaveAccels save_default_keymap_file_wrapper*/
scm_c_define_gsubr ("SaveAccels", 0, 0, 0, scheme_SaveAccels);
/*Keyboard configure_keyboard_dialog*/
scm_c_define_gsubr ("Keyboard", 0, 0, 0, scheme_Keyboard);
/*LoadPlugins load_plugin*/
scm_c_define_gsubr ("LoadPlugins", 0, 0, 0, scheme_LoadPlugins);
/*UnloadPlugins unloadplugins*/
scm_c_define_gsubr ("UnloadPlugins", 0, 0, 0, scheme_UnloadPlugins);
/*ListPlugins list_loaded_plugins*/
scm_c_define_gsubr ("ListPlugins", 0, 0, 0, scheme_ListPlugins);
/*ListAvailablePlugins list_available_plugins*/
scm_c_define_gsubr ("ListAvailablePlugins", 0, 0, 0, scheme_ListAvailablePlugins);
/*SwapStaffs swapstaffs*/
scm_c_define_gsubr ("SwapStaffs", 0, 0, 0, scheme_SwapStaffs);
/*SplitVoices splitstaffs*/
scm_c_define_gsubr ("SplitVoices", 0, 0, 0, scheme_SplitVoices);
/*JoinVoices joinstaffs*/
scm_c_define_gsubr ("JoinVoices", 0, 0, 0, scheme_JoinVoices);
/*SwapMovements swapmovements*/
scm_c_define_gsubr ("SwapMovements", 0, 0, 0, scheme_SwapMovements);
/*VoiceUp voiceup*/
scm_c_define_gsubr ("VoiceUp", 0, 0, 0, scheme_VoiceUp);
/*VoiceDown voicedown*/
scm_c_define_gsubr ("VoiceDown", 0, 0, 0, scheme_VoiceDown);
/*AddBefore newstaffbefore*/
scm_c_define_gsubr ("AddBefore", 0, 0, 0, scheme_AddBefore);
/*AddAfter dnm_newstaffafter*/
scm_c_define_gsubr ("AddAfter", 0, 0, 0, scheme_AddAfter);
/*AddInitial newstaffinitial*/
scm_c_define_gsubr ("AddInitial", 0, 0, 0, scheme_AddInitial);
/*AddLast newstafflast*/
scm_c_define_gsubr ("AddLast", 0, 0, 0, scheme_AddLast);
/*DeleteBefore delete_staff_before*/
scm_c_define_gsubr ("DeleteBefore", 0, 0, 0, scheme_DeleteBefore);
/*DeleteStaff delete_staff_current*/
scm_c_define_gsubr ("DeleteStaff", 0, 0, 0, scheme_DeleteStaff);
/*DeleteAfter delete_staff_after*/
scm_c_define_gsubr ("DeleteAfter", 0, 0, 0, scheme_DeleteAfter);
/*AddVoice dnm_newstaffvoice*/
scm_c_define_gsubr ("AddVoice", 0, 0, 0, scheme_AddVoice);
/*TransposeStaff staff_transposition*/
scm_c_define_gsubr ("TransposeStaff", 0, 0, 0, scheme_TransposeStaff);
/*StaffProperties staff_properties_change_cb*/
scm_c_define_gsubr ("StaffProperties", 0, 0, 0, scheme_StaffProperties);
/*InitialClef clef_change_initial*/
scm_c_define_gsubr ("InitialClef", 0, 0, 0, scheme_InitialClef);
/*InsertClef clef_change_insert*/
scm_c_define_gsubr ("InsertClef", 0, 0, 0, scheme_InsertClef);
/*InitialKey key_change_initial*/
scm_c_define_gsubr ("InitialKey", 0, 0, 0, scheme_InitialKey);
/*InsertKey key_change_insert*/
scm_c_define_gsubr ("InsertKey", 0, 0, 0, scheme_InsertKey);
/*InitialTimeSig timesig_change_initial*/
scm_c_define_gsubr ("InitialTimeSig", 0, 0, 0, scheme_InitialTimeSig);
/*InsertTimeSig timesig_change_insert*/
scm_c_define_gsubr ("InsertTimeSig", 0, 0, 0, scheme_InsertTimeSig);
/*ChangeNotehead set_notehead*/
scm_c_define_gsubr ("ChangeNotehead", 0, 0, 0, scheme_ChangeNotehead);
/*InsertStem stem_directive_insert*/
scm_c_define_gsubr ("InsertStem", 0, 0, 0, scheme_InsertStem);
/*EditLyric lyric_insert*/
scm_c_define_gsubr ("EditLyric", 0, 0, 0, scheme_EditLyric);
/*EditFiguredBass figure_insert*/
scm_c_define_gsubr ("EditFiguredBass", 0, 0, 0, scheme_EditFiguredBass);
/*EditChords fakechord_insert*/
scm_c_define_gsubr ("EditChords", 0, 0, 0, scheme_EditChords);
/*InsertDynamic insert_dynamic*/
scm_c_define_gsubr ("InsertDynamic", 0, 0, 0, scheme_InsertDynamic);
/*InsertLilyDirective lily_directive_insert*/
scm_c_define_gsubr ("InsertLilyDirective", 0, 0, 0, scheme_InsertLilyDirective);
/*InsertLilyPostfix lily_directive_postfix*/
scm_c_define_gsubr ("InsertLilyPostfix", 0, 0, 0, scheme_InsertLilyPostfix);
/*InsertBarline insert_barline*/
scm_c_define_gsubr ("InsertBarline", 0, 0, 0, scheme_InsertBarline);
/*GoToMeasure tomeasurenum*/
scm_c_define_gsubr ("GoToMeasure", 0, 0, 0, scheme_GoToMeasure);
/*GoToBeginning tohome*/
scm_c_define_gsubr ("GoToBeginning", 0, 0, 0, scheme_GoToBeginning);
/*GoToEnd toend*/
scm_c_define_gsubr ("GoToEnd", 0, 0, 0, scheme_GoToEnd);
/*NextMovement next_movement*/
scm_c_define_gsubr ("NextMovement", 0, 0, 0, scheme_NextMovement);
/*PreviousMovement prev_movement*/
scm_c_define_gsubr ("PreviousMovement", 0, 0, 0, scheme_PreviousMovement);
/*DeleteMovement delete_movement*/
scm_c_define_gsubr ("DeleteMovement", 0, 0, 0, scheme_DeleteMovement);
/*DeleteBookmarks deletebookmarks*/
scm_c_define_gsubr ("DeleteBookmarks", 0, 0, 0, scheme_DeleteBookmarks);
/*Play ext_midi_playback*/
scm_c_define_gsubr ("Play", 0, 0, 0, scheme_Play);
/*Stop stop_midi_playback*/
scm_c_define_gsubr ("Stop", 0, 0, 0, scheme_Stop);
/*PlayCSound dnm_csoundplayback*/
scm_c_define_gsubr ("PlayCSound", 0, 0, 0, scheme_PlayCSound);
/*PlaybackProperties playback_properties_change*/
scm_c_define_gsubr ("PlaybackProperties", 0, 0, 0, scheme_PlaybackProperties);
/*Help browse_manual*/
scm_c_define_gsubr ("Help", 0, 0, 0, scheme_Help);
/*About about*/
scm_c_define_gsubr ("About", 0, 0, 0, scheme_About);
/*AddBookmark addbookmark*/
scm_c_define_gsubr ("AddBookmark", 0, 0, 0, scheme_AddBookmark);
/*GotoBookmark gotobookmark*/
scm_c_define_gsubr ("GotoBookmark", 0, 0, 0, scheme_GotoBookmark);
/*NextBookmark nextbookmark*/
scm_c_define_gsubr ("NextBookmark", 0, 0, 0, scheme_NextBookmark);
/*PrevBookmark prevbookmark*/
scm_c_define_gsubr ("PrevBookmark", 0, 0, 0, scheme_PrevBookmark);
/*ToggleEdit toggle_edit_mode*/
scm_c_define_gsubr ("ToggleEdit", 0, 0, 0, scheme_ToggleEdit);
/*ToggleRest toggle_rest_mode*/
scm_c_define_gsubr ("ToggleRest", 0, 0, 0, scheme_ToggleRest);
/*ToggleRhythm toggle_rhythm_mode*/
scm_c_define_gsubr ("ToggleRhythm", 0, 0, 0, scheme_ToggleRhythm);
/*ClearOverlay clear_overlay*/
scm_c_define_gsubr ("ClearOverlay", 0, 0, 0, scheme_ClearOverlay);
/*CreateRhythm create_rhythm_cb*/
scm_c_define_gsubr ("CreateRhythm", 0, 0, 0, scheme_CreateRhythm);
/*DeleteRhythm delete_rhythm_cb*/
scm_c_define_gsubr ("DeleteRhythm", 0, 0, 0, scheme_DeleteRhythm);
