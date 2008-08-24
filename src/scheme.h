/*CursorLeft cursorleft*/
install_scm_function ("CursorLeft", scheme_CursorLeft);
/*CursorDown cursordown*/
install_scm_function ("CursorDown", scheme_CursorDown);
/*CursorUp cursorup*/
install_scm_function ("CursorUp", scheme_CursorUp);
/*CursorRight cursorright*/
install_scm_function ("CursorRight", scheme_CursorRight);
/*StaffUp staffup*/
install_scm_function ("StaffUp", scheme_StaffUp);
/*StaffDown staffdown*/
install_scm_function ("StaffDown", scheme_StaffDown);
/*MeasureLeft measureleft*/
install_scm_function ("MeasureLeft", scheme_MeasureLeft);
/*MeasureRight measureright*/
install_scm_function ("MeasureRight", scheme_MeasureRight);
/*A go_to_A_key*/
install_scm_function ("A", scheme_A);
/*B go_to_B_key*/
install_scm_function ("B", scheme_B);
/*C go_to_C_key*/
install_scm_function ("C", scheme_C);
/*D go_to_D_key*/
install_scm_function ("D", scheme_D);
/*E go_to_E_key*/
install_scm_function ("E", scheme_E);
/*F go_to_F_key*/
install_scm_function ("F", scheme_F);
/*G go_to_G_key*/
install_scm_function ("G", scheme_G);
/*OctaveUp octave_up_key*/
install_scm_function ("OctaveUp", scheme_OctaveUp);
/*OctaveDown octave_down_key*/
install_scm_function ("OctaveDown", scheme_OctaveDown);
/*WholeNote insert_chord_0key*/
install_scm_function ("WholeNote", scheme_WholeNote);
/*HalfNote insert_chord_1key*/
install_scm_function ("HalfNote", scheme_HalfNote);
/*QuarterNote insert_chord_2key*/
install_scm_function ("QuarterNote", scheme_QuarterNote);
/*EighthNote insert_chord_3key*/
install_scm_function ("EighthNote", scheme_EighthNote);
/*SixteenthNote insert_chord_4key*/
install_scm_function ("SixteenthNote", scheme_SixteenthNote);
/*ThirtysecondNote insert_chord_5key*/
install_scm_function ("ThirtysecondNote", scheme_ThirtysecondNote);
/*SixtyfourthNote insert_chord_6key*/
install_scm_function ("SixtyfourthNote", scheme_SixtyfourthNote);
/*InsertBlankWholeNote insert_blankchord_0key*/
install_scm_function ("InsertBlankWholeNote", scheme_InsertBlankWholeNote);
/*InsertBlankHalfNote insert_blankchord_1key*/
install_scm_function ("InsertBlankHalfNote", scheme_InsertBlankHalfNote);
/*InsertBlankQuarterNote insert_blankchord_2key*/
install_scm_function ("InsertBlankQuarterNote", scheme_InsertBlankQuarterNote);
/*InsertBlankEighthNote insert_blankchord_3key*/
install_scm_function ("InsertBlankEighthNote", scheme_InsertBlankEighthNote);
/*InsertBlankSixteenthNote insert_blankchord_4key*/
install_scm_function ("InsertBlankSixteenthNote", scheme_InsertBlankSixteenthNote);
/*InsertBlankThirtysecondNote insert_blankchord_5key*/
install_scm_function ("InsertBlankThirtysecondNote", scheme_InsertBlankThirtysecondNote);
/*InsertBlankSixtyfourthNote insert_blankchord_6key*/
install_scm_function ("InsertBlankSixtyfourthNote", scheme_InsertBlankSixtyfourthNote);
/*ToggleRestMode rest_toggle_key*/
install_scm_function ("ToggleRestMode", scheme_ToggleRestMode);
/*ToggleBlankMode toggle_blank*/
install_scm_function ("ToggleBlankMode", scheme_ToggleBlankMode);
/*InsertWholeRest insert_rest_0key*/
install_scm_function ("InsertWholeRest", scheme_InsertWholeRest);
/*InsertHalfRest insert_rest_1key*/
install_scm_function ("InsertHalfRest", scheme_InsertHalfRest);
/*InsertQuarterRest insert_rest_2key*/
install_scm_function ("InsertQuarterRest", scheme_InsertQuarterRest);
/*InsertEighthRest insert_rest_3key*/
install_scm_function ("InsertEighthRest", scheme_InsertEighthRest);
/*InsertSixteenthRest insert_rest_4key*/
install_scm_function ("InsertSixteenthRest", scheme_InsertSixteenthRest);
/*InsertThirtysecondRest insert_rest_5key*/
install_scm_function ("InsertThirtysecondRest", scheme_InsertThirtysecondRest);
/*InsertSixtyfourthRest insert_rest_6key*/
install_scm_function ("InsertSixtyfourthRest", scheme_InsertSixtyfourthRest);
/*InsertDuplet insert_duplet*/
install_scm_function ("InsertDuplet", scheme_InsertDuplet);
/*InsertTriplet insert_triplet*/
install_scm_function ("InsertTriplet", scheme_InsertTriplet);
/*StartTriplet start_triplet*/
install_scm_function ("StartTriplet", scheme_StartTriplet);
/*EndTuplet end_tuplet*/
install_scm_function ("EndTuplet", scheme_EndTuplet);
/*InsertQuadtuplet insert_quadtuplet*/
install_scm_function ("InsertQuadtuplet", scheme_InsertQuadtuplet);
/*InsertQuintuplet insert_quintuplet*/
install_scm_function ("InsertQuintuplet", scheme_InsertQuintuplet);
/*InsertSextuplet insert_sextuplet*/
install_scm_function ("InsertSextuplet", scheme_InsertSextuplet);
/*InsertSeptuplet insert_septuplet*/
install_scm_function ("InsertSeptuplet", scheme_InsertSeptuplet);
/*AddTone add_tone_key*/
install_scm_function ("AddTone", scheme_AddTone);
/*RemoveTone remove_tone_key*/
install_scm_function ("RemoveTone", scheme_RemoveTone);
/*SharpenOrStemDown sharpen_key*/
install_scm_function ("SharpenOrStemDown", scheme_SharpenOrStemDown);
/*FlattenOrStemUp flatten_key*/
install_scm_function ("FlattenOrStemUp", scheme_FlattenOrStemUp);
/*AddDot add_dot_key*/
install_scm_function ("AddDot", scheme_AddDot);
/*RemoveDot remove_dot_key*/
install_scm_function ("RemoveDot", scheme_RemoveDot);
/*InsertTiedNote tie_notes_key*/
install_scm_function ("InsertTiedNote", scheme_InsertTiedNote);
/*DeleteObject deleteobject*/
install_scm_function ("DeleteObject", scheme_DeleteObject);
/*DeletePreviousObject deletepreviousobject*/
install_scm_function ("DeletePreviousObject", scheme_DeletePreviousObject);
/*InsertMeasure insert_measure_key*/
install_scm_function ("InsertMeasure", scheme_InsertMeasure);
/*AppendMeasure append_measure_key*/
install_scm_function ("AppendMeasure", scheme_AppendMeasure);
/*DeleteMeasure deletemeasure*/
install_scm_function ("DeleteMeasure", scheme_DeleteMeasure);
/*DeleteMeasureAllStaffs deletemeasureallstaffs*/
install_scm_function ("DeleteMeasureAllStaffs", scheme_DeleteMeasureAllStaffs);
/*ShrinkMeasures adjust_measure_less_width_key*/
install_scm_function ("ShrinkMeasures", scheme_ShrinkMeasures);
/*WidenMeasures adjust_measure_more_width_key*/
install_scm_function ("WidenMeasures", scheme_WidenMeasures);
/*ShorterStaffs adjust_staff_less_height_key*/
install_scm_function ("ShorterStaffs", scheme_ShorterStaffs);
/*TallerStaffs adjust_staff_more_height_key*/
install_scm_function ("TallerStaffs", scheme_TallerStaffs);
/*InsertTrebleClef newcleftreble*/
install_scm_function ("InsertTrebleClef", scheme_InsertTrebleClef);
/*InsertBassClef newclefbass*/
install_scm_function ("InsertBassClef", scheme_InsertBassClef);
/*Insertg8clef newclefg8*/
install_scm_function ("Insertg8clef", scheme_Insertg8clef);
/*InsertAltoClef newclefalto*/
install_scm_function ("InsertAltoClef", scheme_InsertAltoClef);
/*InsertTenorClef newcleftenor*/
install_scm_function ("InsertTenorClef", scheme_InsertTenorClef);
/*InsertSopranoClef newclefsoprano*/
install_scm_function ("InsertSopranoClef", scheme_InsertSopranoClef);
/*SetInitialTrebleClef setcleftreble*/
install_scm_function ("SetInitialTrebleClef", scheme_SetInitialTrebleClef);
/*SetInitialBassClef setclefbass*/
install_scm_function ("SetInitialBassClef", scheme_SetInitialBassClef);
/*SetInitialg8clef setclefg8*/
install_scm_function ("SetInitialg8clef", scheme_SetInitialg8clef);
/*SetInitialAltoClef setclefalto*/
install_scm_function ("SetInitialAltoClef", scheme_SetInitialAltoClef);
/*SetInitialTenorClef setcleftenor*/
install_scm_function ("SetInitialTenorClef", scheme_SetInitialTenorClef);
/*SetInitialSopranoClef setclefsoprano*/
install_scm_function ("SetInitialSopranoClef", scheme_SetInitialSopranoClef);
/*Insert22Time newtimesig22*/
install_scm_function ("Insert22Time", scheme_Insert22Time);
/*Insert32Time newtimesig32*/
install_scm_function ("Insert32Time", scheme_Insert32Time);
/*Insert42Time newtimesig42*/
install_scm_function ("Insert42Time", scheme_Insert42Time);
/*Insert44Time newtimesig44*/
install_scm_function ("Insert44Time", scheme_Insert44Time);
/*Insert34Time newtimesig34*/
install_scm_function ("Insert34Time", scheme_Insert34Time);
/*Insert24Time newtimesig24*/
install_scm_function ("Insert24Time", scheme_Insert24Time);
/*Insert64Time newtimesig64*/
install_scm_function ("Insert64Time", scheme_Insert64Time);
/*Insert38Time newtimesig38*/
install_scm_function ("Insert38Time", scheme_Insert38Time);
/*Insert68Time newtimesig68*/
install_scm_function ("Insert68Time", scheme_Insert68Time);
/*Insert128Time newtimesig128*/
install_scm_function ("Insert128Time", scheme_Insert128Time);
/*Insert98Time newtimesig98*/
install_scm_function ("Insert98Time", scheme_Insert98Time);
/*Set22Time settimesig22*/
install_scm_function ("Set22Time", scheme_Set22Time);
/*Set32Time settimesig32*/
install_scm_function ("Set32Time", scheme_Set32Time);
/*Set42Time settimesig42*/
install_scm_function ("Set42Time", scheme_Set42Time);
/*Set44Time settimesig44*/
install_scm_function ("Set44Time", scheme_Set44Time);
/*Set34Time settimesig34*/
install_scm_function ("Set34Time", scheme_Set34Time);
/*Set24Time settimesig24*/
install_scm_function ("Set24Time", scheme_Set24Time);
/*Set64Time settimesig64*/
install_scm_function ("Set64Time", scheme_Set64Time);
/*Set38Time settimesig38*/
install_scm_function ("Set38Time", scheme_Set38Time);
/*Set68Time settimesig68*/
install_scm_function ("Set68Time", scheme_Set68Time);
/*Set128Time settimesig128*/
install_scm_function ("Set128Time", scheme_Set128Time);
/*Set98Time settimesig98*/
install_scm_function ("Set98Time", scheme_Set98Time);
/*InsertCmaj newkeysigcmaj*/
install_scm_function ("InsertCmaj", scheme_InsertCmaj);
/*InsertGmaj newkeysiggmaj*/
install_scm_function ("InsertGmaj", scheme_InsertGmaj);
/*InsertDmaj newkeysigdmaj*/
install_scm_function ("InsertDmaj", scheme_InsertDmaj);
/*InsertAmaj newkeysigamaj*/
install_scm_function ("InsertAmaj", scheme_InsertAmaj);
/*InsertEmaj newkeysigemaj*/
install_scm_function ("InsertEmaj", scheme_InsertEmaj);
/*InsertBmaj newkeysigbmaj*/
install_scm_function ("InsertBmaj", scheme_InsertBmaj);
/*InsertFSharpmaj newkeysigfsharpmaj*/
install_scm_function ("InsertFSharpmaj", scheme_InsertFSharpmaj);
/*InsertCSharpmaj newkeysigcsharpmaj*/
install_scm_function ("InsertCSharpmaj", scheme_InsertCSharpmaj);
/*InsertFmaj newkeysigfmaj*/
install_scm_function ("InsertFmaj", scheme_InsertFmaj);
/*InsertBflatmaj newkeysigbflatmaj*/
install_scm_function ("InsertBflatmaj", scheme_InsertBflatmaj);
/*InsertEflatmaj newkeysigeflatmaj*/
install_scm_function ("InsertEflatmaj", scheme_InsertEflatmaj);
/*InsertAflatmaj newkeysigaflatmaj*/
install_scm_function ("InsertAflatmaj", scheme_InsertAflatmaj);
/*InsertDflatmaj newkeysigdflatmaj*/
install_scm_function ("InsertDflatmaj", scheme_InsertDflatmaj);
/*InsertGflatmaj newkeysiggflatmaj*/
install_scm_function ("InsertGflatmaj", scheme_InsertGflatmaj);
/*InsertCflatmaj newkeysigcflatmaj*/
install_scm_function ("InsertCflatmaj", scheme_InsertCflatmaj);
/*InsertAmin newkeysigamin*/
install_scm_function ("InsertAmin", scheme_InsertAmin);
/*InsertEmin newkeysigemin*/
install_scm_function ("InsertEmin", scheme_InsertEmin);
/*InsertBmin newkeysigbmin*/
install_scm_function ("InsertBmin", scheme_InsertBmin);
/*InsertFSharpmin newkeysigfsharpmin*/
install_scm_function ("InsertFSharpmin", scheme_InsertFSharpmin);
/*InsertCSharpmin newkeysigcsharpmin*/
install_scm_function ("InsertCSharpmin", scheme_InsertCSharpmin);
/*InsertGSharpmin newkeysiggsharpmin*/
install_scm_function ("InsertGSharpmin", scheme_InsertGSharpmin);
/*InsertDSharpmin newkeysigdsharpmin*/
install_scm_function ("InsertDSharpmin", scheme_InsertDSharpmin);
/*InsertASharpmin newkeysigasharpmin*/
install_scm_function ("InsertASharpmin", scheme_InsertASharpmin);
/*InsertDmin newkeysigdmin*/
install_scm_function ("InsertDmin", scheme_InsertDmin);
/*InsertGmin newkeysiggmin*/
install_scm_function ("InsertGmin", scheme_InsertGmin);
/*InsertCmin newkeysigcmin*/
install_scm_function ("InsertCmin", scheme_InsertCmin);
/*InsertFmin newkeysigfmin*/
install_scm_function ("InsertFmin", scheme_InsertFmin);
/*InsertBflatmin newkeysigbflatmin*/
install_scm_function ("InsertBflatmin", scheme_InsertBflatmin);
/*InsertEflatmin newkeysigeflatmin*/
install_scm_function ("InsertEflatmin", scheme_InsertEflatmin);
/*InsertAflatmin newkeysigaflatmin*/
install_scm_function ("InsertAflatmin", scheme_InsertAflatmin);
/*SetInitialCmaj setkeysigcmaj*/
install_scm_function ("SetInitialCmaj", scheme_SetInitialCmaj);
/*SetInitialGmaj setkeysiggmaj*/
install_scm_function ("SetInitialGmaj", scheme_SetInitialGmaj);
/*SetInitialDmaj setkeysigdmaj*/
install_scm_function ("SetInitialDmaj", scheme_SetInitialDmaj);
/*SetInitialAmaj setkeysigamaj*/
install_scm_function ("SetInitialAmaj", scheme_SetInitialAmaj);
/*SetInitialEmaj setkeysigemaj*/
install_scm_function ("SetInitialEmaj", scheme_SetInitialEmaj);
/*SetInitialBmaj setkeysigbmaj*/
install_scm_function ("SetInitialBmaj", scheme_SetInitialBmaj);
/*SetInitialFSharpmaj setkeysigfsharpmaj*/
install_scm_function ("SetInitialFSharpmaj", scheme_SetInitialFSharpmaj);
/*SetInitialCSharpmaj setkeysigcsharpmaj*/
install_scm_function ("SetInitialCSharpmaj", scheme_SetInitialCSharpmaj);
/*SetInitialFmaj setkeysigfmaj*/
install_scm_function ("SetInitialFmaj", scheme_SetInitialFmaj);
/*SetInitialBflatmaj setkeysigbflatmaj*/
install_scm_function ("SetInitialBflatmaj", scheme_SetInitialBflatmaj);
/*SetInitialEflatmaj setkeysigeflatmaj*/
install_scm_function ("SetInitialEflatmaj", scheme_SetInitialEflatmaj);
/*SetInitialAflatmaj setkeysigaflatmaj*/
install_scm_function ("SetInitialAflatmaj", scheme_SetInitialAflatmaj);
/*SetInitialDflatmaj setkeysigdflatmaj*/
install_scm_function ("SetInitialDflatmaj", scheme_SetInitialDflatmaj);
/*SetInitialGflatmaj setkeysiggflatmaj*/
install_scm_function ("SetInitialGflatmaj", scheme_SetInitialGflatmaj);
/*SetInitialCflatmaj setkeysigcflatmaj*/
install_scm_function ("SetInitialCflatmaj", scheme_SetInitialCflatmaj);
/*SetInitialAmin setkeysigamin*/
install_scm_function ("SetInitialAmin", scheme_SetInitialAmin);
/*SetInitialEmin setkeysigemin*/
install_scm_function ("SetInitialEmin", scheme_SetInitialEmin);
/*SetInitialBmin setkeysigbmin*/
install_scm_function ("SetInitialBmin", scheme_SetInitialBmin);
/*SetInitialFSharpmin setkeysigfsharpmin*/
install_scm_function ("SetInitialFSharpmin", scheme_SetInitialFSharpmin);
/*SetInitialCSharpmin setkeysigcsharpmin*/
install_scm_function ("SetInitialCSharpmin", scheme_SetInitialCSharpmin);
/*SetInitialGSharpmin setkeysiggsharpmin*/
install_scm_function ("SetInitialGSharpmin", scheme_SetInitialGSharpmin);
/*SetInitialDSharpmin setkeysigdsharpmin*/
install_scm_function ("SetInitialDSharpmin", scheme_SetInitialDSharpmin);
/*SetInitialASharpmin setkeysigasharpmin*/
install_scm_function ("SetInitialASharpmin", scheme_SetInitialASharpmin);
/*SetInitialDmin setkeysigdmin*/
install_scm_function ("SetInitialDmin", scheme_SetInitialDmin);
/*SetInitialGmin setkeysiggmin*/
install_scm_function ("SetInitialGmin", scheme_SetInitialGmin);
/*SetInitialCmin setkeysigcmin*/
install_scm_function ("SetInitialCmin", scheme_SetInitialCmin);
/*SetInitialFmin setkeysigfmin*/
install_scm_function ("SetInitialFmin", scheme_SetInitialFmin);
/*SetInitialBflatmin setkeysigbflatmin*/
install_scm_function ("SetInitialBflatmin", scheme_SetInitialBflatmin);
/*SetInitialEflatmin setkeysigeflatmin*/
install_scm_function ("SetInitialEflatmin", scheme_SetInitialEflatmin);
/*SetInitialAflatmin setkeysigaflatmin*/
install_scm_function ("SetInitialAflatmin", scheme_SetInitialAflatmin);
/*SetMark set_mark*/
install_scm_function ("SetMark", scheme_SetMark);
/*UnsetMark unset_mark*/
install_scm_function ("UnsetMark", scheme_UnsetMark);
/*ToggleBeginSlur toggle_begin_slur*/
install_scm_function ("ToggleBeginSlur", scheme_ToggleBeginSlur);
/*ToggleEndSlur toggle_end_slur*/
install_scm_function ("ToggleEndSlur", scheme_ToggleEndSlur);
/*ToggleStartCrescendo toggle_start_crescendo*/
install_scm_function ("ToggleStartCrescendo", scheme_ToggleStartCrescendo);
/*ToggleEndCrescendo toggle_end_crescendo*/
install_scm_function ("ToggleEndCrescendo", scheme_ToggleEndCrescendo);
/*ToggleStartDiminuendo toggle_start_diminuendo*/
install_scm_function ("ToggleStartDiminuendo", scheme_ToggleStartDiminuendo);
/*ToggleEndDiminuendo toggle_end_diminuendo*/
install_scm_function ("ToggleEndDiminuendo", scheme_ToggleEndDiminuendo);
/*ToggleAccent add_accent*/
install_scm_function ("ToggleAccent", scheme_ToggleAccent);
/*ToggleFermata add_fermata*/
install_scm_function ("ToggleFermata", scheme_ToggleFermata);
/*ToggleStaccato add_staccato*/
install_scm_function ("ToggleStaccato", scheme_ToggleStaccato);
/*ToggleTenuto add_tenuto*/
install_scm_function ("ToggleTenuto", scheme_ToggleTenuto);
/*ToggleTrill add_trill*/
install_scm_function ("ToggleTrill", scheme_ToggleTrill);
/*ToggleTurn add_turn*/
install_scm_function ("ToggleTurn", scheme_ToggleTurn);
/*ToggleMordent add_mordent*/
install_scm_function ("ToggleMordent", scheme_ToggleMordent);
/*ToggleStaccatissimo add_staccatissimo*/
install_scm_function ("ToggleStaccatissimo", scheme_ToggleStaccatissimo);
/*ToggleCoda add_coda*/
install_scm_function ("ToggleCoda", scheme_ToggleCoda);
/*ToggleFlageolet add_flageolet*/
install_scm_function ("ToggleFlageolet", scheme_ToggleFlageolet);
/*ToggleOpen add_open*/
install_scm_function ("ToggleOpen", scheme_ToggleOpen);
/*TogglePrallMordent add_prallmordent*/
install_scm_function ("TogglePrallMordent", scheme_TogglePrallMordent);
/*TogglePrallPrall add_prallprall*/
install_scm_function ("TogglePrallPrall", scheme_TogglePrallPrall);
/*TogglePrall add_prall*/
install_scm_function ("TogglePrall", scheme_TogglePrall);
/*ToggleReverseTurn add_reverseturn*/
install_scm_function ("ToggleReverseTurn", scheme_ToggleReverseTurn);
/*ToggleSegno add_segno*/
install_scm_function ("ToggleSegno", scheme_ToggleSegno);
/*ToggleSforzato add_sforzato*/
install_scm_function ("ToggleSforzato", scheme_ToggleSforzato);
/*ToggleStopped add_stopped*/
install_scm_function ("ToggleStopped", scheme_ToggleStopped);
/*ToggleThumb add_thumb*/
install_scm_function ("ToggleThumb", scheme_ToggleThumb);
/*ToggleUpprall add_upprall*/
install_scm_function ("ToggleUpprall", scheme_ToggleUpprall);
/*ToggleArpeggio add_arpeggio*/
install_scm_function ("ToggleArpeggio", scheme_ToggleArpeggio);
/*SetGrace set_grace*/
install_scm_function ("SetGrace", scheme_SetGrace);
/*ForceCaution force_cautionary*/
install_scm_function ("ForceCaution", scheme_ForceCaution);
/*ChangePitch change_pitch*/
install_scm_function ("ChangePitch", scheme_ChangePitch);
/*DoubleBar insert_doublebar*/
install_scm_function ("DoubleBar", scheme_DoubleBar);
/*EndBar insert_endbar*/
install_scm_function ("EndBar", scheme_EndBar);
/*OpenRepeat insert_openrepeat*/
install_scm_function ("OpenRepeat", scheme_OpenRepeat);
/*CloseRepeat insert_closerepeat*/
install_scm_function ("CloseRepeat", scheme_CloseRepeat);
/*OpenCloseRepeat insert_opencloserepeat*/
install_scm_function ("OpenCloseRepeat", scheme_OpenCloseRepeat);
/*InsertRhythm insert_rhythm_pattern*/
install_scm_function ("InsertRhythm", scheme_InsertRhythm);
/*NextRhythm nextrhythm*/
install_scm_function ("NextRhythm", scheme_NextRhythm);
/*AppendMesauresToScore append_measure_score*/
install_scm_function ("AppendMesauresToScore", scheme_AppendMesauresToScore);
/*New file_newwrapper*/
install_scm_function ("New", scheme_New);
/*Open file_open_with_check*/
install_scm_function ("Open", scheme_Open);
/*AddStaffs file_add_staffs*/
install_scm_function ("AddStaffs", scheme_AddStaffs);
/*AddMovements file_add_movements*/
install_scm_function ("AddMovements", scheme_AddMovements);
/*MovementProps movement_props_dialog*/
install_scm_function ("MovementProps", scheme_MovementProps);
/*OpenNewWindow openinnew*/
install_scm_function ("OpenNewWindow", scheme_OpenNewWindow);
/*Save file_savewrapper*/
install_scm_function ("Save", scheme_Save);
/*SaveAs file_saveaswrapper*/
install_scm_function ("SaveAs", scheme_SaveAs);
/*OpenTemplate system_template_open_with_check*/
install_scm_function ("OpenTemplate", scheme_OpenTemplate);
/*OpenExample system_example_open_with_check*/
install_scm_function ("OpenExample", scheme_OpenExample);
/*OpenMyTemplate local_template_open_with_check*/
install_scm_function ("OpenMyTemplate", scheme_OpenMyTemplate);
/*SaveTemplate template_save*/
install_scm_function ("SaveTemplate", scheme_SaveTemplate);
/*NewWindow newview*/
install_scm_function ("NewWindow", scheme_NewWindow);
/*InsertMovementBefore insert_movement_before*/
install_scm_function ("InsertMovementBefore", scheme_InsertMovementBefore);
/*InsertMovementAfter insert_movement_after*/
install_scm_function ("InsertMovementAfter", scheme_InsertMovementAfter);
/*SaveParts file_savepartswrapper*/
install_scm_function ("SaveParts", scheme_SaveParts);
/*ExportPDF export_pdf_action*/
install_scm_function ("ExportPDF", scheme_ExportPDF);
/*ConfigureScore scorewizard*/
install_scm_function ("ConfigureScore", scheme_ConfigureScore);
/*PrintPreview printpreview_cb*/
install_scm_function ("PrintPreview", scheme_PrintPreview);
/*PrintExcerptPreview printexcerptpreview_cb*/
install_scm_function ("PrintExcerptPreview", scheme_PrintExcerptPreview);
/*Print printall_cb*/
install_scm_function ("Print", scheme_Print);
/*PrintPart printpart_cb*/
install_scm_function ("PrintPart", scheme_PrintPart);
/*Close close_gui_with_check*/
install_scm_function ("Close", scheme_Close);
/*Quit closewrapper*/
install_scm_function ("Quit", scheme_Quit);
/*Undo undowrapper*/
install_scm_function ("Undo", scheme_Undo);
/*Redo redowrapper*/
install_scm_function ("Redo", scheme_Redo);
/*Copy copywrapper*/
install_scm_function ("Copy", scheme_Copy);
/*Cut cutwrapper*/
install_scm_function ("Cut", scheme_Cut);
/*Paste pastewrapper*/
install_scm_function ("Paste", scheme_Paste);
/*ScoreProperties score_properties_dialog*/
install_scm_function ("ScoreProperties", scheme_ScoreProperties);
/*SaveSelection saveselwrapper*/
install_scm_function ("SaveSelection", scheme_SaveSelection);
/*Preferences preferences_change*/
install_scm_function ("Preferences", scheme_Preferences);
/*SaveAccels save_default_keymap_file_wrapper*/
install_scm_function ("SaveAccels", scheme_SaveAccels);
/*Keyboard configure_keyboard_dialog*/
install_scm_function ("Keyboard", scheme_Keyboard);
/*LoadPlugins load_plugin*/
install_scm_function ("LoadPlugins", scheme_LoadPlugins);
/*UnloadPlugins unloadplugins*/
install_scm_function ("UnloadPlugins", scheme_UnloadPlugins);
/*ListPlugins list_loaded_plugins*/
install_scm_function ("ListPlugins", scheme_ListPlugins);
/*ListAvailablePlugins list_available_plugins*/
install_scm_function ("ListAvailablePlugins", scheme_ListAvailablePlugins);
/*SwapStaffs swapstaffs*/
install_scm_function ("SwapStaffs", scheme_SwapStaffs);
/*SplitVoices splitstaffs*/
install_scm_function ("SplitVoices", scheme_SplitVoices);
/*JoinVoices joinstaffs*/
install_scm_function ("JoinVoices", scheme_JoinVoices);
/*SwapMovements swapmovements*/
install_scm_function ("SwapMovements", scheme_SwapMovements);
/*VoiceUp voiceup*/
install_scm_function ("VoiceUp", scheme_VoiceUp);
/*VoiceDown voicedown*/
install_scm_function ("VoiceDown", scheme_VoiceDown);
/*AddBefore newstaffbefore*/
install_scm_function ("AddBefore", scheme_AddBefore);
/*AddAfter dnm_newstaffafter*/
install_scm_function ("AddAfter", scheme_AddAfter);
/*AddInitial newstaffinitial*/
install_scm_function ("AddInitial", scheme_AddInitial);
/*AddLast newstafflast*/
install_scm_function ("AddLast", scheme_AddLast);
/*DeleteBefore delete_staff_before*/
install_scm_function ("DeleteBefore", scheme_DeleteBefore);
/*DeleteStaff delete_staff_current*/
install_scm_function ("DeleteStaff", scheme_DeleteStaff);
/*DeleteAfter delete_staff_after*/
install_scm_function ("DeleteAfter", scheme_DeleteAfter);
/*AddVoice dnm_newstaffvoice*/
install_scm_function ("AddVoice", scheme_AddVoice);
/*TransposeStaff staff_transposition*/
install_scm_function ("TransposeStaff", scheme_TransposeStaff);
/*StaffProperties staff_properties_change_cb*/
install_scm_function ("StaffProperties", scheme_StaffProperties);
/*InitialClef clef_change_initial*/
install_scm_function ("InitialClef", scheme_InitialClef);
/*InsertClef clef_change_insert*/
install_scm_function ("InsertClef", scheme_InsertClef);
/*InitialKey key_change_initial*/
install_scm_function ("InitialKey", scheme_InitialKey);
/*InsertKey key_change_insert*/
install_scm_function ("InsertKey", scheme_InsertKey);
/*InitialTimeSig timesig_change_initial*/
install_scm_function ("InitialTimeSig", scheme_InitialTimeSig);
/*InsertTimeSig timesig_change_insert*/
install_scm_function ("InsertTimeSig", scheme_InsertTimeSig);
/*ChangeNotehead set_notehead*/
install_scm_function ("ChangeNotehead", scheme_ChangeNotehead);
/*InsertStem stem_directive_insert*/
install_scm_function ("InsertStem", scheme_InsertStem);
/*EditLyric lyric_insert*/
install_scm_function ("EditLyric", scheme_EditLyric);
/*EditFiguredBass figure_insert*/
install_scm_function ("EditFiguredBass", scheme_EditFiguredBass);
/*EditChords fakechord_insert*/
install_scm_function ("EditChords", scheme_EditChords);
/*InsertDynamic insert_dynamic*/
install_scm_function ("InsertDynamic", scheme_InsertDynamic);
/*InsertLilyDirective lily_directive_insert*/
install_scm_function ("InsertLilyDirective", scheme_InsertLilyDirective);
/*InsertLilyPostfix lily_directive_postfix*/
install_scm_function ("InsertLilyPostfix", scheme_InsertLilyPostfix);
/*InsertBarline insert_barline*/
install_scm_function ("InsertBarline", scheme_InsertBarline);
/*GoToMeasure tomeasurenum*/
install_scm_function ("GoToMeasure", scheme_GoToMeasure);
/*GoToBeginning tohome*/
install_scm_function ("GoToBeginning", scheme_GoToBeginning);
/*GoToEnd toend*/
install_scm_function ("GoToEnd", scheme_GoToEnd);
/*NextMovement next_movement*/
install_scm_function ("NextMovement", scheme_NextMovement);
/*PreviousMovement prev_movement*/
install_scm_function ("PreviousMovement", scheme_PreviousMovement);
/*DeleteMovement delete_movement*/
install_scm_function ("DeleteMovement", scheme_DeleteMovement);
/*DeleteBookmarks deletebookmarks*/
install_scm_function ("DeleteBookmarks", scheme_DeleteBookmarks);
/*Play ext_midi_playback*/
install_scm_function ("Play", scheme_Play);
/*Stop stop_midi_playback*/
install_scm_function ("Stop", scheme_Stop);
/*PlayCSound dnm_csoundplayback*/
install_scm_function ("PlayCSound", scheme_PlayCSound);
/*PlaybackProperties playback_properties_change*/
install_scm_function ("PlaybackProperties", scheme_PlaybackProperties);
/*Help browse_manual*/
install_scm_function ("Help", scheme_Help);
/*About about*/
install_scm_function ("About", scheme_About);
/*AddBookmark addbookmark*/
install_scm_function ("AddBookmark", scheme_AddBookmark);
/*GotoBookmark gotobookmark*/
install_scm_function ("GotoBookmark", scheme_GotoBookmark);
/*NextBookmark nextbookmark*/
install_scm_function ("NextBookmark", scheme_NextBookmark);
/*PrevBookmark prevbookmark*/
install_scm_function ("PrevBookmark", scheme_PrevBookmark);
/*ToggleEdit toggle_edit_mode*/
install_scm_function ("ToggleEdit", scheme_ToggleEdit);
/*ToggleRest toggle_rest_mode*/
install_scm_function ("ToggleRest", scheme_ToggleRest);
/*ToggleRhythm toggle_rhythm_mode*/
install_scm_function ("ToggleRhythm", scheme_ToggleRhythm);
/*ClearOverlay clear_overlay*/
install_scm_function ("ClearOverlay", scheme_ClearOverlay);
/*CreateRhythm create_rhythm_cb*/
install_scm_function ("CreateRhythm", scheme_CreateRhythm);
/*DeleteRhythm delete_rhythm_cb*/
install_scm_function ("DeleteRhythm", scheme_DeleteRhythm);
