SCM scheme_CursorLeft (void) {
cursorleft_cb (NULL);
return SCM_EOL;
}
SCM scheme_CursorDown (void) {
cursordown_cb (NULL);
return SCM_EOL;
}
SCM scheme_CursorUp (void) {
cursorup_cb (NULL);
return SCM_EOL;
}
SCM scheme_CursorRight (void) {
cursorright_cb (NULL);
return SCM_EOL;
}
SCM scheme_StaffUp (void) {
staffup_cb (NULL);
return SCM_EOL;
}
SCM scheme_StaffDown (void) {
staffdown_cb (NULL);
return SCM_EOL;
}
SCM scheme_MeasureLeft (void) {
measureleft_cb (NULL);
return SCM_EOL;
}
SCM scheme_MeasureRight (void) {
measureright_cb (NULL);
return SCM_EOL;
}
SCM scheme_A (void) {
go_to_A_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_B (void) {
go_to_B_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_C (void) {
go_to_C_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_D (void) {
go_to_D_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_E (void) {
go_to_E_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_F (void) {
go_to_F_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_G (void) {
go_to_G_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_OctaveUp (void) {
octave_up_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_OctaveDown (void) {
octave_down_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_WholeNote (void) {
insert_chord_0key_cb (NULL);
return SCM_EOL;
}
SCM scheme_HalfNote (void) {
insert_chord_1key_cb (NULL);
return SCM_EOL;
}
SCM scheme_QuarterNote (void) {
insert_chord_2key_cb (NULL);
return SCM_EOL;
}
SCM scheme_EighthNote (void) {
insert_chord_3key_cb (NULL);
return SCM_EOL;
}
SCM scheme_SixteenthNote (void) {
insert_chord_4key_cb (NULL);
return SCM_EOL;
}
SCM scheme_ThirtysecondNote (void) {
insert_chord_5key_cb (NULL);
return SCM_EOL;
}
SCM scheme_SixtyfourthNote (void) {
insert_chord_6key_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertBlankWholeNote (void) {
insert_blankchord_0key_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertBlankHalfNote (void) {
insert_blankchord_1key_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertBlankQuarterNote (void) {
insert_blankchord_2key_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertBlankEighthNote (void) {
insert_blankchord_3key_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertBlankSixteenthNote (void) {
insert_blankchord_4key_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertBlankThirtysecondNote (void) {
insert_blankchord_5key_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertBlankSixtyfourthNote (void) {
insert_blankchord_6key_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleRestMode (void) {
rest_toggle_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleBlankMode (void) {
toggle_blank_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertWholeRest (void) {
insert_rest_0key_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertHalfRest (void) {
insert_rest_1key_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertQuarterRest (void) {
insert_rest_2key_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertEighthRest (void) {
insert_rest_3key_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertSixteenthRest (void) {
insert_rest_4key_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertThirtysecondRest (void) {
insert_rest_5key_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertSixtyfourthRest (void) {
insert_rest_6key_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertDuplet (void) {
insert_duplet_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertTriplet (void) {
insert_triplet_cb (NULL);
return SCM_EOL;
}
SCM scheme_StartTriplet (void) {
start_triplet_cb (NULL);
return SCM_EOL;
}
SCM scheme_EndTuplet (void) {
end_tuplet_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertQuadtuplet (void) {
insert_quadtuplet_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertQuintuplet (void) {
insert_quintuplet_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertSextuplet (void) {
insert_sextuplet_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertSeptuplet (void) {
insert_septuplet_cb (NULL);
return SCM_EOL;
}
SCM scheme_AddTone (void) {
add_tone_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_RemoveTone (void) {
remove_tone_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_SharpenOrStemDown (void) {
sharpen_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_FlattenOrStemUp (void) {
flatten_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_AddDot (void) {
add_dot_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_RemoveDot (void) {
remove_dot_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertTiedNote (void) {
tie_notes_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_DeleteObject (void) {
deleteobject_cb (NULL);
return SCM_EOL;
}
SCM scheme_DeletePreviousObject (void) {
deletepreviousobject_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertMeasure (void) {
insert_measure_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_AppendMeasure (void) {
append_measure_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_DeleteMeasure (void) {
deletemeasure_cb (NULL);
return SCM_EOL;
}
SCM scheme_DeleteMeasureAllStaffs (void) {
deletemeasureallstaffs_cb (NULL);
return SCM_EOL;
}
SCM scheme_ShrinkMeasures (void) {
adjust_measure_less_width_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_WidenMeasures (void) {
adjust_measure_more_width_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_ShorterStaffs (void) {
adjust_staff_less_height_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_TallerStaffs (void) {
adjust_staff_more_height_key_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertTrebleClef (void) {
newcleftreble_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertBassClef (void) {
newclefbass_cb (NULL);
return SCM_EOL;
}
SCM scheme_Insertg8clef (void) {
newclefg8_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertAltoClef (void) {
newclefalto_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertTenorClef (void) {
newcleftenor_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertSopranoClef (void) {
newclefsoprano_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialTrebleClef (void) {
setcleftreble_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialBassClef (void) {
setclefbass_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialg8clef (void) {
setclefg8_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialAltoClef (void) {
setclefalto_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialTenorClef (void) {
setcleftenor_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialSopranoClef (void) {
setclefsoprano_cb (NULL);
return SCM_EOL;
}
SCM scheme_Insert22Time (void) {
newtimesig22_cb (NULL);
return SCM_EOL;
}
SCM scheme_Insert32Time (void) {
newtimesig32_cb (NULL);
return SCM_EOL;
}
SCM scheme_Insert42Time (void) {
newtimesig42_cb (NULL);
return SCM_EOL;
}
SCM scheme_Insert44Time (void) {
newtimesig44_cb (NULL);
return SCM_EOL;
}
SCM scheme_Insert34Time (void) {
newtimesig34_cb (NULL);
return SCM_EOL;
}
SCM scheme_Insert24Time (void) {
newtimesig24_cb (NULL);
return SCM_EOL;
}
SCM scheme_Insert64Time (void) {
newtimesig64_cb (NULL);
return SCM_EOL;
}
SCM scheme_Insert38Time (void) {
newtimesig38_cb (NULL);
return SCM_EOL;
}
SCM scheme_Insert68Time (void) {
newtimesig68_cb (NULL);
return SCM_EOL;
}
SCM scheme_Insert128Time (void) {
newtimesig128_cb (NULL);
return SCM_EOL;
}
SCM scheme_Insert98Time (void) {
newtimesig98_cb (NULL);
return SCM_EOL;
}
SCM scheme_Set22Time (void) {
settimesig22_cb (NULL);
return SCM_EOL;
}
SCM scheme_Set32Time (void) {
settimesig32_cb (NULL);
return SCM_EOL;
}
SCM scheme_Set42Time (void) {
settimesig42_cb (NULL);
return SCM_EOL;
}
SCM scheme_Set44Time (void) {
settimesig44_cb (NULL);
return SCM_EOL;
}
SCM scheme_Set34Time (void) {
settimesig34_cb (NULL);
return SCM_EOL;
}
SCM scheme_Set24Time (void) {
settimesig24_cb (NULL);
return SCM_EOL;
}
SCM scheme_Set64Time (void) {
settimesig64_cb (NULL);
return SCM_EOL;
}
SCM scheme_Set38Time (void) {
settimesig38_cb (NULL);
return SCM_EOL;
}
SCM scheme_Set68Time (void) {
settimesig68_cb (NULL);
return SCM_EOL;
}
SCM scheme_Set128Time (void) {
settimesig128_cb (NULL);
return SCM_EOL;
}
SCM scheme_Set98Time (void) {
settimesig98_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertCmaj (void) {
newkeysigcmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertGmaj (void) {
newkeysiggmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertDmaj (void) {
newkeysigdmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertAmaj (void) {
newkeysigamaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertEmaj (void) {
newkeysigemaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertBmaj (void) {
newkeysigbmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertFSharpmaj (void) {
newkeysigfsharpmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertCSharpmaj (void) {
newkeysigcsharpmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertFmaj (void) {
newkeysigfmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertBflatmaj (void) {
newkeysigbflatmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertEflatmaj (void) {
newkeysigeflatmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertAflatmaj (void) {
newkeysigaflatmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertDflatmaj (void) {
newkeysigdflatmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertGflatmaj (void) {
newkeysiggflatmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertCflatmaj (void) {
newkeysigcflatmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertAmin (void) {
newkeysigamin_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertEmin (void) {
newkeysigemin_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertBmin (void) {
newkeysigbmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertFSharpmin (void) {
newkeysigfsharpmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertCSharpmin (void) {
newkeysigcsharpmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertGSharpmin (void) {
newkeysiggsharpmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertDSharpmin (void) {
newkeysigdsharpmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertASharpmin (void) {
newkeysigasharpmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertDmin (void) {
newkeysigdmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertGmin (void) {
newkeysiggmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertCmin (void) {
newkeysigcmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertFmin (void) {
newkeysigfmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertBflatmin (void) {
newkeysigbflatmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertEflatmin (void) {
newkeysigeflatmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertAflatmin (void) {
newkeysigaflatmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialCmaj (void) {
setkeysigcmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialGmaj (void) {
setkeysiggmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialDmaj (void) {
setkeysigdmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialAmaj (void) {
setkeysigamaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialEmaj (void) {
setkeysigemaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialBmaj (void) {
setkeysigbmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialFSharpmaj (void) {
setkeysigfsharpmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialCSharpmaj (void) {
setkeysigcsharpmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialFmaj (void) {
setkeysigfmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialBflatmaj (void) {
setkeysigbflatmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialEflatmaj (void) {
setkeysigeflatmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialAflatmaj (void) {
setkeysigaflatmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialDflatmaj (void) {
setkeysigdflatmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialGflatmaj (void) {
setkeysiggflatmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialCflatmaj (void) {
setkeysigcflatmaj_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialAmin (void) {
setkeysigamin_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialEmin (void) {
setkeysigemin_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialBmin (void) {
setkeysigbmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialFSharpmin (void) {
setkeysigfsharpmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialCSharpmin (void) {
setkeysigcsharpmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialGSharpmin (void) {
setkeysiggsharpmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialDSharpmin (void) {
setkeysigdsharpmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialASharpmin (void) {
setkeysigasharpmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialDmin (void) {
setkeysigdmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialGmin (void) {
setkeysiggmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialCmin (void) {
setkeysigcmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialFmin (void) {
setkeysigfmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialBflatmin (void) {
setkeysigbflatmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialEflatmin (void) {
setkeysigeflatmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetInitialAflatmin (void) {
setkeysigaflatmin_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetMark (void) {
set_mark_cb (NULL);
return SCM_EOL;
}
SCM scheme_UnsetMark (void) {
unset_mark_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleBeginSlur (void) {
toggle_begin_slur_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleEndSlur (void) {
toggle_end_slur_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleStartCrescendo (void) {
toggle_start_crescendo_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleEndCrescendo (void) {
toggle_end_crescendo_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleStartDiminuendo (void) {
toggle_start_diminuendo_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleEndDiminuendo (void) {
toggle_end_diminuendo_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleAccent (void) {
add_accent_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleFermata (void) {
add_fermata_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleStaccato (void) {
add_staccato_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleTenuto (void) {
add_tenuto_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleTrill (void) {
add_trill_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleTurn (void) {
add_turn_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleMordent (void) {
add_mordent_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleStaccatissimo (void) {
add_staccatissimo_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleCoda (void) {
add_coda_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleFlageolet (void) {
add_flageolet_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleOpen (void) {
add_open_cb (NULL);
return SCM_EOL;
}
SCM scheme_TogglePrallMordent (void) {
add_prallmordent_cb (NULL);
return SCM_EOL;
}
SCM scheme_TogglePrallPrall (void) {
add_prallprall_cb (NULL);
return SCM_EOL;
}
SCM scheme_TogglePrall (void) {
add_prall_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleReverseTurn (void) {
add_reverseturn_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleSegno (void) {
add_segno_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleSforzato (void) {
add_sforzato_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleStopped (void) {
add_stopped_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleThumb (void) {
add_thumb_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleUpprall (void) {
add_upprall_cb (NULL);
return SCM_EOL;
}
SCM scheme_ToggleArpeggio (void) {
add_arpeggio_cb (NULL);
return SCM_EOL;
}
SCM scheme_SetGrace (void) {
set_grace_cb (NULL);
return SCM_EOL;
}
SCM scheme_ForceCaution (void) {
force_cautionary_cb (NULL);
return SCM_EOL;
}
SCM scheme_ChangePitch (void) {
change_pitch_cb (NULL);
return SCM_EOL;
}
SCM scheme_DoubleBar (void) {
insert_doublebar_cb (NULL);
return SCM_EOL;
}
SCM scheme_EndBar (void) {
insert_endbar_cb (NULL);
return SCM_EOL;
}
SCM scheme_OpenRepeat (void) {
insert_openrepeat_cb (NULL);
return SCM_EOL;
}
SCM scheme_CloseRepeat (void) {
insert_closerepeat_cb (NULL);
return SCM_EOL;
}
SCM scheme_OpenCloseRepeat (void) {
insert_opencloserepeat_cb (NULL);
return SCM_EOL;
}
SCM scheme_InsertRhythm (void) {
insert_rhythm_pattern_cb (NULL);
return SCM_EOL;
}
SCM scheme_NextRhythm (void) {
nextrhythm_cb (NULL);
return SCM_EOL;
}
SCM scheme_AppendMesauresToScore (void) {
append_measure_score_cb (NULL);
return SCM_EOL;
}
SCM scheme_New (void) {
file_newwrapper (NULL);
return SCM_EOL;
}
SCM scheme_Open (void) {
file_open_with_check (NULL);
return SCM_EOL;
}
SCM scheme_AddStaffs (void) {
file_add_staffs (NULL);
return SCM_EOL;
}
SCM scheme_AddMovements (void) {
file_add_movements (NULL);
return SCM_EOL;
}
SCM scheme_MovementProps (void) {
movement_props_dialog (NULL);
return SCM_EOL;
}
SCM scheme_OpenNewWindow (void) {
openinnew (NULL);
return SCM_EOL;
}
SCM scheme_Save (void) {
file_savewrapper (NULL);
return SCM_EOL;
}
SCM scheme_SaveAs (void) {
file_saveaswrapper (NULL);
return SCM_EOL;
}
SCM scheme_OpenTemplate (void) {
system_template_open_with_check (NULL);
return SCM_EOL;
}
SCM scheme_OpenExample (void) {
system_example_open_with_check (NULL);
return SCM_EOL;
}
SCM scheme_OpenMyTemplate (void) {
local_template_open_with_check (NULL);
return SCM_EOL;
}
SCM scheme_SaveTemplate (void) {
template_save (NULL);
return SCM_EOL;
}
SCM scheme_NewWindow (void) {
newview (NULL);
return SCM_EOL;
}
SCM scheme_InsertMovementBefore (void) {
insert_movement_before (NULL);
return SCM_EOL;
}
SCM scheme_InsertMovementAfter (void) {
insert_movement_after (NULL);
return SCM_EOL;
}
SCM scheme_SaveParts (void) {
file_savepartswrapper (NULL);
return SCM_EOL;
}
SCM scheme_ExportPDF (void) {
export_pdf_action (NULL);
return SCM_EOL;
}
SCM scheme_ConfigureScore (void) {
scorewizard (NULL);
return SCM_EOL;
}
SCM scheme_PrintPreview (void) {
printpreview_cb (NULL);
return SCM_EOL;
}
SCM scheme_PrintExcerptPreview (void) {
printexcerptpreview_cb (NULL);
return SCM_EOL;
}
SCM scheme_Print (void) {
printall_cb (NULL);
return SCM_EOL;
}
SCM scheme_PrintPart (void) {
printpart_cb (NULL);
return SCM_EOL;
}
SCM scheme_Close (void) {
close_gui_with_check (NULL);
return SCM_EOL;
}
SCM scheme_Quit (void) {
closewrapper (NULL);
return SCM_EOL;
}
SCM scheme_Undo (void) {
undowrapper (NULL);
return SCM_EOL;
}
SCM scheme_Redo (void) {
redowrapper (NULL);
return SCM_EOL;
}
SCM scheme_Copy (void) {
copywrapper (NULL);
return SCM_EOL;
}
SCM scheme_Cut (void) {
cutwrapper (NULL);
return SCM_EOL;
}
SCM scheme_Paste (void) {
pastewrapper (NULL);
return SCM_EOL;
}
SCM scheme_ScoreProperties (void) {
score_properties_dialog (NULL);
return SCM_EOL;
}
SCM scheme_SaveSelection (void) {
saveselwrapper (NULL);
return SCM_EOL;
}
SCM scheme_Preferences (void) {
preferences_change (NULL);
return SCM_EOL;
}
SCM scheme_SaveAccels (void) {
save_default_keymap_file_wrapper (NULL);
return SCM_EOL;
}
SCM scheme_Keyboard (void) {
configure_keyboard_dialog (NULL);
return SCM_EOL;
}
SCM scheme_LoadPlugins (void) {
load_plugin (NULL);
return SCM_EOL;
}
SCM scheme_UnloadPlugins (void) {
unloadplugins (NULL);
return SCM_EOL;
}
SCM scheme_ListPlugins (void) {
list_loaded_plugins (NULL);
return SCM_EOL;
}
SCM scheme_ListAvailablePlugins (void) {
list_available_plugins (NULL);
return SCM_EOL;
}
SCM scheme_SwapStaffs (void) {
swapstaffs (NULL);
return SCM_EOL;
}
SCM scheme_SplitVoices (void) {
splitstaffs (NULL);
return SCM_EOL;
}
SCM scheme_JoinVoices (void) {
joinstaffs (NULL);
return SCM_EOL;
}
SCM scheme_SwapMovements (void) {
swapmovements (NULL);
return SCM_EOL;
}
SCM scheme_VoiceUp (void) {
voiceup (NULL);
return SCM_EOL;
}
SCM scheme_VoiceDown (void) {
voicedown (NULL);
return SCM_EOL;
}
SCM scheme_AddBefore (void) {
newstaffbefore (NULL);
return SCM_EOL;
}
SCM scheme_AddAfter (void) {
dnm_newstaffafter (NULL);
return SCM_EOL;
}
SCM scheme_AddInitial (void) {
newstaffinitial (NULL);
return SCM_EOL;
}
SCM scheme_AddLast (void) {
newstafflast (NULL);
return SCM_EOL;
}
SCM scheme_DeleteBefore (void) {
delete_staff_before (NULL);
return SCM_EOL;
}
SCM scheme_DeleteStaff (void) {
delete_staff_current (NULL);
return SCM_EOL;
}
SCM scheme_DeleteAfter (void) {
delete_staff_after (NULL);
return SCM_EOL;
}
SCM scheme_AddVoice (void) {
dnm_newstaffvoice (NULL);
return SCM_EOL;
}
SCM scheme_TransposeStaff (void) {
staff_transposition (NULL);
return SCM_EOL;
}
SCM scheme_StaffProperties (void) {
staff_properties_change_cb (NULL);
return SCM_EOL;
}
SCM scheme_InitialClef (void) {
clef_change_initial (NULL);
return SCM_EOL;
}
SCM scheme_InsertClef (void) {
clef_change_insert (NULL);
return SCM_EOL;
}
SCM scheme_InitialKey (void) {
key_change_initial (NULL);
return SCM_EOL;
}
SCM scheme_InsertKey (void) {
key_change_insert (NULL);
return SCM_EOL;
}
SCM scheme_InitialTimeSig (void) {
timesig_change_initial (NULL);
return SCM_EOL;
}
SCM scheme_InsertTimeSig (void) {
timesig_change_insert (NULL);
return SCM_EOL;
}
SCM scheme_ChangeNotehead (void) {
set_notehead (NULL);
return SCM_EOL;
}
SCM scheme_InsertStem (void) {
stem_directive_insert (NULL);
return SCM_EOL;
}
SCM scheme_EditLyric (void) {
lyric_insert (NULL);
return SCM_EOL;
}
SCM scheme_EditFiguredBass (void) {
figure_insert (NULL);
return SCM_EOL;
}
SCM scheme_EditChords (void) {
fakechord_insert (NULL);
return SCM_EOL;
}
SCM scheme_InsertDynamic (void) {
insert_dynamic (NULL);
return SCM_EOL;
}
SCM scheme_InsertLilyDirective (void) {
lily_directive_insert (NULL);
return SCM_EOL;
}
SCM scheme_InsertLilyPostfix (void) {
lily_directive_postfix (NULL);
return SCM_EOL;
}
SCM scheme_InsertBarline (void) {
insert_barline (NULL);
return SCM_EOL;
}
SCM scheme_GoToMeasure (void) {
tomeasurenum (NULL);
return SCM_EOL;
}
SCM scheme_GoToBeginning (void) {
tohome (NULL);
return SCM_EOL;
}
SCM scheme_GoToEnd (void) {
toend (NULL);
return SCM_EOL;
}
SCM scheme_NextMovement (void) {
next_movement (NULL);
return SCM_EOL;
}
SCM scheme_PreviousMovement (void) {
prev_movement (NULL);
return SCM_EOL;
}
SCM scheme_DeleteMovement (void) {
delete_movement (NULL);
return SCM_EOL;
}
SCM scheme_DeleteBookmarks (void) {
deletebookmarks (NULL);
return SCM_EOL;
}
SCM scheme_Play (void) {
ext_midi_playback (NULL);
return SCM_EOL;
}
SCM scheme_Stop (void) {
stop_midi_playback (NULL);
return SCM_EOL;
}
SCM scheme_PlayCSound (void) {
dnm_csoundplayback (NULL);
return SCM_EOL;
}
SCM scheme_PlaybackProperties (void) {
playback_properties_change (NULL);
return SCM_EOL;
}
SCM scheme_Help (void) {
browse_manual (NULL);
return SCM_EOL;
}
SCM scheme_About (void) {
about (NULL);
return SCM_EOL;
}
SCM scheme_AddBookmark (void) {
addbookmark (NULL);
return SCM_EOL;
}
SCM scheme_GotoBookmark (void) {
gotobookmark (NULL);
return SCM_EOL;
}
SCM scheme_NextBookmark (void) {
nextbookmark (NULL);
return SCM_EOL;
}
SCM scheme_PrevBookmark (void) {
prevbookmark (NULL);
return SCM_EOL;
}
SCM scheme_ToggleEdit (void) {
toggle_edit_mode (NULL);
return SCM_EOL;
}
SCM scheme_ToggleRest (void) {
toggle_rest_mode (NULL);
return SCM_EOL;
}
SCM scheme_ToggleRhythm (void) {
toggle_rhythm_mode (NULL);
return SCM_EOL;
}
SCM scheme_ClearOverlay (void) {
clear_overlay (NULL);
return SCM_EOL;
}
SCM scheme_CreateRhythm (void) {
create_rhythm_cb (NULL);
return SCM_EOL;
}
SCM scheme_DeleteRhythm (void) {
delete_rhythm_cb (NULL);
return SCM_EOL;
}
