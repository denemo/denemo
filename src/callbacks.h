/******** generated automatically from generate_source. See generate_source.c */
/*CursorLeft cursorleft*/
static void cursorleft_cb (GtkAction *a, DenemoGUI *gui) {
cursorleft (gui);
displayhelper (gui);
}
/*CursorDown cursordown*/
static void cursordown_cb (GtkAction *a, DenemoGUI *gui) {
cursordown (gui);
displayhelper (gui);
}
/*CursorUp cursorup*/
static void cursorup_cb (GtkAction *a, DenemoGUI *gui) {
cursorup (gui);
displayhelper (gui);
}
/*CursorRight cursorright*/
static void cursorright_cb (GtkAction *a, DenemoGUI *gui) {
cursorright (gui);
displayhelper (gui);
}
/*StaffUp staffup*/
static void staffup_cb (GtkAction *a, DenemoGUI *gui) {
staffup (gui);
displayhelper (gui);
}
/*StaffDown staffdown*/
static void staffdown_cb (GtkAction *a, DenemoGUI *gui) {
staffdown (gui);
displayhelper (gui);
}
/*MeasureLeft measureleft*/
static void measureleft_cb (GtkAction *a, DenemoGUI *gui) {
measureleft (gui);
displayhelper (gui);
}
/*MeasureRight measureright*/
static void measureright_cb (GtkAction *a, DenemoGUI *gui) {
measureright (gui);
displayhelper (gui);
}
/*A go_to_A_key*/
static void go_to_A_key_cb (GtkAction *a, DenemoGUI *gui) {
go_to_A_key (gui);
displayhelper (gui);
}
/*B go_to_B_key*/
static void go_to_B_key_cb (GtkAction *a, DenemoGUI *gui) {
go_to_B_key (gui);
displayhelper (gui);
}
/*C go_to_C_key*/
static void go_to_C_key_cb (GtkAction *a, DenemoGUI *gui) {
go_to_C_key (gui);
displayhelper (gui);
}
/*D go_to_D_key*/
static void go_to_D_key_cb (GtkAction *a, DenemoGUI *gui) {
go_to_D_key (gui);
displayhelper (gui);
}
/*E go_to_E_key*/
static void go_to_E_key_cb (GtkAction *a, DenemoGUI *gui) {
go_to_E_key (gui);
displayhelper (gui);
}
/*F go_to_F_key*/
static void go_to_F_key_cb (GtkAction *a, DenemoGUI *gui) {
go_to_F_key (gui);
displayhelper (gui);
}
/*G go_to_G_key*/
static void go_to_G_key_cb (GtkAction *a, DenemoGUI *gui) {
go_to_G_key (gui);
displayhelper (gui);
}
/*OctaveUp octave_up_key*/
static void octave_up_key_cb (GtkAction *a, DenemoGUI *gui) {
octave_up_key (gui);
displayhelper (gui);
}
/*OctaveDown octave_down_key*/
static void octave_down_key_cb (GtkAction *a, DenemoGUI *gui) {
octave_down_key (gui);
displayhelper (gui);
}
/*WholeNote insert_chord_0key*/
static void insert_chord_0key_cb (GtkAction *a, DenemoGUI *gui) {
insert_chord_0key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*HalfNote insert_chord_1key*/
static void insert_chord_1key_cb (GtkAction *a, DenemoGUI *gui) {
insert_chord_1key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*QuarterNote insert_chord_2key*/
static void insert_chord_2key_cb (GtkAction *a, DenemoGUI *gui) {
insert_chord_2key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*EighthNote insert_chord_3key*/
static void insert_chord_3key_cb (GtkAction *a, DenemoGUI *gui) {
insert_chord_3key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SixteenthNote insert_chord_4key*/
static void insert_chord_4key_cb (GtkAction *a, DenemoGUI *gui) {
insert_chord_4key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ThirtysecondNote insert_chord_5key*/
static void insert_chord_5key_cb (GtkAction *a, DenemoGUI *gui) {
insert_chord_5key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SixtyfourthNote insert_chord_6key*/
static void insert_chord_6key_cb (GtkAction *a, DenemoGUI *gui) {
insert_chord_6key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertBlankWholeNote insert_blankchord_0key*/
static void insert_blankchord_0key_cb (GtkAction *a, DenemoGUI *gui) {
insert_blankchord_0key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertBlankHalfNote insert_blankchord_1key*/
static void insert_blankchord_1key_cb (GtkAction *a, DenemoGUI *gui) {
insert_blankchord_1key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertBlankQuarterNote insert_blankchord_2key*/
static void insert_blankchord_2key_cb (GtkAction *a, DenemoGUI *gui) {
insert_blankchord_2key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertBlankEighthNote insert_blankchord_3key*/
static void insert_blankchord_3key_cb (GtkAction *a, DenemoGUI *gui) {
insert_blankchord_3key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertBlankSixteenthNote insert_blankchord_4key*/
static void insert_blankchord_4key_cb (GtkAction *a, DenemoGUI *gui) {
insert_blankchord_4key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertBlankThirtysecondNote insert_blankchord_5key*/
static void insert_blankchord_5key_cb (GtkAction *a, DenemoGUI *gui) {
insert_blankchord_5key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertBlankSixtyfourthNote insert_blankchord_6key*/
static void insert_blankchord_6key_cb (GtkAction *a, DenemoGUI *gui) {
insert_blankchord_6key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleRestMode rest_toggle_key*/
static void rest_toggle_key_cb (GtkAction *a, DenemoGUI *gui) {
rest_toggle_key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleBlankMode toggle_blank*/
static void toggle_blank_cb (GtkAction *a, DenemoGUI *gui) {
toggle_blank (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertWholeRest insert_rest_0key*/
static void insert_rest_0key_cb (GtkAction *a, DenemoGUI *gui) {
insert_rest_0key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertHalfRest insert_rest_1key*/
static void insert_rest_1key_cb (GtkAction *a, DenemoGUI *gui) {
insert_rest_1key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertQuarterRest insert_rest_2key*/
static void insert_rest_2key_cb (GtkAction *a, DenemoGUI *gui) {
insert_rest_2key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertEighthRest insert_rest_3key*/
static void insert_rest_3key_cb (GtkAction *a, DenemoGUI *gui) {
insert_rest_3key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertSixteenthRest insert_rest_4key*/
static void insert_rest_4key_cb (GtkAction *a, DenemoGUI *gui) {
insert_rest_4key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertThirtysecondRest insert_rest_5key*/
static void insert_rest_5key_cb (GtkAction *a, DenemoGUI *gui) {
insert_rest_5key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertSixtyfourthRest insert_rest_6key*/
static void insert_rest_6key_cb (GtkAction *a, DenemoGUI *gui) {
insert_rest_6key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertDuplet insert_duplet*/
static void insert_duplet_cb (GtkAction *a, DenemoGUI *gui) {
insert_duplet (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertTriplet insert_triplet*/
static void insert_triplet_cb (GtkAction *a, DenemoGUI *gui) {
insert_triplet (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*StartTriplet start_triplet*/
static void start_triplet_cb (GtkAction *a, DenemoGUI *gui) {
start_triplet (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*EndTuplet end_tuplet*/
static void end_tuplet_cb (GtkAction *a, DenemoGUI *gui) {
end_tuplet (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertQuadtuplet insert_quadtuplet*/
static void insert_quadtuplet_cb (GtkAction *a, DenemoGUI *gui) {
insert_quadtuplet (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertQuintuplet insert_quintuplet*/
static void insert_quintuplet_cb (GtkAction *a, DenemoGUI *gui) {
insert_quintuplet (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertSextuplet insert_sextuplet*/
static void insert_sextuplet_cb (GtkAction *a, DenemoGUI *gui) {
insert_sextuplet (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertSeptuplet insert_septuplet*/
static void insert_septuplet_cb (GtkAction *a, DenemoGUI *gui) {
insert_septuplet (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*AddTone add_tone_key*/
static void add_tone_key_cb (GtkAction *a, DenemoGUI *gui) {
add_tone_key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*RemoveTone remove_tone_key*/
static void remove_tone_key_cb (GtkAction *a, DenemoGUI *gui) {
remove_tone_key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Sharpen/StemDown sharpen_key*/
static void sharpen_key_cb (GtkAction *a, DenemoGUI *gui) {
sharpen_key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Flatten/StemUp flatten_key*/
static void flatten_key_cb (GtkAction *a, DenemoGUI *gui) {
flatten_key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*AddDot add_dot_key*/
static void add_dot_key_cb (GtkAction *a, DenemoGUI *gui) {
add_dot_key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*RemoveDot remove_dot_key*/
static void remove_dot_key_cb (GtkAction *a, DenemoGUI *gui) {
remove_dot_key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertTiedNote tie_notes_key*/
static void tie_notes_key_cb (GtkAction *a, DenemoGUI *gui) {
tie_notes_key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*DeleteObject deleteobject*/
static void deleteobject_cb (GtkAction *a, DenemoGUI *gui) {
deleteobject (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*DeletePreviousObject deletepreviousobject*/
static void deletepreviousobject_cb (GtkAction *a, DenemoGUI *gui) {
deletepreviousobject (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertMeasure insert_measure_key*/
static void insert_measure_key_cb (GtkAction *a, DenemoGUI *gui) {
insert_measure_key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*AppendMeasure append_measure_key*/
static void append_measure_key_cb (GtkAction *a, DenemoGUI *gui) {
append_measure_key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*DeleteMeasure deletemeasure*/
static void deletemeasure_cb (GtkAction *a, DenemoGUI *gui) {
deletemeasure (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*DeleteMeasureAllStaffs deletemeasureallstaffs*/
static void deletemeasureallstaffs_cb (GtkAction *a, DenemoGUI *gui) {
deletemeasureallstaffs (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ShrinkMeasures adjust_measure_less_width_key*/
static void adjust_measure_less_width_key_cb (GtkAction *a, DenemoGUI *gui) {
adjust_measure_less_width_key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*WidenMeasures adjust_measure_more_width_key*/
static void adjust_measure_more_width_key_cb (GtkAction *a, DenemoGUI *gui) {
adjust_measure_more_width_key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ShorterStaffs adjust_staff_less_height_key*/
static void adjust_staff_less_height_key_cb (GtkAction *a, DenemoGUI *gui) {
adjust_staff_less_height_key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*TallerStaffs adjust_staff_more_height_key*/
static void adjust_staff_more_height_key_cb (GtkAction *a, DenemoGUI *gui) {
adjust_staff_more_height_key (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertTrebleClef newcleftreble*/
static void newcleftreble_cb (GtkAction *a, DenemoGUI *gui) {
newcleftreble (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertBassClef newclefbass*/
static void newclefbass_cb (GtkAction *a, DenemoGUI *gui) {
newclefbass (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Insertg8clef newclefg8*/
static void newclefg8_cb (GtkAction *a, DenemoGUI *gui) {
newclefg8 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertAltoClef newclefalto*/
static void newclefalto_cb (GtkAction *a, DenemoGUI *gui) {
newclefalto (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertTenorClef newcleftenor*/
static void newcleftenor_cb (GtkAction *a, DenemoGUI *gui) {
newcleftenor (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertSopranoClef newclefsoprano*/
static void newclefsoprano_cb (GtkAction *a, DenemoGUI *gui) {
newclefsoprano (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialTrebleClef setcleftreble*/
static void setcleftreble_cb (GtkAction *a, DenemoGUI *gui) {
setcleftreble (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialBassClef setclefbass*/
static void setclefbass_cb (GtkAction *a, DenemoGUI *gui) {
setclefbass (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialg8clef setclefg8*/
static void setclefg8_cb (GtkAction *a, DenemoGUI *gui) {
setclefg8 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialAltoClef setclefalto*/
static void setclefalto_cb (GtkAction *a, DenemoGUI *gui) {
setclefalto (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialTenorClef setcleftenor*/
static void setcleftenor_cb (GtkAction *a, DenemoGUI *gui) {
setcleftenor (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialSopranoClef setclefsoprano*/
static void setclefsoprano_cb (GtkAction *a, DenemoGUI *gui) {
setclefsoprano (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Insert22Time newtimesig22*/
static void newtimesig22_cb (GtkAction *a, DenemoGUI *gui) {
newtimesig22 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Insert32Time newtimesig32*/
static void newtimesig32_cb (GtkAction *a, DenemoGUI *gui) {
newtimesig32 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Insert42Time newtimesig42*/
static void newtimesig42_cb (GtkAction *a, DenemoGUI *gui) {
newtimesig42 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Insert44Time newtimesig44*/
static void newtimesig44_cb (GtkAction *a, DenemoGUI *gui) {
newtimesig44 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Insert34Time newtimesig34*/
static void newtimesig34_cb (GtkAction *a, DenemoGUI *gui) {
newtimesig34 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Insert24Time newtimesig24*/
static void newtimesig24_cb (GtkAction *a, DenemoGUI *gui) {
newtimesig24 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Insert64Time newtimesig64*/
static void newtimesig64_cb (GtkAction *a, DenemoGUI *gui) {
newtimesig64 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Insert38Time newtimesig38*/
static void newtimesig38_cb (GtkAction *a, DenemoGUI *gui) {
newtimesig38 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Insert68Time newtimesig68*/
static void newtimesig68_cb (GtkAction *a, DenemoGUI *gui) {
newtimesig68 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Insert128Time newtimesig128*/
static void newtimesig128_cb (GtkAction *a, DenemoGUI *gui) {
newtimesig128 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Insert98Time newtimesig98*/
static void newtimesig98_cb (GtkAction *a, DenemoGUI *gui) {
newtimesig98 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Set22Time settimesig22*/
static void settimesig22_cb (GtkAction *a, DenemoGUI *gui) {
settimesig22 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Set32Time settimesig32*/
static void settimesig32_cb (GtkAction *a, DenemoGUI *gui) {
settimesig32 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Set42Time settimesig42*/
static void settimesig42_cb (GtkAction *a, DenemoGUI *gui) {
settimesig42 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Set44Time settimesig44*/
static void settimesig44_cb (GtkAction *a, DenemoGUI *gui) {
settimesig44 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Set34Time settimesig34*/
static void settimesig34_cb (GtkAction *a, DenemoGUI *gui) {
settimesig34 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Set24Time settimesig24*/
static void settimesig24_cb (GtkAction *a, DenemoGUI *gui) {
settimesig24 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Set64Time settimesig64*/
static void settimesig64_cb (GtkAction *a, DenemoGUI *gui) {
settimesig64 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Set38Time settimesig38*/
static void settimesig38_cb (GtkAction *a, DenemoGUI *gui) {
settimesig38 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Set68Time settimesig68*/
static void settimesig68_cb (GtkAction *a, DenemoGUI *gui) {
settimesig68 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Set128Time settimesig128*/
static void settimesig128_cb (GtkAction *a, DenemoGUI *gui) {
settimesig128 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*Set98Time settimesig98*/
static void settimesig98_cb (GtkAction *a, DenemoGUI *gui) {
settimesig98 (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertCmaj newkeysigcmaj*/
static void newkeysigcmaj_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigcmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertGmaj newkeysiggmaj*/
static void newkeysiggmaj_cb (GtkAction *a, DenemoGUI *gui) {
newkeysiggmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertDmaj newkeysigdmaj*/
static void newkeysigdmaj_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigdmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertAmaj newkeysigamaj*/
static void newkeysigamaj_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigamaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertEmaj newkeysigemaj*/
static void newkeysigemaj_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigemaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertBmaj newkeysigbmaj*/
static void newkeysigbmaj_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigbmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertFSharpmaj newkeysigfsharpmaj*/
static void newkeysigfsharpmaj_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigfsharpmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertCSharpmaj newkeysigcsharpmaj*/
static void newkeysigcsharpmaj_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigcsharpmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertFmaj newkeysigfmaj*/
static void newkeysigfmaj_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigfmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertBflatmaj newkeysigbflatmaj*/
static void newkeysigbflatmaj_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigbflatmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertEflatmaj newkeysigeflatmaj*/
static void newkeysigeflatmaj_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigeflatmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertAflatmaj newkeysigaflatmaj*/
static void newkeysigaflatmaj_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigaflatmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertDflatmaj newkeysigdflatmaj*/
static void newkeysigdflatmaj_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigdflatmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertGflatmaj newkeysiggflatmaj*/
static void newkeysiggflatmaj_cb (GtkAction *a, DenemoGUI *gui) {
newkeysiggflatmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertCflatmaj newkeysigcflatmaj*/
static void newkeysigcflatmaj_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigcflatmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertAmin newkeysigamin*/
static void newkeysigamin_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigamin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertEmin newkeysigemin*/
static void newkeysigemin_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigemin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertBmin newkeysigbmin*/
static void newkeysigbmin_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigbmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertFSharpmin newkeysigfsharpmin*/
static void newkeysigfsharpmin_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigfsharpmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertCSharpmin newkeysigcsharpmin*/
static void newkeysigcsharpmin_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigcsharpmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertGSharpmin newkeysiggsharpmin*/
static void newkeysiggsharpmin_cb (GtkAction *a, DenemoGUI *gui) {
newkeysiggsharpmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertDSharpmin newkeysigdsharpmin*/
static void newkeysigdsharpmin_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigdsharpmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertASharpmin newkeysigasharpmin*/
static void newkeysigasharpmin_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigasharpmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertDmin newkeysigdmin*/
static void newkeysigdmin_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigdmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertGmin newkeysiggmin*/
static void newkeysiggmin_cb (GtkAction *a, DenemoGUI *gui) {
newkeysiggmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertCmin newkeysigcmin*/
static void newkeysigcmin_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigcmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertFmin newkeysigfmin*/
static void newkeysigfmin_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigfmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertBflatmin newkeysigbflatmin*/
static void newkeysigbflatmin_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigbflatmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertEflatmin newkeysigeflatmin*/
static void newkeysigeflatmin_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigeflatmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertAflatmin newkeysigaflatmin*/
static void newkeysigaflatmin_cb (GtkAction *a, DenemoGUI *gui) {
newkeysigaflatmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialCmaj setkeysigcmaj*/
static void setkeysigcmaj_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigcmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialGmaj setkeysiggmaj*/
static void setkeysiggmaj_cb (GtkAction *a, DenemoGUI *gui) {
setkeysiggmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialDmaj setkeysigdmaj*/
static void setkeysigdmaj_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigdmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialAmaj setkeysigamaj*/
static void setkeysigamaj_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigamaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialEmaj setkeysigemaj*/
static void setkeysigemaj_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigemaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialBmaj setkeysigbmaj*/
static void setkeysigbmaj_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigbmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialFSharpmaj setkeysigfsharpmaj*/
static void setkeysigfsharpmaj_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigfsharpmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialCSharpmaj setkeysigcsharpmaj*/
static void setkeysigcsharpmaj_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigcsharpmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialFmaj setkeysigfmaj*/
static void setkeysigfmaj_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigfmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialBflatmaj setkeysigbflatmaj*/
static void setkeysigbflatmaj_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigbflatmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialEflatmaj setkeysigeflatmaj*/
static void setkeysigeflatmaj_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigeflatmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialAflatmaj setkeysigaflatmaj*/
static void setkeysigaflatmaj_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigaflatmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialDflatmaj setkeysigdflatmaj*/
static void setkeysigdflatmaj_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigdflatmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialGflatmaj setkeysiggflatmaj*/
static void setkeysiggflatmaj_cb (GtkAction *a, DenemoGUI *gui) {
setkeysiggflatmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialCflatmaj setkeysigcflatmaj*/
static void setkeysigcflatmaj_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigcflatmaj (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialAmin setkeysigamin*/
static void setkeysigamin_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigamin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialEmin setkeysigemin*/
static void setkeysigemin_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigemin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialBmin setkeysigbmin*/
static void setkeysigbmin_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigbmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialFSharpmin setkeysigfsharpmin*/
static void setkeysigfsharpmin_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigfsharpmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialCSharpmin setkeysigcsharpmin*/
static void setkeysigcsharpmin_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigcsharpmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialGSharpmin setkeysiggsharpmin*/
static void setkeysiggsharpmin_cb (GtkAction *a, DenemoGUI *gui) {
setkeysiggsharpmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialDSharpmin setkeysigdsharpmin*/
static void setkeysigdsharpmin_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigdsharpmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialASharpmin setkeysigasharpmin*/
static void setkeysigasharpmin_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigasharpmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialDmin setkeysigdmin*/
static void setkeysigdmin_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigdmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialGmin setkeysiggmin*/
static void setkeysiggmin_cb (GtkAction *a, DenemoGUI *gui) {
setkeysiggmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialCmin setkeysigcmin*/
static void setkeysigcmin_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigcmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialFmin setkeysigfmin*/
static void setkeysigfmin_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigfmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialBflatmin setkeysigbflatmin*/
static void setkeysigbflatmin_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigbflatmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialEflatmin setkeysigeflatmin*/
static void setkeysigeflatmin_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigeflatmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetInitialAflatmin setkeysigaflatmin*/
static void setkeysigaflatmin_cb (GtkAction *a, DenemoGUI *gui) {
setkeysigaflatmin (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetMark set_mark*/
static void set_mark_cb (GtkAction *a, DenemoGUI *gui) {
set_mark (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*UnsetMark unset_mark*/
static void unset_mark_cb (GtkAction *a, DenemoGUI *gui) {
unset_mark (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleBeginSlur toggle_begin_slur*/
static void toggle_begin_slur_cb (GtkAction *a, DenemoGUI *gui) {
toggle_begin_slur (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleEndSlur toggle_end_slur*/
static void toggle_end_slur_cb (GtkAction *a, DenemoGUI *gui) {
toggle_end_slur (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleStartCrescendo toggle_start_crescendo*/
static void toggle_start_crescendo_cb (GtkAction *a, DenemoGUI *gui) {
toggle_start_crescendo (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleEndCrescendo toggle_end_crescendo*/
static void toggle_end_crescendo_cb (GtkAction *a, DenemoGUI *gui) {
toggle_end_crescendo (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleStartDiminuendo toggle_start_diminuendo*/
static void toggle_start_diminuendo_cb (GtkAction *a, DenemoGUI *gui) {
toggle_start_diminuendo (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleEndDiminuendo toggle_end_diminuendo*/
static void toggle_end_diminuendo_cb (GtkAction *a, DenemoGUI *gui) {
toggle_end_diminuendo (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleAccent add_accent*/
static void add_accent_cb (GtkAction *a, DenemoGUI *gui) {
add_accent (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleFermata add_fermata*/
static void add_fermata_cb (GtkAction *a, DenemoGUI *gui) {
add_fermata (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleStaccato add_staccato*/
static void add_staccato_cb (GtkAction *a, DenemoGUI *gui) {
add_staccato (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleTenuto add_tenuto*/
static void add_tenuto_cb (GtkAction *a, DenemoGUI *gui) {
add_tenuto (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleTrill add_trill*/
static void add_trill_cb (GtkAction *a, DenemoGUI *gui) {
add_trill (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleTurn add_turn*/
static void add_turn_cb (GtkAction *a, DenemoGUI *gui) {
add_turn (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleMordent add_mordent*/
static void add_mordent_cb (GtkAction *a, DenemoGUI *gui) {
add_mordent (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleStaccatissimo add_staccatissimo*/
static void add_staccatissimo_cb (GtkAction *a, DenemoGUI *gui) {
add_staccatissimo (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleCoda add_coda*/
static void add_coda_cb (GtkAction *a, DenemoGUI *gui) {
add_coda (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleFlageolet add_flageolet*/
static void add_flageolet_cb (GtkAction *a, DenemoGUI *gui) {
add_flageolet (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleOpen add_open*/
static void add_open_cb (GtkAction *a, DenemoGUI *gui) {
add_open (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*TogglePrallMordent add_prallmordent*/
static void add_prallmordent_cb (GtkAction *a, DenemoGUI *gui) {
add_prallmordent (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*TogglePrallPrall add_prallprall*/
static void add_prallprall_cb (GtkAction *a, DenemoGUI *gui) {
add_prallprall (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*TogglePrall add_prall*/
static void add_prall_cb (GtkAction *a, DenemoGUI *gui) {
add_prall (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleReverseTurn add_reverseturn*/
static void add_reverseturn_cb (GtkAction *a, DenemoGUI *gui) {
add_reverseturn (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleSegno add_segno*/
static void add_segno_cb (GtkAction *a, DenemoGUI *gui) {
add_segno (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleSforzato add_sforzato*/
static void add_sforzato_cb (GtkAction *a, DenemoGUI *gui) {
add_sforzato (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleStopped add_stopped*/
static void add_stopped_cb (GtkAction *a, DenemoGUI *gui) {
add_stopped (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleThumb add_thumb*/
static void add_thumb_cb (GtkAction *a, DenemoGUI *gui) {
add_thumb (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleUpprall add_upprall*/
static void add_upprall_cb (GtkAction *a, DenemoGUI *gui) {
add_upprall (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ToggleArpeggio add_arpeggio*/
static void add_arpeggio_cb (GtkAction *a, DenemoGUI *gui) {
add_arpeggio (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*SetGrace set_grace*/
static void set_grace_cb (GtkAction *a, DenemoGUI *gui) {
set_grace (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ForceCaution force_cautionary*/
static void force_cautionary_cb (GtkAction *a, DenemoGUI *gui) {
force_cautionary (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*ChangePitch change_pitch*/
static void change_pitch_cb (GtkAction *a, DenemoGUI *gui) {
change_pitch (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*DoubleBar insert_doublebar*/
static void insert_doublebar_cb (GtkAction *a, DenemoGUI *gui) {
insert_doublebar (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*EndBar insert_endbar*/
static void insert_endbar_cb (GtkAction *a, DenemoGUI *gui) {
insert_endbar (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*OpenRepeat insert_openrepeat*/
static void insert_openrepeat_cb (GtkAction *a, DenemoGUI *gui) {
insert_openrepeat (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*CloseRepeat insert_closerepeat*/
static void insert_closerepeat_cb (GtkAction *a, DenemoGUI *gui) {
insert_closerepeat (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*OpenCloseRepeat insert_opencloserepeat*/
static void insert_opencloserepeat_cb (GtkAction *a, DenemoGUI *gui) {
insert_opencloserepeat (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*InsertRhythm insert_rhythm_pattern*/
static void insert_rhythm_pattern_cb (GtkAction *a, DenemoGUI *gui) {
insert_rhythm_pattern (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*NextRhythm nextrhythm*/
static void nextrhythm_cb (GtkAction *a, DenemoGUI *gui) {
nextrhythm (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
/*AppendMesauresToScore append_measure_score*/
static void append_measure_score_cb (GtkAction *a, DenemoGUI *gui) {
append_measure_score (gui);
displayhelper (gui);
  score_status(gui, TRUE);
}
static void InsertRest0(GtkWidget *menuitem, DenemoGUI *gui){
  highlight_rest(gui, 0);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTREST;
  insert_chord_0key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeRest0(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui);
  gui->mode = INPUTEDIT|INPUTREST;
  insert_chord_0key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  score_status(gui, TRUE);
  displayhelper(gui);
}
void InsertDur0(GtkWidget *menuitem, DenemoGUI *gui){
  highlight_duration(gui, 0);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  insert_chord_0key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeDur0(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui);
  gui->mode = INPUTEDIT|INPUTNORMAL;
  insert_chord_0key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  displayhelper(gui);
}
static void Dur0  (GtkWidget *w, DenemoGUI *gui) {
 if(gui->mode&INPUTINSERT)
   highlight_duration(gui, 0);
 else 
 if( (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))
   ChangeDur0 (w, gui);
else {
 insert_chord_0key(gui);
  score_status(gui, TRUE);
 displayhelper(gui);
 }
}
static void InsertRest1(GtkWidget *menuitem, DenemoGUI *gui){
  highlight_rest(gui, 1);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTREST;
  insert_chord_1key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeRest1(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui);
  gui->mode = INPUTEDIT|INPUTREST;
  insert_chord_1key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  score_status(gui, TRUE);
  displayhelper(gui);
}
void InsertDur1(GtkWidget *menuitem, DenemoGUI *gui){
  highlight_duration(gui, 1);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  insert_chord_1key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeDur1(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui);
  gui->mode = INPUTEDIT|INPUTNORMAL;
  insert_chord_1key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  displayhelper(gui);
}
static void Dur1  (GtkWidget *w, DenemoGUI *gui) {
 if(gui->mode&INPUTINSERT)
   highlight_duration(gui, 1);
 else 
 if( (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))
   ChangeDur1 (w, gui);
else {
 insert_chord_1key(gui);
  score_status(gui, TRUE);
 displayhelper(gui);
 }
}
static void InsertRest2(GtkWidget *menuitem, DenemoGUI *gui){
  highlight_rest(gui, 2);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTREST;
  insert_chord_2key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeRest2(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui);
  gui->mode = INPUTEDIT|INPUTREST;
  insert_chord_2key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  score_status(gui, TRUE);
  displayhelper(gui);
}
void InsertDur2(GtkWidget *menuitem, DenemoGUI *gui){
  highlight_duration(gui, 2);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  insert_chord_2key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeDur2(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui);
  gui->mode = INPUTEDIT|INPUTNORMAL;
  insert_chord_2key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  displayhelper(gui);
}
static void Dur2  (GtkWidget *w, DenemoGUI *gui) {
 if(gui->mode&INPUTINSERT)
   highlight_duration(gui, 2);
 else 
 if( (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))
   ChangeDur2 (w, gui);
else {
 insert_chord_2key(gui);
  score_status(gui, TRUE);
 displayhelper(gui);
 }
}
static void InsertRest3(GtkWidget *menuitem, DenemoGUI *gui){
  highlight_rest(gui, 3);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTREST;
  insert_chord_3key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeRest3(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui);
  gui->mode = INPUTEDIT|INPUTREST;
  insert_chord_3key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  score_status(gui, TRUE);
  displayhelper(gui);
}
void InsertDur3(GtkWidget *menuitem, DenemoGUI *gui){
  highlight_duration(gui, 3);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  insert_chord_3key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeDur3(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui);
  gui->mode = INPUTEDIT|INPUTNORMAL;
  insert_chord_3key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  displayhelper(gui);
}
static void Dur3  (GtkWidget *w, DenemoGUI *gui) {
 if(gui->mode&INPUTINSERT)
   highlight_duration(gui, 3);
 else 
 if( (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))
   ChangeDur3 (w, gui);
else {
 insert_chord_3key(gui);
  score_status(gui, TRUE);
 displayhelper(gui);
 }
}
static void InsertRest4(GtkWidget *menuitem, DenemoGUI *gui){
  highlight_rest(gui, 4);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTREST;
  insert_chord_4key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeRest4(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui);
  gui->mode = INPUTEDIT|INPUTREST;
  insert_chord_4key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  score_status(gui, TRUE);
  displayhelper(gui);
}
void InsertDur4(GtkWidget *menuitem, DenemoGUI *gui){
  highlight_duration(gui, 4);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  insert_chord_4key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeDur4(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui);
  gui->mode = INPUTEDIT|INPUTNORMAL;
  insert_chord_4key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  displayhelper(gui);
}
static void Dur4  (GtkWidget *w, DenemoGUI *gui) {
 if(gui->mode&INPUTINSERT)
   highlight_duration(gui, 4);
 else 
 if( (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))
   ChangeDur4 (w, gui);
else {
 insert_chord_4key(gui);
  score_status(gui, TRUE);
 displayhelper(gui);
 }
}
static void InsertRest5(GtkWidget *menuitem, DenemoGUI *gui){
  highlight_rest(gui, 5);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTREST;
  insert_chord_5key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeRest5(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui);
  gui->mode = INPUTEDIT|INPUTREST;
  insert_chord_5key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  score_status(gui, TRUE);
  displayhelper(gui);
}
void InsertDur5(GtkWidget *menuitem, DenemoGUI *gui){
  highlight_duration(gui, 5);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  insert_chord_5key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeDur5(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui);
  gui->mode = INPUTEDIT|INPUTNORMAL;
  insert_chord_5key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  displayhelper(gui);
}
static void Dur5  (GtkWidget *w, DenemoGUI *gui) {
 if(gui->mode&INPUTINSERT)
   highlight_duration(gui, 5);
 else 
 if( (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))
   ChangeDur5 (w, gui);
else {
 insert_chord_5key(gui);
  score_status(gui, TRUE);
 displayhelper(gui);
 }
}
static void InsertRest6(GtkWidget *menuitem, DenemoGUI *gui){
  highlight_rest(gui, 6);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTREST;
  insert_chord_6key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeRest6(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui);
  gui->mode = INPUTEDIT|INPUTREST;
  insert_chord_6key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  score_status(gui, TRUE);
  displayhelper(gui);
}
void InsertDur6(GtkWidget *menuitem, DenemoGUI *gui){
  highlight_duration(gui, 6);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  insert_chord_6key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeDur6(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui);
  gui->mode = INPUTEDIT|INPUTNORMAL;
  insert_chord_6key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  displayhelper(gui);
}
static void Dur6  (GtkWidget *w, DenemoGUI *gui) {
 if(gui->mode&INPUTINSERT)
   highlight_duration(gui, 6);
 else 
 if( (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))
   ChangeDur6 (w, gui);
else {
 insert_chord_6key(gui);
  score_status(gui, TRUE);
 displayhelper(gui);
 }
}
static void ChangeToA(GtkWidget *menuitem, DenemoGUI *gui){
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui); 
  gint mode = gui->mode;
  gui->mode = INPUTEDIT|INPUTNORMAL;
  go_to_A_key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeToB(GtkWidget *menuitem, DenemoGUI *gui){
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui); 
  gint mode = gui->mode;
  gui->mode = INPUTEDIT|INPUTNORMAL;
  go_to_B_key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeToC(GtkWidget *menuitem, DenemoGUI *gui){
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui); 
  gint mode = gui->mode;
  gui->mode = INPUTEDIT|INPUTNORMAL;
  go_to_C_key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeToD(GtkWidget *menuitem, DenemoGUI *gui){
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui); 
  gint mode = gui->mode;
  gui->mode = INPUTEDIT|INPUTNORMAL;
  go_to_D_key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeToE(GtkWidget *menuitem, DenemoGUI *gui){
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui); 
  gint mode = gui->mode;
  gui->mode = INPUTEDIT|INPUTNORMAL;
  go_to_E_key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeToF(GtkWidget *menuitem, DenemoGUI *gui){
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui); 
  gint mode = gui->mode;
  gui->mode = INPUTEDIT|INPUTNORMAL;
  go_to_F_key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeToG(GtkWidget *menuitem, DenemoGUI *gui){
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    cursorleft(gui); 
  gint mode = gui->mode;
  gui->mode = INPUTEDIT|INPUTNORMAL;
  go_to_G_key(gui);
  gui->mode = mode;
  if(appending)
    cursorright(gui);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void InsertA(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_A_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void InsertB(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_B_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void InsertC(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_C_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void InsertD(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_D_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void InsertE(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_E_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void InsertF(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_F_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void InsertG(GtkWidget *menuitem, DenemoGUI *gui){
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_G_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
