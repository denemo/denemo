/******** generated automatically from generate_source. See generate_source.c */
/*CursorLeft cursorleft*/
static void cursorleft_cb (GtkAction *action, DenemoScriptParam *param) {
cursorleft (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*MoveCursorLeft movecursorleft*/
static void movecursorleft_cb (GtkAction *action, DenemoScriptParam *param) {
movecursorleft (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*CursorDown cursordown*/
static void cursordown_cb (GtkAction *action, DenemoScriptParam *param) {
cursordown (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*CursorUp cursorup*/
static void cursorup_cb (GtkAction *action, DenemoScriptParam *param) {
cursorup (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*CursorRight cursorright*/
static void cursorright_cb (GtkAction *action, DenemoScriptParam *param) {
cursorright (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*MoveCursorRight movecursorright*/
static void movecursorright_cb (GtkAction *action, DenemoScriptParam *param) {
movecursorright (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*StaffUp staffup*/
static void staffup_cb (GtkAction *action, DenemoScriptParam *param) {
staffup (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*StaffDown staffdown*/
static void staffdown_cb (GtkAction *action, DenemoScriptParam *param) {
staffdown (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*MoveToStaffUp movetostaffup*/
static void movetostaffup_cb (GtkAction *action, DenemoScriptParam *param) {
movetostaffup (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*MoveToStaffDown movetostaffdown*/
static void movetostaffdown_cb (GtkAction *action, DenemoScriptParam *param) {
movetostaffdown (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*MeasureLeft measureleft*/
static void measureleft_cb (GtkAction *action, DenemoScriptParam *param) {
measureleft (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*MeasureRight measureright*/
static void measureright_cb (GtkAction *action, DenemoScriptParam *param) {
measureright (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*MoveToMeasureLeft movetomeasureleft*/
static void movetomeasureleft_cb (GtkAction *action, DenemoScriptParam *param) {
movetomeasureleft (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*MoveToMeasureRight movetomeasureright*/
static void movetomeasureright_cb (GtkAction *action, DenemoScriptParam *param) {
movetomeasureright (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*A go_to_A_key*/
static void go_to_A_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
go_to_A_key (gui);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*B go_to_B_key*/
static void go_to_B_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
go_to_B_key (gui);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*C go_to_C_key*/
static void go_to_C_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
go_to_C_key (gui);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*D go_to_D_key*/
static void go_to_D_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
go_to_D_key (gui);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*E go_to_E_key*/
static void go_to_E_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
go_to_E_key (gui);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*F go_to_F_key*/
static void go_to_F_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
go_to_F_key (gui);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*G go_to_G_key*/
static void go_to_G_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
go_to_G_key (gui);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*OctaveUp octave_up_key*/
static void octave_up_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
octave_up_key (gui);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*OctaveDown octave_down_key*/
static void octave_down_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
octave_down_key (gui);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*WholeNote insert_chord_0key*/
static void insert_chord_0key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_chord_0key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*HalfNote insert_chord_1key*/
static void insert_chord_1key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_chord_1key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*QuarterNote insert_chord_2key*/
static void insert_chord_2key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_chord_2key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*EighthNote insert_chord_3key*/
static void insert_chord_3key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_chord_3key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SixteenthNote insert_chord_4key*/
static void insert_chord_4key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_chord_4key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ThirtysecondNote insert_chord_5key*/
static void insert_chord_5key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_chord_5key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SixtyfourthNote insert_chord_6key*/
static void insert_chord_6key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_chord_6key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*OneHundredTwentyEighthNote insert_chord_7key*/
static void insert_chord_7key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_chord_7key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*TwoHundredFiftySixthNote insert_chord_8key*/
static void insert_chord_8key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_chord_8key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertWholeRest insert_rest_0key*/
static void insert_rest_0key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_rest_0key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertHalfRest insert_rest_1key*/
static void insert_rest_1key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_rest_1key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertQuarterRest insert_rest_2key*/
static void insert_rest_2key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_rest_2key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertEighthRest insert_rest_3key*/
static void insert_rest_3key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_rest_3key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertSixteenthRest insert_rest_4key*/
static void insert_rest_4key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_rest_4key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertThirtysecondRest insert_rest_5key*/
static void insert_rest_5key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_rest_5key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertSixtyfourthRest insert_rest_6key*/
static void insert_rest_6key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_rest_6key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertBlankWholeNote insert_blankchord_0key*/
static void insert_blankchord_0key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_blankchord_0key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertBlankHalfNote insert_blankchord_1key*/
static void insert_blankchord_1key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_blankchord_1key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertBlankQuarterNote insert_blankchord_2key*/
static void insert_blankchord_2key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_blankchord_2key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertBlankEighthNote insert_blankchord_3key*/
static void insert_blankchord_3key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_blankchord_3key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertBlankSixteenthNote insert_blankchord_4key*/
static void insert_blankchord_4key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_blankchord_4key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertBlankThirtysecondNote insert_blankchord_5key*/
static void insert_blankchord_5key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_blankchord_5key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertBlankSixtyfourthNote insert_blankchord_6key*/
static void insert_blankchord_6key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_blankchord_6key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertBlankOneHundredTwentyEighthNote insert_blankchord_7key*/
static void insert_blankchord_7key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_blankchord_7key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertBlankTwoHundredFiftySixthNote insert_blankchord_8key*/
static void insert_blankchord_8key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_blankchord_8key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleRestMode rest_toggle_key*/
static void rest_toggle_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
rest_toggle_key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleBlankMode toggle_blank*/
static void toggle_blank_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
toggle_blank (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertDuplet insert_duplet*/
static void insert_duplet_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_duplet (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertTriplet insert_triplet*/
static void insert_triplet_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_triplet (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*StartTriplet start_triplet*/
static void start_triplet_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
start_triplet (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*EndTuplet end_tuplet*/
static void end_tuplet_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
end_tuplet (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertQuadtuplet insert_quadtuplet*/
static void insert_quadtuplet_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_quadtuplet (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertQuintuplet insert_quintuplet*/
static void insert_quintuplet_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_quintuplet (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertSextuplet insert_sextuplet*/
static void insert_sextuplet_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_sextuplet (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertSeptuplet insert_septuplet*/
static void insert_septuplet_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_septuplet (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*AddNoteToChord add_tone_key*/
static void add_tone_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_tone_key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*RemoveNoteFromChord remove_tone_key*/
static void remove_tone_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
remove_tone_key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Sharpen sharpen_key*/
static void sharpen_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
sharpen_key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Flatten flatten_key*/
static void flatten_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
flatten_key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*PendingSharpen pending_sharpen*/
static void pending_sharpen_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
pending_sharpen (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*PendingFlatten pending_flatten*/
static void pending_flatten_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
pending_flatten (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*StemUp stem_up*/
static void stem_up_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
stem_up (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*StemDown stem_down*/
static void stem_down_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
stem_down (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*AddDot add_dot_key*/
static void add_dot_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_dot_key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*RemoveDot remove_dot_key*/
static void remove_dot_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
remove_dot_key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertTiedNote tie_notes_key*/
static void tie_notes_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
tie_notes_key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*DeleteObject deleteobject*/
static void deleteobject_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
deleteobject (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*DeletePreviousObject deletepreviousobject*/
static void deletepreviousobject_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
deletepreviousobject (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertMeasure insert_measure_key*/
static void insert_measure_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_measure_key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*AddMeasure addmeasureafter*/
static void addmeasureafter_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
addmeasureafter (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertMeasureBefore insertmeasurebefore*/
static void insertmeasurebefore_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insertmeasurebefore (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertMeasureAfter insertmeasureafter*/
static void insertmeasureafter_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insertmeasureafter (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*AppendMeasure append_measure_key*/
static void append_measure_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
append_measure_key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*DeleteMeasure deletemeasure*/
static void deletemeasure_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
deletemeasure (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*DeleteMeasureAllStaffs deletemeasureallstaffs*/
static void deletemeasureallstaffs_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
deletemeasureallstaffs (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ShrinkMeasures adjust_measure_less_width_key*/
static void adjust_measure_less_width_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
adjust_measure_less_width_key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*WidenMeasures adjust_measure_more_width_key*/
static void adjust_measure_more_width_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
adjust_measure_more_width_key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ShorterStaffs adjust_staff_less_height_key*/
static void adjust_staff_less_height_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
adjust_staff_less_height_key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*TallerStaffs adjust_staff_more_height_key*/
static void adjust_staff_more_height_key_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
adjust_staff_more_height_key (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertTrebleClef newcleftreble*/
static void newcleftreble_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newcleftreble (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertBassClef newclefbass*/
static void newclefbass_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newclefbass (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Insertg8clef newclefg8*/
static void newclefg8_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newclefg8 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertAltoClef newclefalto*/
static void newclefalto_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newclefalto (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertTenorClef newcleftenor*/
static void newcleftenor_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newcleftenor (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertSopranoClef newclefsoprano*/
static void newclefsoprano_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newclefsoprano (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialTrebleClef setcleftreble*/
static void setcleftreble_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setcleftreble (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialBassClef setclefbass*/
static void setclefbass_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setclefbass (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialg8clef setclefg8*/
static void setclefg8_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setclefg8 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialAltoClef setclefalto*/
static void setclefalto_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setclefalto (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialTenorClef setcleftenor*/
static void setcleftenor_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setcleftenor (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialSopranoClef setclefsoprano*/
static void setclefsoprano_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setclefsoprano (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Insert22Time newtimesig22*/
static void newtimesig22_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newtimesig22 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Insert32Time newtimesig32*/
static void newtimesig32_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newtimesig32 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Insert42Time newtimesig42*/
static void newtimesig42_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newtimesig42 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Insert44Time newtimesig44*/
static void newtimesig44_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newtimesig44 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Insert34Time newtimesig34*/
static void newtimesig34_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newtimesig34 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Insert24Time newtimesig24*/
static void newtimesig24_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newtimesig24 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Insert64Time newtimesig64*/
static void newtimesig64_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newtimesig64 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Insert38Time newtimesig38*/
static void newtimesig38_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newtimesig38 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Insert68Time newtimesig68*/
static void newtimesig68_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newtimesig68 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Insert128Time newtimesig128*/
static void newtimesig128_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newtimesig128 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Insert98Time newtimesig98*/
static void newtimesig98_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newtimesig98 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Set22Time settimesig22*/
static void settimesig22_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
settimesig22 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Set32Time settimesig32*/
static void settimesig32_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
settimesig32 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Set42Time settimesig42*/
static void settimesig42_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
settimesig42 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Set44Time settimesig44*/
static void settimesig44_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
settimesig44 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Set34Time settimesig34*/
static void settimesig34_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
settimesig34 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Set24Time settimesig24*/
static void settimesig24_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
settimesig24 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Set64Time settimesig64*/
static void settimesig64_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
settimesig64 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Set38Time settimesig38*/
static void settimesig38_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
settimesig38 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Set68Time settimesig68*/
static void settimesig68_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
settimesig68 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Set128Time settimesig128*/
static void settimesig128_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
settimesig128 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*Set98Time settimesig98*/
static void settimesig98_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
settimesig98 (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertCmaj newkeysigcmaj*/
static void newkeysigcmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigcmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertGmaj newkeysiggmaj*/
static void newkeysiggmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysiggmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertDmaj newkeysigdmaj*/
static void newkeysigdmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigdmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertAmaj newkeysigamaj*/
static void newkeysigamaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigamaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertEmaj newkeysigemaj*/
static void newkeysigemaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigemaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertBmaj newkeysigbmaj*/
static void newkeysigbmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigbmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertFSharpmaj newkeysigfsharpmaj*/
static void newkeysigfsharpmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigfsharpmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertCSharpmaj newkeysigcsharpmaj*/
static void newkeysigcsharpmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigcsharpmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertFmaj newkeysigfmaj*/
static void newkeysigfmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigfmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertBflatmaj newkeysigbflatmaj*/
static void newkeysigbflatmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigbflatmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertEflatmaj newkeysigeflatmaj*/
static void newkeysigeflatmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigeflatmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertAflatmaj newkeysigaflatmaj*/
static void newkeysigaflatmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigaflatmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertDflatmaj newkeysigdflatmaj*/
static void newkeysigdflatmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigdflatmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertGflatmaj newkeysiggflatmaj*/
static void newkeysiggflatmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysiggflatmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertCflatmaj newkeysigcflatmaj*/
static void newkeysigcflatmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigcflatmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertAmin newkeysigamin*/
static void newkeysigamin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigamin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertEmin newkeysigemin*/
static void newkeysigemin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigemin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertBmin newkeysigbmin*/
static void newkeysigbmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigbmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertFSharpmin newkeysigfsharpmin*/
static void newkeysigfsharpmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigfsharpmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertCSharpmin newkeysigcsharpmin*/
static void newkeysigcsharpmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigcsharpmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertGSharpmin newkeysiggsharpmin*/
static void newkeysiggsharpmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysiggsharpmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertDSharpmin newkeysigdsharpmin*/
static void newkeysigdsharpmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigdsharpmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertASharpmin newkeysigasharpmin*/
static void newkeysigasharpmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigasharpmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertDmin newkeysigdmin*/
static void newkeysigdmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigdmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertGmin newkeysiggmin*/
static void newkeysiggmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysiggmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertCmin newkeysigcmin*/
static void newkeysigcmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigcmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertFmin newkeysigfmin*/
static void newkeysigfmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigfmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertBflatmin newkeysigbflatmin*/
static void newkeysigbflatmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigbflatmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertEflatmin newkeysigeflatmin*/
static void newkeysigeflatmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigeflatmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertAflatmin newkeysigaflatmin*/
static void newkeysigaflatmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
newkeysigaflatmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialCmaj setkeysigcmaj*/
static void setkeysigcmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigcmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialGmaj setkeysiggmaj*/
static void setkeysiggmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysiggmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialDmaj setkeysigdmaj*/
static void setkeysigdmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigdmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialAmaj setkeysigamaj*/
static void setkeysigamaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigamaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialEmaj setkeysigemaj*/
static void setkeysigemaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigemaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialBmaj setkeysigbmaj*/
static void setkeysigbmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigbmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialFSharpmaj setkeysigfsharpmaj*/
static void setkeysigfsharpmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigfsharpmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialCSharpmaj setkeysigcsharpmaj*/
static void setkeysigcsharpmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigcsharpmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialFmaj setkeysigfmaj*/
static void setkeysigfmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigfmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialBflatmaj setkeysigbflatmaj*/
static void setkeysigbflatmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigbflatmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialEflatmaj setkeysigeflatmaj*/
static void setkeysigeflatmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigeflatmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialAflatmaj setkeysigaflatmaj*/
static void setkeysigaflatmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigaflatmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialDflatmaj setkeysigdflatmaj*/
static void setkeysigdflatmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigdflatmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialGflatmaj setkeysiggflatmaj*/
static void setkeysiggflatmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysiggflatmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialCflatmaj setkeysigcflatmaj*/
static void setkeysigcflatmaj_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigcflatmaj (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialAmin setkeysigamin*/
static void setkeysigamin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigamin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialEmin setkeysigemin*/
static void setkeysigemin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigemin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialBmin setkeysigbmin*/
static void setkeysigbmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigbmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialFSharpmin setkeysigfsharpmin*/
static void setkeysigfsharpmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigfsharpmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialCSharpmin setkeysigcsharpmin*/
static void setkeysigcsharpmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigcsharpmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialGSharpmin setkeysiggsharpmin*/
static void setkeysiggsharpmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysiggsharpmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialDSharpmin setkeysigdsharpmin*/
static void setkeysigdsharpmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigdsharpmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialASharpmin setkeysigasharpmin*/
static void setkeysigasharpmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigasharpmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialDmin setkeysigdmin*/
static void setkeysigdmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigdmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialGmin setkeysiggmin*/
static void setkeysiggmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysiggmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialCmin setkeysigcmin*/
static void setkeysigcmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigcmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialFmin setkeysigfmin*/
static void setkeysigfmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigfmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialBflatmin setkeysigbflatmin*/
static void setkeysigbflatmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigbflatmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialEflatmin setkeysigeflatmin*/
static void setkeysigeflatmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigeflatmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetInitialAflatmin setkeysigaflatmin*/
static void setkeysigaflatmin_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
setkeysigaflatmin (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetMark set_mark*/
static void set_mark_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
set_mark (gui);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*UnsetMark unset_mark*/
static void unset_mark_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
unset_mark (gui);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*SetPoint set_point*/
static void set_point_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
set_point (gui);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*ToggleBeginSlur toggle_begin_slur*/
static void toggle_begin_slur_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
toggle_begin_slur (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleEndSlur toggle_end_slur*/
static void toggle_end_slur_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
toggle_end_slur (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleStartCrescendo toggle_start_crescendo*/
static void toggle_start_crescendo_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
toggle_start_crescendo (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleEndCrescendo toggle_end_crescendo*/
static void toggle_end_crescendo_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
toggle_end_crescendo (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleStartDiminuendo toggle_start_diminuendo*/
static void toggle_start_diminuendo_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
toggle_start_diminuendo (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleEndDiminuendo toggle_end_diminuendo*/
static void toggle_end_diminuendo_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
toggle_end_diminuendo (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleAccent add_accent*/
static void add_accent_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_accent (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleFermata add_fermata*/
static void add_fermata_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_fermata (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleStaccato add_staccato*/
static void add_staccato_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_staccato (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleTenuto add_tenuto*/
static void add_tenuto_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_tenuto (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleTrill add_trill*/
static void add_trill_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_trill (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleTurn add_turn*/
static void add_turn_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_turn (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleMordent add_mordent*/
static void add_mordent_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_mordent (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleStaccatissimo add_staccatissimo*/
static void add_staccatissimo_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_staccatissimo (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleCoda add_coda*/
static void add_coda_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_coda (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleFlageolet add_flageolet*/
static void add_flageolet_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_flageolet (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleOpen add_open*/
static void add_open_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_open (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*TogglePrallMordent add_prallmordent*/
static void add_prallmordent_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_prallmordent (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*TogglePrallPrall add_prallprall*/
static void add_prallprall_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_prallprall (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*TogglePrall add_prall*/
static void add_prall_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_prall (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleReverseTurn add_reverseturn*/
static void add_reverseturn_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_reverseturn (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleSegno add_segno*/
static void add_segno_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_segno (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleSforzato add_sforzato*/
static void add_sforzato_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_sforzato (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleStopped add_stopped*/
static void add_stopped_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_stopped (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleThumb add_thumb*/
static void add_thumb_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_thumb (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleUpprall add_upprall*/
static void add_upprall_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_upprall (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ToggleArpeggio add_arpeggio*/
static void add_arpeggio_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
add_arpeggio (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*SetGrace set_grace*/
static void set_grace_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
set_grace (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ForceCaution force_cautionary*/
static void force_cautionary_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
force_cautionary (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*ChangePitch change_pitch*/
static void change_pitch_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
change_pitch (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*InsertRhythm insert_rhythm_pattern*/
static void insert_rhythm_pattern_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
insert_rhythm_pattern (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*NextRhythm nextrhythm*/
static void nextrhythm_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
nextrhythm (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*AppendMeasureAllStaffs append_measure_score*/
static void append_measure_score_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
append_measure_score (gui);
  displayhelper (gui);
score_status(gui, TRUE);
}
/*VoiceUp voiceup*/
static void voiceup_cb (GtkAction *action, DenemoScriptParam *param) {
voiceup (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*VoiceDown voicedown*/
static void voicedown_cb (GtkAction *action, DenemoScriptParam *param) {
voicedown (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*MoveToVoiceUp movetovoiceup*/
static void movetovoiceup_cb (GtkAction *action, DenemoScriptParam *param) {
movetovoiceup (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*MoveToVoiceDown movetovoicedown*/
static void movetovoicedown_cb (GtkAction *action, DenemoScriptParam *param) {
movetovoicedown (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
static void InsertRest0(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_rest(gui, 0);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTREST;
  insert_chord_0key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeRest0(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTREST;
  insert_chord_0key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
void InsertDur0(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 0);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  insert_chord_0key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeDur0(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTNORMAL;
  insert_chord_0key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void SetDur0(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 0);
//  displayhelper(gui);
}
static void Dur0  (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
 if(gui->mode&INPUTINSERT)
   highlight_duration(gui, 0);
 else 
 if( (!gui->mode&INPUTRHYTHM) && (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))
   ChangeDur0 (action, param);
else {
 insert_chord_0key(gui);
   highlight_duration(gui, 0);
  score_status(gui, TRUE);
 displayhelper(gui);
 }
}
static void InsertRest1(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_rest(gui, 1);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTREST;
  insert_chord_1key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeRest1(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTREST;
  insert_chord_1key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
void InsertDur1(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 1);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  insert_chord_1key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeDur1(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTNORMAL;
  insert_chord_1key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void SetDur1(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 1);
//  displayhelper(gui);
}
static void Dur1  (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
 if(gui->mode&INPUTINSERT)
   highlight_duration(gui, 1);
 else 
 if( (!gui->mode&INPUTRHYTHM) && (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))
   ChangeDur1 (action, param);
else {
 insert_chord_1key(gui);
   highlight_duration(gui, 1);
  score_status(gui, TRUE);
 displayhelper(gui);
 }
}
static void InsertRest2(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_rest(gui, 2);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTREST;
  insert_chord_2key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeRest2(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTREST;
  insert_chord_2key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
void InsertDur2(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 2);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  insert_chord_2key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeDur2(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTNORMAL;
  insert_chord_2key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void SetDur2(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 2);
//  displayhelper(gui);
}
static void Dur2  (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
 if(gui->mode&INPUTINSERT)
   highlight_duration(gui, 2);
 else 
 if( (!gui->mode&INPUTRHYTHM) && (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))
   ChangeDur2 (action, param);
else {
 insert_chord_2key(gui);
   highlight_duration(gui, 2);
  score_status(gui, TRUE);
 displayhelper(gui);
 }
}
static void InsertRest3(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_rest(gui, 3);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTREST;
  insert_chord_3key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeRest3(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTREST;
  insert_chord_3key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
void InsertDur3(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 3);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  insert_chord_3key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeDur3(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTNORMAL;
  insert_chord_3key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void SetDur3(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 3);
//  displayhelper(gui);
}
static void Dur3  (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
 if(gui->mode&INPUTINSERT)
   highlight_duration(gui, 3);
 else 
 if( (!gui->mode&INPUTRHYTHM) && (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))
   ChangeDur3 (action, param);
else {
 insert_chord_3key(gui);
   highlight_duration(gui, 3);
  score_status(gui, TRUE);
 displayhelper(gui);
 }
}
static void InsertRest4(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_rest(gui, 4);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTREST;
  insert_chord_4key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeRest4(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTREST;
  insert_chord_4key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
void InsertDur4(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 4);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  insert_chord_4key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeDur4(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTNORMAL;
  insert_chord_4key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void SetDur4(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 4);
//  displayhelper(gui);
}
static void Dur4  (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
 if(gui->mode&INPUTINSERT)
   highlight_duration(gui, 4);
 else 
 if( (!gui->mode&INPUTRHYTHM) && (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))
   ChangeDur4 (action, param);
else {
 insert_chord_4key(gui);
   highlight_duration(gui, 4);
  score_status(gui, TRUE);
 displayhelper(gui);
 }
}
static void InsertRest5(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_rest(gui, 5);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTREST;
  insert_chord_5key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeRest5(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTREST;
  insert_chord_5key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
void InsertDur5(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 5);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  insert_chord_5key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeDur5(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTNORMAL;
  insert_chord_5key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void SetDur5(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 5);
//  displayhelper(gui);
}
static void Dur5  (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
 if(gui->mode&INPUTINSERT)
   highlight_duration(gui, 5);
 else 
 if( (!gui->mode&INPUTRHYTHM) && (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))
   ChangeDur5 (action, param);
else {
 insert_chord_5key(gui);
   highlight_duration(gui, 5);
  score_status(gui, TRUE);
 displayhelper(gui);
 }
}
static void InsertRest6(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_rest(gui, 6);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTREST;
  insert_chord_6key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeRest6(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTREST;
  insert_chord_6key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
void InsertDur6(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 6);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  insert_chord_6key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeDur6(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTNORMAL;
  insert_chord_6key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void SetDur6(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 6);
//  displayhelper(gui);
}
static void Dur6  (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
 if(gui->mode&INPUTINSERT)
   highlight_duration(gui, 6);
 else 
 if( (!gui->mode&INPUTRHYTHM) && (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))
   ChangeDur6 (action, param);
else {
 insert_chord_6key(gui);
   highlight_duration(gui, 6);
  score_status(gui, TRUE);
 displayhelper(gui);
 }
}
static void InsertRest7(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_rest(gui, 7);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTREST;
  insert_chord_7key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeRest7(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTREST;
  insert_chord_7key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
void InsertDur7(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 7);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  insert_chord_7key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeDur7(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTNORMAL;
  insert_chord_7key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void SetDur7(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 7);
//  displayhelper(gui);
}
static void Dur7  (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
 if(gui->mode&INPUTINSERT)
   highlight_duration(gui, 7);
 else 
 if( (!gui->mode&INPUTRHYTHM) && (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))
   ChangeDur7 (action, param);
else {
 insert_chord_7key(gui);
   highlight_duration(gui, 7);
  score_status(gui, TRUE);
 displayhelper(gui);
 }
}
static void InsertRest8(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_rest(gui, 8);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTREST;
  insert_chord_8key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeRest8(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTREST;
  insert_chord_8key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
void InsertDur8(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 8);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  insert_chord_8key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeDur8(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gui->mode = INPUTEDIT|INPUTNORMAL;
  insert_chord_8key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void SetDur8(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  highlight_duration(gui, 8);
//  displayhelper(gui);
}
static void Dur8  (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
 if(gui->mode&INPUTINSERT)
   highlight_duration(gui, 8);
 else 
 if( (!gui->mode&INPUTRHYTHM) && (gui->mode&INPUTEDIT) && (!gui->si->cursor_appending))
   ChangeDur8 (action, param);
else {
 insert_chord_8key(gui);
   highlight_duration(gui, 8);
  score_status(gui, TRUE);
 displayhelper(gui);
 }
}
static void ChangeToA(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL); 
  gint mode = gui->mode;
  gui->mode = INPUTEDIT|INPUTNORMAL;
  go_to_A_key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeToB(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL); 
  gint mode = gui->mode;
  gui->mode = INPUTEDIT|INPUTNORMAL;
  go_to_B_key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeToC(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL); 
  gint mode = gui->mode;
  gui->mode = INPUTEDIT|INPUTNORMAL;
  go_to_C_key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeToD(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL); 
  gint mode = gui->mode;
  gui->mode = INPUTEDIT|INPUTNORMAL;
  go_to_D_key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeToE(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL); 
  gint mode = gui->mode;
  gui->mode = INPUTEDIT|INPUTNORMAL;
  go_to_E_key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeToF(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL); 
  gint mode = gui->mode;
  gui->mode = INPUTEDIT|INPUTNORMAL;
  go_to_F_key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void ChangeToG(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gboolean appending = gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL); 
  gint mode = gui->mode;
  gui->mode = INPUTEDIT|INPUTNORMAL;
  go_to_G_key(gui);
  gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void MoveToA(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTCLASSIC|INPUTNORMAL;
  go_to_A_key(gui);
  gui->mode = mode;
  displayhelper(gui);
}
static void MoveToB(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTCLASSIC|INPUTNORMAL;
  go_to_B_key(gui);
  gui->mode = mode;
  displayhelper(gui);
}
static void MoveToC(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTCLASSIC|INPUTNORMAL;
  go_to_C_key(gui);
  gui->mode = mode;
  displayhelper(gui);
}
static void MoveToD(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTCLASSIC|INPUTNORMAL;
  go_to_D_key(gui);
  gui->mode = mode;
  displayhelper(gui);
}
static void MoveToE(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTCLASSIC|INPUTNORMAL;
  go_to_E_key(gui);
  gui->mode = mode;
  displayhelper(gui);
}
static void MoveToF(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTCLASSIC|INPUTNORMAL;
  go_to_F_key(gui);
  gui->mode = mode;
  displayhelper(gui);
}
static void MoveToG(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTCLASSIC|INPUTNORMAL;
  go_to_G_key(gui);
  gui->mode = mode;
  displayhelper(gui);
}
static void InsertA(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_A_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void InsertB(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_B_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void InsertC(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_C_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void InsertD(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_D_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void InsertE(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_E_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void InsertF(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_F_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void InsertG(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_G_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void AddNoteA(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  movecursorright(NULL);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_A_key(gui);
  gui->mode = mode;
  movecursorleft(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void AddNoteB(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  movecursorright(NULL);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_B_key(gui);
  gui->mode = mode;
  movecursorleft(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void AddNoteC(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  movecursorright(NULL);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_C_key(gui);
  gui->mode = mode;
  movecursorleft(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void AddNoteD(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  movecursorright(NULL);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_D_key(gui);
  gui->mode = mode;
  movecursorleft(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void AddNoteE(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  movecursorright(NULL);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_E_key(gui);
  gui->mode = mode;
  movecursorleft(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void AddNoteF(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  movecursorright(NULL);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_F_key(gui);
  gui->mode = mode;
  movecursorleft(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void AddNoteG(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  movecursorright(NULL);
  gint mode = gui->mode;
  gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_G_key(gui);
  gui->mode = mode;
  movecursorleft(NULL);
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void AddA(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTCLASSIC|INPUTNORMAL;
  go_to_A_key(gui);
  add_tone_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void AddB(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTCLASSIC|INPUTNORMAL;
  go_to_B_key(gui);
  add_tone_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void AddC(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTCLASSIC|INPUTNORMAL;
  go_to_C_key(gui);
  add_tone_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void AddD(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTCLASSIC|INPUTNORMAL;
  go_to_D_key(gui);
  add_tone_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void AddE(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTCLASSIC|INPUTNORMAL;
  go_to_E_key(gui);
  add_tone_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void AddF(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTCLASSIC|INPUTNORMAL;
  go_to_F_key(gui);
  add_tone_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void AddG(GtkAction *action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  gint mode = gui->mode;
  gui->mode = INPUTCLASSIC|INPUTNORMAL;
  go_to_G_key(gui);
  add_tone_key(gui);
  gui->mode = mode;
  score_status(gui, TRUE);
  displayhelper(gui);
}
