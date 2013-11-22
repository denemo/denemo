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
static void go_to_A_key_cb (GtkAction *action, DenemoScriptParam *param) {
  go_to_A_key (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*B go_to_B_key*/
static void go_to_B_key_cb (GtkAction *action, DenemoScriptParam *param) {
  go_to_B_key (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*C go_to_C_key*/
static void go_to_C_key_cb (GtkAction *action, DenemoScriptParam *param) {
  go_to_C_key (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*D go_to_D_key*/
static void go_to_D_key_cb (GtkAction *action, DenemoScriptParam *param) {
  go_to_D_key (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*E go_to_E_key*/
static void go_to_E_key_cb (GtkAction *action, DenemoScriptParam *param) {
  go_to_E_key (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*F go_to_F_key*/
static void go_to_F_key_cb (GtkAction *action, DenemoScriptParam *param) {
  go_to_F_key (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*G go_to_G_key*/
static void go_to_G_key_cb (GtkAction *action, DenemoScriptParam *param) {
  go_to_G_key (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*OctaveUp octave_up_key*/
static void octave_up_key_cb (GtkAction *action, DenemoScriptParam *param) {
  octave_up_key (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*OctaveDown octave_down_key*/
static void octave_down_key_cb (GtkAction *action, DenemoScriptParam *param) {
  octave_down_key (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*WholeNote insert_chord_0key*/
static void insert_chord_0key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_chord_0key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*HalfNote insert_chord_1key*/
static void insert_chord_1key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_chord_1key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*QuarterNote insert_chord_2key*/
static void insert_chord_2key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_chord_2key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*EighthNote insert_chord_3key*/
static void insert_chord_3key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_chord_3key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SixteenthNote insert_chord_4key*/
static void insert_chord_4key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_chord_4key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*ThirtysecondNote insert_chord_5key*/
static void insert_chord_5key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_chord_5key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SixtyfourthNote insert_chord_6key*/
static void insert_chord_6key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_chord_6key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*OneHundredTwentyEighthNote insert_chord_7key*/
static void insert_chord_7key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_chord_7key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*TwoHundredFiftySixthNote insert_chord_8key*/
static void insert_chord_8key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_chord_8key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertWholeRest insert_rest_0key*/
static void insert_rest_0key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_rest_0key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertHalfRest insert_rest_1key*/
static void insert_rest_1key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_rest_1key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertQuarterRest insert_rest_2key*/
static void insert_rest_2key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_rest_2key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertEighthRest insert_rest_3key*/
static void insert_rest_3key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_rest_3key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertSixteenthRest insert_rest_4key*/
static void insert_rest_4key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_rest_4key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertThirtysecondRest insert_rest_5key*/
static void insert_rest_5key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_rest_5key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertSixtyfourthRest insert_rest_6key*/
static void insert_rest_6key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_rest_6key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertBlankWholeNote insert_blankchord_0key*/
static void insert_blankchord_0key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_blankchord_0key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertBlankHalfNote insert_blankchord_1key*/
static void insert_blankchord_1key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_blankchord_1key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertBlankQuarterNote insert_blankchord_2key*/
static void insert_blankchord_2key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_blankchord_2key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertBlankEighthNote insert_blankchord_3key*/
static void insert_blankchord_3key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_blankchord_3key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertBlankSixteenthNote insert_blankchord_4key*/
static void insert_blankchord_4key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_blankchord_4key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertBlankThirtysecondNote insert_blankchord_5key*/
static void insert_blankchord_5key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_blankchord_5key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertBlankSixtyfourthNote insert_blankchord_6key*/
static void insert_blankchord_6key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_blankchord_6key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertBlankOneHundredTwentyEighthNote insert_blankchord_7key*/
static void insert_blankchord_7key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_blankchord_7key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertBlankTwoHundredFiftySixthNote insert_blankchord_8key*/
static void insert_blankchord_8key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_blankchord_8key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*ToggleRestMode rest_toggle_key*/
static void rest_toggle_key_cb (GtkAction *action, DenemoScriptParam *param) {
  rest_toggle_key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*ToggleBlankMode toggle_blank*/
static void toggle_blank_cb (GtkAction *action, DenemoScriptParam *param) {
  toggle_blank (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertDuplet insert_duplet*/
static void insert_duplet_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_duplet (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertTriplet insert_triplet*/
static void insert_triplet_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_triplet (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*StartTriplet start_triplet*/
static void start_triplet_cb (GtkAction *action, DenemoScriptParam *param) {
  start_triplet (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*EndTuplet end_tuplet*/
static void end_tuplet_cb (GtkAction *action, DenemoScriptParam *param) {
  end_tuplet (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertQuadtuplet insert_quadtuplet*/
static void insert_quadtuplet_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_quadtuplet (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertQuintuplet insert_quintuplet*/
static void insert_quintuplet_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_quintuplet (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertSextuplet insert_sextuplet*/
static void insert_sextuplet_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_sextuplet (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertSeptuplet insert_septuplet*/
static void insert_septuplet_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_septuplet (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*AddNoteToChord add_tone_key*/
static void add_tone_key_cb (GtkAction *action, DenemoScriptParam *param) {
  add_tone_key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*RemoveNoteFromChord remove_tone_key*/
static void remove_tone_key_cb (GtkAction *action, DenemoScriptParam *param) {
  remove_tone_key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Sharpen sharpen_key*/
static void sharpen_key_cb (GtkAction *action, DenemoScriptParam *param) {
  sharpen_key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Flatten flatten_key*/
static void flatten_key_cb (GtkAction *action, DenemoScriptParam *param) {
  flatten_key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*PendingSharpen pending_sharpen*/
static void pending_sharpen_cb (GtkAction *action, DenemoScriptParam *param) {
  pending_sharpen (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*PendingFlatten pending_flatten*/
static void pending_flatten_cb (GtkAction *action, DenemoScriptParam *param) {
  pending_flatten (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*StemUp stem_up*/
static void stem_up_cb (GtkAction *action, DenemoScriptParam *param) {
  stem_up (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*StemDown stem_down*/
static void stem_down_cb (GtkAction *action, DenemoScriptParam *param) {
  stem_down (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*AddDot add_dot_key*/
static void add_dot_key_cb (GtkAction *action, DenemoScriptParam *param) {
  add_dot_key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*RemoveDot remove_dot_key*/
static void remove_dot_key_cb (GtkAction *action, DenemoScriptParam *param) {
  remove_dot_key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertTiedNote tie_notes_key*/
static void tie_notes_key_cb (GtkAction *action, DenemoScriptParam *param) {
  tie_notes_key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*DeleteObject deleteobject*/
static void deleteobject_cb (GtkAction *action, DenemoScriptParam *param) {
  deleteobject (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*DeletePreviousObject deletepreviousobject*/
static void deletepreviousobject_cb (GtkAction *action, DenemoScriptParam *param) {
  deletepreviousobject (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertMeasure insert_measure_key*/
static void insert_measure_key_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_measure_key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*AddMeasure addmeasureafter*/
static void addmeasureafter_cb (GtkAction *action, DenemoScriptParam *param) {
  addmeasureafter (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertMeasureBefore insertmeasurebefore*/
static void insertmeasurebefore_cb (GtkAction *action, DenemoScriptParam *param) {
  insertmeasurebefore (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertMeasureAfter insertmeasureafter*/
static void insertmeasureafter_cb (GtkAction *action, DenemoScriptParam *param) {
  insertmeasureafter (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*AppendMeasure append_measure_key*/
static void append_measure_key_cb (GtkAction *action, DenemoScriptParam *param) {
  append_measure_key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*DeleteMeasure deletemeasure*/
static void deletemeasure_cb (GtkAction *action, DenemoScriptParam *param) {
  deletemeasure (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*DeleteMeasureAllStaffs deletemeasureallstaffs*/
static void deletemeasureallstaffs_cb (GtkAction *action, DenemoScriptParam *param) {
  deletemeasureallstaffs (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*ShrinkMeasures adjust_measure_less_width_key*/
static void adjust_measure_less_width_key_cb (GtkAction *action, DenemoScriptParam *param) {
  adjust_measure_less_width_key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*WidenMeasures adjust_measure_more_width_key*/
static void adjust_measure_more_width_key_cb (GtkAction *action, DenemoScriptParam *param) {
  adjust_measure_more_width_key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*ShorterStaffs adjust_staff_less_height_key*/
static void adjust_staff_less_height_key_cb (GtkAction *action, DenemoScriptParam *param) {
  adjust_staff_less_height_key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*TallerStaffs adjust_staff_more_height_key*/
static void adjust_staff_more_height_key_cb (GtkAction *action, DenemoScriptParam *param) {
  adjust_staff_more_height_key (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertTrebleClef newcleftreble*/
static void newcleftreble_cb (GtkAction *action, DenemoScriptParam *param) {
  newcleftreble (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertBassClef newclefbass*/
static void newclefbass_cb (GtkAction *action, DenemoScriptParam *param) {
  newclefbass (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Insertg8clef newclefg8*/
static void newclefg8_cb (GtkAction *action, DenemoScriptParam *param) {
  newclefg8 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertAltoClef newclefalto*/
static void newclefalto_cb (GtkAction *action, DenemoScriptParam *param) {
  newclefalto (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertTenorClef newcleftenor*/
static void newcleftenor_cb (GtkAction *action, DenemoScriptParam *param) {
  newcleftenor (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertSopranoClef newclefsoprano*/
static void newclefsoprano_cb (GtkAction *action, DenemoScriptParam *param) {
  newclefsoprano (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialTrebleClef setcleftreble*/
static void setcleftreble_cb (GtkAction *action, DenemoScriptParam *param) {
  setcleftreble (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialBassClef setclefbass*/
static void setclefbass_cb (GtkAction *action, DenemoScriptParam *param) {
  setclefbass (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialg8clef setclefg8*/
static void setclefg8_cb (GtkAction *action, DenemoScriptParam *param) {
  setclefg8 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialAltoClef setclefalto*/
static void setclefalto_cb (GtkAction *action, DenemoScriptParam *param) {
  setclefalto (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialTenorClef setcleftenor*/
static void setcleftenor_cb (GtkAction *action, DenemoScriptParam *param) {
  setcleftenor (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialSopranoClef setclefsoprano*/
static void setclefsoprano_cb (GtkAction *action, DenemoScriptParam *param) {
  setclefsoprano (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Insert22Time newtimesig22*/
static void newtimesig22_cb (GtkAction *action, DenemoScriptParam *param) {
  newtimesig22 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Insert32Time newtimesig32*/
static void newtimesig32_cb (GtkAction *action, DenemoScriptParam *param) {
  newtimesig32 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Insert42Time newtimesig42*/
static void newtimesig42_cb (GtkAction *action, DenemoScriptParam *param) {
  newtimesig42 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Insert44Time newtimesig44*/
static void newtimesig44_cb (GtkAction *action, DenemoScriptParam *param) {
  newtimesig44 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Insert34Time newtimesig34*/
static void newtimesig34_cb (GtkAction *action, DenemoScriptParam *param) {
  newtimesig34 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Insert24Time newtimesig24*/
static void newtimesig24_cb (GtkAction *action, DenemoScriptParam *param) {
  newtimesig24 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Insert64Time newtimesig64*/
static void newtimesig64_cb (GtkAction *action, DenemoScriptParam *param) {
  newtimesig64 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Insert38Time newtimesig38*/
static void newtimesig38_cb (GtkAction *action, DenemoScriptParam *param) {
  newtimesig38 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Insert68Time newtimesig68*/
static void newtimesig68_cb (GtkAction *action, DenemoScriptParam *param) {
  newtimesig68 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Insert128Time newtimesig128*/
static void newtimesig128_cb (GtkAction *action, DenemoScriptParam *param) {
  newtimesig128 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Insert98Time newtimesig98*/
static void newtimesig98_cb (GtkAction *action, DenemoScriptParam *param) {
  newtimesig98 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Set22Time settimesig22*/
static void settimesig22_cb (GtkAction *action, DenemoScriptParam *param) {
  settimesig22 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Set32Time settimesig32*/
static void settimesig32_cb (GtkAction *action, DenemoScriptParam *param) {
  settimesig32 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Set42Time settimesig42*/
static void settimesig42_cb (GtkAction *action, DenemoScriptParam *param) {
  settimesig42 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Set44Time settimesig44*/
static void settimesig44_cb (GtkAction *action, DenemoScriptParam *param) {
  settimesig44 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Set34Time settimesig34*/
static void settimesig34_cb (GtkAction *action, DenemoScriptParam *param) {
  settimesig34 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Set24Time settimesig24*/
static void settimesig24_cb (GtkAction *action, DenemoScriptParam *param) {
  settimesig24 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Set64Time settimesig64*/
static void settimesig64_cb (GtkAction *action, DenemoScriptParam *param) {
  settimesig64 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Set38Time settimesig38*/
static void settimesig38_cb (GtkAction *action, DenemoScriptParam *param) {
  settimesig38 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Set68Time settimesig68*/
static void settimesig68_cb (GtkAction *action, DenemoScriptParam *param) {
  settimesig68 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Set128Time settimesig128*/
static void settimesig128_cb (GtkAction *action, DenemoScriptParam *param) {
  settimesig128 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*Set98Time settimesig98*/
static void settimesig98_cb (GtkAction *action, DenemoScriptParam *param) {
  settimesig98 (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertCmaj newkeysigcmaj*/
static void newkeysigcmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigcmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertGmaj newkeysiggmaj*/
static void newkeysiggmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysiggmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertDmaj newkeysigdmaj*/
static void newkeysigdmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigdmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertAmaj newkeysigamaj*/
static void newkeysigamaj_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigamaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertEmaj newkeysigemaj*/
static void newkeysigemaj_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigemaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertBmaj newkeysigbmaj*/
static void newkeysigbmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigbmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertFSharpmaj newkeysigfsharpmaj*/
static void newkeysigfsharpmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigfsharpmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertCSharpmaj newkeysigcsharpmaj*/
static void newkeysigcsharpmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigcsharpmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertFmaj newkeysigfmaj*/
static void newkeysigfmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigfmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertBflatmaj newkeysigbflatmaj*/
static void newkeysigbflatmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigbflatmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertEflatmaj newkeysigeflatmaj*/
static void newkeysigeflatmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigeflatmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertAflatmaj newkeysigaflatmaj*/
static void newkeysigaflatmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigaflatmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertDflatmaj newkeysigdflatmaj*/
static void newkeysigdflatmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigdflatmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertGflatmaj newkeysiggflatmaj*/
static void newkeysiggflatmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysiggflatmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertCflatmaj newkeysigcflatmaj*/
static void newkeysigcflatmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigcflatmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertAmin newkeysigamin*/
static void newkeysigamin_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigamin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertEmin newkeysigemin*/
static void newkeysigemin_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigemin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertBmin newkeysigbmin*/
static void newkeysigbmin_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigbmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertFSharpmin newkeysigfsharpmin*/
static void newkeysigfsharpmin_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigfsharpmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertCSharpmin newkeysigcsharpmin*/
static void newkeysigcsharpmin_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigcsharpmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertGSharpmin newkeysiggsharpmin*/
static void newkeysiggsharpmin_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysiggsharpmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertDSharpmin newkeysigdsharpmin*/
static void newkeysigdsharpmin_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigdsharpmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertASharpmin newkeysigasharpmin*/
static void newkeysigasharpmin_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigasharpmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertDmin newkeysigdmin*/
static void newkeysigdmin_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigdmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertGmin newkeysiggmin*/
static void newkeysiggmin_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysiggmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertCmin newkeysigcmin*/
static void newkeysigcmin_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigcmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertFmin newkeysigfmin*/
static void newkeysigfmin_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigfmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertBflatmin newkeysigbflatmin*/
static void newkeysigbflatmin_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigbflatmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertEflatmin newkeysigeflatmin*/
static void newkeysigeflatmin_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigeflatmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertAflatmin newkeysigaflatmin*/
static void newkeysigaflatmin_cb (GtkAction *action, DenemoScriptParam *param) {
  newkeysigaflatmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialCmaj setkeysigcmaj*/
static void setkeysigcmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigcmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialGmaj setkeysiggmaj*/
static void setkeysiggmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysiggmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialDmaj setkeysigdmaj*/
static void setkeysigdmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigdmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialAmaj setkeysigamaj*/
static void setkeysigamaj_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigamaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialEmaj setkeysigemaj*/
static void setkeysigemaj_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigemaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialBmaj setkeysigbmaj*/
static void setkeysigbmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigbmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialFSharpmaj setkeysigfsharpmaj*/
static void setkeysigfsharpmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigfsharpmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialCSharpmaj setkeysigcsharpmaj*/
static void setkeysigcsharpmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigcsharpmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialFmaj setkeysigfmaj*/
static void setkeysigfmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigfmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialBflatmaj setkeysigbflatmaj*/
static void setkeysigbflatmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigbflatmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialEflatmaj setkeysigeflatmaj*/
static void setkeysigeflatmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigeflatmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialAflatmaj setkeysigaflatmaj*/
static void setkeysigaflatmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigaflatmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialDflatmaj setkeysigdflatmaj*/
static void setkeysigdflatmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigdflatmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialGflatmaj setkeysiggflatmaj*/
static void setkeysiggflatmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysiggflatmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialCflatmaj setkeysigcflatmaj*/
static void setkeysigcflatmaj_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigcflatmaj (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialAmin setkeysigamin*/
static void setkeysigamin_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigamin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialEmin setkeysigemin*/
static void setkeysigemin_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigemin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialBmin setkeysigbmin*/
static void setkeysigbmin_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigbmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialFSharpmin setkeysigfsharpmin*/
static void setkeysigfsharpmin_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigfsharpmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialCSharpmin setkeysigcsharpmin*/
static void setkeysigcsharpmin_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigcsharpmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialGSharpmin setkeysiggsharpmin*/
static void setkeysiggsharpmin_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysiggsharpmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialDSharpmin setkeysigdsharpmin*/
static void setkeysigdsharpmin_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigdsharpmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialASharpmin setkeysigasharpmin*/
static void setkeysigasharpmin_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigasharpmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialDmin setkeysigdmin*/
static void setkeysigdmin_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigdmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialGmin setkeysiggmin*/
static void setkeysiggmin_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysiggmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialCmin setkeysigcmin*/
static void setkeysigcmin_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigcmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialFmin setkeysigfmin*/
static void setkeysigfmin_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigfmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialBflatmin setkeysigbflatmin*/
static void setkeysigbflatmin_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigbflatmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialEflatmin setkeysigeflatmin*/
static void setkeysigeflatmin_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigeflatmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetInitialAflatmin setkeysigaflatmin*/
static void setkeysigaflatmin_cb (GtkAction *action, DenemoScriptParam *param) {
  setkeysigaflatmin (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*SetMark set_mark*/
static void set_mark_cb (GtkAction *action, DenemoScriptParam *param) {
  set_mark (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*UnsetMark unset_mark*/
static void unset_mark_cb (GtkAction *action, DenemoScriptParam *param) {
  unset_mark (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*SetPoint set_point*/
static void set_point_cb (GtkAction *action, DenemoScriptParam *param) {
  set_point (param);
gtk_widget_queue_draw(Denemo.scorearea);
}
/*ToggleBeginSlur toggle_begin_slur*/
static void toggle_begin_slur_cb (GtkAction *action, DenemoScriptParam *param) {
  toggle_begin_slur (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*ToggleEndSlur toggle_end_slur*/
static void toggle_end_slur_cb (GtkAction *action, DenemoScriptParam *param) {
  toggle_end_slur (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*ToggleStartCrescendo toggle_start_crescendo*/
static void toggle_start_crescendo_cb (GtkAction *action, DenemoScriptParam *param) {
  toggle_start_crescendo (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*ToggleEndCrescendo toggle_end_crescendo*/
static void toggle_end_crescendo_cb (GtkAction *action, DenemoScriptParam *param) {
  toggle_end_crescendo (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*ToggleStartDiminuendo toggle_start_diminuendo*/
static void toggle_start_diminuendo_cb (GtkAction *action, DenemoScriptParam *param) {
  toggle_start_diminuendo (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*ToggleEndDiminuendo toggle_end_diminuendo*/
static void toggle_end_diminuendo_cb (GtkAction *action, DenemoScriptParam *param) {
  toggle_end_diminuendo (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*ForceCaution force_cautionary*/
static void force_cautionary_cb (GtkAction *action, DenemoScriptParam *param) {
  force_cautionary (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*ChangePitch change_pitch*/
static void change_pitch_cb (GtkAction *action, DenemoScriptParam *param) {
  change_pitch (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*InsertRhythm insert_rhythm_pattern*/
static void insert_rhythm_pattern_cb (GtkAction *action, DenemoScriptParam *param) {
  insert_rhythm_pattern (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*NextRhythm nextrhythm*/
static void nextrhythm_cb (GtkAction *action, DenemoScriptParam *param) {
  nextrhythm (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}
/*AppendMeasureAllStaffs append_measure_score*/
static void append_measure_score_cb (GtkAction *action, DenemoScriptParam *param) {
  append_measure_score (param);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
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
