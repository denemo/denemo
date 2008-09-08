SCM scheme_CursorLeft (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
cursorleft_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_CursorDown (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
cursordown_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_CursorUp (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
cursorup_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_CursorRight (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
cursorright_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_StaffUp (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
staffup_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_StaffDown (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
staffdown_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_MeasureLeft (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
measureleft_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_MeasureRight (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
measureright_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_A (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
go_to_A_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_B (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
go_to_B_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_C (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
go_to_C_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_D (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
go_to_D_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_E (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
go_to_E_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_F (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
go_to_F_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_G (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
go_to_G_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_OctaveUp (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
octave_up_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_OctaveDown (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
octave_down_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_WholeNote (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_chord_0key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_HalfNote (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_chord_1key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_QuarterNote (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_chord_2key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_EighthNote (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_chord_3key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SixteenthNote (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_chord_4key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ThirtysecondNote (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_chord_5key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SixtyfourthNote (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_chord_6key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertBlankWholeNote (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_blankchord_0key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertBlankHalfNote (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_blankchord_1key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertBlankQuarterNote (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_blankchord_2key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertBlankEighthNote (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_blankchord_3key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertBlankSixteenthNote (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_blankchord_4key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertBlankThirtysecondNote (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_blankchord_5key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertBlankSixtyfourthNote (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_blankchord_6key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleRestMode (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
rest_toggle_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleBlankMode (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
toggle_blank_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertWholeRest (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_rest_0key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertHalfRest (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_rest_1key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertQuarterRest (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_rest_2key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertEighthRest (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_rest_3key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertSixteenthRest (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_rest_4key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertThirtysecondRest (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_rest_5key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertSixtyfourthRest (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_rest_6key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertDuplet (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_duplet_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertTriplet (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_triplet_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_StartTriplet (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
start_triplet_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_EndTuplet (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
end_tuplet_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertQuadtuplet (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_quadtuplet_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertQuintuplet (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_quintuplet_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertSextuplet (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_sextuplet_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertSeptuplet (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_septuplet_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_AddNoteToChord (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_tone_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_RemoveNoteFromChord (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
remove_tone_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SharpenOrStemDown (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
sharpen_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_FlattenOrStemUp (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
flatten_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_AddDot (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_dot_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_RemoveDot (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
remove_dot_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertTiedNote (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
tie_notes_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_DeleteObject (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
deleteobject_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_DeletePreviousObject (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
deletepreviousobject_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertMeasure (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_measure_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_AppendMeasure (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
append_measure_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_DeleteMeasure (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
deletemeasure_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_DeleteMeasureAllStaffs (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
deletemeasureallstaffs_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ShrinkMeasures (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
adjust_measure_less_width_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_WidenMeasures (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
adjust_measure_more_width_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ShorterStaffs (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
adjust_staff_less_height_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_TallerStaffs (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
adjust_staff_more_height_key_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertTrebleClef (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newcleftreble_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertBassClef (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newclefbass_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Insertg8clef (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newclefg8_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertAltoClef (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newclefalto_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertTenorClef (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newcleftenor_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertSopranoClef (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newclefsoprano_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialTrebleClef (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setcleftreble_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialBassClef (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setclefbass_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialg8clef (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setclefg8_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialAltoClef (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setclefalto_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialTenorClef (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setcleftenor_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialSopranoClef (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setclefsoprano_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Insert22Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newtimesig22_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Insert32Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newtimesig32_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Insert42Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newtimesig42_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Insert44Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newtimesig44_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Insert34Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newtimesig34_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Insert24Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newtimesig24_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Insert64Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newtimesig64_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Insert38Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newtimesig38_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Insert68Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newtimesig68_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Insert128Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newtimesig128_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Insert98Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newtimesig98_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Set22Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
settimesig22_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Set32Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
settimesig32_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Set42Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
settimesig42_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Set44Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
settimesig44_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Set34Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
settimesig34_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Set24Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
settimesig24_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Set64Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
settimesig64_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Set38Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
settimesig38_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Set68Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
settimesig68_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Set128Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
settimesig128_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Set98Time (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
settimesig98_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertCmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigcmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertGmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysiggmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertDmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigdmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertAmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigamaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertEmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigemaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertBmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigbmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertFSharpmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigfsharpmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertCSharpmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigcsharpmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertFmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigfmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertBflatmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigbflatmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertEflatmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigeflatmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertAflatmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigaflatmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertDflatmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigdflatmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertGflatmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysiggflatmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertCflatmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigcflatmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertAmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigamin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertEmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigemin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertBmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigbmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertFSharpmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigfsharpmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertCSharpmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigcsharpmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertGSharpmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysiggsharpmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertDSharpmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigdsharpmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertASharpmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigasharpmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertDmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigdmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertGmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysiggmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertCmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigcmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertFmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigfmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertBflatmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigbflatmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertEflatmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigeflatmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertAflatmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newkeysigaflatmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialCmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigcmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialGmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysiggmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialDmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigdmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialAmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigamaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialEmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigemaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialBmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigbmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialFSharpmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigfsharpmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialCSharpmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigcsharpmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialFmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigfmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialBflatmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigbflatmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialEflatmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigeflatmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialAflatmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigaflatmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialDflatmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigdflatmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialGflatmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysiggflatmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialCflatmaj (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigcflatmaj_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialAmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigamin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialEmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigemin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialBmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigbmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialFSharpmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigfsharpmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialCSharpmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigcsharpmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialGSharpmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysiggsharpmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialDSharpmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigdsharpmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialASharpmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigasharpmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialDmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigdmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialGmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysiggmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialCmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigcmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialFmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigfmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialBflatmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigbflatmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialEflatmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigeflatmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetInitialAflatmin (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
setkeysigaflatmin_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetMark (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
set_mark_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_UnsetMark (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
unset_mark_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleBeginSlur (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
toggle_begin_slur_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleEndSlur (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
toggle_end_slur_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleStartCrescendo (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
toggle_start_crescendo_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleEndCrescendo (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
toggle_end_crescendo_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleStartDiminuendo (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
toggle_start_diminuendo_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleEndDiminuendo (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
toggle_end_diminuendo_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleAccent (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_accent_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleFermata (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_fermata_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleStaccato (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_staccato_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleTenuto (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_tenuto_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleTrill (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_trill_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleTurn (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_turn_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleMordent (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_mordent_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleStaccatissimo (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_staccatissimo_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleCoda (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_coda_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleFlageolet (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_flageolet_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleOpen (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_open_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_TogglePrallMordent (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_prallmordent_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_TogglePrallPrall (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_prallprall_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_TogglePrall (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_prall_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleReverseTurn (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_reverseturn_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleSegno (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_segno_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleSforzato (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_sforzato_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleStopped (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_stopped_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleThumb (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_thumb_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleUpprall (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_upprall_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleArpeggio (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
add_arpeggio_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SetGrace (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
set_grace_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ForceCaution (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
force_cautionary_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ChangePitch (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
change_pitch_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_DoubleBar (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_doublebar_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_EndBar (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_endbar_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_OpenRepeat (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_openrepeat_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_CloseRepeat (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_closerepeat_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_OpenCloseRepeat (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_opencloserepeat_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertRhythm (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_rhythm_pattern_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_NextRhythm (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
nextrhythm_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_AppendMesauresToScore (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
append_measure_score_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_New (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
file_newwrapper (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Open (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
file_open_with_check (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_AddStaffs (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
file_add_staffs (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_AddMovements (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
file_add_movements (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_MovementProps (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
movement_props_dialog (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_OpenNewWindow (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
openinnew (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Save (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
file_savewrapper (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SaveAs (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
file_saveaswrapper (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_OpenTemplate (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
system_template_open_with_check (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_OpenExample (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
system_example_open_with_check (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_OpenMyTemplate (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
local_template_open_with_check (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SaveTemplate (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
template_save (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_NewWindow (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newview (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertMovementBefore (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_movement_before (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertMovementAfter (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_movement_after (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SaveParts (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
file_savepartswrapper (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ExportPDF (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
export_pdf_action (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ConfigureScore (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
scorewizard (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_PrintPreview (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
printpreview_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_PrintExcerptPreview (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
printexcerptpreview_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Print (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
printall_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_PrintPart (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
printpart_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Close (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
close_gui_with_check (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Quit (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
closewrapper (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Undo (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
undowrapper (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Redo (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
redowrapper (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Copy (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
copywrapper (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Cut (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
cutwrapper (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Paste (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
pastewrapper (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ScoreProperties (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
score_properties_dialog (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SaveSelection (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
saveselwrapper (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Preferences (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
preferences_change (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SaveAccels (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
save_default_keymap_file_wrapper (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_CommandManagement (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
configure_keyboard_dialog (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_LoadPlugins (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
load_plugin (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_UnloadPlugins (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
unloadplugins (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ListPlugins (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
list_loaded_plugins (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ListAvailablePlugins (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
list_available_plugins (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SwapStaffs (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
swapstaffs (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SplitVoices (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
splitstaffs (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_JoinVoices (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
joinstaffs (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_SwapMovements (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
swapmovements (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_VoiceUp (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
voiceup_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_VoiceDown (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
voicedown_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_AddBefore (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newstaffbefore (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_AddAfter (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
dnm_newstaffafter (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_AddInitial (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newstaffinitial (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_AddLast (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
newstafflast (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_DeleteBefore (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
delete_staff_before (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_DeleteStaff (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
delete_staff_current (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_DeleteAfter (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
delete_staff_after (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_AddVoice (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
dnm_newstaffvoice (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_TransposeStaff (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
staff_transposition (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_StaffProperties (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
staff_properties_change_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InitialClef (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
clef_change_initial (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertClef (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
clef_change_insert (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InitialKey (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
key_change_initial (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertKey (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
key_change_insert (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InitialTimeSig (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
timesig_change_initial (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertTimeSig (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
timesig_change_insert (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ChangeNotehead (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
set_notehead (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertStem (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
stem_directive_insert (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_EditLyric (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
lyric_insert (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_EditFiguredBass (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
figure_insert (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_EditChords (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
fakechord_insert (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertDynamic (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_dynamic (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertLilyDirective (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
lily_directive_insert (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertLilyPostfix (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
lily_directive_postfix (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_InsertBarline (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
insert_barline (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_GoToMeasure (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
tomeasurenum (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_GoToBeginning (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
tohome (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_GoToEnd (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
toend (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_NextMovement (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
next_movement (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_PreviousMovement (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
prev_movement (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_DeleteMovement (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
delete_movement (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_DeleteBookmarks (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
deletebookmarks (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Play (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
ext_midi_playback (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Stop (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
stop_midi_playback (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_PlayCSound (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
csoundplayback (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_PlaybackProperties (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
playback_properties_change (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_Help (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
browse_manual (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_About (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
about (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_AddBookmark (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
addbookmark (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_GotoBookmark (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
gotobookmark (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_NextBookmark (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
nextbookmark (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_PrevBookmark (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
prevbookmark (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleEdit (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
toggle_edit_mode (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleRest (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
toggle_rest_mode (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ToggleRhythm (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
toggle_rhythm_mode (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ClearOverlay (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
clear_overlay (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_CreateRhythm (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
create_rhythm_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_DeleteRhythm (SCM optional) {
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
delete_rhythm_cb (NULL, gstr);
if(gstr) g_string_free(gstr, TRUE);return SCM_EOL;
}
SCM scheme_ChangeToA (SCM optional) {
ChangeToA (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertA (SCM optional) {
InsertA (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeToB (SCM optional) {
ChangeToB (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertB (SCM optional) {
InsertB (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeToC (SCM optional) {
ChangeToC (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertC (SCM optional) {
InsertC (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeToD (SCM optional) {
ChangeToD (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertD (SCM optional) {
InsertD (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeToE (SCM optional) {
ChangeToE (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertE (SCM optional) {
InsertE (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeToF (SCM optional) {
ChangeToF (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertF (SCM optional) {
InsertF (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeToG (SCM optional) {
ChangeToG (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertG (SCM optional) {
InsertG (NULL, NULL);
return SCM_EOL;
}
SCM scheme_0 (SCM optional) {
Dur0 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertDur0 (SCM optional) {
InsertDur0 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeDur0 (SCM optional) {
ChangeDur0 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertRest0 (SCM optional) {
InsertRest0 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeRest0 (SCM optional) {
ChangeRest0 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_1 (SCM optional) {
Dur1 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertDur1 (SCM optional) {
InsertDur1 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeDur1 (SCM optional) {
ChangeDur1 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertRest1 (SCM optional) {
InsertRest1 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeRest1 (SCM optional) {
ChangeRest1 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_2 (SCM optional) {
Dur2 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertDur2 (SCM optional) {
InsertDur2 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeDur2 (SCM optional) {
ChangeDur2 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertRest2 (SCM optional) {
InsertRest2 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeRest2 (SCM optional) {
ChangeRest2 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_3 (SCM optional) {
Dur3 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertDur3 (SCM optional) {
InsertDur3 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeDur3 (SCM optional) {
ChangeDur3 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertRest3 (SCM optional) {
InsertRest3 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeRest3 (SCM optional) {
ChangeRest3 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_4 (SCM optional) {
Dur4 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertDur4 (SCM optional) {
InsertDur4 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeDur4 (SCM optional) {
ChangeDur4 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertRest4 (SCM optional) {
InsertRest4 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeRest4 (SCM optional) {
ChangeRest4 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_5 (SCM optional) {
Dur5 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertDur5 (SCM optional) {
InsertDur5 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeDur5 (SCM optional) {
ChangeDur5 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertRest5 (SCM optional) {
InsertRest5 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeRest5 (SCM optional) {
ChangeRest5 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_6 (SCM optional) {
Dur6 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertDur6 (SCM optional) {
InsertDur6 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeDur6 (SCM optional) {
ChangeDur6 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_InsertRest6 (SCM optional) {
InsertRest6 (NULL, NULL);
return SCM_EOL;
}
SCM scheme_ChangeRest6 (SCM optional) {
ChangeRest6 (NULL, NULL);
return SCM_EOL;
}
