SCM scheme_CursorLeft (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
cursorleft_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_CursorDown (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
cursordown_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_CursorUp (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
cursorup_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_CursorRight (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
cursorright_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_StaffUp (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
staffup_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_StaffDown (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
staffdown_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_MeasureLeft (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
measureleft_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_MeasureRight (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
measureright_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_A (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
go_to_A_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_B (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
go_to_B_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_C (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
go_to_C_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_D (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
go_to_D_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_E (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
go_to_E_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_F (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
go_to_F_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_G (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
go_to_G_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_OctaveUp (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
octave_up_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_OctaveDown (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
octave_down_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_WholeNote (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_chord_0key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_HalfNote (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_chord_1key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_QuarterNote (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_chord_2key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_EighthNote (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_chord_3key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SixteenthNote (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_chord_4key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ThirtysecondNote (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_chord_5key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SixtyfourthNote (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_chord_6key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertBlankWholeNote (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_blankchord_0key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertBlankHalfNote (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_blankchord_1key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertBlankQuarterNote (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_blankchord_2key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertBlankEighthNote (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_blankchord_3key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertBlankSixteenthNote (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_blankchord_4key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertBlankThirtysecondNote (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_blankchord_5key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertBlankSixtyfourthNote (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_blankchord_6key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleRestMode (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
rest_toggle_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleBlankMode (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
toggle_blank_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertWholeRest (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_rest_0key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertHalfRest (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_rest_1key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertQuarterRest (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_rest_2key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertEighthRest (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_rest_3key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertSixteenthRest (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_rest_4key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertThirtysecondRest (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_rest_5key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertSixtyfourthRest (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_rest_6key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertDuplet (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_duplet_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertTriplet (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_triplet_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_StartTriplet (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
start_triplet_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_EndTuplet (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
end_tuplet_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertQuadtuplet (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_quadtuplet_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertQuintuplet (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_quintuplet_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertSextuplet (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_sextuplet_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertSeptuplet (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_septuplet_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_AddNoteToChord (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_tone_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_RemoveNoteFromChord (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
remove_tone_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SharpenOrStemDown (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
sharpen_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_FlattenOrStemUp (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
flatten_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_AddDot (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_dot_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_RemoveDot (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
remove_dot_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertTiedNote (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
tie_notes_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_DeleteObject (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
deleteobject_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_DeletePreviousObject (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
deletepreviousobject_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertMeasure (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_measure_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_AppendMeasure (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
append_measure_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_DeleteMeasure (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
deletemeasure_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_DeleteMeasureAllStaffs (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
deletemeasureallstaffs_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ShrinkMeasures (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
adjust_measure_less_width_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_WidenMeasures (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
adjust_measure_more_width_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ShorterStaffs (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
adjust_staff_less_height_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_TallerStaffs (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
adjust_staff_more_height_key_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertTrebleClef (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newcleftreble_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertBassClef (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newclefbass_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Insertg8clef (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newclefg8_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertAltoClef (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newclefalto_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertTenorClef (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newcleftenor_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertSopranoClef (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newclefsoprano_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialTrebleClef (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setcleftreble_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialBassClef (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setclefbass_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialg8clef (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setclefg8_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialAltoClef (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setclefalto_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialTenorClef (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setcleftenor_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialSopranoClef (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setclefsoprano_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Insert22Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newtimesig22_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Insert32Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newtimesig32_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Insert42Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newtimesig42_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Insert44Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newtimesig44_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Insert34Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newtimesig34_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Insert24Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newtimesig24_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Insert64Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newtimesig64_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Insert38Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newtimesig38_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Insert68Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newtimesig68_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Insert128Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newtimesig128_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Insert98Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newtimesig98_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Set22Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
settimesig22_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Set32Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
settimesig32_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Set42Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
settimesig42_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Set44Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
settimesig44_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Set34Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
settimesig34_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Set24Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
settimesig24_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Set64Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
settimesig64_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Set38Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
settimesig38_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Set68Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
settimesig68_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Set128Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
settimesig128_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Set98Time (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
settimesig98_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertCmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigcmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertGmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysiggmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertDmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigdmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertAmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigamaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertEmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigemaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertBmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigbmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertFSharpmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigfsharpmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertCSharpmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigcsharpmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertFmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigfmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertBflatmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigbflatmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertEflatmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigeflatmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertAflatmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigaflatmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertDflatmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigdflatmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertGflatmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysiggflatmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertCflatmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigcflatmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertAmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigamin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertEmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigemin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertBmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigbmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertFSharpmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigfsharpmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertCSharpmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigcsharpmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertGSharpmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysiggsharpmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertDSharpmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigdsharpmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertASharpmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigasharpmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertDmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigdmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertGmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysiggmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertCmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigcmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertFmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigfmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertBflatmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigbflatmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertEflatmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigeflatmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertAflatmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newkeysigaflatmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialCmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigcmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialGmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysiggmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialDmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigdmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialAmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigamaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialEmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigemaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialBmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigbmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialFSharpmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigfsharpmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialCSharpmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigcsharpmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialFmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigfmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialBflatmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigbflatmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialEflatmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigeflatmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialAflatmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigaflatmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialDflatmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigdflatmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialGflatmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysiggflatmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialCflatmaj (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigcflatmaj_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialAmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigamin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialEmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigemin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialBmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigbmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialFSharpmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigfsharpmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialCSharpmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigcsharpmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialGSharpmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysiggsharpmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialDSharpmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigdsharpmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialASharpmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigasharpmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialDmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigdmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialGmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysiggmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialCmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigcmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialFmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigfmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialBflatmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigbflatmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialEflatmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigeflatmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetInitialAflatmin (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
setkeysigaflatmin_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetMark (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
set_mark_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_UnsetMark (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
unset_mark_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleBeginSlur (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
toggle_begin_slur_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleEndSlur (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
toggle_end_slur_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleStartCrescendo (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
toggle_start_crescendo_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleEndCrescendo (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
toggle_end_crescendo_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleStartDiminuendo (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
toggle_start_diminuendo_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleEndDiminuendo (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
toggle_end_diminuendo_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleAccent (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_accent_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleFermata (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_fermata_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleStaccato (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_staccato_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleTenuto (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_tenuto_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleTrill (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_trill_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleTurn (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_turn_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleMordent (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_mordent_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleStaccatissimo (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_staccatissimo_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleCoda (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_coda_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleFlageolet (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_flageolet_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleOpen (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_open_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_TogglePrallMordent (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_prallmordent_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_TogglePrallPrall (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_prallprall_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_TogglePrall (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_prall_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleReverseTurn (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_reverseturn_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleSegno (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_segno_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleSforzato (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_sforzato_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleStopped (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_stopped_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleThumb (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_thumb_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleUpprall (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_upprall_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleArpeggio (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
add_arpeggio_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_SetGrace (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
set_grace_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ForceCaution (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
force_cautionary_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_ChangePitch (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
change_pitch_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_DoubleBar (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_doublebar_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_EndBar (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_endbar_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_OpenRepeat (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_openrepeat_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_CloseRepeat (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_closerepeat_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_OpenCloseRepeat (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_opencloserepeat_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertRhythm (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_rhythm_pattern_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_NextRhythm (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
nextrhythm_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_AppendMesauresToScore (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
append_measure_score_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_New (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
file_newwrapper (NULL, str);
return SCM_EOL;
}
SCM scheme_Open (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
file_open_with_check (NULL, str);
return SCM_EOL;
}
SCM scheme_AddStaffs (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
file_add_staffs (NULL, str);
return SCM_EOL;
}
SCM scheme_AddMovements (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
file_add_movements (NULL, str);
return SCM_EOL;
}
SCM scheme_MovementProps (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
movement_props_dialog (NULL, str);
return SCM_EOL;
}
SCM scheme_OpenNewWindow (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
openinnew (NULL, str);
return SCM_EOL;
}
SCM scheme_Save (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
file_savewrapper (NULL, str);
return SCM_EOL;
}
SCM scheme_SaveAs (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
file_saveaswrapper (NULL, str);
return SCM_EOL;
}
SCM scheme_OpenTemplate (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
system_template_open_with_check (NULL, str);
return SCM_EOL;
}
SCM scheme_OpenExample (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
system_example_open_with_check (NULL, str);
return SCM_EOL;
}
SCM scheme_OpenMyTemplate (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
local_template_open_with_check (NULL, str);
return SCM_EOL;
}
SCM scheme_SaveTemplate (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
template_save (NULL, str);
return SCM_EOL;
}
SCM scheme_NewWindow (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newview (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertMovementBefore (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_movement_before (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertMovementAfter (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_movement_after (NULL, str);
return SCM_EOL;
}
SCM scheme_SaveParts (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
file_savepartswrapper (NULL, str);
return SCM_EOL;
}
SCM scheme_ExportPDF (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
export_pdf_action (NULL, str);
return SCM_EOL;
}
SCM scheme_ConfigureScore (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
scorewizard (NULL, str);
return SCM_EOL;
}
SCM scheme_PrintPreview (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
printpreview_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_PrintExcerptPreview (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
printexcerptpreview_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Print (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
printall_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_PrintPart (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
printpart_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_Close (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
close_gui_with_check (NULL, str);
return SCM_EOL;
}
SCM scheme_Quit (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
closewrapper (NULL, str);
return SCM_EOL;
}
SCM scheme_Undo (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
undowrapper (NULL, str);
return SCM_EOL;
}
SCM scheme_Redo (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
redowrapper (NULL, str);
return SCM_EOL;
}
SCM scheme_Copy (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
copywrapper (NULL, str);
return SCM_EOL;
}
SCM scheme_Cut (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
cutwrapper (NULL, str);
return SCM_EOL;
}
SCM scheme_Paste (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
pastewrapper (NULL, str);
return SCM_EOL;
}
SCM scheme_ScoreProperties (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
score_properties_dialog (NULL, str);
return SCM_EOL;
}
SCM scheme_SaveSelection (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
saveselwrapper (NULL, str);
return SCM_EOL;
}
SCM scheme_Preferences (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
preferences_change (NULL, str);
return SCM_EOL;
}
SCM scheme_SaveAccels (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
save_default_keymap_file_wrapper (NULL, str);
return SCM_EOL;
}
SCM scheme_CommandManagement (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
configure_keyboard_dialog (NULL, str);
return SCM_EOL;
}
SCM scheme_LoadPlugins (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
load_plugin (NULL, str);
return SCM_EOL;
}
SCM scheme_UnloadPlugins (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
unloadplugins (NULL, str);
return SCM_EOL;
}
SCM scheme_ListPlugins (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
list_loaded_plugins (NULL, str);
return SCM_EOL;
}
SCM scheme_ListAvailablePlugins (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
list_available_plugins (NULL, str);
return SCM_EOL;
}
SCM scheme_SwapStaffs (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
swapstaffs (NULL, str);
return SCM_EOL;
}
SCM scheme_SplitVoices (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
splitstaffs (NULL, str);
return SCM_EOL;
}
SCM scheme_JoinVoices (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
joinstaffs (NULL, str);
return SCM_EOL;
}
SCM scheme_SwapMovements (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
swapmovements (NULL, str);
return SCM_EOL;
}
SCM scheme_VoiceUp (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
voiceup_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_VoiceDown (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
voicedown_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_AddBefore (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newstaffbefore (NULL, str);
return SCM_EOL;
}
SCM scheme_AddAfter (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
dnm_newstaffafter (NULL, str);
return SCM_EOL;
}
SCM scheme_AddInitial (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newstaffinitial (NULL, str);
return SCM_EOL;
}
SCM scheme_AddLast (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
newstafflast (NULL, str);
return SCM_EOL;
}
SCM scheme_DeleteBefore (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
delete_staff_before (NULL, str);
return SCM_EOL;
}
SCM scheme_DeleteStaff (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
delete_staff_current (NULL, str);
return SCM_EOL;
}
SCM scheme_DeleteAfter (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
delete_staff_after (NULL, str);
return SCM_EOL;
}
SCM scheme_AddVoice (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
dnm_newstaffvoice (NULL, str);
return SCM_EOL;
}
SCM scheme_TransposeStaff (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
staff_transposition (NULL, str);
return SCM_EOL;
}
SCM scheme_StaffProperties (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
staff_properties_change_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_InitialClef (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
clef_change_initial (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertClef (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
clef_change_insert (NULL, str);
return SCM_EOL;
}
SCM scheme_InitialKey (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
key_change_initial (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertKey (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
key_change_insert (NULL, str);
return SCM_EOL;
}
SCM scheme_InitialTimeSig (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
timesig_change_initial (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertTimeSig (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
timesig_change_insert (NULL, str);
return SCM_EOL;
}
SCM scheme_ChangeNotehead (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
set_notehead (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertStem (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
stem_directive_insert (NULL, str);
return SCM_EOL;
}
SCM scheme_EditLyric (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
lyric_insert (NULL, str);
return SCM_EOL;
}
SCM scheme_EditFiguredBass (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
figure_insert (NULL, str);
return SCM_EOL;
}
SCM scheme_EditChords (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
fakechord_insert (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertDynamic (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_dynamic (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertLilyDirective (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
lily_directive_insert (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertLilyPostfix (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
lily_directive_postfix (NULL, str);
return SCM_EOL;
}
SCM scheme_InsertBarline (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
insert_barline (NULL, str);
return SCM_EOL;
}
SCM scheme_GoToMeasure (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
tomeasurenum (NULL, str);
return SCM_EOL;
}
SCM scheme_GoToBeginning (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
tohome (NULL, str);
return SCM_EOL;
}
SCM scheme_GoToEnd (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
toend (NULL, str);
return SCM_EOL;
}
SCM scheme_NextMovement (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
next_movement (NULL, str);
return SCM_EOL;
}
SCM scheme_PreviousMovement (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
prev_movement (NULL, str);
return SCM_EOL;
}
SCM scheme_DeleteMovement (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
delete_movement (NULL, str);
return SCM_EOL;
}
SCM scheme_DeleteBookmarks (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
deletebookmarks (NULL, str);
return SCM_EOL;
}
SCM scheme_Play (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
ext_midi_playback (NULL, str);
return SCM_EOL;
}
SCM scheme_Stop (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
stop_midi_playback (NULL, str);
return SCM_EOL;
}
SCM scheme_PlayCSound (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
csoundplayback (NULL, str);
return SCM_EOL;
}
SCM scheme_PlaybackProperties (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
playback_properties_change (NULL, str);
return SCM_EOL;
}
SCM scheme_Help (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
browse_manual (NULL, str);
return SCM_EOL;
}
SCM scheme_About (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
about (NULL, str);
return SCM_EOL;
}
SCM scheme_AddBookmark (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
addbookmark (NULL, str);
return SCM_EOL;
}
SCM scheme_GotoBookmark (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
gotobookmark (NULL, str);
return SCM_EOL;
}
SCM scheme_NextBookmark (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
nextbookmark (NULL, str);
return SCM_EOL;
}
SCM scheme_PrevBookmark (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
prevbookmark (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleEdit (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
toggle_edit_mode (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleRest (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
toggle_rest_mode (NULL, str);
return SCM_EOL;
}
SCM scheme_ToggleRhythm (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
toggle_rhythm_mode (NULL, str);
return SCM_EOL;
}
SCM scheme_ClearOverlay (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
clear_overlay (NULL, str);
return SCM_EOL;
}
SCM scheme_CreateRhythm (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
create_rhythm_cb (NULL, str);
return SCM_EOL;
}
SCM scheme_DeleteRhythm (SCM optional) {
    int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
  }
delete_rhythm_cb (NULL, str);
return SCM_EOL;
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
