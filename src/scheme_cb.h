SCM scheme_CursorLeft (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

cursorleft_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_CursorDown (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

cursordown_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_CursorUp (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

cursorup_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_CursorRight (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

cursorright_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_GoToMark (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

goto_mark (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_StaffUp (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

staffup_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_StaffDown (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

staffdown_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MeasureLeft (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

measureleft_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MeasureRight (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

measureright_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_A (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

go_to_A_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_B (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

go_to_B_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_C (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

go_to_C_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_D (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

go_to_D_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_E (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

go_to_E_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_F (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

go_to_F_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_G (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

go_to_G_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_OctaveUp (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

octave_up_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_OctaveDown (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

octave_down_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_WholeNote (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_chord_0key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_HalfNote (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_chord_1key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_QuarterNote (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_chord_2key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EighthNote (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_chord_3key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SixteenthNote (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_chord_4key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ThirtysecondNote (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_chord_5key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SixtyfourthNote (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_chord_6key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBlankWholeNote (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_blankchord_0key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBlankHalfNote (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_blankchord_1key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBlankQuarterNote (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_blankchord_2key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBlankEighthNote (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_blankchord_3key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBlankSixteenthNote (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_blankchord_4key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBlankThirtysecondNote (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_blankchord_5key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBlankSixtyfourthNote (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_blankchord_6key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleRestMode (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

rest_toggle_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleBlankMode (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

toggle_blank_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertWholeRest (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_rest_0key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertHalfRest (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_rest_1key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertQuarterRest (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_rest_2key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertEighthRest (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_rest_3key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertSixteenthRest (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_rest_4key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertThirtysecondRest (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_rest_5key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertSixtyfourthRest (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_rest_6key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertDuplet (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_duplet_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertTriplet (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_triplet_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_StartTriplet (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

start_triplet_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EndTuplet (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

end_tuplet_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertQuadtuplet (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_quadtuplet_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertQuintuplet (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_quintuplet_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertSextuplet (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_sextuplet_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertSeptuplet (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_septuplet_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddNoteToChord (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_tone_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_RemoveNoteFromChord (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

remove_tone_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Sharpen (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

sharpen_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Flatten (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

flatten_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_StemUp (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

stem_up_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_StemDown (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

stem_down_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddDot (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_dot_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_RemoveDot (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

remove_dot_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertTiedNote (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

tie_notes_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteObject (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

deleteobject_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeletePreviousObject (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

deletepreviousobject_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertMeasure (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_measure_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AppendMeasure (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

append_measure_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteMeasure (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

deletemeasure_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteMeasureAllStaffs (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

deletemeasureallstaffs_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ShrinkMeasures (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

adjust_measure_less_width_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_WidenMeasures (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

adjust_measure_more_width_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ShorterStaffs (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

adjust_staff_less_height_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_TallerStaffs (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

adjust_staff_more_height_key_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertTrebleClef (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newcleftreble_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBassClef (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newclefbass_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insertg8clef (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newclefg8_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertAltoClef (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newclefalto_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertTenorClef (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newcleftenor_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertSopranoClef (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newclefsoprano_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialTrebleClef (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setcleftreble_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialBassClef (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setclefbass_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialg8clef (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setclefg8_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialAltoClef (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setclefalto_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialTenorClef (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setcleftenor_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialSopranoClef (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setclefsoprano_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert22Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newtimesig22_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert32Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newtimesig32_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert42Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newtimesig42_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert44Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newtimesig44_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert34Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newtimesig34_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert24Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newtimesig24_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert64Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newtimesig64_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert38Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newtimesig38_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert68Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newtimesig68_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert128Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newtimesig128_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert98Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newtimesig98_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set22Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

settimesig22_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set32Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

settimesig32_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set42Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

settimesig42_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set44Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

settimesig44_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set34Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

settimesig34_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set24Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

settimesig24_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set64Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

settimesig64_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set38Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

settimesig38_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set68Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

settimesig68_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set128Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

settimesig128_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set98Time (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

settimesig98_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertCmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigcmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertGmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysiggmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertDmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigdmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertAmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigamaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertEmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigemaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigbmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertFSharpmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigfsharpmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertCSharpmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigcsharpmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertFmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigfmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBflatmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigbflatmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertEflatmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigeflatmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertAflatmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigaflatmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertDflatmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigdflatmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertGflatmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysiggflatmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertCflatmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigcflatmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertAmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigamin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertEmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigemin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigbmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertFSharpmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigfsharpmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertCSharpmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigcsharpmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertGSharpmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysiggsharpmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertDSharpmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigdsharpmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertASharpmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigasharpmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertDmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigdmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertGmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysiggmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertCmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigcmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertFmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigfmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBflatmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigbflatmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertEflatmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigeflatmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertAflatmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newkeysigaflatmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialCmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigcmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialGmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysiggmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialDmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigdmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialAmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigamaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialEmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigemaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialBmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigbmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialFSharpmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigfsharpmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialCSharpmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigcsharpmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialFmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigfmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialBflatmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigbflatmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialEflatmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigeflatmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialAflatmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigaflatmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialDflatmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigdflatmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialGflatmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysiggflatmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialCflatmaj (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigcflatmaj_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialAmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigamin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialEmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigemin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialBmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigbmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialFSharpmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigfsharpmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialCSharpmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigcsharpmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialGSharpmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysiggsharpmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialDSharpmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigdsharpmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialASharpmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigasharpmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialDmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigdmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialGmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysiggmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialCmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigcmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialFmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigfmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialBflatmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigbflatmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialEflatmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigeflatmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialAflatmin (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

setkeysigaflatmin_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetMark (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

set_mark_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_UnsetMark (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

unset_mark_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleBeginSlur (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

toggle_begin_slur_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleEndSlur (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

toggle_end_slur_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleStartCrescendo (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

toggle_start_crescendo_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleEndCrescendo (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

toggle_end_crescendo_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleStartDiminuendo (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

toggle_start_diminuendo_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleEndDiminuendo (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

toggle_end_diminuendo_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleAccent (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_accent_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleFermata (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_fermata_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleStaccato (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_staccato_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleTenuto (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_tenuto_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleTrill (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_trill_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleTurn (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_turn_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleMordent (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_mordent_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleStaccatissimo (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_staccatissimo_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleCoda (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_coda_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleFlageolet (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_flageolet_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleOpen (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_open_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_TogglePrallMordent (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_prallmordent_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_TogglePrallPrall (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_prallprall_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_TogglePrall (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_prall_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleReverseTurn (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_reverseturn_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleSegno (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_segno_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleSforzato (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_sforzato_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleStopped (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_stopped_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleThumb (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_thumb_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleUpprall (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_upprall_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleArpeggio (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

add_arpeggio_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetGrace (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

set_grace_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ForceCaution (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

force_cautionary_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ChangePitch (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

change_pitch_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DoubleBar (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_doublebar_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EndBar (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_endbar_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_OpenRepeat (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_openrepeat_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_CloseRepeat (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_closerepeat_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_OpenCloseRepeat (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_opencloserepeat_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertRhythm (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_rhythm_pattern_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_NextRhythm (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

nextrhythm_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AppendMeasuresToScore (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

append_measure_score_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SharpenEnharmonicSet (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

set_sharper (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_FlattenEnharmonicSet (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

set_flatter (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_New (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

file_newwrapper (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Open (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

file_open_with_check (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddStaffs (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

file_add_staffs (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddMovements (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

file_add_movements (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MovementProps (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

movement_props_dialog (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_OpenNewWindow (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

openinnew (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Save (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

file_savewrapper (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SaveAs (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

file_saveaswrapper (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_OpenTemplate (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

system_template_open_with_check (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_OpenExample (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

system_example_open_with_check (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_OpenMyTemplate (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

local_template_open_with_check (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SaveTemplate (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

template_save (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_NewWindow (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newview (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertMovementBefore (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_movement_before (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertMovementAfter (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_movement_after (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SaveParts (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

file_savepartswrapper (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ExportPDF (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

export_pdf_action (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ConfigureScore (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

scorewizard (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PrintPreview (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

printpreview_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PrintExcerptPreview (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

printexcerptpreview_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Print (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

printall_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PrintPart (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

printpart_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Close (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

close_gui_with_check (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Quit (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

closewrapper (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Undo (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

undowrapper (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Redo (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

redowrapper (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Copy (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

copywrapper (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Cut (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

cutwrapper (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Paste (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

pastewrapper (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ScoreProperties (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

score_properties_dialog (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SaveSelection (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

saveselwrapper (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Preferences (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

preferences_change (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SaveAccels (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

save_default_keymap_file_wrapper (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_CommandManagement (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

configure_keyboard_dialog (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_LoadPlugins (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

load_plugin (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_UnloadPlugins (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

unloadplugins (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ListPlugins (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

list_loaded_plugins (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ListAvailablePlugins (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

list_available_plugins (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SwapStaffs (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

swapstaffs (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SplitVoices (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

splitstaffs (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_JoinVoices (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

joinstaffs (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SwapMovements (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

swapmovements (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_VoiceUp (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

voiceup_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_VoiceDown (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

voicedown_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddBefore (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newstaffbefore (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddAfter (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

dnm_newstaffafter (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddInitial (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newstaffinitial (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddLast (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

newstafflast (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteBefore (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

delete_staff_before (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteStaff (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

delete_staff_current (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteAfter (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

delete_staff_after (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddVoice (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

dnm_newstaffvoice (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_StaffProperties (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

staff_properties_change_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InitialClef (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

clef_change_initial (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertClef (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

clef_change_insert (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InitialKey (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

key_change_initial (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertKey (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

key_change_insert (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InitialTimeSig (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

timesig_change_initial (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertTimeSig (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

timesig_change_insert (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ChangeNotehead (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

set_notehead (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertStem (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

stem_directive_insert (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EditLyric (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

lyric_insert (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EditFiguredBass (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

figure_insert (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EditChords (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

fakechord_insert (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertDynamic (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_dynamic (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertLilyDirective (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

lily_directive_insert (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertLilyPostfix (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

lily_directive_postfix (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBarline (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

insert_barline (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_GoToMeasure (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

tomeasurenum (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_GoToBeginning (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

tohome (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_GoToEnd (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

toend (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_NextMovement (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

next_movement (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PreviousMovement (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

prev_movement (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteMovement (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

delete_movement (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteBookmarks (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

deletebookmarks (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Play (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

ext_midi_playback (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Stop (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

stop_midi_playback (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PlayCSound (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

csoundplayback (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PlaybackProperties (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

playback_properties_change (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Help (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

browse_manual (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_About (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

about (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MoreCommands (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

morecommands (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MyCommands (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

mycommands (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddBookmark (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

addbookmark (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_GotoBookmark (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

gotobookmark (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_NextBookmark (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

nextbookmark (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PrevBookmark (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

prevbookmark (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleEdit (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

toggle_edit_mode (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleRest (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

toggle_rest_mode (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleRhythm (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

toggle_rhythm_mode (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ClearOverlay (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

clear_overlay (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_CreateRhythm (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

create_rhythm_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteRhythm (SCM optional) {
SCM ret;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(SCM_STRINGP(optional)){
str = gh_scm2newstr(optional, &length);
gstr = g_string_new_len(str, length);
  }
param.string = gstr;
param.status = FALSE;

delete_rhythm_cb (NULL, &param);
if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ChangeToA (SCM optional) {
ChangeToA (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertA (SCM optional) {
InsertA (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeToB (SCM optional) {
ChangeToB (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertB (SCM optional) {
InsertB (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeToC (SCM optional) {
ChangeToC (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertC (SCM optional) {
InsertC (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeToD (SCM optional) {
ChangeToD (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertD (SCM optional) {
InsertD (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeToE (SCM optional) {
ChangeToE (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertE (SCM optional) {
InsertE (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeToF (SCM optional) {
ChangeToF (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertF (SCM optional) {
InsertF (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeToG (SCM optional) {
ChangeToG (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertG (SCM optional) {
InsertG (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_0 (SCM optional) {
Dur0 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertDur0 (SCM optional) {
InsertDur0 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeDur0 (SCM optional) {
ChangeDur0 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertRest0 (SCM optional) {
InsertRest0 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeRest0 (SCM optional) {
ChangeRest0 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_1 (SCM optional) {
Dur1 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertDur1 (SCM optional) {
InsertDur1 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeDur1 (SCM optional) {
ChangeDur1 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertRest1 (SCM optional) {
InsertRest1 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeRest1 (SCM optional) {
ChangeRest1 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_2 (SCM optional) {
Dur2 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertDur2 (SCM optional) {
InsertDur2 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeDur2 (SCM optional) {
ChangeDur2 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertRest2 (SCM optional) {
InsertRest2 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeRest2 (SCM optional) {
ChangeRest2 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_3 (SCM optional) {
Dur3 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertDur3 (SCM optional) {
InsertDur3 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeDur3 (SCM optional) {
ChangeDur3 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertRest3 (SCM optional) {
InsertRest3 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeRest3 (SCM optional) {
ChangeRest3 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_4 (SCM optional) {
Dur4 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertDur4 (SCM optional) {
InsertDur4 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeDur4 (SCM optional) {
ChangeDur4 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertRest4 (SCM optional) {
InsertRest4 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeRest4 (SCM optional) {
ChangeRest4 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_5 (SCM optional) {
Dur5 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertDur5 (SCM optional) {
InsertDur5 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeDur5 (SCM optional) {
ChangeDur5 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertRest5 (SCM optional) {
InsertRest5 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeRest5 (SCM optional) {
ChangeRest5 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_6 (SCM optional) {
Dur6 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertDur6 (SCM optional) {
InsertDur6 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeDur6 (SCM optional) {
ChangeDur6 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertRest6 (SCM optional) {
InsertRest6 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeRest6 (SCM optional) {
ChangeRest6 (NULL, NULL);
return SCM_BOOL(TRUE);
}
