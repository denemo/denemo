SCM scheme_CursorLeft (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
cursorleft_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MoveCursorLeft (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
movecursorleft_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_CursorDown (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
cursordown_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_CursorUp (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
cursorup_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_CursorRight (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
cursorright_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MoveCursorRight (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
movecursorright_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_GoToMark (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
goto_mark (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SwapPointAndMark (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
swap_point_and_mark (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_GoToSelectionStart (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
goto_selection_start (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PushPosition (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
PushPosition (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PopPosition (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
PopPosition (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PopPushPosition (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
PopPushPosition (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleReduceToDrawingArea (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
ToggleReduceToDrawingArea (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_StaffUp (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
staffup_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_StaffDown (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
staffdown_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MoveToStaffUp (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
movetostaffup_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MoveToStaffDown (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
movetostaffdown_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MeasureLeft (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
measureleft_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MeasureRight (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
measureright_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MoveToMeasureLeft (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
movetomeasureleft_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MoveToMeasureRight (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
movetomeasureright_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_A (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
go_to_A_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_B (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
go_to_B_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_C (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
go_to_C_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_D (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
go_to_D_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_E (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
go_to_E_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_F (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
go_to_F_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_G (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
go_to_G_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_OctaveUp (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
octave_up_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_OctaveDown (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
octave_down_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_WholeNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_chord_0key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_HalfNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_chord_1key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_QuarterNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_chord_2key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EighthNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_chord_3key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SixteenthNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_chord_4key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ThirtysecondNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_chord_5key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SixtyfourthNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_chord_6key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_OneHundredTwentyEighthNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_chord_7key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_TwoHundredFiftySixthNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_chord_8key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertWholeRest (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_rest_0key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertHalfRest (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_rest_1key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertQuarterRest (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_rest_2key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertEighthRest (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_rest_3key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertSixteenthRest (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_rest_4key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertThirtysecondRest (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_rest_5key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertSixtyfourthRest (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_rest_6key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBlankWholeNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_blankchord_0key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBlankHalfNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_blankchord_1key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBlankQuarterNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_blankchord_2key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBlankEighthNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_blankchord_3key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBlankSixteenthNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_blankchord_4key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBlankThirtysecondNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_blankchord_5key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBlankSixtyfourthNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_blankchord_6key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBlankOneHundredTwentyEighthNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_blankchord_7key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBlankTwoHundredFiftySixthNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_blankchord_8key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleRestMode (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
rest_toggle_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleBlankMode (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
toggle_blank_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertDuplet (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_duplet_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertTriplet (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_triplet_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_StartTriplet (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
start_triplet_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EndTuplet (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
end_tuplet_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertQuadtuplet (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_quadtuplet_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertQuintuplet (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_quintuplet_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertSextuplet (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_sextuplet_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertSeptuplet (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_septuplet_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddNoteToChord (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
add_tone_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_RemoveNoteFromChord (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
remove_tone_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Sharpen (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
sharpen_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Flatten (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
flatten_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PendingSharpen (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
pending_sharpen_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PendingFlatten (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
pending_flatten_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_StemUp (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
stem_up_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_StemDown (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
stem_down_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddDot (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
add_dot_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_RemoveDot (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
remove_dot_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertTiedNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
tie_notes_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleTie (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
toggle_tie (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteObject (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
deleteobject_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeletePreviousObject (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
deletepreviousobject_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertMeasure (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_measure_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddMeasure (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
addmeasureafter_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertMeasureBefore (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insertmeasurebefore_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertMeasureAfter (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insertmeasureafter_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AppendMeasure (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
append_measure_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteMeasure (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
deletemeasure_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteMeasureAllStaffs (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
deletemeasureallstaffs_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ShrinkMeasures (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
adjust_measure_less_width_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_WidenMeasures (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
adjust_measure_more_width_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ShorterStaffs (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
adjust_staff_less_height_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_TallerStaffs (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
adjust_staff_more_height_key_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertTrebleClef (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newcleftreble_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBassClef (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newclefbass_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insertg8clef (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newclefg8_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertAltoClef (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newclefalto_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertTenorClef (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newcleftenor_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertSopranoClef (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newclefsoprano_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialTrebleClef (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setcleftreble_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialBassClef (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setclefbass_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialg8clef (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setclefg8_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialAltoClef (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setclefalto_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialTenorClef (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setcleftenor_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialSopranoClef (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setclefsoprano_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert22Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newtimesig22_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert32Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newtimesig32_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert42Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newtimesig42_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert44Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newtimesig44_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert34Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newtimesig34_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert24Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newtimesig24_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert64Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newtimesig64_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert38Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newtimesig38_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert68Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newtimesig68_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert128Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newtimesig128_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Insert98Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newtimesig98_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set22Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
settimesig22_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set32Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
settimesig32_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set42Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
settimesig42_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set44Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
settimesig44_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set34Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
settimesig34_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set24Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
settimesig24_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set64Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
settimesig64_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set38Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
settimesig38_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set68Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
settimesig68_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set128Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
settimesig128_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Set98Time (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
settimesig98_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertCmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigcmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertGmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysiggmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertDmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigdmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertAmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigamaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertEmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigemaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigbmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertFSharpmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigfsharpmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertCSharpmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigcsharpmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertFmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigfmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBflatmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigbflatmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertEflatmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigeflatmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertAflatmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigaflatmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertDflatmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigdflatmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertGflatmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysiggflatmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertCflatmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigcflatmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertAmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigamin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertEmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigemin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigbmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertFSharpmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigfsharpmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertCSharpmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigcsharpmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertGSharpmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysiggsharpmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertDSharpmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigdsharpmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertASharpmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigasharpmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertDmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigdmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertGmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysiggmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertCmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigcmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertFmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigfmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBflatmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigbflatmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertEflatmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigeflatmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertAflatmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newkeysigaflatmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialCmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigcmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialGmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysiggmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialDmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigdmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialAmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigamaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialEmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigemaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialBmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigbmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialFSharpmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigfsharpmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialCSharpmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigcsharpmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialFmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigfmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialBflatmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigbflatmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialEflatmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigeflatmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialAflatmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigaflatmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialDflatmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigdflatmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialGflatmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysiggflatmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialCflatmaj (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigcflatmaj_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialAmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigamin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialEmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigemin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialBmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigbmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialFSharpmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigfsharpmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialCSharpmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigcsharpmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialGSharpmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysiggsharpmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialDSharpmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigdsharpmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialASharpmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigasharpmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialDmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigdmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialGmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysiggmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialCmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigcmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialFmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigfmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialBflatmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigbflatmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialEflatmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigeflatmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetInitialAflatmin (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
setkeysigaflatmin_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetMark (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
set_mark_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_UnsetMark (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
unset_mark_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SetPoint (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
set_point_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleBeginSlur (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
toggle_begin_slur_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleEndSlur (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
toggle_end_slur_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleStartCrescendo (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
toggle_start_crescendo_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleEndCrescendo (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
toggle_end_crescendo_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleStartDiminuendo (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
toggle_start_diminuendo_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleEndDiminuendo (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
toggle_end_diminuendo_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleOpen (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
add_open_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleSforzato (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
add_sforzato_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleStopped (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
add_stopped_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleThumb (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
add_thumb_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ForceCaution (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
force_cautionary_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ChangePitch (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
change_pitch_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertRhythm (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_rhythm_pattern_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_NextRhythm (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
nextrhythm_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AppendMeasureAllStaffs (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
append_measure_score_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ExecuteScheme (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
execute_scheme (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SharpenEnharmonicSet (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
set_sharper (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_FlattenEnharmonicSet (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
set_flatter (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_New (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
file_newwrapper (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Open (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
file_open_with_check (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ImportLilypond (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
file_import_lilypond_with_check (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ImportMidi (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
file_import_midi_with_check (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ImportMusicXml (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
file_import_musicxml_with_check (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddStaffs (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
file_add_staffs (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddMovements (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
file_add_movements (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MovementProps (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
movement_props_dialog (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_OpenNewWindow (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
openinnew (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Save (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
file_savewrapper (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SaveAs (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
file_saveaswrapper (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SaveCopy (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
file_copy_save (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_OpenTemplate (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
system_template_open_with_check (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_OpenExample (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
system_example_open_with_check (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_OpenMyTemplate (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
local_template_open_with_check (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SaveTemplate (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
template_save (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_NewWindow (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newview (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertMovementBefore (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_movement_before (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertMovementAfter (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_movement_after (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_NewMovement (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
append_new_movement (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SaveParts (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
file_savepartswrapper (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ExportMUDELA (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
export_mudela_action (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ExportPDF (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
export_pdf_action (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ExportPNG (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
export_png_action (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ExportMIDI (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
export_midi_action (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PrintPreview (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
printpreview_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PrintSelection (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
printselection_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PrintExcerptPreview (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
printexcerptpreview_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Print (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
printall_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PrintPart (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
printpart_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Close (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
close_gui_with_check (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Quit (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
closewrapper (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Undo (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
undowrapper (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Redo (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
redowrapper (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Copy (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
copywrapper (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Cut (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
cutwrapper (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Paste (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
pastewrapper (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PasteClipboard (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
paste_clipboard (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ScoreProperties (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
score_properties_dialog (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SaveSelection (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
saveselwrapper (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Preferences (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
preferences_change (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SaveAccels (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
save_default_keymap_file_wrapper (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_CommandManagement (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
configure_keyboard_dialog (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SwapStaffs (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
swapstaffs (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SplitVoices (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
splitstaffs (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_JoinVoices (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
joinstaffs (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_SwapMovements (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
swapmovements (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_VoiceUp (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
voiceup_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_VoiceDown (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
voicedown_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MoveToVoiceUp (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
movetovoiceup_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MoveToVoiceDown (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
movetovoicedown_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddBefore (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newstaffbefore (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddAfter (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
dnm_newstaffafter (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddInitial (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newstaffinitial (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddLast (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
newstafflast (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteBefore (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
delete_staff_before (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteStaff (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
delete_staff_current (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteAfter (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
delete_staff_after (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddVoice (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
dnm_newstaffvoice (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_StaffProperties (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
staff_properties_change_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InitialClef (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
clef_change_initial (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertClef (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
clef_change_insert (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InitialKey (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
key_change_initial (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertKey (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
key_change_insert (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InitialTimeSig (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
timesig_change_initial (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertTimeSig (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
timesig_change_insert (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ChangeNotehead (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
set_notehead (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertStem (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
stem_directive_insert (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AddVerse (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
add_verse (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteVerse (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
delete_verse (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EditFiguredBass (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
figure_insert (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteFiguredBass (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
delete_figured_bass (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_HideFiguredBass (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
hide_figured_bass (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ShowFiguredBass (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
show_figured_bass (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EditChords (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
fakechord_insert (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertDynamic (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_dynamic (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EditObject (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
edit_object (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EditDirective (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
edit_object_directive (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EditStaffDirective (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
edit_staff_directive (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EditVoiceDirective (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
edit_voice_directive (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EditScoreDirective (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
edit_score_directive (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EditMovementDirective (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
edit_movement_directive (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EditClefDirective (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
edit_clef_directive (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EditTimesigDirective (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
edit_timesig_directive (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_EditKeysigDirective (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
edit_keysig_directive (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteDirective (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
delete_chord_or_note_directive (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AttachLilyToNote (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
note_directive (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_AttachLilyToChord (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
chord_directive (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertBarline (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
insert_barline (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_GoToMeasure (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
tomeasurenum (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_GoToBeginning (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
tohome (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_GoToEnd (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
toend (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MoveToBeginning (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
movetostart (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MoveToEnd (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
movetoend (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_NextMovement (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
next_movement (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PreviousMovement (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
prev_movement (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteMovement (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
delete_movement (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Play (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
ext_midi_playback (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Stop (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
stop_midi_playback (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_PlaybackProperties (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
playback_properties_change (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_Help (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
browse_manual (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_About (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
about (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MoreCommands (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
morecommands (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_MyCommands (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
mycommands (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_FetchCommands (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
fetchcommands (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleEdit (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
toggle_edit_mode (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleRest (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
toggle_rest_mode (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ToggleRhythm (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
toggle_rhythm_mode (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_ClearOverlay (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
clear_overlay (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_CreateRhythm (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
create_rhythm_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_DeleteRhythm (SCM optional) {
gboolean query=FALSE;
DenemoScriptParam param;
GString *gstr=NULL;
int length;
   char *str=NULL;
if(scm_is_string(optional)){
str = scm_to_locale_stringn(optional, &length);
gstr = g_string_new_len(str, length);
if(!strncmp("query",str,5)) query = TRUE;          }
         param.string = gstr;
         param.status = FALSE;
         
delete_rhythm_cb (NULL, &param);
         if(param.status && query) return scm_makfrom0str (gstr->str);         if(gstr) g_string_free(gstr, TRUE);
return SCM_BOOL(param.status);
}
SCM scheme_InsertA (SCM optional) {
InsertA (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_AddNoteA (SCM optional) {
AddNoteA (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_AddA (SCM optional) {
AddA (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeToA (SCM optional) {
ChangeToA (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_MoveToA (SCM optional) {
MoveToA (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertB (SCM optional) {
InsertB (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_AddNoteB (SCM optional) {
AddNoteB (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_AddB (SCM optional) {
AddB (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeToB (SCM optional) {
ChangeToB (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_MoveToB (SCM optional) {
MoveToB (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertC (SCM optional) {
InsertC (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_AddNoteC (SCM optional) {
AddNoteC (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_AddC (SCM optional) {
AddC (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeToC (SCM optional) {
ChangeToC (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_MoveToC (SCM optional) {
MoveToC (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertD (SCM optional) {
InsertD (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_AddNoteD (SCM optional) {
AddNoteD (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_AddD (SCM optional) {
AddD (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeToD (SCM optional) {
ChangeToD (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_MoveToD (SCM optional) {
MoveToD (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertE (SCM optional) {
InsertE (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_AddNoteE (SCM optional) {
AddNoteE (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_AddE (SCM optional) {
AddE (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeToE (SCM optional) {
ChangeToE (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_MoveToE (SCM optional) {
MoveToE (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertF (SCM optional) {
InsertF (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_AddNoteF (SCM optional) {
AddNoteF (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_AddF (SCM optional) {
AddF (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeToF (SCM optional) {
ChangeToF (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_MoveToF (SCM optional) {
MoveToF (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertG (SCM optional) {
InsertG (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_AddNoteG (SCM optional) {
AddNoteG (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_AddG (SCM optional) {
AddG (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeToG (SCM optional) {
ChangeToG (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_MoveToG (SCM optional) {
MoveToG (NULL, NULL);
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
SCM scheme_SetDur0 (SCM optional) {
SetDur0 (NULL, NULL);
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
SCM scheme_SetDur1 (SCM optional) {
SetDur1 (NULL, NULL);
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
SCM scheme_SetDur2 (SCM optional) {
SetDur2 (NULL, NULL);
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
SCM scheme_SetDur3 (SCM optional) {
SetDur3 (NULL, NULL);
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
SCM scheme_SetDur4 (SCM optional) {
SetDur4 (NULL, NULL);
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
SCM scheme_SetDur5 (SCM optional) {
SetDur5 (NULL, NULL);
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
SCM scheme_SetDur6 (SCM optional) {
SetDur6 (NULL, NULL);
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
SCM scheme_7 (SCM optional) {
Dur7 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertDur7 (SCM optional) {
InsertDur7 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeDur7 (SCM optional) {
ChangeDur7 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_SetDur7 (SCM optional) {
SetDur7 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertRest7 (SCM optional) {
InsertRest7 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeRest7 (SCM optional) {
ChangeRest7 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_8 (SCM optional) {
Dur8 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertDur8 (SCM optional) {
InsertDur8 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeDur8 (SCM optional) {
ChangeDur8 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_SetDur8 (SCM optional) {
SetDur8 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_InsertRest8 (SCM optional) {
InsertRest8 (NULL, NULL);
return SCM_BOOL(TRUE);
}
SCM scheme_ChangeRest8 (SCM optional) {
ChangeRest8 (NULL, NULL);
return SCM_BOOL(TRUE);
}
