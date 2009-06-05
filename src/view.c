/* view.c
 * Functions to create a top level Denemo window
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2003-2005  Adam Tee (c) 2007, 2008 Richard Shann
 * 
 */
#include <string.h>
#include <math.h>
#include "view.h"
#include "bookmarks.h"
#include "lyparserfuncs.h"
#include "lilydirectives.h"
#include "dialogs.h"
#include "utils.h"
#include <stdlib.h>
#include <glib/gstdio.h>
#include "scorewizard.h"
#include "playback.h"
#include "alsaplayback.h"
#include "midiseq.h"
#include "pitchentry.h"
#include "exportlilypond.h"
#include "print.h"
#include "kbd-custom.h"
#include "keyboard.h"
#include "csoundplayback.h"
#include "exportlilypond.h"
#include "midi.h"
#include "jackmidi.h"
#if GTK_MAJOR_VERSION > 1
#include <gtk/gtkaccelgroup.h>
#endif
static void
newview (GtkAction *action, gpointer param);

static void
closewrapper (GtkAction *action, gpointer param);
static gboolean
close_gui_with_check (GtkAction *action, gpointer param);
static void
openinnew (GtkAction *action, gpointer param);
static void 
create_rhythm_cb (GtkAction* action, gpointer param);
static void
delete_rhythm_cb (GtkAction * action, gpointer param);
static void
toggle_edit_mode (GtkAction * action, gpointer param);
static void
toggle_rest_mode (GtkAction * action, gpointer param);
static void
toggle_rhythm_mode (GtkAction * action, gpointer param);
static void
fetchcommands (GtkAction *action, gpointer param);
static void
morecommands (GtkAction *action, gpointer param);
static void
mycommands (GtkAction *action, gpointer param);
static void
create_window(void);
static void
toggle_pitch_recognition (void);
static gint dnm_key_snooper(GtkWidget *grab_widget, GdkEventKey *event);
#define MUSIC_FONT(a) "<span  size=\"10000\" face=\"Denemo\">"a"</span>"


GtkAction *sharpaction, *flataction;


extern midi_seq *sq;		/* global denemo sequencer FIXME: should not be global */

static void   activate_action(gchar *path);
typedef enum 
{
  ACCELS_LOADED = 0x0,
  ACCELS_CHANGED = 0x1<<0,
  EXTRA_ACCELS_ACTIVE = 0x1<<1,
  ACCELS_MAY_HAVE_CHANGED = 0x1<<2
} AccelStatus;

#define gh_scm2newstr scm_to_locale_stringn 

static void use_markup(GtkWidget *widget);
static void save_accels (void);

#include "callbacks.h" /* callback functions menuitems that can be called by scheme */
#include <libguile.h>
#include <guile/gh.h>

#include "scheme_cb.h"

SCM call_out_to_guile(char *script) {
 return gh_eval_str_with_catch (script, gh_standard_handler);
}

/***************** definitions to implement calling radio/check items from scheme *******************/
#define MODELESS_STRING "Modeless"
#define CLASSICMODE_STRING "ClassicMode"
#define INSERTMODE_STRING "InsertMode"
#define EDITMODE_STRING "EditMode"
#define NOTE_E_STRING "Note"
#define REST_E_STRING "Rest"
#define BLANK_E_STRING "Blank"
#define RHYTHM_E_STRING "Rhythm"
#define ToggleRhythmToolbar_STRING "ToggleRhythmToolbar"
#define ToggleEntryToolbar_STRING  "ToggleEntryToolbar"
#define ToggleActionMenu_STRING  "ToggleActionMenu"
#define ToggleObjectMenu_STRING  "ToggleObjectMenu"
#define ToggleLilyText_STRING  "ToggleLilyText"
#define ToggleScript_STRING  "ToggleScript"

#define ToggleArticulationPalette_STRING  "ToggleArticulationPalette"
#define TogglePrintView_STRING  "TogglePrintView"
#define ToggleScoreView_STRING  "ToggleScoreView"
#define ToggleScoreTitles_STRING  "ToggleScoreTitles"
#define QuickEdits_STRING  "QuickEdits"
#define RecordScript_STRING  "RecordScript"
#define ReadOnly_STRING  "ReadOnly"


#define FN_DEF(X) void X##_CB(void) {\
activate_action("/MainMenu/ModeMenu/"X##_STRING);}

 FN_DEF(MODELESS);
 FN_DEF(CLASSICMODE);
 FN_DEF(INSERTMODE);
 FN_DEF(EDITMODE);
 FN_DEF(NOTE_E);
 FN_DEF(REST_E);
 FN_DEF(BLANK_E);
 FN_DEF(RHYTHM_E);

typedef struct cb_string_pairs  { gpointer p; gchar *str;} cb_string_pairs;
cb_string_pairs activatable_commands[] = {
  {MODELESS_CB, MODELESS_STRING},
  {CLASSICMODE_CB, CLASSICMODE_STRING},
  {INSERTMODE_CB, INSERTMODE_STRING},
  {EDITMODE_CB, EDITMODE_STRING},
  {NOTE_E_CB, NOTE_E_STRING},
  {REST_E_CB, REST_E_STRING},
  {BLANK_E_CB, BLANK_E_STRING},
  {RHYTHM_E_CB, RHYTHM_E_STRING}

};

/***************** end of definitions to implement calling radio/check items from scheme *******************/




#define DENEMO_SCHEME_PREFIX "d-"
static void install_scm_function(gchar *name, gpointer callback) {
  scm_c_define_gsubr (name, 0, 1, 0, callback); // one optional parameter

}
static void install_scm_function_with_param(gchar *name, gpointer callback) {
scm_c_define_gsubr (name, 1, 1, 0, callback);

}
static void install_scm_function2(gchar *name, gpointer callback) {
scm_c_define_gsubr (name, 2, 0, 0, callback);

}

static void install_scm_function3(gchar *name, gpointer callback) {
scm_c_define_gsubr (name, 3, 0, 0, callback);

}


/* called by a script if it requires initialization
 the initialization script is expected to be in init.scm in the menupath of the action that invoked the script*/
static SCM scheme_initialize_script(SCM action_name) {
  SCM ret;
  gint length;
  //FIXME scm_dynwind_begin (0); etc
  gchar *name = scm_to_locale_string(action_name);//scm_dynwind_free (name);
  GtkAction *action = lookup_action_from_name(name);
  if(!action){
    g_warning("Non-existent action %s", name);
    return SCM_BOOL(FALSE);
  }
    
  gchar *menupath = g_object_get_data(G_OBJECT(action), "menupath");
  gchar *filename = g_build_filename(get_data_dir(), "actions", "menus", menupath, "init.scm", NULL);
  if(g_file_test(filename, G_FILE_TEST_EXISTS)) { 
    g_print("About to load from %s\n", filename);
    ret = scm_c_primitive_load(filename);
  }
  g_free(filename);
  filename = g_build_filename(locatedotdenemo(), "actions", "menus", menupath, "init.scm", NULL);
  if(g_file_test(filename, G_FILE_TEST_EXISTS)) { 
    g_print("About to load from %s\n", filename);
    ret = scm_c_primitive_load(filename);
  }
  g_free(filename);
  return ret;
}



/* when a script calls a command which is itself a script it comes through here */
static SCM scheme_script_callback(SCM script) {
    int length;
    char *name=NULL;
  //FIXME scm_dynwind_begin (0); etc
   if(SCM_STRINGP(script)){
     name = scm_to_locale_string(script);
     if(name) {
       GtkAction *action = lookup_action_from_name (name);
       if(action){
	 gchar *text = g_object_get_data(G_OBJECT(action), "scheme");
	 if(text)
	   return call_out_to_guile(text);
       }
     }
   }
return  SCM_BOOL(FALSE);
}
void create_scheme_function_for_script(gchar *name) {
  gchar *proc = g_strdup_printf("(define (d-%s) (d-ScriptCallback \"%s\"))\n", name, name, name);
   (void)call_out_to_guile(proc);
   g_free(proc);
}


SCM scheme_debug_object (SCM optional) {
 DenemoGUI *gui = Denemo.gui;
 DenemoObject *curObj;

 if(!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data))
   return SCM_BOOL(FALSE);
 g_print("*************\nType = %d\nbasic_durinticks = %d\ndurinticks - %d\nstarttickofnextnote = %d\n***********\n", 
	 curObj->type,
	 curObj->basic_durinticks,
	 curObj->durinticks,
	 curObj->starttickofnextnote);
 return SCM_BOOL(TRUE);
}

/* write MIDI/Audio filter status */
static SCM scheme_input_filter_names(SCM filtername) {
    int length;
    char *name=NULL;
  //FIXME scm_dynwind_begin (0); etc
   if(SCM_STRINGP(filtername)){
     name = scm_to_locale_string(filtername);
     if(name) {
       if(Denemo.input_filters)
	 g_string_assign(Denemo.input_filters, name);
       else
	 Denemo.input_filters = g_string_new(name);
       write_input_status();
       return SCM_BOOL(TRUE);
     }
   }
   if(Denemo.input_filters)
     g_string_free(Denemo.input_filters, TRUE);
   Denemo.input_filters = NULL;
   return  SCM_BOOL(FALSE);
}


SCM scheme_get_cursor_note (SCM optional) {
 DenemoGUI *gui = Denemo.gui;
 SCM scm = scm_makfrom0str (g_strdup_printf("%c", mid_c_offsettoname (gui->si->cursor_y)));//FIXME a dedicated function avoiding memory leak.
   return scm;
}


SCM scheme_chordize (SCM setting) {
  DenemoGUI *gui = Denemo.gui;
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if(!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type!=CHORD) || !(thechord = (chord *)  curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
    return SCM_BOOL(FALSE);
  gboolean val;
  if(SCM_BOOLP(setting)){
    val = gh_scm2bool(setting);
  }
  if( thechord->chordize != val) {
    thechord->chordize = val;
    score_status(gui, TRUE);
  }
  return SCM_BOOL(TRUE);
}


SCM scheme_get_note_name (SCM optional) {
    int length;
    //char *str=NULL;
   //if(SCM_STRINGP(optional)){
   //str = gh_scm2newstr(optional, &length);
   //  }
 DenemoGUI *gui = Denemo.gui;
 DenemoObject *curObj;
 chord *thechord;
 note *thenote;
 if(!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type!=CHORD) || !(thechord = (chord *)  curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
   return scm_makfrom0str ("");
 else {
   SCM scm = scm_makfrom0str (g_strdup_printf("%c",  mid_c_offsettoname (thenote->mid_c_offset)));//FIXME a dedicated function avoiding memory leak.
   return scm;
 }
   
}

SCM scheme_get_note (SCM optional) {
  //int length;
    //   char *str=NULL;
    //if(SCM_STRINGP(optional)){
    //str = gh_scm2newstr(optional, &length);
    //  }
 DenemoGUI *gui = Denemo.gui;
 DenemoObject *curObj;
 chord *thechord;
 note *thenote;
 if(!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type!=CHORD) || !(thechord = (chord *)  curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
   return scm_makfrom0str ("");
 else {
   SCM scm = scm_makfrom0str (g_strdup_printf("%s",  mid_c_offsettolily (thenote->mid_c_offset, thenote->enshift)));//FIXME a dedicated function avoiding memory leak.
   return scm;
 }
   
}

SCM scheme_get_note_as_midi(void) {
 DenemoGUI *gui = Denemo.gui;
 DenemoObject *curObj;
 chord *thechord;
 note *thenote;
 if(!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type!=CHORD) || !(thechord = (chord *)  curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
   return scm_int2num (0);
 else {
   gint midi = dia_to_midinote (thenote->mid_c_offset) + thenote->enshift;
   SCM scm = scm_int2num (midi);
   return scm;
   }
}


SCM scheme_get_notes (SCM optional) {
 DenemoGUI *gui = Denemo.gui;
 DenemoObject *curObj;
 chord *thechord;
 note *thenote;
 SCM scm;
 if(!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type!=CHORD) || !(thechord = (chord *)  curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
   return scm_makfrom0str ("");
 else {
   GList *g;
   GString *str = g_string_new("");
   for(g=thechord->notes;g;g=g->next) {
     thenote =  (note *) g->data;
     gchar *name =  g_strdup_printf("%s",  mid_c_offsettolily (thenote->mid_c_offset, thenote->enshift));
     str = g_string_insert_len (str, -1,/* at end */ name, 1+strlen(name)/*include the NULLs as delimiters for scheme */);
   }
   scm = gh_str2scm /*scm_from_locale_stringn */(str->str, str->len);
   return scm;
 } 
}


SCM scheme_get_user_input(SCM label, SCM prompt, SCM init) {
  gchar *title, *instruction, *initial_value;
  gint length;
  //FIXME scm_dynwind_begin (0);

if(SCM_STRINGP(label)){
  title = scm_to_locale_string(label);
  //scm_dynwind_free (title);
  }
 else title = "Input Required";
 if(SCM_STRINGP(prompt)){
  instruction = scm_to_locale_string(prompt);
  //scm_dynwind_free (instruction);
  }
 else instruction = "Give input: ";

 if(SCM_STRINGP(init)){
  initial_value = scm_to_locale_string(init);
  //scm_dynwind_free (initial_value);
  }
 else initial_value = " ";//FIXME mixed types of string, memory leaks
 
 gchar * ret = string_dialog_entry_with_widget (Denemo.gui, title, instruction, initial_value, NULL);
 SCM scm = scm_makfrom0str (ret);
 //scm_dynwind_end ();
 return scm;
}


SCM scheme_warningdialog(SCM msg) {
  gchar *title;
  gint length;
if(SCM_STRINGP(msg)){
  title = scm_to_locale_string(msg);//scm_dynwind_free (title)
  }
 else title = "Script generated warning";//FIXME mixed types of string, memory leaks
 
 warningdialog (title);
 //scm_dynwind_end ();
 return msg;
}


SCM scheme_get_char(void) {

  GdkEventKey event;
 gboolean success = intercept_scorearea_keypress(&event);
 gchar *str = g_strdup_printf("%c", success?event.keyval:0);
 SCM scm = scm_makfrom0str (str);
 g_free(str);
 return  scm;
}

SCM scheme_get_keypress(void) {
 GdkEventKey event;
 (void) intercept_scorearea_keypress(&event);
 gchar *str = dnm_accelerator_name(event.keyval, event.state);
 SCM scm = scm_makfrom0str (str);
 g_free(str);
 return  scm;
}

/* get last keypress that successfully invoked a command */
SCM scheme_get_command_keypress(void) {
 gchar *str = dnm_accelerator_name(Denemo.last_keyval, Denemo.last_keystate);
 SCM scm = scm_makfrom0str (str);
 g_free(str);
 return  scm;
}



SCM scheme_get_command(void) {
  GdkEventKey event;
 GString *name=g_string_new("");
 gboolean success = intercept_scorearea_keypress(&event);
 if(success) {
   gint cmd = lookup_command_for_keyevent (&event);
   //g_print("command %d for %x %x\n", cmd, event.keyval, event.state);
   if(cmd!=-1)
     name = g_string_append(name, lookup_name_from_idx (Denemo.map, cmd));//FIXME NULL?, memory leaks
   name = g_string_prepend (name, DENEMO_SCHEME_PREFIX);
  }
 SCM scm = scm_makfrom0str (name->str);
 g_string_free(name, TRUE);
 return  scm;
}

static get_drag_offset(GtkWidget *dialog, gint response_id, GtkLabel *label) {
  g_object_set_data(G_OBJECT(dialog), "offset-response", (gpointer)response_id);
  if(response_id < 0)
    gtk_main_quit();
  gint offsetx, offsety;
  offsetx =  (gint)g_object_get_data(G_OBJECT(Denemo.gui->printarea), "offsetx");
  offsety =  (gint)g_object_get_data(G_OBJECT(Denemo.gui->printarea), "offsety");
  gchar *text = g_strdup_printf("Offset now %d %d. Drag again in the print window to change\nOr click OK to apply the position shift", offsetx, offsety);
  gtk_label_set_text(label, text);
  g_free(text);
}

static get_drag_pad(GtkWidget *dialog, gint response_id, GtkLabel *label) {
  g_object_set_data(G_OBJECT(dialog), "pad-response", (gpointer)response_id);
  if(response_id < 0)
    gtk_main_quit();
  gint padding;
  padding =  (gint)g_object_get_data(G_OBJECT(Denemo.gui->printarea), "padding");
  gchar *text = g_strdup_printf("Padding now %d. Drag again in the print window to change\nOr click OK to apply the padding to the graphical object belonging to the directive", padding);
  gtk_label_set_text(label, text);
  g_free(text);
}



/* return a pair x, y representing the offset desired for some lilypond graphic
 or #f if no printarea or user cancels*/
SCM scheme_get_offset(void) {
  SCM x, y, ret;
  if(Denemo.gui->printarea==NULL)
    return SCM_BOOL(FALSE);
  if(g_object_get_data(G_OBJECT(Denemo.gui->printarea), "offset-dialog")){
    warningdialog("Already in a padding dialog");
    return SCM_BOOL_F;
  }
  gint offsetx = (gint)g_object_get_data(G_OBJECT(Denemo.gui->printarea), "offsetx");
  gint offsety = (gint)g_object_get_data(G_OBJECT(Denemo.gui->printarea), "offsety");


  GtkWidget *dialog = gtk_dialog_new_with_buttons ("Select Offset in Print Window",
                                        GTK_WINDOW (Denemo.window),
                                        (GtkDialogFlags) (GTK_DIALOG_DESTROY_WITH_PARENT),
                                        GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
                                        NULL);
  g_object_set_data(G_OBJECT(Denemo.gui->printarea), "offset-dialog", (gpointer)dialog);
  GtkWidget *vbox = gtk_vbox_new(FALSE, 8);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), vbox,
		      TRUE, TRUE, 0);
  gchar *text = g_strdup_printf("Current offset %d, %d\nDrag in print window to change this\nClick OK to apply the position shift to the directive", offsetx, -offsety);
  GtkWidget *label = gtk_label_new(text);
  g_free(text);
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, TRUE, 0);
  gtk_widget_show_all (dialog);

  gint val;

  g_signal_connect(dialog, "response", G_CALLBACK(get_drag_offset), label);
  gtk_widget_show_all(dialog);
  gtk_main();
  offsetx = (gint) g_object_get_data(G_OBJECT(Denemo.gui->printarea), "offsetx");
  offsety = (gint) g_object_get_data(G_OBJECT(Denemo.gui->printarea), "offsety");
  val =  (gint)g_object_get_data(G_OBJECT(dialog), "offset-response");
  g_object_set_data(G_OBJECT(Denemo.gui->printarea), "offset-dialog", NULL);
  gtk_widget_destroy(dialog);
  if(val == GTK_RESPONSE_ACCEPT) {
    x= scm_makfrom0str (g_strdup_printf("%.1f", offsetx/10.0));
    y= scm_makfrom0str (g_strdup_printf("%.1f", -offsety/10.0));
    ret = scm_cons(x, y);
  } else
    ret = SCM_BOOL(FALSE);//FIXME add a RESET button for which return TRUE to reset the overall offset to zero.
  return ret;
}

/* return a string representing the relative font size the user wishes to use*/
SCM scheme_get_relative_font_size(void) {
  if(Denemo.gui->printarea==NULL)
    return SCM_BOOL(FALSE);
  gchar *value = g_object_get_data(G_OBJECT(Denemo.gui->printarea), "font-size");
  if(value)
    g_free(value);
  value = string_dialog_entry (Denemo.gui, "Font Size", "Give a value (+/-) to adjust font size by", "0");
  if(!value)
    value = g_strdup("0");
  gchar *clean = g_strdup_printf("%d", atoi(value));
  g_free(value);
  g_object_set_data(G_OBJECT(Denemo.gui->printarea), "font-size", (gpointer)clean);
  return gh_str2scm (clean, strlen(clean));
}

/* return a string representing the padding desired for some lilypond graphic
 or #f if no printarea or user cancels*/
SCM scheme_get_padding(void) {
  SCM pad, ret;
  if(Denemo.gui->printarea==NULL)
    return SCM_BOOL(FALSE);
  if(g_object_get_data(G_OBJECT(Denemo.gui->printarea), "pad-dialog")){
    warningdialog("Already in a padding dialog");
    return SCM_BOOL_F;
  }
     
  gint padding = (gint)g_object_get_data(G_OBJECT(Denemo.gui->printarea), "padding");

  GtkWidget *dialog = gtk_dialog_new_with_buttons ("Select Padding in Print Window",
                                        GTK_WINDOW (Denemo.window),
                                        (GtkDialogFlags) (GTK_DIALOG_DESTROY_WITH_PARENT),
                                        GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
                                        NULL);
  g_object_set_data(G_OBJECT(Denemo.gui->printarea), "pad-dialog", (gpointer)dialog);
  GtkWidget *vbox = gtk_vbox_new(FALSE, 8);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), vbox,
		      TRUE, TRUE, 0);
  gchar *text = g_strdup_printf("Current padding is %d\nUse right click in print window to change this\nClick OK to apply the padding to the music item drawn by the directive", padding);
  GtkWidget *label = gtk_label_new(text);
  g_free(text);
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, TRUE, 0);
  gtk_widget_show_all (dialog);

  gint val;

  g_signal_connect(dialog, "response", G_CALLBACK(get_drag_pad), label);
  gtk_widget_show_all(dialog);
  gtk_main();
  padding = (gint) g_object_get_data(G_OBJECT(Denemo.gui->printarea), "padding");
  val =  (gint)g_object_get_data(G_OBJECT(dialog), "pad-response");
  g_object_set_data(G_OBJECT(Denemo.gui->printarea), "pad-dialog", NULL);
  gtk_widget_destroy(dialog);
  if(val == GTK_RESPONSE_ACCEPT) {
    ret = scm_makfrom0str (g_strdup_printf("%d", padding/10));
  } else
    ret = SCM_BOOL(FALSE);
  return ret;
}



/* create a dialog with the options & return the one chosen, of #f if
   the user cancels
*/
SCM scheme_get_option(SCM options) {
  SCM scm;
  gchar *response;
  gint length;
  gchar *str=NULL;
  if(SCM_STRINGP(options)){
    str = gh_scm2newstr(options, &length);
    response = get_option(str, length);
  }
  if(response)
    scm = gh_str2scm (response, strlen(response));
  else scm = SCM_BOOL(FALSE);
  return  scm;
}


/* Scheme interface to DenemoDirectives (formerly LilyPond directives attached to notes/chords) */

#define EDIT_DELETE_FN_DEF(what)\
 static SCM scheme_delete_##what##_directive(SCM tag) {\
  if(!SCM_STRINGP(tag))\
     return SCM_BOOL(FALSE);\
  gchar *tagname = scm_to_locale_string(tag);\
  return SCM_BOOL( delete_##what##_directive (tagname));\
}\
 static SCM scheme_text_edit_##what##_directive(SCM tag) {\
  if(!SCM_STRINGP(tag))\
     return SCM_BOOL(FALSE);\
  gchar *tagname = scm_to_locale_string(tag);\
  return SCM_BOOL( text_edit_##what##_directive (tagname));\
}



EDIT_DELETE_FN_DEF(note)
EDIT_DELETE_FN_DEF(chord)
EDIT_DELETE_FN_DEF(staff)
EDIT_DELETE_FN_DEF(voice)
EDIT_DELETE_FN_DEF(score)


#define GETFUNC_DEF(what, field)\
static SCM scheme_##what##_directive_get_##field(SCM tag) {\
  if(!SCM_STRINGP(tag))\
     return SCM_BOOL(FALSE);\
  gchar *tagname = scm_to_locale_string(tag);\
  gchar *value = what##_directive_get_##field(tagname);\
  if(value)\
    return scm_makfrom0str(value);\
  return SCM_BOOL(FALSE);\
}
#define PUTFUNC_DEF(what, field)\
static SCM scheme_##what##_directive_put_##field(SCM tag, SCM value) {\
  if((!SCM_STRINGP(tag))||(!SCM_STRINGP(value)))\
     return SCM_BOOL(FALSE);\
  gchar *tagname = scm_to_locale_string(tag);\
  gchar *valuename = scm_to_locale_string(value);\
  return SCM_BOOL(what##_directive_put_##field (tagname, valuename));\
}



//block to clone for new GString entries in DenemoDirective
GETFUNC_DEF(note, display)
GETFUNC_DEF(chord, display)
GETFUNC_DEF(standalone, display)
GETFUNC_DEF(staff, display)
GETFUNC_DEF(voice, display)
GETFUNC_DEF(score, display)
GETFUNC_DEF(movementcontrol, display)

PUTFUNC_DEF(note, display)
PUTFUNC_DEF(chord, display)
PUTFUNC_DEF(standalone, display)
PUTFUNC_DEF(staff, display)
PUTFUNC_DEF(voice, display)
PUTFUNC_DEF(score, display)
PUTFUNC_DEF(movementcontrol, display)
// end of block to clone


GETFUNC_DEF(note, midibytes)
GETFUNC_DEF(chord, midibytes)
GETFUNC_DEF(standalone, midibytes)
GETFUNC_DEF(staff, midibytes)
GETFUNC_DEF(score, midibytes)
GETFUNC_DEF(movementcontrol, midibytes)

PUTFUNC_DEF(note, midibytes)
PUTFUNC_DEF(chord, midibytes)
PUTFUNC_DEF(standalone, midibytes)
PUTFUNC_DEF(staff, midibytes)
PUTFUNC_DEF(score, midibytes)
PUTFUNC_DEF(movementcontrol, midibytes)



GETFUNC_DEF(note, prefix)
GETFUNC_DEF(note, postfix)
PUTFUNC_DEF(note, prefix)
     //PUTFUNC_DEF(clef, prefix)
PUTFUNC_DEF(note, postfix)

GETFUNC_DEF(score, prefix)
GETFUNC_DEF(score, postfix)
PUTFUNC_DEF(score, prefix)
PUTFUNC_DEF(score, postfix)


PUTFUNC_DEF(staff, prefix)
PUTFUNC_DEF(voice, prefix)
GETFUNC_DEF(staff, prefix)
GETFUNC_DEF(voice, prefix)

PUTFUNC_DEF(staff, postfix)
PUTFUNC_DEF(voice, postfix)
GETFUNC_DEF(staff, postfix)
GETFUNC_DEF(voice, postfix)

GETFUNC_DEF(chord, prefix)
GETFUNC_DEF(chord, postfix)
PUTFUNC_DEF(chord, prefix)
PUTFUNC_DEF(chord, postfix)

GETFUNC_DEF(standalone, prefix)
GETFUNC_DEF(standalone, postfix)
PUTFUNC_DEF(standalone, prefix)
PUTFUNC_DEF(standalone, postfix)


#define INT_PUTFUNC_DEF(what, field)\
static SCM scheme_##what##_directive_put_##field(SCM tag, SCM value) {\
  if((!SCM_STRINGP(tag))||(!scm_integer_p(value)))\
     return SCM_BOOL(FALSE);\
  gchar *tagname = scm_to_locale_string(tag);\
  gint valuename = scm_num2int(value, 0, 0);\
  return SCM_BOOL(what##_directive_put_##field (tagname, valuename));\
}
#define INT_GETFUNC_DEF(what, field)\
static SCM scheme_##what##_directive_get_##field(SCM tag) {\
  if(!SCM_STRINGP(tag))\
     return SCM_BOOL(FALSE);\
  gchar *tagname = scm_to_locale_string(tag);\
  return scm_int2num(what##_directive_get_##field (tagname));\
}


#define PUTGRAPHICFUNC_DEF(what)\
static SCM scheme_##what##_directive_put_graphic(SCM tag, SCM value) {\
  if((!SCM_STRINGP(tag))||(!SCM_STRINGP(value)))\
     return SCM_BOOL(FALSE);\
  gchar *tagname = scm_to_locale_string(tag);\
  gchar *valuename = scm_to_locale_string(value);\
  return SCM_BOOL(what##_directive_put_graphic (tagname, valuename));\
}

PUTGRAPHICFUNC_DEF(note);
PUTGRAPHICFUNC_DEF(chord);
PUTGRAPHICFUNC_DEF(standalone);
PUTGRAPHICFUNC_DEF(staff);
PUTGRAPHICFUNC_DEF(voice);
PUTGRAPHICFUNC_DEF(score);


     //block to copy for new int field in directive
INT_PUTFUNC_DEF(note, minpixels)
INT_PUTFUNC_DEF(chord, minpixels)
INT_PUTFUNC_DEF(standalone, minpixels)
INT_PUTFUNC_DEF(staff, minpixels)
INT_PUTFUNC_DEF(voice, minpixels)
INT_PUTFUNC_DEF(score, minpixels)
INT_PUTFUNC_DEF(clef, minpixels)
INT_PUTFUNC_DEF(timesig, minpixels)
INT_PUTFUNC_DEF(keysig, minpixels)

INT_PUTFUNC_DEF(scoreheader, minpixels)
INT_PUTFUNC_DEF(header, minpixels)
INT_PUTFUNC_DEF(paper, minpixels)
INT_PUTFUNC_DEF(layout, minpixels)
INT_PUTFUNC_DEF(movementcontrol, minpixels)

INT_GETFUNC_DEF(note, minpixels)
INT_GETFUNC_DEF(chord, minpixels)
INT_GETFUNC_DEF(standalone, minpixels)
INT_GETFUNC_DEF(staff, minpixels)
INT_GETFUNC_DEF(voice, minpixels)
INT_GETFUNC_DEF(score, minpixels)
INT_GETFUNC_DEF(clef, minpixels)
INT_GETFUNC_DEF(timesig, minpixels)
INT_GETFUNC_DEF(keysig, minpixels)

INT_GETFUNC_DEF(scoreheader, minpixels)
INT_GETFUNC_DEF(header, minpixels)
INT_GETFUNC_DEF(paper, minpixels)
INT_GETFUNC_DEF(layout, minpixels)
INT_GETFUNC_DEF(movementcontrol, minpixels)

     //end block to ocpy for new int field in directive






INT_PUTFUNC_DEF(note, override)
INT_PUTFUNC_DEF(chord, override)
INT_PUTFUNC_DEF(standalone, override)
INT_PUTFUNC_DEF(staff, override)
INT_PUTFUNC_DEF(voice, override)
INT_PUTFUNC_DEF(score, override)
INT_GETFUNC_DEF(note, override)
INT_GETFUNC_DEF(chord, override)
INT_GETFUNC_DEF(standalone, override)
INT_GETFUNC_DEF(staff, override)
INT_GETFUNC_DEF(voice, override)
INT_GETFUNC_DEF(score, override)






INT_PUTFUNC_DEF(note, y)
INT_PUTFUNC_DEF(chord, y)
INT_PUTFUNC_DEF(standalone, y)
INT_GETFUNC_DEF(note, y)
INT_GETFUNC_DEF(chord, y)
INT_GETFUNC_DEF(standalone, y)
INT_PUTFUNC_DEF(note, x)
INT_PUTFUNC_DEF(chord, x)
INT_PUTFUNC_DEF(standalone, x)
INT_GETFUNC_DEF(note, x)
INT_GETFUNC_DEF(chord, x)
INT_GETFUNC_DEF(standalone, x)

INT_PUTFUNC_DEF(note, ty)
INT_PUTFUNC_DEF(chord, ty)
INT_PUTFUNC_DEF(standalone, ty)
INT_GETFUNC_DEF(note, ty)
INT_GETFUNC_DEF(chord, ty)
INT_GETFUNC_DEF(standalone, ty)
INT_PUTFUNC_DEF(note, tx)
INT_PUTFUNC_DEF(chord, tx)
INT_PUTFUNC_DEF(standalone, tx)
INT_GETFUNC_DEF(note, tx)
INT_GETFUNC_DEF(chord, tx)
INT_GETFUNC_DEF(standalone, tx)

INT_PUTFUNC_DEF(note, gy)
INT_PUTFUNC_DEF(chord, gy)
INT_PUTFUNC_DEF(standalone, gy)
INT_GETFUNC_DEF(note, gy)
INT_GETFUNC_DEF(chord, gy)
INT_GETFUNC_DEF(standalone, gy)
INT_PUTFUNC_DEF(note, gx)
INT_PUTFUNC_DEF(chord, gx)
INT_PUTFUNC_DEF(standalone, gx)
INT_GETFUNC_DEF(note, gx)
INT_GETFUNC_DEF(chord, gx)
INT_GETFUNC_DEF(standalone, gx)

INT_GETFUNC_DEF(note, width)
INT_GETFUNC_DEF(chord, width)
INT_GETFUNC_DEF(standalone, width)
INT_GETFUNC_DEF(note, height)
INT_GETFUNC_DEF(chord, height)
INT_GETFUNC_DEF(standalone, height)
 
INT_GETFUNC_DEF(score, x)
INT_GETFUNC_DEF(score, y)
INT_GETFUNC_DEF(score, tx)
INT_GETFUNC_DEF(score, ty)
INT_GETFUNC_DEF(score, gx)
INT_GETFUNC_DEF(score, gy)
INT_GETFUNC_DEF(score, width)
INT_GETFUNC_DEF(score, height)

INT_PUTFUNC_DEF(score, x)
INT_PUTFUNC_DEF(score, y)
INT_PUTFUNC_DEF(score, tx)
INT_PUTFUNC_DEF(score, ty)
INT_PUTFUNC_DEF(score, gx)
INT_PUTFUNC_DEF(score, gy)
INT_PUTFUNC_DEF(score, width)
INT_PUTFUNC_DEF(score, height)



     // block to copy for new type of directive, !!minpixels is done in block to copy for new fields!!
GETFUNC_DEF(clef, prefix)
GETFUNC_DEF(clef, postfix)
GETFUNC_DEF(clef, display)
PUTFUNC_DEF(clef, prefix)
PUTFUNC_DEF(clef, postfix)
PUTFUNC_DEF(clef, display)
PUTGRAPHICFUNC_DEF(clef);

INT_PUTFUNC_DEF(clef, x)
INT_PUTFUNC_DEF(clef, y)
INT_PUTFUNC_DEF(clef, tx)
INT_PUTFUNC_DEF(clef, ty)
INT_PUTFUNC_DEF(clef, gx)
INT_PUTFUNC_DEF(clef, gy)
INT_PUTFUNC_DEF(clef, override)
INT_GETFUNC_DEF(clef, x)
INT_GETFUNC_DEF(clef, y)
INT_GETFUNC_DEF(clef, tx)
INT_GETFUNC_DEF(clef, ty)
INT_GETFUNC_DEF(clef, gx)
INT_GETFUNC_DEF(clef, gy)
INT_GETFUNC_DEF(clef, override)
INT_GETFUNC_DEF(clef, width)
INT_GETFUNC_DEF(clef, height)
EDIT_DELETE_FN_DEF(clef)
     // end block

GETFUNC_DEF(timesig, prefix)
GETFUNC_DEF(timesig, postfix)
GETFUNC_DEF(timesig, display)
PUTFUNC_DEF(timesig, prefix)
PUTFUNC_DEF(timesig, postfix)
PUTFUNC_DEF(timesig, display)
PUTGRAPHICFUNC_DEF(timesig);

INT_PUTFUNC_DEF(timesig, x)
INT_PUTFUNC_DEF(timesig, y)
INT_PUTFUNC_DEF(timesig, tx)
INT_PUTFUNC_DEF(timesig, ty)
INT_PUTFUNC_DEF(timesig, gx)
INT_PUTFUNC_DEF(timesig, gy)
INT_PUTFUNC_DEF(timesig, override)
INT_GETFUNC_DEF(timesig, x)
INT_GETFUNC_DEF(timesig, y)
INT_GETFUNC_DEF(timesig, tx)
INT_GETFUNC_DEF(timesig, ty)
INT_GETFUNC_DEF(timesig, gx)
INT_GETFUNC_DEF(timesig, gy)
INT_GETFUNC_DEF(timesig, override)
INT_GETFUNC_DEF(timesig, width)
INT_GETFUNC_DEF(timesig, height)
EDIT_DELETE_FN_DEF(timesig)

GETFUNC_DEF(keysig, prefix)
GETFUNC_DEF(keysig, postfix)
GETFUNC_DEF(keysig, display)
PUTFUNC_DEF(keysig, prefix)
PUTFUNC_DEF(keysig, postfix)
PUTFUNC_DEF(keysig, display)
PUTGRAPHICFUNC_DEF(keysig);

INT_PUTFUNC_DEF(keysig, x)
INT_PUTFUNC_DEF(keysig, y)
INT_PUTFUNC_DEF(keysig, tx)
INT_PUTFUNC_DEF(keysig, ty)
INT_PUTFUNC_DEF(keysig, gx)
INT_PUTFUNC_DEF(keysig, gy)
INT_PUTFUNC_DEF(keysig, override)
INT_GETFUNC_DEF(keysig, x)
INT_GETFUNC_DEF(keysig, y)
INT_GETFUNC_DEF(keysig, tx)
INT_GETFUNC_DEF(keysig, ty)
INT_GETFUNC_DEF(keysig, gx)
INT_GETFUNC_DEF(keysig, gy)
INT_GETFUNC_DEF(keysig, override)
INT_GETFUNC_DEF(keysig, width)
INT_GETFUNC_DEF(keysig, height)
EDIT_DELETE_FN_DEF(keysig)


GETFUNC_DEF(scoreheader, prefix)
GETFUNC_DEF(scoreheader, postfix)
GETFUNC_DEF(scoreheader, display)
PUTFUNC_DEF(scoreheader, prefix)
PUTFUNC_DEF(scoreheader, postfix)
PUTFUNC_DEF(scoreheader, display)
PUTGRAPHICFUNC_DEF(scoreheader);

INT_PUTFUNC_DEF(scoreheader, x)
INT_PUTFUNC_DEF(scoreheader, y)
INT_PUTFUNC_DEF(scoreheader, tx)
INT_PUTFUNC_DEF(scoreheader, ty)
INT_PUTFUNC_DEF(scoreheader, gx)
INT_PUTFUNC_DEF(scoreheader, gy)
INT_PUTFUNC_DEF(scoreheader, override)
INT_GETFUNC_DEF(scoreheader, x)
INT_GETFUNC_DEF(scoreheader, y)
INT_GETFUNC_DEF(scoreheader, tx)
INT_GETFUNC_DEF(scoreheader, ty)
INT_GETFUNC_DEF(scoreheader, gx)
INT_GETFUNC_DEF(scoreheader, gy)
INT_GETFUNC_DEF(scoreheader, override)
INT_GETFUNC_DEF(scoreheader, width)
INT_GETFUNC_DEF(scoreheader, height)
EDIT_DELETE_FN_DEF(scoreheader)


GETFUNC_DEF(header, prefix)
GETFUNC_DEF(header, postfix)
GETFUNC_DEF(header, display)
PUTFUNC_DEF(header, prefix)
PUTFUNC_DEF(header, postfix)
PUTFUNC_DEF(header, display)
PUTGRAPHICFUNC_DEF(header);

INT_PUTFUNC_DEF(header, x)
INT_PUTFUNC_DEF(header, y)
INT_PUTFUNC_DEF(header, tx)
INT_PUTFUNC_DEF(header, ty)
INT_PUTFUNC_DEF(header, gx)
INT_PUTFUNC_DEF(header, gy)
INT_PUTFUNC_DEF(header, override)
INT_GETFUNC_DEF(header, x)
INT_GETFUNC_DEF(header, y)
INT_GETFUNC_DEF(header, tx)
INT_GETFUNC_DEF(header, ty)
INT_GETFUNC_DEF(header, gx)
INT_GETFUNC_DEF(header, gy)
INT_GETFUNC_DEF(header, override)
INT_GETFUNC_DEF(header, width)
INT_GETFUNC_DEF(header, height)
EDIT_DELETE_FN_DEF(header)


GETFUNC_DEF(paper, prefix)
GETFUNC_DEF(paper, postfix)
GETFUNC_DEF(paper, display)
PUTFUNC_DEF(paper, prefix)
PUTFUNC_DEF(paper, postfix)
PUTFUNC_DEF(paper, display)
PUTGRAPHICFUNC_DEF(paper);

INT_PUTFUNC_DEF(paper, x)
INT_PUTFUNC_DEF(paper, y)
INT_PUTFUNC_DEF(paper, tx)
INT_PUTFUNC_DEF(paper, ty)
INT_PUTFUNC_DEF(paper, gx)
INT_PUTFUNC_DEF(paper, gy)
INT_PUTFUNC_DEF(paper, override)
INT_GETFUNC_DEF(paper, x)
INT_GETFUNC_DEF(paper, y)
INT_GETFUNC_DEF(paper, tx)
INT_GETFUNC_DEF(paper, ty)
INT_GETFUNC_DEF(paper, gx)
INT_GETFUNC_DEF(paper, gy)
INT_GETFUNC_DEF(paper, override)
INT_GETFUNC_DEF(paper, width)
INT_GETFUNC_DEF(paper, height)
EDIT_DELETE_FN_DEF(paper)


GETFUNC_DEF(layout, prefix)
GETFUNC_DEF(layout, postfix)
GETFUNC_DEF(layout, display)
PUTFUNC_DEF(layout, prefix)
PUTFUNC_DEF(layout, postfix)
PUTFUNC_DEF(layout, display)
PUTGRAPHICFUNC_DEF(layout);

INT_PUTFUNC_DEF(layout, x)
INT_PUTFUNC_DEF(layout, y)
INT_PUTFUNC_DEF(layout, tx)
INT_PUTFUNC_DEF(layout, ty)
INT_PUTFUNC_DEF(layout, gx)
INT_PUTFUNC_DEF(layout, gy)
INT_PUTFUNC_DEF(layout, override)
INT_GETFUNC_DEF(layout, x)
INT_GETFUNC_DEF(layout, y)
INT_GETFUNC_DEF(layout, tx)
INT_GETFUNC_DEF(layout, ty)
INT_GETFUNC_DEF(layout, gx)
INT_GETFUNC_DEF(layout, gy)
INT_GETFUNC_DEF(layout, override)
INT_GETFUNC_DEF(layout, width)
INT_GETFUNC_DEF(layout, height)
EDIT_DELETE_FN_DEF(layout)


GETFUNC_DEF(movementcontrol, prefix)
GETFUNC_DEF(movementcontrol, postfix)

PUTFUNC_DEF(movementcontrol, prefix)
PUTFUNC_DEF(movementcontrol, postfix)

PUTGRAPHICFUNC_DEF(movementcontrol);

INT_PUTFUNC_DEF(movementcontrol, x)
INT_PUTFUNC_DEF(movementcontrol, y)
INT_PUTFUNC_DEF(movementcontrol, tx)
INT_PUTFUNC_DEF(movementcontrol, ty)
INT_PUTFUNC_DEF(movementcontrol, gx)
INT_PUTFUNC_DEF(movementcontrol, gy)
INT_PUTFUNC_DEF(movementcontrol, override)
INT_GETFUNC_DEF(movementcontrol, x)
INT_GETFUNC_DEF(movementcontrol, y)
INT_GETFUNC_DEF(movementcontrol, tx)
INT_GETFUNC_DEF(movementcontrol, ty)
INT_GETFUNC_DEF(movementcontrol, gx)
INT_GETFUNC_DEF(movementcontrol, gy)
INT_GETFUNC_DEF(movementcontrol, override)
INT_GETFUNC_DEF(movementcontrol, width)
INT_GETFUNC_DEF(movementcontrol, height)
EDIT_DELETE_FN_DEF(movementcontrol)


SCM scheme_get_midi(void) {
 gint midi;
 gboolean success = intercept_midi_event(&midi);
 if(!success)
   midi = 0;/* scripts should detect this impossible value and take action */
 SCM scm = scm_int2num (midi);
 return  scm;
}



SCM scheme_put_midi (SCM scm) {
  gchar buf[3];
  gint midi = scm_num2int(scm, 0, 0);

  buf[0] = midi & 0xFF;
  buf[1] = (midi>>8)&0xFF;
  buf[2] = (midi>>16)&0xFF;
  //g_print("got %x\nbreaks as %x %x %x\n", midi&0xFFFFFF, buf[0], buf[1], buf[2]);
  process_midi_event(buf);
  pitchentry(Denemo.gui);// this ensures any note is acted on before returning
 return SCM_BOOL(TRUE);
}


static SCM scheme_play_midikey(SCM scm) {
    guint midi = scm_num2int(scm, 0, 0);
    gint key =  (midi>>8)&0xFF;
    gint channel = midi&0xF;
    double volume = ((midi>>16)&0xFF)/255.0;
    //g_print("Playing %x at %f volume, %d channel\n", key, (double)volume, channel);
    play_midikey(key, 0.2, volume, channel);
    g_usleep(200000);
 return SCM_BOOL(TRUE);
}

static SCM scheme_bass_figure(SCM bass, SCM harmony) {
  gint bassnum = scm_num2int(bass, 0, 0);
  gint harmonynum = scm_num2int(harmony, 0, 0);
  gchar *interval = determine_interval(bassnum, harmonynum);
  SCM ret= scm_makfrom0str(interval);
  g_free(interval);
  return ret;
}


gint name2mid_c_offset(gchar *x, gint *mid_c_offset, gint *enshift) {
  g_print("Mid c offset of %d\n", *x-'c');
  gchar *c;
  gint octave = -2;/* middle c is c'' */
  gint accs = 0;

  for(c = x+1;*c;c++){
    if(*c=='i'&& *(c+1)=='s') {
      accs++;
      c++; ;
    } else if(*c=='e'&& *(c+1)=='s') {
      accs--;
      c++;
    } else if (
      *c==',') {
      octave--;
    } else if (*c=='\'') {
	octave++;
    }
  }
  *mid_c_offset = *x-'c' + 7*octave;
  *enshift = accs;
}

static SCM scheme_put_note_name (SCM optional) {

 DenemoGUI *gui = Denemo.gui;
 DenemoObject *curObj;
 chord *thechord;
 note *thenote;
 if(!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type!=CHORD) || !(thechord = (chord *)  curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
   return SCM_BOOL(FALSE);
 else {
 //FIXME scm_dynwind_begin (0); etc
   char *str=NULL;
   if(SCM_STRINGP(optional)){
     str = scm_to_locale_string(optional);
     gint mid_c_offset;
     gint enshift;
     name2mid_c_offset(str, &mid_c_offset, &enshift);
     //g_print("note %s gives %d and %d\n", str, mid_c_offset, enshift);
     modify_note(thechord, mid_c_offset, enshift,  find_prevailing_clef(Denemo.gui->si));
     //thenote->mid_c_offset = name2mid_c_offset(str);
     displayhelper(Denemo.gui);
   return SCM_BOOL(TRUE);
  }
 }
 return SCM_BOOL(FALSE);  
}

static SCM scheme_get_type (SCM optional) {
 DenemoGUI *gui = Denemo.gui;
 DenemoObject *curObj;
 if(!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || !(DENEMO_OBJECT_TYPE_NAME(curObj)))
   return scm_makfrom0str("None");
 if(Denemo.gui->si->cursor_appending)
    return  scm_makfrom0str("Appending");
 return  scm_makfrom0str(DENEMO_OBJECT_TYPE_NAME(curObj));
}





/* shifts the note at the cursor by the number of diatonic steps passed in */
SCM scheme_diatonic_shift (SCM optional) {
 DenemoGUI *gui = Denemo.gui;
 DenemoObject *curObj;
 chord *thechord;
 note *thenote;
 if(!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type!=CHORD) || !(thechord = (chord *)  curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
   return SCM_BOOL(FALSE);
 else {
   //FIXME scm_dynwind_begin (0); etc
   char *str=NULL;
   if(SCM_STRINGP(optional)){
     str = scm_to_locale_string(optional);
     gint shift;
     sscanf(str, "%d", &shift);
     
     g_print("note shift %s ie %d\n", str, shift);
     modify_note(thechord, thenote->mid_c_offset+shift, gui->si->curmeasureaccs[offsettonumber(thenote->mid_c_offset+shift)],  find_prevailing_clef(Denemo.gui->si));
     //thenote->mid_c_offset = name2mid_c_offset(str);
     displayhelper(Denemo.gui);
   }
 }
 return SCM_BOOL(FALSE);  
}


static gboolean to_next_object(void) {
  DenemoGUI *gui = Denemo.gui;
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if(!Denemo.gui || !(Denemo.gui->si))
    return FALSE;
  GList *this = Denemo.gui->si->currentobject;
  if(!this)
    return FALSE;
  cursorright (NULL);//FIXME use value param->status
  if(this!= Denemo.gui->si->currentobject)
    return TRUE;
  if(Denemo.gui->si->cursor_appending)
    cursorright (NULL);
  if(this!= Denemo.gui->si->currentobject)
    return TRUE;
  return FALSE;  
}

/* moves currentobject to next object by calling cursorright.
   Steps over barlines (i.e. cursor_appending).
   returns TRUE if currentobject is different after than before doing the call
*/
SCM scheme_next_object (SCM optional) {
return SCM_BOOL(to_next_object());
}



SCM scheme_refresh_display (SCM optional) {
  displayhelper(Denemo.gui);
  score_status(Denemo.gui, TRUE);
  return SCM_BOOL(TRUE);
}

/* moves currentobject to next object in the selection.
   Steps over barlines (i.e. cursor_appending).
 returns TRUE if currentobject is different after than before the call
*/
SCM scheme_next_selected_object (SCM optional) {
  DenemoGUI *gui = Denemo.gui;
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if(!Denemo.gui || !(Denemo.gui->si))
    return SCM_BOOL(FALSE);
  save_selection(Denemo.gui->si);
  gboolean success = to_next_object();
  if(!success)
    success = to_next_object();//if at first you don't succeed. Better might be to examine cursor_appending...
  restore_selection(Denemo.gui->si);
  //g_print("success %d\n", success);
  if( (success) && in_selection(Denemo.gui->si))
    return SCM_BOOL(TRUE);
  return SCM_BOOL(FALSE);  
}

//(display (d-NextSelectedObject))





SCM scheme_next_chord (SCM optional) {
  SCM ret = scheme_next_object(optional);
  if(SCM_FALSEP(ret))
    return ret;
  if(Denemo.gui->si->currentobject && Denemo.gui->si->currentobject->data &&
    ((DenemoObject*) Denemo.gui->si->currentobject->data)->type == CHORD)
    return SCM_BOOL(TRUE);
  else
    return 
      scheme_next_chord (optional);
}
  // there is a significant problem with the concept of next note in a chord of several notes. We have no way of iterating over the notes of a chord
  // since the notes may be altered during the iteration and Denemo does not define a "currentnote"
//This next note is next chord that is not a rest.
SCM scheme_next_note (SCM optional) {
 SCM ret = scheme_next_chord(optional);
 if(SCM_FALSEP(ret))
    return ret;
 if(Denemo.gui->si->currentobject && Denemo.gui->si->currentobject->data &&
    ((DenemoObject*) Denemo.gui->si->currentobject->data)->type == CHORD && 
    ((((chord *)(((DenemoObject*) Denemo.gui->si->currentobject->data)->object))->notes))
    && (!Denemo.gui->si->cursor_appending))
   return SCM_BOOL(TRUE);
  else
    return 
      scheme_next_note (optional);
}

int process_command_line(int argc, char**argv);//back in main



static void denemo_scheme_init(void){
  gchar *filename = g_build_filename(get_data_dir(), "actions", "denemo.scm", NULL);

  if(g_file_test(filename, G_FILE_TEST_EXISTS))
    scm_c_primitive_load(filename);
  else
    g_warning("Cannot find Denemo's scheme initialization file denemo.scm");
  g_free(filename);
  filename = g_build_filename(locatedotdenemo(), "actions", "denemo.scm", NULL);
  if(g_file_test(filename, G_FILE_TEST_EXISTS))
    scm_c_primitive_load(filename);
  g_free(filename);
}



/* Called from main for scheme initialization reasons.
   calls back to finish command line processing
*/
void inner_main(void*closure, int argc, char **argv){
  //g_print("Got inner main with  %d and %p\n", argc, argv);
  
  gint i;
  GError *error = NULL;
  
  
  
  /* Initialize preferences */
  initprefs();
  readHistory();
  g_print("init prefs run");
  
  //create window system
  create_window();
  
  /* create the first tab */
  newtab (NULL, NULL);
  load_default_keymap_file();

  //insert mode on startup - should be a pref FIXME
  gtk_widget_show (Denemo.InsertModeMenu);
  gtk_widget_hide (Denemo.EditModeMenu);
  gtk_widget_hide (Denemo.ClassicModeMenu);
  gtk_widget_hide (Denemo.ModelessMenu);
  
  gtk_key_snooper_install(dnm_key_snooper, NULL);
  Denemo.accelerator_status = FALSE;
  /* create scheme identifiers for check/radio item to activate the items (ie not just run the callback) */
  for(i=0;i<G_N_ELEMENTS(activatable_commands);i++) {
    install_scm_function (g_strdup_printf(DENEMO_SCHEME_PREFIX"%s", activatable_commands[i].str), (gpointer)activatable_commands[i].p);
  }
  /* test with
     (d-EditMode)
     (d-2)
     (d-PutNoteName "cis''")
  */
  
  /* create scheme functions d-<name> for all the menuitem callbacks of <name> that are not check/radio items
     The scheme functions are defined to take one optional parameter which by denemo convention will be a String type,
     not necessarily null terminated, which is then passed as a GString * to the callback routines (with the first parameter, the GtkAction*, passed as NULL.
     Note that all such actions (that may be called back by scheme directly in this fashion) are given the attribute "scm" with value 1; I do not think this is being exploited in the code at present, and is perhaps not needed.
  */
#include "scheme.h"
  
  /* install the scheme function for calling actions which are scripts */
  install_scm_function_with_param (DENEMO_SCHEME_PREFIX"ScriptCallback", scheme_script_callback);
			
  install_scm_function_with_param (DENEMO_SCHEME_PREFIX"GetOption", scheme_get_option);
  /* test with (display (d-GetOption "this\0and\0that\0")) */
			
  install_scm_function (DENEMO_SCHEME_PREFIX"GetOffset",  scheme_get_offset);			
  install_scm_function (DENEMO_SCHEME_PREFIX"GetPadding",  scheme_get_padding);
  install_scm_function (DENEMO_SCHEME_PREFIX"GetRelativeFontSize",  scheme_get_relative_font_size);			
			/* install the scheme functions for calling extra Denemo functions created for the scripting interface */
  install_scm_function_with_param (DENEMO_SCHEME_PREFIX"InitializeScript", scheme_initialize_script);

  install_scm_function (DENEMO_SCHEME_PREFIX"GetType",  scheme_get_type);
  install_scm_function (DENEMO_SCHEME_PREFIX"GetCursorNote",  scheme_get_cursor_note);
  install_scm_function (DENEMO_SCHEME_PREFIX"DebugObject",  scheme_debug_object);

  install_scm_function (DENEMO_SCHEME_PREFIX"GetNoteName",  scheme_get_note_name);
  install_scm_function (DENEMO_SCHEME_PREFIX"GetNote",  scheme_get_note);
  install_scm_function (DENEMO_SCHEME_PREFIX"GetNotes",  scheme_get_notes);

  install_scm_function (DENEMO_SCHEME_PREFIX"PutNoteName",  scheme_put_note_name);
  install_scm_function (DENEMO_SCHEME_PREFIX"DiatonicShift", scheme_diatonic_shift);
  install_scm_function (DENEMO_SCHEME_PREFIX"NextObject", scheme_next_object);
  install_scm_function (DENEMO_SCHEME_PREFIX"NextSelectedObject", scheme_next_selected_object);
  install_scm_function (DENEMO_SCHEME_PREFIX"NextChord", scheme_next_chord);
  install_scm_function (DENEMO_SCHEME_PREFIX"NextNote", scheme_next_note);

  install_scm_function (DENEMO_SCHEME_PREFIX"Chordize",  scheme_chordize);
  // test with  (d-PutNoteName "e,,") (d-CursorRight) 
  // test with (d-DiatonicShift "3")  (d-CursorRight) 
  // test with (d-DiatonicShift "3")  (d-NextNote)
  /* test with 
(define this-proc (lambda () 
         (if (d-NextNote) 
	      (begin (d-DiatonicShift "2") (this-proc)))))

  (d-DiatonicShift "2")
  (this-proc)

*/


    install_scm_function3 (DENEMO_SCHEME_PREFIX"GetUserInput", scheme_get_user_input);
    /* test with
       (d-GetUserInput "Named Bookmark" "Give a name" "XXX")

Then 
     (define user-input (d-GetUserInput "Named Bookmark" "Give a name" "XXX"))
     (d-InsertLilyDirective (string-append "%" user-input))
    */

  install_scm_function (DENEMO_SCHEME_PREFIX"WarningDialog", scheme_warningdialog);
  install_scm_function (DENEMO_SCHEME_PREFIX"GetChar", scheme_get_char);
  install_scm_function (DENEMO_SCHEME_PREFIX"GetKeypress", scheme_get_keypress);
  install_scm_function (DENEMO_SCHEME_PREFIX"GetCommandKeypress", scheme_get_command_keypress);

  install_scm_function (DENEMO_SCHEME_PREFIX"GetCommand", scheme_get_command);


#define INSTALL_EDIT(what)\
 install_scm_function_with_param (DENEMO_SCHEME_PREFIX"DirectiveDelete"  "-" #what, scheme_delete_##what##_directive);\
 install_scm_function_with_param (DENEMO_SCHEME_PREFIX"DirectiveTextEdit"  "-" #what, scheme_text_edit_##what##_directive);
  INSTALL_EDIT(note);
  INSTALL_EDIT(chord);
  INSTALL_EDIT(staff);
  INSTALL_EDIT(voice);
  INSTALL_EDIT(score);


#define INSTALL_PUT(what, field)\
  install_scm_function2 (DENEMO_SCHEME_PREFIX"DirectivePut" "-" #what "-" #field, scheme_##what##_directive_put_##field);

#define INSTALL_GET(what, field)\
  install_scm_function_with_param (DENEMO_SCHEME_PREFIX"DirectiveGet" "-" #what "-" #field, scheme_##what##_directive_get_##field);


  //block to repeat for new  directive fields 

  INSTALL_GET(standalone, minpixels);
  INSTALL_GET(chord, minpixels);
  INSTALL_GET(note, minpixels);
  INSTALL_GET(staff, minpixels);
  INSTALL_GET(voice, minpixels);
  INSTALL_GET(score, minpixels);
  INSTALL_GET(clef, minpixels);
  INSTALL_GET(timesig, minpixels);
  INSTALL_GET(keysig, minpixels);

  INSTALL_GET(scoreheader, minpixels);
  INSTALL_GET(header, minpixels);
  INSTALL_GET(paper, minpixels);
  INSTALL_GET(layout, minpixels);
  INSTALL_GET(movementcontrol, minpixels);

  INSTALL_PUT(standalone, minpixels);
  INSTALL_PUT(chord, minpixels);
  INSTALL_PUT(note, minpixels);
  INSTALL_PUT(staff, minpixels);
  INSTALL_PUT(voice, minpixels);
  INSTALL_PUT(score, minpixels);
  INSTALL_PUT(clef, minpixels);
  INSTALL_PUT(timesig, minpixels);
  INSTALL_PUT(keysig, minpixels);


  INSTALL_PUT(scoreheader, minpixels);
  INSTALL_PUT(header, minpixels);
  INSTALL_PUT(paper, minpixels);
  INSTALL_PUT(layout, minpixels);
  INSTALL_PUT(movementcontrol, minpixels);

  //end block to repeat for new  directive fields 


  INSTALL_GET(standalone, midibytes);
  INSTALL_GET(chord, midibytes);
  INSTALL_GET(note, midibytes);
  INSTALL_GET(staff, midibytes);
  INSTALL_GET(score, midibytes);
  INSTALL_GET(movementcontrol, midibytes);
  INSTALL_PUT(standalone, midibytes);
  INSTALL_PUT(chord, midibytes);
  INSTALL_PUT(note, midibytes);
  INSTALL_PUT(staff, midibytes);
  INSTALL_PUT(score, midibytes);
  INSTALL_PUT(movementcontrol, midibytes);





  INSTALL_GET(standalone, override);
  INSTALL_GET(chord, override);
  INSTALL_GET(note, override);
  INSTALL_GET(staff, override);
  INSTALL_GET(voice, override);
  INSTALL_GET(score, override);

  INSTALL_PUT(standalone, override);
  INSTALL_PUT(chord, override);
  INSTALL_PUT(note, override);
  INSTALL_PUT(staff, override);
  INSTALL_PUT(voice, override);
  INSTALL_PUT(score, override);


  //graphic 
  INSTALL_PUT(note, graphic);
  //INSTALL_GET(note, graphic);

  INSTALL_PUT(chord, graphic);
  //INSTALL_GET(chord, graphic);

  INSTALL_PUT(standalone, graphic);
  //INSTALL_GET(standalone, graphic);


  INSTALL_PUT(staff, graphic);
  INSTALL_PUT(score, graphic);
  //graphic



  INSTALL_PUT(chord, display);
  INSTALL_PUT(chord, prefix);
  INSTALL_PUT(chord, postfix);

  INSTALL_GET(chord, display);
  INSTALL_GET(chord, prefix);
  INSTALL_GET(chord, postfix);


  INSTALL_PUT(note, display);
  INSTALL_PUT(note, prefix);
  INSTALL_PUT(note, postfix);

  INSTALL_GET(note, display);
  INSTALL_GET(note, prefix);
  INSTALL_GET(note, postfix);

  INSTALL_PUT(standalone, display);
  INSTALL_PUT(standalone, prefix);
  INSTALL_PUT(standalone, postfix);

  INSTALL_GET(standalone, display);
  INSTALL_GET(standalone, prefix);
  INSTALL_GET(standalone, postfix);


  INSTALL_PUT(staff, display);
  INSTALL_PUT(staff, prefix);
  INSTALL_PUT(staff, postfix);

  INSTALL_GET(staff, display);
  INSTALL_GET(staff, prefix);
  INSTALL_GET(staff, postfix);

  INSTALL_PUT(voice, display);
  INSTALL_PUT(voice, prefix);
  INSTALL_PUT(voice, postfix);

  INSTALL_GET(voice, display);
  INSTALL_GET(voice, prefix);
  INSTALL_GET(voice, postfix);

  INSTALL_PUT(score, display);
  INSTALL_PUT(score, prefix);
  INSTALL_PUT(score, postfix);

  INSTALL_GET(score, display);
  INSTALL_GET(score, prefix);
  INSTALL_GET(score, postfix);


INSTALL_GET(score, x);
INSTALL_GET(score, gx);
INSTALL_GET(score, tx);
INSTALL_PUT(score, x);
INSTALL_PUT(score, gx);
INSTALL_PUT(score, tx);

INSTALL_GET(score, y);
INSTALL_GET(score, gy);
INSTALL_GET(score, ty);
INSTALL_PUT(score, y);
INSTALL_PUT(score, gy);
INSTALL_PUT(score, ty);




  INSTALL_PUT(note, x);
  INSTALL_GET(note, x);
  INSTALL_PUT(chord, x);
  INSTALL_GET(chord, x);
  INSTALL_PUT(note, y);
  INSTALL_GET(note, y);
  INSTALL_PUT(chord, y);
  INSTALL_GET(chord, y);

  INSTALL_PUT(note, tx);
  INSTALL_GET(note, tx);
  INSTALL_PUT(chord, tx);
  INSTALL_GET(chord, tx);
  INSTALL_PUT(note, ty);
  INSTALL_GET(note, ty);
  INSTALL_PUT(chord, ty);
  INSTALL_GET(chord, ty);



  INSTALL_PUT(note, gx);
  INSTALL_GET(note, gx);
  INSTALL_PUT(chord, gx);
  INSTALL_GET(chord, gx);
  INSTALL_PUT(note, gy);
  INSTALL_GET(note, gy);
  INSTALL_PUT(chord, gy);
  INSTALL_GET(chord, gy);


  INSTALL_PUT(standalone, x);
  INSTALL_GET(standalone, x);
  INSTALL_PUT(standalone, y);
  INSTALL_GET(standalone, y);

  INSTALL_PUT(standalone, tx);
  INSTALL_GET(standalone, tx);
  INSTALL_PUT(standalone, ty);
  INSTALL_GET(standalone, ty);

  INSTALL_PUT(standalone, gx);
  INSTALL_GET(standalone, gx);
  INSTALL_PUT(standalone, gy);
  INSTALL_GET(standalone, gy);




  INSTALL_GET(note, width);
  INSTALL_GET(chord, width);
  INSTALL_GET(standalone, width);
  INSTALL_GET(note, height);
  INSTALL_GET(chord, height);
  INSTALL_GET(standalone, height);



     //block to copy for new type of directive
INSTALL_PUT(clef, display);
INSTALL_PUT(clef, prefix);
INSTALL_PUT(clef, postfix);
INSTALL_PUT(clef, graphic);


INSTALL_GET(clef, display);
INSTALL_GET(clef, prefix);
INSTALL_GET(clef, postfix);

INSTALL_PUT(clef, x)
INSTALL_PUT(clef, y)
INSTALL_PUT(clef, tx)
INSTALL_PUT(clef, ty)
INSTALL_PUT(clef, gx)
INSTALL_PUT(clef, gy)
INSTALL_PUT(clef, override)


INSTALL_GET(clef, x)
INSTALL_GET(clef, y)
INSTALL_GET(clef, tx)
INSTALL_GET(clef, ty)
INSTALL_GET(clef, gx)
INSTALL_GET(clef, gy)
INSTALL_GET(clef, override)
INSTALL_GET(clef, width)
INSTALL_GET(clef, height)

INSTALL_EDIT(clef);
     // end of block to copy for new type of directive

INSTALL_PUT(timesig, display);
INSTALL_PUT(timesig, prefix);
INSTALL_PUT(timesig, postfix);
INSTALL_PUT(timesig, graphic);


INSTALL_GET(timesig, display);
INSTALL_GET(timesig, prefix);
INSTALL_GET(timesig, postfix);

INSTALL_PUT(timesig, x)
INSTALL_PUT(timesig, y)
INSTALL_PUT(timesig, tx)
INSTALL_PUT(timesig, ty)
INSTALL_PUT(timesig, gx)
INSTALL_PUT(timesig, gy)
INSTALL_PUT(timesig, override)


INSTALL_GET(timesig, x)
INSTALL_GET(timesig, y)
INSTALL_GET(timesig, tx)
INSTALL_GET(timesig, ty)
INSTALL_GET(timesig, gx)
INSTALL_GET(timesig, gy)
INSTALL_GET(timesig, override)
INSTALL_GET(timesig, width)
INSTALL_GET(timesig, height)

INSTALL_EDIT(timesig);

INSTALL_PUT(keysig, display);
INSTALL_PUT(keysig, prefix);
INSTALL_PUT(keysig, postfix);
INSTALL_PUT(keysig, graphic);


INSTALL_GET(keysig, display);
INSTALL_GET(keysig, prefix);
INSTALL_GET(keysig, postfix);

INSTALL_PUT(keysig, x)
INSTALL_PUT(keysig, y)
INSTALL_PUT(keysig, tx)
INSTALL_PUT(keysig, ty)
INSTALL_PUT(keysig, gx)
INSTALL_PUT(keysig, gy)
INSTALL_PUT(keysig, override)


INSTALL_GET(keysig, x)
INSTALL_GET(keysig, y)
INSTALL_GET(keysig, tx)
INSTALL_GET(keysig, ty)
INSTALL_GET(keysig, gx)
INSTALL_GET(keysig, gy)
INSTALL_GET(keysig, override)
INSTALL_GET(keysig, width)
INSTALL_GET(keysig, height)

INSTALL_EDIT(keysig);


INSTALL_PUT(scoreheader, display);
INSTALL_PUT(scoreheader, prefix);
INSTALL_PUT(scoreheader, postfix);
INSTALL_PUT(scoreheader, graphic);


INSTALL_GET(scoreheader, display);
INSTALL_GET(scoreheader, prefix);
INSTALL_GET(scoreheader, postfix);

INSTALL_PUT(scoreheader, x)
INSTALL_PUT(scoreheader, y)
INSTALL_PUT(scoreheader, tx)
INSTALL_PUT(scoreheader, ty)
INSTALL_PUT(scoreheader, gx)
INSTALL_PUT(scoreheader, gy)
INSTALL_PUT(scoreheader, override)


INSTALL_GET(scoreheader, x)
INSTALL_GET(scoreheader, y)
INSTALL_GET(scoreheader, tx)
INSTALL_GET(scoreheader, ty)
INSTALL_GET(scoreheader, gx)
INSTALL_GET(scoreheader, gy)
INSTALL_GET(scoreheader, override)
INSTALL_GET(scoreheader, width)
INSTALL_GET(scoreheader, height)

INSTALL_EDIT(scoreheader);


INSTALL_PUT(header, display);
INSTALL_PUT(header, prefix);
INSTALL_PUT(header, postfix);
INSTALL_PUT(header, graphic);


INSTALL_GET(header, display);
INSTALL_GET(header, prefix);
INSTALL_GET(header, postfix);

INSTALL_PUT(header, x)
INSTALL_PUT(header, y)
INSTALL_PUT(header, tx)
INSTALL_PUT(header, ty)
INSTALL_PUT(header, gx)
INSTALL_PUT(header, gy)
INSTALL_PUT(header, override)


INSTALL_GET(header, x)
INSTALL_GET(header, y)
INSTALL_GET(header, tx)
INSTALL_GET(header, ty)
INSTALL_GET(header, gx)
INSTALL_GET(header, gy)
INSTALL_GET(header, override)
INSTALL_GET(header, width)
INSTALL_GET(header, height)

INSTALL_EDIT(header);


INSTALL_PUT(paper, display);
INSTALL_PUT(paper, prefix);
INSTALL_PUT(paper, postfix);
INSTALL_PUT(paper, graphic);


INSTALL_GET(paper, display);
INSTALL_GET(paper, prefix);
INSTALL_GET(paper, postfix);

INSTALL_PUT(paper, x)
INSTALL_PUT(paper, y)
INSTALL_PUT(paper, tx)
INSTALL_PUT(paper, ty)
INSTALL_PUT(paper, gx)
INSTALL_PUT(paper, gy)
INSTALL_PUT(paper, override)


INSTALL_GET(paper, x)
INSTALL_GET(paper, y)
INSTALL_GET(paper, tx)
INSTALL_GET(paper, ty)
INSTALL_GET(paper, gx)
INSTALL_GET(paper, gy)
INSTALL_GET(paper, override)
INSTALL_GET(paper, width)
INSTALL_GET(paper, height)

INSTALL_EDIT(paper);


INSTALL_PUT(layout, display);
INSTALL_PUT(layout, prefix);
INSTALL_PUT(layout, postfix);
INSTALL_PUT(layout, graphic);


INSTALL_GET(layout, display);
INSTALL_GET(layout, prefix);
INSTALL_GET(layout, postfix);

INSTALL_PUT(layout, x)
INSTALL_PUT(layout, y)
INSTALL_PUT(layout, tx)
INSTALL_PUT(layout, ty)
INSTALL_PUT(layout, gx)
INSTALL_PUT(layout, gy)
INSTALL_PUT(layout, override)


INSTALL_GET(layout, x)
INSTALL_GET(layout, y)
INSTALL_GET(layout, tx)
INSTALL_GET(layout, ty)
INSTALL_GET(layout, gx)
INSTALL_GET(layout, gy)
INSTALL_GET(layout, override)
INSTALL_GET(layout, width)
INSTALL_GET(layout, height)

INSTALL_EDIT(layout);

INSTALL_PUT(movementcontrol, display);
INSTALL_PUT(movementcontrol, prefix);
INSTALL_PUT(movementcontrol, postfix);
INSTALL_PUT(movementcontrol, graphic);


INSTALL_GET(movementcontrol, display);
INSTALL_GET(movementcontrol, prefix);
INSTALL_GET(movementcontrol, postfix);

INSTALL_PUT(movementcontrol, x)
INSTALL_PUT(movementcontrol, y)
INSTALL_PUT(movementcontrol, tx)
INSTALL_PUT(movementcontrol, ty)
INSTALL_PUT(movementcontrol, gx)
INSTALL_PUT(movementcontrol, gy)
INSTALL_PUT(movementcontrol, override)


INSTALL_GET(movementcontrol, x)
INSTALL_GET(movementcontrol, y)
INSTALL_GET(movementcontrol, tx)
INSTALL_GET(movementcontrol, ty)
INSTALL_GET(movementcontrol, gx)
INSTALL_GET(movementcontrol, gy)
INSTALL_GET(movementcontrol, override)
INSTALL_GET(movementcontrol, width)
INSTALL_GET(movementcontrol, height)

INSTALL_EDIT(movementcontrol);


#undef INSTALL_EDIT
#undef EDIT_DELETE_FN_DEF
#undef INSTALL_PUT
#undef INSTALL_GET
#undef GETFUNC_DEF
#undef PUTFUNC_DEF

#undef INT_PUTFUNC_DEF
#undef INT_GETFUNC_DEF
#undef PUTGRAPHICFUNC_DEF




  /* test with (display (d-DirectivePut-note-display "LHfinger" "test")) after attaching a LH finger directive */
  /* test with (display (d-DirectivePut-note-minpixels "LHfinger" 80)) after attaching a LH finger directive */
  /* test with (display (d-DirectiveGet-note-minpixels "LHfinger")) after attaching a LH finger directive */

  /* test with (display (d-DirectiveGet-note-display "LHfinger")) after attaching a LH finger directive */

  install_scm_function (DENEMO_SCHEME_PREFIX"GetMidi", scheme_get_midi);
  install_scm_function_with_param (DENEMO_SCHEME_PREFIX"PutMidi", scheme_put_midi);
  install_scm_function_with_param (DENEMO_SCHEME_PREFIX"PlayMidiKey", scheme_play_midikey);
  install_scm_function2 (DENEMO_SCHEME_PREFIX"BassFigure", scheme_bass_figure);
  install_scm_function (DENEMO_SCHEME_PREFIX"GetNoteAsMidi", scheme_get_note_as_midi);
  install_scm_function (DENEMO_SCHEME_PREFIX"RefreshDisplay", scheme_refresh_display);
  install_scm_function (DENEMO_SCHEME_PREFIX"InputFilterNames", scheme_input_filter_names);

  /* test with

  (display (d-GetNoteAsMidi))

  (define command (lambda ()
            (d-WarningDialog "To use this function correctly you need to give a duration.")
	    ))
     (define duration (d-GetCommand))
     (cond ((equal? duration "d-3") (set! command d-Change3))
           ((equal? duration "d-4") (set! command d-Change4))
      )
     (d-InsertTiedNote)
     (command)

    
  */

  denemo_scheme_init();
  process_command_line(argc, argv);
  /* Now launch into the main gtk event loop and we're all set */
  gtk_main();
}






GString *get_widget_path(GtkWidget *widget) {
  const gchar * name;
  GString *str = g_string_new("/");
  for(widget = gtk_widget_get_parent(widget);widget;widget = gtk_widget_get_parent(widget)){
    name = gtk_widget_get_name(widget);  
    g_string_prepend(str, name);
    g_string_prepend_c(str,'/');
  }
  g_print("String is %s\n", str->str);
  return str;
}




/**
 * close the application and shut down the sequencer if open
 *
 */
static void
quit (void)
{
#ifdef HAVEALSA
  midi_seq_delete (sq);
#endif
  //gtk_main_quit ();
  exit(0);//do not use gtk_main_quit, as there may be inner loops active.
}


/**
 * Close the movement gui, releasing its memory and removing it from the list
 * Do not close the sequencer
 */
static void
close_gui (DenemoGUI *gui)
{
  stop_midi_playback (NULL, NULL);// if you do not do this, there is a timer moving the score on which will hang
  activate_action("/MainMenu/InputMenu/KeyboardOnly");
  if(Denemo.autosaveid) {
    if(g_list_length(Denemo.guis)>1)
      g_print("Auto save being turned off");
    g_source_remove(Denemo.autosaveid);
    Denemo.autosaveid = 0;
  }

  storeWindowState ();
 //stop_pitch_recognition();
  free_gui(gui);
  
  gtk_widget_destroy (gui->page);
  Denemo.guis = g_list_remove (Denemo.guis, gui);//FIXME ?? or in the destroy callback??
  g_free (gui);
}

/* remove all the music data from a gui */
void free_gui(DenemoGUI *gui)
{
  GList *g;
  for(g=gui->movements;g;g=g->next) {
    gui->si = g->data;
    free_score(gui);
  }
  delete_directives(&gui->lilycontrol.directives);
  delete_directives(&gui->scoreheader.directives);
  delete_directives(&gui->paper.directives);
  g_list_free(gui->movements);
  gui->movements = NULL;
  if(gui->custom_scoreblocks) {
    GList *custom;
    for(custom=gui->custom_scoreblocks;custom;custom=custom->next) {
      g_string_free((GString*)(((DenemoScoreblock*)custom->data)->scoreblock), TRUE);
    }
    g_list_free(gui->custom_scoreblocks);
    gui->custom_scoreblocks=NULL;
  }
  //nullify_gstring(&gui->custom_prolog);

      /* any other free/initializations */
}

/**
* Wrapper function to close application when the quit
* menu item has been used
* 
*
*/
static void
closewrapper (GtkAction *action, gpointer param)
{
  GList *display = NULL;
  //stop_pitch_recognition();
  if(Denemo.accelerator_status) {
    if(confirm("You have made changes to the commands you have","Do you want to save the changes?"))
      save_accels();
  } 
  for (display = Denemo.guis; display != NULL;
       display = g_list_next (display))
    {
     
     Denemo.gui = (DenemoGUI *) display->data;
     if(close_gui_with_check (NULL, NULL) == FALSE)
       break;
  }
}

/**
 * callback from deleting window belonging to gui:
 * close window if check for unsaved data succeeds.
 * 
 */

static gboolean
delete_callback (GtkWidget * widget, GdkEvent * event)
{

  close_gui_with_check (NULL, NULL);
  return TRUE;
}
/**
 * callback to fetch up-to-date system commands from internet, denemo.org hardwired at present
 */
static void
fetchcommands (GtkAction *action, gpointer param)
{
  static gchar *location=NULL;
  location = g_build_filename(locatedotdenemo(), "download", "actions", " ", NULL);
  gboolean err = g_mkdir_with_parents(location, 0770);
  if(err) {
    warningdialog(g_strdup_printf("Could not make folder %s for the downloaded commands", location));
    return;
  }

  g_print("location is %s\n", location);
  GError *error = NULL;
  gchar *arguments[] = {
  "wget",
  "-r",
  "-np",//only below the menus directory
  "-nH",//cut prefix
  "--cut-dirs=1",//cut menus part of path
  DENEMO_DEFAULT_ANON_FTP,
  NULL
  };

  g_spawn_async (location,		/* dir */
		 arguments, NULL,	/* env */
		 G_SPAWN_SEARCH_PATH, /* search in path for executable */
		 NULL,	/* child setup func */
		 NULL,		/* user data */		
		 NULL,
		 &error);
  //FIXME create a callback to tell the user the result...
}


/**
 * callback to load system extra commands
 * if user has a local (possibly updated) set in ~/.denemo/downloads then that directory is used.
 */
static void
morecommands (GtkAction *action, gpointer param)
{
  static gchar *location=NULL;
  location = g_build_filename(locatedotdenemo(), "download", "actions", "menus", " ", NULL);
  if(!g_file_test(location, G_FILE_TEST_EXISTS)){
    g_free(location);
    location = NULL;
  }
  if(location==NULL)
    location = g_build_filename(get_data_dir(), "actions", "menus", " ", NULL);
  load_keymap_dialog_location (NULL, location);
  if(Denemo.last_merged_command && g_str_has_prefix(Denemo.last_merged_command, get_data_dir())) {
    g_free(location);
    location = g_strdup(Denemo.last_merged_command);
  }
}

/**
 * callback to load local extra commands
 * 
 */
static void
mycommands (GtkAction *action, gpointer param)
{
  static gchar *location=NULL;
  if(location==NULL)
    location = g_build_filename(locatedotdenemo(), "actions", "menus", " ", NULL);

  if(Denemo.last_merged_command && g_str_has_prefix(Denemo.last_merged_command, locatedotdenemo())) {
    g_free(location);
    location = g_strdup(Denemo.last_merged_command);
  }
  load_keymap_dialog_location (NULL, location);

  //g_print("The last was %s %s %s\n", Denemo.last_merged_command, location,  locatedotdenemo());
}



/**
 * Open in New Window callback 
 * Creates new view then opens file in the view
 */
void
openinnew (GtkAction *action, gpointer param)
{
  newtab (NULL, param);
  file_open_with_check (NULL, param);
}


/**
 * Close callback 
 * if user confirms close the current gui
 * if it is the last close the application.
 * return FALSE if gui was not closed, else TRUE
 */
gboolean
close_gui_with_check (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  if ((!gui->notsaved) || (gui->notsaved && confirmbox (gui)))
    close_gui (gui);
  else 
    return FALSE;
  if(Denemo.guis==NULL) {
   
    writeHistory ();
    writeXMLPrefs(&Denemo.prefs);
    ext_quit (); /* clean players pidfiles (see external.c) */
    quit (); 
  } else {
    Denemo.gui = Denemo.guis->data;
    g_print("Setting the first piece as your score\n");
    gtk_notebook_set_current_page (GTK_NOTEBOOK(Denemo.notebook), 0);
  }
    
  return TRUE;
}


static void
singleton_callback (GtkToolButton *toolbutton, RhythmPattern *r) {
  DenemoGUI *gui = Denemo.gui;
#define CURRP ((RhythmPattern *)gui->currhythm->data)
  if(gui->currhythm && CURRP)
    unhighlight_rhythm(CURRP);
  gui->currhythm = NULL;

  gui->rstep = r->rsteps;
#define g (gui->rstep)
#define MODE (gui->mode)
  unhighlight_rhythm(gui->prevailing_rhythm);
  gui->prevailing_rhythm = r;
  highlight_rhythm(r);
  /*   if((MODE&INPUTEDIT)) */
  ((GtkFunction)(((RhythmElement*)g->data)->functions->data))(gui), displayhelper(gui); 
#undef CURRP
#undef g
#undef MODE
}

/**
 * Rhythm callback select rhythm
 * inserts the rhythm if pitchless
 */
void
select_rhythm_pattern(GtkToolButton *toolbutton, RhythmPattern *r) {
  DenemoGUI *gui = Denemo.gui;
#define CURRP ((RhythmPattern *)gui->currhythm->data)
  if(gui->currhythm && CURRP)
    unhighlight_rhythm(CURRP);
  else
    if(gui->rstep)
      unhighlight_rhythm(((RhythmElement*)gui->rstep->data)->rhythm_pattern);

  gui->currhythm = g_list_find(gui->rhythms, r);
  gui->rstep = r->rsteps;
#define g (gui->rstep)
#define MODE (gui->mode)
  if(((RhythmElement*)g->data)->icon) {
    GtkWidget *label = LABEL(CURRP->button);
    //g_print("markup is %s\n", ((RhythmElement*)g->data)->icon);
    gtk_label_set_markup(GTK_LABEL(label),((RhythmElement*)g->data)->icon);
/* #define a CURRP->button */
/*     g_print("Visible is %d %d \n", gtk_event_box_get_visible_window(gtk_tool_button_get_label_widget((a))), */
/* 	    gtk_event_box_get_above_child(gtk_tool_button_get_label_widget((a))));  */
/* #undef a */
  }
  highlight_rhythm(CURRP);
  if((MODE&INPUTEDIT))
    insert_rhythm_pattern(gui);
#undef CURRP
#undef g
#undef MODE
}

/* duration_code(gpointer function)
 * return an ascii code to indicate what duration (if any) function gives.
 * '0x0' means not a duration
 * chars 0123456 are the standard note durations
 * 
 */
gchar duration_code(gpointer fn) {
  return fn==(gpointer)insert_chord_0key ? '0':
    fn==(gpointer)insert_chord_1key ? '1':
    fn==(gpointer)insert_chord_2key ? '2':
    fn==(gpointer)insert_chord_3key ? '3':
    fn==(gpointer)insert_chord_4key ? '4':
    fn==(gpointer)insert_chord_5key ? '5':
    fn==(gpointer)insert_chord_6key ? '6':0;
}
/* modifier_code(gpointer function)
 * return an ascii code to indicate what modifier (if any) function gives.
 * '0x0' means not a valid modifier for a rhythmic duration
 * char '.' means a dotted note, '(' and ')' mean start and end slur
 * r to z are rests
 * others to be defined
 * 
 */
gchar modifier_code(gpointer fn) {
  return fn==(gpointer)start_triplet ? '~':
    fn==(gpointer)end_tuplet ? '|':
    fn==(gpointer)add_dot_key ? '.':
    fn==(gpointer)toggle_begin_slur ? '(':
    fn==(gpointer)toggle_end_slur ? ')': 
    fn==(gpointer)insert_rest_0key ? 'r':
    fn==(gpointer)insert_rest_1key ? 's':
    fn==(gpointer)insert_rest_2key ? 't':
    fn==(gpointer)insert_rest_3key ? 'u':
    fn==(gpointer)insert_rest_4key ? 'v':
    fn==(gpointer)insert_rest_5key ? 'w':
    fn==(gpointer)insert_rest_6key ? 'x':0;
}

gboolean code_is_a_duration(gchar code) {
  return code==0 || (code>='r' && code<='z');
}



/* add_to_rhythm appends to a rhythm pattern the callback function fn
   fn is a callback function
   returns TRUE if something was added
 */
static gboolean append_rhythm(RhythmPattern *r,  gpointer fn){     
	 RhythmElement *relement;
	 
	 int keyval = duration_code(fn);
	 if(keyval) {

	     relement = (RhythmElement*)g_malloc0(sizeof(RhythmElement));
	   
	   relement->functions = g_list_append(NULL, fn);

	     r->rsteps = g_list_append(r->rsteps, relement);
	     relement->rhythm_pattern = r;
	   return TRUE;
	 }
	 keyval = modifier_code(fn);
	 if(keyval) {
	   if(r->rsteps) {
	     relement = (RhythmElement *)(g_list_last(r->rsteps)->data);
	   }
	   else {
	     relement = (RhythmElement*)g_malloc0(sizeof(RhythmElement));
	   }
	   relement->functions = g_list_append(relement->functions, (gpointer)fn);
	   if(r->rsteps==NULL) {
	     r->rsteps = g_list_append(r->rsteps, relement);
	   }
	   relement->rhythm_pattern = r;
	   return TRUE;
	 }
	 return FALSE;
}


static gchar *add_to_pattern(gchar **p, gchar c) {
  gchar *temp = g_strdup_printf("%s%c", *p, c);
  g_free(*p);
  *p = temp;
}



/* create_rhythm_cb
   This is overloaded for use as a callback (ACTION is a GtkAction) and
   as a call to set up the "singleton rhythms", 
   (rhythm patterns that are just one note or rest, used for
   ordinary note entry).
   if ACTION is a GtkAction*
        create a rhythm pattern from the current selection
        the rhythm is put in gui->
        a button is created in "/RhythmToolbar"
        and the pattern is added to gui->rhythms 
         with the first step of it put in gui->rstep
   if ACTION is one of the insert_chord_xkey insert_rest_xkey)
   functions
        a button is created in the /EntryToolbar (if not alread present)
   

*/
static void 
create_rhythm_cb (GtkAction* action, gpointer param)     {
  DenemoGUI *gui = Denemo.gui;
  gboolean singleton = FALSE;// set TRUE if action is one of the insert_... functions.
  gboolean already_done = FALSE;// a singleton which has already been installed globally
  gboolean default_rhythm = FALSE;
  DenemoScore * si= gui->si;
  RhythmPattern *r = (RhythmPattern*)g_malloc0(sizeof(RhythmPattern));
  gchar *pattern = NULL;
    if(action ==  (gpointer)insert_chord_0key)
      pattern = g_strdup("0");
    if(action ==  (gpointer)insert_chord_1key)
      pattern = g_strdup("1");
    if(action ==  (gpointer)insert_chord_2key)
      pattern = g_strdup("2"), default_rhythm = TRUE;
    if(action ==  (gpointer)insert_chord_3key)
      pattern = g_strdup("3");
    if(action ==  (gpointer)insert_chord_4key)
      pattern = g_strdup("4");
    if(action ==  (gpointer)insert_chord_5key)
      pattern = g_strdup("5");
    if(action ==  (gpointer)insert_chord_6key)
      pattern = g_strdup("6");

    if(action ==  (gpointer)insert_rest_0key)
      pattern = g_strdup("r");
    if(action ==  (gpointer)insert_rest_1key)
      pattern = g_strdup("s");
    if(action ==  (gpointer)insert_rest_2key)
      pattern = g_strdup("t");
    if(action ==  (gpointer)insert_rest_3key)
      pattern = g_strdup("u");
    if(action ==  (gpointer)insert_rest_4key)
      pattern = g_strdup("v");
    if(action ==  (gpointer)insert_rest_5key)
      pattern = g_strdup("w");
    if(action ==  (gpointer)insert_rest_6key)
      pattern = g_strdup("x");
    if(pattern) {/* if we already have it globally we don't need it again
		    note we never delete the singleton rhythms */
      if(Denemo.singleton_rhythms[*pattern]) {
	g_free(r);
	r = Denemo.singleton_rhythms[*pattern];
	already_done = TRUE;
      }
      else {
	Denemo.singleton_rhythms[*pattern] = r;
	already_done = FALSE;
      }
      singleton=TRUE;
    }
  else
    pattern = g_strdup_printf("");
    GtkToolButton *button;
    GtkWidget *label;
    if(!already_done){
      button = (GtkToolButton *)gtk_tool_button_new(NULL, NULL);
      label = gtk_label_new(NULL);
      gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
      GtkWidget *ev = gtk_event_box_new();
      gtk_container_add (GTK_CONTAINER(ev), label);
      gtk_tool_button_set_label_widget (button, ev);
      //gtk_event_box_set_above_child (ev, TRUE);
      r->button = button;
    }
  if(!singleton) {
    staffnode *curstaff;
    measurenode *curmeasure;
    gint i = si->firststaffmarked;
    curstaff = g_list_nth (si->thescore, i - 1);
    if(curstaff && i <= si->laststaffmarked) {
      int j,k;
      objnode *curobj;
      /* Measure loop.  */
      for (j = si->firstmeasuremarked, k = si->firstobjmarked,
	     curmeasure = g_list_nth (firstmeasurenode (curstaff), j - 1);
	   curmeasure && j <= si->lastmeasuremarked;
	   curmeasure = curmeasure->next, j++)
	{
	  for (curobj = g_list_nth ((objnode *) curmeasure->data, k);
	       /* cursor_x is 0-indexed */
	       curobj && (j < si->lastmeasuremarked
			  || k <= si->lastobjmarked);
	       curobj = curobj->next, k++)
	    {
	      gpointer fn;
	      gchar *temp;
	      DenemoObject *obj = (DenemoObject *) curobj->data;
	      switch(obj->type) {
	      case TUPCLOSE:
		fn = (gpointer)end_tuplet;
		add_to_pattern(&pattern, '|');
		append_rhythm(r, fn);
		break;
	      case TUPOPEN:
		switch(((tupopen*)obj->object)->denominator) {
		case 3:
		  fn=(gpointer)start_triplet;
		  add_to_pattern(&pattern, '~');
		  break;
		default:// need to create start_xxxtuplet() functions to go with start_triplet(), then they can go here.
		  fn = NULL;
		}
		append_rhythm(r, fn);
		break;
	      case CHORD:
		{
		  chord *ch = (chord*)obj->object;
		  
		  if(ch->notes) {
		    switch(ch->baseduration) {
		    case 0:
		      fn = insert_chord_0key;
		      break;
		    case 1:
		      fn = insert_chord_1key;
		      break;
		    case 2:
		      fn = insert_chord_2key;
		      break;
		    case 3:
		      fn = insert_chord_3key;
		      break;
		    case 4:
		      fn = insert_chord_4key;
		      break;
		    case 5:
		      fn = insert_chord_5key;
		      break;
		    case 6:
		      fn = insert_chord_6key;
		      break;
		    }
		    add_to_pattern(&pattern, duration_code(fn));
		    append_rhythm(r, fn);

		  } else {/* a rest */
		    switch(ch->baseduration) {
		    case 0:
		      fn = insert_rest_0key;
		      break;
		      case 1:
		      fn = insert_rest_1key;
		      break;
		      case 2:
		      fn = insert_rest_2key;
		      break;
		      case 3:
		      fn = insert_rest_3key;
		      break;
		      case 4:
		      fn = insert_rest_4key;
		      break;
		      case 5:
		      fn = insert_rest_5key;
		      break;
		      case 6:
		      fn = insert_rest_6key;
		      break;
		    }
		    add_to_pattern(&pattern, modifier_code(fn));
		    append_rhythm(r, fn);
		  } /* end of rests */
		  for (i=ch->numdots;i;i--) {
		    fn = add_dot_key;		    
		    add_to_pattern(&pattern, modifier_code(fn));
		    append_rhythm(r, fn);
		  }
		  if(ch->slur_begin_p) {
		    fn = (gpointer)toggle_begin_slur;
		    add_to_pattern(&pattern,'('); 
		    append_rhythm(r, fn);
		  }
		  if(ch->slur_end_p) {
		    fn = (gpointer)toggle_end_slur;
		    add_to_pattern(&pattern,')'); 
		    append_rhythm(r, fn);
		  }
		}
		break;
	      default:
		;
		
	      }
	      //g_print("Number of rhythms %d\n", g_list_length(r->rsteps));
	    } /* End object loop */	 
	} /* End measure loop */
    }//looking at selection
  if(strlen(pattern)==0) { // nothing useful selected
      warningdialog("No selection to create a rhythm pattern from\nSee Edit->Select menu for selecting notes/rests");
      gtk_widget_destroy(GTK_WIDGET(r->button));
      g_free(pattern);
      g_free(r);
      return;
    }
  } else { // singleton
    if(!already_done) 
      append_rhythm(r, action);
  }
  if(!already_done) {
    gchar *labelstr;
    if(pattern) {
      labelstr = music_font(pattern);
    }
    else
      return;  //FIXME memory leak of r - well pattern is never NULL
    //g_print("rsteps is %p entry is %s, %s\n", r->rsteps, pattern, labelstr);
    label = LABEL(r->button);
    gtk_label_set_markup(GTK_LABEL(label), labelstr);
    g_free(labelstr);
  }
  
  if(!singleton) {
    /* fill the r->rsteps with icons for each step, singletons have NULL icon */
    GList *g;  
    RhythmElement *el;
    gint i;
    for(g=r->rsteps, i=0;g;g=g->next, i++) {
      el = (RhythmElement*)g->data;
      if(i==0 && (*(pattern)<'0' || *(pattern)>'6') && g->next)
	g  = g->next;// pattern does not start with a note, so we skip to the second element, unless there are no notes
      while(*(pattern+i) && (*(pattern+i)<'0' || *(pattern+i)>'6'))
	i++;
      if(*(pattern+i)) {
	*(pattern+i) += 20;
	el->icon = music_font(pattern);
	*(pattern+i) -= 20;
      }
      //g_print("el->icon = %s step %d pattern %s\n", el->icon, i, pattern);
    }
  }
  if(!already_done)
    if(r->rsteps) {
      /* make the list circular */
      r->rsteps->prev = g_list_last(r->rsteps);
      g_list_last(r->rsteps)->next = r->rsteps;
    }
  if(r->rsteps==NULL)
    {
      gtk_widget_destroy(GTK_WIDGET(button));
      g_free(r);
      r = NULL;
    } else 	{
      if(singleton) {
	if(!already_done) {//When creating first gui only
	  GtkWidget *toolbar = gtk_ui_manager_get_widget (Denemo.ui_manager, "/EntryToolBar");
	  gtk_toolbar_insert(GTK_TOOLBAR(toolbar), GTK_TOOL_ITEM(button), -1);
	  gtk_widget_show_all(GTK_WIDGET(button));
	  /* gui->rstep = r->rsteps; */
	  g_signal_connect (G_OBJECT (button), "clicked",
			  G_CALLBACK (singleton_callback), (gpointer)r);
	  unhighlight_rhythm(r);
	}
	if(default_rhythm){
	  gui->prevailing_rhythm = r;
	  gui->rstep = r->rsteps;
	  highlight_rhythm(r);
	  //g_print("prevailing rhythm is %p\n",r);
	}	
      } else {//not singleton
	GtkWidget *toolbar = gtk_ui_manager_get_widget (Denemo.ui_manager, "/RhythmToolBar");
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), GTK_TOOL_ITEM(button), -1);
	gtk_widget_show_all(GTK_WIDGET(button));
	gui->rstep = r->rsteps;
	gui->rhythms = g_list_append(gui->rhythms , r);
	
	if(gui->currhythm)
	  unhighlight_rhythm((RhythmPattern *)gui->currhythm->data);
	gui->currhythm = g_list_last(gui->rhythms);
	highlight_rhythm((RhythmPattern *)gui->currhythm->data);
	g_signal_connect (G_OBJECT (button), "clicked",
			  G_CALLBACK (select_rhythm_pattern), (gpointer)r);
      }      
    }
}

typedef enum shortcut_mod {
  SHORTCUT_NOCHANGE,
  SHORTCUT_ADD,
  SHORTCUT_DELETE
} shortcut_mod;

typedef struct set_accels_cb_data {
  GtkAccelKey *key;
  GtkButton *prop_button;
  shortcut_mod changed;
  gint keyval;
  gint modifiers;
  gint idx;
  gchar *path;
} set_accels_cb_data;

/* define accelerator
 */
static gint
capture_accel_for_action (GtkWidget * widget, GdkEventKey *event,
        set_accels_cb_data * cb_data) {
  cb_data->modifiers = dnm_sanitize_key_state(event);
  cb_data->keyval = event->keyval;
  gchar *accel_label = dnm_accelerator_name (event->keyval, cb_data->modifiers);
  gtk_button_set_label(GTK_BUTTON(widget), g_strdup_printf("%s [%s] %d %d",
              N_("shortcut"), accel_label, event->keyval, cb_data->modifiers));
  //FIXME memory leak
  //g_free(accel_label); what is the free for g_new???

  return TRUE;/* stop other handlers being processed */
}

static void
save_accels (void) {
  save_default_keymap_file (NULL);
  Denemo.accelerator_status = FALSE;
}

static gboolean
accept_keypress(GtkButton *button, set_accels_cb_data *cb_data){
  gtk_button_set_label(button, N_("Press the key combination desired"));
  cb_data->changed = SHORTCUT_ADD;
  // set cb_data->sigid =  and kill the signal when activated.
  g_signal_connect (GTK_OBJECT (button), "key_press_event",
		    G_CALLBACK (capture_accel_for_action), cb_data);
  return TRUE;
}

static gboolean
delete_accel(GtkButton *button, set_accels_cb_data *cb_data) {
  gtk_button_set_label(button, N_("Press the shortcut key that you wish to delete"));
  cb_data->changed = SHORTCUT_DELETE;
  g_signal_connect (GTK_OBJECT (button), "key_press_event",
		    G_CALLBACK (capture_accel_for_action), cb_data);
  //GtkButton *prop_button = cb_data->prop_button;
  //g_free(accel_label); what is the free for g_new???
  //gtk_button_set_label(prop_button, N_("An accel will be deleted"));
  return TRUE;/* stop other handlers being processed */

}

static 	void show_type(GtkWidget *widget, gchar *message);


static void configure_keyboard_idx (GtkWidget*w, gint idx) {
  DenemoGUI *gui = Denemo.gui;
  configure_keyboard_dialog_init_idx (NULL, gui, idx);
}

static void toggleRecording (GtkWidget*w, gboolean *record) {
  g_print("Recording was %d\n", *record);
  *record = !*record;
}

static void 
toggle_record_script(GtkAction *action, gpointer param) {
  Denemo.ScriptRecording = !Denemo.ScriptRecording;
}
/* returns newly allocated string containing current Scheme in the ScriptView
 caller must free
*/
gchar *getSchemeText(void) {
  GtkTextIter startiter, enditer;
  GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(Denemo.ScriptView));
  gtk_text_buffer_get_start_iter (buffer, &startiter);
  gtk_text_buffer_get_end_iter (buffer,  &enditer);
  return gtk_text_buffer_get_text(buffer, &startiter, &enditer, FALSE);

}

/* Return number of characters in Scheme script */
gint getNumCharsSchemeText(void) {
  GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(Denemo.ScriptView));
  return gtk_text_buffer_get_char_count(buffer);
}

void deleteSchemeText(void) {
  GtkTextIter startiter, enditer;
  GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(Denemo.ScriptView));
  gtk_text_buffer_get_start_iter (buffer, &startiter);
  gtk_text_buffer_get_end_iter (buffer,  &enditer);
    gtk_text_buffer_delete (buffer, &startiter, &enditer);
}

void appendSchemeText(gchar *text) {
  GtkTextIter enditer;
  GtkTextBuffer *buffer = gtk_text_view_get_buffer((GtkTextView*)(Denemo.ScriptView));
  gtk_text_buffer_get_end_iter (buffer,  &enditer);
  gtk_text_buffer_insert(buffer, &enditer, text, -1);
}

static appendSchemeText_cb(GtkWidget *widget, gchar *text) {
  appendSchemeText(text);
}


/* execute the script that is in the Scheme script window */
void executeScript(void) {
  gchar *text = getSchemeText();
  g_print("Calling script %s\n", text);
  (void)call_out_to_guile(text);
  g_free(text);
}
static void load_command_from_location(GtkWidget*w, gchar *filepath) {
  gchar *location = g_strdup_printf("%s%c", filepath, G_DIR_SEPARATOR);
  g_print("Calling the file loader with %s\n",location);
  load_keymap_dialog_location (w, location);
  g_free(location);
}


static void  attach_right_click_callback (GtkWidget *widget, GtkAction *action);

/* get the script for action from disk; 
 *   action an action loaded via load_xml_keymap() contains the menupath but no scheme script,
 */
gchar *instantiate_script(GtkAction *action){
  gchar *menupath = (gchar*)g_object_get_data(G_OBJECT(action), "menupath");
  const gchar *name = gtk_action_get_name(action);
  gchar *filename = g_build_filename (locatedotdenemo (), "actions","menus", menupath, name,
                                        NULL);
  g_print("Filename %s\n", filename);
  if (load_xml_keymap (filename)== -1) {

    filename = g_build_filename (get_data_dir (), "actions", "menus", menupath, name,
				 NULL);
    if (load_xml_keymap (filename)== -1)
      warningdialog("Unable to load the script");
  }
  g_free(filename);
  //g_print("Command loaded is following script:\n%s\n;;; end of loaded command script.\n", (gchar*)g_object_get_data(G_OBJECT(action), "scheme"));
  return  (gchar*)g_object_get_data(G_OBJECT(action), "scheme");
}


/* the callback for menu items that are scripts. The script is attached to the action,
tagged as "scheme".
The script may be empty, in which case it is fetched from actions/menus...

This call also ensures that the right-click callback is attached to all the proxies of the action, as there are problems trying to do this earlier, and it defines a scheme variable to give the name of the script being executed.
*/
void
activate_script (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  // the proxy list is NULL until the menu item is first called...
  //BUT if you first activate it with right button ....
  if(GTK_IS_ACTION(action)) {
    if(!g_object_get_data(G_OBJECT(action), "signal_attached")) {
      GSList *h = gtk_action_get_proxies (action);
      for(;h;h=h->next) {
	attach_right_click_callback(h->data, action);
	show_type(h->data, "type is ");
      }
    }
    gchar *text = (gchar*)g_object_get_data(G_OBJECT(action), "scheme");
    //define a global variable in Scheme (CurrentScript) to give the name of the currently executing script
    gchar *current_script = g_strdup_printf("(define CurrentScript \"%s\")\n", gtk_action_get_name(action));
    /*note that scripts must copy their name from CurrentScript into local storage before calling other scripts if they
      need it */
    scm_c_eval_string(current_script);
    g_free(current_script);
    if(*text==0)
      text = instantiate_script(action);
    (void)call_out_to_guile(text);//scm_c_eval_string(text);
  }
  else
    warningdialog("Have no way of getting the script, sorry");
}




/*pop up the help for passed command as info dialog
 */
static void popup_help(GtkWidget *widget, GtkAction *action) {
  const gchar *name = gtk_action_get_name(action);
  gint idx = lookup_command_from_name(Denemo.map, name);
  gchar *tooltip = idx>=0?(gchar *)lookup_tooltip_from_idx (Denemo.map, idx):"A menu for ...";

  tooltip = g_strdup_printf("Command: %s\n\nInformation:\n%s", name,  tooltip);
  infodialog (tooltip);
  g_free(tooltip);
  }

/* replace dangerous characters in command names */
static void  subst_illegals(gchar *myname) {gchar *c;// avoid whitespace etc
  for(c=myname;*c;c++)
    if(*c==' '||*c=='\t'||*c=='\n'||*c=='/'||*c=='\\') 
      *c='-';
  }



typedef struct ModifierAction {
  GtkAction *action;
  gint modnum;/* GdkModifierType number 0...12 */
  mouse_gesture gesture;/* if this is for press move or release */
  gboolean left;/* if this is for left or right mouse button */
}  ModifierAction;


// info->action is the action for which the mouse shortcut is to be set
static void setMouseAction(ModifierAction *info) {
  GString *modname = mouse_shortcut_name(info->modnum, info->gesture, info->left);
  gint command_idx = lookup_command_for_keybinding_name (Denemo.map, modname->str);
  GtkAction *current_action=NULL;
  gchar *title = NULL;
  gchar *prompt = NULL;
  if(command_idx >= 0) {
    current_action = (GtkAction *)lookup_action_from_idx(Denemo.map, command_idx);
    title = g_strdup_printf("The Command %s Responds to this Shortcut", lookup_name_from_idx(Denemo.map, command_idx));
    prompt = g_strdup_printf("Lose the shortcut %s for this?", modname->str);
  }
  if(current_action==NULL || confirm(title, prompt)) {
    remove_keybinding_from_name(Denemo.map, modname->str);//by_name 
    const gchar *name = gtk_action_get_name(info->action);
    command_idx = lookup_command_from_name (Denemo.map, name);
    if(command_idx >= 0)
      add_named_binding_to_idx (Denemo.map,  modname->str, command_idx, POS_LAST);
  }
  g_free(title);
  g_free(prompt);
  g_string_free(modname, TRUE);
}

/* gets a name label and tooltip from the user, then creates a menuitem in the menu 
   given by the path myposition whose callback is the activate on the current scheme script.
*/

static void insertScript(GtkWidget *widget, gchar *myposition) {
  DenemoGUI *gui = Denemo.gui;
  gchar *myname, *mylabel, *myscheme, *mytooltip, *submenu;
  myname = string_dialog_entry (gui, "Create a new menu item", "Give item name (avoid clashes): ", "MyName");
  //FIXME check for name clashes

  if(myname==NULL)
    return;
  subst_illegals(myname);
  mylabel = string_dialog_entry (gui, "Create a new menu item", "Give menu label: ", "My Label");
  if(mylabel==NULL)
    return;

  mytooltip = string_dialog_entry (gui, "Create a new menu item", "Give explanation of what it does: ", "Prints my special effect");
  if(mytooltip==NULL)
    return;
  if(confirm("Create a new menu item", "Do you want the new menu item in a submenu?"))
    {
      submenu = string_dialog_entry (gui, "Create a new menu item", "Give a label for the Sub-Menu", "Sub Menu Label");
      if(submenu) {
	subst_illegals(submenu);
	myposition = g_strdup_printf("%s/%s", myposition, submenu);//FIXME leak
      }
    }
  
  myscheme = getSchemeText();

  //FIXME G_DIR_SEPARATOR in myposition???
  gchar *filename = g_build_filename(locatedotdenemo(), "actions", "menus", myposition, myname,  NULL);
  g_print("The filename built is %s from %s", filename, myposition);
  if((!g_file_test(filename, G_FILE_TEST_EXISTS))  || (g_file_test(filename, G_FILE_TEST_EXISTS) &&
						       confirm("Duplicate Name", "A command of this name is already available in your custom menus; Overwrite?"))) {
    gchar *dirpath = g_path_get_dirname(filename);
    g_mkdir_with_parents(dirpath, 0770);
    g_free(dirpath);
    //g_file_set_contents(filename, text, -1, NULL);
    save_script_as_xml (filename, myname, myscheme, mylabel, mytooltip);
    load_xml_keymap(filename);
  } else
    warningdialog("Operation cancelled");
  return;
}



static void append_scheme_call(gchar *func) {
  GtkTextIter enditer;
  GtkTextBuffer *buffer = gtk_text_view_get_buffer((GtkTextView*)(Denemo.ScriptView));
  //gtk_text_buffer_set_text(buffer,"",-1);
  gtk_text_buffer_get_end_iter (buffer,  &enditer);
  gchar *text = g_strdup_printf("(d-%s)\n",func);//prefix dnm_!!!!!!!
  gtk_text_buffer_insert(buffer, &enditer, text, -1);
  //g_print("Added %s\n", text);
  g_free(text); 
}




static void button_choice_callback(GtkWidget *w, gboolean *left ){
  g_print("left at %p is %d\n", left, *left);
  *left =  gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w));
  g_print("left at %p is now %d\n", left, *left);
}

static void button_move_callback(GtkWidget *w, mouse_gesture *g ){
  if( gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w)))
       *g = GESTURE_MOVE;
  // g_print("move %d\n", *g);
}
static void button_press_callback(GtkWidget *w, mouse_gesture *g ){
  if( gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w)))
       *g = GESTURE_PRESS;
  // g_print("press  %d\n", *g);
}
static void button_release_callback(GtkWidget *w, mouse_gesture *g ){
  if( gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w)))
       *g = GESTURE_RELEASE;
  // g_print("release %d \n", *g);
}



static void button_modifier_callback(GtkWidget *w, GdkEventButton *event,  ModifierAction *ma ){
  ma->modnum = event->state;
  // show_type(w, "button mod callback: ");
  GString *str = g_string_new("Keyboard:");
  append_modifier_name(str, ma->modnum);
  if(!ma->modnum)
    g_string_assign (str, "No keyboard modifier keys\nPress with modifier key to change");
  else
    g_string_append(str, "\nPress with modifier key to change");
  gtk_button_set_label (GTK_BUTTON(w), str->str);
  g_string_free(str,TRUE);
}



static void
mouse_shortcut_dialog(ModifierAction *info){
  GtkWidget *dialog = gtk_dialog_new_with_buttons ("Set Mouse Shortcut",
                                        GTK_WINDOW (Denemo.window),
                                        (GtkDialogFlags) (GTK_DIALOG_MODAL |
                                                       GTK_DIALOG_DESTROY_WITH_PARENT),
                                        GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
                                        NULL);
  GtkWidget *hbox = gtk_hbox_new (FALSE, 1);
  GtkWidget *vbox = gtk_vbox_new (FALSE, 1);  
  gtk_box_pack_start (GTK_BOX (hbox), vbox, FALSE, TRUE, 0);
  gchar *name = (gchar*)gtk_action_get_name(info->action);
  gchar *prompt = g_strdup_printf("Setting mouse shortcut for %s", name);
  GtkWidget *label = gtk_label_new(prompt);
  g_free(prompt);
  gtk_box_pack_start (GTK_BOX (vbox), label, TRUE, TRUE, 0);
  GtkWidget *frame= gtk_frame_new( "Choose the mouse button");
  gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);
  gtk_container_add (GTK_CONTAINER (vbox), frame);
  GtkWidget *vbox2 = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (frame), vbox2);

  info->left = TRUE;
  GtkWidget *widget =   gtk_radio_button_new_with_label(NULL, "Left");
  g_signal_connect(G_OBJECT(widget), "toggled", G_CALLBACK(button_choice_callback), &info->left);
  gtk_box_pack_start (GTK_BOX (vbox2), widget, FALSE, TRUE, 0);
  GtkWidget *widget2  =   gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON (widget), "Right");
  gtk_box_pack_start (GTK_BOX (vbox2), widget2, FALSE, TRUE, 0);
  

  frame= gtk_frame_new( "Choose mouse action");
  gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);
  gtk_container_add (GTK_CONTAINER (vbox), frame);
  vbox2 = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (frame), vbox2);
  info->gesture = GESTURE_PRESS;
  widget =   gtk_radio_button_new_with_label(NULL, "Press Button");
  g_signal_connect(G_OBJECT(widget), "toggled", G_CALLBACK(button_press_callback), &info->gesture);
  gtk_box_pack_start (GTK_BOX (vbox2), widget, FALSE, TRUE, 0);
  widget2  =   gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON (widget), "Release Button");
  g_signal_connect(G_OBJECT(widget2), "toggled", G_CALLBACK(button_release_callback), &info->gesture);
  gtk_box_pack_start (GTK_BOX (vbox2), widget2, FALSE, TRUE, 0);
  widget2  =   gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON (widget), "Drag");
  g_signal_connect(G_OBJECT(widget2), "toggled", G_CALLBACK(button_move_callback), &info->gesture);
  gtk_box_pack_start (GTK_BOX (vbox2), widget2, FALSE, TRUE, 0);

  widget =   gtk_button_new_with_label("Hold Modifier Keys, Engage Caps or Num Lock\nand click here to set shorcut.");
  g_signal_connect(G_OBJECT(widget), "button-release-event", G_CALLBACK(button_modifier_callback), info);
  gtk_box_pack_start (GTK_BOX (vbox), widget, FALSE, TRUE, 0);

  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), hbox,
		      TRUE, TRUE, 0);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT){ 
    setMouseAction(info);
    Denemo.accelerator_status = TRUE;
  }
  gtk_widget_destroy (dialog);
}

static void  createMouseShortcut(GtkWidget *menu, GtkAction *action) {
  static ModifierAction info;
  info.action = action;
  info.gesture = GESTURE_PRESS;
  info.modnum = 0;
  info.left = TRUE;
  mouse_shortcut_dialog(&info);
}

/* save the action (which must be a script),
   setting the script text to the script currently in the ScriptView
   The save is to the user's menu hierarchy on disk
*/
static void saveMenuItem (GtkWidget *widget, GtkAction *action) {
  gchar *name = (gchar *)gtk_action_get_name(action);
  gchar *menupath = g_object_get_data(G_OBJECT(action), "menupath");
  gint idx = lookup_command_from_name(Denemo.map, name);
  gchar *tooltip = (gchar*)lookup_tooltip_from_idx(Denemo.map, idx);
  gchar *label = (gchar*)lookup_label_from_idx(Denemo.map, idx);
  
  gchar *filename = g_build_filename (locatedotdenemo (), "actions","menus", menupath, name,
				      NULL);
  gchar *scheme = getSchemeText();
  if(scheme && *scheme)
    save_script_as_xml (filename, name, scheme, label, tooltip);
  else
    warningdialog("No script to save");
}

static const gchar *
locatebitmapsdir(void) {
  static gchar *bitmapsdir = NULL;
  gboolean err;
  if (!bitmapsdir)
    {
      bitmapsdir = g_build_filename (locatedotdenemo(), "actions", "bitmaps", NULL);
    }
  err = g_mkdir_with_parents(bitmapsdir, 0770);
  if(err) {
    warningdialog("Could not create .denemo/actions/bitmaps for your graphics for customized commands");
    g_free(bitmapsdir);
    bitmapsdir = g_strdup("");
  }
  return bitmapsdir;
}

/* if a graphic file for name exists (local or systemwide) create an icon for it called label
and return label, else return NULL
*/
gchar *
get_icon_for_name(gchar *name, gchar *label) {

  gchar *pngname = g_strconcat(name, ".png", NULL);
  gchar *filename = g_build_filename (locatebitmapsdir (), pngname,
				      NULL);
  if(!g_file_test(filename, G_FILE_TEST_EXISTS)) {
    g_free(filename);
    filename = g_build_filename (get_data_dir (), "actions", "bitmaps", pngname, 
				 NULL);
    if(!g_file_test(filename, G_FILE_TEST_EXISTS)) {
      g_free(filename);
      g_free(pngname);
      return NULL;
    }
  }
  GError *error = NULL;
  GdkPixbuf *pixbuf = gdk_pixbuf_new_from_file (filename, &error);
  g_free(filename);
  g_free(pngname);
  if(error) {
    warningdialog(error->message);		 
    return NULL;
  }
  static GtkIconFactory *icon_factory;
  if(!icon_factory){
    icon_factory = gtk_icon_factory_new ();
    gtk_icon_factory_add_default (icon_factory);
  }
  GtkIconSet *icon_set = gtk_icon_set_new_from_pixbuf (pixbuf);
  g_object_unref(pixbuf);
  gtk_icon_factory_add (icon_factory, label, icon_set);
  return label;
}



gchar *
create_xbm_data_from_pixbuf (GdkPixbuf *pixbuf, int lox, int loy, int hix, int hiy)
{
  int width, height, rowstride, n_channels;
  guchar *pixels, *p;

  n_channels = gdk_pixbuf_get_n_channels (pixbuf);

#ifdef DEBUG
  g_assert (gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);
  g_assert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);
  g_assert (gdk_pixbuf_get_has_alpha (pixbuf));
  g_assert (n_channels == 4);
#endif
  width = hix - lox;
  height = hiy - loy;      
  rowstride = gdk_pixbuf_get_rowstride (pixbuf);
  pixels = gdk_pixbuf_get_pixels (pixbuf);
  int x, y, i;

  unsigned char *chars = g_malloc0(sizeof(char) * width*height);//about 8 times too big!
  unsigned char * this = chars;
  for(i=0, y=loy;y<hiy;y++)
    {
      for(x=lox;x<hix;x++, i++) {
	this = chars + (i/8);
	gint set = ((pixels + y * rowstride + x * n_channels)[3]>0);
	*this += set<<i%8;
      }
      i = ((i+7)/8)*8;
    }
  return chars;
}

static GHashTable *bitmaps;
static void hash_table_insert(gchar *name, GdkBitmap *xbm) {
  if(!bitmaps)
    bitmaps = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
  g_hash_table_insert(bitmaps, g_strdup(name), xbm);
}

static gboolean
loadGraphicFromFormat(gchar *basename, gchar *name, GdkBitmap **xbm, gint *width, gint *height) {

  GError *error = NULL;
  GdkPixbuf *pixbuf = gdk_pixbuf_new_from_file (name, &error);
  if(error) {
    //warningdialog(error->message);
    g_warning("creating pixbuf from file %s gave %s\n", name, error->message);
    return FALSE;
  }
  GdkPixbuf *pixbufa = gdk_pixbuf_add_alpha (pixbuf, TRUE, 255, 255, 255);
  *width = gdk_pixbuf_get_width(pixbufa);
  *height = gdk_pixbuf_get_height(pixbufa);
  gchar *data = create_xbm_data_from_pixbuf(pixbufa, 0,0,*width, *height);
  *xbm = gdk_bitmap_create_from_data (NULL, data, *width, *height);
  hash_table_insert(basename, *xbm);
  g_free(data);
  return TRUE;
}


static gboolean
loadGraphicFromFormats(gchar *basename, gchar *name, GdkBitmap **xbm, gint *width, gint *height ) {
  GError *error = NULL;
  gchar *filename = g_strconcat(name, ".png", NULL);
  return loadGraphicFromFormat(basename, filename, xbm, width, height);//FIXME free filename
  //others .jpg .svg ....		 
    return FALSE;
}


gboolean loadGraphicItem(gchar *name, GdkBitmap **xbm, gint *width, gint *height ) {
  if(bitmaps && (*xbm = (GdkBitmap *) g_hash_table_lookup(bitmaps, name))) {
    gdk_drawable_get_size(*xbm, width, height);
    return TRUE;
  }  
  gchar *filename = g_build_filename (locatebitmapsdir (), name,
				      NULL);
  
  if(!g_file_test(filename, G_FILE_TEST_EXISTS)) {
    if(loadGraphicFromFormats(name, filename, xbm, width, height))
      return TRUE;
    g_free(filename);
    filename = g_build_filename (get_data_dir (), "actions", "bitmaps", name,
				      NULL);
  }
  if(!g_file_test(filename, G_FILE_TEST_EXISTS)) {
    if(loadGraphicFromFormats(name, filename, xbm, width, height))
      return TRUE;
  } 
  FILE *fp = fopen(filename,"rb");
  if(fp) {
    guchar wlo, whi, hlo, hhi;
    gint w, h;
    fread(&wlo, 1, 1, fp);
    fread(&whi, 1, 1, fp);

    fread(&hlo, 1, 1, fp);
    fread(&hhi, 1, 1, fp);
    w = wlo+255*whi;
    h = hlo+255*hhi;
    gint numbytes = h*((w+7)/8)*8;
    gchar *data = g_malloc(numbytes);    
    //g_print("Hope to read %d bytes for %d x %d\n", numbytes, w, h);
    if(numbytes == fread(data, 1, numbytes, fp)){
      *xbm = gdk_bitmap_create_from_data(NULL, data, w, h);
      hash_table_insert(name, *xbm);
      *width = w; *height = h;
      fclose(fp);
      return TRUE;
    }
    fclose(fp);  
  } else {
    g_warning("Could not load graphic");
    //warningdialog("Could not load graphic");
  }


  return FALSE;
}

/* save the current graphic
*/
static void saveGraphicItem (GtkWidget *widget, GtkAction *action) {
  GError *error = NULL;
  gchar *name = (gchar *)gtk_action_get_name(action);
  gchar *pngname = g_strconcat(name, ".png", NULL);
  gchar *filename = g_build_filename (locatebitmapsdir (),  pngname,
				      NULL);
  //FIXME allow fileselector here to change the name
  gchar *msg = g_strdup_printf("Saving a graphic for use in the %s script", name);
  if( !g_file_test(filename,  G_FILE_TEST_EXISTS) || confirm (msg, "Replace current graphic?")) {
    guint width = Denemo.gui->xbm_width;
    guint height = Denemo.gui->xbm_height;
    
    
    GdkBitmap *bitmap = gdk_bitmap_create_from_data(NULL, Denemo.gui->xbm, width, height);
    GdkPixbuf *pixbuf1 = gdk_pixbuf_get_from_drawable (NULL,  bitmap, NULL, 0,0,0,0, width, height);

    GdkPixbuf *pixbuf = gdk_pixbuf_add_alpha (pixbuf1, TRUE, 0,0,0);// 255, 255, 255);

    guchar *pixels;
    gint n_channels = gdk_pixbuf_get_n_channels (pixbuf);
    g_assert (gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);
    g_assert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);
    g_assert (gdk_pixbuf_get_has_alpha (pixbuf));
    g_assert (n_channels == 4);
    gint rowstride = gdk_pixbuf_get_rowstride (pixbuf);
    pixels = gdk_pixbuf_get_pixels (pixbuf);
    int x, y, i;
    for(i=0, y=0;y<height;y++)
      {
	for(x=0;x<width;x++, i++) {
	  gint set = !((pixels + y * rowstride + x * n_channels)[3]>0);
	  (pixels + y * rowstride + x * n_channels)[0] = 0xFF * set;
	  (pixels + y * rowstride + x * n_channels)[1] = 0xFF * set;
	  (pixels + y * rowstride + x * n_channels)[2] = 0xFF * set;
	}
      }


    gdk_pixbuf_save (pixbuf, filename, "png", &error, "compression", "2", NULL);


#if 0
      FILE *fp = fopen(filename,"wb");
      if(fp) {
      guchar whi, wlo, hhi, hlo;
      wlo = width&0xFF;
      whi = width>>8;
      hlo = height&0xFF;
      hhi = height>>8;
      
      fwrite(&wlo, 1, 1, fp);
      fwrite(&whi, 1, 1, fp);
      
      fwrite(&hlo, 1, 1, fp);
      fwrite(&hhi, 1, 1, fp);
      
      gint size = fwrite(Denemo.gui->xbm, 1, height*((width+7)/8)*8, fp);
    //g_print("Wrote %d bytes for %d x %d\n", size, width, height);


      g_free(msg);
      msg = g_strdup_printf("Saved graphic as file %s", filename);
      infodialog(msg);
      fclose(fp);  
    }
    else
      warningdialog("Could not write file");
#endif
  }

  g_free(pngname);
  g_free(msg);
  g_free(filename);
}

/*
  menu_click:
  intercepter for the callback when clicking on menu items for the set of Actions the Denemo offers.
  Left click runs default action, after recording the item in a scheme script if recording.
  Rigth click offers pop-up menu for setting shortcuts etc

*/
static gboolean menu_click (GtkWidget      *widget,
			  GdkEventButton *event,
			  GtkAction *action)
{
  keymap *the_keymap = Denemo.map;
  const gchar *func_name = gtk_action_get_name(action);
  //g_print("widget name %s action name %s\n", gtk_widget_get_name(widget), func_name);

  // GSList *h = gtk_action_get_proxies (action);
  //g_print("In menu click action is %p h is %p\n",action, h);



  gint idx = lookup_command_from_name (the_keymap, func_name);
  //g_print("event button %d, idx %d for %s recording = %d scm = %d\n", event->button, idx, func_name, Denemo.ScriptRecording,g_object_get_data(G_OBJECT(action), "scm") );
  if (event->button != 3) //Not right click
    if(Denemo.ScriptRecording)
      if(idx_has_callback(the_keymap, idx)){
	if(g_object_get_data(G_OBJECT(action), "scm"))	
	   append_scheme_call((gchar*)func_name);
	else if(g_object_get_data(G_OBJECT(action), "scheme"))
	  appendSchemeText(g_object_get_data(G_OBJECT(action), "scheme"));//FIXME Should insert (d-<name of action>) now
	//return TRUE;
      }

  if (event->button != 3)
    return FALSE;


#if 0
  /* This idx is -1 for the toggles and radio entries because they share a callback function. If we want to allow setting keybindings, getting help etc. for these then we would need to re-work all the radio action entries code using generate_source.c. Instead at the moment we have just defined scheme callback functions d-EditMode etc. using a hand-created array activatable_commands earlier in this file.
   It is also for menus themselves, so we process the case further.*/
  if (idx == -1)
    return TRUE;
#endif

  GtkWidget *menu = gtk_menu_new();
  gchar *labeltext = g_strdup_printf("Help for %s", func_name);
  GtkWidget *item = gtk_menu_item_new_with_label(labeltext);
  g_free(labeltext);
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
  g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(popup_help), (gpointer)action);


  if(idx!=-1) {
    item = gtk_menu_item_new_with_label("Create Mouse Shortcut");
    gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
    g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(createMouseShortcut), action);


    item = gtk_menu_item_new_with_label("Edit Shortcuts\nSet Mouse Pointers\nHide/Delete Menu Item");
    gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
    g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(configure_keyboard_idx), (gpointer)idx);


    item = gtk_separator_menu_item_new();
    gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
   
  }//idx!=-1

  gchar *myposition = g_object_get_data(G_OBJECT(widget), "menupath");// applies if it is a built-in command
  if(!myposition)
    myposition = g_object_get_data(G_OBJECT(action), "menupath");//menu item runs a script
  //g_print("Connecting to %s\n", g_object_get_data(G_OBJECT(widget), "menupath"));


  static gchar *filepath;// static so that we can free it next time we are here.
  if(filepath)
    g_free(filepath);
  filepath = g_build_filename (get_data_dir(), "actions", "menus", myposition, NULL);
  if(0==g_access(filepath, 4)) {
    //g_print("We can create a menu item for the path %s\n", filepath);
    item = gtk_menu_item_new_with_label("More Commands for this Menu");
    gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
    g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(load_command_from_location), (gpointer)filepath);
  }



  gchar *scheme = g_object_get_data(G_OBJECT(action), "scheme");
  if(scheme) {
    if(*scheme==0)
      scheme = instantiate_script(action);
    item = gtk_menu_item_new_with_label("Get Script");
    gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
    g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(appendSchemeText_cb), scheme);
    item = gtk_menu_item_new_with_label("Save Script");
    gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
    g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(saveMenuItem), action);
    if(Denemo.gui->xbm) {
      item = gtk_menu_item_new_with_label("Save Graphic");
      // GtkSettings* settings = gtk_settings_get_default();
      // gtk_settings_set_long_property  (settings,"gtk-menu-images",(glong)TRUE, "XProperty");
      //item = gtk_image_menu_item_new_from_stock("Save Graphic", gtk_accel_group_new());
      item = gtk_image_menu_item_new_from_stock("Save Graphic"/*GTK_STOCK_OK*/, NULL);
      
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
      g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(saveGraphicItem), action);
    }


  }
  if (GTK_WIDGET_VISIBLE(gtk_widget_get_toplevel(Denemo.ScriptView))) {
    item = gtk_menu_item_new_with_label("Save Script as New Menu Item");
    gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
    g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(insertScript), myposition);
  }

  /* a check item for showing script window */
  item = gtk_check_menu_item_new_with_label("Show Current Script");
  gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(item), GTK_WIDGET_VISIBLE(gtk_widget_get_toplevel(Denemo.ScriptView)));
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
  //FIXME the next statement triggers a warning that ToggleScript is not a registered denemo commad - correct, since we do not make the toggles available as commands since using such a command would make the check boxes out of step, instead we install function that activate the menuitem.
  gtk_action_connect_proxy(gtk_ui_manager_get_action (Denemo.ui_manager, "/MainMenu/ViewMenu/ToggleScript"), item);


  gtk_widget_show_all(menu);
  gtk_menu_popup (GTK_MENU(menu), NULL, NULL, NULL, NULL,0, gtk_get_current_event_time()); 
  // configure_keyboard_dialog_init_idx (action, gui, idx);
  return TRUE;
}



static void	  color_rhythm_button(RhythmPattern *r, const gchar *color) {
  if(r==NULL) return;
  GdkColor thecolor;
  gdk_color_parse (color, &thecolor);
  gtk_widget_modify_bg (gtk_tool_button_get_label_widget(GTK_TOOL_BUTTON(r->button)), GTK_STATE_NORMAL, &thecolor);
  
}
void	  highlight_rhythm(RhythmPattern *r) {
  color_rhythm_button(r, "green");
}

void	  unhighlight_rhythm(RhythmPattern *r) {
  color_rhythm_button(r, "gray");
}


/*
 

  
*/
void	highlight_rest(DenemoGUI *gui, gint dur) {  

  //g_print("highlight rest");
      if(gui->currhythm) {
	unhighlight_rhythm((RhythmPattern *)gui->currhythm->data);	
      }
      gui->currhythm = NULL;
      gui->rstep = Denemo.singleton_rhythms['r'+dur]->rsteps;
      unhighlight_rhythm(gui->prevailing_rhythm);
      gui->prevailing_rhythm = Denemo.singleton_rhythms['r'+dur];
      highlight_rhythm(gui->prevailing_rhythm);

}

void	highlight_duration(DenemoGUI *gui, gint dur) {  

  //g_print("higlight duration");
      if(gui->currhythm) {
	unhighlight_rhythm((RhythmPattern *)gui->currhythm->data);	
      }
      gui->currhythm = NULL;
      gui->rstep =  Denemo.singleton_rhythms['0'+dur]->rsteps;
      unhighlight_rhythm(gui->prevailing_rhythm);
      gui->prevailing_rhythm = Denemo.singleton_rhythms['0'+dur];
      highlight_rhythm(gui->prevailing_rhythm);
}


/*
 * delete a rhythmic pattern and its button
 * 
 */
static void
delete_rhythm_cb (GtkAction * action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  if(gui->mode&(INPUTEDIT) == 0)
    return;
  if(gui->currhythm==NULL)
    return;
  RhythmPattern *r =(RhythmPattern *)gui->currhythm->data;
  gtk_widget_destroy(GTK_WIDGET(r->button));
  /* list is circular, so before we free it we have to break it */
  r->rsteps->prev->next = NULL;
  r->rsteps->prev = NULL;
  GList *g;
  for(g=r->rsteps;g;g=g->next)
    g_free(g->data);
  g_list_free(r->rsteps);
  g_free(r);
  //g_print("length %d\n", g_list_length(gui->rhythms));
  gui->rhythms = g_list_remove(gui->rhythms, gui->currhythm->data);
  //g_print("length %d %p\n", g_list_length(gui->rhythms), gui->rhythms);
  gui->currhythm = g_list_last(gui->rhythms);

  if(gui->currhythm == NULL)
   gui->rstep = NULL;
  else {
    highlight_rhythm(gui->currhythm->data);
    gui->rstep = ((RhythmPattern *)gui->currhythm->data)->rsteps;
  }
}



/*
 * workaround for glib<2.10
 */
static
void  attach_action_to_widget (GtkWidget *widget, GtkAction *action, DenemoGUI *gui) {
  g_object_set_data(G_OBJECT(widget), "action", action);
}

/* attaches a button-press-event signal to the widget with the action as data
   for use in the callback */
static void  attach_right_click_callback (GtkWidget *widget, GtkAction *action) {


#if 0
  //gtk_widget_add_events (widget, (GDK_BUTTON_RELEASE_MASK));
  if(GTK_IS_CHECK_MENU_ITEM(widget))
    g_signal_connect(G_OBJECT(widget), "toggled", G_CALLBACK (menu_toggle), info);
  //g_print("Action of check menu item is %p\n", action);

  g_signal_connect(G_OBJECT(widget), "button-release-event", G_CALLBACK (menu_click), info);
  
  //if(!strcmp("ToggleRhythm", gtk_action_get_name(action)))
  //  g_print("Action %s has widget %p\n", gtk_action_get_name(action), widget);
#else
  gtk_widget_add_events (widget, (GDK_BUTTON_PRESS_MASK));
  g_signal_connect(G_OBJECT(widget), "button-press-event", G_CALLBACK (menu_click), action);
  //g_print("menu click set on %s GTK_WIDGET_FLAGS %x\n", gtk_action_get_name(action), GTK_WIDGET_FLAGS(widget));
  //show_type(widget, "Type is ");
#endif
  g_object_set_data(G_OBJECT(action), "signal_attached", action);//Non NULL to indicate the signal is attached
}



static void dummy(void) {

  return;
}

/**
 * Menu entries with no shortcut keys, tooltips, and callback functions
 */
GtkActionEntry menu_entries[] = {
#include "entries.h"
  {"Stub",  NULL, N_(" "), NULL, N_("Does nothing"), G_CALLBACK (dummy)}

  };

//Get number of menu entries
//gint n_menu_items = G_N_ELEMENTS (menu_entries);

static
GtkWidget *get_edit_menu_for_mode(gint mode) {
  if(mode&INPUTEDIT)
    return Denemo.EditModeMenu;
  if(mode&INPUTINSERT)
    return Denemo.InsertModeMenu;
  if(mode&INPUTCLASSIC)
    return Denemo.ClassicModeMenu;
  return Denemo.ModelessMenu;
}

/**
 *  callback changing mode  gui->mode
 *
 */
static void
change_mode (GtkRadioAction * action, GtkRadioAction * current) {
  DenemoGUI *gui = Denemo.gui;
gint val = gtk_radio_action_get_current_value (current);
 GtkWidget *menu = get_edit_menu_for_mode(gui->mode);
 if(menu)
   gtk_widget_hide(menu);
 gui->mode=((gui->mode&MODE_MASK)|val);
 menu = get_edit_menu_for_mode(gui->mode);
 if(menu)
   gtk_widget_show(menu);
 write_status(gui);
 
}


static void   activate_action(gchar *path) {
   GtkAction *a;
   a = gtk_ui_manager_get_action (Denemo.ui_manager, path);
   if(a)
   gtk_action_activate(a);
   else 
     g_warning("Internal error, denemogui.xml out of step with literal %s in %s\n", path, __FILE__);
 }

/**
 *  callback changing the input source (keyboard only/audio/midi)
 *
 */

static void
change_input_type (GtkRadioAction * action, GtkRadioAction * current) {
  DenemoGUI *gui = Denemo.gui;
gint val = gtk_radio_action_get_current_value (current);
 switch(val) {
 case INPUTKEYBOARD:
   if(gui->input_source==INPUTAUDIO) {
     // g_print("Stopping audio\n");
     stop_pitch_input();
   }
   if(gui->input_source==INPUTMIDI) {
     // g_print("Stopping midi\n");
     stop_pitch_input();
   }
   gui->input_source=INPUTKEYBOARD;
   break;
 case INPUTAUDIO:
   //g_print("Starting audio\n");
   if(gui->input_source==INPUTMIDI) {
     //g_print("Stopping midi\n");
     stop_pitch_input();
   }
   gui->input_source=INPUTAUDIO;
   if(setup_pitch_input()){
     warningdialog("Could not start Audio input");
     gui->input_source=INPUTKEYBOARD;
   } else
     start_pitch_input();
   break;
 case INPUTMIDI:
   //g_print("Starting midi\n");
   if(gui->input_source==INPUTAUDIO) {
     //g_print("Stopping audio\n");
     stop_pitch_input();
   }
   gui->input_source=INPUTMIDI;
   if(setup_pitch_input()){
     warningdialog("Could not start MIDI input");
     gui->input_source=INPUTKEYBOARD;
   } else
     start_pitch_input();
   break;
 default:
   g_warning("Bad Value\n");
   break;

 }

 write_input_status();
}
/**
 *  callback changing type of entry part of gui->mode,
 * depending on the entry type it switches mode part of gui->mode to Classic mode for entering rests and to Insert for entering notes. FIXME could switch to prefs value.
 *
 */
static void
change_entry_type (GtkRadioAction * action, GtkRadioAction * current) {
  DenemoGUI *gui = Denemo.gui;
gint val = gtk_radio_action_get_current_value (current);
 switch(val) {
#define SET_MODE(m)  (gui->mode=((gui->mode&ENTRY_TYPE_MASK)|m))
 case INPUTREST:
   SET_MODE(INPUTREST);
   activate_action("/MainMenu/ModeMenu/ClassicMode");

   break;
 case INPUTNORMAL:
   SET_MODE(INPUTNORMAL);
   activate_action( "/MainMenu/ModeMenu/InsertMode");
   break;
 case INPUTBLANK:
   SET_MODE(INPUTBLANK);
   activate_action( "/MainMenu/ModeMenu/ClassicMode");
   break;
 case INPUTRHYTHM|INPUTNORMAL:
   SET_MODE(INPUTRHYTHM|INPUTNORMAL);
   activate_action( "/MainMenu/ModeMenu/EditMode");
   break;
 }
#undef SET_MODE

write_status(gui);
 //g_print("Mode is %x masks %x %x\n",ENTRY_TYPE_MASK, MODE_MASK, gui->mode);
}

/* callback: if not Insert mode set Insert mode else set Edit mode */
static void toggle_edit_mode (GtkAction * action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  static gint mode=INPUTINSERT;
  if(gui->mode&INPUTEDIT){
    switch(mode & ~MODE_MASK ) {
    case INPUTINSERT:
      activate_action( "/MainMenu/ModeMenu/InsertMode");
      break;
    case INPUTCLASSIC:
      activate_action( "/MainMenu/ModeMenu/ClassicMode");
      break;
    case 0:
      activate_action( "/MainMenu/ModeMenu/Modeless");
      break;
    default:
      ;
    }
  } else {
    mode = gui->mode;// remember mode for switching back
    activate_action( "/MainMenu/ModeMenu/EditMode");
  }
}

/* callback: if rest entry make note entry and vv */
static void toggle_rest_mode (GtkAction * action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  static gint mode=INPUTNORMAL;
  if(gui->mode&INPUTREST){
    switch(mode & ~ENTRY_TYPE_MASK ) {
    case INPUTNORMAL:
      activate_action( "/MainMenu/ModeMenu/Note");
      break;
    case INPUTBLANK:
      activate_action( "/MainMenu/ModeMenu/Blank");
      break;
    default:
      ;
    }
  } else {
    mode = gui->mode;// remember mode for switching back
    activate_action( "/MainMenu/ModeMenu/Rest");
  }
}


/* callback: if rhythm entry make note entry and vv */
static void toggle_rhythm_mode (GtkAction * action, gpointer param){
  DenemoGUI *gui = Denemo.gui;
  static gint mode=INPUTNORMAL;
  if(gui->mode&INPUTRHYTHM){
    switch(mode & ~ENTRY_TYPE_MASK ) {
    case INPUTNORMAL:
      activate_action( "/MainMenu/ModeMenu/Note");
      break;
    default:
      ;
    }
  } else {
    mode = gui->mode;// remember mode for switching back, breaks with multi gui FIXME
    activate_action( "/MainMenu/ModeMenu/Rhythm");
  }
}

/**
 *  Function to toggle the visibility of the LilyPond text window. It refreshes 
 *  the text if needed
 */
static void
toggle_lilytext (GtkAction * action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
  //if(!gui->textview)
   refresh_lily_cb(action, gui);
 if(!GTK_WIDGET_VISIBLE(gui->textwindow))
   gtk_widget_show/*_all*/(gui->textwindow);
 else
   gtk_widget_hide(gui->textwindow);
 //g_print("toggling lily window");
}


/**
 *  Function to toggle the visibility of the Scheme text window. 
 */
static void
toggle_scheme (GtkAction * action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
  GtkWidget *textwindow = gtk_widget_get_toplevel(Denemo.ScriptView);
 if(!GTK_WIDGET_VISIBLE(textwindow))
   gtk_widget_show_all(textwindow);
 else
   gtk_widget_hide_all(textwindow);
 // g_print("toggling scheme window");
}
static gint
hide_scheme (GtkAction * action, GdkEvent*event,  GtkWidget *w) {
  activate_action("/MainMenu/ViewMenu/ToggleScript");
  return TRUE;
}




/**
 *  Function to toggle whether rhythm toolbar is visible 
 *  (no longer switches keymap to Rhythm.keymaprc when toolbar is on back to standard when off.)
 *  
 */
static void
toggle_rhythm_toolbar (GtkAction * action, DenemoGUI * gui)
{
  GtkWidget *widget;
  widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/RhythmToolBar");
 // g_print("Callback for %s\n", g_type_name(G_TYPE_FROM_INSTANCE(widget)));
  if (GTK_WIDGET_VISIBLE (widget))
    {
      
      gtk_widget_hide (widget);
    }
  else
    {

      gtk_widget_show (widget);
      /* make sure we are in Insert and Note for rhythm toolbar */
      activate_action( "/MainMenu/ModeMenu/Note");
      activate_action( "/MainMenu/ModeMenu/InsertMode");
    }
}


/**
 *  Function to toggle whether entry toolbar is visible 
 *  
 *  
 */
static void
toggle_entry_toolbar (GtkAction * action, DenemoGUI * gui)
{
  GtkWidget *widget;
  widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/EntryToolBar");
  if(!widget) return;// internal error - out of step with menu_entries...
  if (GTK_WIDGET_VISIBLE (widget))
    {
 
      gtk_widget_hide (widget);
    }
  else
    {
      gtk_widget_show (widget);
    }
}

/**
 *  Function to toggle whether keyboard bindings can be set by pressing key over menu item 
 *  
 *  
 */
static void
toggle_quick_edits (GtkAction * action, DenemoGUI * gui)
{
  Denemo.QuickShortcutEdits = !Denemo.QuickShortcutEdits;
}


/**
 *  Function to toggle whether action menubar is visible 
 *  
 *  
 */
static void
toggle_action_menu (GtkAction * action, DenemoGUI * gui)
{
  GtkWidget *widget;
  widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ActionMenu");
  if(!widget) return;// internal error - out of step with menu_entries...
  if (GTK_WIDGET_VISIBLE (widget))
    {
 
      gtk_widget_hide (widget);
    }
  else
    {
      gtk_widget_show (widget);
    }
}

/**
 *  Function to toggle visibility of print preview pane of current gui
 *  
 *  
 */
void
toggle_print_view (GtkAction *action, gpointer param)
{
  GtkWidget *w = gtk_widget_get_parent(gtk_widget_get_parent(Denemo.gui->printarea));
  if(GTK_WIDGET_VISIBLE(w))
    gtk_widget_hide(w);
  else {
    gtk_widget_show(w);
    if(g_object_get_data(G_OBJECT(Denemo.gui->printarea), "printviewupdate")<Denemo.gui->changecount)
      refresh_print_view();
  }
  return;
}
/**
 *  Function to toggle visibility of print preview pane of current gui
 *  
 *  
 */
void
toggle_score_view (GtkAction *action, gpointer param)
{
  GtkWidget *w = gtk_widget_get_parent(gtk_widget_get_parent(Denemo.gui->scorearea));
  if(GTK_WIDGET_VISIBLE(w))
    gtk_widget_hide(w);
  else {
    gtk_widget_show(w);
    gtk_widget_grab_focus(Denemo.gui->scorearea);
  }
  return;
}
/**
 *  Function to toggle visibility of print preview pane of current gui
 *  
 *  
 */
static void
toggle_scoretitles (GtkAction *action, gpointer param)
{
  Denemo.prefs.visible_titles = !Denemo.prefs.visible_titles;
  if(Denemo.gui && Denemo.gui->scorearea)gtk_widget_queue_draw (Denemo.gui->scorearea);
  return;
}
/**
 *  Function to toggle whether object menubar is visible 
 *  
 *  
 */
static void
toggle_object_menu (GtkAction * action, DenemoGUI * gui)
{
  GtkWidget *widget;
  widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu");
  if(!widget) return;// internal error - out of step with menu_entries...
  if (GTK_WIDGET_VISIBLE (widget))
    {
 
      gtk_widget_hide (widget);
    }
  else
    {
      gtk_widget_show (widget);
    }
}



/**
 * Toggle entries for the menus
 */
GtkToggleActionEntry toggle_menu_entries[] = {
  {ToggleRhythmToolbar_STRING, NULL, N_("Rhythms and Overlays"), NULL, N_("Show/hide a toolbar which allows\nyou to enter notes using rhythm patterns and\nto overlay these with pitches"),
   G_CALLBACK (toggle_rhythm_toolbar), FALSE}
  ,
  {ToggleEntryToolbar_STRING, NULL, N_("Note and rest entry"), NULL, N_("Show/hide a toolbar which allows\nyou to enter notes and rests using the mouse"),
   G_CALLBACK (toggle_entry_toolbar), FALSE}
  ,
  {ToggleObjectMenu_STRING, NULL, N_("Menu of objects"), NULL, N_("Show/hide a menu which is arranged by objects\nThe actions available for note objects change with the mode"),
   G_CALLBACK (toggle_object_menu), FALSE}
  ,
  {ToggleLilyText_STRING, NULL, N_("Show LilyPond"), NULL, N_("Show/hide the LilyPond music typesetting language window"),
   G_CALLBACK (toggle_lilytext), FALSE}
  ,
  {ToggleScript_STRING, NULL, N_("Show Scheme Script"), NULL, N_("Show scheme script window"),
   G_CALLBACK (toggle_scheme), FALSE}
  ,

  {ToggleArticulationPalette_STRING, NULL, N_("_Articulation Palette"), NULL, NULL,
   G_CALLBACK (toggle_articulation_palette), FALSE},

  {TogglePrintView_STRING, NULL, N_("Print View"), NULL, NULL,
   G_CALLBACK (toggle_print_view), FALSE},
  {ToggleScoreView_STRING, NULL, N_("Score View"), NULL, NULL,
   G_CALLBACK (toggle_score_view), TRUE},
  {ToggleScoreTitles_STRING, NULL, N_("Score Titles"), NULL, NULL,
   G_CALLBACK (toggle_scoretitles), TRUE},


  {QuickEdits_STRING, NULL, N_("Allow Quick Shortcut Edits"), NULL, "Enable editing keybindings by pressing a key while hovering over the menu item",
   G_CALLBACK (toggle_quick_edits), FALSE},
  {RecordScript_STRING, NULL, N_("Record Scheme Script"), NULL, "Start recording menu clicks into the Scheme script text window",
   G_CALLBACK (toggle_record_script), FALSE},


  {ReadOnly_STRING, NULL, N_("Read Only"), NULL, "Make score read only\nNot working",
   G_CALLBACK (default_mode), FALSE}
};

/**
 * Radio entries for the modes and entry types
 */
static GtkRadioActionEntry mode_menu_entries[] = {
  {MODELESS_STRING, NULL, N_("No mode"), NULL, "Access all editing functions without change of mode",
   0},
  {CLASSICMODE_STRING, NULL, N_("Classic"), NULL, "The original Denemo note entry mode\nUseful for entering notes into chords\nUse the note names to move the cursor\nUse the durations to insert notes",
   INPUTCLASSIC},
  {INSERTMODE_STRING, NULL, N_("Insert"), NULL, N_("Mode for inserting notes into the score at the cursor position\nUses prevailing duration/rhythm\nUse the durations to set the prevailing duration\nUse the note names to insert the note"),
   INPUTINSERT},
  {EDITMODE_STRING, NULL, N_("Edit"), NULL, N_("Mode for changing the note at cursor (name, duration)\nand to enter notes by duration (rhythms)\nUse the durations to insert notes"),
   INPUTEDIT}
};


static GtkRadioActionEntry type_menu_entries[] = {
  {NOTE_E_STRING, NULL, N_("Note"), NULL,  N_("Normal (note) entry"), INPUTNORMAL},
  {REST_E_STRING, NULL, N_("Rest"), NULL,  N_("Entering rests not notes"), INPUTREST},
  {BLANK_E_STRING, NULL, N_("Non printing rests"), NULL,  N_("Enters rests which will not be printed (just take up space)\nUsed for positioning polyphonic voice entries"), INPUTBLANK},
  {RHYTHM_E_STRING, NULL, N_("Rhythm"), NULL, N_("Mode for pure rhythyms"),
   INPUTRHYTHM|INPUTNORMAL}
};

static GtkRadioActionEntry input_menu_entries[] = {
  {"KeyboardOnly", NULL, N_("No External Input"), NULL, N_("Entry of notes via computer keyboard only"),
  INPUTKEYBOARD}
  ,
  {"Microphone", NULL, N_("Audio Input"), NULL, N_("Enable pitch entry from microphone"), INPUTAUDIO
   /*  G_CALLBACK (toggle_pitch_recognition), FALSE*/}
  ,
  {"JackMidi", NULL, N_("Midi Input"), NULL,N_("Input of midi via Jack Audio Connection Kit"), INPUTMIDI/*G_CALLBACK (jackmidi)*/}
};

struct cbdata
{
  DenemoGUI *gui;
  gchar *filename;
};

/**
 * Callback for the history menu
 * opens the selected file
 */
static void
openrecent (GtkWidget * widget, gchar *filename)
{
  DenemoGUI *gui = Denemo.gui;
  if (!gui->notsaved || (gui->notsaved && confirmbox (gui)))
    {
      deletescore(NULL, gui);
      if(open_for_real (filename, gui, FALSE, FALSE))
	{
	  gchar *warning = g_strdup_printf("Load of recently used file %s failed", filename);
	  warningdialog(warning);
	  g_free(warning);
	}
    }
}

 
/**
 * Add history entry to the History menu, create a menu item for it
 */
void
addhistorymenuitem (gchar *filename)
{
  GList *g;
  if(!g_file_test(filename,  G_FILE_TEST_EXISTS))
    return;
  GtkWidget *item =
    gtk_ui_manager_get_widget (Denemo.ui_manager,
			       "/MainMenu/FileMenu/OpenRecent/Stub");
  GtkWidget *menu = gtk_widget_get_parent (GTK_WIDGET (item));
  
  item = gtk_menu_item_new_with_label (filename);
  gtk_menu_shell_insert (GTK_MENU_SHELL (menu), item, 0);
  g_signal_connect (G_OBJECT(item), "activate", G_CALLBACK (openrecent), g_strdup(filename));
  gtk_widget_show (item);
}

/**
 * Top-Level function to populate the History menu
 * with elements read from the denemohistory file
 */
static void
populate_opened_recent (void)
{
  g_queue_foreach (Denemo.prefs.history, (GFunc)addhistorymenuitem, NULL);
}

static 	void show_type(GtkWidget *widget, gchar *message) {
    g_print("%s%s\n",message, widget?g_type_name(G_TYPE_FROM_INSTANCE(widget)):"NULL widget");
  }
/* set all labels in the hierarchy below widget to use markup */
static void use_markup(GtkWidget *widget)
{
  // show_type(widget, "Widget Type: ");

 
  //g_print("container type %x\n", GTK_IS_CONTAINER(widget));
  //g_print("label type %x\n", GTK_IS_LABEL(widget));
  //g_print("menu item type %x\n",GTK_IS_MENU_ITEM(widget));
  //g_print("tool item type %x\n",GTK_IS_TOOL_ITEM(widget));
  //g_print("descended to use markup on %p\n", widget);

  if(GTK_IS_LABEL(widget)) {
   // gtk_label_set_use_underline (GTK_LABEL (widget), FALSE); font_desc gets interpreted in GtkLabel but not GtkAccelLabel hmmm...
    //g_print("Before we have %d\n", gtk_label_get_use_markup        (widget));
    //gchar * label = gtk_label_get_label(widget);
     //g_print("label before is %s\n", label);
    
    gtk_label_set_use_markup (GTK_LABEL (widget), TRUE);
    //g_print("after we have %d\n", gtk_label_get_use_markup        (widget));
    //if(*label=='M')
    //g_print("seting %p", widget),gtk_label_set_markup(widget, "hello"MUSIC_FONT("33")"ok"), show_type(widget, "should be label: "), label = gtk_label_get_label(widget),g_print("label now %s\n",label) ;

  }
  else
 if(GTK_IS_CONTAINER(widget)) {
    GList *g = gtk_container_get_children (GTK_CONTAINER(widget));
    for(;g;g=g->next)
      use_markup(g->data);
    if (GTK_IS_MENU_ITEM(widget)) {
      use_markup(gtk_menu_item_get_submenu(GTK_MENU_ITEM(widget)));
    }
 }
}



/**
 * Key snooper function. This function intercepts all key events before they are
 * passed to other functions for further processing. We use do quick shortcut edits.
 */
static gint dnm_key_snooper(GtkWidget *grab_widget, GdkEventKey *event)
{
    //no special processing for key release events
    if (event->type == GDK_KEY_RELEASE)
        return FALSE;
    //if the grab_widget is a menu, the event could be a quick edit
    if (Denemo.QuickShortcutEdits && GTK_IS_MENU (grab_widget)) {
        return keymap_accel_quick_edit_snooper(grab_widget, event);
    }
    //else we let the event be processed by other functions
    return FALSE;
}

/* 
 * create and populate the keymap - a register of all the Denemo commands with their shortcuts
 */
void init_keymap(void)
{
  if(Denemo.map)
    free_keymap(Denemo.map);
  Denemo.map = allocate_keymap ();
  GtkActionGroup *action_group = Denemo.action_group;
#include "register_commands.h"
} 


static void
switch_page (GtkNotebook *notebook, GtkNotebookPage *page,  guint pagenum) {
  //g_print("switching pagenum %d\n",pagenum);
  DenemoGUI *gui = Denemo.gui;
  if(gui==NULL)
    return;
  GList *g = g_list_nth(Denemo.guis, pagenum);
  if(g==NULL) {
    g_warning("got a switch page, but there is no such page in Denemo.guis\n");
    return;
  }
  DenemoGUI *newgui = g->data;
  if(gui==newgui)
    return;//on arrival Denemo.gui is already set to the new gui when you are doing new window
  /* turn off the LilyPond window if it is on
   it would be nice to keep a record of whether it was open for re-opening
   on return to this tab FIXME*/
  {
    GtkWidget *widget;
    widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/MainMenu/ViewMenu/ToggleLilyText");
    if(gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM (widget)))
      gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (widget), FALSE);
  }
  unhighlight_rhythm(Denemo.gui->prevailing_rhythm);
  Denemo.gui = gui = (DenemoGUI*)(g->data);

  switch(gui->mode & ~MODE_MASK ) {
    case INPUTINSERT:
      activate_action( "/MainMenu/ModeMenu/InsertMode");
      break;
    case INPUTEDIT:
      activate_action( "/MainMenu/ModeMenu/EditMode");
      break;
    case INPUTCLASSIC:
      activate_action( "/MainMenu/ModeMenu/ClassicMode");
      break;
    case 0:
      activate_action( "/MainMenu/ModeMenu/Modeless");
      break;
    default:
      ;
    }

  switch(gui->mode & ~ENTRY_TYPE_MASK ) {
    case INPUTNORMAL:
      activate_action( "/MainMenu/ModeMenu/Note");
      break;
    case INPUTBLANK:
      activate_action( "/MainMenu/ModeMenu/Blank");
      break;
    case INPUTREST:
      activate_action( "/MainMenu/ModeMenu/Rest");
      break;
    case INPUTRHYTHM:
      activate_action( "/MainMenu/ModeMenu/Rhythm");
      break;
    default:
      ;
    }

  highlight_rhythm(Denemo.gui->prevailing_rhythm);

}



/*  proxy_connected
    callback to set callback for right click on menu items and
    set the shortcut label 

*/
static gboolean  thecallback      (GtkWidget      *widget,
                                            GdkEventButton *event,
				   GtkAction *action) {
  if (event->button==1 && !(event->state&(GDK_SHIFT_MASK|GDK_CONTROL_MASK)))
    return FALSE;
  g_print("going for %d for %d\n", event->button, event->state);
  event->button = 3;
  return menu_click(widget, event, action);
}
static void  proxy_connected (GtkUIManager *uimanager, GtkAction *action, GtkWidget    *proxy) {
  int command_idx;

  attach_right_click_callback(proxy, action);

  if(GTK_IS_IMAGE_MENU_ITEM(proxy)) {
    //  ????????????? should I put an icon named for the action->label into an icon factory here (we could just have one, static, and use gtk_icon_factory_add_default??????????
    if(!g_object_get_data(G_OBJECT(action), "connected"))
    g_signal_connect(G_OBJECT(proxy), "button-press-event", G_CALLBACK(thecallback), action);
     g_object_set_data(G_OBJECT(action), "connected", (gpointer)1);  //Unfortunately GtkImageMenuItems that pop up a menu do not wait for a button press - the focus switches to the popped up memory on entry. So we don't see this signal for them
  }
#if (GTK_MINOR_VERSION <10)
       attach_action_to_widget(proxy, action, Denemo.gui);
#endif
  if(Denemo.map==NULL)
     return;
  command_idx = lookup_command_from_name(Denemo.map,
				       gtk_action_get_name(action));

    
  if (command_idx != -1) 
    update_accel_labels(Denemo.map, command_idx);
  //  else //not an error, it occurs for menus being loaded
  //   g_warning("%s is not yet in map\n",  gtk_action_get_name(action));
  gboolean hidden= (gboolean) (action?g_object_get_data(G_OBJECT(action), "hidden"):NULL);
  if(hidden) {
	    set_visibility_for_action(action, FALSE);	   
	  }
}



/*
 create_scheme_window()
 create a text window for scheme scripts

*/

  static void create_scheme_window(void) {
  Denemo.ScriptView = gtk_text_view_new();
  GtkWidget *w = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (w), "Denemo Scheme Script");
  //gtk_window_set_resizable (GTK_WINDOW (w), TRUE);
  g_signal_connect(G_OBJECT(w), "delete-event", G_CALLBACK(hide_scheme/*gtk_widget_hide_on_delete*/), w);
  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (w), main_vbox);
  
  w = gtk_button_new_with_label("Execute Script");
  g_signal_connect(G_OBJECT(w), "clicked",  G_CALLBACK(executeScript), NULL);
  gtk_box_pack_start (GTK_BOX (main_vbox), w, FALSE, TRUE, 0);

  
  GtkWidget *wid = gtk_check_button_new();
  //GtkAction *action2 = gtk_ui_manager_get_action (Denemo.ui_manager, "/MainMenu/EditMenu/KeyBindings/RecordScript");
  //g_print("action %p", action2);
  gtk_action_connect_proxy(gtk_ui_manager_get_action (Denemo.ui_manager, "/MainMenu/ModeMenu/RecordScript"), wid);
  gtk_box_pack_start (GTK_BOX (main_vbox), wid, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (main_vbox), Denemo.ScriptView , FALSE, TRUE, 0);


  }

/* create_window() creates the toplevel window and all the menus - it only
   called once per invocation of Denemo */
static void
create_window(void) {
  DenemoPrefs *prefs;
  GtkWidget *main_vbox, *menubar, *toolbar, *hbox;
  GtkActionGroup *action_group;
  GtkUIManager *ui_manager;
  GtkAccelGroup *accel_group;
  GError *error;
  GtkWidget *widget;
  gchar *data_dir;
  Denemo.window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (Denemo.window), "Denemo Main Window");
  loadWindowState(/* it accesses Denemo.window */);
#ifdef G_OS_WIN32
  data_dir = g_build_filename (get_data_dir (), "icons","denemo.png", NULL);
#else
  data_dir = g_strconcat (get_data_dir (), "/../icons/denemo.png", NULL);//FIXME installed in wrong place
#endif
  gtk_window_set_default_icon_from_file (data_dir, NULL);
  gtk_signal_connect (GTK_OBJECT (Denemo.window), "delete_event",
		      (GtkSignalFunc) delete_callback, NULL);
  g_free (data_dir);

  gtk_window_set_resizable (GTK_WINDOW (Denemo.window), TRUE);

  //create_scheme_window();






  main_vbox = gtk_vbox_new (FALSE, 1);
  gtk_container_border_width (GTK_CONTAINER (main_vbox), 1);
  gtk_container_add (GTK_CONTAINER (Denemo.window), main_vbox);
  gtk_widget_show (main_vbox);

  Denemo.action_group = action_group = gtk_action_group_new ("MenuActions");
  gtk_action_group_set_translation_domain (action_group, NULL); 
  /* This also sets current Denemo.gui as the  callback data for all the functions in the
   * menubar, which is not needed since we have only one set of actions for all
   the guis. We will always act on Denemo.gui anyway.*/
  gtk_action_group_add_actions (action_group, menu_entries,
  			G_N_ELEMENTS (menu_entries),  Denemo.gui);
  gtk_action_group_add_toggle_actions (action_group,
				       toggle_menu_entries,
				       G_N_ELEMENTS (toggle_menu_entries),
				       Denemo.gui);
  gtk_action_group_add_radio_actions (action_group,
				       mode_menu_entries,
				       G_N_ELEMENTS (mode_menu_entries),
				      INPUTINSERT/* initial value */, 
				      G_CALLBACK(change_mode),  Denemo.gui);


  gtk_action_group_add_radio_actions (action_group,
				       type_menu_entries,
				       G_N_ELEMENTS (type_menu_entries),
				      INPUTNORMAL/* initial value */, 
				      G_CALLBACK(change_entry_type),  Denemo.gui);

  gtk_action_group_add_radio_actions (action_group,
				       input_menu_entries,
				       G_N_ELEMENTS (input_menu_entries),
				      INPUTKEYBOARD/* initial value */, 
				      G_CALLBACK(change_input_type),  NULL);




  ui_manager = gtk_ui_manager_new ();
  Denemo.ui_manager = ui_manager;
  gtk_ui_manager_set_add_tearoffs (Denemo.ui_manager, TRUE);
  gtk_ui_manager_insert_action_group (ui_manager, action_group, 0);

  g_signal_connect(G_OBJECT(Denemo.ui_manager), "connect-proxy", G_CALLBACK(proxy_connected), NULL);


  //We do not use accel_group anymore TODO delete the next 2 lines
  //accel_group = gtk_ui_manager_get_accel_group (ui_manager);
  //gtk_window_add_accel_group (GTK_WINDOW (Denemo.window), accel_group);

  /* TODO Lily_menu actions are handled differently for the time being
   * What are these actions?
   */
  GtkActionEntry lily_menus[] = {
    {"LilyToggleShow", NULL, N_("Show/Hide"),NULL, N_("Toggle visibility of section"),G_CALLBACK (toggle_lily_visible_cb)},
    {"LilyCreateCustom", NULL, N_("Create Custom Version"),NULL, N_("Create a custom version of this block"),G_CALLBACK (custom_lily_cb)},
    {"LilyDelete", NULL, N_("Delete Block"),NULL, N_("Delete this block"),G_CALLBACK (delete_lily_cb)}
  };

  data_dir = g_build_filename (
#ifndef USE_LOCAL_DENEMOUI
get_data_dir (),
#endif
 "denemoui.xml", NULL);
  error = NULL;
  if (!gtk_ui_manager_add_ui_from_file (ui_manager, data_dir, &error))
    {
      g_message ("building menu failed: %s", error->message);
      g_error_free (error);
      gchar *message = g_strdup_printf("The denemoui.xml %s file could not be used - exiting", data_dir);
      warningdialog(message);
      exit (EXIT_FAILURE);
    }
  g_free (data_dir);

  //menubar = gtk_item_factory_get_widget (item_factory, "<main>");
  Denemo.menubar = gtk_ui_manager_get_widget (ui_manager, "/MainMenu");// this triggers Lily... missing action
  gtk_box_pack_start (GTK_BOX (main_vbox), Denemo.menubar, FALSE, TRUE, 0);
  gtk_widget_show (Denemo.menubar);

  toolbar = gtk_ui_manager_get_widget (ui_manager, "/ToolBar");
  // The user should be able to decide toolbar style.
  // But without gnome, there is no (ui) to set this option.

  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_BOTH_HORIZ);
  gtk_box_pack_start (GTK_BOX (main_vbox), toolbar, FALSE, TRUE, 0);
  GTK_WIDGET_UNSET_FLAGS(toolbar, GTK_CAN_FOCUS); 
  gtk_widget_show (toolbar);
  toolbar = gtk_ui_manager_get_widget (ui_manager, "/EntryToolBar");
  //g_print("EntryToolbar is %p\n", toolbar);
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_TEXT);
  gtk_box_pack_start (GTK_BOX (main_vbox), toolbar, FALSE, TRUE, 0);
  GTK_WIDGET_UNSET_FLAGS(toolbar, GTK_CAN_FOCUS);

  // gtk_widget_show (toolbar); cannot show this until the GtkLabels have become GtkAccelLabels - a gtk bug

  toolbar = gtk_ui_manager_get_widget (ui_manager, "/RhythmToolBar");
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_TEXT);
  gtk_box_pack_start (GTK_BOX (main_vbox), toolbar, FALSE, TRUE, 0);

  menubar = gtk_ui_manager_get_widget (ui_manager, "/ObjectMenu");
  if(menubar) {
    gtk_box_pack_start (GTK_BOX (main_vbox), menubar, FALSE, TRUE, 0);
  }


  //  menubar = gtk_ui_manager_get_widget (ui_manager, "/ActionMenu");
  //  if(menubar) {
  //    gtk_box_pack_start (GTK_BOX (main_vbox), menubar, FALSE, TRUE, 0);
  //  }

  Denemo.notebook = gtk_notebook_new ();
  gtk_notebook_set_show_tabs (GTK_NOTEBOOK(Denemo.notebook), FALSE);//only show when more than one
  //gtk_notebook_popup_enable (Denemo.notebook);?? doesn't work...
  gtk_widget_show (Denemo.notebook);
  gtk_box_pack_start (GTK_BOX (main_vbox), Denemo.notebook, TRUE, TRUE, 0);


  Denemo.statusbar = gtk_statusbar_new ();
  hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (hbox), Denemo.statusbar, TRUE, TRUE, 5);
  gtk_widget_show (Denemo.statusbar);
  Denemo.status_context_id =
    gtk_statusbar_get_context_id (GTK_STATUSBAR (Denemo.statusbar), "Denemo");
  gtk_statusbar_push (GTK_STATUSBAR (Denemo.statusbar), Denemo.status_context_id,
		      "Denemo");
  Denemo.input_source = gtk_label_new("No external input");
  Denemo.input_filters = NULL;
  gtk_box_pack_end (GTK_BOX (hbox), Denemo.input_source, TRUE, TRUE, 5);
  gtk_widget_show (hbox);

  create_scheme_window();

  populate_opened_recent ();
  gtk_widget_show(Denemo.window);
  /* Now that the window is shown, initialize the gcs */
  gcs_init (Denemo.window->window);
  if (!Denemo.prefs.visible_titles) {
    widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/MainMenu/ViewMenu/"ToggleScoreTitles_STRING);
    g_signal_emit_by_name(widget, "activate", NULL, Denemo.gui);
    Denemo.prefs.visible_titles = !Denemo.prefs.visible_titles; 
  }

#if 1 /* bug #25562 : apparently several people have tried to fix it this way */

   {

     GtkWidget *widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/RhythmToolBar");

     if (GTK_WIDGET_VISIBLE (widget))

       gtk_widget_hide(widget);// I do not understand why this is visible - there is no gtk_widget_show(all) in the hierarchy

     if (Denemo.prefs.rhythm_palette) {

       widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/MainMenu/ViewMenu/ToggleRhythmToolbar");

       g_signal_emit_by_name(widget, "activate", NULL, Denemo.gui);

     }

   }

#endif
#if 0
  /* we have to do this properly, because it introduces a keymap - no longer true */
  if (Denemo.prefs.rhythm_palette) {
    GtkWidget *widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/RhythmToolBar");
    if (GTK_WIDGET_VISIBLE (widget))
      gtk_widget_hide(widget);// I do not understand why this is visible - there is no gtk_widget_show(all) in the hierarchy
    widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/MainMenu/ViewMenu/ToggleRhythmToolbar");
    g_signal_emit_by_name(widget, "activate", NULL, Denemo.gui);
    //g_print("type is %s\n", g_type_name(G_TYPE_FROM_INSTANCE(widget))); 
    }
  // A cheap way of doing activating this toolbar, note it is called variously notation toolbar, duration toolbar and EntryToolBar FIXME
  if (!Denemo.prefs.notation_palette)
    {
      //g_print ("Notation palette %d\n", Denemo.prefs.notation_palette);
      toggle_entry_toolbar (NULL, Denemo.gui);
    }
#endif

  data_dir = g_build_filename (
#ifndef USE_LOCAL_DENEMOUI
get_data_dir (),
#endif
 "denemoui.xml", NULL);
  parse_paths(data_dir, Denemo.gui);


  use_markup(Denemo.window);/* set all the labels to use markup so that we can use the music font. Be aware this means you cannot use labels involving "&" "<" and ">" and so on without escaping them 
FIXME labels in toolitems are not correct until you do NewWindow.
Really we should change the default for the class.*/
  {
  GtkActionGroup *lilyaction_group = gtk_action_group_new ("LilyActions");
  gtk_action_group_set_translation_domain (lilyaction_group, NULL); 
  gtk_action_group_add_actions (lilyaction_group, lily_menus,
				G_N_ELEMENTS (lily_menus), Denemo.gui);
  gtk_ui_manager_insert_action_group (ui_manager, lilyaction_group, 1);
  }
 //  g_print("Turning on the modes\n");


 //write_status(Denemo.gui);
 Denemo.InsertModeMenu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu/NotesRests/InsertModeNote");
 Denemo.EditModeMenu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu/NotesRests/EditModeNote");
 Denemo.ClassicModeMenu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu/NotesRests/ClassicModeNote");
 Denemo.ModelessMenu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu/NotesRests/ModelessNote");

 //gtk_widget_hide (gtk_ui_manager_get_widget (ui_manager, "/ActionMenu"));// make a prefs thing
 gtk_widget_hide (gtk_ui_manager_get_widget (ui_manager, "/EntryToolBar")); //otherwise buttons only sensitive around their edges

#ifdef G_OS_WIN32
 toolbar = gtk_ui_manager_get_widget (ui_manager, "/EntryToolBar");
 gtk_widget_show (toolbar);
#endif

 {GtkToggleAction *action;
 action = (GtkToggleAction *)gtk_ui_manager_get_action (Denemo.ui_manager, "/MainMenu/ViewMenu/ToggleObjectMenu");
 gtk_toggle_action_set_active (action, TRUE);
 toggle_object_menu (NULL, Denemo.gui);
 }
  g_signal_connect (G_OBJECT(Denemo.notebook), "switch_page", G_CALLBACK(switch_page), NULL);
}   /* create window */


static void
newview (GtkAction *action, gpointer param)
{
  newtab(NULL, NULL);
  //should we load init.denemo here as well???
  open_user_default_template(REPLACE_SCORE);
}

/**
 * Creates a new DenemoGUI structure represented by a tab in a notebook to control one musical score
 * of possibly several movements. 
 * This DenemoGUI* gui is appended to the global list Denemo.guis.
 * A single movement (DenemoScore) is instantiated in the gui.
 * 
 */
void
newtab (GtkAction *action, gpointer param) {
#ifdef _HAVE_JACK_
  stop_jack();
#endif
  GtkActionGroup *action_group=Denemo.action_group;
  //  if(Denemo.guis==NULL)
  //    action_group = create_window();
  DenemoGUI *gui = (DenemoGUI *) g_malloc0 (sizeof (DenemoGUI));
  Denemo.guis = g_list_append (Denemo.guis, gui);


  Denemo.gui = NULL;
  // Denemo.gui = gui; must do this after switching to page, so after creating page
  gui->lilycontrol.papersize = g_string_new ("a4");	//A4 default
  gui->lilycontrol.staffsize = g_string_new("18");
  gui->lilycontrol.lilyversion = g_string_new (LILYPOND_VERSION);
  gui->lilycontrol.orientation = TRUE;	//portrait
  // gui->lilycontrol.lilypond = g_string_new ("\\transpose c c");


  gui->pixmap = NULL;

  /* Initialize the GUI */

  //create the tab for this gui
  GtkWidget *top_vbox = gtk_vbox_new (FALSE, 1);
  
  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (top_vbox), main_vbox, TRUE, TRUE,
		      0);
  gint pagenum = gtk_notebook_append_page (GTK_NOTEBOOK (Denemo.notebook), top_vbox, NULL);
  gui->page = gtk_notebook_get_nth_page (GTK_NOTEBOOK(Denemo.notebook), pagenum);
  gtk_notebook_set_current_page (GTK_NOTEBOOK(Denemo.notebook), pagenum);
Denemo.gui = gui;
 if(pagenum)
   gtk_notebook_set_show_tabs (GTK_NOTEBOOK(Denemo.notebook), TRUE);
  set_title_bar(gui);
  gtk_widget_show (top_vbox);
  gtk_widget_show (main_vbox);
  GtkWidget *score_and_scroll_hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_vbox), score_and_scroll_hbox, TRUE, TRUE,
		      0);
  gtk_widget_show (score_and_scroll_hbox);
  //gtk_grab_remove(toolbar);  ?????????
  gui->scorearea = gtk_drawing_area_new ();


  gtk_box_pack_start (GTK_BOX (score_and_scroll_hbox), gui->scorearea, TRUE,
		      TRUE, 0);// with this, the scorearea_expose_event is called
  gtk_widget_show (gui->scorearea);

  gui->vadjustment = gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0);
  gtk_signal_connect (GTK_OBJECT (gui->vadjustment), "value_changed",
		      GTK_SIGNAL_FUNC (vertical_scroll), gui);
  gui->vscrollbar = gtk_vscrollbar_new (GTK_ADJUSTMENT (gui->vadjustment));
  gtk_box_pack_start (GTK_BOX (score_and_scroll_hbox), gui->vscrollbar, FALSE,
		      TRUE, 0);
  gtk_widget_show (gui->vscrollbar);

  gui->hadjustment = gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0);

  gtk_signal_connect (GTK_OBJECT (gui->hadjustment), "value_changed",
		      GTK_SIGNAL_FUNC (horizontal_scroll), gui);
  gui->hscrollbar = gtk_hscrollbar_new (GTK_ADJUSTMENT (gui->hadjustment));
  gtk_box_pack_start (GTK_BOX (main_vbox), gui->hscrollbar, FALSE, TRUE, 0);



  gtk_widget_show (gui->hscrollbar);

#if 0
  GtkWidget *hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);
  gtk_widget_show (hbox);
#endif


  install_printpreview(gui, top_vbox);
 
  //FIXME populate_opened_recent (gui);

  /* create the first movement now because showing the window causes it to try to draw the scorearea
   which it cannot do before there is a score. FIXME use signal blocking to control this - see importxml.c */
  point_to_new_movement (gui);
  gui->movements = g_list_append(NULL, gui->si);
 

  gtk_widget_show (gui->page);
  gtk_widget_grab_focus (gui->scorearea);



 create_rhythm_cb((gpointer)insert_chord_0key, NULL); 
 create_rhythm_cb((gpointer)insert_chord_1key, NULL);   
 create_rhythm_cb((gpointer)insert_chord_2key, NULL);   
 create_rhythm_cb((gpointer)insert_chord_3key, NULL);   
 create_rhythm_cb((gpointer)insert_chord_4key, NULL);   
 create_rhythm_cb((gpointer)insert_chord_5key, NULL); 
 create_rhythm_cb((gpointer)insert_chord_6key, NULL);   


 create_rhythm_cb((gpointer)insert_rest_0key, NULL); 
 create_rhythm_cb((gpointer)insert_rest_1key, NULL);   
 create_rhythm_cb((gpointer)insert_rest_2key, NULL);   
 create_rhythm_cb((gpointer)insert_rest_3key, NULL);   
 create_rhythm_cb((gpointer)insert_rest_4key, NULL);   
 create_rhythm_cb((gpointer)insert_rest_5key, NULL); 
 create_rhythm_cb((gpointer)insert_rest_6key, NULL);   


  if (Denemo.prefs.articulation_palette)
    toggle_articulation_palette (NULL, NULL);
  Denemo.gui->mode = INPUTINSERT | INPUTNORMAL;

  // this stops the keyboard input from getting to  scorearea_keypress_event if done after attaching the signal, why?
  gtk_notebook_set_current_page (GTK_NOTEBOOK(Denemo.notebook), pagenum);


  GTK_WIDGET_SET_FLAGS(gui->scorearea, GTK_CAN_FOCUS);
  gtk_widget_grab_focus (GTK_WIDGET(gui->scorearea));
  g_signal_connect (G_OBJECT (gui->scorearea), "expose_event",
		      G_CALLBACK (scorearea_expose_event), NULL);
  g_signal_connect (G_OBJECT (gui->scorearea), "configure_event",
		      G_CALLBACK (scorearea_configure_event), gui);


  g_signal_connect (G_OBJECT (gui->scorearea), "button_release_event",
		      G_CALLBACK (scorearea_button_release), NULL);

  g_signal_connect (G_OBJECT (gui->scorearea), "motion_notify_event",
		      G_CALLBACK (scorearea_motion_notify), NULL);

  //g_signal_handlers_block_by_func(gui->scorearea, G_CALLBACK (scorearea_motion_notify), gui);
  g_signal_connect (G_OBJECT (gui->scorearea), "button_press_event",
		      G_CALLBACK (scorearea_button_press), NULL);
  //  gtk_signal_connect (GTK_OBJECT (gui->page), "delete_event",
  //		      (GtkSignalFunc) delete_callback, gui);
  gtk_signal_connect (GTK_OBJECT (gui->scorearea), "key_press_event",
		      (GtkSignalFunc) scorearea_keypress_event, gui);

  gtk_signal_connect (GTK_OBJECT (gui->scorearea), "key_release_event",
		      (GtkSignalFunc) scorearea_keyrelease_event, gui);

  gtk_widget_add_events/*gtk_widget_set_events*/ (gui->scorearea, (GDK_EXPOSURE_MASK
					  | GDK_POINTER_MOTION_MASK
					  | GDK_LEAVE_NOTIFY_MASK
					  | GDK_BUTTON_PRESS_MASK
					  | GDK_BUTTON_RELEASE_MASK));

 if (Denemo.prefs.autosave) {
   if(Denemo.autosaveid) {
     g_print("No autosave on new tab");
   }
   else {
     Denemo.autosaveid = g_timeout_add (Denemo.prefs.autosave_timeout * 1000 * 60,
					(GSourceFunc) auto_save_document_timeout, Denemo.gui);
   }
 }

}


