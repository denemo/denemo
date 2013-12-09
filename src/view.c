/* view.c
 * Functions to create a top level Denemo window
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2003-2005  Adam Tee (c) 2007, 2008 2009 Richard Shann
 * 
 */

#include <string.h>
#include <math.h>
#include "view.h"
#include "bookmarks.h"

#include "lilydirectives.h"
#include "dialogs.h"
#include "utils.h"
#include <stdlib.h>
#include <glib/gstdio.h>
#include <cairo.h>
#include <cairo-svg.h>
#include <librsvg/rsvg.h>

#include "playback.h"
#include "pitchentry.h"
#include "exportlilypond.h"
#include "print.h"
#include "printview.h"
#include "graceops.h"
#include "kbd-custom.h"
#include "keyboard.h"
#include "exportmidi.h"
#include "midi.h"
#ifdef _WITH_X11_
#include "screenshot.h"
#endif
#include "source.h"
#include "commandfuncs.h"
#include "calculatepositions.h"
#include "http.h"
#include "texteditors.h"
#include "prefops.h"
#include "audiointerface.h"
#include "sourceaudio.h"
#include "scorelayout.h"
#include "keymapio.h"
#include "measureops.h"
#include "audiofile.h"
#include "guidedimportmidi.h"
#include "scheme-identifiers.h"
#include "scheme-callbacks.h"

static GtkWidget *playbutton;
static GtkWidget *midirecordbutton;
static GtkWidget *audiorecordbutton;
static GtkWidget *midi_in_status;
static GtkWidget *midiplayalongbutton;
static GtkWidget *midiconductbutton;
static GtkWidget *deletebutton;
static GtkWidget *exportbutton;
static GtkWidget *convertbutton;
static GtkSpinButton *leadin;
static GtkAdjustment *master_vol_adj;
static GtkAdjustment *audio_vol_adj;
static GtkAdjustment *master_tempo_adj;
#ifdef _HAVE_RUBBERBAND_
static GtkAdjustment *speed_adj;
#endif
static void pb_audiorecord (GtkWidget * button);
static void pb_exportaudio (GtkWidget * button);

static DenemoProject* new_project();
static void newtab ();

static void create_window (void);

static gint dnm_key_snooper (GtkWidget * grab_widget, GdkEventKey * event);
static void populate_opened_recent_menu (void);
static gchar *get_most_recent_file (void);
static void toggle_record_script (GtkAction * action, gpointer param);
static void new_movement ();

typedef enum
{
  ACCELS_LOADED = 0x0,
  ACCELS_CHANGED = 0x1 << 0,
  EXTRA_ACCELS_ACTIVE = 0x1 << 1,
  ACCELS_MAY_HAVE_CHANGED = 0x1 << 2
} AccelStatus;

GtkWidget*
get_playalong_button(){
  return midiplayalongbutton;
}

GtkWidget*
get_conduct_button(){
  return midiconductbutton;
}

GtkWidget*
get_record_button(){
  return midirecordbutton;
}

static void save_accels (void);

static gint scm_eval_status = 0;

static SCM
standard_handler (gchar * data SCM_UNUSED, SCM key, SCM parameter SCM_UNUSED)
{
  SCM key_str = scm_symbol_to_string(key);
  SCM function = scm_car(parameter);
  SCM error_message = scm_cadr(parameter);
  SCM args = scm_caddr(parameter);

  SCM args_str = 
    !scm_is_null(args) 
      ? (scm_is_symbol(scm_car(args)) 
         ? scm_symbol_to_string(scm_car(args)) 
         : scm_string_concatenate (args)) 
      : SCM_BOOL_F;

  gchar* msg_intro = g_strdup_printf("%s", scm_to_locale_string(key_str));
  gchar* msg_location = data ? g_strdup_printf(" in '%s'", data) : NULL;
  gchar* msg_function = scm_is_string (function) ? g_strdup_printf("%s: ", scm_to_locale_string(function)) : NULL;
  gchar* msg_message = scm_is_string (error_message) ? g_strdup_printf("%s", scm_to_locale_string(error_message)) : NULL;
  gchar* msg_args = scm_is_string (args_str) ? g_strdup_printf(" (%s)", scm_to_locale_string(args_str)) : NULL;

  gchar* message = g_strdup_printf("%s%s\n%s%s%s",
                                   msg_intro,
                                   msg_location ? msg_location : "",
                                   msg_function ? msg_function : "",
                                   msg_message ? msg_message : "",
                                   msg_args ? msg_args : "");
  if(!Denemo.fatal_scheme_errors)
    g_log("Scheme", G_LOG_LEVEL_CRITICAL, message);
  else
    g_log("Scheme", G_LOG_LEVEL_ERROR , message);
  
  g_free(msg_intro);
  g_free(msg_location);
  g_free(msg_function);
  g_free(msg_message);
  g_free(msg_args);
  g_free(message);
  
  scm_eval_status = -1;
  
  //stage_undo(Denemo.project->si, ACTION_SCRIPT_ERROR); We don't need this as control will return to activate_script() which will terminate the undo properly, with anything the script has done on the undo stack.
  return SCM_BOOL_F;
}

static SCM
standard_preunwind_proc (void *data,
                       SCM key,
                       SCM parameters)
{
  // Capture the stack here:
  *(SCM *)data = scm_make_stack (SCM_BOOL_T, SCM_EOL);
}

gint
eval_file_with_catch (gchar * filename)
{
  // scm_c_primitive_load(filename);
  SCM captured_stack = SCM_BOOL_F;
  SCM name = scm_from_locale_string (filename);
  scm_eval_status = 0;
  scm_c_catch (SCM_BOOL_T, 
              (scm_t_catch_body) scm_primitive_load, (void *) name, 
              (scm_t_catch_handler) standard_handler, (void *) filename,
              standard_preunwind_proc, &captured_stack);
  if (captured_stack != SCM_BOOL_F)
  {
#ifdef DEBUG
    scm_display_backtrace(captured_stack, scm_current_error_port (), SCM_BOOL_F, SCM_BOOL_F);
#endif
  }
  return scm_eval_status;
}

gint
call_out_to_guile (const char *script)
{
  SCM captured_stack = SCM_BOOL_F;
  scm_eval_status = 0;
  scm_c_catch (SCM_BOOL_T, 
              (scm_t_catch_body) scm_c_eval_string, (void *) script, 
              (scm_t_catch_handler) standard_handler, (void *) script,
              standard_preunwind_proc, &captured_stack);
  if (captured_stack != SCM_BOOL_F)
  {
#ifdef DEBUG    
    scm_display_backtrace(captured_stack, scm_current_error_port (), SCM_BOOL_F, SCM_BOOL_F);
#endif
  }
  return scm_eval_status;
}

//FIXME common up these!!!
void
define_scheme_variable (gchar * varname, gchar * value, gchar * tooltip)
{

  gchar *def = g_strdup_printf ("\"%s\"", value);
  //g_debug("Defining %s\n", def);
  scm_c_define (varname, scm_from_locale_string (def));
  g_free (def);
}

void
define_scheme_literal_variable (gchar * varname, gchar * value, gchar * tooltip)
{
  scm_c_define (varname, scm_from_locale_string (value));
}

void
define_scheme_int_variable (gchar * varname, gint value, gchar * tooltip)
{
  scm_c_define (varname, scm_from_int (value));
}

void
define_scheme_double_variable (gchar * varname, gdouble value, gchar * tooltip)
{
  scm_c_define (varname, scm_from_double (value));
}




void
define_scheme_bool_variable (gchar * varname, gint value, gchar * tooltip)
{
  scm_c_define (varname, SCM_BOOL (value));
}


GError *
execute_script_file (gchar * filename)
{
  GError *error = NULL;
  gchar *script;
  if (g_file_get_contents (filename, &script, NULL, &error))
    {
      call_out_to_guile (script);       //FIXME setup error here if non null return
      g_free (script);
    }
  return error;
}


void
execute_scheme (GtkAction * action, DenemoScriptParam * param)
{
  if (Denemo.ScriptRecording)
    gtk_action_activate (gtk_action_group_get_action (Denemo.action_group, RecordScript_STRING));
  //Denemo.ScriptRecording = FALSE;
  executeScript ();
}



#define FN_DEF(X) void X##_CB(void) {\
activate_action("/MainMenu/ModeMenu/"X##_STRING);}

FN_DEF (MODELESS);
FN_DEF (CLASSICMODE);
FN_DEF (INSERTMODE);
FN_DEF (EDITMODE);
FN_DEF (NOTE_E);
FN_DEF (REST_E);
FN_DEF (BLANK_E);
FN_DEF (RHYTHM_E);

typedef struct cb_string_pairs
{
  gpointer p;
  gchar *str;
} cb_string_pairs;
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

//returns newly allocated string. FIXME use proper scm_xxx calls not strings
gchar *
get_midi_control_command (guchar type, guchar value)
{
  gchar *command = g_strdup_printf ("(MIDI-shortcut::controller %d %d)", type, value);
  SCM scm = scm_c_eval_string (command);
  g_free (command);
  if (scm_is_string (scm))
    {
      char *ctrl = scm_to_locale_string (scm);
      command = g_strdup (ctrl);        //FIXME
      free (ctrl);
      return command;
    }
  return NULL;
}

//returns newly allocated string. FIXME use proper scm_xxx calls not strings
gchar *
get_midi_pitch_bend_command (gint value)
{
  gchar *command = g_strdup_printf ("(MIDI-shortcut::pitchbend %d)", value);
  SCM scm = scm_c_eval_string (command);
  g_free (command);
  if (scm_is_string (scm))
    {
      char *pbend;
      pbend = scm_to_locale_string (scm);
      command = g_strdup (pbend);       //FIXME
      free (pbend);
      return command;
    }
  return NULL;
}


static void
define_scheme_constants (void)
{
  gint major = 0, minor = 0, micro = 0;
  sscanf (VERSION, "%d.%d.%d", &major, &minor, &micro);
  gchar *denemo_version = g_strdup_printf ("%d.%d.%d%s", major, minor, micro,
#ifdef G_OS_WIN32
                                           "_Win"
#else
                                           ""
#endif
    );
  gchar *filename = g_build_filename (get_system_data_dir (), COMMANDS_DIR, NULL);
  gchar *actions_dir = g_strdup_printf ("%s%c", filename, G_DIR_SEPARATOR);
  if (filename)
    g_free (filename);

  filename = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, NULL);
  gchar *local_actions_dir = g_strdup_printf ("%s%c", filename, G_DIR_SEPARATOR);
  if (filename)
    g_free (filename);

  g_message ("Denemo version %s", denemo_version);

#define DEF_SCHEME_STR(which, what, tooltip)\
  scm_c_define(which, scm_from_locale_string(what));

#define DEF_SCHEME_CONST(which, what)\
  define_scheme_int_variable(which, what, "See documentation elsewhere");

  DEF_SCHEME_CONST ("DENEMO_INPUTMIDI", INPUTMIDI);
  DEF_SCHEME_CONST ("DENEMO_INPUTKEYBOARD", INPUTKEYBOARD);
  DEF_SCHEME_CONST ("DENEMO_INPUTAUDIO", INPUTAUDIO);


  
  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_LILYPOND", DENEMO_OVERRIDE_LILYPOND);
  DEF_SCHEME_CONST ("DENEMO_ALT_OVERRIDE", DENEMO_ALT_OVERRIDE);
  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_GRAPHIC", DENEMO_OVERRIDE_GRAPHIC);
  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_EDITOR", DENEMO_OVERRIDE_EDITOR);
  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_AFFIX", DENEMO_OVERRIDE_AFFIX);
  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_TAGEDIT", DENEMO_OVERRIDE_TAGEDIT);

  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_VOLUME", DENEMO_OVERRIDE_VOLUME);
  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_DURATION", DENEMO_OVERRIDE_DURATION);
  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_REPEAT", DENEMO_OVERRIDE_REPEAT);
  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_CHANNEL", DENEMO_OVERRIDE_CHANNEL);
  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_TEMPO", DENEMO_OVERRIDE_TEMPO);
  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_TRANSPOSITION", DENEMO_OVERRIDE_TRANSPOSITION);

  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_ONCE", DENEMO_OVERRIDE_ONCE);
  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_STEP", DENEMO_OVERRIDE_STEP);
  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_RAMP", DENEMO_OVERRIDE_RAMP);


  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_RELATIVE", DENEMO_OVERRIDE_RELATIVE);
  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_PERCENT", DENEMO_OVERRIDE_PERCENT);

  DEF_SCHEME_CONST ("DENEMO_MIDI_MASK", DENEMO_MIDI_MASK);
  DEF_SCHEME_CONST ("DENEMO_MIDI_INTERPRETATION_MASK", DENEMO_MIDI_INTERPRETATION_MASK);
  DEF_SCHEME_CONST ("DENEMO_MIDI_ACTION_MASK", DENEMO_MIDI_ACTION_MASK);

  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_DYNAMIC", DENEMO_OVERRIDE_DYNAMIC);
  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_HIDDEN", DENEMO_OVERRIDE_HIDDEN);
  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_MARKUP", DENEMO_OVERRIDE_MARKUP);
  DEF_SCHEME_CONST ("DENEMO_OVERRIDE_ABOVE", DENEMO_OVERRIDE_ABOVE);

  DEF_SCHEME_CONST ("VERSION_MAJOR", major);
  DEF_SCHEME_CONST ("VERSION_MINOR", minor);
  DEF_SCHEME_CONST ("VERSION_MICRO", micro);

  DEF_SCHEME_STR ("DENEMO_VERSION", denemo_version, "Holds the denemo version major.minor.micro");
  DEF_SCHEME_STR ("DENEMO_ACTIONS_DIR", actions_dir, "Holds location of system-wide Denemo actions directory");
  DEF_SCHEME_STR ("DENEMO_LILYPOND_DIR", g_build_filename (actions_dir, "lilypond", NULL), "Holds location of Denemo's system-wide  lilypond include files directory");
  DEF_SCHEME_STR ("DENEMO_LOCAL_ACTIONS_DIR", local_actions_dir, "Holds location of Denemo actions directory beneath your home directory");
  DEF_SCHEME_STR ("DENEMO_LOCAL_LILYPOND_DIR", g_build_filename (local_actions_dir, "lilypond", NULL), "Holds location of user lilypond include files directory");
  {
    gint i;
    for (i = 0; i < G_N_ELEMENTS (DenemoObjTypeNames); i++)
      DEF_SCHEME_CONST (DenemoObjTypeNames[i], i);
  }


#undef DEF_SCHEME_STR
#undef DEF_SCHEME_CONST
  g_free (denemo_version);
  g_free (actions_dir);
  g_free (local_actions_dir);
}

/*
  load denemo.scm from user's .denemo 
*/
static void
load_local_scheme_init (void)
{
  gchar *filename = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, SCHEME_INIT, NULL);
  if (g_file_test (filename, G_FILE_TEST_EXISTS))
    eval_file_with_catch (filename);    //scm_c_primitive_load(filename);
  if (filename)
    g_free (filename);
}

void
denemo_scheme_init (void)
{
  gchar *initscheme = Denemo.scheme_file;
  if(!Denemo.non_interactive)
    Denemo.project->si->undo_guard++;

  if (initscheme)
    {
      if (g_file_test (initscheme, G_FILE_TEST_EXISTS))
        eval_file_with_catch (initscheme);      //scm_c_primitive_load(initscheme);
      else
        g_warning ("Cannot find your scheme initialization file %s", initscheme);
    }

  if (Denemo.prefs.profile->len)
    {
      gchar *name = g_strconcat (Denemo.prefs.profile->str, ".scm", NULL);
      gchar *filename = g_build_filename (get_system_data_dir (), COMMANDS_DIR, name, NULL);
      if (g_file_test (filename, G_FILE_TEST_EXISTS))
        eval_file_with_catch (filename);
      g_free (name);
      g_free (filename);
    }

  load_local_scheme_init ();
  if(!Denemo.non_interactive)
    Denemo.project->si->undo_guard--;
}

/*
  append scheme to user's denemo.scm
*/
void
append_to_local_scheme_init (gchar * scheme)
{
  gchar *filename = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, SCHEME_INIT, NULL);
  FILE *fp = fopen (filename, "a+");
  if (fp)
    fprintf (fp, "%s", scheme);
  fclose (fp);
  g_free (filename);
}

/*
  empty the user's user's denemo.scm
*/
void
destroy_local_scheme_init (void)
{
  gchar *filename = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, SCHEME_INIT, NULL);
  FILE *fp = fopen (filename, "w");
  if (fp)
  fclose (fp);
}


/*
  load denemo.scm from system,

*/
static void
load_scheme_init (void)
{
  //Denemo.project->si->undo_guard++;
  gchar* dirs[] = {
    g_build_filename (PACKAGE_SOURCE_DIR, COMMANDS_DIR, NULL),
    g_build_filename (get_system_data_dir (), COMMANDS_DIR, NULL),
    NULL
  };
  gchar *filename = find_path_for_file("denemo.scm", dirs);
  g_debug ("System wide denemo.scm %s\n", filename);
  if (g_file_test (filename, G_FILE_TEST_EXISTS))
    eval_file_with_catch (filename);    //scm_c_primitive_load(filename);
  else
    g_warning ("Cannot find Denemo's scheme initialization file denemo.scm");
  g_free (filename);

  //Denemo.project->si->undo_guard--;
}

/* show the user's preferred view. Assumes all hidden on entry */
void
load_preferences (void)
{
    switch (Denemo.prefs.mode & ~MODE_MASK)
    {
    case INPUTINSERT:
      activate_action ("/MainMenu/ModeMenu/InsertMode");
      break;
    case INPUTEDIT:
      activate_action ("/MainMenu/ModeMenu/EditMode");
      break;
    case INPUTCLASSIC:
      activate_action ("/MainMenu/ModeMenu/ClassicMode");
      break;
    case 0:
      activate_action ("/MainMenu/ModeMenu/Modeless");
      break;
    default:
      activate_action ("/MainMenu/ModeMenu/Modeless");
      break;
    }

  switch (Denemo.prefs.mode & ~ENTRY_TYPE_MASK)
    {
    case INPUTNORMAL:
      activate_action ("/MainMenu/ModeMenu/Note");
      break;
    case INPUTBLANK:
      activate_action ("/MainMenu/ModeMenu/Blank");
      break;
    case INPUTREST:
      activate_action ("/MainMenu/ModeMenu/Rest");
      break;
    default:
      break;
    }

  switch (Denemo.prefs.mode & ~ENTRY_FEEDBACK_MASK)
    {
    case INPUTRHYTHM:
      activate_action ("/MainMenu/ModeMenu/Rhythm");
      break;
    default:
      break;
    }

  Denemo.project->mode = Denemo.prefs.mode;
  // if (Denemo.prefs.startmidiin)
  // activate_action("/MainMenu/InputMenu/JackMidi");
  // if(!have_midi())
  //  activate_action("/MainMenu/InputMenu/KeyboardOnly");
  
  if (!Denemo.prefs.playback_controls)
    activate_action ("/MainMenu/ViewMenu/" TogglePlaybackControls_STRING);
  if (!Denemo.prefs.midi_in_controls)
    activate_action ("/MainMenu/ViewMenu/" ToggleMidiInControls_STRING);

  if (!Denemo.prefs.quickshortcuts)
    activate_action ("/MainMenu/EditMenu/Preferences/Keybindings/" QuickEdits_STRING);

  if (!Denemo.prefs.toolbar)
    activate_action ("/MainMenu/ViewMenu/" ToggleToolbar_STRING);

  if (!Denemo.prefs.lyrics_pane)
    activate_action ("/MainMenu/ViewMenu/" ToggleLyricsView_STRING);

  if (!Denemo.prefs.rhythm_palette)
    activate_action ("/MainMenu/ViewMenu/" ToggleRhythmToolbar_STRING);

  if (!Denemo.prefs.manualtypeset)
    activate_action ("/MainMenu/ViewMenu/" TogglePrintView_STRING);

  if (!Denemo.prefs.object_palette)
    activate_action ("/MainMenu/ViewMenu/" ToggleObjectMenu_STRING);

  if (!Denemo.prefs.visible_directive_buttons)
    activate_action ("/MainMenu/ViewMenu/" ToggleScoreTitles_STRING);

  if (!Denemo.prefs.modal)
    gtk_widget_hide (gtk_ui_manager_get_widget (Denemo.ui_manager, "/MainMenu/ModeMenu"));

  //these menu ones are visible on entry - FIXME is this the array of toolbars below, ending in TRUE?
  if (!Denemo.prefs.playback_controls)
    toggle_playback_controls (NULL, NULL);

  if (!Denemo.prefs.midi_in_controls)
    toggle_midi_in_controls (NULL, NULL);

  if (!Denemo.prefs.toolbar)
    toggle_toolbar (NULL, NULL);

  if (Denemo.prefs.cursor_highlight)
    {
      Denemo.prefs.cursor_highlight = FALSE;
      scheme_highlight_cursor (SCM_BOOL_T);
    }
}

/* 
 * create and populate the keymap - a register of all the Denemo commands with their shortcuts
 */
static void
init_keymap (void)
{
  if (Denemo.map)
    free_keymap (Denemo.map);
  Denemo.map = allocate_keymap ();
#include "generated/register_commands.h"
}

static gboolean
load_files(gchar** files)
{
  gboolean ret = FALSE;
  gint i = 0;
  
  if(!files){
    if(!Denemo.non_interactive)
      newtab ();
    else{
      Denemo.project = new_project ();
      new_movement();
    }
    open_for_real (get_most_recent_file (), Denemo.project, FALSE, REPLACE_SCORE);
    return TRUE;
  }

  for(i=0; files[i]; i++)
    {
      if(!Denemo.non_interactive)
        newtab ();
      else{
        Denemo.project = new_project ();
        new_movement();
      }
      open_for_real (files[i], Denemo.project, FALSE, REPLACE_SCORE);
      ret = TRUE;
    }
  return ret;
}

static void
crash_recovery_check()
{
  gchar *crash_file = g_build_filename (get_user_data_dir (TRUE), "crashrecovery.denemo", NULL);
  if (g_file_test (crash_file, G_FILE_TEST_EXISTS))
    {
      GtkWidget *dialog = gtk_dialog_new_with_buttons (NULL,
                                                       NULL,
                                                       GTK_DIALOG_DESTROY_WITH_PARENT,
                                                       GTK_STOCK_YES,
                                                       GTK_RESPONSE_ACCEPT,
                                                       GTK_STOCK_DELETE,
                                                       GTK_RESPONSE_REJECT,
                                                       NULL);
      GtkWidget *label = gtk_label_new ("Denemo crashed, The open file has been recovered\n" "do you want to continue editing your work?");

      GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
      gtk_container_add (GTK_CONTAINER (content_area), label);

      gtk_widget_show_all (dialog);
      gint result = gtk_dialog_run (GTK_DIALOG (dialog));
      g_debug ("Dialog result is %d\n", result);

      switch (result)
        {
        case GTK_RESPONSE_ACCEPT:
          open_for_real (crash_file, Denemo.project, TRUE, REPLACE_SCORE);
          score_status (Denemo.project, TRUE);
          g_remove (crash_file);
          break;
        case GTK_RESPONSE_CANCEL:
          break;
        case GTK_RESPONSE_REJECT:
          g_remove (crash_file);
          break;
        }
      gtk_widget_destroy (dialog);
    }
}


/* Called from main for scheme initialization reasons.
   calls back to finish command line processing
*/
void*
inner_main (void *files)
{
#if 0
  //disabled pending appearance of pathconfig.h 
  /* initialize guile core */
  {
    SCM load_path;
    char *user_path;

    /* we assume a normal guile with %load-path always be present */
    load_path = scm_c_lookup ("%load-path");

    scm_variable_set_x (load_path, scm_cons (scm_from_locale_string (DENEMO_LOAD_PATH), scm_variable_ref (load_path)));

    /* consider user-specified path extension */
    user_path = getenv ("DENEMO_LOAD_PATH");
    if (user_path)
      {
        scm_variable_set_x (load_path, scm_cons (scm_from_locale_string (user_path), scm_variable_ref (load_path)));
      }
  }
#endif

  initprefs ();
  
  //project Initializations
  if(!Denemo.non_interactive)
  {
    initialize_keystroke_help ();

    if (audio_initialize (&Denemo.prefs))
        g_error ("Failed to initialize audio or MIDI backends");

    create_window ();
    installPalettes ();

    if (Denemo.prefs.tooltip_timeout)
      {
        g_object_set (gtk_widget_get_settings (Denemo.window), "gtk-tooltip-timeout", Denemo.prefs.tooltip_timeout, NULL);
        g_object_set (gtk_widget_get_settings (Denemo.window), "gtk-tooltip-browse-timeout", Denemo.prefs.tooltip_browse_timeout, NULL);
        g_object_set (gtk_widget_get_settings (Denemo.window), "gtk-tooltip-browse-mode-timeout", Denemo.prefs.tooltip_browse_mode_timeout, NULL);
      }

    //ignore setting of mode unless user has explicitly asked for modal use
    if (!Denemo.prefs.modal)
      Denemo.prefs.mode = INPUTEDIT | INPUTRHYTHM | INPUTNORMAL;  //FIXME must correspond with default in prefops.c
    
    Denemo.accelerator_status = FALSE;
  }

  //Scheme initializations
  {
      const char prog[] =
    "(catch #t (lambda () (setlocale LC_ALL \"\")) (lambda _(display \"Locale not supported by the C library. Falling back to default \\\"C\\\" locale.\\n\"(current-error-port))))";
      scm_c_eval_string (prog);
    //scm_setlocale( scm_variable_ref(scm_c_lookup("LC_ALL")), scm_from_locale_string("") );
    create_scheme_identfiers ();

    if (Denemo.prefs.autoupdate)
      fetchcommands (NULL, NULL);
    
    gint i;
    /* create scheme identifiers for check/radio item to activate the items (ie not just run the callback) */
    for (i = 0; i < G_N_ELEMENTS (activatable_commands); i++)
      {
        gchar* function = g_strdup_printf (DENEMO_SCHEME_PREFIX "%s", activatable_commands[i].str);
        install_scm_function (0, NULL, function, activatable_commands[i].p);
        g_free(function);
      }

    //ensure (use-modules (ice-9 optargs)) is loaded first #:optional params
    call_out_to_guile ("(use-modules (ice-9 optargs))");
    init_keymap ();

    define_scheme_constants ();
  
    load_default_keymap_file ();

    load_scheme_init ();

    readHistory ();
    
    gboolean file_loaded = load_files(files);

    if (!file_loaded && !Denemo.scheme_commands)
      {
        gchar* code = g_strdup_printf("(d-InstrumentName \"%s\")", _("Unnamed"));
        call_out_to_guile (code);
        g_free(code);
        denemo_scheme_init ();
      }
  }

  //project related initializations
  if(!Denemo.non_interactive)  
  {
    populate_opened_recent_menu ();
    
    load_preferences ();
    
    gtk_key_snooper_install ((GtkKeySnoopFunc) dnm_key_snooper, NULL);
    
    crash_recovery_check();
    
    score_status (Denemo.project, FALSE);
    
    if (Denemo.prefs.fontspec->len)
      {
        GtkSettings *settings = gtk_settings_get_default ();
        gtk_settings_set_string_property (settings, "gtk-font-name", Denemo.prefs.fontspec->str, "denemo");
      }

    gtk_main ();
  }

  if (Denemo.scheme_commands){
    g_debug("Executing '%s'", Denemo.scheme_commands);
    call_out_to_guile (Denemo.scheme_commands);
  }
  
  return NULL;
}


static void
selection_received (GtkClipboard * clipboard, const gchar * text, DenemoScriptParam * param)
{
  if (!text)
    {
      warningdialog (_("No selection text available"));
      param->status = FALSE;
      return;
    }
  param->string = g_string_new (text);
  param->status = TRUE;
  gtk_main_quit ();
}

/* get the X selection into the param->string */

void
get_clipboard (GtkAction * action, DenemoScriptParam * param)
{
  GtkClipboard *clipboard = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
  gtk_clipboard_request_text (clipboard, (GtkClipboardTextReceivedFunc) selection_received, param);
  gtk_main ();
}



GString *
get_widget_path (GtkWidget * widget)
{
  const gchar *name;
  GString *str = g_string_new ("/");
  for (widget = gtk_widget_get_parent (widget); widget; widget = gtk_widget_get_parent (widget))
    {
      name = gtk_widget_get_name (widget);
      g_string_prepend (str, name);
      g_string_prepend_c (str, '/');
    }
  g_debug ("String is %s\n", str->str);
  return str;
}

static gboolean
action_callbacks (DenemoProject * project)
{
  GList *callbacks = project->callbacks;
  if (callbacks == NULL)
    return FALSE;
  project->callbacks = NULL;        //do this before calling the callbacks, so they cannot run twice
  for (; callbacks; callbacks = g_list_delete_link (callbacks, callbacks))
    {
      call_out_to_guile (callbacks->data);
      g_free (callbacks->data);
    }
  return TRUE;
}



/**
 * Close the current musical score (Denemo.project) freeing all its movements (DenemoMovement), releasing its memory and removing it from the global list Denemo.projects
 * Do not close the sequencer
 */
static gboolean
close_project (void)
{
  if(Denemo.scorearea)
    g_signal_handlers_block_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_draw_event), NULL);       // turn of refresh of display before destroying the data
  stop_midi_playback (NULL, NULL);      // if you do not do this, there is a timer moving the score on which will hang
  //FIXME why was this here??? activate_action("/MainMenu/InputMenu/KeyboardOnly");
#ifdef USE_EVINCE  
  if (Denemo.prefs.enable_thumbnails)
    create_thumbnail (TRUE);
#endif
  if (Denemo.autosaveid)
    {
      if (g_list_length (Denemo.projects) > 1)
        g_debug ("Auto save being turned off");
      g_source_remove (Denemo.autosaveid);
      Denemo.autosaveid = 0;
    }
  if (Denemo.textwindow && gtk_widget_get_visible (Denemo.textwindow))
    {
      activate_action ("/MainMenu/ViewMenu/" ToggleLilyText_STRING);
      //FIXME there is a handler in exportlilypond.c for the delete signal. It would need to be disabled to get the memory freed.
    }
  free_movements (Denemo.project);

  DenemoProject *oldproject = Denemo.project;
  //gtk_widget_destroy (Denemo.page);  //note switch_page from g_signal_connect (G_OBJECT(Denemo.notebook), "switch_page", G_CALLBACK(switch_page), NULL);
  gint index = g_list_index (Denemo.projects, oldproject);
  if(!Denemo.non_interactive)
    gtk_notebook_remove_page (GTK_NOTEBOOK (Denemo.notebook), index);
  g_message ("Closing project %d", index);
  Denemo.projects = g_list_remove (Denemo.projects, oldproject);    //FIXME ?? or in the destroy callback??
  g_free (oldproject);
  if (Denemo.projects)
    {
      if (index > g_list_length (Denemo.projects) - 1)
        index = g_list_length (Denemo.projects) - 1;
      if (index < 0)
        index = 0;

      Denemo.project = g_list_nth_data (Denemo.projects, index);
      //g_debug("Setting the first piece as your score\n");
      gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), index);
    }
  else
    Denemo.project = NULL;
  if(Denemo.scorearea)
    g_signal_handlers_unblock_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_draw_event), NULL);
  return TRUE;
}

/* remove all the movements (ie the DenemoMovement) leaving it with project->si NULL */
void
free_movements (DenemoProject * project)
{
  GList *g;
  free_scoreblocks (project);
  for (g = project->movements; g; g = g->next)
    {
      project->si = g->data;
      project->si->undo_guard = 1;  //no undo as that is per movement
      free_score (project);
    }
  project->si = NULL;
  delete_directives (&project->lilycontrol.directives);
  delete_directives (&project->scoreheader.directives);
  delete_directives (&project->paper.directives);
  g_list_free (project->movements);
  project->movements = NULL;


  /* any other free/initializations */

}

/**
* Wrapper function to close application when the quit
* menu item has been used
* 
*
*/
void
closewrapper (GtkAction * action, DenemoScriptParam* param)
{
  if(!Denemo.non_interactive){
    GList *display;

    if (Denemo.accelerator_status)
      {
        if (confirm (_("You have made changes to the commands you have"), _("Do you want to save the changes?")))
          save_accels ();
      }
    for (display = Denemo.projects; display != NULL; display = g_list_next (display))
      {
        Denemo.project = (DenemoProject *) display->data;
        if (close_gui_with_check (NULL, NULL) == FALSE)
          break;
      }
  }
}

/**
 * callback from deleting window belonging to project:
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
void
fetchcommands (GtkAction * action, DenemoScriptParam* param)
{
  static gchar *location = NULL;
  location = g_build_filename (get_user_data_dir (TRUE), "download", COMMANDS_DIR, NULL);
  gboolean err = g_mkdir_with_parents (location, 0770);
  if (err)
    {
      gchar *message = g_strdup_printf (_("Could not make folder %s for the downloaded commands"), location);
      warningdialog (message);
      g_free (message);
      return;
    }

  g_debug ("location is %s\n", location);
  GError *error = NULL;
  gchar *arguments[] = {
    "wget",
    "-N",
    "-r",
    "-np",                      //only below the menus directory
    "-nH",                      //cut prefix
    "--cut-dirs=1",             //cut download part of path
    DENEMO_DEFAULT_ANON_FTP,
    NULL
  };

  g_spawn_async (location,      /* dir */
                 arguments, NULL,       /* env */
                 G_SPAWN_SEARCH_PATH,   /* search in path for executable */
                 NULL,          /* child setup func */
                 NULL,          /* user data */
                 NULL, &error);
  //FIXME create a callback to tell the user the result...
}


/**
 * callback to load system extra commands
 * if user has a local (possibly updated) set in ~/.denemo/downloads then that directory is used.
 */
void
morecommands (GtkAction * action, DenemoScriptParam* param)
{
  RETURN_IF_NON_INTERACTIVE();
  static gchar *location = NULL;
  location = g_build_filename (get_user_data_dir (TRUE), "download", COMMANDS_DIR, "menus", NULL);
  if (!g_file_test (location, G_FILE_TEST_EXISTS))
    {
      g_free (location);
      location = NULL;
    }
  if (location == NULL)
    location = g_build_filename (get_system_data_dir (), COMMANDS_DIR, "menus", NULL);
  load_keymap_dialog_location (location);
  //#define WARNING_NEW_MENUS "Note: if you load a command that creates a new menu\nSome of the new commands may not work until you have exited\nand re-started denemo"
  //warningdialog(WARNING_NEW_MENUS);
  if (Denemo.last_merged_command && g_str_has_prefix (Denemo.last_merged_command, get_system_data_dir ()))
    {
      g_free (location);
      location = g_strdup (Denemo.last_merged_command); //FIXME
    }
}

/**
 * callback to load local extra commands
 * 
 */
void
mycommands (GtkAction * action, DenemoScriptParam* param)
{
  RETURN_IF_NON_INTERACTIVE();
  static gchar *location = NULL;
  if (location == NULL)
    location = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "menus", NULL);

  if (Denemo.last_merged_command && g_str_has_prefix (Denemo.last_merged_command, get_user_data_dir (TRUE)))
    {
      g_free (location);
      location = g_path_get_dirname (Denemo.last_merged_command);
    }
  load_keymap_dialog_location (location);
  // warningdialog(WARNING_NEW_MENUS);
  //g_debug("The last was %s %s %s\n", Denemo.last_merged_command, location,  get_user_data_dir(FALSE));
}



/**
 * Open in New Window callback 
 * Creates new view then opens file in the view
 */
void
openinnew (GtkAction * action, DenemoScriptParam * param)
{
  RETURN_IF_NON_INTERACTIVE();
  newtab ();
  file_open_with_check (NULL, param);
  if (param && (param->status == FALSE))
    close_project ();
  set_title_bar (Denemo.project);
}


/**
 * Close callback 
 * if user confirms close the current project
 * if it is the last close the application.
 * return FALSE if project was not closed, else TRUE
 */
gboolean
close_gui_with_check (GtkAction * action, DenemoScriptParam* param)
{
  DenemoProject *project = Denemo.project;
  Denemo.prefs.mode = Denemo.project->mode;
  if (action_callbacks (Denemo.project))
    return FALSE;               //Denemo.project may have been closed, depends on script callbacks;

  //do not ask for confirm if scripted FIXME
  if ((!project->notsaved) || (project->notsaved && confirmbox (project)))
    close_project ();
  else
    return FALSE;
  if (Denemo.projects == NULL)
    {
      if(!Denemo.non_interactive)
        storeWindowState ();
      writeHistory ();
      writeXMLPrefs (&Denemo.prefs);
      writePalettes ();
#ifdef G_OS_WIN32
      CoUninitialize ();
      g_message ("Windows - Exiting without shutting down audio");
      if (project->input_source == INPUTMIDI)
        {
          if (confirm (_("MIDI Controller Active?"), _("Please turn off your MIDI keyboard\nif you have not already done so")))
            _exit (0);          //audio shutdown can hang
        }
      else
        _exit (0);
#endif

      audio_shutdown ();


      exit (0);                 //do not use gtk_main_quit, as there may be inner loops active.
    }
  return TRUE;
}


static void
singleton_callback (GtkToolButton * toolbutton, RhythmPattern * r)
{
  DenemoProject *project = Denemo.project;
#define CURRP ((RhythmPattern *)project->currhythm->data)
  if (project->currhythm && CURRP)
    unhighlight_rhythm (CURRP);
  project->currhythm = NULL;

  project->rstep = r->rsteps;
  project->cstep = NULL;

#define g (project->rstep)
#define MODE (project->mode)
  unhighlight_rhythm (project->prevailing_rhythm);
  project->prevailing_rhythm = r;
  highlight_rhythm (r);
  if ((MODE & (INPUTEDIT | INPUTRHYTHM)))
    {
      gint save = MODE;
      MODE = INPUTINSERT | INPUTNORMAL;
      ((GSourceFunc) (((RhythmElement *) g->data)->functions->data)) (project);
      displayhelper (project);
      MODE = save;
    }
#undef CURRP
#undef g
#undef MODE
}

/*UNUSED
static void
pb_first (GtkWidget * button)
{
  call_out_to_guile ("(DenemoFirst)");
}
*/

static void
pb_go_back (GtkWidget * button)
{
  call_out_to_guile ("(DenemoGoBack)");
}

static void
pb_previous (GtkWidget * button)
{
  call_out_to_guile ("(DenemoPrevious)");
}

/*UNUSED
static void
pb_rewind (GtkWidget * button)
{
  call_out_to_guile ("(DenemoRewind)");
}
*/
static void
pb_stop (GtkWidget * button)
{
  call_out_to_guile ("(DenemoStop)");
}

static void
pb_play (GtkWidget * button)
{
  call_out_to_guile ("(DenemoPlay)");
}

/*UNUSED
static void
pb_pause (GtkWidget * button)
{
  call_out_to_guile ("(DenemoPause)");
}
*/
/*UNUSED
static void
pb_forward (GtkWidget * button)
{
  call_out_to_guile ("(DenemoForward)");
}
*/
static void
pb_next (GtkWidget * button)
{
  call_out_to_guile ("(DenemoNext)");
}

static void
pb_go_forward (GtkWidget * button)
{
  call_out_to_guile ("(DenemoGoForward)");
}

/*UNUSED
static void
pb_last (GtkWidget * button)
{
  call_out_to_guile ("(DenemoLast)");
}
*/
static void
pb_start_to_cursor (GtkWidget * button)
{
  call_out_to_guile ("(DenemoSetPlaybackStart)");
  //gtk_widget_draw(Denemo.scorearea, NULL);
  score_area_needs_refresh ();
  draw_score (NULL);
}

static void
pb_end_to_cursor (GtkWidget * button)
{
  call_out_to_guile ("(DenemoSetPlaybackEnd)");
  //gtk_widget_draw(Denemo.scorearea, NULL);
  score_area_needs_refresh ();
  draw_score (NULL);
}

static void
pb_loop (GtkWidget * button)
{
  call_out_to_guile ("(DenemoLoop)");
}

static void
pb_tempo (GtkAdjustment * adjustment)
{
  gdouble tempo;
  gdouble bpm = gtk_adjustment_get_value (adjustment);
  tempo = (Denemo.project->si->tempo > 0) ? bpm / Denemo.project->si->tempo : 1.0;
  scm_c_define ("DenemoTempo::Value", scm_from_double (tempo));
  call_out_to_guile ("(DenemoTempo)");
  Denemo.project->si->smfsync = G_MAXINT;
}
void
update_tempo_widget (gdouble value)
{
  gdouble bpm = gtk_adjustment_get_value (master_tempo_adj);//g_debug("bpm %f and correction %f\n", bpm, value);
  bpm += value;
  gtk_adjustment_set_value (master_tempo_adj, bpm);
  gtk_adjustment_changed (master_tempo_adj);
  Denemo.project->si->smfsync = G_MAXINT;
}

#ifdef _HAVE_RUBBERBAND_
static void
set_speed (GtkAdjustment * adjustment)
{
  gdouble speed = gtk_adjustment_get_value (adjustment);
  set_playback_speed(speed);
}
#endif

static void
pb_volume (GtkAdjustment * adjustment)
{
  gdouble volume = gtk_adjustment_get_value (adjustment);
  scm_c_define ("DenemoVolume::Value", scm_from_double (volume));
  call_out_to_guile ("(DenemoVolume)");
}

static void
audio_volume_cut (GtkAdjustment * adjustment)
{
  if (Denemo.project->si->recording)
    {
      Denemo.project->si->recording->volume = gtk_adjustment_get_value (adjustment);
    }
}

static void
audio_volume_boost (GtkAdjustment * adjustment)
{
  if (Denemo.project->si->recording)
    {
      Denemo.project->si->recording->volume = gtk_adjustment_get_value (adjustment);
    }
}

static void
leadin_changed (GtkSpinButton * spin)
{
  if (Denemo.project->si->recording)
    {
      set_lead_in (gtk_spin_button_get_value (spin));
      //g_debug("%d for %f\n", Denemo.project->si->recording->leadin, gtk_spin_button_get_value(spin));
    }
}

void
update_leadin_widget (gdouble secs)
{
  gtk_spin_button_set_value (leadin, secs);
}

static void
pb_set_range (GtkWidget * button)
{
  call_out_to_guile ("(DenemoSetPlaybackIntervalToSelection)");
}

static void
pb_range (GtkWidget * button)
{
  PlaybackRangeDialog ();
}

static void
pb_panic (GtkWidget * button)
{
  playback_panic ();
  Denemo.project->si->start_time = 0.0;
  Denemo.project->si->end_time = -1.0;      //ie unset
  set_start_and_end_objects_for_draw ();
  reset_temperament ();
  score_area_needs_refresh ();
}

static void
track_delete (smf_track_t * track)
{
  if (track == NULL || track->user_pointer)
    return;
  if (track->smf == NULL)
    {
      smf_t *smf = smf_new ();
      smf_add_track (smf, track);
      smf_delete (smf);
    }
  else
    smf_track_delete (track);
}

void
set_midi_in_status (void)
{
  if (midi_in_status)
    {
      gchar *text = NULL;
      if ((Denemo.project->midi_destination & MIDIRECORD) && (Denemo.project->midi_destination & MIDIPLAYALONG))
        text = _("<span foreground=\"blue\">" "Recording + Play Along" "</span>");
      else if (Denemo.project->midi_destination & MIDIRECORD)
        text = _("<span foreground=\"red\">" "Recording" "</span>");
      else if (Denemo.project->midi_destination & MIDIPLAYALONG)
        text = _("<span foreground=\"red\">" "Play Along" "</span>");
      else if ((Denemo.keyboard_state & ~GDK_LOCK_MASK) == (GDK_CONTROL_MASK))
        text = _("Checking Pitches");
      else if ((Denemo.keyboard_state == (GDK_SHIFT_MASK)) || (Denemo.keyboard_state == (GDK_LOCK_MASK)))
        text = _("Listening to Pitches");

      else if ((Denemo.keyboard_state & CHORD_MASK))
        text = _("Adding to a Chord");
      else if ((Denemo.keyboard_state & ADDING_MASK))
        text = _("Starting a Chord");

      else
        text = _("Appending/Editing Pitches");
      gtk_label_set_markup (GTK_LABEL (midi_in_status), text);
    }
}



void
pb_conduct (GtkWidget * button)
{
  Denemo.project->midi_destination ^= MIDICONDUCT;
  if (Denemo.project->midi_destination & MIDICONDUCT)
    gtk_button_set_label (GTK_BUTTON (button), _("Mouse Conductor ON"));
  else
    gtk_button_set_label (GTK_BUTTON (button), _("Mouse Conductor OFF"));
}



void
pb_playalong (GtkWidget * button)
{
  Denemo.project->midi_destination ^= MIDIPLAYALONG;
  if (Denemo.project->midi_destination & MIDIPLAYALONG)
    gtk_button_set_label (GTK_BUTTON (button), _("Switch to Normal Playback"));
  else
    gtk_button_set_label (GTK_BUTTON (button), _("Switch to Play Along Playback"));
  set_midi_in_status ();
}

gboolean show_midi_record_control(void) {
      gtk_widget_show (deletebutton);
      gtk_widget_show (convertbutton);
      set_midi_in_status ();
      return FALSE;// stop timer callback
 }
gboolean
pb_record (gchar *callback)
{
   if(is_playing())
        {
            warningdialog(_("Stop playing first"));
            return FALSE;   
        }
  if (Denemo.project->si->recording && (Denemo.project->si->recording->type==DENEMO_RECORDING_AUDIO))
    {
        warningdialog(_("Cannot mix audio and MIDI recordings"));
        return  FALSE;
    }
    
  if (Denemo.project->si->recorded_midi_track &&  midi_is_from_file())
    {
        warningdialog(_("Cannot mix MIDI recordings with imported MIDI - delete imported MIDI first"));
        return FALSE;
     }
    
  if (Denemo.project->si->recorded_midi_track && !confirm (_("MIDI Recording"), _("Delete last recording?")))
    {
      return FALSE;
    }
    
  delete_imported_midi (); 
  call_out_to_guile ("(DenemoSetPlaybackStart)");


 
  new_midi_recording();
  
  
  
  Denemo.project->midi_destination |= MIDIRECORD;
  track_delete (Denemo.project->si->recorded_midi_track);
  Denemo.project->si->recorded_midi_track = smf_track_new ();
  gtk_widget_hide (deletebutton);
  gtk_widget_hide (convertbutton);
  
  set_midi_in_status ();
  gchar *script = callback?g_strdup_printf("(d-Play \"%s\")", callback): g_strdup("(d-Play)");
  call_out_to_guile (script);
  g_free(script);
 {//this note off event prevents the first MIDI note from sounding a few seconds into the recording
 //why such a spurious note is heard is unknown, it does not get put into the event queues (immediate or standard)
      gchar buf[] = {0x80, 0x0, 0x0};
      handle_midi_event (buf);
    
  }
  return TRUE;
}

static void
pb_audiorecord (GtkWidget * button)
{
  gtk_button_set_image (GTK_BUTTON (audiorecordbutton),
    gtk_image_new_from_stock (GTK_STOCK_MEDIA_RECORD, GTK_ICON_SIZE_BUTTON));//highlighting may have turned it off
  if (Denemo.prefs.maxrecordingtime)
  {
    Denemo.project->audio_recording = !Denemo.project->audio_recording;
    if(!Denemo.project->audio_recording) gtk_widget_show(exportbutton);
  }
  else
  {
    warningdialog (_("The preference set for recording time is 0 - nothing can be recorded.\nSee Edit → Change Preferences Audio/Midi Tab"));
  }
}
static void
pb_exportaudio (GtkWidget * button)
{
  if(!Denemo.project->audio_recording)
    Denemo.project->audio_recording = FALSE;
  export_recorded_audio ();
}

void highlight_audio_record(void) {
  static gboolean on;
  on = !on;
  gtk_button_set_image (GTK_BUTTON (audiorecordbutton),
    gtk_image_new_from_stock (on?GTK_STOCK_MEDIA_RECORD:GTK_STOCK_MEDIA_STOP, GTK_ICON_SIZE_BUTTON));
}

void delete_recording (void) {
     //FIXME a better name for the mutex which originally was just for midi data, but will work for audio data too.
  if (Denemo.project->si->recording)
    {
      DenemoRecording *temp = Denemo.project->si->recording;
      g_static_mutex_lock (&smfmutex);
      Denemo.project->si->recording = NULL;
      g_static_mutex_unlock (&smfmutex);
      if (temp->sndfile)
        sf_close (temp->sndfile);
      g_free (temp->filename);
      g_list_free_full(temp->notes, g_free);
      g_free (temp);
      Denemo.project->si->recording = NULL;
      Denemo.project->si->marked_onset = NULL;
    }   
}
static void
pb_midi_delete (GtkWidget * button)
{
  DenemoRecording *recording = Denemo.project->si->recording;
  if(recording)
    {
      if(recording->type!=DENEMO_RECORDING_MIDI)
        {
            g_warning("Cannot delete Audio yet");
            return;//see sourceaudio.c:222 for deleting audio
        }
      track_delete (Denemo.project->si->recorded_midi_track);
      Denemo.project->si->recorded_midi_track = NULL;
      
      delete_recording ();
    }
  gtk_widget_hide (convertbutton);
  gtk_widget_hide (button);
  gtk_widget_queue_draw(Denemo.scorearea);
}

static void
pb_midi_convert (GtkWidget * button)
{

  call_out_to_guile ("(DenemoConvert)");

  g_info ("Finished midi convert");
}




/**
 * Rhythm callback select rhythm
 * inserts the rhythm if pitchless
 */
void
select_rhythm_pattern (RhythmPattern * r)
{
  DenemoProject *project = Denemo.project;
#define CURRP ((RhythmPattern *)project->currhythm->data)

  if (project->currhythm && (CURRP != r))
    {                           //Change the highlighting
      if (CURRP)
        unhighlight_rhythm (CURRP);
      else if (project->rstep)
        unhighlight_rhythm (((RhythmElement *) project->rstep->data)->rhythm_pattern);
    }

  project->currhythm = g_list_find (project->rhythms, r);
  if (project->currhythm != NULL)
    {
      project->rstep = r->rsteps;
      project->cstep = r->clipboard->data;

      gchar *text = ((RhythmElement *) project->rstep->data)->icon;
      if (text)
        {
          GtkWidget *label = LABEL (CURRP->button);
          //g_debug("markup is %s\n", ((RhythmElement*)g->data)->icon);
          gtk_label_set_markup (GTK_LABEL (label), text);
        }
      highlight_rhythm (CURRP);
    }
#undef CURRP
}

static void
activate_rhythm_pattern (GtkToolButton * toolbutton, RhythmPattern * r)
{
  select_rhythm_pattern (r);
  if ((Denemo.project->mode & INPUTEDIT))
    {
      if (Denemo.project->input_source == INPUTMIDI)
        {
          insert_note_following_pattern (Denemo.project);
          ((DenemoObject *) Denemo.project->si->currentobject->data)->isinvisible = TRUE;
        }
      else
        insert_clipboard (r->clipboard);
    }
}


/* duration_code(gpointer function)
 * return an ascii code to indicate what duration (if any) function gives.
 * '0x0' means not a duration
 * chars 012345678 are the standard note durations
 * 
 */
gchar
duration_code (gpointer fn)
{
  return fn == (gpointer) insert_chord_0key ? '0' : fn == (gpointer) insert_chord_1key ? '1' : fn == (gpointer) insert_chord_2key ? '2' : fn == (gpointer) insert_chord_3key ? '3' : fn == (gpointer) insert_chord_4key ? '4' : fn == (gpointer) insert_chord_5key ? '5' : fn == (gpointer) insert_chord_6key ? '6' : fn == (gpointer) insert_chord_7key ? '7' : fn == (gpointer) insert_chord_8key ? '8' : 0;
}

/* modifier_code(gpointer function)
 * return an ascii code to indicate what modifier (if any) function gives.
 * '0x0' means not a valid modifier for a rhythmic duration
 * char '.' means a dotted note, '(' and ')' mean start and end slur
 * r to z are rests
 * others to be defined
 * 
 */
gchar
modifier_code (gpointer fn)
{
  return fn == (gpointer) start_triplet ? '~' :
    fn == (gpointer) end_tuplet ? '|' :
    fn == (gpointer) add_dot_key ? '.' :
    fn == (gpointer) toggle_begin_slur ? '(' : fn == (gpointer) toggle_end_slur ? ')' : fn == (gpointer) insert_rest_0key ? 'r' : fn == (gpointer) insert_rest_1key ? 's' : fn == (gpointer) insert_rest_2key ? 't' : fn == (gpointer) insert_rest_3key ? 'u' : fn == (gpointer) insert_rest_4key ? 'v' : fn == (gpointer) insert_rest_5key ? 'w' : fn == (gpointer) insert_rest_6key ? 'x' : fn == (gpointer) insert_rest_7key ? 'y' : fn == (gpointer) insert_rest_8key ? 'z' : 0;
}

gboolean
code_is_a_duration (gchar code)
{
  return code == 0 || (code >= 'r' && code <= 'z');
}



/* add_to_rhythm appends to a rhythm pattern the callback function fn
   fn is a callback function
   returns TRUE if something was added
 */
gboolean
append_rhythm (RhythmPattern * r, gpointer fn)
{
  RhythmElement *relement;

  int keyval = duration_code (fn);
  if (keyval)
    {

      relement = (RhythmElement *) g_malloc0 (sizeof (RhythmElement));

      relement->functions = g_list_append (NULL, fn);

      r->rsteps = g_list_append (r->rsteps, relement);
      relement->rhythm_pattern = r;
      return TRUE;
    }
  keyval = modifier_code (fn);
  if (keyval)
    {
      if (r->rsteps)
        {
          relement = (RhythmElement *) (g_list_last (r->rsteps)->data);
        }
      else
        {
          relement = (RhythmElement *) g_malloc0 (sizeof (RhythmElement));
        }
      relement->functions = g_list_append (relement->functions, (gpointer) fn);
      if (r->rsteps == NULL)
        {
          r->rsteps = g_list_append (r->rsteps, relement);
        }
      relement->rhythm_pattern = r;
      return TRUE;
    }
  return FALSE;
}


static void
add_to_pattern (gchar ** p, gchar c)
{
  gchar *temp = g_strdup_printf ("%s%c", *p, c);        //FIXME this should be done with a GString in the caller.
  g_free (*p);
  *p = temp;
}

static void
remove_breaks (GList * clip)
{
  for (; clip; clip = clip->next)
    {
      GList *g = clip->data;
      for (; g; g = g->next)
        {
          //g_debug("have %x type %d\n", g->data, ((DenemoObject*)g->data)->type);
          if ((((DenemoObject *) g->data)->type == MEASUREBREAK) || (((DenemoObject *) g->data)->type == STAFFBREAK))
            g = clip->data = g_list_delete_link (clip->data, g);        //we search from the start again, as g has been freed
        }
    }
}

static void
attach_clipboard (RhythmPattern * r)
{
  DenemoProject *project = Denemo.project;
  DenemoMovement *si = project->si;
  if (si->markstaffnum)
    {
      push_clipboard ();
      copytobuffer (si);
      push_clipboard ();
      r->clipboard = pop_off_clipboard ();
      remove_breaks (r->clipboard);
      pop_clipboard ();
    }
}


gint
insert_pattern_in_toolbar (RhythmPattern * r)
{
  DenemoProject *project = Denemo.project;
  if (r->clipboard == NULL)
    {
      g_warning ("No clipboard for this pattern, cannot add");
      return -1;
    }
  GtkWidget *toolbar = gtk_ui_manager_get_widget (Denemo.ui_manager, "/RhythmToolBar");
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), GTK_TOOL_ITEM (r->button), -1);
  gtk_widget_show_all (GTK_WIDGET (r->button));
  project->rstep = r->rsteps;
  project->cstep = r->clipboard->data;
  project->rhythms = g_list_append (project->rhythms, r);

  if (project->currhythm)
    unhighlight_rhythm ((RhythmPattern *) project->currhythm->data);
  project->currhythm = g_list_last (project->rhythms);
  highlight_rhythm ((RhythmPattern *) project->currhythm->data);
  g_signal_connect (G_OBJECT (r->button), "clicked", G_CALLBACK (activate_rhythm_pattern), (gpointer) r);
  return g_list_length (project->rhythms);  //the index of the newly added snippet
}

void
install_button_for_pattern (RhythmPattern * r, gchar * thelabel)
{
  GtkToolButton *button;
  GtkWidget *label;
  button = (GtkToolButton *) gtk_tool_button_new (NULL, NULL);
  label = gtk_label_new (thelabel);
  gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
  gtk_tool_button_set_label_widget (button, label);
  r->button = button;
}


/* create_rhythm_cb
   This is overloaded for use as a callback (ACTION is a GtkAction) and
   as a call to set up the "singleton rhythms", 
   (rhythm patterns that are just one note or rest, used for
   ordinary note entry).
   if ACTION is a GtkAction*
        create a rhythm pattern from the current selection
        the rhythm is put in project->
        a button is created in "/RhythmToolbar"
        and the pattern is added to project->rhythms 
         with the first step of it put in project->rstep
    add a clipboard with the selected music to the created rhythm pattern.
   if ACTION is one of the insert_chord_xkey insert_rest_xkey)
   functions
        a button is created in the /EntryToolbar (if not already present)
   

*/
void
create_rhythm_cb (GtkAction * action, DenemoScriptParam* param)
{
  DenemoProject *project = Denemo.project;
  gboolean singleton = FALSE;   // set TRUE if action is one of the insert_... functions.
  gboolean already_done = FALSE;        // a singleton which has already been installed globally
  gboolean default_rhythm = FALSE;
  DenemoMovement *si = project->si;
  RhythmPattern *r = (RhythmPattern *) g_malloc0 (sizeof (RhythmPattern));
  GString *lily_string = g_string_new ("");
  gchar *pattern = NULL;
  if (action == (gpointer) insert_chord_0key)
    pattern = g_strdup ("0");
  if (action == (gpointer) insert_chord_1key)
    pattern = g_strdup ("1");
  if (action == (gpointer) insert_chord_2key)
    pattern = g_strdup ("2"), default_rhythm = TRUE;
  if (action == (gpointer) insert_chord_3key)
    pattern = g_strdup ("3");
  if (action == (gpointer) insert_chord_4key)
    pattern = g_strdup ("4");
  if (action == (gpointer) insert_chord_5key)
    pattern = g_strdup ("5");
  if (action == (gpointer) insert_chord_6key)
    pattern = g_strdup ("6");
  if (action == (gpointer) insert_chord_7key)
    pattern = g_strdup ("7");
  if (action == (gpointer) insert_chord_8key)
    pattern = g_strdup ("8");

  if (action == (gpointer) insert_rest_0key)
    pattern = g_strdup ("r");
  if (action == (gpointer) insert_rest_1key)
    pattern = g_strdup ("s");
  if (action == (gpointer) insert_rest_2key)
    pattern = g_strdup ("t");
  if (action == (gpointer) insert_rest_3key)
    pattern = g_strdup ("u");
  if (action == (gpointer) insert_rest_4key)
    pattern = g_strdup ("v");
  if (action == (gpointer) insert_rest_5key)
    pattern = g_strdup ("w");
  if (action == (gpointer) insert_rest_6key)
    pattern = g_strdup ("x");
  if (action == (gpointer) insert_rest_7key)
    pattern = g_strdup ("y");
  if (action == (gpointer) insert_rest_8key)
    pattern = g_strdup ("z");
  if (pattern)
    {                           /* if we already have it globally we don't need it again
                                   note we never delete the singleton rhythms */
      if (Denemo.singleton_rhythms[(unsigned int) *pattern])
        {
          g_free (r);
          r = Denemo.singleton_rhythms[(unsigned int) *pattern];
          already_done = TRUE;
        }
      else
        {
          Denemo.singleton_rhythms[(unsigned int) *pattern] = r;
          already_done = FALSE;
        }
      singleton = TRUE;
    }
  else
    pattern = g_strdup ("");
  if (!already_done)
    install_button_for_pattern (r, NULL);

  if (!singleton)
    {
      staffnode *curstaff;
      measurenode *curmeasure;
      gint i = si->selection.firststaffmarked;
      attach_clipboard (r);

      if (project->lilysync != project->changecount)
        refresh_lily_cb (NULL, Denemo.project);

      curstaff = g_list_nth (si->thescore, i - 1);
      if (curstaff && i <= si->selection.laststaffmarked)
        {
          int j, k;
          objnode *curobj;
          /* Measure loop.  */
          for (j = si->selection.firstmeasuremarked, k = si->selection.firstobjmarked, curmeasure = g_list_nth (firstmeasurenode (curstaff), j - 1); curmeasure && j <= si->selection.lastmeasuremarked; curmeasure = curmeasure->next, j++)
            {
              for (curobj = g_list_nth ((objnode *) curmeasure->data, k);
                   /* cursor_x is 0-indexed */
                   curobj && (j < si->selection.lastmeasuremarked || k <= si->selection.lastobjmarked); curobj = curobj->next, k++)
                {
                  gpointer fn;
                  DenemoObject *obj = (DenemoObject *) curobj->data;
                  switch (obj->type)
                    {
                    case TUPCLOSE:
                      fn = (gpointer) end_tuplet;
                      add_to_pattern (&pattern, '|');
                      append_rhythm (r, fn);
                      break;
                    case TUPOPEN:
                      switch (((tupopen *) obj->object)->denominator)
                        {
                        case 3:
                          fn = (gpointer) start_triplet;
                          add_to_pattern (&pattern, '~');
                          break;
                        default:       // need to create start_xxxtuplet() functions to go with start_triplet(), then they can go here.
                          fn = NULL;
                        }
                      append_rhythm (r, fn);
                      break;
                    case CHORD:
                      {
                        chord *ch = (chord *) obj->object;

                        if (ch->notes)
                          {
                            switch (ch->baseduration)
                              {
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
                              case 7:
                                fn = insert_chord_7key;
                                break;
                              case 8:
                                fn = insert_chord_8key;
                                break;
                              default:
                                g_warning ("Handling unknown type of chord as whole note");
                                fn = insert_chord_0key;
                                break;
                              }
                            add_to_pattern (&pattern, duration_code (fn));
                            append_rhythm (r, fn);
                          }
                        else
                          {     /* a rest */
                            switch (ch->baseduration)
                              {
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
                              case 7:
                                fn = insert_rest_7key;
                                break;
                              case 8:
                                fn = insert_rest_8key;
                                break;
                              default:
                                g_warning ("Handling unknown type of rest as whole note rest");
                                fn = insert_rest_0key;
                                break;
                              }
                            add_to_pattern (&pattern, modifier_code (fn));
                            append_rhythm (r, fn);
                          }     /* end of rests */
                        for (i = ch->numdots; i; i--)
                          {
                            fn = add_dot_key;
                            add_to_pattern (&pattern, modifier_code (fn));
                            append_rhythm (r, fn);
                          }
                        if (ch->slur_begin_p)
                          {
                            fn = (gpointer) toggle_begin_slur;
                            add_to_pattern (&pattern, '(');
                            append_rhythm (r, fn);
                          }
                        if (ch->slur_end_p)
                          {
                            fn = (gpointer) toggle_end_slur;
                            add_to_pattern (&pattern, ')');
                            append_rhythm (r, fn);
                          }
                      }
                      break;
                    default:
                      //g_warning("ignoring %d", obj->type);
                      break;
                    }           /* end of switch obj type */
                  //g_debug("Number of rhythms %d\n", g_list_length(r->rsteps));
                  if (obj->lilypond)
                    g_string_append (lily_string, obj->lilypond);
                }               /* End object loop */
              k = 0;            //in the new measure start collecting objects from start
            }                   /* End measure loop */
        }                       //if the first staff selected is present in the staff list. This should always be the case

      if ((strlen (pattern) == 0))
        {                       // no selection
          warningdialog (_("No selection to create a music snippet from\nSee Edit → Select menu for selecting music to snip"));
          gtk_widget_destroy (GTK_WIDGET (r->button));
          g_free (pattern);
          g_free (r);
          return;
        }
    }
  else
    {                           // singleton
      if (!already_done)
        append_rhythm (r, action);
    }

  r->lilypond = g_string_free (lily_string, FALSE);
  if (!already_done)
    {
      gchar *labelstr;
      if (pattern)
        {
          labelstr = music_font (pattern);
        }
      else
        return;                 //FIXME memory leak of r - well pattern is never NULL
      //g_debug("rsteps is %p entry is %s, %s\n", r->rsteps, pattern, labelstr);
      GtkWidget *label = gtk_tool_button_get_label_widget (r->button);
      gtk_label_set_markup (GTK_LABEL (label), labelstr);
      g_free (labelstr);
    }

  if (!singleton)
    {
      /* fill the r->rsteps with icons for each step, singletons have NULL icon */
      GList *g;
      RhythmElement *el;
      gint i;
      for (g = r->rsteps, i = 0; g; g = g->next, i++)
        {
          el = (RhythmElement *) g->data;
          if (i == 0 && (*(pattern) < '0' || *(pattern) > '8') && g->next)
            g = g->next;        // pattern does not start with a note, so we skip to the second element, unless there are no notes
          while (*(pattern + i) && (*(pattern + i) < '0' || *(pattern + i) > '8'))
            i++;
          if (*(pattern + i))
            {
              *(pattern + i) += HIGHLIGHT_OFFSET;
              el->icon = music_font (pattern);
              *(pattern + i) -= HIGHLIGHT_OFFSET;
            }
          //g_debug("el->icon = %s step %d pattern %s\n", el->icon, i, pattern);
        }
    }
  if (!already_done)
    if (r->rsteps)
      {
        /* make the list circular */
        r->rsteps->prev = g_list_last (r->rsteps);
        g_list_last (r->rsteps)->next = r->rsteps;
      }
  if (r->rsteps == NULL)
    {
      gtk_widget_destroy (GTK_WIDGET (r->button));
      g_free (r);
      r = NULL;
    }
  else
    {
      if (singleton)
        {
          if (default_rhythm)
            {
              project->prevailing_rhythm = r;
              project->rstep = r->rsteps;
              project->cstep = NULL;
              highlight_rhythm (r);
              //g_debug("prevailing rhythm is %p\n",r);
            }
        }
      else
        {                       //not singleton
          insert_pattern_in_toolbar (r);

        }
    }
  if (pattern)
    g_free (pattern);
}



static void
save_accels (void)
{
  save_default_keymap_file ();
  Denemo.accelerator_status = FALSE;
}


static void show_type (GtkWidget * widget, gchar * message);


static void
configure_keyboard_idx (GtkWidget * w, gint idx)
{
  configure_keyboard_dialog_init_idx (NULL, idx);
}

//static void toggleRecording (GtkWidget*w, gboolean *record) {
//g_debug("Recording was %d\n", *record);
//  *record = !*record;
//}

static void
toggle_record_script (GtkAction * action, gpointer param)
{
  Denemo.ScriptRecording = !Denemo.ScriptRecording;
}

static void
appendSchemeText_cb (GtkWidget * widget, gchar * text)
{
  appendSchemeText (text);
}



static void
load_command_from_location (GtkWidget * w, gchar * filepath)
{
  gchar *location = g_strdup_printf ("%s%c", filepath, G_DIR_SEPARATOR);
  g_info ("Calling the file loader with %s", location);
  load_keymap_dialog_location (location);
  g_free (location);
}


static void attach_right_click_callback (GtkWidget * widget, GtkAction * action);

/* the callback for menu items that are scripts. The script is attached to the action,
tagged as "scheme".
The script may be empty, in which case it is fetched from actions/menus...

This call also ensures that the right-click callback is attached to all the proxies of the action, as there are problems trying to do this earlier, and it defines a scheme variable to give the name of the script being executed.
*/
gboolean
activate_script (GtkAction * action, DenemoScriptParam * param)
{
  DenemoProject *project = Denemo.project;
  gboolean ret = FALSE;
  // the proxy list is NULL until the menu item is first called...
  //BUT if you first activate it with right button ....
  if (GTK_IS_ACTION (action))
    {
      if (!g_object_get_data (G_OBJECT (action), "signal_attached"))
        {
          GSList *h = gtk_action_get_proxies (action);
          for (; h; h = h->next)
            {
              attach_right_click_callback (h->data, action);
              show_type (h->data, "type is ");
            }
        }

      //FIXME use define_scheme_variable for this
      //define a global variable in Scheme (CurrentScript) to give the name of the currently executing script
      const gchar *name = gtk_action_get_name (action);
      gchar *current_script = g_strdup_printf ("(define CurrentScript \"%s\")\n", name);

      /*note that scripts must copy their name from CurrentScript into local storage before calling other scripts if they
         need it */
      gchar *paramvar = NULL;
      if (param && param->string)
        {
          paramvar = g_strdup_printf ("%s::params", name);
          scm_c_define (paramvar, scm_from_locale_string (param->string->str));
        }

      scm_c_eval_string (current_script);
      g_free (current_script);

      gchar *text = (gchar *) g_object_get_data (G_OBJECT (action), "scheme");
      if (!is_action_name_builtin((gchar*) gtk_action_get_name(action)))
        {
          if (!text || !*text)
            text = load_command_data (action);
          if (text && *text)
            {
              stage_undo (project->si, ACTION_STAGE_END);   //undo is a queue so this is the end :)
              ret = (gboolean) ! call_out_to_guile (text);
              stage_undo (project->si, ACTION_STAGE_START);
            }
          else
            {
              g_warning ("Could not get script for %s", gtk_action_get_name (action));
              ret = FALSE;
            }
        }
      if (paramvar)
        scm_c_define (paramvar, SCM_BOOL_F);
      g_free (paramvar);
    }
  else
    warningdialog (_("Have no way of getting the script, sorry"));
  return ret;
}




/*pop up the help for passed command as info dialog
 */
static void
popup_help (GtkWidget * widget, GtkAction * action)
{
  const gchar *name = gtk_action_get_name (action);
  gint idx = lookup_command_from_name (Denemo.map, name);
  gchar *tooltip = idx >= 0 ? (gchar *) lookup_tooltip_from_idx (Denemo.map, idx) : "A menu for ...";

  tooltip = g_strdup_printf (_("Command: %s\n\nInformation:\n%s"), name, tooltip);
  infodialog (tooltip);
  g_free (tooltip);
}

/* replace dangerous characters in command names */
static void
subst_illegals (gchar * myname)
{
  gchar *c;                     // avoid whitespace etc
  for (c = myname; *c; c++)
    if (*c == ' ' || *c == '\t' || *c == '\n' || *c == '/' || *c == '\\')
      *c = '-';
}



typedef struct ModifierAction
{
  GtkAction *action;
  gint modnum;                  /* GdkModifierType number 0...12 */
  mouse_gesture gesture;        /* if this is for press move or release */
  gboolean left;                /* if this is for left or right mouse button */
} ModifierAction;


// info->action is the action for which the mouse shortcut is to be set
static void
setMouseAction (ModifierAction * info)
{
  GString *modname = mouse_shortcut_name (info->modnum, info->gesture, info->left);
  gint command_idx = lookup_command_for_keybinding_name (Denemo.map, modname->str);
  GtkAction *current_action = NULL;
  gchar *title = NULL;
  gchar *prompt = NULL;
  if (command_idx >= 0)
    {
      current_action = (GtkAction *) lookup_action_from_idx (Denemo.map, command_idx);
      title = g_strdup_printf (_("The Command %s Responds to this Shortcut"), lookup_name_from_idx (Denemo.map, command_idx));
      prompt = g_strdup_printf (_("Lose the shortcut %s for this?"), modname->str);
    }
  if (current_action == NULL || confirm (title, prompt))
    {
      remove_keybinding_from_name (Denemo.map, modname->str);   //by_name 
      const gchar *name = gtk_action_get_name (info->action);
      command_idx = lookup_command_from_name (Denemo.map, name);
      if (command_idx >= 0)
        add_named_binding_to_idx (Denemo.map, modname->str, command_idx, POS_LAST);
    }
  g_free (title);
  g_free (prompt);
  g_string_free (modname, TRUE);
}

static void
placeOnButtonBar (GtkWidget * widget, GtkAction * action)
{
  gchar *name = (gchar *) gtk_action_get_name (action);
  gint idx = lookup_command_from_name (Denemo.map, name);
  if (idx > 0)
    {
      gchar *label = (gchar *) lookup_label_from_idx (Denemo.map, idx);

      gchar *scheme = g_strdup_printf ("\n;To remove the %s button delete from here\n(CreateButton \"Button%s\" \"%s\")\n(d-SetDirectiveTagActionScript  \"Button%s\" \"(" DENEMO_SCHEME_PREFIX "%s)\")\n;;End of delete %s button", name, name, g_strescape (label, NULL), name, name, name);
      //g_debug("the scheme is \n%s\n", scheme);
      if (!call_out_to_guile (scheme))
        append_to_local_scheme_init (scheme);
      else
        warningdialog (_("Could not create button"));
      g_free (scheme);
    }
}

static void
placeInPalette (GtkWidget * widget, GtkAction * action)
{
  gchar *name = (gchar *) gtk_action_get_name (action);
  gint idx = lookup_command_from_name (Denemo.map, name);
  if (idx > 0)
    place_action_in_palette (idx, name);
}

/* gets a name label and tooltip from the user, then creates a menuitem in the menu 
   given by the path myposition whose callback is the activate on the current scheme script.
*/

static void
insertScript (GtkWidget * widget, gchar * insertion_point)
{
  DenemoProject *project = Denemo.project;
  gchar *myname, *mylabel, *myscheme, *mytooltip, *submenu;
  gchar *myposition = g_path_get_dirname (insertion_point);
  gchar *after = g_path_get_basename (insertion_point);
  gint idx = lookup_command_from_name (Denemo.map, after);
  myname = string_dialog_entry (project, "Create a new menu item", "Give item name (avoid clashes): ", "MyName");
  //FIXME check for name clashes

  if (myname == NULL)
    return;
  subst_illegals (myname);
  mylabel = string_dialog_entry (project, _("Create a new menu item"), _("Give menu label: "), _("My Label"));
  if (mylabel == NULL)
    return;

  mytooltip = string_dialog_entry (project, _("Create a new menu item"), _("Give explanation of what it does: "), _("Prints my special effect"));
  if (mytooltip == NULL)
    return;
  if (confirm (_("Create a new menu item"), _("Do you want the new menu item in a submenu?")))
    {
      submenu = string_dialog_entry (project, _("Create a new menu item"), _("Give a label for the Sub-Menu"), _("Sub Menu Label"));
      if (submenu)
        {
          subst_illegals (submenu);
          myposition = g_strdup_printf ("%s/%s", myposition, submenu);  //FIXME G_DIR_SEPARATOR in myposition???
        }
    }

  myscheme = getSchemeText ();

  gchar *xml_filename = g_strconcat (myname, XML_EXT, NULL);
  gchar *scm_filename = g_strconcat (myname, SCM_EXT, NULL);
  g_info ("The filename built is %s from %s", xml_filename, myposition);
  gchar *xml_path = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "menus", myposition, xml_filename, NULL);
  gchar *scm_path = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "menus", myposition, scm_filename, NULL);
  g_free (xml_filename);
  if ((!g_file_test (xml_path, G_FILE_TEST_EXISTS)) || (g_file_test (xml_path, G_FILE_TEST_EXISTS) && confirm (_("Duplicate Name"), _("A command of this name is already available in your custom menus; Overwrite?"))))
    {
      gchar *dirpath = g_path_get_dirname (xml_path);
      g_mkdir_with_parents (dirpath, 0770);
      g_free (dirpath);
      //g_file_set_contents(xml_path, text, -1, NULL);
      save_command_metadata (xml_path, myname, mylabel, mytooltip, idx < 0 ? NULL : after);
      save_command_data(scm_path, myscheme);
      load_xml_keymap (xml_path);
      GtkAction* action = lookup_action_from_name (myname);
      load_command_data (action);

      if (confirm (_("New Command Added"), _("Do you want to save this with your default commands?")))
        save_accels ();
    }
  else
    warningdialog (_("Operation cancelled"));
  g_free (myposition);
  return;
}



void
append_scheme_call (gchar * func)
{
  if (strcmp (func, "ExecuteScheme"))
    {
      GtkTextIter enditer;
      GtkTextBuffer *buffer = gtk_text_view_get_buffer ((GtkTextView *) (Denemo.ScriptView));
      //gtk_text_buffer_set_text(buffer,"",-1);
      gtk_text_buffer_get_end_iter (buffer, &enditer);
      gchar *text = g_strdup_printf ("(d-%s)\n", func); //prefix dnm_!!!!!!!
      gtk_text_buffer_insert (buffer, &enditer, text, -1);
      //g_debug("Added %s\n", text);
      g_free (text);
      if (Denemo.prefs.immediateplayback)
        play_note (DEFAULT_BACKEND, 0, 9, 58, 300, 127 * Denemo.project->si->master_volume);
    }
}




static void
button_choice_callback (GtkWidget * w, gboolean * left)
{
  g_debug ("left at %p is %d\n", left, *left);
  *left = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (w));
  g_debug ("left at %p is now %d\n", left, *left);
}

static void
button_move_callback (GtkWidget * w, mouse_gesture * g)
{
  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (w)))
    *g = GESTURE_MOVE;
  //g_debug("move %d\n", *g);
}

static void
button_press_callback (GtkWidget * w, mouse_gesture * g)
{
  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (w)))
    *g = GESTURE_PRESS;
  //g_debug("press  %d\n", *g);
}

static void
button_release_callback (GtkWidget * w, mouse_gesture * g)
{
  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (w)))
    *g = GESTURE_RELEASE;
  //g_debug("release %d \n", *g);
}



static void
button_modifier_callback (GtkWidget * w, GdkEventButton * event, ModifierAction * ma)
{
  ma->modnum = event->state;
  // show_type(w, "button mod callback: ");
  GString *str = g_string_new ("Keyboard:");
  append_modifier_name (str, ma->modnum);
  if (!ma->modnum)
    g_string_assign (str, _("No keyboard modifier keys\nPress with modifier key to change"));
  else
    g_string_append (str, _("\nPress with modifier key to change"));
  gtk_button_set_label (GTK_BUTTON (w), str->str);
  g_string_free (str, TRUE);
}

static void
mouse_shortcut_dialog (ModifierAction * info)
{
  GtkWidget *dialog = gtk_dialog_new_with_buttons (_("Set Mouse Shortcut"),
                                                   GTK_WINDOW (Denemo.window),
                                                   (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
                                                   GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
                                                   GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
                                                   NULL);


  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));

  GtkWidget *hbox = gtk_hbox_new (FALSE, 1);
  GtkWidget *vbox = gtk_vbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (content_area), hbox);
  gtk_container_add (GTK_CONTAINER (hbox), vbox);

  gchar *name = (gchar *) gtk_action_get_name (info->action);
  gchar *prompt = g_strdup_printf (_("Setting mouse shortcut for %s"), name);
  GtkWidget *label = gtk_label_new (prompt);
  g_free (prompt);
  gtk_box_pack_start (GTK_BOX (vbox), label, TRUE, TRUE, 0);
  GtkWidget *frame = gtk_frame_new (_("Choose the mouse button"));
  gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_IN);
  gtk_container_add (GTK_CONTAINER (vbox), frame);
  GtkWidget *vbox2 = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (frame), vbox2);

  info->left = TRUE;
  GtkWidget *widget = gtk_radio_button_new_with_label (NULL, _("Left"));
  g_signal_connect (G_OBJECT (widget), "toggled", G_CALLBACK (button_choice_callback), &info->left);
  gtk_box_pack_start (GTK_BOX (vbox2), widget, FALSE, TRUE, 0);
  GtkWidget *widget2 = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (widget), _("Right"));
  gtk_box_pack_start (GTK_BOX (vbox2), widget2, FALSE, TRUE, 0);


  frame = gtk_frame_new (_("Choose mouse action"));
  gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_IN);
  gtk_container_add (GTK_CONTAINER (vbox), frame);
  vbox2 = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (frame), vbox2);
  info->gesture = GESTURE_PRESS;
  widget = gtk_radio_button_new_with_label (NULL, _("Press Button"));
  g_signal_connect (G_OBJECT (widget), "toggled", G_CALLBACK (button_press_callback), &info->gesture);
  gtk_box_pack_start (GTK_BOX (vbox2), widget, FALSE, TRUE, 0);
  widget2 = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (widget), _("Release Button"));
  g_signal_connect (G_OBJECT (widget2), "toggled", G_CALLBACK (button_release_callback), &info->gesture);
  gtk_box_pack_start (GTK_BOX (vbox2), widget2, FALSE, TRUE, 0);
  widget2 = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (widget), _("Drag"));
  g_signal_connect (G_OBJECT (widget2), "toggled", G_CALLBACK (button_move_callback), &info->gesture);
  gtk_box_pack_start (GTK_BOX (vbox2), widget2, FALSE, TRUE, 0);

  widget = gtk_button_new_with_label (_("Hold Modifier Keys, Engage Caps or Num Lock\nand click here to set shorcut."));
  g_signal_connect (G_OBJECT (widget), "button-release-event", G_CALLBACK (button_modifier_callback), info);
  gtk_box_pack_start (GTK_BOX (vbox), widget, FALSE, TRUE, 0);

  gtk_container_add (GTK_CONTAINER (vbox), hbox);

  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      setMouseAction (info);
      Denemo.accelerator_status = TRUE;
    }
  gtk_widget_destroy (dialog);
}

static void
createMouseShortcut (GtkWidget * menu, GtkAction * action)
{
  static ModifierAction info;
  info.action = action;
  info.gesture = GESTURE_PRESS;
  info.modnum = 0;
  info.left = TRUE;
  mouse_shortcut_dialog (&info);
}

/* get init.scm for the current path into the scheme text editor.
*/
static void
get_initialization_script (GtkWidget * widget, gchar * directory)
{
  GError *error = NULL;
  gchar *script;
  g_debug ("loading %s/init.scm into Denemo.ScriptView\n", directory);
  gchar *filename = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "menus", directory, INIT_SCM, NULL);
  if (!g_file_test (filename, G_FILE_TEST_EXISTS))
    {
      g_free (filename);
      filename = g_build_filename (get_user_data_dir (TRUE), "download", COMMANDS_DIR, "menus", directory, INIT_SCM, NULL);
      if (!g_file_test (filename, G_FILE_TEST_EXISTS))
        {
          g_free (filename);
          filename = g_build_filename (get_system_data_dir (), COMMANDS_DIR, "menus", directory, INIT_SCM, NULL);
          if (!g_file_test (filename, G_FILE_TEST_EXISTS))
            {
              g_free (filename);
              return;
            }
        }
    }
  if (g_file_get_contents (filename, &script, NULL, &error))
    appendSchemeText (script);
  else
    g_warning ("Could not get contents of %s", filename);
  g_free (script);
  g_free (filename);
}

/* write scheme script from Denemo.ScriptView into file init.scm in the user's local menupath.
*/
static void
put_initialization_script (GtkWidget * widget, gchar * directory)
{
  gchar *filename = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "menus", directory, INIT_SCM, NULL);
  if ((!g_file_test (filename, G_FILE_TEST_EXISTS)) || confirm (_("There is already an initialization script here"), _("Do you want to replace it?")))
    {
      gchar *scheme = getSchemeText ();
      if (scheme && *scheme)
        {
          FILE *fp = fopen (filename, "w");
          if (fp)
            {
              fprintf (fp, "%s", scheme);
              fclose (fp);
              if (confirm (_("Wrote init.scm"), _("Shall I execute it now?")))
                call_out_to_guile (scheme);
            }
          else
            {
              warningdialog (_("Could not create init.scm;\n" "you must create your scripted menu item in the menu\n" "before you create the initialization script for it, sorry."));
            }
          g_free (scheme);
        }
    }
}

/* upload scripts for command/tag name.
Parameters: name the name of a command or a tag
            script the scheme script that the command runs, or an editscript for directives with tag name
        init_script the scheme script that is run before the command runs, not used for tags
        command the xml description of that command, or "" for tags
for tags:
   command is "" for an editscript and name is the tag for directives that the script edits
for commands:
   command is the command set file for merging the command as a new menu item
   the script is given in scheme and any initialization script for the menu is given in init_script
*/
static void
upload_scripts (gchar * name, gchar * script, gchar * init_script, gchar * command, gchar * menupath, gchar * label, gchar * tooltip, gchar * after)
{
  SCM func_symbol;
  SCM func;
  func_symbol = scm_c_lookup ("d-UploadRoutine");
  func = scm_variable_ref (func_symbol);
#define ARG(s) s?scm_from_locale_string(s):scm_from_locale_string("")
  SCM list = scm_list_n (ARG (command), ARG (name), ARG (script), ARG (init_script), ARG (menupath), ARG (label), ARG (tooltip), ARG (after), SCM_UNDEFINED);
  scm_call_1 (func, list);
#undef ARG
}



/* save the action (which must be a script),
   setting the script text to the script currently in the ScriptView
   The save is to the user's menu hierarchy on disk
*/
static void
saveMenuItem (GtkWidget * widget, GtkAction * action)
{
  gchar *name = (gchar *) gtk_action_get_name (action);
  gchar *menupath = g_object_get_data (G_OBJECT (action), "menupath");
  gint idx = lookup_command_from_name (Denemo.map, name);
  
  command_row* row = NULL;
  keymap_get_command_row (Denemo.map, &row, idx);
  
  gchar *tooltip = (gchar *) lookup_tooltip_from_idx (Denemo.map, idx);
  gchar *label = (gchar *) lookup_label_from_idx (Denemo.map, idx);
  
  gchar *xml_filename = g_strconcat (name, XML_EXT, NULL);
  gchar *xml_path = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "menus", menupath, xml_filename, NULL);
  g_free (xml_filename);

  gchar *scm_filename = g_strconcat (name, SCM_EXT, NULL);
  gchar *scm_path = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "menus", menupath, scm_filename, NULL);
  g_free (scm_filename);
  
  gchar *scheme = getSchemeText ();
  if (scheme && *scheme && confirm (_("Save Script"), g_strconcat (_("Over-write previous version of the script for "), name, _(" ?"), NULL)))
    {
      gchar *dirpath = g_path_get_dirname (xml_path);
      g_mkdir_with_parents (dirpath, 0770);
      g_free (dirpath);
      save_command_metadata (xml_filename, name, label, tooltip, row->after);
      save_command_data(scm_path, scheme);
      g_object_set_data (G_OBJECT (action), "scheme", (gpointer) "");
      load_command_data (action);
    }
  else
    warningdialog (_("No script saved"));
}


/* upload the action,
   from the user's menu hierarchy on disk, along with initialization script and menu item xml etc
*/
#ifdef UPLOAD_TO_DENEMO_DOT_ORG
static void
uploadMenuItem (GtkWidget * widget, GtkAction * action)
{
  gchar *name = (gchar *) gtk_action_get_name (action);
  gchar *menupath = g_object_get_data (G_OBJECT (action), "menupath");
  gint idx = lookup_command_from_name (Denemo.map, name);
  
  command_row* row = NULL;
  keymap_get_command_row (the_keymap, &row, idx);
  gchar *tooltip = (gchar *) lookup_tooltip_from_idx (Denemo.map, idx);
  gchar *label = (gchar *) lookup_label_from_idx (Denemo.map, idx);

  gchar *filename = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "menus", menupath, name,
                                      NULL);
  gchar *script = g_object_get_data (G_OBJECT (action), "scheme");
  gchar *xml;
  GError *error = NULL;
  g_file_get_contents (filename, &xml, NULL, &error);
  filename = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "menus", menupath, INIT_SCM, NULL);
  gchar *init_script;
  g_file_get_contents (filename, &init_script, NULL, &error);

  if (xml == NULL)
    xml = "";
  if (init_script == NULL)
    init_script = "";
  if (script == NULL)
    script = "";


  upload_scripts (name, script, init_script, xml, menupath, label, tooltip, row->after);

}
#endif

/* upload editscript for tag */
void
upload_edit_script (gchar * tag, gchar * script)
{
  upload_scripts (tag, script, "", "", "", "", "", "");
}


static const gchar *
locatebitmapsdir (void)
{
  static gchar *bitmapsdir = NULL;
  gboolean err;
  if (!bitmapsdir)
    {
      bitmapsdir = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "bitmaps", NULL);
    }
  err = g_mkdir_with_parents (bitmapsdir, 0770);
  if (err)
    {
      warningdialog (_("Could not create .denemo/actions/bitmaps for your graphics for customized commands"));
      g_free (bitmapsdir);
      bitmapsdir = g_strdup ("");       //FIXME
    }
  return bitmapsdir;
}

static const gchar *
locatedownloadbitmapsdir (void)
{
  static gchar *bitmapsdir = NULL;
  if (!bitmapsdir)
    {
      bitmapsdir = g_build_filename (get_user_data_dir (TRUE), "download", COMMANDS_DIR, "bitmaps", NULL);
    }
  return bitmapsdir;
}

/* if a graphic file for name exists (local or downloaded or systemwide) create an icon for it called label
and return label, else return NULL
*/
gchar *
get_icon_for_name (gchar * name, gchar * label)
{
  return NULL;                  //this function is disabled until we figure out why the icons are no longer being shown.
#ifdef PROBLEM_SHOWING_ICONS_FIXED
  gchar *pngname = g_strconcat (name, ".png", NULL);
  gchar *filename = g_build_filename (locatebitmapsdir (), pngname,
                                      NULL);
  if (!g_file_test (filename, G_FILE_TEST_EXISTS))
    {
      g_free (filename);
      filename = g_build_filename (locatedownloadbitmapsdir (), pngname, NULL);
      if (!g_file_test (filename, G_FILE_TEST_EXISTS))
        {
          g_free (filename);
          filename = g_build_filename (get_system_data_dir (), COMMANDS_DIR, "bitmaps", pngname, NULL);
          if (!g_file_test (filename, G_FILE_TEST_EXISTS))
            {
              g_free (filename);
              g_free (pngname);
              return NULL;
            }
        }
    }
  GError *error = NULL;
  GdkPixbuf *pixbuf = gdk_pixbuf_new_from_file (filename, &error);
  g_free (filename);
  g_free (pngname);
  if (error)
    {
      warningdialog (error->message);
      return NULL;
    }
  static GtkIconFactory *icon_factory;
  if (!icon_factory)
    {
      icon_factory = gtk_icon_factory_new ();
      gtk_icon_factory_add_default (icon_factory);
    }
  GtkIconSet *icon_set = gtk_icon_set_new_from_pixbuf (pixbuf);
  g_object_unref (pixbuf);
  gtk_icon_factory_add (icon_factory, label, icon_set);
  return label;
#endif
}



gchar *
create_xbm_data_from_pixbuf (GdkPixbuf * pixbuf, int lox, int loy, int hix, int hiy)
{
  RETURN_IF_NON_INTERACTIVE (NULL);
  int width, height, rowstride, n_channels;
  guchar *pixels;

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

  char *chars = g_malloc0 (sizeof (char) * width * height);     //about 8 times too big!
  char *this = chars;
  for (i = 0, y = loy; y < hiy; y++)
    {
      for (x = lox; x < hix; x++, i++)
        {
          this = chars + (i / 8);
          gint set = ((pixels + y * rowstride + x * n_channels)[3] > 0);
#ifdef G_OS_WIN32
          set = (set ? 0 : 1);  //bizarrely the bitmaps come out inverted on windows
#endif

          *this += set << i % 8;
        }
      i = ((i + 7) / 8) * 8;
    }
  return chars;
}

static GHashTable *bitmaps;
static void
bitmap_table_insert (gchar * name, DenemoGraphic * xbm)
{
  if (!bitmaps)
    bitmaps = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);  //FIXME is this right for GdkBitmap data?
  g_hash_table_insert (bitmaps, g_strdup (name), xbm);  //FIXME
}

static DenemoGraphic *
create_bitmap_from_data (gchar * data, gint width, gint height)
{
#if 0
/*   static GdkColor white, black;gboolean init = FALSE; */
/*   if(!init) { */
/*     gdk_color_parse ("white", &white); */
/*     gdk_colormap_alloc_color (gdk_colormap_get_system (), &white, TRUE, TRUE); */
/*     gdk_color_parse ("black", &black); */
/*     gdk_colormap_alloc_color (gdk_colormap_get_system (), &black, TRUE, TRUE); */
/*   } */
  // return gdk_pixmap_create_from_data(NULL,data,  width, height, 1, &white, &black);
  //return gdk_bitmap_create_from_data(NULL,data,  width, height);
  cairo_surface_t *surface = cairo_image_surface_create_for_data (data, CAIRO_FORMAT_A1, width, height,
                                                                  cairo_format_stride_for_width (CAIRO_FORMAT_A1, width));
  cairo_pattern_t *pattern = cairo_pattern_create_for_surface (surface);
  cairo_pattern_reference (pattern);
  return pattern;
#else
  return NULL;
#endif
}

static gboolean
loadGraphicFromFormat (gchar * basename, gchar * name, DenemoGraphic ** xbm)
{
  RsvgDimensionData thesize;
  gchar *filename = g_strconcat (name, ".png", NULL);
  thesize.width = 40;
  thesize.height = 40;
  cairo_surface_t *surface = cairo_image_surface_create_from_png (filename);
  if (cairo_surface_status (surface) != CAIRO_STATUS_SUCCESS)
    {
      g_free (filename);
      filename = g_strconcat (name, ".svg", NULL);
      if (g_file_test (filename, G_FILE_TEST_EXISTS))
        {
#ifdef CAIRO_HAS_SVG_SURFACE
          GError *error = NULL;
          RsvgHandle *handle = rsvg_handle_new_from_file (filename, &error);
          if (handle == NULL)
            {
              if (error)
                g_warning ("Could not open %s error %s", basename, error->message);
              else
                g_warning ("Opening %s, Bug in librsvg:rsvg handle null but no error message", basename);
              return FALSE;
            }

          rsvg_handle_get_dimensions (handle, &thesize);
          surface = cairo_svg_surface_create_for_stream (NULL, NULL, (double) (thesize.width), (double) (thesize.height));
          cairo_t *cr = cairo_create (surface);
          rsvg_handle_render_cairo (handle, cr);
          rsvg_handle_close (handle, NULL);
          g_object_unref (handle);
          cairo_destroy (cr);
#else
          g_warning ("Cairo svg backend not available");
          return FALSE;
#endif
        }
    }
  else
    {
      FILE *fp = fopen (filename, "rb");
      if (fp)
        {
          fseek (fp, 16, SEEK_SET);
          fread (&thesize.width, 4, 1, fp);
          fread (&thesize.height, 4, 1, fp);
          thesize.width = GINT_FROM_BE (thesize.width);
          thesize.height = GINT_FROM_BE (thesize.height);
          fclose (fp);
        }
    }
  g_free (filename);
  if (cairo_surface_status (surface) == CAIRO_STATUS_SUCCESS)
    {
      cairo_pattern_t *pattern = cairo_pattern_create_for_surface (surface);
      cairo_pattern_reference (pattern);

      DenemoGraphic *graphic = g_malloc (sizeof (DenemoGraphic));
      graphic->type = DENEMO_PATTERN;
      graphic->width = thesize.width;
      graphic->height = thesize.height;
      //g_debug("size %d x %d", thesize.width, thesize.height);

      graphic->graphic = pattern;
      bitmap_table_insert (basename, graphic);
      *xbm = graphic;
      return TRUE;
    }
  else
    return FALSE;
}


gboolean
loadGraphicItem (gchar * name, DenemoGraphic ** xbm)
{

  if (!name || !*name)
    return FALSE;
#define NEWLINE "\n"
  if (*name == *NEWLINE)
    {
//if name starts '\n' treat it as lines holding char font size weight (e.g. bold) slant (e.g. italic) 
//so let user specify a hex value and convert to utf8 here len = g_unichar_to_utf8( uc, utf_string );
//e.g "\n0x20" would be glyph 0x20 from the feta26 font, size 35 not bold or italic (as in  drawfetachar_cr())
//while "\n0x40 0x40\nSans\n16\n1\n1" would be a "AA" string in sans font at 16pt bold and italic
      gchar **spec = g_strsplit (name + 1, NEWLINE, 5);
      gint i;
      DenemoGraphic *graphic = g_malloc (sizeof (DenemoGraphic));
      DenemoGlyph *glyph = (DenemoGlyph *) g_malloc (sizeof (DenemoGlyph));
      graphic->type = DENEMO_FONT;
      graphic->graphic = glyph;
      glyph->fontname = "feta26";
      glyph->size = 35.0;
      for (i = 0; i < 5 && spec[i]; i++)
        {
          switch (i)
            {
            case 0:
              {
                // get a set of hex values (unicodes?) and create a utf8 string
                //should involve strtol(spec[0], &next, 0);
                //and perhaps g_unichar_to_utf8(*spec[0], NULL);
                // and glyph->utf = g_malloc(len);
                //if not hex digits, then treat as utf8 string
                glyph->utf = g_strdup (spec[0]);
                break;
              }
            case 1:
              glyph->fontname = g_strdup (spec[1]);
              break;
            case 2:
              glyph->size = g_ascii_strtod (spec[2], NULL);
              break;
            case 3:
              glyph->weight = atoi (spec[3]);
              break;
            case 4:
              glyph->slant = atoi (spec[4]);
              break;
            }
        }
      g_strfreev (spec);
      *xbm = graphic;
      return TRUE;
    }
  if (bitmaps && (*xbm = (DenemoGraphic *) g_hash_table_lookup (bitmaps, name)))
    {
      return TRUE;
    }

  gchar* files[] = {
    g_strconcat (name, ".png", NULL),
    g_strconcat (name, ".svg", NULL),
    NULL
  };
  
  gchar* dirs[] = {
    g_build_filename (locatebitmapsdir (), NULL),
    g_build_filename (locatedownloadbitmapsdir (), NULL),
    g_build_filename (PACKAGE_SOURCE_DIR, COMMANDS_DIR, "bitmaps", NULL),
    g_build_filename (get_system_data_dir (), COMMANDS_DIR, "bitmaps", NULL),
    NULL
  };

  gboolean success = TRUE;
  gchar *dir = find_dir_for_files (files, dirs);
  if(!dir){
    g_warning ("Could not find graphic item %s", name);
    success = FALSE;
  }

  else{
    gchar* basename = g_build_filename(dir, name, NULL);
    success = loadGraphicFromFormat (name, basename, xbm);
    g_free(basename);

    if(!success)
      g_warning ("Could not load graphic item %s from %s", name, dir);
  }
  
  g_free(dir);

  return success;
}

/* save the current graphic
*/
static void
saveGraphicItem (GtkWidget * widget, GtkAction * action)
{
  gchar *name = (gchar *) gtk_action_get_name (action);
  gchar *pngname = g_strconcat (name, ".png", NULL);
  gchar *filename = g_build_filename (locatebitmapsdir (), pngname,
                                      NULL);
  //FIXME allow fileselector here to change the name
  gchar *msg = g_strdup_printf (_("Saving a graphic for use in the %s script"), name);
  if (!g_file_test (filename, G_FILE_TEST_EXISTS) || confirm (msg, _("Replace current graphic?")))
    {
      guint width = Denemo.project->xbm_width;
      guint height = Denemo.project->xbm_height;


      /*DenemoGraphic *bitmap = */ create_bitmap_from_data (Denemo.project->xbm, width, height);

#if 0

      //  GdkBitmap *bitmap = gdk_bitmap_create_from_data(NULL, Denemo.project->xbm, width, height);

      static GdkColor white, black;
      gboolean init = FALSE;
      if (!init)
        {
          gdk_color_parse ("white", &white);
          gdk_colormap_alloc_color (gdk_colormap_get_system (), &white, TRUE, TRUE);
          gdk_color_parse ("black", &black);
          gdk_colormap_alloc_color (gdk_colormap_get_system (), &black, TRUE, TRUE);
        }
      // GdkBitmap *bitmap = gdk_pixmap_create_from_data(NULL, Denemo.project->xbm, width, height, 1, &white, &black);
      GdkBitmap *bitmap = gdk_pixmap_create_from_data (NULL, Denemo.project->xbm, width, height, 1, &black, &white);

      g_debug ("pixmap create");

#endif
#if 0
      //#if GTK_MAJOR_VERSION==3
      //  GdkPixbuf *pixbuf1 = gdk_pixbuf_get_from_window (NULL,  bitmap, NULL, 0,0,0,0, width, height);
      //#else
      //FIXME  GdkPixbuf *pixbuf1 = gdk_pixbuf_get_from_drawable (NULL,  bitmap, NULL, 0,0,0,0, width, height);
      //#endif
      GdkPixbuf *pixbuf = gdk_pixbuf_add_alpha (pixbuf1, TRUE, 0, 0, 0);        // 255, 255, 255);

      guchar *pixels;
      gint n_channels = gdk_pixbuf_get_n_channels (pixbuf);
      g_assert (gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);
      g_assert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);
      g_assert (gdk_pixbuf_get_has_alpha (pixbuf));
      g_assert (n_channels == 4);
      gint rowstride = gdk_pixbuf_get_rowstride (pixbuf);
      pixels = gdk_pixbuf_get_pixels (pixbuf);
      int x, y, i;
      for (i = 0, y = 0; y < height; y++)
        {
          for (x = 0; x < width; x++, i++)
            {
              gint set = !((pixels + y * rowstride + x * n_channels)[3] > 0);
              (pixels + y * rowstride + x * n_channels)[0] = 0xFF * set;
              (pixels + y * rowstride + x * n_channels)[1] = 0xFF * set;
              (pixels + y * rowstride + x * n_channels)[2] = 0xFF * set;
            }
        }


      gdk_pixbuf_save (pixbuf, filename, "png", &error, "compression", "2", NULL);


#if 0
      FILE *fp = fopen (filename, "wb");
      if (fp)
        {
          guchar whi, wlo, hhi, hlo;
          wlo = width & 0xFF;
          whi = width >> 8;
          hlo = height & 0xFF;
          hhi = height >> 8;

          fwrite (&wlo, 1, 1, fp);
          fwrite (&whi, 1, 1, fp);

          fwrite (&hlo, 1, 1, fp);
          fwrite (&hhi, 1, 1, fp);

          gint size = fwrite (Denemo.project->xbm, 1, height * ((width + 7) / 8) * 8, fp);
          //g_debug("Wrote %d bytes for %d x %d\n", size, width, height);


          g_free (msg);
          msg = g_strdup_printf ("Saved graphic as file %s", filename);
          infodialog (msg);
          fclose (fp);
        }
      else
        warningdialog ("Could not write file");
#endif
#endif
    }

  g_free (pngname);
  g_free (msg);
  g_free (filename);
}

/* return a directory path for a system menu ending in menupath, or NULL if none exists
   checking user's download then the installed menus
   user must free the returned string*/
static gchar *
get_system_menupath (gchar * menupath)
{
  gchar *filepath = g_build_filename (get_user_data_dir (TRUE), "download", COMMANDS_DIR, "menus", menupath, NULL);
  //g_debug("No file %s\n", filepath);
  if (0 != g_access (filepath, 4))
    {
      g_free (filepath);
      filepath = g_build_filename (get_system_data_dir (), COMMANDS_DIR, "menus", menupath, NULL);
    }
  return filepath;
}


/*
  menu_click:
  intercepter for the callback when clicking on menu items for the set of Actions the Denemo offers.
  Left click runs default action, after recording the item in a scheme script if recording.
  Right click offers pop-up menu for setting shortcuts etc

*/
static gboolean
menu_click (GtkWidget * widget, GdkEventButton * event, GtkAction * action)
{
  keymap *the_keymap = Denemo.map;
  const gchar *func_name = gtk_action_get_name (action);
  //g_debug("widget name %s action name %s accel path %s\n", gtk_widget_get_name(widget), func_name, gtk_action_get_accel_path (action));

  // GSList *h = gtk_action_get_proxies (action);
  //g_debug("In menu click action is %p h is %p\n",action, h);



  gint idx = lookup_command_from_name (the_keymap, func_name);
  //g_debug("event button %d, idx %d for %s recording = %d scm = %d\n", event->button, idx, func_name, Denemo.ScriptRecording,g_object_get_data(G_OBJECT(action), "scm") );
  if (event->button != 3)       //Not right click
    if (Denemo.ScriptRecording)
      if (idx_has_callback (the_keymap, idx))
        {
          append_scheme_call ((gchar *) func_name);
        }

  if (event->button != 3)
    return FALSE;


#if 0
  /* This idx is -1 for the toggles and radio entries because they share a callback function. If we want to allow setting keybindings, getting help etc. for these then we would need to re-work all the radio action entries code using generate_source.c. Instead at the moment we have just defined scheme callback functions d-EditMode etc. using a hand-created array activatable_commands earlier in this file.
     It is also for menus themselves, so we process the case further. */
  if (idx == -1)
    return TRUE;
#endif

  GtkWidget *menu = gtk_menu_new ();
  gchar *labeltext = g_strdup_printf ("Help for %s", func_name);
  GtkWidget *item = gtk_menu_item_new_with_label (labeltext);
  g_free (labeltext);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (popup_help), (gpointer) action);

 /* Place button in palette */

  item = gtk_menu_item_new_with_label (_("Place Command in a Palette"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (placeInPalette), action);


  /* "drag" menu item onto button bar */

  item = gtk_menu_item_new_with_label (_("Place Command on the Title Bar"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (placeOnButtonBar), action);


  if (idx != -1)
    {
      item = gtk_menu_item_new_with_label (_("Create Mouse Shortcut"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (createMouseShortcut), action);
      item = gtk_menu_item_new_with_label (_("Edit Shortcuts\nSet Mouse Pointers\nHide/Delete Menu Item"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (configure_keyboard_idx), GINT_TO_POINTER (idx));
      item = gtk_menu_item_new_with_label (_("Save Command Set"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (save_default_keymap_file), action);


      item = gtk_separator_menu_item_new ();
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
    }                           //idx!=-1

  gchar *myposition = g_object_get_data (G_OBJECT (widget), "menupath");        // applies if it is a built-in command: FIXME not set for the popup menus though
  //g_debug("position from built in is %s\n", myposition);
  if (!myposition)
    myposition = g_object_get_data (G_OBJECT (action), "menupath");     //menu item runs a script
  //g_debug("Connecting to %s\n", g_object_get_data(G_OBJECT(widget), "menupath"));

  //g_debug("position is %s\n", myposition);
  if (myposition == NULL)
    {
      // g_warning("Cannot find the position of this menu item %s in the menu system", func_name);
      return TRUE;
    }
  static gchar *filepath;       // static so that we can free it next time we are here.
  if (filepath)
    g_free (filepath);
  filepath = get_system_menupath (myposition);
  if (0 == g_access (filepath, 4))
    {
      //g_debug("We can look for a menu item in the path %s\n", filepath);
      item = gtk_menu_item_new_with_label ("More Commands");
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (load_command_from_location), (gpointer) filepath);
    }

  if (!is_action_name_builtin(func_name))
    {
      gchar *scheme = g_object_get_data (G_OBJECT (action), "scheme");
      if (!scheme || !*scheme)
        scheme = load_command_data (action);
      if (!scheme)
        g_warning ("Could not get script for %s", gtk_action_get_name (action));
      else
        {
          item = gtk_menu_item_new_with_label (_("Get Script"));
          gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
          g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (appendSchemeText_cb), scheme);
        }
      item = gtk_menu_item_new_with_label (_("Save Script"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (saveMenuItem), action);
      if (Denemo.project->xbm)
        {
          //item = gtk_menu_item_new_with_label (_("Save Graphic"));
          // GtkSettings* settings = gtk_settings_get_default();
          // gtk_settings_set_long_property  (settings,"gtk-menu-images",(glong)TRUE, "XProperty");
          //item = gtk_image_menu_item_new_from_stock("Save Graphic", gtk_accel_group_new());
          item = gtk_image_menu_item_new_from_stock (_("Save Graphic") /*GTK_STOCK_OK */ , NULL);

          gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
          g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (saveGraphicItem), action);
        }
#ifdef UPLOAD_TO_DENEMO_DOT_ORG
      item = gtk_menu_item_new_with_label (_("Upload this Script to denemo.org"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (uploadMenuItem), action);
#endif
    }

  {
    gboolean sensitive = gtk_widget_get_visible (gtk_widget_get_toplevel (Denemo.ScriptView));
    item = gtk_menu_item_new_with_label (_("Save Script as New Menu Item"));
    gtk_widget_set_sensitive (item, sensitive);
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
    static gchar *insertion_point;
    if (insertion_point)
      g_free (insertion_point);
    insertion_point = g_build_filename (myposition, func_name, NULL);
    //g_debug("using %p %s for %d %s %s\n", insertion_point, insertion_point, idx, myposition, func_name);
    g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (insertScript), insertion_point);



    /* options for getting/putting init.scm */

    item = gtk_menu_item_new_with_label (_("Get Initialization Script for this Menu"));
    gtk_widget_set_sensitive (item, sensitive);

    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
    g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (get_initialization_script), myposition);

    item = gtk_menu_item_new_with_label (_("Put Script as Initialization Script for this Menu"));
    gtk_widget_set_sensitive (item, sensitive);

    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
    g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (put_initialization_script), myposition);

  }

  /* a check item for showing script window */
  item = gtk_check_menu_item_new_with_label (_("Show Current Script"));
  gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (item), gtk_widget_get_visible (gtk_widget_get_toplevel (Denemo.ScriptView)));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  //FIXME the next statement triggers a warning that ToggleScript is not a registered denemo commad - correct, since we do not make the toggles available as commands since using such a command would make the check boxes out of step, instead we install function that activate the menuitem.
  gtk_activatable_set_related_action (GTK_ACTIVATABLE (item), gtk_ui_manager_get_action (Denemo.ui_manager, "/MainMenu/ViewMenu/ToggleScript"));
  //gtk_action_connect_proxy(gtk_ui_manager_get_action (Denemo.ui_manager, "/MainMenu/ViewMenu/ToggleScript"), item);

  gtk_widget_show_all (menu);
  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
  // configure_keyboard_dialog_init_idx (action, project, idx);
  return TRUE;
}



static void
color_rhythm_button (RhythmPattern * r, const gchar * color)
{
  RETURN_IF_NON_INTERACTIVE ();
  if (r == NULL)
    return;
  GdkColor thecolor;
  gdk_color_parse (color, &thecolor);
  gtk_widget_modify_fg (gtk_tool_button_get_label_widget (GTK_TOOL_BUTTON (r->button)), GTK_STATE_NORMAL, &thecolor);
  //bg does not work, and setting the label in a GtkEvent box gave a problem on some build - R.Rankin patched for this and so we have to use fg
}

void
highlight_rhythm (RhythmPattern * r)
{
  //g_debug("highlight\n");
  color_rhythm_button (r, "black");
}

void
unhighlight_rhythm (RhythmPattern * r)
{
  //g_debug("Unhighlight\n");
  color_rhythm_button (r, "gray");
}


/*
 

  
*/
void
highlight_rest (DenemoProject * project, gint dur)
{
  //g_debug("highlight rest");
  RETURN_IF_NON_INTERACTIVE();

  if (project->currhythm)
    {
      unhighlight_rhythm ((RhythmPattern *) project->currhythm->data);
    }
  project->currhythm = NULL;
  project->cstep = NULL;
  project->rstep = Denemo.singleton_rhythms['r' + dur]->rsteps;
  unhighlight_rhythm (project->prevailing_rhythm);
  project->prevailing_rhythm = Denemo.singleton_rhythms['r' + dur];
  highlight_rhythm (project->prevailing_rhythm);

}

void
highlight_duration (DenemoProject * project, gint dur)
{
  //g_debug("higlight duration");
  RETURN_IF_NON_INTERACTIVE();

  if (project->currhythm)
    {
      unhighlight_rhythm ((RhythmPattern *) project->currhythm->data);
    }
  project->currhythm = NULL;
  project->cstep = NULL;
  project->rstep = Denemo.singleton_rhythms['0' + dur]->rsteps;
  unhighlight_rhythm (project->prevailing_rhythm);
  project->prevailing_rhythm = Denemo.singleton_rhythms['0' + dur];
  highlight_rhythm (project->prevailing_rhythm);
}




/*
 * delete a rhythmic pattern and its button
 * 
 */
void
delete_rhythm_cb (GtkAction * action, DenemoScriptParam* param)
{
  DenemoProject *project = Denemo.project;
  if ((project->mode & (INPUTEDIT)) == 0)
    return;
  if (project->currhythm == NULL)
    return;
  RhythmPattern *r = (RhythmPattern *) project->currhythm->data;

  free_clipboard (r->clipboard);
  r->clipboard = NULL;
  if (r->name)
    {
      gchar *command = g_strdup_printf ("(define Snippet::%s 0)", r->name);
      call_out_to_guile (command);
      g_free (command);
    }

  gtk_widget_destroy (GTK_WIDGET (r->button));
  /* list is circular, so before we free it we have to break it */
  r->rsteps->prev->next = NULL;
  r->rsteps->prev = NULL;
  GList *g;
  for (g = r->rsteps; g; g = g->next)
    g_free (g->data);
  g_list_free (r->rsteps);
  g_free (r);
  //g_debug("length %d\n", g_list_length(project->rhythms));
  project->rhythms = g_list_remove (project->rhythms, project->currhythm->data);
  //g_debug("length %d %p\n", g_list_length(project->rhythms), project->rhythms);
  project->currhythm = g_list_last (project->rhythms);

  if (project->currhythm == NULL)
    {
      project->rstep = NULL;
      project->cstep = NULL;
    }
  else
    {
      highlight_rhythm (project->currhythm->data);
      project->rstep = ((RhythmPattern *) project->currhythm->data)->rsteps;
      project->cstep = ((RhythmPattern *) project->currhythm->data)->clipboard->data;
    }
  update_scheme_snippet_ids ();
}



/*
 * workaround for glib<2.10
 */
/* UNUSED
static void
attach_action_to_widget (GtkWidget * widget, GtkAction * action, DenemoProject * project)
{
  g_object_set_data (G_OBJECT (widget), "action", action);
}
*/
/* attaches a button-press-event signal to the widget with the action as data
   for use in the callback */
static void
attach_right_click_callback (GtkWidget * widget, GtkAction * action)
{
  RETURN_IF_NON_INTERACTIVE ();


  gtk_widget_add_events (widget, (GDK_BUTTON_PRESS_MASK));      //will not work because label are NO_WINDOW
  g_signal_connect (G_OBJECT (widget), "button-release-event", G_CALLBACK (menu_click), action);

  //g_debug("menu click set on %s GTK_WIDGET_FLAGS %x\n", gtk_action_get_name(action), GTK_WIDGET_FLAGS(widget));
  //show_type(widget, "Type is ");

  g_object_set_data (G_OBJECT (action), "signal_attached", action);     //Non NULL to indicate the signal is attached
}


/* UNUSED
static void
dummy (void)
{
  call_out_to_guile ("(d-Insert2)");
  call_out_to_guile ("(d-Insert2)");
  call_out_to_guile ("(d-Insert2)");
  call_out_to_guile ("(d-Insert2)");
  call_out_to_guile ("(d-Insert2)");
  return;
}
*/
/**
 * Menu entries with no shortcut keys, tooltips, and callback functions
 */
GtkActionEntry menu_entries[] = {
#include "generated/entries.h"
  {"Browse", NULL, N_("Browse"), NULL, N_("Opens a dialog for a new file"), G_CALLBACK (file_open_with_check)}

};

//Get number of menu entries
//gint n_menu_items = G_N_ELEMENTS (menu_entries);

static GtkWidget *
get_edit_menu_for_mode (gint mode)
{
  return NULL;
  if (mode & INPUTEDIT)
    return Denemo.EditModeMenu;
  if (mode & INPUTINSERT)
    return Denemo.InsertModeMenu;
  if (mode & INPUTCLASSIC)
    return Denemo.ClassicModeMenu;
  return Denemo.ModelessMenu;
}

/**
 *  callback changing mode  project->mode
 *
 */
static void
change_mode (GtkRadioAction * action, GtkRadioAction * current)
{
  DenemoProject *project = Denemo.project;
  gint val = gtk_radio_action_get_current_value (current);
  GtkWidget *menu = get_edit_menu_for_mode (project->mode);
  if (menu)
    gtk_widget_hide (menu);
  project->mode = ((project->mode & MODE_MASK) | val);
  menu = get_edit_menu_for_mode (project->mode);
  if (menu)
    gtk_widget_show (menu);
  write_status (project);

}


GtkAction *
activate_action (gchar * path)
{
  RETURN_IF_NON_INTERACTIVE (NULL);
  GtkAction *a;
  a = gtk_ui_manager_get_action (Denemo.ui_manager, path);
  if (a)
    gtk_action_activate (a);
  else
    g_warning ("No command at %s - should this be in denemoui.xml?", path);
  return a;
}

/**
 *  callback changing the input source (keyboard only/audio/midi)
 *
 */

static void
change_input_type (GtkRadioAction * action, GtkRadioAction * current)
{
  DenemoProject *project = Denemo.project;

  gint val = gtk_radio_action_get_current_value (current);
  gboolean fail = FALSE;
  if (project->notsaved)
    {
      warningdialog (_("You have unsaved work. Hardware problems may cause the program to exit during this task.\nPlease save first."));
      gtk_radio_action_set_current_value (current, project->input_source);
      return;
    }
  switch (val)
    {
    case INPUTKEYBOARD:
      if (project->input_source == INPUTAUDIO)
        {
          //g_debug("Stopping audio\n");
          stop_pitch_input ();
        }
      if (project->input_source == INPUTMIDI)
        {
          //g_debug("Stopping midi\n");
          stop_pitch_input ();
        }
      project->input_source = INPUTKEYBOARD;
      Denemo.project->last_source = INPUTKEYBOARD;
      g_debug ("Input keyboard %d", Denemo.project->last_source);
      break;
    case INPUTAUDIO:
      //g_debug("Starting audio\n");
      if (project->input_source == INPUTMIDI)
        {
          //g_debug("Stopping midi\n");
          stop_pitch_input ();
        }
      project->input_source = INPUTAUDIO;
      if (setup_pitch_input ())
        {
          fail = TRUE;
          warningdialog (_("Could not start Audio input"));
          gtk_radio_action_set_current_value (current, INPUTKEYBOARD);
        }
      else
        start_pitch_input ();
      break;
    case INPUTMIDI:
      midi_stop ();
      audio_shutdown ();
      (void)audio_initialize (&Denemo.prefs);
      if(have_midi())
        project->input_source = INPUTMIDI;
      else
        fail = TRUE;
      break;
    default:
      g_warning ("Bad Value");
      break;
    }
  if (fail)
    {
      project->input_source = INPUTKEYBOARD;
      gtk_radio_action_set_current_value (current, INPUTKEYBOARD);
    }
  else
    write_input_status ();
}

/**
 *  callback changing type of entry part of project->mode,
 * depending on the entry type it switches mode part of project->mode to Classic mode for entering rests and to Insert for entering notes. FIXME could switch to prefs value.
 *
 */
static void
change_entry_type (GtkRadioAction * action, GtkRadioAction * current)
{
  DenemoProject *project = Denemo.project;
  gint val = gtk_radio_action_get_current_value (current);
  switch (val)
    {
#define SET_MODE(m)  (project->mode=((project->mode&ENTRY_TYPE_MASK)|m))
    case INPUTREST:
      SET_MODE (INPUTREST);
      activate_action ("/MainMenu/ModeMenu/ClassicMode");

      break;
    case INPUTNORMAL:
      SET_MODE (INPUTNORMAL);
      activate_action ("/MainMenu/ModeMenu/InsertMode");
      break;
    case INPUTBLANK:
      SET_MODE (INPUTBLANK);
      activate_action ("/MainMenu/ModeMenu/ClassicMode");
      break;
    case INPUTRHYTHM | INPUTNORMAL:
      SET_MODE (INPUTRHYTHM | INPUTNORMAL);
      activate_action ("/MainMenu/ModeMenu/EditMode");
      break;
    }
#undef SET_MODE

  write_status (project);
  //g_debug("Mode is %x masks %x %x\n",ENTRY_TYPE_MASK, MODE_MASK, project->mode);
}

/* callback: if not Insert mode set Insert mode else set Edit mode */
void
toggle_edit_mode (GtkAction * action, DenemoScriptParam* param)
{
  DenemoProject *project = Denemo.project;
  static gint mode = INPUTINSERT;
  if (project->mode & INPUTEDIT)
    {
      switch (mode & ~MODE_MASK)
        {
        case INPUTINSERT:
          activate_action ("/MainMenu/ModeMenu/InsertMode");
          break;
        case INPUTCLASSIC:
          activate_action ("/MainMenu/ModeMenu/ClassicMode");
          break;
        case 0:
          activate_action ("/MainMenu/ModeMenu/Modeless");
          break;
        default:
          ;
        }
    }
  else
    {
      mode = project->mode;         // remember mode for switching back
      activate_action ("/MainMenu/ModeMenu/EditMode");
    }
}

/* callback: if rest entry make note entry and vv */
void
toggle_rest_mode (GtkAction * action, DenemoScriptParam* param)
{
  DenemoProject *project = Denemo.project;
  static gint mode = INPUTNORMAL;
  if (project->mode & INPUTREST)
    {
      switch (mode & ~ENTRY_TYPE_MASK)
        {
        case INPUTNORMAL:
          activate_action ("/MainMenu/ModeMenu/Note");
          break;
        case INPUTBLANK:
          activate_action ("/MainMenu/ModeMenu/Blank");
          break;
        default:
          ;
        }
    }
  else
    {
      mode = project->mode;         // remember mode for switching back
      activate_action ("/MainMenu/ModeMenu/Rest");
    }
}


/* callback: if rhythm entry make note entry and vv */
void
toggle_rhythm_mode (GtkAction * action, DenemoScriptParam* param)
{
  DenemoProject *project = Denemo.project;
#if 1
  //g_debug("Was mode %x\n", project->mode);
  if (project->mode & INPUTRHYTHM)
    project->mode &= ~INPUTRHYTHM;
  else
    {
      project->mode |= INPUTRHYTHM;
      activate_action ("/MainMenu/ModeMenu/EditMode");
    }
  //g_debug("Now mode %x\n", project->mode);
#else
  static gint mode = INPUTNORMAL;
  if (project->mode & INPUTRHYTHM)
    {
      switch (mode & ~ENTRY_TYPE_MASK)
        {
        case INPUTNORMAL:
          activate_action ("/MainMenu/ModeMenu/Note");
          break;
        default:
          ;
        }
    }
  else
    {
      mode = project->mode;         // remember mode for switching back, breaks with multi project FIXME
      activate_action ("/MainMenu/ModeMenu/Rhythm");
    }
#endif
}

/**
 *  Function to toggle the visibility of the LilyPond text window. It refreshes 
 *  the text if needed
 */
static void
toggle_lilytext (GtkAction * action, gpointer param)
{
  DenemoProject *project = Denemo.project;
  //if(!project->textview)
  refresh_lily_cb (action, project);
  if (!gtk_widget_get_visible (Denemo.textwindow))
    gtk_widget_show /*_all*/ (Denemo.textwindow);
  else
    gtk_widget_hide (Denemo.textwindow);
  //g_debug("toggling lily window");
}


/**
 *  Function to toggle the visibility of the Scheme text window. 
 */
static void
toggle_scheme (GtkAction * action, gpointer param)
{
  GtkWidget *textwindow = gtk_widget_get_toplevel (Denemo.ScriptView);
  if (!gtk_widget_get_visible (textwindow))
    gtk_widget_show_all (textwindow);
  else
    gtk_widget_hide (textwindow);
  //g_debug("toggling scheme window");
}




/**
 *  Function to toggle whether rhythm toolbar is visible 
 *  (no longer switches keymap to Rhythm.keymaprc when toolbar is on back to standard when off.)
 *  
 */
static void
toggle_rhythm_toolbar (GtkAction * action, gpointer param)
{
  GtkWidget *widget;
  widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/RhythmToolBar");
  //g_debug("Callback for %s\n", g_type_name(G_TYPE_FROM_INSTANCE(widget)));
  if ((!action) || gtk_widget_get_visible (widget))
    {

      gtk_widget_hide (widget);
    }
  else
    {

      gtk_widget_show (widget);
      /* make sure we are in Insert and Note for rhythm toolbar */
      // activate_action( "/MainMenu/ModeMenu/Note");
      //activate_action( "/MainMenu/ModeMenu/InsertMode");
    }
  if (Denemo.prefs.persistence && (Denemo.project->view == DENEMO_MENU_VIEW))
    Denemo.prefs.rhythm_palette = gtk_widget_get_visible (widget);
}

/**
 *  Function to toggle whether main toolbar is visible 
 *  
 * 
 */
void
toggle_toolbar (GtkAction * action, gpointer param)
{
  GtkWidget *widget;
  widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ToolBar");
  if ((!action) || gtk_widget_get_visible (widget))
    gtk_widget_hide (widget);
  else
    gtk_widget_show (widget);
  if (Denemo.prefs.persistence && (Denemo.project->view == DENEMO_MENU_VIEW))
    Denemo.prefs.toolbar = gtk_widget_get_visible (widget);
}

/**
 *  Function to toggle whether playback toolbar is visible 
 *  
 * 
 */
void
toggle_playback_controls (GtkAction * action, gpointer param)
{
  GtkWidget *widget;
  widget = Denemo.playback_control;
  if ((!action) || gtk_widget_get_visible (widget))
    gtk_widget_hide (widget);
  else
    gtk_widget_show (widget);
  if (Denemo.prefs.persistence && (Denemo.project->view == DENEMO_MENU_VIEW))
    Denemo.prefs.playback_controls = gtk_widget_get_visible (widget);
}

/**
 *  Function to toggle whether playback toolbar is visible 
 *  
 * 
 */
void
toggle_midi_in_controls (GtkAction * action, gpointer param)
{
  GtkWidget *widget;
  widget = Denemo.midi_in_control;
  if ((!action) || gtk_widget_get_visible (widget))
    gtk_widget_hide (widget);
  else
    gtk_widget_show (widget);
  if (Denemo.prefs.persistence && (Denemo.project->view == DENEMO_MENU_VIEW))
    Denemo.prefs.midi_in_controls = gtk_widget_get_visible (widget);
}


/**
 *  Function to toggle whether keyboard bindings can be set by pressing key over menu item 
 *  
 *  
 */
static void
toggle_quick_edits (GtkAction * action, gpointer param)
{
  Denemo.prefs.quickshortcuts = !Denemo.prefs.quickshortcuts;
}


/*UNUSED
static void
toggle_main_menu (GtkAction * action, gpointer param)
{
  GtkWidget *widget;
  widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/MainMenu");
  if ((!action) || gtk_widget_get_visible (widget))
    gtk_widget_hide (widget);
  else
    gtk_widget_show (widget);
}
*/
/**
 *  Function to toggle whether action menubar is visible 
 *  
 *  
 */
/* UNUSED
static void
toggle_action_menu (GtkAction * action, gpointer param)
{
  GtkWidget *widget;
  widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ActionMenu");
  if (!widget)
    return;                     // internal error - out of step with menu_entries...
  if ((!action) || gtk_widget_get_visible (widget))
    {

      gtk_widget_hide (widget);
    }
  else
    {
      gtk_widget_show (widget);
    }
}*/

/**
 *  Function to toggle visibility of print preview pane of current project
 *  
 *  
 */
static void
toggle_print_view (GtkAction * action, gpointer param)
{
  RETURN_IF_NON_INTERACTIVE();
#ifndef USE_EVINCE  
  g_debug("This feature requires denemo to be built with evince");
#else
  GtkWidget *w = gtk_widget_get_toplevel (Denemo.printarea);
  if ((!action) || gtk_widget_get_visible (w))
    gtk_widget_hide (w);
  else
    {
      gtk_widget_show (w);
      if (GPOINTER_TO_INT (g_object_get_data (G_OBJECT (Denemo.printarea), "printviewupdate")) < Denemo.project->changecount)
        refresh_print_view (TRUE);
    }
#endif
}

/**
 *  Function to toggle visibility of score layout window of current project
 *  
 *  
 */
static void
toggle_score_layout (GtkAction * action, gpointer param)
{
#ifndef USE_EVINCE  
  g_debug("This feature requires denemo to be built with evince");
#else
  DenemoProject *project = Denemo.project;
  GtkWidget *w = project->score_layout;
  GList *g = gtk_container_get_children (GTK_CONTAINER (w));
  if (g == NULL)
    {
      create_default_scoreblock ();
    }
  if ((!action) || gtk_widget_get_visible (w))
    gtk_widget_hide (w);
  else
    {
      gtk_widget_show (w);
    }
#endif
}


/**
 *  Function to toggle visibility of command manager window
 *  
 *  
 */
static void
toggle_command_manager (GtkAction * action, gpointer param)
{
  if(Denemo.command_manager==NULL) 
  {
    configure_keyboard_dialog (action, NULL);
  }
  else 
  {
    GtkWidget *w = Denemo.command_manager;
    if ((!action) || gtk_widget_get_visible (w))
        gtk_widget_hide (w);
    else    
      gtk_widget_show (w);
  }
}

/**
 *  Function to toggle visibility of lyrics view pane of current movement
 *  
 *  
 */
void
toggle_lyrics_view (GtkAction * action, gpointer param)
{
#ifndef USE_EVINCE  
  g_debug("This feature requires denemo to be built with evince");
#else
  GtkWidget *widget = Denemo.project->si->lyricsbox;
  if (!widget)
    g_warning ("No lyrics");
  else
    {
      if ((!action) || gtk_widget_get_visible (widget))
        gtk_widget_hide (widget);
      else
        {
          gtk_widget_show (widget);
        }
      if (Denemo.prefs.persistence && (Denemo.project->view == DENEMO_MENU_VIEW))
        Denemo.prefs.lyrics_pane = gtk_widget_get_visible (widget);
    }
#endif
}


void show_lilypond_errors (void)
{
    GtkWidget *widget = gtk_widget_get_parent (Denemo.console);
    
    if (!gtk_widget_get_visible (widget))
        activate_action ("/MainMenu/ViewMenu/" ToggleConsoleView_STRING); 
}
/**
 *  Function to toggle visibility of print preview pane of current project
 *  
 *  
 */
static void
toggle_score_view (GtkAction * action, gpointer param)
{
#ifndef USE_EVINCE  
  g_debug("This feature requires denemo to be built with evince");
#else
  GtkWidget *w = gtk_widget_get_parent (gtk_widget_get_parent (Denemo.scorearea));
  if ((!action) || gtk_widget_get_visible (w))
    gtk_widget_hide (w);
  else
    {
      gtk_widget_show (w);
      gtk_widget_grab_focus (Denemo.scorearea);
    }
#endif
}

/**
 *  Function to toggle visibility of titles etc of current project
 *  
 *  
 */
static void
toggle_scoretitles (GtkAction * action, gpointer param)
{
#ifndef USE_EVINCE  
  g_debug("This feature requires denemo to be built with evince");
#else
  GtkWidget *widget = Denemo.project->buttonboxes;
  if ((!action) || gtk_widget_get_visible (widget))
    gtk_widget_hide (widget);
  else
    gtk_widget_show (widget);
  if (Denemo.prefs.persistence && (Denemo.project->view == DENEMO_MENU_VIEW))
    Denemo.prefs.visible_directive_buttons = gtk_widget_get_visible (widget);
#endif
}

/**
 *  Function to toggle whether object menubar is visible 
 *  
 *  
 */
static void
toggle_object_menu (GtkAction * action, gpointer param)
{
#ifndef USE_EVINCE  
  g_debug("This feature requires denemo to be built with evince");
#else
  GtkWidget *widget;
  widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu");
  if (!widget)
    return;                     // internal error - out of step with menu_entries...
  if ((!action) || gtk_widget_get_visible (widget))
    {

      gtk_widget_hide (widget);
    }
  else
    {
      gtk_widget_show (widget);
    }
#endif
}



/**
 * Toggle entries for the menus
 */
GtkToggleActionEntry toggle_menu_entries[] = {
  {ToggleToolbar_STRING, NULL, N_("Tools"), NULL, N_("Show/hide a toolbar for general operations on music files"),
   G_CALLBACK (toggle_toolbar), TRUE}
  ,
  {TogglePlaybackControls_STRING, NULL, N_("Playback Control"), NULL, N_("Show/hide playback controls"),
   G_CALLBACK (toggle_playback_controls), TRUE}
  ,
  {ToggleMidiInControls_STRING, NULL, N_("Midi In Control"), NULL, N_("Show/hide Midi Input controls"),
   G_CALLBACK (toggle_midi_in_controls), TRUE}
  ,
  {ToggleRhythmToolbar_STRING, NULL, N_("Snippets"), NULL, N_("Show/hide a toolbar which allows\nyou to store and enter snippets of music and to enter notes using rhythm pattern of a snippet"),
   G_CALLBACK (toggle_rhythm_toolbar), TRUE}
  ,
  {ToggleObjectMenu_STRING, NULL, N_("Object Menu"), NULL, N_("Show/hide a menu which is arranged by objects\nThe actions available for note objects change with the mode"),
   G_CALLBACK (toggle_object_menu), TRUE}
  ,
  {ToggleLilyText_STRING, NULL, N_("LilyPond"), NULL, N_("Show/hide the LilyPond music typesetting language window"),
   G_CALLBACK (toggle_lilytext), FALSE}
  ,
  {ToggleScript_STRING, NULL, N_("Scheme Script"), NULL, N_("Show scheme script window"),
   G_CALLBACK (toggle_scheme), FALSE}
  ,

  {TogglePrintView_STRING, NULL, N_("Typeset Music"), NULL, NULL,
   G_CALLBACK (toggle_print_view), FALSE}
  ,

  {ToggleScoreLayout_STRING, NULL, N_("Score Layout"), NULL, NULL,
   G_CALLBACK (toggle_score_layout), FALSE}
  ,
  {ToggleCommandManager_STRING, NULL, N_("Command Center"), NULL, NULL,
   G_CALLBACK (toggle_command_manager), FALSE}
  ,

  {ToggleLyricsView_STRING, NULL, N_("Lyrics"), NULL, NULL,
   G_CALLBACK (toggle_lyrics_view), TRUE}
  ,


  {ToggleScoreView_STRING, NULL, N_("Score"), NULL, NULL,
   G_CALLBACK (toggle_score_view), TRUE}
  ,

  {ToggleScoreTitles_STRING, NULL, N_("Titles, Buttons etc"), NULL, NULL,
   G_CALLBACK (toggle_scoretitles), FALSE}
  ,


  {QuickEdits_STRING, NULL, N_("Allow Quick Shortcut Edits"), NULL, "Enable editing keybindings by pressing a key while hovering over the menu item",
   G_CALLBACK (toggle_quick_edits), TRUE}
  ,
  {RecordScript_STRING, NULL, N_("Record Scheme Script"), NULL, "Start recording commands into the Scheme script text window",
   G_CALLBACK (toggle_record_script), FALSE}
  ,

  {RHYTHM_E_STRING, NULL, N_("Audible Feedback\nInsert Duration/Edit Note"), NULL, N_("Gives feedback as you enter durations. N.B. durations are entered in Edit mode"),
   G_CALLBACK (toggle_rhythm_mode), FALSE}
  ,
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
  {NOTE_E_STRING, NULL, N_("Note"), NULL, N_("Normal (note) entry"), INPUTNORMAL},
  {REST_E_STRING, NULL, N_("Rest"), NULL, N_("Entering rests not notes"), INPUTREST},
  {BLANK_E_STRING, NULL, N_("Non printing rests"), NULL, N_("Enters rests which will not be printed (just take up space)\nUsed for positioning polyphonic voice entries"), INPUTBLANK}
#if 0
  ,
  {RHYTHM_E_STRING, NULL, N_("Audible Feedback"), NULL, N_("Gives feedback as you enter durations"), INPUTRHYTHM | INPUTNORMAL}
#endif
};

static GtkRadioActionEntry input_menu_entries[] = {
  {"KeyboardOnly", NULL, N_("No External Input"), NULL, N_("Entry of notes via computer keyboard only\nIgnores connected MIDI or microphone devices."),
   INPUTKEYBOARD}
  ,
  {"Microphone", NULL, N_("Audio Input"), NULL, N_("Enable pitch entry from microphone"), INPUTAUDIO
   /*  G_CALLBACK (toggle_pitch_recognition), FALSE */ }
  ,
  {"JackMidi", NULL, N_("Midi Input"), NULL, N_("Input from a MIDI source. Set up the source first using Edit → Change Preferences → Audio/Midi\nUse View → MIDI In Control to control what the input does.\n"), INPUTMIDI /*G_CALLBACK (jackmidi) */ }
};

struct cbdata
{
  DenemoProject *project;
  gchar *filename;
};

/**
 * Add history entry to the History menu, create a menu item for it
 */
void
addhistorymenuitem (gchar * filename)
{
  if (!g_file_test (filename, G_FILE_TEST_EXISTS))
    return;
  GtkWidget *item = gtk_ui_manager_get_widget (Denemo.ui_manager,
                                               "/MainMenu/FileMenu/OpenMenu/OpenRecent/Browse");
  GtkWidget *menu = gtk_widget_get_parent (GTK_WIDGET (item));

  item = gtk_menu_item_new_with_label (filename);
  gtk_menu_shell_insert (GTK_MENU_SHELL (menu), item, 0);
  g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (openrecent), g_strdup (filename)); //FIXME
  gtk_widget_show (item);
}

/**
 * Top-Level function to populate the History menu
 * with elements read from the denemohistory file
 */
static void
populate_opened_recent_menu (void)
{
  g_queue_foreach (Denemo.prefs.history, (GFunc) addhistorymenuitem, NULL);
}

static gchar *
get_most_recent_file (void)
{
  if (Denemo.prefs.history)
    {
      gchar *filename = (gchar *) g_queue_peek_tail (Denemo.prefs.history);
      if (filename && g_file_test (filename, G_FILE_TEST_EXISTS))
        return filename;
    }
  return NULL;
}

static void
show_type (GtkWidget * widget, gchar * message)
{
  g_message ("%s%s", message, widget ? g_type_name (G_TYPE_FROM_INSTANCE (widget)) : "NULL widget");
}




/**
 * Key snooper function. This function intercepts all key events before they are
 * passed to other functions for further processing. We use do quick shortcut edits.
 */
static gint
dnm_key_snooper (GtkWidget * grab_widget, GdkEventKey * event)
{
  //no special processing for key release events
  if (event->type == GDK_KEY_RELEASE)
    return FALSE;
  //if the grab_widget is a menu, the event could be a quick edit
  if (Denemo.prefs.quickshortcuts && GTK_IS_MENU (grab_widget))
    {
      return keymap_accel_quick_edit_snooper (grab_widget, event);
    }
  //else we let the event be processed by other functions
  return FALSE;
}


static void
switch_page (GtkNotebook * notebook, GtkWidget * page, guint pagenum)
{
  //g_debug("switching pagenum %d\n",pagenum);
  DenemoProject *project = Denemo.project;
  if (project == NULL)
    return;
  GList *g = g_list_nth (Denemo.projects, pagenum);
  if (g == NULL)
    {
      g_warning ("Got a switch page, but there is no such page in Denemo.projects");
      return;
    }
  DenemoProject *newproject = g->data;
  if (project == newproject)
    return;                     //on arrival Denemo.project is already set to the new project when you are doing new window
  /* turn off the LilyPond window if it is on
     it would be nice to keep a record of whether it was open for re-opening
     on return to this tab FIXME */

  if (Denemo.textwindow && gtk_widget_get_visible (Denemo.textwindow))
    activate_action ("/MainMenu/ViewMenu/" ToggleLilyText_STRING);

  if (gtk_widget_get_visible (Denemo.project->score_layout))
    activate_action ("/MainMenu/ViewMenu/" ToggleScoreLayout_STRING);

  unhighlight_rhythm (Denemo.project->prevailing_rhythm);

  Denemo.project = project = (DenemoProject *) (g->data);
  //g_debug("switch page\n");

//FIXME if Denemo.project->si->recording then show Denemo.audio_vol_control
  if (Denemo.prefs.visible_directive_buttons)
    {
      gtk_widget_hide (Denemo.project->buttonboxes);
      activate_action ("/MainMenu/ViewMenu/" ToggleScoreTitles_STRING);
    }
  switch (project->mode & ~MODE_MASK)
    {
    case INPUTINSERT:
      activate_action ("/MainMenu/ModeMenu/InsertMode");
      break;
    case INPUTEDIT:
      activate_action ("/MainMenu/ModeMenu/EditMode");
      break;
    case INPUTCLASSIC:
      activate_action ("/MainMenu/ModeMenu/ClassicMode");
      break;
    case 0:
      activate_action ("/MainMenu/ModeMenu/Modeless");
      break;
    default:
      ;
    }

  switch (project->mode & ~ENTRY_TYPE_MASK)
    {
    case INPUTNORMAL:
      activate_action ("/MainMenu/ModeMenu/Note");
      break;
    case INPUTBLANK:
      activate_action ("/MainMenu/ModeMenu/Blank");
      break;
    case INPUTREST:
      activate_action ("/MainMenu/ModeMenu/Rest");
      break;
    case INPUTRHYTHM:
      g_debug ("activating rhythm\n");
      activate_action ("/MainMenu/ModeMenu/Rhythm");
      break;

    default:
      ;
    }
  set_title_bar (Denemo.project);
  highlight_rhythm (Denemo.project->prevailing_rhythm);
  score_area_needs_refresh ();
  draw_score (NULL);
}




static gboolean
thecallback (GtkWidget * widget, GdkEventButton * event, GtkAction * action)
{
  if (event->button == 1 && !(event->state & (GDK_SHIFT_MASK | GDK_CONTROL_MASK)))
    return FALSE;
  g_debug ("going for %d for %d\n", event->button, event->state);
  event->button = 3;
  return menu_click (widget, event, action);
}

/*  proxy_connected
    callback to set callback for right click on menu items and
    set the shortcut label 

*/
static void
proxy_connected (GtkUIManager * uimanager, GtkAction * action, GtkWidget * proxy)
{
  int command_idx;
  command_row* row;
  attach_right_click_callback (proxy, action);
  const gchar *tooltip = gtk_action_get_tooltip (action);
  const gchar *additional_text;
  if (tooltip && g_str_has_prefix (tooltip, _("Menu:")))
    additional_text = _("Click here then hover over the menu items to find out what they will do");
  else
    additional_text = _("Left click to execute the command, press a key to assign a keyboard shortcut to the command,\nRight click to get a menu from which you can\nCreate a button for this command, or a two-key keyboard shortcut or more options still");
  gchar *tip = g_strconcat (tooltip, "\n------------------------------------------------------------------\n", additional_text, NULL);
  // unfortunately submenus seem not to be attached yet ... if((GTK_IS_IMAGE_MENU_ITEM(proxy)) && gtk_menu_item_get_submenu(proxy)) tip = g_strdup("test");
  // Denemo.map is not yet created either :(
  gtk_widget_set_tooltip_text (proxy, tip);
  g_free (tip);
  if (GTK_IS_IMAGE_MENU_ITEM (proxy))
    {
#ifdef FAST_MACHINE
      g_signal_connect_after (action, "activate", G_CALLBACK (switch_back_to_main_window), NULL);       /*ensure keyboard focus returns to drawing area */
#endif
      //  ????????????? should I put an icon named for the action->label into an icon factory here (we could just have one, static, and use gtk_icon_factory_add_default??????????
      if (!g_object_get_data (G_OBJECT (action), "connected"))
        g_signal_connect (G_OBJECT (proxy), "button-press-event", G_CALLBACK (thecallback), action);
      g_object_set_data (G_OBJECT (action), "connected", (gpointer) 1); //Unfortunately GtkImageMenuItems that pop up a menu do not wait for a button press - the focus switches to the popped up memory on entry. So we don't see this signal for them
    }
#if 0                           //(GTK_MINOR_VERSION <10)
  attach_action_to_widget (proxy, action, Denemo.project);
#endif
  if (Denemo.map == NULL)
    return;
  command_idx = lookup_command_from_name (Denemo.map, gtk_action_get_name (action));

  if (command_idx > -1){
    keymap_get_command_row (Denemo.map, &row, command_idx);
    update_accel_labels (Denemo.map, command_idx);

    if (row->hidden)
      set_visibility_for_action (action, FALSE);
  }
}





static GtkWidget *
create_playbutton (GtkWidget * box, gchar * thelabel, gpointer callback, gchar * image, gchar * tooltip)
{
  GtkWidget *button;
  if (thelabel)
    button = gtk_button_new_with_label (thelabel);
  else
    button = gtk_button_new ();
  gtk_widget_set_can_focus (button, FALSE);
  if (image)
    {
      gtk_button_set_image (GTK_BUTTON (button), gtk_image_new_from_stock (image, GTK_ICON_SIZE_BUTTON));
    }
  g_signal_connect (button, "clicked", G_CALLBACK (callback), NULL);
  gtk_box_pack_start (GTK_BOX (box), button, FALSE, TRUE, 0);
  gtk_widget_set_tooltip_text (button, tooltip);
  return button;
}


void
set_playbutton (gboolean pause)
{
  if (pause)
    {
      gtk_button_set_image (GTK_BUTTON (playbutton), gtk_image_new_from_stock (GTK_STOCK_MEDIA_PAUSE, GTK_ICON_SIZE_BUTTON));
    }
  else
    {
      gtk_button_set_image (GTK_BUTTON (playbutton), gtk_image_new_from_stock (GTK_STOCK_MEDIA_PLAY, GTK_ICON_SIZE_BUTTON));
    }
}

//Set the master volume of the passed score and change the slider to suit
void
set_master_volume (DenemoMovement * si, gdouble volume)
{
  si->master_volume = volume;
  if (master_vol_adj)
    {
      gtk_adjustment_set_value (master_vol_adj, volume);
      gtk_adjustment_changed (master_vol_adj);
    }
}

//Set the master tempo of the passed score and change the slider to suit
void
set_master_tempo (DenemoMovement * si, gdouble tempo)
{
    if(si->master_tempo>0.0)
        {
            Denemo.project->si->end_time /= si->master_tempo;
            Denemo.project->si->start_time /= si->master_tempo;
        }
    si->master_tempo = tempo;
    Denemo.project->si->end_time *= si->master_tempo;
    Denemo.project->si->start_time *= si->master_tempo;
  if (master_tempo_adj)
    {
      gtk_adjustment_set_value (master_tempo_adj, tempo * si->tempo);
      gtk_adjustment_changed (master_tempo_adj);
    }
}

/* create_window() creates the toplevel window and all the menus - it only
   called once per invocation of Denemo */
static void
create_window (void)
{

  GtkWidget *outer_main_vbox, *main_hbox, *main_vbox, *menubar, *toolbar, *hbox;
  GtkUIManager *ui_manager;
  GError *error;
  gchar *denemoui_path = NULL;
  gchar* data_file = NULL;

  Denemo.window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (Denemo.window), _("Denemo Main Window"));
  // it accesses Denemo.window
  loadWindowState ();
#ifdef G_OS_WIN32
  g_message ("Denemo icon not used");
  //not installed on windows ... data_file = g_build_filename (get_system_data_dir (), "icons","denemo.png", NULL);
#else
  gchar* icon_dirs[] = {
    g_build_filename(PACKAGE_SOURCE_DIR, PIXMAPS_DIR, NULL),
    g_strconcat (get_system_data_dir (), "/../pixmaps", NULL), //FIXME installed in wrong place?
    NULL
  };
  data_file = find_path_for_file("denemo.png", icon_dirs);
  if(data_file)
    gtk_window_set_default_icon_from_file (data_file, NULL);
#endif

  g_signal_connect (G_OBJECT (Denemo.window), "delete_event", G_CALLBACK (delete_callback), NULL);
  g_free (data_file);

  gtk_window_set_resizable (GTK_WINDOW (Denemo.window), TRUE);

  Denemo.color = 0xFFFFFF;      //white background RGB values



  outer_main_vbox = gtk_vbox_new (FALSE, 1);
  gtk_container_set_border_width (GTK_CONTAINER (outer_main_vbox), 1);
  gtk_container_add (GTK_CONTAINER (Denemo.window), outer_main_vbox);
  gtk_widget_show (outer_main_vbox);

  Denemo.action_group = gtk_action_group_new ("MenuActions");
  gtk_action_group_set_translation_domain (Denemo.action_group, NULL);
  /* This also sets current Denemo.project as the  callback data for all the functions in the
   * menubar, which is not needed since we have only one set of actions for all
   the projects. We will always act on Denemo.project anyway.*/
  gtk_action_group_add_actions (Denemo.action_group, menu_entries, G_N_ELEMENTS (menu_entries), Denemo.project);
  gtk_action_group_add_toggle_actions (Denemo.action_group, toggle_menu_entries, G_N_ELEMENTS (toggle_menu_entries), Denemo.project);
  gtk_action_group_add_radio_actions (Denemo.action_group, mode_menu_entries, G_N_ELEMENTS (mode_menu_entries), INPUTINSERT /* initial value */ ,
                                      G_CALLBACK (change_mode), Denemo.project);


  gtk_action_group_add_radio_actions (Denemo.action_group, type_menu_entries, G_N_ELEMENTS (type_menu_entries), INPUTNORMAL /* initial value */ ,
                                      G_CALLBACK (change_entry_type), Denemo.project);

  gtk_action_group_add_radio_actions (Denemo.action_group, input_menu_entries, G_N_ELEMENTS (input_menu_entries), have_midi ()? INPUTMIDI : INPUTKEYBOARD /* initial value */ ,
                                      G_CALLBACK (change_input_type), NULL);




  ui_manager = gtk_ui_manager_new ();
  Denemo.ui_manager = ui_manager;
  gtk_ui_manager_set_add_tearoffs (Denemo.ui_manager, TRUE);
  gtk_ui_manager_insert_action_group (ui_manager, Denemo.action_group, 0);

  g_signal_connect (G_OBJECT (Denemo.ui_manager), "connect-proxy", G_CALLBACK (proxy_connected), NULL);


  //We do not use accel_group anymore TODO delete the next 2 lines
  //accel_group = gtk_ui_manager_get_accel_group (ui_manager);
  //gtk_window_add_accel_group (GTK_WINDOW (Denemo.window), accel_group);

  gchar* dirs[] = {
    g_build_filename(PACKAGE_SOURCE_DIR, UI_DIR, NULL),
    g_build_filename(get_system_data_dir (), UI_DIR, NULL),
    NULL
  };

  denemoui_path = find_path_for_file("denemoui.xml", dirs);

  if (!denemoui_path)
    {
      g_error ("denemoui.xml could not be found, exiting");
      exit (EXIT_FAILURE);
    }

  error = NULL;
  if (!gtk_ui_manager_add_ui_from_file (ui_manager, denemoui_path, &error))
    {
      g_error ("Could not load %s: %s", denemoui_path, error->message);
      g_error_free (error);
      exit (EXIT_FAILURE);
    }


  {
    //pops up with menu items for the directives attached to the current note
    GtkWidget *menu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/NoteEditPopupDirectives");
    g_signal_connect (menu, "deactivate", G_CALLBACK (unpopulate_menu), NULL);
  }

  //menubar = gtk_item_factory_get_widget (item_factory, "<main>");
  Denemo.menubar = gtk_ui_manager_get_widget (ui_manager, "/MainMenu"); // this triggers Lily... missing action
  gtk_widget_set_tooltip_text (Denemo.menubar, _("This is the Main Menu bar, where menus for the mostly non-musical aspects (saving, printing, setting up input sources etc) are placed. See the Object Menu bar for the commands that edit music"));
  gtk_box_pack_start (GTK_BOX (outer_main_vbox), Denemo.menubar, FALSE, TRUE, 0);
  gtk_widget_show (Denemo.menubar);

  gtk_widget_set_tooltip_text (gtk_ui_manager_get_widget (ui_manager, "/ObjectMenu"), _("This is the Object Menu bar, where menus for the commands that edit music live. They are arranged in a hierarchy Score, Movement, Staff (which contains Voices) and then the things that go on a staff, notes, clefs etc. Directives covers everything else that you can put in amongst the notes to change the behavior from that point in the music."));

  gtk_widget_set_tooltip_markup (gtk_ui_manager_get_widget (ui_manager, "/RhythmToolBar"),
                                 _
                                 ("You can populate this bar with buttons holding a snippet of music. The highlighted snippet is the <i>prevailing duration</i>, that is the next note entered will follow the rhythmic pattern of this snippet.\nYou can enter the whole snippet by clicking on it, or using the command under ObjectMenu → Notes/Rests → Append/InsertDuration → Insert Snippet. You can also select the <i>prevailing snippet</i> using  ObjectMenu → Notes/Rests → Select Duration → Next Snippet.\nYou can hide this bar (to make more room on the screen) using the View menu. You can make it your preference to hide it using MainMenu → Edit → Change Preferences → Display Note/Rest entry toolbar"));

  toolbar = gtk_ui_manager_get_widget (ui_manager, "/ToolBar");
  // The user should be able to decide toolbar style.
  // But without gnome, there is no (ui) to set this option.
  gtk_widget_set_tooltip_text (toolbar, _("This tool bar contains a few conventional commands. You can hide it (to make more room on the screen) using the View menu. You can make it your preference to hide it using MainMenu → Edit → Change Preferences → Display general toolbar"));
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_BOTH_HORIZ);
  gtk_box_pack_start (GTK_BOX (outer_main_vbox), toolbar, FALSE, TRUE, 0);
  gtk_widget_set_can_focus (toolbar, FALSE);
  //GTK_WIDGET_UNSET_FLAGS(toolbar, GTK_CAN_FOCUS); 


  {
    Denemo.playback_control = gtk_vbox_new (FALSE, 1);
    gtk_widget_set_tooltip_text (Denemo.playback_control,
                                 _
                                 ("Controls for playback. The arrows on either side of the PLAY and STOP buttons move the playback start"
                                 " and playback end markers. Loop plays in a loop - you can edit while it plays. You can also record the output and save it as .ogg or .wav file. The temperament used for playing back can be set here."));
    gtk_box_pack_start (GTK_BOX (outer_main_vbox), Denemo.playback_control, FALSE, TRUE, 0);
    GtkFrame *frame = (GtkFrame *) gtk_frame_new (_("Playback Control"));
    gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
    gtk_container_add (GTK_CONTAINER (Denemo.playback_control), GTK_WIDGET (frame));

    GtkWidget *inner1 = gtk_vbox_new (FALSE, 1);
    gtk_container_add (GTK_CONTAINER (frame), inner1);


    GtkWidget *inner = gtk_hbox_new (FALSE, 1);
    gtk_box_pack_start (GTK_BOX (inner1), inner, FALSE, TRUE, 0);

    //gtk_box_pack_start (GTK_BOX (outer_main_vbox), inner, FALSE, TRUE, 0);
    gtk_widget_set_can_focus (inner, FALSE);
    GtkWidget *label;


    //create_playbutton(inner, NULL, pb_first, GTK_STOCK_GOTO_FIRST);



    //create_playbutton(inner,NULL, pb_rewind, GTK_STOCK_MEDIA_REWIND);

    create_playbutton (inner, NULL, pb_go_back, GTK_STOCK_GO_BACK, _("Moves the playback start point (which shows as a green bar) earlier in time\nThe red and green bars do not get drawn until you have started play, or at least created the time base."));

    create_playbutton (inner, NULL, pb_start_to_cursor, GTK_STOCK_GO_DOWN, _("Sets the playback start point (green bar) to the note at the cursor.\nThe red and green bars do not get drawn until you have started play, or at least created the time base."));
    create_playbutton (inner, NULL, pb_next, GTK_STOCK_GO_FORWARD, _("Moves the playback start point (which shows as a green bar) later in time\nThe red and green bars do not get drawn until you have started play, or at least created the time base."));
    create_playbutton (inner, NULL, pb_stop, GTK_STOCK_MEDIA_STOP, _("Stops the playback. On pressing play after this playback will start where the green bar is, not where you stopped. Use the Play/Pause button for that."));
    playbutton = create_playbutton (inner, NULL, pb_play, GTK_STOCK_MEDIA_PLAY, _("Starts playing back from the playback start (green bar) until the playback end (red bar).\nWhen playing it pauses the play, and continues when pressed again."));
    audiorecordbutton = create_playbutton (inner, NULL, pb_audiorecord, GTK_STOCK_MEDIA_RECORD, _("Starts/Stops recording the audio output from Denemo.\nRecords live performance and/or playback,\nsave to disk to avoid overwriting previous recordings."));
    exportbutton =  create_playbutton (inner, NULL, pb_exportaudio, GTK_STOCK_SAVE, _("Exports the audio recorded to disk"));
    
    create_playbutton (inner, NULL, pb_previous, GTK_STOCK_GO_BACK, _("Moves the playback end point (which shows as a red bar) earlier in time\nThe red and green bars do not get drawn until you have started play, or at least created the time base."));
    create_playbutton (inner, NULL, pb_end_to_cursor, GTK_STOCK_GO_UP, _("Sets the playback end point (red bar) to the note at the cursor.\nThe red and green bars do not get drawn until you have started play, or at least created the time base."));

    create_playbutton (inner, NULL, pb_go_forward, GTK_STOCK_GO_FORWARD, _("Moves the playback end point (which shows as a red bar) later in time\nThe red and green bars do not get drawn until you have started play, or at least created the time base."));

    //create_playbutton(inner,NULL, pb_forward, GTK_STOCK_MEDIA_FORWARD);

    create_playbutton (inner, _("Loop"), pb_loop, NULL, _("The music between the red and green bars is played in a loop.\nYou can edit the music while it is playing\nmonitoring your changes."));

    midiconductbutton = create_playbutton (inner, _("Conductor"), pb_conduct, NULL, _("With the mouse conductor once you press play the playback progresses as you move the mouse around\nWith this you can speed up and slow down the playback to listen in detail to a certain passage\n"));

    create_playbutton (inner,
#ifdef _HAVE_JACK_
                       _("Panic")
#else
                       _("Reset")
#endif
                       , pb_panic, NULL, _("Resets the synthesizer, on JACK it sends a JACK panic."));


    create_playbutton (inner, _("Set From Selection"), pb_set_range, NULL, _("Sets the playback range (green and red bars) to the current selection."));
    create_playbutton (inner, _("Playback Range"), pb_range, NULL, _("Pops up a dialog to get timings for start and end of playback."));
    GtkWidget *temperament_control = get_temperament_combo ();
    if (!gtk_widget_get_parent (temperament_control))
      //gtk_container_add (GTK_CONTAINER (inner), temperament_control);
      gtk_box_pack_start (GTK_BOX (inner), temperament_control, FALSE, FALSE, 0);
    {
      GtkWidget *hbox;
      hbox = gtk_hbox_new (FALSE, 1);
      gtk_box_pack_start (GTK_BOX (inner1), hbox, TRUE, TRUE, 0);
      // Tempo
      label = gtk_label_new (_("Tempo:"));
      gtk_widget_set_tooltip_text (label, _("Set the (initial) tempo of the movement"));
      gtk_widget_set_can_focus (label, FALSE);
      gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);
      master_tempo_adj = (GtkAdjustment *) gtk_adjustment_new (120.0, 0.0, 600.0, 1.0, 1.0, 0.0);
      GtkWidget *hscale = gtk_hscale_new (GTK_ADJUSTMENT (master_tempo_adj));
      gtk_scale_set_digits (GTK_SCALE (hscale), 0);
      //GTK_WIDGET_UNSET_FLAGS(hscale, GTK_CAN_FOCUS);
      gtk_widget_set_can_focus (hscale, FALSE);

      g_signal_connect (G_OBJECT (master_tempo_adj), "value_changed", G_CALLBACK (pb_tempo), NULL);
      gtk_box_pack_start (GTK_BOX (hbox), hscale, TRUE, TRUE, 0);

      //create_playbutton(hbox, "Set Tempo", pb_set_tempo, NULL);

      // Volume
      label = gtk_label_new (_("Volume"));
      //GTK_WIDGET_UNSET_FLAGS(label, GTK_CAN_FOCUS);      
      gtk_widget_set_tooltip_text (label, _("Set the (initial) volume of the movement"));

      gtk_widget_set_can_focus (label, FALSE);
      gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

      master_vol_adj = (GtkAdjustment *) gtk_adjustment_new (1.0, 0.0, 1.0, 1.0, 1.0, 0.0);

      hscale = gtk_hscale_new (GTK_ADJUSTMENT (master_vol_adj));
      gtk_scale_set_digits (GTK_SCALE (hscale), 2);
      gtk_widget_set_can_focus (hscale, FALSE);
      //GTK_WIDGET_UNSET_FLAGS(hscale, GTK_CAN_FOCUS);
      g_signal_connect (G_OBJECT (master_vol_adj), "value_changed", G_CALLBACK (pb_volume), NULL);
      gtk_box_pack_start (GTK_BOX (hbox), hscale, TRUE, TRUE, 0);


      // Audio Volume
      Denemo.audio_vol_control = gtk_hbox_new (FALSE, 1);
      label = gtk_label_new (_("Audio Volume Cut"));
      gtk_widget_set_tooltip_text (label, _("Reduce the volume of the source audio relative to the volume of the score"));

      gtk_widget_set_can_focus (label, FALSE);
      gtk_box_pack_start (GTK_BOX (Denemo.audio_vol_control), label, FALSE, TRUE, 0);

      audio_vol_adj = (GtkAdjustment *) gtk_adjustment_new (1.0, 0.0, 1.0, 0.1, 0.2, 0.0);

      hscale = gtk_hscale_new (GTK_ADJUSTMENT (audio_vol_adj));
      gtk_scale_set_digits (GTK_SCALE (hscale), 2);
      gtk_widget_set_can_focus (hscale, FALSE);
      //GTK_WIDGET_UNSET_FLAGS(hscale, GTK_CAN_FOCUS);
      g_signal_connect (G_OBJECT (audio_vol_adj), "value_changed", G_CALLBACK (audio_volume_cut), NULL);
      gtk_box_pack_start (GTK_BOX (Denemo.audio_vol_control), hscale, TRUE, TRUE, 0);

      label = gtk_label_new (_("Audio Volume Boost"));
       gtk_widget_set_tooltip_text (label, _("Boost the volume of the source audio relative to the volume of the score"));
      gtk_widget_set_can_focus (label, FALSE);
      gtk_box_pack_start (GTK_BOX (Denemo.audio_vol_control), label, FALSE, TRUE, 0);

      audio_vol_adj = (GtkAdjustment *) gtk_adjustment_new (1.0, 1.0, 10.0, 0.5, 2.0, 0.0);

      hscale = gtk_hscale_new (GTK_ADJUSTMENT (audio_vol_adj));
      gtk_scale_set_digits (GTK_SCALE (hscale), 2);
      gtk_widget_set_can_focus (hscale, FALSE);
      //GTK_WIDGET_UNSET_FLAGS(hscale, GTK_CAN_FOCUS);
      g_signal_connect (G_OBJECT (audio_vol_adj), "value_changed", G_CALLBACK (audio_volume_boost), NULL);
      gtk_box_pack_start (GTK_BOX (Denemo.audio_vol_control), hscale, TRUE, TRUE, 0);
      label = gtk_label_new (_("Audio Lead In "));
      gtk_widget_set_can_focus (label, FALSE);
      gtk_box_pack_start (GTK_BOX (Denemo.audio_vol_control), label, FALSE, TRUE, 0);
      leadin = (GtkSpinButton *) gtk_spin_button_new_with_range (-2.0, 2.0, 0.01);
      gtk_widget_set_tooltip_text (GTK_WIDGET (label), _("Set the number of seconds to clip from the audio, or if negative number of seconds silence before audio plays.\nThis is useful when the audio track does not begin on a barline."));
      g_signal_connect (G_OBJECT (leadin), "value_changed", G_CALLBACK (leadin_changed), NULL);
      gtk_box_pack_start (GTK_BOX (Denemo.audio_vol_control), GTK_WIDGET (leadin), FALSE, TRUE, 0);
      //label = gtk_label_new (_(" secs."));
      //gtk_widget_set_can_focus (label, FALSE);
      //gtk_box_pack_start (GTK_BOX (Denemo.audio_vol_control), label, FALSE, TRUE, 0);

      gtk_box_pack_start (GTK_BOX (hbox), Denemo.audio_vol_control, TRUE, TRUE, 0);
      
#ifdef _HAVE_RUBBERBAND_      
            /* Speed */
      label = gtk_label_new (_("Slowdown:"));
      gtk_widget_set_can_focus (label, FALSE);
      gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);
      speed_adj = (GtkAdjustment *) gtk_adjustment_new (1.0, 1.0, 4.0, 0.01, 0.1, 0.0);
      
      
      
      hscale = gtk_hscale_new (GTK_ADJUSTMENT (speed_adj));
      //gtk_scale_set_digits (GTK_SCALE (hscale), 0);
      gtk_widget_set_can_focus (hscale, FALSE);
      gtk_widget_set_tooltip_text (label, _("Slow down the audio output maintaining the pitch"));
      g_signal_connect (G_OBJECT (speed_adj), "value_changed", G_CALLBACK (set_speed), NULL);
      gtk_box_pack_start (GTK_BOX (hbox), hscale, TRUE, TRUE, 0);
#endif
      

    }


    Denemo.midi_in_control = gtk_vbox_new (FALSE, 1);
    gtk_widget_set_tooltip_text (Denemo.midi_in_control,
                                 _
                                 ("Controls for managing input from a MIDI controller (e.g. keyboard) attached to the computer. You may need to select your MIDI device first using MainMenu → Edit → Change Preferences → MIDI looking for MIDI in devices (turn your device on first). When you have a MIDI controller durations are inserted without any pitch (they appear in brown) playing on the controller puts the pitches onto the durations. The Shift and Control and ALT keys can also be used for listening without entering notes, checking pitches entered and entering chords. The foot pedal can also be used for chords. Release the ALT key and re-press to start a new chord - timing is unimportant, play the chord fast or slow."));
    gtk_box_pack_start (GTK_BOX (outer_main_vbox), Denemo.midi_in_control, FALSE, TRUE, 0);
    frame = (GtkFrame *) gtk_frame_new (_("Midi In Control"));
    gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
    gtk_container_add (GTK_CONTAINER (Denemo.midi_in_control), GTK_WIDGET (frame));

    inner1 = gtk_vbox_new (FALSE, 1);
    gtk_container_add (GTK_CONTAINER (frame), inner1);
    //inner = gtk_hbox_new(FALSE, 1);
    //gtk_box_pack_start (GTK_BOX (inner1), inner, FALSE, TRUE, 0);

    GtkWidget *enharmonic_control = get_enharmonic_frame ();
    if (!gtk_widget_get_parent (enharmonic_control))
      gtk_container_add (GTK_CONTAINER (inner1), enharmonic_control);

    {
      GtkWidget *hbox;
      hbox = gtk_hbox_new (FALSE, 1);
      gtk_box_pack_start (GTK_BOX (inner1), hbox, TRUE, TRUE, 0);
      midi_in_status = gtk_label_new (_("Appending/Editing Pitches"));
      gtk_widget_set_tooltip_text (midi_in_status, _("This tells you what will happen to a MIDI in event from your controller. Use the Control Shift or ALT keys, or caps lock to affect what will happen."));
      gtk_label_set_use_markup (GTK_LABEL (midi_in_status), TRUE);
      gtk_box_pack_start (GTK_BOX (hbox), midi_in_status, FALSE, TRUE, 0);

      midiplayalongbutton =
        create_playbutton (hbox, _("Switch to Play Along Playback"), pb_playalong, NULL, _("When in playalong mode, on clicking Play, the music plays until it reaches the Denemo cursor\nFrom then on you must play the notes at the cursor to progress the playback.\nSo if you set the cursor on the first note of the part you want to play, then once you have pressed play you can play along with Denemo, with Denemo filling in the other parts and waiting if you play a wrong note."));

      deletebutton = create_playbutton (hbox, "Delete", pb_midi_delete, NULL, _("Delete the MIDI recording you have made."));

      convertbutton = create_playbutton (hbox, "Convert", pb_midi_convert, NULL, _("Convert the MIDI recording you have made to notation."));
      midirecordbutton = create_playbutton (hbox, NULL, pb_record, GTK_STOCK_MEDIA_RECORD, _("Starts playing and simultaneously records from MIDI in.\nOnce a recording is made it is played back with the score when you press Play.\nIt can be deleted with the Delete button or converted to notation with Convert\n.A MIDI recording is not saved with the Denemo score."));

      gtk_widget_show_all (Denemo.midi_in_control);
      gtk_widget_show_all (Denemo.playback_control);
      gtk_widget_hide (deletebutton);
      gtk_widget_hide (convertbutton);
      gtk_widget_hide (exportbutton);
      gtk_widget_hide (Denemo.audio_vol_control);
    }
  }


  toolbar = gtk_ui_manager_get_widget (ui_manager, "/RhythmToolBar");
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_TEXT);
  gtk_box_pack_start (GTK_BOX (outer_main_vbox), toolbar, FALSE, TRUE, 0);

  menubar = gtk_ui_manager_get_widget (ui_manager, "/ObjectMenu");
  if (menubar)
    {
      gtk_box_pack_start (GTK_BOX (outer_main_vbox), menubar, FALSE, TRUE, 0);
    }
    
  main_hbox = gtk_hbox_new (FALSE, 1);
  gtk_widget_show (main_hbox);
  gtk_box_pack_start (GTK_BOX (outer_main_vbox), main_hbox, TRUE, TRUE, 0);
  Denemo.hpalettes = gtk_hbox_new (FALSE, 1);
  gtk_widget_show(Denemo.hpalettes);
  gtk_box_pack_start (GTK_BOX (main_hbox), Denemo.hpalettes, FALSE, FALSE, 0);
  main_vbox = gtk_vbox_new (FALSE, 1);
  gtk_widget_show (main_vbox);
  gtk_box_pack_start (GTK_BOX (main_hbox), main_vbox, TRUE, TRUE, 0);

  Denemo.vpalettes = gtk_vbox_new (FALSE, 1);
  gtk_widget_show(Denemo.vpalettes);
  gtk_box_pack_start (GTK_BOX (main_vbox), Denemo.vpalettes, FALSE, FALSE, 0);

  Denemo.notebook = gtk_notebook_new ();
  gtk_notebook_set_show_tabs (GTK_NOTEBOOK (Denemo.notebook), FALSE);   //only show when more than one
  //gtk_notebook_popup_enable (Denemo.notebook);?? doesn't work...
  gtk_widget_show (Denemo.notebook);
  gtk_box_pack_start (GTK_BOX (main_vbox), Denemo.notebook, FALSE, FALSE, 0);

  {
    Denemo.scorearea = gtk_drawing_area_new ();
    if (Denemo.prefs.newbie)
      gtk_widget_set_tooltip_text (Denemo.scorearea, _("This is the Denemo Display for the music you have entered."
                                                       " See the print view window for the typeset appearance. "
                                                       " The blue lozenge is the Denemo Cursor - it turns Red when when the bar is full or green if you are inserting in a bar."
                                                       " Many commands operate on the object at the Denemo cursor\nOverfull/Underfull bars are colored red/blue,"
                                                       " use the Upbeat (Anacrusis, Pickup) command if that is intentional."
                                                       "\nYou can switch to a menu-less view or a page-view using the Esc key." " For the paged view you drag the red bar up the page to set how many systems you want showing." "\nFor the paged view you will probably want a smaller zoom - use Control+scroll-wheel on your mouse to zoom the display." "Right-click on an object to get a short menu of actions or set the mouse input mode."));
    GtkWidget *scorearea_topbox = gtk_vbox_new (FALSE, 1);
    //gtk_container_add (GTK_CONTAINER (main_vbox), scorearea_topbox);
    gtk_box_pack_start (GTK_BOX (main_vbox), scorearea_topbox, TRUE, TRUE, 0);   

    GtkWidget *score_and_scroll_hbox = gtk_hbox_new (FALSE, 1);
    //gtk_container_add (GTK_CONTAINER (scorearea_topbox), score_and_scroll_hbox);
    gtk_box_pack_start (GTK_BOX (scorearea_topbox), score_and_scroll_hbox, TRUE, TRUE, 0);   
    gtk_widget_show (score_and_scroll_hbox);

    gtk_box_pack_start (GTK_BOX (score_and_scroll_hbox), Denemo.scorearea, TRUE, TRUE, 0);      // with this, the scorearea_draw_event is called
    gtk_widget_show (Denemo.scorearea);
#if GTK_MAJOR_VERSION==3
    g_signal_connect (G_OBJECT (Denemo.scorearea), "draw", G_CALLBACK (scorearea_draw_event), NULL);
#else
    g_signal_connect (G_OBJECT (Denemo.scorearea), "expose_event", G_CALLBACK (scorearea_draw_event), NULL);
#endif
    g_signal_connect (G_OBJECT (Denemo.scorearea), "configure_event", G_CALLBACK (scorearea_configure_event), NULL);
    g_signal_connect (G_OBJECT (Denemo.scorearea), "button_release_event", G_CALLBACK (scorearea_button_release), NULL);
    g_signal_connect (G_OBJECT (Denemo.scorearea), "motion_notify_event", G_CALLBACK (scorearea_motion_notify), NULL);
    g_signal_connect (G_OBJECT (Denemo.scorearea), "leave-notify-event", G_CALLBACK (scorearea_leave_event), NULL);
    g_signal_connect (G_OBJECT (Denemo.scorearea), "enter-notify-event", G_CALLBACK (scorearea_enter_event), NULL);
    g_signal_connect (G_OBJECT (Denemo.scorearea), "scroll_event", G_CALLBACK (scorearea_scroll_event), NULL);
    //g_signal_handlers_block_by_func(Denemo.scorearea, G_CALLBACK (scorearea_motion_notify), NULL);
    g_signal_connect (G_OBJECT (Denemo.scorearea), "button_press_event", G_CALLBACK (scorearea_button_press), NULL);
    g_signal_connect (G_OBJECT (Denemo.scorearea), "key_press_event", G_CALLBACK (scorearea_keypress_event), NULL);
    g_signal_connect (G_OBJECT (Denemo.scorearea), "key_release_event", G_CALLBACK (scorearea_keyrelease_event), NULL);


#if GTK_MAJOR_VERSION==2
    gtk_widget_add_events /*gtk_widget_set_events */ (Denemo.scorearea, (GDK_EXPOSURE_MASK | 
                                                                        GDK_POINTER_MOTION_MASK | GDK_LEAVE_NOTIFY_MASK | GDK_ENTER_NOTIFY_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK));
#else
   gtk_widget_add_events /*gtk_widget_set_events */ (Denemo.scorearea, (GDK_EXPOSURE_MASK | GDK_SCROLL_MASK |
                                                                        GDK_POINTER_MOTION_MASK | GDK_LEAVE_NOTIFY_MASK | GDK_ENTER_NOTIFY_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK));
#endif
    Denemo.vadjustment = (GtkAdjustment *) gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0);
    g_signal_connect (G_OBJECT (Denemo.vadjustment), "value_changed", G_CALLBACK (vertical_scroll), NULL);
    Denemo.vscrollbar = gtk_vscrollbar_new (GTK_ADJUSTMENT (Denemo.vadjustment));
    gtk_box_pack_start (GTK_BOX (score_and_scroll_hbox), Denemo.vscrollbar, FALSE, TRUE, 0);
    gtk_widget_show (Denemo.vscrollbar);

    Denemo.hadjustment = (GtkAdjustment *) gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0);

    g_signal_connect (G_OBJECT (Denemo.hadjustment), "value_changed", G_CALLBACK (horizontal_scroll), NULL);
    Denemo.hscrollbar = gtk_hscrollbar_new (GTK_ADJUSTMENT (Denemo.hadjustment));
    gtk_box_pack_start (GTK_BOX (scorearea_topbox), Denemo.hscrollbar, FALSE, TRUE, 0);
    gtk_widget_show_all (scorearea_topbox);
  }


  create_lilywindow ();

  // This section creates an hbox and places it in the main vbox. Inside this hbox are placed a status bar and a label.
  // The status bar is not properly used within Denemo, and could just as well be a label too.
  Denemo.statuslabel = gtk_label_new ("");
  gtk_widget_set_tooltip_text (Denemo.statuslabel, _("This bar shows:\nPending ♯ or ♭ sign (if the next note entered will be sharpened or flattened)\nThe movement number\nDescription of the object at the Denemo cursor\nPosition and status (appending or inserting) of the cursor.\nIf the Playback Controls are visible then the timing of the object at the cursor is shown.\nIf MIDI in controls are visible the current enharmonic range is shown.\nWhen the first key of a two-key shortcut is pressed the possible continuations are shown here."));
#if GTK_MAJOR_VERSION == 2
    hbox = gtk_hpaned_new ();
#else
    hbox = gtk_paned_new (GTK_ORIENTATION_HORIZONTAL);
#endif
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);
  gtk_paned_add1 (GTK_PANED (hbox), Denemo.statuslabel);
  gtk_widget_show (Denemo.statuslabel);
  //Denemo.status_context_id = gtk_statusbar_get_context_id (GTK_STATUSBAR (Denemo.statusbar), "Denemo");
  //gtk_statusbar_push (GTK_STATUSBAR (Denemo.statusbar), Denemo.status_context_id, "Denemo");
  Denemo.input_source = gtk_label_new (_("No MIDI filter"));
  gtk_widget_set_tooltip_text (Denemo.input_source, _("This area shows which MIDI filters are active. It can also be used by commands to pass information to the user"));
  gtk_widget_show (Denemo.input_source);
  Denemo.input_filters = g_string_new ("");
  gtk_paned_add2 (GTK_PANED (hbox), Denemo.input_source);
  gtk_paned_set_position (GTK_PANED (hbox), 1000);
  gtk_widget_show (hbox);
  // End of status bar stuff - note this is not working on Windows correctly.
    
    
  create_scheme_window ();

  if (!Denemo.non_interactive)
    gtk_widget_show (Denemo.window);
  // Now that the window is shown, initialize the gcs
  // gcs_init (Denemo.window->window);

  parse_paths (denemoui_path, Denemo.project);
  g_free (denemoui_path);
  
  //set all the labels to use markup so that we can use the music font. Be aware this means you cannot use labels involving "&" "<" and ">" and so on without escaping them 
  //                                 FIXME labels in toolitems are not correct until you do NewWindow.
  //                                 Really we should change the default for the class. */
  use_markup (Denemo.window);   
  //g_debug("Turning on the modes\n");


  //write_status(Denemo.project);
  Denemo.InsertModeMenu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu/NotesRests/InsertModeNote");
  Denemo.EditModeMenu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu/NotesRests/EditModeNote");
  Denemo.ClassicModeMenu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu/NotesRests/ClassicModeNote");
  Denemo.ModelessMenu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu/NotesRests/ModelessNote");


  g_signal_connect (G_OBJECT (Denemo.notebook), "switch_page", G_CALLBACK (switch_page), NULL);

}                               /* create window */


void
newview (GtkAction * action, DenemoScriptParam * param)
{
  newtab ();
  Denemo.project->si->undo_guard = 1;       //do not collect undo for initialization of score
  load_scheme_init ();
  Denemo.project->si->undo_guard = Denemo.prefs.disable_undo;
}

void
new_score_cb (GtkAction * action, DenemoScriptParam * param)
{
  DenemoScriptParam dummy;
  dummy.string = NULL;
  if (param == NULL)
    param = &dummy;
  file_newwrapper (action, param);
  if (param->status)
    {
      call_out_to_guile ("(d-InstrumentName  (_ \"Unnamed\"))");
      call_out_to_guile ("(d-ScoreTitle (_ \"Click Title\"))");
      denemo_scheme_init ();
    }
}

static DenemoProject*
new_project()
{
  static gint id = 1;
  DenemoProject *project = (DenemoProject *) g_malloc0 (sizeof (DenemoProject));
  //uniquely identifies this musical score editor for duration of program.
  project->id = id++;
  project->mode = Denemo.prefs.mode;
  project->pending_midi = g_queue_new ();
  Denemo.projects = g_list_append (Denemo.projects, project);
  Denemo.project = NULL;
  project->lilycontrol.papersize = g_string_new ("a4");     //A4 default
  project->lilycontrol.staffsize = g_string_new ("18");
  project->lilycontrol.lilyversion = g_string_new ("");
  project->lilycontrol.orientation = TRUE;  //portrait
  return project;
}

gint
hide_printarea_on_delete (void)
{
  activate_action ("/MainMenu/ViewMenu/" TogglePrintView_STRING);
  return TRUE;
}

static gint
hide_score_layout_on_delete (void)
{
  activate_action ("/MainMenu/ViewMenu/" ToggleScoreLayout_STRING);
  return TRUE;
}

static void toggle_rhythm_toolbar (GtkAction * action, gpointer param);
static void toggle_entry_toolbar (GtkAction * action, gpointer param);
static void toggle_object_menu (GtkAction * action, gpointer param);

/* UNUSED
static void toggle_main_menu (GtkAction * action, gpointer param);
*/
static void toggle_print_view (GtkAction * action, gpointer param);
static void toggle_score_layout (GtkAction * action, gpointer param);
static void toggle_command_manager (GtkAction * action, gpointer param);
static void toggle_scoretitles (GtkAction * action, gpointer param);

static void
toggle_page_view (void)
{

  static gdouble zoom = 1.0;
  static gdouble system_height = 0.25;
  DenemoMovement *si = Denemo.project->si;
  if (si->page_width == 0)
    {
      si->page_width = gdk_screen_get_width (gtk_window_get_screen (GTK_WINDOW (Denemo.window)));
      si->page_height = gdk_screen_get_height (gtk_window_get_screen (GTK_WINDOW (Denemo.window)));
      if (si->page_height / (double) si->page_width < 1.4)
        si->page_width = si->page_height / 1.4;
      si->page_zoom = 0.5;
      si->page_system_height = 0.25;
    }
  if (Denemo.project->view == DENEMO_PAGE_VIEW)
    {
      gtk_window_get_size (GTK_WINDOW (Denemo.window), &si->page_width, &si->page_height);
      si->page_zoom = si->zoom;
      si->page_system_height = si->system_height;
      si->zoom = zoom;
      si->system_height = system_height;
      Denemo.project->view = DENEMO_LINE_VIEW;
      gtk_window_resize (GTK_WINDOW (Denemo.window), si->stored_width, si->stored_height);
    }
  else
    {
      gtk_window_get_size (GTK_WINDOW (Denemo.window), &si->stored_width, &si->stored_height);
      zoom = si->zoom;
      system_height = si->system_height;
      si->zoom = si->page_zoom;
      si->system_height = si->page_system_height;
      Denemo.project->view = DENEMO_PAGE_VIEW;
      gtk_window_resize (GTK_WINDOW (Denemo.window), si->page_width, si->page_height);
    }
}

/* Hide/show everything except the drawing area */
void
toggle_to_drawing_area (gboolean show)
{
#define current_view Denemo.project->view
  gint height;                  // height of menus that are hidden
  gint win_width, win_height;
  height = 0;

  if (current_view == DENEMO_LINE_VIEW)
    {
      toggle_page_view ();
      return;
    }
  if (current_view == DENEMO_PAGE_VIEW)
    {
      toggle_page_view ();
      win_width = Denemo.project->si->stored_width;
      win_height = Denemo.project->si->stored_height;
    }
  else
    gtk_window_get_size (GTK_WINDOW (Denemo.window), &win_width, &win_height);
  //g_debug("window width is %d\n", win_width);
  // NOTE  lyrics are per movement
  GtkWidget *widget;
  gboolean hide = !show;
  if (((current_view == DENEMO_PAGE_VIEW) && hide) || (show && (!current_view)))
    return;
    
   hide ? (gtk_widget_hide(Denemo.vpalettes),gtk_widget_hide(Denemo.hpalettes)):
    (gtk_widget_show(Denemo.vpalettes),gtk_widget_show(Denemo.hpalettes));
  current_view = hide ? DENEMO_LINE_VIEW : DENEMO_MENU_VIEW;
#define ACCUM height += get_widget_height(widget)


#define TOG(name, item, menu)\
  widget = gtk_ui_manager_get_widget (Denemo.ui_manager, name);\
  static gboolean item=TRUE;\
  if(hide)\
    item = gtk_widget_get_visible (widget);\
  if((hide && item) || (show && item))\
    ACCUM, activate_action(menu);

#define TOG2(name, item)\
  widget = gtk_ui_manager_get_widget (Denemo.ui_manager, name);\
  static gboolean item=TRUE;\
  if(hide)\
    item = gtk_widget_get_visible (widget);\
  if(hide && item)\
    ACCUM, gtk_widget_hide(widget);\
  if(!hide && item)\
    ACCUM, gtk_widget_show(widget);

#define TOG3(name, item, menu)\
  widget = name;\
  static gboolean item=TRUE;\
  if(hide) \
    item = gtk_widget_get_visible (widget);\
  if((hide && item) || (show && item))\
    ACCUM, activate_action(menu);

  TOG ("/ToolBar", toolbar, "/MainMenu/ViewMenu/" ToggleToolbar_STRING);
  //TOG("/RhythmToolBar", rtoolbar, "/MainMenu/ViewMenu/"ToggleRhythmToolbar_STRING);
  TOG ("/ObjectMenu", objectmenu, "/MainMenu/ViewMenu/" ToggleObjectMenu_STRING);

  TOG2 ("/MainMenu", mainmenu);

  //TOG3(gtk_widget_get_parent(gtk_widget_get_parent(Denemo.printarea)), print_view, "/MainMenu/ViewMenu/"TogglePrintView_STRING);
  TOG3 (Denemo.project->buttonboxes, scoretitles, "/MainMenu/ViewMenu/" ToggleScoreTitles_STRING);
  TOG3 (Denemo.playback_control, playback_control, "/MainMenu/ViewMenu/" TogglePlaybackControls_STRING);
  TOG3 (Denemo.midi_in_control, midi_in_control, "/MainMenu/ViewMenu/" ToggleMidiInControls_STRING);

  gtk_window_resize (GTK_WINDOW (Denemo.window), win_width, win_height + (current_view ? -height : height));
#undef current_view
}

void
ToggleReduceToDrawingArea (GtkAction * action, DenemoScriptParam * param)
{
  RETURN_IF_NON_INTERACTIVE();
  GtkWidget *widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/MainMenu");
  gboolean visibile = gtk_widget_get_visible (widget);
  if (Denemo.project->view == DENEMO_MENU_VIEW && !visibile)
    {
      g_warning ("Out of step");
      Denemo.project->view = DENEMO_LINE_VIEW;
    }
  toggle_to_drawing_area (!gtk_widget_get_visible (widget));
}

static void
new_movement(){
  point_to_new_movement (Denemo.project);
  Denemo.project->movements = g_list_append (NULL, Denemo.project->si);
}

/**
 * Creates a new DenemoProject structure represented by a tab in a notebook: the DenemoProject can, at anyone time, control one musical score possibly of several movements. It can, from time to time have different musical scores loaded into it. So it is to be thought of as a Music Score Editor.
 * This DenemoProject* project is appended to the global list Denemo.projects.
 * A single movement (DenemoMovement) is instantiated in the project.
 * 
 */
static void
newtab ()
{
  if (Denemo.project && gtk_widget_get_visible (Denemo.project->score_layout))
    activate_action ("/MainMenu/ViewMenu/" ToggleScoreLayout_STRING);
  if (Denemo.project && gtk_widget_get_visible (Denemo.textwindow))
    activate_action ("/MainMenu/ViewMenu/" ToggleLilyText_STRING);

  DenemoProject* project = new_project();
  project->score_layout = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (project->score_layout), "Score Layout");
  gtk_window_set_default_size (GTK_WINDOW (project->score_layout), 400, 800);
  g_signal_connect (G_OBJECT (project->score_layout), "delete-event", G_CALLBACK (hide_score_layout_on_delete), NULL);

  /* Initialize the project */

  //create the tab for this project
  GtkWidget *top_vbox = gtk_vbox_new (FALSE, 1);
  project->buttonboxes = gtk_vbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (top_vbox), project->buttonboxes, FALSE, TRUE, 0);
  project->buttonbox = gtk_hbox_new (FALSE, 1);
  gtk_widget_set_tooltip_text (project->buttonbox, _("A button bar that can be populated by titles and other user generated buttons.\nGenerally by clicking the button you can edit the title or value or execute the action of the button"));
  gtk_box_pack_start (GTK_BOX (project->buttonboxes), project->buttonbox, FALSE, TRUE, 0);

  gtk_widget_set_can_focus (project->buttonboxes, FALSE);
  gtk_widget_set_can_focus (project->buttonbox, FALSE);






  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (top_vbox), main_vbox, TRUE, TRUE, 0);
  gint pagenum =                //gtk_notebook_append_page (GTK_NOTEBOOK (Denemo.notebook), top_vbox, NULL);
    gtk_notebook_insert_page_menu (GTK_NOTEBOOK (Denemo.notebook), top_vbox, NULL, NULL, -1); //puts top_vbox inside Denemo.notebook
  /*(GtkNotebook *notebook,
     GtkWidget *child,
     GtkWidget *tab_label,
     GtkWidget *menu_label,
     gint position); */
  gtk_notebook_popup_enable (GTK_NOTEBOOK (Denemo.notebook));

  Denemo.page = gtk_notebook_get_nth_page (GTK_NOTEBOOK (Denemo.notebook), pagenum);    //note Denemo.page is redundant, it is set to the last page created and it is never unset even when that page is deleted - it is only used by the selection paste routine.
  gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), pagenum);

  Denemo.project = project;
  set_title_bar (project);
  if (pagenum)
    gtk_notebook_set_show_tabs (GTK_NOTEBOOK (Denemo.notebook), TRUE);
  set_title_bar (project);
  gtk_widget_show (top_vbox);
  gtk_widget_show (main_vbox);

  //gtk_grab_remove(toolbar);  ?????????

#if 0
  GtkWidget *hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);
  gtk_widget_show (hbox);
#endif

#ifdef USE_EVINCE  
  install_printpreview (main_vbox);
#endif
  
  //FIXME populate_opened_recent_menu (project);

  /* create the first movement now because showing the window causes it to try to draw the scorearea
     which it cannot do before there is a score. FIXME use signal blocking to control this - see importxml.c */
  new_movement ();

  install_lyrics_preview (project->si, top_vbox);
  gtk_widget_set_can_focus (Denemo.scorearea, TRUE);
  gtk_widget_show (Denemo.page);
  gtk_widget_grab_focus (Denemo.scorearea);



  create_rhythm_cb ((gpointer) insert_chord_0key, NULL);
  create_rhythm_cb ((gpointer) insert_chord_1key, NULL);
  create_rhythm_cb ((gpointer) insert_chord_2key, NULL);
  create_rhythm_cb ((gpointer) insert_chord_3key, NULL);
  create_rhythm_cb ((gpointer) insert_chord_4key, NULL);
  create_rhythm_cb ((gpointer) insert_chord_5key, NULL);
  create_rhythm_cb ((gpointer) insert_chord_6key, NULL);
  create_rhythm_cb ((gpointer) insert_chord_7key, NULL);
  create_rhythm_cb ((gpointer) insert_chord_8key, NULL);


  create_rhythm_cb ((gpointer) insert_rest_0key, NULL);
  create_rhythm_cb ((gpointer) insert_rest_1key, NULL);
  create_rhythm_cb ((gpointer) insert_rest_2key, NULL);
  create_rhythm_cb ((gpointer) insert_rest_3key, NULL);
  create_rhythm_cb ((gpointer) insert_rest_4key, NULL);
  create_rhythm_cb ((gpointer) insert_rest_5key, NULL);
  create_rhythm_cb ((gpointer) insert_rest_6key, NULL);
  create_rhythm_cb ((gpointer) insert_rest_7key, NULL);
  create_rhythm_cb ((gpointer) insert_rest_8key, NULL);

  //Denemo.project->mode = Denemo.prefs.mode;

  // this stops the keyboard input from getting to  scorearea_keypress_event if done after attaching the signal, why?
  gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), pagenum);      //if this is not done Gdk-CRITICAL **: gdk_draw_drawable: assertion `GDK_IS_DRAWABLE (drawable)' failed message results. Presumably because we have failed to block the (expose_event) drawing while we set up the new page. FIXME.

  gtk_widget_set_can_focus (Denemo.scorearea, TRUE);
  //GTK_WIDGET_SET_FLAGS(Denemo.scorearea, GTK_CAN_FOCUS);
  gtk_widget_grab_focus (GTK_WIDGET (Denemo.scorearea));

  if (Denemo.prefs.autosave)
    {
      if (Denemo.autosaveid)
        {
          g_debug ("No autosave on new tab.");
        }
      else
        {
          Denemo.autosaveid = g_timeout_add (Denemo.prefs.autosave_timeout * 1000 * 60, (GSourceFunc) auto_save_document_timeout, Denemo.project);
        }
    }


  if (Denemo.prefs.visible_directive_buttons)
    {
      gtk_widget_hide (Denemo.project->buttonboxes);
      activate_action ("/MainMenu/ViewMenu/" ToggleScoreTitles_STRING);
    }
  if (have_midi () && Denemo.prefs.startmidiin)
    project->input_source = INPUTMIDI;
}                               /* end of newtab creating a new DenemoProject holding one musical score */

void score_area_needs_refresh (void){
  RETURN_IF_NON_INTERACTIVE ();
  score_area_needs_refresh ();
}
