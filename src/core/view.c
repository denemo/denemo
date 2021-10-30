/* view.c
 * Functions to create a top level Denemo window
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2003-2005  Adam Tee (c) 2007, 2008 2009 Richard Shann
 *
 */

#include <string.h>
#include <math.h>
#include "core/view.h"

#include "command/lilydirectives.h"
#include "ui/dialogs.h"
#include "core/utils.h"
#include <stdlib.h>
#include <glib/gstdio.h>
#include <cairo.h>
#include <cairo-svg.h>
#include <librsvg/rsvg.h>
#include <sndfile.h>

#include "audio/playback.h"
#include "audio/pitchentry.h"
#include "audio/portaudiobackend.h"
#include "audio/fluid.h"
#include "export/exportlilypond.h"
#include "export/print.h"
#include "printview/printview.h"
#include "printview/svgview.h"
#include "command/grace.h"
#include "core/kbd-custom.h"
#include "core/keyboard.h"
#include "export/exportmidi.h"
#include "audio/midi.h"
#include "audio/midirecord.h"
#include "source/source.h"
#include "command/commandfuncs.h"
#include "display/calculatepositions.h"
#include "ui/texteditors.h"
#include "core/prefops.h"
#include "audio/audiointerface.h"
#include "source/sourceaudio.h"
#include "command/scorelayout.h"
#include "core/keymapio.h"
#include "core/menusystem.h"
#include "command/measure.h"
#include "export/audiofile.h"
#include "export/guidedimportmidi.h"
#include "scripting/scheme-identifiers.h"
#include "scripting/scheme-callbacks.h"

static GtkWidget *playbutton;
static GtkWidget *midihelpbutton;
static GtkWidget *audiorecordbuttonlabel;
static GtkWidget *midiconductbutton;
static GtkWidget *midi_in_status;
static GtkWidget *midiplayalongbutton;
static GtkWidget *exportbutton;
static GtkSpinButton *leadin;
static GtkAdjustment *master_vol_adj;
static GtkWidget *tempo_widget;
#ifdef _HAVE_RUBBERBAND_
static GtkAdjustment *speed_adj;
#endif
static void pb_audiorecord (GtkWidget * button);
static void pb_exportaudio (GtkWidget * button);

static DenemoProject *new_project (gboolean);
static void newtab ();

static void create_window (void);

static void populate_opened_recent_menu (void);
static gchar *get_most_recent_file (void);

typedef enum
{
  ACCELS_LOADED = 0x0,
  ACCELS_CHANGED = 0x1 << 0,
  EXTRA_ACCELS_ACTIVE = 0x1 << 1,
  ACCELS_MAY_HAVE_CHANGED = 0x1 << 2
} AccelStatus;

GtkWidget *
get_playalong_button ()
{
  return midiplayalongbutton;
}

GtkWidget *
get_conduct_button ()
{
  return midiconductbutton;
}



static gint scm_eval_status = 0;

static SCM
standard_handler (gchar * data SCM_UNUSED, SCM tag, SCM throw_args SCM_UNUSED)
{
  g_warning ("\nA script error for file/script %s; the throw arguments are\n", data);
  scm_display (throw_args, scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  g_warning ("\nThe tag is\n");
  scm_display (tag, scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  scm_eval_status = -1;
  // g_warning ("Undo will be affected\n");
  //stage_undo(Denemo.gui->movement, ACTION_SCRIPT_ERROR); We don't need this as control will return to activate_script() which will terminate the undo properly, with anything the script has done on the undo stack.
  return SCM_BOOL_F;
}

static SCM
standard_preunwind_proc (void *data, SCM key, SCM parameters)
{
  // Capture the stack here:
  *(SCM *) data = scm_make_stack (SCM_BOOL_T, SCM_EOL);
  return *(SCM *) data;
}

gint
eval_file_with_catch (gchar * filename)
{
  // scm_c_primitive_load(filename);
  SCM captured_stack = SCM_BOOL_F;
  SCM name = scm_from_locale_string (filename);
  scm_eval_status = 0;
  scm_c_catch (SCM_BOOL_T, (scm_t_catch_body) scm_primitive_load, (void *) name, (scm_t_catch_handler) standard_handler, (void *) filename, standard_preunwind_proc, &captured_stack);
  if (captured_stack != SCM_BOOL_F)
    {
#ifdef DEBUG
      scm_display_backtrace (captured_stack, scm_current_error_port (), SCM_BOOL_F, SCM_BOOL_F);
#endif
    }
  return scm_eval_status;
}

gint
call_out_to_guile (const char *script)
{
  scm_eval_status = 0;
  scm_internal_catch (SCM_BOOL_T, (scm_t_catch_body) scm_c_eval_string, (void *) script, (scm_t_catch_handler) standard_handler, (void *) script);
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
define_scheme_literal_variable (const gchar * varname, const gchar * value, gchar * tooltip)
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
execute_scheme (DenemoAction * action, DenemoScriptParam * param)
{
  if (Denemo.ScriptRecording)
    denemo_action_activate (denemo_menusystem_get_action (RecordScript_STRING));
  executeScript ();
}





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
  gchar *templates_dir = g_build_filename (get_system_data_dir (), "templates", NULL);
  gchar *instruments_dir = g_build_filename (get_system_data_dir (), "templates", "instruments", NULL);
  gchar *glyphs_dir = g_build_filename (get_system_data_dir (), COMMANDS_DIR, "bitmaps", NULL);
  gchar *graphics_dir = g_build_filename (get_system_data_dir (), COMMANDS_DIR, "graphics", NULL);
  glyphs_dir = g_strdup_printf ("%s%c", glyphs_dir, G_DIR_SEPARATOR);
  graphics_dir = g_strdup_printf ("%s%c", graphics_dir, G_DIR_SEPARATOR);
  if (filename)
    g_free (filename);

  filename = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, NULL);
  gchar *local_actions_dir = g_strdup_printf ("%s%c", filename, G_DIR_SEPARATOR);
  gchar *local_templates_dir = g_build_filename (get_user_data_dir (TRUE), "templates", NULL);
  gchar *local_instruments_dir = g_build_filename (get_user_data_dir (TRUE), "templates", "instruments", NULL);
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
  DEF_SCHEME_STR ("DENEMO_BIN_DIR", get_system_bin_dir (), "Holds location of denemo executable directory");
  DEF_SCHEME_STR ("DENEMO_ACTIONS_DIR", actions_dir, "Holds location of system-wide Denemo actions directory");
  DEF_SCHEME_STR ("DENEMO_TEMPLATES_DIR", templates_dir, "Holds location of system-wide Denemo templates directory");
  DEF_SCHEME_STR ("DENEMO_INSTRUMENTS_DIR", instruments_dir, "Holds location of system-wide Denemo instrument templates directory");
  DEF_SCHEME_STR ("DENEMO_GLYPHS_DIR", glyphs_dir, "Holds location of system-wide Denemo glyphs directory");
  DEF_SCHEME_STR ("DENEMO_GRAPHICS_DIR", graphics_dir, "Holds location of system-wide Denemo graphics directory");
  DEF_SCHEME_STR ("DENEMO_LILYPOND_DIR", g_build_filename (actions_dir, Denemo.lilypond_include_dir, NULL), "Holds location of Denemo's system-wide  lilypond include files directory");
  DEF_SCHEME_STR ("DENEMO_LOCAL_ACTIONS_DIR", local_actions_dir, "Holds location of Denemo actions directory beneath your home directory");
  DEF_SCHEME_STR ("DENEMO_LOCAL_TEMPLATES_DIR", local_templates_dir, "Holds location of Denemo templates directory beneath your home directory");
  DEF_SCHEME_STR ("DENEMO_LOCAL_INSTRUMENTS_DIR", local_instruments_dir, "Holds location of Denemo instrument templates directory beneath your home directory");
  DEF_SCHEME_STR ("DENEMO_LOCAL_LILYPOND_DIR", g_build_filename (local_actions_dir, "lilypond", NULL), "Holds location of user lilypond include files directory");
  DEF_SCHEME_STR ("DENEMO_HOME_DIR", g_get_home_dir (), "Holds location of user home directory");
  {
    gint i;
    for (i = 0; i < G_N_ELEMENTS (DenemoObjTypeNames); i++)
      DEF_SCHEME_CONST (DenemoObjTypeNames[i], i);
  }
  DEF_SCHEME_STR ("DenemoClickTrack", DENEMO_CLICK_TRACK_NAME, "Holds a name for identifying a click track as the staff name query=denemo_name");

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

  if (!Denemo.non_interactive)
    Denemo.project->movement->undo_guard++;
  

  if (Denemo.prefs.profile->len)
    {
      gchar *name = g_strconcat (Denemo.prefs.profile->str, ".scm", NULL);
      gchar *filename = g_build_filename (get_system_data_dir (), COMMANDS_DIR, name, NULL);
      if (g_file_test (filename, G_FILE_TEST_EXISTS))
        eval_file_with_catch (filename);
      g_free (name);
      g_free (filename);
      score_status (Denemo.project, FALSE);
    }

  load_local_scheme_init ();
  if (!Denemo.non_interactive)
    Denemo.project->movement->undo_guard--;
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
    {
      fprintf (fp, "%s", scheme);
      fclose (fp);
    }
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
  //Denemo.project->movement->undo_guard++;
  GList *dirs = NULL;
  dirs = g_list_append (dirs, g_build_filename (PACKAGE_SOURCE_DIR, COMMANDS_DIR, NULL));
  dirs = g_list_append (dirs, g_build_filename (get_system_data_dir (), COMMANDS_DIR, NULL));

  gchar *filename = find_path_for_file (SCHEME_INIT, dirs);

  g_debug ("System wide denemo.scm %s\n", filename);
  if (g_file_test (filename, G_FILE_TEST_EXISTS))
    eval_file_with_catch (filename);    //scm_c_primitive_load(filename);
  else
    g_warning ("Cannot find Denemo's scheme initialization file denemo.scm");
  g_free (filename);
  //Denemo.project->movement->undo_guard--;
}

/* show the user's preferred view. Assumes all hidden on entry */
static void
show_view_preferences (void)
{
  set_toggle (TogglePlaybackControls_STRING, Denemo.prefs.playback_controls);
  set_toggle (ToggleMidiInControls_STRING, Denemo.prefs.midi_in_controls);
  set_toggle (QuickEdits_STRING, Denemo.prefs.quickshortcuts);
  set_toggle (ToggleToolbar_STRING, Denemo.prefs.toolbar);

 
  //  set_toggle (ToggleLyricsView_STRING, Denemo.prefs.lyrics_pane);

  //Denemo.prefs.lyrics_pane = TRUE;      //ignore pref, does not work.
 
 
  set_toggle (ToggleRhythmToolbar_STRING, Denemo.prefs.rhythm_palette);

  if (!Denemo.prefs.manualtypeset)
    set_toggle (TogglePrintView_STRING, TRUE);
    
  set_toggle (ToggleObjectMenu_STRING, Denemo.prefs.object_palette);

  


  //these menu ones are visible on entry - FIXME is this the array of toolbars below, ending in TRUE?
 // if (!Denemo.prefs.playback_controls)
  //  toggle_playback_controls (NULL, NULL);

  //if (!Denemo.prefs.midi_in_controls)
  //  toggle_midi_in_controls (NULL, NULL);

//  if (!Denemo.prefs.toolbar)
 //   toggle_toolbar (NULL, NULL);

  if (Denemo.prefs.cursor_highlight)
    {
      Denemo.prefs.cursor_highlight = FALSE;
      scheme_highlight_cursor (SCM_BOOL_T);//turns the pref back on and starts a timer to keep the Denemo cursor flashing
    }
}

/*
 * create and populate the keymap - a register of all the Denemo commands with their shortcuts - with the built in commands. this means they come first - scrap this, instead load them with Default.commands.
 */
static void
init_keymap (void)
{
  if (Denemo.map)
    free_keymap (Denemo.map);
  Denemo.map = allocate_keymap ();
//#include "generated/register_commands.h"
}

static gboolean
load_files (gchar ** files)
{
  gboolean ret = FALSE;
  gint i = 0;

  if (!files)
    {
      if (!Denemo.non_interactive)
        newtab ();
      else
        Denemo.project = new_project (TRUE);
      open_for_real (get_most_recent_file (), Denemo.project, FALSE, REPLACE_SCORE);
      return TRUE;
    }

  for (i = 0; files[i]; i++)
    {
      if (!Denemo.non_interactive)
        newtab ();
      else
        Denemo.project = new_project (TRUE);
      open_for_real (files[i], Denemo.project, FALSE, REPLACE_SCORE);
      ret = TRUE;
    }
  return ret;
}

static void
autosave_recovery_check (void)
{
  gchar *autosave_file;
  if (!Denemo.project->autosavename)
    return;
  autosave_file = Denemo.project->autosavename->str;
  if (g_file_test (autosave_file, G_FILE_TEST_EXISTS))
    {

      if (choose_option (_("Denemo was terminated abnormally"), _("Open auto-saved file"), _("Delete auto-saved file")))
        {
          gboolean compare = FALSE;
          if (choose_option (_("Denemo was terminated abnormally"), _("Compare auto-saved file with last saved file"), _("Just open auto-saved file")))
          {
            compare = TRUE;
            newtab ();
          }
          open_for_real (autosave_file, Denemo.project, TRUE, REPLACE_SCORE);
          score_status (Denemo.project, TRUE);
          if (compare)
            call_out_to_guile ("(d-CompareScores '(0 . 1))");
        }
      g_remove (autosave_file);
    }
}



/* Called from main for scheme initialization reasons.
   calls back to finish command line processing
*/
void *
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
  initprefs (); //loads the user's prefs
  if (Denemo.old_user_data_dir != NULL) // if Denemo.old_user_data is not NULL the user has preferred to keep their old values. Copy the templates etc...
    {
        gchar *templates_dir = g_build_filename (get_user_data_dir (TRUE), "templates", NULL);
        gchar *old_templates_dir = g_build_filename (Denemo.old_user_data_dir, "templates", NULL);
        copy_files (old_templates_dir, templates_dir);
    }

  define_scheme_literal_variable ("DenemoUserDataDir", get_user_data_dir (TRUE), "Directory ~/.denemo-x.y.z");
  init_lilypond_buffer ();
  initialize_print_status ();
  //project Initializations
  if (!Denemo.non_interactive)
  if (audio_initialize (&Denemo.prefs))
    g_error ("Failed to initialize audio or MIDI backends");

  if (!Denemo.non_interactive)
    {
      initialize_keystroke_help ();
      init_gdk_cursors ();
      create_window ();
      installPalettes ();

      if (Denemo.prefs.tooltip_timeout)
        {
          g_object_set (gtk_widget_get_settings (Denemo.window), "gtk-tooltip-timeout", Denemo.prefs.tooltip_timeout, NULL);
          g_object_set (gtk_widget_get_settings (Denemo.window), "gtk-tooltip-browse-timeout", Denemo.prefs.tooltip_browse_timeout, NULL);
          g_object_set (gtk_widget_get_settings (Denemo.window), "gtk-tooltip-browse-mode-timeout", Denemo.prefs.tooltip_browse_mode_timeout, NULL);
        }


      Denemo.prefs.mode = INPUTEDIT | INPUTRHYTHM | INPUTNORMAL;        //FIXME must correspond with default in prefops.c

      Denemo.accelerator_status = FALSE;
    }

  //Scheme initializations
  {
    const char prog[] = "(catch #t (lambda () (setlocale LC_ALL \"\")) (lambda _(display \"Locale not supported by the C library. Falling back to default \\\"C\\\" locale.\\n\"(current-error-port))))";
    scm_c_eval_string (prog);
    //scm_setlocale( scm_variable_ref(scm_c_lookup("LC_ALL")), scm_from_locale_string("") );
    create_scheme_identfiers ();

    if (Denemo.prefs.autoupdate)
      fetchcommands (NULL, NULL);

    gint i;

    //ensure (use-modules (ice-9 optargs)) is loaded first #:optional params
    call_out_to_guile ("(use-modules (ice-9 optargs))");
    init_keymap ();

    define_scheme_constants ();
    
    load_default_keymap_file (); //loads scripted commands and their shortcuts

    load_scheme_init ();
    if(!Denemo.non_interactive)
      readHistory ();

 

    gboolean file_loaded = load_files (files);
    
    if (Denemo.scheme_commands) //do any command line scheme after loading files and before running any scheme script file specified on the command line
    {
      //g_print ("Executing '%s'\n", Denemo.scheme_commands);
      call_out_to_guile (Denemo.scheme_commands);
    }

    gchar *initscheme = Denemo.scheme_file;
    if (initscheme)
    {
      if (g_file_test (initscheme, G_FILE_TEST_EXISTS))
        eval_file_with_catch (initscheme);      //scm_c_primitive_load(initscheme);
      else
        g_warning ("Cannot find your scheme initialization file %s", initscheme);
    }
    if (!file_loaded && !Denemo.scheme_commands)
      {
        gchar *code = g_strdup_printf ("(d-InstrumentName \"%s\")", _("Unnamed"));
        call_out_to_guile (code);
        g_free (code);
        denemo_scheme_init ();
      }
  }

  //project related initializations
  if (!Denemo.non_interactive)
    {
		populate_opened_recent_menu ();
		score_status (Denemo.project, FALSE);
		if (Denemo.scheme_commands)
		{
		  g_debug ("(interactive) Executing '%s'", Denemo.scheme_commands);
		  call_out_to_guile (Denemo.scheme_commands);
		}
		else
		autosave_recovery_check ();
		if (Denemo.prefs.fontname->len && Denemo.prefs.fontsize)
		{
		  gchar *fontspec = g_strdup_printf ("%s %d", Denemo.prefs.fontname->str, Denemo.prefs.fontsize);
		  GtkSettings *settings = gtk_settings_get_default ();
		  //gtk_settings_set_string_property (settings, "gtk-font-name", fontspec, "denemo");
		  g_object_set (G_OBJECT(settings), "gtk-font-name", fontspec, NULL);
		  g_free (fontspec);
		}
		finalize_menusystem();
		show_view_preferences ();
		Denemo.project->mode = Denemo.prefs.mode;
		if (Denemo.prefs.startmidiin)
		Denemo.project->input_source = INPUTMIDI;
		if (Denemo.prefs.autosave)
		{
		  if (Denemo.autosaveid==0)
			{
			  Denemo.autosaveid = g_timeout_add_seconds (Denemo.prefs.autosave_timeout, (GSourceFunc) auto_save_document_timeout, Denemo.project);
			}
		}
      gtk_main ();
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
get_clipboard (DenemoAction * action, DenemoScriptParam * param)
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
  project->callbacks = NULL;    //do this before calling the callbacks, so they cannot run twice
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
  g_signal_handlers_block_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_draw_event), NULL);       // turn of refresh of display before destroying the data
  stop_midi_playback (NULL, NULL);      // if you do not do this, there is a timer moving the score on which will hang
  
#ifdef USE_EVINCE
  if (Denemo.prefs.enable_thumbnails)
    create_thumbnail (TRUE, NULL);
#endif
  if (Denemo.project->autosavename)
    g_remove (Denemo.project->autosavename->str);
  if (Denemo.textwindow && gtk_widget_get_visible (Denemo.textwindow))
    {
      set_toggle (ToggleLilyText_STRING, FALSE);
    }
  free_movements (Denemo.project);

  DenemoProject *oldproject = Denemo.project;
  /*
   *      gtk_widget_destroy (Denemo.page);  //note switch_page from g_signal_connect (G_OBJECT(Denemo.notebook), "switch_page", G_CALLBACK(switch_page), NULL);
   * this widget destroy causes critical errors - the sequence of New, New Tab, (return to first tab), Close, New shows the effect.
   */
  gint index = g_list_index (Denemo.projects, oldproject);
  gtk_notebook_remove_page (GTK_NOTEBOOK (Denemo.notebook), index);
  //g_message ("Closing project %d", index);
  Denemo.projects = g_list_remove (Denemo.projects, oldproject);        //FIXME ?? or in the destroy callback??
  g_free (oldproject);
  if (Denemo.projects)
    {
      if (index > g_list_length (Denemo.projects) - 1)
        index = g_list_length (Denemo.projects) - 1;
      if (index < 0)
        index = 0;

      Denemo.project = g_list_nth_data (Denemo.projects, index);
      //g_message ("Selecting score (tab) %d\n", index);
      gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), index);
    }
  else
    Denemo.project = NULL;
  g_signal_handlers_unblock_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_draw_event), NULL);
  return TRUE;
}

/* remove all the movements (ie the DenemoMovement) leaving it with project->movement NULL */
void
free_movements (DenemoProject * project)
{
  gint success;
  if (!is_playing ())
    success = delete_imported_midi ();
  //g_message ("Success %d\n", success);
  GList *g;
  free_scoreblocks (project);
  for (g = project->movements; g; g = g->next)
    {
      project->movement = g->data;
      project->movement->undo_guard = 1;        //no undo as that is per movement
      //close_source_audio ();//???
      //  if(!delete_imported_midi ()) not if still playing!!!
      //    delete_imported_midi();
      free_movement (project);
    }
  project->movement = NULL;
  delete_directives (&project->lilycontrol.directives);
  delete_directives (&project->scoreheader.directives);
  delete_directives (&project->paper.directives);
  g_list_free (project->movements);
  project->movements = NULL;


  /* any other free/initializations */
  project->lilycontrol.papersize = g_string_new ("a4"); //A4 default
  project->lilycontrol.staffsize = g_string_new ("18");
  project->lilycontrol.lilyversion = g_string_new ("");
  project->lilycontrol.orientation = TRUE;      //portrait
}

/**
* Wrapper function to close application when the quit
* menu item has been used
*
*
*/
void
closewrapper (DenemoAction * action, DenemoScriptParam * param)
{
  gint ret = 0;
  if(param && param->string && param->string->len)
    ret = atoi (param->string->str);
  if (!Denemo.non_interactive)
    {
      GList *display;
      gint unsaved = 0;
      for (display = Denemo.projects; display != NULL; display = g_list_next (display))
        {
          DenemoProject *project = (DenemoProject *) display->data;
          if (project->notsaved)
            unsaved++;
        }
      if (unsaved > 1)
        {
          GString *options = g_string_new ("");
          g_string_append_printf (options, "%s%c%s", _("Ask me about each"), '\0', _("Close all without saving"));
          gchar *title = g_strdup_printf (_("You have %d score(s) unsaved"), unsaved);
          gchar *response = get_option (title, options->str, options->len);
          if (response != options->str)
            {
              for (display = Denemo.projects; display != NULL; display = g_list_next (display))
                {
                  DenemoProject *project = (DenemoProject *) display->data;
                  project->notsaved = FALSE;
                }
            }
        }

      for (display = Denemo.projects; display != NULL; display = Denemo.projects)
        {
          Denemo.project = (DenemoProject *) display->data;
          if (close_gui_with_check (NULL, NULL) == FALSE)
            break;
        }
    }
  else
    exit(ret);
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
fetchcommands (DenemoAction * action, DenemoScriptParam * param)
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
morecommands (DenemoAction * action, DenemoScriptParam * param)
{
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
mycommands (DenemoAction * action, DenemoScriptParam * param)
{
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
openinnew (DenemoAction * action, DenemoScriptParam * param)
{
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
close_gui_with_check (DenemoAction * action, DenemoScriptParam * param)
{
  
  DenemoProject *project = Denemo.project;
  Denemo.prefs.mode = Denemo.project->mode;
  if (action_callbacks (Denemo.project))
    return FALSE;               //Denemo.project may have been closed, depends on script callbacks;
  if (Denemo.accelerator_status && !param)
    {
      if (confirm (_("You have made changes to the commands you have"), _("Do you want to save the changes?")))
        save_accels ();
    }
  
  if ( param || (!project->notsaved) || (project->notsaved && confirmbox (project)))
    close_project ();
  else
    return FALSE;

  if (action || param)                   //called as Close not Quit, or scripted
    {
      if (Denemo.projects == NULL)
        newtab ();
      return TRUE;
    }
  if (Denemo.projects == NULL)
    {
      storeWindowState ();
      writeHistory ();
      writeXMLPrefs (&Denemo.prefs);
      writePalettes ();
      // Remove the temporary print directory
      removeprintdir ();
#ifdef G_OS_WIN32
      if (project)
        {
          CoUninitialize ();
          g_message ("Windows - Exiting without shutting down audio");
          if (project->input_source == INPUTMIDI)
            {
              if (confirm (_("MIDI Controller Active?"), _("Please turn off your MIDI keyboard\nif you have not already done so")))
                _exit (0);      //audio shutdown can hang
            }
          else
            _exit (0);
        }
#endif

      g_print ("Exiting directly - may leave notes sounding on audio!");
      _exit (0);
      audio_shutdown ();

      exit (0);                 //do not use gtk_main_quit, as there may be inner loops active.
    }
  return TRUE;
}

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

static void
pb_next (GtkWidget * button)
{
  generate_midi ();
  call_out_to_guile ("(DenemoNext)");
}

static void
pb_go_forward (GtkWidget * button)
{
  generate_midi ();
  call_out_to_guile ("(DenemoGoForward)");
}

static void
pb_start_to_cursor (GtkWidget * button)
{ 
  generate_midi ();
  call_out_to_guile ("(DenemoSetPlaybackStart)");
  //gtk_widget_draw(Denemo.scorearea, NULL);
  draw_score_area ();
  draw_score (NULL);
}

static void
pb_end_to_cursor (GtkWidget * button)
{
  generate_midi ();
  call_out_to_guile ("(DenemoSetPlaybackEnd)");
  //gtk_widget_draw(Denemo.scorearea, NULL);
  draw_score_area ();
  draw_score (NULL);
}

static void
pb_loop (GtkWidget * button)
{
  generate_midi ();
  call_out_to_guile ("(DenemoLoop)");
}

//gets the desired tempo in beats per minute from user, sets the tempo widget, re-computes MIDI and adjusts playback end time
// returns any previously set value if no valid new value is given else defaults to 120
gint movement_tempo_from_user (void)
{
 gchar * value;
 gint bpm = Denemo.project->movement?Denemo.project->movement->tempo:120;
 if ((bpm<10) || (bpm>1000))
	value = g_strdup ("120"); 
 else 
	value = g_strdup_printf ("%d", bpm);
 gchar *newvalue = string_dialog_entry (Denemo.project, "Movement Tempo", "Give value in beats per minute", value);
 if (newvalue)
	{
		bpm = atoi (newvalue);
		g_free (value);
		value = newvalue;
		if (bpm > 10 && bpm < 1000)
			{
				set_movement_tempo (bpm);
				update_tempo_widget (value);
				Denemo.project->movement->smfsync = G_MAXINT;
				exportmidi (NULL, Denemo.project->movement);
				switch_back_to_main_window ();
			}
	}
 else 
	bpm = atoi (value);
 return bpm;
}
static void
pb_mute_staffs ()
{
  call_out_to_guile ("(d-MuteStaffs)");
}

void
update_tempo_widget (gchar *value)
{
 gchar *text =  g_strdup_printf ("𝅘𝅥 = %s bpm", value);
 gtk_label_set_markup (GTK_LABEL(tempo_widget), text);
 g_free (text);
}

#ifdef _HAVE_RUBBERBAND_
static void
set_speed (GtkAdjustment * adjustment)
{
  gdouble speed = gtk_adjustment_get_value (adjustment);
  set_playback_speed (speed);
}
#endif

static void
pb_volume (GtkAdjustment * adjustment)
{
	gdouble volume = gtk_adjustment_get_value (adjustment);
	if (volume < 50)
		{
			fluid_set_gain (volume/500);
			if (Denemo.project->movement->recording)
				Denemo.project->movement->recording->volume = (volume/500);
		}
	else
		{
		fluid_set_gain (0.1 + (volume-50)/20);
		if (Denemo.project->movement->recording)
			Denemo.project->movement->recording->volume = 0.1 + (volume-50)/5;
		}
}



static void
leadin_changed (GtkSpinButton * spin)
{
  if (Denemo.project->movement->recording)
    {
#ifdef USE_AUBIO
      set_lead_in (gtk_spin_button_get_value (spin));
      //g_debug("%d for %f\n", Denemo.project->movement->recording->leadin, gtk_spin_button_get_value(spin));
#endif
    }
}

void
update_leadin_widget (gdouble secs)
{
  gtk_spin_button_set_value (leadin, secs);
}

static void
pb_play_range (GtkWidget * button)
{
  if (Denemo.project->movement->markstaffnum)
    call_out_to_guile ("(DenemoSetPlaybackIntervalToSelection)(d-Play)");
  else
    call_out_to_guile ("(d-DenemoPlayCursorToEnd)");
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
#ifndef G_OS_WIN32  //possible crash on windows?
#ifndef _HAVE_JACK_
   midi_stop ();
   audio_shutdown ();
   audio_initialize (&Denemo.prefs);
#endif
#endif  
  Denemo.project->movement->start_time = 0.0;
  Denemo.project->movement->end_time = -1.0;    //ie unset
  Denemo.project->movement->smfsync = G_MAXINT;
  exportmidi (NULL, Denemo.project->movement);
  fix_start_end_ordering ();
  reset_temperament ();
  draw_score_area ();
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
        text = g_strconcat ("<span foreground=\"blue\">", _("Recording + Play Along"), "</span>", NULL);
      else if (Denemo.project->midi_destination & MIDIRECORD)
        text = g_strconcat ("<span foreground=\"red\">", _("Recording (Off/On)"), "</span>", NULL);
      else if (Denemo.project->midi_destination & MIDIPLAYALONG)
        text = g_strconcat ("<span foreground=\"red\">", _("Play Along"), "</span>", NULL);
      else if ((Denemo.keyboard_state & ~GDK_LOCK_MASK) == (GDK_CONTROL_MASK))
        text = g_strconcat ("<span foreground=\"#808000\" size=\"larger\">", _("Checking Pitches"), "</span>", NULL);
      else if ((Denemo.keyboard_state == (GDK_SHIFT_MASK)) || (Denemo.keyboard_state == (GDK_LOCK_MASK)))
        text = g_strconcat ("<span foreground=\"#008080\" size=\"larger\">", _("Listening to Pitches"), "</span>", NULL);
      else if ((Denemo.keyboard_state & CHORD_MASK))
        text = g_strconcat ("<span foreground=\"#000000\">", _("Editing a Chord"), "</span>", NULL);
      else if ((Denemo.keyboard_state & ADDING_MASK))
        text = g_strconcat ("<span foreground=\"#000000\">", _("Starting a Chord"), "</span>", NULL);
      else
        text = g_strconcat ("<span foreground=\"#000000\">", _("Appending/Editing Pitches"), "</span>", NULL);
      gtk_label_set_markup (GTK_LABEL (midi_in_status), text);
      g_free (text);
    }
}

void
midi_in_adjust (gint value)
{
  if (value>0)
	Denemo.keyboard_state = value;
  Denemo.keyboard_state_locked = value; //lock if set to listening or checking or recording
  if (Denemo.project->movement->recording && (Denemo.project->midi_destination & MIDIRECORD))
	toggle_midi_record ();
  set_midi_in_status ();
  switch_back_to_main_window ();
}


static void
midi_in_menu (void)
{
  GtkWidget *menu = gtk_menu_new ();
  GtkWidget *item = gtk_menu_item_new_with_label (_("Checking Pitches"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (midi_in_adjust), GINT_TO_POINTER (GDK_CONTROL_MASK));

  item = gtk_menu_item_new_with_label (_("Listening to Pitches"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (midi_in_adjust), GINT_TO_POINTER (GDK_SHIFT_MASK));

  item = gtk_menu_item_new_with_label (_("Appending/Editing Pitches"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (midi_in_adjust), 0);
  
  item = gtk_menu_item_new_with_label (_("Recording"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (toggle_midi_record), GINT_TO_POINTER (-1));
  
  
  gtk_widget_show_all (menu);
  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
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


void highlight_audio_record (void)
{
  static gboolean on;
  on = !on;
	gtk_label_set_markup(GTK_LABEL (audiorecordbuttonlabel), on? 
			"<span foreground=\"black\"><b> ▇ </b></span>":
			"<span foreground=\"red\"><b>●</b></span>");
}


static void
pb_audiorecord (GtkWidget * button)
{  
  gtk_label_set_markup(GTK_LABEL (audiorecordbuttonlabel),  "<span foreground=\"red\"><b>●</b></span>");
  if (Denemo.prefs.maxrecordingtime)
    {
      Denemo.project->audio_recording = !Denemo.project->audio_recording;
      if (!Denemo.project->audio_recording)
        gtk_widget_show (exportbutton);
    }
  else
    {
      warningdialog (_("The preference set for recording time is 0 - nothing can be recorded.\nSee Edit → Change Preferences Audio/Midi Tab"));
    }
}

void toggle_recording_audio (void)
{
	pb_audiorecord (NULL);
}

static void
pb_exportaudio (GtkWidget * button)
{
  if (!Denemo.project->audio_recording)
    Denemo.project->audio_recording = FALSE;
  export_recorded_audio (NULL);
}

void pb_midi_delete (void)
{
  DenemoRecording *recording = Denemo.project->movement->recording;
  if (recording)
    {
      if (recording->type != DENEMO_RECORDING_MIDI)
        {
          g_warning ("Cannot delete Audio yet");
          return;               //see sourceaudio.c:222 for deleting audio
        }
      track_delete (Denemo.project->movement->recorded_midi_track);
      Denemo.project->movement->recorded_midi_track = NULL;

      delete_recording ();
    }
  gtk_widget_queue_draw (Denemo.scorearea);
}

static void pb_midi_record_help (void)
{
	popup_menu ("Recording");
}
static void
pb_midi_convert (GtkWidget * button)
{

  call_out_to_guile ("(DenemoConvert)");

  g_info ("Finished midi convert");
}

#define CURRP ((RhythmPattern *)project->currhythm->data)
#define LABEL_WIDGET_OF_TOOLBUTTON(a) (gtk_tool_button_get_label_widget((a)))

void
set_rhythm_label (RhythmPattern * r, gchar * text)
{
  DenemoProject *project = Denemo.project;
  GtkWidget *label = LABEL_WIDGET_OF_TOOLBUTTON (CURRP->button);
  gchar *labelstr;
  if (r->nickname && r->nickname->len)
    labelstr = g_strconcat (text, "\n", r->nickname->str, NULL);
  else
    labelstr = g_strdup (text);
  //g_debug("markup is %s\n", ((RhythmElement*)g->data)->icon);
  gtk_label_set_markup (GTK_LABEL (label), labelstr);
  g_free (labelstr);
}

/**
 * Rhythm callback select rhythm
 * inserts the rhythm if pitchless
 */
void
select_rhythm_pattern (RhythmPattern * r)
{
  DenemoProject *project = Denemo.project;


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

      gchar *text = ((RhythmElement *) project->rstep->data)->highlightlabel;
      if (text)
        {
          set_rhythm_label (r, text);
        }
      highlight_rhythm (CURRP);
    }
#undef CURRP
}

#undef LABEL_WIDGET_OF_TOOLBUTTON
static void
insert_and_select_snippet (RhythmPattern * r)
{
  select_rhythm_pattern (r);
  if ((Denemo.project->mode & INPUTEDIT))
    {
      insert_clipboard (r->clipboard);
    }
}

static void
insert_snippet (RhythmPattern * r)
{

  if ((Denemo.project->mode & INPUTEDIT))
    {
      insert_clipboard (r->clipboard);
    }
}

static void rename_snippet (RhythmPattern * r);

static void
activate_rhythm_pattern (GtkToolButton * toolbutton, RhythmPattern * r)
{
  GtkWidget *menu = gtk_menu_new ();
  GtkWidget *item = gtk_menu_item_new_with_label (_("Select and Reset Snippet"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (select_rhythm_pattern), r);

  item = gtk_menu_item_new_with_label (_("Insert Snippet at Cursor"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (insert_snippet), r);
  item = gtk_menu_item_new_with_label (_("Re-label Snippet"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (rename_snippet), r);

  item = gtk_menu_item_new_with_label (_("Insert and Select"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (insert_and_select_snippet), r);

  item = gtk_menu_item_new_with_label (_("Delete Snippet"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (delete_rhythm_pattern), r);
  gtk_widget_show_all (menu);
  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
}

gboolean
insert_nth_rhythm (gint n)
{

  gint nr = g_list_length (Denemo.project->rhythms);
  if (n >= 0 && n < nr)
    {
      insert_clipboard (((RhythmPattern *) (g_list_nth (Denemo.project->rhythms, n))->data)->clipboard);
      return TRUE;
    }
  return FALSE;
}

static void
null_action ()
{
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
  return fn == (gpointer) triplet_start ? '~' :
    fn == (gpointer) tuplet_end ? '|' :
    fn == (gpointer) add_dot_key ? '.' :
    fn == (gpointer) toggle_begin_slur ? '(' :
    fn == (gpointer) toggle_end_slur ? ')' : fn == (gpointer) insert_rest_0key ? 'r' : fn == (gpointer) insert_rest_1key ? 's' : fn == (gpointer) insert_rest_2key ? 't' : fn == (gpointer) insert_rest_3key ? 'u' : fn == (gpointer) insert_rest_4key ? 'v' : fn == (gpointer) insert_rest_5key ? 'w' : fn == (gpointer) insert_rest_6key ? 'x' : fn == (gpointer) insert_rest_7key ? 'y' : fn == (gpointer) insert_rest_8key ? 'z' : fn == (gpointer) null_action ? 'S' : 0;
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
  DenemoMovement *si;
  if (project->movement)
    si = project->movement;
  if (si->markstaffnum)
    {
      push_clipboard ();
      copytobuffer (si);
      push_clipboard ();
      r->clipboard = pop_off_clipboard ();
      remove_breaks (r->clipboard);
      pop_clipboard (0);
    }
}


gint
insert_pattern_in_toolbar (RhythmPattern * r, gboolean highlight)
{
  if (Denemo.non_interactive)
    return -1;
  DenemoProject *project = Denemo.project;
  if (r->clipboard == NULL)
    {
      g_warning ("No clipboard for this pattern, cannot add");
      return -1;
    }
  GtkWidget *toolbar = denemo_menusystem_get_widget ("/RhythmToolBar");
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), GTK_TOOL_ITEM (r->button), -1);
  gtk_widget_show_all (GTK_WIDGET (r->button));

  project->rhythms = g_list_append (project->rhythms, r);
  if (highlight)
    {
      project->rstep = r->rsteps;
      project->cstep = r->clipboard->data;
      if (project->currhythm)
        unhighlight_rhythm ((RhythmPattern *) project->currhythm->data);
      project->currhythm = g_list_last (project->rhythms);
      highlight_rhythm ((RhythmPattern *) project->currhythm->data);
    }
  g_signal_connect (G_OBJECT (r->button), "clicked", G_CALLBACK (activate_rhythm_pattern), (gpointer) r);
  return g_list_length (project->rhythms);      //the index of the newly added snippet
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

static GString *
shorten_string (gchar * str)
{
  GString *ret = g_string_new ("");
  for (; *str; str++)
    {
      switch (*str)
        {
        case '\n':
        case '\t':
        case ' ':
          continue;
        default:
          g_string_append_c (ret, *str);

        }
      if (ret->len > 8)
        break;
    }
  return ret;
}

/*
 * curobj is a list of DenemoObject,
 * creates a RhythmPattern
 * and the ASCII pattern that is used by the caller to generate a label on the snippet button and a set of highlighted labels for each step of the rhythm pattern.
 * accumulates the lilypond field of the rhythm pattern from the lilypond fields of the objects
 */
static void
create_rhythm_and_pattern (GList * curobj, RhythmPattern * r, GString * pattern)
{
  if (r->lilypond == NULL)
    r->lilypond = g_string_new ("");
  for (; curobj; curobj = curobj->next)
    {
      gpointer fn;
      DenemoObject *obj = (DenemoObject *) curobj->data;
      switch (obj->type)
        {
        case TUPCLOSE:
          fn = (gpointer) tuplet_end;
          g_string_append_c (pattern, '|');
          append_rhythm (r, fn);
          break;
        case TUPOPEN:
          switch (((tupopen *) obj->object)->denominator)
            {
            case 3:
              fn = (gpointer) triplet_start;
              g_string_append_c (pattern, '~');
              break;
            default:           // need to create start_xxxtuplet() functions to go with triplet_start(), then they can go here.
              fn = NULL;
            }
          append_rhythm (r, fn);
          break;
        case CHORD:
          {
            chord *ch = (chord *) obj->object;

            if (ch->notes)
              {
				gint baseduration = ch->baseduration;
				if (baseduration<-7)
					baseduration = 0;//ie use insert_chord_0key: breve etc are actually semibreves with a Directive attached, however it means the label for the snippet uses a semibreve glyph FIXME
				else
					if (baseduration<0)
						baseduration = -baseduration;//a swung note
                switch (baseduration)
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
                g_string_append_c (pattern, duration_code (fn));
                append_rhythm (r, fn);
              }
            else
              {                 /* a rest */
				gint baseduration = ch->baseduration;
				if (baseduration<-7)
					baseduration = 0;
				else
					if (baseduration<0)
						baseduration = -baseduration;//a swung rest if such a thing were created
                switch (baseduration)
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
                g_string_append_c (pattern, modifier_code (fn));
                append_rhythm (r, fn);
              }                 /* end of rests */
            gint i;
            for (i = ch->numdots; i; i--)
              {
                fn = add_dot_key;
                g_string_append_c (pattern, modifier_code (fn));
                append_rhythm (r, fn);
              }
            if (ch->slur_begin_p)
              {
                fn = (gpointer) toggle_begin_slur;
                g_string_append_c (pattern, '(');
                append_rhythm (r, fn);
              }
            if (ch->slur_end_p)
              {
                fn = (gpointer) toggle_end_slur;
                g_string_append_c (pattern, ')');
                append_rhythm (r, fn);
              }
            //FIXME other built-ins here - cresc endcresc ...
          }
          break;
        case LILYDIRECTIVE:
          {
            DenemoDirective *direc = (DenemoDirective *) obj->object;
            fn = (gpointer) null_action;
            if ((curobj->prev == NULL) && (curobj->next == NULL) && (r->nickname == NULL))
              {
                if (direc->display)
                  r->nickname = shorten_string (direc->display->str);
                else if (direc->graphic_name)
                  r->nickname = shorten_string (direc->graphic_name->str);
                else if (direc->postfix)
                  r->nickname = shorten_string (direc->prefix->str);
                else
                  r->nickname = g_string_new ("S");
              }
            g_string_append_c (pattern, 'S');
            append_rhythm (r, fn);
          }
          break;
        default:
          {
            fn = null_action;
            g_string_append_c (pattern, 'S');
            append_rhythm (r, fn);
          }
          //g_warning("ignoring %d", obj->type);
          break;
        }                       /* end of switch obj type */
      //g_debug("Number of rhythms %d\n", g_list_length(r->rsteps));
      if (obj->lilypond)
        g_string_append (r->lilypond, obj->lilypond);
    }                           /* End object loop */
}

//create values for highlightlabel field of the r->steps from the pattern
void
fill_in_steps (RhythmPattern * r, GString * pattern)
{
  /* fill the r->rsteps with highlightlabels for each step */
  GList *g;
  RhythmElement *el;
  gint i;
  for (g = r->rsteps, i = 0; g; g = g->next, i++)
    {
      el = (RhythmElement *) g->data;
      if (i == 0 && (*(pattern->str) < '0' || *(pattern->str) > '8') && g->next)
        g = g->next;            // pattern does not start with a note (0 to 8), so we skip to the second element, unless there are no notes
      while (*(pattern->str + i) && (*(pattern->str + i) < '0' || *(pattern->str + i) > '8'))
        i++;
      if (*(pattern->str + i))
        {
          *(pattern->str + i) += HIGHLIGHT_OFFSET;
          el->highlightlabel = music_font (pattern->str);
          *(pattern->str + i) -= HIGHLIGHT_OFFSET;
        }
      //g_debug("el->highlightlabel = %s step %d pattern->str %s\n", el->highlightlabel, i, pattern->str);
    }
}


static void
rename_rhythm (RhythmPattern * r, gchar * name)
{
  GtkWidget *label = gtk_tool_button_get_label_widget (r->button);
  if (r->nickname == NULL)
    r->nickname = g_string_new (r->name);
  if (name == NULL)
    name = string_dialog_entry (Denemo.project, _("Rename Music Snippet"), _("Give new label for snippet"), r->nickname->str);
  if (name && *name)
    {
      g_string_assign (r->nickname, name);
      gtk_label_set_markup (GTK_LABEL (label), r->nickname->str);
    }
}

static void
rename_snippet (RhythmPattern * r)
{
  rename_rhythm (r, NULL);
}

//create a rhythm pattern (aka snippet) from r->clipboard or the selection
//put the rhythm pattern in the Denemo.project and on the snippets toolbar
//highlight the created rhythm if from the selection
void
create_rhythm (RhythmPattern * r, gboolean from_selection)
{
  GString *pattern = g_string_new ("");
  install_button_for_pattern (r, NULL);
  if (from_selection)
    attach_clipboard (r);

  if (r->clipboard)
    create_rhythm_and_pattern ((GList *) ((GList *) r->clipboard)->data, r, pattern);
  gchar *labelstr;
  labelstr = music_font (pattern->str);
  GtkWidget *label = gtk_tool_button_get_label_widget (r->button);
  gtk_label_set_markup (GTK_LABEL (label), labelstr);
  g_free (labelstr);
  fill_in_steps (r, pattern);
  if (r->rsteps)
    {
      /* make the list circular */
      r->rsteps->prev = g_list_last (r->rsteps);
      g_list_last (r->rsteps)->next = r->rsteps;
    }
  insert_pattern_in_toolbar (r, from_selection);
  if (r->nickname)
    rename_rhythm (r, r->nickname->str);
  if (!from_selection)
    unhighlight_rhythm (r);
  g_string_free (pattern, TRUE);
}


/* create_rhythm_cb
        - create a rhythm pattern from the current selection
        - clipboard field is pointed to the selected objects
        - a button is created in "/RhythmToolbar"
        - and the pattern is added to project->rhythms
        - with the first step of it put in project->rstep
        - the rhythm becomes the prevailing_rhythm, setting cstep

*/
void
create_rhythm_cb (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *project = Denemo.project;

  DenemoMovement *si = project->movement;
  RhythmPattern *r;

  if (!si->markstaffnum)        /* Indicator that there's a selection.  */
    {
      warningdialog (_("No selection to create a music snippet from\nSee Edit → Select menu for selecting music to snip"));
      return;
    }

  r = (RhythmPattern *) g_malloc0 (sizeof (RhythmPattern));


  if (project->lilysync != project->changecount)
    refresh_lily_cb (NULL, Denemo.project);     /* ensure lilypond syntax attached to objects in selection */
  create_rhythm (r, TRUE);

}

// create a rhythm pattern for a standard duration note or rest action is the insert_chord or rest for the appropriate duration.
static void
create_singleton_rhythm (gpointer insert_fn)
{
  DenemoProject *project = Denemo.project;
  gboolean already_done = FALSE;        // a singleton which has already been installed globally, that is a new tab is being opened.
  gboolean default_rhythm = FALSE;
  DenemoMovement *si = project->movement;
  RhythmPattern *r = (RhythmPattern *) g_malloc0 (sizeof (RhythmPattern));
  GString *pattern = g_string_new ("");
  if (insert_fn == (gpointer) insert_chord_0key)
    pattern = g_string_assign (pattern, "0");
  if (insert_fn == (gpointer) insert_chord_1key)
    pattern = g_string_assign (pattern, "1");
  if (insert_fn == (gpointer) insert_chord_2key)
    pattern = g_string_assign (pattern, "2"), default_rhythm = TRUE;
  if (insert_fn == (gpointer) insert_chord_3key)
    pattern = g_string_assign (pattern, "3");
  if (insert_fn == (gpointer) insert_chord_4key)
    pattern = g_string_assign (pattern, "4");
  if (insert_fn == (gpointer) insert_chord_5key)
    pattern = g_string_assign (pattern, "5");
  if (insert_fn == (gpointer) insert_chord_6key)
    pattern = g_string_assign (pattern, "6");
  if (insert_fn == (gpointer) insert_chord_7key)
    pattern = g_string_assign (pattern, "7");
  if (insert_fn == (gpointer) insert_chord_8key)
    pattern = g_string_assign (pattern, "8");

  if (insert_fn == (gpointer) insert_rest_0key)
    pattern = g_string_assign (pattern, "r");
  if (insert_fn == (gpointer) insert_rest_1key)
    pattern = g_string_assign (pattern, "s");
  if (insert_fn == (gpointer) insert_rest_2key)
    pattern = g_string_assign (pattern, "t");
  if (insert_fn == (gpointer) insert_rest_3key)
    pattern = g_string_assign (pattern, "u");
  if (insert_fn == (gpointer) insert_rest_4key)
    pattern = g_string_assign (pattern, "v");
  if (insert_fn == (gpointer) insert_rest_5key)
    pattern = g_string_assign (pattern, "w");
  if (insert_fn == (gpointer) insert_rest_6key)
    pattern = g_string_assign (pattern, "x");
  if (insert_fn == (gpointer) insert_rest_7key)
    pattern = g_string_assign (pattern, "y");
  if (insert_fn == (gpointer) insert_rest_8key)
    pattern = g_string_assign (pattern, "z");
  if (pattern->len <= 0)
    {
      g_critical ("Bad call to create_singleton_rhythm");
      g_string_free (pattern, TRUE);
      return;
    }
  /* if we already have it globally we don't need it again
     note we never delete the singleton rhythms */
  if (Denemo.singleton_rhythms[(unsigned int) *pattern->str])
    {
      g_free (r);
      r = Denemo.singleton_rhythms[(unsigned int) *pattern->str];
      already_done = TRUE;
    }
  else
    {
      Denemo.singleton_rhythms[(unsigned int) *pattern->str] = r;
      already_done = FALSE;
    }

  if (!already_done)
    {
      gchar *labelstr;
      append_rhythm (r, insert_fn);
      labelstr = music_font (pattern->str);
      g_free (labelstr);

      /* make the list circular */
      r->rsteps->prev = g_list_last (r->rsteps);
      g_list_last (r->rsteps)->next = r->rsteps;
    }
  if (default_rhythm)
    {
      project->prevailing_rhythm = r;
      project->rstep = r->rsteps;
      project->cstep = NULL;
      highlight_rhythm (r);
    }
  g_string_free (pattern, TRUE);
}


void
save_accels (void)
{
  save_default_keymap_file ();
  Denemo.accelerator_status = FALSE;
}


static void show_type (GtkWidget * widget, gchar * message);


//static void toggleRecording (GtkWidget*w, gboolean *record) {
//g_debug("Recording was %d\n", *record);
//  *record = !*record;
//}



static void attach_right_click_callback (GtkWidget * widget, DenemoAction * action);

/* the callback for menu items that are scripts. The script is attached to the action,
tagged as "scheme".
The script may be empty, in which case it is fetched from actions/menus...

This call also ensures that the right-click callback is attached to all the proxies of the action, as there are problems trying to do this earlier, and it defines a scheme variable to give the name of the script being executed.
*/
gboolean
activate_script (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *project = Denemo.project;
  gchar *name = (gchar *) denemo_action_get_name (action);
  gint idx = lookup_command_from_name (Denemo.map, name);
  gboolean ret = FALSE;

    {
      //FIXME use define_scheme_variable for this
      //define a global variable in Scheme (CurrentScript) to give the name of the currently executing script
      const gchar *name = denemo_action_get_name (action);
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

      gchar *text = get_scheme_from_idx (idx);
      if (!is_action_name_builtin ((gchar *) denemo_action_get_name (action)))
        {
          if (text)
            {
              stage_undo (project->movement, ACTION_STAGE_END); //undo is a queue so this is the end :)
              ret = (gboolean) ! call_out_to_guile (text);
              stage_undo (project->movement, ACTION_STAGE_START);
            }
          else
            {
              g_warning ("Could not get script for %s", denemo_action_get_name (action));
              ret = FALSE;
            }
        }
      if (paramvar)
        scm_c_define (paramvar, SCM_BOOL_F);
      g_free (paramvar);
    }
  return ret;
}




/*pop up the help for passed command as info dialog
 */
void
popup_help_for_action (DenemoAction * action)
{
  const gchar *name = denemo_action_get_name (action);
  gint idx = lookup_command_from_name (Denemo.map, name);
  gchar *tooltip = idx >= 0 ? (gchar *) lookup_tooltip_from_idx (Denemo.map, idx) : "A menu for ...";

  tooltip = g_strdup_printf (_("Command: %s\n\nInformation:\n%s"), name, tooltip);
  infodialog (tooltip);
  g_free (tooltip);
}




static void
placeOnButtonBar (GtkWidget * widget, DenemoAction * action)
{
  gchar *name = (gchar *) denemo_action_get_name (action);
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




void
append_scheme_call (gchar * func)
{
  if (strcmp (func, "ExecuteScheme"))
    {
      GtkTextIter enditer;
      GtkTextBuffer *buffer = gtk_text_view_get_buffer ((GtkTextView *) (Denemo.script_view));
      //gtk_text_buffer_set_text(buffer,"",-1);
      gtk_text_buffer_get_end_iter (buffer, &enditer);
      gchar *text = g_strdup_printf ("(d-%s)\n", func); //prefix dnm_!!!!!!!
      gtk_text_buffer_insert (buffer, &enditer, text, -1);
      //g_debug("Added %s\n", text);
      g_free (text);
      if (Denemo.prefs.immediateplayback)
        play_note (DEFAULT_BACKEND, 0, 9, 58, 300, 127 * Denemo.project->movement->master_volume);
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




/* upload the action,
   from the user's menu hierarchy on disk, along with initialization script and menu item xml etc
*/
#ifdef UPLOAD_TO_DENEMO_DOT_ORG
static void
uploadMenuItem (GtkWidget * widget, DenemoAction * action)
{
  gchar *name = (gchar *) denemo_action_get_name (action);
  gint idx = lookup_command_from_name (Denemo.map, name);

  command_row *row = NULL;
  keymap_get_command_row (the_keymap, &row, idx);
  gchar *tooltip = (gchar *) lookup_tooltip_from_idx (Denemo.map, idx);
  gchar *label = (gchar *) lookup_label_from_idx (Denemo.map, idx);

  gchar *filename = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "menus", row->menupath, name,
                                      NULL);
  gchar *script = get_scheme_from_idx (idx);
  gchar *xml;
  GError *error = NULL;
  g_file_get_contents (filename, &xml, NULL, &error);
  filename = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "menus", row->menupath, INIT_SCM, NULL);
  gchar *init_script;
  g_file_get_contents (filename, &init_script, NULL, &error);

  if (xml == NULL)
    xml = "";
  if (init_script == NULL)
    init_script = "";
  if (script == NULL)
    script = "";


  upload_scripts (name, script, init_script, xml, row->menupath, label, tooltip, row->after);

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
locategraphicsdir (void)
{
  static gchar *bitmapsdir = NULL;
  gboolean err;
  if (!bitmapsdir)
    {
      bitmapsdir = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "graphics", NULL);
    }
  err = g_mkdir_with_parents (bitmapsdir, 0770);
  if (err)
    {
      warningdialog (_("Could not create .denemo-xxx/actions/graphics for your graphics for customized commands"));
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
              filename = g_build_filename (get_system_data_dir (), COMMANDS_DIR, "graphics", pngname, NULL);
              if (!g_file_test (filename, G_FILE_TEST_EXISTS))
                {
                  g_free (filename);
                  g_free (pngname);
                  return NULL;
                }
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
  int width, height, rowstride, n_channels;
  guchar *pixels;

  n_channels = gdk_pixbuf_get_n_channels (pixbuf);

#ifdef DEBUG
  //g_assert (gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);
  //g_assert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);
  //g_assert (gdk_pixbuf_get_has_alpha (pixbuf));
  //g_assert (n_channels == 4);
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
          gboolean emmentaler = FALSE;
          if (thesize.width == 1000)
            emmentaler = TRUE;
          surface = cairo_svg_surface_create_for_stream (NULL, NULL, (double) (thesize.width), (double) (thesize.height));
          cairo_t *cr = cairo_create (surface);
          
          if (emmentaler) //tFIXME this hack identifies svgs that were extracted from the emmentaler font some years ago and which no longer load at the correct scale/location, presumably because of changes to librsvg/cairo. It results in fuzzy glyphs.
            {
              cairo_translate (cr, (double) (thesize.width)/2 -10,(double) (thesize.height)/2); //g_print ("%f %f tramslate\n",(double) (thesize.width)/2,(double) (thesize.height)/2);
              cairo_scale (cr, 33.3/(double) (thesize.width)    , -33.3/(double) (thesize.height));
            }
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
          size_t n;
          fseek (fp, 16, SEEK_SET);
          n = fread (&thesize.width, 4, 1, fp);
          if (n != 1)
            {
              thesize.width = 40;
            }
          else
            {
              thesize.width = GINT_FROM_BE (thesize.width);
            }
          n = fread (&thesize.height, 4, 1, fp);
          if (n != 1)
            {
              thesize.height = 40;
            }
          else
            {
              thesize.height = GINT_FROM_BE (thesize.height);
            }
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

  GList *files = NULL;
  files = g_list_append (files, g_strconcat (name, ".png", NULL));
  files = g_list_append (files, g_strconcat (name, ".svg", NULL));

  GList *dirs = NULL;
  dirs = g_list_append (dirs, g_build_filename (locategraphicsdir (), NULL));
  dirs = g_list_append (dirs, g_build_filename (locatebitmapsdir (), NULL));
  dirs = g_list_append (dirs, g_build_filename (locatedownloadbitmapsdir (), NULL));
  dirs = g_list_append (dirs, g_build_filename (PACKAGE_SOURCE_DIR, COMMANDS_DIR, "bitmaps", NULL));
  dirs = g_list_append (dirs, g_build_filename (get_system_data_dir (), COMMANDS_DIR, "bitmaps", NULL));
  dirs = g_list_append (dirs, g_build_filename (get_system_data_dir (), COMMANDS_DIR, "graphics", NULL));

  gboolean success = TRUE;
  gchar *dir = find_dir_for_files (files, dirs);
  if (!dir)
    {
      g_warning ("Could not find graphic item %s", name);
      success = FALSE;
    }

  else
    {
      gchar *basename = g_build_filename (dir, name, NULL);
      success = loadGraphicFromFormat (name, basename, xbm);
      g_free (basename);

      if (!success)
        g_warning ("Could not load graphic item %s from %s", name, dir);
    }

  g_free (dir);

  return success;
}

/* save the current graphic
*/
static void
saveGraphicItem (GtkWidget * widget, DenemoAction * action)
{
  gchar *name = (gchar *) denemo_action_get_name (action);
  gchar *pngname = g_strconcat (name, ".png", NULL);
  gchar *filename = g_build_filename (locatebitmapsdir (), pngname,
                                      NULL);
  //FIXME allow fileselector here to change the name
  gchar *msg = g_strdup_printf (_("Saving a graphic for use in the %s script"), name);
  if (!g_file_test (filename, G_FILE_TEST_EXISTS) || confirm (msg, _("Replace current graphic?")))
    {
      guint width = Denemo.project->xbm_width;
      guint height = Denemo.project->xbm_height;
      create_bitmap_from_data (Denemo.project->xbm, width, height);
    }

  g_free (pngname);
  g_free (msg);
  g_free (filename);
}




static void
color_rhythm_button (RhythmPattern * r, const gchar * color) // only black and gray
{
  if ((r == NULL) || (r->button == NULL))
    return;
#if GTK_MAJOR_VERSION==2
  GdkColor thecolor;
  gdk_color_parse (color, &thecolor);
  gtk_widget_modify_fg (gtk_tool_button_get_label_widget (GTK_TOOL_BUTTON (r->button)), GTK_STATE_NORMAL, &thecolor);
  //bg does not work, and setting the label in a GtkEvent box gave a problem on some build - R.Rankin patched for this and so we have to use fg
#else  
    set_foreground_color(gtk_tool_button_get_label_widget (GTK_TOOL_BUTTON (r->button)), strcmp (color, "gray")?"#000000":"#cc6666");
#endif  
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
  if (project->currhythm)
    {
      unhighlight_rhythm ((RhythmPattern *) project->currhythm->data);
    }
  gint nb_sr = 'r' + dur;
  if (nb_sr < 0 || nb_sr > NB_SINGLETON_RHYTHMS)
    {
      g_critical ("Singleton rhythm index out of range");
      return;
    }
  if (!Denemo.singleton_rhythms[nb_sr])
    {
      g_debug ("Unvalid singleton rhythm %i", nb_sr);
      return;
    }
  project->currhythm = NULL;
  project->cstep = NULL;
  project->rstep = Denemo.singleton_rhythms[nb_sr]->rsteps;
  unhighlight_rhythm (project->prevailing_rhythm);
  project->prevailing_rhythm = Denemo.singleton_rhythms[nb_sr];
  highlight_rhythm (project->prevailing_rhythm);

}

void
highlight_duration (DenemoProject * project, gint dur)
{
  if (project->currhythm)
    {
      unhighlight_rhythm ((RhythmPattern *) project->currhythm->data);
    }
  gint nb_sr = '0' + dur;
  if (nb_sr < 0 || nb_sr > NB_SINGLETON_RHYTHMS)
    {
      g_critical ("Singleton rhythm index out of range");
      return;
    }
  if (!Denemo.singleton_rhythms[nb_sr])
    {
      g_debug ("Unvalid singleton rhythm %i", nb_sr);
      return;
    }
  project->currhythm = NULL;
  project->cstep = NULL;
  project->rstep = Denemo.singleton_rhythms[nb_sr]->rsteps;
  unhighlight_rhythm (project->prevailing_rhythm);
  project->prevailing_rhythm = Denemo.singleton_rhythms[nb_sr];
  highlight_rhythm (project->prevailing_rhythm);
  write_status (project);
}



void
delete_rhythm_pattern (RhythmPattern * r)
{
  DenemoProject *project = Denemo.project;
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
  project->rhythms = g_list_remove (project->rhythms, r);       //project->currhythm->data);
  //g_debug("length %d %p\n", g_list_length(project->rhythms), project->rhythms);
  project->currhythm = g_list_last (project->rhythms);

  if (project->currhythm == NULL)
    {
      create_singleton_rhythm ((gpointer) insert_chord_2key);   //to re-set the default rhythm which is 1/4 note
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
 * delete a rhythmic pattern and its button
 *
 */
void
delete_rhythm_cb (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *project = Denemo.project;
  if ((project->mode & (INPUTEDIT)) == 0)
    return;
  if (project->currhythm == NULL)
    return;
  RhythmPattern *r = (RhythmPattern *) project->currhythm->data;
  delete_rhythm_pattern (r);
}


/*
 * workaround for glib<2.10
 */
/* UNUSED
static void
attach_action_to_widget (GtkWidget * widget, DenemoAction * action, DenemoProject * project)
{
  g_object_set_data (G_OBJECT (widget), "action", action);
}
*/
/* attaches a button-press-event signal to the widget with the action as data
   for use in the callback */


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




DenemoAction *
activate_action (gchar * path)
{
  DenemoAction *a;
  a = denemo_menusystem_get_action (path);
  if (a)
    denemo_action_activate (a);
  else
    g_warning ("No command at %s - should this be in denemoui.xml?", path);
  return a;
}


struct cbdata
{
  DenemoProject *project;
  gchar *filename;
};

/**
 * Add history entry to the History menu, create a menu item for it
 * Do not add it if it no longer exists, and no gzipped version exists.
 */
void
addhistorymenuitem (gchar * filename)
{
    static gboolean init = FALSE;
  if (!g_file_test (filename, G_FILE_TEST_EXISTS))
    {
        gchar *zip = g_strconcat (filename, ".gz", NULL);
        if (!g_file_test (zip, G_FILE_TEST_EXISTS))
            {
                g_free (zip);
                return;
            }
            g_free(zip);
    }
  GtkWidget *item ;//= gtk_ui_manager_get_widget (Denemo.ui_manager,"/MainMenu/FileMenu/OpenMenu/OpenRecent/Browse");
  //GtkWidget *menu = gtk_widget_get_parent (GTK_WIDGET (item));
  if(!init)
        {
        instantiate_menus  ("/MainMenu/FileMenu/OpenMenu/OpenRecent");
        init = TRUE;
        }
  GtkWidget *menu = denemo_menusystem_get_widget ("/MainMenu/FileMenu/OpenMenu/OpenRecent");
  {
    static gboolean positioned = FALSE;
    if (!positioned)
        {
        GList *g;
        GtkWidget *open_menu = denemo_menusystem_get_widget ("/MainMenu/FileMenu/OpenMenu");
        GList *children = gtk_container_get_children (GTK_CONTAINER(open_menu));
        for (g=children;g;g=g->next)
            {
                if (menu == gtk_menu_item_get_submenu (GTK_MENU_ITEM(g->data)))
                    { 
                        gtk_menu_reorder_child ( GTK_MENU(open_menu), GTK_WIDGET(g->data), 1);
                        positioned = TRUE;
                        break;
                    }
            }
            g_list_free (children);
        }
  }
  static int count = 0;
  
  item = gtk_menu_item_new_with_label (filename);
  gtk_menu_shell_insert (GTK_MENU_SHELL (menu), item, 1);//after the tear-off item
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
      if (filename) // && g_file_test (filename, G_FILE_TEST_EXISTS))
        return filename;
    }
  return NULL;
}

static void
show_type (GtkWidget * widget, gchar * message)
{
  g_message ("%s%s", message, widget ? g_type_name (G_TYPE_FROM_INSTANCE (widget)) : "NULL widget");
}






static void
visible_rhythm_buttons (GList * rhythms, gboolean on)
{
  GList *g;
  for (g = rhythms; g; g = g->next)
    on ? gtk_widget_show (GTK_WIDGET (((RhythmPattern *) (g->data))->button)) : gtk_widget_hide (GTK_WIDGET (((RhythmPattern *) (g->data))->button));

}

static void
switch_page (GtkNotebook * notebook, GtkWidget * page, guint pagenum)
{
  //g_message("switching pagenum %d\n",pagenum);
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
    set_toggle (ToggleLilyText_STRING, FALSE);//activate_action ("/MainMenu/ViewMenu/" ToggleLilyText_STRING);

  if (gtk_widget_get_visible (Denemo.project->score_layout))
    set_toggle (ToggleScoreLayout_STRING, FALSE);

  unhighlight_rhythm (Denemo.project->prevailing_rhythm);

  visible_rhythm_buttons (Denemo.project->rhythms, FALSE);

  Denemo.project = project = (DenemoProject *) (g->data);
  //g_debug("switch page\n");

//Switch to the scheme script attached to this tab of the notebook
  gchar *current_script = Denemo.project->script;
  Denemo.project->script = NULL;
  deleteSchemeText ();
  if (current_script)
    {
        appendSchemeText (current_script);
        Denemo.project->script = current_script;
        Denemo.project->has_script = TRUE;
    }

//FIXME if Denemo.project->movement->recording then show Denemo.audio_vol_control
  if (Denemo.prefs.visible_directive_buttons, 0)
    {
      gtk_widget_hide (Denemo.project->buttonboxes);
      set_toggle (ToggleScoreTitles_STRING, FALSE);
    }


  visible_rhythm_buttons (Denemo.project->rhythms, TRUE);


  set_title_bar (Denemo.project);
  highlight_rhythm (Denemo.project->prevailing_rhythm);
  force_lily_refresh (Denemo.project);
  draw_score_area ();
  draw_score (NULL);
}







static GtkWidget *
create_helpbutton (GtkWidget * box, gchar * thelabel, gpointer callback, gchar * tooltip)
{
  GtkWidget *button;
  if (thelabel)
    {button = gtk_button_new_with_label ("");
    GtkWidget *label = gtk_bin_get_child (GTK_BIN(button));
    gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
    gtk_label_set_markup(GTK_LABEL (label), thelabel);
    }
  else
    button = gtk_button_new ();
  gtk_widget_set_can_focus (button, FALSE);
  if (callback)
    g_signal_connect (G_OBJECT(button), "clicked", G_CALLBACK (callback), NULL);
  gtk_box_pack_start (GTK_BOX (box), button, FALSE, TRUE, 0);
  gtk_widget_set_tooltip_text (button, tooltip);
  return button;
}
static GtkWidget *
create_playbutton (GtkWidget * box, gchar * thelabel, gpointer callback, gchar * tooltip)
{
  GtkWidget *button;
  GtkWidget *label;
  if (thelabel)
    {button = gtk_button_new_with_label ("");
    label = gtk_bin_get_child (GTK_BIN(button));
    gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
    gtk_label_set_markup(GTK_LABEL (label), thelabel);
    }
  else
    button = gtk_button_new ();
  gtk_widget_set_can_focus (button, FALSE);
  if (callback)
    g_signal_connect (G_OBJECT(button), "clicked", G_CALLBACK (callback), NULL);
  gtk_box_pack_start (GTK_BOX (box), button, FALSE, TRUE, 0);
  gtk_widget_set_tooltip_text (button, tooltip);
  return label;
}


void
set_playbutton (gboolean pause)
{
	static gboolean first = TRUE;
	if (first)
	{
		pause = !pause;
		first = FALSE;
	}
	if (pause)
	  gtk_label_set_markup (GTK_LABEL (playbutton), "<span foreground=\"blue\"><b>Ⅱ</b></span>");
	else
	  gtk_label_set_markup (GTK_LABEL (playbutton), "<span foreground=\"blue\"><b>▶</b></span>");
}

//Set the master volume of the passed score and change the slider to suit
void
set_master_volume (DenemoMovement * si, gdouble volume)
{
  si->master_volume = volume;

}

//Set the tempo of the  movement and change the label and start/end times to suit
void
set_movement_tempo (gint tempo)
{
	gdouble factor = tempo/(gdouble) Denemo.project->movement->tempo;
	
	if (tempo != Denemo.project->movement->tempo)
		{
			if (Denemo.project->movement->end_time > 0.0)
				Denemo.project->movement->end_time /= factor;
			if (Denemo.project->movement->start_time > 0.0)
				Denemo.project->movement->start_time /= factor;
			//g_print ("Factor %.2f\n", factor);
			Denemo.project->movement->tempo = tempo;

			if (Denemo.project->movement->smf)
			{
			 Denemo.project->movement->smfsync = G_MAXINT;
			 exportmidi (NULL, Denemo.project->movement);
			}
			score_status (Denemo.project, TRUE);
		}
	 if (tempo_widget)
			{
			  gchar *text = g_strdup_printf ("%d", tempo);
			  update_tempo_widget (text);
			  g_free (text);
			}
}

static void
toggle_dynamic_compression (gboolean * compression)
{
  *compression = 100 * (!*compression);
  Denemo.project->movement->smfsync = G_MAXINT;
}

static void open_command_center_on_LastID (void)
{
  command_center_select_idx (NULL, Denemo.LastCommandId);
}

/* create_window() creates the toplevel window and all the menus - it only
   called once per invocation of Denemo */
static void
create_window (void)
{

  GtkWidget *outer_main_vbox, *main_hbox, *main_vbox, *menubar, *toolbar, *hbox;
  GtkUIManager *ui_manager;
  GError *error;
  gchar *data_file = NULL;

  Denemo.window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (Denemo.window), _("Denemo Main Window"));
  // it accesses Denemo.window
  loadWindowState ();
#ifdef G_OS_WIN32
  g_message ("Denemo icon not used");
  //not installed on windows ... data_file = g_build_filename (get_system_data_dir (), "icons","denemo.png", NULL);
#else
  GList *icon_dirs = NULL;
  icon_dirs = g_list_append (icon_dirs, g_build_filename (PACKAGE_SOURCE_DIR, PIXMAPS_DIR, NULL));
  icon_dirs = g_list_append (icon_dirs, g_strconcat (get_system_data_dir (), "/../pixmaps", NULL));     //FIXME installed in wrong place?

  data_file = find_path_for_file ("org.denemo.Denemo.png", icon_dirs);
  if (data_file)
    gtk_window_set_default_icon_from_file (data_file, NULL);
#endif

  g_signal_connect (G_OBJECT (Denemo.window), "delete_event", G_CALLBACK (delete_callback), NULL);
  g_free (data_file);

  gtk_window_set_resizable (GTK_WINDOW (Denemo.window), TRUE);

  Denemo.color = 0xFFFFFF;      //white background RGB values

//FIXME this is where score_layout should be created.
//score_layout should belong the Denemo.xxx not Denemo.project->xxx (so as to be like the others)
#ifdef USE_EVINCE
  install_printpreview (NULL);
#endif
  install_svgview (NULL);

  outer_main_vbox = gtk_vbox_new (FALSE, 1);
  gtk_container_set_border_width (GTK_CONTAINER (outer_main_vbox), 1);
  gtk_container_add (GTK_CONTAINER (Denemo.window), outer_main_vbox);
  gtk_widget_show (outer_main_vbox);

// the keymap is *NOT* yet created. We create actions here for all the built-ins. Then when init_keymap() is called the built-ins can pickup their callback functions from the actions.
//non built-in actions will be created later.
//the toggle and radio actions and the popup menus will need extra work #ifdef EXTRA_WORK
  
  denemo_menusystem_new ();//creates ActionWidgets hash table the keymap is not yet created

  denemo_menusystem_add_actions ();

#ifdef EXTRA_WORK
  {
    //pops up with menu items for the directives attached to the current note
    GtkWidget *menu = denemo_menusystem_get_widget ("/NoteEditPopupDirectives");
    g_signal_connect (menu, "deactivate", G_CALLBACK (unpopulate_menu), NULL);
  }
#endif
  
  Denemo.menubar = denemo_menusystem_get_widget ("/MainMenu"); 
  if (Denemo.prefs.newbie)
    gtk_widget_set_tooltip_text (Denemo.menubar, _("This is the Main Menu bar, where menus for the mostly non-musical aspects (saving, printing, setting up input sources etc) are placed. See the Object Menu bar for the commands that edit music"));
  gtk_box_pack_start (GTK_BOX (outer_main_vbox), Denemo.menubar, FALSE, TRUE, 0);
  gtk_widget_show (Denemo.menubar);

  if (Denemo.prefs.newbie)
    gtk_widget_set_tooltip_text (denemo_menusystem_get_widget ("/ObjectMenu"), _("This is the Object Menu bar, where menus for the commands that edit music live. They are arranged in a hierarchy Score, Movement, Staff (which contains Voices) and then the things that go on a staff, notes, clefs etc. Directives covers everything else that you can put in amongst the notes to change the behavior from that point in the music."));

  gtk_widget_set_tooltip_markup (denemo_menusystem_get_widget ("/RhythmToolBar"),
                                 _
                                 ("You can populate this bar with buttons holding a snippet of music. The highlighted snippet is the <i>prevailing duration</i>, that is the next note entered will follow the rhythmic pattern of this snippet.\nYou can enter the whole snippet by clicking on it, or using the command under ObjectMenu → Notes/Rests → Append/InsertDuration → Insert Snippet. You can also select the <i>prevailing snippet</i> using  ObjectMenu → Notes/Rests → Select Duration → Next Snippet.\nYou can hide this bar (to make more room on the screen) using the View menu. You can make it your preference to hide it using MainMenu → Edit → Change Preferences → Display Note/Rest entry toolbar"));


  toolbar = denemo_menusystem_get_widget ("/ToolBar");
  if (Denemo.prefs.newbie)
    gtk_widget_set_tooltip_text (toolbar, _("This tool bar contains a few conventional commands. You can hide it (to make more room on the screen) using the View menu. You can make it your preference to hide it using MainMenu → Edit → Change Preferences → Display general toolbar"));
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_BOTH_HORIZ);
  gtk_box_pack_start (GTK_BOX (outer_main_vbox), toolbar, FALSE, TRUE, 0);
  gtk_widget_set_can_focus (toolbar, FALSE);



  {
    Denemo.playback_control = gtk_vbox_new (FALSE, 1);
    //gtk_widget_set_tooltip_text (Denemo.playback_control, _("Controls for playback. The arrows on either side of the PLAY and STOP buttons move the playback start" " and playback end markers. Loop plays in a loop - you can edit while it plays. You can also record the output and save it as .ogg or .wav file. The temperament used for playing back can be set here."));
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

    
     create_playbutton (inner, "<span foreground=\"orange\"><b>◀--</b></span>", pb_go_back,
     _("Moves the playback start point (which shows as a green bar) earlier in time\nThe red and green bars do not get drawn until you have started play, or at least created the time base."));

    create_playbutton (inner, "<span foreground=\"green\"><b>❙</b></span>", pb_start_to_cursor, _("Sets the playback start point (green bar) to the note at the cursor.\nThe red and green bars do not get drawn until you have started play, or at least created the time base."));
    

    create_playbutton (inner, "<span foreground=\"orange\"><b>--▶</b></span>", pb_next,
     _("Moves the playback start point (which shows as a green bar) later in time\nThe red and green bars do not get drawn until you have started play, or at least created the time base."));
    create_playbutton (inner, "■", pb_stop, _("Stops the playback. On pressing play after this playback will start where the green bar is, not where you stopped. Use the Play/Pause button for that."));


    playbutton = create_playbutton (inner, "<span foreground=\"blue\"><b>▶</b></span>", pb_play,
     _("Starts playing back from the playback start (green bar) until the playback end (red bar).\nWhen playing it pauses the play, and continues when pressed again."));




    audiorecordbuttonlabel = create_playbutton (inner, "<span foreground=\"red\"><b>●</b></span>", pb_audiorecord, _("Starts/Stops recording the audio output from Denemo.\nRecords live performance and/or playback,\nsave to disk to avoid overwriting previous recordings."));
    exportbutton = create_helpbutton (inner, "<span foreground=\"green\"><b>☳</b></span>", pb_exportaudio, _("Exports the audio recorded to disk"));

    create_playbutton (inner, "<span foreground=\"orange\"><b>◀--</b></span>", pb_previous,
			 _("Moves the playback end point (which shows as a red bar) earlier in time\nThe red and green bars do not get drawn until you have started play, or at least created the time base."));
    create_playbutton (inner, "<span foreground=\"red\"><b>❙</b></span>", pb_end_to_cursor, _("Sets the playback end point (red bar) to the note at the cursor.\nThe red and green bars do not get drawn until you have started play, or at least created the time base."));

    

     create_playbutton (inner, "<span foreground=\"orange\"><b>--▶</b></span>", pb_go_forward, 
			 _("Moves the playback end point (which shows as a red bar) later in time\nThe red and green bars do not get drawn until you have started play, or at least created the time base."));

    create_playbutton (inner, _("Loop"), pb_loop, _("The music between the red and green bars is played in a loop.\nYou can edit the music while it is playing\n(so that you can continuously listen as you try alternatives)."));

    create_playbutton (inner,
#ifdef _HAVE_JACK_
                       _("Panic")
#else
                       _("Reset")
#endif
                       , pb_panic, _("Resets the synthesizer, on JACK it sends a JACK panic."));


    create_playbutton (inner, _("Play Selection"), pb_play_range, _("Plays the current selection or from the cursor to the end if no selection present."));
    create_playbutton (inner, _("Playback Range"), pb_range,  _("Pops up a dialog to get timings for start and end of playback."));
    GtkWidget *temperament_control = get_temperament_combo ();
    if (!gtk_widget_get_parent (temperament_control))
      //gtk_container_add (GTK_CONTAINER (inner), temperament_control);
      gtk_box_pack_start (GTK_BOX (inner), temperament_control, FALSE, FALSE, 0);
#define PLAYBACK_HELP  _("Controls for playback.\nThe arrows on either side of the PLAY and STOP buttons move the playback start\nand playback end markers.\nLoop plays in a loop - you can edit while it plays.\nYou can also record the output and save it as .ogg or .wav file.\nThe temperament used for playing back can be set here.")
    
     GtkWidget* helpbutton = create_helpbutton (inner, _("Help"), NULL, PLAYBACK_HELP);
     g_signal_connect_swapped (helpbutton, "clicked", G_CALLBACK(infodialog), PLAYBACK_HELP);
      
    {
      GtkWidget *hbox;
      hbox = gtk_hbox_new (FALSE, 1);
      gtk_box_pack_start (GTK_BOX (inner1), hbox, TRUE, TRUE, 0);
      

      GtkWidget *tempo_button = (GtkWidget *) gtk_button_new_with_label ("𝅘𝅥  = 120");
      tempo_widget = gtk_bin_get_child (GTK_BIN (tempo_button));
      gtk_label_set_use_markup (GTK_LABEL (tempo_widget), TRUE);
      g_signal_connect (G_OBJECT (tempo_button), "clicked", G_CALLBACK (movement_tempo_from_user), NULL);
      
      gtk_box_pack_start (GTK_BOX (hbox), GTK_WIDGET (tempo_button), FALSE, TRUE, 0);
      create_playbutton (hbox, _("Mute Staffs"), pb_mute_staffs, _("Select which staffs should be muted during playback."));

      // Volume
      label = gtk_label_new (_("Volume"));
      //GTK_WIDGET_UNSET_FLAGS(label, GTK_CAN_FOCUS);
      gtk_widget_set_tooltip_text (label, _("Set the (initial) volume of the movement"));

      gtk_widget_set_can_focus (label, FALSE);
      gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

      master_vol_adj = (GtkAdjustment *) gtk_adjustment_new (50.0, 0.0, 100.0, 1.0, 1.0, 10.0);

      GtkWidget *hscale = gtk_hscale_new (GTK_ADJUSTMENT (master_vol_adj));
      gtk_scale_set_digits (GTK_SCALE (hscale), 0);
      gtk_widget_set_can_focus (hscale, FALSE);
      //GTK_WIDGET_UNSET_FLAGS(hscale, GTK_CAN_FOCUS);
      g_signal_connect (G_OBJECT (master_vol_adj), "value_changed", G_CALLBACK (pb_volume), NULL);
      gtk_box_pack_start (GTK_BOX (hbox), hscale, TRUE, TRUE, 0);

      GtkWidget *always_full_volume = gtk_check_button_new_with_label (_("Ignore Dynamics"));
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (always_full_volume), Denemo.prefs.dynamic_compression);
      g_signal_connect_swapped (G_OBJECT (always_full_volume), "toggled", G_CALLBACK (toggle_dynamic_compression), &Denemo.prefs.dynamic_compression);
      gtk_box_pack_start (GTK_BOX (hbox), always_full_volume, FALSE, FALSE, 10);



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
    //gtk_widget_set_tooltip_text (Denemo.midi_in_control, _("Controls for managing input from a MIDI controller (e.g. keyboard) attached to the computer. You may need to select your MIDI device first using MainMenu → Edit → Change Preferences → MIDI looking for MIDI in devices (turn your device on first). When you have a MIDI controller durations are inserted without any pitch (they appear in brown) playing on the controller puts the pitches onto the durations. The Shift and Control and ALT keys can also be used for listening without entering notes, checking pitches entered and entering chords. The foot pedal can also be used for chords. Release the ALT key and re-press to start a new chord - timing is unimportant, play the chord fast or slow."));
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
      GtkWidget *midi_in_button = gtk_button_new ();
      g_signal_connect (G_OBJECT (midi_in_button), "clicked", midi_in_menu, NULL);
      midi_in_status = gtk_label_new (_("Appending/Editing Pitches"));
      gtk_container_add (GTK_CONTAINER (midi_in_button), midi_in_status);
      gtk_widget_set_tooltip_text (midi_in_status, _("This tells you what will happen to a MIDI in event from your controller. Click here or use the Control Shift or ALT keys, or caps lock to affect what will happen. Moving the cursor into the display will revert to editing notes."));
      gtk_label_set_use_markup (GTK_LABEL (midi_in_status), TRUE);
      gtk_box_pack_start (GTK_BOX (hbox), midi_in_button, FALSE, TRUE, 0);

      midiplayalongbutton =
        create_playbutton (hbox, _("Switch to Play Along Playback"), pb_playalong, 
        _("When in playalong mode, on clicking Play, the music plays until it reaches the Denemo cursor\nFrom then on you must play the notes at the cursor to progress the playback.\nSo if you set the cursor on the first note of the part you want to play, then once you have pressed play you can play along with Denemo, with Denemo filling in the other parts and waiting if you play a wrong note."));
	  create_helpbutton (hbox,_("Record Commands"), pb_midi_record_help,_("Menu of commands for recording MIDI-in"));
#define MIDI_CONTROL_HELP _("Controls for managing input from a MIDI controller (e.g. keyboard) attached to the computer.\nYou may need to select your MIDI device first using MainMenu → Edit → Change Preferences → MIDI\nlooking for MIDI in devices (turn your device on first).\nWhen you have a MIDI controller durations are inserted without any pitch (they appear in brown)\n playing on the controller puts the pitches onto the durations.\nThe Shift and Control and ALT keys can also be used for listening without entering notes,\nchecking pitches entered and entering chords.\nThe foot pedal can also be used for chords. Release the ALT key and re-press to start a new chord\n- timing is unimportant, play the chord fast or slow.\nOr use Input → MIDI → Chord Entry Without Pedal to enter chords based on playing the notes simultaneously")
      midihelpbutton = create_helpbutton (hbox, _( "Help"), NULL, MIDI_CONTROL_HELP);
      g_signal_connect_swapped (midihelpbutton, "clicked", G_CALLBACK(infodialog), MIDI_CONTROL_HELP);
      
      gtk_widget_show_all (Denemo.midi_in_control);
      gtk_widget_show_all (Denemo.playback_control);
      
      gtk_widget_hide (Denemo.midi_in_control);
      gtk_widget_hide (Denemo.playback_control);
      
      
      //gtk_widget_hide (deletebutton);
      //gtk_widget_hide (convertbutton);
      gtk_widget_hide (exportbutton);
    }
  }


  toolbar = denemo_menusystem_get_widget ("/RhythmToolBar");
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_TEXT);
  gtk_box_pack_start (GTK_BOX (outer_main_vbox), toolbar, FALSE, TRUE, 0);

  menubar = denemo_menusystem_get_widget ("/ObjectMenu");
  if (menubar)
    {
      gtk_box_pack_start (GTK_BOX (outer_main_vbox), menubar, FALSE, TRUE, 0);
    }

  main_hbox = gtk_hbox_new (FALSE, 1);
  gtk_widget_show (main_hbox);
  gtk_box_pack_start (GTK_BOX (outer_main_vbox), main_hbox, TRUE, TRUE, 0);
  Denemo.hpalettes = gtk_hbox_new (FALSE, 1);
  gtk_widget_show (Denemo.hpalettes);
  gtk_box_pack_start (GTK_BOX (main_hbox), Denemo.hpalettes, FALSE, FALSE, 0);
  main_vbox = gtk_vbox_new (FALSE, 1);
  gtk_widget_show (main_vbox);
  gtk_box_pack_start (GTK_BOX (main_hbox), main_vbox, TRUE, TRUE, 0);

  Denemo.vpalettes = gtk_vbox_new (FALSE, 1);
  gtk_widget_show (Denemo.vpalettes);
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
                                                       " The blue lozenge is the Denemo Cursor - it turns red when when the bar is full or green if you are inserting in a bar. "
                                                       "Overfull/Underfull bars are colored red/blue,"
                                                       " use the Upbeat (Anacrusis, Pickup) command if that is intentional."
                                                       "\nYou can switch to a menu-less view or a page-view using the Esc key."
                                                       " For the paged view you drag the red bar up the page to set how many systems you want showing." "For the paged view you will probably want a smaller zoom - use Control+scroll-wheel on your mouse to zoom the display." "\nMany commands operate on the object at the Denemo cursor. " "Right-click on an object to get a short menu of actions or set the mouse input mode.\n" "Shift-Right-click for more objects to insert."));
#if GTK_MAJOR_VERSION == 2
    GtkWidget *outer_pane = gtk_vpaned_new ();
#else
    GtkWidget *outer_pane = gtk_paned_new (GTK_ORIENTATION_VERTICAL);
#endif
    GtkWidget *scorearea_topbox = gtk_vbox_new (FALSE, 1);
    //gtk_container_add (GTK_CONTAINER (main_vbox), scorearea_topbox);
    gtk_box_pack_start (GTK_BOX (main_vbox), outer_pane, TRUE, TRUE, 0);
    gtk_paned_pack1 (GTK_PANED (outer_pane), scorearea_topbox, TRUE, FALSE);
    GtkWidget *score_and_scroll_hbox = gtk_hbox_new (FALSE, 1);
    //gtk_container_add (GTK_CONTAINER (scorearea_topbox), score_and_scroll_hbox);
    gtk_box_pack_start (GTK_BOX (scorearea_topbox), score_and_scroll_hbox, TRUE, TRUE, 0);
    gtk_widget_show (score_and_scroll_hbox);

    gtk_box_pack_start (GTK_BOX (score_and_scroll_hbox), Denemo.scorearea, TRUE, TRUE, 0);      // with this, the scorearea_draw_event is called
    gtk_widget_show (Denemo.scorearea);
#if GTK_MAJOR_VERSION != 2
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
    //g_signal_handlers_block_by_func(Denemo.scorearea, G_CALLBACK (ascorearea_motion_notify), NULL);
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
    gtk_widget_show_all (outer_pane);
  }


  create_lilywindow ();

  // This section creates an hbox and places it in the main vbox. Inside this hbox are placed a status bar and a label.
  // The status bar is not properly used within Denemo, and could just as well be a label too.
  GtkWidget *status_button = gtk_button_new_with_label ("");
  g_signal_connect (G_OBJECT(status_button), "clicked", G_CALLBACK (display_current_object), NULL);
  Denemo.statuslabel = gtk_bin_get_child (GTK_BIN(status_button));
  gtk_widget_show_all (status_button);
  gtk_widget_set_tooltip_text (Denemo.statuslabel,
                               _
                               ("This bar shows:\nPending ♯ or ♭ sign (if the next note entered will be sharpened or flattened)\nThe movement number\nDescription of the object at the Denemo cursor\nPosition and status (appending or inserting) of the cursor.\nIf the Playback Controls are visible then the timing of the object at the cursor is shown.\nIf MIDI in controls are visible the current enharmonic range is shown.\nWhen the first key of a two-key shortcut is pressed the possible continuations are shown here."));
#if GTK_MAJOR_VERSION == 2
  hbox = gtk_hpaned_new ();
#else
  hbox = gtk_paned_new (GTK_ORIENTATION_HORIZONTAL);
#endif
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);
  gtk_paned_pack1 (GTK_PANED (hbox), status_button, TRUE, FALSE);
  //gtk_widget_show (Denemo.statuslabel);
  //Denemo.status_context_id = gtk_statusbar_get_context_id (GTK_STATUSBAR (Denemo.statusbar), "Denemo");
  //gtk_statusbar_push (GTK_STATUSBAR (Denemo.statusbar), Denemo.status_context_id, "Denemo");
  GtkWidget *input_button = gtk_button_new_with_label ("");
  g_signal_connect (G_OBJECT(input_button), "clicked", G_CALLBACK (open_command_center_on_LastID), NULL);
  Denemo.input_label = gtk_bin_get_child (GTK_BIN(input_button));
  gtk_label_set_use_markup (GTK_LABEL (Denemo.input_label), TRUE);
  gtk_label_set_markup(GTK_LABEL (Denemo.input_label), _("No MIDI filter"));
 
  gtk_widget_set_tooltip_text (Denemo.input_label, _("This area shows which MIDI filters are active. It can also be used by commands to pass information to the user"));
  gtk_widget_show_all (input_button);
  Denemo.input_filters = g_string_new ("");
  gtk_paned_pack2 (GTK_PANED (hbox), input_button, FALSE, FALSE);
  gtk_paned_set_position (GTK_PANED (hbox), 600);
  gtk_widget_show (hbox);
  // End of status bar stuff - note this is not working on Windows correctly.


  create_scheme_window ();
  //gtk_widget_hide (denemo_menusystem_get_widget ("/MainMenu/HiddenMenu"));
  if (!Denemo.non_interactive)
    gtk_widget_show (Denemo.window);
 
  //set all the labels to use markup so that we can use the music font. Be aware this means you cannot use labels involving "&" "<" and ">" and so on without escaping them
  //                                 FIXME labels in toolitems are not correct until you do NewWindow.
  //                                 Really we should change the default for the class. */
  use_markup (Denemo.window);
  //g_debug("Turning on the modes\n");





  g_signal_connect (G_OBJECT (Denemo.notebook), "switch_page", G_CALLBACK (switch_page), NULL);

}                               /* create window */


void
newview (DenemoAction * action, DenemoScriptParam * param)
{
  newtab ();
  Denemo.project->movement->undo_guard = 1;     //do not collect undo for initialization of score
  load_scheme_init ();
  Denemo.project->movement->undo_guard = Denemo.prefs.disable_undo;
}

void
new_score_cb (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoScriptParam dummy;
  dummy.string = NULL;
  if (param == NULL)
    param = &dummy;
  file_newwrapper (action, param);
  if (param->status)
    {
      //call_out_to_guile ("(d-InstrumentName  (_ \"Unnamed\"))");
      // call_out_to_guile ("(d-ScoreTitle (_ \"Click Title\"))");
      denemo_scheme_init ();
    }
}

static DenemoProject *
new_project (gboolean new_movement)
{
  static gint id = 1;
  DenemoProject *project = (DenemoProject *) g_malloc0 (sizeof (DenemoProject));
  //uniquely identifies this musical score editor for duration of program.
  project->id = id++;
  project->mode = Denemo.prefs.mode;
  project->pending_midi = g_queue_new ();
  Denemo.projects = g_list_append (Denemo.projects, project);
  Denemo.project = NULL;
  project->lilycontrol.papersize = g_string_new ("a4"); //A4 default
  project->lilycontrol.staffsize = g_string_new ("18");
  project->lilycontrol.lilyversion = g_string_new ("");
  project->lilycontrol.orientation = TRUE;      //portrait
  project->total_edit_time = 0;
  reset_editing_timer ();
  if (new_movement)
    {
      Denemo.project = project;
      point_to_new_movement (project);
      project->movements = g_list_append (NULL, project->movement);
    }

  return project;
}

gint
hide_printarea_on_delete (void)
{
  //activate_action (TogglePrintView_STRING);
  set_toggle (TogglePrintView_STRING, FALSE);
  return TRUE;
}

static gint
hide_score_layout_on_delete (void)
{
  set_toggle (ToggleScoreLayout_STRING, FALSE);
  return TRUE;
}

static void toggle_rhythm_toolbar (DenemoAction * action, gpointer param);
static void toggle_entry_toolbar (DenemoAction * action, gpointer param);
static void toggle_object_menu (DenemoAction * action, gpointer param);

/* UNUSED
static void toggle_main_menu (DenemoAction * action, gpointer param);
*/
static void toggle_print_view (DenemoAction * action, gpointer param);
static void toggle_score_layout (DenemoAction * action, gpointer param);
static void toggle_command_manager (DenemoAction * action, gpointer param);
static void toggle_scoretitles (DenemoAction * action, gpointer param);


#if ((GTK_MAJOR_VERSION>3)||((GTK_MAJOR_VERSION==3) &&  (GTK_MINOR_VERSION>=22)))
static gint denemo_get_screen_width (void)
{
GdkRectangle r;
gdk_monitor_get_workarea (gdk_display_get_primary_monitor (gdk_display_get_default ()), &r);
return r.width;    
}
static gint denemo_get_screen_height (void)
{
GdkRectangle r;
gdk_monitor_get_workarea (gdk_display_get_primary_monitor (gdk_display_get_default ()), &r);
return r.height;    
}
#define gdk_screen_get_width(s) denemo_get_screen_width()
#define gdk_screen_get_height(s) denemo_get_screen_height()
#endif

static void
toggle_page_view (void)
{

  static gdouble zoom = 1.0;
  static gdouble system_height = 0.25;
  DenemoMovement *si = Denemo.project->movement;
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
  if (Denemo.non_interactive)
    return;
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
      win_width = Denemo.project->movement->stored_width;
      win_height = Denemo.project->movement->stored_height;
    }
  else
    gtk_window_get_size (GTK_WINDOW (Denemo.window), &win_width, &win_height);
  //g_debug("window width is %d\n", win_width);
  // NOTE  lyrics are per movement
  GtkWidget *widget;
  gboolean hide = !show;
  if (((current_view == DENEMO_PAGE_VIEW) && hide) || (show && (!current_view)))
    return;

  hide ? (gtk_widget_hide (Denemo.vpalettes), gtk_widget_hide (Denemo.hpalettes)) : (gtk_widget_show (Denemo.vpalettes), gtk_widget_show (Denemo.hpalettes));
  current_view = hide ? DENEMO_LINE_VIEW : DENEMO_MENU_VIEW;
#define ACCUM height += get_widget_height(widget)


#define TOG(name, item, menu)\
  widget = denemo_menusystem_get_widget (name);\
  static gboolean item=TRUE;\
  if(hide)\
    item = gtk_widget_get_visible (widget);\
  if((hide && item) || (show && item))\
    ACCUM, activate_action(menu);

#define TOG2(name, item)\
  widget = denemo_menusystem_get_widget (name);\
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

  TOG ("/ToolBar", toolbar, ToggleToolbar_STRING);
  //TOG("/RhythmToolBar", rtoolbar, "/MainMenu/ViewMenu/"ToggleRhythmToolbar_STRING);
  TOG ("/ObjectMenu", objectmenu, ToggleObjectMenu_STRING);

  TOG2 ("/MainMenu", mainmenu);

  //TOG3(gtk_widget_get_parent(gtk_widget_get_parent(Denemo.printarea)), print_view, "/MainMenu/ViewMenu/"TogglePrintView_STRING);
  TOG3 (Denemo.project->buttonboxes, scoretitles, ToggleScoreTitles_STRING);
  TOG3 (Denemo.playback_control, playback_control, TogglePlaybackControls_STRING);
  TOG3 (Denemo.midi_in_control, midi_in_control, ToggleMidiInControls_STRING);

  gtk_window_resize (GTK_WINDOW (Denemo.window), win_width, win_height + (current_view ? -height : height));
#undef current_view
}

void
ToggleReduceToDrawingArea (DenemoAction * action, DenemoScriptParam * param)
{
  GtkWidget *widget = denemo_menusystem_get_widget ("/MainMenu");
  gboolean visibile = gtk_widget_get_visible (widget);
  if (Denemo.project->view == DENEMO_MENU_VIEW && !visibile)
    {
      g_warning ("Out of step");
      Denemo.project->view = DENEMO_LINE_VIEW;
    }
  toggle_to_drawing_area (!gtk_widget_get_visible (widget));
}

/**
 * Creates a new DenemoProject structure represented by a tab in a notebook: the DenemoProject can, at anyone time, control one musical score possibly of several movements. It can, from time to time have different musical scores loaded into it. So it is to be thought of as a Music Score Editor.
 * This DenemoProject* project is appended to the global list Denemo.projects.
 * A single movement (DenemoMovement) is instantiated in the project.
 *
 */
static void
newtab (void)
{
  if (Denemo.project && gtk_widget_get_visible (Denemo.project->score_layout))
    set_toggle (ToggleScoreLayout_STRING, FALSE);
  if (Denemo.project && gtk_widget_get_visible (Denemo.textwindow))
    set_toggle ( ToggleLilyText_STRING, FALSE);
  if (Denemo.project)
    visible_rhythm_buttons (Denemo.project->rhythms, FALSE);//!!!!!!!!!!!!!!!!!
  DenemoProject *project = new_project (FALSE);
  project->score_layout = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (project->score_layout), "Score Layout");
  gtk_window_set_default_size (GTK_WINDOW (project->score_layout), 1000, 600);
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
  gint pagenum = gtk_notebook_insert_page_menu (GTK_NOTEBOOK (Denemo.notebook), top_vbox, NULL, NULL, -1);      //puts top_vbox inside Denemo.notebook

  gtk_notebook_popup_enable (GTK_NOTEBOOK (Denemo.notebook));

  Denemo.page = gtk_notebook_get_nth_page (GTK_NOTEBOOK (Denemo.notebook), pagenum);    //note Denemo.page is suspect, it is set to the last page created and it is never unset even when that page is deleted - it is only used by the selection paste routine.
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


  //FIXME populate_opened_recent_menu (project);

  /* create the first movement now because showing the window causes it to try to draw the scorearea
     which it cannot do before there is a score. FIXME use signal blocking to control this - see importxml.c */
  point_to_new_movement (project);
  project->movements = g_list_append (NULL, project->movement);

  //install_lyrics_preview (project->movement, top_vbox);
  gtk_widget_set_can_focus (Denemo.scorearea, TRUE);
  gtk_widget_show (Denemo.page);
  gtk_widget_grab_focus (Denemo.scorearea);


  create_singleton_rhythm ((gpointer) insert_chord_0key);
  create_singleton_rhythm ((gpointer) insert_chord_1key);
  create_singleton_rhythm ((gpointer) insert_chord_2key);
  create_singleton_rhythm ((gpointer) insert_chord_3key);
  create_singleton_rhythm ((gpointer) insert_chord_4key);
  create_singleton_rhythm ((gpointer) insert_chord_5key);
  create_singleton_rhythm ((gpointer) insert_chord_6key);
  create_singleton_rhythm ((gpointer) insert_chord_7key);
  create_singleton_rhythm ((gpointer) insert_chord_8key);


  create_singleton_rhythm ((gpointer) insert_rest_0key);
  create_singleton_rhythm ((gpointer) insert_rest_1key);
  create_singleton_rhythm ((gpointer) insert_rest_2key);
  create_singleton_rhythm ((gpointer) insert_rest_3key);
  create_singleton_rhythm ((gpointer) insert_rest_4key);
  create_singleton_rhythm ((gpointer) insert_rest_5key);
  create_singleton_rhythm ((gpointer) insert_rest_6key);
  create_singleton_rhythm ((gpointer) insert_rest_7key);
  create_singleton_rhythm ((gpointer) insert_rest_8key);

  //Denemo.project->mode = Denemo.prefs.mode;

  // this stops the keyboard input from getting to  scorearea_keypress_event if done after attaching the signal, why?
  gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), pagenum);      //if this is not done Gdk-CRITICAL **: gdk_draw_drawable: assertion `GDK_IS_DRAWABLE (drawable)' failed message results. Presumably because we have failed to block the (expose_event) drawing while we set up the new page. FIXME.

  gtk_widget_set_can_focus (Denemo.scorearea, TRUE);
  //GTK_WIDGET_SET_FLAGS(Denemo.scorearea, GTK_CAN_FOCUS);
  gtk_widget_grab_focus (GTK_WIDGET (Denemo.scorearea));


  if (Denemo.prefs.visible_directive_buttons)
    {
      gtk_widget_show (Denemo.project->buttonboxes);
    }
  if (have_midi () && Denemo.prefs.startmidiin)
    project->input_source = INPUTMIDI;
  panic_all ();                 //g_print ("Reset synth as part of newtab()\n");
}                               /* end of newtab creating a new DenemoProject holding one musical score */
void hide() { gtk_widget_hide (Denemo.project->buttonboxes);}
void show() { gtk_widget_show (Denemo.project->buttonboxes);}
gint visible() { if (Denemo.project) return gtk_widget_get_visible (Denemo.project->buttonboxes); else return -1;}
