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
static void pb_playalong (GtkWidget * button);
static void pb_conduct (GtkWidget * button);
static void pb_record (GtkWidget * button);
static void pb_audiorecord (GtkWidget * button);
static void pb_exportaudio (GtkWidget * button);
static void select_rhythm_pattern (RhythmPattern * r);
static gint insert_pattern_in_toolbar (RhythmPattern * r);
static gboolean append_rhythm (RhythmPattern * r, gpointer fn);
static void install_button_for_pattern (RhythmPattern * r, gchar * thelabel);

static void newtab (GtkAction * action, gpointer param);

static void closewrapper (GtkAction * action, gpointer param);
static gboolean close_gui_with_check (GtkAction * action, gpointer param);
static void openinnew (GtkAction * action, DenemoScriptParam * param);
static void create_rhythm_cb (GtkAction * action, gpointer param);
static void delete_rhythm_cb (GtkAction * action, gpointer param);
static void toggle_edit_mode (GtkAction * action, gpointer param);
static void toggle_rest_mode (GtkAction * action, gpointer param);
static void toggle_rhythm_mode (GtkAction * action, gpointer param);
static void fetchcommands (GtkAction * action, gpointer param);
static void morecommands (GtkAction * action, gpointer param);
static void mycommands (GtkAction * action, gpointer param);

static void create_window (void);

static gint dnm_key_snooper (GtkWidget * grab_widget, GdkEventKey * event);
static void populate_opened_recent_menu (void);
static gchar *get_most_recent_file (void);
static void toggle_record_script (GtkAction * action, gpointer param);

typedef enum
{
  ACCELS_LOADED = 0x0,
  ACCELS_CHANGED = 0x1 << 0,
  EXTRA_ACCELS_ACTIVE = 0x1 << 1,
  ACCELS_MAY_HAVE_CHANGED = 0x1 << 2
} AccelStatus;


static void save_accels (void);

#include "generated/callbacks.h"        /* callback functions menuitems that can be called by scheme */
#include <libguile.h>
//#include <guile/gh.h>

#include "generated/scheme_cb.h"


static gboolean
scm_is_list (SCM scm)
{
  return scm_is_true (scm_list_p (scm));
}

#ifdef DEVELOPER
static FILE *DEV_fp;
#define DEV_CODE  gint idx = lookup_command_from_name(Denemo.map, name+strlen(DENEMO_SCHEME_PREFIX));\
  gchar *tooltip =  (idx<0)? "To be documented":(gchar*)lookup_tooltip_from_idx(Denemo.map, idx);\
  if(!DEV_fp) DEV_fp = fopen("functions.xml", "w");
#endif

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
  //stage_undo(Denemo.gui->si, ACTION_SCRIPT_ERROR); We don't need this as control will return to activate_script() which will terminate the undo properly, with anything the script has done on the undo stack.
  return SCM_BOOL_F;
}

gint
eval_file_with_catch (gchar * filename)
{
  // scm_c_primitive_load(filename);
  SCM name = scm_from_locale_string (filename);
  scm_eval_status = 0;
  scm_internal_catch (SCM_BOOL_T, (scm_t_catch_body) scm_primitive_load, (void *) name, (scm_t_catch_handler) standard_handler, (void *) filename);
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
  // g_print("Defining %s\n", def);
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



/***************** definitions to implement calling radio/check items from scheme *******************/
#define MODELESS_STRING "Modeless"
#define CLASSICMODE_STRING "ClassicMode"
#define INSERTMODE_STRING "InsertMode"
#define EDITMODE_STRING "EditMode"
#define NOTE_E_STRING "Note"
#define REST_E_STRING "Rest"
#define BLANK_E_STRING "Blank"
#define RHYTHM_E_STRING "Rhythm"
#define ToggleToolbar_STRING "ToggleToolbar"
#define TogglePlaybackControls_STRING "TogglePlaybackToolbar"
#define ToggleMidiInControls_STRING "ToggleMidiInToolbar"

#define ToggleRhythmToolbar_STRING "ToggleRhythmToolbar"
#define ToggleEntryToolbar_STRING  "ToggleEntryToolbar"
#define ToggleActionMenu_STRING  "ToggleActionMenu"
#define ToggleObjectMenu_STRING  "ToggleObjectMenu"
#define ToggleLilyText_STRING  "ToggleLilyText"
#define ToggleScript_STRING  "ToggleScript"

#define TogglePrintView_STRING  "TogglePrintView"
#define ToggleScoreLayout_STRING  "ToggleScoreLayout"
#define ToggleLyricsView_STRING  "ToggleLyricsView"
#define ToggleConsoleView_STRING  "ToggleConsoleView"

#define ToggleScoreView_STRING  "ToggleScoreView"
#define ToggleScoreTitles_STRING  "ToggleScoreTitles"
#define QuickEdits_STRING  "QuickEdits"
#define RecordScript_STRING  "RecordScript"


#define ReadOnly_STRING  "ReadOnly"


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





static void
install_scm_function (gchar * name, gpointer callback)
{
#ifdef DEVELOPER
  DEV_CODE;
  if (DEV_fp)
    fprintf (DEV_fp, "<listitem>%s one optional parameter: %s </listitem>\n", name, tooltip);
#endif
  scm_c_define_gsubr (name, 0, 1, 0, callback); // one optional parameter

}

static void
install_scm_function1 (gchar * name, gpointer callback)
{
#ifdef DEVELOPER
  DEV_CODE;
  if (DEV_fp)
    fprintf (DEV_fp, "<listitem>%s one required and one optional parameter: %s </listitem>\n", name, tooltip);
#endif
  scm_c_define_gsubr (name, 1, 1, 0, callback);

}

static void
install_scm_function2 (gchar * name, gpointer callback)
{
#ifdef DEVELOPER
  DEV_CODE;
  if (DEV_fp)
    fprintf (DEV_fp, "<listitem>%s two parameters: %s </listitem>\n", name, tooltip);
#endif
  scm_c_define_gsubr (name, 2, 0, 0, callback);

}

/*UNUSED
static void
install_scm_function3 (gchar * name, gpointer callback)
{
#ifdef DEVELOPER
  DEV_CODE;
  if (DEV_fp)
    fprintf (DEV_fp, "<listitem>%s three parameters: %s </listitem>\n", name, tooltip);
#endif
  scm_c_define_gsubr (name, 0, 3, 0, callback);

}
*/
static void
install_scm_function4 (gchar * name, gpointer callback)
{
#ifdef DEVELOPER
  DEV_CODE;
  if (DEV_fp)
    fprintf (DEV_fp, "<listitem>%s four parameters: %s </listitem>\n", name, tooltip);
#endif
  scm_c_define_gsubr (name, 0, 4, 0, callback);

}

#define DENEMO_SCHEME_PREFIX "d-"

#define INSTALL_SCM_FUNCTION(tooltip, name, callback)\
  install_scm_function(name, callback);\
  define_scheme_variable("Help-"name, tooltip, "Value is the help string of the variable");

#define INSTALL_SCM_FUNCTION1(tooltip, name, callback)\
  install_scm_function1(name, callback);\
  define_scheme_variable("Help-"name, tooltip, "Value is the help string of the variable");
#define INSTALL_SCM_FUNCTION2(tooltip, name, callback)\
  install_scm_function2(name, callback);\
  define_scheme_variable("Help-"name, tooltip, "Value is the help string of the variable");
/*UNUSED
#define INSTALL_SCM_FUNCTION3(tooltip, name, callback)\
  install_scm_function3(name, callback);\
  define_scheme_variable("Help-"name, tooltip, "Value is the help string of the variable");
*/
#define INSTALL_SCM_FUNCTION4(tooltip, name, callback)\
  install_scm_function4(name, callback);\
  define_scheme_variable("Help-"name, tooltip, "Value is the help string of the variable");


#undef DEV_CODE

SCM ReturnValue = SCM_BOOL_F;
static void
set_return_value (SCM val)
{
  ReturnValue = val;
}

static SCM
scheme_popup_menu (SCM list)
{

  GtkWidget *menu = gtk_menu_new ();
  set_return_value (SCM_BOOL_F);
  if (scm_is_list (list))
    {
      gint i;
      gint length = scm_to_int (scm_length (list));
      for (i = 0; i < length; i++)
        {
          SCM el = scm_list_ref (list, scm_from_int (i));
          if (scm_is_pair (el))
            {
              gchar *label = NULL;
              gchar *tooltip = NULL;
              SCM sym;
              //g_print("Note that %d is value and %d stringp\n", scm_pair_p(el), scm_string_p(el));
              if (scm_is_string (scm_car (el)))
                {

                  label = scm_to_locale_string (scm_car (el));
                  sym = scm_cdr (el);


                }
              else if (scm_is_pair (scm_car (el)))
                {
                  label = scm_to_locale_string (scm_caar (el));
                  tooltip = scm_to_locale_string (scm_cdar (el));
                  sym = scm_cdr (el);
                }
              if (label)
                {
                  GtkWidget *item = gtk_menu_item_new_with_label (label);
                  if (tooltip)
                    gtk_widget_set_tooltip_text (item, tooltip);
                  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
                  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (set_return_value), sym);
                }
              else
                {
                  set_return_value (SCM_BOOL_F);
                  break;
                }
            }
          else if (scm_is_string (el))
            {
              GtkWidget *item = gtk_menu_item_new_with_label (scm_to_locale_string (el));
              gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
              g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (set_return_value), el);
            }
          else
            {
              set_return_value (SCM_BOOL_F);
              break;
            }
        }
      gtk_widget_show_all (menu);
      g_signal_connect (menu, "selection-done", gtk_main_quit, NULL);
      gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
      gtk_main ();

    }
  return ReturnValue;
}

static SCM
scheme_get_offset (void)
{
#ifndef USE_EVINCE
  g_debug("This feature requires denemo to be built with evince");
#else
  gdouble offsetx, offsety;
  if (get_offset (&offsetx, &offsety))
    {
      offsetx *= 100;
      offsety *= 100;
      offsetx = floor (offsetx);
      offsety = floor (offsety);
      offsetx /= 100;
      offsety /= 100;

      return scm_cons (scm_from_double (offsetx), scm_from_double (offsety));
    }
#endif
  return SCM_BOOL_F;
}

static SCM
scheme_get_control_point (SCM pt)
{
#ifndef USE_EVINCE
  g_debug("This feature requires denemo to be built with evince");
#else
  if (scm_is_integer (pt))
    {
      gint which = scm_to_int (pt);
      if (which > 0 && which < 5)
        return SCM_BOOL (get_control_point (which));
    }
#endif
  return SCM_BOOL_F;
}

static void
prec (gdouble * val)
{
  *val = round ((*val) * 100.0) / 100;
}

static SCM
scheme_get_curve (void)
{
#ifndef USE_EVINCE
  g_debug("This feature requires denemo to be built with evince");
#else
  gdouble x1, y1, x2, y2, x3, y3, x4, y4;
  if (get_curve (&x1, &y1, &x2, &y2, &x3, &y3, &x4, &y4))
    {
      prec (&x1);
      prec (&y1);
      prec (&x2);
      prec (&y2);
      prec (&x3);
      prec (&y3);
      prec (&x4);
      prec (&y4);
      return scm_list_n (scm_cons (scm_from_double (x1), scm_from_double (y1)), scm_cons (scm_from_double (x2), scm_from_double (y2)), scm_cons (scm_from_double (x3), scm_from_double (y3)), scm_cons (scm_from_double (x4), scm_from_double (y4)), SCM_UNDEFINED);
    }
#endif
  return SCM_BOOL_F;
}

static SCM
scheme_get_positions (SCM is_slur)
{
#ifndef USE_EVINCE
  g_debug("This feature requires denemo to be built with evince");
#else
  gdouble neary, fary;
  if (get_positions (&neary, &fary, scm_is_true (is_slur)))
    {
      return scm_cons (scm_from_double (neary), scm_from_double (fary));
    }
#endif
  return SCM_BOOL_F;
}

static SCM
scheme_get_new_target (void)
{
#ifndef USE_EVINCE
  g_debug("This feature requires denemo to be built with evince");
  return SCM_BOOL_F;
#else
  return scm_from_bool (get_new_target ());
#endif
}

static SCM
scheme_get_new_point (void)
{
#ifndef USE_EVINCE
  g_debug("This feature requires denemo to be built with evince");
  return SCM_BOOL_F;
#else
  return scm_from_bool (get_new_point ());
#endif
}

static SCM
scheme_get_reference_point (void)
{
#ifndef USE_EVINCE
  g_debug("This feature requires denemo to be built with evince");
  return SCM_BOOL_F;
#else
  return scm_from_bool (get_reference_point ());
#endif
}

static SCM
scheme_get_target_info (void)
{
  DenemoScore *si = Denemo.gui->si;
  if (Denemo.gui->si->currentobject == NULL)
    return SCM_BOOL_F;
  SCM type = SCM_BOOL_F, grob = SCM_BOOL_F, tag = SCM_BOOL_F;
  switch (si->target.type)
    {
    case TARGET_NONE:
      type = SCM_BOOL_F;
      break;
    case TARGET_OBJECT:
      type = scm_from_locale_string ("Object");
      break;
    case TARGET_CHORD:
      type = scm_from_locale_string ("Chord");
      break;
    case TARGET_NOTE:
      type = scm_from_locale_string ("Note");
      break;
    case TARGET_SLUR:
      type = scm_from_locale_string ("Slur");
      break;
    case TARGET_TIE:
      type = scm_from_locale_string ("Tie");
      break;
    case TARGET_CRESC:
      type = scm_from_locale_string ("Cresc");
      break;
    case TARGET_DIM:
      type = scm_from_locale_string ("Dim");
      break;
    default:
      g_warning ("Unknown target type %d\n", si->target.type);
      type = SCM_BOOL_F;
      break;
    }

  if (si->target.type == TARGET_NOTE) {
    DenemoObject *obj = si->currentobject->data;
    chord *thechord = ((chord *) ((DenemoObject *) obj->object));
    if(thechord->figure)
      grob = scm_from_locale_string ("BassFigure");
  }

            
  if (si->target.directivenum || (si->target.type == TARGET_OBJECT))
    {
      DenemoDirective *directive = NULL;
      DenemoObject *obj = si->currentobject->data;
      if (si->target.type == TARGET_CHORD)
        {
          GList *directives = ((chord *) ((DenemoObject *) obj->object))->directives;
          if (directives)
            {
              directive = (DenemoDirective *) g_list_nth_data (directives, si->target.directivenum - 1);
            }
        }
      else if (si->target.type == TARGET_NOTE)
        {
          directive = get_note_directive_number (si->target.directivenum);
        }
      else if (si->target.type == TARGET_OBJECT)
        {
          if (obj->type == LILYDIRECTIVE)
            directive = (DenemoDirective *) obj->object;
        }
      if (directive)
        {
          if (directive->grob)
            {
              grob = scm_from_locale_string (directive->grob->str);
            }
          else
            grob = SCM_BOOL_F;
        }
      if (directive->tag)
        {
          tag = scm_from_locale_string (directive->tag->str);
        }
    }
  return scm_list_n (type, grob, tag, SCM_UNDEFINED);
}

static SCM
scheme_http (SCM hname, SCM page, SCM other, SCM poststr)
{
  char *name = NULL, *thepage = NULL, *oth = NULL, *post = NULL;

  if (scm_is_string (hname))
    {
      name = scm_to_locale_string (hname);
    }
  if (scm_is_string (page))
    {
      thepage = scm_to_locale_string (page);
    }
  if (scm_is_string (other))
    {
      oth = scm_to_locale_string (other);
    }
  if (scm_is_string (poststr))
    {
      post = scm_to_locale_string (poststr);
    }

  if (name && thepage && post && oth)
    {
      gchar *ret = post_denemodotorg (name, thepage, oth, post);
      SCM scm = scm_from_locale_string (ret);
      g_free (ret);
      free (name);
      free (thepage);
      free (oth);
      free (post);
      return scm;
    }
  else
    {
      free (name);
      free (thepage);
      free (oth);
      free (post);
      return SCM_BOOL (FALSE);
    }
}

//FIXME inelegant!
static gint
interpret_lilypond_notename (gchar * x, gint * mid_c_offset, gint * enshift)
{
  // g_print("Mid c offset of %d\n", *x-'c');
  gchar *c;
  gint octave = -1;             /* middle c is c' */
  gint accs = 0;

  for (c = x + 1; *c; c++)
    {
      if (*c == 'i' && *(c + 1) == 's')
        {
          accs++;
          c++;;
        }
      else if (*c == 'e' && *(c + 1) == 's')
        {
          accs--;
          c++;
        }
      else if (*c == ',')
        {
          octave--;
        }
      else if (*c == '\'')
        {
          octave++;
        }
    }
  if (*x == 'a' || *x == 'b')
    octave++;

  *mid_c_offset = *x - 'c' + 7 * octave;
  *enshift = accs;
  return *mid_c_offset;
}

static gint
lilypond_to_enshift (gchar * enshift_name)
{
  gint enshift = 0;
  gchar *c;
  for (c = enshift_name; *c; c++)
    {
      if (*c == 'i' && *(c + 1) == 's')
        {
          enshift++;
          c++;
        }
      else if (*c == 'e' && *(c + 1) == 's')
        {
          enshift--;
          c++;
        }
    }
  return enshift;
}

/*
  execute init script local dir for menupath or fallback on system dir
*/
static SCM
scheme_execute_init (gchar * menupath)
{
  gchar *filename = g_build_filename (locatedotdenemo (), "actions", "menus", menupath, INIT_SCM, NULL);
  if (g_file_test (filename, G_FILE_TEST_EXISTS))
    {
      g_print ("About to load from %s\n", filename);
      eval_file_with_catch (filename);  //ret = scm_c_primitive_load(filename);
    }
  else
    {
      g_free (filename);
      filename = g_build_filename (get_data_dir (), "actions", "menus", menupath, INIT_SCM, NULL);
      if (g_file_test (filename, G_FILE_TEST_EXISTS))
        {
          g_print ("About to load from %s\n", filename);
          eval_file_with_catch (filename);      //ret = scm_c_primitive_load(filename);
        }
      g_free (filename);
    }
  return SCM_BOOL (TRUE);
}

void
execute_init_scripts (gchar * menupath)
{
  (void) scheme_execute_init (menupath);
}

/* called by a script if it requires initialization
 the initialization script is expected to be in init.scm in the menupath of the action that invoked the script*/
static SCM
scheme_initialize_script (SCM action_name)
{
  SCM ret;
  char *name;
  name = scm_to_locale_string (action_name);
  GtkAction *action = lookup_action_from_name (name);
  if (!action)
    {
      g_warning ("Non-existent action %s", name);
      return SCM_BOOL (FALSE);
    }
  gchar *menupath = g_object_get_data (G_OBJECT (action), "menupath");
  ret = scheme_execute_init (menupath);
  if (name)
    free (name);
  return ret;
}

/* pass in a path (from below menus) to a command script. Loads the command from .denemo or system
 *  if it can be found
 * It is used at startup in .denemo files like ReadingNoteNames.denemo
 * which executes
 (d-LoadCommand "MainMenu/Educational/ReadingNoteNames")
 * to ensure that the command it needs is in the command set.
 */
static SCM
scheme_load_command (SCM command)
{
  gboolean ret;
  char *name;
  name = scm_to_locale_string (command);
  gchar *filename = g_build_filename (locatedotdenemo (), "actions", "menus", name, NULL);
  ret = load_xml_keymap (filename);
  if (ret == FALSE)
    {
      g_free (filename);
      filename = g_build_filename (locatedotdenemo (), "download", "actions", name, NULL);
      ret = load_xml_keymap (filename);
    }
  if (ret == FALSE)
    {
      g_free (filename);
      filename = g_build_filename (get_data_dir (), "actions", name, NULL);
      ret = load_xml_keymap (filename);
    }
  if (name)
    free (name);
  if (filename)
    g_free (filename);
  return SCM_BOOL (ret);
}

static SCM
scheme_activate_menu_item (SCM menupath)
{
  if (scm_is_string (menupath))
    {
      char *item;
      item = scm_to_locale_string (menupath);
      if (item)
        {
          gboolean ret = activate_action (item) ? TRUE : FALSE;
          free (item);
          return SCM_BOOL (ret);
        }
    }
  return SCM_BOOL_F;
}

static void toggle_toolbar (GtkAction * action, gpointer param);
static void toggle_playback_controls (GtkAction * action, gpointer param);
static void toggle_midi_in_controls (GtkAction * action, gpointer param);
static void toggle_rhythm_toolbar (GtkAction * action, gpointer param);
static void toggle_entry_toolbar (GtkAction * action, gpointer param);
static void toggle_object_menu (GtkAction * action, gpointer param);

/* UNUSED
static void toggle_main_menu (GtkAction * action, gpointer param);
*/
static void toggle_console_view (GtkAction * action, gpointer param);
static void toggle_print_view (GtkAction * action, gpointer param);
static void toggle_score_layout (GtkAction * action, gpointer param);
static void toggle_scoretitles (GtkAction * action, gpointer param);


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

static void
toggle_page_view (void)
{

  static gdouble zoom = 1.0;
  static gdouble system_height = 0.25;
  DenemoScore *si = Denemo.gui->si;
  if (si->page_width == 0)
    {
      si->page_width = gdk_screen_get_width (gtk_window_get_screen (GTK_WINDOW (Denemo.window)));
      si->page_height = gdk_screen_get_height (gtk_window_get_screen (GTK_WINDOW (Denemo.window)));
      if (si->page_height / (double) si->page_width < 1.4)
        si->page_width = si->page_height / 1.4;
      si->page_zoom = 0.5;
      si->page_system_height = 0.25;
    }
  if (Denemo.gui->view == DENEMO_PAGE_VIEW)
    {
      gtk_window_get_size (GTK_WINDOW (Denemo.window), &si->page_width, &si->page_height);
      si->page_zoom = si->zoom;
      si->page_system_height = si->system_height;
      si->zoom = zoom;
      si->system_height = system_height;
      Denemo.gui->view = DENEMO_LINE_VIEW;
      gtk_window_resize (GTK_WINDOW (Denemo.window), si->stored_width, si->stored_height);
    }
  else
    {
      gtk_window_get_size (GTK_WINDOW (Denemo.window), &si->stored_width, &si->stored_height);
      zoom = si->zoom;
      system_height = si->system_height;
      si->zoom = si->page_zoom;
      si->system_height = si->page_system_height;
      Denemo.gui->view = DENEMO_PAGE_VIEW;
      gtk_window_resize (GTK_WINDOW (Denemo.window), si->page_width, si->page_height);
    }
}

/* Hide/show everything except the drawing area */
void
toggle_to_drawing_area (gboolean show)
{
#define current_view Denemo.gui->view
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
      win_width = Denemo.gui->si->stored_width;
      win_height = Denemo.gui->si->stored_height;
    }
  else
    gtk_window_get_size (GTK_WINDOW (Denemo.window), &win_width, &win_height);
  //g_print("window width is %d\n", win_width);
  // NOTE  lyrics are per movement
  GtkWidget *widget;
  gboolean hide = !show;
  if (((current_view == DENEMO_PAGE_VIEW) && hide) || (show && (!current_view)))
    return;
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

  TOG2 ("/EntryToolBar", entrymenu);
  TOG2 ("/MainMenu", mainmenu);

  // TOG3(gtk_widget_get_parent(Denemo.console), console_view, "/MainMenu/ViewMenu/"ToggleConsoleView_STRING);
  //TOG3(gtk_widget_get_parent(gtk_widget_get_parent(Denemo.printarea)), print_view, "/MainMenu/ViewMenu/"TogglePrintView_STRING);
  TOG3 (Denemo.gui->buttonboxes, scoretitles, "/MainMenu/ViewMenu/" ToggleScoreTitles_STRING);
  TOG3 (Denemo.playback_control, playback_control, "/MainMenu/ViewMenu/" TogglePlaybackControls_STRING);
  TOG3 (Denemo.midi_in_control, midi_in_control, "/MainMenu/ViewMenu/" ToggleMidiInControls_STRING);

  gtk_window_resize (GTK_WINDOW (Denemo.window), win_width, win_height + (current_view ? -height : height));
#undef current_view
}

void
ToggleReduceToDrawingArea (GtkAction * action, DenemoScriptParam * param)
{
  GtkWidget *widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/MainMenu");
  gboolean visibile = gtk_widget_get_visible (widget);
  if (Denemo.gui->view == DENEMO_MENU_VIEW && !visibile)
    {
      g_warning ("Out of step");
      Denemo.gui->view = DENEMO_LINE_VIEW;
    }
  toggle_to_drawing_area (!gtk_widget_get_visible (widget));
}


static SCM
scheme_hide_buttons (SCM hide)
{
  SCM ret = SCM_BOOL_F;
  GtkWidget *widget = Denemo.gui->buttonbox;
  if (GTK_IS_CONTAINER (widget))
    {
      ret = SCM_BOOL_T;
      if (scm_is_false (hide))
        gtk_container_foreach (GTK_CONTAINER (widget), (GtkCallback) gtk_widget_show, NULL);
      else
        gtk_container_foreach (GTK_CONTAINER (widget), (GtkCallback) gtk_widget_hide, NULL);
    }
  return ret;
}

static SCM
scheme_destroy_buttons (void)
{
  SCM ret = SCM_BOOL_F;
  GtkWidget *widget = Denemo.gui->buttonbox;

  if (GTK_IS_CONTAINER (widget))
    {
      gtk_container_foreach (GTK_CONTAINER (widget), (GtkCallback) gtk_widget_destroy, NULL);
      ret = SCM_BOOL_T;
    }
  return ret;
}


/* hide all menus, leaving only the score titles, used for educational games */
static SCM
scheme_hide_menus (SCM hide)
{
  if (Denemo.gui->view != DENEMO_MENU_VIEW)
    {
      activate_action ("/MainMenu/ViewMenu/" ToggleScoreTitles_STRING);
      ToggleReduceToDrawingArea (NULL, NULL);
      return SCM_BOOL (TRUE);
    }
  gboolean show = FALSE;
  if (scm_is_false (hide))
    show = TRUE;
  toggle_to_drawing_area (show);
  activate_action ("/MainMenu/ViewMenu/" ToggleScoreTitles_STRING);
  return SCM_BOOL (TRUE);
}

static SCM
scheme_hide_window (SCM hide)
{
  gboolean show = FALSE;
  gboolean showing = gtk_widget_get_visible (Denemo.window);
  if (scm_is_false (hide))
    show = TRUE;
  if (show)
    gtk_widget_show (Denemo.window);
  else
    gtk_widget_hide (Denemo.window);
  return SCM_BOOL (showing == show);
}


/* when a script calls a command which is itself a script it comes through here */
static SCM
scheme_script_callback (SCM script, SCM params)
{
  char *name = NULL;
  SCM ret = SCM_BOOL_F;
  if (scm_is_string (script))
    {
      name = scm_to_locale_string (script);
      if (name)
        {
          GtkAction *action = lookup_action_from_name (name);
          if (action && !is_action_name_builtin(name))
            {
              gchar *paramvar = g_strdup_printf ("%s::params", name);
              scm_c_define (paramvar, params);

              gchar *text = g_object_get_data (G_OBJECT (action), "scheme");
              if (text && *text)
                {
                  //undo is a queue so this is the end :)
                  stage_undo (Denemo.gui->si, ACTION_STAGE_END);
                  ret = SCM_BOOL (!call_out_to_guile (text));
                  stage_undo (Denemo.gui->si, ACTION_STAGE_START);
                }
              else
                ret = SCM_BOOL (activate_script (action, NULL));
              scm_c_define (paramvar, SCM_BOOL_F);
              g_free (paramvar);
            }
          if (name)
            free (name);
        }
    }
  return ret;
}

void
create_scheme_function_for_script (gchar * name)
{
  gchar *proc = g_strdup_printf ("(d-%s #:optional params)", name);
  gchar *value = g_strdup_printf ("(d-ScriptCallback \"%s\" params)", name);
  gchar *def = g_strdup_printf ("(define %s::params #f) (define* %s %s)", name, proc, value);

  //g_print("Defining %s\n", def);
  call_out_to_guile (def);
  g_free (proc);
  g_free (value);
  g_free (def);

  // define_scheme_literal_variable(proc, value, "A scheme procedure to call the script of that name");
}


static SCM
scheme_debug_object (SCM optional)
{
  DenemoObject *curObj;

  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data))
    return SCM_BOOL (FALSE);
  g_print ("*************\nType = %d\nbasic_durinticks = %d\ndurinticks - %d\nstarttickofnextnote = %d\n***********\n", curObj->type, curObj->basic_durinticks, curObj->durinticks, curObj->starttickofnextnote);
  return SCM_BOOL (TRUE);
}


static SCM
scheme_load_keybindings (SCM name)
{
  char *filename;
  if (scm_is_string (name))
    {
      filename = scm_to_locale_string (name);
      if (load_xml_keybindings (filename) == 0)
        {
          free (filename);
          return SCM_BOOL_T;
        }
      gchar *name = g_build_filename (locatedotdenemo (), "actions", filename, NULL);
      if (load_xml_keybindings (name) == 0)
        {
          free (filename);
          //g_free(name); CHECKME
          return SCM_BOOL_T;
        }
      g_free (name);
      name = g_build_filename (locatedotdenemo (), "download", "actions", filename, NULL);
      if (load_xml_keybindings (name) == 0)
        {
          //g_free(name); CHECKME
          return SCM_BOOL_T;
        }
      g_free (name);
      name = g_build_filename (get_data_dir (), "actions", filename, NULL);
      if (load_xml_keybindings (name) == 0)
        {
          //g_free(name); CHECKME
          return SCM_BOOL_T;
        }
      g_free (name);
    }
  //if (name) g_free(name); CHECKME
  return SCM_BOOL_F;
}

static SCM
scheme_save_keybindings (SCM name)
{
  char *filename;
  if (scm_is_string (name))
    {
      filename = scm_to_locale_string (name);
      if (save_xml_keybindings (filename) == 0)
        {
          if (filename)
            free (filename);
          return SCM_BOOL_T;
        }
    }
  return SCM_BOOL_F;
}

static SCM
scheme_clear_keybindings (SCM optional)
{
  keymap_clear_bindings (Denemo.map);
  return SCM_BOOL_T;
}


static SCM
scheme_load_commandset (SCM name)
{
  char *filename;
  if (scm_is_string (name))
    {
      filename = scm_to_locale_string (name);
      if (load_xml_keymap (filename) == 0)
        {
          if (filename)
            free (filename);
          return SCM_BOOL_T;
        }
    }
  return SCM_BOOL_F;
}

#ifdef _WITH_X11_
#if 1                           //GTK3 Test
SCM
scheme_user_screenshot (SCM type, SCM position)
{
  GList **sources;
  SCM ret = SCM_BOOL_F;
  gint pos = -1;
  if ((!SCM_UNBNDP (position)) && scm_is_integer (position))
    pos = scm_to_int (position);

  if (scm_is_false (type))
    sources = &Denemo.gui->si->sources;
  else
    sources = &((DenemoStaff *) Denemo.gui->si->currentstaff->data)->sources;
  scheme_hide_window (SCM_BOOL_T);
  GdkRectangle *rect = screenshot_find_rectangle ();
  if (rect)
    {
      //g_print("%d %d %d %d\n", rect->x, rect->y, rect->width, rect->height);
      GdkPixbuf *screenshot = screenshot_get_pixbuf (gdk_get_default_root_window (), rect);
      if (screenshot)
        {
          *sources = g_list_insert (*sources, GINT_TO_POINTER (screenshot), pos);       //-1 appends
          ret = SCM_BOOL_T;
        }
    }
  scheme_hide_window (SCM_BOOL_F);

  return ret;
}

SCM
scheme_delete_screenshot (SCM type)
{
  GList **sources;
  if (scm_is_false (type))
    sources = &Denemo.gui->si->sources;
  else
    sources = &((DenemoStaff *) Denemo.gui->si->currentstaff->data)->sources;
  if (*sources)
    {
      GList *g = g_list_nth (*sources, Denemo.gui->si->currentmeasurenum - 1);
      if (g)
        {
          *sources = g_list_remove_link (*sources, g);
          //FIXME free g->data and g
          return SCM_BOOL_T;
        }
    }
  return SCM_BOOL_F;
}
#endif
#endif //_WITH_X11_
static SCM
scheme_push_clipboard (SCM optional)
{
  push_clipboard ();
  return SCM_BOOL_T;
}

static SCM
scheme_pop_clipboard (SCM optional)
{
  if (pop_clipboard ())
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

static SCM
scheme_delete_selection (SCM optional)
{
  if ((!Denemo.gui->si) || (!Denemo.gui->si->markstaffnum))
    return SCM_BOOL_F;
  delete_selection ();
  return SCM_BOOL_T;
}

static SCM
scheme_set_thumbnail_selection (SCM optional)
{
  if ((!Denemo.gui->si) || (!Denemo.gui->si->markstaffnum))
    return SCM_BOOL_F;
  if (Denemo.gui->si == Denemo.gui->movements->data)
    {
      memcpy (&Denemo.gui->thumbnail, &Denemo.gui->si->selection, sizeof (DenemoSelection));
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

static SCM
scheme_set_newbie (SCM optional)
{
  SCM ret = SCM_BOOL (Denemo.prefs.newbie);
  if (scm_is_true (optional))
    {
      Denemo.prefs.tooltip_timeout = Denemo.prefs.tooltip_browse_timeout = 0;
      Denemo.prefs.learning = 1;
      Denemo.prefs.newbie = 1;
    }
  else
    {
      Denemo.prefs.tooltip_timeout = Denemo.prefs.tooltip_browse_timeout = 2000;
      Denemo.prefs.newbie = 0;
      gtk_widget_set_tooltip_text (Denemo.scorearea, NULL);
    }
  return ret;
}

static SCM
scheme_get_checksum (SCM str)
{
  SCM ret = SCM_BOOL_F;
  if (scm_is_string (str))
    {
      gchar *chk;
      gchar *thestring = scm_to_locale_string (str);
      chk = g_compute_checksum_for_string (G_CHECKSUM_MD5, thestring, -1);
      ret = scm_from_locale_string (chk);
      g_free (chk);
    }
  return ret;
}

static SCM
scheme_create_thumbnail (SCM optional)
{
#ifndef USE_EVINCE
  g_debug("This feature requires denemo to be built with evince");
  return SCM_BOOL_F;
#else
  gboolean ret;
  if ((!SCM_UNBNDP (optional)) && scm_is_true (optional))
    ret = create_thumbnail (TRUE);
  else
    ret = create_thumbnail (FALSE);
  return SCM_BOOL (ret);
#endif
}

static SCM
scheme_exit (SCM optional)
{
  exit (0);
}

static SCM
scheme_create_layout (SCM name, SCM force)
{
  if (scm_is_string (name))
    {
      gchar *layout_name = scm_to_locale_string (name);
      if (create_custom_scoreblock (layout_name, scm_is_true (force)))
        return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

static SCM
scheme_lilypond_for_part (void)
{
  gint save = Denemo.gui->si->markstaffnum;
  Denemo.gui->si->markstaffnum = 0;
  if (!select_custom_layout_for_name (((DenemoStaff *) (Denemo.gui->si->currentstaff->data))->lily_name->str))
    generate_lilypond_part ();
  Denemo.gui->si->markstaffnum = save;
  return SCM_BOOL_T;
}

static SCM
scheme_typeset_part (void)
{
#ifndef USE_EVINCE
  g_debug("This feature requires denemo to be built with evince");
  return SCM_BOOL_F;
#else
  typeset_part ();
  return SCM_BOOL_T;
#endif
}

static SCM
scheme_reduce_layout_to_lilypond (void)
{
  make_scoreblock_editable ();
  return SCM_BOOL_T;
}

static SCM
scheme_get_current_staff_layout_id (void)
{
  guint id;
  if (((DenemoStaff *) (Denemo.gui->si->currentstaff->data))->voicecontrol == DENEMO_PRIMARY)
  {
    id = get_layout_id_for_name(((DenemoStaff *) (Denemo.gui->si->currentstaff->data))->lily_name->str);
    return scm_from_int(id);
  }
  return SCM_BOOL_F;
}
static SCM
scheme_get_layout_id (void)
{
  DenemoScoreblock *sb = selected_scoreblock ();
  if (sb)
    return scm_from_int (sb->id);
  return SCM_BOOL_F;
}
static SCM
scheme_select_layout_id (SCM the_id)
{
  if (scm_is_integer (the_id))
    {
      gint id = scm_to_int (the_id);
      return SCM_BOOL (select_layout_id (id));
    }
  return SCM_BOOL_F;
}

static SCM
scheme_select_default_layout (void)
{
  select_default_scoreblock ();
  return SCM_BOOL_T;
}

static SCM
scheme_get_layout_name (void)
{
  DenemoScoreblock *sb = selected_scoreblock ();
  if (sb && sb->name)
    return scm_from_locale_string (sb->name);
  return SCM_BOOL_F;
}

static SCM
scheme_select_next_layout (void)
{
  if (gtk_widget_get_visible (Denemo.gui->score_layout))
    {
      DenemoScoreblock *sb = get_next_scoreblock ();
      return sb ? SCM_BOOL_T : SCM_BOOL_F;
    }
  return SCM_BOOL_F;
}

static SCM
scheme_select_first_layout (void)
{
  if (gtk_widget_get_visible (Denemo.gui->score_layout))
    {
      DenemoScoreblock *sb = get_first_scoreblock ();
      return sb ? SCM_BOOL_T : SCM_BOOL_F;
    }
  return SCM_BOOL_F;
}


static SCM
scheme_select_next_custom_layout (void)
{
  return SCM_BOOL (iterate_custom_layout (FALSE));
}

static SCM
scheme_select_first_custom_layout (void)
{
  return SCM_BOOL (iterate_custom_layout (TRUE));
}

static SCM
scheme_open_source (SCM link)
{
  SCM ret = SCM_BOOL_F;
#ifndef USE_EVINCE
  g_debug("This feature requires denemo to be built with evince");
#else
  if (scm_is_string (link))
    {
      gchar *thestring = scm_to_locale_string (link);
      gchar *filename = strtok (thestring, ":");
      if (filename)
        {
          gint x, y, page;
          gchar *xstr = strtok (NULL, ":");
          gchar *ystr = strtok (NULL, ":");
          gchar *pstr = strtok (NULL, ":");
          x = xstr ? atoi (xstr) : 0;
          y = ystr ? atoi (ystr) : 0;
          page = pstr ? atoi (pstr) : 0;
          if (open_source (filename, x, y, page))
            ret = SCM_BOOL_T;
        }
      if (thestring)
        free (thestring);
    }
#endif
  return ret;
}

static SCM
scheme_open_source_file (SCM optional)
{
  if (open_source_file ())
    return SCM_BOOL_T;
  return SCM_BOOL_F;
}

static SCM
scheme_export_recorded_audio (void)
{

  return SCM_BOOL (export_recorded_audio ());

}

static SCM
scheme_open_source_audio_file (SCM optional)
{
  return SCM_BOOL (open_source_audio_file ());
}

static SCM
scheme_close_source_audio (SCM optional)
{
  return SCM_BOOL (close_source_audio ());
}

static SCM
scheme_start_audio_play (SCM annotate)
{
  if (Denemo.gui->si->audio)
    {
      start_audio_playing (scm_is_true (annotate));
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

static SCM
scheme_set_audio_lead_in (SCM seconds)
{
  if (scm_is_real (seconds))
    {
      gdouble secs = scm_to_double (seconds);
      return SCM_BOOL (set_lead_in (secs));
    }
  return SCM_BOOL_F;
}

static SCM
scheme_stop_audio_play (SCM annotate)
{
  if (audio_is_playing ())
    {
      stop_audio_playing ();
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

static SCM
scheme_audio_is_playing (void)
{
  return SCM_BOOL (audio_is_playing ());
}

static SCM
scheme_next_audio_timing (SCM optional)
{
  if (Denemo.gui->si->audio)
    {
      gdouble timing = get_audio_timing ();
      if (timing > 0.0)
        return scm_from_double (timing);
    }
  return SCM_BOOL_F;
}

static SCM
scheme_take_snapshot (SCM optional)
{
  return SCM_BOOL (take_snapshot ());
}

static SCM
scheme_increase_guard (SCM optional)
{
  if (Denemo.gui->si->undo_guard++)
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

static SCM
scheme_decrease_guard (SCM optional)
{
  if (Denemo.gui->si->undo_guard > 0)
    return SCM_BOOL (!--Denemo.gui->si->undo_guard);
  Denemo.gui->si->undo_guard = 0;
  return SCM_BOOL_T;
}

//From a script undo must undo only the modifications to the start of the script, and push another STAGE_END for the end of the actions that it will do after the invocation of undo. This function overrides the built-in undo called directly by the user.
static SCM
scheme_undo (SCM optional)
{
  stage_undo (Denemo.gui->si, ACTION_STAGE_START);
  undowrapper (NULL, NULL);
  stage_undo (Denemo.gui->si, ACTION_STAGE_END);
  return SCM_BOOL_T;
}

//Break the script up for undo purposes
static SCM
scheme_stage_for_undo (SCM optional)
{
  stage_undo (Denemo.gui->si, ACTION_STAGE_START);
  stage_undo (Denemo.gui->si, ACTION_STAGE_END);
  return SCM_BOOL_T;
}

static SCM
scheme_get_last_change (SCM optional)
{
  SCM ret = SCM_BOOL_F;
  gchar *last = get_last_change (Denemo.gui->si);
  if (last)
    ret = scm_from_locale_string (last);
  g_free (last);
  return ret;
}




static SCM
scheme_new_window (SCM optional)
{
  stage_undo (Denemo.gui->si, ACTION_STAGE_START);

  //gint current =  Denemo.gui->scorearea->allocation.width;
  newview (NULL, NULL);
  // Denemo.gui->scorearea->allocation.width = current;

  stage_undo (Denemo.gui->si, ACTION_STAGE_END);
  return SCM_BOOL_T;
}


static SCM
scheme_zoom (SCM factor)
{
  if (scm_is_real (factor))
    Denemo.gui->si->zoom = scm_to_double (factor);
  else if (scm_is_string (factor))
    {
      char *name;
      name = scm_to_locale_string (factor);
      if (name)
        {
          Denemo.gui->si->zoom = atof (name);
          free (name);
        }
    }
  else
    {
      return scm_from_double (Denemo.gui->si->zoom);
    }
  scorearea_configure_event (Denemo.scorearea, NULL);
  if (Denemo.gui->si->zoom > 0.01)
    {
      return scm_from_int (Denemo.gui->si->zoom);
    }
  Denemo.gui->si->zoom = 1.0;
  return SCM_BOOL_F;
}

static SCM
scheme_master_tempo (SCM factor)
{
  DenemoScore *si = Denemo.gui->si;
  gdouble request_time = get_time ();
  gdouble duration = request_time - si->tempo_change_time;
  si->start_player += duration * (1.0 - si->master_tempo);

  if (scm_is_real (factor))
    si->master_tempo = scm_to_double (factor);
  else if (scm_is_string (factor))
    {
      char *name;
      name = scm_to_locale_string (factor);
      if (name)
        {
          si->master_tempo = atof (name);
          free (name);
        }
    }
  else
    {
      return scm_from_double (si->master_tempo);
    }
  if (si->master_tempo < 0.0)
    si->master_tempo = 1.0;

  si->tempo_change_time = request_time;
  return scm_from_double (si->master_tempo);
}

static SCM
scheme_movement_tempo (SCM bpm)
{
  DenemoScore *si = Denemo.gui->si;
  if (scm_is_real (bpm))
    si->tempo = scm_to_int (bpm);
  if (scm_is_string (bpm))
    {
      char *name;
      name = scm_to_locale_string (bpm);
      if (name)
        {
          si->tempo = atof (name);
          free (name);
        }
    }

  if (si->tempo < 1)
    si->tempo = 120;
  return scm_from_int (si->tempo);
}

static SCM
scheme_master_volume (SCM factor)
{
  DenemoScore *si = Denemo.gui->si;
  if (scm_is_real (factor))
    si->master_volume = scm_to_double (factor);
  if (scm_is_string (factor))
    {
      char *name;
      name = scm_to_locale_string (factor);
      if (name)
        {
          si->master_volume = atof (name);
          free (name);
        }
    }
  if (si->master_volume < 0.0)
    si->master_volume = 1.0;
  return scm_from_double (si->master_volume);
}

static SCM
scheme_staff_master_volume (SCM level)
{
  DenemoStaff *thestaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (scm_is_real (level))
    {
      gdouble master_volume = scm_to_double (level);
      thestaff->volume = (gint) (master_volume * 127);
      if (thestaff->volume > 127)
        thestaff->volume = 127;
      if (thestaff->volume < 0)
        thestaff->volume = 0;
    }
  return scm_from_double (thestaff->volume / 127.0);
}

static SCM
scheme_get_midi_tuning (void)
{
  gchar *cents = get_cents_string ();
  SCM ret = scm_from_locale_string (cents);
  g_free (cents);
  return ret;
}

static SCM
scheme_get_sharpest (void)
{
  gchar *name = get_sharpest ();
  SCM ret = scm_from_locale_string (name);
  g_free (name);
  return ret;
}

static SCM
scheme_get_flattest (void)
{
  gchar *name = get_flattest ();
  SCM ret = scm_from_locale_string (name);
  g_free (name);
  return ret;
}

static SCM
scheme_get_temperament (void)
{
  gchar *name = get_temperament_name ();
  SCM ret = scm_from_locale_string (name);
  g_free (name);
  return ret;
}

static SCM
ignore_handler (gchar * data SCM_UNUSED, SCM tag, SCM throw_args SCM_UNUSED)
{
  // g_warning("ignoring throw");
  return SCM_BOOL_F;
}

void
set_meantone_tuning (gint step)
{
  SCM thestep = scm_from_int (step);
  if (SCM_BOOL_F == scm_internal_catch (SCM_BOOL_T, (scm_t_catch_body) scm_c_lookup, (void *) "SetQuarterCommaMeanTone", (scm_t_catch_handler) ignore_handler, (void *) "whoops"))
    return;
  SCM func_symbol = scm_c_lookup ("SetQuarterCommaMeanTone");
  SCM func = scm_variable_ref (func_symbol);
  scm_call_1 (func, thestep);
}

SCM
scheme_set_enharmonic_position (SCM position)
{
  if (scm_is_integer (position))
    {
      gint pos = scm_to_int (position);
      set_enharmonic_position (pos);
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

static SCM
scheme_rewind_midi (SCM start)
{
  DenemoGUI *gui = Denemo.gui;
  double thetime = 0.0;
  SCM scm = SCM_BOOL_T;
  gint err;
  if ((gui->si->smf == NULL) || (gui->si->smfsync != gui->si->changecount))
    generate_midi ();
  if (scm_is_real (start))
    thetime = scm_to_double (start);
  if (thetime > 0.0)
    {
      err = smf_seek_to_seconds (gui->si->smf, thetime);
      if (err)
        scm = SCM_BOOL_F;
    }
  else
    smf_rewind (gui->si->smf);
  return scm;
}


static SCM
scheme_next_midi_notes (SCM interval)
{
  SCM scm = scm_list_n (SCM_UNDEFINED);
  DenemoScore *si = Denemo.gui->si;
  if (scm_is_real (interval))
    {
      double margin = scm_to_double (interval);
      double start = -1.0;      //unset  
      smf_event_t *event = si->smf ? smf_peek_next_event (si->smf) : NULL;
      if (event)
        {
          while ((event = smf_peek_next_event (si->smf)))
            {
              gint key;
              if ((key = noteon_key (event)))
                {
                  if (start < 0.0)
                    start = event->time_seconds;
                  if ((event->time_seconds - start) < margin)
                    {
                      event = smf_get_next_event (si->smf);
                      scm = scm_cons (scm_from_int (key), scm);
                    }
                  else
                    {
                      break;
                    }
                }
              else
                {
                  event = smf_get_next_event (si->smf);
                }
            }
        }
      scm = scm_cons (scm, scm_from_double (start));
      return scm;
    }
  return SCM_BOOL_F;
}

static SCM
scheme_get_midi_on_time (void)
{
  if (!(Denemo.gui->si->currentobject))
    return SCM_BOOL_F;
  DenemoObject *curobj = Denemo.gui->si->currentobject->data;
  if (!curobj->midi_events)
    return SCM_BOOL_F;
  return scm_from_double (get_midi_on_time (curobj->midi_events));
}

static SCM
scheme_get_midi_off_time (void)
{
  if (!(Denemo.gui->si->currentobject))
    return SCM_BOOL_F;
  DenemoObject *curobj = Denemo.gui->si->currentobject->data;
  if (!curobj->midi_events)
    return SCM_BOOL_F;
  return scm_from_double (get_midi_off_time (curobj->midi_events));
}

static SCM
scheme_restart_play (void)
{
  restart_play ();
  return SCM_BOOL_T;
}

static SCM
scheme_set_playback_interval (SCM start, SCM end)
{
  if (scm_is_real (start) && scm_is_real (end))
    {
      Denemo.gui->si->start_time = scm_to_double (start);
      Denemo.gui->si->end_time = scm_to_double (end);
      set_start_and_end_objects_for_draw ();
      return SCM_BOOL_T;
    }
  if (scm_is_real (start))
    {
      Denemo.gui->si->start_time = scm_to_double (start);
      set_start_and_end_objects_for_draw ();
      return SCM_BOOL_T;
    }
  if (scm_is_real (end))
    {
      Denemo.gui->si->end_time = scm_to_double (end);
      set_start_and_end_objects_for_draw ();
      return SCM_BOOL_T;
    }
  if (scm_is_string (start) && scm_is_string (end))
    {
      char *name;
      name = scm_to_locale_string (start);
      if (name)
        {
          Denemo.gui->si->start_time = atof (name);
          free (name);
        }
      name = scm_to_locale_string (end);
      if (name)
        {
          Denemo.gui->si->end_time = atof (name);
          free (name);
        }
      set_start_and_end_objects_for_draw ();
      return SCM_BOOL_T;
    }
  if (scm_is_string (start))
    {
      char *name;
      name = scm_to_locale_string (start);
      if (name)
        {
          Denemo.gui->si->start_time = atof (name);
          free (name);
        }
      set_start_and_end_objects_for_draw ();
      return SCM_BOOL_T;
    }
  if (scm_is_string (end))
    {
      char *name;
      name = scm_to_locale_string (end);
      if (name)
        {
          Denemo.gui->si->end_time = atof (name);
          free (name);
        }
      set_start_and_end_objects_for_draw ();
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

static SCM
scheme_adjust_playback_start (SCM adj)
{
  SCM ret = SCM_BOOL_F;
  if (scm_is_real (adj))
    {
      Denemo.gui->si->start_time += scm_to_double (adj);
      if (Denemo.gui->si->start_time < 0.0)
        Denemo.gui->si->start_time = 0.0;
      else
        ret = SCM_BOOL_T;
    }
  set_start_and_end_objects_for_draw ();
  return ret;
}

static SCM
scheme_adjust_playback_end (SCM adj)
{
  SCM ret = SCM_BOOL_F;
  if (scm_is_real (adj))
    {
      Denemo.gui->si->end_time += scm_to_double (adj);
      if (Denemo.gui->si->end_time < 0.0)
        Denemo.gui->si->end_time = 0.0;
      else
        ret = SCM_BOOL_T;
    }
  set_start_and_end_objects_for_draw ();
  return ret;
}


static SCM
scheme_get_help (SCM command)
{
  char *name = NULL;
  if (scm_is_string (command))
    name = scm_to_locale_string (command);
  if (name == NULL)
    {
      return SCM_BOOL_F;
    }
  gint idx = lookup_command_from_name (Denemo.map, name);
  if (name)
    free (name);
  if (idx < 0)
    {
#if 0
      SCM help = scm_c_eval_string (g_strconcat ("Help-d-", name));
      return help;
#else
      return SCM_BOOL_F;
#endif
    }
  return scm_from_locale_string ((gchar *) lookup_tooltip_from_idx (Denemo.map, idx));
}

static SCM
scheme_get_lily_version (SCM optional)
{
  gchar *version = get_lily_version_string ();
  return scm_from_locale_string (version);
}

static SCM
scheme_check_lily_version (SCM check_version)
{
  char *version;
  if (scm_is_string (check_version))
    {
      version = scm_to_locale_string (check_version);
    }
  else
    {
      return SCM_BOOL_F;
    }
  gint result = check_lily_version (version);
  if (version)
    free (version);
  if (result > 0)
    {
      return SCM_BOOL_T;
    }
  else
    {
      return SCM_BOOL_F;
    }
}

static SCM
scheme_get_id (SCM command)
{
  char *name;
  if (scm_is_string (command))
    {
      gint id;
      name = scm_to_locale_string (command);
      id = lookup_command_from_name (Denemo.map, name);
      if (name)
        free (name);
      if (id != -1)
        {
          return scm_from_int (id);
        }
    }
  return SCM_BOOL_F;
}

static SCM
scheme_add_keybinding (SCM command, SCM binding)
{
  char *shortcut;
  char *name;
  gint id;
  gint old_id = -1;
  if (scm_is_string (binding))
    {
      shortcut = scm_to_locale_string (binding);
      if (scm_is_string (command))
        {
          name = scm_to_locale_string (command);
          old_id = add_keybinding_for_name (name, shortcut);
        }
      else if (scm_is_integer (command))
        {
          id = scm_to_int (command);
          if (id >= 0)
            old_id = add_keybinding_for_command (id, shortcut);
        }
      if (shortcut)
        free (shortcut);
      if (name)
        free (name);
    }
  if (old_id >= 0)
    {
      return scm_from_int (old_id);
    }
  else
    {
      return SCM_BOOL_F;
    }
}

static SCM
scheme_get_label (SCM command)
{
  char *name;
  if (scm_is_string (command))
    {
      name = scm_to_locale_string (command);
    }
  else
    {
      return SCM_BOOL_F;
    }
  if (name == NULL)
    {
      return SCM_BOOL_F;
    }
  gint idx = lookup_command_from_name (Denemo.map, name);
  if (name)
    free (name);
  if (idx < 0)
    {
      return SCM_BOOL_F;
    }
  return scm_from_locale_string ((gchar *) lookup_label_from_idx (Denemo.map, idx));
}

static SCM
scheme_get_menu_path (SCM command)
{
  char *name;
  if (scm_is_string (command))
    {
      name = scm_to_locale_string (command);
    }
  else
    {
      return SCM_BOOL_F;
    }
  if (name == NULL)
    {
      return SCM_BOOL_F;
    }
  gint idx = lookup_command_from_name (Denemo.map, name);
  if (name)
    free (name);
  if (idx < 0)
    {
      return SCM_BOOL_F;
    }
  GtkAction *action = (GtkAction *) lookup_action_from_idx (Denemo.map, idx);
  if (action == NULL)
    return SCM_BOOL_F;
  gchar *menupath = g_object_get_data (G_OBJECT (action), "menupath");
  if (menupath == NULL)
    {
      return SCM_BOOL_F;
    }
  return scm_from_locale_string (menupath);
}

static SCM
scheme_get_verse (SCM number)
{
  gchar *text = NULL;
  DenemoGUI *gui = Denemo.gui;
  if (scm_is_integer (number))
    {
      text = get_lyrics_for_verse_num (scm_to_int (number));

    }
  else
    {
      DenemoStaff *staff = (DenemoStaff *) gui->si->currentstaff->data;
      text = get_lyrics_for_current_verse (staff);
    }
  if (text)
    {
      SCM scm = scm_from_locale_string (text);
      //wrong!! g_free(text);
      return scm;
    }
  return SCM_BOOL_F;
}

static SCM
scheme_put_verse (SCM verse)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoStaff *staff = (DenemoStaff *) gui->si->currentstaff->data;
  if (scm_is_string (verse))
    {
      char *text;
      text = scm_to_locale_string (verse);
      gboolean ret = put_lyrics_for_current_verse (staff, text);
      if (text)
        free (text);
      return SCM_BOOL (ret);
    }
  return SCM_BOOL_F;
}

static SCM
scheme_append_to_verse (SCM verse)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoStaff *staff = (DenemoStaff *) gui->si->currentstaff->data;
  if (scm_is_string (verse))
    {
      char *text;
      text = scm_to_locale_string (verse);
      gboolean ret = append_lyrics_for_current_verse (staff, text);
      if (text)
        free (text);
      return SCM_BOOL (ret);
    }
  return SCM_BOOL_F;
}

/* write MIDI/Audio filter status */
static SCM
scheme_input_filter_names (SCM filtername)
{
  char *name = NULL;

  if (scm_is_string (filtername))
    {
      name = scm_to_locale_string (filtername);
      if (name)
        {
          g_string_printf (Denemo.input_filters, "MIDI Input: %s", name);
          gtk_widget_show (Denemo.input_source);
          write_input_status ();
          free (name);
          return SCM_BOOL_T;
        }
    }
  else
    {
      gtk_widget_hide (Denemo.input_source);
    }
  return SCM_BOOL_F;
}

/* write a status label on bottom right of window*/
static SCM
scheme_write_status (SCM filtername)
{
  char *name = NULL;

  if (scm_is_string (filtername))
    {
      name = scm_to_locale_string (filtername);
      if (name)
        {

          g_string_assign (Denemo.input_filters, name);
          gtk_widget_show (Denemo.input_source);
          write_input_status ();
          free (name);
          return SCM_BOOL_T;
        }
    }
  else
    {
      gtk_widget_hide (Denemo.input_source);
    }
  return SCM_BOOL_F;
}

SCM
scheme_goto_position (SCM movement, SCM staff, SCM measure, SCM object)
{
  gint movementnum, staffnum, measurenum, objectnum;
  if (scm_is_integer (movement))
    movementnum = scm_to_int (movement);
  else
    movementnum = g_list_index (Denemo.gui->movements, Denemo.gui->si) + 1;
  if (scm_is_integer (staff))
    staffnum = scm_to_int (staff);
  else
    staffnum = Denemo.gui->si->currentstaffnum;

  if (scm_is_integer (measure))
    measurenum = scm_to_int (measure);
  else
    measurenum = Denemo.gui->si->currentmeasurenum;

  if (scm_is_integer (object))
    objectnum = scm_to_int (object);
  else
    objectnum = 1 + Denemo.gui->si->cursor_x;
#if 0
  // 1 is ambiguous, either empty measure or object 1
  gboolean result = goto_movement_staff_obj (NULL, movementnum, staffnum, measurenum, objectnum);
  if (Denemo.gui->si->currentmeasure->data == NULL && objectnum == 1)
    return SCM_BOOL (goto_movement_staff_obj (NULL, movementnum, staffnum, measurenum, 0));
  gint numobjs = (Denemo.gui->si->currentmeasure->data) ? g_list_length (Denemo.gui->si->currentmeasure->data) : 0;
  if (objectnum == 1 + numobjs)
    Denemo.gui->si->cursor_appending = TRUE;
  write_status (Denemo.gui);
  if (objectnum > 1 + numobjs)
    return SCM_BOOL_F;
  return SCM_BOOL (result);
#endif
  gint origmvt = g_list_index (Denemo.gui->movements, Denemo.gui->si) + 1, origstaff = Denemo.gui->si->currentstaffnum, origmeas = Denemo.gui->si->currentmeasurenum, origpos = 1 + Denemo.gui->si->cursor_x;
  goto_movement_staff_obj (NULL, movementnum, staffnum, measurenum, objectnum);
  if ((movementnum == g_list_index (Denemo.gui->movements, Denemo.gui->si) + 1) && (staffnum == Denemo.gui->si->currentstaffnum) && (measurenum == Denemo.gui->si->currentmeasurenum) && (objectnum == 1 + Denemo.gui->si->cursor_x))
    return SCM_BOOL_T;
  else
    goto_movement_staff_obj (NULL, origmvt, origstaff, origmeas, origpos);

  return SCM_BOOL_F;
}

SCM
scheme_shift_cursor (SCM value)
{
  if (!scm_is_integer (value))
    return SCM_BOOL_F;
  gint shift = scm_to_int (value);
  Denemo.gui->si->cursor_y += shift;
  Denemo.gui->si->staffletter_y = offsettonumber (Denemo.gui->si->staffletter_y + shift);
  return SCM_BOOL_T;



}

static SCM
scheme_mid_c_offsettoname (gint offset)
{
  gchar *notename = g_strdup_printf ("%c", mid_c_offsettoname (offset));
  SCM scm = scm_from_locale_string (notename);
  g_free (notename);
  return scm;
}

static SCM
scheme_get_horizontal_position (void)
{
  return scm_from_int (1 + Denemo.gui->si->cursor_x);
}

static SCM
scheme_set_object_display_width (SCM value)
{
  if (!scm_is_integer (value))
    return SCM_BOOL_F;
  if (Denemo.gui->si->currentobject)
    {
      DenemoObject *obj = Denemo.gui->si->currentobject->data;
      gint minpixels = scm_to_int (value);
      obj->minpixelsalloted = minpixels;
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}




static SCM
scheme_get_movement (void)
{
  gint num = g_list_index (Denemo.gui->movements, Denemo.gui->si) + 1;
  return scm_from_int (num);
}

static SCM
scheme_get_staff (void)
{
  gint num = Denemo.gui->si->currentstaffnum;
  return scm_from_int (num);
}

static SCM
scheme_get_measure (void)
{
  gint num = Denemo.gui->si->currentmeasurenum;
  return scm_from_int (num);
}

static SCM
scheme_get_cursor_note (SCM optional)
{
  DenemoGUI *gui = Denemo.gui;
  return scheme_mid_c_offsettoname (gui->si->cursor_y);
}

static SCM
scheme_get_cursor_note_with_octave (SCM optional)
{
  DenemoGUI *gui = Denemo.gui;
  scm_from_locale_string (mid_c_offsettolily (gui->si->cursor_y, 0));
  return SCM_BOOL_T;
}


static SCM
scheme_set_prefs (SCM xml)
{
  if (scm_is_string (xml))
    {
      char *xmlprefs;
      xmlprefs = scm_to_locale_string (xml);
      gint fail = readxmlprefsString (xmlprefs);
      if (xmlprefs)
        free (xmlprefs);
      return SCM_BOOL (!fail);
    }
  return SCM_BOOL (FALSE);
}


static SCM
scheme_get_boolean_pref (SCM pref)
{
  gchar *prefname = NULL;
  gboolean val;
  if (scm_is_string (pref))
    {
      prefname = scm_to_locale_string (pref);
      val = get_bool_pref (prefname);
      free (prefname);
      return SCM_BOOL (val);
    }
  return SCM_BOOL_F;
}

static SCM
scheme_get_int_pref (SCM pref)
{
  gchar *prefname = NULL;
  gint val;
  if (scm_is_string (pref))
    {
      prefname = scm_to_locale_string (pref);
      val = get_int_pref (prefname);
      free (prefname);
      return scm_from_int (val);
    }
  return SCM_BOOL_F;
}

static SCM
scheme_get_string_pref (SCM pref)
{
  gchar *prefname = NULL;
  gchar *val;
  if (scm_is_string (pref))
    {
      prefname = scm_to_locale_string (pref);
      val = get_string_pref (prefname);
      free (prefname);
      if (val)
        return scm_from_locale_string (val);
    }
  return SCM_BOOL_F;
}

SCM
scheme_attach_quit_callback (SCM callback)
{
  DenemoGUI *gui = Denemo.gui;
  if (scm_is_string (callback))
    {
      char *scheme;
      scheme = scm_to_locale_string (callback);
      gui->callbacks = g_list_prepend (gui->callbacks, scheme);
      if (scheme)
        free (scheme);
    }
  return SCM_BOOL (TRUE);
}

SCM
scheme_detach_quit_callback (void)
{
  DenemoGUI *gui = Denemo.gui;
  if (gui->callbacks)
    {
      g_free (gui->callbacks->data);
      gui->callbacks = g_list_delete_link (gui->callbacks, gui->callbacks);
      return SCM_BOOL (TRUE);
    }
  else
    g_warning ("No callback registered");
  return SCM_BOOL (FALSE);
}

SCM
scheme_get_input_source (void)
{
  return scm_from_int (Denemo.gui->input_source);
}


SCM
scheme_chordize (SCM setting)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
    return SCM_BOOL (FALSE);
  if (SCM_BOOLP (setting))
    {
      gboolean val = scm_to_bool (setting);
      if (thechord->chordize != val)
        {
          thechord->chordize = val;
          score_status (gui, TRUE);
        }
    }
  return SCM_BOOL (TRUE);
}


SCM
scheme_get_note_name (SCM optional)
{
  //char *str=NULL;
  //if(scm_is_string(optional)){
  //str = scm_to_locale_stringn(optional, &length);
  //  }
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
    return SCM_BOOL (FALSE);
  else
    {
      return scheme_mid_c_offsettoname (thenote->mid_c_offset);
    }

}



//Insert rests to the value of the timesig and return the number of rests inserted.
SCM
scheme_put_whole_measure_rests (void)
{
  DenemoGUI *gui = Denemo.gui;
  SCM scm;
  if (!Denemo.gui || !(Denemo.gui->si))
    return SCM_MAKINUM (0);
  else
    {
      gint numerator = gui->si->cursortime1;    // staff->timesig.time1;
      gint denominator = gui->si->cursortime2;  //staff->timesig.time2;
      gboolean dot = TRUE;
      if (numerator % 3)
        dot = FALSE;
      else
        numerator = 2 * numerator / 3;
      gint length = (numerator * 4) / denominator;
      gchar *str = NULL;
      scm = SCM_MAKINUM (1);
      switch (length)
        {
        case 1:                // e.g.  2/8 timesig
          str = g_strdup_printf ("(d-InsertRest2)(d-MoveCursorLeft)%s", dot ? "(d-AddDot)" : "");
          break;
        case 2:
          str = g_strdup_printf ("(d-InsertRest1)(d-MoveCursorLeft)%s", dot ? "(d-AddDot)" : "");
          break;
        case 3:                // e.g. 9/8 timesig
          str = g_strdup_printf ("(d-InsertRest0)(d-InsertRest3)(d-MoveCursorLeft)(d-MoveCursorLeft)");
          scm = SCM_MAKINUM (2);
          break;
        case 4:
          str = g_strdup_printf ("(d-InsertRest0)(d-MoveCursorLeft)%s", dot ? "(d-AddDot)" : "");
          break;
        case 8:
          str = g_strdup_printf ("(d-InsertRest0)(d-InsertRest0)(d-MoveCursorLeft)%s", dot ? "(d-AddDot)" : "");
          scm = SCM_MAKINUM (2);
          break;
        default:
          g_warning ("Not implemented %d %s", length, dot ? "dotted" : "");
          scm = SCM_MAKINUM (0);
          break;
        }
      if (str)
        {
          call_out_to_guile (str);
        }
      g_free (str);
      return scm;
    }
}

SCM
scheme_get_dots (void)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object))
    return SCM_BOOL_F;
  return scm_from_int (thechord->numdots);
}

SCM
scheme_get_note_base_duration (void)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object))
    return SCM_BOOL_F;
  return scm_from_int (thechord->baseduration);
}

SCM
scheme_get_note_duration (void)
{
  DenemoObject *curObj;
  chord *thechord;
  gint duration;
  gint numdots = 0;
  gchar *str;

  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object))
    return SCM_BOOL_F;
  if (thechord->baseduration >= 0)
    {
      duration = 1 << thechord->baseduration;
      str = g_strdup_printf ("%d", duration);
      if (thechord->numdots)
        {
          gchar *tmp = NULL;
          while (numdots++ < thechord->numdots)
            {
              tmp = g_strdup_printf ("%s" "%c", str, '.');
              g_free (str);
              str = tmp;
            }
        }

      SCM scm = scm_from_locale_string (str);
      g_free (str);
      return scm;
    }
  return SCM_BOOL_F;
}

static SCM
scheme_set_duration_in_ticks (SCM duration)
{
  DenemoObject *curObj;
  gint thedur = 0;
  if (scm_is_integer (duration))
    {
      thedur = scm_to_int (duration);
    }
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data))
    return SCM_BOOL_F;
  if (thedur > 0)
    {
      curObj->basic_durinticks = curObj->durinticks = thedur;
      if (curObj->type == CHORD)
        {
          ((chord *) curObj->object)->baseduration = -thedur;
          ((chord *) curObj->object)->numdots = 0;
        }
      objnode *prev = Denemo.gui->si->currentobject->prev;
      DenemoObject *prevObj = prev ? (DenemoObject *) prev->data : NULL;
      gint starttick = (prevObj ? prevObj->starttickofnextnote : 0);
      curObj->starttickofnextnote = starttick + thedur;
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

static SCM
scheme_get_onset_time (void)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoObject *curObj;
  if ((Denemo.gui->si->currentobject) && (curObj = Denemo.gui->si->currentobject->data))
    if ((gui->si->smfsync == gui->si->changecount))
      {
        if (curObj->midi_events)
          {
            smf_event_t *event = (smf_event_t *) curObj->midi_events->data;
            gdouble time = event->time_seconds;
            return scm_from_double (time);
          }
      }
  return SCM_BOOL_F;
}




static SCM
scheme_get_duration_in_ticks (void)
{
  DenemoObject *curObj;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data))
    return SCM_BOOL (FALSE);
  return scm_from_int (curObj->durinticks);
}

static SCM
scheme_get_base_duration_in_ticks (void)
{
  DenemoObject *curObj;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data))
    return SCM_BOOL (FALSE);
  if (curObj->type == CHORD)
    return scm_from_int (((chord *) curObj->object)->baseduration >= 0 ?        /* (* (expt 2 (- 8 number)) 6) */
                         (int) pow (2.0, (8.0 - ((chord *) curObj->object)->baseduration)) * 6 : ((chord *) curObj->object)->baseduration);
  return SCM_BOOL (FALSE);
}


SCM
scheme_get_end_tick (void)
{
  DenemoObject *curObj;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data))
    return SCM_BOOL (FALSE);
  return scm_from_int (curObj->starttickofnextnote);
}





SCM
scheme_get_measure_number (void)
{
  return scm_from_int (Denemo.gui->si->currentmeasurenum);
}




SCM
scheme_get_note (SCM count)
{
  gint index = 0;
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if (scm_is_integer (count))
    {
      index = scm_to_int (count) - 1;
      if (index < 0)
        return SCM_BOOL_F;
    }
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) g_list_nth_data (thechord->notes, index)))
    return SCM_BOOL_F;
  else
    {
      gchar *str = g_strdup_printf ("%s", mid_c_offsettolily (thenote->mid_c_offset, thenote->enshift));
      SCM scm = scm_from_locale_string (str);
      g_free (str);
      return scm;
    }

}

SCM
scheme_get_note_from_top (SCM count)
{
  gint index = 1;
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if (scm_is_integer (count))
    {
      index = scm_to_int (count);
      if (index < 1)
        return SCM_BOOL_F;
    }
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes))
    return SCM_BOOL_F;
  else
    {
      SCM scm;
      gint end = g_list_length (thechord->notes);
      index = end - index;
      if (index < 0)
        scm = SCM_BOOL_F;
      else
        {
          thenote = (note *) g_list_nth_data (thechord->notes, index);
          gchar *str = g_strdup_printf ("%s", mid_c_offsettolily (thenote->mid_c_offset, thenote->enshift));
          scm = scm_from_locale_string (str);
          g_free (str);
        }
      return scm;
    }

}

SCM
scheme_get_note_from_top_as_midi (SCM count)
{
  gint index = 1;
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if (scm_is_integer (count))
    {
      index = scm_to_int (count);
      if (index < 1)
        return SCM_BOOL_F;
    }
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes))
    return SCM_BOOL_F;
  else
    {
      SCM scm;
      gint end = g_list_length (thechord->notes);
      index = end - index;
      if (index < 0)
        scm = SCM_BOOL_F;
      else
        {
          thenote = (note *) g_list_nth_data (thechord->notes, index);
          gint midi = dia_to_midinote (thenote->mid_c_offset) + thenote->enshift;
          scm = scm_from_int (midi);
        }
      return scm;
    }

}


SCM
scheme_spell_check_midi_chord (SCM list)
{
  SCM scm;
  GList *notes = NULL;
  gboolean status;
  if (scm_is_list (list))
    {
      for (scm = list; !scm_is_null (scm); scm = scm_cdr (scm))
        {
          gint note = scm_to_int (scm_car (scm));
          notes = g_list_prepend (notes, GINT_TO_POINTER (note));
        }
      status = check_midi_intervals (notes);
      g_list_free (notes);
      return status ? SCM_BOOL_T : SCM_BOOL_F;
    }
  else
    {
      g_print ("Bad pitch spell list\n");
      return SCM_BOOL_F;
    }
}


SCM
scheme_get_cursor_note_as_midi (SCM optional)
{

  DenemoGUI *gui = Denemo.gui;
  gint midi = dia_to_midinote (gui->si->cursor_y);
  SCM scm = scm_from_int (midi);
  return scm;
}


SCM
scheme_get_note_as_midi (void)
{
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
    return scm_from_int (0);
  else
    {
      gint midi = dia_to_midinote (thenote->mid_c_offset) + thenote->enshift;
      SCM scm = scm_from_int (midi);
      return scm;
    }
}


SCM
scheme_get_notes (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  GString *str = g_string_new ("");
  SCM scm;

  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
    return SCM_BOOL (FALSE);
  else
    {
      GList *g;
      for (g = thechord->notes; g; g = g->next)
        {
          thenote = (note *) g->data;
          gchar *name = mid_c_offsettolily (thenote->mid_c_offset, thenote->enshift);
          str = g_string_append (str, name);
          if (g->next)
            str = g_string_append (str, " ");
        }

      scm = scm_from_locale_string (g_string_free (str, FALSE));
      return scm;
    }
}

SCM
scheme_add_movement (SCM optional)
{
  append_blank_movement ();
  return SCM_BOOL_T;
}

SCM
scheme_get_prevailing_clef (SCM optional)
{
  gint theclef = find_prevailing_clef (Denemo.gui->si);
  //FIXME look at directives to see if it is overridden, e.g. drum clef
  const gchar *clefname = get_clef_name (theclef);
  if (clefname)
    return scm_from_locale_string (clefname);
  else
    return SCM_BOOL_F;
}

SCM
scheme_get_prevailing_clef_as_lilypond (SCM optional)
{
  const gchar *clefname = get_prevailing_clef_as_lilypond ();
  if (clefname)
    return scm_from_locale_string (clefname);
  else
    return SCM_BOOL_F;
}

SCM
scheme_get_prevailing_keysig_as_lilypond (SCM optional)
{
  const gchar *keysigname = get_prevailing_keysig_as_lilypond ();
  if (keysigname)
    return scm_from_locale_string (keysigname);
  else
    return SCM_BOOL_F;
}

SCM
scheme_get_prevailing_timesig_as_lilypond (SCM optional)
{
  const gchar *timesigname = get_prevailing_timesig_as_lilypond ();
  if (timesigname)
    return scm_from_locale_string (timesigname);
  else
    return SCM_BOOL_F;
}

SCM
scheme_get_prevailing_duration (SCM optional)
{
  return scm_from_int (get_prevailing_duration ());
}

SCM
scheme_get_prevailing_timesig (SCM optional)
{
  timesig *timesig = get_prevailing_context (TIMESIG);
  //FIXME look at directives to see if it is overridden, e.g. drum clef
  gchar *name = g_strdup_printf ("%d/%d", timesig->time1, timesig->time2);
  SCM ret = scm_from_locale_string (name);
  g_free (name);
  return ret;
}

SCM
scheme_get_prevailing_keysig (SCM optional)
{
  GString *str = g_string_new (" ");
  keysig *keysig = get_prevailing_context (KEYSIG);
  gint i;
  for (i = 0; i < 7; i++)
    g_string_append_printf (str, "%d ", keysig->accs[i]);
  return scm_from_locale_string (g_string_free (str, FALSE));
}

SCM
scheme_set_prevailing_keysig (SCM keyaccs)
{
  //keysigs have a field called "number" which determines how it is drawn, setting like this does not get a keysig drawn, nor does it affect lilypond output
  char *accs = NULL;
  if (scm_is_string (keyaccs))
    {
      accs = scm_to_locale_string (keyaccs);
    }
  if (!accs)
    {
      return SCM_BOOL_F;
    }
  keysig *keysig = get_prevailing_context (KEYSIG);
  sscanf (accs, "%d%d%d%d%d%d%d", keysig->accs + 0, keysig->accs + 1, keysig->accs + 2, keysig->accs + 3, keysig->accs + 4, keysig->accs + 5, keysig->accs + 6);
  showwhichaccidentalswholestaff ((DenemoStaff *) Denemo.gui->si->currentstaff->data);
  free (accs);
  displayhelper (Denemo.gui);   //score_status(Denemo.gui, TRUE);
  return SCM_BOOL_T;
}

SCM
scheme_increment_initial_keysig (SCM amount)
{
  DenemoStaff *curstaff = Denemo.gui->si->currentstaff->data;
  SCM ret = SCM_BOOL_F;
  gint inc = 1;
  if (scm_is_integer (amount))
    inc = scm_to_int (amount);
  keysig *sig = &curstaff->keysig;
  inc += sig->number;
  if (inc < 8 && inc > -8)
    {
      dnm_setinitialkeysig (curstaff, inc, curstaff->keysig.isminor);
      score_status (Denemo.gui, TRUE);
      ret = SCM_BOOL_T;
    }
  return ret;
}

SCM
scheme_increment_keysig (SCM amount)
{
  DenemoStaff *curstaff = Denemo.gui->si->currentstaff->data;
  DenemoObject *curObj = NULL;
  SCM ret = SCM_BOOL_F;
  gint inc = 1;
  if (scm_is_integer (amount))
    inc = scm_to_int (amount);
  keysig *sig = &curstaff->keysig;
  if ((Denemo.gui->si->currentobject) && (curObj = Denemo.gui->si->currentobject->data) && (curObj->type == KEYSIG))
    {
      sig = curObj->object;
    }

  inc += sig->number;
  if (inc < 8 && inc > -8)
    {
      if (sig == &curstaff->keysig)
        {
          dnm_setinitialkeysig (curstaff, inc, curstaff->keysig.isminor);
        }
      else
        {
          sig->number = inc;
          initkeyaccs (sig->accs, inc);
          set_basic_numticks (curObj);
          setpixelmin (curObj);
        }
      score_status (Denemo.gui, TRUE);
      displayhelper (Denemo.gui);
      ret = SCM_BOOL_T;
    }
  return ret;
}

SCM
scheme_cursor_to_note (SCM lilyname)
{
  DenemoGUI *gui = Denemo.gui;
  gint mid_c_offset;
  gint enshift;
  char *notename;

  if (scm_is_string (lilyname))
    {
      notename = scm_to_locale_string (lilyname);
      interpret_lilypond_notename (notename, &mid_c_offset, &enshift);
      gui->si->cursor_y = mid_c_offset;
      gui->si->staffletter_y = offsettonumber (gui->si->cursor_y);
      displayhelper (gui);
      if (notename)
        free (notename);
      return SCM_BOOL (TRUE);
    }
  else
    {
      return SCM_BOOL (FALSE);
    }
}

SCM
scheme_change_chord_notes (SCM lilynotes)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  char *notename;
  gchar *chordnote;
  gint mid_c_offset;
  gint enshift;
  gint dclef;
  GList *g = NULL;
  GList *n = NULL;
  GList *directives = NULL;

  if (scm_is_string (lilynotes))
    {

      if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
        return SCM_BOOL (FALSE);

      else
        {
          /* delete all chord tones */
          while (thechord->notes)
            {
              thenote = thechord->notes->data;
              g = g_list_append (g, thenote->directives);
              thenote->directives = NULL;
              delete_chordnote (gui);
            }
          /* add changed tones */
          dclef = find_prevailing_clef (Denemo.gui->si);
          notename = scm_to_locale_string (lilynotes);
          chordnote = strtok (notename, " ");
          while (chordnote)
            {
              interpret_lilypond_notename (chordnote, &mid_c_offset, &enshift);
              dnm_addtone (curObj, mid_c_offset, enshift, dclef);
              chordnote = strtok (NULL, " ");
            }
          /* paste directives over */
          for (n = thechord->notes; n && g; n = n->next, g = g->next)
            {
              thenote = (note *) n->data;
              directives = (GList *) g->data;

              if (directives)
                thenote->directives = directives;

            }
          score_status (gui, TRUE);
          displayhelper (gui);
          if (notename)
            free (notename);
          return SCM_BOOL (TRUE);
        }
    }
  else
    return SCM_BOOL (FALSE);
}

SCM
scheme_get_user_input (SCM label, SCM prompt, SCM init, SCM modal)
{
  char *title, *instruction, *initial_value;

  if (scm_is_string (label))
    {
      title = scm_to_locale_string (label);
    }
  else
    title = strdup ("Input Required");
  if (scm_is_string (prompt))
    {
      instruction = scm_to_locale_string (prompt);
    }
  else
    instruction = strdup ("Give input: ");

  if (scm_is_string (init))
    {
      initial_value = scm_to_locale_string (init);
    }
  else
    initial_value = strdup (" ");

  gchar *ret = string_dialog_entry_with_widget_opt (Denemo.gui, title, instruction, initial_value, NULL, (modal == SCM_UNDEFINED) || scm_is_true (modal));
  SCM scm = ret ? scm_from_locale_string (ret) : SCM_BOOL_F;

  if (title)
    free (title);
  if (instruction)
    free (instruction);
  if (initial_value)
    free (initial_value);
  if (ret)
    g_free (ret);
  return scm;
}

static void
paste_snippet_lilypond (GtkWidget * button)
{
  DenemoGUI *gui = Denemo.gui;
  GtkWidget *hbox = gtk_widget_get_parent (button);
  GtkWidget *textbuffer = (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textbuffer");
  if (textbuffer)
    {
      RhythmPattern *r = (gui->currhythm) ? ((RhythmPattern *) gui->currhythm->data) : NULL;
      if (r)
        {
          const gchar *transpose, *clefname = get_prevailing_clef_as_lilypond (), *keysigname = get_prevailing_keysig_as_lilypond (), *timesigname = get_prevailing_timesig_as_lilypond ();
          extern gchar *score_directive_get_postfix (gchar * tagname);
          transpose = score_directive_get_postfix ("TransposeScorePrint");
          transpose = transpose ? transpose : "";
          gchar *text = g_strdup_printf ("§\\raise #6.0 \\score { %s { {%s}{%s}{%s} %s } \\layout {indent=0.0}}§", transpose, clefname, keysigname, timesigname, r->lilypond);
          gtk_text_buffer_insert_at_cursor (GTK_TEXT_BUFFER (textbuffer), text, -1 /*gint len */ );
          g_free (text);
        }
    }
  else
    {
      g_warning ("Denemo program error, widget hierarchy changed???");
    }
  GtkWidget *textview = (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textview");
  gtk_widget_grab_focus (textview);
}

#define SECTION_UTF8_STRING "§"
//#define SECTION_UTF8_STRING "\302\247"

static gchar *
create_lilypond_from_text (gchar * orig)
{
  gchar *text = g_strdup (orig);
  GString *ret = g_string_new ("");
  g_print ("looking at %s\n", text);
  gunichar section = g_utf8_get_char (SECTION_UTF8_STRING);
  gchar *this = g_utf8_strchr (text, -1, section);
  if (this)
    {
      gchar *start = g_utf8_next_char (this);
      *this = 0;
      if (*text)
        {
          g_string_append_printf (ret, "\\wordwrap-string #\"%s\"", g_strescape (text, "\"\\"));
        }
      gchar *end = g_utf8_strchr (start, -1, section);
      if (end == NULL)
        {
          g_warning ("Unbalanced § marks");
          g_string_free (ret, TRUE);
          return g_strdup ("%{error %}");
        }
      gchar *next = g_utf8_next_char (end);
      *end = 0;
      g_string_append_printf (ret, "%s %s", start, create_lilypond_from_text (next));
    }
  else
    {
      g_string_append_printf (ret, "\\wordwrap-string #\"%s\"", g_strescape (text, "\"\\"));
    }
  g_free (text);
  return g_string_free (ret, FALSE);
}

SCM
scheme_get_user_input_with_snippets (SCM label, SCM prompt, SCM init, SCM modal)
{
  char *title, *instruction, *initial_value;
  SCM scm;
  if (scm_is_string (label))
    {
      title = scm_to_locale_string (label);
    }
  else
    title = strdup ("Input Required");
  if (scm_is_string (prompt))
    {
      instruction = scm_to_locale_string (prompt);
    }
  else
    instruction = strdup ("Give input: ");

  if (scm_is_string (init))
    {
      initial_value = scm_to_locale_string (init);
    }
  else
    initial_value = strdup (" ");
  GtkWidget *hbox = gtk_hbox_new (FALSE, 8);
  GtkWidget *button = gtk_button_new_with_label (_("Paste Current Snippet"));
  gtk_widget_set_tooltip_text (button, _("Pastes the music captured in the currently selected Snippet into the text at the cursor. The music appears here in the LilyPond typesetter syntax between two markers (\302\247). It will print as typeset music embedded in the sentence you are writing.\nYou can edit the syntax, taking care to leave the markers in position. If you delete one marker be sure to delete the other.\n"));
// if(!Denemo.gui->rhythms)
  //   gtk_widget_set_sensitive(button, FALSE);
  g_signal_connect (button, "clicked", G_CALLBACK (paste_snippet_lilypond), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  button = gtk_button_new_with_label (_("Next Snippet"));
  gtk_widget_set_tooltip_text (button, _("Makes the next Snippet the one that can be pasted. To see the music snippets you need to check View → Snippets\nThe one selected is in bold black."));
  GtkAction *action = gtk_ui_manager_get_action (Denemo.ui_manager, "/ObjectMenu/NotesRests/SelectDuration/NextRhythm");
  if (action)
    g_signal_connect_swapped (button, "clicked", G_CALLBACK (gtk_action_activate), action);
  else
    gtk_widget_set_sensitive (button, FALSE);
// if(!Denemo.gui->rhythms)
//   gtk_widget_set_sensitive(button, FALSE);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  gchar *text = string_dialog_editor_with_widget_opt (Denemo.gui, title, instruction, initial_value, hbox, (modal == SCM_UNDEFINED) || scm_is_true (modal));
  if (text)
    {
      gchar *lilypond = create_lilypond_from_text (text);
      scm = scm_cons (scm_from_locale_string (text), scm_from_locale_string (lilypond));
      g_free (lilypond);
    }
  else
    scm = SCM_BOOL_F;

  if (title)
    free (title);
  if (instruction)
    free (instruction);
  if (initial_value)
    free (initial_value);
  if (text)
    g_free (text);

  return scm;
}

SCM
scheme_warningdialog (SCM msg)
{
  char *title;
  if (scm_is_string (msg))
    {
      title = scm_to_locale_string (msg);
    }
  else
    title = strdup ("Script generated warning");

  warningdialog (title);
  if (title)
    free (title);
  return msg;
}

SCM
scheme_infodialog (SCM msg)
{
  char *title;
  if (scm_is_string (msg))
    {
      title = scm_to_locale_string (msg);
      msg = SCM_BOOL (TRUE);
    }
  else
    {
      title = strdup (_("Script error, wrong parameter type to d-InfoDialog"));
      msg = SCM_BOOL (FALSE);
    }
  static GtkWidget *dialog;

  if (dialog)
    {
      gtk_widget_show (dialog);
      gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (dialog), title);
    }
  else
    {
      dialog = infodialog (title);
      g_signal_connect (dialog, "delete-event", G_CALLBACK (gtk_widget_hide_on_delete), NULL);
    }
  if (*title)
    {
      gtk_widget_show (dialog);
    }
  else
    {
      gtk_widget_hide (dialog);
    }
  if (title)
    free (title);
  return msg;
}

SCM
scheme_progressbar (SCM msg)
{
  char *title = NULL;
  if (scm_is_string (msg))
    {
      title = scm_to_locale_string (msg);
      progressbar (title);
      msg = SCM_BOOL (TRUE);
    }
  else
    msg = SCM_BOOL (FALSE);
  if (title)
    free (title);
  return msg;
}

SCM
scheme_progressbar_stop (void)
{
  progressbar_stop ();
  return SCM_BOOL (TRUE);
}

static SCM
scheme_typeset_for_script (SCM thescript)
{
  SCM ret = SCM_BOOL_F;
#ifndef USE_EVINCE
  g_debug("This feature requires denemo to be built with evince");
#else  
  if (scm_is_string (thescript))
    {
      gchar *script = scm_to_locale_string (thescript);
      if (typeset_for_script (script))
        ret = SCM_BOOL_T;
    }
#endif
  return ret;
}

static SCM
scheme_print_typeset_pdf (void)
{
#ifndef USE_EVINCE
  g_debug("This feature requires denemo to be built with evince");
  return SCM_BOOL_F;
#else
  return print_typeset_pdf ()? SCM_BOOL_F : SCM_BOOL_T;
#endif
}

SCM
scheme_get_char (void)
{

  GdkEventKey event;
  gboolean success = intercept_scorearea_keypress (&event);
  if (success)
    {
      gchar *str = g_strdup_printf ("%c", success ? event.keyval : 0);
      SCM scm = scm_from_locale_string (str);
      g_free (str);
      return scm;
    }
  else
    return SCM_BOOL (FALSE);
}

SCM
scheme_get_keypress (void)
{
  GdkEventKey event;
  gboolean success = intercept_scorearea_keypress (&event);
  if (success)
    {
      gchar *str = dnm_accelerator_name (event.keyval, event.state);
      SCM scm = scm_from_locale_string (str);
      g_free (str);
      return scm;
    }
  else
    return SCM_BOOL (FALSE);
}

/* get last keypress that successfully invoked a command */
SCM
scheme_get_command_keypress (void)
{
  gchar *str = dnm_accelerator_name (Denemo.last_keyval, Denemo.last_keystate);
  SCM scm = scm_from_locale_string (str);
  g_free (str);
  return scm;
}



SCM
scheme_get_command (void)
{
  GdkEventKey event;
  GString *name = g_string_new ("");
  gboolean success = intercept_scorearea_keypress (&event);
  if (success)
    {
      gint cmd = lookup_command_for_keyevent (&event);
      //g_print("command %d for %x %x\n", cmd, event.keyval, event.state);
      if (cmd != -1)
        name = g_string_append (name, lookup_name_from_idx (Denemo.map, cmd));  //FIXME NULL?, memory leaks
      name = g_string_prepend (name, DENEMO_SCHEME_PREFIX);
    }
  SCM scm = success ? scm_from_locale_string (name->str) : SCM_BOOL (FALSE);
  g_string_free (name, TRUE);
  return scm;
}


gchar *
return_command (gchar * name, GdkEvent * event)
{
  return name;
}

/* listens for a shortcut and returns a command, or if keypresses are not shortcut returns #f */
SCM
scheme_get_command_from_user (void)
{

  GdkEventKey event;

  if (intercept_scorearea_keypress (&event))
    {
      gchar *command = process_key_event (&event, &return_command);
      if (command == NULL)
        return SCM_BOOL_F;
      if (*command == 0)
        {                       //can be two-key shortcut
          if (intercept_scorearea_keypress (&event))
            {
              command = process_key_event (&event, &return_command);
              if (command == NULL)
                return SCM_BOOL_F;
            }
          else
            return SCM_BOOL_F;
        }
      write_status (Denemo.gui);
      SCM scm = scm_from_locale_string (command);       //command is from lookup_name_from... functions, do not free.
      return scm;
    }
  return SCM_BOOL_F;
}


/*UNUSED
static void
get_drag_offset (GtkWidget * dialog, gint response_id, GtkLabel * label)
{
  g_object_set_data (G_OBJECT (dialog), "offset-response", (gpointer) (intptr_t) response_id);
  if (response_id < 0)
    gtk_main_quit ();
  gint offsetx, offsety;
  offsetx = (intptr_t) g_object_get_data (G_OBJECT (Denemo.printarea), "offsetx");
  offsety = (intptr_t) g_object_get_data (G_OBJECT (Denemo.printarea), "offsety");
  gchar *text = g_strdup_printf ("Offset now %d %d. Drag again in the print window to change\nOr click OK to apply the position shift", offsetx, offsety);
  gtk_label_set_text (label, text);
  g_free (text);
}*/

static void
get_drag_pad (GtkWidget * dialog, gint response_id, GtkLabel * label)
{
  g_object_set_data (G_OBJECT (dialog), "pad-response", (gpointer) (intptr_t) response_id);
  if (response_id < 0)
    gtk_main_quit ();
  gint padding;
  padding = (intptr_t) g_object_get_data (G_OBJECT (Denemo.printarea), "padding");
  gchar *text = g_strdup_printf ("Padding now %d. Drag again in the print window to change\nOr click OK to apply the padding to the graphical object belonging to the directive", padding);
  gtk_label_set_text (label, text);
  g_free (text);
}



/* return a string representing the relative font size the user wishes to use*/
SCM
scheme_get_relative_font_size (void)
{
  if (Denemo.printarea == NULL)
    return SCM_BOOL (FALSE);
  gchar *value = g_object_get_data (G_OBJECT (Denemo.printarea), "font-size");
  if (value)
    g_free (value);
  value = string_dialog_entry (Denemo.gui, "Font Size", "Give a value (+/-) to adjust font size by", "0");
  if (!value)
    value = g_strdup ("0");
  gchar *clean = g_strdup_printf ("%d", atoi (value));
  g_free (value);
  g_object_set_data (G_OBJECT (Denemo.printarea), "font-size", (gpointer) clean);
  return scm_from_locale_stringn (clean, strlen (clean));
}

void get_clipboard (GtkAction * action, DenemoScriptParam * param);
/* return a string from the X selection */
SCM
scheme_get_text_selection (void)
{
  SCM ret;
  DenemoScriptParam param;
  get_clipboard (NULL, &param);
  if (param.status)
    {
      ret = scm_from_locale_stringn (param.string->str, param.string->len);
      g_string_free (param.string, TRUE);
    }
  else
    ret = SCM_BOOL (FALSE);
  return ret;
}






/* return a string representing the padding desired for some lilypond graphic
 or #f if no printarea or user cancels*/
SCM
scheme_get_padding (void)
{
  SCM ret;
  if (Denemo.printarea == NULL)
    return SCM_BOOL (FALSE);
  if (g_object_get_data (G_OBJECT (Denemo.printarea), "pad-dialog"))
    {
      warningdialog (_("Already in a padding dialog"));
      return SCM_BOOL_F;
    }

  gint padding = (intptr_t) g_object_get_data (G_OBJECT (Denemo.printarea), "padding");

  GtkWidget *dialog = gtk_dialog_new_with_buttons ("Select Padding in Print Window",
                                                   GTK_WINDOW (Denemo.window),
                                                   (GtkDialogFlags) (GTK_DIALOG_DESTROY_WITH_PARENT),
                                                   GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
                                                   GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
                                                   NULL);
  g_object_set_data (G_OBJECT (Denemo.printarea), "pad-dialog", (gpointer) dialog);
  GtkWidget *vbox = gtk_vbox_new (FALSE, 8);

  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_container_add (GTK_CONTAINER (content_area), vbox);

  gchar *text = g_strdup_printf ("Current padding is %d\nUse right click in print window to change this\nClick OK to apply the padding to the music item drawn by the directive", padding);
  GtkWidget *label = gtk_label_new (text);
  g_free (text);
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, TRUE, 0);
  gtk_widget_show_all (dialog);

  gint val;

  g_signal_connect (dialog, "response", G_CALLBACK (get_drag_pad), label);
  gtk_widget_show_all (dialog);
  gtk_main ();
  padding = (intptr_t) g_object_get_data (G_OBJECT (Denemo.printarea), "padding");
  val = (intptr_t) g_object_get_data (G_OBJECT (dialog), "pad-response");
  g_object_set_data (G_OBJECT (Denemo.printarea), "pad-dialog", NULL);
  gtk_widget_destroy (dialog);
  if (val == GTK_RESPONSE_ACCEPT)
    {
      gchar *pad = g_strdup_printf ("%d", padding / 10);
      ret = scm_from_locale_string (pad);
      g_free (pad);
    }
  else
    ret = SCM_BOOL (FALSE);
  return ret;
}



/* create a dialog with the options & return the one chosen, of #f if
   the user cancels
*/
SCM
scheme_get_option (SCM options)
{
  gchar *response = NULL;
  size_t length;
  //gchar *str=NULL;
  if (scm_is_string (options))
    {
      char *str_unterm;
      str_unterm = scm_to_locale_stringn (options, &length);
      response = get_option (str_unterm, length);       //returns NULL or a pointer to a location in str_unterm
      //g_print("Got %p holding %s\n", response, response);
      if (response)
        response = g_strdup (response);
      if (str_unterm)
        free (str_unterm);
    }
  if (response)
    {
      SCM ret = scm_from_locale_stringn (response, strlen (response));
      //g_print("Freeing %p holding %s\n", response, response);
      g_free (response);        //FIXME the g_strdup above is not needed?
      return ret;
      //return scm_from_locale_stringn (response, strlen(response));
    }
  else
    {
      return SCM_BOOL_F;
    }
}


/* Scheme interface to DenemoDirectives (formerly LilyPond directives attached to notes/chords) */


static SCM
scheme_lock_directive (SCM lock)
{
  DenemoObject *curObj;
  DenemoDirective *directive;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != LILYDIRECTIVE) || !(directive = (DenemoDirective *) curObj->object))
    return SCM_BOOL (FALSE);
  directive->locked = scm_is_true (lock);
  return SCM_BOOL_T;
}


/* store the script to be invoked as an action for a directive tagged with tag */
static SCM
scheme_set_action_script_for_tag (SCM tag, SCM script)
{
  if (scm_is_string (tag))
    {
      char *the_tag;
      the_tag = scm_to_locale_string (tag);
      if (scm_is_string (script))
        {
          char *the_script;
          the_script = scm_to_locale_string (script);
          gchar *stored_script = g_strdup (the_script); //FIXME
          free (the_script);
          set_action_script_for_tag (the_tag, stored_script);
          if (the_tag)
            free (the_tag);
          return SCM_BOOL (TRUE);
        }
      if (the_tag)
        free (the_tag);
    }
  return SCM_BOOL (FALSE);
}



#define GET_NTH_TAG(what)\
 static SCM scheme_##what##_directive_get_nth_tag(SCM index) {\
  gint n;\
  if(!scm_is_integer(index))\
     return SCM_BOOL_F;\
    n = scm_to_int(index);\
  extern gchar *get_nth_##what##_tag (gint n);\
  gchar *val = get_nth_##what##_tag (n);\
  if(val) return scm_from_locale_stringn (val, strlen(val));\
  return SCM_BOOL_F;\
}
GET_NTH_TAG (chord);
GET_NTH_TAG (note);
GET_NTH_TAG (staff);
GET_NTH_TAG (voice);
GET_NTH_TAG (score);
GET_NTH_TAG (clef);
GET_NTH_TAG (timesig);
GET_NTH_TAG (tuplet);
GET_NTH_TAG (stemdirective);
GET_NTH_TAG (keysig);
GET_NTH_TAG (scoreheader);
GET_NTH_TAG (header);
GET_NTH_TAG (paper);
GET_NTH_TAG (layout);
GET_NTH_TAG (movementcontrol);
#undef GET_NTH_TAG








#define GET_TAG_FN_DEF(what)\
 static SCM scheme_##what##_directive_get_tag(SCM tag) {\
  char *tagname;\
  if(!scm_is_string(tag))\
     tagname = NULL;\
  else { \
    tagname = scm_to_locale_string(tag);\
  } \
  extern gchar *what##_directive_get_tag (gchar *tagname);\
  gchar *val = (gchar*)what##_directive_get_tag ((gchar*)tagname);\
  if(val){\
    SCM ret = scm_from_locale_stringn (val, strlen(val));\
    if(tagname) free(tagname);\
    return ret;\
  }\
  if(tagname) free(tagname);\
  return SCM_BOOL(FALSE);\
}

GET_TAG_FN_DEF (object);
GET_TAG_FN_DEF (standalone);
GET_TAG_FN_DEF (chord);
GET_TAG_FN_DEF (note);
GET_TAG_FN_DEF (staff);
GET_TAG_FN_DEF (voice);
GET_TAG_FN_DEF (score);
GET_TAG_FN_DEF (clef);
GET_TAG_FN_DEF (timesig);
GET_TAG_FN_DEF (tuplet);
GET_TAG_FN_DEF (stemdirective);
GET_TAG_FN_DEF (keysig);
GET_TAG_FN_DEF (scoreheader);
GET_TAG_FN_DEF (header);
GET_TAG_FN_DEF (paper);
GET_TAG_FN_DEF (layout);
GET_TAG_FN_DEF (movementcontrol);
#undef GET_TAG_FN_DEF
#define ACTIVATE_FN_DEF(what)\
 static SCM scheme_activate_##what##_directive(SCM tag) {\
  if(!scm_is_string(tag)){\
    return SCM_BOOL(FALSE);\
  }\
  char *tagname;\
  tagname = scm_to_locale_string(tag);\
  extern gboolean activate_##what##_directive (gchar *tagname);\
  gboolean ret = activate_##what##_directive (tagname);\
  if(tagname) g_free(tagname);\
  return SCM_BOOL(ret);\
}

#define EDIT_FN_DEF(what)\
 static SCM scheme_text_edit_##what##_directive(SCM tag) {\
  if(!scm_is_string(tag)){\
    return SCM_BOOL(FALSE);\
  }\
  char *tagname;\
  tagname = scm_to_locale_string(tag);\
  extern gboolean text_edit_##what##_directive (gchar *tagname);\
  gboolean ret = text_edit_##what##_directive (tagname);\
  if(tagname) g_free(tagname);\
  return SCM_BOOL(ret);\
}
#define DELETE_FN_DEF(what)\
 static SCM scheme_delete_##what##_directive(SCM tag) {\
  if(!scm_is_string(tag)){\
    return SCM_BOOL(FALSE);\
  }\
  char *tagname;\
  tagname = scm_to_locale_string(tag);\
  extern gboolean delete_##what##_directive (gchar *tagname);\
  gboolean ret = delete_##what##_directive (tagname);\
  if(tagname) free(tagname);\
  return SCM_BOOL(ret);\
}
#define EDIT_DELETE_FN_DEF(what)\
EDIT_FN_DEF(what)\
DELETE_FN_DEF(what)\
ACTIVATE_FN_DEF(what)

EDIT_FN_DEF (standalone) EDIT_DELETE_FN_DEF (note) EDIT_DELETE_FN_DEF (chord) EDIT_DELETE_FN_DEF (staff) EDIT_DELETE_FN_DEF (voice) EDIT_DELETE_FN_DEF (score)
#define GETFUNC_DEF(what, field)\
static SCM scheme_##what##_directive_get_##field(SCM tag) {\
  if(!scm_is_string(tag)){\
    return SCM_BOOL(FALSE);\
  }\
  char *tagname;\
  tagname = scm_to_locale_string(tag);\
  extern gchar* what##_directive_get_##field(gchar *tagname);\
  gchar *value = (gchar*)what##_directive_get_##field((gchar*)tagname);\
  if(tagname) free(tagname);\
  if(value){\
    return scm_from_locale_string(value);\
  }\
  return SCM_BOOL(FALSE);\
}
#define PUTFUNC_DEF(what, field)\
static SCM scheme_##what##_directive_put_##field(SCM tag, SCM value) {\
  if((!scm_is_string(tag))||(!scm_is_string(value)))\
     return SCM_BOOL(FALSE);\
  char *tagname;\
  tagname = scm_to_locale_string(tag);\
  char *valuename;\
  valuename = scm_to_locale_string(value);\
  extern gboolean what##_directive_put_##field (gchar *tagname, gchar *valuename);\
  gboolean ret = what##_directive_put_##field ((gchar*)tagname, (gchar*)valuename);\
  if(tagname) free(tagname);\
  if(valuename) free(valuename);\
  return SCM_BOOL(ret);\
}
//block to clone for new GString entries in DenemoDirective
  GETFUNC_DEF (note, display);
GETFUNC_DEF (chord, display);
GETFUNC_DEF (standalone, display);
GETFUNC_DEF (staff, display);
GETFUNC_DEF (voice, display);
GETFUNC_DEF (score, display);
GETFUNC_DEF (movementcontrol, display);
PUTFUNC_DEF (note, display);
PUTFUNC_DEF (chord, display);
PUTFUNC_DEF (standalone, display);
PUTFUNC_DEF (staff, display);
PUTFUNC_DEF (voice, display);
PUTFUNC_DEF (score, display);
PUTFUNC_DEF (movementcontrol, display);
// end of block to clone ??? there are now stem tuplet and keysigs as well - see grob
GETFUNC_DEF (note, grob);
GETFUNC_DEF (chord, grob);
GETFUNC_DEF (standalone, grob);
GETFUNC_DEF (staff, grob);
GETFUNC_DEF (voice, grob);
GETFUNC_DEF (score, grob);
/*UNUSED
  GETFUNC_DEF (movementcontrol, grob);
  */
GETFUNC_DEF (clef, grob);
GETFUNC_DEF (timesig, grob);
GETFUNC_DEF (tuplet, grob);
GETFUNC_DEF (stemdirective, grob);
GETFUNC_DEF (keysig, grob);
PUTFUNC_DEF (note, grob);
PUTFUNC_DEF (chord, grob);
PUTFUNC_DEF (standalone, grob);
//PUTFUNC_DEF(staff, grob)
//PUTFUNC_DEF(voice, grob)
PUTFUNC_DEF (score, grob)
//PUTFUNC_DEF(movementcontrol, grob)
  PUTFUNC_DEF (clef, grob)
PUTFUNC_DEF (timesig, grob)
PUTFUNC_DEF (tuplet, grob)
PUTFUNC_DEF (stemdirective, grob)
PUTFUNC_DEF (keysig, grob)
GETFUNC_DEF (note, midibytes)
GETFUNC_DEF (chord, midibytes)
GETFUNC_DEF (standalone, midibytes) GETFUNC_DEF (staff, midibytes) GETFUNC_DEF (voice, midibytes) GETFUNC_DEF (score, midibytes) GETFUNC_DEF (movementcontrol, midibytes) PUTFUNC_DEF (note, midibytes) PUTFUNC_DEF (chord, midibytes) PUTFUNC_DEF (standalone, midibytes) PUTFUNC_DEF (staff, midibytes) PUTFUNC_DEF (voice, midibytes) PUTFUNC_DEF (score, midibytes) PUTFUNC_DEF (movementcontrol, midibytes) GETFUNC_DEF (note, prefix) GETFUNC_DEF (note, postfix) PUTFUNC_DEF (note, prefix)
  //PUTFUNC_DEF(clef, prefix)
  PUTFUNC_DEF (note, postfix)
GETFUNC_DEF (score, prefix)
GETFUNC_DEF (score, postfix)
PUTFUNC_DEF (score, prefix)
PUTFUNC_DEF (score, postfix)
PUTFUNC_DEF (staff, prefix) PUTFUNC_DEF (voice, prefix) GETFUNC_DEF (staff, prefix) GETFUNC_DEF (voice, prefix) PUTFUNC_DEF (staff, postfix) PUTFUNC_DEF (voice, postfix) GETFUNC_DEF (staff, postfix) GETFUNC_DEF (voice, postfix) GETFUNC_DEF (chord, prefix) GETFUNC_DEF (chord, postfix) PUTFUNC_DEF (chord, prefix) PUTFUNC_DEF (chord, postfix) GETFUNC_DEF (standalone, prefix) GETFUNC_DEF (standalone, postfix) PUTFUNC_DEF (standalone, prefix) PUTFUNC_DEF (standalone, postfix)
#define INT_PUTFUNC_DEF(what, field)\
static SCM scheme_##what##_directive_put_##field(SCM tag, SCM value) {\
  if((!scm_is_string(tag))||(!scm_is_integer(value))){\
    return SCM_BOOL(FALSE);\
  }\
  char *tagname;\
  tagname = scm_to_locale_string(tag);\
  gint valuename = scm_to_int(value);\
  extern  gboolean  what##_directive_put_##field (gchar *tag, gint value);\
  gboolean ret = what##_directive_put_##field ((gchar*)tagname, valuename);\
  if(tagname) free(tagname);\
  return SCM_BOOL(ret);\
}
#define INT_GETFUNC_DEF(what, field)\
static SCM scheme_##what##_directive_get_##field(SCM tag) {\
  if(!scm_is_string(tag)){\
    return SCM_BOOL(FALSE);\
  }\
  char *tagname;\
  tagname = scm_to_locale_string(tag);\
  extern gint what##_directive_get_##field (gchar *tag);\
  gint ret = what##_directive_get_##field ((gchar*)tagname);\
  if(tagname) free(tagname);\
  return scm_from_int(ret);\
}
#define PUTGRAPHICFUNC_DEF(what)\
static SCM scheme_##what##_directive_put_graphic(SCM tag, SCM value) {\
  if((!scm_is_string(tag))||(!scm_is_string(value))){\
    return SCM_BOOL(FALSE);\
  }\
  char *tagname;\
  tagname = scm_to_locale_string(tag);\
  char *valuename;\
  valuename = scm_to_locale_string(value);\
  gboolean ret = what##_directive_put_graphic ((gchar*)tagname, (gchar*)valuename);\
  if(tagname) free(tagname);\
  if(valuename) free(valuename);\
  return SCM_BOOL(ret);\
}
  PUTGRAPHICFUNC_DEF (note);
PUTGRAPHICFUNC_DEF (chord);
PUTGRAPHICFUNC_DEF (standalone);
PUTGRAPHICFUNC_DEF (staff);
PUTGRAPHICFUNC_DEF (voice);
PUTGRAPHICFUNC_DEF (score);


     //block to copy for new int field in directive
INT_PUTFUNC_DEF (note, minpixels)
INT_PUTFUNC_DEF (chord, minpixels)
INT_PUTFUNC_DEF (standalone, minpixels)
INT_PUTFUNC_DEF (staff, minpixels)
INT_PUTFUNC_DEF (voice, minpixels)
INT_PUTFUNC_DEF (score, minpixels)
INT_PUTFUNC_DEF (clef, minpixels)
INT_PUTFUNC_DEF (timesig, minpixels)
INT_PUTFUNC_DEF (tuplet, minpixels)
INT_PUTFUNC_DEF (stemdirective, minpixels)
INT_PUTFUNC_DEF (keysig, minpixels)
INT_PUTFUNC_DEF (scoreheader, minpixels)
INT_PUTFUNC_DEF (header, minpixels)
INT_PUTFUNC_DEF (paper, minpixels)
INT_PUTFUNC_DEF (layout, minpixels)
INT_PUTFUNC_DEF (movementcontrol, minpixels)
INT_GETFUNC_DEF (note, minpixels)
INT_GETFUNC_DEF (chord, minpixels)
INT_GETFUNC_DEF (standalone, minpixels)
INT_GETFUNC_DEF (staff, minpixels) INT_GETFUNC_DEF (voice, minpixels) INT_GETFUNC_DEF (score, minpixels) INT_GETFUNC_DEF (clef, minpixels) INT_GETFUNC_DEF (timesig, minpixels) INT_GETFUNC_DEF (tuplet, minpixels) INT_GETFUNC_DEF (stemdirective, minpixels) INT_GETFUNC_DEF (keysig, minpixels) INT_GETFUNC_DEF (scoreheader, minpixels) INT_GETFUNC_DEF (header, minpixels) INT_GETFUNC_DEF (paper, minpixels) INT_GETFUNC_DEF (layout, minpixels) INT_GETFUNC_DEF (movementcontrol, minpixels)
  //end block to ocpy for new int field in directive
  INT_PUTFUNC_DEF (note, override)
INT_PUTFUNC_DEF (chord, override)
INT_PUTFUNC_DEF (standalone, override)
INT_PUTFUNC_DEF (staff, override)
INT_PUTFUNC_DEF (voice, override)
INT_PUTFUNC_DEF (score, override)
INT_GETFUNC_DEF (note, override)
INT_GETFUNC_DEF (chord, override)
INT_GETFUNC_DEF (standalone, override)
INT_GETFUNC_DEF (staff, override)
INT_GETFUNC_DEF (voice, override)
INT_GETFUNC_DEF (score, override)
INT_PUTFUNC_DEF (note, y)
INT_PUTFUNC_DEF (chord, y)
INT_PUTFUNC_DEF (standalone, y)
INT_GETFUNC_DEF (note, y)
INT_GETFUNC_DEF (chord, y)
INT_GETFUNC_DEF (standalone, y)
INT_PUTFUNC_DEF (note, x)
INT_PUTFUNC_DEF (chord, x)
INT_PUTFUNC_DEF (standalone, x)
INT_GETFUNC_DEF (note, x)
INT_GETFUNC_DEF (chord, x)
INT_GETFUNC_DEF (standalone, x)
INT_PUTFUNC_DEF (note, ty)
INT_PUTFUNC_DEF (chord, ty)
INT_PUTFUNC_DEF (standalone, ty)
INT_GETFUNC_DEF (note, ty)
INT_GETFUNC_DEF (chord, ty)
INT_GETFUNC_DEF (standalone, ty)
INT_PUTFUNC_DEF (note, tx)
INT_PUTFUNC_DEF (chord, tx)
INT_PUTFUNC_DEF (standalone, tx)
INT_GETFUNC_DEF (note, tx)
INT_GETFUNC_DEF (chord, tx)
INT_GETFUNC_DEF (standalone, tx)
INT_PUTFUNC_DEF (note, gy)
INT_PUTFUNC_DEF (chord, gy)
INT_PUTFUNC_DEF (standalone, gy)
INT_GETFUNC_DEF (note, gy)
INT_GETFUNC_DEF (chord, gy)
INT_GETFUNC_DEF (standalone, gy)
INT_PUTFUNC_DEF (note, gx)
INT_PUTFUNC_DEF (chord, gx)
INT_PUTFUNC_DEF (standalone, gx)
INT_GETFUNC_DEF (note, gx)
INT_GETFUNC_DEF (chord, gx)
INT_GETFUNC_DEF (standalone, gx)
INT_GETFUNC_DEF (note, width)
INT_GETFUNC_DEF (chord, width)
INT_GETFUNC_DEF (standalone, width)
INT_GETFUNC_DEF (note, height)
INT_GETFUNC_DEF (chord, height)
INT_GETFUNC_DEF (standalone, height)
INT_GETFUNC_DEF (score, x) INT_GETFUNC_DEF (score, y) INT_GETFUNC_DEF (score, tx) INT_GETFUNC_DEF (score, ty) INT_GETFUNC_DEF (score, gx) INT_GETFUNC_DEF (score, gy) INT_GETFUNC_DEF (score, width) INT_GETFUNC_DEF (score, height) INT_PUTFUNC_DEF (score, x) INT_PUTFUNC_DEF (score, y) INT_PUTFUNC_DEF (score, tx) INT_PUTFUNC_DEF (score, ty) INT_PUTFUNC_DEF (score, gx) INT_PUTFUNC_DEF (score, gy) INT_GETFUNC_DEF (object, minpixels) INT_PUTFUNC_DEF (object, minpixels) DELETE_FN_DEF (object)
  // block to copy for new type of directive, !!minpixels is done in block to copy for new fields!!
  GETFUNC_DEF (clef, prefix) GETFUNC_DEF (clef, postfix) GETFUNC_DEF (clef, display) PUTFUNC_DEF (clef, prefix) PUTFUNC_DEF (clef, postfix) PUTFUNC_DEF (clef, display) PUTGRAPHICFUNC_DEF (clef);

INT_PUTFUNC_DEF (clef, x) INT_PUTFUNC_DEF (clef, y) INT_PUTFUNC_DEF (clef, tx) INT_PUTFUNC_DEF (clef, ty) INT_PUTFUNC_DEF (clef, gx) INT_PUTFUNC_DEF (clef, gy) INT_PUTFUNC_DEF (clef, override) INT_GETFUNC_DEF (clef, x) INT_GETFUNC_DEF (clef, y) INT_GETFUNC_DEF (clef, tx) INT_GETFUNC_DEF (clef, ty) INT_GETFUNC_DEF (clef, gx) INT_GETFUNC_DEF (clef, gy) INT_GETFUNC_DEF (clef, override) INT_GETFUNC_DEF (clef, width) INT_GETFUNC_DEF (clef, height) EDIT_DELETE_FN_DEF (clef)
  // end block
  GETFUNC_DEF (timesig, prefix) GETFUNC_DEF (timesig, postfix) GETFUNC_DEF (timesig, display) PUTFUNC_DEF (timesig, prefix) PUTFUNC_DEF (timesig, postfix) PUTFUNC_DEF (timesig, display) PUTGRAPHICFUNC_DEF (timesig);

INT_PUTFUNC_DEF (timesig, x)
INT_PUTFUNC_DEF (timesig, y)
INT_PUTFUNC_DEF (timesig, tx)
INT_PUTFUNC_DEF (timesig, ty)
INT_PUTFUNC_DEF (timesig, gx)
INT_PUTFUNC_DEF (timesig, gy)
INT_PUTFUNC_DEF (timesig, override)
INT_GETFUNC_DEF (timesig, x)
INT_GETFUNC_DEF (timesig, y) INT_GETFUNC_DEF (timesig, tx) INT_GETFUNC_DEF (timesig, ty) INT_GETFUNC_DEF (timesig, gx) INT_GETFUNC_DEF (timesig, gy) INT_GETFUNC_DEF (timesig, override) INT_GETFUNC_DEF (timesig, width) INT_GETFUNC_DEF (timesig, height) EDIT_DELETE_FN_DEF (timesig) GETFUNC_DEF (tuplet, prefix) GETFUNC_DEF (tuplet, postfix) GETFUNC_DEF (tuplet, display) PUTFUNC_DEF (tuplet, prefix) PUTFUNC_DEF (tuplet, postfix) PUTFUNC_DEF (tuplet, display) PUTGRAPHICFUNC_DEF (tuplet);

INT_PUTFUNC_DEF (tuplet, x)
INT_PUTFUNC_DEF (tuplet, y)
INT_PUTFUNC_DEF (tuplet, tx)
INT_PUTFUNC_DEF (tuplet, ty)
INT_PUTFUNC_DEF (tuplet, gx)
INT_PUTFUNC_DEF (tuplet, gy)
INT_PUTFUNC_DEF (tuplet, override)
INT_GETFUNC_DEF (tuplet, x)
INT_GETFUNC_DEF (tuplet, y)
INT_GETFUNC_DEF (tuplet, tx) INT_GETFUNC_DEF (tuplet, ty) INT_GETFUNC_DEF (tuplet, gx) INT_GETFUNC_DEF (tuplet, gy) INT_GETFUNC_DEF (tuplet, override) INT_GETFUNC_DEF (tuplet, width) INT_GETFUNC_DEF (tuplet, height) EDIT_DELETE_FN_DEF (tuplet) GETFUNC_DEF (stemdirective, prefix) GETFUNC_DEF (stemdirective, postfix) GETFUNC_DEF (stemdirective, display) PUTFUNC_DEF (stemdirective, prefix) PUTFUNC_DEF (stemdirective, postfix) PUTFUNC_DEF (stemdirective, display) PUTGRAPHICFUNC_DEF (stemdirective);

INT_PUTFUNC_DEF (stemdirective, x)
INT_PUTFUNC_DEF (stemdirective, y)
INT_PUTFUNC_DEF (stemdirective, tx)
INT_PUTFUNC_DEF (stemdirective, ty)
INT_PUTFUNC_DEF (stemdirective, gx)
INT_PUTFUNC_DEF (stemdirective, gy)
INT_PUTFUNC_DEF (stemdirective, override)
INT_GETFUNC_DEF (stemdirective, x)
INT_GETFUNC_DEF (stemdirective, y)
INT_GETFUNC_DEF (stemdirective, tx)
INT_GETFUNC_DEF (stemdirective, ty) INT_GETFUNC_DEF (stemdirective, gx) INT_GETFUNC_DEF (stemdirective, gy) INT_GETFUNC_DEF (stemdirective, override) INT_GETFUNC_DEF (stemdirective, width) INT_GETFUNC_DEF (stemdirective, height) EDIT_DELETE_FN_DEF (stemdirective) GETFUNC_DEF (keysig, prefix) GETFUNC_DEF (keysig, postfix) GETFUNC_DEF (keysig, display) PUTFUNC_DEF (keysig, prefix) PUTFUNC_DEF (keysig, postfix) PUTFUNC_DEF (keysig, display) PUTGRAPHICFUNC_DEF (keysig);

INT_PUTFUNC_DEF (keysig, x)
INT_PUTFUNC_DEF (keysig, y)
INT_PUTFUNC_DEF (keysig, tx)
INT_PUTFUNC_DEF (keysig, ty)
INT_PUTFUNC_DEF (keysig, gx)
INT_PUTFUNC_DEF (keysig, gy)
INT_PUTFUNC_DEF (keysig, override)
INT_GETFUNC_DEF (keysig, x)
INT_GETFUNC_DEF (keysig, y)
INT_GETFUNC_DEF (keysig, tx) INT_GETFUNC_DEF (keysig, ty) INT_GETFUNC_DEF (keysig, gx) INT_GETFUNC_DEF (keysig, gy) INT_GETFUNC_DEF (keysig, override) INT_GETFUNC_DEF (keysig, width) INT_GETFUNC_DEF (keysig, height) EDIT_DELETE_FN_DEF (keysig) GETFUNC_DEF (scoreheader, prefix) GETFUNC_DEF (scoreheader, postfix) GETFUNC_DEF (scoreheader, display) PUTFUNC_DEF (scoreheader, prefix) PUTFUNC_DEF (scoreheader, postfix) PUTFUNC_DEF (scoreheader, display) PUTGRAPHICFUNC_DEF (scoreheader);

INT_PUTFUNC_DEF (scoreheader, x)
INT_PUTFUNC_DEF (scoreheader, y)
INT_PUTFUNC_DEF (scoreheader, tx)
INT_PUTFUNC_DEF (scoreheader, ty)
INT_PUTFUNC_DEF (scoreheader, gx)
INT_PUTFUNC_DEF (scoreheader, gy)
INT_PUTFUNC_DEF (scoreheader, override)
INT_GETFUNC_DEF (scoreheader, x)
INT_GETFUNC_DEF (scoreheader, y)
INT_GETFUNC_DEF (scoreheader, tx) INT_GETFUNC_DEF (scoreheader, ty) INT_GETFUNC_DEF (scoreheader, gx) INT_GETFUNC_DEF (scoreheader, gy) INT_GETFUNC_DEF (scoreheader, override) INT_GETFUNC_DEF (scoreheader, width) INT_GETFUNC_DEF (scoreheader, height) EDIT_DELETE_FN_DEF (scoreheader) GETFUNC_DEF (header, prefix) GETFUNC_DEF (header, postfix) GETFUNC_DEF (header, display) PUTFUNC_DEF (header, prefix) PUTFUNC_DEF (header, postfix) PUTFUNC_DEF (header, display) PUTGRAPHICFUNC_DEF (header);

INT_PUTFUNC_DEF (header, x)
INT_PUTFUNC_DEF (header, y)
INT_PUTFUNC_DEF (header, tx)
INT_PUTFUNC_DEF (header, ty)
INT_PUTFUNC_DEF (header, gx)
INT_PUTFUNC_DEF (header, gy)
INT_PUTFUNC_DEF (header, override)
INT_GETFUNC_DEF (header, x) INT_GETFUNC_DEF (header, y) INT_GETFUNC_DEF (header, tx) INT_GETFUNC_DEF (header, ty) INT_GETFUNC_DEF (header, gx) INT_GETFUNC_DEF (header, gy) INT_GETFUNC_DEF (header, override) INT_GETFUNC_DEF (header, width) INT_GETFUNC_DEF (header, height) EDIT_DELETE_FN_DEF (header) GETFUNC_DEF (paper, prefix) GETFUNC_DEF (paper, postfix) GETFUNC_DEF (paper, display) PUTFUNC_DEF (paper, prefix) PUTFUNC_DEF (paper, postfix) PUTFUNC_DEF (paper, display) PUTGRAPHICFUNC_DEF (paper);

INT_PUTFUNC_DEF (paper, x)
INT_PUTFUNC_DEF (paper, y)
INT_PUTFUNC_DEF (paper, tx)
INT_PUTFUNC_DEF (paper, ty)
INT_PUTFUNC_DEF (paper, gx)
INT_PUTFUNC_DEF (paper, gy)
INT_PUTFUNC_DEF (paper, override)
INT_GETFUNC_DEF (paper, x) INT_GETFUNC_DEF (paper, y) INT_GETFUNC_DEF (paper, tx) INT_GETFUNC_DEF (paper, ty) INT_GETFUNC_DEF (paper, gx) INT_GETFUNC_DEF (paper, gy) INT_GETFUNC_DEF (paper, override) INT_GETFUNC_DEF (paper, width) INT_GETFUNC_DEF (paper, height) EDIT_DELETE_FN_DEF (paper) GETFUNC_DEF (layout, prefix) GETFUNC_DEF (layout, postfix) GETFUNC_DEF (layout, display) PUTFUNC_DEF (layout, prefix) PUTFUNC_DEF (layout, postfix) PUTFUNC_DEF (layout, display) PUTGRAPHICFUNC_DEF (layout);

INT_PUTFUNC_DEF (layout, x)
INT_PUTFUNC_DEF (layout, y)
INT_PUTFUNC_DEF (layout, tx)
INT_PUTFUNC_DEF (layout, ty)
INT_PUTFUNC_DEF (layout, gx)
INT_PUTFUNC_DEF (layout, gy)
INT_PUTFUNC_DEF (layout, override)
INT_GETFUNC_DEF (layout, x) INT_GETFUNC_DEF (layout, y) INT_GETFUNC_DEF (layout, tx) INT_GETFUNC_DEF (layout, ty) INT_GETFUNC_DEF (layout, gx) INT_GETFUNC_DEF (layout, gy) INT_GETFUNC_DEF (layout, override) INT_GETFUNC_DEF (layout, width) INT_GETFUNC_DEF (layout, height) EDIT_DELETE_FN_DEF (layout) GETFUNC_DEF (movementcontrol, prefix) GETFUNC_DEF (movementcontrol, postfix) PUTFUNC_DEF (movementcontrol, prefix) PUTFUNC_DEF (movementcontrol, postfix) PUTGRAPHICFUNC_DEF (movementcontrol);

INT_PUTFUNC_DEF (movementcontrol, x)
INT_PUTFUNC_DEF (movementcontrol, y)
INT_PUTFUNC_DEF (movementcontrol, tx)
INT_PUTFUNC_DEF (movementcontrol, ty)
INT_PUTFUNC_DEF (movementcontrol, gx)
INT_PUTFUNC_DEF (movementcontrol, gy) INT_PUTFUNC_DEF (movementcontrol, override) INT_GETFUNC_DEF (movementcontrol, x) INT_GETFUNC_DEF (movementcontrol, y) INT_GETFUNC_DEF (movementcontrol, tx) INT_GETFUNC_DEF (movementcontrol, ty) INT_GETFUNC_DEF (movementcontrol, gx) INT_GETFUNC_DEF (movementcontrol, gy) INT_GETFUNC_DEF (movementcontrol, override) INT_GETFUNC_DEF (movementcontrol, width) INT_GETFUNC_DEF (movementcontrol, height) EDIT_DELETE_FN_DEF (movementcontrol)
     static
       SCM
     scheme_put_text_clipboard (SCM optional)
{
  size_t length;
  char *str = NULL;
  if (scm_is_string (optional))
    {
      str = scm_to_locale_stringn (optional, &length);
      GtkClipboard *clipboard = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
      gtk_clipboard_set_text (clipboard, str, length);
      if (str)
        free (str);
      return SCM_BOOL (TRUE);
    }
  return SCM_BOOL (FALSE);
}



static SCM
scheme_get_username (void)
{
  return scm_from_locale_string (Denemo.prefs.username->str);
}

static SCM
scheme_get_password (void)
{
  return scm_from_locale_string (Denemo.prefs.password->str);
}

static SCM
scheme_set_midi_capture (SCM setting)
{
  gboolean prev;
  prev = set_midi_capture ((setting != SCM_BOOL_F));
  return prev ? SCM_BOOL_T : SCM_BOOL_F;
}

static SCM
scheme_get_keyboard_state (void)
{
  return scm_from_int (Denemo.keyboard_state);
}

static SCM
scheme_set_midi_thru (SCM set)
{
  SCM ret = scm_from_int (Denemo.keyboard_state);
  if (scm_is_true (set))
    Denemo.keyboard_state = GDK_SHIFT_MASK;
  else
    Denemo.keyboard_state = 0;
  set_midi_in_status ();
  return ret;
}

static SCM
scheme_get_recorded_midi_on_tick (void)
{
  smf_track_t *track = Denemo.gui->si->recorded_midi_track;
  if (track)
    {
#define MIDI_NOTEOFF		0x80
#define MIDI_NOTEON		0x90
      smf_event_t *event = smf_track_get_next_event (track);
      if (event)
        switch (event->midi_buffer[0] & 0xF0)
          {
          case MIDI_NOTEON:
            return scm_from_int (event->time_pulses);
          case MIDI_NOTEOFF:
            return scm_from_int (-event->time_pulses);
          default:
            return SCM_BOOL_F;
          }
    }
  return SCM_BOOL_F;
}

static SCM
scheme_get_recorded_midi_note (void)
{
  smf_track_t *track = Denemo.gui->si->recorded_midi_track;
  if (track)
    {
      smf_event_t *event = NULL;
      if (track->next_event_number > 0 && (track->next_event_number <= track->events_array->len))
        event = g_ptr_array_index (track->events_array, track->next_event_number - 1);
      if (event)
        switch (event->midi_buffer[0] & 0xF0)
          {
          case MIDI_NOTEON:
          case MIDI_NOTEOFF:
            return scm_from_int (event->midi_buffer[1]);
          default:
            return SCM_BOOL_F;
          }
    }
  return SCM_BOOL_F;
}

static SCM
scheme_rewind_recorded_midi (void)
{
  smf_track_t *track = Denemo.gui->si->recorded_midi_track;
  if (track)
    {
      if (track->smf == NULL)
        {
          if (Denemo.gui->si->smf)
            {
              smf_add_track (Denemo.gui->si->smf, track);
              smf_rewind (Denemo.gui->si->smf);
            }
          else
            return SCM_BOOL_F;
        }
      smf_rewind (track->smf);
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

static SCM
scheme_get_note_for_midi_key (SCM scm)
{
  gint notenum = 0, offset, enshift, octave;
  if (scm_is_integer (scm))
    notenum = scm_to_int (scm);
  if (notenum > 0 && notenum < 256)
    {
      notenum2enharmonic (notenum, &offset, &enshift, &octave);
      gchar *name = mid_c_offsettolily (offset + 7 * octave, enshift);
      return scm_from_locale_string (name);
    }
  return SCM_BOOL_F;
}

static SCM
scheme_get_midi (void)
{
  gint midi;
  gboolean success = intercept_midi_event (&midi);
  if (!success)
    midi = 0;                   /* scripts should detect this impossible value and take action */
  else
    Denemo.gui->last_source = INPUTMIDI;
  gchar *buf = (gchar *) & midi;
  *buf &= 0xF0;                 //do not return channel info

  SCM scm = scm_from_int (midi);
  return scm;
}

//Simulates a midi event, with no capture by any calling scheme script unless midi==0
static SCM
scheme_put_midi (SCM scm)
{
  gchar buf[3];
  gint midi = scm_to_int (scm);

  buf[0] = midi & 0xFF;
  buf[1] = (midi >> 8) & 0xFF;
  buf[2] = (midi >> 16) & 0xFF;
  //g_print("got %x\nbreaks as %x %x %x\n", midi&0xFFFFFF, buf[0], buf[1], buf[2]);
  if (midi)
    {
      gboolean capture = set_midi_capture (FALSE);      //Turn off any capturing
      process_midi_event (buf);
      set_midi_capture (capture);       //Restore any capturing that might be on
    }
  else
    process_midi_event (buf);
  return SCM_BOOL (TRUE);
}

static SCM
scheme_output_midi (SCM scm)
{
  gchar buf[3];
  gint midi = scm_to_int (scm);

  buf[0] = midi & 0xFF;
  buf[1] = (midi >> 8) & 0xFF;
  buf[2] = (midi >> 16) & 0xFF;
  play_adjusted_midi_event (buf);
  return SCM_BOOL_T;
}


/* outputs a midibytes string to MIDI out. Format of midibytes as in DenemoDirective->midibytes */
SCM
scheme_output_midi_bytes (SCM input)
{
  char *next;
  gint i, numbytes;
  gint channel;
  gint volume;
  if (!scm_is_string (input))
    {
      return SCM_BOOL_F;
    }
  DenemoStaff *curstaffstruct = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  channel = get_midi_channel (curstaffstruct);
  volume = curstaffstruct->volume;
  char *string_input;
  string_input = scm_to_locale_string (input);
  gchar *bytes = substitute_midi_values (string_input, channel, volume);

  for (i = 0, next = bytes; *next; next++)
    {
      i++;
      if (*next == 0)
        break;
    }
  numbytes = i;
  unsigned char *buffer = (unsigned char *) g_malloc0 (numbytes);
  for (i = 0, next = bytes; i < numbytes; i++, next++)
    buffer[i] = (unsigned char) strtol (next, &next, 0);
  g_free (bytes);
  //g_print("\nbuffer[0] = %x buffer[1] = %x buffer[2] = %x\n", buffer[0], buffer[1], buffer[2]);

  play_midi_event (DEFAULT_BACKEND, curstaffstruct->midi_port, buffer);

  if (string_input)
    free (string_input);
  return SCM_BOOL (TRUE);
}

static SCM
scheme_create_timebase (SCM optional)
{
  DenemoScore *si = Denemo.gui->si;
  if (si->smfsync != si->changecount)
    {
      exportmidi (NULL, si, 0, 0);
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

static SCM
scheme_pending_midi (SCM scm)
{
  if (scm_is_integer (scm))
    {
      guint key = scm_to_int (scm);
      g_queue_push_head (Denemo.gui->pending_midi, GINT_TO_POINTER (key));
      return SCM_BOOL_T;
    }
  else
    return SCM_BOOL_F;
}

static SCM
scheme_play_midi_note (SCM note, SCM volume, SCM channel, SCM duration)
{
  guint vol = scm_to_int (volume);
  gint key = scm_to_int (note);
  gint chan = scm_to_int (channel);
  gint dur = scm_to_int (duration);

  //g_print("Playing %x at %f volume, %d channel for %dms\n", key, vol/255.0, channel, dur);
  play_note (DEFAULT_BACKEND, 0 /*port */ , chan, key, dur, vol);
  return SCM_BOOL (TRUE);
}

static SCM
scheme_play_midikey (SCM scm)
{
  guint midi = scm_to_int (scm);
  gint key = (midi >> 8) & 0xFF;
  gint channel = midi & 0xF;
  gint volume = ((midi >> 16) & 0x7F);
  //g_print("Playing %x at %f volume, %d channel\n", key, (double)volume, channel);
  play_note (DEFAULT_BACKEND, 0 /*port */ , channel, key, 1000 /*duration */ , volume);
  //g_usleep(200000);
  return SCM_BOOL (TRUE);
}

//Insert a rest without setting the prevailing duration
SCM
scheme_put_rest (SCM optional_duration)
{
  gint duration;
  if (scm_is_integer (optional_duration))
    {
      duration = scm_to_int (optional_duration);
    }
  else
    {
      duration = get_prevailing_duration ();
    }
  if ((duration < 0) || (duration > 7))
    return SCM_BOOL_F;

  dnm_insertchord (Denemo.gui, duration, 0, TRUE);
  displayhelper (Denemo.gui);   //without this a call to d-AddVoice causes a crash as the chord length info has not been updated
  return SCM_BOOL_T;
}

//Insert a rest in the given (or prevailing duration) and set the prevailing duration
static SCM
scheme_insert_rest (SCM optional)
{
  SCM ret = scheme_put_rest (optional);
  if (scm_is_integer (optional))
    {
      gint duration = scm_to_int (optional);
      highlight_duration (Denemo.gui, duration);
    }
  return ret;
}


static SCM
scheme_toggle_playalong (void)
{
  pb_playalong (midiplayalongbutton);
  return SCM_BOOL (Denemo.gui->midi_destination | MIDIPLAYALONG);
}

static SCM
scheme_toggle_conduct (void)
{
  pb_conduct (midiconductbutton);
  return SCM_BOOL (Denemo.gui->midi_destination | MIDICONDUCT);
}

static SCM
scheme_midi_record (void)
{
  pb_record (midirecordbutton);
  return SCM_BOOL (Denemo.gui->midi_destination | MIDIRECORD);
}

typedef struct cb_scheme_and_id
{
  char *scheme_code;
  gint id;
} cb_scheme_and_id;

static gboolean
scheme_callback_one_shot_timer (cb_scheme_and_id * scheme)
{
  char *scheme_code = scheme->scheme_code;
  if (scheme->id == Denemo.gui->id)
    call_out_to_guile (scheme_code);
  else
    g_warning ("Timer missed for gui %d\n", scheme->id);
  g_free (scheme);
  free (scheme_code);
  return FALSE;
}

static SCM
scheme_one_shot_timer (SCM duration_amount, SCM callback)
{
  char *scheme_code;
  scheme_code = scm_to_locale_string (callback);
  gint duration = scm_to_int (duration_amount);
  cb_scheme_and_id *scheme = g_malloc (sizeof (cb_scheme_and_id));
  scheme->scheme_code = scheme_code;
  scheme->id = Denemo.gui->id;
  g_timeout_add (duration, (GSourceFunc) scheme_callback_one_shot_timer, GINT_TO_POINTER (scheme));
  return SCM_BOOL (TRUE);
}

static gboolean
scheme_callback_timer (cb_scheme_and_id * scheme)
{
  char *scheme_code = scheme->scheme_code;
  if (scheme->id == Denemo.gui->id)
    call_out_to_guile (scheme_code);
  else
    g_warning ("Timer missed for gui %d\n", scheme->id);

  return TRUE;                  //continue to call
}


static SCM
scheme_timer (SCM duration_amount, SCM callback)
{
  char *scheme_code;
  if (scm_is_string (callback))
    {
      scheme_code = scm_to_locale_string (callback);    //FIXME check that type of callback is tring
      gint duration = scm_to_int (duration_amount);
      //g_print("setting timer for %s after %d ms", scheme_code, duration);
      cb_scheme_and_id *scheme = g_malloc (sizeof (cb_scheme_and_id));
      scheme->scheme_code = scheme_code;
      scheme->id = Denemo.gui->id;
      g_timeout_add (duration, (GSourceFunc) scheme_callback_timer, GINT_TO_POINTER (scheme));
      //if(scheme_code) free(scheme_code);
      return scm_from_int (GPOINTER_TO_INT (scheme));   //FIXME pointer may not fit in int
    }
  else
    return SCM_BOOL_F;
}

static SCM
scheme_kill_timer (SCM id)
{
  if (scm_is_integer (id))
    {
      //FIXME the int may not be large enough for a pointer
      cb_scheme_and_id *scheme = GINT_TO_POINTER (scm_to_int (id));
      if (scheme)
        {
          g_source_remove_by_user_data (scheme);
          free (scheme->scheme_code);
          g_free (scheme);
          return SCM_BOOL_T;
        }
    }
  return SCM_BOOL_F;
}




static SCM
scheme_bass_figure (SCM bass, SCM harmony)
{
  SCM ret = SCM_BOOL_F;
  gboolean status = FALSE;
  gint bassnum = scm_to_int (bass);
  gint harmonynum = scm_to_int (harmony);
  gchar *interval = determine_interval (bassnum, harmonynum, &status);
  if (interval)
    {
      ret = scm_cons (status ? SCM_BOOL_T : SCM_BOOL_F, scm_from_locale_string (interval));
      g_free (interval);
    }
  return ret;
}

static SCM
scheme_has_figures (SCM optional)
{
  return SCM_BOOL (((DenemoStaff *) Denemo.gui->si->currentstaff->data)->hasfigures);
}



//badly named:
static SCM
scheme_put_note_name (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
    return SCM_BOOL (FALSE);
  else
    {
      char *str = NULL;
      if (scm_is_string (optional))
        {
          str = scm_to_locale_string (optional);
          gint mid_c_offset;
          gint enshift;
          interpret_lilypond_notename (str, &mid_c_offset, &enshift);
          //g_print("note %s gives %d and %d\n", str, mid_c_offset, enshift);
          modify_note (thechord, mid_c_offset, enshift, find_prevailing_clef (Denemo.gui->si));
          if (str)
            free (str);
          return SCM_BOOL (TRUE);
        }
    }
  return SCM_BOOL (FALSE);
}

static SCM
scheme_set_accidental (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
    return SCM_BOOL (FALSE);
  else
    {
      GList *g;
      for (g = thechord->notes; g; g = g->next)
        {
          thenote = (note *) g->data;
          if (thenote->mid_c_offset == Denemo.gui->si->cursor_y)
            break;
        }
      if (g == NULL)
        return SCM_BOOL_F;
      DenemoScore *si = Denemo.gui->si;
      char *str = NULL;

      if (scm_is_string (optional))
        {
          str = scm_to_locale_string (optional);
          thenote->enshift = lilypond_to_enshift (str);
        }
      else if (scm_is_integer (optional))
        thenote->enshift = scm_to_int (optional);
      else
        thenote->enshift = 0;
      if ((thenote->enshift < -2) || (thenote->enshift > 2))
        thenote->enshift = 0;
      showwhichaccidentals ((objnode *) si->currentmeasure->data, si->curmeasurekey, si->curmeasureaccs);
      //  find_xes_in_measure (si, si->currentmeasurenum, si->cursortime1,
      //                      si->cursortime2); causes a crash, si is not passed correctly, why???
      //thenote->mid_c_offset = interpret_lilypond_notename(str);
      displayhelper (Denemo.gui);
      if (str)
        free (str);
      return SCM_BOOL (TRUE);
    }
}





//create a putnote here that takes a duration and numdots and note name, inserts a chord and calls the scheme_put_note_name above - this can be done via script at present, e.g. (d-C) (d-Change3) (d-AddDot) (d-PutNoteName "eis''")


//Puts a note into the chord at the cursor PARAM lily is a string representation of the note
static SCM
scheme_insert_note_in_chord (SCM lily)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoObject *curObj;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) )
    return SCM_BOOL_F;
#ifdef INSERT_NOTE_IN_CHORD_WORKS_ON_PREVIOUS_CHORD
  if(curObj->type != CHORD) {
    objnode *theobj = Denemo.gui->si->currentobject;
    while(theobj->prev)
      {
        theobj = theobj->prev;
        curObj = theobj->data;
        if(curObj->type ==CHORD)
          break;
      }
    if(curObj->type !=CHORD)
      return SCM_BOOL_F;
  }
#else
   if(curObj->type !=CHORD)
      return SCM_BOOL_F;
#endif
      
  char *str = NULL;
  if (scm_is_string (lily))
    {
      str = scm_to_locale_string (lily);
      gint mid_c_offset;
      gint enshift;
      interpret_lilypond_notename (str, &mid_c_offset, &enshift);

      //g_print("note %s gives %d and %d\n", str, mid_c_offset, enshift);
      addtone (curObj, mid_c_offset, enshift, find_prevailing_clef (Denemo.gui->si));
      score_status (gui, TRUE);
      displayhelper (Denemo.gui);
      if (str)
        free (str);
      return SCM_BOOL_T;
    }
  return SCM_BOOL (FALSE);
}


//return the number of objects in the copybuffer at staff m
static SCM
scheme_get_clip_objects (SCM m)
{
  gint staff = scm_to_int (m);
  gint num = get_clip_objs (staff);
  if (num == -1)
    return SCM_BOOL_F;
  else
    return scm_from_int (num);
}

//return the type of the nth object in the copybuffer
static SCM
scheme_get_clip_obj_type (SCM m, SCM n)
{
  gint value = scm_to_int (n);
  gint staff = scm_to_int (m);
  DenemoObjType type = get_clip_obj_type (staff, value);
  if (type == -1)
    return SCM_BOOL_F;
  else
    return scm_from_int (type);
}


//insert the nth object from the denemo copybuffer
static SCM
scheme_put_clip_obj (SCM m, SCM n)
{
  gint value = scm_to_int (n);
  gint staff = scm_to_int (m);
  return SCM_BOOL (insert_clip_obj (staff, value));
}

static SCM
scheme_adjust_xes (SCM optional)
{
  find_xes_in_all_measures (Denemo.gui->si);
  return SCM_BOOL_T;
}

static gint
flash_cursor (void)
{
  gtk_widget_queue_draw (Denemo.scorearea);
  draw_score (NULL);
  return TRUE;
}

static SCM
scheme_highlight_cursor (SCM optional)
{
  static gint id;
  Denemo.prefs.cursor_highlight = !Denemo.prefs.cursor_highlight;
  if (id)
    {
      g_source_remove (id);
      id = 0;
    }
  else if (Denemo.prefs.cursor_highlight)
    id = g_timeout_add (500, (GSourceFunc) flash_cursor, NULL);
  //g_print("Cursor highlighting %d id %d", Denemo.prefs.cursor_highlight, id);
  return SCM_BOOL_T;
}

static SCM
scheme_get_type (SCM optional)
{
  DenemoObject *curObj;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || !(DENEMO_OBJECT_TYPE_NAME (curObj)))
    return scm_from_locale_string ("None");
  if (Denemo.gui->si->cursor_appending)
    return scm_from_locale_string ("Appending");
  return scm_from_locale_string (DENEMO_OBJECT_TYPE_NAME (curObj));
}

static SCM
scheme_get_lilypond (SCM optional)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoObject *curObj;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || !(DENEMO_OBJECT_TYPE_NAME (curObj)))
    return SCM_BOOL_F;
//g_print("Before %d %d\n", gui->lilysync, gui->changecount);

  if (gui->lilysync != gui->changecount)
    refresh_lily_cb (NULL, Denemo.gui);
//g_print("After %d %d\n", gui->lilysync, gui->changecount);
  if (curObj->lilypond)
    return scm_from_locale_string (curObj->lilypond);
  return SCM_BOOL_F;
}

static SCM
scheme_get_tuplet (SCM optional)
{
  DenemoObject *curObj;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != TUPOPEN))
    return SCM_BOOL_F;
  GString *ratio = g_string_new ("");
  g_string_printf (ratio, "%d/%d", ((tupopen *) curObj->object)->numerator, ((tupopen *) curObj->object)->denominator);
  return scm_from_locale_string (g_string_free (ratio, FALSE));
}

static SCM
scheme_set_tuplet (SCM ratio)
{
  DenemoObject *curObj;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != TUPOPEN))
    {
      return SCM_BOOL_F;
    }
  char *theratio;
  theratio = scm_to_locale_string (ratio);
  sscanf (theratio, "%d/%d", &((tupopen *) curObj->object)->numerator, &((tupopen *) curObj->object)->denominator);
  //g_print("Set %d/%d\n", (((tupopen*)curObj->object)->numerator), (((tupopen*)curObj->object)->denominator));
  free (theratio);
  if (((tupopen *) curObj->object)->denominator)
    {
      return SCM_BOOL_T;
    }
  ((tupopen *) curObj->object)->denominator = 1;
  return SCM_BOOL_F;
}

static SCM
scheme_set_background (SCM color)
{
  if (scm_is_integer (color))
    {
      gint value = scm_to_int (color);
      Denemo.color = value;
      gtk_widget_queue_draw (Denemo.scorearea);
      draw_score (NULL);
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}


static SCM
scheme_get_nonprinting (SCM optional)
{
  DenemoObject *curObj;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || curObj->isinvisible)
    return SCM_BOOL_T;
  return SCM_BOOL_F;
}

static SCM
scheme_set_nonprinting (SCM optional)
{
  DenemoObject *curObj;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data))
    return SCM_BOOL_F;
  if (scm_is_false (optional))
    curObj->isinvisible = FALSE;
  else
    curObj->isinvisible = TRUE;
  return SCM_BOOL_T;
}

static SCM
scheme_is_grace (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->is_grace))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

static SCM
scheme_is_tied (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->is_tied))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}


static SCM
scheme_is_slur_start (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->slur_begin_p))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

static SCM
scheme_is_slur_end (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->slur_end_p))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

static SCM
scheme_is_cresc_start (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->crescendo_begin_p))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

static SCM
scheme_is_cresc_end (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->crescendo_end_p))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

static SCM
scheme_is_dim_start (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->diminuendo_begin_p))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

static SCM
scheme_is_dim_end (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->diminuendo_end_p))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

SCM
scheme_is_in_selection (void)
{
  return SCM_BOOL (in_selection (Denemo.gui->si));
}

SCM
scheme_is_appending (void)
{
  return SCM_BOOL (Denemo.gui->si->cursor_appending);
}




static SCM
scheme_clear_clipboard (SCM optional)
{
  clearbuffer ();
  return SCM_BOOL (TRUE);
}

static SCM
scheme_get_staffs_in_clipboard (SCM optional)
{
  gint num = get_staffs_in_clipboard ();
  if (num)
    return scm_from_int (num);
  return SCM_BOOL_F;
}


static SCM
scheme_get_measures_in_staff (SCM optional)
{
  gint num = g_list_length (((DenemoStaff *) Denemo.gui->si->currentstaff->data)->measures);
  return scm_from_int (num);
}

static SCM
scheme_staff_to_voice (SCM optional)
{
  SCM ret = SCM_BOOL_F;
  if (Denemo.gui->si->currentstaff->prev && (((DenemoStaff *) Denemo.gui->si->currentstaff->data)->voicecontrol == DENEMO_PRIMARY))
    {
      ((DenemoStaff *) Denemo.gui->si->currentstaff->data)->voicecontrol |= DENEMO_SECONDARY;
      setcurrentprimarystaff (Denemo.gui->si);
      ret = SCM_BOOL_T;
      gtk_widget_queue_draw (Denemo.scorearea);
      score_status (Denemo.gui, TRUE);
      draw_score (NULL);
    }
  return ret;
}

static SCM
scheme_voice_to_staff (SCM optional)
{
  SCM ret = SCM_BOOL_F;
  if (((DenemoStaff *) Denemo.gui->si->currentstaff->data)->voicecontrol & DENEMO_SECONDARY)
    {
      ((DenemoStaff *) Denemo.gui->si->currentstaff->data)->voicecontrol = DENEMO_PRIMARY;
      setcurrentprimarystaff (Denemo.gui->si);
      ret = SCM_BOOL_T;
      score_status (Denemo.gui, TRUE);
      gtk_widget_queue_draw (Denemo.scorearea);
    }
  return ret;
}

static SCM
scheme_is_voice (void)
{
return SCM_BOOL ((((DenemoStaff *) Denemo.gui->si->currentstaff->data)->voicecontrol & DENEMO_SECONDARY));
}
/* shifts the note at the cursor by the number of diatonic steps passed in */
SCM
scheme_diatonic_shift (SCM optional)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if (!Denemo.gui || !(Denemo.gui->si) || !(Denemo.gui->si->currentobject) || !(curObj = Denemo.gui->si->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
    {
      return SCM_BOOL (FALSE);
    }
  else
    {
      char *str = NULL;
      if (scm_is_string (optional))
        {
          str = scm_to_locale_string (optional);
          gint shift;
          sscanf (str, "%d", &shift);
//     g_print("note shift %s ie %d\n", str, shift);
          modify_note (thechord, thenote->mid_c_offset + shift, gui->si->curmeasureaccs[offsettonumber (thenote->mid_c_offset + shift)], find_prevailing_clef (Denemo.gui->si));
          free (str);
        }
    }
  return SCM_BOOL (FALSE);
}

/* moves currentobject to next object by calling cursorright.
   Steps over barlines (i.e. cursor_appending).
   returns TRUE if currentobject is different after than before doing the call
*/
SCM
scheme_next_object (void)
{
  return SCM_BOOL (cursor_to_next_object (FALSE, FALSE));
}

/* moves currentobject to prev object by calling cursorleft.
   Steps over barlines (i.e. cursor_appending).
   returns TRUE if currentobject is different after than before doing the call
*/
SCM
scheme_prev_object (void)
{
  return SCM_BOOL (cursor_to_prev_object (FALSE, FALSE));
}


/* moves currentobject to next object in measure, if any
   returns TRUE if currentobject is different after than before doing the call
*/
SCM
scheme_next_object_in_measure (void)
{
  return SCM_BOOL (cursor_to_next_object (TRUE, FALSE));
}

/* moves currentobject to previous object in measure, if any
   returns TRUE if currentobject is different after than before doing the call
*/
SCM
scheme_prev_object_in_measure (void)
{
  return SCM_BOOL (cursor_to_prev_object (TRUE, FALSE));
}


SCM
scheme_refresh_display (SCM optional)
{
  displayhelper (Denemo.gui);
  //done in displayhelper write_status(Denemo.gui);
  return SCM_BOOL (TRUE);
}



SCM
scheme_set_saved (SCM optional)
{
  //scm_is_bool(optional) &&
  if (scm_is_false (optional))
    score_status (Denemo.gui, TRUE);
  else
    score_status (Denemo.gui, FALSE);
  return SCM_BOOL (TRUE);
}

SCM
scheme_get_saved (SCM optional)
{
  return SCM_BOOL (!Denemo.gui->notsaved);
}

SCM
scheme_mark_status (SCM optional)
{
  return SCM_BOOL (mark_status ());

}







/* moves currentobject to next object in the selection.
   Steps over barlines (i.e. cursor_appending).
 returns TRUE if currentobject is different after than before the call
*/
SCM
scheme_next_selected_object (SCM optional)
{
  return SCM_BOOL (cursor_to_next_selected_object ());
}

/* moves currentobject to previous object in the selection.
   Steps over barlines (i.e. cursor_appending).
 returns TRUE if currentobject is different after than before the call
*/
SCM
scheme_prev_selected_object (SCM optional)
{
  return SCM_BOOL (cursor_to_prev_selected_object ());
}




SCM
scheme_next_standalone_directive (SCM optional)
{
  return SCM_BOOL (cursor_to_next_standalone_directive ());
}

SCM
scheme_prev_standalone_directive (SCM optional)
{
  return SCM_BOOL (cursor_to_prev_standalone_directive ());
}

SCM
scheme_next_standalone_directive_in_measure (SCM optional)
{
  return SCM_BOOL (cursor_to_next_standalone_in_measure ());
}

SCM
scheme_prev_standalone_directive_in_measure (SCM optional)
{
  return SCM_BOOL (cursor_to_prev_standalone_in_measure ());
}


SCM
scheme_next_chord (SCM optional)
{
  DenemoPosition pos;
  get_position (Denemo.gui->si, &pos);
  gboolean ret = cursor_to_next_chord ();
  if (!ret)
    goto_movement_staff_obj (NULL, -1, pos.staff, pos.measure, pos.object);
  return SCM_BOOL (ret);
}

SCM
scheme_prev_chord (SCM optional)
{
  DenemoPosition pos;
  get_position (Denemo.gui->si, &pos);
  gboolean ret = cursor_to_prev_chord ();
  if (!ret)
    goto_movement_staff_obj (NULL, -1, pos.staff, pos.measure, pos.object);
  return SCM_BOOL (ret);
}


SCM
scheme_next_chord_in_measure (SCM optional)
{
  return SCM_BOOL (cursor_to_next_chord_in_measure ());
}

SCM
scheme_prev_chord_in_measure (SCM optional)
{
  return SCM_BOOL (cursor_to_prev_chord_in_measure ());
}




SCM
scheme_next_note (SCM optional)
{
  return SCM_BOOL (cursor_to_next_note ());
}

SCM
scheme_prev_note (SCM optional)
{
  return SCM_BOOL (cursor_to_prev_note ());
}

static void
update_scheme_snippet_ids (void)
{
  DenemoGUI *gui = Denemo.gui;
  GList *g;
  gint i;
  for (g = gui->rhythms, i = 1; g; g = g->next, i++)
    {
      RhythmPattern *r = (RhythmPattern *) g->data;
      if (r->name)
        {
          gchar *command = g_strdup_printf ("(define Snippet::%s %d)", r->name, i);
          call_out_to_guile (command);
          g_free (command);
        }
    }
}

static SCM
scheme_create_snippet_from_object (SCM name)
{
  if (scm_is_string (name))
    {
      char *str;
      str = scm_to_locale_string (name);
      if (Denemo.gui->si->currentobject)
        {
          DenemoObject *clonedobj = dnm_clone_object (Denemo.gui->si->currentobject->data);
          RhythmPattern *r = (RhythmPattern *) g_malloc0 (sizeof (RhythmPattern));
          install_button_for_pattern (r, str);
          r->clipboard = g_list_append (NULL, g_list_append (NULL, clonedobj));
          append_rhythm (r, NULL);
          RhythmElement *relement = (RhythmElement *) g_malloc0 (sizeof (RhythmElement));
          //relement->icon = str; was wrong, must be NULL for a singleton.
          r->name = str;
          r->rsteps = g_list_append (NULL, relement);
          r->rsteps->prev = r->rsteps->next = r->rsteps;        //make list circular
          SCM ret = scm_from_int (insert_pattern_in_toolbar (r));
          update_scheme_snippet_ids ();
          if (str)
            free (str);
          return ret;
        }
      if (str)
        free (str);
    }
  return SCM_BOOL_F;
}

static SCM
scheme_select_snippet (SCM number)
{
  if (scm_is_integer (number))
    {
      gint position = scm_to_int (number);
      GList *g = g_list_nth (Denemo.gui->rhythms, position - 1);
      if (g)
        {
          RhythmPattern *r = g->data;
          if (r)
            {

              select_rhythm_pattern (r);

              return SCM_BOOL_T;
            }
        }
    }

  return SCM_BOOL_F;
}

static SCM
scheme_insert_snippet (SCM number)
{
  if (scm_is_integer (number))
    {
      gint position = scm_to_int (number);
      GList *g = g_list_nth (Denemo.gui->rhythms, position - 1);
      if (g)
        {
          RhythmPattern *r = g->data;
          if (r)
            {

              select_rhythm_pattern (r);
              insert_note_following_pattern (Denemo.gui);

              return SCM_BOOL_T;
            }
        }
    }
  return SCM_BOOL_F;
}



SCM
scheme_locate_dotdenemo (SCM optional)
{
  const gchar *dotdenemo = locatedotdenemo ();
  if (!dotdenemo)
    return SCM_BOOL (FALSE);
  SCM scm = scm_from_locale_string (dotdenemo);
  return scm;
}

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
  gchar *denemo_version = g_strdup_printf ("%d_%d_%d%s", major, minor, micro,
#ifdef G_OS_WIN32
                                           "_Win"
#else
                                           ""
#endif
    );
  gchar *filename = g_build_filename (get_data_dir (), "actions", NULL);
  gchar *actions_dir = g_strdup_printf ("%s%c", filename, G_DIR_SEPARATOR);
  if (filename)
    g_free (filename);

  filename = g_build_filename (locatedotdenemo (), "actions", NULL);
  gchar *local_actions_dir = g_strdup_printf ("%s%c", filename, G_DIR_SEPARATOR);
  if (filename)
    g_free (filename);

  g_print ("Version %s", denemo_version);

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
  gchar *filename = g_build_filename (locatedotdenemo (), "actions", "denemo.scm", NULL);
  if (g_file_test (filename, G_FILE_TEST_EXISTS))
    eval_file_with_catch (filename);    //scm_c_primitive_load(filename);
  if (filename)
    g_free (filename);
}

void
denemo_scheme_init (void)
{
  gchar *initscheme = Denemo.scheme_file;
  Denemo.gui->si->undo_guard++;

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
      gchar *filename = g_build_filename (get_data_dir (), "actions", name, NULL);
      if (g_file_test (filename, G_FILE_TEST_EXISTS))
        eval_file_with_catch (filename);
      g_free (name);
      g_free (filename);
    }

  load_local_scheme_init ();
  Denemo.gui->si->undo_guard--;
}

/*
  append scheme to user's denemo.scm
*/
void
append_to_local_scheme_init (gchar * scheme)
{
  gchar *filename = g_build_filename (locatedotdenemo (), "actions", "denemo.scm", NULL);
  FILE *fp = fopen (filename, "a+");
  if (fp)
    fprintf (fp, "%s", scheme);
  fclose (fp);
  g_free (filename);
}


/*
  load denemo.scm from system,

*/
static void
load_scheme_init (void)
{
  //Denemo.gui->si->undo_guard++;
  gchar *filename = g_build_filename (get_data_dir (), "actions", "denemo.scm", NULL);
  g_debug ("System wide denemo.scm %s\n", filename);
  if (g_file_test (filename, G_FILE_TEST_EXISTS))
    eval_file_with_catch (filename);    //scm_c_primitive_load(filename);
  else
    g_warning ("Cannot find Denemo's scheme initialization file denemo.scm");
  g_free (filename);

  //Denemo.gui->si->undo_guard--;
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

  Denemo.gui->mode = Denemo.prefs.mode;
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

  if (!Denemo.prefs.notation_palette)
    activate_action ("/MainMenu/ViewMenu/" ToggleEntryToolbar_STRING);

  if (!Denemo.prefs.console_pane)
    activate_action ("/MainMenu/ViewMenu/" ToggleConsoleView_STRING);

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
  GtkActionGroup *action_group = Denemo.action_group;
#include "generated/register_commands.h"
}

static void
create_scheme_identfiers (void)
{

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
#include "generated/scheme.h"
  init_denemo_notenames ();

  INSTALL_SCM_FUNCTION ("Hides all the menus", DENEMO_SCHEME_PREFIX "HideMenus", scheme_hide_menus);
  INSTALL_SCM_FUNCTION ("Hides Score buttons or shows them if passed #f", DENEMO_SCHEME_PREFIX "HideButtons", scheme_hide_buttons);
  INSTALL_SCM_FUNCTION ("Removes Score buttons", DENEMO_SCHEME_PREFIX "DestroyButtons", scheme_destroy_buttons);
  INSTALL_SCM_FUNCTION ("Hides the Denemo gui or shows it if passed #f", DENEMO_SCHEME_PREFIX "HideWindow", scheme_hide_window);

  INSTALL_SCM_FUNCTION1 ("Takes the the name of a scripted command. Runs the script stored for that command. Scripts which invoke other scripted commands use this (implicitly?) ", DENEMO_SCHEME_PREFIX "ScriptCallback", scheme_script_callback);

  INSTALL_SCM_FUNCTION1 ("create a dialog with the options & return the one chosen, of #f if the user cancels", DENEMO_SCHEME_PREFIX "GetOption", scheme_get_option);
  /* test with (display (d-GetOption "this\0and\0that\0")) */
  INSTALL_SCM_FUNCTION ("Returns the text on the clipboard", DENEMO_SCHEME_PREFIX "GetTextSelection", scheme_get_text_selection);
  INSTALL_SCM_FUNCTION ("Returns the padding that has been set by dragging in the Print view window", DENEMO_SCHEME_PREFIX "GetPadding", scheme_get_padding);
  INSTALL_SCM_FUNCTION ("Deprecated - gets an integer from the user via a dialog", DENEMO_SCHEME_PREFIX "GetRelativeFontSize", scheme_get_relative_font_size);
  /* install the scheme functions for calling extra Denemo functions created for the scripting interface */
  INSTALL_SCM_FUNCTION1 ("Takes a command name. called by a script if it requires initialization the initialization script is expected to be in init.scm in the menupath of the command passed in.", DENEMO_SCHEME_PREFIX "InitializeScript", scheme_initialize_script);
  INSTALL_SCM_FUNCTION1 (" pass in a path (from below menus) to a command script. Loads the command from .denemo or system if it can be found. It is used at startup in .denemo files like ReadingNoteNames.denemo which executes (d-LoadCommand \"MainMenu/Educational/ReadingNoteNames\") to ensure that the command it needs is in the command set.", DENEMO_SCHEME_PREFIX "LoadCommand", scheme_load_command);

  INSTALL_SCM_FUNCTION1 ("Takes a string, a menu path (from below menus). It executes the command for that menu item. Returns #f for no menu item.", DENEMO_SCHEME_PREFIX "ActivateMenuItem", scheme_activate_menu_item);


  INSTALL_SCM_FUNCTION ("Returns the directory holding the user's preferences", DENEMO_SCHEME_PREFIX "LocateDotDenemo", scheme_locate_dotdenemo);
  INSTALL_SCM_FUNCTION ("Returns the name of the type of object at the cursor", DENEMO_SCHEME_PREFIX "GetType", scheme_get_type);
  INSTALL_SCM_FUNCTION ("Returns the lilypond typesetting text for object at the cursor or #f if the object has not yet been typeset", DENEMO_SCHEME_PREFIX "GetLilyPond", scheme_get_lilypond);

  INSTALL_SCM_FUNCTION ("Returns a string numerator/denominator for a tuplet open object or #f if cursor not on a tuplet open", DENEMO_SCHEME_PREFIX "GetTuplet", scheme_get_tuplet);
  INSTALL_SCM_FUNCTION ("Set passed string as numerator/denominator for a tuplet open at cursor", DENEMO_SCHEME_PREFIX "SetTuplet", scheme_set_tuplet);

  INSTALL_SCM_FUNCTION ("Set passed 24 bit number as RGB color of background.", DENEMO_SCHEME_PREFIX "SetBackground", scheme_set_background);

  INSTALL_SCM_FUNCTION2 ("Takes a staff number m and a object number n. Returns the type of object at the (m, n)th position on the Denemo Clipboard or #f if none.", DENEMO_SCHEME_PREFIX "GetClipObjType", scheme_get_clip_obj_type);
  INSTALL_SCM_FUNCTION1 ("Takes a staff number m, Returns the number of objects in the mth staff on the Denemo Clipboard or #f if none.", DENEMO_SCHEME_PREFIX "GetClipObjects", scheme_get_clip_objects);


  INSTALL_SCM_FUNCTION2 ("Takes a staff number m and a object number n. Inserts the (m, n)th Denemo Object from Denemo Clipboard into the staff at the cursor position", DENEMO_SCHEME_PREFIX "PutClipObj", scheme_put_clip_obj);
  INSTALL_SCM_FUNCTION ("Clears the Denemo Music Clipboard", DENEMO_SCHEME_PREFIX "ClearClipboard", scheme_clear_clipboard);
  INSTALL_SCM_FUNCTION ("Gives the number of staffs in the Denemo Music Clipboard", DENEMO_SCHEME_PREFIX "GetStaffsInClipboard", scheme_get_staffs_in_clipboard);

  INSTALL_SCM_FUNCTION ("Gives the number of measures in the current staff", DENEMO_SCHEME_PREFIX "GetMeasuresInStaff", scheme_get_measures_in_staff);

  INSTALL_SCM_FUNCTION ("Makes the current staff a voice belonging to the staff above", DENEMO_SCHEME_PREFIX "StaffToVoice", scheme_staff_to_voice);

  INSTALL_SCM_FUNCTION ("Makes the current voice a independent staff", DENEMO_SCHEME_PREFIX "VoiceToStaff", scheme_voice_to_staff);
  INSTALL_SCM_FUNCTION ("Returns #f if the current staff is not a voice else true", DENEMO_SCHEME_PREFIX "IsVoice", scheme_is_voice);

  INSTALL_SCM_FUNCTION ("Adjusts the horizontal (x-) positioning of notes etc after paste", DENEMO_SCHEME_PREFIX "AdjustXes", scheme_adjust_xes);

  INSTALL_SCM_FUNCTION ("Turn highlighting of cursor off/on", DENEMO_SCHEME_PREFIX "HighlightCursor", scheme_highlight_cursor);

  INSTALL_SCM_FUNCTION ("Returns #t if there is an object at the cursor which has any printing behavior it may have overridden", DENEMO_SCHEME_PREFIX "GetNonprinting", scheme_get_nonprinting);

  INSTALL_SCM_FUNCTION ("Sets the Non Printing attribute of a chord (or note/rest) at the cursor. For a rest this makes a non printing rest, for a note it makes it ia pure rhythm (which will not print, but can be assigned pitch, e.g. via a MIDI keyboard. Pass in #f to unset the attribute", DENEMO_SCHEME_PREFIX "SetNonprinting", scheme_set_nonprinting);

  INSTALL_SCM_FUNCTION ("Returns #t if there is a grace note/chord at cursor, else #f", DENEMO_SCHEME_PREFIX "IsGrace", scheme_is_grace);
  INSTALL_SCM_FUNCTION ("Returns #t if there is a tied note/chord at cursor, else #f", DENEMO_SCHEME_PREFIX "IsTied", scheme_is_tied);

  INSTALL_SCM_FUNCTION ("Returns #t if there is a chord with slur starting at cursor, else #f", DENEMO_SCHEME_PREFIX "IsSlurStart", scheme_is_slur_start);

  INSTALL_SCM_FUNCTION ("Returns #t if there is a chord with slur ending at cursor, else #f", DENEMO_SCHEME_PREFIX "IsSlurEnd", scheme_is_slur_end);

  INSTALL_SCM_FUNCTION ("Returns #t if there is a chord with crescendo starting at cursor, else #f", DENEMO_SCHEME_PREFIX "IsCrescStart", scheme_is_cresc_start);
  INSTALL_SCM_FUNCTION ("Returns #t if there is a chord with crescendo ending at cursor, else #f", DENEMO_SCHEME_PREFIX "IsCrescEnd", scheme_is_cresc_end);

  INSTALL_SCM_FUNCTION ("Returns #t if there is a chord with diminuendo starting at cursor, else #f", DENEMO_SCHEME_PREFIX "IsDimStart", scheme_is_dim_start);
  INSTALL_SCM_FUNCTION ("Returns #t if there is a chord with diminuendo ending at cursor, else #f", DENEMO_SCHEME_PREFIX "IsDimEnd", scheme_is_dim_end);



  INSTALL_SCM_FUNCTION ("Returns #t if the cursor is in the selection area, else #f", DENEMO_SCHEME_PREFIX "IsInSelection", scheme_is_in_selection);

  INSTALL_SCM_FUNCTION ("Returns #t if the cursor is in the appending position, else #f", DENEMO_SCHEME_PREFIX "IsAppending", scheme_is_appending);

  INSTALL_SCM_FUNCTION ("Shifts the cursor up or down by the integer amount passed in", DENEMO_SCHEME_PREFIX "ShiftCursor", scheme_shift_cursor);


  INSTALL_SCM_FUNCTION ("Returns the movement number counting from 1", DENEMO_SCHEME_PREFIX "GetMovement", scheme_get_movement);
  INSTALL_SCM_FUNCTION ("Returns the staff/voice number counting from 1", DENEMO_SCHEME_PREFIX "GetStaff", scheme_get_staff);
  INSTALL_SCM_FUNCTION ("Returns the measure number counting from 1", DENEMO_SCHEME_PREFIX "GetMeasure", scheme_get_measure);
  INSTALL_SCM_FUNCTION ("Sets the display width of the object at the cursor to the value passed (in pixels)", DENEMO_SCHEME_PREFIX "SetObjectDisplayWidth", scheme_set_object_display_width);
  INSTALL_SCM_FUNCTION ("Returns the cursor horizontal position in current measure.\n 1 = first position in measure, n+1 is appending position where n is the number of objects in current measure", DENEMO_SCHEME_PREFIX "GetHorizontalPosition", scheme_get_horizontal_position);

  INSTALL_SCM_FUNCTION ("Returns the note name for the line or space where the cursor is", DENEMO_SCHEME_PREFIX "GetCursorNote", scheme_get_cursor_note);
  INSTALL_SCM_FUNCTION ("Returns the note name and octave in LilyPond notation for the line or space where the cursor is", DENEMO_SCHEME_PREFIX "GetCursorNoteWithOctave", scheme_get_cursor_note_with_octave);


  INSTALL_SCM_FUNCTION ("Prints out information about the object at the cursor", DENEMO_SCHEME_PREFIX "DebugObject", scheme_debug_object);

  INSTALL_SCM_FUNCTION ("Returns the name of the (highest) note in any chord at the cursor position, or #f if none", DENEMO_SCHEME_PREFIX "GetNoteName", scheme_get_note_name);
  INSTALL_SCM_FUNCTION ("Insert a rest at the cursor in the prevailing duration, or if given a integer, in that duration, setting the prevailing duration. If MIDI in is active, the cursor stays on the rest after inserting it, else it moves right.", DENEMO_SCHEME_PREFIX "InsertRest", scheme_insert_rest);
  INSTALL_SCM_FUNCTION ("Insert rests at the cursor to the value of the one whole measure in the key signature and return the number of rests inserted", DENEMO_SCHEME_PREFIX "PutWholeMeasureRests", scheme_put_whole_measure_rests);
  INSTALL_SCM_FUNCTION ("Takes optional integer parameter n = 1..., returns LilyPond representation of the nth note of the chord at the cursor counting from the lowest, or #f if none", DENEMO_SCHEME_PREFIX "GetNote", scheme_get_note);
  INSTALL_SCM_FUNCTION ("Takes optional integer parameter n = 1..., returns LilyPond representation of the nth note of the chord at the cursor counting from the highest, or #f if none", DENEMO_SCHEME_PREFIX "GetNoteFromTop", scheme_get_note_from_top);
  INSTALL_SCM_FUNCTION ("Takes optional integer parameter n = 1..., returns MIDI key for the nth note of the chord at the cursor counting from the highest, or #f if none", DENEMO_SCHEME_PREFIX "GetNoteFromTopAsMidi", scheme_get_note_from_top_as_midi);
  INSTALL_SCM_FUNCTION ("Returns a space separated string of LilyPond notes for the chord at the cursor position or #f if none", DENEMO_SCHEME_PREFIX "GetNotes", scheme_get_notes);
  INSTALL_SCM_FUNCTION ("Returns the number of dots on the note at the cursor, or #f if no note", DENEMO_SCHEME_PREFIX "GetDots", scheme_get_dots);
  INSTALL_SCM_FUNCTION ("Returns the base duration of the note at the cursor number=0, 1, 2 for whole half quarter note etc, or #f if none", DENEMO_SCHEME_PREFIX "GetNoteBaseDuration", scheme_get_note_base_duration);
  INSTALL_SCM_FUNCTION ("Returns the duration in LilyPond syntax of the note at the cursor, or #f if none", DENEMO_SCHEME_PREFIX "GetNoteDuration", scheme_get_note_duration);
  INSTALL_SCM_FUNCTION ("Returns start time for the object at the cursor, or #f if it has not been calculated", DENEMO_SCHEME_PREFIX "GetOnsetTime", scheme_get_onset_time);

  INSTALL_SCM_FUNCTION1 ("Takes an integer, Sets the number of ticks (PPQN) for the object at the cursor, returns #f if none; if the object is a chord it is set undotted", DENEMO_SCHEME_PREFIX "SetDurationInTicks", scheme_set_duration_in_ticks);

  INSTALL_SCM_FUNCTION ("Returns the number of ticks (PPQN) for the object at the cursor, or #f if none", DENEMO_SCHEME_PREFIX "GetDurationInTicks", scheme_get_duration_in_ticks);
  INSTALL_SCM_FUNCTION ("Returns the number of ticks (PPQN) for the chord without dots or tuplet effects at the cursor, or #f if not a chord. The value is -ve for special durations (i.e. non-standard notes)", DENEMO_SCHEME_PREFIX "GetBaseDurationInTicks", scheme_get_base_duration_in_ticks);

  INSTALL_SCM_FUNCTION ("Returns the tick count (PPQN) for the end of the object at the cursor, or #f if none", DENEMO_SCHEME_PREFIX "GetEndTick", scheme_get_end_tick);

  INSTALL_SCM_FUNCTION ("Returns the measure number at cursor position.", DENEMO_SCHEME_PREFIX "GetMeasureNumber", scheme_get_measure_number);


  INSTALL_SCM_FUNCTION ("Takes LilyPond note name string. Moves the cursor to the line or space", DENEMO_SCHEME_PREFIX "CursorToNote", scheme_cursor_to_note);

  INSTALL_SCM_FUNCTION ("Returns the prevailing key signature at the cursor", DENEMO_SCHEME_PREFIX "GetPrevailingKeysig", scheme_get_prevailing_keysig);
  INSTALL_SCM_FUNCTION ("Returns the prevailing time signature at the cursor", DENEMO_SCHEME_PREFIX "GetPrevailingTimesig", scheme_get_prevailing_timesig);
  INSTALL_SCM_FUNCTION ("Returns the prevailing clef at the cursor. Note that non-builtin clefs like drum are not handled yet.", DENEMO_SCHEME_PREFIX "GetPrevailingClef", scheme_get_prevailing_clef);

  INSTALL_SCM_FUNCTION ("Returns the LilyPond typesetting syntax for prevailing clef at the cursor.", DENEMO_SCHEME_PREFIX "GetPrevailingClefAsLilyPond", scheme_get_prevailing_clef_as_lilypond);
  INSTALL_SCM_FUNCTION ("Returns the LilyPond typesetting syntax for prevailing key signature at the cursor.", DENEMO_SCHEME_PREFIX "GetPrevailingKeysigAsLilyPond", scheme_get_prevailing_keysig_as_lilypond);
  INSTALL_SCM_FUNCTION ("Returns the LilyPond typesetting syntax for prevailing time signature at the cursor.", DENEMO_SCHEME_PREFIX "GetPrevailingTimesigAsLilyPond", scheme_get_prevailing_timesig_as_lilypond);


  INSTALL_SCM_FUNCTION ("Returns the prevailing duration, ie duration which will be used for the next inserted note.", DENEMO_SCHEME_PREFIX "GetPrevailingDuration", scheme_get_prevailing_duration);

  //more work needed, see above INSTALL_SCM_FUNCTION ("Sets the prevailing keysignature at the cursor to the string of 7 steps passed. Each step can be -1, 0 or 1",DENEMO_SCHEME_PREFIX"SetPrevailingKeysig", scheme_set_prevailing_keysig);

  INSTALL_SCM_FUNCTION ("Makes the initial keysig sharper/flatter", DENEMO_SCHEME_PREFIX "IncrementInitialKeysig", scheme_increment_initial_keysig);
  INSTALL_SCM_FUNCTION ("Makes the keysig sharper/flatter, affects keysig change when cursor is on one or appending after one, otherwise affects initial keysig", DENEMO_SCHEME_PREFIX "IncrementKeysig", scheme_increment_keysig);
  INSTALL_SCM_FUNCTION ("Appends a new movement without copying staff structure.", DENEMO_SCHEME_PREFIX "AddMovement", scheme_add_movement);



  INSTALL_SCM_FUNCTION ("Takes a string of LilyPond note names. Replaces the notes of the chord at the cursor with these notes, preserving other attributes", DENEMO_SCHEME_PREFIX "ChangeChordNotes", scheme_change_chord_notes);
  INSTALL_SCM_FUNCTION ("Takes a LilyPond note name, and changes the note at the cursor to that note", DENEMO_SCHEME_PREFIX "PutNoteName", scheme_put_note_name);
  INSTALL_SCM_FUNCTION ("Takes a LilyPond note name, changes the note at the cursor to have the accidental passed in either LilyPond string or integer -2..+2. Returns #f if cursor is not on a note position.  ", DENEMO_SCHEME_PREFIX "SetAccidental", scheme_set_accidental);

  INSTALL_SCM_FUNCTION ("Inserts a rest at the cursor; either passed in duration (note  prevailing duration not supported properly).", DENEMO_SCHEME_PREFIX "PutRest", scheme_put_rest);



  INSTALL_SCM_FUNCTION ("Takes a LilyPond note name, and adds that note to the chord", DENEMO_SCHEME_PREFIX "InsertNoteInChord", scheme_insert_note_in_chord);


  INSTALL_SCM_FUNCTION ("Moves the note at the cursor by the number of diatonic steps passed in", DENEMO_SCHEME_PREFIX "DiatonicShift", scheme_diatonic_shift);
  INSTALL_SCM_FUNCTION ("Moves the cursor to the right returning #t if this was possible", DENEMO_SCHEME_PREFIX "NextObject", scheme_next_object);
  INSTALL_SCM_FUNCTION ("Moves the cursor to the left returning #t if the cursor moved", DENEMO_SCHEME_PREFIX "PrevObject", scheme_prev_object);
  INSTALL_SCM_FUNCTION ("Moves the cursor to the next object in the current measure, returning #f if there were no more objects to the left in the current measure", DENEMO_SCHEME_PREFIX "NextObjectInMeasure", scheme_next_object_in_measure);
  INSTALL_SCM_FUNCTION ("Moves the cursor to the previous object in the current measure, returning #f if the cursor was on the first object", DENEMO_SCHEME_PREFIX "PrevObjectInMeasure", scheme_prev_object_in_measure);
  INSTALL_SCM_FUNCTION ("Moves the cursor to the next object in the selection. Returns #t if the cursor moved", DENEMO_SCHEME_PREFIX "NextSelectedObject", scheme_next_selected_object);
  INSTALL_SCM_FUNCTION ("Moves the cursor to the previous object in the selection. Returns #t if the cursor moved", DENEMO_SCHEME_PREFIX "PrevSelectedObject", scheme_prev_selected_object);
  INSTALL_SCM_FUNCTION ("Moves the cursor the the next object of type CHORD in the current staff. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "NextChord", scheme_next_chord);
  INSTALL_SCM_FUNCTION ("Moves the cursor the the previous object of type CHORD in the current staff. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "PrevChord", scheme_prev_chord);

  INSTALL_SCM_FUNCTION ("Moves the cursor the the next object of type CHORD in the current measure. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "NextChordInMeasure", scheme_next_chord_in_measure);
  INSTALL_SCM_FUNCTION ("Moves the cursor the the previous object of type CHORD in the current measure. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "PrevChordInMeasure", scheme_prev_chord_in_measure);


  INSTALL_SCM_FUNCTION ("Moves the cursor the next object of type CHORD which is not a rest in the current staff. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "NextNote", scheme_next_note);
  INSTALL_SCM_FUNCTION ("Moves the cursor the previous object of type CHORD which is not a rest in the current staff. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "PrevNote", scheme_prev_note);
  INSTALL_SCM_FUNCTION ("Creates a music Snippet comprising the object at the cursor Returns #f if not possible, otherwise an identifier for that snippet", DENEMO_SCHEME_PREFIX "CreateSnippetFromObject", scheme_create_snippet_from_object);

  INSTALL_SCM_FUNCTION ("Selects music Snippet from passed id Returns #f if not possible", DENEMO_SCHEME_PREFIX "SelectSnippet", scheme_select_snippet);

  INSTALL_SCM_FUNCTION ("Inserts music Snippet from passed id Returns #f if not possible", DENEMO_SCHEME_PREFIX "InsertSnippet", scheme_insert_snippet);



  INSTALL_SCM_FUNCTION ("Moves the cursor the next object that is a Denemo Directive in the current staff. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "NextStandaloneDirective", scheme_next_standalone_directive);
  INSTALL_SCM_FUNCTION ("Moves the cursor the previous object that is a Denemo Directive in the current staff. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "PrevStandaloneDirective", scheme_prev_standalone_directive);
  INSTALL_SCM_FUNCTION ("Moves the cursor within the current measure to the next object that is a Denemo Directive in the current staff. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "NextStandaloneDirectiveInMeasure", scheme_next_standalone_directive_in_measure);
  INSTALL_SCM_FUNCTION ("Moves the cursor within the current measure to the previous object that is a Denemo Directive in the current staff. Returns #f if the cursor did not move", DENEMO_SCHEME_PREFIX "PrevStandaloneDirectiveInMeasure", scheme_prev_standalone_directive_in_measure);


  INSTALL_SCM_FUNCTION ("Enforces the treatment of the note at the cursor as a chord in LilyPond", DENEMO_SCHEME_PREFIX "Chordize", scheme_chordize);
  INSTALL_SCM_FUNCTION ("Takes xml representation of a preference and adds it to the Denemo preferences", DENEMO_SCHEME_PREFIX "SetPrefs", scheme_set_prefs);

  INSTALL_SCM_FUNCTION ("Takes string name of a boolean-valued preference and returns the current value. Non-existent prefs return #f, ensure that the preference name is correct before using.", DENEMO_SCHEME_PREFIX "GetBooleanPref", scheme_get_boolean_pref);
  INSTALL_SCM_FUNCTION ("Takes string name of an int-valued preference and returns the current value. Non-existent prefs return #f", DENEMO_SCHEME_PREFIX "GetIntPref", scheme_get_int_pref);
  INSTALL_SCM_FUNCTION ("Takes string name of a string-valued preference and returns the current value. Non-existent prefs return #f", DENEMO_SCHEME_PREFIX "GetStringPref", scheme_get_string_pref);

  INSTALL_SCM_FUNCTION ("Takes a script as a string, which will be stored. All the callbacks are called when the musical score is closed", DENEMO_SCHEME_PREFIX "AttachQuitCallback", scheme_attach_quit_callback);
  INSTALL_SCM_FUNCTION ("Removes a callback from the current musical score", DENEMO_SCHEME_PREFIX "DetachQuitCallback", scheme_detach_quit_callback);
  INSTALL_SCM_FUNCTION ("Returns DENEMO_INPUTMIDI, DENEMO_INPUTKEYBOARD, DENEMO_INPUTAUDIO depending on the source of input to Denemo.", DENEMO_SCHEME_PREFIX "GetInputSource", scheme_get_input_source);
  INSTALL_SCM_FUNCTION ("Pops up a menu given by the list of pairs in the argument. Each pair should be a label string and an expression, the expression for the chosen label is returned. Alternatively the label string can be replaced by a pair of strings, label . tooltip. The third syntax is just a list of string labels, the chosen string is returned.", DENEMO_SCHEME_PREFIX "PopupMenu", scheme_popup_menu);
  INSTALL_SCM_FUNCTION ("Returns a list of the target type and grob (if a directive). Target is set by clicking on the typeset version of the score at a link that LilyPond has inserted.", DENEMO_SCHEME_PREFIX "GetTargetInfo", scheme_get_target_info);
  INSTALL_SCM_FUNCTION ("Interactively sets a target (a click on a LilyPond link in the printview window) from the user ", DENEMO_SCHEME_PREFIX "GetNewTarget", scheme_get_new_target);
  INSTALL_SCM_FUNCTION ("Interactively sets a point in the printview window from the user", DENEMO_SCHEME_PREFIX "GetNewPoint", scheme_get_new_point);
  INSTALL_SCM_FUNCTION ("Interactively sets a reference point (a click on a point in the printview window) from the user showing a cross hairs prompt ", DENEMO_SCHEME_PREFIX "GetReferencePoint", scheme_get_reference_point);
  INSTALL_SCM_FUNCTION ("Interactively gets an offset from the user in the print view window. The offset is from the last clicked object in the print view window. Returns pair of numbers x is positive to the right, y is positive upwards.", DENEMO_SCHEME_PREFIX "GetOffset", scheme_get_offset);
  INSTALL_SCM_FUNCTION ("Interactively sets a control point for a curve in the print view window. Takes one parameter the number 1-4 of the control point to set.", DENEMO_SCHEME_PREFIX "GetControlPoint", scheme_get_control_point);
  INSTALL_SCM_FUNCTION ("Interactively gets a curve from the user in the print view window. Returns a list of pairs of numbers, the control points of the curve.", DENEMO_SCHEME_PREFIX "GetCurve", scheme_get_curve);
  INSTALL_SCM_FUNCTION ("Interactively gets two positions from the user in the print view window. Returns pair of pairs numbers.", DENEMO_SCHEME_PREFIX "GetPositions", scheme_get_positions);

  INSTALL_SCM_FUNCTION4 ("Takes 4 parameters and makes http transaction with www.denemo.org", DENEMO_SCHEME_PREFIX "HTTP", scheme_http);

  INSTALL_SCM_FUNCTION4 ("Move to given Movement, voice measure and object position. Takes 4 parameters integers starting from 1, use #f for no change. Returns #f if it fails", DENEMO_SCHEME_PREFIX "GoToPosition", scheme_goto_position);


  INSTALL_SCM_FUNCTION4 ("Takes up to three strings, title, prompt and initial value. Shows these to the user and returns the user's string. Fourth parameter makes the dialog not block waiting for input", DENEMO_SCHEME_PREFIX "GetUserInput", scheme_get_user_input);
  INSTALL_SCM_FUNCTION4 ("Takes up to three strings, title, prompt and initial value. Shows these to the user with a text editor for the user to return a string. Buttons are present to insert snippets which are bracketed with secion characters in the return string. Fourth parameter makes the dialog not block waiting for input", DENEMO_SCHEME_PREFIX "GetUserInputWithSnippets", scheme_get_user_input_with_snippets);
  INSTALL_SCM_FUNCTION ("Takes a message as a string. Pops up the message for the user to take note of as a warning", DENEMO_SCHEME_PREFIX "WarningDialog", scheme_warningdialog);
  INSTALL_SCM_FUNCTION ("Takes a message as a string. Pops up the message for the user to take note of as a informative message", DENEMO_SCHEME_PREFIX "InfoDialog", scheme_infodialog);
  INSTALL_SCM_FUNCTION ("Takes a message as a string. Pops up the message inside of a pulsing progressbar", DENEMO_SCHEME_PREFIX "ProgressBar", scheme_progressbar);
  INSTALL_SCM_FUNCTION ("If running, Stops the ProgressBar.", DENEMO_SCHEME_PREFIX "ProgressBarStop", scheme_progressbar_stop);
  INSTALL_SCM_FUNCTION ("Typesets the score. Takes a script which will be called when Refresh is performed on the typeset window.", DENEMO_SCHEME_PREFIX "TypesetForScript", scheme_typeset_for_script);
  INSTALL_SCM_FUNCTION ("Prints from the PDF file generated by TypesetForScript.", DENEMO_SCHEME_PREFIX "PrintTypesetPDF", scheme_print_typeset_pdf);


  INSTALL_SCM_FUNCTION ("Intercepts the next keypress and returns a string containing the character. Returns #f if keyboard interception was not possible.", DENEMO_SCHEME_PREFIX "GetChar", scheme_get_char);
  INSTALL_SCM_FUNCTION ("Intercepts the next keypress and returns a string containing the name of the keypress (the shortcut name). Returns #f if keyboard interception was not possible.", DENEMO_SCHEME_PREFIX "GetKeypress", scheme_get_keypress);
  INSTALL_SCM_FUNCTION ("Returns the last keypress that successfully invoked a command ", DENEMO_SCHEME_PREFIX "GetCommandKeypress", scheme_get_command_keypress);

  INSTALL_SCM_FUNCTION ("Intercepts the next keypress and returns the name of the command invoked, before invoking the command. Returns #f if the keypress is not a shortcut for any command", DENEMO_SCHEME_PREFIX "GetCommand", scheme_get_command);
  INSTALL_SCM_FUNCTION ("Intercepts the next keyboard shortcut and returns the name of the command invoked, before invoking the command. Returns #f if the keypress(es) are not a shortcut for any command", DENEMO_SCHEME_PREFIX "GetCommandFromUser", scheme_get_command_from_user);
  INSTALL_SCM_FUNCTION ("Locks the standalone directive at the cursor so that it runs its delete action when deleted. The tag should be the name of a command that responds to the delete parameter.", DENEMO_SCHEME_PREFIX "LockDirective", scheme_lock_directive);

  INSTALL_SCM_FUNCTION2 ("Sets an \"action script\" on the directive of the given tag", DENEMO_SCHEME_PREFIX "SetDirectiveTagActionScript", scheme_set_action_script_for_tag);

#define INSTALL_GET_TAG(what)\
  INSTALL_SCM_FUNCTION1 ("Takes a optional tag. Returns that tag if a "#what" directive exists at the cursor, else returns the tag of the first such directive at the cursor, or #f if none", DENEMO_SCHEME_PREFIX"DirectiveGetForTag"  "-" #what, scheme_##what##_directive_get_tag);
  INSTALL_GET_TAG (object);
  INSTALL_GET_TAG (standalone);
  INSTALL_GET_TAG (chord);
  INSTALL_GET_TAG (note);
  INSTALL_GET_TAG (staff);
  INSTALL_GET_TAG (voice);
  INSTALL_GET_TAG (score);
  INSTALL_GET_TAG (clef);
  INSTALL_GET_TAG (timesig);
  INSTALL_GET_TAG (tuplet);
  INSTALL_GET_TAG (stemdirective);
  INSTALL_GET_TAG (keysig);
  INSTALL_GET_TAG (scoreheader);
  INSTALL_GET_TAG (header);
  INSTALL_GET_TAG (paper);
  INSTALL_GET_TAG (layout);
  INSTALL_GET_TAG (movementcontrol);
#undef INSTALL_GET_TAG

#define INSTALL_GET_NTH_TAG(what)\
  INSTALL_SCM_FUNCTION1 ("Takes a number n. Returns the tag of the nth "#what" directive if it exists else returns #f if none", DENEMO_SCHEME_PREFIX"DirectiveGetNthTag"  "-" #what, scheme_##what##_directive_get_nth_tag);
  INSTALL_GET_NTH_TAG (chord);
  INSTALL_GET_NTH_TAG (note);
  INSTALL_GET_NTH_TAG (staff);
  INSTALL_GET_NTH_TAG (voice);
  INSTALL_GET_NTH_TAG (score);
  INSTALL_GET_NTH_TAG (clef);
  INSTALL_GET_NTH_TAG (timesig);
  INSTALL_GET_NTH_TAG (tuplet);
  INSTALL_GET_NTH_TAG (stemdirective);
  INSTALL_GET_NTH_TAG (keysig);
  INSTALL_GET_NTH_TAG (scoreheader);
  INSTALL_GET_NTH_TAG (header);
  INSTALL_GET_NTH_TAG (paper);
  INSTALL_GET_NTH_TAG (layout);
  INSTALL_GET_NTH_TAG (movementcontrol);
#undef INSTALL_GET_NTH_TAG




#define INSTALL_EDIT(what)\
  INSTALL_SCM_FUNCTION1 ("Deletes a "#what" directive of the passed in tag. Returns #f if not deleted", DENEMO_SCHEME_PREFIX"DirectiveDelete"  "-" #what, scheme_delete_##what##_directive); \
  INSTALL_SCM_FUNCTION1 ("Activates a "#what" directive widget of the passed in tag. Returns #f if not a button", DENEMO_SCHEME_PREFIX"DirectiveActivate"  "-" #what, scheme_activate_##what##_directive); \
  INSTALL_SCM_FUNCTION1 ("Takes a tag. Lets the user edit (by running the editscript named by the tag) a "#what" directive of the passed in tag. Returns #f if none", DENEMO_SCHEME_PREFIX"DirectiveTextEdit"  "-" #what, scheme_text_edit_##what##_directive);
  INSTALL_EDIT (note);
  INSTALL_EDIT (chord);
  INSTALL_EDIT (staff);
  INSTALL_EDIT (voice);
  INSTALL_EDIT (score);
  install_scm_function1 (DENEMO_SCHEME_PREFIX "DirectiveTextEdit-standalone", scheme_text_edit_standalone_directive);

  install_scm_function1 (DENEMO_SCHEME_PREFIX "DirectiveDelete-object", scheme_delete_object_directive);


#define INSTALL_PUT(what, field)\
 INSTALL_SCM_FUNCTION2 ("Writes the " #field" field (a string) of the " #what" directive with the passed int tag. Creates the directive of the given type and tag if it does not exist.",DENEMO_SCHEME_PREFIX"DirectivePut" "-" #what "-" #field, scheme_##what##_directive_put_##field);

#define INSTALL_GET(what, field)\
 INSTALL_SCM_FUNCTION1 ("Gets the value of the " #field" field (a string) of the " #what" directive with the passed tag.",DENEMO_SCHEME_PREFIX"DirectiveGet" "-" #what "-" #field, scheme_##what##_directive_get_##field);

  INSTALL_GET (object, minpixels);
  INSTALL_PUT (object, minpixels);

  //block to repeat for new  directive fields 

  INSTALL_GET (standalone, minpixels);
  INSTALL_GET (chord, minpixels);
  INSTALL_GET (note, minpixels);
  INSTALL_GET (staff, minpixels);
  INSTALL_GET (voice, minpixels);
  INSTALL_GET (score, minpixels);
  INSTALL_GET (clef, minpixels);
  INSTALL_GET (timesig, minpixels);
  INSTALL_GET (tuplet, minpixels);
  INSTALL_GET (stemdirective, minpixels);
  INSTALL_GET (keysig, minpixels);

  INSTALL_GET (scoreheader, minpixels);
  INSTALL_GET (header, minpixels);
  INSTALL_GET (paper, minpixels);
  INSTALL_GET (layout, minpixels);
  INSTALL_GET (movementcontrol, minpixels);

  INSTALL_PUT (standalone, minpixels);
  INSTALL_PUT (chord, minpixels);
  INSTALL_PUT (note, minpixels);
  INSTALL_PUT (staff, minpixels);
  INSTALL_PUT (voice, minpixels);
  INSTALL_PUT (score, minpixels);
  INSTALL_PUT (clef, minpixels);
  INSTALL_PUT (timesig, minpixels);
  INSTALL_PUT (tuplet, minpixels);
  INSTALL_PUT (stemdirective, minpixels);
  INSTALL_PUT (keysig, minpixels);


  INSTALL_PUT (scoreheader, minpixels);
  INSTALL_PUT (header, minpixels);
  INSTALL_PUT (paper, minpixels);
  INSTALL_PUT (layout, minpixels);
  INSTALL_PUT (movementcontrol, minpixels);

  //end block to repeat for new  directive fields 


  INSTALL_GET (standalone, grob);
  INSTALL_GET (chord, grob);
  INSTALL_GET (note, grob);
  INSTALL_GET (staff, grob);
  INSTALL_GET (voice, grob);
  INSTALL_GET (score, grob);
  INSTALL_GET (clef, grob);
  INSTALL_GET (timesig, grob);
  INSTALL_GET (tuplet, grob);
  INSTALL_GET (stemdirective, grob);
  INSTALL_GET (keysig, grob);
  INSTALL_GET (standalone, grob);

  // INSTALL_GET(scoreheader, grob);
  // INSTALL_GET(header, grob);
  // INSTALL_GET(paper, grob);
  // INSTALL_GET(layout, grob);
  // INSTALL_GET(movementcontrol, grob);

  INSTALL_PUT (standalone, grob);
  INSTALL_PUT (chord, grob);
  INSTALL_PUT (note, grob);
  //INSTALL_PUT(staff, grob);
  //INSTALL_PUT(voice, grob);
  INSTALL_PUT (score, grob);
  INSTALL_PUT (clef, grob);
  INSTALL_PUT (timesig, grob);
  INSTALL_PUT (tuplet, grob);
  INSTALL_PUT (stemdirective, grob);
  INSTALL_PUT (keysig, grob);


  // INSTALL_PUT(scoreheader, grob);
  // INSTALL_PUT(header, grob);
  // INSTALL_PUT(paper, grob);
  // INSTALL_PUT(layout, grob);
  // INSTALL_PUT(movementcontrol, grob);
  //end of grob





  INSTALL_GET (standalone, midibytes);
  INSTALL_GET (chord, midibytes);
  INSTALL_GET (note, midibytes);
  INSTALL_GET (staff, midibytes);
  INSTALL_GET (voice, midibytes);
  INSTALL_GET (score, midibytes);
  INSTALL_GET (movementcontrol, midibytes);
  INSTALL_PUT (standalone, midibytes);
  INSTALL_PUT (chord, midibytes);
  INSTALL_PUT (note, midibytes);
  INSTALL_PUT (staff, midibytes);
  INSTALL_PUT (voice, midibytes);
  INSTALL_PUT (score, midibytes);
  INSTALL_PUT (movementcontrol, midibytes);





  INSTALL_GET (standalone, override);
  INSTALL_GET (chord, override);
  INSTALL_GET (note, override);
  INSTALL_GET (staff, override);
  INSTALL_GET (voice, override);
  INSTALL_GET (score, override);

  INSTALL_PUT (standalone, override);
  INSTALL_PUT (chord, override);
  INSTALL_PUT (note, override);
  INSTALL_PUT (staff, override);
  INSTALL_PUT (voice, override);
  INSTALL_PUT (score, override);


  //graphic 
  INSTALL_PUT (note, graphic);
  //INSTALL_GET(note, graphic);

  INSTALL_PUT (chord, graphic);
  //INSTALL_GET(chord, graphic);

  INSTALL_PUT (standalone, graphic);
  //INSTALL_GET(standalone, graphic);


  INSTALL_PUT (staff, graphic);
  INSTALL_PUT (voice, graphic);

  INSTALL_PUT (score, graphic);
  //graphic



  INSTALL_PUT (chord, display);
  INSTALL_PUT (chord, prefix);
  INSTALL_PUT (chord, postfix);

  INSTALL_GET (chord, display);
  INSTALL_GET (chord, prefix);
  INSTALL_GET (chord, postfix);


  INSTALL_PUT (note, display);
  INSTALL_PUT (note, prefix);
  INSTALL_PUT (note, postfix);

  INSTALL_GET (note, display);
  INSTALL_GET (note, prefix);
  INSTALL_GET (note, postfix);

  INSTALL_PUT (standalone, display);
  INSTALL_PUT (standalone, prefix);
  INSTALL_PUT (standalone, postfix);

  INSTALL_GET (standalone, display);
  INSTALL_GET (standalone, prefix);
  INSTALL_GET (standalone, postfix);


  INSTALL_PUT (staff, display);
  INSTALL_PUT (staff, prefix);
  INSTALL_PUT (staff, postfix);

  INSTALL_GET (staff, display);
  INSTALL_GET (staff, prefix);
  INSTALL_GET (staff, postfix);

  INSTALL_PUT (voice, display);
  INSTALL_PUT (voice, prefix);
  INSTALL_PUT (voice, postfix);

  INSTALL_GET (voice, display);
  INSTALL_GET (voice, prefix);
  INSTALL_GET (voice, postfix);

  INSTALL_PUT (score, display);
  INSTALL_PUT (score, prefix);
  INSTALL_PUT (score, postfix);

  INSTALL_GET (score, display);
  INSTALL_GET (score, prefix);
  INSTALL_GET (score, postfix);


  INSTALL_GET (score, width);
  INSTALL_GET (score, height);



  INSTALL_GET (score, x);
  INSTALL_GET (score, gx);
  INSTALL_GET (score, tx);
  INSTALL_PUT (score, x);
  INSTALL_PUT (score, gx);
  INSTALL_PUT (score, tx);

  INSTALL_GET (score, y);
  INSTALL_GET (score, gy);
  INSTALL_GET (score, ty);
  INSTALL_PUT (score, y);
  INSTALL_PUT (score, gy);
  INSTALL_PUT (score, ty);




  INSTALL_PUT (note, x);
  INSTALL_GET (note, x);
  INSTALL_PUT (chord, x);
  INSTALL_GET (chord, x);
  INSTALL_PUT (note, y);
  INSTALL_GET (note, y);
  INSTALL_PUT (chord, y);
  INSTALL_GET (chord, y);

  INSTALL_PUT (note, tx);
  INSTALL_GET (note, tx);
  INSTALL_PUT (chord, tx);
  INSTALL_GET (chord, tx);
  INSTALL_PUT (note, ty);
  INSTALL_GET (note, ty);
  INSTALL_PUT (chord, ty);
  INSTALL_GET (chord, ty);



  INSTALL_PUT (note, gx);
  INSTALL_GET (note, gx);
  INSTALL_PUT (chord, gx);
  INSTALL_GET (chord, gx);
  INSTALL_PUT (note, gy);
  INSTALL_GET (note, gy);
  INSTALL_PUT (chord, gy);
  INSTALL_GET (chord, gy);


  INSTALL_PUT (standalone, x);
  INSTALL_GET (standalone, x);
  INSTALL_PUT (standalone, y);
  INSTALL_GET (standalone, y);

  INSTALL_PUT (standalone, tx);
  INSTALL_GET (standalone, tx);
  INSTALL_PUT (standalone, ty);
  INSTALL_GET (standalone, ty);

  INSTALL_PUT (standalone, gx);
  INSTALL_GET (standalone, gx);
  INSTALL_PUT (standalone, gy);
  INSTALL_GET (standalone, gy);




  INSTALL_GET (note, width);
  INSTALL_GET (chord, width);
  INSTALL_GET (standalone, width);
  INSTALL_GET (note, height);
  INSTALL_GET (chord, height);
  INSTALL_GET (standalone, height);



  //block to copy for new type of directive
  INSTALL_PUT (clef, display);
  INSTALL_PUT (clef, prefix);
  INSTALL_PUT (clef, postfix);
  INSTALL_PUT (clef, graphic);


  INSTALL_GET (clef, display);
  INSTALL_GET (clef, prefix);
  INSTALL_GET (clef, postfix);

  INSTALL_PUT (clef, x) INSTALL_PUT (clef, y) INSTALL_PUT (clef, tx) INSTALL_PUT (clef, ty) INSTALL_PUT (clef, gx) INSTALL_PUT (clef, gy) INSTALL_PUT (clef, override) INSTALL_GET (clef, x) INSTALL_GET (clef, y) INSTALL_GET (clef, tx) INSTALL_GET (clef, ty) INSTALL_GET (clef, gx) INSTALL_GET (clef, gy) INSTALL_GET (clef, override) INSTALL_GET (clef, width) INSTALL_GET (clef, height) INSTALL_EDIT (clef);
  // end of block to copy for new type of directive

  INSTALL_PUT (timesig, display);
  INSTALL_PUT (timesig, prefix);
  INSTALL_PUT (timesig, postfix);
  INSTALL_PUT (timesig, graphic);


  INSTALL_GET (timesig, display);
  INSTALL_GET (timesig, prefix);
  INSTALL_GET (timesig, postfix);

  INSTALL_PUT (timesig, x) INSTALL_PUT (timesig, y) INSTALL_PUT (timesig, tx) INSTALL_PUT (timesig, ty) INSTALL_PUT (timesig, gx) INSTALL_PUT (timesig, gy) INSTALL_PUT (timesig, override) INSTALL_GET (timesig, x) INSTALL_GET (timesig, y) INSTALL_GET (timesig, tx) INSTALL_GET (timesig, ty) INSTALL_GET (timesig, gx) INSTALL_GET (timesig, gy) INSTALL_GET (timesig, override) INSTALL_GET (timesig, width) INSTALL_GET (timesig, height) INSTALL_EDIT (timesig);

  INSTALL_PUT (tuplet, display);
  INSTALL_PUT (tuplet, prefix);
  INSTALL_PUT (tuplet, postfix);
  INSTALL_PUT (tuplet, graphic);


  INSTALL_GET (tuplet, display);
  INSTALL_GET (tuplet, prefix);
  INSTALL_GET (tuplet, postfix);

  INSTALL_PUT (tuplet, x) INSTALL_PUT (tuplet, y) INSTALL_PUT (tuplet, tx) INSTALL_PUT (tuplet, ty) INSTALL_PUT (tuplet, gx) INSTALL_PUT (tuplet, gy) INSTALL_PUT (tuplet, override) INSTALL_GET (tuplet, x) INSTALL_GET (tuplet, y) INSTALL_GET (tuplet, tx) INSTALL_GET (tuplet, ty) INSTALL_GET (tuplet, gx) INSTALL_GET (tuplet, gy) INSTALL_GET (tuplet, override) INSTALL_GET (tuplet, width) INSTALL_GET (tuplet, height) INSTALL_EDIT (tuplet);

  INSTALL_PUT (stemdirective, display);
  INSTALL_PUT (stemdirective, prefix);
  INSTALL_PUT (stemdirective, postfix);
  INSTALL_PUT (stemdirective, graphic);


  INSTALL_GET (stemdirective, display);
  INSTALL_GET (stemdirective, prefix);
  INSTALL_GET (stemdirective, postfix);

  INSTALL_PUT (stemdirective, x)
    INSTALL_PUT (stemdirective, y)
    INSTALL_PUT (stemdirective, tx) INSTALL_PUT (stemdirective, ty) INSTALL_PUT (stemdirective, gx) INSTALL_PUT (stemdirective, gy) INSTALL_PUT (stemdirective, override) INSTALL_GET (stemdirective, x) INSTALL_GET (stemdirective, y) INSTALL_GET (stemdirective, tx) INSTALL_GET (stemdirective, ty) INSTALL_GET (stemdirective, gx) INSTALL_GET (stemdirective, gy) INSTALL_GET (stemdirective, override) INSTALL_GET (stemdirective, width) INSTALL_GET (stemdirective, height) INSTALL_EDIT (stemdirective);

  INSTALL_PUT (keysig, display);
  INSTALL_PUT (keysig, prefix);
  INSTALL_PUT (keysig, postfix);
  INSTALL_PUT (keysig, graphic);


  INSTALL_GET (keysig, display);
  INSTALL_GET (keysig, prefix);
  INSTALL_GET (keysig, postfix);

  INSTALL_PUT (keysig, x) INSTALL_PUT (keysig, y) INSTALL_PUT (keysig, tx) INSTALL_PUT (keysig, ty) INSTALL_PUT (keysig, gx) INSTALL_PUT (keysig, gy) INSTALL_PUT (keysig, override) INSTALL_GET (keysig, x) INSTALL_GET (keysig, y) INSTALL_GET (keysig, tx) INSTALL_GET (keysig, ty) INSTALL_GET (keysig, gx) INSTALL_GET (keysig, gy) INSTALL_GET (keysig, override) INSTALL_GET (keysig, width) INSTALL_GET (keysig, height) INSTALL_EDIT (keysig);


  INSTALL_PUT (scoreheader, display);
  INSTALL_PUT (scoreheader, prefix);
  INSTALL_PUT (scoreheader, postfix);
  INSTALL_PUT (scoreheader, graphic);


  INSTALL_GET (scoreheader, display);
  INSTALL_GET (scoreheader, prefix);
  INSTALL_GET (scoreheader, postfix);

  INSTALL_PUT (scoreheader, x)
    INSTALL_PUT (scoreheader, y) INSTALL_PUT (scoreheader, tx) INSTALL_PUT (scoreheader, ty) INSTALL_PUT (scoreheader, gx) INSTALL_PUT (scoreheader, gy) INSTALL_PUT (scoreheader, override) INSTALL_GET (scoreheader, x) INSTALL_GET (scoreheader, y) INSTALL_GET (scoreheader, tx) INSTALL_GET (scoreheader, ty) INSTALL_GET (scoreheader, gx) INSTALL_GET (scoreheader, gy) INSTALL_GET (scoreheader, override) INSTALL_GET (scoreheader, width) INSTALL_GET (scoreheader, height) INSTALL_EDIT (scoreheader);


  INSTALL_PUT (header, display);
  INSTALL_PUT (header, prefix);
  INSTALL_PUT (header, postfix);
  INSTALL_PUT (header, graphic);


  INSTALL_GET (header, display);
  INSTALL_GET (header, prefix);
  INSTALL_GET (header, postfix);

  INSTALL_PUT (header, x) INSTALL_PUT (header, y) INSTALL_PUT (header, tx) INSTALL_PUT (header, ty) INSTALL_PUT (header, gx) INSTALL_PUT (header, gy) INSTALL_PUT (header, override) INSTALL_GET (header, x) INSTALL_GET (header, y) INSTALL_GET (header, tx) INSTALL_GET (header, ty) INSTALL_GET (header, gx) INSTALL_GET (header, gy) INSTALL_GET (header, override) INSTALL_GET (header, width) INSTALL_GET (header, height) INSTALL_EDIT (header);


  INSTALL_PUT (paper, display);
  INSTALL_PUT (paper, prefix);
  INSTALL_PUT (paper, postfix);
  INSTALL_PUT (paper, graphic);


  INSTALL_GET (paper, display);
  INSTALL_GET (paper, prefix);
  INSTALL_GET (paper, postfix);

  INSTALL_PUT (paper, x) INSTALL_PUT (paper, y) INSTALL_PUT (paper, tx) INSTALL_PUT (paper, ty) INSTALL_PUT (paper, gx) INSTALL_PUT (paper, gy) INSTALL_PUT (paper, override) INSTALL_GET (paper, x) INSTALL_GET (paper, y) INSTALL_GET (paper, tx) INSTALL_GET (paper, ty) INSTALL_GET (paper, gx) INSTALL_GET (paper, gy) INSTALL_GET (paper, override) INSTALL_GET (paper, width) INSTALL_GET (paper, height) INSTALL_EDIT (paper);


  INSTALL_PUT (layout, display);
  INSTALL_PUT (layout, prefix);
  INSTALL_PUT (layout, postfix);
  INSTALL_PUT (layout, graphic);


  INSTALL_GET (layout, display);
  INSTALL_GET (layout, prefix);
  INSTALL_GET (layout, postfix);

  INSTALL_PUT (layout, x) INSTALL_PUT (layout, y) INSTALL_PUT (layout, tx) INSTALL_PUT (layout, ty) INSTALL_PUT (layout, gx) INSTALL_PUT (layout, gy) INSTALL_PUT (layout, override) INSTALL_GET (layout, x) INSTALL_GET (layout, y) INSTALL_GET (layout, tx) INSTALL_GET (layout, ty) INSTALL_GET (layout, gx) INSTALL_GET (layout, gy) INSTALL_GET (layout, override) INSTALL_GET (layout, width) INSTALL_GET (layout, height) INSTALL_EDIT (layout);

  INSTALL_PUT (movementcontrol, display);
  INSTALL_PUT (movementcontrol, prefix);
  INSTALL_PUT (movementcontrol, postfix);
  INSTALL_PUT (movementcontrol, graphic);


  INSTALL_GET (movementcontrol, display);
  INSTALL_GET (movementcontrol, prefix);
  INSTALL_GET (movementcontrol, postfix);

  INSTALL_PUT (movementcontrol, x)
    INSTALL_PUT (movementcontrol, y)
    INSTALL_PUT (movementcontrol, tx)
    INSTALL_PUT (movementcontrol, ty) INSTALL_PUT (movementcontrol, gx) INSTALL_PUT (movementcontrol, gy) INSTALL_PUT (movementcontrol, override) INSTALL_GET (movementcontrol, x) INSTALL_GET (movementcontrol, y) INSTALL_GET (movementcontrol, tx) INSTALL_GET (movementcontrol, ty) INSTALL_GET (movementcontrol, gx) INSTALL_GET (movementcontrol, gy) INSTALL_GET (movementcontrol, override) INSTALL_GET (movementcontrol, width) INSTALL_GET (movementcontrol, height) INSTALL_EDIT (movementcontrol);


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
  install_scm_function1 (DENEMO_SCHEME_PREFIX "PutTextClipboard", scheme_put_text_clipboard);

  INSTALL_SCM_FUNCTION ("Asks the user for a user name which is returned", DENEMO_SCHEME_PREFIX "GetUserName", scheme_get_username);
  INSTALL_SCM_FUNCTION ("Asks the user for a password which is returned", DENEMO_SCHEME_PREFIX "GetPassword", scheme_get_password);

  INSTALL_SCM_FUNCTION ("Returns an integer value, a set of bitfields representing the keyboard state, e.g. GDK_SHIFT_MASK etc", DENEMO_SCHEME_PREFIX "GetKeyboardState", scheme_get_keyboard_state);
  INSTALL_SCM_FUNCTION ("Routes the MIDI in to MIDI out if it is not intercepted by d-GetMidi", DENEMO_SCHEME_PREFIX "SetMidiThru", scheme_set_midi_thru);

  INSTALL_SCM_FUNCTION ("Returns the ticks of the next event on the recorded MIDI track -ve if it is a NOTEOFF or #f if none. Advances to the next note.", DENEMO_SCHEME_PREFIX "GetRecordedMidiOnTick", scheme_get_recorded_midi_on_tick);

  INSTALL_SCM_FUNCTION ("Returns the LilyPond representation of the passed MIDI key number, using the current enharmonic set.", DENEMO_SCHEME_PREFIX "GetNoteForMidiKey", scheme_get_note_for_midi_key);



  INSTALL_SCM_FUNCTION ("Returns the ticks of the next event on the recorded MIDI track -ve if it is a NOTEOFF or #f if none", DENEMO_SCHEME_PREFIX "GetRecordedMidiNote", scheme_get_recorded_midi_note);

  INSTALL_SCM_FUNCTION ("Rewinds the recorded MIDI track returns #f if no MIDI track recorded", DENEMO_SCHEME_PREFIX "RewindRecordedMidi", scheme_rewind_recorded_midi);

  INSTALL_SCM_FUNCTION ("Intercepts a MIDI event and returns it as a 4 byte number", DENEMO_SCHEME_PREFIX "GetMidi", scheme_get_midi);

  INSTALL_SCM_FUNCTION ("Takes one bool parameter - MIDI events will be captured/not captured depending on the value passed in, returns previous value.", DENEMO_SCHEME_PREFIX "SetMidiCapture", scheme_set_midi_capture);


  INSTALL_SCM_FUNCTION ("Switches to playalong playback. When playing or recording playback will not advance beyond the cursor position unless then mouse is moved or the next note is played in via MIDI in.", DENEMO_SCHEME_PREFIX "TogglePlayAlong", scheme_toggle_playalong);
  INSTALL_SCM_FUNCTION ("Switches to mouse conducting playback. Playback will not advance beyond the cursor position unless then mouse is moved in the drawing area.", DENEMO_SCHEME_PREFIX "ToggleConduct", scheme_toggle_conduct);

  INSTALL_SCM_FUNCTION ("Starts playback and synchronously records from MIDI in. The recording will play back with future play until deleted. The recording is not saved with the score - convert to notation first,", DENEMO_SCHEME_PREFIX "MidiRecord", scheme_midi_record);

  INSTALL_SCM_FUNCTION ("Generates the MIDI timings for the music of the current movement. Returns TRUE if the MIDI was re-computed else FALSE (call was unnecessary).", DENEMO_SCHEME_PREFIX "CreateTimebase", scheme_create_timebase);



  INSTALL_SCM_FUNCTION1 ("Takes and int as MIDI data and simulates a midi event, avoiding capturing of midi by scripts. Value 0 is special and is received by scripts.", DENEMO_SCHEME_PREFIX "PutMidi", scheme_put_midi);
  INSTALL_SCM_FUNCTION1 ("Takes and int as MIDI data and sends it directly to the MIDI out backend", DENEMO_SCHEME_PREFIX "OutputMidi", scheme_output_midi);


  install_scm_function1 (DENEMO_SCHEME_PREFIX "OutputMidiBytes", scheme_output_midi_bytes);
  install_scm_function1 (DENEMO_SCHEME_PREFIX "PlayMidiKey", scheme_play_midikey);
  INSTALL_SCM_FUNCTION1 ("Takes a midi note key and plays it with next rhythm effect", DENEMO_SCHEME_PREFIX "PendingMidi", scheme_pending_midi);
  INSTALL_SCM_FUNCTION4 ("Takes midi key number, volume 0-255, duration in ms and channel 0-15 and plays the note on midi out.", DENEMO_SCHEME_PREFIX "PlayMidiNote", scheme_play_midi_note);

  INSTALL_SCM_FUNCTION1 ("Takes duration and executable scheme script. Executes the passed scheme code after the passed duration milliseconds", DENEMO_SCHEME_PREFIX "OneShotTimer", scheme_one_shot_timer);
  INSTALL_SCM_FUNCTION1 ("Takes a duration and scheme script, starts a timer that tries to execute the script after every duration ms. It returns a timer id which must be passed back to destroy the timer", DENEMO_SCHEME_PREFIX "Timer", scheme_timer);
  INSTALL_SCM_FUNCTION ("Takes a timer id and destroys the timer", DENEMO_SCHEME_PREFIX "KillTimer", scheme_kill_timer);

  INSTALL_SCM_FUNCTION ("Returns #f if the current staff has no figures (or will not print out figured bass. See d-ShowFiguredBass)", DENEMO_SCHEME_PREFIX "HasFigures", scheme_has_figures);

  INSTALL_SCM_FUNCTION2 ("Returns a string for the bass figure for the two MIDI keys passed in", DENEMO_SCHEME_PREFIX "BassFigure", scheme_bass_figure);


  INSTALL_SCM_FUNCTION ("returns #t if the passed list of MIDI keys fails the pitch spellcheck", DENEMO_SCHEME_PREFIX "SpellCheckMidiChord", scheme_spell_check_midi_chord);

  INSTALL_SCM_FUNCTION ("Gets the MIDI key number for the note-position where the cursor is", DENEMO_SCHEME_PREFIX "GetCursorNoteAsMidi", scheme_get_cursor_note_as_midi);
  INSTALL_SCM_FUNCTION ("Returns the MIDI key number for the note at the cursor, or 0 if none", DENEMO_SCHEME_PREFIX "GetNoteAsMidi", scheme_get_note_as_midi);
  INSTALL_SCM_FUNCTION ("Re-draws the Denemo display, which can have side effects on the data", DENEMO_SCHEME_PREFIX "RefreshDisplay", scheme_refresh_display);
  INSTALL_SCM_FUNCTION ("Sets the status of the current musical score to saved, or unsaved if passed #f", DENEMO_SCHEME_PREFIX "SetSaved", scheme_set_saved);
  INSTALL_SCM_FUNCTION ("Gets the saved status of the current musical score", DENEMO_SCHEME_PREFIX "GetSaved", scheme_get_saved);
  INSTALL_SCM_FUNCTION ("Returns #f if mark is not set", DENEMO_SCHEME_PREFIX "MarkStatus", scheme_mark_status);
  INSTALL_SCM_FUNCTION ("Takes a command name and returns the tooltip or #f if none", DENEMO_SCHEME_PREFIX "GetHelp", scheme_get_help);

  INSTALL_SCM_FUNCTION ("Takes a file name, loads keybindings from actions/menus returns #f if it fails", DENEMO_SCHEME_PREFIX "LoadKeybindings", scheme_load_keybindings);

  INSTALL_SCM_FUNCTION ("Takes a file name, saves keybindings from actions/menus returns #f if it fails", DENEMO_SCHEME_PREFIX "SaveKeybindings", scheme_save_keybindings);

  INSTALL_SCM_FUNCTION ("Clears all keybindings returns #t", DENEMO_SCHEME_PREFIX "ClearKeybindings", scheme_clear_keybindings);

  INSTALL_SCM_FUNCTION ("Takes a file name for xml format commandset, loads commands, returns #f if it fails", DENEMO_SCHEME_PREFIX "LoadCommandset", scheme_load_commandset);

  INSTALL_SCM_FUNCTION ("Takes a double or string and scales the display; return #f for invalid value else the value set. With no parameter returns the current value. ", DENEMO_SCHEME_PREFIX "Zoom", scheme_zoom);

  INSTALL_SCM_FUNCTION ("Takes a double or string and scales the tempo; returns the tempo set. With no parameter returns the current master tempo ", DENEMO_SCHEME_PREFIX "MasterTempo", scheme_master_tempo);

  INSTALL_SCM_FUNCTION ("Takes an integer or string number of beats (quarter notes) per minute as the tempo for the current movement; returns the tempo set ", DENEMO_SCHEME_PREFIX "MovementTempo", scheme_movement_tempo);


  INSTALL_SCM_FUNCTION ("Takes a double or string and scales the volume; returns the volume set ", DENEMO_SCHEME_PREFIX "MasterVolume", scheme_master_volume);
  INSTALL_SCM_FUNCTION ("Takes a double 0-1 and sets the staff master volume for the current staff, returns the value. With no (or bad) parameter returns the current value.", DENEMO_SCHEME_PREFIX "StaffMasterVolume", scheme_staff_master_volume);

  INSTALL_SCM_FUNCTION ("Takes a integer sets the enharmonic range to use 0 = E-flat to G-sharp ", DENEMO_SCHEME_PREFIX "SetEnharmonicPosition", scheme_set_enharmonic_position);


  INSTALL_SCM_FUNCTION ("Return a string of tuning bytes (offsets from 64) for MIDI tuning message", DENEMO_SCHEME_PREFIX "GetMidiTuning", scheme_get_midi_tuning);
  INSTALL_SCM_FUNCTION ("Return name of flattest degree of current temperament", DENEMO_SCHEME_PREFIX "GetFlattest", scheme_get_flattest);

  INSTALL_SCM_FUNCTION ("Return name of sharpest degree of current temperament", DENEMO_SCHEME_PREFIX "GetSharpest", scheme_get_sharpest);
  INSTALL_SCM_FUNCTION ("Return name of current temperament", DENEMO_SCHEME_PREFIX "GetTemperament", scheme_get_temperament);

  INSTALL_SCM_FUNCTION ("Rewind the MIDI generated for the current movement. Given a time in seconds it tries to rewind to there.", DENEMO_SCHEME_PREFIX "RewindMidi", scheme_rewind_midi);
  INSTALL_SCM_FUNCTION ("Takes an interval, returns a pair, a list of the next note-on events that occur within that interval and the time of these events.", DENEMO_SCHEME_PREFIX "NextMidiNotes", scheme_next_midi_notes);

  INSTALL_SCM_FUNCTION ("Restart midi play, cancelling any pause", DENEMO_SCHEME_PREFIX "RestartPlay", scheme_restart_play);
  INSTALL_SCM_FUNCTION ("Return a number, the midi time in seconds for the start of the object at the cursor; return #f if none ", DENEMO_SCHEME_PREFIX "GetMidiOnTime", scheme_get_midi_on_time);
  INSTALL_SCM_FUNCTION ("Return a number, the midi time in seconds for the end of the object at the cursor; return #f if none ", DENEMO_SCHEME_PREFIX "GetMidiOffTime", scheme_get_midi_off_time);

  INSTALL_SCM_FUNCTION2 ("Set start and/or end time for playback to the passed numbers/strings in seconds. Use #t if a value is not to be changed. Returns #f for bad parameters ", DENEMO_SCHEME_PREFIX "SetPlaybackInterval", scheme_set_playback_interval);

  INSTALL_SCM_FUNCTION ("Adjust start time for playback by passed number of seconds. Returns #f for bad parameter ", DENEMO_SCHEME_PREFIX "AdjustPlaybackStart", scheme_adjust_playback_start);

  INSTALL_SCM_FUNCTION ("Adjust end time for playback by passed number of seconds. Returns #f for bad parameter ", DENEMO_SCHEME_PREFIX "AdjustPlaybackEnd", scheme_adjust_playback_end);
#ifdef _WITH_X11_
  INSTALL_SCM_FUNCTION1 ("Takes a parameter #t or #f and optional position: Get a screenshot from the user and append or insert it in a list (one per measure) either applying across the staffs or to the current staff.", DENEMO_SCHEME_PREFIX "UserScreenshot", scheme_user_screenshot);
  INSTALL_SCM_FUNCTION ("Takes a parameter #t or #f: Delete a screenshot for the current measure, either across staffs or for current staff.", DENEMO_SCHEME_PREFIX "DeleteScreenshot", scheme_delete_screenshot);
#endif /*_WITH_X11_*/
  INSTALL_SCM_FUNCTION ("Pushes the Denemo clipboard (cut/copy buffer) onto a stack; Use d-PopClipboard to retrieve it.", DENEMO_SCHEME_PREFIX "PushClipboard", scheme_push_clipboard);

  INSTALL_SCM_FUNCTION ("Pops the Denemo clipboard (cut/copy buffer) from a stack created by d-PushClipboard. Returs #f if nothing on stack, else #t.", DENEMO_SCHEME_PREFIX "PopClipboard", scheme_pop_clipboard);

  INSTALL_SCM_FUNCTION ("Deletes all objects in the selection Returns #f if no selection else #t.", DENEMO_SCHEME_PREFIX "DeleteSelection", scheme_delete_selection);

  INSTALL_SCM_FUNCTION ("Sets the selection to be used for a thumbnail. Returns #f if no selection or selection not in first movement else #t.", DENEMO_SCHEME_PREFIX "SetThumbnailSelection", scheme_set_thumbnail_selection);

  INSTALL_SCM_FUNCTION ("Creates a thumbnail for the current score. With no argument it waits for the thumbnail to complete, freezing any display. With #t it generates the thumbnail asynchrously. It does not report on completion.", DENEMO_SCHEME_PREFIX "CreateThumbnail", scheme_create_thumbnail);

  INSTALL_SCM_FUNCTION ("Exits Denemo without saving history, prefs etc.", DENEMO_SCHEME_PREFIX "Exit", scheme_exit);

  INSTALL_SCM_FUNCTION ("Snapshots the current movement putting it in the undo queue returns #f if no snapshot was taken because of a guard", DENEMO_SCHEME_PREFIX "TakeSnapshot", scheme_take_snapshot);

  INSTALL_SCM_FUNCTION ("Creates the default layout.", DENEMO_SCHEME_PREFIX "SelectDefaultLayout", scheme_select_default_layout);
  INSTALL_SCM_FUNCTION1 ("Creates a custom layout from the currently selected (standard) layout if the score layouts window is open. Uses the passed name for the new layout. Returns #f if nothing happened. An additional parameter #t can force creation of the layout while score layout window is closed.", DENEMO_SCHEME_PREFIX "CreateLayout", scheme_create_layout);
  INSTALL_SCM_FUNCTION ("Returns the id of the currently selected score layout (see View->Score Layout). Returns #f if no layout is selected.", DENEMO_SCHEME_PREFIX "GetLayoutId", scheme_get_layout_id);
  INSTALL_SCM_FUNCTION ("Returns the id of a score layout for typesetting the part for the current staff. Returns #f if not a primary voice.", DENEMO_SCHEME_PREFIX "GetCurrentStaffLayoutId", scheme_get_current_staff_layout_id);
  INSTALL_SCM_FUNCTION ("Selects the score layout with the passed id. Returns #f if there is no such layout.", DENEMO_SCHEME_PREFIX "SelectLayoutId", scheme_select_layout_id);
  INSTALL_SCM_FUNCTION ("Generates LilyPond layout for the current part (ie staffs with the name of the staff with the cursor), all movements and staffs with that staff name are generated.", DENEMO_SCHEME_PREFIX "LilyPondForPart", scheme_lilypond_for_part);
  INSTALL_SCM_FUNCTION ("Typesets the current part (ie the staff with the cursor), all movements and staffs with that staff name are typeset.", DENEMO_SCHEME_PREFIX "TypesetPart", scheme_typeset_part);
  INSTALL_SCM_FUNCTION ("Converts the current score layout to editable LilyPond text. After this the score layout is only affected by editing the LilyPond syntax.", DENEMO_SCHEME_PREFIX "ReduceLayoutToLilyPond", scheme_reduce_layout_to_lilypond);
  INSTALL_SCM_FUNCTION ("Returns the name of the currently selected score layout (see View->Score Layout). Returns #f if no layout is selected.", DENEMO_SCHEME_PREFIX "GetLayoutName", scheme_get_layout_name);
  INSTALL_SCM_FUNCTION ("Selects the next score layout. If the current layout is the last, returns #f otherwise #t.", DENEMO_SCHEME_PREFIX "SelectNextLayout", scheme_select_next_layout);
  INSTALL_SCM_FUNCTION ("Selects the first score layout.", DENEMO_SCHEME_PREFIX "SelectFirstLayout", scheme_select_first_layout);
  INSTALL_SCM_FUNCTION ("Selects the next custom score layout. If the current layout is the last, returns #f otherwise #t.", DENEMO_SCHEME_PREFIX "SelectNextCustomLayout", scheme_select_next_custom_layout);
  INSTALL_SCM_FUNCTION ("Selects the first custom score layout.", DENEMO_SCHEME_PREFIX "SelectFirstCustomLayout", scheme_select_first_custom_layout);


  INSTALL_SCM_FUNCTION ("Follows a link to a source file of form string \"filename:x:y:page\". It opens the file and places a marker there. ", DENEMO_SCHEME_PREFIX "OpenSource", scheme_open_source);

  INSTALL_SCM_FUNCTION ("Converts the recorded audio to user chosen audio file.", DENEMO_SCHEME_PREFIX "ExportRecordedAudio", scheme_export_recorded_audio);
  INSTALL_SCM_FUNCTION ("Opens a source file for transcribing from. Links to this source file can be placed by shift-clicking on its contents", DENEMO_SCHEME_PREFIX "OpenSourceFile", scheme_open_source_file);

  INSTALL_SCM_FUNCTION ("Opens a source audio file for transcribing from. ", DENEMO_SCHEME_PREFIX "OpenSourceAudioFile", scheme_open_source_audio_file);
  INSTALL_SCM_FUNCTION ("Closes a source audio attached to the current movement.", DENEMO_SCHEME_PREFIX "CloseSourceAudio", scheme_close_source_audio);

  INSTALL_SCM_FUNCTION ("Plays audio allowing timings to be entered via keypresses if passed #t as parameter. ", DENEMO_SCHEME_PREFIX "StartAudioPlay", scheme_start_audio_play);
  INSTALL_SCM_FUNCTION ("Stops audio playback", DENEMO_SCHEME_PREFIX "StopAudioPlay", scheme_stop_audio_play);
  INSTALL_SCM_FUNCTION ("Takes a number of seconds to be used as lead-in for audio. If negative clips that much from the start of the audio. ", DENEMO_SCHEME_PREFIX "SetAudioLeadIn", scheme_set_audio_lead_in);
  INSTALL_SCM_FUNCTION ("returns #f if audio is not playing else #t", DENEMO_SCHEME_PREFIX "AudioIsPlaying", scheme_audio_is_playing);

  INSTALL_SCM_FUNCTION ("Returns the next in the list of timings registered by the user during audio play.", DENEMO_SCHEME_PREFIX "NextAudioTiming", scheme_next_audio_timing);





  INSTALL_SCM_FUNCTION ("Stop collecting undo information. Call DecreaseGuard when finished. Returns #f if already guarded, #t if this call is stopping the undo collection", DENEMO_SCHEME_PREFIX "IncreaseGuard", scheme_increase_guard);

  INSTALL_SCM_FUNCTION ("Drop one guard against collecting undo information. Returns #t if there are no more guards \n(undo information will be collected) \nor #f if there are still guards in place.", DENEMO_SCHEME_PREFIX "DecreaseGuard", scheme_decrease_guard);

  INSTALL_SCM_FUNCTION ("Undoes the actions performed by the script so far, starts another undo stage for the subsequent actions of the script. Note this command has the same name as the built-in Undo command, to override it when called from a script. Returns #t", DENEMO_SCHEME_PREFIX "Undo" /*sic */ , scheme_undo);
  INSTALL_SCM_FUNCTION ("Creates a new tab. Note this command has the same name as the built-in NewWindow command, to override it when called from a script. Returns #t", DENEMO_SCHEME_PREFIX "NewWindow" /*sic */ , scheme_new_window);

  INSTALL_SCM_FUNCTION ("Undo normally undoes all the actions performed by a script. This puts a stage at the point in a script where it is called, so that a user-invoked undo will stop at this point, continuing when a further undo is invoked. Returns #t", DENEMO_SCHEME_PREFIX "StageForUndo", scheme_stage_for_undo);

  INSTALL_SCM_FUNCTION ("return a string giving the latest step available for Undo", DENEMO_SCHEME_PREFIX "GetLastChange", scheme_get_last_change);

  INSTALL_SCM_FUNCTION ("Takes a command name and returns the menu path to that command or #f if none", DENEMO_SCHEME_PREFIX "GetMenuPath", scheme_get_menu_path);
  INSTALL_SCM_FUNCTION ("Takes a string and returns a string representing an MD5 checksum for the passed string.", DENEMO_SCHEME_PREFIX "GetChecksum", scheme_get_checksum);
  INSTALL_SCM_FUNCTION ("Sets the newbie status to the passed value", DENEMO_SCHEME_PREFIX "SetNewbie", scheme_set_newbie);
  INSTALL_SCM_FUNCTION ("Gets the current verse of the current staff or #f if none, with an integer parameter, gets the nth verse", DENEMO_SCHEME_PREFIX "GetVerse", scheme_get_verse);
  INSTALL_SCM_FUNCTION ("Puts the passed string as the current verse of the current staff", DENEMO_SCHEME_PREFIX "PutVerse", scheme_put_verse);
  INSTALL_SCM_FUNCTION ("Appends the passed string to the current verse of the current staff", DENEMO_SCHEME_PREFIX "AppendToVerse", scheme_append_to_verse);


  INSTALL_SCM_FUNCTION ("Takes a command name and returns and id for it or #f if no command of that name exists", DENEMO_SCHEME_PREFIX "GetId", scheme_get_id);

  INSTALL_SCM_FUNCTION2 ("Takes a command name or command id and binding name and sets that binding on that command returns the command id that previously had the binding or #f if none", DENEMO_SCHEME_PREFIX "AddKeybinding", scheme_add_keybinding);

  INSTALL_SCM_FUNCTION ("Takes a command name and returns the label for the menu item that executes the command or #f if none", DENEMO_SCHEME_PREFIX "GetLabel", scheme_get_label);


  INSTALL_SCM_FUNCTION ("Returns the installed LilyPond version", DENEMO_SCHEME_PREFIX "GetLilyVersion", scheme_get_lily_version);
  INSTALL_SCM_FUNCTION ("Returns a boolean if the installed version of LilyPond is greater than or equal to the passed in version string", DENEMO_SCHEME_PREFIX "CheckLilyVersion", scheme_check_lily_version);



  INSTALL_SCM_FUNCTION ("Takes a string putting it on the scheme-controlled status bar as a list of active filters", DENEMO_SCHEME_PREFIX "InputFilterNames", scheme_input_filter_names);

  INSTALL_SCM_FUNCTION ("Takes a string putting the scheme controlled status bar; with no argument it hides this  status bar", DENEMO_SCHEME_PREFIX "WriteStatus", scheme_write_status);

}

static gboolean
load_files(gchar** files)
{
  gboolean ret = FALSE;
  gint i = 0;
  
  if(!files){
    newtab (NULL, NULL);
    open_for_real (get_most_recent_file (), Denemo.gui, FALSE, REPLACE_SCORE);
    return TRUE;
  }

  for(i=0; files[i]; i++)
    {
      newtab (NULL, NULL);
      open_for_real (files[i], Denemo.gui, FALSE, REPLACE_SCORE);
      ret = TRUE;
    }
  return ret;
}

static void
crash_recovery_check()
{
  gchar *crash_file = g_build_filename (locatedotdenemo (), "crashrecovery.denemo", NULL);
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
          open_for_real (crash_file, Denemo.gui, TRUE, REPLACE_SCORE);
          score_status (Denemo.gui, TRUE);
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

  /* Initialize preferences */
  initprefs ();

  initialize_keystroke_help ();

  // initialize the audio subsystem
  if (audio_initialize (&Denemo.prefs))
      g_error ("Failed to initialize audio or MIDI backends\n");

  create_window ();

  create_scheme_identfiers ();

  if (Denemo.prefs.tooltip_timeout)
    {
      g_object_set (gtk_widget_get_settings (Denemo.window), "gtk-tooltip-timeout", Denemo.prefs.tooltip_timeout, NULL);
      g_object_set (gtk_widget_get_settings (Denemo.window), "gtk-tooltip-browse-timeout", Denemo.prefs.tooltip_browse_timeout, NULL);
      g_object_set (gtk_widget_get_settings (Denemo.window), "gtk-tooltip-browse-mode-timeout", Denemo.prefs.tooltip_browse_mode_timeout, NULL);
    }

  /*ignore setting of mode unless user has explicitly asked for modal use */
  if (!Denemo.prefs.modal)
    Denemo.prefs.mode = INPUTEDIT | INPUTRHYTHM | INPUTNORMAL;  //FIXME must correspond with default in prefops.c

  if (Denemo.prefs.autoupdate)
    fetchcommands (NULL, NULL);

  gint i;
  /* create scheme identifiers for check/radio item to activate the items (ie not just run the callback) */
  for (i = 0; i < G_N_ELEMENTS (activatable_commands); i++)
    {
      gchar* function = g_strdup_printf (DENEMO_SCHEME_PREFIX "%s", activatable_commands[i].str);
      install_scm_function (function, activatable_commands[i].p);
      g_free(function);
    }

  //ensure (use-modules (ice-9 optargs)) is loaded first #:optional params
  call_out_to_guile ("(use-modules (ice-9 optargs))");
  init_keymap ();

  load_default_keymap_file ();

  Denemo.accelerator_status = FALSE;

  define_scheme_constants ();

  readHistory ();
  populate_opened_recent_menu ();

  load_scheme_init ();

  gboolean file_loaded = load_files(files);

  load_preferences ();

  if (!file_loaded && !Denemo.scheme_commands)
    {
      gchar* code = g_strdup_printf("(d-InstrumentName \"%s\")", _("Unnamed"));
      call_out_to_guile (code);
      g_free(code);
      denemo_scheme_init ();
    }

  gtk_key_snooper_install ((GtkKeySnoopFunc) dnm_key_snooper, NULL);
  
  crash_recovery_check();
  
  score_status (Denemo.gui, FALSE);
  
  if (Denemo.scheme_commands)
    call_out_to_guile (Denemo.scheme_commands);

  if (Denemo.prefs.fontspec->len)
    {
      GtkSettings *settings = gtk_settings_get_default ();
      gtk_settings_set_string_property (settings, "gtk-font-name", Denemo.prefs.fontspec->str, "denemo");
    }

  gtk_main ();
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
  g_print ("String is %s\n", str->str);
  return str;
}

static gboolean
action_callbacks (DenemoGUI * gui)
{
  GList *callbacks = gui->callbacks;
  if (callbacks == NULL)
    return FALSE;
  gui->callbacks = NULL;        //do this before calling the callbacks, so they cannot run twice
  for (; callbacks; callbacks = g_list_delete_link (callbacks, callbacks))
    {
      call_out_to_guile (callbacks->data);
      g_free (callbacks->data);
    }
  return TRUE;
}



/**
 * Close the current musical score (Denemo.gui) freeing all its movements (DenemoScore), releasing its memory and removing it from the global list Denemo.guis
 * Do not close the sequencer
 */
static gboolean
close_gui (void)
{
  g_signal_handlers_block_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_draw_event), NULL);       // turn of refresh of display before destroying the data
  stop_midi_playback (NULL, NULL);      // if you do not do this, there is a timer moving the score on which will hang
  //FIXME why was this here??? activate_action("/MainMenu/InputMenu/KeyboardOnly");
#ifdef USE_EVINCE  
  if (Denemo.prefs.enable_thumbnails)
    create_thumbnail (TRUE);
#endif
  if (Denemo.autosaveid)
    {
      if (g_list_length (Denemo.guis) > 1)
        g_print ("Auto save being turned off");
      g_source_remove (Denemo.autosaveid);
      Denemo.autosaveid = 0;
    }
  if (Denemo.textwindow && gtk_widget_get_visible (Denemo.textwindow))
    {
      activate_action ("/MainMenu/ViewMenu/" ToggleLilyText_STRING);
      //FIXME there is a handler in exportlilypond.c for the delete signal. It would need to be disabled to get the memory freed.
    }
  free_movements (Denemo.gui);

  DenemoGUI *oldgui = Denemo.gui;
  //gtk_widget_destroy (Denemo.page);  //note switch_page from g_signal_connect (G_OBJECT(Denemo.notebook), "switch_page", G_CALLBACK(switch_page), NULL);
  gint index = g_list_index (Denemo.guis, oldgui);
  gtk_notebook_remove_page (GTK_NOTEBOOK (Denemo.notebook), index);
  g_print ("Removed %d\n", index);
  Denemo.guis = g_list_remove (Denemo.guis, oldgui);    //FIXME ?? or in the destroy callback??
  g_free (oldgui);
  if (Denemo.guis)
    {
      if (index > g_list_length (Denemo.guis) - 1)
        index = g_list_length (Denemo.guis) - 1;
      if (index < 0)
        index = 0;

      Denemo.gui = g_list_nth_data (Denemo.guis, index);
      //g_print("Setting the first piece as your score\n");
      gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), index);
    }
  else
    Denemo.gui = NULL;
  g_signal_handlers_unblock_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_draw_event), NULL);
  return TRUE;
}

/* remove all the movements (ie the DenemoScore) leaving it with gui->si NULL */
void
free_movements (DenemoGUI * gui)
{
  GList *g;
  free_scoreblocks (gui);
  for (g = gui->movements; g; g = g->next)
    {
      gui->si = g->data;
      gui->si->undo_guard = 1;  //no undo as that is per movement
      free_score (gui);
    }
  gui->si = NULL;
  delete_directives (&gui->lilycontrol.directives);
  delete_directives (&gui->scoreheader.directives);
  delete_directives (&gui->paper.directives);
  g_list_free (gui->movements);
  gui->movements = NULL;


  /* any other free/initializations */

}

/**
* Wrapper function to close application when the quit
* menu item has been used
* 
*
*/
static void
closewrapper (GtkAction * action, gpointer param)
{
  GList *display;

  if (Denemo.accelerator_status)
    {
      if (confirm (_("You have made changes to the commands you have"), _("Do you want to save the changes?")))
        save_accels ();
    }
  for (display = Denemo.guis; display != NULL; display = g_list_next (display))
    {
      Denemo.gui = (DenemoGUI *) display->data;
      if (close_gui_with_check (NULL, NULL) == FALSE)
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
fetchcommands (GtkAction * action, gpointer param)
{
  static gchar *location = NULL;
  location = g_build_filename (locatedotdenemo (), "download", "actions", NULL);
  gboolean err = g_mkdir_with_parents (location, 0770);
  if (err)
    {
      gchar *message = g_strdup_printf (_("Could not make folder %s for the downloaded commands"), location);
      warningdialog (message);
      g_free (message);
      return;
    }

  g_print ("location is %s\n", location);
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
static void
morecommands (GtkAction * action, gpointer param)
{
  static gchar *location = NULL;
  location = g_build_filename (locatedotdenemo (), "download", "actions", "menus", NULL);
  if (!g_file_test (location, G_FILE_TEST_EXISTS))
    {
      g_free (location);
      location = NULL;
    }
  if (location == NULL)
    location = g_build_filename (get_data_dir (), "actions", "menus", NULL);
  load_keymap_dialog_location (NULL, location);
  //#define WARNING_NEW_MENUS "Note: if you load a command that creates a new menu\nSome of the new commands may not work until you have exited\nand re-started denemo"
  //warningdialog(WARNING_NEW_MENUS);
  if (Denemo.last_merged_command && g_str_has_prefix (Denemo.last_merged_command, get_data_dir ()))
    {
      g_free (location);
      location = g_strdup (Denemo.last_merged_command); //FIXME
    }
}

/**
 * callback to load local extra commands
 * 
 */
static void
mycommands (GtkAction * action, gpointer param)
{
  static gchar *location = NULL;
  if (location == NULL)
    location = g_build_filename (locatedotdenemo (), "actions", "menus", NULL);

  if (Denemo.last_merged_command && g_str_has_prefix (Denemo.last_merged_command, locatedotdenemo ()))
    {
      g_free (location);
      location = g_path_get_dirname (Denemo.last_merged_command);
    }
  load_keymap_dialog_location (NULL, location);
  // warningdialog(WARNING_NEW_MENUS);
  //g_print("The last was %s %s %s\n", Denemo.last_merged_command, location,  locatedotdenemo());
}



/**
 * Open in New Window callback 
 * Creates new view then opens file in the view
 */
void
openinnew (GtkAction * action, DenemoScriptParam * param)
{
  newtab (NULL, param);
  file_open_with_check (NULL, param);
  if (param && (param->status == FALSE))
    close_gui ();
  set_title_bar (Denemo.gui);
}


/**
 * Close callback 
 * if user confirms close the current gui
 * if it is the last close the application.
 * return FALSE if gui was not closed, else TRUE
 */
gboolean
close_gui_with_check (GtkAction * action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  Denemo.prefs.mode = Denemo.gui->mode;
  if (action_callbacks (Denemo.gui))
    return FALSE;               //Denemo.gui may have been closed, depends on script callbacks;

  //do not ask for confirm if scripted FIXME
  if ((!gui->notsaved) || (gui->notsaved && confirmbox (gui)))
    close_gui ();
  else
    return FALSE;
  if (Denemo.guis == NULL)
    {
      storeWindowState ();
      writeHistory ();
      writeXMLPrefs (&Denemo.prefs);
#ifdef G_OS_WIN32
      CoUninitialize ();
      g_print ("\n\n\nWindows - Exiting without shutting down audio\n\n\n");
      if (gui->input_source == INPUTMIDI)
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
  DenemoGUI *gui = Denemo.gui;
#define CURRP ((RhythmPattern *)gui->currhythm->data)
  if (gui->currhythm && CURRP)
    unhighlight_rhythm (CURRP);
  gui->currhythm = NULL;

  gui->rstep = r->rsteps;
  gui->cstep = NULL;

#define g (gui->rstep)
#define MODE (gui->mode)
  unhighlight_rhythm (gui->prevailing_rhythm);
  gui->prevailing_rhythm = r;
  highlight_rhythm (r);
  if ((MODE & (INPUTEDIT | INPUTRHYTHM)))
    {
      gint save = MODE;
      MODE = INPUTINSERT | INPUTNORMAL;
      ((GSourceFunc) (((RhythmElement *) g->data)->functions->data)) (gui);
      displayhelper (gui);
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
  gtk_widget_queue_draw (Denemo.scorearea);
  draw_score (NULL);
}

static void
pb_end_to_cursor (GtkWidget * button)
{
  call_out_to_guile ("(DenemoSetPlaybackEnd)");
  //gtk_widget_draw(Denemo.scorearea, NULL);
  gtk_widget_queue_draw (Denemo.scorearea);
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
  tempo = (Denemo.gui->si->tempo > 0) ? bpm / Denemo.gui->si->tempo : 1.0;
  scm_c_define ("DenemoTempo::Value", scm_from_double (tempo));
  call_out_to_guile ("(DenemoTempo)");

}

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
  if (Denemo.gui->si->audio)
    {
      Denemo.gui->si->audio->volume = gtk_adjustment_get_value (adjustment);
    }
}

static void
audio_volume_boost (GtkAdjustment * adjustment)
{
  if (Denemo.gui->si->audio)
    {
      Denemo.gui->si->audio->volume = gtk_adjustment_get_value (adjustment);
    }
}

static void
leadin_changed (GtkSpinButton * spin)
{
  if (Denemo.gui->si->audio)
    {
      set_lead_in (gtk_spin_button_get_value (spin));
      //g_print("%d for %f\n", Denemo.gui->si->audio->leadin, gtk_spin_button_get_value(spin));
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
  Denemo.gui->si->start_time = 0.0;
  Denemo.gui->si->end_time = -1.0;      //ie unset
  set_start_and_end_objects_for_draw ();
  reset_temperament ();
}

static void
track_delete (smf_track_t * track)
{
  if (track == NULL)
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
set_midi_in_status ()
{
  if (midi_in_status)
    {
      gchar *text = NULL;
      if ((Denemo.gui->midi_destination & MIDIRECORD) && (Denemo.gui->midi_destination & MIDIPLAYALONG))
        text = _("<span foreground=\"blue\">" "Recording + Play Along" "</span>");
      else if (Denemo.gui->midi_destination & MIDIRECORD)
        text = _("<span foreground=\"red\">" "Recording" "</span>");
      else if (Denemo.gui->midi_destination & MIDIPLAYALONG)
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
finish_recording (void)
{
  if ((Denemo.gui->midi_destination & MIDIRECORD))
    {
      Denemo.gui->midi_destination ^= MIDIRECORD;
      g_print ("Showing");
      gtk_widget_show (deletebutton);
      gtk_widget_show (convertbutton);
      set_midi_in_status ();
    }
}

static void
pb_conduct (GtkWidget * button)
{
  Denemo.gui->midi_destination ^= MIDICONDUCT;
  if (Denemo.gui->midi_destination & MIDICONDUCT)
    gtk_button_set_label (GTK_BUTTON (button), _("Mouse Conductor ON"));
  else
    gtk_button_set_label (GTK_BUTTON (button), _("Mouse Conductor OFF"));
}



static void
pb_playalong (GtkWidget * button)
{
  Denemo.gui->midi_destination ^= MIDIPLAYALONG;
  if (Denemo.gui->midi_destination & MIDIPLAYALONG)
    gtk_button_set_label (GTK_BUTTON (button), _("Switch to Normal Playback"));
  else
    gtk_button_set_label (GTK_BUTTON (button), _("Switch to Play Along Playback"));
  set_midi_in_status ();
}

static void
pb_record (GtkWidget * button)
{
  if (Denemo.gui->si->recorded_midi_track && !confirm (_("MIDI Recording"), _("Delete last recording?")))
    {
      return;
    }
  // if(!(Denemo.gui->midi_destination & MIDITHRU))
  //  pb_midi_thru(midithrubutton);
  Denemo.gui->midi_destination |= MIDIRECORD;
  track_delete (Denemo.gui->si->recorded_midi_track);
  Denemo.gui->si->recorded_midi_track = smf_track_new ();
  gtk_widget_hide (deletebutton);
  gtk_widget_hide (convertbutton);
  set_midi_in_status ();
  pb_play (playbutton);
  return;
}

static void
pb_audiorecord (GtkWidget * button)
{
  gtk_button_set_image (GTK_BUTTON (audiorecordbutton),
    gtk_image_new_from_stock (GTK_STOCK_MEDIA_RECORD, GTK_ICON_SIZE_BUTTON));//highlighting may have turned it off
  if (Denemo.prefs.maxrecordingtime)
  {
    Denemo.gui->audio_recording = !Denemo.gui->audio_recording;
    if(!Denemo.gui->audio_recording) gtk_widget_show(exportbutton);
  }
  else
  {
    warningdialog (_("The preference set for recording time is 0 - nothing can be recorded.\nSee Edit → Change Preferences Audio/Midi Tab"));
  }
}
static void
pb_exportaudio (GtkWidget * button)
{
  if(!Denemo.gui->audio_recording)
    Denemo.gui->audio_recording = FALSE;
  export_recorded_audio ();
}

void highlight_audio_record(void) {
  static gboolean on;
  on = !on;
  gtk_button_set_image (GTK_BUTTON (audiorecordbutton),
    gtk_image_new_from_stock (on?GTK_STOCK_MEDIA_RECORD:GTK_STOCK_MEDIA_STOP, GTK_ICON_SIZE_BUTTON));
}
static void
pb_midi_delete (GtkWidget * button)
{
  track_delete (Denemo.gui->si->recorded_midi_track);
  Denemo.gui->si->recorded_midi_track = NULL;
  gtk_widget_hide (convertbutton);
  gtk_widget_hide (button);
}

static void
pb_midi_convert (GtkWidget * button)
{

  call_out_to_guile ("(DenemoConvert)");

  g_print ("Finished midi convert\n");
}




/**
 * Rhythm callback select rhythm
 * inserts the rhythm if pitchless
 */
static void
select_rhythm_pattern (RhythmPattern * r)
{
  DenemoGUI *gui = Denemo.gui;
#define CURRP ((RhythmPattern *)gui->currhythm->data)

  if (gui->currhythm && (CURRP != r))
    {                           //Change the highlighting
      if (CURRP)
        unhighlight_rhythm (CURRP);
      else if (gui->rstep)
        unhighlight_rhythm (((RhythmElement *) gui->rstep->data)->rhythm_pattern);
    }

  gui->currhythm = g_list_find (gui->rhythms, r);
  if (gui->currhythm != NULL)
    {
      gui->rstep = r->rsteps;
      gui->cstep = r->clipboard->data;

      gchar *text = ((RhythmElement *) gui->rstep->data)->icon;
      if (text)
        {
          GtkWidget *label = LABEL (CURRP->button);
          //g_print("markup is %s\n", ((RhythmElement*)g->data)->icon);
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
  if ((Denemo.gui->mode & INPUTEDIT))
    {
      if (Denemo.gui->input_source == INPUTMIDI)
        {
          insert_note_following_pattern (Denemo.gui);
          ((DenemoObject *) Denemo.gui->si->currentobject->data)->isinvisible = TRUE;
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
static gboolean
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
          //g_print("have %x type %d\n", g->data, ((DenemoObject*)g->data)->type);
          if ((((DenemoObject *) g->data)->type == MEASUREBREAK) || (((DenemoObject *) g->data)->type == STAFFBREAK))
            g = clip->data = g_list_delete_link (clip->data, g);        //we search from the start again, as g has been freed
        }
    }
}

static void
attach_clipboard (RhythmPattern * r)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoScore *si = gui->si;
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


static gint
insert_pattern_in_toolbar (RhythmPattern * r)
{
  DenemoGUI *gui = Denemo.gui;
  if (r->clipboard == NULL)
    {
      g_warning ("No clipboard for this pattern, cannot add\n");
      return -1;
    }
  GtkWidget *toolbar = gtk_ui_manager_get_widget (Denemo.ui_manager, "/RhythmToolBar");
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), GTK_TOOL_ITEM (r->button), -1);
  gtk_widget_show_all (GTK_WIDGET (r->button));
  gui->rstep = r->rsteps;
  gui->cstep = r->clipboard->data;
  gui->rhythms = g_list_append (gui->rhythms, r);

  if (gui->currhythm)
    unhighlight_rhythm ((RhythmPattern *) gui->currhythm->data);
  gui->currhythm = g_list_last (gui->rhythms);
  highlight_rhythm ((RhythmPattern *) gui->currhythm->data);
  g_signal_connect (G_OBJECT (r->button), "clicked", G_CALLBACK (activate_rhythm_pattern), (gpointer) r);
  return g_list_length (gui->rhythms);  //the index of the newly added snippet
}

static void
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
        the rhythm is put in gui->
        a button is created in "/RhythmToolbar"
        and the pattern is added to gui->rhythms 
         with the first step of it put in gui->rstep
	add a clipboard with the selected music to the created rhythm pattern.
   if ACTION is one of the insert_chord_xkey insert_rest_xkey)
   functions
        a button is created in the /EntryToolbar (if not already present)
   

*/
static void
create_rhythm_cb (GtkAction * action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  gboolean singleton = FALSE;   // set TRUE if action is one of the insert_... functions.
  gboolean already_done = FALSE;        // a singleton which has already been installed globally
  gboolean default_rhythm = FALSE;
  DenemoScore *si = gui->si;
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

      if (gui->lilysync != gui->changecount)
        refresh_lily_cb (NULL, Denemo.gui);

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
                      //g_warning("ignoring %d\n", obj->type);
                      break;
                    }           /* end of switch obj type */
                  //g_print("Number of rhythms %d\n", g_list_length(r->rsteps));
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
      //g_print("rsteps is %p entry is %s, %s\n", r->rsteps, pattern, labelstr);
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
          //g_print("el->icon = %s step %d pattern %s\n", el->icon, i, pattern);
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
          if (!already_done)
            {                   //When creating first gui only
              GtkWidget *toolbar = gtk_ui_manager_get_widget (Denemo.ui_manager, "/EntryToolBar");
              gtk_toolbar_insert (GTK_TOOLBAR (toolbar), GTK_TOOL_ITEM (r->button), -1);
              gtk_widget_show_all (GTK_WIDGET (r->button));
              /* gui->rstep = r->rsteps; */
              g_signal_connect (G_OBJECT (r->button), "clicked", G_CALLBACK (singleton_callback), (gpointer) r);
              unhighlight_rhythm (r);
            }
          if (default_rhythm)
            {
              gui->prevailing_rhythm = r;
              gui->rstep = r->rsteps;
              gui->cstep = NULL;
              highlight_rhythm (r);
              //g_print("prevailing rhythm is %p\n",r);
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
//  g_print("Recording was %d\n", *record);
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
  g_print ("Calling the file loader with %s\n", location);
  load_keymap_dialog_location (w, location);
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
  DenemoGUI *gui = Denemo.gui;
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
              stage_undo (gui->si, ACTION_STAGE_END);   //undo is a queue so this is the end :)
              ret = (gboolean) ! call_out_to_guile (text);
              stage_undo (gui->si, ACTION_STAGE_START);
            }
          else
            {
              g_warning ("Could not get script for %s\n", gtk_action_get_name (action));
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
      //g_print("the scheme is \n%s\n", scheme);
      if (!call_out_to_guile (scheme))
        append_to_local_scheme_init (scheme);
      else
        warningdialog (_("Could not create button"));
      g_free (scheme);
    }
}

/* gets a name label and tooltip from the user, then creates a menuitem in the menu 
   given by the path myposition whose callback is the activate on the current scheme script.
*/

static void
insertScript (GtkWidget * widget, gchar * insertion_point)
{
  DenemoGUI *gui = Denemo.gui;
  gchar *myname, *mylabel, *myscheme, *mytooltip, *submenu;
  gchar *myposition = g_path_get_dirname (insertion_point);
  gchar *after = g_path_get_basename (insertion_point);
  gint idx = lookup_command_from_name (Denemo.map, after);
  myname = string_dialog_entry (gui, "Create a new menu item", "Give item name (avoid clashes): ", "MyName");
  //FIXME check for name clashes

  if (myname == NULL)
    return;
  subst_illegals (myname);
  mylabel = string_dialog_entry (gui, _("Create a new menu item"), _("Give menu label: "), _("My Label"));
  if (mylabel == NULL)
    return;

  mytooltip = string_dialog_entry (gui, _("Create a new menu item"), _("Give explanation of what it does: "), _("Prints my special effect"));
  if (mytooltip == NULL)
    return;
  if (confirm (_("Create a new menu item"), _("Do you want the new menu item in a submenu?")))
    {
      submenu = string_dialog_entry (gui, _("Create a new menu item"), _("Give a label for the Sub-Menu"), _("Sub Menu Label"));
      if (submenu)
        {
          subst_illegals (submenu);
          myposition = g_strdup_printf ("%s/%s", myposition, submenu);  //FIXME G_DIR_SEPARATOR in myposition???
        }
    }

  myscheme = getSchemeText ();

  gchar *xml_filename = g_strconcat (myname, XML_EXT, NULL);
  gchar *scm_filename = g_strconcat (myname, SCM_EXT, NULL);
  g_print ("The filename built is %s from %s", xml_filename, myposition);
  gchar *xml_path = g_build_filename (locatedotdenemo (), "actions", "menus", myposition, xml_filename, NULL);
  gchar *scm_path = g_build_filename (locatedotdenemo (), "actions", "menus", myposition, scm_filename, NULL);
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
      //g_print("Added %s\n", text);
      g_free (text);
      if (Denemo.prefs.immediateplayback)
        play_note (DEFAULT_BACKEND, 0, 9, 58, 300, 127 * Denemo.gui->si->master_volume);
    }
}




static void
button_choice_callback (GtkWidget * w, gboolean * left)
{
  g_print ("left at %p is %d\n", left, *left);
  *left = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (w));
  g_print ("left at %p is now %d\n", left, *left);
}

static void
button_move_callback (GtkWidget * w, mouse_gesture * g)
{
  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (w)))
    *g = GESTURE_MOVE;
  // g_print("move %d\n", *g);
}

static void
button_press_callback (GtkWidget * w, mouse_gesture * g)
{
  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (w)))
    *g = GESTURE_PRESS;
  // g_print("press  %d\n", *g);
}

static void
button_release_callback (GtkWidget * w, mouse_gesture * g)
{
  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (w)))
    *g = GESTURE_RELEASE;
  // g_print("release %d \n", *g);
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
  g_print ("loading %s/init.scm into Denemo.ScriptView\n", directory);
  gchar *filename = g_build_filename (locatedotdenemo (), "actions", "menus", directory, INIT_SCM, NULL);
  if (!g_file_test (filename, G_FILE_TEST_EXISTS))
    {
      g_free (filename);
      filename = g_build_filename (locatedotdenemo (), "download", "actions", "menus", directory, INIT_SCM, NULL);
      if (!g_file_test (filename, G_FILE_TEST_EXISTS))
        {
          g_free (filename);
          filename = g_build_filename (get_data_dir (), "actions", "menus", directory, INIT_SCM, NULL);
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
    g_warning ("Could not get contents of %s\n", filename);
  g_free (script);
  g_free (filename);
}

/* write scheme script from Denemo.ScriptView into file init.scm in the user's local menupath.
*/
static void
put_initialization_script (GtkWidget * widget, gchar * directory)
{
  gchar *filename = g_build_filename (locatedotdenemo (), "actions", "menus", directory, INIT_SCM, NULL);
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
  gchar *after = g_object_get_data (G_OBJECT (action), "after");
  gint idx = lookup_command_from_name (Denemo.map, name);
  gchar *tooltip = (gchar *) lookup_tooltip_from_idx (Denemo.map, idx);
  gchar *label = (gchar *) lookup_label_from_idx (Denemo.map, idx);
  
  gchar *xml_filename = g_strconcat (name, XML_EXT, NULL);
  gchar *xml_path = g_build_filename (locatedotdenemo (), "actions", "menus", menupath, xml_filename, NULL);
  g_free (xml_filename);

  gchar *scm_filename = g_strconcat (name, SCM_EXT, NULL);
  gchar *scm_path = g_build_filename (locatedotdenemo (), "actions", "menus", menupath, scm_filename, NULL);
  g_free (scm_filename);
  
  gchar *scheme = getSchemeText ();
  if (scheme && *scheme && confirm (_("Save Script"), g_strconcat (_("Over-write previous version of the script for "), name, _(" ?"), NULL)))
    {
      gchar *dirpath = g_path_get_dirname (xml_path);
      g_mkdir_with_parents (dirpath, 0770);
      g_free (dirpath);
      save_command_metadata (xml_filename, name, label, tooltip, after);
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
  gchar *after = g_object_get_data (G_OBJECT (action), "after");
  gint idx = lookup_command_from_name (Denemo.map, name);
  gchar *tooltip = (gchar *) lookup_tooltip_from_idx (Denemo.map, idx);
  gchar *label = (gchar *) lookup_label_from_idx (Denemo.map, idx);

  gchar *filename = g_build_filename (locatedotdenemo (), "actions", "menus", menupath, name,
                                      NULL);
  gchar *script = g_object_get_data (G_OBJECT (action), "scheme");
  gchar *xml;
  GError *error = NULL;
  g_file_get_contents (filename, &xml, NULL, &error);
  filename = g_build_filename (locatedotdenemo (), "actions", "menus", menupath, INIT_SCM, NULL);
  gchar *init_script;
  g_file_get_contents (filename, &init_script, NULL, &error);

  if (xml == NULL)
    xml = "";
  if (init_script == NULL)
    init_script = "";
  if (script == NULL)
    script = "";


  upload_scripts (name, script, init_script, xml, menupath, label, tooltip, after);

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
      bitmapsdir = g_build_filename (locatedotdenemo (), "actions", "bitmaps", NULL);
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
      bitmapsdir = g_build_filename (locatedotdenemo (), "download", "actions", "bitmaps", NULL);
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
          filename = g_build_filename (get_data_dir (), "actions", "bitmaps", pngname, NULL);
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
                g_warning ("Could not open %s error %s\n", basename, error->message);
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
          g_warning ("Cairo svg backend not available\n");
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
      //g_print("size %d x %d", thesize.width, thesize.height);

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
  gchar *filename = g_build_filename (locatebitmapsdir (), name,
                                      NULL);
  if (loadGraphicFromFormat (name, filename, xbm))
    return TRUE;
  g_free (filename);
  filename = g_build_filename (locatedownloadbitmapsdir (), name, NULL);
  if (loadGraphicFromFormat (name, filename, xbm))
    return TRUE;
  g_free (filename);
  filename = g_build_filename (get_data_dir (), "actions", "bitmaps", name, NULL);
  if (loadGraphicFromFormat (name, filename, xbm))
    return TRUE;
  g_warning ("Could not load graphic");
  return FALSE;
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
      guint width = Denemo.gui->xbm_width;
      guint height = Denemo.gui->xbm_height;


      /*DenemoGraphic *bitmap = */ create_bitmap_from_data (Denemo.gui->xbm, width, height);

#if 0

      //  GdkBitmap *bitmap = gdk_bitmap_create_from_data(NULL, Denemo.gui->xbm, width, height);

      static GdkColor white, black;
      gboolean init = FALSE;
      if (!init)
        {
          gdk_color_parse ("white", &white);
          gdk_colormap_alloc_color (gdk_colormap_get_system (), &white, TRUE, TRUE);
          gdk_color_parse ("black", &black);
          gdk_colormap_alloc_color (gdk_colormap_get_system (), &black, TRUE, TRUE);
        }
      // GdkBitmap *bitmap = gdk_pixmap_create_from_data(NULL, Denemo.gui->xbm, width, height, 1, &white, &black);
      GdkBitmap *bitmap = gdk_pixmap_create_from_data (NULL, Denemo.gui->xbm, width, height, 1, &black, &white);

      g_print ("pixmap create");

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

          gint size = fwrite (Denemo.gui->xbm, 1, height * ((width + 7) / 8) * 8, fp);
          //g_print("Wrote %d bytes for %d x %d\n", size, width, height);


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
  gchar *filepath = g_build_filename (locatedotdenemo (), "download", "actions", "menus", menupath, NULL);
  //g_print("No file %s\n", filepath);
  if (0 != g_access (filepath, 4))
    {
      g_free (filepath);
      filepath = g_build_filename (get_data_dir (), "actions", "menus", menupath, NULL);
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
  //g_print("widget name %s action name %s\n", gtk_widget_get_name(widget), func_name);

  // GSList *h = gtk_action_get_proxies (action);
  //g_print("In menu click action is %p h is %p\n",action, h);



  gint idx = lookup_command_from_name (the_keymap, func_name);
  //g_print("event button %d, idx %d for %s recording = %d scm = %d\n", event->button, idx, func_name, Denemo.ScriptRecording,g_object_get_data(G_OBJECT(action), "scm") );
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


  /* "drag" menu item onto button bar */

  item = gtk_menu_item_new_with_label (_("Place Command on Button Bar"));
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
  //g_print("position from built in is %s\n", myposition);
  if (!myposition)
    myposition = g_object_get_data (G_OBJECT (action), "menupath");     //menu item runs a script
  //g_print("Connecting to %s\n", g_object_get_data(G_OBJECT(widget), "menupath"));

  //g_print("position is %s\n", myposition);
  if (myposition == NULL)
    {
      // g_warning("Cannot find the position of this menu item %s in the menu system\n", func_name);
      return TRUE;
    }
  static gchar *filepath;       // static so that we can free it next time we are here.
  if (filepath)
    g_free (filepath);
  filepath = get_system_menupath (myposition);
  if (0 == g_access (filepath, 4))
    {
      //g_print("We can look for a menu item in the path %s\n", filepath);
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
        g_warning ("Could not get script for %s\n", gtk_action_get_name (action));
      else
        {
          item = gtk_menu_item_new_with_label (_("Get Script"));
          gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
          g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (appendSchemeText_cb), scheme);
        }
      item = gtk_menu_item_new_with_label (_("Save Script"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (saveMenuItem), action);
      if (Denemo.gui->xbm)
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
    //g_print("using %p %s for %d %s %s\n", insertion_point, insertion_point, idx, myposition, func_name);
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
  // configure_keyboard_dialog_init_idx (action, gui, idx);
  return TRUE;
}



static void
color_rhythm_button (RhythmPattern * r, const gchar * color)
{
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
  //g_print("highlight\n");
  color_rhythm_button (r, "black");
}

void
unhighlight_rhythm (RhythmPattern * r)
{
  //g_print("Unhighlight\n");
  color_rhythm_button (r, "gray");
}


/*
 

  
*/
void
highlight_rest (DenemoGUI * gui, gint dur)
{

  //g_print("highlight rest");
  if (gui->currhythm)
    {
      unhighlight_rhythm ((RhythmPattern *) gui->currhythm->data);
    }
  gui->currhythm = NULL;
  gui->cstep = NULL;
  gui->rstep = Denemo.singleton_rhythms['r' + dur]->rsteps;
  unhighlight_rhythm (gui->prevailing_rhythm);
  gui->prevailing_rhythm = Denemo.singleton_rhythms['r' + dur];
  highlight_rhythm (gui->prevailing_rhythm);

}

void
highlight_duration (DenemoGUI * gui, gint dur)
{

  //g_print("higlight duration");
  if (gui->currhythm)
    {
      unhighlight_rhythm ((RhythmPattern *) gui->currhythm->data);
    }
  gui->currhythm = NULL;
  gui->cstep = NULL;
  gui->rstep = Denemo.singleton_rhythms['0' + dur]->rsteps;
  unhighlight_rhythm (gui->prevailing_rhythm);
  gui->prevailing_rhythm = Denemo.singleton_rhythms['0' + dur];
  highlight_rhythm (gui->prevailing_rhythm);
}




/*
 * delete a rhythmic pattern and its button
 * 
 */
static void
delete_rhythm_cb (GtkAction * action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  if ((gui->mode & (INPUTEDIT)) == 0)
    return;
  if (gui->currhythm == NULL)
    return;
  RhythmPattern *r = (RhythmPattern *) gui->currhythm->data;

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
  //g_print("length %d\n", g_list_length(gui->rhythms));
  gui->rhythms = g_list_remove (gui->rhythms, gui->currhythm->data);
  //g_print("length %d %p\n", g_list_length(gui->rhythms), gui->rhythms);
  gui->currhythm = g_list_last (gui->rhythms);

  if (gui->currhythm == NULL)
    {
      gui->rstep = NULL;
      gui->cstep = NULL;
    }
  else
    {
      highlight_rhythm (gui->currhythm->data);
      gui->rstep = ((RhythmPattern *) gui->currhythm->data)->rsteps;
      gui->cstep = ((RhythmPattern *) gui->currhythm->data)->clipboard->data;
    }
  update_scheme_snippet_ids ();
}



/*
 * workaround for glib<2.10
 */
/* UNUSED
static void
attach_action_to_widget (GtkWidget * widget, GtkAction * action, DenemoGUI * gui)
{
  g_object_set_data (G_OBJECT (widget), "action", action);
}
*/
/* attaches a button-press-event signal to the widget with the action as data
   for use in the callback */
static void
attach_right_click_callback (GtkWidget * widget, GtkAction * action)
{



  gtk_widget_add_events (widget, (GDK_BUTTON_PRESS_MASK));      //will not work because label are NO_WINDOW
  g_signal_connect (G_OBJECT (widget), "button-release-event", G_CALLBACK (menu_click), action);

  //g_print("menu click set on %s GTK_WIDGET_FLAGS %x\n", gtk_action_get_name(action), GTK_WIDGET_FLAGS(widget));
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
 *  callback changing mode  gui->mode
 *
 */
static void
change_mode (GtkRadioAction * action, GtkRadioAction * current)
{
  DenemoGUI *gui = Denemo.gui;
  gint val = gtk_radio_action_get_current_value (current);
  GtkWidget *menu = get_edit_menu_for_mode (gui->mode);
  if (menu)
    gtk_widget_hide (menu);
  gui->mode = ((gui->mode & MODE_MASK) | val);
  menu = get_edit_menu_for_mode (gui->mode);
  if (menu)
    gtk_widget_show (menu);
  write_status (gui);

}


GtkAction *
activate_action (gchar * path)
{
  GtkAction *a;
  a = gtk_ui_manager_get_action (Denemo.ui_manager, path);
  if (a)
    gtk_action_activate (a);
  else
    g_warning ("No command at %s - should this be in denemoui.xml?\n", path);
  return a;
}

/**
 *  callback changing the input source (keyboard only/audio/midi)
 *
 */

static void
change_input_type (GtkRadioAction * action, GtkRadioAction * current)
{
  DenemoGUI *gui = Denemo.gui;

  gint val = gtk_radio_action_get_current_value (current);
  gboolean fail = FALSE;
  if (gui->notsaved)
    {
      warningdialog (_("You have unsaved work. Hardware problems may cause the program to exit during this task.\nPlease save first."));
      gtk_radio_action_set_current_value (current, gui->input_source);
      return;
    }
  switch (val)
    {
    case INPUTKEYBOARD:
      if (gui->input_source == INPUTAUDIO)
        {
          // g_print("Stopping audio\n");
          stop_pitch_input ();
        }
      if (gui->input_source == INPUTMIDI)
        {
          // g_print("Stopping midi\n");
          stop_pitch_input ();
        }
      gui->input_source = INPUTKEYBOARD;
      Denemo.gui->last_source = INPUTKEYBOARD;
      g_print ("Input keyboard %d", Denemo.gui->last_source);
      break;
    case INPUTAUDIO:
      //g_print("Starting audio\n");
      if (gui->input_source == INPUTMIDI)
        {
          //g_print("Stopping midi\n");
          stop_pitch_input ();
        }
      gui->input_source = INPUTAUDIO;
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
        gui->input_source = INPUTMIDI;
      else
        fail = TRUE;
      break;
    default:
      g_warning ("Bad Value\n");
      break;
    }
  if (fail)
    {
      gui->input_source = INPUTKEYBOARD;
      gtk_radio_action_set_current_value (current, INPUTKEYBOARD);
    }
  else
    write_input_status ();
}

/**
 *  callback changing type of entry part of gui->mode,
 * depending on the entry type it switches mode part of gui->mode to Classic mode for entering rests and to Insert for entering notes. FIXME could switch to prefs value.
 *
 */
static void
change_entry_type (GtkRadioAction * action, GtkRadioAction * current)
{
  DenemoGUI *gui = Denemo.gui;
  gint val = gtk_radio_action_get_current_value (current);
  switch (val)
    {
#define SET_MODE(m)  (gui->mode=((gui->mode&ENTRY_TYPE_MASK)|m))
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

  write_status (gui);
  //g_print("Mode is %x masks %x %x\n",ENTRY_TYPE_MASK, MODE_MASK, gui->mode);
}

/* callback: if not Insert mode set Insert mode else set Edit mode */
static void
toggle_edit_mode (GtkAction * action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  static gint mode = INPUTINSERT;
  if (gui->mode & INPUTEDIT)
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
      mode = gui->mode;         // remember mode for switching back
      activate_action ("/MainMenu/ModeMenu/EditMode");
    }
}

/* callback: if rest entry make note entry and vv */
static void
toggle_rest_mode (GtkAction * action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  static gint mode = INPUTNORMAL;
  if (gui->mode & INPUTREST)
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
      mode = gui->mode;         // remember mode for switching back
      activate_action ("/MainMenu/ModeMenu/Rest");
    }
}


/* callback: if rhythm entry make note entry and vv */
static void
toggle_rhythm_mode (GtkAction * action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
#if 1
  //g_print("Was mode %x\n", gui->mode);
  if (gui->mode & INPUTRHYTHM)
    gui->mode &= ~INPUTRHYTHM;
  else
    {
      gui->mode |= INPUTRHYTHM;
      activate_action ("/MainMenu/ModeMenu/EditMode");
    }
  // g_print("Now mode %x\n", gui->mode);
#else
  static gint mode = INPUTNORMAL;
  if (gui->mode & INPUTRHYTHM)
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
      mode = gui->mode;         // remember mode for switching back, breaks with multi gui FIXME
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
  DenemoGUI *gui = Denemo.gui;
  //if(!gui->textview)
  refresh_lily_cb (action, gui);
  if (!gtk_widget_get_visible (Denemo.textwindow))
    gtk_widget_show /*_all*/ (Denemo.textwindow);
  else
    gtk_widget_hide (Denemo.textwindow);
  //g_print("toggling lily window");
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
  // g_print("toggling scheme window");
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
  // g_print("Callback for %s\n", g_type_name(G_TYPE_FROM_INSTANCE(widget)));
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
  if (Denemo.prefs.persistence && (Denemo.gui->view == DENEMO_MENU_VIEW))
    Denemo.prefs.rhythm_palette = gtk_widget_get_visible (widget);
}

/**
 *  Function to toggle whether main toolbar is visible 
 *  
 * 
 */
static void
toggle_toolbar (GtkAction * action, gpointer param)
{
  GtkWidget *widget;
  widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ToolBar");
  if ((!action) || gtk_widget_get_visible (widget))
    gtk_widget_hide (widget);
  else
    gtk_widget_show (widget);
  if (Denemo.prefs.persistence && (Denemo.gui->view == DENEMO_MENU_VIEW))
    Denemo.prefs.toolbar = gtk_widget_get_visible (widget);
}

/**
 *  Function to toggle whether playback toolbar is visible 
 *  
 * 
 */
static void
toggle_playback_controls (GtkAction * action, gpointer param)
{
  GtkWidget *widget;
  widget = Denemo.playback_control;
  if ((!action) || gtk_widget_get_visible (widget))
    gtk_widget_hide (widget);
  else
    gtk_widget_show (widget);
  if (Denemo.prefs.persistence && (Denemo.gui->view == DENEMO_MENU_VIEW))
    Denemo.prefs.playback_controls = gtk_widget_get_visible (widget);
}

/**
 *  Function to toggle whether playback toolbar is visible 
 *  
 * 
 */
static void
toggle_midi_in_controls (GtkAction * action, gpointer param)
{
  GtkWidget *widget;
  widget = Denemo.midi_in_control;
  if ((!action) || gtk_widget_get_visible (widget))
    gtk_widget_hide (widget);
  else
    gtk_widget_show (widget);
  if (Denemo.prefs.persistence && (Denemo.gui->view == DENEMO_MENU_VIEW))
    Denemo.prefs.midi_in_controls = gtk_widget_get_visible (widget);
}

/**
 *  Function to toggle whether entry toolbar is visible 
 *  
 * 
 */
static void
toggle_entry_toolbar (GtkAction * action, gpointer param)
{
  GtkWidget *widget;
  widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/EntryToolBar");
  if ((!action) || gtk_widget_get_visible (widget))
    gtk_widget_hide (widget);
  else
    gtk_widget_show (widget);
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
 *  Function to toggle visibility of print preview pane of current gui
 *  
 *  
 */
static void
toggle_print_view (GtkAction * action, gpointer param)
{
#ifndef USE_EVINCE  
  g_debug("This feature requires denemo to be built with evince");
#else
  GtkWidget *w = gtk_widget_get_toplevel (Denemo.printarea);
  if ((!action) || gtk_widget_get_visible (w))
    gtk_widget_hide (w);
  else
    {
      gtk_widget_show (w);
      if (GPOINTER_TO_INT (g_object_get_data (G_OBJECT (Denemo.printarea), "printviewupdate")) < Denemo.gui->changecount)
        refresh_print_view (TRUE);
    }
#endif
}

/**
 *  Function to toggle visibility of score layout window of current gui
 *  
 *  
 */
static void
toggle_score_layout (GtkAction * action, gpointer param)
{
#ifndef USE_EVINCE  
  g_debug("This feature requires denemo to be built with evince");
#else
  DenemoGUI *gui = Denemo.gui;
  GtkWidget *w = gui->score_layout;
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
  GtkWidget *widget = Denemo.gui->si->lyricsbox;
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
      if (Denemo.prefs.persistence && (Denemo.gui->view == DENEMO_MENU_VIEW))
        Denemo.prefs.lyrics_pane = gtk_widget_get_visible (widget);
    }
#endif
}

/**
 *  Function to toggle visibility of console view pane
 *  
 *  
 */
static void
toggle_console_view (GtkAction * action, gpointer param)
{
#ifndef USE_EVINCE  
  g_debug("This feature requires denemo to be built with evince");
#else
  GtkWidget *widget = gtk_widget_get_parent (Denemo.console);
  if (!widget)
    g_warning ("Internal Error");
  else
    {
      if ((!action) || gtk_widget_get_visible (widget))
        gtk_widget_hide (widget);
      else
        {
          gtk_widget_show (widget);
          GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (Denemo.console));
          GtkTextIter iter;
          /* get end iter */
          gtk_text_buffer_get_end_iter (buffer, &iter);
          /* scroll to end iter */
          gtk_text_view_scroll_to_iter (GTK_TEXT_VIEW (Denemo.console), &iter, 0.0, FALSE, 0, 0);
        }
    }
  if (Denemo.prefs.persistence && (Denemo.gui->view == DENEMO_MENU_VIEW))
    Denemo.prefs.console_pane = gtk_widget_get_visible (widget);
#endif
}


/**
 *  Function to toggle visibility of print preview pane of current gui
 *  
 *  
 */
void
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
 *  Function to toggle visibility of titles etc of current gui
 *  
 *  
 */
static void
toggle_scoretitles (GtkAction * action, gpointer param)
{
#ifndef USE_EVINCE  
  g_debug("This feature requires denemo to be built with evince");
#else
  GtkWidget *widget = Denemo.gui->buttonboxes;
  if ((!action) || gtk_widget_get_visible (widget))
    gtk_widget_hide (widget);
  else
    gtk_widget_show (widget);
  if (Denemo.prefs.persistence && (Denemo.gui->view == DENEMO_MENU_VIEW))
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
  {ToggleEntryToolbar_STRING, NULL, N_("Note and Rest Entry"), NULL, N_("Show/hide a toolbar which allows\nyou to enter notes and rests using the mouse"),
   G_CALLBACK (toggle_entry_toolbar), TRUE}
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

  {ToggleLyricsView_STRING, NULL, N_("Lyrics"), NULL, NULL,
   G_CALLBACK (toggle_lyrics_view), TRUE}
  ,

  {ToggleConsoleView_STRING, NULL, N_("LilyPond Errors"), NULL, NULL,
   G_CALLBACK (toggle_console_view), TRUE}
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
  DenemoGUI *gui;
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
  g_print ("%s%s\n", message, widget ? g_type_name (G_TYPE_FROM_INSTANCE (widget)) : "NULL widget");
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
  //g_print("switching pagenum %d\n",pagenum);
  DenemoGUI *gui = Denemo.gui;
  if (gui == NULL)
    return;
  GList *g = g_list_nth (Denemo.guis, pagenum);
  if (g == NULL)
    {
      g_warning ("got a switch page, but there is no such page in Denemo.guis\n");
      return;
    }
  DenemoGUI *newgui = g->data;
  if (gui == newgui)
    return;                     //on arrival Denemo.gui is already set to the new gui when you are doing new window
  /* turn off the LilyPond window if it is on
     it would be nice to keep a record of whether it was open for re-opening
     on return to this tab FIXME */

  if (Denemo.textwindow && gtk_widget_get_visible (Denemo.textwindow))
    activate_action ("/MainMenu/ViewMenu/" ToggleLilyText_STRING);

  if (gtk_widget_get_visible (Denemo.gui->score_layout))
    activate_action ("/MainMenu/ViewMenu/" ToggleScoreLayout_STRING);

  unhighlight_rhythm (Denemo.gui->prevailing_rhythm);

  Denemo.gui = gui = (DenemoGUI *) (g->data);
  //g_print("switch page\n");

//FIXME if Denemo.gui->si->audio then show Denemo.audio_vol_control
  if (Denemo.prefs.visible_directive_buttons)
    {
      gtk_widget_hide (Denemo.gui->buttonboxes);
      activate_action ("/MainMenu/ViewMenu/" ToggleScoreTitles_STRING);
    }
  switch (gui->mode & ~MODE_MASK)
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

  switch (gui->mode & ~ENTRY_TYPE_MASK)
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
      g_print ("activating rhythm\n");
      activate_action ("/MainMenu/ModeMenu/Rhythm");
      break;

    default:
      ;
    }
  set_title_bar (Denemo.gui);
  highlight_rhythm (Denemo.gui->prevailing_rhythm);
  gtk_widget_queue_draw (Denemo.scorearea);
  draw_score (NULL);
}




static gboolean
thecallback (GtkWidget * widget, GdkEventButton * event, GtkAction * action)
{
  if (event->button == 1 && !(event->state & (GDK_SHIFT_MASK | GDK_CONTROL_MASK)))
    return FALSE;
  g_print ("going for %d for %d\n", event->button, event->state);
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
  attach_action_to_widget (proxy, action, Denemo.gui);
#endif
  if (Denemo.map == NULL)
    return;
  command_idx = lookup_command_from_name (Denemo.map, gtk_action_get_name (action));


  if (command_idx != -1)
    update_accel_labels (Denemo.map, command_idx);
  //  else //not an error, it occurs for menus being loaded
  //   g_warning("%s is not yet in map\n",  gtk_action_get_name(action));
  gboolean hidden = (gboolean) GPOINTER_TO_INT ((action ? g_object_get_data (G_OBJECT (action), "hidden") : NULL));
  if (hidden)
    {
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
set_master_volume (DenemoScore * si, gdouble volume)
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
set_master_tempo (DenemoScore * si, gdouble tempo)
{
  si->master_tempo = tempo;
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
  GtkWidget *main_vbox, *menubar, *toolbar, *hbox;
  GtkActionGroup *action_group;
  GtkUIManager *ui_manager;
  GError *error;
  gchar *denemoui_path = NULL, *data_file = NULL;

  Denemo.window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (Denemo.window), _("Denemo Main Window"));
  loadWindowState ( /* it accesses Denemo.window */ );
#ifdef G_OS_WIN32
  g_print ("Denemo icon not used");
  //not installed on windows ... data_file = g_build_filename (get_data_dir (), "icons","denemo.png", NULL);
#else
  data_file = g_strconcat (get_data_dir (), "/../pixmaps/denemo.png", NULL);    //FIXME installed in wrong place?
  gtk_window_set_default_icon_from_file (data_file, NULL);
#endif

  g_signal_connect (G_OBJECT (Denemo.window), "delete_event", G_CALLBACK (delete_callback), NULL);
  g_free (data_file);

  gtk_window_set_resizable (GTK_WINDOW (Denemo.window), TRUE);

  Denemo.color = 0xFFFFFF;      //white background RGB values



  main_vbox = gtk_vbox_new (FALSE, 1);
  gtk_container_set_border_width (GTK_CONTAINER (main_vbox), 1);
  gtk_container_add (GTK_CONTAINER (Denemo.window), main_vbox);
  gtk_widget_show (main_vbox);

  Denemo.action_group = action_group = gtk_action_group_new ("MenuActions");
  gtk_action_group_set_translation_domain (action_group, NULL);
  /* This also sets current Denemo.gui as the  callback data for all the functions in the
   * menubar, which is not needed since we have only one set of actions for all
   the guis. We will always act on Denemo.gui anyway.*/
  gtk_action_group_add_actions (action_group, menu_entries, G_N_ELEMENTS (menu_entries), Denemo.gui);
  gtk_action_group_add_toggle_actions (action_group, toggle_menu_entries, G_N_ELEMENTS (toggle_menu_entries), Denemo.gui);
  gtk_action_group_add_radio_actions (action_group, mode_menu_entries, G_N_ELEMENTS (mode_menu_entries), INPUTINSERT /* initial value */ ,
                                      G_CALLBACK (change_mode), Denemo.gui);


  gtk_action_group_add_radio_actions (action_group, type_menu_entries, G_N_ELEMENTS (type_menu_entries), INPUTNORMAL /* initial value */ ,
                                      G_CALLBACK (change_entry_type), Denemo.gui);

  gtk_action_group_add_radio_actions (action_group, input_menu_entries, G_N_ELEMENTS (input_menu_entries), have_midi ()? INPUTMIDI : INPUTKEYBOARD /* initial value */ ,
                                      G_CALLBACK (change_input_type), NULL);




  ui_manager = gtk_ui_manager_new ();
  Denemo.ui_manager = ui_manager;
  gtk_ui_manager_set_add_tearoffs (Denemo.ui_manager, TRUE);
  gtk_ui_manager_insert_action_group (ui_manager, action_group, 0);

  g_signal_connect (G_OBJECT (Denemo.ui_manager), "connect-proxy", G_CALLBACK (proxy_connected), NULL);


  //We do not use accel_group anymore TODO delete the next 2 lines
  //accel_group = gtk_ui_manager_get_accel_group (ui_manager);
  //gtk_window_add_accel_group (GTK_WINDOW (Denemo.window), accel_group);


  data_file = g_build_filename (get_data_dir (), "denemoui.xml", NULL);
  if (g_file_test (data_file, G_FILE_TEST_EXISTS))
    denemoui_path = data_file;
  else
    g_free (data_file);

  if (!denemoui_path)
    {
      data_file = g_build_filename ("denemoui.xml", NULL);
      if (g_file_test (data_file, G_FILE_TEST_EXISTS))
        denemoui_path = data_file;
      else
        g_free (data_file);
    }

  if (!denemoui_path)
    {
      g_error ("denemoui.xml could not be found, exiting");
      exit (EXIT_FAILURE);
    }

  error = NULL;
  if (!gtk_ui_manager_add_ui_from_file (ui_manager, data_file, &error))
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
  gtk_box_pack_start (GTK_BOX (main_vbox), Denemo.menubar, FALSE, TRUE, 0);
  gtk_widget_show (Denemo.menubar);

  gtk_widget_set_tooltip_text (gtk_ui_manager_get_widget (ui_manager, "/ObjectMenu"), _("This is the Object Menu bar, where menus for the commands that edit music live. They are arranged in a hierarchy Score, Movement, Staff (which contains Voices) and then the things that go on a staff, notes, clefs etc. Directives covers everything else that you can put in amongst the notes to change the behavior from that point in the music."));
  gtk_widget_set_tooltip_markup (gtk_ui_manager_get_widget (ui_manager, "/EntryToolBar"), _("This bar has buttons for entering notes and rests. The highlighted duration is the <i>prevailing duration</i>, that is the duration which will be applied to the next note entered. You can hide this bar (to make more room on the screen) using the View menu. You can make it your preference to hide it using MainMenu → Edit → Change Preferences → Display Note/Rest entry toolbar"));

  gtk_widget_set_tooltip_markup (gtk_ui_manager_get_widget (ui_manager, "/RhythmToolBar"),
                                 _
                                 ("You can populate this bar with buttons holding a snippet of music. The highlighted snippet is the <i>prevailing duration</i>, that is the next note entered will follow the rhythmic pattern of this snippet.\nYou can enter the whole snippet by clicking on it, or using the command under ObjectMenu → Notes/Rests → Append/InsertDuration → Insert Snippet. You can also select the <i>prevailing snippet</i> using  ObjectMenu → Notes/Rests → Select Duration → Next Snippet.\nYou can hide this bar (to make more room on the screen) using the View menu. You can make it your preference to hide it using MainMenu → Edit → Change Preferences → Display Note/Rest entry toolbar"));

  toolbar = gtk_ui_manager_get_widget (ui_manager, "/ToolBar");
  // The user should be able to decide toolbar style.
  // But without gnome, there is no (ui) to set this option.
  gtk_widget_set_tooltip_text (toolbar, _("This tool bar contains a few conventional commands. You can hide it (to make more room on the screen) using the View menu. You can make it your preference to hide it using MainMenu → Edit → Change Preferences → Display general toolbar"));
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_BOTH_HORIZ);
  gtk_box_pack_start (GTK_BOX (main_vbox), toolbar, FALSE, TRUE, 0);
  gtk_widget_set_can_focus (toolbar, FALSE);
  //GTK_WIDGET_UNSET_FLAGS(toolbar, GTK_CAN_FOCUS); 


  {
    Denemo.playback_control = gtk_vbox_new (FALSE, 1);
    gtk_widget_set_tooltip_text (Denemo.playback_control,
                                 _
                                 ("Controls for playback. The arrows on either side of the PLAY and STOP buttons move the playback start"
                                 " and playback end markers. Loop plays in a loop - you can edit while it plays. You can also record the output and save it as .ogg or .wav file. The temperament used for playing back can be set here."));
    gtk_box_pack_start (GTK_BOX (main_vbox), Denemo.playback_control, FALSE, TRUE, 0);
    GtkFrame *frame = (GtkFrame *) gtk_frame_new (_("Playback Control"));
    gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
    gtk_container_add (GTK_CONTAINER (Denemo.playback_control), GTK_WIDGET (frame));

    GtkWidget *inner1 = gtk_vbox_new (FALSE, 1);
    gtk_container_add (GTK_CONTAINER (frame), inner1);


    GtkWidget *inner = gtk_hbox_new (FALSE, 1);
    gtk_box_pack_start (GTK_BOX (inner1), inner, FALSE, TRUE, 0);

    //gtk_box_pack_start (GTK_BOX (main_vbox), inner, FALSE, TRUE, 0);
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
      /* Tempo */
      label = gtk_label_new (_("Tempo:"));
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

      /* Volume */
      label = gtk_label_new (_("Volume"));
      //GTK_WIDGET_UNSET_FLAGS(label, GTK_CAN_FOCUS);
      gtk_widget_set_can_focus (label, FALSE);
      gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

      master_vol_adj = (GtkAdjustment *) gtk_adjustment_new (1.0, 0.0, 1.0, 1.0, 1.0, 0.0);

      hscale = gtk_hscale_new (GTK_ADJUSTMENT (master_vol_adj));
      gtk_scale_set_digits (GTK_SCALE (hscale), 2);
      gtk_widget_set_can_focus (hscale, FALSE);
      //GTK_WIDGET_UNSET_FLAGS(hscale, GTK_CAN_FOCUS);
      g_signal_connect (G_OBJECT (master_vol_adj), "value_changed", G_CALLBACK (pb_volume), NULL);
      gtk_box_pack_start (GTK_BOX (hbox), hscale, TRUE, TRUE, 0);


      /*Audio Volume */
      Denemo.audio_vol_control = gtk_hbox_new (FALSE, 1);
      label = gtk_label_new (_("Audio Volume Cut"));
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
      gtk_widget_set_tooltip_text (GTK_WIDGET (leadin), _("Set the number of seconds to clip from the audio, or if negative number of seconds silence before audio plays.\nThis is useful when the audio track does not begin on a barline."));
      g_signal_connect (G_OBJECT (leadin), "value_changed", G_CALLBACK (leadin_changed), NULL);
      gtk_box_pack_start (GTK_BOX (Denemo.audio_vol_control), GTK_WIDGET (leadin), FALSE, TRUE, 0);
      label = gtk_label_new (_(" secs."));
      gtk_widget_set_can_focus (label, FALSE);
      gtk_box_pack_start (GTK_BOX (Denemo.audio_vol_control), label, FALSE, TRUE, 0);

      gtk_box_pack_start (GTK_BOX (hbox), Denemo.audio_vol_control, TRUE, TRUE, 0);

    }


    Denemo.midi_in_control = gtk_vbox_new (FALSE, 1);
    gtk_widget_set_tooltip_text (Denemo.midi_in_control,
                                 _
                                 ("Controls for managing input from a MIDI controller (e.g. keyboard) attached to the computer. You may need to select your MIDI device first using MainMenu → Edit → Change Preferences → MIDI looking for MIDI in devices (turn your device on first). When you have a MIDI controller durations are inserted without any pitch (they appear in brown) playing on the controller puts the pitches onto the durations. The Shift and Control and ALT keys can also be used for listening without entering notes, checking pitches entered and entering chords. The foot pedal can also be used for chords. Release the ALT key and re-press to start a new chord - timing is unimportant, play the chord fast or slow."));
    gtk_box_pack_start (GTK_BOX (main_vbox), Denemo.midi_in_control, FALSE, TRUE, 0);
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


  toolbar = gtk_ui_manager_get_widget (ui_manager, "/EntryToolBar");
  //g_print("EntryToolbar is %p\n", toolbar);
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_TEXT);
  gtk_box_pack_start (GTK_BOX (main_vbox), toolbar, FALSE, TRUE, 0);
  gtk_widget_set_can_focus (toolbar, FALSE);
  //GTK_WIDGET_UNSET_FLAGS(toolbar, GTK_CAN_FOCUS);

  // gtk_widget_show (toolbar); cannot show this until the GtkLabels have become GtkAccelLabels - a gtk bug

  toolbar = gtk_ui_manager_get_widget (ui_manager, "/RhythmToolBar");
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_TEXT);
  gtk_box_pack_start (GTK_BOX (main_vbox), toolbar, FALSE, TRUE, 0);

  menubar = gtk_ui_manager_get_widget (ui_manager, "/ObjectMenu");
  if (menubar)
    {
      gtk_box_pack_start (GTK_BOX (main_vbox), menubar, FALSE, TRUE, 0);
    }


  //  menubar = gtk_ui_manager_get_widget (ui_manager, "/ActionMenu");
  //  if(menubar) {
  //    gtk_box_pack_start (GTK_BOX (main_vbox), menubar, FALSE, TRUE, 0);
  //  }

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
    gtk_container_add (GTK_CONTAINER (main_vbox), scorearea_topbox);

    GtkWidget *score_and_scroll_hbox = gtk_hbox_new (FALSE, 1);
    gtk_container_add (GTK_CONTAINER (scorearea_topbox), score_and_scroll_hbox);
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



    gtk_widget_add_events /*gtk_widget_set_events */ (Denemo.scorearea, (GDK_EXPOSURE_MASK
                                                                         | GDK_POINTER_MOTION_MASK | GDK_LEAVE_NOTIFY_MASK | GDK_ENTER_NOTIFY_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK));

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
  // create_console(GTK_BOX(main_vbox));
  Denemo.statusbar = gtk_statusbar_new ();
  gtk_widget_set_tooltip_text (Denemo.statusbar, _("This bar shows:\nPending ♯ or ♭ sign (if the next note entered will be sharpened or flattened)\nThe movement number\nDescription of the object at the Denemo cursor\nPosition and status (appending or inserting) of the cursor.\nIf the Playback Controls are visible then the timing of the object at the cursor is shown.\nWhen the first key of a two-key shortcut is pressed the possible continuations are shown here."));
  hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (hbox), Denemo.statusbar, TRUE, TRUE, 5);
  gtk_widget_show (Denemo.statusbar);
  Denemo.status_context_id = gtk_statusbar_get_context_id (GTK_STATUSBAR (Denemo.statusbar), "Denemo");
  gtk_statusbar_push (GTK_STATUSBAR (Denemo.statusbar), Denemo.status_context_id, "Denemo");
  Denemo.input_source = gtk_label_new ("");
  gtk_widget_set_tooltip_text (Denemo.input_source, _("This area shows which MIDI filters are active. It can also be used by commands to pass information to the user"));
  gtk_widget_show (Denemo.input_source);
  Denemo.input_filters = g_string_new ("");
  gtk_box_pack_end (GTK_BOX (hbox), Denemo.input_source, TRUE, TRUE, 5);
  gtk_widget_show (hbox);

  create_scheme_window ();

  if (!Denemo.non_interactive)
    gtk_widget_show (Denemo.window);
  /* Now that the window is shown, initialize the gcs */
  // gcs_init (Denemo.window->window);

  parse_paths (denemoui_path, Denemo.gui);
  g_free (denemoui_path);

  use_markup (Denemo.window);   /* set all the labels to use markup so that we can use the music font. Be aware this means you cannot use labels involving "&" "<" and ">" and so on without escaping them 
                                   FIXME labels in toolitems are not correct until you do NewWindow.
                                   Really we should change the default for the class. */
  //  g_print("Turning on the modes\n");


  //write_status(Denemo.gui);
  Denemo.InsertModeMenu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu/NotesRests/InsertModeNote");
  Denemo.EditModeMenu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu/NotesRests/EditModeNote");
  Denemo.ClassicModeMenu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu/NotesRests/ClassicModeNote");
  Denemo.ModelessMenu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu/NotesRests/ModelessNote");

  //gtk_widget_hide (gtk_ui_manager_get_widget (ui_manager, "/ActionMenu"));// make a prefs thing
  //GTK bug now fixed gtk_widget_hide (gtk_ui_manager_get_widget (ui_manager, "/EntryToolBar")); //otherwise buttons only sensitive around their edges


  g_signal_connect (G_OBJECT (Denemo.notebook), "switch_page", G_CALLBACK (switch_page), NULL);
}                               /* create window */


void
newview (GtkAction * action, gpointer param)
{
  newtab (NULL, NULL);
  Denemo.gui->si->undo_guard = 1;       //do not collect undo for initialization of score
  load_scheme_init ();
  Denemo.gui->si->undo_guard = Denemo.prefs.disable_undo;
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

/**
 * Creates a new DenemoGUI structure represented by a tab in a notebook: the DenemoGUI can, at anyone time, control one musical score possibly of several movements. It can, from time to time have different musical scores loaded into it. So it is to be thought of as a Music Score Editor.
 * This DenemoGUI* gui is appended to the global list Denemo.guis.
 * A single movement (DenemoScore) is instantiated in the gui.
 * 
 */
static void
newtab (G_GNUC_UNUSED GtkAction * action, G_GNUC_UNUSED gpointer param)
{
  if (Denemo.gui && gtk_widget_get_visible (Denemo.gui->score_layout))
    activate_action ("/MainMenu/ViewMenu/" ToggleScoreLayout_STRING);
  if (Denemo.gui && gtk_widget_get_visible (Denemo.textwindow))
    activate_action ("/MainMenu/ViewMenu/" ToggleLilyText_STRING);

  static gint id = 1;
  DenemoGUI *gui = (DenemoGUI *) g_malloc0 (sizeof (DenemoGUI));
  //uniquely identifies this musical score editor for duration of program.
  gui->id = id++;
  gui->mode = Denemo.prefs.mode;
  gui->pending_midi = g_queue_new ();
  gui->score_layout = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (gui->score_layout), "Score Layout");
  gtk_window_set_default_size (GTK_WINDOW (gui->score_layout), 400, 800);
  g_signal_connect (G_OBJECT (gui->score_layout), "delete-event", G_CALLBACK (hide_score_layout_on_delete), NULL);

  Denemo.guis = g_list_append (Denemo.guis, gui);


  Denemo.gui = NULL;
  // Denemo.gui = gui; must do this after switching to page, so after creating page
  gui->lilycontrol.papersize = g_string_new ("a4");     //A4 default
  gui->lilycontrol.staffsize = g_string_new ("18");
  gui->lilycontrol.lilyversion = g_string_new ("");
  gui->lilycontrol.orientation = TRUE;  //portrait



  //gui->pixmap = NULL;

  /* Initialize the GUI */

  //create the tab for this gui
  GtkWidget *top_vbox = gtk_vbox_new (FALSE, 1);
  gui->buttonboxes = gtk_vbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (top_vbox), gui->buttonboxes, FALSE, TRUE, 0);
  gui->buttonbox = gtk_hbox_new (FALSE, 1);
  gtk_widget_set_tooltip_text (gui->buttonbox, _("A button bar that can be populated by titles and other user generated buttons.\nGenerally by clicking the button you can edit the title or value or execute the action of the button"));
  gtk_box_pack_start (GTK_BOX (gui->buttonboxes), gui->buttonbox, FALSE, TRUE, 0);

  gtk_widget_set_can_focus (gui->buttonboxes, FALSE);
  gtk_widget_set_can_focus (gui->buttonbox, FALSE);






  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (top_vbox), main_vbox, TRUE, TRUE, 0);
  gint pagenum =                //gtk_notebook_append_page (GTK_NOTEBOOK (Denemo.notebook), top_vbox, NULL);
    gtk_notebook_insert_page_menu (GTK_NOTEBOOK (Denemo.notebook), top_vbox, NULL, NULL, -1);
  /*(GtkNotebook *notebook,
     GtkWidget *child,
     GtkWidget *tab_label,
     GtkWidget *menu_label,
     gint position); */
  gtk_notebook_popup_enable (GTK_NOTEBOOK (Denemo.notebook));

  Denemo.page = gtk_notebook_get_nth_page (GTK_NOTEBOOK (Denemo.notebook), pagenum);    //note Denemo.page is redundant, it is set to the last page created and it is never unset even when that page is deleted - it is only used by the selection paste routine.
  gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), pagenum);

  Denemo.gui = gui;
  set_title_bar (gui);
  if (pagenum)
    gtk_notebook_set_show_tabs (GTK_NOTEBOOK (Denemo.notebook), TRUE);
  set_title_bar (gui);
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
  
  //FIXME populate_opened_recent_menu (gui);

  /* create the first movement now because showing the window causes it to try to draw the scorearea
     which it cannot do before there is a score. FIXME use signal blocking to control this - see importxml.c */
  point_to_new_movement (gui);
  gui->movements = g_list_append (NULL, gui->si);

  install_lyrics_preview (gui->si, top_vbox);
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

  //Denemo.gui->mode = Denemo.prefs.mode;

  // this stops the keyboard input from getting to  scorearea_keypress_event if done after attaching the signal, why?
  gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), pagenum);      //if this is not done Gdk-CRITICAL **: gdk_draw_drawable: assertion `GDK_IS_DRAWABLE (drawable)' failed message results. Presumably because we have failed to block the (expose_event) drawing while we set up the new page. FIXME.

  gtk_widget_set_can_focus (Denemo.scorearea, TRUE);
  //GTK_WIDGET_SET_FLAGS(Denemo.scorearea, GTK_CAN_FOCUS);
  gtk_widget_grab_focus (GTK_WIDGET (Denemo.scorearea));

  if (Denemo.prefs.autosave)
    {
      if (Denemo.autosaveid)
        {
          g_print ("No autosave on new tab.\n");
        }
      else
        {
          Denemo.autosaveid = g_timeout_add (Denemo.prefs.autosave_timeout * 1000 * 60, (GSourceFunc) auto_save_document_timeout, Denemo.gui);
        }
    }


  if (Denemo.prefs.visible_directive_buttons)
    {
      gtk_widget_hide (Denemo.gui->buttonboxes);
      activate_action ("/MainMenu/ViewMenu/" ToggleScoreTitles_STRING);
    }
  if (have_midi () && Denemo.prefs.startmidiin)
    gui->input_source = INPUTMIDI;
}                               /* end of newtab creating a new DenemoGUI holding one musical score */
