#include <math.h>
#include "scripting/scheme-callbacks.h"
#include "command/commandfuncs.h"
#include "command/grace.h"
#include "command/lilydirectives.h"
#include "command/lyric.h"
#include "command/measure.h"
#include "command/object.h"
#include "command/processstaffname.h"
#include "command/select.h"
#include "command/score.h"
#include "command/scorelayout.h"
#include "core/cache.h"
#include "core/utils.h"
#include "core/menusystem.h"
#include "core/view.h"
#include "core/graphicseditor.h"
#include "core/keymapio.h"
#include "audio/pitchentry.h"
#include "command/lilydirectives.h"
#include "audio/playback.h"
#include "audio/audiointerface.h"

#include "export/audiofile.h"
#include "export/guidedimportmidi.h"
#include "export/print.h"
#include "export/file.h"
#include "export/exportmidi.h"
#include "ui/markup.h"
#include "ui/keysigdialog.h"
#include "ui/virtualkeyboard.h"
#include "ui/texteditors.h"
#include "ui/mousing.h"
#include "printview/svgview.h"
#include "printview/printview.h"
#include "printview/markupview.h"
#include "display/calculatepositions.h"
#include "source/source.h"
#include "source/sourceaudio.h"

#include "scripting/scheme_cb.h"
SCM
scheme_call_callback (SCM optional, callback_function callback)
{
  gboolean query = FALSE;
  DenemoScriptParam param;
  GString *gstr = NULL;
  int length;
  char *str = NULL;

  if (scm_is_string (optional))
    {
      str = scm_to_locale_stringn (optional, (size_t *) & length);
      gstr = g_string_new_len (str, length);
      if (!strncmp ("query", str, 5))
        query = TRUE;
    }
  param.string = gstr;
  param.status = FALSE;

  callback (NULL, &param);
  if (param.status && query)
    return scm_from_locale_string (gstr->str);
  if (gstr)
    g_string_free (gstr, TRUE);
  return SCM_BOOL (param.status);
}

SCM ReturnValue = SCM_BOOL_F;
static void
set_return_value (SCM val)
{
  ReturnValue = val;
}

static gboolean
scm_is_list (SCM scm)
{
  return scm_is_true (scm_list_p (scm));
}

SCM
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
              //g_debug("Note that %d is value and %d stringp\n", scm_pair_p(el), scm_string_p(el));
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

static void
toggle_value (gboolean * value)
{
  *value = !*value;
}

void
check_all (GtkWidget * button)
{
  GList *children = gtk_container_get_children ((GtkContainer *) gtk_widget_get_parent (button));
  for (; children; children = children->next)
    {
      GtkWidget *child = children->data;
      if (GTK_IS_CHECK_BUTTON (child))
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (child), TRUE);
    }
}

void
uncheck_all (GtkWidget * button)
{
  GList *children = gtk_container_get_children ((GtkContainer *) gtk_widget_get_parent (button));
  for (; children; children = children->next)
    {
      GtkWidget *child = children->data;
      if (GTK_IS_CHECK_BUTTON (child))
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (child), FALSE);
    }
}

SCM
scheme_check_boxes (SCM list, SCM title)
{
  gchar *thetitle = scm_is_string (title) ? scm_to_locale_string (title) : _("Set Values");
  GtkWidget *dialog = gtk_dialog_new_with_buttons (thetitle,
                                                   GTK_WINDOW (Denemo.window),
                                                   GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                                   GTK_STOCK_OK,
                                                   GTK_RESPONSE_ACCEPT,
                                                   GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
                                                   NULL);
  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));

  if (scm_is_list (list))
    {
      gint i;
      gint length = scm_to_int (scm_length (list));
      gboolean *status;
      status = (gint *) g_malloc0 (length * sizeof (gboolean));
      GtkWidget *button = gtk_button_new_with_label (_("Check all"));
      g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (check_all), NULL);
      gtk_container_add (GTK_CONTAINER (content_area), button);
      button = gtk_button_new_with_label (_("Un-check all"));
      g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (uncheck_all), NULL);
      gtk_container_add (GTK_CONTAINER (content_area), button);


      scm_reverse (list);
      for (i = 0; i < length; i++)
        {
          SCM el = scm_list_ref (list, scm_from_int (i));
          if (scm_is_pair (el))
            {
              gchar *label = NULL;
              if (scm_is_string (scm_car (el)))
                {
                  label = scm_to_locale_string (scm_car (el));
                  status[i] = scm_is_true (scm_cdr (el));
                }
              if (label)
                {
                  GtkWidget *item = gtk_check_button_new_with_label (label);

                  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (item), status[i]);
                  gtk_container_add (GTK_CONTAINER (content_area), item);
                  g_signal_connect_swapped (G_OBJECT (item), "toggled", G_CALLBACK (toggle_value), &status[i]);
                }
              else
                {
                  return SCM_BOOL_F;
                }
            }
        }
      gtk_widget_show_all (dialog);
      gint result = gtk_dialog_run (GTK_DIALOG (dialog));
      switch (result)
        {
        case GTK_RESPONSE_ACCEPT:
          for (i = 0; i < length; i++)
            {
              scm_list_set_x (list, scm_from_int (i), scm_cons (scm_car (scm_list_ref (list, scm_from_int (i))), scm_from_bool (status[i])));
            }
          g_free (status);
          break;
        default:
          gtk_widget_destroy (dialog);
          return SCM_BOOL_F;
        }
      gtk_widget_destroy (dialog);
    }
  return list;
}



SCM
scheme_create_palette_button (SCM palette, SCM lbl, SCM tltp, SCM scrp)
{
  SCM ret;
  gchar *name = scm_to_locale_string (palette);
  gchar *label = scm_to_locale_string (lbl);
  gchar *tooltip = scm_to_locale_string (tltp);
  gchar *script = scm_to_locale_string (scrp);

  DenemoPalette *pal = create_palette (name, FALSE, TRUE);

  ret = palette_add_button (pal, label, tooltip, script) ? SCM_BOOL_T : SCM_BOOL_F;
  gtk_widget_show (gtk_widget_get_parent (pal->box));
  free (name);
  free (label);
  free (tooltip);
  free (script);
  return ret;
}

SCM
scheme_set_palette_shape (SCM palette, SCM horizontal, SCM limit)
{
  gchar *name = scm_to_locale_string (palette);
  gboolean horiz = scm_is_true (horizontal);
  gint lim = scm_to_int (limit);

  DenemoPalette *pal = create_palette (name, FALSE, horiz);
  free (name);
  if (lim > 0)
    {
      pal->limit = lim;
      pal->rows = horiz;
      repack_palette (pal);
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

SCM
scheme_show_palettes (SCM option, SCM show)
{
  if (scm_is_true (option))
    {
      gchar *name;
      if (scm_is_string (option))
        {
          name = scm_to_locale_string (option);
          DenemoPalette *pal = get_palette (name);
          if (pal == NULL)
            {
              mergePalette ((const gchar *) name);
              pal = get_palette (name);
            }
          if (pal)
            {
              if (!scm_is_false (show))
                {
                  gtk_widget_show (gtk_widget_get_parent (pal->box));
                  gtk_widget_show_all (pal->box);
                }
              else
                {
                  gtk_widget_hide (pal->box);
                  gtk_widget_hide (gtk_widget_get_parent (pal->box));
                }
              return SCM_BOOL_T;
            }
          else
            {
              return SCM_BOOL_F;
            }
        }
      else
        {
          name = choose_palette_by_name (FALSE, TRUE);
          if (name)
            {
              DenemoPalette *pal = get_palette (name);
              if (pal)
                {
                  gtk_widget_show (gtk_widget_get_parent (pal->box));
                  gtk_widget_show_all (pal->box);
                  return SCM_BOOL_T;
                }
              else
                {
                  return SCM_BOOL_F;
                }
            }
        }
    }
  else
    {
      if (Denemo.palettes)
        {
          GList *g;
          for (g = Denemo.palettes; g; g = g->next)
            {
              DenemoPalette *pal = (DenemoPalette *) g->data;
              if (!scm_is_false (show))
                {
                  gtk_widget_show (gtk_widget_get_parent (pal->box));
                  gtk_widget_show_all (pal->box);
                }
              else
                {
                  gtk_widget_hide (pal->box);
                  gtk_widget_hide (gtk_widget_get_parent (pal->box));
                }
            }
          return SCM_BOOL_T;
        }
      else
        return SCM_BOOL_F;
    }
  return SCM_BOOL_F;
}

SCM
scheme_select_palette (SCM aname)
{
  gchar *name = NULL;
  if (scm_is_string (aname))
    name = scm_to_locale_string (aname);
  else if (!SCM_UNBNDP (aname))
    name = choose_palette_by_name (FALSE, FALSE);
  if (name)
    {
      DenemoPalette *pal = get_palette (name);
      if (pal)
        Denemo.currentpalette = pal;
    }
  if (Denemo.currentpalette)
    return scm_from_locale_string (Denemo.currentpalette->name);
  return SCM_BOOL_F;
}

static gint
flash_input_cursor (void)
{
  if (Denemo.input_filters->len > 0)
    {
      if (g_str_has_suffix (Denemo.input_filters->str, "|"))
        g_string_truncate (Denemo.input_filters, Denemo.input_filters->len - 1);
      else
        g_string_append_c (Denemo.input_filters, '|');
    }
  write_input_status ();
  return TRUE;
}

static void
choose_palette (GtkWidget * button)
{
  GtkWidget *win = gtk_widget_get_toplevel (button);
  gchar *name = choose_palette_by_name (FALSE, FALSE);
  if (name)
    {
      DenemoPalette *pal = get_palette (name);
      if (pal)
        {
          Denemo.currentpalette = pal;
          gtk_window_set_title (GTK_WINDOW (win), pal->name);
        }
    }
}

static gboolean
check_character (GtkWidget * entry, GdkEventKey * event, GtkWidget * dialog)
{
  if (event->keyval == GDK_KEY_Tab)
    {
      choose_palette (entry);
      return TRUE;
    }
  if (event->keyval == GDK_KEY_Return)
    {
      gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
      return TRUE;
    }
  return FALSE;
}

static gchar *
label_entry (gchar * wlabel, gchar * direction)
{
  GtkWidget *dialog;
  GtkWidget *entry;
  GtkWidget *label;
  gchar *entry_string;
  GString *string;
  entry = gtk_entry_new ();
  dialog = gtk_dialog_new ();
  gtk_window_set_title ((GtkWindow *) dialog, wlabel);
  gtk_window_set_transient_for ((GtkWindow *) dialog, GTK_WINDOW (Denemo.window));
  gtk_window_set_destroy_with_parent ((GtkWindow *) dialog, TRUE);
  g_signal_connect (G_OBJECT (entry), "key-press-event", G_CALLBACK (check_character), dialog);
  label = gtk_label_new (direction);
  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_container_add (GTK_CONTAINER (content_area), label);
  gtk_container_add (GTK_CONTAINER (content_area), entry);

  label = gtk_label_new (_("Use <Tab> to switch palette\n<Return>to activate"));
  gtk_container_add (GTK_CONTAINER (content_area), label);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (Denemo.window));
  gtk_window_set_keep_above (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);
  gtk_widget_grab_focus (entry);
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      entry_string = (gchar *) gtk_entry_get_text (GTK_ENTRY (entry));
      string = g_string_new (entry_string);
      gtk_widget_destroy (dialog);
      return g_string_free (string, FALSE);
    }
  else
    {
      gtk_widget_destroy (dialog);
      return NULL;
    }
  return NULL;
}

SCM
scheme_activate_palette_button (void)
{
  GdkEventKey event;
  GString *input = g_string_new ("");

  SCM ret = SCM_BOOL_F;
  gint id = 0;

  if (Denemo.palettes)
    {
      DenemoPalette *pal = Denemo.currentpalette;
      if (pal == NULL)
        pal = (DenemoPalette *) Denemo.palettes->data;
      gchar *str = label_entry (pal->name, _("Key in (part of) label"));
      pal = Denemo.currentpalette;
      if (str)
        {
          g_string_assign (input, str);
          g_free (str);
          if (input->len && palette_action_button (pal, input->str))
            {
              ret = SCM_BOOL_T;
            }
          else
            {
              infodialog (_("No such label"));
              gtk_widget_show (gtk_widget_get_parent (pal->box));
              gtk_widget_show_all (pal->box);
            }
        }
    }
  return ret;
}

SCM
scheme_get_offset (void)
{
#ifndef USE_EVINCE
  g_debug ("This feature requires denemo to be built with evince");
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

SCM
scheme_get_control_point (SCM pt)
{
#ifndef USE_EVINCE
  g_debug ("This feature requires denemo to be built with evince");
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

SCM
scheme_get_curve (void)
{
#ifndef USE_EVINCE
  g_debug ("This feature requires denemo to be built with evince");
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

SCM
scheme_get_positions (SCM is_slur)
{
#ifndef USE_EVINCE
  g_debug ("This feature requires denemo to be built with evince");
#else
  gdouble neary, fary;
  if (get_positions (&neary, &fary, scm_is_true (is_slur) ? Slur : Beam))
    {
      return scm_cons (scm_from_double (neary), scm_from_double (fary));
    }
#endif
  return SCM_BOOL_F;
}

SCM
scheme_get_new_target (void)
{
#ifndef USE_EVINCE
  g_debug ("This feature requires denemo to be built with evince");
  return SCM_BOOL_F;
#else
  return scm_from_bool (get_new_target ());
#endif
}

SCM
scheme_get_new_point (void)
{
#ifndef USE_EVINCE
  g_debug ("This feature requires denemo to be built with evince");
  return SCM_BOOL_F;
#else
  return scm_from_bool (get_new_point ());
#endif
}

SCM
scheme_get_reference_point (void)
{
#ifndef USE_EVINCE
  g_debug ("This feature requires denemo to be built with evince");
  return SCM_BOOL_F;
#else
  return scm_from_bool (get_reference_point ());
#endif
}

SCM
scheme_get_target_info (void)
{
  DenemoMovement *si = Denemo.project->movement;
  if (Denemo.project->movement->currentobject == NULL)
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
      g_warning ("Unknown target type %d", si->target.type);
      type = SCM_BOOL_F;
      break;
    }

  if (si->target.type == TARGET_NOTE)
    {
      DenemoObject *obj = si->currentobject->data;
      if (obj->type == CHORD)
        {
          chord *thechord = ((chord *) ((DenemoObject *) obj->object));
          if (thechord->figure)
            grob = scm_from_locale_string ("BassFigure");
        }
    }


  if (si->target.directivenum || (si->target.type == TARGET_OBJECT))
    {
      DenemoDirective *directive = NULL;
      DenemoObject *obj = si->currentobject->data;
      if (si->target.type == TARGET_CHORD)
        {
          if (obj->type == CHORD)
            {
              GList *directives = ((chord *) ((DenemoObject *) obj->object))->directives;
              if (directives)
                {
                  directive = (DenemoDirective *) g_list_nth_data (directives, si->target.directivenum - 1);
                }
            }
        }
      else if (si->target.type == TARGET_NOTE)
        {
          if (obj->type == CHORD)
            {
              directive = get_note_directive_number (si->target.directivenum);
            }
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
      if (directive && directive->tag)
        {
          tag = scm_from_locale_string (directive->tag->str);
        }
    }
  return scm_list_n (type, grob, tag, SCM_UNDEFINED);
}


//FIXME inelegant!
static gint
interpret_lilypond_notename (gchar * x, gint * mid_c_offset, gint * enshift)
{
  //g_debug("Mid c offset of %d\n", *x-'c');
  gchar *c;
  gint octave = -1;             /* middle c is c' */
  gint accs = 0;

  for (c = x + 1; *c; c++)
    {
      if (*c == 'i' && *(c + 1) == 's')
        {
          accs++;
          c++;
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
SCM
scheme_execute_init (gchar * menupath)
{
  GList *dirs = NULL;
  dirs = g_list_append (dirs, g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "menus", menupath, NULL));
  dirs = g_list_append (dirs, g_build_filename (get_system_data_dir (), COMMANDS_DIR, "menus", menupath, NULL));

  gchar *path = find_path_for_file (INIT_SCM, dirs);

  if (path)
    {
      g_message ("About to load from %s", path);
      eval_file_with_catch (path);      //ret = scm_c_primitive_load(filename);
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
SCM
scheme_initialize_script (SCM action_name)
{
  SCM ret = SCM_BOOL_T;
  char *name = scm_to_locale_string (action_name);

  gint idx = lookup_command_from_name (Denemo.map, name);
  command_row *row = NULL;
  if (keymap_get_command_row (Denemo.map, &row, idx))
    ret = scheme_execute_init (row->menupath);
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
SCM
scheme_load_command (SCM command)
{
  gboolean ret;
  char *name;
  name = scm_to_locale_string (command);
  GList *files = NULL;
  files = g_list_append (files, g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "menus", name, NULL));
  files = g_list_append (files, g_build_filename (get_user_data_dir (TRUE), "download", COMMANDS_DIR, name, NULL));
  files = g_list_append (files, g_build_filename (get_system_data_dir (), COMMANDS_DIR, name, NULL));
  ret = load_keymap_files (files);

  if (name)
    free (name);
  return SCM_BOOL (ret);
}

SCM
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


SCM
scheme_hide_buttons (SCM hide)
{
  SCM ret = SCM_BOOL_F;
  GtkWidget *widget = Denemo.project->buttonbox;
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

SCM
scheme_destroy_buttons (void)
{
  SCM ret = SCM_BOOL_F;
  GtkWidget *widget = Denemo.project->buttonbox;

  if (GTK_IS_CONTAINER (widget))
    {
      gtk_container_foreach (GTK_CONTAINER (widget), (GtkCallback) gtk_widget_destroy, NULL);
      ret = SCM_BOOL_T;
    }
  return ret;
}


/* hide all menus, leaving only the score titles, used for educational games */
SCM
scheme_hide_menus (SCM hide)
{
  if (Denemo.project->view != DENEMO_MENU_VIEW)
    {
      set_toggle (ToggleScoreTitles_STRING, FALSE);
      ToggleReduceToDrawingArea (NULL, NULL);
      return SCM_BOOL (TRUE);
    }
  gboolean show = FALSE;
  if (scm_is_false (hide))
    show = TRUE;
  if (!Denemo.non_interactive)
    {
      toggle_to_drawing_area (show);
      set_toggle (ToggleScoreTitles_STRING, FALSE);
    }
  return SCM_BOOL (TRUE);
}

SCM
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

SCM
scheme_activate_object (void)
{
  activate_right_click (Denemo.keyboard_state);
  GString *modname = mouse_shortcut_name (Denemo.keyboard_state, GESTURE_PRESS, FALSE);
  gint command_idx = lookup_command_for_keybinding_name (Denemo.map, modname->str);
  if (command_idx>=0) 
    execute_callback_from_idx (Denemo.map, command_idx);
  g_string_free (modname, TRUE);
  return SCM_BOOL_T;
}

/* when a script calls a command which is itself a script it comes through here */
SCM
scheme_script_callback (SCM script, SCM params)
{
  char *name = NULL;
  SCM ret = SCM_BOOL_F;
  if (scm_is_string (script))
    {
      name = scm_to_locale_string (script);
      gint idx = lookup_command_from_name (Denemo.map, name);
      if (name)
        {
          if (!is_action_name_builtin (name))
            {
              gchar *paramvar = g_strdup_printf ("%s::params", name);
              scm_c_define (paramvar, params);

              gchar *text = get_scheme_from_idx (idx);
              if (text)
                {
                  //undo is a queue so this is the end :)
                  stage_undo (Denemo.project->movement, ACTION_STAGE_END);
                  ret = SCM_BOOL (!call_out_to_guile (text));
                  stage_undo (Denemo.project->movement, ACTION_STAGE_START);
                }
              else if (!Denemo.non_interactive)
                {
                  DenemoAction *action = lookup_action_from_name (name);
                  ret = SCM_BOOL (activate_script (action, NULL));
                }
              else
                g_warning ("Could not execute %s script", name);
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

  //g_debug("Defining %s\n", def);
  call_out_to_guile (def);
  g_free (proc);
  g_free (value);
  g_free (def);

  // define_scheme_literal_variable(proc, value, "A scheme procedure to call the script of that name");
}

SCM
scheme_swap_display_and_source (void)
{
  gint root_x, root_y, sx, sy;
  
  if (get_source_position (&sx, &sy))
    {
      gtk_window_get_position (GTK_WINDOW (Denemo.window), &root_x, &root_y);
      move_source_window (root_x, root_y);
      gtk_window_move (GTK_WINDOW (Denemo.window), sx, sy);
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}
SCM
scheme_debug_object (SCM optional)
{
  DenemoObject *curObj;

  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data))
    return SCM_BOOL (FALSE);
  g_debug ("*************\nType = %d\nbasic_durinticks = %d\ndurinticks - %d\nstarttickofnextnote = %d\n***********\n", curObj->type, curObj->basic_durinticks, curObj->durinticks, curObj->starttickofnextnote);
  return SCM_BOOL (TRUE);
}

SCM
scheme_display_object (void)
{
  DenemoObject *curObj;

  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data))
    return SCM_BOOL (FALSE);
  display_current_object ();
  return SCM_BOOL (TRUE);
}

SCM
scheme_get_editing_time (void)
{
  SCM ret;
  gchar *edit_time = time_spent_editing ();
  ret = scm_from_locale_string (edit_time);
  g_free (edit_time);
  return ret;
}

SCM
scheme_destroy_scheme_init (void)
{
  if (confirm (_("Destroying Customized Buttons"), _("Remove buttons and other customized scheme on startup?")))
    {
      destroy_local_scheme_init ();
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}



SCM
scheme_append_scheme_text (SCM script)
  {
    if (scm_is_string (script))
    {
      gchar *text = scm_to_locale_string (script);
      appendSchemeText (text);
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
  }
SCM
scheme_get_scheme_text (void)
  {
  return scm_from_locale_string (get_script_view_text());
  }
  
SCM
scheme_load_keybindings (SCM name)
{
  char *filename;
  if (scm_is_string (name))
    {
      filename = scm_to_locale_string (name);
      GList *files = NULL;

      files = g_list_append (files, g_strdup (filename));
      files = g_list_append (files, g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, filename, NULL));
      files = g_list_append (files, g_build_filename (get_user_data_dir (TRUE), "download", COMMANDS_DIR, filename, NULL));
      files = g_list_append (files, g_build_filename (get_system_data_dir (), COMMANDS_DIR, filename, NULL));
      g_free (name);

      return SCM_BOOL (load_keymap_files (files));
    }
  //if (name) g_free(name); CHECKME
  return SCM_BOOL_F;
}

SCM
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

SCM
scheme_clear_keybindings (SCM optional)
{
  keymap_clear_bindings (Denemo.map);
  return SCM_BOOL_T;
}


SCM
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


SCM
scheme_push_clipboard (SCM optional)
{
  push_clipboard ();
  return SCM_BOOL_T;
}

SCM
scheme_pop_clipboard (SCM num)
{ 
  gint count = 0;
  if (scm_is_integer (num))
    count = scm_to_int (num);
  if (pop_clipboard (count))
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

SCM
scheme_delete_selection (SCM optional)
{
  if ((!Denemo.project->movement) || (!Denemo.project->movement->markstaffnum))
    return SCM_BOOL_F;
  delete_selection ();
  return SCM_BOOL_T;
}

SCM
scheme_set_thumbnail_selection (SCM optional)
{
  if ((!Denemo.project->movement) || (!Denemo.project->movement->markstaffnum))
    return SCM_BOOL_F;
  if (Denemo.project->movement == Denemo.project->movements->data)
    {
      memcpy (&Denemo.project->thumbnail, &Denemo.project->movement->selection, sizeof (DenemoSelection));
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

SCM
scheme_set_newbie (SCM optional)
{
  SCM ret = SCM_BOOL (Denemo.prefs.newbie);
  if (scm_is_true (optional))
    {
      Denemo.prefs.tooltip_timeout = 1000;
      Denemo.prefs.tooltip_browse_timeout = 700;
      Denemo.prefs.tooltip_browse_mode_timeout = 1000;

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

SCM
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

SCM
scheme_create_thumbnail (SCM optional, SCM filename)
{
#ifndef USE_EVINCE
  g_debug ("This feature requires denemo to be built with evince");
  return SCM_BOOL_F;
#else
  gchar *path = scm_is_string (filename) ? scm_to_locale_string (filename) : NULL;
  gboolean ret;
  if ((!SCM_UNBNDP (optional)) && scm_is_true (optional))
    ret = create_thumbnail (TRUE, path);
  else
    ret = create_thumbnail (FALSE, path);
  return SCM_BOOL (ret);
#endif
}

SCM
scheme_exit (SCM optional)
{
  exit (0);
}

SCM
scheme_create_layout (SCM name)
{
  if (scm_is_string (name))
    {
      gchar *layout_name = scm_to_locale_string (name);
      return scm_from_bool (create_custom_scoreblock (layout_name, TRUE));
    }
  return SCM_BOOL_F;
}
SCM
scheme_set_pending_layout (SCM name)
{
  if (scm_is_string (name))
    {
      gchar *layout_name = scm_to_locale_string (name);
      
      Denemo.pending_layout_id = get_layout_id_for_name ((guchar *) layout_name);
    }
  else
    Denemo.pending_layout_id = 0;
  return scm_from_int ((guint)Denemo.pending_layout_id);
}

SCM
scheme_delete_layout (SCM name)
{
  if (scm_is_string (name))
    {
      gchar *layout_name = scm_to_locale_string (name);
      return scm_from_bool (delete_custom_scoreblock (layout_name));
    }
  return SCM_BOOL_F;
}

SCM
scheme_lilypond_for_part (void)
{
  gint save = Denemo.project->movement->markstaffnum;
  Denemo.project->movement->markstaffnum = 0;
  if (!select_custom_layout_for_name (((DenemoStaff *) (Denemo.project->movement->currentstaff->data))->lily_name->str))
    generate_lilypond_part ();
  Denemo.project->movement->markstaffnum = save;
  return SCM_BOOL_T;
}

SCM
scheme_typeset_part (void)
{
#ifndef USE_EVINCE
  g_debug ("This feature requires denemo to be built with evince");
  return SCM_BOOL_F;
#else
  typeset_part ();
  return SCM_BOOL_T;
#endif
}
SCM
scheme_typeset_current_movement (void)
{
#ifndef USE_EVINCE
  g_debug ("This feature requires denemo to be built with evince");
  return SCM_BOOL_F;
#else
  typeset_current_movement ();
  return SCM_BOOL_T;
#endif
}

SCM
scheme_reduce_layout_to_lilypond (void)
{
  make_scoreblock_editable ();
  return SCM_BOOL_T;
}

SCM
scheme_get_current_staff_layout_id (void)
{
  guint id;
  if (((DenemoStaff *) (Denemo.project->movement->currentstaff->data))->voicecontrol == DENEMO_PRIMARY)
    {
      id = get_layout_id_for_name (((DenemoStaff *) (Denemo.project->movement->currentstaff->data))->lily_name->str);
      return scm_from_int (id);
    }
  return SCM_BOOL_F;
}

SCM
scheme_get_layout_id (void)
{
  DenemoScoreblock *sb = (DenemoScoreblock *) selected_scoreblock ();
  if (sb)
    return scm_from_int (sb->id);
  return SCM_BOOL_F;
}

SCM
scheme_select_layout_id (SCM the_id)
{
  if (scm_is_integer (the_id))
    {
      gint id = scm_to_int (the_id);
      return SCM_BOOL (select_layout_id (id));
    }
  return SCM_BOOL_F;
}

SCM
scheme_select_default_layout (void)
{
  select_default_scoreblock ();
  return SCM_BOOL_T;
}

SCM
scheme_get_layout_name (void)
{
  DenemoScoreblock *sb = (DenemoScoreblock *) selected_scoreblock ();
  if (sb && sb->name)
    return scm_from_locale_string (sb->name);
  return SCM_BOOL_F;
}

SCM
scheme_select_next_layout (void)
{
  if (gtk_widget_get_visible (Denemo.project->score_layout))
    {
      DenemoScoreblock *sb = (DenemoScoreblock *) get_next_scoreblock ();
      return sb ? SCM_BOOL_T : SCM_BOOL_F;
    }
  return SCM_BOOL_F;
}

SCM
scheme_select_first_layout (void)
{
  if (gtk_widget_get_visible (Denemo.project->score_layout))
    {
      DenemoScoreblock *sb = (DenemoScoreblock *) get_first_scoreblock ();
      return sb ? SCM_BOOL_T : SCM_BOOL_F;
    }
  return SCM_BOOL_F;
}


SCM
scheme_select_next_custom_layout (void)
{
  return SCM_BOOL (iterate_custom_layout (FALSE));
}

SCM
scheme_select_first_custom_layout (void)
{
  return SCM_BOOL (iterate_custom_layout (TRUE));
}

SCM
scheme_get_filename (void)
{
  if (Denemo.project && Denemo.project->filename && Denemo.project->filename->len && strcmp ("Unnamed", Denemo.project->filename->str))
    return scm_from_locale_string (Denemo.project->filename->str);
  return SCM_BOOL_F;
}

SCM
scheme_clear_filename (void)
{
  if (Denemo.project && Denemo.project->filename)
    {
      set_project_filename (Denemo.project, "");
      return SCM_BOOL_T;
    } else
    return SCM_BOOL_F;
}


SCM
scheme_select_tab (SCM index)
{
  if (scm_is_integer (index))
    {
      gint num = scm_to_int (index);
      gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), num);
    }
  return scm_from_int (gtk_notebook_get_current_page (GTK_NOTEBOOK (Denemo.notebook)));
}
/* compare objects at the cursor in the two passed tabs. If move is true
 * move the cursor to the next object before doing the comparison.
 * If move fails on both tabs return #f
 * If it fails on one tab only return a pair (error-message #f)
 * Otherwise (if there is no move or it succeeds on both) call compare_objects
 * to find any mis-matching objects.
 * If it returns NULL - all objects match and end of staff reached, return #f
 * else return a pair (error-message #t)
 */
SCM
scheme_compare_objects (SCM index1, SCM index2, SCM move)
{
  SCM ret = SCM_BOOL_F;
  DenemoMovement *si;
  gboolean skip = scm_is_true (move);
  if (scm_is_integer (index1) && scm_is_integer (index2))
    {
      gint original_index = gtk_notebook_get_current_page (GTK_NOTEBOOK (Denemo.notebook));
      gint num1 = scm_to_int (index1);
      gint num2 = scm_to_int (index2);
      
      gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), num1);//sets Denemo.project to new value
      si = Denemo.project->movement;
      gint move1 = TRUE;
      
      if (skip) 
        move1 = cursor_to_next_object (FALSE, FALSE);
      GList *curobj1 = si->currentobject;
      GList *curmeasure1 = si->currentmeasure;
      gint curmeasurenum1 = si->currentmeasurenum;
      gint curobjnum1 = si->cursor_x;
      gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), num2);
      si = Denemo.project->movement;
      gint move2 = TRUE;
      if (skip) 
        move2 = cursor_to_next_object (FALSE, FALSE);
      GList *curobj2 = si->currentobject;
      GList *curmeasure2 = si->currentmeasure;
      gint curmeasurenum2 = si->currentmeasurenum;
      gint curobjnum2 = si->cursor_x;
      if ((!move1) && (!move2))
        {
            ret = SCM_BOOL_F;
        }
      else
        {
          if ((!move1) || (!move2))
            {
              ret = scm_cons (scm_from_locale_string (_("Extra Objects in one staff")), SCM_BOOL_F);
            }
          else
            {
              gchar *status = compare_objects (curmeasure1, curobj1, &curmeasurenum1, &curobjnum1, curmeasure2, curobj2, &curmeasurenum2, &curobjnum2);
              
              if (status)
                {
                  ret = scm_cons (scm_from_locale_string (status), SCM_BOOL_T);
                  g_free (status);
                  gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), num1);
                  goto_movement_staff_obj  (NULL, -1, -1, curmeasurenum1, curobjnum1+1, 0);
                  gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), num2);
                  goto_movement_staff_obj  (NULL, -1, -1, curmeasurenum2, curobjnum2+1, 0);
                }
              else
                ret = SCM_BOOL_F;
              gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), original_index);
            }
        }
      }
  return ret;
}
//looks for a difference in current staffs in the passed tabs (not the music in the staff) moves to the staff below if move is not #f
SCM
scheme_difference_of_staffs (SCM index1, SCM index2, SCM move)
{
  if (scm_is_integer (index1) && scm_is_integer (index2))
    {
      gint original_index = gtk_notebook_get_current_page (GTK_NOTEBOOK (Denemo.notebook));
      gint num1 = scm_to_int (index1);
      gint num2 = scm_to_int (index2);
      gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), num1);
    
      DenemoStaff *staff1 = (DenemoStaff *)Denemo.project->movement->currentstaff->data;
      if (scm_is_true (move))
        movetostaffdown (NULL, NULL);
      gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), num2);

      DenemoStaff *staff2 =  (DenemoStaff *)Denemo.project->movement->currentstaff->data;
      if (scm_is_true (move))
        movetostaffdown (NULL, NULL);
      gchar *compare = difference_of_staffs (staff1, staff2);
      gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), original_index);
      if (compare)
        { SCM ret;
          ret = scm_from_locale_string (compare);
          g_free (compare);
          return ret;
        }
    }
  return SCM_BOOL_F;
}

SCM
scheme_difference_of_projects (SCM index1, SCM index2)
{
  if (scm_is_integer (index1) && scm_is_integer (index2))
    {
      gint original_index = gtk_notebook_get_current_page (GTK_NOTEBOOK (Denemo.notebook));
      gint num1 = scm_to_int (index1);
      gint num2 = scm_to_int (index2);
      gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), num1);
    
      DenemoProject *p1 = Denemo.project;
      
      gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), num2);
      DenemoProject *p2 = Denemo.project;
      //DenemoLilyControl *lc1 = p1->lilycontrol; there are papersize etc which i'm ignoring...
      //DenemoLilyControl *lc2 = p2->lilycontrol;

      gchar *compare = difference_of_directive_lists (p1->lilycontrol.directives, p2->lilycontrol.directives);
      if (!compare)
        compare = difference_of_directive_lists (p1->scoreheader.directives, p2->scoreheader.directives);
      if (!compare)
        compare = difference_of_directive_lists (p1->paper.directives, p2->paper.directives);
       
#if 0        
      if (!compare)
        {
          gchar *text = _("Paper Directive: ");
          compare = difference_of_directive_list (p1->paper.directives, p2->paper.directives);
          if (compare)
            {
              text = strdup_printf ("%s %s", compare);
              g_free (compare);
              compare = text;
            }
        }
#endif

      gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), original_index);
      if (compare)
        { SCM ret;
          ret = scm_from_locale_string (compare);
          g_free (compare);
          return ret;
        }
      
    }
  return SCM_BOOL_F;
}


SCM 
scheme_wakeup (void)
{
  while (gtk_events_pending ())
  gtk_main_iteration ();
  return SCM_BOOL_T;
}

SCM
scheme_difference_of_movements (SCM index1, SCM index2)
{
  if (scm_is_integer (index1) && scm_is_integer (index2))
    {
      gint original_index = gtk_notebook_get_current_page (GTK_NOTEBOOK (Denemo.notebook));
      gint num1 = scm_to_int (index1);
      gint num2 = scm_to_int (index2);
      gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), num1);
    
      DenemoMovement *mv1 = Denemo.project->movement;
      
      gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), num2);
      DenemoMovement *mv2 = Denemo.project->movement;
      //DenemoLilyControl *lc1 = mv1->lilycontrol; there are papersize etc which i'm ignoring...
      //DenemoLilyControl *lc2 = mv2->lilycontrol;

      gchar *compare = difference_of_directive_lists (mv1->movementcontrol.directives, mv2->movementcontrol.directives);
      if (!compare)
        compare = difference_of_directive_lists (mv1->header.directives, mv2->header.directives);
      if (!compare)
        compare = difference_of_directive_lists (mv1->layout.directives, mv2->layout.directives);
       
#if 0        
      if (!compare)
        {
          gchar *text = _("Paper Directive: ");
          compare = difference_of_directive_list (mv1->paper.directives, mv2->paper.directives);
          if (compare)
            {
              text = strdup_printf ("%s %s", compare);
              g_free (compare);
              compare = text;
            }
        }
#endif

      gtk_notebook_set_current_page (GTK_NOTEBOOK (Denemo.notebook), original_index);
      if (compare)
        { SCM ret;
          ret = scm_from_locale_string (compare);
          g_free (compare);
          return ret;
        }
      
    }
  return SCM_BOOL_F;
}
  
SCM
scheme_path_from_filename (SCM filepath)
{
  SCM ret = SCM_BOOL_F;
  if (scm_is_string (filepath))
    {
      char *temp = scm_to_locale_string (filepath);
      gchar *dirname = g_path_get_dirname (temp);
      free (temp);
      ret = scm_from_locale_string (dirname);
      g_free (dirname);
    }
  return ret;
}

SCM
scheme_filename_from_path (SCM filepath)
{
  SCM ret = SCM_BOOL_F;
  if (scm_is_string (filepath))
    {
      char *temp = scm_to_locale_string (filepath);
      gchar *dirname = g_path_get_basename (temp);
      free (temp);
      ret = scm_from_locale_string (dirname);
      g_free (dirname);
    }
  return ret;
}

SCM
scheme_file_exists (SCM filepath)
{
  SCM ret = SCM_BOOL_F;
  if (scm_is_string (filepath))
    {
      char *temp = scm_to_locale_string (filepath);
      return SCM_BOOL (g_file_test (temp, G_FILE_TEST_EXISTS));
    }
  return ret;
}

static SCM
scheme_choose_file_or_directory (SCM title, SCM startdir, SCM list, gboolean dir)
{
  gchar *thetitle = g_strdup (_("Choose File"));
  gchar *thedir = get_project_dir ();
  GList *exts = NULL;
  gchar *filename;
  SCM ret = SCM_BOOL_F;
  if (scm_is_string (title))
    {
      char *temp = scm_to_locale_string (title);
      thetitle = g_strdup (temp);
      free (temp);
    }
  if (scm_is_string (startdir))
    {
      char *temp = scm_to_locale_string (startdir);
      g_free (thedir);
      thedir = g_strdup (temp);
      free (temp);
    }
  if (scm_is_list (list))
    {
      gint length = scm_to_int (scm_length (list));
      int i;
      for (i = 0; i < length; i++)
        {
          SCM glob = scm_list_ref (list, scm_from_int (i));
          if (scm_is_string (glob))
            {
              char *extension = scm_to_locale_string (glob);
              exts = g_list_append (exts, g_strdup (extension));
              free (extension);
            }
        }
    }
  if(dir)
   filename = choose_directory (thetitle, thedir, exts);
  else
   filename = choose_file (thetitle, thedir, exts);
  g_free (thetitle);
  g_free (thedir);
  g_list_free_full (exts, g_free);
  if (filename)
    ret = scm_from_locale_string (filename);
  g_free (filename);
  return ret;
}

SCM
scheme_choose_file (SCM title, SCM startdir, SCM list)
{
    return scheme_choose_file_or_directory(title, startdir, list, FALSE);
}
SCM
scheme_choose_directory (SCM title, SCM startdir, SCM list)
{
    return scheme_choose_file_or_directory(title, startdir, list, TRUE);
}
SCM
scheme_edit_graphics (SCM name, SCM newname)
{
  SCM ret = SCM_BOOL_F;
  gchar *opened = NULL;
  gchar *thenewname = NULL;
  if (scm_is_string (newname))
    thenewname = scm_to_locale_string (newname);
  if (scm_is_string (name))
    {
      gchar *filename = scm_to_locale_string (name);

      opened = edit_graphics_file (filename, thenewname);
      free (filename);
    }
  else
    {
      opened = edit_graphics_file (NULL, thenewname);
    }
  if (opened)
    ret = scm_from_locale_string (opened);
  g_free (opened);
  if (thenewname)
    free (thenewname);
  return ret;
}

SCM
scheme_open_source (SCM link)
{
  SCM ret = SCM_BOOL_F;
#ifndef USE_EVINCE
  g_debug ("This feature requires denemo to be built with evince");
#else
  if (scm_is_string (link))
    {
      gchar *thestring = scm_to_locale_string (link);
      gchar *filename;
#ifdef G_OS_WIN32
      filename = thestring;
      (void) strtok (thestring + 2, ":");       //skip leading drive name on windows
#else
      filename = strtok (thestring, ":");       //will not work if filename contains ':' characters.
#endif
      if (filename)
        {
          gint x, y, page;

          gchar *xstr = strtok (NULL, ":");
          gchar *ystr = strtok (NULL, ":");
          gchar *pstr = strtok (NULL, ":");
          x = xstr ? atoi (xstr) : 0;
          y = ystr ? atoi (ystr) : 0;
          page = pstr ? atoi (pstr) : 0;
          //check on file exists otherwise try Denemo.prefs.denemopath
          if(!Denemo.non_interactive)
            if (!g_file_test (filename, G_FILE_TEST_IS_REGULAR))
              {
                gchar *base = Denemo.prefs.denemopath->str;
                gchar *found = try_to_find_file (base, filename);

                while (!found)
                  { gchar *message = g_strdup_printf ("%s%s%s", _("Unable to find file: "), filename, _("\nChoose a directory (below which to search)\nin the next dialog"));
                    gchar *path; 
                    warningdialog (message);
                    path = choose_directory (_("Give Toplevel Directory"), Denemo.prefs.denemopath->str, NULL);
                    if (path)
                      found = try_to_find_file (path, filename);
                    else
                      break;
                  }
                if (found)
                  filename = found;
              }

          if (open_source (filename, x, y, page))
            ret = SCM_BOOL_T;
        }
      if (thestring)
        free (thestring);
    }
#endif
  return ret;
}

SCM
scheme_open_source_file (SCM optional)
{
  if (open_source_file ())
    return SCM_BOOL_T;
  return SCM_BOOL_F;
}

SCM
scheme_open_proofread_file (SCM optional)
{
  if (open_proof_file ())
    return SCM_BOOL_T;
  return SCM_BOOL_F;
}

#ifdef DISABLE_AUBIO
#else
SCM
scheme_export_recorded_audio (void)
{

  return SCM_BOOL (export_recorded_audio ());

}

SCM
scheme_open_source_audio_file (SCM optional)
{
  if (open_source_audio_file () && Denemo.project->movement->recording && Denemo.project->movement->recording->samplerate)
    {
      return scm_from_double (Denemo.project->movement->recording->nframes / (double) Denemo.project->movement->recording->samplerate);
    }
  return SCM_BOOL_F;
}

SCM
scheme_close_source_audio (SCM optional)
{
  return SCM_BOOL (close_source_audio ());
}

SCM
scheme_start_audio_play (SCM annotate)
{
  if (Denemo.project->movement->recording)
    {
      start_audio_playing (scm_is_true (annotate));
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

SCM
scheme_set_audio_lead_in (SCM seconds)
{
  if (scm_is_real (seconds))
    {
      gdouble secs = scm_to_double (seconds);
      return SCM_BOOL (set_lead_in (secs));
    }
  return SCM_BOOL_F;
}

SCM
scheme_stop_audio_play (SCM annotate)
{
  if (audio_is_playing ())
    {
      stop_audio_playing ();
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

SCM
scheme_next_audio_timing (SCM optional)
{
  if (Denemo.project->movement->recording)
    {
      gdouble timing = get_audio_timing ();
      if (timing > 0.0)
        return scm_from_double (timing);
    }
  return SCM_BOOL_F;
}
#endif

SCM
scheme_audio_is_playing (void)
{
  return SCM_BOOL (/*audio_*/is_playing ()); //the  audio_is_playing() is AUBIO only, for playing source audio. So the name of this command d-AudioIsPlaying is now less than ideal.
}

SCM
scheme_take_snapshot (SCM optional)
{
  return SCM_BOOL (take_snapshot ());
}

SCM
scheme_increase_guard (SCM optional)
{
  if (Denemo.project->movement->undo_guard++)
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

SCM
scheme_decrease_guard (SCM optional)
{
  if (Denemo.project->movement->undo_guard > 0)
    return SCM_BOOL (!--Denemo.project->movement->undo_guard);
  Denemo.project->movement->undo_guard = 0;
  return SCM_BOOL_T;
}

//From a script undo must undo only the modifications to the start of the
//script, and push another STAGE_END for the end of the actions that it will do
//after the invocation of undo. This function overrides the built-in undo called
//directly by the user.
SCM
scheme_undo (SCM optional)
{
  stage_undo (Denemo.project->movement, ACTION_STAGE_START);
  undowrapper (NULL, NULL);
  stage_undo (Denemo.project->movement, ACTION_STAGE_END);
  return SCM_BOOL_T;
}

//Break the script up for undo purposes
SCM
scheme_stage_for_undo (SCM optional)
{
  stage_undo (Denemo.project->movement, ACTION_STAGE_START);
  stage_undo (Denemo.project->movement, ACTION_STAGE_END);
  return SCM_BOOL_T;
}

SCM
scheme_get_last_change (SCM optional)
{
  SCM ret = SCM_BOOL_F;
  gchar *last = get_last_change (Denemo.project->movement);
  if (last)
    ret = scm_from_locale_string (last);
  g_free (last);
  return ret;
}




SCM
scheme_new_window (SCM optional)
{
  stage_undo (Denemo.project->movement, ACTION_STAGE_START);

  //gint current =  Denemo.project->scorearea->allocation.width;
  newview (NULL, NULL);
  // Denemo.project->scorearea->allocation.width = current;

  stage_undo (Denemo.project->movement, ACTION_STAGE_END);
  return SCM_BOOL_T;
}


SCM
scheme_zoom (SCM factor)
{
  if (scm_is_real (factor))
    Denemo.project->movement->zoom = scm_to_double (factor);
  else if (scm_is_string (factor))
    {
      char *name;
      name = scm_to_locale_string (factor);
      if (name)
        {
          Denemo.project->movement->zoom = atof (name);
          free (name);
        }
    }
  else
    {
      return scm_from_double (Denemo.project->movement->zoom);
    }
  scorearea_configure_event (Denemo.scorearea, NULL);
  if (Denemo.project->movement->zoom > 0.01)
    {
      return scm_from_int (Denemo.project->movement->zoom);
    }
  Denemo.project->movement->zoom = 1.0;
  return SCM_BOOL_F;
}

SCM
scheme_master_tempo (SCM factor)
{
  DenemoMovement *si = Denemo.project->movement;
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

SCM
scheme_movement_tempo (SCM bpm)
{
  DenemoMovement *si = Denemo.project->movement;
  if (scm_is_real (bpm))
    si->tempo = scm_to_int (bpm);
  if (scm_is_string (bpm))
    {
      char *name;
      name = scm_to_locale_string (bpm);
      if (name)
        {
          gdouble tempo = atof (name);
          si->tempo = tempo;
          set_master_tempo (si, si->master_tempo);
          free (name);
        }
    }

  if (si->tempo < 1)
    si->tempo = 120;
  return scm_from_int (si->tempo);
}

SCM
scheme_master_volume (SCM factor)
{
  DenemoMovement *si = Denemo.project->movement;
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

SCM
scheme_staff_master_volume (SCM level)
{
  DenemoStaff *thestaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (scm_is_real (level))
    {
      gdouble master_volume = scm_to_double (level);
      thestaff->volume = (gint) (master_volume * 127);
      if (thestaff->volume > 127)
        thestaff->volume = 127;
      if (thestaff->volume < 0)
        thestaff->volume = 0;

      return scm_from_double (thestaff->volume / 127.0);
    }
  if (scm_is_false (level))
    {
      thestaff->mute = TRUE;
      return scm_from_double (-thestaff->volume / 127.0);
    }
  if (scm_is_eq (level, SCM_UNDEFINED))
    return thestaff->mute ? scm_from_double (-thestaff->volume / 127.0) : scm_from_double (thestaff->volume / 127.0);
  thestaff->mute = FALSE;
  return scm_from_double (thestaff->volume / 127.0);
}

SCM
scheme_get_midi_tuning (void)
{
  gchar *cents = get_cents_string ();
  SCM ret = scm_from_locale_string (cents);
  g_free (cents);
  return ret;
}

SCM
scheme_get_sharpest (void)
{
  gchar *name = get_sharpest ();
  SCM ret = scm_from_locale_string (name);
  g_free (name);
  return ret;
}

SCM
scheme_get_flattest (void)
{
  gchar *name = get_flattest ();
  SCM ret = scm_from_locale_string (name);
  g_free (name);
  return ret;
}

SCM
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
  if (scm_is_false (scm_internal_catch (SCM_BOOL_T, (scm_t_catch_body) scm_c_lookup, (void *) "SetQuarterCommaMeanTone", (scm_t_catch_handler) ignore_handler, (void *) "whoops")))
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

SCM
scheme_rewind_midi (SCM start)
{
  DenemoProject *gui = Denemo.project;
  double thetime = 0.0;
  SCM scm = SCM_BOOL_T;
  gint err;
  if ((gui->movement->smf == NULL) || (gui->movement->smfsync != gui->movement->changecount))
    generate_midi ();
  if (scm_is_real (start))
    thetime = scm_to_double (start);
  if (thetime > 0.0)
    {
      err = smf_seek_to_seconds (gui->movement->smf, thetime);
      if (err)
        scm = SCM_BOOL_F;
    }
  else
    smf_rewind (gui->movement->smf);
  return scm;
}


SCM
scheme_next_midi_notes (SCM interval)
{
  SCM scm = scm_list_n (SCM_UNDEFINED);
  DenemoMovement *si = Denemo.project->movement;
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

SCM
scheme_get_midi_on_time (void)
{
  if (!(Denemo.project->movement->currentobject))
    return SCM_BOOL_F;
  GList *curObj = Denemo.project->movement->currentobject;
  while (curObj && (((DenemoObject *) curObj->data)->durinticks == 0))
    curObj = curObj->next;      //find first note/rest or duration-full object after the cursor
  if (curObj && (Denemo.project->movement->smfsync == Denemo.project->movement->changecount))
    return scm_from_double (((DenemoObject *) curObj->data)->earliest_time);
  return SCM_BOOL_F;
}

SCM
scheme_get_midi_off_time (void)
{
  if (!(Denemo.project->movement->currentobject))
    return SCM_BOOL_F;
  GList *curObj = Denemo.project->movement->currentobject;
  while (curObj && (((DenemoObject *) curObj->data)->durinticks == 0))
    curObj = curObj->prev;      //find last note/rest or duration-full object before the cursor
  if (curObj && (Denemo.project->movement->smfsync == Denemo.project->movement->changecount))
    return scm_from_double (((DenemoObject *) curObj->data)->latest_time);
  return SCM_BOOL_F;
}

SCM
scheme_midi_in_listening (void)
{
  midi_in_adjust (GDK_SHIFT_MASK);
  return SCM_BOOL_T;
}

SCM
scheme_midi_in_checking (void)
{
  midi_in_adjust (GDK_CONTROL_MASK);
  return SCM_BOOL_T;
}

SCM
scheme_midi_in_append_edit (void)
{
  midi_in_adjust (0);
  return SCM_BOOL_T;
}

SCM
scheme_restart_play (void)
{
  restart_play ();
  return SCM_BOOL_T;
}

SCM
scheme_staff_to_play (SCM num)
{
  if (scm_is_integer (num))
    {
      gint staffnum = scm_to_int (num);
      if (staffnum >= 0 && staffnum <= g_list_length (Denemo.project->movement->thescore))
        {
          Denemo.project->movement->stafftoplay = staffnum;
          exportmidi (NULL, Denemo.project->movement);
          return scm_from_int (staffnum);
        }
    }
  Denemo.project->movement->stafftoplay = 0;
  exportmidi (NULL, Denemo.project->movement);
  return SCM_BOOL_F;
}

static double
convert_and_adjust (SCM time)
{
  return scm_to_double (time) * get_playback_speed ();
}

SCM
scheme_set_playback_interval (SCM start, SCM end)
{
  stop_midi_playback (NULL, NULL);
  if (scm_is_real (start) && scm_is_real (end))
    {
      Denemo.project->movement->start_time = convert_and_adjust (start);
      Denemo.project->movement->end_time = convert_and_adjust (end);
      fix_start_end_ordering ();
      return SCM_BOOL_T;
    }
  if (scm_is_real (start))
    {
      Denemo.project->movement->start_time = convert_and_adjust (start);
      fix_start_end_ordering ();
      return SCM_BOOL_T;
    }
  if (scm_is_real (end))
    {
      Denemo.project->movement->end_time = convert_and_adjust (end);
      fix_start_end_ordering ();
      return SCM_BOOL_T;
    }
  if (scm_is_string (start) && scm_is_string (end))
    {
      char *name;
      name = scm_to_locale_string (start);
      if (name)
        {
          Denemo.project->movement->start_time = atof (name);
          free (name);
        }
      name = scm_to_locale_string (end);
      if (name)
        {
          Denemo.project->movement->end_time = atof (name);
          free (name);
        }
      fix_start_end_ordering ();
      return SCM_BOOL_T;
    }
  if (scm_is_string (start))
    {
      char *name;
      name = scm_to_locale_string (start);
      if (name)
        {
          Denemo.project->movement->start_time = atof (name);
          free (name);
        }
      fix_start_end_ordering ();
      return SCM_BOOL_T;
    }
  if (scm_is_string (end))
    {
      char *name;
      name = scm_to_locale_string (end);
      if (name)
        {
          Denemo.project->movement->end_time = atof (name);
          free (name);
        }
      fix_start_end_ordering ();
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

SCM
scheme_adjust_playback_start (SCM adj)
{
  SCM ret = SCM_BOOL_F;
  if (scm_is_real (adj))
    {
      stop_midi_playback (NULL, NULL);
      Denemo.project->movement->start_time += convert_and_adjust (adj);
      if (Denemo.project->movement->start_time < 0.0)
        Denemo.project->movement->start_time = 0.0;
      else
        ret = SCM_BOOL_T;
    }
  fix_start_end_ordering ();
  return ret;
}

SCM
scheme_adjust_playback_end (SCM adj)
{
  SCM ret = SCM_BOOL_F;
  if (scm_is_real (adj))
    {
      stop_midi_playback (NULL, NULL);
      if (Denemo.project->movement->end_time > 0)
        Denemo.project->movement->end_time += convert_and_adjust (adj);
      if (Denemo.project->movement->end_time > 0.0)
        ret = SCM_BOOL_T;
    }
  fix_start_end_ordering ();
  return ret;
}


SCM
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

SCM
scheme_email_help (SCM text)
{gchar *page;
  if (scm_is_string (text))
    {
      page = scm_to_locale_string (text);
      email_help (page);
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}


SCM
scheme_get_lily_version (SCM optional)
{
  gchar *version = get_lily_version_string ();
  return scm_from_locale_string (version);
}

SCM
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

SCM
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

SCM
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

SCM
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




SCM
scheme_get_menu_position (SCM command)
{
  char *name;
  SCM ret;
  gchar *menuposition = NULL;
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
  command_row *row = NULL;
  keymap_get_command_row (Denemo.map, &row, idx);
  if (name)
    free (name);
  if ((row==NULL) || (idx < 0))
    {
      return SCM_BOOL_F;
    }
  menuposition = get_menu_position (row->menupath);

  if (menuposition && *menuposition)
    {
      ret = scm_from_locale_string (menuposition);
      g_free (menuposition);
    }
  return ret;
}


SCM
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
  command_row *row = NULL;
  keymap_get_command_row (Denemo.map, &row, idx);
  if (name)
    free (name);
  if ((row==NULL) || (idx < 0))
    {
      return SCM_BOOL_F;
    }
  if (row->menupath == NULL)
    {
      return SCM_BOOL_F;
    }
  return scm_from_locale_string (row->menupath);
}

SCM
scheme_get_verse (SCM number)
{
  gchar *text = NULL;
  DenemoProject *gui = Denemo.project;
  if (scm_is_integer (number))
    {
      text = get_lyrics_for_verse_num (scm_to_int (number));
    }
  else
    {
      DenemoStaff *staff = (DenemoStaff *) gui->movement->currentstaff->data;
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
SCM
scheme_get_verse_number (SCM number)
{
 DenemoProject *gui = Denemo.project;
 if (scm_is_integer (number))
    {
        guint num = scm_to_int (number);
        if (verse_set_current ((DenemoStaff *) gui->movement->currentstaff->data, num))
            return SCM_BOOL_T;
    }
 else 
    {
     gint num = verse_get_current  ((DenemoStaff *) gui->movement->currentstaff->data);
     if (num>-1)
        return scm_from_int (num);
    }
  return SCM_BOOL_F;
}

SCM
scheme_syllable_count (void)
{
  if (Denemo.project->movement->currentobject)
    return scm_from_int (syllable_count ());
  return SCM_BOOL_F;
}

SCM
scheme_typeset_lyrics_for_staff (SCM on)
{
  DenemoStaff *staff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (scm_is_bool (on))
    {
        signal_structural_change (Denemo.project);
        staff->hide_lyrics = !scm_is_true (on);
        score_status (Denemo.project, TRUE);
    }
  return scm_from_bool (!staff->hide_lyrics);
}

SCM
scheme_synchronize_lyric_cursor (SCM val)
{
  gint offset = 0;
  if (scm_is_integer (val))
    offset = scm_to_int (val);
  return SCM_BOOL (synchronize_lyric_cursor (offset));

}

SCM
scheme_insert_text_in_verse (SCM text)
{
  if (scm_is_string (text))
    {
      char *thetext;
      thetext = scm_to_locale_string (text);
      return SCM_BOOL (insert_text_in_verse (thetext));
    }
  return SCM_BOOL_F;
}

SCM
scheme_put_verse (SCM verse)
{
  DenemoProject *gui = Denemo.project;
  DenemoStaff *staff = (DenemoStaff *) gui->movement->currentstaff->data;
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

SCM
scheme_append_to_verse (SCM verse)
{
  DenemoProject *gui = Denemo.project;
  DenemoStaff *staff = (DenemoStaff *) gui->movement->currentstaff->data;
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
SCM
scheme_input_filter_names (SCM filtername)
{
  char *name = NULL;

  if (scm_is_string (filtername))
    {
      name = scm_to_locale_string (filtername);
      if (name)
        {
          g_string_printf (Denemo.input_filters, "MIDI Input: %s", name);
          gtk_widget_show (Denemo.input_label);
          write_input_status ();
          free (name);
          return SCM_BOOL_T;
        }
    }
  else
    {
      gtk_widget_hide (Denemo.input_label);
    }
  return SCM_BOOL_F;
}

/* write a status label on bottom right of window*/
SCM
scheme_write_status (SCM filtername)
{
  char *name = NULL;

  if (scm_is_string (filtername))
    {
      name = scm_to_locale_string (filtername);
      if (name && Denemo.input_filters)
        {

          g_string_assign (Denemo.input_filters, name);
          gtk_widget_show (Denemo.input_label);
          write_input_status ();
          free (name);
          return SCM_BOOL_T;
        }
    }
  else
    {
      gtk_widget_hide (Denemo.input_label);
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
    movementnum = g_list_index (Denemo.project->movements, Denemo.project->movement) + 1;
  if (scm_is_integer (staff))
    staffnum = scm_to_int (staff);
  else
    staffnum = Denemo.project->movement->currentstaffnum;

  if (scm_is_integer (measure))
    measurenum = scm_to_int (measure);
  else
    measurenum = Denemo.project->movement->currentmeasurenum;

  if (scm_is_integer (object))
    objectnum = scm_to_int (object);
  else
    objectnum = 1 + Denemo.project->movement->cursor_x;

  gint origmvt = g_list_index (Denemo.project->movements, Denemo.project->movement) + 1, origstaff = Denemo.project->movement->currentstaffnum, origmeas = Denemo.project->movement->currentmeasurenum, origpos = 1 + Denemo.project->movement->cursor_x;
  goto_movement_staff_obj (NULL, movementnum, staffnum, measurenum, objectnum, 0);
  if ((movementnum == g_list_index (Denemo.project->movements, Denemo.project->movement) + 1) && (staffnum == Denemo.project->movement->currentstaffnum) && (measurenum == Denemo.project->movement->currentmeasurenum) && (objectnum == 1 + Denemo.project->movement->cursor_x))
    return SCM_BOOL_T;
  else
    goto_movement_staff_obj (NULL, origmvt, origstaff, origmeas, origpos, 0);

  return SCM_BOOL_F;
}

SCM
scheme_shift_cursor (SCM value)
{
  if (!scm_is_integer (value))
    return SCM_BOOL_F;
  gint shift = scm_to_int (value);
  Denemo.project->movement->cursor_y += shift;
  Denemo.project->movement->staffletter_y = offsettonumber (Denemo.project->movement->staffletter_y + shift);
  return SCM_BOOL_T;



}

SCM
scheme_mid_c_offsettoname (gint offset)
{
  gchar *notename = g_strdup_printf ("%c", mid_c_offsettoname (offset));
  SCM scm = scm_from_locale_string (notename);
  g_free (notename);
  return scm;
}

SCM
scheme_get_horizontal_position (void)
{
  return scm_from_int (1 + Denemo.project->movement->cursor_x);
}

SCM
scheme_set_object_display_width (SCM value)
{
  if (!scm_is_integer (value))
    return SCM_BOOL_F;
  if (Denemo.project->movement->currentobject)
    {
      DenemoObject *obj = Denemo.project->movement->currentobject->data;
      gint minpixels = scm_to_int (value);
      obj->minpixelsalloted = minpixels;
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}




SCM
scheme_get_movement (void)
{
  gint num = g_list_index (Denemo.project->movements, Denemo.project->movement) + 1;
  return scm_from_int (num);
}

SCM
scheme_get_staff (void)
{
  gint num = Denemo.project->movement->currentstaffnum;
  return scm_from_int (num);
}

SCM
scheme_staff_hidden (SCM set)
{
  DenemoProject *gui = Denemo.project;
  DenemoStaff *staff = (DenemoStaff *) gui->movement->currentstaff->data;
  if (scm_is_bool (set))
    staff->hidden = scm_is_true (set);
  return SCM_BOOL (staff->hidden);
}

SCM
scheme_get_voice_identifier (void)
{
  gint snum = Denemo.project->movement->currentstaffnum;
  gint mnum = g_list_index (Denemo.project->movements, Denemo.project->movement) + 1;
  GString *voice_ident = g_string_new ("");
  GString *name = g_string_new ("");
  g_string_printf (name, "Mvmnt%dVoice%d", mnum, snum);
  set_lily_name (name, voice_ident);
  SCM ret = scm_from_locale_string (voice_ident->str);
  g_string_free (name, TRUE);
  g_string_free (voice_ident, TRUE);
  return ret;
}
SCM
scheme_roman_numeral (SCM number)
{
    SCM ret = SCM_BOOL_F;
    if (scm_is_string (number))
        {
            GString *to, *from;
            gchar *input = scm_to_locale_string (number);
            
            from = g_string_new (input);
            to = g_string_new("");
            set_lily_name (from, to);
            g_string_free (from, TRUE);
            ret = scm_from_locale_string (to->str);
            g_string_free (to, TRUE);
            if (input)
                free (input);
        }
    return ret;
}

SCM
scheme_get_measure (void)
{
  gint num = Denemo.project->movement->currentmeasurenum;
  return scm_from_int (num);
}

SCM
scheme_get_cursor_note (SCM optional)
{
  DenemoProject *gui = Denemo.project;
  return scheme_mid_c_offsettoname (gui->movement->cursor_y);
}

SCM
scheme_get_cursor_note_with_octave (SCM optional)
{
  DenemoProject *gui = Denemo.project;
  return scm_from_locale_string (mid_c_offsettolily (gui->movement->cursor_y, 0));
}


SCM
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


SCM
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

SCM
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

SCM
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
  DenemoProject *gui = Denemo.project;
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
  DenemoProject *gui = Denemo.project;
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
  return scm_from_int (Denemo.project->input_source);
}


SCM
scheme_chordize (SCM setting)
{
  DenemoProject *gui = Denemo.project;
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
    return SCM_BOOL (FALSE);
  gboolean old = thechord->chordize;
  thechord->chordize = scm_is_true (setting);
  if (old != thechord->chordize)
    score_status (Denemo.project, TRUE);
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
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
    return SCM_BOOL (FALSE);
  else
    {
      return scheme_mid_c_offsettoname (thenote->mid_c_offset);
    }

}

SCM
scheme_get_note_names_from_user (SCM num, SCM initial, SCM message)
{
    SCM ret = SCM_BOOL_F;
    gint number = 1;
    gchar *text = NULL;
    gchar *init = NULL;
    if (scm_is_integer(num))
        number = scm_to_int (num);
    if (number != 1)
    number = 2;
    if (scm_is_string (initial))
        init = scm_to_locale_string (initial);
    if (scm_is_string (message))
        text = scm_to_locale_string (message);
    gchar *note = notes_choice_dialog (number, init, text);
    if (note)
        ret = scm_from_locale_string (note);
    g_free (text);
    g_free (init);
    g_free (note);
    return ret;
}
SCM
scheme_get_multiline_user_input (SCM title, SCM instruction, SCM initial)
{
    SCM ret = SCM_BOOL_F;
    gchar *tl = _("Text Input");
    gchar *ins = _("Give Text");
    gchar *init = "";
    
    if (scm_is_string (title))
        tl = scm_to_locale_string (title); 
    if (scm_is_string (instruction))
        ins = scm_to_locale_string (instruction); 
    if (scm_is_string (initial))
        init = scm_to_locale_string (initial);
    gchar *val = get_multiline_input (tl, ins, init);
    if (val)
        ret = scm_from_locale_string (val);
    g_free (val);
    return ret;
}
SCM
scheme_get_lilypond_syntax_from_user (SCM tit, SCM ins, SCM prior, SCM post, SCM initial) 
{

    SCM ret = SCM_BOOL_F;
#ifndef USE_EVINCE
   g_debug ("This feature requires denemo to be built with evince");
#else
    gchar *title = _("LilyPond Syntax");
    gchar *prior_context = "\\score {<<";
    gchar *post_context = ">>}";
    gchar *instruction = _("Give LilyPond Syntax");
    gchar *initial_markup = "";
    if (scm_is_string (tit))
        title = scm_to_locale_string (tit);
    if (scm_is_string (ins))
        instruction = scm_to_locale_string (ins); 
    if (scm_is_string (prior))
        prior_context = scm_to_locale_string (prior); 
    if (scm_is_string (post))
        post_context = scm_to_locale_string (post); 
    if (scm_is_string (initial))
        initial_markup = scm_to_locale_string (initial);
    gchar *val = get_lilypond_syntax_from_user (title, instruction, prior_context, post_context, initial_markup);
    if (val)
        ret = scm_from_locale_string (val);
    g_free (val);
#endif
    return ret;
}

//Insert rests to the value of the timesig and return the number of rests inserted.
SCM
scheme_put_whole_measure_rests (void)
{
  DenemoProject *gui = Denemo.project;
  SCM scm;
  if (!Denemo.project || !(Denemo.project->movement))
    return scm_from_int (0);
  else
    {
      gint numerator = ((DenemoMeasure *) gui->movement->currentmeasure->data)->timesig->time1; // staff->timesig.time1;
      gint denominator = ((DenemoMeasure *) gui->movement->currentmeasure->data)->timesig->time2;       //staff->timesig.time2;
      gboolean dot = TRUE;
      if (numerator % 3)
        dot = FALSE;
      else
        numerator = 2 * numerator / 3;
      gint length = (numerator * 4) / denominator;
      gchar *str = NULL;
      scm = scm_from_int (1);
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
          scm = scm_from_int (2);
          break;
        case 4:
          str = g_strdup_printf ("(d-InsertRest0)(d-MoveCursorLeft)%s", dot ? "(d-AddDot)" : "");
          break;
        case 8:
          str = g_strdup_printf ("(d-InsertRest0)(d-InsertRest0)(d-MoveCursorLeft)%s", dot ? "(d-AddDot)" : "");
          scm = scm_from_int (2);
          break;
        default:
          g_warning ("Not implemented %d %s", length, dot ? "dotted" : "");
          scm = scm_from_int (0);
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
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object))
    return SCM_BOOL_F;
  return scm_from_int (thechord->numdots);
}

SCM
scheme_get_note_base_duration (void)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object))
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

  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object))
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

SCM
scheme_set_duration_in_ticks (SCM duration)
{
  DenemoObject *curObj;
  gint thedur = 0;
  if (scm_is_integer (duration))
    {
      thedur = scm_to_int (duration);
    }
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data))
    return SCM_BOOL_F;
  if (thedur >= 0)
    {
      curObj->basic_durinticks = curObj->durinticks = thedur;
      if (curObj->type == CHORD)
        {
          ((chord *) curObj->object)->baseduration = -thedur;
          ((chord *) curObj->object)->numdots = 0;
        }
      objnode *prev = Denemo.project->movement->currentobject->prev;
      DenemoObject *prevObj = prev ? (DenemoObject *) prev->data : NULL;
      gint starttick = (prevObj ? prevObj->starttickofnextnote : 0);
      curObj->starttickofnextnote = starttick + thedur;
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}


SCM
scheme_get_recorded_midi_tempo (SCM index)
{
  SCM scm = scm_list_n (SCM_UNDEFINED);
  if (scm_is_integer (index))
    {
      gint idx = scm_to_int (index);
      smf_tempo_t *tempo = get_recorded_midi_tempo (idx);
      if (tempo)
        {
          scm = scm_cons (scm_from_double (tempo->microseconds_per_quarter_note / 1000000.0), scm);
          scm = scm_cons (scm_from_int (tempo->denominator), scm);
          scm = scm_cons (scm_from_int (tempo->numerator), scm);
          scm = scm_cons (scm_from_int (tempo->time_seconds), scm);
          return scm;
        }
    }
  return SCM_BOOL_F;
}

SCM
scheme_get_imported_midi_track (SCM index)
{
  if (scm_is_integer (index))
    {
      gint idx = scm_to_int (index);
      if (get_imported_midi_track (idx))
        return SCM_BOOL_F;
    }
  return SCM_BOOL_F;
}

SCM
scheme_delete_imported_midi (void)
{
  return SCM_BOOL (delete_imported_midi ());
}


SCM
scheme_get_current_midi_track (void)
{
  gint track = get_current_midi_track ();
  if (track)
    return scm_from_int (track);
  return SCM_BOOL_F;
}

SCM
scheme_get_imported_midi_tracks (void)
{
  gint num = get_imported_midi_tracks ();
  if (num < 1)
    return SCM_BOOL_F;
  else
    return scm_from_int (num);
}

SCM
scheme_get_recorded_midi_duration (void)
{
  gdouble duration = get_recorded_midi_duration ();
  g_debug ("Duration returned %f so %d\n", duration, duration > 0.0);
  if (duration > 0.0)
    return scm_from_double (duration);
  return SCM_BOOL_F;
}

SCM
scheme_get_duration_in_ticks (void)
{
  DenemoObject *curObj;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data))
    return SCM_BOOL (FALSE);
  return scm_from_int (curObj->durinticks);
}

SCM
scheme_get_base_duration_in_ticks (void)
{
  DenemoObject *curObj;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data))
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
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data))
    return SCM_BOOL (FALSE);
  return scm_from_int (curObj->starttickofnextnote);
}


SCM
scheme_get_start_tick (void)
{
  DenemoObject *curObj;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data))
    return SCM_BOOL (FALSE);
  return scm_from_int (curObj->starttick);
}


SCM
scheme_get_measure_number (void)
{
  return scm_from_int (Denemo.project->movement->currentmeasurenum);
}

SCM
scheme_set_measure_number_offset (SCM val)
{
  DenemoMeasure *themeasure;
  if (scm_is_integer (val))
    {
      gint offset = scm_to_int (val);
      DenemoPosition pos;
      get_position (Denemo.project->movement, &pos);

      gint i = 1;

      while (goto_movement_staff_obj (NULL, -1, i++, pos.measure, 0, 0))
        {
          themeasure = (DenemoMeasure *) Denemo.project->movement->currentmeasure->data;
          themeasure->measure_numbering_offset = offset;
        }
      goto_movement_staff_obj (NULL, -1, pos.staff, pos.measure, pos.object, pos.leftmeasurenum);
      cache_all ();
    }
  else
    return SCM_BOOL_F;
  themeasure = Denemo.project->movement->currentmeasure->data;
  return scm_from_int (themeasure->measure_numbering_offset);
}

SCM
scheme_get_measure_number_offset (void)
{
  DenemoMeasure *themeasure = Denemo.project->movement->currentmeasure->data;
  return scm_from_int (themeasure->measure_numbering_offset);
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
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) g_list_nth_data (thechord->notes, index)))
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
scheme_get_note_staff_position (SCM count)
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
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) g_list_nth_data (thechord->notes, index)))
    return SCM_BOOL_F;
  else
    {
      return scm_from_int(4 - thenote->y/HALF_LINE_SPACE);
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
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes))
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
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes))
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
      g_warning ("Bad pitch spell list");
      return SCM_BOOL_F;
    }
}


SCM
scheme_get_cursor_note_as_midi (SCM optional)
{

  DenemoProject *gui = Denemo.project;
  gint midi = dia_to_midinote (gui->movement->cursor_y);
  SCM scm = scm_from_int (midi);
  return scm;
}


SCM
scheme_get_note_as_midi (void)
{
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
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

  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
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
scheme_get_note_at_cursor (void)
{
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  GString *str = g_string_new ("");
  SCM scm;

  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
    return SCM_BOOL_F;
  GList *g;
  for (g = thechord->notes; g; g = g->next)
    {
      thenote = (note *) g->data;
      if (thenote->mid_c_offset == Denemo.project->movement->cursor_y)
        {
          gchar *name = mid_c_offsettolily (thenote->mid_c_offset, thenote->enshift);
          return scm_from_locale_string (name);
        }
    }
  return SCM_BOOL_F;
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
  gint theclef = find_prevailing_clef (Denemo.project->movement);
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
  if (scm_is_integer (optional))
    {
      gint duration = scm_to_int (optional);
      if (duration >= 0 && (duration < 8))
        {
          SetDur (duration);
          return SCM_BOOL_T;
        }
      return SCM_BOOL_F;
    }
  else
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
scheme_get_prevailing_keysig_name (void)
{
  SCM ret;
  gchar *key = get_prevailing_keysig_name();
  ret = scm_from_locale_string (key);
  g_free (key);
  return ret;
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
  staff_show_which_accidentals ((DenemoStaff *) Denemo.project->movement->currentstaff->data);
  free (accs);
  displayhelper (Denemo.project);       //score_status(Denemo.project, TRUE);
  return SCM_BOOL_T;
}

SCM
scheme_increment_initial_keysig (SCM amount)
{
  DenemoStaff *curstaff = Denemo.project->movement->currentstaff->data;
  SCM ret = SCM_BOOL_F;
  gint inc = 1;
  if (scm_is_integer (amount))
    inc = scm_to_int (amount);
  keysig *sig = &curstaff->keysig;
  inc += sig->number;
  if (inc < 8 && inc > -8)
    {
      dnm_setinitialkeysig (curstaff, inc, curstaff->keysig.isminor);
      score_status (Denemo.project, TRUE);
      ret = SCM_BOOL_T;
    }
  return ret;
}

SCM
scheme_increment_keysig (SCM amount)
{
  DenemoStaff *curstaff = Denemo.project->movement->currentstaff->data;
  DenemoObject *curObj = NULL;
  SCM ret = SCM_BOOL_F;
  gint inc = 1;
  if (scm_is_integer (amount))
    inc = scm_to_int (amount);
  keysig *sig = &curstaff->keysig;
  if ((Denemo.project->movement->currentobject) && (curObj = Denemo.project->movement->currentobject->data) && (curObj->type == KEYSIG))
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
      score_status (Denemo.project, TRUE);
      displayhelper (Denemo.project);
      ret = SCM_BOOL_T;
    }
  return ret;
}

SCM
scheme_swap_notes_at_cursor_height (SCM nochange)
{
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
     return SCM_BOOL_F;
  GList *g;
  for (g=thechord->notes;g;g=g->next)
    {
        thenote = (note *) g->data;
        if (thenote->mid_c_offset == Denemo.project->movement->cursor_y)
            {
                if (g->next)
                    {  gpointer temp;
                       note *next = (note*)g->next->data;
                       if (next->mid_c_offset == Denemo.project->movement->cursor_y)
                        {
                                if(scm_is_true (nochange)) //not just a query, actually swap notes
                                    {
                                        temp = g->next->data;
                                        g->next->data = g->data;
                                        g->data = temp;
                                    }
                                return SCM_BOOL_T;
                        }
                    }
                else
                    return SCM_BOOL_F;
                
            }
        }
  return SCM_BOOL_F;
}

SCM
scheme_cursor_to_nth_note_height (SCM number)
{
  return SCM_BOOL (cursor_to_nth_note_height (scm_to_int (number) - 1));
}

SCM
scheme_cursor_to_next_note_height (void)
{
  return SCM_BOOL (cursor_to_next_note_height ());
}

SCM
scheme_cursor_to_note (SCM lilyname)
{
  DenemoProject *gui = Denemo.project;
  gint mid_c_offset;
  gint enshift;
  char *notename;

  if (scm_is_string (lilyname))
    {
      notename = scm_to_locale_string (lilyname);
      interpret_lilypond_notename (notename, &mid_c_offset, &enshift);
      gui->movement->cursor_y = mid_c_offset;
      gui->movement->staffletter_y = offsettonumber (gui->movement->cursor_y);
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
  DenemoProject *gui = Denemo.project;
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

      if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
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
          dclef = find_prevailing_clef (Denemo.project->movement);
          notename = scm_to_locale_string (lilynotes);
          chordnote = strtok (notename, " ");
          while (chordnote)
            {
              interpret_lilypond_notename (chordnote, &mid_c_offset, &enshift);
              dnm_addtone (curObj, mid_c_offset, enshift);
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

  gchar *ret = string_dialog_entry_with_widget_opt (Denemo.project, title, instruction, initial_value, NULL, scm_is_eq (modal, SCM_UNDEFINED) || scm_is_true (modal));
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


SCM
scheme_get_user_input_with_snippets (SCM label, SCM prompt, SCM init, SCM modal)
{
  char *title, *instruction, *initial_value;
  SCM scm;
  gboolean ismodal = FALSE, format = FALSE;
  if (scm_is_string (modal))
    {
      char *arg = scm_to_locale_string (modal);
      if (!strcmp (arg, "format"))
        ismodal = format = TRUE;
      else if (!strcmp (arg, "modal"))
        ismodal = TRUE, format = FALSE;
      //g_print ("setting from  %s %d %d\n", arg, ismodal, format);
      free (arg);
    }
  else
    {
      ismodal = !scm_is_false (modal), format = (!scm_is_false (modal)) && (!scm_is_eq (modal, SCM_UNDEFINED));
    }
  if (scm_is_string (label))
    {
      title = scm_to_locale_string (label);
    }
  else
    title = strdup (_("Input Required"));
  if (scm_is_string (prompt))
    {
      instruction = scm_to_locale_string (prompt);
    }
  else
    instruction = strdup (_("Give input: "));

  if (scm_is_string (init))
    {
      initial_value = scm_to_locale_string (init);
    }
  else
    initial_value = strdup (" ");
  GString *text = g_string_new (""), *lilypond = g_string_new ("");     // g_print ("Called with %d %d\n", ismodal, format);
  gboolean ok = get_user_markup (text, lilypond, title, instruction, initial_value, ismodal, format);

  if (ok)
    {
      scm = scm_cons (scm_from_locale_string (text->str), scm_from_locale_string (lilypond->str));
    }
  else
    scm = SCM_BOOL_F;

  if (title)
    free (title);
  if (instruction)
    free (instruction);
  if (initial_value)
    free (initial_value);
  g_string_free (text, TRUE);
  g_string_free (lilypond, TRUE);

  return scm;
}

static gchar *
select_font (gchar * title)
{
  gchar *fontname = NULL;
  GtkResponseType result;

  GtkWidget *dialog =
#if GTK_MAJOR_VERSION == 2
    gtk_font_selection_dialog_new (title);
#else
    gtk_font_chooser_dialog_new (title, NULL);
#endif
  result = gtk_dialog_run (GTK_DIALOG (dialog));

  if (result == GTK_RESPONSE_OK || result == GTK_RESPONSE_APPLY)
#if GTK_MAJOR_VERSION == 2
    fontname = gtk_font_selection_dialog_get_font_name (GTK_FONT_SELECTION_DIALOG (dialog));
  fontname = string_dialog_entry (Denemo.project, title, _("Please delete the font size and bold/italic indications,\nleaving just the font family name."), fontname);
#else
    fontname = g_strdup (pango_font_family_get_name (gtk_font_chooser_get_font_family (GTK_FONT_CHOOSER (dialog))));


#endif
  gtk_widget_destroy (dialog);
  return fontname;
}

SCM
scheme_select_font (SCM text)
{
  SCM ret = SCM_BOOL_F;
  gchar *title, *choice;
  if (scm_is_string (text))
    {
      title = scm_to_locale_string (text);
    }
  else
    title = strdup (_("Choose Font"));
  choice = select_font (title);
  if (choice)
    ret = scm_from_locale_string (choice);
  g_free (choice);
  return ret;
}

#define GDOUBLE_TO_POINTER(x) (GINT_TO_POINTER((gint)(10000*x)))
#define GPOINTER_TO_DOUBLE(x) (GPOINTER_TO_INT(x)/10000.0)

static GList *
select_color (gchar * title)
{
  GtkResponseType result;
  GList *ret = NULL;
  GtkWidget *dialog =
#if GTK_MAJOR_VERSION == 2
    gtk_color_selection_dialog_new (title);
#else
    gtk_color_chooser_dialog_new (title, NULL);
#endif
  result = gtk_dialog_run (GTK_DIALOG (dialog));

  if (result == GTK_RESPONSE_OK || result == GTK_RESPONSE_APPLY)
    {
#if GTK_MAJOR_VERSION == 2
      GdkColor color;
      GtkColorSelection *colorsel = gtk_color_selection_dialog_get_color_selection (dialog);
      gtk_color_selection_get_current_color (colorsel, &color);
      ret = g_list_append (ret, GDOUBLE_TO_POINTER (color.red / 65535.0));
      ret = g_list_append (ret, GDOUBLE_TO_POINTER (color.green / 65535.0));
      ret = g_list_append (ret, GDOUBLE_TO_POINTER (color.blue / 65535.0));

#else
      GdkRGBA color;
      gtk_color_chooser_get_rgba (GTK_COLOR_CHOOSER (dialog), &color);
      ret = g_list_append (ret, GDOUBLE_TO_POINTER (color.red));
      ret = g_list_append (ret, GDOUBLE_TO_POINTER (color.green));
      ret = g_list_append (ret, GDOUBLE_TO_POINTER (color.blue));
#endif
    }
  gtk_widget_destroy (dialog);
  return ret;
}

SCM
scheme_select_color (SCM text)
{
  SCM ret = SCM_BOOL_F;
  gchar *title;
  GList *list;
  if (scm_is_string (text))
    {
      title = scm_to_locale_string (text);
    }
  else
    title = strdup (_("Choose Font"));
  list = select_color (title);
  if (list)
    {
      ret = scm_list_n (scm_from_double (GPOINTER_TO_DOUBLE (list->data)), scm_from_double (GPOINTER_TO_DOUBLE (list->next->data)), scm_from_double (GPOINTER_TO_DOUBLE (list->next->next->data)), SCM_UNDEFINED);
      g_list_free (list);
    }
  return ret;
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


static void
info_response (GtkWidget * dialog, gint reponse_id, gchar * script)
{
  if (script)
    call_out_to_guile (script);
  gtk_widget_destroy (dialog);
}


SCM
scheme_info_with_hook (SCM title, SCM hook)
{
  GtkWidget *dialog;
  gchar *msg = NULL, *script = NULL;
  if (scm_is_string (hook))
    script = scm_to_locale_string (hook);
  if (scm_is_string (title))
    msg = scm_to_locale_string (title);
  if (msg)
    {
      dialog = gtk_message_dialog_new (GTK_WINDOW (Denemo.window), GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_INFO, GTK_BUTTONS_CLOSE, "%s", msg);
#ifdef G_OS_WIN32
      gtk_window_set_resizable (GTK_WINDOW (dialog), TRUE);     //needed on windows because of a bug, not all text can be seen.
#endif
      g_signal_connect (dialog, "response", G_CALLBACK (info_response), script);
      gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (Denemo.window));
      gtk_window_set_keep_above (GTK_WINDOW (dialog), TRUE);
      gtk_widget_show_all (dialog);
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}



SCM
scheme_infodialog (SCM msg, SCM noblock)
{
  char *title;
  gboolean modal = FALSE;
  if (scm_is_false (noblock))
    modal = TRUE;
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
  if (modal)
    {
      infowarningdialog (title, FALSE);
    }
  else
    {
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
      progressbar (title, NULL);
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

SCM
scheme_typeset_for_script (SCM thescript)
{
  SCM ret = SCM_BOOL_F;
#ifndef USE_EVINCE
  g_debug ("This feature requires denemo to be built with evince");
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

SCM
scheme_print_typeset_pdf (void)
{
#ifndef USE_EVINCE
  g_debug ("This feature requires denemo to be built with evince");
  return SCM_BOOL_F;
#else
  return print_typeset_pdf ()? SCM_BOOL_F : SCM_BOOL_T;
#endif
}

SCM
scheme_continous_typsetting (void)
{
  return SCM_BOOL (continuous_typesetting ());

}

SCM
scheme_display_typeset_svg (SCM scaling, SCM part)
{
  if (!continuous_typesetting ())
    {
      gdouble scale = 1.0;
      if (scm_is_real (scaling))
        scale = scm_to_double (scaling);
      display_svg (scale, scm_is_true (part));
      return SCM_BOOL_T;
    }
  else
    return SCM_BOOL_F;
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
scheme_get_keypress (SCM putback)
{
  static GdkEventKey event;
  if (scm_is_false (putback))
    {
    gint cmd = lookup_command_for_keyevent (&event);
      ;;if (cmd != -1) 
       return SCM_BOOL ( execute_callback_from_idx (Denemo.map, cmd));   
    }
  else {
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
      //g_debug("command %d for %x %x\n", cmd, event.keyval, event.state);
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
      write_status (Denemo.project);
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
  value = string_dialog_entry (Denemo.project, "Font Size", "Give a value (+/-) to adjust font size by", "0");
  if (!value)
    value = g_strdup ("0");
  gchar *clean = g_strdup_printf ("%d", atoi (value));
  g_free (value);
  g_object_set_data (G_OBJECT (Denemo.printarea), "font-size", (gpointer) clean);
  return scm_from_locale_stringn (clean, strlen (clean));
}

void get_clipboard (DenemoAction * action, DenemoScriptParam * param);
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
                                                   _("_OK"), GTK_RESPONSE_ACCEPT,
                                                   _("_Cancel"), GTK_RESPONSE_REJECT,
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



/* create a dialog with two strings & title
 * return #t if the first is chosen else #f 
*/
SCM
scheme_make_choice (SCM default_option, SCM alternative, SCM title)
{
  gchar *first, *second, *thetitle;
  if (scm_is_string (title))
    {
      thetitle = scm_to_locale_string (title);
      if (scm_is_string (default_option))
        {
          first = scm_to_locale_string (default_option);  
          if (scm_is_string (alternative))
            {
              second = scm_to_locale_string (alternative);
              return SCM_BOOL (confirm_first_choice (thetitle, first, second));
            }
        }
    }
  return SCM_BOOL_F;
}

/* create a dialog with the options & return the one chosen, of #f if
   the user cancels
*/
SCM
scheme_get_option (SCM options, SCM title)
{
  gchar *response = NULL;
  size_t length;
  gchar *thetitle = NULL;
  //gchar *str=NULL;
  if (scm_is_string (title))
    thetitle = scm_to_locale_string (title);

  if (scm_is_string (options))
    {
      char *str_unterm;
      str_unterm = scm_to_locale_stringn (options, &length);
      response = get_option (thetitle, str_unterm, length);     //returns NULL or a pointer to a location in str_unterm
      //g_debug("Got %p holding %s\n", response, response);
      if (response)
        response = g_strdup (response);
      if (str_unterm)
        free (str_unterm);
    }
  if (response)
    {
      SCM ret = scm_from_locale_stringn (response, strlen (response));
      //g_debug("Freeing %p holding %s\n", response, response);
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


SCM
scheme_lock_directive (SCM lock)
{
  DenemoObject *curObj;
  DenemoDirective *directive;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != LILYDIRECTIVE) || !(directive = (DenemoDirective *) curObj->object))
    return SCM_BOOL (FALSE);
  directive->locked = scm_is_true (lock);
  return SCM_BOOL_T;
}


/* store the script to be invoked as an action for a directive tagged with tag */
SCM
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

SCM
scheme_put_standalone_directive (SCM tag, SCM width)
{
  if (scm_is_string (tag))
    {
      char *the_tag;
      gint pixelwidth = 40;
      if (scm_is_integer (width))
        pixelwidth = scm_to_int (width);
      the_tag = scm_to_locale_string (tag);
      put_standalone_directive (the_tag, pixelwidth);
      if (the_tag)
        free (the_tag);
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

SCM
scheme_choose_tag_at_cursor (void)
{
  DenemoObject *curObj;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data))
    return SCM_BOOL_F;
  gchar *tag;
  gboolean is_note = choose_tag_at_cursor (&tag);
  if (tag)
    {
      if (curObj->type == CHORD)
        return scm_cons (scm_from_locale_string (tag), scm_from_bool (is_note));
      else
        return scm_from_locale_string (tag);
    }
  return SCM_BOOL_F;
}

SCM
scheme_get_script_for_directive (SCM tagname, SCM isnote)
{
  DenemoObject *curObj;
  SCM ret = SCM_BOOL_F;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD))
    return SCM_BOOL_F;
  gchar *tag, *script;
  gboolean note = scm_is_true (isnote);
  if (scm_is_string (tagname))
    {
      tag = scm_to_locale_string (tagname);

      DenemoDirective *directive = note ? get_note_directive (tag) : find_directive (((chord *) curObj->object)->directives, tag);
      if (directive)
        {
          Denemo.project->movement->directive_on_clipboard = (gpointer) directive;
          script = get_script_for_directive (directive, note ? "note" : "chord");
          ret = scm_from_locale_string (script);
          g_free (script);
        }
    }
  return ret;
}



SCM
scheme_directive_change_tag (SCM tag)
{
  DenemoObject *curObj;
  if (scm_is_string (tag))
    {
      gchar *thetag = scm_to_locale_string (tag);
      DenemoDirective *directive;
      if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != LILYDIRECTIVE) || !(directive = (DenemoDirective *) curObj->object))
        return SCM_BOOL (FALSE);
      if (directive->tag == NULL)
        directive->tag = g_string_new ("");
      g_string_assign (directive->tag, thetag);
      g_free (thetag);
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

#define GET_NTH_TAG(what)\
 SCM scheme_##what##_directive_get_nth_tag(SCM index) {\
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

#define PRIORITIZE_TAG(what)\
 SCM scheme_##what##_directive_prioritize_tag(SCM name) {\
  gchar *tag;\
  if(!scm_is_string(name))\
     return SCM_BOOL_F;\
    tag = scm_to_locale_string (name);\
  extern gboolean prioritize_##what##_tag (gchar *name);\
  gboolean val = prioritize_##what##_tag (tag);\
  if(val) return name;\
  return SCM_BOOL_F;\
}
PRIORITIZE_TAG (chord);
PRIORITIZE_TAG (note);
PRIORITIZE_TAG (staff);
PRIORITIZE_TAG (voice);
PRIORITIZE_TAG (score);
PRIORITIZE_TAG (clef);
PRIORITIZE_TAG (timesig);
PRIORITIZE_TAG (tuplet);
PRIORITIZE_TAG (stemdirective);
PRIORITIZE_TAG (keysig);
PRIORITIZE_TAG (scoreheader);
PRIORITIZE_TAG (header);
PRIORITIZE_TAG (paper);
PRIORITIZE_TAG (layout);
PRIORITIZE_TAG (movementcontrol);
#undef PRIORITIZE_TAG


SCM
scheme_edit_system_directive (void)
{
  edit_system_directive ();
  return SCM_BOOL_T;
}

SCM
scheme_display_directive_text_editor (SCM type, SCM tagname)
{
  if (scm_is_string (type) && scm_is_string (tagname))
    {
      gchar *what = scm_to_locale_string (type);
      gchar *tag = scm_to_locale_string (tagname);
      DenemoDirective *directive;
      if (!strcmp (what, "score"))
        directive = get_score_directive (tag);
      else
        directive = get_movementcontrol_directive (tag);        //others - note, chord ...

      if (directive && directive->override & DENEMO_OVERRIDE_EDITOR)
        {
          GtkWidget *texteditor = (GtkWidget *) g_object_get_data (G_OBJECT (directive->widget), DENEMO_TEXTEDITOR_TAG);
          if (texteditor)
            {
              gtk_widget_show_all (gtk_widget_get_toplevel (texteditor));
              gtk_window_present (GTK_WINDOW (gtk_widget_get_toplevel (texteditor)));
            }
          return SCM_BOOL_T;
        }
    }
  return SCM_BOOL_F;
}

//only retrieve directives when cursor is actually on the note
SCM
scheme_directive_get_nth_tag_strict_note (SCM index)
{
  gint n;
  if (!scm_is_integer (index))
    return SCM_BOOL_F;
  n = scm_to_int (index);
  gchar *val = get_nth_strict_note_tag (n);
  if (val)
    return scm_from_locale_stringn (val, strlen (val));
  return SCM_BOOL_F;
}

SCM
scheme_directive_get_for_tag_strict_note (SCM tagname)
{
  SCM ret = SCM_BOOL_F;
  const gchar *tag = NULL;
  if (scm_is_string (tagname))
    tag = scm_to_locale_string (tagname);
  tag = strict_note_directive_get_tag ((gchar *) tag);
  if (tag)
    ret = scm_from_locale_string (tag);
  return ret;
}

#define GET_TAG_FN_DEF(what)\
 SCM scheme_##what##_directive_get_tag(SCM tag) {\
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
 SCM scheme_activate_##what##_directive(SCM tag) {\
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
 SCM scheme_text_edit_##what##_directive(SCM tag) {\
  if(!scm_is_string(tag)){\
    return SCM_BOOL(text_edit_##what##_directive (NULL));\
  }\
  char *tagname;\
  tagname = scm_to_locale_string(tag);\
  extern gboolean text_edit_##what##_directive (gchar *tagname);\
  gboolean ret = text_edit_##what##_directive (tagname);\
  if(tagname) g_free(tagname);\
  return SCM_BOOL(ret);\
}

#define DELETE_FN_DEF(what)\
 SCM scheme_delete_##what##_directive(SCM tag) {\
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

#define EDIT_DELETE_FN_DEF(what) \
EDIT_FN_DEF(what) \
DELETE_FN_DEF(what) \
ACTIVATE_FN_DEF(what)

EDIT_FN_DEF (standalone);
EDIT_DELETE_FN_DEF (note);
EDIT_DELETE_FN_DEF (chord);
EDIT_DELETE_FN_DEF (staff);
EDIT_DELETE_FN_DEF (voice);
EDIT_DELETE_FN_DEF (score);

#define GETFUNC_DEF(what, field)\
SCM scheme_##what##_directive_get_##field(SCM tag) {\
  if(!scm_is_string(tag)){\
    return SCM_BOOL(FALSE);\
  }\
  char *tagname;\
  tagname = scm_to_locale_string(tag);\
  extern gchar* what##_directive_get_##field(gchar *tagname);\
  gchar *value = (gchar*)what##_directive_get_##field((gchar*)tagname);\
  if(tagname) free(tagname);\
  if(value && *value){\
    return scm_from_locale_string(value);\
  }\
  return SCM_BOOL(FALSE);\
}

static void
structural_change_note (void)
{
}

static void
structural_change_chord (void)
{
}

static void
structural_change_clef (void)
{
    if (!(Denemo.project->movement->currentobject && (((DenemoObject*)(Denemo.project->movement->currentobject->data))->type == CLEF)))
        signal_structural_change (Denemo.project);
}

static void
structural_change_timesig (void)
{
    if (!(Denemo.project->movement->currentobject && (((DenemoObject*)(Denemo.project->movement->currentobject->data))->type == TIMESIG)))
        signal_structural_change (Denemo.project);}

static void
structural_change_keysig (void)
{
    if (!(Denemo.project->movement->currentobject && (((DenemoObject*)(Denemo.project->movement->currentobject->data))->type == KEYSIG)))
        signal_structural_change (Denemo.project);}

static void
structural_change_stemdirective (void)
{
}

static void
structural_change_tuplet (void)
{
}

static void
structural_change_standalone (void)
{
}

static void
structural_change_score (void)
{
  signal_structural_change (Denemo.project);
}

static void
structural_change_scoreheader (void)
{
  signal_structural_change (Denemo.project);
}

static void
structural_change_movementcontrol (void)
{
  signal_structural_change (Denemo.project);
}

static void
structural_change_paper (void)
{
  signal_structural_change (Denemo.project);
}

static void
structural_change_header (void)
{
  signal_structural_change (Denemo.project);
}

static void
structural_change_layout (void)
{
  signal_structural_change (Denemo.project);
}

static void
structural_change_staff (void)
{
  signal_structural_change (Denemo.project);
}

static void
structural_change_voice (void)
{
  signal_structural_change (Denemo.project);
}


static void
structural_change_object (void)
{
}                               //not sure what this is - some sort of temporary marker thing Nils devised.



#define PUTFUNC_DEF(what, field)\
SCM scheme_##what##_directive_put_##field(SCM tag, SCM value) {\
  if((!scm_is_string(tag))||(!scm_is_string(value)))\
     return SCM_BOOL(FALSE);\
  char *tagname;\
  tagname = scm_to_locale_string(tag);\
  char *valuename;\
  valuename = scm_to_locale_string(value);\
  extern gboolean what##_directive_put_##field (gchar *tagname, gchar *valuename);\
  gboolean ret = what##_directive_put_##field ((gchar*)tagname, (gchar*)valuename);\
  structural_change_##what ();\
  if(tagname) free(tagname);\
  if(valuename) free(valuename);\
  return SCM_BOOL(ret);\
}

// block to clone for new GString entries in DenemoDirective

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
GETFUNC_DEF (standalone, graphic_name);
GETFUNC_DEF (chord, graphic_name);
GETFUNC_DEF (note, graphic_name);
GETFUNC_DEF (clef, graphic_name);
GETFUNC_DEF (keysig, graphic_name);
GETFUNC_DEF (timesig, graphic_name);
GETFUNC_DEF (tuplet, graphic_name);
GETFUNC_DEF (staff, grob);
GETFUNC_DEF (voice, grob);
GETFUNC_DEF (score, grob);
// UNUSED: GETFUNC_DEF (movementcontrol, grob);
GETFUNC_DEF (clef, grob);
GETFUNC_DEF (timesig, grob);
GETFUNC_DEF (tuplet, grob);
GETFUNC_DEF (stemdirective, grob);
GETFUNC_DEF (keysig, grob);
PUTFUNC_DEF (note, grob);
PUTFUNC_DEF (chord, grob);
PUTFUNC_DEF (standalone, grob);
// UNUSED: PUTFUNC_DEF(staff, grob)
// UNUSED: PUTFUNC_DEF(voice, grob)
PUTFUNC_DEF (score, grob);
// UNUSED: PUTFUNC_DEF(movementcontrol, grob)
PUTFUNC_DEF (clef, grob);
PUTFUNC_DEF (timesig, grob);
PUTFUNC_DEF (tuplet, grob);
PUTFUNC_DEF (stemdirective, grob);
PUTFUNC_DEF (keysig, grob);

GETFUNC_DEF (note, data);
GETFUNC_DEF (chord, data);
GETFUNC_DEF (standalone, data);
GETFUNC_DEF (staff, data);
GETFUNC_DEF (voice, data);
GETFUNC_DEF (score, data);
GETFUNC_DEF (scoreheader, data);
GETFUNC_DEF (header, data);
GETFUNC_DEF (paper, data);
GETFUNC_DEF (layout, data);
GETFUNC_DEF (movementcontrol, data);
GETFUNC_DEF (clef, data);
GETFUNC_DEF (timesig, data);
GETFUNC_DEF (tuplet, data);
GETFUNC_DEF (stemdirective, data);
GETFUNC_DEF (keysig, data);
PUTFUNC_DEF (note, data);
PUTFUNC_DEF (chord, data);
PUTFUNC_DEF (standalone, data);
PUTFUNC_DEF (staff, data);
PUTFUNC_DEF (voice, data);
PUTFUNC_DEF (score, data);
PUTFUNC_DEF (scoreheader, data);
PUTFUNC_DEF (header, data);
PUTFUNC_DEF (paper, data);
PUTFUNC_DEF (layout, data);
PUTFUNC_DEF (movementcontrol, data);
PUTFUNC_DEF (clef, data);
PUTFUNC_DEF (timesig, data);
PUTFUNC_DEF (tuplet, data);
PUTFUNC_DEF (stemdirective, data);
PUTFUNC_DEF (keysig, data);

GETFUNC_DEF (note, midibytes);
GETFUNC_DEF (chord, midibytes);
GETFUNC_DEF (keysig, midibytes);
GETFUNC_DEF (timesig, midibytes);
GETFUNC_DEF (tuplet, midibytes);
GETFUNC_DEF (clef, midibytes);
GETFUNC_DEF (standalone, midibytes);
GETFUNC_DEF (staff, midibytes);
GETFUNC_DEF (voice, midibytes);
GETFUNC_DEF (score, midibytes);
GETFUNC_DEF (movementcontrol, midibytes);
PUTFUNC_DEF (note, midibytes);
PUTFUNC_DEF (chord, midibytes);
PUTFUNC_DEF (keysig, midibytes);
PUTFUNC_DEF (timesig, midibytes);
PUTFUNC_DEF (tuplet, midibytes);
PUTFUNC_DEF (clef, midibytes);
PUTFUNC_DEF (standalone, midibytes);
PUTFUNC_DEF (staff, midibytes);
PUTFUNC_DEF (voice, midibytes);
PUTFUNC_DEF (score, midibytes);
PUTFUNC_DEF (movementcontrol, midibytes);
GETFUNC_DEF (note, prefix);
GETFUNC_DEF (note, postfix);
PUTFUNC_DEF (note, prefix);
// UNUSED: PUTFUNC_DEF(clef, prefix)
PUTFUNC_DEF (note, postfix);
GETFUNC_DEF (score, prefix);
GETFUNC_DEF (score, postfix);
PUTFUNC_DEF (score, prefix);
PUTFUNC_DEF (score, postfix);
PUTFUNC_DEF (staff, prefix);
PUTFUNC_DEF (voice, prefix);
GETFUNC_DEF (staff, prefix);
GETFUNC_DEF (voice, prefix);
PUTFUNC_DEF (staff, postfix);
PUTFUNC_DEF (voice, postfix);
GETFUNC_DEF (staff, postfix);
GETFUNC_DEF (voice, postfix);
GETFUNC_DEF (chord, prefix);
GETFUNC_DEF (chord, postfix);
PUTFUNC_DEF (chord, prefix);
PUTFUNC_DEF (chord, postfix);
GETFUNC_DEF (standalone, prefix);
GETFUNC_DEF (standalone, postfix);
PUTFUNC_DEF (standalone, prefix);
PUTFUNC_DEF (standalone, postfix);

#define ALLOW_PUTFUNC_DEF(what)\
SCM scheme_##what##_directive_put_allow(SCM tag, SCM value) {\
  if((!scm_is_string(tag))||(!scm_is_integer(value))){\
    return SCM_BOOL(FALSE);\
  }\
  char *tagname;\
  tagname = scm_to_locale_string(tag);\
  gint valuename = scm_to_int(value);\
  extern  gboolean  what##_directive_put_allow (gchar *tag, gint value);\
  gboolean ret = what##_directive_put_allow ((gchar*)tagname, valuename);\
  structural_change_##what ();\
  if(tagname) free(tagname);\
  return SCM_BOOL(ret);\
}

#define IGNORE_PUTFUNC_DEF(what)\
SCM scheme_##what##_directive_put_ignore(SCM tag, SCM value) {\
  if((!scm_is_string(tag))||(!scm_is_integer(value))){\
    return SCM_BOOL(FALSE);\
  }\
  char *tagname;\
  tagname = scm_to_locale_string(tag);\
  gint valuename = scm_to_int(value);\
  extern  gboolean  what##_directive_put_ignore (gchar *tag, gint value);\
  gboolean ret = what##_directive_put_ignore ((gchar*)tagname, valuename);\
  structural_change_##what ();\
  if(tagname) free(tagname);\
  return SCM_BOOL(ret);\
}

#define INT_PUTFUNC_DEF(what, field)\
SCM scheme_##what##_directive_put_##field(SCM tag, SCM value) {\
  if((!scm_is_string(tag))||(!scm_is_integer(value))){\
    return SCM_BOOL(FALSE);\
  }\
  char *tagname;\
  tagname = scm_to_locale_string(tag);\
  gint valuename = scm_to_int(value);\
  extern  gboolean  what##_directive_put_##field (gchar *tag, gint value);\
  gboolean ret = what##_directive_put_##field ((gchar*)tagname, valuename);\
structural_change_##what ();\
  if(tagname) free(tagname);\
  return SCM_BOOL(ret);\
}

#define INT_GETFUNC_DEF(what, field)\
SCM scheme_##what##_directive_get_##field(SCM tag) {\
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
SCM scheme_##what##_directive_put_graphic(SCM tag, SCM value) {\
  if((!scm_is_string(tag))||(!scm_is_string(value))){\
    return SCM_BOOL(FALSE);\
  }\
  char *tagname;\
  tagname = scm_to_locale_string(tag);\
  char *valuename;\
  valuename = scm_to_locale_string(value);\
  gboolean ret = what##_directive_put_graphic ((gchar*)tagname, (gchar*)valuename);\
structural_change_##what ();\
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

// block to copy for new int field in directive

INT_PUTFUNC_DEF (note, minpixels);
INT_PUTFUNC_DEF (chord, minpixels);
INT_PUTFUNC_DEF (standalone, minpixels);
INT_PUTFUNC_DEF (staff, minpixels);
INT_PUTFUNC_DEF (voice, minpixels);
INT_PUTFUNC_DEF (score, minpixels);
INT_PUTFUNC_DEF (clef, minpixels);
INT_PUTFUNC_DEF (timesig, minpixels);
INT_PUTFUNC_DEF (tuplet, minpixels);
INT_PUTFUNC_DEF (stemdirective, minpixels);
INT_PUTFUNC_DEF (keysig, minpixels);
INT_PUTFUNC_DEF (scoreheader, minpixels);
INT_PUTFUNC_DEF (header, minpixels);
INT_PUTFUNC_DEF (paper, minpixels);
INT_PUTFUNC_DEF (layout, minpixels);
INT_PUTFUNC_DEF (movementcontrol, minpixels);
INT_GETFUNC_DEF (note, minpixels);
INT_GETFUNC_DEF (chord, minpixels);
INT_GETFUNC_DEF (standalone, minpixels);
INT_GETFUNC_DEF (staff, minpixels);
INT_GETFUNC_DEF (voice, minpixels);
INT_GETFUNC_DEF (score, minpixels);
INT_GETFUNC_DEF (clef, minpixels);
INT_GETFUNC_DEF (timesig, minpixels);
INT_GETFUNC_DEF (tuplet, minpixels);
INT_GETFUNC_DEF (stemdirective, minpixels);
INT_GETFUNC_DEF (keysig, minpixels);
INT_GETFUNC_DEF (scoreheader, minpixels);
INT_GETFUNC_DEF (header, minpixels);
INT_GETFUNC_DEF (paper, minpixels);
INT_GETFUNC_DEF (layout, minpixels);
INT_GETFUNC_DEF (movementcontrol, minpixels);

// end block to ocpy for new int field in directive

INT_PUTFUNC_DEF (note, override);
INT_PUTFUNC_DEF (chord, override);
INT_PUTFUNC_DEF (standalone, override);
INT_PUTFUNC_DEF (staff, override);
INT_PUTFUNC_DEF (voice, override);
INT_PUTFUNC_DEF (score, override);
INT_GETFUNC_DEF (note, override);
INT_GETFUNC_DEF (chord, override);
INT_GETFUNC_DEF (standalone, override);
INT_GETFUNC_DEF (staff, override);
INT_GETFUNC_DEF (voice, override);
INT_GETFUNC_DEF (score, override);
IGNORE_PUTFUNC_DEF (note);
IGNORE_PUTFUNC_DEF (chord);
IGNORE_PUTFUNC_DEF (standalone);
IGNORE_PUTFUNC_DEF (staff);
IGNORE_PUTFUNC_DEF (voice);
// UNUSED: INT_PUTFUNC_DEF (standalone)
// UNUSED: INT_GETFUNC_DEF (note)
// UNUSED: INT_GETFUNC_DEF (chord)
// UNUSED: INT_GETFUNC_DEF (standalone)
ALLOW_PUTFUNC_DEF (note);
ALLOW_PUTFUNC_DEF (chord);
ALLOW_PUTFUNC_DEF (standalone);
ALLOW_PUTFUNC_DEF (staff);
ALLOW_PUTFUNC_DEF (voice);

// UNUSED: INT_GETFUNC_DEF (note)
// UNUSED: INT_GETFUNC_DEF (chord)
// UNUSED: INT_GETFUNC_DEF (standalone)
INT_PUTFUNC_DEF (note, ty);
INT_PUTFUNC_DEF (chord, ty);
INT_PUTFUNC_DEF (standalone, ty);
INT_GETFUNC_DEF (note, ty);
INT_GETFUNC_DEF (chord, ty);
INT_GETFUNC_DEF (standalone, ty);
INT_PUTFUNC_DEF (note, tx);
INT_PUTFUNC_DEF (chord, tx);
INT_PUTFUNC_DEF (standalone, tx);
INT_GETFUNC_DEF (note, tx);
INT_GETFUNC_DEF (chord, tx);
INT_GETFUNC_DEF (standalone, tx);
INT_PUTFUNC_DEF (note, gy);
INT_PUTFUNC_DEF (chord, gy);
INT_PUTFUNC_DEF (standalone, gy);
INT_GETFUNC_DEF (note, gy);
INT_GETFUNC_DEF (chord, gy);
INT_GETFUNC_DEF (standalone, gy);
INT_PUTFUNC_DEF (note, gx);
INT_PUTFUNC_DEF (chord, gx);
INT_PUTFUNC_DEF (standalone, gx);
INT_GETFUNC_DEF (note, gx);
INT_GETFUNC_DEF (chord, gx);
INT_GETFUNC_DEF (standalone, gx);
INT_GETFUNC_DEF (note, width);
INT_GETFUNC_DEF (chord, width);
INT_GETFUNC_DEF (standalone, width);
INT_GETFUNC_DEF (note, height);
INT_GETFUNC_DEF (chord, height);
INT_GETFUNC_DEF (standalone, height);
INT_GETFUNC_DEF (score, tx);
INT_GETFUNC_DEF (score, ty);
INT_GETFUNC_DEF (score, gx);
INT_GETFUNC_DEF (score, gy);
INT_GETFUNC_DEF (score, width);
INT_GETFUNC_DEF (score, height);
ALLOW_PUTFUNC_DEF (score);
IGNORE_PUTFUNC_DEF (score);
INT_PUTFUNC_DEF (score, tx);
INT_PUTFUNC_DEF (score, ty);
INT_PUTFUNC_DEF (score, gx);
INT_PUTFUNC_DEF (score, gy);
INT_GETFUNC_DEF (object, minpixels);
INT_PUTFUNC_DEF (object, minpixels);
DELETE_FN_DEF (object);

// block to copy for new type of directive, !!minpixels is done in block to copy for new fields!!

GETFUNC_DEF (clef, prefix) GETFUNC_DEF (clef, postfix) GETFUNC_DEF (clef, display) PUTFUNC_DEF (clef, prefix) PUTFUNC_DEF (clef, postfix) PUTFUNC_DEF (clef, display) PUTGRAPHICFUNC_DEF (clef);

ALLOW_PUTFUNC_DEF (clef);
IGNORE_PUTFUNC_DEF (clef);
INT_PUTFUNC_DEF (clef, tx);
INT_PUTFUNC_DEF (clef, ty);
INT_PUTFUNC_DEF (clef, gx);
INT_PUTFUNC_DEF (clef, gy);
INT_PUTFUNC_DEF (clef, override);
INT_GETFUNC_DEF (clef, tx);
INT_GETFUNC_DEF (clef, ty);
INT_GETFUNC_DEF (clef, gx);
INT_GETFUNC_DEF (clef, gy);
INT_GETFUNC_DEF (clef, override);
INT_GETFUNC_DEF (clef, width);
INT_GETFUNC_DEF (clef, height);
EDIT_DELETE_FN_DEF (clef);

// end block

GETFUNC_DEF (timesig, prefix) GETFUNC_DEF (timesig, postfix) GETFUNC_DEF (timesig, display) PUTFUNC_DEF (timesig, prefix) PUTFUNC_DEF (timesig, postfix) PUTFUNC_DEF (timesig, display) PUTGRAPHICFUNC_DEF (timesig);

ALLOW_PUTFUNC_DEF (timesig);
IGNORE_PUTFUNC_DEF (timesig);
INT_PUTFUNC_DEF (timesig, tx);
INT_PUTFUNC_DEF (timesig, ty);
INT_PUTFUNC_DEF (timesig, gx);
INT_PUTFUNC_DEF (timesig, gy);
INT_PUTFUNC_DEF (timesig, override);
INT_GETFUNC_DEF (timesig, tx);
INT_GETFUNC_DEF (timesig, ty);
INT_GETFUNC_DEF (timesig, gx);
INT_GETFUNC_DEF (timesig, gy);
INT_GETFUNC_DEF (timesig, override);
INT_GETFUNC_DEF (timesig, width);
INT_GETFUNC_DEF (timesig, height);
EDIT_DELETE_FN_DEF (timesig);
GETFUNC_DEF (tuplet, prefix);
GETFUNC_DEF (tuplet, postfix);
GETFUNC_DEF (tuplet, display);
PUTFUNC_DEF (tuplet, prefix);
PUTFUNC_DEF (tuplet, postfix);
PUTFUNC_DEF (tuplet, display);
PUTGRAPHICFUNC_DEF (tuplet);

ALLOW_PUTFUNC_DEF (tuplet);
IGNORE_PUTFUNC_DEF (tuplet);
INT_PUTFUNC_DEF (tuplet, tx);
INT_PUTFUNC_DEF (tuplet, ty);
INT_PUTFUNC_DEF (tuplet, gx);
INT_PUTFUNC_DEF (tuplet, gy);
INT_PUTFUNC_DEF (tuplet, override);
INT_GETFUNC_DEF (tuplet, tx);
INT_GETFUNC_DEF (tuplet, ty);
INT_GETFUNC_DEF (tuplet, gx);
INT_GETFUNC_DEF (tuplet, gy);
INT_GETFUNC_DEF (tuplet, override);
INT_GETFUNC_DEF (tuplet, width);
INT_GETFUNC_DEF (tuplet, height);
EDIT_DELETE_FN_DEF (tuplet);
GETFUNC_DEF (stemdirective, prefix);
GETFUNC_DEF (stemdirective, postfix);
GETFUNC_DEF (stemdirective, display);
PUTFUNC_DEF (stemdirective, prefix);
PUTFUNC_DEF (stemdirective, postfix);
PUTFUNC_DEF (stemdirective, display);
PUTGRAPHICFUNC_DEF (stemdirective);

ALLOW_PUTFUNC_DEF (stemdirective);
IGNORE_PUTFUNC_DEF (stemdirective);
INT_PUTFUNC_DEF (stemdirective, tx);
INT_PUTFUNC_DEF (stemdirective, ty);
INT_PUTFUNC_DEF (stemdirective, gx);
INT_PUTFUNC_DEF (stemdirective, gy);
INT_PUTFUNC_DEF (stemdirective, override);
INT_GETFUNC_DEF (stemdirective, tx);
INT_GETFUNC_DEF (stemdirective, ty);
INT_GETFUNC_DEF (stemdirective, gx);
INT_GETFUNC_DEF (stemdirective, gy);
INT_GETFUNC_DEF (stemdirective, override);
INT_GETFUNC_DEF (stemdirective, width);
INT_GETFUNC_DEF (stemdirective, height);
EDIT_DELETE_FN_DEF (stemdirective);
GETFUNC_DEF (keysig, prefix);
GETFUNC_DEF (keysig, postfix);
GETFUNC_DEF (keysig, display);
PUTFUNC_DEF (keysig, prefix);
PUTFUNC_DEF (keysig, postfix);
PUTFUNC_DEF (keysig, display);
PUTGRAPHICFUNC_DEF (keysig);

ALLOW_PUTFUNC_DEF (keysig);
IGNORE_PUTFUNC_DEF (keysig);
INT_PUTFUNC_DEF (keysig, tx);
INT_PUTFUNC_DEF (keysig, ty);
INT_PUTFUNC_DEF (keysig, gx);
INT_PUTFUNC_DEF (keysig, gy);
INT_PUTFUNC_DEF (keysig, override);
INT_GETFUNC_DEF (keysig, tx);
INT_GETFUNC_DEF (keysig, ty);
INT_GETFUNC_DEF (keysig, gx);
INT_GETFUNC_DEF (keysig, gy);
INT_GETFUNC_DEF (keysig, override);
INT_GETFUNC_DEF (keysig, width);
INT_GETFUNC_DEF (keysig, height);
EDIT_DELETE_FN_DEF (keysig);
GETFUNC_DEF (scoreheader, prefix);
GETFUNC_DEF (scoreheader, postfix);
GETFUNC_DEF (scoreheader, display);
PUTFUNC_DEF (scoreheader, prefix);
PUTFUNC_DEF (scoreheader, postfix);
PUTFUNC_DEF (scoreheader, display);
PUTGRAPHICFUNC_DEF (scoreheader);

ALLOW_PUTFUNC_DEF (scoreheader);
IGNORE_PUTFUNC_DEF (scoreheader);
INT_PUTFUNC_DEF (scoreheader, tx);
INT_PUTFUNC_DEF (scoreheader, ty);
INT_PUTFUNC_DEF (scoreheader, gx);
INT_PUTFUNC_DEF (scoreheader, gy);
INT_PUTFUNC_DEF (scoreheader, override);
INT_GETFUNC_DEF (scoreheader, tx);
INT_GETFUNC_DEF (scoreheader, ty);
INT_GETFUNC_DEF (scoreheader, gx);
INT_GETFUNC_DEF (scoreheader, gy);
INT_GETFUNC_DEF (scoreheader, override);
INT_GETFUNC_DEF (scoreheader, width);
INT_GETFUNC_DEF (scoreheader, height);
EDIT_DELETE_FN_DEF (scoreheader);
GETFUNC_DEF (header, prefix);
GETFUNC_DEF (header, postfix);
GETFUNC_DEF (header, display);
PUTFUNC_DEF (header, prefix);
PUTFUNC_DEF (header, postfix);
PUTFUNC_DEF (header, display);
PUTGRAPHICFUNC_DEF (header);

ALLOW_PUTFUNC_DEF (header);
IGNORE_PUTFUNC_DEF (header);
INT_PUTFUNC_DEF (header, tx);
INT_PUTFUNC_DEF (header, ty);
INT_PUTFUNC_DEF (header, gx);
INT_PUTFUNC_DEF (header, gy);
INT_PUTFUNC_DEF (header, override);
INT_GETFUNC_DEF (header, tx);
INT_GETFUNC_DEF (header, ty);
INT_GETFUNC_DEF (header, gx);
INT_GETFUNC_DEF (header, gy);
INT_GETFUNC_DEF (header, override);
INT_GETFUNC_DEF (header, width);
INT_GETFUNC_DEF (header, height);
EDIT_DELETE_FN_DEF (header);
GETFUNC_DEF (paper, prefix);
GETFUNC_DEF (paper, postfix);
GETFUNC_DEF (paper, display);
PUTFUNC_DEF (paper, prefix);
PUTFUNC_DEF (paper, postfix);
PUTFUNC_DEF (paper, display);
PUTGRAPHICFUNC_DEF (paper);

ALLOW_PUTFUNC_DEF (paper);
IGNORE_PUTFUNC_DEF (paper);
INT_PUTFUNC_DEF (paper, tx);
INT_PUTFUNC_DEF (paper, ty);
INT_PUTFUNC_DEF (paper, gx);
INT_PUTFUNC_DEF (paper, gy);
INT_PUTFUNC_DEF (paper, override);
INT_GETFUNC_DEF (paper, tx);
INT_GETFUNC_DEF (paper, ty);
INT_GETFUNC_DEF (paper, gx);
INT_GETFUNC_DEF (paper, gy);
INT_GETFUNC_DEF (paper, override);
INT_GETFUNC_DEF (paper, width);
INT_GETFUNC_DEF (paper, height);
EDIT_DELETE_FN_DEF (paper);
GETFUNC_DEF (layout, prefix);
GETFUNC_DEF (layout, postfix);
GETFUNC_DEF (layout, display);
PUTFUNC_DEF (layout, prefix);
PUTFUNC_DEF (layout, postfix);
PUTFUNC_DEF (layout, display);
PUTGRAPHICFUNC_DEF (layout);

ALLOW_PUTFUNC_DEF (layout);
IGNORE_PUTFUNC_DEF (layout);
INT_PUTFUNC_DEF (layout, tx);
INT_PUTFUNC_DEF (layout, ty);
INT_PUTFUNC_DEF (layout, gx);
INT_PUTFUNC_DEF (layout, gy);
INT_PUTFUNC_DEF (layout, override);
INT_GETFUNC_DEF (layout, tx);
INT_GETFUNC_DEF (layout, ty);
INT_GETFUNC_DEF (layout, gx);
INT_GETFUNC_DEF (layout, gy);
INT_GETFUNC_DEF (layout, override);
INT_GETFUNC_DEF (layout, width);
INT_GETFUNC_DEF (layout, height);
EDIT_DELETE_FN_DEF (layout);
GETFUNC_DEF (movementcontrol, prefix);
GETFUNC_DEF (movementcontrol, postfix);
PUTFUNC_DEF (movementcontrol, prefix);
PUTFUNC_DEF (movementcontrol, postfix);
PUTGRAPHICFUNC_DEF (movementcontrol);

ALLOW_PUTFUNC_DEF (movementcontrol);
IGNORE_PUTFUNC_DEF (movementcontrol);
INT_PUTFUNC_DEF (movementcontrol, tx);
INT_PUTFUNC_DEF (movementcontrol, ty);
INT_PUTFUNC_DEF (movementcontrol, gx);
INT_PUTFUNC_DEF (movementcontrol, gy);
INT_PUTFUNC_DEF (movementcontrol, override);
INT_GETFUNC_DEF (movementcontrol, tx);
INT_GETFUNC_DEF (movementcontrol, ty);
INT_GETFUNC_DEF (movementcontrol, gx);
INT_GETFUNC_DEF (movementcontrol, gy);
INT_GETFUNC_DEF (movementcontrol, override);
INT_GETFUNC_DEF (movementcontrol, width);
INT_GETFUNC_DEF (movementcontrol, height);
EDIT_DELETE_FN_DEF (movementcontrol);

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



SCM
scheme_get_username (void)
{
  return scm_from_locale_string (Denemo.prefs.username->str);
}

SCM
scheme_get_password (void)
{
  return scm_from_locale_string (Denemo.prefs.password->str);
}

SCM
scheme_set_midi_capture (SCM setting)
{
  gboolean prev;
  prev = set_midi_capture ((setting != SCM_BOOL_F));
  return prev ? SCM_BOOL_T : SCM_BOOL_F;
}
SCM
scheme_virtual_keyboard (SCM octaves)
{
    gint num = 6;
    if (scm_is_integer (octaves))
        {
           num = scm_to_int (octaves);
        }
  create_virtual_keyboard (num);
  return SCM_BOOL_T;
}
SCM
scheme_get_keyboard_state (void)
{
  return scm_from_int (Denemo.keyboard_state);
}

SCM
scheme_set_midi_thru (SCM set)  // see also d-MidiInListening this doesn't lock the state against keyboard changes
{
  SCM ret = scm_from_int (Denemo.keyboard_state);
  if (scm_is_true (set))
    Denemo.keyboard_state = GDK_SHIFT_MASK;
  else
    Denemo.keyboard_state = 0;
  set_midi_in_status ();
  return ret;
}

SCM
scheme_get_recorded_midi_on_tick (void)
{
  smf_track_t *track = Denemo.project->movement->recorded_midi_track;
  if (track)
    {
#define MIDI_NOTEOFF        0x80
#define MIDI_NOTEON     0x90
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

SCM
scheme_get_recorded_midi_note (void)
{
  smf_track_t *track = Denemo.project->movement->recorded_midi_track;
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

SCM
scheme_rewind_recorded_midi (void)
{
  smf_track_t *track = Denemo.project->movement->recorded_midi_track;
  if (track)
    {
      if (track->smf == NULL)
        {
          if (Denemo.project->movement->smf)
            {
              smf_add_track (Denemo.project->movement->smf, track);
              smf_rewind (Denemo.project->movement->smf);
            }
          else
            return SCM_BOOL_F;
        }
      smf_rewind (track->smf);
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

SCM
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

SCM
scheme_get_midi (SCM bytes)
{
  gint midi;
  SCM scm;
  gboolean success = intercept_midi_event (&midi);
  if (scm_is_false (bytes))
    {
      if (!success)
        scm = SCM_BOOL_F;
      else
        {
          Denemo.project->last_source = INPUTMIDI;
          scm = scm_list_n (scm_from_int (midi & 0xFF), scm_from_int ((midi >> 8) & 0xFF), scm_from_int ((midi >> 16) & 0xFF), scm_from_int (midi >> 24), SCM_UNDEFINED);
        }
    }
  else
    {
      if (!success)
        midi = 0;               /* scripts should detect this impossible value and take action */
      else
        Denemo.project->last_source = INPUTMIDI;
      gchar *buf = (gchar *) & midi;
      *buf &= 0xF0;             //do not return channel info

      scm = scm_from_int (midi);
    }
  return scm;
}

//Simulates a midi event, with no capture by any calling scheme script unless midi==0
SCM
scheme_put_midi (SCM scm)
{
  gchar buf[3];
  gint midi;
  if (scm_is_list (scm))
    {
      buf[0] = scm_to_int (scm_list_ref (scm, scm_from_int (0)));
      buf[1] = scm_to_int (scm_list_ref (scm, scm_from_int (1)));
      buf[2] = scm_to_int (scm_list_ref (scm, scm_from_int (2)));
      midi = TRUE;
    }
  else
    {
      midi = scm_to_int (scm);
      buf[0] = midi & 0xFF;
      buf[1] = (midi >> 8) & 0xFF;
      buf[2] = (midi >> 16) & 0xFF;
    }

  //g_debug("got %x\nbreaks as %x %x %x\n", midi&0xFFFFFF, buf[0], buf[1], buf[2]);
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

SCM
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
  DenemoStaff *curstaffstruct = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
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
  //g_debug("\nbuffer[0] = %x buffer[1] = %x buffer[2] = %x\n", buffer[0], buffer[1], buffer[2]);

  play_midi_event (DEFAULT_BACKEND, curstaffstruct->midi_port, buffer);

  if (string_input)
    free (string_input);
  return SCM_BOOL (TRUE);
}

SCM
scheme_create_timebase (SCM optional)
{
  DenemoMovement *si = Denemo.project->movement;
  if (si->smfsync != si->changecount)
    {
      exportmidi (NULL, si);
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

SCM
scheme_pending_midi (SCM scm)
{
  if (scm_is_integer (scm))
    {
      guint key = scm_to_int (scm);
      g_queue_push_head (Denemo.project->pending_midi, GINT_TO_POINTER (key));
      return SCM_BOOL_T;
    }
  else
    return SCM_BOOL_F;
}

SCM
scheme_play_midi_note (SCM note, SCM volume, SCM channel, SCM duration)
{
  guint vol = scm_to_int (volume);
  gint key = scm_to_int (note);
  gint chan = scm_to_int (channel);
  gint dur = scm_to_int (duration);

  //g_debug("Playing %x at %f volume, %d channel for %dms\n", key, vol/255.0, channel, dur);
  play_note (DEFAULT_BACKEND, 0 /*port */ , chan, key, dur, vol);
  return SCM_BOOL (TRUE);
}

SCM
scheme_play_midikey (SCM scm)
{
  guint midi = scm_to_int (scm);
  gint key = (midi >> 8) & 0xFF;
  gint channel = midi & 0xF;
  gint volume = ((midi >> 16) & 0x7F);
  //g_debug("Playing %x at %f volume, %d channel\n", key, (double)volume, channel);
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
//FIXME should not allow spillover?
  dnm_insertnote (Denemo.project, duration, 0, TRUE);
  displayhelper (Denemo.project);       //without this a call to d-AddVoice causes a crash as the chord length info has not been updated
  return SCM_BOOL_T;
}

//Insert a note without setting the prevailing duration
SCM
scheme_put_note (SCM optional_duration)
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
  gboolean spill = Denemo.prefs.spillover;
  gint mode = Denemo.project->mode;
  Denemo.project->mode = 0;
  Denemo.prefs.spillover = 0;

  if (scm_is_false (optional_duration))
    dnm_insertnote (Denemo.project, duration, INPUTNORMAL | INPUTBLANK, FALSE);        //pass #f for nonprinting note
  else
    dnm_insertnote (Denemo.project, duration, INPUTNORMAL, FALSE);
  Denemo.project->mode = mode;
  Denemo.prefs.spillover = spill;

  displayhelper (Denemo.project);       //without this a call to d-AddVoice causes a crash as the chord length info has not been updated
  return SCM_BOOL_T;
}

//Insert a chord at the cursor notes contains space separated lilypond note names
SCM
scheme_insert_chord (SCM notes, SCM duration)
{
    GList *thenotes = NULL;
    gint theduration;
    if (scm_is_integer (duration))
        theduration = scm_to_int (duration);
    else
        theduration = get_prevailing_duration ();
   if (scm_is_string (notes))
    {
        gchar *notestring = scm_to_locale_string (notes);
        gchar *thenote = strtok (notestring, " ");
        
        while (thenote)
           {
               struct twoints *data = (struct twoints *)g_malloc (sizeof (struct twoints));
               interpret_lilypond_notename (thenote, &data->a, &data->b);
               thenotes = g_list_prepend (thenotes, data);
               thenote = strtok (NULL, " ");
            }
       insert_chord (thenotes, theduration);
       return SCM_BOOL_T;
   } 
        
    return SCM_BOOL_F;
}

//Insert a rest in the given (or prevailing duration) and set the prevailing duration
SCM
scheme_insert_rest (SCM optional)
{
  SCM ret = scheme_put_rest (optional);
  if (scm_is_integer (optional))
    {
      gint duration = scm_to_int (optional);
      highlight_duration (Denemo.project, duration);
    }
  return ret;
}


SCM
scheme_toggle_playalong (void)
{
  pb_playalong (get_playalong_button ());
  return SCM_BOOL (Denemo.project->midi_destination | MIDIPLAYALONG);
}

SCM
scheme_toggle_conduct (void)
{
  pb_conduct (get_conduct_button ());
  return SCM_BOOL (Denemo.project->midi_destination | MIDICONDUCT);
}

SCM
scheme_midi_record (SCM script)
{
  if (is_playing ())
    return SCM_BOOL_F;
  if (scm_is_string (script))
    {
      gchar *text = scm_to_locale_string (script);
      pb_record (text);
      free (text);
    }
  else
    pb_record (NULL);
  return SCM_BOOL (Denemo.project->midi_destination | MIDIRECORD);
}

SCM
scheme_compute_midi_note_durations (void)
{
  return SCM_BOOL (compute_midi_note_durations ());
}

SCM
scheme_get_marked_midi_note (void)
{
  SCM scm = SCM_BOOL_F;
  DenemoProject *gui = Denemo.project;
  DenemoMovement *si = gui->movement;
  if (si->recording && (si->recording->type == DENEMO_RECORDING_MIDI) && si->marked_onset)
    {
      GList *marked = si->marked_onset;
      DenemoRecordedNote *thenote = (DenemoRecordedNote *) marked->data;
      gchar *name = mid_c_offsettolily (thenote->mid_c_offset, thenote->enshift);
      gchar *str = g_strdup_printf ("%s", mid_c_offsettolily (thenote->mid_c_offset + 7 * thenote->octave, thenote->enshift));
      scm = scm_from_locale_string (str);
    }
  return scm;
}

SCM
scheme_get_marked_midi_note_seconds (void)
{
  SCM scm = SCM_BOOL_F;
  DenemoProject *gui = Denemo.project;
  DenemoMovement *si = gui->movement;
  if (si->recording && (si->recording->type == DENEMO_RECORDING_MIDI) && si->marked_onset)
    {
      GList *marked = si->marked_onset;
      DenemoRecordedNote *thenote = (DenemoRecordedNote *) marked->data;
      gdouble seconds = thenote->timing / ((gdouble) si->recording->samplerate);
      scm = scm_from_double (seconds);
    }
  return scm;
}

SCM
scheme_advance_marked_midi (SCM advance)
{
  SCM scm = SCM_BOOL_F;
  DenemoProject *gui = Denemo.project;
  DenemoMovement *si = gui->movement;
  if (si->recording && (si->recording->type == DENEMO_RECORDING_MIDI))
    {
      if (SCM_UNBNDP (advance))
        {
          if (si->marked_onset)
            si->marked_onset = si->marked_onset->next;
        }
      else if (scm_is_integer (advance))
        {
          gint i = scm_to_int (advance);
          if (i > 0)
            {
              while (i-- && si->marked_onset)
                si->marked_onset = si->marked_onset->next;
            }
          else if (i < 0)
            {
              while (i++ && si->marked_onset)
                si->marked_onset = si->marked_onset->prev;
            }
          else
            si->marked_onset = si->recording->notes;

        }
      else if (scm_is_false (advance))
        {
          si->marked_onset = NULL;
          return SCM_BOOL_T;
        }
      if (si->marked_onset)
        scm = SCM_BOOL_T;
    }

  return scm;
}

SCM
scheme_insert_marked_midi_note (void)
{
  return SCM_BOOL (insert_marked_midi_note ());
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
  if (scheme->id == Denemo.project->id)
    call_out_to_guile (scheme_code);
  else
    g_warning ("Timer missed for gui %d", scheme->id);
  g_free (scheme);
  free (scheme_code);
  return FALSE;
}

SCM
scheme_one_shot_timer (SCM duration_amount, SCM callback)
{
  char *scheme_code;
  scheme_code = scm_to_locale_string (callback);
  gint duration = scm_to_int (duration_amount);
  cb_scheme_and_id *scheme = g_malloc (sizeof (cb_scheme_and_id));
  scheme->scheme_code = scheme_code;
  scheme->id = Denemo.project->id;
  g_timeout_add (duration, (GSourceFunc) scheme_callback_one_shot_timer, GINT_TO_POINTER (scheme));
  return SCM_BOOL (TRUE);
}

static gboolean
scheme_callback_timer (cb_scheme_and_id * scheme)
{
  char *scheme_code = scheme->scheme_code;
  if (scheme->id == Denemo.project->id)
    call_out_to_guile (scheme_code);
  else
    g_warning ("Timer missed for gui %d", scheme->id);

  return TRUE;                  //continue to call
}


SCM
scheme_timer (SCM duration_amount, SCM callback)
{
  char *scheme_code;
  if (scm_is_string (callback))
    {
      scheme_code = scm_to_locale_string (callback);    //FIXME check that type of callback is tring
      gint duration = scm_to_int (duration_amount);
      //g_debug("setting timer for %s after %d ms", scheme_code, duration);
      cb_scheme_and_id *scheme = g_malloc (sizeof (cb_scheme_and_id));
      scheme->scheme_code = scheme_code;
      scheme->id = Denemo.project->id;
      g_timeout_add (duration, (GSourceFunc) scheme_callback_timer, GINT_TO_POINTER (scheme));
      //if(scheme_code) free(scheme_code);
      return scm_from_int (GPOINTER_TO_INT (scheme));   //FIXME pointer may not fit in int
    }
  else
    return SCM_BOOL_F;
}

SCM
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




SCM
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

SCM
scheme_has_figures (SCM optional)
{
  return SCM_BOOL (((DenemoStaff *) Denemo.project->movement->currentstaff->data)->hasfigures);
}

SCM
scheme_get_bass_figure (void)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) 
      || !(thechord = (chord *) curObj->object)
      || ! (((DenemoStaff *) Denemo.project->movement->currentstaff->data)->hasfigures))
    return SCM_BOOL_F;
  else
    { 
      if ((thechord->figure == NULL) || ((((GString *) ((chord *) thechord->figure))->str) == NULL))
        return scm_from_locale_string ("_");       /* the no-figure figure */
      else
        return scm_from_locale_string (((GString *) ((chord *) thechord->figure))->str);    
    }
}

//badly named:
SCM
scheme_put_note_name (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
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
          //g_debug("note %s gives %d and %d\n", str, mid_c_offset, enshift);
          modify_note (thechord, mid_c_offset, enshift, find_prevailing_clef (Denemo.project->movement));
          if (str)
            free (str);
          return SCM_BOOL (TRUE);
        }
    }
  return SCM_BOOL (FALSE);
}

SCM
scheme_set_accidental (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
    return SCM_BOOL (FALSE);
  else
    {
      GList *g;
      for (g = thechord->notes; g; g = g->next)
        {
          thenote = (note *) g->data;
          if (thenote->mid_c_offset == Denemo.project->movement->cursor_y)
            break;
        }
      if (g == NULL)
        return SCM_BOOL_F;
      DenemoMovement *si = Denemo.project->movement;
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
      showwhichaccidentals ((objnode *) ((DenemoMeasure *) si->currentmeasure->data)->objects);
      //  find_xes_in_measure (si, si->currentmeasurenum, si->cursortime1,
      //                      si->cursortime2); causes a crash, si is not passed correctly, why???
      //thenote->mid_c_offset = interpret_lilypond_notename(str);
      displayhelper (Denemo.project);
      if (str)
        free (str);
      return SCM_BOOL (TRUE);
    }
}





//create a putnote here that takes a duration and numdots and note name, inserts a chord and calls the scheme_put_note_name above - this can be done via script at present, e.g. (d-C) (d-Change3) (d-AddDot) (d-PutNoteName "eis''")


//Puts a note into the chord at the cursor PARAM lily is a string representation of the note
SCM
scheme_insert_note_in_chord (SCM lily)
{
  DenemoProject *gui = Denemo.project;
  DenemoObject *curObj;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data))
    return SCM_BOOL_F;
#ifdef INSERT_NOTE_IN_CHORD_WORKS_ON_PREVIOUS_CHORD
  if (curObj->type != CHORD)
    {
      objnode *theobj = Denemo.project->movement->currentobject;
      while (theobj->prev)
        {
          theobj = theobj->prev;
          curObj = theobj->data;
          if (curObj->type == CHORD)
            break;
        }
      if (curObj->type != CHORD)
        return SCM_BOOL_F;
    }
#else
  if (curObj->type != CHORD)
    return SCM_BOOL_F;
#endif

  char *str = NULL;
  if (scm_is_string (lily))
    {
      str = scm_to_locale_string (lily);
      gint mid_c_offset;
      gint enshift;
      interpret_lilypond_notename (str, &mid_c_offset, &enshift);

      //g_debug("note %s gives %d and %d\n", str, mid_c_offset, enshift);
      addtone (curObj, mid_c_offset, enshift);
      score_status (gui, TRUE);
      displayhelper (Denemo.project);
      if (str)
        free (str);
      return SCM_BOOL_T;
    }
  return SCM_BOOL (FALSE);
}


//return the number of objects in the copybuffer at staff m
SCM
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
SCM
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
SCM
scheme_put_clip_obj (SCM m, SCM n)
{
  gint value = scm_to_int (n);
  gint staff = scm_to_int (m);
  return SCM_BOOL (insert_clip_obj (staff, value));
}

SCM
scheme_adjust_xes (SCM optional)
{
  find_xes_in_all_measures (Denemo.project->movement);
  return SCM_BOOL_T;
}

static gint
flash_cursor (void)
{
  draw_score_area ();
  // draw_score (NULL); what was this for?????
  return TRUE;
}

SCM
scheme_highlight_cursor (SCM optional)
{
  static gint id;
  SCM ret = SCM_BOOL_T;
  gboolean old_value = Denemo.prefs.cursor_highlight;
  if (scm_is_bool (optional))
    {
      Denemo.prefs.cursor_highlight = scm_is_true (optional);
      ret = old_value ? SCM_BOOL_T : SCM_BOOL_F;
    }
  else
    {
      Denemo.prefs.cursor_highlight = !Denemo.prefs.cursor_highlight;
    }
  if (id && !Denemo.prefs.cursor_highlight)
    {
      g_source_remove (id);
      id = 0;
    }
  else if (Denemo.prefs.cursor_highlight)
    id = g_timeout_add (500, (GSourceFunc) flash_cursor, NULL);
  //g_debug("Cursor highlighting %d id %d", Denemo.prefs.cursor_highlight, id);
  return ret;
}

SCM
scheme_get_type (SCM at_or_before)
{
  DenemoObject *curObj;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || !(DENEMO_OBJECT_TYPE_NAME (curObj)))
    return scm_from_locale_string ("None");
  if (scm_is_true (at_or_before) && Denemo.project->movement->cursor_appending)
    return scm_from_locale_string ("Appending");
  return scm_from_locale_string (DENEMO_OBJECT_TYPE_NAME (curObj));
}

SCM
scheme_get_lilypond (SCM optional)
{
  DenemoProject *gui = Denemo.project;
  DenemoObject *curObj;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || !(DENEMO_OBJECT_TYPE_NAME (curObj)))
    return SCM_BOOL_F;
//g_debug("Before %d %d\n", gui->lilysync, gui->changecount);

  if (gui->lilysync != gui->changecount)
    refresh_lily_cb (NULL, Denemo.project);
//g_debug("After %d %d\n", gui->lilysync, gui->changecount);
  if (curObj->lilypond)
    return scm_from_locale_string (curObj->lilypond);
  return SCM_BOOL_F;
}

SCM
scheme_refresh_lilypond (void)
{
    force_lily_refresh (Denemo.project);
    return SCM_BOOL_T;
}

SCM
scheme_get_tuplet (SCM optional)
{
  DenemoObject *curObj;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != TUPOPEN))
    return SCM_BOOL_F;
  GString *ratio = g_string_new ("");
  g_string_printf (ratio, "%d/%d", ((tupopen *) curObj->object)->numerator, ((tupopen *) curObj->object)->denominator);
  return scm_from_locale_string (g_string_free (ratio, FALSE));
}

SCM
scheme_set_tuplet (SCM ratio)
{
  DenemoObject *curObj;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != TUPOPEN))
    {
      return SCM_BOOL_F;
    }
  char *theratio;
  theratio = scm_to_locale_string (ratio);
  sscanf (theratio, "%d/%d", &((tupopen *) curObj->object)->numerator, &((tupopen *) curObj->object)->denominator);
  //g_debug("Set %d/%d\n", (((tupopen*)curObj->object)->numerator), (((tupopen*)curObj->object)->denominator));
  free (theratio);
  if (((tupopen *) curObj->object)->denominator)
    {
      return SCM_BOOL_T;
    }
  ((tupopen *) curObj->object)->denominator = 1;
  return SCM_BOOL_F;
}

SCM
scheme_set_background (SCM color)
{
  if (scm_is_integer (color))
    {
      gint value = scm_to_int (color);
      Denemo.color = value;
      draw_score_area ();
      if (!Denemo.non_interactive)
        draw_score (NULL);
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}


SCM
scheme_get_nonprinting (SCM optional)
{
  DenemoObject *curObj;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || curObj->isinvisible)
    return SCM_BOOL_T;
  return SCM_BOOL_F;
}

SCM
scheme_set_nonprinting (SCM optional)
{
  DenemoObject *curObj;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data))
    return SCM_BOOL_F;
  if (scm_is_false (optional))
    curObj->isinvisible = FALSE;
  else
    curObj->isinvisible = TRUE;
  return SCM_BOOL_T;
}

SCM
scheme_is_grace (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->is_grace))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

SCM
scheme_is_tied (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->is_tied))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}


SCM
scheme_is_slur_start (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->slur_begin_p))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

SCM
scheme_is_slur_end (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->slur_end_p))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

SCM
scheme_is_cresc_start (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->crescendo_begin_p))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

SCM
scheme_is_cresc_end (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->crescendo_end_p))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

SCM
scheme_is_dim_start (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->diminuendo_begin_p))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

SCM
scheme_is_dim_end (SCM optional)
{
  DenemoObject *curObj;
  chord *thechord;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->diminuendo_end_p))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

SCM
scheme_is_in_selection (void)
{
  return SCM_BOOL (in_selection (Denemo.project->movement));
}

SCM
scheme_has_selection (void)
{
  return SCM_BOOL (Denemo.project->movement->markstaffnum != 0);
}

SCM
scheme_is_appending (void)
{
  return SCM_BOOL (Denemo.project->movement->cursor_appending);
}




SCM
scheme_clear_clipboard (SCM optional)
{
  clearbuffer ();
  return SCM_BOOL (TRUE);
}

SCM
scheme_get_staffs_in_clipboard (SCM optional)
{
  gint num = get_staffs_in_clipboard ();
  if (num)
    return scm_from_int (num);
  return SCM_BOOL_F;
}


SCM
scheme_get_measures_in_staff (SCM optional)
{
  gint num = g_list_length (((DenemoStaff *) Denemo.project->movement->currentstaff->data)->themeasures);
  return scm_from_int (num);
}

SCM
scheme_get_staffs_in_movement (SCM optional)
{
  gint num = g_list_length (Denemo.project->movement->thescore);
  return scm_from_int (num);
}

SCM
scheme_get_movements_in_score (void)
{
  gint num = g_list_length (Denemo.project->movements);
  return scm_from_int (num);
}

SCM
scheme_set_lines_in_staff (SCM lines)
{
  DenemoStaff *thestaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (scm_is_integer (lines))
    {
      gint value = scm_to_int (lines);
      thestaff->no_of_lines = value;
      displayhelper (Denemo.project);
    }

  return scm_from_int (thestaff->no_of_lines);
}

SCM
scheme_inherit_staff_properties (void)
{
  DenemoStaff *thestaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  DenemoStaff *prevstaff = (DenemoStaff *) (Denemo.project->movement->currentstaff->prev ? Denemo.project->movement->currentstaff->prev->data : NULL);

  if (prevstaff)
    {
      staff_copy (prevstaff, thestaff, FALSE);
      return SCM_BOOL_T;
    }

  return SCM_BOOL_F;
}

static SCM
set_staff_range (SCM setting, gboolean hi)
{
  DenemoStaff *thestaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (scm_is_integer (setting))
    {
      gint value = scm_to_int (setting);
      *(hi ? &thestaff->range_hi : &thestaff->range_lo) = value;
      thestaff->range = TRUE;
    }
  if (thestaff->range)
    return scm_from_int (hi ? thestaff->range_hi : thestaff->range_lo);
  return SCM_BOOL_F;
}

SCM
scheme_set_staff_range_hi (SCM hi)
{
  return set_staff_range (hi, TRUE);
}

SCM
scheme_set_staff_range_lo (SCM lo)
{
  return set_staff_range (lo, FALSE);
}

SCM
scheme_set_staff_range (void)
{
  DenemoStaff *thestaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (Denemo.project->movement->currentobject)
    {
      DenemoObject *curobj = Denemo.project->movement->currentobject->data;
      if (curobj->type == CHORD)
        {
          chord *thechord = ((chord *) curobj->object);
          thestaff->range_hi = thechord->highestpitch;
          thestaff->range_lo = thechord->lowestpitch;
          thestaff->range = TRUE;
          return SCM_BOOL_T;
        }
    }
  return SCM_BOOL_F;
}

SCM
scheme_shorten_staff_height (SCM shorten)
{
  DenemoStaff *thestaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;

  if (scm_is_integer (shorten))
    {
      gint value = scm_to_int (shorten);
      thestaff->space_shorten = value;
      displayhelper (Denemo.project);
    }
  return scm_from_int (thestaff->space_shorten);
}

SCM
scheme_set_color_of_staff (SCM color)
{
  DenemoStaff *thestaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  gint current = thestaff->color;
  if (scm_is_integer (color))
    {
      gint value = scm_to_ulong (color);
      thestaff->color = value;
      displayhelper (Denemo.project);
    }

  return scm_from_ulong (thestaff->color);
}

SCM
scheme_staff_to_voice (SCM optional)
{
  SCM ret = SCM_BOOL_F;
  DenemoStaff *current = ((DenemoStaff *) Denemo.project->movement->currentstaff->data);
  if (Denemo.project->movement->currentstaff->prev && (current->voicecontrol == DENEMO_PRIMARY))
    {
      current->voicecontrol |= DENEMO_SECONDARY;
      staff_set_current_primary (Denemo.project->movement);
      DenemoStaff *primary = (DenemoStaff *) Denemo.project->movement->currentprimarystaff->data;
      if ((current->timesig.time1 != primary->timesig.time1) || (current->timesig.time2 != primary->timesig.time2))
        {
          warningdialog (_("Time Signatures do not match, will not make voice"));
          current->voicecontrol = DENEMO_PRIMARY;
          staff_set_current_primary (Denemo.project->movement);
          return SCM_BOOL_F;
        }
      if ((current->keysig.number != primary->keysig.number) || (current->keysig.isminor != primary->keysig.isminor) || (current->keysig.mode != primary->keysig.mode))
        {
          warningdialog (_("Key Signatures do not match, will not make voice"));
          current->voicecontrol = DENEMO_PRIMARY;
          staff_set_current_primary (Denemo.project->movement);
          return SCM_BOOL_F;
        }

      if (current->clef.type != primary->clef.type)
        {
          warningdialog (_("This voice has a different clef from the staff it will be typeset on. This clef will be used for the display only."));
        }
      ret = SCM_BOOL_T;
      signal_structural_change (Denemo.project);
      draw_score_area ();
      score_status (Denemo.project, TRUE);
      if (!Denemo.non_interactive)
        draw_score (NULL);
    }
  return ret;
}

SCM
scheme_voice_to_staff (SCM optional)
{
  SCM ret = SCM_BOOL_F;
  if (((DenemoStaff *) Denemo.project->movement->currentstaff->data)->voicecontrol & DENEMO_SECONDARY)
    {
      ((DenemoStaff *) Denemo.project->movement->currentstaff->data)->voicecontrol = DENEMO_PRIMARY;
      staff_set_current_primary (Denemo.project->movement);
      ret = SCM_BOOL_T;
      signal_structural_change (Denemo.project);
      score_status (Denemo.project, TRUE);
      draw_score_area ();
    }
  return ret;
}

SCM
scheme_is_voice (void)
{
  return SCM_BOOL ((((DenemoStaff *) Denemo.project->movement->currentstaff->data)->voicecontrol & DENEMO_SECONDARY));
}

/* shifts the note at the cursor by the number of diatonic steps passed in */
SCM
scheme_diatonic_shift (SCM optional)
{
  DenemoProject *gui = Denemo.project;
  DenemoObject *curObj;
  chord *thechord;
  note *thenote;
  if (!Denemo.project || !(Denemo.project->movement) || !(Denemo.project->movement->currentobject) || !(curObj = Denemo.project->movement->currentobject->data) || (curObj->type != CHORD) || !(thechord = (chord *) curObj->object) || !(thechord->notes) || !(thenote = (note *) thechord->notes->data))
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
//     g_debug("note shift %s ie %d\n", str, shift);
          modify_note (thechord, thenote->mid_c_offset + shift, curObj->keysig->accs[offsettonumber (thenote->mid_c_offset + shift)], find_prevailing_clef (Denemo.project->movement));
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
  displayhelper (Denemo.project);
  //done in displayhelper write_status(Denemo.project);
  return SCM_BOOL (TRUE);
}

SCM
scheme_keep_alive (SCM optional)
{
 while (gtk_events_pending ())
  gtk_main_iteration ();
  return SCM_BOOL (TRUE);
}

SCM
scheme_refresh_cache (void)
{
  draw_score (NULL);
  return SCM_BOOL (TRUE);
}



SCM
scheme_set_saved (SCM optional)
{
  if (scm_is_false (optional))
    score_status (Denemo.project, TRUE);
  else
    score_status (Denemo.project, FALSE);
  return SCM_BOOL (TRUE);
}

SCM
scheme_get_saved (SCM optional)
{
  return SCM_BOOL (!Denemo.project->notsaved);
}

SCM
scheme_changecount (SCM count)
{
  if (scm_is_integer (count))
    {
      Denemo.project->changecount = scm_to_int (count);
    }
  return scm_from_int (Denemo.project->changecount);

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
  get_position (Denemo.project->movement, &pos);
  gboolean ret = cursor_to_next_chord ();
  if (!ret)
    goto_movement_staff_obj (NULL, -1, pos.staff, pos.measure, pos.object, pos.leftmeasurenum);
  return SCM_BOOL (ret);
}

SCM
scheme_prev_chord (SCM optional)
{
  DenemoPosition pos;
  get_position (Denemo.project->movement, &pos);
  gboolean ret = cursor_to_prev_chord ();
  if (!ret)
    goto_movement_staff_obj (NULL, -1, pos.staff, pos.measure, pos.object, pos.leftmeasurenum);
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
SCM
scheme_scroll_left (void)
{
  scroll_left ();
  return SCM_BOOL_T;
}

SCM
scheme_scroll_right (void)
{
  scroll_right ();
  return SCM_BOOL_T;
}

void
update_scheme_snippet_ids (void)
{
  DenemoProject *gui = Denemo.project;
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

SCM
scheme_create_snippet_from_object (SCM name)
{
  if (scm_is_string (name))
    {
      char *str;
      str = scm_to_locale_string (name);
      if (Denemo.project->movement->currentobject)
        {
          DenemoObject *clonedobj = dnm_clone_object (Denemo.project->movement->currentobject->data);
          RhythmPattern *r = (RhythmPattern *) g_malloc0 (sizeof (RhythmPattern));
          install_button_for_pattern (r, str);
          r->clipboard = g_list_append (NULL, g_list_append (NULL, clonedobj));
          append_rhythm (r, NULL);
          RhythmElement *relement = (RhythmElement *) g_malloc0 (sizeof (RhythmElement));
          //relement->icon = str; was wrong, must be NULL for a singleton.
          r->name = str;
          r->nickname = g_string_new (str);
          r->rsteps = g_list_append (NULL, relement);
          r->rsteps->prev = r->rsteps->next = r->rsteps;        //make list circular
          SCM ret = scm_from_int (insert_pattern_in_toolbar (r, TRUE));
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

SCM
scheme_select_snippet (SCM number)
{
  if (scm_is_integer (number))
    {
      gint position = scm_to_int (number);
      GList *g = g_list_nth (Denemo.project->rhythms, position - 1);
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

SCM
scheme_insert_snippet (SCM number, SCM select)
{
  if (scm_is_integer (number))
    {
      gint position = scm_to_int (number);
      GList *g = g_list_nth (Denemo.project->rhythms, position - 1);
      if (g)
        {
          RhythmPattern *r = g->data;
          if (r)
            {
              if (scm_is_true (select))
                {
                  select_rhythm_pattern (r);
                  insert_note_following_pattern (Denemo.project);
                }
              else
                {
                  insert_nth_rhythm (position - 1);
                }

              return SCM_BOOL_T;
            }
        }
    }
  return SCM_BOOL_F;
}



SCM
scheme_locate_dotdenemo (SCM optional)
{
  const gchar *dotdenemo = get_user_data_dir (TRUE);
  if (!dotdenemo)
    return SCM_BOOL (FALSE);
  SCM scm = scm_from_locale_string (dotdenemo);
  return scm;
}


SCM
scheme_log_debug (SCM message)
{
  if (scm_is_string (message))
    {
      const gchar *msg = scm_to_locale_string (message);
      g_debug ("%s", msg);
    }
  return SCM_BOOL_T;
}

SCM
scheme_log_info (SCM message)
{
  if (scm_is_string (message))
    {
      const gchar *msg = scm_to_locale_string (message);
      g_info ("%s", msg);
    }
  return SCM_BOOL_T;
}

SCM
scheme_log_message (SCM message)
{
  if (scm_is_string (message))
    {
      const gchar *msg = scm_to_locale_string (message);
      g_message ("%s", msg);
    }
  return SCM_BOOL_T;
}

SCM
scheme_log_warning (SCM message)
{
  if (scm_is_string (message))
    {
      const gchar *msg = scm_to_locale_string (message);
      g_warning ("%s", msg);
    }
  return SCM_BOOL_T;
}

SCM
scheme_log_critical (SCM message)
{
  if (scm_is_string (message))
    {
      const gchar *msg = scm_to_locale_string (message);
      g_critical ("%s", msg);
    }
  return SCM_BOOL_T;
}

SCM
scheme_log_error (SCM message)
{
  if (scm_is_string (message))
    {
      const gchar *msg = scm_to_locale_string (message);
      g_error ("%s", msg);
    }
  return SCM_BOOL_T;
}
