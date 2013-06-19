#include "keyboard.h"
#include "kbd-custom.h"
#include "view.h"
#include <stdio.h>
#include <string.h>

/*
 * translate a keybinding from the format used in denemo keymaprc file to the
 * format understood by gtk_accelerator_parse. The output is an allocated string
 * that must be freed by the caller.
 */
gchar *
translate_binding_dnm_to_gtk (const gchar * dnm_binding)
{

  if (!Denemo.prefs.strictshortcuts)
    {
      return g_strdup (dnm_binding);
    }


  /* hold is now "modifiers+keyname" or just "keyname" */
  guint len, i;
  gchar **tokens = g_strsplit (dnm_binding, "+", 0);
  gchar *res, *save;
  len = g_strv_length (tokens);
  if (len == 0)
    res = NULL;
  else if (len == 1)
    res = g_strdup (dnm_binding);
  else
    {
      res = "";
      for (i = 0; i < len - 1; i++)
        {
          save = res;
          res = g_strconcat (res, "<", tokens[i], ">", NULL);
          if (save[0])
            g_free (save);
        }
      save = res;
      res = g_strconcat (res, tokens[len - 1], NULL);
      g_free (save);
    }
  g_strfreev (tokens);
  return res;
}

/*
 * translate a keybinding from the format used in denemo keymaprc file to the
 * format understood by gtk_accelerator_parse. The output is an allocated string
 * that must be freed by the caller.
 */
gchar *
translate_binding_gtk_to_dnm (const gchar * gtk_binding)
{
  if (!Denemo.prefs.strictshortcuts)
    {
      return g_strdup (gtk_binding);
    }


  gchar *res = "", *save, *next, *mod;
  const gchar *cur = gtk_binding;
  while (cur[0] == '<')
    {
      next = strchr (cur, '>');
      if (!next)
        {
          cur = NULL;
          if (res[0])
            g_free (res);
          return NULL;
        }
      mod = g_strndup (cur + 1, next - cur - 1);
      if (res[0])
        {
          save = res;
          res = g_strconcat (res, "+", NULL);
          g_free (save);
        }
      save = res;
      res = g_strconcat (res, mod, NULL);
      g_free (mod);
      if (save[0])
        g_free (save);
      cur = next + 1;
    }
  if (!res[0])
    return g_strdup (gtk_binding);
  else
    {
      save = res;
      res = g_strconcat (res, "+", cur, NULL);
      g_free (save);
      return res;
    }
}
/* UNUSED
static gint
get_state (gchar * key)
{
  gint ret = -1;
  if (0 == strcmp (key, "Ctrl"))
    ret = 4;
  else if (0 == strcmp (key, "Shift"))
    ret = 1;
  else if (0 == strcmp (key, "Alt"))
    ret = 8;
  else if (0 == strcmp (key, "Ctrl+Shift"))
    ret = 5;
  else if (0 == strcmp (key, "Alt+Shift"))
    ret = 9;
  else if (0 == strcmp (key, "Alt+Ctrl"))
    ret = 12;
  else if (0 == strcmp (key, "Alt+Ctrl+Shift"))
    ret = 13;

  return ret;
}
*/
/* add ui elements for menupath if missing */
static void
instantiate_menus (gchar * menupath)
{
  //g_print("Instantiate menus for %s\n", menupath);

  gchar *up1 = g_path_get_dirname (menupath);
  gchar *name = g_path_get_basename (menupath);
  GtkWidget *widget = gtk_ui_manager_get_widget (Denemo.ui_manager, up1);
  if (!strcmp (up1, "/"))
    {
      g_critical ("bad menu path");
      return;
    }
  if (widget == NULL)
    instantiate_menus (up1);
  GList *groups = gtk_ui_manager_get_action_groups (Denemo.ui_manager);
  GtkActionGroup *action_group = GTK_ACTION_GROUP (groups->data);       //FIXME assuming the one we want is first
  if (NULL == gtk_action_group_get_action (action_group, name))
    {
      gchar *tooltip = g_strconcat (_("Menu:\nnamed \""), name, _("\" located at "), menupath, _(" in the menu system"), NULL);
      GtkAction *action = gtk_action_new (name, name, tooltip, NULL);
      g_free (tooltip);
      gtk_action_group_add_action (action_group, action);
      g_object_set_data (G_OBJECT (action), "menupath", up1);
    }
  gtk_ui_manager_add_ui (Denemo.ui_manager, gtk_ui_manager_new_merge_id (Denemo.ui_manager), up1, name, name, GTK_UI_MANAGER_MENU, FALSE);
  //g_print("Adding %s to %s\n", name, up1);
  // widget = gtk_ui_manager_get_widget(Denemo.ui_manager, menupath);
  //show_type (widget, "for menupath widget is ");

}

void
set_visibility_for_action (GtkAction * action, gboolean visible)
{
  if (GTK_IS_ACTION (action))
    {
      GSList *h = gtk_action_get_proxies (action);
      for (; h; h = h->next)
        {
          if (visible)
            gtk_widget_show (h->data);
          else
            gtk_widget_hide (h->data);
        }
      g_object_set_data (G_OBJECT (action), "hidden", GINT_TO_POINTER (!visible));
    }

}

void
hide_action_of_name (gchar * name)
{
  GtkAction *action = lookup_action_from_name (name);
  set_visibility_for_action (action, FALSE);
}

void
show_action_of_name (gchar * name)
{
  GtkAction *action = lookup_action_from_name (name);
  set_visibility_for_action (action, TRUE);
}

void
add_ui (gchar * menupath, gchar * after, gchar * name)
{
  GtkWidget *widget = gtk_ui_manager_get_widget (Denemo.ui_manager, menupath);
  if (widget == NULL)
    {
      instantiate_menus (menupath);
    }
  //We place the item after the "after" item in the menupath, unless that isn't yet
  //installed, in which case we just append to the menu
  gchar *menupath_item = g_build_filename (menupath, after, NULL);
  GtkAction *sibling = gtk_ui_manager_get_action (Denemo.ui_manager, menupath_item);
#ifdef DEBUG
  if ((after != NULL) & (sibling == NULL))
    {
      static gboolean once = TRUE;
      if (once)
        {
          gchar *msg = g_strdup_printf ("Cannot place %s after %s as requested,\nbecause I haven't seen %s yet\nTo fix this delete the %s command save the command set,\nQuit and restart Denemo\nThen re-install %s by right clicking on the %s item\nand choosing More Commands\nAnd finally saving command set again", name, after, after, name, name, after);
          // infodialog(msg);
          g_warning ("%s", msg);
          g_free (msg);
          //once = FALSE;
        }
    }
#endif
  gtk_ui_manager_add_ui (Denemo.ui_manager, gtk_ui_manager_new_merge_id (Denemo.ui_manager), sibling ? menupath_item : menupath, name, name, GTK_UI_MANAGER_AUTO, FALSE);
  g_free (menupath_item);
}

void
create_command(gchar* after,
               gchar* fallback,
               command_row *command)
{
  gchar* menupath = NULL;
  if (command->script_type == COMMAND_SCHEME)
  {
    gboolean new_command = FALSE;
    command->action = lookup_action_from_name (command->name);
    if (command->action == NULL)
    {
      new_command = TRUE;
      gchar *icon_name = get_icon_for_name (command->name, command->label);
      command->action = gtk_action_new (command->name, command->label, command->tooltip, icon_name);
      command->callback = activate_script;

      if (command->hidden)
        g_object_set_data (G_OBJECT (command->action), "hidden", (gpointer) TRUE);
      if (after)
        g_object_set_data (G_OBJECT (command->action), "after", (gpointer) after);
      register_command_row (Denemo.map, command);
      GtkActionGroup *action_group;
      // GList *groups = gtk_ui_manager_get_action_groups (Denemo.ui_manager);
      action_group = Denemo.action_group;
      gtk_action_group_add_action (action_group, command->action);
      // create a scheme function to call this script
      create_scheme_function_for_script (command->name);
    }

    if(g_list_length(command->locations) > 0)
    {
      GList *g = NULL;
      for (g = command->locations; g; g = g->next)
      {
        menupath = (gchar *) g->data;
        menupath = menupath ? menupath : (gchar *) "/MainMenu/Other";
        add_ui (menupath, after, command->name);
      }
    }
    else if (fallback)
    {           
      /* no path given, use fallback */
      menupath = fallback;
      add_ui (menupath, after, command->name);
    }

    g_object_set_data (G_OBJECT (command->action), "menupath", menupath);

    if (new_command)
      g_signal_connect (G_OBJECT (command->action), "activate", G_CALLBACK (activate_script), NULL);

    // Note the script should *not* be in Default.cmdset
    // to delay loading it, but we should set the signal initally and we should not repeat setting the signal later.
    // the signal does not specify which script will be run, that is decided lazily, when the action is invoked for the first time

    if (command->hidden)
      g_object_set_data (G_OBJECT (command->action), "deleted", (gpointer) TRUE);      //Mark hidden items as deleted on loading them
  }
  
  // we are not as yet re-writing tooltips etc on builtin commands
  else if (command->hidden)
  {
    hide_action_of_name (command->name);
    command->hidden = FALSE;
    g_print ("Hiding Builtin %s\n", command->name);
  }
}

/* if filename ends in /menus/.... hierarchy extract and return the tail
   below menus/
*/
gchar *
extract_menupath (gchar * filename)
{
  gchar *head = g_strdup_printf ("%c%s", G_DIR_SEPARATOR, "menus");
  gchar *base = g_strrstr (filename, head);
  if (base)
    {
      base += strlen (head);
      base = g_path_get_dirname (base);
      gchar *c;
      for (c = base; c && *c; c++)
        if (*c == G_DIR_SEPARATOR)
          *c = '/';
      //g_print("got base as %s\n", base);
    }
  g_free (head);

  return base;
}
