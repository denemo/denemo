#include "core/keyboard.h"
#include "core/kbd-custom.h"
#include "core/view.h"
#include "core/menusystem.h"
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


void
set_visibility_for_action (DenemoAction * action, gboolean visible)
{

  GList *h = denemo_action_get_proxies (action);
  for (; h; h = h->next)
    {
      if (visible)
        gtk_widget_show (h->data);
      else
        gtk_widget_hide (h->data);
    }
  command_row* row = NULL;
  const gchar* name = denemo_action_get_name(action);
  gint id = lookup_command_from_name (Denemo.map, name);
  if(id < 0)
    g_warning("Invalid command name:'%s' id:'%i'", name, id);
  else
  {
    keymap_get_command_row (Denemo.map, &row, id);
    row->hidden = !visible;
    }
}

void
hide_action_of_name (gchar * name)
{
  DenemoAction *action = lookup_action_from_name (name);
 if(action)
    set_visibility_for_action (action, FALSE);
  else g_warning ("Action %s not created yet\n", name);}

void
show_action_of_name (gchar * name)
{
  DenemoAction *action = lookup_action_from_name (name);
  if(action)
    set_visibility_for_action (action, TRUE);
  else g_warning ("Action %s not created yet\n", name);
}

void
add_ui (gchar * menupath, gchar * after, gchar * name)
{
  GtkWidget *widget = denemo_menusystem_get_widget (menupath); 
  if (widget == NULL)
    instantiate_menus (menupath);
  denemo_menusystem_add_command (menupath, name, after);
}

//Called while parsing Default.commands etc
void
create_command(command_row *command)
{
  DenemoAction* action = NULL;
  if (command->script_type == COMMAND_SCHEME)
  {
    gboolean new_command = (g_hash_table_lookup(Denemo.map->idx_from_name, command->name) == NULL);

    if(!Denemo.non_interactive){

      if (!new_command)
        action = lookup_action_from_name (command->name);
      else
      {
        gchar *icon_name = get_icon_for_name (command->name, command->label);
        action = denemo_action_new (command->name, command->label, command->tooltip);
        denemo_action_group_add_action (action);
      }
    }

    if(new_command){
      register_command_row (Denemo.map, command);
      command->callback = activate_script;

      // create a scheme function to call this script
      create_scheme_function_for_script (command->name);
    }

    if(g_list_length(command->locations) > 0)
    {
      GList *g = NULL;
      for (g = command->locations; g; g = g->next)
      {
        command->menupath = (gchar *) (g->data ?: "/MainMenu/Other");
        if(!Denemo.non_interactive)
          add_ui (command->menupath, command->after, command->name);
      }
    }
    else if (command->fallback)
    {
      command->menupath = command->fallback;
      add_ui (command->menupath, command->after, command->name);
    }

#ifdef EXTRA_WORK
    if(!Denemo.non_interactive){
      if (new_command)
        g_signal_connect (G_OBJECT (action), "activate", G_CALLBACK (activate_script), NULL); 
    }
#endif
    // Note the script should *not* be in Default.cmdset
    // to delay loading it, but we should set the signal initally and we should not repeat setting the signal later.
    // the signal does not specify which script will be run, that is decided lazily, when the action is invoked for the first time
  }

  // we are not as yet re-writing tooltips etc on builtin commands
  else { //built-in
    if(!Denemo.non_interactive)
        {
        //action = lookup_action_from_name (command->name);
        
        //register_command (command->name, command->label, command->tooltip, command->callback);
        
        
        if(command->locations)
            {
                //action = lookup_action_from_name (command->name);
             command->menupath = (gchar *) command->locations->data; //only doing one location for now FIXME
             //g_print ("Command %s has %s menupath\n", command->name, command->menupath);
             if(!Denemo.non_interactive)
                    add_ui (command->menupath, NULL, command->name);
                    
                    
                    
                    
            }
            
            register_command_row (Denemo.map, command);
            
            
        //else
            //g_warning ("Command %s has no menupath\n", command->name);
        }
      if (command->hidden && !Denemo.non_interactive)
          {
            hide_action_of_name (command->name);
            command->hidden = FALSE;
            g_info ("Hiding Builtin %s\n", command->name);
          }
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
      //g_debug("got base as %s\n", base);
    }
  g_free (head);

  return base;
}
