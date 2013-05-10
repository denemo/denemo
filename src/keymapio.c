#include "keymapio.h"


static void
parseScripts (xmlDocPtr doc, xmlNodePtr cur, gchar * fallback, gint merge)
{
  xmlChar *name = NULL, *menupath = NULL, *label = NULL, *tooltip = NULL, *scheme = NULL, *after = NULL;
  GList *menupaths = NULL;
  cur = cur->xmlChildrenNode;
  gboolean is_script = FALSE;
  gboolean hidden = FALSE;

  for (; cur; cur = cur->next)
    {
      if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_ACTION))
        {
          if (cur->xmlChildrenNode == NULL)
            {
              g_warning ("Empty action node found in keymap file\n");
            }
          else
            {
              name = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
              menupaths = NULL; //We allow multiple menupaths for a given action, all are added to the gtk_ui when this command is processed after the tooltip node. This is very bad xml, as the action should have all the others as children, and not depend on the order.FIXME
#if 0
              g_print ("Action %s\n", (gchar *) name);
#endif
            }
        }
      else if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_SCHEME))
        {
          scheme = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
          is_script = TRUE;
        }
      else if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_HIDDEN))
        {
          hidden = TRUE;
        }

      else if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_MENUPATH))
        {
          menupath = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
          menupaths = g_list_append (menupaths, menupath);
        }
      else if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_LABEL))
        {
          label = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
        }
      else if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_AFTER))
        {
          after = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
        }
      else if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_TOOLTIP))
        {
          tooltip = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
          create_command(is_script, name, label, scheme, tooltip, hidden, after, menupath, fallback, menupaths, merge);

        }                       // tooltip found, assumed last field
    }                           // for all nodes
  //alphabeticalize_commands(Denemo.map);



}                               /* end of parseScripts */


static void
parseBindings (xmlDocPtr doc, xmlNodePtr cur, keymap * the_keymap)
{

  xmlChar *name;                //keyval variables
  gint command_number = -1;
  guint keyval = 0;
  GdkModifierType state = 0;
  name = 0;                     //defend against corrupt files.
  for (cur = cur->xmlChildrenNode; cur != NULL; cur = cur->next)
    {
      if (0 == xmlStrcmp (cur->name, BINDINGXML_TAG_ACTION))
        {
          if (cur->xmlChildrenNode == NULL)
            {
              g_warning ("Empty children node found in keymap file\n");
            }
          else
            {
              name = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
              show_action_of_name (name);
#ifdef DEBUG
              g_print ("Action %s\n", (gchar *) name);
#endif         /*DEBUG*/
            }
        }
      else if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_HIDDEN))
        {
          if (name)
            hide_action_of_name (name);

        }
      else if (0 == xmlStrcmp (cur->name, BINDINGXML_TAG_BIND))
        {
          if (name)
            command_number = lookup_command_from_name (the_keymap, (gchar *) name);
          //g_print("Found bind node for action %s %d\n", name, command_number);
          if (cur->xmlChildrenNode == NULL)
            {
              g_warning ("Empty <bind><\\bind> found in commandset file\n");
            }
          else
            {
              xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
              if (name && tmp)
                {
                  gchar *gtk_binding = translate_binding_dnm_to_gtk ((gchar *) tmp);
                  //g_print("gtk_binding is %s\n", gtk_binding);
                  if (gtk_binding)
                    {
                      keyval = 0;
                      if (Denemo.prefs.strictshortcuts)
                        dnm_accelerator_parse (gtk_binding, &keyval, &state);
#ifdef DEBUG
                      g_print ("binding %s, keyval %d, state %d, Command Number %d\n", gtk_binding, keyval, state, command_number);
#endif
                      {
                        gchar *comma;
                        comma = strtok (gtk_binding, ",");
                        if (comma)      //two key binding, remove any single keybinding
                          {
                            if (-1 != lookup_command_for_keybinding_name (the_keymap, comma))
                              remove_keybinding_from_name (the_keymap, comma);
                            *(comma + strlen (comma)) = ',';
                          }
                      }
                      if (command_number != -1)
                        {
                          if (keyval)
                            add_keybinding_to_idx (the_keymap, keyval, state, command_number, POS_LAST);
                          else
                            add_named_binding_to_idx (the_keymap, tmp, command_number, POS_LAST);
                        }
                      g_free (gtk_binding);
                    }
                  else
                    {
                      g_print ("No gtk equivalent for shortcut %s\n", tmp);
                    }
                  xmlFree (tmp);
                }
            }
        }

    }
}                               // parseBindings

static void
parseCursorBinding (xmlDocPtr doc, xmlNodePtr cur)
{
  gint state, cursor_num;
  xmlChar *tmp;
  for (cur = cur->xmlChildrenNode; cur != NULL; cur = cur->next)
    {
      if (0 == xmlStrcmp (cur->name, BINDINGXML_TAG_STATE))
        {
          tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
          if (tmp)
            {
              sscanf (tmp, "%x", &state);       // = atoi(tmp);
              xmlFree (tmp);
            }

        }
      else if (0 == xmlStrcmp (cur->name, BINDINGXML_TAG_CURSOR))
        {
          tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
          if (tmp)
            {
              cursor_num = atoi (tmp);
              xmlFree (tmp);
            }
          assign_cursor (state, cursor_num);
          // g_print("type is %s\n",g_type_name(G_TYPE_FROM_INSTANCE(Denemo.window->window)));
          // set_cursor_for(state);
        }
    }
}


static void
parseCursors (xmlDocPtr doc, xmlNodePtr cur)
{
  for (cur = cur->xmlChildrenNode; cur != NULL; cur = cur->next)
    {
      if (0 == xmlStrcmp (cur->name, BINDINGXML_TAG_CURSORBINDING))
        {
          parseCursorBinding (doc, cur);
        }
    }
}



static void
parseCommands (xmlDocPtr doc, xmlNodePtr cur, keymap * the_keymap, gchar * menupath, gint merge)
{
  xmlNodePtr ncur = cur->xmlChildrenNode;
  int i;
  static commands_installed = FALSE;

  if (!commands_installed)
    {
      install_commands ();
      commands_installed = TRUE;
    }

  for (i = 0; i < 2; i++)
    {                           //Two passes, as all Commands have to be added before bindings
      for (ncur = cur->xmlChildrenNode; ncur; ncur = ncur->next)
        {
          if ((0 == xmlStrcmp (ncur->name, COMMANDXML_TAG_ROW)))
            {
              i ? parseBindings (doc, ncur, the_keymap) : parseScripts (doc, ncur, menupath, merge);
            }
          else if (i && (0 == xmlStrcmp (ncur->name, COMMANDXML_TAG_CURSORS)))
            {
              parseCursors (doc, ncur);
            }
        }
    }
}

static void
parseKeymap (xmlDocPtr doc, xmlNodePtr cur, keymap * the_keymap, gchar * menupath, gint merge)
{
  for (cur = cur->xmlChildrenNode; cur != NULL; cur = cur->next)
    {
      if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_MAP))
        {
          parseCommands (doc, cur, the_keymap, menupath, merge);
        }
    }
}


/* 
 * load a command from filename. 
 * if not scripted and merging with other commands, tell the user where the new command is. 
 * If an action exists but has an empty script load the scheme script,
 if action is a new action leave script as empty string for loading on demand.
 * Create a widget for the action in a menupath found in filename or failing that deduced from
 * the path to filename itself (starting from actions/menus).
 * returns 0 on success
 * negative on failure
 */
gint
load_xml_keymap (gchar * filename, gboolean interactive)
{
  gint ret = -1;
  xmlDocPtr doc;
  //xmlNsPtr ns;
  xmlNodePtr rootElem;
  if (filename == NULL)
    return ret;
  if (g_file_test (filename, G_FILE_TEST_IS_DIR))
    {
      warningdialog ("There is no support for loading whole folders of commands yet, sorry");
      return ret;
    }
  if (!g_file_test (filename, G_FILE_TEST_EXISTS))
    return ret;
  doc = xmlParseFile (filename);
  gchar *menupath = extract_menupath (filename);
  if (doc == NULL)
    {
      g_debug ("Could not read XML file %s", filename);
      return ret;
    }

  rootElem = xmlDocGetRootElement (doc);
  if (rootElem == NULL)
    {
      g_warning ("Empty Document\n");
      xmlFreeDoc (doc);
      return ret;
    }
  //g_print ("RootElem %s\n", rootElem->name);
  if (xmlStrcmp (rootElem->name, COMMANDXML_TAG_ROOT))
    {
      g_warning ("Document has wrong type\n");
      xmlFreeDoc (doc);
      return ret;
    }
  rootElem = rootElem->xmlChildrenNode;


  while (rootElem != NULL)
    {
#ifdef DEBUG
      g_print ("RootElem %s\n", rootElem->name);
#endif


      parseKeymap (doc, rootElem, Denemo.map, menupath, interactive);

      if (Denemo.last_merged_command)
        g_free (Denemo.last_merged_command);
      Denemo.last_merged_command = g_strdup (filename);
      if (menupath)
        execute_init_scripts (menupath);

      update_all_labels (Denemo.map);
      ret = 0;

      rootElem = rootElem->next;
    }

  xmlFreeDoc (doc);

  {                             //if this is a new-style .commands file, we need to load the keybindings separately
    gchar *name = g_strdup (filename);
    gchar *ext = remove_extension (name);
    if (ext && !strcmp (ext, "commands"))
      {
        gchar *newname = g_strdup_printf ("%s%s", name, ".shortcuts");
        load_xml_keybindings (newname);
        g_free (newname);
      }
    g_free (name);
  }
  return ret;
}





gint
load_xml_keybindings (gchar * filename)
{
  gint ret = -1;
  xmlDocPtr doc;
  //xmlNsPtr ns;
  xmlNodePtr rootElem;
  if (filename == NULL)
    return ret;
  if (!g_file_test (filename, G_FILE_TEST_EXISTS))
    return ret;
  doc = xmlParseFile (filename);
  if (doc == NULL)
    {
      g_debug ("Could not read XML file %s", filename);
      return ret;
    }
  rootElem = xmlDocGetRootElement (doc);
  if (rootElem == NULL)
    {
      g_warning ("Empty Document\n");
      xmlFreeDoc (doc);
      return ret;
    }
  //g_print ("RootElem %s\n", rootElem->name);
  if (xmlStrcmp (rootElem->name, COMMANDXML_TAG_ROOT))
    {
      g_warning ("Document has wrong type\n");
      xmlFreeDoc (doc);
      return ret;
    }
  rootElem = rootElem->xmlChildrenNode;

  while (rootElem != NULL)
    {
      if ((0 == xmlStrcmp (rootElem->name, COMMANDXML_TAG_MERGE)))
        {
          xmlNodePtr cur;
          for (cur = rootElem->xmlChildrenNode; cur != NULL; cur = cur->next)
            {
              if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_MAP))
                {
                  xmlNodePtr ncur;
                  for (ncur = cur->xmlChildrenNode; ncur != NULL; ncur = ncur->next)
                    {
                      parseBindings (doc, ncur, Denemo.map);
                    }
                  ret = 0;
                }
            }
          update_all_labels (Denemo.map);
        }
      rootElem = rootElem->next;
    }
  xmlFreeDoc (doc);
  return ret;
}


static void
write_xml_keybinding_info (gchar * kb_name, xmlNodePtr node)
{
  gchar *dnm_binding = translate_binding_gtk_to_dnm (kb_name);
#ifdef DEBUG
  g_print ("binding is : (dnm) %s, (gtk) %s \n", dnm_binding, kb_name);
#endif
  if (!(Denemo.prefs.return_key_is_special && !strcmp (dnm_binding, N_("Return"))))
    xmlNewTextChild (node, NULL, (xmlChar *) "bind", (xmlChar *) dnm_binding);
  g_free (dnm_binding);
}

static void
output_pointer_shortcut (gint * state, GdkCursor * cursor, xmlNodePtr parent)
{
  gchar *statestr = g_strdup_printf ("%x", *state);
#if GTK_MAJOR_VERSION==3
  gint cursor_num = gdk_cursor_get_cursor_type (cursor);
#else
  gint cursor_num = cursor->type;
#endif
  gchar *numstr = g_strdup_printf ("%d", cursor_num);
  xmlNodePtr child = xmlNewTextChild (parent, NULL, (xmlChar *) "cursor-binding", NULL);
  xmlNewChild (child, NULL, "state", statestr);
  xmlNewChild (child, NULL, "cursor", numstr);
  g_free (statestr);
  g_free (numstr);
}

gint
save_xml_keymap (gchar * filename)      //_!!! create a DEV version here, saving C-code to create the actions at runtime with translatable tooltips.
{
  keymap *the_keymap = Denemo.map;
  gint i, ret = -1;
  xmlDocPtr doc;
  //xmlNsPtr ns;
  xmlNodePtr parent, child;

  doc = xmlNewDoc ((xmlChar *) "1.0");
  doc->xmlRootNode = parent = xmlNewDocNode (doc, NULL, COMMANDXML_TAG_ROOT, NULL);


  child = xmlNewChild (parent, NULL, COMMANDXML_TAG_MERGE, NULL);

  xmlNewTextChild (child, NULL, COMMANDXML_TAG_TITLE, (xmlChar *) "A Denemo Command Set");
  xmlNewTextChild (child, NULL, COMMANDXML_TAG_AUTHOR, (xmlChar *) "AT, JRR, RTS");

  parent = xmlNewChild (child, NULL, COMMANDXML_TAG_MAP, NULL);

  child = xmlNewChild (parent, NULL, COMMANDXML_TAG_CURSORS, NULL);

  g_hash_table_foreach (Denemo.map->cursors, (GHFunc) output_pointer_shortcut, child);


  for (i = 0; i < keymap_size (the_keymap); i++)
    {


      gpointer action = (gpointer) lookup_action_from_idx (the_keymap, i);
      gchar *scheme = action ? g_object_get_data (action, "scheme") : NULL;
      gchar *after = action ? g_object_get_data (action, "after") : NULL;
      gboolean deleted = (gboolean) (action ? GPOINTER_TO_INT (g_object_get_data (action, "deleted")) : 0);
      //  if(deleted && scheme)
      //        continue;

      child = xmlNewChild (parent, NULL, COMMANDXML_TAG_ROW, NULL);


      gchar *name = (gchar *) lookup_name_from_idx (the_keymap, i);
#ifdef DEBUG
      g_print ("%s \n", name);
#endif
      xmlNewTextChild (child, NULL, COMMANDXML_TAG_ACTION, (xmlChar *) name);
      if (after)
        xmlNewTextChild (child, NULL, COMMANDXML_TAG_AFTER, (xmlChar *) after);
      if (deleted)              //store as hidden in commands file
        xmlNewTextChild (child, NULL, COMMANDXML_TAG_HIDDEN, "true");

      if (scheme)
        xmlNewTextChild (child, NULL, COMMANDXML_TAG_SCHEME,
                         /* (xmlChar *) scheme */ (xmlChar *) "");

      gchar *menupath = action ? g_object_get_data (action, "menupath") : NULL;
      if (menupath)
        xmlNewTextChild (child, NULL, COMMANDXML_TAG_MENUPATH, (xmlChar *) menupath);

      gchar *label = (gchar *) lookup_label_from_idx (the_keymap, i);
      if (label)
        xmlNewTextChild (child, NULL, COMMANDXML_TAG_LABEL, (xmlChar *) label);


      gchar *tooltip = (gchar *) lookup_tooltip_from_idx (the_keymap, i);
      if (tooltip)
        xmlNewTextChild (child, NULL, COMMANDXML_TAG_TOOLTIP, (xmlChar *) tooltip);

      //  keymap_foreach_command_binding (the_keymap, i,
      //                                      (GFunc) write_xml_keybinding_info, child);

    }

  xmlSaveFormatFile (filename, doc, 1);

  xmlFreeDoc (doc);
  return ret;
}

gint
save_xml_keybindings (gchar * filename)
{
  keymap *the_keymap = Denemo.map;
  gint i, ret = -1;
  xmlDocPtr doc;
  //xmlNsPtr ns;
  xmlNodePtr parent, child;

  doc = xmlNewDoc ((xmlChar *) "1.0");
  doc->xmlRootNode = parent = xmlNewDocNode (doc, NULL, COMMANDXML_TAG_ROOT, NULL);
  child = xmlNewChild (parent, NULL, COMMANDXML_TAG_MERGE, NULL);

  xmlNewTextChild (child, NULL, COMMANDXML_TAG_TITLE, (xmlChar *) "A Denemo Command Set");
  xmlNewTextChild (child, NULL, COMMANDXML_TAG_AUTHOR, (xmlChar *) "AT, JRR, RTS");

  parent = xmlNewChild (child, NULL, COMMANDXML_TAG_MAP, NULL);

  child = xmlNewChild (parent, NULL, COMMANDXML_TAG_CURSORS, NULL);

  g_hash_table_foreach (Denemo.map->cursors, (GHFunc) output_pointer_shortcut, child);


  for (i = 0; i < keymap_size (the_keymap); i++)
    {
      gpointer action = (gpointer) lookup_action_from_idx (the_keymap, i);
      gchar *scheme = action ? g_object_get_data (action, "scheme") : NULL;
      gboolean deleted = (gboolean) (action ? GPOINTER_TO_INT (g_object_get_data (action, "deleted")) : 0);
      gboolean hidden = (gboolean) (action ? GPOINTER_TO_INT (g_object_get_data (action, "hidden")) : 0);
      if (deleted && scheme)
        continue;
      if (hidden || command_has_binding (i))
        {
          child = xmlNewChild (parent, NULL, COMMANDXML_TAG_ROW, NULL);

          gchar *name = (gchar *) lookup_name_from_idx (the_keymap, i);
#ifdef DEBUG
          g_print ("%s %s binding(s) \n", name, command_has_binding (i) ? "has" : "does not have");
#endif
          xmlNewTextChild (child, NULL, COMMANDXML_TAG_ACTION, (xmlChar *) name);
          if (hidden)
            xmlNewTextChild (child, NULL, COMMANDXML_TAG_HIDDEN, "true");

          keymap_foreach_command_binding (the_keymap, i, (GFunc) write_xml_keybinding_info, child);
        }
    }

  xmlSaveFormatFile (filename, doc, 1);

  xmlFreeDoc (doc);
  return ret;
}

static void
show_type (GtkWidget * widget, gchar * message)
{
  g_print ("%s%s\n", message, widget ? g_type_name (G_TYPE_FROM_INSTANCE (widget)) : "NULL widget");
}

/* not used */
/*
static gint
create_dir_for_menu (gchar * str)
{
  gchar *thismenu = g_build_filename (locatedotdenemo (), "actions", "menus", str, NULL);
  if (!g_file_test (thismenu, G_FILE_TEST_IS_DIR))
    {
      return g_mkdir_with_parents (thismenu, 0770);
    }
  return 0;
}
*/

/* parse an entry in denemoui.xml recursively
   set menupath as attribute of widgets
 */
static gint
parseMenu (xmlNodePtr rootElem, gchar * path, DenemoGUI * gui)
{
  for (rootElem = rootElem->xmlChildrenNode; rootElem; rootElem = rootElem->next)
    {
      if (0 == xmlStrcmp (rootElem->name, MENUXML_TAG_MENUBAR) || 
          0 == xmlStrcmp (rootElem->name, MENUXML_TAG_MENU) || 
          0 == xmlStrcmp (rootElem->name, MENUXML_TAG_TOOLBAR))
        /* ignoring popup menus */
        {
          gchar *name = (gchar *) xmlGetProp (rootElem, MENUXML_PROP_NAME);
          if (name == NULL)
            name = (gchar *) xmlGetProp (rootElem, MENUXML_PROP_ACTION);

          if (name)
            {
              gchar *str = g_strdup_printf ("%s%s%s", path, "/", name);
              GtkWidget *widget = gtk_ui_manager_get_widget (Denemo.ui_manager, str);
              if (widget)
                {
                  g_object_set_data (G_OBJECT (widget), "menupath", str);
                  // we do this in menu_click when needed create_dir_for_menu(str);//FIXME we only need do this once for a given denemoui.xml
                  //show_type(widget, "The type is ");
                  //g_print("set %p %s\n",widget, str);
                  parseMenu (rootElem, str, gui);
                }
              else
                g_warning ("no object for %s\n", str);
            }

        }
      if (0 == xmlStrcmp (rootElem->name, MENUXML_TAG_MENUITEM))
        {
          gchar *name = (gchar *) xmlGetProp (rootElem, MENUXML_PROP_ACTION);
          if (name)
            {
              gchar *str = g_strdup_printf ("%s%s%s", path, "/", name);
              GtkWidget *widget = gtk_ui_manager_get_widget (Denemo.ui_manager, str);
              g_free (str);
              //show_type(widget, "The type is ");
              //g_print("set %p %s\n",widget, path);
              if (widget)
                {
                  g_object_set_data (G_OBJECT (widget), "menupath", g_strdup (path));
                }
            }
          g_free (name);
        }
    }
  return 0;
}

/* 
 * attaches the menu hierarchy path to each menuitem widget in the passed file (denemoui.xml)
 * returns 0 on success
 * negative on failure
 */
gint
parse_paths (gchar * filename, DenemoGUI * gui)
{
  gint ret = -1;
  xmlDocPtr doc;
  //xmlNsPtr ns;
  xmlNodePtr rootElem;
  if (filename == NULL)
    return ret;
  doc = xmlParseFile (filename);

  if (doc == NULL)
    {
      g_warning ("Could not read XML file %s", filename);
      return ret;
    }

  rootElem = xmlDocGetRootElement (doc);
  if (rootElem == NULL)
    {
      g_warning ("Empty Document\n");
      xmlFreeDoc (doc);
      return ret;
    }
  //g_print ("RootElem %s\n", rootElem->name);
  if (xmlStrcmp (rootElem->name, (const xmlChar *) "ui"))
    {
      g_warning ("Document has wrong type\n");
      xmlFreeDoc (doc);
      return ret;
    }
  gchar *path = "";
  parseMenu (rootElem, path, gui);
  ret = 0;
  xmlFreeDoc (doc);
  return ret;
}



gint
save_script_as_xml (gchar * filename, gchar * myname, gchar * myscheme, gchar * mylabel, gchar * mytooltip, gchar * after)
{
  xmlDocPtr doc;
  //xmlNsPtr ns;
  xmlNodePtr parent, child;

  doc = xmlNewDoc ((xmlChar *) "1.0");
  doc->xmlRootNode = parent = xmlNewDocNode (doc, NULL, COMMANDXML_TAG_ROOT, NULL);
  child = xmlNewChild (parent, NULL, COMMANDXML_TAG_MERGE, NULL);

  xmlNewTextChild (child, NULL, COMMANDXML_TAG_TITLE, (xmlChar *) "A Denemo Keymap");
  xmlNewTextChild (child, NULL, COMMANDXML_TAG_AUTHOR, (xmlChar *) "AT, JRR, RTS");

  parent = xmlNewChild (child, NULL, COMMANDXML_TAG_MAP, NULL);

  child = xmlNewChild (parent, NULL, COMMANDXML_TAG_ROW, NULL);
  if (after)
    xmlNewTextChild (child, NULL, COMMANDXML_TAG_AFTER, (xmlChar *) after);

  xmlNewTextChild (child, NULL, COMMANDXML_TAG_ACTION, (xmlChar *) myname);

  xmlNewTextChild (child, NULL, COMMANDXML_TAG_SCHEME, (xmlChar *) myscheme);

  xmlNewTextChild (child, NULL, COMMANDXML_TAG_LABEL, (xmlChar *) mylabel);


  xmlNewTextChild (child, NULL, COMMANDXML_TAG_TOOLTIP, (xmlChar *) mytooltip);
  xmlSaveFormatFile (filename, doc, 1);

  xmlFreeDoc (doc);
  return 0;
}