#include "core/keymapio.h"
#include "core/kbd-custom.h"
#include "core/view.h"
#include "ui/mousing.h"

static gchar*
find_command_dir(gint idx, gchar* filename)
{
  command_row* row = NULL;
  keymap_get_command_row(Denemo.map, &row, idx);

 // if(!row->menupath)
 //   g_print("Command %s has no menupath", filename);

  GList* dirs = NULL;
  dirs = g_list_append(dirs, g_build_filename (PACKAGE_SOURCE_DIR, COMMANDS_DIR, "menus", row->menupath, NULL));
  dirs = g_list_append(dirs, g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "menus", row->menupath, NULL));
  dirs = g_list_append(dirs, g_build_filename (get_user_data_dir (TRUE), "download", COMMANDS_DIR, "menus", row->menupath, NULL));
  dirs = g_list_append(dirs, g_build_filename (get_system_data_dir (), COMMANDS_DIR, "menus", row->menupath, NULL));

  return find_dir_for_file (filename, dirs);
}

static int
get_command_type(xmlChar* type)
{
  return 0 == xmlStrcmp (type, COMMAND_TYPE_SCHEME) ? COMMAND_SCHEME : COMMAND_BUILTIN;
}

static void
parseScripts (xmlDocPtr doc, xmlNodePtr cur, gchar * fallback)
{
  command_row* command = NULL;
  xmlChar *type = NULL;
  xmlNodePtr head = cur;
  type = xmlGetProp(cur, COMMANDXML_TAG_TYPE);
  //this first pass finds the <action> of this <row> and does get_or_create_command() on it, then for scheme ones it runs get_command()
    for (cur = cur->xmlChildrenNode; cur; cur = cur->next)
    {
      if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_ACTION))
        {
          if (cur->xmlChildrenNode == NULL)
            {
              g_warning ("Empty action node found in keymap file");
            }
          else
            {
              // We allow multiple locations for a given action, all are added to the gtk_ui when this command is processed after the tooltip node.
              // This is very bad xml, as the action should have all the others as children, and not depend on the order.FIXME
              gchar* name = (gchar*) xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
              command = get_or_create_command(name); //g_print ("in parseScripts called get_or_create_command row with action name %s\n", command->name);
              command->fallback = fallback;
              command->locations = NULL;

              if(type && 0 == xmlStrcmp (type, COMMAND_TYPE_SCHEME))
                command->script_type = get_command_type(type);
               
            }
        }
    }
//second pass gets the other fields in this <row> and fills in command with them
  cur = head;
  for (cur = cur->xmlChildrenNode; cur; cur = cur->next)
    {
    if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_HIDDEN))
        {
          command->hidden = TRUE;
        }
      else if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_MENUPATH))
        {
          command->locations = g_list_append (command->locations, xmlNodeListGetString (doc, cur->xmlChildrenNode, 1));
        }
      else if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_LABEL))
        {
          command->label = _((gchar*) xmlNodeListGetString (doc, cur->xmlChildrenNode, 1));
        }
      else if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_AFTER))
        {
          command->after = (gchar*) xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
        }
      else if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_TOOLTIP))
        {
          command->tooltip = _((gchar*) xmlNodeListGetString (doc, cur->xmlChildrenNode, 1));
        }
    }
  create_command(command);//g_print ("calling create_command for %s path %s\n", command->name, command->menupath);
  xmlFree(type);
}

static void
parseBindings (xmlDocPtr doc, xmlNodePtr cur, keymap * the_keymap)
{

  xmlChar *name = NULL;                //keyval variables
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
              g_warning ("Empty children node found in keymap file");
            }
          else
            {
              name = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
              show_action_of_name ((gchar*) name);
            }
        }
      else if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_HIDDEN))
        {
          if (name)
            hide_action_of_name ((gchar*) name);

        }
      else if (0 == xmlStrcmp (cur->name, BINDINGXML_TAG_BIND))
        {
          if (name)
            command_number = lookup_command_from_name (the_keymap, (gchar *) name);
          //g_print("Found bind node for action %s %d\n", name, command_number);
          if (cur->xmlChildrenNode == NULL)
            {
              g_warning ("Empty <bind><\\bind> found in commandset file");
            }
          else
            {
              xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
              if (name && tmp)
                {
                  gchar *gtk_binding = translate_binding_dnm_to_gtk ((gchar *) tmp);
                  //g_debug("gtk_binding is %s\n", gtk_binding);
                  if (gtk_binding)
                    {
                      keyval = 0;
                      if (Denemo.prefs.strictshortcuts)
                        dnm_accelerator_parse (gtk_binding, &keyval, &state);
                      //g_debug ("binding %s, keyval %d, state %d, Command Number %d", gtk_binding, keyval, state, command_number);
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
                            add_named_binding_to_idx (the_keymap, (gchar*) tmp, command_number, POS_LAST);
                        }
                      g_free (gtk_binding);
                    }
                  else
                    {
                      g_warning ("No gtk equivalent for shortcut %s", tmp);
                    }
                  xmlFree (tmp);
                }
            }
        }
    }
}

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
              sscanf ((char*) tmp, "%x", &state);       // = atoi(tmp);
              xmlFree (tmp);
            }

        }
      else if (0 == xmlStrcmp (cur->name, BINDINGXML_TAG_CURSOR))
        {
          tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
          if (tmp)
            {
              cursor_num = atoi ((char*) tmp);
              xmlFree (tmp);
            }
          assign_cursor (state, cursor_num);
          //g_debug("type is %s\n",g_type_name(G_TYPE_FROM_INSTANCE(Denemo.window->window)));
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
static xmlDocPtr docx;
static int compare_nodes (xmlNodePtr *a, xmlNodePtr *b)
{
    xmlNodePtr ptr1, ptr2;
    char *type1="", *menupath1="", *label1="";
    char *type2="", *menupath2="", *label2="";
    
    type1 = xmlGetProp( (*a), COMMANDXML_TAG_TYPE);
   // g_print ("Found type1 %s for %p vs %p\n", type1, *a, *b);
    if ((!type1) && (!type2)) return 0;
     if (!type1) return -1;
     
    if (!type2) return 1;
    
    for (ptr1 = (*a)->xmlChildrenNode;ptr1;ptr1 = ptr1->next)
        {
        if (0 == xmlStrcmp (ptr1->name, COMMANDXML_TAG_MENUPATH))
            menupath1 =  xmlNodeListGetString (docx, ptr1->xmlChildrenNode, 1);
          else if (0 == xmlStrcmp (ptr1->name, COMMANDXML_TAG_LABEL))
             label1 =  xmlNodeListGetString (docx, ptr1->xmlChildrenNode, 1);
         }
    type2 = xmlGetProp((*b), COMMANDXML_TAG_TYPE);
    for (ptr2 = (*b)->xmlChildrenNode;ptr2;ptr2 = ptr2->next)
        {
          if (0 == xmlStrcmp (ptr2->name, COMMANDXML_TAG_MENUPATH))
            menupath2 =  xmlNodeListGetString (docx, ptr2->xmlChildrenNode, 1);
          else if (0 == xmlStrcmp (ptr2->name, COMMANDXML_TAG_LABEL))
             label2 =  xmlNodeListGetString (docx, ptr2->xmlChildrenNode, 1);
         }    
    //g_print (" |%s| |%s| |%s| vs |%s| |%s| |%s|\n", type1, menupath1, label1, type2, menupath2, label2);
  //  if (!strcmp (type1, type2))
   //     {
            if (!strcmp (menupath1, menupath2))
                return strcmp (label1, label2);
            else
                return strcmp (menupath1, menupath2); //the other way round they are in reverse, but come before menu items, this way they are in order but come after menu items!!!
            
  //     }
 //   else
  //      {
  //          if (!strcmp (type1, "scheme"))
   //             return 1;
   //         else
   //             return -1;
            
  //      }
}
static void
parseCommands (xmlDocPtr doc, xmlNodePtr cur, keymap * the_keymap, gchar * menupath)
{
    docx = doc;
  xmlNodePtr ncur;
  
#ifdef DEVELOPER
//HERE WE CAN qsort the ncur->children by the menupath tag they hold  
  // make an array of them first.
   gint i=0, num_nodes;
   xmlNodePtr ptr=cur->xmlChildrenNode;
   while (ptr = ptr->next) i++;
   //g_print ("Number of <key> entries %d\n", i);
   num_nodes = i;
   xmlNodePtr *array = g_malloc (sizeof (xmlNodePtr) * num_nodes);
   //g_print ("first %s %s\n", cur->xmlChildrenNode->name, xmlGetProp( (cur->xmlChildrenNode), COMMANDXML_TAG_TYPE)); 
   for (i=0, ptr=cur->xmlChildrenNode;i<num_nodes;i++, ptr = ptr->next)
    array[i] = ptr; 
    //g_print ("last written index %d\n", i-1);
   qsort (array, num_nodes, sizeof (xmlNodePtr), (__compar_fn_t)compare_nodes);
   
   //this won't work - you have to take each xmlNodePtr from the array and set its next field to the next.
   
   for (i=0,  cur->xmlChildrenNode = array[0];i<num_nodes-1;i++)
    array[i]->next = array[i+1];
    array[num_nodes-1]->next = NULL;
   //g_print ("last read index %d\n", i-1);
#endif //DEVELOPER


   
  //Parse commands first
  for (ncur = cur->xmlChildrenNode; ncur; ncur = ncur->next)
    {
      if ((0 == xmlStrcmp (ncur->name, COMMANDXML_TAG_ROW)))
        {
          parseScripts (doc, ncur, menupath);
        }
    }

  //Then parse bindings
  if(!Denemo.non_interactive){
    for (ncur = cur->xmlChildrenNode; ncur; ncur = ncur->next)
      {
        if ((0 == xmlStrcmp (ncur->name, COMMANDXML_TAG_ROW)))
          {
            parseBindings (doc, ncur, the_keymap);
          }
        else if (0 == xmlStrcmp (ncur->name, COMMANDXML_TAG_CURSORS))
          {
            parseCursors (doc, ncur);
          }
      }
  }
}

static void
parseKeymap (xmlDocPtr doc, xmlNodePtr cur, keymap * the_keymap, gchar * menupath)
{
  for (cur = cur->xmlChildrenNode; cur != NULL; cur = cur->next)
    {
      if (0 == xmlStrcmp (cur->name, COMMANDXML_TAG_MAP))
        {
          parseCommands (doc, cur, the_keymap, menupath);
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
load_xml_keymap (gchar * filename)
{
  gint ret = -1;
  xmlDocPtr doc;
  xmlNodePtr rootElem;
  xmlKeepBlanksDefault (0);

  if (filename == NULL)
    return ret;

  if (!g_file_test (filename, G_FILE_TEST_EXISTS))
    return ret;

  if (g_file_test (filename, G_FILE_TEST_IS_DIR))
    {
      warningdialog (_("There is no support for loading whole folders of commands yet, sorry"));
      return ret;
    }
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
      g_warning ("Empty Document");
      xmlFreeDoc (doc);
      return ret;
    }

  if (xmlStrcmp (rootElem->name, COMMANDXML_TAG_ROOT))
    {
      g_warning ("Document has wrong type");
      xmlFreeDoc (doc);
      return ret;
    }
  rootElem = rootElem->xmlChildrenNode;


  while (rootElem != NULL)
    {
      parseKeymap (doc, rootElem, Denemo.map, menupath);

      if (Denemo.last_merged_command)
        g_free (Denemo.last_merged_command);
      Denemo.last_merged_command = g_strdup (filename);
      if (menupath)
        execute_init_scripts (menupath);

      if(!Denemo.non_interactive)
        update_all_labels (Denemo.map);
      ret = 0;

      rootElem = rootElem->next;
    }

  xmlFreeDoc (doc);

  {
    //if this is a new-style .commands file, we need to load the keybindings separately
    gchar *name = g_strdup (filename);
    gchar *ext = remove_extension (name);
    if (ext && !strcmp (ext, "commands") && !Denemo.non_interactive)
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
      g_warning ("Empty Document");
      xmlFreeDoc (doc);
      return ret;
    }
  //g_debug ("RootElem %s\n", rootElem->name);
  if (xmlStrcmp (rootElem->name, COMMANDXML_TAG_ROOT))
    {
      g_warning ("Document has wrong type");
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
  g_debug ("binding is : (dnm) %s, (gtk) %s \n", dnm_binding, kb_name);
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
  xmlNewChild (child, NULL, (xmlChar*) "state", (xmlChar*) statestr);
  xmlNewChild (child, NULL, (xmlChar*) "cursor", (xmlChar*) numstr);
  g_free (statestr);
  g_free (numstr);
}

gint
save_xml_keymap (gchar * filename)      //_!!! create a DEV version here, saving C-code to create the actions at runtime with translatable tooltips.
{
  keymap *the_keymap = Denemo.map;
  gint i, ret = -1;
  xmlDocPtr doc;
  xmlNodePtr parent, child;
  gchar* cfilename = NULL;
  const gchar *basename = NULL;
  gchar* dir = NULL;
  doc = xmlNewDoc ((xmlChar *) "1.0");
  command_row* row;
  doc->xmlRootNode = parent = xmlNewDocNode (doc, NULL, COMMANDXML_TAG_ROOT, NULL);

  child = xmlNewChild (parent, NULL, COMMANDXML_TAG_MERGE, NULL);

  xmlNewTextChild (child, NULL, COMMANDXML_TAG_TITLE, (xmlChar *) "A Denemo Command Set");
  xmlNewTextChild (child, NULL, COMMANDXML_TAG_AUTHOR, (xmlChar *) "AT, JRR, RTS");

  parent = xmlNewChild (child, NULL, COMMANDXML_TAG_MAP, NULL);

  child = xmlNewChild (parent, NULL, COMMANDXML_TAG_CURSORS, NULL);

  g_hash_table_foreach (Denemo.map->cursors, (GHFunc) output_pointer_shortcut, child);


  for (i = 0; i < keymap_size (the_keymap); i++)
    {
      keymap_get_command_row (the_keymap, &row, i);

      gpointer action = (gpointer) lookup_action_from_idx (the_keymap, i);
      gchar *name = (gchar *) lookup_name_from_idx (the_keymap, i);
//g_print ("Storing %s at %d\n", name, i);
      basename = denemo_action_get_name (action);

      if(!is_action_name_builtin(name))
      {
        // Check if the command metada file exists
        cfilename = g_strconcat (basename, XML_EXT, NULL);
        dir = find_command_dir(i, cfilename);
        g_free(cfilename);
        if(!dir)
        {
          g_warning("Unable to find metadata file for script %s", name);
          continue;
        }

        // Check if the command data file exists
        cfilename = g_strconcat (basename, SCM_EXT, NULL);
        dir = find_command_dir(i, cfilename);
        g_free(cfilename);
        if(!dir)
        {
          g_warning("Unable to find data file for script %s", name);
          continue;
        }
      }

      child = xmlNewChild (parent, NULL, COMMANDXML_TAG_ROW, NULL);

      xmlNewTextChild (child, NULL, COMMANDXML_TAG_ACTION, (xmlChar *) name);

      if(!is_action_name_builtin(name))
          xmlNewProp(child, COMMANDXML_TAG_TYPE, COMMAND_TYPE_SCHEME);
      else
          xmlNewProp(child, COMMANDXML_TAG_TYPE, COMMAND_TYPE_BUILTIN);

      if (row->after)
        xmlNewTextChild (child, NULL, COMMANDXML_TAG_AFTER, (xmlChar *) row->after);
      if (row->deleted)              //store as hidden in commands file
        xmlNewTextChild (child, NULL, COMMANDXML_TAG_HIDDEN, (xmlChar *) "true");
      gchar *menupath;
      if (row->menupath)
        menupath = row->menupath;
      else
        menupath = get_menupath_for_name (name);
      if (menupath)
        xmlNewTextChild (child, NULL, COMMANDXML_TAG_MENUPATH, (xmlChar *) menupath);
        
      gchar *label = (gchar *) lookup_label_from_idx (the_keymap, i);
      if (label)
        xmlNewTextChild (child, NULL, COMMANDXML_TAG_LABEL, (xmlChar *) label);


      gchar *tooltip = (gchar *) lookup_tooltip_from_idx (the_keymap, i);
      if (tooltip)
        xmlNewTextChild (child, NULL, COMMANDXML_TAG_TOOLTIP, (xmlChar *) tooltip);
    }

  xmlSaveFormatFileEnc (filename, doc, XML_ENCODING, 1);

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
  command_row *row;

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
      keymap_get_command_row (the_keymap, &row, i);

      gpointer action = (gpointer) lookup_action_from_idx (the_keymap, i);
      if (row->deleted && !is_action_id_builtin(i))
        continue;
      if (row->hidden || command_has_binding (i))
        {
          child = xmlNewChild (parent, NULL, COMMANDXML_TAG_ROW, NULL);

          gchar *name = (gchar *) lookup_name_from_idx (the_keymap, i);
          //g_debug ("%s %s binding(s) \n", name, command_has_binding (i) ? "has" : "does not have");
          xmlNewTextChild (child, NULL, COMMANDXML_TAG_ACTION, (xmlChar *) name);
          if (row->hidden)
            xmlNewTextChild (child, NULL, COMMANDXML_TAG_HIDDEN, (xmlChar *) "true");

          g_list_foreach(row->bindings, (GFunc) write_xml_keybinding_info, child);
        }
    }

  xmlSaveFormatFileEnc (filename, doc, XML_ENCODING, 1);

  xmlFreeDoc (doc);
  return ret;
}


gint
save_command_metadata (gchar * filename, gchar * myname, gchar * mylabel, gchar * mytooltip, gchar * after)
{
  xmlDocPtr doc;
  xmlNodePtr parent, child;

  doc = xmlNewDoc ((xmlChar *) "1.0");
  doc->xmlRootNode = parent = xmlNewDocNode (doc, NULL, COMMANDXML_TAG_ROOT, NULL);
  child = xmlNewChild (parent, NULL, COMMANDXML_TAG_MERGE, NULL);

  xmlNewTextChild (child, NULL, COMMANDXML_TAG_TITLE, (xmlChar *) "A Denemo Keymap");
  xmlNewTextChild (child, NULL, COMMANDXML_TAG_AUTHOR, (xmlChar *) "AT, JRR, RTS");

  parent = xmlNewChild (child, NULL, COMMANDXML_TAG_MAP, NULL);

  child = xmlNewChild (parent, NULL, COMMANDXML_TAG_ROW, NULL);
  xmlNewProp(child, COMMANDXML_TAG_TYPE, COMMAND_TYPE_SCHEME);

  xmlNewTextChild (child, NULL, COMMANDXML_TAG_ACTION, (xmlChar *) myname);

  if (after)
    xmlNewTextChild (child, NULL, COMMANDXML_TAG_AFTER, (xmlChar *) after);


  xmlNewTextChild (child, NULL, COMMANDXML_TAG_LABEL, (xmlChar *) mylabel);

  xmlNewTextChild (child, NULL, COMMANDXML_TAG_TOOLTIP, (xmlChar *) mytooltip);

  xmlSaveFormatFileEnc (filename, doc, XML_ENCODING, 1);
  xmlFreeDoc (doc);
  return 0;
}

gint
save_command_data (gchar * filename, gchar * myscheme)
{
  g_file_set_contents (filename, myscheme, -1, NULL);
  return 0;
}

gchar *
load_command_data (gint idx)
{
  gchar *basename = (gchar*) lookup_name_from_idx (Denemo.map, idx);
  gchar *filename = g_strconcat (basename, SCM_EXT, NULL);
  gchar* path = NULL;
  gchar* scheme = NULL;
  GError* error = NULL;

  // Locate the script
  gchar* dir = find_command_dir(idx, filename);
  if(!dir)
  {
    gchar* msg = g_strdup_printf(_("Unable to locate the script %s"), filename);
    warningdialog (msg);
    g_free(msg);
    g_free(filename);
    return NULL;
  }

  // Load the script
  path = g_build_filename(dir, filename, NULL);
  g_free(filename);
  if(!g_file_get_contents (path, &scheme, NULL, &error))
  {
    gchar* msg = g_strdup_printf(_("Unable to load the script %s"), path);
    warningdialog (msg);
    g_free(msg);
    g_free(path);
    return NULL;
  }
  g_free(path);

  // Load the init script if there is one
  path = g_build_filename (dir, INIT_SCM, NULL);
  if (g_file_test (path, G_FILE_TEST_EXISTS))
    //scm_c_primitive_load(path);Use scm_c_primitive_load together with scm_internal_catch and scm_handle_by_message_no_exit instead.
    eval_file_with_catch (path);
  g_free (path);

  g_free (dir);

  return scheme;
}

#ifdef DEVELOPER
//code to take denemoui.xml and Default.commands from the cwd and output a new version of Default.commands with builtin commands' menupaths inserted
GString *contents = NULL;


static void insert_path (gchar *name, gchar *path)
{
 if(!contents)
    {
     GError* error = NULL;
     gchar *temp;
     g_file_get_contents ("Default.commands", &temp, NULL, &error);
     if(error) exit (-1);
     contents = g_string_new (temp);
     //g_print ("contents %s\n", contents->str);
    }
 name = g_strconcat ("<action>", name, "</action>", NULL);
 gchar *found = g_strrstr (contents->str, name);
 if(found)
    {
     path = g_strconcat ("<menupath>", path, "</menupath>\n", NULL);
     contents = g_string_insert (contents, found - contents->str, path);
    }
}

static void
parseMenuItems (xmlDocPtr doc, xmlNodePtr cur, gchar *folder)
{
  for (cur = cur->xmlChildrenNode; cur != NULL; cur = cur->next)
    {
      if (0 == xmlStrcmp (cur->name, "menuitem"))
        {
          gchar *name = xmlGetProp (cur, "action");
          insert_path (name, folder);
        }
     else if (0 == xmlStrcmp (cur->name, "menu"))
        {
         gchar *menufolder = g_strconcat (folder, "/", xmlGetProp (cur, "action"), NULL);
         parseMenuItems (doc, cur, menufolder);
        }
    }
}

gint
attach_menupaths (void)
{
    gchar * filename = "denemoui.xml";
    gchar * commands = "Default.commands";
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
      g_warning ("Empty Document");
      xmlFreeDoc (doc);
      return ret;
    }
  //g_debug ("RootElem %s\n", rootElem->name);
  if (xmlStrcmp (rootElem->name, "ui"))
    {
      g_warning ("Document has wrong type");
      xmlFreeDoc (doc);
      return ret;
    }

  xmlNodePtr cur;
 for (cur = rootElem->xmlChildrenNode; cur != NULL; cur = cur->next)
    {
      if ((0 == xmlStrcmp (cur->name, "menubar")))
        {
          gchar *folder = xmlGetProp (cur, "name");
          parseMenuItems (doc, cur, folder);
        }
    }
    g_print ("\n%s", contents->str);
  xmlFreeDoc (doc);
  return ret;
}
#endif
