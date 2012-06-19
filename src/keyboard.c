#include "keyboard.h"
#include "kbd-custom.h"
#include "view.h"
#include <string.h>

/* merge types for command sets */
#define DENEMO_MERGING (1<<0)
#define DENEMO_INTERACTIVE (1<<1)

static 	void show_type(GtkWidget *widget, gchar *message);
/*
 * translate a keybinding from the format used in denemo keymaprc file to the
 * format understood by gtk_accelerator_parse. The output is an allocated string
 * that must be freed by the caller.
 */
static gchar *
translate_binding_dnm_to_gtk (const gchar *dnm_binding)
{

  if(!Denemo.prefs.strictshortcuts) {
    return g_strdup(dnm_binding);
  }


  /* hold is now "modifiers+keyname" or just "keyname" */
  guint len, i;
  gchar **tokens = g_strsplit (dnm_binding, "+", 0);
  gchar *res, *save;
  len = g_strv_length(tokens);
  if (len == 0)
      res = NULL;
  else if (len == 1)
      res = g_strdup(dnm_binding);
  else {
      res = "";
      for (i = 0; i < len - 1; i++) {
          save = res;
          res = g_strconcat(res, "<", tokens[i], ">", NULL);
          if (save[0])
            g_free(save);
      }
      save = res;
      res = g_strconcat(res, tokens[len-1], NULL);
      g_free(save);
  }
  g_strfreev(tokens);
  return res;
}

/*
 * translate a keybinding from the format used in denemo keymaprc file to the
 * format understood by gtk_accelerator_parse. The output is an allocated string
 * that must be freed by the caller.
 */
static gchar *
translate_binding_gtk_to_dnm (const gchar *gtk_binding)
{
  if(!Denemo.prefs.strictshortcuts) {
    return g_strdup(gtk_binding);
  }


  gchar *res = "", *save, *next, *mod;
  const gchar *cur = gtk_binding;
  while (cur[0] == '<') {
      next = strchr(cur, '>');
      if (!next) {
          cur = NULL;
          if (res[0])
            g_free(res);
          return NULL;
      }
      mod = g_strndup(cur + 1, next - cur - 1);
      if (res[0]) {
        save = res;
        res = g_strconcat(res, "+", NULL);
        g_free(save);
      }
      save = res;
      res = g_strconcat(res, mod, NULL);
      g_free(mod);
      if (save[0])
        g_free(save);
      cur = next + 1;
  }
  if (!res[0])
      return g_strdup(gtk_binding);
  else {
      save = res;
      res = g_strconcat(res, "+", cur, NULL);
      g_free(save);
      return res;
  }
}

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

/* add ui elements for menupath if missing */
static void
instantiate_menus(gchar *menupath) {
  //g_print("Instantiate menus for %s\n", menupath);
 
  gchar *up1 = g_path_get_dirname(menupath);
  gchar *name=g_path_get_basename(menupath);
  GtkWidget *widget = gtk_ui_manager_get_widget(Denemo.ui_manager, up1);
  if(!strcmp(up1, "/")) {
    g_critical("bad menu path");
    return;
  }
  if(widget==NULL)
    instantiate_menus(up1);
  GList *groups = gtk_ui_manager_get_action_groups (Denemo.ui_manager);
  GtkActionGroup *action_group = GTK_ACTION_GROUP(groups->data); //FIXME assuming the one we want is first
  if(NULL == gtk_action_group_get_action(action_group, name)) {
    gchar *tooltip = g_strconcat("Menu:\nnamed \"", name, "\" located at ", menupath, " in the menu system", NULL);
    GtkAction *action = gtk_action_new(name,name,tooltip,NULL);
    g_free(tooltip);
    gtk_action_group_add_action(action_group, action);
    g_object_set_data(G_OBJECT(action), "menupath", up1);
  }
  gtk_ui_manager_add_ui(Denemo.ui_manager,gtk_ui_manager_new_merge_id(Denemo.ui_manager), 
			up1,
			name, name, GTK_UI_MANAGER_MENU, FALSE);
  //g_print("Adding %s to %s\n", name, up1);
  // widget = gtk_ui_manager_get_widget(Denemo.ui_manager, menupath);
  //show_type (widget, "for menupath widget is ");
  
}

void set_visibility_for_action(GtkAction *action, gboolean visible) {
  if(GTK_IS_ACTION(action)) {
    GSList *h = gtk_action_get_proxies (action);
    for(;h;h=h->next) {
      if(visible)
	gtk_widget_show(h->data);
      else
	gtk_widget_hide(h->data);
    }
    g_object_set_data(G_OBJECT(action), "hidden", (gpointer)!visible);
  }

}

static void hide_action_of_name(gchar *name){
  GtkAction *action = lookup_action_from_name (name);
  set_visibility_for_action(action, FALSE);
}
static void show_action_of_name(gchar *name){
  GtkAction *action = lookup_action_from_name (name);
  set_visibility_for_action(action, TRUE);
}
static void
add_ui(gchar *menupath, gchar *after, gchar *name) {
  GtkWidget *widget = gtk_ui_manager_get_widget(Denemo.ui_manager, menupath);
  if(widget==NULL) {
    instantiate_menus(menupath);
  }
  //We place the item after the "after" item in the menupath, unless that isn't yet
  //installed, in which case we just append to the menu
  gchar *menupath_item = g_build_filename(menupath,after,NULL);
  GtkAction *sibling = gtk_ui_manager_get_action (Denemo.ui_manager, menupath_item);
#ifdef DEBUG
  if( (after!=NULL) & (sibling==NULL)) {
    static gboolean once=TRUE;
    if(once) {
      gchar *msg = g_strdup_printf("Cannot place %s after %s as requested,\nbecause I haven't seen %s yet\nTo fix this delete the %s command save the command set,\nQuit and restart Denemo\nThen re-install %s by right clicking on the %s item\nand choosing More Commands\nAnd finally saving command set again", name, after, after, name, name, after);
      // infodialog(msg);
      g_warning("%s", msg);
      g_free(msg);
      //once = FALSE;
    }
  }
#endif
  gtk_ui_manager_add_ui(Denemo.ui_manager,gtk_ui_manager_new_merge_id(Denemo.ui_manager),  sibling?menupath_item:menupath, name, name, GTK_UI_MANAGER_AUTO, FALSE);
  g_free(menupath_item);
}

static void
parseScripts (xmlDocPtr doc, xmlNodePtr cur, keymap * the_keymap, gchar *fallback, gint merge)
{
  xmlChar *name=NULL, *menupath=NULL, *label=NULL, *tooltip=NULL, *scheme=NULL, *after=NULL;
  GList *menupaths = NULL;
  cur = cur->xmlChildrenNode;
  gint command_number = -1;
  guint keyval = 0;
  gboolean is_script = FALSE;
  GdkModifierType state = 0;
  DenemoGUI *gui = Denemo.gui;
  gboolean hidden=FALSE;
  for ( ;cur; cur = cur->next)
    {
      if (0 == xmlStrcmp (cur->name, (const xmlChar *) "action")) {
	if (cur->xmlChildrenNode == NULL)
	  {
	    g_warning ("Empty action node found in keymap file\n");
	  }
	else
	  {
	    name = 
	      xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	    menupaths = NULL;//We allow multiple menupaths for a given action, all are added to the gtk_ui when this command is processed after the tooltip node. This is very bad xml, as the action should have all the others as children, and not depend on the order.FIXME
#if 0
	    g_print ("Action %s\n", (gchar *) name);
#endif
	  }
      } else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "scheme")) {
	scheme = 
	  xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);

	is_script = TRUE;
      }    else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "hidden")) {
	hidden = TRUE; 
      }

      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "menupath")) {
	menupath = 
	  xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	menupaths = g_list_append(menupaths, menupath);
      }
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "label")) {
	label = 
	  xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
      }
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "after")) {
	after = 
	  xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
      }
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "tooltip")) {
	tooltip = 
	  xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	/* by convention this is the last of the fields defining a scheme script menu item */
	if(is_script) {
	  name = name?name:(xmlChar*)"NoName";
	  label = label?label:(xmlChar*)"No label";
	  scheme = scheme?scheme:(xmlChar*)"";
	  tooltip = tooltip?tooltip:(xmlChar*)"No indication what this done beyond the name and label :(";
	  GtkAction *action;
	  gboolean new_command = FALSE;
	  action = lookup_action_from_name(name);	     
	  if(action==NULL) {
	    new_command = TRUE;
	    gchar *icon_name = get_icon_for_name(name, label);
	    action = gtk_action_new(name,label,tooltip, icon_name);
	    //g_print("New action %s\n", name);
	    if(hidden)
	      g_object_set_data(G_OBJECT(action), "hidden",  (gpointer)TRUE);
	    if(after)
	      g_object_set_data(G_OBJECT(action), "after", (gpointer)after);
	    register_command(Denemo.map, action, name, label, tooltip, activate_script);
	    GtkActionGroup *action_group;
	    GList *groups = gtk_ui_manager_get_action_groups (Denemo.ui_manager);
	    action_group = Denemo.action_group; 
	    gtk_action_group_add_action(action_group, action);
	    /* create a scheme function to call this script */
	    create_scheme_function_for_script(name);
	  }
	  if(menupath) {
	      GList *g;
	      for(g=menupaths;g;g=g->next){
		menupath = g->data;
		menupath = menupath?menupath:(xmlChar*)"/MainMenu/Other";
		add_ui(menupath, after, name);
	      }
	  } else {
	    if(fallback) {/* no path given, use fallback */
	      menupath = fallback;
	      add_ui(menupath, after, name);
	    }
	  }
	  if((merge&DENEMO_INTERACTIVE)
	     && (merge&DENEMO_MERGING)
	     && new_command) {
	    gchar *msg = g_strdup_printf("Installed a command in the menu system\nat %s\n", menupath);
	    infodialog(msg);
	    g_free(msg);
	  }
	  /*FIXME free?	  gchar *old_scheme = (gchar *)g_object_get_data(G_OBJECT(action), "scheme");*/
	  //g_print("Setting scheme %s\n", scheme);
	  g_object_set_data(G_OBJECT(action), "scheme", scheme);
	  g_object_set_data(G_OBJECT(action), "menupath", menupath);
	  if(new_command) {
	  g_signal_connect (G_OBJECT (action), "activate",
			    G_CALLBACK (activate_script), gui);
	  //g_print("Signal activate is set on action %p %s scheme is %s\n", action, name, scheme);
	  }

	  //g_print("scheme now %s", scheme);
	  // Note the script should *not* be in Default.cmdset
	  // to delay loading it, but we should set the signal initally and we should not repeat setting the signal later.
	  // the signal does not specify which script will be run, that is decided lazily, when the action is invoked for the first time

	  if(hidden)
	    g_object_set_data(G_OBJECT(action), "deleted", (gpointer)TRUE);//Mark hidden items as deleted on loading them
	} // is_script
	// we are not as yet re-writing tooltips etc on builtin commands
#if 1
	else if(hidden) {

	  hide_action_of_name(name); hidden = FALSE;
	  g_print("Hiding Builtin %s\n", name);
	}
#endif
      }// tooltip found, assumed last field
    } // for all nodes
  //alphabeticalize_commands(Denemo.map);
}/* end of parseScripts */


static void
parseBindings (xmlDocPtr doc, xmlNodePtr cur, keymap * the_keymap)
{
  cur = cur->xmlChildrenNode;
  gint command_number = -1;
  guint keyval = 0;
  gboolean is_script = FALSE;
  GdkModifierType state = 0;
  DenemoGUI *gui = Denemo.gui;
  while (cur != NULL)
    {
      //keyval variables
      xmlChar *name, *menupath, *label, *tooltip, *scheme;
      if (0 == xmlStrcmp (cur->name, (const xmlChar *) "action"))
	{
	  if (cur->xmlChildrenNode == NULL)
            {
              g_warning ("Empty children node found in keymap file\n");
            }
	  else
	    {
	      name = 
		xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	      show_action_of_name(name);
#ifdef DEBUG
	      g_print ("Action %s\n", (gchar *) name);
#endif /*DEBUG*/
	    }
	} else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "hidden"))
	{
	    hide_action_of_name(name);

	} else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "bind"))
	{
	  command_number = lookup_command_from_name (the_keymap, (gchar *) name);
	  //g_print("Found bind node for action %s %d\n", name, command_number);
	  if (cur->xmlChildrenNode == NULL)
            {
              g_warning ("Empty <bind><\\bind> found in commandset file\n");
            }
	  else
	    {
	      xmlChar *tmp = 
		xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	      if (tmp)
		{
		  gchar *gtk_binding = translate_binding_dnm_to_gtk((gchar *) tmp);
		  //g_print("gtk_binding is %s\n", gtk_binding);
		  if (gtk_binding) {
		    keyval = 0;
		    if(Denemo.prefs.strictshortcuts)
		      dnm_accelerator_parse(gtk_binding, &keyval, &state);
#ifdef DEBUG
		    g_print ("binding %s, keyval %d, state %d, Command Number %d\n",
			     gtk_binding, keyval, state, command_number);
#endif
		    {
		      gchar *comma;
		      comma = strtok (gtk_binding, ",");
		      if(comma)//two key binding, remove any single keybinding
			{
			  if(-1 != lookup_command_for_keybinding_name(the_keymap, comma))
			    remove_keybinding_from_name (the_keymap, comma);
			  *(comma+strlen(comma))=',';
			}
		    }
		    if (command_number != -1){
		      if(keyval) 
			add_keybinding_to_idx (the_keymap, keyval, state,
					       command_number, POS_LAST);
		      else
			add_named_binding_to_idx (the_keymap, tmp, command_number, POS_LAST);
		    }
		    //FIXME PROBLEM, there are no proxies for the MyName command, even though we have just created it...
		    
		    g_free (gtk_binding);
		  } else {
		    g_print("No gtk equivalent for shortcut %s\n", tmp);
		  }
		  xmlFree (tmp);
		}
	    }
	}  
      cur = cur->next;
    }
} // parseBindings

static void
parseCursorBinding (xmlDocPtr doc, xmlNodePtr cur) {
  gint state, cursor_num;
  xmlChar *tmp;
  for (cur = cur->xmlChildrenNode;cur != NULL; cur = cur->next)
    {
      if (0 == xmlStrcmp (cur->name, (const xmlChar *) "state")) {
	tmp = 
	  xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	      if (tmp)
		{
		  sscanf(tmp, "%x",&state);// = atoi(tmp);
		  xmlFree (tmp);
		}
	     
      } else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "cursor"))
	{
	tmp = 
	  xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	      if (tmp)
		{
		  cursor_num = atoi(tmp);
		  xmlFree (tmp);
		}
	      assign_cursor(state, cursor_num);
	      // g_print("type is %s\n",g_type_name(G_TYPE_FROM_INSTANCE(Denemo.window->window)));
	      // set_cursor_for(state);
	}
    }
}


static void
parseCursors (xmlDocPtr doc, xmlNodePtr cur) {
  xmlChar *tmp;
  for (cur = cur->xmlChildrenNode;cur != NULL; cur = cur->next)
    {
      if (0 == xmlStrcmp (cur->name, (const xmlChar *) "cursor-binding")) {
	parseCursorBinding(doc, cur);
      }
    }
}



static void
parseCommands (xmlDocPtr doc, xmlNodePtr cur, keymap * the_keymap, gchar *menupath, gint merge)
{
  xmlNodePtr ncur = cur->xmlChildrenNode;
  int i;
  for(i=0;i<2;i++)//Two passes, as all Commands have to be added before bindings
  for (ncur = cur->xmlChildrenNode; ncur; ncur = ncur->next)
    {
      if ((0 == xmlStrcmp (ncur->name, (const xmlChar *) "row")))
	{
	  i?parseBindings (doc, ncur, the_keymap):parseScripts (doc, ncur, the_keymap, menupath, merge);
	} else 
	  if (i && (0 == xmlStrcmp (ncur->name, (const xmlChar *) "cursors")))
	    {
	      parseCursors(doc, ncur);

	    }
    }
}


static void
parseKeymap (xmlDocPtr doc, xmlNodePtr cur, keymap * the_keymap, gchar *menupath, gint merge)
{
  for (cur = cur->xmlChildrenNode; cur != NULL;  cur = cur->next)
    {
      if (0 == xmlStrcmp (cur->name, (const xmlChar *) "map"))
	{
	  parseCommands (doc, cur, the_keymap, menupath, merge);
	}
    }
}

/* if filename ends in /menus/.... hierarchy extract and return the tail
   below menus/
*/
gchar *extract_menupath(gchar *filename) {
  gchar *head = g_strdup_printf("%c%s", G_DIR_SEPARATOR, "menus");
  GString *str = g_string_new("");
  gchar *base = g_strrstr(filename, head);
  if(base) {
    base += strlen(head);
    base = g_path_get_dirname(base);
    gchar *c;
    for(c = base; c && *c;c++)
      if(*c==G_DIR_SEPARATOR)
	*c = '/';
    //g_print("got base as %s\n", base);
  }
  g_free(head);


 



  return base;
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
  keymap *the_keymap = Denemo.map; 
  gint ret = -1;
  xmlDocPtr doc;
  //xmlNsPtr ns;
  xmlNodePtr rootElem;
  if(filename==NULL)
    return ret;
  if(g_file_test(filename, G_FILE_TEST_IS_DIR)) {
    warningdialog("There is no support for loading whole folders of commands yet, sorry");
    return ret;
  }
  if(!g_file_test(filename, G_FILE_TEST_EXISTS))
    return ret;
  doc = xmlParseFile (filename);
  gchar *menupath = extract_menupath(filename);
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
  if (xmlStrcmp (rootElem->name, (const xmlChar *) "Denemo"))
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

	    if(Denemo.last_merged_command)
	      g_free(Denemo.last_merged_command);
	    Denemo.last_merged_command = g_strdup(filename);
	    if(menupath)
	      execute_init_scripts(menupath);

	  update_all_labels(Denemo.map);
	  ret = 0;
	
      rootElem = rootElem->next;
    }

  xmlFreeDoc (doc);

  { //if this is a new-style .commands file, we need to load the keybindings separately
    gchar *name = g_strdup(filename);
    gchar *ext = remove_extension(name); 
    if(ext && !strcmp(ext, "commands")) {
      gchar *newname = g_strdup_printf("%s%s", name, ".shortcuts");
      load_xml_keybindings(newname);
      g_free(newname);
    }
    g_free(name);
  }
  return ret;
}





gint
load_xml_keybindings (gchar * filename)
{
  keymap *the_keymap = Denemo.map; 
  gint ret = -1;
  xmlDocPtr doc;
  //xmlNsPtr ns;
  xmlNodePtr rootElem;
  if(filename==NULL)
    return ret;
  if(!g_file_test(filename, G_FILE_TEST_EXISTS))
    return ret;
  doc = xmlParseFile (filename);
  if (doc == NULL) {
    g_debug ("Could not read XML file %s", filename);
    return ret;
  }
  rootElem = xmlDocGetRootElement (doc);
  if (rootElem == NULL) {
    g_warning ("Empty Document\n");
    xmlFreeDoc (doc);
    return ret;
  }
  //g_print ("RootElem %s\n", rootElem->name);
  if (xmlStrcmp (rootElem->name, (const xmlChar *) "Denemo"))
    {
      g_warning ("Document has wrong type\n");
      xmlFreeDoc (doc);
      return ret;
    }
  rootElem = rootElem->xmlChildrenNode;

  while (rootElem != NULL) {
    if ( (0 == xmlStrcmp (rootElem->name, (const xmlChar *) "merge"))) {
      xmlNodePtr cur;
      for (cur = rootElem->xmlChildrenNode; cur != NULL;  cur = cur->next) {
	if (0 == xmlStrcmp (cur->name, (const xmlChar *) "map")) {
	  xmlNodePtr ncur;
	  for(ncur=cur->xmlChildrenNode;ncur != NULL; ncur = ncur->next) {
	    parseBindings (doc, ncur, Denemo.map);
	  }
	  ret = 0;
	}
      }
      update_all_labels(Denemo.map);
    }
    rootElem = rootElem->next;
  }
  xmlFreeDoc (doc);
  return ret;
}






















static void
write_xml_keybinding_info (gchar *kb_name, xmlNodePtr node)
{
  gchar *dnm_binding = translate_binding_gtk_to_dnm(kb_name);
#ifdef DEBUG
  g_print ("binding is : (dnm) %s, (gtk) %s \n", dnm_binding, kb_name);
#endif
  xmlNewTextChild (node, NULL, (xmlChar *) "bind", (xmlChar *) dnm_binding);
  g_free(dnm_binding);

}

static void
output_pointer_shortcut(gint *state, GdkCursor *cursor, xmlNodePtr parent){
  gchar *statestr = g_strdup_printf("%x", *state);
#if GTK_MAJOR_VERSION==3
  gint cursor_num = gdk_cursor_get_cursor_type(cursor);
#else
  gint cursor_num = cursor->type;
#endif
  gchar *numstr = g_strdup_printf("%d", cursor_num);
  xmlNodePtr child = xmlNewTextChild (parent, NULL, (xmlChar *) "cursor-binding", NULL);
  xmlNewChild (child, NULL, "state", statestr);
  xmlNewChild (child, NULL, "cursor", numstr);
  g_free(statestr);
  g_free(numstr);
}
gint
save_xml_keymap (gchar * filename)
{
  keymap *the_keymap = Denemo.map;
  gint i, ret = -1;
  xmlDocPtr doc;
  //xmlNsPtr ns;
  xmlNodePtr parent, child, command;

  doc = xmlNewDoc ((xmlChar *) "1.0");
  doc->xmlRootNode = parent = xmlNewDocNode (doc, NULL, (xmlChar *) "Denemo",
					     NULL);


  child = xmlNewChild (parent, NULL, (xmlChar *) "merge", NULL);

  xmlNewTextChild (child, NULL, (xmlChar *) "title", (xmlChar *) "A Denemo Command Set");
  xmlNewTextChild (child, NULL, (xmlChar *) "author", (xmlChar *) "AT, JRR, RTS");

  parent = xmlNewChild (child, NULL, (xmlChar *) "map", NULL);

  child = xmlNewChild (parent, NULL, (xmlChar *) "cursors", NULL);

  g_hash_table_foreach(Denemo.map->cursors, (GHFunc)output_pointer_shortcut, child);


  for (i = 0; i < keymap_size(the_keymap); i++)
    {

     
      gpointer action = (gpointer)lookup_action_from_idx(the_keymap, i);
      gchar *scheme = action?g_object_get_data(action, "scheme"):NULL;
      gchar *after = action?g_object_get_data(action, "after"):NULL;
      gboolean deleted = (gboolean) (action?g_object_get_data(action, "deleted"):NULL);
      gboolean hidden = (gboolean) (action?g_object_get_data(action, "hidden"):NULL);
      //  if(deleted && scheme)
      //	continue;

      child = xmlNewChild (parent, NULL, (xmlChar *) "row", NULL);

			
      gchar *name = (gchar*)lookup_name_from_idx(the_keymap, i);
#ifdef DEBUG
      g_print ("%s \n", name);
#endif	
      xmlNewTextChild (child, NULL, (xmlChar *) "action",
		       (xmlChar *) name); 
      if(after)
	xmlNewTextChild (child, NULL, (xmlChar *) "after",
			 (xmlChar *) after);  
      if(deleted) //store as hidden in commands file
      	xmlNewTextChild (child, NULL, (xmlChar *) "hidden", "true");

      if(scheme) 	
	xmlNewTextChild (child, NULL, (xmlChar *) "scheme",
			 /* (xmlChar *) scheme*/ (xmlChar*)"");
      
      gchar *menupath = action?g_object_get_data(action, "menupath"):NULL;
      if(menupath)
	xmlNewTextChild (child, NULL, (xmlChar *) "menupath",
			 (xmlChar *) menupath);
      
      gchar *label =   (gchar*)lookup_label_from_idx (the_keymap, i);
      if(label)
	xmlNewTextChild (child, NULL, (xmlChar *) "label",
			 (xmlChar *) label);
      
      
      gchar *tooltip = (gchar*)lookup_tooltip_from_idx (the_keymap, i);
      if(tooltip)
	xmlNewTextChild (child, NULL, (xmlChar *) "tooltip",
			 (xmlChar *) tooltip);
      
      //  keymap_foreach_command_binding (the_keymap, i,
      //				      (GFunc) write_xml_keybinding_info, child);
      
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
  xmlNodePtr parent, child, command;

  doc = xmlNewDoc ((xmlChar *) "1.0");
  doc->xmlRootNode = parent = xmlNewDocNode (doc, NULL, (xmlChar *) "Denemo",
					     NULL);
  child = xmlNewChild (parent, NULL, (xmlChar *) "merge", NULL);

  xmlNewTextChild (child, NULL, (xmlChar *) "title", (xmlChar *) "A Denemo Command Set");
  xmlNewTextChild (child, NULL, (xmlChar *) "author", (xmlChar *) "AT, JRR, RTS");

  parent = xmlNewChild (child, NULL, (xmlChar *) "map", NULL);

  child = xmlNewChild (parent, NULL, (xmlChar *) "cursors", NULL);

  g_hash_table_foreach(Denemo.map->cursors, (GHFunc)output_pointer_shortcut, child);


  for (i = 0; i < keymap_size(the_keymap); i++)
    {
      gpointer action = (gpointer)lookup_action_from_idx(the_keymap, i);
      gchar *scheme = action?g_object_get_data(action, "scheme"):NULL;
      gboolean deleted = (gboolean) (action?g_object_get_data(action, "deleted"):NULL);
      gboolean hidden = (gboolean) (action?g_object_get_data(action, "hidden"):NULL);
      if(deleted && scheme)
	continue;
      if(hidden || command_has_binding(i)) {
	child = xmlNewChild (parent, NULL, (xmlChar *) "row", NULL);
		
	gchar *name = (gchar*)lookup_name_from_idx(the_keymap, i);
#ifdef DEBUG
	g_print ("%s %s binding(s) \n", name, command_has_binding(i)?"has":"does not have" );
#endif	
	xmlNewTextChild (child, NULL, (xmlChar *) "action",
			 (xmlChar *) name); 
	if(hidden)
	  xmlNewTextChild (child, NULL, (xmlChar *) "hidden", "true");
	
	keymap_foreach_command_binding (the_keymap, i,
					(GFunc) write_xml_keybinding_info, child);
      }
    }

  xmlSaveFormatFile (filename, doc, 1);

  xmlFreeDoc (doc);
  return ret;
}

static 	void show_type(GtkWidget *widget, gchar *message) {
    g_print("%s%s\n",message, widget?g_type_name(G_TYPE_FROM_INSTANCE(widget)):"NULL widget");
  }

/* not used */
static gint create_dir_for_menu(gchar *str) {
  gchar *thismenu = g_build_filename (locatedotdenemo(), "actions", "menus", str , NULL);
    if(!g_file_test(thismenu, G_FILE_TEST_IS_DIR)) {
      return g_mkdir_with_parents(thismenu, 0770);
    }
  return 0;
}


/* parse an entry in denemoui.xml recursively
   set menupath as attribute of widgets
 */
static gint
parseMenu(xmlNodePtr rootElem, gchar *path, DenemoGUI *gui ) {
  for ( rootElem = rootElem->xmlChildrenNode;rootElem; rootElem = rootElem->next)
    {
      if(0 == xmlStrcmp (rootElem->name, (const xmlChar *) "menubar") ||
	 0 == xmlStrcmp (rootElem->name, (const xmlChar *) "menu") ||
	 0 == xmlStrcmp (rootElem->name, (const xmlChar *) "menubar") ||
	 0 == xmlStrcmp (rootElem->name, (const xmlChar *) "toolbar"))
	/* ignoring popup menus */ {
	gchar *name = (gchar *) xmlGetProp (rootElem, (xmlChar *) "name");
	if(name==NULL)
	  name = (gchar *) xmlGetProp (rootElem, (xmlChar *) "action");
	
	if(name) {
	  gchar *str = g_strdup_printf("%s%s%s", path, "/", name);
	  GtkWidget *widget = gtk_ui_manager_get_widget (Denemo.ui_manager, str); 
	  if(widget) {
	    g_object_set_data(G_OBJECT(widget), "menupath", str);
	    // we do this in menu_click when needed create_dir_for_menu(str);//FIXME we only need do this once for a given denemoui.xml
	    //show_type(widget, "The type is ");
	    //g_print("set %p %s\n",widget, str);
	    parseMenu(rootElem, str, gui);
	  }
	  else
	    g_warning("no object for %s\n", str);
	}

      }
      if(0 == xmlStrcmp (rootElem->name, (const xmlChar *) "menuitem")) {
	gchar *name = (gchar *) xmlGetProp (rootElem, (xmlChar *) "action");
       if(name) {
	  gchar *str = g_strdup_printf("%s%s%s", path, "/", name);
	  GtkWidget *widget = gtk_ui_manager_get_widget (Denemo.ui_manager, str);
	  g_free(str);
	  //show_type(widget, "The type is ");
	  //g_print("set %p %s\n",widget, path);
	  if(widget) {
	    g_object_set_data(G_OBJECT(widget), "menupath", g_strdup(path));
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
parse_paths (gchar * filename, DenemoGUI *gui)
{
  gint ret = -1;
  xmlDocPtr doc;
  //xmlNsPtr ns;
  xmlNodePtr rootElem;
  if(filename==NULL)
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
  gchar *path="";
  parseMenu(rootElem, path, gui);
  ret = 0;
  xmlFreeDoc (doc);
  return ret;
}



gint
save_script_as_xml (gchar * filename, gchar *myname, gchar *myscheme, gchar *mylabel, gchar *mytooltip, gchar *after)
{
  xmlDocPtr doc;
  //xmlNsPtr ns;
  xmlNodePtr parent, child, command;

  doc = xmlNewDoc ((xmlChar *) "1.0");
  doc->xmlRootNode = parent = xmlNewDocNode (doc, NULL, (xmlChar *) "Denemo",
					     NULL);
  child = xmlNewChild (parent, NULL, (xmlChar *) "merge", NULL);
  
  xmlNewTextChild (child, NULL, (xmlChar *) "title", (xmlChar *) "A Denemo Keymap");
  xmlNewTextChild (child, NULL, (xmlChar *) "author", (xmlChar *) "AT, JRR, RTS");
  
  parent = xmlNewChild (child, NULL, (xmlChar *) "map", NULL);
  
  child = xmlNewChild (parent, NULL, (xmlChar *) "row", NULL);
  if(after)
    xmlNewTextChild (child, NULL, (xmlChar *) "after",
		     (xmlChar *) after);  
  
  xmlNewTextChild (child, NULL, (xmlChar *) "action",
		   (xmlChar *) myname);  
  
  xmlNewTextChild (child, NULL, (xmlChar *) "scheme",
		   (xmlChar *) myscheme);
  
  xmlNewTextChild (child, NULL, (xmlChar *) "label",
		   (xmlChar *) mylabel);
  
  
  xmlNewTextChild (child, NULL, (xmlChar *) "tooltip",
		   (xmlChar *) mytooltip);      
  xmlSaveFormatFile (filename, doc, 1);
  
  xmlFreeDoc (doc);
  return 0;
}
