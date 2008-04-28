#include "keyboard.h"
#include "kbd-custom.h"
#include <string.h>

extern struct name_action_and_function *denemo_commands;
static gint
lookup_command (gchar * name)
{
  gint i, ret = -1;

  for (i = 0; i < denemo_commands_size; i++)
    {
      /* g_print("Denemo Cmd %s,  Passed Name %s\n",
         denemo_commands[i].name, name); */
      if (strcmp (_(denemo_commands[i].name), name) == 0)
	{
	  ret = i;
	  break;
	}

    }

  return ret;
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



static void
parseBinding (xmlDocPtr doc, xmlNodePtr cur, keymap * the_keymap)
{
  cur = cur->xmlChildrenNode;
  gint command_number = -1, state = 0, keyval = 0;

  while (cur != NULL)
    {
      //keyval variables

      if (0 == xmlStrcmp (cur->name, (const xmlChar *) "action"))
	{
	  if (cur->xmlChildrenNode == NULL)
            {
              g_warning ("Empty children node found in keymap file\n");
            }
	  else
	    {
	      xmlChar *tmp = 
		xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
#ifdef DEBUG
	      g_print ("Action %s\n", (gchar *) tmp);
#endif /*DEBUG*/

	      if (tmp)
		{
		  command_number = lookup_command ((gchar *) tmp);
		  xmlFree (tmp);
		}
	    }
	}
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "bind"))
	{
	  
	  if (cur->xmlChildrenNode == NULL)
            {
              g_warning ("Empty children node found in keymap file\n");
            }
	  else
	    {
	      xmlChar *tmp = 
		xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	      if (tmp)
		{
		  gchar *hold = g_strdup ((gchar *) tmp);
		  /* hold is now "modifiers+keyname" or just "keyname" */
		  gchar *keyname = strrchr (hold, '+');
		  gchar *modifiers = hold;
		  if (keyname)
		    {
		      *keyname++ = '\0';
		    }
		  else
		    {
		      keyname = modifiers;
		      modifiers = NULL;
		    }
		  state = modifiers ? get_state (modifiers) : 0;
		  keyval = gdk_keyval_from_name (keyname);
#ifdef DEBUG
		  g_print ("keyval %d, state %d, Command Number %d\n", keyval,
			   state, command_number);
#endif
		  if (command_number != -1)
		    add_keybinding (the_keymap, keyval, state, command_number);
		  
		  g_free (hold);
		  xmlFree (tmp);
		}
	    }
	}
      cur = cur->next;
      
    }
  
}

static void
parseRow (xmlDocPtr doc, xmlNodePtr cur, keymap * the_keymap)
{
  cur = cur->xmlChildrenNode;

  while (cur != NULL)
    {
      if (0 == xmlStrcmp (cur->name, (const xmlChar *) "row"))
	{
	  parseBinding (doc, cur, the_keymap);
	}
      cur = cur->next;
    }

}

static void
parseKeymap (xmlDocPtr doc, xmlNodePtr cur, keymap * the_keymap)
{
  cur = cur->xmlChildrenNode;

  while (cur != NULL)
    {
      if (0 == xmlStrcmp (cur->name, (const xmlChar *) "map"))
	{
	  parseRow (doc, cur, the_keymap);
	}
      cur = cur->next;
    }


}

/* returns 0 on success
 * negative on failure
 */
gint
load_xml_keymap (gchar * filename, keymap * the_keymap)
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
  g_print ("RootElem %s\n", rootElem->name);
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
      if (0 == xmlStrcmp (rootElem->name, (const xmlChar *) "keymap"))
	{

	  parseKeymap (doc, rootElem, the_keymap);
	}
      rootElem = rootElem->next;
    }
  ret = 0;
  xmlFreeDoc (doc);
  return ret;
}


static void
write_xml_keybinding_info (KeybindingInfo * ki, xmlNodePtr node)
{
  gchar *string = NULL;

  if (ki->state)
    {
      set_state (ki->state, &string);
      string = g_strconcat (string, gdk_keyval_name (ki->keyval), NULL);
    }
  else
    {
      string = gdk_keyval_name (ki->keyval);
    }
#ifdef DEBUG
  g_print ("binding is %s \n", string);
#endif
  xmlNewTextChild (node, NULL, (xmlChar *) "bind", (xmlChar *) string);
  if (ki->state)
    g_free (string);
}

gint
save_xml_keymap (gchar * filename, keymap * the_keymap)
{
  gint i, ret = -1;
  xmlDocPtr doc;
  //xmlNsPtr ns;
  xmlNodePtr parent, child;

  doc = xmlNewDoc ((xmlChar *) "1.0");
  doc->xmlRootNode = parent = xmlNewDocNode (doc, NULL, (xmlChar *) "Denemo",
					     NULL);
  child = xmlNewChild (parent, NULL, (xmlChar *) "keymap", NULL);

  xmlNewTextChild (child, NULL, (xmlChar *) "title", (xmlChar *) "Test");
  xmlNewTextChild (child, NULL, (xmlChar *) "author", (xmlChar *) "Adam Tee");

  parent = xmlNewChild (child, NULL, (xmlChar *) "map", NULL);

  for (i = 0; i < denemo_commands_size; i++)
    {
      child = xmlNewChild (parent, NULL, (xmlChar *) "row", NULL);
      xmlNewTextChild (child, NULL, (xmlChar *) "action",
		       (xmlChar *) denemo_commands[i].name);
#ifdef DEBUG
      g_print ("%s \n", denemo_commands[i].name);
#endif
      g_list_foreach (the_keymap->commands[i],
		      (GFunc) write_xml_keybinding_info, child);

    }


  xmlSaveFormatFile (filename, doc, 1);

  xmlFreeDoc (doc);
  return ret;
}
