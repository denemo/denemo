#include "keyboard.h"
#include "kbd-custom.h"
#include <string.h>

extern struct name_action_and_function *denemo_commands;

/*
 * translate a keybinding from the format used in denemo keymaprc file to the
 * format understood by gtk_accelerator_parse. The output is an allocated string
 * that must be freed by the caller.
 */
static gchar *
translate_binding_dnm_to_gtk (const gchar *dnm_binding)
{
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

static void
parseBinding (xmlDocPtr doc, xmlNodePtr cur, keymap * the_keymap)
{
  cur = cur->xmlChildrenNode;
  gint command_number = -1;
  guint keyval = 0;
  GdkModifierType state = 0;

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
		  command_number = lookup_index_from_name (the_keymap, (gchar *) tmp);
          if (command_number == -1) {
              g_print("Action %s does not exist\n", (gchar *) tmp);
          }
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
		  gchar *gtk_binding = translate_binding_dnm_to_gtk((gchar *) tmp);
          if (gtk_binding) {
            dnm_accelerator_parse(gtk_binding, &keyval, &state);
#ifdef DEBUG
		    g_print ("binding %s, keyval %d, state %d, Command Number %d\n",
                  gtk_binding, keyval, state, command_number);
#endif
		    if (command_number != -1)
		        add_keybinding_from_idx (the_keymap, keyval, state,
                        command_number);
		  
		    g_free (gtk_binding);
          } else {
              g_warning("Could not parse binding", (gchar *) tmp);
          }
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
write_xml_keybinding_info (gchar *kb_name, xmlNodePtr node)
{
  gchar *dnm_binding = translate_binding_gtk_to_dnm(kb_name);
#ifdef DEBUG
  g_print ("binding is : (dnm) %s, (gtk) %s \n", dnm_binding, kb_name);
#endif
  xmlNewTextChild (node, NULL, (xmlChar *) "bind", (xmlChar *) dnm_binding);
  g_free(dnm_binding);
}

gint
save_xml_keymap (gchar * filename, keymap * the_keymap)
{
  gint i, ret = -1;
  xmlDocPtr doc;
  //xmlNsPtr ns;
  xmlNodePtr parent, child;
  const gchar *command_name;

  doc = xmlNewDoc ((xmlChar *) "1.0");
  doc->xmlRootNode = parent = xmlNewDocNode (doc, NULL, (xmlChar *) "Denemo",
					     NULL);
  child = xmlNewChild (parent, NULL, (xmlChar *) "keymap", NULL);

  xmlNewTextChild (child, NULL, (xmlChar *) "title", (xmlChar *) "Test");
  xmlNewTextChild (child, NULL, (xmlChar *) "author", (xmlChar *) "Adam Tee");

  parent = xmlNewChild (child, NULL, (xmlChar *) "map", NULL);

  for (i = 0; i < keymap_size(the_keymap); i++)
    {
      command_name = lookup_name_from_idx(the_keymap, i);
      child = xmlNewChild (parent, NULL, (xmlChar *) "row", NULL);
      xmlNewTextChild (child, NULL, (xmlChar *) "action",
		       (xmlChar *) command_name);
#ifdef DEBUG
      g_print ("%s \n", command_name);
#endif
      keymap_foreach_command_binding (the_keymap, i,
              (GFunc) write_xml_keybinding_info, child);

    }

  xmlSaveFormatFile (filename, doc, 1);

  xmlFreeDoc (doc);
  return ret;
}
