#include <libxml/parser.h>
#include <libxml/tree.h>
#include <glib.h>
#include <string.h>

static char *output = "../src/commands.c";
static char *input = "../actions/Default.commands";

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
    }
  g_free (head);

  return base;
}

static void
parseScripts (xmlDocPtr doc, xmlNodePtr cur, gchar * fallback, FILE * fp)
{
  xmlChar *name = NULL, *menupath = NULL, *label = NULL, *tooltip = NULL, *scheme = NULL, *after = NULL;
  GList *menupaths = NULL;
  cur = cur->xmlChildrenNode;
  gboolean is_script = FALSE;
  gboolean hidden = FALSE;

  if (!fp)
    return;

  for (; cur; cur = cur->next)
    {
      if (0 == xmlStrcmp (cur->name, (const xmlChar *) "action"))
        {
          if (cur->xmlChildrenNode == NULL)
            {
              g_warning ("Empty action node found in keymap file\n");
            }
          else
            {
              name = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
              menupaths = NULL;
            }
        }
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "scheme"))
        {
          scheme = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);

          is_script = TRUE;
        }
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "hidden"))
        {
          hidden = TRUE;
        }

      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "menupath"))
        {
          menupath = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
          menupaths = g_list_append (menupaths, menupath);
        }
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "label"))
        {
          label = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
        }
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "after"))
        {
          after = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
        }
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "tooltip"))
        {
          tooltip = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
          /* by convention this is the last of the fields defining a scheme script menu item */
          if (is_script)
            {
              name = name ? name : (xmlChar *) "NoName";
              label = label ? label : (xmlChar *) "No label";
              scheme = scheme ? scheme : (xmlChar *) "";
              tooltip = tooltip ? tooltip : (xmlChar *) "No indication what this done beyond the name and label";
              gboolean new_command = FALSE;

              new_command = TRUE;


              fprintf (fp, "\n/* %s xgettext:no-c-format*/\n", name);
              fprintf (fp, "action = gtk_action_new(\"%s\",_(\"%s\"),/* xgettext:no-c-format*/_(\"%s\"), ", g_strescape ((const gchar *) name, "\\"), g_strescape ((const gchar *) label, "\\"), g_strescape ((const gchar *) tooltip, "\\"));
              fprintf (fp, "get_icon_for_name(\"%s\", \"%s\"));\n", g_strescape ((const gchar *) name, "\\"), g_strescape ((const gchar *) label, "\\"));
              if (after)
                fprintf (fp, "g_object_set_data(G_OBJECT(action), \"after\", (gpointer)\"%s\");\n", g_strescape ((const gchar *) after, "\\"));
              if (hidden)
                fprintf (fp, "g_object_set_data(G_OBJECT(action), \"hidden\",  (gpointer)TRUE) );\n");
              fprintf (fp, "/* xgettext:no-c-format*/\n" "register_command(Denemo.map, action, \"%s\", _(\"%s\"), /* xgettext:no-c-format*/_(\"%s\"), activate_script);\n", g_strescape ((const gchar *) name, "\\"), g_strescape ((const gchar *) label, "\\"), g_strescape ((const gchar *) tooltip, "\\"));
              fprintf (fp, "gtk_action_group_add_action(Denemo.action_group, action);\n");
              fprintf (fp, "create_scheme_function_for_script(\"%s\");\n", g_strescape ((const gchar *) name, "\\"));


              if (menupath)
                {
                  GList *g;
                  for (g = menupaths; g; g = g->next)
                    {
                      menupath = (xmlChar *) g->data;
                      menupath = menupath ? menupath : (xmlChar *) "/MainMenu/Other";

                      if (new_command)
                        {
                          if (after)
                            fprintf (fp, "add_ui(\"%s\", \"%s\", \"%s\");\n", g_strescape ((const gchar *) menupath, "\\"), g_strescape ((const gchar *) after, "\\"), g_strescape ((const gchar *) name, "\\"));
                          else
                            fprintf (fp, "add_ui(\"%s\", NULL, \"%s\");\n", g_strescape ((const gchar *) menupath, "\\"), g_strescape ((const gchar *) name, "\\"));
                        }
                    }
                }
              else
                {
                  if (fallback)
                    {           /* no path given, use fallback */
                      menupath = (xmlChar *) fallback;
                      if (new_command)
                        {
                          if (after)
                            fprintf (fp, "add_ui(\"%s\", \"%s\", \"%s\");\n", g_strescape ((const gchar *) menupath, "\\"), g_strescape ((const gchar *) after, "\\"), g_strescape ((const gchar *) name, "\\"));
                          else
                            fprintf (fp, "add_ui(\"%s\", NULL, \"%s\");\n", g_strescape ((const gchar *) menupath, "\\"), g_strescape ((const gchar *) name, "\\"));
                        }
                    }
                }

              if (new_command)
                {
                  fprintf (fp, "g_object_set_data(G_OBJECT(action), \"scheme\", \"%s\");\n", g_strescape ((const gchar *) scheme, "\\"));
                  if (menupath)
                    fprintf (fp, "g_object_set_data(G_OBJECT(action), \"menupath\", \"%s\");\n", g_strescape ((const gchar *) menupath, "\\"));
                  fprintf (fp, "g_signal_connect (G_OBJECT (action), \"activate\", G_CALLBACK (activate_script), NULL);\n");
                }

            }
        }
    }
}

static void
parseCommands (xmlDocPtr doc, xmlNodePtr cur, gchar * menupath)
{
  xmlNodePtr ncur = cur->xmlChildrenNode;
  FILE *fp = NULL;
  fp = fopen (output, "w");
  if (!fp)
    {
      g_warning ("Could not write to commands.c");
      return;
    }

  fprintf (fp, "//Automatically Generated by Denemo 'generate_commands' tool\n" "#include \"view.h\"\n" "#include \"keyboard.h\"\n" "void install_commands(void) {\n" "DenemoGUI *gui = Denemo.gui;\n" "GtkAction *action;\n");

  for (ncur = cur->xmlChildrenNode; ncur; ncur = ncur->next)
    if ((0 == xmlStrcmp (ncur->name, (const xmlChar *) "row")))
      parseScripts (doc, ncur, menupath, fp);

  fprintf (fp, "}\n");
  fclose (fp);

  g_print ("The new source code file %s has been created\n", output);
  exit (0);
}

gint
load_xml_keymap (gchar * filename)
{

  xmlDocPtr doc;
  xmlNodePtr rootElem;
  if (filename == NULL)
    {
      g_warning ("Filename is null");
      return -1;
    }

  if (g_file_test (filename, G_FILE_TEST_IS_DIR))
    {
      g_warning ("There is no support for loading whole folders of commands yet, sorry");
      return -1;
    }
  if (!g_file_test (filename, G_FILE_TEST_EXISTS))
    {
      g_warning ("File %s does not exist", filename);
      return -1;
    }

  doc = xmlParseFile (filename);
  gchar *menupath = extract_menupath (filename);
  if (doc == NULL)
    {
      g_debug ("Could not read XML file %s", filename);
      return -1;
    }

  rootElem = xmlDocGetRootElement (doc);
  if (rootElem == NULL)
    {
      g_warning ("Empty Document\n");
      xmlFreeDoc (doc);
      return -1;
    }

  if (xmlStrcmp (rootElem->name, (const xmlChar *) "Denemo"))
    {
      g_warning ("Document has wrong type\n");
      xmlFreeDoc (doc);
      return -1;
    }

  rootElem = rootElem->xmlChildrenNode;
  xmlNodePtr cur;
  while (rootElem != NULL)
    {
      for (cur = rootElem->xmlChildrenNode; cur != NULL; cur = cur->next)
        if (0 == xmlStrcmp (cur->name, (const xmlChar *) "map"))
          parseCommands (doc, cur, menupath);

      rootElem = rootElem->next;
    }

  xmlFreeDoc (doc);
  return 0;
}

int
main (int argc, char **argv)
{
  return load_xml_keymap (input);
}
