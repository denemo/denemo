#include <glib.h>
#include <unistd.h>
#include <libxml/parser.h>
#include "keymapio.h"

/* Coverage test suite tries to launch every possible scheme command
 */

#define DENEMO "../src/denemo"
#define DATA_DIR "integration-data"
#define TEMP_DIR "coverage-tmp"
#define BAR "================================================================================\n"

/*******************************************************************************
 * SETUP AND TEARDOWN
 ******************************************************************************/

static void
setup(gpointer fixture, gconstpointer data)
{
  if(!g_file_test(TEMP_DIR, G_FILE_TEST_EXISTS)){
    if(g_mkdir(TEMP_DIR, 0777) < 0)
      g_warning("Could not create " TEMP_DIR);
  }
}

static void
teardown(gpointer fixture, gconstpointer data)
{  
  if(g_file_test(TEMP_DIR, G_FILE_TEST_EXISTS)){
    if(g_remove(TEMP_DIR) < 0)
      g_warning("Could not remove " TEMP_DIR);
  }
}

/*******************************************************************************
 * TEST FUNCTIONS
 ******************************************************************************/

static GList*
get_all_scheme_commands(const gchar* filename){
  xmlDocPtr doc;
  xmlNodePtr rootElem, curElem, curChild;
  xmlKeepBlanksDefault (0);
  GList* commands = NULL;
  gchar* name = NULL;
  
  doc = xmlParseFile (filename);
  if(doc == NULL)
    return NULL;
  
  rootElem = xmlDocGetRootElement (doc);
  if (rootElem == NULL)
  {
    xmlFreeDoc (doc);
    return NULL;
  }

  if (xmlStrcmp (rootElem->name, COMMANDXML_TAG_ROOT))
  {
    xmlFreeDoc (doc);
    return NULL;
  }
  rootElem = rootElem->xmlChildrenNode;

  if (xmlStrcmp (rootElem->name, COMMANDXML_TAG_MERGE))
  {
    xmlFreeDoc (doc);
    return NULL;
  }
  
  for(rootElem = rootElem->xmlChildrenNode; rootElem; rootElem = rootElem->next)
  {
    if (0 == xmlStrcmp (rootElem->name, COMMANDXML_TAG_MAP))
    {
      for (curElem = rootElem->xmlChildrenNode; curElem; curElem = curElem->next)
      {
        if (0 == xmlStrcmp (curElem->name, COMMANDXML_TAG_ROW))
        {
          for (curChild = curElem->xmlChildrenNode; curChild; curChild = curChild->next)
          {
            if (0 == xmlStrcmp (curChild->name, COMMANDXML_TAG_ACTION))
            {
              name = (gchar*) xmlNodeListGetString (doc, curChild->xmlChildrenNode, 1);
              commands = g_list_append(commands, name);
            }
          }
        }
      }
    }
  }
  
  xmlFreeDoc (doc);

  return commands;
}

/** test_all_scheme_functions:
 * Tries to launch denemo with every scheme possibl functions.
 */
static void
test_all_scheme_functions (gpointer fixture, gconstpointer data)
{
  const gchar* input  = DATA_DIR "/blank.denemo";
  GList* commands = get_all_scheme_commands("../actions/Default.commands");
  g_assert(commands != NULL);
  GList* c;
  for(c = commands; c; c = g_list_next(c)){
    if (g_test_trap_fork (0, 0))
    {
      gchar* scheme = g_strdup_printf("(d-%s)(d-Quit)", c->data);
      g_print(BAR "Testing %s\ndenemo -n -a \"%s\"\n" BAR, c->data, scheme);
      execl(DENEMO, DENEMO, "-n", "-a", scheme, input, NULL);
      g_warn_if_reached ();
    }
    g_test_trap_assert_passed ();
  }
}

/*******************************************************************************
 * MAIN
 ******************************************************************************/

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  if(!g_file_test(DENEMO, G_FILE_TEST_EXISTS))
    g_error("Denemo has not been compiled successfully");

  g_test_add ("/coverage/all-scheme-functions", void, NULL, setup, test_all_scheme_functions, teardown);

  return g_test_run ();
}