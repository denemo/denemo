/* parseinstruments.cpp
 * Parse instruments for score setup
 * File taken from lily4jedit
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Adam Tee
 */

#include "parseinstruments.h"

#include <string.h>
#include "core/utils.h"


/**
 * Look up instrument group
 *
 * @param string	textual description of instrument group
 *
 * @return enum value for specific group
 */
InstrumentType
lookuptype (gchar * string)
{
  InstrumentType type = NONE;
  if (!strcmp (string, "Woodwinds"))
    type = WOODWIND;
  else if (!strcmp (string, "Brass"))
    type = BRASS;
  else if (!strcmp (string, "Pitched Percussion"))
    type = PITCHEDPERCUSSION;
  else if (!strcmp (string, "Plucked Strings"))
    type = PLUCKEDSTRINGS;
  else if (!strcmp (string, "Keyboards"))
    type = KEYBOARDS;
  else if (!strcmp (string, "Vocals"))
    type = VOCALS;
  else if (!strcmp (string, "Strings"))
    type = STRINGS;

  return type;
}


/**
 * Look up number of staffs instrument uses
 *
 * @param string textual description of number of staves/type
 * @return integer value of the number of staffs
 */
static gint
lookupnumstaffs (gchar * string)
{
  if (!strcmp (string, "single"))
    return 1;
  else if (!strcmp (string, "piano"))
    return 2;
  else if (!strcmp (string, "organ"))
    return 3;

  return 0;
}

/**
 * Parse the instrument description
 *
 * @param doc	document pointer
 * @param cur pointer to the current XML Node
 * @param list pointer to the list of instruments
 *
 * @return list pointer to the updated list
 */
GList *
ParseInstruments (xmlDocPtr doc, xmlNodePtr cur, GList * list)
{
  cur = cur->children;
  while (cur != NULL)
    {

      if (0 == xmlStrcmp (cur->name, (const xmlChar *) "Instrument"))
        {
          g_debug ("%s\n", cur->name);
          InstrumentConfig *config = (InstrumentConfig *) g_malloc0 (sizeof (InstrumentConfig));
          gchar *tmpname = (gchar *) xmlGetProp (cur, (xmlChar *) "name");
          if (tmpname)
            {
              config->name = g_string_new (tmpname);
              g_free (tmpname);
              tmpname = NULL;
            }
          gchar *tmpmidi = (gchar *) xmlGetProp (cur, (xmlChar *) "midiName");
          if (tmpmidi)
            {
              config->midiinstrument = g_string_new (tmpmidi);
              g_free (tmpmidi);
              tmpmidi = NULL;
            }
          gchar *tmptrans = (gchar *) xmlGetProp (cur, (xmlChar *) "transposing");
          if (tmptrans)
            {
              config->transposition = atoi (tmptrans);
              g_free (tmptrans);
              tmptrans = NULL;
            }
          gchar *tmpclef = (gchar *) xmlGetProp (cur, (xmlChar *) "clef");
          if (tmpclef)
            {
              config->sclef = cleftypefromname (tmpclef);
              //g_free(tmpclef);
              //tmpclef = NULL;
            }
          gchar *tmpstaff = (gchar *) xmlGetProp (cur, (xmlChar *) "staffType");
          if (tmpstaff)
            {
              config->numstaffs = lookupnumstaffs (tmpstaff);
              g_free (tmpstaff);
              tmpstaff = NULL;
            }

          g_debug ("Names %s\n", config->name->str);

          list = g_list_append (list, config);
        }
      cur = cur->next;
    }
  return list;
}


/**
 * parse Instruments description file
 * Creates a list containing each instrument type's set of instruments
 *
 * @param instruments pointer to a list to insert each instrument type into
 * @return a pointer to the updated list
 */
GList *
parseInstruments (GList * instruments)
{
  xmlDocPtr doc = NULL;
  xmlNodePtr rootElem;

  static gchar *filename = NULL;
  if (filename == NULL)
    filename = g_build_filename (get_system_data_dir (), "instruments.xml", NULL);

  doc = xmlParseFile (filename);
  if (doc == NULL)
    {
      g_warning ("Could not read XML file %s", filename);
      return instruments;
    }

  rootElem = xmlDocGetRootElement (doc);
  if (rootElem == NULL)
    {
      g_warning ("Empty Document");
      xmlFreeDoc (doc);
      return instruments;
    }

  g_debug ("RootElem: %s\n", rootElem->name);
  if (0 != xmlStrcmp (rootElem->name, (xmlChar*) "Instruments"))
    {
      g_warning ("Document has wrong type");
      xmlFreeDoc (doc);
      return instruments;
    }

  rootElem = rootElem->children;
  while (rootElem != NULL)
    {
      g_debug ("RootElem %s\n", rootElem->name);
      if (0 == xmlStrcmp (rootElem->name, (const xmlChar *) "InstrumentType"))
        {
          InstrumentList *list = (InstrumentList *) g_malloc0 (sizeof (InstrumentList));

          gchar *tmp = (gchar *) xmlGetProp (rootElem, (xmlChar *) "name");
          list->type = lookuptype (tmp);
          g_debug ("Type %s\n", tmp);
          list->instruments = ParseInstruments (doc, rootElem, list->instruments);
          instruments = g_list_append (instruments, list);
        }
      rootElem = rootElem->next;
    }

  xmlFreeDoc (doc);
  return instruments;
}
