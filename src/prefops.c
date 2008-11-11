/* prefops.c
 * Functions for initializing preferences
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */

#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include "config.h"
#include <denemo/denemo.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include "utils.h"
#include "prefops.h"

static gint
readxmlprefs (gchar * filename, DenemoPrefs * prefs);

/**
 * This checks to see if there's a .denemo/ directory in the user's
 * home directory, tries to create one if there isn't, and returns the
 * path to it
 *
 * .denemo/ is used for holding configuration files, templates, and so on.
 *
 * I suspect that this may have some Windows portability issues,
 * but I'm not sure on this. 
 *
 */

const gchar *
locatedotdenemo ()
{
  static gchar *dotdenemo = NULL;

  gboolean err;
  if (!dotdenemo)
    {
      dotdenemo = g_build_filename (g_get_home_dir(), ".denemo", NULL);
    }
  err = g_mkdir_with_parents(dotdenemo, 0770);
  if(err) {
    warningdialog("Could not create .denemo for you personal settings");
    g_free(dotdenemo);
    dotdenemo = NULL;
  }

  return dotdenemo;
}


/**
 * Initialise user preferences to reasonable defaults 
 * read global denemorc file
 * then local preferences file
 *
 */
void
initprefs ()
{
  gchar *systemwide = g_build_filename (get_conf_dir (), "denemo.conf", NULL);
  DenemoPrefs *ret = &Denemo.prefs;
  gchar * dotdenemo = locatedotdenemo ();
  gchar *localrc = dotdenemo?g_build_filename (dotdenemo, "denemorc", NULL):NULL;

  /* Reasonable default values */

  ret->lilypath = g_string_new ("lilypond");
  ret->midiplayer = g_string_new ("playmidi");
  ret->audioplayer = g_string_new ("play");
  ret->csoundcommand = g_string_new ("csound -dm6");
  ret->browser = g_string_new ("firefox");
  ret->csoundorcfile = g_string_new ("");
  ret->pdfviewer = g_string_new ("xpdf");
  ret->sequencer = g_string_new ("/dev/sequencer");
  ret->midi_in = g_string_new ("/dev/midi");


  ret->imageviewer = g_string_new ("eog");
  ret->texteditor = g_string_new ("xedit");
  ret->denemopath = g_string_new (g_get_home_dir());
  ret->lilyversion = g_string_new (LILYPOND_VERSION);
  ret->temperament = g_string_new("Equal");
  ret->strictshortcuts = FALSE;
  ret->overlays = FALSE;
  ret->continuous = TRUE;

  ret->immediateplayback = TRUE;
  ret->saveparts = FALSE;
  ret->rtcs = TRUE;
  ret->playbackoutput = FALSE;
  ret->lilyentrystyle = FALSE;
  ret->createclones = FALSE;
  ret->autosave = TRUE;
  ret->autosave_timeout = 5;
  ret->notation_palette = TRUE;
  ret->articulation_palette = FALSE;
  ret->rhythm_palette = TRUE;
  ret->history = g_queue_new ();

  /* Read values from systemwide preferences file */

  readxmlprefs (systemwide, ret);

  /* Read values from personal preferences file */

  //readpreffile (localrc, ret);
  if(localrc) {
    if(g_file_test(localrc,  G_FILE_TEST_EXISTS))
      readxmlprefs (localrc, ret);
    else
      writeXMLPrefs(ret);
  }
  g_free (systemwide);
  g_free (localrc);
}


/*
 *  Local function definitions to parse denemorc file
 */

/**
 * parseConfig searches the rc file for the configuration settings.
 *
 * @param doc document pointer
 * @param cur pointer to the current XML Node
 * @param prefs pointer to the preferences structure
 *
 */
static void
parseConfig (xmlDocPtr doc, xmlNodePtr cur, DenemoPrefs * prefs)
{
  cur = cur->xmlChildrenNode;
  while (cur != NULL)
    {
      if (0 == xmlStrcmp (cur->name, (const xmlChar *) "lilypondpath"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->lilypath = 
		g_string_assign (prefs->lilypath, (gchar *) tmp);
	      g_print ("Lilypond Path %s\n", tmp);
	      xmlFree (tmp);
	    }
	}
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "midiplayer"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->midiplayer =
		g_string_assign (prefs->midiplayer, (gchar *) tmp);
	      g_print ("midiplayer %s\n", tmp);
	      xmlFree (tmp);
	    }
	}
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "audioplayer"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->audioplayer =
		g_string_assign (prefs->audioplayer, (gchar *) tmp);
	      g_print ("Audioplayer %s\n", tmp);
	      xmlFree (tmp);
	    }
	}
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "browser"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->browser = g_string_assign (prefs->browser, (gchar *) tmp);
	      g_print ("Help Browser %s\n", tmp);
	      xmlFree (tmp);
	    }
	}
      else if (0 ==
	       xmlStrcmp (cur->name, (const xmlChar *) "autosavetimeout"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->autosave_timeout = atoi ((gchar *) tmp);
	      if(prefs->autosave_timeout <1) prefs->autosave_timeout = 1;
	      g_print ("Autosave Timeout %s\n", tmp);
	      xmlFree (tmp);
	    }
	}
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "csoundcommand"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->csoundcommand =
		g_string_assign (prefs->csoundcommand, (gchar *) tmp);
	      g_print ("Csound Command %s\n", tmp);
	      xmlFree (tmp);
	    }
	}
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "csoundorcfile"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if (tmp != NULL)
	    {
	      prefs->csoundorcfile =
		g_string_assign (prefs->csoundorcfile, (gchar *) tmp);
	      g_print ("Csound Orchestrafile %s\n", tmp);
	      xmlFree (tmp);
	    }

	}
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "rtcs"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->rtcs = atoi ((gchar *) tmp);
	      xmlFree (tmp);
	    }

	}

      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "autosave"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->autosave = atoi ((gchar *) tmp);
	      xmlFree (tmp);
	    }
	}
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "pdfviewer"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->pdfviewer =
		g_string_assign (prefs->pdfviewer, (gchar *) tmp);
	      xmlFree (tmp);
	    }

	}
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "imageviewer"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->imageviewer =
		g_string_assign (prefs->imageviewer, (gchar *) tmp);
	      xmlFree (tmp);
	    }

	}
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "texteditor"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->texteditor =
		g_string_assign (prefs->texteditor, (gchar *) tmp);
	      xmlFree (tmp);
	    }
	}

      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "denemopath"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->denemopath =
		g_string_assign (prefs->denemopath, (gchar *) tmp);
	      xmlFree (tmp);
	    }

	}
#define READXMLENTRY(field)       else if (0 == xmlStrcmp (cur->name, (const xmlChar *) #field))\
	{\
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);\
	  if(tmp)\
	    {\
	      prefs->field =\
		g_string_assign (prefs->field, (gchar *) tmp);\
	      xmlFree (tmp);\
	    }\
	}

      READXMLENTRY(temperament)
      READXMLENTRY(midi_in)
      READXMLENTRY(sequencer)

      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "createclones"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->createclones = atoi ((gchar *) tmp);
	      xmlFree (tmp);
	    }
	}

      else if (0 ==
	       xmlStrcmp (cur->name, (const xmlChar *) "immediateplayback"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->immediateplayback = atoi ((gchar *) tmp);
	      xmlFree (tmp);
	    }
	}

#define READINTXMLENTRY(field) \
      else if (0 ==\
	       xmlStrcmp (cur->name, (const xmlChar *) #field))\
	{\
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);\
	  if(tmp)\
	    {\
	      prefs->field = atoi ((gchar *) tmp);\
	      xmlFree (tmp);\
	    }\
	}\



      READINTXMLENTRY(strictshortcuts)
      READINTXMLENTRY(overlays)
      READINTXMLENTRY(continuous)

      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "lilystyleentry"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->lilyentrystyle = atoi ((gchar *) tmp);
	      xmlFree (tmp);
	    }
	}
      else if (0 ==
	       xmlStrcmp (cur->name, (const xmlChar *) "notationpalette"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->notation_palette = atoi ((gchar *) tmp);
	      xmlFree (tmp);
	    }
	}
      else if (0 ==
	       xmlStrcmp (cur->name, (const xmlChar *) "articulationpalette"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->articulation_palette = atoi ((gchar *) tmp);
	      xmlFree (tmp);
	    }
	}
      else if (0 ==
	       xmlStrcmp (cur->name, (const xmlChar *) "rhythmpalette"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->rhythm_palette = atoi ((gchar *) tmp);
	      xmlFree (tmp);
	    }
	}
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "lilyversion"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->lilyversion =
		g_string_assign (prefs->lilyversion, (gchar *) tmp);
	      xmlFree (tmp);
	    }
	}
      else if(0 == xmlStrcmp(cur->name, (const xmlChar *) "saveparts"))
	{
	  xmlChar *tmp = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      prefs->saveparts = atoi((gchar *) tmp);
	      g_print ("Autosave Parts %s\n", tmp);
	      xmlFree(tmp);	
	    }
	}
      cur = cur->next;
    }
  return;
}

/**
 * writeHistoryEntry - adds history entry to the denemohistory file
 * @param data - pointer to the filename to add
 * @param user_data	- pointer to the xml Node
 */
static void
writeHistoryEntry (gpointer data, gpointer user_data)
{
  g_print ("filename %s\n", (gchar *) data);
  xmlNewTextChild ((xmlNodePtr) user_data, NULL, (xmlChar *) "file",
		   (xmlChar *) data);
}

/**
 * parseHistory - reads history entry from xml node and adds it to the history queue
 * 
 * @param  doc	document pointer
 * @param cur pointer to the current XML Node
 * @param prefs pointer to the preferences structure
 */

static void
parseHistory (xmlDocPtr doc, xmlNodePtr cur, DenemoPrefs * prefs)
{
  cur = cur->xmlChildrenNode;
  while (cur != NULL)
    {
      if (xmlStrcmp (cur->name, (const xmlChar *) "file") == 0)
	{
	  gsize read = 0, written = 0;
	  GError *err = NULL;
	  gchar *tmp = NULL;
	  tmp = (gchar *) xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if (tmp)
	    {
	      gchar *file =
		g_filename_to_utf8 (tmp, -1, &read, &written, &err);
	      if (err != NULL)
		{
		  file = "Unknown file name";
		  g_warning ("%s", err->message);
		  g_error_free (err);
		}
#ifdef DEBUG
	      g_print ("Filename %s\n", tmp);
#endif

	      g_queue_push_tail (prefs->history, g_strdup(file));
	      g_free (tmp);
	    }
	}

      cur = cur->next;
    }
  return;
}

/**
 * Top-level function to read the denemotc xml file.
 * @param filename - denemorc file name 
 * @param prefs - struct to populate data into

 *
 */
static gint
readxmlprefs (gchar * filename, DenemoPrefs * prefs)
{
  gint ret = -1;
  xmlDocPtr doc = NULL;
  xmlNodePtr rootElem;


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
      g_print ("RootElem 2 %s\n", rootElem->name);
#endif

      if (0 == xmlStrcmp (rootElem->name, (const xmlChar *) "Config"))
	{
	  
	    parseConfig (doc, rootElem, prefs);
	}

      rootElem = rootElem->next;

    }

  xmlFreeDoc (doc);

  ret = 0;
  return ret;
}

/**
 * Output an integer child as a child of the given node.
 *
 * @param parent - pointer to child nodes parent
 * @param name -  name for the child node
 * @param content - data to add to the child node
 */
static xmlNodePtr
newXMLIntChild (xmlNodePtr parent, const xmlChar * name, gint content)
{
  static gchar sIntBuf[12];	/* enough for -2000000000 + '\0' */
  sprintf (sIntBuf, "%d", content);
  return xmlNewChild (parent, NULL, name, (xmlChar *) sIntBuf);
}

/**
 * Write the denemorc file
 *
 * @param prefs a pointer to the preferences structure
 *
 */

gint
writeXMLPrefs (DenemoPrefs * prefs)
{
  gint ret = -1;
  xmlDocPtr doc;
  xmlNodePtr parent, child;
  static GString *localrc = NULL;

  if (!localrc)
    {
      localrc = g_string_new (locatedotdenemo ());
      g_string_append (localrc, "/denemorc");
    }

  doc = xmlNewDoc ((xmlChar *) "1.0");
  doc->xmlRootNode = parent =
    xmlNewDocNode (doc, NULL, (xmlChar *) "Denemo", NULL);
  child = xmlNewChild (parent, NULL, (xmlChar *) "Config", NULL);

  if (prefs->lilypath)
    xmlNewChild (child, NULL, (xmlChar *) "lilypondpath",
		 (xmlChar *) prefs->lilypath->str);
  if (prefs->lilypath)
    xmlNewChild (child, NULL, (xmlChar *) "midiplayer",
		 (xmlChar *) prefs->midiplayer->str);
  if (prefs->audioplayer)
    xmlNewChild (child, NULL, (xmlChar *) "audioplayer",
		 (xmlChar *) prefs->audioplayer->str);
  if (prefs->csoundcommand)
    xmlNewChild (child, NULL, (xmlChar *) "csoundcommand",
		 (xmlChar *) prefs->csoundcommand->str);
  if (prefs->csoundorcfile)
    xmlNewChild (child, NULL, (xmlChar *) "csoundorcfile",
		 (xmlChar *) prefs->csoundorcfile->str);
  if (prefs->pdfviewer)
    xmlNewChild (child, NULL, (xmlChar *) "pdfviewer",
		 (xmlChar *) prefs->pdfviewer->str);
  if (prefs->imageviewer)
    xmlNewChild (child, NULL, (xmlChar *) "imageviewer",
		 (xmlChar *) prefs->imageviewer->str);
  if (prefs->texteditor)
    xmlNewChild (child, NULL, (xmlChar *) "texteditor",
		 (xmlChar *) prefs->texteditor->str);
  if (prefs->denemopath)
    xmlNewChild (child, NULL, (xmlChar *) "denemopath",
		 (xmlChar *) prefs->denemopath->str);
#define WRITEXMLENTRY(field) \
  if (prefs->field)\
    xmlNewChild (child, NULL, (xmlChar *) #field,\
		 (xmlChar *) prefs->field->str)

  WRITEXMLENTRY(temperament);
  WRITEXMLENTRY(midi_in);
  WRITEXMLENTRY(sequencer);

  if (prefs->lilyversion)
    xmlNewChild (child, NULL, (xmlChar *) "lilyversion",
		 (xmlChar *) prefs->lilyversion->str);
  newXMLIntChild (child, (xmlChar *) "autosave", prefs->autosave);
  newXMLIntChild (child, (xmlChar *) "autosavetimeout",
		  prefs->autosave_timeout);
  newXMLIntChild (child, (xmlChar *) "saveparts", prefs->saveparts);
  newXMLIntChild (child, (xmlChar *) "createclones", prefs->createclones);
  newXMLIntChild (child, (xmlChar *) "lilystyleentry", prefs->lilyentrystyle);
  newXMLIntChild (child, (xmlChar *) "immediateplayback",
		  prefs->immediateplayback);

#define WRITEINTXMLENTRY(field) \
  newXMLIntChild (child, (xmlChar *) #field,\
		  prefs->field);

  WRITEINTXMLENTRY(strictshortcuts);
  WRITEINTXMLENTRY(overlays);
  WRITEINTXMLENTRY(continuous);

  newXMLIntChild (child, (xmlChar *) "rtcs", prefs->rtcs);
  newXMLIntChild (child, (xmlChar *) "notationpalette",
		  prefs->notation_palette);
  newXMLIntChild (child, (xmlChar *) "articulationpalette",
		  prefs->articulation_palette);
  newXMLIntChild (child, (xmlChar *) "rhythmpalette",
		  prefs->rhythm_palette);
  if (prefs->browser)
    xmlNewChild (child, NULL, (xmlChar *) "browser",
		 (xmlChar *) prefs->browser->str);



  xmlSaveFormatFile (localrc->str, doc, 1);
  xmlFreeDoc (doc);
  ret = 0;
  return ret;
}

/**
 * Read denemohistory file
 *
 * @param prefs	pointer to the preferences structure 
 *
 */
gint
readHistory ()
{
  gint ret = -1;
  xmlDocPtr doc = NULL;
  xmlNodePtr rootElem;

  static GString *filename = NULL;
  if (!filename)
    {
      filename = g_string_new (locatedotdenemo ());
      g_string_append (filename, "/denemohistory");
    }
  doc = xmlParseFile (filename->str);

  if (doc == NULL)
    {
      g_warning ("Could not read XML file %s", filename->str);
      xmlSaveFormatFile ((gchar *) filename->str, doc, 0);
      return ret;
    }

  rootElem = xmlDocGetRootElement (doc);
  if (rootElem == NULL)
    {
      g_warning ("Empty Document\n");
      xmlFreeDoc (doc);
      return ret;
    }
#ifdef DEBUG
  g_print ("RootElem %s\n", rootElem->name);
#endif
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
      g_print ("RootElem 2 %s\n", rootElem->name);
#endif
      if (0 == xmlStrcmp (rootElem->name, (const xmlChar *) "History"))
	{
	  parseHistory (doc, rootElem, &Denemo.prefs);
	}

      rootElem = rootElem->next;

    }

  xmlFreeDoc (doc);

  ret = 0;
  return ret;

}

/**
 * Write denemohistory file
 *
 * @param none
 *
 */
void
writeHistory (void)
{


  xmlDocPtr doc;
  xmlNodePtr parent, child;
  static GString *filename = NULL;
  if (!filename)
    {
      filename = g_string_new (locatedotdenemo ());
      g_string_append (filename, "/denemohistory");
    }

  doc = xmlNewDoc ((xmlChar *) "1.0");
  doc->xmlRootNode = parent =
    xmlNewDocNode (doc, NULL, (xmlChar *) "Denemo", NULL);
  child = xmlNewChild (parent, NULL, (xmlChar *) "History", NULL);
  g_queue_foreach (Denemo.prefs.history, writeHistoryEntry, child);
  xmlSaveFormatFile (filename->str, doc, 0);
  xmlFreeDoc (doc);

}

/**
 * Store default window position size etc
 *
 *
 */
void storeWindowState (void)
{
   GKeyFile *keyfile;
   gchar *contents;
   gchar *filename;
   gtk_window_get_size (Denemo.window, &(Denemo.width), &(Denemo.height));
   keyfile = g_key_file_new ();
   g_key_file_set_integer (keyfile, "State", "width", Denemo.width);
   g_key_file_set_integer (keyfile, "State", "height", Denemo.height);
   g_key_file_set_boolean (keyfile, "State", "maximized", Denemo.maximized);
   contents = g_key_file_to_data (keyfile, NULL, NULL);
   g_key_file_free (keyfile);
   filename = g_build_filename (locatedotdenemo (), "state.ini", NULL);
   g_file_set_contents (filename, contents, -1, NULL);
   g_free (filename);
   g_free (contents); 
}

/**
 * Load default window position
 *
 * @param the denemo gui
 *
 */
void loadWindowState (void)
{
    gchar *filename;
    GKeyFile *keyfile;
    gint w,h;
    gboolean maximized;
    GError *err=NULL;

    filename = g_build_filename (locatedotdenemo (), "state.ini", NULL);
    keyfile = g_key_file_new ();
    if (g_key_file_load_from_file (keyfile, filename, G_KEY_FILE_NONE, NULL) == FALSE) {
        g_free (filename);
        w = INITIAL_WIDTH;
        h = INITIAL_HEIGHT;
    } else {
        g_free (filename);
        w = g_key_file_get_integer (keyfile, "State", "width", &err);
        if (err != NULL) {
            w = 0;
            g_error_free (err);
            err = NULL;
        }
        h = g_key_file_get_integer (keyfile, "State", "height", &err);
        if (err != NULL) {
            h = 0;
            g_error_free (err);
            err = NULL;
        }
        maximized = g_key_file_get_boolean (keyfile, "State", "maximized", &err);
        if (err != NULL) {
            g_error_free (err);
            err = NULL;
        }
        g_key_file_free (keyfile);
        if (w > 0 && h > 0 && maximized == FALSE) {
                gtk_window_set_default_size (GTK_WINDOW (Denemo.window), w, h);
                Denemo.width = w;
                Denemo.height = h;
        } else if (Denemo.maximized != FALSE) {
                gtk_window_maximize (GTK_WINDOW (Denemo.window));
        }
    }
}
