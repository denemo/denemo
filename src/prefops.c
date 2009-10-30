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
#include <string.h>
#include "config.h"
#include <denemo/denemo.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include "utils.h"
#include "prefops.h"

static gint
readxmlprefsFile (gchar * filename);

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
  #define ret (&Denemo.prefs)
  gchar * dotdenemo = (gchar*)locatedotdenemo ();
  gchar *localrc = dotdenemo?g_build_filename (dotdenemo, "denemorc", NULL):NULL;

  /* Reasonable default values */

  ret->csoundcommand = g_string_new ("csound -dm6");

  ret->csoundorcfile = g_string_new ("");
  const gchar *name = g_get_user_name();
  ret->username = g_string_new (name?name:"DenemoUser");
  ret->password = g_string_new ("");
  //char midi_audio_string[][24] = {"portaudio", "jack", "fluidsynth"};
//#ifdef _HAVE_PORTAUDIO_
  ret->midi_audio_output = PORTAUDIO;
//#endif
#ifdef _HAVE_JACK_
  ret->midi_audio_output = JACK;
#endif
#ifdef _HAVE_FLUIDSYNTH_ 
  ret->midi_audio_output = FLUIDSYNTH;	  
#endif
#ifdef G_OS_WIN32
  ret->browser = g_string_new ("firefox");
  ret->midiplayer = g_string_new ("wmplayer");
  ret->audioplayer = g_string_new ("wmplayer");
  ret->lilypath = g_string_new ("lilypond-windows");
  ret->pdfviewer = g_string_new ("acrord32");
  ret->imageviewer = g_string_new ("mspaint");
  ret->texteditor = g_string_new ("wordpad");
  ret->midiplayer = g_string_new("wmplayer");
#else /* !G_OS_WIN32 */
  ret->browser = g_string_new ("firefox");
  ret->midiplayer = g_string_new ("playmidi");
  ret->audioplayer = g_string_new ("play");
  ret->lilypath = g_string_new ("lilypond");
  ret->pdfviewer = g_string_new ("xpdf");
  ret->imageviewer = g_string_new ("eog");
  ret->texteditor = g_string_new ("xedit");
#endif /* !G_OS_WIN32 */
  ret->sequencer = g_string_new ("/dev/sequencer");
  ret->midi_in = g_string_new ("/dev/midi");

  ret->denemopath = g_string_new (g_get_home_dir());
  ret->lilyversion = g_string_new (LILYPOND_VERSION);
  ret->temperament = g_string_new("Equal");
  ret->strictshortcuts = FALSE;
  ret->resolution = 300;
  ret->overlays = FALSE;
  ret->continuous = TRUE;
#ifdef _HAVE_JACK_
  ret->immediateplayback = FALSE;
#else
  ret->immediateplayback = TRUE;
#endif
#ifdef _HAVE_FLUIDSYNTH_
  /*TODO needs to check if linux and set fluidsynth_audio_driver = alsa
  	for some reason the default for linux is jack */
  ret->fluidsynth_audio_driver = g_string_new (fluidsynth_get_default_audio_driver());
  ret->fluidsynth_soundfont = g_string_new("/usr/share/sounds/sf2/FluidR3_GM.sf2");
#endif
  ret->saveparts = FALSE;
  ret->rtcs = TRUE;
  ret->playbackoutput = FALSE;
  ret->lilyentrystyle = FALSE;
  ret->createclones = FALSE;
  ret->autosave = TRUE;
  ret->autosave_timeout = 5;
  ret->maxhistory = 20;
  ret->notation_palette = TRUE;
  ret->articulation_palette = FALSE;
  ret->visible_directive_buttons = TRUE;
  ret->autoupdate = FALSE;
  ret->rhythm_palette = TRUE;
  ret->object_palette = TRUE;
  ret->history = g_queue_new ();

  /* Read values from systemwide preferences file */

  readxmlprefsFile (systemwide);

  /* Read values from personal preferences file */

  //readpreffile (localrc, ret);
  if(localrc) {
    if(g_file_test(localrc,  G_FILE_TEST_EXISTS))
      readxmlprefsFile (localrc);
    else
      writeXMLPrefs(ret);
  }
  g_free (systemwide);
  g_free (localrc);
#undef ret
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
#define READXMLENTRY(field)  \
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) #field))\
	{\
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);\
	  if(tmp)\
	    {\
              define_scheme_variable("DenemoPref_" #field, tmp, NULL);\
	      prefs->field =\
		g_string_assign (prefs->field, (gchar *) tmp);\
	      xmlFree (tmp);\
	    }\
	}

#define READINTXMLENTRY(field) \
      else if (0 ==\
	       xmlStrcmp (cur->name, (const xmlChar *) #field))\
	{\
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);\
	  if(tmp)\
	    {\
              define_scheme_variable("DenemoPref_" #field, tmp, NULL);\
	      prefs->field = atoi ((gchar *) tmp);\
	      xmlFree (tmp);\
	    }\
	}\

      if (0 == xmlStrcmp (cur->name, (const xmlChar *) "lilypath"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      gchar *curname = g_strdup_printf("DenemoPref_%s", cur->name);
	      define_scheme_variable(curname, tmp, NULL); 
	      g_free(curname);
	      prefs->lilypath = 
		g_string_assign (prefs->lilypath, (gchar *) tmp);
	      //g_print ("Lilypond Path %s\n", tmp);
	      xmlFree (tmp);
	    }
	}
	READXMLENTRY(midiplayer)      
	READXMLENTRY(audioplayer)        
	READXMLENTRY(browser)
        else if (0 ==
	       xmlStrcmp (cur->name, (const xmlChar *) "autosavetimeout"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      gchar *curname = g_strdup_printf("DenemoPref_%s", cur->name);
	      define_scheme_variable(curname, tmp, NULL); 
	      g_free(curname);
	      prefs->autosave_timeout = atoi ((gchar *) tmp);
	      if(prefs->autosave_timeout <1) prefs->autosave_timeout = 1;
	      //g_print ("Autosave Timeout %s\n", tmp);
	      xmlFree (tmp);
	    }
	}
      else if (0 ==
	       xmlStrcmp (cur->name, (const xmlChar *) "maxhistory"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      gchar *curname = g_strdup_printf("DenemoPref_%s", cur->name);
	      define_scheme_variable(curname, tmp, NULL); 
	      g_free(curname);
	      prefs->maxhistory = atoi ((gchar *) tmp);
	      if(prefs->maxhistory <1) prefs->maxhistory = 1;
	      xmlFree (tmp);
	    }
	}
      READXMLENTRY(csoundcommand)
      READXMLENTRY(csoundorcfile)
	
      READINTXMLENTRY(rtcs)
      READINTXMLENTRY(autosave)
      
      READXMLENTRY(pdfviewer)
      READXMLENTRY(imageviewer)    
      READXMLENTRY(username)    
      READXMLENTRY(password)           
      READXMLENTRY(texteditor)            
      READXMLENTRY(denemopath)          
      READXMLENTRY(temperament)
      READXMLENTRY(midi_in)
      READXMLENTRY(sequencer)
      
      READINTXMLENTRY(createclones)
      READINTXMLENTRY(immediateplayback)   
      READINTXMLENTRY(strictshortcuts)
      READINTXMLENTRY(resolution)
      READINTXMLENTRY(overlays)
      READINTXMLENTRY(continuous)
      READINTXMLENTRY(jacktransport)
      READINTXMLENTRY(jacktransport_start_stopped)
      READINTXMLENTRY(jack_at_startup)
      READINTXMLENTRY(lilyentrystyle)
      READINTXMLENTRY(notation_palette)
      READINTXMLENTRY(articulation_palette)
      READINTXMLENTRY(visible_directive_buttons)
      READINTXMLENTRY(rhythm_palette) 
      READINTXMLENTRY(object_palette)
      READINTXMLENTRY(autoupdate) 
     
      READXMLENTRY(fluidsynth_audio_driver)
      READXMLENTRY(fluidsynth_soundfont)
      READINTXMLENTRY(fluidsynth_reverb)
      READINTXMLENTRY(fluidsynth_chorus)

      READXMLENTRY(lilyversion) 
      READINTXMLENTRY(saveparts)
      
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
  //g_print ("filename %s\n", (gchar *) data);
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


static gint
readxmlprefs (gchar * xmlsource, gboolean from_file);

/**
 * Read a denemo preferences xml file.
 * @param filename - denemorc file name 
 * @param prefs - struct to populate data into
 *
 */

static gint
readxmlprefsFile (gchar * filename) {
  return readxmlprefs(filename,  TRUE);
}

/**
 * Read denemo preferences from an xml string.
 * @param content - a string containing prefs in xml 
 *
 */
gint
readxmlprefsString (gchar * content) {
  gchar *xml = g_strconcat("<?xml version=\"1.0\"?><Denemo><Config>", content, "</Config></Denemo>", NULL);
  gint ret= readxmlprefs(xml, FALSE);
  g_free(xml);
  return ret;
}



static gint
readxmlprefs (gchar * xmlsource,  gboolean from_file)
{
  DenemoPrefs * prefs = &Denemo.prefs;
  gint ret = -1;
  xmlDocPtr doc = NULL;
  xmlNodePtr rootElem;

  if(from_file) 
    doc = xmlParseFile (xmlsource);
  else
    doc = xmlReadMemory(xmlsource, strlen(xmlsource), "noname.xml", NULL, 0);
  if (doc == NULL)
    {
      g_warning ("Could not read XML %s %s\n", from_file?"File: ":":\n", xmlsource);
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

#define WRITEXMLENTRY(field) \
  if (prefs->field){\
    gchar *def = g_strdup("Holds the value of the user's " #field " preference");\
    gchar *curname = g_strdup_printf("DenemoPref_%s", #field);\
    define_scheme_variable(curname, prefs->field->str, def);\
    g_free(curname);\
    g_free(def);\
    xmlNewChild (child, NULL, (xmlChar *) #field,\
		 (xmlChar *) prefs->field->str);}

  WRITEXMLENTRY(lilypath)
  WRITEXMLENTRY(midiplayer)
  WRITEXMLENTRY(audioplayer)
  WRITEXMLENTRY(csoundcommand)
  WRITEXMLENTRY(csoundorcfile)  
  WRITEXMLENTRY(pdfviewer)
  WRITEXMLENTRY(imageviewer)
  WRITEXMLENTRY(username)
  WRITEXMLENTRY(password)
  WRITEXMLENTRY(texteditor) 
  WRITEXMLENTRY(denemopath)
  WRITEXMLENTRY(temperament)
  WRITEXMLENTRY(midi_in)
  WRITEXMLENTRY(sequencer)
  WRITEXMLENTRY(lilyversion)
 
#define WRITEINTXMLENTRY(field){ \
    gchar *def = g_strdup("Holds the value of the user's " #field " preference");\
    gchar *value = g_strdup_printf("%d", prefs->field);\
    gchar *curname = g_strdup_printf("DenemoPref_%s", #field);\
    define_scheme_variable(curname, value, def);\
    g_free(curname);\
    g_free(value);\
    g_free(def);\
  newXMLIntChild (child, (xmlChar *) #field,\
		  prefs->field);}

  WRITEINTXMLENTRY(autosave)
  WRITEINTXMLENTRY(autosave_timeout)
  WRITEINTXMLENTRY(maxhistory)
  WRITEINTXMLENTRY(saveparts)
  WRITEINTXMLENTRY(createclones)
  WRITEINTXMLENTRY(lilyentrystyle)
  WRITEINTXMLENTRY(immediateplayback)
  WRITEINTXMLENTRY(strictshortcuts)
  WRITEINTXMLENTRY(resolution)
  WRITEINTXMLENTRY(overlays)
  WRITEINTXMLENTRY(continuous)
  WRITEINTXMLENTRY(jacktransport)
  WRITEINTXMLENTRY(jacktransport_start_stopped)
  WRITEINTXMLENTRY(jack_at_startup)
  WRITEINTXMLENTRY(rtcs)
  WRITEINTXMLENTRY(notation_palette)
  WRITEINTXMLENTRY(articulation_palette)
  WRITEINTXMLENTRY(visible_directive_buttons)
  WRITEINTXMLENTRY(autoupdate)
  WRITEINTXMLENTRY(rhythm_palette)
  WRITEINTXMLENTRY(object_palette)

  WRITEXMLENTRY(fluidsynth_audio_driver)
  WRITEXMLENTRY(fluidsynth_soundfont)
  WRITEINTXMLENTRY(fluidsynth_reverb)
  WRITEINTXMLENTRY(fluidsynth_chorus)

  WRITEXMLENTRY(browser) 
  
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
   gtk_window_get_size ( GTK_WINDOW (Denemo.window), &(Denemo.width), &(Denemo.height));
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
    gboolean maximized=FALSE;
    GError *err=NULL;
    filename = g_build_filename (locatedotdenemo (), "state.ini", NULL);
    keyfile = g_key_file_new ();
    if (g_key_file_load_from_file (keyfile, filename, G_KEY_FILE_NONE, NULL) == FALSE) {
        g_free (filename);
        w = INITIAL_WIDTH;
        h = INITIAL_HEIGHT;
	maximized = FALSE;
    } else {
        g_free (filename);
        w = g_key_file_get_integer (keyfile, "State", "width", &err);
        if (err != NULL) {
            w = INITIAL_WIDTH;
            g_error_free (err);
            err = NULL;
        }
        h = g_key_file_get_integer (keyfile, "State", "height", &err);
        if (err != NULL) {
            h = INITIAL_HEIGHT;
            g_error_free (err);
            err = NULL;
        }
        maximized = g_key_file_get_boolean (keyfile, "State", "maximized", &err);
        if (err != NULL) {
	  maximized = FALSE;
	  g_error_free (err);
	  err = NULL;
        }

        g_key_file_free (keyfile);
	Denemo.width = (w<=0?INITIAL_WIDTH:w);
	Denemo.height = (h<=0?INITIAL_HEIGHT:h);
	gtk_window_set_default_size (GTK_WINDOW (Denemo.window), Denemo.width, Denemo.height);
        if ((Denemo.maximized=maximized)) {
	  gtk_window_maximize (GTK_WINDOW (Denemo.window));
        } else
	  gtk_window_unmaximize (GTK_WINDOW (Denemo.window));
    }
}
