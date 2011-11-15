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
#include "playback.h"

#ifdef G_OS_WIN32
#define PREFS_FILE "denemorcV2"
#else
#define PREFS_FILE "denemorc"
#endif



static gint
readxmlprefsFile (gchar * filename);

/**
 * This checks to see if there's a .denemo/ directory in the user's
 * home directory, tries to create one if there isn't, and returns the
 * path to it
 *
 * .denemo/ is used for holding configuration files, templates, and so on.
 *
 * On windows the home directory is the one containing the My Documents folder.
 */

const gchar *
locatedotdenemo ()
{
  static gchar *dotdenemo = NULL;

  gboolean err;
  if (!dotdenemo)
    {
      dotdenemo = g_build_filename (g_get_home_dir(), ".denemo-"PACKAGE_VERSION, NULL);
    }
  err = g_mkdir_with_parents(dotdenemo, 0770);
  if(err) {
    warningdialog("Could not create .denemo for you personal settings");
    g_free(dotdenemo);
    dotdenemo = NULL;
  }

  return dotdenemo;
}

/* return a path to a temporary directory to be used for print intermediate files */
const gchar *
locateprintdir (void)
{
  static gchar *printdir = NULL;
  if (!printdir)
    printdir = make_temp_dir();
  return printdir;
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
  gchar *localrc = dotdenemo?g_build_filename (dotdenemo, PREFS_FILE, NULL):NULL;

  /* Reasonable default values */

  ret->mode = INPUTEDIT|INPUTRHYTHM|INPUTNORMAL;

  const gchar *name = g_get_user_name();
  ret->username = g_string_new (name?name:"DenemoUser");
  ret->password = g_string_new ("");
  ret->midi_audio_output = None;

#ifdef _HAVE_PORTAUDIO_
  ret->midi_audio_output = Portaudio;
#endif

#ifdef _HAVE_JACK_
  ret->midi_audio_output = Jack;
#endif

#ifdef _HAVE_FLUIDSYNTH_ 
  ret->midi_audio_output = Fluidsynth;	  
#endif
  ret->fontspec = g_string_new ("Denemo 9");
#ifdef G_OS_WIN32
  ret->browser = g_string_new ("");//use file association
  ret->midiplayer = g_string_new ("");
  ret->audioplayer = g_string_new ("");
  ret->lilypath = g_string_new ("lilypond.exe");//We don't assume the file assoc works - we are installing this anyway to a known place,the option  neither lilypond-windows.exe nor the -dgui option are used
  ret->pdfviewer = g_string_new ("");
  ret->imageviewer = g_string_new ("");

  ret->midiplayer = g_string_new("");
#else /* !G_OS_WIN32 */
  ret->browser = g_string_new ("firefox");
  ret->midiplayer = g_string_new ("playmidi");
  ret->audioplayer = g_string_new ("play");
  ret->lilypath = g_string_new ("lilypond");
  ret->pdfviewer = g_string_new ("xpdf");
  ret->imageviewer = g_string_new ("eog");
#endif /* !G_OS_WIN32 */
  ret->sequencer = g_string_new ("/dev/sequencer");
  ret->midi_in = g_string_new ("/dev/midi");
  ret->profile = g_string_new("");
  ret->denemopath = g_string_new (g_get_home_dir());
  ret->lilyversion = g_string_new ("");//meaning use installed LilyPond version
  ret->temperament = g_string_new("Equal");
  ret->strictshortcuts = FALSE;
  ret->resolution = 300;
  ret->display_refresh = 0.01;
  ret->animation_steps = 10;
  ret->overlays = FALSE;
  ret->continuous = TRUE;
  ret->cursor_highlight = TRUE;

#ifdef _HAVE_JACK_
  ret->immediateplayback = FALSE;
#else
  ret->immediateplayback = TRUE;
#endif
#ifdef _HAVE_FLUIDSYNTH_
  /*TODO needs to check if linux and set fluidsynth_audio_driver = alsa
  	for some reason the default for linux is jack */
#ifdef G_OS_WIN32
  ret->fluidsynth_audio_driver = g_string_new ("portaudio");
  ret->fluidsynth_midi_driver = g_string_new ("");
#else
  ret->fluidsynth_audio_driver = g_string_new ("alsa");
  ret->fluidsynth_midi_driver = g_string_new ("alsa_seq");
#endif
  gchar *soundfontpath = g_build_filename (get_data_dir (), "soundfonts",
		                                           "A320U.sf2", NULL);
  ret->fluidsynth_soundfont = g_string_new(soundfontpath);
  ret->pitchspellingchannel = 15;
  ret->pitchspellingprogram = 17;

#ifdef G_OS_WIN32
  ret->fluidsynth_sample_rate = 22050;//the worst case slow machine
#else
  ret->fluidsynth_sample_rate = 0;//do not set
#endif
#endif
  ret->saveparts = FALSE;
  ret->lilyentrystyle = FALSE;
  ret->createclones = FALSE;
  ret->enable_thumbnails = TRUE;
  ret->autosave = TRUE;
  ret->autosave_timeout = 5;
  ret->maxhistory = 20;
  ret->notation_palette = TRUE;
  ret->articulation_palette = FALSE;
  ret->midi_in_controls = FALSE;
  ret->playback_controls = FALSE;
  ret->toolbar = TRUE;
  ret->console_pane = FALSE;
  ret->lyrics_pane = TRUE;
  ret->visible_directive_buttons = TRUE;
  ret->autoupdate = FALSE;
  ret->rhythm_palette = TRUE;
  ret->object_palette = TRUE;
  ret->history = g_queue_new ();
  ret->zoom = 100;
  ret->system_height = 100;
  ret->applytoselection = TRUE;
  ret->quickshortcuts = TRUE;
  ret->progressbardecorations = TRUE;
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

static void
parsePorts (xmlDocPtr doc, xmlNodePtr cur, gint i)
{
  gint j=0;
  xmlNodePtr child;
  //first count the ports
  for (j=0, child = cur->xmlChildrenNode;child != NULL;child = child->next)
    {
      if (xmlStrcmp (child->name, (const xmlChar *) "port") == 0)
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, child->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      j++;
	      xmlFree (tmp); 
	    }
	}
    }
  g_debug("We have %d ports for client %s\n",j, Denemo.prefs.midi_device[i].client_name->str);

#if 1
  {GArray *arr = g_array_new(TRUE, TRUE, sizeof(DeviceManagerPort));
    g_array_set_size(arr, j);
    Denemo.prefs.midi_device[i].ports =  (DeviceManagerPort *)arr->data;
    Denemo.prefs.midi_device[i].ports_array = arr;
  }
#else
  Denemo.prefs.midi_device[i].ports = g_malloc0((j+1) * sizeof(DeviceManagerPort));
#endif


  for (j=0,child = cur->xmlChildrenNode;child != NULL;child = child->next)
    {
      if (xmlStrcmp (child->name, (const xmlChar *) "port") == 0)
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, child->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      //g_print("storing port %s into client\n", tmp);
	      Denemo.prefs.midi_device[i].ports[j].midi_buffer = g_malloc0(DENEMO_BUFFER_MAX_INDEX*sizeof(MidiBuffer));
	      Denemo.prefs.midi_device[i].ports[j].port_name = g_string_new(tmp);
	      j++;
	      //FIXME check j<max
	      xmlFree (tmp); 
	    }
	}
    }
}

/**
 * parseDevices - reads midi device prefs
 * 
 * @param  doc	document pointer
 * @param cur pointer to the current XML Node
 * @param prefs pointer to the preferences structure
 */

static void
parseDevices (xmlDocPtr doc, xmlNodePtr cur, DenemoPrefs * prefs)
{
  gint i;
  xmlNodePtr child;
  //first count the client devices
  for (i=0, child = cur->xmlChildrenNode;child != NULL && i<DENEMO_MAX_DEVICES;child = child->next)
    {
      if (xmlStrcmp (child->name, (const xmlChar *) "device") == 0)
	{
	  i++;	  
	}
    }
#if 1  
  {GArray *arr = g_array_new(TRUE, TRUE, sizeof(DeviceManagerDevice));
    g_array_set_size(arr, i);
    prefs->midi_device =  (DeviceManagerDevice *)arr->data;
    prefs->midi_device_array = arr;
  }
#else
  prefs->midi_device = (DeviceManagerDevice *)g_malloc0((i+1)*sizeof(DeviceManagerDevice));
#endif
  for (i=0, child = cur->xmlChildrenNode;child != NULL && i<DENEMO_MAX_DEVICES;child = child->next, i++)
    {
      if (xmlStrcmp (child->name, (const xmlChar *) "device") == 0)
	{
	  gchar *tmp = (gchar *) xmlGetProp (child, (xmlChar *) "client");
	  //g_print("storing client name %s\n", tmp);
	  if(tmp)
	    {
	      if(prefs->midi_device[i].client_name)
		g_string_assign (prefs->midi_device[i].client_name, (gchar *) tmp);
	      else
		prefs->midi_device[i].client_name = g_string_new ((gchar *) tmp);
	      xmlFree (tmp);
	    }
	}
      xmlNodePtr ports = child->xmlChildrenNode;
      // g_print("for client ports is %p\n", ports);   
      if (ports && xmlStrcmp (ports->name, (const xmlChar *) "ports") == 0)
	{
	  parsePorts(doc, ports, i);
	}
    }
}

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

#define READXMLENTRY2(field)  \
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) #field))\
	{\
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);\
	  if(tmp)\
	    {\
              define_scheme_variable("DenemoPref_" #field, tmp, NULL);\
	      prefs->field = get_midi_audio_pointer(tmp);\
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
              define_scheme_int_variable("DenemoPref_" #field, atoi(tmp), NULL); \
	      prefs->field = atoi ((gchar *) tmp);\
	      xmlFree (tmp);\
	    }\
	}

#define READDOUBLEXMLENTRY(field) \
      else if (0 ==\
	       xmlStrcmp (cur->name, (const xmlChar *) #field))\
	{\
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);\
	  if(tmp)\
	    {\
              define_scheme_int_variable("DenemoPref_" #field, atof(tmp), NULL); \
	      prefs->field = atof ((gchar *) tmp);\
	      xmlFree (tmp);\
	    }\
	}


#define READBOOLXMLENTRY(field) \
      else if (0 ==\
	       xmlStrcmp (cur->name, (const xmlChar *) #field))\
	{\
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);\
	  if(tmp)\
	    {\
              define_scheme_bool_variable("DenemoPref_" #field, atoi(tmp), NULL); \
	      prefs->field = atoi ((gchar *) tmp);\
	      xmlFree (tmp);\
	    }\
	}




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

      if (0 == xmlStrcmp (cur->name, (const xmlChar *) "midi-devices"))
	{
	  parseDevices(doc, cur, &Denemo.prefs);

	}

	READXMLENTRY(midiplayer)      
	READXMLENTRY(audioplayer)
	READXMLENTRY(fontspec)
	       
	READXMLENTRY(browser)
        else if (0 ==
	       xmlStrcmp (cur->name, (const xmlChar *) "autosavetimeout"))
	{
	  xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
	  if(tmp)
	    {
	      gchar *curname = g_strdup_printf("DenemoPref_%s", cur->name);
	      define_scheme_int_variable(curname, atoi(tmp), NULL); 
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
	      define_scheme_int_variable(curname, atoi(tmp), NULL); 
	      g_free(curname);
	      prefs->maxhistory = atoi ((gchar *) tmp);
	      if(prefs->maxhistory <1) prefs->maxhistory = 1;
	      xmlFree (tmp);
	    }
	}
	
      READBOOLXMLENTRY(autosave)
      
      READXMLENTRY(pdfviewer)
      READXMLENTRY(imageviewer) 
         
      READXMLENTRY(profile) 
	
      READXMLENTRY(username)    
      READXMLENTRY(password)           
      READXMLENTRY(denemopath)          
      READXMLENTRY(temperament)
      READXMLENTRY(midi_in)
      READXMLENTRY(sequencer)
      READXMLENTRY2(midi_audio_output)
      
      READBOOLXMLENTRY(createclones)
      READBOOLXMLENTRY(immediateplayback)
      READINTXMLENTRY(pitchspellingchannel)
      READINTXMLENTRY(pitchspellingprogram)
      READBOOLXMLENTRY(modal) 
      READBOOLXMLENTRY(persistence) 
      READBOOLXMLENTRY(cursor_highlight) 
      READBOOLXMLENTRY(applytoselection) 
      READBOOLXMLENTRY(quickshortcuts) 
      READBOOLXMLENTRY(startmidiin) 
      READINTXMLENTRY(mode) 
  
      READBOOLXMLENTRY(strictshortcuts)
      READINTXMLENTRY(resolution)
      READDOUBLEXMLENTRY(display_refresh)
      READINTXMLENTRY(animation_steps)
      READBOOLXMLENTRY(overlays)
      READBOOLXMLENTRY(enable_thumbnails)
      
      READBOOLXMLENTRY(continuous)
      READBOOLXMLENTRY(jacktransport)
      READBOOLXMLENTRY(jacktransport_start_stopped)
      READBOOLXMLENTRY(lilyentrystyle)
      READBOOLXMLENTRY(toolbar)
      READBOOLXMLENTRY(notation_palette)
      READBOOLXMLENTRY(articulation_palette)
	READBOOLXMLENTRY(midi_in_controls)
	READBOOLXMLENTRY(playback_controls)
      READBOOLXMLENTRY(console_pane)
      READBOOLXMLENTRY(lyrics_pane)
      READBOOLXMLENTRY(visible_directive_buttons)
      READBOOLXMLENTRY(rhythm_palette) 
      READBOOLXMLENTRY(object_palette)
      READBOOLXMLENTRY(autoupdate) 
     
      READXMLENTRY(fluidsynth_audio_driver)
      READXMLENTRY(fluidsynth_midi_driver)
      READXMLENTRY(fluidsynth_soundfont)
      READBOOLXMLENTRY(fluidsynth_reverb)
      READBOOLXMLENTRY(fluidsynth_chorus)
      READINTXMLENTRY(fluidsynth_sample_rate)
      READINTXMLENTRY(fluidsynth_period_size)
      READINTXMLENTRY(zoom)
      READINTXMLENTRY(dynamic_compression)
      READINTXMLENTRY(system_height)
      READBOOLXMLENTRY(progressbardecorations)


      READBOOLXMLENTRY(saveparts)
      
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
	      g_free (file);
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
 * Output an double child as a child of the given node.
 *
 * @param parent - pointer to child nodes parent
 * @param name -  name for the child node
 * @param content - data to add to the child node
 */
static xmlNodePtr
newXMLDoubleChild (xmlNodePtr parent, const xmlChar * name, gdouble content)
{
  static GString* str;
  if(str==NULL)
    str = g_string_new("");
  g_string_printf(str, "%f", content);
  return xmlNewChild (parent, NULL, name, (xmlChar *) str->str);
}


/**
 * Write the denemorc file
 *
 * @param prefs a pointer to the preferences structure
 *
 */
static void
writeDevices(xmlDocPtr doc, xmlNodePtr parent, DenemoPrefs * prefs) {
  gint i;
  xmlNodePtr child;
  if(!prefs->midi_device)
    return;
  if(prefs->midi_device[0].client_name) {
    child =  xmlNewChild (parent, NULL, (xmlChar *) "midi-devices", NULL);
    for(i=0;i<DENEMO_MAX_DEVICES && prefs->midi_device[i].client_name;i++){
	xmlNodePtr device = xmlNewChild (child, NULL, "device", NULL);
	xmlSetProp (device, (xmlChar *) "client",
		    (xmlChar *) prefs->midi_device[i].client_name->str);
	
	if(prefs->midi_device[i].ports && prefs->midi_device[i].ports[0].port_name) {
	  gint j;
	  xmlNodePtr ports = xmlNewChild (device, NULL, (xmlChar *) "ports", NULL);
	  for(j=0;prefs->midi_device[i].ports[j].port_name;j++)
	    xmlNewChild (ports, NULL, (xmlChar *)"port", (xmlChar *)prefs->midi_device[i].ports[j].port_name->str);
	} 
    }
  }
}


gint
writeXMLPrefs (DenemoPrefs * prefs)
{
  gint ret = -1;
  xmlDocPtr doc;
  xmlNodePtr parent, child;
  static GString *localrc = NULL;
  if (!localrc)
    {
      localrc = g_string_new (g_build_filename(locatedotdenemo (), PREFS_FILE, NULL));
    }

  doc = xmlNewDoc ((xmlChar *) "1.0");
  doc->xmlRootNode = parent =
    xmlNewDocNode (doc, NULL, (xmlChar *) "Denemo", NULL);
  child = xmlNewChild (parent, NULL, (xmlChar *) "Config", NULL);

    writeDevices(doc, child, prefs);

#define WRITEXMLENTRY(field) \
  if (prefs->field){\
    gchar *def = g_strdup("Holds the value of the user's " #field " preference");\
    gchar *curname = g_strdup_printf("DenemoPref_%s", #field);\
    define_scheme_variable(curname, prefs->field->str, def);\
    g_free(curname);\
    g_free(def);\
    xmlNewChild (child, NULL, (xmlChar *) #field,\
		 (xmlChar *) prefs->field->str);}

 #define WRITEXMLENTRY2(field) \
  if (prefs->field){\
    gchar *def = g_strdup("Holds the value of the user's " #field " preference");\
    gchar *curname = g_strdup_printf("DenemoPref_%s", #field);\
    define_scheme_variable(curname, prefs->field, def);\
    g_free(curname);\
    g_free(def);\
    xmlNewChild (child, NULL, (xmlChar *) #field,\
		 (xmlChar *) prefs->field);}
   

  WRITEXMLENTRY(lilypath)

  WRITEXMLENTRY(midiplayer)
  WRITEXMLENTRY(audioplayer)
  WRITEXMLENTRY(fontspec)

  WRITEXMLENTRY(pdfviewer)
  WRITEXMLENTRY(imageviewer)
  WRITEXMLENTRY(profile)
  WRITEXMLENTRY(username)
  WRITEXMLENTRY(password)
  WRITEXMLENTRY(denemopath)
  WRITEXMLENTRY(temperament)
  WRITEXMLENTRY(midi_in)
  WRITEXMLENTRY(sequencer)

#define WRITEINTXMLENTRY(field){ \
    gchar *def = g_strdup("Holds the interger value of the user's " #field " preference");\
    gint value = prefs->field;\
    gchar *curname = g_strdup_printf("DenemoPref_%s", #field);\
    define_scheme_int_variable(curname, value, def);\
    g_free(curname);\
    g_free(def);\
  newXMLIntChild (child, (xmlChar *) #field,\
		  prefs->field);}

#define WRITEDOUBLEXMLENTRY(field){ \
    gchar *def = g_strdup("Holds the interger value of the user's " #field " preference");\
    gdouble value = prefs->field;\
    gchar *curname = g_strdup_printf("DenemoPref_%s", #field);\
    define_scheme_double_variable(curname, value, def);\
    g_free(curname);\
    g_free(def);\
  newXMLDoubleChild (child, (xmlChar *) #field,\
		  prefs->field);}





#define WRITEBOOLXMLENTRY(field){ \
    gchar *def = g_strdup("Holds #t or #f, the user's " #field " preference");\
    gboolean value = prefs->field;\
    gchar *curname = g_strdup_printf("DenemoPref_%s", #field);\
    define_scheme_bool_variable(curname, value, def);\
    g_free(curname);\
    g_free(def);\
  newXMLIntChild (child, (xmlChar *) #field,\
		  prefs->field);}

  WRITEBOOLXMLENTRY(autosave)
  WRITEINTXMLENTRY(autosave_timeout)
  WRITEINTXMLENTRY(maxhistory)
  WRITEBOOLXMLENTRY(saveparts)
  WRITEBOOLXMLENTRY(createclones)
  WRITEBOOLXMLENTRY(lilyentrystyle)
  WRITEBOOLXMLENTRY(immediateplayback)
  WRITEINTXMLENTRY(pitchspellingchannel)
  WRITEINTXMLENTRY(pitchspellingprogram)
  WRITEBOOLXMLENTRY(modal)
  WRITEBOOLXMLENTRY(persistence)
  WRITEBOOLXMLENTRY(cursor_highlight)
  WRITEBOOLXMLENTRY(applytoselection)
  WRITEBOOLXMLENTRY(quickshortcuts)
  WRITEBOOLXMLENTRY(startmidiin)
  WRITEINTXMLENTRY(mode)
  WRITEBOOLXMLENTRY(strictshortcuts)
  WRITEINTXMLENTRY(resolution)
  WRITEDOUBLEXMLENTRY(display_refresh)
  WRITEINTXMLENTRY(animation_steps)
  WRITEBOOLXMLENTRY(overlays)
  WRITEBOOLXMLENTRY(enable_thumbnails)
  WRITEBOOLXMLENTRY(continuous)
  WRITEBOOLXMLENTRY(jacktransport)
  WRITEBOOLXMLENTRY(jacktransport_start_stopped)
  WRITEBOOLXMLENTRY(toolbar)
  WRITEBOOLXMLENTRY(notation_palette)
  WRITEBOOLXMLENTRY(midi_in_controls)
  WRITEBOOLXMLENTRY(playback_controls)
  WRITEBOOLXMLENTRY(articulation_palette)
  WRITEBOOLXMLENTRY(console_pane)
  WRITEBOOLXMLENTRY(lyrics_pane)
  WRITEBOOLXMLENTRY(visible_directive_buttons)
  WRITEBOOLXMLENTRY(autoupdate)
  WRITEBOOLXMLENTRY(rhythm_palette)
  WRITEBOOLXMLENTRY(object_palette)
  WRITEXMLENTRY2(midi_audio_output)
  WRITEXMLENTRY(fluidsynth_audio_driver)
  WRITEXMLENTRY(fluidsynth_midi_driver)
  WRITEXMLENTRY(fluidsynth_soundfont)
  WRITEBOOLXMLENTRY(fluidsynth_reverb)
  WRITEBOOLXMLENTRY(fluidsynth_chorus)
  WRITEINTXMLENTRY(fluidsynth_sample_rate)
  WRITEINTXMLENTRY(fluidsynth_period_size)
  WRITEINTXMLENTRY(dynamic_compression)
  WRITEINTXMLENTRY(zoom)
  WRITEINTXMLENTRY(system_height)
  WRITEBOOLXMLENTRY(progressbardecorations)
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
  if(g_file_test(filename->str, G_FILE_TEST_EXISTS))
    doc = xmlParseFile (filename->str);
  else
    return ret;

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
