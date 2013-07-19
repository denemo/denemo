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
#include "view.h"

static gint readxmlprefsFile (gchar * filename);

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
get_user_data_dir ()
{
  static gchar *dotdenemo = NULL;

  gboolean err;
  if (!dotdenemo)
    {
      dotdenemo = g_build_filename (g_get_home_dir (), ".denemo-" PACKAGE_VERSION, NULL);
    }
  err = g_mkdir_with_parents (dotdenemo, 0770);
  if (err)
    {
      warningdialog (_("Could not create .denemo for you personal settings"));
      g_free (dotdenemo);
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
    printdir = make_temp_dir ();
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
#define ret (&Denemo.prefs)
  gchar *dotdenemo = (gchar *) get_user_data_dir ();
  gchar *localrc = dotdenemo ? g_build_filename (dotdenemo, PREFS_FILE, NULL) : NULL;

  /* Reasonable default values */

  ret->mode = INPUTEDIT | INPUTRHYTHM | INPUTNORMAL;

  const gchar *name = g_get_user_name ();
  ret->username = g_string_new (name ? name : "DenemoUser");
  ret->password = g_string_new ("");

  ret->fontspec = g_string_new ("Denemo 9");

#ifdef G_OS_WIN32
  ret->browser = g_string_new ("");     //use file association
  ret->audioplayer = g_string_new ("");
  ret->lilypath = g_string_new (g_build_filename (get_system_bin_dir (), "lilypond-windows.exe", NULL));       //We don't assume the file assoc works - we are installing this anyway to a known place,the option  neither lilypond-windows.exe nor the -dgui option are used
  ret->imageviewer = g_string_new ("");
#else /* !G_OS_WIN32 */
  ret->browser = g_string_new ("firefox");
  ret->audioplayer = g_string_new ("play");
 #ifdef _GUB_BUILD_
  ret->lilypath = g_string_new (g_build_filename (get_system_bin_dir (), "lilypond", NULL));
 #else
  ret->lilypath = g_string_new ("lilypond");
 #endif
  
  ret->imageviewer = g_string_new ("eog");
#endif /* !G_OS_WIN32 */

  ret->profile = g_string_new ("Default");
  ret->denemopath = g_string_new (g_get_home_dir ());
  ret->lilyversion = g_string_new (""); //meaning use installed LilyPond version
  ret->temperament = g_string_new ("Equal");
  ret->strictshortcuts = FALSE;
  ret->resolution = 300;
  ret->display_refresh = 0.01;
  ret->animation_steps = 10;
  ret->overlays = FALSE;
  ret->continuous = TRUE;
  ret->cursor_highlight = TRUE;
  ret->return_key_is_special = TRUE;
  ret->newbie = TRUE;
  ret->persistence = TRUE;
  ret->learning = TRUE;
  ret->immediateplayback = TRUE;
  ret->manualtypeset = FALSE;
  ret->typesetrefresh = 10;
  ret->typesettype = TYPESET_ALL_MOVEMENTS;
  ret->firstmeasure = 4;
  ret->firststaff = 4;
  ret->lastmeasure = 4;
  ret->laststaff = 4;


  ret->audio_driver = g_string_new ("default");
  ret->midi_driver = g_string_new ("default");
#ifdef _HAVE_PORTAUDIO_
  ret->audio_driver = g_string_new ("portaudio");
  ret->midi_driver = g_string_new ("portmidi");
#endif
  ret->jack_connect_ports_l = g_string_new ("system:playback_1");
  ret->jack_connect_ports_r = g_string_new ("system:playback_2");
  ret->jack_connect_midi_in_port = g_string_new ("");
  ret->jack_connect_midi_out_port = g_string_new ("");

  ret->portaudio_device = g_string_new ("default");
  ret->portaudio_sample_rate = 44100;
  ret->portaudio_period_size = 256;

  ret->portmidi_input_device = g_string_new ("default");
  ret->portmidi_output_device = g_string_new ("default");

  gchar* file_path = g_build_filename (SOUNDFONTS_DIR, "A320U.sf2", NULL);
  gchar *soundfontpath = find_file(file_path);
  g_free(file_path);

  ret->fluidsynth_soundfont = g_string_new (soundfontpath);
  ret->pitchspellingchannel = 15;
  ret->pitchspellingprogram = 17;

  ret->saveparts = FALSE;
  ret->lilyentrystyle = FALSE;
  ret->createclones = FALSE;
  ret->enable_thumbnails = TRUE;
  ret->autosave = TRUE;
  ret->autosave_timeout = 5;
  ret->maxhistory = 20;
  ret->notation_palette = TRUE;
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

  /* Read values from personal preferences file */

  //readpreffile (localrc, ret);
  if (localrc)
    {
      if (g_file_test (localrc, G_FILE_TEST_EXISTS))
        readxmlprefsFile (localrc);
      else
        writeXMLPrefs (ret);
    }
  g_free (localrc);
#undef ret

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
              define_scheme_variable("DenemoPref_" #field, (gchar*) tmp, NULL);\
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
              define_scheme_int_variable("DenemoPref_" #field, atoi((gchar *) tmp), NULL); \
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
              define_scheme_int_variable("DenemoPref_" #field, atof((gchar *) tmp), NULL); \
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
              define_scheme_bool_variable("DenemoPref_" #field, atoi((gchar *) tmp), NULL); \
	      prefs->field = atoi ((gchar *) tmp);\
	      xmlFree (tmp);\
	    }\
	}




      if (0 == xmlStrcmp (cur->name, (const xmlChar *) "lilypath"))
        {
          xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
          if (tmp)
            {
              gchar *curname = g_strdup_printf ("DenemoPref_%s", cur->name);
              define_scheme_variable (curname, (gchar*) tmp, NULL);
              g_free (curname);
              prefs->lilypath = g_string_assign (prefs->lilypath, (gchar *) tmp);
              //g_print ("Lilypond Path %s\n", tmp);
              xmlFree (tmp);
            }
        }

      READXMLENTRY (audioplayer) READXMLENTRY (fontspec) READXMLENTRY (browser)
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "autosavetimeout"))
        {
          xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
          if (tmp)
            {
              gchar *curname = g_strdup_printf ("DenemoPref_%s", cur->name);
              define_scheme_int_variable (curname, atoi ((char*) tmp), NULL);
              g_free (curname);
              prefs->autosave_timeout = atoi ((gchar *) tmp);
              if (prefs->autosave_timeout < 1)
                prefs->autosave_timeout = 1;
              //g_print ("Autosave Timeout %s\n", tmp);
              xmlFree (tmp);
            }
        }
      else if (0 == xmlStrcmp (cur->name, (const xmlChar *) "maxhistory"))
        {
          xmlChar *tmp = xmlNodeListGetString (doc, cur->xmlChildrenNode, 1);
          if (tmp)
            {
              gchar *curname = g_strdup_printf ("DenemoPref_%s", cur->name);
              define_scheme_int_variable (curname, atoi ((char*) tmp), NULL);
              g_free (curname);
              prefs->maxhistory = atoi ((gchar *) tmp);
              if (prefs->maxhistory < 1)
                prefs->maxhistory = 1;
              xmlFree (tmp);
            }
        }

      READBOOLXMLENTRY (autosave)
        
        READXMLENTRY (imageviewer)
        READXMLENTRY (profile)
        READXMLENTRY (username)
        READXMLENTRY (password)
        READXMLENTRY (denemopath)
        READXMLENTRY (temperament)
        READBOOLXMLENTRY (createclones)
        READBOOLXMLENTRY (immediateplayback)
        READBOOLXMLENTRY (manualtypeset)
        READINTXMLENTRY (typesetrefresh)
        READINTXMLENTRY (typesettype)
        READINTXMLENTRY (firstmeasure)
        READINTXMLENTRY (firststaff)
        READINTXMLENTRY (lastmeasure)
        READINTXMLENTRY (laststaff)
        READINTXMLENTRY (pitchspellingchannel)
        READINTXMLENTRY (pitchspellingprogram)
        READBOOLXMLENTRY (modal)
        READBOOLXMLENTRY (persistence)
        READBOOLXMLENTRY (cursor_highlight)
        READBOOLXMLENTRY (return_key_is_special)
        READBOOLXMLENTRY (newbie)
        READBOOLXMLENTRY (learning)
        READBOOLXMLENTRY (applytoselection)
        READBOOLXMLENTRY (quickshortcuts)
        READBOOLXMLENTRY (startmidiin)
        READINTXMLENTRY (mode)
        READBOOLXMLENTRY (strictshortcuts)
        READBOOLXMLENTRY (menunavigation)
        READINTXMLENTRY (resolution)
        READDOUBLEXMLENTRY (display_refresh)
        READINTXMLENTRY (animation_steps)
        READINTXMLENTRY (tooltip_timeout)
        READINTXMLENTRY (tooltip_browse_timeout)
        READINTXMLENTRY (tooltip_browse_mode_timeout)
        READBOOLXMLENTRY (overlays)
        READBOOLXMLENTRY (enable_thumbnails)
        READBOOLXMLENTRY (continuous)
        READBOOLXMLENTRY (lilyentrystyle)
        READBOOLXMLENTRY (toolbar)
        READBOOLXMLENTRY (notation_palette)
        READBOOLXMLENTRY (midi_in_controls)
        READBOOLXMLENTRY (playback_controls)
        READBOOLXMLENTRY (console_pane)
        READBOOLXMLENTRY (lyrics_pane)
        READBOOLXMLENTRY (visible_directive_buttons)
        READBOOLXMLENTRY (rhythm_palette)
        READBOOLXMLENTRY (object_palette)
        READBOOLXMLENTRY (autoupdate)
        READXMLENTRY (audio_driver)
        READXMLENTRY (midi_driver)
        READBOOLXMLENTRY (jacktransport)
        READBOOLXMLENTRY (jacktransport_start_stopped)
        READXMLENTRY (jack_connect_ports_l)
        READXMLENTRY (jack_connect_ports_r)
        READXMLENTRY (jack_connect_midi_in_port)
        READXMLENTRY (jack_connect_midi_out_port)
        READXMLENTRY (portaudio_device)
        READINTXMLENTRY (portaudio_sample_rate)
        READINTXMLENTRY (portaudio_period_size)
        READINTXMLENTRY (maxrecordingtime)
        READXMLENTRY (portmidi_input_device)
        READXMLENTRY (portmidi_output_device)
        READXMLENTRY (fluidsynth_soundfont)
        READBOOLXMLENTRY (fluidsynth_reverb)
        READBOOLXMLENTRY (fluidsynth_chorus)
        READINTXMLENTRY (zoom)
        READINTXMLENTRY (dynamic_compression)
        READINTXMLENTRY (system_height)
        READBOOLXMLENTRY (progressbardecorations)
        READBOOLXMLENTRY (saveparts) cur = cur->next;
    }

  return;
}

#undef READXMLENTRY
#undef READBOOLXMLENTRY
#undef READINTXMLENTRY
#undef READDOUBLEXMLENTRY



#define GETBOOLPREF(field) \
  else if (!strcmp(prefname, #field))\
    return Denemo.prefs.field;


gboolean
get_bool_pref (gchar * prefname)
{
  if (*prefname == 0)
    return FALSE;
  GETBOOLPREF (autosave)
    GETBOOLPREF (createclones)
    GETBOOLPREF (immediateplayback)
    GETBOOLPREF (manualtypeset)
    GETBOOLPREF (modal)
    GETBOOLPREF (persistence)
    GETBOOLPREF (cursor_highlight)
    GETBOOLPREF (return_key_is_special)
    GETBOOLPREF (newbie)
    GETBOOLPREF (learning)
    GETBOOLPREF (applytoselection)
    GETBOOLPREF (quickshortcuts)
    GETBOOLPREF (startmidiin)
    GETBOOLPREF (strictshortcuts)
    GETBOOLPREF (menunavigation)
    GETBOOLPREF (overlays)
    GETBOOLPREF (enable_thumbnails)
    GETBOOLPREF (continuous)
    GETBOOLPREF (lilyentrystyle) GETBOOLPREF (toolbar) GETBOOLPREF (notation_palette) GETBOOLPREF (midi_in_controls) GETBOOLPREF (playback_controls) GETBOOLPREF (console_pane) GETBOOLPREF (lyrics_pane) GETBOOLPREF (visible_directive_buttons) GETBOOLPREF (rhythm_palette) GETBOOLPREF (object_palette) GETBOOLPREF (autoupdate) GETBOOLPREF (jacktransport_start_stopped) GETBOOLPREF (fluidsynth_reverb) GETBOOLPREF (fluidsynth_chorus) GETBOOLPREF (progressbardecorations) return FALSE;
}

#undef GETBOOLPREF
#define GETINTPREF(field) \
  else if (!strcmp(prefname, #field))\
    return Denemo.prefs.field;

gint
get_int_pref (gchar * prefname)
{
  if (*prefname == 0)
    return 0;
  GETINTPREF (typesettype)
    GETINTPREF (typesetrefresh)
    GETINTPREF (firstmeasure) GETINTPREF (firststaff) GETINTPREF (lastmeasure) GETINTPREF (laststaff) GETINTPREF (pitchspellingchannel) GETINTPREF (pitchspellingprogram) GETINTPREF (mode) GETINTPREF (resolution) GETINTPREF (animation_steps) GETINTPREF (tooltip_timeout) GETINTPREF (tooltip_browse_timeout) GETINTPREF (tooltip_browse_timeout) GETINTPREF (portaudio_sample_rate) GETINTPREF (portaudio_period_size) GETINTPREF (zoom) GETINTPREF (dynamic_compression) GETINTPREF (system_height) return 0;
}

#undef GETINTPREF
#define GETSTRINGPREF(field) \
  else if (!strcmp(prefname, #field))\
    return Denemo.prefs.field->str;

gchar *
get_string_pref (gchar * prefname)
{
  if (*prefname == 0)
    return NULL;
  
    GETSTRINGPREF (imageviewer)
    GETSTRINGPREF (profile)
    GETSTRINGPREF (username) GETSTRINGPREF (password) GETSTRINGPREF (denemopath) GETSTRINGPREF (temperament) GETSTRINGPREF (audio_driver) GETSTRINGPREF (midi_driver) GETSTRINGPREF (jack_connect_ports_l) GETSTRINGPREF (jack_connect_ports_r) GETSTRINGPREF (jack_connect_midi_in_port) GETSTRINGPREF (jack_connect_midi_out_port) GETSTRINGPREF (portaudio_device) GETSTRINGPREF (portmidi_input_device) GETSTRINGPREF (portmidi_output_device) GETSTRINGPREF (fluidsynth_soundfont) return NULL;
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
  xmlNewTextChild ((xmlNodePtr) user_data, NULL, (xmlChar *) "file", (xmlChar *) data);
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
              gchar *file = g_filename_to_utf8 (tmp, -1, &read, &written, &err);
              if (err != NULL)
                {
                  file = "Unknown file name";
                  g_warning ("%s", err->message);
                  g_error_free (err);
                }
              g_debug ("Filename %s\n", tmp);
              g_queue_push_tail (prefs->history, g_strdup (file));
              g_free (tmp);
              g_free (file);
            }
        }

      cur = cur->next;
    }
  return;
}


static gint readxmlprefs (gchar * xmlsource, gboolean from_file);

/**
 * Read a denemo preferences xml file.
 * @param filename - denemorc file name 
 * @param prefs - struct to populate data into
 *
 */

static gint
readxmlprefsFile (gchar * filename)
{
  return readxmlprefs (filename, TRUE);
}

/**
 * Read denemo preferences from an xml string.
 * @param content - a string containing prefs in xml 
 *
 */
gint
readxmlprefsString (gchar * content)
{
  gchar *xml = g_strconcat ("<?xml version=\"1.0\"?><Denemo><Config>", content, "</Config></Denemo>", NULL);
  gint ret = readxmlprefs (xml, FALSE);
  g_free (xml);
  return ret;
}



static gint
readxmlprefs (gchar * xmlsource, gboolean from_file)
{
  DenemoPrefs *prefs = &Denemo.prefs;
  gint ret = -1;
  xmlDocPtr doc = NULL;
  xmlNodePtr rootElem;
  if (from_file)
    printf ("\nxmlsource == %s\n", xmlsource);
  if (from_file)
    doc = xmlParseFile (xmlsource);
  else
    doc = xmlReadMemory (xmlsource, strlen (xmlsource), "noname.xml", NULL, 0);
  if (doc == NULL)
    {
      g_warning ("Could not read XML %s %s\n", from_file ? "File: " : ":\n", xmlsource);
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
      g_debug ("RootElem 2 %s\n", rootElem->name);

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
  static gchar sIntBuf[12];     /* enough for -2000000000 + '\0' */
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
  static GString *str;
  if (str == NULL)
    str = g_string_new ("");
  g_string_printf (str, "%f", content);
  return xmlNewChild (parent, NULL, name, (xmlChar *) str->str);
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
      localrc = g_string_new (g_build_filename (get_user_data_dir (), PREFS_FILE, NULL));
    }

  doc = xmlNewDoc ((xmlChar *) "1.0");
  doc->xmlRootNode = parent = xmlNewDocNode (doc, NULL, (xmlChar *) "Denemo", NULL);
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

#define WRITEXMLENTRY2(field) \
  if (prefs->field){\
    gchar *def = g_strdup("Holds the value of the user's " #field " preference");\
    gchar *curname = g_strdup_printf("DenemoPref_%s", #field);\
    define_scheme_variable(curname, prefs->field, def);\
    g_free(curname);\
    g_free(def);\
    xmlNewChild (child, NULL, (xmlChar *) #field,\
		 (xmlChar *) prefs->field);}


  WRITEXMLENTRY (lilypath) WRITEXMLENTRY (audioplayer) WRITEXMLENTRY (fontspec) WRITEXMLENTRY (imageviewer) WRITEXMLENTRY (profile) WRITEXMLENTRY (username) WRITEXMLENTRY (password) WRITEXMLENTRY (denemopath) WRITEXMLENTRY (temperament)
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
    WRITEBOOLXMLENTRY (autosave)
    WRITEINTXMLENTRY (autosave_timeout)
    WRITEINTXMLENTRY (maxhistory)
    WRITEBOOLXMLENTRY (saveparts)
    WRITEBOOLXMLENTRY (createclones)
    WRITEBOOLXMLENTRY (lilyentrystyle)
    WRITEBOOLXMLENTRY (immediateplayback)
    WRITEBOOLXMLENTRY (manualtypeset)
    WRITEINTXMLENTRY (typesetrefresh)
    WRITEINTXMLENTRY (typesettype)
    WRITEINTXMLENTRY (firstmeasure)
    WRITEINTXMLENTRY (firststaff)
    WRITEINTXMLENTRY (lastmeasure)
    WRITEINTXMLENTRY (laststaff)
    WRITEINTXMLENTRY (pitchspellingchannel)
    WRITEINTXMLENTRY (pitchspellingprogram)
    WRITEBOOLXMLENTRY (modal)
    WRITEBOOLXMLENTRY (persistence)
    WRITEBOOLXMLENTRY (cursor_highlight)
    WRITEBOOLXMLENTRY (return_key_is_special)
    WRITEBOOLXMLENTRY (newbie)
    WRITEBOOLXMLENTRY (learning)
    WRITEBOOLXMLENTRY (applytoselection)
    WRITEBOOLXMLENTRY (quickshortcuts)
    WRITEBOOLXMLENTRY (startmidiin)
    WRITEINTXMLENTRY (mode)
    WRITEBOOLXMLENTRY (strictshortcuts)
    WRITEBOOLXMLENTRY (menunavigation)
    WRITEINTXMLENTRY (resolution)
    WRITEDOUBLEXMLENTRY (display_refresh)
    WRITEINTXMLENTRY (animation_steps)
    WRITEINTXMLENTRY (tooltip_timeout)
    WRITEINTXMLENTRY (tooltip_browse_timeout)
    WRITEINTXMLENTRY (tooltip_browse_mode_timeout)
    WRITEBOOLXMLENTRY (overlays)
    WRITEBOOLXMLENTRY (enable_thumbnails)
    WRITEBOOLXMLENTRY (continuous)
    WRITEBOOLXMLENTRY (toolbar)
    WRITEBOOLXMLENTRY (notation_palette)
    WRITEBOOLXMLENTRY (midi_in_controls)
    WRITEBOOLXMLENTRY (playback_controls)
    WRITEBOOLXMLENTRY (console_pane)
    WRITEBOOLXMLENTRY (lyrics_pane)
    WRITEBOOLXMLENTRY (visible_directive_buttons)
    WRITEBOOLXMLENTRY (autoupdate)
    WRITEBOOLXMLENTRY (rhythm_palette)
    WRITEBOOLXMLENTRY (object_palette)
    WRITEXMLENTRY (audio_driver)
    WRITEXMLENTRY (midi_driver)
    WRITEBOOLXMLENTRY (jacktransport)
    WRITEBOOLXMLENTRY (jacktransport_start_stopped)
    WRITEXMLENTRY (jack_connect_ports_l)
    WRITEXMLENTRY (jack_connect_ports_r)
    WRITEXMLENTRY (jack_connect_midi_in_port)
    WRITEXMLENTRY (jack_connect_midi_out_port)
    WRITEXMLENTRY (portaudio_device)
    WRITEINTXMLENTRY (portaudio_sample_rate)
    WRITEINTXMLENTRY (portaudio_period_size)
    WRITEINTXMLENTRY (maxrecordingtime)
    WRITEXMLENTRY (portmidi_input_device) WRITEXMLENTRY (portmidi_output_device) WRITEXMLENTRY (fluidsynth_soundfont) WRITEBOOLXMLENTRY (fluidsynth_reverb) WRITEBOOLXMLENTRY (fluidsynth_chorus) WRITEINTXMLENTRY (dynamic_compression) WRITEINTXMLENTRY (zoom) WRITEINTXMLENTRY (system_height) WRITEBOOLXMLENTRY (progressbardecorations) WRITEXMLENTRY (browser) xmlSaveFormatFile (localrc->str, doc, 1);
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
      filename = g_string_new (get_user_data_dir ());
      g_string_append (filename, "/denemohistory");
    }
  if (g_file_test (filename->str, G_FILE_TEST_EXISTS))
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
  g_debug ("RootElem %s\n", rootElem->name);
  if (xmlStrcmp (rootElem->name, (const xmlChar *) "Denemo"))
    {
      g_warning ("Document has wrong type\n");
      xmlFreeDoc (doc);
      return ret;
    }
  rootElem = rootElem->xmlChildrenNode;
  while (rootElem != NULL)
    {
      g_debug ("RootElem 2 %s\n", rootElem->name);
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
      filename = g_string_new (get_user_data_dir ());
      g_string_append (filename, "/denemohistory");
    }

  doc = xmlNewDoc ((xmlChar *) "1.0");
  doc->xmlRootNode = parent = xmlNewDocNode (doc, NULL, (xmlChar *) "Denemo", NULL);
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
void
storeWindowState (void)
{
  GKeyFile *keyfile;
  gchar *contents;
  gchar *filename;
  gtk_window_get_size (GTK_WINDOW (Denemo.window), &(Denemo.width), &(Denemo.height));
  keyfile = g_key_file_new ();
  g_key_file_set_integer (keyfile, "State", "width", Denemo.width);
  g_key_file_set_integer (keyfile, "State", "height", Denemo.height);
  g_key_file_set_boolean (keyfile, "State", "maximized", Denemo.maximized);
  contents = g_key_file_to_data (keyfile, NULL, NULL);
  g_key_file_free (keyfile);
  filename = g_build_filename (get_user_data_dir (), "state.ini", NULL);
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
void
loadWindowState (void)
{
  gchar *filename;
  GKeyFile *keyfile;
  gint w, h;
  gboolean maximized = FALSE;
  GError *err = NULL;
  filename = g_build_filename (get_user_data_dir (), "state.ini", NULL);
  keyfile = g_key_file_new ();
  if (g_key_file_load_from_file (keyfile, filename, G_KEY_FILE_NONE, NULL) == FALSE)
    {
      g_free (filename);
      w = INITIAL_WIDTH;
      h = INITIAL_HEIGHT;
      maximized = FALSE;
    }
  else
    {
      g_free (filename);
      w = g_key_file_get_integer (keyfile, "State", "width", &err);
      if (err != NULL)
        {
          w = INITIAL_WIDTH;
          g_error_free (err);
          err = NULL;
        }
      h = g_key_file_get_integer (keyfile, "State", "height", &err);
      if (err != NULL)
        {
          h = INITIAL_HEIGHT;
          g_error_free (err);
          err = NULL;
        }
      maximized = g_key_file_get_boolean (keyfile, "State", "maximized", &err);
      if (err != NULL)
        {
          maximized = FALSE;
          g_error_free (err);
          err = NULL;
        }

      g_key_file_free (keyfile);
      Denemo.width = (w <= 0 ? INITIAL_WIDTH : w);
      Denemo.height = (h <= 0 ? INITIAL_HEIGHT : h);
      gtk_window_set_default_size (GTK_WINDOW (Denemo.window), Denemo.width, Denemo.height);
      if ((Denemo.maximized = maximized))
        {
          gtk_window_maximize (GTK_WINDOW (Denemo.window));
        }
      else
        gtk_window_unmaximize (GTK_WINDOW (Denemo.window));
    }
}
