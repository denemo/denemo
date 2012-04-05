/*	file.c
 * License: this file may be used under the FSF GPL version 3 or later
 * 	Denemo File IO 
 *
 * 	for Denemo, a gtk+ frontend to GNU Lilypond
 * 	(c) Adam Tee, Matthew Hiller 2000-2005
 * 	(c) University of Leeds 2000-2005
 *      (c) Richard Shann 2010
 */

#include "calculatepositions.h"
#include "commandfuncs.h"
#include "contexts.h"
#include <denemo/denemo.h>
#include "dialogs.h"
#include "exportabc.h"
#include "exportlilypond.h"
#include "file.h"
#include "moveviewport.h"
#include "staffops.h"
#include "scoreops.h"
#include "utils.h"
#include "exportxml.h"
#include "exportmidi.h"
#include "importxml.h"
#include "importmusicxml.h"
#include "importmidi.h"

#include "prefops.h"
#include "binreloc.h"
#include "view.h"
#include "lilydirectives.h"
#include "texteditors.h"
#include "print.h"
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>		/* check existance and type of files */
#include <dirent.h>		/* filter and sort filenames */



static gint
file_open (DenemoGUI * gui, DenemoSaveType template, ImportType type, gchar *filename);
static gint
file_import_lilypond (DenemoGUI * gui, DenemoSaveType template, ImportType type, gchar *filename);
static gint
file_import_midi (DenemoGUI * gui, DenemoSaveType template, ImportType type, gchar *filename);
static gint
file_import_musicxml (DenemoGUI * gui, DenemoSaveType template, ImportType type, gchar *filename);
static gboolean
replace_existing_file_dialog (const gchar * filename, GtkWindow * parent_window, gint format_id);

typedef enum
{ DENEMO_FORMAT = 0,
  DNM_FORMAT,
  MUDELA_FORMAT,
  PDF_FORMAT,
  PNG_FORMAT,
  ABC_FORMAT,
  MIDI_FORMAT,
  CSOUND_FORMAT,
  MUSICXML_FORMAT
}
FileFormatNames;

struct FileFormatData
{
  gchar *filename_mask;
  gchar *description;
  gchar *filename_extension;
  gboolean async; /* TRUE if uses async */
};

static struct FileFormatData supported_file_formats[] = {/* WARNING this array has to match the FileFormatNames enum above which is used to index it!!!!!!!!!"*/
  {"*.denemo", N_("Denemo XML format (*.denemo)"), ".denemo", 0},
  {"*.dnm", N_("Denemo XML format (*.dnm)"), ".dnm", 0},
  {"*.ly", N_("Lilypond (*.ly)"), ".ly", 0},
  {"*.pdf", N_("PDF (*.pdf)"), ".pdf", 1},
  {"*.png", N_("PNG Image format (*.png)"), ".png", 1},
  {"*.abc", N_("ABC (*.abc)"), ".abc", 0},
  {"*.mid", N_("Midi (*.mid, *.midi)"), ".mid", 0},
  {"*.sco", N_("CSound Score File (*.sco)"), ".sco", 0},
  {"*.mxml", N_("MusicXML file (*.mxml, *.xml)"), ".mxml", 0}
};

static gchar* supported_denemo_file_extension[] = {
  "*.denemo", "*.DENEMO" 
};
static gchar *supported_dnm_file_extension[] = {
  "*.dnm", "*.DNM" 
};
static gchar *supported_lilypond_file_extension[] = {
  "*.ly", "*.LY" 
};
static gchar *supported_midi_file_extension[] = {
  "*.midi", "*.mid", "*.MIDI", "*.MID" 
};
static gchar *supported_musicxml_file_extension[] = {
  "*.mxml", "*.MXML", "*.xml"
};
static gchar *supported_evince_file_extension[] = {
  "*.pdf", "*.PDF"
};
/* Some macros just to shorten lines */
#define FORMAT_MASK(i) supported_file_formats[i].filename_mask
#define FORMAT_DESCRIPTION(i) supported_file_formats[i].description
#define FORMAT_EXTENSION(i) supported_file_formats[i].filename_extension
#define FORMAT_ASYNC(i) supported_file_formats[i].async

struct FileDialogData
{
  DenemoSaveType template;
  gint format_id;
};

/* directory last used for saving */
static gchar *file_selection_path = NULL;
static gchar *system_template_path = NULL;
static gchar *system_example_path = NULL;
static gchar *local_template_path = NULL;
static gchar *default_template_path = NULL;


/**
 * Display a message box asking the user whether to save unsaved changes
 * or close without saving
 */
static gboolean
confirm_save (DenemoGUI *gui, gchar *primary, gchar *secondary)
{
  GtkWidget *dialog;
  gboolean r = FALSE;

  dialog = gtk_message_dialog_new (GTK_WINDOW (Denemo.window),
				   (GtkDialogFlags)
				   (GTK_DIALOG_MODAL |
				    GTK_DIALOG_DESTROY_WITH_PARENT),
				   GTK_MESSAGE_QUESTION,
				   GTK_BUTTONS_NONE,
				   "%s", primary);
  GtkWidget *d_save = gtk_dialog_add_button( (GtkDialog*)dialog, 
                                          "Close without Saving",
                                          GTK_RESPONSE_NO);
	
  GtkWidget *cancel = gtk_dialog_add_button( (GtkDialog*)dialog, 
                                          GTK_STOCK_CANCEL,
                                          GTK_RESPONSE_CANCEL);
	
  GtkWidget *save = gtk_dialog_add_button( (GtkDialog*)dialog, 
                                          GTK_STOCK_SAVE_AS,
                                          GTK_RESPONSE_YES);
	
  gtk_dialog_set_default_response( (GtkDialog*)dialog, GTK_RESPONSE_YES);
	
  gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog), "%s",
					    secondary);
  gtk_widget_show_all (dialog);
  gint response = gtk_dialog_run (GTK_DIALOG (dialog));

  if(response == GTK_RESPONSE_YES)
  {
	gtk_widget_destroy (dialog);
	file_saveas (gui, SAVE_NORMAL);
	if(gui->notsaved)
	  r = FALSE;
	else
	  r = TRUE;
  }
  else if(response == GTK_RESPONSE_NO)
  {
	gtk_widget_destroy (dialog);
	r = TRUE;
  }
  else
  {
	gtk_widget_destroy (dialog);
	r = FALSE;
  }
  return r;
}

/**
 * Display a message box asking the user to confirm that unsaved 
 * changes will be lost
 * @return TRUE if the OK button clicked or Enter pressed
 */
gboolean
confirmbox (DenemoGUI * gui) {
  gboolean ret;
  gchar *primary = g_strdup_printf(_("The score %s has unsaved changes"), gui->filename->len?gui->filename->str:"(Untitled)");
  ret = confirm_save (gui, primary,  _("Save changes?"));
  g_free(primary);
  return ret;
}

/**
 * Custom function to compare the elements of the History queue
 * it simply wraps up stcmp
 * 
 * @param a pointer to a queue elements
 * @param b pointer to the comparison value
 * @return gint 0 if match -1 or 1 otherwise
 */
static gint
history_compare (gconstpointer a, gconstpointer b)
{
  return (strcmp ((gchar *) a, (gchar *) b));
}


/**
 * Callback for the history menu
 * opens the selected file
 */
void
openrecent (GtkWidget * widget, gchar *filename)
{
  DenemoGUI *gui = Denemo.gui;
  if (!gui->notsaved || (gui->notsaved && confirmbox (gui)))
    {
      // deletescore(NULL, gui);
      if(open_for_real (filename, gui, FALSE, FALSE))
	{
	  gchar *warning = g_strdup_printf("Load of recently used file %s failed", filename);
	  warningdialog(warning);
	  g_free(warning);
	}
    }
}

/**
 * Decorate the window with the tile
 */
static void
set_gui_tabname (DenemoGUI * gui, gchar * filename)
{
  g_string_assign (gui->tabname, filename);
  set_title_bar (gui);
}
/**
 * Sets the filename for storing the passed in gui.
 * and adds it to the history
 */
static void
set_gui_filename (DenemoGUI * gui, gchar * filename)
{
  GList *link=NULL;
  g_string_assign (gui->filename, filename);
  set_gui_tabname (gui, filename);
  
  if ((link = g_queue_find_custom
       (Denemo.prefs.history, gui->filename->str, &history_compare)))
    g_queue_remove(Denemo.prefs.history, link->data);
  
  g_debug("max history now %d\n", Denemo.prefs.maxhistory);
  if (g_queue_get_length (Denemo.prefs.history) > Denemo.prefs.maxhistory)
    {
      gpointer data = g_queue_pop_head (Denemo.prefs.history);
      g_print("losing one history\n");
      if (data)
	g_free (data);
    }
  if(link) /* not a new one */
    addhistorymenuitem (filename);
  g_queue_push_tail (Denemo.prefs.history, g_strdup(gui->filename->str));  
}

static gchar *
strip_path_and_extension (gchar *filename) {
  gchar *basename;
  basename = g_path_get_basename (filename);
  (void)strtok(basename, ".");
  return basename;
} 

static void 
update_file_selection_path (gchar *file) {
  if(file_selection_path)
    g_free(file_selection_path);
  file_selection_path = g_path_get_dirname(file);
}

gint 
lyinput(gchar *filename, DenemoGUI *gui) {
  gchar *path = g_path_get_dirname(filename);
  gchar *base = g_path_get_basename(filename);
#ifdef G_OS_WIN32
  gchar *call = g_strescape(path, "");
  call = g_strdup_printf("%s%s%s%s%s", "(debug-set! stack 200000) (lyimport::load-file \"", call, "\\\\\" \"", base,"\")" );
  g_print("Calling %s\n", call);
#else
  gchar *call = g_strdup_printf("%s%s%c%s%s%s", "(lyimport::load-file \"", path, G_DIR_SEPARATOR,"\" \"", base,"\")" );
#endif

  
  call_out_to_guile(call);
  g_free(path);
  g_free(base);
  g_free(call);
  return 0;
}


/**
 * The function that actually determines the file type and calls the
 *  function that opens the file.  (So many layers of indirection...)  
 *filename must be full path
 * @return 0 for success non zero for failure
 */
gint
open_for_real (gchar * filename, DenemoGUI * gui, DenemoSaveType template, ImportType type)
{
  g_signal_handlers_block_by_func(G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_draw_event), NULL);
  gint result;
  gboolean xml = FALSE;
  result = 1;//FAILURE
#define EXISTS(extension) (strcmp (filename + strlen (filename) - strlen(extension), extension) == 0)
  if(g_file_test(filename, G_FILE_TEST_EXISTS)) 
    {
      if(EXISTS(".denemo"))
        xml=TRUE, result = importXML (filename, gui, type);
      else if(EXISTS(".dnm"))
        xml=TRUE, result = importXML (filename, gui, type);
      else if(EXISTS(".ly"))
        result = lyinput (filename, gui);
      else if(EXISTS(".mxml"))
        result = mxmlinput (filename, gui);
      else if(EXISTS(".mid") || EXISTS(".midi"))
        result = importMidi (filename, gui);
      else if(EXISTS(".pdf") || EXISTS(".PDF")) {// a .pdf file for transcribing from, does not affect the current score.
        g_signal_handlers_unblock_by_func(G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_draw_event), NULL);
        return !open_source (filename, 0, 0);
      }
#undef EXISTS
    }
  //printf("\nResult == %d type == %d template == %d xml == %d\n",result,type,template,(int)xml);
  if (result == 0)
    {
      if(!template) 
	{// not a template
	  update_file_selection_path (filename);
	  if(type==REPLACE_SCORE)
	    {
	      if (xml)
	        set_gui_filename (gui, filename);
	      else 
	        {
		  gchar *sname = strip_path_and_extension (filename);
		  set_gui_tabname (gui, sname);
		  g_free(sname);
	        }
	    }

	  if(type==ADD_STAFFS || type==ADD_MOVEMENTS)
	    score_status(gui, TRUE);
        } else 
	  g_string_assign (gui->filename, "");
      if(Denemo.printarea) 
	g_object_set_data(G_OBJECT(Denemo.printarea), "printviewupdate", (gpointer)G_MAXUINT);
      if(!xml)
	updatescoreinfo (gui);
      set_rightmeasurenum (gui->si);
      select_lyrics();
      set_bottom_staff (gui);
      update_hscrollbar (gui);
      update_vscrollbar (gui);
      gtk_widget_queue_draw (Denemo.scorearea);
      g_signal_emit_by_name (G_OBJECT (Denemo.hadjustment), "changed");
      g_signal_emit_by_name (G_OBJECT (Denemo.vadjustment), "changed");
      force_lily_refresh(gui);
    } else /*file load failed - gui may not be valid */
    deletescore(NULL, gui);
      
  g_signal_handlers_unblock_by_func(G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_draw_event), NULL);
  gui->si->undo_guard=1;


  denemo_scheme_init();//to re-instate any user defined directives for whole score
  if(!(type==ADD_STAFFS || type==ADD_MOVEMENTS))
	    score_status(gui, FALSE);

  rewind_audio();
  gui->si->undo_guard=Denemo.prefs.disable_undo;//user pref to (dis)allow undo information to be collected
  return result;
}

/*
	If the filename format already has the correct extension use
	it. otherwise add the file name extension 
*/
static gchar * 
create_filename (const gchar * file_name, gint format_id)
{
  if (g_pattern_match_simple (FORMAT_MASK (format_id), file_name))
    return (g_strdup (file_name));
  else 
    return (g_strconcat (file_name, FORMAT_EXTENSION (format_id), NULL));
}

/*
  This is used to strip the extension (if it exists) off of ASYNC file types.
*/
static gchar *
strip_filename_ext (const gchar * file_name, gint format_id)
{
    gchar *ext = strrchr(file_name, '.');
    if (ext == NULL)
      return file_name;
    gint i;
    GString *file_name_stripped = g_string_new ("");
    gint filename_size = strlen(file_name);
    gint ext_size = strlen(FORMAT_EXTENSION (format_id));
    if (strlen(ext) != ext_size)
      return file_name;
    gint stripped_filename_size = filename_size - ext_size;
    for (i=0;i < stripped_filename_size;i++){
      g_string_append_c(file_name_stripped, file_name[i]);
    }
    printf("\nTruncated filename == %s\n", file_name_stripped->str);
    return g_string_free(file_name_stripped, FALSE);
}

/* Save gui in the file in format format_id to the file filename (or gui->filename
   if filename is NULL)
   If there is a scheme script, offers to save that with the file.
 */
static void 
save_in_format(gint format_id, DenemoGUI * gui, gchar *filename) {
  gchar *file = filename? filename:gui->filename->str;
  switch (format_id)
    {
    case DENEMO_FORMAT:
    case DNM_FORMAT:
      {
	/* HERE examine Denemo.Script and 
         * if present ask it it should be 
         * saved with the file, if not 
         * delete the script.
	 */
	    
	if(getNumCharsSchemeText())
	  if(!confirm("You have a Script defined", 
		      "Use this script every time this file is opened?")) 
	  {
	    deleteSchemeText();
	  }										 
	exportXML (file, gui, 0, 0);
	break;
      };
    case MUDELA_FORMAT:
      {
	gui->si->markstaffnum = 0;
	exportlilypond (file, gui, TRUE);
	break;
      };
    case PDF_FORMAT:
      {
	gui->si->markstaffnum = 0;
        export_pdf (file, gui);
        break;
      };
    case PNG_FORMAT:
      {
	gui->si->markstaffnum = 0;
	export_png (file, (GChildWatchFunc)printpng_finished, gui);
        break;
      };
    case ABC_FORMAT:
      {
	exportabc (file, gui, 0, 0);
	break;
      };
    case MIDI_FORMAT:
      {
	exportmidi (file, gui->si, 0, 0);
	break;
      };
    default:
      break;
    };
}

/**
 * File save called by fileselsave callback
 * param file_name is full path to file possibly with extension
 */
static void
filesel_save (DenemoGUI * gui, const gchar * file_name, gint format_id, DenemoSaveType template)
{
  g_assert (gui != NULL);
  g_assert (file_name != NULL);
  g_assert (format_id >= 0 && format_id <
	    (int) G_N_ELEMENTS (supported_file_formats));

  DenemoScore *si = gui->si;
  // Append file extension if needed
  gchar *file = NULL;
  gchar *basename = NULL;
  file = create_filename(file_name, format_id);
  if(!template && format_id==DENEMO_FORMAT) {
    update_file_selection_path(file);
    set_gui_filename (gui, file);
  }
  basename = g_path_get_basename (file);
  if (basename[0] != '.') // avoids empty filename
    {
      if (FORMAT_ASYNC(format_id))
        save_in_format(format_id, gui, strip_filename_ext(file_name, format_id));
      else
        save_in_format(format_id, gui, file);
 
      /*export parts as lilypond files*/
      if(Denemo.prefs.saveparts)
	export_lilypond_parts(file,gui);
      si->readonly = FALSE;
    }
  g_free(basename);
  g_free(file);
}

/* set local_template_path up */
static void
init_local_path(void) {
      local_template_path = g_build_filename (locatedotdenemo(), "templates", NULL);
      gboolean err = g_mkdir_with_parents(local_template_path, 0770);
      if(err) {
	warningdialog("Could not create .denemo/templates for you personal templates");
	g_free(local_template_path);
	local_template_path = NULL;
      }
}

typedef enum {
  LOCAL,
  SYSTEM,
  EXAMPLE
} TemplateType;

/*
 * Sets the file_selection_path to the templates directory and 
 * calls file_open to create the file selection dialog
 * LOCAL whether to use the local templates or systemwide templates or examples
 * does nothing if unable to access templates
 * filename is NULL for interactive use, otherwise file base name
 */
static gint
template_open (DenemoGUI * gui, TemplateType local, gchar *filename)
{
  gboolean ret = FALSE;
  if(local==LOCAL) {
    if(local_template_path==NULL) {
      init_local_path();
    }
    default_template_path = local_template_path;   
  } else if(local==EXAMPLE){
    if(system_example_path==NULL) {
      system_example_path = g_build_filename (get_data_dir (), "examples", NULL);
      GDir *denemo_path = g_dir_open(system_example_path, 0, NULL);
      if(denemo_path == NULL) {
	warningdialog ("No examples directory in installation");
	system_example_path = NULL;
      }
    }
    default_template_path = system_example_path;
  } else{ 
    if(system_template_path==NULL) {
      system_template_path = g_build_filename (get_data_dir (), "templates", NULL);
      GDir *denemo_path = g_dir_open(system_template_path, 0, NULL);
      if(denemo_path == NULL) {
	warningdialog ("No templates directory in installation");
	system_template_path = NULL;
      }
    }
    default_template_path = system_template_path;
  }
  if(default_template_path) {
    gchar *filepath = g_build_filename(default_template_path, filename, NULL);
    ret = file_open (gui, TRUE, REPLACE_SCORE, filepath);
    g_free(filepath);
    gui->filename = g_string_new("");
    gui->tabname = g_string_new("");
  }
  return ret;
}

#define OPEN_WITH_CHECK(dir) \
GET_1PARAM(action, param, filename); \
  DenemoGUI *gui = Denemo.gui; \
  if (gui->notsaved) \
    { \
      if (filename==NULL && confirmbox (gui)) \
	{ \
	  param->status = !template_open (gui, dir, filename); \
	} \
    } \
  else \
    { \
      param->status = !template_open (gui, dir, filename); \
    } 

/*
 * Open system template file callback function 
 */
void
system_template_open_with_check (GtkAction * action, DenemoScriptParam * param)
{
  OPEN_WITH_CHECK(SYSTEM)  
}

/*
 * Open system template file callback function 
 */
void
system_example_open_with_check (GtkAction * action, DenemoScriptParam * param)
{
  OPEN_WITH_CHECK(EXAMPLE)  
}
/*
 * Open local template file callback function 
 */
void
local_template_open_with_check (GtkAction * action, DenemoScriptParam * param)
{
  OPEN_WITH_CHECK(LOCAL)
}

/**
 * Wrapper function for opening a file, d-Open
 * if no param checks to see if current score has changed and prompts user to save 
 * otherwise opens the file
 */
void
file_open_with_check (GtkAction * action, DenemoScriptParam * param)
{
  GET_1PARAM(action, param, filename);
  if(query){
    param->status = (Denemo.gui->filename!=NULL) && Denemo.gui->filename->len;
    if(param->status)
      g_string_assign(param->string, Denemo.gui->filename->str);
    return;
  }
  DenemoGUI *gui = Denemo.gui;
  if (!gui->notsaved ||  (gui->notsaved && (confirmbox (gui))))
    {
      param->status = !file_open (gui, FALSE, REPLACE_SCORE, filename);
    }
}

#define IMPORT(import_type) \
  GET_1PARAM(action, param, filename); \
  if(query){ \
    param->status = (Denemo.gui->filename!=NULL) && Denemo.gui->filename->len; \
    if(param->status) \
      g_string_assign(param->string, Denemo.gui->filename->str); \
    return; \
  } \
  DenemoGUI *gui = Denemo.gui; \
  param->status = !file_import_##import_type (gui, FALSE, REPLACE_SCORE, filename); 

void
file_import_lilypond_with_check (GtkAction * action, DenemoScriptParam * param)
{
  IMPORT(lilypond)
}

void
file_import_midi_with_check (GtkAction * action, DenemoScriptParam * param)
{
  IMPORT(midi)
}

void
file_import_musicxml_with_check (GtkAction * action, DenemoScriptParam * param)
{
  IMPORT(musicxml)
}

#define ADD(insertion_strategy)\
  DenemoGUI *gui = Denemo.gui;\
  GET_1PARAM(action, param, filename);\
  if(filename==NULL && !confirm_insertstaff_custom_scoreblock(gui))\
    return;\
  param->status = !file_open(gui, FALSE, insertion_strategy, filename);\
  score_status(gui, TRUE);\

/**
 * Wrapper function for opening a file to add movements to the current score
 * 
 */
void
file_add_movements(GtkAction * action,  DenemoScriptParam * param){
  ADD(ADD_MOVEMENTS)
}
/**
 * Wrapper function for opening a file to add staffs to the current movement
 * 
 */
void
file_add_staffs(GtkAction * action,  DenemoScriptParam * param){
  ADD(ADD_STAFFS)
}

static void
set_current_folder(GtkWidget *file_selection, DenemoGUI *gui, DenemoSaveType template) {
  gchar *path, *fallback;
  if(template==SAVE_TEMPLATE) {
    fallback = path = default_template_path;
  } else {
    fallback = path = file_selection_path;
    GDir *denemo_path = g_dir_open(Denemo.prefs.denemopath->str, 0, NULL);
    if(denemo_path != NULL)
      {
	g_dir_close(denemo_path);
	fallback = Denemo.prefs.denemopath->str;
      } 
  }
  if (path != NULL)
    {
      gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (file_selection),
					   path);
    } else {
      if(fallback != NULL)
	gtk_file_chooser_set_current_folder 
	  (GTK_FILE_CHOOSER (file_selection), fallback);
    } 
}

gchar *
file_dialog(gchar *message, gboolean type, gchar *location){
  GtkWidget *file_selection;
  GtkFileFilter *filter;
  gchar *filename;
  file_selection = gtk_file_chooser_dialog_new (message,
						GTK_WINDOW (Denemo.window),
						type?
						  GTK_FILE_CHOOSER_ACTION_OPEN:
						  GTK_FILE_CHOOSER_ACTION_SAVE,
						GTK_STOCK_CANCEL,
						GTK_RESPONSE_REJECT,
						type?
						  GTK_STOCK_OPEN:
						  GTK_STOCK_SAVE,
						GTK_RESPONSE_ACCEPT, NULL);

  if (location) 
    gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (file_selection),
					   location);
  gtk_dialog_set_default_response (GTK_DIALOG (file_selection),
				   GTK_RESPONSE_ACCEPT);
  gtk_widget_show_all (file_selection);
  if (gtk_dialog_run (GTK_DIALOG (file_selection)) == GTK_RESPONSE_ACCEPT)
    filename =
	gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (file_selection));
  else
    filename = NULL;
  gtk_widget_destroy (file_selection);\
  return filename;
}

static void
update_preview_cb(GtkFileChooser *file_chooser, gpointer data){

  GtkWidget *preview;
  gchar *thumb_filename;
  gchar *selection_filename;
  GdkPixbuf *pixbuf;
  gboolean have_preview;

  preview = GTK_WIDGET(data);
  selection_filename = gtk_file_chooser_get_preview_filename(file_chooser);
  thumb_filename = large_thumbnail_name(selection_filename); 
  pixbuf = gdk_pixbuf_new_from_file_at_size(thumb_filename, 512,512, NULL);
  have_preview = (pixbuf !=NULL);
  
  printf("\n# %s for %s thumbnail = %s\n",have_preview? "We have a thumbnail generated":
				     "We have not yet generated a thumbnail",
 				     selection_filename,
				     thumb_filename);
  
  g_free(selection_filename);
  g_free(thumb_filename);

  gtk_image_set_from_pixbuf (GTK_IMAGE (preview), pixbuf);
  if(pixbuf)
    gdk_pixbuf_unref(pixbuf);
  
  gtk_file_chooser_set_preview_widget_active(file_chooser, have_preview);
}


#define FILE_OPEN_DIALOG(message, format, save_type) \
  gboolean ret = -1;\
  if(filename && !g_file_test(filename, G_FILE_TEST_IS_DIR))\
    return (open_for_real(filename, gui, template, type));\
  \
  GtkWidget *file_selection;\
  GtkFileFilter *filter;\
  gint i;\
  \
  file_selection = gtk_file_chooser_dialog_new (_(message),\
						GTK_WINDOW (Denemo.window),\
						GTK_FILE_CHOOSER_ACTION_OPEN,\
						GTK_STOCK_CANCEL,\
						GTK_RESPONSE_REJECT,\
						GTK_STOCK_OPEN,\
						GTK_RESPONSE_ACCEPT, NULL);\
  /* Open the last visited directory, if any. */\
  set_current_folder(file_selection, gui, template);\
  \
  filter = gtk_file_filter_new ();\
  gtk_file_filter_set_name (filter, FORMAT_DESCRIPTION(save_type));\
  \
  for (i = 0; i < (gint) G_N_ELEMENTS (supported_##format##_file_extension); i++)\
    gtk_file_filter_add_pattern (filter, supported_##format##_file_extension[i]); \
  gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (file_selection), filter);\
  gtk_dialog_set_default_response (GTK_DIALOG (file_selection),\
				   GTK_RESPONSE_ACCEPT);\
  gtk_widget_show_all (file_selection);\
  GtkWidget *preview;\
  preview = gtk_image_new();\
  gtk_file_chooser_set_preview_widget(GTK_FILE_CHOOSER (file_selection), preview);\
  g_signal_connect (GTK_FILE_CHOOSER(file_selection), "update-preview",\
			G_CALLBACK (update_preview_cb), preview);\
  gtk_widget_show_all (preview);\
  if (gtk_dialog_run (GTK_DIALOG (file_selection)) == GTK_RESPONSE_ACCEPT)\
    {\
      gchar *name =\
	gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (file_selection));\
      if((ret=open_for_real (name, gui, template, type))) {\
	gchar *warning = g_strdup_printf("Load of file %s failed", name);\
	warningdialog(warning);\
	g_free(warning);\
      }\
      g_free (name);\
    }\
  gtk_widget_destroy (file_selection);\
  return ret;\

/**
 * File open dialog - opened where appropriate 
 * return 0 on success non-zero on failure.
 * filename must be full path or NULL for dialog
 */
static gint
file_open (DenemoGUI * gui, DenemoSaveType template, ImportType type, gchar *filename)
{
  FILE_OPEN_DIALOG("Open", denemo, DENEMO_FORMAT)
  if(getNumCharsSchemeText())
	  executeScript(); 
}

gint open_source_file(void){
  gchar *filename = NULL;
  ImportType type = 0;
  DenemoSaveType template = 0;
  DenemoGUI *gui = Denemo.gui;
  FILE_OPEN_DIALOG("Open", evince, PDF_FORMAT);
}
/**
 * Lilypond Import dialog - opened where appropriate 
 * return 0 on success non-zero on failure.
 * filename must be full path or NULL for dialog
 */
static gint
file_import_lilypond (DenemoGUI * gui, DenemoSaveType template, ImportType type, gchar *filename)
{
  FILE_OPEN_DIALOG("Import Lilypond", lilypond, MUDELA_FORMAT)  
}

/**
 * Midi Import dialog - opened where appropriate 
 * return 0 on success non-zero on failure.
 * filename must be full path or NULL for dialog
 */
static gint
file_import_midi (DenemoGUI * gui, DenemoSaveType template, ImportType type, gchar *filename)
{
  FILE_OPEN_DIALOG("Import Midi", midi, MIDI_FORMAT) 
}

/**
 * MusicXML Import dialog - opened where appropriate 
 * return 0 on success non-zero on failure.
 * filename must be full path or NULL for dialog
 */
static gint
file_import_musicxml (DenemoGUI * gui, DenemoSaveType template, ImportType type, gchar *filename)
{
  FILE_OPEN_DIALOG("Import MusicXML", musicxml, MUSICXML_FORMAT)
}

/**
 * Wrapper function to save the current file if not already 
 * saved.
 */
void
file_saveaswrapper (GtkAction * action, DenemoScriptParam *param)
{
  GET_1PARAM(action, param, filename);
  DenemoGUI *gui = Denemo.gui;
  if(filename==NULL) {
    file_saveas (gui, FALSE);
  } else {
      filesel_save (gui, filename, DENEMO_FORMAT, FALSE);
      force_lily_refresh(gui);
    }
}

/**
 * Wrapper function to save the current file as template
 */
void
template_save (GtkAction * action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  init_local_path();
  default_template_path = local_template_path;
  file_saveas (gui, SAVE_TEMPLATE);
}


/**
 * Wrapper function to save the current file as a copy
 */
void
file_copy_save (GtkAction * action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  init_local_path();
  file_saveas (gui, SAVE_COPY);
}

/**
 * Wrapper function for saving an existing file
 *
 */
void
file_savewrapper (GtkAction * action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  file_save (NULL, gui);
}

/**
 * if gui->filename exists saves gui to the filename  based on its extension
 * otherwise call saveas routine
 */
void
file_save (GtkWidget * widget, DenemoGUI * gui)
{
  DenemoScore *si = gui->si;
  g_print ("READONLY %d\n", si->readonly);
  if ((gui->filename->len == 0)/* || (si->readonly == TRUE)*/)
    /* No filename's been given or is opened from template */
    file_saveas (gui, FALSE);
  else
    save_in_format(DENEMO_FORMAT, gui, NULL);
  
  /*Save parts as lilypond files*/   
  if(Denemo.prefs.saveparts)
    export_lilypond_parts(gui->filename->str,gui);
  
  score_status(gui, FALSE);
}

static void
file_dialog_response(GtkWidget *dialog, gint response_id, struct FileDialogData *data)
{
  DenemoGUI *gui = Denemo.gui;
  if (response_id == GTK_RESPONSE_ACCEPT){
    gchar *file_name =
      gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
    if (replace_existing_file_dialog
          (file_name, GTK_WINDOW (Denemo.window), data->format_id)){
      filesel_save (gui, file_name, data->format_id, data->template);
      force_lily_refresh(gui);
    }
    g_free (file_name);
  }
  gtk_widget_destroy (dialog);
  g_free(data);
}


#define FILE_SAVE_DIALOG(description)\
  GtkWidget *file_selection;\
  GtkWidget *label;\
  GtkWidget *hbox;\
  GtkFileFilter *filter;\
  file_selection = gtk_file_chooser_dialog_new (description,\
						GTK_WINDOW (Denemo.window),\
						GTK_FILE_CHOOSER_ACTION_SAVE,\
						GTK_STOCK_CANCEL,\
						GTK_RESPONSE_REJECT,\
						GTK_STOCK_SAVE,\
						GTK_RESPONSE_ACCEPT, NULL);\
  /*set default folder for saving */\
  set_current_folder(file_selection, gui, SAVE_NORMAL);\
  \
  /* assign title */ \
  gchar *title = get_scoretitle();\
  if (title)\
    { \
      gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (file_selection), title);\
    } \
  \
  filter = gtk_file_filter_new ();\
  gtk_file_filter_set_name (filter, FORMAT_DESCRIPTION(format_id));\
  gtk_file_filter_add_pattern (filter, FORMAT_MASK(format_id));\
  gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (file_selection), filter);\
  gtk_dialog_set_default_response (GTK_DIALOG (file_selection),\
				   GTK_RESPONSE_ACCEPT);\
  gtk_widget_show_all (file_selection);\
  struct FileDialogData *data = (struct FileDialogData *) g_malloc (sizeof (struct FileDialogData));\
  data->template=template;\
  data->format_id=format_id;\
  g_signal_connect(file_selection, "response", G_CALLBACK(file_dialog_response), data);

/**
 * Create file saveas dialog to enable user to export the current file to
 *
 *
 */
void
file_export (DenemoGUI * gui, FileFormatNames format_id)
{
  gchar *description = g_strconcat(_("Export As "), FORMAT_DESCRIPTION(format_id), NULL);
  DenemoSaveType  template = FALSE;
  FILE_SAVE_DIALOG(description)
  g_free(description);
}

/**
 * Create file saveas dialog to enable user to save the current file to
 *
 *
 */
void
file_saveas (DenemoGUI * gui, DenemoSaveType  template)
{
  gint format_id = DENEMO_FORMAT;
  FILE_SAVE_DIALOG(_("Save As"))
}

/**
 * Wrapper function for command New which asks to delete the current gui and on success creates an empty score
 *
 */
void
file_newwrapper (GtkAction * action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  if (gui->notsaved)
    {
      if (confirmbox (gui))
	{
	  deletescore(NULL, gui);
	}
      else
	return;
    }
  else
    {
      deletescore(NULL, gui);
    }
  //open_user_default_template(REPLACE_SCORE);
  load_initdotdenemo();
  set_enharmonic_position(0);
  if(Denemo.printarea) 
    g_object_set_data(G_OBJECT(Denemo.printarea), "printviewupdate", (gpointer)G_MAXUINT);
  score_status(gui, FALSE);
}

/* open_user_default_template
 * open the user's standard template if there is one
 * @return 0 for success non zero for failure
 **/
gint
open_user_default_template(ImportType type) {
  gint ret;
  gchar *filename = g_build_filename(locatedotdenemo(), "templates", "default.denemo", NULL);
  if(g_file_test(filename, G_FILE_TEST_EXISTS)) { 
    ret = open_for_real(filename, Denemo.gui, TRUE, type);
  }
  g_free(filename);
  return ret;
}

/* load local init.denemo or failing that system wide template file init.denemo*/
void
load_initdotdenemo(void) {
   gchar *init_file;

   init_file = g_build_filename(locatedotdenemo (), "actions", "init.denemo", NULL);
   if(g_file_test(init_file, G_FILE_TEST_EXISTS)) {
     if(open_for_real (init_file, Denemo.gui, TRUE, REPLACE_SCORE))
       g_warning("Could not open %s\n", init_file);
   } else {
     g_free(init_file);
     init_file = g_build_filename(get_data_dir (), "actions", "init.denemo", NULL);
     if (open_for_real (init_file, Denemo.gui, TRUE, REPLACE_SCORE) == -1)
       g_warning("Denemo initialization file %s not found", init_file);
     g_free(init_file);
   }
   deleteSchemeText();
} 

/**
 * Creates dialog to say that the chosen filename already exists
 * and do you want to overwrite it.
 *
 */
static gboolean
replace_existing_file_dialog (const gchar * filename,
			      GtkWindow * parent_window, gint format_id)
{
  gboolean ret;
  gchar *file = create_filename (filename, format_id);
  if (!g_file_test (file, G_FILE_TEST_EXISTS))
    {
      g_free (file);
      return TRUE;
    }
  
  gchar *primary = g_strdup_printf(_("A file with the name %s already exists"), file);
  ret = confirm (primary,  _("Do you want to replace it?"));
  
  g_free (file);
  g_free (primary);
  return ret;
}


/**
 * Save parts to individual files
 */
void
file_savepartswrapper (GtkAction * action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  if (gui->filename->len==0)
    {
      file_saveas (gui, FALSE);
    }

  export_lilypond_parts (gui->filename->str, gui);
}


static void 
selection_received (GtkClipboard *clipboard, const gchar *text, gpointer data) {
  if(!text) {
    warningdialog("No selection text available");
    return;
  }
  gchar *filename = g_build_filename (locatedotdenemo(), "denemopaste.ly", NULL);
  FILE *fp = fopen(filename, "w");
  if(fp){
    fprintf(fp, "music = { %s }\n\\score {\n\\music\n\\layout {}\n}\n", text);
    fclose(fp);
    gint theclef = find_prevailing_clef(Denemo.gui->si);
    newview(NULL, NULL);
    gint fail = open_for_real(filename, Denemo.gui, TRUE, REPLACE_SCORE);
    //thescore can be NULL after failed load....
    if(fail) {
      DenemoGUI *gui = Denemo.gui;
      //FIXME repeated code
      free_movements(gui);  
      gtk_widget_destroy (Denemo.page);
      Denemo.guis = g_list_remove (Denemo.guis, gui);
      g_free (gui);
      warningdialog("Could not interpret selection as LilyPond notes");
      return;
    }
    dnm_setinitialclef(Denemo.gui->si, (DenemoStaff*)Denemo.gui->si->currentstaff->data, theclef);
    call_out_to_guile("(while (None?) (d-DeleteStaff))");
    if(confirm("Paste from Selection", "Paste this music into your score?")) {
      DenemoGUI *gui = Denemo.gui;
      tohome(NULL, NULL);
      set_mark(gui);
      toend(NULL, NULL);
      copywrapper(NULL, NULL);
      free_movements(gui);  
      gtk_widget_destroy (Denemo.page);
      Denemo.guis = g_list_remove (Denemo.guis, gui);
      g_free (gui);
      pastewrapper(NULL, NULL);
    }
  }
}

void
paste_clipboard(GtkAction * action, gpointer param) {
  if(Denemo.gui != g_list_last(Denemo.guis)->data) {
    warningdialog("Can only paste LilyPond text into the last tab, sorry");
    return;
  }
  GtkClipboard* clipboard = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
  gtk_clipboard_request_text (clipboard, (GtkClipboardTextReceivedFunc) selection_received, NULL);

}

#define EXPORT_INTERFACE(format_id) \
  GET_1PARAM(action, param, filename); \
  DenemoGUI *gui = Denemo.gui; \
  if (filename==NULL) \
    file_export(gui, format_id); \
  else \
    if (replace_existing_file_dialog \
       (filename, GTK_WINDOW (Denemo.window), format_id)){ \
      filesel_save (gui, filename, format_id, SAVE_COPY); \
      force_lily_refresh(gui); \
    }  


/**
 * Export mudela callback prompts for filename
 *
 */
void
export_mudela_action (GtkAction *action, DenemoScriptParam *param)
{
  EXPORT_INTERFACE(MUDELA_FORMAT)
}

/**
 * Export pdf callback prompts for filename
 *
 */
void
export_pdf_action (GtkAction *action, DenemoScriptParam *param)
{
  EXPORT_INTERFACE(PDF_FORMAT)
}

/**
 * Export pdf callback prompts for filename
 *
 */
void
export_png_action (GtkAction *action, DenemoScriptParam *param)
{
  EXPORT_INTERFACE(PNG_FORMAT)
}

/**
 * Export ABC callback prompts for filename
 *
 */
void
export_ABC_action (GtkAction *action, DenemoScriptParam *param)
{
  EXPORT_INTERFACE(ABC_FORMAT)
}

/**
 * Export MIDI callback prompts for filename
 *
 */
void
export_midi_action (GtkAction *action, DenemoScriptParam *param)
{
  EXPORT_INTERFACE(MIDI_FORMAT)
}
