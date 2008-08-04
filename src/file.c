/*	file.cpp
 * 	Denemo File IO 
 *
 * 	for Denemo, a gtk+ frontend to GNU Lilypond
 * 	(c) Adam Tee, Matthew Hiller 2000-2005
 * 	(c) University of Leeds 2000-2005
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
#include "exportcsound.h"
#include "importmidi.h"
#include "lyparserfuncs.h"
/*#include "lyparser.h"*/
#include "prefops.h"
#include "binreloc.h"
#include "view.h"

#include <stdio.h>
#include <string.h>
#include <sys/stat.h>		/* check existance and type of files */
#include <dirent.h>		/* filter and sort filenames */
#include <malloc.h>		/* I use free() */



static void
file_open (DenemoGUI * gui, gboolean template, ImportType type);



typedef enum
{ DENEMO_FORMAT = 0,
  DNM_FORMAT,
  MUDELA_FORMAT,
  PNG_FORMAT,
  ABC_FORMAT,
  MIDI_FORMAT,
  CSOUND_FORMAT
}
FileFormatNames;

/* Keep this up to date ! */

#define FIRST_FORMAT_NAME DENEMO_FORMAT
#define LAST_FORMAT_NAME CSOUND_FORMAT

struct FileFormatData
{
  gchar *filename_mask;
  gchar *description;
  gchar *filename_extension;
};

static struct FileFormatData supported_import_file_formats[] = {
  {"*.denemo", N_("Denemo XML format (*.denemo)"), ".denemo"},
  {"*.dnm", N_("Denemo XML format (*.dnm)"), ".dnm"},
  {"*.ly", N_("Lilypond (*.ly)"), ".ly"},
  {"*.mid", N_("Midi (*.mid)"), ".mid"},
  {"*.midi", N_("Midi (*.midi)"), ".midi"}
};


static struct FileFormatData supported_export_file_formats[] = {
  {"*.denemo", N_("Denemo XML format (*.denemo)"), ".denemo"},
  {"*.dnm", N_("Denemo XML format (*.dnm)"), ".dnm"},
  {"*.ly", N_("Lilypond (*.ly)"), ".ly"},
  {"*.png", N_("png image format (*.png)"), ".png"},
  {"*.abc", N_("ABC (*.abc)"), ".abc"},
  {"*.mid", N_("Midi (*.mid)"), ".mid"},
  {"*.sco", N_("CSound Score File (*.sco)"), ".sco"}
};

/* Some macros just to shorten lines */
#define FORMAT_MASK(i) supported_export_file_formats[i].filename_mask
#define FORMAT_DESCRIPTION(i) supported_export_file_formats[i].description
#define FORMAT_EXTENSION(i) supported_export_file_formats[i].filename_extension

#define COLUMN_NAME (0)
#define COLUMN_ID (1)

struct callbackdata
{
  struct scoreinfo *si;
  GtkWidget *fs;
  GtkWidget *comboentry;
};

/* directory last used for saving */
static gchar *file_selection_path = NULL;
static gchar *system_template_path = NULL;
static gchar *system_example_path = NULL;
static gchar *local_template_path = NULL;
static gchar *default_template_path = NULL;

/* Prototypes for non-exported functions */
static gint guess_file_format (gchar * file_name);



/**
 * Display a message box asking the user to confirm that unsaved 
 * changes will be lost
 * @return TRUE if the OK button clicked or Enter pressed
 */
gboolean
confirmbox (DenemoGUI * gui) {
  gboolean ret;
  gchar *primary = g_strdup_printf(_("The score %s has unsaved changes"), gui->filename->len?gui->filename->str:"(Untitled)");
  ret = confirm (primary,  _("Discard changes?"));
  g_free(primary);
  return ret;
}




/**
 * Recalculates the stored information about a movement
 * either gui->si or if that does exist yet, gui->movements->data, the first movement.(FIXME)
 *
 * @param gui pointer to the gui structure
 */
void
updatescoreinfo (DenemoGUI * gui)
{
  staffnode *curstaff;
  DenemoScore *si;
  GList *g = gui->movements;
  if(g)
    si = g->data;
  else
    si = gui->si;
  do {
  for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
    {
      beamsandstemdirswholestaff ((DenemoStaff *) curstaff->data);
      showwhichaccidentalswholestaff ((DenemoStaff *) curstaff->data);
    }
  find_xes_in_all_measures (si);
  find_leftmost_allcontexts (si);

  si->currentstaff = si->thescore;
  si->currentmeasure = firstmeasurenode (si->currentstaff);
  si->currentobject = firstobjnode (si->currentmeasure);
  if (!si->currentobject)
    si->cursor_appending = TRUE;
  else
    si->cursor_appending = FALSE;
  si->leftmeasurenum = si->currentstaffnum = si->currentmeasurenum = 1;
  } while(g && (g=g->next) && (si=g->data));
  score_status(gui, FALSE);
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
 * Sets the filename for storing the passed in gui.
 * decorates the title bar with it and adds it to the history
 */
static void
set_gui_filename (DenemoGUI * gui, gchar * filename)
{
  g_string_assign (gui->filename, filename);
  set_title_bar (gui);
  
  if (!g_queue_find_custom
      (Denemo.prefs.history, gui->filename->str, &history_compare))
    {
#ifdef DEBUG
      g_print ("%s not in history list\n", gui->filename->str);
#endif
      if (g_queue_get_length (Denemo.prefs.history) > MAX_HISTORY)
	{
	  gpointer data = g_queue_pop_head (Denemo.prefs.history);
	  if (data)
	    g_free (data);
	}
      addhistorymenuitem (filename);
      g_queue_push_tail (Denemo.prefs.history, g_strdup(gui->filename->str));
    }
}


static void      update_file_selection_path (gchar *file) {
  if(file_selection_path)
    g_free(file_selection_path);
  file_selection_path = g_path_get_dirname(file);
}




/**
 * The function that actually determines the file type and calls the
 *  function that opens the file.  (So many layers of indirection...)  
 *
 * @return 0 for success non zero for failure
 */
gint
open_for_real (gchar * filename, DenemoGUI * gui, gboolean template, ImportType type)
{
  gint result;
  result = 1;//FAILURE
  if (strcmp (filename + strlen (filename) - 7, ".denemo") == 0)
    result = importXML (filename, gui, type);
  else if (strcmp (filename + strlen (filename) - 4, ".dnm") == 0)
    result = importXML (filename, gui, type);
  else if (strcmp (filename + strlen (filename) - 3, ".ly") == 0)
    result = lyinput (filename, gui);
  else if (strcmp (filename + strlen (filename) - 4, ".mid") == 0 ||
	   strcmp (filename + strlen (filename) - 5, ".midi") == 0)
    result = importMidi (filename, gui);
  if (result == 0)
    {
      if(!template) {// not a template
	update_file_selection_path (filename);
	if(type==REPLACE_SCORE)
	  set_gui_filename (gui, filename);
	if(type==ADD_STAFFS || type==ADD_MOVEMENTS)
	  score_status(gui, TRUE);
      }
      updatescoreinfo (gui);
      set_rightmeasurenum (gui->si);
      set_bottom_staff (gui);
      update_hscrollbar (gui);
      update_vscrollbar (gui);
      gtk_widget_queue_draw (gui->scorearea);
      gtk_signal_emit_by_name (GTK_OBJECT (gui->hadjustment), "changed");
      gtk_signal_emit_by_name (GTK_OBJECT (gui->vadjustment), "changed");
      gui->lilysync = G_MAXUINT;//FIXME move these two lines into a function, they force refresh of lily text
      refresh_lily_cb(NULL, gui);
    }
  return result;
}

/**
 * denemo_warning prompts the user to save the work in the denemo  
 * format if not done so.
 * @param si pointer to the denemo score object
 * @param format_id the numeric id of the files format
 * @return none
 */

static void
denemo_warning (DenemoGUI * gui, gint format_id)
{
  DenemoScore *si = gui->si;

  if (format_id != DENEMO_FORMAT && format_id != DNM_FORMAT)
    {
      GtkWidget *dialog;
      dialog = gtk_message_dialog_new (NULL,
				       GTK_DIALOG_DESTROY_WITH_PARENT,
				       GTK_MESSAGE_WARNING,
				       GTK_BUTTONS_YES_NO,
				       "You have made changes to your document that was not saved as denemo file."
				       " I advise you save your work now as a denemo file to easily continue work later. Save as denemo?");
      gtk_dialog_set_default_response (GTK_DIALOG (dialog),
				       GTK_RESPONSE_ACCEPT);
      if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_YES)
	{

	  gchar **file = g_strsplit (gui->filename->str, ".", 0);

	  file[0] = g_strconcat (file[0], ".denemo", NULL);
	  g_print ("file %s\n", file[0]);
	  exportXML (file[0], gui, 0, 0);
	  g_strfreev (file);
	}
      gtk_widget_destroy (dialog);
    }


}

/*
	If the filename already has a denemo file name extension use
	it regardless of the value of format_id, otherwise add the 
	file name extension 
*/
static gchar * 
create_filename (const gchar * file_name, gint *format_id)
{
  gint i;

  if (*format_id < 0)
        return (g_strdup (file_name));

  for (i = 0; i < (gint) G_N_ELEMENTS (supported_export_file_formats); i++)
    {
  	if (g_pattern_match_simple (FORMAT_MASK (i), file_name))
    	{
	     *format_id = i;
     	     return (g_strdup (file_name));
    	}
    }
  return (g_strconcat (file_name, FORMAT_EXTENSION (*format_id), NULL));
}

/**
 * File save called by fileselsave callback
 * param file_name is full path to file possibly with extension
 * FIXME duplicate code with file_save
 */
static void
filesel_save (DenemoGUI * gui, const gchar * file_name, gint format_id, gboolean template)
{
  g_assert (gui != NULL);
  g_assert (file_name != NULL);
  g_assert (format_id >= 0 && format_id <
	    (int) G_N_ELEMENTS (supported_export_file_formats));

  DenemoScore *si = gui->si;
  // Append file extension if needed
  gchar *file = NULL;
  gchar *basename = NULL;
  file = create_filename(file_name, &format_id);
#ifdef DEBUG
  g_print("Saving to file %s", file);
#endif
  if(!template) {
    update_file_selection_path(file);
    set_gui_filename (gui, file);
  }
  basename = g_path_get_basename (file);
  /* we don't want to save scores under ".denemo",
     ".jtf" and so on. do we? */
  if (basename[0] != '.')
    {
      switch (format_id)
	{
	case DENEMO_FORMAT:
	case DNM_FORMAT:
	  {
	    exportXML (file, gui, 0, 0);
	    break;
	  };
	case MUDELA_FORMAT:
	  {
	    gui->si->markstaffnum = 0;
	    exportlilypond (file, gui, TRUE);
	    break;
	  };
	case PNG_FORMAT:
	  {
	    gui->lilycontrol.excerpt = TRUE;
            exportlilypond (file, gui, TRUE);
	    break;
	  }
	case ABC_FORMAT:
	  {
	    exportabc (file, gui, 0, 0);
	    break;
	  };
	case MIDI_FORMAT:
	  {
	    exportmidi (file, si, 0, 0);
	    break;
	  };
	case CSOUND_FORMAT:
	  {
	    exportcsound (file, si, 0, 0);
	    break;
	  };
	default:
	  break;
	};
	
	/*export parts as lilypond files*/
	if(Denemo.prefs.saveparts)
		export_lilypond_parts(file,gui);
	if(gui->lilysync==gui->changecount)
	  gui->lilysync = 0;//still in sync
	gui->changecount = 0;
	score_status(gui, FALSE);

	si->readonly = FALSE;
    }
  g_free(basename);
  g_free(file);
  denemo_warning (gui, format_id);
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
 */
static void
template_open (DenemoGUI * gui, TemplateType local)
{
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
    file_open (gui, TRUE, REPLACE_SCORE);
    gui->filename = g_string_new("");
  }
}




/*
 * Open system template file callback function 
 */
void
system_template_open_with_check (GtkAction * action) {
  DenemoGUI *gui = Denemo.gui;
  if (gui->changecount)
    {
      if (confirmbox (gui))
	{
	  template_open (gui, SYSTEM);
	}
    }
  else
    {
      template_open (gui, SYSTEM);
    }
}

/*
 * Open system template file callback function 
 */
void
system_example_open_with_check (GtkAction * action) {
  DenemoGUI *gui = Denemo.gui;
  if (gui->changecount)
    {
      if (confirmbox (gui))
	{
	  template_open (gui, EXAMPLE);
	}
    }
  else
    {
      template_open (gui, EXAMPLE);
    }
}
/*
 * Open local template file callback function 
 */
void
local_template_open_with_check (GtkAction * action) {
  DenemoGUI *gui = Denemo.gui;
  if (gui->changecount)
    {
      if (confirmbox (gui))
	{
	  template_open (gui, LOCAL);
	}
    }
  else
    {
      template_open (gui, LOCAL);
    }
}


/**
 * Wrapper function for opening a file
 * checks to see if current score has changed and prompts user to save 
 * otherwise opens the file
 */
void
file_open_with_check (GtkAction * action)
{
  DenemoGUI *gui = Denemo.gui;
  if (gui->changecount)
    {
      if (confirmbox (gui))
	{
	  deletescore (NULL, gui);
	  file_open (gui, FALSE, REPLACE_SCORE);
	}
    }
  else
    {
      deletescore (NULL, gui);
      file_open (gui, FALSE, REPLACE_SCORE);
    }
}


/**
 * Wrapper function for opening a file to add movements to the current score
 * 
 */
void
file_add_movements(GtkAction * action){
  DenemoGUI *gui = Denemo.gui;
  if(!confirm_insertstaff_custom_scoreblock(gui))
    return;
  file_open(gui, FALSE, ADD_MOVEMENTS);
  score_status(gui, TRUE);
}
/**
 * Wrapper function for opening a file to add staffs to the current movement
 * 
 */
void
file_add_staffs(GtkAction * action){
  DenemoGUI *gui = Denemo.gui;
  if(!confirm_insertstaff_custom_scoreblock(gui))
    return;
  file_open(gui, FALSE, ADD_STAFFS);
  score_status(gui, TRUE);
}

static void  set_current_folder(GtkWidget *file_selection, DenemoGUI *gui, gboolean template) {
  gchar *path, *fallback;
  if(template) {
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
/**
 * File open dialog - opened where appropriate 
 *
 */
void
file_open (DenemoGUI * gui, gboolean template, ImportType type)
{
  GtkWidget *file_selection;
  GtkFileFilter *filter;

  int i;

  file_selection = gtk_file_chooser_dialog_new (_("Open"),
						GTK_WINDOW (Denemo.window),
						GTK_FILE_CHOOSER_ACTION_OPEN,
						GTK_STOCK_CANCEL,
						GTK_RESPONSE_REJECT,
						GTK_STOCK_OPEN,
						GTK_RESPONSE_ACCEPT, NULL);
  /* Open the last visited directory, if any. */
  set_current_folder(file_selection, gui, template);


  for (i = 0; i < (gint) G_N_ELEMENTS (supported_import_file_formats); i++)
    {
      filter = gtk_file_filter_new ();
      gtk_file_filter_set_name (filter,
				supported_import_file_formats[i].description);
      gtk_file_filter_add_pattern (filter,
				   supported_import_file_formats[i].
				   filename_mask);
      gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (file_selection), filter);
    }
  filter = gtk_file_filter_new ();
  gtk_file_filter_set_name (filter, _("All files"));
  gtk_file_filter_add_pattern (filter, "*");
  gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (file_selection), filter);
  gtk_dialog_set_default_response (GTK_DIALOG (file_selection),
				   GTK_RESPONSE_ACCEPT);
  gtk_widget_show_all (file_selection);
  if (gtk_dialog_run (GTK_DIALOG (file_selection)) == GTK_RESPONSE_ACCEPT)
    {
      gchar *filename =
	gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (file_selection));
      if(open_for_real (filename, gui, template, type)) {
	gchar *warning = g_strdup_printf("Load of file %s failed", filename);
	warningdialog(warning);
	g_free(warning);
      }
      g_free (filename);
    }
  gtk_widget_destroy (file_selection);

}

/**
 * Wrapper function to save the current file if not already 
 * saved.
 */
void
file_saveaswrapper (GtkAction * action)
{
  DenemoGUI *gui = Denemo.gui;
  file_saveas (gui, FALSE);
}

/**
 * Wrapper function to save the current file as template
 */
void
template_save (GtkAction * action)
{
  DenemoGUI *gui = Denemo.gui;
  init_local_path();
  default_template_path = local_template_path;
  file_saveas (gui, TRUE);
}





/**
 * Wrapper function for saving an existing file
 *
 */
void
file_savewrapper (GtkAction * action)
{
  DenemoGUI *gui = Denemo.gui;
  file_save (NULL, gui);
}

/**
 * Filters the filename based on its extension and calls
 * the relevant export function
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
    switch (guess_file_format (gui->filename->str))
      {
      case DENEMO_FORMAT:
      case DNM_FORMAT:
	{
	  exportXML (gui->filename->str, gui, 0, 0);
	  break;
	};
      case MUDELA_FORMAT:
	{
	  gui->lilycontrol.excerpt = TRUE;
	  exportlilypond (gui->filename->str, gui, TRUE);
	  break;
	};
      case ABC_FORMAT:
	{
	  exportabc (gui->filename->str, gui, 0, 0);
	  break;
	};
      case MIDI_FORMAT:
	{
	  exportmidi (gui->filename->str, si, 0, 0);
	  break;
	};
      case CSOUND_FORMAT:
	{
	  exportcsound (gui->filename->str, si, 0, 0);
	  break;
	};

      default:
	{
	  exportXML (gui->filename->str, gui, 0, 0);
	  break;
	};
      };
   /*Save parts as lilypond files*/   
   if(Denemo.prefs.saveparts)
	export_lilypond_parts(gui->filename->str,gui);
  
  denemo_warning (gui, guess_file_format (gui->filename->str));
  if(gui->lilysync==gui->changecount)
    gui->lilysync = 0;//still in sync
  gui->changecount = 0;
  score_status(gui, FALSE);
}

/**
 * Create file saveas dialog to enable user to save the current file to
 *
 *
 */
void
file_saveas (DenemoGUI * gui, gboolean template)
{
  GtkWidget *file_selection;
  GtkWidget *label;
  GtkWidget *combobox;
  GtkWidget *hbox;
  GtkListStore *list_store;
  GtkTreeIter iter;
  GtkCellRenderer *renderer;


  file_selection = gtk_file_chooser_dialog_new (_("Save As"),
						GTK_WINDOW (Denemo.window),
						GTK_FILE_CHOOSER_ACTION_SAVE,
						GTK_STOCK_CANCEL,
						GTK_RESPONSE_REJECT,
						GTK_STOCK_SAVE,
						GTK_RESPONSE_ACCEPT, NULL);


  /*set default folder for saving */
  set_current_folder(file_selection, gui, template);

  /* assign title */ 
  if (gui->si->headerinfo.title->len)
    { 
      gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (file_selection), 
					 gui->si->headerinfo.title->str);
    }
 // else {
  //	gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (file_selection), "Untitled Document");
  //}


  hbox = gtk_hbox_new (FALSE, 8);
  label = gtk_label_new (_("Format:"));
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

  list_store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT);
  combobox = gtk_combo_box_new_with_model (GTK_TREE_MODEL (list_store));
  renderer = gtk_cell_renderer_text_new ();
  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combobox), renderer, TRUE);
  gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT (combobox),
				 renderer, "text", COLUMN_NAME);
  gtk_box_pack_start (GTK_BOX (hbox), combobox, TRUE, TRUE, 0);

      int i;
      for (i = 0; i < (int) G_N_ELEMENTS (supported_export_file_formats); i++)
	{
	  gtk_list_store_append (list_store, &iter);
	  gtk_list_store_set (list_store, &iter,
			      COLUMN_NAME,
			      supported_export_file_formats[i].description,
			      COLUMN_ID, i, -1);
	}

  gtk_tree_model_get_iter_first (GTK_TREE_MODEL (list_store), &iter);
  gtk_combo_box_set_active_iter (GTK_COMBO_BOX (combobox), &iter);
  gtk_file_chooser_set_extra_widget (GTK_FILE_CHOOSER (file_selection), hbox);
  gtk_dialog_set_default_response (GTK_DIALOG (file_selection),
				   GTK_RESPONSE_ACCEPT);
  gtk_widget_show_all (file_selection);
  gboolean close = FALSE;
  do
    {
      if (gtk_dialog_run (GTK_DIALOG (file_selection)) == GTK_RESPONSE_ACCEPT)
	{
	  gint format_id = -1;
	  gchar *file_name
	    =
	    gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (file_selection));

          gtk_combo_box_get_active_iter (GTK_COMBO_BOX (combobox), &iter);
          gtk_tree_model_get (GTK_TREE_MODEL (list_store), &iter,
                                  COLUMN_ID, &format_id, -1);


	  if (replace_existing_file_dialog
	      (file_name, GTK_WINDOW (Denemo.window), format_id))
	    {
	      filesel_save (gui, file_name, format_id, template);
	      close = TRUE;
	    }
	  g_free (file_name);
	}
      else
	{
	  close = TRUE;
	}
    }
  while (!close);

  gtk_widget_destroy (file_selection);

}

/**
 * Wrapper function for creating a new file
 *
 */
void
file_newwrapper (GtkAction * action)
{
  DenemoGUI *gui = Denemo.gui;
  if (gui->changecount)
    {
      if (confirmbox (gui))
	{
	  deletescore(NULL, gui);
	}
    }
  else
    {
      deletescore(NULL, gui);
    };
}

/**
 * Delete the given score and create a new one
 * with one movement and empty music data
 *
 */
void
deletescore (GtkWidget * widget, DenemoGUI * gui)
{
  free_gui(gui);
  score_status(gui, FALSE);
  if(gui->filename) {
    g_string_free(gui->filename, TRUE);
    g_string_free(gui->autosavename, TRUE);
    gui->filename = NULL;
    set_title_bar (gui);
  }

  new_score(gui);
  gui->movements = g_list_append(gui->movements, gui->si);
  set_width_to_work_with(gui);
  set_rightmeasurenum (gui->si);
  update_hscrollbar (gui);
  update_vscrollbar (gui);
  gtk_widget_queue_draw (gui->scorearea);
  gtk_signal_emit_by_name (GTK_OBJECT (gui->hadjustment), "changed");
  gtk_signal_emit_by_name (GTK_OBJECT (gui->vadjustment), "changed");
  gui->lilysync = G_MAXUINT;
  refresh_lily_cb(NULL, gui);
}

void
dnm_deletescore (GtkWidget * widget, DenemoGUI * gui){
  deletescore (widget, gui);
}


/**
 * Try to suggest the format of a given file, from file name extension. A
 * more powerful function could be written to guess the format from the
 * file contents 
 */
gint
guess_file_format (gchar * file_name)
{
  gint name_iterator;
  gboolean format_match;

  name_iterator = FIRST_FORMAT_NAME;
  format_match = FALSE;

  while (!format_match && name_iterator <= LAST_FORMAT_NAME)
    {
      format_match = g_pattern_match_simple (FORMAT_MASK (name_iterator++),
                                             file_name);
    };

  /* In case no match could be found, we just give a 'default' format.
   * Chances are that all formats will be wrong, however ;-) */
  if (!format_match)
    return (DENEMO_FORMAT);
  else
    return (--name_iterator);
};



/**
 * Reloads a lilypond file specified by the .denemo/reloadfile.ly
 * only used when lilypond mode is active
 */
void
reload_lily_file (GtkWidget * button, gpointer data)
{
  // delete me
}

/**
 * Creates dialog to say that the chosen filename already exists
 * and do you want to overwrite it.
 *
 */
gboolean
replace_existing_file_dialog (const gchar * filename,
			      GtkWindow * parent_window, gint format_id)
{

  gchar *file = create_filename (filename, &format_id);
  if (!g_file_test (file, G_FILE_TEST_EXISTS))
    {
      g_free (file);
      return TRUE;
    }

  GtkWidget *dialog = gtk_message_dialog_new (parent_window,
					      (GtkDialogFlags)
					      (GTK_DIALOG_MODAL |
					       GTK_DIALOG_DESTROY_WITH_PARENT),
					      GTK_MESSAGE_QUESTION,
					      GTK_BUTTONS_YES_NO,
					      _
					      ("A file with the name %s already exists."),
					      file);

  gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
					    _("Do you want to replace it?"));
  gtk_widget_show_all (dialog);
  gboolean r = (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_YES);
  gtk_widget_destroy (dialog);
  g_free (file);
  //g_print ("Yes dialog is %d\n", r);
  return r;
}


/**
 * Save parts to individual files
 */
void
file_savepartswrapper (GtkAction * action)
{
  DenemoGUI *gui = Denemo.gui;
  if (gui->filename->len==0)
    {
      file_saveas (gui, FALSE);
    }

  export_lilypond_parts (gui->filename->str, gui);
}
