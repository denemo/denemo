/*  file.c
 * License: this file may be used under the FSF GPL version 3 or later
 *  Denemo File IO
 *
 *  for Denemo, a gtk+ frontend to GNU Lilypond
 *  (c) Adam Tee, Matthew Hiller 2000-2005
 *  (c) University of Leeds 2000-2005
 *      (c) Richard Shann 2010
 */

#include "display/calculatepositions.h"
#include "command/commandfuncs.h"
#include "command/contexts.h"
#include <denemo/denemo.h>
#include "ui/dialogs.h"
#include "export/exportabc.h"
#include "export/exportlilypond.h"
#include "export/file.h"
#include "export/guidedimportmidi.h"
#include "ui/moveviewport.h"
#include "command/staff.h"
#include "command/score.h"
#include "core/cache.h"
#include "core/utils.h"
#include "core/exportxml.h"
#include "export/exportmidi.h"
#include "core/importxml.h"
#include "export/exportmusicxml.h"
#include "export/importmusicxml.h"
#include "importmidi.h"

#include "core/prefops.h"
#include "core/binreloc.h"
#include "core/view.h"
#include "command/lilydirectives.h"
#include "ui/texteditors.h"
#include "export/print.h"
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>           /* check existance and type of files */
#include <dirent.h>             /* filter and sort filenames */
#include "source/source.h"
#include "source/sourceaudio.h"
#include "source/proof.h"
#include "audio/pitchentry.h"
#include "audio/audiointerface.h"
#include "printview/printview.h"

static gint file_open (DenemoProject * gui, DenemoSaveType template, ImportType type, gchar * filename);
static gint file_import_lilypond (DenemoProject * gui, DenemoSaveType template, ImportType type, gchar * filename);
static gint file_import_midi (DenemoProject * gui, DenemoSaveType template, ImportType type, gchar * filename);
static gint file_import_musicxml (DenemoProject * gui, DenemoSaveType template, ImportType type, gchar * filename);
static gboolean replace_existing_file_dialog (const gchar * filename, gint format_id);

typedef enum
{
  DENEMO_FORMAT = 0,
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
  gboolean async;               /* TRUE if uses async */
};

/* WARNING this array has to match the FileFormatNames enum above which is used to index it!!!!!!!!!" */
static struct FileFormatData supported_file_formats[] = {
  {"*.denemo*", N_("Denemo format (*.denemo *.denemo.gz)"),  ".denemo", 0},
  {"*.denemo.gz",    N_("CompressedDenemo XML format (*.denemo.gz)"),     ".denemo", 0},
  {"*.ly",     N_("Lilypond (*.ly)"),               ".ly", 0},
  {"*.pdf",    N_("PDF (*.pdf)"),                   ".pdf", 1},
  {"*.png",    N_("PNG Image format (*.png)"),      ".png", 1},
  {"*.abc",    N_("ABC (*.abc)"),                   ".abc", 0},
  {"*.mid",    N_("Midi (*.mid, *.midi)"),          ".mid", 0},
  {"*.sco",    N_("CSound Score File (*.sco)"),     ".sco", 0},
  {"*.xml",   N_("MusicXML file (*.musicxml, *.mxml, *.xml)"), ".xml", 0}
};

static GList*
supported_file_extensions(gchar* format){
  GList* exts = NULL;

  if(g_strcmp0 ("denemo", format) == 0){
    exts = g_list_append(exts, "*.denemo");
    exts = g_list_append(exts, "*.DENEMO");
    exts = g_list_append(exts, "*.denemo.gz");
  }

  if(g_strcmp0 ("lilypond", format) == 0){
    exts = g_list_append(exts, "*.ly");
    exts = g_list_append(exts, "*.LY");
  }

  if(g_strcmp0 ("midi", format) == 0){
    exts = g_list_append(exts, "*.midi");
    exts = g_list_append(exts, "*.mid");
    exts = g_list_append(exts, "*.MIDI");
    exts = g_list_append(exts, "*.MID");
  }

  if(g_strcmp0 ("musicxml", format) == 0){
    exts = g_list_append(exts, "*.xml");	  
    exts = g_list_append(exts, "*.musicxml");
    exts = g_list_append(exts, "*.mxml");
    exts = g_list_append(exts, "*.MXML");
  }

  if(g_strcmp0 ("pdf", format) == 0){
    exts = g_list_append(exts, "*.pdf");
    exts = g_list_append(exts, "*.PDF");
  }
  if(g_strcmp0 ("evince", format) == 0){
    exts = g_list_append(exts, "*.pdf");
    exts = g_list_append(exts, "*.PDF");
  }
  if(g_strcmp0 ("proof", format) == 0){
    exts = g_list_append(exts, "*.pdf");
    exts = g_list_append(exts, "*.PDF");
  }
  return exts;
}

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

static void warn_export_lilypond_parts (char *filename, DenemoProject * gui)
{
  static gboolean shown;
  if (!shown)
  {
    warningdialog (_("Because you have the preference \"Create Parts\" set, Score Layouts for all the parts are now created.\nChoose Typeset in the Print View to select the Default Layout if reqquired."));
    shown = TRUE;
  }
  export_lilypond_parts (filename, gui);
}
/**
 * Display a message box asking the user whether to save unsaved changes
 * or close without saving
 */
static gboolean
confirm_save (DenemoProject * gui, gchar * primary, gchar * secondary)
{
  if (Denemo.non_interactive)
    return TRUE;
  GtkWidget *dialog;
  gboolean r = FALSE;

  dialog = gtk_message_dialog_new (GTK_WINDOW (Denemo.window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE, "%s", primary);
  (void) gtk_dialog_add_button ((GtkDialog *) dialog, _("Close without Saving"), GTK_RESPONSE_NO);

  (void) gtk_dialog_add_button ((GtkDialog *) dialog, _("_Cancel"), GTK_RESPONSE_CANCEL);

  (void) gtk_dialog_add_button ((GtkDialog *) dialog, _("Save _As"), GTK_RESPONSE_YES);

  gtk_dialog_set_default_response ((GtkDialog *) dialog, GTK_RESPONSE_YES);

  gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog), "%s", secondary);
  gtk_widget_show_all (dialog);
  gint response = gtk_dialog_run (GTK_DIALOG (dialog));

  if (response == GTK_RESPONSE_YES)
    {
      gtk_widget_destroy (dialog);
      file_saveas (SAVE_NORMAL);
      if (gui->notsaved)
        r = FALSE;
      else
        r = TRUE;
    }
  else if (response == GTK_RESPONSE_NO)
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
confirmbox (DenemoProject * gui)
{
  gboolean ret;
  gchar *primary = g_strdup_printf (_("The score %s has unsaved changes"), gui->filename->len ? gui->filename->str : _("(Untitled)"));
  ret = confirm_save (gui, primary, _("Save changes?"));
  g_free (primary);
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
openrecent (G_GNUC_UNUSED GtkWidget * widget, gchar * filename)
{
  DenemoProject *gui = Denemo.project;
  if (!gui->notsaved || (gui->notsaved && confirmbox (gui)))
    {
      // deletescore(NULL, gui);
      if (open_for_real (filename, gui, FALSE, FALSE))
        {
          gchar *warning = g_strdup_printf (_("Load of recently used file %s failed"), filename);
          warningdialog (warning);
          g_free (warning);
        }
    }
}

/**
 * Decorate the window with the tile
 */
static void
set_gui_tabname (DenemoProject * gui, gchar * filename)
{
    if(gui->tabname==NULL)
       gui->tabname = g_string_new (filename);
    else
        g_string_assign (gui->tabname, filename);
  if(!Denemo.non_interactive)
    set_title_bar (gui);
}

/**
 * Sets the filename for storing the passed in gui.
 * and adds it to the history
 */
void set_project_filename (DenemoProject * gui, gchar * filename)
{
  GList *link = NULL;
  g_string_assign (gui->filename, filename);
  set_gui_tabname (gui, filename);
  if (*filename == 0)
    return;
  if ((link = g_queue_find_custom (Denemo.prefs.history, gui->filename->str, &history_compare)))
     g_queue_remove (Denemo.prefs.history, link->data);
  if (Denemo.prefs.maxhistory>0)
    {
        //g_debug ("max history now %d\n", Denemo.prefs.maxhistory);
      if (g_queue_get_length (Denemo.prefs.history) > Denemo.prefs.maxhistory)
        {
          gpointer data = g_queue_pop_head (Denemo.prefs.history);
          //g_print ("Losing one file name from history %s\n\n", data);
          if (data)
            g_free (data);
        }
      if(!Denemo.non_interactive){
        if (!link)                     /* a new one, needs to go into the menu */
          addhistorymenuitem (filename);
      }
      g_queue_push_tail (Denemo.prefs.history, g_strdup (gui->filename->str));
      //g_print ("Added %s to history, now %d in history\n\n", gui->filename->str, g_queue_get_length (Denemo.prefs.history));
      writeHistory ();
  }
}

static gchar *
strip_path_and_extension (gchar * filename)
{
  gchar *basename;
  basename = g_path_get_basename (filename);
  (void) strtok (basename, ".");
  return basename;
}

static void
update_file_selection_path (gchar * file)
{
  if (file_selection_path)
    g_free (file_selection_path);
  file_selection_path = g_path_get_dirname (file);
}

gint
lyinput (gchar * filename)
{
  gchar *path = g_path_get_dirname (filename);
  gchar *base = g_path_get_basename (filename);
#ifdef G_OS_WIN32
  gchar *call = g_strescape (path, "");
  call = g_strdup_printf ("%s%s%s%s%s", "(debug-set! stack 200000) (lyimport::load-file \"", call, "\\\\\" \"", base, "\")");
  g_debug ("Calling %s\n", call);
#else
  gchar *call = g_strdup_printf ("%s%s%c%s%s%s", "(lyimport::load-file \"", path, G_DIR_SEPARATOR, "\" \"", base, "\")");
#endif


  call_out_to_guile (call);
  g_free (path);
  g_free (base);
  g_free (call);
  return 0;
}

static gboolean
has_extension(gchar* filename, const gchar* extension)
{
  return (strcmp (filename + strlen (filename) - strlen(extension), extension) == 0);
}

static void delete_all_rhythms (void)
{
  DenemoProject *project = Denemo.project;
  GList *g;
  for (g = project->rhythms;g;g=project->rhythms)
    {
        delete_rhythm_pattern (g->data);
    }
}
static void enquire_rhythms (void) {
if (Denemo.project->rhythms && choose_option (_("Music Snippets Can be Kept"), _("Drop Music Snippets"), _("Keep Music Snippets")))
    delete_all_rhythms ();
}


/**
 * The function that actually determines the file type and calls the
 * function that opens the file.
 * filename must be full path else it is looked for below Denemo.prefs.denemopath or user is asked for location
 * @return 0 for success non zero for failure
 */
gint
open_for_real (gchar * filename, DenemoProject * gui, DenemoSaveType template, ImportType type)
{
  if(!Denemo.non_interactive)
    g_signal_handlers_block_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_draw_event), NULL);
  gint result;
  gchar *zipfile = NULL;
  gboolean xml = FALSE;
  result = 1;                   //FAILURE

  if (type == REPLACE_SCORE)
  {
    if(Denemo.non_interactive)
        delete_all_rhythms ();
    else
        enquire_rhythms ();
  }
  
  if (filename && has_extension (filename, ".denemo"))
      zipfile = g_strconcat (filename, ".gz", NULL);
  if (filename) {
                  if(!Denemo.non_interactive)
                    if ((!g_file_test (filename, G_FILE_TEST_IS_REGULAR)) && (!g_file_test (zipfile, G_FILE_TEST_IS_REGULAR)))
                      {
                       gchar *base = Denemo.prefs.denemopath->str;
                       gchar *found = try_to_find_file (base, filename);
                       if (zipfile && (!found))
                        found = try_to_find_file (base, zipfile);
                      
                       while (!found)
                        { gchar *message = g_strdup_printf ("%s%s%s", _("Unable to find file: "), filename, _("\nChoose a directory (below which to search)\nin the next dialog"));
                          gchar *path; 
                          warningdialog (message);
                          path = choose_directory (_("Give Toplevel Directory"), Denemo.prefs.denemopath->str, NULL);
                          if (path)
                            {
                              found = try_to_find_file (path, filename);
                              if (zipfile && (!found))
                                found = try_to_find_file (path, zipfile);
                            }
                          else
                            break;
                        }
                      if (found)
                        filename = found;
                      }
                  g_free (zipfile);
                }
  if (filename)
    {
      if (has_extension (filename, ".denemo") || has_extension (filename, ".denemo.gz"))
        {
            xml = TRUE;
            if (g_file_test (filename, G_FILE_TEST_IS_REGULAR))
              result = importXML (filename, gui, type);
            if (result  && has_extension (filename, ".denemo"))
              {
                gchar *zip = g_strconcat (filename, ".gz", NULL);
                result = importXML (zip, gui, type);
                g_free (zip);
              }
        }
      else if (has_extension (filename, ".ly"))
        result = lyinput (filename);
      else if (has_extension (filename, ".mxml") || has_extension (filename, ".xml") || has_extension (filename, ".musicxml"))
        result = mxmlinput (filename);
      else if (has_extension (filename, ".mid") || has_extension (filename, ".midi"))
        result = (type==GUIDED_IMPORT)?guidedImportMidi (filename):importMidi (filename);
      else if (has_extension (filename, ".pdf") || has_extension (filename, ".PDF"))
        {
#ifndef USE_EVINCE
          g_debug("This feature requires denemo to be built with evince");
#else
          // a .pdf file for transcribing from, does not affect the current score.
          g_signal_handlers_unblock_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_draw_event), NULL);
          return type==PROOFREAD? (!open_proofread_file(filename)) : !open_source (filename, 0, 0, 0);
#endif
        }
    //if (result == 0) g_message("Opening file %s", filename);
    }
  //printf("\nResult == %d type == %d template == %d xml == %d\n",result,type,template,(int)xml);
  if (result == 0)
    {
      if (!template)
        {                       // not a template
          update_file_selection_path (filename);
          if (type == REPLACE_SCORE)
            {
              if (xml){
                 if (g_str_has_suffix (filename, ".gz"))
                    {
                     *(filename + strlen(filename) -3) = 0;
                    }
                 set_project_filename (gui, filename);
              }
              else
                {
                  gchar *sname = strip_path_and_extension (filename);
                  set_gui_tabname (gui, sname);
                  g_free (sname);
                }
            }
          if (type == ADD_STAFFS || type == ADD_MOVEMENTS)
            score_status (gui, TRUE);
        }
      else
        {
          g_string_assign (gui->filename, "");
          set_gui_tabname (gui, "");
        }
      //if (Denemo.printarea)
      //  g_object_set_data (G_OBJECT (Denemo.printarea), "printviewupdate", (gpointer) G_MAXUINT);
      if (!xml)
        updatescoreinfo (gui);
      else
        { 
          if ((!Denemo.prefs.ignorescripts) && (gui->script) && (*gui->script) && !(type == ADD_STAFFS || type == ADD_MOVEMENTS))
            {
              gui->has_script = TRUE;
              cache_all ();
              executeScript ();
            }
        }


      set_rightmeasurenum (gui->movement);
      select_lyrics ();

      if(!Denemo.non_interactive){
        set_bottom_staff (gui);
        update_hscrollbar (gui);
        update_vscrollbar (gui);
        draw_score_area();
        g_signal_emit_by_name (G_OBJECT (Denemo.hadjustment), "changed");
        g_signal_emit_by_name (G_OBJECT (Denemo.vadjustment), "changed");
        force_lily_refresh (gui);
      }
    }
  else                          /*file load failed - gui may not be valid */
    {
      if(!Denemo.non_interactive)
        deletescore (NULL, gui);
    }
  if(!Denemo.non_interactive){
    g_signal_handlers_unblock_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_draw_event), NULL);
    gui->movement->undo_guard = 1;
  }
  if (result==0)
    {
      if((!Denemo.prefs.ignorescripts))
            denemo_scheme_init ();        //to re-instate any user defined directives for whole score
      if(!Denemo.non_interactive){
        if (!(type == ADD_STAFFS || type == ADD_MOVEMENTS))
          score_status (gui, FALSE);
    }
#ifdef DISABLE_AUBIO
#else
    rewind_audio ();
#endif
  if((result==0) && !Denemo.non_interactive)
    panic_all ();// g_print ("Reset synth in file open\n");
  gui->movement->undo_guard = Denemo.prefs.disable_undo;      //user pref to (dis)allow undo information to be collected
  }
  //look for a link to a source file at the start of the score, open it if there is one
  if ((result==0) && (Denemo.prefs.opensources) && (type != ADD_STAFFS) && (type != ADD_MOVEMENTS) && Denemo.project->movement && Denemo.project->movement->thescore)
        {
        DenemoStaff*thestaff = (DenemoStaff*)Denemo.project->movement->thescore->data;
        DenemoMeasure* themeasure = (DenemoMeasure*)thestaff->themeasures->data;
        GList *theobjs = themeasure->objects;
        for (;theobjs;theobjs=theobjs->next)
            {
                DenemoObject *theobj = (DenemoObject *)theobjs->data;
                if (theobj->type == LILYDIRECTIVE)
                    {
                        DenemoDirective *direc = (DenemoDirective *)theobj->object;
                        if (direc->tag && !strcmp (direc->tag->str, "DenemoLink") &&direc->data)
                            {
                                gchar *script = g_strdup_printf ("(d-OpenSource (scheme-escape \"%s\"))", direc->data->str);
                                call_out_to_guile (script);
                                g_free (script);
                            }
                    }
            }
        }
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
  gchar *ext = strrchr (file_name, '.');
  if (ext == NULL)
    return g_strdup (file_name);
  gint i;
  GString *file_name_stripped = g_string_new ("");
  gint filename_size = strlen (file_name);
  gint ext_size = strlen (FORMAT_EXTENSION (format_id));
  if (strlen (ext) != ext_size)
    return g_strdup (file_name);
  gint stripped_filename_size = filename_size - ext_size;
  for (i = 0; i < stripped_filename_size; i++)
    {
      g_string_append_c (file_name_stripped, file_name[i]);
    }
  printf ("\nTruncated filename == %s\n", file_name_stripped->str);
  return g_string_free (file_name_stripped, FALSE);
}

/* Save gui in the file in format format_id to the file filename (or gui->filename
   if filename is NULL)
   If there is a scheme script, offers to save that with the file.
 */
static gint
save_in_format (gint format_id, DenemoProject * gui, gchar * filename)
{
  gint ret = 0;
  gchar *file = filename ? filename : gui->filename->str;
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
        if (getNumCharsSchemeText () == 0)
            gui->has_script = FALSE;
        if ((!Denemo.non_interactive) && getNumCharsSchemeText () && (!gui->has_script))
          if (choose_option (_("You have a Script defined"), _("Normal Save"), _("Advanced: Execute the script every time this file is opened?")))
            {
              deleteSchemeText ();
              gui->has_script = FALSE;
            }
        ret = exportXML (file, gui);
        break;
      };
    case MUSICXML_FORMAT:
      {
        exportmusicXML (file, gui);
        break;
      };
    case MUDELA_FORMAT:
      {
        gui->movement->markstaffnum = 0;
        exportlilypond (file, gui, TRUE);
        break;
      };
    case PDF_FORMAT:
      {
        gui->movement->markstaffnum = 0;
        export_pdf (file, gui);
        break;
      };
    case PNG_FORMAT:
      {
        gui->movement->markstaffnum = 0;
        export_png (file, (GChildWatchFunc) printpng_finished, gui);
        break;
      };
    case ABC_FORMAT:
      {
        exportabc (file, gui, 0, 0);
        break;
      };
    case MIDI_FORMAT:
      {
        exportmidi (file, gui->movement);
        break;
      };
    default:
      break;
    };
  return ret;
}

/**
 * File save called by fileselsave callback
 * param file_name is full path to file possibly with extension
 */
static gint
filesel_save (DenemoProject * gui, const gchar * file_name, gint format_id, DenemoSaveType template)
{
  gint ret = 0;
  //g_assert (gui != NULL);
  //g_assert (file_name != NULL);
  //g_assert (format_id >= 0 && format_id < (int) G_N_ELEMENTS (supported_file_formats));

  // Append file extension if needed
  gchar *file = NULL;
  gchar *basename = NULL;
  file = create_filename (file_name, format_id);
  if (!template && format_id == DENEMO_FORMAT)
    {
      update_file_selection_path (file);
    }
  basename = g_path_get_basename (file);
  if (basename[0] != '.')       // avoids empty filename
    {
      if (FORMAT_ASYNC (format_id))
        ret = save_in_format (format_id, gui, strip_filename_ext (file_name, format_id));       //FIXME strip_filename is not freed
      else
        ret = save_in_format (format_id, gui, file);

      /*export parts as lilypond files */
      if (Denemo.prefs.saveparts)
        warn_export_lilypond_parts (file, gui);
      if(!Denemo.non_interactive)
        gui->movement->readonly = FALSE;
    }
  if (!template && format_id == DENEMO_FORMAT)
    {
      if(!Denemo.non_interactive)
        set_project_filename (gui, file);
    }    
    
    
  g_free (basename);
  g_free (file);
  return ret;
}

/* set local_template_path up */
static void
init_local_path (void)
{
  local_template_path = g_build_filename (get_user_data_dir (TRUE), "templates", NULL);
  gboolean err = g_mkdir_with_parents (local_template_path, 0770);
  if (err)
    {
      warningdialog (_("Could not create .denemo/templates for you personal templates"));
      g_free (local_template_path);
      local_template_path = NULL;
    }
}

typedef enum
{
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
template_open (DenemoProject * gui, TemplateType local, gchar * filename)
{
  gboolean ret = FALSE;
  if (local == LOCAL)
    {
      if (local_template_path == NULL)
        {
          init_local_path ();
        }
      default_template_path = local_template_path;
    }
  else if (local == EXAMPLE)
    {
      if (system_example_path == NULL)
        {
          system_example_path = g_build_filename (get_system_data_dir (), "examples", NULL);
          GDir *denemo_path = g_dir_open (system_example_path, 0, NULL);
          if (denemo_path == NULL)
            {
              warningdialog (_("No examples directory in installation"));
              system_example_path = NULL;
            }
        }
      default_template_path = system_example_path;
    }
  else
    {
      if (system_template_path == NULL)
        {
          system_template_path = g_build_filename (get_system_data_dir (), "templates", NULL);
          GDir *denemo_path = g_dir_open (system_template_path, 0, NULL);
          if (denemo_path == NULL)
            {
              warningdialog (_("No templates directory in installation"));
              system_template_path = NULL;
            }
        }
      default_template_path = system_template_path;
    }
  if (default_template_path)
    {
      gchar *filepath = g_build_filename (default_template_path, filename, NULL);
      ret = file_open (gui, TRUE, REPLACE_SCORE, filepath);
      g_free (filepath);
      gui->filename = g_string_new ("");
      gui->tabname = g_string_new ("");
    }
  return ret;
}

static void
open_with_check(TemplateType dir, DenemoAction * action, DenemoScriptParam * param){
  GET_1PARAM(action, param, filename);
  if (Denemo.project->notsaved){
    if (filename==NULL && confirmbox (Denemo.project)){
      param->status = !template_open (Denemo.project, dir, filename);
    }
  }
  else{
    param->status = !template_open (Denemo.project, dir, filename);
  }
}

/*
 * Open system template file callback function
 */
void
system_template_open_with_check (DenemoAction * action, DenemoScriptParam * param)
{
  open_with_check (SYSTEM, action, param);
}

/*
 * Open system template file callback function
 */
void
system_example_open_with_check (DenemoAction * action, DenemoScriptParam * param)
{
  open_with_check (EXAMPLE, action, param);
}

/*
 * Open local template file callback function
 */
void
local_template_open_with_check (DenemoAction * action, DenemoScriptParam * param)
{
  open_with_check (LOCAL, action, param);
}

/**
 * Wrapper function for opening a file, d-Open
 * if no param checks to see if current score has changed and prompts user to save
 * otherwise opens the file
 */
void
file_open_with_check (DenemoAction * action, DenemoScriptParam * param)
{
  GET_1PARAM (action, param, filename);
  if (query)
    {
      param->status = (Denemo.project->filename != NULL) && Denemo.project->filename->len;
      if (param->status)
        g_string_assign (param->string, Denemo.project->filename->str);
      return;
    }
  DenemoProject *gui = Denemo.project;
  if (!gui->notsaved || (gui->notsaved && (confirmbox (gui))))
    {
      param->status = !file_open (gui, FALSE, REPLACE_SCORE, filename);
    }
}

void
file_import_lilypond_with_check (DenemoAction * action, DenemoScriptParam * param)
{
  GET_1PARAM(action, param, filename);
  param->status = !file_import_lilypond (Denemo.project, FALSE, REPLACE_SCORE, filename);
}

void
file_import_midi_with_check (DenemoAction * action, DenemoScriptParam * param)
{
    GET_2PARAMS(action, param, filename, guided);
    if(guided)
        file_import_midi (Denemo.project, FALSE, GUIDED_IMPORT, filename);
    else
        file_import_midi (Denemo.project, FALSE, REPLACE_SCORE, filename);
}

void
file_import_musicxml_with_check (DenemoAction * action, DenemoScriptParam * param)
{
  GET_1PARAM(action, param, filename);
  param->status = !file_import_musicxml (Denemo.project, FALSE, REPLACE_SCORE, filename);
}

#define ADD(insertion_strategy)\
  GET_1PARAM(action, param, filename);\
  (void)signal_structural_change(Denemo.project);\
  param->status = !file_open(Denemo.project, FALSE, insertion_strategy, filename);\
  score_status(Denemo.project, TRUE);\

/**
 * Wrapper function for opening a file to add movements to the current score
 *
 */
void
file_add_movements (DenemoAction * action, DenemoScriptParam * param)
{
ADD (ADD_MOVEMENTS)}

/**
 * Wrapper function for opening a file to add staffs to the current movement
 *
 */
void
file_add_staffs (DenemoAction * action, DenemoScriptParam * param)
{
ADD (ADD_STAFFS)}

static void
set_current_folder (GtkWidget * file_selection, DenemoSaveType template)
{
  gchar *path, *fallback;
  if (template == SAVE_TEMPLATE)
    {
      fallback = path = default_template_path;
    }
  else
    {
      fallback = path = file_selection_path;
      GDir *denemo_path = g_dir_open (Denemo.prefs.denemopath->str, 0, NULL);
      if (denemo_path != NULL)
        {
          g_dir_close (denemo_path);
          fallback = Denemo.prefs.denemopath->str;
        }
    }
  if (path != NULL)
    {
      gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (file_selection), path);
    }
  else
    {
      if (fallback != NULL)
        gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (file_selection), fallback);
    }
}

gchar *
file_dialog (gchar * message, gboolean type, gchar * location)
{
  GtkWidget *file_selection;
  gchar *filename;
  file_selection = gtk_file_chooser_dialog_new (message, GTK_WINDOW (Denemo.window), type ? GTK_FILE_CHOOSER_ACTION_OPEN : GTK_FILE_CHOOSER_ACTION_SAVE, _("_Cancel"), GTK_RESPONSE_REJECT, type ? _("_Open") : _("_Save"), GTK_RESPONSE_ACCEPT, NULL);

  if (location)
    gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (file_selection), location);
  gtk_dialog_set_default_response (GTK_DIALOG (file_selection), GTK_RESPONSE_ACCEPT);
  gtk_widget_show_all (file_selection);
  if (gtk_dialog_run (GTK_DIALOG (file_selection)) == GTK_RESPONSE_ACCEPT)
    filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (file_selection));
  else
    filename = NULL;
  gtk_widget_destroy (file_selection);
  return filename;
}

static void
update_preview_cb (GtkFileChooser * file_chooser, gpointer data)
{
#ifndef USE_EVINCE
  g_debug("This feature requires denemo to be built with evince");
#else
  GtkWidget *preview = NULL;
  gchar *thumb_filename = NULL;
  gchar *selection_filename = NULL;
  GdkPixbuf *pixbuf = NULL;
  gboolean have_preview = FALSE;

  preview = GTK_WIDGET (data);
  selection_filename = gtk_file_chooser_get_preview_filename (file_chooser);
  thumb_filename = large_thumbnail_name (selection_filename);
  pixbuf = gdk_pixbuf_new_from_file_at_size (thumb_filename, 512, 512, NULL);
  have_preview = (pixbuf != NULL);
  g_debug ("\n# %s for %s thumbnail = %s\n", have_preview ? "We have a thumbnail generated" : "We have not yet generated a thumbnail", selection_filename, thumb_filename);
  g_free (selection_filename);
  g_free (thumb_filename);

  gtk_image_set_from_pixbuf (GTK_IMAGE (preview), pixbuf);
  if (pixbuf)
    g_object_unref (pixbuf);

  gtk_file_chooser_set_preview_widget_active (file_chooser, have_preview);
#endif
}

static gboolean
file_open_dialog(gchar* message, gchar* format, FileFormatNames save_type, DenemoSaveType template, ImportType type, gchar* filename){
  gboolean ret = -1;
   if(filename && (!g_file_test(filename, G_FILE_TEST_EXISTS)) && (!g_path_is_absolute (filename)) && file_selection_path)
            filename = g_build_filename (file_selection_path, filename, NULL);//memory leak

  if(filename && !g_file_test(filename, G_FILE_TEST_IS_DIR))
    return (open_for_real(filename, Denemo.project, template, type));

  GtkWidget *file_selection;
  GtkFileFilter *filter;
  gint i;


  file_selection = gtk_file_chooser_dialog_new (_(message),
                        GTK_WINDOW (Denemo.window),
                        GTK_FILE_CHOOSER_ACTION_OPEN,
                        _("_Cancel"),
                        GTK_RESPONSE_REJECT,
                        _("_Open"),
                        GTK_RESPONSE_ACCEPT, NULL);
  /* Open in passed in directory or the last visited directory, if any, or a default depending on template. */
  if (filename && g_file_test(filename, G_FILE_TEST_IS_DIR))
    gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (file_selection), filename);
  else
    set_current_folder(file_selection, template);

  filter = gtk_file_filter_new ();
  gtk_file_filter_set_name (filter, FORMAT_DESCRIPTION(save_type));

  GList* exts = supported_file_extensions (format);
  GList* cur = NULL;
  for (cur = exts; cur; cur = cur->next)
    gtk_file_filter_add_pattern (filter, cur->data);
  gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (file_selection), filter);
  gtk_dialog_set_default_response (GTK_DIALOG (file_selection),
                   GTK_RESPONSE_ACCEPT);
  gtk_widget_show_all (file_selection);
  GtkWidget *preview;
  preview = gtk_image_new();
  gtk_file_chooser_set_preview_widget(GTK_FILE_CHOOSER (file_selection), preview);
  g_signal_connect (GTK_FILE_CHOOSER(file_selection), "update-preview",
            G_CALLBACK (update_preview_cb), preview);
  gtk_widget_show_all (preview);
  if (gtk_dialog_run (GTK_DIALOG (file_selection)) == GTK_RESPONSE_ACCEPT)
    {
      gchar *name = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (file_selection));
      if((ret=open_for_real (name, Denemo.project, template, type))) {
        gchar *warning = g_strdup_printf(_("Load of file %s failed"), name);
        infodialog(warning);//FIXME keep track of if this is called from a script or not, and so should be interactive or not
        g_free(warning);
      }
      g_free (name);
    }
  gtk_widget_destroy (file_selection);


  return ret;
}

/**
 * File open dialog - opened where appropriate
 * return 0 on success non-zero on failure.
 * filename must be full path or NULL for dialog
 */
static gint
file_open (DenemoProject * gui, DenemoSaveType template, ImportType type, gchar * filename)
{
  return file_open_dialog ("Open", "denemo", DENEMO_FORMAT, template, type, filename);
}

gint
open_source_file (void)
{
  return file_open_dialog ("Open", "evince", PDF_FORMAT, 0, SOURCE_PDF, NULL);
}
gint
open_proof_file (void)
{
  return file_open_dialog ("Open", "proof", PDF_FORMAT, 0, PROOFREAD, NULL);
}

/**
 * Lilypond Import dialog - opened where appropriate
 * return 0 on success non-zero on failure.
 * filename must be full path or NULL for dialog
 */
static gint
file_import_lilypond (DenemoProject * gui, DenemoSaveType template, ImportType type, gchar * filename)
{
  return file_open_dialog ("Import Lilypond", "lilypond", MUDELA_FORMAT, template, type, filename);
}

/**
 * Midi Import dialog - opened where appropriate
 * return 0 on success non-zero on failure.
 * filename must be full path or NULL for dialog
 */
static gint
file_import_midi (DenemoProject * gui, DenemoSaveType template, ImportType type, gchar * filename)
{
  return file_open_dialog ("Import Midi", "midi", MIDI_FORMAT, template, type, filename);
}

/**
 * MusicXML Import dialog - opened where appropriate
 * return 0 on success non-zero on failure.
 * filename must be full path or NULL for dialog
 */
static gint
file_import_musicxml (DenemoProject * gui, DenemoSaveType template, ImportType type, gchar * filename)
{
  return file_open_dialog ("Import MusicXML", "musicxml", MUSICXML_FORMAT, template, type, filename);
}

/**
 * Wrapper function to save the current file if not already
 * saved.
 */
void
file_saveaswrapper (DenemoAction * action, DenemoScriptParam * param)
{
  GET_1PARAM (action, param, filename);
  DenemoProject *gui = Denemo.project;
  if (filename == NULL)
    {
      file_saveas (FALSE);
    }
  else
    {
      gint status = filesel_save (gui, filename, DENEMO_FORMAT, FALSE);

      if(!Denemo.non_interactive){
        if (status == 0)
          score_status (gui, FALSE);
        force_lily_refresh (gui);
      }
    }
}

/**
 * Wrapper function to save the current file as template
 */
void
template_save (G_GNUC_UNUSED DenemoAction * action, G_GNUC_UNUSED DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  init_local_path ();
  default_template_path = local_template_path;
  Denemo.project->total_edit_time = 0;
  file_saveas (SAVE_TEMPLATE);
  g_string_assign (gui->filename, "");
  set_gui_tabname (gui, "");
}


/**
 * Wrapper function to save the current file as a copy
 */
void
file_copy_save (G_GNUC_UNUSED DenemoAction * action, G_GNUC_UNUSED DenemoScriptParam * param)
{
  init_local_path ();
  file_saveas (SAVE_COPY);
}

/**
 * Wrapper function for saving an existing file
 *
 */
void
file_savewrapper (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  GET_1PARAM (action, param, filename);
  if (filename) {
    exportXML (filename, gui);
    return;
  }
  if (file_save (NULL, gui))
    {
      if (action && Denemo.project->filename && Denemo.project->filename->len)
        {
          warningdialog (_("File save failed"));
          score_status (gui, TRUE);
        }
      else
        {
          if (param)
            param->status = FALSE;
        }
    }
}

/**
 * if gui->filename exists saves gui to the filename  based on its extension
 * otherwise call saveas routine
 */
gint
file_save (GtkWidget * widget, DenemoProject * gui)
{
  gint ret;
  DenemoMovement *si = gui->movement;
  g_debug ("READONLY %d\n", si->readonly);
  if ((gui->filename->len == 0) /* || (si->readonly == TRUE) */ )
    /* No filename's been given or is opened from template */
    file_saveas (FALSE);
  else
    ret = save_in_format (DENEMO_FORMAT, gui, NULL);

  /*Save parts as lilypond files */
  if (Denemo.prefs.saveparts)
    warn_export_lilypond_parts (gui->filename->str, gui);

  score_status (gui, FALSE);
  return ret;
}

static void
file_dialog_response (GtkWidget * dialog, gint response_id, struct FileDialogData *data)
{
  DenemoProject *gui = Denemo.project;
  if (response_id == GTK_RESPONSE_ACCEPT)
    {
      gchar *file_name = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
      if (g_str_has_suffix (file_name, ".gz"))
        *(file_name + strlen (file_name) - strlen (".gz")) = 0;
      if (replace_existing_file_dialog (file_name, data->format_id))
        {
          gint status = filesel_save (gui, file_name, data->format_id, data->template);
          if (status == 0)
            score_status (gui, FALSE);
          force_lily_refresh (gui);     //FIXME why is this here???
        }
      g_free (file_name);
    }
  gtk_widget_destroy (dialog);
  g_free (data);
}


#define FILE_SAVE_DIALOG(description, template)\
  GtkWidget *file_selection;\
  GtkFileFilter *filter;\
  file_selection = gtk_file_chooser_dialog_new (description,\
                        GTK_WINDOW (Denemo.window),\
                        GTK_FILE_CHOOSER_ACTION_SAVE,\
                        _("_Cancel"),\
                        GTK_RESPONSE_REJECT,\
                        _("_Save"),\
                        GTK_RESPONSE_ACCEPT, NULL);\
  /*set default folder for saving */\
  set_current_folder(file_selection, template==SAVE_TEMPLATE?SAVE_TEMPLATE:SAVE_NORMAL);\
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
static void
file_export (FileFormatNames format_id)
{
  gchar *description = g_strconcat (_("Export As "), FORMAT_DESCRIPTION (format_id), NULL);
  DenemoSaveType template = FALSE;
  FILE_SAVE_DIALOG (description, template) g_free (description);
}

/**
 * Create file saveas dialog to enable user to save the current file to
 *
 *
 */
void
file_saveas (DenemoSaveType template)
{
  gint format_id = DENEMO_FORMAT;
FILE_SAVE_DIALOG (_("Save As"), template)}

/**
 * Wrapper function for command New which asks to delete the current gui and on success creates an empty score
 *
 */
void
file_newwrapper (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoScriptParam dummy;
  dummy.string = NULL;
  if (param == NULL)
    param = &dummy;
  DenemoProject *gui = Denemo.project;
  if(!Denemo.non_interactive)
    g_signal_handlers_block_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_draw_event), NULL);
  if (!Denemo.non_interactive && gui->notsaved)
    {
      if (confirmbox (gui))
        {
          enquire_rhythms ();
          deletescore (NULL, gui);
        }
      else
        {
          param->status = FALSE;
          g_signal_handlers_unblock_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_draw_event), NULL);
          return;
        }
    }
  else
    {  if(Denemo.non_interactive)
            delete_all_rhythms ();
        else
            enquire_rhythms ();
      deletescore (NULL, gui);
    }
  deleteSchemeText ();
  delete_conditions (gui);
  gui->has_script = FALSE;
  set_enharmonic_position (0);
 // if (Denemo.printarea)
  //  g_object_set_data (G_OBJECT (Denemo.printarea), "printviewupdate", (gpointer) G_MAXUINT);
  score_status (gui, FALSE);
  param->status = TRUE;
  if(!Denemo.non_interactive)
    g_signal_handlers_unblock_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_draw_event), NULL);
}

#if 0
/* open_user_default_template
 * open the user's standard template if there is one
 * @return 0 for success non zero for failure
 **/
gint
open_user_default_template (ImportType type)
{
  gint ret = -1;
  gchar *filename = g_build_filename (get_user_data_dir (TRUE), "templates", "default.denemo", NULL);
  if (g_file_test (filename, G_FILE_TEST_EXISTS))
    {
      ret = open_for_real (filename, Denemo.project, TRUE, type);
    }
  g_free (filename);
  return ret;
}
#endif

/**
 * Creates dialog to say that the chosen filename already exists
 * and do you want to overwrite it.
 *
 */
static gboolean
replace_existing_file_dialog (const gchar * filename, gint format_id)
{
  gboolean ret;
  gchar *file = create_filename (filename, format_id);
  if (!g_file_test (file, G_FILE_TEST_EXISTS))
    {
      g_free (file);
      return TRUE;
    }

  gchar *primary = g_strdup_printf (_("A file with the name %s already exists"), file);
  ret = confirm (primary, _("Do you want to replace it?"));

  g_free (file);
  g_free (primary);
  return ret;
}


/**
 * Save parts to individual files
 */
void
file_savepartswrapper (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  if (gui->filename->len == 0)
    {
      file_saveas (FALSE);
    }

  export_lilypond_parts (gui->filename->str, gui);
}


static void
lilypond_selection_received (G_GNUC_UNUSED GtkClipboard * clipboard, const gchar * text, G_GNUC_UNUSED gpointer data)
{
  if (!text)
    {
      warningdialog (_("No selection text available"));
      return;
    }
    
  static gchar *timesig = NULL;
  if (timesig==NULL) timesig = g_strdup ("4/4");
  gchar *newtimesig = string_dialog_entry (Denemo.project, _("Paste LilyPond notes"), "Give time signature", timesig);
  if (newtimesig==NULL)
    return;
  gchar *filename = g_build_filename (get_user_data_dir (TRUE), "denemopaste.ly", NULL);
  FILE *fp = fopen (filename, "w");
  if (fp)
    {
     if (*newtimesig == 0)
       fprintf (fp, "music = { %s }\n\\score {\n\\music\n\\layout {}\n}\n", text);
     else
        fprintf (fp, "music = { \\time %s %s }\n\\score {\n\\music\n\\layout {}\n}\n", newtimesig, text);
      if (*newtimesig)
        {
          g_free (timesig);
          timesig = newtimesig;
        }
      fclose (fp);
      gint theclef = find_prevailing_clef (Denemo.project->movement);
      newview (NULL, NULL);
      gint fail = open_for_real (filename, Denemo.project, TRUE, REPLACE_SCORE);
      //thescore can be NULL after failed load....
      if (fail)
        {
          DenemoProject *gui = Denemo.project;
          //FIXME repeated code
          free_movements (gui);
          gtk_widget_destroy (Denemo.page);
          Denemo.projects = g_list_remove (Denemo.projects, gui);
          g_free (gui);
          warningdialog (_("Could not interpret selection as LilyPond notes"));
          return;
        }
      dnm_setinitialclef (Denemo.project->movement, (DenemoStaff *) Denemo.project->movement->currentstaff->data, theclef);
     
      call_out_to_guile ("(while (and (None?) (d-MoveToStaffDown)) (begin (d-MoveToStaffUp)(d-DeleteStaff)))");
      if (confirm (_("Paste LilyPond Notes"), _("Paste this music into your score?")))
        {
          DenemoProject *gui = Denemo.project;
          tohome (NULL, NULL);
          set_mark (NULL, NULL);
          toend (NULL, NULL);
          copywrapper (NULL, NULL);
          free_movements (gui);
          gtk_widget_destroy (Denemo.page);
          Denemo.projects = g_list_remove (Denemo.projects, gui);
          g_free (gui);
          pastewrapper (NULL, NULL);
        }
    }
}

void
paste_clipboard (DenemoAction * action, DenemoScriptParam * param)
{
  if (Denemo.project != g_list_last (Denemo.projects)->data)
    {
      warningdialog (_("Can only paste LilyPond text into the last tab, sorry"));
      return;
    }
  GtkClipboard *clipboard = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
  gtk_clipboard_request_text (clipboard, (GtkClipboardTextReceivedFunc) lilypond_selection_received, NULL);

}

static void
comment_selection_received (G_GNUC_UNUSED GtkClipboard * clipboard, const gchar * text)
{
 gchar *comment;
 GString *exceptions = g_string_new ("");
 gint i;
 for (i=0x1;i<0x10;i++)
    g_string_append_printf (exceptions, "%c", i);
  for (i=0x7F;i<0x100;i++)
    g_string_append_printf (exceptions, "%c", i);

  if ((!text) || (*text == 0))
    {
      warningdialog (_("No selection text available"));
      return;
    }
    gchar *escaped = g_strescape(text, exceptions->str);
    gchar *info = g_strconcat(_("Inserted:\n"), escaped, NULL);
    comment = g_strdup_printf ("(d-Comment \"%s\")(d-InfoDialog \"%s\")", escaped, info);
    call_out_to_guile (comment);
    g_string_free (exceptions, TRUE);
    g_free (escaped);
    g_free (info);
    g_free (comment);
}
void
paste_comment (DenemoAction * action, DenemoScriptParam * param)
{
    if(param && param->string && param->string->len)
        comment_selection_received (NULL, param->string->str);
    else
        {
        GtkClipboard *clipboard = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
        gtk_clipboard_request_text (clipboard, (GtkClipboardTextReceivedFunc) comment_selection_received, NULL);
        }
}
static void
export_interface(DenemoAction* action, DenemoScriptParam* param, gint format_id){
  GET_1PARAM(action, param, filename);
  if (filename==NULL)
    file_export(format_id);
  else
    if (action==NULL || replace_existing_file_dialog(filename, format_id)){
      filesel_save (Denemo.project, filename, format_id, SAVE_COPY);
      force_lily_refresh(Denemo.project);
    }
}

/**
 * Export mudela callback prompts for filename
 *
 */
void
export_mudela_action (DenemoAction * action, DenemoScriptParam * param)
{
  export_interface (action, param, MUDELA_FORMAT);
}
/**
 * Export musicxml callback prompts for filename
 *
 */
void
export_musicxml_action (DenemoAction * action, DenemoScriptParam * param)
{
  export_interface (action, param, MUSICXML_FORMAT);
}
/**
 * Export pdf callback prompts for filename
 *
 */
void
export_pdf_action (DenemoAction * action, DenemoScriptParam * param)
{
  export_interface (action, param, PDF_FORMAT);
}

/**
 * Export pdf callback prompts for filename
 *
 */
void
export_png_action (DenemoAction * action, DenemoScriptParam * param)
{
  export_interface (action, param, PNG_FORMAT);
}

/**
 * Export ABC callback prompts for filename
 *
 */
void
export_ABC_action (DenemoAction * action, DenemoScriptParam * param)
{
  export_interface (action, param, ABC_FORMAT);
}

/**
 * Export MIDI callback prompts for filename
 *
 */
void
export_midi_action (DenemoAction * action, DenemoScriptParam * param)
{
  export_interface (action, param, MIDI_FORMAT);
}
