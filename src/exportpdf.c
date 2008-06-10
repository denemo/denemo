/* exportpdf.cpp
 * Export pdf using lilypond
 *
 * (c) 2000-2005 Adam Tee
 */

#include <unistd.h>
#include <errno.h>
#include "config.h"
#include <denemo/denemo.h>
#include "exportpdf.h"
#include "exportlilypond.h"
#include "file.h"
#include "external.h"
#include "prefops.h"

/**
 * Export pdf callback prompts for filename
 *
 */

void
export_pdf_action (GtkAction * action, DenemoGUI * gui)
{

  GtkWidget *file_selection;

  file_selection = gtk_file_chooser_dialog_new (_("Export PDF"),
						GTK_WINDOW (gui->window),
						GTK_FILE_CHOOSER_ACTION_SAVE,
						GTK_STOCK_CANCEL,
						GTK_RESPONSE_REJECT,
						GTK_STOCK_SAVE,
						GTK_RESPONSE_ACCEPT, NULL);

  gtk_widget_show_all (file_selection);
  gboolean close = FALSE;
  do
    {

      if (gtk_dialog_run (GTK_DIALOG (file_selection)) == GTK_RESPONSE_ACCEPT)
	{
	  gchar *filename =
	    gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (file_selection));

	  if (replace_existing_file_dialog
	      (filename, GTK_WINDOW (gui->window), -1))
	    {
	      export_pdf (filename, gui);
	      close = TRUE;
	    }
	  g_free (filename);
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
 * Does all the export pdf work.
 * calls exportmudela and then  
 * runs lilypond to a create a temporary pdf file and 
 * renames to filename.pdf
 *
 *	@param filename filename to save score to
 *  @param gui pointer to the DenemoGUI structure
 */
void
export_pdf (const gchar * filename, DenemoGUI * gui)
{
  const gchar *tmpdir = locatedotdenemo ();
  gchar *tmpfile;
  gchar *mudelafile;
  gchar *midifile;
  gchar *dvifile;
  gchar *psfile;
  gchar *pdffile;
  GError *err = NULL;
  gint exit_status;
  gboolean ok;

  /* look for lilypond */
  gchar *lilypath = g_find_program_in_path (Denemo.prefs.lilypath->str);
  if (lilypath == NULL)
    {
      /* show a warning dialog */
      GtkWidget *dialog =
        gtk_message_dialog_new (GTK_WINDOW (gui->window),
                                GTK_DIALOG_DESTROY_WITH_PARENT,
                                GTK_MESSAGE_WARNING,
                                GTK_BUTTONS_OK,
                                _("Could not find %s"),
                                Denemo.prefs.lilypath->str);
      gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                                                _("Please edit lilypond path "
                                                  "in the preferences."));
      gtk_dialog_run (GTK_DIALOG (dialog));

      /* free the memory and return */
      gtk_widget_destroy (dialog);
      return;
    }

  /* create a temp (and not existing) filepath in .denemo folder */
  do
    {
      tmpfile = get_temp_filename (NULL);
      mudelafile = g_strconcat (tmpfile, ".ly", NULL);
      midifile = g_strconcat (tmpfile, ".midi", NULL);
      dvifile = g_strconcat (tmpfile, ".dvi", NULL);
      psfile = g_strconcat (tmpfile, ".ps", NULL);
      pdffile = g_strconcat (tmpfile, ".pdf", NULL);

      if (g_file_test (mudelafile, G_FILE_TEST_EXISTS) ||
          g_file_test (midifile, G_FILE_TEST_EXISTS) ||
          g_file_test (dvifile, G_FILE_TEST_EXISTS) ||
          g_file_test (psfile, G_FILE_TEST_EXISTS) ||
          g_file_test (pdffile, G_FILE_TEST_EXISTS))
        ok = FALSE;
      else
        ok = TRUE;

      if (!ok)
        {
          g_free (tmpfile);
          g_free (mudelafile);
          g_free (midifile);
          g_free (dvifile);
          g_free (psfile);
          g_free (pdffile);
        }
    }
  while(!ok);

  /* generate the lilypond file */
  exportlilypond (mudelafile, gui, TRUE);

  /* generate the pdf file */
  gchar *argv[] =
    {
      Denemo.prefs.lilypath->str,
      "--pdf",
      "-o",
      tmpfile,
      mudelafile,
      NULL
    };
  g_spawn_sync (tmpdir,         /* dir */
                argv, NULL,     /* env */
                G_SPAWN_SEARCH_PATH, NULL,      /* child setup func */
                NULL,           /* user data */
                NULL,           /* stdout */
                NULL,           /* stderr */
                &exit_status, &err);

  if (err != NULL)
    {
      g_warning ("%s", err->message);
      g_error_free (err);
      remove (mudelafile);

      g_free (tmpfile);
      g_free (mudelafile);
      g_free (midifile);
      g_free (dvifile);
      g_free (psfile);
      g_free (pdffile);

      return;
    }

  /* move the pdf file to its destination */
  if (rename (pdffile, filename) != 0)
    g_warning ("Failed to rename %s to %s\n", pdffile, filename);

  /* remove unnecessary files and free the memory */
  remove (mudelafile);
  remove (midifile);
  remove (dvifile);
  remove (psfile);

  g_free (tmpfile);
  g_free (mudelafile);
  g_free (midifile);
  g_free (dvifile);
  g_free (psfile);
  g_free (pdffile);
}
