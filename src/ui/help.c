/* help.cpp
 * implements the stuff under Help in the menubar
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller
 */

#include <denemo/denemo.h>
#include "config.h"
#include "core/utils.h"
#include "core/kbd-custom.h"
#include <string.h>             /* for strlen */
/* The tutorial mentioned that the actual gchar * held within a
 * GtkText widget needs to be freed.  I don't do such a free, though,
 * so I think this function has a memory leak in it. */

/**
 * Create the about dialog
 *
 */
void
about (DenemoAction * action, DenemoScriptParam* callback_data)
{
  GtkWidget *dialog;
  const char *authors[] = { "Richard Shann", "Jeremiah Benham", "Matthew Hiller", "Adam Tee", "Nils Gey", NULL };

  dialog = gtk_about_dialog_new ();
  gtk_about_dialog_set_program_name (GTK_ABOUT_DIALOG (dialog), _("GNU Denemo"));
  gtk_about_dialog_set_comments (GTK_ABOUT_DIALOG (dialog), _("Free and Open Music Notation Editor"));
  gtk_about_dialog_set_version (GTK_ABOUT_DIALOG (dialog), VERSION);
  gtk_about_dialog_set_website (GTK_ABOUT_DIALOG (dialog), "http://www.denemo.org");
  gtk_about_dialog_set_website_label (GTK_ABOUT_DIALOG (dialog), _("Denemo Website"));
  gtk_about_dialog_set_license (GTK_ABOUT_DIALOG (dialog), _("(c) 1999 - 2021 Matthew Hiller, Adam Tee, Jeremiah Benham, Richard Shann and others.\n\n\
http://www.denemo.org\n\n\
  This program is free software; you can redistribute it and/or modify\
  it under the terms of the GNU General Public License as published by\
  the Free Software Foundation; either version 3 of the License, or\
  (at your option) any later version.\
  This program is distributed in the hope that it will be useful,\
  but WITHOUT ANY WARRANTY; without even the implied warranty of\
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\
  GNU General Public License for more details.\
  You should have received a copy of the GNU General Public License\
  along with this program; if not, write to the Free Software\
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,\
  MA 02110-1301, USA."));

  gtk_about_dialog_set_wrap_license (GTK_ABOUT_DIALOG (dialog), TRUE);
  gtk_about_dialog_set_authors (GTK_ABOUT_DIALOG (dialog), authors);
  gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (Denemo.window));
  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
}

/**
 * Function to allow browsing the user manual
 * uses the given web browser to display the manual
 * If param contains a url it opens that
 */
void
browse_manual (DenemoAction * action, DenemoScriptParam * param)
{
  GET_1PARAM (action, param, url);
  gboolean retval;
  GError *error = NULL;

  /* get the uri to the manual */
  gchar *manualpath = g_build_filename (get_system_data_dir (), "manual",
                                        "denemo-manual.html", NULL);
  gchar *manualuri = url ? g_strdup (url) : g_filename_to_uri (manualpath, NULL, NULL);

  /* check that the browser exists */
  gchar *browserpath = g_find_program_in_path (Denemo.prefs.browser->str);
  if (browserpath == NULL)
    {
      if (run_file_association (manualuri))
        return;
      /* show a warning dialog */
      GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW (Denemo.window),
                                                  GTK_DIALOG_DESTROY_WITH_PARENT,
                                                  GTK_MESSAGE_WARNING,
                                                  GTK_BUTTONS_OK,
                                                  _("Could not find %s in the path"),
                                                  Denemo.prefs.browser->str);
      gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog), _("Please edit the chosen " "browser in the " "preferences."));
      gtk_dialog_run (GTK_DIALOG (dialog));

      /* free the memory and return */
      gtk_widget_destroy (dialog);
      g_free (manualpath);
      g_free (manualuri);
      return;
    }

  /* spawn the process to show the manual */
  gchar *argv[] = { Denemo.prefs.browser->str,
    manualuri,
    NULL
  };
  retval = g_spawn_async (NULL, argv, NULL, G_SPAWN_SEARCH_PATH, NULL, NULL, NULL, &error);
  if (!retval)
    {
      g_message (_("Could not execute specified web browser: %s"), error->message);
      g_error_free (error);
    }

  /* free the memory */
  g_free (browserpath);
  g_free (manualpath);
  g_free (manualuri);
}
void email_help (gchar *page)
{
  DenemoScriptParam param;
  gchar *filename = g_build_filename (g_get_tmp_dir (), "Denemo_email", NULL);
  
  g_file_set_contents (filename, page, -1, NULL);
  param.string = g_string_new (g_strdup_printf("%s%s", "file://", filename));
  param.status = FALSE;
  browse_manual (NULL, &param);
}
void display_shortcuts (void)
{
  GtkWidget *window =  gtk_window_new (GTK_WINDOW_TOPLEVEL);
  GtkTextView *text_view = (GtkTextView*)gtk_text_view_new ();
  gtk_text_view_set_editable (GTK_TEXT_VIEW (text_view), FALSE);
  GtkWidget *scrolled_text_view = gtk_scrolled_window_new (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0), gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
  gtk_container_add (GTK_CONTAINER (scrolled_text_view), GTK_WIDGET(text_view));
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_text_view), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_set_size_request (GTK_WIDGET (scrolled_text_view), 150, 300);
  gtk_container_add (GTK_CONTAINER (window), scrolled_text_view);
  GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text_view));
  GString *shortcuts = keymap_get_bindings (Denemo.map);
  gtk_text_buffer_set_text (buffer, shortcuts->str, -1);
  gtk_widget_show_all(window);
  Denemo.prefs.learning = TRUE;
  g_string_free(shortcuts, TRUE);
}
