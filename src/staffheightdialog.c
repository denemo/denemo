/* staffheightdialog.cpp
 * a callback that creates a dialog box prompting the user to change the
 * staffheightdialog
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller */

#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>
#include "contexts.h"
#include <denemo/denemo.h>
#include "staffops.h"
#include "utils.h"

struct callbackdata
{
  struct scoreinfo *si;
  GtkWidget *textentry;
};

void
set_staffspace (GtkWidget * widget, gpointer data)
{
  struct callbackdata *cbdata = (struct callbackdata *) data;
  gint space = atoi (gtk_entry_get_text (GTK_ENTRY (cbdata->textentry)));

  if (space >= 2 * STAFF_HEIGHT && space < 10000)
    cbdata->si->staffspace = space;
  gtk_widget_queue_draw (cbdata->si->scorearea);
}

void
score_staffspace_change (GtkAction * action, gpointer callback_data)
{
  struct scoreinfo *si = (struct scoreinfo *) callback_data;
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *textentry;
  GtkWidget *okbutton;
  GtkWidget *cancelbutton;
  static GString *entrycontent = NULL;
  static struct callbackdata cbdata;

  if (!entrycontent)
    entrycontent = g_string_new (NULL);

  dialog = gtk_dialog_new ();
  gtk_window_set_title (GTK_WINDOW (dialog), _("Set staff height"));

  label = gtk_label_new (_("Enter space (in pixels) between staves:"));
#ifdef _USE_GTK3_
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)),
		      label, TRUE, TRUE, 0);
#else
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox),
		      label, TRUE, TRUE, 0);
#endif
  gtk_widget_show (label);

  textentry = gtk_entry_new ();
  g_string_sprintf (entrycontent, "%d", si->staffspace);
  gtk_entry_set_text (GTK_ENTRY (textentry), entrycontent->str);
#ifdef _USE_GTK3_
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)),
		      textentry, TRUE, TRUE, 0);
#else
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox),
		      textentry, TRUE, TRUE, 0);
#endif  
  gtk_widget_show (textentry);

  okbutton = gtk_button_new_with_label (_("OK"));
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->action_area),
		      okbutton, TRUE, TRUE, 0);
  cbdata.textentry = textentry;
  cbdata.si = si;

  processenter (textentry, set_staffspace, cbdata, dialog);
  gtk_signal_connect (G_OBJECT (okbutton), "clicked",
		      GTK_SIGNAL_FUNC (set_staffspace), &cbdata);
  gtk_signal_connect_object (G_OBJECT (okbutton), "clicked",
			     GTK_SIGNAL_FUNC (gtk_widget_destroy),
			     G_OBJECT (dialog));
  gtk_widget_show (okbutton);

  cancelbutton = gtk_button_new_with_label (_("Cancel"));
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->action_area),
		      cancelbutton, TRUE, TRUE, 0);
  gtk_signal_connect_object (G_OBJECT (cancelbutton), "clicked",
			     GTK_SIGNAL_FUNC (gtk_widget_destroy),
			     G_OBJECT (dialog));
  gtk_widget_show (cancelbutton);

  gtk_widget_grab_focus (textentry);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_widget_show (dialog);
}
