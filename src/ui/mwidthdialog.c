/* mwidthdialog.cpp
 * a callback that creates a dialog box prompting the user to change the
 * measure width - this is a denemo display-only thing(?)

 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller */

#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>
#include "display/calculatepositions.h"
#include "command/commandfuncs.h"
#include "command/contexts.h"
#include <denemo/denemo.h>
#include "display/draw.h"
#include "command/staff.h"
#include "core/utils.h"

struct callbackdata
{
  DenemoProject *gui;
  GtkWidget *textentry;
};

/**
 * Set the measurewidth to the given value
 *
 */
void
set_mwidth (GtkWidget * widget, gpointer data)
{
  struct callbackdata *cbdata = (struct callbackdata *) data;
  gint width = atoi (gtk_entry_get_text (GTK_ENTRY (cbdata->textentry)));

  if (width)
    cbdata->gui->movement->measurewidth = width;
  adjustmeasurewidth (cbdata->gui->movement, 0);
  draw_score_area();
}

/**
 * Dialog to set the si->measurewidth value
 *
 */
void
score_mwidth_change (DenemoAction * action, gpointer param)
{
  DenemoProject *gui = Denemo.project;
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *textentry;
//  GtkWidget *okbutton;
//  GtkWidget *cancelbutton;
  static GString *entrycontent = NULL;
  static struct callbackdata cbdata;

  if (!entrycontent)
    entrycontent = g_string_new (NULL);

  dialog = gtk_dialog_new ();
  gtk_window_set_title (GTK_WINDOW (dialog), _("Set minimum measure width"));

  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));

  GtkWidget *hbox = gtk_hbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (content_area), hbox);

  label = gtk_label_new (_("Enter width (in pixels) of measures:"));
  gtk_container_add (GTK_CONTAINER (hbox), label);

  textentry = gtk_entry_new ();
  g_string_sprintf (entrycontent, "%d", gui->movement->measurewidth);
  gtk_entry_set_text (GTK_ENTRY (textentry), entrycontent->str);
  gtk_box_pack_start (GTK_BOX (hbox), textentry, TRUE, TRUE, 0);
  gtk_widget_show (textentry);

//  okbutton = gtk_button_new_with_label (_("OK"));
//  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->action_area),
//                    okbutton, TRUE, TRUE, 0);
  cbdata.textentry = textentry;
  cbdata.gui = gui;

  processenter (textentry, G_CALLBACK (set_mwidth), cbdata, dialog);
  // gtk_signal_connect (G_OBJECT (okbutton), "clicked",
//                    GTK_SIGNAL_FUNC (set_mwidth), &cbdata);
//  gtk_signal_connect_object (G_OBJECT (okbutton), "clicked",
//                           GTK_SIGNAL_FUNC (gtk_widget_destroy),
//                           G_OBJECT (dialog));
//  gtk_widget_show (okbutton);

//  cancelbutton = gtk_button_new_with_label (_("Cancel"));
//  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->action_area),
//                    cancelbutton, TRUE, TRUE, 0);
//  gtk_signal_connect_object (G_OBJECT (cancelbutton), "clicked",
//                           GTK_SIGNAL_FUNC (gtk_widget_destroy),
//                           G_OBJECT (dialog));
//  gtk_widget_show (cancelbutton);

  gtk_widget_grab_focus (textentry);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_widget_show_all (dialog);
}
