/* tupletops.cpp  
 * Set Tuplet options 
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Adam Tee Matthew Hiller
 */

#include <gtk/gtk.h>
#include <stdlib.h>
#include <string.h>
#include "tupletops.h"
#include "chordops.h"
#include "contexts.h"
#include <denemo/denemo.h>
#include "staffops.h"
#include "utils.h"
#include "draw.h"
#include "measureops.h"
#include "midi.h"
#include "objops.h"
#include "commandfuncs.h"

DenemoObject *
newtupopen (gint numerator, gint denominator)
{
  DenemoObject *tuplet;
  tupopen *newtup = (tupopen *) g_malloc (sizeof (tupopen));
  tuplet = (DenemoObject *) g_malloc (sizeof (DenemoObject));
  tuplet->type = TUPOPEN;
  newtup->numerator = numerator;
  newtup->denominator = denominator;

  tuplet->object = newtup;
  set_basic_numticks (tuplet);
  setpixelmin (tuplet);
  return tuplet;
}

DenemoObject *
newtupclose ()
{
  DenemoObject *tuplet;
  tupopen *newtup = (tupopen *) g_malloc (sizeof (tupopen));//avoids a null object
  tuplet = (DenemoObject *) g_malloc (sizeof (DenemoObject));
  tuplet->type = TUPCLOSE;
  tuplet->object = newtup;//avoids a null object
  set_basic_numticks (tuplet);
  setpixelmin (tuplet);
  return tuplet;
}




/* This is broken at the moment because the program doesn't pause
 * when the dialog is created. Fix me. */

void
tupletchangedialog (DenemoObject * theobj, GtkWidget * scorearea)
{

  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *numerator;
  GtkWidget *denominator;


  GString *entrycontent = NULL;

  dialog =
    gtk_dialog_new_with_buttons (_("Customize tuplet multiplier"), NULL,
				 (GtkDialogFlags) (GTK_DIALOG_MODAL |
						   GTK_DIALOG_DESTROY_WITH_PARENT),
				 GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
				 GTK_STOCK_CANCEL, GTK_STOCK_CANCEL, NULL);
  if (!entrycontent)
    entrycontent = g_string_new (NULL);


  label = gtk_label_new (_("Numerator"));
#ifdef _USE_GTK3_
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)),
		      label, TRUE, TRUE, 0);
#else
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox),
		      label, TRUE, TRUE, 0);
#endif
  gtk_widget_show (label);

  numerator = gtk_entry_new ();
  g_string_sprintf (entrycontent, "%d",
		    ((tupopen *) theobj->object)->numerator);
  gtk_entry_set_text (GTK_ENTRY (numerator), entrycontent->str);
#ifdef _USE_GTK3_
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)),
		      numerator, TRUE, TRUE, 0);
#else
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox),
		      numerator, TRUE, TRUE, 0);
#endif
  gtk_widget_show (numerator);

  label = gtk_label_new (_("Denominator"));
#ifdef _USE_GTK3_
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)),
		      label, TRUE, TRUE, 0);
#else
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox),
		      label, TRUE, TRUE, 0);
#endif
  gtk_widget_show (label);

  denominator = gtk_entry_new ();
  g_string_sprintf (entrycontent, "%d",
		    ((tupopen *) theobj->object)->denominator);
  gtk_entry_set_text (GTK_ENTRY (denominator), entrycontent->str);
#ifdef _USE_GTK3_
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)),
		      denominator, TRUE, TRUE, 0);
#else
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox),
		      denominator, TRUE, TRUE, 0);
#endif
  gtk_widget_show (denominator);



  gtk_widget_grab_focus (numerator);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_entry_set_activates_default (GTK_ENTRY (numerator), TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY (denominator), TRUE);
  gtk_widget_show (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      ((tupopen *) theobj->object)->numerator =
	atoi (gtk_entry_get_text (GTK_ENTRY (numerator)));
      ((tupopen *) theobj->object)->denominator =
	atoi (gtk_entry_get_text (GTK_ENTRY (denominator)));
    }

  gtk_widget_destroy (dialog);
}
