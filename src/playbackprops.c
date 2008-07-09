/* playbackprops.c
 * callback that creates a "Playback Properties" dialog box asking
 * the user to change the properties of the playback for the current 
 * score
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Adam Tee, Matthew Hiller */

#include <stdlib.h>
#include <string.h>
#include <denemo/denemo.h>
#include "utils.h"
#include "csoundplayback.h"
#include "prefops.h"

struct callbackdata
{
  DenemoGUI *gui;
  DenemoPrefs *prefs;
  GtkWidget *csoundcommand;
  GtkWidget *orcfile;
  GtkWidget *rtcs;
  GtkWidget *midiplayerentry;
  GtkWidget *audioplayerentry;
  GtkWidget *checkplayback;
  GtkWidget *play_measure;
  GtkWidget *from_measure;
  GtkWidget *to_measure;
  GtkWidget *play_only;
  GtkWidget *staves;
};


/**
 * Callback: "Play all staves" was toggled
 */
static void
toggle_play_staves (GtkWidget * radio_button, struct callbackdata *cbdata)
{
  gtk_widget_set_sensitive (cbdata->staves,
			    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON
							  (radio_button)));
}

/**
 * Callback: "Play measure" was toggled
 */
static void
toggle_play_measure (GtkWidget * radio_button, struct callbackdata *cbdata)
{
  gboolean enabled =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (radio_button));
  gtk_widget_set_sensitive (cbdata->from_measure, enabled);
  gtk_widget_set_sensitive (cbdata->to_measure, enabled);
}


static void
set_preferences (struct callbackdata *cbdata)
{
  DenemoPrefs *prefs = cbdata->prefs;

  g_string_assign (prefs->midiplayer,
		   gtk_entry_get_text (GTK_ENTRY (cbdata->midiplayerentry)));
  g_string_assign (prefs->audioplayer,
		   gtk_entry_get_text (GTK_ENTRY (cbdata->audioplayerentry)));
  g_string_assign (prefs->csoundcommand,
		   gtk_entry_get_text (GTK_ENTRY (cbdata->csoundcommand)));

  if (!prefs->csoundorcfile)
    prefs->csoundorcfile =
      g_string_new (gtk_entry_get_text (GTK_ENTRY (cbdata->orcfile)));
  else
    g_string_assign (prefs->csoundorcfile,
		     gtk_entry_get_text (GTK_ENTRY (cbdata->orcfile)));

  prefs->rtcs =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->rtcs));

//  prefs->playbackoutput =
  //   gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->checkplayback));
  writeXMLPrefs (prefs);
}


void
playback_properties_change (GtkAction * action)
{
  DenemoGUI *gui = Denemo.gui;
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *table;
  GtkWidget *hbox;
  GtkWidget *tempo;
  GtkWidget *play_entire_piece;
  GtkWidget *play_measure;
  GtkWidget *from_measure;
  GtkWidget *to_measure;
  GtkWidget *play_all_staves;
  GtkWidget *play_only;
  GtkWidget *staves;
  GtkWidget *notebook;
  GtkWidget *csoundcommand;
  GtkWidget *orcfile;
  GtkWidget *rtcs;
  GtkWidget *button;
  GtkWidget *midiplayerentry;
  GtkWidget *audioplayerentry;
  GtkWidget *checkplayback;
  staffnode *n;


  static struct callbackdata cbdata;
  static struct cs_callback csdata;	/* for csound file selection */

  dialog = gtk_dialog_new_with_buttons (_("Playback properties"),
					GTK_WINDOW (Denemo.window),
					(GtkDialogFlags) (GTK_DIALOG_MODAL |
							  GTK_DIALOG_DESTROY_WITH_PARENT),
					GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
					GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
					NULL);

  gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);
  notebook = gtk_notebook_new ();

  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), notebook, TRUE,
		      TRUE, 0);
  table = gtk_table_new (8, 2, FALSE);

  gtk_container_set_border_width (GTK_CONTAINER (table), 12);
  gtk_table_set_row_spacings (GTK_TABLE (table), 8);
  gtk_table_set_col_spacings (GTK_TABLE (table), 8);

  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), table, NULL);
  gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (notebook), table,
				   _("Playback"));


  label = gtk_label_new (_("<b>Tempo</b>"));
  gtk_table_attach (GTK_TABLE (table), label, 0, 2, 0, 1,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);

  // spacer
  label = gtk_label_new ("");
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 1, 2,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_set_size_request (label, 12, -1);

  hbox = gtk_hbox_new (FALSE, 8);
  gtk_table_attach (GTK_TABLE (table), hbox, 1, 2, 1, 2,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (GTK_FILL), 0, 0);

  tempo = gtk_spin_button_new_with_range (10.0, 250.0, 1.0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (tempo),
			     (gdouble) gui->si->tempo);
  gtk_box_pack_start (GTK_BOX (hbox), tempo, FALSE, FALSE, 0);

  label = gtk_label_new (_("BPM"));
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);


  label = gtk_label_new (_("<b>Play measures</b>"));
  gtk_table_attach (GTK_TABLE (table), label, 0, 2, 2, 3,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);


  // spacer
  label = gtk_label_new ("");
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 3, 4,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_set_size_request (label, 12, -1);


  play_entire_piece =
    gtk_radio_button_new_with_label (NULL, _("Play from cursor to end"));
  gtk_table_attach (GTK_TABLE (table), play_entire_piece, 1, 2, 3, 4,
		    (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0,
		    0);

  // spacer
  label = gtk_label_new ("");
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 4, 5,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_set_size_request (label, 12, -1);

  hbox = gtk_hbox_new (FALSE, 8);
  gtk_table_attach (GTK_TABLE (table), hbox, 1, 2, 4, 5,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (GTK_FILL), 0, 0);

  play_measure =
    gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON
						 (play_entire_piece),
						 _("Play measure"));
  gtk_box_pack_start (GTK_BOX (hbox), play_measure, FALSE, FALSE, 0);

  gint max_mesure =
    g_list_length (((DenemoStaff *) (gui->si->thescore->data))->measures);

  from_measure =
    gtk_spin_button_new_with_range (1.0, (gdouble) max_mesure, 1.0);
  gtk_box_pack_start (GTK_BOX (hbox), from_measure, TRUE, TRUE, 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (from_measure),
			     (gdouble) gui->si->start);

  label = gtk_label_new (_("to"));
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);

  to_measure =
    gtk_spin_button_new_with_range (1.0, (gdouble) max_mesure, 1.0);
  gtk_box_pack_start (GTK_BOX (hbox), to_measure, TRUE, TRUE, 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (to_measure),
			     (gdouble) gui->si->end);

  if (gui->si->start == 0)
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (play_entire_piece),
				    TRUE);
      gtk_widget_set_sensitive (from_measure, FALSE);
      gtk_widget_set_sensitive (to_measure, FALSE);
    }
  else
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (play_measure), TRUE);
    }


  label = gtk_label_new (_("<b>Play staves</b>"));
  gtk_table_attach (GTK_TABLE (table), label, 0, 2, 5, 6,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);

  // spacer
  label = gtk_label_new ("");
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 6, 7,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_set_size_request (label, 12, -1);

  play_all_staves =
    gtk_radio_button_new_with_label (NULL, _("Play all staves"));
  gtk_table_attach (GTK_TABLE (table), play_all_staves, 1, 2, 6, 7,
		    (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0,
		    0);
  // spacer
  label = gtk_label_new ("");
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 7, 8,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_set_size_request (label, 12, -1);

  hbox = gtk_hbox_new (FALSE, 8);
  gtk_table_attach (GTK_TABLE (table), hbox, 1, 2, 7, 8,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (GTK_FILL), 0, 0);

  play_only =
    gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON
						 (play_all_staves),
						 _("Play only"));
  gtk_box_pack_start (GTK_BOX (hbox), play_only, FALSE, FALSE, 0);

  staves = gtk_combo_box_new_text ();
  gtk_box_pack_start (GTK_BOX (hbox), staves, TRUE, TRUE, 0);
  for (n = gui->si->thescore; n != NULL; n = g_list_next (n))
    {
      gchar *staff_label = NULL;
      DenemoStaff *s = (DenemoStaff *) n->data;
      if (s->staff_name != NULL)
	{
	  staff_label = s->staff_name->str;
	}
      else if (s->denemo_name != NULL)
	{
	  staff_label = s->denemo_name->str;
	}

      if (staff_label == NULL)
	{
	  staff_label = _("Unnamed staff");
	}
      gtk_combo_box_append_text (GTK_COMBO_BOX (staves), staff_label);
    }
  if (gui->si->stafftoplay == 0)
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (play_all_staves),
				    TRUE);
      gtk_widget_set_sensitive (staves, FALSE);
      gtk_combo_box_set_active (GTK_COMBO_BOX (staves), 0);
    }
  else
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (play_only), TRUE);
      gtk_combo_box_set_active (GTK_COMBO_BOX (staves),
				gui->si->stafftoplay - 1);
    }

  cbdata.gui = gui;
  cbdata.play_measure = play_measure;
  cbdata.from_measure = from_measure;
  cbdata.to_measure = to_measure;
  cbdata.play_only = play_only;
  cbdata.staves = staves;

  g_signal_connect (play_only, "toggled", G_CALLBACK (toggle_play_staves),
		    &cbdata);
  g_signal_connect (play_measure, "toggled", G_CALLBACK (toggle_play_measure),
		    &cbdata);


  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);


  /*
   * CSound Settings
   */
  table = gtk_table_new (4, 2, FALSE);
  gtk_container_set_border_width (GTK_CONTAINER (table), 12);
  gtk_table_set_row_spacings (GTK_TABLE (table), 8);
  gtk_table_set_col_spacings (GTK_TABLE (table), 8);
  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), table, NULL);
  gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (notebook), table,
				   _("CSound Settings"));
  label = gtk_label_new (_("CSound Command:"));
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 0, 1,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  csoundcommand = gtk_entry_new ();
  gtk_entry_set_text (GTK_ENTRY (csoundcommand),
		      Denemo.prefs.csoundcommand->str);
  gtk_table_attach (GTK_TABLE (table), csoundcommand, 1, 2, 0, 1,
		    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  label = gtk_label_new (_("Orchestra Filename:"));
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 1, 2,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  orcfile = gtk_entry_new ();
  if (Denemo.prefs.csoundorcfile)
    gtk_entry_set_text (GTK_ENTRY (orcfile), Denemo.prefs.csoundorcfile->str);
  gtk_table_attach (GTK_TABLE (table), orcfile, 1, 2, 1, 2,
		    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  csdata.entry = orcfile;
  csdata.gui = gui;
  csdata.dialog = dialog;

  button = gtk_button_new_with_label (_("Choose File"));
  gtk_table_attach (GTK_TABLE (table), button, 1, 2, 2, 3,
		    (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0,
		    0);
  gtk_signal_connect (GTK_OBJECT (button), "clicked",
		      GTK_SIGNAL_FUNC (chooseorcfile), &csdata);
  gtk_widget_show (button);


  label = gtk_label_new (_("playback after render"));
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 3, 4,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (label);

  rtcs = gtk_check_button_new ();
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (rtcs), Denemo.prefs.rtcs);
  gtk_table_attach (GTK_TABLE (table), rtcs, 1, 2, 3, 4,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);

  gtk_widget_show (rtcs);

  /*
   * Playback settings
   */
  table = gtk_table_new (2, 2, FALSE);
  gtk_container_set_border_width (GTK_CONTAINER (table), 12);
  gtk_table_set_row_spacings (GTK_TABLE (table), 8);
  gtk_table_set_col_spacings (GTK_TABLE (table), 8);
  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), table, NULL);
  gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (notebook), table,
				   _("Externals"));

  label = gtk_label_new (_("External midi player:"));
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 1, 2,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);

  midiplayerentry = gtk_entry_new ();
  gtk_entry_set_text (GTK_ENTRY (midiplayerentry),
		      Denemo.prefs.midiplayer->str);
  gtk_table_attach (GTK_TABLE (table), midiplayerentry, 1, 2, 1, 2,
		    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);

  label = gtk_label_new (_("External audio player:"));
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 2, 3,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);

  audioplayerentry = gtk_entry_new ();
  gtk_entry_set_text (GTK_ENTRY (audioplayerentry),
		      Denemo.prefs.audioplayer->str);
  gtk_table_attach (GTK_TABLE (table), audioplayerentry, 1, 2, 2, 3,
		    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);

  gtk_widget_show_all (dialog);

  /* Set up the callback data */
  cbdata.prefs = &Denemo.prefs;
  cbdata.midiplayerentry = midiplayerentry;
  cbdata.audioplayerentry = audioplayerentry;
  cbdata.csoundcommand = csoundcommand;
  cbdata.orcfile = orcfile;
  cbdata.rtcs = rtcs;

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_entry_set_activates_default (GTK_ENTRY (tempo), TRUE);  
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      gui->si->tempo =
	gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (tempo));
      //printf("\nsi->tempo = %i\n", si->tempo);
      if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (play_measure)))
	{
	  gui->si->start =
	    gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (from_measure));
	  gui->si->end =
	    gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (to_measure));
	}
      else
	{

	  gui->si->start = 0;
	  gui->si->end = 0;
	}

      if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (play_only)))
	{
	  gui->si->stafftoplay =
	    1 + gtk_combo_box_get_active (GTK_COMBO_BOX (staves));
	  //printf("\nstafftoplay = %i\n", si->stafftoplay);
	}
      else
	{
	  gui->si->stafftoplay = 0;
	  //printf("\nstafftoplay = %i\n", si->stafftoplay);
	}


      set_preferences (&cbdata);
    }
  gtk_widget_destroy (dialog);


}
