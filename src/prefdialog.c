/* prefdialog.cpp
 * functions for a preferences dialog
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller, Adam Tee */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <denemo/denemo.h>
#include "prefops.h"
#include "utils.h"
#ifdef _HAVE_JACK_
#include "jackmidi.h"
#endif
#ifdef _HAVE_FLUIDSYNTH_
#include "fluid.h"
#endif

struct callbackdata
{
  DenemoPrefs *prefs;
  GtkWidget *lilypath;
  GtkWidget *immediateplayback;
  GtkWidget *saveparts;
  GtkWidget *autosave;
  GtkWidget *notation_palette;
  GtkWidget *rhythm_palette;
  GtkWidget *object_palette;
  GtkWidget *articulation_palette;
  GtkWidget *visible_directive_buttons;
  GtkWidget *autoupdate;

  GtkWidget *autosave_timeout;
  GtkWidget *maxhistory;
  GtkWidget *browser;
  GtkWidget *pdfviewer;
  GtkWidget *imageviewer;
  GtkWidget *username;
  GtkWidget *password;
  GtkWidget *sequencer;
  GtkWidget *midi_in;
#ifdef _HAVE_JACK_
  GtkWidget *jacktransport;
  GtkWidget *jacktransport_start_stopped;
  GtkWidget *jack_at_startup;
#endif
#ifdef _HAVE_FLUIDSYNTH_
  GtkWidget *fluidsynth_audio_driver;
#endif
  GtkWidget *texteditor;
  GtkWidget *midiplayer;
  GtkWidget *denemopath;
  GtkWidget *temperament;
  GtkWidget *strictshortcuts;
  GtkWidget *resolution;
  GtkWidget *overlays;
  GtkWidget *continuous;
};

struct callbackdata1
{
  DenemoGUI *gui;
  GtkListStore *model;
};

/**
 * Callback to enable/disable the autosave entry when the auto save button is 
 * clicked
 */
static void
toggle_autosave (GtkToggleButton * togglebutton, GtkWidget * autosave_timeout)
{
  g_debug("autosave now %d\n", 
     gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(autosave_timeout)));
  gtk_widget_set_sensitive (autosave_timeout,
			    gtk_toggle_button_get_active (togglebutton));
}

static void
set_preferences (struct callbackdata *cbdata)
{
  DenemoPrefs *prefs = cbdata->prefs;


#define ASSIGNTEXT(field) \
  g_string_assign (prefs->field,\
                   gtk_entry_get_text (GTK_ENTRY (cbdata->field)));

#define ASSIGNBOOLEAN(field) \
  prefs->field =\
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(cbdata->field));

#define ASSIGNINT(field) \
   prefs->field =\
    gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(cbdata->field));


  ASSIGNTEXT(lilypath)
  ASSIGNTEXT(browser) 
  ASSIGNTEXT(pdfviewer)
  ASSIGNTEXT(imageviewer)
  ASSIGNTEXT(username)
  ASSIGNTEXT(password)
  ASSIGNTEXT(texteditor)
  ASSIGNTEXT(midiplayer)
  ASSIGNTEXT(denemopath)
  ASSIGNTEXT(sequencer)
  ASSIGNTEXT(midi_in)
  ASSIGNTEXT(sequencer)
  ASSIGNTEXT(midi_in)
#ifdef _HAVE_JACK_
  ASSIGNBOOLEAN(jacktransport)
  ASSIGNBOOLEAN(jacktransport_start_stopped)
  ASSIGNBOOLEAN(jack_at_startup)
#endif
#ifdef _HAVE_FLUIDSYNTH_
  ASSIGNTEXT(fluidsynth_audio_driver);
#endif
  ASSIGNTEXT(temperament)
  ASSIGNBOOLEAN(strictshortcuts)
  ASSIGNBOOLEAN(overlays)
  ASSIGNBOOLEAN(continuous)
  ASSIGNINT(resolution)
  ASSIGNINT(maxhistory)
  ASSIGNBOOLEAN(immediateplayback)
  ASSIGNBOOLEAN(autosave)
  ASSIGNINT(autosave_timeout)
  ASSIGNBOOLEAN(articulation_palette)
  ASSIGNBOOLEAN(visible_directive_buttons)
  ASSIGNBOOLEAN(autoupdate)
  ASSIGNBOOLEAN(notation_palette)
  ASSIGNBOOLEAN(rhythm_palette)
  ASSIGNBOOLEAN(object_palette)
  ASSIGNBOOLEAN(saveparts)
  //g_print ("Timeout %d \n", prefs->autosave_timeout);

  /* Now write it all to denemorc */
  writeXMLPrefs (prefs);
}

void
preferences_change (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *main_vbox;
  GtkWidget *autosave;
  GtkWidget *autosave_timeout;
  GtkWidget *maxhistory;
  GtkWidget *notebook;
  GtkWidget *hbox;
  GtkWidget *vbox;
  GtkListStore *list_store;
  GtkWidget *tree;
  GtkTreeIter iter;
  GtkTreeViewColumn *column;
  GtkCellRenderer *renderer;
  GtkWidget *entrywidget;
  static struct callbackdata cbdata;
  g_assert (gui != NULL);

  dialog = gtk_dialog_new_with_buttons (_("Preferences - Denemo"),
					GTK_WINDOW (Denemo.window),
					(GtkDialogFlags) (GTK_DIALOG_MODAL |
							  GTK_DIALOG_DESTROY_WITH_PARENT),
					GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
					GTK_STOCK_CANCEL, GTK_STOCK_CANCEL,
					NULL);

  gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);

  notebook = gtk_notebook_new ();
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), notebook, TRUE,
		      TRUE, 0);

#define NEWPAGE(thelabel) \
    main_vbox = gtk_vbox_new (FALSE, 1);\
    gtk_notebook_append_page (GTK_NOTEBOOK (notebook), main_vbox, NULL);\
    gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (notebook), main_vbox,\
                                                           _(thelabel));

#define BOOLEANENTRY(thelabel, field) \
  GtkWidget *field =\
    gtk_check_button_new_with_label (thelabel); \
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (field),\
				(gboolean)Denemo.prefs.field);\
  gtk_box_pack_start (GTK_BOX (main_vbox), field, FALSE, TRUE, 0);\
  cbdata.field = field;

#define TEXTENTRY(thelabel, field) \
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (_(thelabel));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  GtkWidget *field = gtk_entry_new ();\
  gtk_entry_set_text (GTK_ENTRY (field), Denemo.prefs.field->str);\
  gtk_box_pack_start (GTK_BOX (hbox), field, TRUE, TRUE, 0);\
  cbdata.field = field;

#define PASSWORDENTRY(thelabel, field) \
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (_(thelabel));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  GtkWidget *field = gtk_entry_new ();\
  gtk_entry_set_visibility(GTK_ENTRY(field), FALSE);\
  gtk_entry_set_invisible_char(GTK_ENTRY(field), '*');\
  gtk_entry_set_text (GTK_ENTRY (field), Denemo.prefs.field->str);\
  gtk_box_pack_start (GTK_BOX (hbox), field, TRUE, TRUE, 0);\
  cbdata.field = field;

#define INTENTRY(thelabel, field) \
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (thelabel);\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  field = gtk_spin_button_new_with_range (1, 50, 1.0);\
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (field), Denemo.prefs.field);\
  gtk_box_pack_start (GTK_BOX (hbox), field, FALSE, FALSE, 0);\
  cbdata.field = field;

#define INTENTRY_LIMITS(thelabel, field, min, max) \
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (thelabel);\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  GtkWidget *field = gtk_spin_button_new_with_range (min, max, 1.0);\
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (field), Denemo.prefs.field);\
  gtk_box_pack_start (GTK_BOX (hbox), field, FALSE, FALSE, 0);\
  cbdata.field = field;

  /*
   * Note entry settings
   */
  
  NEWPAGE("View");
  
  BOOLEANENTRY("Play back entered notes immediately", immediateplayback);  
  BOOLEANENTRY("Display Note/Rest entry toolbar", notation_palette);
  BOOLEANENTRY("Display articulation palette", articulation_palette);
  BOOLEANENTRY("Display Titles. Controls etc", visible_directive_buttons);

  BOOLEANENTRY("Display rhythm pattern toolbar", rhythm_palette);
  BOOLEANENTRY("Display menu of objects toolbar", object_palette);


  hbox = gtk_hbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);
  autosave = gtk_check_button_new_with_label (_("Autosave every"));
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (autosave),
				Denemo.prefs.autosave);
  gtk_box_pack_start (GTK_BOX (hbox), autosave, FALSE, FALSE, 0);

  autosave_timeout = gtk_spin_button_new_with_range (1, 50, 1.0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (autosave_timeout),
			     Denemo.prefs.autosave_timeout);
  gtk_widget_set_sensitive (autosave_timeout, Denemo.prefs.autosave);
  gtk_box_pack_start (GTK_BOX (hbox), autosave_timeout, FALSE, FALSE, 0);
  g_debug("autosave %p\n", autosave);
  label = gtk_label_new (_("minute(s)"));
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);
  g_signal_connect (GTK_OBJECT (autosave),
		    "toggled", G_CALLBACK (toggle_autosave), autosave_timeout);


  BOOLEANENTRY("Autosave Parts", saveparts);
  TEXTENTRY("Sequencer Device", sequencer)
  TEXTENTRY("Midi Input Device", midi_in)


  /*
   * Pitch Entry Parameters 
   */
  NEWPAGE("Pitch Entry");

  TEXTENTRY("Temperament", temperament)
  BOOLEANENTRY("Use Overlays", overlays);
  BOOLEANENTRY("Continuous Entry", continuous);

  /*
   * Shortcut control 
   */
  NEWPAGE("Shortcuts");

  //  TEXTENTRY("Strict", strictshortcuts)
  BOOLEANENTRY("Strict Shortcuts", strictshortcuts);

  /*
   * External (Helper) Programs 
   */
  NEWPAGE("Externals");
 
  TEXTENTRY("Path to Lilypond", lilypath)
  TEXTENTRY("Pdf Viewer", pdfviewer)
  TEXTENTRY("File/Internet Browser", browser)

  TEXTENTRY("Image Viewer", imageviewer)
  TEXTENTRY("Text Editor", texteditor)
  TEXTENTRY("Midi Player", midiplayer)

  TEXTENTRY("Default Save Path", denemopath)
  BOOLEANENTRY("Update the command set on startup", autoupdate);
   /*
   * Misc Menu 
   */
  NEWPAGE("Misc");
 
  INTENTRY_LIMITS(_("Excerpt Resolution"), resolution, 72, 600);

  INTENTRY(_("Max recent files"), maxhistory)
  TEXTENTRY("User Name", username)
  PASSWORDENTRY("Password for Denemo.org", password)
  /*
   * Jack Menu
   */
#ifdef _HAVE_JACK_
  NEWPAGE("JACK");
  BOOLEANENTRY("Enable Jack Transport", jacktransport);
  BOOLEANENTRY("Jack Transport starts stopped", jacktransport_start_stopped);
  BOOLEANENTRY("Enable Jack at startup", jack_at_startup);
  /* Start/Restart Button */
  hbox = gtk_hbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);
  GtkWidget *jack_activate = gtk_button_new_with_label("Start/Restart Jack Client");
  gtk_box_pack_start (GTK_BOX (hbox), jack_activate, FALSE, FALSE, 0);
  g_signal_connect (G_OBJECT (jack_activate), "clicked",
  G_CALLBACK (jack_start_restart), (gpointer) NULL);
#endif
  /*
   * Fluidsynth Menu
   */
#ifdef _HAVE_FLUIDSYNTH_
  NEWPAGE("FLUIDSYNTH");
  /* Start/Restart Button */
  hbox = gtk_hbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);
  GtkWidget *fluidsynth_activate = gtk_button_new_with_label("Start/Restart FLUIDSYNTH");
  gtk_box_pack_start (GTK_BOX (hbox), fluidsynth_activate, FALSE, FALSE, 0);
  g_signal_connect (G_OBJECT (fluidsynth_activate), "clicked",
  G_CALLBACK (fluidsynth_start_restart), (gpointer) NULL);

  TEXTENTRY("Audio Driver", fluidsynth_audio_driver) 
#endif


  /*
   * Help settings
   */
  //  NEWPAGE("Help Settings")

  /* Set up the callback data */

#define SETCALLBACKDATA(field) \
  cbdata.field = field;
  
  cbdata.prefs = &Denemo.prefs;
  SETCALLBACKDATA(autosave);
  SETCALLBACKDATA(autosave_timeout); 
  SETCALLBACKDATA(maxhistory);
  
  
  //gtk_widget_grab_focus (lilypath);
  gtk_widget_show_all (dialog);
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      set_preferences (&cbdata);
    }
  gtk_widget_destroy (dialog);
}


