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
#include "jackmidi.h"
#include "fluid.h"
#include "device_manager.h"

gchar *output_options[] = {"Portaudio", "Jack"
#ifdef _HAVE_FLUIDSYNTH_
, "Fluidsynth"
#endif
};

gint FindStringIndex(gchar *output_selection){
  gint i;
  for (i=0;i<G_N_ELEMENTS(output_options);i++) 
    if (g_strcmp0(output_selection, output_options[i]) == 0)
      return i;
}


struct callbackdata
{
  DenemoPrefs *prefs;
  GtkWidget *lilypath;
  GtkWidget *midi_audio_output;
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
  GtkWidget *fluidsynth_soundfont;
  GtkWidget *fluidsynth_reverb;
  GtkWidget *fluidsynth_chorus;
  GtkWidget *fluidsynth_sample_rate;
  GtkWidget *fluidsynth_period_size;

#endif
  GtkWidget *texteditor;
  GtkWidget *midiplayer;
  GtkWidget *audioplayer;
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

#define ASSIGNCOMBO(field) \
  g_string_assign (prefs->field,\
    (gchar *) gtk_entry_get_text (GTK_ENTRY (GTK_COMBO (cbdata->field)->entry)));

  ASSIGNTEXT(lilypath)
  ASSIGNTEXT(browser) 
  ASSIGNTEXT(pdfviewer)
  ASSIGNTEXT(imageviewer)
  ASSIGNTEXT(username)
  ASSIGNTEXT(password)
  ASSIGNTEXT(texteditor)
  ASSIGNTEXT(midiplayer)
  ASSIGNTEXT(audioplayer)
  ASSIGNTEXT(denemopath)
  ASSIGNTEXT(sequencer)
  ASSIGNTEXT(midi_in)
  ASSIGNTEXT(sequencer)
  ASSIGNTEXT(midi_in)
#ifdef _HAVE_JACK_
    // ASSIGNBOOLEAN(jacktransport)
    //  ASSIGNBOOLEAN(jacktransport_start_stopped)
    //  ASSIGNBOOLEAN(jack_at_startup)
#endif
#ifdef _HAVE_FLUIDSYNTH_
  /*TODO save combo as int????*/
  ASSIGNCOMBO(fluidsynth_audio_driver);
  ASSIGNTEXT(fluidsynth_soundfont);
  ASSIGNBOOLEAN(fluidsynth_reverb)
  ASSIGNBOOLEAN(fluidsynth_chorus)
    ASSIGNINT(fluidsynth_sample_rate)
    ASSIGNINT(fluidsynth_period_size)
#endif
  ASSIGNTEXT(temperament)
  ASSIGNBOOLEAN(strictshortcuts)
  ASSIGNBOOLEAN(overlays)
  ASSIGNBOOLEAN(continuous)
  ASSIGNINT(resolution)
  ASSIGNINT(maxhistory)


  gchar *AudioMidiOut =
    (gchar *) gtk_entry_get_text (GTK_ENTRY (GTK_COMBO (cbdata->midi_audio_output)->entry));
  
  prefs->midi_audio_output = FindStringIndex(AudioMidiOut);

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

#define BUTTON(thelabel, field, thecallback, data) \
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (vbox1), hbox, FALSE, FALSE, 0);\
  GtkWidget *field = gtk_button_new_with_label(thelabel);\
  gtk_box_pack_start (GTK_BOX (vbox1), field, FALSE, FALSE, 0);\
  g_signal_connect (G_OBJECT (field), "clicked",\
  G_CALLBACK (thecallback), (gpointer) data);

  /*
   * Note entry settings
   */
  
  NEWPAGE("View");
  
  //Doesnt GList need to be freed
  GList *output_option_list = NULL;
  int i;
  for (i=0;i<G_N_ELEMENTS(output_options);i++)
    output_option_list = g_list_append (output_option_list, output_options[i]);

#define COMBOBOX(thelable, field, thelist, settext)\
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (thelable);\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);\
  GtkWidget *field = gtk_combo_new ();\
  gtk_combo_set_popdown_strings (GTK_COMBO (field), thelist);\
  gtk_box_pack_start (GTK_BOX (hbox), field, FALSE, FALSE, 0);\
  gtk_entry_set_text\
    (GTK_ENTRY (GTK_COMBO (field)->entry), settext);\
  gtk_widget_show (field);\
  cbdata.field = field;

  COMBOBOX("Midi/Audio output", midi_audio_output, output_option_list, output_options[Denemo.prefs.midi_audio_output]) 
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
  TEXTENTRY("Audio Player", audioplayer)

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
   * Fluidsynth Menu
   */
#ifdef _HAVE_FLUIDSYNTH_
  NEWPAGE("Fluidsynth");
  /* Start/Restart Button */
  //BUTTON("Start/Restart FLUIDSYNTH", fluid_restart, fluidsynth_start_restart, NULL)

  /*TODO ifdef differnet os's and support
   *jack, alsa, oss, pulseaudio, coreaudio, dsound, portaudio, sndman, dart, file 
   *defaults are:
   *jack (Linux), dsound (Windows), sndman (MacOS9), coreaudio (Mac OS X), dart (OS/2) 
   */
#ifdef G_OS_WIN32
  gchar *driver_options[5] = {"portaudio", "jack"};
#else
  gchar *driver_options[5] = {"alsa", "jack", "oss", "pulseaudio", "portaudio"};
#endif
  GList *driver_option_list = NULL;
  for (i=0;i<G_N_ELEMENTS(driver_options);i++)
    driver_option_list = g_list_append (driver_option_list, driver_options[i]);

  COMBOBOX("Audio Driver", fluidsynth_audio_driver, driver_option_list, Denemo.prefs.fluidsynth_audio_driver->str)
  TEXTENTRY("Soundfont", fluidsynth_soundfont)	
  
  hbox = gtk_hbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);
  GtkWidget *button = gtk_button_new_with_label (_("Choose Soundfont"));
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, FALSE, 0);
  gtk_signal_connect (GTK_OBJECT (button), "clicked",
    GTK_SIGNAL_FUNC (choose_sound_font), fluidsynth_soundfont);
  gtk_widget_show (button);

  BOOLEANENTRY("Enable Reverb on soundfont", fluidsynth_reverb)
  BOOLEANENTRY("Enable Chorus on soundfont", fluidsynth_chorus)
    INTENTRY_LIMITS(_("Sample Rate"), fluidsynth_sample_rate, 0, 96000);
   INTENTRY_LIMITS(_("Period Size"), fluidsynth_period_size, 0, 2048);

#endif
  /**
   * Device Manager
   */
#ifdef _HAVE_JACK_
  NEWPAGE("MIDI Device Manager");
  
  GtkWidget *mhbox = gtk_hbox_new(FALSE, 5);
  gtk_box_pack_start (GTK_BOX (main_vbox), mhbox, FALSE, FALSE, 0);

  GtkWidget *vbox1 = gtk_vbox_new(FALSE, 5);
  gtk_box_pack_start (GTK_BOX (mhbox), vbox1, FALSE, FALSE, 0);
  
  //None of this is working right now anyway  
  //BOOLEANENTRY("Enable Jack Transport", jacktransport);
  //BOOLEANENTRY("Jack Transport starts stopped", jacktransport_start_stopped);
  //BOOLEANENTRY("Enable Jack at startup", jack_at_startup);

  BUTTON("Start/Restart Jack Client", jack_restart, jack_start_restart, NULL);
  BUTTON("Add Device", midi_add_device, device_manager_create_device, NULL); 
  BUTTON("Remove Device", midi_remove_device, device_manager_remove_device, NULL); 
  BUTTON("Add Port", midi_device_add_port, device_manager_create_port, NULL);
  BUTTON("Remove Port", midi_device_remove_port, device_manager_remove_port, NULL);
  
  GtkWidget *vbox2 = gtk_vbox_new(FALSE, 5);
  gtk_box_pack_start (GTK_BOX (mhbox), vbox2, FALSE, FALSE, 0);
  
  GtkWidget *view = DeviceManager();
  gtk_box_pack_start (GTK_BOX (vbox2), view, FALSE, FALSE, 0);
  
  gtk_widget_show (view);
  device_manager_refresh_model();
#endif

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

