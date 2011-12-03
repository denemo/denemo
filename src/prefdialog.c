/* prefdialog.c
 * functions for a preferences dialog
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller, Adam Tee, 2011 Richard Shann */


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
#include "playback.h"
#include "jackmidi.h"
#include "fluid.h"
#include "device_manager.h"

struct callbackdata
{
  DenemoPrefs *prefs;
  GtkWidget *lilypath;
  GtkWidget *midi_audio_output;
  GtkWidget *immediateplayback;
  GtkWidget *pitchspellingchannel;
  GtkWidget *pitchspellingprogram;
  GtkWidget *modal;
  GtkWidget *cursor_highlight;
  GtkWidget *persistence;
  GtkWidget *startmidiin;
  GtkWidget *applytoselection;
  GtkWidget *quickshortcuts;
  GtkWidget *saveparts;
  GtkWidget *autosave;
  GtkWidget *toolbar;
  GtkWidget *notation_palette;
  GtkWidget *rhythm_palette;
  GtkWidget *object_palette;
  GtkWidget *articulation_palette;
  GtkWidget *midi_in_controls;
  GtkWidget *playback_controls;

  GtkWidget *console_pane;
  GtkWidget *lyrics_pane;
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
  GtkWidget *dynamic_compression;

  GtkWidget *zoom;
  GtkWidget *system_height;
  GtkWidget *progressbardecorations;
#ifdef _HAVE_JACK_
  GtkWidget *jacktransport;
  GtkWidget *jacktransport_start_stopped;
#endif
#ifdef _HAVE_FLUIDSYNTH_
  GtkWidget *fluidsynth_audio_driver;
  GtkWidget *fluidsynth_midi_driver;
  GtkWidget *fluidsynth_soundfont;
  GtkWidget *fluidsynth_reverb;
  GtkWidget *fluidsynth_chorus;
  GtkWidget *fluidsynth_sample_rate;
  GtkWidget *fluidsynth_period_size;

#endif
  GtkWidget *display_refresh;
  GtkWidget *animation_steps;
  GtkWidget *profile;
  GtkWidget *midiplayer;
  GtkWidget *audioplayer;
  GtkWidget *fontspec;
  GtkWidget *denemopath;
  GtkWidget *temperament;
  GtkWidget *strictshortcuts;
  GtkWidget *resolution;
  GtkWidget *overlays;
  GtkWidget *enable_thumbnails;
  GtkWidget *continuous;
};

struct audio_callback_data
{
  GtkWidget *main_vbox;
  GtkWidget *fs;
  GtkWidget *DM;
  GtkWidget *pas;
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

#define ASSIGNDOUBLE(field) \
   prefs->field =\
     gtk_spin_button_get_value (GTK_SPIN_BUTTON(cbdata->field));

#if GTK_MAJOR_VERSION==3
 #define ASSIGNCOMBO(field) \
   g_string_assign (prefs->field,\
		   (gchar *) gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT(cbdata->field)));

 #define ASSIGNCOMBO2(field) \
   prefs->field = get_midi_audio_pointer(\
		   (gchar *) gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT(cbdata->field)));
#else
#define ASSIGNCOMBO(field) \
  g_string_assign (prefs->field,\
    (gchar *) gtk_entry_get_text (GTK_ENTRY (GTK_COMBO (cbdata->field)->entry)));

#define ASSIGNCOMBO2(field) \
  prefs->field = get_midi_audio_pointer(\
    (gchar *) gtk_entry_get_text (GTK_ENTRY (GTK_COMBO (cbdata->field)->entry)));
#endif
  ASSIGNTEXT(lilypath)
  ASSIGNTEXT(browser) 
  ASSIGNTEXT(pdfviewer)
  ASSIGNTEXT(imageviewer)
  ASSIGNTEXT(username)
  ASSIGNTEXT(password)
  ASSIGNTEXT(profile)

  ASSIGNTEXT(fontspec)
  ASSIGNTEXT(denemopath)
    // ASSIGNTEXT(sequencer)
#ifndef _HAVE_FLUIDSYNTH_
#ifndef G_OS_WIN32
  ASSIGNTEXT(midi_in)
#endif
#endif
#ifdef _HAVE_JACK_
    // ASSIGNBOOLEAN(jacktransport)
    //  ASSIGNBOOLEAN(jacktransport_start_stopped)
#endif
#ifdef _HAVE_FLUIDSYNTH_
  /*TODO save combo as int????*/
  ASSIGNCOMBO(fluidsynth_audio_driver);
  ASSIGNCOMBO(fluidsynth_midi_driver);
  ASSIGNTEXT(fluidsynth_soundfont);
  ASSIGNBOOLEAN(fluidsynth_reverb)
  ASSIGNBOOLEAN(fluidsynth_chorus)
    ASSIGNINT(fluidsynth_sample_rate)
    ASSIGNINT(fluidsynth_period_size)
#endif
  ASSIGNDOUBLE(display_refresh)
  ASSIGNINT(animation_steps)
  ASSIGNTEXT(temperament)
  ASSIGNBOOLEAN(strictshortcuts)
  ASSIGNBOOLEAN(overlays)
  ASSIGNBOOLEAN(enable_thumbnails)
  ASSIGNBOOLEAN(continuous)
  ASSIGNINT(resolution)
  ASSIGNINT(maxhistory)
  ASSIGNINT(dynamic_compression)
  ASSIGNINT(zoom)
  ASSIGNINT(system_height)
  ASSIGNCOMBO2(midi_audio_output)
  ASSIGNBOOLEAN(progressbardecorations)
  ASSIGNBOOLEAN(immediateplayback)
  ASSIGNINT(pitchspellingchannel)
  ASSIGNINT(pitchspellingprogram)
  ASSIGNBOOLEAN(modal)
  ASSIGNBOOLEAN(persistence)
  ASSIGNBOOLEAN(cursor_highlight)
  ASSIGNBOOLEAN(startmidiin)
  ASSIGNBOOLEAN(applytoselection)
  ASSIGNBOOLEAN(quickshortcuts)
  ASSIGNBOOLEAN(autosave)
  ASSIGNINT(autosave_timeout)
  ASSIGNBOOLEAN(articulation_palette)
  ASSIGNBOOLEAN(midi_in_controls)
  ASSIGNBOOLEAN(playback_controls)
  ASSIGNBOOLEAN(console_pane)
  ASSIGNBOOLEAN(lyrics_pane)
  ASSIGNBOOLEAN(visible_directive_buttons)
  ASSIGNBOOLEAN(autoupdate)
  ASSIGNBOOLEAN(toolbar)
  ASSIGNBOOLEAN(notation_palette)
  ASSIGNBOOLEAN(rhythm_palette)
  ASSIGNBOOLEAN(object_palette)
  ASSIGNBOOLEAN(saveparts)
  //g_print ("Timeout %d \n", prefs->autosave_timeout);

  /* Now write it all to denemorc */
  writeXMLPrefs (prefs);
}

static void
midi_audio_tab_update(GtkWidget *box, gpointer data)
{
  struct audio_callback_data *cbdata = (struct audio_callback_data *) data;
#if GTK_MAJOR_VERSION==3
  gchar *output = get_midi_audio_pointer((gchar *)gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT(box)));
#else
  gchar *output = (gchar *)get_midi_audio_pointer((gchar *) 
		  gtk_entry_get_text (GTK_ENTRY (GTK_COMBO (box)->entry)));
#endif 
  if (output == Fluidsynth){
    gtk_widget_hide(cbdata->pas);
    if(cbdata->DM)
      gtk_widget_hide(cbdata->DM);
    gtk_widget_show(cbdata->fs);
  }
  else if (output == Jack){
    gtk_widget_hide(cbdata->pas);
    gtk_widget_hide(cbdata->fs);
    if(cbdata->DM)
      gtk_widget_show(cbdata->DM);
  }
  else if (output == Portaudio){
    gtk_widget_hide(cbdata->fs);
    if(cbdata->DM)
      gtk_widget_hide(cbdata->DM);
    gtk_widget_show(cbdata->pas);
  }
  else if (output == None){
    gtk_widget_hide(cbdata->fs);
    if(cbdata->DM)
      gtk_widget_hide(cbdata->DM);
  }

}

static gint
find_element_position(gchar **haystack, gchar *needle)
{
  gint i;
  for(i=0;i<G_N_ELEMENTS(haystack);i++)
    if (g_strcmp0(haystack[i], needle) == 0)
      return i;
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
  GtkWidget *fs;
  GtkWidget *DM; 
  GtkWidget *content_area;
  gint i;
  static struct callbackdata cbdata;
  g_assert (gui != NULL);

  dialog = gtk_dialog_new_with_buttons (_("Preferences - Denemo"),
					GTK_WINDOW (Denemo.window),
					(GtkDialogFlags) (GTK_DIALOG_MODAL |
							  GTK_DIALOG_DESTROY_WITH_PARENT),
					GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
					GTK_STOCK_CANCEL, GTK_STOCK_CANCEL,
					NULL);

  //gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);
  content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  notebook = gtk_notebook_new ();
  gtk_container_add (GTK_CONTAINER (content_area), notebook);

#define VBOX main_vbox

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
  gtk_box_pack_start (GTK_BOX (VBOX), field, FALSE, TRUE, 0);\
  cbdata.field = field;

#define TEXTENTRY(thelabel, field) \
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (VBOX), hbox, FALSE, TRUE, 0);\
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


#define ENTRY_LIMITS(thelabel, field, min, max, step)	\
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (VBOX), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (thelabel);\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  GtkWidget *field = gtk_spin_button_new_with_range (min, max, step);\
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (field), Denemo.prefs.field);\
  gtk_box_pack_start (GTK_BOX (hbox), field, FALSE, FALSE, 0);\
  cbdata.field = field;



#define INTENTRY_LIMITS(thelabel, field, min, max) ENTRY_LIMITS(thelabel, field, min, max, 0.1)

#define DOUBLEENTRY_LIMITS  ENTRY_LIMITS


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
  BOOLEANENTRY("Highlight the cursor", cursor_highlight); 
  //Doesnt GList need to be freed
#if 0
  GList *output_option_list = NULL;
  output_option_list = g_list_append (output_option_list, (gpointer) None);
#ifdef _HAVE_PORTAUDIO_
  output_option_list = g_list_append (output_option_list, (gpointer) Portaudio);
#endif
#ifdef _HAVE_JACK_
  output_option_list = g_list_append (output_option_list, (gpointer) Jack);
#endif 
#ifdef _HAVE_FLUIDSYNTH_
  output_option_list = g_list_append (output_option_list, (gpointer) Fluidsynth);
#endif
#endif
 
  gchar *output_option_list[4] = {"None", 
#ifdef _HAVE_PORTAUDIO_
  "Portaudio",
#else
  NULL,
#endif
#ifdef _HAVE_JACK_
  "Jack",
#else
  NULL,
#endif 
#ifdef _HAVE_FLUIDSYNTH_
  "Fluidsynth"
#else
  NULL
#endif
};

#if GTK_MAJOR_VERSION==3
 #define COMBOBOX(thelable, field, thelist, settext)\
  label = gtk_label_new (thelable);\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_container_add(GTK_CONTAINER(VBOX), label);\
    GtkWidget *field = gtk_combo_box_text_new ();\
    for(i=0;i<G_N_ELEMENTS(thelist);i++)\
      if (thelist[i]) gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT(field), thelist[i]);\
  gtk_container_add(GTK_CONTAINER(VBOX), field);\
  gtk_widget_show (field);\
  cbdata.field = field;
#else
 #define COMBOBOX(thelable, field, thelist, settext)\
  label = gtk_label_new (thelable);\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_container_add(GTK_CONTAINER(VBOX), label);\
  GtkWidget *field = gtk_combo_new();\
  gtk_combo_set_popdown_strings (GTK_COMBO (field), thelist);\
  gtk_entry_set_text\
    (GTK_ENTRY (GTK_COMBO (field)->entry),  settext);\
  gtk_container_add(GTK_CONTAINER(VBOX), field);\
  gtk_widget_show (field);\
  cbdata.field = field;
#endif

  BOOLEANENTRY("Display general toolbar", toolbar);
  BOOLEANENTRY("Display Note/Rest entry toolbar", notation_palette);
  BOOLEANENTRY("Display articulation palette", articulation_palette);
  BOOLEANENTRY("Display Controls for Incoming MIDI signals", midi_in_controls);
  BOOLEANENTRY("Display Controls for Playback", playback_controls);

  BOOLEANENTRY("Display console pane", console_pane);
  BOOLEANENTRY("Display lyrics pane", lyrics_pane);
  BOOLEANENTRY("Display Titles. Controls etc", visible_directive_buttons);

  BOOLEANENTRY("Display Music Snippets", rhythm_palette);
  BOOLEANENTRY("Display menu of objects toolbar", object_palette);
  INTENTRY_LIMITS(_("% Zoom"), zoom, 1, 100);
  INTENTRY_LIMITS(_("% of display height per system"), system_height, 1, 100);
  BOOLEANENTRY("Display progressbar decorations", progressbardecorations);

  /*
   * Pitch Entry Parameters 
   */
  NEWPAGE("Pitch Entry");
  
  TEXTENTRY("Temperament", temperament)
  BOOLEANENTRY("Use Overlays", overlays);
  BOOLEANENTRY("Continuous Entry", continuous);

  /*
   * Preferences to do with commands
   */
  NEWPAGE("Command Behavior");
  TEXTENTRY("Profile", profile)
  //  TEXTENTRY("Strict", strictshortcuts)
  BOOLEANENTRY("Apply commands to selection if present", applytoselection); 
  BOOLEANENTRY("Allow Quick Setting of Shortcuts", quickshortcuts); 

 
  BOOLEANENTRY("Strict Shortcuts", strictshortcuts);

  /*
   * External (Helper) Programs 
   */
  NEWPAGE("Externals");
 
  TEXTENTRY("Path to Lilypond", lilypath)
  TEXTENTRY("Pdf Viewer", pdfviewer)
  TEXTENTRY("File/Internet Browser", browser)

  TEXTENTRY("Image Viewer", imageviewer)

  TEXTENTRY("Audio Player", audioplayer)
  TEXTENTRY("Default Font Specification", fontspec)

  TEXTENTRY("Default Save Path", denemopath)
  BOOLEANENTRY("Update the command set on startup", autoupdate);
   /*
   * Misc Menu 
   */
  NEWPAGE("Misc");
  BOOLEANENTRY("Use Denemo modally", modal);   
  BOOLEANENTRY("Re-use last settings on startup", persistence);   
  DOUBLEENTRY_LIMITS(_("Playback Display Refresh"), display_refresh, 0.001, 0.5, 0.002);
  INTENTRY_LIMITS(_("Page Turn Steps"), animation_steps, 1, 200);

  INTENTRY_LIMITS(_("Excerpt Resolution"), resolution, 72, 600);
  BOOLEANENTRY("Enable Thumbnails", enable_thumbnails);
  INTENTRY(_("Max recent files"), maxhistory)
  TEXTENTRY("User Name", username)
  PASSWORDENTRY("Password for Denemo.org", password)


  autosave = gtk_check_button_new_with_label (_("Autosave every"));
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (autosave),
				Denemo.prefs.autosave);
  gtk_box_pack_start (GTK_BOX (main_vbox), autosave, FALSE, FALSE, 0);

  autosave_timeout = gtk_spin_button_new_with_range (1, 50, 1.0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (autosave_timeout),
			     Denemo.prefs.autosave_timeout);
  gtk_widget_set_sensitive (autosave_timeout, Denemo.prefs.autosave);
  gtk_box_pack_start (GTK_BOX (main_vbox), autosave_timeout, FALSE, FALSE, 0);
  g_debug("autosave %p\n", autosave);
  label = gtk_label_new (_("minute(s)"));
  gtk_box_pack_start (GTK_BOX (main_vbox), label, FALSE, FALSE, 0);
  g_signal_connect (G_OBJECT (autosave),
		    "toggled", G_CALLBACK (toggle_autosave), autosave_timeout);


  BOOLEANENTRY("Autosave Parts", saveparts);

  static struct audio_callback_data audio_cbdata;
  NEWPAGE("Audio/MIDI")
 
  {
    label = gtk_label_new ("");	
    gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
    gtk_label_set_markup(GTK_LABEL (label), _("<span background=\"#FFA0A0\">Warning: changes only have effect after quitting and re-starting Denemo</span>"));
    gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
    gtk_box_pack_start (GTK_BOX (main_vbox), label, FALSE, FALSE, 0);
  }

  BOOLEANENTRY("Play back entered notes immediately", immediateplayback);
  INTENTRY_LIMITS(_("Pitch Spelling Channel"), pitchspellingchannel, 0, 15);
  INTENTRY_LIMITS(_("Pitch Spelling Program"), pitchspellingprogram, 0, 127);
    
  BOOLEANENTRY("Auto-start midi in", startmidiin);
  INTENTRY_LIMITS(_("% MIDI-in Dynamic Compression"), dynamic_compression, 1, 100);
#if GTK_MAJOR_VERSION==3
  COMBOBOX("Midi/Audio output", midi_audio_output, output_option_list, Denemo.prefs.midi_audio_output)
  g_signal_connect(G_OBJECT(GTK_COMBO_BOX_TEXT(midi_audio_output)), "changed",
		   G_CALLBACK( G_CALLBACK(midi_audio_tab_update) ), &audio_cbdata);
#else
  GList *option_list = NULL;
  for(i=0;i<G_N_ELEMENTS(output_option_list);i++)
    if (output_option_list[i]) option_list = g_list_append(option_list, output_option_list[i]);
  COMBOBOX("Midi/Audio output", midi_audio_output, option_list, Denemo.prefs.midi_audio_output)
  g_signal_connect(G_OBJECT(GTK_COMBO(midi_audio_output)), "changed",
		   G_CALLBACK( G_CALLBACK(midi_audio_tab_update) ), &audio_cbdata);
#endif


  /*
   * Fluidsynth Menu
   */
#ifdef _HAVE_FLUIDSYNTH_
#undef VBOX
#define VBOX fs

  /* Start/Restart Button */
  //BUTTON("Start/Restart FLUIDSYNTH", fluid_restart, fluidsynth_start_restart, NULL)

  /*TODO ifdef differnet os's and support
   *jack, alsa, oss, pulseaudio, coreaudio, dsound, portaudio, sndman, dart, file 
   *defaults are:
   *jack (Linux), dsound (Windows), sndman (MacOS9), coreaudio (Mac OS X), dart (OS/2) 
   */
  fs = gtk_vbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (main_vbox), fs, FALSE, TRUE, 0);
  //gtk_widget_show(fs);
#ifdef G_OS_WIN32
  gchar *driver_options[5] = {"portaudio", "jack"}; //Isn't there more options?
  gchar *midi_driver_options[1] = {"portaudio"}; //Is this correct?
#else
  gchar *driver_options[5] = {"alsa", "jack", "oss", "pulseaudio", "portaudio"};
  gchar *midi_driver_options[2] = {"alsa_seq", "oss"};
#endif
  //GList *driver_option_list = NULL;
  //GList *midi_driver_option_list = NULL;
  //gint i;
  //for (i=0;i<G_N_ELEMENTS(driver_options);i++)
  //  driver_option_list = g_list_append (driver_option_list, driver_options[i]);
  //for (i=0;i<G_N_ELEMENTS(midi_driver_options);i++)
  //  midi_driver_option_list = g_list_append (midi_driver_option_list, midi_driver_options[i]);
#if GTK_MAJOR_VERSION==3 
  COMBOBOX("Audio Driver", fluidsynth_audio_driver, driver_options, Denemo.prefs.fluidsynth_audio_driver->str)
  COMBOBOX("Midi Driver", fluidsynth_midi_driver, midi_driver_options, Denemo.prefs.fluidsynth_midi_driver->str)	  
#else
  GList *driver_option_list = NULL;
  GList *midi_driver_option_list = NULL;
  for (i=0;i<G_N_ELEMENTS(driver_options);i++)
    driver_option_list = g_list_append 
	(driver_option_list, driver_options[i]);
  for (i=0;i<G_N_ELEMENTS(midi_driver_options);i++)
    midi_driver_option_list = g_list_append 
	(midi_driver_option_list, midi_driver_options[i]);
  COMBOBOX("Audio Driver", fluidsynth_audio_driver, driver_option_list, Denemo.prefs.fluidsynth_audio_driver->str)
  COMBOBOX("Midi Driver", fluidsynth_midi_driver, midi_driver_option_list, Denemo.prefs.fluidsynth_midi_driver->str)
#endif

  TEXTENTRY("Soundfont", fluidsynth_soundfont)	
  
  hbox = gtk_hbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (VBOX), hbox, FALSE, TRUE, 0);
  GtkWidget *button = gtk_button_new_with_label (_("Choose Soundfont"));
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, FALSE, 0);
  g_signal_connect (G_OBJECT (button), "clicked",
    G_CALLBACK(choose_sound_font), fluidsynth_soundfont);
  gtk_widget_show (button);

  BOOLEANENTRY("Enable Reverb on soundfont", fluidsynth_reverb)
  BOOLEANENTRY("Enable Chorus on soundfont", fluidsynth_chorus)
    INTENTRY_LIMITS(_("Sample Rate"), fluidsynth_sample_rate, 0, 96000);
   INTENTRY_LIMITS(_("Period Size"), fluidsynth_period_size, 0, 2048);
#undef VBOX
#endif

#ifdef _HAVE_JACK_
  DM = DeviceManager();
  gtk_box_pack_start (GTK_BOX (main_vbox), DM, FALSE, TRUE, 0);
  //gtk_widget_show(DM);
#else
  DM = NULL;
#endif
#define VBOX pas
  GtkWidget *pas;
  pas = gtk_vbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (main_vbox), pas, FALSE, TRUE, 0);
  //TEXTENTRY("Sequencer Device", sequencer)
#ifndef G_OS_WIN32
  TEXTENTRY("Midi Input Device", midi_in) 
#endif
#undef VBOX

  gtk_widget_show_all (dialog);

  if (Denemo.prefs.midi_audio_output == Fluidsynth){
    gtk_widget_hide(pas);
    if(DM)
      gtk_widget_hide(DM);
    gtk_widget_show(fs);
  }
  else if (Denemo.prefs.midi_audio_output == Jack){
    gtk_widget_hide(pas);
    gtk_widget_hide(fs);
    if(DM)
      gtk_widget_show(DM);
  }
  else if (Denemo.prefs.midi_audio_output == Portaudio){
    gtk_widget_hide(fs);
    if(DM)
      gtk_widget_hide(DM);
    gtk_widget_show(pas);
  }
  else if (Denemo.prefs.midi_audio_output == None){
    gtk_widget_hide(fs);
    gtk_widget_hide(pas);
    if(DM)
      gtk_widget_hide(DM);
  }

  audio_cbdata.main_vbox = main_vbox;
  audio_cbdata.fs = fs;
  audio_cbdata.DM = DM;
  audio_cbdata.pas = pas;

#define SETCALLBACKDATA(field) \
  cbdata.field = field;
  
  cbdata.prefs = &Denemo.prefs;
  SETCALLBACKDATA(autosave);
  SETCALLBACKDATA(autosave_timeout); 
  SETCALLBACKDATA(maxhistory);
  
  
  //gtk_widget_grab_focus (lilypath);
  
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      set_preferences (&cbdata);
    }
#ifdef _HAVE_JACK_
  //gtk_container_remove(GTK_CONTAINER(vbox2), view);
#endif
  gtk_widget_destroy (dialog);
}

