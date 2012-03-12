/* prefdialog.c
 * functions for a preferences dialog
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller, Adam Tee
 * (c) 2011 Richard Shann, Dominic Sacré
 */

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
#include "fluid.h"

#ifdef _HAVE_JACK_
#include "jackutil.h"
#endif
#ifdef _HAVE_PORTAUDIO_
#include "portaudioutil.h"
#endif
#ifdef _HAVE_PORTMIDI_
#include "portmidiutil.h"
#endif


#if GTK_MAJOR_VERSION==2
 #define gtk_combo_box_text_new_with_entry gtk_combo_box_new_text
 #define gtk_combo_box_text_append_text gtk_combo_box_append_text
 #define gtk_combo_box_text_get_active_text gtk_combo_box_get_active_text
 #define GTK_COMBO_BOX_TEXT GTK_COMBO_BOX
#endif

struct callbackdata
{
  DenemoPrefs *prefs;
  GtkWidget *lilypath;
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
  GtkWidget *dynamic_compression;

  GtkWidget *zoom;
  GtkWidget *system_height;


  GtkWidget *audio_driver;
  GtkWidget *midi_driver;

#ifdef _HAVE_JACK_
  GtkWidget *jacktransport;
  GtkWidget *jacktransport_start_stopped;
  GtkWidget *jack_connect_ports_l;
  GtkWidget *jack_connect_ports_r;
  GtkWidget *jack_connect_midi_in_port;
  GtkWidget *jack_connect_midi_out_port;
#endif
#ifdef _HAVE_PORTAUDIO_
  GtkWidget *portaudio_device;
  GtkWidget *portaudio_sample_rate;
  GtkWidget *portaudio_period_size;
#endif
#ifdef _HAVE_PORTMIDI_
  GtkWidget *portmidi_input_device;
  GtkWidget *portmidi_output_device;
#endif
#ifdef _HAVE_FLUIDSYNTH_
  GtkWidget *fluidsynth_soundfont;
  GtkWidget *fluidsynth_reverb;
  GtkWidget *fluidsynth_chorus;
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

  GList *audio_backend_list;
  GList *audio_driver_option_list;
  GList *midi_backend_list;
  GList *midi_driver_option_list;
};

struct audio_callback_data
{
  GtkWidget *dialog;

  GtkWidget *audio_driver;
  GtkWidget *midi_driver;
#ifdef _HAVE_JACK_
  GtkWidget *jack_audio_settings;
  GtkWidget *jack_midi_settings;
#endif
#ifdef _HAVE_PORTAUDIO_
  GtkWidget *portaudio_settings;
#endif
#ifdef _HAVE_PORTMIDI_
  GtkWidget *portmidi_settings;
#endif
};

static void
free_g_lists(struct callbackdata *cbdata){
  g_list_free(cbdata->audio_backend_list);
  g_list_free(cbdata->audio_driver_option_list);
  g_list_free(cbdata->midi_backend_list);
  g_list_free(cbdata->midi_driver_option_list);

  cbdata->audio_backend_list = NULL;
  cbdata->audio_driver_option_list = NULL;
  cbdata->midi_backend_list = NULL;
  cbdata->midi_driver_option_list = NULL;
}
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
  gboolean midi_in_device_was_default = !strcmp(prefs->portmidi_input_device->str, "default");
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

#define ASSIGNCOMBO(field) \
  if (cbdata->field)\
   g_string_assign (prefs->field,\
    (gchar *) gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT(cbdata->field)));

  ASSIGNTEXT(lilypath)
  ASSIGNTEXT(browser)
  ASSIGNTEXT(pdfviewer)
  ASSIGNTEXT(imageviewer)
  ASSIGNTEXT(username)
  ASSIGNTEXT(password)
  ASSIGNTEXT(profile)

  ASSIGNTEXT(fontspec)
  ASSIGNTEXT(denemopath)

  gchar const *text = (gchar *) gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT(cbdata->audio_driver));
  GList *item = g_list_find_custom(cbdata->audio_driver_option_list, text, (GCompareFunc)strcmp);
  gint index = g_list_position(cbdata->audio_driver_option_list, item);
  if(index<0) index=0;
  gchar *backend = g_list_nth_data(cbdata->audio_backend_list, index);
  g_string_assign(prefs->audio_driver, backend);

  text = (gchar *) gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT(cbdata->midi_driver));
  item = g_list_find_custom(cbdata->midi_driver_option_list, text, (GCompareFunc)strcmp);
  index = g_list_position(cbdata->midi_driver_option_list, item);
  if(index<0) index=0;
  backend = g_list_nth_data(cbdata->midi_backend_list, index);
  g_string_assign(prefs->midi_driver, backend);


#ifdef _HAVE_JACK_
//  ASSIGNBOOLEAN(jacktransport)
//  ASSIGNBOOLEAN(jacktransport_start_stopped)
  ASSIGNCOMBO(jack_connect_ports_l)
  ASSIGNCOMBO(jack_connect_ports_r)
  ASSIGNCOMBO(jack_connect_midi_in_port)
  ASSIGNCOMBO(jack_connect_midi_out_port)
#endif

#ifdef _HAVE_PORTAUDIO_
  ASSIGNCOMBO(portaudio_device)
  ASSIGNINT(portaudio_sample_rate)
  ASSIGNINT(portaudio_period_size)
#endif

#ifdef _HAVE_PORTMIDI_
  ASSIGNCOMBO(portmidi_input_device)
  ASSIGNCOMBO(portmidi_output_device)
#endif

#ifdef _HAVE_FLUIDSYNTH_
  ASSIGNTEXT(fluidsynth_soundfont)
  ASSIGNBOOLEAN(fluidsynth_reverb)
  ASSIGNBOOLEAN(fluidsynth_chorus)
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
  if(midi_in_device_was_default && strcmp(prefs->portmidi_input_device->str, "default")) {
    Denemo.gui->input_source = INPUTMIDI;
    prefs->startmidiin = TRUE;
  }
  /* Now write it all to denemorc */
  writeXMLPrefs (prefs);
}

static void
midi_audio_tab_update(GtkWidget *box, gpointer data)
{
  struct audio_callback_data *cbdata = (struct audio_callback_data *) data;

  gchar const *audio_driver = gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(cbdata->audio_driver));
  gchar const *midi_driver = gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(cbdata->midi_driver));

#ifdef _HAVE_JACK_
  gtk_widget_set_visible(cbdata->jack_audio_settings, strcmp(audio_driver, "JACK") == 0);
  gtk_widget_set_visible(cbdata->jack_midi_settings, strcmp(midi_driver, "JACK") == 0);
#endif
#ifdef _HAVE_PORTAUDIO_
  gtk_widget_set_visible(cbdata->portaudio_settings, strcmp(audio_driver, "PortAudio") == 0);
#endif
#ifdef _HAVE_PORTMIDI_
  gtk_widget_set_visible(cbdata->portmidi_settings, strcmp(midi_driver, "PortMidi") == 0);
#endif

  // resize the dialog to whatever size is necessary to show all widgets
  gtk_window_resize(GTK_WINDOW(cbdata->dialog), 1, 1);
}

void
preferences_change (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *separator;
  GtkWidget *main_vbox;
  GtkWidget *autosave;
  GtkWidget *autosave_timeout;
  GtkWidget *maxhistory;
  GtkWidget *notebook;
  GtkWidget *hbox;
#ifdef _HAVE_JACK_
  GtkWidget *jack_audio_settings;
  GtkWidget *jack_midi_settings;
#endif
#ifdef _HAVE_PORTAUDIO_
  GtkWidget *portaudio_settings;
#endif
#ifdef _HAVE_PORTMIDI_
  GtkWidget *portmidi_settings;
#endif
  GList *g;
  gint i;
  static struct callbackdata cbdata;
  g_assert (gui != NULL);
  
  
  cbdata.audio_backend_list = NULL;
  cbdata.audio_driver_option_list = NULL;
  cbdata.midi_backend_list = NULL;
  cbdata.midi_driver_option_list = NULL;

  // these lists need to be initialized the first time this function is called
  // The order is chose to default to portaudio, alsa, jack if present
  if (!cbdata.audio_backend_list) {
#ifdef _HAVE_PORTAUDIO_
    cbdata.audio_backend_list = g_list_append(cbdata.audio_backend_list, (gpointer)"portaudio");
    cbdata.audio_driver_option_list = g_list_append(cbdata.audio_driver_option_list, (gpointer)"PortAudio");
#endif

#ifdef _HAVE_JACK_
    cbdata.audio_backend_list = g_list_append(cbdata.audio_backend_list, (gpointer)"jack");
    cbdata.audio_driver_option_list = g_list_append(cbdata.audio_driver_option_list, (gpointer)"JACK");
#endif

    cbdata.audio_backend_list = g_list_append(cbdata.audio_backend_list, (gpointer)"dummy");
    cbdata.audio_driver_option_list = g_list_append(cbdata.audio_driver_option_list, (gpointer)"none");

#ifdef _HAVE_PORTMIDI_
    cbdata.midi_backend_list = g_list_append(cbdata.midi_backend_list, (gpointer)"portmidi");
    cbdata.midi_driver_option_list = g_list_append(cbdata.midi_driver_option_list, (gpointer)"PortMidi");
#endif
#ifdef _HAVE_ALSA_
    cbdata.midi_backend_list = g_list_append(cbdata.midi_backend_list, (gpointer)"alsa");
    cbdata.midi_driver_option_list = g_list_append(cbdata.midi_driver_option_list, (gpointer)"ALSA");
#endif
#ifdef _HAVE_JACK_
    cbdata.midi_backend_list = g_list_append(cbdata.midi_backend_list, (gpointer)"jack");
    cbdata.midi_driver_option_list = g_list_append(cbdata.midi_driver_option_list, (gpointer)"JACK");
#endif

    cbdata.midi_backend_list = g_list_append(cbdata.midi_backend_list, (gpointer)"dummy");
    cbdata.midi_driver_option_list = g_list_append(cbdata.midi_driver_option_list, (gpointer)"none");
  }


  dialog = gtk_dialog_new_with_buttons (_("Preferences - Denemo"),
                                        GTK_WINDOW (Denemo.window),
                                        (GtkDialogFlags) (GTK_DIALOG_MODAL |
                                                          GTK_DIALOG_DESTROY_WITH_PARENT),
                                        GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
                                        GTK_STOCK_CANCEL, GTK_STOCK_CANCEL,
                                        NULL);

  //gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);
  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  notebook = gtk_notebook_new ();
  gtk_container_add (GTK_CONTAINER(content_area), notebook);
#define VBOX main_vbox

#define NEWPAGE(thelabel) \
    main_vbox = gtk_vbox_new (FALSE, 1);\
    gtk_notebook_append_page (GTK_NOTEBOOK (notebook), main_vbox, NULL);\
    gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (notebook), main_vbox, _(thelabel));

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
  gtk_box_pack_start (GTK_BOX (VBOX), hbox, FALSE, TRUE, 0);\
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
  gtk_box_pack_start (GTK_BOX (VBOX), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (thelabel);\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  field = gtk_spin_button_new_with_range (1, 50, 1.0);\
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (field), Denemo.prefs.field);\
  gtk_box_pack_start (GTK_BOX (hbox), field, FALSE, FALSE, 0);\
  cbdata.field = field;

#define ENTRY_LIMITS(thelabel, field, min, max, step)   \
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

#define CBOX(thelable, field, thelist, settext)\
 GtkWidget *field = gtk_combo_box_text_new_with_entry ();\
 i=0;\
 for (g=thelist;g;g=g->next){\
  gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT(field), g->data);\
  if (0==strcmp(g->data, settext))\
    gtk_combo_box_set_active(GTK_COMBO_BOX(field), i);\
  i++;\
 }

#define COMBOBOX(thelabel, field, thelist, settext, editable)\
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (VBOX), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (thelabel);\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (VBOX), hbox, FALSE, TRUE, 0);\
  CBOX(thelable, field, thelist, settext)\
  gtk_box_pack_start (GTK_BOX (hbox), field, FALSE, FALSE, 0);\
  gtk_widget_show (field);\
  cbdata.field = field;

#define SEPARATOR()\
  separator = gtk_hseparator_new();\
  gtk_box_pack_start (GTK_BOX (VBOX), separator, FALSE, TRUE, 4);\

  /*
   * Note entry settings
   */

  NEWPAGE("View");
  BOOLEANENTRY("Highlight the cursor", cursor_highlight);

  BOOLEANENTRY("Display general toolbar", toolbar);
  BOOLEANENTRY("Display Note/Rest entry toolbar", notation_palette);
 
  BOOLEANENTRY("Display Controls for Incoming MIDI signals", midi_in_controls);
  BOOLEANENTRY("Display Controls for Playback", playback_controls);

  BOOLEANENTRY("Display console pane", console_pane);
  BOOLEANENTRY("Display lyrics pane", lyrics_pane);
  BOOLEANENTRY("Display Titles. Controls etc", visible_directive_buttons);

  BOOLEANENTRY("Display Music Snippets", rhythm_palette);
  BOOLEANENTRY("Display menu of objects toolbar", object_palette);
  INTENTRY_LIMITS(_("% Zoom"), zoom, 1, 100);
  INTENTRY_LIMITS(_("% of display height per system"), system_height, 1, 100);

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
  g_signal_connect (G_OBJECT (autosave), "toggled",
    G_CALLBACK (toggle_autosave), autosave_timeout);

  BOOLEANENTRY("Autosave Parts", saveparts);

  static struct audio_callback_data audio_cbdata;

  NEWPAGE("Audio/MIDI");

  BOOLEANENTRY("Play back entered notes immediately", immediateplayback);
  INTENTRY_LIMITS(_("Pitch Spelling Channel"), pitchspellingchannel, 0, 15);
  INTENTRY_LIMITS(_("Pitch Spelling Program"), pitchspellingprogram, 0, 127);

  BOOLEANENTRY("Rhythm Entry for MIDI in", startmidiin);

  INTENTRY_LIMITS(_("% MIDI-in Dynamic Compression"), dynamic_compression, 1, 100);


  GList *item = g_list_find_custom(cbdata.audio_backend_list, Denemo.prefs.audio_driver->str, (GCompareFunc)strcmp);
  gint index = g_list_position(cbdata.audio_backend_list, item);
  if(index<0) index=0;
  gchar *driver = g_list_nth_data(cbdata.audio_driver_option_list, index);


  SEPARATOR();

  COMBOBOX("Audio backend", audio_driver, cbdata.audio_driver_option_list, driver, FALSE);
  g_signal_connect(G_OBJECT(GTK_COMBO_BOX(audio_driver)), "changed", G_CALLBACK(midi_audio_tab_update), &audio_cbdata);
  /*
   * JACK settings
   */
#ifdef _HAVE_JACK_

#undef VBOX
#define VBOX jack_audio_settings
  jack_audio_settings = gtk_vbox_new(FALSE, 1);
  gtk_box_pack_start(GTK_BOX(main_vbox), jack_audio_settings, FALSE, TRUE, 0);

  GList *jack_audio_output_ports = get_jack_ports(FALSE, FALSE);

  COMBOBOX("Connect to port (left)",  jack_connect_ports_l, jack_audio_output_ports, Denemo.prefs.jack_connect_ports_l->str, TRUE);
  COMBOBOX("Connect to port (right)", jack_connect_ports_r, jack_audio_output_ports, Denemo.prefs.jack_connect_ports_r->str, TRUE);

#undef VBOX
#define VBOX main_vbox

#endif // _HAVE_JACK_

  /*
   * PortAudio settings
   */
#ifdef _HAVE_PORTAUDIO_

#undef VBOX
#define VBOX portaudio_settings
  portaudio_settings = gtk_vbox_new(FALSE, 1);
  gtk_box_pack_start(GTK_BOX(main_vbox), portaudio_settings, FALSE, TRUE, 0);

  GList *devices = get_portaudio_devices();
  /* if default is requested choose first in portaudio list, rather than rely on portaudio which fails to select a default */
  if((!strcmp(Denemo.prefs.portaudio_device->str, "default")) && (g_list_length(devices)>1))
    g_string_assign(Denemo.prefs.portaudio_device, (gchar*)(devices->next->data));
    
  COMBOBOX("Output device", portaudio_device, devices, Denemo.prefs.portaudio_device->str, FALSE);
  free_portaudio_devices(devices);

  INTENTRY_LIMITS(_("Sample rate"), portaudio_sample_rate, 0, 96000);
  INTENTRY_LIMITS(_("Period size"), portaudio_period_size, 0, 2048);

#undef VBOX
#define VBOX main_vbox

#endif // _HAVE_PORTAUDIO_


  item = g_list_find_custom(cbdata.midi_backend_list, Denemo.prefs.midi_driver->str, (GCompareFunc)strcmp);
  index = g_list_position(cbdata.midi_backend_list, item);
  if(index<0) index=0;
  driver = g_list_nth_data(cbdata.midi_driver_option_list, index);


  SEPARATOR();


  COMBOBOX("MIDI backend", midi_driver, cbdata.midi_driver_option_list, driver, FALSE);
  g_signal_connect(G_OBJECT(GTK_COMBO_BOX(midi_driver)), "changed", G_CALLBACK(midi_audio_tab_update), &audio_cbdata);
  /*
   * JACK settings
   */
#ifdef _HAVE_JACK_

#undef VBOX
#define VBOX jack_midi_settings
  jack_midi_settings = gtk_vbox_new(FALSE, 1);
  gtk_box_pack_start(GTK_BOX(main_vbox), jack_midi_settings, FALSE, TRUE, 0);

  GList *jack_midi_input_ports = get_jack_ports(TRUE, FALSE);
  GList *jack_midi_output_ports = get_jack_ports(TRUE, TRUE);
  COMBOBOX("Connect input to port", jack_connect_midi_in_port, jack_midi_output_ports, Denemo.prefs.jack_connect_midi_in_port->str, TRUE);
  COMBOBOX("Connect output to port", jack_connect_midi_out_port, jack_midi_input_ports, Denemo.prefs.jack_connect_midi_out_port->str, TRUE);
  free_jack_ports(jack_midi_output_ports);
  free_jack_ports(jack_midi_input_ports);

#undef VBOX
#define VBOX main_vbox

#endif // _HAVE_JACK_

  /*
   * PortMidi settings
   */
#ifdef _HAVE_PORTMIDI_

#undef VBOX
#define VBOX portmidi_settings
  portmidi_settings = gtk_vbox_new(FALSE, 1);
  gtk_box_pack_start(GTK_BOX(main_vbox), portmidi_settings, FALSE, TRUE, 0);

  GList *input_devices = get_portmidi_devices(FALSE);
  GList *output_devices = get_portmidi_devices(TRUE);

  COMBOBOX("Input device", portmidi_input_device, input_devices, Denemo.prefs.portmidi_input_device->str, FALSE);
  COMBOBOX("Output device", portmidi_output_device, output_devices, Denemo.prefs.portmidi_output_device->str, FALSE);

  free_portmidi_devices(input_devices);
  free_portmidi_devices(output_devices);

#undef VBOX
#define VBOX main_vbox

#endif


  SEPARATOR();

#ifdef _HAVE_FLUIDSYNTH_
  /*
   * FluidSynth settings
   */
  TEXTENTRY("Soundfont", fluidsynth_soundfont)

  hbox = gtk_hbox_new(FALSE, 8);
  gtk_box_pack_start(GTK_BOX(VBOX), hbox, FALSE, TRUE, 0);
  GtkWidget *button = gtk_button_new_with_label(_("Choose Soundfont"));
  gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);

  g_signal_connect (G_OBJECT (button), "clicked",
    G_CALLBACK(choose_sound_font), fluidsynth_soundfont);

  gtk_widget_show(button);

  BOOLEANENTRY("Enable Reverb on soundfont", fluidsynth_reverb)
  BOOLEANENTRY("Enable Chorus on soundfont", fluidsynth_chorus)
#endif

  gtk_widget_show_all (dialog);

  audio_cbdata.dialog = dialog;

  audio_cbdata.audio_driver = cbdata.audio_driver;
  audio_cbdata.midi_driver = cbdata.midi_driver;
#ifdef _HAVE_JACK_
  audio_cbdata.jack_audio_settings = jack_audio_settings;
  audio_cbdata.jack_midi_settings = jack_midi_settings;
#endif
#ifdef _HAVE_PORTAUDIO_
  audio_cbdata.portaudio_settings = portaudio_settings;
#endif
#ifdef _HAVE_PORTMIDI_
  audio_cbdata.portmidi_settings = portmidi_settings;
#endif

  midi_audio_tab_update(NULL, (gpointer*) &audio_cbdata);


#define SETCALLBACKDATA(field) \
  cbdata.field = field;

  cbdata.prefs = &Denemo.prefs;
  SETCALLBACKDATA(autosave);
  SETCALLBACKDATA(autosave_timeout);
  SETCALLBACKDATA(maxhistory);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      set_preferences (&cbdata);

      // stop playback and restart audio subsystem
      // FIXME: only do this when audio settings actually changed
      midi_stop();
      audio_shutdown();
      audio_initialize(cbdata.prefs);
      free_g_lists(&cbdata);
    }
  else
    {
      free_g_lists(&cbdata);
    }
    gtk_widget_destroy (dialog);
}

