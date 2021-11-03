/* staffpropdialog.c
 * callback that creates a "Staff Properties" dialog box asking
 * the user to change the properties of the current staff

 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller */

#include <glib.h>
#include <glib/gprintf.h>
#include <gtk/gtk.h>
#include "display/calculatepositions.h"
#include "command/chord.h"
#include "command/contexts.h"
#include <denemo/denemo.h>
#include "command/processstaffname.h"
#include "command/staff.h"
#include "core/view.h"
#include "core/utils.h"
#include "ui/dialogs.h"
#include <stdlib.h>
#include <string.h>
#include "export/xmldefs.h"
#include "audio/midi.h"
#include "command/select.h"
#include "audio/audiointerface.h"

extern int ParseSoundfont (gchar * soundfont, gint index, gchar ** name, gint * preset, gint * bank);        //in "external" code libsffile/sfile.c


static gchar *GM_Instrument_Names[] = {
"Acoustic Grand Piano",
"Bright Acoustic Piano",
"Electric Grand Piano",
"Honky-tonk Piano",
"Electric Piano 1",
"Electric Piano 2",
"Harpsichord",
"Clavinet",
"Celesta",
"Glockenspiel",
"Music Box",
"Vibraphone",
"Marimba",
"Xylophone",
"Tubular Bells",
"Dulcimer",
"Drawbar Organ",
"Percussive Organ",
"Rock Organ",
"Church Organ",
"Reed Organ",
"Accordion",
"Harmonica",
"Tango Accordion",
"Acoustic Guitar (nylon)",
"Acoustic Guitar (steel)",
"Electric Guitar (jazz)",
"Electric Guitar (clean)",
"Electric Guitar (muted)",
"Overdriven Guitar",
"Distortion Guitar",
"Guitar Harmonics",
"Acoustic Bass",
"Electric Bass (finger)",
"Electric Bass (pick)",
"Fretless Bass",
"Slap Bass 1",
"Slap Bass 2",
"Synth Bass 1",
"Synth Bass 2",
"Violin",
"Viola",
"Cello",
"Contrabass",
"Tremolo Strings",
"Pizzicato Strings",
"Orchestral Harp",
"Timpani",
"String Ensemble 1",
"String Ensemble 2",
"Synth Strings 1",
"Synth Strings 2",
"Choir Aahs",
"Voice Oohs",
"Synth Choir",
"Orchestra Hit",
"Trumpet",
"Trombone",
"Tuba",
"Muted Trumpet",
"French Horn",
"Brass Section",
"Synth Brass 1",
"Synth Brass 2",
"Soprano Sax",
"Alto Sax",
"Tenor Sax",
"Baritone Sax",
"Oboe",
"English Horn",
"Bassoon",
"Clarinet",
"Piccolo",
"Flute",
"Recorder",
"Pan Flute",
"Blown bottle",
"Shakuhachi",
"Whistle",
"Ocarina",
"Lead 1 (square)",
"Lead 2 (sawtooth)",
"Lead 3 (calliope)",
"Lead 4 chiff",
"Lead 5 (charang)",
"Lead 6 (voice)",
"Lead 7 (fifths)",
"Lead 8 (bass + lead)",
"Pad 1 (new age)",
"Pad 2 (warm)",
"Pad 3 (polysynth)",
"Pad 4 (choir)",
"Pad 5 (bowed)",
"Pad 6 (metallic)",
"Pad 7 (halo)",
"Pad 8 (sweep)",
"FX 1 (rain)",
"FX 2 (soundtrack)",
"FX 3 (crystal)",
"FX 4 (atmosphere)",
"FX 5 (brightness)",
"FX 6 (goblins)",
"FX 7 (echoes)",
"FX 8 (sci-fi)",
"Sitar",
"Banjo",
"Shamisen",
"Koto",
"Kalimba",
"Bagpipe",
"Fiddle",
"Shanai",
"Tinkle Bell",
"Agogo",
"Steel Drums",
"Woodblock",
"Taiko Drum",
"Melodic Tom",
"Synth Drum",
"Reverse Cymbal",
"Guitar Fret Noise",
"Breath Noise",
"Seashore",
"Bird Tweet",
"Telephone Ring",
"Helicopter",
"Applause",
"Gunshot"
};


/**
 * Callback data used for setting the staffs properties
 *
 */
struct callbackdata
{
  DenemoProject *gui;
  DenemoStaff *staffstruct;
  GtkWidget *denemo_name;
  GtkWidget *subpart;
  GtkWidget *midi_instrument;
  GtkWidget *midi_prognum;
  GtkWidget *midi_channel;
  GtkWidget *midi_prognum_override;
  GtkWidget *device_port;
  GtkWidget *space_above;
  GtkWidget *space_below;
  GtkWidget *no_of_lines;
  GtkWidget *transposition;
  GtkWidget *volume;
  GtkWidget *override_volume;
};


/**
 * Set the staffs properties
 * @param cbdata pointer to the callback data structure containing the preference data to set.
 * @return none
 */
static void
set_properties (struct callbackdata *cbdata)
{

  DenemoStaff *staffstruct = cbdata->staffstruct;
  gint old_above = staffstruct->space_above;
  gint old_below = staffstruct->space_below;
  gint old_volume = staffstruct->volume;
#if GTK_MAJOR_VERSION==3
#define ASSIGNTEXT(field) \
  if(cbdata->field) {\
    gchar *text=gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT (cbdata->field));\
    if (text)\
        g_string_assign (staffstruct->field,text);\
    }
#else
#define ASSIGNTEXT(field) \
  if(cbdata->field)\
    g_string_assign (staffstruct->field,\
    gtk_entry_get_text (GTK_ENTRY (cbdata->field)))
#endif

#define ASSIGNNUMBER(field) \
  if(cbdata->field)\
    staffstruct->field = \
      atoi(gtk_entry_get_text(GTK_ENTRY (cbdata->field)))
#define ASSIGNNUMBER_1(field) \
  if(cbdata->field)\
    staffstruct->field = \
      atoi(gtk_entry_get_text(GTK_ENTRY (cbdata->field)))-1

#define ASSIGNBOOLEAN(field) \
  if(cbdata->field)\
    staffstruct->field = \
    (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->field)));

  /* rename of part name */
  if(canonicalize_denemo_name ((gchar *) gtk_entry_get_text (GTK_ENTRY (cbdata->denemo_name)), staffstruct->denemo_name))
    warningdialog (_("Only alphanumeric characters are allowed - part name unchanged"));
  if(cbdata->subpart)
    {
     if(staffstruct->subpart==NULL)
        staffstruct->subpart = g_string_new ("");
     canonicalize_denemo_name ((gchar *) gtk_entry_get_text (GTK_ENTRY (cbdata->subpart)), staffstruct->subpart);
    }
  set_lily_name (staffstruct->denemo_name, staffstruct->lily_name);


  //g_debug("first %d\t", staffstruct->space_above);
  ASSIGNNUMBER (space_above);
  //g_debug("then %d\t", staffstruct->space_above);
  ASSIGNNUMBER (space_below);
  ASSIGNNUMBER (no_of_lines);
  ASSIGNNUMBER (transposition);


  /* set MIDI channel/prognum */
  ASSIGNTEXT (midi_instrument);

  ASSIGNTEXT (device_port);
  ASSIGNBOOLEAN (override_volume);
  ASSIGNNUMBER (volume);
  
 if ((Denemo.prefs.dynamic_compression) && (old_volume != staffstruct->volume))
	 warningdialog (_("The setting for the loudness of this staff will ignored (or at least modified) by the setting of the dynamic compression preference you have.\nSee Preferences Dialog, Audio Tab or Playback Controls \"Always Full Volume\" checkbox."));

  // ASSIGNBOOLEAN(midi_prognum_override);
  if (staffstruct->midi_instrument->len)
    {
      staffstruct->midi_prognum = get_midi_prognum (staffstruct);
      gint i;
      gchar *name;
      gint preset, bank;
      gint npresets = ParseSoundfont (Denemo.prefs.fluidsynth_soundfont->str, 0, NULL, NULL, NULL);
      if (npresets)
        {
          for (i = 0; i < npresets - 1; i++)
            {
              (void) ParseSoundfont (NULL, i, &name, &preset, &bank);
              if (!strcmp (name, staffstruct->midi_instrument->str))
                {
                  staffstruct->midi_prognum = preset;
                  gchar *scheme = g_strdup_printf ("(d-MidiInstrumentName \"%s\")", GM_Instrument_Names[preset&0xFF]);
                  call_out_to_guile (scheme);
                  g_free (scheme);
                  printf ("\nMIDI Instrument == %s (GM name %s)\nMIDI PROGRAM == %d\n", staffstruct->midi_instrument->str,  GM_Instrument_Names[preset&0xFF], staffstruct->midi_prognum);
                  break;
                }
            }
        }
      //   if(staffstruct->midi_prognum != i) /* I am not sure why this was necessary and if it is still needed*/
      //     ASSIGNNUMBER_1(midi_prognum);
      ASSIGNNUMBER_1 (midi_channel);
      //printf ("\nAssigned MIDI Instrument == %s \nAssigned MIDI PROGRAM == %d i == %d\n", staffstruct->midi_instrument->str, staffstruct->midi_prognum, i);

    }
  else
    {
      ASSIGNNUMBER_1 (midi_prognum);
      ASSIGNNUMBER_1 (midi_channel);
    }
    {
      unsigned char buffer[3];/* third byte is unused but is put into the queue so must be accessible */
      /* set selected midi program on the synthesizer so that users can play MIDI controller with current staff instrument without having to do playback first*/
      //g_info ("Using channel %d port %d prognum %d\n",  staffstruct->midi_channel, staffstruct->midi_port, staffstruct->midi_prognum);
      buffer[0] = 0xC0 /*MIDI_PROG_CHANGE*/ | staffstruct->midi_channel;
      buffer[1] = staffstruct->midi_prognum;
      play_midi_event (DEFAULT_BACKEND, staffstruct->midi_port, buffer);
    }
    
    if ((old_above != staffstruct->space_above) || (old_below != staffstruct->space_below))
		{
			if (!staffstruct->fixed_height)
				{
					staffstruct->fixed_height = TRUE;
					warningdialog (_("This staff will no longer auto-adjust its height to the staff content"));
				}
		} 
  //g_debug ("Staff Transposition %d\n", staffstruct->transposition);
  gtk_widget_queue_draw (Denemo.scorearea);
  score_status (cbdata->gui, TRUE);
}

//Check all staffs for one with same channel but different program as the passed staff and report its number >0 if found, else 0
static gint check_for_channel_conflicts (DenemoStaff *staffstruct)
{
  DenemoProject *gui = Denemo.project;
  DenemoMovement *si = gui->movement;
  gint midi_channel = staffstruct->midi_channel;
  gint midi_prognum = staffstruct->midi_prognum;
  gint staffnum = 1;
  GList *curstaff = si->thescore;
    for (curstaff;curstaff;curstaff=curstaff->next, staffnum++)
      {
        DenemoStaff *thisstaff = (DenemoStaff *)curstaff->data;
        if (thisstaff == staffstruct)
          continue;
        if (thisstaff->midi_channel == midi_channel && 
            thisstaff->midi_prognum != midi_prognum)
            return staffnum;
      }
  return 0;
}


/**
 * Create Dialog to allow the user to set the current staff's parameters
 *
 */
static gboolean
staff_properties_change (void)
{
  DenemoProject *gui = Denemo.project;
  DenemoMovement *si = gui->movement;
  DenemoStaff *staffstruct = (DenemoStaff *) si->currentstaff->data;
  gboolean result = FALSE;

  GtkWidget *dialog;
  GtkWidget *notebook;
  GtkWidget *label;
  GtkWidget *main_vbox;
  GtkWidget *hbox;
  GtkWidget *entrywidget;
  static GString *entrycontent;
  static GList *instrument_list = NULL;
  static struct callbackdata cbdata;
  gint i;
  if (instrument_list)
    {
            g_list_free_full (instrument_list, g_free);
            instrument_list = NULL;
    }
  {
  gint i;
  gchar *name;
  gint preset = 0, bank = 0;
  gint npresets = ParseSoundfont (Denemo.prefs.fluidsynth_soundfont->str, 0, NULL, NULL, NULL);
  if (npresets)
    {
      gchar **array = g_malloc0 (128 * sizeof (gchar *));
      for (i = 0; i < npresets - 1; i++)
        {
          (void) ParseSoundfont (NULL, i, &name, &preset, &bank);
          if (bank == 0) {
               array[preset&0x7F] = g_strdup ((gchar *) name);
          }
        }
      for (i = 0; i < 128; i++)
        if(array[i])
            instrument_list = g_list_append(instrument_list, array[i]);
      g_free (array);
    }
  }

  if (!entrycontent)
    {
      entrycontent = g_string_new (NULL);
    }

  dialog = gtk_dialog_new_with_buttons (_("Staff Properties"), GTK_WINDOW (Denemo.window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), _("_OK"), GTK_RESPONSE_ACCEPT, _("_Cancel"), GTK_RESPONSE_REJECT, NULL);
  //gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);
  notebook = gtk_notebook_new ();
  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_container_add (GTK_CONTAINER (content_area), notebook);

#define NEWPAGE(thelabel) \
  main_vbox = gtk_vbox_new (FALSE, 1);\
  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), main_vbox, NULL);\
  gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (notebook), main_vbox,\
                                               _(thelabel));

#define TEXTENTRY(thelabel, field) \
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (_(thelabel));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  entrywidget = gtk_entry_new ();\
  g_string_sprintf (entrycontent, "%s", staffstruct->field?staffstruct->field->str:"");\
  gtk_entry_set_text (GTK_ENTRY (entrywidget), entrycontent->str);\
  gtk_box_pack_start (GTK_BOX (hbox), entrywidget, TRUE, TRUE, 0);\
  cbdata.field = entrywidget;

#define INTENTRY_LIMITS(thelabel, field, min, max) \
  GtkWidget *field;\
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (_(thelabel));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  field = gtk_spin_button_new_with_range (min, max, 1.0);\
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (field), staffstruct->field);\
  gtk_box_pack_start (GTK_BOX (hbox), field, FALSE, FALSE, 0);\
  cbdata.field = field;

#define INTENTRY_LIMITS_1(thelabel, field, min, max) \
  GtkWidget *field;\
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (_(thelabel));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  field = gtk_spin_button_new_with_range (min, max, 1.0);\
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (field), staffstruct->field+1);\
  gtk_box_pack_start (GTK_BOX (hbox), field, FALSE, FALSE, 0);\
  cbdata.field = field;

#define BOOLEANENTRY(thelabel, field) \
  GtkWidget *field;\
  field =\
    gtk_check_button_new_with_label (_(thelabel)); \
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (field),\
                                            staffstruct->field);\
    gtk_box_pack_start (GTK_BOX (main_vbox), field, FALSE, TRUE, 0);\
    cbdata.field = field;
#if GTK_MAJOR_VERSION==3
#define COMBOBOXENTRY(thelabel, field, thelist, setstring) \
  GtkWidget *field;\
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_container_add (GTK_CONTAINER(main_vbox), hbox);   \
  label = gtk_label_new (_(thelabel));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_container_add (GTK_CONTAINER(hbox), label);   \
  field = gtk_combo_box_text_new ();\
  i=0;\
  while (thelist){\
    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT(field), (gchar *) thelist->data);\
      if (!g_strcmp0 (thelist->data, setstring->str))\
    gtk_combo_box_set_active(GTK_COMBO_BOX (field),i);\
    i++;\
    thelist=thelist->next;\
  }\
  gtk_container_add (GTK_CONTAINER(hbox), field);\
  cbdata.field = field;
#else
#define COMBOBOXENTRY(thelabel, field, thelist, setstring) \
  GtkWidget *field;\
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_container_add (GTK_CONTAINER(main_vbox), hbox);   \
  label = gtk_label_new (_(thelabel));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_container_add (GTK_CONTAINER(hbox), label);\
  field = gtk_combo_new ();\
  gtk_combo_set_popdown_strings(GTK_COMBO(field), thelist); \
  gtk_entry_set_text (GTK_ENTRY (GTK_COMBO (field)->entry), setstring->str);\
  gtk_container_add (GTK_CONTAINER(hbox), field);   \
  cbdata.field = GTK_COMBO (field)->entry;
#endif
  /* Print appearance tab */
  NEWPAGE (_("Typeset Appearance"));
  TEXTENTRY (_("Part name (a-z,0-9):"), denemo_name);
  gtk_widget_set_tooltip_text (hbox, _( "All staffs with the same part name will be typeset with the Print Part command. Use a blank part name for staffs that should be printed with every part. Only alphabetic characters, space and numerals are allowed"));
  TEXTENTRY (_("Sub Part name:"), subpart);
  gtk_widget_set_tooltip_text (hbox, _( "If a single part (e.g. piano) has more than one staff they should be named here."));

  /* Display appearance tab */
  NEWPAGE ("Display Appearance");
  //gtk_widget_grab_focus (entrywidget);
  INTENTRY_LIMITS (_("Space above:"), space_above, 0, MAXEXTRASPACE);
  INTENTRY_LIMITS (_("Space below:"), space_below, 0, MAXEXTRASPACE);
  INTENTRY_LIMITS (_("Number of Display Staff Lines:"), no_of_lines, 1, 5);


  /* MIDI tab */
  NEWPAGE (_("MIDI"));
  COMBOBOXENTRY (_("MIDI Instrument:"), midi_instrument, instrument_list, staffstruct->midi_instrument);
  INTENTRY_LIMITS (_("Transposition:"), transposition, -30, 30);
  BOOLEANENTRY (_("Always Full Volume"), override_volume);
  INTENTRY_LIMITS (_("Master Volume:"), volume, 0, 127);
  // BOOLEANENTRY("Override MIDI Channel/Program", midi_prognum_override);
  INTENTRY_LIMITS_1 (_("Channel:"), midi_channel, 1, 16);
  INTENTRY_LIMITS_1 (_("Program:"), midi_prognum, 1, 128);
  gtk_widget_set_sensitive (midi_prognum, !staffstruct->midi_instrument->len); // meaningless to set prognum if instrument name is set
 // g_debug ("chan prog %d %d\n", staffstruct->midi_channel, staffstruct->midi_prognum);

  // FIXME
//  GList *md = device_manager_DevicePort_list();
//  if(md) {
//#ifdef _HAVE_JACK_
//    COMBOBOXENTRY("Midi Devices", device_port, md, staffstruct->device_port);
//#endif
//  }
//  else
  cbdata.device_port = NULL;
  /* Set up the callback data */
#define SETCALLBACKDATA(field) \
    cbdata.field = field;

  SETCALLBACKDATA (gui);
  SETCALLBACKDATA (staffstruct);

  /* FIXME
     Also set things up so that the callback'll run when you hit enter
     * in the text entries */

/*
  gtk_entry_set_activates_default (GTK_ENTRY (denemo_name), TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY (space_above), TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY (space_below), TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY (no_of_lines), TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY (transposition), TRUE);

*/
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);

  gtk_widget_show_all (dialog);
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      set_properties (&cbdata);
      result = TRUE;
    }
  gtk_widget_destroy (dialog);
  gint conflict_staff = check_for_channel_conflicts (staffstruct);
  if (conflict_staff)
    { gchar *msg = g_strdup_printf ("%s%d%s", _("Staff "), conflict_staff, _(" is using the same MIDI channel as this staff, but a different MIDI instrument - this won't work. Choose a different channel."));
      warningdialog (msg);
      g_free (msg);
    }
  return result;
}


void
staff_properties_change_cb (DenemoAction * action, DenemoScriptParam * param)
{
  GET_5PARAMS (action, param, denemo_name, transposition, device_port, midi_prognum, midi_channel);
  DenemoStaff *staff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;

  if (query)
    {
      if (*query)
        if (!strcmp ("denemo_name", query))
          {
            g_string_assign (param->string, staff->denemo_name->str);
            if (staff->subpart)
                g_string_append_printf (param->string, "_%s", staff->subpart->str);
            param->status = TRUE;
          }
      if (*query)
        if (!strcmp ("transposition", query))
          {
            g_string_printf (param->string, "%d", staff->transposition);
            param->status = TRUE;
          }
      if (*query)
        if (!strcmp ("midi_channel", query))
          {
            g_string_printf (param->string, "%d", staff->midi_channel);
            param->status = TRUE;
          }
      if (*query)
        if (!strcmp ("midi_prognum", query))
          {
            g_string_printf (param->string, "%d", staff->midi_prognum);
            param->status = TRUE;
          }
      if (*query)
        if (!strcmp ("lily_name", query))
          {
             g_string_assign (param->string, staff->lily_name->str);
            param->status = TRUE;
          }

      return;
    }
  take_snapshot ();
  signal_structural_change (Denemo.project);
  if (denemo_name)
    { gboolean ok = TRUE;
      gchar *name = strtok (denemo_name, "_");
      gchar *subpart = strtok (NULL, "_");
      if (name)
        ok = canonicalize_denemo_name (name, staff->denemo_name);
      else
        g_string_assign (staff->denemo_name, "");
      if ((ok==0) && subpart)
        {
            if(staff->subpart == NULL)
                staff->subpart = g_string_new("");
            ok = canonicalize_denemo_name (subpart, staff->subpart);
        }
      set_lily_name (staff->denemo_name, staff->lily_name);
      param->status = ok;
      return;
    }
  Denemo.project->movement->smfsync = G_MAXINT;
  if (device_port)
    {
      g_string_assign (staff->device_port, device_port);
      param->status = TRUE;
      return;
    }

  if (midi_prognum)
    {
      staff->midi_prognum = atoi (midi_prognum);
      param->status = TRUE;
      return;
    }
  if (midi_channel)
    {
      staff->midi_channel = atoi (midi_channel);
      param->status = TRUE;
      return;
    }
  if (transposition)
    {
      staff->transposition = atoi (transposition);
      param->status = TRUE;
      return;
    }
  (void) staff_properties_change ();
}
