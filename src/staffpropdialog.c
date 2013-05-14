/* staffpropdialog.c
 * callback that creates a "Staff Properties" dialog box asking
 * the user to change the properties of the current staff
 
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller */

#include <glib.h>
#include <glib/gprintf.h>
#include <gtk/gtk.h>
#include "calculatepositions.h"
#include "chordops.h"
#include "contexts.h"
#include <denemo/denemo.h>
#include "processstaffname.h"
#include "staffops.h"
#include "utils.h"
#include "dialogs.h"
#include <stdlib.h>
#include <string.h>
#include "xmldefs.h"
#include "midi.h"
#include "selectops.h"


extern int ParseSoundfont (gchar * soundfont, gint index, gchar ** name, gint * preset);        //in "external" code libsffile/sfile.c




/**
 * Callback data used for setting the staffs properties
 * 
 */
struct callbackdata
{
  DenemoGUI *gui;
  DenemoStaff *staffstruct;
  GtkWidget *denemo_name;
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
void
set_properties (struct callbackdata *cbdata)
{

  DenemoStaff *staffstruct = cbdata->staffstruct;
#if GTK_MAJOR_VERSION==3
#define ASSIGNTEXT(field) \
  if(cbdata->field)\
    g_string_assign (staffstruct->field,\
    gtk_combo_box_text_get_active_text (GTK_COMBO_BOX (cbdata->field)))
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

  /* rename of set staff/voice name */
  canonicalize_denemo_name ((gchar *) gtk_entry_get_text (GTK_ENTRY (cbdata->denemo_name)), staffstruct->denemo_name);
  set_lily_name (staffstruct->denemo_name, staffstruct->lily_name);

  /* !!!! Insert advisory function for detecting colliding staff names
   * here */
  //g_print("first %d\t", staffstruct->space_above);
  ASSIGNNUMBER (space_above);
  //g_print("then %d\t", staffstruct->space_above);
  ASSIGNNUMBER (space_below);
  ASSIGNNUMBER (no_of_lines);
  ASSIGNNUMBER (transposition);


  /* set MIDI channel/prognum */
  ASSIGNTEXT (midi_instrument);
  ASSIGNTEXT (device_port);
  ASSIGNBOOLEAN (override_volume);
  ASSIGNNUMBER (volume);
  // ASSIGNBOOLEAN(midi_prognum_override);
  if (staffstruct->midi_instrument->len)
    {
      staffstruct->midi_prognum = get_midi_prognum (staffstruct);
      gint i;
      gchar *name;
      gint preset;
      gint npresets = ParseSoundfont (Denemo.prefs.fluidsynth_soundfont->str, 0, NULL, NULL);
      if (npresets)
        {
          for (i = 0; i < npresets - 1; i++)
            {
              (void) ParseSoundfont (NULL, i, &name, &preset);
              if (!strcmp (name, staffstruct->midi_instrument->str))
                {
                  staffstruct->midi_prognum = preset;
                  printf ("\nMIDI Instrument == %s \nMIDI PROGRAM == %d\n", staffstruct->midi_instrument->str, staffstruct->midi_prognum);
                  break;
                }
            }
        }
      //   if(staffstruct->midi_prognum != i) /* I am not sure why this was necessary and if it is still needed*/
      //     ASSIGNNUMBER_1(midi_prognum);
      ASSIGNNUMBER_1 (midi_channel);
      printf ("\nAssigned MIDI Instrument == %s \nAssigned MIDI PROGRAM == %d i == %d\n", staffstruct->midi_instrument->str, staffstruct->midi_prognum, i);

    }
  else
    {
      ASSIGNNUMBER_1 (midi_prognum);
      ASSIGNNUMBER_1 (midi_channel);
    }

#ifdef DEBUG
  g_printf ("Staff Transposition %d\n", staffstruct->transposition);
#endif
  score_status (cbdata->gui, TRUE);
}


/**
 * Create Dialog to allow the user to set the current staff's parameters
 * 
 */
static gboolean
staff_properties_change (void)
{
  DenemoScore *si;
  DenemoGUI *gui;

  gboolean result = FALSE;
  DenemoStaff *staffstruct;
  GtkWidget *dialog;
  GtkWidget *notebook;
  GtkWidget *label;
  GtkWidget *main_vbox;
  GtkWidget *hbox;
  GtkWidget *entrywidget;
  static GString *entrycontent;
  GList *instrument_list = NULL;
  static struct callbackdata cbdata;
  gint i;

  if (!instrument_list)
    {
      gint i;
      gchar *name;
      gint npresets = ParseSoundfont (Denemo.prefs.fluidsynth_soundfont->str, 0, NULL, NULL);
      if (npresets)
        {
          for (i = 0; i < npresets - 1; i++)
            {
              (void) ParseSoundfont (NULL, i, &name, NULL);
              instrument_list = g_list_append (instrument_list, g_strdup ((gchar *) name));
            }
        }
    }



  {
    gui = Denemo.gui;
    si = gui->si;
    staffstruct = (DenemoStaff *) si->currentstaff->data;
    /*  if(staffstruct->staff_prolog && staffstruct->staff_prolog->len) { */
/* 	warningdialog("This staff has a custom prolog for the staff.\n" */
/* 		      "You will need to make your edits in the LilyPond window\n" */
/* 		      "to see them in the print-out."); */
/*       } */
  }


  if (!entrycontent)
    {
      entrycontent = g_string_new (NULL);
    }

  dialog = gtk_dialog_new_with_buttons (_("Staff Properties"), GTK_WINDOW (Denemo.window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_STOCK_CANCEL, NULL);
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
  gtk_container_add (GTK_CONTAINER(main_vbox), hbox);	\
  label = gtk_label_new (_(thelabel));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_container_add (GTK_CONTAINER(hbox), label);   \
  field = gtk_combo_box_text_new ();\
  i=0;\
  while (thelist){\
    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT(field), (gchar *) thelist->data);\
      if (!g_strcmp0 (thelist->data, setstring->str))\
	gtk_combo_box_set_active(GTK_COMBO_BOX_TEXT (field),i);\
    i++;\
    thelist=thelist->next;\
  }\
  gtk_container_add (GTK_CONTAINER(hbox), field);\
  cbdata.field = field;
#else
#define COMBOBOXENTRY(thelabel, field, thelist, setstring) \
  GtkWidget *field;\
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_container_add (GTK_CONTAINER(main_vbox), hbox);	\
  label = gtk_label_new (_(thelabel));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_container_add (GTK_CONTAINER(hbox), label);\
  field = gtk_combo_new ();\
  gtk_combo_set_popdown_strings(GTK_COMBO(field), thelist); \
  gtk_entry_set_text (GTK_ENTRY (GTK_COMBO (field)->entry), setstring->str);\
  gtk_container_add (GTK_CONTAINER(hbox), field);	\
  cbdata.field = GTK_COMBO (field)->entry;
#endif
  /* Display appearance tab */
  NEWPAGE ("Display Appearance");
  TEXTENTRY ("Staff name:", denemo_name);
  //gtk_widget_grab_focus (entrywidget);
  INTENTRY_LIMITS ("Space above:", space_above, 0, MAXEXTRASPACE);
  INTENTRY_LIMITS ("Space below:", space_below, 0, MAXEXTRASPACE);
  INTENTRY_LIMITS ("Number of Lines:", no_of_lines, 1, 5);


  /* MIDI tab */
  NEWPAGE (_("MIDI"));
  COMBOBOXENTRY (_("MIDI Instrument:"), midi_instrument, instrument_list, staffstruct->midi_instrument);
  INTENTRY_LIMITS (_("Transposition:"), transposition, -30, 30);
  BOOLEANENTRY (_("Always Full Volume"), override_volume);
  INTENTRY_LIMITS (_("Master Volume:"), volume, 0, 127);
  // BOOLEANENTRY("Override MIDI Channel/Program", midi_prognum_override);  
  INTENTRY_LIMITS_1 (_("Channel:"), midi_channel, 1, 16);
  INTENTRY_LIMITS_1 (_("Program:"), midi_prognum, 1, 128);
  g_print ("chan prog %d %d\n", staffstruct->midi_channel, staffstruct->midi_prognum);

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

  return result;
}


void
staff_properties_change_cb (GtkAction * action, DenemoScriptParam * param)
{
  GET_4PARAMS (action, param, denemo_name, device_port, midi_prognum, midi_channel);
  DenemoStaff *staff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;

  if (query)
    {
      if (*query)
        if (!strcmp ("denemo_name", query))
          {
            g_string_assign (param->string, staff->denemo_name->str);
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


      return;
    }
  take_snapshot ();
  signal_structural_change (Denemo.gui);
  if (denemo_name)
    {
      g_string_assign (staff->denemo_name, denemo_name);
      canonicalize_denemo_name (denemo_name, staff->denemo_name);
      set_lily_name (staff->denemo_name, staff->lily_name);
      param->status = TRUE;
      return;
    }
  if (device_port)
    {
      g_string_assign (staff->device_port, device_port);
      param->status = TRUE;
      return;
    }

  if (midi_prognum)
    {
      staff->midi_prognum = atoi (midi_prognum);
      return;
    }
  if (midi_channel)
    {
      staff->midi_channel = atoi (midi_channel);
      return;
    }
  (void) staff_properties_change ();
}
