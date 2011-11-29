/* staffpropdialog.c
 * callback that creates a "Staff Properties" dialog box asking
 * the user to change the properties of the current staff
 
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller */

#include <gtk/gtk.h>
#include "calculatepositions.h"
#include "chordops.h"
#include "contexts.h"
#include "device_manager.h"
#include <denemo/denemo.h>
#include "processstaffname.h"
#include "staffops.h"
#include "utils.h"
#include "dialogs.h"
#include <stdlib.h>
#include <string.h>
#include "xmldefs.h"
/**
 * List of MIDI instrument names
 * 
 */
static gchar *instruments[] = {
  "acoustic grand",
  "bright acoustic",
  "electric grand",
  "honky-tonk",
  "electric piano 1",
  "electric piano 2",
  "harpsichord",
  "clav",
  "celesta",
  "glockenspiel",
  "music box",
  "vibraphone",
  "marimba",
  "xylophone",
  "tubular bells",
  "dulcimer",
  "drawbar organ",
  "percussive organ",
  "rock organ",
  "church organ",
  "reed organ",
  "accordion",
  "harmonica",
  "concertina",
  "acoustic guitar (nylon)",
  "acoustic guitar (steel)",
  "electric guitar (jazz)",
  "electric guitar (clean)",
  "electric guitar (muted)",
  "overdriven guitar",
  "distorted guitar",
  "guitar harmo(dinics",
  "acoustic bass",
  "electric bass (finger)",
  "electric bass (pick)",
  "fretless bass",
  "slap bass 1",
  "slap bass 2",
  "synth bass 1",
  "synth bass 2",
  "violin",
  "viola",
  "cello",
  "contrabass",
  "tremolo strings",
  "pizzicato strings",
  "orchestral harp",
  "timpani",
  "string ensemble 1",
  "string ensemble 2",
  "synthstrings 1",
  "synthstrings 2",
  "choir aahs",
  "voice oohs",
  "synth voice",
  "orchestra hit",
  "trumpet",
  "trombone",
  "tuba",
  "muted trumpet",
  "french horn",
  "brass section",
  "synthbrass 1",
  "synthbrass 2",
  "soprano sax",
  "alto sax",
  "tenor sax",
  "baritone sax",
  "oboe",
  "english horn",
  "bassoon",
  "clarinet",
  "piccolo",
  "flute",
  "recorder",
  "pan flute",
  "blown bottle",
  "skakuhachi",
  "whistle",
  "ocarina",
  "lead 1 (square)",
  "lead 2 (sawtooth)",
  "lead 3 (calliope)",
  "lead 4 (chiff)",
  "lead 5 (charang)",
  "lead 6 (voice)",
  "lead 7 (fifths)",
  "lead 8 (bass+lead)",
  "pad 1 (new age)",
  "pad 2 (warm)",
  "pad 3 (polysynth)",
  "pad 4 (choir)",
  "pad 5 (bowed)",
  "pad 6 (metallic)",
  "pad 7 (halo)",
  "pad 8 (sweep)",
  "fx 1 (rain)",
  "fx 2 (soundtrack)",
  "fx 3 (crystal)",
  "fx 4 (atmosphere)",
  "fx 5 (brightness)",
  "fx 6 (goblins)",
  "fx 7 (echoes)",
  "fx 8 (sci-fi)",
  "sitar",
  "banjo",
  "shamisen",
  "koto",
  "kalimba",
  "bagpipe",
  "fiddle",
  "shanai",
  "tinkle bell",
  "agogo",
  "steel drums",
  "woodblock",
  "taiko drum",
  "melodic tom",
  "synth drum",
  "reverse cymbal",
  "guitar fret noise",
  "breath noise",
  "seashore",
  "bird tweet",
  "telephone ring",
  "helicopter",
  "applause",
  "gunshot",
  NULL
};


static const gchar *context_strings[] = {
  NONE_STRING,
  PIANO_START_STRING,
  PIANO_END_STRING,
  CHOIR_START_STRING,
  CHOIR_END_STRING,
  GROUP_START_STRING,
  GROUP_END_STRING,
  NULL
};


/**
 *  return GString with contexts contained in the passed DenemoContext
 *  caller must free the string.
 */
static GString *context_string (DenemoContext c) {

  GString *s = g_string_new("");
#define  APPEND(A,B)  if(c & A) g_string_append_printf(s, ":%s", B)
  APPEND(DENEMO_PIANO_START, PIANO_START_STRING);
  APPEND(DENEMO_PIANO_END, PIANO_END_STRING);
  APPEND(DENEMO_CHOIR_START, CHOIR_START_STRING);
  APPEND(DENEMO_CHOIR_END, CHOIR_END_STRING);
  APPEND(DENEMO_GROUP_START, GROUP_START_STRING);
  APPEND(DENEMO_GROUP_END, GROUP_END_STRING);
  if(s->len==0)
    g_string_append(s, NONE_STRING);
  return s;
}

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
  GtkWidget *mute_volume;
 
  

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
  gint n;
  gint err;

#define ASSIGNTEXT(field) \
  if(cbdata->field)\
    g_string_assign (staffstruct->field,\
    gtk_entry_get_text (GTK_ENTRY (cbdata->field)))

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
  canonicalize_denemo_name
    ((gchar *) gtk_entry_get_text (GTK_ENTRY (cbdata->denemo_name)),
     staffstruct->denemo_name);
  set_lily_name (staffstruct->denemo_name, staffstruct->lily_name);

  /* !!!! Insert advisory function for detecting colliding staff names
   * here */
  //g_print("first %d\t", staffstruct->space_above);
  ASSIGNNUMBER(space_above);
  //g_print("then %d\t", staffstruct->space_above);
  ASSIGNNUMBER(space_below);
  ASSIGNNUMBER(no_of_lines);
  ASSIGNNUMBER(transposition);

 
  /* set MIDI channel/prognum */
  ASSIGNTEXT(midi_instrument);
  ASSIGNTEXT(device_port);
  ASSIGNBOOLEAN(mute_volume);
  ASSIGNNUMBER(volume);
  // ASSIGNBOOLEAN(midi_prognum_override);
  if(staffstruct->midi_instrument->len) {
    staffstruct->midi_prognum = get_midi_prognum();
    gint i;
    for(i=0;instruments[i];i++) {
      if(!strcmp(instruments[i],staffstruct->midi_instrument->str)) {
	staffstruct->midi_prognum = i;
	break;
      }
    }
    if(staffstruct->midi_prognum != i)
      ASSIGNNUMBER_1(midi_prognum);
    ASSIGNNUMBER_1(midi_channel);
  } else {
    ASSIGNNUMBER_1(midi_prognum);
    ASSIGNNUMBER_1(midi_channel);
  }

#ifdef DEBUG
  g_printf("Staff Transposition %d\n", staffstruct->transposition);
#endif
  score_status(cbdata->gui, TRUE);
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
  GList *context_list = NULL;
  static struct callbackdata cbdata;
  
  if (!instrument_list)
    {
      int i=0;
      instrument_list = g_list_append (instrument_list, "");
      do {
	   instrument_list = g_list_append (instrument_list, instruments[i++]);
      } while (instruments[i]);
    }

  if (!context_list)
    {
      int i=0;
      while (context_strings[i++])
	{
	  context_list = g_list_append (context_list, (gpointer) (context_strings[i]?context_strings[i]:"None"));
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
 
  dialog = gtk_dialog_new_with_buttons (_("Staff Properties"), GTK_WINDOW(Denemo.window),
					(GtkDialogFlags)
					(GTK_DIALOG_MODAL |
					 GTK_DIALOG_DESTROY_WITH_PARENT),
					GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
					GTK_STOCK_CANCEL, GTK_STOCK_CANCEL,
					NULL);
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
  GtkWidget *field;\
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
  label = gtk_label_new (thelabel);\
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
  label = gtk_label_new (thelabel);\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  field = gtk_spin_button_new_with_range (min, max, 1.0);\
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (field), staffstruct->field+1);\
  gtk_box_pack_start (GTK_BOX (hbox), field, FALSE, FALSE, 0);\
  cbdata.field = field;

#define BOOLEANENTRY(thelabel, field) \
  GtkWidget *field;\
  field =\
    gtk_check_button_new_with_label (thelabel); \
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
  gint i;\
  for(i=0;i<G_N_ELEMENTS(thelist);i++)\
    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT(field), thelist[i]);\
  //gtk_entry_set_text (GTK_ENTRY (GTK_COMBO (field)->entry),\
//		  setstring->str);\
  gtk_container_add (GTK_CONTAINER(hbox), field);	\
  cbdata.field = GTK_COMBO (field)->entry;
#else
 #define COMBOBOXENTRY(thelabel, field, thelist, setstring) \
  GtkWidget *field;\
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_container_add (GTK_CONTAINER(main_vbox), hbox);	\
  label = gtk_label_new (_(thelabel));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_container_add (GTK_CONTAINER(hbox), label);\
  field = gtk_combo_new ();\
  gint i;\
  for(i=0;i<G_N_ELEMENTS(thelist);i++)\
    gtk_entry_set_text (GTK_ENTRY (GTK_COMBO (field)->entry), setstring->str);\
  gtk_container_add (GTK_CONTAINER(hbox), field);	\
  cbdata.field = GTK_COMBO (field)->entry;
#endif
  /* Display appearance tab */
  NEWPAGE("Display Appearance");
  TEXTENTRY("Staff name:", denemo_name);
   //gtk_widget_grab_focus (entrywidget);
  INTENTRY_LIMITS("Space above:", space_above, 0, MAXEXTRASPACE);
  INTENTRY_LIMITS("Space below:", space_below, 0, MAXEXTRASPACE); 
  INTENTRY_LIMITS("Number of Lines:", no_of_lines, 1, 5);

  
  /*print appearance tab */
  // NEWPAGE("Printout Appearance");

  
  //GString *s = context_string(staffstruct->context);
  // g_print("\ncontext string = %s\n",s->str);
  //  COMBOBOXENTRY("Context:", context, context_list, s);
  // g_string_free(s, TRUE); 
 
  /* MIDI tab */
  NEWPAGE("MIDI");
  COMBOBOXENTRY("MIDI Instrument:", midi_instrument, instruments, staffstruct->midi_instrument);
  INTENTRY_LIMITS("Transposition:", transposition, -30, 30);
  BOOLEANENTRY("Mute", mute_volume);
  INTENTRY_LIMITS("Volume:", volume, 0, 127);
  // BOOLEANENTRY("Override MIDI Channel/Program", midi_prognum_override);  
  INTENTRY_LIMITS_1("Channel:", midi_channel, 1, 16);
  INTENTRY_LIMITS_1("Program:", midi_prognum, 1, 128);
  g_print("chan prog %d %d\n", staffstruct->midi_channel, staffstruct->midi_prognum); 

  GList *md = device_manager_DevicePort_list();
  if(md) {
#ifdef _HAVE_JACK_
    COMBOBOXENTRY("Midi Devices", device_port, md, staffstruct->device_port);
#endif
  }
  else
    cbdata.device_port = NULL;
  /* Set up the callback data */
#define SETCALLBACKDATA(field) \
    cbdata.field = field;

  SETCALLBACKDATA(gui);
  SETCALLBACKDATA(staffstruct);
  
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

  gtk_widget_show_all(dialog);
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      set_properties (&cbdata);
      result = TRUE;
    }
  gtk_widget_destroy (dialog);

  return result;
}


void staff_properties_change_cb (GtkAction *action, DenemoScriptParam * param) {
  GET_4PARAMS(action, param, denemo_name, device_port, midi_prognum, midi_channel);
  DenemoStaff *staff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;

 if(query) {
   if(*query) if(!strcmp("denemo_name", query)) {
     g_string_assign(param->string, staff->denemo_name->str);
     param->status = TRUE;
   }
   if(*query) if(!strcmp("transposition", query)) {
       g_string_printf(param->string, "%d", staff->transposition);
     param->status = TRUE;
   }
   return;
 }
 take_snapshot();

 if(denemo_name) {
    g_string_assign(staff->denemo_name, denemo_name);
    canonicalize_denemo_name (denemo_name, staff->denemo_name);
    set_lily_name (staff->denemo_name, staff->lily_name);
    param->status = TRUE;
   return;
 }
 if(device_port) {
   g_string_assign(staff->device_port, device_port);
    param->status = TRUE;
    return;
 }
  
 if(midi_prognum) {
   staff->midi_prognum = atoi(midi_prognum);
   return;
 }
 if(midi_channel) {
   staff->midi_channel = atoi(midi_channel);
   return;
 }
 (void) staff_properties_change();
}
