/* staffpropdialog.cpp
 * callback that creates a "Staff Properties" dialog box asking
 * the user to change the properties of the current staff
 
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller */

#include <gtk/gtk.h>
#include "calculatepositions.h"
#include "chordops.h"
#include "contexts.h"
#include <denemo/denemo.h>
#include "processstaffname.h"
#include "staffops.h"
#include "utils.h"
#include <stdlib.h>
#include <string.h>
#include "xmldefs.h"
/**
 * List of midi instrument names
 * 
 */
static gchar *instruments[128] = {
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
  "orchestral strings",
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
  "gunshot"
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
  GtkWidget *nameentry;
  GtkWidget *midi_prognum;
  GtkWidget *midi_channel;
  GtkWidget *midi_prognum_override;
  GtkWidget *aboveentry;
  GtkWidget *belowentry;
  GtkWidget *numlinesentry;
  GtkWidget *transposeentry;
  GtkWidget *volume;
  GtkWidget *posinhalflinesentry;
  GtkWidget *midientry;
  GtkWidget *contexts;
  GtkWidget *lilybefore;

  
};

/**
 * Convert Context string to denemocontext 
 * 
 * @param string the context string
 * @return denemocontext of the string or -1 if not passed string is not set to a context
 */
static gint
setcontext (const gchar * string)
{
  if (!strcmp (NONE_STRING, string))
    return DENEMO_NONE;
  if (!strcmp (PIANO_START_STRING, string))
    return DENEMO_PIANO_START;
  if (!strcmp (PIANO_END_STRING, string))
    return DENEMO_PIANO_END;
  if (!strcmp (CHOIR_START_STRING, string))
    return DENEMO_CHOIR_START;
  if (!strcmp (CHOIR_END_STRING, string))
    return DENEMO_CHOIR_END;
  if (!strcmp (GROUP_START_STRING, string))
    return DENEMO_GROUP_START;
  if (!strcmp (GROUP_END_STRING, string))
    return DENEMO_GROUP_END;
  return -1;
}

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

  canonicalize_denemo_name
    ((gchar *) gtk_entry_get_text (GTK_ENTRY (cbdata->nameentry)),
     staffstruct->denemo_name);
  set_lily_name (staffstruct->denemo_name, staffstruct->lily_name);
  
  /* set channel/prognum */
  staffstruct->midi_prognum_override = (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->midi_prognum_override)));
  staffstruct->midi_prognum = (guint8) atoi (gtk_entry_get_text (GTK_ENTRY (cbdata->midi_prognum)));
  staffstruct->midi_channel = (guint8) atoi (gtk_entry_get_text (GTK_ENTRY (cbdata->midi_channel)));
  
  /* !!!! Insert advisory function for detecting colliding staff names
   * here */

  g_string_assign (staffstruct->midi_instrument,
		   gtk_entry_get_text (GTK_ENTRY (cbdata->midientry)));
  if ((n = atoi (gtk_entry_get_text (GTK_ENTRY (cbdata->aboveentry)))) >= 0)
    staffstruct->space_above = n;
  if ((n = atoi (gtk_entry_get_text (GTK_ENTRY (cbdata->belowentry)))) >= 0)
    staffstruct->space_below = n;
  if ((n = atoi (gtk_entry_get_text (GTK_ENTRY (cbdata->numlinesentry)))))
    staffstruct->no_of_lines = n;
  staffstruct->transposition = atoi (gtk_entry_get_text (GTK_ENTRY (cbdata->transposeentry)));
  if ((n =
       atoi (gtk_entry_get_text (GTK_ENTRY (cbdata->posinhalflinesentry)))))
    staffstruct->pos_in_half_lines = n;
  if ((n = atoi (gtk_entry_get_text (GTK_ENTRY (cbdata->volume)))))
    staffstruct->volume = n;
  DenemoContext old = staffstruct->context;
  staffstruct->context =
    setcontext (gtk_entry_get_text (GTK_ENTRY (cbdata->contexts)));
  if(staffstruct->context==-1)
    staffstruct->context=old;
  else
    if(staffstruct->context != DENEMO_NONE)
      staffstruct->context |= old;//Allow more than one context to start/end, reset using DENEMO_NONE (0)
  if(staffstruct->staff_prolog) 
    g_string_free(staffstruct->staff_prolog, TRUE);
  staffstruct->staff_prolog =
    g_string_new(gtk_entry_get_text (GTK_ENTRY (cbdata->lilybefore)));

#ifdef DEBUG
	g_printf("Staff Transposition %d\n", staffstruct->transposition);
#endif
	//	if(cbdata->gui) must update changecount
	  score_status(cbdata->gui, TRUE);
}

/**
 * Create Dialog to allow the user to set the staffs parameters
 * 
 * @param action Gtk Action event or NULL if called programatically
 * @param callback_data pointer either the newstaffinfotopass structure or (if action) the DenemoGUI structure
 * @return success or failure 
 */
gboolean
staff_properties_change (GtkAction * action, gpointer callback_data)
{
  DenemoScore *si;
  DenemoGUI *gui;
  DenemoStaff *thestaff;
  guint callback_action = 0;
  gboolean result = FALSE;
  DenemoStaff *staffstruct;
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *table;
  GtkWidget *nameentry;
  GtkWidget *aboveentry;
  GtkWidget *belowentry;
  GtkWidget *numlinesentry;
  GtkWidget *transposeentry;
  GtkWidget *posinhalflinesentry;
  GtkWidget *midi_prognum_override;
  GtkWidget *midi_channel;
  GtkWidget *midi_prognum;
  GtkWidget *volume;
  GtkWidget *lilybefore;

  GtkWidget *midicombo;
  GtkWidget *context;
  static GString *entrycontent;
  GList *instrument_list = NULL;
  GList *context_list = NULL;
  static struct callbackdata cbdata;

  if (action)
    {
      gui = (DenemoGUI *) callback_data;
      si = gui->si;
      staffstruct = (DenemoStaff *) si->currentstaff->data;
      if(action && staffstruct->staff_prolog && staffstruct->staff_prolog->len) {
	warningdialog("This staff has a custom prolog for the staff.\n"
		      "You will need to make your edits in the LilyPond window\n"
		      "to see them in the print-out.");
      }
    }
  else
    {
      struct newstaffinfotopass *cbdata1 =
	(struct newstaffinfotopass *) callback_data;
      /* si =  cbdata1->si; */
      gui = cbdata1->gui;
      staffstruct = cbdata1->staff;
      callback_action = cbdata1->addat;
    }





  if (!entrycontent)
    {
      entrycontent = g_string_new (NULL);
    }

  dialog = gtk_dialog_new_with_buttons (_("Staff Properties"), NULL,	/* no parent window (should be gui->window but not always passing that here */
					(GtkDialogFlags)
					(GTK_DIALOG_MODAL |
					 GTK_DIALOG_DESTROY_WITH_PARENT),
					GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
					GTK_STOCK_CANCEL, GTK_STOCK_CANCEL,
					NULL);

  table = gtk_table_new (4, 8, FALSE);
  gtk_container_set_border_width (GTK_CONTAINER (table), 12);
  gtk_table_set_row_spacings (GTK_TABLE (table), 8);
  gtk_table_set_col_spacings (GTK_TABLE (table), 8);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), table,
		      TRUE, TRUE, 0);

  label = gtk_label_new (_("Staff name:"));
  //gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 0, 1,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (label);

  nameentry = gtk_entry_new ();
  gtk_entry_set_text (GTK_ENTRY (nameentry), staffstruct->denemo_name->str);
  gtk_table_attach (GTK_TABLE (table), nameentry, 1, 2, 0, 1,
		    (GtkAttachOptions) (GTK_FILL | GTK_EXPAND),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (nameentry);

  label = gtk_label_new (_("Space above:"));
  //gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 1, 2,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (label);

  aboveentry = gtk_entry_new ();
  g_string_sprintf (entrycontent, "%i", staffstruct->space_above);
  gtk_entry_set_text (GTK_ENTRY (aboveentry), entrycontent->str);
  gtk_table_attach (GTK_TABLE (table), aboveentry, 1, 2, 1, 2,
		    (GtkAttachOptions) (GTK_FILL | GTK_EXPAND),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (aboveentry);

  label = gtk_label_new (_("Space below:"));
  //gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 2, 3,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (label);
  belowentry = gtk_entry_new ();
  g_string_sprintf (entrycontent, "%i", staffstruct->space_below);
  gtk_entry_set_text (GTK_ENTRY (belowentry), entrycontent->str);
  gtk_table_attach (GTK_TABLE (table), belowentry, 1, 2, 2, 3,
		    (GtkAttachOptions) (GTK_FILL | GTK_EXPAND),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (belowentry);

  label = gtk_label_new (_("Number of lines:"));
  gtk_table_attach (GTK_TABLE (table), label, 2, 3, 0, 1,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (label);
  numlinesentry = gtk_entry_new ();
  g_string_sprintf (entrycontent, "%i", staffstruct->no_of_lines);
  gtk_entry_set_text (GTK_ENTRY (numlinesentry), entrycontent->str);
  gtk_table_attach (GTK_TABLE (table), numlinesentry, 3, 4, 0, 1,
		    (GtkAttachOptions) (GTK_FILL | GTK_EXPAND),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (numlinesentry);

  label = gtk_label_new (_("Transposition:"));
  gtk_table_attach (GTK_TABLE (table), label, 2, 3, 1, 2,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (label);
  transposeentry = gtk_entry_new ();
  g_string_sprintf (entrycontent, "%i", staffstruct->transposition);
#ifdef DEBUG
	g_printf("Staff Transposition %i\n", staffstruct->transposition);
#endif
  gtk_entry_set_text (GTK_ENTRY (transposeentry), entrycontent->str);
  gtk_table_attach (GTK_TABLE (table), transposeentry, 3, 4, 1, 2,
		    (GtkAttachOptions) (GTK_FILL | GTK_EXPAND),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (transposeentry);

  label = gtk_label_new (_("Position in half-lines:"));
  gtk_table_attach (GTK_TABLE (table), label, 2, 3, 2, 3,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (label);
  posinhalflinesentry = gtk_entry_new ();
  g_string_sprintf (entrycontent, "%i", staffstruct->pos_in_half_lines);
  gtk_entry_set_text (GTK_ENTRY (posinhalflinesentry), entrycontent->str);
  gtk_table_attach (GTK_TABLE (table), posinhalflinesentry,
		    3, 4, 2, 3, (GtkAttachOptions) (GTK_FILL | GTK_EXPAND),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (posinhalflinesentry);

  label = gtk_label_new (_("MIDI Instrument:"));
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 3, 4,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (label);
  midicombo = gtk_combo_new ();
  if (!instrument_list)
    {
      int i;
      for (i = 0; i < 128; i++)
	{
	  instrument_list = g_list_append (instrument_list, instruments[i]);
	}
    }
  gtk_combo_set_popdown_strings (GTK_COMBO (midicombo), instrument_list);

  gtk_entry_set_text (GTK_ENTRY (GTK_COMBO (midicombo)->entry),
		      staffstruct->midi_instrument->str);
  gtk_table_attach (GTK_TABLE (table), midicombo, 1, 2, 3, 4,
		    (GtkAttachOptions) (GTK_FILL | GTK_EXPAND),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (midicombo);

  label = gtk_label_new (_("Volume:"));
  gtk_table_attach (GTK_TABLE (table), label, 2, 3, 3, 4,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (label);
  volume = gtk_entry_new ();
  g_string_sprintf (entrycontent, "%i", staffstruct->volume);
  gtk_entry_set_text (GTK_ENTRY (volume), entrycontent->str);
  gtk_table_attach (GTK_TABLE (table), volume, 3, 4, 3, 4,
		    (GtkAttachOptions) (GTK_FILL | GTK_EXPAND),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (volume);
 
  /*prognum/channel override */
  label = gtk_label_new (_("Override Midi Channel/Program"));
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 4, 5,
				(GtkAttachOptions) (GTK_FILL),
					    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (label);

  midi_prognum_override = gtk_check_button_new ();
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (midi_prognum_override), staffstruct->midi_prognum_override);
  gtk_table_attach (GTK_TABLE (table), midi_prognum_override, 1, 2, 4, 5,
				(GtkAttachOptions) (GTK_FILL),
					    (GtkAttachOptions) (0), 0, 0);

  gtk_widget_show (midi_prognum_override);

  /**/
  label = gtk_label_new (_("Channel:"));
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 5, 6,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (label);
  
  midi_channel = gtk_entry_new ();
  g_string_sprintf (entrycontent, "%i", staffstruct->midi_channel);
  gtk_entry_set_text (GTK_ENTRY (midi_channel), entrycontent->str);
  gtk_table_attach (GTK_TABLE (table), midi_channel, 1, 2, 5, 6,
		    (GtkAttachOptions) (GTK_FILL | GTK_EXPAND),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (midi_channel);

  label = gtk_label_new (_("Program:"));
  gtk_table_attach (GTK_TABLE (table), label, 2, 3, 5, 6,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (label);
  
  midi_prognum = gtk_entry_new ();
  g_string_sprintf (entrycontent, "%i", staffstruct->midi_prognum);
  gtk_entry_set_text (GTK_ENTRY (midi_prognum), entrycontent->str);
  gtk_table_attach (GTK_TABLE (table), midi_prognum, 3, 4, 5, 6,
		    (GtkAttachOptions) (GTK_FILL | GTK_EXPAND),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (midi_prognum);

  /**/

  label = gtk_label_new (_("Custom prolog:"));
  gtk_table_attach (GTK_TABLE (table), label, 2, 3, 6, 7,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (label);
  lilybefore = gtk_entry_new ();
  g_string_sprintf (entrycontent, "%s", staffstruct->staff_prolog?staffstruct->staff_prolog->str:"");
  gtk_entry_set_text (GTK_ENTRY (lilybefore), entrycontent->str);
  gtk_table_attach (GTK_TABLE (table), lilybefore, 3, 4, 6, 7,
		    (GtkAttachOptions) (GTK_FILL | GTK_EXPAND),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (lilybefore);

  label = gtk_label_new (_("Context:"));
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 6, 7,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (label);
  context = gtk_combo_new ();
  if (!context_list)
    {
      int i=0;
      while (context_strings[i++])
	{
	  context_list = g_list_append (context_list, (gpointer) (context_strings[i]?context_strings[i]:"None"));
	}
    }
  gtk_combo_set_popdown_strings (GTK_COMBO (context), context_list);
  GString *s = context_string(staffstruct->context);
  gtk_entry_set_text (GTK_ENTRY (GTK_COMBO (context)->entry),
		      s->str);
  g_string_free(s, TRUE);
  gtk_table_attach (GTK_TABLE (table), context, 1, 2, 6, 7,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_show (context);
  gtk_widget_show (table);

  /* Set up the callback data */

  cbdata.gui = gui;
  cbdata.staffstruct = staffstruct;
  cbdata.nameentry = nameentry;
  cbdata.midi_prognum_override = midi_prognum_override;//prognum_override;
  cbdata.midi_prognum = midi_prognum;
  cbdata.midi_channel = midi_channel;
  cbdata.aboveentry = aboveentry;
  cbdata.belowentry = belowentry;
  cbdata.numlinesentry = numlinesentry;
  cbdata.transposeentry = transposeentry;
  cbdata.posinhalflinesentry = posinhalflinesentry;
  cbdata.volume = volume;
  cbdata.lilybefore = lilybefore;

  cbdata.midientry = GTK_COMBO (midicombo)->entry;
  cbdata.contexts = GTK_COMBO (context)->entry;
  /* Also set things up so that the callback'll run when you hit enter
   * in the text entries */
  gtk_entry_set_activates_default (GTK_ENTRY (nameentry), TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY (aboveentry), TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY (belowentry), TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY (numlinesentry), TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY (transposeentry), TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY (posinhalflinesentry), TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY (GTK_COMBO (midicombo)->entry),
				   TRUE);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);

  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_widget_show (dialog);
  gtk_widget_grab_focus (nameentry);
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      set_properties (&cbdata);
      result = TRUE;

    }
  gtk_widget_destroy (dialog);

  return result;
}
