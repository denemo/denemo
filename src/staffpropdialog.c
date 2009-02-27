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
#include "dialogs.h"
#include <stdlib.h>
#include <string.h>
#include "xmldefs.h"
/**
 * List of midi instrument names
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
  GtkWidget *space_above;
  GtkWidget *space_below;
  GtkWidget *no_of_lines;
  GtkWidget *transposition;
  GtkWidget *volume;
  GtkWidget *mute_volume;
  GtkWidget *pos_in_half_lines;
  GtkWidget *context;
  GtkWidget *staff_prolog_insert;
  GtkWidget *voice_prolog_insert;
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

#define ASSIGNTEXT(field) \
    g_string_assign (staffstruct->field,\
    gtk_entry_get_text (GTK_ENTRY (cbdata->field)))

#define ASSIGNNUMBER(field) \
    staffstruct->field = \
      atoi(gtk_entry_get_text(GTK_ENTRY (cbdata->field)))

#define ASSIGNBOOLEAN(field) \
    staffstruct->field = \
    (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->field)));

  canonicalize_denemo_name
    ((gchar *) gtk_entry_get_text (GTK_ENTRY (cbdata->denemo_name)),
     staffstruct->denemo_name);
  set_lily_name (staffstruct->denemo_name, staffstruct->lily_name);
    
  /* !!!! Insert advisory function for detecting colliding staff names
   * here */

  ASSIGNNUMBER(space_above);
  ASSIGNNUMBER(space_below);
  ASSIGNNUMBER(no_of_lines);
  ASSIGNNUMBER(transposition);
  ASSIGNNUMBER(pos_in_half_lines);
 
  /* set midi channel/prognum */
  ASSIGNTEXT(midi_instrument);
  ASSIGNBOOLEAN(mute_volume);
  ASSIGNNUMBER(volume);
  ASSIGNBOOLEAN(midi_prognum_override);
  ASSIGNNUMBER(midi_prognum);
  ASSIGNNUMBER(midi_channel);

  /* staff context */
  DenemoContext old = staffstruct->context;
  staffstruct->context =
    setcontext (gtk_entry_get_text (GTK_ENTRY (cbdata->context)));
  if(staffstruct->context==-1)
    staffstruct->context=old;
  else
    if(staffstruct->context != DENEMO_NONE)
      staffstruct->context |= old;//Allow more than one context to start/end, reset using DENEMO_NONE (0)
  
  /* staff prolog insert */
  if(staffstruct->staff_prolog_insert) 
    g_string_free(staffstruct->staff_prolog_insert, TRUE);
  staffstruct->staff_prolog_insert =
    g_string_new(gtk_entry_get_text (GTK_ENTRY (cbdata->staff_prolog_insert))); 
  /* voice prolog insert */
  if(staffstruct->voice_prolog_insert) 
    g_string_free(staffstruct->voice_prolog_insert, TRUE);
  staffstruct->voice_prolog_insert =
    g_string_new(gtk_entry_get_text (GTK_ENTRY (cbdata->voice_prolog_insert))); 

#ifdef DEBUG
  g_printf("Staff Transposition %d\n", staffstruct->transposition);
#endif
  score_status(cbdata->gui, TRUE);
}

void staff_properties_change_cb (GtkAction *action, gpointer param) {
  if(action)
    (void) staff_properties_change(NULL);
  else if(param==NULL)
    (void) staff_properties_change(NULL);
  else {
    GString *values = (GString *)param;
    gchar *str;
    DenemoStaff *staff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
#define SET_STRING(a, b)     if( (str = g_strstr_len(values->str+i,strlen(values->str+i), a))) {\
      if(staff->b)\
	g_string_free(staff->b, TRUE);\
      staff->b = g_string_new(str+strlen(a)+1);\
    }
    gint i;
    for(i=0;i<values->len;i+=strlen(values->str+i)+1) {
      SET_STRING("staff-prolog-insert", staff_prolog_insert); 
      SET_STRING("voice-prolog-insert", voice_prolog_insert);
    // others ....
    }
#undef SET_STRING
  }
}


/**
 * Create Dialog to allow the user to set the staffs parameters
 * 
 * @param action Gtk Action event or NULL if called programatically
 * @param callback_data if action==NULL this holds newstaffinfotopass structure otherwise unused.
 * @return success or failure 
 */
gboolean
staff_properties_change (gpointer callback_data)
{
  DenemoScore *si;
  DenemoGUI *gui;
  DenemoStaff *thestaff;
  guint callback_action = 0;
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

  if (callback_data==NULL)
    {
      gui = Denemo.gui;
      si = gui->si;
      staffstruct = (DenemoStaff *) si->currentstaff->data;
      if(staffstruct->staff_prolog && staffstruct->staff_prolog->len) {
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
 
  gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);
  notebook = gtk_notebook_new ();
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), notebook, TRUE,
		                            TRUE, 0);
 
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

#define BOOLEANENTRY(thelabel, field) \
  GtkWidget *field;\
  field =\
    gtk_check_button_new_with_label (thelabel); \
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (field),\
		                                    staffstruct->field);\
    gtk_box_pack_start (GTK_BOX (main_vbox), field, FALSE, TRUE, 0);\
    cbdata.field = field;

#define COMBOBOXENTRY(thelabel, field, thelist, setstring) \
  GtkWidget *field;\
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (_(thelabel));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  field = gtk_combo_new ();\
  gtk_combo_set_popdown_strings (GTK_COMBO (field), thelist);\
  gtk_entry_set_text (GTK_ENTRY (GTK_COMBO (field)->entry),\
		  setstring->str);\
  gtk_box_pack_start (GTK_BOX (hbox), field, FALSE, FALSE, 0);\
  cbdata.field = GTK_COMBO (field)->entry;

  /* appearance tab */
  NEWPAGE("Appearance");
  TEXTENTRY("Staff name:", denemo_name);
  INTENTRY_LIMITS("Space above:", space_above, 0, 30);
  INTENTRY_LIMITS("Space below:", space_below, 0, 30); 
  INTENTRY_LIMITS("Number of Lines:", no_of_lines, 1, 5);
  INTENTRY_LIMITS("Transposition:", transposition, -30, 30);
  INTENTRY_LIMITS("Position in half-lines:", pos_in_half_lines, -5, 5);
  
  TEXTENTRY("Staff prolog:", staff_prolog_insert); 
  TEXTENTRY("Voice prolog:", voice_prolog_insert);
  
  GString *s = context_string(staffstruct->context);
  g_print("\ncontext string = %s\n",s->str);
  COMBOBOXENTRY("Context:", context, context_list, s);
  g_string_free(s, TRUE); 
 
  /* midi tab */
  NEWPAGE("midi");
  COMBOBOXENTRY("Midi Instrument:", midi_instrument, instrument_list, staffstruct->midi_instrument);
  BOOLEANENTRY("Mute", mute_volume);
  INTENTRY_LIMITS("Volume:", volume, 0, 127);
  BOOLEANENTRY("Override Midi Channel/Program", midi_prognum_override);  
  INTENTRY_LIMITS("Channel:", midi_channel, 0, 15);
  INTENTRY_LIMITS("Program:", midi_prognum, 0, 127);
  
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
  gtk_entry_set_activates_default (GTK_ENTRY (pos_in_half_lines), TRUE);
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
