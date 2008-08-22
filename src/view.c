/* view.c
 * Functions to create a top level Denemo window
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2003-2005  Adam Tee
 * 
 */
#include <string.h>
#include "view.h"
#include "bookmarks.h"
#include "lyparserfuncs.h"
#include "lilydirectives.h"
#include "dialogs.h"
#include "utils.h"
#include <stdlib.h>
#include "scorewizard.h"
#include "playback.h"
#include "alsaplayback.h"
#include "midiseq.h"
#include "pitchentry.h"
#include "exportlilypond.h"
#include "transpose.h"
#include "print.h"
#include "kbd-custom.h"
#if GTK_MAJOR_VERSION > 1
#include <gtk/gtkaccelgroup.h>
#endif

static void
closewrapper (GtkAction *action);
static void
openinnew (GtkAction *action);
static void 
create_rhythm_cb (GtkAction* action);
static void
delete_rhythm_cb (GtkAction * action);
static void
toggle_edit_mode (GtkAction * action);
static void
toggle_rest_mode (GtkAction * action);
static void
toggle_rhythm_mode (GtkAction * action);

#define MUSIC_FONT(a) "<span  size=\"10000\" face=\"Denemo\">"a"</span>"





extern midi_seq *sq;		/* global denemo sequencer FIXME: should not be global */

static void   activate_action(gchar *path);
typedef enum 
{
  ACCELS_LOADED = 0x0,
  ACCELS_CHANGED = 0x1<<0,
  EXTRA_ACCELS_ACTIVE = 0x1<<1,
  ACCELS_MAY_HAVE_CHANGED = 0x1<<2
} AccelStatus;



static void use_markup(GtkWidget *widget);
static void save_accels (void);

#include "callbacks.h" /* callback functions menuitems that can be called by scheme */
#include <libguile.h> 
#include "scheme_cb.h"


#define MODELESS_STRING "Modeless"
#define CLASSICMODE_STRING "ClassicMode"
#define INSERTMODE_STRING "InsertMode"
#define EDITMODE_STRING "EditMode"
#define NOTE_E_STRING "Note"
#define REST_E_STRING "Rest"
#define BLANK_E_STRING "Blank"
#define RHYTHM_E_STRING "Rhythm"
#define ToggleRhythmToolbar_STRING "ToggleRhythmToolbar"
#define ToggleEntryToolbar_STRING  "ToggleEntryToolbar"
#define ToggleActionMenu_STRING  "ToggleActionMenu"
#define ToggleObjectMenu_STRING  "ToggleObjectMenu"
#define ToggleLilyText_STRING  "ToggleLilyText"
#define ToggleScript_STRING  "ToggleScript"
#define TogglePitchRecognition_STRING  "TogglePitchRecognition"
#define ToggleArticulationPalette_STRING  "ToggleArticulationPalette"
#define QuickEdits_STRING  "QuickEdits"
#define ReadOnly_STRING  "ReadOnly"

#define FN_DEF(X) X##_CB(void) {\
activate_action("/MainMenu/EntryMenu/"X##_STRING);}

 FN_DEF(MODELESS);
 FN_DEF(CLASSICMODE);
 FN_DEF(INSERTMODE);
 FN_DEF(EDITMODE);
 FN_DEF(NOTE_E);
 FN_DEF(REST_E);
 FN_DEF(BLANK_E);
 FN_DEF(RHYTHM_E);

//void MODELESS_CB(void) {
//      activate_action( "/MainMenu/EntryMenu/"MODELESS_STRING);
//}

typedef struct cb_string_pairs  { gpointer p; gchar *str;} cb_string_pairs;
cb_string_pairs activatable_commands[] = {
  {MODELESS_CB, MODELESS_STRING},
  {CLASSICMODE_CB, CLASSICMODE_STRING},
  {INSERTMODE_CB, INSERTMODE_STRING},
  {EDITMODE_CB, EDITMODE_STRING},
  {NOTE_E_CB, NOTE_E_STRING},
  {REST_E_CB, REST_E_STRING},
  {BLANK_E_CB, BLANK_E_STRING},
  {RHYTHM_E_CB, RHYTHM_E_STRING}

};


#ifdef TRIAL_SCHEME
/* trial */
void denemoy(char *action_name) {
  g_print("doing %s\n", action_name);
  g_print("====================== %p %p\n", &Denemo, Denemo.ui_manager); 
  activate_action(action_name);
}
SCM scheme_denemoy(SCM val) {
  gint len;
  char *str = (char*)gh_scm2newstr(val, &len);
  denemoy(str);//free(str);
  return SCM_EOL;
}
#endif


int inner_main(void){
  /* create scheme identifiers for all the menuitem callbacks that are not check/radio items */
#include "scheme.h"
  /* create scheme identifiers for check/radio item to activate the items (ie not just run the callback) */
  gint i;
  for(i=0;i<G_N_ELEMENTS(activatable_commands);i++)
  scm_c_define_gsubr (activatable_commands[i].str, 0, 0, 0, activatable_commands[i].p);

#ifdef TRIAL_SCHEME
  scm_c_define_gsubr ("denemoy", 1, 0, 0, scheme_denemoy);
#endif
  gtk_main();
  return 0;
}
//#include <guile/gh.h>
int call_out_to_guile(char *script) {
  // char * SCM_STRING_CHARS (SCM x)
  // SCM val = scm_take_locale_string(script); this will free script
  // SCM val = scm_from_locale_string(script);
  (void)scm_c_eval_string(script);
}

/****************
 * write the status bar

********************/

void write_status(DenemoGUI *gui) {
  gchar *selection;
  if(gui->si==NULL)
    return;
  if(gui->si->currentobject && gui->si->currentobject->data ) {
    DenemoObject *curObj = gui->si->currentobject->data;
    switch(curObj->type) {
    case CHORD: {
      chord *thechord =  ((chord *) curObj->object);
      selection = g_strdup_printf("%s%s%s%s%s%s%s%s",
				  thechord->notes?
				  (g_list_length(thechord->notes)>1?"Chord ":"Note "):"Rest ",				  
				  thechord->slur_begin_p?", begin slur":"",
				  thechord->slur_end_p?", end slur":"",
				  thechord->is_tied?", tied":"",
				  thechord->crescendo_begin_p?", begin cresc.":"",
				  thechord->crescendo_end_p?", end cresc.":"",
				  thechord->diminuendo_begin_p?", begin dim.":"",
				  thechord->diminuendo_end_p?", end dim.":"",
				  thechord->is_grace?", grace note":""
				  );
      if(thechord->notes){
	GList *g;
	for(g= thechord->notes;g;g=g->next) {
	  note *thenote = (note *) g->data;
	  if(thenote->directive && thenote->directive->len) {
	    gchar *old = selection;
	    selection = g_strdup_printf("%s: %.50s",selection, thenote->directive->str);
	    g_free(old);
	  }
	}
      }
    }
      break;

    case TUPOPEN:
      selection = g_strdup_printf("Tuplet %d/%d", 	((tupopen *) curObj->object)->numerator,
				  ((tupopen *) curObj->object)->denominator);
      break;
    case TUPCLOSE: 
      selection = g_strdup_printf("End tuplet");
      break;
    case CLEF:
      selection = g_strdup_printf("clef change");
      break;
    case TIMESIG:
      selection = g_strdup_printf("time signature change");
      break;
    case KEYSIG:
      selection = g_strdup_printf("key signature change");
      break;
    case BARLINE:
      selection = g_strdup_printf("special barline type %d", 	((barline *) curObj->object)->type);// FIXME names for these
      break;
    case STEMDIRECTIVE:
      selection = g_strdup_printf("stem directive: %s",((stemdirective *) curObj->object)->type==DENEMO_STEMDOWN?
				  "stem down":((stemdirective *) curObj->object)->type==DENEMO_STEMUP?"stem up":
				  "normal stemming");
      break;
    case MEASUREBREAK:
      selection = g_strdup_printf("measurebreak");
      break;
    case STAFFBREAK:
      selection = g_strdup_printf("staffbreak");
      break;
    case DYNAMIC:
      selection = g_strdup_printf("Dynamic: %s", ((dynamic *) curObj->object)->type->str  );
      break;
    case GRACE_START:
      selection = g_strdup_printf("Grace note: %s duration %d ", ((grace *) curObj->object)->on_beat?"On beat":"Before beat",
				  ((grace *) curObj->object)->duration);
      break;
    case GRACE_END:
      selection = g_strdup_printf("Grace note end");
      break;
    case LYRIC:
      selection = g_strdup_printf("Lyric: %s",  ((lyric *) curObj->object)->lyrics->str  );
      break;
    case FIGURE:
      selection = g_strdup_printf("Figure");
      break;

    case LILYDIRECTIVE:
      selection = g_strdup_printf("Lily directive: %.50s", ((GString *)((lilydirective *) curObj->object)->directive)->str);
      break;
    case FAKECHORD:
      selection = g_strdup_printf("Fakechord"   );
      break;
    case PARTIAL:
      selection = g_strdup_printf("Partial"   );
      break;
    default:
      selection = g_strdup_printf("Cursor on a unknown object");
    }
  } else
    selection = g_strdup_printf("Cursor not on any object");

  gchar *status;
  if(gui->si->headerinfo.subtitle->len)
    status = gui->si->headerinfo.subtitle->str;
  else if(gui->si->headerinfo.piece->len)
    status = gui->si->headerinfo.piece->str;
  else
    status = "";
  status = g_strdup_printf("%s: %s", status, selection);

  g_free(selection);
  gtk_statusbar_pop(GTK_STATUSBAR (Denemo.statusbar), Denemo.status_context_id);
  gtk_statusbar_push(GTK_STATUSBAR (Denemo.statusbar), Denemo.status_context_id,
		     status);
  g_free(status);

  gtk_widget_queue_draw (gui->scorearea);
}



/**
 * close the application and shut down the sequencer if open
 *
 */
static void
quit (void)
{
#ifdef HAVEALSA
  midi_seq_delete (sq);
#endif
  gtk_main_quit ();
}


/**
 * Close the movement gui, releasing its memory and removing it from the list
 * Do not close the sequencer
 */
static void
close_gui (DenemoGUI *gui)
{
  if(Denemo.autosaveid) {
    if(g_list_length(Denemo.guis)>1)
      g_print("Auto save being turned off");
    g_source_remove(Denemo.autosaveid);
    Denemo.autosaveid = 0;
  }

  storeWindowState ();
 //stop_pitch_recognition();
  free_gui(gui);
  
  gtk_widget_destroy (gui->page);
  Denemo.guis = g_list_remove (Denemo.guis, gui);//FIXME ?? or in the destroy callback??
  g_free (gui);
}

/* remove all the music data from a gui */
void free_gui(DenemoGUI *gui)
{
  GList *g;
  for(g=gui->movements;g;g=g->next) {
    gui->si = g->data;
    free_score(gui);
  }
  g_list_free(gui->movements);
  gui->movements = NULL;
  if(gui->custom_scoreblocks) {
    GList *custom;
    for(custom=gui->custom_scoreblocks;custom;custom=custom->next) {
      g_string_free((GString*)(((DenemoScoreblock*)custom->data)->scoreblock), TRUE);
    }
    g_list_free(gui->custom_scoreblocks);
    gui->custom_scoreblocks=NULL;
  }
  nullify_gstring(&gui->custom_prolog);

      /* any other free/initializations */
}

/**
* Wrapper function to close application when the quit
* menu item has been used
* 
*
*/
static void
closewrapper (GtkAction *action)
{
  GList *display = NULL;
  //stop_pitch_recognition();
  if(Denemo.accelerator_status&ACCELS_CHANGED) {
    if(confirm("You have changed the keyboard accelerators","Do you want to save the changes?"))
      save_accels();
  } else if(Denemo.accelerator_status&ACCELS_MAY_HAVE_CHANGED) {
    if(confirm("You may have changed the keyboard accelerators","Do you want to save any changes?"))
      save_accels();
  }
  for (display = Denemo.guis; display != NULL;
       display = g_list_next (display))
    {
     
     Denemo.gui = (DenemoGUI *) display->data;
     if(close_gui_with_check (NULL) == FALSE)
       break;
  }
}

/**
 * callback from deleting window belonging to gui:
 * close window if check for unsaved data succeeds.
 * 
 */

static gboolean
delete_callback (GtkWidget * widget, GdkEvent * event)
{

  close_gui_with_check (NULL);
  return TRUE;
}


/**
 * Open in New Window callback 
 * Creates new view then opens file in the view
 */
static void
openinnew (GtkAction *action)
{
  newview (NULL);
  file_open_with_check (NULL);
}


/**
 * Close callback 
 * if user confirms close the current gui
 * if it is the last close the application.
 * return FALSE if gui was not closed, else TRUE
 */
gboolean
close_gui_with_check (GtkAction * action)
{
  DenemoGUI *gui = Denemo.gui;
  if ((!gui->changecount) || (gui->changecount && confirmbox (gui)))
    close_gui (gui);
  else 
    return FALSE;
  if(Denemo.guis==NULL) {
    quit (); 
    writeHistory ();
    ext_quit (); /* clean players pidfiles (see external.c) */
  } else {
    Denemo.gui = Denemo.guis->data;
    g_print("Setting the first piece as your score\n");
    gtk_notebook_set_current_page (GTK_NOTEBOOK(Denemo.notebook), 0);
  }
    
  return TRUE;
}


static void
singleton_callback (GtkToolButton *toolbutton, RhythmPattern *r) {
  DenemoGUI *gui = Denemo.gui;
#define CURRP ((RhythmPattern *)gui->currhythm->data)
  if(gui->currhythm && CURRP)
    unhighlight_rhythm(CURRP);
  gui->currhythm = NULL;

  gui->rstep = r->rsteps;
#define g (gui->rstep)
#define MODE (gui->mode)
  unhighlight_rhythm(gui->prevailing_rhythm);
  gui->prevailing_rhythm = r;
  highlight_rhythm(r);
  /*   if((MODE&INPUTEDIT)) */
  ((GtkFunction)(((RhythmElement*)g->data)->functions->data))(gui), displayhelper(gui); 
#undef CURRP
#undef g
#undef MODE
}

/**
 * Rhythm callback select rhythm
 * inserts the rhythm if pitchless
 */
void
select_rhythm_pattern(GtkToolButton *toolbutton, RhythmPattern *r) {
  DenemoGUI *gui = Denemo.gui;
#define CURRP ((RhythmPattern *)gui->currhythm->data)
  if(gui->currhythm && CURRP)
    unhighlight_rhythm(CURRP);
  else
    if(gui->rstep)
      unhighlight_rhythm(((RhythmElement*)gui->rstep->data)->rhythm_pattern);

  gui->currhythm = g_list_find(gui->rhythms, r);
  gui->rstep = r->rsteps;
#define g (gui->rstep)
#define MODE (gui->mode)
  if(((RhythmElement*)g->data)->icon) {
    GtkWidget *label = LABEL(CURRP->button);
    //g_print("markup is %s\n", ((RhythmElement*)g->data)->icon);
    gtk_label_set_markup(GTK_LABEL(label),((RhythmElement*)g->data)->icon);
/* #define a CURRP->button */
/*     g_print("Visible is %d %d \n", gtk_event_box_get_visible_window(gtk_tool_button_get_label_widget((a))), */
/* 	    gtk_event_box_get_above_child(gtk_tool_button_get_label_widget((a))));  */
/* #undef a */
  }
  highlight_rhythm(CURRP);
  if((MODE&INPUTEDIT))
    insert_rhythm_pattern(gui);
#undef CURRP
#undef g
#undef MODE
}

/* duration_code(gpointer function)
 * return an ascii code to indicate what duration (if any) function gives.
 * '0x0' means not a duration
 * chars 0123456 are the standard note durations
 * 
 */
gchar duration_code(gpointer fn) {
  return fn==(gpointer)insert_chord_0key ? '0':
    fn==(gpointer)insert_chord_1key ? '1':
    fn==(gpointer)insert_chord_2key ? '2':
    fn==(gpointer)insert_chord_3key ? '3':
    fn==(gpointer)insert_chord_4key ? '4':
    fn==(gpointer)insert_chord_5key ? '5':
    fn==(gpointer)insert_chord_6key ? '6':0;
}
/* modifier_code(gpointer function)
 * return an ascii code to indicate what modifier (if any) function gives.
 * '0x0' means not a valid modifier for a rhythmic duration
 * char '.' means a dotted note, '(' and ')' mean start and end slur
 * r to z are rests
 * others to be defined
 * 
 */
gchar modifier_code(gpointer fn) {
  return fn==(gpointer)start_triplet ? '~':
    fn==(gpointer)end_tuplet ? '|':
    fn==(gpointer)add_dot_key ? '.':
    fn==(gpointer)toggle_begin_slur ? '(':
    fn==(gpointer)toggle_end_slur ? ')': 
    fn==(gpointer)insert_rest_0key ? 'r':
    fn==(gpointer)insert_rest_1key ? 's':
    fn==(gpointer)insert_rest_2key ? 't':
    fn==(gpointer)insert_rest_3key ? 'u':
    fn==(gpointer)insert_rest_4key ? 'v':
    fn==(gpointer)insert_rest_5key ? 'w':
    fn==(gpointer)insert_rest_6key ? 'x':0;
}

gboolean code_is_a_duration(gchar code) {
  return code==0 || (code>='r' && code<='z');
}



/* add_to_rhythm appends to a rhythm pattern the callback function fn
   fn is a callback function
   returns TRUE if something was added
 */
static gboolean append_rhythm(RhythmPattern *r,  gpointer fn){     
	 RhythmElement *relement;
	 
	 int keyval = duration_code(fn);
	 if(keyval) {

	     relement = (RhythmElement*)g_malloc0(sizeof(RhythmElement));
	   
	   relement->functions = g_list_append(NULL, fn);

	     r->rsteps = g_list_append(r->rsteps, relement);
	     relement->rhythm_pattern = r;
	   return TRUE;
	 }
	 keyval = modifier_code(fn);
	 if(keyval) {
	   if(r->rsteps) {
	     relement = (RhythmElement *)(g_list_last(r->rsteps)->data);
	   }
	   else {
	     relement = (RhythmElement*)g_malloc0(sizeof(RhythmElement));
	   }
	   relement->functions = g_list_append(relement->functions, (gpointer)fn);
	   if(r->rsteps==NULL) {
	     r->rsteps = g_list_append(r->rsteps, relement);
	   }
	   relement->rhythm_pattern = r;
	   return TRUE;
	 }
	 return FALSE;
}


static gchar *add_to_pattern(gchar **p, gchar c) {
  gchar *temp = g_strdup_printf("%s%c", *p, c);
  g_free(*p);
  *p = temp;
}



/* create_rhythm_cb
   This is overloaded for use as a callback (ACTION is a GtkAction) and
   as a call to set up the "singleton rhythms", 
   (rhythm patterns that are just one note or rest, used for
   ordinary note entry).
   if ACTION is a GtkAction*
        create a rhythm pattern from the current selection
        the rhythm is put in gui->
        a button is created in "/RhythmToolbar"
        and the pattern is added to gui->rhythms 
         with the first step of it put in gui->rstep
   if ACTION is one of the insert_chord_xkey insert_rest_xkey)
   functions
        a button is created in the /EntryToolbar (if not alread present)
   

*/
static void 
create_rhythm_cb (GtkAction* action)     {
  DenemoGUI *gui = Denemo.gui;
  gboolean singleton = FALSE;// set TRUE if action is one of the insert_... functions.
  gboolean already_done = FALSE;// a singleton which has already been installed globally
  gboolean default_rhythm = FALSE;
  DenemoScore * si= gui->si;
  RhythmPattern *r = (RhythmPattern*)g_malloc0(sizeof(RhythmPattern));
  gchar *pattern = NULL;
    if(action ==  (gpointer)insert_chord_0key)
      pattern = g_strdup("0");
    if(action ==  (gpointer)insert_chord_1key)
      pattern = g_strdup("1");
    if(action ==  (gpointer)insert_chord_2key)
      pattern = g_strdup("2"), default_rhythm = TRUE;
    if(action ==  (gpointer)insert_chord_3key)
      pattern = g_strdup("3");
    if(action ==  (gpointer)insert_chord_4key)
      pattern = g_strdup("4");
    if(action ==  (gpointer)insert_chord_5key)
      pattern = g_strdup("5");
    if(action ==  (gpointer)insert_chord_6key)
      pattern = g_strdup("6");

    if(action ==  (gpointer)insert_rest_0key)
      pattern = g_strdup("r");
    if(action ==  (gpointer)insert_rest_1key)
      pattern = g_strdup("s");
    if(action ==  (gpointer)insert_rest_2key)
      pattern = g_strdup("t");
    if(action ==  (gpointer)insert_rest_3key)
      pattern = g_strdup("u");
    if(action ==  (gpointer)insert_rest_4key)
      pattern = g_strdup("v");
    if(action ==  (gpointer)insert_rest_5key)
      pattern = g_strdup("w");
    if(action ==  (gpointer)insert_rest_6key)
      pattern = g_strdup("x");
    if(pattern) {/* if we already have it globally we don't need it again
		    note we never delete the singleton rhythms */
      if(Denemo.singleton_rhythms[*pattern]) {
	g_free(r);
	r = Denemo.singleton_rhythms[*pattern];
	already_done = TRUE;
      }
      else {
	Denemo.singleton_rhythms[*pattern] = r;
	already_done = FALSE;
      }
      singleton=TRUE;
    }
  else
    pattern = g_strdup_printf("");
    GtkToolButton *button;
    GtkWidget *label;
    if(!already_done){
      button = (GtkToolButton *)gtk_tool_button_new(NULL, NULL);
      label = gtk_label_new(NULL);
      gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
      GtkWidget *ev = gtk_event_box_new();
      gtk_container_add (GTK_CONTAINER(ev), label);
      gtk_tool_button_set_label_widget (button, ev);
      //gtk_event_box_set_above_child (ev, TRUE);
      r->button = button;
    }
  if(!singleton) {
    staffnode *curstaff;
    measurenode *curmeasure;
    gint i = si->firststaffmarked;
    curstaff = g_list_nth (si->thescore, i - 1);
    if(curstaff && i <= si->laststaffmarked) {
      int j,k;
      objnode *curobj;
      /* Measure loop.  */
      for (j = si->firstmeasuremarked, k = si->firstobjmarked,
	     curmeasure = g_list_nth (firstmeasurenode (curstaff), j - 1);
	   curmeasure && j <= si->lastmeasuremarked;
	   curmeasure = curmeasure->next, j++)
	{
	  for (curobj = g_list_nth ((objnode *) curmeasure->data, k);
	       /* cursor_x is 0-indexed */
	       curobj && (j < si->lastmeasuremarked
			  || k <= si->lastobjmarked);
	       curobj = curobj->next, k++)
	    {
	      gpointer fn;
	      gchar *temp;
	      DenemoObject *obj = (DenemoObject *) curobj->data;
	      switch(obj->type) {
	      case TUPCLOSE:
		fn = (gpointer)end_tuplet;
		add_to_pattern(&pattern, '|');
		append_rhythm(r, fn);
		break;
	      case TUPOPEN:
		switch(((tupopen*)obj->object)->denominator) {
		case 3:
		  fn=(gpointer)start_triplet;
		  add_to_pattern(&pattern, '~');
		  break;
		default:// need to create start_xxxtuplet() functions to go with start_triplet(), then they can go here.
		  fn = NULL;
		}
		append_rhythm(r, fn);
		break;
	      case CHORD:
		{
		  chord *ch = (chord*)obj->object;
		  
		  if(ch->notes) {
		    switch(ch->baseduration) {
		    case 0:
		      fn = insert_chord_0key;
		      break;
		    case 1:
		      fn = insert_chord_1key;
		      break;
		    case 2:
		      fn = insert_chord_2key;
		      break;
		    case 3:
		      fn = insert_chord_3key;
		      break;
		    case 4:
		      fn = insert_chord_4key;
		      break;
		    case 5:
		      fn = insert_chord_5key;
		      break;
		    case 6:
		      fn = insert_chord_6key;
		      break;
		    }
		    add_to_pattern(&pattern, duration_code(fn));
		    append_rhythm(r, fn);

		  } else {/* a rest */
		    switch(ch->baseduration) {
		    case 0:
		      fn = insert_rest_0key;
		      break;
		      case 1:
		      fn = insert_rest_1key;
		      break;
		      case 2:
		      fn = insert_rest_2key;
		      break;
		      case 3:
		      fn = insert_rest_3key;
		      break;
		      case 4:
		      fn = insert_rest_4key;
		      break;
		      case 5:
		      fn = insert_rest_5key;
		      break;
		      case 6:
		      fn = insert_rest_6key;
		      break;
		    }
		    add_to_pattern(&pattern, modifier_code(fn));
		    append_rhythm(r, fn);
		  } /* end of rests */
		  for (i=ch->numdots;i;i--) {
		    fn = add_dot_key;		    
		    add_to_pattern(&pattern, modifier_code(fn));
		    append_rhythm(r, fn);
		  }
		  if(ch->slur_begin_p) {
		    fn = (gpointer)toggle_begin_slur;
		    add_to_pattern(&pattern,'('); 
		    append_rhythm(r, fn);
		  }
		  if(ch->slur_end_p) {
		    fn = (gpointer)toggle_end_slur;
		    add_to_pattern(&pattern,')'); 
		    append_rhythm(r, fn);
		  }
		}
		break;
	      default:
		;
		
	      }
	      //g_print("Number of rhythms %d\n", g_list_length(r->rsteps));
	    } /* End object loop */	 
	} /* End measure loop */
    }//looking at selection
  if(strlen(pattern)==0) { // nothing useful selected
      warningdialog("No selection to create a rhythm pattern from\nSee Edit->Select menu for selecting notes/rests");
      gtk_widget_destroy(GTK_WIDGET(r->button));
      g_free(pattern);
      g_free(r);
      return;
    }
  } else { // singleton
    if(!already_done) 
      append_rhythm(r, action);
  }
  if(!already_done) {
    gchar *labelstr;
    if(pattern) {
      labelstr = music_font(pattern);
    }
    else
      return;  //FIXME memory leak of r - well pattern is never NULL
    //g_print("rsteps is %p entry is %s, %s\n", r->rsteps, pattern, labelstr);
    label = LABEL(r->button);
    gtk_label_set_markup(GTK_LABEL(label), labelstr);
    g_free(labelstr);
  }
  
  if(!singleton) {
    /* fill the r->rsteps with icons for each step, singletons have NULL icon */
    GList *g;  
    RhythmElement *el;
    gint i;
    for(g=r->rsteps, i=0;g;g=g->next, i++) {
      el = (RhythmElement*)g->data;
      if(i==0 && (*(pattern)<'0' || *(pattern)>'6') && g->next)
	g  = g->next;// pattern does not start with a note, so we skip to the second element, unless there are no notes
      while(*(pattern+i) && (*(pattern+i)<'0' || *(pattern+i)>'6'))
	i++;
      if(*(pattern+i)) {
	*(pattern+i) += 20;
	el->icon = music_font(pattern);
	*(pattern+i) -= 20;
      }
      //g_print("el->icon = %s step %d pattern %s\n", el->icon, i, pattern);
    }
  }
  if(!already_done)
    if(r->rsteps) {
      /* make the list circular */
      r->rsteps->prev = g_list_last(r->rsteps);
      g_list_last(r->rsteps)->next = r->rsteps;
    }
  if(r->rsteps==NULL)
    {
      gtk_widget_destroy(GTK_WIDGET(button));
      g_free(r);
      r = NULL;
    } else 	{
      if(singleton) {
	if(!already_done) {//When creating first gui only
	  GtkWidget *toolbar = gtk_ui_manager_get_widget (Denemo.ui_manager, "/EntryToolBar");
	  gtk_toolbar_insert(GTK_TOOLBAR(toolbar), GTK_TOOL_ITEM(button), -1);
	  gtk_widget_show_all(GTK_WIDGET(button));
	  /* gui->rstep = r->rsteps; */
	  g_signal_connect (G_OBJECT (button), "clicked",
			  G_CALLBACK (singleton_callback), (gpointer)r);
	  unhighlight_rhythm(r);
	}
	if(default_rhythm){
	  gui->prevailing_rhythm = r;
	  gui->rstep = r->rsteps;
	  highlight_rhythm(r);
	  //g_print("prevailing rhythm is %p\n",r);
	}	
      } else {//not singleton
	GtkWidget *toolbar = gtk_ui_manager_get_widget (Denemo.ui_manager, "/RhythmToolBar");
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), GTK_TOOL_ITEM(button), -1);
	gtk_widget_show_all(GTK_WIDGET(button));
	gui->rstep = r->rsteps;
	gui->rhythms = g_list_append(gui->rhythms , r);
	
	if(gui->currhythm)
	  unhighlight_rhythm((RhythmPattern *)gui->currhythm->data);
	gui->currhythm = g_list_last(gui->rhythms);
	highlight_rhythm((RhythmPattern *)gui->currhythm->data);
	g_signal_connect (G_OBJECT (button), "clicked",
			  G_CALLBACK (select_rhythm_pattern), (gpointer)r);
      }      
    }
}

typedef enum shortcut_mod {
  SHORTCUT_NOCHANGE,
  SHORTCUT_ADD,
  SHORTCUT_DELETE
} shortcut_mod;

typedef struct set_accels_cb_data {
  GtkAccelKey *key;
  GtkButton *prop_button;
  shortcut_mod changed;
  gint keyval;
  gint modifiers;
  gint idx;
  gchar *path;
} set_accels_cb_data;

/* define accelerator
 */
static gint
capture_accel_for_action (GtkWidget * widget, GdkEventKey *event,
        set_accels_cb_data * cb_data) {
  cb_data->modifiers = dnm_sanitize_key_state(event);
  cb_data->keyval = event->keyval;
  gchar *accel_label = dnm_accelerator_name (event->keyval, cb_data->modifiers);
  gtk_button_set_label(GTK_BUTTON(widget), g_strdup_printf("%s [%s] %d %d",
              N_("shortcut"), accel_label, event->keyval, cb_data->modifiers));
  //FIXME memory leak
  //g_free(accel_label); what is the free for g_new???

  return TRUE;/* stop other handlers being processed */
}

static void
save_accels (void) {
  gchar *filename = g_build_filename(locatedotdenemo(),"standard.accels",NULL);
  gtk_accel_map_save (filename);
  g_free(filename);
  if(!(Denemo.accelerator_status & EXTRA_ACCELS_ACTIVE))// truncate the EXTRA_ACCELS file
    {
      FILE *fp;
      gchar * dotdenemo = (gchar*)locatedotdenemo ();
      gchar *filename = dotdenemo?g_build_filename(locatedotdenemo(), EXTRA_ACCELS, NULL):NULL;
      if(filename) {
	fp = fopen(filename,"w");
	fclose(fp);
      }
      g_free(filename);
      Denemo.accelerator_status = ACCELS_LOADED;
    } else
      Denemo.accelerator_status = EXTRA_ACCELS_ACTIVE;
}

static gboolean
accept_keypress(GtkButton *button, set_accels_cb_data *cb_data){
  gtk_button_set_label(button, N_("Press the key combination desired"));
  cb_data->changed = SHORTCUT_ADD;
  // set cb_data->sigid =  and kill the signal when activated.
  g_signal_connect (GTK_OBJECT (button), "key_press_event",
		    G_CALLBACK (capture_accel_for_action), cb_data);
  return TRUE;
}

static gboolean
delete_accel(GtkButton *button, set_accels_cb_data *cb_data) {
  gtk_button_set_label(button, N_("Press the shortcut key that you wish to delete"));
  cb_data->changed = SHORTCUT_DELETE;
  g_signal_connect (GTK_OBJECT (button), "key_press_event",
		    G_CALLBACK (capture_accel_for_action), cb_data);
  //GtkButton *prop_button = cb_data->prop_button;
  //g_free(accel_label); what is the free for g_new???
  //gtk_button_set_label(prop_button, N_("An accel will be deleted"));
  return TRUE;/* stop other handlers being processed */

}



typedef struct accel_cb {
  DenemoGUI *gui;
  GtkAction *action;
} accel_cb;


static void configure_keyboard_idx (GtkWidget*w, gint idx) {
  DenemoGUI *gui = Denemo.gui;
  configure_keyboard_dialog_init_idx (NULL, gui, idx);
}

static void toggleRecording (GtkWidget*w, gboolean *record) {
  g_print("Recording was %d\n", *record);
  *record = !*record;
}
static void executeScript(GtkWidget*w) {
  GtkTextIter startiter, enditer;
  GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(Denemo.ScriptView));
  gtk_text_buffer_get_start_iter (buffer, &startiter);
  gtk_text_buffer_get_end_iter (buffer,  &enditer);
  gchar *text = gtk_text_buffer_get_text(buffer, &startiter, &enditer, FALSE);
  g_print("Calling script %s\n", text);
 call_out_to_guile(text);
 g_free(text);
}


/*
  menu_click:
  intercepter for the callback when clicking on menu items for the set of Actions the Denemo offers.
  Left click runs default action, after recording the item in a scheme script if recording.
  Rigth click offers pop-up menu for setting shortcuts etc

*/
static 	void show_type(GtkWidget *widget, gchar *message);

static void append_scheme_call(gchar *func) {
  GtkTextIter enditer;
  GtkTextBuffer *buffer = gtk_text_view_get_buffer((GtkTextView*)(Denemo.ScriptView));
  //gtk_text_buffer_set_text(buffer,"",-1);
  gtk_text_buffer_get_end_iter (buffer,  &enditer);
  gchar *text = g_strdup_printf("(%s)\n",func);//prefix dnm_!!!!!!!
  gtk_text_buffer_insert(buffer, &enditer, text, -1);
  g_print("Added %s\n", text);
  g_free(text); 
}

static gboolean menu_click (GtkWidget      *widget,
			  GdkEventButton *event,
			  accel_cb *info)
{

  GtkAction *action = info->action;
  DenemoGUI *gui = info->gui;
  keymap *the_keymap = Denemo.prefs.standard_keymap;
  const gchar *func_name = gtk_action_get_name(action);

  gint idx = lookup_index_from_name (the_keymap, func_name);
  if (event->button != 3) //Not right click
    if(Denemo.ScriptRecording)
      if(idx_has_callback(the_keymap, idx, gui)){	
	append_scheme_call((gchar*)func_name);
	//return TRUE;
      }
  //g_print("event button %d, idx %d for %s\n", event->button, idx, func_name);
  if (event->button != 3)
    return FALSE;
  if (idx == -1)
    return TRUE;
  GtkWidget *menu = gtk_menu_new();
  GtkWidget *item = gtk_menu_item_new_with_label("Edit/Create Shortcut");
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
  g_signal_connect(item, "activate", G_CALLBACK(configure_keyboard_idx), (gpointer)idx);

  item = gtk_check_menu_item_new_with_label("Recording Script");
  gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(item), Denemo.ScriptRecording);
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
  g_signal_connect(item, "toggled", G_CALLBACK(toggleRecording), &Denemo.ScriptRecording);
  gtk_menu_popup (GTK_MENU(menu), NULL, NULL, NULL, NULL,0, gtk_get_current_event_time()); 


  item = gtk_menu_item_new_with_label("Execute Script");
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
  g_signal_connect(item, "activate", G_CALLBACK(executeScript), NULL);


  gtk_widget_show_all(menu);
  gtk_menu_popup (GTK_MENU(menu), NULL, NULL, NULL, NULL,0, gtk_get_current_event_time()); 

  // configure_keyboard_dialog_init_idx (action, gui, idx);
  return FALSE;
}



static void	  color_rhythm_button(RhythmPattern *r, const gchar *color) {
  if(r==NULL) return;
  GdkColor thecolor;
  gdk_color_parse (color, &thecolor);
  gtk_widget_modify_bg (gtk_tool_button_get_label_widget(GTK_TOOL_BUTTON(r->button)), GTK_STATE_NORMAL, &thecolor);
  
}
void	  highlight_rhythm(RhythmPattern *r) {
  color_rhythm_button(r, "green");
}

void	  unhighlight_rhythm(RhythmPattern *r) {
  color_rhythm_button(r, "gray");
}


/*
 

  
*/
void	highlight_rest(DenemoGUI *gui, gint dur) {  

  //g_print("highlight rest");
      if(gui->currhythm) {
	unhighlight_rhythm((RhythmPattern *)gui->currhythm->data);	
      }
      gui->currhythm = NULL;
      gui->rstep = Denemo.singleton_rhythms['r'+dur]->rsteps;
      unhighlight_rhythm(gui->prevailing_rhythm);
      gui->prevailing_rhythm = Denemo.singleton_rhythms['r'+dur];
      highlight_rhythm(gui->prevailing_rhythm);

}

void	highlight_duration(DenemoGUI *gui, gint dur) {  

  //g_print("higlight duration");
      if(gui->currhythm) {
	unhighlight_rhythm((RhythmPattern *)gui->currhythm->data);	
      }
      gui->currhythm = NULL;
      gui->rstep =  Denemo.singleton_rhythms['0'+dur]->rsteps;
      unhighlight_rhythm(gui->prevailing_rhythm);
      gui->prevailing_rhythm = Denemo.singleton_rhythms['0'+dur];
      highlight_rhythm(gui->prevailing_rhythm);
}


/*
 * delete a rhythmic pattern and its button
 * 
 */
static void
delete_rhythm_cb (GtkAction * action)
{
  DenemoGUI *gui = Denemo.gui;
  if(gui->mode&(INPUTEDIT) == 0)
    return;
  if(gui->currhythm==NULL)
    return;
  RhythmPattern *r =(RhythmPattern *)gui->currhythm->data;
  gtk_widget_destroy(GTK_WIDGET(r->button));
  /* list is circular, so before we free it we have to break it */
  r->rsteps->prev->next = NULL;
  r->rsteps->prev = NULL;
  GList *g;
  for(g=r->rsteps;g;g=g->next)
    g_free(g->data);
  g_list_free(r->rsteps);
  g_free(r);
  //g_print("length %d\n", g_list_length(gui->rhythms));
  gui->rhythms = g_list_remove(gui->rhythms, gui->currhythm->data);
  //g_print("length %d %p\n", g_list_length(gui->rhythms), gui->rhythms);
  gui->currhythm = g_list_last(gui->rhythms);

  if(gui->currhythm == NULL)
   gui->rstep = NULL;
  else {
    highlight_rhythm(gui->currhythm->data);
    gui->rstep = ((RhythmPattern *)gui->currhythm->data)->rsteps;
  }
}



/*
 * workaround for glib<2.10
 */
static
void  attach_action_to_widget (GtkWidget *widget, GtkAction *action, DenemoGUI *gui) {
  g_object_set_data(G_OBJECT(widget), "action", action);
}


void  attach_set_accel_callback (GtkWidget *widget, GtkAction *action, gchar *path, DenemoGUI *gui) {
  accel_cb *info = g_malloc0(sizeof(accel_cb));
  info->gui = gui;
  info->action = action;
  g_signal_connect(widget, "button-press-event", G_CALLBACK (menu_click), info);
}

#ifdef DENEMO_DYNAMIC_MENU_ITEMS

/* add a new menu item dynamically */
static void add_favorite(GtkAction *action, DenemoGUI *gui) {
  gchar *myname, *mylabel, *mylily, *mytooltip, *myposition;
  myname = string_dialog_entry (gui, "Create a new menu item", "Give item name (avoid clashes): ", "MyName");
  //FIXME check for name clashes
  mylabel = string_dialog_entry (gui, "Create a new menu item", "Give menu label: ", "My Label");
  mylily = string_dialog_entry (gui, "Create a new menu item", "Give LilyPond to insert: ", "%any LilyPond you like");
  mytooltip = string_dialog_entry (gui, "Create a new menu item", "Give explanation of what it does: ", "Prints my special effect");
   myposition = string_dialog_entry (gui, "Create a new menu item", "Say where in the menu systeme you want it placed: ", "/ObjectMenu/Favorites");
  GtkAction *myaction = gtk_action_new(myname,mylabel,mytooltip,NULL);
  GtkActionGroup *action_group;
  GList *groups = gtk_ui_manager_get_action_groups (Denemo.ui_manager);
  action_group = GTK_ACTION_GROUP(groups->data); 
  GtkAccelGroup *accel_group = gtk_ui_manager_get_accel_group (Denemo.ui_manager);
  gtk_action_group_add_action(action_group, myaction);
  g_object_set_data(G_OBJECT(myaction), "lilypond", mylily);
  g_signal_connect (G_OBJECT (myaction), "activate",
		    G_CALLBACK (myactivate), gui); 
  gtk_action_set_accel_group (myaction, accel_group);
  gchar *accelpath = g_strdup_printf("%s%s", "<Actions>/MenuActions/", myname);
  gtk_action_set_accel_path (myaction, accelpath);
  g_free(accelpath);
  gtk_ui_manager_add_ui(Denemo.ui_manager,gtk_ui_manager_new_merge_id(Denemo.ui_manager), 
			myposition,
			myname, myname, GTK_UI_MANAGER_AUTO, FALSE);
  GSList *h = gtk_action_get_proxies (myaction);//what is a proxy? any widget that callbacks the action
   for(;h;h=h->next) {
     attach_set_accel_callback(h->data, myaction, myposition, gui);
   }
   GtkActionEntry *menu_entry = g_malloc0(sizeof(GtkActionEntry));
   menu_entry->name = myname;
   menu_entry->label = mylabel;
   menu_entry->tooltip =  mytooltip;
   menu_entry->callback = G_CALLBACK (myactivate);
   register_entry_commands(Denemo.prefs.the_keymap, menu_entry, 1,
          KeymapEntry);
  end_command_registration(Denemo.prefs.the_keymap);
  return;
}
#endif /* ifdef DENEMO_DYNAMIC_MENU_ITEMS */
extern int call_out_to_guile(char *script);
static void dummy(void) {
#ifdef TRIAL_SCHEME
  //call_out_to_guile("(CursorRight)(CursorRight)(CursorRight)\n");
  g_print("calling guile with %p %p\n", &Denemo, Denemo.ui_manager);
  call_out_to_guile("(denemoy \"/MainMenu/EntryMenu/Rest\")\n");
#endif
  return;
}
static void voiceup_cb(GtkAction *action) {
  DenemoGUI *gui = Denemo.gui;
  voiceup(gui);
  displayhelper(gui);
}
static void voicedown_cb(GtkAction *action) {
  DenemoGUI *gui = Denemo.gui;
  voicedown(gui);
  displayhelper(gui);
}
/**
 * Menu entries with no shortcut keys, tooltips, and callback functions
 */
GtkActionEntry menu_entries[] = {
#include "entries.h"
  {"Stub",  NULL, N_(" "), NULL, N_("Does nothing"), G_CALLBACK (dummy)}

  };

//Get number of menu entries
gint n_menu_items = G_N_ELEMENTS (menu_entries);

static
GtkWidget *get_edit_menu_for_mode(gint mode) {
  if(mode&INPUTEDIT)
    return Denemo.EditModeMenu;
  if(mode&INPUTINSERT)
    return Denemo.InsertModeMenu;
  if(mode&INPUTCLASSIC)
    return Denemo.ClassicModeMenu;
  return Denemo.ModelessMenu;
}

/**
 *  callback changing mode  gui->mode
 *
 */
static void
change_mode (GtkRadioAction * action, GtkRadioAction * current) {
  DenemoGUI *gui = Denemo.gui;
gint val = gtk_radio_action_get_current_value (current);
 GtkWidget *menu = get_edit_menu_for_mode(gui->mode);
 if(menu)
   gtk_widget_hide(menu);
 gui->mode=((gui->mode&MODE_MASK)|val);
 menu = get_edit_menu_for_mode(gui->mode);
 if(menu)
   gtk_widget_show(menu);
 write_status(gui);
 
}


static void   activate_action(gchar *path) {
   GtkAction *a;
   a = gtk_ui_manager_get_action (Denemo.ui_manager, path);
   if(a)
   gtk_action_activate(a);
   else 
     g_warning("Internal error, denemogui.xml out of step with literal %s in %s\n", path, __FILE__);
 }
/**
 *  callback changing type of entry part of gui->mode,
 * depending on the entry type it switches mode part of gui->mode to Classic mode for entering rests and to Insert for entering notes. FIXME could switch to prefs value.
 *
 */
static void
change_entry_type (GtkRadioAction * action, GtkRadioAction * current) {
  DenemoGUI *gui = Denemo.gui;
gint val = gtk_radio_action_get_current_value (current);
 switch(val) {
#define SET_MODE(m)  (gui->mode=((gui->mode&ENTRY_TYPE_MASK)|m))
 case INPUTREST:
   SET_MODE(INPUTREST);
   activate_action("/MainMenu/EntryMenu/ClassicMode");

   break;
 case INPUTNORMAL:
   SET_MODE(INPUTNORMAL);
   activate_action( "/MainMenu/EntryMenu/InsertMode");
   break;
 case INPUTBLANK:
   SET_MODE(INPUTBLANK);
   activate_action( "/MainMenu/EntryMenu/ClassicMode");
   break;
 case INPUTRHYTHM|INPUTNORMAL:
   SET_MODE(INPUTRHYTHM|INPUTNORMAL);
   activate_action( "/MainMenu/EntryMenu/EditMode");
   break;
 }
#undef SET_MODE

write_status(gui);
 //g_print("Mode is %x masks %x %x\n",ENTRY_TYPE_MASK, MODE_MASK, gui->mode);
}

/* callback: if not Insert mode set Insert mode else set Edit mode */
static void toggle_edit_mode (GtkAction * action){
  DenemoGUI *gui = Denemo.gui;
  static gint mode=INPUTINSERT;
  if(gui->mode&INPUTEDIT){
    switch(mode & ~MODE_MASK ) {
    case INPUTINSERT:
      activate_action( "/MainMenu/EntryMenu/InsertMode");
      break;
    case INPUTCLASSIC:
      activate_action( "/MainMenu/EntryMenu/ClassicMode");
      break;
    case 0:
      activate_action( "/MainMenu/EntryMenu/Modeless");
      break;
    default:
      ;
    }
  } else {
    mode = gui->mode;// remember mode for switching back
    activate_action( "/MainMenu/EntryMenu/EditMode");
  }
}

/* callback: if rest entry make note entry and vv */
static void toggle_rest_mode (GtkAction * action){
  DenemoGUI *gui = Denemo.gui;
  static gint mode=INPUTNORMAL;
  if(gui->mode&INPUTREST){
    switch(mode & ~ENTRY_TYPE_MASK ) {
    case INPUTNORMAL:
      activate_action( "/MainMenu/EntryMenu/Note");
      break;
    case INPUTBLANK:
      activate_action( "/MainMenu/EntryMenu/Blank");
      break;
    default:
      ;
    }
  } else {
    mode = gui->mode;// remember mode for switching back
    activate_action( "/MainMenu/EntryMenu/Rest");
  }
}


/* callback: if rhythm entry make note entry and vv */
static void toggle_rhythm_mode (GtkAction * action){
  DenemoGUI *gui = Denemo.gui;
  static gint mode=INPUTNORMAL;
  if(gui->mode&INPUTRHYTHM){
    switch(mode & ~ENTRY_TYPE_MASK ) {
    case INPUTNORMAL:
      activate_action( "/MainMenu/EntryMenu/Note");
      break;
    default:
      ;
    }
  } else {
    mode = gui->mode;// remember mode for switching back, breaks with multi gui FIXME
    activate_action( "/MainMenu/EntryMenu/Rhythm");
  }
}

/**
 *  Function to toggle the visibility of the LilyPond text window. It refreshes 
 *  the text if needed
 */
static void
toggle_lilytext (GtkAction * action) {
  DenemoGUI *gui = Denemo.gui;
 if(!gui->textview)
   refresh_lily_cb(action, gui);
 if(!GTK_WIDGET_VISIBLE(gui->textwindow))
   gtk_widget_show/*_all*/(gui->textwindow);
 else
   gtk_widget_hide(gui->textwindow);
 //g_print("toggling lily window");
}


/**
 *  Function to toggle the visibility of the Scheme text window. 
 */
static void
toggle_scheme (GtkAction * action) {
  DenemoGUI *gui = Denemo.gui;
  GtkWidget *textwindow = gtk_widget_get_toplevel(Denemo.ScriptView);
 if(!GTK_WIDGET_VISIBLE(textwindow))
   gtk_widget_show_all(textwindow);
 else
   gtk_widget_hide_all(textwindow);
 // g_print("toggling scheme window");
}
static gint
hide_scheme (GtkAction * action, GdkEvent*event,  GtkWidget *w) {
  activate_action("/MainMenu/ViewMenu/ToggleScript");
  return TRUE;
}


/**
 *  Function to toggle entry of notes by pitch recognition off/on 
 *
 */
static void
toggle_pitch_recognition (GtkAction * action) {
  DenemoGUI *gui = Denemo.gui;
  if(!gui->pitch_recognition) {
    if(setup_pitch_recognition(gui))  
      {/*FIXME error */
	g_warning("Error setting up pitch recognition");
	return;
      }
  }
  /* then turn on/off */
 
  if(gui->pitch_recognition) {
    stop_pitch_recognition();
  } else {
    start_pitch_recognition(gui);// FIXME different guis
    activate_action( "/MainMenu/EntryMenu/Note"); 
  }
  gui->pitch_recognition =  !gui->pitch_recognition;
}


/**
 *  Function to toggle whether rhythm toolbar is visible 
 *  (no longer switches keymap to Rhythm.keymaprc when toolbar is on back to standard when off.)
 *  
 */
static void
toggle_rhythm_toolbar (GtkAction * action, DenemoGUI * gui)
{
  static keymap *rhythm_keymap;// special keymap in rhythm submode
  GtkWidget *widget;
  widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/RhythmToolBar");
 // g_print("Callback for %s\n", g_type_name(G_TYPE_FROM_INSTANCE(widget)));
  if (GTK_WIDGET_VISIBLE (widget))
    {
      if(Denemo.prefs.standard_keymap)
	Denemo.prefs.the_keymap = Denemo.prefs.standard_keymap;
      gtk_widget_hide (widget);
    }
  else
    {
#if 0
      if(rhythm_keymap==NULL){
	rhythm_keymap = create_keymap ("Rhythm.keymaprc");
	//printf("CREATED keymap\n");
      }
      Denemo.prefs.the_keymap = rhythm_keymap;
#endif
      gtk_widget_show (widget);
      /* make sure we are in Insert and Note for rhythm toolbar */
      activate_action( "/MainMenu/EntryMenu/Note");
      activate_action( "/MainMenu/EntryMenu/InsertMode");
    }
}


/**
 *  Function to toggle whether entry toolbar is visible 
 *  
 *  
 */
static void
toggle_entry_toolbar (GtkAction * action, DenemoGUI * gui)
{
  GtkWidget *widget;
  widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/EntryToolBar");
  if(!widget) return;// internal error - out of step with menu_entries...
  if (GTK_WIDGET_VISIBLE (widget))
    {
 
      gtk_widget_hide (widget);
    }
  else
    {
      gtk_widget_show (widget);
    }
}

/**
 *  Function to toggle whether keyboard bindings can be set by pressing key over menu item 
 *  
 *  
 */
static void
toggle_quick_edits (GtkAction * action, DenemoGUI * gui)
{
  static gboolean set;
  GtkSettings * settings =
    gtk_settings_get_for_screen(gdk_screen_get_default());
  if (settings)
    gtk_settings_set_long_property(settings, "gtk-can-change-accels", set = !set, ".gtkrc:0");
  if(set)
    Denemo.accelerator_status |= ACCELS_MAY_HAVE_CHANGED;// we can't detect if they actually are
  //TODO now we can, by adding a status on the keymap, for the time being, left
  //unchanged
  // g_print("edits are %d\n",set);
}


/**
 *  Function to toggle whether action menubar is visible 
 *  
 *  
 */
static void
toggle_action_menu (GtkAction * action, DenemoGUI * gui)
{
  GtkWidget *widget;
  widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ActionMenu");
  if(!widget) return;// internal error - out of step with menu_entries...
  if (GTK_WIDGET_VISIBLE (widget))
    {
 
      gtk_widget_hide (widget);
    }
  else
    {
      gtk_widget_show (widget);
    }
}



/**
 *  Function to toggle whether object menubar is visible 
 *  
 *  
 */
static void
toggle_object_menu (GtkAction * action, DenemoGUI * gui)
{
  GtkWidget *widget;
  widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu");
  if(!widget) return;// internal error - out of step with menu_entries...
  if (GTK_WIDGET_VISIBLE (widget))
    {
 
      gtk_widget_hide (widget);
    }
  else
    {
      gtk_widget_show (widget);
    }
}



/**
 * Toggle entries for the menus
 */
GtkToggleActionEntry toggle_menu_entries[] = {
  {ToggleRhythmToolbar_STRING, NULL, N_("Rhythms and Overlays"), NULL, N_("Show/hide a toolbar which allows\nyou to enter notes using rhythm patterns and\nto overlay these with pitches"),
   G_CALLBACK (toggle_rhythm_toolbar), FALSE}
  ,
  {ToggleEntryToolbar_STRING, NULL, N_("Note and rest entry"), NULL, N_("Show/hide a toolbar which allows\nyou to enter notes and rests using the mouse"),
   G_CALLBACK (toggle_entry_toolbar), FALSE}
  ,
  {ToggleActionMenu_STRING, NULL, N_("Menu of actions"), NULL, N_("Show/hide a menu which is arranged by actions\nThe actions are independent of any mode set"),
   G_CALLBACK (toggle_action_menu), FALSE}
  ,
  {ToggleObjectMenu_STRING, NULL, N_("Menu of objects"), NULL, N_("Show/hide a menu which is arranged by objects\nThe actions available for note objects change with the mode"),
   G_CALLBACK (toggle_object_menu), FALSE}
  ,
  {ToggleLilyText_STRING, NULL, N_("Show LilyPond"), NULL, N_("Show/hide the LilyPond music typesetting language window"),
   G_CALLBACK (toggle_lilytext), FALSE}
  ,
  {ToggleScript_STRING, NULL, N_("Show Scheme Script"), NULL, N_("Show scheme script window"),
   G_CALLBACK (toggle_scheme), FALSE}
  ,
  {TogglePitchRecognition_STRING, NULL, N_("Microphone"), NULL, N_("Enable pitch entry from microphone"),
   G_CALLBACK (toggle_pitch_recognition), FALSE}
  ,
  {ToggleArticulationPalette_STRING, NULL, N_("_Articulation Palette"), NULL, NULL,
   G_CALLBACK (toggle_articulation_palette), FALSE},
  {QuickEdits_STRING, NULL, N_("Allow Quick Shortcut Edits"), NULL, "Enable editing keybindings by pressing a key while hovering over the menu item",
   G_CALLBACK (toggle_quick_edits), FALSE},


  {ReadOnly_STRING, NULL, N_("Read Only"), NULL, "Make score read only\nNot working",
   G_CALLBACK (default_mode), FALSE}
};

/**
 * Radio entries for the modes and entry types
 */
static GtkRadioActionEntry mode_menu_entries[] = {
  {MODELESS_STRING, NULL, N_("No mode"), NULL, "Access all editing functions without change of mode",
   0},
  {CLASSICMODE_STRING, NULL, N_("Classic"), NULL, "The original Denemo note entry mode\nUseful for entering notes into chords\nUse the note names to move the cursor\nUse the durations to insert notes",
   INPUTCLASSIC},
  {INSERTMODE_STRING, NULL, N_("Insert"), NULL, N_("Mode for inserting notes into the score at the cursor position\nUses prevailing duration/rhythm\nUse the durations to set the prevailing duration\nUse the note names to insert the note"),
   INPUTINSERT},
  {EDITMODE_STRING, NULL, N_("Edit"), NULL, N_("Mode for changing the note at cursor (name, duration)\nand to enter notes by duration (rhythms)\nUse the durations to insert notes"),
   INPUTEDIT}
};


static GtkRadioActionEntry type_menu_entries[] = {
  {NOTE_E_STRING, NULL, N_("Note"), NULL,  N_("Normal (note) entry"), INPUTNORMAL},
  {REST_E_STRING, NULL, N_("Rest"), NULL,  N_("Entering rests not notes"), INPUTREST},
  {BLANK_E_STRING, NULL, N_("Non printing rests"), NULL,  N_("Enters rests which will not be printed (just take up space)\nUsed for positioning polyphonic voice entries"), INPUTBLANK},
  {RHYTHM_E_STRING, NULL, N_("Rhythm"), NULL, N_("Mode for pure rhythyms"),
   INPUTRHYTHM|INPUTNORMAL}
};

struct cbdata
{
  DenemoGUI *gui;
  gchar *filename;
};

/**
 * Callback for the history menu
 * opens the selected file
 */
static void
openrecent (GtkWidget * widget, gchar *filename)
{
  DenemoGUI *gui = Denemo.gui;
  if (!gui->changecount || (gui->changecount && confirmbox (gui)))
    {
      deletescore(NULL, gui);
      if(open_for_real (filename, gui, FALSE, FALSE))
	{
	  gchar *warning = g_strdup_printf("Load of recently used file %s failed", filename);
	  warningdialog(warning);
	  g_free(warning);
	}
    }
}

 
/**
 * Add history entry to the History menu, create a menu item for it
 */
void
addhistorymenuitem (gchar *filename)
{
  GList *g;
  if(!g_file_test(filename,  G_FILE_TEST_EXISTS))
    return;
  GtkWidget *item =
    gtk_ui_manager_get_widget (Denemo.ui_manager,
			       "/MainMenu/FileMenu/OpenRecent/Stub");
  GtkWidget *menu = gtk_widget_get_parent (GTK_WIDGET (item));
  
  item = gtk_menu_item_new_with_label (filename);
  gtk_menu_shell_insert (GTK_MENU_SHELL (menu), item, 0);
  g_signal_connect (item, "activate", G_CALLBACK (openrecent), g_strdup(filename));
  gtk_widget_show (item);
}

/**
 * Top-Level function to populate the History menu
 * with elements read from the denemohistory file
 */
static void
populate_opened_recent (void)
{
  g_queue_foreach (Denemo.prefs.history, (GFunc)addhistorymenuitem, NULL);
}

static 	void show_type(GtkWidget *widget, gchar *message) {
    g_print("%s%s\n",message, widget?g_type_name(G_TYPE_FROM_INSTANCE(widget)):"NULL widget");
  }
/* set all labels in the hierarchy below widget to use markup */
static void use_markup(GtkWidget *widget)
{
  // show_type(widget, "Widget Type: ");

 
  //g_print("container type %x\n", GTK_IS_CONTAINER(widget));
  //g_print("label type %x\n", GTK_IS_LABEL(widget));
  //g_print("menu item type %x\n",GTK_IS_MENU_ITEM(widget));
  //g_print("tool item type %x\n",GTK_IS_TOOL_ITEM(widget));
  //g_print("descended to use markup on %p\n", widget);

  if(GTK_IS_LABEL(widget)) {
   // gtk_label_set_use_underline (GTK_LABEL (widget), FALSE); font_desc gets interpreted in GtkLabel but not GtkAccelLabel hmmm...
    //g_print("Before we have %d\n", gtk_label_get_use_markup        (widget));
    //gchar * label = gtk_label_get_label(widget);
     //g_print("label before is %s\n", label);
    
    gtk_label_set_use_markup (GTK_LABEL (widget), TRUE);
    //g_print("after we have %d\n", gtk_label_get_use_markup        (widget));
    //if(*label=='M')
    //g_print("seting %p", widget),gtk_label_set_markup(widget, "hello"MUSIC_FONT("33")"ok"), show_type(widget, "should be label: "), label = gtk_label_get_label(widget),g_print("label now %s\n",label) ;

  }
  else
 if(GTK_IS_CONTAINER(widget)) {
    GList *g = gtk_container_get_children (GTK_CONTAINER(widget));
    for(;g;g=g->next)
      use_markup(g->data);
    if (GTK_IS_MENU_ITEM(widget)) {
      use_markup(gtk_menu_item_get_submenu(GTK_MENU_ITEM(widget)));
    }
 }
}



/**
 * Key snooper function. This function intercepts all key events before they are
 * passed to other functions for further processing. We use it to override some
 * default behaviour of gtk accel management so as to maintain a consistent
 * keymap.
 */
gint dnm_key_snooper(GtkWidget *grab_widget, GdkEventKey *event,
        gpointer func_data)
{
    keymap *the_keymap = (keymap *) func_data;
    //no special processing for key release events
    if (event->type == GDK_KEY_RELEASE)
        return FALSE;
    //if the grab_widget is a menu, the event could be a quick edit
    if (GTK_IS_MENU (grab_widget)) {
        return keymap_accel_quick_edit_snooper(grab_widget, event, the_keymap);
    }
    //else we let the event be processed by other functions
    return FALSE;
}

/* 
 * create and populate the action group containing actions handled by the keymap
 * allocate the keymap
 */
void init_keymap()
{
  //TODO we initialize the keymap before the first windowin shown because it has
  //to be once and for all. Does it break something in regards of the next
  //comment?
  /* Similarly, the keymap should be initialized after the
     only once si->window is shown, as it may pop up an advisory
     dialog. */
  Denemo.prefs.standard_keymap = allocate_keymap ("MenuActions");
  Denemo.prefs.the_keymap = Denemo.prefs.standard_keymap; 
  register_entry_commands(Denemo.prefs.the_keymap, menu_entries, n_menu_items,
          KeymapEntry);
  register_entry_commands(Denemo.prefs.the_keymap, toggle_menu_entries,
          G_N_ELEMENTS (toggle_menu_entries), KeymapToggleEntry);
  register_entry_commands(Denemo.prefs.the_keymap, mode_menu_entries,
          G_N_ELEMENTS (mode_menu_entries), KeymapRadioEntry);
  register_entry_commands(Denemo.prefs.the_keymap, type_menu_entries,
          G_N_ELEMENTS (type_menu_entries), KeymapRadioEntry);
  end_command_registration(Denemo.prefs.the_keymap);
  load_default_keymap_file(Denemo.prefs.the_keymap);
    
  gtk_key_snooper_install(dnm_key_snooper, Denemo.prefs.the_keymap);
}


static void
switch_page (GtkNotebook *notebook, GtkNotebookPage *page,  guint pagenum) {
  //g_print("switching pagenum %d\n",pagenum);
  DenemoGUI *gui = Denemo.gui;
  if(gui==NULL)
    return;
  GList *g = g_list_nth(Denemo.guis, pagenum);
  if(g==NULL) {
    g_warning("got a switch page, but there is no such page in Denemo.guis\n");
    return;
  }
  DenemoGUI *newgui = g->data;
  if(gui==newgui)
    return;//on arrival Denemo.gui is already set to the new gui when you are doing new window
  /* turn off the LilyPond window if it is on
   it would be nice to keep a record of whether it was open for re-opening
   on return to this tab FIXME*/
  {
    GtkWidget *widget;
    widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/MainMenu/ViewMenu/ToggleLilyText");
    if(gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM (widget)))
      gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (widget), FALSE);
  }
  unhighlight_rhythm(Denemo.gui->prevailing_rhythm);
  Denemo.gui = gui = (DenemoGUI*)(g->data);

  switch(gui->mode & ~MODE_MASK ) {
    case INPUTINSERT:
      activate_action( "/MainMenu/EntryMenu/InsertMode");
      break;
    case INPUTEDIT:
      activate_action( "/MainMenu/EntryMenu/EditMode");
      break;
    case INPUTCLASSIC:
      activate_action( "/MainMenu/EntryMenu/ClassicMode");
      break;
    case 0:
      activate_action( "/MainMenu/EntryMenu/Modeless");
      break;
    default:
      ;
    }

  switch(gui->mode & ~ENTRY_TYPE_MASK ) {
    case INPUTNORMAL:
      activate_action( "/MainMenu/EntryMenu/Note");
      break;
    case INPUTBLANK:
      activate_action( "/MainMenu/EntryMenu/Blank");
      break;
    case INPUTREST:
      activate_action( "/MainMenu/EntryMenu/Rest");
      break;
    case INPUTRHYTHM:
      activate_action( "/MainMenu/EntryMenu/Rhythm");
      break;
    default:
      ;
    }

  highlight_rhythm(Denemo.gui->prevailing_rhythm);

}

/* create_window() creates the toplevel window and all the menus - it only
   called once per invocation of Denemo */
static void
create_window(void) {
  DenemoPrefs *prefs;
  GtkWidget *main_vbox, *menubar, *toolbar, *hbox;
  GtkActionGroup *action_group;
  GtkUIManager *ui_manager;
  GtkAccelGroup *accel_group;
  GError *error;
  GtkWidget *widget;
  gchar *data_dir;
  Denemo.window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (Denemo.window), "Denemo Main Window");
  loadWindowState(/* it accesses Denemo.window */);
#ifdef G_OS_WIN32
  data_dir = g_build_filename (get_data_dir (), "icons","denemo.png", NULL);
#else
  data_dir = g_strconcat (get_data_dir (), "/../icons/denemo.png", NULL);//FIXME installed in wrong place
#endif
  gtk_window_set_default_icon_from_file (data_dir, NULL);
  gtk_signal_connect (GTK_OBJECT (Denemo.window), "delete_event",
		      (GtkSignalFunc) delete_callback, NULL);
  g_free (data_dir);

  gtk_window_set_resizable (GTK_WINDOW (Denemo.window), TRUE);
  Denemo.ScriptView = gtk_text_view_new();
  GtkWidget *w = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (w), "Denemo Scheme Script");
  //gtk_window_set_resizable (GTK_WINDOW (w), TRUE);
  g_signal_connect(w, "delete-event", G_CALLBACK(hide_scheme/*gtk_widget_hide_on_delete*/), w);
  main_vbox = gtk_vbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (w), main_vbox);
  
  w = gtk_button_new_with_label("Execute Script");
  g_signal_connect(w, "clicked",  G_CALLBACK(executeScript), NULL);
  gtk_box_pack_start (GTK_BOX (main_vbox), w, FALSE, TRUE, 0);

  
  gtk_box_pack_start (GTK_BOX (main_vbox), Denemo.ScriptView , FALSE, TRUE, 0);




  main_vbox = gtk_vbox_new (FALSE, 1);
  gtk_container_border_width (GTK_CONTAINER (main_vbox), 1);
  gtk_container_add (GTK_CONTAINER (Denemo.window), main_vbox);
  gtk_widget_show (main_vbox);

  action_group = gtk_action_group_new ("MenuActions");
  /* This also sets current Denemo.gui as the  callback data for all the functions in the
   * menubar, which is not needed since we have only one set of actions for all
   the guis. We will always act on Denemo.gui anyway.*/
  gtk_action_group_add_actions (action_group, menu_entries,
  			G_N_ELEMENTS (menu_entries),  Denemo.gui);
  gtk_action_group_add_toggle_actions (action_group,
				       toggle_menu_entries,
				       G_N_ELEMENTS (toggle_menu_entries),
				       Denemo.gui);
  gtk_action_group_add_radio_actions (action_group,
				       mode_menu_entries,
				       G_N_ELEMENTS (mode_menu_entries),
				      INPUTINSERT/* initial value */, 
				      G_CALLBACK(change_mode),  Denemo.gui);


  gtk_action_group_add_radio_actions (action_group,
				       type_menu_entries,
				       G_N_ELEMENTS (type_menu_entries),
				      INPUTNORMAL/* initial value */, 
				      G_CALLBACK(change_entry_type),  Denemo.gui);


#ifdef DENEMO_DYNAMIC_MENU_ITEMS
  //GtkAction *favorites =  gtk_action_new("Favorites","Favorites","LilyPond inserts often needed",NULL);
  //gtk_action_group_add_action(action_group, favorites);

  GtkAction *myaction = gtk_action_new("My Name","My Label","My Tooltip",NULL);
  gtk_action_group_add_action(action_group, myaction);

  g_object_set_data(G_OBJECT(myaction), "lilypond", "\\mark \\default\n");
  g_signal_connect (G_OBJECT (myaction), "activate",
		    G_CALLBACK (myactivate), Denemo.gui); 
#endif

  ui_manager = gtk_ui_manager_new ();
  Denemo.ui_manager = ui_manager;
  gtk_ui_manager_set_add_tearoffs (Denemo.ui_manager, TRUE);
  gtk_ui_manager_insert_action_group (ui_manager, action_group, 0);
  //We do not use accel_group anymore TODO delete the next 2 lines
  //accel_group = gtk_ui_manager_get_accel_group (ui_manager);
  //gtk_window_add_accel_group (GTK_WINDOW (Denemo.window), accel_group);

  /* TODO Lily_menu actions are handled differently for the time being
   * What are these actions?
   */
  GtkActionEntry lily_menus[] = {
    {"LilyToggleShow", NULL, N_("Show/Hide"),NULL, N_("Toggle visibility of section"),G_CALLBACK (toggle_lily_visible_cb)},
    {"LilyCreateCustom", NULL, N_("Create Custom Version"),NULL, N_("Create a custom version of this block"),G_CALLBACK (custom_lily_cb)},
    {"LilyDelete", NULL, N_("Delete Block"),NULL, N_("Delete this block"),G_CALLBACK (delete_lily_cb)}
  };

  data_dir = g_build_filename (
#ifndef USE_LOCAL_DENEMOUI
get_data_dir (),
#endif
 "denemoui.xml", NULL);
  error = NULL;
  if (!gtk_ui_manager_add_ui_from_file (ui_manager, data_dir, &error))
    {
      g_message ("building menu failed: %s", error->message);
      g_error_free (error);
      gchar *message = g_strdup_printf("The denemoui.xml %s file could not be used - exiting", data_dir);
      warningdialog(message);
      exit (EXIT_FAILURE);
    }
  g_free (data_dir);

#ifdef DENEMO_DYNAMIC_MENU_ITEMS
  //We do not use accel_group anymore TODO delete the next 2 lines
  //gtk_action_set_accel_group (myaction, accel_group);
  //gtk_action_set_accel_path (myaction,"<Actions>/MenuActions/My Name");
  //gtk_ui_manager_add_ui(ui_manager,gtk_ui_manager_new_merge_id(ui_manager), "/ObjectMenu", "Favorites", "Favorites", GTK_UI_MANAGER_MENU, FALSE);

  gtk_ui_manager_add_ui(ui_manager,gtk_ui_manager_new_merge_id(ui_manager), "/ObjectMenu/Favorites",
                                             "My Name", "My Name", GTK_UI_MANAGER_AUTO, FALSE);

#endif

  //menubar = gtk_item_factory_get_widget (item_factory, "<main>");
  Denemo.menubar = gtk_ui_manager_get_widget (ui_manager, "/MainMenu");// this triggers Lily... missing action
  gtk_box_pack_start (GTK_BOX (main_vbox), Denemo.menubar, FALSE, TRUE, 0);
  gtk_widget_show (Denemo.menubar);





  toolbar = gtk_ui_manager_get_widget (ui_manager, "/ToolBar");
  // The user should be able to decide toolbar style.
  // But without gnome, there is no (ui) to set this option.

  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_BOTH_HORIZ);
  gtk_box_pack_start (GTK_BOX (main_vbox), toolbar, FALSE, TRUE, 0);
  GTK_WIDGET_UNSET_FLAGS(toolbar, GTK_CAN_FOCUS); 
  gtk_widget_show (toolbar);
  toolbar = gtk_ui_manager_get_widget (ui_manager, "/EntryToolBar");
  //g_print("EntryToolbar is %p\n", toolbar);
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_TEXT);
  gtk_box_pack_start (GTK_BOX (main_vbox), toolbar, FALSE, TRUE, 0);
  GTK_WIDGET_UNSET_FLAGS(toolbar, GTK_CAN_FOCUS);

  // gtk_widget_show (toolbar); cannot show this until the GtkLabels have become GtkAccelLabels - a gtk bug

  toolbar = gtk_ui_manager_get_widget (ui_manager, "/RhythmToolBar");
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_TEXT);
  gtk_box_pack_start (GTK_BOX (main_vbox), toolbar, FALSE, TRUE, 0);

  menubar = gtk_ui_manager_get_widget (ui_manager, "/ObjectMenu");
  if(menubar) {
    gtk_box_pack_start (GTK_BOX (main_vbox), menubar, FALSE, TRUE, 0);
  }


  menubar = gtk_ui_manager_get_widget (ui_manager, "/ActionMenu");
  if(menubar) {
    gtk_box_pack_start (GTK_BOX (main_vbox), menubar, FALSE, TRUE, 0);
  }

  Denemo.notebook = gtk_notebook_new ();
  gtk_notebook_set_show_tabs (GTK_NOTEBOOK(Denemo.notebook), FALSE);//only show when more than one
  //gtk_notebook_popup_enable (Denemo.notebook);?? doesn't work...
  gtk_widget_show (Denemo.notebook);
  gtk_box_pack_start (GTK_BOX (main_vbox), Denemo.notebook, TRUE, TRUE, 0);


  Denemo.statusbar = gtk_statusbar_new ();
  hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (hbox), Denemo.statusbar, TRUE, TRUE, 5);
  gtk_widget_show (Denemo.statusbar);
  Denemo.status_context_id =
    gtk_statusbar_get_context_id (GTK_STATUSBAR (Denemo.statusbar), "Denemo");
  gtk_statusbar_push (GTK_STATUSBAR (Denemo.statusbar), Denemo.status_context_id,
		      "Denemo");

  gtk_widget_show (hbox);

  populate_opened_recent ();
  gtk_widget_show(Denemo.window);
  /* Now that the window is shown, initialize the gcs */
  gcs_init (Denemo.window->window);

  /* we have to do this properly, because it introduces a keymap - no longer true */
  if (Denemo.prefs.rhythm_palette) {
    GtkWidget *widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/RhythmToolBar");
    if (GTK_WIDGET_VISIBLE (widget))
      gtk_widget_hide(widget);// I do not understand why this is visible - there is no gtk_widget_show(all) in the hierarchy
    widget = gtk_ui_manager_get_widget (Denemo.ui_manager, "/MainMenu/ViewMenu/ToggleRhythmToolbar");
    g_signal_emit_by_name(widget, "activate", NULL, Denemo.gui);
    //g_print("type is %s\n", g_type_name(G_TYPE_FROM_INSTANCE(widget))); 
    }
  // A cheap way of doing activating this toolbar, note it is called variously notation toolbar, duration toolbar and EntryToolBar FIXME
  if (!Denemo.prefs.notation_palette)
    {
      //g_print ("Notation palette %d\n", Denemo.prefs.notation_palette);
      toggle_entry_toolbar (NULL, Denemo.gui);
    }
 
 
  data_dir = g_build_filename (
#ifndef USE_LOCAL_DENEMOUI
get_data_dir (),
#endif
 "denemoui.xml", NULL);
  parse_paths(data_dir, Denemo.gui);


  use_markup(Denemo.window);/* set all the labels to use markup so that we can use the music font. Be aware this means you cannot use labels involving "&" "<" and ">" and so on without escaping them 
FIXME labels in toolitems are not correct until you do NewWindow.
Really we should change the default for the class.*/
  action_group = gtk_action_group_new ("LilyActions");
  gtk_action_group_add_actions (action_group, lily_menus,
				G_N_ELEMENTS (lily_menus), Denemo.gui);
  gtk_ui_manager_insert_action_group (ui_manager, action_group, 1);
 //  g_print("Turning on the modes\n");


 //write_status(Denemo.gui);
 Denemo.InsertModeMenu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu/InsertModeNote");
 Denemo.EditModeMenu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu/EditModeNote");
 Denemo.ClassicModeMenu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu/ClassicModeNote");
 Denemo.ModelessMenu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/ObjectMenu/NoteProperties");
 gtk_widget_show (Denemo.InsertModeMenu);
 gtk_widget_hide (Denemo.EditModeMenu);
 gtk_widget_hide (Denemo.ClassicModeMenu);
 gtk_widget_hide (Denemo.ModelessMenu);
 gtk_widget_hide (gtk_ui_manager_get_widget (ui_manager, "/ActionMenu"));// make a prefs thing
 gtk_widget_hide (gtk_ui_manager_get_widget (ui_manager, "/EntryToolBar")); //otherwise buttons only sensitive around their edges

#ifdef G_OS_WIN32
 toolbar = gtk_ui_manager_get_widget (ui_manager, "/EntryToolBar");
 gtk_widget_show (toolbar);
#endif

 {GtkToggleAction *action;
 action = (GtkToggleAction *)gtk_ui_manager_get_action (Denemo.ui_manager, "/MainMenu/ViewMenu/ToggleObjectMenu");
 gtk_toggle_action_set_active (action, TRUE);
 toggle_object_menu (NULL, Denemo.gui);
 }
  g_signal_connect (G_OBJECT(Denemo.notebook), "switch_page", G_CALLBACK(switch_page), NULL);
}   /* create window */

/**
 * Creates a new DenemoGUI structure represented by a tab in a notebook to control one musical score
 * of possibly several movements. 
 * This DenemoGUI* gui is appended to the global list Denemo.guis.
 * A single movement (DenemoScore) is instantiated in the gui.
 * 
 */
void
newview (GtkAction *action)
{
  if(Denemo.guis==NULL)
    create_window();
  DenemoGUI *gui = (DenemoGUI *) g_malloc0 (sizeof (DenemoGUI));
  Denemo.guis = g_list_append (Denemo.guis, gui);


  Denemo.gui = NULL;
  // Denemo.gui = gui; must do this after switching to page, so after creating page
  gui->lilycontrol.papersize = g_string_new ("a4");	//A4 default
  gui->lilycontrol.fontsize = 16;
  gui->lilycontrol.lilyversion = g_string_new (LILYPOND_VERSION);
  gui->lilycontrol.orientation = TRUE;	//portrait
  gui->lilycontrol.lilypond = g_string_new ("\\transpose c c");


  gui->pixmap = NULL;

  /* Initialize the GUI */

  //create the tab for this gui
  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);

  gint pagenum = gtk_notebook_append_page (GTK_NOTEBOOK (Denemo.notebook), main_vbox, NULL);
  gui->page = gtk_notebook_get_nth_page (GTK_NOTEBOOK(Denemo.notebook), pagenum);
  gtk_notebook_set_current_page (GTK_NOTEBOOK(Denemo.notebook), pagenum);
Denemo.gui = gui;
 if(pagenum)
   gtk_notebook_set_show_tabs (GTK_NOTEBOOK(Denemo.notebook), TRUE);
  set_title_bar(gui);
  gtk_widget_show (main_vbox);
  GtkWidget *score_and_scroll_hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_vbox), score_and_scroll_hbox, TRUE, TRUE,
		      0);
  gtk_widget_show (score_and_scroll_hbox);
  //gtk_grab_remove(toolbar);  ?????????
  gui->scorearea = gtk_drawing_area_new ();



  gtk_box_pack_start (GTK_BOX (score_and_scroll_hbox), gui->scorearea, TRUE,
		      TRUE, 0);// with this, the scoreare_expose_event is called
  gtk_widget_show (gui->scorearea);

  gui->vadjustment = gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0);
  gtk_signal_connect (GTK_OBJECT (gui->vadjustment), "value_changed",
		      GTK_SIGNAL_FUNC (vertical_scroll), gui);
  gui->vscrollbar = gtk_vscrollbar_new (GTK_ADJUSTMENT (gui->vadjustment));
  gtk_box_pack_start (GTK_BOX (score_and_scroll_hbox), gui->vscrollbar, FALSE,
		      TRUE, 0);
  gtk_widget_show (gui->vscrollbar);

  gui->hadjustment = gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0);

  gtk_signal_connect (GTK_OBJECT (gui->hadjustment), "value_changed",
		      GTK_SIGNAL_FUNC (horizontal_scroll), gui);
  gui->hscrollbar = gtk_hscrollbar_new (GTK_ADJUSTMENT (gui->hadjustment));
  gtk_box_pack_start (GTK_BOX (main_vbox), gui->hscrollbar, FALSE, TRUE, 0);
  gtk_widget_show (gui->hscrollbar);

  GtkWidget *hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);
  gtk_widget_show (hbox);


  //FIXME populate_opened_recent (gui);

  /* create the first movement now because showing the window causes it to try to draw the scorearea
   which it cannot do before there is a score. */
  new_score (gui);
  gui->movements = g_list_append(NULL, gui->si);
 

  gtk_widget_show (gui->page);
  gtk_widget_grab_focus (gui->scorearea);



 create_rhythm_cb((gpointer)insert_chord_0key); 
 create_rhythm_cb((gpointer)insert_chord_1key);   
 create_rhythm_cb((gpointer)insert_chord_2key);   
 create_rhythm_cb((gpointer)insert_chord_3key);   
 create_rhythm_cb((gpointer)insert_chord_4key);   
 create_rhythm_cb((gpointer)insert_chord_5key); 
 create_rhythm_cb((gpointer)insert_chord_6key);   


 create_rhythm_cb((gpointer)insert_rest_0key); 
 create_rhythm_cb((gpointer)insert_rest_1key);   
 create_rhythm_cb((gpointer)insert_rest_2key);   
 create_rhythm_cb((gpointer)insert_rest_3key);   
 create_rhythm_cb((gpointer)insert_rest_4key);   
 create_rhythm_cb((gpointer)insert_rest_5key); 
 create_rhythm_cb((gpointer)insert_rest_6key);   


  if (Denemo.prefs.articulation_palette)
    toggle_articulation_palette (NULL);
  Denemo.gui->mode = INPUTINSERT | INPUTNORMAL;

  // this stops the keyboard input from getting to  scorearea_keypress_event if done after attaching the signal, why?
  gtk_notebook_set_current_page (GTK_NOTEBOOK(Denemo.notebook), pagenum);


  GTK_WIDGET_SET_FLAGS(gui->scorearea, GTK_CAN_FOCUS);
  gtk_widget_grab_focus (GTK_WIDGET(gui->scorearea));
  g_signal_connect (G_OBJECT (gui->scorearea), "expose_event",
		      G_CALLBACK (scorearea_expose_event), gui);
  g_signal_connect (G_OBJECT (gui->scorearea), "configure_event",
		      G_CALLBACK (scorearea_configure_event), gui);


  g_signal_connect (G_OBJECT (gui->scorearea), "button_release_event",
		      G_CALLBACK (scorearea_button_release), gui);

  g_signal_connect (G_OBJECT (gui->scorearea), "motion_notify_event",
		      G_CALLBACK (scorearea_motion_notify), gui);

  g_signal_handlers_block_by_func(gui->scorearea, G_CALLBACK (scorearea_motion_notify), gui);
  g_signal_connect (G_OBJECT (gui->scorearea), "button_press_event",
		      G_CALLBACK (scorearea_button_press), gui);
  //  gtk_signal_connect (GTK_OBJECT (gui->page), "delete_event",
  //		      (GtkSignalFunc) delete_callback, gui);
  gtk_signal_connect (GTK_OBJECT (gui->scorearea), "key_press_event",
		      (GtkSignalFunc) scorearea_keypress_event, gui);




  gtk_widget_add_events/*gtk_widget_set_events*/ (gui->scorearea, (GDK_EXPOSURE_MASK
					  | GDK_POINTER_MOTION_MASK
					  | GDK_LEAVE_NOTIFY_MASK
					  | GDK_BUTTON_PRESS_MASK
					  | GDK_BUTTON_RELEASE_MASK));



  /* Set up the keymap if not already set up*/
 {static gboolean initialized = FALSE;
 g_print("init %d for %p\n", initialized, Denemo.gui);
 if(!initialized) {
   init_keymap();
   /*
     The next loop goes through the actions and connects a signal to menu_click. This allows the shortcuts to be modified/inspected on the menu item itself, by right-clicking. It also provides the help for the menuitem.
     Furthermore, it updates the label of the widgets proxying the action so that accelerators are also displayed.
     FIXME: it shouldn't allow shortcuts to be set on menus, only on menuitems (fix in the callback).
   */
   GtkActionGroup *action_group;
   GList *groups = gtk_ui_manager_get_action_groups (Denemo.ui_manager);
   action_group = GTK_ACTION_GROUP(groups->data); 
   GList *g = gtk_action_group_list_actions(action_group);
   for(;g;g=g->next) {
     gint command_idx;
     keymap *the_keymap = Denemo.prefs.the_keymap;
     GSList *h = gtk_action_get_proxies (g->data);
     for(;h;h=h->next) {
#if (GTK_MINOR_VERSION <10)
       attach_action_to_widget(h->data, g->data, Denemo.gui);
#endif
       attach_set_accel_callback(h->data, g->data,"", Denemo.gui);
       command_idx = lookup_index_from_name(the_keymap,
					    gtk_action_get_name(g->data));
       if (command_idx != -1) {
	 update_accel_labels(the_keymap, command_idx);
       }
     }
   }
   initialized = TRUE;
 }
 if (Denemo.prefs.autosave) {
   if(Denemo.autosaveid) {
     g_print("No autosave on new gui");
   }
   else {
     Denemo.autosaveid = g_timeout_add (Denemo.prefs.autosave_timeout * 1000 * 60,
					(GSourceFunc) auto_save_document_timeout, Denemo.gui);
   }
 }

 }





}

