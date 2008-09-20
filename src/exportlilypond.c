/* exportlilypond.c
 * Functions for actually exporting what Denemo's working on to a LilyPond file
 *
 * AJT 14/3/2000 Parametised for quick midi playback
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000, 2001, 2002 Matthew Hiller, Adam Tee
 */



#include "config.h"
#include <denemo/denemo.h>
#include "utils.h"
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

#include "lyparserfuncs.h"
/*#include "lyparser.h"*/
#include "exportlilypond.h"
#include "print.h"
#include "scoreops.h"
#include "objops.h"
#include "xmldefs.h"

#define ENTER_NOTIFY_EVENT "focus-in-event"
#define LEAVE_NOTIFY_EVENT "focus-out-event"

#define SIGNAL_WIDGET gui->textwindow /* Denemo.window */

#define TARGET "target"
#define MUSIC "music"
#define START "start"
#define SCOREBLOCK "scoreblock"
#define CUSTOM "custom"
#define OBJECT "object"
#define ORIGINAL "original"

#define OBJECTNUM "objectnum"
#define MEASURENUM "measurenum"
#define STAFFNUM "staffnum"
#define MOVEMENTNUM "movementnum"
#define STANDARD_SCOREBLOCK "Standard scoreblock"
#define INEDITABLE "ineditable"
#define HIGHLIGHT "highlight"
#define ERRORTEXT "error text"


#define TAB "        "

static void 
create_lilywindow(DenemoGUI *gui);
static void
output_score_to_buffer (DenemoGUI *gui, gboolean all_movements, gchar * partname);
static GtkTextTagTable *tagtable;

void
highlight_lily_error(DenemoGUI *gui) {
  if(gui->textbuffer==NULL)
    return;
  GtkTextIter enditer, iter;
  gtk_text_buffer_get_end_iter (gui->textbuffer, &enditer);
  gtk_text_buffer_get_start_iter (gui->textbuffer, &iter);
  gtk_text_buffer_remove_tag_by_name(gui->textbuffer, ERRORTEXT, &enditer, &iter);
  gint line, column;
  line = (gint)g_object_get_data(G_OBJECT(gui->textbuffer), "error line");
  column = (gint)g_object_get_data(G_OBJECT(gui->textbuffer), "error column");
  line--;
  if(line>0) {
#ifdef BUG_COLUMN_OFFSET_TOO_LARGE_FIXED
    gtk_text_buffer_get_iter_at_line_offset
      (gui->textbuffer,
       &iter,
       line,
       column);
#else
    gtk_text_buffer_get_iter_at_line_offset
      (gui->textbuffer,
       &iter,
       line,
       0);
    g_print("line %d column %d\n", line, column);
    g_print("line has %d chars\n", gtk_text_iter_get_chars_in_line(&iter));
    while(column--) 
      (void)gtk_text_iter_forward_char(&iter);//EEEK TAB is 8 spaces for lilypond find these!!!!

#endif
    /*     gtk_text_iter_set_line(&iter, line); */
    /*     gtk_text_iter_set_visible_line_offset(&iter, column); */

    gtk_text_buffer_apply_tag_by_name(gui->textbuffer, ERRORTEXT, &enditer, &iter);
  }
}


/*  set_lily_error() 
 *  set line, column as the current line and column in gui->textbuffer where an error has been found
 *  in the LilyPond interpreter. line starts from 1, column starts from 0
 *  line=0 means no error
 */
void
set_lily_error(gint line, gint column, DenemoGUI *gui) {
  if(gui->textbuffer){
    g_object_set_data(G_OBJECT(gui->textbuffer),"error line", (gpointer)line);
    g_object_set_data(G_OBJECT(gui->textbuffer),"error column", (gpointer)column);
  }
}


/* pop up an appropriate menu for the section attached to the button */
static gboolean popup_menu(GtkButton *button,GdkEvent *event, DenemoGUI *gui) {
  GtkTextChildAnchor *anchor = g_object_get_data(G_OBJECT(button), "anchor");
  GtkWidget *menu;
  if(g_object_get_data(G_OBJECT(anchor), STANDARD_SCOREBLOCK))
    menu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/LilyScoreblockMenu");
  else {
    if(g_object_get_data(G_OBJECT(anchor), CUSTOM))
      menu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/LilyCustomScoreblockMenu");
    else
      menu = gtk_ui_manager_get_widget (Denemo.ui_manager, "/LilyMenu");
  }


  //g_print("type of menu is %s\n", g_type_name(G_TYPE_FROM_INSTANCE(menu)));
  // g_print("gui->custom_scoreblock = %p\n", g_object_get_data(button, "DenemoObject"));
  gui->lilystart = anchor;
  //gui->lilyend = g_object_get_data(G_OBJECT(anchor), "end");
  //gui->target = g_object_get_data(G_OBJECT(anchor), TARGET);
  gtk_menu_popup (GTK_MENU(menu), NULL, NULL, NULL, NULL,0, gtk_get_current_event_time()); 
  //event_button->button, event_button->time);

  return TRUE;

}

/* insert a pair of anchors and a mark to denote a section.
   if str is non-null it is a target for saving edited versions of the section to, in this
   case the start anchor of the section is prepended to the list gui->anchors 
   if name is non-null a button is attached to the start anchor.
*/
static GtkTextChildAnchor * insert_section(GString **str, gchar *markname, gchar *name, GtkTextIter *iter, DenemoGUI *gui)
{
  GtkTextIter back;
  GtkTextChildAnchor *objanc = gtk_text_buffer_create_child_anchor (gui->textbuffer, iter);
  back = *iter;
  (void)gtk_text_iter_backward_char(&back);
  gtk_text_buffer_apply_tag_by_name(gui->textbuffer, INEDITABLE, &back, iter);
  if(name==NULL)
    gtk_text_buffer_apply_tag_by_name(gui->textbuffer, "system_invisible", &back, iter);
  gtk_text_buffer_insert (gui->textbuffer, iter, "\n", -1);
  GtkTextChildAnchor *endanc = gtk_text_buffer_create_child_anchor (gui->textbuffer, iter);
  back = *iter;
  (void)gtk_text_iter_backward_char(&back);
  gtk_text_buffer_apply_tag_by_name(gui->textbuffer, INEDITABLE, &back, iter);

  gtk_text_buffer_apply_tag_by_name(gui->textbuffer, "system_invisible", &back, iter); 
  g_object_set_data(G_OBJECT(objanc), "end", (gpointer)endanc);
  g_object_set_data(G_OBJECT(objanc), TARGET, (gpointer)str);
  if(str)
    gui->anchors = g_list_prepend(gui->anchors, objanc);

  if(name) {
    GtkWidget *button = gtk_button_new ();
    char *markup = g_markup_printf_escaped ("<tt>%s</tt>", name);//monospace label to get serifs
    GtkWidget *label = gtk_label_new ("");
    gtk_label_set_markup (GTK_LABEL (label), markup);
    g_free (markup);
    gtk_container_add (GTK_CONTAINER(button), label);
    gtk_text_view_add_child_at_anchor (GTK_TEXT_VIEW(gui->textview), button, objanc);
    g_object_set_data(G_OBJECT(button), "anchor", (gpointer)objanc);
    g_signal_connect (G_OBJECT (button), "button-press-event",
		      G_CALLBACK (popup_menu), gui);
    gtk_widget_show_all(button);
  }
  (void)gtk_text_iter_backward_char(iter);
  (void)gtk_text_iter_backward_char(iter);
  gtk_text_buffer_create_mark (gui->textbuffer, markname, iter, FALSE);
  return objanc;
}


#define FIGURES_SEP "|"
/* a separator for groups of figured bass figures on one note
   this could be a user preference thingy */


/**
 * Output the lilypond representation of the given keysignature 
 *
 */
static void
determinekey (gint number, gchar ** keyname)
{
  switch (number)
    {
    case -7:
      *keyname = "ces";
      break;
    case -6:
      *keyname = "ges";
      break;
    case -5:
      *keyname = "des";
      break;
    case -4:
      *keyname = "aes";
      break;
    case -3:
      *keyname = "ees";
      break;
    case -2:
      *keyname = "bes";
      break;
    case -1:
      *keyname = "f";
      break;
    case 0:
      *keyname = "c";
      break;
    case 1:
      *keyname = "g";
      break;
    case 2:
      *keyname = "d";
      break;
    case 3:
      *keyname = "a";
      break;
    case 4:
      *keyname = "e";
      break;
    case 5:
      *keyname = "b";
      break;
    case 6:
      *keyname = "fis";
      break;
    case 7:
      *keyname = "cis";
      break;
    case 8:
      *keyname = "gis";
      break;
    case 9:
      *keyname = "dis";
      break;
    case 10:
      *keyname = "ais";
      break;
    default:
      *keyname = _("%{error. defaulting to%}c");
      break;
    }
}

/**
 * Output the lilypond representataion of the given clef
 *
 */
static void
determineclef (gint type, gchar ** clefname)
{
  switch (type)
    {
    case DENEMO_TREBLE_CLEF:
      *clefname = "treble";
      break;
    case DENEMO_BASS_CLEF:
      *clefname = "bass";
      break;
    case DENEMO_ALTO_CLEF:
      *clefname = "alto";
      break;
    case DENEMO_G_8_CLEF:
      *clefname = "\"G_8\"";
      break;
    case DENEMO_TENOR_CLEF:
      *clefname = "tenor";
      break;
    case DENEMO_SOPRANO_CLEF:
      *clefname = "soprano";
      break;
    default:
      *clefname = _("%{error. defaulting to%}treble");
      break;
    }
  /* I've found the quotes are necessary for ^ and _ clefs
   * to parse correctly */
}

/**
 * Convert denemo duration to lilypond duration
 */
static gint
internaltomuduration (gint internalduration)
{
  return 1 << internalduration;
}


/**
 * append mudela duration information to FIGURES.
 * This could be optimized to remember the previous value and
 * avoid repetition - an initialization call would be needed to
 * set up initial values in that case
 */
static void
append_duration (GString * figures, gint duration, gint numdots)
{
  int i;
  g_string_sprintfa (figures, "%d", duration);
  for (i = 0; i < numdots; i++)
    figures = g_string_append (figures, ".");
}

static void output_lyric(GString *lyrics, chord *pchord, gboolean *pis_syllable, gboolean *pcenter_lyric) {
  /*Lyrics */
		 
  gint duration = internaltomuduration (pchord->baseduration);
  if (pchord->lyric)
    {	 
      if (*pis_syllable && !pchord->is_syllable)
	lyrics = g_string_append (lyrics, " ");
      lyrics = g_string_append (lyrics, pchord->lyric->str);
		  
      if (pchord->center_lyric)
	{
	  lyrics = g_string_append (lyrics, "-- ");
	  *pcenter_lyric = TRUE;// FIXME unused
	}
      else
	*pcenter_lyric = FALSE;
		  
      if (pchord->is_syllable)
	{
	  *pis_syllable = TRUE;
	  lyrics = g_string_append (lyrics, "__ ");
	}
      else
	{
	  *pis_syllable = FALSE;
	  lyrics = g_string_append (lyrics, " ");
	}
    }
  else if (*pis_syllable)
    {
      gint extend;
      //g_print ("duration %d\t mod %d\n", duration, duration % 4);
      if (duration < 4)
	{
	  for (extend = 0; extend < duration % 4; extend++)
	    lyrics = g_string_append (lyrics, "__ ");
	}
      else if (duration > 4)
	{
	  for (extend = 0; extend < 4 % duration; extend++)
	    lyrics = g_string_append (lyrics, "_");
	}
      else
	lyrics = g_string_append (lyrics, "__ ");
    } else
      lyrics = g_string_append (lyrics, "--");//FIXME is this right?
}



/**
 * add figures to *pfigures for *pchord  
 */
static void
output_figured_bass (DenemoScore * si, GString *figures, chord * pchord)
{
  gint duration = internaltomuduration (pchord->baseduration);
  gint numdots = pchord->numdots;
  GString *fig_str;		/*working copy of figures string stored in pchord */
  char *str;			/* pointer into the figure string fig_str */
  gint num_groups = 1;		/* number of groups of figures */

  figures = g_string_append (figures, "<");
  if (pchord->figure == NULL
      ||
      (((GString
	 *) ((chord *) pchord->figure))->str) == NULL)
    fig_str = g_string_new ("_");	/* the no-figure figure */
  else
    fig_str =
      g_string_new (((GString
		      *) ((chord *) pchord->figure))->str);
		      
		    

  /* multiple figures are separated by a FIGURES_SEP char,
     output these at subdivisions of the duration */
  str = strchr (fig_str->str, *(char *) FIGURES_SEP);
  if (str != NULL)
    {
      /* we have more than one group of figures to be output
         for one bass note. Count the number of groups */
      num_groups = 2;
      /* one on either side of the FIGURES_SEP found */
      while ((str = strchr (++str, *(char *) FIGURES_SEP)) != NULL)
	num_groups++;
    }

  switch (num_groups)
    {
    default:
    case 1:
      figures = g_string_append (figures, fig_str->str);
      figures = g_string_append (figures, ">");
      append_duration (figures, duration, numdots);
      break;
      /* Each group of figures is assigned a duration to
         achieve a normal looking output */
    case 2:
      {
	gint first_duration, second_duration;
	if (numdots)
	  {			/* divide unequally */
	    first_duration = duration;
	    second_duration = duration * 2;
	  }
	else
	  {
	    first_duration = second_duration = duration * 2;
	  }
	str = strtok (fig_str->str, FIGURES_SEP);
	figures = g_string_append (figures, str);
	figures = g_string_append (figures, ">");
	append_duration (figures, first_duration, 0);
	figures = g_string_append (figures, "<");
	str = strtok (NULL, FIGURES_SEP);
	figures = g_string_append (figures, str);
	figures = g_string_append (figures, ">");
	append_duration (figures, second_duration, 0);
      }
      break;
    case 3:
      {
	gint first_duration, second_duration, third_duration;
	if (numdots == 1)
	  {			/* divide equally */

	    first_duration = second_duration = third_duration = duration * 2;
	  }
	else if (numdots == 2)
	  {
	    first_duration = second_duration = duration * 2;
	    third_duration = duration * 4;
	  }			/* no more dots please! */
	else
	  {			/* divide unequally */
	    first_duration = duration * 2;
	    second_duration = third_duration = duration * 4;
	  }
	str = strtok (fig_str->str, FIGURES_SEP);
	figures = g_string_append (figures, str);
	figures = g_string_append (figures, ">");
	append_duration (figures, first_duration, 0);
	figures = g_string_append (figures, "<");
	str = strtok (NULL, FIGURES_SEP);
	figures = g_string_append (figures, str);
	figures = g_string_append (figures, ">");
	append_duration (figures, second_duration, 0);
	str = strtok (NULL, FIGURES_SEP);
	figures = g_string_append (figures, "<");
	figures = g_string_append (figures, str);
	figures = g_string_append (figures, ">");
	append_duration (figures, third_duration, 0);
      }
      break;
    case 4:
      {
	gint first_duration, second_duration, third_duration, fourth_duration;
	if (numdots == 1)
	  {			/* divide unequally */

	    first_duration = second_duration = duration * 2;
	    third_duration = fourth_duration = duration * 4;
	  }
	else if (numdots == 2)
	  {
	    first_duration = second_duration = duration * 2;
	    third_duration = duration * 4;
	    fourth_duration = duration * 8;
	  }			/* no more dots please! */
	else
	  {			/* divide equally */
	    first_duration =
	      second_duration = third_duration = fourth_duration = duration * 4;
	  }
	str = strtok (fig_str->str, FIGURES_SEP);
	figures = g_string_append (figures, str);
	figures = g_string_append (figures, ">");
	append_duration (figures, first_duration, 0);
	figures = g_string_append (figures, "<");
	str = strtok (NULL, FIGURES_SEP);
	figures = g_string_append (figures, str);
	figures = g_string_append (figures, ">");
	append_duration (figures, second_duration, 0);
	str = strtok (NULL, FIGURES_SEP);
	figures = g_string_append (figures, "<");
	figures = g_string_append (figures, str);
	figures = g_string_append (figures, ">");
	append_duration (figures, third_duration, 0);
	str = strtok (NULL, FIGURES_SEP);
	figures = g_string_append (figures, "<");
	figures = g_string_append (figures, str);
	figures = g_string_append (figures, ">");
	append_duration (figures, fourth_duration, 0);
      }
      break;
    }
}

/**
 * add figures to *pfigures for *pchord  
 */
static void
output_fakechord (DenemoScore * si, GString *fakechord, chord * pchord)
{
  gint duration = internaltomuduration (pchord->baseduration);
  gint numdots = pchord->numdots;
  GString *fig_str, *extension;		/*working copy of figures string */
  char *str;			/* pointer into the figure string fig_str */
  gint num_groups = 1;		/* number of groups of figures */

  fakechord = g_string_append (fakechord, " ");
  if (pchord->fakechord == NULL
      ||
      (((GString
	 *) ((chord *) pchord->fakechord))->str) == NULL)
    fig_str = g_string_new (" r");	/* the no-fakechord figure */
  else {
    fig_str =
      g_string_new (((GString
		      *) ((chord *) pchord->fakechord))->str);
  }
  if (pchord->fakechord_extension == NULL)
    extension = NULL;
  else {
    extension =
      g_string_new (((GString
		      *) ((chord *) pchord->fakechord_extension))->str);
  }      
		      

  str = strchr (fig_str->str, *(char *) FIGURES_SEP);
  if (str != NULL)
    {
      /* we have more than one group of figures to be output
         for one bass note. Count the number of groups */
      num_groups = 2;
      /* one on either side of the FIGURES_SEP found */
      while ((str = strchr (++str, *(char *) FIGURES_SEP)) != NULL)
	num_groups++;
    }

  switch (num_groups)
    {
    default:
    case 1:
      fakechord = g_string_append (fakechord, fig_str->str);
      //fakechord = g_string_append (fakechord, " ");
      append_duration (fakechord, duration, numdots);
      if (extension != NULL)
      	//printf("\nhas extenion in export mudela\n");
	fakechord = g_string_append (fakechord, extension->str);
      break;
      /* Each group of fakechord is assigned a duration to
         achieve a normal looking output */
    case 2:
      {
	gint first_duration, second_duration;
	if (numdots)
	  {		/* divide unequally */
	    first_duration = duration;
	    second_duration = duration * 2;
	  }
	else
	  {
	    first_duration = second_duration = duration * 2;
	  }
	str = strtok (fig_str->str, FIGURES_SEP);
	fakechord = g_string_append (fakechord, str);
	fakechord = g_string_append (fakechord, " ");
	append_duration (fakechord, first_duration, 0);
	fakechord = g_string_append (fakechord, " ");
	str = strtok (NULL, FIGURES_SEP);
	fakechord = g_string_append (fakechord, str);
	fakechord = g_string_append (fakechord, " ");
	append_duration (fakechord, second_duration, 0);
      }
      break;
    case 3:
      {
	gint first_duration, second_duration, third_duration;
	if (numdots == 1)
	  {			/* divide equally */

	    first_duration = second_duration = third_duration = duration * 2;
	  }
	else if (numdots == 2)
	  {
	    first_duration = second_duration = duration * 2;
	    third_duration = duration * 4;
	  }			/* no more dots please! */
	else
	  {			/* divide unequally */
	    first_duration = duration * 2;
	    second_duration = third_duration = duration * 4;
	  }
	str = strtok (fig_str->str, FIGURES_SEP);
	fakechord = g_string_append (fakechord, str);
	fakechord = g_string_append (fakechord, " ");
	append_duration (fakechord, first_duration, 0);
	fakechord = g_string_append (fakechord, " ");
	str = strtok (NULL, FIGURES_SEP);
	fakechord = g_string_append (fakechord, str);
	fakechord = g_string_append (fakechord, " ");
	append_duration (fakechord, second_duration, 0);
	str = strtok (NULL, FIGURES_SEP);
	fakechord = g_string_append (fakechord, " ");
	fakechord = g_string_append (fakechord, str);
	fakechord = g_string_append (fakechord, " ");
	append_duration (fakechord, third_duration, 0);
      }
      break;
    }
}

/*
 * insert_editable()
 * Insert pair of invisble anchors and editable text between, adding the start anchor to the list in gui->anchors
 * if directive is NULL or empty string provide a space for editing.
 * ORIGINAL: string containing text to initialize with: the caller owns this string
 * DIRECTIVE: pointer to a target GString where changes should be stored,
 *            or NULL if editable text is to be allowed here (an editable space is inserted in this case)
 * ITER: the current iter in gui->textbuffer
 * INVISIBILITY: tag name to use for the insert (may be NULL)
 * GUI: the gui with the textbuffer
 * 
 */
static void
insert_editable (GString **pdirective, gchar *original, GtkTextIter *iter, gchar *invisibility, DenemoGUI *gui) {
  GString *directive;
  if(pdirective) directive = *pdirective;
  GtkTextChildAnchor *lilyanc = gtk_text_buffer_create_child_anchor (gui->textbuffer, iter);
  GtkTextIter back;
  back = *iter;
  (void)gtk_text_iter_backward_char(&back);
  gtk_text_buffer_apply_tag_by_name(gui->textbuffer, INEDITABLE, &back, iter);
  gtk_text_buffer_apply_tag_by_name(gui->textbuffer, "system_invisible", &back, iter);
  g_object_set_data(G_OBJECT(lilyanc), TARGET, (gpointer)pdirective);
  g_object_set_data(G_OBJECT(lilyanc), ORIGINAL, original);

  gui->anchors = g_list_prepend(gui->anchors, lilyanc);
  gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, iter, g_strdup(original), -1, "bold", invisibility, NULL);

  GtkTextChildAnchor *endanc  = gtk_text_buffer_create_child_anchor (gui->textbuffer, iter);
  back = *iter;
  (void)gtk_text_iter_backward_char(&back);
  gtk_text_buffer_apply_tag_by_name(gui->textbuffer, INEDITABLE, &back, iter);
  gtk_text_buffer_apply_tag_by_name(gui->textbuffer, "system_invisible", &back, iter);
  g_object_set_data(G_OBJECT(lilyanc), "end", (gpointer)endanc);
  gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, iter, " ", -1, HIGHLIGHT, invisibility, NULL);
}

/**
 * generate the lilypond for the DenemoObject curobj
 * the state of the prevailing duration, clef keysignature are updated and returned.
 */
static void
generate_lily_for_obj (DenemoGUI *gui, GtkTextIter *iter, gchar *invisibility, DenemoObject * curobj, 
		       GtkTextChildAnchor *objanc,		       
		       gint * pprevduration, gint * pprevnumdots,
		       gchar ** pclefname,
		       gchar ** pkeyname, gint * pcur_stime1,
		       gint * pcur_stime2)
{
  GString *ret = g_string_new ("");
#define outputret gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, iter, ret->str, -1, INEDITABLE, invisibility, NULL), g_string_assign(ret, "")
#define output(astring) (gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, iter, astring, -1, INEDITABLE, invisibility, NULL))
  gint prevduration = *pprevduration;
  gint prevnumdots = *pprevnumdots;
  chord *pchord;
  gchar *clefname = *pclefname;
  gchar *keyname = *pkeyname;
  gint cur_stime1 = *pcur_stime1;
  gint cur_stime2 = *pcur_stime2;

  gchar temp[50];
  gint j, k;
  gint duration, numdots;
  gboolean is_normalnotehead = TRUE;
  gboolean is_chordmode = FALSE;
  gint octave, enshift;
  gint noteheadtype;
  gint mid_c_offset;


  GString *dynamic_string = NULL;

  if(curobj->type==LILYDIRECTIVE){    
    gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, iter,  ((lilydirective *) curobj->object)->directive->str, -1, "bold", invisibility, NULL);// Make it editable as well, the rest not...
    GtkTextChildAnchor *endanc  = gtk_text_buffer_create_child_anchor (gui->textbuffer, iter);
    GtkTextIter back;
    back = *iter;
    (void)gtk_text_iter_backward_char(&back);
    gtk_text_buffer_apply_tag_by_name(gui->textbuffer, INEDITABLE, &back, iter);
    gtk_text_buffer_apply_tag_by_name(gui->textbuffer, "system_invisible", &back, iter);
    g_object_set_data(G_OBJECT(objanc), "end", (gpointer)endanc);
    g_object_set_data(G_OBJECT(objanc), TARGET, (gpointer)&((lilydirective *) curobj->object)->directive);
    gui->anchors = g_list_prepend(gui->anchors, objanc);
    prevduration = -1;
    prevnumdots = -1;// the LILYDIRECTIVE may have changed the duration
  }  else {
    gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, iter, " ", -1, INEDITABLE, HIGHLIGHT, invisibility, NULL);

    switch (curobj->type)
      {
      case CHORD:

	pchord = (chord *) curobj->object;
	duration = internaltomuduration (pchord->baseduration);
	numdots = pchord->numdots;
	is_chordmode = FALSE;

	if (!pchord->notes)
	  {			/* A rest */
	    if (!curobj->isinvisible)
	      {
		g_string_append_printf (ret, "r");
		/* Duplicated code follows. I ought to fix that */
		if (duration != prevduration || numdots != prevnumdots)
		  {
		    /* only in this case do we explicitly note the duration */
		    g_string_append_printf (ret, "%d", duration);
		    prevduration = duration;
		    prevnumdots = numdots;
		    for (j = 0; j < numdots; j++)
		      g_string_append_printf (ret, ".");
		  }
	      }
	    else
	      {
		g_string_append_printf (ret, "\\skip ");
		/* only in this case do we explicitly
		   note the duration */
		g_string_append_printf (ret, "%d", duration);
		prevduration = duration;
		prevnumdots = numdots;
		for (j = 0; j < numdots; j++)
		  g_string_append_printf (ret, ".");
	      }
	    outputret;
	  } 
	else /* there are notes */
	  {
	    GList *tmpornament;
	    if (!curobj->isinvisible)
	      {
		if (pchord->notes->next  || ((note *) (pchord->notes)->data)->directive )//multinote chord, or treat as such
		  {
		    is_chordmode = TRUE;
		    g_string_append_printf (ret, "<");
		  }
		GList *notenode;
		outputret;
		for (notenode = pchord->notes; notenode; notenode = notenode->next)
		  {
		    note *curnote = (note *) notenode->data;
		    noteheadtype = curnote->noteheadtype;

		    switch (noteheadtype)
		      {
		      case DENEMO_NORMAL_NOTEHEAD:
			if (!is_normalnotehead)
			  {
			    g_string_append_printf
			      (ret, "\n"TAB"\\revert NoteHead #'style ");
			    is_normalnotehead = !is_normalnotehead;
			  }
			break;
		      case DENEMO_CROSS_NOTEHEAD:
			g_string_append_printf
			  (ret,
			   "\n"TAB"\\once \\override NoteHead #'style = #'cross ");
			is_normalnotehead = FALSE;
			break;
		      case DENEMO_HARMONIC_NOTEHEAD:
			g_string_append_printf
			  (ret,
			   "\n"TAB"\\once \\override NoteHead #'style = #'harmonic ");
			is_normalnotehead = FALSE;
			break;
		      case DENEMO_DIAMOND_NOTEHEAD:
			g_string_append_printf
			  (ret,
			   "\n"TAB"\\once \\override Voice.NoteHead #'style = #'diamond ");
			is_normalnotehead = FALSE;
			break;
		      default:
			g_string_append_printf
			  (ret, "\n"TAB"\\revert Voice.NoteHead #'style ");
			break;
		      }

		    mid_c_offset = curnote->mid_c_offset;
		    g_string_append_printf (ret, "%c",
					    mid_c_offsettoname (mid_c_offset));
		    enshift = curnote->enshift;
		    if (enshift < 0)
		      for (k = enshift; k; k++)
			g_string_append_printf (ret, "es");
		    else
		      for (k = enshift; k; k--)
			g_string_append_printf (ret, "is");
		    octave = mid_c_offsettooctave (mid_c_offset);
		    if (octave < 0)
		      for (; octave; octave++)
			g_string_append_printf (ret, ",");
		    else
		      for (; octave; octave--)
			g_string_append_printf (ret, "\'");


		    outputret;
		    if(curnote->directive ) {
		      insert_editable(&curnote->directive, curnote->directive->len?curnote->directive->str:" ", iter, invisibility, gui);
		    }
		    else
		      if (notenode->next)
			output(" ");
		  }		/* End notes in chord loop */

		if (pchord->notes->next ||  ((note *) (pchord->notes)->data)->directive) //multi-note chord
		  g_string_append_printf (ret, ">");
	      } //end of note(s) that is(are) not invisible
	    else //invisible note
	      {
		g_string_append_printf (ret, "s");
	      }
	    if (duration != prevduration || numdots != prevnumdots)
	      {
		/* only in this case do we explicitly note the duration */
		g_string_append_printf (ret, "%d", duration);
		prevduration = duration;
		prevnumdots = numdots;
		for (j = 0; j < numdots; j++)
		  g_string_append_printf (ret, ".");
	      }

	    if (pchord->dynamics && (pchord->notes->next==NULL))
	      {
		dynamic_string = (GString *) pchord->dynamics->data;
		if (is_chordmode)
		  g_string_append_printf (ret, "\\%s", dynamic_string->str);
		else
		  g_string_append_printf (ret, "\\%s ", dynamic_string->str);
	      }

	    for (tmpornament = pchord->ornamentlist; tmpornament;
		 tmpornament = tmpornament->next)
	      {
		//g_print ("in tmpornament\n");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) STACCATO)
		  g_string_append_printf (ret, " \\staccato");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) TENUTO)
		  g_string_append_printf (ret, " \\tenuto");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) D_ACCENT)
		  g_string_append_printf (ret, " \\accent");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) FERMATA)
		  g_string_append_printf (ret, " \\fermata");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) TRILL)
		  g_string_append_printf (ret, " \\trill");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) TURN)
		  g_string_append_printf (ret, " \\turn");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) MORDENT)
		  g_string_append_printf (ret, " \\mordent");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) STACCATISSIMO)
		  g_string_append_printf (ret, " \\staccatissimo");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) CODA)
		  g_string_append_printf (ret, " \\coda");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) FLAGEOLET)
		  g_string_append_printf (ret, " \\flageolet");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) OPEN)
		  g_string_append_printf (ret, " \\open");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) PRALLMORDENT)
		  g_string_append_printf (ret, " \\prallmordent");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) PRALLPRALL)
		  g_string_append_printf (ret, " \\prallprall");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) PRALL)
		  g_string_append_printf (ret, " \\prall");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) REVERSETURN)
		  g_string_append_printf (ret, " \\reverseturn");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) SEGNO)
		  g_string_append_printf (ret, " \\segno");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) STOPPED)
		  g_string_append_printf (ret, " \\stopped");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) THUMB)
		  g_string_append_printf (ret, " \\thumb");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) UPPRALL)
		  g_string_append_printf (ret, " \\upprall");
		if (*(enum ornament *) tmpornament->data ==
		    (enum ornament) D_ARPEGGIO)
		  g_string_append_printf (ret, " \\arpeggio");
	      }



	    if (pchord->crescendo_begin_p)
	      g_string_append_printf (ret, " \\cr");
	    else if (pchord->diminuendo_begin_p)
	      g_string_append_printf (ret, " \\decr");
	    if (pchord->crescendo_end_p)
	      g_string_append_printf (ret, " \\!");
	    else if (pchord->diminuendo_end_p)
	      g_string_append_printf (ret, " \\!");
	    if (pchord->slur_end_p)
	      g_string_append_printf (ret, ")");
	    if (pchord->slur_begin_p)
	      g_string_append_printf (ret, "(");
	    if (pchord->is_tied)
	      g_string_append_printf (ret, " ~");
	    /* do this in caller                    g_string_append_printf (ret, " "); */
	  } /* End of else chord with note(s) */
	break;
      case CLEF:
	determineclef (((clef *) curobj->object)->type, &clefname);
	g_string_append_printf (ret, "\\clef %s", clefname);
	break;
      case KEYSIG:
	determinekey (((keysig *) curobj->object)->isminor ?
		      ((keysig *) curobj->object)->number + 3 :
		      ((keysig *) curobj->object)->number, &keyname);
	g_string_append_printf (ret, "\\key %s", keyname);
	if (((keysig *) curobj->object)->isminor)
	  g_string_append_printf (ret, " \\minor");
	else
	  g_string_append_printf (ret, " \\major");
	/*do this in caller             g_string_append_printf (ret, " "); */
	break;
      case TIMESIG:
	g_string_append_printf (ret, "\\time %d/%d",
				((timesig *) curobj->object)->time1,
				((timesig *) curobj->object)->time2);
	cur_stime1 = ((timesig *) curobj->object)->time1;
	cur_stime2 = ((timesig *) curobj->object)->time2;
	break;
      case TUPOPEN:
	/* added by Yu CHeung "toby" Ho 3 Jun 00, adapted by Hiller
	 * 8 Jun 00 (happy birthday to me...) :) */
	g_string_append_printf (ret, "\\times %d/%d {",
				((tupopen *) curobj->object)->numerator,
				((tupopen *) curobj->object)->denominator);
	break;
      case TUPCLOSE:
	g_string_append_printf (ret, "}");
	break;
      case GRACE_START:
	g_string_append_printf (ret, "\\grace {");
	break;
      case GRACE_END:
	g_string_append_printf (ret, "}");
	break;
      case STEMDIRECTIVE:
	switch (((stemdirective *) curobj->object)->type)
	  {
	  case DENEMO_STEMDOWN:
	    g_string_append_printf (ret, "\\stemDown");
	    break;
	  case DENEMO_STEMBOTH:
	    g_string_append_printf (ret, "\\stemNeutral");
	    break;
	  case DENEMO_STEMUP:
	    g_string_append_printf (ret, "\\stemUp");
	    break;
	  }
	break;
      case DYNAMIC:
	/*if (is_chordmode)
	  {
	  sprintf (dynamic_string, "-\\%s ",
	  ((dynamic *)curobj->object)->type->str);
	  strcat (dynamic_string, temp);
	  g_string_append_printf (ret, "%s", dynamic_string);
	  }
	  else
	  g_string_append_printf (ret, "-\\%s ", 
	  ((dynamic *)curobj->object)->type->str); */
	break;
      case LILYDIRECTIVE:
	; //handled in the if block
	break; 
      case BARLINE:
	switch (((barline *) curobj->object)->type)
	  {

	  case ORDINARY_BARLINE:
	    g_string_append (ret, "|\n");
	    break;
	  case DOUBLE_BARLINE:
	    g_string_append (ret, "\\bar \"||\"\n");
	    break;
	  case END_BARLINE:
	    g_string_append (ret, "\\bar \"|.\"\n");
	    break;
	  case OPENREPEAT_BARLINE:
	    g_string_append (ret, "\\bar \"|:\"\n");
	    break;
	  case CLOSE_REPEAT:
	    g_string_append (ret, "\\bar \":|\"\n");
	    break;
	  case OPEN_CLOSE_REPEAT:
	    g_string_append (ret, "\\bar \":\"\n");
	    break;

	  }
	break;
      case LYRIC:
      case FIGURE:
	//handle in caller
	break;

      case PARTIAL:

	pchord = (chord *) curobj->object;
	duration = internaltomuduration (pchord->baseduration);
	numdots = pchord->numdots;
	ret = g_string_append (ret, "\\partial ");
	g_string_append_printf (ret, "%d", duration);
	for (j = 0; j < numdots; j++)
	  ret = g_string_append (ret, ".");
	ret = g_string_append (ret, " ");
	break;

      default:
	break;
      }



    outputret;
  }//not a LilyPond directive
    *pprevduration = prevduration;
    *pprevnumdots = prevnumdots;

    *pclefname = clefname;
    *pkeyname = keyname;
    *pcur_stime1 = cur_stime1;
    *pcur_stime2 = cur_stime2;

}

/* create and insertion point and button for the next piece of music */
static void insert_music_section(DenemoGUI *gui, gchar *name) {
  GtkTextIter iter;
  gtk_text_buffer_get_iter_at_mark(gui->textbuffer, &iter, gtk_text_buffer_get_mark(gui->textbuffer, MUSIC));
  gtk_text_buffer_insert (gui->textbuffer, &iter, "\n", -1);
  (void)gtk_text_iter_backward_char(&iter);
  insert_section(NULL, name, name, &iter, gui);
}

/* create and insertion point and button for the next scoreblock */
static GtkTextChildAnchor * insert_scoreblock_section(DenemoGUI *gui, gchar *name, DenemoScoreblock *sb) {
  GString** target = sb?&sb->scoreblock:NULL;
  GtkTextChildAnchor *anchor;
  GtkTextIter iter;
  gtk_text_buffer_get_iter_at_mark(gui->textbuffer, &iter, gtk_text_buffer_get_mark(gui->textbuffer, SCOREBLOCK));
  gtk_text_buffer_insert (gui->textbuffer, &iter, "\n", -1);
  (void)gtk_text_iter_backward_char(&iter);
  if(sb && *target) {// custom scoreblock
    if((*target)->len && *(*target)->str=='%'){
      gint maxlength= 50;
      gchar *v = g_malloc0(maxlength*sizeof(gchar));
      strncpy(v,(*target)->str+1,maxlength-1);//skip the opening % sign to make label, leave a NULL at the end
      while(--maxlength) 
	if(*(v+maxlength)=='\n')*(v+maxlength)=0;//truncate at first end of line
      anchor = insert_section(target, name, v, &iter, gui);
      g_free(v);
    } else {
      anchor = insert_section(target, name, name, &iter, gui);
    }
    g_object_set_data(G_OBJECT(anchor), CUSTOM,(gpointer) sb);
  } else {//standard scoreblock
    anchor = insert_section(target, name, name, &iter, gui); 
    g_object_set_data(G_OBJECT(anchor), STANDARD_SCOREBLOCK,(gpointer) 1);
  }
  return anchor;
}


/* gets the text for the section that starts at anchor */
static gchar *get_text(DenemoGUI *gui, GtkTextChildAnchor *anchor) {
  GtkTextIter start, end;
  gtk_text_buffer_get_iter_at_child_anchor(gui->textbuffer, &start, anchor);
  GtkTextChildAnchor *endanc = g_object_get_data(G_OBJECT(anchor), "end");
  gtk_text_buffer_get_iter_at_child_anchor(gui->textbuffer, &end, endanc);
  return gtk_text_buffer_get_text (gui->textbuffer, &start, &end, FALSE/* get only visible text */);
  // return gtk_text_buffer_get_text (gui->textbuffer, &start, &end, TRUE/* ignore invisibility*/);
}

/**
 * Output the header information using Lilypond syntax
 * 
 * 
 */
static void
outputHeader (GString *str, DenemoGUI * gui)
{
  DenemoScore *si = gui->si;

  g_string_append_printf (str, _("%% LilyPond file generated by Denemo version "));
  g_string_append_printf (str, VERSION "\n\n");
  if (gui->lilycontrol.excerpt == TRUE)
    g_string_append_printf (str, "\\include \"lilypond-book-preamble.ly\" \n\n");

  g_string_append_printf (str, "%%http://www.gnu.org/software/denemo/\n\n");
  /*Print out lilypond syntax version */
  g_string_append_printf (str, "\\version \"%s\"\n", LILYPOND_VERSION);

  /* print \paper block setting printing of all headers */
  g_string_append_printf (str, "\\paper {printallheaders = ##%c }\n", gui->lilycontrol.excerpt?'f':'t');



  g_string_append_printf (str, "#(set-global-staff-size %d)\n", gui->lilycontrol.fontsize);
  g_string_append_printf (str, "#(set-default-paper-size \"%s\")\n",
			  gui->lilycontrol.papersize->str);
  if(((DenemoScore*)gui->movements->data)->headerinfo.tagline->len) {
    g_string_append_printf (str, "\n\\header{\n");
    g_string_append_printf(str, TAB"tagline = \"%s\"\n", ((DenemoScore*)gui->movements->data)->headerinfo.tagline->str);
    g_string_append_printf(str,"}\n");
  }
}

/**
 * Output a Denemo Staff in Lilypond syntax
 * A section is created in the gui->textbuffer and the music inserted into it.
 * each DenemoObject is given an anchor and a pointer to the object is stored with the anchor,
 * so that it will be possible to create LilyPond directives from within the buffer (not yet
 * implemented FIXME).
 * Any lyrics, chord symbols and figured basses are put in separate sections.
 * 
 */
static void
outputStaff (DenemoGUI *gui, DenemoScore * si, DenemoStaff * curstaffstruct,
	     gint start, gint end, gchar *movement, gchar *voice, gint movement_count, gint voice_count)
{
  gint cur_stime1 = curstaffstruct->stime1;
  gint cur_stime2 = curstaffstruct->stime2;

  gint prevduration, prevnumdots;
  gchar *clefname;
  /* clef name */
  gchar *keyname;
  /* key signature name */
  measurenode *curmeasure;
  objnode *curobjnode;
  DenemoObject *curobj;
  gint curmeasurenum;// count of measures printed
  gint measurenum; //count of measures from start of staff starting at 1
  gint objnum;//count of objects in measure starting at 1

  GString *str = g_string_new("");
  GString * lyrics = g_string_new("");
  GString * figures = g_string_new("");
  GString * fakechords = g_string_new("");
  prevduration = 0;
  prevnumdots = -1;
  GtkTextIter iter;
  GtkTextMark *curmark;/* movable mark for insertion point of the music of the staff */
  /* a button and mark for the music of this staff */
  gchar *invisibility =  ((movement_count>0) && (voice_count>0))?NULL:"invisible";/* tag to control visibility */

  GString *voice_name = g_string_new(movement);
  g_string_prepend(voice_name, "Notes for ");
  g_string_append_printf(voice_name, " Voice %d",/* ABS*/(voice_count));
  //g_print("making %s\n", voice_name->str);
  insert_music_section(gui, voice_name->str);
  gtk_text_buffer_get_iter_at_mark(gui->textbuffer, &iter, gtk_text_buffer_get_mark(gui->textbuffer, voice_name->str));
  curmark = gtk_text_buffer_create_mark (gui->textbuffer, NULL, &iter, FALSE);//FIXME remove this mark at the end of the output of this staff...
  
  /* a button and mark for the lyrics of this staff */
  GString *lyrics_name = g_string_new(movement);
  if(curstaffstruct->haslyrics) {
    g_string_prepend(lyrics_name, "Lyrics for ");
    g_string_append_printf(lyrics_name, " Voice %d", voice_count);
    insert_music_section(gui, lyrics_name->str);
  }

  /* a button and mark for the figures of this staff */
  GString *figures_name = g_string_new(movement);
  if(curstaffstruct->hasfigures) {
    g_string_prepend(figures_name, "Figured Bass for ");
    g_string_append_printf(figures_name, " Voice %d", voice_count);
    insert_music_section(gui, figures_name->str);
  }
  /* a button and mark for the chord symbols of this staff */
  GString *fakechords_name = g_string_new(movement);
  if(curstaffstruct->hasfakechords) {
    g_string_prepend(fakechords_name, "Chord symbols for ");
    g_string_append_printf(fakechords_name, " Voice %d", voice_count);
    insert_music_section(gui, fakechords_name->str);
  }

  gtk_text_buffer_get_iter_at_mark(gui->textbuffer, &iter, curmark);

  /* output staff prolog */
  if(curstaffstruct->staff_prolog && curstaffstruct->staff_prolog->len) {/* custom staff-prolog */
    insert_editable(&curstaffstruct->staff_prolog, curstaffstruct->staff_prolog->str, &iter, invisibility, gui);
  } else {
    
    /* The midi instrument */
    g_string_append_printf(str, "\n"TAB"%s%sMidiInst = \\set Staff.midiInstrument = \"%s\"\n", movement, voice, curstaffstruct->midi_instrument->str);
    
    /* Time signature */
    g_string_append_printf(str, "\n"TAB"%s%sTimeSig = \\time %d/%d\n", movement, voice, curstaffstruct->stime1,
			   curstaffstruct->stime2);
    /* Determine the key signature */
    gchar *clefname;
    /* clef name */
    gchar *keyname;
    /* key signature name */
    determinekey (curstaffstruct->skey_isminor ?
		  curstaffstruct->skey + 3 : curstaffstruct->skey, &keyname);
    g_string_append_printf(str, ""TAB"%s%sKeySig = \\key %s", movement, voice, keyname);
    if (curstaffstruct->skey_isminor)
      g_string_append_printf(str, " \\minor\n");
    else
      g_string_append_printf(str, " \\major\n");
    /* Determine the clef */
    determineclef (curstaffstruct->sclef, &clefname);
    g_string_append_printf(str, ""TAB"%s%sClef = \\clef %s\n", movement, voice, clefname);
    g_string_append_printf(str, ""TAB"%s%sProlog = {\\%s%sMidiInst \\%s%sTimeSig \\%s%sKeySig \\%s%sClef}\n", 
			   movement, voice, movement, voice, movement, voice, movement, voice, movement, voice);
    
    g_string_append_printf(str, "%s%s = {\n",
			   movement, voice);    
    gtk_text_buffer_get_iter_at_mark (gui->textbuffer, &iter, curmark);
    insert_editable(&curstaffstruct->staff_prolog, str->str, &iter,  invisibility, gui);
  } /* standard staff-prolog */

  /* end of output staff-prolog */

  g_string_assign(str,"");
 
  curmeasurenum = 0;
  curmeasure = curstaffstruct->measures;
  if(!end)
    end = g_list_length (curmeasure);



  /* Now each measure */
  if (start)
    curmeasure = g_list_nth (curmeasure, start - 1);

  for (measurenum = MAX (start, 1); curmeasure && measurenum <= end;
       curmeasure = curmeasure->next, measurenum++)
    {
      gboolean lilydirective_now = FALSE;
      gboolean empty_measure = TRUE;

      gboolean is_syllable = FALSE;
      gboolean center_lyric = FALSE;
      if ((++curmeasurenum % 5) == 0) {
	g_string_append_printf(str, "%%%d\n", curmeasurenum);
	if(figures->len)
	  g_string_append_printf(figures, "\n%%%d\n", curmeasurenum);
	if(fakechords->len)
	  g_string_append_printf(fakechords, "\n%%%d\n", curmeasurenum);
      }
      g_string_append_printf(str, ""TAB"");
      gtk_text_buffer_get_iter_at_mark (gui->textbuffer, &iter, curmark);
      gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, &iter, str->str, -1, INEDITABLE, invisibility, NULL);
      g_string_assign(str,"");
      gint firstobj=1, lastobj= G_MAXINT;
      if(start && gui->si->firstobjmarked) {//firstobjmarked==0 means not set
	firstobj = 1+MIN( gui->si->firstobjmarked, gui->si->lastobjmarked);
	lastobj =  1+MAX( gui->si->firstobjmarked, gui->si->lastobjmarked);
      }
      //g_print("First last, %d %d %d\n", firstobj, lastobj, start);
      for (objnum=1, curobjnode = (objnode *) curmeasure->data;/* curobjnode NULL checked at end */;
	   curobjnode = curobjnode->next, objnum++)
	{

	  if(objnum>=firstobj && objnum<=lastobj) {

	  if(curobjnode) {
	  curobj = (DenemoObject *) curobjnode->data;
	  if (curobj->type==CHORD||curobj->type==PARTIAL||curobj->type==LILYDIRECTIVE)
	    empty_measure=FALSE; 
	  if( (curobj->type==LILYDIRECTIVE) &&
	      (((lilydirective *) curobj->object)->directive->len==0))
	    continue;
	  //put in an invisible ineditable anchor to mark the start of the object
	  gtk_text_buffer_get_iter_at_mark (gui->textbuffer, &iter, curmark);
	  GtkTextChildAnchor *objanc = gtk_text_buffer_create_child_anchor (gui->textbuffer, &iter);
	  g_object_set_data(G_OBJECT(objanc), OBJECT, (gpointer)curobjnode);//store objnode here
	  g_object_set_data(G_OBJECT(objanc), MOVEMENTNUM, (gpointer)ABS(movement_count));
	  g_object_set_data(G_OBJECT(objanc), MEASURENUM, (gpointer)measurenum);
	  g_object_set_data(G_OBJECT(objanc), STAFFNUM, (gpointer)ABS(voice_count));
	  g_object_set_data(G_OBJECT(objanc), OBJECTNUM, (gpointer)ABS(objnum));

	  GtkTextIter back;
	  back = iter;
	  (void)gtk_text_iter_backward_char(&back);
	  gtk_text_buffer_apply_tag_by_name(gui->textbuffer, INEDITABLE, &back, &iter);
	  gtk_text_buffer_apply_tag_by_name(gui->textbuffer, "system_invisible", &back, &iter);

	  generate_lily_for_obj (gui, &iter, invisibility, curobj, objanc, 
				 &prevduration, &prevnumdots, &clefname,
				 &keyname,
				 &cur_stime1, &cur_stime2);
	  }// if curobjnode

	  if( (curobjnode==NULL) || (curobjnode->next==NULL)) {	//at end of measure
	    GString *endstr = g_string_new("");
	    if (empty_measure)// measure has nothing to use up the duration, assume whole measure rest
	      {
		g_string_append_printf(endstr, "R1*%d/%d ", cur_stime1, cur_stime2);
		gtk_text_buffer_get_iter_at_mark (gui->textbuffer, &iter, curmark);
		gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, &iter, endstr->str, -1,invisibility,NULL);
		g_string_assign(endstr,"");
		prevduration = 0;
	      }
	    if(figures->len)
	      g_string_append(figures, "\n");
	    if(fakechords->len)
	      g_string_append(fakechords, "\n");
	    if(curobj == NULL || curobj->type!=LILYDIRECTIVE) /* if it ends in a lilydirective, the user may want to choose their own
					       barline style, let them */
	      if (curmeasure->next)
		g_string_append_printf(endstr, "|\n");
	      else
		g_string_append_printf(endstr, " \\bar \"|.\"\n");
	    gtk_text_buffer_get_iter_at_mark (gui->textbuffer, &iter, curmark);
	    gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, &iter, endstr->str, -1, INEDITABLE, invisibility,NULL);
	    //g_string_assign(endstr,"");
	  }   //if end of measure

	  if(curobjnode) {
	    curobj = (DenemoObject *) curobjnode->data;
	    /* lyrics, figures and chord symbols */
	    if(curobj->type==CHORD) {
	      chord *pchord = (chord *) curobj->object;
	      if(curstaffstruct->haslyrics)
		output_lyric(lyrics, pchord, &is_syllable, &center_lyric);
	       
	      if(curstaffstruct->hasfigures)
		output_figured_bass (si, figures, pchord);
	      
	      if (curstaffstruct->hasfakechords)
		output_fakechord(si, fakechords, pchord);
	    /* end of lyrics, figures and chord symbols*/
	    }
	  }
	  }//in obj range
	  if(curobjnode==NULL || curobjnode->next==NULL)
	    break;//we want to go through once for empty measures
	} /* For each object in the measure */



    } /* for each staff */
  
  // str is empty again now FIXME
  g_string_append_printf(str, "}\n");
  g_string_append_printf(str, "%s%sMusicVoice = \\context Voice = %s%s %s {\\%s%sProlog \\%s%s}\n",
			 movement, voice,  voice, movement, 
			 (curstaffstruct->staff_prolog_insert && curstaffstruct->staff_prolog_insert->len)? curstaffstruct->staff_prolog_insert->str:"",
 movement, voice, movement, voice);
  g_string_append_printf(str, "%s%sMusic =  {\\%s%sProlog \\%s%s}\n",
			 movement, voice, movement, voice, movement, voice);
  
  gtk_text_buffer_get_iter_at_mark (gui->textbuffer, &iter, curmark);
  
  gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, &iter, str->str, -1, INEDITABLE, invisibility,NULL);
  

  if (lyrics->len)
    {  
      GString *temp = g_string_new("");
      gtk_text_buffer_get_iter_at_mark (gui->textbuffer, &iter, gtk_text_buffer_get_mark(gui->textbuffer, lyrics_name->str));
      /* output lyrics prolog */
      if(curstaffstruct->lyrics_prolog && curstaffstruct->lyrics_prolog->len) {
	insert_editable(&curstaffstruct->lyrics_prolog, curstaffstruct->lyrics_prolog->str, &iter, invisibility, gui);
      } else {	
	g_string_printf(temp, "%s%sLyrics = \\lyricmode { \n", movement,
			voice);
	insert_editable(&curstaffstruct->lyrics_prolog, temp->str, &iter,  invisibility, gui);
      }
      g_string_printf(temp, "%s \n}\n", lyrics->str);
      gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, &iter, temp->str, -1, INEDITABLE, invisibility,NULL);
      g_string_free(temp, TRUE);
    }
  g_string_free(lyrics_name, TRUE);

  if (figures->len)
    {
      GString *temp = g_string_new("");
      gtk_text_buffer_get_iter_at_mark (gui->textbuffer, &iter, gtk_text_buffer_get_mark(gui->textbuffer, figures_name->str));
      /* output figures prolog */
      if(curstaffstruct->figures_prolog && curstaffstruct->figures_prolog->len) {
	insert_editable(&curstaffstruct->figures_prolog, curstaffstruct->figures_prolog->str, &iter, invisibility, gui);
      } else {
	g_string_printf(temp,  "%s%sBassFiguresLine = \\figuremode {\n"
			"\\set figuredBassAlterationDirection = #1\n"
			"\\set figuredBassPlusDirection = #1\n"
			"\\override FiguredBass.BassFigure "
			"#'font-size = #-2\n",movement, voice);
	insert_editable(&curstaffstruct->figures_prolog, temp->str, &iter,  invisibility, gui);
      }
      g_string_printf(temp, "%s \n}\n", figures->str);
      gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, &iter, temp->str, -1, INEDITABLE, invisibility,NULL);
      g_string_free(temp, TRUE);
    }
  g_string_free(figures_name, TRUE);

  if (fakechords->len)
    {
      GString *temp = g_string_new("");
      gtk_text_buffer_get_iter_at_mark (gui->textbuffer, &iter, gtk_text_buffer_get_mark(gui->textbuffer, fakechords_name->str));
      /* output fakechords prolog */
      if(curstaffstruct->fakechords_prolog && curstaffstruct->fakechords_prolog->len) {
	insert_editable(&curstaffstruct->fakechords_prolog, curstaffstruct->fakechords_prolog->str, &iter, invisibility, gui);
      } else {
	//g_string_append_printf(temp, "%s%sChordsProlog ="TAB"\n", movement, voice);
	g_string_append_printf(temp, "%s%sChords = \\new ChordNames \\chordmode {\n", movement, voice);
	insert_editable(&curstaffstruct->fakechords_prolog, temp->str, &iter,  invisibility, gui);
      }
      g_string_printf(temp, "%s \n}\n"/* another definition here */, fakechords->str);
      gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, &iter, temp->str, -1, INEDITABLE, invisibility,NULL);
      g_string_free(temp, TRUE);
    }
  g_string_free(fakechords_name, TRUE);

  g_string_free(str, TRUE);
  g_string_free(lyrics, TRUE);
  g_string_free(figures, TRUE);
  g_string_free(fakechords, TRUE);
} /* outputStaff */

/* Merge back any modified LilyPond text into the Denemo Score */
void merge_lily_strings (DenemoGUI *gui) {
  //g_print("Merge...\n");
  GList *g;
  if(gui==Denemo.gui)
    write_status(gui);
  if(!gtk_text_buffer_get_modified(gui->textbuffer)) {
    // g_print("not modified\n");
    return;
  }
  if(gui->lilysync!=gui->changecount) {
    warningdialog("The score has been altered so that this LilyPond text is out of date - ignoring request");
    return;
  }
  for(g = gui->anchors;g;g=g->next) {
    GtkTextChildAnchor *anchor = g->data;
    GString **target = g_object_get_data(G_OBJECT(anchor), TARGET);
    if(target) { 
      gchar *lily = get_text(gui, anchor);   
      if(strcmp(lily, g_object_get_data(G_OBJECT(anchor),ORIGINAL))){
	//g_print("Compare %s\nwith %s\n", lily, g_object_get_data(anchor,ORIGINAL));
	if(!*target)
	  *target = g_string_new(lily);
	else
	  g_string_assign(*target, lily);
	/* white space becomes empty string */
	g_strstrip(lily);
	if(*lily == '\0')
	  {
	    g_string_free(*target, TRUE);
	    *target = g_string_new("");
	  }
	g_free(g_object_get_data(G_OBJECT(anchor),ORIGINAL));
	g_object_set_data(G_OBJECT(anchor),ORIGINAL, get_text(gui, anchor));
	

	gui->changecount++;
	set_title_bar(gui);
	g_free(lily);
      }

    }

  }
  gtk_text_buffer_set_modified(gui->textbuffer, FALSE);
}

void merge_lily_cb (GtkAction *action, DenemoGUI *gui) {
  merge_lily_strings(gui);
}

/* if there is not yet a textbuffer for the passed gui, it creates and populates one,
   if there is it finds the offset of the current point in the buffer, refreshes it from 
   the Denemo data and then repositions the cursor at that offset. The refresh is subject to
   conditions (see output_score_to_buffer()).
*/
void refresh_lily_cb (GtkAction *action, DenemoGUI *gui) {
  if(gui->textbuffer) {
  GtkTextIter iter;
  GtkTextMark *cursor = gtk_text_buffer_get_insert(gui->textbuffer);
  gtk_text_buffer_get_iter_at_mark (gui->textbuffer, &iter, cursor);
  gint offset = gtk_text_iter_get_offset (&iter);
  //  g_print("Offset %d for %p\n", offset, gui->textbuffer);
  output_score_to_buffer (gui, TRUE, NULL);
  gtk_text_buffer_get_iter_at_offset (gui->textbuffer, &iter, offset);
  gtk_text_buffer_place_cursor(gui->textbuffer, &iter);
  // gtk_text_view_scroll_mark_onscreen(GTK_TEXT_VIEW(gui->textview), gtk_text_buffer_get_insert(gui->textbuffer));
  gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW(gui->textview), 
			      gtk_text_buffer_get_insert(gui->textbuffer), 0.0, TRUE, 0.5, 0.5);
  } else 
    output_score_to_buffer (gui, TRUE, NULL);
}


void delete_lily_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
  GtkTextChildAnchor *anchor = gui->lilystart;
  GtkTextIter start, end;
  gtk_text_buffer_get_iter_at_child_anchor(gui->textbuffer, &start, anchor);
  GtkTextChildAnchor *endanc = g_object_get_data(G_OBJECT(anchor), "end");
  gtk_text_buffer_get_iter_at_child_anchor(gui->textbuffer, &end, endanc);
  gpointer sb = g_object_get_data(G_OBJECT(anchor), CUSTOM);
  //g_print("retrieving %p %p\n",anchor, target);
  gui->anchors = g_list_remove(gui->anchors, anchor);
  gui->custom_scoreblocks = g_list_remove(gui->custom_scoreblocks, sb);


  gtk_text_buffer_delete(gui->textbuffer, &start, &end);
}
static gboolean print_lily_cb (GtkWidget *item, DenemoGUI *gui){
  gchar *filename = get_printfile_pathbasename();
  gchar *lilyfile = g_strconcat (filename, ".ly", NULL);
  FILE *fp = fopen(lilyfile, "w");
  if(fp){
    GtkTextIter startiter, enditer;
    gtk_text_buffer_get_start_iter (gui->textbuffer, &startiter);
    gtk_text_buffer_get_end_iter (gui->textbuffer, &enditer);
    gchar *lily = gtk_text_buffer_get_text (gui->textbuffer, &startiter, &enditer, FALSE);
    fprintf(fp, "%s", lily);
    fclose(fp);
    run_lilypond_and_viewer(filename, gui);
  }
}

/* create a new custom scoreblock from the text of the one passed in lilystart */
void custom_lily_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
  GtkTextChildAnchor *anchor = gui->lilystart;
  merge_lily_strings(gui);
  if(g_object_get_data(G_OBJECT(anchor),STANDARD_SCOREBLOCK)){
    GtkTextIter start;
    gtk_text_buffer_get_iter_at_child_anchor(gui->textbuffer, &start, anchor);
    GtkTextTag *tag = gtk_text_tag_table_lookup(tagtable, "invisible");
    (void)gtk_text_iter_forward_char(&start);
    if(gtk_text_iter_has_tag(&start,tag)) {
      warningdialog("The scoreblock is hidden,\nso you will have an empty custom scoreblock to start with");
    }
    gchar *lily = get_text(gui, anchor);
    DenemoScoreblock *sb = g_malloc0(sizeof(DenemoScoreblock));
    sb->scoreblock= g_string_new(lily);
    sb->visible = TRUE;
    anchor = insert_scoreblock_section(gui, NULL, sb);
    gui->custom_scoreblocks = g_list_prepend(gui->custom_scoreblocks, sb);
    g_object_set_data(G_OBJECT(anchor),ORIGINAL, lily);
    gui->changecount++;
    set_title_bar(gui);
    refresh_lily_cb(action, gui);
    GtkTextIter iter;//Place cursor at end of MUSIC which is where the score block has been placed
    gtk_text_buffer_get_iter_at_mark (gui->textbuffer, &iter, gtk_text_buffer_get_mark(gui->textbuffer, MUSIC));
    gtk_text_buffer_place_cursor(gui->textbuffer, &iter);
    //gtk_text_view_scroll_mark_onscreen(GTK_TEXT_VIEW(gui->textview), gtk_text_buffer_get_insert(gui->textbuffer));
    gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW(gui->textview), 
			      gtk_text_buffer_get_insert(gui->textbuffer), 0.0, TRUE, 0.5, 0.5);
    
  }
  else
    refresh_lily_cb(action, gui);
}

void toggle_lily_visible_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
  GtkTextIter start, end;
  GtkTextChildAnchor *anchor = gui->lilystart;
  DenemoScoreblock *sb = g_object_get_data(G_OBJECT(anchor), CUSTOM);
  gtk_text_buffer_get_iter_at_child_anchor(gui->textbuffer, &start, anchor);
  (void)gtk_text_iter_forward_char(&start);
  GtkTextChildAnchor *endanc = g_object_get_data(G_OBJECT(anchor), "end");

  gtk_text_buffer_get_iter_at_child_anchor(gui->textbuffer, &end, endanc);

  GtkTextTag *tag = gtk_text_tag_table_lookup(tagtable, "invisible");
  /*   GtkTextTag *systemtag = gtk_text_tag_table_lookup(tagtable, "system_invisible"); */
  if(gtk_text_iter_has_tag(&start,tag)) {
    if(sb)
      sb->visible = TRUE;
    gtk_text_buffer_remove_tag_by_name (gui->textbuffer,  "invisible", &start, &end);
  }
  else {
    if(sb)
      sb->visible = FALSE;
    gtk_text_buffer_apply_tag_by_name (gui->textbuffer,  "invisible", &start, &end);
  }
  g_print("visible %d\n", sb?sb->visible:-1);
}
  
static void  place_cursor_cb(GtkAction *action, DenemoGUI *gui) {
  /* place cursor on current object */
  if(gui->si->currentobject){
    DenemoObject *targetobj = gui->si->currentobject->data;
    GList *curobjnode;
    GtkTextIter iter;
    gtk_text_buffer_get_start_iter (gui->textbuffer, &iter);
    while (gtk_text_iter_forward_char (&iter))
      {
	GtkTextChildAnchor *anchor;
	anchor = gtk_text_iter_get_child_anchor(&iter);
	if (anchor && (curobjnode = g_object_get_data(G_OBJECT(anchor), OBJECT)) &&
	    curobjnode->data == targetobj) {
	  gtk_text_buffer_place_cursor(gui->textbuffer, &iter);
	  gtk_text_view_scroll_mark_onscreen(GTK_TEXT_VIEW(gui->textview), gtk_text_buffer_get_insert(gui->textbuffer));
gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW(gui->textview), 
			      gtk_text_buffer_get_insert(gui->textbuffer), 0.0, TRUE, 0.5, 0.5);
	  //g_print("placed cursor\n"); FIXME as well color in relevant objects
	}
      }
  }
  }
  
/*
 *writes the current score in LilyPond format to the textbuffer.
 *sets gui->lilysync equal to gui->changecount
 *if gui->lilysync is up to date with changecount on entry does nothing unless
 *the set of score blocks will be different from the last call
 * this namespec is not otherwise used FIXME
 */


static void
output_score_to_buffer (DenemoGUI *gui, gboolean all_movements, gchar * partname)
{
  gchar *last_namespec=gui->namespec?gui->namespec:g_strdup_printf("");/* to check if the scoreblocks to make visible are different */

  gchar *namespec;
  gchar *movementname;
  if(all_movements) 
    movementname = g_strdup("All movements");
  else
    movementname = g_strdup_printf("Movement %d", 1+g_list_index(gui->movements, gui->si));
  if(partname)
    namespec = g_strdup_printf("%s Part %s", movementname, partname);
  else
    namespec = g_strdup_printf("%s all parts", movementname);
  g_free(movementname);
  if(gui->textview==NULL)
    create_lilywindow(gui);
  staffnode *curstaff;
  DenemoStaff *curstaffstruct;

  gboolean context = FALSE;
  DenemoContext curcontext = DENEMO_NONE;
  /*figured basses */

  /*lyrics */
  GString *lyrics = NULL;

  if(gui->textbuffer && (gui->changecount==gui->lilysync)
     && !strcmp(last_namespec, namespec)) {
    g_free(namespec);
    gui->namespec = namespec;
    //g_print("changecount=%d and lilysync= %d\n", gui->changecount, gui->lilysync);
    return;
  }
  g_free(namespec);
  gui->namespec = namespec;
  //g_print("actually refreshing %d %d", gui->lilysync, gui->changecount);
  gui->lilysync = gui->changecount;
  if(gui->textbuffer)
    gtk_text_buffer_set_text(gui->textbuffer,"",-1);
  else
    warningdialog("trouble - no textbuffer");
  if(gui->anchors) {
    //FIXME  the working curmark at the end of the creation of the text
    g_list_free(gui->anchors);
    gui->anchors = NULL;
  }
  /* divide up the buffer for the various parts of the lily file */
  GtkTextIter iter;

  gtk_text_buffer_get_start_iter (gui->textbuffer, &iter);

  insert_section(&gui->custom_prolog, START, "Prolog", &iter, gui);

  gtk_text_buffer_get_end_iter (gui->textbuffer, &iter);

  insert_section(NULL, MUSIC, NULL, &iter, gui);
  gtk_text_buffer_get_end_iter (gui->textbuffer, &iter);

  insert_section(NULL, SCOREBLOCK, NULL, &iter, gui);

  gtk_text_buffer_get_iter_at_mark (gui->textbuffer, &iter, gtk_text_buffer_get_mark(gui->textbuffer, START));
  gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, &iter, "\n", -1, "bold", NULL);
 
  gtk_text_buffer_get_iter_at_mark (gui->textbuffer, &iter, gtk_text_buffer_get_mark(gui->textbuffer, MUSIC));
  gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, &iter, "% The music follows\n", -1, INEDITABLE, NULL);

  gtk_text_buffer_get_iter_at_mark (gui->textbuffer, &iter, gtk_text_buffer_get_mark(gui->textbuffer, SCOREBLOCK));
  gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, &iter, "% The scoreblocks follow\n", -1, "bold","system_invisible", NULL); 



  gtk_text_buffer_get_iter_at_mark(gui->textbuffer, &iter, gtk_text_buffer_get_mark(gui->textbuffer, START));
  if(gui->custom_prolog && gui->custom_prolog->len ) 
    gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, &iter, gui->custom_prolog->str, -1, "bold", NULL);
  else {
    GString *header = g_string_new("");
    outputHeader (header, gui);
    gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, &iter, header->str, -1, "bold", NULL);
    g_string_free(header, TRUE);
  }
  

  //  GTimeVal time;long seconds;
  //  g_get_current_time(&time);
  //  g_print("time %ld secs", seconds = time.tv_sec);

  /* output custom scoreblocks making the first one visible unless this is just a part being printed */
  GList *custom;
  for(custom=gui->custom_scoreblocks;custom;custom=custom->next) 
    if(custom->data){
      DenemoScoreblock *sb = (DenemoScoreblock *)custom->data;
      insert_scoreblock_section(gui, "custom scoreblock", sb);
      gtk_text_buffer_get_iter_at_mark(gui->textbuffer, &iter, gtk_text_buffer_get_mark(gui->textbuffer, "custom scoreblock"));
      gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, &iter, (sb->scoreblock)->str, -1, "bold", sb->visible?NULL:"invisible", NULL);
    }


  /* output standard scoreblock, possibly invisible*/


  insert_scoreblock_section(gui, STANDARD_SCOREBLOCK, NULL);
  GList *g; gint movement_count;
  gint visible_movement;/* 1 for visible -1 for invisible */ 
  for(g = gui->movements,  movement_count=1;g;g = g->next,  movement_count++) {
    DenemoScore *si = g->data;gint voice_count;
    visible_movement = (((all_movements) || (g->data == gui->si))?1:-1);
    GString *movement_name = g_string_new("");
    GString *name = g_string_new("");
    g_string_printf(name, "Mvmnt%d", movement_count);
    set_lily_name (name, movement_name);
    g_string_free(name, TRUE);
    context = FALSE;
    curcontext = DENEMO_NONE;
    GString *scoreblock = g_string_new("");
    if(visible_movement==1) {

      /* Any markup before the score block */
      if(si->headerinfo.lilypond_before->len)
	g_string_append_printf (scoreblock, "%s\n", si->headerinfo.lilypond_before->str);
   
      //standard score block
      g_string_append_printf (scoreblock, "\\score {\n<<%s <<\n", gui->lilycontrol.lilypond->str);
    }
 
    for (curstaff = si->thescore, voice_count=1; curstaff; curstaff = curstaff->next, voice_count++)
      {
	gint visible_part=1;/* 1 for visible -1 for invisible */ 
	curstaffstruct = (DenemoStaff *) curstaff->data;
	GString *voice_name = g_string_new("");
	GString *name = g_string_new("");
	g_string_printf(name, "Voice%d", voice_count);
	set_lily_name (name, voice_name);
	g_string_free(name, TRUE);
	gint start = 0, end = 0;
	if(gui->si->markstaffnum) {
	  if(!(voice_count>=gui->si->firststaffmarked && voice_count<=gui->si->laststaffmarked))
	    visible_part=-1;
	  start = gui->si->firstmeasuremarked;
	  end = gui->si->lastmeasuremarked;
	} else {
	  if(partname &&strcmp(curstaffstruct->lily_name->str, partname))
	    visible_part=-1;
	}
	outputStaff (gui, si, curstaffstruct, start, end, movement_name->str, voice_name->str, movement_count*visible_movement, voice_count*visible_part);
	//FIXME amalgamate movement and voice names below here...
	/* output score block */
	if(visible_movement==1 && (visible_part==1)) {    
	  if (curstaffstruct->hasfakechords)
	    g_string_append_printf(scoreblock, ""TAB""TAB"\\new ChordNames \\chordmode { \\%s%sChords }\n", 
				   movement_name->str, voice_name->str);
	  GString *str = g_string_new("");


	  if (curstaffstruct->context & DENEMO_CHOIR_START)
	    g_string_append_printf(str, "\\new ChoirStaff << \n");
	  if (curstaffstruct->context & DENEMO_GROUP_START)
	    g_string_append_printf(str, "\\new StaffGroup << \n");
	  if (curstaffstruct->context & DENEMO_PIANO_START) /* Piano staff cannot start before Group */
	    g_string_append_printf(str, "\\new PianoStaff << \n");

	  if(curstaffstruct->voicenumber == 1)
	    g_string_append_printf(str, "\\new Staff << {\n");
    else
      g_string_append_printf(str, "\\new Voice {\n");
    if (curstaffstruct->no_of_lines != 5)
      g_string_append_printf(str, "\n"TAB"\\override Staff.StaffSymbol  #'line-count = #%d\n",
			     curstaffstruct->no_of_lines);

    const gchar *endofblock;
    if(curstaff->next && ((DenemoStaff *) curstaff->next->data)->voicenumber == 2)
      endofblock = "";
    else
      endofblock = ">>";

	  if (curstaffstruct->voicenumber != 2)
	    {
	    
	      g_string_append_printf(scoreblock, "%s"TAB""TAB"\\%s%sMusicVoice\n"TAB""TAB"}\n"TAB""TAB"%s\n",str->str, movement_name->str, voice_name->str, endofblock);
	      if (curstaffstruct->haslyrics)
	      {
		g_string_append_printf(scoreblock, 
				"\n"TAB""TAB" \\lyricsto %s%s \\new Lyrics \\%s%sLyrics\n", 
				voice_name->str, movement_name->str, 
				movement_name->str, voice_name->str);
	      }
	    }
	  else if (curstaffstruct->voicenumber == 2)
	    g_string_append_printf(scoreblock, "%s"TAB""TAB"\\%s%s\n"TAB""TAB"}\n"TAB""TAB"%s\n",str->str, movement_name->str, voice_name->str, endofblock);
	  if (curstaffstruct->hasfigures)
	    g_string_append_printf(scoreblock, ""TAB""TAB" \\context FiguredBass \\%s%sBassFiguresLine\n", movement_name->str, voice_name->str);




	    if(curstaffstruct->context & DENEMO_PIANO_END)
	      g_string_append_printf(scoreblock, ">>\n\n");
	    if(curstaffstruct->context & DENEMO_CHOIR_END)
	      g_string_append_printf(scoreblock, ">>\n\n");
	    if(curstaffstruct->context & DENEMO_GROUP_END)
	      g_string_append_printf(scoreblock, ">>\n\n");
	  

       
	}
      }/*end for staff loop */  


    if(visible_movement==1) {
      if (context)
	g_string_append_printf(scoreblock, ""TAB">> \n");
     
      g_string_append_printf(scoreblock,
			     ">>\n>>\n"
			     ""TAB"\\layout {\n"
			     ""TAB"}\n");
     
      /* \header block */
      g_string_append_printf(scoreblock, "\\header{\n");
#define HEADER(field) if(si->headerinfo.field->len) \
g_string_append_printf(scoreblock, ""TAB"%s = \"%s\"\n", #field, si->headerinfo.field->str);
      HEADER(title);
      HEADER(subtitle);
      HEADER(poet);
      HEADER(composer);
      HEADER(meter);
      HEADER(piece);
      HEADER(opus);
      HEADER(arranger);
      HEADER(instrument);
      HEADER(dedication);
      HEADER(head);
      HEADER(copyright);
      HEADER(footer);

      if(si->headerinfo.extra->len)
	g_string_append_printf(scoreblock, ""TAB"%s\n",si->headerinfo.extra->str );
     
      /*end of  \header block */
      g_string_append_printf(scoreblock, ""TAB"}\n\n");
      /*end of \score block */
      g_string_append_printf(scoreblock, "}\n");
 

      /* any markup after score block */
      if(si->headerinfo.lilypond_after->len)
	g_string_append_printf(scoreblock, "%s\n", si->headerinfo.lilypond_after->str);


      // Put this standard scoreblock in the textbuffer

      gtk_text_buffer_get_iter_at_mark(gui->textbuffer, &iter, gtk_text_buffer_get_mark(gui->textbuffer, STANDARD_SCOREBLOCK));
      gtk_text_buffer_insert_with_tags_by_name (gui->textbuffer, &iter,scoreblock->str, -1, INEDITABLE,
						(partname==NULL && gui->custom_scoreblocks)?"invisible":NULL, NULL);

    }/* if visible movement */

    g_string_free(scoreblock, TRUE);
 
  }/* for each movement */




  // now go through gui->anchors, and to each anchor attach a copy of the original text, for checking when saving.
  {
    GList *g;
    for(g=gui->anchors;g;g=g->next) {
      GtkTextChildAnchor *anchor = g->data;
      GString **target = g_object_get_data(G_OBJECT(anchor), TARGET);
      if(target)
	g_object_set_data(G_OBJECT(anchor), ORIGINAL, get_text(gui, anchor));
    }


  }
#if 0
  g_get_current_time(&time);
  g_print("time %ld secs",  time.tv_sec);
  seconds -= time.tv_sec;
  g_print("time diff = %ld\n", seconds);
#endif

{
  GtkTextIter startiter, enditer;
  gtk_text_buffer_get_start_iter (gui->textbuffer, &startiter);
  gtk_text_buffer_get_end_iter (gui->textbuffer, &enditer);
  gtk_text_buffer_apply_tag_by_name (gui->textbuffer,  "monospace", &startiter, &enditer);
}

  gtk_text_buffer_set_modified (gui->textbuffer, FALSE);
} /* output_score_to_buffer */



/**
 * Write out LilyPond to correspond with the music in the DenemoGUI from measure start to measure end
 * in the current movement or all the movements if all_movements is TRUE.
 * param PARTNAME if not NULL, print only this parts of this name
 *
 * The function works in two passes: the first pass writes out all the
 * LilyPond music blocks defined by identifiers;  the second pass writes out the score blocks (one for
 * each movement) with the
 * identifiers placed suitably. 
 */
static void
export_lilypond (gchar * thefilename, DenemoGUI *gui, gboolean all_movements, gchar * partname)
{
  GtkTextIter startiter, enditer;

  output_score_to_buffer (gui, all_movements, partname);
  GString *filename = g_string_new (thefilename);
  if(filename) {
    gtk_text_buffer_get_start_iter (gui->textbuffer, &startiter);
    gtk_text_buffer_get_end_iter (gui->textbuffer, &enditer);
    gchar *lily = gtk_text_buffer_get_text (gui->textbuffer, &startiter, &enditer, FALSE);
    /* Append .ly onto the filename if necessary */
    if (strcmp (filename->str + filename->len - 3, ".ly"))
      g_string_append (filename, ".ly");
    /* Now open the file */
    FILE *fp;
    fp = fopen (filename->str, "w");
    if (!fp)
      {
        warningdialog("Could not open output file for writing");
	g_warning ("Cannot open %s \n", filename->str);
	return;
      }
    fprintf(fp, "%s",lily);
    g_free(lily);
    fclose (fp);
    g_string_free (filename, TRUE);
  }
}

void
exportlilypond (gchar * thefilename, DenemoGUI *gui, gboolean all_movements) {
  export_lilypond(thefilename, gui, all_movements, NULL);
}



 

/* output lilypond for the current staff
 */
void
export_lilypond_part (char *filename,  DenemoGUI *gui, gboolean all_movements)
{
  export_lilypond (filename, gui, all_movements, ((DenemoStaff*)gui->si->currentstaff->data)->lily_name->str);
}
/* output lilypond for each part into a separate file
 */
void
export_lilypond_parts (char *filename, DenemoGUI *gui)
{
  FILE *fp;
  gchar *staff_filename;
  staffnode *curstaff;
  DenemoStaff *curstaffstruct;
  DenemoScore * si = gui->si;
  for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
    {

      curstaffstruct = (DenemoStaff *) curstaff->data;
      gchar *c = filename+strlen(filename);// find .extension FIXME dots in filename
      while(*c!='.'&&c!=filename)
	c--;
      if(c!=filename)
	*c='\0';
      else {
	warningdialog("Filename does not have extension");
	return;
      }
      staff_filename = g_strconcat(filename, "_", curstaffstruct->lily_name->str, ".ly", NULL);
      *c = '.';
      export_lilypond (staff_filename, gui, FALSE,  ((DenemoStaff*)curstaff->data)->lily_name->str);
      
    }

  if (staff_filename)
    g_free (staff_filename);
}

/* callback on destroying lilypond window */
static gboolean lilywindow_destroyed(GtkObject *object, DenemoGUI *gui) {
  merge_lily_strings (gui);
  if(gui==Denemo.gui){
    GtkWidget * toggle = gtk_ui_manager_get_widget (Denemo.ui_manager,
						  "/MainMenu/ViewMenu/ToggleLilyText");
    gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (toggle),
				  FALSE);
  }
  gui->textwindow = NULL;
  gui->textview = NULL;
  gui->lilysync = G_MAXUINT;
  return TRUE;
}


static gboolean 
lily_refresh(GtkWidget *item, GdkEventCrossing *e, DenemoGUI *gui);

static gboolean 
lily_save(GtkWidget *item, GdkEventCrossing *e, DenemoGUI *gui){
  g_print("Consider Save ... %d %d", gui->lilysync, gui->changecount);
  g_signal_handlers_block_by_func (SIGNAL_WIDGET, G_CALLBACK (lily_save), gui);
  if(gui->textwindow) {
    g_signal_handlers_unblock_by_func (G_OBJECT (gui->textwindow), G_CALLBACK (lily_refresh), gui);
    merge_lily_strings(gui);
    gtk_widget_queue_draw (gui->scorearea);
  }
  return FALSE;
}
static gboolean 
lily_refresh(GtkWidget *item, GdkEventCrossing *e, DenemoGUI *gui){
  g_print("Consider Refresh ... %d %d", gui->lilysync, gui->changecount);
  if(gui->textwindow)
    g_signal_handlers_block_by_func(gui->textwindow, G_CALLBACK (lily_refresh), gui);
  g_signal_handlers_unblock_by_func (G_OBJECT (SIGNAL_WIDGET), G_CALLBACK (lily_save), gui);
  
  if( gui->si->markstaffnum || (gui->lilysync!=gui->changecount)) {
    gui->si->markstaffnum =  0;//remove selection, else we will only see that bit in LilyText   
    refresh_lily_cb (NULL, gui);
  }
  return FALSE;
}


static void
prepend_menu_item(GtkMenuShell *menu, DenemoGUI *gui, gchar *text, gpointer callback) {
  GtkWidget* item;
  item = gtk_menu_item_new_with_label(text);
  g_signal_connect(item, "activate",  G_CALLBACK (callback), gui);
  gtk_menu_shell_prepend(menu,GTK_WIDGET( item));
  gtk_widget_show(GTK_WIDGET(item));
}
static gboolean populate_called(GtkWidget *view, GtkMenuShell *menu, DenemoGUI *gui){
  //g_print("populate called with %p\n", menu);
  GtkWidget *item;
  prepend_menu_item(menu, gui, "Find Current Object", (gpointer) place_cursor_cb);
  prepend_menu_item(menu, gui, "Print from visible LilyPond text", (gpointer) print_lily_cb);
  return FALSE;
}


static lily_keypress(GtkWidget *w, GdkEventKey *event, DenemoGUI *gui) {
  GtkTextIter cursor;
  gtk_text_buffer_get_iter_at_mark(gui->textbuffer, &cursor, gtk_text_buffer_get_insert(gui->textbuffer));


#ifndef TEXTBUFFER_BUG_FIXED
  //FIXME workaround for a nasty bug, presumably in GTK, where the back arrow gets a wrong char count, off end of line
  if((event->keyval==65361) && (gtk_text_iter_get_line_offset(&cursor)<2)){
    g_print("avoiding gtk bug...\n");
    GtkTextTag *tag = gtk_text_tag_table_lookup(tagtable, "invisible");
    if(gtk_text_iter_has_tag(&cursor,tag)) {
      while(gtk_text_iter_backward_cursor_position(&cursor) && gtk_text_iter_has_tag(&cursor,tag))
	gtk_text_buffer_place_cursor(gui->textbuffer, &cursor);
      g_print("backed up\n");
      return TRUE;
    }
    if(gtk_text_iter_backward_cursor_position(&cursor))
      gtk_text_buffer_place_cursor(gui->textbuffer, &cursor);
    return TRUE;
  }
#endif

  if(event->state &(GDK_CONTROL_MASK ))
    return FALSE;
  // if you have a visible marker you do this gtk_text_iter_backward_cursor_position(&cursor);
  GtkTextChildAnchor *anchor = gtk_text_iter_get_child_anchor(&cursor);
  // g_print("Got a keypress event at anchor %p\n", anchor);
  //g_print("The character is %x keyval %x at %d\n", (guint)gdk_keyval_to_unicode(event->keyval), event->keyval,  gtk_text_iter_get_line_offset(&cursor));
  if(anchor){
    gint objnum =  (gint) g_object_get_data(G_OBJECT(anchor), OBJECTNUM);
    gint measurenum =  (gint) g_object_get_data(G_OBJECT(anchor), MEASURENUM);
    gint staffnum =  (gint) g_object_get_data(G_OBJECT(anchor), STAFFNUM);
    gint movementnum =  (gint) g_object_get_data(G_OBJECT(anchor), MOVEMENTNUM);
    //g_print("location %d %d %d %d\n", objnum, measurenum, staffnum, movementnum);
    if(movementnum<1)
      return FALSE;
    if(!goto_movement_staff_obj (gui, movementnum, staffnum, measurenum, objnum))
      return FALSE;
    gchar *key = g_strdup_printf("%c",gdk_keyval_to_unicode(event->keyval));
    GList *curobjnode =  gui->si->currentobject;
    DenemoObject *obj =  curobjnode?curobjnode->data:NULL;
    if(obj && *key>0x1f ) {
      switch(obj->type) {
      case LILYDIRECTIVE:
	gtk_text_iter_forward_char (&cursor);// past anchor
	GString **target = g_object_get_data(G_OBJECT(anchor), TARGET);
	if(!*target ){
	  *target = g_string_new(key);
	  //g_print("new string %s (%x)\n", key, *key);
	}
	else {
	  gchar *lily = get_text(gui, anchor); 
	  g_string_assign(*target, lily);//FIXME free original
	
	  g_free(lily);
	  g_string_prepend(*target, key);
	  g_object_set_data(G_OBJECT(anchor),ORIGINAL, get_text(gui, anchor));
	  //g_print("prepended %s (%x)\n", key, *key);
	}
	gui->changecount++;
	refresh_lily_cb(NULL, gui);
	gtk_text_buffer_get_iter_at_mark(gui->textbuffer, &cursor, gtk_text_buffer_get_insert(gui->textbuffer));
	if(gtk_text_iter_forward_char (&cursor))
	  gtk_text_buffer_place_cursor(gui->textbuffer, &cursor);
	g_free(key);
	return TRUE;
      case CHORD:
      default:
	{
	  DenemoObject *lilyobj = lily_directive_new (key);
	  //g_print("inserted a lilydirective  %s (%x)\n", key, *key);
	  g_free(key);
	  object_insert (gui, lilyobj);
	  displayhelper(gui);
	  refresh_lily_cb(NULL, gui);
	  gtk_text_buffer_get_iter_at_mark(gui->textbuffer, &cursor, gtk_text_buffer_get_insert(gui->textbuffer));
	  if(gtk_text_iter_forward_char (&cursor) && gtk_text_iter_forward_char (&cursor))
	    gtk_text_buffer_place_cursor(gui->textbuffer, &cursor);
	  g_free(key);
	  return TRUE;
	}
      }// switch obj->type

    }//if useful keypress??
    g_free(key);
  }//if cursor is at anchor
  return FALSE;//let the normal handler have the keypress
}
static void create_lilywindow(DenemoGUI *gui) {
  gui->textwindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  //gtk_window_set_position (GTK_WINDOW (gui->textwindow), GTK_WIN_POS_NONE);
  gtk_window_set_default_size (GTK_WINDOW (gui->textwindow), 800, 600);
  gtk_window_set_title(GTK_WINDOW (gui->textwindow), "LilyPond Text - Denemo");
  g_signal_connect (G_OBJECT (gui->textwindow), "destroy",
		    G_CALLBACK (lilywindow_destroyed), gui); 
  GtkWidget *vpaned = gtk_vpaned_new ();
  gtk_container_set_border_width (GTK_CONTAINER(vpaned), 5);

  gtk_container_add (GTK_CONTAINER (gui->textwindow), vpaned);
  GtkWidget *view = gtk_text_view_new ();
  GtkWidget *sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
				  GTK_POLICY_AUTOMATIC,
				  GTK_POLICY_AUTOMATIC);
  gtk_paned_add1 (GTK_PANED (vpaned), sw);
  gtk_container_add (GTK_CONTAINER (sw), view);
  gtk_widget_show_all(vpaned);

  GtkTextIter iter;
  tagtable =  (GtkTextTagTable *)gtk_text_tag_table_new();
  gui->textview = (GtkTextView *)view;

#if 1
  g_signal_connect (G_OBJECT (gui->textview), "key-press-event",
		    G_CALLBACK (lily_keypress), gui);
#endif

  g_signal_connect (G_OBJECT (gui->textview), "populate-popup",
		    G_CALLBACK (populate_called), gui);



  /*   g_object_set_data(G_OBJECT (SIGNAL_WIDGET),"enter-signal", (gpointer)id); */
  GtkTextTag *t;
  t = gtk_text_tag_new("invisible");
  g_object_set(G_OBJECT(t),  "invisible", TRUE, NULL);
  gtk_text_tag_table_add (tagtable, t);

  t = gtk_text_tag_new("system_invisible");
  g_object_set(G_OBJECT(t),  "invisible", TRUE, NULL);
  gtk_text_tag_table_add (tagtable, t);

  t = gtk_text_tag_new(INEDITABLE);
  g_object_set(G_OBJECT(t),  "editable", FALSE, NULL);
  gtk_text_tag_table_add (tagtable, t);

  t = gtk_text_tag_new(HIGHLIGHT);
  g_object_set(G_OBJECT(t),  "background", "light gray", NULL);
  gtk_text_tag_table_add (tagtable, t);

  t = gtk_text_tag_new(ERRORTEXT);
  g_object_set(G_OBJECT(t),  "background", "pink", NULL);
  gtk_text_tag_table_add (tagtable, t);


  t = gtk_text_tag_new("bold");
  g_object_set(G_OBJECT(t), "weight", PANGO_WEIGHT_BOLD, "family", "monospace", NULL);
  gtk_text_tag_table_add (tagtable, t);

  t = gtk_text_tag_new("monospace");
  g_object_set(G_OBJECT(t), "family", "monospace", NULL);
  gtk_text_tag_table_add (tagtable, t);

  
  gui->textbuffer = gtk_text_buffer_new(tagtable);
  gtk_text_view_set_buffer (GTK_TEXT_VIEW(gui->textview), gui->textbuffer);
  gui->lilysync = G_MAXUINT;//buffer not yet up to date

  gboolean has_signal = (gboolean)g_object_get_data(G_OBJECT (SIGNAL_WIDGET),"has signal");
  if(has_signal)     
    g_signal_handlers_unblock_by_func (G_OBJECT (SIGNAL_WIDGET), G_CALLBACK (lily_save), gui);//FIXME is it blocked?
  else {
    g_signal_connect (G_OBJECT (SIGNAL_WIDGET), LEAVE_NOTIFY_EVENT ,
		      G_CALLBACK (lily_save), gui);
    g_object_set_data(G_OBJECT (SIGNAL_WIDGET),"has signal", (gpointer)TRUE);
  }
  g_signal_connect (G_OBJECT (gui->textwindow), ENTER_NOTIFY_EVENT,
		    G_CALLBACK (lily_refresh), gui);
  g_signal_handlers_block_by_func(gui->textwindow, G_CALLBACK (lily_refresh), gui);
  //g_signal_handlers_block_by_func (SIGNAL_WIDGET, G_CALLBACK (lily_save), gui);

}
