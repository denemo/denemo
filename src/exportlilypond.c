/* exportlilypond.c
 * Functions for generating LilyPond representation of the music
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
#include "exportlilypond.h"
#include "print.h"
#include "printview.h"
#include "scoreops.h"
#include "objops.h"
#include "xmldefs.h"
#include "lyric.h"
#include "processstaffname.h"
#include "commandfuncs.h"
#include "scorelayout.h"
#include "contexts.h"
#include "draw.h"
#include "view.h"

#define ENTER_NOTIFY_EVENT "focus-in-event"
#define LEAVE_NOTIFY_EVENT "focus-out-event"

#define SIGNAL_WIDGET Denemo.textwindow /* Denemo.window */

#define GSTRINGP "gstring"
#define MUSIC "music"
#define START "start"
#define SCOREBLOCK "scoreblock"
#define CUSTOM "custom"
#define OBJECTNODE "object"
#define ORIGINAL "original"

#define OBJECTNUM "objectnum"
#define TARGETTYPE "type"
#define MIDCOFFSET "midcoffset"
#define DIRECTIVENUM "directivenum"
#define MEASURENUM "measurenum"
#define STAFFNUM "staffnum"
#define MOVEMENTNUM "movementnum"
#define STANDARD_SCOREBLOCK "Standard scoreblock"
#define INEDITABLE "ineditable"
#define HIGHLIGHT "highlight"
#define ERRORTEXT "error text"


gchar *get_postfix (GList * g);

static void output_score_to_buffer (DenemoGUI * gui, gboolean all_movements, gchar * partname);
static GtkTextTagTable *tagtable;

/* inserts a navigation anchor into the lilypond textbuffer at curmark */
static void
place_navigation_anchor (GtkTextMark * curmark, gpointer curobjnode, gint movement_count, gint measurenum, gint voice_count, gint objnum, DenemoTargetType type, gint mid_c_offset)
{
  //GtkTextIter iter = *piter; 
  //put in an ineditable anchor to mark the start of the object
  GtkTextIter iter;
  gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, curmark);
  GtkTextChildAnchor *objanc = gtk_text_buffer_create_child_anchor (Denemo.textbuffer, &iter);
  g_object_set_data (G_OBJECT (objanc), OBJECTNODE, curobjnode);        //store objnode here
  g_object_set_data (G_OBJECT (objanc), MOVEMENTNUM, GINT_TO_POINTER (movement_count));
  g_object_set_data (G_OBJECT (objanc), MEASURENUM, GINT_TO_POINTER (measurenum));
  g_object_set_data (G_OBJECT (objanc), STAFFNUM, GINT_TO_POINTER (voice_count));
  g_object_set_data (G_OBJECT (objanc), OBJECTNUM, GINT_TO_POINTER (objnum));
  g_object_set_data (G_OBJECT (objanc), TARGETTYPE, GINT_TO_POINTER (type));
  g_object_set_data (G_OBJECT (objanc), MIDCOFFSET, GINT_TO_POINTER (mid_c_offset));


  //g_print("place nav anchor marked anchor %p as  type %d\n", objanc, type);
  GtkTextIter back;
  back = iter;
  (void) gtk_text_iter_backward_char (&back);
  gtk_text_buffer_apply_tag_by_name (Denemo.textbuffer, INEDITABLE, &back, &iter);
#if 1                           //0 makes anchors visible - but throws out locations
  gtk_text_buffer_apply_tag_by_name (Denemo.textbuffer, "system_invisible", &back, &iter);
#endif
}

void
highlight_lily_error ()
{
  if (Denemo.textbuffer == NULL)
    return;
  GtkTextIter enditer, iter;
  gtk_text_buffer_get_end_iter (Denemo.textbuffer, &enditer);
  gtk_text_buffer_get_start_iter (Denemo.textbuffer, &iter);
  gtk_text_buffer_remove_tag_by_name (Denemo.textbuffer, ERRORTEXT, &enditer, &iter);
  gint line, column;
  line = (intptr_t) g_object_get_data (G_OBJECT (Denemo.textbuffer), "error line");
  column = (intptr_t) g_object_get_data (G_OBJECT (Denemo.textbuffer), "error column");
  line--;
  if (line > 0)
    {
#ifdef BUG_COLUMN_OFFSET_TOO_LARGE_FIXED
      gtk_text_buffer_get_iter_at_line_offset (Denemo.textbuffer, &iter, line, column);
#else
      gtk_text_buffer_get_iter_at_line_offset (Denemo.textbuffer, &iter, line, 0);
      g_print ("line %d column %d\n", line, column);
      g_print ("line has %d chars\n", gtk_text_iter_get_chars_in_line (&iter));
      while (column--)
        (void) gtk_text_iter_forward_char (&iter);      //EEEK TAB is 8 spaces for lilypond find these!!!!

#endif
      /*     gtk_text_iter_set_line(&iter, line); */
      /*     gtk_text_iter_set_visible_line_offset(&iter, column); */

      gtk_text_buffer_apply_tag_by_name (Denemo.textbuffer, ERRORTEXT, &enditer, &iter);
    }
}


/*  set_lily_error() 
 *  set line, column as the current line and column in Denemo.textbuffer where an error has been found
 *  in the LilyPond interpreter. line starts from 1, column starts from 0
 *  line=0 means no error
 */
void
set_lily_error (gint line, gint column)
{
  if (Denemo.textbuffer)
    {
      g_object_set_data (G_OBJECT (Denemo.textbuffer), "error line", (gpointer) (intptr_t) line);
      g_object_set_data (G_OBJECT (Denemo.textbuffer), "error column", (gpointer) (intptr_t) column);
    }
}

static void
explain_temporary_scoreblock (void)
{
  infodialog (_("This scorelayout is purely for continuous typesetting, and will change as you edit the score.\n"
    "To get a real score layout turn off continuous typesetting in the Print View and "
    "typeset using Part, Movement or Typeset (Default Layout) buttons."));
}

static void
popup_score_layout_options (void)
{
  GtkWidget *menu = gtk_menu_new ();
  GtkWidget *item;
  GList *g;
  for (g = Denemo.gui->standard_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = g->data;
      gchar *text = g_strdup_printf (_("Switch to Layout \"%s\""), sb->name);
      item = gtk_menu_item_new_with_label (text);
      g_free (text);
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (select_standard_layout), sb);
    }
  for (g = Denemo.gui->custom_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = g->data;
      gchar *text = g_strdup_printf (_("Switch to Layout \"%s\""), sb->name);
      item = gtk_menu_item_new_with_label (text);
      g_free (text);
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (select_custom_layout), sb);
    }


  if (Denemo.gui->standard_scoreblocks == NULL)
    {
      item = gtk_menu_item_new_with_label (_("Create Standard Score Layout"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (select_standard_layout), NULL);
    }
  item = gtk_menu_item_new_with_label ("Customize Score Layout");
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (make_scoreblock_editable), NULL);
  gtk_widget_show_all (menu);

  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
}

void
make_scoreblock_editable (void)
{
  if (!gtk_widget_get_visible (Denemo.gui->score_layout))
    activate_action ("/MainMenu/ViewMenu/ToggleScoreLayout");
  create_custom_lilypond_scoreblock ();
  force_lily_refresh (Denemo.gui);
}

/* insert a pair of anchors and a mark to denote a section.
   if str is non-null it is a target for saving edited versions of the section to, in this
   case the start anchor of the section is prepended to the list gui->anchors 
   if name is non-null a button is attached to the start anchor.
*/
static GtkTextChildAnchor *
insert_section (GString ** str, gchar * markname, gchar * name, GtkTextIter * iter, DenemoGUI * gui)
{
  GtkTextIter back;
  GtkTextChildAnchor *objanc = gtk_text_buffer_create_child_anchor (Denemo.textbuffer, iter);
  back = *iter;
  (void) gtk_text_iter_backward_char (&back);
  gtk_text_buffer_apply_tag_by_name (Denemo.textbuffer, INEDITABLE, &back, iter);
  if (name == NULL)
    gtk_text_buffer_apply_tag_by_name (Denemo.textbuffer, "system_invisible", &back, iter);
  gtk_text_buffer_insert (Denemo.textbuffer, iter, "\n", -1);
  GtkTextChildAnchor *endanc = gtk_text_buffer_create_child_anchor (Denemo.textbuffer, iter);
  back = *iter;
  (void) gtk_text_iter_backward_char (&back);
  gtk_text_buffer_apply_tag_by_name (Denemo.textbuffer, INEDITABLE, &back, iter);

  gtk_text_buffer_apply_tag_by_name (Denemo.textbuffer, "system_invisible", &back, iter);
  g_object_set_data (G_OBJECT (objanc), "end", (gpointer) endanc);
  g_object_set_data (G_OBJECT (objanc), GSTRINGP, (gpointer) str);
  if (str)
    gui->anchors = g_list_prepend (gui->anchors, objanc);

  if (name)
    {
      if (!strcmp (markname, "standard scoreblock"))
        {
          GtkWidget *button = gtk_button_new ();
          gtk_button_set_label (GTK_BUTTON (button), _("Score Layout Options"));

          g_signal_connect (G_OBJECT (button), "button-press-event", G_CALLBACK (popup_score_layout_options), NULL);
          gtk_widget_show_all (button);
          gtk_text_view_add_child_at_anchor (GTK_TEXT_VIEW (Denemo.textview), button, objanc);
        }
      else if (!strcmp (markname, "temporary scoreblock"))
        {
          GtkWidget *button = gtk_button_new ();
          gtk_button_set_label (GTK_BUTTON (button), _("Temporary Score Layout"));

          g_signal_connect (G_OBJECT (button), "button-press-event", G_CALLBACK (explain_temporary_scoreblock), NULL);
          gtk_widget_show_all (button);
          gtk_text_view_add_child_at_anchor (GTK_TEXT_VIEW (Denemo.textview), button, objanc);
        }
      else
        {
          char *markup = g_markup_printf_escaped ("<tt><big>%% %s</big></tt>\n", name); //monospace label to get serifs
          GtkWidget *label = gtk_label_new ("");
          gtk_label_set_markup (GTK_LABEL (label), markup);
          gtk_text_view_add_child_at_anchor (GTK_TEXT_VIEW (Denemo.textview), label, objanc);
          gtk_widget_show_all (label);
        }
    }
  (void) gtk_text_iter_backward_char (iter);
  (void) gtk_text_iter_backward_char (iter);
  gtk_text_buffer_create_mark (Denemo.textbuffer, markname, iter, FALSE);
  return objanc;
}

#define FAKECHORD_SEP " |"      /* allow space or | to separate chord symbols */

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
      // *clefname = "\"G_8\"";
      *clefname = "\"treble_8\"";
      break;
    case DENEMO_F_8_CLEF:
      *clefname = "\"bass_8\"";
      break;
    case DENEMO_TENOR_CLEF:
      *clefname = "tenor";
      break;
    case DENEMO_SOPRANO_CLEF:
      *clefname = "soprano";
      break;
    case DENEMO_FRENCH_CLEF:
      *clefname = "french";
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
  if (internalduration < 0)
    return internalduration;
  return 1 << internalduration;
}


/**
 * append mudela duration information to FIGURES.
 * This could be optimized to remember the previous value and
 * avoid repetition - an initialization call would be needed to
 * set up initial values in that case
 */
static GString *
append_duration (GString * figures, gint duration, gint numdots)
{
  int i;
  g_string_sprintfa (figures, "%d", duration);
  for (i = 0; i < numdots; i++)
    figures = g_string_append (figures, ".");
  return figures;
}

static void
output_figured_bass_prefix (GString * figures, DenemoDirective * directive)
{
  if ((directive->override & DENEMO_ALT_OVERRIDE) && directive->prefix)
    g_string_append (figures, directive->prefix->str);
}

/**
 * add figures to *pfigures for *pchord  
 */
static void
output_figured_bass (GString * figures, chord * pchord)
{
  static gboolean continuation = FALSE;
  gboolean continuation_finishing = FALSE;
  static GString *last_figure;  // for continuations
  gint duration = internaltomuduration (pchord->baseduration);
  gint numdots = pchord->numdots;
  GString *fig_str;             /*working copy of figures string stored in pchord */
  char *str;                    /* pointer into the figure string fig_str */
  gint num_groups = 1;          /* number of groups of figures */
  gchar *duration_string = NULL;        //whole measure rests etc

  //First any override (e.g. to tweak position of figures) 
  //This is stored in note-directives with DENEMO_ALT_OVERRIDE set.
  // This does mean that figures on rests can only be tweaked with a postfix on the previous note.
  DenemoDirective *directive;
  if (pchord->notes && (note *) (pchord->notes->data) && ((note *) (pchord->notes->data))->directives && (directive = ((note *) (pchord->notes->data))->directives->data))
    {
      output_figured_bass_prefix (figures, directive);
    }


  if (duration < 0)
    {
      gchar *lily = get_postfix (pchord->directives);
      if (lily)
        {
          duration_string = g_strrstr (lily, "R1*");
          if (!duration_string)
            {
              g_warning ("duration is special but cannot find R1* - output in figured bass");
              duration_string = g_strdup ("R1*4/4");
            }
        }
      else
        {
          g_warning ("duration is special but no directives to account for it - output in figured bass");
          duration_string = g_strdup ("R1*4/4");
        }
      duration_string++;        //the duration, after the  R
    }
#define APPEND_DUR(a, b, c) duration<0?g_string_append(figures, duration_string):append_duration(a,b,c)


  if (!last_figure)
    last_figure = g_string_new ("");

  if (pchord->figure == NULL || (((GString *) ((chord *) pchord->figure))->str) == NULL)
    fig_str = g_string_new ("_");       /* the no-figure figure */
  else
    fig_str = g_string_new (((GString *) ((chord *) pchord->figure))->str);
  gchar *figstr = fig_str->str;
//handle direct insert into the figured bass staff

  if (*figstr == '$')
    {
      figstr++;
      str = strchr (figstr, '$');
      if (str)
        {
          *str = 0;
          g_string_append (figures, figstr);
          *str = '$';
          figstr = str + 1;
        }
    }




  if (*figstr == '~')
    {
      if (!continuation)
        {
          figures = g_string_append (figures, " \\set Staff.useBassFigureExtenders = ##t ");
          continuation = TRUE;
        }
      if (last_figure->len)
        {
          figures = g_string_append (figures, "<");
          figures = g_string_append (figures, last_figure->str);
        }
    }
  else
    figures = g_string_append (figures, "<");

  /* multiple figures are separated by a FIGURES_SEP char,
     output these at subdivisions of the duration */
  str = strchr (figstr, *(char *) FIGURES_SEP);
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
      if (*figstr != '~')
        {

          figures = g_string_append (figures, figstr);
          if (continuation)
            {
              continuation_finishing = TRUE;
              figures = g_string_append (figures, " \\! ");
            }
          g_string_assign (last_figure, figstr);
        }
      figures = g_string_append (figures, ">");
      APPEND_DUR (figures, duration, numdots);
      if (continuation_finishing)
        {
          figures = g_string_append (figures, "\\set Staff.useBassFigureExtenders = ##f ");
          continuation = FALSE;
        }
      break;
      /* Each group of figures is assigned a duration to
         achieve a normal looking output */
    case 2:
      {
        gint first_duration, second_duration;
        if (numdots)
          {                     /* divide unequally */
            first_duration = duration;
            second_duration = duration * 2;
          }
        else
          {
            first_duration = second_duration = duration * 2;
          }
        str = strtok (figstr, FIGURES_SEP);
        figures = g_string_append (figures, str);
        figures = g_string_append (figures, ">");
        APPEND_DUR (figures, first_duration, 0);


        if (pchord->notes && (note *) (pchord->notes->data) && ((note *) (pchord->notes->data))->directives && ((note *) (pchord->notes->data))->directives->next && (directive = ((note *) (pchord->notes->data))->directives->next->data))
          {
            output_figured_bass_prefix (figures, directive);
          }


        figures = g_string_append (figures, "<");
        str = strtok (NULL, FIGURES_SEP);
        figures = g_string_append (figures, str);
        figures = g_string_append (figures, ">");
        APPEND_DUR (figures, second_duration, 0);
      }
      break;
    case 3:
      {
        gint first_duration, second_duration, third_duration;
        if (numdots == 1)
          {                     /* divide equally */

            first_duration = second_duration = third_duration = duration * 2;
          }
        else if (numdots == 2)
          {
            first_duration = second_duration = duration * 2;
            third_duration = duration * 4;
          }                     /* no more dots please! */
        else
          {                     /* divide unequally */
            first_duration = duration * 2;
            second_duration = third_duration = duration * 4;
          }
        str = strtok (figstr, FIGURES_SEP);
        figures = g_string_append (figures, str);
        figures = g_string_append (figures, ">");
        APPEND_DUR (figures, first_duration, 0);
        if (pchord->notes && (note *) (pchord->notes->data) && ((note *) (pchord->notes->data))->directives && ((note *) (pchord->notes->data))->directives->next && (directive = ((note *) (pchord->notes->data))->directives->next->data))
          {
            output_figured_bass_prefix (figures, directive);
          }
        figures = g_string_append (figures, "<");
        str = strtok (NULL, FIGURES_SEP);
        figures = g_string_append (figures, str);
        figures = g_string_append (figures, ">");
        APPEND_DUR (figures, second_duration, 0);
        if (pchord->notes && (note *) (pchord->notes->data) && ((note *) (pchord->notes->data))->directives && ((note *) (pchord->notes->data))->directives->next && ((note *) (pchord->notes->data))->directives->next->next && (directive = ((note *) (pchord->notes->data))->directives->next->next->data))
          {
            output_figured_bass_prefix (figures, directive);
          }
        str = strtok (NULL, FIGURES_SEP);
        figures = g_string_append (figures, "<");
        figures = g_string_append (figures, str);
        figures = g_string_append (figures, ">");
        APPEND_DUR (figures, third_duration, 0);
      }
      break;
    case 4:
      {
        gint first_duration, second_duration, third_duration, fourth_duration;
        if (numdots == 1)
          {                     /* divide unequally */

            first_duration = second_duration = duration * 2;
            third_duration = fourth_duration = duration * 4;
          }
        else if (numdots == 2)
          {
            first_duration = second_duration = duration * 2;
            third_duration = duration * 4;
            fourth_duration = duration * 8;
          }                     /* no more dots please! */
        else
          {                     /* divide equally */
            first_duration = second_duration = third_duration = fourth_duration = duration * 4;
          }
        str = strtok (figstr, FIGURES_SEP);
        figures = g_string_append (figures, str);
        figures = g_string_append (figures, ">");
        APPEND_DUR (figures, first_duration, 0);
        if (pchord->notes && (note *) (pchord->notes->data) && ((note *) (pchord->notes->data))->directives && ((note *) (pchord->notes->data))->directives->next && (directive = ((note *) (pchord->notes->data))->directives->next->data))
          {
            output_figured_bass_prefix (figures, directive);
          }
        figures = g_string_append (figures, "<");
        str = strtok (NULL, FIGURES_SEP);
        figures = g_string_append (figures, str);
        figures = g_string_append (figures, ">");
        APPEND_DUR (figures, second_duration, 0);
        if (pchord->notes && (note *) (pchord->notes->data) && ((note *) (pchord->notes->data))->directives && ((note *) (pchord->notes->data))->directives->next && ((note *) (pchord->notes->data))->directives->next->next && (directive = ((note *) (pchord->notes->data))->directives->next->next->data))
          {
            output_figured_bass_prefix (figures, directive);
          }
        str = strtok (NULL, FIGURES_SEP);
        figures = g_string_append (figures, "<");
        figures = g_string_append (figures, str);
        figures = g_string_append (figures, ">");
        APPEND_DUR (figures, third_duration, 0);
        if (pchord->notes && (note *) (pchord->notes->data) && ((note *) (pchord->notes->data))->directives && ((note *) (pchord->notes->data))->directives->next && ((note *) (pchord->notes->data))->directives->next->next && (directive = ((note *) (pchord->notes->data))->directives->next->next->data))
          {
            output_figured_bass_prefix (figures, directive);
          }
        str = strtok (NULL, FIGURES_SEP);
        figures = g_string_append (figures, "<");
        figures = g_string_append (figures, str);
        figures = g_string_append (figures, ">");
        APPEND_DUR (figures, fourth_duration, 0);
      }
      break;
    }
  // if(duration_string) g_free(--duration_string);
}

static gchar *
parse_extension (gchar * input)
{
  gchar *colon = strtok (input, ":");
  if (colon)
    colon = strtok (NULL, ":");

  return colon;
}

/**
 * add figures to *pfigures for *pchord  
 */
static void
output_fakechord (GString * fakechord, chord * pchord)
{
  gint duration = internaltomuduration (pchord->baseduration);
  gint numdots = pchord->numdots;
  GString *fig_str;             /*working copy of figures string */
  char *str;                    /* pointer into the figure string fig_str */
  gint num_groups = 1;          /* number of groups of figures */
  gchar *extension;
  fakechord = g_string_append (fakechord, " ");
  if (pchord->fakechord == NULL || (((GString *) ((chord *) pchord->fakechord))->len) == 0)
    fig_str = g_string_new ("s");       /* the no-fakechord figure */
  else
    {
      fig_str = g_string_ascii_down (g_string_new (((GString *) ((chord *) pchord->fakechord))->str));
    }

  str = strchr (fig_str->str, *(char *) FAKECHORD_SEP);
  if (str != NULL)
    {
      /* we have more than one group of figures to be output
         for one bass note. Count the number of groups */
      num_groups = 2;
      /* one on either side of the FAKECHORD_SEP found */
      while ((str = strchr (++str, *(char *) FAKECHORD_SEP)) != NULL)
        num_groups++;
    }

  switch (num_groups)
    {
    default:
    case 1:
      {
        extension = parse_extension (fig_str->str);
        fakechord = g_string_append (fakechord, fig_str->str);
        append_duration (fakechord, duration, numdots);
        if (extension)
          g_string_append_printf (fakechord, "%c%s", ':', extension);
        break;
      }
      /* Each group of fakechord is assigned a duration to
         achieve a normal looking output */
    case 2:
      {
        gint first_duration, second_duration;
        if (numdots)
          {                     /* divide unequally */
            first_duration = duration;
            second_duration = duration * 2;
          }
        else
          {
            first_duration = second_duration = duration * 2;
          }
        str = strtok (fig_str->str, FAKECHORD_SEP);
        gint length = strlen (str);
        extension = parse_extension (str);
        fakechord = g_string_append (fakechord, str);
        fakechord = g_string_append (fakechord, " ");
        append_duration (fakechord, first_duration, 0);
        if (extension)
          g_string_append_printf (fakechord, "%c%s", ':', extension);

        fakechord = g_string_append (fakechord, " ");
        str = strtok (fig_str->str + length + 1, FAKECHORD_SEP);
        extension = parse_extension (str);
        fakechord = g_string_append (fakechord, str);
        fakechord = g_string_append (fakechord, " ");
        append_duration (fakechord, second_duration, 0);
        if (extension)
          g_string_append_printf (fakechord, "%c%s", ':', extension);
      }
      break;
    case 3:
      {
        gint first_duration, second_duration, third_duration;
        if (numdots == 1)
          {                     /* divide equally */

            first_duration = second_duration = third_duration = duration * 2;
          }
        else if (numdots == 2)
          {
            first_duration = second_duration = duration * 2;
            third_duration = duration * 4;
          }                     /* no more dots please! */
        else
          {                     /* divide unequally */
            first_duration = duration * 2;
            second_duration = third_duration = duration * 4;
          }
        str = strtok (fig_str->str, FAKECHORD_SEP);
        gint length = strlen (str);
        extension = parse_extension (str);
        fakechord = g_string_append (fakechord, str);
        fakechord = g_string_append (fakechord, " ");
        append_duration (fakechord, first_duration, 0);
        if (extension)
          g_string_append_printf (fakechord, "%c%s", ':', extension);
        fakechord = g_string_append (fakechord, " ");
        str = strtok (fig_str->str + length + 1, FAKECHORD_SEP);
        length += strlen (str);
        extension = parse_extension (str);

        fakechord = g_string_append (fakechord, str);
        fakechord = g_string_append (fakechord, " ");
        append_duration (fakechord, second_duration, 0);
        if (extension)
          g_string_append_printf (fakechord, "%c%s", ':', extension);
        fakechord = g_string_append (fakechord, " ");
        str = strtok (fig_str->str + length + 2, FAKECHORD_SEP);
        extension = parse_extension (str);
        fakechord = g_string_append (fakechord, str);
        fakechord = g_string_append (fakechord, " ");
        append_duration (fakechord, third_duration, 0);
        if (extension)
          g_string_append_printf (fakechord, "%c%s", ':', extension);
        fakechord = g_string_append (fakechord, " ");
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
 * ITER: the current iter in Denemo.textbuffer
 
 * GUI: the gui with the textbuffer
 * 
 */
static void
insert_editable (GString ** pdirective, gchar * original, GtkTextIter * iter, DenemoGUI * gui, GString * lily_for_obj, DenemoTargetType type, gint movement_count, gint measurenum, gint voice_count, gint objnum, gint directive_index, gint midcoffset)
{
  gint directivenum = directive_index + 1;
  GtkTextChildAnchor *lilyanc = gtk_text_buffer_create_child_anchor (Denemo.textbuffer, iter);
  GtkTextIter back;
  back = *iter;
  (void) gtk_text_iter_backward_char (&back);
  gtk_text_buffer_apply_tag_by_name (Denemo.textbuffer, INEDITABLE, &back, iter);
  gtk_text_buffer_apply_tag_by_name (Denemo.textbuffer, "system_invisible", &back, iter);
  g_object_set_data (G_OBJECT (lilyanc), GSTRINGP, (gpointer) pdirective);
  g_object_set_data (G_OBJECT (lilyanc), ORIGINAL, original);

  g_object_set_data (G_OBJECT (lilyanc), MOVEMENTNUM, GINT_TO_POINTER (movement_count));
  g_object_set_data (G_OBJECT (lilyanc), MEASURENUM, GINT_TO_POINTER (measurenum));
  g_object_set_data (G_OBJECT (lilyanc), STAFFNUM, GINT_TO_POINTER (voice_count));
  g_object_set_data (G_OBJECT (lilyanc), OBJECTNUM, GINT_TO_POINTER (objnum));
  g_object_set_data (G_OBJECT (lilyanc), TARGETTYPE, GINT_TO_POINTER (type));
  if (directivenum)
    g_object_set_data (G_OBJECT (lilyanc), DIRECTIVENUM, GINT_TO_POINTER (directivenum));
  //g_print("insert editable marked target anchor %p directivenum %d type %d\n", lilyanc, directivenum, type);
  if (midcoffset)
    g_object_set_data (G_OBJECT (lilyanc), MIDCOFFSET, GINT_TO_POINTER (midcoffset));

  if (type)
    g_object_set_data (G_OBJECT (lilyanc), TARGETTYPE, GINT_TO_POINTER (type));
  //g_print("marked anchor %p as %d %d %d %d type %d\n", lilyanc, movement_count, measurenum, voice_count, objnum, type);
  gui->anchors = g_list_prepend (gui->anchors, lilyanc);
  gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, iter, g_strdup (original), -1, "bold", NULL);
  if (lily_for_obj)
    g_string_append (lily_for_obj, original);
  GtkTextChildAnchor *endanc = gtk_text_buffer_create_child_anchor (Denemo.textbuffer, iter);
  back = *iter;
  (void) gtk_text_iter_backward_char (&back);
  gtk_text_buffer_apply_tag_by_name (Denemo.textbuffer, INEDITABLE, &back, iter);
  gtk_text_buffer_apply_tag_by_name (Denemo.textbuffer, "system_invisible", &back, iter);
  g_object_set_data (G_OBJECT (lilyanc), "end", (gpointer) endanc);
  if ((*pdirective) == NULL || (*pdirective)->len == 0)
    gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, iter, " ", -1, HIGHLIGHT, NULL);
}

static gint
brace_count (gchar * str)
{
  gint ret;
  for (ret = 0; *str; str++)
    {
      if (*str == '{')
        ret++;
      if (*str == '}')
        ret--;
    }
  return ret;
}

// get_overridden_prefix, postfix returns the relevant fields from the directive list assembled with those marked as overriding lilypond  in front and other at end; ones with AFFIX not matching override are omitted.
#define GET_OVERRIDDEN_AFFIX(field)\
static gchar *get_overridden_##field(GList *g, gboolean override) {\
  if(g==NULL)\
    return g_strdup("");\
  GString *ret = g_string_new("");\
  for(;g;g=g->next) {\
    DenemoDirective *d = g->data;\
    if(override == ((d->override&DENEMO_OVERRIDE_AFFIX)==0))\
      continue;					\
    if(!((d->override & DENEMO_OVERRIDE_HIDDEN))){\
      if(d->field && d->field->len) {\
	if(d->override & DENEMO_OVERRIDE_LILYPOND)\
	  g_string_prepend(ret, d->field->str);\
	else\
	  g_string_append(ret, d->field->str);\
      }\
    }\
  }\
return g_string_free(ret, FALSE);\
}

#define GET_AFFIX(field)\
   gchar *get_##field(GList *g) {\
  return get_overridden_##field(g, FALSE);\
}

GET_OVERRIDDEN_AFFIX (prefix);
GET_OVERRIDDEN_AFFIX (postfix);

GET_AFFIX (prefix);
GET_AFFIX (postfix);




/* insert editable prefix string from passed directives, updating duration and open brace count
 omit or include those with AFFIX override set. Skip any directive with HIDDEN attribute set */



//NAVANC
#define DIRECTIVES_INSERT_EDITABLE_AFFIX(field) static void \
directives_insert_##field##_editable (GList *directives, gint *popen_braces, gint *pprevduration, GtkTextIter *iter, gboolean override, GString *lily_for_obj,\
						DenemoTargetType type, gint movement_count,	gint measurenum, gint voice_count, gint objnum, gint midcoffset) {\
  DenemoGUI *gui = Denemo.gui;\
  GList *g = directives; gint num;\
  for(num=0;g;g=g->next, num++) {\
    DenemoDirective *directive = (DenemoDirective *)g->data;\
    if(override == ((directive->override&DENEMO_OVERRIDE_AFFIX)==0))\
      continue;\
    if(directive->override&DENEMO_OVERRIDE_HIDDEN)\
      continue;\
    if(directive->field && directive->field->len) {\
      if(pprevduration) *pprevduration = -1;		    \
      if(popen_braces) *popen_braces += brace_count(directive->field->str); \
      insert_editable(&directive->field, directive->field->str, iter, gui, lily_for_obj\
      , type, movement_count, measurenum, voice_count, objnum, num, midcoffset);\
    }\
  }\
}

DIRECTIVES_INSERT_EDITABLE_AFFIX (prefix);
DIRECTIVES_INSERT_EDITABLE_AFFIX (postfix);

static void
directives_insert_affix_postfix_editable (GList * directives, gint * popen_braces, gint * pprevduration, GtkTextIter * iter, GString * lily_for_obj, DenemoTargetType type, gint movement_count, gint measurenum, gint voice_count, gint objnum, gint midcoffset)
{
  DenemoGUI *gui = Denemo.gui;
  GList *g = directives;;
  gint num;
  for (num = 0; g; g = g->next, num++)
    {
      DenemoDirective *directive = (DenemoDirective *) g->data;
      if (!(directive->override & DENEMO_OVERRIDE_AFFIX))
        continue;
      if (directive->override & DENEMO_OVERRIDE_HIDDEN)
        continue;
      if (directive->postfix && directive->postfix->len)
        {
          if (pprevduration)
            *pprevduration = -1;
          if (popen_braces)
            *popen_braces += brace_count (directive->postfix->str);
          insert_editable (&directive->postfix, directive->postfix->str, iter, gui, lily_for_obj, type, movement_count, measurenum, voice_count, objnum, num, midcoffset);
        }
    }
}


/* returns if there is a directive overriding the normal LilyPond output */
static gint
get_lily_override (GList * g)
{
  return get_override (g) & DENEMO_OVERRIDE_LILYPOND;
}




/**
 * generate the lilypond for the DenemoObject curobj
 * the state of the prevailing duration, clef keysignature are updated and returned.
 * returns the excess of open braces "{" created by this object.
 */
static gint
generate_lily_for_obj (DenemoGUI * gui, GtkTextIter * iter, DenemoObject * curobj, gint * pprevduration, gint * pprevnumdots, gchar ** pclefname, gchar ** pkeyname, gint * pcur_stime1, gint * pcur_stime2, gint * pgrace_status, GString * figures, GString * fakechords, GtkTextMark * curmark, gpointer curobjnode, gint movement_count, gint measurenum, gint voice_count, gint objnum)
{
  GString *lily_for_obj = g_string_new ("");
  GString *ret = g_string_new ("");     //no longer returned, instead put into *music
#define outputret gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, iter, ret->str, -1, INEDITABLE, NULL), \
    g_string_append(lily_for_obj, ret->str);\
    open_braces +=  brace_count(ret->str), \
    g_string_assign(ret, "")
#define output(astring) (gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, iter, astring, -1, INEDITABLE, NULL));\
			g_string_append(lily_for_obj, astring);
  gint prevduration = *pprevduration;
  gint prevnumdots = *pprevnumdots;
  chord *pchord;
  gchar *clefname = *pclefname;
  gchar *keyname = *pkeyname;
  gint cur_stime1 = *pcur_stime1;
  gint cur_stime2 = *pcur_stime2;
  gint j, k;
  gint duration, numdots;
  gboolean is_normalnotehead = TRUE;
  gboolean is_chordmode = FALSE;
  gint octave, enshift;
  gint noteheadtype;
  gint mid_c_offset;
  gint open_braces = 0;         /* keep track of excess open braces "{" so as to ensure they are closed */

  GString *dynamic_string = NULL;

  gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, iter, " ", -1, INEDITABLE, HIGHLIGHT, NULL);     //A gray blank between objects
  g_string_append (lily_for_obj, " ");


#define NAVANC(type, offset)  place_navigation_anchor(curmark, (gpointer)curobjnode, movement_count, measurenum, voice_count, objnum, type, offset);\
								gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, iter, curmark);


  switch (curobj->type)
    {
    case CHORD:
      {
        gint lily_override;
        pchord = (chord *) curobj->object;
        gchar *chord_prefix = get_prefix (pchord->directives);
        duration = internaltomuduration (pchord->baseduration);
        numdots = pchord->numdots;
        is_chordmode = FALSE;
        lily_override = get_lily_override (pchord->directives);

        if (!lily_override)
          if ((!*pgrace_status) && pchord->is_grace)
            {
              *pgrace_status = TRUE;
              if (pchord->is_grace & GRACED_NOTE)
                g_string_append_printf (ret, "\\grace {");
              else
                g_string_append_printf (ret, "\\acciaccatura {");
              if (figures->len)
                g_string_append_printf (figures, "\\grace {");
              if (fakechords->len)
                g_string_append_printf (fakechords, "\\grace {");
            }
        /* prefix is before duration unless AFFIX override is set */
        directives_insert_prefix_editable (pchord->directives, &open_braces, &prevduration, iter, !lily_override, lily_for_obj, TARGET_CHORD, movement_count, measurenum, voice_count, objnum, 0);

        if (!lily_override)
          {                     //all LilyPond is output for this chord
            if (!pchord->notes)
              {                 /* A rest */
                if (!curobj->isinvisible)
                  {
                    g_string_append_printf (ret, "r");
                    /* Duplicated code follows. I ought to fix that */

                    NAVANC (TARGET_CHORD, 0);
                    outputret;
                    directives_insert_prefix_editable (pchord->directives, &open_braces, &prevduration, iter, FALSE, lily_for_obj, TARGET_CHORD, movement_count, measurenum, voice_count, objnum, 0);
                    if (duration != prevduration || numdots != prevnumdots || duration < 0)
                      {
                        /* only in this case do we explicitly note the duration */
                        if (duration > 0)
                          g_string_append_printf (ret, "%d", duration);
                        prevduration = duration;
                        prevnumdots = numdots;
                        for (j = 0; j < numdots; j++)
                          g_string_append_printf (ret, ".");
                      }
                  }
                else
                  {             /* non printing rest */
                    g_string_append_printf (ret, "\\skip ");
                    NAVANC (TARGET_CHORD, 0);
                    outputret;
                    directives_insert_prefix_editable (pchord->directives, &open_braces, &prevduration, iter, FALSE, lily_for_obj, TARGET_CHORD, movement_count, measurenum, voice_count, objnum, 0);
                    if (duration > 0)
                      g_string_append_printf (ret, "%d", duration);
                    prevduration = -1;
                    prevnumdots = -1;
                    for (j = 0; j < numdots; j++)
                      g_string_append_printf (ret, ".");
                  }

                outputret;
                directives_insert_postfix_editable (pchord->directives, &open_braces, &prevduration, iter, FALSE, lily_for_obj, TARGET_CHORD, movement_count, measurenum, voice_count, objnum, 0);
              }
            else                /* there are notes */
              {
                if (pchord->notes->next || pchord->chordize)
                  {             //multinote chord
                    is_chordmode = TRUE;
                    g_string_append_printf (ret, "<");
                  }
                GList *notenode;
                outputret;
                for (notenode = pchord->notes; notenode; notenode = notenode->next)
                  {
                    note *curnote = (note *) notenode->data;
                    noteheadtype = curnote->noteheadtype;
                    //As with chord-prefix, this is perhaps not a useful position, but until some other use is found for this field it is here...
                    GList *g = curnote->directives;
                    gint num = 0;
                    for (; g; g = g->next, num++)
                      {
                        DenemoDirective *directive = (DenemoDirective *) g->data;
                        if (directive->prefix && !(directive->override & DENEMO_ALT_OVERRIDE))
                          {
                            prevduration = -1;
                            insert_editable (&directive->prefix, directive->prefix->len ? directive->prefix->str : " ", iter, gui, lily_for_obj, TARGET_NOTE, movement_count, measurenum, voice_count, objnum, num, curnote->mid_c_offset);
                          }
                      }

                    if (!get_lily_override (curnote->directives))
                      {         //not skip all LilyPond output for this note
                        switch (noteheadtype)
                          {
                          case DENEMO_NORMAL_NOTEHEAD:
                            if (!is_normalnotehead)
                              {
                                g_string_append_printf (ret, "\n" TAB "\\revert NoteHead #'style ");
                                is_normalnotehead = !is_normalnotehead;
                              }
                            break;
                          case DENEMO_CROSS_NOTEHEAD:
                            g_string_append_printf (ret, "\n" TAB "\\once \\override NoteHead #'style = #'cross ");
                            is_normalnotehead = FALSE;
                            break;
                          case DENEMO_HARMONIC_NOTEHEAD:
                            g_string_append_printf (ret, "\n" TAB "\\once \\override NoteHead #'style = #'harmonic ");
                            is_normalnotehead = FALSE;
                            break;
                          case DENEMO_DIAMOND_NOTEHEAD:
                            g_string_append_printf (ret, "\n" TAB "\\once \\override Voice.NoteHead #'style = #'diamond ");
                            is_normalnotehead = FALSE;
                            break;
                          default:
                            g_string_append_printf (ret, "\n" TAB "\\revert Voice.NoteHead #'style ");
                            break;
                          }
                        mid_c_offset = curnote->mid_c_offset;
                        g_string_append_printf (ret, "%c", mid_c_offsettoname (mid_c_offset));
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
                        NAVANC (TARGET_NOTE, mid_c_offset);     //we target the note
                        outputret;
                        g = curnote->directives;
                        if (!g && notenode->next)
                          output (" ");
                        for (num = 0; g; g = g->next, num++)
                          {
                            DenemoDirective *directive = (DenemoDirective *) g->data;
                            if (directive->postfix && !(directive->override & DENEMO_OVERRIDE_HIDDEN) && !(directive->override & DENEMO_ALT_OVERRIDE))
                              {
                                insert_editable (&directive->postfix, directive->postfix->len ? directive->postfix->str : " ", iter, gui, lily_for_obj, TARGET_NOTE, movement_count, measurenum, voice_count, objnum, num, curnote->mid_c_offset);
                                prevduration = -1;
                              }
                            else if (notenode->next)
                              output (" ");
                          }     //for directives
                      }         /* End of LilyPond output for note, skipped if override set ie !get_lily_override (curnote->directives) */
                  }             /* End notes in chord loop */

                if (pchord->notes->next || pchord->chordize)    //multi-note chord
                  g_string_append_printf (ret, ">");

                if (duration != prevduration || numdots != prevnumdots || duration < 0)
                  {
                    /* only in this case do we explicitly note the duration */
                    outputret;
                    directives_insert_prefix_editable (pchord->directives, &open_braces, &prevduration, iter, FALSE, lily_for_obj, TARGET_CHORD, movement_count, measurenum, voice_count, objnum, 0);
                    if (duration > 0)
                      g_string_append_printf (ret, "%d", duration);
                    prevduration = duration;
                    prevnumdots = numdots;
                    for (j = 0; j < numdots; j++)
                      g_string_append_printf (ret, ".");
                    outputret;
                  }
                else
                  {
                    outputret;
                    directives_insert_prefix_editable (pchord->directives, &open_braces, &prevduration, iter, FALSE, lily_for_obj, TARGET_CHORD, movement_count, measurenum, voice_count, objnum, 0);
                    outputret;
                  }

                directives_insert_postfix_editable (pchord->directives, &open_braces, &prevduration, iter, FALSE, lily_for_obj, TARGET_CHORD, movement_count, measurenum, voice_count, objnum, 0);
//!!! dynamics like \cr have their own positional info in LilyPond - how to tell Denemo????
                if (pchord->dynamics && (pchord->notes->next == NULL))
                  {
                    dynamic_string = (GString *) pchord->dynamics->data;
                    if (is_chordmode)
                      g_string_append_printf (ret, "\\%s", dynamic_string->str);
                    else
                      g_string_append_printf (ret, "\\%s ", dynamic_string->str);
                  }

//!! dynamics like \cr have their own positional info in LilyPond - how to tell Denemo????
//We have to output the cresc/dim begin and the slur begin and tie with a NAVANC(type, 0) before each one.
                if (pchord->crescendo_begin_p)
                  {
                    NAVANC (TARGET_CRESC, 0);
                    g_string_append_printf (ret, " \\cr");
                    outputret;
                  }
                if (pchord->diminuendo_begin_p)
                  {
                    NAVANC (TARGET_DIM, 0);
                    g_string_append_printf (ret, " \\decr");
                    outputret;
                  }

                if (pchord->slur_begin_p)
                  {
                    NAVANC (TARGET_SLUR, 0);
                    g_string_append_printf (ret, "(");
                    outputret;
                  }
                if (pchord->is_tied)
                  {
                    NAVANC (TARGET_TIE, 0);
                    g_string_append_printf (ret, " ~");
                    outputret;
                  }
                if (pchord->crescendo_end_p)
                  g_string_append_printf (ret, " \\!");
                if (pchord->diminuendo_end_p)
                  g_string_append_printf (ret, " \\!");
                if (pchord->slur_end_p)
                  g_string_append_printf (ret, ")");
                outputret;
              }                 /* End of else chord with note(s) */
            //now output the postfix field of directives that have AFFIX set, which are not emitted 
            directives_insert_affix_postfix_editable (pchord->directives, &open_braces, &prevduration, iter, lily_for_obj, TARGET_CHORD, movement_count, measurenum, voice_count, objnum, 0);
          }                     /* End of outputting LilyPond for this chord because LILYPOND_OVERRIDE not set in a chord directive, ie !lily_override */
        else
          {
            GList *g = pchord->directives;
            gint num;
            for (num = 0; g; g = g->next, num++)
              {
                DenemoDirective *directive = (DenemoDirective *) g->data;
                if (directive->postfix && directive->postfix->len && !(directive->override & DENEMO_OVERRIDE_HIDDEN))
                  {
                    prevduration = -1;
                    open_braces += brace_count (directive->postfix->str);
                    insert_editable (&directive->postfix, directive->postfix->str, iter, gui, lily_for_obj, TARGET_CHORD, movement_count, measurenum, voice_count, objnum, num, 0);
                  }
              }
          }

        if (!lily_override)
          if ((pchord->is_grace & ENDGRACE) && *pgrace_status)
            {
              *pgrace_status = FALSE, g_string_append_printf (ret, "} ");
            }

        g_free (chord_prefix);
      }                         //end of case CHORD
      break;
    case CLEF:
      {
//                              no NAVANC ??? but one for keysig
        gboolean override = FALSE;
        gchar *clef_string = "";
        gchar *clef_prestring = "";
        GList *directives = ((clef *) curobj->object)->directives;
        if (directives)
          {
            override = get_lily_override (directives);
            clef_string = get_postfix (directives);
            clef_prestring = get_prefix (directives);
          }
        if (override)
          g_string_append_printf (ret, "%s", clef_string);
        else
          {
            if (!curobj->isinvisible)
              {
                determineclef (((clef *) curobj->object)->type, &clefname);
                g_string_append_printf (ret, "%s\\clef %s%s", clef_prestring, clefname, clef_string);
              }
          }
      }
      break;
    case KEYSIG:
      {
        gboolean override = FALSE;
        gchar *keysig_string = "";
        gchar *keysig_prestring = "";
        GList *directives = ((keysig *) curobj->object)->directives;
        if (directives)
          {
            override = get_lily_override (directives);
            keysig_string = get_postfix (directives);
            keysig_prestring = get_prefix (directives);
          }
        NAVANC (0, 0);          //this could indicate keysig type, although that is clear from the object at the cursor.
        if (override)
          g_string_append_printf (ret, "%s", keysig_string);
        else
          {
            determinekey (((keysig *) curobj->object)->isminor ? ((keysig *) curobj->object)->number + 3 : ((keysig *) curobj->object)->number, &keyname);
            g_string_append_printf (ret, "%s\\key %s", keysig_prestring, keyname);
            if (((keysig *) curobj->object)->isminor)
              g_string_append_printf (ret, " \\minor%s", keysig_string);
            else
              g_string_append_printf (ret, " \\major%s", keysig_string);
          }
      }
      break;
    case TIMESIG:
      {
        gboolean override = FALSE;
        gchar *timesig_string = "";
        gchar *timesig_prestring = "";

        GList *directives = ((timesig *) curobj->object)->directives;
        if (directives)
          {
            override = get_lily_override (directives);
            timesig_string = get_postfix (directives);
            timesig_prestring = get_prefix (directives);
          }
        if (override)
          g_string_append_printf (ret, "%s", timesig_string);
        else
          g_string_append_printf (ret, "%s\\time %d/%d%s", timesig_prestring, ((timesig *) curobj->object)->time1, ((timesig *) curobj->object)->time2, timesig_string);
      }
      cur_stime1 = ((timesig *) curobj->object)->time1;
      cur_stime2 = ((timesig *) curobj->object)->time2;
      break;
    case TUPOPEN:
      {
        gboolean override = FALSE;
        gchar *prestem_string = "";
        gchar *poststem_string = "";
        GList *directives = ((tuplet *) curobj->object)->directives;
        if (directives)
          {
            override = get_lily_override (directives);
            poststem_string = get_postfix (directives);
            prestem_string = get_prefix (directives);
          }

        NAVANC (0, 0);          // 0 means we don't need to say what the target is, it is the object itself
        if (override)
          g_string_append_printf (ret, "%s%s", prestem_string, poststem_string);
        else
          {
            g_string_append_printf (ret, "%s\\times %d/%d %s{", prestem_string, ((tupopen *) curobj->object)->numerator, ((tupopen *) curobj->object)->denominator, poststem_string);
            if (figures->len)
              g_string_append_printf (figures, " \\times %d/%d {", ((tupopen *) curobj->object)->numerator, ((tupopen *) curobj->object)->denominator);
            if (fakechords->len)
              g_string_append_printf (fakechords, " \\times %d/%d {", ((tupopen *) curobj->object)->numerator, ((tupopen *) curobj->object)->denominator);
          }
      }
      break;
    case TUPCLOSE:
      {
//      no NAVANC
        gboolean override = FALSE;
        gchar *prestem_string = "";
        gchar *poststem_string = "";
        GList *directives = ((tuplet *) curobj->object)->directives;
        if (directives)
          {
            override = get_lily_override (directives);
            poststem_string = get_postfix (directives);
            prestem_string = get_prefix (directives);
          }
        if (override)
          g_string_append_printf (ret, "%s%s", prestem_string, poststem_string);
        else
          {
            g_string_append_printf (ret, "%s}%s", prestem_string, poststem_string);
            if (figures->len)
              g_string_append_printf (figures, "}");
            if (fakechords->len)
              g_string_append_printf (fakechords, "}");
          }
      }
      break;
    case STEMDIRECTIVE:
      {
//no NAVANC 
        gboolean override = FALSE;
        gchar *prestem_string = "";
        gchar *poststem_string = "";
        GList *directives = ((stemdirective *) curobj->object)->directives;
        if (directives)
          {
            override = get_lily_override (directives);
            poststem_string = get_postfix (directives);
            prestem_string = get_prefix (directives);
          }
        if (override)
          g_string_append_printf (ret, "%s%s", prestem_string, poststem_string);
        else
          switch (((stemdirective *) curobj->object)->type)
            {
            case DENEMO_STEMDOWN:
              g_string_append_printf (ret, "%s\\stemDown" "%s", prestem_string, poststem_string);
              break;
            case DENEMO_STEMBOTH:
              g_string_append_printf (ret, "%s\\stemNeutral" "%s", prestem_string, poststem_string);
              break;
            case DENEMO_STEMUP:
              g_string_append_printf (ret, "%s\\stemUp" "%s", prestem_string, poststem_string);
              break;
            }
        if (*poststem_string)
          g_free (poststem_string);
        if (*prestem_string)
          g_free (prestem_string);
      }
      break;
    case LILYDIRECTIVE:
      ;                         //handled in the if block
      break;
    default:
      break;
    }

  outputret;

  g_free (curobj->lilypond);
  curobj->lilypond = g_string_free (lily_for_obj, FALSE);       //There is a scheme command d-GetLilyPondthat retrieves the LilyPond text associated with the current object
  *pprevduration = prevduration;
  *pprevnumdots = prevnumdots;

  *pclefname = clefname;
  *pkeyname = keyname;
  *pcur_stime1 = cur_stime1;
  *pcur_stime2 = cur_stime2;
  return open_braces;
}

/* create and insertion point and button for the next piece of music */
static void
insert_music_section (DenemoGUI * gui, gchar * name)
{
  GtkTextIter iter;
  gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, gtk_text_buffer_get_mark (Denemo.textbuffer, MUSIC));
  gtk_text_buffer_insert (Denemo.textbuffer, &iter, "\n", -1);
  (void) gtk_text_iter_backward_char (&iter);
  insert_section (NULL, name, name, &iter, gui);
}

/* create and insertion point and button for the next scoreblock */
static GtkTextChildAnchor *
insert_scoreblock_section (DenemoGUI * gui, gchar * name, DenemoScoreblock * sb)
{
  GString **target = sb ? &sb->lilypond : NULL;
  GtkTextChildAnchor *anchor;
  GtkTextIter iter;
  gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, gtk_text_buffer_get_mark (Denemo.textbuffer, SCOREBLOCK));
  gtk_text_buffer_insert (Denemo.textbuffer, &iter, "\n", -1);
  (void) gtk_text_iter_backward_char (&iter);
  if (sb && *target)
    {                           // custom scoreblock
      if ((*target)->len && *(*target)->str == '%')
        {
          gint maxlength = 50;
          gchar *v = g_malloc0 (maxlength * sizeof (gchar));
          strncpy (v, (*target)->str + 1, maxlength - 1);       //skip the opening % sign to make label, leave a NULL at the end
          while (--maxlength)
            if (*(v + maxlength) == '\n')
              *(v + maxlength) = 0;     //truncate at first end of line
          anchor = insert_section (target, name, v, &iter, gui);
          g_free (v);
        }
      else
        {
          anchor = insert_section (target, name, name, &iter, gui);
        }
      g_object_set_data (G_OBJECT (anchor), CUSTOM, (gpointer) sb);
    }
  else
    {                           //standard scoreblock
      anchor = insert_section (target, name, name, &iter, gui);
      g_object_set_data (G_OBJECT (anchor), STANDARD_SCOREBLOCK, (gpointer) 1);
    }
  return anchor;
}


/* gets the text for the section that starts at anchor */
static gchar *
get_text (GtkTextChildAnchor * anchor)
{
  GtkTextIter start, end;
  gtk_text_buffer_get_iter_at_child_anchor (Denemo.textbuffer, &start, anchor);
  GtkTextChildAnchor *endanc = g_object_get_data (G_OBJECT (anchor), "end");
  gtk_text_buffer_get_iter_at_child_anchor (Denemo.textbuffer, &end, endanc);
  return gtk_text_buffer_get_text (Denemo.textbuffer, &start, &end, FALSE /* get only visible text */ );
}

/**
 * Output the header information using Lilypond syntax
 * 
 * 
 */
static void
outputHeader (GString * str, DenemoGUI * gui)
{
  g_string_append_printf (str, "%s", _("%% LilyPond file generated by Denemo version "));
  g_string_append_printf (str, "%s", VERSION "\n\n");
  if (gui->lilycontrol.excerpt == TRUE)
    g_string_append_printf (str, "%s", "\\include \"lilypond-book-preamble.ly\" \n\n");

  g_string_append_printf (str, "%s", "%%http://www.gnu.org/software/denemo/\n\n");
  /*Print out lilypond syntax version */
  if (gui->lilycontrol.lilyversion->len)
    g_string_append_printf (str, "\\version \"%s\"\n", gui->lilycontrol.lilyversion->str /*LILYPOND_VERSION */ );
  else
    g_string_append_printf (str, "\\version \"%s\"\n", get_lily_version_string ());
}

gchar *
get_lilypond_paper (void)
{
  DenemoGUI *gui = Denemo.gui;
  GString *str = g_string_new ("");
  /* \paper block settings for excerpt */
  if (gui->lilycontrol.excerpt == TRUE)
    {
      g_string_append_printf (str, "%s", "print-all-headers = ##f\n");
      g_string_append_printf (str, "%s", TAB "#(define dump-extents #t)\n");
      g_string_append_printf (str, "%s", TAB "line-width = 160\\mm - 2.0 * 0.4\\in\n");
      g_string_append_printf (str, "%s", TAB "ragged-right = ##t\n");
      g_string_append_printf (str, "%s", TAB "indent = 0\\mm\n");
      g_string_append_printf (str, "%s", TAB "force-assignment = #\"\"\n");
      g_string_append_printf (str, "%s", TAB "line-width = #(- line-width (* mm  3.000000))\n");

    }
  /* \paper block settings for the score */
  else
    {

      gchar *paper_settings = get_postfix (gui->paper.directives);
      if (paper_settings)
        {
          g_string_append (str, paper_settings);
          g_free (paper_settings);
        }
      else
        g_string_append_printf (str, "%s", "print-all-headers = ##f\n");
    }
  return g_string_free (str, FALSE);
}


const gchar *
get_prevailing_clef_as_lilypond (void)
{
  clef *theclef = get_prevailing_context (CLEF);
  const gchar *clefname = get_lilypond_for_clef (theclef);
  return clefname;
}

const gchar *
get_prevailing_keysig_as_lilypond (void)
{
  keysig *thekeysig = get_prevailing_context (KEYSIG);
  const gchar *keysigname = get_lilypond_for_keysig (thekeysig);
  return keysigname;
}

const gchar *
get_prevailing_timesig_as_lilypond (void)
{
  timesig *thetimesig = get_prevailing_context (TIMESIG);
  const gchar *timesigname = get_lilypond_for_timesig (thetimesig);
  return timesigname;
}



gchar *
get_lilypond_for_timesig (timesig * time)
{
  gboolean override = get_lily_override (time->directives);
  gchar *timesig_string = get_postfix (time->directives);
  gchar *time_prefix = get_prefix (time->directives);
  if (override){
    g_free (time_prefix);
    return timesig_string;
  }

  gchar *ret = g_strdup_printf ("{%s \\time %d/%d %s}\n", time_prefix, time->time1,
                                time->time2, timesig_string);
  g_free (timesig_string);
  g_free (time_prefix);
  return ret;
}


gchar *
get_lilypond_for_keysig (struct keysig * key)
{
  gchar *keyname;
  gboolean override = get_lily_override (key->directives);
  gchar *keysig_string = get_postfix (key->directives);
  gchar *key_prefix = get_prefix (key->directives);
  if (override){
    g_free (key_prefix);
    return keysig_string;
  }
  determinekey (key->isminor ? key->number + 3 : key->number, &keyname);
  gchar *ret = g_strdup_printf ("{%s \\key %s%s\n%s", key_prefix, keyname, (key->isminor) ? " \\minor}" : " \\major}", keysig_string);
  g_free (keysig_string);
  g_free (key_prefix);
  return ret;

}

void
do_clef (GString * definitions, DenemoStaff * curstaffstruct, gchar * movement, gchar * voice)
{
  gchar *clefname;
  determineclef (curstaffstruct->clef.type, &clefname);
  gboolean clef_override = get_lily_override (curstaffstruct->clef.directives);
  gchar *clef_postfix_insert = get_postfix (curstaffstruct->clef.directives);
  gchar *clef_prefix = get_prefix (curstaffstruct->clef.directives);
  if (clef_override)
    g_string_append_printf (definitions, "%s%sClef = %s\n", movement, voice, clef_postfix_insert);
  else
    g_string_append_printf (definitions, "%s%sClef = %s \\clef %s %s\n", movement, voice, clef_prefix, clefname, clef_postfix_insert);
  g_free (clef_postfix_insert);
  g_free (clef_prefix);
}

gchar *
get_lilypond_for_clef (clef * theclef)
{
  gchar *clefname = NULL;
  determineclef (theclef->type, &clefname);
  gboolean clef_override = get_lily_override (theclef->directives);
  gchar *clef_postfix_insert = get_postfix (theclef->directives);
  gchar *clef_prefix = get_prefix (theclef->directives);
  if (clef_override){
    g_free (clef_prefix);
    return clef_postfix_insert;

  }
  gchar *ret = g_strdup_printf ("%s \\clef %s %s\n", clef_prefix, clefname, clef_postfix_insert);
  g_free (clef_postfix_insert);
  g_free (clef_prefix);
  return ret;
}






/**
 * Output a Denemo Staff in Lilypond syntax
 * A section is created in the Denemo.textbuffer and the music inserted into it.
 * each DenemoObject is given an anchor and a pointer to the object is stored with the anchor,
 * so that it will be possible to create LilyPond directives from within the buffer (not yet
 * implemented FIXME).
 * Any lyrics, chord symbols and figured basses are put in separate sections.
 * 
 */
static void
outputStaff (DenemoGUI * gui, DenemoStaff * curstaffstruct, gint start, gint end, gchar * movement, gchar * voice, gint movement_count, gint voice_count, DenemoScoreblock * sb)
{
  gint cur_stime1 = curstaffstruct->timesig.time1;
  gint cur_stime2 = curstaffstruct->timesig.time2;

  gint prevduration, prevnumdots;
  gchar *clefname;
  /* clef name */
  gchar *keyname;
  /* key signature name */
  measurenode *curmeasure;
  objnode *curobjnode;
  DenemoObject *curobj;
  gint curmeasurenum;           // count of measures printed
  gint measurenum;              //count of measures from start of staff starting at 1
  gint objnum;                  //count of objects in measure starting at 1
  gint open_braces;             //Keep track of the number of open brace "{" chars in the music, in case of imbalance.
  GString *staff_str = g_string_new ("");       //Bits of the music of the staff are accumulated here and then stored in the lilypond view buffer
  GList *lyrics = NULL;
  GString *figures = g_string_new ("");
  GString *fakechords = g_string_new ("");
  prevduration = -1;
  prevnumdots = -1;
  gint grace_status = 0;
  GtkTextIter iter;
  GtkTextMark *curmark;         /* movable mark for insertion point of the music of the staff */
  /* a button and mark for the music of this staff */


  GString *voice_name = g_string_new (movement);
  g_string_prepend (voice_name, "Notes for ");
  g_string_append_printf (voice_name, " Voice %d", /* ABS */ (voice_count));
  //g_print("making %s\n", voice_name->str);
  insert_music_section (gui, voice_name->str);
  gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, gtk_text_buffer_get_mark (Denemo.textbuffer, voice_name->str));
  curmark = gtk_text_buffer_create_mark (Denemo.textbuffer, NULL, &iter, FALSE);        //FIXME remove this mark at the end of the output of this staff...

  /* a button and mark for the lyrics of this staff */
  GString *lyrics_name = g_string_new (movement);
  if (curstaffstruct->verses)
    {
      g_string_prepend (lyrics_name, "Lyrics for ");
      g_string_append_printf (lyrics_name, " Voice %d", voice_count);
      insert_music_section (gui, lyrics_name->str);
      GList *g;
      for (g = curstaffstruct->verses; g; g = g->next)
        {
          lyrics = g_list_append (lyrics, get_text_from_view (g->data));
        }
    }

  /* a button and mark for the figures of this staff */
  GString *figures_name = g_string_new (movement);
  if (curstaffstruct->hasfigures)
    {
      g_string_prepend (figures_name, "Figured Bass for ");
      g_string_append_printf (figures_name, " Voice %d", voice_count);
      insert_music_section (gui, figures_name->str);
      g_string_append (figures, "%figures follow\n\\set Staff.implicitBassFigures = #'(0)\n");
    }
  /* a button and mark for the chord symbols of this staff */
  GString *fakechords_name = g_string_new (movement);
  if (curstaffstruct->hasfakechords)
    {
      g_string_prepend (fakechords_name, "Chord symbols for ");
      g_string_append_printf (fakechords_name, " Voice %d", voice_count);
      insert_music_section (gui, fakechords_name->str);
      g_string_append (fakechords, "%chord symbols follow\n");
    }

  gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, curmark);

  {                             /* standard staff-prolog */
    /* Determine the key signature */

    gchar *keyname;
    /* key signature name */

    determinekey (curstaffstruct->keysig.isminor ? curstaffstruct->keysig.number + 3 : curstaffstruct->keysig.number, &keyname);
    gchar *clefname;

    /* Determine the clef */

    determineclef (curstaffstruct->clef.type, &clefname);


    g_string_append_printf (staff_str, "%s%s = {\n", movement, voice);

    gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, curmark);
    gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, staff_str->str, -1, INEDITABLE, NULL);
  }                             /*end standard staff-prolog */

  g_string_assign (staff_str, "");

  curmeasurenum = 0;
  curmeasure = curstaffstruct->measures;
  if (!end)
    end = g_list_length (curmeasure);
  /* Now each measure */
  if (start)
    curmeasure = g_list_nth (curmeasure, start - 1);
  open_braces = 0;              //keep track of excess open braces "{"
  gboolean nonprintingnotes = FALSE;    //no nonprinting notes found yet in this voice, once there are issue a cross-head directive
  for (measurenum = MAX (start, 1); curmeasure && measurenum <= end; curmeasure = curmeasure->next, measurenum++)
    {

      gboolean empty_measure = TRUE;


      if ((++curmeasurenum % 5) == 0)
        {
          g_string_append_printf (staff_str, "%%%d\n", curmeasurenum);
          if (figures->len)
            g_string_append_printf (figures, "\n%%%d\n", curmeasurenum);
          if (fakechords->len)
            g_string_append_printf (fakechords, "\n%%%d\n", curmeasurenum);
        }
      g_string_append_printf (staff_str, "%s", TAB);
      gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, curmark);
      gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, staff_str->str, -1, INEDITABLE, NULL);
      g_string_assign (staff_str, "");
      gint firstobj = 1, lastobj = G_MAXINT - 1;
      if (start && gui->si->markstaffnum)
        {                       //markstaffnum==0 means not set
          firstobj = 1 + gui->si->selection.firstobjmarked;
          lastobj = 1 + gui->si->selection.lastobjmarked;
        }
      //g_print("First last, %d %d %d\n", firstobj, lastobj, start);
      for (objnum = 1, curobjnode = (objnode *) curmeasure->data; /* curobjnode NULL checked at end */ ;
           curobjnode = curobjnode->next, objnum++)
        {
          curobj = NULL;        //avoid random values for debugabililty
          if ((measurenum > MAX (start, 1) && (measurenum < end)) || (start == end && measurenum == start && objnum >= firstobj && objnum <= lastobj) || (start != end && ((((measurenum == MAX (start, 1)) && (objnum >= firstobj))) || ((measurenum == end) && (objnum <= lastobj)))))
            {

              if (curobjnode)
                {
                  curobj = (DenemoObject *) curobjnode->data;
                  // if (curobj->type==CHORD||curobj->type==PARTIAL||curobj->type==LILYDIRECTIVE)
                  if (curobj->durinticks || curobj->type == LILYDIRECTIVE)
                    empty_measure = FALSE;
                  //Print rhythm notes with cross head. We ignore the case where someone reverts to real notes after rhythm only notes
                  if (curobj->type == CHORD && ((chord *) curobj->object)->notes && curobj->isinvisible && !nonprintingnotes)
                    {
                      gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, curmark);
                      gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, "\n" TAB "\\override NoteHead #'style = #'cross" "\n\\override NoteHead #'color = #darkyellow" "\n\\override Stem #'color = #darkyellow" "\n\\override Flag #'color = #darkyellow" "\n\\override Beam #'color = #darkyellow ", -1, INEDITABLE, NULL);
                      nonprintingnotes = TRUE;
                    }

                  if (curobj->type == LILYDIRECTIVE)
                    {
                      DenemoDirective *directive = ((lilydirective *) curobj->object);
#define OUTPUT_LILY(what) \
  if(directive->what && directive->what->len && ((directive->x == 0 || (directive->x!=sb->id))) && ((directive->y == 0 || (directive->y==sb->id))) \
     && (!(directive->override & DENEMO_OVERRIDE_HIDDEN)) \
     ) {								\
	  gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, curmark);\
	  GtkTextChildAnchor *objanc = gtk_text_buffer_create_child_anchor (Denemo.textbuffer, &iter);\
	  g_object_set_data(G_OBJECT(objanc), OBJECTNODE, (gpointer)curobjnode);\
	  g_object_set_data(G_OBJECT(objanc), MOVEMENTNUM, (gpointer)(intptr_t)ABS(movement_count));\
	  g_object_set_data(G_OBJECT(objanc), MEASURENUM, (gpointer)(intptr_t)measurenum);\
	  g_object_set_data(G_OBJECT(objanc), STAFFNUM, (gpointer)(intptr_t)ABS(voice_count));\
	  g_object_set_data(G_OBJECT(objanc), OBJECTNUM, (gpointer)(intptr_t)ABS(objnum));\
	  g_object_set_data(G_OBJECT(objanc), TARGETTYPE, (gpointer)(intptr_t)ABS(TARGET_OBJECT));\
	  GtkTextIter back;\
	  back = iter;\
	  (void)gtk_text_iter_backward_char(&back);\
	  gtk_text_buffer_apply_tag_by_name(Denemo.textbuffer, INEDITABLE, &back, &iter);\
	  gtk_text_buffer_apply_tag_by_name(Denemo.textbuffer, "system_invisible", &back, &iter);\
    open_braces += brace_count( ((lilydirective *) curobj->object)->what->str);\
    gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter,  ((lilydirective *) curobj->object)->what->str, -1, "bold", NULL); \
    g_free(curobj->lilypond);\
    curobj->lilypond = g_strdup(((lilydirective *) curobj->object)->what->str);\
    GtkTextChildAnchor *endanc  = gtk_text_buffer_create_child_anchor (Denemo.textbuffer, &iter);\
    back = iter;\
    (void)gtk_text_iter_backward_char(&back);\
    gtk_text_buffer_apply_tag_by_name(Denemo.textbuffer, INEDITABLE, &back, &iter);\
    gtk_text_buffer_apply_tag_by_name(Denemo.textbuffer, "system_invisible", &back, &iter);\
    g_object_set_data(G_OBJECT(objanc), "end", (gpointer)endanc);\
    g_object_set_data(G_OBJECT(objanc), GSTRINGP, (gpointer)&((lilydirective *) curobj->object)->what);\
    gui->anchors = g_list_prepend(gui->anchors, objanc);\
  }


                      OUTPUT_LILY (prefix);
                      gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, " ", -1, INEDITABLE, HIGHLIGHT, NULL);
                      OUTPUT_LILY (postfix);

#undef OUTPUT_LILY



                      prevduration = -1;
                      prevnumdots = -1; // the LILYDIRECTIVE may have changed the duration
                    }
                  else
                    {

#if 0
                      place_navigation_anchor (curmark, (gpointer) curobjnode, ABS (movement_count), measurenum, ABS (voice_count), ABS (objnum), 0, 0);
#endif



                      open_braces += generate_lily_for_obj (gui, &iter, curobj, &prevduration, &prevnumdots, &clefname, &keyname, &cur_stime1, &cur_stime2, &grace_status, figures, fakechords, curmark, (gpointer) curobjnode, ABS (movement_count), measurenum, ABS (voice_count), ABS (objnum));
                    }           // end not lilydirective



                }               // if curobjnode

              if ((curobjnode == NULL) || (curobjnode->next == NULL))
                {               //at end of measure
                  GString *endstr = g_string_new ("");
                  if (empty_measure)    // measure has nothing to use up the duration, assume  SKIP 
                    {
                      g_string_append_printf (endstr, " s1*%d/%d ", cur_stime1, cur_stime2);
                      gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, curmark);
                      gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, endstr->str, -1, NULL, NULL);
                      g_string_assign (endstr, "");
                      prevduration = -1;
                    }
                  if (figures->len)
                    g_string_append (figures, "\n");
                  if (fakechords->len)
                    g_string_append (fakechords, "\n");

                  if ((curobjnode != NULL) && ((curobj == NULL) || curobj->type != LILYDIRECTIVE))      /* if it ends in a lilydirective, the user may want to choose their own barline style, let them */
                    {
                      if (curmeasure->next)
                        g_string_append_printf (endstr, "%s", "\\AutoBarline\n");
                      else
                        g_string_append_printf (endstr, "%s", " \\AutoEndMovementBarline\n");
                    }

                  gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, curmark);
                  gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, endstr->str, -1, INEDITABLE, NULL);
                }               //if end of measure

              if (curobjnode)
                {
                  curobj = (DenemoObject *) curobjnode->data;
                  /*  figures and chord symbols */
                  if (curobj->type == CHORD)
                    {
                      chord *pchord = (chord *) curobj->object;


                      if (curstaffstruct->hasfigures)
                        output_figured_bass (figures, pchord);

                      if (curstaffstruct->hasfakechords)
                        output_fakechord (fakechords, pchord);
                      if ((pchord->is_grace & ENDGRACE))
                        {
                          if (figures->len)
                            g_string_append_printf (figures, "}");
                          if (fakechords->len)
                            g_string_append_printf (fakechords, "}");
                        }

                      /* end of figures and chord symbols */
                    }           // if CHORD
                }               // if object
            }                   //in obj range
          if (curobjnode == NULL || curobjnode->next == NULL)
            break;              //we want to go through once for empty measures
        }                       /* For each object in the measure */
    }                           /* for each measure */


  for (; open_braces > 0; open_braces--)
    {
      g_string_append_printf (staff_str, "%s", "\n} %% missing close brace\n");
    }
  g_string_append_printf (staff_str, "%s", "}\n");
  gchar *voice_prolog_insert = get_postfix (curstaffstruct->voice_directives);



  g_free (voice_prolog_insert);
  gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, curmark);

  gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, staff_str->str, -1, INEDITABLE, NULL);


  if (lyrics)
    {
      GList *g;
      gint versenum;
      for (versenum = 1, g = lyrics; g; g = g->next, versenum++)
        {
          GString *versename = g_string_new ("");
          GString *temp = g_string_new ("");
          g_string_printf (temp, "Verse%d", versenum);
          set_lily_name (temp, versename);
          gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, gtk_text_buffer_get_mark (Denemo.textbuffer, lyrics_name->str));
          g_string_printf (temp, "%s%sLyrics%s = \\lyricmode { \n", movement, voice, versename->str);
          g_string_append_printf (temp, "%s \n}\n", (char *) g->data);
          gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, temp->str, -1, INEDITABLE, NULL);
          g_string_free (temp, TRUE);
          g_string_free (versename, TRUE);
          g_free (g->data);
        }
    }
  g_string_free (lyrics_name, TRUE);

  if (figures->len)
    {
      GString *temp = g_string_new ("");
      gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, gtk_text_buffer_get_mark (Denemo.textbuffer, figures_name->str));
      /* output figures prolog */

      g_string_printf (temp, "%s%sBassFiguresLine = \\figuremode {\n" "\\set figuredBassAlterationDirection = #1\n" "\\set figuredBassPlusDirection = #1\n" "\\override FiguredBass.BassFigure " "#'font-size = #-1\n", movement, voice);

      gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, temp->str, -1, INEDITABLE, NULL);

      g_string_printf (temp, "%s \n}\n", figures->str);
      gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, temp->str, -1, INEDITABLE, NULL);
      g_string_free (temp, TRUE);
    }
  g_string_free (figures_name, TRUE);

  if (fakechords->len)
    {
      GString *temp = g_string_new ("");
      gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, gtk_text_buffer_get_mark (Denemo.textbuffer, fakechords_name->str));
      /* output fakechords prolog */

      g_string_append_printf (temp, "%s%sChords = \\new ChordNames \\chordmode {\n", movement, voice);

      gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, temp->str, -1, INEDITABLE, NULL);

      g_string_printf (temp, "%s \n}\n" /* another definition here */ , fakechords->str);
      gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, temp->str, -1, INEDITABLE, NULL);
      g_string_free (temp, TRUE);
    }
  g_string_free (fakechords_name, TRUE);

  g_string_free (staff_str, TRUE);
  //g_string_free(lyrics, TRUE);
  g_string_free (figures, TRUE);
  g_string_free (fakechords, TRUE);
}                               /* outputStaff */

/* Merge back any modified LilyPond text into the Denemo Score */
void
merge_lily_strings (DenemoGUI * gui)
{
  //g_print("Merge...\n");
  GList *g;
  if (gui == Denemo.gui)
    write_status (gui);
  if (!gtk_text_buffer_get_modified (Denemo.textbuffer))
    {
      // g_print("not modified\n");
      return;
    }
  if (gui->lilysync != gui->changecount)
    {
      warningdialog (_("The score has been altered so that this LilyPond text is out of date - ignoring request"));
      return;
    }
  for (g = gui->anchors; g; g = g->next)
    {
      GtkTextChildAnchor *anchor = g->data;
      GString **gstringp = g_object_get_data (G_OBJECT (anchor), GSTRINGP);
      if (gstringp)
        {
          gchar *lily = get_text (anchor);
          if (strcmp (lily, g_object_get_data (G_OBJECT (anchor), ORIGINAL)))
            {
              //      g_print("Compare %s\nwith %s for gstringp %p\n", lily, g_object_get_data(anchor,ORIGINAL), *gstringp);
              if (!*gstringp)
                *gstringp = g_string_new (lily);
              else
                g_string_assign (*gstringp, lily);


              //this does not prevent corruption!!!!! on deleting all the string...
              /* white space becomes empty string */
              g_strstrip (lily);
              if (*lily == '\0')
                {
                  g_string_free (*gstringp, TRUE);
                  *gstringp = g_string_new ("");
                }
#if 0
              g_print ("gstringp %p at %p holds %s\n", *gstringp, gstringp, (*gstringp)->str);
#endif
              /* this is    ((DenemoDirective*)((DenemoObject*)(Denemo.gui->si->currentobject->data))->object)->postfix */
              g_free (g_object_get_data (G_OBJECT (anchor), ORIGINAL));
              g_object_set_data (G_OBJECT (anchor), ORIGINAL, get_text (anchor));


              score_status (gui, TRUE);
              g_free (lily);
            }

        }

    }
  gtk_text_buffer_set_modified (Denemo.textbuffer, FALSE);
}
/* UNUSED
void
merge_lily_cb (GtkAction * action, DenemoGUI * gui)
{
  merge_lily_strings (gui);
}
*/
/* if there is not yet a textbuffer for the passed gui, it creates and populates one,
   if there is it finds the offset of the current point in the buffer, refreshes it from 
   the Denemo data and then repositions the cursor at that offset. The refresh is subject to
   conditions (see output_score_to_buffer()).
*/
void
refresh_lily_cb (GtkAction * action, DenemoGUI * gui)
{
  if (Denemo.textbuffer)
    {
      GtkTextIter iter;
      GtkTextMark *cursor = gtk_text_buffer_get_insert (Denemo.textbuffer);
      gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, cursor);
      gint offset = gtk_text_iter_get_offset (&iter);
      //  g_print("Offset %d for %p\n", offset, Denemo.textbuffer);
      output_score_to_buffer (gui, TRUE, NULL);
      gtk_text_buffer_get_iter_at_offset (Denemo.textbuffer, &iter, offset);
      gtk_text_buffer_place_cursor (Denemo.textbuffer, &iter);
      // gtk_text_view_scroll_mark_onscreen(GTK_TEXT_VIEW(Denemo.textview), gtk_text_buffer_get_insert(Denemo.textbuffer));
      gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (Denemo.textview), gtk_text_buffer_get_insert (Denemo.textbuffer), 0.0, TRUE, 0.5, 0.5);
    }
  else
    output_score_to_buffer (gui, TRUE, NULL);
}

void
force_lily_refresh (DenemoGUI * gui)
{
  gui->lilysync = G_MAXUINT;
  refresh_lily_cb (NULL, gui);
}

void
delete_lily_cb (GtkAction * action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  GtkTextChildAnchor *anchor = gui->lilystart;
  GtkTextIter start, end;
  gtk_text_buffer_get_iter_at_child_anchor (Denemo.textbuffer, &start, anchor);
  GtkTextChildAnchor *endanc = g_object_get_data (G_OBJECT (anchor), "end");
  gtk_text_buffer_get_iter_at_child_anchor (Denemo.textbuffer, &end, endanc);
  gpointer sb = g_object_get_data (G_OBJECT (anchor), CUSTOM);
  gui->anchors = g_list_remove (gui->anchors, anchor);
  gui->custom_scoreblocks = g_list_remove (gui->custom_scoreblocks, sb);


  gtk_text_buffer_delete (Denemo.textbuffer, &start, &end);
}


void
toggle_lily_visible_cb (GtkAction * action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  GtkTextIter start, end;
  GtkTextChildAnchor *anchor = gui->lilystart;
  DenemoScoreblock *sb = g_object_get_data (G_OBJECT (anchor), CUSTOM);
  gtk_text_buffer_get_iter_at_child_anchor (Denemo.textbuffer, &start, anchor);
  (void) gtk_text_iter_forward_char (&start);
  GtkTextChildAnchor *endanc = g_object_get_data (G_OBJECT (anchor), "end");

  gtk_text_buffer_get_iter_at_child_anchor (Denemo.textbuffer, &end, endanc);

  GtkTextTag *tag = gtk_text_tag_table_lookup (tagtable, "invisible");
  /*   GtkTextTag *systemtag = gtk_text_tag_table_lookup(tagtable, "system_invisible"); */
  if (gtk_text_iter_has_tag (&start, tag))
    {
      if (sb)
        sb->visible = TRUE;
      gtk_text_buffer_remove_tag_by_name (Denemo.textbuffer, "invisible", &start, &end);
    }
  else
    {
      if (sb)
        sb->visible = FALSE;
      gtk_text_buffer_apply_tag_by_name (Denemo.textbuffer, "invisible", &start, &end);
    }
  //g_print("visible %d\n", sb?sb->visible:-1);
}

static void
place_cursor_cb (GtkAction * action, DenemoGUI * gui)
{
  /* place cursor on current object */
  if (gui->si->currentobject)
    {
      DenemoObject *targetobj = gui->si->currentobject->data;
      GList *curobjnode;
      GtkTextIter iter;
      gtk_text_buffer_get_start_iter (Denemo.textbuffer, &iter);
      while (gtk_text_iter_forward_char (&iter))
        {
          GtkTextChildAnchor *anchor;
          anchor = gtk_text_iter_get_child_anchor (&iter);
          if (anchor && (curobjnode = g_object_get_data (G_OBJECT (anchor), OBJECTNODE)) && curobjnode->data == targetobj)
            {
              gtk_text_buffer_place_cursor (Denemo.textbuffer, &iter);
              gtk_text_view_scroll_mark_onscreen (GTK_TEXT_VIEW (Denemo.textview), gtk_text_buffer_get_insert (Denemo.textbuffer));
              gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (Denemo.textview), gtk_text_buffer_get_insert (Denemo.textbuffer), 0.0, TRUE, 0.5, 0.5);
              //g_print("placed cursor\n"); FIXME as well color in relevant objects
            }
        }
    }
}

#if 0
static void
print_cursor_cb (void)
{
  GtkTextIter iter;
  DenemoGUI *gui = Denemo.gui;
  gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, gtk_text_buffer_get_insert (Denemo.textbuffer));
  g_print ("Char is %c at bytes=%d chars=%d\n", gtk_text_iter_get_char (&iter), gtk_text_iter_get_visible_line_index (&iter), gtk_text_iter_get_visible_line_offset (&iter));

}
#endif

void
set_initiate_scoreblock (DenemoScore * si, GString * scoreblock)
{
  gchar *movement_prolog = get_postfix (si->movementcontrol.directives);
  g_string_append_printf (scoreblock, "%s", get_lily_override (si->movementcontrol.directives) ? movement_prolog : " <<\n");
  g_free (movement_prolog);
}


void
set_staff_definition (GString * str, DenemoStaff * curstaffstruct, gchar * denemo_name)
{
  gint staff_override = get_lily_override (curstaffstruct->staff_directives);

  gchar *staff_prolog_insert = get_prefix (curstaffstruct->staff_directives);
  gchar *staff_epilog_insert = get_postfix (curstaffstruct->staff_directives);
  if (staff_override)
    {
      g_string_append_printf (str, "%s%s", staff_prolog_insert, staff_epilog_insert);
    }
  else
    {
      g_string_append_printf (str, "\n%%Start of Staff\n\\new Staff = \"%s\" %s << %s\n", denemo_name, staff_prolog_insert, staff_epilog_insert);
    }
}

void
set_voice_definition (GString * str, DenemoStaff * curstaffstruct, gchar * denemo_name)
{
  gint voice_override = get_lily_override (curstaffstruct->voice_directives);

  gchar *voice_prolog_insert = get_prefix (curstaffstruct->voice_directives);
  gchar *voice_epilog_insert = get_postfix (curstaffstruct->voice_directives);
  if (voice_override)
    {
      g_string_append_printf (str, "%s", voice_prolog_insert);
    }
  else
    {
      g_string_append_printf (str, "\\new Voice = \"%s\" %s { %s\n", denemo_name, voice_prolog_insert, voice_epilog_insert);
    }
}

void
set_voice_termination (GString * str, DenemoStaff * curstaffstruct)
{
  gint voice_override = get_lily_override (curstaffstruct->voice_directives);
  gchar *voice_epilog_insert = get_postfix (curstaffstruct->voice_directives);
  if (voice_override)
    {
      g_string_append_printf (str, "%s", voice_epilog_insert);
    }
  else
    {
      g_string_assign (str, TAB TAB "} %End of voice\n");
    }
}

void
set_staff_termination (GString * str, DenemoStaff * curstaffstruct)
{
  gint staff_override = (DENEMO_OVERRIDE_LILYPOND | DENEMO_OVERRIDE_AFFIX) == (get_override (curstaffstruct->staff_directives) & (DENEMO_OVERRIDE_LILYPOND | DENEMO_OVERRIDE_AFFIX));
  gchar *staff_epilog_insert = get_postfix (curstaffstruct->staff_directives);
  if (staff_override)
    {
      g_string_append_printf (str, "%s", staff_epilog_insert);
    }
  else
    {
      g_string_assign (str, TAB TAB "\n>>\n%End of Staff\n");
    }
}

void
generate_lilypond_part (void)
{
  output_score_to_buffer (Denemo.gui, TRUE, ((DenemoStaff *) (Denemo.gui->si->currentstaff->data))->lily_name->str);
}

/*
 *writes the current score in LilyPond format to the textbuffer.
 *sets gui->lilysync equal to gui->changecount
 *if gui->lilysync is up to date with changecount on entry does nothing unless
 *the set of score blocks will be different from the last call
 * this namespec is not otherwise used FIXME
 */


static void
output_score_to_buffer (DenemoGUI * gui, gboolean all_movements, gchar * partname)
{

  GString *definitions = g_string_new ("");
  GString *staffdefinitions = g_string_new ("");

  if (gui->namespec == NULL)
    gui->namespec = g_strdup ("");      /* to check if the scoreblocks to make visible are different */

  gchar *namespec;
  gchar *movementname;
  if (all_movements)
    movementname = g_strdup ("All movements");
  else
    movementname = g_strdup_printf ("Movement %d", 1 + g_list_index (gui->movements, gui->si));
  if (partname)
    namespec = g_strdup_printf ("%s Part %s", movementname, partname);
  else
    namespec = g_strdup_printf ("%s all parts", movementname);
  g_free (movementname);
  //if(Denemo.textview==NULL)
  //  create_lilywindow();


  DenemoScoreblock *sb = select_layout (all_movements, partname);       //FIXME gui->namespec mechanism is probably redundant, and could well cause trouble...

  if (gui->si->markstaffnum)
    all_movements = FALSE;

  staffnode *curstaff;
  DenemoStaff *curstaffstruct;
//  if(Denemo.gui->custom_scoreblocks==NULL)
  //   create_default_scoreblock();
  if ((gui->si->markstaffnum == 0) && Denemo.textbuffer && (gui->changecount == gui->lilysync) && !strcmp (gui->namespec, namespec))
    {
      g_free (gui->namespec);
      gui->namespec = namespec;
      //g_print("changecount=%d and lilysync= %d\n", gui->changecount, gui->lilysync);
      return;
    }
  g_free (gui->namespec);
  gui->namespec = namespec;
  //g_print("actually refreshing %d %d", gui->lilysync, gui->changecount);
  gui->lilysync = gui->changecount;
  if (Denemo.textbuffer)
    gtk_text_buffer_set_text (Denemo.textbuffer, "", -1);
  else
    warningdialog (_("No textbuffer"));
  if (gui->anchors)
    {
      //FIXME  the working curmark at the end of the creation of the text
      g_list_free (gui->anchors);
      gui->anchors = NULL;
    }



  /* divide up the buffer for the various parts of the lily file */
  GtkTextIter iter;

  gtk_text_buffer_get_start_iter (Denemo.textbuffer, &iter);

  insert_section (NULL, START, "Prolog", &iter, gui);

  gtk_text_buffer_get_end_iter (Denemo.textbuffer, &iter);

  insert_section (NULL, MUSIC, NULL, &iter, gui);
  gtk_text_buffer_get_end_iter (Denemo.textbuffer, &iter);

  insert_section (NULL, SCOREBLOCK, NULL, &iter, gui);

  gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, gtk_text_buffer_get_mark (Denemo.textbuffer, START));
  gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, "\n", -1, "bold", NULL);


  {                             //no custom prolog

    GString *header = g_string_new ("");
    outputHeader (header, gui);
    gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, header->str, -1, INEDITABLE, NULL);
    g_string_free (header, TRUE);

  }                             //end of standard prolog

  {                             //Score prefix
//    !!used in DenemoBar command (set barlines literally) along with postfix.
//    change this script to have DENEMO_OVERRIDE_AFFIX set and then move all others to the score layout section

    //Default value for barline = barline check
    gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, "\nAutoBarline = {}\nAutoEndMovementBarline = \\bar \"|.\"\n", -1, INEDITABLE, NULL, NULL);
    GList *g = gui->lilycontrol.directives;
    /* num is not needed, as at the moment we can never get this location from LilyPond */
    for (; g; g = g->next)
      {
        DenemoDirective *directive = g->data;
        if (directive->prefix && (directive->override & (DENEMO_OVERRIDE_AFFIX)))       //This used to be (mistakenly) DENEMO_ALT_OVERRIDE
          insert_editable (&directive->prefix, directive->prefix->str, &iter, gui, NULL, TARGET_OBJECT, 0, 0, 0, 0, 0, 0);
        //insert_section(&directive->prefix, directive->tag->str, NULL, &iter, gui);
      }
  }

  gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, "\n% The music follows\n", -1, INEDITABLE, NULL);

  gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, gtk_text_buffer_get_mark (Denemo.textbuffer, SCOREBLOCK));
  gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, "% The scoreblocks follow\n", -1, "bold", "system_invisible", NULL);

  /* output scoreblock */
  {
    gchar *scoreblock_tag;
#ifdef USE_EVINCE  
    if (continuous_typesetting ())
      scoreblock_tag = "temporary scoreblock";
    else
      scoreblock_tag = "standard scoreblock";
#else
      scoreblock_tag = "standard scoreblock";
#endif
    insert_scoreblock_section (gui, scoreblock_tag, sb);
    gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, gtk_text_buffer_get_mark (Denemo.textbuffer, scoreblock_tag));
    if (sb->text_only)
      insert_editable (&sb->lilypond, (sb->lilypond)->str, &iter, gui, 0, 0, 0, 0, 0, 0, 0, 0);
    else
      gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, (sb->lilypond)->str, -1, INEDITABLE, NULL);
  }
  /* insert standard scoreblock section */
  //insert_scoreblock_section(gui, STANDARD_SCOREBLOCK, NULL);

  GList *g;
  gint movement_count;
  gint visible_movement;        /* 1 for visible -1 for invisible */
  for (g = gui->movements, movement_count = 1; g; g = g->next, movement_count++)
    {
      DenemoScore *si = g->data;
      gint voice_count;         //which voice counting from 1st voice of 1st staff thru to last voice of last staff.
      gint staff_count;         //which staff (not counting voices)
      visible_movement = (((all_movements) || (g->data == gui->si)) ? 1 : -1);
      GString *movement_name = g_string_new ("");
      GString *name = g_string_new ("");
      g_string_printf (name, "Mvmnt%d", movement_count);
      set_lily_name (name, movement_name);
      g_string_free (name, TRUE);
      //context = FALSE;



      for (curstaff = si->thescore, voice_count = 1, staff_count = 0; curstaff; curstaff = curstaff->next, voice_count++)
        {
          gint visible_part = 1;        /* 1 for visible -1 for invisible */
          curstaffstruct = (DenemoStaff *) curstaff->data;
          GString *voice_name = g_string_new ("");
          GString *staff_name = g_string_new ("");
          GString *name = g_string_new ("");
          if (curstaffstruct->voicecontrol == DENEMO_PRIMARY)
            staff_count++;

          g_string_printf (name, "Voice%d", voice_count);
          set_lily_name (name, voice_name);

          g_string_printf (name, "Staff%d", staff_count);
          set_lily_name (name, staff_name);

          g_string_free (name, TRUE);
          gint start = 0, end = 0;
          if (gui->si->markstaffnum)
            {
              if (!(voice_count >= gui->si->selection.firststaffmarked && voice_count <= gui->si->selection.laststaffmarked))
                visible_part = -1;
              start = gui->si->selection.firstmeasuremarked;
              end = gui->si->selection.lastmeasuremarked;
            }
          if (visible_part > 0 && visible_movement > 0)
            outputStaff (gui, curstaffstruct, start, end, movement_name->str, voice_name->str, movement_count * visible_movement, voice_count * visible_part, sb);
          //g_print("Music for staff is \n%s\n", visible_part>0?"visible":"NOT visible");

          //FIXME amalgamate movement and voice names below here...
          /* output score block */
          if (visible_movement == 1 && (visible_part == 1))
            {
              if (!(curstaffstruct->voicecontrol & DENEMO_SECONDARY))
                {
                  if (curstaffstruct->verses)
                    {
                      GList *g;
                      gint versenum;
                      for (g = curstaffstruct->verses, versenum = 1; g; g = g->next, versenum++)
                        {
                          GString *versename = g_string_new ("");
                          GString *temp = g_string_new ("");
                          g_string_printf (temp, "Verse%d", versenum);
                          set_lily_name (temp, versename);

                          gchar *sofar = g_strdup (staffdefinitions->str);
                          //this definition is used by score_layout.c to get LilyPond to provide the durations for lyrics, which denemo does not compute (contrast figured bass and chord symbols where durations are computed, see above).
                          g_string_printf (staffdefinitions, "\n%s%s%sContext = \\context Lyrics = %s%s%s \\lyricsto %s%s \\%s%sLyrics%s\n%s", movement_name->str, voice_name->str, versename->str, movement_name->str, voice_name->str, versename->str, voice_name->str, movement_name->str, movement_name->str, voice_name->str, versename->str, sofar);
                          g_free (sofar);
                          g_string_free (versename, TRUE);
                          g_string_free (temp, TRUE);

                        }
                    }
#if 0
                  if (curstaffstruct->hasfigures)
                    g_string_append_printf (staffdefinitions, TAB TAB " \\context Staff = \"%s\" \\with {implicitBassFigures = #'(0) } \\%s%sBassFiguresLine\n", curstaffstruct->denemo_name->str, movement_name->str, voice_name->str);
#endif
                  //g_string_append_printf(staffdefinitions, TAB TAB"%s\n", endofblock);
                }
              else if (curstaffstruct->voicecontrol & DENEMO_SECONDARY)
                {
                  //g_string_append_printf(staffdefinitions, "%s"TAB TAB"\\%s%s\n"TAB TAB"\n"TAB TAB"\n", thestr->str, movement_name->str, voice_name->str);

                  if (curstaffstruct->verses)
                    {
                      GList *g;
                      gint versenum;
                      for (g = curstaffstruct->verses, versenum = 1; g; g = g->next, versenum++)
                        {
                          GString *versename = g_string_new ("");
                          GString *temp = g_string_new ("");
                          g_string_printf (temp, "Verse%d", versenum);
                          set_lily_name (temp, versename);

                          gchar *sofar = g_strdup (staffdefinitions->str);
                          //this definition is used by score_layout.c to get LilyPond to provide the durations for lyrics, which denemo does not compute (contrast figured bass and chord symbols where durations are computed, see above).
                          g_string_printf (staffdefinitions, "\n%s%s%sContext = \\context Lyrics = %s%s%s \\lyricsto %s%s \\%s%sLyrics%s\n%s", movement_name->str, voice_name->str, versename->str, movement_name->str, voice_name->str, versename->str, voice_name->str, movement_name->str, movement_name->str, voice_name->str, versename->str, sofar);
                          g_free (sofar);


                          g_string_free (versename, TRUE);
                          g_string_free (temp, TRUE);

                        }
                    }
                  // g_string_append_printf(staffdefinitions,"}%s", endofblock);
                }
//        g_string_free(thestr, TRUE);
            }
        }                       /*end for staff loop */



      if (visible_movement == 1)
        {




          /* output the definitions to a definitions block in the music section */
          {
            GtkTextIter iter;
            GtkTextMark *curmark;
            gchar *name = g_strdup_printf ("%s Definitions", movement_name->str);
            insert_music_section (gui, name);
            gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, gtk_text_buffer_get_mark (Denemo.textbuffer, name));
            curmark = gtk_text_buffer_create_mark (Denemo.textbuffer, NULL, &iter, FALSE);
            gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &iter, curmark);
            gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, definitions->str, -1, INEDITABLE, NULL);

            gtk_text_buffer_insert_with_tags_by_name (Denemo.textbuffer, &iter, staffdefinitions->str, -1, INEDITABLE, NULL);

            g_free (name);
            g_string_assign (definitions, "");
            g_string_assign (staffdefinitions, "");
          }



        }                       /* if visible movement */



    }                           /* for each movement */


  g_string_free (definitions, TRUE);

  // now go through gui->anchors, and to each anchor attach a copy of the original text, for checking when saving.
  {
    GList *g;
    for (g = gui->anchors; g; g = g->next)
      {
        GtkTextChildAnchor *anchor = g->data;
        GString **target = g_object_get_data (G_OBJECT (anchor), GSTRINGP);
        if (target)
          g_object_set_data (G_OBJECT (anchor), ORIGINAL, get_text (anchor));
      }


  }
#if 0
  g_get_current_time (&time);
  g_print ("time %ld secs", time.tv_sec);
  seconds -= time.tv_sec;
  g_print ("time diff = %ld\n", seconds);
#endif

  {
    GtkTextIter startiter, enditer;
    gtk_text_buffer_get_start_iter (Denemo.textbuffer, &startiter);
    gtk_text_buffer_get_end_iter (Denemo.textbuffer, &enditer);
    gtk_text_buffer_apply_tag_by_name (Denemo.textbuffer, "monospace", &startiter, &enditer);
  }

  gtk_text_buffer_set_modified (Denemo.textbuffer, FALSE);

}                               /* output_score_to_buffer */



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
export_lilypond (gchar * thefilename, DenemoGUI * gui, gboolean all_movements, gchar * partname)
{
  GtkTextIter startiter, enditer;

  output_score_to_buffer (gui, all_movements, partname);
  GString *filename = g_string_new (thefilename);
  if (filename)
    {
      gtk_text_buffer_get_start_iter (Denemo.textbuffer, &startiter);
      gtk_text_buffer_get_end_iter (Denemo.textbuffer, &enditer);
      gchar *lily = gtk_text_buffer_get_text (Denemo.textbuffer, &startiter, &enditer, FALSE);
      /* Append .ly onto the filename if necessary */
      if (strcmp (filename->str + filename->len - 3, ".ly"))
        g_string_append (filename, ".ly");
      /* Now open the file */
      FILE *fp;
      fp = fopen (filename->str, "w");
      if (!fp)
        {
          warningdialog (_("Could not open output file for writing"));
          g_warning ("Cannot open %s \n", filename->str);
          return;
        }
      fprintf (fp, "%s", lily);
      g_free (lily);
      fclose (fp);
      g_string_free (filename, TRUE);
    }
}

void
exportlilypond (gchar * thefilename, DenemoGUI * gui, gboolean all_movements)
{
  export_lilypond (thefilename, gui, all_movements, NULL);
}





/* output lilypond for the current staff
 */
void
export_lilypond_part (char *filename, DenemoGUI * gui, gboolean all_movements)
{
  export_lilypond (filename, gui, all_movements, ((DenemoStaff *) gui->si->currentstaff->data)->lily_name->str);
}

/* output lilypond for each part into a separate file
 */
void
export_lilypond_parts (char *filename, DenemoGUI * gui)
{
  gchar *staff_filename;
  staffnode *curstaff;
  DenemoStaff *curstaffstruct;
  DenemoScore *si = gui->si;
  for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
    {

      curstaffstruct = (DenemoStaff *) curstaff->data;
      gchar *c = filename + strlen (filename);  // find .extension FIXME dots in filename
      while (*c != '.' && c != filename)
        c--;
      if (c != filename)
        *c = '\0';
      else
        {
          warningdialog (_("Filename does not have extension"));
          return;
        }
      staff_filename = g_strconcat (filename, "_", curstaffstruct->lily_name->str, ".ly", NULL);
      *c = '.';
      export_lilypond (staff_filename, gui, FALSE, ((DenemoStaff *) curstaff->data)->lily_name->str);

    }

  if (staff_filename)
    g_free (staff_filename);
}

/* callback on closing lilypond window */
static gboolean
lilywindow_closed ()
{
  activate_action ("/MainMenu/ViewMenu/ToggleLilyText");
  return TRUE;
}


static gboolean lily_refresh (GtkWidget * item, GdkEventCrossing * e);

static gboolean
lily_save (G_GNUC_UNUSED GtkWidget * item, G_GNUC_UNUSED GdkEventCrossing * e)
{
  DenemoGUI *gui = Denemo.gui;
  //g_print("Consider Save ... %d %d", gui->lilysync, gui->changecount);
  // g_signal_handlers_block_by_func (G_OBJECT (SIGNAL_WIDGET), G_CALLBACK (lily_save), NULL);
  // g_signal_handlers_unblock_by_func (G_OBJECT (SIGNAL_WIDGET), G_CALLBACK (lily_refresh), gui);
  merge_lily_strings (gui);
  if (gui->si)
    update_drawing_cache ();
  return FALSE;
}

static gboolean
lily_refresh (G_GNUC_UNUSED GtkWidget * item, G_GNUC_UNUSED GdkEventCrossing * e)
{
  DenemoGUI *gui = Denemo.gui;
  //g_print("Consider Refresh ... %d %d", gui->lilysync, gui->changecount);

  //g_signal_handlers_block_by_func(G_OBJECT (SIGNAL_WIDGET), G_CALLBACK (lily_refresh), NULL);
  //g_signal_handlers_unblock_by_func (G_OBJECT (SIGNAL_WIDGET), G_CALLBACK (lily_save), NULL);

  if (gui->si->markstaffnum || (gui->lilysync != gui->changecount))
    {
      gui->si->markstaffnum = 0;        //remove selection, else we will only see that bit in LilyText   
      refresh_lily_cb (NULL, gui);
    }
  return FALSE;
}


static void
prepend_menu_item (GtkMenuShell * menu, DenemoGUI * gui, gchar * text, gpointer callback)
{
  GtkWidget *item;
  item = gtk_menu_item_new_with_label (text);
  g_signal_connect (item, "activate", G_CALLBACK (callback), gui);
  gtk_menu_shell_prepend (menu, GTK_WIDGET (item));
  gtk_widget_show (GTK_WIDGET (item));
}

static gboolean
populate_called (G_GNUC_UNUSED GtkWidget * view, GtkMenuShell * menu)
{
  DenemoGUI *gui = Denemo.gui;
  //g_print("populate called with %p\n", menu);
  prepend_menu_item (menu, gui, _("Find Current Object"), (gpointer) place_cursor_cb);
  
#ifdef USE_EVINCE  
  prepend_menu_item (menu, gui, _("Print from visible LilyPond text"), (gpointer) typeset_current_layout);
#endif
  
  return FALSE;
}

// moves cursor to position indicated by anchor found before line and column, and sets si->target to indicate type of construct there.
gboolean
goto_lilypond_position (gint line, gint column)
{
  DenemoGUI *gui = Denemo.gui;
  GtkTextIter enditer, iter;
#ifdef G_OS_WIN32
  mswin ("goto_lilypond_position called for line %d column %d\n", line, column);
#endif
  gtk_text_buffer_get_end_iter (Denemo.textbuffer, &enditer);
  gtk_text_buffer_get_start_iter (Denemo.textbuffer, &iter);

  line--;

  column++;                     //needed to avoid stepping back after anchor on directives
  if (column > 0 && line > 0)
    {
      gtk_text_buffer_get_iter_at_line_offset (Denemo.textbuffer, &iter, line, 0);
      gint maxcol = gtk_text_iter_get_chars_in_line (&iter);
      //g_print("line %d column %d\n", line, column);
      //g_print("line has %d chars\n", maxcol);
      gtk_text_iter_set_visible_line_offset (&iter, MIN (maxcol, column));
      gtk_text_buffer_place_cursor (Denemo.textbuffer, &iter);
      GtkTextChildAnchor *anchor = gtk_text_iter_get_child_anchor (&iter);
      //if(anchor) g_print("At anchor %x <%c> ", anchor, gtk_text_iter_get_char (&iter));
      //else g_print("Not at anchor <%c> ", gtk_text_iter_get_char (&iter));
      if (anchor && (g_object_get_data (G_OBJECT (anchor), MOVEMENTNUM) == NULL))
        anchor = NULL;
      while ((anchor == NULL) && gtk_text_iter_backward_char (&iter))
        {
          anchor = gtk_text_iter_get_child_anchor (&iter);
          if (anchor && (g_object_get_data (G_OBJECT (anchor), MOVEMENTNUM) == NULL))
            anchor = NULL;      //ignore anchors without positional info
          //g_print("#%c#", gtk_text_iter_get_char (&iter));
        }
      if (anchor)
        {
          gint objnum = (intptr_t) g_object_get_data (G_OBJECT (anchor), OBJECTNUM);
          gint measurenum = (intptr_t) g_object_get_data (G_OBJECT (anchor), MEASURENUM);
          gint staffnum = (intptr_t) g_object_get_data (G_OBJECT (anchor), STAFFNUM);
          gint movementnum = (intptr_t) g_object_get_data (G_OBJECT (anchor), MOVEMENTNUM);
          gint directivenum = (intptr_t) g_object_get_data (G_OBJECT (anchor), DIRECTIVENUM);
          gint mid_c_offset = (intptr_t) g_object_get_data (G_OBJECT (anchor), MIDCOFFSET);
          //g_print("location %d %d %d %d\n", objnum, measurenum, staffnum, movementnum);
          DenemoTargetType type = (intptr_t) g_object_get_data (G_OBJECT (anchor), TARGETTYPE);
          gui->si->target.objnum = objnum;
          gui->si->target.measurenum = measurenum;
          gui->si->target.staffnum = staffnum;
          gui->si->target.type = type;
          gui->si->target.directivenum = directivenum;
#ifdef G_OS_WIN32
          g_print ("goto_lilypond_position: anchor located and target set %d %d\n", measurenum, objnum);
#endif
          if (movementnum < 1)
            {
              g_warning ("Object %p has no location data\n", g_object_get_data (G_OBJECT (anchor), OBJECTNODE));
              return FALSE;
            }
          if (!goto_movement_staff_obj (gui, movementnum, staffnum, measurenum, objnum))
            return FALSE;
          //g_print("TARGET is %d\n", type);
          if (type == TARGET_NOTE)
            {
              int midcoffset = (intptr_t) g_object_get_data (G_OBJECT (anchor), MIDCOFFSET);
              //!!!!move cursor to midcoffset  This has been lifted from view.c, but there surely should exist a function to do this 
              {
                //dclef =  find_prevailing_clef(gui->si); This should be dropped from scheme_cursor_to_note() as well I guess.
                gui->si->cursor_y = mid_c_offset;
                gui->si->staffletter_y = offsettonumber (gui->si->cursor_y);
                displayhelper (gui);
              }
              gui->si->target.mid_c_offset = midcoffset;
            }
#ifdef G_OS_WIN32
          g_print ("goto_lilypond_position: Success\n");
#endif
          return TRUE;
        }
      else
        g_warning ("Anchor not found\n");
    }                           //if reasonable column and line number
  return FALSE;
}

static gboolean
lily_keypress (G_GNUC_UNUSED GtkWidget * w, GdkEventKey * event)
{
  DenemoGUI *gui = Denemo.gui;
  GtkTextIter cursor;
  gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &cursor, gtk_text_buffer_get_insert (Denemo.textbuffer));


#ifndef TEXTBUFFER_BUG_FIXED
  //FIXME workaround for a nasty bug, presumably in GTK, where the back arrow gets a wrong char count, off end of line
  if ((event->keyval == 65361) && (gtk_text_iter_get_line_offset (&cursor) < 2))
    {
      g_print ("avoiding gtk bug...\n");
      GtkTextTag *tag = gtk_text_tag_table_lookup (tagtable, "invisible");
      if (tag && gtk_text_iter_has_tag (&cursor, tag))
        {
          while (gtk_text_iter_backward_cursor_position (&cursor) && gtk_text_iter_has_tag (&cursor, tag))
            gtk_text_buffer_place_cursor (Denemo.textbuffer, &cursor);
          g_print ("backed up\n");
          return TRUE;
        }
      if (gtk_text_iter_backward_cursor_position (&cursor))
        gtk_text_buffer_place_cursor (Denemo.textbuffer, &cursor);
      return TRUE;
    }
#endif

  if (event->state & (GDK_CONTROL_MASK))
    return FALSE;
  // if you have a visible marker you do this gtk_text_iter_backward_cursor_position(&cursor);
  GtkTextChildAnchor *anchor = gtk_text_iter_get_child_anchor (&cursor);
  //g_print("Got a keypress event at anchor %p\n", anchor);
  //g_print("The character is %x keyval %x at %d\n", (guint)gdk_keyval_to_unicode(event->keyval), event->keyval,  gtk_text_iter_get_line_offset(&cursor));
  if (anchor)
    {
      gint objnum = (intptr_t) g_object_get_data (G_OBJECT (anchor), OBJECTNUM);
      gint measurenum = (intptr_t) g_object_get_data (G_OBJECT (anchor), MEASURENUM);
      gint staffnum = (intptr_t) g_object_get_data (G_OBJECT (anchor), STAFFNUM);
      gint movementnum = (intptr_t) g_object_get_data (G_OBJECT (anchor), MOVEMENTNUM);
      //g_print("location %d %d %d %d\n", objnum, measurenum, staffnum, movementnum);
      if (movementnum < 1)
        return FALSE;
      if (!goto_movement_staff_obj (gui, movementnum, staffnum, measurenum, objnum))
        return FALSE;
      gchar *key = g_strdup_printf ("%c", gdk_keyval_to_unicode (event->keyval));
      GList *curobjnode = gui->si->currentobject;
      DenemoObject *obj = curobjnode ? curobjnode->data : NULL;
      if (obj && *key > 0x1f)
        {
          switch (obj->type)
            {
            case LILYDIRECTIVE:
              gtk_text_iter_forward_char (&cursor);     // past anchor
              GString **target = g_object_get_data (G_OBJECT (anchor), GSTRINGP);
              if (!*target)
                {
                  *target = g_string_new (key);
                  //g_print("new string %s (%x)\n", key, *key);
                }
              else
                {
                  gchar *lily = get_text (anchor);
                  g_string_assign (*target, lily);      //FIXME free original

                  g_free (lily);
                  g_string_prepend (*target, key);
                  g_object_set_data (G_OBJECT (anchor), ORIGINAL, get_text (anchor));
                  //g_print("prepended %s (%x)\n", key, *key);
                }
              score_status (gui, TRUE);
              refresh_lily_cb (NULL, gui);
              gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &cursor, gtk_text_buffer_get_insert (Denemo.textbuffer));
              if (gtk_text_iter_forward_char (&cursor))
                gtk_text_buffer_place_cursor (Denemo.textbuffer, &cursor);
              g_free (key);
              return TRUE;
            case CHORD:
            default:
              {
                DenemoObject *lilyobj = lily_directive_new (key);
                //g_print("inserted a lilydirective  %s (%x)\n", key, *key);
                object_insert (gui, lilyobj);
                displayhelper (gui);
                refresh_lily_cb (NULL, gui);
                gtk_text_buffer_get_iter_at_mark (Denemo.textbuffer, &cursor, gtk_text_buffer_get_insert (Denemo.textbuffer));
                if (gtk_text_iter_forward_char (&cursor) && gtk_text_iter_forward_char (&cursor))
                  gtk_text_buffer_place_cursor (Denemo.textbuffer, &cursor);
                g_free (key);
                return TRUE;
              }
            }                   // switch obj->type

        }                       //if useful keypress??
      g_free (key);
    }                           //if cursor is at anchor
  return FALSE;                 //let the normal handler have the keypress
}

static void
create_console (GtkWidget * box)
{
  //GtkWidget *vpaned = gtk_vpaned_new ();
  //gtk_container_set_border_width (GTK_CONTAINER(vpaned), 5);
  //gtk_box_pack_start (GTK_BOX (box), vpaned, FALSE, TRUE, 0);
  if (Denemo.console)
    return;
  Denemo.console = gtk_text_view_new ();
  GtkWidget *sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  //gtk_paned_add1 (GTK_PANED (vpaned), sw);
  gtk_box_pack_start (GTK_BOX (box), sw, FALSE, TRUE, 0);
  gtk_container_add (GTK_CONTAINER (sw), Denemo.console);
  // gtk_widget_show_all(vpaned);
  gtk_widget_show_all (sw);
}


void
create_lilywindow (void)
{
  Denemo.textwindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  //gtk_window_set_position (GTK_WINDOW (Denemo.textwindow), GTK_WIN_POS_NONE);
  gtk_window_set_default_size (GTK_WINDOW (Denemo.textwindow), 800, 600);
  gtk_window_set_title (GTK_WINDOW (Denemo.textwindow), "LilyPond Text - Denemo");
  g_signal_connect (G_OBJECT (Denemo.textwindow), "delete-event", G_CALLBACK (lilywindow_closed), NULL);
  GtkWidget *vbox = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (Denemo.textwindow), vbox);
  GtkWidget *vpaned = gtk_vpaned_new ();
  gtk_container_set_border_width (GTK_CONTAINER (vpaned), 5);
  gtk_box_pack_start (GTK_BOX (vbox), vpaned, TRUE, TRUE, 0);
  //gtk_container_add (GTK_CONTAINER (Denemo.textwindow), vpaned);
  create_console (vbox);

  GtkWidget *view = gtk_text_view_new ();
  GtkWidget *sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_paned_add1 (GTK_PANED (vpaned), sw);
  gtk_container_add (GTK_CONTAINER (sw), view);
  gtk_widget_show_all (vbox);

  tagtable = (GtkTextTagTable *) gtk_text_tag_table_new ();
  Denemo.textview = (GtkTextView *) view;

#if 1
  g_signal_connect (G_OBJECT (Denemo.textview), "key-press-event", G_CALLBACK (lily_keypress), NULL);
#endif

  g_signal_connect (G_OBJECT (Denemo.textview), "populate-popup", G_CALLBACK (populate_called), NULL);



  /*   g_object_set_data(G_OBJECT (SIGNAL_WIDGET),"enter-signal", (gpointer)id); */
  GtkTextTag *t;

  t = gtk_text_tag_new ("system_invisible");
  g_object_set (G_OBJECT (t), "invisible", TRUE, NULL);
  gtk_text_tag_table_add (tagtable, t);

  t = gtk_text_tag_new (INEDITABLE);
  g_object_set (G_OBJECT (t), "editable", FALSE, NULL);
  gtk_text_tag_table_add (tagtable, t);

  t = gtk_text_tag_new (HIGHLIGHT);
  g_object_set (G_OBJECT (t), "background", "light gray", NULL);
  gtk_text_tag_table_add (tagtable, t);

  t = gtk_text_tag_new (ERRORTEXT);
  g_object_set (G_OBJECT (t), "background", "pink", NULL);
  gtk_text_tag_table_add (tagtable, t);


  t = gtk_text_tag_new ("bold");
  g_object_set (G_OBJECT (t), "weight", PANGO_WEIGHT_BOLD, "family", "monospace", NULL);
  gtk_text_tag_table_add (tagtable, t);

  t = gtk_text_tag_new ("monospace");
  g_object_set (G_OBJECT (t), "family", "monospace", NULL);
  gtk_text_tag_table_add (tagtable, t);


  Denemo.textbuffer = gtk_text_buffer_new (tagtable);
  gtk_text_view_set_buffer (GTK_TEXT_VIEW (Denemo.textview), Denemo.textbuffer);
  //gui->lilysync = G_MAXUINT;//buffer not yet up to date


  g_signal_connect (G_OBJECT (SIGNAL_WIDGET), LEAVE_NOTIFY_EVENT, G_CALLBACK (lily_save), NULL);
  g_signal_connect (G_OBJECT (SIGNAL_WIDGET), ENTER_NOTIFY_EVENT, G_CALLBACK (lily_refresh), NULL);
  //g_signal_handlers_block_by_func(G_OBJECT (SIGNAL_WIDGET), G_CALLBACK (lily_refresh), NULL);
}
