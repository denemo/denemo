/* lyric.c
 *
 * Functions for the manipulations of lyrics
 *
 * for Denemo, a gtk+ frontend for GNU Lilypond
 * (c)2009 Richard Shann
 */
#include <stdlib.h>
#include <string.h>             /* for strcmp() */
#include "command/lyric.h"
#include "command/object.h"
#include "command/staff.h"
#include "core/utils.h"
#include "core/view.h"
#include "core/menusystem.h"
#include "command/score.h"
#include "display/draw.h"
#include "command/lilydirectives.h"

// movement->lyricsbox is a GtkVBox containing GtkNotebook s for each staff with verses. The verses are GtkTextView s  notebook
//staff->verse_views is a list of GtkTextView which are packed in a GtkScrolledWindow
//so  movement->lyricsbox contains Notebook->ScrolledWindow->TextView
//the text of the verses is stored in staff->verses
static GtkWidget *DummyVerse;   /* a non-existent verse */
static gint SkipCount = 0;      //count of syllables to be skipped


gboolean
denemo_pango_scan_string (const char **pos, GString *out)
{
  const char *p = *pos;

  while (g_ascii_isspace (*p))
    p++;

  if (G_UNLIKELY (!*p))
    return FALSE;
  else if (*p == '"')
    {
      gboolean quoted = FALSE;
      g_string_truncate (out, 0);

      p++;

      while (TRUE)
    {
      if (quoted)
        {
          int c = *p;

          switch (c)
        {
        case '\0':
          return FALSE;
        case 'n':
          c = '\n';
          break;
        case 't':
          c = '\t';
          break;
        default:
          break;
        }

          quoted = FALSE;
          g_string_append_c (out, c);
        }
      else
        {
          switch (*p)
        {
        case '\0':
          return FALSE;
        case '\\':
          quoted = TRUE;
          break;
        case '"':
          p++;
          goto done;
        default:
          g_string_append_c (out, *p);
          break;
        }
        }
      p++;
    }
    done:
      ;
    }
  else
    {
      g_string_truncate (out, 0);

      while (*p && !g_ascii_isspace (*p))
    {
      g_string_append_c (out, *p);
      p++;
    }
    }

  *pos = p;

  return TRUE;
}

GtkWidget *
verse_get_current_view (DenemoStaff * staff)
{
  if (!staff)
    return NULL;
  if (!staff->current_verse_view)
    return NULL;
  return staff->current_verse_view->data;
}

gboolean
verse_set_current (DenemoStaff * staff, gint id)
{
if (staff && staff->verse_views && (id >= 0) && (id < g_list_length (staff->verse_views)))
    {
      staff->current_verse_view = g_list_nth (staff->verse_views, id);
      GtkWidget *w = staff->verse_views->data;
      GtkWidget *notebook = gtk_widget_get_parent (gtk_widget_get_parent (w));
      if(notebook) gtk_notebook_set_current_page (GTK_NOTEBOOK (notebook), id);
      return TRUE;
    }
  else
    return FALSE;
}

static void point_to_verse (DenemoStaff * staff, guint id)
{
  if (staff && staff->verse_views && (id >= 0) && (id < g_list_length (staff->verse_views)))
    {
      staff->current_verse_view = g_list_nth (staff->verse_views, id);
  }
}

gint
verse_get_current (DenemoStaff * staff)
{
  return g_list_position (staff->verse_views, staff->current_verse_view);
}

static void
previous_verse (void)
{
  DenemoStaff *thestaff = Denemo.project->movement->currentstaff->data;
  GtkWidget *w = thestaff->verse_views->data;
  GtkNotebook *notebook = (GtkNotebook *)gtk_widget_get_parent (gtk_widget_get_parent (w));
  gtk_notebook_prev_page (notebook);
}

static void
next_verse (void)
{
  DenemoStaff *thestaff = Denemo.project->movement->currentstaff->data;
  GtkWidget *w = thestaff->verse_views->data;
  GtkNotebook *notebook = (GtkNotebook *)gtk_widget_get_parent (gtk_widget_get_parent (w));
  gtk_notebook_next_page (notebook);
}

void
verse_set_current_text (DenemoStaff * staff, gchar * text)
{
  if (text)
    {
      gint pos = verse_get_current (staff);
      if (pos >= 0)
        {
          GList *the_current_verse = g_list_nth (staff->verses, pos);
          if (the_current_verse->data)
            g_free (the_current_verse->data);
          the_current_verse->data = text;
        }
    }
}

gchar *
verse_get_current_text (DenemoStaff * staff)
{
  gint id = verse_get_current (staff);
  if (id >= 0)
    {
      GList *verse = g_list_nth (staff->verses, id);
      if (verse)
        return (gchar *) verse->data;
    }
  return NULL;
}

gboolean
lyric_changed_cb (GtkTextBuffer * buffer)
{
  // Synchronizes buffers and verses
  DenemoStaff *staff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  gchar *text = get_lyrics_for_current_verse (staff);
  verse_set_current_text (staff, text);
  {
    GtkTextIter startiter, enditer;
    gtk_text_buffer_get_start_iter (buffer, &startiter);
    gtk_text_buffer_get_end_iter (buffer, &enditer);
    gtk_text_buffer_apply_tag_by_name (buffer, "highlight", &startiter, &enditer);
  }
  score_status (Denemo.project, TRUE);
  draw_score_area ();
  return FALSE;
}

static GtkWidget *
new_lyric_editor (void)
{
  GtkTextTagTable *tagtable = (GtkTextTagTable *) gtk_text_tag_table_new ();
  GtkTextTag *t;
  t = gtk_text_tag_new ("highlight");
  g_object_set (G_OBJECT (t), "background", "white", NULL);
  gtk_text_tag_table_add (tagtable, t);
  GtkTextBuffer *buffer = gtk_text_buffer_new (tagtable);
  GtkWidget *view = gtk_text_view_new_with_buffer (buffer);
  GtkWidget *sw = gtk_scrolled_window_new (gtk_adjustment_new (0, 0, 0, 0, 0, 0), gtk_adjustment_new (0, 0, 0, 0, 0, 0));
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  if (Denemo.prefs.newbie)
    gtk_widget_set_tooltip_text (sw, _("The text of a verse can be typed or pasted here. Press Esc to return to editing notes.\n" "Separate syllables with space double hyphen space, -- , if they should have their own note(s).\n" "New lines and extra spaces have no special significance. Slurs on notes make them take only one syllable. Use the underscore _ for blank syllables."));
  gtk_container_add (GTK_CONTAINER (sw), view);

  return view;
}

DenemoObject *
newlyric (gint baseduration, gint numdots, gchar * lys)
{
  g_warning ("Not implemented");
  return NULL;
}

static void
switch_page (GtkNotebook * notebook, gpointer dummy, guint pagenum, DenemoStaff * staff)
{
  draw_score_area ();
  point_to_verse (staff, pagenum);
}

//scans *next for a syllable putting the syllable into gs and moving *next to the address beyond the syllable
//return TRUE if a syllable was found
static gboolean
scan_syllable (gchar ** next, GString * gs)
{
  gboolean result;
  gchar *initial = *next;
  if (SkipCount > 0)
    {
      SkipCount--;
      g_string_assign (gs, "(skip)");
      return TRUE;
    }
  result = denemo_pango_scan_string ((const char **) next, gs);
  if (result && (*gs->str == '\\') && (*(gs->str + 1) != '\\') && (*(gs->str + 1) != '\"'))
    {
      //if it is \repeat unfold n \skip 1 put the number n into SkipCount and return a space indicator
      if (!strcmp (gs->str, "\\repeat"))
        {
          result = denemo_pango_scan_string ((const char **) next, gs);
          if (result && (!strcmp (gs->str, "unfold")))
            {
              result = denemo_pango_scan_string ((const char **) next, gs);
              if (result)
                SkipCount = atoi (gs->str);
              while (**next && **next != '\n')
                (*next)++;      //ignore to end of line
              return scan_syllable (next, gs);
            }
        }
      while (**next && **next != '\n')
        (*next)++;              //ignore to end of line
      return scan_syllable (next, gs);
    }
  if (result && (*gs->str == '%'))
    {
      while (**next && **next != '\n')
        (*next)++;              //ignore to end of line
      return scan_syllable (next, gs);
    }
  if (result && ((!strcmp (gs->str, "--") || (!strcmp (gs->str, "__")))))
    return scan_syllable (next, gs);
  return result;
}


//get the count of the syllable at the cursor.
static gint
get_syllable_count (GtkTextBuffer * buffer)
{
  GString *gs = g_string_new ("");
  GtkTextIter cursor, startiter;
  gtk_text_buffer_get_iter_at_mark (buffer, &cursor, gtk_text_buffer_get_insert (buffer));
  gtk_text_buffer_get_start_iter (GTK_TEXT_BUFFER (buffer), &startiter);
  gchar *text = gtk_text_buffer_get_text (GTK_TEXT_BUFFER (buffer), &startiter, &cursor, FALSE);
  gchar *next = text;
  gint count = 0;
  while (scan_syllable (&next, gs))
    count++;
  g_string_free (gs, TRUE);
  g_free (text);
  return count ? count : 1;
}

/* fills the measure and object fields of the passed in position with values for num'th syllable */
static void
get_pos_at_syllable_count (DenemoStaff * staff, gint num, DenemoPosition * pos)
{
  gint count = 0;
  gint measurenum, objnum;
  GList *curmeasure = staff->themeasures;
  gboolean in_slur = FALSE;
  for (measurenum = 0; curmeasure && (count < num); measurenum++, curmeasure = curmeasure->next)
    {
      objnode *curobj;
      for (objnum = 0, curobj = ((DenemoMeasure *) curmeasure->data)->objects; curobj && (count < num); objnum++, curobj = curobj->next)
        {
          DenemoObject *obj = curobj->data;

          if (obj->type == CHORD)
            {
              chord *thechord = ((chord *) obj->object);
              if (thechord->notes && !in_slur)
                count++;
              if (thechord->slur_begin_p)
                in_slur = TRUE;
              if (thechord->slur_end_p)
                in_slur = FALSE;
              if (thechord->is_tied)
                count--;
            }
        }                       //for objs
    }                           //for measures
  pos->measure = measurenum;
  pos->object = objnum;
}

/* count the number of syllables up to Denemo cursor position */
gint
syllable_count (void)
{
  DenemoStaff *thestaff = Denemo.project->movement->currentstaff->data;
  gint count = 0;
  gint i;
  GList *curmeasure;
  gboolean in_slur = FALSE;
  objnode *curobj = Denemo.project->movement->currentobject;
  for (curmeasure = thestaff->themeasures; curmeasure; curmeasure = curmeasure->next)
    {
      objnode *thisobj;
      for (thisobj = ((DenemoMeasure *) curmeasure->data)->objects; thisobj && (thisobj != curobj); thisobj = thisobj->next)
        {
          DenemoObject *obj = thisobj->data;
          if (obj->type == CHORD)
            {
              chord *thechord = ((chord *) obj->object);
              if (thechord->notes && !in_slur && !find_directive (thechord->directives, "MoveRest"))
                count++;
              if (thechord->slur_begin_p)
                in_slur = TRUE;
              if (thechord->slur_end_p)
                in_slur = FALSE;
              if (thechord->is_tied)
                count--;
            }
        }                       //for objs

      if (thisobj == curobj)
        break;
    }                           //for measures

  return count;
}

static gint
get_character_count_at_syllable (gchar * text, gint count)
{
  gint chars = 0;
  GString *gs = g_string_new ("");
  for (; count; count--)
    {
      gchar *next = text + chars;
      gint this;
      if (!scan_syllable (&next, gs))
        break;
      chars = next - text;
    }
  g_string_free (gs, TRUE);
  return chars;
}

gboolean
synchronize_lyric_cursor (gint offset)
{
  DenemoStaff *thestaff = Denemo.project->movement->currentstaff->data;
  gint count = syllable_count () + 1 + offset;
  GtkTextView *verse_view = (GtkTextView *) verse_get_current_view (thestaff);
  if (verse_view)
    {
      gchar *text = get_text_from_view (GTK_WIDGET (verse_view));
      gint character_count = get_character_count_at_syllable (text, count);
      GtkTextBuffer *textbuffer = gtk_text_view_get_buffer (verse_view);
      GtkTextIter where;
      gtk_text_buffer_get_iter_at_offset (textbuffer, &where, character_count);
      gtk_text_buffer_place_cursor (textbuffer, &where);
      gtk_widget_grab_focus (GTK_WIDGET (verse_view));
      gtk_text_view_scroll_mark_onscreen (verse_view, gtk_text_buffer_get_insert (textbuffer));
      return TRUE;
    }
  return FALSE;
}

static void
synchronize_cursor (GtkWidget * textview)
{
  DenemoStaff *thestaff = Denemo.project->movement->currentstaff->data;
  gint count;
  DenemoPosition pos;
  count = get_syllable_count (gtk_text_view_get_buffer (GTK_TEXT_VIEW (textview)));
  get_pos_at_syllable_count (thestaff, count, &pos);
  goto_movement_staff_obj (NULL, 0, Denemo.project->movement->currentstaffnum, pos.measure, pos.object, 0 /* means ignore */ );
}

static gboolean
keypress (GtkWidget * textview, GdkEventKey * event)
{
  guint keyval = event->keyval; //g_print ("press %x mask %x", event->keyval, event->state);
  if (keyval == 0xFF63)
    return TRUE;                // ignore Ins, don't want to have overwrite mode

  if (keyval == 0xFF09)         //TAB
    return TRUE;
  if (event->state & GDK_CONTROL_MASK)  //allow save etc from lyrics pane
    {
      switch (event->keyval)
        {
        case 0x6C:             //Control-l standard prefix for lyrics commands
          switch_back_to_main_window ();
          break;                //return TRUE;
        case 0xFF55:           //Control-PgDn
          previous_verse ();
          return TRUE;
        case 0xFF56:           //Control-PgUp
          next_verse ();
          return TRUE;
        case 0x73:             //Control-s save but stay in verse
          call_out_to_guile ("(d-Save)");
          return TRUE;
        case 0xFF52:           //Control-up
          call_out_to_guile ("(d-MoveToStaffUp)(d-EditLyricAtCursor)");
          return TRUE;
        case 0xFF54:           //Control-down
          call_out_to_guile ("(d-MoveToStaffDown)(d-EditLyricAtCursor)");
          return TRUE;
        default:
          break;
        }
    }
  return FALSE;
}

static gboolean
text_inserted_cb (GtkWidget * textview, GdkEventKey * event)
{
  static gboolean seen_space;
  gchar *str = event->string;
  guint keyval = event->keyval; // g_print ("release %x", event->keyval);
  if ((keyval == 0x20) || (keyval == 0xFF0D) || (keyval == 0xFF09) || (keyval == 0xFF8D))       //space return tab Enter
    {
      seen_space = TRUE;
    }
  else if ((keyval == 0xFF51) || (keyval == 0xFF52) || (keyval == 0xFF53) || (keyval == 0xFF54) || seen_space)  //arrows
    {
      seen_space = FALSE;
      synchronize_cursor (textview);
    }
  //Note a Control-l is received when used as a shortcut to switch to the lyrics pane, so we use Esc or Tab to switch back
  if ((keyval == 0xFF09)        //TAB
      || (keyval == 0xFF1B))    //ESC
    {
      switch_back_to_main_window ();
      return TRUE;
    }

  return FALSE;
}

static gboolean
button_released_cb (GtkWidget * textview)
{
  synchronize_cursor (textview);
  return FALSE;
}

//Insert the text at the current insertion point of the current verse
// return FALSE if no verse
gboolean
insert_text_in_verse (gchar * text)
{
  DenemoProject *project = Denemo.project;
  DenemoMovement *movement = project->movement;
  DenemoStaff *staff = movement->currentstaff->data;
  GtkTextView *verse_view = (GtkTextView *) verse_get_current_view (staff);
  GtkTextIter iter;
  if (verse_view)
    {
      GtkTextBuffer *textbuffer = gtk_text_view_get_buffer (verse_view);
      GtkTextMark *cursor = gtk_text_buffer_get_insert (textbuffer);
      gtk_text_buffer_get_iter_at_mark (textbuffer, &iter, cursor);
      gtk_text_buffer_insert (textbuffer, &iter, text, -1);
      return TRUE;
    }
  return FALSE;
}

static void
insert_stanza_number (void)
{
  DenemoProject *project = Denemo.project;
  DenemoMovement *movement = project->movement;
  if (movement->currentstaff)
    {
      gchar *text = string_dialog_entry (Denemo.project, _("Stanza Number"), _("Give text to appear before lyrics"), _("1. "));
      if (text)
        {
          gchar *stanza = g_strdup_printf ("\\set stanza = #\"%s\"\n", text);
          g_free (text);
          (void) insert_text_in_verse (stanza);
          g_free (stanza);
        }
    }
}

static void
prepend_menu_item (GtkMenuShell * menu, gchar * text, gpointer callback, gchar * tooltip)
{
  GtkWidget *item;
  item = gtk_menu_item_new_with_label (text);
  gtk_widget_set_tooltip_text (item, tooltip);
  g_signal_connect (item, "activate", G_CALLBACK (callback), NULL);
  gtk_menu_shell_prepend (menu, GTK_WIDGET (item));
  gtk_widget_show (GTK_WIDGET (item));
}

static gboolean
populate_called (G_GNUC_UNUSED GtkWidget * view, GtkMenuShell * menu)
{
  prepend_menu_item (menu, _("Insert Stanza Number"), (gpointer) insert_stanza_number, _("Insert a stanza number using the LilyPond syntax"));
  return FALSE;
}



guint
add_verse_to_staff (DenemoMovement * movement, DenemoStaff * staff)
{
  staff->verses = g_list_append (staff->verses, NULL);
  if (Denemo.non_interactive)
    return g_list_length (staff->verses) - 1;

  GtkWidget *notebook, *textview;
  if (staff->verse_views == NULL)
    {
      notebook = gtk_notebook_new ();
      gtk_widget_show (notebook);

      g_signal_connect (G_OBJECT (notebook), "switch_page", G_CALLBACK (switch_page), staff);
      if (movement->lyricsbox == NULL)
        //FIXME we need a proper way of getting to the top vbox, that will not break when scorearea is moved in the widget hierarchy.
        install_lyrics_preview (movement, gtk_widget_get_parent (gtk_widget_get_parent (Denemo.scorearea)));
      gtk_box_pack_start (GTK_BOX (movement->lyricsbox), notebook, TRUE, TRUE, 0);
    }
  else
    {
      GtkWidget *w = staff->verse_views->data;
      notebook = gtk_widget_get_parent (gtk_widget_get_parent (w));
    }
  textview = new_lyric_editor ();
  gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (textview), GTK_WRAP_WORD_CHAR);
  gtk_widget_show_all (gtk_widget_get_parent (textview));
  staff->verse_views = g_list_append (staff->verse_views, textview);
  guint pos = g_list_position (staff->verse_views, g_list_last (staff->verse_views));
  point_to_verse (staff, pos);
  gint pagenum = gtk_notebook_append_page (GTK_NOTEBOOK (notebook), gtk_widget_get_parent (textview), NULL);
  gtk_notebook_set_current_page (GTK_NOTEBOOK (notebook), pagenum);
  gchar *tablabel = g_strdup_printf (_("Verse %d"), pagenum + 1);
  gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (notebook), gtk_widget_get_parent (textview), tablabel);
  g_free (tablabel);
  if (pagenum)
    gtk_notebook_set_show_tabs (GTK_NOTEBOOK (notebook), TRUE);
  GtkTextView *verse_view = (GtkTextView *) verse_get_current_view (staff);
  g_signal_connect (G_OBJECT (gtk_text_view_get_buffer (verse_view)), "changed", G_CALLBACK (lyric_changed_cb), NULL);
  g_signal_connect (G_OBJECT (verse_view), "key-release-event", G_CALLBACK (text_inserted_cb), NULL);
  g_signal_connect (G_OBJECT (verse_view), "key-press-event", G_CALLBACK (keypress), NULL);

  g_signal_connect (G_OBJECT (verse_view), "button-release-event", G_CALLBACK (button_released_cb), NULL);
  g_signal_connect_after (G_OBJECT (verse_view), "populate-popup", G_CALLBACK (populate_called), NULL);
#if GTK_MAJOR_VERSION==2
  GdkColor thecolor;
  //gdk_color_parse ("white", &thecolor);
  //gtk_widget_modify_bg (verse_view, GTK_STATE_SELECTED, &thecolor);
  gdk_color_parse ("gray", &thecolor);
  gtk_widget_modify_bg (verse_view, GTK_STATE_NORMAL, &thecolor);
#else
{
GtkCssProvider *gcp;
GtkStyleContext *gsc;
gsc = gtk_widget_get_style_context(GTK_WIDGET (verse_view));
gchar *str = "GtkTextView {background-color: rgb(128,128,80);}"; //this overrides the focus one unless it is before.
gcp= gtk_css_provider_new();
gtk_css_provider_load_from_data(gcp, str, -1, 0);
gtk_style_context_add_provider(gsc, GTK_STYLE_PROVIDER(gcp), 
    GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);

str = "GtkTextView:focus {background-color: rgb(128,128,0);}"; //this is effective
gcp= gtk_css_provider_new();
gtk_css_provider_load_from_data(gcp, str, -1, 0);
gtk_style_context_add_provider(gsc, GTK_STYLE_PROVIDER(gcp), 
    GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
    
str =  "GtkTextView:selected {background-color: rgb(200,128,0);}";
gcp= gtk_css_provider_new();
gtk_css_provider_load_from_data(gcp, str, -1, 0);
gtk_style_context_add_provider(gsc, GTK_STYLE_PROVIDER(gcp), 
    GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);

#if ((GTK_MAJOR_VERSION==3) && (GTK_MINOR_VERSION >= 20))
str = "GtkTextView {caret-color: rgb(200,200,0);}";
#else
str = "GtkTextView {-GtkWidget-cursor-color: rgb(200,200,0);}";
#endif

gcp= gtk_css_provider_new();
gtk_css_provider_load_from_data(gcp, str, -1, 0);
gtk_style_context_add_provider(gsc, GTK_STYLE_PROVIDER(gcp), 
    GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);


str =  "GtkTextView {-GtkWidget-cursor-aspect-ratio: 0.1;}";
gcp= gtk_css_provider_new();
gtk_css_provider_load_from_data(gcp, str, -1, 0);
gtk_style_context_add_provider(gsc, GTK_STYLE_PROVIDER(gcp), 
    GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
       
} 
#endif
  show_verses ();
  return pos;
}

void
add_verse (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *project = Denemo.project;
  DenemoMovement *movement = project->movement;
  if (project->movement->currentstaff)
    {
      DenemoStaff *staff = movement->currentstaff->data;
      add_verse_to_staff (movement, staff);
      signal_structural_change (project);
      GtkTextView *verse_view = (GtkTextView *) verse_get_current_view (staff);
      gtk_widget_show (GTK_WIDGET (verse_view));
    }
}

void
delete_verse (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  DenemoMovement *si = gui->movement;
  if (si->currentstaff)
    {
      DenemoStaff *staff = si->currentstaff->data;
      if (staff->verses && staff->verse_views)
        {
          GtkTextView *verse_view = (GtkTextView *) verse_get_current_view (staff);
          gint versenum = verse_get_current (staff);
          gchar *verse_text = verse_get_current_text (staff);
         
          staff->verse_views = g_list_remove (staff->verse_views, verse_view);
          staff->verses = g_list_remove (staff->verses, verse_text);

          if (staff->verse_views == NULL)
            {
              staff->current_verse_view = NULL;
              gtk_widget_destroy (gtk_widget_get_parent (gtk_widget_get_parent (GTK_WIDGET (verse_view))));//destroy notebook and so notebook->scrolled window->text view
            }
          else
            gtk_widget_destroy (gtk_widget_get_parent (GTK_WIDGET (verse_view)));
          // g_print("Children are %p\n",  gtk_container_get_children (GTK_CONTAINER (gtk_container_get_children (GTK_CONTAINER (si->lyricsbox))->data)));
          if (!verse_set_current (staff, versenum-1))
            staff->current_verse_view = NULL;// no verses left
          signal_structural_change (gui);
          score_status (gui, TRUE);
          draw_score_area ();
            
        }
    }
}

gchar *
get_text_from_view (GtkWidget * textview)
{
  GtkTextIter startiter, enditer;
  GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (textview));
  gtk_text_buffer_get_start_iter (GTK_TEXT_BUFFER (buffer), &startiter);
  gtk_text_buffer_get_end_iter (GTK_TEXT_BUFFER (buffer), &enditer);
  return gtk_text_buffer_get_text (GTK_TEXT_BUFFER (buffer), &startiter, &enditer, FALSE);
}


// For the first call a textview is passed and the count'th syllable in that textview is set to be the next syllable returned.
// Subsequent calls with NULL for textview return the next syllable of the textview that was set up by the above
static gchar *
lyric_iterator (GtkWidget * textview, gint count)
{
  static const gchar *next;
  static gchar *lyrics;
  static GString *gs;
  if (gs == NULL)
    gs = g_string_new ("");
  if (textview == NULL)
    {
      gboolean result = scan_syllable ((gchar **) & next, gs);
      if (result && gs->len)
        return gs->str;
      else
        return NULL;
    }
  if (textview != DummyVerse)
    {
      SkipCount = 0;
      if (lyrics)
        g_free (lyrics);
      lyrics = get_text_from_view (textview);
      next = lyrics;
      while (count--)
        scan_syllable ((gchar **) & next, gs);
    }
  return NULL;
}

//for every chord while drawing next_syllable is called.
gchar *
next_syllable (void)
{
  return lyric_iterator (NULL, 0);
}

/* reset_lyrics sets up the lyric iterator so that a call to next_syllable() will return the count'th syllable */
void
reset_lyrics (DenemoStaff * staff, gint count)
{
  if (Denemo.non_interactive)
    return;

  if (DummyVerse == NULL)
    DummyVerse = gtk_text_view_new ();
  GtkTextView *verse_view = (GtkTextView *) verse_get_current_view (staff);
  if (staff && verse_view)
    lyric_iterator (GTK_WIDGET (verse_view), count);
  else
    lyric_iterator (DummyVerse, count);
}

void
install_lyrics_preview (DenemoMovement * si, GtkWidget * top_vbox)
{
  if (Denemo.non_interactive)
    return;
  GtkWidget *parent = gtk_widget_get_parent (top_vbox);

  if (si->lyricsbox == NULL)
    si->lyricsbox = (GtkWidget *) gtk_vbox_new (FALSE, 1);      //box to hold notebook of textview widgets
  if (parent)
    {
      if (!gtk_paned_get_child2 (GTK_PANED (parent)))
        {
          GtkWidget *vbox = (GtkWidget *) gtk_vbox_new (FALSE, 8);
          gtk_paned_pack2 (GTK_PANED (parent), vbox, FALSE, TRUE);      //si->lyricsbox);
          gtk_widget_show (vbox);
        }
      gtk_box_pack_start (GTK_BOX (gtk_paned_get_child2 (GTK_PANED (parent))), si->lyricsbox, TRUE, TRUE, 0);
    }
 // if (Denemo.prefs.lyrics_pane)
    gtk_widget_show (si->lyricsbox);
}

/* hide the notebook of verses for the current staff */
void
hide_lyrics (void)
{
  DenemoProject *gui = Denemo.project;
  if (gui->movement->currentstaff && ((DenemoStaff *) gui->movement->currentstaff->data)->verse_views)
    //hide the notebook
    gtk_widget_hide (gtk_widget_get_parent (gtk_widget_get_parent (((DenemoStaff *) gui->movement->currentstaff->data)->verse_views->data)));
}

/* show the notebook of verses for the current staff hide all others*/
void
show_lyrics (void)
{
  DenemoProject *gui = Denemo.project;
  DenemoMovement *si = gui->movement;
  if (si->currentstaff && ((DenemoStaff *) si->currentstaff->data)->verse_views)
    //show the notebook
    gtk_widget_show (gtk_widget_get_parent (gtk_widget_get_parent (((DenemoStaff *) si->currentstaff->data)->verse_views->data)));
  select_lyrics ();
}

/* hide the notebooks of verses for the non-current staffs */
void
select_lyrics (void)
{
  DenemoProject *gui = Denemo.project;
  DenemoMovement *si = gui->movement;
  GList *current = si->thescore;
  for (; current; current = current->next)
    {
      if (current != si->currentstaff && ((DenemoStaff *) current->data)->verse_views)
        //hide the notebook
        gtk_widget_hide (gtk_widget_get_parent (gtk_widget_get_parent (((DenemoStaff *) current->data)->verse_views->data)));
    }
}

gchar *
get_lyrics_for_current_verse (DenemoStaff * thestaff)
{
  GtkTextView *verse_view = (GtkTextView *) verse_get_current_view (thestaff);
  if (verse_view)
    return get_text_from_view (GTK_WIDGET (verse_view));
  else
    return NULL;
}

gboolean
append_lyrics_for_current_verse (DenemoStaff * thestaff, gchar * text)
{
  GtkTextView *verse_view = (GtkTextView *) verse_get_current_view (thestaff);
  if (verse_view)
    {
      GtkTextIter iter;
      GtkTextBuffer *textbuffer = gtk_text_view_get_buffer (verse_view);
      gtk_text_buffer_get_end_iter (GTK_TEXT_BUFFER (textbuffer), &iter);
      gtk_text_buffer_insert (textbuffer, &iter, text, -1);
      return TRUE;
    }
  else
    return FALSE;
}

gboolean
put_lyrics_for_current_verse (DenemoStaff * thestaff, gchar * text)
{
  GtkTextView *verse_view = (GtkTextView *) verse_get_current_view (thestaff);
  if (verse_view)
    {
      GtkTextBuffer *textbuffer = gtk_text_view_get_buffer (verse_view);
      GtkTextIter startiter, enditer;
      gtk_text_buffer_get_start_iter (GTK_TEXT_BUFFER (textbuffer), &startiter);
      gtk_text_buffer_get_end_iter (GTK_TEXT_BUFFER (textbuffer), &enditer);
      gtk_text_buffer_delete (textbuffer, &startiter, &enditer);
      gtk_text_buffer_get_end_iter (GTK_TEXT_BUFFER (textbuffer), &enditer);
      gtk_text_buffer_insert (textbuffer, &enditer, text, -1);
      return TRUE;
    }
  else
    return FALSE;
}

gchar *
get_lyrics_for_verse_num (gint number)
{
  DenemoProject *gui = Denemo.project;
  if (gui->movement->currentstaff)
    {
      DenemoStaff *thestaff = ((DenemoStaff *) gui->movement->currentstaff->data);
      if (thestaff->verses)
        {
          GList *verse = g_list_nth (thestaff->verses, number - 1);
          if (verse)
            return verse->data;
        }
    }
  return NULL;
}
