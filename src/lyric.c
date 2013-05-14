/* lyric.c
 *
 * Functions for the manipulations of lyrics
 *
 * for Denemo, a gtk+ frontend for GNU Lilypond
 * (c)2009 Richard Shann
 */
#include <string.h>             /* for strcmp() */
#include "lyric.h"
#include "objops.h"
#include "staffops.h"
#include "utils.h"

static GtkWidget *DummyVerse;   /* a non-existent verse */

gboolean
lyric_change (GtkTextBuffer * buffer)
{
  DenemoGUI *gui = Denemo.gui;
  score_status (gui, TRUE);
  gtk_widget_queue_draw (Denemo.scorearea);
  return FALSE;
}

static GtkWidget *
new_lyric_editor (void)
{
  GtkWidget *view = gtk_text_view_new ();


  GtkWidget *sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  if (Denemo.prefs.newbie)
    gtk_widget_set_tooltip_text (sw, _("The text of a verse can be typed or pasted here. The space between syllables determines which syllable goes beneath which note(s).\nNew lines have no special significance for this. Slurs on notes make them take only one syllable. Use the underscore _ for blank syllables and the hyphen - for extending syllables."));
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
  gtk_widget_queue_draw (Denemo.scorearea);
  staff->currentverse = g_list_nth (staff->verses, pagenum);
}

GtkWidget *
add_verse_to_staff (DenemoScore * si, DenemoStaff * staff)
{
  GtkWidget *notebook, *textview;
  if (staff->verses == NULL)
    {
      notebook = gtk_notebook_new ();
      gtk_widget_show (notebook);
      g_signal_connect (G_OBJECT (notebook), "switch_page", G_CALLBACK (switch_page), staff);
      if (si->lyricsbox == NULL)
        install_lyrics_preview (si, gtk_widget_get_parent (gtk_widget_get_parent (Denemo.scorearea)));  //FIXME we need a proper way of getting to the top vbox, that will not break when scorearea is moved in the widget hierarchy.
      gtk_box_pack_start (GTK_BOX (si->lyricsbox), notebook, TRUE, TRUE, 0);
      if (si->measurewidth == DENEMO_INITIAL_MEASURE_WIDTH)
        si->measurewidth = DENEMO_INITIAL_MEASURE_WIDTH * 3;
    }
  else
    {
      GtkWidget *w = staff->verses->data;
      notebook = gtk_widget_get_parent (gtk_widget_get_parent (w));
    }
  // if(staff->currentverse)    
  //  gtk_widget_hide(staff->currentverse->data); 
  //if(si->currentstaff && si->currentstaff->data == staff)
  //  gtk_widget_show(staff->currentverse->data);
  textview = new_lyric_editor ();
  gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (textview), GTK_WRAP_WORD_CHAR);
  gtk_widget_show_all (gtk_widget_get_parent (textview));
  staff->verses = g_list_append (staff->verses, textview);
  staff->currentverse = g_list_last (staff->verses);
  //  g_print("Setting verse to %p\n", staff->currentverse);
  gint pagenum = gtk_notebook_append_page (GTK_NOTEBOOK (notebook), gtk_widget_get_parent (textview), NULL);
  gtk_notebook_set_current_page (GTK_NOTEBOOK (notebook), pagenum);
  gchar *tablabel = g_strdup_printf ("Verse %d", pagenum + 1);
  gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (notebook), gtk_widget_get_parent (textview), tablabel);
  g_free (tablabel);
  if (pagenum)
    gtk_notebook_set_show_tabs (GTK_NOTEBOOK (notebook), TRUE);
  return textview;
}

void
add_verse (GtkAction * action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoScore *si = gui->si;
  if (gui->si->currentstaff)
    {
      DenemoStaff *staff = si->currentstaff->data;
      add_verse_to_staff (si, staff);
      signal_structural_change (gui);
      gtk_widget_show (staff->currentverse->data);
      g_signal_connect (G_OBJECT (gtk_text_view_get_buffer (staff->currentverse->data)), "changed", G_CALLBACK (lyric_change), NULL);
    }
}

void
delete_verse (GtkAction * action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoScore *si = gui->si;
  if (si->currentstaff)
    {
      DenemoStaff *staff = si->currentstaff->data;
      if (staff->currentverse)
        {
          staff->verses = g_list_remove_link (staff->verses, staff->currentverse);
          gtk_widget_destroy (gtk_widget_get_parent (staff->currentverse->data));
          staff->currentverse = staff->verses;
          signal_structural_change (gui);
          score_status (gui, TRUE);
          gtk_widget_queue_draw (Denemo.scorearea);
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

gboolean
scan_syllable (gchar ** next, GString * gs)
{
  gboolean result;
  result = pango_scan_string ((const char **) next, gs);
  if (result && (*gs->str == '\\') && (*(gs->str + 1) != '\\') && (*(gs->str + 1) != '\"'))
    {
      while (**next && **next != '\n')
        (*next)++;              //skip to end of line
      return scan_syllable (next, gs);
    }
  if (result && ((!strcmp (gs->str, "--") || (!strcmp (gs->str, "__")))))
    return scan_syllable (next, gs);
  return result;
}

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
      if (lyrics)
        g_free (lyrics);
      lyrics = get_text_from_view (textview);
      next = lyrics;
      while (count--)
        scan_syllable ((gchar **) & next, gs);
    }
  return NULL;
}


gchar *
next_syllable (gint count)
{
  return lyric_iterator (NULL, count);
}


/* rename reset_lyrics */
void
reset_lyrics (DenemoStaff * staff, gint count)
{
  if (DummyVerse == NULL)
    DummyVerse = gtk_text_view_new ();
  if (staff && staff->currentverse)
    lyric_iterator (staff->currentverse->data, count);
  else
    lyric_iterator (DummyVerse, count);
}



void
install_lyrics_preview (DenemoScore * si, GtkWidget * top_vbox)
{
  if (si->lyricsbox == NULL)
    si->lyricsbox = gtk_vbox_new (FALSE, 1);    //box to hold notebook of textview widgets
  gtk_box_pack_start (GTK_BOX (top_vbox), si->lyricsbox, FALSE, TRUE, 0);
  if (Denemo.prefs.lyrics_pane)
    gtk_widget_show (si->lyricsbox);
}

/* hide the notebook of verses for the current staff */
void
hide_lyrics (void)
{
  DenemoGUI *gui = Denemo.gui;
  if (gui->si->currentstaff && ((DenemoStaff *) gui->si->currentstaff->data)->verses)
    gtk_widget_hide (gtk_widget_get_parent (gtk_widget_get_parent (((DenemoStaff *) gui->si->currentstaff->data)->verses->data)));      //hide the notebook
}

/* show the notebook of verses for the current staff hide all others*/
void
show_lyrics (void)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoScore *si = gui->si;
  if (si->currentstaff && ((DenemoStaff *) si->currentstaff->data)->verses)
    gtk_widget_show (gtk_widget_get_parent (gtk_widget_get_parent (((DenemoStaff *) si->currentstaff->data)->verses->data)));   //show the notebook
  select_lyrics ();
}

/* hide the notebooks of verses for the non-current staffs */
void
select_lyrics (void)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoScore *si = gui->si;
  GList *current = si->thescore;
  for (; current; current = current->next)
    {
      if (current != si->currentstaff && ((DenemoStaff *) current->data)->verses)
        gtk_widget_hide (gtk_widget_get_parent (gtk_widget_get_parent (((DenemoStaff *) current->data)->verses->data)));        //hide the notebook
    }
}

gchar *
get_lyrics_for_current_verse (DenemoStaff * thestaff)
{
  if (thestaff->currentverse && thestaff->currentverse->data)
    return get_text_from_view (thestaff->currentverse->data);
  else
    return NULL;
}

gboolean
append_lyrics_for_current_verse (DenemoStaff * thestaff, gchar * text)
{
  if (thestaff->currentverse && thestaff->currentverse->data)
    {
      GtkTextIter iter;
      GtkTextBuffer *textbuffer = gtk_text_view_get_buffer (thestaff->currentverse->data);
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
  if (thestaff->currentverse && thestaff->currentverse->data)
    {
      GtkTextBuffer *textbuffer = gtk_text_view_get_buffer (thestaff->currentverse->data);
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

#ifdef VERSE_NAVIGATION_CODE
//return the verse number for the current verse (starting at 1) or 0 if none
gint
get_current_verse_number (void)
{
  DenemoGUI *gui = Denemo.gui;
  if (gui->si->currentstaff)
    {
      DenemoStaff *thestaff = ((DenemoStaff *) gui->si->currentstaff->data);
      if (thestaff->verses)
        return 1 + g_list_index (thestaff->verses, thestaff->currentverse);

    }
}

return 0;
}

gboolean
set_current_verse (gint number)
{
  DenemoGUI *gui = Denemo.gui;
  if (gui->si->currentstaff)
    {
      DenemoStaff *thestaff = ((DenemoStaff *) gui->si->currentstaff->data);
      if (thestaff->verses)
        {
          GList *g = g_list_nth (thestaff->verses, number - 1);
          if (g)
            {
              thestaff->currentverse = g;
            emit_signal ("switch-page", ....return TRUE;}
                         }
                         }
#endif

                         gchar * get_lyrics_for_verse_num (gint number)
                         {
                         DenemoGUI * gui = Denemo.gui; if (gui->si->currentstaff)
                         {
                         DenemoStaff * thestaff = ((DenemoStaff *) gui->si->currentstaff->data); if (thestaff->verses)
                         {
                         GList * verse = g_list_nth (thestaff->verses, number - 1); if (verse) return get_text_from_view (verse->data);}
                         }
                         return NULL;}
