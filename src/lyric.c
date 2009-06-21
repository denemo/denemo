/* lyric.c
 *
 * Functions for the manipulations of lyrics
 *
 * for Denemo, a gtk+ frontend for GNU Lilypond
 * (c)2009 Richard Shann
 */

#include "objops.h"
#include "staffops.h"
#include "utils.h"

static GtkWidget *DummyVerse;/* a non-existent verse */

static void lyric_keypress(GtkWidget *w, GdkEventKey *event) {
DenemoGUI *gui = Denemo.gui;
gtk_widget_queue_draw (gui->scorearea);
}

static GtkWidget *new_lyric_editor(void) {
 GtkWidget *view = gtk_text_view_new ();
 g_signal_connect (G_OBJECT (view), "key-press-event",
		   G_CALLBACK (lyric_keypress), NULL);
 return view;
}

DenemoObject *
newlyric (gint baseduration, gint numdots, gchar *lys) {
  g_warning("Not implemented");
  return NULL;
}

void add_verse_to_staff(DenemoStaff *staff) {
  staff->verses = g_list_append(staff->verses, new_lyric_editor());
  staff->currentverse = g_list_last(staff->verses);
}

void add_verse(GtkAction *action, gpointer param) {
DenemoGUI *gui = Denemo.gui;
DenemoScore *si = gui->si;
 if(gui->si->currentstaff) 
   add_verse_to_staff((DenemoStaff *) si->currentstaff->data);
}

gchar *get_text_from_view(GtkWidget *textview) {
  GtkTextIter startiter, enditer; 
  GtkTextBuffer *buffer = gtk_text_view_get_buffer (textview);    
  gtk_text_buffer_get_start_iter (buffer, &startiter);
  gtk_text_buffer_get_end_iter (buffer, &enditer);
  return gtk_text_buffer_get_text (buffer, &startiter, &enditer, FALSE);
}



static gchar *lyric_iterator(GtkWidget *textview) {
  static  gchar *next;
  static gchar *lyrics;
  static GString *gs;
  if(gs==NULL)
    gs = g_string_new("");
  if(textview==NULL) {
    g_print("next = %p, %s\n", next, next);
    pango_scan_string(&next, gs);
    g_print("after next = %p, %s\n", next, next);
    if(gs->len)
      return gs->str;
    else
      return NULL;
  }
  if(textview != DummyVerse) {
    //FIXME free lyrics
    lyrics = get_text_from_view(textview);
    next = lyrics;
 }
  return NULL;
}

gchar *next_syllable(void) {
  return lyric_iterator(NULL);
}

void init_lyrics(DenemoStaff *staff) {
  if(DummyVerse==NULL)
    DummyVerse = gtk_text_view_new();
  if(staff->currentverse)
    lyric_iterator(staff->currentverse->data);
  else lyric_iterator(DummyVerse);
}

