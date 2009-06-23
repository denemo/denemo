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

static gboolean lyric_keypress(GtkWidget *w, GdkEventKey *event) {
DenemoGUI *gui = Denemo.gui;
gtk_widget_queue_draw (gui->scorearea);
 return FALSE;
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

GtkWidget * add_verse_to_staff(DenemoScore *si, DenemoStaff *staff) {
  GtkWidget *notebook, *textview;
  if(staff->verses==NULL) {
    notebook = gtk_notebook_new();
    gtk_widget_show(notebook);
    if(si->lyricsbox==NULL)
      install_lyrics_preview(si, gtk_widget_get_parent(gtk_widget_get_parent(Denemo.gui->scorearea)));//FIXME we need a proper way of getting to the top vbox, that will not break when scorearea is moved in the widget hierarchy.
    gtk_box_pack_start(si->lyricsbox, notebook, TRUE, TRUE, 0);
  } else {
    GtkWidget *w = staff->verses->data;
    notebook = gtk_widget_get_parent(w);
  }
  if(staff->currentverse)
    gtk_widget_hide(staff->currentverse->data);
  textview = new_lyric_editor();
  gtk_widget_show(textview);
  staff->verses = g_list_append(staff->verses, textview);
  staff->currentverse = g_list_last(staff->verses);
  gint pagenum = gtk_notebook_append_page (GTK_NOTEBOOK (notebook), textview, NULL);
  gtk_notebook_set_current_page (GTK_NOTEBOOK(notebook), pagenum);
  if(pagenum)
    gtk_notebook_set_show_tabs (GTK_NOTEBOOK(notebook), TRUE);
  // g_print("pagenum is %d\n", pagenum);
  // gtk_widget_show_all(si->lyricsbox);
  return textview;
}

void add_verse(GtkAction *action, gpointer param) {
DenemoGUI *gui = Denemo.gui;
DenemoScore *si = gui->si;
 if(gui->si->currentstaff) 
   add_verse_to_staff(si, (DenemoStaff *) si->currentstaff->data);
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
    pango_scan_string(&next, gs);
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


/* rename reset_lyrics */
void reset_lyrics(DenemoStaff *staff) {
  if(DummyVerse==NULL)
    DummyVerse = gtk_text_view_new();
  if(staff && staff->currentverse)
    lyric_iterator(staff->currentverse->data);
  else lyric_iterator(DummyVerse);
}



void install_lyrics_preview(DenemoScore *si, GtkWidget *top_vbox){ 
  if(si->lyricsbox==NULL)
    si->lyricsbox = gtk_vbox_new (FALSE, 1);//box to hold notebook of textview widgets
  gtk_box_pack_start (GTK_BOX (top_vbox), si->lyricsbox, TRUE, TRUE, 0);
  //gtk_widget_show(si->lyricsbox);
}

/* hide/show lyrics for current staff */
hide_lyrics(void) {
DenemoGUI *gui = Denemo.gui;
DenemoScore *si = gui->si;
 if(gui->si->currentstaff && ((DenemoStaff *)gui->si->currentstaff->data)->currentverse)
   gtk_widget_hide(((DenemoStaff *)gui->si->currentstaff->data)->currentverse->data);
}

show_lyrics(void) {
DenemoGUI *gui = Denemo.gui;
DenemoScore *si = gui->si;
 if(gui->si->currentstaff && ((DenemoStaff *)gui->si->currentstaff->data)->currentverse)
   gtk_widget_show(((DenemoStaff *)gui->si->currentstaff->data)->currentverse->data);
}
