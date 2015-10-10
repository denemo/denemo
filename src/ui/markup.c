/* markup.c
 * dialog for getting markup from user
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2015  Richard Shann
 * 
 */


#include "printview/printview.h"
#include "core/view.h"
#include "scripting/scheme-callbacks.h"

#define SECTION_UTF8_STRING ""
#define PILCROW_UTF8_STRING "¶"


static gchar *
create_lilypond_from_text (gchar * orig)
{
  gchar *text = g_strdup (orig);
  GString *ret = g_string_new ("\\line\\large{");
  g_debug ("looking at %s\n", text);
  gunichar section = g_utf8_get_char (PILCROW_UTF8_STRING);
  gchar *this;
  for(this = text;*this;this = g_utf8_next_char (this))
    {
        gunichar thechar = g_utf8_get_char (this);
        if (thechar == SECTION_UTF8_STRING) continue;//don't show old paragraph marks used in previous version.
        if (thechar == section)
            {
              g_string_append (ret, "}\\line\\large{");
            } else 
            {
                g_string_append_printf (ret, "%c", *this);
            }
    }
  g_string_append (ret, "}\n");
  g_free (text);
  return g_string_free (ret, FALSE);
}
static void
paste_snippet_lilypond (GtkWidget * button)
{
  DenemoProject *gui = Denemo.project;
  GtkWidget *hbox = gtk_widget_get_parent (button);
  GtkWidget *textbuffer = (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textbuffer");
  if (textbuffer)
    {
      RhythmPattern *r = (gui->currhythm) ? ((RhythmPattern *) gui->currhythm->data) : NULL;
      if (r)
        {
          const gchar *clefname = get_prevailing_clef_as_lilypond (), *keysigname = get_prevailing_keysig_as_lilypond (), *timesigname = get_prevailing_timesig_as_lilypond ();
          gchar *text = g_strdup_printf ("\\raise #0.5 \\score{\n\\DenemoGlobalTranspose {{%s}{%s}{%s}%s}\\layout{indent=0.0}\n}", clefname, keysigname, timesigname, r->lilypond?r->lilypond->str:"");
          gtk_text_buffer_insert_at_cursor (GTK_TEXT_BUFFER (textbuffer), text, -1 /*gint len */ );
          g_free (text);
        }
    }
  else
    {
      g_warning ("Denemo program error, widget hierarchy changed???");
    }
  GtkWidget *textview = (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textview");
  gtk_widget_grab_focus (textview);
}
static void
paste_current_lilypond_as_fakechord (GtkWidget * button)
{
  DenemoProject *gui = Denemo.project;
  GtkWidget *hbox = gtk_widget_get_parent (button);
  GtkWidget *textbuffer = (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textbuffer");
  if (textbuffer)
    {
      gchar *text = get_fakechord_as_markup ();
      if(text)
        {
            gchar *insert = g_strdup_printf("%s", text);   
            gtk_text_buffer_insert_at_cursor (GTK_TEXT_BUFFER (textbuffer), insert, -1 /*gint len */ );
            g_free (text);
            g_free (insert);
        }
    }
  else
    {
      g_warning ("Denemo program error, widget hierarchy changed???");
    }
  GtkWidget *textview = (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textview");
  gtk_widget_grab_focus (textview);
}
static void
paste_current_lilypond_as_fretdiagram (GtkWidget * button)
{
  DenemoProject *gui = Denemo.project;
  GtkWidget *hbox = gtk_widget_get_parent (button);
  GtkWidget *textbuffer = (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textbuffer");
  if (textbuffer)
    {
      gchar *text = get_fretdiagram_as_markup ();
      if(text)
        {
            gchar *insert = g_strdup_printf("%s", text);   
            gtk_text_buffer_insert_at_cursor (GTK_TEXT_BUFFER (textbuffer), insert, -1 /*gint len */ );
            g_free (text);
            g_free (insert);
        }
    }
  else
    {
      g_warning ("Denemo program error, widget hierarchy changed???");
    }
  GtkWidget *textview = (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textview");
  gtk_widget_grab_focus (textview);
}

static void
insert_markup (GtkWidget * button, gchar *text)
{
  DenemoProject *gui = Denemo.project;
  GtkWidget *hbox = gtk_widget_get_parent (button);
  GtkWidget *textbuffer = (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textbuffer");
  if (textbuffer)
          gtk_text_buffer_insert_at_cursor (GTK_TEXT_BUFFER (textbuffer), text, -1);
   else
    {
      g_warning ("Denemo program error, widget hierarchy changed???");
    }      
 GtkWidget *textview = (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textview");
 gtk_widget_grab_focus (textview);
}

static void
insert_font_mag (GtkWidget * button)
{
  DenemoProject *gui = Denemo.project;
  GtkWidget *hbox = gtk_widget_get_parent (button);
  GtkWidget *textbuffer = (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textbuffer");
  gchar *text = string_dialog_entry (gui, _( "Font Magnification"), _("Give a relative font size +/- "), "-2");
  if (text && *text && atoi(text))
    {
        gchar *out = g_strdup_printf ("\\fontsize #%s ", text);
      if (textbuffer)
              gtk_text_buffer_insert_at_cursor (GTK_TEXT_BUFFER (textbuffer), out, -1);
       else
        {
          g_warning ("Denemo program error, widget hierarchy changed???");
        }      
     GtkWidget *textview = (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textview");
     gtk_widget_grab_focus (textview);
     g_free (out);
    }
 g_free (text);
}
static void
insert_vert (GtkWidget * button)
{
  DenemoProject *gui = Denemo.project;
  GtkWidget *hbox = gtk_widget_get_parent (button);
  GtkWidget *textbuffer = (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textbuffer");
  gchar *text = string_dialog_entry (gui, _( "Space Above"), _("Give space to leave above +/- "), "2");
  if (text && *text && atoi(text))
    {
        gchar *out = g_strdup_printf ("\\vspace #%s ", text);
      if (textbuffer)
              gtk_text_buffer_insert_at_cursor (GTK_TEXT_BUFFER (textbuffer), out, -1);
       else
        {
          g_warning ("Denemo program error, widget hierarchy changed???");
        }      
     GtkWidget *textview = (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textview");
     gtk_widget_grab_focus (textview);
     g_free (out);
    }
 g_free (text);
}

static void
insert_horiz (GtkWidget * button)
{
  DenemoProject *gui = Denemo.project;
  GtkWidget *hbox = gtk_widget_get_parent (button);
  GtkWidget *textbuffer = (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textbuffer");
  gchar *text = string_dialog_entry (gui, _( "Insert Space"), _("Give space to insert +/- "), "2");
  if (text && *text && atoi(text))
    {
        gchar *out = g_strdup_printf ("\\hspace #%s ", text);
      if (textbuffer)
              gtk_text_buffer_insert_at_cursor (GTK_TEXT_BUFFER (textbuffer), out, -1);
       else
        {
          g_warning ("Denemo program error, widget hierarchy changed???");
        }      
     GtkWidget *textview = (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textview");
     gtk_widget_grab_focus (textview);
     g_free (out);
    }
 g_free (text);
}
static void
preview_markup (GtkWidget * button)
{
    DenemoProject *gui = Denemo.project;
    GtkWidget *hbox = gtk_widget_get_parent (button);
    GtkWidget *textbuffer = (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textbuffer");     
    GtkTextIter startiter, enditer;
    gtk_text_buffer_get_start_iter (GTK_TEXT_BUFFER(textbuffer), &startiter);
    gtk_text_buffer_get_end_iter (GTK_TEXT_BUFFER(textbuffer), &enditer);
    gchar *text = gtk_text_buffer_get_text (GTK_TEXT_BUFFER(textbuffer), &startiter, &enditer, FALSE);
    gchar *lilypond = create_lilypond_from_text (text);//g_print("At this point lilypond is <<<%s>>>\n\n\n", lilypond);
    gchar *syntax = g_strconcat (LILYPOND_SYMBOL_DEFINITIONS, " \\markup \\column {",lilypond," }", NULL);
    create_pdf_for_lilypond (syntax);
    g_free (syntax);
    g_free (lilypond);
}

static gboolean keypress_callback (G_GNUC_UNUSED GtkWidget * w, GdkEventKey * event, GtkTextBuffer *textbuffer)
{
  DenemoProject *gui = Denemo.project;
  GtkTextIter cursor;
  if (event->keyval == GDK_KEY_Return)
    {
  //gchar *key = g_strdup_printf ("%c", gdk_keyval_to_unicode (event->keyval));
  gtk_text_buffer_get_iter_at_mark (textbuffer, &cursor, gtk_text_buffer_get_insert (textbuffer));
  gtk_text_buffer_insert (textbuffer, &cursor, "\n""¶", -1);
  //g_print ("Got %s\n", key);
 // g_free (key);
  return TRUE;
   }
  if ('#' == gdk_keyval_to_unicode (event->keyval))
      {
          gdk_beep();
          g_warning ("The character # can only be used for scheme code, paste it in if needed");
          return TRUE;
      } 
  return FALSE; //pass it on to the standard handler.
 } 

gchar *get_user_markup (GString *user_text, GString *marked_up_text, gchar* title, char *instruction, gchar *initial_value, gboolean modal, gboolean format_only)
{
  implement_show_print_view (FALSE);
  GtkWidget *hbox = gtk_hbox_new (FALSE, 8);
  GtkWidget *button = gtk_button_new_with_label (_("Paste Current Snippet"));
  gtk_widget_set_tooltip_text (button, _("Pastes the music captured in the currently selected Snippet into the text at the cursor.\nThe music appears here in the LilyPond syntax.\nIt will print as typeset music embedded in the sentence you are writing.\nYou can edit the syntax following the LilyPond syntax.\n"));

  g_signal_connect (button, "clicked", G_CALLBACK (paste_snippet_lilypond), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  button = gtk_button_new_with_label (_("Next Snippet"));
  gtk_widget_set_tooltip_text (button, _("Makes the next Snippet the one that can be pasted.\nTo see the music snippets you need to check View → Snippets\nThe one selected is in bold black."));
  GtkAction *action = gtk_ui_manager_get_action (Denemo.ui_manager, "/ObjectMenu/NotesRests/SelectDuration/NextRhythm");
  if (action)
    g_signal_connect_swapped (button, "clicked", G_CALLBACK (gtk_action_activate), action);
  else
    gtk_widget_set_sensitive (button, FALSE);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  
  
  button = gtk_button_new_with_label (_("Paste Note Name/Chord Symbol"));
  gtk_widget_set_tooltip_text (button, _("Pastes the note or chord at the cursor as a Note Name/Chord Symbol\n"
    "The music appears here in the LilyPond syntax.\n"
    "It will print as note name/chord symbol in the sentence you are writing, transposed according to the global transposition set.\n"
    "Use, for example, to specify the key of a piece.\n"));

  g_signal_connect (button, "clicked", G_CALLBACK (paste_current_lilypond_as_fakechord), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label (_("Paste Fret Diagram"));
  gtk_widget_set_tooltip_text (button, _("Pastes the chord at the cursor as a Fret Diagram\n"
    "The music appears here in the LilyPond syntax.\n"
    "It will print as fret diagram in the sentence you are writing, transposed according to the global transposition set.\n"));

  g_signal_connect (button, "clicked", G_CALLBACK (paste_current_lilypond_as_fretdiagram), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  

  button = gtk_button_new_with_label (_("Bold"));
  gtk_widget_set_tooltip_text (button, _("Inserts markup to make the following text bold. Enclose the passage to be bold in {}."));
  g_signal_connect (button, "clicked", G_CALLBACK (insert_markup), "\\bold ");
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  
  button = gtk_button_new_with_label (_("Italic"));
  gtk_widget_set_tooltip_text (button, _("Inserts markup to make the following text italic. Enclose the passage to be italic in {}."));
  g_signal_connect (button, "clicked", G_CALLBACK (insert_markup), "\\italic ");
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label (_("“"));
  gtk_widget_set_tooltip_text (button, _("Inserts code for open quotes - leave a space after this code. Note that this is not the \" character which is used for grouping words not to be treated as markup. The \" marks must be paired or LilyPond will not typeset the music."));
  g_signal_connect (button, "clicked", G_CALLBACK (insert_markup), "\\char ##x201C ");
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label (_("”"));
  gtk_widget_set_tooltip_text (button, _("Inserts code for close quotes - leave a space after this code. Note that this is not the \" character which is used for grouping words  not to be treated as markup. The \" marks must be paired or LilyPond will not typeset the music."));
  g_signal_connect (button, "clicked", G_CALLBACK (insert_markup), "\\char ##x201D ");
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label (_("size"));
  gtk_widget_set_tooltip_text (button, _("Inserts code to change the relative font size."));
  g_signal_connect (button, "clicked", G_CALLBACK (insert_font_mag), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
    button = gtk_button_new_with_label ("⬆");
  gtk_widget_set_tooltip_text (button, _("Inserts code to leave space above this line of text. Ineffective on the top line of standalone text, only titles"));
  g_signal_connect (button, "clicked", G_CALLBACK (insert_vert), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
    button = gtk_button_new_with_label (_("⬌"));
  gtk_widget_set_tooltip_text (button, _("Inserts code to insert space at the cursor."));
  g_signal_connect (button, "clicked", G_CALLBACK (insert_horiz), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  
  button = gtk_button_new_with_label (_("Preview"));
  gtk_widget_set_tooltip_text (button, _("Shows what the text will look like when typeset in the Print View window. For score and movement titles the appearance is correct only relative to the default title."));
  g_signal_connect (button, "clicked", G_CALLBACK (preview_markup), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  gchar *text;
    if(format_only)
        text = g_strdup (initial_value); //anything else, just format the passed in string
    else
        text = string_dialog_editor_with_widget_opt (Denemo.project, title, instruction, initial_value, hbox, modal, keypress_callback);    
    
 if (text)
    {
      gchar *lilypond = create_lilypond_from_text (text);
      g_string_assign (user_text, text);
      g_string_assign (marked_up_text, lilypond);
      g_free (text);
      g_free (lilypond);
      return TRUE;
    }
  else
    return FALSE;
 }   
