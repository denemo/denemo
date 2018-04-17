/* markup.c
 * dialog for getting markup from user
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2015  Richard Shann
 *
 */


#include "printview/printview.h"
#include "printview/markupview.h"
#include "core/view.h"
#include "scripting/scheme-callbacks.h"
#include "core/menusystem.h"

#define SECTION_UTF8_STRING "§"
#define PILCROW_UTF8_STRING "¶"

static gint changes = 0;
static gint TimerId = 0;
static gboolean run_preview (GtkWidget *textbuffer);

static gchar *
create_lilypond_from_text (gchar * text)
{
  GString *ret = g_string_new ("\\line{");
  gunichar section = g_utf8_get_char (PILCROW_UTF8_STRING);
  gchar *this;
  for(this = text;*this; this = g_utf8_next_char (this))
    {
        gunichar thechar = g_utf8_get_char (this);
        if (thechar == g_utf8_get_char (SECTION_UTF8_STRING))
            continue;//don't show old paragraph marks used in previous version.
        if (thechar == section)
            {
              g_string_append (ret, "}\\line{");
            } else
            {
                gchar *end = g_utf8_offset_to_pointer  (this, 1);
                gchar val = *end;
                *end = '\0';
                g_string_append_printf (ret, "%s", this);
                *end = val;
            }
    }
  g_string_append (ret, "}\n");
  return g_string_free (ret, FALSE);
}

static GtkWidget *get_textbuffer_from_button (GtkWidget *button) {
    GtkWidget *hbox = gtk_widget_get_parent (gtk_widget_get_parent (button)); 
    return (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textbuffer");
}
static GtkWidget *get_textbuffer_from_menuitem (GtkWidget *menuitem) {
    GtkWidget *hbox = gtk_widget_get_parent  (menuitem); 
    return (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textbuffer");
}
//static GtkWidget *get_textview_from_button (GtkWidget *button) {
//    GtkWidget *hbox = gtk_widget_get_parent (gtk_widget_get_parent (button)); 
//    return (GtkWidget *) g_object_get_data (G_OBJECT (hbox), "textview");
//}
static void
paste_snippet_lilypond (GtkWidget * menuitem)
{
  DenemoProject *gui = Denemo.project;
  
  GtkWidget *textbuffer = get_textbuffer_from_menuitem (menuitem);
  if (textbuffer)
    {
      RhythmPattern *r = (gui->currhythm) ? ((RhythmPattern *) gui->currhythm->data) : NULL;
      if (r)
        {
          const gchar *clefname = get_prevailing_clef_as_lilypond (), *keysigname = get_prevailing_keysig_as_lilypond (), *timesigname = get_prevailing_timesig_as_lilypond ();
          gchar *text = g_strdup_printf ("\\raise #0.5 \\score{\n\\DenemoGlobalTranspose {{%s}{%s}{%s}%s}\\layout{indent=0.0}\\paper{top-margin=0.0 left-margin=0.0}\n}", clefname, keysigname, timesigname, r->lilypond?r->lilypond->str:"");
          gtk_text_buffer_insert_at_cursor (GTK_TEXT_BUFFER (textbuffer), text, -1), changes++;
          g_free (text);
        }
    }
  else
    {
      g_warning ("Denemo program error, widget hierarchy changed???");
    }
 // GtkWidget *textview = get_textview_from_button (button);
 // gtk_widget_grab_focus (textview);
}
/* get a chord symbol for the chord at the cursor or user provided note/chord if not on the chord or at_cursor false */
static gchar *
get_fakechord_as_markup (gchar * font, gboolean at_cursor)
{
  DenemoProject *gui = Denemo.project;
  DenemoObject *curObj;
  gchar *text = NULL;
  if (!Denemo.project || !(Denemo.project->movement))
    return NULL;
    
  if (at_cursor)
    {
      if (Denemo.project->movement->currentobject) 
        {
          curObj = (DenemoObject*) (Denemo.project->movement->currentobject->data);
          gchar *chordnotes = NULL, *title = NULL;
         chordnotes =  get_chord_notes();
            
         if (chordnotes) 
            title = g_strdup_printf ("%s %s", _("Current Chord:"), chordnotes);

            if (gui->lilysync != gui->changecount)
            refresh_lily_cb (NULL, Denemo.project);

            text = curObj->lilypond;
               
            g_free (title);
            g_free (chordnotes);
        }
    else
        warningdialog (_( "Cursor not on chord"));
    } 
  else
        text = notes_choice_dialog (1, NULL, NULL);
  if (text)
    {
      gchar *ret = g_strdup_printf ("\\hspace  #-0.5 \\scale #'(0.975 . 0.975)\\score{\n\\DenemoGlobalTranspose\n\\new ChordNames {\n\\override ChordName.font-name = #'\"%s\"\n%s %%note-name to insert, in Dutch\n}\n\\layout{indent=0.0}}\n", font, text);
      return ret;
    }
  return NULL;
}
static void
paste_current_lilypond_as_fakechord (GtkWidget * menuitem, gboolean at_cursor)
{
  DenemoProject *gui = Denemo.project;
 
  GtkWidget *textbuffer = get_textbuffer_from_menuitem (menuitem);
  if (textbuffer)
    {
    gchar *text = NULL;
    gchar *font = string_dialog_entry (gui, _( "Note/Chord Name"), _("Give a font name "), "Times");
    if (font && *font)
        text = get_fakechord_as_markup (font, at_cursor);
    g_free (font);
    if(text)
        {
            gchar *insert = g_strdup_printf("%s", text);
            gtk_text_buffer_insert_at_cursor (GTK_TEXT_BUFFER (textbuffer), text, -1), changes++;
            g_free (text);
            g_free (insert);
        }
    }
    else
    {
      g_warning ("Denemo program error, widget hierarchy changed???");
    }
}
static void
paste_current_lilypond_as_fretdiagram (GtkWidget * menuitem)
{
  DenemoProject *gui = Denemo.project;
  
  GtkWidget *textbuffer = get_textbuffer_from_menuitem (menuitem);
  if (textbuffer)
    {
      gchar *text = get_fretdiagram_as_markup ();
      if(text)
        {
            gchar *insert = g_strdup_printf("%s", text);
            gtk_text_buffer_insert_at_cursor (GTK_TEXT_BUFFER (textbuffer), text, -1), changes++;
            g_free (text);
            g_free (insert);
        }
    }
  else
    {
      g_warning ("Denemo program error, widget hierarchy changed???");
    }
}

static void
insert_markup (GtkWidget * menuitem, gchar *text)
{
  DenemoProject *gui = Denemo.project;
  
  GtkWidget *textbuffer = get_textbuffer_from_menuitem (menuitem);
  if (textbuffer)
    { 
        GtkTextIter cursor;
        gtk_text_buffer_insert_at_cursor (GTK_TEXT_BUFFER (textbuffer), text, -1), changes++;
        gtk_text_buffer_get_iter_at_mark (GTK_TEXT_BUFFER (textbuffer), &cursor, gtk_text_buffer_get_insert (GTK_TEXT_BUFFER(textbuffer)));
        gtk_text_buffer_insert_with_tags_by_name (GTK_TEXT_BUFFER (textbuffer), &cursor, " ",    -1,  "ineditable",NULL);
    }
  else
    g_warning ("Denemo program error, widget hierarchy changed???");
}

//static GtkTextIter StartSelection, EndSelection; 
static void
markup_selection (GtkWidget * button, gchar *text)
{
  DenemoProject *gui = Denemo.project;
  GtkTextIter start, end;
  GtkWidget *textbuffer =  (GtkWidget *) g_object_get_data (G_OBJECT (gtk_widget_get_parent(button)), "textbuffer");
  //gtk_text_buffer_select_range (GTK_TEXT_BUFFER(textbuffer), &StartSelection, &EndSelection);
   if (gtk_text_buffer_get_selection_bounds (GTK_TEXT_BUFFER (textbuffer), &start, &end))
    {
      if (textbuffer)
         {
             gtk_text_buffer_insert_with_tags_by_name  (GTK_TEXT_BUFFER (textbuffer), &start, text, -1, "code", NULL);
             if (gtk_text_buffer_get_selection_bounds (GTK_TEXT_BUFFER (textbuffer), &start, &end))
                gtk_text_buffer_insert_with_tags_by_name (GTK_TEXT_BUFFER (textbuffer), &end, "}", -1, "code", NULL), changes++;
         }
      else
          g_critical ("Denemo program error, widget hierarchy changed???");
      }
 else
  warningdialog ( _("Select the text first."));
}

static void
insert_font_mag (GtkWidget * button)
{
  DenemoProject *gui = Denemo.project;
  
  GtkWidget *textbuffer = get_textbuffer_from_button (button);
  if (!textbuffer)
        {
        g_warning ("Denemo program error, widget hierarchy changed???");
        return;
        }
  GtkTextIter start, end;
  if (!gtk_text_buffer_get_selection_bounds (GTK_TEXT_BUFFER (textbuffer), &start, &end))
    {
        warningdialog  ( _("Select the text first."));
        return;
    }
  gchar *text = string_dialog_entry (gui, _( "Font Magnification"), _("Give a relative font size +/- "), "-2");

  if (text && *text)
    {
        gchar *out = g_strdup_printf ("\\fontsize #%s {", text);
        gtk_text_buffer_select_range (GTK_TEXT_BUFFER (textbuffer), &start, &end);//the dialog has destroyed the selection
        if (!gtk_text_buffer_get_selection_bounds (GTK_TEXT_BUFFER (textbuffer), &start, &end))
            {
            g_critical  ( _("Select the text first."));
            return;
            }
        markup_selection (button, out);
//        GtkWidget *textview = get_textview_from_button (button);
//        gtk_widget_grab_focus (textview);
        g_free (out);
    }
 g_free (text);
}
static void
insert_vert (GtkWidget * button)
{
  DenemoProject *gui = Denemo.project;
  
  GtkWidget *textbuffer = get_textbuffer_from_button (button);
  gchar *text = string_dialog_entry (gui, _( "Space Above"), _("Give space to leave above + only "), "2");
  if (text && *text)
    {
        gchar *out = g_strdup_printf ("\\vspace #%s", text);
      if (textbuffer)
        {
            GtkTextIter cursor;
            gtk_text_buffer_get_iter_at_mark (GTK_TEXT_BUFFER (textbuffer), &cursor, gtk_text_buffer_get_insert (GTK_TEXT_BUFFER(textbuffer)));
            gtk_text_buffer_insert_with_tags_by_name (GTK_TEXT_BUFFER (textbuffer), &cursor, out,    -1,  "code",NULL);

            gtk_text_buffer_get_iter_at_mark (GTK_TEXT_BUFFER (textbuffer), &cursor, gtk_text_buffer_get_insert (GTK_TEXT_BUFFER(textbuffer)));
            gtk_text_buffer_insert_with_tags_by_name (GTK_TEXT_BUFFER (textbuffer), &cursor, " ",    -1,  "ineditable",NULL);
            changes++;
        }
       else
        {
          g_warning ("Denemo program error, widget hierarchy changed???");
        }
//     GtkWidget *textview = get_textview_from_button (button);
//     gtk_widget_grab_focus (textview);
     g_free (out);
    }
 g_free (text);
}

static void
insert_horiz (GtkWidget * button)
{
  DenemoProject *gui = Denemo.project;
  
  GtkWidget *textbuffer = get_textbuffer_from_button (button);
  gchar *text = string_dialog_entry (gui, _( "Insert Space"), _("Give space to insert +/- "), "2");
  if (text && *text)
    {
        gchar *out = g_strdup_printf ("\\hspace #%s", text);
      if (textbuffer)
        {
            GtkTextIter cursor;
            gtk_text_buffer_get_iter_at_mark (GTK_TEXT_BUFFER (textbuffer), &cursor, gtk_text_buffer_get_insert (GTK_TEXT_BUFFER(textbuffer)));
            gtk_text_buffer_insert_with_tags_by_name (GTK_TEXT_BUFFER (textbuffer), &cursor, out,    -1,  "code",NULL);

            gtk_text_buffer_get_iter_at_mark (GTK_TEXT_BUFFER (textbuffer), &cursor, gtk_text_buffer_get_insert (GTK_TEXT_BUFFER(textbuffer)));
            gtk_text_buffer_insert_with_tags_by_name (GTK_TEXT_BUFFER (textbuffer), &cursor, " ",    -1,  "ineditable",NULL);
            changes++;
        }
       else
        {
          g_warning ("Denemo program error, widget hierarchy changed???");
        }
//     GtkWidget *textview = get_textview_from_button (button);
//     gtk_widget_grab_focus (textview);
     g_free (out);
    }

  g_free (text);
}


static void preview_text (gchar *text)
{
    gchar *lilypond = create_lilypond_from_text (text);//g_print("At this point lilypond is <<<%s>>>\ntext was %s\n\n", lilypond, text);
    gchar *syntax = g_strconcat (LILYPOND_SYMBOL_DEFINITIONS, " \\markup \\column {",lilypond," }", NULL);
    create_pdf_for_lilypond (syntax);
    g_free (syntax);
    g_free (lilypond);
}
static gboolean run_preview (GtkWidget *textbuffer)
{//g_print (".");
   // if (TimerId == 0) //before a character is entered there is no timer, but Preview button should run_preview.
   //     return FALSE;//no further calls
    static gint counter = 0;
    if (changes && (counter==10))
        counter = 0;
    if (!changes)
        counter++;
    if (changes || (counter==10))
        {   if (changes)
                counter = 0;
            changes = 0;
            GtkTextIter startiter, enditer;
            gtk_text_buffer_get_start_iter (GTK_TEXT_BUFFER(textbuffer), &startiter);
            gtk_text_buffer_get_end_iter (GTK_TEXT_BUFFER(textbuffer), &enditer);
            gchar *text = gtk_text_buffer_get_text (GTK_TEXT_BUFFER(textbuffer), &startiter, &enditer, FALSE);
#ifdef _USE_EVINCE_            
            pause_continuous_typesetting ();
#endif
            preview_text (text);
    }
    return TRUE; //continuous timer
}
static void
preview_markup (GtkWidget * button)
{
    DenemoProject *gui = Denemo.project;
    GtkWidget *textbuffer = get_textbuffer_from_button (button);
    changes++;//force recalculation.
    run_preview (textbuffer);
}

static gboolean keypress_callback (GtkWidget * w, GdkEventKey * event, GtkWidget *textbuffer)
{
  DenemoProject *gui = Denemo.project;
  GtkTextIter cursor;
  if (TimerId==0)
    TimerId = g_timeout_add ( 300, (GSourceFunc)run_preview, textbuffer);
  changes++;
  if (event->keyval == GDK_KEY_Return)
    {
  //gchar *key = g_strdup_printf ("%c", gdk_keyval_to_unicode (event->keyval));
  gtk_text_buffer_get_iter_at_mark (GTK_TEXT_BUFFER(textbuffer), &cursor, gtk_text_buffer_get_insert (GTK_TEXT_BUFFER(textbuffer)));
  gtk_text_buffer_insert (GTK_TEXT_BUFFER(textbuffer), &cursor, "\n""¶", -1);
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
 
 
static void adjust_selection (GtkWidget *w, gchar  *syntax) 
{
 GtkTextIter start, end;
  GtkWidget *textbuffer =  (GtkWidget *) g_object_get_data (G_OBJECT (gtk_widget_get_parent(w)), "textbuffer");
 
  if (gtk_text_buffer_get_selection_bounds (GTK_TEXT_BUFFER (textbuffer), &start, &end))
    {
        gchar *text = string_dialog_entry (Denemo.project, syntax, _("Give amount "), "0");
          if (text && *text)
            {
                gchar *out = g_strdup_printf ("%s #%s {", syntax, text);
                gtk_text_buffer_select_range (GTK_TEXT_BUFFER (textbuffer), &start, &end);//the dialog has destroyed the selection
                markup_selection (w, out);
                g_free (out);  
            }
          g_free (text);
 }
}

static void adjust_insert (GtkWidget *w, gchar  *syntax) 
{
    GtkWidget *textbuffer =  (GtkWidget *) g_object_get_data (G_OBJECT (gtk_widget_get_parent(w)), "textbuffer");
 
    gchar *text = string_dialog_entry (Denemo.project, syntax, _("Give amount "), "0");
    if (text && *text)
        {
            gchar *out = g_strdup_printf ("%s%s", syntax, text);
           
            insert_markup (w, out);
            g_free (out);  
        }
    g_free (text);
}


static void insert_glyph_from_user (GtkWidget * menuitem)
{
  DenemoProject *gui = Denemo.project;
  
  GtkWidget *textbuffer = get_textbuffer_from_menuitem (menuitem);
  if (textbuffer)
    {
      gchar *text =  string_dialog_entry (gui, _( "Music Glyphs"), _("Give a name of glyph (see LilyPond Documentation for these) "), "rests.2");
      if(text)
        {
            gchar *insert = g_strdup_printf("\\musicglyph #\"%s\" ", text);
            GtkTextIter cursor;
            gtk_text_buffer_insert_at_cursor (GTK_TEXT_BUFFER (textbuffer), insert, -1), changes++;
            gtk_text_buffer_get_iter_at_mark (GTK_TEXT_BUFFER (textbuffer), &cursor, gtk_text_buffer_get_insert (GTK_TEXT_BUFFER(textbuffer)));
            gtk_text_buffer_insert_with_tags_by_name (GTK_TEXT_BUFFER (textbuffer), &cursor, " ",    -1,  "ineditable",NULL);
            g_free (text);
            g_free (insert);
        }
    }
  else
    {
      g_warning ("Denemo program error, widget hierarchy changed???");
    }
}
//pops up a menu of adjustments to make to the selection
static void
popup_adjust_menu ( GtkWidget *textbuffer)
{
    GtkWidget *menu = gtk_menu_new ();
    g_object_set_data (G_OBJECT (menu), "textbuffer", (gpointer)textbuffer);

    GtkWidget *menuitem = gtk_menu_item_new_with_label (_("Raise"));
    gtk_widget_set_tooltip_text (menuitem, _("raises the selection by the amount given."));
    g_signal_connect (menuitem, "activate", G_CALLBACK (adjust_selection), "\\raise ");
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem); 
    
    
    menuitem = gtk_menu_item_new_with_label (_("Font Magnification"));
    gtk_widget_set_tooltip_text (menuitem,  _("Inserts markup to set the relative font size for the selected text."));
    g_signal_connect (menuitem, "activate", G_CALLBACK (adjust_selection), "\\fontsize ");
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem); 
    
    
          
    gtk_widget_show_all (menu);    

    if (gtk_text_buffer_get_has_selection (GTK_TEXT_BUFFER (textbuffer)))
        {
         gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, GDK_CURRENT_TIME);
        }
    else
        warningdialog ( _("Select the text first."));
}
//pops up a menu of inserts requring a value
static void
popup_insert_by_value_menu ( GtkWidget *textbuffer)
{
    GtkWidget *menu = gtk_menu_new ();
    g_object_set_data (G_OBJECT (menu), "textbuffer", (gpointer)textbuffer);

    GtkWidget *menuitem = gtk_menu_item_new_with_label (_("Horizontal Space (+/-)"));
    gtk_widget_set_tooltip_text (menuitem, _("Inserts a spacer (+) or shifts the rest of the line rightwards (-) by amount given"));
    g_signal_connect (menuitem, "activate", G_CALLBACK (adjust_insert), "\\hspace #");
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem); 
    
    
        menuitem = gtk_menu_item_new_with_label  (_("Paste Note Name"));
    gtk_widget_set_tooltip_text (menuitem, _("Pastes user supplied note name in a choice of font style and size.\n"
    "The note name is pasted as LilyPond markup.\n"
    "It will print as the note name in the sentence you are writing, transposed according to any global transposition you set.\n"
    "Use, for example, to specify the key of a piece in a title.\n"));

    g_signal_connect (menuitem, "activate", G_CALLBACK (paste_current_lilypond_as_fakechord), GINT_TO_POINTER(FALSE));
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem); 

    menuitem = gtk_menu_item_new_with_label (_("LilyPond Glyph"));
    gtk_widget_set_tooltip_text (menuitem, _("Inserts LilyPond music glyph at the cursor position in the text."));
    g_signal_connect (menuitem, "activate", G_CALLBACK (insert_glyph_from_user), NULL);
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem); 
      
    
    gtk_widget_show_all (menu);    
    gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, GDK_CURRENT_TIME);     
}

//pops up a menu of styles to be applied to the selection
static void
popup_style_menu (GtkWidget *button)
{
  GtkWidget *menu = gtk_menu_new ();
  GtkWidget *textbuffer = get_textbuffer_from_button (button);


    
    g_object_set_data (G_OBJECT (menu), "textbuffer", (gpointer)textbuffer);
    
    GtkWidget *menuitem = gtk_menu_item_new_with_label (_("Adjust Selection (with value)"));
    gtk_widget_set_tooltip_text (menuitem, _("Pop up a menu of adjustments to be made to the selection with a value you give."));
    g_signal_connect_swapped (menuitem, "activate", G_CALLBACK (popup_adjust_menu), textbuffer);
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);
    
    menuitem = gtk_menu_item_new_with_label (_("Bold"));
    gtk_widget_set_tooltip_text (menuitem, _("Inserts markup to make the selected text bold."));
    g_signal_connect (menuitem, "activate", G_CALLBACK (markup_selection), "\\bold {");
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);

    menuitem = gtk_menu_item_new_with_label (_("Italic"));
    gtk_widget_set_tooltip_text (menuitem, _("Inserts markup to make the selected text italic."));
    g_signal_connect (menuitem, "activate", G_CALLBACK (markup_selection), "\\italic {");
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);

    menuitem = gtk_menu_item_new_with_label (_("Upright"));
    gtk_widget_set_tooltip_text (menuitem, _("Inserts markup to make the selected text not italic."));
    g_signal_connect (menuitem, "activate", G_CALLBACK (markup_selection), "\\upright {");
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);

    menuitem = gtk_menu_item_new_with_label (_("Un-Bold"));
    gtk_widget_set_tooltip_text (menuitem, _("Inserts markup to make the selected text not bold."));
    g_signal_connect (menuitem, "activate", G_CALLBACK (markup_selection), "\\medium {");
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);

    menuitem = gtk_menu_item_new_with_label (_("Superscript"));
    gtk_widget_set_tooltip_text (menuitem, _("Inserts markup to make the selected text as superscript."));
    g_signal_connect (menuitem, "activate", G_CALLBACK (markup_selection), "\\super {");
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);

    menuitem = gtk_menu_item_new_with_label (_("Subscript"));
    gtk_widget_set_tooltip_text (menuitem, _("Inserts markup to make the selected text as subscript."));
    g_signal_connect (menuitem, "activate", G_CALLBACK (markup_selection), "\\sub {");
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);

    menuitem = gtk_menu_item_new_with_label (_("Column"));
    gtk_widget_set_tooltip_text (menuitem, _("Inserts markup to place the selected text in a column. You can nest lines inside columns."));
    g_signal_connect (menuitem, "activate", G_CALLBACK (markup_selection), "\\column {");
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);

    menuitem = gtk_menu_item_new_with_label (_("Line"));
    gtk_widget_set_tooltip_text (menuitem, _("Inserts markup to place the selected text in a line."));
    g_signal_connect (menuitem, "activate", G_CALLBACK (markup_selection), "\\line {");
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);

    menuitem = gtk_menu_item_new_with_label (_("Box"));
    gtk_widget_set_tooltip_text (menuitem, _("Inserts markup to place the selected text in a box."));
    g_signal_connect (menuitem, "activate", G_CALLBACK (markup_selection), "\\box {");
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);
       
      
    gtk_widget_show_all (menu);    

    if (gtk_text_buffer_get_has_selection (GTK_TEXT_BUFFER (textbuffer)))
        {
         gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, GDK_CURRENT_TIME);
        }
    else
        warningdialog ( _("Select the text first."));
}


//pops up a menu of objects to be inserted at cursor
static void
popup_insert_menu (GtkWidget *button)
{
    GtkWidget *menu = gtk_menu_new ();
    GtkWidget *textbuffer = get_textbuffer_from_button (button);
    g_object_set_data (G_OBJECT (menu), "textbuffer", (gpointer)textbuffer);
    GtkWidget *menuitem;


    menuitem = gtk_menu_item_new_with_label (_("Insert (with value)"));
    gtk_widget_set_tooltip_text (menuitem, _("Pop up a menu of insertions that require a value to insert."));
    g_signal_connect_swapped (menuitem, "activate", G_CALLBACK (popup_insert_by_value_menu), textbuffer);
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);
    
    menuitem = gtk_menu_item_new_with_label (_("Paste Current Snippet"));
    gtk_widget_set_tooltip_text (menuitem,  _("Pastes the music captured in the currently selected Snippet into the text at the cursor.\nThe music appears here in the LilyPond syntax.\nIt will print as typeset music embedded in the sentence you are writing.\nYou can edit the syntax following the LilyPond syntax.\n"));
    g_signal_connect (menuitem, "activate", G_CALLBACK (paste_snippet_lilypond), NULL);
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);






     menuitem = gtk_menu_item_new_with_label  (_("Paste Chord Symbol"));
    gtk_widget_set_tooltip_text (menuitem, _("Pastes chord symbol for the chord at the cursor in a choice of font style and size.\n"
    "The chord is pasted as LilyPond markup.\n"
    "It will print as the chord symbol (fakechord) in the sentence you are writing, transposed according to any global transposition you set.\n"));

    g_signal_connect (menuitem, "activate", G_CALLBACK (paste_current_lilypond_as_fakechord), GINT_TO_POINTER(TRUE));
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem); 


    menuitem = gtk_menu_item_new_with_label (_("Paste Fret Diagram"));
    gtk_widget_set_tooltip_text (menuitem, _("Pastes the chord at the cursor as a Fret Diagram\n"
    "The music appears here in the LilyPond syntax.\n"
    "It will print as fret diagram in the sentence you are writing, transposed according to the global transposition set.\n"));

    g_signal_connect (menuitem, "activate", G_CALLBACK (paste_current_lilypond_as_fretdiagram), NULL);
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem); 

    menuitem = gtk_menu_item_new_with_label (_("Insert “ Open Quotes"));
    gtk_widget_set_tooltip_text (menuitem, _("Inserts open double quote. Note that this is not the \" character which is used for grouping words not to be treated as markup. The \" marks must be paired or LilyPond will not typeset the music."));
    g_signal_connect (menuitem, "activate", G_CALLBACK (insert_markup), "“");
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem); 

    menuitem = gtk_menu_item_new_with_label (_("Insert ” Close Quotes"));
    gtk_widget_set_tooltip_text (menuitem, _("Inserts close double quote. Note that this is not the \" character which is used for grouping words  not to be treated as markup. The \" marks must be paired or LilyPond will not typeset the music."));
    g_signal_connect (menuitem, "activate", G_CALLBACK (insert_markup), "”");
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem); 

    menuitem = gtk_menu_item_new_with_label (_("Segno"));
    gtk_widget_set_tooltip_text (menuitem, _("Inserts Segno sign at the cursor position in the text."));
    g_signal_connect (menuitem, "activate", G_CALLBACK (insert_markup), "\\musicglyph #\"scripts.segno\"");
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem); 

    menuitem = gtk_menu_item_new_with_label (_("Coda"));
    gtk_widget_set_tooltip_text (menuitem, _("Inserts Coda sign at the cursor position in the text."));
    g_signal_connect (menuitem, "activate", G_CALLBACK (insert_markup), "\\musicglyph #\"scripts.coda\"");
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem); 


  
    gtk_widget_show_all (menu);    
    gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, GDK_CURRENT_TIME); 
}  



gboolean get_user_markup (GString *user_text, GString *marked_up_text, gchar* title, gchar *instruction, gchar *initial_value, gboolean modal, gboolean format_only)
{
#ifndef USE_EVINCE
          g_debug("This feature requires denemo to be built with evince");
    return FALSE;
#endif
  GtkWidget *top_vbox = gtk_vbox_new (FALSE, 8);
  
  install_markup_preview (top_vbox, instruction);

  GtkWidget *hbox = gtk_hbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (top_vbox), hbox, FALSE, TRUE, 0);
  
  GtkWidget *button = gtk_button_new_with_label (_("Insert"));
  gtk_widget_set_tooltip_text (button, _("Menu of special characters and graphics to insert at the cursor position"));
  g_signal_connect (button, "clicked", G_CALLBACK (popup_insert_menu), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  
  button = gtk_button_new_with_label (_("Next Snippet"));
  gtk_widget_set_tooltip_text (button, _("Makes the next Snippet the one that can be pasted.\nTo see the music snippets you need to check View → Snippets\nThe one selected is in bold black."));
  DenemoAction *action = denemo_menusystem_get_action ( "NextRhythm");
  if (action)
    g_signal_connect_swapped (button, "clicked", G_CALLBACK (denemo_action_activate), action);
  else
    gtk_widget_set_sensitive (button, FALSE);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label (_("Selection"));
  gtk_widget_set_tooltip_text (button, _("Pops up a menu to apply some style to the selection."));
  g_signal_connect (button, "clicked", G_CALLBACK (popup_style_menu), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  
  
  button = gtk_button_new_with_label ("⬆");
  gtk_widget_set_tooltip_text (button, _("Inserts the markup needed to leave space above this line of text. Ineffective on the top line of standalone text, instead drag such text in the Print View"));
  g_signal_connect (button, "clicked", G_CALLBACK (insert_vert), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  

  button = gtk_button_new_with_label (_("Help"));
  gtk_widget_set_tooltip_text (button, instruction);
  g_signal_connect_swapped (button, "clicked", G_CALLBACK (warningdialog), instruction);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);


  button = gtk_button_new_with_label (_("Preview"));
  gtk_widget_set_tooltip_text (button, _("Shows what the text will look like when typeset in the Print View window. For score and movement titles the appearance is correct only relative to the default title."));
  g_signal_connect (button, "clicked", G_CALLBACK (preview_markup), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  
  gchar *text;
    if(format_only)
        text = g_strdup (initial_value); //anything else, just format the passed in string
    else
        text = string_dialog_editor_with_widget_opt (Denemo.project, title, NULL, initial_value, top_vbox, modal, keypress_callback);//this call attaches "textbuffer" to top_vbox
 if(TimerId)
    g_source_remove (TimerId);
 TimerId = 0;
 drop_markup_area ();
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
