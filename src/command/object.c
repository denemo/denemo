/* object.c
 * functions that do operations to DenemoObjects
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */

#include <denemo/denemo.h>
#include "command/chord.h"
#include "core/utils.h"
#include "command/commandfuncs.h"
#include "command/object.h"
#include "command/staff.h"
#include "command/tuplet.h"
#include "command/select.h"
#include "audio/pitchentry.h"
#include "core/utils.h"
#include "core/view.h"
#include "command/lilydirectives.h"
#include <string.h>

void initkeyaccs (gint * accs, gint number);

DenemoObject *
get_object (void)
{
  DenemoProject *gui = Denemo.project;
  DenemoMovement *si = gui->movement;
  return (DenemoObject *) si->currentobject ? (DenemoObject *) si->currentobject->data : NULL;
}

/**
 * Free the given object
 * @param mudobj the DenemoObject to free
FIXME is this failing to free the object field???
 */
void
freeobject (DenemoObject * mudobj)
{
  if (mudobj == NULL)
    return;
  if (mudobj->midi_events)
    g_list_free (mudobj->midi_events);  //do not free the data it belongs to libsmf
  if (mudobj->lilypond)
    g_free (mudobj->lilypond);
  switch (mudobj->type)
    {
    case CHORD:
      freechord (mudobj);       /* Which also frees mudobj itself */
      break;
    case CLEF:
      free_directives (((clef *) mudobj->object)->directives);
      g_free (mudobj->object);
      g_free (mudobj);
      break;
    case KEYSIG:
      free_directives (((keysig *) mudobj->object)->directives);
      g_free (mudobj->object);
      g_free (mudobj);
      break;
    case TIMESIG:
      free_directives (((timesig *) mudobj->object)->directives);
      g_free (mudobj->object);
      g_free (mudobj);
      break;

    case TUPOPEN:
    case TUPCLOSE:
      free_directives (((tuplet *) mudobj->object)->directives);
      g_free (mudobj->object);
      g_free (mudobj);
      break;
    default:
      g_free (mudobj);
      break;
    }
}
/* drop display full information about the object at the cursor */
static GtkWidget *ObjectInfo = NULL;
static gboolean
drop_object_info (void)
{
  if (ObjectInfo)
    gtk_widget_hide (ObjectInfo);
  return TRUE;
}
/* display full information about the object at the cursor if Inspector is open*/
void
update_object_info (void)
{
  if (ObjectInfo && gtk_widget_get_visible (ObjectInfo))
    display_current_object ();
}
static void
append_directives_information (GString * selection, GList * directives)
{
    gboolean first = TRUE;
  do
    {
      DenemoDirective *directive = directives->data;
      if(directive->tag==NULL)
            directive->tag = g_string_new("<Unknown Tag>");//shouldn't happen
      const gchar *label = get_label_for_command (directive->tag->str);
      const gchar *menupath = get_menu_path_for_command (directive->tag->str);
      const gchar *tooltip = get_tooltip_for_command (directive->tag->str);
      if (tooltip == NULL) tooltip = _("No tooltip");

      if(directive->tag==NULL)
            directive->tag = g_string_new("<Unknown Tag>");//shouldn't happen
      gchar *label_e = label? g_markup_escape_text(label, -1): g_markup_escape_text(directive->tag->str, -1);
      if(!first)
        g_string_append (selection, "\n<span foreground=\"blue\"weight=\"bold\">---------------------------------------------------------</span>\n");
      else
         g_string_append (selection, "\n<span foreground=\"green\"weight=\"bold\">---------------------------------------------------------</span>\n");
      first = FALSE;
      if(label)
                g_string_append_printf (selection, _("Directive for command: <span weight=\"bold\">\"%s\"</span>\n"), label_e);
      else
                g_string_append_printf (selection, _("Directive tagged: <span foreground=\"red\"weight=\"bold\">\"%s\"</span>\n"), label_e);
      g_free (label_e);
      if(menupath)
        {
            gchar *menupath_e = g_markup_escape_text(menupath, -1);
            g_string_append_printf (selection, _("Menu location for this command: <span style=\"italic\" weight=\"bold\">\"%s\"</span>\n"), menupath_e);
            g_free (menupath_e);
        }
      if(tooltip)
        {
            gchar * tooltip_e = g_markup_escape_text(tooltip, -1);
            g_string_append_printf (selection, _("The help for the command that created this directive is:\n<big>\"%s\"</big>\n"), tooltip_e);
            g_free (tooltip_e);
        }    
      if(directive->prefix)
        {
            gchar *lily = g_markup_escape_text(directive->prefix->str, -1);
            g_string_append_printf (selection, _("LilyPond inserted in prefix to this object is <tt>\"%s\"</tt>\n"), lily);
            g_free (lily);
        }
      if(directive->postfix)
        {
            gchar *lily = g_markup_escape_text(directive->postfix->str, -1);
            g_string_append_printf (selection, _("LilyPond inserted in postfix to this object is <tt>\"%s\"</tt>\n"), lily);
            g_free (lily);
        }
     if(!directives->next)
        g_string_append (selection, "<span foreground=\"blue\"weight=\"bold\">---------------------------------------------------------</span>");
    }
  while (directives->next && (directives = directives->next));
  g_string_append (selection, "\n");
}

static void append_lilypond (DenemoObject *curObj, GString *selection)
    {
        DenemoProject *gui = Denemo.project;
        if( gui->lilysync != gui->changecount)
            refresh_lily_cb (NULL, gui); //if(curObj->lilypond)g_print ("lili |%s|\n", curObj->lilypond);
        if(curObj->lilypond && *curObj->lilypond)
            g_string_append_printf (selection, _("The LilyPond syntax generated is: <tt>\"%s\"</tt>\n"), g_markup_escape_text(curObj->lilypond, -1));
        else
            g_string_append_printf (selection, _("This object does not affect the music typesetting, (no LilyPond syntax is generated)\n"));
    }
    
static gint gcd384 (gint n)
{
    gint remainder, m = 384;
    while (n != 0)
    {
        remainder = m % n;
        m = n;
        n = remainder;
    }
    return m;
}
static void reset_cursors (void)
{
    gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), gdk_cursor_new (GDK_LEFT_PTR)); 
    if(ObjectInfo)
        gdk_window_set_cursor (gtk_widget_get_window (ObjectInfo), gdk_cursor_new (GDK_LEFT_PTR));
    if(Denemo.printarea && gtk_widget_get_window (Denemo.printarea))
        gdk_window_set_cursor (gtk_widget_get_window (Denemo.printarea), gdk_cursor_new (GDK_LEFT_PTR));    
}

static void move_to_next_note (GtkWidget *editwin)
{
if(!cursor_to_next_note_height())
    cursor_to_nth_note_height (0);
if (editwin)
   {
    gtk_widget_destroy (editwin);
    reset_cursors ();
    edit_object();
    }
}

static void go_left(GtkWidget *editwin)
{
cursor_to_prev_object (FALSE, FALSE);
if(Denemo.project->movement->currentobject == NULL)
    {
        cursor_to_next_object (FALSE, FALSE);
        warningdialog (_("Preceding measures are empty"));
    }
if (editwin)
   {
    gtk_widget_destroy (editwin);
    reset_cursors ();
    edit_object();
    }
}
static void go_right(GtkWidget *editwin)
{
cursor_to_next_object (FALSE, FALSE);
if(Denemo.project->movement->currentobject == NULL)
    {
      cursor_to_prev_object (FALSE, FALSE);
      warningdialog (_("Subsequent measures are empty"));
    }
if (editwin)
   {
    gtk_widget_destroy (editwin);
    reset_cursors ();
    edit_object();
    }
}
void
display_current_object (void)
{
  DenemoProject *gui = Denemo.project;
  gchar *type = "object";
  if (ObjectInfo == NULL)
    {
      ObjectInfo = gtk_window_new (GTK_WINDOW_TOPLEVEL);
      gtk_window_set_title (GTK_WINDOW (ObjectInfo), _("Denemo Object Inspector"));
      g_signal_connect (G_OBJECT (ObjectInfo), "delete-event", G_CALLBACK (drop_object_info), NULL);
     // gtk_window_set_keep_above (GTK_WINDOW (ObjectInfo), TRUE);
     gtk_window_set_default_size (GTK_WINDOW (ObjectInfo), 400, 400);
     gtk_window_set_accept_focus (GTK_WINDOW (ObjectInfo), FALSE);
    }
  else
    {
      gtk_widget_destroy (gtk_bin_get_child (GTK_BIN (ObjectInfo)));
    }

  GtkWidget *vbox = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (ObjectInfo), vbox);

  if (gui->movement->currentobject == NULL)
    {
      GtkWidget *label = gtk_label_new ("The cursor is in an empty measure.\n" "As a special case this will be typeset as a non-printing whole measure rest.\n" "Note that if you put anything at all in this measure\n" "you must insert a real whole measure rest if that is what you want.");
      gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, TRUE, 0);
    }
  else
    {
    DenemoObject *curObj = gui->movement->currentobject->data;
    
    if (curObj->type==CHORD)
       {
            chord *thechord = ((chord *) curObj->object);
            if (thechord->notes)
              {
                note *thenote = findnote (curObj, Denemo.project->movement->cursor_y);
                GtkWidget *button = gtk_button_new_with_label (
                    thechord->notes->next?_("Inspect next note in chord"):_("Inspect the note"));
                gtk_widget_set_sensitive (button, 
                    ( (!thenote) || thechord->notes->next || (Denemo.project->movement->cursor_y!=thenote->mid_c_offset)));
                   
                g_signal_connect_swapped (button, "clicked", G_CALLBACK (move_to_next_note), NULL);
                gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, TRUE, 0);
            }
        }
    GtkWidget *edit_button = gtk_button_new_with_label (_("Run the Object Editor"));
    g_signal_connect (edit_button, "clicked", G_CALLBACK (edit_object), NULL);
    gtk_box_pack_start (GTK_BOX (vbox), edit_button, FALSE, TRUE, 0);
      
      
      GString *selection = g_string_new (gui->movement->cursor_appending ? _("The cursor is in the appending position after ") : _("The cursor is on "));
      GString *warning = g_string_new ("");
      switch (curObj->type)
        {
        case CHORD:
          {
            chord *thechord = ((chord *) curObj->object);
            if (thechord->notes)
              {
                if (thechord->notes->next)
                  {
                    type = _("chord");
                    selection = g_string_append (selection, _("a chord.\n"));
                  }
                else
                  {
                    type = _("note");
                    selection = g_string_append (selection, _("a one-note chord.\n"));
                  }
                if (thechord->slur_begin_p)
                  selection = g_string_append (selection, _("A slur starts from here.\n" "There should be a matching end slur later.\n"));
                if (thechord->slur_end_p)
                  selection = g_string_append (selection, _("A slur ends here\n" "There should be a matching start slur earlier.\n"));
                if (thechord->is_tied)
                  selection = g_string_append (selection, _("This is tied to the following note or chord.\n" "The following note or chord should have the same pitch\n"));
                 if (thechord->crescendo_begin_p)
                  selection = g_string_append (selection, _("This note begins a crescendo. Use the Right Click → Dynamics menu to control this.\n"));
                 if (thechord->crescendo_end_p)
                  selection = g_string_append (selection, _("This note ends a crescendo. Use the Right Click → Dynamics menu to control this.\n"));
                if (thechord->diminuendo_begin_p)
                  selection = g_string_append (selection, _("This note begins a diminuendo. Use the Right Click → Dynamics menu to control this.\n"));
                 if (thechord->diminuendo_end_p)
                  selection = g_string_append (selection, _("This note ends a diminuendo. Use the Right Click → Dynamics menu to control this.\n"));
                  
                if (thechord->is_grace && !(thechord->is_grace & GRACED_NOTE))
                  selection = g_string_append (selection, _("This is an acciaccatura note\n"));    
                  
                if (thechord->is_grace & GRACED_NOTE)
                  selection = g_string_append (selection, _("This is an appoggiatura note\n"));
                if (curObj->isinvisible)
                  selection = g_string_append (selection, _("This note denotes a rhythm - use a MIDI keyboard to add pitches by playing.\n"));
                if (thechord->fakechord)
                    g_string_append_printf (selection, _("A Chord Symbol \"%s\" is attached to this note.\n"), ((GString *)thechord->fakechord)->str);

                if (thechord->figure)
                    g_string_append_printf (selection, _("A Bass Figure \"%s\" is attached to this note.\n"), ((GString *)thechord->figure)->str);



                note *thenote = findnote (curObj, gui->movement->cursor_y);
                if (thenote && gui->movement->cursor_y==thenote->mid_c_offset)
                  {
  
                    g_string_append_printf (selection, _("<b>Within the chord the cursor is on the note %s </b>\n"),
                                            pretty_name(mid_c_offsettolily (thenote->mid_c_offset, thenote->enshift)));
                    if (thenote->directives)
                      {
                        selection = g_string_append (selection, _("Attached to this note:"));
                        append_directives_information (selection, thenote->directives);
                      }
                  }
                if ((thechord->notes->next) && curObj->isinvisible)
                        warning = g_string_append (warning, _("This rhythm has extra notes added to it, delete them and use the foot-pedal or Alt key to enter chords with the MIDI controller.\n"));

              }
            else
              {
                type = _("rest");
                selection = g_string_append (selection, _("a rest.\n"));
                if (thechord->slur_begin_p)
                  warning = g_string_append (warning, _("This rest has a slur start on it, use the Right Click  → Slurs menu to remove it\n"));
                if (thechord->slur_end_p)
                  warning = g_string_append (warning, _("This rest has a slur end on it, use the Right Click → Slurs menu to remove it\n"));
                if (thechord->is_tied)
                  warning = g_string_append (warning, _("This rest has a tie starting on it, use the Right Click  → Tied Note to remove it\n"));

                  
                if (curObj->isinvisible)
                  selection = g_string_append (selection, _("This rest will not print, just act as a spacer.\n"));
                if (thechord->is_grace && curObj->isinvisible)
                  warning = g_string_append (warning, _("This rest has the grace attribute set: these objects are usually inserted automatically to match real grace notes in other parts, this helps the music typesetter place the grace note correctly in the Print View.\n"));

              }
            if (thechord->directives) 
                            {
                            selection = g_string_append (selection, _("Attached to the chord:"));
                            append_directives_information (selection, thechord->directives);
                            }
           gint gcd_s = gcd384 (curObj->starttick);
           gint gcd_d = gcd384 (curObj->durinticks);
           if (gcd_s == 384)
            {
                if (gcd_d == 384)
                 g_string_append_printf (selection, _("This %s starts %d 𝅘𝅥  's into the measure and lasts %d 𝅘𝅥 's.\n"), type, curObj->starttick/384, curObj->durinticks/384);
                else
                g_string_append_printf (selection, _("This %s starts %d 𝅘𝅥  's into the measure and lasts %d/%d 𝅘𝅥 's.\n"), type, curObj->starttick/384, curObj->durinticks/gcd_d, 384/gcd_d);
            } else 
            {
                if (gcd_d == 384)
                    g_string_append_printf (selection, _("This %s starts %d/%d 𝅘𝅥  's into the measure and lasts %d 𝅘𝅥 's.\n"), type, curObj->starttick/gcd_s, 384/gcd_s, curObj->durinticks/384);
                else
                    g_string_append_printf (selection, _("This %s starts %d/%d 𝅘𝅥  's into the measure and lasts %d/%d 𝅘𝅥 's.\n"), type, curObj->starttick/gcd_s, 384/gcd_s, curObj->durinticks/gcd_d, 384/gcd_d);
            }
           append_lilypond (curObj, selection);
          }
          break;
        case TUPOPEN:
          { tuplet *thetup = ((tuplet *) curObj->object);
            //type = _("start tuplet marker");
            g_string_append_printf (selection, _(" a Start Tuplet object\n""Meaning %d notes will take the time of %d notes\n" "until an End Tuplet object.\nSee the Notes/Rests → Tuplets for control over how tuplets print\n"), thetup->denominator, thetup->numerator);
                        if (thetup->directives) 
                            {
                                selection = g_string_append (selection, _("Attached to the Start Tuplet:"));
                                append_directives_information (selection, thetup->directives);
                            }
             append_lilypond (curObj, selection);
          }
          break;
        case TUPCLOSE:
          { tuplet *thetup = ((tuplet *) curObj->object);
            //type = _("end tuplet marker");
            g_string_append_printf (selection, _("an End Tuplet object\n" "Note: the Start Tuplet must be in the same measure.\n"));
            if (thetup->directives) 
                            {
                                selection = g_string_append (selection, _("Attached to the End Tuplet:"));
                                append_directives_information (selection, thetup->directives);
                            }
            append_lilypond (curObj, selection);
          }
          break;
        case CLEF:
          {
            clef *theclef = ((clef *) curObj->object);
            //type = _("clef change object");
            g_string_append_printf (selection, _("a Clef Change object.\n"));   
            if (theclef->directives) 
                            {
                                selection = g_string_append (selection, _("Attached to the Clef Change:"));
                                append_directives_information (selection, theclef->directives);
                            }
            if (curObj->isinvisible)
                           selection = g_string_append (selection, _("This clef change is non-printing, it just affects the display.\n"));
            append_lilypond (curObj, selection);

          }
          break;
        case TIMESIG:
          {
            timesig *thetime = ((timesig *) curObj->object);
            //type = _("time signature change object");
            g_string_append_printf (selection, _("a Time Signature Change object.\n"));
                        if (thetime->directives) 
                            {
                                selection = g_string_append (selection, _("Attached to the Time Signature Change:"));
                                append_directives_information (selection, thetime->directives);
                            }   
            append_lilypond (curObj, selection);
            if (gui->movement->currentobject->prev)
              g_string_append_printf (warning, _("A Time Signature Change should be the first object in a measure\n" "unless you are trying to do something unusual"));
          }
          break;
        case KEYSIG:
          {
            keysig *thekey = ((keysig *) curObj->object);
            //type = _("key signature change object");

            g_string_append_printf (selection, _("a Key Signature Change object.\n"));
            if (thekey->directives) 
                {
                    selection = g_string_append (selection, _("Attached to the Key Signature Change:"));
                    append_directives_information (selection, thekey->directives);
                }
            append_lilypond (curObj, selection);
            }
          break;
        case STEMDIRECTIVE:
          {
            stemdirective *thestem = ((stemdirective *) curObj->object);
            //type = _("stem direction change object");
            g_string_append_printf (selection, _("a Stem Direction Control Object. The notes after the cursor %s"), ((stemdirective *) curObj->object)->type == DENEMO_STEMDOWN ? _("will have stems downwards.") : ((stemdirective *) curObj->object)->type == DENEMO_STEMUP ? _("will have stems upwards.") : _("will have stems up or down as needed."));
                        if (thestem->directives) 
                            {
                                selection = g_string_append (selection, _("\nAttached to the Stemming Change:"));
                                append_directives_information (selection, thestem->directives);
                            }
            append_lilypond (curObj, selection);
          }
          break;
        case LILYDIRECTIVE:
          {
            DenemoDirective *directive = (DenemoDirective *) curObj->object;
            //type = _("Denemo directive object");

            if(directive->tag==NULL)
                            directive->tag = g_string_new("<Unknown Tag>");//shouldn't happen
            const gchar *label = get_label_for_command (directive->tag->str);
            const gchar *menupath = get_menu_path_for_command (directive->tag->str);
            const gchar *tooltip = get_tooltip_for_command (directive->tag->str);
            if (tooltip == NULL) tooltip = _("No tooltip");
            gchar *label_e = label? g_markup_escape_text(label, -1): g_markup_escape_text(directive->tag->str, -1);
            if(label)
               g_string_append_printf (selection, _("a Denemo Directive: <span weight=\"bold\">%s</span>\n"), label_e);
            else
                g_string_append_printf (selection, _("a Denemo Directive tagged: <span foreground=\"red\"weight=\"bold\">%s</span>\n"), label_e);
            g_free (label_e);
            if(tooltip)
                {
                    gchar *tooltip_e = g_markup_escape_text(tooltip, -1);
                    g_string_append_printf (selection, _("\nThe help for the command that created this directive is\n<big>\"%s\"</big>"), tooltip_e);
                    g_free (tooltip_e);
                }

           g_string_append_printf (selection, _("%s"), directive->x ? _("\nNot all layouts\n") : directive->y ? _("\nOnly for one Layout\n"): "\n");
           if(menupath)
                {
                    gchar *menupath_e = g_markup_escape_text(menupath, -1);
                    g_string_append_printf (selection, _("Menu location for this command: <span style=\"italic\" weight=\"bold\">\"%s\"</span>\n"), menupath_e);
                    g_free (menupath_e);
                }
            
            {
               gchar *text =  g_strconcat(directive->prefix?directive->prefix->str:"",
                            directive->postfix?directive->postfix->str:"", NULL);
               g_strchug (text); //does not allocate memory
               if (*text)   
                {
                gchar *lily1 = directive->prefix?g_markup_escape_text(directive->prefix->str, -1):g_strdup("");
                gchar *lily2 = directive->postfix?g_markup_escape_text(directive->postfix->str, -1):g_strdup("");
                
                g_string_append_printf (selection, _("The LilyPond text inserted is <tt>%s%s</tt>\n"),  
                          lily1,lily2);//puts the whitespace back
                g_free (lily1);
                g_free (lily2);
                }
            else
                g_string_append_printf (selection, _("This object does not affect the printed output (no LilyPond syntax is generated for the typesetter)\n"));//well this ignores possible effect of whitespace...
            g_free (text);
            
            }
           if (gui->movement->currentobject->next == NULL && (gui->movement->currentmeasure->next == NULL))
              g_string_assign (warning, _("This Directive is at the end of the music" 
                "\nYou may need a closing double bar line -\n" 
                "see Directives → Markings → Inserting Barlines"));
          }
          break;
        default:
          {
            g_string_append (selection, _("The cursor is on an unknown object type. Please report how this happened!"));
          }
          break;
        } //end switch curObj type
        
        if (curObj->midi_events)
        {
          smf_event_t *event = (smf_event_t *) curObj->midi_events->data;
          gdouble time = event->time_seconds;
          gint minutes = time / 60.0;
          gdouble seconds = time - 60 * minutes;
          char *buf = event->midi_buffer;
          g_string_append_printf (selection, _("Playback timing: %d minutes %1.2f seconds"), minutes, seconds);
          #define velocity ((*(buf+2))&0x7F)
          if ((*buf & 0xf0) == 0x90)
            g_string_append_printf (selection, _(", Volume: %1.1f%%\n"), (100.0/127) * buf[2]);
          else
            g_string_append (selection, _(".\n"));
        }

        if (warning->len)
        {
          GtkWidget *label = gtk_label_new ("");
          warning = g_string_prepend (warning, _("<span font-desc=\"30\">Warning</span> "));
          gtk_label_set_markup (GTK_LABEL (label), warning->str);
          gtk_label_set_line_wrap (GTK_LABEL(label), TRUE);
          gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, TRUE, 0);
        }
        if (selection->len)
        {
          GtkWidget *scrolled_window = gtk_scrolled_window_new (NULL, NULL);
          gtk_box_pack_start (GTK_BOX (vbox), scrolled_window, TRUE, TRUE, 0);
          GtkWidget *inner_box = gtk_vbox_new (FALSE, 0);
          gtk_scrolled_window_add_with_viewport  (GTK_SCROLLED_WINDOW(scrolled_window),  inner_box);
          GtkWidget *label = gtk_label_new ("");
          gtk_label_set_selectable (GTK_LABEL(label), TRUE);
         
         /*
              making the text selectable unfortunately results in the label text being selected at the start
              the following code would undo that, but selection-bound cannot be written to, so it does not work.
              int start, end;
              gtk_label_get_selection_bounds (GTK_LABEL(label), &start, &end);
              GValue value = { 0 };
              g_value_init (&value, G_TYPE_INT);
              g_value_set_int (&value, end);
              g_object_set_property (G_OBJECT (label), "selection-bound", &value);
            *this also does not work:
              g_signal_emit_by_name (GTK_LABEL(label), "move-cursor", GTK_MOVEMENT_BUFFER_ENDS, -1, FALSE);
          */
          gtk_label_set_markup (GTK_LABEL (label), selection->str);
          gtk_label_set_line_wrap (GTK_LABEL(label), TRUE);
          gtk_box_pack_start (GTK_BOX (inner_box), label, TRUE, TRUE, 0);
        }
      g_string_free (warning, TRUE);
      g_string_free (selection, TRUE);
    }
  gtk_widget_show_all (ObjectInfo);
#ifdef G_OS_WIN32
//on windows, the ObjectInfo window takes the focus regardless of having told it not to, so it is up to the user to bring the inspector to the front.
  gtk_window_set_keep_above (GTK_WINDOW (ObjectInfo), TRUE);

#else
  gtk_window_present (GTK_WINDOW (ObjectInfo));
#endif
}

static void 
set_false (GtkWidget *button, gboolean *bool)
{
    gtk_widget_destroy (button);
    if (*bool)
        score_status(Denemo.project, TRUE);
    *bool = FALSE;
}

typedef gboolean fn_type (gchar*);
static void advanced_edit_type_directive (GtkWidget *button, gpointer fn)
{
    DenemoDirective *directive = (DenemoDirective*) g_object_get_data (G_OBJECT(button), "directive");
    GList **directives = (GList **)g_object_get_data (G_OBJECT(button), "directives");
    if (!( ((fn_type *)fn) (directive->tag->str)))
        {
            if(directives) 
                *directives = g_list_remove (*directives, directive);
            else
                dnm_deleteobject (Denemo.project->movement);
            gtk_widget_destroy (gtk_widget_get_parent (gtk_widget_get_parent (button)));
            score_status(Denemo.project, TRUE);
        }   
    
}

static void  create_palette_button_for_directive (GtkWidget *button, gchar *what)
{
    DenemoDirective *directive = (DenemoDirective*) g_object_get_data (G_OBJECT(button), "directive");
    DenemoPalette *pal = NULL;
    gchar *script = get_script_for_directive (directive, what);
    gchar *name = choose_palette_by_name (TRUE, FALSE);
    if (name)
        pal = create_palette (name, FALSE, TRUE);
    if(pal) {
        gchar *button_name = g_strdup_printf (_( "Clone %s"), directive->tag->str);
        gchar *label = string_dialog_entry (Denemo.project, _("Palette Button Creation"), _("Give a (unique) name for the button"), button_name);
        if (label)
            {
                if (!palette_add_button (pal, label, _("Creates a cloned Denemo Directive"), script))
                    warningdialog (_("Could not create a button of that name in that palette"));
            } else
            warningdialog (_("Cancelled"));
        g_free (label);    
        g_free (button_name);
        gtk_widget_show_all (gtk_widget_get_parent(pal->box));
        gtk_widget_destroy (gtk_widget_get_toplevel (button));
        reset_cursors ();
    }
    g_free (script);
}
static void  create_palette_button_for_command (GtkWidget *button, gchar *tooltip)
{
    DenemoDirective *directive = (DenemoDirective*) g_object_get_data (G_OBJECT(button), "directive");
    DenemoPalette *pal = NULL;
    gchar *script = g_strdup_printf ("(d-%s)", directive->tag->str);
    gchar *name = choose_palette_by_name (TRUE, FALSE);
    if (name)
        pal = create_palette (name, FALSE, TRUE);
    if(pal) {
        gboolean success = palette_add_button (pal, directive->tag->str, tooltip, script);
        gtk_widget_show_all (gtk_widget_get_parent(pal->box));
        gtk_widget_destroy (gtk_widget_get_toplevel (button));
        reset_cursors ();
    }
    g_free (script);
}

static void delete_directive (GtkWidget *button, gpointer fn)
{
    DenemoDirective *directive = (DenemoDirective*) g_object_get_data (G_OBJECT(button), "directive");
    GList **directives = (GList **)g_object_get_data (G_OBJECT(button), "directives");
    if(directives) 
        *directives = g_list_remove (*directives, directive);
    else
        dnm_deleteobject (Denemo.project->movement);
    gtk_widget_destroy (gtk_widget_get_parent (gtk_widget_get_parent (button)));
    score_status(Denemo.project, TRUE);    
}
typedef enum DIRECTIVE_TYPE {DIRECTIVE_OBJECT = 0,  DIRECTIVE_SCORE = 1, DIRECTIVE_MOVEMENT = 2, DIRECTIVE_STAFF = 3, DIRECTIVE_VOICE = 4, DIRECTIVE_KEYSIG = 5, DIRECTIVE_TIMESIG = 6} DIRECTIVE_TYPE;

static void
call_edit_on_action (GtkWidget *button, DIRECTIVE_TYPE score_edit)
{
   DenemoScriptParam param;
   GtkAction *action = (GtkAction*)g_object_get_data (G_OBJECT (button), "action");
   GList *currentobject = Denemo.project->movement->currentobject;
   param.string = g_string_new ("edit");
   g_debug ("Script can look for params \"edit\" - a string to catch this");
   activate_script (action, &param);
   g_string_free (param.string, TRUE);  
   gtk_widget_destroy (gtk_widget_get_toplevel (button));
   if (score_edit)
    {
        if (score_edit==DIRECTIVE_SCORE)
            edit_score_properties ();
        else
        if (score_edit==DIRECTIVE_MOVEMENT)
            edit_movement_properties ();
        else
        if (score_edit==DIRECTIVE_VOICE)
            edit_voice_properties ();
        else
            edit_staff_properties ();//also for KEYSIG AND TIMESIG
        
    } else
    {
       if (!score_edit && (currentobject == Denemo.project->movement->currentobject))
        edit_object();
       else 
        reset_cursors (); 
    }
}

static void execute_editscript (GtkWidget *button, gchar *filename)
{
 gchar *keep = g_strdup (filename);
 gtk_widget_destroy (gtk_widget_get_toplevel (button));
 GError *error = (GError *) execute_script_file (keep);
 g_free (keep);
 GList *currentobject = Denemo.project->movement->currentobject;

 if (error)
    g_warning ("%s", error->message);
 
 if (currentobject == Denemo.project->movement->currentobject)
    edit_object();
 else
    reset_cursors ();
}

/* linked to type_str[] array!!! */
typedef enum {
  EDIT_CHORD = 0,
  EDIT_NOTE,
  EDIT_TUPLET_START,
  EDIT_TUPLET_END,
  EDIT_CLEF,
  EDIT_KEY,
  EDIT_TIMESIG,
  EDIT_STEMDIR,
  EDIT_STANDALONE,
  
} EditObjectType;

static gchar *type_str [] =
{
 "chord",
  "note",
  "tuplet",
  "tuplet",
  "clef",
  "keysig",
  "timesig",
  "stemdirective",
  "standalone",      
};
static void general_edit_popup (GtkWidget *button, EditObjectType type)
{

#ifdef G_OS_WIN32
//the popup does not work on windows...
#else
 if (type == EDIT_CHORD)
            popup_menu ("/NoteEditPopup");
 else
#endif
 infodialog (_("To add or remove built-in attributes right click on the object in the display window"));
       
    gtk_widget_destroy (gtk_widget_get_parent (gtk_widget_get_parent (button)));
    reset_cursors ();
 
}
static void display_help(gchar *help)
    {
       infowarningdialog (help, TRUE); 
    }
    
#if GTK_MAJOR_VERSION == 2
#define GdkRGBA GdkColor
#define gtk_widget_override_color gtk_widget_modify_fg
#define gtk_widget_override_background_color gtk_widget_modify_bg
#define GTK_STATE_FLAG_NORMAL (0)
static void get_color (GdkColor *color, gdouble r, gdouble g, gdouble b, gdouble a) {
    gchar *col = g_strdup_printf ( "#%02x%02x%02x", (gint)(r*254),(gint)(g*254),(gint)(b*254));
    gdk_color_parse (col, color);
    g_free(col);
}
#else
static void get_color (GdkRGBA *color, gdouble r, gdouble g, gdouble b, gdouble a) {
            color->red = r; color->green = g;
            color->blue = b; 
            color->alpha = a;
            }
#endif 

static void
place_directives (GtkWidget *vbox, GList **pdirectives, EditObjectType type)
{
    GList *directives = *pdirectives;
    for (; directives;directives = directives->next)
        {
            DenemoDirective *directive = directives->data;
            const gchar *label = get_label_for_command (directive->tag->str);
            GtkAction *action = lookup_action_from_name (directive->tag->str);
            gchar *name = label?(gchar*)label:directive->tag->str;
            const gchar *tooltip = get_tooltip_for_command (directive->tag->str);

            gchar *filename = get_editscript_filename (directive->tag->str);

            if (!label)
                label = directive->tag->str;
            GtkWidget *expander = gtk_expander_new (label);
            gtk_expander_set_expanded (GTK_EXPANDER(expander), TRUE);
            gtk_widget_set_sensitive (expander, TRUE);
            gtk_container_set_border_width (GTK_CONTAINER (expander), 0);
            gtk_box_pack_start (GTK_BOX (vbox), expander, FALSE, TRUE, 0);
            GtkWidget *inner_box = gtk_vbox_new (FALSE, 0);
            gtk_container_add (GTK_CONTAINER (expander), inner_box);

            if (filename)
                {
                gchar *thelabel = g_strconcat (_("Run the Edit Script for "), name, NULL);
                GtkWidget *button = gtk_button_new_with_label (thelabel);
                g_signal_connect (button, "clicked", G_CALLBACK (execute_editscript), filename);
                g_signal_connect_swapped (button, "destroy", G_CALLBACK (g_free), filename);
                gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);   
                g_free(thelabel);
                }  
            else if (action)
                {
                gchar *thelabel = g_strconcat (_("Execute command: "), name, NULL);
                GtkWidget *button = gtk_button_new_with_label (thelabel);
                gtk_widget_set_tooltip_text (button, _("Re-run the command to edit the Denemo Directive"));
                g_object_set_data (G_OBJECT(button), "action", (gpointer)action);
                g_signal_connect (button, "clicked", G_CALLBACK (call_edit_on_action), NULL);
                gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);                       
                g_free(thelabel);  
                }


            fn_type *func;
            switch (type) {
                case EDIT_NOTE:
                        func = text_edit_note_directive;
                        break;
                case EDIT_CHORD:
                        func = text_edit_chord_directive;
                        break;   
                case EDIT_CLEF:
                        func = text_edit_clef_directive;
                        break;   
                case EDIT_KEY:
                        func = text_edit_keysig_directive;
                        break;   
                case EDIT_TIMESIG:
                        func = text_edit_timesig_directive;
                        break;   
                case EDIT_STEMDIR:
                        func = text_edit_stemdirective_directive;
                        break;
                default:
                        g_critical ("Unknown type");
                        func = text_edit_standalone_directive;
                        break;
                
            }
            GtkWidget *hbox = gtk_hbox_new (FALSE, 0);
            gtk_box_pack_start (GTK_BOX (inner_box), hbox, FALSE, TRUE, 0);
            
            //gtk_widget_override_color (inner_box, GTK_STATE_FLAG_NORMAL, &color);
            GtkWidget *button = gtk_button_new_with_label (_("Delete"));
            GtkWidget *labelwidget = (GtkWidget *) gtk_bin_get_child (GTK_BIN (button));
                
            GdkRGBA color;
            get_color (&color, 1.0, 0.0, 0.0, 1.0);
            gtk_widget_override_color (labelwidget, GTK_STATE_FLAG_NORMAL, &color);
            g_object_set_data (G_OBJECT(button), "directives", (gpointer)pdirectives);
            g_object_set_data (G_OBJECT(button), "directive", (gpointer)directive);
            g_signal_connect (button, "clicked", G_CALLBACK (delete_directive), func);
            gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 30);
            
            if (tooltip)
                {
                    button = gtk_button_new_with_label (_("Help"));
                    get_color (&color, 0.0, 0.7, 0.7, 1.0);//color.red = 0.0; color.green = 0.7,  color.blue = 0.3; color.alpha = 1.0;
                    gtk_widget_override_color (button, GTK_STATE_FLAG_NORMAL, &color);
                    g_signal_connect_swapped (G_OBJECT(button), "clicked", G_CALLBACK (display_help), (gpointer)tooltip);
                    gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
                }            
            if (tooltip == NULL) tooltip = _("No tooltip");

            if (action) {
                button = gtk_button_new_with_label (_("Create Button for Command"));
                gtk_widget_set_tooltip_text (button, _( "Make a palette button for running the command that created this attribute."));
                g_object_set_data (G_OBJECT(button), "directive", (gpointer)directive);
                g_signal_connect (button, "clicked", G_CALLBACK (create_palette_button_for_command), (gpointer)tooltip);
                gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);   
            }
            
            button = gtk_button_new_with_label (_("Create Button for Clone"));
            gtk_widget_set_tooltip_text (button, _( "Make a palette button for installing this attribute elsewhere."));
            g_object_set_data (G_OBJECT(button), "directive", (gpointer)directive);
            g_signal_connect (button, "clicked", G_CALLBACK (create_palette_button_for_directive), (gpointer)(type_str[type]));
            gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
            
            button = gtk_button_new_with_label (_("Advanced"));
            gtk_widget_set_tooltip_text (button, _( "Examine/Edit this directive at a low-level"));
            g_object_set_data (G_OBJECT(button), "directives", (gpointer)pdirectives);
            g_object_set_data (G_OBJECT(button), "directive", (gpointer)directive);
            g_signal_connect (button, "clicked", G_CALLBACK (advanced_edit_type_directive), func);
            gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
        }
}
static void
place_chord_attributes (GtkWidget *vbox, chord *thechord)
{
    GtkWidget *expander = gtk_expander_new (_("Built-in Chord Attributes"));//gtk_expander_new
    gtk_expander_set_expanded (GTK_EXPANDER(expander), TRUE);
    gtk_widget_set_sensitive (expander, TRUE);
    gtk_container_set_border_width (GTK_CONTAINER (expander),10);
    GdkRGBA color;
    get_color (&color, 0.1, 0.1, 0.8, 1.0);
    gtk_widget_override_color (expander, GTK_STATE_FLAG_NORMAL, &color);
    gtk_box_pack_start (GTK_BOX (vbox), expander, FALSE, TRUE, 0);
    GtkWidget *inner_box = gtk_vbox_new (FALSE, 0);
    gtk_container_add (GTK_CONTAINER (expander), inner_box);
    GtkWidget *hbox = gtk_hbox_new (FALSE, 0);
    gtk_box_pack_start (GTK_BOX (inner_box), hbox, FALSE, TRUE, 0);
    inner_box = gtk_vbox_new (FALSE, 0);
    gtk_box_pack_start (GTK_BOX (hbox), inner_box, FALSE, TRUE, 30);
    if (thechord->slur_begin_p)
          {
            GtkWidget *button = gtk_button_new_with_label (_("Remove slur start"));
            g_signal_connect (button, "clicked", G_CALLBACK (set_false), &thechord->slur_begin_p);
            gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);
        }
    if (thechord->slur_end_p)
            {
            GtkWidget *button = gtk_button_new_with_label (_("Remove slur end"));
            g_signal_connect (button, "clicked", G_CALLBACK (set_false), &thechord->slur_end_p);
            gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);
            }
    if (thechord->is_tied)
            {
            GtkWidget *button = gtk_button_new_with_label (_("Remove tie"));
            g_signal_connect (button, "clicked", G_CALLBACK (set_false), &thechord->is_tied);
            gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);
            }
    if (thechord->crescendo_begin_p)
            {
            GtkWidget *button = gtk_button_new_with_label (_("Remove Start cresc. marking"));
            g_signal_connect (button, "clicked", G_CALLBACK (set_false), &thechord->crescendo_begin_p);
            gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);
            }
    if (thechord->crescendo_end_p)
            {
            GtkWidget *button = gtk_button_new_with_label (_("Remove End cresc. marking"));
            g_signal_connect (button, "clicked", G_CALLBACK (set_false), &thechord->crescendo_end_p);
            gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);
            }
    if (thechord->diminuendo_begin_p)
            {
            GtkWidget *button = gtk_button_new_with_label (_("Remove Start dim. marking"));
            g_signal_connect (button, "clicked", G_CALLBACK (set_false), &thechord->diminuendo_begin_p);
            gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);
            }
    if (thechord->diminuendo_end_p)
            {
            GtkWidget *button = gtk_button_new_with_label (_("Remove End dim. marking"));
            g_signal_connect (button, "clicked", G_CALLBACK (set_false), &thechord->diminuendo_end_p);
            gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);
            }
                  
    if (thechord->is_grace)
            {
            GtkWidget *button = gtk_button_new_with_label (_("Un-grace the note"));
            g_signal_connect (button, "clicked", G_CALLBACK (set_false), &thechord->is_grace);
            gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);
            }
    if(!gtk_container_get_children (GTK_CONTAINER (inner_box)))
        gtk_widget_destroy (expander);
}

static void update_and_close (GtkWidget *editwin)
{
    update_object_info ();
    gtk_widget_destroy (editwin);
    reset_cursors ();
}

static void run_script (GtkWidget *button, gchar *script)
{
    call_out_to_guile (script);
    gtk_widget_destroy (gtk_widget_get_parent (gtk_widget_get_parent (button)));
    reset_cursors ();
}

static void delete_standalone (GtkWidget *button)
{
    dnm_deleteobject (Denemo.project->movement);
    gtk_widget_destroy (gtk_widget_get_toplevel (button));
    score_status(Denemo.project, TRUE);
    reset_cursors (); 
}
// edit the specific object at the cursor
void
edit_object (void)
{
    DenemoMovement *si = Denemo.project->movement;
    DenemoObject *curObj = get_object ();
    if (curObj == NULL)
    {
      warningmessage (_("No object here to edit"));
      return;
    }
    GtkWidget *editwin = gtk_window_new (GTK_WINDOW_TOPLEVEL); 
    GdkRGBA color;
    get_color (&color, 1.0, 1.0, 1.0, 1.0);//.red = color.green = color.blue = color.alpha = 1.0;
    gtk_widget_override_background_color (editwin, GTK_STATE_FLAG_NORMAL, &color);
    gtk_window_set_modal (GTK_WINDOW (editwin), TRUE);
    gtk_window_set_title (GTK_WINDOW (editwin), _("Denemo Object Editor"));
    gtk_window_set_keep_above (GTK_WINDOW (editwin), TRUE);
    GtkWidget *vbox = gtk_vbox_new (FALSE, 0);
    gtk_container_add (GTK_CONTAINER (editwin), vbox);
    GtkWidget *close_button = gtk_button_new_with_label (_("Close"));
    g_signal_connect_swapped (close_button, "clicked", G_CALLBACK (update_and_close), editwin);
    
    g_signal_connect (G_OBJECT (editwin), "destroy", G_CALLBACK (reset_cursors), NULL);
    gtk_box_pack_start (GTK_BOX (vbox), close_button, FALSE, TRUE, 0);
    GtkWidget *hbox = gtk_hbox_new (FALSE, 0);
    gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);

    GtkWidget *button = gtk_button_new_with_label (_("⬅ Previous Object"));
    g_signal_connect_swapped (button, "clicked", G_CALLBACK (go_left), editwin);
    gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
    
    if ((!si->currentobject->prev)&&(!si->currentmeasure->prev))
        gtk_widget_set_sensitive (button, FALSE);

    GtkWidget *note_up_button = gtk_button_new_with_label (_("Next note in chord"));
    g_signal_connect_swapped (note_up_button, "clicked", G_CALLBACK (move_to_next_note), editwin);
    gtk_box_pack_start (GTK_BOX (hbox), note_up_button, FALSE, TRUE, 0);
    
    button = gtk_button_new_with_label (_("Next Object ➡"));
    g_signal_connect_swapped (button, "clicked", G_CALLBACK (go_right), editwin);
    gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
    
    if ((!si->currentobject->next) && (!si->currentmeasure->next))
        gtk_widget_set_sensitive (button, FALSE);
    
    switch (curObj->type)
    {
        case CHORD:
          {
            chord *thechord = ((chord *) curObj->object);
            if (thechord->directives) 
                {
                GtkWidget *frame = gtk_frame_new (thechord->notes? _("Attached to the chord:"):
                                                                   _("Attached to the rest:"));
                gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
                GdkRGBA color;
                get_color (&color, 0.8, 0.1, 0.1, 1.0);//color.red = 0.8;color.green = color.blue = 0.1; color.alpha = 1;
                gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
                gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
                GtkWidget *inner_box = gtk_vbox_new (FALSE, 0);
                get_color (&color, 0.1, 0.8, 0.1, 1.0);
                gtk_widget_override_color (inner_box, GTK_STATE_FLAG_NORMAL, &color);
                gtk_container_add (GTK_CONTAINER (frame), inner_box);
                place_directives (inner_box, &thechord->directives, EDIT_CHORD);
                }
                
            if (thechord->notes)
              {
                place_chord_attributes (vbox, thechord);
                note *thenote = findnote (curObj, Denemo.project->movement->cursor_y);
                if (!thechord->notes->next)
                    gtk_button_set_label (GTK_BUTTON (note_up_button), _("Edit the note"));
                if ( (!thenote) || thechord->notes->next || (Denemo.project->movement->cursor_y!=thenote->mid_c_offset))
                    {
                        note_up_button = NULL; // a tricksy bit of code this: the button is already packed in the vbox, by setting this NULL we stop it being set insensitive as it must be for all other cases.
                    }
                if (thenote && Denemo.project->movement->cursor_y==thenote->mid_c_offset)
                  {
                    GString *text = g_string_new ("");
                    
                    if (thenote->directives)
                      {
                        g_string_append_printf (text, _("Attached to Note %s"),
                                            pretty_name (mid_c_offsettolily (thenote->mid_c_offset, thenote->enshift)));
                        GtkWidget *frame = gtk_frame_new (text->str);
                        gtk_container_set_border_width (GTK_CONTAINER (frame), 20);
                        gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
                        GdkRGBA color;
                        get_color (&color, 0.2, 0.7, 0.2, 1.0);/// color.green = 0.7; color.red = color.blue = 0.2; color.alpha = 1;
                        gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
                        gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
                        GtkWidget *inner_box = gtk_vbox_new (FALSE, 0);
                        get_color (&color, 0.1, 0.8, 0.1, 1.0);
                        gtk_widget_override_color (inner_box, GTK_STATE_FLAG_NORMAL, &color);

                        gtk_container_add (GTK_CONTAINER (frame), inner_box);
                        place_directives (inner_box, &thenote->directives, EDIT_NOTE);
                      } 
                    else
                      {
                        g_string_append_printf (text, _("Nothing attached to Note %s"),
                                            pretty_name (mid_c_offsettolily (thenote->mid_c_offset, thenote->enshift)));
                        GtkWidget *frame = gtk_frame_new (text->str);
                        gtk_container_set_border_width (GTK_CONTAINER (frame), 20);
                        gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
                        GdkRGBA color;
                        get_color (&color, 0.8, 0.1, 0.1, 1.0);//color.red = 0.8;color.green = color.blue = 0.1; color.alpha = 1;
                        gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
                        gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
                      }
                      
                      g_string_free (text, TRUE);
                      
                  }
                if ((thechord->notes) && curObj->isinvisible)
                    {
                        GtkWidget *button = gtk_button_new_with_label (_("Assign Pitch to Rhythm"));
                        g_signal_connect (button, "clicked", G_CALLBACK (set_false), &curObj->isinvisible);
                        gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, TRUE, 0);
                    }
            }
            else
              {
                //type = _("rest");
                place_chord_attributes (vbox, thechord);
    
              }
              if (thechord->fakechord)
                {
                    GtkWidget *button = gtk_button_new_with_label (_("Edit Chord Symbol"));
                    g_signal_connect (button, "clicked", G_CALLBACK (run_script), "(d-EditChords)");
                    gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, TRUE, 0);
                }

            if (thechord->figure)
                {
                    GtkWidget *button = gtk_button_new_with_label (_("Edit Bass Figure"));
                    g_signal_connect (button, "clicked", G_CALLBACK (run_script), "(d-EditFiguredBass)");
                    gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, TRUE, 0);
                }

            {
            GtkWidget *separator = 
#if GTK_MAJOR_VERSION == 2
            gtk_hseparator_new ();
#else                
            gtk_separator_new (GTK_ORIENTATION_HORIZONTAL);
#endif                   
            gtk_box_pack_start (GTK_BOX (vbox), separator, FALSE, TRUE, 0);
             
            GtkWidget *button = gtk_button_new_with_label (_("Add/Remove attributes"));
            g_signal_connect (button, "clicked", G_CALLBACK (general_edit_popup), EDIT_CHORD);
            gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, TRUE, 0);
            }
       
        }
        break;
        case TUPOPEN:
          { tuplet *thetup = ((tuplet *) curObj->object);
            //type = _("start tuplet marker");
            GtkWidget *button = gtk_button_new_with_label (_("Alter Tuplet Type"));
            g_signal_connect_swapped (button, "clicked", G_CALLBACK (call_out_to_guile), "(d-StartTuplet)");
            gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, TRUE, 0);
            if (thetup->directives) 
                {
                GtkWidget *frame = gtk_frame_new (_("Attached to the tuplet start:"));
                GdkRGBA color;
                get_color (&color, 0.8, 0.1, 0.1, 1.0);
                gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
                gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
                gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
                GtkWidget *inner_box = gtk_vbox_new (FALSE, 8);
                gtk_container_add (GTK_CONTAINER (frame), inner_box);
                place_directives (inner_box, &thetup->directives, EDIT_TUPLET_START);
                }
          }
          break;
        case TUPCLOSE:
            { tuplet *thetup = ((tuplet *) curObj->object);
            //type = _("start tuplet marker");
            if (thetup->directives) 
                {
                GtkWidget *frame = gtk_frame_new (_("Attached to the tuplet end:"));
                GdkRGBA color;
                get_color (&color, 0.8, 0.1, 0.1, 1.0);
                gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
                gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
                gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
                GtkWidget *inner_box = gtk_vbox_new (FALSE, 8);
                gtk_container_add (GTK_CONTAINER (frame), inner_box);
                place_directives (inner_box, &thetup->directives, EDIT_TUPLET_END);
                }
          }
          break;

        case CLEF:
          {
            clef *theclef = ((clef *) curObj->object);
            //type = _("clef change object");
             
            if (theclef->directives) 
                {
                GtkWidget *frame = gtk_frame_new (_("Attached to the clef change object:"));
                GdkRGBA color;
                get_color (&color, 0.8, 0.1, 0.1, 1.0);
                gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
                gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
                gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
                GtkWidget *inner_box = gtk_vbox_new (FALSE, 8);
                gtk_container_add (GTK_CONTAINER (frame), inner_box);
                place_directives (inner_box, &theclef->directives, EDIT_CLEF);
                }
            if (curObj->isinvisible)
                {
                GtkWidget *button = gtk_button_new_with_label (_("Transform to printing clef"));
                g_signal_connect (button, "clicked", G_CALLBACK (set_false), &curObj->isinvisible);
                gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, TRUE, 0);
                }
          }
          break;

        case TIMESIG:
          {
            timesig *thetime = ((timesig *) curObj->object);
            //type = _("time signature change object");
            if (thetime->directives) 
                { 
                GtkWidget *frame = gtk_frame_new (_("Attached to the time signature change object:"));
                GdkRGBA color;
                get_color (&color, 0.8, 0.1, 0.1, 1.0);
                gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
                gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
                gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
                GtkWidget *inner_box = gtk_vbox_new (FALSE, 8);
                gtk_container_add (GTK_CONTAINER (frame), inner_box);
                place_directives (inner_box, &thetime->directives, EDIT_TIMESIG);
                }   
           }
          break;

        case KEYSIG:
          {
            keysig *thekey = ((keysig *) curObj->object);
            //type = _("key signature change object");
            if (thekey->directives) 
                {                 
                GtkWidget *frame = gtk_frame_new (_("Attached to the key signature change object:"));
                                GdkRGBA color;
                get_color (&color, 0.8, 0.1, 0.1, 1.0);
                gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
                gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
                gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
                GtkWidget *inner_box = gtk_vbox_new (FALSE, 8);
                gtk_container_add (GTK_CONTAINER (frame), inner_box);
                place_directives (inner_box, &thekey->directives, EDIT_KEY);
                }   
            }
          break;
          
       
        case STEMDIRECTIVE:
          {
            stemdirective *thestem = ((stemdirective *) curObj->object);
            //type = _("stem direction change object");
            if (thestem->directives) 
                             {                 
                GtkWidget *frame = gtk_frame_new (_("Attached to the stemming change object:"));
                                GdkRGBA color;
                get_color (&color, 0.8, 0.1, 0.1, 1.0);
                gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
                gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
                gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
                GtkWidget *inner_box = gtk_vbox_new (FALSE, 8);
                gtk_container_add (GTK_CONTAINER (frame), inner_box);
                place_directives (inner_box, &thestem->directives, EDIT_STEMDIR);
                }   
                            
          }
          break;

        case LILYDIRECTIVE:
            {
            DenemoDirective *directive = (DenemoDirective *) curObj->object;
            const gchar *label = get_label_for_command (directive->tag->str);
            GtkAction *action = lookup_action_from_name (directive->tag->str);
            gchar *name = label?(gchar*)label:directive->tag->str;
            const gchar *tooltip = get_tooltip_for_command (directive->tag->str);
            gchar *filename = get_editscript_filename (directive->tag->str);
            GtkWidget *frame = gtk_frame_new ( _("Standalone Denemo Directive:"));
            gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
            GdkRGBA color;
            get_color (&color, 0.5, 0.5, 0.1, 1.0);// .red = 0.5;
            //color.green = 0.5; color.blue = 0.1; color.alpha = 1;
            gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
            gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
            GtkWidget *inner_box = gtk_vbox_new (FALSE, 0);
            get_color (&color, 0.8, 0.1, 0.1, 1.0);
            gtk_widget_override_color (inner_box, GTK_STATE_FLAG_NORMAL, &color);
            gtk_container_add (GTK_CONTAINER (frame), inner_box);
                
             if (filename)
                {
                gchar *thelabel = g_strconcat ( _("Run the Edit Script for "), name, NULL);
                GtkWidget *button = gtk_button_new_with_label (thelabel);
                g_signal_connect (button, "clicked", G_CALLBACK (execute_editscript), filename);
                g_signal_connect_swapped (button, "destroy", G_CALLBACK (g_free), filename);
                gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);   
                g_free(thelabel);
                }  
            else if (action) 
                {
                gchar *thelabel = g_strconcat ( _("Execute command: "), name, NULL);
                GtkWidget *button = gtk_button_new_with_label (thelabel);
                gtk_widget_set_tooltip_text (button, _("Re-run the command to edit the Denemo Directive"));

                g_object_set_data (G_OBJECT(button), "action", (gpointer)action);
                g_signal_connect (button, "clicked", G_CALLBACK (call_edit_on_action), NULL);
                gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);                       
                g_free(thelabel);  
                }
            {
                GtkWidget *hbox = gtk_hbox_new (FALSE, 0);
                gtk_box_pack_start (GTK_BOX (inner_box), hbox, FALSE, TRUE, 0);
                
                
                button = gtk_button_new_with_label (_("Delete"));
                GtkWidget *labelwidget = (GtkWidget *) gtk_bin_get_child (GTK_BIN (button));
                get_color (&color, 1.0, 0.0, 0.0, 1.0);
                //color.red = 1.0; color.green = color.blue = 0.0; color.alpha = 1.0;
                gtk_widget_override_color (labelwidget, GTK_STATE_FLAG_NORMAL, &color);
                g_object_set_data (G_OBJECT(button), "directive", (gpointer)directive);
                g_signal_connect (button, "clicked", G_CALLBACK (delete_standalone), NULL);
                gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 30);
                
                if (tooltip)
                    {
                        button = gtk_button_new_with_label (_("Help"));
                        get_color (&color, 0.0, 0.7, 0.3, 1.0);//color.red = 0.0; color.green = 0.7,  color.blue = 0.3; color.alpha = 1.0;
                        gtk_widget_override_color (button, GTK_STATE_FLAG_NORMAL, &color);
                        g_signal_connect_swapped (G_OBJECT(button), "clicked", G_CALLBACK (display_help), (gpointer)tooltip);
                        gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
                    }
                if (tooltip == NULL) tooltip = _("No tooltip");
            
                if (action) {
                            button = gtk_button_new_with_label (_("Create Button for Command"));
                            gtk_widget_set_tooltip_text (button, _( "Make a palette button for running the command that created inserted this object."));
                            g_object_set_data (G_OBJECT(button), "directive", (gpointer)directive);
                            g_signal_connect (G_OBJECT(button), "clicked", G_CALLBACK (create_palette_button_for_command), (gpointer)tooltip);
                            gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);   
                            }
                button = gtk_button_new_with_label (_("Create Button for Clone"));
                gtk_widget_set_tooltip_text (button, _( "Make a palette button for inserting a clone of this object elsewhere."));
                g_object_set_data (G_OBJECT(button), "directive", (gpointer)directive);
                g_signal_connect (button, "clicked", G_CALLBACK (create_palette_button_for_directive), "standalone");
                gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
                
                button = gtk_button_new_with_label (_("Advanced"));
                g_object_set_data (G_OBJECT(button), "directives", NULL);
                g_object_set_data (G_OBJECT(button), "directive", (gpointer)directive);
                g_signal_connect (button, "clicked", G_CALLBACK (advanced_edit_type_directive), text_edit_standalone_directive);
                gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
            }
            }
            break;
            
        default:
            g_critical ("Type not done");
            break;
    }
  if(g_list_length ( gtk_container_get_children (GTK_CONTAINER(vbox))) == 1)
    {//just the close button
        warningdialog ("Nothing editable on this object\nYou can add attributes to the object at the cursor by right-clicking on it.");
        gtk_widget_destroy (editwin);
    }
    else
    {
      if (note_up_button)
            gtk_widget_set_sensitive (note_up_button, FALSE);
      gtk_widget_show_all (editwin);
      gtk_window_present (GTK_WINDOW (editwin));
      gdk_window_set_cursor (gtk_widget_get_window (editwin), gdk_cursor_new (GDK_RIGHT_PTR)); 
      gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), gdk_cursor_new (GDK_X_CURSOR));
      if(ObjectInfo)
        gdk_window_set_cursor (gtk_widget_get_window (ObjectInfo), gdk_cursor_new (GDK_X_CURSOR));
     if(Denemo.printarea && gtk_widget_get_window (Denemo.printarea))
        gdk_window_set_cursor (gtk_widget_get_window (Denemo.printarea), gdk_cursor_new (GDK_X_CURSOR));

    }
}






static void score_update_and_close (GtkWidget *editwin)
{
    gtk_widget_destroy (editwin);
    reset_cursors ();
}
static void go_previous(GtkWidget *editwin)
{
prev_movement (NULL, NULL);//FIXME pass in a DenemoParam * to get status
if (editwin)
   {
    gtk_widget_destroy (editwin);
    reset_cursors ();
    edit_movement_properties();
    }
}
static void go_next(GtkWidget *editwin)
{
next_movement (NULL, NULL);//FIXME pass in a DenemoParam * to get status
if (editwin)
   {
    gtk_widget_destroy (editwin);
    reset_cursors ();
    edit_movement_properties();
    }
}
typedef gboolean fn2_type (DenemoDirective*);
static void low_level_edit_type_directive (GtkWidget *button, gpointer fn)
{
    DenemoDirective *directive = (DenemoDirective*) g_object_get_data (G_OBJECT(button), "directive");
    GList **directives = (GList **)g_object_get_data (G_OBJECT(button), "directives");
    if (!( ((fn2_type *)fn) (directive)))
        {
            if(directives) 
                *directives = g_list_remove (*directives, directive);
            gtk_widget_destroy (gtk_widget_get_parent (gtk_widget_get_parent (button)));
            score_status(Denemo.project, TRUE);
        }
}
static void delete_score_directive (GtkWidget *button)
{
    DenemoDirective *directive = (DenemoDirective*) g_object_get_data (G_OBJECT(button), "directive");
    GList **directives = (GList **)g_object_get_data (G_OBJECT(button), "directives");
    *directives = g_list_remove (*directives, directive);
    gtk_widget_destroy (gtk_widget_get_parent (gtk_widget_get_parent (button)));
    score_status(Denemo.project, TRUE);    
}

static void place_buttons_for_directives (GList **pdirectives, GtkWidget *vbox, DIRECTIVE_TYPE score_or_movement, gchar *field)
{
    GList *g;
    gchar *type = "";
    if(!strcmp (field, "lilycontrol"))
        type = _( "Score ");
    else  if(!strcmp (field, "movementcontrol"))
        type = _( "Movement ");
    else  if(!strcmp (field, "scoreheader"))
        type = _( "Score Header ");
    else  if(!strcmp (field, "paper"))
        type = _( "Paper ");
    else  if(!strcmp (field, "header"))
        type = _( "Movement Header ");
    else  if(!strcmp (field, "layout"))
        type = _( "Movement Layout ");           
     for (g=*pdirectives;g;g=g->next)
      {
            DenemoDirective *directive = g->data;       
            const gchar *label = get_label_for_command (directive->tag->str);
            GtkAction *action = lookup_action_from_name (directive->tag->str);
            gchar *name = label?(gchar*)label:directive->tag->str;
            const gchar *tooltip = get_tooltip_for_command (directive->tag->str);
            gchar *filename = get_editscript_filename (directive->tag->str);
             GtkWidget *frame;
             gchar *text;
            if (label == NULL)
                text = g_strdup_printf( _("Denemo %s Directive tagged: %s"), type, name);
            else
                text = g_strdup_printf (_("Denemo %s Directive: %s"), type, label);
            frame = gtk_frame_new (text);
            g_free(text);
            //gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
            GdkRGBA color;
            get_color (&color, 0.5, 0.5, 0.1, 1.0); //color.red = 0.5;
            //color.green = 0.5; color.blue = 0.1; color.alpha = 1;
            gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
            gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
            GtkWidget *inner_box = gtk_vbox_new (FALSE, 0);
            get_color (&color, 0.8, 0.8, 0.1, 1.0);
            gtk_widget_override_color (inner_box, GTK_STATE_FLAG_NORMAL, &color);
            gtk_container_add (GTK_CONTAINER (frame), inner_box);
            GtkWidget *button;    
             if (filename)
                {
                gchar *thelabel = g_strconcat ( _("Run the Edit Script for "), name, NULL);
                button = gtk_button_new_with_label (thelabel);
                g_signal_connect (button, "clicked", G_CALLBACK (execute_editscript), filename);
                g_signal_connect_swapped (button, "destroy", G_CALLBACK (g_free), filename);
                gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);   
                g_free(thelabel);
                }  
            else if (action) 
                {
                gchar *thelabel = g_strconcat ( _("Execute command: "), name, NULL);
                button = gtk_button_new_with_label (thelabel);
                gtk_widget_set_tooltip_text (button, _("Re-run the command to edit the Denemo Directive"));

                g_object_set_data (G_OBJECT(button), "action", (gpointer)action);
                g_signal_connect (button, "clicked", G_CALLBACK (call_edit_on_action), GINT_TO_POINTER(score_or_movement));
                gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);                       
                g_free(thelabel);  
                }
            if (tooltip)
                    gtk_widget_set_tooltip_text (button, tooltip);
        
            GtkWidget *hbox = gtk_hbox_new (FALSE, 0);
            gtk_box_pack_start (GTK_BOX (inner_box), hbox, FALSE, TRUE, 0);
            
            button = gtk_button_new_with_label (_("Delete"));
            GtkWidget *labelwidget = (GtkWidget *) gtk_bin_get_child (GTK_BIN (button));
            get_color (&color, 1.0, 0.0, 0.0, 1.0);
            //color.red = 1.0; color.green = color.blue = 0.0; color.alpha = 1.0;
            gtk_widget_override_color (labelwidget, GTK_STATE_FLAG_NORMAL, &color);
            g_object_set_data (G_OBJECT(button), "directives", (gpointer)pdirectives);
            g_object_set_data (G_OBJECT(button), "directive", (gpointer)directive);
            g_signal_connect (button, "clicked", G_CALLBACK (delete_score_directive), NULL);
            gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 30);
            
             if (tooltip)
            {
                button = gtk_button_new_with_label (_("Help"));
                get_color (&color, 0.0, 0.7, 0.3, 1.0);
                gtk_widget_override_color (button, GTK_STATE_FLAG_NORMAL, &color);
                g_signal_connect_swapped (G_OBJECT(button), "clicked", G_CALLBACK (display_help), (gpointer)tooltip);
                gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
            }
            if (tooltip == NULL) tooltip = _("No tooltip");

            if (action) {
                button = gtk_button_new_with_label (_("Create Button for Command"));
            gtk_widget_set_tooltip_text (button, _( "Make a palette button for running the command that created this attribute."));
            g_object_set_data (G_OBJECT(button), "directive", (gpointer)directive);
            g_signal_connect (G_OBJECT(button), "clicked", G_CALLBACK (create_palette_button_for_command), (gpointer)tooltip);
            gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);   
            }
            button = gtk_button_new_with_label (_("Create Button for Clone"));
            gtk_widget_set_tooltip_text (button, _( "Make a palette button for installing a clone of this attribute elsewhere."));
            g_object_set_data (G_OBJECT(button), "directive", (gpointer)directive);
            g_signal_connect (button, "clicked", G_CALLBACK (create_palette_button_for_directive), (gpointer)(field));
            gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
    
    
            button = gtk_button_new_with_label (_("Advanced"));
            g_object_set_data (G_OBJECT(button), "directives", pdirectives);
            g_object_set_data (G_OBJECT(button), "directive", (gpointer)directive);
            g_signal_connect (G_OBJECT(button), "clicked", G_CALLBACK (low_level_edit_type_directive), low_level_directive_edit);
            gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
            
           
            
            
        
    }
}


static void
edit_score_and_movement_properties (gboolean show_score)
{
    GtkWidget *editscorewin = gtk_window_new (GTK_WINDOW_TOPLEVEL); 
    GdkRGBA color;
    gint window_height = 800;
    //color.red = color.green = color.blue = color.alpha = 1.0;
    get_color (&color, 1.0, 1.0, 1.0, 1.0);
    gtk_widget_override_background_color (editscorewin, GTK_STATE_FLAG_NORMAL, &color);
    gtk_window_set_modal (GTK_WINDOW (editscorewin), TRUE);
    gtk_window_set_title (GTK_WINDOW (editscorewin), _("Score and Movement Properties Editor"));
    gtk_window_set_keep_above (GTK_WINDOW (editscorewin), TRUE);
    gtk_window_set_default_size (GTK_WINDOW (editscorewin), 600, window_height);
    
    
    GtkWidget *vbox = gtk_vbox_new (FALSE, 0);
    gtk_container_add (GTK_CONTAINER (editscorewin), vbox);   
    GtkWidget *close_button = gtk_button_new_with_label (_("Close"));
    g_signal_connect_swapped (close_button, "clicked", G_CALLBACK (score_update_and_close), editscorewin);
    
    g_signal_connect (G_OBJECT (editscorewin), "destroy", G_CALLBACK (reset_cursors), NULL);
    gtk_box_pack_start (GTK_BOX (vbox), close_button, FALSE, TRUE, 0);

    GtkWidget *button;
    GtkWidget *pane;
#if GTK_MAJOR_VERSION == 2
    pane = gtk_vpaned_new ();
#else                                                       
    pane = gtk_paned_new (GTK_ORIENTATION_VERTICAL);
#endif
    gtk_box_pack_start (GTK_BOX (vbox), pane, TRUE, TRUE, 0);
    GtkWidget *expander = gtk_expander_new (_("Score Properties"));
    gtk_expander_set_expanded (GTK_EXPANDER(expander), show_score);
    gtk_widget_set_sensitive (expander, TRUE);
    gtk_container_set_border_width (GTK_CONTAINER (expander),10);
    get_color (&color, 0.1, 0.8, 0.1, 1.0);

    gtk_widget_override_color (expander, GTK_STATE_FLAG_NORMAL, &color);
   // gtk_box_pack_start (GTK_BOX (vbox), expander, TRUE, TRUE, 0);
   
   GtkWidget *frame = gtk_frame_new (NULL);
   //gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_IN);

   
   
    gtk_paned_add1 (GTK_PANED(pane), frame);
    
    gtk_container_add (GTK_CONTAINER (frame), expander);
    
    GtkWidget *scrolled_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_container_add (GTK_CONTAINER (expander), scrolled_window);
    
    GtkWidget *inner_box = gtk_vbox_new (FALSE, 0);
    gtk_scrolled_window_add_with_viewport  (GTK_SCROLLED_WINDOW(scrolled_window),  inner_box);

    button = gtk_button_new_with_label (_("Edit Built-in Score Properties"));
    g_signal_connect (button, "clicked", G_CALLBACK (score_properties_dialog), NULL);
    gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);
   
    place_buttons_for_directives ((GList**)&Denemo.project->lilycontrol.directives, inner_box, DIRECTIVE_SCORE, "lilycontrol");
    place_buttons_for_directives ((GList**)&Denemo.project->scoreheader, inner_box, DIRECTIVE_SCORE, "scoreheader");
    place_buttons_for_directives ((GList**)&Denemo.project->paper, inner_box, DIRECTIVE_SCORE, "paper");
    

    
    gchar *mnum = g_strdup_printf ("%s %d %s", _("Movement"), Denemo.project->movement->currentmovementnum, _("Properties"));
    expander = gtk_expander_new (mnum);
    g_free(mnum);
    gtk_expander_set_expanded (GTK_EXPANDER(expander), !show_score);
    gtk_widget_set_sensitive (expander, TRUE);
    gtk_container_set_border_width (GTK_CONTAINER (expander),10);
    get_color (&color, 0.1, 0.1, 0.8, 1.0);
    
    gtk_widget_override_color (expander, GTK_STATE_FLAG_NORMAL, &color);
 
    frame = gtk_frame_new (NULL);
    //gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_IN);

    gtk_paned_add2 (GTK_PANED(pane), frame);
    gtk_container_add (GTK_CONTAINER (frame), expander);
     
    scrolled_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_container_add (GTK_CONTAINER (expander), scrolled_window);
    inner_box = gtk_vbox_new (FALSE, 0);
    gtk_scrolled_window_add_with_viewport  (GTK_SCROLLED_WINDOW(scrolled_window),  inner_box);
     
    GtkWidget *hbox = gtk_hbox_new (FALSE, 0);
    gtk_box_pack_start (GTK_BOX (inner_box), hbox, FALSE, TRUE, 0);

    button = gtk_button_new_with_label (_("⬅ Previous Movement"));
    g_signal_connect_swapped (button, "clicked", G_CALLBACK (go_previous), editscorewin);
    gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
    
    if ( Denemo.project->movement->currentmovementnum == 1)
        gtk_widget_set_sensitive (button, FALSE);
    
    button = gtk_button_new_with_label (_("Next Movement ➡"));
    g_signal_connect_swapped (button, "clicked", G_CALLBACK (go_next), editscorewin);
    gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
    if (g_list_length(Denemo.project->movements) == Denemo.project->movement->currentmovementnum)
        gtk_widget_set_sensitive (button, FALSE);
 
 
    place_buttons_for_directives ((GList**)&Denemo.project->movement->movementcontrol, inner_box, DIRECTIVE_MOVEMENT, "movementcontrol");
    place_buttons_for_directives ((GList**)&Denemo.project->movement->header, inner_box, DIRECTIVE_MOVEMENT, "header");
    place_buttons_for_directives ((GList**)&Denemo.project->movement->layout, inner_box, DIRECTIVE_MOVEMENT, "layout");
 
   gtk_paned_set_position (GTK_PANED(pane), show_score? window_height-50 : 50);
 
 
 
  if(g_list_length ( gtk_container_get_children (GTK_CONTAINER(vbox))) == 1)
    {//just the close button
        warningdialog ("No properties have been set on the current score.");
        gtk_widget_destroy (editscorewin);
    }
    else
    {
      gtk_widget_show_all (editscorewin);
      gtk_window_present (GTK_WINDOW (editscorewin));
      gdk_window_set_cursor (gtk_widget_get_window (editscorewin), gdk_cursor_new (GDK_RIGHT_PTR)); 
      gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), gdk_cursor_new (GDK_X_CURSOR));
      if(ObjectInfo)
        gdk_window_set_cursor (gtk_widget_get_window (ObjectInfo), gdk_cursor_new (GDK_X_CURSOR));
     if(Denemo.printarea && gtk_widget_get_window (Denemo.printarea))
        gdk_window_set_cursor (gtk_widget_get_window (Denemo.printarea), gdk_cursor_new (GDK_X_CURSOR));

    }
 
}


void edit_score_properties (void)
{
    edit_score_and_movement_properties (TRUE);
}

void edit_movement_properties (void)
{
    edit_score_and_movement_properties (FALSE);
}



static void
edit_staff_and_voice_properties (gboolean show_staff)
{
    GtkWidget *editstaffwin = gtk_window_new (GTK_WINDOW_TOPLEVEL); 
    GdkRGBA color;
    gint window_height = 800;
    get_color (&color, 1.0, 1.0, 1.0, 1.0);
    gtk_widget_override_background_color (editstaffwin, GTK_STATE_FLAG_NORMAL, &color);
    gtk_window_set_modal (GTK_WINDOW (editstaffwin), TRUE);
    gtk_window_set_title (GTK_WINDOW (editstaffwin), _("Staff and Voice Properties Editor"));
    gtk_window_set_keep_above (GTK_WINDOW (editstaffwin), TRUE);
    gtk_window_set_default_size (GTK_WINDOW (editstaffwin), 400, window_height);
    
    
    GtkWidget *vbox = gtk_vbox_new (FALSE, 0);
    gtk_container_add (GTK_CONTAINER (editstaffwin), vbox);   
    GtkWidget *close_button = gtk_button_new_with_label (_("Close"));
    g_signal_connect_swapped (close_button, "clicked", G_CALLBACK (score_update_and_close), editstaffwin);
    
    g_signal_connect (G_OBJECT (editstaffwin), "destroy", G_CALLBACK (reset_cursors), NULL);
    gtk_box_pack_start (GTK_BOX (vbox), close_button, FALSE, TRUE, 0);

    GtkWidget *button;
    GtkWidget *pane;
#if GTK_MAJOR_VERSION == 2
    pane = gtk_vpaned_new ();
#else                                                       
    pane = gtk_paned_new (GTK_ORIENTATION_VERTICAL);
#endif
    gtk_box_pack_start (GTK_BOX (vbox), pane, TRUE, TRUE, 0);
    GtkWidget *expander = gtk_expander_new (_("Staff Properties"));
    gtk_expander_set_expanded (GTK_EXPANDER(expander), show_staff);
    gtk_widget_set_sensitive (expander, TRUE);
    gtk_container_set_border_width (GTK_CONTAINER (expander),10);
    get_color (&color, 0.1, 0.1, 0.8, 1.0);
    gtk_widget_override_color (expander, GTK_STATE_FLAG_NORMAL, &color);
   // gtk_box_pack_start (GTK_BOX (vbox), expander, TRUE, TRUE, 0);
   
   GtkWidget *frame = gtk_frame_new (NULL);
   //gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_IN);

   
   
    gtk_paned_add1 (GTK_PANED(pane), frame);
    
    gtk_container_add (GTK_CONTAINER (frame), expander);
    
    GtkWidget *scrolled_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_container_add (GTK_CONTAINER (expander), scrolled_window);
    
    GtkWidget *inner_box = gtk_vbox_new (FALSE, 0);
    gtk_scrolled_window_add_with_viewport  (GTK_SCROLLED_WINDOW(scrolled_window),  inner_box);

    button = gtk_button_new_with_label (_("Edit Built-in Staff Properties"));
    g_signal_connect (button, "clicked", G_CALLBACK (staff_properties_change_cb), NULL);
    gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);
    DenemoStaff *thestaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
    
    place_buttons_for_directives ((GList**)&thestaff->staff_directives, inner_box, DIRECTIVE_STAFF, "staff");
     if(thestaff->keysig.directives)
        {
        GtkWidget *label = gtk_label_new (_("Key Signature Directives"));
        gtk_box_pack_start (GTK_BOX (inner_box), label, FALSE, TRUE, 0);
        place_buttons_for_directives ((GList**)&(thestaff->keysig.directives), inner_box, DIRECTIVE_KEYSIG, "keysig");
        }
    if(thestaff->timesig.directives)
        {
        GtkWidget *label = gtk_label_new (_("Time Signature Directives"));
        gtk_box_pack_start (GTK_BOX (inner_box), label, FALSE, TRUE, 0);
        place_buttons_for_directives ((GList**)&(thestaff->timesig.directives), inner_box, DIRECTIVE_TIMESIG, "timesig");
        }
    expander = gtk_expander_new (_("Voice Properties"));

    gtk_expander_set_expanded (GTK_EXPANDER(expander), !show_staff);
    gtk_widget_set_sensitive (expander, TRUE);
    gtk_container_set_border_width (GTK_CONTAINER (expander),10);
    get_color (&color, 0.1, 0.1, 0.8, 1.0);
    gtk_widget_override_color (expander, GTK_STATE_FLAG_NORMAL, &color);
 
    frame = gtk_frame_new (NULL);
    //gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_IN);

    gtk_paned_add2 (GTK_PANED(pane), frame);
    gtk_container_add (GTK_CONTAINER (frame), expander);
     
    scrolled_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_container_add (GTK_CONTAINER (expander), scrolled_window);
    inner_box = gtk_vbox_new (FALSE, 0);
    gtk_scrolled_window_add_with_viewport  (GTK_SCROLLED_WINDOW(scrolled_window),  inner_box);
     
    GtkWidget *hbox = gtk_hbox_new (FALSE, 0);
    gtk_box_pack_start (GTK_BOX (inner_box), hbox, FALSE, TRUE, 0);

    button = gtk_button_new_with_label (_("Staff Above"));
    //g_signal_connect_swapped (button, "clicked", G_CALLBACK (staff_above), editstaffwin);
    gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
    
    if ( Denemo.project->movement->currentstaffnum == 1)
        gtk_widget_set_sensitive (button, FALSE);
    
    button = gtk_button_new_with_label (_("Staff Below"));
    //g_signal_connect_swapped (button, "clicked", G_CALLBACK (staff_below), editstaffwin);
    gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
    if (g_list_length(Denemo.project->movement->thescore) == Denemo.project->movement->currentstaffnum)
        gtk_widget_set_sensitive (button, FALSE);
 
 
    place_buttons_for_directives ((GList**)&thestaff->voice_directives, inner_box, DIRECTIVE_VOICE, "voice");

 
   gtk_paned_set_position (GTK_PANED(pane), show_staff? window_height-50 : 50);
 
 
 
  if(g_list_length ( gtk_container_get_children (GTK_CONTAINER(vbox))) == 1)
    {//just the close button
        warningdialog ("No properties have been set on the current score.");
        gtk_widget_destroy (editstaffwin);
    }
    else
    {
      gtk_widget_show_all (editstaffwin);
      gtk_window_present (GTK_WINDOW (editstaffwin));
      gdk_window_set_cursor (gtk_widget_get_window (editstaffwin), gdk_cursor_new (GDK_RIGHT_PTR)); 
      gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), gdk_cursor_new (GDK_X_CURSOR));
      if(ObjectInfo)
        gdk_window_set_cursor (gtk_widget_get_window (ObjectInfo), gdk_cursor_new (GDK_X_CURSOR));
     if(Denemo.printarea && gtk_widget_get_window (Denemo.printarea))
        gdk_window_set_cursor (gtk_widget_get_window (Denemo.printarea), gdk_cursor_new (GDK_X_CURSOR));

    }
 
}
void
edit_staff_properties (void)
{
    edit_staff_and_voice_properties (TRUE);
}
void
edit_voice_properties (void)
{
    edit_staff_and_voice_properties (FALSE);
}




void
set_modeaccs (gint * accs, gint number, gint mode)
{

  g_debug ("Mode %d : %d \n", number, mode);
  if (mode == 0)
    {
      switch (number)
        {
        case 11:
          number -= 7;
          break;
        }
      initkeyaccs (accs, number);

    }
  else if (mode == 2)
    initkeyaccs (accs, 0);
  else if (number == 1 && mode == 1)
    initkeyaccs (accs, 0);
}

/** 
 * This function initializes the accidental-context array associated with
 * a key signature or a staff to that appropriate for _number_ 
 */
void
initkeyaccs (gint * accs, gint number)
{
  int index;
  memset (accs, 0, SEVENGINTS);
  if (number > 0)
    for (index = 3; number; number--, index = (index + 4) % 7)
      accs[index] = 1;
  else if (number < 0)
    for (index = 6; number; number++, index = (index + 3) % 7)
      accs[index] = -1;

}

/**
 * Create a new measure break object
 * @return the measurebreak
 */
DenemoObject *
newmeasurebreakobject ()
{
  DenemoObject *ret;

  ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  ret->type = MEASUREBREAK;
  return ret;
}

/**
 * Create a new staff break object
 * @return the staffbreak
 */
DenemoObject *
newstaffbreakobject ()
{
  DenemoObject *ret;
  ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  ret->type = STAFFBREAK;
  return ret;
}

/* clone the directive, excluding the widget */
DenemoDirective *
clone_directive (DenemoDirective * directive)
{
  DenemoDirective *ret = (DenemoDirective *) g_malloc0 (sizeof (DenemoDirective));
  memcpy (ret, directive, sizeof (DenemoDirective));    //BEWARE all pointers in DenemoDirective require code, as follows:
#define CLONE(field) \
      if(directive->field && directive->field->len)\
        ret->field = g_string_new(directive->field->str);\
      else\
        ret->field = NULL;
  CLONE (tag);
  CLONE (prefix);
  CLONE (postfix);
  CLONE (display);
  CLONE (graphic_name);
  CLONE (grob);
  CLONE (midibytes);
  CLONE (data);
#undef CLONE
  if (directive->graphic)
    {
      ret->graphic = directive->graphic;        //alternatively could load it via loadGraphicItem, is the same
    }
  if (directive->widget)
    {
      //  gpointer fn = g_object_get_data(G_OBJECT(directive->widget), "fn");
      ret->widget = NULL;       //FIXME call widget_for_directive here???
      //   widget_for_directive(ret, fn);
    }
  return ret;
}


GList *
clone_directives (GList * directives)
{
  GList *ret = NULL;
  for (; directives; directives = directives->next)
    ret = g_list_append (ret, clone_directive (directives->data));
  return ret;
}

void
free_directive_data (DenemoDirective * directive)
{
#define DFREE(field) if(directive->field) g_string_free(directive->field, TRUE);
  DFREE (tag);
  DFREE (display);
  DFREE (prefix);
  DFREE (postfix);
  DFREE (graphic_name);
  DFREE (grob);
#undef DFREE

  if (directive->widget && !G_IS_OBJECT (directive->widget))
    {
      g_debug ("Found non-gobject widget %p\n", directive->widget);
    }
  if (directive->widget && G_IS_OBJECT (directive->widget))
    {
      //g_debug("We should destroy the widget now ");
      GtkWidget *texteditor = (GtkWidget *) g_object_get_data (G_OBJECT (directive->widget), DENEMO_TEXTEDITOR_TAG);
      if (texteditor)
        gtk_widget_destroy (texteditor);        //FIXME we may need to destroy its parents
      gtk_widget_destroy ((GtkWidget *) directive->widget);
    }



}

void
free_directive (DenemoDirective * directive)
{
  free_directive_data (directive);
  g_free (directive);
}

void
free_directives (GList * directives)
{
  for (; directives; directives = directives->next)
    {
      DenemoDirective *directive = directives->data;
      free_directive (directive);
    }
  g_list_free (directives);
}

/**
 * Create a clone of the given object
 * @param orig the object to clone
 * @return the cloned object
 */
DenemoObject *
dnm_clone_object (DenemoObject * orig)
{
  DenemoObject *ret = NULL;
  if (orig != NULL)
    {
      switch (orig->type)
        {
        case CHORD:
          ret = clone_chord (orig);
          break;

        case TUPOPEN:
          ret = (DenemoObject *) tuplet_open_new (((tupopen *) orig->object)->numerator, ((tupopen *) orig->object)->denominator);
          ((tupopen *) ret->object)->directives = clone_directives (((tupopen *) orig->object)->directives);

          break;
        case TUPCLOSE:
          ret = (DenemoObject *) tuplet_close_new ();
          ((tupopen *) ret->object)->directives = clone_directives (((tupopen *) orig->object)->directives);

          break;
        case CLEF:
          ret = clef_new (((clef *) orig->object)->type);
          ((clef *) ret->object)->directives = clone_directives (((clef *) orig->object)->directives);
          break;
        case TIMESIG:
          ret = dnm_newtimesigobj (((timesig *) orig->object)->time1, ((timesig *) orig->object)->time2);
          ((timesig *) ret->object)->directives = clone_directives (((timesig *) orig->object)->directives);

          break;
        case KEYSIG:
          ret = dnm_newkeyobj (((keysig *) orig->object)->number, ((keysig *) orig->object)->isminor, ((keysig *) orig->object)->mode);
          ((keysig *) ret->object)->directives = clone_directives (((keysig *) orig->object)->directives);

          break;
          break;
        case STEMDIRECTIVE:
          ret = dnm_stem_directive_new (((stemdirective *) orig->object)->type);

          ((stemdirective *) ret->object)->directives = clone_directives (((stemdirective *) orig->object)->directives);

          break;
        case MEASUREBREAK:
          ret = newmeasurebreakobject ();
          break;
        case STAFFBREAK:
          ret = newstaffbreakobject ();
          break;
        case LILYDIRECTIVE:
          {
            lilydirective *curlily = (lilydirective *) orig->object;
            ret = directive_object_new (clone_directive (curlily));
            ret->durinticks = orig->durinticks;
            ret->basic_durinticks = orig->basic_durinticks;

          }
          break;
        default:
          g_warning ("Unknown object type %x", orig->type);
          ret = lily_directive_new ("%unknown object\n");
          break;
        }
    }
    if(ret){
        if (orig->lilypond)
            ret->lilypond = g_strdup (orig->lilypond);
        else
            ret->lilypond = NULL;
        ret->midi_events = NULL;
        ret->isinvisible = orig->isinvisible;
  }
  return ret;
}

/**
 *  Create a new stem directive 
 *  @param type the stem directive type
 *  @return the stem directive
 */
DenemoObject *
dnm_stem_directive_new (enum stemdirections type)
{
  DenemoObject *ret;
  stemdirective *newstemdir = (stemdirective *) g_malloc (sizeof (stemdirective));
  ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  ret->type = STEMDIRECTIVE;
  ret->isinvisible = FALSE;
  newstemdir->type = type;
  ret->object = newstemdir;
  set_basic_numticks (ret);
  setpixelmin (ret);
  return ret;
}

/**
 * Create a new lilypond directive 
 *
 * @param type the lilypond directive body
 * @return the lilypond directive
 *
*/
DenemoObject *
lily_directive_new (gchar * type)
{
  DenemoObject *ret;
  lilydirective *newlily = (lilydirective *) g_malloc0 (sizeof (lilydirective));
  ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  ret->type = LILYDIRECTIVE;
  newlily->postfix = g_string_new (type);
  ret->object = newlily;
  set_basic_numticks (ret);
  setpixelmin (ret);
  return ret;
}

DenemoObject *
directive_object_new (DenemoDirective * directive)
{
  DenemoObject *ret;
  ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  ret->type = LILYDIRECTIVE;
  ret->object = directive;
  set_basic_numticks (ret);
  setpixelmin (ret);
  return ret;
}

/**
 * Create a new dynamic object
 * @param type the dynamic to create
 * @return the dynamic
  */
DenemoObject *
dynamic_new (gchar * type)
{
  DenemoObject *ret;
  dynamic *newdyn = (dynamic *) g_malloc0 (sizeof (dynamic));
  ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  ret->type = DYNAMIC;
  ret->isinvisible = FALSE;
  newdyn->type = g_string_new (type);
  ret->object = newdyn;
  set_basic_numticks (ret);
  setpixelmin (ret);
  return ret;
}

/**
 * Create a new lyric object
 * @param type the lyric to create
 * @param position whether it shoul be centered or not
 * @param syllable whether it is a syllable
 * @return the dynamic
  */
DenemoObject *
dnm_lyric_new (gchar * type, gint position, gboolean syllable)
{
  DenemoObject *ret;
  lyric *newlyric = (lyric *) g_malloc0 (sizeof (lyric));
  ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  ret->type = LYRIC;
  ret->isinvisible = FALSE;


  newlyric->lyrics = g_string_new (type);
  newlyric->position = position;
  newlyric->is_syllable = syllable;
  ret->object = newlyric;
  set_basic_numticks (ret);
  setpixelmin (ret);
  return ret;
}

/**
 *  Create a DenemoObject
 * @param type DenemoObject type 
 * @return the DenemoObject
 */

DenemoObject *
dnm_newobj (DenemoObjType type)
{
  DenemoObject *ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));;
  ret->type = type;
  set_basic_numticks (ret);
  setpixelmin (ret);            /* these do nothing at present - but if we introduce
                                   a show markers option then we will want to allot 
                                   some space
                                 */
  return ret;
}

/**
 * Set the key signature into the score
 * @param curstaff the current staff
 * @param tokey the key sig to insert
 * @param type  major/minor/mode
 */
void
dnm_setinitialkeysig (DenemoStaff * curstaff, gint tokey, gint type)
{
  take_snapshot ();
  curstaff->keysig.number = tokey;
  curstaff->keysig.isminor = type;

  initkeyaccs (curstaff->keysig.accs, tokey);
  //memcpy (curstaff->keysig.keyaccs, curstaff->leftmost_keyaccs, SEVENGINTS);
  curstaff->leftmost_keysig = &curstaff->keysig;
  staff_show_which_accidentals (curstaff);
  adjust_tonal_center (curstaff->keysig.accs);
  displayhelper (Denemo.project);
  score_status(Denemo.project, TRUE);
}
