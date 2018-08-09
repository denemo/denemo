/* object.c
 * functions that do operations to DenemoObjects
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */

#include <denemo/denemo.h>
#include "command/chord.h"
#include "core/utils.h"
#include "core/kbd-custom.h"
#include "core/menusystem.h"
#include "command/commandfuncs.h"
#include "command/object.h"
#include "command/staff.h"
#include "command/tuplet.h"
#include "command/select.h"
#include "audio/pitchentry.h"
#include "core/utils.h"
#include "core/view.h"
#include "command/lilydirectives.h"
#include "command/scorelayout.h"
#include <string.h>

typedef enum DIRECTIVE_TYPE
{ DIRECTIVE_OBJECT = 0, DIRECTIVE_SCORE = 1, DIRECTIVE_MOVEMENT = 2, DIRECTIVE_STAFF = 3, DIRECTIVE_VOICE = 4, DIRECTIVE_KEYSIG = 5, DIRECTIVE_TIMESIG = 6, DIRECTIVE_CLEF = 7} DIRECTIVE_TYPE;

static GList *OldCurrentObject; //current object when object editor was left
static GtkWidget *TheEditorWidget = NULL;
static GtkWidget *ObjectInfo = NULL;


static void edit_staff_and_voice_properties (gboolean show_staff);
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
      if (directive->tag == NULL)
        directive->tag = g_string_new ("<Unknown Tag>");        //shouldn't happen
      const gchar *label = get_label_for_command (directive->tag->str);
      const gchar *menupath = get_menu_path_for_command (directive->tag->str);
      const gchar *tooltip = get_tooltip_for_command (directive->tag->str);
      if (tooltip == NULL)
        tooltip = _("No tooltip");

      if (directive->tag == NULL)
        directive->tag = g_string_new ("<Unknown Tag>");        //shouldn't happen
      gchar *label_e = label ? g_markup_escape_text (label, -1) : g_markup_escape_text (directive->tag->str, -1);
      if (!first)
        g_string_append (selection, "\n<span foreground=\"blue\"weight=\"bold\">---------------------------------------------------------</span>\n");
      else
        g_string_append (selection, "\n<span foreground=\"green\"weight=\"bold\">---------------------------------------------------------</span>\n");
      first = FALSE;
      if (label)
        g_string_append_printf (selection, _("Directive for command: <span weight=\"bold\">\"%s\"</span>\n"), label_e);
      else
        g_string_append_printf (selection, _("Directive tagged: <span foreground=\"red\"weight=\"bold\">\"%s\"</span>\n"), label_e);
      g_free (label_e);
      if (menupath)
        {
          gchar *menupath_e = g_markup_escape_text (menupath, -1);
          g_string_append_printf (selection, _("Menu location for this command: <span style=\"italic\" weight=\"bold\">\"%s\"</span>\n"), menupath_e);
          g_free (menupath_e);
        }
      if (tooltip)
        {
          gchar *tooltip_e = g_markup_escape_text (tooltip, -1);
          g_string_append_printf (selection, _("The help for the command that created this directive is:\n<big>\"%s\"</big>\n"), tooltip_e);
          g_free (tooltip_e);
        }
      if (directive->prefix)
        {
          gchar *lily = g_markup_escape_text (directive->prefix->str, -1);
          g_string_append_printf (selection, _("LilyPond inserted in prefix to this object is <tt>\"%s\"</tt>\n"), lily);
          g_free (lily);
        }
      if (directive->postfix)
        {
          gchar *lily = g_markup_escape_text (directive->postfix->str, -1);
          g_string_append_printf (selection, _("LilyPond inserted in postfix to this object is <tt>\"%s\"</tt>\n"), lily);
          g_free (lily);
        }
      if (directive->layouts && (directive->flag == DENEMO_IGNORE_FOR_LAYOUTS))
        g_string_append (selection, _("<span foreground=\"red\"weight=\"bold\">THIS DIRECTIVE IS IGNORED FOR SOME LAYOUTS\n</span>"));
      else if (directive->layouts && (directive->flag == DENEMO_ALLOW_FOR_LAYOUTS))
        g_string_append (selection, _("<span foreground=\"red\"weight=\"bold\">THIS DIRECTIVE ONLY FOR SOME LAYOUTS\n</span>"));
      if (!directives->next)
        g_string_append (selection, "<span foreground=\"blue\"weight=\"bold\">---------------------------------------------------------</span>");

    }
  while (directives->next && (directives = directives->next));
  g_string_append (selection, "\n");
}

static void
append_lilypond (DenemoObject * curObj, GString * selection)
{
  DenemoProject *gui = Denemo.project;
  if (gui->lilysync != gui->changecount)
    refresh_lily_cb (NULL, gui);        //if(curObj->lilypond)g_print ("lili |%s|\n", curObj->lilypond);
  if (curObj->lilypond && *curObj->lilypond)
    g_string_append_printf (selection, _("The LilyPond syntax generated is: <tt>\"%s\"</tt>\n"), g_markup_escape_text (curObj->lilypond, -1));
  else
    g_string_append_printf (selection, _("This object does not affect the music typesetting, (no LilyPond syntax is generated)\n"));
}

static gint
gcd384 (gint n)
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

static void
reset_cursors (void)
{
  gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), Denemo.GDK_LEFT_PTR);
  if (ObjectInfo)
    gdk_window_set_cursor (gtk_widget_get_window (ObjectInfo), Denemo.GDK_LEFT_PTR);
  if (Denemo.printarea && gtk_widget_get_window (Denemo.printarea))
    gdk_window_set_cursor (gtk_widget_get_window (Denemo.printarea), Denemo.GDK_LEFT_PTR);
}

static void
move_to_next_note (GtkWidget * editwin)
{
  if (!cursor_to_next_note_height ())
    cursor_to_nth_note_height (0);
  if (editwin)
    {
      gtk_widget_destroy (editwin);
      reset_cursors ();
      edit_object ();
    }
}

static void
go_left (GtkWidget * editwin)
{
  cursor_to_prev_object (FALSE, FALSE);
  if (Denemo.project->movement->currentobject == NULL)
    {
      cursor_to_next_object (FALSE, FALSE);
      warningdialog (_("Preceding measures are empty"));
    }
  if (editwin)
    {
      gtk_widget_destroy (editwin);
      reset_cursors ();
      edit_object ();
    }
}

static void
go_right (GtkWidget * editwin)
{
  cursor_to_next_object (FALSE, FALSE);
  if (Denemo.project->movement->currentobject == NULL)
    {
      cursor_to_prev_object (FALSE, FALSE);
      warningdialog (_("Subsequent measures are empty"));
    }
  if (editwin)
    {
      gtk_widget_destroy (editwin);
      reset_cursors ();
      edit_object ();
    }
}

//swap element node with following element
static void swap_notes (GList *node)
{
    gpointer temp;
    temp = node->data;
    node->data = node->next->data;
    node->next->data = temp;
   
}


static void swap_notes_for_inspector (GList *node)
{
   
    swap_notes (node);
    update_object_info ();
}

static void swap_notes_for_edit (GList *node)
{
    gtk_widget_destroy (TheEditorWidget);
    TheEditorWidget = NULL;
    swap_notes (node);
    edit_object ();
}

static void
create_palette_button_for_clone (void)
{
  call_out_to_guile ("(d-CreateButtonForObject)");  
    
}

static gint display_timeout_id = 0;     //timeout to avoid calling display_current_object() repeatedly during rapid changes/entry of music
static gboolean
display_current_object_callback (void)
{
  DenemoProject *gui = Denemo.project;
  if (Denemo.project->movement == NULL)
    return FALSE;
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

      if (curObj->type == CHORD)
        {
          chord *thechord = ((chord *) curObj->object);
          if (thechord->notes)
            {
              note *thenote = findnote (curObj, Denemo.project->movement->cursor_y);
              GtkWidget *button = gtk_button_new_with_label (thechord->notes->next ? _("Inspect next note in chord") : _("Inspect the note"));
              gtk_widget_set_sensitive (button, ((!thenote) || thechord->notes->next || (Denemo.project->movement->cursor_y != thenote->mid_c_offset)));

              g_signal_connect_swapped (button, "clicked", G_CALLBACK (move_to_next_note), NULL);
              gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, TRUE, 0);
              
             
              GList * notenode = g_list_find (thechord->notes, thenote);
              if (notenode->next)
                {
                   note *nextnote = notenode->next->data;
                   if (thenote->mid_c_offset == nextnote->mid_c_offset)
                        { 
                          GtkWidget *button = gtk_button_new_with_label ( _("Alternate note at cursor"));
                          g_signal_connect_swapped (button, "clicked", G_CALLBACK (swap_notes_for_inspector), notenode);
                          gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, TRUE, 0);
                            
                        }
                }
              
            }
        }

      GtkWidget *edit_button = gtk_button_new_with_label (_("Run the Object Editor"));
      g_signal_connect (edit_button, "clicked", G_CALLBACK (edit_object), NULL);
      gtk_box_pack_start (GTK_BOX (vbox), edit_button, FALSE, TRUE, 0);
      
      
     if ((curObj->type == CHORD) || (curObj->type == LILYDIRECTIVE) || (curObj->type == CLEF)  || (curObj->type == KEYSIG)   || (curObj->type == TIMESIG)    || (curObj->type == TUPOPEN) || (curObj->type == TUPCLOSE))
        {      
          GtkWidget *clone_button = gtk_button_new_with_label (_("Create Button for Clone"));
          g_signal_connect (clone_button, "clicked", G_CALLBACK (create_palette_button_for_clone), NULL);
          gtk_box_pack_start (GTK_BOX (vbox), clone_button, FALSE, TRUE, 0);
        }

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
                  g_string_append_printf (selection, _("A Chord Symbol \"%s\" is attached to this note.\n"), ((GString *) thechord->fakechord)->str);

                if (thechord->figure)
                  g_string_append_printf (selection, _("A Bass Figure \"%s\" is attached to this note.\n"), ((GString *) thechord->figure)->str);



                note *thenote = findnote (curObj, gui->movement->cursor_y);
                if (thenote && gui->movement->cursor_y == thenote->mid_c_offset)
                  {

                    g_string_append_printf (selection, _("<b>Within the chord the cursor is on the note %s </b>\n"), pretty_name (mid_c_offsettolily (thenote->mid_c_offset, thenote->enshift)));
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
                  g_string_append_printf (selection, _("This %s starts %d 𝅘𝅥  's into the measure and lasts %d 𝅘𝅥 's.\n"), type, curObj->starttick / 384, curObj->durinticks / 384);
                else
                  g_string_append_printf (selection, _("This %s starts %d 𝅘𝅥  's into the measure and lasts %d/%d 𝅘𝅥 's.\n"), type, curObj->starttick / 384, curObj->durinticks / gcd_d, 384 / gcd_d);
              }
            else
              {
                if (gcd_d == 384)
                  g_string_append_printf (selection, _("This %s starts %d/%d 𝅘𝅥  's into the measure and lasts %d 𝅘𝅥 's.\n"), type, curObj->starttick / gcd_s, 384 / gcd_s, curObj->durinticks / 384);
                else
                  g_string_append_printf (selection, _("This %s starts %d/%d 𝅘𝅥  's into the measure and lasts %d/%d 𝅘𝅥 's.\n"), type, curObj->starttick / gcd_s, 384 / gcd_s, curObj->durinticks / gcd_d, 384 / gcd_d);
              }
            append_lilypond (curObj, selection);
          }
          break;
        case TUPOPEN:
          {
            tuplet *thetup = ((tuplet *) curObj->object);
            //type = _("start tuplet marker");
            g_string_append_printf (selection, _(" a Start Tuplet object\n" "Meaning %d notes will take the time of %d notes\n" "until an End Tuplet object.\nSee the Notes/Rests → Tuplets for control over how tuplets print\n"), thetup->denominator, thetup->numerator);
            if (thetup->directives)
              {
                selection = g_string_append (selection, _("Attached to the Start Tuplet:"));
                append_directives_information (selection, thetup->directives);
              }
            append_lilypond (curObj, selection);
          }
          break;
        case TUPCLOSE:
          {
            tuplet *thetup = ((tuplet *) curObj->object);
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
            if (curObj->starttick)
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

            if (directive->tag == NULL)
              directive->tag = g_string_new ("<Unknown Tag>");  //shouldn't happen
            const gchar *label = get_label_for_command (directive->tag->str);
            const gchar *menupath = get_menu_path_for_command (directive->tag->str);
            const gchar *tooltip = get_tooltip_for_command (directive->tag->str);
            if (tooltip == NULL)
              tooltip = _("No tooltip");
            gchar *label_e = label ? g_markup_escape_text (label, -1) : g_markup_escape_text (directive->tag->str, -1);
            if (label)
              g_string_append_printf (selection, _("a Denemo Directive: <span weight=\"bold\">%s</span>\n"), label_e);
            else
              g_string_append_printf (selection, _("a Denemo Directive tagged: <span foreground=\"red\"weight=\"bold\">%s</span>\n"), label_e);
            g_free (label_e);
            if (tooltip)
              {
                gchar *tooltip_e = g_markup_escape_text (tooltip, -1);
                g_string_append_printf (selection, _("\nThe help for the command that created this directive is\n<big>\"%s\"</big>"), tooltip_e);
                g_free (tooltip_e);
              }

            g_string_append_printf (selection, _("%s"), (directive->layouts && (directive->flag == DENEMO_IGNORE_FOR_LAYOUTS)) ? _("\nNot for some layouts\n") : (directive->layouts && (directive->flag == DENEMO_ALLOW_FOR_LAYOUTS)) ? _("\nOnly for some Layout(s)\n") : "\n");
            if (menupath)
              {
                gchar *menupath_e = g_markup_escape_text (menupath, -1);
                g_string_append_printf (selection, _("Menu location for this command: <span style=\"italic\" weight=\"bold\">\"%s\"</span>\n"), menupath_e);
                g_free (menupath_e);
              }

            {
              gchar *text = g_strconcat (directive->prefix ? directive->prefix->str : "",
                                         directive->postfix ? directive->postfix->str : "", NULL);
              g_strchug (text); //does not allocate memory
              if (*text)
                {
                  gchar *lily1 = directive->prefix ? g_markup_escape_text (directive->prefix->str, -1) : g_strdup ("");
                  gchar *lily2 = directive->postfix ? g_markup_escape_text (directive->postfix->str, -1) : g_strdup ("");

                  g_string_append_printf (selection, _("The LilyPond text inserted is <tt>%s%s</tt>\n"), lily1, lily2); //puts the whitespace back
                  g_free (lily1);
                  g_free (lily2);
                }
              else
                g_string_append_printf (selection, _("This object does not affect the printed output (no LilyPond syntax is generated for the typesetter)\n")); //well this ignores possible effect of whitespace...
              g_free (text);
              if (directive->layouts)
                g_string_append (selection, _("<span foreground=\"red\"weight=\"bold\">THIS DIRECTIVE IS CONDITIONAL ON THE LAYOUT\n</span>"));
            }
            if (gui->movement->currentobject->next == NULL && (gui->movement->currentmeasure->next == NULL))
              g_string_assign (warning, _("This Directive is at the end of the music" "\nYou may need a closing double bar line -\n" "see Directives → Markings → Inserting Barlines"));
          }
          break;
        default:
          {
            g_string_append (selection, _("The cursor is on an unknown object type. Please report how this happened!"));
          }
          break;
        }                       //end switch curObj type

      if (gui->movement->smfsync == gui->movement->changecount)
        {

          gdouble time = curObj->earliest_time;
          gint minutes = time / 60.0;
          gdouble seconds = time - 60 * minutes;
          g_string_append_printf (selection, _("Playback timing: %d minutes %1.2f seconds"), minutes, seconds);
        }

      if (warning->len)
        {
          GtkWidget *label = gtk_label_new ("");
          warning = g_string_prepend (warning, _("<span font-desc=\"30\">Warning</span> "));
          gtk_label_set_markup (GTK_LABEL (label), warning->str);
          gtk_label_set_line_wrap (GTK_LABEL (label), TRUE);
          gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, TRUE, 0);
        }
      if (selection->len)
        {
          GtkWidget *scrolled_window = gtk_scrolled_window_new (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0), gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
#if GTK_MAJOR_VERSION==3
          gtk_widget_set_vexpand (scrolled_window, TRUE);
#endif
          gtk_box_pack_start (GTK_BOX (vbox), scrolled_window, TRUE, TRUE, 0);
          GtkWidget *inner_box = gtk_vbox_new (FALSE, 0);
          
#if (GTK_MAJOR_VERSION==3 && GTK_MINOR_VERSION<8)       
          gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (scrolled_window), inner_box);
#else          
          gtk_container_add (GTK_CONTAINER(scrolled_window), inner_box);
#endif          
          GtkWidget *label = gtk_label_new ("");
          gtk_label_set_selectable (GTK_LABEL (label), TRUE);

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
          gtk_label_set_line_wrap (GTK_LABEL (label), TRUE);
          gtk_box_pack_start (GTK_BOX (inner_box), label, TRUE, TRUE, 0);
        }
      g_string_free (warning, TRUE);
      g_string_free (selection, TRUE);
    }
  gtk_widget_show_all (ObjectInfo);
#ifdef G_OS_WIN32
//on windows, the ObjectInfo window takes the focus regardless of having told it not to, so it is up to the user to bring the inspector to the front.
  gtk_window_set_transient_for (GTK_WINDOW (ObjectInfo), GTK_WINDOW (Denemo.window));
  gtk_window_set_keep_above (GTK_WINDOW (ObjectInfo), TRUE);

#else
  gtk_window_present (GTK_WINDOW (ObjectInfo));
#endif
  display_timeout_id = 0;
  return FALSE;
}

void
display_current_object (void)
{
  static gint delay = 10;
  if (display_timeout_id == 0)
    {
      display_timeout_id = g_timeout_add (delay, (GSourceFunc) display_current_object_callback, NULL);
      delay = 10;
    }
  else
    {

      if (delay < 1500)
        delay += 100;
    }
}

static void
show_window (GtkWidget * w)
{
  w = gtk_widget_get_toplevel (w);
  gtk_widget_show (w);
  gtk_window_present (GTK_WINDOW (w));
  //g_print ("Window presented");
}

static void
set_false (GtkWidget * button, gboolean * bool)
{
  GtkWidget *w = gtk_widget_get_toplevel (button);
  gtk_widget_destroy (button);
  show_window (w);
  if (*bool)
    score_status (Denemo.project, TRUE);
  *bool = FALSE;
}
static void
chuck_object_editor (void)
{
  if (TheEditorWidget)
    gtk_widget_destroy (TheEditorWidget);
  else
    g_warning ("Call to chuck editor window, but it is not set\n");
  TheEditorWidget = NULL;
  OldCurrentObject = Denemo.project->movement->currentobject;
}

static gboolean
recover_object_editor (void)
{
  if (OldCurrentObject == Denemo.project->movement->currentobject)
    {
      edit_object ();
      return TRUE;
    }
  else
    reset_cursors ();
  return FALSE;
}
typedef gboolean fn_type (gchar *);
static void
advanced_edit_type_directive (GtkWidget * button, gpointer fn)
{
  DenemoDirective *directive = (DenemoDirective *) g_object_get_data (G_OBJECT (button), "directive");
  GList **directives = (GList **) g_object_get_data (G_OBJECT (button), "directives");
  chuck_object_editor();
  if (!(((fn_type *) fn) (directive->tag->str)))
    {
      if (directives)
        *directives = g_list_remove (*directives, directive);
      else
        dnm_deleteobject (Denemo.project->movement);
      gtk_widget_destroy (gtk_widget_get_parent (gtk_widget_get_parent (button)));
      score_status (Denemo.project, TRUE);
    }
  else
     recover_object_editor ();

}

static void
create_palette_button_for_directive (GtkWidget * button, gchar * what)
{
  DenemoDirective *directive = (DenemoDirective *) g_object_get_data (G_OBJECT (button), "directive");
  DenemoPalette *pal = NULL;
  if (!strcmp (what, "lilycontrol"))
    what = "score";
  GString *script = g_string_new (get_script_for_directive (directive, what));
  gchar *name = choose_palette_by_name (TRUE, FALSE);
  DenemoObject *curObj = get_object ();
  if (curObj && (curObj->type == CHORD)
        && (!strcmp (what, "note")))
    {
      chord *thechord = (chord *) curObj->object;
      if (thechord->chordize)
        g_string_append (script, "(d-Chordize #t)\n");
    }

  if (name)
    pal = create_palette (name, FALSE, TRUE);
  if (pal)
    {
      gchar *button_name = g_strdup_printf (_("Clone %s"), directive->tag->str);
      gchar *label = string_dialog_entry (Denemo.project, _("Palette Button Creation"), _("Give a (unique) name for the button"), button_name);
      if (label)
        {
          if (!palette_add_button (pal, label, _("Creates a cloned Denemo Directive"), script->str))
            warningdialog (_("Could not create a button of that name in that palette"));
        }
      else
        warningdialog (_("Cancelled"));
      g_free (label);
      g_free (button_name);
      gtk_widget_show_all (gtk_widget_get_parent (pal->box));
      gtk_widget_destroy (gtk_widget_get_toplevel (button));
      reset_cursors ();
    }
  g_string_free (script, TRUE);
}
static void dummy_rerun (void)
{
    g_warning ("No action");
}
static gpointer
get_rerun (gchar * field)
{
  gpointer rerun = dummy_rerun;
  if (!strcmp (field, "movementcontrol"))
    rerun = edit_movement_properties;
  else if (!strcmp (field, "scoreheader"))
    rerun = edit_score_properties;
  else if (!strcmp (field, "lilycontrol"))
    rerun = edit_score_properties;
  else if (!strcmp (field, "header"))
    rerun = edit_score_properties;
  else if (!strcmp (field, "layout"))
    rerun = edit_score_properties;
  else if (!strcmp (field, "paper"))
    rerun = edit_score_properties;
  else if (!strcmp (field, "staff"))
    rerun = edit_staff_properties;
  else if (!strcmp (field, "clef"))
    rerun = edit_staff_properties;
  else if (!strcmp (field, "timesig"))
    rerun = edit_staff_properties;
  else if (!strcmp (field, "keysig"))
    rerun = edit_staff_properties;
  else if (!strcmp (field, "voice"))
    rerun = edit_voice_properties;
  else
    g_warning ("The field %s should have a Advanced button but does not.\n\n\n", field);
  return rerun;
}

static void
create_duplicate_directive (GtkWidget * button, gchar * what)
{
  DenemoDirective *directive = (DenemoDirective *) g_object_get_data (G_OBJECT (button), "directive");
  DenemoPalette *pal = NULL;
  GList **directives = (GList **) g_object_get_data (G_OBJECT (button), "directives");
  gpointer rerun = get_rerun (what);
  DenemoScoreblock *sb = (DenemoScoreblock *) selected_scoreblock ();
  if (directive && directives)
    {
      gchar *tag_suffix = string_dialog_entry (Denemo.project, _("Duplicate Directive"), _("Give layout duplicate directive is for: "), (sb && sb->name) ? sb->name : _("Score"));
      if (tag_suffix)
        {
          DenemoDirective *newdirective = clone_directive (directive);
          newdirective->tag = g_string_new (g_strdup_printf ("%s\n%s", directive->tag->str, tag_suffix));
          *directives = g_list_append (*directives, newdirective);      //g_print ("override was %x", newdirective->override);
          newdirective->override &= ~DENEMO_OVERRIDE_GRAPHIC;   //g_print ("override becomes %x", newdirective->override);

          newdirective->flag = DENEMO_ALLOW_FOR_LAYOUTS;
          newdirective->layouts = g_list_append (NULL, GUINT_TO_POINTER (get_layout_id_for_name (tag_suffix)));
          gchar *info = g_strdup_printf ("%s: %c%s%c %s", _("The duplicate will be typeset only for layout: "), '\"', tag_suffix, '\"',
                                         _("The original directive should be made to ignore that layout even though later directives generally override earlier ones. The duplicate directive will appear at the end of the directives of its type in the editor, tagged with the name of the layout (you can change the conditional behavior regardless of this name though). "));
          warningdialog (info);
          g_free (info);
          gtk_widget_destroy (gtk_widget_get_toplevel (button));
          signal_structural_change (Denemo.project);
          score_status (Denemo.project, TRUE);
          if (rerun)
            G_CALLBACK (rerun) ();
        }
      else
        warningdialog (_("Cancelled"));
    }
}

static void
create_palette_button_for_command (GtkWidget * button, gchar * tooltip)
{
  DenemoDirective *directive = (DenemoDirective *) g_object_get_data (G_OBJECT (button), "directive");
  DenemoPalette *pal = NULL;
  gchar *script = g_strdup_printf ("(d-%s)", directive->tag->str);
  gchar *name = choose_palette_by_name (TRUE, FALSE);
  if (name)
    pal = create_palette (name, FALSE, TRUE);
  if (pal)
    {
      gboolean success = palette_add_button (pal, directive->tag->str, tooltip, script);
      gtk_widget_show_all (gtk_widget_get_parent (pal->box));
      gtk_widget_destroy (gtk_widget_get_toplevel (button));
      reset_cursors ();
    }
  g_free (script);
}

static void
delete_directive (GtkWidget * button, gpointer fn)
{
  DenemoDirective *directive = (DenemoDirective *) g_object_get_data (G_OBJECT (button), "directive");
  GList **directives = (GList **) g_object_get_data (G_OBJECT (button), "directives");
  if (directives)
    {
      *directives = g_list_remove (*directives, directive);
      free_directive (directive);
    }
  else
    dnm_deleteobject (Denemo.project->movement);
  gtk_widget_destroy (gtk_widget_get_toplevel (button));
  score_status (Denemo.project, TRUE);
}




static void
call_edit_on_action (GtkWidget * button, DIRECTIVE_TYPE score_edit)
{
  //gtk_widget_destroy (gtk_widget_get_toplevel (button));
  DenemoScriptParam param;
  DenemoAction *action = (DenemoAction *) g_object_get_data (G_OBJECT (button), "action");
  //GList *currentobject = Denemo.project->movement->currentobject;
  chuck_object_editor ();
  param.string = g_string_new ("edit");
  g_debug ("Script can look for params \"edit\" - a string to catch this");
  activate_script (action, &param);
  g_string_free (param.string, TRUE);
//   gtk_widget_destroy (gtk_widget_get_toplevel (button));
  if (score_edit)
    {
      if (score_edit == DIRECTIVE_SCORE)
        edit_score_properties ();
      else if (score_edit == DIRECTIVE_MOVEMENT)
        edit_movement_properties ();
      else if (score_edit == DIRECTIVE_VOICE)
        edit_voice_properties ();
      else
        edit_staff_properties ();       //also for KEYSIG AND TIMESIG

    }
  else
    {
      if (score_edit || (!recover_object_editor ()))
        reset_cursors ();
    }
}

static void
call_score_properties_dialog (GtkWidget * button)
{
  gtk_widget_hide (gtk_widget_get_toplevel (button));
  score_properties_dialog (NULL, NULL);
  show_window (button);         //gtk_widget_show (gtk_widget_get_toplevel (button));
}

static void
execute_editscript (GtkWidget * button, gchar * filename)
{
  gchar *keep = g_strdup (filename);
  gtk_widget_destroy (gtk_widget_get_toplevel (button));
  GError *error = (GError *) execute_script_file (keep);
  g_free (keep);
  GList *currentobject = Denemo.project->movement->currentobject;

  if (error)
    g_warning ("%s", error->message);

  if (currentobject == Denemo.project->movement->currentobject)
    edit_object ();
  else
    reset_cursors ();
}

/* linked to type_str[] array!!! */
typedef enum
{
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

static gchar *type_str[] = {
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

static void
general_edit_popup (GtkWidget * button, EditObjectType type)
{

#ifdef G_OS_WIN32
//the popup does not work on windows...
#else
  if (type == EDIT_CHORD)
    popup_menu ("NotesRests");
  else
#endif
    infodialog (_("To add or remove built-in attributes right click on the object in the display window"));

  gtk_widget_destroy (gtk_widget_get_parent (gtk_widget_get_parent (button)));
  reset_cursors ();

}

static void
display_help (gchar * help)
{
  infowarningdialog (help, TRUE);
}


static void
seek_directive (GtkWidget * button, gchar * type, gchar * tag)
{
  gtk_widget_destroy (gtk_widget_get_toplevel (button));
  GList *currentobject = Denemo.project->movement->currentobject;
  gchar *script = type ? g_strconcat ("(define-once EditSimilar::last #f)\n(set! EditSimilar::last (cons ",
                                      type, " \"",
                                      tag, "\"))\n(d-ResumeEdit)",
                                      NULL) : g_strdup ("(d-EditSimilar)");
  call_out_to_guile (script);
  g_free (script);

  if (currentobject == Denemo.project->movement->currentobject)
    edit_object ();
  else
    reset_cursors ();
}

static void
seek_note_directive (GtkWidget * button, gchar * tag)
{
  seek_directive (button, "'note", tag);
}

static void
seek_chord_directive (GtkWidget * button, gchar * tag)
{
  seek_directive (button, "'chord", tag);
}

static void
seek_keysig_directive (GtkWidget * button, gchar * tag)
{
  seek_directive (button, "'keysigdir", tag);
}

static void
seek_timesig_directive (GtkWidget * button, gchar * tag)
{
  seek_directive (button, "'timesigdir", tag);
}

static void
seek_stemdirective_directive (GtkWidget * button, gchar * tag)
{
  seek_directive (button, "'stemdir", tag);
}

static void
seek_clef_directive (GtkWidget * button, gchar * tag)
{
  seek_directive (button, "'clefdir", tag);
}

static void
seek_standalone_directive (GtkWidget * button, gchar * tag)
{
  seek_directive (button, NULL, tag);
}

static void
make_type_directive_conditional (gchar *type, gchar * tag)
{
  gchar *script = g_strdup_printf ("(SetDirectiveConditional #f (cons \"%s\" \"%s\"))", type, tag);
  chuck_object_editor ();
  call_out_to_guile (script);
  g_free (script);
  recover_object_editor ();
}
static void
make_clef_directive_conditional (gchar * tag)
{
  make_type_directive_conditional ("clef", tag);
}
static void
make_keysig_directive_conditional (gchar * tag)
{
  make_type_directive_conditional ("keysig", tag);
}
static void
make_timesig_directive_conditional (gchar * tag)
{
  make_type_directive_conditional ("timesig", tag);
}

static void
make_chord_directive_conditional (gchar * tag)
{
  gchar *script = g_strdup_printf ("(d-ChooseCondition (cons \"%s\" #f))", tag);
  chuck_object_editor ();
  call_out_to_guile (script);
  g_free (script);
  recover_object_editor ();
}

static void
make_note_directive_conditional (gchar * tag)
{
  gchar *script = g_strdup_printf ("(d-ChooseCondition (cons \"%s\" #t))", tag);
  chuck_object_editor ();
  call_out_to_guile (script);
  g_free (script);
  recover_object_editor ();
}

static void
make_directive_conditional (GtkWidget * button, DenemoDirective * directive)
{
  static gboolean notwarned = TRUE;
  if (Denemo.project->custom_scoreblocks)
    {
      warningdialog (_("You have custom score layout(s). Making this directive conditional will not affect them until you Reload Score-wide Settings in the Score Layout view."));
      notwarned = FALSE;
    }
  gtk_widget_destroy (TheEditorWidget);
  gpointer rerun;
  const gchar *field;
  rerun = g_object_get_data (G_OBJECT (button), "rerun");
  if (!rerun) rerun = dummy_rerun;
  field = (const gchar *) g_object_get_data (G_OBJECT (button), "field");
  if (directive && directive->tag)
    {
      gchar *script = g_strdup_printf ("(SetDirectiveConditional  #f (cons \"%s\" \"%s\"))", field, directive->tag->str);       // g_print ("Calling %s\n\n", script);
      signal_structural_change (Denemo.project);        //changing the conditional behavior of non-object directives requires score layouts to be reconstructed
      call_out_to_guile (script);
      g_free (script);
      score_status (Denemo.project, TRUE);
    }
  G_CALLBACK (rerun) ();
}


static void
install_conditional_button (GtkWidget * hbox, DenemoDirective * directive, gchar * field)
{
  GdkRGBA color;
  GtkWidget *button = gtk_button_new_with_label (_("Conditional"));
#if GTK_MAJOR_VERSION == 2  
  get_color (&color, 0.0, 0.0, 0.5, 1.0);
  gtk_widget_override_color (button, GTK_STATE_FLAG_NORMAL, &color);
#else
    set_foreground_color (button, "rgb(0, 0, 128)");//dark blue
#endif
  gpointer rerun = NULL;
  if (!strcmp (field, "movementcontrol"))
    rerun = edit_movement_properties;
  else if (!strcmp (field, "scoreheader"))
    rerun = edit_score_properties;
  else if (!strcmp (field, "lilycontrol"))
    rerun = edit_score_properties;
  else if (!strcmp (field, "header"))
    rerun = edit_score_properties;
  else if (!strcmp (field, "layout"))
    rerun = edit_score_properties;
  else if (!strcmp (field, "paper"))
    rerun = edit_score_properties;
  else if (!strcmp (field, "staff"))
    rerun = edit_staff_properties;
  else if (!strcmp (field, "clef"))
    rerun = edit_staff_properties;
  else if (!strcmp (field, "timesig"))
    rerun = edit_staff_properties;
  else if (!strcmp (field, "keysig"))
    rerun = edit_staff_properties;
  else if (!strcmp (field, "voice"))
    rerun = edit_voice_properties;
  else
    g_warning ("The field %s should have a conditional button but doesnt.\n\n\n", field);

  g_object_set_data (G_OBJECT (button), "rerun", rerun);
  g_object_set_data (G_OBJECT (button), "field", (!strcmp (field, "lilycontrol")) ? "score" : field);
  if (rerun)
    {
      g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (make_directive_conditional), (gpointer) directive);
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
    }
}

static void
copy_chord_directive_to_clipboard (gchar * tag)
{
  gchar *script = g_strdup_printf ("(CreateScriptForDirective (cons \"%s\" #f))", tag);
  call_out_to_guile (script);
  g_free (script);
}

static void
copy_note_directive_to_clipboard (gchar * tag)
{
  gchar *script = g_strdup_printf ("(CreateScriptForDirective (cons \"%s\" 'note))", tag);
  call_out_to_guile (script);
  g_free (script);
}

static void
place_directives (GtkWidget * vbox, GList ** pdirectives, EditObjectType type)
{
  GList *directives = *pdirectives;
  for (; directives; directives = directives->next)
    {
      DenemoDirective *directive = directives->data;
      const gchar *label = get_label_for_command (directive->tag->str);
      DenemoAction *action = lookup_action_from_name (directive->tag->str);
      gchar *name = label ? (gchar *) label : directive->tag->str;
      const gchar *tooltip = get_tooltip_for_command (directive->tag->str);
      gchar *display = directive->display ? directive->display->str : "";
      gchar *filename = get_editscript_filename (directive->tag->str);

      if (!label)
        label = directive->tag->str;

      gchar *label_text = g_strdup_printf ("%s %c%s%c", label, '[', display, ']');
      GtkWidget *expander = gtk_expander_new (label_text);
      g_free (label_text);


      gtk_expander_set_expanded (GTK_EXPANDER (expander), TRUE);
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
          g_free (thelabel);
        }
      else if (action)
        {
          gchar *thelabel = g_strconcat (_("Execute command: "), name, NULL);
          GtkWidget *button = gtk_button_new_with_label (thelabel);
          gtk_widget_set_tooltip_text (button, _("Re-run the command to edit the Denemo Directive"));
          g_object_set_data (G_OBJECT (button), "action", (gpointer) action);
          g_signal_connect (button, "clicked", G_CALLBACK (call_edit_on_action), NULL);
          gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);
          g_free (thelabel);
        }


      fn_type *func;
      switch (type)
        {
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
      //GtkWidget *labelwidget = (GtkWidget *) gtk_bin_get_child (GTK_BIN (button));

      //GdkRGBA color;
      //get_color (&color, 1.0, 0.0, 0.0, 1.0);
      //gtk_widget_override_color (labelwidget, GTK_STATE_FLAG_NORMAL, &color);
      set_foreground_color (button, "rgb(255,0,0)");
      
      g_object_set_data (G_OBJECT (button), "directives", (gpointer) pdirectives);
      g_object_set_data (G_OBJECT (button), "directive", (gpointer) directive);
      g_signal_connect (button, "clicked", G_CALLBACK (delete_directive), func);
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 30);


      {
        fn_type *func;
        switch (type)
          {
          case EDIT_NOTE:
            func = (fn_type *) seek_note_directive;     //seek_directive (note, tag);
            break;
          case EDIT_CHORD:
            func = (fn_type *) seek_chord_directive;
            break;

          case EDIT_CLEF:
            func = (fn_type *) seek_clef_directive;
            break;
          case EDIT_KEY:
            func = (fn_type *) seek_keysig_directive;
            break;
          case EDIT_TIMESIG:
            func = (fn_type *) seek_timesig_directive;
            break;
          case EDIT_STEMDIR:
            func = (fn_type *) seek_stemdirective_directive;
            break;
          default:
            g_critical ("Unknown type");
            func = NULL;
            break;

          }
        if (func)
          {
            button = gtk_button_new_with_label (_("Next ➡"));
            //get_color (&color, 0.0, 0.7, 0.7, 1.0);
            //gtk_widget_override_color (button, GTK_STATE_FLAG_NORMAL, &color);
            set_foreground_color (button, "rgb(0,180,180)");
            g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (func), (gpointer) label);
            gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
          }
      }
      if ((type == EDIT_NOTE) || (type == EDIT_CHORD) || (type == EDIT_CLEF)|| (type == EDIT_KEY)|| (type == EDIT_TIMESIG))
        {
          button = gtk_button_new_with_label (_("Conditional"));
#if GTK_MAJOR_VERSION == 2  
  get_color (&color, 0.0, 0.0, 0.5, 1.0);
  gtk_widget_override_color (button, GTK_STATE_FLAG_NORMAL, &color);
#else
    set_foreground_color (button, "rgb(0, 0, 128)");//dark blue
#endif
          if (type == EDIT_CHORD)
            g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (make_chord_directive_conditional), (gpointer) directive->tag->str);
          else
           if (type == EDIT_CLEF)
            g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (make_clef_directive_conditional), (gpointer) directive->tag->str);
          if (type == EDIT_KEY)
            g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (make_keysig_directive_conditional), (gpointer) directive->tag->str);
          if (type == EDIT_TIMESIG)
            g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (make_timesig_directive_conditional), (gpointer) directive->tag->str);
          else
            g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (make_note_directive_conditional), (gpointer) directive->tag->str);
          gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

          button = gtk_button_new_with_label (_("Copy"));
          //get_color (&color, 0.0, 0.4, 0.5, 1.0);
          //gtk_widget_override_color (button, GTK_STATE_FLAG_NORMAL, &color);
          set_foreground_color (button,  "rgb(0,110,128)");
          if (type == EDIT_CHORD)
            g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (copy_chord_directive_to_clipboard), (gpointer) directive->tag->str);
          else
            g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (copy_note_directive_to_clipboard), (gpointer) directive->tag->str);
          gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);


        }

      if (tooltip)
        {
          button = gtk_button_new_with_label (_("Help"));
          //get_color (&color, 0.0, 0.7, 0.7, 1.0);       //color.red = 0.0; color.green = 0.7,  color.blue = 0.3; color.alpha = 1.0;
          //gtk_widget_override_color (button, GTK_STATE_FLAG_NORMAL, &color);
          set_foreground_color (button, "rgb(0,255,180)");
          g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (display_help), (gpointer) tooltip);
          gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
        }
      if (tooltip == NULL)
        tooltip = _("No tooltip");







      if (action)
        {
          button = gtk_button_new_with_label (_("Create Button for Command"));
          gtk_widget_set_tooltip_text (button, _("Make a palette button for running the command that created this attribute."));
          g_object_set_data (G_OBJECT (button), "directive", (gpointer) directive);
          g_signal_connect (button, "clicked", G_CALLBACK (create_palette_button_for_command), (gpointer) tooltip);
          gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
        }

      button = gtk_button_new_with_label (_("Create Button for Clone"));
      gtk_widget_set_tooltip_text (button, _("Make a palette button for installing this attribute elsewhere."));
      g_object_set_data (G_OBJECT (button), "directive", (gpointer) directive);
      g_signal_connect (button, "clicked", G_CALLBACK (create_palette_button_for_directive), (gpointer) (type_str[type]));
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

      button = gtk_button_new_with_label (_("Advanced"));
      gtk_widget_set_tooltip_text (button, _("Examine/Edit this directive at a low-level"));
      g_object_set_data (G_OBJECT (button), "directives", (gpointer) pdirectives);
      g_object_set_data (G_OBJECT (button), "directive", (gpointer) directive);
      g_signal_connect (button, "clicked", G_CALLBACK (advanced_edit_type_directive), func);
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
    }
}

static void
place_chord_attributes (GtkWidget * vbox, chord * thechord)
{
  GtkWidget *expander = gtk_expander_new (_("Built-in Chord Attributes"));      //gtk_expander_new
  gtk_expander_set_expanded (GTK_EXPANDER (expander), TRUE);
  gtk_widget_set_sensitive (expander, TRUE);
  gtk_container_set_border_width (GTK_CONTAINER (expander), 10);
  GdkRGBA color;
#if GTK_MAJOR_VERSION == 2
  get_color (&color, 0.1, 0.1, 0.8, 1.0);
  gtk_widget_override_color (expander, GTK_STATE_FLAG_NORMAL, &color);
#else
  set_foreground_color (expander, "rgb(25, 25, 200)");//blue
#endif 
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
  if (!gtk_container_get_children (GTK_CONTAINER (inner_box)))
    gtk_widget_destroy (expander);
}

static void
update_and_close (GtkWidget * editwin)
{
  update_object_info ();
  gtk_widget_destroy (editwin);
  reset_cursors ();
}

static void
run_script (GtkWidget * button, gchar * script)
{
  call_out_to_guile (script);
  gtk_widget_destroy (gtk_widget_get_parent (gtk_widget_get_parent (button)));
  reset_cursors ();
}

static void
delete_standalone (GtkWidget * button)
{
  dnm_deleteobject (Denemo.project->movement);
  gtk_widget_destroy (gtk_widget_get_toplevel (button));
  score_status (Denemo.project, TRUE);
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
  TheEditorWidget = editwin;
  GdkRGBA color;
#if GTK_MAJOR_VERSION == 2  
  get_color (&color, 1.0, 1.0, 1.0, 1.0);       //.red = color.green = color.blue = color.alpha = 1.0;
  gtk_widget_override_background_color (editwin, GTK_STATE_FLAG_NORMAL, &color);
  
#else 
{
GtkCssProvider *gcp;
GtkStyleContext *gsc;
gsc = gtk_widget_get_style_context(GTK_WIDGET (editwin));
gchar *str = "GtkWindow {background-color: #ffffff;}"; 
gcp= gtk_css_provider_new();
gtk_css_provider_load_from_data(gcp, str, -1, 0);
gtk_style_context_add_provider(gsc, GTK_STYLE_PROVIDER(gcp), 
    GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
}
#endif
  
  gtk_window_set_modal (GTK_WINDOW (editwin), TRUE);
  gtk_window_set_title (GTK_WINDOW (editwin), _("Denemo Object Editor"));
  gtk_window_set_transient_for (GTK_WINDOW (editwin), GTK_WINDOW (Denemo.window));
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

  if ((!si->currentobject->prev) && (!si->currentmeasure->prev))
    gtk_widget_set_sensitive (button, FALSE);

  GtkWidget *note_up_button = gtk_button_new_with_label (_("Next note in chord"));
  g_signal_connect_swapped (note_up_button, "clicked", G_CALLBACK (move_to_next_note), editwin);
  gtk_box_pack_start (GTK_BOX (hbox), note_up_button, FALSE, TRUE, 0);
  
  GtkWidget *alternate_note_button = gtk_button_new_with_label (_("Alternate note"));
  
  gtk_box_pack_start (GTK_BOX (hbox), alternate_note_button, FALSE, TRUE, 0);

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
            GtkWidget *frame = gtk_frame_new (thechord->notes ? _("Attached to the chord:") : _("Attached to the rest:"));
            gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
            //GdkRGBA color;
            //get_color (&color, 0.8, 0.1, 0.1, 1.0);     //color.red = 0.8;color.green = color.blue = 0.1; color.alpha = 1;
            //gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
            set_foreground_color (frame, "rgb(200,25,25)");
            gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
            GtkWidget *inner_box = gtk_vbox_new (FALSE, 0);
            //get_color (&color, 0.1, 0.8, 0.1, 1.0);
            //gtk_widget_override_color (inner_box, GTK_STATE_FLAG_NORMAL, &color);
            set_foreground_color (inner_box, "rgb(25,200,25)");
            gtk_container_add (GTK_CONTAINER (frame), inner_box);
            place_directives (inner_box, &thechord->directives, EDIT_CHORD);
          }

        if (thechord->notes)
          {
            place_chord_attributes (vbox, thechord);
            note *thenote = findnote (curObj, Denemo.project->movement->cursor_y);
            GList * notenode = g_list_find (thechord->notes, thenote);
            if (!thechord->notes->next) //only one note
              gtk_button_set_label (GTK_BUTTON (note_up_button), _("Edit the note"));
            if ((!thenote) || thechord->notes->next || (Denemo.project->movement->cursor_y != thenote->mid_c_offset))
              {
                note_up_button = NULL;  // a tricksy bit of code this: the button is already packed in the vbox, by setting this NULL we stop it being set insensitive as it must be for all other cases.
              }
              
              
           if (notenode->next)
                {
                   note *nextnote = notenode->next->data;
                   if (thenote->mid_c_offset == nextnote->mid_c_offset)
                        {
                          g_signal_connect_swapped (alternate_note_button, "clicked", G_CALLBACK (swap_notes_for_edit), notenode);
                          alternate_note_button = NULL; //to prevent it being set insensitive
                        }
                    
                }
              
              
              
              
            if (thenote && Denemo.project->movement->cursor_y == thenote->mid_c_offset)
              {
                GString *text = g_string_new ("");

                if (thenote->directives)
                  {
                    g_string_append_printf (text, _("Attached to Note %s"), pretty_name (mid_c_offsettolily (thenote->mid_c_offset, thenote->enshift)));
                    GtkWidget *frame = gtk_frame_new (text->str);
                    gtk_container_set_border_width (GTK_CONTAINER (frame), 20);
                    gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
                    //GdkRGBA color;
                    //get_color (&color, 0.2, 0.7, 0.2, 1.0);     /// color.green = 0.7; color.red = color.blue = 0.2; color.alpha = 1;
                    //gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
                    set_foreground_color (frame, "rgb(50,200,50)");
                    gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
                    GtkWidget *inner_box = gtk_vbox_new (FALSE, 0);
                    //get_color (&color, 0.1, 0.8, 0.1, 1.0);
                    //gtk_widget_override_color (inner_box, GTK_STATE_FLAG_NORMAL, &color);
                    set_foreground_color (inner_box, "rgb(25,200,25)");//this colors the description of the directives placed inside it
                    gtk_container_add (GTK_CONTAINER (frame), inner_box);
                    place_directives (inner_box, &thenote->directives, EDIT_NOTE);
                  }
                else
                  {
                    g_string_append_printf (text, _("Nothing attached to Note %s"), pretty_name (mid_c_offsettolily (thenote->mid_c_offset, thenote->enshift)));
                    GtkWidget *frame = gtk_frame_new (text->str);
                    gtk_container_set_border_width (GTK_CONTAINER (frame), 20);
                    gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
                    //GdkRGBA color;
                    //get_color (&color, 0.8, 0.1, 0.1, 1.0);     //color.red = 0.8;color.green = color.blue = 0.1; color.alpha = 1;
                    //gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
                    set_foreground_color (frame, "rgb(200,25,25)");
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
      {
        tuplet *thetup = ((tuplet *) curObj->object);
        //type = _("start tuplet marker");
        GtkWidget *button = gtk_button_new_with_label (_("Alter Tuplet Type"));
        g_signal_connect_swapped (button, "clicked", G_CALLBACK (call_out_to_guile), "(d-StartTuplet)");
        gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, TRUE, 0);
        if (thetup->directives)
          {
            GtkWidget *frame = gtk_frame_new (_("Attached to the tuplet start:"));
            //GdkRGBA color;
            //get_color (&color, 0.8, 0.1, 0.1, 1.0);
            //gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
            set_foreground_color (frame, "rgb(200,25,25)");
            gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
            gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
            GtkWidget *inner_box = gtk_vbox_new (FALSE, 8);
            gtk_container_add (GTK_CONTAINER (frame), inner_box);
            place_directives (inner_box, &thetup->directives, EDIT_TUPLET_START);
          }
      }
      break;
    case TUPCLOSE:
      {
        tuplet *thetup = ((tuplet *) curObj->object);
        //type = _("start tuplet marker");
        if (thetup->directives)
          {
            GtkWidget *frame = gtk_frame_new (_("Attached to the tuplet end:"));
            //GdkRGBA color;
            //get_color (&color, 0.8, 0.1, 0.1, 1.0);
            //gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
            set_foreground_color (frame, "rgb(200,25,25)");
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
            //GdkRGBA color;
            //get_color (&color, 0.8, 0.1, 0.1, 1.0);
            //gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
            set_foreground_color (frame, "rgb(200,25,25)");
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
            //GdkRGBA color;
            //get_color (&color, 0.8, 0.1, 0.1, 1.0);
            //gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
            gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
            set_foreground_color (frame, "rgb(200,25,25)");
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
            //GdkRGBA color;
            //get_color (&color, 0.8, 0.1, 0.1, 1.0);
            //gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
            set_foreground_color (frame, "rgb(200,25,25)");
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
            //GdkRGBA color;
            //get_color (&color, 0.8, 0.1, 0.1, 1.0);
            //gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
            set_foreground_color (frame, "rgb(200,25,25)");
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
        DenemoAction *action = lookup_action_from_name (directive->tag->str);
        gchar *name = label ? (gchar *) label : directive->tag->str;
        const gchar *tooltip = get_tooltip_for_command (directive->tag->str);
        gchar *filename = get_editscript_filename (directive->tag->str);
        GtkWidget *frame = gtk_frame_new (_("Standalone Denemo Directive:"));
        gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
        //GdkRGBA color;
        //get_color (&color, 0.5, 0.5, 0.1, 1.0); // .red = 0.5;
        //color.green = 0.5; color.blue = 0.1; color.alpha = 1;
        //gtk_widget_override_color (frame, GTK_STATE_FLAG_NORMAL, &color);
        set_foreground_color (frame, "rgb(128,128,25)");
        gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
        GtkWidget *inner_box = gtk_vbox_new (FALSE, 0);
        //get_color (&color, 0.8, 0.1, 0.1, 1.0);
        //gtk_widget_override_color (inner_box, GTK_STATE_FLAG_NORMAL, &color);
        set_foreground_color (frame, "rgb(200,25,25)");
        gtk_container_add (GTK_CONTAINER (frame), inner_box);

        if (filename)
          {
            gchar *thelabel = g_strconcat (_("Run the Edit Script for "), name, NULL);
            GtkWidget *button = gtk_button_new_with_label (thelabel);
            g_signal_connect (button, "clicked", G_CALLBACK (execute_editscript), filename);
            g_signal_connect_swapped (button, "destroy", G_CALLBACK (g_free), filename);
            gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);
            g_free (thelabel);
          }
        else if (action)
          {
            gchar *thelabel = g_strconcat (_("Execute command: "), name, NULL);
            GtkWidget *button = gtk_button_new_with_label (thelabel);
            gtk_widget_set_tooltip_text (button, _("Re-run the command to edit the Denemo Directive"));

            g_object_set_data (G_OBJECT (button), "action", (gpointer) action);
            g_signal_connect (button, "clicked", G_CALLBACK (call_edit_on_action), NULL);
            gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);
            g_free (thelabel);
          }
        {
          GtkWidget *hbox = gtk_hbox_new (FALSE, 0);
          gtk_box_pack_start (GTK_BOX (inner_box), hbox, FALSE, TRUE, 0);


          button = gtk_button_new_with_label (_("Delete"));
          //GtkWidget *labelwidget = (GtkWidget *) gtk_bin_get_child (GTK_BIN (button));
          //get_color (&color, 1.0, 0.0, 0.0, 1.0);
          //color.red = 1.0; color.green = color.blue = 0.0; color.alpha = 1.0;
          //gtk_widget_override_color (labelwidget, GTK_STATE_FLAG_NORMAL, &color);
          
          set_foreground_color (button, "rgb(255,0,0)");
          g_object_set_data (G_OBJECT (button), "directive", (gpointer) directive);
          g_signal_connect (button, "clicked", G_CALLBACK (delete_standalone), NULL);
          gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 30);

          button = gtk_button_new_with_label (_("Next ➡"));
         // get_color (&color, 0.0, 0.7, 0.7, 1.0);
         // gtk_widget_override_color (button, GTK_STATE_FLAG_NORMAL, &color);
          set_foreground_color (button, "rgb(0,180,180)");
          g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (seek_standalone_directive), (gpointer) label);
          gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

          {
            button = gtk_button_new_with_label (_("Conditional"));
            //get_color (&color, 0.0, 0., 0.5, 1.0);
            //gtk_widget_override_color (button, GTK_STATE_FLAG_NORMAL, &color);
            set_foreground_color (button, "#000080");

            g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (call_out_to_guile), (gpointer) "(d-ChooseCondition #f)");

            gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
          }

          if (tooltip)
            {
              button = gtk_button_new_with_label (_("Help"));
              //get_color (&color, 0.0, 0.7, 0.3, 1.0);   //color.red = 0.0; color.green = 0.7,  color.blue = 0.3; color.alpha = 1.0;
              //gtk_widget_override_color (button, GTK_STATE_FLAG_NORMAL, &color);
              set_foreground_color (button, "#00b34d");
              
              g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (display_help), (gpointer) tooltip);
              gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
            }
          if (tooltip == NULL)
            tooltip = _("No tooltip");

          if (action)
            {
              button = gtk_button_new_with_label (_("Create Button for Command"));
              gtk_widget_set_tooltip_text (button, _("Make a palette button for running the command that created/inserted this object."));
              g_object_set_data (G_OBJECT (button), "directive", (gpointer) directive);
              g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (create_palette_button_for_command), (gpointer) tooltip);
              gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
            }
          button = gtk_button_new_with_label (_("Create Button for Clone"));
          gtk_widget_set_tooltip_text (button, _("Make a palette button for inserting a clone of this object elsewhere."));
          g_object_set_data (G_OBJECT (button), "directive", (gpointer) directive);
          g_signal_connect (button, "clicked", G_CALLBACK (create_palette_button_for_directive), "standalone");
          gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

          button = gtk_button_new_with_label (_("Advanced"));
          g_object_set_data (G_OBJECT (button), "directives", NULL);
          g_object_set_data (G_OBJECT (button), "directive", (gpointer) directive);
          g_signal_connect (button, "clicked", G_CALLBACK (advanced_edit_type_directive), text_edit_standalone_directive);
          gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
        }
      }
      break;

    default:
      g_critical ("Type not done");
      break;
    }
  if (g_list_length (gtk_container_get_children (GTK_CONTAINER (vbox))) == 1)
    {                           //just the close button
      warningdialog ("Nothing editable on this object\nYou can add attributes to the object at the cursor by right-clicking on it.");
      gtk_widget_destroy (editwin);
    }
  else
    {
      if (note_up_button)
        gtk_widget_set_sensitive (note_up_button, FALSE);
      if (alternate_note_button)
        gtk_widget_set_sensitive (alternate_note_button, FALSE);
      gtk_widget_show_all (editwin);
      gtk_window_present (GTK_WINDOW (editwin));
      gdk_window_set_cursor (gtk_widget_get_window (editwin), Denemo.GDK_LEFT_PTR);
      gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), Denemo.GDK_X_CURSOR);
      if (ObjectInfo)
        gdk_window_set_cursor (gtk_widget_get_window (ObjectInfo), Denemo.GDK_X_CURSOR);
      if (Denemo.printarea && gtk_widget_get_window (Denemo.printarea))
        gdk_window_set_cursor (gtk_widget_get_window (Denemo.printarea), Denemo.GDK_X_CURSOR);
#ifdef G_OS_WIN32
      if (ObjectInfo)
        gtk_widget_hide (ObjectInfo);   //windows doesn't handle order properly, so this can hide the object editor
#endif
    }
}






static void
score_update_and_close (GtkWidget * editwin)
{
  gtk_widget_destroy (editwin);
  reset_cursors ();
}

static void
go_previous (GtkWidget * editwin)
{
  prev_movement (NULL, NULL);   //FIXME pass in a DenemoParam * to get status
  if (editwin)
    {
      gtk_widget_destroy (editwin);
      reset_cursors ();
      edit_movement_properties ();
    }
}

static void
go_next (GtkWidget * editwin)
{
  next_movement (NULL, NULL);   //FIXME pass in a DenemoParam * to get status
  if (editwin)
    {
      gtk_widget_destroy (editwin);
      reset_cursors ();
      edit_movement_properties ();
    }
}

static void
low_level_edit_type_directive (GtkWidget * button, gpointer rerun)
{
  gtk_widget_hide (gtk_widget_get_toplevel (button));
  DenemoDirective *directive = (DenemoDirective *) g_object_get_data (G_OBJECT (button), "directive");
  GList **directives = (GList **) g_object_get_data (G_OBJECT (button), "directives");
  signal_structural_change (Denemo.project);    // system directive may be edited in the next call
  if (!low_level_directive_edit (directive))
    {
      gtk_widget_destroy (gtk_widget_get_toplevel (button));
      if (directives)
        {
          *directives = g_list_remove (*directives, directive);
          free_directive (directive);
        }
      if (rerun)
        G_CALLBACK (rerun) ();
      score_status (Denemo.project, TRUE);
    }
  else
    show_window (button);       //gtk_widget_show (gtk_widget_get_toplevel (button));

}

static void
delete_score_directive (GtkWidget * button, gpointer rerun)
{
  DenemoDirective *directive = (DenemoDirective *) g_object_get_data (G_OBJECT (button), "directive");
  GList **directives = (GList **) g_object_get_data (G_OBJECT (button), "directives");
  *directives = g_list_remove (*directives, directive);
  free_directive (directive);
  gtk_widget_destroy (gtk_widget_get_toplevel (button));
  signal_structural_change (Denemo.project);
  G_CALLBACK (rerun) ();        //edit_score_properties ();
  score_status (Denemo.project, TRUE);
}


static void
open_command_center_for_action (DenemoAction *action)
{
    gint idx = lookup_command_from_name (Denemo.map, denemo_action_get_name (action));
    command_center_select_idx (NULL, idx);
    gtk_widget_destroy (TheEditorWidget);
    TheEditorWidget = NULL;
}

static void
place_buttons_for_directives (GList ** pdirectives, GtkWidget * vbox, DIRECTIVE_TYPE score_or_movement, gchar * field)
{
  GList *g;
  gchar *type = "";
  gchar *thecolor = "#ffffff";
  if (!strcmp (field, "lilycontrol"))
    thecolor = "#e0ffff", type = _("Score ");
  else if (!strcmp (field, "movementcontrol"))
    thecolor = "#e0ffff", type = _("Movement ");
  else if (!strcmp (field, "scoreheader"))
    thecolor = "#ffe0ff", type = _("Score Header ");
  else if (!strcmp (field, "paper"))
    thecolor = "#ffffe0", type = _("Paper ");
  else if (!strcmp (field, "header"))
    thecolor = "#ffe0ff", type = _("Movement Header ");
  else if (!strcmp (field, "layout"))
    thecolor = "#ffffe0", type = _("Movement Layout ");
  else if (!strcmp (field, "keysig"))
    thecolor = "#e0ffff", type = _("Key Signature");
  else if (!strcmp (field, "timesig"))
    thecolor = "#ffe0ff", type = _("Time Signature");
  else if (!strcmp (field, "clef"))
    thecolor = "#ffffe0", type = _("Clef");
  else if (!strcmp (field, "staff"))
    thecolor = "#ffffff", type = _("Staff");
 else if (!strcmp (field, "voice"))
    thecolor = "#ffffff", type = _("Voice");
      
        
   {
   gchar *text = g_strdup_printf (" \n▼▼▼%s %s▼▼▼", type, _("Directives"));
   GtkWidget *label = gtk_label_new (text);
   //set_background_color (label, "#f0f060");
   set_background_color (label, thecolor);
   g_free (text);
   gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, TRUE, 0);
   set_background_color (vbox, "#e0e0a0");
   //set_background_color (vbox, thecolor);
   }
        
  for (g = *pdirectives; g; g = g->next)
    {
      DenemoDirective *directive = g->data;
      const gchar *label = get_label_for_command (directive->tag->str);
      DenemoAction *action = lookup_action_from_name (directive->tag->str);
      gchar *name = label ? (gchar *) label : directive->tag->str;
      const gchar *tooltip = get_tooltip_for_command (directive->tag->str);
      gchar *filename = get_editscript_filename (directive->tag->str);
      gchar *display = directive->display ? directive->display->str : "";
      GtkWidget *frame;
      gchar *text, *oneline;
      oneline = g_strescape (name,"");
      
 
        
      if (label == NULL)
        text = g_strdup_printf (_("%sDenemo %s Directive tagged: %s %c%s%c"), (directive->layouts) ? _("(Conditional) ") : "", type, oneline, '[', display, ']');
      else
        text = g_strdup_printf (_("%sDenemo %s Directive: %s %c%s%c"), (directive->layouts) ? _("(Conditional) ") : "", type, label, '[', display, ']');
      frame = gtk_frame_new (text);
#if ((GTK_MAJOR_VERSION==3)&&(GTK_MINOR_VERSION>=12))
      gtk_widget_set_margin_start (frame, 50);
      gtk_widget_set_margin_top (frame, 30);
#endif
      g_free (text);
      g_free (oneline);
      
      set_foreground_color (frame, "#404010");
      //set_foreground_color (frame, "#000000");
      
      GtkWidget *evbox = gtk_event_box_new (); set_background_color (evbox, thecolor);
      gtk_container_add (GTK_CONTAINER (vbox), evbox);
      gtk_container_add (GTK_CONTAINER (evbox), frame);
      GtkWidget *inner_box = gtk_vbox_new (FALSE, 0);
      set_foreground_color (inner_box, "#303c20");//what does this do???
      
      gtk_container_add (GTK_CONTAINER (frame), inner_box);
      GtkWidget *button;
 

      if (filename)
        {
          gchar *thelabel = g_strconcat (_("Run the Edit Script for "), name, NULL);
          button = gtk_button_new_with_label (thelabel);
          g_signal_connect (button, "clicked", G_CALLBACK (execute_editscript), filename);
          g_signal_connect_swapped (button, "destroy", G_CALLBACK (g_free), filename);
          gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);
          g_free (thelabel);
        }
      else if (action)
        {
          gchar *thelabel = g_strconcat (_("Execute command: "), name, NULL);
          button = gtk_button_new_with_label (thelabel);
          set_foreground_color (button, "rgb(0,70,130)");
          gtk_widget_set_tooltip_text (button, _("Re-run the command to edit the Denemo Directive"));

          g_object_set_data (G_OBJECT (button), "action", (gpointer) action);
          g_signal_connect (button, "clicked", G_CALLBACK (call_edit_on_action), GINT_TO_POINTER (score_or_movement));
          GtkWidget *hbox = gtk_hbox_new (FALSE, 0);
          gtk_box_pack_start (GTK_BOX (inner_box), hbox, FALSE, FALSE, 0);
          gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, FALSE, 0);
          
          g_free (thelabel);
        }
      if (tooltip)
        gtk_widget_set_tooltip_text (button, tooltip);

      GtkWidget *hbox = gtk_hbox_new (FALSE, 0);
      gtk_box_pack_start (GTK_BOX (inner_box), hbox, FALSE, TRUE, 0);

      button = gtk_button_new_with_label (_("Delete"));
      //GtkWidget *labelwidget = (GtkWidget *) gtk_bin_get_child (GTK_BIN (button));
      //get_color (&color, 1.0, 0.0, 0.0, 1.0);
      //color.red = 1.0; color.green = color.blue = 0.0; color.alpha = 1.0;
      //gtk_widget_override_color (labelwidget, GTK_STATE_FLAG_NORMAL, &color);
      
      set_foreground_color (button, "rgb(255,0,0)");
      g_object_set_data (G_OBJECT (button), "directives", (gpointer) pdirectives);
      g_object_set_data (G_OBJECT (button), "directive", (gpointer) directive);
      g_signal_connect (button, "clicked", G_CALLBACK (delete_score_directive), get_rerun (field));
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 30);

      install_conditional_button (hbox, directive, field);

      if (tooltip)
        {
          button = gtk_button_new_with_label (_("Help"));
          //get_color (&color, 0.0, 0.7, 0.3, 1.0);
          //gtk_widget_override_color (button, GTK_STATE_FLAG_NORMAL, &color);
          set_foreground_color(button, "#00b34d");
          g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (display_help), (gpointer) tooltip);
          gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
        }



      if (tooltip == NULL)
        tooltip = _("No tooltip");

      button = gtk_button_new_with_label (_("Create Duplicate"));
      gtk_widget_set_tooltip_text (button, _("Duplicate this directive with a new name. Usually only makes sense when the two directives are conditional applying to different layouts."));
      g_object_set_data (G_OBJECT (button), "directive", (gpointer) directive);
      g_object_set_data (G_OBJECT (button), "directives", pdirectives);
      g_signal_connect (button, "clicked", G_CALLBACK (create_duplicate_directive), (gpointer) (field));
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);



      if (action)
        {
          button = gtk_button_new_with_label (_("Create Button for Command"));
          gtk_widget_set_tooltip_text (button, _("Make a palette button for running the command that created this attribute."));
          g_object_set_data (G_OBJECT (button), "directive", (gpointer) directive);
          g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (create_palette_button_for_command), (gpointer) tooltip);
          gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
          
          
          button = gtk_button_new_with_label (_("Open Command Center"));
          gtk_widget_set_tooltip_text (button, _("Opens the Command Center on this command. Here you can find the location of the command in the menu system, set shortcuts etc."));
          g_object_set_data (G_OBJECT (button), "directive", (gpointer) directive);
          g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (open_command_center_for_action), (gpointer) action);
          gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
             
          
          
        }
      button = gtk_button_new_with_label (_("Create Button for Clone"));
      gtk_widget_set_tooltip_text (button, _("Make a palette button for installing a clone of this attribute elsewhere."));
      g_object_set_data (G_OBJECT (button), "directive", (gpointer) directive);
      g_signal_connect (button, "clicked", G_CALLBACK (create_palette_button_for_directive), (gpointer) (field));
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);


      button = gtk_button_new_with_label (_("Advanced"));
      g_object_set_data (G_OBJECT (button), "directives", pdirectives);
      g_object_set_data (G_OBJECT (button), "directive", (gpointer) directive);
      g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (low_level_edit_type_directive), get_rerun (field));
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
    }
}


static void
edit_score_and_movement_properties (gboolean show_score)
{
  GtkWidget *editscorewin = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  TheEditorWidget = editscorewin;
  gint window_height = 800;
  GdkRGBA color;
#if GTK_MAJOR_VERSION == 2  
  
  get_color (&color, 1.0, 1.0, 1.0, 1.0);       //.red = color.green = color.blue = color.alpha = 1.0;
  gtk_widget_override_background_color (editscorewin, GTK_STATE_FLAG_NORMAL, &color);
  
#else 
{
GtkCssProvider *gcp;
GtkStyleContext *gsc;
gsc = gtk_widget_get_style_context(GTK_WIDGET (editscorewin));
gchar *str = "GtkWindow {background-color: #ffffff;}"; 
gcp= gtk_css_provider_new();
gtk_css_provider_load_from_data(gcp, str, -1, 0);
gtk_style_context_add_provider(gsc, GTK_STYLE_PROVIDER(gcp), 
    GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
}
#endif  
  
  
  gtk_window_set_modal (GTK_WINDOW (editscorewin), TRUE);
  gtk_window_set_title (GTK_WINDOW (editscorewin), _("Score and Movement Properties Editor"));
  gtk_window_set_transient_for (GTK_WINDOW (editscorewin), GTK_WINDOW (Denemo.window));
  gtk_window_set_keep_above (GTK_WINDOW (editscorewin), TRUE);
  gtk_window_set_default_size (GTK_WINDOW (editscorewin), 1400, window_height);


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
  gtk_expander_set_expanded (GTK_EXPANDER (expander), show_score);
  gtk_widget_set_sensitive (expander, TRUE);
  gtk_container_set_border_width (GTK_CONTAINER (expander), 10);
  //get_color (&color, 0.1, 0.8, 0.1, 1.0);

  //gtk_widget_override_color (expander, GTK_STATE_FLAG_NORMAL, &color);
  
  
  //set_foreground_color (expander, "rgb(2525,200,25)");
  set_foreground_color (expander, "rgb(0,100,0)");// "Edit Built-in Properties" and "Score Directives" are this color
  // gtk_box_pack_start (GTK_BOX (vbox), expander, TRUE, TRUE, 0);

  GtkWidget *frame = gtk_frame_new (NULL);
  //gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_IN);



  gtk_paned_add1 (GTK_PANED (pane), frame);

  gtk_container_add (GTK_CONTAINER (frame), expander);

  GtkWidget *scrolled_window = gtk_scrolled_window_new (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0), gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
#if GTK_MAJOR_VERSION==3
  gtk_widget_set_vexpand (scrolled_window, TRUE);
#endif  
  gtk_container_add (GTK_CONTAINER (expander), scrolled_window);

  GtkWidget *inner_box = gtk_vbox_new (FALSE, 0);
  
#if GTK_MAJOR_VERSION==2       
          gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (scrolled_window), inner_box);
#else          
          gtk_container_add (GTK_CONTAINER(scrolled_window), inner_box);
#endif   

  button = gtk_button_new_with_label (_("Edit Built-in Score Properties"));
  g_signal_connect (button, "clicked", G_CALLBACK (call_score_properties_dialog), NULL);
  gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);

  place_buttons_for_directives ((GList **) & Denemo.project->lilycontrol.directives, inner_box, DIRECTIVE_SCORE, "lilycontrol");
  place_buttons_for_directives ((GList **) & Denemo.project->scoreheader, inner_box, DIRECTIVE_SCORE, "scoreheader");
  place_buttons_for_directives ((GList **) & Denemo.project->paper, inner_box, DIRECTIVE_SCORE, "paper");



  gchar *mnum = g_strdup_printf ("%s %d %s", _("Movement"), Denemo.project->movement->currentmovementnum, _("Properties"));
  expander = gtk_expander_new (mnum);
  g_free (mnum);
  gtk_expander_set_expanded (GTK_EXPANDER (expander), !show_score);
  gtk_widget_set_sensitive (expander, TRUE);
  gtk_container_set_border_width (GTK_CONTAINER (expander), 10);
#if GTK_MAJOR_VERSION == 2
  get_color (&color, 0.1, 0.1, 0.8, 1.0);
  gtk_widget_override_color (expander, GTK_STATE_FLAG_NORMAL, &color);
#else
  set_foreground_color (expander, "rgb(25, 25, 200)");//blue
#endif 


  frame = gtk_frame_new (NULL);
  //gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_IN);

  gtk_paned_add2 (GTK_PANED (pane), frame);
  gtk_container_add (GTK_CONTAINER (frame), expander);

  scrolled_window = gtk_scrolled_window_new (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0), gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
#if GTK_MAJOR_VERSION==3
  gtk_widget_set_vexpand (scrolled_window, TRUE);
#endif
  gtk_container_add (GTK_CONTAINER (expander), scrolled_window);
  inner_box = gtk_vbox_new (FALSE, 0);
          
#if GTK_MAJOR_VERSION==2       
          gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (scrolled_window), inner_box);
#else          
          gtk_container_add (GTK_CONTAINER(scrolled_window), inner_box);
#endif    
  GtkWidget *hbox = gtk_hbox_new (FALSE, 0);
  gtk_box_pack_start (GTK_BOX (inner_box), hbox, FALSE, TRUE, 0);

  button = gtk_button_new_with_label (_("⬅ Previous Movement"));
  g_signal_connect_swapped (button, "clicked", G_CALLBACK (go_previous), editscorewin);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  if (Denemo.project->movement->currentmovementnum == 1)
    gtk_widget_set_sensitive (button, FALSE);

  button = gtk_button_new_with_label (_("Next Movement ➡"));
  g_signal_connect_swapped (button, "clicked", G_CALLBACK (go_next), editscorewin);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  if (g_list_length (Denemo.project->movements) == Denemo.project->movement->currentmovementnum)
    gtk_widget_set_sensitive (button, FALSE);


  place_buttons_for_directives ((GList **) & Denemo.project->movement->movementcontrol, inner_box, DIRECTIVE_MOVEMENT, "movementcontrol");
  place_buttons_for_directives ((GList **) & Denemo.project->movement->header, inner_box, DIRECTIVE_MOVEMENT, "header");
  place_buttons_for_directives ((GList **) & Denemo.project->movement->layout, inner_box, DIRECTIVE_MOVEMENT, "layout");

  gtk_paned_set_position (GTK_PANED (pane), show_score ? window_height - 50 : 50);



  if (g_list_length (gtk_container_get_children (GTK_CONTAINER (vbox))) == 1)
    {                           //just the close button
      warningdialog ("No properties have been set on the current score.");
      gtk_widget_destroy (editscorewin);
    }
  else
    {
      gtk_widget_show_all (editscorewin);
      gtk_window_present (GTK_WINDOW (editscorewin));
      gdk_window_set_cursor (gtk_widget_get_window (editscorewin), Denemo.GDK_LEFT_PTR);
      gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), Denemo.GDK_X_CURSOR);
      if (ObjectInfo)
        gdk_window_set_cursor (gtk_widget_get_window (ObjectInfo), Denemo.GDK_X_CURSOR);
      if (Denemo.printarea && gtk_widget_get_window (Denemo.printarea))
        gdk_window_set_cursor (gtk_widget_get_window (Denemo.printarea), Denemo.GDK_X_CURSOR);

    }

}


void
edit_score_properties (void)
{
  edit_score_and_movement_properties (TRUE);
}

void
edit_movement_properties (void)
{
  edit_score_and_movement_properties (FALSE);
}

static void
clef_change_initial_cb (GtkWidget * editstaffwin)
{
  DenemoStaff *thestaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (thestaff->voicecontrol != DENEMO_PRIMARY)
    warningdialog (_("The clef here only affects the display as this voice is typeset on the staff above.\nNormally you will want it set the same as the staff the notes will appear on.\nDismiss this warning and make any needed changes via the popup dialog coming next."));
  clef_change_initial (NULL, NULL);
}

static void
keysig_change_info (void)
{
  DenemoStaff *thestaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (thestaff->voicecontrol != DENEMO_PRIMARY)
    {
      warningdialog (_("This voice should have the same key signature as the staff it appears on. Use the key signature menu commands to correct it if needed."));      // warningdialog (param.string);
    }
  else
    {
      warningdialog (_("Change the keysignature by clicking on it, or via the Key Signatures menu (after closing this editor)."));      //for some reason the change_key() call does not get the keyboard focus so we can't change the key here.
    }
}

static void
timesig_change_initial_cb (GtkWidget * editstaffwin)
{
  DenemoStaff *thestaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (thestaff->voicecontrol != DENEMO_PRIMARY)
    {
      warningdialog (_("This voice should have the same time signature as the staff it appears on. Use the time signature menu commands to correct it if needed."));    // warningdialog (param.string);
    }
  else
    {
      timesig_change (Denemo.project, CHANGEINITIAL);
      gtk_widget_destroy (editstaffwin);
      edit_staff_and_voice_properties (TRUE);
    }
}

static void
staff_above (GtkWidget * editstaffwin)
{
  movetostaffup (NULL, NULL);
  gtk_widget_destroy (editstaffwin);
  edit_staff_and_voice_properties (TRUE);
}

static void
staff_below (GtkWidget * editstaffwin)
{
  movetostaffdown (NULL, NULL);
  gtk_widget_destroy (editstaffwin);
  edit_staff_and_voice_properties (TRUE);
}

static void
change_staff_properties (GtkWidget * editstaffwin)
{
  gtk_widget_destroy (editstaffwin);
  staff_properties_change_cb (NULL, NULL);
  edit_staff_and_voice_properties (TRUE);
}


static void
edit_staff_and_voice_properties (gboolean show_staff)
{
  GtkWidget *editstaffwin = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  TheEditorWidget = editstaffwin;
  gint window_height = 800;
  GdkRGBA color;
#if GTK_MAJOR_VERSION == 2  
  get_color (&color, 1.0, 1.0, 1.0, 1.0);       //.red = color.green = color.blue = color.alpha = 1.0;
  gtk_widget_override_background_color (editstaffwin, GTK_STATE_FLAG_NORMAL, &color);
  
#else 
{
GtkCssProvider *gcp;
GtkStyleContext *gsc;
gsc = gtk_widget_get_style_context(GTK_WIDGET (editstaffwin));
gchar *str = "GtkWindow {background-color: #ffffff;}"; 
gcp= gtk_css_provider_new();
gtk_css_provider_load_from_data(gcp, str, -1, 0);
gtk_style_context_add_provider(gsc, GTK_STYLE_PROVIDER(gcp), 
    GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
}
#endif  
  
  gtk_window_set_modal (GTK_WINDOW (editstaffwin), TRUE);
  gtk_window_set_title (GTK_WINDOW (editstaffwin), _("Staff and Voice Properties Editor"));
  gtk_window_set_transient_for (GTK_WINDOW (editstaffwin), GTK_WINDOW (Denemo.window));
  gtk_window_set_keep_above (GTK_WINDOW (editstaffwin), TRUE);
  gtk_window_set_default_size (GTK_WINDOW (editstaffwin), 800, window_height);


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
  gtk_expander_set_expanded (GTK_EXPANDER (expander), TRUE);
  gtk_widget_set_sensitive (expander, TRUE);
  gtk_container_set_border_width (GTK_CONTAINER (expander), 10);

#if GTK_MAJOR_VERSION == 2
  get_color (&color, 0.1, 0.1, 0.8, 1.0);
  gtk_widget_override_color (expander, GTK_STATE_FLAG_NORMAL, &color);
#else
  set_foreground_color (expander, "rgb(25, 25, 200)");//blue
#endif 
  GtkWidget *frame = gtk_frame_new (NULL);
  //gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_IN);



  gtk_paned_add1 (GTK_PANED (pane), frame);

  gtk_container_add (GTK_CONTAINER (frame), expander);

  GtkWidget *scrolled_window = gtk_scrolled_window_new (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0), gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
#if GTK_MAJOR_VERSION==3
  gtk_widget_set_vexpand (scrolled_window, TRUE);
#endif
  gtk_container_add (GTK_CONTAINER (expander), scrolled_window);

  GtkWidget *inner_box = gtk_vbox_new (FALSE, 0);
          
#if GTK_MAJOR_VERSION==2       
          gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (scrolled_window), inner_box);
#else          
          gtk_container_add (GTK_CONTAINER(scrolled_window), inner_box);
#endif    
  GtkWidget *inner_hbox;
  inner_hbox = gtk_hbox_new (FALSE, 0);
  gtk_box_pack_start (GTK_BOX (inner_box), inner_hbox, FALSE, TRUE, 0);
  button = gtk_button_new_with_label (_("Staff Above"));
  g_signal_connect_swapped (button, "clicked", G_CALLBACK (staff_above), editstaffwin);
  gtk_box_pack_start (GTK_BOX (inner_hbox), button, FALSE, TRUE, 0);

  if (Denemo.project->movement->currentstaffnum == 1)
    gtk_widget_set_sensitive (button, FALSE);

  button = gtk_button_new_with_label (_("Staff Below"));
  g_signal_connect_swapped (button, "clicked", G_CALLBACK (staff_below), editstaffwin);
  gtk_box_pack_start (GTK_BOX (inner_hbox), button, FALSE, TRUE, 0);
  if (g_list_length (Denemo.project->movement->thescore) == Denemo.project->movement->currentstaffnum)
    gtk_widget_set_sensitive (button, FALSE);
  DenemoStaff *thestaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (thestaff->voicecontrol != DENEMO_PRIMARY)
    {
      GtkWidget *label = gtk_label_new ("");
      gtk_label_set_markup (GTK_LABEL (label), _("<b>This voice will be typeset on the staff above</b>"));
      gtk_label_set_line_wrap (GTK_LABEL (label), TRUE);
      gtk_box_pack_start (GTK_BOX (inner_box), label, FALSE, TRUE, 0);
    }
  inner_hbox = gtk_hbox_new (FALSE, 0);
  gtk_box_pack_start (GTK_BOX (inner_box), inner_hbox, FALSE, TRUE, 0);

  button = gtk_button_new_with_label (_("Edit Built-in Staff Properties"));
  g_signal_connect_swapped (button, "clicked", G_CALLBACK (change_staff_properties), editstaffwin);
  gtk_box_pack_start (GTK_BOX (inner_hbox), button, FALSE, TRUE, 0);


  const gchar *theclef = get_clef_name (thestaff->clef.type);
  gchar *text = g_strconcat (_("Clef: "), theclef, NULL);
  button = gtk_button_new_with_label (text);
  g_signal_connect_swapped (button, "clicked", G_CALLBACK (clef_change_initial_cb), editstaffwin);
  gtk_box_pack_start (GTK_BOX (inner_hbox), button, FALSE, TRUE, 0);
  g_free (text);

  text = g_strdup_printf (_("Time: %d/%d"), thestaff->timesig.time1, thestaff->timesig.time2);
  button = gtk_button_new_with_label (text);
  g_signal_connect_swapped (button, "clicked", G_CALLBACK (timesig_change_initial_cb), editstaffwin);
  gtk_box_pack_start (GTK_BOX (inner_hbox), button, FALSE, TRUE, 0);
  g_free (text);

  text = g_strdup_printf (_("Key: %s"), get_lilypond_for_keysig (&(thestaff->keysig)));
  button = gtk_button_new_with_label (text);
  g_signal_connect (button, "clicked", G_CALLBACK (keysig_change_info), NULL);
  gtk_box_pack_start (GTK_BOX (inner_hbox), button, FALSE, TRUE, 0);
  g_free (text);


  place_buttons_for_directives ((GList **) & thestaff->staff_directives, inner_box, DIRECTIVE_STAFF, "staff");
  if (thestaff->keysig.directives)
    {
      //GtkWidget *label = gtk_label_new (_("Key Signature Directives"));
     //gtk_box_pack_start (GTK_BOX (inner_box), label, FALSE, TRUE, 0);
      place_buttons_for_directives ((GList **) & (thestaff->keysig.directives), inner_box, DIRECTIVE_KEYSIG, "keysig");
    }
  if (thestaff->timesig.directives)
    {
      //GtkWidget *label = gtk_label_new (_("Time Signature Directives"));
      //gtk_box_pack_start (GTK_BOX (inner_box), label, FALSE, TRUE, 0);
      place_buttons_for_directives ((GList **) & (thestaff->timesig.directives), inner_box, DIRECTIVE_TIMESIG, "timesig");
    }
  if (thestaff->clef.directives)
    {
      //GtkWidget *label = gtk_label_new (_("Clef Directives"));
      //gtk_box_pack_start (GTK_BOX (inner_box), label, FALSE, TRUE, 0);
      place_buttons_for_directives ((GList **) & (thestaff->clef.directives), inner_box, DIRECTIVE_CLEF, "clef");
    }


  expander = gtk_expander_new (_("Voice Properties"));

  gtk_expander_set_expanded (GTK_EXPANDER (expander), TRUE);
  gtk_widget_set_sensitive (expander, TRUE);
  gtk_container_set_border_width (GTK_CONTAINER (expander), 10);

#if GTK_MAJOR_VERSION == 2
  get_color (&color, 0.1, 0.1, 0.8, 1.0);
  gtk_widget_override_color (expander, GTK_STATE_FLAG_NORMAL, &color);
#else
  set_foreground_color (expander, "rgb(25, 25, 200)");//blue
#endif 

  frame = gtk_frame_new (NULL);
  //gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_IN);

  gtk_paned_add2 (GTK_PANED (pane), frame);
  gtk_container_add (GTK_CONTAINER (frame), expander);

  scrolled_window = gtk_scrolled_window_new (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0), gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
#if GTK_MAJOR_VERSION==3
  gtk_widget_set_vexpand (scrolled_window, TRUE);
#endif
  gtk_container_add (GTK_CONTAINER (expander), scrolled_window);
  inner_box = gtk_vbox_new (FALSE, 0);
          
#if GTK_MAJOR_VERSION==2       
          gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (scrolled_window), inner_box);
#else          
          gtk_container_add (GTK_CONTAINER(scrolled_window), inner_box);
#endif    
  GtkWidget *hbox = gtk_hbox_new (FALSE, 0);
  gtk_box_pack_start (GTK_BOX (inner_box), hbox, FALSE, TRUE, 0);




  place_buttons_for_directives ((GList **) & thestaff->voice_directives, inner_box, DIRECTIVE_VOICE, "voice");


  gtk_paned_set_position (GTK_PANED (pane), window_height / 2);



  if (g_list_length (gtk_container_get_children (GTK_CONTAINER (vbox))) == 1)
    {                           //just the close button
      warningdialog ("No properties have been set on the current score.");
      gtk_widget_destroy (editstaffwin);
    }
  else
    {
      gtk_widget_show_all (editstaffwin);
      gtk_window_present (GTK_WINDOW (editstaffwin));
      gdk_window_set_cursor (gtk_widget_get_window (editstaffwin), Denemo.GDK_LEFT_PTR);
      gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), Denemo.GDK_X_CURSOR);
      if (ObjectInfo)
        gdk_window_set_cursor (gtk_widget_get_window (ObjectInfo), Denemo.GDK_X_CURSOR);
      if (Denemo.printarea && gtk_widget_get_window (Denemo.printarea))
        gdk_window_set_cursor (gtk_widget_get_window (Denemo.printarea), Denemo.GDK_X_CURSOR);

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
  GList *g;
  GList *start = directive->layouts;
  directive->layouts = NULL;
  for (g = start; g; g = g->next)
    directive->layouts = g_list_append (directive->layouts, g->data);
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
  g_list_free (directive->layouts);
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
  if (ret)
    {
      if (orig->lilypond)
        ret->lilypond = g_strdup (orig->lilypond);
      else
        ret->lilypond = NULL;

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
  signal_structural_change (Denemo.project);
  curstaff->keysig.number = tokey;
  curstaff->keysig.isminor = type;

  initkeyaccs (curstaff->keysig.accs, tokey);
  curstaff->leftmost_keysig = &curstaff->keysig;
  staff_show_which_accidentals (curstaff);
  adjust_tonal_center (curstaff->keysig.accs);
  displayhelper (Denemo.project);
  score_status (Denemo.project, TRUE);
}
