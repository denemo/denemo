/* object.cpp
 * functions that do operations to mudela objects
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
static void
call_edit_on_action (GtkWidget *button)
{
   DenemoScriptParam param;
   GtkAction *action = (GtkAction*)g_object_get_data (G_OBJECT (button), "action");
   param.string = g_string_new ("edit");
    g_debug ("Script can look for params \"edit\" - a string to catch this");
    activate_script (action, &param);
    g_string_free (param.string, TRUE);  
   gtk_widget_destroy (gtk_widget_get_toplevel (button));
   edit_object();   
}

static void execute_editscript (GtkWidget *button, gchar *filename)
{
 GError *error = (GError *) execute_script_file (filename);
 if (error)
    g_warning ("%s", error->message);
 gtk_widget_destroy (gtk_widget_get_toplevel (button));
 edit_object();
}
typedef enum {
  EDIT_CHORD,
  EDIT_NOTE,
  EDIT_TUPLET_START,
  EDIT_TUPLET_END,
  EDIT_CLEF,
  EDIT_KEY,
  EDIT_TIMESIG,
  EDIT_STEMDIR,
  EDIT_STANDALONE,
  
} EditObjectType;

static void general_edit_popup (GtkWidget *button, EditObjectType type)
{
    popup_menu ("/NoteEditPopup");
    gtk_widget_destroy (gtk_widget_get_parent (gtk_widget_get_parent (button)));
}

static void
place_directives (GtkWidget *vbox, GList **pdirectives, EditObjectType type)
{
    GList *directives = *pdirectives;
    GdkRGBA color;
    color.red = color.green = color.blue = color.alpha = 1.0;
    gtk_widget_override_background_color (vbox, 0, &color);
    for (; directives;directives = directives->next)
        {
            DenemoDirective *directive = directives->data;
            const gchar *label = get_label_for_command (directive->tag->str);
            GtkAction *action = lookup_action_from_name (directive->tag->str);
            gchar *name = label?(gchar*)label:directive->tag->str;

            gchar *filename = get_editscript_filename (directive->tag->str);

            if (!label)
                label = directive->tag->str;
            GtkWidget *expander = gtk_expander_new (label);
            gtk_expander_set_expanded (GTK_EXPANDER(expander), TRUE);
            gtk_widget_set_sensitive (expander, TRUE);
            gtk_container_set_border_width (GTK_CONTAINER (expander),10);
            gtk_box_pack_start (GTK_BOX (vbox), expander, FALSE, TRUE, 0);
            GtkWidget *inner_box = gtk_vbox_new (FALSE, 8);
            gtk_container_add (GTK_CONTAINER (expander), inner_box);

            if (filename)
                {
                gchar *thelabel = g_strconcat (_("Edit "), name, NULL);
                GtkWidget *button = gtk_button_new_with_label (thelabel);
                g_signal_connect (button, "clicked", G_CALLBACK (execute_editscript), filename);
                g_signal_connect_swapped (button, "destroy", G_CALLBACK (g_free), filename);
                gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);   
                g_free(thelabel);
                }  
            else if (action)
                {
                gchar *thelabel = g_strconcat (_("Re-run command "), name, NULL);
                GtkWidget *button = gtk_button_new_with_label (thelabel);
                g_object_set_data (G_OBJECT(button), "action", (gpointer)action);
                g_signal_connect (button, "clicked", G_CALLBACK (call_edit_on_action), NULL);
                gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);                       
                g_free(thelabel);  
                }

            GtkWidget *button = gtk_button_new_with_label (_("Advanced"));
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
            g_object_set_data (G_OBJECT(button), "directives", (gpointer)pdirectives);
            g_object_set_data (G_OBJECT(button), "directive", (gpointer)directive);
            g_signal_connect (button, "clicked", G_CALLBACK (advanced_edit_type_directive), func);
            gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);
        }
}
static void
place_chord_attributes (GtkWidget *vbox, chord *thechord)
{
    GtkWidget *expander = gtk_expander_new (_("Built-in Chord Attributes"));//gtk_expander_new
    gtk_expander_set_expanded (GTK_EXPANDER(expander), TRUE);
    gtk_widget_set_sensitive (expander, TRUE);
    gtk_container_set_border_width (GTK_CONTAINER (expander),10);
    gtk_box_pack_start (GTK_BOX (vbox), expander, FALSE, TRUE, 0);
    GtkWidget *inner_box = gtk_vbox_new (FALSE, 8);
    gtk_container_add (GTK_CONTAINER (expander), inner_box);
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
    gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), gdk_cursor_new (GDK_LEFT_PTR)); 

}
// edit the specific object at the cursor
void
edit_object (void)
{
    DenemoObject *curObj = get_object ();
    if (curObj == NULL)
    {
      warningmessage (_("No object here to edit"));
      return;
    }
    GtkWidget *editwin = gtk_window_new (GTK_WINDOW_TOPLEVEL); 
    GdkRGBA color;
    color.red = color.green = color.blue = color.alpha = 1.0;
    gtk_widget_override_background_color (editwin, 0, &color);
    gtk_window_set_modal (GTK_WINDOW (editwin), TRUE);
    gtk_window_set_title (GTK_WINDOW (editwin), _("Denemo Object Editor"));
    gtk_window_set_keep_above (GTK_WINDOW (editwin), TRUE);
    GtkWidget *vbox = gtk_vbox_new (FALSE, 8);
    gtk_container_add (GTK_CONTAINER (editwin), vbox);
    GtkWidget *close_button = gtk_button_new_with_label (_("Close"));
    g_signal_connect_swapped (close_button, "clicked", G_CALLBACK (update_and_close), editwin);
    gtk_box_pack_start (GTK_BOX (vbox), close_button, FALSE, TRUE, 0);
    switch (curObj->type)
    {
        case CHORD:
          {
            chord *thechord = ((chord *) curObj->object);
            if (thechord->notes)
              {
                place_chord_attributes (vbox, thechord);
                note *thenote = findnote (curObj, Denemo.project->movement->cursor_y);
                if (thenote && Denemo.project->movement->cursor_y==thenote->mid_c_offset)
                  {
                    GString *text = g_string_new ("");
                    g_string_append_printf (text, _("Attached to Note |%s|"),
                                            mid_c_offsettolily (thenote->mid_c_offset, thenote->enshift));
                    if (thenote->directives)
                      {
                        GtkWidget *frame = gtk_frame_new (text->str);
                        gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
                        gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
                        GtkWidget *inner_box = gtk_vbox_new (FALSE, 8);
                        gtk_container_add (GTK_CONTAINER (frame), inner_box);
                        place_directives (inner_box, &thenote->directives, EDIT_NOTE);
                      }
                  }
                if ((thechord->notes->next) && curObj->isinvisible)
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
             
            if (thechord->directives) 
                {
                GtkWidget *frame = gtk_frame_new (_("Attached to the chord:"));
                gtk_frame_set_shadow_type ((GtkFrame *) frame, GTK_SHADOW_IN);
                gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
                GtkWidget *inner_box = gtk_vbox_new (FALSE, 8);
                gtk_container_add (GTK_CONTAINER (frame), inner_box);
                place_directives (inner_box, &thechord->directives, EDIT_CHORD);
                }
            {
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
                GtkWidget *frame = gtk_frame_new (_("Attached to the stemmingchange object:"));
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
            gchar *filename = get_editscript_filename (directive->tag->str);
             if (filename)
                {
                gchar *thelabel = g_strconcat ( _("Edit "), name, NULL);
                GtkWidget *button = gtk_button_new_with_label (thelabel);
                g_signal_connect (button, "clicked", G_CALLBACK (execute_editscript), filename);
                g_signal_connect_swapped (button, "destroy", G_CALLBACK (g_free), filename);
                gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, TRUE, 0);   
                g_free(thelabel);
                }  
            else if (action)
                {
                gchar *thelabel = g_strconcat ( _("Re-run command "), name, NULL);
                GtkWidget *button = gtk_button_new_with_label (thelabel);
                g_object_set_data (G_OBJECT(button), "action", (gpointer)action);
                g_signal_connect (button, "clicked", G_CALLBACK (call_edit_on_action), NULL);
                gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, TRUE, 0);                       
                g_free(thelabel);  
                }

            GtkWidget *button = gtk_button_new_with_label (_("Advanced (low-level) edit"));
            g_object_set_data (G_OBJECT(button), "directives", NULL);
            g_object_set_data (G_OBJECT(button), "directive", (gpointer)directive);
            g_signal_connect (button, "clicked", G_CALLBACK (advanced_edit_type_directive), text_edit_standalone_directive);
            gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, TRUE, 0);
            }
            break;
            
        default:
            g_critical ("Type not done");
            break;
    }
  if(g_list_length ( gtk_container_get_children (vbox)) == 1)
    {//just the close button
        warningdialog ("Nothing editable on this object");
        gtk_widget_destroy (editwin);
    }
    else
    {
      gtk_widget_show_all (editwin);
      gtk_window_present (GTK_WINDOW (editwin));
      gdk_window_set_cursor (gtk_widget_get_window (editwin), gdk_cursor_new (GDK_HAND2)); 
      gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), gdk_cursor_new (GDK_X_CURSOR));
    }
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
