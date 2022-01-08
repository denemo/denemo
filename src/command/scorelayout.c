//      scorelayout.c
//
//      Copyright 2012 Richard Shann
//
//      This program is free software; you can redistribute it and/or modify
//      it under the terms of the GNU General Public License as published by
//      the Free Software Foundation; either version 3 of the License, or
//      (at your option) any later version.
//
//      This program is distributed in the hope that it will be useful,
//      but WITHOUT ANY WARRANTY; without even the implied warranty of
//      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//      GNU General Public License for more details.
//
//      You should have received a copy of the GNU General Public License
//      along with this program; if not, write to the Free Software
//      Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
//      MA 02110-1301, USA.

#include <string.h>
#include "command/scorelayout.h"
#include "export/exportlilypond.h"
#include "core/utils.h"
#include "export/print.h"
#include "printview/printview.h"
#include "command/lilydirectives.h"
#include "command/score.h"
#include "command/processstaffname.h"
#include "core/view.h"
#include "core/menusystem.h"



#define LILYPOND_TEXT_EDITOR _("LilyPond text editor")
#define DEFAULT_SCORE_LAYOUT _("Default Score Layout")
static void set_notebook_page (GtkWidget * w);
static void prefix_edit_callback (GtkWidget * widget, GtkWidget * frame);
static void create_element (GtkWidget * vbox, GtkWidget * widget, gchar * lilypond);
static void create_standard_scoreblock (DenemoScoreblock ** psb, gint movement, gchar * partname);
static void recreate_standard_scoreblock (DenemoScoreblock ** psb);
static DenemoScoreblock *get_standard_scoreblock (GtkWidget * widget);
static GtkWidget *get_options_button (DenemoScoreblock * sb, gboolean custom);
static void install_duplicate_movement_callback (DenemoScoreblock * sb);
static void reorder_movement_callback (DenemoScoreblock * psb);
static gboolean edit_lilypond_prefix (GtkWidget * widget, gchar * oldval, gchar * newval);
static void reload_scorewide_block (GtkWidget * frame);
static gint layout_sync;

// Reverses (reflects) bits in a 32-bit word.
static guint32
bit_reverse (guint32 x)
{
  x = ((x & 0x55555555) << 1) | ((x >> 1) & 0x55555555);
  x = ((x & 0x33333333) << 2) | ((x >> 2) & 0x33333333);
  x = ((x & 0x0F0F0F0F) << 4) | ((x >> 4) & 0x0F0F0F0F);
  x = (x << 24) | ((x & 0xFF00) << 8) | ((x >> 8) & 0xFF00) | (x >> 24);
  return x;
}

/* This is the basic CRC algorithm with no optimizations. It follows the
logic circuit as closely as possible. */
//returns a 32 bit crc for the passed string
static guint32
crc32 (guchar * message)
{
  int i, j;
  guint32 byte, crc;
  i = 0;
  crc = 0xFFFFFFFF;
  while (message[i] != 0)
    {
      byte = message[i];        // Get next byte.
      byte = bit_reverse (byte);        // 32-bit reversal.
      for (j = 0; j <= 7; j++)
        {                       // Do eight times.
          if ((int) (crc ^ byte) < 0)
            crc = (crc << 1) ^ 0x04C11DB7;
          else
            crc = crc << 1;
          byte = byte << 1;     // Ready next msg bit.
        }
      i = i + 1;
    }
  return bit_reverse (~crc);
}

static guint
get_location (guint movementnum, guint voicecount)
{
  return (movementnum << 16) | voicecount;
}

static void
navigate_to_location (GtkWidget * w, guint location)
{
  guint movementnum = location >> 16;
  guint staffnum = location & 0xFFFF;
  goto_movement_staff_obj (NULL, movementnum, staffnum, 1, 0, 0);
}

static void
popup_staff_groups_menu (GtkWidget * button)
{
  GtkWidget *menuitem = denemo_menusystem_get_widget ("StaffGroupings");
  if (get_standard_scoreblock (button))
    {
      if (menuitem)
        gtk_menu_popup (GTK_MENU (gtk_menu_item_get_submenu (GTK_MENU_ITEM (menuitem))), NULL, NULL, NULL, NULL, 0, GDK_CURRENT_TIME);
      else
        g_warning ("No such menu path");
    }
  else
    warningdialog (_("This button is for changing the score itself, it will not affect this custom layout"));
}

static void
staff_groups_menu (GtkWidget * w, GdkEvent * event, guint location)
{
  navigate_to_location (NULL, location);
  popup_staff_groups_menu (w);
}

//callback on destroying widgets that are on the staff_list of a scoreblock.
static gboolean
remove_from_staff_list (GtkWidget * widget, GList ** g)
{
  *g = g_list_remove (*g, widget);
  return TRUE;                  //that is go on to destroy the widget
}

//free the passed DenemoScoreblock structure for re-use or disposal
static void
free_scoreblock (DenemoScoreblock * sb)
{
  if (sb->widget)
    gtk_widget_destroy (sb->widget);    //FIXME free lilypond attached to widgets
  sb->widget = 0;
  if (sb->lilypond)
    g_string_free ((GString *) (sb->lilypond), TRUE);
  g_free (sb->instrumentation);
  sb->instrumentation = NULL;
  sb->lilypond = NULL;
}

void
free_standard_scoreblocks (DenemoProject * gui)
{
  if (gui->standard_scoreblocks)
    {
      GList *g;
      for (g = gui->standard_scoreblocks; g; g = g->next)
        {
          DenemoScoreblock *sb = ((DenemoScoreblock *) g->data);
          free_scoreblock (sb);
        }
      g_list_free (gui->standard_scoreblocks);
      gui->standard_scoreblocks = NULL;
    }
}

void
free_custom_scoreblocks (DenemoProject * gui)
{

  if (gui->custom_scoreblocks)
    {
      GList *g;
      for (g = gui->custom_scoreblocks; g; g = g->next)
        {
          DenemoScoreblock *sb = ((DenemoScoreblock *) g->data);
          free_scoreblock (sb);
        }
      g_list_free (gui->custom_scoreblocks);
      gui->custom_scoreblocks = NULL;
    }
}
void
free_scoreblocks (DenemoProject * gui)
{
  free_standard_scoreblocks (gui);
  free_custom_scoreblocks (gui);
}

static gboolean
is_in_standard_scoreblock (DenemoScoreblock * sb)
{
  return (g_list_find (Denemo.project->standard_scoreblocks, sb) != NULL);
}


static gboolean
free_prefix_and_postfix (GtkWidget * widget)
{
  // glib 2.28 and greater can use:
  //g_list_free_full(g_object_get_data(G_OBJECT(widget), "prefix"), (GDestroyNotify)g_free);
  //g_list_free_full(g_object_get_data(G_OBJECT(widget), "postfix"), (GDestroyNotify)g_free);
  GList *g = g_object_get_data (G_OBJECT (widget), "prefix");
  g_list_foreach (g, (GFunc) (g_free), NULL);
  g_list_free (g);
  g = g_object_get_data (G_OBJECT (widget), "postfix");
  g_list_foreach (g, (GFunc) (g_free), NULL);
  g_list_free (g);
  return TRUE;                  //and destroy widget
}

void
break_and_carry_on (void)
{
}

// attaches the two lilypond strings to the prefix and postfix lists of widget
// prefix is created in the reverse order to postfix so they can nest
// the destroy widget is arranged to free the lists
static void
add_lilypond (GtkWidget * w, gchar * prefix, gchar * postfix)
{
  if (g_object_get_data (G_OBJECT (w), "prefix") == NULL && g_object_get_data (G_OBJECT (w), "postfix") == NULL)
    g_signal_connect (G_OBJECT (w), "destroy", G_CALLBACK (free_prefix_and_postfix), NULL);
  if (prefix)
    g_object_set_data (G_OBJECT (w), "prefix", (gpointer) g_list_append (g_object_get_data (G_OBJECT (w), "prefix"), prefix));
  if (postfix)
    g_object_set_data (G_OBJECT (w), "postfix", (gpointer) g_list_prepend (g_object_get_data (G_OBJECT (w), "postfix"), postfix));
}


#define get_voicetag get_voicename
//they don't need to be different.

static gchar *
get_voicename (gint movementnum, gint voice_count)
{
  GString *str = g_string_new ("");
  GString *name = g_string_new ("");
  g_string_printf (str, "Mvmnt%d Voice%d", movementnum, voice_count);
  set_lily_name (str, name);
  g_string_free (str, TRUE);
  return g_string_free (name, FALSE);
}


static gchar *
get_versename (gint movementnum, gint voice_count, gint versenum)
{
  GString *str = g_string_new ("");
  GString *name = g_string_new ("");
  g_string_printf (str, "Mvmnt%d Voice%d Verse%d", movementnum, voice_count, versenum);
  set_lily_name (str, name);
  g_string_free (str, TRUE);
  return g_string_free (name, FALSE);
}

//Change the name of the scoreblock to user given value
static gboolean
name_scoreblock (DenemoScoreblock * sb, gchar * name)
{
  gchar *value;
  if (name == NULL)
    value = string_dialog_entry (Denemo.project, _("New Score Layout"), _("Give a name for this new score layout"), _("Custom Layout"));
  else
    value = name;
  if (value)
    {
      sb->name = g_strdup (value);
      if (!Denemo.non_interactive) 
        gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (get_score_layout_notebook (Denemo.project)), sb->widget, value);
      //FIXME if name==NULL g_free(value) I think.
      return TRUE;
    }
  else
    return FALSE;
}

//gets the toplevel standard DenemoScoreblock which contains the passed in widget, or NULL if not in the standard scoreblocks
static DenemoScoreblock *
get_standard_scoreblock (GtkWidget * widget)
{
  GList *g;
  for (g = Denemo.project->standard_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = (DenemoScoreblock *) g->data;
      if (sb->widget && gtk_widget_is_ancestor (widget, sb->widget))
        return sb;
    }
  return NULL;
}

//gets the toplevel custom DenemoScoreblock which contains the passed in widget, or NULL if not in the custom scoreblocks
/* UNUSED
static DenemoScoreblock *
get_custom_scoreblock (GtkWidget * widget)
{
  GList *g;
  for (g = Denemo.project->custom_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = (DenemoScoreblock *) g->data;
      if (sb->widget && gtk_widget_is_ancestor (widget, sb->widget))
        return sb;
    }
  return NULL;
}
*/
static gboolean
is_lilypond_text_layout (DenemoScoreblock * sb)
{
  return sb->text_only;
//return sb->widget && GTK_IS_FRAME(sb->widget) && gtk_frame_get_label(GTK_FRAME(sb->widget)) && !strcmp(gtk_frame_get_label(GTK_FRAME(sb->widget)), LILYPOND_TEXT_EDITOR);
}

static void
open_lilypond_window_callback (void)
{
  if (Denemo.textwindow && !gtk_widget_get_visible (Denemo.textwindow))
    set_toggle ("ToggleLilyText", TRUE);
}

static void
convert_to_lilypond_callback (GtkWidget * widget, DenemoScoreblock * sb)
{
  refresh_lilypond (sb);
  DenemoScoreblock *newsb = get_scoreblock_for_lilypond (sb->lilypond->str);
  Denemo.project->custom_scoreblocks = g_list_remove (Denemo.project->custom_scoreblocks, sb);
  Denemo.project->standard_scoreblocks = g_list_remove (Denemo.project->standard_scoreblocks, sb);
  GtkWidget *notebook = get_score_layout_notebook (Denemo.project);
  GtkWidget *label = gtk_label_new (sb->name);
  if (sb->widget)
    gtk_widget_destroy (sb->widget);
  gtk_widget_show_all (newsb->widget);
  gtk_notebook_prepend_page (GTK_NOTEBOOK (notebook), newsb->widget, label);
  gtk_notebook_set_current_page (GTK_NOTEBOOK (notebook), 0);
  Denemo.project->custom_scoreblocks = g_list_append (Denemo.project->custom_scoreblocks, newsb);
  Denemo.project->layout_id = 0;
}

static void
delete_custom_scoreblock_callback (GtkWidget * dummy, DenemoScoreblock * sb)
{
  Denemo.project->custom_scoreblocks = g_list_remove (Denemo.project->custom_scoreblocks, sb);
  gtk_widget_destroy (sb->widget);
  if (Denemo.project->standard_scoreblocks == NULL && Denemo.project->custom_scoreblocks == NULL)
    create_default_scoreblock ();
  score_status (Denemo.project, TRUE);
}

static void
delete_standard_scoreblock_callback (GtkWidget * widget, DenemoScoreblock * sb)
{
  Denemo.project->standard_scoreblocks = g_list_remove (Denemo.project->standard_scoreblocks, sb);
  gtk_widget_destroy (sb->widget);
  if (Denemo.project->standard_scoreblocks == NULL && Denemo.project->custom_scoreblocks == NULL)
    create_default_scoreblock ();
  score_status (Denemo.project, TRUE);
}

static void
recreate_standard_scoreblock_callback (GtkWidget * widget, DenemoScoreblock * psb)
{
  recreate_standard_scoreblock (&psb);
}

static gboolean
customize_scoreblock (DenemoScoreblock * sb, gchar * name)
{
  if (is_lilypond_text_layout (sb))
    {
      DenemoScoreblock *newsb = get_scoreblock_for_lilypond (sb->lilypond->str);
      GtkWidget *notebook = get_score_layout_notebook (Denemo.project);
      name_scoreblock (newsb, name);
      GtkWidget *label = gtk_label_new (newsb->name);
      gtk_widget_show_all (newsb->widget);
      gtk_notebook_prepend_page (GTK_NOTEBOOK (notebook), newsb->widget, label);
      gtk_notebook_set_current_page (GTK_NOTEBOOK (notebook), 0);
      Denemo.project->custom_scoreblocks = g_list_append (Denemo.project->custom_scoreblocks, newsb);
      Denemo.project->layout_id = 0;
      gchar *new_name = g_strdup_printf ("%%%s\n", newsb->name);
      g_string_prepend (newsb->lilypond, new_name);
      g_free (new_name);
      g_string_append_printf (newsb->lilypond, "\n\\header {DenemoLayoutName = \"%s\"}\n", newsb->name);
      score_status (Denemo.project, TRUE);
    }
  else if (name_scoreblock (sb, name))
    {
      g_free (sb->partname);
      sb->partname = NULL;
      sb->movement = 0;
      if (sb->lilypond)
        g_string_free (sb->lilypond, TRUE);
      sb->lilypond = NULL;
      Denemo.project->standard_scoreblocks = g_list_remove (Denemo.project->standard_scoreblocks, sb);
      Denemo.project->custom_scoreblocks = g_list_append (Denemo.project->custom_scoreblocks, sb);
      score_status (Denemo.project, TRUE);
    }
  else
    {
      return FALSE;
    }
  return TRUE;
}

#if 0
//having buttons that affect the score itself is confusing
/* go through the layout deleting score block elements that are marked standard as they are not needed for a custom scoreblock*/
static void
prune_layout (GtkWidget * layout)
{
  if (GTK_IS_CONTAINER (layout))
    {
      GList *g = gtk_container_get_children (GTK_CONTAINER (layout));
      if (g)
        do
          {
            prune_layout (g->data);
          }
        while ((g = g->next));
    }
  if (g_object_get_data (G_OBJECT (layout), "standard"))
    gtk_widget_destroy (layout);
}
#endif
static DenemoScoreblock *
clone_scoreblock (DenemoScoreblock * sb, gchar * name)
{
  gchar *partname = g_strdup (sb->partname);
  //prune_layout (sb->widget);
  GtkWidget *options = get_options_button (sb, TRUE);
  gtk_widget_show_all (options);
  GtkWidget *viewport = gtk_bin_get_child (GTK_BIN (sb->widget));
  GList *children = gtk_container_get_children (GTK_CONTAINER (viewport));
  GtkWidget *vbox = children->data;
  GList *grandchildren = gtk_container_get_children (GTK_CONTAINER (vbox));
  gtk_container_remove (GTK_CONTAINER (vbox), grandchildren->data);
  gtk_box_pack_start (GTK_BOX (vbox), options, FALSE, FALSE, 0);
  gtk_box_reorder_child (GTK_BOX (vbox), options, 0);
  if (customize_scoreblock (sb, name))
    {
#if 0
      DenemoScoreblock *newsb = g_malloc0 (sizeof (DenemoScoreblock));
      create_standard_scoreblock (&newsb, movement, partname);
      Denemo.project->standard_scoreblocks = g_list_prepend (Denemo.project->standard_scoreblocks, newsb);
      Denemo.project->lilysync = G_MAXUINT;
      Denemo.project->layout_id = 0;
      gtk_window_present (GTK_WINDOW (gtk_widget_get_toplevel (GTK_WIDGET (Denemo.project->score_layout))));
      return newsb;
#else
      g_free (partname);
      return sb;
#endif
    }
  else
    {
      g_free (partname);
      return NULL;
    }
}

static void
customize_standard_scoreblock_callback (GtkWidget * widget, DenemoScoreblock * sb)
{
  if (confirm (_("Customize Layout"), _("Replace Standard Layout?")))
    {
      static gboolean warned;
      clone_scoreblock (sb, sb->name);
      if (!warned)
        infodialog (_("This layout will be used in place of the standard one, unless you delete it.\nAny new staffs added to the score will not appear in it unless you edit it."));
      warned = TRUE;
    }
  else
    clone_scoreblock (sb, NULL);
  gtk_window_present (GTK_WINDOW (gtk_widget_get_toplevel (GTK_WIDGET (Denemo.project->score_layout))));
}

static void
duplicate_lilypond_scoreblock_callback (GtkWidget * widget, DenemoScoreblock * sb)
{
  customize_scoreblock (sb, NULL);
}


static GtkWidget *
get_options_button (DenemoScoreblock * sb, gboolean custom)
{
  GtkWidget *frame = gtk_frame_new (_("Actions for this Layout"));
  GtkWidget *hbox = gtk_hbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (frame), hbox);

  GtkWidget *button = gtk_button_new_with_label (_("Typeset"));
  gtk_widget_set_tooltip_text (button, _("Typeset the score using this layout to determine which movements, parts, titles, page breaks etc should be used"));
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
#ifdef USE_EVINCE
  g_signal_connect (button, "clicked", G_CALLBACK (show_print_view), NULL);
#endif
  if (custom)
    {
      if (sb->text_only)
        {
          button = gtk_button_new_with_label (_("Edit LilyPond Text of Layout"));
          gtk_widget_set_tooltip_text (button, _("Opens the LilyPond window for further editing."));
          gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
          g_signal_connect (button, "clicked", G_CALLBACK (open_lilypond_window_callback), sb);
          button = gtk_button_new_with_label (_("Duplicate"));
          gtk_widget_set_tooltip_text (button, _("Create a duplicate of this layout."));
          gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
          g_signal_connect (button, "clicked", G_CALLBACK (duplicate_lilypond_scoreblock_callback), sb);


        }
      else
        {
          button = gtk_button_new_with_label (_("Convert to LilyPond Text"));
          gtk_widget_set_tooltip_text (button, _("Converts this layout to LilyPond text for further editing."));
          gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
          g_signal_connect (button, "clicked", G_CALLBACK (convert_to_lilypond_callback), sb);

        }
      button = gtk_button_new_with_label (_("Delete"));
      gtk_widget_set_tooltip_text (button, _("Discard this customized score layout."));
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
      g_signal_connect (button, "clicked", G_CALLBACK (delete_custom_scoreblock_callback), sb);

      button = gtk_button_new_with_label (_("Create Default Score Layout"));
      gtk_widget_set_tooltip_text (button, _("Creates the Default Score Layout"));
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
      g_signal_connect (button, "clicked", G_CALLBACK (create_default_scoreblock), NULL);

      if (!sb->text_only)
        {
          button = gtk_button_new_with_label (_("Append Current Movement"));
          gtk_widget_set_tooltip_text (button, _("Appends the current movement at the end of this layout. Select the movement you wish to append to the layout in the Denemo Display first. The same movement can be placed multiple times in the layout, with individual edits as needed."));
          gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
          g_signal_connect_swapped (button, "clicked", G_CALLBACK (install_duplicate_movement_callback), sb);

          button = gtk_button_new_with_label (_("Re-order Movement"));
          gtk_widget_set_tooltip_text (button, _("Moves the first expanded movement in this layout to the end.\nThis does not alter the score, just this layout. To re-order the actual movements of the score see the Movements menu."));
          gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
          g_signal_connect_swapped (button, "clicked", G_CALLBACK (reorder_movement_callback), sb);
        }
    }
  else
    {
      button = gtk_button_new_with_label (_("Refresh"));
      gtk_widget_set_tooltip_text (button, _("Re-calculate this layout to incorporate changes made to the score structure."));
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
      g_signal_connect (button, "clicked", G_CALLBACK (recreate_standard_scoreblock_callback), sb);

      button = gtk_button_new_with_label (_("Customize"));
      gtk_widget_set_tooltip_text (button, _("Create a layout from this standard layout that you can then modify."));
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
      g_signal_connect (button, "clicked", G_CALLBACK (customize_standard_scoreblock_callback), sb);

      button = gtk_button_new_with_label (_("Create Default Score Layout"));
      gtk_widget_set_tooltip_text (button, _("Creates the Default Score Layout"));
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
      g_signal_connect (button, "clicked", G_CALLBACK (create_default_scoreblock), NULL);

      button = gtk_button_new_with_label (_("Delete"));
      gtk_widget_set_tooltip_text (button, _("Discard this standard score layout."));
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
      g_signal_connect (button, "clicked", G_CALLBACK (delete_standard_scoreblock_callback), sb);
    }


  return frame;
}



//if widget is in a standard scoreblock, clone the scoreblock converting the standard one into a custom one.
static gboolean
clone_scoreblock_if_needed (GtkWidget * widget)
{
  DenemoScoreblock *default_sb;
  if ((default_sb = get_standard_scoreblock (widget)))
    {
      //the button widget was on the default scoreblock, so we convert this to a custom scoreblock
      return clone_scoreblock (default_sb, NULL) != NULL;
    }
  return TRUE;
}


static gboolean
move_parent (GtkWidget * widget, gboolean up)
{
  if (!clone_scoreblock_if_needed (widget))
    return TRUE;

  GtkWidget *parent = gtk_widget_get_parent (widget);
  GtkWidget *grandparent = gtk_widget_get_parent (parent);
  GList *children = gtk_container_get_children (GTK_CONTAINER (grandparent));
  gint position = g_list_index (children, parent);
  if (up)
    {
      if (position > 0)
        {
          position--;
          gtk_box_reorder_child (GTK_BOX (grandparent), parent, position);
          score_status (Denemo.project, TRUE);
        }
    }
  else
    {
      position++;
      gtk_box_reorder_child (GTK_BOX (grandparent), parent, position);
      score_status (Denemo.project, TRUE);
    }
  return TRUE;
}


static gboolean
move_grandparent (GtkWidget * widget, gboolean up)
{
  if (!clone_scoreblock_if_needed (widget))
    return TRUE;

  GtkWidget *parent = gtk_widget_get_parent (gtk_widget_get_parent (widget));
  GtkWidget *grandparent = gtk_widget_get_parent (parent);
  GList *children = gtk_container_get_children (GTK_CONTAINER (grandparent));
  gint position = g_list_index (children, parent);
  if (up)
    {
      if (position > 0)
        {
          position--;
          gtk_box_reorder_child (GTK_BOX (grandparent), parent, position);
          score_status (Denemo.project, TRUE);
        }
    }
  else
    {
      position++;
      gtk_box_reorder_child (GTK_BOX (grandparent), parent, position);
      score_status (Denemo.project, TRUE);
    }
  return TRUE;
}


static gboolean
remove_element (GtkWidget * widget)
{
  if (!clone_scoreblock_if_needed (widget))
    return TRUE;

  GtkWidget *parent = gtk_widget_get_parent (widget);
  gtk_widget_destroy (parent);
  score_status (Denemo.project, TRUE);
  return TRUE;
}

static gboolean
remove_parent_element (GtkWidget * widget)
{
  if (!clone_scoreblock_if_needed (widget))
    return TRUE;
  GtkWidget *parent = gtk_widget_get_parent (gtk_widget_get_parent (widget));
  gtk_widget_destroy (parent);
  score_status (Denemo.project, TRUE);
  return TRUE;
}

static gboolean
remove_lyric_element (GtkWidget * widget, gchar * context_text)
{
  if (!clone_scoreblock_if_needed (widget))
    return TRUE;
  GtkWidget *parent = gtk_widget_get_parent (widget);
  GtkWidget *grandparent = gtk_widget_get_parent (parent);
  gtk_widget_destroy (parent);
  GList *postfixes = (GList *) g_object_get_data (G_OBJECT (grandparent), "postfix");
  g_object_set_data (G_OBJECT (grandparent), "postfix", (gpointer) g_list_remove (postfixes, context_text));
  //free context_text here
  score_status (Denemo.project, TRUE);
  return TRUE;
}


static void
substitute_voice_name (GtkWidget * button, GtkWidget * frame)
{
  if (!clone_scoreblock_if_needed (frame))
    return;
  GList *m;
  gint mvmnt = 1, staffnum = Denemo.project->movement->currentstaffnum;
  if (Denemo.project->movements)
    mvmnt = 1 + g_list_index (Denemo.project->movements, Denemo.project->movement);

  gchar *thename = g_strdup_printf ("\\Mvmnt%dVoice%d", mvmnt, staffnum);
  GString *initial = g_string_new (thename);
  g_free (thename);
  GString *lily = g_string_new ("");
  set_lily_name (initial, lily);
  GList *g = g_object_get_data (G_OBJECT (frame), "prefix");
  if (g)
    {
      gchar *oldlily = (gchar *) g->data;
      edit_lilypond_prefix (frame, oldlily, g_strdup (lily->str));
      score_status (Denemo.project, TRUE);
      gchar *text = g_strdup_printf (_("The music for this staff has been replaced by the music from the current staff, i.e. staff where the cursor is, Movement %d, Staff %d."), mvmnt, staffnum);
      infodialog (text);
    }
  g_string_free (lily, TRUE);
}


static GtkWidget *
create_voice_widget (DenemoStaff * staff, gchar * voicename, guint location)
{
  gchar *name = staff->denemo_name->str;
  GtkWidget *ret = gtk_hbox_new (FALSE, 8);
  GtkWidget *w = gtk_button_new_with_label (_("Edit"));
  gtk_widget_set_tooltip_text (w, _("Edit the voice directives for this layout"));
  gtk_box_pack_start (GTK_BOX (ret), w, FALSE, TRUE, 0);
  g_signal_connect (w, "clicked", G_CALLBACK (prefix_edit_callback), ret);

  w = gtk_button_new_with_label ("X");
  gtk_widget_set_tooltip_text (w, _("Delete this voice from the score layout\nNote that if it is the first voice the clef time and keysignatures will be deleted too."));
  g_signal_connect (w, "clicked", G_CALLBACK (remove_element), NULL);
  gtk_box_pack_end (GTK_BOX (ret), w, FALSE, TRUE, 0);

  gchar *text = g_strdup_printf (" \\%s", voicename);
  gchar *label_text = _("Initial Signatures");
  if (staff->voicecontrol == DENEMO_PRIMARY)
    {
      GtkWidget *expander = gtk_expander_new (label_text);
      gtk_widget_set_tooltip_text (expander, _("Click here to view and edit the clef, key and time signatures of this staff"));
      gtk_box_pack_start (GTK_BOX (ret), expander, FALSE, TRUE, 0);
      GtkWidget *hbox = gtk_hbox_new (FALSE, 8);
      gtk_container_add (GTK_CONTAINER (expander), hbox);

      GtkWidget *button = gtk_button_new_with_label (_("Clef"));
      gtk_widget_set_tooltip_text (button, _("Edit the LilyPond definition of the clef. The editing affects only this layout."));
      g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (prefix_edit_callback), button);
      add_lilypond (button, get_lilypond_for_clef (&staff->clef), NULL);
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

      button = gtk_button_new_with_label (_("Key"));
      gtk_widget_set_tooltip_text (button, _("Edit the LilyPond definition of the key signature. The editing affects only this layout."));
      g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (prefix_edit_callback), button);
      add_lilypond (button, get_lilypond_for_keysig (&staff->keysig), NULL);
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

      button = gtk_button_new_with_label (_("Time"));
      gtk_widget_set_tooltip_text (button, _("Edit the LilyPond definition of the time signature. The editing affects only this layout."));
      g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (prefix_edit_callback), button);
      add_lilypond (button, get_lilypond_for_timesig (&staff->timesig), NULL);
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
    }
  else
    {                           //Make a matching expander so the music aligns with primary voice

      GtkWidget *expander = gtk_expander_new (label_text);
      gtk_widget_set_sensitive (expander, FALSE);
      gtk_widget_set_tooltip_text (expander, _("The clef, time and key signatures attached to other voices are ignored, only the primary one has effect"));
      gtk_box_pack_start (GTK_BOX (ret), expander, FALSE, TRUE, 0);
    }
  gchar *music = g_strconcat (_("Music for "), name, NULL);
  w = gtk_button_new_with_label (music);
  gtk_widget_set_tooltip_text (w, _("Click here to move the Denemo cursor to the start of this music.\nThe actual notes live here. You can only edit these in the main Denemo display.\nHowever you can place conditional directives that are to be used only when using this layout. For example page breaks just for this layout can be placed at points in the music.\n"));
  g_signal_connect (G_OBJECT (w), "clicked", G_CALLBACK (navigate_to_location), GINT_TO_POINTER (location));
  g_free (music);
  add_lilypond (w, text, NULL);
  gtk_box_pack_start (GTK_BOX (ret), w, FALSE, TRUE, 0);

  GtkWidget *button = gtk_button_new_with_label (_("Substitute"));
  gtk_widget_set_tooltip_text (button, _("Substitute the music of this staff/voice with the music of the current staff/voice.\nYou can click the \"Music for ...\" button to move the cursor in the Denemo Display onto the staff/voice whose music you want to use before clicking this button to substitute the music. The editing affects only this layout."));
  g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (substitute_voice_name), w);       //prefix_edit_callback), w);
  gtk_box_pack_start (GTK_BOX (ret), button, FALSE, TRUE, 0);

  return ret;
}

static GtkWidget *
create_lyric_widget (gchar * context_text, gchar * name)
{
  GtkWidget *ret = gtk_hbox_new (FALSE, 8);

  GtkWidget *w = gtk_button_new_with_label ("X");
  gtk_widget_set_tooltip_text (w, _("Drop this lyric part from the score layout"));
  g_signal_connect (w, "clicked", G_CALLBACK (remove_lyric_element), context_text);
  gtk_box_pack_end (GTK_BOX (ret), w, FALSE, TRUE, 0);
  w = gtk_button_new_with_label (name);
  //FIXME pop up the lyrics here or some info, or navigate??
  gtk_box_pack_start (GTK_BOX (ret), w, FALSE, TRUE, 0);

  w = gtk_button_new_with_label ("⬆");
  gtk_widget_set_tooltip_text (w, _("Move this lyric part above the staff for this score layout"));
  gtk_box_pack_start (GTK_BOX (ret), w, FALSE, TRUE, 0);
  g_signal_connect (w, "clicked", G_CALLBACK (move_parent), (gpointer) TRUE);
  w = gtk_button_new_with_label ("⬇");
  gtk_widget_set_tooltip_text (w, _("Move this lyric part below the staff for this score layout"));
  gtk_box_pack_start (GTK_BOX (ret), w, FALSE, TRUE, 0);
  g_signal_connect (w, "clicked", G_CALLBACK (move_parent), (gpointer) FALSE);
  return ret;
}

static void
install_voice (DenemoStaff * staff, gint movementnum, gint voice_count, GtkWidget * vbox)
{

  gchar *voicetag = get_voicetag (movementnum, voice_count);
  gchar *voicename = get_voicename (movementnum, voice_count);
  GtkWidget *voice = create_voice_widget (staff, voicename, get_location (movementnum, voice_count));

  GString *voicetext = g_string_new ("");
  //That is \new Voice = name prefix { postfix FIXME is prefix any use here????
  set_voice_definition (voicetext, staff, voicetag);
  gchar *text = g_strdup_printf (" %s ", voicetext->str);
  g_string_assign (voicetext, "");
  set_voice_termination (voicetext, staff);     // TAB TAB"} %End of voice" if not overridden
  add_lilypond (voice, text, g_string_free (voicetext, FALSE));


  gtk_box_pack_start (GTK_BOX (vbox), voice, FALSE, TRUE, 0);
}

static void
do_verses (DenemoStaff * staff, GtkWidget * vbox, gint movementnum, gint voice_count)
{
  //FIXME do text of the verses get_text_from_view(GtkWidget *textview) where staff->verse_views->data is textview widget
  GList *g = staff->verse_views;
  gint versenum = 1;
  if (!staff->hide_lyrics)
    for (versenum = 1; g; g = g->next, versenum++)
      {
        gchar *versename = get_versename (movementnum, voice_count, versenum);
        gchar *context_text = g_strdup_printf ("\n" TAB "\\%s%s", versename, "Context\n");
        //gchar *label = g_strconcat("Lyrics:", staff->denemo_name->str, NULL);
        gchar *label = g_strdup_printf ("Verse %d: %s", versenum, staff->denemo_name->str);
        GtkWidget *voice = create_lyric_widget (context_text, label);
        g_free (label);
        gchar *lyrics = g_strdup_printf ("\n" TAB "\\new Lyrics = %s\n", versename /*e.g. MvmntIVoiceIVerseI */ );
        add_lilypond (voice, lyrics, NULL);     //FIXME the destroy of these widgets should free the string
        add_lilypond (voice, NULL, context_text);

        gtk_box_pack_start (GTK_BOX (vbox), voice, FALSE, TRUE, 0);     //has to go outside the staff

        g_free (versename);
      }
}


//if oldval is in the prefix list attached to widget, replace it with newval and free oldval
static gboolean
edit_lilypond_prefix (GtkWidget * widget, gchar * oldval, gchar * newval)
{
  GList *g;
  for (g = g_object_get_data (G_OBJECT (widget), "prefix"); g; g = g->next)
    if (!strcmp ((gchar *) g->data, oldval))
      {
        g->data = (gpointer) newval;
        g_free (oldval);
        return TRUE;
      }
  return FALSE;
}


static void
prefix_edit_callback (GtkWidget * widget, GtkWidget * frame)
{
  if (!clone_scoreblock_if_needed (frame))
    return;
  GList *g = g_object_get_data (G_OBJECT (frame), "prefix");
  if (g)
    {
      gchar *lily = (gchar *) g->data;
      gchar *newval = string_dialog_editor_with_widget (Denemo.project, _("Edit LilyPond"), _("Edit this using LilyPond syntax\nThe editing applies just to this score layout"), lily ? lily : "", NULL, NULL);
      if (newval)
        {
          edit_lilypond_prefix (frame, lily, newval);
          score_status (Denemo.project, TRUE);
        }
    }
}

static void
affixes_delete_callback (GtkWidget * widget, GtkWidget * frame)
{
  if (!clone_scoreblock_if_needed (frame))
    return;
  GList *g = g_object_get_data (G_OBJECT (frame), "prefix");
  if (g)
    g_free (g->data);
  g_object_set_data (G_OBJECT (frame), "prefix", NULL);
  g = g_object_get_data (G_OBJECT (frame), "postfix");
  if (g)
    g_free (g->data);
  g_object_set_data (G_OBJECT (frame), "postfix", NULL);
  gtk_frame_set_label_widget (GTK_FRAME (frame), NULL);

  score_status (Denemo.project, TRUE);
}


static gboolean
popup (GtkWidget * button, GtkWidget * menu)
{
  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, GDK_CURRENT_TIME);
  return FALSE;
}

static void
create_element (GtkWidget * vbox, GtkWidget * widget, gchar * lilypond)
{
  GtkWidget *hbox = gtk_hbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);
  add_lilypond (hbox, lilypond, NULL);
  gtk_box_pack_start (GTK_BOX (hbox), widget, FALSE, TRUE, 0);
  GtkWidget *button = gtk_button_new_with_label (_("Edit"));
  gtk_widget_set_tooltip_text (button, _("Edit this element for this layout"));
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  g_signal_connect (button, "clicked", G_CALLBACK (prefix_edit_callback), hbox);
  button = gtk_button_new_with_label ("⬆");
  gtk_widget_set_tooltip_text (button, _("Move this element upwards for this score layout"));
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  g_signal_connect (button, "clicked", G_CALLBACK (move_parent), (gpointer) TRUE);
  button = gtk_button_new_with_label ("⬇");
  gtk_widget_set_tooltip_text (button, _("Move this element downwards for this score layout"));
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  g_signal_connect (button, "clicked", G_CALLBACK (move_parent), (gpointer) FALSE);
  button = gtk_button_new_with_label ("X");
  gtk_widget_set_tooltip_text (button, _("Remove this element from this layout"));
  gtk_box_pack_end (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  g_signal_connect (button, "clicked", G_CALLBACK (remove_element), NULL);
  gtk_widget_show_all (hbox);
}



//gtk_expander_set_expanded (expander,gtk_expander_get_expanded (expander));
/* UNUSED
static void
toggle_expand (GtkWidget * item, GtkWidget * expander)
{
  gtk_expander_set_expanded (GTK_EXPANDER (expander), !gtk_expander_get_expanded (GTK_EXPANDER (expander)));
}
*/

static void
page_break (GtkWidget * item, GtkWidget * vbox)
{
  if (!clone_scoreblock_if_needed (vbox))
    return;
  GtkWidget *button = gtk_button_new_with_label (_("Page Break"));
  gtk_widget_set_tooltip_text (button, _("This forces a new page, useful for avoiding page turns\n"));
  create_element (vbox, button, g_strdup ("\\pageBreak\n"));
  gtk_box_reorder_child (GTK_BOX (vbox), gtk_widget_get_parent (button), 0);
  score_status (Denemo.project, TRUE);
}

static void
blank_page (GtkWidget * item, GtkWidget * vbox)
{
  if (!clone_scoreblock_if_needed (vbox))
    return;
  GtkWidget *button = gtk_button_new_with_label (_("Blank Page"));
  gtk_widget_set_tooltip_text (button, _("This prints a page intentionally left blank, useful for avoiding page turns\n"));
  create_element (vbox, button, g_strdup ("\\pageBreak\n\\markup \\italic \"This page is intentionally left blank\"\n\\pageBreak\n"));
  gtk_box_reorder_child (GTK_BOX (vbox), gtk_widget_get_parent (button), 0);
  score_status (Denemo.project, TRUE);
}

static void
custom_lilypond (GtkWidget * item, GtkWidget * vbox)
{
  if (!clone_scoreblock_if_needed (vbox))
    return;
  GtkWidget *button = gtk_button_new_with_label ("LilyPond");
  gtk_widget_set_tooltip_text (button, _("This lets you insert your own titles etc just for this layout.\nFor book titles use \\titledPiece \\markup \"myname\"\nSimple titles are not placed here, but appear in a header block at the end of the movement.\nFor other possible uses, see LilyPond manual."));
  create_element (vbox, button, g_strdup ("%Enter LilyPond syntax here\n"));
  gtk_box_reorder_child (GTK_BOX (vbox), gtk_widget_get_parent (button), 0);
  score_status (Denemo.project, TRUE);
}

static GtkWidget *
get_titles_menu (GtkWidget * vbox)
{
  GtkWidget *menu = gtk_menu_new ();
  GtkWidget *item;

  item = gtk_menu_item_new_with_label (_("Create Page Break"));
  gtk_widget_set_tooltip_text (item, _("This inserts a page break, useful for avoiding page turns\nMove it before the title (using the up arrow) once created!"));
  g_signal_connect (item, "activate", G_CALLBACK (page_break), vbox);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);

  item = gtk_menu_item_new_with_label (_("Create Blank Page"));
  gtk_widget_set_tooltip_text (item, _("This inserts a page intentionally left blank, useful for avoiding page turns when printing on both sides of the paper"));
  g_signal_connect (item, "activate", G_CALLBACK (blank_page), vbox);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);

  item = gtk_menu_item_new_with_label (_("Insert LilyPond"));
  gtk_widget_set_tooltip_text (item, _(_("This creates a LilyPond comment which you can then edit to give titles etc for this movment, applying just to this layout.")));
  g_signal_connect (item, "activate", G_CALLBACK (custom_lilypond), vbox);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);

  item = gtk_separator_menu_item_new ();
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);

  gtk_widget_show_all (menu);
  return menu;
}


//return a newly allocated label to hint at the contents of a directive
static gchar *
label_for_directive (DenemoDirective * d)
{
  gchar *text = g_strdup_printf ("%s%s%.25s", d->tag ? d->tag->str : "<Unknown Tag>", d->display ? ": " : "", d->display ? d->display->str : "");
  gchar *etext = g_strescape (text, NULL);
  g_free (text);
  return etext;
}

static void
popup_movement_menu (GtkWidget * w, GtkWidget * vbox)
{
  gtk_menu_popup (GTK_MENU (get_titles_menu (vbox)), NULL, NULL, NULL, NULL, 0, GDK_CURRENT_TIME);
}

static void
popup_movement_titles_menu (GtkWidget * button)
{
  GtkWidget *menuitem = denemo_menusystem_get_widget ("BookTitles");
  if (get_standard_scoreblock (button))
    {
      if (menuitem)
        gtk_menu_popup (GTK_MENU (gtk_menu_item_get_submenu (GTK_MENU_ITEM (menuitem))), NULL, NULL, NULL, NULL, 0, GDK_CURRENT_TIME);
      else
        g_warning ("No such menu path");
    }
  else
    warningdialog (_("This button is for changing the score itself, it will not affect this custom layout"));
}

/* installs movement titles, page breaks etc
 *
 */
static void
install_pre_movement_widgets (GtkWidget * vbox, DenemoMovement * si, gboolean standard, DenemoScoreblock * sb)
{
  GtkWidget *frame = gtk_frame_new (NULL);
  gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
  GtkWidget *expander = gtk_expander_new (_("Movement Titles, Page Breaks etc"));
  gtk_widget_set_tooltip_text (expander, _("In here are settings for the movement title, page breaks before the movement etc"));
  gtk_container_add (GTK_CONTAINER (frame), expander);

  GtkWidget *inner_vbox = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (expander), inner_vbox);

  GtkWidget *inner_hbox = gtk_hbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (inner_vbox), inner_hbox, FALSE, TRUE, 0);
  GtkWidget *button;
#if 0
//having buttons that affect the score itself is confusing
  if (standard)
    {
      button = gtk_button_new_with_label (_("Create Titles for Movement"));
      mark_as_non_custom (button);
      gtk_widget_set_tooltip_text (button, _("Set book titles for this movement in the score"));
      g_signal_connect (button, "clicked", G_CALLBACK (popup_movement_titles_menu), NULL);
      gtk_box_pack_start (GTK_BOX (inner_hbox), button, FALSE, TRUE, 0);
    }
#endif

  button = gtk_button_new_with_label (_("Create for Custom Layout"));
  gtk_widget_set_tooltip_text (button, _("Create page breaks, blank pages ...for this layout"));
  g_signal_connect (button, "clicked", G_CALLBACK (popup_movement_menu), inner_vbox);
  gtk_box_pack_start (GTK_BOX (inner_hbox), button, FALSE, TRUE, 0);

  GList *g;
  for (g = si->movementcontrol.directives; g; g = g->next)
    {
      DenemoDirective *d = (DenemoDirective *) g->data;
      if (d->override & DENEMO_OVERRIDE_AFFIX)  //see set_initiate_scoreblock() call which outputs the DENEMO_OVERRIDE_AFFIX
        continue;
      if (d->override & DENEMO_OVERRIDE_HIDDEN)
        continue;
      if (sb && wrong_layout (d, sb->id))
        continue;
      if (d->prefix)
        {
          gchar *text = label_for_directive (d);
          GtkWidget *label = gtk_label_new (text);
          g_free (text);
          create_element (inner_vbox, label, g_strdup (d->prefix->str));
        }
    }
}

static void
popup_score_titles_menu (GtkWidget * button)
{
  GtkWidget *menuitem = denemo_menusystem_get_widget ("BookTitles");
  if (get_standard_scoreblock (button))
    {
      if (menuitem)
        gtk_menu_popup (GTK_MENU (gtk_menu_item_get_submenu (GTK_MENU_ITEM (menuitem))), NULL, NULL, NULL, NULL, 0, GDK_CURRENT_TIME);
      else
        g_warning ("No such menu path");
    }
  else
    warningdialog (_("This button is for changing the score itself, it will not affect this custom layout"));
}


/* install widgets for the postfix field of score directives (lilycontrol.directives) which are not hidden and which are not OVERRIDE_AFFIX and not conditionally out -
 * the prefix is done in create_score_directives.
 * Ones with OVERRIDE_AFFIX are done in set_default_scoreblock()
 */
static GtkWidget *
install_scoreblock_overrides (GtkWidget * vbox, DenemoProject * gui, DenemoMovement * si, gboolean last_movement, DenemoScoreblock * sb)
{

  GList *g;
  for (g = gui->lilycontrol.directives; g; g = g->next)
    {
      DenemoDirective *d = g->data;
      if (d->override & DENEMO_OVERRIDE_HIDDEN)
        continue;
      if (d->override & DENEMO_OVERRIDE_AFFIX)
        continue;
      if (wrong_layout (d, sb->id))
        continue;

      gchar *start = (d->postfix && d->postfix->len) ? d->postfix->str : NULL;
      if (start)
        {
          GtkWidget *frame = gtk_frame_new (NULL);

          gchar *text = label_for_directive (d);
          GtkWidget *button = gtk_button_new_with_label (text);
          g_free (text);
          gtk_frame_set_label_widget (GTK_FRAME (frame), button);
          gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
          gchar *lily = g_strdup_printf ("\n<< %s\n<< ", start);
          add_lilypond (frame, lily, g_strdup ("\n          >>\n>>"));

          GtkWidget *menu = gtk_menu_new ();
          GtkWidget *menuitem = gtk_menu_item_new_with_label ("Edit");
          g_signal_connect (menuitem, "activate", G_CALLBACK (prefix_edit_callback), frame);
          gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);
          menuitem = gtk_menu_item_new_with_label ("Delete");
          g_signal_connect (menuitem, "activate", G_CALLBACK (affixes_delete_callback), frame);
          gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);
          gtk_widget_show_all (menu);
          g_signal_connect (button, "clicked", G_CALLBACK (popup), menu);



          vbox = gtk_vbox_new (FALSE, 8);
          gtk_container_add (GTK_CONTAINER (frame), vbox);
        }
    }
  return vbox;
}



static gboolean
draw_staff_brace_for_layout (GtkWidget * w, cairo_t * cr, gchar * context)
{
  GtkAllocation allocation;
  gtk_widget_get_allocation (w, &allocation);
  gint height = allocation.height;
  cairo_set_source_rgb (cr, 0.9, 0.9, 0.9);
  cairo_paint (cr);
  gboolean curly = !((!strcmp (context, "ChoirStaffStart")) || (!strcmp (context, "GroupStaffStart")));
  draw_staff_brace (cr, curly, 5, 8, height * 0.9 - 15);
  return TRUE;
}

#if GTK_MAJOR_VERSION == 2
static gboolean
draw_staff_brace_gtk2 (GtkWidget * w, GdkEventExpose * event, gchar * context)
{
  cairo_t *cr = gdk_cairo_create (event->window);
  gdk_cairo_region (cr, event->region);
  cairo_clip (cr);
  draw_staff_brace_for_layout (w, cr, context);
  cairo_destroy (cr);
  return TRUE;
}
#endif

static void
show_type (GtkWidget * widget, gchar * message)
{
  g_message ("%s%s", message, widget ? g_type_name (G_TYPE_FROM_INSTANCE (widget)) : "NULL widget");
}

static void
delete_brace (gchar * postfix)
{
  gchar *c = g_strrstr (postfix, ">>%");
  if (c && c > postfix)
    {
      *c = 0;
      return;
    }

#if 0
  gchar *c = postfix;
  while (*(c++));
  c--;
  c--;
  c--;
  if (c != postfix)
    while (*(c--) != '\n');
  c++;
  if (*c == '\n')
    *c = 0;
#endif
}

static void
remove_brace_end (GtkWidget * vbox)
{
  show_type (vbox, "new vbox ??? ");
  GList *g = g_object_get_data (G_OBJECT (vbox), "postfix");
  for (; g; g = g->next)
    {
      //g_print ("Next postfix %s\n", (char *) g->data);
      delete_brace (g->data);
      //g_print ("transformed to %s\n", (char *) g->data);
    }
}

static gboolean
remove_context (GtkWidget * button, GtkWidget * parent)
{
  if (!clone_scoreblock_if_needed (parent))
    return TRUE;
  show_type (parent, "parent ");
  GList *children = gtk_container_get_children (GTK_CONTAINER (parent));
  //show_type (g_list_last (children)->data, "vbox type is");
  //show_type (gtk_widget_get_parent (gtk_widget_get_parent (parent)), "Reparenting on ");
  GtkWidget *vbox = g_list_last (children)->data;
  GList *staff_list = gtk_container_get_children (GTK_CONTAINER (vbox));
  GList *g;
  for (g = staff_list; g; g = g->next)
    {
      //show_type (g->data, "The staff frame is ");
      gtk_widget_reparent (g->data,     //frame
                           gtk_widget_get_parent (gtk_widget_get_parent (parent)));
    }

  GtkWidget *topw = gtk_widget_get_parent (gtk_widget_get_parent (gtk_widget_get_parent (parent)));
  //show_type (topw, "new vbox parent ");g_print ("parent %p, grandparent %p, great-grp %p\n", parent, gtk_widget_get_parent (parent), topw);

  if (g_object_get_data (G_OBJECT (parent), "postfix") == NULL)
    remove_brace_end (gtk_bin_get_child (g_list_last (staff_list)->data));      // without it deleting a top level brace leaving lower ones fails.
  else
    for (g = g_object_get_data (G_OBJECT (parent), "postfix"); g; g = g->next)
      {
        delete_brace (g->data);
        if (GTK_IS_BOX (topw))
          add_lilypond (topw, NULL, g_strdup (g->data));
      }
  Denemo.project->lilysync = G_MAXUINT;
  gtk_widget_destroy (gtk_widget_get_parent (parent));
  return TRUE;
}

//Move the frame above into this frame's vbox, unless we are inside it.
/* UNUSED
static gboolean
move_context_up (GtkWidget * button, GtkWidget * parent)
{                               //!!!! these not working
  if (!clone_scoreblock_if_needed (parent))
    return TRUE;
  DenemoScoreblock *sb = get_custom_scoreblock (parent);
  gint index = g_list_index (sb->staff_list, parent);
  if (index > 0)
    {
      GtkWidget *frame = (GtkWidget *) g_list_nth (sb->staff_list, index - 1)->data;
      if (!gtk_widget_is_ancestor (parent, frame))
        {
          GtkWidget *hbox = gtk_bin_get_child (GTK_BIN (parent));
          GList *children = gtk_container_get_children (GTK_CONTAINER (hbox));
          GtkWidget *vbox = g_list_last (children)->data;
          g_list_free (children);
          gtk_widget_reparent (frame, vbox);
          gtk_box_reorder_child (GTK_BOX (vbox), frame, 0);
          //now move the frame on one in the list of frames
          sb->staff_list = g_list_remove (sb->staff_list, frame);
          sb->staff_list = g_list_insert (sb->staff_list, frame, index);
          //layout = ???
        }
    }
  return TRUE;
}
*/
//Move the frame below into this frame's vbox, unless we are inside it.
/* UNUSED
static gboolean
move_context_down (GtkWidget * button, GtkWidget * parent)
{
  if (!clone_scoreblock_if_needed (parent))
    return TRUE;
  DenemoScoreblock *sb = get_custom_scoreblock (parent);        //!!!!doesn't work see junk.denemo
  g_assert (sb);
  gint index = g_list_index (sb->staff_list, parent);
  if (index < g_list_length (sb->staff_list))
    {
      GtkWidget *frame = (GtkWidget *) g_list_nth (sb->staff_list, index + 1)->data;
      if (!gtk_widget_is_ancestor (parent, frame))
        {
          GtkWidget *hbox = gtk_bin_get_child (GTK_BIN (parent));
          GList *children = gtk_container_get_children (GTK_CONTAINER (hbox));
          GtkWidget *vbox = g_list_last (children)->data;
          g_list_free (children);
          gtk_widget_reparent (frame, vbox);
          gtk_box_reorder_child (GTK_BOX (vbox), frame, -1);
          //now move the frame on one in the list of frames
          //sb->staff_list = g_list_remove(sb->staff_list, frame);
          //sb->staff_list = g_list_insert(sb->staff_list, frame, index);
//                      layout_sync = ???
        }
    }
  return TRUE;
}
*/

static GtkWidget *
install_staff_group_start (GList ** pstaffs, GtkWidget * vbox, GList * directives, gint * nesting)
{
  GList *g;
  for (g = directives; g; g = g->next)
    {
      DenemoDirective *directive = g->data;
      if ((directive->override & DENEMO_OVERRIDE_AFFIX) && !(directive->override & DENEMO_ALT_OVERRIDE))
        {
          if (wrong_layout (directive, Denemo.project->layout_id))
            continue;
          if (directive->prefix && (directive->prefix->len > 0))
            {
              GtkWidget *frame = (GtkWidget *) gtk_frame_new (directive->tag->str);
              add_lilypond (frame, directive->prefix ? g_strdup (directive->prefix->str) : NULL, directive->postfix ? g_strdup (directive->postfix->str) : NULL);
              (*nesting)++;
              *pstaffs = g_list_append (*pstaffs, frame);
              g_signal_connect (G_OBJECT (frame), "destroy", G_CALLBACK (remove_from_staff_list), pstaffs);
              gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
              GtkWidget *hbox = gtk_hbox_new (FALSE, 8);
              gtk_container_add (GTK_CONTAINER (frame), hbox);


              GtkWidget *layout = gtk_drawing_area_new ();
              gtk_widget_set_tooltip_text (layout, _("This brace connects together several staffs - you can delete it for a customized layout."));

#if GTK_MAJOR_VERSION == 2
              g_signal_connect (G_OBJECT (layout), "expose_event", G_CALLBACK (draw_staff_brace_gtk2), directive->tag->str);
#else
              g_signal_connect (G_OBJECT (layout), "draw", G_CALLBACK (draw_staff_brace_for_layout), directive->tag->str);
#endif

              gint width = 20, height = 100;
              gtk_widget_set_size_request (layout, width, height);

              gtk_box_pack_start (GTK_BOX (hbox), layout, TRUE, TRUE, 0);


              GtkWidget *controls = gtk_vbox_new (FALSE, 8);
              gtk_box_pack_start (GTK_BOX (hbox), controls, FALSE, TRUE, 0);

              GtkWidget *button = gtk_button_new_with_label ("X");
              gtk_widget_set_tooltip_text (button, _("Remove this staff brace from these staffs for a customized layout."));
              g_signal_connect (button, "clicked", G_CALLBACK (remove_context), hbox);
              gtk_box_pack_start (GTK_BOX (controls), button, FALSE, TRUE, 0);
              vbox = gtk_vbox_new (FALSE, 8);   //this vbox will be passed back so that the staffs can be put inside this staff group frame.
              gtk_box_pack_end (GTK_BOX (hbox), vbox, FALSE, TRUE, 0);
            }
        }
    }
  return vbox;
}

static GtkWidget *
install_staff_group_end (GtkWidget * vbox, GList * directives, gint * nesting)
{
  GList *g;
  for (g = directives; g; g = g->next)
    {
      DenemoDirective *directive = g->data;
      if (wrong_layout (directive, Denemo.project->layout_id))
        continue;

      if ((directive->override & DENEMO_OVERRIDE_AFFIX) && !(directive->override & DENEMO_ALT_OVERRIDE))
        {
          if (directive->postfix && (directive->postfix->len > 0))
            {
              if (*nesting)
                {
                  //show_type (gtk_widget_get_parent (vbox), "Adding beam ends to type: "); g_print ("Specifically %s to %p\n", directive->postfix->str, gtk_widget_get_parent (vbox));
                  add_lilypond (gtk_widget_get_parent (vbox), NULL, g_strdup (directive->postfix->str));
                  gint number_of_ends = 1;
                  if (directive->data)
                    number_of_ends = atoi (directive->data->str);
                  if (number_of_ends < 0 || (number_of_ends > 10))
                    number_of_ends = 1; //sanity check on data in directive

                  (*nesting) -= number_of_ends;
                  vbox = gtk_widget_get_parent (gtk_widget_get_parent (gtk_widget_get_parent (vbox)));
                }
              else
                g_warning ("Badly placed end of staff group - ignored");
            }
        }
    }
  return vbox;
}

static GtkWidget *
get_label (GtkWidget * button)
{
  if (GTK_IS_CONTAINER (button))
    {
      GList *g = gtk_container_get_children (GTK_CONTAINER (button));
      for (; g; g = g->next)
        if (GTK_IS_LABEL (g->data))
          return GTK_WIDGET (g->data);
    }
  return NULL;
}

static GtkWidget *
get_large_button (gchar * text)
{
  GtkWidget *button = gtk_button_new_with_label ("dummy");
  GtkWidget *label = get_label (button);
  gchar *markup = g_markup_printf_escaped ("<big>%s</big>", text);
  gtk_label_set_markup (GTK_LABEL (label), markup);
  g_free (markup);
  return button;
}

static GtkWidget *
get_small_button (gchar * text)
{
  GtkWidget *button = gtk_button_new_with_label ("dummy");
  GtkWidget *label = get_label (button);
  gchar *markup = g_markup_printf_escaped ("<small>%s</small>", text);
  gtk_label_set_markup (GTK_LABEL (label), markup);
  g_free (markup);
  return button;
}

static void
add_staff_widget (DenemoStaff * staff, GtkWidget * hbox)
{
  gchar *clef_glyph = "\xF0\x9D\x84\x9E     ";
  switch (staff->clef.type)
    {
    case DENEMO_FRENCH_CLEF:

    case DENEMO_TREBLE_CLEF:
      clef_glyph = "𝄞     ";
      break;
    case DENEMO_G_8_CLEF:
      clef_glyph = "𝄟  ";
      break;

    case DENEMO_F_8_CLEF:
      clef_glyph = "𝄤   ";
      break;

    case DENEMO_BASS_CLEF:
      clef_glyph = "𝄢     ";
      break;
    case DENEMO_SOPRANO_CLEF:
    case DENEMO_ALTO_CLEF:
    case DENEMO_TENOR_CLEF:
      clef_glyph = "𝄡   ";
      break;
    default:
      break;
    }
  GtkWidget *button = get_large_button (clef_glyph);
  gtk_widget_set_tooltip_text (button, _("This shows the clef in the Denemo score - the actual clef printed may be modified by Directives attached to it.\nYou can edit the clef for a custom layout - do this on the first voice on the staff."));
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = get_small_button ("⬆");
  gtk_widget_set_tooltip_text (button, _("Move this staff (with all its voices) above the preceding staff."));
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  g_signal_connect (button, "clicked", G_CALLBACK (move_grandparent), (gpointer) TRUE);
  button = get_small_button ("⬇");
  gtk_widget_set_tooltip_text (button, _("Move this staff (with all its voices) after the following staff."));
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  g_signal_connect (button, "clicked", G_CALLBACK (move_grandparent), (gpointer) FALSE);

  button = gtk_button_new_with_label ("X");
  gtk_widget_set_tooltip_text (button, _("Remove this staff (with all its voices) for customized layout."));

  gtk_box_pack_end (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  g_signal_connect_swapped (button, "clicked", G_CALLBACK (remove_parent_element), hbox);
}



static void
install_staff_with_voices (GList ** pstaffs, GtkWidget ** pvbox, gchar * partname, GList ** pstafflist, gint * pvoice_count, gint staff_count, gint movementnum, gint * pstaff_group_nesting, gboolean standard, gboolean append_only)
{
  DenemoMovement *si = Denemo.project->movement;
  GList *g = *pstafflist;
  GtkWidget *vbox = *pvbox;
  DenemoStaff *staff = g->data;
  DenemoStaff *nextstaff = g->next ? g->next->data : NULL;


  //if (partname == NULL) Don't omit staff groups start for single part, since parts can be multi-staff e.g. piano, it will be closed at the end if the part doesn't include the close
  if (!append_only)
    vbox = install_staff_group_start (pstaffs, vbox, staff->staff_directives, pstaff_group_nesting);

  if (staff->hasfakechords)
    {                           //the reason these are outside the staff frame is it makes them appear above the staff
      GtkWidget *chords = gtk_label_new (_("Chord Symbols"));
      gchar *text = g_strdup_printf ("\n" TAB TAB "\\new ChordNames \\chordmode { \\%sChords }\n",
                                     get_voicename (movementnum, (*pvoice_count)));
      add_lilypond (chords, text, NULL);
      gtk_box_pack_start (GTK_BOX (vbox), chords, FALSE, TRUE, 0);
      *pstaffs = g_list_append (*pstaffs, chords);
      g_signal_connect (G_OBJECT (chords), "destroy", G_CALLBACK (remove_from_staff_list), pstaffs);
    }

  gchar *label_text = (si->thescore->next == NULL) ? g_strdup (_("Staff Start")) : g_strdup_printf (_("Staff %d Start"), staff_count);
  GtkWidget *frame = gtk_frame_new (NULL);

  GtkWidget *staff_hbox = gtk_hbox_new (FALSE, 8);
  //gtk_frame_set_label_widget (GTK_FRAME (frame), staff_hbox); !!!!! setting the label widget it's position looks odd because no frame is visible
  GtkWidget *button = gtk_button_new_with_label (label_text);
  g_free (label_text);
  gtk_box_pack_start (GTK_BOX (staff_hbox), button, FALSE, TRUE, 0);
  gtk_widget_set_tooltip_text (button, _("Click for a menu to position the Denemo cursor on this staff\nor to alter this staff for a customized layout"));


  GtkWidget *menu = gtk_menu_new ();
  GtkWidget *menuitem = gtk_menu_item_new_with_label (_("Move Denemo Cursor to this staff"));
  gtk_widget_set_tooltip_text (menuitem, _("This will move the Denemo Cursor to the start of this staff in this movement"));
  g_signal_connect (G_OBJECT (menuitem), "activate", G_CALLBACK (navigate_to_location), GINT_TO_POINTER (get_location (movementnum, (*pvoice_count))));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);



  menuitem = gtk_menu_item_new_with_label (_("Edit Staff Opening Syntax"));
  gtk_widget_set_tooltip_text (menuitem, _("Edit the syntax creating this staff to customize this layout\nTake care only alter the obvious bits, such as instrument name etc\nInjudicious deletion of the LilyPond typesetting characters {<<# etc can make the layout unreadable by the LilyPond typesetter. Just delete the layout if you get stuck."));
  g_signal_connect (menuitem, "activate", G_CALLBACK (prefix_edit_callback), frame);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);

  gtk_widget_show_all (menu);
  g_signal_connect (button, "clicked", G_CALLBACK (popup), menu);
#if 0
//having buttons that affect the score itself is confusing
  if (standard && (si->thescore->next != NULL))
    {
      button = gtk_button_new_with_label (_("Set Staff Group Start/End"));
      mark_as_non_custom (button);
      gtk_box_pack_start (GTK_BOX (staff_hbox), button, FALSE, TRUE, 0);
      gtk_widget_set_tooltip_text (button, _("The braces { and [ binding staffs together can be set here. Set the start on one staff and the end on a later staff.\nThis is editing the score, not just customizing a layout.\nRefresh the layout view (see under Options for this Layout button at the top) once you have made the changes."));
      g_signal_connect (button, "button-press-event", G_CALLBACK (staff_groups_menu), GINT_TO_POINTER (get_location (movementnum, (*pvoice_count))));
    }
#endif
  *pstaffs = g_list_append (*pstaffs, frame);
  g_signal_connect (G_OBJECT (frame), "destroy", G_CALLBACK (remove_from_staff_list), pstaffs);

  GString *staffprefix = g_string_new ("");
  set_staff_definition (staffprefix, staff); // TAKES DENEMO_ALT_OVERRIDE that are DENEMO_OVERRIDE_AFFIX in exportlilypond

  // if (staff->no_of_lines != 5) now done by a directive
  //   g_string_append_printf (staffprefix, TAB "\\override Staff.StaffSymbol  #'line-count = #%d\n", staff->no_of_lines);     //FIXME create_element

  GString *stafftext = g_string_new ("");
  g_string_assign (stafftext, "");
  set_staff_termination (stafftext, staff);     // "\n>>\n%End of Staff\n"


  add_lilypond (frame, g_string_free (staffprefix, FALSE), g_string_free (stafftext, FALSE));



  gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);

  GtkWidget *outer_vbox = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (frame), outer_vbox);


  gtk_box_pack_start (GTK_BOX (outer_vbox), staff_hbox, FALSE, TRUE, 0);

  GtkWidget *hbox = gtk_hbox_new (FALSE, 8);
  gtk_box_pack_end (GTK_BOX (outer_vbox), hbox, FALSE, TRUE, 0);
  //gtk_container_add (GTK_CONTAINER (frame), hbox);
  add_staff_widget (staff, hbox);

  label_text = (nextstaff && nextstaff->voicecontrol & DENEMO_SECONDARY) ? _("Voices") : _("Voice");
  GtkWidget *expander = gtk_expander_new (label_text);
  gtk_widget_set_tooltip_text (expander, _("This holds the voice(s) of the staff - the clef, time signature, key signature and music are all here"));
  //gtk_container_add (GTK_CONTAINER (frame), expander);
  gtk_box_pack_end (GTK_BOX (hbox), expander, FALSE, TRUE, 0);
  GtkWidget *voices_vbox = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (expander), voices_vbox);

  if (staff->hasfigures)
    {
      GtkWidget *voice = gtk_label_new ("Bass figures");
      gchar *text = g_strdup_printf ("\n" TAB TAB "\\context Staff \\with {implicitBassFigures = #'(0) } \\%sBassFiguresLine %%End of bass figures\n",
                                     get_voicename (movementnum, (*pvoice_count)));
      add_lilypond (voice, text, NULL);
      gtk_box_pack_start (GTK_BOX (voices_vbox), voice, FALSE, TRUE, 0);
    }

  install_voice (staff, movementnum, (*pvoice_count), voices_vbox);     //Primary voice
  do_verses (staff, vbox, movementnum, (*pvoice_count));

{  
    label_text = (si->thescore->next == NULL) ? g_strdup (_("Staff End")) : g_strdup_printf (_("Staff %d End"), staff_count);
     GString *text = g_string_new ("");
    set_staff_finalize (text, staff);
    GtkWidget *butt = gtk_button_new_with_label (label_text);
    g_free (label_text);
    create_element (vbox, butt,  g_string_free (text, FALSE));
}


  if (nextstaff && (nextstaff->voicecontrol & DENEMO_SECONDARY))
    {
      for (g = g->next, (*pvoice_count)++; g && (((DenemoStaff *) g->data)->voicecontrol & DENEMO_SECONDARY); g = g->next, (*pvoice_count)++)
        {

          DenemoStaff *staff = g->data;
          install_voice (staff, movementnum, (*pvoice_count), voices_vbox);
          do_verses (staff, vbox, movementnum, (*pvoice_count));        //!!! these need *pstaffs = g_list_append(*pstaffs, voice); treatment too...
          if (partname == NULL)
            vbox = install_staff_group_end (vbox, staff->staff_directives, pstaff_group_nesting);
        }
      if (g != NULL)
        {
          g = g->prev;
          (*pvoice_count)--;
        }
    }
  if (partname == NULL)         //Have to omit all end braces for part layouts, since the part may not include all the start braces for them.
    vbox = install_staff_group_end (vbox, staff->staff_directives, pstaff_group_nesting);
  *pstafflist = g;
  *pvbox = vbox;
}

static void
append_staff (GtkWidget * widget, GList ** pstaffs)
{
  if (!clone_scoreblock_if_needed (widget))
    return;
  gint staff_group_nesting = 0;
  gint voice_count = Denemo.project->movement->currentstaffnum;
  gint movementnum = 1;
  if (Denemo.project->movements)
    movementnum = 1 + g_list_index (Denemo.project->movements, Denemo.project->movement);
  GtkWidget *vbox = gtk_widget_get_parent (widget);
  install_staff_with_voices (pstaffs, &vbox, NULL, &Denemo.project->movement->currentstaff, &voice_count, Denemo.project->movement->currentstaffnum, movementnum, &staff_group_nesting, FALSE, TRUE);
  gtk_widget_show_all (vbox);
  Denemo.project->lilysync = G_MAXUINT;
}

static GtkWidget *
get_movement_widget (GList ** pstaffs, gchar * partname, DenemoMovement * si, gint movementnum, gboolean last_movement, gboolean standard, DenemoScoreblock * sb)
{
  DenemoProject *gui = Denemo.project;
  gint staff_group_nesting = 0; //to check on loose staff group markers
  gint voice_count;             //a count of voices from the very top of the score (ie DenemoStaffs in thescore)
  gint staff_count;             //a count of staffs excluding voices from top of score
  GtkWidget *ret = gtk_frame_new (NULL);
  GString *start = g_string_new ("");
  set_initiate_scoreblock (si, start);  // ie << possibly overridden
  add_lilypond (ret, g_string_free (start, FALSE), g_strdup ("\n          >>\n"));

  GtkWidget *vbox = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (ret), vbox);

  vbox = install_scoreblock_overrides (vbox, gui, si, last_movement, sb);       //things like transpose whole score etc

  gchar *label_text = (si->thescore->next == NULL) ? _("The Staff") : _("The Staffs");
  GtkWidget *topexpander = gtk_expander_new (label_text);
  gtk_widget_set_tooltip_text (topexpander, _("This holds the staffs below which are the voices with the music."));
  gtk_expander_set_expanded (GTK_EXPANDER (topexpander), si == Denemo.project->movement);
  gtk_box_pack_start (GTK_BOX (vbox), topexpander, FALSE, TRUE, 0);
  vbox = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (topexpander), vbox);
  GtkWidget *addbutton = gtk_button_new_with_label (_("Append Current Staff"));
  gtk_widget_set_tooltip_text (addbutton, _("Appends the current staff (the one where the cursor is in the Denemo Display) to this layout. The same staff can be placed at multiple positions in the layout with individual edits in each."));

  gtk_box_pack_start (GTK_BOX (vbox), addbutton, FALSE, TRUE, 0);
  g_signal_connect (G_OBJECT (addbutton), "clicked", G_CALLBACK (append_staff), pstaffs);

  GList *g;
  for (voice_count = 1, staff_count = 1, g = si->thescore; g; g = g->next, voice_count++, staff_count++)
    {
      DenemoStaff *staff = g->data;
      if ((*(staff->lily_name->str)) && (partname && strcmp (partname, staff->lily_name->str))) // empty partname means include with all parts.
        continue;
      install_staff_with_voices (pstaffs, &vbox, partname, &g, &voice_count, staff_count /*sic */ , movementnum, &staff_group_nesting, standard, FALSE);

      if (g == NULL)
        break;
    }                           //for each staff

  if (staff_group_nesting < 0)
    {
      g_critical ("Impossible staff group nesting");
    }
  else
    {

      for (; staff_group_nesting; staff_group_nesting--)
        {
          if (partname == NULL)
            {
              g_warning ("Staff group start without end - terminating it");
              add_lilypond (vbox, NULL, g_strdup (" >>%Missing staff group end inserted here\n"));
            }
          else
            add_lilypond (vbox, NULL, g_strdup (" >>%Closing staff group end for part layout\n"));

        }
    }




  return ret;
}

/* append the data labeled with affix on the widget s to the out string */
static void
append_lilypond_for_affix (const gchar * affix, GtkWidget * w, GString * out)
{
  GList *g;
  for (g = (GList *) g_object_get_data (G_OBJECT (w), affix); g; g = g->next)
    {
      gchar *text = (gchar *) g->data;
      if (text)
        g_string_append (out, text);
    }
}

/* go through the layout appending score block elements to the out string */
static void
lilypond_for_layout (GString * out, GtkWidget * layout)
{
  append_lilypond_for_affix ("prefix", layout, out);
  if (GTK_IS_CONTAINER (layout))
    {
      GList *list = gtk_container_get_children (GTK_CONTAINER (layout));
      if (list)
        {
          GList *g = list;
          do
            {
              lilypond_for_layout (out, g->data);
            }
          while ((g = g->next));
          g_list_free (list);
        }
    }
  append_lilypond_for_affix ("postfix", layout, out);
}

//return a newly allocated name for a standard scoreblock
static gchar *
movement_part_name (gint movement, gchar * partname)
{
  if (movement && partname)
    return g_strdup_printf ("%s M%d", partname, movement);
  if (movement)
    return g_strdup_printf (_("Movement %d"), movement);
  if (partname)
    return g_strdup_printf ("%s", partname);
  return g_strdup (DEFAULT_SCORE_LAYOUT);
}

static GtkWidget *
get_event_box (GtkWidget * vbox)
{
  GtkWidget *event_box = gtk_event_box_new ();
  gtk_box_pack_start (GTK_BOX (vbox), event_box, FALSE, TRUE, 0);
#if GTK_MAJOR_VERSION == 2
  GdkColor color;
  if (gdk_color_parse ("#BBFFCC", &color))
    gtk_widget_modify_bg (event_box, GTK_STATE_NORMAL, &color);
#else 
{
    set_background_color (event_box, "#bbffcc");
}
#endif
  return event_box;
}

static void
create_misc_scorewide (GtkWidget * inner_vbox)
{
  DenemoProject *gui = Denemo.project;


  gchar *lily = g_strdup_printf ("#(set-default-paper-size \"%s\"%s)\n", gui->lilycontrol.papersize->str, gui->lilycontrol.orientation ? "" : " 'landscape");
  create_element (inner_vbox, gtk_button_new_with_label (_("paper size")), lily);
  lily = g_strdup_printf ("#(set-global-staff-size %s)\n", gui->lilycontrol.staffsize->str);
  create_element (inner_vbox, gtk_button_new_with_label (_("Global staff size")), lily);
  GtkWidget *expander = gtk_expander_new (_("Paper Block"));
  gtk_widget_set_tooltip_text (expander, _("Settings for whole score: includes overall staff size, paper size ...\n"));
  add_lilypond (expander, g_strdup ("\\paper {\n"), g_strdup ("\n       }\n"));
  gtk_box_pack_start (GTK_BOX (inner_vbox), expander, FALSE, TRUE, 0);
  GtkWidget *paper_box = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (expander), paper_box);

  create_element (paper_box, gtk_button_new_with_label (_("the paper block contents")), get_lilypond_paper ());

}

static void
create_scoreheader_directives (GtkWidget * vbox, DenemoScoreblock * sb)
{
  DenemoProject *gui = Denemo.project;
  GtkWidget *frame = gtk_frame_new (NULL);
  gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
  GtkWidget *top_expander = gtk_expander_new (_("Score Titles"));
  gtk_expander_set_expanded (GTK_EXPANDER (top_expander), TRUE);
  add_lilypond (top_expander, g_strdup ("\n\\header {\n"), g_strdup ("\n        }\n"));
  gtk_widget_set_tooltip_text (top_expander, _("Titles, layout settings, preferences etc for the whole score.\nIncludes main title, composer, date, instrumentation, tagline"));
  gtk_container_add (GTK_CONTAINER (frame), top_expander);
  GtkWidget *header_box = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (top_expander), header_box);

  gchar *escaped_name = g_strescape (gui->filename->str, NULL);
  gchar *default_tagline = g_strdup_printf ("tagline = \\markup {\"%s\" on \\simple #(strftime \"%%x\" (localtime (current-time)))}\n", escaped_name);
  g_free (escaped_name);
  create_element (header_box, gtk_label_new (_("Default tagline")), default_tagline);

  GList *g;
  for (g = gui->scoreheader.directives; g; g = g->next)
    {
      DenemoDirective *directive = (DenemoDirective *) g->data;
      if (directive->override & (DENEMO_OVERRIDE_AFFIX | DENEMO_OVERRIDE_HIDDEN))
        continue;
      if (directive->postfix == NULL)
        continue;
      if (wrong_layout (directive, sb->id))
        continue;


      create_element (header_box, gtk_label_new (directive->tag->str), g_strdup (directive->postfix->str));
    }
}

static void
create_score_directives (GtkWidget * vbox, DenemoScoreblock * sb)
{
  DenemoProject *gui = Denemo.project;
  if (gui->lilycontrol.directives == NULL)
    return;
  GtkWidget *frame = gtk_frame_new (NULL);
  gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
  GtkWidget *top_expander = gtk_expander_new (_("Score Directives"));
  gtk_widget_set_tooltip_text (top_expander, _("Includes the indent before first measure, LilyPond include files ..."));
  gtk_container_add (GTK_CONTAINER (frame), top_expander);
  GtkWidget *inner_vbox = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (top_expander), inner_vbox);

  GList *g = gui->lilycontrol.directives;
  for (; g; g = g->next)
    {
      DenemoDirective *directive = g->data;
      if (wrong_layout (directive, sb->id))
        continue;

      if (directive->prefix && !(directive->override & (DENEMO_OVERRIDE_AFFIX)) && !(directive->override & (DENEMO_ALT_OVERRIDE)))
        {
          GtkWidget *label = gtk_label_new (directive->tag->str);
          create_element (inner_vbox, label, g_strdup (directive->prefix->str));
        }
    }
}

static void
create_alt_score_directives (GtkWidget * vbox, DenemoScoreblock * sb)
{
  DenemoProject *gui = Denemo.project;
  if (gui->lilycontrol.directives == NULL)
    return;
  GtkWidget *frame = gtk_frame_new (NULL);
  gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 0);
  GtkWidget *top_expander = gtk_expander_new (_("Alt Score Directives"));
  gtk_widget_set_tooltip_text (top_expander, _("Includes the global font size ..."));
  gtk_container_add (GTK_CONTAINER (frame), top_expander);
  GtkWidget *inner_vbox = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (top_expander), inner_vbox);

  GList *g = gui->lilycontrol.directives;
  for (; g; g = g->next)
    {
      DenemoDirective *directive = g->data;
      if (wrong_layout (directive, sb->id))
        continue;

      if (directive->prefix && (directive->override & (DENEMO_ALT_OVERRIDE)))
        {
          GtkWidget *label = gtk_label_new (directive->tag->str);
          create_element (inner_vbox, label, g_strdup (directive->prefix->str));
        }
    }
}



static void
fill_scorewide_frame (GtkWidget * frame, GtkWidget * reload_button, DenemoScoreblock * sb)
{

  GtkWidget *expander = gtk_expander_new (_("Score-wide Settings."));
  gtk_widget_set_tooltip_text (expander, _("Setting the score title, composer, headers and footers for this layout"));
  gtk_container_add (GTK_CONTAINER (frame), expander);

  GtkWidget *inner_box = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (expander), inner_box);
  gtk_box_pack_start (GTK_BOX (inner_box), reload_button, FALSE, TRUE, 0);
#if 0
//having buttons that affect the score itself is confusing
  GtkWidget *button = gtk_button_new_with_label (_("Create Book Titles"));
  mark_as_non_custom (button);
  gtk_widget_set_tooltip_text (button, _("Set book titles for the score"));
  g_signal_connect (button, "clicked", G_CALLBACK (popup_score_titles_menu), NULL);
  gtk_box_pack_start (GTK_BOX (inner_box), button, FALSE, TRUE, 0);
#endif

  create_scoreheader_directives (inner_box, sb);
  create_score_directives (inner_box, sb);
  create_misc_scorewide (inner_box);
  create_alt_score_directives (inner_box, sb);
}

static GtkWidget *
get_reload_button (GtkWidget * frame)
{
  GtkWidget *reload_button = gtk_button_new_with_label (_("Reload Score-Wide Settings"));
  gtk_widget_set_tooltip_text (reload_button, _("Reload the score wide settings for this layout from the current values in the score.\nDo this if you have made changes to the score titles etc which you wish to be used for this layout."));
  g_signal_connect_swapped (reload_button, "clicked", G_CALLBACK (reload_scorewide_block), frame);
  return reload_button;
}

static void
reload_scorewide_block (GtkWidget * frame)
{
  GtkWidget *event_box = gtk_widget_get_parent (frame);
  GtkWidget *vbox = gtk_widget_get_parent (event_box);
  gtk_widget_destroy (frame);
  frame = gtk_frame_new (NULL);
  GtkWidget *reload_button = get_reload_button (frame);
  gtk_container_add (GTK_CONTAINER (event_box), frame);
  fill_scorewide_frame (frame, reload_button, selected_scoreblock ());
  gtk_widget_show_all (vbox);
  Denemo.project->lilysync = G_MAXUINT;

}

static void
create_scorewide_block (GtkWidget * vbox, DenemoScoreblock * sb)
{
  GtkWidget *frame = gtk_frame_new (NULL);
  GtkWidget *reload_button = get_reload_button (frame);
  GtkWidget *event_box = get_event_box (vbox);       // event_box is packed into vbox
  gtk_container_add (GTK_CONTAINER (event_box), frame);
  fill_scorewide_frame (frame, reload_button, sb);
  gtk_widget_show_all (vbox);
}

static void
install_movement_widget (DenemoMovement * si, GtkWidget * vbox, DenemoScoreblock ** psb, gchar * partname, gint movement_num, gboolean last, gboolean standard)
{
  DenemoProject *gui = Denemo.project;
  DenemoScoreblock *sb = *psb;  //if(sb)g_print ("Typesetting for id = %d\n\n\n\n", sb->id); else g_print ("No score layout\n\n");
  gchar *label_text = gui->movements->next ? g_strdup_printf (_("<b>Movement %d</b>"), movement_num) : g_strdup (_("Movement"));
  GtkWidget *movement_frame = gtk_expander_new (label_text);
  gtk_label_set_use_markup (GTK_LABEL (gtk_expander_get_label_widget (GTK_EXPANDER (movement_frame))), TRUE);
  gtk_widget_set_tooltip_text (movement_frame, _("This contains the layout of the movement- the movement title, and the actual music itself"));
  gtk_expander_set_expanded (GTK_EXPANDER (movement_frame), si == gui->movement);
  g_free (label_text);
  gtk_box_pack_start (GTK_BOX (vbox), movement_frame, FALSE, TRUE, 0);

  GtkWidget *frame_box = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (movement_frame), frame_box);
  GtkWidget *remove_box = gtk_hbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (frame_box), remove_box, FALSE, FALSE, 0);
  GtkWidget *w = gtk_button_new_with_label ("<span foreground=\"red\">Remove Movement</span>");
  gtk_label_set_use_markup (GTK_LABEL (gtk_bin_get_child (GTK_BIN (w))), TRUE);
  gtk_widget_set_tooltip_text (w, _("Remove this movement from the score layout"));
  g_signal_connect_swapped (w, "clicked", G_CALLBACK (remove_element), frame_box);      //grandparent
  gtk_box_pack_start (GTK_BOX (remove_box), w, FALSE, FALSE, 14);
  // GtkWidget *dummy = gtk_label_new (" dummy ");
  //gtk_box_pack_start (GTK_BOX (frame_box), dummy, TRUE, TRUE, 0);

  GtkWidget *outer_hbox = gtk_hbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (frame_box), outer_hbox, FALSE, TRUE, 0);

  GtkWidget *movement_vbox = gtk_vbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (outer_hbox), movement_vbox, FALSE, TRUE, 10);
  install_pre_movement_widgets (movement_vbox, si, standard, *psb);
  GtkWidget *frame = gtk_frame_new (NULL);
  add_lilypond (frame, g_strdup ("\n\\score { %Start of Movement\n"), g_strdup ("\n       } %End of Movement\n"));
  gtk_box_pack_start (GTK_BOX (movement_vbox), frame, FALSE, TRUE, 0);
  GtkWidget *outer_vbox = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (frame), outer_vbox);
  GtkWidget *hbox = gtk_hbox_new (FALSE, 8);

  gtk_box_pack_start (GTK_BOX (hbox), get_movement_widget (&(*psb)->staff_list, partname, si, movement_num, last, standard, *psb), FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (outer_vbox), hbox, FALSE, TRUE, 0);
  if (si->header.directives)
    {
      GtkWidget *frame = gtk_frame_new (_("Header block"));
      gtk_box_pack_start (GTK_BOX (outer_vbox), frame, FALSE, TRUE, 0);
      add_lilypond (frame, g_strdup ("\n\\header {\n"), g_strdup ("\n        }\n"));
      GtkWidget *innerbox = gtk_vbox_new (FALSE, 8);
      gtk_container_add (GTK_CONTAINER (frame), innerbox);
      GList *g;
      for (g = si->header.directives; g; g = g->next)
        {
          DenemoDirective *d = g->data;
          if (d->override & DENEMO_OVERRIDE_HIDDEN)
            continue;
          if (sb && wrong_layout (d, sb->id))
            continue;
          gchar *lily = (d->postfix && d->postfix->len) ? d->postfix->str : NULL;
          if (lily)
            {
              create_element (innerbox, gtk_button_new_with_label (d->tag->str), g_strdup (lily));
            }
        }
    }
  if (si->layout.directives)
    {
      GtkWidget *frame = gtk_frame_new (_("Layout block"));
      gtk_box_pack_start (GTK_BOX (outer_vbox), frame, FALSE, TRUE, 0);
      add_lilypond (frame, g_strdup ("\n\\layout {\n"), g_strdup ("\n}\n"));
      GtkWidget *innerbox = gtk_vbox_new (FALSE, 8);
      gtk_container_add (GTK_CONTAINER (frame), innerbox);
      GList *g;
      for (g = si->layout.directives; g; g = g->next)
        {
          DenemoDirective *d = g->data;
          if (d->override & DENEMO_OVERRIDE_HIDDEN)
            continue;
          if (sb && wrong_layout (d, sb->id))
            continue;
          gchar *lily = (d->postfix && d->postfix->len) ? d->postfix->str : NULL;
          if (lily)
            {
              create_element (innerbox, gtk_button_new_with_label (d->tag->str), g_strdup (lily));
            }
        }
    }

  if (si->movementcontrol.directives)
    {
      GtkWidget *frame = gtk_frame_new (_("Movement Block"));
      gtk_box_pack_start (GTK_BOX (outer_vbox), frame, FALSE, TRUE, 0);
      GtkWidget *innerbox = gtk_vbox_new (FALSE, 8);
      gtk_container_add (GTK_CONTAINER (frame), innerbox);
      GList *g;
      for (g = si->movementcontrol.directives; g; g = g->next)
        {
          DenemoDirective *d = (DenemoDirective *) g->data;
          if (sb && wrong_layout (d, sb->id))
            continue;
          if (d->override & DENEMO_OVERRIDE_AFFIX && d->postfix)
            {
              gchar *text = label_for_directive (d);
              GtkWidget *label = gtk_label_new (text);
              g_free (text);
              create_element (innerbox, label, g_strdup (d->postfix->str));
            }
        }
    }



  if (si->movementcontrol.directives)
    {
      GtkWidget *frame = gtk_frame_new (_("Movement Epilog"));
      gtk_box_pack_start (GTK_BOX (movement_vbox), frame, FALSE, TRUE, 0);
      GtkWidget *innerbox = gtk_vbox_new (FALSE, 8);
      gtk_container_add (GTK_CONTAINER (frame), innerbox);
      GList *g;
      for (g = si->movementcontrol.directives; g; g = g->next)
        {
          DenemoDirective *d = (DenemoDirective *) g->data;
          if (d->override & DENEMO_OVERRIDE_AFFIX)
            continue;
          if (d->override & DENEMO_OVERRIDE_HIDDEN)
            continue;
          if (sb && wrong_layout (d, sb->id))
            continue;
          if (d->postfix)
            {
              gchar *text = label_for_directive (d);
              GtkWidget *label = gtk_label_new (text);
              g_free (text);
              create_element (innerbox, label, g_strdup (d->postfix->str));
            }
        }
    }
}

static void
install_duplicate_movement (DenemoScoreblock ** psb, gint movement)
{
  gchar *partname = NULL;
  //show_type  (gtk_bin_get_child (gtk_bin_get_child ((*psb)->widget)), "Type of widget ");
  GtkWidget *vbox = gtk_bin_get_child (GTK_BIN (gtk_bin_get_child (GTK_BIN ((*psb)->widget))));
  DenemoMovement *si = g_list_nth_data (Denemo.project->movements, movement - 1);
  if (si)
    install_movement_widget (si, vbox, psb, partname, movement, TRUE, FALSE);
}

static void
install_duplicate_movement_callback (DenemoScoreblock * sb)
{
  install_duplicate_movement (&sb, Denemo.project->movement->currentmovementnum);
  score_status (Denemo.project, TRUE);
  gtk_widget_show_all (sb->widget);
}

static void
reorder_movement (DenemoScoreblock * psb)
{
  //show_type  (gtk_bin_get_child (gtk_bin_get_child ((*psb)->widget)), "Type of widget ");
  GtkWidget *vbox = gtk_bin_get_child (GTK_BIN (gtk_bin_get_child (GTK_BIN (psb->widget))));
  GList *children = gtk_container_get_children (GTK_CONTAINER (vbox));
  //show_type  ( g_list_nth_data (children, movement)     , "Type of widget ");
  for (; children; children = children->next)
    {
      if (GTK_IS_EXPANDER (children->data) && gtk_expander_get_expanded (children->data))
        {
          if (children->next == NULL)
            warningdialog (_("The currently expanded movement is already at the end"));
          else
            gtk_box_reorder_child (GTK_BOX (vbox), children->data, -1); //-1 = to end
          g_list_free (children);
          return;
        }
    }
  warningdialog (_("No movement is expanded - don't know which movement to move"));
}

static void
reorder_movement_callback (DenemoScoreblock * psb)
{
  reorder_movement (psb);
  score_status (Denemo.project, TRUE);

}

 //find the one that is expanded FIXME

//populates the scoreblock *psb with the movement or movements for partname from the current score Denemo.project
static void
set_default_scoreblock (DenemoScoreblock ** psb, gint movement, gchar * partname)
{
  DenemoProject *gui = Denemo.project;
  (*psb)->staff_list = NULL;    //list of staff frames in order they appear in scoreblock

  (*psb)->widget = gtk_scrolled_window_new (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0), gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));

  (*psb)->visible = FALSE;      //will be set true when/if tab is selected
  if (partname)
    (*psb)->partname = g_strdup (partname);
  (*psb)->movement = movement;
  layout_sync = (*psb)->layout_sync = gui->layout_sync;


  GtkWidget *vbox = gtk_vbox_new (FALSE, 8);
 
#if (GTK_MAJOR_VERSION==3 && GTK_MINOR_VERSION<8) 
           gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW ((*psb)->widget), vbox);
#else          
          gtk_container_add (GTK_CONTAINER((*psb)->widget), vbox);
#endif   
  
  
  GtkWidget *options = get_options_button (*psb, FALSE);
  gtk_box_pack_start (GTK_BOX (vbox), options, FALSE, FALSE, 0);
  //now create a hierarchy of widgets representing the score
  create_scorewide_block (vbox, *psb);

  GList *g;
  gint movement_num = 1;

  for (g = gui->movements; g; g = g->next, movement_num++)
    {
      if (movement == 0 /*all movements */  || (movement == movement_num) /*this movement */ )
        {
          DenemoMovement *si = (DenemoMovement *) g->data;
          if (!(movement==0 && si->sketch))
			install_movement_widget (si, vbox, psb, partname, movement_num, !(gboolean) GPOINTER_TO_INT (g->next), TRUE);
        }                       //if movement is wanted
    }                           //for all movements

  for (g = gui->lilycontrol.directives; g; g = g->next)
    {
      DenemoDirective *d = g->data;     // g_print("Trying tag %s postfix %s\n", d->tag->str, d->postfix?d->postfix->str:"No postfix");
      if (d->override & DENEMO_OVERRIDE_HIDDEN)
        continue;
      if (!(d->override & DENEMO_OVERRIDE_AFFIX))
        continue;
      if (wrong_layout (d, (*psb)->id))
        continue;

      gchar *post = (d->postfix && d->postfix->len) ? d->postfix->str : NULL;
      if (post)
        {
          create_element (vbox, gtk_button_new_with_label (d->tag->str), g_strdup (post));
        }
    }


}

//recompute a standard scoreblock if out of date
static void
recreate_standard_scoreblock (DenemoScoreblock ** psb)
{
  if (Denemo.non_interactive) return;
  if ((*psb)->layout_sync == Denemo.project->layout_sync)
    return;
  gint movement = (*psb)->movement;
  gchar *partname = (*psb)->partname ? g_strdup ((*psb)->partname) : NULL;
  gchar *instrumentation = (*psb)->instrumentation ? g_strdup ((*psb)->instrumentation) : NULL;
  gboolean visible = (*psb)->visible;
  GtkNotebook *notebook = GTK_NOTEBOOK (get_score_layout_notebook (Denemo.project));
  if ((*psb)->widget)
    set_notebook_page ((*psb)->widget);
  gint position = gtk_notebook_get_current_page (notebook);
  free_scoreblock ((*psb));     //this changes the page in the notebook if it was selected before. So if sb->visible then re-select this page after reconstruction
  create_standard_scoreblock (psb, movement, partname);
  (*psb)->instrumentation = instrumentation;
  gtk_notebook_reorder_child (notebook, (*psb)->widget, position);
//alternatively pass in desired position to create_standard_scoreblock....


  // if (visible)
  //  gtk_notebook_set_current_page (GTK_NOTEBOOK (get_score_layout_notebook (Denemo.project)), 0);
}

//return value must not be freed
/* UNUSED
static const gchar *
scoreblock_name (DenemoScoreblock * sb)
{
  return gtk_notebook_get_tab_label_text (GTK_NOTEBOOK (get_score_layout_notebook (Denemo.project)), sb->widget);
}
*/


//refreshes the lilypond field of all the standard scoreblocks after re-computing the standard scoreblocks already present
//returns FALSE if structure has not changed since they were computed.
static gboolean
check_for_update (void)
{
  DenemoProject *gui = Denemo.project;
  if (gui->layout_sync > layout_sync)
    {
      GList *g;
      for (g = gui->standard_scoreblocks; g; g = g->next)
        {
          DenemoScoreblock *sb = (DenemoScoreblock *) g->data;
          recreate_standard_scoreblock (&sb);
        }
      return TRUE;
    }
  return FALSE;
}

void update_standard_scoreblocks (void)
{
  //DenemoScoreblock *sb = selected_scoreblock ();
  guint id = selected_layout_id ();
  Denemo.project->layout_sync = layout_sync + 1;
  check_for_update ();
  select_layout_id (id);
}
static gboolean
change_tab (GtkNotebook * notebook, GtkWidget * page, gint pagenum)
{
  //this is getting called with pagenum 0 when clicking on the lilypond text window...
  // Denemo.project->lilysync = G_MAXUINT; this would trigger off a re-typeset in continuous typesetting. It seems to happen *before the code below is executed.
  // g_print ("Page num %d widget 0x%x\n", pagenum, page);
  page = gtk_notebook_get_nth_page (notebook, pagenum); // value passed in appears to be something else - it is not documented what.
//g_print ("gtk_notebook_get_nth_page  %d widget 0x%x\n", pagenum, page);
  GList *g;
  for (g = Denemo.project->custom_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = ((DenemoScoreblock *) g->data);
      sb->visible = (sb->widget == page);       //g_print ("%s for layout 0x%x\n", sb->visible?"Visible":"Invisible", sb->id);
      if (sb->visible)
        Denemo.project->layout_id = sb->id;
    }
  for (g = Denemo.project->standard_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = ((DenemoScoreblock *) g->data);
      sb->visible = (sb->widget == page);       //g_print ("%s for layout 0x%x\n", sb->visible?"Visible":"Invisible", sb->id);
      if (sb->visible)
        Denemo.project->layout_id = sb->id;
    }
  // Denemo.project->layout_id = 0;
  Denemo.project->lilysync = G_MAXUINT;
  return TRUE;
}


//takes a DenemoScoreblock that has a valid widget field and recomputes the lilypond field of the scoreblock
//from the widget. It also sets the name field of the scoreblock to the name on the Notebook tab.
// It sets the instrumentation if set in the scoreblock structure.
void
refresh_lilypond (DenemoScoreblock * sb)
{
  if (sb->widget)
    {

      if ((!is_lilypond_text_layout (sb)))
        {
          gchar *instrumentation = sb->instrumentation;
          gchar *set_instr = instrumentation ? g_strdup (instrumentation) : ((!strcmp (sb->name, DEFAULT_SCORE_LAYOUT)) ? g_strdup (_("Full Score")) : ((g_str_has_prefix (sb->name, _("Movement"))) ? g_strdup (sb->name) : NULL));

          instrumentation = set_instr ? g_strdup_printf ("        instrumentation = \\markup { \\with-url #'\"scheme:(d-BookInstrumentation)\" \"%s\"}\n", set_instr) : g_strdup ("");
          g_free (set_instr);
          set_instr = instrumentation;

          sb->id = crc32 ((guchar *) sb->name);
          if (sb->lilypond == NULL)
            sb->lilypond = g_string_new (sb->name);
          else
            g_string_assign (sb->lilypond, sb->name);
          g_string_prepend (sb->lilypond, "%");
          g_string_append_printf (sb->lilypond, "\n\\header{DenemoLayoutName = \"%s\"\n%s        }\n", sb->name, set_instr);
          g_free (set_instr);
          lilypond_for_layout (sb->lilypond, sb->widget);
        }
    }
  else
    g_warning ("No widget for scoreblock");
}

gboolean
current_scoreblock_is_custom (void)
{
  GtkWidget *notebook = get_score_layout_notebook (Denemo.project);
  gint pagenum = gtk_notebook_get_current_page (GTK_NOTEBOOK (notebook));
  GtkWidget *page = gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook), pagenum);
  GList *g;
  for (g = Denemo.project->custom_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = ((DenemoScoreblock *) g->data);
      if (sb->widget == page)
        return TRUE;
    }
  return FALSE;
}

DenemoScoreblock *
selected_scoreblock (void)
{
  if (Denemo.non_interactive) 
    {
      GList *g;
      //g_print ("Layout id %d scoreblocks %p and %p\n\n", Denemo.project->layout_id, Denemo.project->custom_scoreblocks, Denemo.project->standard_scoreblocks);
      for (g = Denemo.project->custom_scoreblocks; g; g = g->next)
      {
        DenemoScoreblock *sb = ((DenemoScoreblock *) g->data);
        if (sb->id == Denemo.project->layout_id)
            return sb;
      }
      for (g = Denemo.project->standard_scoreblocks; g; g = g->next)
      {
        DenemoScoreblock *sb = ((DenemoScoreblock *) g->data);
        if (sb->id == Denemo.project->layout_id)
            return sb;
      }
    create_default_scoreblock ();// does not necessarily create a standard scoreblock, can be a custom scoreblock
    return (Denemo.project->standard_scoreblocks? (DenemoScoreblock *) (Denemo.project->standard_scoreblocks->data):
                                               (DenemoScoreblock *) (Denemo.project->custom_scoreblocks->data));
    }

  GtkWidget *notebook = get_score_layout_notebook (Denemo.project);
  gint pagenum = gtk_notebook_get_current_page (GTK_NOTEBOOK (notebook));       // value passed in appears to be something else - it is not documented what.
  GtkWidget *page = gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook), pagenum);
  GList *g;
  for (g = Denemo.project->custom_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = ((DenemoScoreblock *) g->data);
      if (sb->widget == page)
        {
          refresh_lilypond (sb);        //!!!! needs sorting out !!!
          return sb;
        }
    }
  for (g = Denemo.project->standard_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = ((DenemoScoreblock *) g->data);
      if (sb->widget == page)
        {
          refresh_lilypond (sb);
          return sb;
        }
    }
  return NULL;
}

//returns a uri for the pdf output for the current scoreblock. The user must free when done.
gchar *
get_output_uri_from_scoreblock (void)
{
  DenemoScoreblock *sb = selected_scoreblock ();
  if (sb == NULL)
    {
      g_warning ("No Score Layout");
      return g_strdup ("");
    }
  DenemoProject *gui = Denemo.project;
  if (sb->uri)
    return g_strdup (sb->uri);
  gchar *basename;
  gchar *dirname;
  if (Denemo.project->filename && Denemo.project->filename->len)
    {
      gchar *filename = gui->filename->str;
      dirname = g_path_get_dirname (filename);
      basename = g_path_get_basename (filename);
      gchar *suffix = g_strrstr (basename, DENEMO_FILE_SUFFIX);
      if (suffix)
        *suffix = 0;
    }
  else
    {
      basename = g_strdup ("output");
      dirname = g_get_current_dir ();
    }
  gchar *uri = g_strdup_printf ("file://%s", dirname);
  g_free (dirname);
  gchar *ret;
  if (sb)
    {
      gchar *pdf_name = g_strconcat (basename, "-", sb->name, ".pdf", NULL);
      ret = g_build_filename (uri, pdf_name, NULL);
      g_free (pdf_name);
    }
  else
    {
      ret = g_build_filename (uri, "output.pdf", NULL);
    }
  g_free (basename);
  g_free (uri);
  return ret;
}

void
set_current_scoreblock_uri (gchar * uri)
{
  DenemoScoreblock *sb = selected_scoreblock ();
  if (sb)
    {
      g_free (sb->uri);
      sb->uri = uri;
    }
}

//Returns the next scoreblock in the score layout notebook, or NULL if it is the last
DenemoScoreblock *
get_next_scoreblock (void)
{
  GtkWidget *notebook = get_score_layout_notebook (Denemo.project);
  gint pagenum = gtk_notebook_get_current_page (GTK_NOTEBOOK (notebook));
  GtkWidget *page = gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook), pagenum + 1);
  if (page)
    gtk_notebook_set_current_page (GTK_NOTEBOOK (notebook), pagenum + 1);
  GList *g;
  for (g = Denemo.project->custom_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = ((DenemoScoreblock *) g->data);
      if (sb->widget == page)
        {
          refresh_lilypond (sb);        //!!!! needs sorting out !!!
          return sb;
        }
    }
  for (g = Denemo.project->standard_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = ((DenemoScoreblock *) g->data);
      if (sb->widget == page)
        {
          refresh_lilypond (sb);
          return sb;
        }
    }
  return NULL;
}

//Returns the next scoreblock in the score layout notebook, or NULL if it is the last
DenemoScoreblock *
get_first_scoreblock (void)
{
  GtkWidget *notebook = get_score_layout_notebook (Denemo.project);
  gtk_notebook_set_current_page (GTK_NOTEBOOK (notebook), 0);
  GtkWidget *page = gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook), 0);
  GList *g;
  for (g = Denemo.project->custom_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = ((DenemoScoreblock *) g->data);
      if (sb->widget == page)
        {
          refresh_lilypond (sb);
          return sb;
        }
    }
  for (g = Denemo.project->standard_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = ((DenemoScoreblock *) g->data);
      if (sb->widget == page)
        {
          refresh_lilypond (sb);
          return sb;
        }
    }
  return NULL;
}


gboolean
iterate_custom_layout (gboolean init)
{                               //!!!!!!!!problem for lilypondized ones is widget NULL????
  static gint current;
  DenemoScoreblock *sb;
  if (Denemo.project->custom_scoreblocks == NULL)
    {
      return FALSE;
    }
  if (init)
    {
      current = 0;
      sb = (DenemoScoreblock *) (Denemo.project->custom_scoreblocks->data);
    }
  else
    {
      current++;
      sb = (DenemoScoreblock *) g_list_nth_data (Denemo.project->custom_scoreblocks, current);
    }
  if (sb && sb->widget)
    {
      if (!Denemo.non_interactive)
        {
          if (!gtk_widget_get_visible (Denemo.project->score_layout))
            set_toggle ("ToggleScoreLayout", TRUE);
          set_notebook_page (sb->widget);
        }
      return TRUE;
    }
  else
    {
      if (Denemo.non_interactive)
        {
          Denemo.project->layout_id = sb->id;//this is the bit of set_notebook_page() that is needed for non-interactive case I think
          return TRUE;
        }
      g_debug ("No custom layout %d sb = %p\n", current, sb);
      return FALSE;
    }
}

guint
get_layout_id_for_name (gchar * name)
{
  return crc32 (name);
}

guint
selected_layout_id (void)
{
  if (Denemo.project->layout_id == 0)
    {
      DenemoScoreblock *sb = selected_scoreblock ();
      if (sb)
        Denemo.project->layout_id = sb->id;
    }
  return Denemo.project->layout_id;
}

GtkWidget *
get_score_layout_notebook (DenemoProject * gui)
{
  if (Denemo.non_interactive)
    return NULL;
  GtkWidget *notebook = gtk_bin_get_child (GTK_BIN (gui->score_layout));
  if (notebook == NULL)
    {
      notebook = gtk_notebook_new ();
      g_signal_connect (notebook, "switch_page", G_CALLBACK (change_tab), NULL);
      g_signal_connect (gui->score_layout, "focus-in-event", G_CALLBACK (check_for_update), NULL);
      gtk_container_add (GTK_CONTAINER (gui->score_layout), notebook);
    }
  return notebook;
}

//create a standard scoreblock in the passed DenemoScoreblock structure and put it in a new tab in the score
//does not add it to the standard scoreblocks list
static void
create_standard_scoreblock (DenemoScoreblock ** psb, gint movement, gchar * partname)
{
  DenemoProject *gui = Denemo.project;
  gchar *label_text = movement_part_name (movement, partname);
  (*psb)->name = g_strdup (label_text);
  Denemo.project->layout_id = (*psb)->id = crc32 ((guchar *) (*psb)->name);
  set_default_scoreblock (psb, movement, partname);
  if (!Denemo.non_interactive)
    {
    GtkWidget *notebook = get_score_layout_notebook (gui);
    GtkWidget *label = gtk_label_new (label_text);
    g_free (label_text);
    gtk_notebook_prepend_page (GTK_NOTEBOOK (notebook), (*psb)->widget, label);
    gtk_widget_set_tooltip_markup ((*psb)->widget,
                                   _
                                   ("This is a score layout - the buttons mostly customize the layout\nYou can have several layouts and use them to print different versions of your score.\nOnce customized e.g. by adding page breaks, deleting certain parts etc the layout will be saved with your score and can be used for printing from even though you may have made corrections to the music.\nStandard layouts are created by invoking the standard print commands - print, print part, print movement etc.\nThese standard layouts provide a convenient starting point for your customized layouts.<b>Note 1</b>Custom layouts are not saved for further graphical editing, only the typesetting commands are saved, so, unless you are familiar with LilyPond do all your work on the layout in one session.<b>Note 2</b>The first comment in the LilyPond text of the layout holds the name of the layout. If you change it any conditional directives that are for the layout will need refreshing"));
    gtk_widget_show_all (notebook);
    }
}

static void
set_notebook_page (GtkWidget * w)
{
  GtkWidget *notebook = get_score_layout_notebook (Denemo.project);
  GList *g = gtk_container_get_children (GTK_CONTAINER (notebook));
  gint position = g_list_index (g, w);
  g_list_free (g);
  gtk_notebook_set_current_page (GTK_NOTEBOOK (notebook), position);
}

void
create_default_scoreblock (void)
{
  DenemoProject *gui = Denemo.project;
  if (gui->custom_scoreblocks)
    {
      GList *g;
      for (g = gui->custom_scoreblocks; g; g = g->next)
        {
          DenemoScoreblock *sb = g->data;
          if (!strcmp (sb->name, DEFAULT_SCORE_LAYOUT))
            {
              set_notebook_page (sb->widget);
              return;
            }
        }
    }
  if (gui->standard_scoreblocks)
    {
      GList *g;
      for (g = gui->standard_scoreblocks; g; g = g->next)
        {
          DenemoScoreblock *sb = g->data;
          if (!strcmp (sb->name, DEFAULT_SCORE_LAYOUT))
            {
              set_notebook_page (sb->widget);
              return;
            }
        }
    }
  DenemoScoreblock *sb = g_malloc0 (sizeof (DenemoScoreblock));
  (void) create_standard_scoreblock (&sb, 0, NULL);
  gui->standard_scoreblocks = g_list_prepend (gui->standard_scoreblocks, (gpointer) sb);
}

/* select the scoreblock with the standard default scoreblock name, choosing a customized
 * version over any standard version. Create one if it does not exist.
 */
void
select_default_scoreblock (void)
{
  DenemoProject *gui = Denemo.project;
  if (gui->custom_scoreblocks)
    {
      GList *g;
      for (g = gui->custom_scoreblocks; g; g = g->next)
        {
          DenemoScoreblock *sb = g->data;
          if (!strcmp (sb->name, DEFAULT_SCORE_LAYOUT))
            {
              set_notebook_page (sb->widget);
              return;
            }
        }
    }
  if (gui->standard_scoreblocks)
    {
      GList *g;
      for (g = gui->standard_scoreblocks; g; g = g->next)
        {
          DenemoScoreblock *sb = g->data;
          if (!strcmp (sb->name, DEFAULT_SCORE_LAYOUT))
            {
              set_notebook_page (sb->widget);
              return;
            }
        }
    }
  create_default_scoreblock ();
  if (gui->standard_scoreblocks)
    {
      GList *g;
      for (g = gui->standard_scoreblocks; g; g = g->next)
        {
          DenemoScoreblock *sb = g->data;
          if (!strcmp (sb->name, DEFAULT_SCORE_LAYOUT))
            {
              set_notebook_page (sb->widget);
              return;
            }
        }
    }
}

static void
selection_install_voice (DenemoStaff * staff, gint movementnum, gint voice_count, GString * lilypond, GString * tail, GString * voice_tail)
{
  gchar *voicetag = get_voicetag (movementnum, voice_count);
  gchar *voicename = get_voicename (movementnum, voice_count);
  gchar *text1;


  GString *voicetext = g_string_new ("");
  set_voice_definition (voicetext, staff, voicetag);    //That is \new Voice = name prefix { postfix FIXME is prefix any use here????
  gchar *text = g_strdup_printf (" %s ", voicetext->str);
  g_string_assign (voicetext, "");

  set_voice_termination (voice_tail, staff);    // TAB TAB"} %End of voice" if not overridden



  g_string_append (lilypond, text);

  text1 = g_strdup_printf (" \\%s", voicename);
  if (staff->voicecontrol == DENEMO_PRIMARY)
    {
      g_string_append (lilypond, get_lilypond_for_clef (&staff->clef));
      g_string_append (lilypond, get_lilypond_for_keysig (&staff->keysig));
      g_string_append (lilypond, get_lilypond_for_timesig (&staff->timesig));
    }
  g_string_append (lilypond, text1);
  g_free (text1);


  g_string_prepend (tail, g_string_free (voicetext, FALSE));
}

//returns a layout with no widget whose lilypond is the scoreblock for just the selected staffs
DenemoScoreblock *
selection_layout (void)
{
  DenemoProject *gui = Denemo.project;
  DenemoMovement *si = gui->movement;
  GString *movement_tail = g_string_new ("");
  gint movementnum = g_list_index (Denemo.project->movements, Denemo.project->movement) + 1;
  static DenemoScoreblock *sb;
  if (sb == NULL)
    {
      sb = g_malloc0 (sizeof (DenemoScoreblock));
      sb->lilypond = g_string_new ("");
    }

  g_string_assign (sb->lilypond, "\n\\score\n{ %Start of Selection from current movement\n");
  set_initiate_scoreblock (si, sb->lilypond);   // ie << possibly overridden

  GList *g;                     //things like transpose whole score etc
  for (g = gui->lilycontrol.directives; g; g = g->next)
    {
      DenemoDirective *d = g->data;
      if (d->override & DENEMO_OVERRIDE_HIDDEN)
        continue;
      if (d->override & DENEMO_OVERRIDE_AFFIX)
        continue;               //g_print ("Trying tag %s with postfix %s at %x\n", d->tag->str, (d->postfix && d->postfix->len) ? d->postfix->str :"", selected_layout_id ());
      if (wrong_layout (d, sb->id))
        continue;

      gchar *start = (d->postfix && d->postfix->len) ? d->postfix->str : NULL;
      if (start)
        {
          g_string_append_printf (sb->lilypond, "\n << %s\n  << ", start);
          g_string_prepend (movement_tail, "\n >>\n  >>");
        }
    }
  gint voice_count;
  for (voice_count = 1, g = gui->movement->thescore; g; g ? g = g->next : g, voice_count++)
    {
      DenemoStaff *staff = g->data;
      DenemoStaff *nextstaff = g->next ? g->next->data : NULL;
      GString *stafftext = g_string_new ("");
      if (!(voice_count >= gui->movement->selection.firststaffmarked && voice_count <= gui->movement->selection.laststaffmarked))
        continue;
      if (staff->hasfakechords)
        {                       //the reason these are outside the staff frame is it makes them appear above the staff
          g_string_append_printf (sb->lilypond, "\n" TAB TAB "\\new ChordNames \\chordmode { \\%sChords }\n", get_voicename (movementnum, voice_count));
        }
      set_staff_definition (sb->lilypond, staff);




      //if (staff->no_of_lines != 5)
      //  g_string_append_printf (sb->lilypond, TAB "\\override Staff.StaffSymbol  #'line-count = #%d\n", staff->no_of_lines);    //FIXME create_element
      GString *tail = g_string_new ("");
      GString *voice_tail = g_string_new ("");
      g_string_assign (stafftext, "");
      set_staff_termination (stafftext, staff); // "\n>>\n%End of Staff\n"

      g_string_prepend (tail, g_string_free (stafftext, FALSE));

      if (staff->hasfigures)
        {
          g_string_append_printf (sb->lilypond, "\n" TAB TAB "\\context Staff \\with {implicitBassFigures = #'(0) } \\%sBassFiguresLine %%End of bass figures\n", get_voicename (movementnum, voice_count));
        }

      selection_install_voice (staff, movementnum, voice_count, sb->lilypond, tail, voice_tail);        //Primary voice

      g_string_append (sb->lilypond, voice_tail->str);
      g_string_assign (voice_tail, "");
      //selection_do_verses(staff, vbox, movementnum, this is repeated below
      gboolean voices_intervened;
      voices_intervened = FALSE;
      if (nextstaff && (nextstaff->voicecontrol & DENEMO_SECONDARY))
        {
          for (g = g->next, voice_count++; g && (((DenemoStaff *) g->data)->voicecontrol & DENEMO_SECONDARY); g = g->next, voice_count++)
            {
              GString *voicetail = g_string_new ("");
              DenemoStaff *staff = g->data;
              selection_install_voice (staff, movementnum, voice_count, sb->lilypond, voicetail, voice_tail);
              g_string_append (sb->lilypond, g_string_free (voicetail, FALSE));
              g_string_append (sb->lilypond, voice_tail->str);
              g_string_assign (voice_tail, "");
              voices_intervened = TRUE;
              //selection_do_verses(staff, vbox, movementnum, this is repeated above
            }
        }

      g_string_free (voice_tail, TRUE);

      if (tail->len)
        g_string_append (sb->lilypond, g_string_free (tail, FALSE));
      else
        g_string_free (tail, FALSE);

      if (g && voices_intervened)       // we have added voices to the staff, so the for loop has advanced the iterator over staffs and voice count already, so back up
        {
          g = g->prev;
          voice_count--;
        }
    }                           // end of for each staff Now loop back for all the staffs in firststaffnum -  laststaffnum
  g_string_append (sb->lilypond, movement_tail->str);
  g_string_free (movement_tail, TRUE);
  g_string_append (sb->lilypond, "\n          >>\n");
  g_string_append_printf (sb->lilypond, "\n\\header {\n");

  for (g = si->header.directives; g; g = g->next)
    {
      DenemoDirective *d = g->data;
      if (d->override & DENEMO_OVERRIDE_HIDDEN)
        continue;
      if (wrong_layout (d, sb->id))
        continue;

      gchar *lily = (d->postfix && d->postfix->len) ? d->postfix->str : NULL;
      if (lily)
        {
          g_string_append (sb->lilypond, lily);
        }
    }

  g_string_append (sb->lilypond, "\n}\n");



  if (si->layout.directives)
    {
      g_string_append (sb->lilypond, "\n\\layout {\n");
      for (g = si->layout.directives; g; g = g->next)
        {
          DenemoDirective *d = g->data;
          if (d->override & DENEMO_OVERRIDE_HIDDEN)
            continue;
          if (wrong_layout (d, sb->id))
            continue;
          gchar *lily = (d->postfix && d->postfix->len) ? d->postfix->str : NULL;
          if (lily)
            {
              g_string_append (sb->lilypond, lily);
            }
        }
      g_string_append (sb->lilypond, ("\n}\n"));
    }
  g_string_append (sb->lilypond, "\n} %End of Movement\n");
  return sb;
}

//if the call is all_movements is current (1) and no partname ie default, then current layout is returned, re-created (if need be) if it is a standard one
//otherwise selects or creates a standard layout for the given spec: all_movements (0=all, 1 = current) and part (NULL is all parts, otherwise parts with partname).
DenemoScoreblock *
select_layout (gboolean all_movements, gchar * partname, gchar * instrumentation)
{
  GList *g;
  DenemoScoreblock *sb;//g_print ("select_layout %s\n", partname);
  if (Denemo.project->movement->markstaffnum)
    return selection_layout ();

  if (all_movements && partname == NULL)
    {
      sb = selected_scoreblock ();
      if (sb)
        { //g_print ("selected scoreblock %s\n\n", sb->name);
          if (is_in_standard_scoreblock (sb))
            {
              recreate_standard_scoreblock (&sb);
              refresh_lilypond (sb);
              //g_print ("refreshed scoreblock %s\n\n", sb->name);
            }
          set_notebook_page (sb->widget);
          return sb;
        }
    }


//otherwise return a standard scoreblock recreating it - though this should only need doing if changecount has moved on

  //make sure at least the default scoreblock has been created, this can now be a custom version named with default scoreblock name
  if (Denemo.project->standard_scoreblocks == NULL)
    {
      create_default_scoreblock ();
      if (Denemo.project->standard_scoreblocks)
        sb = (DenemoScoreblock *) (Denemo.project->standard_scoreblocks->data);
      else if (Denemo.project->custom_scoreblocks)
        sb = (DenemoScoreblock *) (Denemo.project->custom_scoreblocks->data);
      else
        {
          g_critical ("No score layout available");
          return NULL;
        }

      refresh_lilypond (sb);    //creating a scoreblock does *not* include generating the lilypond from its widgets.
    }



//first recreate all the standard scoreblocks and set them not visible
  for (g = Denemo.project->standard_scoreblocks; g; g = g->next)
    {
      sb = (DenemoScoreblock *) g->data;
      // if(sb->layout_sync!=Denemo.project->layout_sync) //this conditional was dropped, it will be better to fix the cases where project->layout_sync is not updated as it should, that is places where signal_structural_change() call is missed
      recreate_standard_scoreblock (&sb);
      sb->visible = FALSE;
    }

  if (all_movements && partname == NULL)
    {                           //select the one for the whole score
      for (g = Denemo.project->standard_scoreblocks; g; g = g->next)
        {
          sb = (DenemoScoreblock *) g->data;
          if ((sb->movement == 0) && (sb->partname == NULL))
            {
              sb->visible = TRUE;
              refresh_lilypond (sb);
              set_notebook_page (sb->widget);
              return sb;
            }
        }
      if (Denemo.project->custom_scoreblocks)
        {
          sb = (DenemoScoreblock *) (Denemo.project->custom_scoreblocks->data);
          return sb;
        }
      g_warning ("Error in logic: the default standard scoreblock should exist or a custom one of that name ");
    }
  else
    {                           //Not a whole score print
      for (g = Denemo.project->standard_scoreblocks; g; g = g->next)
        {
          sb = (DenemoScoreblock *) g->data;

          //a good match to partname ?
          gboolean good = (sb->partname && partname && !strcmp (sb->partname, partname)) || (sb->partname == NULL && partname == NULL);

          if (good && (all_movements && (sb->movement == 0)))
            {                   //scoreblock is for good partname and is for all movements - use it
              sb->visible = TRUE;
              refresh_lilypond (sb);
              set_notebook_page (sb->widget);
              return sb;
            }
        }
    }

//either just the current movement or just the part named or both. Set up the movement number (1 ...) or 0 for the all movements case
  gint movement;
  if (all_movements)
    {
      movement = 0;
    }
  else
    {
      movement = g_list_index (Denemo.project->movements, Denemo.project->movement) + 1;        //current movement
    }


  if (movement || partname)
    {                           //a specific movement and/or a specific part
      for (g = Denemo.project->standard_scoreblocks; g; g = g->next)
        {
          sb = (DenemoScoreblock *) g->data;
          if ((movement == sb->movement) && ((partname == sb->partname) || (partname && sb->partname && !strcmp (sb->partname, partname))))
            {
              sb->visible = TRUE;
              refresh_lilypond (sb);
              set_notebook_page (sb->widget);
              return sb;
            }
        }
      sb = g_malloc0 (sizeof (DenemoScoreblock));
      create_standard_scoreblock (&sb, movement, partname);
      Denemo.project->standard_scoreblocks = g_list_prepend (Denemo.project->standard_scoreblocks, sb);
      sb->visible = TRUE;
      sb->instrumentation = g_strdup (instrumentation); //g_print ("instrumentation %s\n", sb->instrumentation);
      refresh_lilypond (sb);
      set_notebook_page (sb->widget);
      return sb;
    }
  //NOT REACHED
  g_warning ("Error in logic: the default standard scoreblock should exist and be returned ");
  return sb;                    //this is the last in the list of standard scoreblocks but cannot be reached
}

void
select_standard_layout (DenemoScoreblock * sb)
{
  if (Denemo.project->standard_scoreblocks == NULL)
    {
      create_default_scoreblock ();
      //creating a scoreblock does *not* include generating the lilypond from its widgets.
      if (Denemo.project->standard_scoreblocks == NULL)
        {
          DenemoScoreblock *sb = g_malloc0 (sizeof (DenemoScoreblock));
          (void) create_standard_scoreblock (&sb, 0, NULL);
          Denemo.project->standard_scoreblocks = g_list_prepend (NULL, (gpointer) sb);
        }
      sb = (DenemoScoreblock *) (Denemo.project->standard_scoreblocks->data);
    }
  refresh_lilypond (sb);
  set_notebook_page (sb->widget);
}

void
select_custom_layout (DenemoScoreblock * sb)
{
  if (Denemo.project->custom_scoreblocks == NULL)
    {
      return;
    }
  set_notebook_page (sb->widget);
}

gboolean
select_custom_layout_for_name (gchar * name)
{
  GList *g = Denemo.project->custom_scoreblocks;
  for (; g; g = g->next)
    {
      DenemoScoreblock *sb = g->data;

      if (sb->name && !strcmp (name, sb->name))
        return TRUE;
    }
  return FALSE;
}

gboolean
select_layout_id (gint id)
{
  GList *g = Denemo.project->custom_scoreblocks;
  for (; g; g = g->next)
    {
      DenemoScoreblock *sb = g->data;
      if (sb->id == id)
        {
          set_notebook_page (sb->widget);
          return TRUE;
        }
    }

  for (g = Denemo.project->standard_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = g->data;
      if (sb->id == id)
        {
          set_notebook_page (sb->widget);
          return TRUE;
        }
    }
  return FALSE;
}

/* UNUSED
static void
text_modified (GtkTextBuffer * textbuffer, DenemoScoreblock * sb)
{
  GtkTextIter startiter, enditer;
  gtk_text_buffer_get_start_iter (textbuffer, &startiter);
  gtk_text_buffer_get_end_iter (textbuffer, &enditer);
  gchar *text = gtk_text_buffer_get_text (textbuffer, &startiter, &enditer, FALSE);
  if (sb->lilypond)
    g_string_assign (sb->lilypond, text);
  else
    sb->lilypond = g_string_new (text);
  score_status (Denemo.project, TRUE);
}
*/

DenemoScoreblock *
get_scoreblock_for_lilypond (gchar * lily)
{
  gchar *text = _("The LilyPond text for this layout can be edited in the LilyPond view window.\nYou can safely delete this layout if you no longer need it\n(for example if you have made structural changes to the score\nnot reflected in this layout).");
  gchar *name = NULL;
  DenemoScoreblock *sb = g_malloc0 (sizeof (DenemoScoreblock));
  sb->text_only = TRUE;

  if (!Denemo.non_interactive)
    {
      GtkWidget *frame = gtk_frame_new (LILYPOND_TEXT_EDITOR);
      sb->widget = frame;
      gtk_widget_set_tooltip_text (frame, _("This is a customized layout, which has been transformed into instructions for the LilyPond music typesetter.\nThis is the form in which customized layouts are stored in a Denemo score on disk - the graphical interface is no longer available. You can, however still edit the layout with care (and some understanding of LilyPond).\nUse the View → LilyPond window to do this.\nOtherwise you can delete it and create a new one from a standard layout."));
      GtkWidget *vbox = gtk_vbox_new (FALSE, 8);
      gtk_container_add (GTK_CONTAINER (sb->widget), vbox);
      GtkWidget *options = get_options_button (sb, TRUE);
      gtk_box_pack_start (GTK_BOX (vbox), options, FALSE, FALSE, 0);
      GtkWidget *textview = gtk_text_view_new ();
      gtk_text_view_set_cursor_visible (GTK_TEXT_VIEW (textview), TRUE);
      GtkTextBuffer *textbuffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (textview));
      gtk_text_buffer_set_text (textbuffer, text, -1);

      GtkWidget *sw = gtk_scrolled_window_new (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0), gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
      gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
      gtk_container_add (GTK_CONTAINER (sw), textview);
      gtk_box_pack_start (GTK_BOX (vbox), sw, TRUE, TRUE, 0);
    }

  gchar *newline = g_strstr_len (lily, -1, "\n");
  if (newline)
    {
      name = g_strndup (lily, newline - lily);
      sb->name = g_strdup (name + 1);
      sb->id = crc32 ((guchar *) sb->name);
      g_free (name);
    }
  else
    sb->name = g_strdup (_("Custom Scoreblock"));
  sb->id = crc32 ((guchar *) sb->name);
  sb->lilypond = g_string_new (lily);

  refresh_lilypond (sb);
  return sb;
}

//if the score_layout window is visible and a standard scoreblock is selected, create a custom one cloned from it with the passed name
DenemoScoreblock *
create_custom_scoreblock (gchar * layout_name, gboolean force)
{
  GList *g;
  if (!force && !gtk_widget_get_visible (Denemo.project->score_layout))
    return NULL;
  for (g = Denemo.project->custom_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = (DenemoScoreblock *) g->data;
      if (!strcmp (layout_name, sb->name))
        return NULL;
    }
  for (g = Denemo.project->standard_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = (DenemoScoreblock *) g->data;
      if (sb->visible)
        {
          if (clone_scoreblock (sb, layout_name))
            return sb;
        }
    }
  return NULL;
}


gboolean
delete_custom_scoreblock (gchar * layout_name)
{
  GList *g;

  for (g = Denemo.project->custom_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = (DenemoScoreblock *) g->data;
      if (!strcmp (layout_name, sb->name))
        {
          delete_custom_scoreblock_callback (NULL, sb);
          return TRUE;
        }
    }
  return FALSE;
}

DenemoScoreblock *
create_custom_lilypond_scoreblock (void)
{
  //called for
  //make_scoreblock_editable(); in view.c
  DenemoScoreblock *sb = NULL;
  GList *g;
  for (g = Denemo.project->custom_scoreblocks; g; g = g->next)
    {
      sb = (DenemoScoreblock *) g->data;
      if (sb->visible)
        {
          if (!sb->text_only)
            convert_to_lilypond_callback (NULL, sb);
          return sb;
        }
    }
  for (g = Denemo.project->standard_scoreblocks; g; g = g->next)
    {
      sb = (DenemoScoreblock *) g->data;
      if (sb->visible)
        {
          if (!sb->text_only)
            convert_to_lilypond_callback (NULL, sb);
          return sb;
        }
    }
  //if none, create the default and convert that to lilypoond.
  create_default_scoreblock (); // does not necessarily create a standard scoreblock, can be a custom scoreblock
  sb =  (Denemo.project->standard_scoreblocks? (DenemoScoreblock *) (Denemo.project->standard_scoreblocks->data):
                                               (DenemoScoreblock *) (Denemo.project->custom_scoreblocks->data));
  convert_to_lilypond_callback (NULL, sb);
  return sb;
}


static GtkWidget *LayoutMenu;   //a menu for the default layout and all created layouts
static void
typeset_layout (DenemoScoreblock * sb)
{
#ifndef USE_EVINCE
  g_debug ("This feature requires denemo to be built with evince");
#else
  set_notebook_page (sb->widget);
  g_debug ("Switched to %s\n", sb->name);
  typeset_current_layout ();
#endif
}

static void
remove_menuitem (GtkWidget * menuitem, GtkContainer * container)
{
  gtk_container_remove (container, menuitem);
}

static void
attach_item (DenemoScoreblock * sb)
{
  GtkWidget *menuitem = gtk_menu_item_new_with_label (sb->name);
  gtk_widget_set_tooltip_text (menuitem, _("Typesets this layout"));
  g_signal_connect_swapped (menuitem, "activate", G_CALLBACK (typeset_layout), sb);
  gtk_menu_shell_append (GTK_MENU_SHELL (LayoutMenu), menuitem);
}

GtkWidget *
GetLayoutMenu (void)
{
  GList *g;
  if (LayoutMenu == NULL)
    {
      LayoutMenu = gtk_menu_new ();
    }
  else
    {
      gtk_container_foreach (GTK_CONTAINER (LayoutMenu), (GtkCallback) remove_menuitem, LayoutMenu);
    }
  if (Denemo.project->standard_scoreblocks == NULL)
    create_default_scoreblock ();
  for (g = Denemo.project->standard_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = (DenemoScoreblock *) g->data;
      attach_item (sb);
    }
  for (g = Denemo.project->custom_scoreblocks; g; g = g->next)
    {
      DenemoScoreblock *sb = (DenemoScoreblock *) g->data;
      attach_item (sb);
    }

  gtk_widget_show_all (LayoutMenu);
  return LayoutMenu;
}
