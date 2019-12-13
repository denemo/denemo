/*
 * palettes.c
 *
 * Copyright 2013 Richard Shann <rshann@virgin.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 */
#include "ui/palettes.h"
#include "core/view.h"
#include "core/utils.h"
#include "core/menusystem.h"
#include "ui/texteditors.h"
#include <librsvg/rsvg.h>


#if GTK_MAJOR_VERSION==2
#define gtk_grid_new() (pal->rows?gtk_hbox_new (FALSE, 1):gtk_vbox_new (FALSE, 1))
#define gtk_grid_attach(widget, obj, a,b,c,d) gtk_box_pack_start(widget, obj, FALSE, TRUE, 0)
#define GTK_GRID(a) a
#endif
static void hide_palette_widget (GtkWidget *w) {
    GtkWidget *parent = gtk_widget_get_parent (w);
    if(GTK_IS_WINDOW(parent))
        gtk_widget_hide (parent);
    else
       gtk_widget_hide (w);
}
static void hide_docked_palettes (void) {
    GList *g;
    for (g=Denemo.palettes;g;g=g->next)
    {
            DenemoPalette *pal = g->data;
            if(pal->docked)
                hide_palette_widget (pal->box);
    }
}

static void hide_all_palettes (void) {
    GList *g;
    for (g=Denemo.palettes;g;g=g->next)
    {
            DenemoPalette *pal = g->data;
            hide_palette_widget (pal->box);
    }
}

static void destroy_all_palettes (void) {
    if(confirm(_("Destroy All Palettes"), _("Get rid of all palettes permanently?")))
    {
        hide_all_palettes();
        Denemo.palettes = NULL;//on exit an empty palettes.xml will be written
    }
}
static void position_palette_menu (GtkMenu *menu, gint *x, gint *y, gboolean *push_in)
{
    *x = *y = 0;
    *push_in = TRUE;
    
}

static void popupmenu (GtkWidget *menu) {
      g_signal_connect (menu, "selection-done", gtk_main_quit, NULL);
      gtk_widget_show_all (menu);
      gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
#ifdef G_OS_WIN32
        position_palette_menu,
#else
       NULL, 
#endif       
       NULL, 0, gtk_get_current_event_time ());
      gtk_main ();//this works with the "selection-done" signal above; I haven't used this in other menu popups, I did this at the outset of creating palettes.
}

void repack_palette (DenemoPalette *pal)
{
    gint i;
    GList *g;
    GtkWidget *parent = gtk_widget_get_parent(pal->box);
    for(g=pal->buttons;g;g=g->next)
    {
        if(gtk_widget_get_parent(g->data))
            gtk_container_remove(GTK_CONTAINER(pal->box), g->data);
    }
    gtk_widget_destroy (pal->box);
    pal->box = gtk_grid_new();
    set_background_color (pal->box, "#8080a0");
   // gchar *tooltip = g_strdup_printf("The \"%s\" Palette:\n%s", pal->name, _("To edit this palette, dock/undock, hide etc, right click on a button and choose Edit Palette."));
   // gtk_widget_set_tooltip_text (pal->box, tooltip);
   // g_free(tooltip);
    for (i=0, g=pal->buttons;g;i++, g=g->next)
    {
                    
        if(pal->rows)
            gtk_grid_attach (GTK_GRID(pal->box), g->data, i/pal->limit, i%pal->limit, 1, 1);
        else
            gtk_grid_attach (GTK_GRID(pal->box), g->data, i%pal->limit, i/pal->limit, 1, 1);
        gtk_widget_show (GTK_WIDGET(g->data));
    }
    gtk_container_add (GTK_CONTAINER (parent), pal->box);
    if(pal->docked)
    {
        gtk_widget_hide (pal->window);
        pal->rows? gtk_widget_reparent (pal->box, Denemo.vpalettes):gtk_widget_reparent (pal->box, Denemo.hpalettes);
    }
    else
    {
        gtk_window_resize (GTK_WINDOW(pal->window), 1, 1);
        gtk_widget_show (pal->window);
        gtk_widget_set_can_focus (pal->window, FALSE);
        switch_back_to_main_window ();
    }
    gtk_widget_show(pal->box);

}


static void toggle_rows (DenemoPalette *pal) {
    pal->rows = !pal->rows;
    repack_palette (pal);
}
static void toggle_dock (DenemoPalette *pal) {
    if(pal->docked)
        gtk_widget_reparent (pal->box, pal->window), gtk_window_set_keep_above  (GTK_WINDOW (pal->window), TRUE);
    else
        pal->rows? gtk_widget_reparent (pal->box, Denemo.vpalettes):gtk_widget_reparent (pal->box, Denemo.hpalettes);
    pal->docked = !pal->docked;
    pal->docked?
        gtk_widget_hide (pal->window):
        gtk_widget_show (pal->window);
}
static void set_limit (DenemoPalette *pal) {
    gchar *initial = g_strdup_printf("%d", pal->limit);
    gchar *response = string_dialog_entry (Denemo.project, "Palette Layout", _("Give Limit"), initial);
    g_free(initial);
    if(response && atoi(response))
    {
        pal->limit = atoi(response);
        repack_palette (pal);
    } else g_warning("Cancelled %s", response);
}


static GtkWidget *get_palette_menu(DenemoPalette *pal) {
  GtkWidget *menu = gtk_menu_new ();
  GtkWidget *item = gtk_menu_item_new_with_label (_("Hide"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (hide_palette_widget), (gpointer) pal->box);
    if(!pal->rows)
  {
    item = gtk_menu_item_new_with_label (_("Make Horizontal"));
    gtk_widget_set_tooltip_text (item, _("Arrange the buttons extending horizontally"));
    //gtk_widget_add_events (item, GDK_ENTER_NOTIFY_MASK);
    //g_signal_connect (item, "enter-notify-event", show_tooltip, _("Arrange the buttons extending horizontally"));



    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
    g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (toggle_rows), (gpointer) pal);
  } else
  {
    item = gtk_menu_item_new_with_label (_("Make Vertical"));
    gtk_widget_set_tooltip_text (item, _("Arrange the buttons extending vertically"));
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
    g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (toggle_rows), (gpointer) pal);
  }

    {
    item = gtk_menu_item_new_with_label (pal->rows?_("Vertical Limit"): _("Horizontal Limit"));
    gtk_widget_set_tooltip_text (item, pal->rows? _("Set maximum extent vertically") : _("Set maximum extent horizontally") );
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
    g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (set_limit), (gpointer) pal);
    }
    {
    item = gtk_menu_item_new_with_label ( pal->docked?_("Undock"):_("Dock"));
    gtk_widget_set_tooltip_text (item,  pal->docked?_("Dock this palette in the main display"):_("Undock this palette from the main display") );
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
    g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (toggle_dock), (gpointer) pal);
    }
    if (pal->docked)    {
    item = gtk_menu_item_new_with_label ( _("Hide All Docked"));
    gtk_widget_set_tooltip_text (item, _("Hide all the docked palettes"));
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
    g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (hide_docked_palettes), (gpointer) pal);
    }
    {
    item = gtk_menu_item_new_with_label ( _("Hide All Palettes"));
    gtk_widget_set_tooltip_text (item, _("Hide all the palettes"));
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
    g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (hide_all_palettes), (gpointer) pal);
    }
        {
    item = gtk_menu_item_new_with_label ( _("Destroy All Palettes"));
    gtk_widget_set_tooltip_text (item, _("Destroy all the palettes - this will save time at startup, and shorten the palette menu."));
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
    g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (destroy_all_palettes), (gpointer) pal);
    }
  return menu;
}

static void get_script_for_button (GtkWidget *button) {
    gchar *script = g_object_get_data (G_OBJECT(button), "script");
    appendSchemeText (script);
    gboolean sensitive = gtk_widget_get_visible (gtk_widget_get_toplevel (Denemo.script_view));
    if(!sensitive) set_toggle ("ToggleScript", TRUE);
}

static void put_script_for_button (GtkWidget *button) {
    gchar *text = g_strdup_printf(_("Overwrite script for button \"%s\"?"), gtk_button_get_label(GTK_BUTTON(button)));
    if(confirm (_("Re-write Script"), text))
    {
    gchar *script = get_script_view_text ();
    gchar *oldscript = (gchar*) g_object_get_data (G_OBJECT(button), "script");
    g_object_set_data (G_OBJECT(button), "script", script);
    g_signal_handlers_block_by_func(G_OBJECT(button), G_CALLBACK (call_out_to_guile), oldscript);
    g_free(oldscript);
    g_signal_connect_swapped ( G_OBJECT (button), "clicked", G_CALLBACK (call_out_to_guile), script);
    }
    else
    infodialog (_("Cancelled"));
    g_free(text);
}

static void set_image_for_button (GtkWidget *button, gchar *name)
{
    gchar *icon = find_denemo_file (DENEMO_DIR_PIXMAPS, name);
    GtkWidget *child_widget = gtk_bin_get_child(GTK_BIN(button));
    if(GTK_IS_LABEL(child_widget)) {
        gtk_button_set_label (GTK_BUTTON(button), "");
        //g_debug("destroy %p, \n", child_widget);
        child_widget = gtk_bin_get_child(GTK_BIN(button));
        //g_debug("or rather destroy %p, is %d \n", child_widget, GTK_IS_WIDGET(child_widget));
        gtk_widget_destroy (child_widget);
    }
    
    GdkPixbuf *pb = 
#ifdef G_OS_WIN32    
    rsvg_pixbuf_from_file (icon, NULL);
#else                          
     gdk_pixbuf_new_from_file(icon, NULL);// Works on GNU/Linux but not windows - pixbuf loader not working...
#endif
    if(pb)
        gtk_button_set_image(GTK_BUTTON(button),gtk_image_new_from_pixbuf(pb));
    else
        gtk_button_set_label (GTK_BUTTON(button), name);
            //gtk_button_set_always_show_image (button, TRUE);
    g_object_set_data (G_OBJECT(button), "icon", (gpointer)g_strdup(name));
    g_type_class_unref (g_type_class_ref (GTK_TYPE_BUTTON));
    g_object_set (gtk_settings_get_default (), "gtk-button-images", TRUE, NULL);
    gtk_widget_show_all(button);
    g_free (icon);
}

static void edit_label_for_button (GtkWidget *button) {
    const gchar *label;
    label = g_object_get_data (G_OBJECT(button), "icon");
    if(label==NULL)
        label = gtk_button_get_label (GTK_BUTTON(button));
    gchar *newlabel = string_dialog_entry (Denemo.project, _("Write Label"), _("Write a label for this button"), (gchar*)label);

    if(newlabel && *newlabel) {
        gchar *icon = find_denemo_file (DENEMO_DIR_PIXMAPS, newlabel);
        if(icon) {
            set_image_for_button (button, newlabel);
            g_free (icon);
        }
        else
        {
        gtk_button_set_label (GTK_BUTTON(button), newlabel); //setting the label changes the widget: this works around a bizarre bug, if you just set the markup on the label the button has two labels
        GtkWidget *label_widget = gtk_bin_get_child(GTK_BIN(button));
        gtk_label_set_use_markup (GTK_LABEL(label_widget), TRUE);
        gtk_label_set_markup (GTK_LABEL (label_widget), newlabel);
        }
    }
    g_free(newlabel);

}
static void edit_tooltip_for_button (GtkWidget *button) {
    const gchar *tooltip = gtk_widget_get_tooltip_text (button);
    gchar *newtooltip = string_dialog_entry (Denemo.project, _("Write Tooltip"), _("Write a tooltip for this button"), (gchar*)tooltip);
    if(newtooltip) {
        gtk_widget_set_tooltip_text (button, newtooltip);
    }
    g_free(newtooltip);
}
static void help_for_button (GtkWidget *button) {
    gchar *tooltip = gtk_widget_get_tooltip_text (button);
    if (tooltip)
        infodialog (tooltip);
}
static void remove_button (GtkWidget *button) {
    DenemoPalette *pal = g_object_get_data (G_OBJECT(button), "palette");
    palette_delete_button (pal, button);

}

static void move_button_to_start (GtkWidget *button) {
    DenemoPalette *pal = g_object_get_data (G_OBJECT(button), "palette");
    pal->buttons = g_list_remove (pal->buttons, button);
    pal->buttons = g_list_prepend (pal->buttons, button);
    repack_palette (pal);
}
static void move_button_to_end (GtkWidget *button) {
    DenemoPalette *pal = g_object_get_data (G_OBJECT(button), "palette");
    pal->buttons = g_list_remove (pal->buttons, button);
    pal->buttons = g_list_append (pal->buttons, button);
    repack_palette (pal);
}
static void copy_button (GtkWidget *button) {
    gchar *tooltip =  gtk_widget_get_tooltip_text (button);
    gchar *label = (gchar*)gtk_button_get_label (GTK_BUTTON(button));
    gchar *script = g_object_get_data (G_OBJECT(button), "script");
    gchar *name = get_palette_name (TRUE);
    if(name)
    {
        DenemoPalette *pal = get_palette (name);
        if(pal==NULL)
            pal = create_palette (name, FALSE, TRUE);
        palette_add_button (pal, label, tooltip, script);
    }
}
static void duplicate_button (GtkWidget *button) {
    DenemoPalette *pal = g_object_get_data (G_OBJECT(button), "palette");
    gchar *tooltip =  gtk_widget_get_tooltip_text (button);
    gchar *label = _("Duplicate Button");
    gchar *script = g_object_get_data (G_OBJECT(button), "script");
    palette_add_button (pal, label, tooltip, script);
}
static void popup_button_menu(DenemoPalette *pal, GtkWidget *button) {
  GtkWidget *menu = gtk_menu_new ();
  GtkWidget *item;
  gboolean sensitive = gtk_widget_get_visible (gtk_widget_get_toplevel (Denemo.script_view));//some menu items should be insensitive if the Scheme window is not visible
  item = gtk_menu_item_new_with_label (_("Help"));
  gtk_widget_set_tooltip_text (item, _("Show the help for this button"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (help_for_button), (gpointer) button);

  item = gtk_menu_item_new_with_label (_("Edit Label"));
  gtk_widget_set_tooltip_text (item, _("Edit the label of this button"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (edit_label_for_button), (gpointer) button);

    item = gtk_menu_item_new_with_label (_("Edit Tooltip"));
  gtk_widget_set_tooltip_text (item, _("Edit the tooltip of this button"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (edit_tooltip_for_button), (gpointer) button);



  item = gtk_menu_item_new_with_label (_("Copy to another Palette"));
  gtk_widget_set_tooltip_text (item, _("Copy this button to another palette"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (copy_button), (gpointer) button);

  item = gtk_menu_item_new_with_label (_("Duplicate this button"));
  gtk_widget_set_tooltip_text (item, _("Create a new button with the same effect"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (duplicate_button), (gpointer) button);

  item = gtk_menu_item_new_with_label (_("Remove from Palette"));
  gtk_widget_set_tooltip_text (item, _("Remove this button from this palette"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (remove_button), (gpointer) button);

  item = gtk_menu_item_new_with_label (_("Get Script into Scheme Window"));
  gtk_widget_set_tooltip_text (item, _("Appends the script that this button executes to the Scheme window"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (get_script_for_button), (gpointer) button);

  item = gtk_menu_item_new_with_label (_("Save Script from Scheme Window"));
  gtk_widget_set_tooltip_text (item, _("Uses the script in the Scheme Window as the one that this button executes when clicked, overwriting its old script"));
  gtk_widget_set_sensitive (item, sensitive);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (put_script_for_button), (gpointer) button);

  item = gtk_menu_item_new_with_label (_("Move to Start"));
  gtk_widget_set_tooltip_text (item, _("Moves this button to the start of the palette"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (move_button_to_start), (gpointer) button);

  item = gtk_menu_item_new_with_label (_("Move to End"));
  gtk_widget_set_tooltip_text (item, _("Moves this button to the end of the palette"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (move_button_to_end), (gpointer) button);

  item = gtk_menu_item_new_with_label (_("Edit this Palette"));
  gtk_widget_set_tooltip_text (item, _("Edits the palette containing this button"));
  gtk_menu_item_set_submenu (GTK_MENU_ITEM(item), (gpointer)get_palette_menu(pal));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);



  g_signal_connect (menu, "selection-done", gtk_main_quit, NULL);
  gtk_widget_show_all (menu);
  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
  gtk_main ();
}
static DenemoPalette *new_palette (gchar *name, gboolean by_row)
{
    DenemoPalette *pal = g_malloc0(sizeof (DenemoPalette));
    pal->name = g_strdup(_(name));
    pal->limit = 1;
    pal->rows = by_row;
    pal->box =  gtk_grid_new();
    Denemo.palettes = g_list_append(Denemo.palettes, (gpointer)pal);
    return pal;
}

DenemoPalette *get_palette (gchar *name)
{
    GList *g;
    for( g = Denemo.palettes; g; g = g->next)
    {
        DenemoPalette *pal = (DenemoPalette*)g->data;
        if (!strcmp(name, pal->name))
            return pal;
    }
    return NULL;
}

static gboolean button_pressed (GtkWidget *button, GdkEventButton  *event, DenemoPalette *pal)
{
    Denemo.currentpalette = pal;
    if (event->button == 1)
        return FALSE;
    popup_button_menu(pal, button);
    return TRUE;
}
static gboolean already_present (DenemoPalette *pal, gchar *label) {
    GList *g;
    for(g=pal->buttons;g;g=g->next) {
        gchar *icon = g_object_get_data (G_OBJECT(g->data), "icon");
        if (!strcmp(label, gtk_button_get_label (GTK_BUTTON(g->data))) || (icon && !strcmp(icon, label)))
            return TRUE;
    }
    return FALSE;
}

static void fixup_image (GtkWidget *button, gchar *label) {
    //g_debug("Fixing up image");
    set_image_for_button (button, label);
}

void switch_and_call_out_to_guile (gchar *script)
    {
        switch_back_to_main_window ();
        g_string_assign (Denemo.input_filters, _("Palette Button Activated"));
        write_input_status();
        call_out_to_guile (script);
    }
gboolean palette_add_button (DenemoPalette *pal, gchar *label, const gchar *tooltip, gchar *script)
{
    if (already_present(pal, label))
        return FALSE;
    gchar *thescript = g_strdup(script);
    GtkWidget *button = gtk_button_new_with_label (label);
#if ((GTK_MAJOR_VERSION==3)&&(GTK_MINOR_VERSION>=12))
      gtk_widget_set_margin_start (button, 1);
      gtk_widget_set_margin_end (button, 1);
      gtk_widget_set_margin_top (button, 1);
      gtk_widget_set_margin_bottom (button, 1);
#endif
    gchar *icon = find_denemo_file (DENEMO_DIR_PIXMAPS, label);
    if(icon)
    {
        g_signal_connect (button, "realize", G_CALLBACK (fixup_image), label);
    } else
    {
        GtkWidget *label_widget = gtk_bin_get_child(GTK_BIN(button));//g_debug("is %s\n", g_type_name(G_TYPE_FROM_INSTANCE(label_widget)));
        gtk_label_set_use_markup (GTK_LABEL(label_widget), TRUE);
    }
    //put button in a list pal->buttons and then call repack_palette.
    //REF it for repacking
    g_object_ref (button);
    pal->buttons = g_list_append(pal->buttons, (gpointer)button);
    repack_palette (pal);

    gtk_widget_set_tooltip_text (button, _(tooltip));
    g_object_set_data (G_OBJECT(button), "script", thescript);
    g_object_set_data (G_OBJECT(button), "palette", pal);
    g_signal_connect_swapped ( G_OBJECT (button), "clicked", G_CALLBACK (switch_and_call_out_to_guile), thescript);
    g_signal_connect_after ( G_OBJECT (button), "clicked", G_CALLBACK (switch_back_to_main_window), NULL);
    g_signal_connect (G_OBJECT (button), "button-press-event", G_CALLBACK (button_pressed), (gpointer)pal);
    Denemo.currentpalette = pal;

    return TRUE;
}

void palette_delete_button (DenemoPalette *pal, GtkWidget *button)
{
    g_free( g_object_get_data (G_OBJECT(button), "script"));
    pal->buttons = g_list_remove (pal->buttons, button);
    gtk_widget_destroy(button);
}

gboolean palette_action_button (DenemoPalette *pal, gchar *label)
{
    gboolean ret = FALSE;
    GList *g;
    for (g=pal->buttons;g;g=g->next)
    {
        GtkWidget *button = g->data;
        const gchar *this = gtk_button_get_label (GTK_BUTTON(button));
        if (*this=='<')
           {
            while (*this && *this != '>')
                this++;
            if(*this) this++;
            }
           // g_print ("this %s and %s %d\n", this, label,  g_str_has_prefix (this, label));
        if(*this && g_str_has_prefix (this, label))
            {
            ret = gtk_widget_activate (button);
            break;
            }
    }
    Denemo.currentpalette = pal;
    return ret;
}
DenemoPalette *create_palette (gchar *name, gboolean docked, gboolean rows) {
    DenemoPalette *pal = get_palette (name);

    if(pal==NULL)
    {
        pal = new_palette (name, TRUE);
        pal->window =  gtk_window_new (GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title (GTK_WINDOW (pal->window), pal->name);
        if(!docked)
            {
             gtk_window_set_default_size (GTK_WINDOW (pal->window), 200, 100);//try and get folk to notice it!
             gtk_window_present (GTK_WINDOW (pal->window));
         gtk_window_set_transient_for (GTK_WINDOW(pal->window), GTK_WINDOW(Denemo.window));
             gtk_window_set_keep_above  (GTK_WINDOW (pal->window), TRUE);
         }

        g_signal_connect (G_OBJECT (pal->window), "delete-event", G_CALLBACK (gtk_widget_hide_on_delete), NULL);
        pal->rows = rows;
        if(docked)
            {
             pal->docked = TRUE;
             pal->rows?
             gtk_box_pack_start (GTK_BOX(Denemo.vpalettes), pal->box, FALSE, TRUE, 0):
             gtk_box_pack_start (GTK_BOX(Denemo.hpalettes), pal->box, FALSE, TRUE, 0);
            }
        else
            {
            gtk_widget_show (pal->window);
            gtk_container_add (GTK_CONTAINER (pal->window), pal->box);
        }
    }
    Denemo.currentpalette = pal;
    return pal;
}

DenemoPalette *set_palette_shape (gchar *name, gboolean row_wise, gint limit)
{
 DenemoPalette *pal = create_palette (name, FALSE, row_wise);
    if (limit>0) {
        pal->limit = limit;
        pal->rows = row_wise;
        repack_palette (pal);
        //gtk_window_resize (GTK_WINDOW (window), 1, 1);
        Denemo.currentpalette = pal;
        return pal;
    }
  return NULL;
}

void delete_palette (DenemoPalette *pal) {
    GList *g;
    for(g=pal->buttons;g;g=g->next)
        palette_delete_button (pal, GTK_WIDGET(g->data));
    gtk_widget_destroy (gtk_widget_get_parent (pal->box));//FIXME if docked this will not be a toplevel
    Denemo.palettes = g_list_remove (Denemo.palettes, pal);
    Denemo.currentpalette = NULL;
}

static gchar *selected_palette_name = NULL;
static void palette_selected (gchar *name)
{
    selected_palette_name = name;
}
static void user_palette_name (void)
{
    gchar *name;
    name = string_dialog_entry (Denemo.project, _("Palette Name"), _("Give name for Palette: "), _("MyPalette"));
    selected_palette_name = name;
}

static void set_flag (gboolean *flag)
{
    *flag = TRUE;
}

//recursive:
static gchar *choose_palette_by_name_and_type (gboolean allow_custom, gboolean non_showing, gboolean non_menus)
{
  GtkWidget *menu = gtk_menu_new ();
  GtkWidget *item;
  GList *g;
  selected_palette_name = NULL;
  gboolean menu_palette_wanted = FALSE;
  if(Denemo.palettes) {
      gboolean menu_palettes_present = FALSE;
      if(allow_custom) {
        item = gtk_menu_item_new_with_label (_("Create Custom Palette"));
        gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
        }

      for (g=Denemo.palettes;g;g=g->next)
        {
        DenemoPalette *pal = (DenemoPalette *)g->data;
        if(non_showing && pal->docked && gtk_widget_get_visible (pal->box))
            continue;//g_debug("palette %s is %d\n", pal->name,  gtk_widget_get_visible (pal->box));//continue;
        if(non_showing && (!pal->docked) && gtk_widget_get_visible (gtk_widget_get_parent(pal->box)))
            continue;
        if (pal->menu && non_menus)
            {
                menu_palettes_present = TRUE;
                continue; //we include them via a button to pop them up
            }
        if ((!pal->menu) && (!non_menus))
            continue; //we don't want the non-menu buttons
        item = gtk_menu_item_new_with_label (pal->name);
        gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
        g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (palette_selected), (gpointer) pal->name);
        }
        if (menu_palettes_present)
            {
                        item = gtk_menu_item_new_with_label (_("Palettes from Menus"));
                        gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
                        g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (set_flag), &menu_palette_wanted);
                
            }
        popupmenu (menu);
    }
    if (non_menus && menu_palette_wanted)
        return choose_palette_by_name_and_type (FALSE, non_showing, FALSE);
    if(allow_custom && (selected_palette_name==NULL))
        {
            user_palette_name ();
        } g_print ("selected %s\n", selected_palette_name);
    return selected_palette_name;
}
gchar *choose_palette_by_name (gboolean allow_custom, gboolean non_showing)
{
 return  choose_palette_by_name_and_type (allow_custom, non_showing, TRUE);
}
gchar *get_palette_name (gboolean allow_custom)
{
    return choose_palette_by_name (allow_custom, FALSE);
}

void place_action_in_palette (gint idx, const gchar *name)
{
     gchar *label = (gchar *) lookup_label_from_idx (Denemo.map, idx);
     if(name==NULL)
        name = lookup_name_from_idx (Denemo.map, idx);
      gchar *script = g_strdup_printf ("(d-%s)", name);
      const gchar *tooltip = lookup_tooltip_from_idx (Denemo.map, idx);
      gchar *palette_name = get_palette_name (TRUE);g_print("palette name %s\n", palette_name);
      if(palette_name) {
        DenemoPalette *pal = get_palette (palette_name);
        if(pal==NULL)
            {
            pal = set_palette_shape (palette_name, TRUE, 1);
        }
        if(pal)
            palette_add_button (pal, label, tooltip, script);
        }
      g_free (script);
}



