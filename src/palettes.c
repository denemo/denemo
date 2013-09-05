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
#include "palettes.h"
#include "view.h"
static void hide_parent_widget (GtkWidget *w) {
	GtkWidget *parent = gtk_widget_get_parent (w);
	gtk_widget_hide (parent);
}
static void popupmenu (GtkWidget *menu) {
	  g_signal_connect (menu, "selection-done", gtk_main_quit, NULL);
	  gtk_widget_show_all (menu);
      gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
      gtk_main ();
}
static GtkWidget *get_palette_menu(GtkWidget *widget) {
  GtkWidget *menu = gtk_menu_new ();
  GtkWidget *item = gtk_menu_item_new_with_label (_("Hide"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (hide_parent_widget), (gpointer) widget);
  GtkWidget *menubutton = gtk_button_new_with_label (_("Adjust"));
  g_signal_connect_swapped ( G_OBJECT (menubutton), "clicked", G_CALLBACK (popupmenu), menu);
  return menubutton;	
}

static void get_script_for_button (GtkWidget *button) {
	gchar *script = g_object_get_data (G_OBJECT(button), "script");
	g_print("Script is %s\n\n", script);
}

static void remove_button (button) {
	DenemoPalette *pal = g_object_get_data (G_OBJECT(button), "palette");
	palette_delete_button (pal, button);
}

static GtkWidget *popup_button_menu(GtkWidget *button) {
  GtkWidget *menu = gtk_menu_new ();
  GtkWidget *item = gtk_menu_item_new_with_label (_("Get Script"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (get_script_for_button), (gpointer) button);
  
  
  item = gtk_menu_item_new_with_label (_("Remove from Palette"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (remove_button), (gpointer) button);
  
  
  g_signal_connect (menu, "selection-done", gtk_main_quit, NULL);
  gtk_widget_show_all (menu);
  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
  gtk_main ();
}
DenemoPalette *new_palette (gchar *name, gboolean by_row)
{
	DenemoPalette *pal = g_malloc0(sizeof (DenemoPalette));
	pal->name = name;
	pal->limit = 1;
	pal->rows = by_row;
	pal->box = by_row?gtk_vbox_new(FALSE, 1):gtk_hbox_new(FALSE, 1);
	GtkWidget *menu = get_palette_menu(pal->box);
	gtk_box_pack_end (pal->box, menu, FALSE, TRUE, 0);
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

static button_pressed (GtkWidget *button, GdkEventButton  *event, DenemoPalette *pal)
{ 
	if (event->button == 1)
		return FALSE;
	popup_button_menu(button);
	return TRUE;	
}
void palette_add_button (DenemoPalette *pal, gchar *label, gchar *tooltip, gchar *script) 
{
	GtkWidget *button = gtk_button_new_with_label (label);
	gtk_box_pack_end (pal->box, button, FALSE, TRUE, 0);
	gtk_widget_set_tooltip_text (button, tooltip);
	g_object_set_data (G_OBJECT(button), "script", script);
	g_object_set_data (G_OBJECT(button), "palette", pal);
	g_signal_connect_swapped ( G_OBJECT (button), "clicked", G_CALLBACK (call_out_to_guile), script);
	g_signal_connect ( G_OBJECT (button), "button-press-event", G_CALLBACK (button_pressed), (gpointer)pal);
}

void palette_delete_button (DenemoPalette *pal, GtkWidget *button) 
{
	g_free( g_object_get_data (G_OBJECT(button), "script"));
	pal->buttons = g_list_remove (pal->buttons, button);
	gtk_widget_destroy(button);
}
