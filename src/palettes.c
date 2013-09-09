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
#include "utils.h"
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
	gtk_widget_set_tooltip_text (pal->box, _("To edit the shape of this palette, remove buttons etc right click on a button"));
	for (i=0, g=pal->buttons;g;i++, g=g->next)
	{
		if(pal->rows)
			gtk_grid_attach (GTK_GRID(pal->box), g->data, i/pal->limit, i%pal->limit, 1, 1);
		else
			gtk_grid_attach (GTK_GRID(pal->box), g->data, i%pal->limit, i/pal->limit, 1, 1);
	}
	gtk_container_add (GTK_CONTAINER (parent), pal->box);
	gtk_window_resize (GTK_WINDOW(gtk_widget_get_parent (pal->box)), 1, 1);//FIXME if docked in main display this will not be a GTK_WINDOW
	gtk_widget_show_all(pal->box);//without this non-hidden palettes have hidden buttons.
}


static void toggle_rows (DenemoPalette *pal) {
	pal->rows = !pal->rows;
	repack_palette (pal);
}
static void set_limit (DenemoPalette *pal) {
	gchar *initial = g_strdup_printf("%d", pal->limit);
	gchar *response = string_dialog_entry (Denemo.gui, "Palette Layout", _("Give Limit"), initial);
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
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (hide_parent_widget), (gpointer) pal->box);
    if(!pal->rows) 
  {
	item = gtk_menu_item_new_with_label (_("Make Horizontal"));
	gtk_widget_set_tooltip_text (item, _("Arrange the buttons extending horizontally"));
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
  return menu;
}

static void get_script_for_button (GtkWidget *button) {
	gchar *script = g_object_get_data (G_OBJECT(button), "script");
}

static void remove_button (GtkWidget *button) {
	DenemoPalette *pal = g_object_get_data (G_OBJECT(button), "palette");
	palette_delete_button (pal, button);
}

static GtkWidget *popup_button_menu(DenemoPalette *pal, GtkWidget *button) {
  GtkWidget *menu = gtk_menu_new ();
  GtkWidget *item;

  
  item = gtk_menu_item_new_with_label (_("Get Script"));
  gtk_widget_set_tooltip_text (item, _("Places the script that this button executs into the Scheme window"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (get_script_for_button), (gpointer) button);
  
  
  item = gtk_menu_item_new_with_label (_("Remove from Palette"));
  gtk_widget_set_tooltip_text (item, _("Remove this button from this palette"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (remove_button), (gpointer) button);
 
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
	pal->name = g_strdup(name);
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

static button_pressed (GtkWidget *button, GdkEventButton  *event, DenemoPalette *pal)
{ 
	if (event->button == 1)
		return FALSE;
	popup_button_menu(pal, button);
	return TRUE;	
}
static gboolean already_present (DenemoPalette *pal, gchar *label) {
	GList *g;
	for(g=pal->buttons;g;g=g->next) {
		if (!strcmp(label, gtk_button_get_label (GTK_BUTTON(g->data))))
			return TRUE;	
	}
	return FALSE;
}
gboolean palette_add_button (DenemoPalette *pal, gchar *label, gchar *tooltip, gchar *script) 
{
	if (already_present(pal, label))
		return FALSE;
	gchar *thescript = g_strdup(script);
	GtkWidget *button = gtk_button_new_with_label (label);
	GtkWidget *label_widget = gtk_bin_get_child(GTK_BIN(button));//g_print("is %s\n", g_type_name(G_TYPE_FROM_INSTANCE(label_widget)));
	gtk_label_set_use_markup (GTK_LABEL(label_widget), TRUE);
	
	//put button in a list pal->buttons and then call repack_palette.
	//REF it for repacking
	g_object_ref (button);
	pal->buttons = g_list_append(pal->buttons, (gpointer)button);
	repack_palette (pal);
	
	gtk_widget_set_tooltip_text (button, tooltip);
	g_object_set_data (G_OBJECT(button), "script", thescript);
	g_object_set_data (G_OBJECT(button), "palette", pal);
	g_signal_connect_swapped ( G_OBJECT (button), "clicked", G_CALLBACK (call_out_to_guile), thescript);
	g_signal_connect_after ( G_OBJECT (button), "clicked", G_CALLBACK (switch_back_to_main_window), NULL);
	g_signal_connect (G_OBJECT (button), "button-press-event", G_CALLBACK (button_pressed), (gpointer)pal);
	return TRUE;
}

void palette_delete_button (DenemoPalette *pal, GtkWidget *button) 
{
	g_free( g_object_get_data (G_OBJECT(button), "script"));
	pal->buttons = g_list_remove (pal->buttons, button);
	gtk_widget_destroy(button);
}


DenemoPalette *create_palette (gchar *name, gboolean docked) {
	DenemoPalette *pal = get_palette (name);
	
	if(pal==NULL) 
	{
		pal = new_palette (name, TRUE);
		GtkWidget *window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
		gtk_window_set_title (GTK_WINDOW (window), name);		
		gtk_widget_show (window);
		gtk_container_add (GTK_CONTAINER (window), pal->box);
		g_signal_connect (G_OBJECT (window), "delete-event", G_CALLBACK (gtk_widget_hide_on_delete), NULL);
	}	
	return pal;
}

DenemoPalette *set_palate_shape (gchar *name, gboolean row_wise, gint limit)
{
 DenemoPalette *pal = create_palette (name, FALSE);
	if (limit>0) {
		pal->limit = limit;
		pal->rows = row_wise;
		repack_palette (pal);
		//gtk_window_resize (GTK_WINDOW (window), 1, 1);
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
}

static gchar *selected_palette_name = NULL;
static void palette_selected (gchar *name) 
{
	selected_palette_name = name;
}
static void user_palette_name (void)
{
	gchar *name;
	name = string_dialog_entry (Denemo.gui, _("Palette Name"), _("Give name for Palette: "), _("MyPalette"));
	selected_palette_name = name;
}
gchar *get_palette_name (void)
{
  GtkWidget *menu = gtk_menu_new ();
  GtkWidget *item;
  GList *g;
  	item = gtk_menu_item_new_with_label (_("Custom"));
	gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
	//g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (user_palette_name), NULL);
  for (g=Denemo.palettes;g;g=g->next)
	{
	DenemoPalette *pal = (DenemoPalette *)g->data;
	item = gtk_menu_item_new_with_label (pal->name);
	gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
	g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (palette_selected), (gpointer) pal->name);
	}	

	
	selected_palette_name = NULL;
	popupmenu (menu);
	if(selected_palette_name==NULL)
		{
			//gtk_widget_destroy (menu);
			user_palette_name ();
		}
	return selected_palette_name;
}


