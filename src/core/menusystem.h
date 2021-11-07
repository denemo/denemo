/*
 * menusystem.h
 *
 * Copyright 2016 Richard Shann <richard@rshann.plus.com>
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor Boston, MA 02110-1301,  USA
 */


#ifndef UIMANAGER_H
#define UIMANAGER_H
#include <denemo/denemo.h>
void denemo_menusystem_new (void);
void denemo_menusystem_add_menu (gchar *path, gchar *name);
GtkWidget *denemo_menusystem_get_widget (gchar *path);
DenemoAction *denemo_menusystem_get_action (gchar *path);
DenemoAction *denemo_action_new (const gchar *name, const gchar *label, const gchar *tooltip);
gchar *denemo_action_get_name (DenemoAction *action);
gchar *denemo_action_get_tooltip (DenemoAction *action);
GList* denemo_action_get_proxies (DenemoAction *action);
void denemo_action_activate (DenemoAction *action);
void finalize_menusystem(void);
void instantiate_menus (gchar * menupath);
gchar *get_menupath_for_name (gchar *name);
gchar *get_location_for_name (gchar *name);
void set_toggle (gchar *name, gboolean value);
gboolean get_toggle (gchar *name);
void toggle_scheme (void);
void denemo_menusystem_add_command (gchar *path, gchar *name, gchar *after);
void denemo_menusystem_add_actions (void);
void show_verses (void);
#endif
