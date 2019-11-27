//      scorelayout.h
//
//      Copyright 2012 Richard Shann <rshann@debian2>
//
//      This program is free software; you can redistribute it and/or modify
//      it under the terms of the GNU General Public License as published by
//      the Free Software Foundation; either version 2 of the License, or
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





#ifndef scorelayout_H
#define scorelayout_H

#include <denemo/denemo.h>
void create_default_scoreblock (void);
void free_scoreblocks (DenemoProject * gui);
void free_standard_scoreblocks (DenemoProject * gui);
void free_custom_scoreblocks (DenemoProject * gui);

DenemoScoreblock *select_layout (gboolean all_movements, gchar * partname, gchar * instrumentation);
DenemoScoreblock *get_scoreblock_for_lilypond (gchar * lily);
GtkWidget *get_score_layout_notebook (DenemoProject * gui);
DenemoScoreblock *create_custom_scoreblock (gchar * layout_name, gboolean force);
gboolean delete_custom_scoreblock (gchar * layout_name);
DenemoScoreblock *create_custom_lilypond_scoreblock (void);
void select_default_scoreblock (void);
DenemoScoreblock *selected_scoreblock (void);
gboolean current_scoreblock_is_custom (void);
guint selected_layout_id (void);
void update_standard_scoreblocks (void);
DenemoScoreblock *get_next_scoreblock (void);
DenemoScoreblock *get_first_scoreblock (void);
gboolean iterate_custom_layout (gboolean init);
gchar *get_output_uri_from_scoreblock (void);
void set_current_scoreblock_uri (gchar * uri);
void select_standard_layout (DenemoScoreblock * sb);
void select_custom_layout (DenemoScoreblock * sb);
gboolean select_custom_layout_for_name (gchar * name);
GtkWidget *GetLayoutMenu (void);
void refresh_lilypond (DenemoScoreblock * sb);
gboolean select_layout_id (gint id);
guint get_layout_id_for_name (gchar * name);
#endif
