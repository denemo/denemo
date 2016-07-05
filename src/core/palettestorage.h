/*
 * palettestorage.h
 *
 * Copyright 2013 Richard Shann
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
#ifndef PALETTESTORAGE_H
#define PALETTESTORAGE_H
#include <gtk/gtk.h>
#include <denemo/denemo.h>
#include <string.h>

gint writePalettes (void);
/* installs palettes from user's palettes or, failing, from system palettes */
gint installPalettes (void);
/* installs palette of given name from the system palettes merging with those already present */
gint mergePalette (const gchar *name);
/*installs palettes from the give xml file, merging with those already present. If hide is TRUE hide all elements in the merged palettes */
gint installPalettesFile (gchar *filename, gboolean hide);
#endif
