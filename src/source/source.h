//      source.h
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

#ifndef SOURCE_H
#define SOURCE_H

#include <denemo/denemo.h>

//Opens a window with an evince widget displaying filename marked scrolled to the internal position x, y
//keeps a list of opened files and re-uses the already opened file if there is one.
gboolean open_source (gchar * filename, gint x, gint y, gint page);


gboolean move_source_window (gint x, gint y);
#endif
