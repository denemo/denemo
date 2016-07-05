/*
 * cache.h
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


#ifndef CACHE_H
#define CACHE_H
#include <denemo/denemo.h>


void update_timesig_cache (measurenode *mnode);
void update_clef_cache (measurenode *mnode, objnode *onode);
void update_keysig_cache (measurenode *mnode, objnode *onode);
void update_stemdir_cache (measurenode *mnode, objnode *onode);
void cache_measure (measurenode *mnode);
void cache_staff (staffnode *s);
void cache_all (void);

#endif
