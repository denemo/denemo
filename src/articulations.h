/*
 *  articulations.h
 *  articulation callback prototypes 
 * 
 *  for Denemo, a gtk+frontend to GNU Lilypond	
 *  (c) 2001-2005 Adam
 */

#ifndef ARTICULATIONS_H
#define ARTICULATIONS_H

enum articulationcallbackaction
{ GENERAL, STRING, ORGAN };


void set_articulation(gchar *string, DenemoObject *obj);


void toggle_articulation_palette (GtkAction *action);

GList* insert_ornament_list(enum ornament orn, GList *list);
#endif
