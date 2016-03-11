/*
 * cache.c
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
#include "core/cache.h"

static void measure_set_cache (DenemoMeasure *meas, clef *clef, timesig *timesig, keysig *keysig)
{
    meas->clef = clef;
    meas->timesig = timesig;
    meas->keysig = keysig;
}
static void object_set_cache (DenemoObject *meas, clef *clef, timesig *timesig, keysig *keysig)
{
    meas->clef = clef;
    meas->timesig = timesig;
    meas->keysig = keysig;
}
void cache_from_cursor (void)
{
    
    
}

void cache_all (void)
{
  GList *s;
  if (Denemo.project->movement &&  (s = Denemo.project->movement->thescore))
     for (; s; s = s->next)
    {
      DenemoStaff *staff = (DenemoStaff*)s->data;
      GList *m;
        DenemoMeasure *measure = (DenemoMeasure*)staff->themeasures->data;
        clef *cclef =  &staff->clef;
        timesig *ctim =  &staff->timesig;
        keysig *ckey =  &staff->keysig;
        
                
        for (m = staff->themeasures;m; m = m->next)
            {
                GList *o;
                measure = (DenemoMeasure*)m->data;
                measure_set_cache (measure, cclef, ctim, ckey);
                for (o=measure->objects; o; o = o->next)
                    {
                       DenemoObject *obj = (DenemoObject *)o->data;
                       switch (obj->type)
                            {
                                case CLEF:
                                    cclef = obj->object;
                                
                                break;
                                case TIMESIG:
                                    ctim = obj->object;
                                
                                break;
                                 case KEYSIG:
                                    ckey = obj->object;
                                
                                break;                               
                                default:
                                    break;
                                
                            }
                     object_set_cache (obj, cclef, ctim, ckey);  
                    }
            }
        }
}
