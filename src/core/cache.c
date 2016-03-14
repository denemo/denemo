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

static void measure_set_cache (DenemoMeasure *meas, clef *clef, timesig *timesig, keysig *keysig, stemdirective *stem)
{
    meas->clef = clef;
    meas->timesig = timesig;
    meas->keysig = keysig;
    meas->stemdir = stem;
}
static void object_set_cache (DenemoObject *obj, clef *clef, keysig *keysig, stemdirective *stem)
{
    obj->clef = clef;
    obj->keysig = keysig;
    obj->stemdir = stem;
}
void cache_from_cursor (void)
{
    
    
}
void update_timesig_cache (measurenode *mnode)
{
    DenemoMeasure *measure = mnode->data;
    gint time1 = measure->timesig->time1;
    gint time2 = measure->timesig->time2;
    timesig *new = measure->timesig;
    for (mnode=mnode->next;mnode;mnode=mnode->next)
        {
            measure = mnode->data;
            if ((measure->timesig->time1 != time1) || (measure->timesig->time2 != time2))
                measure->timesig = new;
            else
                return;
        }
}

void update_clef_cache (measurenode *mnode, objnode *onode)
{
    DenemoMeasure *measure = mnode->data;
    clef *current = onode?((DenemoObject*)onode->data)->clef: measure->clef;
    if (onode) onode = onode->next;
    for (;mnode;mnode=mnode->next, onode = mnode? ((DenemoMeasure*)mnode->data)->objects:NULL, 
                                    mnode?((DenemoMeasure*)mnode->data)->clef = current:NULL)
        {
            while (onode)
                    {
                       DenemoObject *obj = (DenemoObject *)onode->data;
                       if (obj->type == CLEF)
                            return;
                        obj->clef = current;
                        onode = onode->next;
                    }
        }
}

void update_keysig_cache (measurenode *mnode, objnode *onode)
{
    DenemoMeasure *measure = mnode->data;
    keysig *current =((DenemoObject*)onode->data)->keysig;
    
    for (onode = onode->next;mnode;mnode=mnode->next,
                                    onode = mnode? ((DenemoMeasure*)mnode->data)->objects:NULL,
                                    mnode?((DenemoMeasure*)mnode->data)->keysig = current:NULL)
        {
            while (onode)
                    {
                       DenemoObject *obj = (DenemoObject *)onode->data;
                       if (obj->type == KEYSIG)
                            return;
                        obj->keysig = current;
                        onode = onode->next;
                    }
        }
}

void update_stemdir_cache (measurenode *mnode, objnode *onode)
{
    DenemoMeasure *measure = mnode->data;
    stemdirective *current =((DenemoObject*)onode->data)->stemdir;
    
    for (onode = onode->next;mnode;mnode=mnode->next,
                                    onode = mnode? ((DenemoMeasure*)mnode->data)->objects:NULL,
                                    mnode?((DenemoMeasure*)mnode->data)->stemdir = current:NULL)
        {
            while (onode)
                    {
                       DenemoObject *obj = (DenemoObject *)onode->data;
                       if (obj->type == STEMDIRECTIVE)
                            return;
                        obj->stemdir = current;
                        onode = onode->next;
                    }
        }
}
static stemdirective StemNeutral = {
    DENEMO_STEMBOTH,
    NULL
};

void cache_staff (staffnode *s)
{
      DenemoStaff *staff = (DenemoStaff*)s->data;
      GList *m;
        DenemoMeasure *measure = (DenemoMeasure*)staff->themeasures->data;
        clef *cclef =  &staff->clef;
        timesig *ctim =  &staff->timesig;
        keysig *ckey =  &staff->keysig;
        stemdirective *cstem =  &StemNeutral;
        for (m = staff->themeasures;m; m = m->next)
            {
                GList *o;
                measure = (DenemoMeasure*)m->data;
                measure_set_cache (measure, cclef, ctim, ckey, cstem);
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
                                    measure->timesig = ctim;
                                break;
                                case KEYSIG:
                                    ckey = obj->object;
                                break;  
                                case STEMDIRECTIVE:
                                    cstem = obj->object;
                                break;        
                                default:
                                    break;
                                
                            }
                     object_set_cache (obj, cclef, ckey, cstem);  
                    }
            }
}



void cache_all (void)
{
  GList *s;
  if (Denemo.project->movement &&  (s = Denemo.project->movement->thescore))
     for (; s; s = s->next)
    cache_staff (s);
}
