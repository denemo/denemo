/* measureops.h
 * header file for functions dealing with measures
 *
 * for Denemo, a gtk+ frontent to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller
 */

#include <denemo/denemo.h>

struct fourints
{
  gint time1;
  gint time2;
  gint clef;
  gint stem_directive;
};

measurenode *addmeasures (DenemoMovement * si, gint pos, guint nummeasures, gint all);

void freeobjlist (gpointer data, gpointer user_data);

measurenode *removemeasures (DenemoMovement * si, guint pos, guint nummeasures, gboolean all);

void calculatebeamsandstemdirs (objnode * theobjs, gint * clef, gint * time1, gint * time2, gint * stemdirs);

gint showwhichaccidentals (objnode * theobjs, gint initialnum, gint * initialaccs);

void forceaccidentals (DenemoObject * theobj);

objnode *measure_first_obj_node (measurenode * mnode);

objnode *measure_last_obj_node (measurenode * mnode);