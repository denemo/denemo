#include "core/utils.h"
#include <denemo/denemo.h>
#include <string.h>

/**
 * Draw fakechords on the score
 *
 */
void
draw_fakechord (cairo_t * cr, gint xx, gint y, DenemoObject * theobj)
{
  chord *ch;
  if (theobj->type == FAKECHORD)
    {
      g_warning ("FAKECHORD type found and not handled");
    }
  else if (theobj->type == CHORD)
    {
      ch = (chord *) theobj->object;
      drawnormaltext_cr (cr, ((GString *) ch->fakechord)->str, xx, y + STAFF_HEIGHT - 10);
    }
}
