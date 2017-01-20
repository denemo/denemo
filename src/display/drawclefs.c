/* drawclefs.cpp
 * functions for drawing clefs
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller, Adam Tee
 */

#include "core/utils.h"              /* Includes gtk.h */
#include <denemo/denemo.h>

#define NUMCLEFTYPES DENEMO_INVALID_CLEF
#define TREBLE_TOPOFFSET 30
#define BASS_TOPOFFSET 10
#define ALTO_TOPOFFSET 20
#define G_8_TOPOFFSET 30
#define TENOR_TOPOFFSET 10
#define SOPRANO_TOPOFFSET 40
#define FRENCH_TOPOFFSET 40
#define BARITONE_TOPOFFSET 0

/**
 * This function draws the clef appropriate for the current context
 * onto the backing pixmap
 */
void
draw_clef (cairo_t * cr, gint xx, gint y, clef * clef)
{
  gint type = clef->type;
  static gint clefoffsets[NUMCLEFTYPES] = { TREBLE_TOPOFFSET, BASS_TOPOFFSET, ALTO_TOPOFFSET, G_8_TOPOFFSET,
    TENOR_TOPOFFSET, SOPRANO_TOPOFFSET, BASS_TOPOFFSET, FRENCH_TOPOFFSET, BARITONE_TOPOFFSET
  };
  static gunichar clef_char[NUMCLEFTYPES] = { 0xc9, 0xc7, 0xc5, 0xc9, 0xc5, 0xc5, 0xc7, 0xc9, 0xc5
  };

  gint override = 0;
  if (clef->directives)
    {
      gint count = 0;
      GList *g = clef->directives;
      for (; g; g = g->next, count++)
        {
          DenemoDirective *directive = g->data;
          override = override | directive->override;
          if (directive->display)
            {
              drawnormaltext_cr (cr, directive->display->str, xx + directive->tx, y + count * 10);
            }
          if (directive->graphic)
            {
#ifdef G_OS_WIN32
              y-= 4; // bizarrely clefs are drawn slightly too low using the windows routine windows_draw_text() in utils.c while standalone and notehead overrides need +4
              drawbitmapinverse_cr (cr, directive->graphic, xx + directive->gx + count, y + directive->gy, FALSE);
#else              
              drawbitmapinverse_cr (cr, directive->graphic, xx + directive->gx + count, y + directive->gy, FALSE);
#endif
            }
        }
    }
  if (!(DENEMO_OVERRIDE_GRAPHIC & override))
    {
      drawfetachar_cr (cr, clef_char[type], xx, y + clefoffsets[type]);
      if (type == DENEMO_G_8_CLEF)
        drawnormaltext_cr (cr, "8", xx + 8, y + 65);
      if (type == DENEMO_F_8_CLEF)
        drawnormaltext_cr (cr, "8", xx + 8, y + 55);
    }
}
