/* drawrepeats.c
 *
 * Functions for drawing notes and rests
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Matthew Hiller, Adam Tee
 */

#include "utils.h"		/* Includes <gdk.h> */
#include "drawingprims.h"
#include "notewidths.h"
#include "gcs.h"

/* Include the bitmaps; they'll actually be made into GdkBitmaps with the
 * GDK create_from_xbm_d functions. Doing things this way circumvents the need
 * to install the pixmaps into a /usr/share directory of some kind before
 * the program can be run from anywhere on the system. */

#include "../pixmaps/repeat_open.xbm"
#include "../pixmaps/repeat_close.xbm"

/**
 * draw_repeat
 * This function actually draws a repeat sign the backing pixmap 
 *
 */

void
draw_repeat (GdkPixmap * pixmap, GdkGC * gc, gint xx, gint y)
{
    
  if (!repeat[0])
    {
      repeat[0] = bitmaphelper (NULL, repeat_open);
      repeat[1] = bitmaphelper (NULL, repeat_close);
    }

  /*drawbitmapinverse (pixmap, gc, rests[duration],
		     xx, y + restoffsets[duration],
		     restwidths[duration], restheights[duration]);
*/
}


