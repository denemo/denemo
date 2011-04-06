/* drawfigure.cpp 
 *
 * function to display Figured Bass figures on score
 * for Denemo, a gtk+ frontend to GNU Lilypond 
 * 
 * (c) 2003-2005 Richard Shann <richard.shann@virgin.net>
 */

#include "utils.h"
#include <denemo/denemo.h>
#include <string.h>
static gboolean  substitute_accs(gchar *str) {
  gint accs=FALSE;
  for(;*str;str++) {   
	if(*str=='_') {
	  if(*(str+1)) {	  
	    if(*(str+1)=='+') {
	      *str++='#';
	      *str=' ';	      
	    } else
	      if(*(str+1)=='-') {
		*str++='b';
		*str=' ';
	
	      } else
		if(*(str+1)=='|')
		  *str++=' ';	   
	  } else 
	    *str=' ';
	} else
	  if(g_ascii_isdigit(*str)) {
	     if(*(str+1)=='+') {
	       str++;
	       *str='#';
	       accs = TRUE;
	     } else
	       if(*(str+1)=='-') {
	       str++;
	       *str='b';
	       accs = TRUE;
	     }
	  }
	// else
	// if(*str=='~')
	//   *str=' '; 	  
  }
  return accs;
}
/**
 * Draw figured bass on the score
 *
 */
void
draw_figure (cairo_t *cr,
	     gint xx, gint y, DenemoObject * theobj)
{
  gchar *text = NULL;
  chord *ch;
  if (theobj->type == CHORD)
    {
      ch = (chord *) theobj->object;     
      gint ystep=0, xstep=0;
      gint accs=FALSE;
      gchar *orig = ((GString *) (ch->figure))->str;
      text = g_strdup(orig);
      gchar *str = strtok (text, " ");
      while(str) {
	
	if(*str=='|') {	 
	  ystep = 0;
	  xstep += 25 + (accs?10:0);
	  accs = FALSE;
	} else
	  if(*(orig + (str-text) + strlen(str))==' ' || *(orig + (str-text) + strlen(str))==0 ) {
	    if(substitute_accs(str))
	      accs = TRUE;
	  drawnormaltext_cr( cr, str, xx+xstep, y + STAFF_HEIGHT + 10 + ystep );
	  ystep+=16;
	}
	str = strtok (NULL, " ");
      }	
	g_free(text);
    }
  //g_debug ("%s\n", text);
}


