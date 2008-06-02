/* drawingprims.h
 * header file for the drawing primitive .c files,
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */


#include <denemo/denemo.h> 



void
draw_rest (GdkPixmap *pixmap, GdkGC *gc, gint baseduration, gint numdots,
	   gint xx, gint y);

void
draw_notehead (GdkPixmap *pixmap, GdkGC *gc,
	       note *thenote, gint baseduration,
	       gint numdots, gint xx, gint y, gint *accs, gint is_stemup);

void
draw_ledgers (GdkPixmap *pixmap, GdkGC *gc,
	      gint greaterheight, gint lesserheight,
	      gint xx, gint y, gint width);

void
draw_chord (GdkPixmap *pixmap, GdkGC *gc,
	    objnode *curobj, gint xx, gint y, gint mwidth, gint *accs, gboolean selected);
void
draw_tuplet (GdkPixmap *pixmap, GdkGC *gc,
	     objnode *curobj, gint xx, gint y, gint mwidth, gint *accs);

void
draw_clef (GdkPixmap *pixmap, GdkGC *gc, gint xx, gint y, gint type);

gint
draw_key (GdkPixmap *pixmap, GdkGC *gc, gint xx, gint y,
	  gint number, gint prevnumber, gint dclef, gint wetrun);

void
draw_timesig (GdkPixmap *pixmap, GdkGC *gc, GdkFont *font,
	      gint xx, gint y, gint time1, gint time2);

void
draw_tupbracket (GdkPixmap *pixmap, GdkGC *gc, GdkFont *font,
		 gint xx, gint y, DenemoObject *theobj);

void
draw_cursor (GdkPixmap *pixmap, DenemoScore *si,
	     gint xx, gint y, input_mode mode, gint dclef);

void
draw_accidental (GdkPixmap *pixmap, GdkGC *gc,
		 gint xx, gint aggheight, gint enshift);

void
draw_selection (GdkPixmap *pixmap, GdkGC *gc, gint x1, gint y1,
		gint x2, gint y2);

void
draw_stem_directive (GdkPixmap * pixmap, GdkGC * gc, GdkFont *font,
		     gint xx, gint y, DenemoObject * theobj);

void
draw_dynamic(GdkPixmap *pixmap, GdkGC *gc, GdkFont *font,
	      gint xx, gint y, DenemoObject *theobj);

void
draw_lily_dir(GdkPixmap *pixmap, GdkGC *gc, GdkFont *font,
	      gint xx, gint y, gint highy, gint lowy, DenemoObject *theobj, gboolean selected);

void
draw_gracebracket (GdkPixmap *pixmap, GdkGC *gc, GdkFont *font,
		   gint xx, gint y, DenemoObject *theobj);


gint calc_offset(chord thechord, gint stemdir);
void draw_articulations(GdkPixmap *pixmap, GdkGC *gc,
			chord thechord, gint xx, gint y);

void draw_lyric(GdkPixmap *pixmap, GdkGC *gc, GdkFont *font,
                gint xx, gint y, DenemoObject *theobj);

void draw_figure(GdkPixmap *pixmap, GdkGC *gc, GdkFont *font,
                        gint xx, gint y, DenemoObject *theobj);

void draw_fakechord(GdkPixmap *pixmap, GdkGC *gc, GdkFont *font,
                        gint xx, gint y, DenemoObject *theobj);

void
drawbarline(GdkPixmap *pixmap, GdkGC *gc,gint xx, gint top_y,gint y, gint type);
