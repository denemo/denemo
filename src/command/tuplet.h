/* tuplet.h
 * 
 * tuplet function prototypes
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Adam Tee, Matthew Hiller
 */



#include <denemo/denemo.h>

DenemoObject *newtupopen (gint numerator, gint denominator);

DenemoObject *newtupclose ();

void tupletchangedialog (DenemoObject * newobj, GtkWidget * scorearea);
