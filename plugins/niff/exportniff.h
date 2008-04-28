/* exportmudela.h
 * Header file for mudela exportation

 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000, 2001 Matthew Hiller */

#include <denemo/denemo.h>
#include <gtk/gtk.h>
#include <niff/niffio.h>

void exportniff (gchar * thefilename, struct scoreinfo *si, gint start,
		   gint end);

void
doerror(const char *strMessage);

void doheight(int height);

void dowidth(int width);

void doabsolute(int horz, int vert);

void dostafflines(int lines) ;

RATIONAL makerational(int numerator, int denominator);

int determineclef (gint type);

void dosmall(void);

void saveniff(GtkWidget *widget, gpointer data);
void save(GtkWidget *widget, gpointer data);
void MyWriteStringTable(void);
