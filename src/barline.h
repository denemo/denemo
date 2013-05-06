#ifndef BARLINE_H
#define BARLINE_H


#include <denemo/denemo.h>
void insert_barline (GtkAction * action, gpointer param);

DenemoObject *newbarline (enum barline_type type);

void add_barline (GtkWidget * widget, gpointer data);
enum barline_type barlinefromname (gchar * thetext);

#endif
