#ifndef CSOUNDPLAYBACK_H
#define CSOUNDPLAYBACK_H

#include <denemo/denemo.h>
/*
struct cs_callback
{
	GtkWidget *entry;
	GtkWidget *dialog;
	DenemoGUI *gui;
	
};
*/
void csoundplayback(GtkAction *action, gpointer param);
void chooseorcfile (GtkWidget * widget, struct cs_callback *data);

#endif
