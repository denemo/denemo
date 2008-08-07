#ifndef TRANSPOSE_H
#define TRANSPOSE_H
#include <denemo/denemo.h>

void transpose_entire_piece(DenemoGUI *gui);

void transpose_staff (DenemoGUI *gui, gint amount);

gboolean staff_transposition (GtkAction * action);
#endif /*TRANSPOSE_H*/

