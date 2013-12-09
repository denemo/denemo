/* exportabc.h
 * Header file for exporting ABC files
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2001 Eric Galluzzo
 */


#ifndef EXPORTABC_H
#define EXPORTABC_H

#include <denemo/denemo.h>

void exportabc (gchar * thefilename, DenemoProject * gui, gint start, gint end);


#endif
