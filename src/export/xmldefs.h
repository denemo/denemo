/* xmldefs.h
 * Definitions common to importing and exporting Denemo's "native" XML file
 * format
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2001-2005 Eric Galluzzo
 */

#ifndef XMLDEFS_H
#define XMLDEFS_H

#include <denemo/denemo.h>

#include <glib.h>

/* The namespace in which built-in Denemo XML elements reside */
#define DENEMO_XML_NAMESPACE "http://denemo.org/xmlns/Denemo"
/* some strings useful elsewhere */
#define    NONE_STRING "None"
#define    PIANO_START_STRING "PianoContextStart"
#define    PIANO_END_STRING "PianoContextEnd"
#define    CHOIR_START_STRING "ChoirContextStart"
#define    CHOIR_END_STRING "ChoirContextEnd"
#define    GROUP_START_STRING "StaffGroupStart"
#define    GROUP_END_STRING "StaffGroupEnd"

#define CURRENT_XML_VERSION (8) /* the highest version of the .denemo xml format supported */


#endif
