/* importxml.h
 * Header file for importing "native" Denemo XML files
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2001-2005 Eric Galluzzo
 */

#ifndef IMPORTXML_H
#define IMPORTXML_H

#include <denemo/denemo.h>
#include "export/xmldefs.h"

#include <glib.h>

/* libxml includes: for libxml2 this should be <libxml/tree.h> */
#include <libxml/tree.h>


/*
 * This is the "interface" that all import handlers must "implement" if they
 * wish to import data into Denemo.  If any of the callbacks in the structure
 * are NULL, then they are not called.  However, all other fields must be
 * non-NULL.
 */
typedef struct _DenemoImportXMLNSHandler
{
  /*
   * The XML namespace URI handled by this handler (e.g.
   * "http://www.denemo.sourceforge.net/Denemo/Lilypond")
   */
  gchar *xmlnsURI;

  /*
   * Create and return user data necessary for importing the given score.  If
   * no user data is required, set this startScore function to NULL.
   */
    gpointer (*startScore) (xmlDocPtr doc);

  /*
   * Delete the given user data.  If no user data is required, set this
   * endScore function to NULL.
   */
  void (*endScore) (xmlDocPtr doc, gpointer userData);

  /*
   * Import the given score-level XML element into the given score.
   */
  void (*importScoreInfo) (DenemoMovement * si, xmlNodePtr elem);

  /*
   * Import the given staff-level XML element into the "primary" staff located
   * in the given score (si->currentprimarystaff).
   */
  void (*importStaffInfo) (DenemoMovement * si, xmlNodePtr elem);

  /*
   * Import the given voice-level XML element into the voice located in the
   * given score (si->currentstaff).  Note that elem may be a child of
   * <voice-info> or <voice-init-params>.
   */
  void (*importVoiceInfo) (DenemoMovement * si, xmlNodePtr elem);

  /*
   * Import the given measure-level XML element into the measure located in
   * the given score (si->currentmeasure).
   */
  void (*importMeasure) (DenemoMovement * si, xmlNodePtr elem);

  /*
   * Import the given object-level XML element into the DenemoObject located
   * in the given score (si->currentobj).  Note that this function is only
   * called for built-in object types, not custom types.
   */
  void (*importObjectInfo) (DenemoMovement * si, xmlNodePtr elem);

  /*
   * Import the given note-level XML element into the note located in the
   * given score (N.B.: not yet implemented!).
   */
  void (*importNoteInfo) (DenemoMovement * si, xmlNodePtr elem);

  /*
   * Create a new DenemoObject from the given object-level XML element and
   * return it.  Return NULL if no object should be created.
   */
  DenemoObject *(*importCustomObject) (DenemoMovement * si, xmlNodePtr elem);
} DenemoImportXMLNSHandler;



/*
 * Import the given file in Denemo's "native" XML file format into the given
 * score.  Return TRUE if the file was imported successfully, FALSE otherwise.
 */
gboolean importXML (gchar * filename, DenemoProject * gui, ImportType type);

void registerImportXMLNSHandler (DenemoImportXMLNSHandler * handler);

void unregisterImportXMLNSHandler (DenemoImportXMLNSHandler * handler);

#endif
