/* exportxml.h
 * Header file for exporting "native" Denemo XML files
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2001 Eric Galluzzo
 */

#ifndef EXPORTXML_H
#define EXPORTXML_H

#include <denemo/denemo.h>
#include "export/xmldefs.h"

#include <glib.h>

/* libxml includes: for libxml2 this should be <libxml/tree.h> */
#include <libxml/tree.h>


/*
 * This is the "interface" that all export handlers must "implement" if they
 * wish to export data from Denemo.  If any of the callbacks in the structure
 * are NULL, then they are not called.  However, all other fields must be
 * non-NULL.
 */
typedef struct _DenemoExportXMLNSHandler
{
  /*
   * The prefix of the XML namespace (e.g. "lily")
   */
  gchar *xmlnsPrefix;

  /*
   * The actual XML namespace URI (e.g.
   * "http://www.denemo.sourceforge.net/Denemo/Lilypond")
   */
  gchar *xmlnsURI;

  /*
   * The DenemoObject types for which to call this structure's exportObjectInfo
   * and createXMLObject callbacks
   */
  gint numHandlerObjectTypes;
  gint *handlerObjectTypes;

  /*
   * Create and return user data necessary for importing the given score.  If
   * no user data is required, set this startScore function to NULL.
   */
    gpointer (*startScore) (DenemoMovement * si);

  /*
   * Delete the given user data.  If no user data is required, set this
   * endScore function to NULL.
   */
  void (*endScore) (DenemoMovement * si, gpointer userData);

  /*
   * Export any information related to this score as children of
   * scoreInfoElem.  All XML elements should be in the given namespace.
   */
  void (*exportScoreInfo) (DenemoMovement * si, xmlNsPtr ns, xmlNodePtr scoreInfoElem);

  /*
   * Export any information related to this staff as children of
   * staffInfoElem.  All XML elements should be in the given namespace.
   */
  void (*exportStaffInfo) (DenemoMovement * si, xmlNsPtr ns, xmlNodePtr staffInfoElem);

  /*
   * Export any metadata about this voice as children of voiceInfoElem.  All
   * XML elements should be in the given namespace.
   */
  void (*exportVoiceInfo) (DenemoMovement * si, xmlNsPtr ns, xmlNodePtr voiceInfoElem);

  /*
   * Export any non-built-in initial parameters for this voice as children of
   * voiceInitParamsElem.  All XML elements should be in the given namespace.
   */
  void (*exportVoiceInitParams) (DenemoMovement * si, xmlNsPtr ns, xmlNodePtr voiceInitParamsElem);

  /*
   * Export any information related to this measure as children of measureElem.
   * All XML elements should be in the given namespace.
   */
  void (*exportMeasure) (DenemoMovement * si, xmlNsPtr ns, xmlNodePtr measureElem);

  /*
   * Export any information related to this built-in type DenemoObject as
   * children of objectElem.  All XML elements should be in the given
   * namespace.
   */
  void (*exportObjectInfo) (DenemoMovement * si, xmlNsPtr ns, xmlNodePtr objectElem);

  /*
   * Export any information related to this note as children of chordElem.
   * All XML elements should be in the given namespace.
   */
  void (*exportNoteInfo) (DenemoMovement * si, xmlNsPtr ns, xmlNodePtr chordElem);

  /*
   * Create a new XML element or multiple elements corresponding to the given
   * custom type DenemoObject, as children of measureElem.  All XML elements
   * should be in the given namespace.
   */
  void (*exportCustomObject) (DenemoMovement * si, xmlNsPtr ns, xmlNodePtr measureElem);
} DenemoExportXMLNSHandler;


/*
 * Export the given score as Denemo's "native" XML file format to the given
 * filename.  The file will be exported from the measures numbered start to
 * end.
 */
gint exportXML (gchar * thefilename, DenemoProject * gui);

void registerExportXMLNSHandler (DenemoExportXMLNSHandler * handler);

void unregisterExportXMLNSHandler (DenemoExportXMLNSHandler * handler);

#endif
