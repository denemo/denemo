/* importxml.cpp
 * Import Denemo's "native" XML file format into a Denemo score structure
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2001-2005 Eric Galluzzo, Adam Tee
 */

#include "chordops.h"
#include "graceops.h"
#include "importxml.h"
#include "measureops.h"
#include "objops.h"
#include "scoreops.h"
#include "staffops.h"
#include "processstaffname.h"
#include "tupletops.h"
#include "xmldefs.h"
#include "articulations.h"
#include <string.h>
#include <glib.h>
/* libxml includes: for libxml2 this should be <libxml.h> */
#include <libxml/parser.h>
#include <libxml/tree.h>


/* Defines for making traversing XML trees easier */

#define FOREACH_CHILD_ELEM(childElem, parentElem) \
for ((childElem) = (parentElem)->xmlChildrenNode; \
     (childElem) != NULL; \
     (childElem) = (childElem)->next)

#define ELEM_NAME_EQ(childElem, childElemName) \
(strcmp ((gchar *)(childElem)->name, (childElemName)) == 0)

#define ILLEGAL_ELEM(parentElemName, childElem) \
do \
  { \
    g_warning ("Illegal element inside <%s>: <%s>", parentElemName, \
               (childElem)->name); \
  } while (0)

#define RETURN_IF_ELEM_NOT_FOUND(parentElemName, childElem, childElemName) \
do \
  { \
    if (childElem == NULL) \
      { \
        g_warning ("Element <%s> not found inside <%s>", childElemName, \
                   parentElemName); \
        return -1; \
      } \
  } while (0)


/* The list of handlers for elements in other XML namespaces */
/*static GList *sImportHandlers = NULL;*/

/*
 * The global XML ID to element map
 *
 * FIXME: This won't work for multi-threaded apps.
 */
static GHashTable *sXMLIDToElemMap = NULL;
/*
 * The previous staff element that we came across
 *
 * FIXME: This won't work for multi-threaded apps, but I'm too lazy to pass
 *        this silly thing around everywhere.
 */
static xmlNodePtr sPrevStaffElem = NULL;


/**
 * Free a hash table key.  This function is suitable for use with
 * g_hash_table_foreach.
 */
static void
freeHashTableKey (gpointer key, gpointer value, gpointer userData)
{
  g_free (key);
}


/**
 * Recursively descend the tree starting from node elem and add any element
 * which has an "id" attribute to the map.
 */
static void
buildXMLIDMapForChildren (xmlNodePtr elem)
{
  xmlNodePtr childElem;
  gchar *id;
  if (elem != NULL)
    {
      id = (gchar *) xmlGetProp (elem, (xmlChar *) "id");
      if (id != NULL)
	{
	  g_hash_table_insert (sXMLIDToElemMap, id, elem);
	}

      FOREACH_CHILD_ELEM (childElem, elem)
      {
	buildXMLIDMapForChildren (childElem);
      }
    }
}


/**
 * Build the global (ick) XML ID to element map from the given document.  We
 * consider any attribute with name "id" to be of type ID.  Not technically
 * correct, but currently true.
 */
static void
buildXMLIDToElemMap (xmlDocPtr doc)
{
  sXMLIDToElemMap = g_hash_table_new (g_str_hash, g_str_equal);

  buildXMLIDMapForChildren (xmlDocGetRootElement (doc));
}


/**
 * Try to find the given XML ID in the ID -> XML element map.  If it's found,
 * return it; if not, return NULL.
 */
static xmlNodePtr
lookupXMLID (gchar * id)
{
  return (xmlNodePtr) g_hash_table_lookup (sXMLIDToElemMap, id);
}


/**
 * Get the text from the child node list of elem, convert it to an integer,
 * and return it.  If unsuccessful, return G_MAXINT.
 */
static gint
getXMLIntChild (xmlNodePtr elem)
{
  gchar *text =
    (gchar *) xmlNodeListGetString (elem->doc, elem->xmlChildrenNode, 1);
  gint num = G_MAXINT;
  if (text == NULL)
    {
      g_warning ("No child text found");
    }
  else
    {
      if (sscanf (text, " %d", &num) != 1)
	{
	  g_warning ("Could not convert child text \"%s\" of <%s> to number",
		     text, elem->name);
	  num = G_MAXINT;
	}
      g_free (text);
    }
  return num;
}


/**
 * Try to find the element with the given name and namespace as an immediate
 * child of the given parent element.  If found, return the child element; if
 * not, return NULL.
 */
static xmlNodePtr
getXMLChild (xmlNodePtr parentElem, gchar * childElemName, xmlNsPtr childNs)
{
  xmlNodePtr childElem;

  FOREACH_CHILD_ELEM (childElem, parentElem)
    if (ELEM_NAME_EQ (childElem, childElemName) && childElem->ns == childNs)
    return childElem;

  return NULL;
}


/**
 * Convert textual context to a denemo context
 *
 */
static DenemoContext
lookupContext (gchar * string)
{
  if (!strcmp (string, "Staff"))
    {
      return DENEMO_NONE;
    }
  else if (!strcmp (string, "PianoStaff"))
    {
      return DENEMO_PIANO;
    }
  else if (!strcmp (string, "ChoirStaff"))
    {
      return DENEMO_CHOIR;
    }
  else if (!strcmp (string, "StaffGroup"))
    {
      return DENEMO_GROUP;
    }
  else
    {
      return DENEMO_NONE;
    }
}

/**
 * Return the numerator and denominator from the given XML fraction.
 */
static gint
parseFraction (xmlNodePtr parentElem, xmlNsPtr ns,
	       gint * numerator, gint * denominator)
{
  xmlNodePtr childElem;
  gboolean gotNumerator = FALSE, gotDenominator = FALSE;
  gboolean gotCorrectNumerator = FALSE, gotCorrectDenominator = FALSE;

  FOREACH_CHILD_ELEM (childElem, parentElem)
  {
    if (childElem->ns == ns)
      {
	if (ELEM_NAME_EQ (childElem, "numerator"))
	  {
	    if (gotNumerator)
	      {
		g_warning ("Two numerators in the same fraction under <%s>",
			   parentElem->name);
		gotCorrectNumerator = FALSE;
	      }
	    else
	      {
		gotNumerator = TRUE;
		*numerator = getXMLIntChild (childElem);
		if (*numerator == G_MAXINT)
		  *numerator = 1;
		else
		  gotCorrectNumerator = TRUE;
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "denominator"))
	  {
	    if (gotDenominator)
	      {
		g_warning ("Two numerators in the same fraction under <%s>",
			   parentElem->name);
		gotCorrectDenominator = FALSE;
	      }
	    else
	      {
		gotDenominator = TRUE;
		*denominator = getXMLIntChild (childElem);
		if (*denominator == G_MAXINT)
		  *denominator = 1;
		else
		  gotCorrectDenominator = TRUE;
	      }
	  }
      }
    else
      {
	ILLEGAL_ELEM (parentElem->name, childElem);
      }
  }

  if (!gotNumerator)
    {
      g_warning
	("Fraction's numerator not found inside <%s>; defaulting to 1",
	 parentElem->name);
      *numerator = 1;
    }

  if (!gotDenominator)
    {
      g_warning
	("Fraction's denominator not found inside <%s>; defaulting to " "1",
	 parentElem->name);
      *denominator = 1;
    }

  return (gotCorrectNumerator && gotCorrectDenominator) ? 0 : -1;
}


/**
 * From the given XML accidental name, determine the accidental shift.
 */
static gint
determineAccidentalShift (gchar * accidentalName)
{
  if (strcmp (accidentalName, "natural") == 0)
    return 0;
  else if (strcmp (accidentalName, "sharp") == 0)
    return 1;
  else if (strcmp (accidentalName, "flat") == 0)
    return -1;
  else if (strcmp (accidentalName, "double-sharp") == 0)
    return 2;
  else if (strcmp (accidentalName, "double-flat") == 0)
    return -2;
  else
    {
      g_warning ("Unknown accidental \"%s\"; defaulting to natural",
		 accidentalName);
      return 0;
    }
}


/**
 * Parse the given <clef> element into a numeric clef type.
 */
static void
parseClef (xmlNodePtr clefElem, xmlNsPtr ns, gint * clefType)
{
  gchar *clefTypeName = (gchar *) xmlGetProp (clefElem, (xmlChar *) "name");
  if (clefTypeName == NULL)
    {
      g_warning ("No clef name specified; defaulting to treble");
      *clefType = DENEMO_TREBLE_CLEF;
    }
  else if (strcmp (clefTypeName, "treble") == 0)
    *clefType = DENEMO_TREBLE_CLEF;
  else if (strcmp (clefTypeName, "bass") == 0)
    *clefType = DENEMO_BASS_CLEF;
  else if (strcmp (clefTypeName, "alto") == 0)
    *clefType = DENEMO_ALTO_CLEF;
  else if (strcmp (clefTypeName, "treble-8vb") == 0)
    *clefType = DENEMO_G_8_CLEF;
  else if (strcmp (clefTypeName, "tenor") == 0)
    *clefType = DENEMO_TENOR_CLEF;
  else if (strcmp (clefTypeName, "soprano") == 0)
    *clefType = DENEMO_SOPRANO_CLEF;
  else
    {
      g_warning ("Unknown clef type \"%s\"; defaulting to treble",
		 clefTypeName);
      *clefType = DENEMO_TREBLE_CLEF;
    }
  g_free (clefTypeName);

}


/**
 * Parse the given <key-signature> element into a key signature (with the
 * given number of sharps plus whether it's minor or not).
 */
static void
parseKeySignature (xmlNodePtr keySigElem, xmlNsPtr ns,
		   gint * keySig, gboolean * isMinor)
{
  xmlNodePtr childElem;
  gint childCount = 0;
  gboolean successful = FALSE;
  gchar *noteName, *accidentalName, *modeName;
  gint note, accidental;

  FOREACH_CHILD_ELEM (childElem, keySigElem)
  {
    if (childElem->ns == ns)
      {
	if (++childCount > 1)
	  {
	    /* Only print this warning once (when the child count is 2). */
	    if (childCount == 2)
	      g_warning ("<key-signature> element should only have one "
			 "child (<modal-key-signature>)");
	  }
	else
	  {
	    if (ELEM_NAME_EQ (childElem, "modal-key-signature"))
	      {
		noteName = (gchar *) xmlGetProp
		  (childElem, (xmlChar *) "note-name");
		accidentalName = (gchar *) xmlGetProp
		  (childElem, (xmlChar *) "accidental");
		modeName =
		  (gchar *) xmlGetProp (childElem, (xmlChar *) "mode");

		if (noteName == NULL)
		  {
		    g_warning ("<modal-key-signature> should have a "
			       "note-name attribute; defaulting to C");
		    noteName = "C";
		  }
		if (accidentalName == NULL)
		  accidentalName = "natural";
		if (modeName == NULL)
		  {
		    g_warning ("<modal-key-signature> should have a "
			       "mode attribute; defaulting to major");
		    modeName = "major";
		  }

		/* Translate note name (A to G) into note number (0 to 6). */

		note = noteName[0] - 'A';
		if (strlen (noteName) != 1 || note < 0 || note > 6)
		  {
		    g_warning ("<modal-key-signature> note name should be A "
			       "through G, received \"%s\"; defaulting to C",
			       noteName);
		    note = 2;
		  }

		accidental = determineAccidentalShift (accidentalName);

		/* Try to determine the base key signature. */

		if (note == 2 && accidental == -1)
		  *keySig = -7;
		else if (note == 6 && accidental == -1)
		  *keySig = -6;
		else if (note == 3 && accidental == -1)
		  *keySig = -5;
		else if (note == 0 && accidental == -1)
		  *keySig = -4;
		else if (note == 4 && accidental == -1)
		  *keySig = -3;
		else if (note == 1 && accidental == -1)
		  *keySig = -2;
		else if (note == 5 && accidental == 0)
		  *keySig = -1;
		else if (note == 2 && accidental == 0)
		  *keySig = 0;
		else if (note == 6 && accidental == 0)
		  *keySig = 1;
		else if (note == 3 && accidental == 0)
		  *keySig = 2;
		else if (note == 0 && accidental == 0)
		  *keySig = 3;
		else if (note == 4 && accidental == 0)
		  *keySig = 4;
		else if (note == 1 && accidental == 0)
		  *keySig = 5;
		else if (note == 5 && accidental == 1)
		  *keySig = 6;
		else if (note == 2 && accidental == 1)
		  *keySig = 7;
		else if (note == 6 && accidental == 1)
		  *keySig = 8;
		else if (note == 3 && accidental == 1)
		  *keySig = 9;
		else if (note == 0 && accidental == 1)
		  *keySig = 10;
		else
		  {
		    g_warning ("Unknown key signature with note name %s and "
			       "accidental %s; defaulting to C", noteName,
			       accidentalName);
		    *keySig = 0;
		  }

		/* Determine whether it's major or minor. */

		if (strcmp (modeName, "major") == 0)
		  *isMinor = FALSE;
		else if (strcmp (modeName, "minor") == 0)
		  *isMinor = TRUE;
		else
		  {
		    g_warning ("Unknown mode %s; defaulting to major",
			       modeName);
		    *isMinor = FALSE;
		  }

		successful = TRUE;

		g_free (noteName);
		//g_free (accidentalName);
		g_free (modeName);
	      }
	  }
      }

    /*
     * Note: We can ignore other namespaces because the generic "parse this
     *       DenemoObject" code takes care of them for us.
     */
  }

  if (childCount == 0)
    g_warning ("<key-signature> element should have a child in the Denemo "
	       "namespace (<modal-key-signature>); defaulting to C Major");

  if (!successful)
    {
      *keySig = 0;
      *isMinor = FALSE;
    }

  if (*isMinor)
    *keySig -= 3;
}


/**
 * Parse the given <key-signature> element into a key signature (with the
 * given number of sharps plus whether it's minor or not).
 */
static void
parseTimeSignature (xmlNodePtr timeSigElem, xmlNsPtr ns,
		    gint * numerator, gint * denominator)
{
  xmlNodePtr childElem;
  gint childCount = 0;
  gboolean successful = FALSE;

  FOREACH_CHILD_ELEM (childElem, timeSigElem)
  {
    if (childElem->ns == ns)
      {
	if (++childCount > 1)
	  {
	    /* Only print this warning once (when the child count is 2). */
	    if (childCount == 2)
	      g_warning ("<time-signature> element should only have one "
			 "child (<simple-time-signature>)");
	  }
	else
	  {
	    if (ELEM_NAME_EQ (childElem, "simple-time-signature"))
	      {
		if (parseFraction (childElem, ns, numerator, denominator)
		    != 0)
		  g_warning ("Could not parse <simple-time-signature>; "
			     "defaulting to 4/4");
		else
		  successful = TRUE;
	      }
	  }
      }

    /*
     * Note: We can ignore other namespaces because the generic "parse this
     *       DenemoObject" code takes care of them for us.
     */
  }

  if (childCount == 0)
    g_warning ("<time-signature> element should have a child in the Denemo "
	       "namespace (<simple-time-signature>); defaulting to 4/4");

  if (!successful)
    {
      *numerator = 4;
      *denominator = 4;
    }
}

/**
 * Parse the given <lyric> into the current chord object
 *
 *
 */
static void
parseLyric (xmlNodePtr lyricElem, DenemoObject * curobj)
{
  gchar *lyric = (gchar *) xmlNodeListGetString (lyricElem->doc,
						 lyricElem->xmlChildrenNode,
						 1);

  gchar *extend = (gchar *) xmlGetProp (lyricElem, (xmlChar *) "extend");
  gchar *center = (gchar *) xmlGetProp (lyricElem, (xmlChar *) "center");

  ((chord *) curobj->object)->lyric = g_string_new (lyric);

  if (!strcmp (extend, "true"))
    ((chord *) curobj->object)->is_syllable = TRUE;
  else
    ((chord *) curobj->object)->is_syllable = FALSE;

  if (!strcmp (center, "true"))
    ((chord *) curobj->object)->center_lyric = TRUE;
  else
    ((chord *) curobj->object)->center_lyric = FALSE;

  g_free (lyric);
  g_free (extend);
  g_free (center);

}





/**
 * Parse the given <figure> into the current chord object
 */
static void
parseFigure (xmlNodePtr figureElem, DenemoObject * curobj)
{
  gchar *figure = (gchar *) xmlNodeListGetString (figureElem->doc,
						  figureElem->xmlChildrenNode,
						  1);
  ((chord *) curobj->object)->is_figure = TRUE;
  //chord *ch = curobj->object;
  ((chord *) curobj->object)->figure = g_string_new (figure); 
  //g_string_new (figure);
   //printf("\n figure == %s\n", ((GString *) ((chord *) curobj->object)->figure)->str);
  g_free (figure);

}

/**
 * Parse the given <fakechord> into the current chord object
 */ 
static void
parseFakechord (xmlNodePtr fakechordElem, DenemoObject * curobj)
{
  gchar *fakechord = (gchar *) xmlNodeListGetString (fakechordElem->doc,
						  fakechordElem->xmlChildrenNode,
						  1);
  
  ((chord *) curobj->object)->is_fakechord = TRUE; 
  separate_fakechord_elements (fakechord, curobj); 
  
  //((chord *) curobj->object)->fakechord = g_string_new (fakechord);

  g_free (fakechord);

}

/**
 * Parse the given <note> element into a note structure and add it to the
 * given chord.
 */
static void
parseNote (xmlNodePtr noteElem, xmlNsPtr ns,
	   DenemoScore * si, DenemoObject * chordObj, gint currentClef)
{
  xmlNodePtr childElem;
  gint middleCOffset = 0, accidental = 0,
    noteHeadType = DENEMO_NORMAL_NOTEHEAD;
  gboolean showAccidental = FALSE;
  gchar *accidentalName, *showAccidentalProp, *noteHeadName, *directive = NULL;

  FOREACH_CHILD_ELEM (childElem, noteElem)
  {
    if (childElem->ns == ns)
      {
	if (ELEM_NAME_EQ (childElem, "middle-c-offset"))
	  {
	    middleCOffset = getXMLIntChild (childElem);
	    if (middleCOffset == G_MAXINT)
	      {
		g_warning ("Couldn't get middle C offset; defaulting to 0");
		middleCOffset = 0;
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "accidental"))
	  {
	    accidentalName =
	      (gchar *) xmlGetProp (childElem, (xmlChar *) "name");
	    if (accidentalName == NULL)
	      {
		g_warning ("<accidental> had no name attribute; defaulting "
			   "to natural");
		accidental = 0;
	      }
	    else if (strcmp (accidentalName, "natural") == 0)
	      accidental = 0;
	    else if (strcmp (accidentalName, "flat") == 0)
	      accidental = -1;
	    else if (strcmp (accidentalName, "sharp") == 0)
	      accidental = 1;
	    else if (strcmp (accidentalName, "double-flat") == 0)
	      accidental = -2;
	    else if (strcmp (accidentalName, "double-sharp") == 0)
	      accidental = 2;
	    else
	      {
		g_warning ("Unknown accidental name \"%s\"; defaulting to "
			   "natural", accidentalName);
		accidental = 0;
	      }
	    g_free (accidentalName);

	    showAccidentalProp =
	      (gchar *) xmlGetProp (childElem, (xmlChar *) "show");
	    if (showAccidentalProp != NULL)
	      {
		if (strcmp (showAccidentalProp, "true") == 0)
		  showAccidental = TRUE;
		else if (strcmp (showAccidentalProp, "false") == 0)
		  showAccidental = FALSE;
		else
		  {
		    g_warning ("Unknown show accidental attribute value "
			       "\"%s\" (should be true or false); "
			       "defaulting to false", showAccidentalProp);
		  }
		g_free (showAccidentalProp);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "directive"))
	  {
	    directive = (gchar *) xmlNodeListGetString
	      (childElem->doc, childElem->xmlChildrenNode, 1);	    
	  }
	else if (ELEM_NAME_EQ (childElem, "note-head"))
	  {
	    noteHeadName =
	      (gchar *) xmlGetProp (childElem, (xmlChar *) "type");
	    if (noteHeadName == NULL)
	      {
		g_warning ("<note-head> element had no type attribute; "
			   "defaulting to normal");
		noteHeadType = DENEMO_NORMAL_NOTEHEAD;
	      }
	    else if (strcmp (noteHeadName, "normal") == 0)
	      noteHeadType = DENEMO_NORMAL_NOTEHEAD;
	    else if (strcmp (noteHeadName, "cross") == 0)
	      noteHeadType = DENEMO_CROSS_NOTEHEAD;
	    else if (strcmp (noteHeadName, "harmonic") == 0)
	      noteHeadType = DENEMO_HARMONIC_NOTEHEAD;
	    else if (strcmp (noteHeadName, "diamond") == 0)
	      noteHeadType = DENEMO_DIAMOND_NOTEHEAD;
	    else
	      {
		g_warning ("Unknown notehead type \"%s\"; defaulting to "
			   "normal", noteHeadName);
		noteHeadType = DENEMO_NORMAL_NOTEHEAD;
	      }
	    g_free (noteHeadName);
	  }
      }

    /*
     * The code near the bottom of parseMeasures() handles other namespaces
     * inside <note>s, so we won't do anything here.
     */
  }

  /* Now actually construct the note object. */

  note *newnote = addtone (chordObj, middleCOffset, accidental, currentClef);
  if(newnote && directive)
    newnote->directive = g_string_new(directive);
  if (noteHeadType != DENEMO_NORMAL_NOTEHEAD)
    {
      /* FIXME: Is this the right note in the chord? */
      ((note *) ((chord *) chordObj->object)->notes->data)->noteheadtype =
	(enum headtype) noteHeadType;
    }
}


/**
 * Parse the given <chord> or <rest>'s duration only, not any other
 * subelements, and return a simple DenemoObject corresponding to that chord
 * or rest.
 */
static DenemoObject *
parseBaseChord (xmlNodePtr chordElem, xmlNsPtr ns, DenemoScore * si)
{
  xmlNodePtr durationElem, dotsElem;
  gboolean successful = FALSE;
  gchar *durationType;
  gint baseDuration = 2, numDots = 0;

  /*
   * First, in order to actually create a chord object, we must figure out the
   * Denemo duration and number of dots from the XML duration.
   */
  durationElem = getXMLChild (chordElem, "duration", ns);
  if (durationElem == NULL)
    {
      g_warning ("No duration found in chord; defaulting to quarter note");
    }
  else
    {
      durationType = (gchar *) xmlGetProp (durationElem, (xmlChar *) "base");
      if (durationType == NULL)
	{
	  g_warning ("No base attribute found in chord duration; defaulting "
		     "to quarter note");
	}
      else
	{
	  if (strcmp (durationType, "whole") == 0)
	    baseDuration = 0;
	  else if (strcmp (durationType, "half") == 0)
	    baseDuration = 1;
	  else if (strcmp (durationType, "quarter") == 0)
	    baseDuration = 2;
	  else if (strcmp (durationType, "eighth") == 0)
	    baseDuration = 3;
	  else if (strcmp (durationType, "sixteenth") == 0)
	    baseDuration = 4;
	  else if (strcmp (durationType, "thirty-second") == 0)
	    baseDuration = 5;
	  else if (strcmp (durationType, "sixty-fourth") == 0)
	    baseDuration = 6;
	  else
	    {
	      g_warning ("Unknown base duration type \"%s\"; defaulting to "
			 "quarter note", durationType);
	    }
	  g_free (durationType);

	  successful = TRUE;
	  dotsElem = getXMLChild (durationElem, "dots", ns);
	  if (dotsElem != NULL)
	    {
	      numDots = getXMLIntChild (dotsElem);
	      if (numDots == G_MAXINT)
		{
		  g_warning ("Invalid number of dots; defaulting to 0");
		  numDots = 0;
		}
	    }
	}
    }

  if (!successful)
    {
      baseDuration = 2;
      numDots = 0;
    }

  return newchord (baseDuration, numDots, 0);
}


/**
 * Parse the given <rest> element and return a chord-type DenemoObject.
 */
static DenemoObject *
parseRest (xmlNodePtr restElem, xmlNsPtr ns, DenemoScore * si)
{
  gchar *showProp = (gchar *) xmlGetProp (restElem, (xmlChar *) "show");
  gboolean show = TRUE;
  if (showProp != NULL)
    {
      if (strcmp (showProp, "true") == 0)
	show = TRUE;
      else if (strcmp (showProp, "false") == 0)
	show = FALSE;
      else
	{
	  g_warning ("Invalid value for show attribute of <rest>: \"%s\"; "
		     "defaulting to false", showProp);
	  show = FALSE;
	}
    }

  if (show)
    return parseBaseChord (restElem, ns, si);
  else
    return (hidechord (parseBaseChord (restElem, ns, si)));
}

/**
 * Parse the given <dynamic> into a dynamic DenemoObject.
 */
static void
parseDynamic (xmlNodePtr dynamicElem, DenemoObject * curobj)
{
  gchar *dynamicName = (gchar *) xmlGetProp (dynamicElem, (xmlChar *) "name");
  /*DenemoObject *result = NULL; */
  GString *dynString = g_string_new (dynamicName);
  if (dynamicName == NULL)
    {
      g_warning ("No \"name\" attribute on <dynamic> element; ignoring");
    }
  else
    {
      ((chord *) curobj->object)->dynamics =
	g_list_append (((chord *) curobj->object)->dynamics, dynString);
    }
  g_free (dynamicName);

}

/**
 * Parse the given <decoration> element into the given chord-type DenemoObject.
 */
static void
parseDecoration (xmlNodePtr decorationElem, DenemoObject * chordObj)
{
  gchar *decorationType = (gchar *) xmlGetProp (decorationElem,
						(xmlChar *) "type");

  if (decorationType == NULL)
    g_warning ("No \"type\" attribute present on <decoration> element; "
	       "ignoring");
  else if (strcmp (decorationType, "accent") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (D_ACCENT,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "down-bow") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (DBOW, ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "fermata") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (FERMATA,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "left-heel") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (LHEEL,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "left-toe") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (LTOE, ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "marcato") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (MARCATO,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "mordent") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (MORDENT,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "right-heel") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (RHEEL,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "right-toe") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (RTOE, ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "staccato") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (STACCATO,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "staccatissimo") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (STACCATISSIMO,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "tenuto") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (TENUTO,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "trill") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (TRILL,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "turn") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (TURN, ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "up-bow") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (UBOW, ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "coda") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (CODA, ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "flageolet") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (FLAGEOLET,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "open") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (OPEN, ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "prallmordent") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (PRALLMORDENT,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "prallprall") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (PRALLPRALL,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "prall") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (PRALL,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "reverseturn") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (REVERSETURN,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "segno") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (SEGNO,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "sforzato") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (SFORZATO,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "stopped") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (STOPPED,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "thumb") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (THUMB,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "upprall") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (UPPRALL,
			    ((chord *) chordObj->object)->ornamentlist);
  else if (strcmp (decorationType, "arpeggio") == 0)
    ((chord *) chordObj->object)->ornamentlist =
      insert_ornament_list (D_ARPEGGIO,
			    ((chord *) chordObj->object)->ornamentlist);
  else
    g_warning ("Unknown decoration type \"%s\"; ignoring", decorationType);

  g_free (decorationType);
}


/**
 * Parse the given <chord> element and return a chord-type DenemoObject.
 * 
 * @param chordElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoScore to populate 
 * @param slurEndChordElems pointer to a slur end element
 * @param crescEndChordElems pointer to a crescendo end element
 * @param diminEndChordElems pointer to a diminuendo end element
 * @param noteElem a pointer to a note XML element
 * @return the new DenemoObject
 */
static DenemoObject *
parseChord (xmlNodePtr chordElem, xmlNsPtr ns,
	    DenemoScore * si,
	    gint currentClef,
	    GList ** slurEndChordElems,
	    GList ** crescEndChordElems,
	    GList ** diminEndChordElems, xmlNodePtr * notesElem)
{
  DenemoObject *chordObj = parseBaseChord (chordElem, ns, si);
  xmlNodePtr childElem, grandchildElem;
  xmlNodePtr slurEndElem, crescEndElem, diminEndElem;
  gchar *slurEndXMLID, *crescEndXMLID, *diminEndXMLID;
  GList *thisSlurListNode, *thisCrescListNode, *thisDiminListNode;

  *notesElem = NULL;

  /* Check to see if this is the end of a slur. */

  thisSlurListNode = g_list_find (*slurEndChordElems, chordElem);
  if (thisSlurListNode != NULL)
    {
      ((chord *) chordObj->object)->slur_end_p = TRUE;
      *slurEndChordElems = g_list_remove_link (*slurEndChordElems,
					       thisSlurListNode);
      g_list_free_1 (thisSlurListNode);
    }

  /* Check to see if this is the end of a crescendo. */

  thisCrescListNode = g_list_find (*crescEndChordElems, chordElem);
  if (thisCrescListNode != NULL)
    {
      ((chord *) chordObj->object)->crescendo_end_p = TRUE;
      *crescEndChordElems = g_list_remove_link (*crescEndChordElems,
						thisCrescListNode);
      g_list_free_1 (thisCrescListNode);
    }

  /* Check to see if this is the end of a diminuendo. */

  thisDiminListNode = g_list_find (*diminEndChordElems, chordElem);
  if (thisDiminListNode != NULL)
    {
      ((chord *) chordObj->object)->diminuendo_end_p = TRUE;
      *diminEndChordElems = g_list_remove_link (*diminEndChordElems,
						thisDiminListNode);
      g_list_free_1 (thisDiminListNode);
    }

  FOREACH_CHILD_ELEM (childElem, chordElem)
  {
    if (childElem->ns == ns)
      {
	if (ELEM_NAME_EQ (childElem, "duration"))
	  {
	    /* This was already parsed during parseBaseChord(). */
	  }
	else if (ELEM_NAME_EQ (childElem, "dynamic"))
	  {
	    parseDynamic (childElem, chordObj);
	  }
	else if (ELEM_NAME_EQ (childElem, "decorations"))
	  {
	    FOREACH_CHILD_ELEM (grandchildElem, childElem)
	    {
	      if (grandchildElem->ns == ns
		  && ELEM_NAME_EQ (grandchildElem, "decoration"))
		{
		  parseDecoration (grandchildElem, chordObj);
		}
	      else
		{
		  ILLEGAL_ELEM ("decorations", grandchildElem);
		}
	    }
	  }
	else if (ELEM_NAME_EQ (childElem, "tie"))
	  {
	    /* For now at least, ignore the "to" attribute. */
	    ((chord *) chordObj->object)->is_tied = TRUE;
	  }
	else if (ELEM_NAME_EQ (childElem, "slurs"))
	  {
	    FOREACH_CHILD_ELEM (grandchildElem, childElem)
	    {
	      if (grandchildElem->ns == ns
		  && ELEM_NAME_EQ (grandchildElem, "slur"))
		{
		  /*
		   * Prepend the slur's end element to the list that we're
		   * keeping.
		   */

		  slurEndXMLID = (gchar *) xmlGetProp (grandchildElem,
						       (xmlChar *) "to");
		  if (slurEndXMLID == NULL)
		    {
		      g_warning ("No \"to\" attribute on <slur> element; "
				 "ignoring");
		    }
		  else
		    {
		      slurEndElem = lookupXMLID (slurEndXMLID);
		      if (slurEndElem == NULL)
			{
			  g_warning ("Chord is slurred to nonexistent "
				     "chord with ID \"%s\"", slurEndXMLID);
			}
		      else if (!ELEM_NAME_EQ (slurEndElem, "chord"))
			{
			  g_warning ("Chord is slurred to non-chord "
				     "element with tag name \"%s\" and ID "
				     "\"%s\"", slurEndElem->name,
				     slurEndXMLID);
			}
		      else if (slurEndElem == chordElem)
			{
			  g_warning ("Can't slur a chord to itself");
			}
		      else
			{
			  ((chord *) chordObj->object)->slur_begin_p = TRUE;
			  *slurEndChordElems =
			    g_list_prepend (*slurEndChordElems, slurEndElem);
			}
		      g_free (slurEndXMLID);
		    }
		}
	      else
		{
		  ILLEGAL_ELEM ("slurs", grandchildElem);
		}
	    }
	  }
	else if (ELEM_NAME_EQ (childElem, "crescendos"))
	  {
	    FOREACH_CHILD_ELEM (grandchildElem, childElem)
	    {
	      if (grandchildElem->ns == ns
		  && ELEM_NAME_EQ (grandchildElem, "crescendo"))
		{
		  /*
		   * Prepend the crescendo's end element to the list that we're
		   * keeping.
		   */

		  crescEndXMLID = (gchar *) xmlGetProp (grandchildElem,
							(xmlChar *) "to");
		  if (crescEndXMLID == NULL)
		    {
		      g_warning
			("No \"to\" attribute on <crescendo> element; "
			 "ignoring");
		    }
		  else
		    {
		      crescEndElem = lookupXMLID (crescEndXMLID);
		      if (crescEndElem == NULL)
			{
			  g_warning ("Chord has crescendo to nonexistent "
				     "chord with ID \"%s\"", crescEndXMLID);
			}
		      else if (!ELEM_NAME_EQ (crescEndElem, "chord"))
			{
			  g_warning ("Chord has crescendo to non-chord "
				     "element with tag name \"%s\" and ID "
				     "\"%s\"", crescEndElem->name,
				     crescEndXMLID);
			}
		      else if (crescEndElem == chordElem)
			{
			  g_warning ("Can't crescendo on one chord");
			}
		      else
			{
			  ((chord *) chordObj->object)->crescendo_begin_p =
			    TRUE;
			  *crescEndChordElems =
			    g_list_prepend (*crescEndChordElems,
					    crescEndElem);
			}
		      g_free (crescEndXMLID);
		    }
		}
	      else
		{
		  ILLEGAL_ELEM ("crescendos", grandchildElem);
		}
	    }
	  }
	else if (ELEM_NAME_EQ (childElem, "diminuendos"))
	  {
	    FOREACH_CHILD_ELEM (grandchildElem, childElem)
	    {
	      if (grandchildElem->ns == ns
		  && ELEM_NAME_EQ (grandchildElem, "diminuendo"))
		{
		  /*
		   * Prepend the diminuendo's end element to the list that we're
		   * keeping.
		   */

		  diminEndXMLID = (gchar *) xmlGetProp (grandchildElem,
							(xmlChar *) "to");
		  if (diminEndXMLID == NULL)
		    {
		      g_warning
			("No \"to\" attribute on <diminuendo> element; "
			 "ignoring");
		    }
		  else
		    {
		      diminEndElem = lookupXMLID (diminEndXMLID);
		      if (diminEndElem == NULL)
			{
			  g_warning ("Chord has diminuendo to nonexistent "
				     "chord with ID \"%s\"", diminEndXMLID);
			}
		      else if (!ELEM_NAME_EQ (diminEndElem, "chord"))
			{
			  g_warning ("Chord has diminuendo to non-chord "
				     "element with tag name \"%s\" and ID "
				     "\"%s\"", diminEndElem->name,
				     diminEndXMLID);
			}
		      else if (diminEndElem == chordElem)
			{
			  g_warning ("Can't diminuendo on one chord");
			}
		      else
			{
			  ((chord *) chordObj->object)->diminuendo_begin_p =
			    TRUE;
			  *diminEndChordElems =
			    g_list_prepend (*diminEndChordElems,
					    diminEndElem);
			}
		      g_free (diminEndXMLID);
		    }
		}
	      else
		{
		  ILLEGAL_ELEM ("diminuendos", grandchildElem);
		}
	    }
	  }
	else if (ELEM_NAME_EQ (childElem, "notes"))
	  {
	    *notesElem = childElem;
	    FOREACH_CHILD_ELEM (grandchildElem, childElem)
	    {
	      if (grandchildElem->ns == ns
		  && ELEM_NAME_EQ (grandchildElem, "note"))
		{
		  parseNote (grandchildElem, ns, si, chordObj, currentClef);
		}
	      else
		{
		  ILLEGAL_ELEM ("notes", grandchildElem);
		}
	    }
	  }
	else if (ELEM_NAME_EQ (childElem, "lyric"))
	  {
	    parseLyric (childElem, chordObj);
	  }
	else if (ELEM_NAME_EQ (childElem, "figure"))
	  {
	    parseFigure (childElem, chordObj);
	    si->has_figures = (gpointer) TRUE;
	  }
	else if (ELEM_NAME_EQ (childElem, "fakechord"))
	  {
	    parseFakechord (childElem, chordObj);
	    si->has_fakechords = (gpointer) TRUE;
	  }

	else
	  {
	    ILLEGAL_ELEM ("chord", childElem);
	  }
      }				/* end if childElem->ns == ns */

    /*
     * No need to handle other namespaces here, the generic DenemoObject
     * code will do it for us.
     */

  }				/* end for each childElem in scoreElem */

  return chordObj;
}

/*
 * Parse the given <lily:directive> element into a DenemoObject.
 */
 static DenemoObject *
parseLilyDir (xmlNodePtr LilyDirectiveElem, xmlNsPtr ns, DenemoScore *si)
{
  gchar *directive = (gchar *) xmlNodeListGetString (LilyDirectiveElem->doc,
                     LilyDirectiveElem->
                     xmlChildrenNode,
                     1);

  return lily_directive_new (directive);
}


/**
 * Parse the given <stem-directive> element into a DenemoObject.
 * @param stemDirectiveElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoScore to populate 
 * 
 * @return the new DenemoObject
 * 
 */
static DenemoObject *
parseStemDirective (xmlNodePtr stemDirectiveElem,
		    xmlNsPtr ns, DenemoScore * si)
{
  gchar *stemDirName =
    (gchar *) xmlGetProp (stemDirectiveElem, (xmlChar *) "type");
  enum stemdirections stemDir = DENEMO_STEMBOTH;

  if (stemDirName == NULL)
    g_warning ("No \"type\" attribute on <stem-directive> element; "
	       "defaulting to auto");
  else if (strcmp (stemDirName, "auto") == 0)
    stemDir = DENEMO_STEMBOTH;
  else if (strcmp (stemDirName, "up") == 0)
    stemDir = DENEMO_STEMUP;
  else if (strcmp (stemDirName, "down") == 0)
    stemDir = DENEMO_STEMDOWN;
  else
    g_warning ("Invalid stem directive type \"%s\"; defaulting to auto",
	       stemDirName);

  return dnm_stem_directive_new (stemDir);
}


/**
 * Parse the given <tuplet-start> into a tuplet open DenemoObject.
 * 
 * @param tupletStartElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoScore to populate 
 * 
 * @return the new DenemoObject
 */
static DenemoObject *
parseTupletStart (xmlNodePtr tupletStartElem, xmlNsPtr ns, DenemoScore * si)
{
  xmlNodePtr multiplierElem = getXMLChild (tupletStartElem, "multiplier", ns);
  gboolean successful = FALSE;
  gint numerator, denominator;

  if (multiplierElem == NULL)
    {
      g_warning ("No <multiplier> element found under <tuplet-start>; "
		 "defaulting to 1/1");
    }
  else
    {
      if (parseFraction (multiplierElem, ns, &numerator, &denominator) == 0)
	{
	  successful = TRUE;
	}
      else
	{
	  g_warning ("Bad multiplier fraction found for <tuplet-start>; "
		     "defaulting to 1/1");
	}
    }

  if (!successful)
    {
      numerator = 1;
      denominator = 1;
    }

  return newtupopen (numerator, denominator);
}





/**
 * Parse the given <grace-start> into a grace-start DenemoObject.
 * @param graceStartElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoScore to populate 
 * 
 * @return the new DenemoObject
 */
static DenemoObject *
parseGraceStart (xmlNodePtr graceStartElem, xmlNsPtr ns, DenemoScore * si)
{
  gchar *onBeatStr =
    (gchar *) xmlGetProp (graceStartElem, (xmlChar *) "on-beat");
  gboolean onBeat = FALSE;
  DenemoObject *result = newgracestart ();

  if (onBeatStr != NULL)
    {
      if (!strcmp (onBeatStr, "true"))
	onBeat = TRUE;
      else if (!strcmp (onBeatStr, "false"))
	onBeat = FALSE;
      else
	g_warning ("Invalid value \"%s\" for \"on-beat\" attribute on "
		   "<grace-start> element; defaulting to \"false\"",
		   onBeatStr);
    }
  ((grace *) result->object)->on_beat = onBeat;

  return result;
}


/**
 * Parse the given <setup-info> element into the given score.
 * @param editInfoElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoScore to populate 
 * 
 * @return 0 on succes, -1 on failure
 */
static gint
parseSetupInfo (xmlNodePtr editInfoElem, xmlNsPtr ns, DenemoGUI * gui)
{
  xmlNodePtr childElem;
  gint cursorposition = 0;
  gchar *tmp;

  FOREACH_CHILD_ELEM (childElem, editInfoElem)
  {
    if (childElem->ns == ns)
      {
	if (ELEM_NAME_EQ (childElem, "lilypondversion"))
	  {
	    tmp = (gchar *) xmlNodeListGetString (childElem->doc,
						  childElem->
						  xmlChildrenNode, 1);
	    if (tmp != NULL)
	      {
		//g_print ("lilypond version %s", tmp);
		g_string_assign (gui->lilycontrol.lilyversion, tmp);
		g_free (tmp);
	      }
	  }
	if (ELEM_NAME_EQ (childElem, "lilypond"))
	  {
	    tmp = (gchar *) xmlNodeListGetString (childElem->doc,
						  childElem->
						  xmlChildrenNode, 1);
	    if (tmp != NULL)
	      {
		//g_print ("lilypond directive for all music %s", tmp);
		g_string_assign (gui->lilycontrol.lilypond, tmp);
		g_free (tmp);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "fontsize"))
	  {
	    gint font = getXMLIntChild (childElem);
	    gui->lilycontrol.fontsize = font;
	    //g_print ("Font Size %d", font);
	  }
	else if (ELEM_NAME_EQ (childElem, "papersize"))
	  {

	    tmp = (gchar *) xmlNodeListGetString (childElem->doc,
						  childElem->
						  xmlChildrenNode, 1);
	    if (tmp != NULL)
	      {
		//g_print ("Paper size %s\n", tmp);
		g_string_assign (gui->lilycontrol.papersize, tmp);
		g_free (tmp);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "orientation"))
	  {

	    gint orientation = getXMLIntChild (childElem);
	    gui->lilycontrol.orientation = orientation;
	    //g_print ("Orientation %d\n", orientation);
	  }
      }
  }

  return 0;
}

/**
 * Parse the given <edit-info> element into the given score.
 * @param editInfoElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoScore to populate 
 * 
 * @return 0 on success, -1 on failure
 */
static gint
parseEditInfo (xmlNodePtr editInfoElem, xmlNsPtr ns, DenemoScore * si,
	       gint * staffnoPtr)
{
  xmlNodePtr childElem;
  gint cursorposition = 0;
  FOREACH_CHILD_ELEM (childElem, editInfoElem)
  {
    if (childElem->ns == ns)
      {
	if (ELEM_NAME_EQ (childElem, "staffno"))
	  {
	    *staffnoPtr = getXMLIntChild (childElem);
	    //g_print ("Staff no %d\t", *staffnoPtr);
	  }
	else if (ELEM_NAME_EQ (childElem, "cursorposition"))
	  {
	    cursorposition = getXMLIntChild (childElem);
	    //g_print ("Cursor Pos %d\n", cursorposition);
	    si->cursor_x = cursorposition;
	  }
      }
  }
  return 0;
}


/**
 * Parse the given <score-info> element into the given score.
 * @param scoreInfoElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoScore to populate 
 * 
 * @return 0 on success, -1 on failure
 */
static gint
parseScoreInfo (xmlNodePtr scoreInfoElem, xmlNsPtr ns, DenemoScore * si)
{
  xmlNodePtr childElem, grandchildElem;
  gint bpm;
  gchar *title, *subtitle, *composer, *poet, *meter, *arranger, *opus;
  gchar *instrument, *dedication, *piece, *head, *copyright, *footer,
    *tagline, *mvmnt_header, *markup_before, *markup_after;

  FOREACH_CHILD_ELEM (childElem, scoreInfoElem)
  {
    if (childElem->ns == ns)
      {
	if (ELEM_NAME_EQ (childElem, "tempo"))
	  {
	    FOREACH_CHILD_ELEM (grandchildElem, childElem)
	    {
	      if (grandchildElem->ns == ns)
		{
		  if (ELEM_NAME_EQ (grandchildElem, "duration"))
		    {
		      /* Do nothing -- it's always going to be 1/4. */
		    }
		  else if (ELEM_NAME_EQ (grandchildElem, "bpm"))
		    {
		      bpm = getXMLIntChild (grandchildElem);
		      if (bpm == G_MAXINT)
			{
			  bpm = 60;
			  g_warning
			    ("Bad value for <bpm>: must be an integer");
			}
		      else
			{
			  si->tempo = bpm;
			}
		    }
		  else
		    {
		      ILLEGAL_ELEM ("tempo", grandchildElem);
		    }
		}
	      else
		{
		  ILLEGAL_ELEM ("tempo", grandchildElem);
		}
	    }
	  }
	else if (ELEM_NAME_EQ (childElem, "title"))
	  {
	    title = (gchar *) xmlNodeListGetString (childElem->doc,
						    childElem->
						    xmlChildrenNode, 1);
	    if (title != NULL)
	      {
		g_string_assign (si->headerinfo.title, title);
		g_free (title);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "subtitle"))
	  {
	    subtitle = (gchar *) xmlNodeListGetString (childElem->doc,
						       childElem->
						       xmlChildrenNode, 1);
	    if (subtitle != NULL)
	      {
		g_string_assign (si->headerinfo.subtitle, subtitle);
		g_free (subtitle);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "composer"))
	  {
	    composer = (gchar *) xmlNodeListGetString (childElem->doc,
						       childElem->
						       xmlChildrenNode, 1);
	    if (composer != NULL)
	      {
		g_string_assign (si->headerinfo.composer, composer);
		g_free (composer);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "poet"))
	  {
	    poet = (gchar *) xmlNodeListGetString (childElem->doc,
						   childElem->
						   xmlChildrenNode, 1);
	    if (poet != NULL)
	      {
		g_string_assign (si->headerinfo.poet, poet);
		g_free (poet);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "meter"))
	  {
	    meter = (gchar *) xmlNodeListGetString (childElem->doc,
						    childElem->
						    xmlChildrenNode, 1);
	    if (meter != NULL)
	      {
		g_string_assign (si->headerinfo.meter, meter);
		g_free (meter);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "opus"))
	  {
	    opus = (gchar *) xmlNodeListGetString (childElem->doc,
						   childElem->
						   xmlChildrenNode, 1);
	    if (opus != NULL)
	      {
		g_string_assign (si->headerinfo.opus, opus);
		g_free (opus);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "arranger"))
	  {
	    arranger = (gchar *) xmlNodeListGetString (childElem->doc,
						       childElem->
						       xmlChildrenNode, 1);
	    if (arranger != NULL)
	      {
		g_string_assign (si->headerinfo.arranger, arranger);
		g_free (arranger);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "instrument"))
	  {
	    instrument = (gchar *) xmlNodeListGetString (childElem->doc,
							 childElem->
							 xmlChildrenNode, 1);
	    if (instrument != NULL)
	      {
		g_string_assign (si->headerinfo.instrument, instrument);
		g_free (instrument);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "dedication"))
	  {
	    dedication = (gchar *) xmlNodeListGetString (childElem->doc,
							 childElem->
							 xmlChildrenNode, 1);
	    if (dedication != NULL)
	      {
		g_string_assign (si->headerinfo.dedication, dedication);
		g_free (dedication);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "piece"))
	  {
	    piece = (gchar *) xmlNodeListGetString (childElem->doc,
						    childElem->
						    xmlChildrenNode, 1);
	    if (piece != NULL)
	      {
		g_string_assign (si->headerinfo.piece, piece);
		g_free (piece);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "head"))
	  {
	    head = (gchar *) xmlNodeListGetString (childElem->doc,
						   childElem->
						   xmlChildrenNode, 1);
	    if (head != NULL)
	      {
		g_string_assign (si->headerinfo.head, head);
		g_free (head);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "copyright"))
	  {
	    copyright = (gchar *) xmlNodeListGetString (childElem->doc,
							childElem->
							xmlChildrenNode, 1);
	    if (copyright != NULL)
	      {
		g_string_assign (si->headerinfo.copyright, copyright);
		g_free (copyright);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "footer"))
	  {
	    footer = (gchar *) xmlNodeListGetString (childElem->doc,
						     childElem->
						     xmlChildrenNode, 1);
	    if (footer != NULL)
	      {
		g_string_assign (si->headerinfo.footer, footer);
		g_free (footer);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "tagline"))
	  {
	    tagline = (gchar *) xmlNodeListGetString (childElem->doc,
						      childElem->
						      xmlChildrenNode, 1);
	    if (tagline != NULL)
	      {
		g_string_assign (si->headerinfo.tagline, tagline);
		g_free (tagline);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "extra"))
	  {
	    mvmnt_header = (gchar *) xmlNodeListGetString (childElem->doc,
						      childElem->
						      xmlChildrenNode, 1);
	    if (mvmnt_header != NULL)
	      {
		g_string_assign (si->headerinfo.extra, mvmnt_header);
		g_free (mvmnt_header);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "markup_before"))
	  {
	    markup_before = (gchar *) xmlNodeListGetString (childElem->doc,
						      childElem->
						      xmlChildrenNode, 1);
	    if (markup_before != NULL)
	      {
		g_string_assign (si->headerinfo.markup_before, markup_before);
		g_free (markup_before);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "markup_after"))
	  {
	    markup_after = (gchar *) xmlNodeListGetString (childElem->doc,
						      childElem->
						      xmlChildrenNode, 1);
	    if (markup_after != NULL)
	      {
		g_string_assign (si->headerinfo.markup_after, markup_after);
		g_free (markup_after);
	      }
	  }




      }
    else
      {
	/* FIXME: Change this so that other namespaces are handled here. */
	ILLEGAL_ELEM ("score-info", childElem);
      }
  }

  return 0;
}


/**
 * Parse the given <staff> element into the staff in the given score.
 * @param staffElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoScore to populate 
 * 
 * @return 0 on success, -1 on failure
 */
static gint
parseStaff (xmlNodePtr staffElem, xmlNsPtr ns, DenemoScore * si)
{
  xmlNodePtr staffInfoElem, childElem;
  DenemoStaff *curStaff = (DenemoStaff *) si->currentprimarystaff->data;

  staffInfoElem = getXMLChild (staffElem, "staff-info", ns);
  RETURN_IF_ELEM_NOT_FOUND ("staff", staffInfoElem, "staff-info");

  FOREACH_CHILD_ELEM (childElem, staffInfoElem)
  {
    if (childElem->ns == ns)
      {
	if (ELEM_NAME_EQ (childElem, "number-of-lines"))
	  {
	    curStaff->no_of_lines = getXMLIntChild (childElem);
	    if (curStaff->no_of_lines == G_MAXINT)
	      {
		g_warning ("Could not determine number of lines in staff; "
			   "defaulting to 5");
		curStaff->no_of_lines = 5;
	      }
	  }
        else if (ELEM_NAME_EQ (childElem, "midi_prognum_override"))
	  {
  	    curStaff->midi_prognum_override = getXMLIntChild (childElem);
  	  }	    
  	else if (ELEM_NAME_EQ (childElem, "midi_prognum"))
	  {
  	    curStaff->midi_prognum = getXMLIntChild (childElem);
  	  }
	else if (ELEM_NAME_EQ (childElem, "midi_channel"))
	  {
  	    curStaff->midi_channel = getXMLIntChild (childElem);
  	  }
	else if (ELEM_NAME_EQ (childElem, "transpose"))
	  {
	    curStaff->transposition = getXMLIntChild (childElem);
	  }
	else if (ELEM_NAME_EQ (childElem, "space_above"))
	  {
	    curStaff->space_above = getXMLIntChild (childElem);
	  }
	else if (ELEM_NAME_EQ (childElem, "space_below"))
	  {
	    curStaff->space_below = getXMLIntChild (childElem);
	  }
	else if (ELEM_NAME_EQ (childElem, "hasfigures"))
	  {
	    curStaff->hasfigures = getXMLIntChild (childElem);
	    if(curStaff->hasfigures)
	      si->has_figures = (gpointer) TRUE;
	  }
	else if (ELEM_NAME_EQ (childElem, "hasfakechords"))
	  {
	    curStaff->hasfakechords = getXMLIntChild (childElem);
	    
	  }
	else if (ELEM_NAME_EQ (childElem, "haslyrics"))
	  {
	    curStaff->haslyrics = getXMLIntChild (childElem);
	    
	  }
	else if (ELEM_NAME_EQ (childElem, "instrument"))
	  {
	    gchar *temp = (gchar *) xmlNodeListGetString
	      (childElem->doc, childElem->xmlChildrenNode, 1);
	    g_string_assign (curStaff->midi_instrument, temp);
	    g_free (temp);
	  }
	else if (ELEM_NAME_EQ (childElem, "context"))
	  {
	    gchar *temp = NULL;
	    temp = (gchar *) xmlNodeListGetString
	      (childElem->doc, childElem->xmlChildrenNode, 1);
	    curStaff->context = lookupContext (temp);
	    g_free (temp);
	  }
	else if (ELEM_NAME_EQ (childElem, "lilybefore"))
	  {
	    gchar *temp = (gchar *) xmlNodeListGetString
	      (childElem->doc, childElem->xmlChildrenNode, 1);
	    if(temp)
	      g_warning("Ignoring the old-style string %s\nAdd this in LilyPond window if required\n", temp);
	    g_free(temp);
	  }
	else if (ELEM_NAME_EQ (childElem, "staff-prolog"))
	  {
	    gchar *temp = 
	      (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
	    curStaff->staff_prolog = (temp?g_string_new(temp):NULL);
	    g_free (temp);
	  }
	else if (ELEM_NAME_EQ (childElem, "lyrics-prolog"))
	  {
	    gchar *temp = 
	      (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
	    curStaff->lyrics_prolog = (temp?g_string_new(temp):NULL);
	    g_free (temp);
	  }
	else if (ELEM_NAME_EQ (childElem, "figures-prolog"))
	  {
	    gchar *temp = 
	      (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
	    curStaff->figures_prolog = (temp?g_string_new(temp):NULL);
	    g_free (temp);
	  }
	else if (ELEM_NAME_EQ (childElem, "fakechords-prolog"))
	  {
	    gchar *temp = 
	      (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
	    curStaff->fakechords_prolog = (temp?g_string_new(temp):NULL);
	    g_free (temp);
	  }
	else
	  {
	    ILLEGAL_ELEM ("staff-info", childElem);
	  }
      }
    else
      {
	/* FIXME: Handle other namespaces here. */
	ILLEGAL_ELEM ("staff-info", childElem);
      }
  }

  return 0;
}


/**
 * Parse the given <voice-info> element into the voice in the given score.
 * @param voiceInfoElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoScore to populate 
 * 
 * @return 0 on success, -1 on failure
 */
static gint
parseVoiceInfo (xmlNodePtr voiceInfoElem, xmlNsPtr ns, DenemoScore * si)
{
  xmlNodePtr childElem;
  gchar *voiceName;

  FOREACH_CHILD_ELEM (childElem, voiceInfoElem)
  {
    if (childElem->ns == ns)
      {
	if (ELEM_NAME_EQ (childElem, "voice-name"))
	  {
	    voiceName = (gchar *) xmlNodeListGetString (childElem->doc,
							childElem->
							xmlChildrenNode, 1);
	    if (voiceName != NULL)
	      {
		DenemoStaff *thestaffstruct =
		  (DenemoStaff *) si->currentstaff->data;
		g_string_assign (thestaffstruct->denemo_name, voiceName);
		g_free (voiceName);
		set_lily_name (thestaffstruct->denemo_name,
			       thestaffstruct->lily_name);
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "first-measure-number"))
	  {
	    /* Do nothing (yet).  All voices start at measure 1. */
	  }
	else
	  {
	    ILLEGAL_ELEM ("voice-info", childElem);
	  }
      }
    else
      {
	/* FIXME: Handle other namespaces here. */
	ILLEGAL_ELEM ("voice-info", childElem);
      }
  }

  return 0;
}


/**
 * Parse the given <initial-voice-params> element into the voice in the given
 * score.
 * @param initVoiceParamsElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoScore to populate 
 * 
 * @return 0 on success, -1 on failure
 */
static gint
parseInitVoiceParams (xmlNodePtr initVoiceParamsElem, xmlNsPtr ns,
		      DenemoScore * si)
{
  DenemoStaff *curVoice = (DenemoStaff *) si->currentstaff->data;
  xmlNodePtr childElem, staffElem;
  gchar *staffXMLID;

  FOREACH_CHILD_ELEM (childElem, initVoiceParamsElem)
  {
    if (childElem->ns == ns)
      {
	if (ELEM_NAME_EQ (childElem, "staff-ref"))
	  {
	    /*
	     * FIXME: This all assumes that the staves and voices appear top
	     *        down in the XML file, which is true when we save it
	     *        but may not be if someone tinkers with the file.
	     */

	    staffXMLID =
	      (gchar *) xmlGetProp (childElem, (xmlChar *) "staff");
	    if (staffXMLID == NULL)
	      {
		g_warning ("No ID found on <staff-ref> element");
	      }
	    else
	      {
		staffElem = lookupXMLID (staffXMLID);
		g_free (staffXMLID);

		if (staffElem == NULL)
		  {
		    g_warning ("Invalid staff ID specified in <staff-ref>");
		  }
		else if (staffElem == sPrevStaffElem)
		  {
		    /* This is a new voice on the previous staff. */

		    curVoice->voicenumber = 2;
		    curVoice->no_of_lines =
		      ((DenemoStaff *) si->currentprimarystaff->data)->
		      no_of_lines;
		  }
		else
		  {
		    /* This is a new staff. */

		    if (!ELEM_NAME_EQ (staffElem, "staff"))
		      {
			g_warning ("<staff-ref> points to a <%s>, not a "
				   "<staff>", staffElem->name);
		      }

		    si->currentprimarystaff = si->currentstaff;
		    if (parseStaff (staffElem, ns, si) != 0)
		      return -1;
		  }

		sPrevStaffElem = staffElem;
	      }
	  }
	else if (ELEM_NAME_EQ (childElem, "clef"))
	  {
	    parseClef (childElem, ns, &(curVoice->sclef));
	  }
	else if (ELEM_NAME_EQ (childElem, "key-signature"))
	  {
	    parseKeySignature (childElem, ns, &(curVoice->skey),
			       &(curVoice->skey_isminor));
	    initkeyaccs (curVoice->skeyaccs, curVoice->skey);
	    //dnm_setinitialkeysig(curVoice, curVoice->skey, curVoice->skey_isminor);
	  }
	else if (ELEM_NAME_EQ (childElem, "time-signature"))
	  {
	    parseTimeSignature (childElem, ns, &(curVoice->stime1),
				&(curVoice->stime2));
	  }
	else
	  {
	    ILLEGAL_ELEM ("initial-voice-params", childElem);
	  }
      }
    else
      {
	/* FIXME: Handle other namespaces here. */
	ILLEGAL_ELEM ("initial-voice-params", childElem);
      }
  }

  return 0;
}


/**
 * Parse the given <measures> element into the voice in the given score.
 * @param measuresElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoScore to populate 
 *
 * @return 0 on success, -1 on failure
 */
static gint
parseMeasures (xmlNodePtr measuresElem, xmlNsPtr ns, DenemoScore * si)
{
  xmlNodePtr childElem, objElem, notesElem = NULL;
  DenemoObject *curObj, *prevChord = NULL;
  gboolean startedBeam = FALSE;
  gint currentClef = ((DenemoStaff *) si->currentstaff->data)->sclef;
  enum clefs newClef;
  gint keySig;
  gboolean isMinor;
  gint numerator, denominator;
  GList *slurEndChordElems = NULL;
  GList *crescEndChordElems = NULL;
  GList *diminEndChordElems = NULL;

  FOREACH_CHILD_ELEM (childElem, measuresElem)
  {
    if (ELEM_NAME_EQ (childElem, "measure") && (childElem->ns == ns))
      {
	if (si->currentmeasure == NULL)
	  {

	    si->currentmeasure =
	      dnm_addmeasures (si, si->currentmeasurenum - 1, 1, FALSE);
#ifdef DEBUG
	    g_print ("ImportXML Adding Measure \n currentmeasurenum %d",
		     si->currentmeasurenum);
#endif
	  }

	FOREACH_CHILD_ELEM (objElem, childElem)
	{
	  curObj = NULL;
	  notesElem = NULL;

	  if (objElem->ns == ns)
	    {
	      if (ELEM_NAME_EQ (objElem, "barline"))
		{
		  /* FIXME */
		  g_warning ("Cannot yet handle <barline> elements");
		}
	      else if (ELEM_NAME_EQ (objElem, "beam-end"))
		{
		  /* Ignore beam attribute for now. */
		  if (prevChord == NULL)
		    {
		      g_warning ("Received <beam-end> without chord "
				 "before it");
		    }
		  else
		    {
		      prevChord->isstart_beamgroup = FALSE;
		      prevChord->isend_beamgroup = TRUE;
		    }
		}
	      else if (ELEM_NAME_EQ (objElem, "beam-start"))
		{
		  /* Ignore ID for now. */
		  startedBeam = TRUE;
		}
	      else if (ELEM_NAME_EQ (objElem, "chord"))
		{
		  curObj = parseChord (objElem, ns, si, currentClef,
				       &slurEndChordElems,
				       &crescEndChordElems,
				       &diminEndChordElems, &notesElem);
		  /* old format files will not have haslyrics... fields of staff explicit
		   so for backwards compatibility we reconstruct it here*/
		  if(((chord *) curObj->object)->lyric)
		    ((DenemoStaff *) si->currentstaff->data)->haslyrics = TRUE;
		  if(((chord *) curObj->object)->figure)
		    ((DenemoStaff *) si->currentstaff->data)->hasfigures = TRUE;
		  if(((chord *) curObj->object)->fakechord)
		    ((DenemoStaff *) si->currentstaff->data)->hasfakechords = TRUE;

		  if (startedBeam)
		    {
		      curObj->isstart_beamgroup = TRUE;
		      curObj->isend_beamgroup = FALSE;
		      startedBeam = FALSE;
		    }
		  prevChord = curObj;
		}
	      else if (ELEM_NAME_EQ (objElem, "clef"))
		{
		  parseClef (objElem, ns, (gint *) & newClef);
		  curObj = dnm_newclefobj (newClef);
		  currentClef = newClef;
		}
	      else if (ELEM_NAME_EQ (objElem, "lyric"))
		{
		  /*gchar *text = (gchar *)xmlNodeListGetString
		     (objElem->doc, 
		     objElem->xmlChildrenNode,
		     1); */
		  /*curObj = lyric_new(text,0,); */

		}
	      else if (ELEM_NAME_EQ (objElem, "lily-directive"))
		{
		  curObj = parseLilyDir (objElem, ns, si);		  
		}
	      
	      /* else if (ELEM_NAME_EQ (objElem, "dynamic"))
	         {
	         curObj = parseDynamic (objElem, ns, si);
	         } */
	      else if (ELEM_NAME_EQ (objElem, "grace-end"))
		{
		  curObj = newgraceend ();
		}
	      else if (ELEM_NAME_EQ (objElem, "grace-start"))
		{
		  curObj = parseGraceStart (objElem, ns, si);
		}
	      else if (ELEM_NAME_EQ (objElem, "key-signature"))
		{
#if 1
		  parseKeySignature (objElem, ns, &keySig, &isMinor);
		  curObj = dnm_newkeyobj (keySig, isMinor, 0);
		  initkeyaccs (((keysig *) curObj->object)->accs, keySig);
		  //dnm_setinitialkeysig(((keysig *) curObj->object), keySig, isMinor);
#else
		  //warningdialog("Dodgy code");
		  parseKeySignature (objElem, ns, &keySig, &isMinor);
		  //curObj = dnm_newkeyobj (keySig, isMinor, 0);
		  //initkeyaccs (((keysig *) curObj->object)->accs, keySig);
		  dnm_setinitialkeysig((DenemoStaff *) si->currentstaff->data, keySig, isMinor);
#endif
		}
	      else if (ELEM_NAME_EQ (objElem, "measure-break"))
		{
		  /* FIXME */
		  g_warning ("Cannot yet handle <measure-break> elements");
		}
	      else if (ELEM_NAME_EQ (objElem, "rest"))
		{
		  curObj = parseRest (objElem, ns, si);
		  if (curObj)
		    {
		      if (startedBeam)
			{
			  curObj->isstart_beamgroup = TRUE;
			  curObj->isend_beamgroup = FALSE;
			  startedBeam = FALSE;
			}
		      prevChord = curObj;
		    }
		}
	      else if (ELEM_NAME_EQ (objElem, "stem-directive"))
		{
		  curObj = parseStemDirective (objElem, ns, si);
		}
	      else if (ELEM_NAME_EQ (objElem, "time-signature"))
		{
		  parseTimeSignature (objElem, ns, &numerator, &denominator);
		  curObj = dnm_newtimesigobj (numerator, denominator);
		}
	      else if (ELEM_NAME_EQ (objElem, "tuplet-end"))
		{
		  curObj = newtupclose ();
		}
	      else if (ELEM_NAME_EQ (objElem, "tuplet-start"))
		{
		  curObj = parseTupletStart (objElem, ns, si);
		}
	      else
		{
		  ILLEGAL_ELEM ("measure", objElem);
		}

	      if (curObj != NULL)
		{
		  /*
		   * FIXME: Appending is very inefficient!  But we can't
		   *        prepend then reverse when we're done because
		   *        the import handler callbacks expect the data
		   *        structures to be in the right order.  Instead,
		   *        we should manually keep track of the end of
		   *        the list (si->currentobject?) and append to
		   *        that.
		   */
		  si->currentmeasure->data =
		    g_list_append ((objnode *) si->currentmeasure->data,
				   curObj);
		  //g_print("Element type lily %d\n", curObj->type==LILYDIRECTIVE);

		  /*
		   * FIXME: Loop through notesElem to search for
		   * subelements of <note> that are in other namespaces.
		   */
		}
	    }			/* end if objElem is in the right namespace */
	  else
	    {
	      /* FIXME: Handle other namespaces here. */
	      ILLEGAL_ELEM ("measure", objElem);
	    }
	}			/* end foreach objElem in childElem */
	si->currentmeasurenum++;
	si->currentmeasure = si->currentmeasure->next;
      }				/* end if childElem is a <measure> */
    else
      {
	ILLEGAL_ELEM ("measures", childElem);
      }
  }

  if (slurEndChordElems != NULL)
    {
      g_warning ("Some unterminated slurs were left at the end of the voice");
      g_list_free (slurEndChordElems);
    }

  if (crescEndChordElems != NULL)
    {
      g_warning
	("Some unterminated crescendos were left at the end of the voice");
      g_list_free (crescEndChordElems);
    }

  if (diminEndChordElems != NULL)
    {
      g_warning
	("Some unterminated diminuendos were left at the end of the voice");
      g_list_free (diminEndChordElems);
    }

  if (crescEndChordElems != NULL)
    {
      g_warning
	("Some unterminated crescendos were left at the end of the voice");
      g_list_free (crescEndChordElems);
    }

  if (diminEndChordElems != NULL)
    {
      g_warning
	("Some unterminated diminuendos were left at the end of the voice");
      g_list_free (diminEndChordElems);
    }

  return 0;
}



/**
 * Parse the given <voice> element into a voice in the given score.
 * @param voiceElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoScore to populate 
 * 
 * @return 0 on success, -1 on failure
 */
static gint
parseVoice (xmlNodePtr voiceElem, xmlNsPtr ns, DenemoGUI * gui)
{
  DenemoScore *si = gui->si;
  xmlNodePtr childElem;
  /*  gchar *id; */

  /* Create the staff structure. */

  si->currentstaffnum++;
  newstaff (gui, ADDFROMLOAD, DENEMO_NONE);
  si->currentstaff = g_list_last (si->thescore);
  si->currentmeasurenum = 1;

  /* Parse the child elements. */

  childElem = getXMLChild (voiceElem, "voice-info", ns);
  RETURN_IF_ELEM_NOT_FOUND ("voice", childElem, "voice-info");
  if (parseVoiceInfo (childElem, ns, si) != 0)
    return -1;

  childElem = getXMLChild (voiceElem, "initial-voice-params", ns);
  RETURN_IF_ELEM_NOT_FOUND ("voice", childElem, "initial-voice-params");
  if (parseInitVoiceParams (childElem, ns, si) != 0)
    return -1;

  childElem = getXMLChild (voiceElem, "measures", ns);
  RETURN_IF_ELEM_NOT_FOUND ("voice", childElem, "measures");
  if (parseMeasures (childElem, ns, si) != 0)
    return -1;

  /* FIXME: Handle elements in other namespaces. */

  return 0;
}


/**
 * Parse the given <score> element into the given score.
 * 
 * @param scoreElem the score element to process
 * @param ns Denemo's xml namespaces
 * @param si the DenemoScore structure to populate
 * @param staffnoPtr a pointer to contain the staff number
 * 
 * @return 0 on success ,-1 on failure
 */
static gint
parseScore (xmlNodePtr scoreElem, xmlNsPtr ns, DenemoGUI * gui,
	    gint * staffnoPtr, ImportType type)
{
  DenemoScore *si = gui->si;
  xmlNodePtr childElem, voiceElem;


  childElem = getXMLChild (scoreElem, "edit-info", ns);

  if (childElem != 0)
    parseEditInfo (childElem, ns, si, staffnoPtr);
  //if (parseEditInfo (childElem, ns, si, staffnoPtr) != 0)
  //  return -1;

  childElem = getXMLChild (scoreElem, "score-info", ns);
  RETURN_IF_ELEM_NOT_FOUND ("score", childElem, "score-info");
  if(type==REPLACE_SCORE)
    if ( parseScoreInfo (childElem, ns, si) != 0)
      return -1;

  /*
   * Note: We don't currently care about <staves>, but we will need to
   *       eventually, since we should parse out all the elements in other
   *       namespaces that are children of <staff-info>.
   */

  childElem = getXMLChild (scoreElem, "voices", ns);
  RETURN_IF_ELEM_NOT_FOUND ("score", childElem, "voices");
  FOREACH_CHILD_ELEM (voiceElem, childElem)
  {
    if (ELEM_NAME_EQ (voiceElem, "voice") && (voiceElem->ns == ns))
      {
	if (parseVoice (voiceElem, ns, gui) != 0)
	  return -1;
      }
    else
      {
	ILLEGAL_ELEM ("voices", voiceElem);
      }
  }

  FOREACH_CHILD_ELEM (childElem, scoreElem)
  {
    if (childElem->ns != ns)
      {
	/* FIXME: Handle other namespaces here! */
	ILLEGAL_ELEM ("score", childElem);
      }
  }

  return 0;
}


/* parse the movement (ie DenemoScore) from childElem */
static gint   parseMovement(xmlNodePtr childElem, xmlNsPtr ns, DenemoGUI *gui, ImportType type) {
  int ret= 0, staffno = 0;

   DenemoScore *si = gui->si;
   if(type!=ADD_STAFFS)
     gui->movements = g_list_append(gui->movements, gui->si);
   si->currentstaffnum = 0;
   sPrevStaffElem = NULL;
   ret = parseScore (childElem, ns, gui, &staffno, type);
   sPrevStaffElem = NULL;
   gui->si->currentstaffnum = staffno;
   updatescoreinfo (gui);
   set_rightmeasurenum (gui->si);
   set_bottom_staff (gui);
   set_width_to_work_with(gui);
   return ret;
}






/**
 * Import the given (possibly zlib-compressed) Denemo "native" XML file into
 * the given score.
 * 
 * @param filename the file to importxml
 * @param  gui DenemoGUI to hold the score
 * @return 0 on success, -1 on failure
 */
gint
importXML (gchar * filename, DenemoGUI *gui, ImportType type)
{
  gint ret = 0;
  xmlDocPtr doc = NULL;
  xmlNsPtr ns;
  /*  xmlNodePtr rootElem, childElem; */
  xmlNodePtr rootElem;
  gchar *version = NULL;

  /* Try to parse the file. */

  doc = xmlParseFile (filename);
  if (doc == NULL)
    {
      g_warning ("Could not read XML file %s", filename);
      ret = -1;
      return ret;
    }

  /*
   * Do a couple of sanity checks to make sure we've actually got a Denemo
   * format XML file.
   */

  rootElem = xmlDocGetRootElement (doc);
  ns = rootElem->ns;
  if (strcmp ((gchar *) ns->href, DENEMO_XML_NAMESPACE) != 0)
    {
      g_warning ("Root element is not in Denemo namespace");
      ret = -1;
      goto cleanup;
    }
  if (strcmp ((gchar *) rootElem->name, "score") != 0)
    {
      g_warning ("Root element is not <score>");
      ret = -1;
      goto cleanup;
    }
  version = (gchar *) xmlGetProp (rootElem, (xmlChar *) "version");
  if (version == NULL)
    {
      g_warning ("No version found on root element");
      ret = -1;
      goto cleanup;
    }
  gint version_number = 2;
  if (strcmp (version, "2.0") != 0)
    {
      if(strcmp (version, "1.0") != 0) {
      g_warning ("Denemo XML file version %s found, but we can only handle "
		 "versions 1.0 and 2.0", version);
      ret = -1;
      goto cleanup;
      } else
	version_number = 1;
    }

  /*
   * Okay, we've got a bona fide, 100% genuine Denemo XML file (hopefully).
   * So let's parse it.  The first thing to do is to construct a map from IDs
   * (or rather, attributes with name "id") to their respective elements.
   */

  buildXMLIDToElemMap (doc);
  /* Then, parse the score. */
  if(version_number==2) {
    xmlNodePtr childElem;
    switch(type) {
    case ADD_STAFFS:
      FOREACH_CHILD_ELEM(childElem, rootElem){
	if (ELEM_NAME_EQ (childElem, "lilycontrol") ||ELEM_NAME_EQ (childElem, "custom_scoreblock") ){
	  continue;
	} else
	ret |=  parseMovement(childElem, ns, gui, type);
	//g_print("parsed more staffs breaking now\n");
	break;//Note: we only adds staffs from first movement
      }
      break;
    case ADD_MOVEMENTS:
      FOREACH_CHILD_ELEM(childElem, rootElem){
	if (ELEM_NAME_EQ (childElem, "lilycontrol") ||ELEM_NAME_EQ (childElem, "custom_scoreblock")){
	  continue;/* do not change the header when adding movements parseScoreInfo(childElem, ns, gui);*/
	} else {
	  new_empty_score(gui);
	  ret |=  parseMovement(childElem, ns, gui, type);
	  //g_print("parsed movement\n");
	}
      }
      break;
    case REPLACE_SCORE:
      free_gui(gui);
      
      FOREACH_CHILD_ELEM(childElem, rootElem){
	if (ELEM_NAME_EQ (childElem, "lilycontrol")){
	  parseSetupInfo(childElem, ns, gui);
	} else
	if (ELEM_NAME_EQ (childElem, "custom_scoreblock")){
	   {
	    gchar *tmp = (gchar *) xmlNodeListGetString (childElem->doc,
						  childElem->
						  xmlChildrenNode, 1);
	    if (tmp != NULL)
	      {
		gui->custom_scoreblocks = g_list_append(gui->custom_scoreblocks, g_string_new(tmp));
		g_free (tmp);
	      }
	  }
	}

	else {
	  new_empty_score(gui);
	  ret |=  parseMovement(childElem, ns, gui, type);
	}
      }
      break;
    default:
      warningdialog("Erroneous call");
      goto cleanup;
    } 
  } else {//version 1
    switch(type) {
    case ADD_STAFFS:
      ret +=  parseMovement(rootElem, ns, gui, type);
      break;
    case ADD_MOVEMENTS:
      new_empty_score(gui);
      ret =  parseMovement(rootElem, ns, gui, type);
      break;
    case REPLACE_SCORE:
      free_score (gui);
      if(gui->movements)
	g_list_free(gui->movements);/*FIXME free all the other si */
      gui->movements = NULL;
      init_score(gui->si, gui);
      gui->si->currentstaffnum = 0;
      ret =  parseMovement(rootElem, ns, gui, type);
      break;
    default:
      warningdialog("Erroneous call");
      goto cleanup;
    }
  }
  //gui->si = gui->movements->data;
 cleanup:
  
  if (version != NULL)
    g_free (version);
  if (doc != NULL)
    xmlFreeDoc (doc);
  g_hash_table_foreach (sXMLIDToElemMap, freeHashTableKey, NULL);
  g_hash_table_destroy (sXMLIDToElemMap);
  sXMLIDToElemMap = NULL;


  //g_print("Number of movements %d\n", g_list_length(gui->movements));
  return ret;
}


