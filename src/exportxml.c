/* exportxml.cpp
 * Functions for exporting what Denemo's working on to an XML file (adapted
 * somewhat from exportabc.c)
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2001, 2002 Eric Galluzzo
 */

#include "config.h"
#include <denemo/denemo.h>
#include "exportxml.h"
#include "utils.h"

#include <stdlib.h>
#include <string.h>

/* libxml includes: for libxml2 this should be <libxml/tree.h> */
#include <libxml/tree.h>


#define XML_COMPRESSION_RATIO 3


/* The list of export handlers */
/*static GList *sExportHandlers = NULL;*/

/**
 * The map from staves, voices, chords, etc. to XML IDs
 *
 * FIXME: This won't work for multi-threaded applications!  I should really
 *        encapsulate these in a nice DenemoXMLContext struct or some such
 *        thing (or does libxml already provide generating unique IDs?) that I
 *        pass around.
 */
static gint sNextXMLID = 0;
static GHashTable *sStructToXMLIDMap = NULL;


/**
 * Free the value (a string) of the given key/value pair.  For use with
 * g_hash_table_foreach.
 */
static void
freeHashTableValue (gpointer key, gpointer value, gpointer userData)
{
  g_free (value);
}

/**
 * Determine the key signature as it should
 * be written to the XML file
 */
static void
determineKeySignature (gint number, gboolean isMinor, gchar ** baseNote,
		       gchar ** baseAcc, gchar ** mode)
{
  if (isMinor)
    {
      number += 3;
      *mode = "minor";
    }
  else
    {
      *mode = "major";
    }

  switch (number)
    {
    case -7:
      *baseNote = "C";
      *baseAcc = "flat";
      break;
    case -6:
      *baseNote = "G";
      *baseAcc = "flat";
      break;
    case -5:
      *baseNote = "D";
      *baseAcc = "flat";
      break;
    case -4:
      *baseNote = "A";
      *baseAcc = "flat";
      break;
    case -3:
      *baseNote = "E";
      *baseAcc = "flat";
      break;
    case -2:
      *baseNote = "B";
      *baseAcc = "flat";
      break;
    case -1:
      *baseNote = "F";
      *baseAcc = NULL;
      break;
    case 0:
      *baseNote = "C";
      *baseAcc = NULL;
      break;
    case 1:
      *baseNote = "G";
      *baseAcc = NULL;
      break;
    case 2:
      *baseNote = "D";
      *baseAcc = NULL;
      break;
    case 3:
      *baseNote = "A";
      *baseAcc = NULL;
      break;
    case 4:
      *baseNote = "E";
      *baseAcc = NULL;
      break;
    case 5:
      *baseNote = "B";
      *baseAcc = NULL;
      break;
    case 6:
      *baseNote = "F";
      *baseAcc = "sharp";
      break;
    case 7:
      *baseNote = "C";
      *baseAcc = "sharp";
      break;
    case 8:
      *baseNote = "G";
      *baseAcc = "sharp";
      break;
    case 9:
      *baseNote = "D";
      *baseAcc = "sharp";
      break;
    case 10:
      *baseNote = "A";
      *baseAcc = "sharp";
      break;
    default:
      if (isMinor)
	number -= 3;
      g_warning ("Unknown key signature with %d %s, using C major",
		 abs (number), number < 0 ? "flats" : "sharps");
      *baseNote = "C";
      *baseAcc = NULL;
      *mode = "major";
      break;
    }
}

/**
 * Determine the clef as it should
 * be written to the XML file
 */
static void
determineClef (gint type, gchar ** clefName)
{
  switch (type)
    {
    case DENEMO_TREBLE_CLEF:
      *clefName = "treble";
      break;
    case DENEMO_BASS_CLEF:
      *clefName = "bass";
      break;
    case DENEMO_ALTO_CLEF:
      *clefName = "alto";
      break;
    case DENEMO_G_8_CLEF:
      *clefName = "treble-8vb";
      break;
    case DENEMO_TENOR_CLEF:
      *clefName = "tenor";
      break;
    case DENEMO_SOPRANO_CLEF:
      *clefName = "soprano";
      break;
    default:
      g_warning ("Unknown clef type %d, using treble", type);
      *clefName = "treble";
      break;
    }
}



/**
 * Determine the duration as it should be written to the XML file.  duration
 * is the Denemo duration, and the XML duration is returned in durationName.
 */
static void
determineDuration (gint duration, gchar ** durationName)
{
  switch (duration)
    {
    case 0:
      *durationName = "whole";
      break;
    case 1:
      *durationName = "half";
      break;
    case 2:
      *durationName = "quarter";
      break;
    case 3:
      *durationName = "eighth";
      break;
    case 4:
      *durationName = "sixteenth";
      break;
    case 5:
      *durationName = "thirty-second";
      break;
    case 6:
      *durationName = "sixty-fourth";
      break;
    default:
      g_warning ("Unknown note duration 1/%d, using quarter", 1 << duration);
      *durationName = "quarter";
      break;
    }
}


/**
 * return a child node of parent, holding the passed name and integer.
 */
static xmlNodePtr
newXMLIntChild (xmlNodePtr parent, xmlNsPtr ns, const xmlChar * name,
		gint content)
{
  gchar *integer = g_strdup_printf("%d", content);
  xmlNodePtr child = xmlNewChild (parent, ns, name, (xmlChar *) integer);
  g_free(integer);
  return child;
}

/**
 * 
Set a prop of the parent, holding the passed name and integer.
 */
static void
newXMLIntProp (xmlNodePtr parent,  const xmlChar * name,
		gint content)
{
  gchar *integer = g_strdup_printf("%d", content);
  xmlSetProp  (parent, name, (xmlChar *) integer);
  g_free(integer);
}




/**
 * Return a newly allocated unique XML ID string which must be freed by the
 * caller when it is no longer needed.
 */
static gchar *
newXMLID ()
{
  /* Allocate enough space for "id2000000000" + '\0'. */

  gchar *result = g_new (char, 13);
  sprintf (result, "id%d", sNextXMLID++);
  return result;
}


/**
 * Return a unique XML ID for the given pointer, and register it in the global
 * structure -> XML ID map.  If it already exists in the map, return its XML
 * ID; otherwise, create a new XML ID and return that.
 */
static gchar *
getXMLID (gpointer ptr)
{
  gchar *xmlID = (gchar *) g_hash_table_lookup (sStructToXMLIDMap, ptr);
  if (xmlID == NULL)
    g_hash_table_insert (sStructToXMLIDMap, ptr, (xmlID = newXMLID ()));
  return xmlID;
}


/**
 * Output a fraction (<numerator>, <denominator>) as a child of the given
 * node.
 */
static void
newXMLFraction (xmlNodePtr parent, xmlNsPtr ns, gint num, gint denom)
{
  newXMLIntChild (parent, ns, (xmlChar *) "numerator", num);
  newXMLIntChild (parent, ns, (xmlChar *) "denominator", denom);
}


/**
 * Output a clef of the form:
 *
 *   <clef name="treble"/>
 *
 * as a child of the given node.
 */
static xmlNodePtr
newXMLClef (xmlNodePtr parent, xmlNsPtr ns, gint clef)
{
  gchar *clefName = NULL;
  xmlNodePtr clefElem = NULL;

  determineClef (clef, &clefName);
  clefElem = xmlNewChild (parent, ns, (xmlChar *) "clef", NULL);
  xmlSetProp (clefElem, (xmlChar *) "name", (xmlChar *) clefName);
  return clefElem;
}


/**
 * Output a key signature of the form:
 *
 *   <key-signature>
 *     <modal-key-signature note-name="B" accidental="flat" mode="major"/>
 *   </key-signature>
 *
 * as a child of the given node.
 */
static xmlNodePtr
newXMLKeySignature (xmlNodePtr parent, xmlNsPtr ns, gint keySig,
		    gboolean isMinor)
{
  gchar *noteName = NULL, *accidental = NULL, *mode = NULL;
  xmlNodePtr keySigElem = NULL, modalKeySigElem = NULL;

  determineKeySignature (keySig, isMinor, &noteName, &accidental, &mode);
  keySigElem = xmlNewChild (parent, ns, (xmlChar *) "key-signature", NULL);
  modalKeySigElem =
    xmlNewChild (keySigElem, ns, (xmlChar *) "modal-key-signature", NULL);
  xmlSetProp (modalKeySigElem, (xmlChar *) "note-name", (xmlChar *) noteName);
  if (accidental != NULL)
    xmlSetProp (modalKeySigElem, (xmlChar *) "accidental",
		(xmlChar *) accidental);
  xmlSetProp (modalKeySigElem, (xmlChar *) "mode", (xmlChar *) mode);

  return keySigElem;
}


/**
 * Output a time signature of the form:
 *
 *   <time-signature>
 *     <simple-time-signature>
 *       <numerator>3</numerator>
 *       <denominator>4</denominator>
 *     </simple-time-signature>
 *   </time-signature>
 *
 * as a child of the given node.
 */
static xmlNodePtr
newXMLTimeSignature (xmlNodePtr parent, xmlNsPtr ns, gint numerator,
		     gint denominator)
{
  xmlNodePtr timeSigElem =
    xmlNewChild (parent, ns, (xmlChar *) "time-signature", NULL);
  newXMLFraction (xmlNewChild
		  (timeSigElem, ns, (xmlChar *) "simple-time-signature",
		   NULL), ns, numerator, denominator);
  return timeSigElem;
}


/**
 * Output a decoration of the form:
 *
 *   <decoration type="staccato"/>
 *
 * as a child of the given node.
 */
static xmlNodePtr
newXMLDecoration (xmlNodePtr parent, xmlNsPtr ns, gchar * decorationName)
{
  xmlNodePtr decorationElem =
    xmlNewChild (parent, ns, (xmlChar *) "decoration", NULL);
  xmlSetProp (decorationElem, (xmlChar *) "type", (xmlChar *) decorationName);
  return decorationElem;
}


/**
 * Output an accidental of the form:
 *
 *   <accidental name="double-sharp" show="true"/>
 *
 * as a child of the given node.
 */
static xmlNodePtr
newXMLAccidental (xmlNodePtr parent, xmlNsPtr ns, gint enshift, gboolean show)
{
  xmlNodePtr accElem =
    xmlNewChild (parent, ns, (xmlChar *) "accidental", NULL);
  gchar *accName = NULL;
  switch (enshift)
    {
    case -2:
      accName = "double-flat";
      break;
    case -1:
      accName = "flat";
      break;
    case 0:
      accName = "natural";
      break;
    case 1:
      accName = "sharp";
      break;
    case 2:
      accName = "double-sharp";
      break;
    default:
      g_warning ("Illegal accidental shift %d: must be between -2 and 2; "
		 "using 0", enshift);
      accName = "natural";
      break;
    }
  xmlSetProp (accElem, (xmlChar *) "name", (xmlChar *) accName);
  xmlSetProp (accElem, (xmlChar *) "show",
	      (show ? (xmlChar *) "true" : (xmlChar *) "false"));
  return accElem;
}


/**
 * Output a stem directive of the form:
 *
 *   <stem-directive type="down"/>
 *
 * as a child of the given node.
 */
static xmlNodePtr
newXMLStemDirective (xmlNodePtr parent, xmlNsPtr ns, enum stemdirections type)
{
  xmlNodePtr stemElem =
    xmlNewChild (parent, ns, (xmlChar *) "stem-directive", NULL);
  gchar *stemName = NULL;
  switch (type)
    {
    case DENEMO_STEMDOWN:
      stemName = "down";
      break;
    case DENEMO_STEMBOTH:
      stemName = "auto";
      break;
    case DENEMO_STEMUP:
      stemName = "up";
      break;
    default:
      g_warning ("Unknown stem directive type %d, using auto", type);
      stemName = "auto";
      break;
    }
  xmlSetProp (stemElem, (xmlChar *) "type", (xmlChar *) stemName);
  return stemElem;
}


/**
 * Output a notehead element of the form:
 *
 *   <note-head type="diamond"/>
 *
 * as a child of the given node.
 */
static xmlNodePtr
newXMLNoteHead (xmlNodePtr parent, xmlNsPtr ns, enum headtype noteHeadType)
{
  xmlNodePtr noteHeadElem = xmlNewChild (parent, ns, (xmlChar *) "note-head",
					 NULL);
  gchar *headTypeName = NULL;
  switch (noteHeadType)
    {
    case DENEMO_NORMAL_NOTEHEAD:
      headTypeName = "normal";
      break;
    case DENEMO_CROSS_NOTEHEAD:
      headTypeName = "cross";
      break;
    case DENEMO_HARMONIC_NOTEHEAD:
      headTypeName = "harmonic";
      break;
    case DENEMO_DIAMOND_NOTEHEAD:
      headTypeName = "diamond";
      break;
    default:
      g_warning ("Unknown notehead type %d, using normal", noteHeadType);
      headTypeName = "normal";
      break;
    }
  xmlSetProp (noteHeadElem, (xmlChar *) "type", (xmlChar *) headTypeName);
  return noteHeadElem;
}

static void
newDirectivesElem(xmlNodePtr objElem, xmlNsPtr ns, GList *g, gchar *type) { 
  xmlNodePtr directivesElem =  xmlNewChild (objElem, ns, (xmlChar *) type, NULL);
  //GList *g = ((chord *) curObj->object)->directives;
  for(;g;g=g->next) {
    DenemoDirective *directive = (DenemoDirective *)g->data;
     xmlNodePtr directiveElem =  xmlNewChild (directivesElem, ns, (xmlChar *) "directive", NULL);
#define DO_DIREC(field)  if (directive->field \
                   && directive->field->len)\
                      xmlNewChild (directiveElem, ns, (xmlChar *) #field,\
				     (xmlChar *) directive->field->str);
#define DO_INTDIREC(field)   newXMLIntChild (directiveElem, ns, (xmlChar *) #field,\
				             directive->field);
    DO_DIREC(tag);
    DO_DIREC(prefix);
    DO_DIREC(postfix);
    DO_DIREC(display);
    DO_DIREC(graphic_name);
    
    DO_INTDIREC(minpixels);
    DO_INTDIREC(x);
    DO_INTDIREC(y);
    DO_INTDIREC(tx);
    DO_INTDIREC(ty);
    DO_INTDIREC(gx);
    DO_INTDIREC(gy);
    DO_INTDIREC(override);

#undef DO_DIREC
#undef DO_INTDIREC
  }
}


/**
 * Export the given score (from measure start to measure end) as a "native"
 * Denemo XML file to the given file.
 */
void
exportXML (gchar * thefilename, DenemoGUI *gui, gint start, gint end)
{
  GString *filename = g_string_new (thefilename);
  xmlDocPtr doc;
  xmlNodePtr scoreElem, mvmntElem, stavesElem, voicesElem, voiceElem;
  xmlNodePtr measuresElem, measureElem, objElem, prevTieElem, directivesElem, directiveElem;
  xmlNodePtr curElem, parentElem;
  xmlNsPtr ns;
  staffnode *curStaff;
  DenemoStaff *curStaffStruct;
  gchar *staffXMLID = 0, *voiceXMLID;
  gchar *lastBeamStartXMLID, *chordXMLID, *noteXMLID;
  gchar *lastTupletStartXMLID, *lastGraceStartXMLID;
  //gchar *clefname, *baseKeyName, *accidental;
  measurenode *curMeasure;
  gboolean emptyMeasure;
  objnode *curObjNode;
  DenemoObject *curObj;
  GList *curNoteNode;
  note *curNote;
  GList *slurElemStack;
  GList *crescElemStack;
  GList *diminElemStack;
  gint curTime1, curTime2;
  //gint numerator, denominator;
  gchar *durationType;
  gdouble fraction = 0.0;

  /* Append .denemo onto the filename if necessary. */

  //if (strcmp (filename->str + filename->len - 7, ".denemo"))
  //  g_string_append (filename, ".denemo");

  /* Initialize score-wide variables. */
  
  sStructToXMLIDMap = g_hash_table_new (NULL, NULL);
  
  /* Create the XML document and output the root element. */
  
  doc = xmlNewDoc ((xmlChar *) "1.0");
  xmlSetDocCompressMode (doc, XML_COMPRESSION_RATIO);
  doc->xmlRootNode = scoreElem =
    xmlNewDocNode (doc, NULL, (xmlChar *) "score", NULL);
  ns = xmlNewNs (doc->xmlRootNode, (xmlChar *) DENEMO_XML_NAMESPACE, NULL);
  xmlSetProp (scoreElem, (xmlChar *) "version", (xmlChar *) "2.0");
  /* FIXME: Put comment here ("Denemo XML file generated by..."). */
  
  if(getNumCharsSchemeText()) {
    gchar *text = (gchar*) getSchemeText();
    xmlNewChild (scoreElem, ns, "scheme", (xmlChar *)text);
    g_free(text);
  }
  /* lilycontrol for the whole musical score */
  
  
  parentElem = xmlNewChild (scoreElem, ns, (xmlChar *) "lilycontrol", NULL);
#define NEWCHILD(field) if(gui->lilycontrol.field->len) \
                       xmlNewChild (parentElem, ns, (xmlChar *) #field,\
                      (xmlChar *) gui->lilycontrol.field->str)
  NEWCHILD(papersize);
  NEWCHILD(lilyversion);

  newXMLIntChild (parentElem, ns, (xmlChar *) "fontsize",  atoi(gui->lilycontrol.staffsize->str));
  newXMLIntChild (parentElem, ns, (xmlChar *) "orientation",  gui->lilycontrol.orientation);

  if(gui->lilycontrol.directives) 
	    newDirectivesElem(parentElem, ns, gui->lilycontrol.directives, "score-directives");

  GList *custom;
  for(custom=g_list_last(gui->custom_scoreblocks);custom;custom=custom->prev) {
    xmlNewChild (scoreElem, ns, "custom_scoreblock", (xmlChar *)((GString*)(((DenemoScoreblock*)custom->data)->scoreblock)->str));
    if(((DenemoScoreblock*)custom->data)->visible)
      xmlNewChild (scoreElem, ns, "visible_scoreblock", NULL);
  }
  if(gui->custom_prolog && gui->custom_prolog->len)
    xmlNewChild (scoreElem, ns, "custom_prolog", (xmlChar *)gui->custom_prolog->str);
  
  GList *g;
  for(g=gui->movements;g;g=g->next) {
    DenemoScore *si = g->data;

  mvmntElem = xmlNewChild (scoreElem, ns, (xmlChar *) "movement", NULL);
  parentElem = xmlNewChild (mvmntElem, ns, (xmlChar *) "edit-info", NULL);
  newXMLIntChild (parentElem, ns, (xmlChar *) "staffno", si->currentstaffnum);
  newXMLIntChild (parentElem, ns, (xmlChar *) "cursorposition",
		  si->cursor_x - 1);


  parentElem = xmlNewChild (mvmntElem, ns, (xmlChar *) "score-info", NULL);
  curElem = xmlNewChild (parentElem, ns, (xmlChar *) "tempo", NULL);
  newXMLFraction (xmlNewChild (curElem, ns, (xmlChar *) "duration", NULL), ns,
		  1, 4);
  newXMLIntChild (curElem, ns, (xmlChar *) "bpm", si->tempo);
  xmlNewChild (parentElem, ns, (xmlChar *) "title",
	       (xmlChar *) si->headerinfo.title->str);
  xmlNewChild (parentElem, ns, (xmlChar *) "subtitle",
	       (xmlChar *) si->headerinfo.subtitle->str);
  xmlNewChild (parentElem, ns, (xmlChar *) "composer",
	       (xmlChar *) si->headerinfo.composer->str);
  xmlNewChild (parentElem, ns, (xmlChar *) "poet",
	       (xmlChar *) si->headerinfo.poet->str);
  xmlNewChild (parentElem, ns, (xmlChar *) "meter",
	       (xmlChar *) si->headerinfo.meter->str);
  xmlNewChild (parentElem, ns, (xmlChar *) "opus",
	       (xmlChar *) si->headerinfo.opus->str);
  xmlNewChild (parentElem, ns, (xmlChar *) "arranger",
	       (xmlChar *) si->headerinfo.arranger->str);
  xmlNewChild (parentElem, ns, (xmlChar *) "instrument",
	       (xmlChar *) si->headerinfo.instrument->str);
  xmlNewChild (parentElem, ns, (xmlChar *) "dedication",
	       (xmlChar *) si->headerinfo.dedication->str);
  xmlNewChild (parentElem, ns, (xmlChar *) "piece",
	       (xmlChar *) si->headerinfo.piece->str);
  xmlNewChild (parentElem, ns, (xmlChar *) "head",
	       (xmlChar *) si->headerinfo.head->str);
  xmlNewChild (parentElem, ns, (xmlChar *) "copyright",
	       (xmlChar *) si->headerinfo.copyright->str);
  xmlNewChild (parentElem, ns, (xmlChar *) "footer",
	       (xmlChar *) si->headerinfo.footer->str);
  xmlNewChild (parentElem, ns, (xmlChar *) "tagline",
	       (xmlChar *) si->headerinfo.tagline->str);
  xmlNewChild (parentElem, ns, (xmlChar *) "extra",
	       (xmlChar *) si->headerinfo.extra->str);
  xmlNewChild (parentElem, ns, (xmlChar *) "markup_before",
	       (xmlChar *) si->headerinfo.lilypond_before->str);
  xmlNewChild (parentElem, ns, (xmlChar *) "markup_after",
	       (xmlChar *) si->headerinfo.lilypond_after->str);
  xmlNewChild (parentElem, ns, (xmlChar *) "layout_markup",
	       (xmlChar *) si->headerinfo.layout->str);

  /* Output each (primary) staff, and store the IDs in a hash table. */
  fraction = 1.0 / (gdouble) g_list_length (si->thescore);
  fraction /= 3;
	
	
  stavesElem = xmlNewChild (mvmntElem, ns, (xmlChar *) "staves", NULL);
  for (curStaff = si->thescore; curStaff != NULL; curStaff = curStaff->next)
    {
      curStaffStruct = (DenemoStaff *) curStaff->data;
      if (curStaffStruct->voicenumber != 2)
	{
	  parentElem =
	    xmlNewChild (stavesElem, ns, (xmlChar *) "staff", NULL);
	  staffXMLID = getXMLID (curStaffStruct);
	  xmlSetProp (parentElem, (xmlChar *) "id", (xmlChar *) staffXMLID);
	  curElem = xmlNewChild (parentElem, ns,
				 (xmlChar *) "staff-info", NULL);
	  newXMLIntChild (curElem, ns, (xmlChar *) "number-of-lines",
			  curStaffStruct->no_of_lines);
	  newXMLIntChild (curElem, ns, (xmlChar *) "transpose",
			  curStaffStruct->transposition);
	  xmlNewChild (curElem, ns, (xmlChar *) "instrument",
		       (xmlChar *) curStaffStruct->midi_instrument->str);
	   newXMLIntChild (curElem, ns, (xmlChar *) "volume",
			   			  curStaffStruct->volume);
	  newXMLIntChild (curElem, ns, (xmlChar *) "midi_prognum_override",
	               curStaffStruct->midi_prognum_override);
	  newXMLIntChild (curElem, ns, (xmlChar *) "midi_prognum",
	               curStaffStruct->midi_prognum);
	  newXMLIntChild (curElem, ns, (xmlChar *) "midi_channel",
	               curStaffStruct->midi_channel);
	  

#define PUTCHILD(A,B) if(curStaffStruct->context & A) xmlNewChild (curElem, ns, (xmlChar *) "context", B);
          PUTCHILD(DENEMO_PIANO_START, PIANO_START_STRING);
          PUTCHILD(DENEMO_PIANO_END, PIANO_END_STRING);
          PUTCHILD(DENEMO_CHOIR_START, CHOIR_START_STRING);
          PUTCHILD(DENEMO_CHOIR_END, CHOIR_END_STRING);
          PUTCHILD(DENEMO_GROUP_START, GROUP_START_STRING);
          PUTCHILD(DENEMO_GROUP_END, GROUP_END_STRING);
#undef PUTCHILD
	  newXMLIntChild (curElem, ns, (xmlChar *) "space_above",
			  curStaffStruct->space_above);
	  newXMLIntChild (curElem, ns, (xmlChar *) "space_below",
			  curStaffStruct->space_below);
	  newXMLIntChild (curElem, ns, (xmlChar *) "hasfigures",
			  curStaffStruct->hasfigures);
	  newXMLIntChild (curElem, ns, (xmlChar *) "haslyrics",
			  curStaffStruct->haslyrics);
	  newXMLIntChild (curElem, ns, (xmlChar *) "hasfakechords",
			  curStaffStruct->hasfakechords);
	  
	  if(curStaffStruct->staff_prolog && curStaffStruct->staff_prolog->len)
	     xmlNewChild (curElem, ns, (xmlChar *) "staff-prolog",
		       (xmlChar *) curStaffStruct->staff_prolog->str);
	  
	  if(curStaffStruct->staff_directives) 
	    newDirectivesElem(curElem, ns,curStaffStruct->staff_directives, "staff-directives");
	  
	  if(curStaffStruct->voice_directives)
	    newDirectivesElem(curElem, ns,curStaffStruct->voice_directives, "voice-directives");
	  if(curStaffStruct->clef.directives)
	    newDirectivesElem(curElem, ns,curStaffStruct->voice_directives, "clef-directives");
	    

	  if(curStaffStruct->lyrics_prolog && curStaffStruct->lyrics_prolog->len)
	     xmlNewChild (curElem, ns, (xmlChar *) "lyrics-prolog",
		       (xmlChar *) curStaffStruct->lyrics_prolog->str);
	  if(curStaffStruct->figures_prolog && curStaffStruct->figures_prolog->len)
	     xmlNewChild (curElem, ns, (xmlChar *) "figures-prolog",
		       (xmlChar *) curStaffStruct->figures_prolog->str);
	  if(curStaffStruct->fakechords_prolog && curStaffStruct->fakechords_prolog->len)
	     xmlNewChild (curElem, ns, (xmlChar *) "fakechords-prolog",
		       (xmlChar *) curStaffStruct->fakechords_prolog->str);
	}
    }

  /* Output each voice. */

  voicesElem = xmlNewChild (mvmntElem, ns, (xmlChar *) "voices", NULL);
  for (curStaff = si->thescore; curStaff != NULL; curStaff = curStaff->next)
    {
      curStaffStruct = (DenemoStaff *) curStaff->data;

      /* Initialize voice-wide variables. */

      prevTieElem = NULL;
      slurElemStack = NULL;
      crescElemStack = NULL;
      diminElemStack = NULL;
      lastBeamStartXMLID = NULL;
      lastTupletStartXMLID = NULL;
      lastGraceStartXMLID = NULL;

      /*
       * If this is a primary voice, find the ID of its staff, which applies
       * until the next primary voice we run across.
       */

      if (curStaffStruct->voicenumber != 2)
	{
	  staffXMLID = getXMLID (curStaffStruct);
	}

      voiceElem = xmlNewChild (voicesElem, ns, (xmlChar *) "voice", NULL);
      voiceXMLID = newXMLID ();
      xmlSetProp (voiceElem, (xmlChar *) "id", (xmlChar *) voiceXMLID);

      /* Nobody actually needs the voice ID right now, so we throw it away. */

      g_free (voiceXMLID);

      /*
       * Output the voice info (voice name and first measure number, which
       * currently is always 1.
       */

      parentElem =
	xmlNewChild (voiceElem, ns, (xmlChar *) "voice-info", NULL);
      xmlNewChild (parentElem, ns, (xmlChar *) "voice-name",
		   (xmlChar *) curStaffStruct->denemo_name->str);
      newXMLIntChild (parentElem, ns, (xmlChar *) "first-measure-number", 1);

      /*
       * Output the initial voice parameters:
       *     - staff on which this voice resides
       *     - clef
       *     - key signature
       *     - time signature
       */

      parentElem = xmlNewChild (voiceElem, ns,
				(xmlChar *) "initial-voice-params", NULL);
      curElem = xmlNewChild (parentElem, ns, (xmlChar *) "staff-ref", NULL);
      xmlSetProp (curElem, (xmlChar *) "staff", (xmlChar *) staffXMLID);
      newXMLClef (parentElem, ns, curStaffStruct->clef.type);
      newXMLKeySignature (parentElem, ns, curStaffStruct->skey,
			  curStaffStruct->skey_isminor);
      curTime1 = curStaffStruct->stime1;
      curTime2 = curStaffStruct->stime2;
      newXMLTimeSignature (parentElem, ns, curTime1, curTime2);

      /* Write out the measures. */
      gboolean warning_given = FALSE;//No warning yet about empty measures
      measuresElem =
	xmlNewChild (voiceElem, ns, (xmlChar *) "measures", NULL);
      for (curMeasure = curStaffStruct->measures; curMeasure != NULL;
	   curMeasure = curMeasure->next)
	{
	  emptyMeasure = TRUE;
	  measureElem = xmlNewChild (measuresElem, ns, (xmlChar *) "measure",
				     NULL);

	  for (curObjNode = (objnode *) curMeasure->data;
	       curObjNode != NULL; curObjNode = curObjNode->next)
	    {
	      curObj = (DenemoObject *) curObjNode->data;

	      switch (curObj->type)
		{
		case CHORD:
		  {chord *thechord = (chord *) curObj->object;
		  emptyMeasure = FALSE;

		  
		  /* FIXME WHY DO WE EXPORT THIS INFO: it is derived from the data
		     If this is the start of a beam, output a <beam-start>. */

		  if (curObj->isstart_beamgroup && !curObj->isend_beamgroup)
		    {
		      curElem = xmlNewChild (measureElem, ns,
					     (xmlChar *) "beam-start", NULL);
		      lastBeamStartXMLID = newXMLID ();
		      xmlSetProp (curElem, (xmlChar *) "id",
				  (xmlChar *) lastBeamStartXMLID);
		    }

		  /* Output the root element, "rest" or "chord". */

		  if (((chord *) curObj->object)->notes == NULL)
		    {
		      objElem = xmlNewChild (measureElem, ns,
					     (xmlChar *) "rest", NULL);
		      if (curObj->isinvisible)
			xmlSetProp (objElem, (xmlChar *) "show", (xmlChar *)
				    "false");
		      else
			xmlSetProp (objElem, (xmlChar *) "show",
				    (xmlChar *) "true");
		    }
		  else
		    objElem =
		      xmlNewChild (measureElem, ns, (xmlChar *) "chord",
				   NULL);
		  chordXMLID = getXMLID (curObj);
		  xmlSetProp (objElem, (xmlChar *) "id",
			      (xmlChar *) chordXMLID);

		  /* Output the duration. */

		  determineDuration (((chord *) curObj->object)->baseduration,
				     &durationType);
		  parentElem = xmlNewChild (objElem, ns,
					    (xmlChar *) "duration", NULL);
		  xmlSetProp (parentElem, (xmlChar *) "base",
			      (xmlChar *) durationType);
		  if (((chord *) curObj->object)->numdots != 0)
		    newXMLIntChild (parentElem, ns, (xmlChar *) "dots",
				    ((chord *) curObj->object)->numdots);

		  /* Output the DenemoDirectives on the chord */
		  if(((chord *) curObj->object)->directives) {
		    newDirectivesElem(objElem, ns,  ((chord *) curObj->object)->directives, "directives");
		  }
		  if(((chord *) curObj->object)->chordize)
		    newXMLIntChild (objElem, ns, (xmlChar *) "chordize", TRUE);

		  /*Output Lyric */
		  if (((chord *) curObj->object)->lyric)
		    {
		      parentElem =
			xmlNewChild (objElem, ns, (xmlChar *) "lyric",
				     (xmlChar *) ((chord *) curObj->object)->
				     lyric->str);
		      if (((chord *) curObj->object)->is_syllable)
			xmlSetProp (parentElem, (xmlChar *) "extend",
				    (xmlChar *) "true");
		      else
			xmlSetProp (parentElem, (xmlChar *) "extend",
				    (xmlChar *) "false");

		      if (((chord *) curObj->object)->center_lyric)
			xmlSetProp (parentElem, (xmlChar *) "center",
				    (xmlChar *) "true");
		      else
			xmlSetProp (parentElem, (xmlChar *) "center",
				    (xmlChar *) "false");

		    }
		  /*Output Figured Bass */
		  if (((chord *) curObj->object)->figure)
		    {
		    //DenemoObject *mud = (DenemoObject *) ((GList *) (((chord *) curObj->object)->figure)->data);
		    //chord *mych = (chord *) mud->object;
		  
		   	parentElem = xmlNewChild (objElem, ns, (xmlChar *) "figure",
						(xmlChar *) ((GString *) ((chord *) curObj->object)->figure)->str);
			
			// printf("\nfigure in exportxml == %s\n", ((GString *) ((chord *) curObj->object)->figure)->str);
		  } 
		    //((chord *) curObj->object)->is_figure = FALSE;
		 //     parentElem = xmlNewChild (objElem, ns, (xmlChar *) "figure",
		//				(GString *) mych->figure);
		    
		  /*Output Fakechords*/
		  if (((chord *) curObj->object)->fakechord && ((GString *) ((chord *) curObj->object)->fakechord)->len)
		    {
		    	
  			   GString *temp = g_string_new("");
		           temp = g_string_append(temp, ((GString *) ((chord *) curObj->object)->fakechord)->str);
			   if (((chord *) curObj->object)->fakechord_extension != NULL)
			   	temp = g_string_append(temp, ((GString *) ((chord *) curObj->object)->fakechord_extension)->str);
			   

		      parentElem = xmlNewChild (objElem, ns, (xmlChar *) "fakechord",
						(xmlChar *) ((GString *) temp)->str);

  
		      }


		  /* Output all the decorations. */

		  if (((chord *) curObj->object)->ornamentlist)
		    {
		      GList *tmp;
		      for (tmp = ((chord *) curObj->object)->ornamentlist;
			   tmp; tmp = tmp->next)
			{
			  parentElem = xmlNewChild (objElem, ns,
						    (xmlChar *) "decorations",
						    NULL);
			  if (*(enum ornament *) tmp->data == STACCATO)
			    newXMLDecoration (parentElem, ns, "staccato");
			  if (*(enum ornament *) tmp->data == D_ACCENT)
			    newXMLDecoration (parentElem, ns, "accent");
			  if (*(enum ornament *) tmp->data == FERMATA)
			    newXMLDecoration (parentElem, ns, "fermata");
			  if (*(enum ornament *) tmp->data == TENUTO)
			    newXMLDecoration (parentElem, ns, "tenuto");
			  if (*(enum ornament *) tmp->data == TRILL)
			    newXMLDecoration (parentElem, ns, "trill");
			  if (*(enum ornament *) tmp->data == TURN)
			    newXMLDecoration (parentElem, ns, "turn");
			  if (*(enum ornament *) tmp->data == MORDENT)
			    newXMLDecoration (parentElem, ns, "mordent");
			  if (*(enum ornament *) tmp->data == STACCATISSIMO)
			    newXMLDecoration (parentElem, ns,
					      "staccatissimo");
			  if (*(enum ornament *) tmp->data == MARCATO)
			    newXMLDecoration (parentElem, ns, "marcato");
			  if (*(enum ornament *) tmp->data == UBOW)
			    newXMLDecoration (parentElem, ns, "up-bow");
			  if (*(enum ornament *) tmp->data == DBOW)
			    newXMLDecoration (parentElem, ns, "down-bow");
			  if (*(enum ornament *) tmp->data == RHEEL)
			    newXMLDecoration (parentElem, ns, "right-heel");
			  if (*(enum ornament *) tmp->data == LHEEL)
			    newXMLDecoration (parentElem, ns, "left-heel");
			  if (*(enum ornament *) tmp->data == RTOE)
			    newXMLDecoration (parentElem, ns, "right-toe");
			  if (*(enum ornament *) tmp->data == LTOE)
			    newXMLDecoration (parentElem, ns, "left-toe");
			  if (*(enum ornament *) tmp->data == CODA)
			    newXMLDecoration (parentElem, ns, "coda");
			  if (*(enum ornament *) tmp->data == FLAGEOLET)
			    newXMLDecoration (parentElem, ns, "flageolet");
			  if (*(enum ornament *) tmp->data == OPEN)
			    newXMLDecoration (parentElem, ns, "open");
			  if (*(enum ornament *) tmp->data == PRALLMORDENT)
			    newXMLDecoration (parentElem, ns, "prallmordent");
			  if (*(enum ornament *) tmp->data == PRALLPRALL)
			    newXMLDecoration (parentElem, ns, "prallprall");
			  if (*(enum ornament *) tmp->data == PRALL)
			    newXMLDecoration (parentElem, ns, "prall");
			  if (*(enum ornament *) tmp->data == REVERSETURN)
			    newXMLDecoration (parentElem, ns, "reverseturn");
			  if (*(enum ornament *) tmp->data == SEGNO)
			    newXMLDecoration (parentElem, ns, "segno");
			  if (*(enum ornament *) tmp->data == SFORZATO)
			    newXMLDecoration (parentElem, ns, "sforzato");
			  if (*(enum ornament *) tmp->data == STOPPED)
			    newXMLDecoration (parentElem, ns, "stopped");
			  if (*(enum ornament *) tmp->data == THUMB)
			    newXMLDecoration (parentElem, ns, "thumb");
			  if (*(enum ornament *) tmp->data == UPPRALL)
			    newXMLDecoration (parentElem, ns, "upprall");
			  if (*(enum ornament *) tmp->data == D_ARPEGGIO)
			    newXMLDecoration (parentElem, ns, "arpeggio");
			}
		    }

		  /*
		   *  Output Dynamic which is now part of note 
		   *
		   */
		  if (((chord *) curObj->object)->dynamics)
		    {
		      GString *string =
			(GString *) ((chord *) curObj->object)->dynamics->
			data;
		      parentElem =
			xmlNewChild (objElem, ns, (xmlChar *) "dynamic",
				     NULL);
		      xmlSetProp (parentElem, (xmlChar *) "name",
				  (xmlChar *) string->str);
		    }
		  /*
		   * If this is the end of a slur, terminate the previous
		   * <slur> element.
		   */

		  if (((chord *) curObj->object)->slur_end_p)
		    {
		      if (slurElemStack == NULL)
			{
			  g_warning ("Encountered slur end without a "
				     "beginning");
			}
		      else
			{
			  xmlSetProp ((xmlNodePtr) slurElemStack->data,
				      (xmlChar *) "to",
				      (xmlChar *) chordXMLID);

			  /* Pop the top element off the stack. */

			  slurElemStack = g_list_remove (slurElemStack,
							 slurElemStack->data);
			}
		    }

		  /*
		   * If this is the end of a crescendo, terminate the previous
		   * <crescendo> element.
		   */

		  if (((chord *) curObj->object)->crescendo_end_p)
		    {
		      if (crescElemStack == NULL)
			{
			  g_warning ("Encountered crescendo end without a "
				     "beginning");
			}
		      else
			{
			  xmlSetProp ((xmlNodePtr) crescElemStack->data,
				      (xmlChar *) "to",
				      (xmlChar *) chordXMLID);

			  /* Pop the top element off the stack. */

			  crescElemStack = g_list_remove (crescElemStack,
							  crescElemStack->
							  data);
			}
		    }

		  /*
		   * If this is the end of a diminuendo, terminate the previous
		   * <diminuendo> element.
		   */

		  if (((chord *) curObj->object)->diminuendo_end_p)
		    {
		      if (diminElemStack == NULL)
			{
			  g_warning ("Encountered diminuendo end without a "
				     "beginning");
			}
		      else
			{
			  xmlSetProp ((xmlNodePtr) diminElemStack->data,
				      (xmlChar *) "to",
				      (xmlChar *) chordXMLID);

			  /* Pop the top element off the stack. */

			  diminElemStack = g_list_remove (diminElemStack,
							  diminElemStack->
							  data);
			}
		    }

		  /*
		   * Output a <slur> element (to be filled in by the end of
		   * the slur) if there's a slur beginning on this chord.
		   */

		  if (((chord *) curObj->object)->slur_begin_p)
		    {
		      parentElem =
			xmlNewChild (objElem, ns, (xmlChar *) "slurs", NULL);
		      curElem =
			xmlNewChild (parentElem, ns, (xmlChar *) "slur",
				     NULL);

		      /* Push the <slur> element onto the slur stack. */

		      slurElemStack = g_list_prepend (slurElemStack, curElem);
		    }

		  /*
		   * Output a <crescendo> element (to be filled in by the end of
		   * the crescendo) if there's a crescendo beginning on this chord.
		   */

		  if (((chord *) curObj->object)->crescendo_begin_p)
		    {
		      parentElem =
			xmlNewChild (objElem, ns, (xmlChar *) "crescendos",
				     NULL);
		      curElem =
			xmlNewChild (parentElem, ns, (xmlChar *) "crescendo",
				     NULL);

		      /* Push the <crescendo> element onto the crescendo stack. */

		      crescElemStack =
			g_list_prepend (crescElemStack, curElem);
		    }

		  /*
		   * Output a <diminuendo> element (to be filled in by the end of
		   * the diminuendo) if there's a diminuendo beginning on this chord.
		   */

		  if (((chord *) curObj->object)->diminuendo_begin_p)
		    {
		      parentElem =
			xmlNewChild (objElem, ns, (xmlChar *) "diminuendos",
				     NULL);
		      curElem =
			xmlNewChild (parentElem, ns, (xmlChar *) "diminuendo",
				     NULL);

		      /* Push the <diminuendo> element onto the diminuendo stack. */

		      diminElemStack =
			g_list_prepend (diminElemStack, curElem);
		    }

		  /*
		   * If the previous chord was tied, fill in the <tie>
		   * element to point to this one.
		   */

		  if (prevTieElem != NULL)
		    {
		      xmlSetProp (prevTieElem, (xmlChar *) "to",
				  (xmlChar *) chordXMLID);
		      prevTieElem = NULL;
		    }

		  /* Output a <tie> element if this chord is tied. */

		  if (((chord *) curObj->object)->is_tied)
		    {
		      prevTieElem =
			xmlNewChild (objElem, ns, (xmlChar *) "tie", NULL);
		    }

 


		  /* Output all the notes, if this isn't a rest. */

		  if (((chord *) curObj->object)->notes != NULL)
		    {
		      parentElem =
			xmlNewChild (objElem, ns, (xmlChar *) "notes", NULL);
		      for (curNoteNode = ((chord *) curObj->object)->notes;
			   curNoteNode != NULL;
			   curNoteNode = curNoteNode->next)
			{
			  curNote = (note *) curNoteNode->data;
			  curElem =
			    xmlNewChild (parentElem, ns, (xmlChar *) "note",
					 NULL);
			  noteXMLID = getXMLID (curNote);
			  xmlSetProp (curElem, (xmlChar *) "id",
				      (xmlChar *) noteXMLID);
			  newXMLIntChild (curElem, ns,
					  (xmlChar *) "middle-c-offset",
					  curNote->mid_c_offset);
			  if (curNote->enshift != 0
			      || curNote->showaccidental)
			    {
			      newXMLAccidental (curElem, ns, curNote->enshift,
						curNote->showaccidental);
			    }
			  if (curNote->noteheadtype != DENEMO_NORMAL_NOTEHEAD)
			    {
			      newXMLNoteHead (curElem, ns,
					      curNote->noteheadtype);
			    }

			  if(curNote->directives) {
			    newDirectivesElem(curElem, ns, curNote->directives, "directives");
#if 0
			    directivesElem =  xmlNewChild (curElem, ns, (xmlChar *) "directives", NULL);
			    GList *g = curNote->directives;
			    for(;g;g=g->next) {
			      DenemoDirective *directive = (DenemoDirective *)g->data;
			      directiveElem =  xmlNewChild (directivesElem, ns, (xmlChar *) "directive", NULL);	
#define DO_DIREC(field)  if (directive->field\
                   && directive->field->len)\
                      xmlNewChild (directiveElem, ns, (xmlChar *) #field,\
				     (xmlChar *) directive->field->str);

#define DO_INTDIREC(field)   newXMLIntChild (directiveElem, ns, (xmlChar *) #field,\
				             directive->field);

			      DO_DIREC(tag);
			      DO_DIREC(prefix);
			      DO_DIREC(postfix);
			      DO_DIREC(display);
			      DO_DIREC(graphic_name);

			      DO_INTDIREC(minpixels);
			      DO_INTDIREC(x);
			      DO_INTDIREC(y);
			      DO_INTDIREC(tx);
			      DO_INTDIREC(ty);
			      DO_INTDIREC(gx);
			      DO_INTDIREC(gy);
			      DO_INTDIREC(override);


#undef DO_DIREC
#undef DO_INTDIREC
			    }
#endif
			  }

			}
		      }
		  /* If this is the end of a beam, output a <beam-end>. */

		  if (curObj->isend_beamgroup && !curObj->isstart_beamgroup)
		    {
		      curElem =
			xmlNewChild (measureElem, ns, (xmlChar *) "beam-end",
				     NULL);
		      if (lastBeamStartXMLID == NULL)
			{
			  g_warning
			    ("Encountered the end of a beam without a "
			     "beginning");
			}
		      else
			{
			  xmlSetProp (curElem, (xmlChar *) "beam",
				      (xmlChar *) lastBeamStartXMLID);
			  g_free (lastBeamStartXMLID);
			  lastBeamStartXMLID = NULL;
			}
		    }
		  }
		  break;

		case TUPOPEN:
		  objElem =
		    xmlNewChild (measureElem, ns, (xmlChar *) "tuplet-start",
				 NULL);
		  /*
		   * FIXME: This code does not yet handle nested tuplets.  For
		   *        that, we'd need a stack of "tuplet-start" IDs
		   *        instead of just a single "last ID."
		   */

		  lastTupletStartXMLID = getXMLID (curObj);
		  xmlSetProp (objElem, (xmlChar *) "id",
			      (xmlChar *) lastTupletStartXMLID);
		  newXMLFraction (xmlNewChild
				  (objElem, ns, (xmlChar *) "multiplier",
				   NULL), ns,
				  ((tupopen *) curObj->object)->numerator,
				  ((tupopen *) curObj->object)->denominator);
		  break;

		case TUPCLOSE:
		  objElem =
		    xmlNewChild (measureElem, ns, (xmlChar *) "tuplet-end",
				 NULL);
		  if (lastTupletStartXMLID == NULL)
		    {
		      g_warning ("Encountered nested tuplets or tuplet end "
				 "without start");
		    }
		  else
		    {
		      xmlSetProp (objElem, (xmlChar *) "tuplet",
				  (xmlChar *) lastTupletStartXMLID);
		      lastTupletStartXMLID = NULL;
		    }
		  break;

		case CLEF:
		  objElem = newXMLClef (measureElem, ns,
					((clef *) curObj->object)->type);
		  break;

		case TIMESIG:
		  objElem = newXMLTimeSignature (measureElem, ns,
						 ((timesig *)
						  curObj->object)->time1,
						 ((timesig *)
						  curObj->object)->time2);
		  break;

		case KEYSIG:
		  objElem = newXMLKeySignature
		    (measureElem, ns,
		     ((keysig *) curObj->object)->number,
		     ((keysig *) curObj->object)->isminor);
		  break;

		case STEMDIRECTIVE:
		  objElem = newXMLStemDirective (measureElem, ns,
						 ((stemdirective *)
						  curObj->object)->type);
		  break;

		case GRACE_START:
		  objElem =
		    xmlNewChild (measureElem, ns, (xmlChar *) "grace-start",
				 NULL);

		  /*
		   * FIXME: This code does not yet handle nested grace note
		   *        passages.  For that, we'd need a stack of
		   *        "grace-start" IDs instead of just a single "last
		   *        ID."
		   */

		  lastGraceStartXMLID = getXMLID (curObj);
		  xmlSetProp (objElem, (xmlChar *) "id",
			      (xmlChar *) lastGraceStartXMLID);
		  xmlSetProp (objElem, (xmlChar *) "on-beat",
			      ((grace *) curObj->object)->on_beat ?
			      (xmlChar *) "true" : (xmlChar *) "false");
		  /*
		   * FIXME: What's curObj->u.graceval.duration for?  It
		   *        doesn't seem to be used.
		   */
		  break;

		case GRACE_END:
		  objElem =
		    xmlNewChild (measureElem, ns, (xmlChar *) "grace-end",
				 NULL);
		  if (lastGraceStartXMLID == NULL)
		    {
		      g_warning ("Encountered nested grace note passages or "
				 "grace note end without start");
		    }
		  else
		    {
		      xmlSetProp (objElem, (xmlChar *) "grace",
				  (xmlChar *) lastGraceStartXMLID);
		      lastGraceStartXMLID = NULL;
		    }
		  break;
		case LYRIC:
		  objElem = xmlNewChild (measureElem, ns, (xmlChar *) "lyric",
					 (xmlChar *) ((lyric *) curObj->
						      object)->lyrics->str);

		  if (((lyric *) curObj->object)->is_syllable)
		    xmlSetProp (objElem, (xmlChar *) "extend",
				(xmlChar *) "true");
		  else
		    xmlSetProp (objElem, (xmlChar *) "extend",
				(xmlChar *) "false");

		  if (((lyric *) curObj->object)->center_lyric)
		    xmlSetProp (objElem, (xmlChar *) "center",
				(xmlChar *) "true");
		  else
		    xmlSetProp (objElem, (xmlChar *) "center",
				(xmlChar *) "false");

		  break;
		case LILYDIRECTIVE:
		  //FIXME this should really have been the tag saved here, but for backwards compatibility we use the postfix string.
		  if( ((lilydirective *) curObj->object)->
				     postfix && ((lilydirective *) curObj->object)->
		      postfix->len)
		    objElem =
		      xmlNewChild (measureElem, ns, (xmlChar *) "lily-directive",
				   (xmlChar *) ((lilydirective *) curObj->object)->
				   postfix->str);
		  else
		    objElem =
		      xmlNewChild (measureElem, ns, (xmlChar *) "lily-directive",
				   (xmlChar *)" ");

		  
		  xmlSetProp (objElem, (xmlChar *) "locked",
			      (xmlChar *) (((lilydirective *) curObj->object)->locked?"true":"false"));

#define SETSTRING_PROP(field)\
		  if(((lilydirective *) curObj->object)->field && ((lilydirective *) curObj->object)->field->len)\
		    xmlSetProp (objElem, (xmlChar *) #field,\
			      (xmlChar *) (((lilydirective *) curObj->object)->field->str));

		    SETSTRING_PROP (tag);
		    SETSTRING_PROP (display);
		    SETSTRING_PROP (graphic_name);
		    SETSTRING_PROP (prefix);//postfix done above, for backward compatibility
#undef SETSTRING_PROP

#define SETINT_PROP(x)\
		    newXMLIntProp (objElem, (xmlChar *) #x,\
				   (((lilydirective *) curObj->object)->x));
		    SETINT_PROP (x);
		    SETINT_PROP (y);
		    SETINT_PROP (tx);
		    SETINT_PROP (ty);
		    SETINT_PROP (gx);
		    SETINT_PROP (gy);
		    SETINT_PROP (minpixels);
#undef SETINT_PROP		  
		  break;
		case BARLINE:
		case MEASUREBREAK:
		  g_warning ("Cannot yet handle DenemoObjects of type %d",
			     curObj->type);
		  break;
		default:
		  /* FIXME: First try handlers. */
		  g_warning ("Got a DenemoObject of unknown type %d",
			     curObj->type);
		  break;
		}		/* end switch on object type */
	    }			/* end for each object in measure */

	  if (emptyMeasure)
	    {
#if 0
	      curElem =
		xmlNewChild (measureElem, ns, (xmlChar *) "rest", NULL);
	      xmlSetProp (curElem, (xmlChar *) "show", (xmlChar *) "false");
	      newXMLFraction (xmlNewChild
			      (curElem, ns, (xmlChar *) "duration", NULL), ns,
			      curTime1, curTime2);
	      if(!warning_given)
		if(strcmp(filename->str, gui->autosavename->str))
		warningdialog("Empty measure - putting a non-printing rest into it");
	      warning_given = TRUE;
#endif
	    }
	}			/* end for each measure in voice */

      /* Clean up voice-specific variables. */

      g_list_free (slurElemStack);
      g_list_free (crescElemStack);
      g_list_free (diminElemStack);
      g_free (lastBeamStartXMLID);
    }				/* end for each voice in score */
  }// for each movement
  /* Save the file. */

  if (xmlSaveFile (filename->str, doc) < 0)
    g_warning ("Could not save file %s", filename->str);

  /* Clean up all the memory we've allocated. */

  xmlFreeDoc (doc);
  g_hash_table_foreach (sStructToXMLIDMap, freeHashTableValue, NULL);
  g_hash_table_destroy (sStructToXMLIDMap);
  sNextXMLID = 0;

  g_string_free (filename, TRUE);
  }
