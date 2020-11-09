/* exportmusicxml.c
 * Functions for exporting a score to MusicXml
 * 
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2020 Richard Shann
 */

#include "config.h"
#include <denemo/denemo.h>
#include <libxml/xmlsave.h>
#include "core/exportxml.h"
#include "source/source.h"
#include "core/utils.h"
#include "command/lyric.h"
#include "command/lilydirectives.h"
#include "ui/texteditors.h"
#include "export/xmldefs.h"
#include "command/scorelayout.h"
#include "audio/pitchentry.h"
#include <stdlib.h>
#include <string.h>

/* libxml includes: for libxml2 this should be <libxml/tree.h> */
#include <libxml/tree.h>


#define XML_COMPRESSION_RATIO 3
/**
 * return a child node of parent, holding the passed name and integer.
 */
static xmlNodePtr
newXMLIntChild (xmlNodePtr parent, xmlNsPtr ns, const xmlChar * name, gint content)
{
  gchar *integer = g_strdup_printf ("%d", content);
  xmlNodePtr child = xmlNewChild (parent, ns, name, (xmlChar *) integer);
  g_free (integer);
  return child;
}

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
      *durationName = "16th";
      break;
    case 5:
      *durationName = "32nd";
      break;
    case 6:
      *durationName = "64th";
      break;
    case 7:
      *durationName = "128th";
      break;
    default:
      if (duration < -7)
        *durationName = "whole";
      else  if (duration < 0)
        {
          return determineDuration (-duration, durationName);
        }
      break;
    }
}

static void get_clef_sign (clefs theclef, gchar **sign, gint *line)
{
  switch (theclef)
	{
		case DENEMO_TREBLE_CLEF:
		*sign = "G";
		*line = 2;
		return;
		case DENEMO_BASS_CLEF:
		*sign = "F";
		*line = 3;
		return;
		case DENEMO_ALTO_CLEF:
		*sign = "C";
		*line = 3;
		return;
		
		case DENEMO_G_8_CLEF:
		*sign = "G";
		*line = 2;
		return;
		
		case DENEMO_TENOR_CLEF:
		*sign = "C";
		*line = 4;
		return;
		
		case DENEMO_SOPRANO_CLEF:
		*sign = "C";
		*line = 1;
		return;
		
		case DENEMO_F_8_CLEF:
		*sign = "F";
		*line = 2;
		return;
		
		case DENEMO_FRENCH_CLEF:
		*sign = "G";
		*line = 1;
		return;
		
		case DENEMO_BARITONE_CLEF:
		*sign = "C";
		*line = 5;
		return;
	}
}

/**

/**
 * Export the given score as a MusicXML file thefilname

 */
gint
exportmusicXML (gchar * thefilename, DenemoProject * gui)
{
  gint ret = 0;
  gint i, j, k;
  GString *filename = g_string_new (thefilename);
  xmlDocPtr doc;
  xmlNodePtr scoreElem, mvmntElem, stavesElem, voicesElem, voiceElem;
  xmlNodePtr measuresElem, measureElem;

  xmlNodePtr curElem, parentElem;
  xmlNsPtr ns;
  staffnode *curStaff;
  DenemoStaff *curStaffStruct;
  gchar *staffXMLID = 0, *voiceXMLID;

  measurenode *curMeasure;

  static gchar *version_string;
  if (version_string == NULL)
    version_string = g_strdup_printf ("%d", CURRENT_XML_VERSION);

  /* Initialize score-wide variables. */

  /* Create the XML document and output the root element. */

  doc = xmlNewDoc ((xmlChar *) "1.0");
  xmlSetDocCompressMode (doc, Denemo.prefs.compression);
  g_print ("Document compression mode set to %d, (read back as %d)\n", Denemo.prefs.compression, xmlGetDocCompressMode (doc));
  doc->xmlRootNode = scoreElem = xmlNewDocNode (doc, NULL, (xmlChar *) "score-partwise", NULL);
  ns = xmlNewNs (doc->xmlRootNode, (xmlChar *) DENEMO_XML_NAMESPACE, NULL);//FIXME - can we put NULL here???
  //xmlSetProp (scoreElem, (xmlChar *) "version", (xmlChar *) version_string);
DenemoMovement *si = gui->movement; // FIXME loop for movements
  parentElem = xmlNewChild (scoreElem, ns, (xmlChar *) "part-list", NULL);
		// give part ids and attributes for the staffs the ids as perhaps an arrav part_id[]
		
 		
  for (i=0, curStaff = si->thescore; curStaff != NULL; i++, curStaff = curStaff->next)
        {
		  xmlNodePtr partElem;
          curStaffStruct = (DenemoStaff *) curStaff->data;
		  

//output the i'th staff
	      partElem = xmlNewChild (scoreElem, ns, (xmlChar *) "part", NULL);
	      xmlSetProp (parentElem, (xmlChar *) "id", (xmlChar *) g_strdup_printf ("%d", i));
//output measures

          for (j=0, curMeasure = curStaffStruct->themeasures; curMeasure != NULL; j++, curMeasure = curMeasure->next)
            {
				
              DenemoMeasure *themeasure = (DenemoMeasure *) curMeasure->data;
              measureElem = xmlNewChild (partElem, ns, (xmlChar *) "measure", NULL);
			  xmlSetProp (measureElem, (xmlChar *) "number", (xmlChar *) g_strdup_printf ("%d", j));
//output initial clef, time key in first measure

			 if (j==0)
				{
				xmlNodePtr keyElem, clefElem, timeElem;
				xmlNodePtr attrElem;
				attrElem = xmlNewChild (measureElem, ns, (xmlChar *) "attributes", NULL);
				
				clefElem =  xmlNewChild (attrElem, ns, (xmlChar *) "clef", NULL);
				gchar *sign;
				gint line;
				get_clef_sign (curStaffStruct->clef.type, &sign, &line);
				xmlNewTextChild (clefElem, ns, (xmlChar *) "sign", (xmlChar *) sign);
				newXMLIntChild (clefElem, ns, (xmlChar *) "line", line);
			  
				timeElem = xmlNewChild (attrElem, ns, (xmlChar *) "time", NULL);
				newXMLIntChild (timeElem, ns, (xmlChar *) "beats", curStaffStruct->timesig.time1);
				newXMLIntChild (timeElem, ns, (xmlChar *) "beat-type", curStaffStruct->timesig.time2);
				
				keyElem = xmlNewChild (attrElem, ns, (xmlChar *) "key", NULL);
				newXMLIntChild (keyElem, ns, (xmlChar *) "fifths", curStaffStruct->keysig.number);
				xmlNewTextChild (keyElem, ns, (xmlChar *) "mode", (xmlChar *) (curStaffStruct->keysig.isminor?"Minor":"Major"));
				}
			 //output objects
			 //parseObjects (measureElem, ns, (objnode *) ((DenemoMeasure *) curMeasure->data)->objects);
			  objnode *curObjNode = (objnode *) ((DenemoMeasure *) curMeasure->data)->objects;
			  for (; curObjNode != NULL; curObjNode = curObjNode->next)
				{
				  DenemoObject *curObj = (DenemoObject *) curObjNode->data;

				  switch (curObj->type)
					{
						case CHORD:
						  {
							  xmlNodePtr pitchElem;
							  xmlNodePtr noteElem;
							  chord *thechord = (chord*)curObj->object;
							  noteElem = xmlNewChild (measureElem, ns, (xmlChar *) "note", NULL);

							  if (thechord->notes)
								{
								  pitchElem = xmlNewChild (noteElem, ns, (xmlChar *) "pitch", NULL);
								  note* curnote = (note*)(((chord*)thechord)->notes->data);
								  gchar *val = g_strdup_printf("%c", mid_c_offsettoname (curnote->mid_c_offset));
								  xmlNewTextChild (pitchElem, ns, (xmlChar *) "step", (xmlChar *) val); //the note-name from curnote
								  g_free(val);
								  newXMLIntChild (pitchElem, ns, (xmlChar *) "octave", 3+mid_c_offsettooctave (curnote->mid_c_offset)); //the octave from curnote
								  newXMLIntChild (pitchElem, ns, (xmlChar *) "alter", curnote->enshift);
								  
								  
								  
							  } else
							  {
								xmlNewChild (noteElem, ns, (xmlChar *) "rest", NULL); 
							  }
							  newXMLIntChild (noteElem, ns,  (xmlChar *) "duration", 1);//1 is duration a sounding duration quarter note
							  gchar *durationType;
							  determineDuration ((thechord)->baseduration, &durationType);
							  xmlNewTextChild (noteElem, ns,  (xmlChar *) "type",  (xmlChar *) durationType);
							  gint m = thechord->numdots;
							  for (;m;m--)
								xmlNewChild (noteElem, ns, (xmlChar *) "dot", NULL);
						}
						break;
					default:
						break;
				}//switch type

			 }//for each object
		}//for each measure

	}//for each staff

   // for each movement not done
  /* Save the file. */

  xmlSaveCtxt *ctxt = xmlSaveToFilename (filename->str, "UTF-8", XML_SAVE_FORMAT | XML_SAVE_NO_EMPTY);
  if (!ctxt || xmlSaveDoc (ctxt, doc) < 0 || xmlSaveClose (ctxt) < 0)
    {
      g_warning ("Could not save file %s", filename->str);
      ret = -1;
    }

  /* Clean up all the memory we've allocated. */

  xmlFreeDoc (doc);
  g_string_free (filename, TRUE);
  return ret;
}
