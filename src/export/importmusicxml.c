/*
 * importmusicxml.c
 *
 * Functions for importing a MusicXML file
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (C)  2010 Richard Shann
 *
 * License: this file may be used under the FSF GPL version 3 or later
 */
#include <string.h>
#include <denemo/denemo.h>
#include "core/prefops.h"            //for get_user_data_dir()
#include "export/file.h"
#include "core/utils.h"
#include "core/view.h"
#include "command/lilydirectives.h"

/* libxml includes: for libxml2 this should be <libxml.h> */
#include <libxml/parser.h>
#include <libxml/tree.h>

//A bad array access accessing before the array start is causing a crash sporadically (perhaps a bad xml file?) this pads the memory allocated as work-around
//#define g_malloc0(a) (g_malloc0(2*(a)) + (a))

static gint InitialVoiceNum = 0;
static gint *OttavaVals;

static GString *Warnings;
static GString *awaiting_note;//anything that has to wait for a note to be added
static gboolean pending_tuplet_end = FALSE;//to catch tuplets that end and re-start with the same value
/* Defines for making traversing XML trees easier */

#define FOREACH_CHILD_ELEM(childElem, parentElem) \
for ((childElem) = (parentElem)->children; \
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

/**
 * Get the text from the child node list of elem, convert it to an integer,
 * and return it.  If unsuccessful, return G_MAXINT.
 */
static gint
getXMLIntChild (xmlNodePtr elem)
{
  gchar *text = (gchar *) xmlNodeListGetString (elem->doc, elem->children, 1);
  gint num = G_MAXINT;
  if (text == NULL)
    {
      g_warning ("No child text found %s", elem->name);
    }
  else
    {
      if (sscanf (text, " %d", &num) != 1)
        {
          g_warning ("Could not convert child text \"%s\" of <%s> to number", text, elem->name);
          num = G_MAXINT;
        }
      g_free (text);
    }
  return num;
}

static void
parse_time (GString ** scripts, gint numvoices, gint measurenum, xmlNodePtr rootElem)
{
  xmlNodePtr childElem;
  gint numerator = 0, denominator = 0;
  gint i;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {

    if (ELEM_NAME_EQ (childElem, "beats"))
      numerator = getXMLIntChild (childElem);
    if (ELEM_NAME_EQ (childElem, "beat-type"))
      denominator = getXMLIntChild (childElem);
  }
  if (numerator && denominator)
    for (i = 0; i < numvoices; i++)
      if (measurenum == 1)
        g_string_append_printf (scripts[i + 1], "(d-InitialTimeSig \"%d/%d\")", numerator, denominator);
      else
        g_string_append_printf (scripts[i + 1], "(d-InsertTimeSig \"%d/%d\")(if (not (Appending?))(d-MoveCursorRight))", numerator, denominator);
}

static const gchar *
get_clef (gint line, gchar * sign, gint octave)
{
  switch (line)
    {
    case 1:
      if (*sign == 'G')
        return "French";
      if (*sign == 'C')
        return "Soprano";
    case 2:
      if (*sign == 'G')
      if (octave==-1)
		return "Treble Octava bassa";
	  else
        return "Treble";
    case 3:
      if (*sign == 'C')
        return "Alto";
    case 4:
      if (*sign == 'F')
      if (octave==-1)
		return "Bass Octava bassa";
	  else
        return "Bass";
      if (*sign == 'C')
        return "Tenor";
    case 5:
      if (*sign == 'C')
		return "Baritone";	
    default:
      return "Treble";
    }

}

static void
parse_key (GString ** scripts, gint numvoices, gint measurenum, xmlNodePtr rootElem)
{
  xmlNodePtr childElem;
  gint fifths = 0;
  gchar *mode = NULL;
  gint i;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {

    if (ELEM_NAME_EQ (childElem, "fifths"))
      fifths = getXMLIntChild (childElem);
   // if (ELEM_NAME_EQ (childElem, "mode"))
   //   mode = xmlNodeListGetString (childElem->doc, childElem->children, 1);
  }
 // if (mode)
    for (i = 0; i < numvoices; i++)
      if (measurenum == 1)
        g_string_append_printf (scripts[i + 1], "(d-InitialKey \"C major\")(d-IncrementKeysig %d)", fifths);
      else
        g_string_append_printf (scripts[i + 1], "(d-InsertKey \"C major\")(d-IncrementKeysig %d)", fifths);
 //g_free (mode);
}

static void
parse_clef (GString ** scripts, gint division, gint * voice_timings, gint voicenum, gint numvoices, gint * staff_for_voice, gint divisions, gint measurenum, xmlNodePtr rootElem)
{
  xmlNodePtr childElem;
  gint line = 0;
  gchar *sign = NULL;
  gint octave = 0;
  gchar *number = xmlGetProp (rootElem, (xmlChar *) "number");
  gint staffnum = 0;
  if (number)
    staffnum = atoi (number);
  if (staffnum == 0)
    staffnum = 1;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {                             //g_debug("clef change %s \n", childElem->name);
    if (ELEM_NAME_EQ (childElem, "line"))
      line = getXMLIntChild (childElem);
    if (ELEM_NAME_EQ (childElem, "sign"))
      sign = xmlNodeListGetString (childElem->doc, childElem->children, 1);
    if (ELEM_NAME_EQ (childElem, "clef-octave-change"))
      octave = getXMLIntChild (childElem);      
  }                            
  if (division > voice_timings[voicenum - 1])
    {
		g_print ("Clef called for invisible rests - ignored");
      //insert_invisible_rest (scripts[voicenum], division - voice_timings[voicenum - 1], divisions);
      voice_timings[voicenum - 1] = division;
    }
  if (sign)
    {
      gint i;
      const gchar *clef = get_clef (line, sign, octave);
      for (i = 0; i < numvoices; i++)
        {
          if (staff_for_voice[i] == staffnum)
            {  g_string_append_printf (scripts[i + 1], "(d-InsertClef \"%s\")", clef);
             //g_print ("clef inserted staff %d voice %d\n", staffnum, i+1);
             break;//secondary voices on that staff don't need a clef
			}
        }
    }
    g_free (sign);
}

static const gchar *
alteration (gint alter)
{
  switch (alter)
    {
    case -2:
      return "eses";
    case -1:
      return "es";
    case 1:
      return "is";
    case 2:
      return "isis";
    default:
      return "";
    }
}

static const gchar *
octave_string (gint octave)
{

  gchar *octavation;
  switch (octave)
    {
    case 0:
      octavation = ",,,";
      break;
    case 1:
      octavation = ",,";
      break;
    case 2:
      octavation = ",";
      break;
    case 3:
      octavation = "";
      break;
    case 4:
      octavation = "'";
      break;
    case 5:
      octavation = "''";
      break;

    case 6:
      octavation = "'''";
      break;

    case 7:
      octavation = "''''";
      break;

    case 8:
      octavation = "'''''";
      break;

    case 9:
      octavation = "''''''";
      break;

    default:
      octavation = "%{duration not implemented%}";
      break;
    }
  return octavation;
}

static gchar *
insert_note (gchar * type, gint octave, gchar * step, gint alter)
{
  if (step == NULL)
    {
      g_warning ("Note without step");
      return g_strdup ("");
    }
  gchar *duration_text = "";
  if (!strcmp (type, "whole"))
    duration_text = "\n(d-Set0)";
  else if (!strcmp (type, "half"))
    duration_text = "\n(d-Set1)";
  else if (!strcmp (type, "quarter"))
    duration_text = "\n(d-Set2)";
  else if (!strcmp (type, "eighth"))
    duration_text = "\n(d-Set3)";
  else if (!strcmp (type, "16th"))
    duration_text = "\n(d-Set4)";
  else if (!strcmp (type, "32nd"))
    duration_text = "\n(d-Set5)";
  else if (!strcmp (type, "64th"))
    duration_text = "\n(d-Set6)";
  else if (!strcmp (type, "128th"))
    duration_text = "\n(d-Set7)";
  else if (!strcmp (type, "256th"))
    duration_text = "\n(d-Set8)";
  else if (!strcmp (type, "breve"))
    duration_text = "\n(d-SetBreve)";
  else if (!strcmp (type, "longa"))
    duration_text = "\n(d-SetLonga)";
  else
    g_warning ("Note duration %s not implemented", type);
  const gchar *octavation = octave_string (octave);
  gchar *put_text = g_strdup_printf ("(d-InsertC)(d-PutNoteName \"%c%s%s\")", g_ascii_tolower (*step), alteration (alter), octavation);
  GString *ret = g_string_new (duration_text);
  g_string_append (ret, put_text);

  return g_string_free (ret, FALSE);
}

static gchar *
add_note (gint octave, gchar * step, gint alter)
{                               // d-InsertNoteInChord lily (d-ShiftCursor is relative (d-MoveTo ????
  const gchar *octavation = octave_string (octave);
  gchar *text = g_strdup_printf ("(InsertNoteInChord \"%c%s%s\")", g_ascii_tolower (*step), alteration (alter), octavation);

  GString *ret = g_string_new (text);
  g_free (text);
  return g_string_free (ret, FALSE);
}

static void
get_numstaffs_from_note (xmlNodePtr rootElem, gint * maxstaffs, gint * maxvoices)
{
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
    if (ELEM_NAME_EQ (childElem, "staff"))
      {
        gint staffnum = getXMLIntChild (childElem);
        if (staffnum > *maxstaffs)
          *maxstaffs = staffnum;        //g_debug("staff num %d ...", staffnum);
      }
    if (ELEM_NAME_EQ (childElem, "voice"))
      {
        gint voicenum = getXMLIntChild (childElem);
        if (voicenum > *maxvoices)
          *maxvoices = voicenum;
      }
  }
  //g_debug("So far %d %d\t", *maxstaffs, *maxvoices);
}

static void
get_numstaffs_in_measure (xmlNodePtr rootElem, gint * maxstaffs, gint * maxvoices)
{
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
    if (ELEM_NAME_EQ (childElem, "note"))
      {
        get_numstaffs_from_note (childElem, maxstaffs, maxvoices);
      }
    if (ELEM_NAME_EQ (childElem, "backup"))
      {                         //should someone do backup without specifying a voice for it.
        if (*maxvoices == 1)
          *maxvoices = (*maxvoices) + 1;
      }

  }

}


static gint
parseDuration (gint * current_voice, xmlNodePtr rootElem)
{
  xmlNodePtr childElem;
  gint duration = 0;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
    if (ELEM_NAME_EQ (childElem, "duration"))
      duration = getXMLIntChild (childElem);

    if (ELEM_NAME_EQ (childElem, "voice"))
      *current_voice = getXMLIntChild (childElem);
  }
  return duration;
}

static void
get_rest_for_duration (GString * ret, gint duration, gint divisions)
{
  //g_debug("Rest duration %d, divisions %d\n", duration, divisions);
  if (duration >= 4 * divisions)
    {
      g_string_append (ret, "(d-InsertRest0)");
      duration -= 4 * divisions;
      return get_rest_for_duration (ret, duration, divisions);
    }
  else if (duration >= 2 * divisions)
    {
      g_string_append (ret, "(d-InsertRest1)");
      duration -= 2 * divisions;
      return get_rest_for_duration (ret, duration, divisions);
    }
  else if (duration >= 1 * divisions)
    {
      g_string_append (ret, "(d-InsertRest2)");
      duration -= 1 * divisions;
      return get_rest_for_duration (ret, duration, divisions);
    }
  else if (2 * duration >= divisions && (divisions / 2))
    {
      g_string_append (ret, "(d-InsertRest3)");
      duration -= divisions / 2;
      return get_rest_for_duration (ret, duration, divisions);
    }
  else if (4 * duration >= divisions && (divisions / 4))
    {
      g_string_append (ret, "(d-InsertRest4)");
      duration -= divisions / 4;
      return get_rest_for_duration (ret, duration, divisions);
    }
  else if (8 * duration >= divisions && (divisions / 8))
    {
      g_string_append (ret, "(d-InsertRest5)");
      duration -= divisions / 8;
      return get_rest_for_duration (ret, duration, divisions);
    }
  else if (16 * duration >= divisions && (divisions / 16))
    {
      g_string_append (ret, "(d-InsertRest6)");
      duration -= divisions / 16;
      return get_rest_for_duration (ret, duration, divisions);
    }
  else if (32 * duration >= divisions && (divisions / 32))
    {
      g_string_append (ret, "(d-InsertRest7)");
      duration -= divisions / 32;
      return get_rest_for_duration (ret, duration, divisions);
    }
  else if (duration == 0)
    return;

  g_string_append (ret, "\n;Duration of rest not recognized\n");
}


static gchar *
add_rest (gchar * type, gint duration, gint divisions)
{
  gchar *duration_text = "";
  if (!strcmp (type, "whole"))
    {
      if (4 * divisions == duration)
        duration_text = "(d-InsertRest0)";
      else
        {
          GString *ret = g_string_new ("");
          get_rest_for_duration (ret, duration, divisions);
          return g_string_free (ret, FALSE);
        }
    }
  else if (!strcmp (type, "half"))
    duration_text = "(d-InsertRest1)";
  else if (!strcmp (type, "quarter"))
    duration_text = "(d-InsertRest2)";
  else if (!strcmp (type, "eighth"))
    duration_text = "(d-InsertRest3)";
  else if (!strcmp (type, "16th"))
    duration_text = "(d-InsertRest4)";
  else if (!strcmp (type, "32nd"))
    duration_text = "(d-InsertRest5)";
  else if (!strcmp (type, "64th"))
    duration_text = "(d-InsertRest6)";
  else if (!strcmp (type, "128th"))
    duration_text = "(d-InsertRest7)";
  else if (!strcmp (type, "256th"))
    duration_text = "(d-InsertRest8)";
  else if (!strcmp (type, "breve"))
    duration_text = "(d-InsertBreveRest)";
  else if (!strcmp (type, "longa"))
    duration_text = "(d-InsertLongaRest)";
  else
    g_warning ("Restduration %s not implemented", type);
  return g_strdup (duration_text);
}

static void
modify_time (xmlNodePtr rootElem, gint * actual_notes, gint * normal_notes)
{
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
    if (ELEM_NAME_EQ (childElem, "actual-notes"))
      *actual_notes = getXMLIntChild (childElem);
    if (ELEM_NAME_EQ (childElem, "normal-notes"))
      *normal_notes = getXMLIntChild (childElem);
  }
}




static void
parse_ornaments (GString * notations, xmlNodePtr rootElem)
{
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
    if (ELEM_NAME_EQ (childElem, "trill-mark"))
      {
        g_string_append (notations, "(d-ToggleTrill)");
      }
    if (ELEM_NAME_EQ (childElem, "turn"))
      {
        g_string_append (notations, "(d-ToggleTurn)");
      }
  }
}

static void
parse_articulations (GString * notations, xmlNodePtr rootElem)
{
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
    if (ELEM_NAME_EQ (childElem, "staccato"))
      g_string_append (notations, "(ToggleStaccato)");
    if (ELEM_NAME_EQ (childElem, "staccatissimo"))
      g_string_append (notations, "(ToggleStaccatissimo)");
  }
}

static void
parse_notations (GString * notations, xmlNodePtr rootElem, gint normal_notes, gint actual_notes)
{
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
    if (ELEM_NAME_EQ (childElem, "tuplet"))
      {
        gchar *type = xmlGetProp (childElem, (xmlChar *) "type");
        if (type && (!strcmp (type, "stop")))
			  g_string_append_printf (notations, "\n;Restart tuplet because pending tupend \n(ReStartTuplet \"%d/%d \")\n", normal_notes, actual_notes);
			  //pending_tuplet_end = TRUE;
        if (type && (!strcmp (type, "start")))
			  pending_tuplet_end = FALSE;
  
      }	
    if (ELEM_NAME_EQ (childElem, "articulations"))
      {
        parse_articulations (notations, childElem);
      }
    if (ELEM_NAME_EQ (childElem, "slur"))
      {
        gchar *type = xmlGetProp (childElem, (xmlChar *) "type");

        if (type && (!strcmp (type, "start")))
          g_string_append (notations, "(ToggleBeginSlur)");
        if (type && (!strcmp (type, "stop")))
          {
			  g_string_append (notations, "(ToggleEndSlur)");
			  //g_print ("notations %s\n", notations->str);
			 }
         //g_print ("slur type %s compare as %d\n", type, (!strcmp (type, "end")));
      }

    if (ELEM_NAME_EQ (childElem, "fermata"))
      {
        g_string_append (notations, "(ToggleFermata)");
      }
    //  I think we need functions to apply that aren't toggles.
//note tuplets will be ignored, we will depend on the timing changes (as at present), since tuplet start/end is a separate object which once inserted prevents us seeing the note/chord

    if (ELEM_NAME_EQ (childElem, "ornaments"))
      parse_ornaments (notations, childElem);
  }
}
#define INSERT_REST(num, den, rest) \
if(duration >= (num*divisions)/den)\
  {\
    g_string_append (script, "(d-InsertRest" rest ")(d-SetNonprinting)");\
    return insert_invisible_rest (script, duration - (num*divisions)/den, divisions);\
  } else

static gint
insert_invisible_rest (GString * script, gint duration, gint divisions)
{
  //g_assert (divisions);
  if (duration == 0)
    return TRUE;
  //g_debug("invis rest  %d, %d\n",  duration, divisions);
  INSERT_REST (4, 1, "0") INSERT_REST (2, 1, "1") INSERT_REST (1, 1, "2") INSERT_REST (1, 2, "3") INSERT_REST (1, 4, "4") INSERT_REST (1, 8, "5") INSERT_REST (1, 16, "6") INSERT_REST (1, 32, "7") INSERT_REST (1, 64, "8") g_warning ("Cannot cope with rest of %d/%d quarter notes", duration, divisions);
  return FALSE;
}

#undef INSERT_REST


// *division is the current position of the tick counter from the start of the measure
static gchar *
parse_note (xmlNodePtr rootElem, GString ** scripts, gint * staff_for_voice, gint * division, gint divisions, gint * voice_timings, gint * current_voice, gint * actual_notes, gint * normal_notes, gboolean is_nonprinting)
{
  GString *ret = g_string_new ("");
  xmlNodePtr childElem;
  gint octave, alter = 0;
  gchar *step = NULL;
  gchar *type = NULL;//duration type e.g. quarter, half etc
  gboolean in_chord = FALSE, is_dotted = FALSE, is_double_dotted = FALSE, is_rest = FALSE, is_whole_measure_rest = FALSE, is_grace = FALSE, is_tied = FALSE;
  GString *notations = g_string_new ("");
  gint voicenum = 1, staffnum = 1;
  gint duration = 0;
  gint initial_actual_notes = *actual_notes;
  gint initial_normal_notes = *normal_notes;
  gboolean timing_set = FALSE;  //for case where one voice ends during a tuplet and the next one starts during a tuplet
  GString *text = g_string_new ("");
  
  //if (pending_tuplet_end)
		//{
			//g_string_append_printf (awaiting_note, "\n;Restart tuplet because pending tupend \n(ReStartTuplet \"%d/%d \")\n", *normal_notes, *actual_notes);
			//pending_tuplet_end = FALSE;
		//}
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
    if (ELEM_NAME_EQ (childElem, "pitch"))
      {
        xmlNodePtr grandchildElem;
        FOREACH_CHILD_ELEM (grandchildElem, childElem)
        {
          if (ELEM_NAME_EQ (grandchildElem, "step"))
            step = xmlNodeListGetString (grandchildElem->doc, grandchildElem->children, 1);
          if (ELEM_NAME_EQ (grandchildElem, "octave"))
            octave = getXMLIntChild (grandchildElem) + OttavaVals[*current_voice];//g_print ("Ottava Vals %d", OttavaVals[*current_voice]);
          if (ELEM_NAME_EQ (grandchildElem, "alter"))
            alter = getXMLIntChild (grandchildElem);
        }
      }
    if (ELEM_NAME_EQ (childElem, "chord"))
      {
        in_chord = TRUE;
      }
    if (ELEM_NAME_EQ (childElem, "grace"))
      {
        is_grace = TRUE;
      }
      
  
    if (ELEM_NAME_EQ (childElem, "rest"))
      {
        is_rest = TRUE;
        gchar *whole  = xmlGetProp (childElem, (xmlChar *) "measure");
        is_whole_measure_rest = !g_strcmp0 (whole , "yes");
      }
    if (ELEM_NAME_EQ (childElem, "dot"))
      {
        if (is_dotted)
          is_double_dotted = TRUE;
        is_dotted = TRUE;
      }

    if (ELEM_NAME_EQ (childElem, "tie"))
      {
        gchar *start = xmlGetProp (childElem, (xmlChar *) "type");
        if (start && !strcmp ("start", start))
          is_tied = TRUE;
      }

    if (ELEM_NAME_EQ (childElem, "type")) //duration type e.g. quarter, half etc
      type = xmlNodeListGetString (childElem->doc, childElem->children, 1);
    if (ELEM_NAME_EQ (childElem, "duration"))
      {
        duration = getXMLIntChild (childElem);

      }
    if (ELEM_NAME_EQ (childElem, "voice"))
      voicenum = getXMLIntChild (childElem);
    if (ELEM_NAME_EQ (childElem, "staff"))
      staffnum = getXMLIntChild (childElem);
/*
               <notations>
                <tuplet number="1" type="stop"/>
                </notations>
*/
    if (ELEM_NAME_EQ (childElem, "notations"))
      {
        parse_notations (notations, childElem, *normal_notes, *actual_notes);
      }

    if (ELEM_NAME_EQ (childElem, "time-modification"))
      {
        timing_set = TRUE;
        modify_time (childElem, actual_notes, normal_notes);
      }
  } // end of for each child of the <note> element we are parsing
  if (voicenum < 1)
    {
      g_warning ("Bad MusicXML file voice 0 encountered");
      voicenum = 1;
    }
  if (staffnum < 1)
    {
      g_warning ("Bad MusicXML file staff 0 encountered");
      staffnum = 1;
    }
  if (staff_for_voice[voicenum - 1] == 0)
    staff_for_voice[voicenum - 1] = staffnum;

#ifdef FIXED_PROBLEM_WITH_VOICE_CHANGE_IN_CHORDS
// PROBLEM - this inserts an object which will come before inserting notes in a chord...
// Do we need a command to add a note to a chord which works in this position???? otherwise it's like tuplets.
  if (!in_chord && (staff_for_voice[voicenum - 1] != staffnum))
    {
      g_string_append_printf (scripts[voicenum], "(d-ChangeStaff \"voice %d\")(d-MoveCursorRight)", staffnum + InitialVoiceNum);        //always at end of bar !!!!!!!!!!! voice 1 staff 1 in debmand example.
      staff_for_voice[voicenum - 1] = staffnum;
      // g_warning("Voice %d in staff %d + %d need a staff change directive", voicenum, staffnum, InitialVoiceNum);
    }
#else
  if (!in_chord && (staff_for_voice[voicenum - 1] != staffnum))
    {
      g_string_append (ret, "Change Staff Omitted ");
    }
#endif


  if (*division > voice_timings[voicenum - 1])
    {g_print ("Discrepancy at %d %d\n", *division, voice_timings[voicenum - 1]);
     //this happens when you get a <backup> followed by a note in a different voice.
     insert_invisible_rest (scripts[voicenum], *division - voice_timings[voicenum - 1], divisions);
      voice_timings[voicenum - 1] = *division;
    }

  if (type)
    {

      g_string_append (text, in_chord ? add_note (octave, step, alter) : ((is_rest ||!step)? add_rest (type, duration, divisions) : insert_note (type, octave, step, alter)));

      if (is_nonprinting)
        g_string_append (text, "(d-SetNonprinting)");
      if (!(in_chord || is_grace))
        voice_timings[voicenum - 1] += duration;
      if (is_tied && !in_chord)
        g_string_append (text, "(d-ToggleTie)");
      if (is_grace)
        g_string_append (text, "(d-ToggleGrace)");
      if ((!in_chord) && is_dotted)
        g_string_append (text, "(d-AddDot)");
      if (is_double_dotted)
        g_string_append (text, "(d-AddDot)");


    }
  else if (is_rest)
    {                           //for the case where a rest is given without a type, just a duration.
     if(is_whole_measure_rest)
            g_string_append (text, "(d-InsertWholeMeasureRest)(d-MoveCursorLeft)");
     else
            get_rest_for_duration (text, duration, divisions);

    voice_timings[voicenum - 1] += duration;

    }


  if (!timing_set)
    {
      *actual_notes = 1;
      *normal_notes = 1;
    }

  if (((*current_voice != voicenum) && !(((initial_actual_notes) == 1) && (initial_normal_notes == 1))))
    {                           /* an unterminated tuplet in the last voice *///g_assert(*current_voice>0);
      g_string_append (scripts[*current_voice], "\n;Voice terminated during a tuplet\n(d-EndTuplet)");
pending_tuplet_end = FALSE;
      initial_actual_notes = 1;
      initial_normal_notes = 1;
    }

  if (((initial_actual_notes != *actual_notes) || (initial_normal_notes != *normal_notes)))
    {
      gchar *str;
      if ((initial_actual_notes) == 1 && (initial_normal_notes == 1))
        //str = g_strdup_printf ("(d-StartTuplet \"%d/%d\")", *normal_notes, *actual_notes);
        str = g_strdup_printf("\n;not end tuplet? %d and entered with normal timings in_chord %d  \n(d-StartTuplet \"%d/%d\")", pending_tuplet_end, in_chord, *normal_notes, *actual_notes);
      else
        {
        if (*normal_notes==1 && *actual_notes==1)
            //str = g_strdup_printf ("\n;Leaving tuplet timing\n(d-EndTuplet)");
            str = g_strdup_printf ("\n;Leaving tuplet timing with pending end %d\n(d-EndTuplet)", pending_tuplet_end);
        else
            //str = g_strdup_printf ("\n;Changed tuplet timings\n(d-EndTuplet)(d-StartTuplet \"%d/%d\")", *normal_notes, *actual_notes);
            str = g_strdup_printf ("\n;Changed tuplet timings at pending end %d\n(d-EndTuplet)(d-StartTuplet \"%d/%d\")", pending_tuplet_end, *normal_notes, *actual_notes);
        }
      g_string_append (scripts[voicenum], str);
      g_free (str);
    }


  
  g_string_append (text, awaiting_note->str);//is this neccesarily on the right voice???
  g_string_assign (awaiting_note, "");
  
  g_string_append (scripts[voicenum], text->str);
  g_string_append (scripts[voicenum], notations->str);
  g_string_free (notations, TRUE);
  g_string_free (text, TRUE);

  if (!(in_chord || is_grace))
    *division = *division + duration;
  *current_voice = voicenum;
  g_free (step);
  g_free (type);
  return g_string_free (ret, FALSE);
}

static void
get_staff_for_voice_note (xmlNodePtr rootElem, gint * staff_for_voice)
{
  xmlNodePtr childElem;
  gint voicenum = 1, staffnum = 1;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
    if (ELEM_NAME_EQ (childElem, "voice"))
      voicenum = getXMLIntChild (childElem);
    if (ELEM_NAME_EQ (childElem, "staff"))
      staffnum = getXMLIntChild (childElem);
  }
  if (voicenum < 1)
    {
      g_warning ("Bad MusicXML file voice 0 encountered");
      voicenum = 1;
    }
  if (staffnum < 1)
    {
      g_warning ("Bad MusicXML file staff 0 encountered");
      staffnum = 1;
    }
  if (staff_for_voice[voicenum - 1] == 0)
    staff_for_voice[voicenum - 1] = staffnum;
}

static void
parse_attributes (xmlNodePtr rootElem, GString ** scripts, gint numvoices, gint * staff_for_voice, gint division, gint * voice_timings, gint * divisions, gint * current_voice, gint measurenum)
{
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {                             //g_debug("attribute %s at division %d\n", childElem->name, division);
    if (ELEM_NAME_EQ (childElem, "time"))
      parse_time (scripts, numvoices, measurenum, childElem);
    if (ELEM_NAME_EQ (childElem, "key"))
      parse_key (scripts, numvoices, measurenum, childElem);
    if (ELEM_NAME_EQ (childElem, "clef"))
      parse_clef (scripts, division, voice_timings, *current_voice, numvoices, staff_for_voice, *divisions, measurenum, childElem);
    if (ELEM_NAME_EQ (childElem, "divisions"))
      *divisions = getXMLIntChild (childElem);
  }

}



static void
parse_barline (xmlNodePtr rootElem, GString ** scripts, gint numvoices)
{
  xmlNodePtr childElem;
  gchar *text = NULL;
  gchar *location;
  gchar *style = NULL, *repeat = NULL;
  gint i;
  gboolean left = TRUE;
  gchar *alt_type = NULL;//start or stop for 1st & 2nd time bars
  gchar *alt_num = NULL;//1 or 2 (or text???)
 
 // location = xmlGetProp (rootElem, "location");
//  if (location) g_string_append_printf (scripts[1], "(d-DirectivePut-standalone-data \"MusicXMLmove\" \"%s\")", location);
  
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {                             //g_debug("attribute %s at division %d\n", childElem->name, division);
    if (ELEM_NAME_EQ (childElem, "bar-style"))
      style = xmlNodeListGetString (childElem->doc, childElem->children, 1);
    if (ELEM_NAME_EQ (childElem, "repeat"))
      repeat = xmlGetProp (childElem, "direction");
     if (ELEM_NAME_EQ (childElem, "ending"))
		{
		alt_type = xmlGetProp (childElem, "type");
		alt_num = xmlGetProp (childElem, "number");
		}
  }
  
  if (alt_type)
	{
		if (!strcmp (alt_type, "start"))
			{
				if (!alt_num)
					alt_num = "1";
				g_string_append_printf (scripts[1], "(d-OpenNthTimeBar \"%s\")", alt_num);
			}			
		else	
			g_string_append (scripts[1], "(d-EndVolta)");
	} 
	
	
  if (repeat)
    {
      if ((!strcmp (repeat, "backward")))
        text = "(d-RepeatEnd)";
      else if ((!strcmp (repeat, "forward")))
        text = "(d-RepeatStart)";
      else if ((!strcmp (repeat, "forward-backward")))
        text = "(d-RepeatEndStart)";
    }
  else if (style)
    {
      if ((!strcmp (style, "light-light")))
        text = "(d-DoubleBarline)";
      else if ((!strcmp (style, "light-heavy")))
        text = "(d-ClosingBarline)";
    }
  if (text)g_string_append (scripts[1], text);
    //for (i = 0; i < numvoices; i++)
      //g_string_append (scripts[i + 1], text);
 g_free (style);
}



 /*          <direction placement="above">
    <direction-type>
    <wedge default-y="34" spread="0" type="crescendo"/>
    </direction-type>
    </direction>
  */

static gchar *
parse_direction_type (xmlNodePtr rootElem, GString * script, gchar *placement, gint current_voice)
{
  xmlNodePtr childElem;
  gchar *pending = NULL;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
	if (ELEM_NAME_EQ (childElem, "rehearsal"))
      g_string_append (script, "(d-RehearsalMark)");
	if (ELEM_NAME_EQ (childElem, "octave-shift"))
		{
			OttavaVals [current_voice] = 1;
			gchar *v = xmlGetProp (childElem, (xmlChar *) "size");
			if (v && !strcmp (v, "15")) OttavaVals [current_voice] = 2;
			v = xmlGetProp (childElem, (xmlChar *) "type");
			if (v && !strcmp (v, "stop"))
				OttavaVals [current_voice] = 0;
			else
		       if (v && !strcmp (v, "down"))
				OttavaVals [current_voice] *= -1;
			g_string_append_printf (script, "(d-Ottava %d)", OttavaVals [current_voice]);
		}
	  
	  
    if (ELEM_NAME_EQ (childElem, "wedge"))
      {
        gchar *type = xmlGetProp (childElem, (xmlChar *) "type");
        gchar *spread = xmlGetProp (childElem, (xmlChar *) "spread");
        if (type)

         {
            if (!strcmp (type, "crescendo"))
              g_string_append (awaiting_note, "(ToggleStartCrescendo)");
            if (!strcmp (type, "diminuendo"))
              g_string_append (awaiting_note, "(ToggleStartDiminuendo)");

            if (!strcmp (type, "stop"))
              {
                if (!spread)
                  g_string_append (script, "(ToggleEndDiminuendo)");
                else
                  g_string_append (script, "(ToggleEndCrescendo)");
              }
          }         
      }
     if (ELEM_NAME_EQ (childElem, "words"))
      {
          //FIXME get italic etc here xmlGetProp
          gchar *words = xmlNodeListGetString (childElem->doc, childElem->children, 1);
          //pending = g_strdup_printf ("(d-TextAnnotation \"%s\")(GoToMeasureEnd)", g_strescape(words, NULL));
          gchar *font_style = xmlGetProp (childElem, "font-style");
          if(font_style)
            {
                if(!strcmp(font_style, "italic"))
                    font_style = "\\\\italic";
                else
                    font_style = "";
            }
          else
            font_style = "";
          if(placement)
            {
            if(!strcmp(placement, "above"))
                placement = "^";
            else if(!strcmp(placement, "below"))
                placement = "_";
            else
                placement = "-";
            }
          else
            placement = "-";
        //if(pending==NULL)
         //   pending = "";
         gchar *thewords = escape_scheme (words);
        pending = g_strdup_printf ("(StandaloneText \"TextAnnotation\" \"%s\" \"%s\" \"%s\")(GoToMeasureEnd)", /*words g_strescape(words, NULL)*/ escape_scheme (words), placement, font_style);
        g_free (thewords);
        g_free (words);

      }


      /*
  <direction placement="below">
        <direction-type>
          <dynamics default-x="-21" default-y="-67" halign="center">
            <f/>
          </dynamics>
        </direction-type>
        <offset sound="yes">4</offset>
        <staff>2</staff>
        <sound dynamics="98"/>
      </direction>
   */
    if (ELEM_NAME_EQ (childElem, "dynamics"))
      {
        if(pending==NULL)
            pending = "";
        if (childElem->children)
			pending = g_strdup_printf ("%s(if (Appending?)(d-MoveCursorLeft))(d-DynamicText \"%s\")(GoToMeasureEnd)", pending, childElem->children->name);
      // g_string_append_printf (script, "(if (Appending?)(d-MoveCursorLeft))(d-DynamicText \"%s\")(GoToMeasureEnd)", childElem->children->name);
      }
  }
  return pending;
}

static gchar *
parse_direction (xmlNodePtr rootElem, GString * script, gchar *placement, gint current_voice)
{
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
    if (ELEM_NAME_EQ (childElem, "direction-type"))
      return parse_direction_type (childElem, script, placement, current_voice);



  }
  return NULL;
}

static void
get_staff_for_voice_measure (xmlNodePtr rootElem, gint * staff_for_voice)
{
  xmlNodePtr childElem;
  gint division = 0;
  gint current_voice = 1;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
    if (ELEM_NAME_EQ (childElem, "note"))
      {
        get_staff_for_voice_note (childElem, staff_for_voice);
      }
  }
}

static gchar *
parse_measure (xmlNodePtr rootElem, GString ** scripts, gint * staff_for_voice, gint * divisions, gint * voice_timings, gint numvoices, gint measurenum)
{
  GString *ret = g_string_new ("");
  gint note_count = 0;
  xmlNodePtr childElem;
  gint division = 0;
  gint current_voice = 1;
  gint actual_notes = 1, normal_notes = 1;      /* for tuplets */
  gint last_voice_with_notes = 1;       /* in case a voice with not "note" elements moves the current voice on while unfinished stuff in last voice */
  GString *pendings = g_string_new("");
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
    //g_debug("name %s at voicenumber %d at division %d\n", childElem->name, current_voice, division);
    if (ELEM_NAME_EQ (childElem, "attributes"))
      parse_attributes (childElem, scripts, numvoices, staff_for_voice, division, voice_timings, divisions, &current_voice, measurenum);

    if (ELEM_NAME_EQ (childElem, "backup"))
      {
        division -= parseDuration (&current_voice, childElem); // g_print("backward arrives at %d\n", division);
       // sets current_voice to the voice mentioned in the "backup" element???? are there any examples of this???? it would seem not.
       //if backup mentioned the voice we could set voice_timings[] for the backup, else what happens???
      }
    gint voicenum = current_voice;
    if (ELEM_NAME_EQ (childElem, "forward"))
      {
		  xmlNodePtr gchildElem;
		  
		  gint oldvoice = current_voice;
		  FOREACH_CHILD_ELEM (gchildElem, childElem)
			{
		     if (ELEM_NAME_EQ (gchildElem, "voice"))
				voicenum = getXMLIntChild (gchildElem);
			}
			if( voicenum != current_voice)
				g_warning ("Forward for voice that is not current");
			current_voice = voicenum;
			gint duration = parseDuration (&current_voice, childElem);
			get_rest_for_duration (scripts[voicenum], duration, *divisions);
			g_string_append (scripts[voicenum], "(d-MoveCursorLeft)(d-SetNonprinting)(d-MoveCursorRight)");
			voice_timings[voicenum -1] += duration;
			//current_voice = oldvoice;g_warning ("Reverted to voice %d from the forward one %d\n", current_voice, voicenum);
			division += duration;  //g_print("forward arrives at %d\n", division);
      }
    if (ELEM_NAME_EQ (childElem, "note"))
      {
        gchar *printing = xmlGetProp (childElem, "print-object");
        gboolean is_nonprinting = FALSE;
        if (printing && !strcmp (printing, "no"))
          is_nonprinting = TRUE;

        gchar *warning = parse_note (childElem, scripts, staff_for_voice, &division, *divisions, voice_timings, &current_voice, &actual_notes, &normal_notes, is_nonprinting);
			
        if(pendings->len)
            {
                g_string_prepend (pendings, "(d-MoveCursorLeft)");
                g_string_append (scripts[current_voice], pendings->str);
                g_string_assign(pendings, "");
            }
        note_count++;
        if (*warning)
          g_string_append_printf (ret, "%s at note number %d, ", warning, note_count);
        last_voice_with_notes = current_voice;
      }


    if (ELEM_NAME_EQ (childElem, "direction"))
      {                         //g_assert(current_voice>0);
        gchar *placement = xmlGetProp (childElem, "placement");
        gchar *text = parse_direction (childElem, scripts[current_voice], placement, current_voice);
        if(text)
            g_string_append(pendings, text);
      }
    if (ELEM_NAME_EQ (childElem, "barline"))
      {
        parse_barline (childElem, scripts, numvoices);
      }
  }
  //g_assert(last_voice_with_notes>0);
  if ((actual_notes != 1) || (normal_notes != 1))
    g_string_append_printf (scripts[last_voice_with_notes], "\n;measure end with tuplet still active in voice %d\n(IfNeededEndTuplet)", current_voice);//FIXME fires off on last tuplet end
  pending_tuplet_end = FALSE;  
  g_string_free(pendings, TRUE);
  return g_string_free (ret, FALSE);
}

static gchar *
parse_part (xmlNodePtr rootElem)
{
  GString *warnings = g_string_new ("");
  gint i, j;
  xmlNodePtr childElem;
  gint numstaffs = 1, numvoices = 1;
  gint divisions = 384;         //will be overriden anyway.
  
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
    gint maxstaffs = 1, maxvoices = 1;
    if (ELEM_NAME_EQ (childElem, "measure"))
      {
        get_numstaffs_in_measure (childElem, &maxstaffs, &maxvoices);
        if (maxstaffs > numstaffs)
          numstaffs = maxstaffs;
        if (maxvoices > numvoices)
          numvoices = maxvoices;
      }
  }
  g_info ("Number of staffs %d, voices %d\n", numstaffs, numvoices);
  gint *staff_for_voice = (gint *) g_malloc0 (numvoices * sizeof (gint));
  
  GString **scripts = (GString **) g_malloc0 ((1 + numvoices) * sizeof (GString *));
  for (i = 0; i <= numvoices; i++)
    scripts[i] = g_string_new ("\n");
  gint *voice_timings = (gint *) g_malloc0 (numvoices * sizeof (gint));
  OttavaVals = (gint *) g_malloc0 (numvoices * sizeof (gint));//when inside an ottava we must shift the octave value of each pitch in this voice

  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
    if (ELEM_NAME_EQ (childElem, "measure"))
      {
        get_staff_for_voice_measure (childElem, staff_for_voice);//fills in values for  staff_for_voice[], telling which voice goes on which staff
      }
  }
  for (i = 0; i < numvoices; i++)
    if (staff_for_voice[i] == 0)
      {
        g_info ("Voicenum %d was not actually used", i + 1);
        staff_for_voice[i] = 1; //if a voice was not actually used, assign it to the first staff
      }

  gint *numvoices_for_staff = (gint *) g_malloc0 (numstaffs * sizeof (gint));

  for (i = 0; i < numvoices; i++)
    {                           //g_assert(staff_for_voice[i]>0);
      numvoices_for_staff[staff_for_voice[i] - 1]++;
    }

/* create script to make enough staffs and voices, we are already in staff 1 voice 1 */
  g_string_append (scripts[0], "(d-PushPosition)");
  for (i = 0; i < numstaffs; i++)
    {
      //g_debug("Staff %d with %d voices\n", i, numvoices_for_staff[i]);
      if (i > 0)                /*already have first staff */
        g_string_append (scripts[0], "(d-AddAfter)");
      for (j = 1 /*already have first voice */ ; j < numvoices_for_staff[i]; j++)
        {
          //g_debug("Voice %d on Staff %d\n", j, i);
          g_string_append (scripts[0], "(d-AddAfter)(d-SetCurrentStaffAsVoice)");
        }
    }
  g_string_append (scripts[0], "(d-PopPosition)");

  g_string_append (scripts[0], "(d-PushPosition)");
  gint measure_count = 1;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {

    if (ELEM_NAME_EQ (childElem, "measure"))
      {
        gint maxduration = 0;
        memset (voice_timings, 0, numvoices * sizeof (gint));
        
       // HERE we could get property "number" if 0 it is a pickup measure, but instead we just detect unfilled first measure
       
       // <measure implicit="yes" means it's not really a new bar, delete it if the bars are being created, or move back to the end of the previous bar to continue that one
       
        gchar *implicit = xmlGetProp (childElem, (xmlChar *) "implicit");
        if ((measure_count>1) && implicit && (!strcmp (implicit, "yes")))
			g_string_append (scripts [0], "(if (LastMeasure?)\n(begin (d-PopPosition)(d-DeleteMeasure)(d-GoToEnd)(d-PushPosition))\n(d-MoveCursorLeft))\n");
        
        gchar *warning = parse_measure (childElem, scripts, staff_for_voice, &divisions, voice_timings, numvoices, measure_count);
        if (*warning)
          g_string_append_printf (warnings, "%s in bar %d.\n", warning, measure_count);
        for (i = 0; i < numvoices; i++)
          if (maxduration < voice_timings[i])
            maxduration = voice_timings[i];
        for (i = 0; i < numvoices; i++)
          {
            if (voice_timings[i] < maxduration)
              {
				  //g_print ("measure elem calls for invisible rests - ignored\n");
                //insert_invisible_rest (scripts[i + 1], maxduration - voice_timings[i], divisions);
              }
            g_string_append (scripts[0], scripts[i + 1]->str);
            g_string_assign (scripts[i + 1], "");
            g_string_append_printf (scripts[0], "%s\n;;;;;;;finished voice %d\n", "(d-MoveToStaffDown)", i + 1);
            voice_timings[i] = 0;
          }
        g_string_append_printf (scripts[0], "(d-PopPosition)(if (not (d-MoveToMeasureRight))(d-AddMeasure))(d-PushPosition)\n;;;;;;;;;End of measure %d\n ", measure_count);
        measure_count++;
      }
  }
  g_string_append (scripts[0], "(d-PopPosition)");

  for (i = 0; i < numvoices; i++)
    {

      g_string_append (scripts[0], "(d-MoveToBeginning)(d-MoveToStaffDown)");
    }
  g_string_append (scripts[0], "(d-AddAfter)(d-InitialKey \"C major\")");
  if (warnings->len)
    g_warning ("Parsing MusicXML gave these warnings:\n%s", warnings->str);
  g_string_free (warnings, TRUE);
  InitialVoiceNum += numvoices;

  g_free (numvoices_for_staff);
  g_free (staff_for_voice);
  g_free (voice_timings);
  return g_string_free (scripts[0], FALSE);
}

static void
parse_identification (xmlNodePtr rootElem, GString *script, gboolean use_book_titles)
{
  gchar *title = NULL;
  xmlNodePtr childElem;
   FOREACH_CHILD_ELEM (childElem, rootElem)
  {
    if (ELEM_NAME_EQ (childElem, "creator"))
        {
			gchar *type = (gchar *) xmlGetProp (childElem, (xmlChar *) "type");
			if (!strcmp (type, "composer"))
                {
					title = xmlNodeListGetString (childElem->doc, childElem->children, 1);
					if (use_book_titles)
						g_string_append_printf (script, "(d-BookComposer \"%s\")", title);
					else
						g_string_append_printf (script, "(SetField \"composer\" \"%s\")", title);
				}
        }
    if (ELEM_NAME_EQ (childElem, "rights"))
        {
            title = xmlNodeListGetString (childElem->doc, childElem->children, 1);
           if (use_book_titles)
            g_string_append_printf (script, "(d-BookCopyright \"%s\")", title);
           else 
			g_string_append_printf (script, "(SetField \"copyright\" \"%s\")", title);
        }
  }
  g_free (title);
}
/**
 * Try to find the element with the given name and namespace as an immediate
 * child of the given parent element.  If found, return the child element; if
 * not, return NULL.
 */
static xmlNodePtr
getXMLChild (xmlNodePtr parentElem, gchar * childElemName)
{
  xmlNodePtr childElem;

  FOREACH_CHILD_ELEM (childElem, parentElem) if (ELEM_NAME_EQ (childElem, childElemName))
    return childElem;

  return NULL;
}

static void replace_quotes (gchar *str)
{
	for (;*str;str++)
		if (*str == '\"')
			*str = '\'';
}
gint
mxmlinput (gchar * filename)
{
  GError *err = NULL;
  gint ret = 0;
  xmlDocPtr doc = NULL;
  xmlNsPtr ns;
  xmlNodePtr rootElem;
  gboolean title_set = FALSE;
  gboolean use_book_titles = (get_scoreheader_directive ("BookTitle") != NULL);//if the score being imported into has book titles use those otherwise simple titles
  gboolean spillover = Denemo.prefs.spillover;
  Denemo.prefs.spillover = FALSE;

  /* ignore blanks between nodes that appear as "text" */
  xmlKeepBlanksDefault (0);
  
  awaiting_note = g_string_new ("");
  /* Try to parse the file. */

  doc = xmlParseFile (filename);
  if (doc == NULL)
    {
      g_warning ("Could not read MusicXML file %s", filename);
      Denemo.prefs.spillover = spillover;
      return -1;
    }

  rootElem = xmlDocGetRootElement (doc);
  xmlNodePtr childElem;
  //to avoid ReStartTuplet spilling into the next bar a rehearsal mark is inserted and then removed after the tupend/start are inserted FIXME
  GString *script = g_string_new (";Score\n\
  (define spillover (d-GetBooleanPref \"spillover\"))\n\
  (define startmidiin (d-GetBooleanPref \"startmidiin\"))\n\
    (define (IfNeededEndTuplet)\n(d-MoveCursorLeft)(if (TupletOpen?) (d-DeleteObject) (d-MoveCursorRight)))\n\
    (define (ReStartTuplet tuptype)\n\
  (d-RehearsalMark)(d-MoveCursorLeft)(d-EndTuplet)(d-StartTuplet tuptype)(GoToMeasureEnd)(d-MoveCursorLeft)(d-DeleteObject))\
  (define (InsertNoteInChord n)\
	(d-MoveCursorLeft)(if (not (Note?))\
		(d-PrevNote))\
	(d-InsertNoteInChord n)\
	(GoToMeasureEnd))\
  (define (ToggleBeginSlur)\
	(d-MoveCursorLeft)(if (not (Note?))\
		(d-PrevNote))\
	(d-ToggleBeginSlur)\
	(GoToMeasureEnd))\
  (define (ToggleEndSlur)\
	(d-MoveCursorLeft)(if (not (Note?))\
		(d-PrevNote))\
	(d-ToggleEndSlur)\
	(GoToMeasureEnd))\
  (define (ToggleStaccato)\
	(d-MoveCursorLeft)(if (not (Note?))\
		(d-PrevNote))\
	(d-ToggleStaccato)\
	(GoToMeasureEnd))\
 (define (ToggleStaccatissimo)\
	(d-MoveCursorLeft)(if (not (Note?))\
		(d-PrevNote))\
	(d-ToggleStaccatissimo)\
	(GoToMeasureEnd))\
 (define (ToggleFermata)\
	(d-MoveCursorLeft)(if (not (Music?))\
		(d-PrevNote))\
	(d-ToggleFermata)\
	(GoToMeasureEnd))\
 (define (ToggleTrill)\
	(d-MoveCursorLeft)(if (not (Note?))\
		(d-PrevNote))\
	(d-ToggleTrill)\
	(GoToMeasureEnd))\
 (define (ToggleTurn)\
	(d-MoveCursorLeft)(if (not (Note?))\
		(d-PrevNote))\
	(d-ToggleTurn)\
	(GoToMeasureEnd))\
 (define (ToggleTie)\
	(d-MoveCursorLeft)(if (not (Note?))\
		(d-PrevNote))\
	(d-ToggleTie)\
	(GoToMeasureEnd))\
 (define (ToggleStartCrescendo)\
	(d-MoveCursorLeft)(if (not (Music?))\
		(d-PrevNote))\
	(d-ToggleStartCrescendo)\
	(GoToMeasureEnd))\
 (define (ToggleStartDiminuendo)\
	(d-MoveCursorLeft)(if (not (Music?))\
		(d-PrevNote))\
	(d-ToggleStartDiminuendo)\
	(GoToMeasureEnd))\
 (define (ToggleEndCrescendo)\
	(d-MoveCursorLeft)(if (not (Music?))\
		(d-PrevNote))\
	(d-ToggleEndCrescendo)\
	(GoToMeasureEnd))\
 (define (ToggleEndDiminuendo)\
	(d-MoveCursorLeft)(if (not (Music?))\
		(d-PrevNote))\
	(d-ToggleEndDiminuendo)\
	(GoToMeasureEnd))\n\
 (define (ToggleGrace)\
	(d-MoveCursorLeft)(if (not (Music?))\
		(d-PrevNote))\
	(d-ToggleGrace)\
	(GoToMeasureEnd))\n\
 (define (SetTitledPiece title)\n\
	(d-DirectivePut-movementcontrol-override \"TitledPiece\"  (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_GRAPHIC))\n\
	(d-DirectivePut-movementcontrol-prefix  \"TitledPiece\" (string-append \"\\\\titledPiece \\\\markup {\" title \"}\")))\n\
(define (RemoveEmptyTuplets)\n\
  (define (do-staff)\n\
      (d-MoveToBeginning)\n\
       (while (d-NextObject)\n\
       		(if (TupletOpen?)\n\
       			(begin\n\
       				(d-NextObject)\n\
       				(if (TupletClose?)\n\
       					(begin\n\
       						(d-DeleteObject)\n\
       						(if (Appending?)\n\
       							(d-MoveCursorLeft)\n\
       							(d-PrevObject))\n\
       						(d-DeleteObject)))))))\n\
    (while (d-StaffUp))\n\
    (do-staff)\n\
    (while (d-StaffDown)\n\
        (do-staff)))\n\
(define (CatchUnterminatedTuplets)\n\
  (define (do-staff)\n\
       (d-MoveToBeginning)\n\
	   (let do-measure ((count 0))\n\
		(let loop ()\n\
       		(if (TupletOpen?)\n\
       			(set! count (1+ count))\n\
       			(if (TupletClose?)\n\
					(set! count (1- count))))\n\
			(if (d-NextObjectInMeasure)\n\
				(loop)))\n\
		(d-MoveCursorRight)\n\
		(while (positive? count)\n\
				(d-EndTuplet)\n\
				(set! count (1- count)))\n\
		(if (d-MoveToMeasureRight)\n\
			(do-measure 0))))\n\
    (while (d-StaffUp))\n\
    (do-staff)\n\
    (while (d-StaffDown)\n\
        (do-staff)))\n\
 (define (SetField field title)\n\
		(define tag \"ScoreTitles\")\n\
		(define postfix #f)\n\
		(define data #f)\n\
		(set! postfix (d-DirectiveGet-scoreheader-postfix tag))\n\
		(set! data (d-DirectiveGet-scoreheader-data tag))\n\
		(if data\n\
			(set! data (eval-string data))\n\
			(set! data '()))\n\
		(if (not postfix)\n\
			(set! postfix \"\"))\n\
		(set! data (assq-set! data (string->symbol field) title))\n\
		(d-DirectivePut-scoreheader-postfix tag (string-append postfix \"\\n \"field\" = \\\\markup  { \" title \"}\\n\"))\n\
		(d-DirectivePut-scoreheader-data tag (format #f \"'~s\" data)))\n\
 (define (RemoveEmptyStaffs)\n\
			(define (do-staff)\n\
			   (d-MoveToBeginning)\n\
			   (let loop ()\n\
					(if (not (and (Music?) (not (Rest?))))\n\
						(if (d-NextObject)\n\
							(loop))))\n\
						(if (and (not (Music?)) (not (Rest?)) (not (d-NextObject)))\n\
						(begin\n\
							(d-DeleteStaff)\n\
							(d-StaffUp))))\n\
			(while (d-StaffUp))\n\
			(do-staff)\n\
			(while (d-StaffDown)\n\
				(do-staff)))\n\
(define (PadMeasures)\n\
	(define (fill-measure)\n\
		(while (and (not (EmptyMeasure?)) (not (Appending?))) (d-MoveCursorRight))\n\
		(let loop ()\n\
			(if (UnderfullMeasure?)\n\
				(let ((shortfall (duration::shortfall)))\n\
					(if shortfall\n\
						(let ((dot (equal? (string-ref shortfall (1- (string-length shortfall))) #\\.)))\n\
							(if dot\n\
								(set! shortfall (substring shortfall 0 (1- (string-length shortfall)))))\n\
							(eval-string (string-append \"(d-Insert\" shortfall \")\"))\n\
							(if dot (d-AddDot))\n\
							(if (not (d-GetNonprinting))\n\
								(d-StagedDelete))\n\
							(d-StagedDelete)\n\
							(d-MoveCursorRight)\n\
							(loop)))))))\n\
	(define (do-staff)\n\
      (d-MoveToBeginning)\n\
      (fill-measure)\n\
       (while (d-MeasureRight)\n\
       	(fill-measure)))\n\
    (d-GoToPosition #f 1 1 1 )\n\
    (do-staff)\n\
    (while (d-StaffDown)\n\
        (do-staff)))\n\
(define (assign-voices)\n\
	(d-MergeRests)\n\
	 (d-GoToPosition #f 1 1 1 )\n\
	 (let outer-loop ()\n\
		 (if (d-StaffDown)\n\
			 (let loop ()\n\
				(if (d-IsVoice)\n\
					(begin\n\
						(d-StaffUp)\n\
						(d-InitialVoiceOne)\n\
						(d-StaffDown)\n\
						(d-InitialVoiceFour)\n\
						(if (and (d-StaffDown)(d-IsVoice))\n\
							(begin\n\
								(d-InitialVoiceThree)\n\
								(if (and (d-StaffDown)(d-IsVoice))\n\
									(begin\n\
										(d-InitialVoiceFour)\n\
										(while (and (d-StaffDown) (d-IsVoice)))\n\
										(loop)))))))\n\
					(outer-loop)))))\n\
(d-MasterVolume 0) (d-IncreaseGuard) (d-StaffProperties \"denemo_name=voice 1\")\n");

  gint part_count = 1;
  InitialVoiceNum = 0;
  if (Warnings == NULL)
    Warnings = g_string_new ("");
  else
    g_string_assign (Warnings, "");
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
	    if (ELEM_NAME_EQ (childElem, "work"))
        {
			xmlNodePtr wtElem = getXMLChild (childElem, "work-title");
			if (wtElem)
				{
					gchar *title = xmlNodeListGetString (wtElem->doc, wtElem->children, 1);
					replace_quotes (title);
					if (use_book_titles)
						g_string_append_printf (script, "(d-BookTitle \"%s\")", escape_scheme(title));
					else
						g_string_append_printf (script, "(SetField \"title\" \"%s\")", escape_scheme(title));
					title_set = TRUE;

				}
		}
	  
       if (ELEM_NAME_EQ (childElem, "movement-title"))
        {
            gchar *title = xmlNodeListGetString (childElem->doc, childElem->children, 1);
            if(title)
				{
					replace_quotes (title);
					if (use_book_titles) 
						g_string_append_printf (script, "(SetTitledPiece \"%s\")", escape_scheme(title));
					else
						g_string_append_printf (script, "(SetField \"title\" \"%s\")", escape_scheme(title));
				}
            g_free (title);
        }
     if (ELEM_NAME_EQ (childElem, "identification"))   {
            parse_identification (childElem, script, use_book_titles);
    }
    if (ELEM_NAME_EQ (childElem, "part"))
      {
        g_string_append_printf (script, "\n;;-------------------------Part (ie Instrument) %d ----------------------\n", part_count++);
        g_string_append (script, parse_part (childElem));
      }
  }
  g_string_append (script, 
  "  (d-DeleteStaff)(d-MoveToEnd)(if (None?) (d-DeleteMeasureAllStaffs))\n\
  (d-SetPrefs \"<spillover>0</spillover>\")    \n\
  (d-SetPrefs \"<startmidiin>0</startmidiin>\")    \n\
(d-MoveToBeginning)(RemoveEmptyStaffs)(d-GoToPosition #f 1 1 1 )\n\
(let ()\n\
	(define (do-staff)\n\
		(let loop ()\n\
			(if (Clef?)\n\
				(begin\n\
					(d-InitialClef (d-GetPrevailingClef))\n\
					(d-DeleteObject))\n\
				(if (d-NextObjectInMeasure)\n\
					(loop)))))\n\
	(while (d-StaffUp))\n\
	(do-staff)\n\
	(while (d-StaffDown)\n\
		(do-staff)))\n\
(if (and (not (None?))(UnderfullMeasure?))(d-Upbeat))\n\
(PadMeasures)(RemoveEmptyTuplets)(CatchUnterminatedTuplets)(d-AmalgamateRepeatBarlines)\n\
(d-ConvertToWholeMeasureRests)(d-InstallGraceNoteHints)(assign-voices)(d-DecreaseGuard)\n\
  (d-SetPrefs (string-append \"<spillover>\" (if spillover \"1\" \"0\") \"</spillover>\"))    \n\
  (d-SetPrefs (string-append \"<startmidiin>\"(if startmidiin \"1\" \"0\") \"</startmidiin>\"))\n\
  (d-MasterVolume 1)\n\
  ");
#ifdef DEVELOPER
  {
    FILE *fp = fopen ("/home/rshann/junk.scm", "w");
    if (fp)
      {
        fprintf (fp, ";Script to create imported musicxml score\n %s", script->str);
        fclose (fp);
      }
  }
#endif
  call_out_to_guile (script->str);
  g_string_free (script, TRUE);
  Denemo.prefs.spillover = spillover;
  return ret;
}
