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
#include "prefops.h"            //for locatedotdenemo()
#include "file.h"

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

/**
 * Get the text from the child node list of elem, convert it to an integer,
 * and return it.  If unsuccessful, return G_MAXINT.
 */
static gint
getXMLIntChild (xmlNodePtr elem)
{
  gchar *text = (gchar *) xmlNodeListGetString (elem->doc, elem->xmlChildrenNode, 1);
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





#define INSERT_REST(num, den, rest) \
if(duration >= (num*divisions)/den)\
  {\
    g_string_append (script, "(d-InsertRest" rest ")(d-SetNonprinting)");\
    return insert_invisible_rest (script, duration - (num*divisions)/den, divisions);\
  } else 
  
static gint insert_invisible_rest (GString *script, gint duration, gint divisions)
{g_assert(divisions);
if(duration==0) return TRUE;
  //g_print("invis rest  %d, %d\n",  duration, divisions);
INSERT_REST(4, 1, "0")

INSERT_REST(2, 1, "1")

INSERT_REST(1, 1, "2")

INSERT_REST(1, 2, "3")

INSERT_REST(1, 4, "4")

INSERT_REST(1, 8, "5")

INSERT_REST(1, 16, "6")

INSERT_REST(1, 32, "7")

INSERT_REST(1, 64, "8")
g_warning ("Cannot cope with rest of %d/%d quarter notes\n", duration, divisions);
return FALSE;
}
#undef INSERT_REST



static gint parse_time(GString **scripts, gint numvoices, gint measurenum, xmlNodePtr rootElem)
{
  xmlNodePtr childElem;
  gint numerator = 0, denominator = 0;
  gint i;  
  FOREACH_CHILD_ELEM (childElem, rootElem)
          {
            
            if (ELEM_NAME_EQ (childElem, "beats"))
              numerator = getXMLIntChild(childElem);
            if (ELEM_NAME_EQ (childElem, "beat-type"))
              denominator = getXMLIntChild(childElem);
          }
if(numerator && denominator)
  for(i=0;i<numvoices;i++)
  if(measurenum==1)
    g_string_append_printf(scripts[i+1], "(d-InitialTimeSig \"%d/%d\")", numerator, denominator);
  else
    g_string_append_printf(scripts[i+1], "(d-InsertTimeSig \"%d/%d\")(if (not (Appending?))(d-MoveCursorRight))", numerator, denominator);
}

const gchar *get_clef(gint line, gchar *sign)
{
switch (line) {
  case 1:
   if(*sign =='G')
      return "French";
  case 2:
    if(*sign =='G')
      return "Treble";
  case 3:
    if(*sign =='C')
      return "Alto";
  case 4:
    if(*sign =='F')
      return "Bass";
    if(*sign =='C')
      return "Tenor";
  default:
    return "Treble";
}

}
static gint parse_key(GString **scripts, gint numvoices, gint measurenum, xmlNodePtr rootElem)
{
  xmlNodePtr childElem;
  gint fifths = 0;
  gchar *mode = NULL;
  gint i;
  FOREACH_CHILD_ELEM (childElem, rootElem)
          {
            
            if (ELEM_NAME_EQ (childElem, "fifths"))
              fifths = getXMLIntChild(childElem);
            if (ELEM_NAME_EQ (childElem, "mode"))
              mode = xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
          }
if(mode)
  for(i=0;i<numvoices;i++)
    if(measurenum==1)
      g_string_append_printf(scripts[i+1], "(d-InitialKey \"C major\")(d-IncrementKeysig %d)", fifths);
    else
      g_string_append_printf(scripts[i+1], "(d-InsertKey \"C major\")(d-IncrementKeysig %d)", fifths);
}
static gint parse_clef(GString **scripts, gint division, gint *voice_timings, gint voicenum, gint numvoices, gint *staff_for_voice, gint divisions, gint measurenum, xmlNodePtr rootElem)
{
  xmlNodePtr childElem;
  gint line = 0;
  gchar *sign = NULL;
  gchar *number = xmlGetProp (rootElem, (xmlChar *) "number");
  gint staffnum = 0;
  if(number)
    staffnum= atoi(number);
  if(staffnum==0)
    staffnum = 1;
  FOREACH_CHILD_ELEM (childElem, rootElem)
          {g_print("clef change %s \n", childElem->name);
            if (ELEM_NAME_EQ (childElem, "line"))
              line = getXMLIntChild(childElem);
            if (ELEM_NAME_EQ (childElem, "sign"))
              sign = xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
          }
  if(division > voice_timings[voicenum-1])
    {
      insert_invisible_rest(scripts[voicenum], division - voice_timings[voicenum-1], divisions);
      voice_timings[voicenum-1] = division;
    }
 if(sign) {
   gint i;
   const gchar *clef = get_clef(line, sign);
   for(i=0;i<numvoices;i++) {
    if(staff_for_voice[i]==staffnum)
      if(measurenum==1)
        g_string_append_printf(scripts[i+1], "(d-InitialClef \"%s\")", clef);
      else
        g_string_append_printf(scripts[i+1], "(d-InsertClef \"%s\")", clef);
   }
 }           
}

static const gchar *alteration (gint alter) {
  switch(alter) {
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
static const gchar *octave_string(gint octave) {
  
  gchar *octavation;
  switch (octave) {
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
static gchar *insert_note(gchar* type, gint octave, gchar* step, gint alter)
{
  if(step==NULL)
    {
      g_warning("Note without step\n");
      return g_strdup("");
    }
  gchar *duration_text="";
  if(!strcmp(type, "whole"))
  duration_text = "(d-Set0)"; else if(!strcmp(type, "half"))
  duration_text = "(d-Set1)"; else if(!strcmp(type, "quarter"))
  duration_text = "(d-Set2)"; else if(!strcmp(type, "eighth"))
  duration_text = "(d-Set3)"; else if(!strcmp(type, "16th"))
  duration_text = "(d-Set4)"; else if(!strcmp(type, "32nd"))
  duration_text = "(d-Set5)"; else if(!strcmp(type, "64th"))
  duration_text = "(d-Set6)"; else if(!strcmp(type, "128th"))
  duration_text = "(d-Set7)"; else if(!strcmp(type, "256th"))
  duration_text = "(d-Set8)"; else if(!strcmp(type, "breve"))
  duration_text = "(d-SetBreve)"; else if(!strcmp(type, "longa"))
  duration_text = "(d-SetLonga)"; else
  g_warning("Note duration %s not implemented\n", type);
  const gchar *octavation = octave_string(octave);
  gchar *put_text = g_strdup_printf("(d-InsertC)(d-PutNoteName \"%c%s%s\")", g_ascii_tolower(*step), alteration (alter), octavation);
  GString *ret = g_string_new(duration_text);
  g_string_append(ret, put_text);
  
  return g_string_free(ret, FALSE);
}

static gchar *add_note(gint octave, gchar *step, gint alter)
{// d-InsertNoteInChord lily (d-ShiftCursor is relative (d-MoveTo ????
  const gchar *octavation = octave_string(octave);
  gchar *text = g_strdup_printf ("(d-InsertNoteInChord \"%c%s%s\")", g_ascii_tolower(*step), alteration (alter), octavation);
  GString *ret = g_string_new (text);
  g_free(text);
  return g_string_free(ret, FALSE);
}

static void get_numstaffs_from_note(xmlNodePtr rootElem, gint *maxstaffs, gint *maxvoices)
{
 xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, rootElem)
    {
      if (ELEM_NAME_EQ (childElem, "staff")) {
        gint staffnum = getXMLIntChild (childElem);
        if(staffnum>*maxstaffs) *maxstaffs = staffnum;//g_print("staff num %d ...", staffnum);
      }
      if (ELEM_NAME_EQ (childElem, "voice")) {
        gint voicenum = getXMLIntChild (childElem);
        if(voicenum>*maxvoices) *maxvoices = voicenum;
      }
    }
   // g_print("So far %d %d\t", *maxstaffs, *maxvoices);
}
static void get_numstaffs_in_measure(xmlNodePtr rootElem, gint *maxstaffs, gint *maxvoices)
{
 xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, rootElem)
    {
      if (ELEM_NAME_EQ (childElem, "note")) {
          get_numstaffs_from_note(childElem,  maxstaffs, maxvoices);
      }
      if (ELEM_NAME_EQ (childElem, "backup")) {//should someone do backup without specifying a voice for it.
          if(*maxvoices == 1) *maxvoices = (*maxvoices) + 1;
      }
                      
    }

}


static gint parseDuration (gint *current_voice, xmlNodePtr rootElem)
{
  xmlNodePtr childElem;
  gint duration = 0;
  FOREACH_CHILD_ELEM (childElem, rootElem)
          {
            if (ELEM_NAME_EQ (childElem, "duration"))
                      duration = getXMLIntChild(childElem);

           if (ELEM_NAME_EQ (childElem, "voice"))
                      *current_voice = getXMLIntChild(childElem);
          }
return duration;
}



static gchar *add_rest (gchar *type)
{
  gchar *duration_text="";
  if(!strcmp(type, "whole"))
  duration_text = "(d-InsertRest0)"; else if(!strcmp(type, "half"))
  duration_text = "(d-InsertRest1)"; else if(!strcmp(type, "quarter"))
  duration_text = "(d-InsertRest2)"; else if(!strcmp(type, "eighth"))
  duration_text = "(d-InsertRest3)"; else if(!strcmp(type, "16th"))
  duration_text = "(d-InsertRest4)"; else if(!strcmp(type, "32nd"))
  duration_text = "(d-InsertRest5)"; else if(!strcmp(type, "64th"))
  duration_text = "(d-InsertRest6)"; else if(!strcmp(type, "128th"))
  duration_text = "(d-InsertRest7)"; else if(!strcmp(type, "256th"))
  duration_text = "(d-InsertRest8)"; else if(!strcmp(type, "breve"))
  duration_text = "(d-InsertBreveRest)"; else if(!strcmp(type, "longa"))
  duration_text = "(d-InsertLongaRest)"; else
  g_warning("Restduration %s not implemented\n", type);
  return g_strdup(duration_text);
}
static void modify_time(xmlNodePtr rootElem, gint *actual_notes, gint *normal_notes)
{
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, rootElem)
          {
            if (ELEM_NAME_EQ (childElem, "actual-notes"))
              *actual_notes = getXMLIntChild(childElem);
            if (ELEM_NAME_EQ (childElem, "normal-notes"))
              *normal_notes = getXMLIntChild(childElem);
          }
}

static gboolean  parse_end_tuplet(xmlNodePtr rootElem) {
 xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
    if (ELEM_NAME_EQ (childElem, "tuplet"))
    {
              gchar *type = xmlGetProp(childElem, (xmlChar *)"type");
             if(type)
              return !strcmp(type, "stop");
    }
  }
return FALSE;
}
static void parse_slur(xmlNodePtr rootElem, gint *is_slur_start, gint *is_slur_end)
{
 xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, rootElem)
  {
    if (ELEM_NAME_EQ (childElem, "slur"))
    {
      gchar *type = xmlGetProp(childElem, (xmlChar *)"type");
      
      if(type && (!strcmp(type, "start")))
        *is_slur_start = TRUE;
      if(type && (!strcmp(type, "stop")))
        *is_slur_end = TRUE;
      
    }
  }
}
static gchar *get_rest_for_duration(GString *ret, gint duration, gint divisions) {
  //g_print("Rest duration %d, divisions %d\n", duration, divisions);
  if(duration >= 4*divisions) {
    g_string_append(ret, "(d-InsertRest0)");
    duration -= 4*divisions;
    return get_rest_for_duration(ret, duration, divisions);
  } else if (duration >= 2*divisions) {
    g_string_append(ret, "(d-InsertRest1)");
    duration -= 2*divisions;
    return get_rest_for_duration(ret, duration, divisions);
  } else if (duration >= 1*divisions) {
    g_string_append(ret, "(d-InsertRest2)");
    duration -= 1*divisions;
    return get_rest_for_duration(ret, duration, divisions);
  } else if (2*duration >= divisions && (divisions/2)) {
    g_string_append(ret, "(d-InsertRest3)");
    duration -= divisions/2;
    return get_rest_for_duration(ret, duration, divisions);
  } else if (4*duration >= divisions && (divisions/4)) {
    g_string_append(ret, "(d-InsertRest4)");
    duration -= divisions/4;
    return get_rest_for_duration(ret, duration, divisions);
  } else if (8*duration >= divisions && (divisions/8)) {
    g_string_append(ret, "(d-InsertRest5)");
    duration -= divisions/8;
    return get_rest_for_duration(ret, duration, divisions);
  } else if (16*duration >= divisions && (divisions/16)) {
    g_string_append(ret, "(d-InsertRest6)");
    duration -= divisions/16;
    return get_rest_for_duration(ret, duration, divisions);
  } else if (32*duration >= divisions && (divisions/32)) {
    g_string_append(ret, "(d-InsertRest7)");
    duration -= divisions/32;
    return get_rest_for_duration(ret, duration, divisions);
  } else if (duration==0)
    return;

  g_string_append(ret, "\n;Duration of rest not recognized\n");
}
// *division is the current position of the tick counter from the start of the measure
static gint parse_note(xmlNodePtr rootElem, GString **scripts, gint *staff_for_voice,
  gint *division, gint divisions, gint *voice_timings, gint *current_voice, gint *actual_notes, gint *normal_notes, gboolean is_nonprinting)
{
  xmlNodePtr childElem;
  gint octave, alter = 0;
  gchar *step = NULL;
  gchar *type = NULL;
  gboolean in_chord = FALSE, is_dotted = FALSE, is_double_dotted = FALSE, is_rest = FALSE, is_grace = FALSE, is_tied = FALSE,
   end_tuplet = FALSE, is_slur_start = FALSE, is_slur_end = FALSE;
  gint voicenum = 1, staffnum = 1;
  gint duration = 0;
  gint initial_actual_notes = *actual_notes;
  gint initial_normal_notes = *normal_notes;
  gboolean timing_set = FALSE;//for case where one voice ends during a tuplet and the next one starts during a tuplet
  GString *text = g_string_new("");
  FOREACH_CHILD_ELEM (childElem, rootElem)
          {
             if (ELEM_NAME_EQ (childElem, "pitch"))
              {
                xmlNodePtr grandchildElem;
                FOREACH_CHILD_ELEM (grandchildElem, childElem) {
                  if (ELEM_NAME_EQ (grandchildElem, "step"))
                      step = xmlNodeListGetString (grandchildElem->doc, grandchildElem->xmlChildrenNode, 1);
                  if (ELEM_NAME_EQ (grandchildElem, "octave"))
                      octave = getXMLIntChild(grandchildElem);
                  if (ELEM_NAME_EQ (grandchildElem, "alter"))
                      alter = getXMLIntChild(grandchildElem);
                }
              }
              if (ELEM_NAME_EQ (childElem, "chord")) {
              in_chord = TRUE;
              }
              if (ELEM_NAME_EQ (childElem, "grace")) {
              is_grace = TRUE;
              }
              if (ELEM_NAME_EQ (childElem, "rest")) {
              is_rest = TRUE;//FIXME measure attribute set for whole measure rest - get this.
              }
              if (ELEM_NAME_EQ (childElem, "dot")) {
                if(is_dotted)
                  is_double_dotted = TRUE;
                is_dotted = TRUE;
              }

              if (ELEM_NAME_EQ (childElem, "tie")) {
                gchar *start = xmlGetProp (childElem, (xmlChar *) "type");
                  if(start && !strcmp("start", start))
                    is_tied = TRUE;
              }
              
              if (ELEM_NAME_EQ (childElem, "type"))
                type = xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
              if (ELEM_NAME_EQ (childElem, "duration")) {
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
              if (ELEM_NAME_EQ (childElem, "notations")) {
                end_tuplet = parse_end_tuplet(childElem);
                parse_slur(childElem, &is_slur_start, &is_slur_end);
              }
 
              if (ELEM_NAME_EQ (childElem, "time-modification")) {
                timing_set = TRUE;
                modify_time(childElem, actual_notes, normal_notes);
              }
          }

  if( staff_for_voice[voicenum-1] == 0)
            staff_for_voice[voicenum-1] = staffnum;
  if(*division > voice_timings[voicenum-1])
    {
            insert_invisible_rest(scripts[voicenum], *division - voice_timings[voicenum-1], divisions);
            voice_timings[voicenum-1] = *division;
    }
  if(type)
    {
      g_string_append(text, in_chord? add_note(octave, step, alter):(is_rest?add_rest(type):insert_note(type, octave, step, alter)));

      if(is_nonprinting)
        g_string_append(text, "(d-SetNonprinting)");
      if(!(in_chord || is_grace))
        voice_timings[voicenum-1] += duration;
      if(is_tied && !in_chord)
         g_string_append(text, "(d-ToggleTie)");
      if(is_grace)
        g_string_append(text, "(d-ToggleGrace)");
      if( (!in_chord) && is_dotted)
        g_string_append(text, "(d-AddDot)");
      if(is_double_dotted)
        g_string_append(text, "(d-AddDot)");

      if (staff_for_voice[voicenum-1] != staffnum)
        g_warning("Voice %d in staff %d need a staff change directive\n", voicenum, staffnum);
    } else  if(is_rest)
    {//for the case where a rest is given without a type, just a duration.
        get_rest_for_duration(text, duration, divisions);
        voice_timings[voicenum-1] += duration;
    }
   

  if(!timing_set)
   {
        *actual_notes = 1;
        *normal_notes = 1;
   }
  
  if (/* (!end_tuplet) && */ ((*current_voice != voicenum) && !(((initial_actual_notes)==1) && (initial_normal_notes==1))))
    {/* an unterminated tuplet in the last voice */
            g_string_append(scripts[*current_voice], "(d-EndTuplet)");
 
            initial_actual_notes = 1;
            initial_normal_notes = 1;
    }

  if (/*  (!end_tuplet) &&  */ ((initial_actual_notes != *actual_notes) || (initial_normal_notes != *normal_notes)))
    {gchar *str;
          if ((initial_actual_notes)==1 && (initial_normal_notes==1))
           //str = g_strdup_printf("(d-StartTuplet \"%d/%d\")", *normal_notes, *actual_notes);
           str = g_strdup_printf("\n;not end tuplet and entered with normal timings %d  \n(d-StartTuplet \"%d/%d\")", in_chord, *normal_notes, *actual_notes);
          else
            str = g_strdup("\n;Changed timings\n(d-EndTuplet)");
          g_string_append(scripts[voicenum], str);
          g_free(str);
    }

    
      
 g_string_append(scripts[voicenum], text->str);
 g_string_free(text, TRUE);

 /*      
    if(end_tuplet)
        {
         *actual_notes = 1;
         *normal_notes = 1;
         g_string_append(scripts[voicenum], "\n;end tuplet marked\n(d-EndTuplet)");// outside note loop because it can come before the chord is finished
         //WARNING this was *current_voice instead of voicenum but that is surely wrong???
        }
*/
 if((!in_chord) && is_slur_start)
    g_string_append(scripts[voicenum],"(d-ToggleBeginSlur)");
 if((!in_chord) && is_slur_end)
    g_string_append(scripts[voicenum],"(d-ToggleEndSlur)");

    
    if(!(in_chord || is_grace))
      *division = *division + duration;
    *current_voice = voicenum;       
}

static gint get_staff_for_voice_note(xmlNodePtr rootElem, gint *staff_for_voice)
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
          if( staff_for_voice[voicenum-1] == 0)
            staff_for_voice[voicenum-1] = staffnum;
}

static gint parse_attributes(xmlNodePtr rootElem, GString **scripts, gint numvoices, gint *staff_for_voice, gint division, gint *voice_timings, gint *divisions, gint *current_voice, gint measurenum)
{
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, rootElem)
          { //g_print("attribute %s at division %d\n", childElem->name, division);
            if (ELEM_NAME_EQ (childElem, "time"))
              parse_time(scripts, numvoices, measurenum, childElem);
            if (ELEM_NAME_EQ (childElem, "key"))
              parse_key(scripts, numvoices, measurenum, childElem);
            if (ELEM_NAME_EQ (childElem, "clef"))
              parse_clef(scripts, division, voice_timings, *current_voice, numvoices, staff_for_voice, *divisions, measurenum, childElem);
            if (ELEM_NAME_EQ (childElem, "divisions"))
              *divisions = getXMLIntChild(childElem);  
          }

}



static gint parse_barline(xmlNodePtr rootElem, GString **scripts, gint numvoices)
{
  xmlNodePtr childElem;
  gchar *text = NULL;
  gchar *style = NULL, *repeat = NULL;
  gint i;
  FOREACH_CHILD_ELEM (childElem, rootElem)
          { //g_print("attribute %s at division %d\n", childElem->name, division);
            if (ELEM_NAME_EQ (childElem, "bar-style"))
              style = xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
           if (ELEM_NAME_EQ (childElem, "repeat"))
              repeat = xmlGetProp (childElem, "direction");
          }
  if(repeat)
    {
    if ((!strcmp(repeat, "backward")))
      text = "(d-RepeatEnd)"; else
    if ((!strcmp(repeat, "forward")))
     text = "(d-RepeatStart)"; else
    if ((!strcmp(repeat, "forward-backward")))
      text = "(d-RepeatEndStart)"; 
    } else if (style)
    {
    if ((!strcmp(style, "light-light")))
      text = "(d-DoubleBarline)"; else
    if ((!strcmp(style, "light-heavy")))
      text = "(d-ClosingBarline)";
    }
  if(text)
  for(i=0;i<numvoices;i++)
      g_string_append(scripts[i+1], text);
}

static gint get_staff_for_voice_measure(xmlNodePtr rootElem, gint *staff_for_voice)
{
  xmlNodePtr childElem;
  gint division = 0;
  gint current_voice = 1;
  FOREACH_CHILD_ELEM (childElem, rootElem)
    {
      if (ELEM_NAME_EQ (childElem, "note")) {
          get_staff_for_voice_note(childElem, staff_for_voice);
      }                
    }
}

static gint parse_measure(xmlNodePtr rootElem, GString **scripts, gint *staff_for_voice, gint *divisions, gint *voice_timings, gint numvoices, gint measurenum)
{
  xmlNodePtr childElem;
  gint division = 0;
  gint current_voice = 1;
  gint actual_notes = 1, normal_notes = 1; /* for tuplets */
  gint last_voice_with_notes = 1;/* in case a voice with not "note" elements moves the current voice on while unfinished stuff in last voice */
  FOREACH_CHILD_ELEM (childElem, rootElem)
    {
      //g_print("name %s at voicenumber %d at division %d\n", childElem->name, current_voice, division);
      if (ELEM_NAME_EQ (childElem, "attributes"))
          parse_attributes(childElem, scripts, numvoices, staff_for_voice, division, voice_timings, divisions, &current_voice, measurenum);
          
      if (ELEM_NAME_EQ (childElem, "backup")) {
         division -= parseDuration(&current_voice, childElem);g_print("backward arrives at %d\n", division);
      }
      if (ELEM_NAME_EQ (childElem, "forward")) {
         division += parseDuration(&current_voice, childElem);g_print("forward arrives at %d\n", division);
      }
      if (ELEM_NAME_EQ (childElem, "note")) {
        gchar *printing = xmlGetProp (childElem, "print-object");
        gboolean is_nonprinting = FALSE;
        if(printing && !strcmp(printing, "no"))
          is_nonprinting = TRUE;
        parse_note(childElem, scripts, staff_for_voice, &division, *divisions, voice_timings, &current_voice, &actual_notes, &normal_notes, is_nonprinting);
        last_voice_with_notes = current_voice;
      }
      if (ELEM_NAME_EQ (childElem, "barline")) {
        parse_barline(childElem, scripts, numvoices);
      }              
    }
  if((actual_notes != 1) || (normal_notes != 1))
     g_string_append_printf(scripts[last_voice_with_notes], "\n;measure end with tuplet still active in voice %d\n(d-EndTuplet)", current_voice);
}
static gchar *parse_part(xmlNodePtr rootElem)
{
  gint i, j;
  xmlNodePtr childElem;
  gint numstaffs = 1, numvoices = 1;
  gint divisions = 384;//will be overriden anyway.

  FOREACH_CHILD_ELEM (childElem, rootElem)
          {
            gint maxstaffs=1, maxvoices=1;
            if (ELEM_NAME_EQ (childElem, "measure")) {
              get_numstaffs_in_measure(childElem, &maxstaffs, &maxvoices);
              if( maxstaffs > numstaffs)
                numstaffs = maxstaffs;
              if( maxvoices > numvoices)
                numvoices = maxvoices;
            }
          }
  g_print("Number of staffs %d, voices %d\n", numstaffs, numvoices);
  gint *staff_for_voice = (gint *)g_malloc0(numvoices*sizeof(gint));

  GString **scripts = (GString **)g_malloc0((1+numvoices)*sizeof(GString *));
  for(i=0;i<=numvoices;i++)
    scripts[i] = g_string_new("\n");
  gint *voice_timings = (gint *)g_malloc0(numvoices*sizeof(gint));


  FOREACH_CHILD_ELEM (childElem, rootElem)
          {
            if (ELEM_NAME_EQ (childElem, "measure"))
            {
              get_staff_for_voice_measure(childElem, staff_for_voice);
            }            
          }
  for(i=0;i<numvoices;i++)
   if(staff_for_voice[i]==0)
    {
      g_print("Voicenum %d was not actually used\n", i+1);
      staff_for_voice[i] = 1;//if a voice was not actually used, assign it to the first staff
    }

  gint *numvoices_for_staff = (gint *)g_malloc0(numstaffs*sizeof(gint)); 
 
  for(i=0;i<numvoices;i++)
    {
      numvoices_for_staff[staff_for_voice[i]-1]++;
    }

/* create script to make enough staffs and voices, we are already in staff 1 voice 1 */
  g_string_append(scripts[0], "(d-PushPosition)");
  for(i=0;i<numstaffs;i++)
    {
      //g_print("Staff %d with %d voices\n", i, numvoices_for_staff[i]);
      if(i>0)/*already have first staff*/
       g_string_append(scripts[0], "(d-AddAfter)");
      for(j=1/*already have first voice*/; j<numvoices_for_staff[i]; j++) {
        //g_print("Voice %d on Staff %d\n", j, i);
        g_string_append(scripts[0], "(d-AddAfter)(d-SetCurrentStaffAsVoice)");
      }
    }
  g_string_append(scripts[0], "(d-PopPosition)");

 g_string_append(scripts[0], "(d-PushPosition)");
 gint measure_count = 1;
 FOREACH_CHILD_ELEM (childElem, rootElem)
          {
            
            if (ELEM_NAME_EQ (childElem, "measure"))
            {
              gint maxduration=0;
              memset(voice_timings, 0, numvoices*sizeof(gint));
              parse_measure(childElem, scripts, staff_for_voice, &divisions, voice_timings, numvoices, measure_count);
              for (i=0;i<numvoices;i++)
                if(maxduration < voice_timings[i])
                  maxduration = voice_timings[i];
              for (i=0;i<numvoices;i++)
                {
                  if(voice_timings[i]<maxduration)
                    {
                     insert_invisible_rest (scripts[i+1], maxduration-voice_timings[i], divisions);   
                    }
                  g_string_append (scripts[0], scripts[i+1]->str);
                  g_string_assign(scripts[i+1], "");
                  g_string_append_printf(scripts[0],"%s\n;;;;;;;finished voice %d\n", "(d-MoveToStaffDown)", i+1);
                  voice_timings[i] = 0;
                }
              g_string_append_printf(scripts[0],
              "(d-PopPosition)(if (not (d-MoveToMeasureRight))(d-AddMeasure))(d-PushPosition)\n;;;;;;;;;End of measure %d\n ", measure_count);
              measure_count++;
            }
          }
 g_string_append(scripts[0], "(d-PopPosition)");

  for(i=0;i<numvoices;i++)
    {
      
      g_string_append(scripts[0], "(d-MoveToBeginning)(d-MoveToStaffDown)");
    }
  g_string_append(scripts[0], "(d-AddAfter)(d-InitialKey \"C major\")");
  return g_string_free(scripts[0], FALSE);
}

  
gint
mxmlinput (gchar * filename)
{
  GError *err = NULL;
  gint ret = 0;
  xmlDocPtr doc = NULL;
  xmlNsPtr ns;
  xmlNodePtr rootElem;
  /* ignore blanks between nodes that appear as "text" */
  xmlKeepBlanksDefault (0);
  /* Try to parse the file. */

  doc = xmlParseFile (filename);
  if (doc == NULL)
    {
      g_warning ("Could not read MusicXML file %s", filename);
      return -1;
    }

  rootElem = xmlDocGetRootElement (doc);
  xmlNodePtr childElem;
  GString *script = g_string_new(";Score\n\n(d-MasterVolume 0)");
  gint part_count = 1;
  FOREACH_CHILD_ELEM (childElem, rootElem)
          {
            if (ELEM_NAME_EQ (childElem, "part")) {
              g_string_append_printf(script, "\n;;-------------------------Part (ie Instrument) %d ----------------------\n", part_count++);
              g_string_append(script, parse_part(childElem));
            }
          }
 g_string_append(script, "(d-DeleteStaff)(d-MoveToEnd)(if (None?) (d-DeleteMeasureAllStaffs))(d-MasterVolume 1)(d-MoveToBeginning)(if (UnderfullMeasure?)(d-Upbeat))");
#ifndef DEVELOPER
 {FILE *fp = fopen("/home/rshann/junk.scm", "w");
    if(fp) {
      fprintf(fp, ";Parser not yet finished:\n %s", script->str);
      fclose(fp);
    }
  }
#endif
 call_out_to_guile (script->str);
 g_string_free(script, TRUE);
 return ret;
}
