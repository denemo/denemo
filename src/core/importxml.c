/* importxml.c
 * Import Denemo's "native" XML file format into a Denemo score structure
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2001-2005 Eric Galluzzo, Adam Tee
 * 2009, 2010, 2011 Richard Shann
 */

#include "command/chordops.h"
#include "command/graceops.h"
#include "core/importxml.h"
#include "command/measureops.h"
#include "command/objops.h"
#include "command/scoreops.h"
#include "command/staffops.h"
#include "command/processstaffname.h"
#include "command/tupletops.h"
#include "export/xmldefs.h"
#include "core/view.h"
#include "ui/texteditors.h"
#include "command/lilydirectives.h"
#include "display/calculatepositions.h"
#include "command/scorelayout.h"
#include "audio/pitchentry.h"
#include <string.h>


/* libxml includes: for libxml2 this should be <libxml.h> */
#include <libxml/parser.h>
#include <libxml/tree.h>

static gint version_number;


static gint current_movement = 0, current_staff = 0, current_measure = 0, current_position = 0, tonal_center = 0;

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
freeHashTableKey (gpointer key, G_GNUC_UNUSED gpointer value, G_GNUC_UNUSED gpointer userData)
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
  gchar *text = (gchar *) xmlNodeListGetString (elem->doc, elem->xmlChildrenNode, 1);
  gint num = G_MAXINT;
  if (text == NULL)
    {
      g_warning ("No child text found");
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


/**
 * Try to find the element with the given name and namespace as an immediate
 * child of the given parent element.  If found, return the child element; if
 * not, return NULL.
 */
static xmlNodePtr
getXMLChild (xmlNodePtr parentElem, gchar * childElemName, xmlNsPtr childNs)
{
  xmlNodePtr childElem;

  FOREACH_CHILD_ELEM (childElem, parentElem) if (ELEM_NAME_EQ (childElem, childElemName) && childElem->ns == childNs)
    return childElem;

  return NULL;
}


/**
 * Convert textual context to a denemo context
 *
 */
static void
add_staff_context (DenemoContext c)
{
  if (c == DENEMO_NONE)
    return;
  switch (c)
    {
    case DENEMO_PIANO_START:
      staff_directive_put_prefix ("ContextPianoStaff", " \\new PianoStaff <<\n");
      staff_directive_put_override ("ContextPianoStaff", DENEMO_OVERRIDE_AFFIX);
      break;
    case DENEMO_GROUP_START:
      staff_directive_put_prefix ("ContextGroupStaff", " \\new StaffGroup <<\n");
      staff_directive_put_override ("ContextGroupStaff", DENEMO_OVERRIDE_AFFIX);
      break;
    case DENEMO_CHOIR_START:
      staff_directive_put_prefix ("ContextChoirStaff", " \\new ChoirStaff <<\n");
      staff_directive_put_override ("ContextChoirStaff", DENEMO_OVERRIDE_AFFIX);
      break;
    case DENEMO_PIANO_END:
      staff_directive_put_postfix ("ContextPianoStaff", ">>\n");
      staff_directive_put_override ("ContextPianoStaff", DENEMO_OVERRIDE_AFFIX);
      break;
    case DENEMO_GROUP_END:
      staff_directive_put_postfix ("ContextGroupStaff", ">>\n");
      staff_directive_put_override ("ContextGroupStaff", DENEMO_OVERRIDE_AFFIX);
      break;
    case DENEMO_CHOIR_END:
      staff_directive_put_postfix ("ContextChoirStaff", ">>\n");
      staff_directive_put_override ("ContextChoirStaff", DENEMO_OVERRIDE_AFFIX);
      break;
    default:
      g_warning ("Unexpected context value");
    }


}

static void
addContext (gchar * string)
{
  if (string == NULL)
    return;
  if ((!strcmp (string, "PianoStaff")) || (!strcmp (string, "ChoirStaff")) || (!strcmp (string, "GroupStaff")))
    {
      g_warning ("Old context specs found - no longer supported. You will have to reset the Staff contexts");
      return;
    }
#define LOOKUP(A,B)  if (!strcmp (string, A)) {add_staff_context(B); return;}
  LOOKUP (PIANO_START_STRING, DENEMO_PIANO_START) LOOKUP (PIANO_END_STRING, DENEMO_PIANO_END) LOOKUP (CHOIR_START_STRING, DENEMO_CHOIR_START) LOOKUP (CHOIR_END_STRING, DENEMO_CHOIR_END) LOOKUP (GROUP_START_STRING, DENEMO_GROUP_START) LOOKUP (GROUP_END_STRING, DENEMO_GROUP_END)
#undef LOOKUP
}

#define UPDATE_OVERRIDE(directive)      \
  if(version_number<4)\
    directive->override |= DENEMO_OVERRIDE_TAGEDIT;


#define DO_DIREC(field) if (ELEM_NAME_EQ (childElem, #field))\
         directive->field = g_string_new((gchar *)xmlNodeListGetString (childElem->doc,\
                          childElem->xmlChildrenNode, 1));
#define DO_INTDIREC(field) if (ELEM_NAME_EQ (childElem, #field))\
         directive->field = getXMLIntChild(childElem);

static void
parseDirective (xmlNodePtr parentElem, DenemoDirective * directive)
{
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, parentElem)
  {
    DO_DIREC (tag);
    DO_DIREC (prefix);
    DO_DIREC (postfix);
    DO_DIREC (display);
    DO_DIREC (midibytes);
    DO_DIREC (data);
    DO_DIREC (grob);
    DO_INTDIREC (override);
    DO_INTDIREC (minpixels);
    DO_INTDIREC (x);
    DO_INTDIREC (y);
    DO_INTDIREC (tx);
    DO_INTDIREC (ty);
    DO_INTDIREC (gx);
    DO_INTDIREC (gy);

    if (ELEM_NAME_EQ (childElem, "graphic_name"))
      {
        directive->graphic_name = g_string_new ((gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1));
        loadGraphicItem (directive->graphic_name->str, (DenemoGraphic **) & directive->graphic);
        /* FIXME,handle not loaded */
      }
  }
  UPDATE_OVERRIDE (directive);

}

static gint
parseWidgetDirective (xmlNodePtr parentElem, gpointer fn, DenemoDirective * directive, GtkMenu * menu)
{
  xmlNodePtr childElem;


  FOREACH_CHILD_ELEM (childElem, parentElem)
  {
    DO_DIREC (tag);
    DO_DIREC (prefix);
    DO_DIREC (postfix);
    DO_DIREC (display);
    DO_DIREC (graphic_name);

    DO_DIREC (midibytes);
    DO_DIREC (grob);
    DO_DIREC (data);
    DO_INTDIREC (override);
    DO_INTDIREC (minpixels);
    DO_INTDIREC (x);
    DO_INTDIREC (y);
    DO_INTDIREC (tx);
    DO_INTDIREC (ty);
    DO_INTDIREC (gx);
    DO_INTDIREC (gy);
  }
  if (directive->tag == NULL)
    directive->tag = g_string_new ("<Unknown Tag>");
  if (directive->postfix && (g_str_has_prefix (directive->postfix->str, "tagline = \"Generated by Denemo Version")))    //drop old automated taglinesdirective->postfix->str
    g_string_assign (directive->postfix, "");
  UPDATE_OVERRIDE (directive);

  widget_for_directive_menu (directive, fn, menu);
  return TRUE;
}


#undef DO_DIREC
#undef DO_INTDIREC

static void
parseVerse (xmlNodePtr parentElem, GtkWidget * verse)
{
  gchar *text = (gchar *) xmlNodeListGetString (parentElem->doc, parentElem->xmlChildrenNode, 1);

  gtk_text_buffer_set_text (gtk_text_view_get_buffer ((GtkTextView *) verse), text, -1);
  //gtk_text_buffer_set_modified(gtk_text_view_get_buffer(verse), FALSE);
  g_signal_connect (G_OBJECT (gtk_text_view_get_buffer ((GtkTextView *) verse)), "changed", G_CALLBACK (lyric_change), NULL);
  g_free (text);
}

static void
parseVerses (DenemoMovement * si, DenemoStaff * staff, xmlNodePtr parentElem)
{
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, parentElem)
  {
    GtkWidget *verse = add_verse_to_staff (si, staff);
    parseVerse (childElem, verse);
  }
}

/* Fix for semantic change to prefix field in the directives */
static void
fix_prefix_use (GList * directives)
{
  GList *g;
  for (g = directives; g; g = g->next)
    {
      DenemoDirective *directive = g->data;
      if (directive->tag == NULL)
        directive->tag = g_string_new ("<Unknown Tag>");
      if (directive->prefix)
        {
          directive->prefix = g_string_new (g_strdup_printf ("%%{Disabled form \n%s\n use newer command %%}\n", directive->prefix->str));
          directive->display = g_string_new (g_strdup_printf ("Warning - re-run the %s command here!\nold version", directive->tag->str));
        }
    }
}

static GList *
parseDirectives (xmlNodePtr parentElem)
{
  GList *directives = NULL;
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, parentElem)
  {
    DenemoDirective *directive = (DenemoDirective *) g_malloc0 (sizeof (DenemoDirective));
    parseDirective (childElem, directive);
    directives = g_list_append (directives, directive);
  }
  return directives;
}

static GList *
parseWidgetDirectives (xmlNodePtr parentElem, gpointer fn, GtkMenu * menu, GList ** directives_pointer)
{
  GList *directives = NULL;
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, parentElem)
  {
    DenemoDirective *directive = (DenemoDirective *) g_malloc0 (sizeof (DenemoDirective));
    parseWidgetDirective (childElem, fn, directive, menu);
    directives = g_list_append (directives, directive);
    if (directive->widget && directives_pointer)
      g_object_set_data (G_OBJECT (directive->widget), "directives-pointer", (gpointer) directives_pointer);    //FIXME this const string has to match with lilydirectives.c
  }
  return directives;
}

/**
 * Return the numerator and denominator from the given XML fraction.
 */
static gint
parseFraction (xmlNodePtr parentElem, xmlNsPtr ns, gint * numerator, gint * denominator)
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
                g_warning ("Two numerators in the same fraction under <%s>", parentElem->name);
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
                g_warning ("Two numerators in the same fraction under <%s>", parentElem->name);
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
      g_warning ("Fraction's numerator not found inside <%s>; defaulting to 1", parentElem->name);
      *numerator = 1;
    }

  if (!gotDenominator)
    {
      g_warning ("Fraction's denominator not found inside <%s>; defaulting to " "1", parentElem->name);
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
      g_warning ("Unknown accidental \"%s\"; defaulting to natural", accidentalName);
      return 0;
    }
}


/**
 * Parse the given <clef> element into a numeric clef type.
 */
static void
parseClef (xmlNodePtr clefElem, clef * clef)
{
  gchar *clefTypeName = (gchar *) xmlGetProp (clefElem, (xmlChar *) "name");
  if (clefTypeName == NULL)
    {
      g_warning ("No clef name specified; defaulting to treble");
      clef->type = DENEMO_TREBLE_CLEF;
    }
  else if (strcmp (clefTypeName, "treble") == 0)
    clef->type = DENEMO_TREBLE_CLEF;
  else if (strcmp (clefTypeName, "bass") == 0)
    clef->type = DENEMO_BASS_CLEF;
  else if (strcmp (clefTypeName, "alto") == 0)
    clef->type = DENEMO_ALTO_CLEF;
  else if (strcmp (clefTypeName, "treble-8vb") == 0)
    clef->type = DENEMO_G_8_CLEF;
  else if (strcmp (clefTypeName, "bass-8vb") == 0)
    clef->type = DENEMO_F_8_CLEF;
  else if (strcmp (clefTypeName, "tenor") == 0)
    clef->type = DENEMO_TENOR_CLEF;
  else if (strcmp (clefTypeName, "soprano") == 0)
    clef->type = DENEMO_SOPRANO_CLEF;
  else if (strcmp (clefTypeName, "french") == 0)
    clef->type = DENEMO_FRENCH_CLEF;
  else
    {
      g_warning ("Unknown clef type \"%s\"; defaulting to treble", clefTypeName);
      clef->type = DENEMO_TREBLE_CLEF;
    }
  g_free (clefTypeName);
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, clefElem)
  {
    if (ELEM_NAME_EQ (childElem, "directives"))
      {
        clef->directives = parseDirectives (childElem);
      }
  }
}


/**
 * Parse the given <key-signature> element into a key signature (with the
 * given number of sharps plus whether it's minor or not).
 */
static void
parseKeySignature (xmlNodePtr keySigElem, keysig * keysig)
{
  gint *keySig = &keysig->number;
  gboolean *isMinor = &keysig->isminor;
  xmlNodePtr childElem;
  gboolean successful = FALSE;
  gchar *noteName, *accidentalName, *modeName;
  gint note, accidental;

  FOREACH_CHILD_ELEM (childElem, keySigElem)
  {
    if (ELEM_NAME_EQ (childElem, "modal-key-signature"))
      {
        noteName = (gchar *) xmlGetProp (childElem, (xmlChar *) "note-name");
        accidentalName = (gchar *) xmlGetProp (childElem, (xmlChar *) "accidental");
        modeName = (gchar *) xmlGetProp (childElem, (xmlChar *) "mode");

        if (noteName == NULL)
          {
            g_warning ("<modal-key-signature> should have a " "note-name attribute; defaulting to C");
            noteName = "C";
          }
        if (accidentalName == NULL)
          accidentalName = "natural";
        if (modeName == NULL)
          {
            g_warning ("<modal-key-signature> should have a " "mode attribute; defaulting to major");
            modeName = "major";
          }

        /* Translate note name (A to G) into note number (0 to 6). */

        note = noteName[0] - 'A';
        if (strlen (noteName) != 1 || note < 0 || note > 6)
          {
            g_warning ("<modal-key-signature> note name should be A " "through G, received \"%s\"; defaulting to C", noteName);
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
            g_warning ("Unknown key signature with note name %s and " "accidental %s; defaulting to C", noteName, accidentalName);
            *keySig = 0;
          }

        /* Determine whether it's major or minor. */

        if (strcmp (modeName, "major") == 0)
          *isMinor = FALSE;
        else if (strcmp (modeName, "minor") == 0)
          *isMinor = TRUE;
        else
          {
            g_warning ("Unknown mode %s; defaulting to major", modeName);
            *isMinor = FALSE;
          }

        successful = TRUE;

        g_free (noteName);
        //g_free (accidentalName);
        g_free (modeName);
      }
    if (ELEM_NAME_EQ (childElem, "directives"))
      {
        keysig->directives = parseDirectives (childElem);
      }
  }
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
parseTimeSignature (xmlNodePtr timeSigElem, xmlNsPtr ns, timesig * timesig)
{
  xmlNodePtr childElem;
  gboolean successful = FALSE;
  gint *numerator = &timesig->time1;
  gint *denominator = &timesig->time2;

  FOREACH_CHILD_ELEM (childElem, timeSigElem)
  {

    if (ELEM_NAME_EQ (childElem, "simple-time-signature"))
      {
        if (parseFraction (childElem, ns, numerator, denominator) != 0)
          g_warning ("Could not parse <simple-time-signature>; " "defaulting to 4/4");
        else
          successful = TRUE;
      }

    if (ELEM_NAME_EQ (childElem, "directives"))
      {
        timesig->directives = parseDirectives (childElem);
      }
    /*
     * Note: We can ignore other namespaces because the generic "parse this
     *       DenemoObject" code takes care of them for us.
     */
  }

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
static GString *Lyric = NULL;

static void
parseLyric (xmlNodePtr lyricElem)
{
  gchar *lyric = (gchar *) xmlNodeListGetString (lyricElem->doc,
                                                 lyricElem->xmlChildrenNode,
                                                 1);
  if (lyric)
    Lyric = g_string_append (Lyric, lyric);
  Lyric = g_string_append (Lyric, " ");
  g_free (lyric);
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
  if (fakechord)
    {
      ((chord *) curobj->object)->is_fakechord = TRUE;
      ((chord *) curobj->object)->fakechord = g_string_new (fakechord);
    }
  g_free (fakechord);
}




/**
 * Parse the given <note> element into a note structure and add it to the
 * given chord.
 */
static void
parseNote (xmlNodePtr noteElem, xmlNsPtr ns, DenemoObject * chordObj, gint currentClef)
{
  xmlNodePtr childElem;
  gint middleCOffset = 0, accidental = 0, noteHeadType = DENEMO_NORMAL_NOTEHEAD;
  gchar *accidentalName, *showAccidentalProp, *noteHeadName;
  GList *directives = NULL;

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
            accidentalName = (gchar *) xmlGetProp (childElem, (xmlChar *) "name");
            if (accidentalName == NULL)
              {
                g_warning ("<accidental> had no name attribute; defaulting " "to natural");
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
                g_warning ("Unknown accidental name \"%s\"; defaulting to " "natural", accidentalName);
                accidental = 0;
              }
            g_free (accidentalName);

            showAccidentalProp = (gchar *) xmlGetProp (childElem, (xmlChar *) "show");
            if (showAccidentalProp != NULL)
              {
                if (strcmp (showAccidentalProp, "true") != 0 && strcmp (showAccidentalProp, "false") != 0)
                  {
                    g_warning ("Unknown show accidental attribute value " "\"%s\" (should be true or false); " "defaulting to false", showAccidentalProp);
                  }
                g_free (showAccidentalProp);
              }
          }
        else if (ELEM_NAME_EQ (childElem, "directives"))
          {
            directives = parseDirectives (childElem);
          }
        else if (ELEM_NAME_EQ (childElem, "note-head"))
          {
            noteHeadName = (gchar *) xmlGetProp (childElem, (xmlChar *) "type");
            if (noteHeadName == NULL)
              {
                g_warning ("<note-head> element had no type attribute; " "defaulting to normal");
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
                g_warning ("Unknown notehead type \"%s\"; defaulting to " "normal", noteHeadName);
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
  newnote->directives = directives;

  if (noteHeadType != DENEMO_NORMAL_NOTEHEAD)
    {
      /* FIXME: Is this the right note in the chord? */
      ((note *) ((chord *) chordObj->object)->notes->data)->noteheadtype = (enum headtype) noteHeadType;
    }
}


/**
 * Parse the given <chord> or <rest>'s duration only, not any other
 * subelements, and return a simple DenemoObject corresponding to that chord
 * or rest.
 */
static DenemoObject *
parseBaseChord (xmlNodePtr chordElem, xmlNsPtr ns, DenemoMovement * si)
{
  xmlNodePtr durationElem, dotsElem;
  xmlNodePtr childElem;
  gboolean successful = FALSE;
  gchar *durationType;
  gint baseDuration = 2, numDots = 0;

  gchar *showProp = (gchar *) xmlGetProp (chordElem, (xmlChar *) "show");
  gboolean show = TRUE;
  if (showProp != NULL)
    {
      if (strcmp (showProp, "true") == 0)
        show = TRUE;
      else if (strcmp (showProp, "false") == 0)
        show = FALSE;
      else
        {
          g_warning ("Invalid value for show attribute of <rest> or <chord>: \"%s\"; " "defaulting to false", showProp);
          show = FALSE;
        }
    }
  gchar *grace_type = (gchar*) xmlGetProp (chordElem, (xmlChar *) "grace");
  gint grace = (grace_type ? (strcmp (grace_type, "true") ? ACCIACCATURA : GRACED_NOTE) : 0);   //we only store this for grace notes
  g_free (grace_type);

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
          g_warning ("No base attribute found in chord duration; defaulting " "to quarter note");
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
          else if (strcmp (durationType, "one-hundred-twenty-eighth") == 0)
            baseDuration = 7;
          else
            {
              g_warning ("Unknown base duration type \"%s\"; defaulting to " "quarter note", durationType);
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
  DenemoObject *chordObj = show ? newchord (baseDuration, numDots, 0) : hidechord (newchord (baseDuration, numDots, 0));

  ((chord *) chordObj->object)->is_grace = grace;


  FOREACH_CHILD_ELEM (childElem, chordElem)
  {
    if (childElem->ns == ns)
      {
        if (ELEM_NAME_EQ (childElem, "directives"))
          {
            ((chord *) chordObj->object)->directives = parseDirectives (childElem);
            if (version_number < 3)
              fix_prefix_use (((chord *) chordObj->object)->directives);
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
      }
  }



  return chordObj;
}


/**
 * Parse the given <rest> element and return a chord-type DenemoObject.
 */
static DenemoObject *
parseRest (xmlNodePtr restElem, xmlNsPtr ns, DenemoMovement * si)
{
  DenemoObject *chordObj;
  xmlNodePtr childElem;

  chordObj = parseBaseChord (restElem, ns, si);
  FOREACH_CHILD_ELEM (childElem, restElem)
  {
    if (ELEM_NAME_EQ (childElem, "ticks"))
      {
        chordObj->basic_durinticks = getXMLIntChild (childElem);
        ((chord *) chordObj->object)->baseduration = -chordObj->basic_durinticks;
      }
  }
  return chordObj;
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
      ((chord *) curobj->object)->dynamics = g_list_append (((chord *) curobj->object)->dynamics, dynString);
    }
  g_free (dynamicName);

}


static GdkPixbuf *
parseSource (xmlNodePtr parentElem)
{
  GError *error = NULL;
  gchar *cdata = (gchar*) xmlNodeListGetString (parentElem->doc, parentElem->xmlChildrenNode, 1);
  gsize len;
  guchar *buf = g_base64_decode (cdata, &len);
// xml free(cdata);
  GInputStream *is = g_memory_input_stream_new_from_data (buf, len, NULL);
  GdkPixbuf *pb = gdk_pixbuf_new_from_stream (is, NULL, &error);
  g_free (buf);
  return pb;
}

/**
 * Parse the given sources element.
 * 
 * @param chordElem the XML node to process
 * @param sources the GList* to populate  */
static GList *
parseSources (xmlNodePtr parentElem)
{
  GList *sources = NULL;
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, parentElem) sources = g_list_append (sources, parseSource (childElem));
  return sources;
}

static void
parseAudio (xmlNodePtr parentElem, DenemoMovement * si)
{
  xmlNodePtr childElem;

  FOREACH_CHILD_ELEM (childElem, parentElem)
  {
    if (ELEM_NAME_EQ (childElem, "filename"))
      si->recording->filename = g_strdup ((gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1));
    if (ELEM_NAME_EQ (childElem, "lead-in"))
      si->recording->leadin = getXMLIntChild (childElem);
  }
}

/**
 * Parse the given <chord> element and return a chord-type DenemoObject.
 * 
 * @param chordElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoMovement to populate 
 * @param slurEndChordElems pointer to a slur end element
 * @param crescEndChordElems pointer to a crescendo end element
 * @param diminEndChordElems pointer to a diminuendo end element
 * @param noteElem a pointer to a note XML element
 * @return the new DenemoObject
 */
static DenemoObject *
parseChord (xmlNodePtr chordElem, xmlNsPtr ns, DenemoMovement * si, gint currentClef, GList ** slurEndChordElems, GList ** crescEndChordElems, GList ** diminEndChordElems, xmlNodePtr * notesElem)
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
      *slurEndChordElems = g_list_remove_link (*slurEndChordElems, thisSlurListNode);
      g_list_free_1 (thisSlurListNode);
    }

  /* Check to see if this is the end of a crescendo. */

  thisCrescListNode = g_list_find (*crescEndChordElems, chordElem);
  if (thisCrescListNode != NULL)
    {
      ((chord *) chordObj->object)->crescendo_end_p = TRUE;
      *crescEndChordElems = g_list_remove_link (*crescEndChordElems, thisCrescListNode);
      g_list_free_1 (thisCrescListNode);
    }

  /* Check to see if this is the end of a diminuendo. */

  thisDiminListNode = g_list_find (*diminEndChordElems, chordElem);
  if (thisDiminListNode != NULL)
    {
      ((chord *) chordObj->object)->diminuendo_end_p = TRUE;
      *diminEndChordElems = g_list_remove_link (*diminEndChordElems, thisDiminListNode);
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
        else if (ELEM_NAME_EQ (childElem, "ticks"))
          {
            chordObj->basic_durinticks = getXMLIntChild (childElem);
            ((chord *) chordObj->object)->baseduration = -chordObj->basic_durinticks;
          }
        else if (ELEM_NAME_EQ (childElem, "dynamic"))
          {
            parseDynamic (childElem, chordObj);
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
              if (grandchildElem->ns == ns && ELEM_NAME_EQ (grandchildElem, "slur"))
                {
                  /*
                   * Prepend the slur's end element to the list that we're
                   * keeping.
                   */

                  slurEndXMLID = (gchar *) xmlGetProp (grandchildElem, (xmlChar *) "to");
                  if (slurEndXMLID == NULL)
                    {
                      g_warning ("No \"to\" attribute on <slur> element; " "ignoring");
                    }
                  else
                    {
                      slurEndElem = lookupXMLID (slurEndXMLID);
                      if (slurEndElem == NULL)
                        {
                          g_warning ("Chord is slurred to nonexistent " "chord with ID \"%s\"", slurEndXMLID);
                        }
                      else if (!ELEM_NAME_EQ (slurEndElem, "chord"))
                        {
                          g_warning ("Chord is slurred to non-chord " "element with tag name \"%s\" and ID " "\"%s\"", slurEndElem->name, slurEndXMLID);
                        }
                      else if (slurEndElem == chordElem)
                        {
                          g_warning ("Can't slur a chord to itself");
                        }
                      else
                        {
                          ((chord *) chordObj->object)->slur_begin_p = TRUE;
                          *slurEndChordElems = g_list_prepend (*slurEndChordElems, slurEndElem);
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
              if (grandchildElem->ns == ns && ELEM_NAME_EQ (grandchildElem, "crescendo"))
                {
                  /*
                   * Prepend the crescendo's end element to the list that we're
                   * keeping.
                   */

                  crescEndXMLID = (gchar *) xmlGetProp (grandchildElem, (xmlChar *) "to");
                  if (crescEndXMLID == NULL)
                    {
                      g_warning ("No \"to\" attribute on <crescendo> element; " "ignoring");
                    }
                  else
                    {
                      crescEndElem = lookupXMLID (crescEndXMLID);
                      if (crescEndElem == NULL)
                        {
                          g_warning ("Chord has crescendo to nonexistent " "chord with ID \"%s\"", crescEndXMLID);
                        }
                      else if (!ELEM_NAME_EQ (crescEndElem, "chord"))
                        {
                          g_warning ("Chord has crescendo to non-chord " "element with tag name \"%s\" and ID " "\"%s\"", crescEndElem->name, crescEndXMLID);
                        }
                      else if (crescEndElem == chordElem)
                        {
                          g_warning ("Can't crescendo on one chord");
                        }
                      else
                        {
                          ((chord *) chordObj->object)->crescendo_begin_p = TRUE;
                          *crescEndChordElems = g_list_prepend (*crescEndChordElems, crescEndElem);
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
              if (grandchildElem->ns == ns && ELEM_NAME_EQ (grandchildElem, "diminuendo"))
                {
                  /*
                   * Prepend the diminuendo's end element to the list that we're
                   * keeping.
                   */

                  diminEndXMLID = (gchar *) xmlGetProp (grandchildElem, (xmlChar *) "to");
                  if (diminEndXMLID == NULL)
                    {
                      g_warning ("No \"to\" attribute on <diminuendo> element; " "ignoring");
                    }
                  else
                    {
                      diminEndElem = lookupXMLID (diminEndXMLID);
                      if (diminEndElem == NULL)
                        {
                          g_warning ("Chord has diminuendo to nonexistent " "chord with ID \"%s\"", diminEndXMLID);
                        }
                      else if (!ELEM_NAME_EQ (diminEndElem, "chord"))
                        {
                          g_warning ("Chord has diminuendo to non-chord " "element with tag name \"%s\" and ID " "\"%s\"", diminEndElem->name, diminEndXMLID);
                        }
                      else if (diminEndElem == chordElem)
                        {
                          g_warning ("Can't diminuendo on one chord");
                        }
                      else
                        {
                          ((chord *) chordObj->object)->diminuendo_begin_p = TRUE;
                          *diminEndChordElems = g_list_prepend (*diminEndChordElems, diminEndElem);
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
              if (grandchildElem->ns == ns && ELEM_NAME_EQ (grandchildElem, "note"))
                {
                  parseNote (grandchildElem, ns, chordObj, currentClef);
                }
              else
                {
                  ILLEGAL_ELEM ("notes", grandchildElem);
                }
            }
          }
        else if (ELEM_NAME_EQ (childElem, "lyric"))
          {
            parseLyric (childElem);
          }


        //Support for old files 0.8.2 only
#define DO_DIREC(field) else if (ELEM_NAME_EQ (childElem, #field))\
      {\
        if( ((chord *) chordObj->object)->directives == NULL) {\
          DenemoDirective *directive =  (DenemoDirective*)g_malloc0(sizeof(DenemoDirective));\
      ((chord *) chordObj->object)->directives = g_list_append(NULL, directive) ;\
      }\
        ((DenemoDirective*) (((chord *) chordObj->object)->directives)->data)->field = \
         g_string_new((gchar *)xmlNodeListGetString (childElem->doc,\
         childElem->xmlChildrenNode, 1));\
      }
        DO_DIREC (prefix) DO_DIREC (postfix) DO_DIREC (display) DO_DIREC (midibytes)
#undef DO_DIREC
          // End 0.8.2 support
          else if (ELEM_NAME_EQ (childElem, "chordize"))
          {
            ((chord *) chordObj->object)->chordize = TRUE;
          }

        else if (ELEM_NAME_EQ (childElem, "directives"))
          {
            ;                   //done in base
          }
        else if (ELEM_NAME_EQ (childElem, "figure"))
          {
            ;                   //done in base
          }
        else if (ELEM_NAME_EQ (childElem, "fakechord"))
          {
            ;                   //done in base
          }
        else
          {
            ILLEGAL_ELEM ("chord", childElem);
          }
      }                         /* end if childElem->ns == ns */

    /*
     * No need to handle other namespaces here, the generic DenemoObject
     * code will do it for us.
     */

  }                             /* end for each childElem in scoreElem */

  return chordObj;
}

/*
 * Parse the given <lily:directive> element into a DenemoObject.
 */
static DenemoObject *
parseLilyDir (xmlNodePtr LilyDirectiveElem)
{
  gchar *directive = (gchar *) xmlNodeListGetString (LilyDirectiveElem->doc,
                                                     LilyDirectiveElem->xmlChildrenNode,
                                                     1);

  DenemoObject *curobj = lily_directive_new (directive ? directive : " ");
  DenemoDirective *thedirective = (lilydirective *) curobj->object;

  gchar *locked = (gchar *) xmlGetProp (LilyDirectiveElem, (xmlChar *) "locked");

  if (locked)
    thedirective->locked = !strcmp (locked, "true");
  g_free (locked);

#define GET_STR_FIELD(display)\
  gchar *display = (gchar *) xmlGetProp (LilyDirectiveElem, (xmlChar *) #display);\
  if(display)\
    ((lilydirective*)curobj->object)->display = g_string_new(display);\
  g_free(display);

  GET_STR_FIELD (tag);
  GET_STR_FIELD (display);
  GET_STR_FIELD (midibytes);
  GET_STR_FIELD (grob);
  GET_STR_FIELD (data);
  GET_STR_FIELD (graphic_name);
  GET_STR_FIELD (prefix);
  if (thedirective->graphic_name && thedirective->graphic_name->len)
    loadGraphicItem (thedirective->graphic_name->str, (DenemoGraphic **) & thedirective->graphic);
    
   if(version_number < 7) {
       GString *lily = ((lilydirective*)curobj->object)->postfix;
       //convert old style barlines to LilyPond 2.18 style
       if(lily)
       {
        gchar *postfix = lily->str;
        if (!g_strcmp0 ("\\bar \":|\"", postfix))
            g_string_assign(lily, "\\bar \":|.\"");
        else if (!g_strcmp0 (" \\bar \"|:\"", postfix))//Note there was a space in the old directive before \bar!
            g_string_assign(lily, "\\bar \".|:\"");
        else if (!g_strcmp0 (" \\bar \":|:\"", postfix)) //Note there was a space in the old directive!
            g_string_assign(lily, "\\bar \":..:\"");
        
       }
   } 
    
    
#define GET_INT_FIELD(x)\
  gchar *x = (gchar *) xmlGetProp (LilyDirectiveElem, (xmlChar *) #x);\
  if(x)\
    thedirective->x = atoi(x);\
  g_free(x);

  GET_INT_FIELD (x);
  GET_INT_FIELD (y);
  GET_INT_FIELD (tx);
  GET_INT_FIELD (ty);
  GET_INT_FIELD (gx);
  GET_INT_FIELD (gy);
  GET_INT_FIELD (override);
  UPDATE_OVERRIDE (thedirective);
  GET_INT_FIELD (minpixels);
  // curobj->minpixelsalloted = thedirective->minpixels?thedirective->minpixels:16;//FIXME setpixelmin
  setpixelmin (curobj);
  gchar *ticks = (gchar *) xmlGetProp (LilyDirectiveElem, (xmlChar *) "ticks");
  if (ticks)
    curobj->durinticks = atoi (ticks);
  //curobj->baseduration = - curobj->durinticks;
  return curobj;
}


/**
 * Parse the given <stem-directive> element into a DenemoObject.
 * @param stemDirectiveElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoMovement to populate 
 * 
 * @return the new DenemoObject
 * 
 */
static void
parseStemDirective (xmlNodePtr stemDirectiveElem, stemdirective * stem)
{
  xmlNodePtr childElem;
  gchar *stemDirName = (gchar *) xmlGetProp (stemDirectiveElem, (xmlChar *) "type");
  enum stemdirections stemDir = DENEMO_STEMBOTH;

  if (stemDirName == NULL)
    g_warning ("No \"type\" attribute on <stem-directive> element; " "defaulting to auto");
  else if (strcmp (stemDirName, "auto") == 0)
    stemDir = DENEMO_STEMBOTH;
  else if (strcmp (stemDirName, "up") == 0)
    stemDir = DENEMO_STEMUP;
  else if (strcmp (stemDirName, "down") == 0)
    stemDir = DENEMO_STEMDOWN;
  else
    g_warning ("Invalid stem directive type \"%s\"; defaulting to auto", stemDirName);

  FOREACH_CHILD_ELEM (childElem, stemDirectiveElem)
  {

    if (ELEM_NAME_EQ (childElem, "directives"))
      {
        stem->directives = parseDirectives (childElem);
      }
  }
  stem->type = stemDir;
  return;
}


/**
 * Parse the given <tuplet-start> into a tuplet open DenemoObject.
 * 
 * @param tupletStartElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoMovement to populate 
 * 
 * @return the new DenemoObject
 */
static void
parseTupletStart (xmlNodePtr tupletStartElem, xmlNsPtr ns, tuplet * tup)
{
  xmlNodePtr childElem;
  xmlNodePtr multiplierElem = getXMLChild (tupletStartElem, "multiplier", ns);
  gboolean successful = FALSE;
  gint numerator, denominator;

  if (multiplierElem == NULL)
    {
      g_warning ("No <multiplier> element found under <tuplet-start>; " "defaulting to 1/1");
    }
  else
    {
      if (parseFraction (multiplierElem, ns, &numerator, &denominator) == 0)
        {
          successful = TRUE;
        }
      else
        {
          g_warning ("Bad multiplier fraction found for <tuplet-start>; " "defaulting to 1/1");
        }
    }
  //FIXME get mult elem in this loop
  FOREACH_CHILD_ELEM (childElem, tupletStartElem)
  {

    if (ELEM_NAME_EQ (childElem, "directives"))
      {
        tup->directives = parseDirectives (childElem);
      }
  }
  if (!successful)
    {
      numerator = 1;
      denominator = 1;
    }


  tup->numerator = numerator;
  tup->denominator = denominator;
  return;
}

/* tupet end  */
static void
parseTupletEnd (xmlNodePtr tupletStartElem, tuplet * tup)
{
  xmlNodePtr childElem;

  FOREACH_CHILD_ELEM (childElem, tupletStartElem)
  {

    if (ELEM_NAME_EQ (childElem, "directives"))
      {
        tup->directives = parseDirectives (childElem);
      }
  }

  return;
}





/**
 * Parse the given <thumbnail> into the thumbnail  DenemoSelection.
 * @param thumbElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param selection the DenemoSelection to populate 
 * 
 * @return 
 */
static void
parseThumbElem (xmlNodePtr thumbElem, DenemoSelection * selection)
{
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, thumbElem)
  {
    if (ELEM_NAME_EQ (childElem, "first-staff"))
      selection->firststaffmarked = getXMLIntChild (childElem);
    else if (ELEM_NAME_EQ (childElem, "last-staff"))
      selection->laststaffmarked = getXMLIntChild (childElem);
    else if (ELEM_NAME_EQ (childElem, "first-measure"))
      selection->firstmeasuremarked = getXMLIntChild (childElem);
    else if (ELEM_NAME_EQ (childElem, "last-measure"))
      selection->lastmeasuremarked = getXMLIntChild (childElem);
    else if (ELEM_NAME_EQ (childElem, "first-obj"))
      selection->firstobjmarked = getXMLIntChild (childElem);
    else if (ELEM_NAME_EQ (childElem, "last-obj"))
      selection->lastobjmarked = getXMLIntChild (childElem);
  }
}

/**
 * Parse the given <thumbnail> into the thumbnail  DenemoSelection.
 * @param thumbElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param selection the DenemoSelection to populate 
 * 
 * @return 
 */
static void
parseSourceFileElem (xmlNodePtr sElem, DenemoProject * gui)
{
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, sElem)
  {
    if (ELEM_NAME_EQ (childElem, "x"))
      gui->source_x = getXMLIntChild (childElem);
    else if (ELEM_NAME_EQ (childElem, "y"))
      gui->source_y = getXMLIntChild (childElem);
    else if (ELEM_NAME_EQ (childElem, "width"))
      gui->source_width = getXMLIntChild (childElem);
    else if (ELEM_NAME_EQ (childElem, "height"))
      gui->source_height = getXMLIntChild (childElem);
    else if (ELEM_NAME_EQ (childElem, "scale"))
      gui->source_scale = getXMLIntChild (childElem);
  }

}


/**
 * Parse the given <setup-info> element into the given score.
 * @param editInfoElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoMovement to populate 
 * 
 * @return 0 on succes, -1 on failure
 */
static gint
parseSetupInfo (xmlNodePtr editInfoElem, xmlNsPtr ns, DenemoProject * gui)
{
  xmlNodePtr childElem;
  gchar *tmp;

  FOREACH_CHILD_ELEM (childElem, editInfoElem)
  {
    if (childElem->ns == ns)
      {
        if (ELEM_NAME_EQ (childElem, "lilyversion"))
          {
            tmp = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
            if (tmp != NULL && strcmp (tmp, "2.8.7"))   //2.8.7 is ignored, it was the value used when version was mandated, but did not mean anything.
              {
                //g_debug ("lilypond version %s", tmp);
                //g_string_assign (gui->lilycontrol.lilyversion, tmp);
                if (strcmp (tmp, INSTALLED_LILYPOND_VERSION))
                  g_warning ("This file may contain embedded LilyPond from an earlier LilyPond version\nIf you have problems printing from it\nrefresh the directives responsible.");
                g_free (tmp);
              }
            else
              g_warning ("ignoring version %s", tmp);
          }
        if (ELEM_NAME_EQ (childElem, "lilypond"))       //backward compatibility only
          {
            tmp = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
            if (tmp != NULL)
              {
                DenemoDirective *directive = (DenemoDirective *) g_malloc0 (sizeof (DenemoDirective));
                directive->postfix = g_string_new (tmp);
                directive->tag = g_string_new ("UnknownScoreTag");
                gui->lilycontrol.directives = g_list_append (NULL, directive);
                g_free (tmp);
              }
          }
        else if (ELEM_NAME_EQ (childElem, "fontsize"))
          {
            gint font = getXMLIntChild (childElem);
            gui->lilycontrol.staffsize = g_string_new (g_strdup_printf ("%d", font));
            //g_debug ("Font Size %d", font);
          }
        else if (ELEM_NAME_EQ (childElem, "papersize"))
          {

            tmp = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
            if (tmp != NULL)
              {
                //g_debug ("Paper size %s\n", tmp);
                g_string_assign (gui->lilycontrol.papersize, tmp);
                g_free (tmp);
              }
          }
        else if (ELEM_NAME_EQ (childElem, "orientation"))
          {

            gint orientation = getXMLIntChild (childElem);
            gui->lilycontrol.orientation = orientation;
            //g_debug ("Orientation %d\n", orientation);
          }
        else if (ELEM_NAME_EQ (childElem, "score-directives"))
          {
            gui->lilycontrol.directives = parseWidgetDirectives (childElem, (gpointer) score_directive_put_graphic, NULL, NULL);
          }



      }
  }

  return 0;
}

/**
 * Parse the given <edit-info> element into the given score.
 * @param editInfoElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoMovement to populate 
 * 
 * @return 0 on success, -1 on failure
 */
static gint
parseEditInfo (xmlNodePtr editInfoElem, xmlNsPtr ns, DenemoMovement * si)
{
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, editInfoElem)
  {
    if (childElem->ns == ns)
      {
        if (ELEM_NAME_EQ (childElem, "staffno"))
          {
            current_staff = getXMLIntChild (childElem);
            //g_debug ("Staff no %d\t", current_staff);
          }
        if (ELEM_NAME_EQ (childElem, "measureno"))
          {
            current_measure = getXMLIntChild (childElem);
            //g_debug ("Staff no %d\t", current_staff);
          }
        else if (ELEM_NAME_EQ (childElem, "cursorposition"))
          {
            current_position = getXMLIntChild (childElem);
            if (current_position < 0)
              current_position = 0;
          }
        else if (ELEM_NAME_EQ (childElem, "tonalcenter"))
          {
            tonal_center = getXMLIntChild (childElem);
            if (tonal_center < -7 || tonal_center > 7)
              tonal_center = 0;
            set_enharmonic_position (tonal_center);
          }
        else if (ELEM_NAME_EQ (childElem, "zoom"))
          {
            si->zoom = getXMLIntChild (childElem) / 100.0;
            if (si->zoom < 0.01)
              si->zoom = 1.0;
          }
        else if (ELEM_NAME_EQ (childElem, "system-height"))
          {
            si->system_height = getXMLIntChild (childElem) / 100.0;
            if (si->system_height < 0.01)
              si->system_height = 1.0;
          }
        else if (ELEM_NAME_EQ (childElem, "page-zoom"))
          {
            si->page_zoom = getXMLIntChild (childElem) / 100.0;
            if (si->page_zoom < 0.01)
              si->page_zoom = 1.0;
          }
        else if (ELEM_NAME_EQ (childElem, "page-system-height"))
          {
            si->page_system_height = getXMLIntChild (childElem) / 100.0;
            if (si->page_system_height < 0.01)
              si->page_system_height = 1.0;
          }
        else if (ELEM_NAME_EQ (childElem, "page-width"))
          {
            si->page_width = getXMLIntChild (childElem);
            if (si->page_width < 0)
              si->page_width = 0;
          }
        else if (ELEM_NAME_EQ (childElem, "page-height"))
          {
            si->page_height = getXMLIntChild (childElem);
            if (si->page_height < 0)
              si->page_height = 0;
          }
        else if (ELEM_NAME_EQ (childElem, "measure-width"))
          {
            si->measurewidth = getXMLIntChild (childElem);
            if (si->measurewidth < 10)
              si->measurewidth = DENEMO_INITIAL_MEASURE_WIDTH;
          }

      }
  }
  return 0;
}


/**
 * Parse the given <score-info> element into the given score.
 * @param scoreInfoElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoMovement to populate 
 * 
 * @return 0 on success, -1 on failure
 */
static gint
parseScoreInfo (xmlNodePtr scoreInfoElem, xmlNsPtr ns, DenemoMovement * si)
{
  xmlNodePtr childElem, grandchildElem;
  gint bpm;
  // gchar *title, *subtitle, *composer, *poet, *meter, *arranger, *opus;
  // gchar *instrument, *dedication, *piece, *head, *copyright, *footer,
  gchar *title, *mvmnt_header, *markup_before, *markup_after, *layout_markup;

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
                          bpm = 120;
                          g_warning ("Bad value for <bpm>: must be an integer");
                        }
                      si->tempo = bpm;
                      if (si->tempo < 1 || si->tempo > 1000)
                        si->tempo = 120;
                      set_master_tempo (si, 1.0);
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
            title = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
            if (title != NULL)
              {
                //g_string_assign (si->headerinfo.title, title);
                gchar *val = g_strdup_printf ("title = \"%s\"\n", title);
                header_directive_put_postfix ("Movement-title", val);
                header_directive_put_display ("Movement-title", title);
                paper_directive_put_postfix ("PrintAllHeaders", "printallheaders = ##t\n");
                g_free (val);
                g_free (title);
              }
          }

#define DO_ELEM(subtitle, dummy)\
    else if (ELEM_NAME_EQ (childElem, subtitle))\
      {\
        gchar *field = (gchar *) xmlNodeListGetString (childElem->doc,\
                               childElem->\
                               xmlChildrenNode, 1);\
        if (field != NULL)\
          {\
        gchar *val = g_strdup_printf(subtitle" = \"%s\"\n", field);\
            header_directive_put_postfix("Movement-"subtitle, val);\
            header_directive_put_display("Movement-"subtitle, field);\
                paper_directive_put_postfix("PrintAllHeaders", "printallheaders = ##t\n");\
        g_free(val);\
        g_free (field);\
          }\
      }\
                                //FIXME are these actually in use??? I think the fields are place in a <scoreheaders> element
        DO_ELEM ("subtitle", 0) DO_ELEM ("composer", "HeaderComposer") DO_ELEM ("poet", "HeaderPoet") DO_ELEM ("meter", "HeaderMeter") DO_ELEM ("opus", "HeaderOpus") DO_ELEM ("arranger", "HeaderArranger") DO_ELEM ("instrument", "HeaderInstrument") DO_ELEM ("dedication", "HeaderDedication") DO_ELEM ("piece", "HeaderPiece") DO_ELEM ("head", "HeaderHead") DO_ELEM ("copyright", "HeaderCopyright") DO_ELEM ("footer", "HeaderFooter")
#undef DO_ELEM
#if 0
//drop support for this legacy setting of the tagline field
          else if (ELEM_NAME_EQ (childElem, "tagline"))
          {
            tagline = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
            if (tagline != NULL)
              {
                gchar *val = g_strdup_printf ("tagline = \"%s\"\n", tagline);
                scoreheader_directive_put_postfix ("Scoretagline", val);
                g_free (val);
                g_free (tagline);
              }
          }
#endif
        else if (ELEM_NAME_EQ (childElem, "extra"))
          {
            mvmnt_header = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
            if (mvmnt_header != NULL)
              {
                //g_string_assign (si->headerinfo.extra, mvmnt_header);
                g_free (mvmnt_header);
              }
          }
        else if (ELEM_NAME_EQ (childElem, "markup_before"))
          {
            markup_before = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
            if (markup_before != NULL)
              {
                //g_string_assign (si->headerinfo.lilypond_before, markup_before);
                g_free (markup_before);
              }
          }
        else if (ELEM_NAME_EQ (childElem, "markup_after"))
          {
            markup_after = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
            if (markup_after != NULL)
              {
                //g_string_assign (si->headerinfo.lilypond_after, markup_after);
                g_free (markup_after);
              }
          }
        else if (ELEM_NAME_EQ (childElem, "layout_markup"))
          {
            layout_markup = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
            if (layout_markup != NULL)
              {
                //g_string_assign (si->headerinfo.layout, layout_markup);
                g_free (layout_markup);
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
 * @param si the DenemoMovement to populate 
 * 
 * @return 0 on success, -1 on failure
 */
static gint
parseStaff (xmlNodePtr staffElem, xmlNsPtr ns, DenemoMovement * si)
{
  xmlNodePtr staffInfoElem, childElem;
  DenemoStaff *curStaff = (DenemoStaff *) si->currentprimarystaff->data;

  staffInfoElem = getXMLChild (staffElem, "staff-info", ns);
  //RETURN_IF_ELEM_NOT_FOUND ("staff", staffInfoElem, "staff-info");
  if (staffInfoElem == NULL)
    return 0;
//rest is backward compatibility only
  FOREACH_CHILD_ELEM (childElem, staffInfoElem)
  {
    if (childElem->ns == ns)
      {
        if (ELEM_NAME_EQ (childElem, "number-of-lines"))
          {
            curStaff->no_of_lines = getXMLIntChild (childElem);
            if (curStaff->no_of_lines == G_MAXINT)
              {
                g_warning ("Could not determine number of lines in staff; " "defaulting to 5");
                curStaff->no_of_lines = 5;
              }
          }
        else if (ELEM_NAME_EQ (childElem, "volume"))
          {
            curStaff->volume = getXMLIntChild (childElem);
          }
        else if (ELEM_NAME_EQ (childElem, "midi_prognum_override"))
          {
            /*obsolete */ ;
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
          {                     //  set dynamically now
            //   curStaff->space_above = getXMLIntChild (childElem);
          }
        else if (ELEM_NAME_EQ (childElem, "space_below"))
          {                     //  set dynamically now
            //    curStaff->space_below = getXMLIntChild (childElem);
          }
        else if (ELEM_NAME_EQ (childElem, "hasfigures"))
          {
            curStaff->hasfigures = getXMLIntChild (childElem);
            if (curStaff->hasfigures)
              si->has_figures = (gpointer) TRUE;
          }
        else if (ELEM_NAME_EQ (childElem, "hasfakechords"))
          {
            curStaff->hasfakechords = getXMLIntChild (childElem);

          }
        else if (ELEM_NAME_EQ (childElem, "haslyrics"))
          {
            //backward compatibility only

          }
        else if (ELEM_NAME_EQ (childElem, "verses"))
          {

            parseVerses (si, curStaff, childElem);
          }
        else if (ELEM_NAME_EQ (childElem, "instrument"))
          {
            gchar *temp = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
            if (temp)
              g_string_assign (curStaff->midi_instrument, temp);
            else
              curStaff->midi_instrument = g_string_new ("");
            g_free (temp);
          }

        else if (ELEM_NAME_EQ (childElem, "device-port"))
          {
            gchar *temp = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
            if (temp)
              g_string_assign (curStaff->device_port, temp);
            else
              curStaff->device_port = g_string_new ("");
            g_free (temp);
          }



        else if (ELEM_NAME_EQ (childElem, "context"))
          {
            gchar *temp = NULL;
            temp = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
            addContext (temp);
            g_free (temp);
          }


        else if (ELEM_NAME_EQ (childElem, "lilybefore"))
          {
            gchar *temp = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
            if (temp)
              g_warning ("Ignoring the old-style string %s\nAdd this in LilyPond window if required", temp);
            g_free (temp);
          }
/*  else if (ELEM_NAME_EQ (childElem, "staff-prolog")) */
/*    { */
/*      gchar *temp =  */
/*        (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1); */
/*      curStaff->staff_prolog = (temp?g_string_new(temp):NULL); */
/*      g_free (temp); */
/*    } */
        else if (ELEM_NAME_EQ (childElem, "staff-prolog-insert"))       //backward compatibility only
          {
            gchar *temp = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
            if (temp)
              {
                DenemoDirective *directive = (DenemoDirective *) g_malloc0 (sizeof (DenemoDirective));
                directive->postfix = g_string_new (temp);
                curStaff->staff_directives = g_list_append (NULL, directive);
                g_free (temp);
              }
          }
        else if (ELEM_NAME_EQ (childElem, "voice-prolog-insert"))       //backward compatibility only
          {
            gchar *temp = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
            if (temp)
              {
                DenemoDirective *directive = (DenemoDirective *) g_malloc0 (sizeof (DenemoDirective));
                directive->postfix = g_string_new (temp);
                curStaff->voice_directives = g_list_append (NULL, directive);
                g_free (temp);
              }
          }
        else if (ELEM_NAME_EQ (childElem, "staff-directives"))
          {
            curStaff->staff_directives = parseWidgetDirectives (childElem, (gpointer) staff_directive_put_graphic, curStaff->staffmenu, &curStaff->staff_directives);
          }
        else if (ELEM_NAME_EQ (childElem, "voice-directives"))
          {
            curStaff->voice_directives = parseWidgetDirectives (childElem, (gpointer) voice_directive_put_graphic, curStaff->voicemenu, &curStaff->voice_directives);
          }
        else if (ELEM_NAME_EQ (childElem, "clef-directives"))
          {
            curStaff->clef.directives = parseDirectives (childElem);
          }


/*  else if (ELEM_NAME_EQ (childElem, "lyrics-prolog")) */
/*    { */
/*      gchar *temp =  */
/*        (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1); */
/*      curStaff->lyrics_prolog = (temp?g_string_new(temp):NULL); */
/*      g_free (temp); */
/*    } */
/*  else if (ELEM_NAME_EQ (childElem, "figures-prolog")) */
/*    { */
/*      gchar *temp =  */
/*        (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1); */
/*      curStaff->figures_prolog = (temp?g_string_new(temp):NULL); */
/*      g_free (temp); */
/*    } */
/*  else if (ELEM_NAME_EQ (childElem, "fakechords-prolog")) */
/*    { */
/*      gchar *temp =  */
/*        (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1); */
/*      curStaff->fakechords_prolog = (temp?g_string_new(temp):NULL); */
/*      g_free (temp); */
/*    } */
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
 * @param si the DenemoMovement to populate 
 * 
 * @return 0 on success, -1 on failure
 */
static gint
parseVoiceProps (xmlNodePtr voicePropElem, DenemoMovement * si)
{
  DenemoStaff *curStaff = (DenemoStaff *) si->currentstaff->data;
  xmlNodePtr childElem;
  FOREACH_CHILD_ELEM (childElem, voicePropElem)
  {
    if (ELEM_NAME_EQ (childElem, "number-of-lines"))
      {
        curStaff->no_of_lines = getXMLIntChild (childElem);
        if (curStaff->no_of_lines == G_MAXINT)
          {
            g_warning ("Could not determine number of lines in staff; " "defaulting to 5");
            curStaff->no_of_lines = 5;
          }
      }
    else if (ELEM_NAME_EQ (childElem, "voice-control"))
      {
        curStaff->voicecontrol = getXMLIntChild (childElem);
      }
    else if (ELEM_NAME_EQ (childElem, "instrument"))
      {
        gchar *temp = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
        if (temp)
          g_string_assign (curStaff->midi_instrument, temp);
        else
          curStaff->midi_instrument = g_string_new ("");
        g_free (temp);
      }
    else if (ELEM_NAME_EQ (childElem, "volume"))
      {
        curStaff->volume = getXMLIntChild (childElem);
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
    else if (ELEM_NAME_EQ (childElem, "hasfigures"))
      {
        curStaff->hasfigures = getXMLIntChild (childElem);
        if (curStaff->hasfigures)
          si->has_figures = (gpointer) TRUE;
      }
    else if (ELEM_NAME_EQ (childElem, "hasfakechords"))
      {
        curStaff->hasfakechords = getXMLIntChild (childElem);

      }
    else if (ELEM_NAME_EQ (childElem, "verses"))
      {
        parseVerses (si, curStaff, childElem);
      }
    else if (ELEM_NAME_EQ (childElem, "instrument"))
      {
        gchar *temp = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
        if (temp)
          g_string_assign (curStaff->midi_instrument, temp);
        else
          curStaff->midi_instrument = g_string_new ("");
        g_free (temp);
      }
    else if (ELEM_NAME_EQ (childElem, "device-port"))
      {
        gchar *temp = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
        if (temp)
          g_string_assign (curStaff->device_port, temp);
        else
          curStaff->device_port = g_string_new ("");
        g_free (temp);
      }
    else if (ELEM_NAME_EQ (childElem, "staff-directives"))
      {
        curStaff->staff_directives = parseWidgetDirectives (childElem, (gpointer) staff_directive_put_graphic, curStaff->staffmenu, &curStaff->staff_directives);
      }
    else if (ELEM_NAME_EQ (childElem, "voice-directives"))
      {
        curStaff->voice_directives = parseWidgetDirectives (childElem, (gpointer) voice_directive_put_graphic, curStaff->voicemenu, &curStaff->voice_directives);
      }
    else if (ELEM_NAME_EQ (childElem, "clef-directives"))
      {
        curStaff->clef.directives = parseDirectives (childElem);
      }
    else
      {
        ILLEGAL_ELEM ("staff-info", childElem);
      }
  }
  return 0;
}


/**
 * Parse the given <voice-info> element into the voice in the given score.
 * @param voiceInfoElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoMovement to populate 
 * 
 * @return 0 on success, -1 on failure
 */
static gint
parseVoiceInfo (xmlNodePtr voiceInfoElem, xmlNsPtr ns, DenemoMovement * si)
{
  xmlNodePtr childElem;
  gchar *voiceName;

  FOREACH_CHILD_ELEM (childElem, voiceInfoElem)
  {
    if (childElem->ns == ns)
      {
        if (ELEM_NAME_EQ (childElem, "voice-name"))
          {
            voiceName = (gchar *) xmlNodeListGetString (childElem->doc, childElem->xmlChildrenNode, 1);
            if (voiceName != NULL)
              {
                DenemoStaff *thestaffstruct = (DenemoStaff *) si->currentstaff->data;
                g_string_assign (thestaffstruct->denemo_name, voiceName);
                g_free (voiceName);
                set_lily_name (thestaffstruct->denemo_name, thestaffstruct->lily_name);
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
 * @param si the DenemoMovement to populate 
 * 
 * @return 0 on success, -1 on failure
 */
static gint
parseInitVoiceParams (xmlNodePtr initVoiceParamsElem, xmlNsPtr ns, DenemoMovement * si)
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

            staffXMLID = (gchar *) xmlGetProp (childElem, (xmlChar *) "staff");
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

                    curVoice->voicecontrol = DENEMO_SECONDARY;  //Set primary as well if display is to be separated
                    curVoice->no_of_lines = ((DenemoStaff *) si->currentprimarystaff->data)->no_of_lines;
                  }
                else
                  {
                    /* This is a new staff. */

                    if (!ELEM_NAME_EQ (staffElem, "staff"))
                      {
                        g_warning ("<staff-ref> points to a <%s>, not a " "<staff>", staffElem->name);
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
            parseClef (childElem, &curVoice->clef);
          }
        else if (ELEM_NAME_EQ (childElem, "key-signature"))
          {
            parseKeySignature (childElem, &(curVoice->keysig));
            initkeyaccs (curVoice->keysig.accs, curVoice->keysig.number);
            //dnm_setinitialkeysig(curVoice, curVoice->skey, curVoice->skey_isminor);
          }
        else if (ELEM_NAME_EQ (childElem, "time-signature"))
          {
            parseTimeSignature (childElem, ns, &curVoice->timesig);
          }

        else if (ELEM_NAME_EQ (childElem, "sources"))
          {
            curVoice->sources = parseSources (childElem);
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
 * @param si the DenemoMovement to populate 
 *
 * @return 0 on success, -1 on failure
 */
static gint
parseMeasures (xmlNodePtr measuresElem, xmlNsPtr ns, DenemoMovement * si)
{
  xmlNodePtr childElem, objElem, notesElem = NULL;
  DenemoObject *curObj;
  gboolean startedBeam = FALSE;
  gint currentClef = ((DenemoStaff *) si->currentstaff->data)->clef.type;
  GList *slurEndChordElems = NULL;
  GList *crescEndChordElems = NULL;
  GList *diminEndChordElems = NULL;

  FOREACH_CHILD_ELEM (childElem, measuresElem)
  {
    if (ELEM_NAME_EQ (childElem, "measure") && (childElem->ns == ns))
      {
        if (si->currentmeasure == NULL)
          {

            si->currentmeasure = dnm_addmeasures (si, si->currentmeasurenum - 1, 1, FALSE);
            g_debug ("ImportXML Adding Measure \n currentmeasurenum %d", si->currentmeasurenum);
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
#if 0
                  /* Ignore beam attribute for now. */
                  if (prevChord == NULL)
                    {
                      g_warning ("Received <beam-end> without chord " "before it");
                    }
                  else
                    {
                      prevChord->isstart_beamgroup = FALSE;
                      prevChord->isend_beamgroup = TRUE;
                    }
#endif
                }
              else if (ELEM_NAME_EQ (objElem, "beam-start"))
                {
#if 0
                  /* Ignore ID for now. */
                  startedBeam = TRUE;
#endif
                }
              else if (ELEM_NAME_EQ (objElem, "chord"))
                {
                  curObj = parseChord (objElem, ns, si, currentClef, &slurEndChordElems, &crescEndChordElems, &diminEndChordElems, &notesElem);
                  /* old format files will not have has... fields of staff explicit
                     so for backwards compatibility we reconstruct it here */
                  if (((chord *) curObj->object)->figure)
                    ((DenemoStaff *) si->currentstaff->data)->hasfigures = TRUE;
                  if (((chord *) curObj->object)->fakechord)
                    ((DenemoStaff *) si->currentstaff->data)->hasfakechords = TRUE;

                  if (startedBeam)
                    {
                      curObj->isstart_beamgroup = TRUE;
                      curObj->isend_beamgroup = FALSE;
                      startedBeam = FALSE;
                    }
                }
              else if (ELEM_NAME_EQ (objElem, "clef"))
                {
                  curObj = dnm_newclefobj (DENEMO_TREBLE_CLEF);
                  parseClef (objElem, curObj->object);
                  gchar *showProp = (gchar *) xmlGetProp (objElem, (xmlChar *) "show");
                  if (showProp)
                    curObj->isinvisible = !strcmp (showProp, "false");
                  currentClef = ((clef *) curObj->object)->type;
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
                  curObj = parseLilyDir (objElem);
                }

              /* else if (ELEM_NAME_EQ (objElem, "dynamic"))
                 {
                 curObj = parseDynamic (objElem, ns, si);
                 } */
              else if (ELEM_NAME_EQ (objElem, "grace-end"))
                {
                  g_warning (_("Obsolete form, use earlier Denemo version to convert"));        //curObj = newgraceend ();
                }
              else if (ELEM_NAME_EQ (objElem, "grace-start"))
                {
                  g_warning (_("Obsolete form, use earlier Denemo version to convert"));        //curObj = parseGraceStart (objElem, ns, si);
                }
              else if (ELEM_NAME_EQ (objElem, "key-signature"))
                {
#if 1
                  curObj = dnm_newkeyobj (0, 0, 0);
                  parseKeySignature (objElem, curObj->object);

                  initkeyaccs (((keysig *) curObj->object)->accs, ((keysig *) curObj->object)->number);
                  //dnm_setinitialkeysig(((keysig *) curObj->object), keySig, isMinor);
#else
                  //warningdialog(_("Dodgy code"));
                  parseKeySignature (objElem, &keySig, &isMinor);
                  //curObj = dnm_newkeyobj (keySig, isMinor, 0);
                  //initkeyaccs (((keysig *) curObj->object)->accs, keySig);
                  dnm_setinitialkeysig ((DenemoStaff *) si->currentstaff->data, keySig, isMinor);
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
                    }
                }
              else if (ELEM_NAME_EQ (objElem, "stem-directive"))
                {
                  curObj = dnm_stem_directive_new (DENEMO_STEMBOTH);
                  parseStemDirective (objElem, curObj->object);
                }
              else if (ELEM_NAME_EQ (objElem, "time-signature"))
                {
                  curObj = dnm_newtimesigobj (4, 4);
                  parseTimeSignature (objElem, ns, curObj->object);
                }
              else if (ELEM_NAME_EQ (objElem, "tuplet-end"))
                {
                  curObj = newtupclose ();
                  parseTupletEnd (objElem, curObj->object);
                }
              else if (ELEM_NAME_EQ (objElem, "tuplet-start"))
                {
                  curObj = newtupopen (4, 4);
                  parseTupletStart (objElem, ns, curObj->object);
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
                  si->currentmeasure->data = g_list_append ((objnode *) si->currentmeasure->data, curObj);
                  //g_debug("Element type lily %d\n", curObj->type==LILYDIRECTIVE);

                  /*
                   * FIXME: Loop through notesElem to search for
                   * subelements of <note> that are in other namespaces.
                   */
                }
            }                   /* end if objElem is in the right namespace */
          else
            {
              /* FIXME: Handle other namespaces here. */
              ILLEGAL_ELEM ("measure", objElem);
            }
        }                       /* end foreach objElem in childElem */
        si->currentmeasurenum++;
        si->currentmeasure = si->currentmeasure->next;
      }                         /* end if childElem is a <measure> */
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
      g_warning ("Some unterminated crescendos were left at the end of the voice");
      g_list_free (crescEndChordElems);
    }

  if (diminEndChordElems != NULL)
    {
      g_warning ("Some unterminated diminuendos were left at the end of the voice");
      g_list_free (diminEndChordElems);
    }

  if (crescEndChordElems != NULL)
    {
      g_warning ("Some unterminated crescendos were left at the end of the voice");
      g_list_free (crescEndChordElems);
    }

  if (diminEndChordElems != NULL)
    {
      g_warning ("Some unterminated diminuendos were left at the end of the voice");
      g_list_free (diminEndChordElems);
    }

  return 0;
}



/**
 * Parse the given <voice> element into a voice in the given score.
 * @param voiceElem the XML node to process
 * @param ns the Denemo XML namespaces
 * @param si the DenemoMovement to populate 
 * 
 * @return 0 on success, -1 on failure
 */
static gint
parseVoice (xmlNodePtr voiceElem, xmlNsPtr ns, DenemoProject * gui)
{
  DenemoMovement *si = gui->movement;
  xmlNodePtr childElem;
  /*  gchar *id; */

  /* Create the staff structure. */

  si->currentstaffnum++;
  newstaff (gui, ADDFROMLOAD, DENEMO_NONE);
  si->currentstaff = g_list_last (si->thescore);
  si->currentmeasurenum = 1;
  Lyric = g_string_new ("");

  /* Parse the child elements. */

  childElem = getXMLChild (voiceElem, "voice-info", ns);
  RETURN_IF_ELEM_NOT_FOUND ("voice", childElem, "voice-info");
  if (parseVoiceInfo (childElem, ns, si) != 0)
    return -1;

  childElem = getXMLChild (voiceElem, "initial-voice-params", ns);
  RETURN_IF_ELEM_NOT_FOUND ("voice", childElem, "initial-voice-params");
  if (parseInitVoiceParams (childElem, ns, si) != 0)
    return -1;

  childElem = getXMLChild (voiceElem, "voice-props", ns);
  if (childElem)                //older files will not have this
    if (parseVoiceProps (childElem, si) != 0)
      return -1;

  childElem = getXMLChild (voiceElem, "measures", ns);
  RETURN_IF_ELEM_NOT_FOUND ("voice", childElem, "measures");
  if (parseMeasures (childElem, ns, si) != 0)
    return -1;
  if (Lyric->len)
    {
      DenemoStaff *staff = (DenemoStaff *) si->currentstaff->data;
      add_verse_to_staff (si, staff);
      gtk_text_buffer_set_text (gtk_text_view_get_buffer (staff->currentverse->data), Lyric->str, Lyric->len);
      g_signal_connect (G_OBJECT (gtk_text_view_get_buffer (staff->currentverse->data)), "changed", G_CALLBACK (lyric_change), NULL);
      //allow save on backward compatibility files... gtk_text_buffer_set_modified(gtk_text_view_get_buffer(staff->currentverse->data), FALSE);
      //g_debug("Appended <%s>\n", Lyric->str);
    }
  g_string_free (Lyric, FALSE);
  Lyric = NULL;
  /* FIXME: Handle elements in other namespaces. */

  return 0;
}


/**
 * Parse the given <score> element into the given score.
 * 
 * @param scoreElem the score element to process
 * @param ns Denemo's xml namespaces
 * @param si the DenemoMovement structure to populate
 * 
 * @return 0 on success ,-1 on failure
 */
static gint
parseScore (xmlNodePtr scoreElem, xmlNsPtr ns, DenemoProject * gui, ImportType type)
{
  DenemoMovement *si = gui->movement;
  xmlNodePtr childElem, voiceElem;


  childElem = getXMLChild (scoreElem, "edit-info", ns);

  if (childElem != 0)
    parseEditInfo (childElem, ns, si);

  childElem = getXMLChild (scoreElem, "header-directives", ns);
  if (childElem != 0)
    si->header.directives = parseWidgetDirectives (childElem, (gpointer) header_directive_put_graphic, NULL, &(si->header.directives));

  childElem = getXMLChild (scoreElem, "layout-directives", ns);
  if (childElem != 0)
    si->layout.directives = parseWidgetDirectives (childElem, (gpointer) layout_directive_put_graphic, NULL, &(si->layout.directives));

  childElem = getXMLChild (scoreElem, "movementcontrol-directives", ns);
  if (childElem != 0)
    si->movementcontrol.directives = parseWidgetDirectives (childElem, (gpointer) movementcontrol_directive_put_graphic, NULL, &(si->movementcontrol.directives));

  childElem = getXMLChild (scoreElem, "sources", ns);
  if (childElem != 0)
    si->sources = parseSources (childElem); //puts the pixbufs after decode_base64 into a GList* at this location

  childElem = getXMLChild (scoreElem, "audio", ns);
  if (childElem != 0)
    {
      si->recording = (DenemoRecording *) g_malloc (sizeof (DenemoRecording));
      parseAudio (childElem, si);
    }

  childElem = getXMLChild (scoreElem, "score-info", ns);
  RETURN_IF_ELEM_NOT_FOUND ("score", childElem, "score-info");
  if (type == REPLACE_SCORE)
    if (parseScoreInfo (childElem, ns, si) != 0)
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


/* parse the movement (ie DenemoMovement) from childElem */
static gint
parseMovement (xmlNodePtr childElem, xmlNsPtr ns, DenemoProject * gui, ImportType type)
{
  int ret = 0;

  DenemoMovement *si = gui->movement;
  if (type != ADD_STAFFS)
    gui->movements = g_list_append (gui->movements, gui->movement);
  si->currentstaffnum = 0;
  sPrevStaffElem = NULL;
  ret = parseScore (childElem, ns, gui, type);
  sPrevStaffElem = NULL;
  staffnode *curstaff;
  if (si->thescore == NULL)
    {
      g_warning ("Bad Denemo file");
      return -1;
    }
  for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
    {
      beamsandstemdirswholestaff ((DenemoStaff *) curstaff->data);
      showwhichaccidentalswholestaff ((DenemoStaff *) curstaff->data);
    }
  find_xes_in_all_measures (si);
  find_leftmost_allcontexts (si);
  if(current_staff==0)
    current_staff=1;
  si->currentstaffnum = current_staff ? current_staff : 1;

  si->currentmeasurenum = current_measure ? current_measure : 1;
  si->currentstaff = g_list_nth (si->thescore, current_staff - 1);
  setcurrents (si);
  si->cursor_x = current_position;
  si->currentobject = (objnode *) g_list_nth (si->currentmeasure->data, si->cursor_x);

  if (!si->currentobject)
    si->cursor_appending = TRUE;
  else
    si->cursor_appending = FALSE;
  // si->leftmeasurenum = si->currentstaffnum = si->currentmeasurenum = 1;

  if(!Denemo.non_interactive){
    set_rightmeasurenum (gui->movement);
    set_bottom_staff (gui);
    set_width_to_work_with (gui);
  }
  return ret;
}






/**
 * Import the given (possibly zlib-compressed) Denemo "native" XML file into
 * the given score.
 * 
 * @param filename the file to importxml
 * @param  gui DenemoProject to hold the score
 * @return 0 on success, -1 on failure
 */
gint
importXML (gchar * filename, DenemoProject * gui, ImportType type)
{

  gint ret = 0;
  xmlDocPtr doc = NULL;
  xmlNsPtr ns;
  /*  xmlNodePtr rootElem, childElem; */
  xmlNodePtr rootElem;
  /* ignore blanks between nodes that appear as "text" */
  xmlKeepBlanksDefault (0);
  gchar *version = NULL;
  current_movement = 0, current_staff = 0, current_measure = 0, current_position = 0;   //0 means is not set.

  if (sXMLIDToElemMap != NULL)
    {
      g_warning ("Recursive call to importXML - ignored");
      return -1;
    }
  /* Try to parse the file. */

  doc = xmlParseFile (filename);
  if (doc == NULL)
    {
      g_warning ("Could not read XML file %s", filename);
      return -1;
    }

  /*
   * Do a couple of sanity checks to make sure we've actually got a Denemo
   * format XML file.
   */

  rootElem = xmlDocGetRootElement (doc);
  ns = rootElem->ns;
  if ((strcmp ((gchar *) ns->href, DENEMO_XML_NAMESPACE) != 0) &&
      /*backward compatibility */ (strcmp ((gchar *) ns->href, "http://denemo.sourceforge.net/xmlns/Denemo") != 0))
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


  sscanf (version, "%d", &version_number);
  if (version_number > CURRENT_XML_VERSION)
    {
      g_warning ("Denemo XML file version %d found, but we can only handle %d and lower", version_number, CURRENT_XML_VERSION);
      ret = -1;
      goto cleanup;
    }

  /*
   * Okay, we've got a bona fide, 100% genuine Denemo XML file (hopefully).
   * So let's parse it.  The first thing to do is to construct a map from IDs
   * (or rather, attributes with name "id") to their respective elements.
   */

  buildXMLIDToElemMap (doc);
  /* Then, parse the score. */
  if (version_number >= 2)
    {
      xmlNodePtr childElem;
      switch (type)
        {
        case ADD_STAFFS:
          FOREACH_CHILD_ELEM (childElem, rootElem)
          {
            if (ELEM_NAME_EQ (childElem, "movement"))
              ret |= parseMovement (childElem, ns, gui, type);
            else
              continue;
            //g_debug("parsed more staffs breaking now\n");
            break;              //Note: we only adds staffs from first movement
          }
          break;
        case ADD_MOVEMENTS:
          FOREACH_CHILD_ELEM (childElem, rootElem)
          {
            if (ELEM_NAME_EQ (childElem, "lilycontrol") || ELEM_NAME_EQ (childElem, "custom_scoreblock") || ELEM_NAME_EQ (childElem, "visible_scoreblock") || ELEM_NAME_EQ (childElem, "scoreheader-directives") || ELEM_NAME_EQ (childElem, "paper-directives"))
              {
                continue;       /* do not change the header when adding movements parseScoreInfo(childElem, ns, gui); */
              }
            else if (ELEM_NAME_EQ (childElem, "movement"))
              {
                point_to_empty_movement (gui);
                ret |= parseMovement (childElem, ns, gui, type);
                //g_debug("parsed movement\n");
              }
            else
              {
                g_warning ("Unexpected %s", childElem->name);
              }
          }
          break;
        case REPLACE_SCORE:
          free_movements (gui);
          if(!Denemo.non_interactive)
            deleteSchemeText ();
          gui->has_script = FALSE;
          /* this is dependent on the order of elements, which is not strictly correct */
          FOREACH_CHILD_ELEM (childElem, rootElem)
          {
            if (!Denemo.non_interactive && ELEM_NAME_EQ (childElem, "scheme"))
              {
                gchar *tmp = (gchar *) xmlNodeListGetString (childElem->doc,
                                                             childElem->xmlChildrenNode, 1);
                if (tmp != NULL)
                  {
                    appendSchemeText (tmp);
                    g_free (tmp);
                  }
              }
            else if (ELEM_NAME_EQ (childElem, "movement-number"))
              {
                current_movement = getXMLIntChild (childElem);
              }
            else if (ELEM_NAME_EQ (childElem, "custom_prolog"))
              {
                gchar *tmp = (gchar *) xmlNodeListGetString (childElem->doc,
                                                             childElem->xmlChildrenNode, 1);
                //gui->custom_prolog = g_string_new(tmp);
                g_info ("The custom prolog \n\"%s\"\n is being ignored\n", tmp);
                warningdialog (_("Custom prolog is no longer supported. Use score directive prefix instead"));
                g_free (tmp);
              }
            else if (ELEM_NAME_EQ (childElem, "lilycontrol"))
              {
                parseSetupInfo (childElem, ns, gui);
              }
            else if (ELEM_NAME_EQ (childElem, "thumbnail"))
              {
                parseThumbElem (childElem, &gui->thumbnail);
              }
            else if (ELEM_NAME_EQ (childElem, "sourcefile"))
              {
                parseSourceFileElem (childElem, gui);
              }
            else if (ELEM_NAME_EQ (childElem, "scoreheader-directives"))
              {
                gui->scoreheader.directives = parseWidgetDirectives (childElem, (gpointer) scoreheader_directive_put_graphic, NULL, &(gui->scoreheader.directives));
              }
            else if (ELEM_NAME_EQ (childElem, "paper-directives"))
              {
                gui->paper.directives = parseWidgetDirectives (childElem, (gpointer) paper_directive_put_graphic, NULL, &(gui->paper.directives));
              }
            else if (ELEM_NAME_EQ (childElem, "custom_scoreblock"))
              {
                gchar *tmp = (gchar *) xmlNodeListGetString (childElem->doc,
                                                             childElem->xmlChildrenNode, 1);
                gchar *uri = (gchar *) xmlGetProp (childElem, (xmlChar *) "scoreblock_uri");
                if (tmp != NULL)
                  {
                    DenemoScoreblock *sb = get_scoreblock_for_lilypond (tmp);

                    if(!Denemo.non_interactive)
                    {
                      GtkWidget *notebook = get_score_layout_notebook (gui);
                      GtkWidget *label = gtk_label_new (sb->name);
                      gtk_notebook_prepend_page (GTK_NOTEBOOK (notebook), sb->widget, label);
                      gtk_widget_show_all (notebook);
                    }
                    gui->custom_scoreblocks = g_list_prepend (gui->custom_scoreblocks, sb);
                    sb->uri = uri;      //do not free uri
                    g_free (tmp);
                  }
              }
            else if (ELEM_NAME_EQ (childElem, "visible_scoreblock"))
              {
                if (gui->custom_scoreblocks)
                  {
                    DenemoScoreblock *sb = (DenemoScoreblock *) gui->custom_scoreblocks->data;
                    sb->visible = TRUE;
                  }
              }
            else if (ELEM_NAME_EQ (childElem, "movement"))
              {
                point_to_empty_movement (gui);
                ret |= parseMovement (childElem, ns, gui, type);
              }
            else
              {
                g_warning ("unrecognized element in score %s - abandoning file", childElem->name);
                point_to_empty_movement (gui);
                ret |= parseMovement (childElem, ns, gui, type);
              }
            if (gui->movement && gui->movement->lyricsbox)
              gtk_widget_hide (gui->movement->lyricsbox);
          }
          break;
        default:
          warningdialog (_("Erroneous call"));
          goto cleanup;
        }
    }

  if (gui->movement->lyricsbox)
    gtk_widget_hide (gui->movement->lyricsbox);
  gint steps_back = g_list_length (gui->movements) - current_movement;
  while (steps_back-- > 0)
    {
      if (gui->movement->lyricsbox)
        gtk_widget_hide (gui->movement->lyricsbox);
      prev_movement (NULL, NULL);
    }
  if (gui->movement->lyricsbox)
    {
      if (!Denemo.prefs.lyrics_pane)
        gtk_widget_hide (gui->movement->lyricsbox);
      else
        gtk_widget_show (gui->movement->lyricsbox);
    }
  if(!Denemo.non_interactive)
    score_status (gui, FALSE);

cleanup:

  if (version != NULL)
    g_free (version);
  if (doc != NULL)
    xmlFreeDoc (doc);
  if (sXMLIDToElemMap != NULL)
    {
      g_hash_table_foreach (sXMLIDToElemMap, freeHashTableKey, NULL);
      g_hash_table_destroy (sXMLIDToElemMap);
    }
  sXMLIDToElemMap = NULL;
  //g_debug("Number of movements %d\n", g_list_length(gui->movements));
  reset_movement_numbers (gui);
  if(!Denemo.non_interactive)
    set_movement_selector (gui);
  return ret;
}