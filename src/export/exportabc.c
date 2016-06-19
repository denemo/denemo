/* exportabc.cpp
 * Functions for exporting what Denemo's working on to an ABC file (adapted
 * somewhat from exportmudela.c)
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2001-2005 Eric Galluzzo
 */

#include <config.h>
#include <denemo/denemo.h>
#include "core/twoints.h"
#include "core/utils.h"

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <ctype.h>


/**
 * Determine key signature from denemo value
 *
 */
static void
determinebasekey (gint number, gchar ** basekey)
{
  switch (number)
    {
    case -7:
      *basekey = "Cb";
      break;
    case -6:
      *basekey = "Gb";
      break;
    case -5:
      *basekey = "Db";
      break;
    case -4:
      *basekey = "Ab";
      break;
    case -3:
      *basekey = "Eb";
      break;
    case -2:
      *basekey = "Bb";
      break;
    case -1:
      *basekey = "F";
      break;
    case 0:
      *basekey = "C";
      break;
    case 1:
      *basekey = "G";
      break;
    case 2:
      *basekey = "D";
      break;
    case 3:
      *basekey = "A";
      break;
    case 4:
      *basekey = "E";
      break;
    case 5:
      *basekey = "B";
      break;
    case 6:
      *basekey = "F#";
      break;
    case 7:
      *basekey = "C#";
      break;
    case 8:
      *basekey = "G#";
      break;
    case 9:
      *basekey = "D#";
      break;
    case 10:
      *basekey = "A#";
      break;
    default:
      *basekey = _("%{error. defaulting to%}C");
      break;
    }
}

/**
 * Determine clef from denemo value
 *
 */
static void
determineclef (gint type, gchar ** clefname, gint * octaveshift)
{
  switch (type)
    {
    case DENEMO_TREBLE_CLEF:
      *clefname = "treble";
      *octaveshift = 0;
      break;
    case DENEMO_BASS_CLEF:
      *clefname = "bass";
      *octaveshift = 2;
      break;
    case DENEMO_ALTO_CLEF:
      *clefname = "alto";
      *octaveshift = 1;
      break;
    case DENEMO_G_8_CLEF:
      *clefname = "treble8";
      *octaveshift = 1;
      break;
    case DENEMO_TENOR_CLEF:
      *clefname = "tenor";      /* FIXME: highly nonstandard */
      *octaveshift = 1;         /* FIXME: ditto */
      break;
    case DENEMO_SOPRANO_CLEF:
      *clefname = "soprano";    /* FIXME: highly nonstandard */
      *octaveshift = 0;         /* FIXME: ditto */
      break;
    default:
      *clefname = _("%{error. defaulting to%}treble");
      *octaveshift = 0;
      break;
    }
}

/**
 * determine the timesig denominator
 *
 */
static gint
determinedefaultlength (struct twoints *timesig)
{
  /* For 1/x and 2/x time signatures, return x*2; for all others, return x. */

  if (timesig->a < 3)
    return timesig->b << 1;
  else
    return timesig->b;
}


/**
 * Determine the length as it should be written to the ABC file as a fraction.
 * defaultlength is a denominator (e.g. "8"), the current default length
 * (e.g. "L:1/8").  duration and numdots are the Denemo duration and number of
 * dots, and the ABC length is returned in length.
 */
static void
determinelength (gint duration, gint numdots, gint defaultlength, struct twoints *length)
{
  /* ABC duration is:

   *                        numdots + 1
   *     defaultlength * (2             - 1)
   *     -----------------------------------
   *               numdots + duration
   *             2                          <--   = 1 << (numdots + duration)
   *
   * simplified as far as possible (defaultlength is always a multiple of 2).
   */

  //gint i;
  gint num = 1;                 /* so far */
  gint denom = 1 << (numdots + duration);

  while (denom > 1 && defaultlength > 1)
    {
      denom >>= 1;
      defaultlength >>= 1;
    }
  while (defaultlength > 1)
    {
      num <<= 1;
      defaultlength >>= 1;
    }
  num *= ((1 << (numdots + 1)) - 1);

  length->a = num;
  length->b = denom;
}


/**
 * Print out the given length (numerator/denominator), omitting 1's.
 */
static void
printlength (FILE * fp, struct twoints *length)
{
  if (length->a > 1)
    fprintf (fp, "%d", length->a);
  if (length->b > 1)
    {
      fprintf (fp, "/");
      if (length->b > 2)
        fprintf (fp, "%d", length->b);
    }
}


/**
 * Print out the given chord, given the current default length.
 */
static void
printchord (FILE * fp, chord * chordptr, gint octaveshift, gint defaultlength)
{
  gint i;
  struct twoints length;

  determinelength (chordptr->baseduration, chordptr->numdots, defaultlength, &length);

  if (chordptr->notes)          /* it's a note or chord, not a rest */
    {
      /* If there's only one note, it's a note, not a chord. */

      gboolean ischord = chordptr->notes->next != NULL;
      GList *curnotenode;
      note *curnote;
      gchar notename;
      gint octave;

      /* Print out the decorations and beginning of the slur (if any). */

      if (chordptr->slur_begin_p)
        fprintf (fp, "(");

      if (ischord)
        fprintf (fp, "[");

      for (curnotenode = chordptr->notes; curnotenode != NULL; curnotenode = curnotenode->next)
        {
          curnote = (note *) curnotenode->data;
          notename = mid_c_offsettoname (curnote->mid_c_offset);
          octave = mid_c_offsettooctave (curnote->mid_c_offset) + octaveshift - 2;

          /* Print accidentals. */

          if (curnote->showaccidental)
            {
              if (curnote->enshift > 0)
                {
                  for (i = 0; i < curnote->enshift; i++)
                    fprintf (fp, "^");
                }
              else if (curnote->enshift < 0)
                {
                  for (i = 0; i > curnote->enshift; i--)
                    fprintf (fp, "_");
                }
              else
                {
                  fprintf (fp, "=");
                }
            }

          /* Print the note name. */

          if (octave < 0)
            {
              octave += 1;
              notename = toupper (notename);
            }
          fprintf (fp, "%c", notename);

          /* Print the correct number of "'" or "," marks. */

          if (octave > 0)
            {
              for (i = 0; i < octave; i++)
                fprintf (fp, "'");
            }
          else
            {
              for (i = 0; i > octave; i--)
                fprintf (fp, ",");
            }

          printlength (fp, &length);
        }

      if (ischord)
        fprintf (fp, "]");

      if (chordptr->slur_end_p)
        fprintf (fp, ")");
      if (chordptr->is_tied)
        fprintf (fp, "-");
    }
  else                          /* it's a rest */
    {
      fprintf (fp, "z");
      printlength (fp, &length);
    }
}


/**
 * Export the ABC.  This works as follows:
 *   1. Write out all the headers (X:, T:, C:, Q:, and K:).
 *   2. Write out each voice in turn.
 */

void
exportabc (gchar * thefilename, DenemoProject * gui, gint start, gint end)
{
  DenemoMovement *si = gui->movement;
  gchar *clefname;
  gchar *basekeyname;
  FILE *fp;
  GString *filename = g_string_new (thefilename);
  /* FIXME: The following assumes that there is at least one staff. */
  DenemoStaff *firststaffstruct = (DenemoStaff *) si->thescore->data;
  staffnode *curstaff;
  DenemoStaff *curstaffstruct;
  gint defaultlength;
  gint curvoicenum;
  gint octaveshift;
  measurenode *curmeasure;
  gint externalmeasurenum;
  gint internalmeasurenum;
  gboolean emptymeasure;
  objnode *curobjnode;
  DenemoObject *curobj;
  struct twoints curtime;
  gboolean curisnonprimary;
  gboolean nextisnonprimary;
  objnode *tupleobjnode;
  DenemoObject *tupleobj;
  gint notesintuple;
  gboolean ishomogenoustuple;
  gint prevduration;
  gint prevnumdots;
  gdouble fraction = 0;
  enum clefs type = DENEMO_TREBLE_CLEF;
  /* Append .abc onto the filename if necessary */
  if (strcmp (filename->str + filename->len - 4, ".abc"))
    g_string_append (filename, ".abc");

  /* Now open the file */
  fp = fopen (filename->str, "w");

  /* And cut off the filename extension for use in the file itself */
  g_string_truncate (filename, filename->len - 4);

  fprintf (fp, _("%% ABC file generated by Denemo version "));
  fprintf (fp, VERSION "\n\n");
  fprintf (fp, "%% http://www.denemo.org\n\n");

  /* We export the following headers in this order:
   *   - X: (tune number)
   *   - T: (title)
   *   - C: (composer)
   *   - Q: (tempo)
   *   - M: (initial time signature)
   *   - L: (initial default note length)
   *   - %%staves (for the benefit of abcm2ps)
   *   - K: (key signature)
   */

  determinebasekey (firststaffstruct->keysig.isminor ? firststaffstruct->keysig.number + 3 : firststaffstruct->keysig.number, &basekeyname);
  curtime.a = firststaffstruct->timesig.time1;
  curtime.b = firststaffstruct->timesig.time2;
  defaultlength = determinedefaultlength (&curtime);

  fprintf (fp, "X:1\n");
  fprintf (fp, "T:%s\n", "No title");
  fprintf (fp, "Q:1/4=%d\n", si->tempo);
  fprintf (fp, "M:%d/%d\n", firststaffstruct->timesig.time1, firststaffstruct->timesig.time2);
  fprintf (fp, "L:1/%d\n", defaultlength);

  fraction = 1.0 / (gdouble) g_list_length (si->thescore);
  fraction /= 2;
  g_debug ("Fraction %lf", fraction);
  /* Figure out the %%staves comment.  It should look something like:

   *     %%staves [{(1 2) 3} 4 (5 6)] [7 8] 9
   *
   * where {} denotes curly braces (e.g. a piano, harp, or organ staff),
   *       [] denotes square braces,
   *   and () denotes two voices on the same staff.
   */
  fprintf (fp, "%%%%staves [");
  for (curstaff = si->thescore, curvoicenum = 1; curstaff != NULL; curstaff = curstaff->next, curvoicenum++)
    {
      curstaffstruct = (DenemoStaff *) curstaff->data;

      nextisnonprimary = (curstaff->next != NULL && ((DenemoStaff *) curstaff->next->data)->voicecontrol & DENEMO_SECONDARY);
      curisnonprimary = (curstaffstruct->voicecontrol & DENEMO_SECONDARY);

      if (curvoicenum != 1)
        fprintf (fp, " ");
      if (!curisnonprimary && nextisnonprimary)
        fprintf (fp, "(");
      fprintf (fp, "%d", curvoicenum);
      if (curisnonprimary && !nextisnonprimary)
        fprintf (fp, ")");
    }
  fprintf (fp, "]\n");

  fprintf (fp, "K:%s%s\n", basekeyname, firststaffstruct->keysig.isminor ? "m" : "");

  /* Now we output each voice in turn. */

  for (curstaff = si->thescore, curvoicenum = 1; curstaff != NULL; curstaff = curstaff->next, curvoicenum++)
    {
      curstaffstruct = (DenemoStaff *) curstaff->data;
      determineclef (curstaffstruct->clef.type, &clefname, &octaveshift);
      determinebasekey (curstaffstruct->keysig.isminor ? curstaffstruct->keysig.number + 3 : curstaffstruct->keysig.number, &basekeyname);
      curtime.a = curstaffstruct->timesig.time1;
      curtime.b = curstaffstruct->timesig.time2;
      defaultlength = determinedefaultlength (&curtime);

      /* First, the V: header. FIXME: Output name="..." too. */

      fprintf (fp, "%%\n%%\nV:%d clef=%s\n", curvoicenum, clefname);
      fprintf (fp, "I:octave=%d\n", -octaveshift);      /* FIXME: nonstandard */
      fprintf (fp, "M:%d/%d\n", curstaffstruct->timesig.time1, curstaffstruct->timesig.time2);
      fprintf (fp, "L:1/%d\n", defaultlength);
      fprintf (fp, "K:%s%s\n", basekeyname, curstaffstruct->keysig.isminor ? "m" : "");

      /* Then the actual notes, measure for measure (sorry, excuse the bad
       * Shakespeare pun). */

      for (curmeasure = curstaffstruct->themeasures, internalmeasurenum = MAX (start, 1), externalmeasurenum = 1; curmeasure != NULL && (end == 0 || internalmeasurenum <= end); curmeasure = curmeasure->next, externalmeasurenum++, internalmeasurenum++)
        {
          /* Print the measure number every 5 measures. */

          if (externalmeasurenum % 5 == 0)
            fprintf (fp, "%%%d\n", externalmeasurenum);

          emptymeasure = TRUE;

          /* Print out everything in this measure. */

          for (curobjnode = (objnode *) ((DenemoMeasure*)curmeasure->data)->objects; curobjnode; curobjnode = curobjnode->next)
            {
              curobj = (DenemoObject *) curobjnode->data;

              switch (curobj->type)
                {
                case CHORD:
                  emptymeasure = FALSE;
                  printchord (fp, (chord *) curobj->object, octaveshift, defaultlength);
                  if (curobj->isend_beamgroup)
                    fprintf (fp, " ");
                  break;

                case CLEF:
                  type = ((clef *) curobj->object)->type;
                  determineclef (type, &clefname, &octaveshift);
                  fprintf (fp, "[K:%s][I:octave=%d]", clefname, -octaveshift);
                  break;

                case KEYSIG:
                  determinebasekey (((keysig *) curobj->object)->isminor ? ((keysig *) curobj->object)->number + 3 : ((keysig *) curobj->object)->number, &basekeyname);
                  fprintf (fp, "[K:%s%s]", basekeyname, ((keysig *) curobj->object)->isminor ? "m" : "");
                  break;

                case TIMESIG:
                  curtime.a = ((timesig *) curobj->object)->time1;
                  curtime.b = ((timesig *) curobj->object)->time2;
                  defaultlength = determinedefaultlength (&curtime);
                  fprintf (fp, "[M:%d/%d][L:1/%d]", curtime.a, curtime.b, defaultlength);
                  break;

                case TUPOPEN:
                  /* Count the number of chords in the tuplet, and determine
                   * whether they are all of the same length.
                   *
                   * Note: This could eventually cross measures, maybe, and
                   *       the current algorithm doesn't take that into
                   *       account. */

                  notesintuple = 0;
                  ishomogenoustuple = TRUE;
                  prevduration = G_MAXINT;
                  prevnumdots = G_MAXINT;
                  for (tupleobjnode = curobjnode->next; tupleobjnode != NULL; tupleobjnode = tupleobjnode->next)
                    {
                      tupleobj = (DenemoObject *) tupleobjnode->data;
                      if (tupleobj->type == TUPCLOSE)
                        break;
                      if (tupleobj->type == CHORD)
                        {
                          notesintuple++;
                          if (ishomogenoustuple)
                            {
                              if ((prevduration != G_MAXINT && prevduration != ((chord *) tupleobj->object)->baseduration) || (prevnumdots != G_MAXINT && prevnumdots != ((chord *) tupleobj->object)->numdots))
                                {
                                  ishomogenoustuple = FALSE;
                                }
                              else
                                {
                                  prevduration = ((chord *) tupleobj->object)->baseduration;
                                  prevnumdots = ((chord *) tupleobj->object)->numdots;
                                }
                            }   /* End if ishomogenoustuple */
                        }       /* End if tuple object is a chord */
                    }           /* End for each object in tuple */

                  if (notesintuple == 3 && ishomogenoustuple && ((tupopen *) curobj->object)->numerator == 2 && ((tupopen *) curobj->object)->denominator == 3) /* a simple triplet */
                    fprintf (fp, "(3");
                  else
                    fprintf (fp, "(%d:%d:%d", ((tupopen *) curobj->object)->denominator, ((tupopen *) curobj->object)->numerator, notesintuple);
                  break;

                case GRACE_START:
                  fprintf (fp, "{");
                  break;

                case GRACE_END:
                  fprintf (fp, "}");
                  break;

                case TUPCLOSE:
                case STEMDIRECTIVE:
                case DYNAMIC:
                  /* Do nothing at present.

                   * FIXME: I should probably do the !pp! thing for dynamics
                   *        at some point, when it becomes mainstream in ABC
                   *        parsers.
                   */
                  break;

                default:
                  /* Danger, Will Robinson! */
                  g_warning ("Warning: unknown DenemoObject type \"%d\" " "found, ignoring", curobj->type);
                  break;
                }               /* End switch on object type */
            }                   /* End for each note */

          /* Print out (e.g.) "x3" for an empty measure. */

          if (emptymeasure)
            fprintf (fp, "x%d ", curtime.a * (defaultlength / curtime.b));

          /* And finally, of course, the barline. */

          if (curmeasure->next)
            fprintf (fp, "|\n");
          else
            fprintf (fp, "|]\n");
        }                       /* End for each measure */

    }                           /* End for each staff (voice) */

  /* Clean up and go home. */

  fclose (fp);
  g_string_free (filename, FALSE);
}
