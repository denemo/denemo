/**
 * exportcsound.c
 *
 * Functions for exporting in CSound Score format
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2002 Adam Tee
 */

#include "frogio.h"
#include "exportcsound.h"
#include "utils.h"

/**
 * Determine the current pitches name
 *
 */
gchar
pitch2char (int pitch)
{
  gchar note = 'c';
  switch (offsettonumber (pitch))
    {
    case 0:
      note = 'c';
      break;
    case 1:
      note = 'd';
      break;
    case 2:
      note = 'e';
      break;
    case 3:
      note = 'f';
      break;
    case 4:
      note = 'g';
      break;
    case 5:
      note = 'a';
      break;
    case 6:
      note = 'b';
      break;
    default:
      note = ' ';
      break;
    }

  return (note);
}


/**
 * Determine the current notes duration in seconds
 *
 */
float
duration2time (int duration, int dots, int tempo)
{
  float realtimeduration = 0.0;

  //realtimeduration = (durationtofloat (duration, dots) * 60) / tempo;
  realtimeduration = durationtofloat (duration, dots);
  return realtimeduration;
}

/**
 * Convert pitch to csound pitch
 *
 */
float
pitchtopch (gchar pitch, int enshift, int octave)
{
  float pch = 0.0;

  /*Set pitch value */
  if (pitch == 'c')
    pch = .00;
  else if (pitch == 'd')
    pch = .02;
  else if (pitch == 'e')
    pch = .04;
  else if (pitch == 'f')
    pch = .05;
  else if (pitch == 'g')
    pch = .07;
  else if (pitch == 'a')
    pch = .09;
  else if (pitch == 'b')
    pch = .11;
  /* Set enharmonic accidental */
  if (enshift == 1)
    pch += .01;
  else if (enshift == 2)
    pch += .02;
  else if (enshift == -1)
    pch -= .01;
  else if (enshift == -2)
    pch -= .02;
  /* Set octave */
  if (octave == 0)
    pch += 4.00;
  else if (octave == 1)
    pch += 5.00;
  else if (octave == 2)
    pch += 6.00;
  else if (octave == 3)
    pch += 7.00;
  else if (octave == 4)
    pch += 8.00;
  else if (octave == 5)
    pch += 9.00;
  else if (octave == 6)
    pch += 10.00;
  else if (octave == 7)
    pch += 11.00;



  return pch;
}

/**
 * Export csound score file
 *
 */
int
exportcsound (gchar * thefilename, DenemoScore * si, gint start, gint end)
{
  FILE *fp;
  staffnode *curstaff = NULL;
  DenemoStaff *curstaffstruct = NULL;
  gchar *locale;

  gint i = 0;

  if ((fp = fopen ((char *) thefilename, "w")) == NULL)
    {
      g_print (_("Cannot open file %s"), thefilename);
      return -1;
    }
  /* in France and some other Latin countries, printing a 
   * (float)x.y converts to an "x,y" string instead of the 
   * expected "x.y" that we want to print in the CSound score.
   * see "man 5 locale" and "man 3 setlocale"
   */
  locale = setlocale (LC_NUMERIC, NULL);
  setlocale (LC_NUMERIC, "C");

  fprintf (fp, "f 1 0 2048 10 1; A SINE WAVETABLE\n");

  fprintf (fp, "t0 %i; tempo\n", si->tempo);	/* The tempo csound will perfrom at */

  for (curstaff = si->thescore, i = 1; curstaff;
       curstaff = curstaff->next, i++)
    {
      curstaffstruct = (DenemoStaff *) curstaff->data;
      write_stave (fp, curstaffstruct, i, start, end, si);
      fprintf (fp, "\n");
    }
  fprintf (fp, "e");
  fclose (fp);
  setlocale (LC_NUMERIC, locale);
  return 0;
}

/**
 * Write a specfic staff to the file
 * only interested in the chord objects
 */
int
write_stave (FILE * fp, DenemoStaff * curstaffstruct, gint i, gint start,
	     gint end, DenemoScore * si)
{
  measurenode *curmeasure;
  objnode *curobj;
  DenemoObject *mudelaitem;
  float current_time = 0.0;
  float duration = 0.0;		//, total_duration;
  float prevdur = 0.0;
  GList *node = NULL;
  gboolean is_tied = FALSE;

  fprintf (fp, ";%s\n", curstaffstruct->lily_name->str);	/*place the staff name on top of staff's data */
  for (curmeasure = (measurenode *) curstaffstruct->measures;
       curmeasure; curmeasure = curmeasure->next)
    {
      for (curobj = (objnode *) curmeasure->data; curobj;
	   curobj = curobj->next)
	{
	  mudelaitem = (DenemoObject *) curobj->data;
	  note *newnote = NULL;
	  switch (mudelaitem->type)
	    {
	    case CHORD:

	      node = ((chord *) mudelaitem->object)->notes;


	      duration =
		duration2time (((chord *) mudelaitem->object)->baseduration,
			       ((chord *) mudelaitem->object)->numdots,
			       si->tempo);

	      /*if(((chord *)mudelaitem->object)->has_stacatto_p)
	         duration = total_duration * 0.5;
	         else if(((chord *)mudelaitem->object)->has_tenuto_p)
	         duration = total_duration;
	         else 
	         duration = total_duration * 0.75; */

	      if (((chord *) mudelaitem->object)->is_tied)
		{
		  is_tied = TRUE;
		  prevdur += duration;
		}
	      else
		{
		  if (node != NULL)
		    {
		      for (; node; node = node->next)
			{
			  newnote = (note *) node->data;
			  int octave = pitchtooctave (newnote->mid_c_offset);
			  float pitch =
			    pitchtopch (pitch2char (newnote->mid_c_offset),
					newnote->enshift,
					octave);
			  if (is_tied)
			    {
			      fprintf (fp, "i %d %.4f %.3f 7000 %.2f\n", i,
				       current_time - prevdur,
				       ((duration + prevdur)), pitch);
			      //current_time += prevdur;
			      prevdur = 0.0;
			      is_tied = FALSE;
			    }
			  else
			    {
			      fprintf (fp, "i %d %.4f %.3f 8000 %.2f\n", i,
				       current_time, (duration), pitch);

			    }
			}
		    }
		  else
		    {
		      is_tied = FALSE;
		      prevdur = 0.0;

		    }
		}

	      current_time += duration;
	      break;
	    case TUPOPEN:
	    case TUPCLOSE:
	    case CLEF:
	    case TIMESIG:
	    case KEYSIG:
	    case BARLINE:
	      //            case LILYDIRECTIVE:
	      //            case COMMENT:
	    case STEMDIRECTIVE:
	    case MEASUREBREAK:
	    case DYNAMIC:
	    case GRACE_START:
	    case GRACE_END:
	    default:
	      break;
	    }
	}
      fprintf (fp, "\n");	/* place a new line between measures for readability */
    }
  return 0;
}
