/*
 * frogio.cpp
 *
 * Functions saving the datastructures in the 
 * jtf file format
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000, 2001, 2002 Adam Tee 
 *
 *
 */
#include <stdio.h>
#include <string.h>
#include "frogio.h"
#include "dialogs.h"
#include "exportlilypond.h"
#include "staffops.h"
#include "utils.h"
#include "file.h"

/**
 * Saves the given selection in the jtf format
 *
 */
int
filesaveselection (gchar * file, DenemoScore * si)
{
  FILE *fp = NULL;
  objnode *obj;
  DenemoObject *item;
  gint i, k, bar;
  GList *temp;
  struct durations beat;

  if ((fp = fopen ((char *) file, "w")) == NULL)
    {
      g_print (_("Cannot open file %s"), file);
      return 1;
    }
  fprintf (fp, "{\n[\n");
  fprintf (fp, "( %s %d %d %d\n",
	   ((DenemoStaff *) si->currentstaff->data)->lily_name->str,
	   ((DenemoStaff *) si->currentstaff->data)->no_of_lines,
	   ((DenemoStaff *) si->currentstaff->data)->pos_in_half_lines,
	   ((DenemoStaff *) si->currentstaff->data)->transposition);
  saveclef (fp, ((DenemoStaff *) si->currentstaff->data)->sclef);
  savekey (fp, ((DenemoStaff *) si->currentstaff->data)->skey,
	   ((DenemoStaff *) si->currentstaff->data)->skey_isminor, 1, 1.0);

  /*Save the Time Signautre */
  fprintf (fp, "1 1.0 0.0,0.0 %d/%d\n",
	   ((DenemoStaff *) si->currentstaff->data)->stime1,
	   ((DenemoStaff *) si->currentstaff->data)->stime2);
  beat.beat = 1.0;
  beat.prevduration = 0;
  bar = 1;
  k = 1;
  for (temp = si->savebuffer, i = 0; temp; temp = temp->next)
    {
      for (obj = (objnode *) temp->data; obj; obj = obj->next)
	{

	  item = (DenemoObject *) obj->data;

	  beat =
	    writeobjects (fp, (DenemoStaff *) si->currentstaff->data, item,
			  bar, k, &beat);
	  if (beat.beat >
	      ((DenemoStaff *) si->currentstaff->data)->stime1 + 1)
	    {
	      bar++;
	      beat.beat = 1;
	    }
	}
    }
  fprintf (fp, ")\n");
  fprintf (fp, "]\n}\n");
  fclose (fp);
  return 0;
}



/**
 * File save function - this traverses the list and 
 * writes the correct string to the file.  It uses a 
 * similar hierarchy to the drawutils functions in that 
 * it goes through the score a staff at a time.  Then 
 * a staff is traversed a measure at a time, then a measure 
 * is travesed a beat at a time.
 *
 */
int
filesave (gchar * file, DenemoScore * si, gint start, gint end,
	  gint polyphony)
{
  FILE *fp;
  staffnode *curstaff;
  DenemoStaff *curstaffstruct, *extravoice = NULL;
  gint i;


  if ((fp = fopen ((char *) file, "w")) == NULL)
    {
      g_print (_("Cannot open file %s"), file);
      return 1;
    }
  fprintf (fp, "{\n[\n");


  if (polyphony)
    {
      for (curstaff = si->thescore, i = 0; curstaff;
	   curstaff = curstaff->next, i++)
	{
	  curstaffstruct = (DenemoStaff *) curstaff->data;
	  if (curstaff->next)
	    {
	      extravoice = (DenemoStaff *) curstaff->next->data;
	      if (extravoice->voicenumber != 2)
		extravoice = NULL;
	    }
	  if (curstaffstruct->voicenumber == 1)
	    fprintf (fp, "( %s %d %d %d\n",
		     curstaffstruct->lily_name->str,
		     curstaffstruct->no_of_lines,
		     curstaffstruct->pos_in_half_lines,
		     curstaffstruct->transposition);
	  if (extravoice && extravoice->voicenumber <= 2)
	    {
	      writepolyphony (fp, curstaffstruct, extravoice);
	    }
	  else
	    writesinglestave (fp, curstaffstruct, start, end);

	  fprintf (fp, ")\n\n");

	  if (extravoice)
	    curstaff = curstaff->next;
	}
    }
  else
    {
      for (curstaff = si->thescore, i = 0; curstaff;
	   curstaff = curstaff->next, i++)
	{
	  curstaffstruct = (DenemoStaff *) curstaff->data;
	  fprintf (fp, "( %s %d %d %d\n",
		   curstaffstruct->lily_name->str,
		   curstaffstruct->no_of_lines,
		   curstaffstruct->pos_in_half_lines,
		   curstaffstruct->transposition);
	  writesinglestave (fp, curstaffstruct, start, end);
	  fprintf (fp, ")\n\n");
	}
    }
  fprintf (fp, "]\n}\n");
  fclose (fp);
  return 0;
}

/**
 * Writes polyphonic music into one staff as opposed to the datastructures
 * multi stave representation
 */
void
writepolyphony (FILE * fp, DenemoStaff * curstaffstruct,
		DenemoStaff * extravoice)
{
  measurenode *curmeasure, *curmeasure2;
  objnode *curobj, *curobj2;
  DenemoObject *mudelaitem = NULL, *mudelaitem2 = NULL;
  struct durations beat, beat2;
  gint bar, k;


  for (curmeasure = (measurenode *) curstaffstruct->measures, bar =
       1, curmeasure2 = (measurenode *) extravoice->measures; curmeasure;
       curmeasure = curmeasure->next, curmeasure2 = curmeasure2->next, bar++)
    {
      if (bar == 1)
	{

	  saveclef (fp, curstaffstruct->sclef);

	  savekey (fp, curstaffstruct->skey, curstaffstruct->skey_isminor, 1,
		   1.0);

	  /*Save the Time Signautre */
	  fprintf (fp, "1 1.0 0.0,0.0 %d/%d\n",
		   curstaffstruct->stime1, curstaffstruct->stime2);
	}
      beat.beat = 1;
      beat.prevduration = 0;
      beat2.beat = 1;
      beat2.prevduration = 0;
      curobj = (objnode *) curmeasure->data;
      curobj2 = (objnode *) curmeasure2->data;
      k = 1;
      while ((curobj || curobj2))
	{
	  if (curobj)
	    mudelaitem = (DenemoObject *) curobj->data;
	  if (curobj2)
	    mudelaitem2 = (DenemoObject *) curobj2->data;
	  if (beat.beat < beat2.beat)
	    {
	      beat =
		writeobjects (fp, curstaffstruct, mudelaitem, bar, k, &beat);
	      beat2 =
		writeobjects (fp, extravoice, mudelaitem2, bar, k, &beat2);
	    }
	  else if (beat.beat > beat2.beat)
	    {
	      beat2 =
		writeobjects (fp, extravoice, mudelaitem2, bar, k, &beat2);
	      beat =
		writeobjects (fp, curstaffstruct, mudelaitem, bar, k, &beat);
	    }
	  else
	    {
	      beat =
		writeobjects (fp, curstaffstruct, mudelaitem, bar, k, &beat);
	      beat2 =
		writeobjects (fp, extravoice, mudelaitem2, bar, k, &beat2);
	    }
	  if (curobj)
	    {
	      if (curobj->next)
		curobj = curobj->next;
	      else
		curobj = NULL;
	    }
	  if (curobj2)
	    {
	      if (curobj2->next)
		curobj2 = curobj2->next;
	      else
		curobj2 = NULL;
	    }
	  k++;
	}
    }
}

struct durations
writeobjects (FILE * fp, DenemoStaff * curstaffstruct,
	      DenemoObject * mudelaitem, gint bar, gint k,
	      struct durations *beat)
{

  GList *node;
  note *newnote;
  gchar *stemdir;
  static gboolean is_tup = FALSE;
  static gfloat numerator, denominator;
  gchar notename[3];
  gint octave;
  gchar output[150], temp[50];
  gfloat duration;
  GString *dynamic = NULL;

  switch (mudelaitem->type)
    {
    case CHORD:
      /*
       * Found chord go through chord notes list outputting 
       * the data for the note
       */
      newnote = (note *) g_malloc (sizeof (note));
      node = ((chord *) mudelaitem->object)->notes;
      /*beat->beat = beat->beat + beat->prevduration; */
      duration = durationtofloat
	(((chord *) mudelaitem->object)->baseduration,
	 ((chord *) mudelaitem->object)->numdots);
      if (is_tup)
	{

	  beat->prevduration = (gfloat) (numerator / denominator) * duration;
	}
      else
	{
	  beat->prevduration = duration;
	}
      /* Dividing by 4 gives the correct beat increment for the 
       * given time signature */
      beat->prevduration =
	(curstaffstruct->stime2 * beat->prevduration) / 4.0;
      if (curstaffstruct->stime2 == 8 && !(curstaffstruct->stime1 % 3))
	{
	  beat->prevduration /= 3.0;
	}

      if (is_tup)
	duration = (numerator / denominator) * durationtofloat
	  (((chord *) mudelaitem->object)->baseduration,
	   ((chord *) mudelaitem->object)->numdots);

      if (node == NULL)
	{
	  if (((chord *) mudelaitem->object)->numdots == 0)
	    fprintf (fp, "%d %f 0.0,0.0 r %f\n", bar, beat->beat, duration);
	  else
	    fprintf (fp, "%d %f 0.0,0.0 r %d %f\n", bar, beat->beat,
		     ((chord *) mudelaitem->object)->numdots, duration);
	}
      for (; node; node = node->next)
	{
	  newnote = (note *) node->data;

	  if (((chord *) mudelaitem->object)->is_stemup == 1)
	    stemdir = "up";
	  else if (((chord *) mudelaitem->object)->is_stemup == 0)
	    stemdir = "down";
	  else
	    stemdir = "null";


	  octave = pitchtooctave (newnote->mid_c_offset);
	  pitchtonotename (newnote->mid_c_offset, newnote->enshift, notename);
	  sprintf (output, "%d %f 0.0,0.0 %s %d %s ", bar, beat->beat,
		   notename, octave, stemdir);
	  /*fprintf (stderr, "%s\n", output);
	     fprintf (stderr, "%s\t%d\n", notename, strlen (notename)); */

	  if (((chord *) mudelaitem->object)->is_tied)
	    {
	      strcat (output, "tie 0.0,0.0 ");
	    }
	  if (((chord *) mudelaitem->object)->hasanacc)
	    {
	      if (newnote->enshift == 1)
		{
		  strcat (output, "# 0.0,0.0 ");
		}
	      else if (newnote->enshift == 2)
		{
		  strcat (output, "## 0.0,0.0 ");
		}
	      else if (newnote->enshift == 0)
		{
		  strcat (output, "n 0.0,0.0 ");
		}
	      else if (newnote->enshift == -1)
		{
		  strcat (output, "b 0.0,0.0 ");
		}
	      else if (newnote->enshift == -2)
		{
		  strcat (output, "bb 0.0,0.0 ");
		}
	    }

	  if (((chord *) mudelaitem->object)->numdots > 0)
	    {
	      sprintf (temp, "%d 0.0,0.0 ",
		       ((chord *) mudelaitem->object)->numdots);
	      strcat (output, temp);
	    }
	  /*
	     if (((chord *) mudelaitem->object)->has_fermata_p)
	     strcat (output, "fermata 0.0,0.0 ");
	     if (((chord *) mudelaitem->object)->has_stacatto_p)
	     strcat (output, "staccato 0.0,0.0 ");
	     if (((chord *) mudelaitem->object)->has_staccatissimo_p)
	     strcat (output, "staccattissimo 0.0,0.0 ");
	     if (((chord *) mudelaitem->object)->is_accented_p)
	     strcat (output, "accent 0.0,0.0 ");
	     if (((chord *) mudelaitem->object)->has_marcato_p)
	     strcat (output, "marcato 0.0,0.0 ");
	     if (((chord *) mudelaitem->object)->has_tenuto_p)
	     strcat (output, "tenuto 0.0,0.0 ");
	     if (((chord *) mudelaitem->object)->has_trill_p)
	     strcat (output, "trill 0.0,0.0 ");
	     if (((chord *) mudelaitem->object)->has_turn_p)
	     strcat (output, "turn 0.0,0.0 ");
	     if (((chord *) mudelaitem->object)->has_mordent_p)
	     strcat (output, "mordent 0.0,0.0 ");
	     if (((chord *) mudelaitem->object)->has_ubow_p)
	     strcat (output, "ubow 0.0,0.0 ");
	     if (((chord *) mudelaitem->object)->has_dbow_p)
	     strcat (output, "dbow 0.0,0.0 ");
	   */
	  if (newnote->noteheadtype == DENEMO_CROSS_NOTEHEAD)
	    strcat (output, "cross 0.0,0.0 ");
	  else if (newnote->noteheadtype == DENEMO_DIAMOND_NOTEHEAD)
	    strcat (output, "diamond 0.0,0.0 ");
	  else if (newnote->noteheadtype == DENEMO_HARMONIC_NOTEHEAD)
	    strcat (output, "harmonic 0.0,0.0 ");

	  if (mudelaitem->isstart_beamgroup &&
	      (((chord *) mudelaitem->object)->baseduration > 2))
	    {
	      strcat (output, "right ");
	    }
	  else if (mudelaitem->isend_beamgroup &&
		   (((chord *) mudelaitem->object)->baseduration > 2))
	    {
	      strcat (output, "left ");
	    }
	  else
	    {
	      strcat (output, "null ");
	    }



	  /*duration =  durationtofloat
	     (((chord *)mudelaitem->object)->baseduration,
	     ((chord *)mudelaitem->object)->numdots);
	     if(is_tup)
	     sprintf(temp, "%f",duration * (numerator/denominator));
	     else */
	  sprintf (temp, "%f", duration);
	  strcat (output, temp);


	  fprintf (fp, "%s\n", output);

	  /*output slurs */
	  if (((chord *) mudelaitem->object)->slur_begin_p)
	    fprintf (fp, "%d %f 0.0,0.0 slur_begin 0.0,0.0 arc\n", bar,
		     beat->beat);
	  else if (((chord *) mudelaitem->object)->slur_end_p)
	    fprintf (fp, "%d %f 0.0,0.0 slur_end 0.0,0.0 arc\n", bar,
		     beat->beat);

	  /*output crescendo */
	  if (((chord *) mudelaitem->object)->crescendo_begin_p)
	    fprintf (fp, "%d %f 0.0,0.0 cresc_begin\n", bar, beat->beat);
	  else if (((chord *) mudelaitem->object)->crescendo_end_p)
	    fprintf (fp, "%d %f 0.0,0.0 cresc_end\n", bar, beat->beat);

	  /*output crescendo */
	  if (((chord *) mudelaitem->object)->diminuendo_begin_p)
	    fprintf (fp, "%d %f 0.0,0.0 dim_begin\n", bar, beat->beat);
	  else if (((chord *) mudelaitem->object)->diminuendo_end_p)
	    fprintf (fp, "%d %f 0.0,0.0 dim_end\n", bar, beat->beat);

	  /*output dynamics */
	  if (((chord *) mudelaitem->object)->dynamics)
	    {
	      dynamic =
		(GString *) ((chord *) mudelaitem->object)->dynamics->data;
	      if (!strcmp (dynamic->str, "f"))
		fprintf (fp, "%d %f 0.0,0.0 l\n", bar, beat->beat);
	      else if (!strcmp (dynamic->str, "ff"))
		fprintf (fp, "%d %f 0.0,0.0 ll\n", bar, beat->beat);
	      else if (!strcmp (dynamic->str, "fff"))
		fprintf (fp, "%d %f 0.0,0.0 lll\n", bar, beat->beat);
	      else
		fprintf (fp, "%d %f 0.0,0.0 %s\n", bar, beat->beat,
			 dynamic->str);
	    }
	  memset (temp, 0, sizeof (temp));
	  memset (output, 0, sizeof (output));


	}
      beat->beat = beat->beat + beat->prevduration;
      break;
      /*
         case REST:
         fprintf(fp,"%d %f 0.0,0.0 r %f\n", bar,(float)k,
         durationtofloat(((chord *)mudelaitem->object)->baseduration));
         break;
       */
    case CLEF:
      /*
       * Found Clef - check what type and then output the correct 
       * Clef
       *
       * If the clef is the initial clef the beat number is always 1
       */

      switch (((clef *) mudelaitem->object)->type)
	{
	case DENEMO_TREBLE_CLEF:
	  fprintf (fp, "%d %f 0.0,0.0 G\n", bar, beat->beat);
	  break;
	case DENEMO_ALTO_CLEF:
	  fprintf (fp, "%d %f 0.0,0.0 C\n", bar, beat->beat);
	  break;
	case DENEMO_BASS_CLEF:
	  fprintf (fp, "%d %f 0.0,0.0 F\n", bar, beat->beat);
	  break;
	case DENEMO_G_8_CLEF:
	  fprintf (fp, "%d %f 0.0,0.0 G8\n", bar, beat->beat);
	  break;
	case DENEMO_TENOR_CLEF:
	  fprintf (fp, "%d %f 0.0,0.0 tenor\n", bar, beat->beat);
	  break;
	case DENEMO_SOPRANO_CLEF:
	  fprintf (fp, "%d %f 0.0,0.0 soprano\n", bar, beat->beat);
	  break;
	default:
	  fprintf (fp, "%d %f 0.0,0.0 G\n", bar, beat->beat);
	  break;
	}



      break;
    case TIMESIG:
      /*
       * Found timesignature - again if initial beat number is always 1
       *
       */

      fprintf (fp, "%d %f 0.0,0.0 %d/%d\n",
	       bar, beat->beat, ((timesig *) mudelaitem->object)->time1,
	       ((timesig *) mudelaitem->object)->time2);
      k = 0;
      break;

    case KEYSIG:
      /*
       * Found keysignature - if initial beat is 1
       * If mudelaitem->u.keyval.number is positive then 
       * the key has sharps in it, if it is negative the 
       * key has flats in it, if 0 there is no key signature
       */
      savekey (fp, ((keysig *) mudelaitem->object)->number,
	       ((keysig *) mudelaitem->object)->isminor, bar, beat->beat);

      break;

    case TUPOPEN:
      /*  Hey Adam -- I broke this to get Denemo compiling properly */
      fprintf (fp, "%d %f  0.0,0.0 t %d %d\n", bar, beat->beat,
	       ((tupopen *) mudelaitem->object)->numerator,
	       ((tupopen *) mudelaitem->object)->denominator);
      is_tup = TRUE;
      numerator = ((tupopen *) mudelaitem->object)->numerator;
      denominator = ((tupopen *) mudelaitem->object)->denominator;
      break;
    case TUPCLOSE:
      is_tup = !is_tup;
      fprintf (fp, "%d %f 0.0,0.0 t end\n", bar, beat->beat);
      break;
    case DYNAMIC:
      /*if (!strcmp (((dynamic *)mudelaitem->object)->type->str, "f"))
         fprintf (fp, "%d %f 0.0,0.0 l\n", bar, beat->beat);
         else if (!strcmp (((dynamic *)mudelaitem->object)->type->str, "ff"))
         fprintf (fp, "%d %f 0.0,0.0 ll\n", bar, beat->beat);
         else if (!strcmp (((dynamic *)mudelaitem->object)->type->str, "fff"))
         fprintf (fp, "%d %f 0.0,0.0 lll\n", bar, beat->beat);
         else
         fprintf (fp, "%d %f 0.0,0.0 %s\n", bar, beat->beat,
         ((dynamic *)mudelaitem->object)->type->str); */
      break;

    /**
     *
     * The features below are not yet umplemented so nothing will 
     * happen at the present time.
     */
    case BARLINE:
      unimplemented ();
      break;
/*    case LILYDIRECTIVE:
      unimplemented ();
      break;
    case COMMENT:
      unimplemented ();
      break;*/
    case STEMDIRECTIVE:
      break;
    case MEASUREBREAK:
      break;
    case GRACE_START:
    case GRACE_END:
    case LYRIC:
    case FIGURE:
      break;


    }

  return *beat;
}


/**
 * Write data from datastructure to file
 *
 */
void
writesinglestave (FILE * fp, DenemoStaff * curstaffstruct, gint start,
		  gint end)
{
  measurenode *curmeasure;
  objnode *curobj;
  DenemoObject *mudelaitem;
  gint bar, k;
  struct durations beat;
/*  gint first, last;*/
  gint last;
  gint i = 0;

  curmeasure = (measurenode *) curstaffstruct->measures;

  if (!end)
    last = g_list_length (curmeasure);
  else
    last = end;			/* end is always sent as 0, so should never happen */

  if (start)
    curmeasure = g_list_nth (curmeasure, start - 1);



  for (i = MAX (start, 1), bar = 1;
       curmeasure && (i <= last); curmeasure = curmeasure->next, bar++, i++)
    {
      if (bar == 1)
	{

	  saveclef (fp, curstaffstruct->sclef);

	  savekey (fp, curstaffstruct->skey, curstaffstruct->skey_isminor, 1,
		   1.0);

	  /*Save the Time Signautre */
	  fprintf (fp, "1 1.0 0.0,0.0 %d/%d\n",
		   curstaffstruct->stime1, curstaffstruct->stime2);


	  k = 0;


	}
      beat.beat = 1;
      beat.prevduration = 0;
      for (curobj = (objnode *) curmeasure->data, k = 1; curobj;
	   curobj = curobj->next)
	{
	  mudelaitem = (DenemoObject *) curobj->data;
	  beat = writeobjects (fp, curstaffstruct, mudelaitem, bar, k, &beat);
	}
    }
}

/**
 * Print error message if the feature is not implemented 
 * at the present time.
 */
void
unimplemented ()
{
  /*fprintf (stderr, _("Feature not implemented\n")); */
}

/**
 * Convert Denemo duration into a floating point
 * equivalent for the frog file format
 */
gfloat
durationtofloat (gint duration, gint dots)
{
  gfloat value = 0;
  switch (duration)
    {
    case 0:
      value = 4.0;
      break;
    case 1:
      value = 2.0;
      break;
    case 2:
      value = 1.0;
      break;
    case 3:
      value = 0.5;
      break;
    case 4:
      value = 0.25;
      break;
    case 5:
      value = 0.125;
      break;
    case 6:
      value = 0.0625;
      break;
    default:
      value = 0.0;
      break;
    }


  switch (dots)
    {
    case 0:
      break;
    case 1:
      value = value + (value / 2);
      break;
    case 2:
      value = value + (value / 2) + (value / 4);
      break;
    case 3:
      value = value + (value / 2) + (value / 4) + (value / 8);
      break;
    case 4:
      value = value + (value / 2) + (value / 4) + (value / 8) + (value / 16);
      break;
    }

  return value;
}

/**
 * Convert Denemo offset/pitch to the correct octave spec 
 * for the Frog file format
 */
int
pitchtooctave (gint pitch)
{
  int octave = 4;
  /*fprintf (stderr, "%d\n", pitch); */
  if (pitch <= 27 && pitch >= 21)
    {
      octave = 7;
    }
  else if (pitch <= 20 && pitch >= 14)
    {
      octave = 6;
    }
  else if (pitch <= 13 && pitch >= 7)
    {
      octave = 5;
    }
  else if (pitch <= 6 && pitch >= 0)
    {
      octave = 4;
    }
  else if (pitch <= -1 && pitch >= -7)
    {
      octave = 3;
    }
  else if (pitch <= -8 && pitch >= -14)
    {
      octave = 2;
    }
  else if (pitch <= -15 && pitch >= -21)
    {
      octave = 1;
    }
  else if (pitch <= -22 && pitch >= -28)
    {
      octave = 0;
    }

  return octave;
}

/**
 * Convert Denemo pitch to notename
 * for the Frog file Format
 */
char *
pitchtonotename (gint pitch, gint enharmonic, char *note)
{
  /*char note[3]; */

  switch (offsettonumber (pitch))
    {
    case 0:
      strcpy (note, "c");
      break;
    case 1:
      strcpy (note, "d");
      break;
    case 2:
      strcpy (note, "e");
      break;
    case 3:
      strcpy (note, "f");
      break;
    case 4:
      strcpy (note, "g");
      break;
    case 5:
      strcpy (note, "a");
      break;
    case 6:
      strcpy (note, "b");
      break;
    }


  switch (enharmonic)
    {
    case 0:
      break;
    case 1:
      /*concat # with notename */
      strcat (note, "#");
      break;
    case 2:
      /*concat ## with notename */
      strcat (note, "##");
      break;
    case -1:
      /*concat b with notename */
      strcat (note, "b");
      break;
    case -2:
      /*concat bb with notename */
      strcat (note, "bb");
      break;
    }

  return note;
}

/**
 * Convert Frog floating point duration to 
 * Denemo duration
 */
gint
floattoduration (float duration, gboolean is_tup)
{

  if (duration >= 4 && duration < 8)
    {
      return 0;

    }
  else if (duration >= 2 && duration < 4)
    {
      return 1;

    }
  else if (duration >= 1 && duration < 2)
    {
      if (is_tup)
	return 1;
      else
	return 2;

    }
  else if (duration >= 0.5 && duration < 1)
    {
      if (is_tup)
	return 2;
      else
	return 3;
    }
  else if (duration >= 0.25 && duration < 0.5)
    {
      if (is_tup)
	return 3;
      else
	return 4;
    }
  else if (duration >= 0.125 && duration < 0.25)
    {
      if (is_tup)
	return 4;
      else
	return 5;
    }
  else if (duration >= 0.0625 && duration < 0.125)
    {
      if (is_tup)
	return 5;
      else
	return 6;
    }
  else if (duration >= 0.03125 && duration < 0.0625)
    {
      if (is_tup)
	return 6;
      else
	return 7;
    }
  else
    return 0;
}

/**
 * Convert pitch and octave to Denemo pitch
 */
gint
fetchnotename (char *notename, int octave)
{
  char note[3];
  gint mid_c_offset = 0;
  strncpy (note, notename, 3);
  if (note[0] == 'c')
    {
      mid_c_offset = 0;
    }
  else if (note[0] == 'd')
    {
      mid_c_offset = 1;
    }
  else if (note[0] == 'e')
    {
      mid_c_offset = 2;
    }
  else if (note[0] == 'f')
    {
      mid_c_offset = 3;
    }
  else if (note[0] == 'g')
    {
      mid_c_offset = 4;
    }
  else if (note[0] == 'a')
    {
      mid_c_offset = 5;
    }
  else if (note[0] == 'b')
    {
      mid_c_offset = 6;
    }
  if (octave == 0)
    {
      mid_c_offset -= 28;
    }
  else if (octave == 1)
    {
      mid_c_offset -= 21;
    }
  else if (octave == 2)
    {
      mid_c_offset -= 14;
    }
  else if (octave == 3)
    {
      mid_c_offset -= 7;
    }
  else if (octave == 4)
    {
      mid_c_offset += 0;
    }
  else if (octave == 5)
    {
      mid_c_offset += 7;
    }
  else if (octave == 6)
    {
      mid_c_offset += 14;
    }
  else if (octave == 7)
    {
      mid_c_offset += 21;
    }
  return mid_c_offset;
}

/**
 * Get the enharmonic change (#/b) for Denemo 
 */
gint
fetchenharmonic (char *notename)
{
  char note[3];
  gint enharmonic;
  strncpy (note, notename, 3);
  if (note[1] == '#')
    {
      enharmonic = 1;
    }
  else if (note[1] == 'b')
    {
      enharmonic = -1;
    }
  else if (note[1] == '#' && note[2] == '#')
    {
      enharmonic = 2;
    }
  else if (note[1] == 'b' && note[2] == 'b')
    {
      enharmonic = -2;
    }
  else
    enharmonic = 0;

  return enharmonic;
}

/**
 * Change Frog Clef Description to Denemo Clef
 */
enum clefs
cleftoenum (char *clefname)
{
  if (!strcmp (clefname, "G"))
    {
      return DENEMO_TREBLE_CLEF;
    }
  else if (!strcmp (clefname, "F"))
    {
      return DENEMO_BASS_CLEF;
    }
  else if (!strcmp (clefname, "C"))
    {
      return DENEMO_ALTO_CLEF;
    }
  else if (!strcmp (clefname, "G8"))
    {
      return DENEMO_G_8_CLEF;
    }
  else if (!strcmp (clefname, "tenor"))
    {
      return DENEMO_TENOR_CLEF;
    }
  else if (!strcmp (clefname, "soprano"))
    {
      return DENEMO_SOPRANO_CLEF;
    }
  /*default value */
  return DENEMO_TREBLE_CLEF;
}

/**
 * Convert Frog KeySig to Denemo Key Representation
 */
gint
Keytoint (char *keyname)
{
  gint key = 0;

  if (!strcmp (keyname, "Cmaj"))
    {
      key = 0;
    }
  else if (!strcmp (keyname, "Gmaj"))
    {
      key = 1;
    }
  else if (!strcmp (keyname, "Dmaj"))
    {
      key = 2;
    }
  else if (!strcmp (keyname, "Amaj"))
    {
      key = 3;
    }
  else if (!strcmp (keyname, "Emaj"))
    {
      key = 4;
    }
  else if (!strcmp (keyname, "Bmaj"))
    {
      key = 5;
    }
  else if (!strcmp (keyname, "F#maj"))
    {
      key = 6;
    }
  else if (!strcmp (keyname, "C#maj"))
    {
      key = 7;
    }
  else if (!strcmp (keyname, "Fmaj"))
    {
      key = -1;
    }
  else if (!strcmp (keyname, "Bbmaj"))
    {
      key = -2;
    }
  else if (!strcmp (keyname, "Ebmaj"))
    {
      key = -3;
    }
  else if (!strcmp (keyname, "Abmaj"))
    {
      key = -4;
    }
  else if (!strcmp (keyname, "Dbmaj"))
    {
      key = -5;
    }
  else if (!strcmp (keyname, "Gbmaj"))
    {
      key = -6;
    }
  else if (!strcmp (keyname, "Cbmaj"))
    {
      key = -7;
    }
  else if (!strcmp (keyname, "Amin"))
    {
      key = 0;
    }
  else if (!strcmp (keyname, "Dmin"))
    {
      key = -1;
    }
  else if (!strcmp (keyname, "Gmin"))
    {
      key = -2;
    }
  else if (!strcmp (keyname, "Cmin"))
    {
      key = -3;
    }
  else if (!strcmp (keyname, "Fmin"))
    {
      key = -4;
    }
  else if (!strcmp (keyname, "Bbmin"))
    {
      key = -5;
    }
  else if (!strcmp (keyname, "Ebmin"))
    {
      key = -6;
    }
  else if (!strcmp (keyname, "Abmin"))
    {
      key = -7;
    }
  else if (!strcmp (keyname, "Emin"))
    {
      key = 1;
    }
  else if (!strcmp (keyname, "Bmin"))
    {
      key = 2;
    }
  else if (!strcmp (keyname, "F#min"))
    {
      key = 3;
    }
  else if (!strcmp (keyname, "C#min"))
    {
      key = 4;
    }
  else if (!strcmp (keyname, "G#min"))
    {
      key = 5;
    }
  else if (!strcmp (keyname, "D#min"))
    {
      key = 6;
    }
  else if (!strcmp (keyname, "A#min"))
    {
      key = 7;
    }
  return key;
}

/**
 * Convert Denemo Clef to Frog Clef and 
 * Write to the file
 */
void
saveclef (FILE * fp, gint clef)
{
  switch (clef)
    {
    case DENEMO_TREBLE_CLEF:
      fprintf (fp, "1 1.0 0.0,0.0 G\n");
      break;
    case DENEMO_ALTO_CLEF:
      fprintf (fp, "1 1.0 0.0,0.0 C\n");
      break;
    case DENEMO_BASS_CLEF:
      fprintf (fp, "1 1.0 0.0,0.0 F\n");
      break;
    case DENEMO_G_8_CLEF:
      fprintf (fp, "1 1.0 0.0,0.0 G8\n");
      break;
    case DENEMO_TENOR_CLEF:
      fprintf (fp, "1 1.0 0.0,0.0 tenor\n");
      break;
    case DENEMO_SOPRANO_CLEF:
      fprintf (fp, "1 1.0 0.0,0.0 soprano\n");
      break;
    default:
      fprintf (fp, "1 1.0 0.0,0.0 G\n");
    }
}

/**
 * Convert Denemo Key to Frog Key and 
 * Write to the file
 */
void
savekey (FILE * fp, gint key, gboolean min, gint bar, gfloat beat)
{
  char keyname[10];
  gint keysig = key;

  if (min)
    keysig += 3;


  switch (keysig)
    {
    case -7:
      strcpy (keyname, "Cb");
      break;
    case -6:
      strcpy (keyname, "Gb");
      break;
    case -5:
      strcpy (keyname, "Db");
      break;
    case -4:
      strcpy (keyname, "Ab");
      break;
    case -3:
      strcpy (keyname, "Eb");
      break;
    case -2:
      strcpy (keyname, "Bb");
      break;
    case -1:
      strcpy (keyname, "F");
      break;
    case 0:
      strcpy (keyname, "C");
      break;
    case 1:
      strcpy (keyname, "G");
      break;
    case 2:
      strcpy (keyname, "D");
      break;
    case 3:
      strcpy (keyname, "A");
      break;
    case 4:
      strcpy (keyname, "E");
      break;
    case 5:
      strcpy (keyname, "B");
      break;
    case 6:
      strcpy (keyname, "F#");
      break;
    case 7:
      strcpy (keyname, "C#");
      break;
    default:
      strcpy (keyname, "C");
      break;

    }
  if (min)
    {
      strcat (keyname, "min");
    }
  else
    {
      strcat (keyname, "maj");
    }

  fprintf (fp, "%d %f 0.0,0.0 %s\n", bar, beat, keyname);


}
