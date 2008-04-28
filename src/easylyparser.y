/* easylyparser.y
 * implements a parser and an ad-hoc lexer for the constrained
 * subset of mudela that Denemo produces */

/* (c) 2000 Matthew Hiller */
/* Tuplet code added by Yu Cheung "toby" Ho, adapted by Hiller */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include "calculatepositions.h"
#include "chordops.h"
#include "contexts.h"
#include "lyparserfuncs.h"
#include "measureops.h"
#include "objops.h"
#include "scoreops.h"
#include "staffops.h"
#include "tupletops.h"
#include "graceops.h"

int lylex();
gint lyerror(gchar *);

struct scoreinfo *lysi;
FILE *lyin;   /* Global filehandle */
mudelaobject *newmudelaobj = NULL;
struct twoints prevduration;
enum clefs currentclef;
staff *curstaffstruct;


%}

%union {
  gint a;
  gchar c;
  gchar *strval;
  struct twoints t;
};

%token <strval> STAFFBEGIN
%token STAFFEND
%token <t> TIMESIGTOK
%token <strval> KEYSIGTOK
%token <strval> CLEFTOK
%token <strval> INSTRUMENT
%token BEGINCHORD
%token ENDCHORD
%token <c> TONETYPE
%token <a> ENSHIFT
%token <a> OCTIND
%token <a> BASEDURATION
%token <a> NUMDOTS
%token MINOR
%token MAJOR
%token BLINE
%token <t> BEGINTUPLET
%token ENDTUPLET
%token <a> STEMDIRECTIVETOK
%token RELOADDIRECTIVE
%token <strval> DYNAMICTOK
%token SLUR_START
%token TIE
%token STARTGRACE
%token ENDGRACE
%token <strval> LYTONEOPTION
%left SLUR_END

%type <t> keysig 
%type <t> duration
%type <strval> instrument
%type <a> reloaddirective
%type <a> slur_begin_option
%type <a> slur_end_option
%type <a> tie_option
%type <strval> tone_option

/* Grammar follows */
%%

staffs: /* empty */
| staff staffs
;

staff: STAFFBEGIN reloaddirective instrument TIMESIGTOK keysig CLEFTOK {
  lysi->currentstaffnum++;
  newstaff (lysi, ADDFROMLOAD);
  lysi->currentstaff = g_list_last (lysi->thescore);
  curstaffstruct = (staff *)lysi->currentstaff->data;
  setstaffname (lysi, $1);
  if ($2)
    curstaffstruct->voicenumber = 2;
  setclef (lysi, $6);
  currentclef = (enum clefs)curstaffstruct->sclef;
  setkey (lysi, $5);
  settime (lysi, $4);
  find_leftmost_staffcontext ((staff *)lysi->currentstaff->data, lysi);
  g_string_assign (curstaffstruct->midi_instrument, $3);
  g_free ($3);
} measures
;

instrument: /* empty */ { $$ = g_strdup("acoustic grand") }
| INSTRUMENT { $$ = $1 }
;

reloaddirective: /* empty */ { $$ = FALSE }
| RELOADDIRECTIVE { $$ = TRUE }
;

measures: mudelaobjects BLINE {
  if (!lysi->currentmeasure->next)
    lysi->currentmeasure = addmeasures (lysi, lysi->currentmeasurenum, 1);
  else
    lysi->currentmeasure = lysi->currentmeasure->next;
  lysi->currentmeasurenum++;
} measures
| mudelaobjects STAFFEND   /* empty */
;

mudelaobjects:  /* empty */
| mudelaobject mudelaobjects
;

mudelaobject: chordandassoc
| CLEFTOK {
  currentclef = cleftypefromname ($1);
  newmudelaobj = newclefobj (currentclef);
  lysi->currentmeasure->data = g_list_append ((objnode *)lysi->currentmeasure->data,
					      newmudelaobj);
}
| keysig {
  if ($1.b)
    newmudelaobj = newkeyobj ($1.a - 3, $1.b);
  else
    newmudelaobj = newkeyobj ($1.a, $1.b);
  lysi->currentmeasure->data = g_list_append ((objnode *)lysi->currentmeasure->data,
					      newmudelaobj);
}
| TIMESIGTOK {
  newmudelaobj = newtimesigobj ($1.a, $1.b);
  lysi->currentmeasure->data = 
    g_list_append ((objnode *)lysi->currentmeasure->data,
		   newmudelaobj);
}
| tuplet
| grace
| STEMDIRECTIVETOK {
  newmudelaobj = stem_directive_new ((enum stemdirections)$1);
  lysi->currentmeasure->data = 
    g_list_append ((objnode *)lysi->currentmeasure->data,
		   newmudelaobj);
}
| DYNAMICTOK {
  GString *dynamic = g_string_new($1);

  mudelaobject *curobj = 
    (mudelaobject *) (lysi->currentobject ? lysi->currentobject->data : NULL);

  if(curobj->type == CHORD && curobj) {
    ((chord *)curobj->object)->dynamics = 
      g_list_append(((chord *)curobj->object)->dynamics, dynamic);
  }
  /*newmudelaobj = dynamic_new ($1);
  lysi->currentmeasure->data = 
    g_list_append ((objnode *)lysi->currentmeasure->data,
		   newmudelaobj);*/
  g_free ($1);
}
;

keysig: KEYSIGTOK MINOR { $$ = twointer (keynametonumber ($1), TRUE) }
| KEYSIGTOK  { $$ = twointer (keynametonumber ($1), FALSE) }  /* legacy */
| KEYSIGTOK MAJOR { $$ = twointer (keynametonumber ($1), FALSE) }
;

chordandassoc: slur_end_option chord slur_begin_option tie_option 
{
  ((chord *)newmudelaobj->object)->slur_end_p = $1;
  ((chord *)newmudelaobj->object)->slur_begin_p = $3;
  ((chord *)newmudelaobj->object)->is_tied = $4;

}
;

slur_end_option: /* empty */ { $$ = FALSE }
| SLUR_END { $$ = TRUE }
;

slur_begin_option: /* empty */ { $$ = FALSE }
| SLUR_START { $$ = TRUE }
;

tie_option: /* empty */ { $$ = FALSE }
| TIE { $$ = TRUE }
;

chord: BEGINCHORD {
  newmudelaobj = newchord (0, 0);
  lysi->currentmeasure->data = g_list_append ((objnode *)lysi->currentmeasure->data,
					      newmudelaobj);
} tones ENDCHORD
| {
  newmudelaobj = newchord(0, 0);
  lysi->currentmeasure->data = g_list_append ((objnode *)lysi->currentmeasure->data,
					      newmudelaobj);

} tone  

;

tones:  /* empty */
| tone tones
;

tone: TONETYPE ENSHIFT OCTIND duration tone_option {
  addtonewrapper (newmudelaobj, $1, $2, $3, $4, currentclef);
  set_tone_option(newmudelaobj,$5);
  prevduration = $4;
}
| TONETYPE OCTIND duration tone_option{
  addtonewrapper (newmudelaobj, $1, 0, $2, $3, currentclef);
  set_tone_option(newmudelaobj,$4);
  prevduration = $3;
}
| TONETYPE ENSHIFT duration tone_option{
  addtonewrapper (newmudelaobj, $1, $2, 0, $3, currentclef);
  set_tone_option(newmudelaobj,$4);
  prevduration = $3;
}
| TONETYPE ENSHIFT OCTIND tone_option {
  addtonewrapper (newmudelaobj, $1, $2, $3, prevduration, currentclef);
  set_tone_option(newmudelaobj,$4);
}
| TONETYPE ENSHIFT tone_option {
  addtonewrapper (newmudelaobj, $1, $2, 0, prevduration, currentclef);
  set_tone_option(newmudelaobj,$3);
}
| TONETYPE OCTIND tone_option {
  addtonewrapper (newmudelaobj, $1, 0, $2, prevduration, currentclef);
  set_tone_option(newmudelaobj,$3);
}
| TONETYPE duration tone_option {
  addtonewrapper (newmudelaobj, $1, 0, 0, $2, currentclef);
  set_tone_option(newmudelaobj,$3);

  prevduration = $2;
}
| TONETYPE tone_option {
  addtonewrapper (newmudelaobj, $1, 0, 0, prevduration, currentclef);
  set_tone_option(newmudelaobj,$2);
}
;

tone_option: /* empty */ { $$ = ""; }
| LYTONEOPTION {
  $$ = $1;
}

;

duration: BASEDURATION NUMDOTS {
  $$ = twointer (mutointernalduration ($1), $2);
}
| BASEDURATION {
  $$ = twointer (mutointernalduration ($1), 0);
}
;

tuplet: BEGINTUPLET {
  newmudelaobj = newtupopen ($1.a, $1.b);
  lysi->currentmeasure->data = g_list_append ((objnode *)lysi->currentmeasure->data,
					      newmudelaobj);
} mudelaobjects ENDTUPLET {
  newmudelaobj = newtupclose ();
  lysi->currentmeasure->data = g_list_append ((objnode *)lysi->currentmeasure->data,
					      newmudelaobj);
}
;

grace: STARTGRACE {
  newmudelaobj = newgracestart();
  lysi->currentmeasure->data = g_list_append ((objnode *)lysi->currentmeasure->data,
					      newmudelaobj);
} mudelaobjects ENDGRACE {
  newmudelaobj = newgraceend();
  lysi->currentmeasure->data = g_list_append ((objnode *)lysi->currentmeasure->data,
					      newmudelaobj);
}

%%

/* Both of the otherwise infinite loops in these functions are exited when the
 * lylex returns (i.e., my ad-hoc lexer finds a token) */

int
lyinput (gchar *filename, struct scoreinfo *si)
{
  if ((lyin = fopen (filename, "r")) == NULL) {
    fprintf(stderr, "Cannot open the file: %s\n", filename);
    return -1;
  }
  else {
    lysi = si;
    free_score (lysi);
    lysi->currentstaffnum = 0;
    while (!feof (lyin)) {
      lyparse ();
    }
    fclose (lyin);
    return 0;
  }
}

/* This is an ad-hoc lexer. It really needs to be rewritten in
 * some other way - probably with the help of flex, though I
 * was hoping to avoid this. The problem is it's very much a house
 * of cards right now, and doesn't handle standard mudela block-comments
 * correctly (though it does one-liners properly) */

int
lylex ()
{
  gchar scannedchars[2];
  gchar lotsachars[100];
  gchar thrownaway[5];  /* Nothing is done with the values assigned to
		       * this variable - it's just used to get the
		       * desired return value out of scanf */
  gchar getced;
  static gboolean instaff = FALSE;
  static gboolean tuplet_mode = FALSE;
#if DEBUG
  g_print("file position %ld\n",ftell(lyin));
#endif
  if ((getced = getc (lyin)) == EOF)
    return 0;
  else
    ungetc (getced, lyin);
  if (!instaff)
    {
      while ((getced = getc (lyin)) != EOF)
	{
	  if (getced == '%')  /* a comment */
	    while ((getced = getc (lyin)) != '\n' && getced != EOF)
	      ;
	  else
	    {
	      ungetc (getced, lyin);
	      if (fscanf (lyin, "%s = \\notes%*[ 0-9A-Za-z=$\\]%1[{]",
			  lotsachars, thrownaway) > 1)
		/* That fscanf is really convoluted so that it'll
		 * match both pre- and post-0.5.3 staffbegins */
		{
		  instaff = TRUE;
		  lylval.strval = g_strdup(lotsachars);
#ifdef DEBUG
		  g_print ("found staffbegin\n");
#endif
		  return STAFFBEGIN;
		}
	      if (fscanf (lyin, "\\tempo %d = %d", &lylval.t.a, &lylval.t.b)
		  == 2)
		{
#ifdef DEBUG
		  g_print ("found tempo\n");
#endif
		  lysi->tempo = lylval.t.b;
		}
	    }
	}
      return 0;
    }   /* End if !instaff*/
  else /* instaff */
    {
      /* The following fscanfs have more hacks in them than you can
       * shake a stick at */

      /* The main hack to notice is that no two scanfs executed one after
       * the other will have a common prefix. This includes leading
       * whitespace */
      while ((getced = getc (lyin)) != EOF)
	{
	  ungetc (getced, lyin);
#if DEBUG
  g_print("first char %c\n",getced);
#endif

	  if (fscanf (lyin, "i%1[s]", thrownaway))
	    {   /* sharp */
#ifdef DEBUG
	      g_print ("found enshift\n");
#endif
	      if (fscanf(lyin, "i%1[s]", thrownaway))  /* Double sharp */
		lylval.a = 2;
	      else
		lylval.a = 1;
	      return ENSHIFT;
	    }
	  if (fscanf (lyin, "%1[e]", scannedchars))
	    {  /* either an e or a flat */
	      if (fscanf (lyin, "%1[s]", thrownaway))
		{   /* flat */
#ifdef DEBUG
		  g_print ("found enshift\n");
#endif
		  if (fscanf (lyin, "e%1[s]", thrownaway))  /* Double flat */
		    lylval.a = -2;
		  else
		    lylval.a = -1;
		  return ENSHIFT;
		}
	      else
		{
#ifdef DEBUG
		  g_print ("found tonetype\n");
#endif
		  lylval.c = scannedchars[0];
		  return TONETYPE;
		}
	    }
	  else if (fscanf (lyin, "%[\']%n", thrownaway, &lylval.a))
	    {
#ifdef DEBUG
	      g_print ("found octind\n");	
#endif
	      return OCTIND;
	    }
	  else if (fscanf (lyin, "%[,]%n", thrownaway, &lylval.a))
	    {
#ifdef DEBUG
	      g_print ("found octind\n");
#endif
	      lylval.a = -lylval.a;
	      return OCTIND;
	    }
	  else if (fscanf (lyin, "%[0-9]", lotsachars))
	    {
#ifdef DEBUG
	      g_print("found baseduration\n");
#endif
	      lylval.a = atoi (lotsachars);
	      return BASEDURATION;
	    }
	  else if (fscanf (lyin, "%[.]%n", thrownaway, &lylval.a))
	    {
#ifdef DEBUG
	      g_print ("found numdots\n");
#endif
	      return NUMDOTS;
	    }

	  /* Note that the next fscanf will eat up any leading whitespace that
	   * there might have been; all the fscanf's preceding this one
	   * were specifically looking for an absence of leading whitespace */
	  else if (fscanf (lyin, " %1[\\]", thrownaway))
	    {
	      if (fscanf (lyin, "tim%1[e]", thrownaway))
		{
		  if (fscanf (lyin, " %d/%d", &lylval.t.a, &lylval.t.b)
		      == 2) {
#ifdef DEBUG
		    g_print ("found timesig\n");
#endif
		    return TIMESIGTOK;
		  }
		  else if (fscanf (lyin, "s %d/%d", &lylval.t.a, &lylval.t.b)
			   == 2)
		    {
#ifdef DEBUG
		      g_print ("found tupopen\n");
#endif
		      tuplet_mode = TRUE;
		      return BEGINTUPLET;
		    }
		}
	      else if (fscanf (lyin, "grace {"))
		{
/*#ifdef DEBUG*/
		  g_print ("found grace\n");
/*#endif*/
		  return STARTGRACE;
		}
	      else if (fscanf (lyin, "key %[a-gis]", lotsachars))
		{
#ifdef DEBUG
		  g_print ("found keysig\n");
#endif
		  lylval.strval = g_strdup(lotsachars);
		  return KEYSIGTOK;
		}
	      else if (fscanf (lyin, "%1[m]", thrownaway))
		{
		  if (fscanf (lyin, "ino%1[r]", thrownaway))
		    {
#ifdef DEBUG
		      g_print ("found minor\n");
#endif
		      return MINOR;
		    }
		  else if (fscanf (lyin, "ajo%1[r]", thrownaway))
		    {
#ifdef DEBUG
		      g_print ("found major\n");
#endif
		      return MAJOR;
		    }
		}
	      else if (fscanf (lyin, "clef %[^\n]", lotsachars))
		{
#ifdef DEBUG
		  g_print ("found clef\n");
#endif
		  lylval.strval = g_strdup (lotsachars);
		  return CLEFTOK;
		}
	      else if (fscanf (lyin, "propert%1[y] ", thrownaway))
		{
		  if (fscanf (lyin, "Staff.midiInstrument = \"%[^\"]\"",
			      lotsachars))
		    {
#ifdef DEBUG
		      g_print ("found instrument\n");
#endif
		      lylval.strval = g_strdup (lotsachars);
		      return INSTRUMENT;
		    }
		  else if (fscanf (lyin, "Voice.noteHeadStyle = #\'%s",
				   lotsachars))
		    {
		      /* Recognize this, but don't return anything;
		       * this is just a placeholder. */
#ifdef DEBUG
		      g_print ("found noteheadstyle directive\n");
#endif
		    }
		}
	      else if (fscanf (lyin, "stem%s", lotsachars))
		{
#ifdef DEBUG
		  g_print ("found stemming directive");
#endif
		  if (g_strcasecmp (lotsachars, "down") == 0)
		    lylval.a = DENEMO_STEMDOWN;
		  else if (g_strcasecmp (lotsachars, "up") == 0)
		    lylval.a = DENEMO_STEMUP;
		  else
		    lylval.a = DENEMO_STEMBOTH;
		  return STEMDIRECTIVETOK;
		}
	      else if (fscanf(lyin, "bar \"|.%1[\"]", thrownaway))
		{
#ifdef DEBUG
		  g_print ("found staffend\n");
#endif
		  instaff = FALSE;
		  return STAFFEND;
		}
	    }
	  else if (fscanf (lyin, "s1*%d/%d",  &lylval.t.a, &lylval.t.b) == 2)
	    {
	      /* ignore full measure rests */
	    }
	  else if (fscanf (lyin, "%1[<]", thrownaway))
	    {
#ifdef DEBUG
	      g_print("found beginchord\n");	
#endif
	      return BEGINCHORD;
	    }
	  else if (fscanf (lyin, "%1[>]", thrownaway))
	    {
#ifdef DEBUG
	      g_print ("found endchord \n");
#endif
	      return ENDCHORD;
	    }
	  else if (fscanf (lyin, "%1[a-gr]", scannedchars))
	    {
#ifdef DEBUG
	      g_print ("found tonetype\n");	
#endif
	      lylval.c = scannedchars[0];
	      return TONETYPE;
	    }
	  else if (fscanf (lyin, "%1[~]", thrownaway))
	    {  /* Tie indicator */
#ifdef DEBUG
	      g_print ("found tie\n");
#endif
	      return TIE;
	    }
	  else if (fscanf(lyin, "%1[(]", thrownaway))
	    {  /* Slur indicator */
#ifdef DEBUG
	      g_print ("found slur\n");
#endif
	      return SLUR_START;
	    }
	  else if (fscanf(lyin, "%1[)]", thrownaway))
	    {  /* Slur indicator */
#ifdef DEBUG
	      g_print ("found end slur\n");
#endif
	      return SLUR_END;
	    }
	  else if (fscanf(lyin, "%1[|]", thrownaway))
	    {  /* Barline marker */
#ifdef DEBUG
	      g_print ("found barline\n");
#endif
	      return BLINE;
	    }
	  else if (fscanf (lyin, "%1[}]", thrownaway))
	    {  /* Close bracket - end of tuplet group */

/*#ifdef DEBUG*/
	      if(tuplet_mode)
		g_print ("found tupclose\n");
	      else
		g_print ("found grace end\n"); 
/*#endif*/
	      if(tuplet_mode)
		{
		  tuplet_mode = FALSE;
		  return ENDTUPLET;
		}
	      else
		return ENDGRACE;
	    }
	  else if (fscanf (lyin, "-\\%[A-Za-z]", lotsachars))
	    {
	      if(lotsachars[0] == 'p' ||
		 !strcmp(lotsachars,"mf") ||
		 !strcmp(lotsachars,"mfp") ||
		 !strcmp(lotsachars,"mp") ||
		 !strcmp(lotsachars, "f") ||
		 !strcmp(lotsachars, "ff") ||
		 !strcmp(lotsachars, "fff") ||
		 !strcmp(lotsachars, "sf") ||
		 !strcmp(lotsachars, "sfp") ||
		 !strcmp(lotsachars, "sfz") ||
		 !strcmp(lotsachars, "cr") ||
		 !strcmp(lotsachars, "rc") ||
		 !strcmp(lotsachars, "dr") ||
		 !strcmp(lotsachars, "rd") )
		{
		  lylval.strval = g_strdup (lotsachars);
#ifdef DEBUG
		  g_print ("found dynamic %s\n", lotsachars);
#endif
		  return DYNAMICTOK;
		  
		}
	      else
		{
		  lylval.strval = g_strdup(lotsachars);
#ifdef DEBUG
		  g_print ("found lytoneoption %s\n", lotsachars);
#endif
		  return LYTONEOPTION;
		}
	    }
	  else
	    {
	      getced = fgetc (lyin);
	      /* Shift to the next character and hope it's more interesting */
	      if (getced == '%')
		{  /* a comment */
		  getced = getc (lyin);
		  if (getced == '!')
		    {
#ifdef DEBUG
		      g_print ("found reloaddirective\n");
#endif
		      while ((getced = getc (lyin)) != '\n' && getced != EOF)
			;
		      return RELOADDIRECTIVE;
		    }
		  else
		    {
		      ungetc (getced, lyin);
		      while ((getced = getc (lyin)) != '\n' && getced != EOF)
			;
		    }
		}
	    }
	}
      return 0;
    }    /* End else */
}

gint lyerror (char *s)
{
  fprintf (stderr, "%s\n", s);
  return 1;
}
