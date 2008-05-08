%{
/*
	(c) 2000  Adam Tee  <eenajt@electeng.leeds.ac.uk>
	(c) 2000  University of Leeds 
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "frogdefs.h"
#include "redefs.h"
#include "contexts.h"
#include "file.h"
#include "frogio.h"
#include "draw.h"
#include "keyresponses.h"
#include "chordops.h"
#include "scoreops.h"
#include "staffops.h"
#include "dialogs.h"
#include "measureops.h"
#include "calculatepositions.h"
#include "objops.h"
#include "processstaffname.h"
#include "tupletops.h"
#include "dynamic.h"
#include "articulations.h"

#define yyin frogin
#define yytext frogtext
#define YYDEBUG 0
#define YYMAXDEPTH 100000
#define YYINITDEPTH 500
  objnode *currentobject;
  extern int errcount;

  static void yy_setscore (DenemoGUI *gui);
  static DenemoScore *frogsi;
  static DenemoGUI *froggui;

  extern int yylex (void);
  int yyerror (char *);

  static void
    newnoteobj (struct p_note *thenote, gfloat length, DenemoScore *si);

  static void newclef (char *clefname);

  static void newkeysigobj (char *keyname);

  static void newtimesig (int upper, int lower);

 // static void newdynamic (gchar * type);

  static enum headtype setnoteheadtype (gchar * type);
  optionlist *createoption(struct p_modifier option);
  void emptylist(optionlist *head) ;
  void insertoption(optionlist **header, optionlist *newitem);
%}

%union
{
  gfloat f;
  char string[FROG_MAX_TOKEN_STRING];
  int number;
  char character;
  struct p_pitch t_pitch;
  optionlist *t_option;
  struct p_note t_note;
  struct p_modifier t_modifier;
  struct p_position t_position;
  struct p_grace t_grace;
  struct p_beam t_beamd;
  struct p_beam_type t_beamtype;
  struct p_gracenoteoption t_graceoption;
  struct p_staff t_staff;
  struct p_rest t_rest;
  struct p_tupops t_tupops;
  struct p_slur t_slur;
  struct p_hairpin t_hairpin;
}

%token < string > STAFFNAME
%token < string > TIE
%token < number > END
%token < f > FLOAT
%token < string > NOTENAME
%token < f > DURATION
%token < character > RESTTYPE
%token < string > DIRECTION
%token < string > SLURPOSITION
%token < string > NUMPOSITION
%token < string > KEYTYPE
%token < number > NUM
%token < string > BRACKET
%token < string > CLEFTYPE
%token < string > ORNAMENT
%token < string > FULL
%token < string > PARTIAL_
%token < string > MODE
%token < string > BARTYPE
%token < string > TEMPOTERM
%token < string > TRANSTEMPO
%token < string > DYN
%token < string > ACCIDENTAL
%token < string > ACCENT
%token < string > NOTEHEAD
%token < string > STYLE
%token < string > CURVESHAPE
%token < character > SYSTEM
%token < character > STAFFGROUP
%token < character > STAFF 
%token < character > ENDSTAFFGROUP
%token < character > TUPLET
%token < string > TUP_END
%token < string > SLURTYPE
%token < string > HAIRPINTYPE
%type < number > additive
%type < number > octave
%type < string > directions
%type < number > no_of_lines
%type < number > position_in_half_lines
%type < number > transposition
%type < number > barnumber
%type < number > dot
%type < f > beatnumber
%type < t_pitch > pitch
%type < t_option > note_option
%type < t_note > note
%type < string > stem_dir
%type < string > flag
%type < number > number_of_half_lines
%type < t_modifier > modifier_note_type
%type < string > tie
%type < t_grace > grace_notes
%type < t_beamd > beam
%type < t_beamtype > beam_type
%type < string > primary
%type < string > secondary
%type < t_position > position
%type < t_graceoption > grace_note_option
%type < t_staff > staffparams
%type < t_rest > rest
%type < t_tupops > tuplet_option 
%type < t_slur > slur
%type < t_hairpin > hairpin

/*
 * Global variables for the new objects
 */
%{
  DenemoStaff *thestaffstruct = NULL;
  

  measurenode *themeasures = NULL;
  gint cleftype;
  static int counter = 0;
  static int n = 1;
  static int currentbar=1;
  static gfloat beatnum = 0;
  static gfloat totaldurs = 0;
  static gint staffnum = 0;
  gboolean is_tup = FALSE;
  static gfloat currentbeat = 0;
  gint no_of_options = 0;
%}

%%
system:symbol staffgroup;

symbol:SYSTEM
{
  fprintf (stderr, "Found %c \n", $1);
}

;

brace:BRACKET
{
 
}

|;

staffgroup:brace staffgroupsym staffgroup endstaffsym
  | staffspec | staffspec staffgroup endstaffsym;

staffgroupsym:STAFFGROUP
{

}

;

endstaffsym:ENDSTAFFGROUP
{

}

;

staffspec:staffsym staffparams musicalobject
{

  /*p_newstaff(frogsi, &$2); */
}

;



staffsym:STAFF
{

  frogsi->currentstaffnum++;
  newstaff (froggui, ADDFROMLOAD, DENEMO_NONE);
  frogsi->currentstaff = g_list_last (frogsi->thescore);
  n = 1;
  currentbar = 1;
  staffnum++;
  counter = 0;
  frogsi->cursor_x = 0;
  totaldurs = 0;
  beatnum = 0;
  currentbeat = 0;
}

;

staffparams:STAFFNAME no_of_lines position_in_half_lines transposition
{

  g_string_assign (((DenemoStaff *) frogsi->currentstaff->data)->lily_name, $1);
  set_denemo_name (((DenemoStaff *) frogsi->currentstaff->data)->lily_name,
		   ((DenemoStaff *) frogsi->currentstaff->data)->denemo_name);
  ((DenemoStaff *) frogsi->currentstaff->data)->no_of_lines = $2;
  ((DenemoStaff *) frogsi->currentstaff->data)->transposition = $4;
  ((DenemoStaff *) frogsi->currentstaff->data)->pos_in_half_lines = $3;


}

;

musicalobject:characteristics musicalobjects musicalobject |;

characteristics:barnumber beatnumber position
{
 
  if (n != $1)
    {
      currentbar = $1;
      if (!frogsi->currentmeasure->next)
	{
	  frogsi->currentmeasure = addmeasures
	    (frogsi, frogsi->currentmeasurenum, 1,1);
	}
      else
	{
	  frogsi->currentmeasure = frogsi->currentmeasure->next;
	}

      frogsi->currentmeasurenum++;
      frogsi->cursor_x = 0;
      
      totaldurs = 0;
      currentbeat = 0;
      beatnum = 0;
      
    }
  counter++;
  n = $1;
  currentbeat = $2;



}

;

barnumber:NUM
{
  $$ = $1;
}

;

beatnumber:FLOAT
{
  $$ = $1;
}

;

musicalobjects:durationobject | slur
{

  DenemoObject *obj = (DenemoObject *)
    (frogsi->currentobject ? frogsi->currentobject->data : NULL);
  printf ("Found Slur\n");
  if (obj && (obj->type == CHORD))
    {
      if (strcmp ($1.string, "slur_begin") == 0)
	{
	  ((chord *)obj->object)->slur_begin_p = 
	    !((chord *)obj->object)->slur_begin_p;
	  /*obj->u.chordval.slur_end_p == FALSE; */
	  printf ("Current start slur val: %d\n",
		  ((chord *)obj->object)->slur_begin_p);
	}
      else if (strcmp ($1.string, "slur_end") == 0)
	{
	  ((chord *)obj->object)->slur_end_p =
	    !((chord *)obj->object)->slur_end_p;
	  /*obj->u.chordval.slur_begin_p == FALSE; */
	  printf ("Current end slur val: %d\n", 
		  ((chord *)obj->object)->slur_end_p);
	}
    }
}
|CLEFTYPE
{
  newclef ($1);
}
|timesignature 
| BARTYPE { }
|DYN
{
  GString *dynamic = g_string_new($1);
  DenemoObject *obj = (DenemoObject *)
    (frogsi->currentobject ? frogsi->currentobject->data : NULL);
  add_dynamic(obj, dynamic);  
  
  
 /* newdynamic ($1);*/
}

|tempo 
| keysignature
| hairpin 
{
  DenemoObject *obj = (DenemoObject *)
    (frogsi->currentobject ? frogsi->currentobject->data : NULL);
  printf ("Found Hairpin\n");
  if (obj && (obj->type == CHORD))
    {
      if (strcmp ($1.string, "cresc_begin") == 0)
	((chord *)obj->object)->crescendo_begin_p = 
	  !((chord *)obj->object)->crescendo_begin_p;
      else if (strcmp ($1.string, "cresc_end") == 0)
	((chord *)obj->object)->crescendo_end_p = 
	  !((chord *)obj->object)->crescendo_end_p;
	 
      if (strcmp ($1.string, "dim_begin") == 0)
	((chord *)obj->object)->diminuendo_begin_p =
	  !((chord *)obj->object)->diminuendo_begin_p;
      else if (strcmp ($1.string, "dim_end") == 0)
	((chord *)obj->object)->diminuendo_end_p = 
	  !((chord *)obj->object)->diminuendo_end_p;
	
      
    }


};

position:FLOAT ',' FLOAT
{
  $$.xoff = $1;
  $$.yoff = $3;
  
}

;

durationobject:note FLOAT
{
  if (currentbeat == beatnum && n == currentbar)
    {
      totaldurs = beatnum;
    }
  else
    {
      beatnum = currentbeat;
    }

#ifdef DEBUG
  printf("JTF Duration %f\n", $2);
  printf("Current beat: %f \t Beat Number: %f\n",currentbeat, beatnum); 
#endif
  newnoteobj (&$1, $2, frogsi);
  /*$1.t_option = NULL;*/
  g_free($1.t_option);
  no_of_options = 0;
}

|rest FLOAT
{
  
  gint duration = 0;
  
  DenemoObject *newmudelaobj = NULL;
  if (currentbeat == beatnum && n == currentbar) 
    {
      totaldurs = beatnum;
    }
  else
    {
      beatnum = currentbeat;
    }
 

  duration = floattoduration ($2, is_tup);
#ifdef DEBUG
  printf("JTF Duration %f\t Denemo Duration: %d\n", $2,duration);
  printf("Current beat: %f \t Beat Number: %f\n",currentbeat, beatnum); 
#endif
   

  newmudelaobj = newchord (duration, $1.dots,0);
 
  frogsi->currentmeasure->data = g_list_append
    ((objnode *)frogsi->currentmeasure->data,
     newmudelaobj);
  frogsi->currentobject = 
    g_list_last((objnode *)frogsi->currentmeasure->data);
  no_of_options = 0;
  /*g_free(newmudelaobj);*/

}

|tuplet;

rest:RESTTYPE dot
{

  $$.rest = $1;
  $$.dots = $2;

}

|RESTTYPE
{
  $$.rest = $1;
  $$.dots = 0;
}

;

note:pitch stem_dir note_option beam
{
  $$.t_option = NULL;
  $$.t_pitch = $1;
  strcpy ($$.stemdir, $2);
  $$.t_option = $3;
  $$.t_beamd = $4;
  
  g_free($3);  
}

;

pitch:NOTENAME octave
{
  strcpy ($$.notename, $1);
  $$.octave = $2;

}

|number_of_half_lines
{
  $$.number_of_lines = $1;
}

;

octave:NUM
{
  $$ = $1;
}

;

stem_dir:directions
{
  strcpy ($$, $1);
}

|END
{
}

;

directions:DIRECTION
{
  strcpy ($$, $1);
}

;

note_option: {
  $$ = (struct p_noteoption *)g_malloc(sizeof( struct p_noteoption));
  $$->next = NULL;
  strcpy($$->type, "\0");
  strcpy($$->t_modifier.type, "\0");
  $$->t_modifier.dots = 0;
  
 /*g_free(item);*/
}
| note_option modifier_note_type position 
{
  optionlist *nitem = createoption($2);
  strcpy($$->type,$2.type);
  
  insertoption(&$$,nitem);
  nitem = NULL;
  /*g_free(nitem);*/
}
;

modifier_note_type:ACCIDENTAL {  strcpy ($$.type, $1); }
|ACCENT {  strcpy ($$.type, $1); }
|STYLE { strcpy ($$.type, $1);}
|NUM {  
  strcpy($$.type,"dots");
  $$.dots = $1;
}
|ORNAMENT {  strcpy ($$.type, $1); }
|grace_notes {  $$.gracenote = $1; }
|TIE {  strcpy ($$.type, $1); }
|NOTEHEAD {  strcpy ($$.type, $1); }
;


tie:TIE
{
  strcpy ($$, $1);

};

dot:NUM
{
  $$ = $1;

}

;

grace_notes:pitch stem_dir grace_note_option beam FLOAT position
{
  $$.t_pitch = $1;
  strcpy ($$.stemdir, $2);
  $$.option = $3;
  $$.t_beamd = $4;
  $$.duration = $5;
  $$.t_position = $6;

}

;

grace_note_option:ACCIDENTAL position
{
  strcpy ($$.type, $1);
  $$.t_position = $2;

}

|ACCENT position
{
  strcpy ($$.type, $1);
  $$.t_position = $2;

}

|STYLE position
{
  strcpy ($$.type, $1);
  $$.t_position = $2;

}

|dot position
{
  $$.dots = $1;
  $$.t_position = $2;
}

|ORNAMENT position
{
  strcpy ($$.type, $1);
  $$.t_position = $2;

}

|tie position
{
  strcpy ($$.type, $1);
  $$.t_position = $2;
};



beam:beam_type
{
  $$.type = $1;
}

|flag
{
  strcpy ($$.direction, $1);
}

|END
{

}
 
;

beam_type:primary DIRECTION
{
  strcpy ($$.btype, $1);
  strcpy ($$.direction, $2);

}

|secondary DIRECTION
{
  strcpy ($$.btype, $1);
  strcpy ($$.direction, $2);

}

;

flag:DIRECTION
{

}

;

primary:FULL
{

}

;

secondary:PARTIAL_
{

}

;

tuplet:TUPLET tuplet_option
{

  objnode *theobj = firstobjnode (frogsi->currentmeasure);
  DenemoObject *newmudelaobj = NULL;
  newmudelaobj = newtupopen ($2.numerator, $2.denominator);
  theobj = g_list_insert (theobj, newmudelaobj, counter);
  frogsi->cursor_x++;
  frogsi->currentmeasure->data = theobj;
  if (frogsi->cursor_appending)
    {
      frogsi->currentobject = g_list_last (theobj);
    }
  else
    {
      frogsi->currentobject = g_list_nth (theobj, frogsi->cursor_x);

    }
  is_tup = TRUE;
}

|TUPLET TUP_END
{

  objnode *theobj = firstobjnode (frogsi->currentmeasure);
  DenemoObject *newmudelaobj = NULL;
  newmudelaobj = newtupclose ();
  theobj = g_list_insert (theobj, newmudelaobj, counter);
  frogsi->cursor_x++;
  frogsi->currentmeasure->data = theobj;
  if (frogsi->cursor_appending)
    {
      frogsi->currentobject = g_list_last (theobj);
    }
  else
    {
      frogsi->currentobject = g_list_nth (theobj, frogsi->cursor_x);

    }
  is_tup = FALSE;
}

;

tuplet_option:NUM NUM
{
  $$.numerator = $1;
  $$.denominator = $2;
}				/*slur_position number_position */

;



slur:SLURTYPE position CURVESHAPE
{
  strcpy ($$.string, $1);
  $$.t_position = $2;
  strcpy ($$.curve, $3);


};

hairpin: HAIRPINTYPE 
{
  strcpy ($$.string, $1);
};

keysignature:no_of_sharps no_of_flats name_of_sharps name_of_flats 
| MODE { }
|KEYTYPE { newkeysigobj ($1); }
;

no_of_sharps:NUM { }
;

no_of_flats:NUM { }
;

name_of_sharps:NOTENAME name_of_sharps { } 
|END { }
;

name_of_flats:NOTENAME name_of_flats { }
|END { }
;

tempo:stationary | transitional;

stationary:tempo_term | FLOAT '=' NUM
{

}

;

tempo_term:TEMPOTERM
{

}

;

transitional:TRANSTEMPO
{

}

;

no_of_lines:NUM
{
  $$ = $1;
}

;

transposition:NUM
{
  $$ = $1;
}

;

additive:NUM '+' additive
{
  $$ = $1;
}

|NUM
{
  $$ = $1;
}

;

timesignature:additive '/' NUM
{
  newtimesig ($1, $3);
}

|additive '/' NUM '+' timesignature
{

}

;

number_of_half_lines:NUM
{

}

;

position_in_half_lines:NUM
{
  $$ = $1;
}

;

%%int
froginput (char *filename, DenemoGUI *gui)
{
  extern FILE *yyin;
#ifdef YYDEBUG
  //  yydebug =1;
#endif
  if ((yyin = fopen (filename, "r")) == NULL)
    {
      fprintf (stderr, "Cannot open the file: %s\n", filename);
      return -1;
    }
  else
    {
      fprintf (stderr, "In FROGINPUT");
      yy_setscore (gui);
      while (!feof (yyin))
	{
	  yyparse ();
	}
    }
  fclose (yyin);

  return 0;
}

int
yyerror (char *errmsg)
{
  extern char *yytext;

  fprintf (stderr, "%d : %s at default %s\n", errcount, errmsg, yytext);
  return 0;

}

/*
 * Add a new note to the bar takes p_note structure with all the data 
 * for the note to be inserted
 *
 */
void
newnoteobj (struct p_note *thenote, gfloat length, DenemoScore *si)
{
  gint no_dots=0;
  gchar notehead[20];
  objnode *theobj = firstobjnode (si->currentmeasure);
  DenemoObject *newmudelaobj = NULL;
  /*  struct p_modifier *mod = NULL;*/
  struct p_noteoption *list = 0;
  note *newnote = NULL;
  gint duration = 0, notename = 0, enharmonic = 0;
  
  gboolean is_tie = FALSE;
  DenemoObject *curmudelaobj = (DenemoObject *)
    (si->currentobject ? si->currentobject->data : NULL);
  notename =
    fetchnotename (thenote->t_pitch.notename, thenote->t_pitch.octave);
  enharmonic = fetchenharmonic (thenote->t_pitch.notename);
  
  
  for(list = thenote->t_option; list; list = list->next)
    {
      

      /*printf("Modifier = %s  :  %d  \n",list->t_modifier.type, 
	     list->t_modifier.dots);*/
      if (!strcmp (list->t_modifier.type, "tie"))
	is_tie = TRUE;
      if (list->t_modifier.dots)
	no_dots = list->t_modifier.dots;
      
      if (!strcmp (list->t_modifier.type, "cross")
	  || !strcmp (list->t_modifier.type, "diamond")
	  || !strcmp (list->t_modifier.type, "harmonic"))
	{
	  strcpy (notehead,list->t_modifier.type);
	}
      else
	strcpy (notehead,"normal");
    }
  
  if (beatnum != totaldurs)
    {
      duration = floattoduration (length, is_tup);
      newmudelaobj = newchord (duration, no_dots,0);
      
      ((chord *)newmudelaobj->object)->is_tied = is_tie;
      for(list = thenote->t_option; list; list = list->next)
	{
	  
	  
	  if (strcmp (list->t_modifier.type, "accent") == 0)
	   ((chord *)newmudelaobj->object)->ornamentlist = 
				  insert_ornament_list(D_ACCENT,
					  					    ((chord *)newmudelaobj->object)->ornamentlist);
	  if (strcmp (list->t_modifier.type, "staccato") == 0)
	  ((chord *)newmudelaobj->object)->ornamentlist = 
				  insert_ornament_list(STACCATO,
					  					    ((chord *)newmudelaobj->object)->ornamentlist);
	  if (strcmp (list->t_modifier.type, "fermata") == 0)
	    ((chord *)newmudelaobj->object)->ornamentlist = 
				  insert_ornament_list(FERMATA,
					  					    ((chord *)newmudelaobj->object)->ornamentlist);
	  if (strcmp (list->t_modifier.type, "tenuto") == 0)
	  ((chord *)newmudelaobj->object)->ornamentlist = 
				  insert_ornament_list(TENUTO,
					  					    ((chord *)newmudelaobj->object)->ornamentlist);

	  if (strcmp (list->t_modifier.type, "trill") == 0)
	  ((chord *)newmudelaobj->object)->ornamentlist = 
				  insert_ornament_list(TRILL,
					  					    ((chord *)newmudelaobj->object)->ornamentlist);
	   
	  if (strcmp (list->t_modifier.type, "turn") == 0)
	    ((chord *)newmudelaobj->object)->ornamentlist = 
				  insert_ornament_list(TURN,
					  					    ((chord *)newmudelaobj->object)->ornamentlist);
	  if (strcmp (list->t_modifier.type, "mordent") == 0)
	    ((chord *)newmudelaobj->object)->ornamentlist = 
				  insert_ornament_list(MORDENT,
					  					    ((chord *)newmudelaobj->object)->ornamentlist);
	  if (strcmp (list->t_modifier.type, "staccatissimo") == 0)
	    ((chord *)newmudelaobj->object)->ornamentlist = 
				  insert_ornament_list(STACCATISSIMO,
					  					    ((chord *)newmudelaobj->object)->ornamentlist);
	  if (strcmp (list->t_modifier.type, "ubow") == 0)
		  ((chord *)newmudelaobj->object)->ornamentlist = 
				  insert_ornament_list(UBOW,
					  					    ((chord *)newmudelaobj->object)->ornamentlist);
	  if (strcmp (list->t_modifier.type, "dbow") == 0)	
	    ((chord *)newmudelaobj->object)->ornamentlist = 
				  insert_ornament_list(DBOW,
					  					    ((chord *)newmudelaobj->object)->ornamentlist);
		 
	}
      addtone (newmudelaobj, notename, enharmonic, cleftype);
      if(notehead)
	{
	  newnote = (note *)((chord *)newmudelaobj->object)->notes->data;
	  newnote->noteheadtype = setnoteheadtype (notehead);
	}
      theobj = g_list_insert (theobj, newmudelaobj, counter);
      si->cursor_x++;
      si->currentmeasure->data = theobj;
      if (si->cursor_appending)
	si->currentobject = g_list_last (theobj);
      else
	si->currentobject = g_list_nth (theobj, si->cursor_x);


    }
  else
    {

      addtone (curmudelaobj, notename, enharmonic, cleftype);
    }

  /* Should really do these two things once per measure, but anyway: */


  
}


static enum headtype
setnoteheadtype (gchar * type)
{
  if (strcmp (type, "cross") == 0)
    return DENEMO_CROSS_NOTEHEAD;
  else if (strcmp (type, "diamond") == 0)
    return DENEMO_DIAMOND_NOTEHEAD;
  else if (strcmp (type, "harmonic") == 0)
    return DENEMO_HARMONIC_NOTEHEAD;
  else
    return DENEMO_NORMAL_NOTEHEAD;
}

/*
 * Add new Clef at the beginning of the staff
 *
 */
static void
newclef (char *clefname)
{
  enum clefs clefstring;
  DenemoObject *clefobj;

  clefstring = cleftoenum (clefname);
  cleftype = clefstring;
  if (counter == 1)
    {
      ((DenemoStaff *) frogsi->currentstaff->data)->sclef = clefstring;
      find_leftmost_staffcontext ((DenemoStaff *)frogsi->currentstaff->data, frogsi);

    }
  else
    {

      clefobj = dnm_newclefobj (clefstring);
      frogsi->currentmeasure->data = g_list_append
	((objnode *)frogsi->currentmeasure->data, clefobj);

    }
}


/*
 * Add new Key Signature at the beginning of the staff
 *
 */
static void
newkeysigobj (char *keyname)
{
  DenemoObject *newobj = NULL;
  gboolean isminor = FALSE;
  gint keynum = 0;
  gint len = strlen (keyname);

  keynum = Keytoint (keyname);

  fprintf (stderr, "%d len \t %d keynum \n ", len, keynum);

  if (len == 4)
    {
      if (keyname[1] == 'm' && keyname[2] == 'i' && keyname[3] == 'n')
	{
	  if (counter == 2)
	    ((DenemoStaff *) frogsi->currentstaff->data)->skey_isminor = TRUE;
	  else
	    isminor = TRUE;
	}
    }
  else if (len == 5)
    {
      if (keyname[2] == 'm' && keyname[3] == 'i' && keyname[4] == 'n')
	{
	  if (counter == 2)
	    ((DenemoStaff *) frogsi->currentstaff->data)->skey_isminor = TRUE;
	  else
	    isminor = TRUE;

	}
    }
  if (counter == 2)
    {
      ((DenemoStaff *) frogsi->currentstaff->data)->skey = keynum;
      find_leftmost_allcontexts (frogsi);
    }
  else
    {

      newobj = dnm_newkeyobj (keynum, isminor, 0);
      frogsi->currentmeasure->data = g_list_append
	((objnode *)frogsi->currentmeasure->data, newobj);
    }
  initkeyaccs (((DenemoStaff *) frogsi->currentstaff->data)->skeyaccs, keynum);
}

/*
 * Add new Time Signature at the beginning of the staff
 *
 */
static void
newtimesig (gint upper, gint lower)
{
  DenemoObject *ret;
  if (counter == 3)
    {
      ((DenemoStaff *) frogsi->currentstaff->data)->stime1 = upper;
      ((DenemoStaff *) frogsi->currentstaff->data)->stime2 = lower;
      find_leftmost_staffcontext 
	((DenemoStaff *)frogsi->currentstaff->data, frogsi);
    }
  else
    {
      ret = dnm_newtimesigobj (upper, lower);
      frogsi->currentmeasure->data = g_list_append
	((objnode *)frogsi->currentmeasure->data, ret);
    }
}

/*
static void
newdynamic (gchar * type)
{
  DenemoObject *ret;

  ret = dynamic_new (type);
  frogsi->currentmeasure->data = g_list_append
    ((objnode *)frogsi->currentmeasure->data, ret);
}
*/

/* Set global Parser structure equal to the main scoreinfo structure */
void
yy_setscore (DenemoGUI *gui)
{
  frogsi = gui->si;
  froggui = gui;
  free_score (frogsi);
  frogsi->currentstaffnum = 0;

}


optionlist *createoption(struct p_modifier option)
{
  optionlist *newitem = (optionlist *)malloc(sizeof(optionlist));

  if(!newitem)
    return NULL;
  else {
    newitem->t_modifier = option;
    newitem->next = NULL;
  }
  return newitem;
}

void insertoption(optionlist **header, optionlist *newitem)
{
  newitem->next = *header;
  *header = newitem;

  return;
}


void emptylist(optionlist *head) 
{
  optionlist *itemdel = NULL;
  while(head != NULL) {
    itemdel = head;
    itemdel->next = NULL;
    free(itemdel);
    head = head->next;
   
  }
}
