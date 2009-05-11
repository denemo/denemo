/* lyparserfuncs.c
 * utility functions invoked by the
 * mudela parser */

/* For Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller */
//#define DEBUG 1
#include "chordops.h"
#include <denemo/denemo.h>
#include "lyparserfuncs.h"
#include "objops.h"
#include "processstaffname.h"
#include "utils.h"
#include <string.h>
#include "lyparser.h"
#include "contexts.h"
#include "staffops.h"
#include "calculatepositions.h"
#include "measureops.h"
#include "exportlilypond.h"
#include "commandfuncs.h"
#include "articulations.h"

 crescendo_state cresc_state;

gboolean g_string_equal (const GString * v, const GString * v2);

/* RECURSIVE return the first object in object list 
   CUROBJ of type thetype before any CHORD type,
   following branches made for references to identifiers 
*/
static DenemoObject *
first_node_context (objnode * curobj, gint thetype)
{
  while (curobj)
    {
      if (((DenemoObject *) curobj->data)->type == MUSIC_IDENTIFIER)
	{
	  DenemoObject *mud = first_node_context (br (curobj), thetype);
	  if (mud)
	    return mud;
	}
      if (((DenemoObject *) curobj->data)->type == thetype)
	return (DenemoObject *) curobj->data;
      if (((DenemoObject *) curobj->data)->type == CHORD)
	return NULL;
      curobj = curobj->next;
    }
  return NULL;
}

/* delete obj from curmeasure if it occurs before the first chord */
static void
delete_context(measurenode * curmeasure, DenemoObject * obj)
{
  objnode *curobj = (objnode *) curmeasure->data;
 while (curobj)
    {
      if ((DenemoObject *) curobj->data == obj)
      {
        curmeasure->data = g_list_remove_link ((objnode *) curmeasure->data,
                                          curobj);
	freeobject ((DenemoObject *) curobj->data);
        g_list_free_1 (curobj);
        return;
      }
      if (((DenemoObject *) curobj->data)->type == CHORD)
        return ;
      curobj = curobj->next;
    }
}
/* return the first object in measure curmeasure of type thetype before any CHORD type */
static DenemoObject *
first_context (measurenode * curmeasure, gint thetype)
{
  objnode *curobj = (objnode *) curmeasure->data;
  return first_node_context (curobj, thetype);
}

/* Denemo holds initial staff properties in the staff structure rather
   than nodes, so transfer node data to staff structure and delete
   the nodes. If we do not do this, objects like initial key, clef, and time
   signatures are shown twice in denemo */
static gint
set_initial_staffcontext (DenemoStaff * curstaffstruct, DenemoScore *si)
{
  measurenode *firstmeasure = curstaffstruct->measures;
  DenemoObject *obj;
  curstaffstruct->context = DENEMO_NONE;
  if(firstmeasure==NULL) {
    warningdialog("Unable to load this file - quitting");
    curstaffstruct->measures = g_list_append(NULL, NULL);
    return -1;
    //g_assert (firstmeasure);
  }
  if ((obj = first_context (firstmeasure, CLEF)))
    {
      curstaffstruct->clef.type = ((clef *) obj->object)->type;
      find_leftmost_staffcontext (curstaffstruct, si);
      fixnoteheights (curstaffstruct);
      delete_context (firstmeasure, obj);
    }
  if ((obj = first_context (firstmeasure, KEYSIG)))
    {
      curstaffstruct->keysig.number = ((keysig *) obj->object)->number;
      dnm_setinitialkeysig (curstaffstruct, curstaffstruct->keysig.number, curstaffstruct->keysig.isminor);
      //initkeyaccs (curstaffstruct->skeyaccs, curstaffstruct->skey);
      delete_context (firstmeasure, obj);
    }
  if ((obj = first_context (firstmeasure, TIMESIG)))
    {
      curstaffstruct->timesig.time1 = ((timesig *) obj->object)->time1;
      curstaffstruct->timesig.time2 = ((timesig *) obj->object)->time2;
      delete_context (firstmeasure, obj);
    }
  if ((obj = first_context (firstmeasure, SET )))
  {
	char *pt  = strstr(obj->user_string, "Staff.midiInstrument");
	if ( pt != NULL)
	{
	    char tmp[40];
	    int i;

	    pt += 20;
	    while(*pt && *pt != '"')pt++;
	    if (*pt) pt++;
	    for(i=0; *pt && *pt != '"' && i < 39; i++)
		tmp[i] = *pt++;
	    tmp[i] = 0;
	    curstaffstruct->midi_instrument = g_string_new(tmp);
	    delete_context (firstmeasure, obj);
	}
  }
  return 0;
}

gint
set_initial_staffcontexts (DenemoScore *si)
{
  staffnode *curstaff = si->thescore;

  si->maxkeywidth = G_MININT;
  for (; curstaff; curstaff = curstaff->next)
    {
      if(set_initial_staffcontext ((DenemoStaff *) curstaff->data, si))
	return -1;
    }
  return 0;
}


void
setstaffname (DenemoScore * si, gchar * str)
{
  DenemoStaff *curstaffstruct = (DenemoStaff *) si->currentstaff->data;

  g_string_assign (curstaffstruct->lily_name, str);
  set_denemo_name (curstaffstruct->lily_name, curstaffstruct->denemo_name);
  g_free (str);
}

enum clefs
cleftypefromname (gchar * str)
{
  enum clefs ret = DENEMO_TREBLE_CLEF;

  if (g_strcasecmp (str, "treble") == 0)
    ret = DENEMO_TREBLE_CLEF;
  else if (g_strcasecmp (str, "bass") == 0)
    ret = DENEMO_BASS_CLEF;
  else if (g_strcasecmp (str, "alto") == 0)
    ret = DENEMO_ALTO_CLEF;
  else if (g_strcasecmp (str, "\"g_8\"") == 0)
    ret = DENEMO_G_8_CLEF;
  else if (g_strcasecmp (str, "tenor") == 0)
    ret = DENEMO_TENOR_CLEF;
  else if (g_strcasecmp (str, "soprano") == 0)
    ret = DENEMO_SOPRANO_CLEF;
  g_free (str);
  return ret;
}

void
set_clef (DenemoScore * si, gchar * str)
{
  ((DenemoStaff *) si->currentstaff->data)->clef.type = cleftypefromname (str);
}

void
set_key (DenemoScore * si, struct twoints t)
{
  DenemoStaff *curstaffstruct = (DenemoStaff *) si->currentstaff->data;

  if (t.b)
    {				/* Minor key  */
      curstaffstruct->keysig.number = t.a - 3;
      curstaffstruct->keysig.isminor = TRUE;
    }
  else
    {
      curstaffstruct->keysig.number = t.a;
      curstaffstruct->keysig.isminor = FALSE;
    }
  //dnm_setinitialkeysig (curstaffstruct, curstaffstruct->skey, curstaffstruct->skey_isminor);
  initkeyaccs (curstaffstruct->keysig.accs, curstaffstruct->keysig.number);
}

void
set_time (DenemoScore * si, struct twoints t)
{
  DenemoStaff *curstaffstruct = (DenemoStaff *) si->currentstaff->data;

  curstaffstruct->timesig.time1 = t.a;
  curstaffstruct->timesig.time2 = t.b;
}

struct twoints
twointer (gint a, gint b)
{
  struct twoints ret;

  ret.a = a;
  ret.b = b;
  return ret;
}

/* This is basically the inverse to determinekey in
 * exportmudela.c */

gint
keynametonumber (gchar * str)
{
  if (g_strcasecmp (str, "ces") == 0)
    return -7;
  else if (g_strcasecmp (str, "ges") == 0)
    return -6;
  else if (g_strcasecmp (str, "des") == 0)
    return -5;
  else if (g_strcasecmp (str, "aes") == 0)
    return -4;
  else if (g_strcasecmp (str, "ees") == 0)
    return -3;
  else if (g_strcasecmp (str, "bes") == 0)
    return -2;
  else if (g_strcasecmp (str, "f") == 0)
    return -1;
  else if (g_strcasecmp (str, "c") == 0)
    return 0;
  else if (g_strcasecmp (str, "g") == 0)
    return 1;
  else if (g_strcasecmp (str, "d") == 0)
    return 2;
  else if (g_strcasecmp (str, "a") == 0)
    return 3;
  else if (g_strcasecmp (str, "e") == 0)
    return 4;
  else if (g_strcasecmp (str, "b") == 0)
    return 5;
  else if (g_strcasecmp (str, "fis") == 0)
    return 6;
  else if (g_strcasecmp (str, "cis") == 0)
    return 7;
  else if (g_strcasecmp (str, "gis") == 0)
    return 8;
  else if (g_strcasecmp (str, "dis") == 0)
    return 9;
  else if (g_strcasecmp (str, "ais") == 0)
    return 10;
  else
    return 0;			/* Defaults to c minor/major */
  g_free (str);
}

gint
mutointernalduration (gint muduration)
{
  gint ret = 0;

  while (muduration > 1)
    {
      ret++;
      muduration >>= 1;
    }
  return ret;
}

gint
pitchtomid_c_offset (gchar name, gint octave)
{
  return 7 * (octave - 1) + (name - 'a' + 5) % 7;
}

void
addtonewrapper (DenemoObject * curmudelaobj, gchar tonetype,
		gint enshift, gint octave, struct twoints duration,
		gint dclef)
{
  gint mid_c_offset = pitchtomid_c_offset (tonetype, octave);

  if (tonetype != 'r')
    addtone (curmudelaobj, mid_c_offset, enshift, dclef);
  ((chord *) curmudelaobj->object)->baseduration = duration.a;
  ((chord *) curmudelaobj->object)->numdots = duration.b;
  set_basic_numticks (curmudelaobj);
}

void
set_tone_option (DenemoObject * curmudelaobj, gchar * option)
{
  set_articulation (option, curmudelaobj);
}

typedef struct Keyword_ent
{
  gchar *str;
  gint type;
}
Keyword_ent;
static Keyword_ent the_key_tab[] = {
  {"alias", ALIAS},
  {"apply", APPLY},
  {"arpeggio", ARPEGGIO},
  {"autochange", AUTOCHANGE},
  {"spanrequest", SPANREQUEST},
  {"commandspanrequest", COMMANDSPANREQUEST},
  {"simultaneous", SIMULTANEOUS},
  {"sequential", SEQUENTIAL},
  {"accepts", ACCEPTS},
  {"alternative", ALTERNATIVE},
  {"bar", BAR},
  {"breathe", BREATHE},
  {"break", BREAK},
  {"char", CHAR_T},
  {"chordmodifiers", CHORDMODIFIERS},
  {"chords", CHORDS},
  {"clef", CLEF_},		/* underscore added for denemo name clash */
  {"cm", CM_T},
  {"consists", CONSISTS},
  {"consistsend", CONSISTSEND},
  {"context", CONTEXT},
  {"default", DEFAULT},
  {"denies", DENIES},
  {"duration", DURATION},
  {"dynamicscript", DYNAMICSCRIPT},
  {"grobdescriptions", GROBDESCRIPTIONS},
  {"figures", FIGURES},
  {"grace", GRACE},
  {"glissando", GLISSANDO},
  {"header", HEADER},
  {"in", IN_T},
  {"key", KEY},
  {"mark", MARK},
  {"new", NEWCONTEXT},
  {"pitch", PITCH},
  {"time", TIME_T},
  {"times", TIMES},
  {"layout", LAYOUT},
  {"lyricmode", LYRICMODE},
  {"lyrics", LYRICS},
  {"lyricsto", LYRICSTO},
  {"midi", MIDI},
  {"mm", MM_T},
  {"name", NAME},
  {"pitchnames", PITCHNAMES},
  {"notes", NOTES},
  {"outputproperty", OUTPUTPROPERTY},
  {"override", OVERRIDE},
  {"set", SET},
  {"rest", REST},
  {"revert", REVERT},
  {"partial", PARTIAL_},
  {"paper", PAPER},
  {"penalty", PENALTY},
  {"property", PROPERTY},
  {"pt", PT_T},
  {"relative", RELATIVE},
  {"remove", REMOVE},
  {"repeat", REPEAT},
  {"addlyrics", ADDLYRICS},
  {"partcombine", PARTCOMBINE},
  {"score", SCORE},
  {"script", SCRIPT},
  {"stylesheet", STYLESHEET},
  {"skip", SKIP},
  {"tempo", TEMPO},
  {"translator", TRANSLATOR},
  {"transpose", TRANSPOSE},
  {"type", TYPE},
  {"unset", UNSET},
  { "version", LILYVERSION},
  {0, 0}
};

static Keyword_ent the_par_tab[] = {
  {"MUSIC_IDENTIFIER", MUSIC_IDENTIFIER},
  {"voicecontext", voicecontext},
  {"staffcontext", staffcontext},
  {"endcontext", endcontext},
  { NULL, 0}
};

gint
lookup_keyword (gchar * str)
{
  int i;
  for (i = 0; the_key_tab[i].str; i++)
    {
      if (!strcmp (the_key_tab[i].str, str))
	return the_key_tab[i].type;	/*very inefficient! */
    }



  return -1;
}
gchar *
lookup_type (gint type)
{
  int i;
  for (i = 0; the_key_tab[i].str; i++)
    {
      if (the_key_tab[i].type == type)
	return the_key_tab[i].str;	/*very inefficient! */
    }
  for (i = 0; the_par_tab[i].str; i++)
    {
      if (the_par_tab[i].type == type)
	return the_par_tab[i].str;	/*very inefficient! */
    }

  return g_strdup_printf("%d", type);
}

static gchar *
lookup_type_of_node (GList *node) {
  if(!node)
    return "NULL pointer";
  return lookup_type(ntype(node));
}

/* sets si->currentstaff to the first staff in si->thescore with lily_name 
 *  NAME or NULL if none.
 * sets si->currentstaffnum to correspond 
 */
static void
findstaff (GString * name, DenemoScore * si)
{
  DenemoStaff *curstaffstruct;
  if (si->thescore)
  {
    for (si->currentstaff = si->thescore, si->currentstaffnum = 0;
	 si->currentstaff;
	 si->currentstaff = si->currentstaff->next, si->currentstaffnum++)
      {
	curstaffstruct = (DenemoStaff *) (si->currentstaff->data);
	if (g_string_equal (curstaffstruct->staff_name, name))
	{
	  return;
	}
      }
  }
  si->currentstaff = NULL;
  si->currentstaffnum = g_list_length (si->thescore);	/*set to insert at end */
}

static GList *staffctx = NULL, *voicectx = NULL, *lyricsctx =
  NULL, *figuresctx = NULL;

static void
anewstaff (DenemoScore * si, GString * staffname, GString * voicename);
/* set si->currentstaff to the insertion point for voice named NAME in staff 
 * STAFFCTX, return TRUE if voice already exists else FALSE. 
 * sets si->currentstaffnum to correspond 
 */
static gboolean
findvoice (GString *name, DenemoScore * si)
{
  DenemoStaff *curstaffstruct;
  DenemoStaff *staffctxstruct;
  if (!si->thescore || !staffctx)
  {
  	si->currentstaff = NULL;
  	si->currentstaffnum = g_list_length (si->thescore);	/*set to insert at end */
      	si->currentstaff = si->thescore;
      	anewstaff (si, g_string_new ("staff"), name);
	si->currentmeasurenum = 1;
      	staffctx = si->currentstaff = g_list_last (si->thescore);
	set_denemo_name (name, staffstruct (staffctx)->denemo_name);
  }
  if (si->thescore)
  {
    if (staffctx)
    {
	staffctxstruct = (DenemoStaff *) (staffctx->data);
	for (si->currentstaff = si->thescore, si->currentstaffnum = 1;
	     si->currentstaff && (staffctx != si->currentstaff);
	     si->currentstaff = si->currentstaff->next, si->currentstaffnum++)
	  /* move to primary voice in current staff */ ;
    

	curstaffstruct = (DenemoStaff *) (si->currentstaff->data);
	if (curstaffstruct->lily_name->len == 0)
	  {			/* no voice here yet */
	    curstaffstruct->lily_name = name;
	    curstaffstruct->voicenumber = 1;
	    set_denemo_name (name, staffstruct (staffctx)->denemo_name);
	    return TRUE;
	  }
	while (g_string_equal
	       (curstaffstruct->staff_name, staffctxstruct->staff_name))
	  {			/* still in same staff */
	    /* found the voice */
	    if (g_string_equal (curstaffstruct->lily_name, name))
	      return TRUE;
	    if (!si->currentstaff->next)
	      return FALSE;
	    si->currentstaff = si->currentstaff->next, si->currentstaffnum++;
	  }
	si->currentstaff = si->currentstaff->prev, si->currentstaffnum--;
	/* just come off this staff, go back one */
	return FALSE;
     }
  }
  si->currentstaff = NULL;
  si->currentstaffnum = 0;
  return FALSE;
}

static void
anewstaff (DenemoScore * si, GString * staffname, GString * voicename)
{
  DenemoStaff *thestaffstruct = (DenemoStaff *) g_malloc0 (sizeof (DenemoStaff));

  thestaffstruct->clef.type = DENEMO_TREBLE_CLEF;
  thestaffstruct->timesig.time1 = 4;
  thestaffstruct->timesig.time2 = 4;
  thestaffstruct->no_of_lines = 5;
  thestaffstruct->staff_name = staffname;
  thestaffstruct->denemo_name = g_string_new ("");
  thestaffstruct->lily_name = voicename;
  thestaffstruct->midi_instrument = g_string_new ("acoustic grand");
  thestaffstruct->voicenumber = 1;
  thestaffstruct->nummeasures = 1;
  if (si->thescore)
    {
      si->thescore =
	g_list_insert (si->thescore, thestaffstruct, si->currentstaffnum);
      si->currentstaff = g_list_nth (si->thescore, si->currentstaffnum);
      si->currentstaffnum++;
    }
  else
    {				/* FIXME for some bizarre reason si->currentstaffnum is set to 1 
				   in init_score but zero elsewhere? */
      si->currentstaffnum = 1;
      si->thescore = si->currentstaff = g_list_append (NULL, thestaffstruct);
    }
}

/* traverse G a GList of object nodes returning the first of type T or
NULL if none */
GList *
findtok (GList * g, int t)
{
  while (g && ntype (g) != t)
    g = g->next;
  return g;
}

/* put separate chords into a notes list of a chord and return that,
 * amalgamating the user_strings 
 */
static DenemoObject *
generate_chord (GList * chordnode)
{
  GList *g;
  GList *firstchord = findtok (chordnode, CHORD);

  gchar *str;			/* collect up the user strings here */
  if (firstchord == NULL)
    {
      g_warning ("<> without any notes");
      return (DenemoObject *) chordnode->data;	/* FIXME return everything */
    }

  for (str = NULL, g = chordnode; g; g = g->next)
    {
      if (str && u_str (g))
	str = g_strconcat (str, u_str (g), NULL);
      else if (!str)
	str = u_str (g);
    }
  u_str (firstchord) = str;
  for (g = firstchord->next; g; g = g->next)
    {
      if (ntype (g) == CHORD)
	{
	  addtone ((DenemoObject *) firstchord->data,
		   ((note
		     *) (((chord *) (((DenemoObject *) g->data)->object))->
			 notes->data))->mid_c_offset,
		   ((note
		     *) (((chord *) (((DenemoObject *) g->data)->object))->
			 notes->data))->enshift, 0);
	}			/* FIXME memory leak */
    }
  return (DenemoObject *) firstchord->data;
}

/* RECURSIVE: generate_chords_and_sequentials()
 * called within a SEQUENTIAL it detects <> and { } music.
 * The <> are chords are in lilypond style ie SIMULTANEOUS nodes 
 * this function turns them into notes lists
 * belonging to a chord structure in denemo style
 * the { } are things like repeats etc, inline these
 * Note we don't cope with new contexts being created...
 */

static int create_score (DenemoScore * si, GList * top);
void
list_tree(gchar *message, GList * top);

/* generate_chords_and_sequentials
	PARAM g is a node with a branch, 
	the branch is traversed and any 
	sequential music is inserted in the branch
	any simultaneous music is formed into a chord
	any music identifiers were not being handled.
 */
static void
generate_chords_and_sequentials (DenemoScore * si, GList * g)
{
  GList *chordnode;

#ifdef DEBUG
  g_print ("%s called\n", __FUNCTION__);
#endif
  for (chordnode = br (g); chordnode; chordnode = chordnode->next)
    {
      if (ntype (chordnode) == SIMULTANEOUS)
	{
	  gchar *str = u_str (chordnode);
	  gchar *str2 = u_post_str (chordnode);
	  chordnode->data = generate_chord (br (chordnode));
	  u_str (chordnode) =
	    g_strconcat (str, u_str (chordnode), str2, NULL);
	  /* FIXME memory leak */
	}
      else if (ntype (chordnode) == SEQUENTIAL)
	{
	  /*create a text node for the u_str, one for the u_post_str and 
	     in between link in the create_score(br(chordnode) */
	  GList *ret;
	  GList *temp = br (chordnode);
#ifdef DEBUG
	  g_print ("%s Case SEQUENTIAL temp type %d\n", __FUNCTION__, ntype (temp));
#endif
	  generate_chords_and_sequentials (si, chordnode);
	  u_str (temp) = g_strconcat (u_str (chordnode), u_str (temp), NULL);
	  (chordnode->prev)->next = NULL;
	  ret = g_list_concat (chordnode->prev, temp);
	  temp = g_list_last (temp);
	  u_str (temp) =
	    g_strconcat (u_str (temp), u_post_str (chordnode), NULL);
	  chordnode->prev = NULL;
	  //g_print("Concatenating lists\n");
	  br (chordnode) = g_list_concat (temp, chordnode->next);
	  
	  /* FIXME memory leak of chordnode itself */
	}
      else if (ntype (chordnode) == MUSIC_IDENTIFIER)
	{
	  GList *temp = ((nodeid*)(chordnode)->data)->id;
	  if (ntype (temp) == SEQUENTIAL)
	      generate_chords_and_sequentials (si, temp);
        }
    }
#ifdef DEBUG
    g_print ("%s exit\n", __FUNCTION__);
#endif
}



/* RECURSIVE: perform the guts of break_into_measures, going inside
   MUSIC_IDENTIFIERS - assume such a thing does not cross a tuplet or grace */
static GList *
recursive_end_of_measure (
		DenemoScore * si,
		GList * theobjs, DenemoStaff * staff,
		GList ** current_measure, GList * measures_list,
		gint * pticks_so_far, gint * ptickspermeasure,
		gint * pnumerator, gint * pdenominator)
{

  objnode *curobjnode;
  DenemoObject *theobj;
  gint basic_ticks_in_tuplet_group = 0;
  gint basic_ticks_in_grace_group = 0;
  gboolean in_tuplet = FALSE;
  gboolean in_grace = FALSE;
  for (curobjnode = theobjs; curobjnode; curobjnode = curobjnode->next)
    {
      theobj = (DenemoObject *) curobjnode->data;

#ifdef DEBUG
fprintf(stderr, "%s type %d ticks_so_far %d tickspermeasure %d %s\n", __FUNCTION__, 
		theobj->type, *pticks_so_far, *ptickspermeasure,
		theobj->user_string);
#endif

      theobj->starttick =
	*pticks_so_far + (basic_ticks_in_tuplet_group * *pnumerator
			  / *pdenominator) + basic_ticks_in_grace_group;

      switch (theobj->type)
	{
	case voicecontext:
	  create_score(si, curobjnode);
  	  return measures_list;
	  break;

	case PARTIAL:
	case CHORD:

	  if (in_tuplet)
	    {
	      set_tuplefied_numticks (theobj, *pnumerator, *pdenominator);
	      basic_ticks_in_tuplet_group += theobj->basic_durinticks;
	    }
	  else if (in_grace)
	    {
	      set_grace_numticks (theobj, 8);
	      basic_ticks_in_grace_group += theobj->basic_durinticks;
	    }
	  else
	    {
	      set_tuplefied_numticks (theobj, 1, 1);
	      set_grace_numticks (theobj, 1);
	      if (theobj->type == PARTIAL)
		{
		  theobj->durinticks = *ptickspermeasure - theobj->durinticks;
		  /* FIXME need to handle the FRACTION multiplier which is not 
		     being passed through at the moment */
		}
	      *pticks_so_far += theobj->durinticks;
	    }
	  break;

	case TUPOPEN:

	  in_tuplet = TRUE;
	  *pnumerator = ((tupopen *) theobj->object)->numerator;
	  *pdenominator = ((tupopen *) theobj->object)->denominator;
	  basic_ticks_in_tuplet_group = 0;	/* Probably gratuitous */
	  break;
	case TUPCLOSE:

	  in_tuplet = FALSE;
	  *pticks_so_far += ((basic_ticks_in_tuplet_group * *pnumerator)
			     / *pdenominator);
	  *pnumerator = 1;
	  *pdenominator = 1;
	  basic_ticks_in_tuplet_group = 0;
	  break;
	case GRACE_START:

	  in_grace = TRUE;
	  basic_ticks_in_grace_group = 0;
	  break;
	case GRACE_END:

	  in_grace = FALSE;
	  /*      *pticks_so_far += basic_ticks_in_grace_group; */
	  basic_ticks_in_grace_group = 0;
	  break;
	case TIMESIG:
	  staff->timesig.time1 = ((timesig *) theobj->object)->time1;
	  staff->timesig.time2 = ((timesig *) theobj->object)->time2;
	  *ptickspermeasure = staff->timesig.time1 * WHOLE_NUMTICKS / staff->timesig.time2;
	  break;
	case MUSIC_IDENTIFIER:
	  {
	    GList *next = ((nodeid *) (curobjnode)-> data)->id;
	    if (ntype (next) == SEQUENTIAL)
		next = br (next);
	        measures_list = recursive_end_of_measure (
		si, 
		next, staff,
		current_measure, measures_list,
		pticks_so_far, ptickspermeasure,
		pnumerator, pdenominator
		);
	  }
	  break;

	case SKIPNAME:  // FIXME memory leak and
			// This assumes skip is for entire bar which is not
			// generally true. The issue is duration, dot(s) used
			// by denemo cannot describe full measure skip 
			// durations in some time signatures such as 5/4. 
	  *pticks_so_far = *ptickspermeasure;
	  break;

	case LILYDIRECTIVE:
	  //g_print("Passing LilyPond insert %s thru\n", ((lilydirective *) theobj->object)->directive->str);
	  break;
	default:
	  break;
	}// switch obj type

      if (theobj->type != MUSIC_IDENTIFIER)
      {
      *current_measure = g_list_append ( *current_measure, theobj);

      theobj->starttickofnextnote =
	*pticks_so_far + (basic_ticks_in_tuplet_group * *pnumerator
			  / *pdenominator) + basic_ticks_in_grace_group;
      // check if this measure is finished 

#if 1
      if(curobjnode->next && 
	 (DenemoObject *) curobjnode->next->data &&
	 ((DenemoObject *) curobjnode->next->data)->type==LILYDIRECTIVE) {// include any LILYDIRECTIVEs in the measure
	;//go on to include it in this measure
      } else
#endif
	if (*pticks_so_far >= *ptickspermeasure)
	  {
	    GList *g;
	    
	    if (measures_list)
	      staff->nummeasures++;
	    
	    measures_list = g_list_append (measures_list, *current_measure);
	    
#ifdef DEBUG
	    g_print ("%s: measure %d\n", __FUNCTION__, staff->nummeasures);
	    for (g = *current_measure; g; g = g->next)
	      g_print ("\ttype= %d %s\n",  ntype(g), ((nodegeneric *) g->data)->user_string);
#endif
	    
	    
	    *current_measure = NULL;
	    *pticks_so_far = 0;
	  }// enough for this measure
      }// Not music_ident
    }// for each object
  // g_print("At the end of for each object with %d %d\n", *pticks_so_far>0, *current_measure);
  if (*pticks_so_far>0
      && *current_measure
)
    {
      if (measures_list)
	staff->nummeasures++;
      measures_list = g_list_append (measures_list, *current_measure);
      *current_measure = NULL;
    }
  
  return measures_list;
}


/*  attach the measures in BRANCH to the MEASURES_LIST, by counting ticks 
 * return the measures list built up */
static GList *
break_into_measures (DenemoScore * si, GList * branch, DenemoStaff * staff)
{
  GList * measures_list = staff->measures;
  GList * current_measure = NULL;
  gint numerator = 1, denominator = 1;  /* varies if in tuplet etc */
  gint ticks_so_far = 0;
  gint tickspermeasure = staff->timesig.time1 * WHOLE_NUMTICKS / staff->timesig.time2;


#ifdef DEBUG
    g_print ("enter %s, type of branch %d\n",  __FUNCTION__, ntype(branch));
#endif
    measures_list = recursive_end_of_measure (
		si,
		branch, staff, 
		&current_measure, measures_list, 
		&ticks_so_far, &tickspermeasure,
		&numerator, &denominator
		);

#ifdef DEBUG
  g_print ("exit %s return measures_list=%p\n", __FUNCTION__, measures_list);
#endif
  return measures_list;
}

/*
	is_tied is TRUE after either a slur or tie becomes active
	calling is_tied with an argument of NULL resets it's state
*/
static gboolean
is_tied ( chord * chord_pt)
{
    static gboolean active_tie;
    static gboolean active_slur;
    gboolean ret = active_tie || active_slur;

    if (chord_pt == NULL)
      {
	active_tie = FALSE;
	active_slur = FALSE;
	return FALSE;
      }

    if (chord_pt->is_tied)
	active_tie = TRUE;
    else
	active_tie = FALSE;

    if (chord_pt->slur_end_p)
	active_slur = FALSE;

    if (chord_pt->slur_begin_p)
	active_slur = TRUE;

    return ret;
}
/*
	recursively scan lyrucsto object lyparser tree for lyrics
	and attach lyrics to chord structure

	Note: Denemo does not support multiple lines of lyrics.
*/ 
static gboolean
get_lyric (GList * glyric, measurenode **meas, objnode *curobjnode)
{
      DenemoObject *curobj;

      if (glyric == NULL)
	  return (FALSE);

      switch (ntype (glyric))
      {
	case MUSIC_IDENTIFIER:
	  get_lyric (((nodeid*)(glyric)->data)->id, meas, curobjnode);
	  glyric = glyric->next;
	  break;

	case SIMULTANEOUS:
	  get_lyric (br (glyric), meas, curobjnode);
	  if ((br (glyric))->next)
		g_warning("Lyricsto: Denemo does not support more than one line of lyrics");
	  glyric = glyric->next;
	  break;

	case SEQUENTIAL:
	  get_lyric (br (glyric), meas, curobjnode);
	  glyric = glyric->next;
	  break;

	case OVERRIDE:
	case LYRICMODE:
	case lyricscontext:
	case endcontext:
	  glyric = glyric->next;
	  break;

	case LYRICS:
	case SKIP:
	  do
	    {
		if (curobjnode == NULL)
		  {
		    if (*meas == NULL)
			return FALSE;
		    curobjnode = (objnode *) (*meas)->data;
		    *meas = (*meas)->next;
		  }
                curobj = (DenemoObject *) curobjnode->data;
	        if (curobj->type == CHORD 
		    && ( (chord *) curobj->object)->numnotes
		    && ! is_tied ((chord *) curobj->object))
	          {
		    if (ntype (glyric) == LYRICS)
	  	        ( (chord *) curobj->object)->lyric = 
				((chord *) gobj (glyric))->lyric;
	  	    glyric = glyric->next;
		  }
		curobjnode = curobjnode->next;
	    }
	  while (glyric && (ntype (glyric) == LYRICS || ntype (glyric) == SKIP));
	  break;

	default:
	  g_warning("get_lyric unexpected lyric type %s", lookup_type(ntype (glyric)));
	  return FALSE;
	  break;

      }
      return (get_lyric (glyric, meas, curobjnode));

}
/*
	process the lyricsto list generated by the lyparser
*/
static void
process_lyricsto (DenemoScore * si, GList * top)
{
	staffnode *staff;
        measurenode *meas;


    for (staff = si->thescore; staff; staff = staff->next)
      {
	if (!strcmp (( (DenemoStaff *) (staff->data))->lily_name->str, 
		u_str (top)))
	{
	  break;
	}
      }
      if (!staff)
      {
	g_warning("Lyricsto staff %s not found\n", u_str (top));
	return;
      }


      meas = (measurenode *) (((DenemoStaff *) (staff->data))->measures);
      get_lyric (br (top), &meas, NULL);
}

void
list_tree(gchar *message, GList * top) {
  GList *g;
  g_print("<<<<<<<<<<<<<<<<Begin %s\n",message);
  for (g = top; g; g = g->next)
    {
      nodegeneric *node =  (nodegeneric *) g->data;
      g_print ("node type = %s(%d) string = %s\n",
	       lookup_type(node->type),
	       node->type,
	       node->user_string);
      switch(node->type) {
      case MUSIC_IDENTIFIER:
	if(br(g)) {
	  g_print("***************");
	  //list_tree("Music Identfier found", br(g));
	  g_print("____________");
	}
	else
	  g_warning("No branch");
	break;
      case SEQUENTIAL:
	if(br(g)) {
	  g_print("***************");
	  //list_tree("Sequential found", br(g));
	  g_print("____________");
	}
	else
	  g_warning("No branch");
	break;
      case SIMULTANEOUS:
	if(br(g)) {
	  g_print("***************");
	  //list_tree("Simultaneous found", br(g));
	  g_print("____________");
	}
	else
	  g_warning("No branch");
	break;
      case CHORD:
	  g_print("*************** Chord *************");
	  break;
      }
      
    }
  g_print(">>>>>>>>>>>>>>>>>>>>>End %s\n",message);
}

/* create_score: Recursively traverse the GList TOP which represents a 
   score block of the input lily file. Create denemo structures to enable 
   it to be edited graphically
*/
static int
create_score (DenemoScore * si, GList * top)
{
  GList *g;

  DenemoStaff *curstaffstruct;
#if DEBUG
  char *name;
  list_tree("From create_score ",top); 
#endif

  for (g = top; g; (g = g->next))
    {
#if DEBUG
      g_print ("Handling: node type = %s string = %s\t",
	       lookup_type(((nodegeneric *) g->data)->type),
	       ((nodegeneric *) g->data)->user_string);
#endif
      switch (ntype (g))
	{
	case figuredbasscontext:
	case staffcontext:
	  ; // for GString 
	  GString *staffname = gstr (g) ? gstr (g) : g_string_new ("");
	  findstaff (staffname, si);	/*sets si->currentstaffnum */
	  staffctx = si->currentstaff;
#ifdef DEBUG
	  g_print ("%s:staffcontext staffname -%s- staffctx=%p\n",
		__FUNCTION__, staffname->str, staffctx);
#endif
	  if (!staffctx)
	    {
	      si->currentstaff = si->thescore;
	      anewstaff (si, staffname, g_string_new (""));
	      staffctx = si->currentstaff = g_list_last (si->thescore);
	      set_denemo_name (staffname, staffstruct (staffctx)->denemo_name);
	      staffstruct (staffctx)->voicenumber = 1;
	    }
	  si->currentprimarystaff = staffctx;
	  if (ntype (g) == figuredbasscontext)
	    {
	      staff_info *staffs =
		(staff_info *) g_malloc0 (sizeof (staff_info));
	      if (staffctx->prev == NULL)
		{
		  parser_error ("Figured Bass not preceded by a bass staff ",
				0);
		  return -1;
		}
	      staffs->main_staff = (DenemoStaff *) (staffctx->prev->data);
	      staffs->related_staff = (DenemoStaff *) (staffctx->data);
	      si->has_figures = staffs;
	      ((DenemoStaff *) (staffctx->data))->no_of_lines = 0;
	    }
	  break;

	case voicecontext:
	  ; // for GString
          GString *name = gstr(g) ? gstr (g) : g_string_new ("");

#ifdef DEBUG
	  g_print ("%s:voicecontext name %s\n", __FUNCTION__, name->str);
#endif
	  if (!findvoice (name, si))
	    {
#ifdef DEBUG	      
	      g_print ("\tvoicecontext name not found,  create new staff\n");
#endif
	      anewstaff (si, staffstruct (staffctx)->staff_name, name);
	      si->currentmeasurenum = 1;
	      voicectx = si->currentstaff;
	      set_denemo_name (name, staffstruct (voicectx)->denemo_name);
	      //staffstruct (voicectx)->voicenumber = 2;
	      staffstruct (voicectx)->voicenumber = 1;
	    }
	  voicectx = si->currentstaff;
	  break;

	case endcontext:
	  if (voicectx)
	    {
	      voicectx = NULL;
	    }
	  else
	    staffctx = NULL;
	  break;
	case MUSIC_IDENTIFIER:
#define id(g) (((nodeid*)(g)->data)->id)
	  if (create_score (si, id (g)))
	    return -1;
	  break;

	case SIMULTANEOUS:
	  if (create_score (si, br (g)))
	    return -1;
	  break;

	case SCORE:
	  parser_error ("Score block inside a score block", 0);
	  return -1;
	  break;

	case DENEMO_MEASURES:
	case SEQUENTIAL:
	  /* first find out if we have voice and staff contexts,
	     if not create use/create
	     default staff structures */
	  if (!staffctx)
	    {			/* use/create the first staff structure in si->thescore */
	      if (si->thescore)
		{
		  /* if we have a named voice context create a staff context 
		     for it otherwise insert it as a voice in the first staff 
		   */
		  staffctx = voicectx = g_list_last (si->thescore);
		}
	      else
		{
		  anewstaff (si, g_string_new ("dummyname"),
			     g_string_new ("dummyname"));
		  staffctx = voicectx = si->currentstaff;
		}
	    }
	  else
	    {			/* there is a staff context */
	      if (!voicectx)
		voicectx = staffctx;
	    }
	  si->currentstaff = voicectx;
	  curstaffstruct = staffstruct (voicectx);
	  if (ntype (g) == SEQUENTIAL)
	    {
/*
	      if (ntype (br (g)) == MUSIC_IDENTIFIER)
                {
	  	  if (create_score (si, ((nodeid*)(g)->data)->id))
	    	      return -1;
	        }
	      else
*/
	      {
		  GList * music;
	          generate_chords_and_sequentials (si, g);

		  music = break_into_measures (si, br (g), curstaffstruct);
		  if (music)
		    {
	      		curstaffstruct->measures = music;
		         br (g) = (GList *) & (curstaffstruct->measures);
		         /* nasty - we store the address
		           of the GList* pointer. This is so that if
		           editing in denemo adds a new bar at the beginning we
		           will not find ourselves pointing to the second bar. 
			   Or, worse if the first bar got deleted. 
			   We are not yet tackling the case where the staff 
			   gets deleted. We should probably store the name 
			   and prevent that being changed uncontrollably */
		         ntype (g) = DENEMO_MEASURES;
		    }
	      }
	    }
	  else
	    {			/* these measures are already in another staff */
	      curstaffstruct->measures = *(measurenode **) br (g);
	      curstaffstruct->is_parasite = (measurenode **) br (g);
	    }
	  break;

	case LYRICSTO: {
		process_lyricsto (si, g);
	    }
	  break;

	default:
#if DEBUG
	  g_print("create score ignores type %s\n", lookup_type(ntype(g)));
#endif
	  break;
		}
    }
  return 0;

}

/* returns 0 on success */
int
create_score_from_lily (DenemoScore * si, GList * top)
{
  staffctx = NULL;
  voicectx = NULL;
  lyricsctx = NULL;
  figuresctx = NULL;
  return create_score (si, top);
}


/* this function seems to be missing from the glib library, 
   although in the headers */
gboolean
g_string_equal (const GString * v, const GString * v2)
{
  if ((!v) || (!v2) || (v->len != v2->len))
    return FALSE;
  return !strcmp (v->str, v2->str);	/* not general case of embedded 0 bytes */
}
void
init_crescendo_state ()
{
	cresc_state.active_cres = FALSE;
	cresc_state.active_decres = FALSE;
}
/*
	terminate crescendo or decrescendo if active
*/
static void
terminate_crescendo_state (chord *cd)
{
	if (cresc_state.active_cres)
	{
	    cd->crescendo_end_p = TRUE;
	    cresc_state.active_cres = FALSE;
	}
	if (cresc_state.active_decres)
	{
	    cd->diminuendo_end_p = TRUE;
	    cresc_state.active_decres = FALSE;
	}
}

void
set_post_events (DenemoObject *mud, gchar *usr_str, GList *g)
{
    DenemoObject *obj = mud;
    chord * chordpt;

    if (!g)
	return;

    if (obj->type == SIMULTANEOUS)
    {
	obj = (((nodeglist*)obj)->branch)->data;
    }

    if (obj->type != CHORD)
    {

      return;//g_assert (obj->type == CHORD);
    }
    chordpt = (chord *) (obj->object);
	while (g)
	{
		usr_str = g_strconcat (usr_str , u_str (g), NULL);
		switch (ntype (g)) {
		case '(':
			chordpt->slur_begin_p = TRUE;
			break;

		case ')':
			chordpt->slur_end_p = TRUE;
			break;	

		case '~':
			chordpt->is_tied = TRUE;
			break;	

		case E_SMALLER:
			terminate_crescendo_state (chordpt);
			chordpt->crescendo_begin_p = TRUE;
			cresc_state.active_cres = TRUE;
			break;

		case E_BIGGER:
			terminate_crescendo_state (chordpt);
			chordpt->diminuendo_begin_p = TRUE;
			cresc_state.active_decres = TRUE;
			break;

		case E_EXCLAMATION:
			terminate_crescendo_state (chordpt);
			break;

		case TONEOPTION:
			set_articulation (gstr (g)->str, obj);
			break;

		case DYNAMICMARK:
		    {

			/*
			   A dynamic mark terminates crescendo or decrescendo
			*/
			terminate_crescendo_state (chordpt);

			/*
				\cr alternate \<
				\decr alternate \>
				\rc & \rced (\!) backward compatibility Denemo 
			*/
			if (!strcmp ( gstr (g)->str, "cr"))
			{
			    chordpt->crescendo_begin_p = TRUE;
			    cresc_state.active_cres = TRUE;
			}
			else if (!strcmp ( gstr (g)->str, "decr"))
			{
			    chordpt->diminuendo_begin_p = TRUE;
			    cresc_state.active_decres = TRUE;
			}
			else if (!strcmp ( gstr (g)->str, "rced")
				 || !strcmp ( gstr (g)->str, "rc"))
			{
				/* we have already terminated hairpin
				   this is so Denemo does not show these
				   strings as dynamics */
			}
			else
			
    			    chordpt->dynamics = 
      				g_list_append ( chordpt->dynamics, gstr (g));
  		    }
			break;


		default: g_warning ("type %s not processed in set_post_events", 
				lookup_type(ntype (g)));
			break;
		}
	    g = g->next;
	}
}

#ifdef LILYEDIT
/***************** functions for managing display of lily text ***************/

static void
string_edited (GtkTextBuffer * buffer, gpointer data)
{
  GtkTextIter start, end;
  gchar *text;
  DenemoScore *si = (DenemoScore *) data;
  gtk_text_buffer_get_bounds (buffer, &start, &end);
  text = gtk_text_iter_get_text (&start, &end);
  if (si->curlilynode == NULL)
    {
      g_warning ("Attempt to edit nonexistent node - ignored\n");
      return;
    }
  if (u_str (si->curlilynode))
    g_free (u_str (si->curlilynode));
  /* else big memory leak - check all lilynodes 
     xsuser_strings are mallocced strings  */
  u_str (si->curlilynode) = text;
}


void
lily_text_change (DenemoScore * si)
{
  static gboolean text_is_readonly = FALSE;
  g_signal_handler_block (G_OBJECT (si->textbuffer), si->sigid);
  if (ntype (si->curlilynode) == DENEMO_MEASURES)
    {
      gtk_widget_hide (si->textview);
      gtk_widget_show (si->musicdatabutton);
      text_is_readonly = TRUE;
    }
  else
    {
      if (text_is_readonly)
	{
	  gtk_widget_show (si->textview);
	  gtk_widget_hide (si->musicdatabutton);
	}
      text_is_readonly = FALSE;
      if (ntype (si->curlilynode) == endcontext)
	gtk_text_buffer_set_text (si->textbuffer,
				  g_strdup ("<<Music Data End>>"), -1);
      if (!u_str (si->curlilynode))
	u_str (si->curlilynode) = generate_lily (si->curlilynode);
      gtk_text_buffer_set_text (si->textbuffer, u_str (si->curlilynode), -1);

    }

  g_signal_handler_unblock (G_OBJECT (si->textbuffer), si->sigid);
}


void
set_text_node (GtkWidget * button, DenemoScore * si)
{
  /* FIXME: the next line can coredump when you have multiple score blocks
     and you have destroyed the textwindow and switched scores...
     there are a series of fields like these that need to be file-wide
     not score block wide */
  if (!si->textwindow)
    create_text_display (si);
  if (si->currentobject)
    si->curlilynode = si->currentobject;
  lily_text_change (si);
}

void
toggle_top_node (GtkWidget * button, DenemoScore * si)
{
  if (si->lily_file->next == NULL)
    return;			/* all in one node */
  if (GTK_WIDGET_VISIBLE (si->scorearea))	/* not in toplevel */
    {
      if (si->lily_file)
	si->curlilynode = si->lily_file;
      gtk_widget_hide (si->scorearea);
      if (!GTK_WIDGET_IS_SENSITIVE (si->textview))
	{
	  gtk_widget_set_sensitive (si->textview, TRUE);
	  gtk_widget_set_sensitive (si->scorearea, FALSE);
	}
    }
  else
    gtk_widget_show (si->scorearea);
  lily_text_change (si);
}
static void
node_text_next (GtkWidget * win, gpointer data)
{
  DenemoScore *si = (DenemoScore *) data;
  if (si->curlilynode == si->currentobject)
    {
      cursorright (NULL);
      si->curlilynode = si->currentobject;
      gtk_widget_draw (si->scorearea, NULL);
      /* so cursor is seen to move in score */
    }
  else if (si->curlilynode->next)
    {
      si->curlilynode = si->curlilynode->next;
    }
#if DEBUG
  g_print ("next node has node type = %s string = %s\n",
	   lookup_type(ntype (si->curlilynode)), u_str (si->curlilynode));
#endif
  lily_text_change (si);
}

static void
node_text_previous (GtkWidget * win, gpointer data)
{
  DenemoScore *si = (DenemoScore *) data;
  /*  if(si->curlilynode == NULL) si->curlilynode = lily_file; */
  if (si->curlilynode == si->currentobject)
    {
      cursorleft (NULL);
      si->curlilynode = si->currentobject;
      gtk_widget_draw (si->scorearea, NULL);
      /* so cursor is seen to move in score */
    }
  else if (si->curlilynode->prev)
    {
      si->curlilynode = si->curlilynode->prev;
    }

#if DEBUG
  g_print ("next node has node type = %s string = %s\n",
	   lookup_type(ntype (si->curlilynode)), u_str (si->curlilynode));
#endif
  lily_text_change (si);
}


static void
toggle_window (GtkWidget * widget, gpointer data)
{
  DenemoScore *si = (DenemoScore *) data;
  /*  printf("toggling display\n"); */
  if (GTK_WIDGET_IS_SENSITIVE (si->textview))
    {
      gtk_widget_set_sensitive (si->textview, FALSE);
      gtk_widget_set_sensitive (si->scorearea, TRUE);
    }
  else
    {
      gtk_widget_set_sensitive (si->textview, TRUE);
      gtk_widget_set_sensitive (si->scorearea, FALSE);
    }

}

/* create a textwindow to display the nodes in DenemoScore lily_file
    */
void
create_text_display (DenemoScore * si)
{
  GtkWidget *main_vbox;
  static GtkWidget *second_window = NULL;
  GtkWidget *hbox;
  GtkWidget *sw;
  GtkWidget *togglebutton, *topbutton, *prevbutton, *nextbutton,
    *reloadbutton;

  main_vbox = gtk_bin_get_child ((GtkBin *) si->window);
  si->textwindow = gtk_vbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (main_vbox), si->textwindow);
  gtk_widget_show (si->textwindow);
  main_vbox = si->textwindow;
  if (!second_window)
    {
      second_window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
      gtk_window_set_title (GTK_WINDOW (second_window),
			    "Lilypond Mode Functions");
      gtk_window_set_default_size (GTK_WINDOW (second_window), 100, 75);
      gtk_widget_show (second_window);

      hbox = gtk_hbox_new (FALSE, 1);
      gtk_container_add (GTK_CONTAINER (second_window), hbox);
      gtk_widget_show (hbox);
      /* the reload node button */
      reloadbutton = gtk_button_new_with_label ("Reload");
      g_signal_connect (G_OBJECT (reloadbutton), "clicked",
			(GtkSignalFunc) reload_lily_file, si);
      gtk_box_pack_start (GTK_BOX (hbox), reloadbutton, TRUE, FALSE, 0);
      gtk_widget_show (reloadbutton);
      /* the edit button */
      togglebutton = gtk_button_new_with_label ("Toggle window");
      gtk_widget_set_size_request (togglebutton, -1, 20);
      g_signal_connect (G_OBJECT (togglebutton), "clicked",
			(GtkSignalFunc) toggle_window, si);
      gtk_box_pack_start (GTK_BOX (hbox), togglebutton, TRUE, FALSE, 0);
      gtk_widget_show (togglebutton);

      /* the top node button */
      topbutton = gtk_button_new_with_label ("Toplevel <-> Music Data");
      gtk_widget_set_size_request (topbutton, -1, 20);
      g_signal_connect (G_OBJECT (topbutton), "clicked",
			(GtkSignalFunc) toggle_top_node, si);
      gtk_box_pack_start (GTK_BOX (hbox), topbutton, TRUE, FALSE, 0);
      gtk_widget_show (topbutton);


      /* the previous node button */
      prevbutton = gtk_button_new_with_label ("<-");
      gtk_widget_set_size_request (prevbutton, -1, 20);
      g_signal_connect (G_OBJECT (prevbutton), "clicked",
			(GtkSignalFunc) node_text_previous, si);
      gtk_box_pack_start (GTK_BOX (hbox), prevbutton, TRUE, FALSE, 0);
      gtk_widget_show (prevbutton);
      /* the next node button */
      nextbutton = gtk_button_new_with_label ("->");
      gtk_widget_set_size_request (nextbutton, -1, 20);
      g_signal_connect (G_OBJECT (nextbutton), "clicked",
			(GtkSignalFunc) node_text_next, si);
      gtk_box_pack_start (GTK_BOX (hbox), nextbutton, TRUE, FALSE, 0);
      gtk_widget_show (nextbutton);
      /* the music data button */
      si->musicdatabutton =
	gtk_button_new_with_label ("music data - edit graphically");
      gtk_widget_set_size_request (si->musicdatabutton, -1, 20);
      /* could have signal here to go to current object? */
      gtk_box_pack_start (GTK_BOX (hbox), si->musicdatabutton, TRUE,
			  FALSE, 0);
      gtk_widget_show (si->musicdatabutton);

    }
  /******* the text to edit ***********/
  sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
				  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  gtk_container_add (GTK_CONTAINER (main_vbox), sw);
  gtk_widget_show (sw);


  si->textview = gtk_text_view_new ();
  gtk_widget_set_sensitive (si->textview, FALSE);
  gtk_container_add (GTK_CONTAINER (sw), si->textview);
  si->textbuffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (si->textview));
  si->curlilynode = si->lily_file;
  gtk_text_buffer_set_text (si->textbuffer, u_str (si->curlilynode), -1);
  si->sigid = g_signal_connect (G_OBJECT (si->textbuffer), "changed",
				(GtkSignalFunc) string_edited, si);
  gtk_widget_show (si->textview);
}
#else /* GTK version 1 */
void
toggle_top_node (GtkWidget * button, DenemoGUI *gui)
{
  return;
}

void
set_text_node (GtkWidget * button, DenemoGUI *gui)
{
  return;
}

void
lily_text_change (DenemoGUI *gui)
{
  return;
}

void
create_text_display (DenemoGUI *gui)
{
  updatescoreinfo (gui); // RRR
  return;
}
#endif
