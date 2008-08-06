%{ // -*-Fundamental-*-
/*
  lexer.l -- implement the Flex lexer

  source file of the LilyPond music typesetter

  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
           Jan Nieuwenhuizen <janneke@gnu.org>
 Modified as a front end GUI for lilypond:

 (c) 2003-2005 Richard Shann
*/



#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <string.h> /* for strlen */
#include <glib.h>
#include <denemo/denemo.h>
#include "lyparserfuncs.h"


#include "lyparser.h" 

#define YYText() yytext

/* the lexer allocates an GList aka objnode with a mudelaobject as data 
	for each token as it recognises it and sets the mudelaobject type to 
	be the type of the token by invoking the macro RETURN quoted strings -
	these are just storing the user_text and returning token STRING_ 
 */

extern YYLTYPE lylloc;
#define YY_USER_ACTION { \
  lylloc.first_line=lylineno; \
  lylval.generic.user_string = g_strconcat(input_text?input_text:"",\
														 (gchar *) yytext, NULL);\
  g_free(input_text);input_text=NULL;\
}
#define RETURN(token) {lylval.generic.type = token;return token;}
#define UNDO_YY_USER_ACTION  {input_text = lylval.generic.user_string;}
#define YY_NO_UNPUT 1
#define YY_NO_TOP_STATE 1

/* discard the str stored in input_text by YY_USER_ACTION */
static void remove_from_input (gchar *str);

/* for skipping over the body of { } blocks not of interest to denemo */
static int open_brace_count; 
/* for skipping over parenthetisized scheme */
static int open_paren_count; 




static char  escaped_char(char a);
static void 
scan_fraction (gchar *str, struct twoints *pt);


extern GHashTable* name_value_pairs;
extern GHashTable* scm_identifiers;
int scan_escaped_word (gchar *);

int scan_bare_word (gchar *);

static nodeglist * lookup_identifier (gchar *);
static nodegstr* lookup_scm_identifier(gchar *str);
static gboolean lookup_pitch(gchar *str);
static int lookup_enshift(gchar *str);

static GString *quoted_string; /* somewhere to accumulate a quoted string in 
											 the lexer - the whole user text is being 
											 stored here as I am not interpreting the 
											 actual contents inside the quotes*/


/*
RH 7 fix (?)
*/
#define isatty HORRIBLEKLUDGE
static gchar *input_text;
static void start_main_input (void) {
	return;
}

static gboolean main_input_b_;


#define start_quote()	\
	yy_push_state (quote);\
	quoted_string=g_string_new(yytext)
#define end_quote()	\
		pop_state (); \
		g_free(lylval.gstr.user_string); \
		quoted_string = g_string_append(quoted_string, yytext); \
		lylval.gstr.user_string = quoted_string->str; \
		lylval.gstr.gstr = g_string_new(quoted_string->str); \
		g_string_free(quoted_string, FALSE);

static void remove_from_input (gchar *str) {
int len = strlen(str);
*(lylval.generic.user_string + strlen(lylval.generic.user_string)-len) = 0;
input_text = lylval.generic.user_string;
}
/* to debug put this below with the options %option debug */

%}

%option yylineno
%option noyywrap
%option nodefault


%option stack
%option never-interactive 
%option warn

%x version
%x chords
%x incl
%x lyrics
%x notes
%x figures
%x quote
%x longcomment
%x body
%x parens
%x scheme


A		[a-zA-Z]
AA		{A}|_
N		[0-9]
AN		{AA}|{N}
PUNCT		[?!:'`]
ACCENT		\\[`'"^]
NATIONAL	[\001-\006\021-\027\031\036\200-\377]
TEX		{AA}|-|{PUNCT}|{ACCENT}|{NATIONAL}
WORD		{A}{AN}*
ALPHAWORD	{A}+
DIGIT		{N}
UNSIGNED	{N}+
FRACTION	{N}+\/{N}+
INT		-?{UNSIGNED}
REAL		({INT}\.{N}*)|(-?\.{N}+)
KEYWORD		\\{WORD}
WHITE		[ \n\t\f\r]
HORIZONTALWHITE		[ \t]
BLACK		[^ \n\t\f\r]
NOTECOMMAND	\\{A}+
LYRICS		({AA}|{TEX})[^0-9 \t\n\f]*
ESCAPED		[nt\\'"]
EXTENDER	__
HYPHEN		--
IGNORE_BLOCK   "\\paper"|"\\layout"
%%


<*>\r		{
	// windows-suck-suck-suck
}

<INITIAL,chords,incl,lyrics,notes,figures>{
  "%{"	{
	UNDO_YY_USER_ACTION;
	yy_push_state (longcomment);
  }
  {IGNORE_BLOCK}{WHITE}+"{"   {
	UNDO_YY_USER_ACTION;
	open_brace_count = 1;
	yy_push_state (body);
  }


  %[^{\n].*\n |
  %[^{\n]     |
  %\n         |
  %[^{\n].*   {
    UNDO_YY_USER_ACTION;
	}
  {WHITE}+ 	{
    UNDO_YY_USER_ACTION;
  }
}

<longcomment>{
	[^\%]*   	|
	\%*[^}%]*	{
    			UNDO_YY_USER_ACTION;
	}
	"%"+"}"		{
    		UNDO_YY_USER_ACTION;
		pop_state ();
	}
	<<EOF>> 	{
		//g_error ("EOF found inside a comment");
	UNDO_YY_USER_ACTION;	
		pop_state ();
		/* if (! close_input ()) 
		  yyterminate (); can't move this, since it actually rets a YY_NULL */
	}
}
<body>{ 
	[^}{]* {
    		UNDO_YY_USER_ACTION;
	}
	"{" {
		UNDO_YY_USER_ACTION;
		open_brace_count++;
	}
	"}" {
	if (--open_brace_count ==  0) {
		UNDO_YY_USER_ACTION;
		pop_state ();
	} else {
		UNDO_YY_USER_ACTION;
	}
	}


}
<parens>{ 
	[^)(]* {
    		UNDO_YY_USER_ACTION;
	}
	"(" {
		UNDO_YY_USER_ACTION;
		open_paren_count++;
	}
	")" {

	if (--open_paren_count ==  0) {
		pop_state ();
		pop_state (); /* pop scheme state as well */
		RETURN (SCM_T);
	} else {
		UNDO_YY_USER_ACTION;
	}

	}
}

<INITIAL,chords,lyrics,notes,figures>\\maininput           {
	if (!main_input_b_)
	{
		start_main_input ();
		main_input_b_ = TRUE;
	}
	else
		g_error ("\\maininput disallowed outside init files");
}

<INITIAL,chords,lyrics,figures,notes>\\include           {
	 /* ignore include files */
	UNDO_YY_USER_ACTION;
	yy_push_state (incl);
}
<incl>\"[^"]*\";?   { /* got the include file name */
	UNDO_YY_USER_ACTION;
	pop_state ();
}
<incl>\\{BLACK}*;?{WHITE} { /* got the include identifier */
 	UNDO_YY_USER_ACTION;
}
<incl>\"[^"]*   { // backup rule
	parser_error ("Missing end quote", lylineno);
}
<chords,notes,figures>r 	{
	RETURN (RESTNAME);
}
<chords,notes,figures>s 	{
	RETURN (SKIPNAME);
}
<chords,notes,figures>R		{
	RETURN (MULTI_MEASURE_REST);
}
<chords,notes,figures>\|		{
	remove_from_input(yytext);
}

<INITIAL,chords,lyrics,notes,figures>\\\${BLACK}*{WHITE}	{
	*(yytext+strlen(yytext)-1) = '\0';
	return scan_escaped_word (yytext + 2);
}
<INITIAL,chords,lyrics,notes,figures>\${BLACK}*{WHITE}		{
	*(yytext+strlen(yytext)-1) = '\0';
	return scan_bare_word (yytext + 1);
}
<INITIAL,chords,lyrics,notes,figures>\\\${BLACK}*		{ // backup rule
	g_error ("white expected");
	exit (1);
}
<INITIAL,chords,lyrics,notes,figures>\${BLACK}*		{ // backup rule
	g_error ("white expected");
	exit (1);
}

<INITIAL,chords,lyrics,notes,figures>#    {	
	UNDO_YY_USER_ACTION;
	yy_push_state (scheme);
	}

<scheme>{
	[^ \n\t\r\f(]+	{
	UNDO_YY_USER_ACTION; 	
	}
	{WHITE}+ {
	pop_state();
	RETURN (SCM_T);
	}
	"("   {
		UNDO_YY_USER_ACTION;
		open_paren_count = 1;
		yy_push_state (parens);
	  }
}

<INITIAL,notes,lyrics>{
        \<\<   {
                return DOUBLE_ANGLE_OPEN;
        }
        \>\>   {
                return DOUBLE_ANGLE_CLOSE;
        }
}

<figures>{
	_	{
		RETURN (FIGURE_SPACE);
	}
	\>		{
		RETURN (FIGURE_CLOSE);
	}
	\< 	{
		RETURN (FIGURE_OPEN);
	}
}
<notes,figures>{
	{ALPHAWORD}	{
		return scan_bare_word (YYText ());
	}

	{NOTECOMMAND}	{
		return scan_escaped_word (YYText () + 1); 
	}
	{FRACTION}	{
		scan_fraction(yytext, &lylval.t.t);
		RETURN (FRACTION);
	}

	{DIGIT}		{
		lylval.i.i = atoi (YYText ());
		RETURN (DIGIT);
	}
	{UNSIGNED}		{
		lylval.i.i = atoi (YYText ());
		RETURN (UNSIGNED);
	}

	\" {
		start_quote ();
	}
}

\"		{
	start_quote ();
	
}
<quote>{
	\\{ESCAPED}	{
		g_string_append_c (quoted_string, escaped_char (*(yytext+1)));
	}
	[^\\"]+	{
		g_string_append (quoted_string, yytext);
	}
	\"	{
		end_quote();
		RETURN (STRING_);
	}
	.	{
		g_string_append (quoted_string, yytext);
	}
}

<lyrics>{
	\" {
		start_quote ();
	}
	{FRACTION}	{
		scan_fraction(yytext, &lylval.t.t);
		RETURN (FRACTION);
	}
	{UNSIGNED}		{
		lylval.i.i = atoi (YYText ());
		RETURN (UNSIGNED);
	}
	{NOTECOMMAND}	{
		return scan_escaped_word (YYText () + 1);
	}
	{LYRICS} {
		
		if (!strcmp(yytext, "__"))
			RETURN (EXTENDER);
		if (!strcmp(yytext, "--"))
			RETURN (HYPHEN);
		lylval.gstr.gstr = g_string_new(yytext);
		RETURN (STRING_);
	}
	. {
		RETURN (*yytext);
	}
}
<chords>{
	{ALPHAWORD}	{
		return scan_bare_word (YYText ());
	}
	{NOTECOMMAND}	{
		return scan_escaped_word (YYText () + 1);
	}
	{FRACTION}	{
		scan_fraction(yytext, &lylval.t.t);
		RETURN (FRACTION);
	}
	{UNSIGNED}		{
		lylval.i.i = atoi (YYText ());
		RETURN (UNSIGNED);
	}
	\" {
		start_quote ();
	}
	-  {
		RETURN (CHORD_MINUS);
	}
	:  {
		RETURN (CHORD_COLON);
	}
	\/\+ {
		RETURN (CHORD_BASS);
	}
	\^  {
		RETURN (CHORD_CARET);
	}
	. {
		RETURN (*yytext);
	}
}

<<EOF>> {
	set_trailing_white_space(input_text);
	g_free(input_text);input_text=NULL;/* note that YY_USER_ACTION has not been run */
 	yyterminate ();
}


{WORD}	{
	return scan_bare_word (YYText ());
}
{KEYWORD}	{
	return scan_escaped_word (YYText () + 1);
}
{REAL}		{
	double r;
	int cnv=sscanf (YYText (), "%lf", &r);
	g_assert (cnv == 1);
	lylval.r.r = r;	
	RETURN (REAL);
}

{UNSIGNED}	{
	lylval.i.i = atoi (YYText ());
	RETURN (UNSIGNED);
}

[{}]	{

	RETURN (*yytext);
}
[*:=]		{
	char c = YYText ()[0];

	RETURN (c);
}

<INITIAL,notes,figures>.	{
	RETURN (*yytext);
}

<INITIAL,lyrics,notes,figures>\\. {
    char c= *(yytext+1);

    switch (c) {
    case '>':
	RETURN (E_BIGGER);
    case '<':
	RETURN (E_SMALLER);
    case '!':
	RETURN (E_EXCLAMATION);
    case '(':
	RETURN (E_OPEN);
    case ')':
	RETURN (E_CLOSE);
    case '[':
	RETURN (E_LEFTSQUARE);
    case ']':
	RETURN (E_RIGHTSQUARE);
    case '~':
	RETURN (E_TILDE);
    case '\\':
	RETURN (E_BACKSLASH);
    default:
	RETURN (E_CHAR);
    }
}

<*>.		{
	g_error("invalid character: `%c'", *yytext);
	RETURN (*yytext);
}

%%
void
push_note_state ()
{
  yy_push_state (notes);
}

void
push_figuredbass_state ()
{
  yy_push_state (figures);
}

void
push_chord_state ()
{
  yy_push_state (chords);
}

void
push_lyric_state ()
{
  yy_push_state (lyrics);
}

void
pop_state ()
{
  yy_pop_state ();
}


int
scan_escaped_word (gchar * str)
{
  nodeglist *sid;	/* a definition of an identifier already turned into a 
			typed glist by the parser encountering an assignment */
  nodegstr *scm_str;
  int look = lookup_keyword (str);
  if (look != -1)
    {
      RETURN (look);	/* this is many of the tokens eg /times and so on */
    }
  sid = lookup_identifier (str); /* there follows all the things that can have
					been defined using the = syntax */
  if (sid)
    {
      lylval.id.id  = sid->branch;
      RETURN (sid->type);
    }

  scm_str = lookup_scm_identifier (str);
  if (scm_str)
    {
      lylval.gstr.type = scm_str->type;
      lylval.gstr.gstr = g_string_new (scm_str->gstr->str);
      return scm_str->type;
    }

  if ((YYSTATE != notes) && (YYSTATE != chords))
    {
#ifdef LATER
      /*      SCM pitch = scm_hashq_get_handle (pitchname_tab_, str); 
	  some sort of lookup of pitchnames to yield a pitch object to be 
	  stored in lylval */

      if (gh_pair_p (pitch))
	{
	  lylval.id.id = ly_cdr (pitch);
	  RETURN (NOTENAME_PITCH);
	}
#endif
    }
#if 0
  parser_error (g_strdup_printf
		("scan escaped word: Unknown escaped string: `\\%s'", str),
		yylineno);

  lylval.gstr.gstr = g_string_new (yytext);
  RETURN (STRING_);
#else
  lylval.gstr.gstr = g_string_new (yytext);
  RETURN (LILYDIRECTIVE_TOKEN);
#endif

}

/* scan_bare_word takes a string and if in chords or notes it tries to 
	interpret the word as a notename of one sort or another eg 
	NOTENAME_PITCH etc. It uses tables for notes pitchname_tab_ 
	and  chordmodifier_tab_ which I will need to create similar 
	things for (ultimately they can be changed by the include file). 
	My lylval should have a field for the str and another for
	the pitchname in some binary form ready to be attached to a 
	mudelaobject...
	Otherwise it returns a STRING_ token. */
int
scan_bare_word (gchar * str)
{

  if ((YYSTATE == notes) || (YYSTATE == chords))
    {
      if (lookup_pitch (str))
	{
	  lylval.t.t.a = *str - 'a';
	  lylval.t.t.b = lookup_enshift (str + 1);
	  RETURN ((YYSTATE != chords) ? NOTENAME_PITCH : TONICNAME_PITCH);
	}
    }
#ifdef LATER
  else if ((pitch = scm_hashq_get_handle (chordmodifier_tab_, str)) !=
	   SCM_BOOL_F)
    {

      lylval.id.id = ly_cdr (pitch);

      RETURN (CHORDMODIFIER_PITCH);
    }

#endif
  lylval.gstr.gstr = g_string_new (yytext);
  RETURN (STRING_);

}

gboolean
note_state_b ()
{
  return YY_START == notes;
}

gboolean
chord_state_b ()
{
  return YY_START == chords;
}

gboolean
lyric_state_b ()
{
  return YY_START == lyrics;
}

gboolean
figure_state_b ()
{
  return YY_START == figures;
}

void
reset_initial_lexer_state ()
{
  while (YY_START != INITIAL)
    yy_pop_state ();

}

/* 1.3.146 == removal of ;
Lilypond_version oldest_version ("1.3.146");
 */



static void
scan_fraction (gchar * str, struct twoints *pt)
{
#ifndef G_OS_WIN32
  char *slash = index (lytext, '/');
  sscanf (lytext, "%d", &pt->a);
  if (slash)
    sscanf (slash + 1, "%d", &pt->b);
  else
    pt->b = 1;
#else
  char *slash;
    for(slash=lytext;*slash && *slash!='/';slash++)
    ;
  sscanf (lytext, "%d", &pt->a);
  if (*slash)
    sscanf (slash + 1, "%d", &pt->b);
  else
    pt->b = 1;
#endif 
}


static nodeglist *
lookup_identifier (gchar * str)
{
  return (nodeglist *) g_hash_table_lookup (name_value_pairs, str);
}

static nodegstr *
lookup_scm_identifier (gchar * str)
{
  return (nodegstr *) g_hash_table_lookup (scm_identifiers, str);
}



static gboolean
lookup_pitch (gchar * str)
{
  if (*str < 'a' || *str > 'g')
    return FALSE;
  if (lookup_enshift (str + 1) == BAD_ENSHIFT)
    return FALSE;
  return TRUE;
}

static int
lookup_enshift (gchar * str)
{
  if (*str == 0)
    return 0;
  if (!strcmp (str, "es"))
    return -1;
  if (!strcmp (str, "is"))
    return 1;
  if (!strcmp (str, "eses"))
    return -2;
  if (!strcmp (str, "isis"))
    return 2;
  return BAD_ENSHIFT;
}

static char
escaped_char (char c)
{
  switch (c)
    {
    case 'n':
      return '\n';
    case 't':
      return '\t';
    case '\'':
    case '\"':
    case '\\':
      return c;
    }
  return 0;
}


