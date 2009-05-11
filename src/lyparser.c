/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 1

/* Substitute the variable and function names.  */
#define yyparse lyparse
#define yylex   lylex
#define yyerror lyerror
#define yylval  lylval
#define yychar  lychar
#define yydebug lydebug
#define yynerrs lynerrs
#define yylloc lylloc

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     DENEMO_MEASURES = 258,
     TEXT = 259,
     staffcontext = 260,
     voicecontext = 261,
     lyricscontext = 262,
     figuredbasscontext = 263,
     endcontext = 264,
     LILYDIRECTIVE_TOKEN = 265,
     MUSICMODE = 266,
     TONEOPTION = 267,
     DYNAMICMARK = 268,
     AUTOCHANGE = 269,
     ALIAS = 270,
     APPLY = 271,
     ARPEGGIO = 272,
     DYNAMICSCRIPT = 273,
     ACCEPTS = 274,
     ALTERNATIVE = 275,
     BAR = 276,
     BREAK = 277,
     BREATHE = 278,
     CHORDMODIFIERS = 279,
     CHORDS = 280,
     CHAR_T = 281,
     CLEF_ = 282,
     CM_T = 283,
     CONSISTS = 284,
     SEQUENTIAL = 285,
     SIMULTANEOUS = 286,
     GROBDESCRIPTIONS = 287,
     CONSISTSEND = 288,
     DENIES = 289,
     DURATION = 290,
     EXTENDER = 291,
     FIGURES = 292,
     FIGURE_OPEN = 293,
     FIGURE_CLOSE = 294,
     FIGURE_BRACKET_CLOSE = 295,
     FIGURE_BRACKET_OPEN = 296,
     GLISSANDO = 297,
     GRACE = 298,
     HEADER = 299,
     HYPHEN = 300,
     IN_T = 301,
     INVALID = 302,
     KEY = 303,
     LYRICS = 304,
     MARK = 305,
     MARKUP = 306,
     MULTI_MEASURE_REST = 307,
     MIDI = 308,
     MM_T = 309,
     PITCH = 310,
     DEFAULT = 311,
     NAME = 312,
     PITCHNAMES = 313,
     NOTES = 314,
     PAPER = 315,
     PARTIAL_ = 316,
     PENALTY = 317,
     PROPERTY = 318,
     OVERRIDE = 319,
     SET = 320,
     REVERT = 321,
     PT_T = 322,
     RELATIVE = 323,
     REMOVE = 324,
     REPEAT = 325,
     ADDLYRICS = 326,
     PARTCOMBINE = 327,
     SCORE = 328,
     SCRIPT = 329,
     SKIP = 330,
     SPANREQUEST = 331,
     STYLESHEET = 332,
     COMMANDSPANREQUEST = 333,
     TEMPO = 334,
     OUTPUTPROPERTY = 335,
     TIME_T = 336,
     TIMES = 337,
     TRANSLATOR = 338,
     TRANSPOSE = 339,
     TYPE = 340,
     UNSET = 341,
     CONTEXT = 342,
     LAYOUT = 343,
     LYRICSTO = 344,
     LYRICMODE = 345,
     NEWCONTEXT = 346,
     LILYVERSION = 347,
     DRUM_PITCH = 348,
     MUSIC_FUNCTION = 349,
     REST = 350,
     DOUBLE_ANGLE_CLOSE = 351,
     DOUBLE_ANGLE_OPEN = 352,
     E_CHAR = 353,
     E_EXCLAMATION = 354,
     E_SMALLER = 355,
     E_BIGGER = 356,
     E_OPEN = 357,
     E_CLOSE = 358,
     E_LEFTSQUARE = 359,
     E_RIGHTSQUARE = 360,
     E_TILDE = 361,
     E_BACKSLASH = 362,
     CHORD_BASS = 363,
     CHORD_COLON = 364,
     CHORD_MINUS = 365,
     CHORD_CARET = 366,
     FIGURE_SPACE = 367,
     DIGIT = 368,
     NOTENAME_PITCH = 369,
     TONICNAME_PITCH = 370,
     CHORDMODIFIER_PITCH = 371,
     DURATION_IDENTIFIER = 372,
     FRACTION = 373,
     IDENTIFIER = 374,
     SCORE_IDENTIFIER = 375,
     MUSIC_OUTPUT_DEF_IDENTIFIER = 376,
     NUMBER_IDENTIFIER = 377,
     REQUEST_IDENTIFIER = 378,
     MUSIC_IDENTIFIER = 379,
     TRANSLATOR_IDENTIFIER = 380,
     STRING_IDENTIFIER = 381,
     SCM_IDENTIFIER = 382,
     RESTNAME = 383,
     SKIPNAME = 384,
     STRING_ = 385,
     SCM_T = 386,
     UNSIGNED = 387,
     REAL = 388,
     UNARY_MINUS = 389
   };
#endif
/* Tokens.  */
#define DENEMO_MEASURES 258
#define TEXT 259
#define staffcontext 260
#define voicecontext 261
#define lyricscontext 262
#define figuredbasscontext 263
#define endcontext 264
#define LILYDIRECTIVE_TOKEN 265
#define MUSICMODE 266
#define TONEOPTION 267
#define DYNAMICMARK 268
#define AUTOCHANGE 269
#define ALIAS 270
#define APPLY 271
#define ARPEGGIO 272
#define DYNAMICSCRIPT 273
#define ACCEPTS 274
#define ALTERNATIVE 275
#define BAR 276
#define BREAK 277
#define BREATHE 278
#define CHORDMODIFIERS 279
#define CHORDS 280
#define CHAR_T 281
#define CLEF_ 282
#define CM_T 283
#define CONSISTS 284
#define SEQUENTIAL 285
#define SIMULTANEOUS 286
#define GROBDESCRIPTIONS 287
#define CONSISTSEND 288
#define DENIES 289
#define DURATION 290
#define EXTENDER 291
#define FIGURES 292
#define FIGURE_OPEN 293
#define FIGURE_CLOSE 294
#define FIGURE_BRACKET_CLOSE 295
#define FIGURE_BRACKET_OPEN 296
#define GLISSANDO 297
#define GRACE 298
#define HEADER 299
#define HYPHEN 300
#define IN_T 301
#define INVALID 302
#define KEY 303
#define LYRICS 304
#define MARK 305
#define MARKUP 306
#define MULTI_MEASURE_REST 307
#define MIDI 308
#define MM_T 309
#define PITCH 310
#define DEFAULT 311
#define NAME 312
#define PITCHNAMES 313
#define NOTES 314
#define PAPER 315
#define PARTIAL_ 316
#define PENALTY 317
#define PROPERTY 318
#define OVERRIDE 319
#define SET 320
#define REVERT 321
#define PT_T 322
#define RELATIVE 323
#define REMOVE 324
#define REPEAT 325
#define ADDLYRICS 326
#define PARTCOMBINE 327
#define SCORE 328
#define SCRIPT 329
#define SKIP 330
#define SPANREQUEST 331
#define STYLESHEET 332
#define COMMANDSPANREQUEST 333
#define TEMPO 334
#define OUTPUTPROPERTY 335
#define TIME_T 336
#define TIMES 337
#define TRANSLATOR 338
#define TRANSPOSE 339
#define TYPE 340
#define UNSET 341
#define CONTEXT 342
#define LAYOUT 343
#define LYRICSTO 344
#define LYRICMODE 345
#define NEWCONTEXT 346
#define LILYVERSION 347
#define DRUM_PITCH 348
#define MUSIC_FUNCTION 349
#define REST 350
#define DOUBLE_ANGLE_CLOSE 351
#define DOUBLE_ANGLE_OPEN 352
#define E_CHAR 353
#define E_EXCLAMATION 354
#define E_SMALLER 355
#define E_BIGGER 356
#define E_OPEN 357
#define E_CLOSE 358
#define E_LEFTSQUARE 359
#define E_RIGHTSQUARE 360
#define E_TILDE 361
#define E_BACKSLASH 362
#define CHORD_BASS 363
#define CHORD_COLON 364
#define CHORD_MINUS 365
#define CHORD_CARET 366
#define FIGURE_SPACE 367
#define DIGIT 368
#define NOTENAME_PITCH 369
#define TONICNAME_PITCH 370
#define CHORDMODIFIER_PITCH 371
#define DURATION_IDENTIFIER 372
#define FRACTION 373
#define IDENTIFIER 374
#define SCORE_IDENTIFIER 375
#define MUSIC_OUTPUT_DEF_IDENTIFIER 376
#define NUMBER_IDENTIFIER 377
#define REQUEST_IDENTIFIER 378
#define MUSIC_IDENTIFIER 379
#define TRANSLATOR_IDENTIFIER 380
#define STRING_IDENTIFIER 381
#define SCM_IDENTIFIER 382
#define RESTNAME 383
#define SKIPNAME 384
#define STRING_ 385
#define SCM_T 386
#define UNSIGNED 387
#define REAL 388
#define UNARY_MINUS 389




/* Copy the first part of user declarations.  */
#line 1 "./lyparser.y"
 // -*-Fundamental-*-

/*
	adapted from
  parser.yy -- Bison/C++ parser for lilypond

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
           Jan Nieuwenhuizen <janneke@gnu.org>
	adapted for lilyfront (c) Richard Shann 2003

The result of parsing is a GList starting at lily_file
The data elements of the GList are node* where node is a structure with type, 
user_string and a union.
Each node in the list represents the result of some rule below, with the 
user_string containing the part of the input file that parsed to that node.
The nodes are a simple list except
1) assignment statements where the assignee is a separate list entered in a 
symbol table.
2) SIMULTANEOUS blocks which are nodes pointing to a list for their contents
3) \score blocks which are nodes pointing to a list for their contents
After parsing the function create_score() is run on each score block. This 
creates a staff structure for each \context Staff encountered. The 
staff->measures field is pointed to the start of the block enclosed by the 
\context Staff. This block is traversed and the durations calculated to break 
the list into measures which are linked into the staff->measures list.
After editing graphically denemo stype the whole list is traversed writing 
out the user_string fields or (where they are NULL) re-creating them from 
the data in the DenemoObject concerned.
Note that the structure node is arranged to have the same first fields as
DenemoObject so that either type can appear in the parse tree.

*/
#define YYDEBUG 1
#define YYPRINT fprintf
//#define DEBUG 1
#define YYTOKEN_TABLE 1
#include <string.h> /*for memcpy */
#include <stdlib.h> /* for system() */
#include <denemo/denemo.h>
#include "view.h" /* this includes many others - many are not 
		     protected against double inclusion */
#include "chordops.h"
#include "objops.h"
#include "twoints.h"
#include "processstaffname.h"
#include "tupletops.h"
#include "graceops.h"

#include <ctype.h>

#include "lyparserfuncs.h"
void lyrestart( FILE *new_file );
extern int lylineno;
static int parser_error_linenum = 0;
static gchar *parser_error_message = NULL; /* NULL for no parse error */
nodemin endcontextnode={endcontext,NULL}; /*the only instantiation of this
node, used as a marker */


#define stradd(m,n) if(n.user_string){m.user_string = \
	g_strconcat(m.user_string,n.user_string, NULL);}

#define LATER_MESSAGE(line) \
	call_parser_error("later at %d\n", __LINE__, line);return EOF


static GtkWidget *parser_error_dialog;
static int error_level_;
/* lexer states FIXME */
void push_note_state(void);
void push_figuredbass_state(void);
void push_chord_state(void);
void push_lyric_state(void);
void pop_state(void);
gboolean note_state_b(void);


int lylex(void);
static void lyerror(char *);

static void call_parser_error(gchar *text, int lineno, int input_line_number) {
   g_print("The parser needs finishing a lyparser.y:%d for this idiom"
	   " to be usable\nThe problem occured at line number %d of the"
	   " lily input", lineno, input_line_number);
   parser_error("Edit or comment out the offending idiom if possible\n", 
	        input_line_number);
}

static GList *lily_file = NULL; /* the entire data generated by the parse */
/* before EOF there may be white space which won't
 *  be collected by any rule - the lexer passes it
 *  using set_trailing_white_space() 
 */
static gchar *trailing_white_space = NULL; 

GHashTable* name_value_pairs=NULL;
GHashTable* scm_identifiers=NULL;

#if 0
#define MALLOC_NODE(n, a) nodegeneric*n = \
	(nodegeneric*)g_malloc0(sizeof(nodegeneric));\
	 memcpy(n, &a, sizeof(a));
#else
#define MALLOC_NODE(n, a) nodegeneric*n = \
	(nodegeneric*)g_malloc0(sizeof(nodegeneric));\
        n->type=a.type;n->user_string = a.user_string;
#endif


static void set_identifier (char* name, nodeglist *value) {	
  if(!name_value_pairs)	
    name_value_pairs = g_hash_table_new (g_str_hash, g_str_equal);
  g_hash_table_insert (name_value_pairs, (gpointer)name, (gpointer)value);
#if DEBUG
//g_warning("Set identifier %s to value %p\n",name, value);
g_print("Set identifier %s to value %s\n",name,  u_str(value->branch));
#endif	
}

static nodeglist * typed_glist (GList *g, guint t) {
	nodeglist *nodeg = (nodeglist *)g_malloc0(sizeof(nodeglist));
	nodeg->type = t;
	nodeg->branch = g;
	return nodeg;
}

#ifdef YYPRINT
static gchar * type_name(gint type);
#endif

gboolean
regular_identifier_b (char *s)
{
  gboolean v = TRUE;
  while (*s && v)
   {
        v = v && isalpha (*s);
        s++;
   }
  return v;
}



static int intlog2(int t) {
int i=1, n=0;
while ( (i<<n)<t) n++;
return n;
}

gboolean
is_duration_b (int t)
{
  return t && t == 1 << intlog2 (t);
}

node4i default_duration_;

static gchar * keytoname(gint pitch, gint enshift) {
gchar *ret;
	ret = g_strdup("a");
	*ret += pitch;
	for(;enshift>0;enshift--) ret = g_strconcat(ret,"is",NULL);
	for(;enshift<0;enshift++) ret = g_strconcat(ret,"es",NULL);/*tricksy - only one happens */
	return ret;
}

// needed for bison.simple's malloc () and free ()

#include <malloc.h>
#include <stdlib.h>
#include <stdio.h>

#define YYERROR_VERBOSE 1




static void lyerror(char *s) {
	parser_error(s, lylineno);
}




#line 195 "./lyparser.y"







/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 203 "./lyparser.y"
{
	nodemin minimal;
	nodegeneric generic;
	nodec c;
	nodei i;
	node2i t;
	node4i f;
	nodeb b;
	noden n;
	nodeid id;

	nodegstr gstr;
	nodemus music;	
	noder r;


	nodeglist *branch;
	GList *scm;
}
/* Line 187 of yacc.c.  */
#line 587 "lyparser.tab.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 612 "lyparser.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
	     && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
    YYLTYPE yyls;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1581

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  158
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  99
/* YYNRULES -- Number of rules.  */
#define YYNRULES  271
/* YYNRULES -- Number of states.  */
#define YYNSTATES  415

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   389

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   128,     2,     2,     2,     2,     2,   129,
     121,   122,   120,   153,   130,   152,   126,   119,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   157,     2,
     115,   113,   117,   127,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   124,     2,   125,   155,   156,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   114,   118,   116,   123,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   131,   132,
     133,   134,   135,   136,   137,   138,   139,   140,   141,   142,
     143,   144,   145,   146,   147,   148,   149,   150,   151,   154
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     7,    10,    13,    16,    18,    20,
      22,    24,    26,    28,    30,    32,    35,    38,    40,    41,
      44,    49,    52,    56,    58,    60,    62,    64,    66,    68,
      70,    72,    74,    79,    81,    86,    88,    90,    93,    96,
      99,   102,   106,   109,   112,   115,   116,   118,   123,   124,
     127,   130,   132,   134,   135,   140,   146,   151,   155,   160,
     164,   166,   168,   171,   177,   179,   181,   183,   187,   191,
     194,   200,   206,   210,   214,   216,   218,   220,   225,   229,
     233,   234,   238,   239,   243,   244,   248,   249,   253,   254,
     258,   260,   262,   264,   268,   272,   276,   281,   286,   293,
     299,   306,   313,   320,   326,   330,   337,   339,   341,   343,
     344,   348,   350,   352,   356,   358,   360,   364,   365,   368,
     370,   376,   379,   381,   383,   385,   387,   389,   391,   394,
     397,   400,   403,   405,   407,   409,   411,   413,   415,   417,
     419,   421,   425,   428,   431,   434,   436,   439,   443,   444,
     447,   449,   452,   455,   457,   459,   460,   462,   465,   468,
     470,   473,   475,   478,   480,   483,   486,   488,   491,   494,
     496,   498,   501,   504,   506,   508,   510,   512,   514,   516,
     518,   520,   522,   524,   526,   528,   530,   532,   534,   536,
     538,   540,   542,   544,   546,   548,   550,   552,   554,   556,
     558,   560,   562,   564,   565,   567,   569,   572,   575,   577,
     581,   585,   587,   591,   592,   595,   597,   600,   601,   603,
     610,   613,   616,   619,   622,   624,   626,   628,   635,   636,
     639,   641,   645,   646,   649,   650,   653,   654,   657,   659,
     661,   664,   666,   669,   672,   676,   680,   682,   684,   688,
     692,   696,   699,   701,   703,   705,   707,   710,   713,   716,
     719,   722,   724,   726,   728,   731,   733,   735,   739,   740,
     743,   744
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     159,     0,    -1,    -1,   159,   160,    -1,   159,   167,    -1,
     159,     1,    -1,   159,    47,    -1,   163,    -1,   162,    -1,
     166,    -1,   171,    -1,   173,    -1,   161,    -1,   149,    -1,
     145,    -1,    24,   164,    -1,    58,   164,    -1,   161,    -1,
      -1,   165,   167,    -1,    44,   114,   165,   116,    -1,    92,
     148,    -1,   148,   113,   168,    -1,   171,    -1,   173,    -1,
     169,    -1,   178,    -1,   210,    -1,   219,    -1,   248,    -1,
     254,    -1,   161,    -1,    83,   114,   170,   116,    -1,   143,
      -1,    73,   114,   172,   116,    -1,   178,    -1,   138,    -1,
     172,   166,    -1,   172,   173,    -1,   172,     1,    -1,   174,
     116,    -1,    53,   114,   175,    -1,    60,   114,    -1,    88,
     114,    -1,   174,     1,    -1,    -1,   176,    -1,    79,   231,
     113,   252,    -1,    -1,   177,   178,    -1,   177,     1,    -1,
     183,    -1,   184,    -1,    -1,    20,   114,   177,   116,    -1,
      70,   254,   252,   178,   179,    -1,    30,   114,   177,   116,
      -1,   114,   177,   116,    -1,    31,   114,   177,   116,    -1,
     238,   177,   239,    -1,   197,    -1,    10,    -1,    10,   148,
      -1,    80,   161,   161,   113,   161,    -1,   142,    -1,   194,
      -1,   193,    -1,    87,   148,   178,    -1,    14,   148,   178,
      -1,    43,   178,    -1,    87,   254,   113,   254,   178,    -1,
      91,   254,   113,   254,   178,    -1,    91,   148,   178,    -1,
      82,   233,   178,    -1,   180,    -1,   182,    -1,   181,    -1,
      84,   217,   217,   178,    -1,    84,   216,   178,    -1,    16,
     161,   178,    -1,    -1,    59,   185,   178,    -1,    -1,    37,
     186,   178,    -1,    -1,    25,   187,   178,    -1,    -1,    49,
     188,   178,    -1,    -1,    90,   189,   178,    -1,   190,    -1,
     191,    -1,   192,    -1,    68,   228,   178,    -1,    71,   178,
     178,    -1,    89,   148,   178,    -1,    72,   148,   178,   178,
      -1,    83,   148,   113,   148,    -1,    63,   148,   126,   148,
     113,   195,    -1,    63,   148,   126,   148,    86,    -1,    65,
     148,   126,   148,   113,   161,    -1,    65,   148,   126,   148,
     113,   148,    -1,    64,   148,   126,   148,   113,   161,    -1,
      64,   148,   161,   113,   161,    -1,    66,   148,   161,    -1,
      63,   148,   126,   148,    66,   161,    -1,   254,    -1,   253,
      -1,   161,    -1,    -1,   196,   237,   209,    -1,   205,    -1,
     198,    -1,   201,   230,   209,    -1,   115,    -1,   117,    -1,
     199,   202,   200,    -1,    -1,   202,   204,    -1,    94,    -1,
     217,   255,   256,   212,   209,    -1,    93,   209,    -1,   203,
      -1,   206,    -1,   104,    -1,   105,    -1,   107,    -1,   118,
      -1,    21,   148,    -1,    61,   229,    -1,    27,   148,    -1,
      81,   233,    -1,    22,    -1,   207,    -1,   208,    -1,   220,
      -1,   221,    -1,   124,    -1,   125,    -1,    23,    -1,   106,
      -1,    78,   253,   148,    -1,    50,    56,    -1,    50,   195,
      -1,    75,   229,    -1,   176,    -1,    48,    56,    -1,    48,
     132,    11,    -1,    -1,   209,   210,    -1,   222,    -1,   227,
     211,    -1,   227,   222,    -1,   225,    -1,   226,    -1,    -1,
     113,    -1,   113,   214,    -1,   113,   213,    -1,   129,    -1,
     213,   129,    -1,   130,    -1,   214,   130,    -1,   132,    -1,
     132,   213,    -1,   132,   214,    -1,   133,    -1,   133,   213,
      -1,   133,   214,    -1,   215,    -1,   218,    -1,    55,   161,
      -1,    35,   161,    -1,    36,    -1,    45,    -1,   223,    -1,
      13,    -1,    12,    -1,   235,    -1,   121,    -1,   122,    -1,
     123,    -1,   100,    -1,   101,    -1,    99,    -1,   102,    -1,
      51,    -1,   224,    -1,   254,    -1,   131,    -1,   155,    -1,
     153,    -1,   152,    -1,   118,    -1,   117,    -1,   126,    -1,
     156,    -1,   156,    -1,   155,    -1,   152,    -1,   215,    -1,
     232,    -1,   219,    -1,    -1,   232,    -1,   219,    -1,   252,
     234,    -1,   135,   234,    -1,   231,    -1,   232,   120,   252,
      -1,   232,   120,   136,    -1,   136,    -1,   150,   119,   150,
      -1,    -1,   234,   126,    -1,   157,    -1,   157,   252,    -1,
      -1,    95,    -1,   217,   255,   256,   212,   230,   236,    -1,
     146,   230,    -1,   147,   230,    -1,    52,   230,    -1,   148,
     230,    -1,   240,    -1,    97,    -1,    96,    -1,   216,   230,
     241,   243,   244,   245,    -1,    -1,   109,   242,    -1,   246,
      -1,   242,   126,   246,    -1,    -1,   111,   242,    -1,    -1,
     119,   216,    -1,    -1,   108,   216,    -1,   247,    -1,   134,
      -1,   134,   247,    -1,   252,    -1,   252,   153,    -1,   252,
     110,    -1,   248,   153,   249,    -1,   248,   152,   249,    -1,
     249,    -1,   250,    -1,   250,   120,   250,    -1,   250,   119,
     250,    -1,   121,   248,   122,    -1,   152,   250,    -1,   251,
      -1,   150,    -1,   151,    -1,   140,    -1,   151,    28,    -1,
     151,    67,    -1,   151,    46,    -1,   151,    54,    -1,   151,
      26,    -1,   150,    -1,   131,    -1,   251,    -1,   152,   253,
      -1,   148,    -1,   144,    -1,   254,   153,   254,    -1,    -1,
     255,   128,    -1,    -1,   256,   127,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   455,   455,   456,   464,   471,   474,   480,   491,   494,
     497,   500,   503,   510,   514,   523,   529,   533,   539,   542,
     552,   561,   576,   611,   614,   617,   620,   623,   626,   630,
     633,   640,   647,   654,   662,   673,   677,   680,   685,   690,
     701,   712,   717,   721,   725,   731,   734,   740,   754,   757,
     773,   779,   780,   787,   790,   802,   824,   832,   844,   853,
     869,   870,   874,   878,   904,   914,   923,   930,   951,   967,
     978,  1001,  1024,  1046,  1059,  1062,  1065,  1068,  1080,  1093,
    1098,  1097,  1107,  1106,  1115,  1114,  1130,  1129,  1139,  1138,
    1146,  1149,  1152,  1158,  1173,  1178,  1202,  1220,  1237,  1244,
    1251,  1257,  1264,  1271,  1278,  1285,  1296,  1298,  1300,  1303,
    1307,  1314,  1317,  1323,  1334,  1337,  1341,  1353,  1354,  1360,
    1370,  1399,  1414,  1420,  1423,  1427,  1431,  1439,  1443,  1451,
    1460,  1466,  1472,  1478,  1479,  1483,  1486,  1489,  1494,  1499,
    1506,  1516,  1527,  1535,  1543,  1558,  1562,  1570,  1584,  1587,
    1597,  1600,  1606,  1616,  1619,  1625,  1626,  1627,  1628,  1633,
    1637,  1645,  1649,  1657,  1664,  1672,  1687,  1693,  1701,  1714,
    1717,  1723,  1736,  1750,  1761,  1772,  1775,  1780,  1785,  1791,
    1795,  1799,  1803,  1807,  1811,  1815,  1827,  1832,  1844,  1849,
    1864,  1870,  1876,  1882,  1888,  1894,  1900,  1909,  1910,  1911,
    1916,  1922,  1925,  1934,  1937,  1942,  1952,  1963,  1977,  1982,
    1992,  2008,  2009,  2018,  2019,  2032,  2035,  2102,  2103,  2109,
    2151,  2158,  2165,  2173,  2180,  2192,  2195,  2200,  2209,  2215,
    2224,  2227,  2236,  2242,  2249,  2255,  2264,  2270,  2279,  2285,
    2291,  2302,  2313,  2325,  2342,  2348,  2354,  2358,  2364,  2370,
    2379,  2382,  2388,  2393,  2399,  2405,  2411,  2417,  2423,  2429,
    2435,  2445,  2448,  2454,  2468,  2475,  2478,  2484,  2494,  2495,
    2508,  2509
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "DENEMO_MEASURES", "TEXT",
  "staffcontext", "voicecontext", "lyricscontext", "figuredbasscontext",
  "endcontext", "LILYDIRECTIVE_TOKEN", "MUSICMODE", "TONEOPTION",
  "DYNAMICMARK", "AUTOCHANGE", "ALIAS", "APPLY", "ARPEGGIO",
  "DYNAMICSCRIPT", "ACCEPTS", "ALTERNATIVE", "BAR", "BREAK", "BREATHE",
  "CHORDMODIFIERS", "CHORDS", "CHAR_T", "CLEF_", "CM_T", "CONSISTS",
  "SEQUENTIAL", "SIMULTANEOUS", "GROBDESCRIPTIONS", "CONSISTSEND",
  "DENIES", "DURATION", "EXTENDER", "FIGURES", "FIGURE_OPEN",
  "FIGURE_CLOSE", "FIGURE_BRACKET_CLOSE", "FIGURE_BRACKET_OPEN",
  "GLISSANDO", "GRACE", "HEADER", "HYPHEN", "IN_T", "INVALID", "KEY",
  "LYRICS", "MARK", "MARKUP", "MULTI_MEASURE_REST", "MIDI", "MM_T",
  "PITCH", "DEFAULT", "NAME", "PITCHNAMES", "NOTES", "PAPER", "PARTIAL_",
  "PENALTY", "PROPERTY", "OVERRIDE", "SET", "REVERT", "PT_T", "RELATIVE",
  "REMOVE", "REPEAT", "ADDLYRICS", "PARTCOMBINE", "SCORE", "SCRIPT",
  "SKIP", "SPANREQUEST", "STYLESHEET", "COMMANDSPANREQUEST", "TEMPO",
  "OUTPUTPROPERTY", "TIME_T", "TIMES", "TRANSLATOR", "TRANSPOSE", "TYPE",
  "UNSET", "CONTEXT", "LAYOUT", "LYRICSTO", "LYRICMODE", "NEWCONTEXT",
  "LILYVERSION", "DRUM_PITCH", "MUSIC_FUNCTION", "REST", "\">>\"",
  "\"<<\"", "E_CHAR", "E_EXCLAMATION", "E_SMALLER", "E_BIGGER", "E_OPEN",
  "E_CLOSE", "E_LEFTSQUARE", "E_RIGHTSQUARE", "E_TILDE", "E_BACKSLASH",
  "CHORD_BASS", "CHORD_COLON", "CHORD_MINUS", "CHORD_CARET",
  "FIGURE_SPACE", "'='", "'{'", "'<'", "'}'", "'>'", "'|'", "'/'", "'*'",
  "'('", "')'", "'~'", "'['", "']'", "'.'", "'?'", "'!'", "'''", "','",
  "DIGIT", "NOTENAME_PITCH", "TONICNAME_PITCH", "CHORDMODIFIER_PITCH",
  "DURATION_IDENTIFIER", "FRACTION", "IDENTIFIER", "SCORE_IDENTIFIER",
  "MUSIC_OUTPUT_DEF_IDENTIFIER", "NUMBER_IDENTIFIER", "REQUEST_IDENTIFIER",
  "MUSIC_IDENTIFIER", "TRANSLATOR_IDENTIFIER", "STRING_IDENTIFIER",
  "SCM_IDENTIFIER", "RESTNAME", "SKIPNAME", "STRING_", "SCM_T", "UNSIGNED",
  "REAL", "'-'", "'+'", "UNARY_MINUS", "'^'", "'_'", "':'", "$accept",
  "lilypond", "toplevel_expression", "embedded_scm",
  "chordmodifiers_block", "notenames_block", "notenames_body",
  "lilypond_header_body", "lilypond_header", "assignment",
  "identifier_init", "translator_spec_block", "translator_spec_body",
  "score_block", "score_body", "output_def", "music_output_def_body",
  "tempo_optional", "tempo_event", "Music_list", "Music",
  "Alternative_music", "Repeated_music", "Sequential_music",
  "Simultaneous_music", "Simple_music", "Composite_music", "@1", "@2",
  "@3", "@4", "@5", "relative_music", "re_rhythmed_music",
  "part_combined_music", "translator_change", "property_def", "scalar",
  "pre_events", "event_chord", "note_chord_element", "chord_open",
  "chord_close", "chord_body", "chord_body_elements",
  "music_function_chord_body", "chord_body_element", "command_element",
  "command_req", "shorthand_command_req", "verbose_command_req",
  "post_events", "post_event", "direction_reqd_event", "octave_check",
  "sup_quotes", "sub_quotes", "steno_pitch", "steno_tonic_pitch", "pitch",
  "explicit_pitch", "verbose_duration", "extender_req", "hyphen_req",
  "direction_less_event", "direction_less_char", "full_markup",
  "gen_text_def", "script_abbreviation", "script_dir", "absolute_pitch",
  "duration_length", "optional_notemode_duration", "steno_duration",
  "multiplied_duration", "fraction", "dots", "tremolo_type",
  "optional_rest", "simple_element", "simul_open", "simul_close", "chord",
  "chord_additions", "chord_notes", "chord_subtractions",
  "chord_inversion", "chord_bass", "chord_step", "chord_note",
  "number_expression", "number_term", "number_factor", "bare_number",
  "bare_unsigned", "bare_int", "string", "exclamations", "questions", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,    61,   123,    60,   125,    62,   124,    47,
      42,    40,    41,   126,    91,    93,    46,    63,    33,    39,
      44,   368,   369,   370,   371,   372,   373,   374,   375,   376,
     377,   378,   379,   380,   381,   382,   383,   384,   385,   386,
     387,   388,    45,    43,   389,    94,    95,    58
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   158,   159,   159,   159,   159,   159,   160,   160,   160,
     160,   160,   160,   161,   161,   162,   163,   164,   165,   165,
     166,   166,   167,   168,   168,   168,   168,   168,   168,   168,
     168,   168,   169,   170,   171,   172,   172,   172,   172,   172,
     173,   174,   174,   174,   174,   175,   175,   176,   177,   177,
     177,   178,   178,   179,   179,   180,   181,   181,   182,   182,
     183,   183,   183,   183,   183,   183,   183,   184,   184,   184,
     184,   184,   184,   184,   184,   184,   184,   184,   184,   184,
     185,   184,   186,   184,   187,   184,   188,   184,   189,   184,
     184,   184,   184,   190,   191,   191,   192,   193,   194,   194,
     194,   194,   194,   194,   194,   194,   195,   195,   195,   196,
     197,   197,   197,   198,   199,   200,   201,   202,   202,   203,
     204,   204,   204,   205,   205,   205,   205,   205,   205,   205,
     205,   205,   205,   206,   206,   207,   207,   207,   207,   207,
     207,   208,   208,   208,   208,   208,   208,   208,   209,   209,
     210,   210,   210,   211,   211,   212,   212,   212,   212,   213,
     213,   214,   214,   215,   215,   215,   216,   216,   216,   217,
     217,   218,   219,   220,   221,   222,   222,   222,   222,   223,
     223,   223,   223,   223,   223,   223,   224,   225,   225,   225,
     226,   226,   226,   226,   226,   226,   226,   227,   227,   227,
     228,   229,   229,   230,   230,   230,   231,   231,   232,   232,
     232,   233,   233,   234,   234,   235,   235,   236,   236,   237,
     237,   237,   237,   237,   237,   238,   239,   240,   241,   241,
     242,   242,   243,   243,   244,   244,   245,   245,   246,   246,
     246,   247,   247,   247,   248,   248,   248,   249,   249,   249,
     250,   250,   250,   251,   251,   251,   251,   251,   251,   251,
     251,   252,   252,   253,   253,   254,   254,   254,   255,   255,
     256,   256
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     2,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     1,     0,     2,
       4,     2,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     4,     1,     4,     1,     1,     2,     2,     2,
       2,     3,     2,     2,     2,     0,     1,     4,     0,     2,
       2,     1,     1,     0,     4,     5,     4,     3,     4,     3,
       1,     1,     2,     5,     1,     1,     1,     3,     3,     2,
       5,     5,     3,     3,     1,     1,     1,     4,     3,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       1,     1,     1,     3,     3,     3,     4,     4,     6,     5,
       6,     6,     6,     5,     3,     6,     1,     1,     1,     0,
       3,     1,     1,     3,     1,     1,     3,     0,     2,     1,
       5,     2,     1,     1,     1,     1,     1,     1,     2,     2,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     2,     2,     2,     1,     2,     3,     0,     2,
       1,     2,     2,     1,     1,     0,     1,     2,     2,     1,
       2,     1,     2,     1,     2,     2,     1,     2,     2,     1,
       1,     2,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     1,     1,     2,     2,     1,     3,
       3,     1,     3,     0,     2,     1,     2,     0,     1,     6,
       2,     2,     2,     2,     1,     1,     1,     6,     0,     2,
       1,     3,     0,     2,     0,     2,     0,     2,     1,     1,
       2,     1,     2,     2,     3,     3,     1,     1,     3,     3,
       3,     2,     1,     1,     1,     1,     2,     2,     2,     2,
       2,     1,     1,     1,     2,     1,     1,     3,     0,     2,
       0,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     1,     5,     0,     0,     6,     0,     0,     0,
       0,     0,     0,    14,     0,    13,     3,    12,     8,     7,
       9,     4,    10,    11,     0,    17,    15,    18,    45,    16,
      42,   109,    43,    21,   109,    44,    40,     0,     0,    41,
      46,    61,     0,     0,     0,   132,   139,    84,     0,     0,
       0,   173,    82,   109,   174,     0,    86,     0,    80,     0,
       0,     0,     0,     0,     0,     0,   109,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    88,     0,   225,
     124,   125,   140,   126,    48,   114,   127,   137,   138,    36,
      64,     0,   145,    35,    74,    76,    75,    51,    52,    90,
      91,    92,    66,    65,     0,    60,   112,   117,   203,   111,
     123,   133,   134,   135,   136,    48,   177,   176,     0,     0,
     184,   182,   183,   185,   179,   180,   181,   255,   266,   265,
     253,   254,   199,   198,   197,   215,    31,    22,    25,    23,
      24,    26,    27,    28,   150,   175,     0,   178,    29,   246,
     247,   252,    30,    20,    19,   262,   213,   261,     0,   213,
      62,   109,   109,   128,   109,   130,    48,    48,   109,    69,
     146,     0,   109,   142,     0,   108,   143,   263,   107,   106,
     109,   202,   129,   208,   201,     0,     0,     0,     0,   163,
     200,   109,     0,   109,   109,   144,     0,     0,   211,     0,
     131,   109,     0,     0,   166,   169,   109,     0,   170,   109,
       0,   109,   109,   109,     0,     0,    39,    34,    37,    38,
     203,   203,   203,   203,   203,   268,   148,   224,     0,   205,
     148,   204,     0,   172,     0,     0,     0,     0,   260,   256,
     258,   259,   257,   251,   216,   186,   194,   193,   179,   195,
     189,   192,   191,   190,   196,   151,   152,   187,   153,   154,
     188,     0,     0,     0,     0,     0,   207,     0,   206,    68,
      79,    85,     0,     0,    83,   147,    87,   264,    81,     0,
       0,     0,     0,     0,   104,   159,   161,   164,   165,    93,
     109,    94,   109,   141,     0,     0,    73,     0,   171,   167,
     168,    78,   109,    67,     0,    95,    89,    72,     0,    50,
      57,    49,   222,   220,   221,   223,   228,   270,   110,   148,
     119,   115,   116,   122,   118,   268,   113,   226,    59,    33,
       0,   250,   245,   244,   249,   248,   267,   214,    47,    56,
      58,   210,   209,     0,     0,     0,     0,   160,   162,    53,
      96,     0,   212,    97,    77,   109,   109,     0,   232,   269,
     155,   199,   149,   121,   270,    32,     0,    99,     0,     0,
     103,     0,     0,    55,    63,    70,    71,   239,   229,   230,
     238,   241,     0,   234,   156,   271,   203,   155,   105,    98,
     102,   101,   100,    48,   240,     0,   243,   242,   233,     0,
     236,   158,   157,   217,   148,     0,   231,   235,     0,   227,
     218,   219,   120,    54,   237
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,    16,    25,    18,    19,    26,    37,    20,    21,
     137,   138,   330,    22,    91,    23,    24,    39,    92,   215,
     311,   373,    94,    95,    96,    97,    98,   180,   168,   164,
     172,   212,    99,   100,   101,   102,   103,   176,   104,   105,
     106,   107,   322,   108,   228,   323,   324,   109,   110,   111,
     112,   318,   362,   255,   386,   287,   288,   205,   206,   207,
     208,   229,   113,   114,   144,   145,   257,   258,   259,   146,
     191,   182,   230,   183,   231,   200,   266,   147,   411,   226,
     115,   328,   227,   358,   378,   383,   400,   409,   379,   380,
     237,   149,   150,   151,   159,   178,   179,   317,   360
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -219
static const yytype_int16 yypact[] =
{
    -219,    24,  -219,  -219,  -108,   -82,  -219,   -76,  -108,   -71,
     -67,   -60,   -48,  -219,    -5,  -219,  -219,  -219,  -219,  -219,
    -219,  -219,  -219,  -219,    27,  -219,  -219,  -219,     2,  -219,
    -219,  1321,  -219,  -219,   326,  -219,  -219,   -87,   -32,  -219,
    -219,   -33,   -19,  -108,   -12,  -219,  -219,  -219,     0,     9,
      13,  -219,  -219,  1439,  -219,   -25,  -219,   119,  -219,   -22,
      11,    29,    43,    50,   -44,    26,  1439,    64,   -22,   154,
    -108,     3,     3,    75,   -40,    56,    82,  -219,    69,  -219,
    -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,
    -219,     6,  -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,
    -219,  -219,  -219,  -219,     8,  -219,  -219,  -219,   -22,  -219,
    -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,  -108,   -69,
    -219,  -219,  -219,  -219,    57,  -219,  -219,  -219,  -219,  -219,
    -219,   134,    78,  -219,  -219,   -85,  -219,  -219,  -219,  -219,
    -219,  -219,  -219,  -219,  -219,  -219,    93,  -219,    15,  -219,
     -29,  -219,   -20,  -219,  -219,  -219,  -219,  -219,    38,  -219,
    -219,  1439,  1439,  -219,  1439,  -219,  -219,  -219,  1439,  -219,
    -219,   194,  1439,  -219,   154,  -219,  -219,  -219,  -219,   -20,
    1439,  -219,  -219,  -219,    45,   105,   -62,   110,  -108,    91,
    -219,  1439,   -64,  1439,  1439,  -219,    90,  -108,  -219,   123,
    -219,  1439,   138,  -108,    91,  -219,  1439,   -28,  -219,  1107,
     -83,  1439,  1439,  1107,   -73,   478,  -219,  -219,  -219,  -219,
     -22,   -22,   -22,   -22,   -22,  -219,  -219,  -219,   -21,  -219,
    -219,    45,   607,  -219,   114,    57,    57,   -27,  -219,  -219,
    -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,
    -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,
     -20,    57,    57,    57,    57,    26,   127,   -85,   127,  -219,
    -219,  -219,   736,   865,  -219,  -219,  -219,  -219,  -219,    16,
     113,   118,   145,   125,  -219,  -219,  -219,   146,   144,  -219,
    1439,  -219,  1439,  -219,   163,   129,  -219,   132,  -219,   146,
     144,  -219,  1439,  -219,    26,  -219,  -219,  -219,    26,  -219,
    -219,  -219,  -219,  -219,  -219,  -219,   168,   153,   133,  -219,
    -219,  -219,  -219,  -219,  -219,  -219,   133,  -219,  -219,  -219,
     167,  -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,
    -219,  -219,  -219,   -11,   173,  -108,   174,  -219,  -219,   272,
    -219,  -108,  -219,  -219,  -219,  1214,  1214,    72,   182,  -219,
      -3,  -219,  -219,   133,   153,  -219,  -108,  -219,   151,  -108,
    -219,    95,   183,  -219,  -219,  -219,  -219,   -85,   184,  -219,
    -219,   -75,    72,   189,    91,  -219,   -22,    -3,  -219,  -219,
    -219,  -219,  -219,  -219,  -219,    72,  -219,  -219,   184,   176,
     203,   146,   144,   217,  -219,   994,  -219,  -219,   176,  -219,
    -219,  -219,   133,  -219,  -219
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -219,  -219,  -219,    -1,  -219,  -219,   306,  -219,   224,   279,
    -219,  -219,  -219,   283,  -219,   -17,  -219,  -219,   290,  -109,
     -30,  -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,
    -219,  -219,  -219,  -219,  -219,  -219,  -219,   -49,  -219,  -219,
    -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,
    -219,  -218,   286,  -219,   -66,  -195,  -194,   258,  -101,   -93,
    -219,    17,  -219,  -219,   177,  -219,  -219,  -219,  -219,  -219,
    -219,   256,  -202,   289,   -15,   257,   169,  -219,  -219,  -219,
    -219,  -219,  -219,  -219,   -52,  -219,  -219,  -219,   -63,   -46,
     299,   -36,  -106,   -55,  -130,   -53,   -26,    10,   -23
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -266
static const yytype_int16 yytable[] =
{
      17,    93,   177,   224,   141,   244,   232,   216,   152,   299,
     300,   225,   326,   118,   177,   203,   196,   140,   312,   313,
     314,   315,   316,   169,     2,     3,   243,   203,    35,   153,
     304,   170,    27,   136,   203,   396,   193,    13,    28,   192,
     308,    15,   162,    30,   184,   234,   155,    31,     4,   210,
       5,   143,   214,   184,    32,   366,   175,   272,   273,     7,
     220,    14,   290,   203,   281,   157,     9,   155,     5,   197,
     265,     6,   319,   320,   219,   367,   181,     7,   397,   202,
     265,    38,     8,    13,     9,   181,   157,    15,   189,   265,
     263,   264,   189,   204,    11,   331,   321,    10,    12,   155,
      33,   363,   368,   156,   189,   116,   117,   171,    34,   155,
     384,   189,    11,   156,   302,   160,    12,   233,   157,   177,
     260,   277,   217,   166,   385,   261,   262,   167,   157,   161,
     243,   269,   270,   265,   271,   325,   163,   338,   274,   198,
     189,   204,   276,    36,   245,   116,   117,   155,   165,   342,
     278,   267,   341,   199,   221,   222,   223,   334,   335,   185,
     238,   289,   239,   291,   292,   279,   157,   261,   262,    13,
     128,   296,    14,    15,   129,   173,   301,   186,   235,   303,
     240,   305,   306,   307,   403,   282,   412,   284,   241,   401,
     402,   187,   120,   121,   122,   123,   294,   127,   188,   235,
     128,   242,   298,   155,   209,   275,   377,   130,   131,   236,
     246,   247,   194,   128,   248,   125,   126,   213,   127,   249,
     285,   286,   157,   202,   250,   332,   333,   381,   130,   131,
     211,   280,   120,   121,   122,   123,   283,   128,   293,   336,
      13,   129,   295,   391,    15,   251,   252,   381,   253,   254,
     135,   297,   381,   337,   248,   125,   126,   329,   345,   127,
     349,   343,   350,   128,    13,   381,   344,   129,    15,   130,
     131,   174,   354,   346,   348,   347,   351,   357,   355,   352,
     353,   359,   356,   365,   405,   361,   369,   371,   133,   134,
     135,   127,   372,   382,   127,   128,    13,   393,   407,   129,
      15,   130,   131,   174,   130,   131,   174,   414,   399,   204,
     395,   408,   410,   177,    29,   218,   154,   139,    40,   389,
     142,   404,   190,   256,   195,   375,   376,   158,   268,   201,
     398,   394,   406,   148,     0,   364,    41,     0,   116,   117,
      42,   387,    43,     0,   370,     0,     0,    44,    45,    46,
     374,    47,     0,    48,     0,     0,    49,    50,     0,     0,
       0,   118,    51,    52,     0,   388,     0,   175,   390,    53,
     392,    54,     0,     0,    55,    56,    57,     0,     0,     7,
       0,     0,     0,     0,     0,    58,     9,    59,     0,    60,
      61,    62,    63,     0,    64,     0,    65,    66,    67,    10,
       0,    68,     0,     0,    69,    38,    70,    71,    72,   119,
      74,     0,     0,    75,    11,    76,    77,    78,     0,     0,
       0,     0,     0,    79,     0,   120,   121,   122,   123,     0,
      80,    81,    82,    83,     0,     0,     0,     0,     0,     0,
      84,    85,     0,     0,    86,     0,     0,   124,   125,   126,
      87,    88,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   127,     0,    90,     0,
     128,    13,     0,     0,   129,    15,   130,   131,   132,   309,
       0,   133,   134,   135,     0,     0,     0,     0,    41,     0,
       0,     0,    42,     0,    43,     0,     0,     0,     0,    44,
      45,    46,     0,    47,     0,    48,     0,     0,    49,    50,
       0,     0,     0,     0,    51,    52,     0,     0,     0,     0,
       0,    53,     0,    54,     0,     0,    55,    56,    57,     0,
    -109,     0,     0,  -109,     0,     0,     0,    58,     0,    59,
       0,    60,    61,    62,    63,     0,    64,     0,    65,    66,
      67,     0,     0,    68,     0,     0,    69,    38,    70,    71,
      72,    73,    74,     0,     0,    75,     0,    76,    77,    78,
       0,     0,     0,     0,     0,    79,     0,     0,     0,     0,
       0,     0,    80,    81,    82,    83,     0,     0,     0,     0,
       0,     0,    84,    85,   310,     0,    86,     0,     0,     0,
       0,     0,    87,    88,     0,     0,     0,     0,   309,     0,
    -109,  -109,     0,     0,     0,     0,     0,    41,     0,     0,
      90,    42,     0,    43,  -109,  -109,  -109,     0,    44,    45,
      46,     0,    47,     0,    48,     0,     0,    49,    50,     0,
       0,     0,     0,    51,    52,     0,     0,     0,     0,     0,
      53,     0,    54,     0,     0,    55,    56,    57,     0,  -109,
       0,     0,  -109,     0,     0,     0,    58,     0,    59,     0,
      60,    61,    62,    63,     0,    64,     0,    65,    66,    67,
       0,     0,    68,     0,     0,    69,    38,    70,    71,    72,
      73,    74,     0,     0,    75,     0,    76,    77,    78,     0,
       0,     0,     0,   327,    79,     0,     0,     0,     0,     0,
       0,    80,    81,    82,    83,     0,     0,     0,     0,     0,
       0,    84,    85,     0,     0,    86,     0,     0,     0,     0,
       0,    87,    88,     0,     0,     0,     0,   309,     0,  -109,
    -109,     0,     0,     0,     0,     0,    41,     0,     0,    90,
      42,     0,    43,  -109,  -109,  -109,     0,    44,    45,    46,
       0,    47,     0,    48,     0,     0,    49,    50,     0,     0,
       0,     0,    51,    52,     0,     0,     0,     0,     0,    53,
       0,    54,     0,     0,    55,    56,    57,     0,  -109,     0,
       0,  -109,     0,     0,     0,    58,     0,    59,     0,    60,
      61,    62,    63,     0,    64,     0,    65,    66,    67,     0,
       0,    68,     0,     0,    69,    38,    70,    71,    72,    73,
      74,     0,     0,    75,     0,    76,    77,    78,     0,     0,
       0,     0,     0,    79,     0,     0,     0,     0,     0,     0,
      80,    81,    82,    83,     0,     0,     0,     0,     0,     0,
      84,    85,   339,     0,    86,     0,     0,     0,     0,     0,
      87,    88,     0,     0,     0,     0,   309,     0,  -109,  -109,
       0,     0,     0,     0,     0,    41,     0,     0,    90,    42,
       0,    43,  -109,  -109,  -109,     0,    44,    45,    46,     0,
      47,     0,    48,     0,     0,    49,    50,     0,     0,     0,
       0,    51,    52,     0,     0,     0,     0,     0,    53,     0,
      54,     0,     0,    55,    56,    57,     0,  -109,     0,     0,
    -109,     0,     0,     0,    58,     0,    59,     0,    60,    61,
      62,    63,     0,    64,     0,    65,    66,    67,     0,     0,
      68,     0,     0,    69,    38,    70,    71,    72,    73,    74,
       0,     0,    75,     0,    76,    77,    78,     0,     0,     0,
       0,     0,    79,     0,     0,     0,     0,     0,     0,    80,
      81,    82,    83,     0,     0,     0,     0,     0,     0,    84,
      85,   340,     0,    86,     0,     0,     0,     0,     0,    87,
      88,     0,     0,     0,     0,   309,     0,  -109,  -109,     0,
       0,     0,     0,     0,    41,     0,     0,    90,    42,     0,
      43,  -109,  -109,  -109,     0,    44,    45,    46,     0,    47,
       0,    48,     0,     0,    49,    50,     0,     0,     0,     0,
      51,    52,     0,     0,     0,     0,     0,    53,     0,    54,
       0,     0,    55,    56,    57,     0,  -109,     0,     0,  -109,
       0,     0,     0,    58,     0,    59,     0,    60,    61,    62,
      63,     0,    64,     0,    65,    66,    67,     0,     0,    68,
       0,     0,    69,    38,    70,    71,    72,    73,    74,     0,
       0,    75,     0,    76,    77,    78,     0,     0,     0,     0,
       0,    79,     0,     0,     0,     0,     0,     0,    80,    81,
      82,    83,     0,     0,     0,     0,     0,     0,    84,    85,
     413,     0,    86,     0,     0,     0,     0,    41,    87,    88,
       0,    42,     0,    43,     0,     0,  -109,  -109,    44,    45,
      46,     0,    47,     0,    48,     0,    90,    49,    50,     0,
    -109,  -109,  -109,    51,    52,     0,     0,     0,     0,     0,
      53,     0,    54,     0,     0,    55,    56,    57,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,    59,     0,
      60,    61,    62,    63,     0,    64,     0,    65,    66,    67,
       0,     0,    68,     0,     0,    69,    38,    70,    71,    72,
      73,    74,     0,     0,    75,     0,    76,    77,    78,     0,
       0,     0,     0,     0,    79,     0,     0,     0,     0,     0,
       0,    80,    81,    82,    83,     0,     0,     0,     0,     0,
    -265,    84,    85,     0,    41,    86,     0,     0,    42,     0,
      43,    87,    88,     0,     0,    44,    45,    46,     0,    47,
       0,    48,     0,     0,    49,    50,     0,     0,     0,    90,
      51,    52,     0,     0,     0,     0,     0,    53,     0,    54,
    -265,     0,    55,    56,    57,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,    59,     0,    60,    61,    62,
      63,     0,    64,     0,    65,    66,    67,     0,     0,    68,
       0,     0,    69,    38,    70,    71,    72,    73,    74,     0,
       0,    75,     0,    76,    77,    78,     0,     0,     0,     0,
       0,    79,     0,     0,     0,     0,     0,     0,    80,    81,
      82,    83,     0,     0,     0,     0,     0,     0,    84,    85,
       0,    41,    86,     0,     0,    42,     0,    43,    87,    88,
       0,     0,    44,    45,    46,     0,    47,     0,    48,     0,
       0,    49,    50,     0,     0,     0,    90,    51,    52,     0,
       0,     0,     0,     0,    53,     0,    54,   265,     0,    55,
      56,    57,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,    59,     0,    60,    61,    62,    63,     0,    64,
       0,    65,    66,    67,     0,     0,    68,     0,     0,    69,
      38,    70,    71,    72,    73,    74,     0,     0,    75,     0,
      76,    77,    78,     0,     0,     0,     0,     0,    79,     0,
       0,     0,     0,     0,     0,    80,    81,    82,    83,     0,
       0,     0,     0,     0,     0,    84,    85,     0,     0,    86,
       0,     0,     0,     0,     0,    87,    88,     0,     0,    41,
       0,     0,     0,    42,     0,    43,     0,     0,     0,    89,
      44,    45,    46,    90,    47,     0,    48,     0,     0,    49,
      50,     0,     0,     0,     0,    51,    52,     0,     0,     0,
       0,     0,    53,     0,    54,     0,     0,    55,    56,    57,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
      59,     0,    60,    61,    62,    63,     0,    64,     0,    65,
      66,    67,     0,     0,    68,     0,     0,    69,    38,    70,
      71,    72,    73,    74,     0,     0,    75,     0,    76,    77,
      78,     0,     0,     0,     0,     0,    79,     0,     0,     0,
       0,     0,     0,    80,    81,    82,    83,     0,     0,     0,
       0,     0,     0,    84,    85,     0,     0,    86,     0,     0,
       0,     0,     0,    87,    88,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    90
};

static const yytype_int16 yycheck[] =
{
       1,    31,    57,   104,    34,   135,   115,     1,    34,   204,
     204,   104,   230,    35,    69,    55,    69,    34,   220,   221,
     222,   223,   224,    53,     0,     1,   132,    55,     1,   116,
     113,    56,   114,    34,    55,   110,    66,   145,   114,    65,
     113,   149,    43,   114,    59,   114,   131,   114,    24,    75,
      44,    34,    78,    68,   114,    66,    57,   166,   167,    53,
      52,   148,   192,    55,   126,   150,    60,   131,    44,    70,
     153,    47,    93,    94,    91,    86,    59,    53,   153,   148,
     153,    79,    58,   145,    60,    68,   150,   149,   132,   153,
     119,   120,   132,   133,    88,   122,   117,    73,    92,   131,
     148,   319,   113,   135,   132,    12,    13,   132,   113,   131,
     113,   132,    88,   135,   207,   148,    92,   118,   150,   174,
     146,   174,   116,   114,   127,   152,   153,   114,   150,   148,
     236,   161,   162,   153,   164,   228,   148,   267,   168,   136,
     132,   133,   172,   116,    51,    12,    13,   131,   148,   279,
     180,   113,   136,   150,   146,   147,   148,   263,   264,   148,
      26,   191,    28,   193,   194,   120,   150,   152,   153,   145,
     144,   201,   148,   149,   148,    56,   206,   148,   121,   209,
      46,   211,   212,   213,   386,   186,   404,   188,    54,   384,
     384,   148,    99,   100,   101,   102,   197,   140,   148,   121,
     144,    67,   203,   131,   148,    11,   134,   150,   151,   152,
     117,   118,   148,   144,   121,   122,   123,   148,   140,   126,
     129,   130,   150,   148,   131,   261,   262,   357,   150,   151,
     148,   126,    99,   100,   101,   102,   126,   144,   148,   265,
     145,   148,   119,   148,   149,   152,   153,   377,   155,   156,
     157,   113,   382,   126,   121,   122,   123,   143,   113,   140,
     290,   148,   292,   144,   145,   395,   148,   148,   149,   150,
     151,   152,   302,   148,   130,   129,   113,   109,   304,   150,
     148,   128,   308,   116,   393,   152,   113,   113,   155,   156,
     157,   140,    20,   111,   140,   144,   145,   114,   399,   148,
     149,   150,   151,   152,   150,   151,   152,   408,   119,   133,
     126,   108,    95,   368,     8,    91,    37,    34,    28,   368,
      34,   387,    64,   146,    68,   355,   356,    38,   159,    72,
     382,   377,   395,    34,    -1,   325,    10,    -1,    12,    13,
      14,   364,    16,    -1,   345,    -1,    -1,    21,    22,    23,
     351,    25,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,
      -1,    35,    36,    37,    -1,   366,    -1,   368,   369,    43,
     371,    45,    -1,    -1,    48,    49,    50,    -1,    -1,    53,
      -1,    -1,    -1,    -1,    -1,    59,    60,    61,    -1,    63,
      64,    65,    66,    -1,    68,    -1,    70,    71,    72,    73,
      -1,    75,    -1,    -1,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    87,    88,    89,    90,    91,    -1,    -1,
      -1,    -1,    -1,    97,    -1,    99,   100,   101,   102,    -1,
     104,   105,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
     114,   115,    -1,    -1,   118,    -1,    -1,   121,   122,   123,
     124,   125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   140,    -1,   142,    -1,
     144,   145,    -1,    -1,   148,   149,   150,   151,   152,     1,
      -1,   155,   156,   157,    -1,    -1,    -1,    -1,    10,    -1,
      -1,    -1,    14,    -1,    16,    -1,    -1,    -1,    -1,    21,
      22,    23,    -1,    25,    -1,    27,    -1,    -1,    30,    31,
      -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    -1,    -1,
      -1,    43,    -1,    45,    -1,    -1,    48,    49,    50,    -1,
      52,    -1,    -1,    55,    -1,    -1,    -1,    59,    -1,    61,
      -1,    63,    64,    65,    66,    -1,    68,    -1,    70,    71,
      72,    -1,    -1,    75,    -1,    -1,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    87,    -1,    89,    90,    91,
      -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,   114,   115,   116,    -1,   118,    -1,    -1,    -1,
      -1,    -1,   124,   125,    -1,    -1,    -1,    -1,     1,    -1,
     132,   133,    -1,    -1,    -1,    -1,    -1,    10,    -1,    -1,
     142,    14,    -1,    16,   146,   147,   148,    -1,    21,    22,
      23,    -1,    25,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    -1,    -1,    -1,
      43,    -1,    45,    -1,    -1,    48,    49,    50,    -1,    52,
      -1,    -1,    55,    -1,    -1,    -1,    59,    -1,    61,    -1,
      63,    64,    65,    66,    -1,    68,    -1,    70,    71,    72,
      -1,    -1,    75,    -1,    -1,    78,    79,    80,    81,    82,
      83,    84,    -1,    -1,    87,    -1,    89,    90,    91,    -1,
      -1,    -1,    -1,    96,    97,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,   106,   107,    -1,    -1,    -1,    -1,    -1,
      -1,   114,   115,    -1,    -1,   118,    -1,    -1,    -1,    -1,
      -1,   124,   125,    -1,    -1,    -1,    -1,     1,    -1,   132,
     133,    -1,    -1,    -1,    -1,    -1,    10,    -1,    -1,   142,
      14,    -1,    16,   146,   147,   148,    -1,    21,    22,    23,
      -1,    25,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,
      -1,    -1,    36,    37,    -1,    -1,    -1,    -1,    -1,    43,
      -1,    45,    -1,    -1,    48,    49,    50,    -1,    52,    -1,
      -1,    55,    -1,    -1,    -1,    59,    -1,    61,    -1,    63,
      64,    65,    66,    -1,    68,    -1,    70,    71,    72,    -1,
      -1,    75,    -1,    -1,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    87,    -1,    89,    90,    91,    -1,    -1,
      -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
     114,   115,   116,    -1,   118,    -1,    -1,    -1,    -1,    -1,
     124,   125,    -1,    -1,    -1,    -1,     1,    -1,   132,   133,
      -1,    -1,    -1,    -1,    -1,    10,    -1,    -1,   142,    14,
      -1,    16,   146,   147,   148,    -1,    21,    22,    23,    -1,
      25,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
      -1,    36,    37,    -1,    -1,    -1,    -1,    -1,    43,    -1,
      45,    -1,    -1,    48,    49,    50,    -1,    52,    -1,    -1,
      55,    -1,    -1,    -1,    59,    -1,    61,    -1,    63,    64,
      65,    66,    -1,    68,    -1,    70,    71,    72,    -1,    -1,
      75,    -1,    -1,    78,    79,    80,    81,    82,    83,    84,
      -1,    -1,    87,    -1,    89,    90,    91,    -1,    -1,    -1,
      -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,   114,
     115,   116,    -1,   118,    -1,    -1,    -1,    -1,    -1,   124,
     125,    -1,    -1,    -1,    -1,     1,    -1,   132,   133,    -1,
      -1,    -1,    -1,    -1,    10,    -1,    -1,   142,    14,    -1,
      16,   146,   147,   148,    -1,    21,    22,    23,    -1,    25,
      -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,    -1,
      36,    37,    -1,    -1,    -1,    -1,    -1,    43,    -1,    45,
      -1,    -1,    48,    49,    50,    -1,    52,    -1,    -1,    55,
      -1,    -1,    -1,    59,    -1,    61,    -1,    63,    64,    65,
      66,    -1,    68,    -1,    70,    71,    72,    -1,    -1,    75,
      -1,    -1,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    87,    -1,    89,    90,    91,    -1,    -1,    -1,    -1,
      -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,
     116,    -1,   118,    -1,    -1,    -1,    -1,    10,   124,   125,
      -1,    14,    -1,    16,    -1,    -1,   132,   133,    21,    22,
      23,    -1,    25,    -1,    27,    -1,   142,    30,    31,    -1,
     146,   147,   148,    36,    37,    -1,    -1,    -1,    -1,    -1,
      43,    -1,    45,    -1,    -1,    48,    49,    50,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    59,    -1,    61,    -1,
      63,    64,    65,    66,    -1,    68,    -1,    70,    71,    72,
      -1,    -1,    75,    -1,    -1,    78,    79,    80,    81,    82,
      83,    84,    -1,    -1,    87,    -1,    89,    90,    91,    -1,
      -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,   106,   107,    -1,    -1,    -1,    -1,    -1,
     113,   114,   115,    -1,    10,   118,    -1,    -1,    14,    -1,
      16,   124,   125,    -1,    -1,    21,    22,    23,    -1,    25,
      -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,   142,
      36,    37,    -1,    -1,    -1,    -1,    -1,    43,    -1,    45,
     153,    -1,    48,    49,    50,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    59,    -1,    61,    -1,    63,    64,    65,
      66,    -1,    68,    -1,    70,    71,    72,    -1,    -1,    75,
      -1,    -1,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    87,    -1,    89,    90,    91,    -1,    -1,    -1,    -1,
      -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,
      -1,    10,   118,    -1,    -1,    14,    -1,    16,   124,   125,
      -1,    -1,    21,    22,    23,    -1,    25,    -1,    27,    -1,
      -1,    30,    31,    -1,    -1,    -1,   142,    36,    37,    -1,
      -1,    -1,    -1,    -1,    43,    -1,    45,   153,    -1,    48,
      49,    50,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      59,    -1,    61,    -1,    63,    64,    65,    66,    -1,    68,
      -1,    70,    71,    72,    -1,    -1,    75,    -1,    -1,    78,
      79,    80,    81,    82,    83,    84,    -1,    -1,    87,    -1,
      89,    90,    91,    -1,    -1,    -1,    -1,    -1,    97,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,   118,
      -1,    -1,    -1,    -1,    -1,   124,   125,    -1,    -1,    10,
      -1,    -1,    -1,    14,    -1,    16,    -1,    -1,    -1,   138,
      21,    22,    23,   142,    25,    -1,    27,    -1,    -1,    30,
      31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    -1,
      -1,    -1,    43,    -1,    45,    -1,    -1,    48,    49,    50,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    59,    -1,
      61,    -1,    63,    64,    65,    66,    -1,    68,    -1,    70,
      71,    72,    -1,    -1,    75,    -1,    -1,    78,    79,    80,
      81,    82,    83,    84,    -1,    -1,    87,    -1,    89,    90,
      91,    -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,   106,   107,    -1,    -1,    -1,
      -1,    -1,    -1,   114,   115,    -1,    -1,   118,    -1,    -1,
      -1,    -1,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   159,     0,     1,    24,    44,    47,    53,    58,    60,
      73,    88,    92,   145,   148,   149,   160,   161,   162,   163,
     166,   167,   171,   173,   174,   161,   164,   114,   114,   164,
     114,   114,   114,   148,   113,     1,   116,   165,    79,   175,
     176,    10,    14,    16,    21,    22,    23,    25,    27,    30,
      31,    36,    37,    43,    45,    48,    49,    50,    59,    61,
      63,    64,    65,    66,    68,    70,    71,    72,    75,    78,
      80,    81,    82,    83,    84,    87,    89,    90,    91,    97,
     104,   105,   106,   107,   114,   115,   118,   124,   125,   138,
     142,   172,   176,   178,   180,   181,   182,   183,   184,   190,
     191,   192,   193,   194,   196,   197,   198,   199,   201,   205,
     206,   207,   208,   220,   221,   238,    12,    13,    35,    83,
      99,   100,   101,   102,   121,   122,   123,   140,   144,   148,
     150,   151,   152,   155,   156,   157,   161,   168,   169,   171,
     173,   178,   210,   219,   222,   223,   227,   235,   248,   249,
     250,   251,   254,   116,   167,   131,   135,   150,   231,   252,
     148,   148,   161,   148,   187,   148,   114,   114,   186,   178,
      56,   132,   188,    56,   152,   161,   195,   251,   253,   254,
     185,   219,   229,   231,   232,   148,   148,   148,   148,   132,
     215,   228,   254,   178,   148,   229,   253,   161,   136,   150,
     233,   233,   148,    55,   133,   215,   216,   217,   218,   148,
     254,   148,   189,   148,   254,   177,     1,   116,   166,   173,
      52,   146,   147,   148,   216,   217,   237,   240,   202,   219,
     230,   232,   177,   161,   114,   121,   152,   248,    26,    28,
      46,    54,    67,   250,   252,    51,   117,   118,   121,   126,
     131,   152,   153,   155,   156,   211,   222,   224,   225,   226,
     254,   152,   153,   119,   120,   153,   234,   113,   234,   178,
     178,   178,   177,   177,   178,    11,   178,   253,   178,   120,
     126,   126,   161,   126,   161,   129,   130,   213,   214,   178,
     252,   178,   178,   148,   161,   119,   178,   113,   161,   213,
     214,   178,   217,   178,   113,   178,   178,   178,   113,     1,
     116,   178,   230,   230,   230,   230,   230,   255,   209,    93,
      94,   117,   200,   203,   204,   217,   209,    96,   239,   143,
     170,   122,   249,   249,   250,   250,   254,   126,   252,   116,
     116,   136,   252,   148,   148,   113,   148,   129,   130,   178,
     178,   113,   150,   148,   178,   254,   254,   109,   241,   128,
     256,   152,   210,   209,   255,   116,    66,    86,   113,   113,
     161,   113,    20,   179,   161,   178,   178,   134,   242,   246,
     247,   252,   111,   243,   113,   127,   212,   256,   161,   195,
     161,   148,   161,   114,   247,   126,   110,   153,   242,   119,
     244,   213,   214,   230,   212,   177,   246,   216,   108,   245,
      95,   236,   209,   116,   216
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value, Location); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
#endif
{
  if (!yyvaluep)
    return;
  YYUSE (yylocationp);
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep, yylocationp)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  YY_LOCATION_PRINT (yyoutput, *yylocationp);
  YYFPRINTF (yyoutput, ": ");
  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yylsp, yyrule)
    YYSTYPE *yyvsp;
    YYLTYPE *yylsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       , &(yylsp[(yyi + 1) - (yynrhs)])		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, yylsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
#else
static void
yydestruct (yymsg, yytype, yyvaluep, yylocationp)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
    YYLTYPE *yylocationp;
#endif
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;
/* Location data for the look-ahead symbol.  */
YYLTYPE yylloc;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;

  /* The location stack.  */
  YYLTYPE yylsa[YYINITDEPTH];
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;
  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[2];

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;
  yylsp = yyls;
#if YYLTYPE_IS_TRIVIAL
  /* Initialize the default location before parsing starts.  */
  yylloc.first_line   = yylloc.last_line   = 1;
  yylloc.first_column = yylloc.last_column = 0;
#endif

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;
	YYLTYPE *yyls1 = yyls;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yyls1, yysize * sizeof (*yylsp),
		    &yystacksize);
	yyls = yyls1;
	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);
	YYSTACK_RELOCATE (yyls);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;
  *++yylsp = yylloc;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location.  */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 3:
#line 456 "./lyparser.y"
    { 
		if(lily_file)  {
		  lily_file = g_list_concat (lily_file, (yyvsp[(2) - (2)].scm));
//list_tree("In parser toplevel", lily_file);
		} else {
		lily_file = (yyvsp[(2) - (2)].scm);
		}
	;}
    break;

  case 4:
#line 464 "./lyparser.y"
    { 
		if(lily_file)  {
		 	 lily_file= g_list_concat(lily_file, (yyvsp[(2) - (2)].scm));
		} else {
		lily_file = (yyvsp[(2) - (2)].scm);
		}
	;}
    break;

  case 5:
#line 471 "./lyparser.y"
    {
		error_level_  = 1;
	;}
    break;

  case 6:
#line 474 "./lyparser.y"
    {
		error_level_  = 1;
	;}
    break;

  case 7:
#line 481 "./lyparser.y"
    { /* this is the \pitchnames thing in the include files - we will
	     perhaps leave this out for now - see below for where the
	     include file is parsed for the pitchname table - we have to
	     recognize at least one set of pitchnames of course ...*/
	     (yyval.scm) = g_list_append(NULL, (yyvsp[(1) - (1)].scm)); 
	  /* creates a new mudelaobj structure comprising the token TEXT,
	     and the Gstring pointed to by input_text, which is	reset to NULL.
	     We ignore the value of notename_block since we don't interpret 
	     it further */ 
	;}
    break;

  case 8:
#line 491 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 9:
#line 494 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 10:
#line 497 "./lyparser.y"
    {
		(yyval.scm) =  (yyvsp[(1) - (1)].scm);/* add this score to the root data list lily_file */
	;}
    break;

  case 11:
#line 500 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 12:
#line 503 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 13:
#line 510 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (1)].generic));
	(yyval.scm) = g_list_append(NULL,n);
	;}
    break;

  case 14:
#line 514 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (1)].gstr));
	((nodegstr*)n)->gstr = (yyvsp[(1) - (1)].gstr).gstr;
	(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 15:
#line 523 "./lyparser.y"
    {  (yyval.scm) = (yyvsp[(2) - (2)].scm); 
/*	intercept this at lexical level*/
	;}
    break;

  case 16:
#line 529 "./lyparser.y"
    {  (yyval.scm) = (yyvsp[(2) - (2)].scm); ;}
    break;

  case 17:
#line 533 "./lyparser.y"
    {
	(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 18:
#line 539 "./lyparser.y"
    {
	(yyval.scm) = NULL;
	;}
    break;

  case 19:
#line 542 "./lyparser.y"
    { 
		if((yyvsp[(1) - (2)].scm)) {
			(yyval.scm) = g_list_concat((yyvsp[(1) - (2)].scm), (yyvsp[(2) - (2)].scm));
			}
		else 
			(yyval.scm) = (yyvsp[(2) - (2)].scm);
	;}
    break;

  case 20:
#line 552 "./lyparser.y"
    {
	MALLOC_NODE(n1, (yyvsp[(1) - (4)].generic));
	MALLOC_NODE(n4, (yyvsp[(4) - (4)].generic));
	n1->user_string = g_strconcat((yyvsp[(1) - (4)].generic).user_string, (yyvsp[(2) - (4)].generic).user_string, NULL);
	(yyval.scm) = g_list_append(NULL,n1);
	(yyval.scm) = g_list_concat((yyval.scm),(yyvsp[(3) - (4)].scm));
	(yyval.scm) = g_list_append((yyval.scm), n4);	
	;}
    break;

  case 21:
#line 562 "./lyparser.y"
    {
		MALLOC_NODE(n,(yyvsp[(1) - (2)].generic));
	    g_free(n->user_string);
	    n->user_string = (yyvsp[(2) - (2)].gstr).gstr->str;
	set_identifier("lilyversion", typed_glist (g_list_append(NULL,n), STRING_IDENTIFIER));
	(yyval.scm) = NULL;
	;}
    break;

  case 22:
#line 576 "./lyparser.y"
    {
		GList *ret;
		MALLOC_NODE (n1, (yyvsp[(1) - (3)].gstr));
		MALLOC_NODE (n2, (yyvsp[(2) - (3)].generic));
		if (! regular_identifier_b ((yyvsp[(1) - (3)].gstr).gstr->str))
		{
		   g_warning (_("Identifier should have alphabetic characters"
				" only please"));
		}
#if DEBUG
		g_print("got to assignment with %s %s ",(yyvsp[(1) - (3)].gstr).gstr->str, 
			(yyvsp[(2) - (3)].generic).user_string);
		if ((yyvsp[(3) - (3)].branch)->type == STRING_IDENTIFIER)
			g_print("%s\n", u_str((yyvsp[(3) - (3)].branch)->branch));
		else
			g_print("type %d\n", (yyvsp[(3) - (3)].branch)->type);
#endif
		/* $3 because mid rule action deleted */
	        set_identifier ((yyvsp[(1) - (3)].gstr).gstr->str, (yyvsp[(3) - (3)].branch));
		
		((nodeglist*)n2)->branch = (yyvsp[(3) - (3)].branch)->branch;
		ret = g_list_append(NULL, n2);
		(yyval.scm) = g_list_prepend(ret, n1);
/*
 TODO: devise standard for protection in parser.

  The parser stack lives on the C-stack, which means that
all objects can be unprotected as soon as they're here.

*/
	;}
    break;

  case 23:
#line 611 "./lyparser.y"
    { /* I don't think this can ever get used, once defined! */
		(yyval.branch) = typed_glist ((yyvsp[(1) - (1)].scm), SCORE_IDENTIFIER);
	;}
    break;

  case 24:
#line 614 "./lyparser.y"
    {
		(yyval.branch) = typed_glist ((yyvsp[(1) - (1)].scm), MUSIC_OUTPUT_DEF_IDENTIFIER);
	;}
    break;

  case 25:
#line 617 "./lyparser.y"
    {
		(yyval.branch) = typed_glist (g_list_append(NULL,(yyvsp[(1) - (1)].scm)), TRANSLATOR_IDENTIFIER);
	;}
    break;

  case 26:
#line 620 "./lyparser.y"
    {
		(yyval.branch) = typed_glist ((yyvsp[(1) - (1)].scm), MUSIC_IDENTIFIER);
	;}
    break;

  case 27:
#line 623 "./lyparser.y"
    {
		(yyval.branch) = typed_glist ((yyvsp[(1) - (1)].scm), REQUEST_IDENTIFIER);
	;}
    break;

  case 28:
#line 626 "./lyparser.y"
    {
		MALLOC_NODE(n,(yyvsp[(1) - (1)].f));
		(yyval.branch) = typed_glist (g_list_append(NULL,n), DURATION_IDENTIFIER);
	;}
    break;

  case 29:
#line 630 "./lyparser.y"
    {
		(yyval.branch) = typed_glist ((yyvsp[(1) - (1)].scm), STRING_IDENTIFIER);
	;}
    break;

  case 30:
#line 633 "./lyparser.y"
    {
		nodegeneric x;
		x.type = STRING_IDENTIFIER;
		x.user_string = strdup((yyvsp[(1) - (1)].gstr).gstr->str);
		MALLOC_NODE(n,x);
		(yyval.branch) = typed_glist (g_list_append(NULL,n), STRING_IDENTIFIER);
	;}
    break;

  case 31:
#line 640 "./lyparser.y"
    {
		(yyval.branch) = typed_glist ((yyvsp[(1) - (1)].scm), SCM_IDENTIFIER);
	;}
    break;

  case 32:
#line 648 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(3) - (4)].scm);
	;}
    break;

  case 33:
#line 654 "./lyparser.y"
    {
	;}
    break;

  case 34:
#line 662 "./lyparser.y"
    {
		MALLOC_NODE(n,(yyvsp[(1) - (4)].generic));
		n->user_string = g_strconcat((yyvsp[(1) - (4)].generic).user_string, "{" , NULL);
		/*FIXME memory leak of $1,2 */
		((nodeglist*)n)->post_user_string = "}";
		((nodeglist*)n)->branch = (yyvsp[(3) - (4)].scm);
		(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 35:
#line 673 "./lyparser.y"
    {

		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 36:
#line 677 "./lyparser.y"
    {
		(yyval.scm) = g_list_append(NULL, (yyvsp[(1) - (1)].id).id);
	;}
    break;

  case 37:
#line 680 "./lyparser.y"
    {
	(yyval.scm) = (yyvsp[(1) - (2)].scm);
	lyerror ("parser should have caught this");
		/*intercept this at lexical level*/	
	;}
    break;

  case 38:
#line 685 "./lyparser.y"
    {
	(yyval.scm) = (yyvsp[(1) - (2)].scm);
	lyerror ("parser should have caught this");
		/*intercept this at lexical level*/
	;}
    break;

  case 39:
#line 690 "./lyparser.y"
    {
	(yyval.scm) = (yyvsp[(1) - (2)].scm);
	lyerror("score_body error");
	;}
    break;

  case 40:
#line 701 "./lyparser.y"
    {
	//	MALLOC_NODE(n2, $2)
	//	$$ = g_list_append($1, n2);
	(yyval.scm) = NULL;
		
/*		THIS-> lexer_-> scopes_.pop ();*/
	;}
    break;

  case 41:
#line 712 "./lyparser.y"
    {
	   //     $$ = NULL;
	//	set_identifier("midi_tempo", typed_glist ($3, STRING_IDENTIFIER));
	//	$$ = $3;
	;}
    break;

  case 42:
#line 717 "./lyparser.y"
    {
		/* caught by lexer - does not occur*/
		lyerror ("parser should have caught this-paper");
	;}
    break;

  case 43:
#line 721 "./lyparser.y"
    {
		/* caught by lexer - does not occur*/
		lyerror ("parser should have caught this - layout");
	;}
    break;

  case 44:
#line 725 "./lyparser.y"
    {
		lyerror("music_output_def_body error");
		;}
    break;

  case 45:
#line 731 "./lyparser.y"
    {
		(yyval.scm) = NULL;
	;}
    break;

  case 46:
#line 734 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 47:
#line 740 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(4) - (4)].i));
	((nodei*)n)->i = (yyvsp[(4) - (4)].i).i;
	(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 48:
#line 754 "./lyparser.y"
    {
		(yyval.scm) = NULL;
	;}
    break;

  case 49:
#line 757 "./lyparser.y"
    {
		if((yyvsp[(1) - (2)].scm)) 
		{
#if DEBUG
g_print("building up a music list now from %s to %s\n", u_str((yyvsp[(1) - (2)].scm)),  u_str((yyvsp[(2) - (2)].scm)));
#endif
			(yyval.scm) = g_list_concat((yyvsp[(1) - (2)].scm), (yyvsp[(2) - (2)].scm));
		}
		else 
			(yyval.scm) = (yyvsp[(2) - (2)].scm);
	;}
    break;

  case 50:
#line 773 "./lyparser.y"
    {
	;}
    break;

  case 52:
#line 781 "./lyparser.y"
    {
			(yyval.scm) = (yyvsp[(1) - (1)].scm);
		;}
    break;

  case 53:
#line 787 "./lyparser.y"
    {
			(yyval.scm) = NULL;
	;}
    break;

  case 54:
#line 790 "./lyparser.y"
    {
		GList* ret;
		MALLOC_NODE(n1, (yyvsp[(1) - (4)].generic));
		MALLOC_NODE(n4, (yyvsp[(4) - (4)].generic));
		stradd((yyvsp[(1) - (4)].generic), (yyvsp[(2) - (4)].generic));
		ret = g_list_append(NULL, n1);
		ret = g_list_concat(ret, (yyvsp[(3) - (4)].scm));
		(yyval.scm) = g_list_append(ret, n4);
	;}
    break;

  case 55:
#line 803 "./lyparser.y"
    {
	GList* ret;
	MALLOC_NODE(n1, (yyvsp[(1) - (5)].generic));
	stradd((yyvsp[(1) - (5)].generic), (yyvsp[(2) - (5)].gstr));
	stradd((yyvsp[(1) - (5)].generic), (yyvsp[(3) - (5)].i));
	ret = g_list_append(NULL, n1);
	ret = g_list_concat(ret, (yyvsp[(4) - (5)].scm));
	if((yyvsp[(5) - (5)].scm))
		ret = g_list_concat(ret, (yyvsp[(5) - (5)].scm));
	(yyval.scm) = ret;

#if 0
	
		(yyval.scm) = new_data_el (REPEAT,); FINISH THIS
		(yyval.scm) = g_list_append ((yyval.scm), new_data_el (STRINGL)....need to have the input strings
	for each token available - use a struct in yylval.
#endif
	;}
    break;

  case 56:
#line 824 "./lyparser.y"
    {
		MALLOC_NODE(n1, (yyvsp[(1) - (4)].generic));
		n1->user_string = g_strconcat((yyvsp[(1) - (4)].generic).user_string, "{", NULL);
		/*FIXME memory leak $1,2 and $4*/
		((nodeglist*)n1)->post_user_string = "}" ;
		((nodeglist*)n1)->branch = (yyvsp[(3) - (4)].scm);
		(yyval.scm) = g_list_prepend(NULL, n1);
	;}
    break;

  case 57:
#line 832 "./lyparser.y"
    {
		MALLOC_NODE(n1, (yyvsp[(1) - (3)].generic));
		n1->user_string = "{";	
		((nodeglist*)n1)->post_user_string = "}";
		/* FIXME memory leak of $3 */
		((nodeglist*)n1)->type = SEQUENTIAL;
		((nodeglist*)n1)->branch = (yyvsp[(2) - (3)].scm);
		(yyval.scm) = g_list_prepend(NULL, n1);
	;}
    break;

  case 58:
#line 844 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		(yyval.scm) = new Simultaneous_music (SCM_EOL);
		(yyval.scm)->set_mus_property ("elements", ly_car ((yyvsp[(3) - (4)].scm)));
		(yyval.scm)->set_spot(THIS->here_input());
#endif
	;}
    break;

  case 59:
#line 853 "./lyparser.y"
    {
	/* we don't try to disambiguate chords on one stave from 
	   notes one to a staff here
	   that is done in generate_chords() called by create_score() */
// RRR		MALLOC_NODE(n1, $2);
		nodegeneric*n1 = (nodegeneric*)g_malloc0(sizeof(nodegeneric));
		n1->user_string = "<<";
		((nodeglist*)n1)->post_user_string = ">>";
		/* FIXME memory leak of $3 */
		((nodeglist*)n1)->type = SIMULTANEOUS;
		((nodeglist*)n1)->branch = (yyvsp[(2) - (3)].scm);
		(yyval.scm) = g_list_prepend(NULL, n1);
	;}
    break;

  case 60:
#line 869 "./lyparser.y"
    { (yyval.scm) = (yyvsp[(1) - (1)].scm); ;}
    break;

  case 61:
#line 870 "./lyparser.y"
    {
		DenemoObject *mud = lily_directive_new ((yyvsp[(1) - (1)].gstr).user_string);		
		(yyval.scm) = g_list_append(NULL,mud);
	;}
    break;

  case 62:
#line 874 "./lyparser.y"
    {
		DenemoObject *mud = lily_directive_new (g_strconcat((yyvsp[(1) - (2)].gstr).user_string, (yyvsp[(2) - (2)].gstr).user_string));	
		(yyval.scm) = g_list_append(NULL,mud);
	;}
    break;

  case 63:
#line 878 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		SCM pred = (yyvsp[(2) - (5)].scm);
		if (!gh_symbol_p ((yyvsp[(3) - (5)].scm)))
		{
		  THIS->parser_error (_ ("Second argument must be a symbol")); 
		}
		/* Should check # args */
		if (!gh_procedure_p (pred))
		{
		   THIS->parser_error (_ ("First argument must be a procedure"
					  " taking one argument"));
		}

		Music *m = new Music (SCM_EOL);
		m->set_mus_property ("predicate", pred);
		m->set_mus_property ("grob-property", (yyvsp[(3) - (5)].scm));
		m->set_mus_property ("grob-value",  (yyvsp[(5) - (5)].scm));
		m->set_mus_property ("iterator-ctor",
		Output_property_music_iterator::constructor_cxx_function);

		(yyval.scm) = m;
#endif
	;}
    break;

  case 64:
#line 904 "./lyparser.y"
    { /* this may be ok now ... */
		/* has to be big enough	for DenemoObject access 
		   eg when writing start_ticks in break into measures... */
		nodeid *n = (nodeid*)g_malloc0(sizeof(nodegeneric));
		n->type = (yyvsp[(1) - (1)].id).type;
		n->user_string = (yyvsp[(1) - (1)].id).user_string;
		n->id = (yyvsp[(1) - (1)].id).id;
		(yyval.scm) = g_list_append(NULL, n);

	;}
    break;

  case 65:
#line 914 "./lyparser.y"
    {

		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	/* FIXME - we really don't want to put all these nodes into
	   the music, as denemo will have to go over them -
	   amalgamate the strings into a TEXT node */
		

	;}
    break;

  case 66:
#line 923 "./lyparser.y"
    {
	LATER_MESSAGE((yyloc).first_line);
	;}
    break;

  case 67:
#line 930 "./lyparser.y"
    {
		MALLOC_NODE (n1, (yyvsp[(1) - (3)].minimal));
		if(!strcmp("Staff",(yyvsp[(2) - (3)].gstr).gstr->str)) 
		  n1->type = staffcontext;
		else if(!strcmp("Voice",(yyvsp[(2) - (3)].gstr).gstr->str)) 
		  n1->type = voicecontext;
		else if(!strcmp("Lyrics",(yyvsp[(2) - (3)].gstr).gstr->str)) 
		  n1->type = lyricscontext;
		else if(!strcmp("FiguredBass",(yyvsp[(2) - (3)].gstr).gstr->str)) 
		  n1->type = figuredbasscontext;
		else 
	          n1->type = TEXT;/*ignore other contexts at present */
		n1->user_string = g_strconcat((yyvsp[(1) - (3)].minimal).user_string, (yyvsp[(2) - (3)].gstr).user_string, 
					      NULL);	
		if (n1->type == TEXT)
			(yyval.scm) = g_list_prepend((yyvsp[(3) - (3)].scm), n1);
		else
			(yyval.scm) = g_list_append(g_list_prepend((yyvsp[(3) - (3)].scm), n1), 
					   &endcontextnode);	  
		/* FIXME memory leak of $2 */
	;}
    break;

  case 68:
#line 951 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		Music * chm = new Music_wrapper (SCM_EOL);
		chm->set_mus_property ("element", (yyvsp[(3) - (3)].scm));
		chm->set_mus_property ("iterator-ctor", 
			Auto_change_iterator::constructor_cxx_function);

		scm_gc_unprotect_object ((yyvsp[(3) - (3)].scm));
		chm->set_mus_property ("what", (yyvsp[(2) - (3)].gstr)); 

		(yyval.scm) = chm;
		chm->set_spot (*(yyvsp[(3) - (3)].scm)->origin ());
#endif
	;}
    break;

  case 69:
#line 967 "./lyparser.y"
    {
	DenemoObject *start, *end;
	start = newgracestart();
	end =  newgraceend();
	start->user_string = (yyvsp[(1) - (2)].generic).user_string;
	/* prevent denemo generating a "}" string
	   this will take some effort to handle better than this hack. FIXME */
	end->user_string = g_strdup(" ");
	(yyval.scm) = g_list_append(g_list_prepend ((yyvsp[(2) - (2)].scm),start ), end);

	;}
    break;

  case 70:
#line 978 "./lyparser.y"
    {
		MALLOC_NODE(n1, (yyvsp[(1) - (5)].minimal));
 		((nodegstr*)n1)->gstr = (yyvsp[(4) - (5)].gstr).gstr;
		if(!strcmp("Staff",(yyvsp[(2) - (5)].gstr).gstr->str)) 
		   n1->type = staffcontext;
		else if(!strcmp("Voice",(yyvsp[(2) - (5)].gstr).gstr->str)) 
		   n1->type = voicecontext;
		else if(!strcmp("Lyrics",(yyvsp[(2) - (5)].gstr).gstr->str)) 
		   n1->type = lyricscontext;
		else if(!strcmp("FiguredBass",(yyvsp[(2) - (5)].gstr).gstr->str)) 
		   n1->type = figuredbasscontext;
		else 
		   n1->type = TEXT;/*ignore other contexts at present */
		n1->user_string = g_strconcat((yyvsp[(1) - (5)].minimal).user_string, (yyvsp[(2) - (5)].gstr).user_string, 
					      "=", (yyvsp[(4) - (5)].gstr).user_string, 
					      NULL);	
		if (n1->type == TEXT)
			(yyval.scm) = g_list_prepend((yyvsp[(5) - (5)].scm), n1);
		else
			(yyval.scm) = g_list_append(g_list_prepend((yyvsp[(5) - (5)].scm), n1), 
					   &endcontextnode);	  
		/* FIXME memory leak $2 $3 $4 */
	;}
    break;

  case 71:
#line 1001 "./lyparser.y"
    {
		MALLOC_NODE(n1, (yyvsp[(1) - (5)].generic));
 		((nodegstr*)n1)->gstr = (yyvsp[(4) - (5)].gstr).gstr;
		if(!strcmp("Staff",(yyvsp[(2) - (5)].gstr).gstr->str)) 
		   n1->type = staffcontext;
		else if(!strcmp("Voice",(yyvsp[(2) - (5)].gstr).gstr->str)) 
		   n1->type = voicecontext;
		else if(!strcmp("Lyrics",(yyvsp[(2) - (5)].gstr).gstr->str)) 
		   n1->type = lyricscontext;
		else if(!strcmp("FiguredBass",(yyvsp[(2) - (5)].gstr).gstr->str)) 
		   n1->type = figuredbasscontext;
		else 
		   n1->type = TEXT;/*ignore other contexts at present */
		n1->user_string = g_strconcat((yyvsp[(1) - (5)].generic).user_string, (yyvsp[(2) - (5)].gstr).user_string, 
					      "=", (yyvsp[(4) - (5)].gstr).user_string, 
					      NULL);	
		if (n1->type == TEXT)
			(yyval.scm) = g_list_prepend((yyvsp[(5) - (5)].scm), n1);
		else
			(yyval.scm) = g_list_append(g_list_prepend((yyvsp[(5) - (5)].scm), n1), 
					   &endcontextnode);	  
		/* FIXME memory leak $2 $3 $4 */
	;}
    break;

  case 72:
#line 1024 "./lyparser.y"
    {
		MALLOC_NODE (n1, (yyvsp[(1) - (3)].generic));
		if(!strcmp("Staff",(yyvsp[(2) - (3)].gstr).gstr->str)) 
		  n1->type = staffcontext;
		else if(!strcmp("Voice",(yyvsp[(2) - (3)].gstr).gstr->str)) 
		  n1->type = voicecontext;
		else if(!strcmp("Lyrics",(yyvsp[(2) - (3)].gstr).gstr->str)) 
		{
		  n1->type = lyricscontext;
		}
		else if(!strcmp("FiguredBass",(yyvsp[(2) - (3)].gstr).gstr->str)) 
		  n1->type = figuredbasscontext;
		else 
	          n1->type = TEXT;/*ignore other contexts at present */
		n1->user_string = g_strconcat((yyvsp[(1) - (3)].generic).user_string, (yyvsp[(2) - (3)].gstr).user_string, 
					      NULL);	
		if (n1->type == TEXT)
			(yyval.scm) = g_list_prepend((yyvsp[(3) - (3)].scm), n1);
		else
			(yyval.scm) = g_list_append(g_list_prepend((yyvsp[(3) - (3)].scm), n1), 
					   &endcontextnode);	  
	;}
    break;

  case 73:
#line 1048 "./lyparser.y"
    {DenemoObject *tupopen, *tupclose;
		tupopen = newtupopen ((yyvsp[(2) - (3)].t).t.a, (yyvsp[(2) - (3)].t).t.b);
		tupclose = newtupclose ();
		//g_assert(ntype($3)==SEQUENTIAL);
		
		tupopen->user_string = g_strconcat((yyvsp[(1) - (3)].generic).user_string, 
						   (yyvsp[(2) - (3)].t).user_string, u_str((yyvsp[(3) - (3)].scm)),
						   NULL);
		tupclose->user_string = u_post_str((yyvsp[(3) - (3)].scm));
		(yyval.scm) = g_list_append(g_list_prepend (br((yyvsp[(3) - (3)].scm)), tupopen), tupclose);
	;}
    break;

  case 74:
#line 1059 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 75:
#line 1062 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 76:
#line 1065 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 77:
#line 1068 "./lyparser.y"
    {

 		/* could we try to display transposed?? later FIXME */
		GList* ret;
		MALLOC_NODE(n1, (yyvsp[(1) - (4)].generic));
		n1->user_string = g_strconcat((yyvsp[(1) - (4)].generic).user_string, (yyvsp[(2) - (4)].n).user_string,  (yyvsp[(3) - (4)].n).user_string,
					      NULL);	
		ret = g_list_append(NULL, n1);
		(yyval.scm) = g_list_concat(ret, (yyvsp[(4) - (4)].scm));
		
		
	;}
    break;

  case 78:
#line 1080 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		(yyval.scm) = new Transposed_music (SCM_EOL);
		Music *p = (yyvsp[(3) - (3)].scm);
		Pitch pit = *unsmob_pitch ((yyvsp[(2) - (3)].n));

		p->transpose (pit);
		(yyval.scm)->set_mus_property ("element", p);
		scm_gc_unprotect_object (p);
#endif	
	;}
    break;

  case 79:
#line 1093 "./lyparser.y"
    {
		u_str((yyvsp[(2) - (3)].scm)) = g_strconcat((yyvsp[(1) - (3)].generic).user_string, u_str((yyvsp[(2) - (3)].scm)), NULL);
		(yyval.scm) = g_list_concat((yyvsp[(2) - (3)].scm), (yyvsp[(3) - (3)].scm));
	;}
    break;

  case 80:
#line 1098 "./lyparser.y"
    { push_note_state (); ;}
    break;

  case 81:
#line 1101 "./lyparser.y"
    { 
		MALLOC_NODE(n1, (yyvsp[(1) - (3)].generic));
		(yyval.scm) = g_list_prepend((yyvsp[(3) - (3)].scm), n1);
		pop_state();
		;}
    break;

  case 82:
#line 1107 "./lyparser.y"
    { push_figuredbass_state (); ;}
    break;

  case 83:
#line 1109 "./lyparser.y"
    {
		MALLOC_NODE(n1, (yyvsp[(1) - (3)].generic));
		(yyval.scm) = g_list_prepend((yyvsp[(3) - (3)].scm), n1);
		pop_state();
	;}
    break;

  case 84:
#line 1115 "./lyparser.y"
    { push_chord_state (); ;}
    break;

  case 85:
#line 1117 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		  Music * chm = new Un_relativable_music ;
		  chm->set_mus_property ("element", (yyvsp[(3) - (3)].scm));
		  scm_gc_unprotect_object ((yyvsp[(3) - (3)].scm)->self_scm());
		  (yyval.scm) = chm;

		  THIS->lexer_->pop_state ();
#endif	
	;}
    break;

  case 86:
#line 1130 "./lyparser.y"
    { push_lyric_state (); ;}
    break;

  case 87:
#line 1132 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
		MALLOC_NODE(n, (yyvsp[(1) - (3)].generic))
		(yyval.scm) = g_list_prepend((yyvsp[(3) - (3)].scm), n);
		pop_state();
	;}
    break;

  case 88:
#line 1139 "./lyparser.y"
    { push_lyric_state (); ;}
    break;

  case 89:
#line 1141 "./lyparser.y"
    {
		MALLOC_NODE(n, (yyvsp[(1) - (3)].generic))
		(yyval.scm) = g_list_prepend((yyvsp[(3) - (3)].scm), n);
		pop_state();
	;}
    break;

  case 90:
#line 1146 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 91:
#line 1149 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 92:
#line 1152 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 93:
#line 1158 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (3)].generic))
	g_warning("\\relative not yet handled - do not edit graphically");
	n->user_string = g_strconcat((yyvsp[(1) - (3)].generic).user_string, (yyvsp[(2) - (3)].n).user_string, NULL);
	(yyval.scm) = g_list_prepend((yyvsp[(3) - (3)].scm), n);
	/* we have to record the pitch in the node and then use it to start
	relative interpretation of future CHORD nodes FIXME
	well, not really we have to record it in a variable akin to 
	default_duration_ which can then be tracked to determine the 
	semantics of future notes??? Or has
	Music already been interpreted when this rule is activated...  */
	;}
    break;

  case 94:
#line 1173 "./lyparser.y"
    {
		MALLOC_NODE(n, (yyvsp[(1) - (3)].generic));
		((nodeglist*)n)->branch = g_list_append(g_list_append(NULL, g_list_append(NULL, (yyvsp[(2) - (3)].scm))), g_list_append(NULL, (yyvsp[(3) - (3)].scm))); /* ADDLYRICS is a branch containing two GLists */
		(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 95:
#line 1178 "./lyparser.y"
    {
	    	GList *g;
		MALLOC_NODE(n, (yyvsp[(1) - (3)].generic));
		g_free(n->user_string);
		if (*(yyvsp[(2) - (3)].gstr).gstr->str == '\"')
		{
		    GString * name = g_string_erase ((yyvsp[(2) - (3)].gstr).gstr, 0, 1);
		    name = g_string_truncate(name, name->len - 1);
		    n->user_string = name->str;
		}
		else
		    n->user_string = (yyvsp[(2) - (3)].gstr).gstr->str;
		if (ntype ((yyvsp[(3) - (3)].scm)) == NEWCONTEXT)
		{
		    g = (yyvsp[(3) - (3)].scm);
		    (yyvsp[(3) - (3)].scm) = g_list_remove_link ( (yyvsp[(3) - (3)].scm), g);
		    g_list_free(g);
		}
		((nodeglist*)n)->branch = (yyvsp[(3) - (3)].scm);
		(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 96:
#line 1202 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		Part_combine_music * p = new Part_combine_music (SCM_EOL);

		p->set_mus_property ("what", (yyvsp[(2) - (4)].gstr));
		p->set_mus_property ("elements", gh_list ((yyvsp[(3) - (4)].scm),(yyvsp[(4) - (4)].scm), SCM_UNDEFINED));  

		scm_gc_unprotect_object ((yyvsp[(3) - (4)].scm));
		scm_gc_unprotect_object ((yyvsp[(4) - (4)].scm));  

		(yyval.scm) = p;
#endif	
	;}
    break;

  case 97:
#line 1220 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		Music * t = new Music (SCM_EOL);
		t->set_mus_property ("iterator-ctor",
			Change_iterator::constructor_cxx_function);
		t-> set_mus_property ("change-to-type", (yyvsp[(2) - (4)].gstr));
		t-> set_mus_property ("change-to-id", (yyvsp[(4) - (4)].gstr));

		(yyval.scm) = t;
		(yyval.scm)->set_spot (THIS->here_input ());
#endif	
	;}
    break;

  case 98:
#line 1237 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (6)].generic))
	 n->user_string = g_strconcat((yyvsp[(1) - (6)].generic).user_string, (yyvsp[(2) - (6)].gstr).user_string, 
				      ".", (yyvsp[(4) - (6)].gstr).user_string, 
				      "=", u_str((yyvsp[(6) - (6)].scm)), NULL);
	(yyval.scm) = g_list_append(NULL, n); /* FIXME memory leak*/
	;}
    break;

  case 99:
#line 1244 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (5)].generic))
	 n->user_string = g_strconcat((yyvsp[(1) - (5)].generic).user_string, (yyvsp[(2) - (5)].gstr).user_string, 
				      (yyvsp[(3) - (5)].i).user_string, (yyvsp[(4) - (5)].gstr).user_string, 
				      (yyvsp[(5) - (5)].generic).user_string, NULL);
	(yyval.scm) = g_list_append(NULL, n); /* FIXME memory leak*/
	;}
    break;

  case 100:
#line 1251 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (6)].generic))
	n->user_string = g_strconcat((yyvsp[(1) - (6)].generic).user_string, (yyvsp[(2) - (6)].gstr).user_string, 
				     (yyvsp[(3) - (6)].i).user_string, (yyvsp[(4) - (6)].gstr).user_string, (yyvsp[(5) - (6)].generic).user_string, u_str((yyvsp[(6) - (6)].scm)), NULL);
	(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 101:
#line 1257 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (6)].generic))
	n->user_string = g_strconcat((yyvsp[(1) - (6)].generic).user_string, (yyvsp[(2) - (6)].gstr).user_string, 
				     (yyvsp[(3) - (6)].i).user_string, (yyvsp[(4) - (6)].gstr).user_string, 
				(yyvsp[(5) - (6)].generic).user_string, (yyvsp[(6) - (6)].gstr).user_string, NULL);
	(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 102:
#line 1264 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (6)].generic));
	n->user_string = g_strconcat((yyvsp[(1) - (6)].generic).user_string, (yyvsp[(2) - (6)].gstr).user_string, 
				     (yyvsp[(3) - (6)].i).user_string, (yyvsp[(4) - (6)].gstr).user_string, 
				     (yyvsp[(5) - (6)].generic).user_string, u_str((yyvsp[(6) - (6)].scm)), NULL);
	(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 103:
#line 1271 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (5)].generic));
	n->user_string = g_strconcat((yyvsp[(1) - (5)].generic).user_string, (yyvsp[(2) - (5)].gstr).user_string, 
				     u_str ((yyvsp[(3) - (5)].scm)), (yyvsp[(4) - (5)].generic).user_string, 
				     u_str ((yyvsp[(5) - (5)].scm)), NULL);
	(yyval.scm) = g_list_append (NULL, n);
        ;}
    break;

  case 104:
#line 1278 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (3)].generic));
	n->user_string = g_strconcat((yyvsp[(1) - (3)].generic).user_string, (yyvsp[(2) - (3)].gstr).user_string, 
				     u_str ((yyvsp[(3) - (3)].scm)), NULL);
	(yyval.scm) = g_list_append (NULL, n);
        ;}
    break;

  case 105:
#line 1285 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (6)].generic));
	n->user_string = g_strconcat((yyvsp[(1) - (6)].generic).user_string, (yyvsp[(2) - (6)].gstr).user_string, 
				     (yyvsp[(3) - (6)].i).user_string, (yyvsp[(4) - (6)].gstr).user_string, 
				     (yyvsp[(5) - (6)].generic).user_string, u_str ((yyvsp[(6) - (6)].scm)), NULL);
	(yyval.scm) = g_list_append (NULL, n);
	;}
    break;

  case 106:
#line 1296 "./lyparser.y"
    { MALLOC_NODE(n, (yyvsp[(1) - (1)].gstr));(yyval.scm) = g_list_append (NULL, n); 
			/*FIXME copy value */;}
    break;

  case 107:
#line 1298 "./lyparser.y"
    { MALLOC_NODE(n, (yyvsp[(1) - (1)].i));(yyval.scm) = g_list_append (NULL, n);
			  /*FIXME copy value */ ;}
    break;

  case 108:
#line 1300 "./lyparser.y"
    { (yyval.scm) = (yyvsp[(1) - (1)].scm); ;}
    break;

  case 110:
#line 1307 "./lyparser.y"
    {
/* things like start cresc, simple_element end cresc */

	set_post_events ((DenemoObject *) ((yyvsp[(2) - (3)].scm)->data), u_str ((yyvsp[(2) - (3)].scm)), (yyvsp[(3) - (3)].scm));
			
	(yyval.scm) = (yyvsp[(2) - (3)].scm);/* FIXME memory leak */
	;}
    break;

  case 111:
#line 1314 "./lyparser.y"
    {
	(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 112:
#line 1317 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 113:
#line 1324 "./lyparser.y"
    {
	    GList *firstchord = br ((yyvsp[(1) - (3)].scm));
	    if (firstchord && firstchord->data)
		changedur ((DenemoObject *)(firstchord->data), (yyvsp[(2) - (3)].f).t1.a, (yyvsp[(2) - (3)].f).t1.b);
	    set_post_events ((DenemoObject *) ((yyvsp[(1) - (3)].scm)->data), u_str ((yyvsp[(1) - (3)].scm)), (yyvsp[(3) - (3)].scm));
	    (yyval.scm) = (yyvsp[(1) - (3)].scm);
	;}
    break;

  case 116:
#line 1342 "./lyparser.y"
    {
	    nodegeneric*n1 = (nodegeneric*)g_malloc0(sizeof(nodegeneric));
	    n1->user_string = "<";	
	    ((nodeglist*)n1)->post_user_string = ">";
	    ((nodeglist*)n1)->type = SIMULTANEOUS;
	    ((nodeglist*)n1)->branch = (yyvsp[(2) - (3)].scm);
	    (yyval.scm) = g_list_prepend(NULL, n1);
	;}
    break;

  case 117:
#line 1353 "./lyparser.y"
    { (yyval.scm) = NULL; ;}
    break;

  case 118:
#line 1354 "./lyparser.y"
    {
		(yyval.scm) = g_list_concat ((yyvsp[(1) - (2)].scm), (yyvsp[(2) - (2)].scm));
	;}
    break;

  case 119:
#line 1360 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
                (yyval.scm) = scm_list_2 ((yyvsp[(1) - (1)].generic), make_input ((yyloc)));
#endif
        ;}
    break;

  case 120:
#line 1370 "./lyparser.y"
    {
		DenemoObject *mud = newchord ( 0, 0, 0);

		if (!note_state_b ())
			lyerror (_ ("Have to be in Note mode for notes"));

		addtone ( mud, (yyvsp[(1) - (5)].n).n.mid_c_offset, (yyvsp[(1) - (5)].n).n.enshift, 0);/*FIXME should be
							using $1.n directly */

#define no ((note*)((((chord *)mud->object)->notes)->data))
		if ((yyvsp[(3) - (5)].i).i % 2) 
		{
			no->showaccidental = TRUE;
			( (chord *)mud->object)->hasanacc = TRUE;
		}
		if ((yyvsp[(2) - (5)].i).i % 2 ) {
			no->showaccidental = TRUE;
			( (chord *)mud->object)->hasanacc = TRUE;
		}
#undef no

		mud->user_string = (yyvsp[(1) - (5)].n).user_string;
		if ((yyvsp[(2) - (5)].i).i) stradd ( (*mud),(yyvsp[(2) - (5)].i));
		if ((yyvsp[(3) - (5)].i).i) stradd ( (*mud),(yyvsp[(3) - (5)].i));
		//stradd ( (*mud),$4);

	        set_post_events (mud, mud->user_string, (yyvsp[(5) - (5)].scm));
		(yyval.scm) = g_list_append (NULL,mud);
        ;}
    break;

  case 121:
#line 1399 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
                Music *n = MY_MAKE_MUSIC ("NoteEvent");
                n->set_property ("duration", (yyvsp[(2) - (2)].scm));
                n->set_property ("drum-type", (yyvsp[(1) - (2)].generic));
                n->set_spot ((yyloc));

                if (scm_is_pair ((yyvsp[(2) - (2)].scm))) {
                        SCM arts = scm_reverse_x ((yyvsp[(2) - (2)].scm), SCM_EOL);
                        n->set_property ("articulations", arts);
                }
                (yyval.scm) = n;
#endif // LATER
        ;}
    break;

  case 122:
#line 1414 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
        ;}
    break;

  case 123:
#line 1420 "./lyparser.y"
    {
	(yyval.scm) = (yyvsp[(1) - (1)].scm);
;}
    break;

  case 124:
#line 1423 "./lyparser.y"
    {
	DenemoObject *mud = lily_directive_new ("[");	
		(yyval.scm) = g_list_append(NULL,mud);
	;}
    break;

  case 125:
#line 1427 "./lyparser.y"
    {
	DenemoObject *mud = lily_directive_new ("]");	
		(yyval.scm) = g_list_append(NULL,mud);
	;}
    break;

  case 126:
#line 1431 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		(yyval.scm) = new Music (gh_list (gh_cons (ly_symbol2scm ("name"), ly_symbol2scm ("separator")), SCM_UNDEFINED));
		(yyval.scm)->set_spot (THIS->here_input ());
#endif
	;}
    break;

  case 127:
#line 1439 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (1)].generic));
	(yyval.scm) = g_list_append(NULL, n); /* this node used to be used by denemo to split the glist into measures */
	;}
    break;

  case 128:
#line 1443 "./lyparser.y"
    {
	                 DenemoObject *mud = lily_directive_new (g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].gstr).user_string, NULL));
		         (yyval.scm) = g_list_append(NULL,mud);

			//MALLOC_NODE(n, $1);
		  //n->user_string = g_strconcat($1.user_string, $2.user_string, NULL);/* FIXME memory leaks */
		 // $$ = g_list_append(NULL, n);
	;}
    break;

  case 129:
#line 1451 "./lyparser.y"
    {
	//	g_warning ("\\partial not currently supported");
		DenemoObject *mud = newchord( (yyvsp[(2) - (2)].f).t1.a, (yyvsp[(2) - (2)].f).t1.b,0);
		mud->type = PARTIAL;
/* FIXME - we need to store all four ints then use them to determine how much measure to skip */
		mud->user_string = g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].f).user_string,NULL);
		/* FIXME memory leaks on strings concatenated */
		(yyval.scm) = g_list_append(NULL,mud);
	;}
    break;

  case 130:
#line 1460 "./lyparser.y"
    {
  		DenemoObject *mud = dnm_newclefobj (cleftypefromname((yyvsp[(2) - (2)].gstr).gstr->str));
		mud->user_string = g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].gstr).user_string,NULL);
		/* FIXME memory leaks on strings concatenated */
		(yyval.scm) = g_list_append(NULL,mud);
	;}
    break;

  case 131:
#line 1466 "./lyparser.y"
    {
  		DenemoObject *mud = dnm_newtimesigobj ((yyvsp[(2) - (2)].t).t.a, (yyvsp[(2) - (2)].t).t.b);
		mud->user_string = g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].t).user_string,NULL);
		/* FIXME memory leaks on strings concatenated */
		(yyval.scm) = g_list_append(NULL,mud);
	;}
    break;

  case 132:
#line 1472 "./lyparser.y"
    { /* ignore */
		(yyval.scm) = NULL;
	;}
    break;

  case 133:
#line 1478 "./lyparser.y"
    { (yyval.scm) = (yyvsp[(1) - (1)].scm); ;}
    break;

  case 134:
#line 1479 "./lyparser.y"
    { (yyval.scm) = (yyvsp[(1) - (1)].scm); ;}
    break;

  case 135:
#line 1483 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 136:
#line 1486 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 137:
#line 1489 "./lyparser.y"
    {
	DenemoObject *mud = lily_directive_new ("[");	
		(yyval.scm) = g_list_append(NULL,mud);

	;}
    break;

  case 138:
#line 1494 "./lyparser.y"
    {
	DenemoObject *mud = lily_directive_new ("]");	
		(yyval.scm) = g_list_append(NULL,mud);	

	;}
    break;

  case 139:
#line 1499 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		(yyval.scm) = new Breathing_sign_req;
#endif
	;}
    break;

  case 140:
#line 1506 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		(yyval.scm) = new Porrectus_req;
#endif
	;}
    break;

  case 141:
#line 1516 "./lyparser.y"
    { /*TODO: junkme */
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		Span_req * sp = new Span_req;
		sp-> set_span_dir ( Direction ((yyvsp[(2) - (3)].i)));
		sp->set_mus_property ("span-type",(yyvsp[(3) - (3)].gstr));
		sp->set_spot (THIS->here_input ());
		(yyval.scm) = sp;
#endif
	;}
    break;

  case 142:
#line 1527 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		Mark_req * m = new Mark_req;
		(yyval.scm) = m;
#endif
	;}
    break;

  case 143:
#line 1535 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		Mark_req *m = new Mark_req;
		m->set_mus_property ("label", (yyvsp[(2) - (2)].scm));
		(yyval.scm) = m;
#endif
	;}
    break;

  case 144:
#line 1543 "./lyparser.y"
    {
	/* denemo doesn't want to know? */
	MALLOC_NODE(n, (yyvsp[(1) - (2)].generic))

	n->type=TEXT;
	n->user_string = g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].f).user_string, NULL);
	(yyval.scm) = g_list_append(NULL,n);

#ifdef LATER
		Skip_req * skip = new Skip_req;
		skip->set_mus_property ("duration", (yyvsp[(2) - (2)].f));

		(yyval.scm) = skip;
#endif
	;}
    break;

  case 145:
#line 1558 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
//		$$ = $1;
	;}
    break;

  case 146:
#line 1562 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		Key_change_req *key= new Key_change_req;
		(yyval.scm) = key;
#endif
	;}
    break;

  case 147:
#line 1570 "./lyparser.y"
    {
		gchar *keyname = keytoname((yyvsp[(2) - (3)].t).t.a, (yyvsp[(2) - (3)].t).t.b);
		DenemoObject *mud;
		/* the convoluted conversion is due to historical mismatch of lily and denemo*/
		if (!strcmp((yyvsp[(3) - (3)].gstr).gstr->str, "minor"))	
			mud = dnm_newkeyobj(keynametonumber(keyname)-3, TRUE, 0);
		else
 		 	mud = dnm_newkeyobj(keynametonumber(keyname), FALSE, 0);
		mud->user_string = g_strconcat((yyvsp[(1) - (3)].generic).user_string, (yyvsp[(2) - (3)].t).user_string, (yyvsp[(3) - (3)].gstr).user_string, NULL);
		(yyval.scm) = g_list_append(NULL,mud);
	;}
    break;

  case 148:
#line 1584 "./lyparser.y"
    {
	(yyval.scm) = NULL;
	;}
    break;

  case 149:
#line 1587 "./lyparser.y"
    {
		if((yyvsp[(1) - (2)].scm)) {
			(yyval.scm) = g_list_concat((yyvsp[(1) - (2)].scm), (yyvsp[(2) - (2)].scm));
			}
		else 
			(yyval.scm) = (yyvsp[(2) - (2)].scm);
	;}
    break;

  case 150:
#line 1597 "./lyparser.y"
    {
                (yyval.scm) = (yyvsp[(1) - (1)].scm);
        ;}
    break;

  case 151:
#line 1600 "./lyparser.y"
    {
	   /* script_dir is an integer saying whether up down or centred
	      denemo doesn't understand this yet */
	    u_str((yyvsp[(2) - (2)].scm)) = g_strconcat ( (yyvsp[(1) - (2)].i).user_string, u_str((yyvsp[(2) - (2)].scm)), NULL);
	    (yyval.scm) = (yyvsp[(2) - (2)].scm);
        ;}
    break;

  case 152:
#line 1606 "./lyparser.y"
    {
	   /* script_dir is an integer saying whether up down or centred
	      denemo doesn't understand this yet */
	    u_str((yyvsp[(2) - (2)].scm)) = g_strconcat ( (yyvsp[(1) - (2)].i).user_string, u_str((yyvsp[(2) - (2)].scm)), NULL);
	    (yyval.scm) = (yyvsp[(2) - (2)].scm);
        ;}
    break;

  case 153:
#line 1616 "./lyparser.y"
    {
                (yyval.scm) = (yyvsp[(1) - (1)].scm);
        ;}
    break;

  case 154:
#line 1619 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
        ;}
    break;

  case 155:
#line 1625 "./lyparser.y"
    {  ;}
    break;

  case 156:
#line 1626 "./lyparser.y"
    { (yyval.i).i = 0; ;}
    break;

  case 157:
#line 1627 "./lyparser.y"
    { (yyval.i) = (yyvsp[(2) - (2)].i); ;}
    break;

  case 158:
#line 1628 "./lyparser.y"
    { (yyval.i) = (yyvsp[(2) - (2)].i); ;}
    break;

  case 159:
#line 1633 "./lyparser.y"
    {
		(yyvsp[(1) - (1)].i).i = 1;
		(yyval.i) = (yyvsp[(1) - (1)].i);
	;}
    break;

  case 160:
#line 1637 "./lyparser.y"
    {
		(yyvsp[(1) - (2)].i).i ++;
		(yyvsp[(1) - (2)].i).user_string = g_strconcat((yyvsp[(1) - (2)].i).user_string,(yyvsp[(2) - (2)].i).user_string, NULL);/*FIXME memory leak */
		(yyval.i) = (yyvsp[(1) - (2)].i);
	;}
    break;

  case 161:
#line 1645 "./lyparser.y"
    {
		(yyvsp[(1) - (1)].i).i = 1;
		(yyval.i) = (yyvsp[(1) - (1)].i);	
	;}
    break;

  case 162:
#line 1649 "./lyparser.y"
    {
		(yyvsp[(1) - (2)].i).i ++ ;
		(yyvsp[(1) - (2)].i).user_string = g_strconcat((yyvsp[(1) - (2)].i).user_string,(yyvsp[(2) - (2)].i).user_string, NULL);/*FIXME memory leak */
		(yyval.i) = (yyvsp[(1) - (2)].i);
	;}
    break;

  case 163:
#line 1657 "./lyparser.y"
    {
		char notename = 'a' + (yyvsp[(1) - (1)].t).t.a;
		int enshift = (yyvsp[(1) - (1)].t).t.b;
		(yyval.n).user_string = (yyvsp[(1) - (1)].t).user_string;
		(yyval.n).n.enshift = enshift;
		(yyval.n).n.mid_c_offset = pitchtomid_c_offset (notename, 0);		
	;}
    break;

  case 164:
#line 1664 "./lyparser.y"
    {
		char notename = 'a' + (yyvsp[(1) - (2)].t).t.a;
		int enshift = (yyvsp[(1) - (2)].t).t.b;
		int sups=(yyvsp[(2) - (2)].i).i;
		(yyval.n).user_string = g_strconcat((yyvsp[(1) - (2)].t).user_string, (yyvsp[(2) - (2)].i).user_string, NULL);
		(yyval.n).n.enshift = enshift;
		(yyval.n).n.mid_c_offset = pitchtomid_c_offset (notename, sups);
	;}
    break;

  case 165:
#line 1672 "./lyparser.y"
    {
		char notename = 'a' + (yyvsp[(1) - (2)].t).t.a;
		int enshift = (yyvsp[(1) - (2)].t).t.b;
		int subs = -(yyvsp[(2) - (2)].i).i;
		(yyval.n).user_string = g_strconcat((yyvsp[(1) - (2)].t).user_string, (yyvsp[(2) - (2)].i).user_string, NULL);
		(yyval.n).n.enshift = enshift;
		(yyval.n).n.mid_c_offset = pitchtomid_c_offset (notename, subs);
	;}
    break;

  case 166:
#line 1687 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.n) = (yyvsp[(1) - (1)].t);
#endif
	;}
    break;

  case 167:
#line 1693 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		Pitch p = *unsmob_pitch ((yyvsp[(1) - (2)].t));
		p.octave_ +=  (yyvsp[(2) - (2)].i);
		(yyval.n) = p.smobbed_copy ();
#endif
	;}
    break;

  case 168:
#line 1701 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		Pitch p =* unsmob_pitch ((yyvsp[(1) - (2)].t));

		p.octave_ +=  -(yyvsp[(2) - (2)].i);
		(yyval.n) = p.smobbed_copy ();

#endif
	;}
    break;

  case 169:
#line 1714 "./lyparser.y"
    {
		(yyval.n) = (yyvsp[(1) - (1)].n);
	;}
    break;

  case 170:
#line 1717 "./lyparser.y"
    {
		(yyval.n) = (yyvsp[(1) - (1)].n);
	;}
    break;

  case 171:
#line 1723 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.n) = (yyvsp[(2) - (2)].scm);
		if (!unsmob_pitch ((yyvsp[(2) - (2)].scm))) {
			THIS->parser_error (_f ("Expecting musical-pitch value", 3));
			 (yyval.n) = Pitch ().smobbed_copy ();
		}
#endif
	;}
    break;

  case 172:
#line 1736 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.f) = (yyvsp[(2) - (2)].scm);
		if (!unsmob_duration ((yyvsp[(2) - (2)].scm)))
		{
			THIS->parser_error (_ ("Must have duration object"));
			(yyval.f) = Duration ().smobbed_copy ();
		}
#endif
	;}
    break;

  case 173:
#line 1750 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		if (!THIS->lexer_->lyric_state_b ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));
		(yyval.scm) = new Extender_req;
#endif
	;}
    break;

  case 174:
#line 1761 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		if (!THIS->lexer_->lyric_state_b ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));
		(yyval.scm) = new Hyphen_req;
#endif
	;}
    break;

  case 175:
#line 1772 "./lyparser.y"
    {
              (yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 176:
#line 1775 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].gstr));
	    ((nodegstr*)n)->gstr = (yyvsp[(1) - (1)].gstr).gstr;
	    (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 177:
#line 1780 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].gstr));
	    ((nodegstr*)n)->gstr = (yyvsp[(1) - (1)].gstr).gstr;
	    (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 178:
#line 1785 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
	;}
    break;

  case 179:
#line 1791 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 180:
#line 1795 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 181:
#line 1799 "./lyparser.y"
    {	/* tie */
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 182:
#line 1803 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 183:
#line 1807 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 184:
#line 1811 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 185:
#line 1815 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		Span_req* s= new Span_req;
		(yyval.scm) = s;
		s->set_mus_property ("span-type", scm_makfrom0str ( "phrasing-slur"));
		s->set_spot (THIS->here_input());
#endif
	;}
    break;

  case 186:
#line 1827 "./lyparser.y"
    {
	(yyval.generic) = (yyvsp[(1) - (1)].generic);
	;}
    break;

  case 187:
#line 1832 "./lyparser.y"
    {

	DenemoObject *mud = lily_directive_new ((yyvsp[(1) - (1)].generic).user_string);	
		(yyval.scm) = g_list_append(NULL,mud);
        ;}
    break;

  case 188:
#line 1844 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].gstr));
	    ((nodegstr*)n)->gstr = (yyvsp[(1) - (1)].gstr).gstr;
	    (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 189:
#line 1849 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		String ds = to_string ((yyvsp[(1) - (1)].i));
		Text_script_req* t = new Text_script_req;
		SCM finger = ly_symbol2scm ("finger");
		t->set_mus_property ("text",  scm_makfrom0str (ds.to_str0 ()));
		t->set_mus_property ("text-type" , finger);
		t->set_spot (THIS->here_input ());
		(yyval.scm) = t;
#endif
	;}
    break;

  case 190:
#line 1864 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Hat");
#endif
	;}
    break;

  case 191:
#line 1870 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Plus");
#endif
	;}
    break;

  case 192:
#line 1876 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Dash");
#endif
	;}
    break;

  case 193:
#line 1882 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Bar");
#endif
	;}
    break;

  case 194:
#line 1888 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Larger");
#endif
	;}
    break;

  case 195:
#line 1894 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Dot");
#endif
	;}
    break;

  case 196:
#line 1900 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Underscore");
#endif
	;}
    break;

  case 197:
#line 1909 "./lyparser.y"
    { /* $$ = DOWN; */ ;}
    break;

  case 198:
#line 1910 "./lyparser.y"
    {  /* $$ = UP; */ ;}
    break;

  case 199:
#line 1911 "./lyparser.y"
    {  /* $$ = CENTER; */ ;}
    break;

  case 200:
#line 1916 "./lyparser.y"
    {
		(yyval.n) = (yyvsp[(1) - (1)].n);
	;}
    break;

  case 201:
#line 1922 "./lyparser.y"
    {
	(yyval.f) = (yyvsp[(1) - (1)].f);
	;}
    break;

  case 202:
#line 1925 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.f) = (yyvsp[(1) - (1)].f);
#endif
	;}
    break;

  case 203:
#line 1934 "./lyparser.y"
    {
	(yyval.f) = default_duration_;
	;}
    break;

  case 204:
#line 1937 "./lyparser.y"
    {
		(yyval.f) = (yyvsp[(1) - (1)].f);
		 default_duration_.t1.a = (yyvsp[(1) - (1)].f).t1.a;
		 default_duration_.t1.b = (yyvsp[(1) - (1)].f).t1.b;
	;}
    break;

  case 205:
#line 1942 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.f) = (yyvsp[(1) - (1)].f);
		THIS->default_duration_ = *unsmob_duration ((yyval.f));
#endif
	;}
    break;

  case 206:
#line 1952 "./lyparser.y"
    {
		int l = 0;
		if (!is_duration_b ((yyvsp[(1) - (2)].i).i))
			lyerror ("value not a duration");/*  $1.i */
		else
			l =  intlog2 ((yyvsp[(1) - (2)].i).i);

		if((yyvsp[(2) - (2)].i).i) (yyval.f).user_string = g_strconcat((yyvsp[(1) - (2)].i).user_string, (yyvsp[(2) - (2)].i).user_string, NULL);
		(yyval.f).t1.a = l;
		(yyval.f).t1.b =(yyvsp[(2) - (2)].i).i;
	;}
    break;

  case 207:
#line 1963 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		Duration *d =unsmob_duration ((yyvsp[(1) - (2)].id));
		Duration k (d->duration_log (),d->dot_count () + (yyvsp[(2) - (2)].i));
		(yyval.f) = k.smobbed_copy ();
#endif
	;}
    break;

  case 208:
#line 1977 "./lyparser.y"
    { /* note 4 integers are used for these */
		(yyvsp[(1) - (1)].f).t2.a = 1;
		(yyvsp[(1) - (1)].f).t2.b = 1;
		(yyval.f) = (yyvsp[(1) - (1)].f);
	;}
    break;

  case 209:
#line 1982 "./lyparser.y"
    {/* note 4 integers are used for these */
	stradd((yyvsp[(1) - (3)].f),(yyvsp[(2) - (3)].generic));	
	stradd((yyvsp[(1) - (3)].f),(yyvsp[(3) - (3)].i));
	(yyvsp[(1) - (3)].f).t2.a *= (yyvsp[(3) - (3)].i).i; 
	(yyval.f) = (yyvsp[(1) - (3)].f);

#ifdef LATER
		(yyval.f) = unsmob_duration ((yyval.f))->compressed ( (yyvsp[(3) - (3)].i)) .smobbed_copy ();
#endif
	;}
    break;

  case 210:
#line 1992 "./lyparser.y"
    {/* note 4 integers are used for these */
	stradd((yyvsp[(1) - (3)].f),(yyvsp[(2) - (3)].generic));	
	stradd((yyvsp[(1) - (3)].f),(yyvsp[(3) - (3)].t));
	(yyvsp[(1) - (3)].f).t2.a *= (yyvsp[(3) - (3)].t).t.a; /* numerator of fraction */
	(yyvsp[(1) - (3)].f).t2.b  = (yyvsp[(3) - (3)].t).t.b; /* denominator of fraction */
	(yyval.f) = (yyvsp[(1) - (3)].f);

#ifdef LATER
		Rational  m (gh_scm2int (ly_car ((yyvsp[(3) - (3)].t))), gh_scm2int (ly_cdr ((yyvsp[(3) - (3)].t))));

		(yyval.f) = unsmob_duration ((yyval.f))->compressed (m).smobbed_copy ();
#endif
	;}
    break;

  case 211:
#line 2008 "./lyparser.y"
    { (yyval.t) = (yyvsp[(1) - (1)].t); ;}
    break;

  case 212:
#line 2009 "./lyparser.y"
    {

		(yyval.t).user_string =  g_strconcat((yyvsp[(1) - (3)].i).user_string, (yyvsp[(2) - (3)].generic).user_string, (yyvsp[(3) - (3)].i).user_string, NULL);
		(yyval.t).t.a = (yyvsp[(1) - (3)].i).i;
		(yyval.t).t.b = (yyvsp[(3) - (3)].i).i;
	;}
    break;

  case 213:
#line 2018 "./lyparser.y"
    { (yyval.i).i = 0; ;}
    break;

  case 214:
#line 2019 "./lyparser.y"
    { 
		if((yyvsp[(1) - (2)].i).i == 0){
			(yyval.i).i = 1;
			(yyval.i).user_string = (yyvsp[(2) - (2)].i).user_string;
		} else {
			(yyval.i).user_string = g_strconcat((yyvsp[(1) - (2)].i).user_string, (yyvsp[(2) - (2)].i).user_string, NULL);/* FIXME memory leak $2*/
				
			(yyval.i).i = (yyvsp[(1) - (2)].i).i+1;
		 }
	;}
    break;

  case 215:
#line 2032 "./lyparser.y"
    {
		(yyval.i).i = 0;
	;}
    break;

  case 216:
#line 2035 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		if (!is_duration_b ((yyvsp[(2) - (2)].i)))
			THIS->parser_error (_f ("not a duration: %d", (yyvsp[(2) - (2)].i)));
		(yyval.i) = (yyvsp[(2) - (2)].i);
#endif
	;}
    break;

  case 217:
#line 2102 "./lyparser.y"
    { (yyval.i).i = 0; ;}
    break;

  case 218:
#line 2103 "./lyparser.y"
    { (yyval.i).i = 1; ;}
    break;

  case 219:
#line 2109 "./lyparser.y"
    {
		/* pitch is a node* with union a denemo note, 
		   exclamations and questions are nodei,
		   optional_notemode_duration is now a node4i, 
			but I don't know how to calculate with 
			the multiplier fraction in the top two ints, 
		  optional rest is boolean for the \rest keyword 
			appearing after a note - it turns it into a rest
		  the duration is on the chord structure, 
		  the pitch on the note structure.
		print *(note*)(((DenemoObject*)(((GList*)(((staff*)si->thescore->data)->measures->data))->data))->u.chordval.notes.data)
		 */ 
		DenemoObject *mud = newchord( (yyvsp[(5) - (6)].f).t1.a, (yyvsp[(5) - (6)].f).t1.b, 0);

		if (!note_state_b ())
			lyerror (_ ("Have to be in Note mode for notes"));
		if ((yyvsp[(6) - (6)].i).i)
		     	/* this is a rest vertically placed at the note
			   no special representation in denemo yet */;
		else {
			addtone( mud, (yyvsp[(1) - (6)].n).n.mid_c_offset, (yyvsp[(1) - (6)].n).n.enshift, 0);/*FIXME should be
							using $1.n directly */

#define no ((note*)((((chord *)mud->object)->notes)->data))		
			if ((yyvsp[(3) - (6)].i).i % 2) {
				no->showaccidental = TRUE;
				((chord *)mud->object)->hasanacc = TRUE;
			}
			if ((yyvsp[(2) - (6)].i).i % 2 ) {
				no->showaccidental = TRUE;
				((chord *)mud->object)->hasanacc = TRUE;
			}
#undef no
		}
		mud->user_string = (yyvsp[(1) - (6)].n).user_string;
		if ((yyvsp[(2) - (6)].i).i) stradd ( (*mud),(yyvsp[(2) - (6)].i));
		if ((yyvsp[(3) - (6)].i).i) stradd ( (*mud),(yyvsp[(3) - (6)].i));
		stradd ( (*mud),(yyvsp[(5) - (6)].f));
		if ((yyvsp[(6) - (6)].i).i) stradd ( (*mud),(yyvsp[(6) - (6)].i)); /* FIXME memory leaks on strings concatenated */

		(yyval.scm) = g_list_append (NULL,mud);
	;}
    break;

  case 220:
#line 2151 "./lyparser.y"
    {
	/* denemo wants a chord with no notes */
	DenemoObject *mud = newchord( (yyvsp[(2) - (2)].f).t1.a, (yyvsp[(2) - (2)].f).t1.b,0);

	mud->user_string = g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].f).user_string, NULL);
	(yyval.scm) = g_list_append(NULL,mud); 
	;}
    break;

  case 221:
#line 2158 "./lyparser.y"
    {
	/* denemo wants a chord with no notes */
	DenemoObject *mud = newchord( (yyvsp[(2) - (2)].f).t1.a, (yyvsp[(2) - (2)].f).t1.b,0);
	mud->type = SKIPNAME;
	mud->user_string = g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].f).user_string, NULL);
	(yyval.scm) = g_list_append(NULL,mud); 
	;}
    break;

  case 222:
#line 2165 "./lyparser.y"
    {
	/* treat as skip for the moment */
	DenemoObject *mud = newchord( (yyvsp[(2) - (2)].f).t1.a, (yyvsp[(2) - (2)].f).t1.b,0);
	mud->type = SKIPNAME;
	mud->user_string = g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].f).user_string, NULL);
	(yyval.scm) = g_list_append(NULL,mud); 

	;}
    break;

  case 223:
#line 2173 "./lyparser.y"
    {
		DenemoObject *mud = newlyric((yyvsp[(2) - (2)].f).t1.a, (yyvsp[(2) - (2)].f).t1.b, (yyvsp[(1) - (2)].gstr).gstr->str);
		mud->user_string = (yyvsp[(1) - (2)].gstr).user_string;
	        mud->type = LYRICS;
		stradd((*mud),(yyvsp[(2) - (2)].f));
		(yyval.scm) = g_list_append(NULL,mud);
	;}
    break;

  case 224:
#line 2180 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		Input i = THIS->pop_spot ();

		if (!THIS->lexer_->chord_state_b ())
			THIS->parser_error (_ ("Have to be in Chord mode for chords"));
		(yyval.scm) = (yyvsp[(1) - (1)].music);
#endif
	;}
    break;

  case 227:
#line 2200 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
                (yyval.music) = Chord::get_chord ((yyvsp[(1) - (6)].n), (yyvsp[(3) - (6)].scm), (yyvsp[(4) - (6)].scm), (yyvsp[(5) - (6)].scm), (yyvsp[(6) - (6)].scm), (yyvsp[(2) - (6)].f));
		(yyval.music)->set_spot (THIS->here_input ());
#endif
        ;}
    break;

  case 228:
#line 2209 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = SCM_EOL;
#endif
	;}
    break;

  case 229:
#line 2215 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(2) - (2)].scm);
#endif
	;}
    break;

  case 230:
#line 2224 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 231:
#line 2227 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_append2 ((yyval.scm), (yyvsp[(3) - (3)].scm));
#endif
	;}
    break;

  case 232:
#line 2236 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = SCM_EOL;
#endif
	;}
    break;

  case 233:
#line 2242 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(2) - (2)].scm);
	;}
    break;

  case 234:
#line 2249 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = SCM_EOL;
#endif
	;}
    break;

  case 235:
#line 2255 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(2) - (2)].n);
#endif
	;}
    break;

  case 236:
#line 2264 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = SCM_EOL;
#endif
	;}
    break;

  case 237:
#line 2270 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(2) - (2)].n);
#endif
	;}
    break;

  case 238:
#line 2279 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_cons ((yyvsp[(1) - (1)].scm), SCM_EOL);
#endif
	;}
    break;

  case 239:
#line 2285 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_cons (unsmob_pitch ((yyvsp[(1) - (1)].t))->smobbed_copy (), SCM_EOL);
#endif
	;}
    break;

  case 240:
#line 2291 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
	 /* Ugh. */
		(yyval.scm) = scm_list_n (unsmob_pitch ((yyvsp[(1) - (2)].t))->smobbed_copy (),
			(yyvsp[(2) - (2)].scm), SCM_UNDEFINED);
#endif
	;}
    break;

  case 241:
#line 2302 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		 Pitch m;
		m.notename_ = ((yyvsp[(1) - (1)].i) - 1) % 7;
		m.octave_ = (yyvsp[(1) - (1)].i) > 7 ? 1 : 0;
		m.alteration_ = 0;

		(yyval.scm) = m.smobbed_copy ();
#endif
        ;}
    break;

  case 242:
#line 2313 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		Pitch m;
		m.notename_ = ((yyvsp[(1) - (2)].i) - 1) % 7;
		m.octave_ = (yyvsp[(1) - (2)].i) > 7 ? 1 : 0;
		m.alteration_ = 1;


		(yyval.scm) = m.smobbed_copy ();
#endif
	;}
    break;

  case 243:
#line 2325 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		Pitch m;
		m.notename_ = ((yyvsp[(1) - (2)].i) - 1) % 7;
		m.octave_ = (yyvsp[(1) - (2)].i) > 7 ? 1 : 0;
		m.alteration_ = -1;

		(yyval.scm) = m.smobbed_copy ();
#endif
	;}
    break;

  case 244:
#line 2342 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_sum ((yyvsp[(1) - (3)].scm), (yyvsp[(3) - (3)].scm));
#endif
	;}
    break;

  case 245:
#line 2348 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_difference ((yyvsp[(1) - (3)].scm), (yyvsp[(3) - (3)].scm));
#endif
	;}
    break;

  case 247:
#line 2358 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
#endif
	;}
    break;

  case 248:
#line 2364 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_product ((yyvsp[(1) - (3)].scm), (yyvsp[(3) - (3)].scm));
#endif
	;}
    break;

  case 249:
#line 2370 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_divide ((yyvsp[(1) - (3)].scm), (yyvsp[(3) - (3)].scm));
#endif
	;}
    break;

  case 250:
#line 2379 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(2) - (3)].scm);
	;}
    break;

  case 251:
#line 2382 "./lyparser.y"
    { /* %prec UNARY_MINUS */
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_difference ((yyvsp[(2) - (2)].scm), SCM_UNDEFINED);
#endif
	;}
    break;

  case 253:
#line 2393 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_int2scm ((yyvsp[(1) - (1)].i));
#endif
	;}
    break;

  case 254:
#line 2399 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(1) - (1)].r);
#endif
	;}
    break;

  case 255:
#line 2405 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(1) - (1)].id);
#endif
	;}
    break;

  case 256:
#line 2411 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_double2scm (gh_scm2double ((yyvsp[(1) - (2)].r)) CM );
#endif
	;}
    break;

  case 257:
#line 2417 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_double2scm (gh_scm2double ((yyvsp[(1) - (2)].r)) PT);
#endif
	;}
    break;

  case 258:
#line 2423 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_double2scm (gh_scm2double ((yyvsp[(1) - (2)].r)) INCH);
#endif
	;}
    break;

  case 259:
#line 2429 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_double2scm (gh_scm2double ((yyvsp[(1) - (2)].r)) MM);
#endif
	;}
    break;

  case 260:
#line 2435 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_double2scm (gh_scm2double ((yyvsp[(1) - (2)].r)) CHAR);
#endif
	;}
    break;

  case 261:
#line 2445 "./lyparser.y"
    {
			(yyval.i) = (yyvsp[(1) - (1)].i);
	;}
    break;

  case 262:
#line 2448 "./lyparser.y"
    {
		(yyval.i) = (yyvsp[(1) - (1)].i);
	;}
    break;

  case 263:
#line 2454 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		if (scm_integer_p ((yyvsp[(1) - (1)].scm)) == SCM_BOOL_T)
		{
			int k = gh_scm2int ((yyvsp[(1) - (1)].scm));
			(yyval.i) = k;
		} else
		{
			THIS->parser_error (_ ("need integer number arg"));
			(yyval.i) = 0;
		}
#endif
	;}
    break;

  case 264:
#line 2468 "./lyparser.y"
    {
		(yyval.i).i = -(yyvsp[(2) - (2)].i).i;
	;}
    break;

  case 265:
#line 2475 "./lyparser.y"
    {
		(yyval.gstr) = (yyvsp[(1) - (1)].gstr);
	;}
    break;

  case 266:
#line 2478 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.gstr) = (yyvsp[(1) - (1)].id);
#endif
	;}
    break;

  case 267:
#line 2484 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.gstr) = scm_string_append (scm_list_n ((yyvsp[(1) - (3)].gstr), (yyvsp[(3) - (3)].gstr), SCM_UNDEFINED));
#endif
	;}
    break;

  case 268:
#line 2494 "./lyparser.y"
    { (yyval.i).i = 0; ;}
    break;

  case 269:
#line 2495 "./lyparser.y"
    { 
	if((yyvsp[(1) - (2)].i).i == 0){
			(yyval.i).i = 1;
			(yyval.i).user_string = (yyvsp[(2) - (2)].i).user_string;
		} else {
			(yyval.i).user_string = g_strconcat((yyvsp[(1) - (2)].i).user_string, (yyvsp[(2) - (2)].i).user_string, NULL);/* FIXME memory leak $2*/
				
			(yyval.i).i = (yyvsp[(1) - (2)].i).i+1;
		 }
	;}
    break;

  case 270:
#line 2508 "./lyparser.y"
    { (yyval.i).i = 0; ;}
    break;

  case 271:
#line 2509 "./lyparser.y"
    { 
		if((yyvsp[(1) - (2)].i).i == 0){
			(yyval.i).i = 1;
			(yyval.i).user_string = (yyvsp[(2) - (2)].i).user_string;
		} else {
			(yyval.i).user_string = g_strconcat((yyvsp[(1) - (2)].i).user_string, (yyvsp[(2) - (2)].i).user_string, NULL);/* FIXME memory leak $2*/
				
			(yyval.i).i = (yyvsp[(1) - (2)].i).i+1;
		 }
	;}
    break;


/* Line 1267 of yacc.c.  */
#line 5227 "lyparser.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }

  yyerror_range[0] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval, &yylloc);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  yyerror_range[0] = yylsp[1-yylen];
  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;

      yyerror_range[0] = *yylsp;
      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;

  yyerror_range[1] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the look-ahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, (yyerror_range - 1), 2);
  *++yylsp = yyloc;

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval, &yylloc);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 2522 "./lyparser.y"


#if GTK_MAJOR_VERSION <= 1
#define gtk_window_present(a)
#endif


static objnode *
use_up_ticks (objnode * h, gint ticks)
{
  DenemoObject *figmud = (DenemoObject *) h->data;
  while (ticks > 0)
    {
      h = h->next;
      figmud = h ? (DenemoObject *) h->data : NULL;
      if (!figmud)
	{
	  g_warning ("Insufficient figures for bass part");
	  return NULL;
	}
      ticks -= figmud->basic_durinticks;
    }
  return h;
}

static gboolean
create_figures (GList * b, GList * f)
{
  objnode *h = (objnode *) f->data;
  DenemoObject *figmud;

  for (figmud = (DenemoObject *) h->data; figmud;
       h = h->next, figmud = h ? (DenemoObject *) h->data : NULL)
    {
      if (figmud->type == CHORD)
	return FALSE;		/* there are already figures present */
    }


  for (; b && b->data; b = b->next)
    {
      objnode *g = (objnode *) b->data;
      DenemoObject *mud;
      for (mud = (DenemoObject *) g->data; mud;
	   g = g->next, mud = (g ? (DenemoObject *) g->data : NULL))
	{
	  if (mud->type == CHORD)
	    {

	    }

	  if (mud->type == TUPOPEN || mud->type == TUPCLOSE
	      || mud->type == GRACE_START || mud->type == GRACE_END)
	    {
	      DenemoObject *figmud =
		(DenemoObject *) g_malloc0 (sizeof (DenemoObject));
	      memcpy (figmud, mud, sizeof (DenemoObject));
	      f->data = g_list_append ((GList *) f->data, figmud);
	    }
	}
      f = (f->next ? f->next : g_list_append (f, NULL));
    }

  return TRUE;
}

/* return next objnode of CHORD (is_figure will be true for h) type */
static objnode *
next_figure (objnode * h)
{
  DenemoObject *figmud;
  for (figmud = (DenemoObject *) h->data; figmud;
       h = h->next, figmud = h ? (DenemoObject *) h->data : NULL)
    {
      if (figmud->type == CHORD)
	return h;
    }
  //g_assert (h == NULL);
  return NULL;
}

/* Link each chord in BASS to a figure (CHORD) or sequence of them in
FIGURES; create blank figures for each note present in BASS if none is present in
FIGURES, abort if incomplete figures present */
static void
fill_in_figures (DenemoStaff * bass, DenemoStaff * figures)
{
  GList *b = bass->measures, *f = figures->measures;
  /* if there are no figures create a set cloning the durations from bass */
  if (create_figures (b, f))
    return;

  for (b = bass->measures, f = figures->measures;
       b && b->data; b = b->next, f = f->next)
    {
      objnode *g = (objnode *) b->data;
      objnode *h = (objnode *) f->data;
      DenemoObject *mud;
      for (mud = (DenemoObject *) g->data; mud;
	   g = g->next, mud = (g ? (DenemoObject *) g->data : NULL))
	{

	  if (mud->type == CHORD)
	    {
	      if (h)
		{
		  /* set figure of bass chord to the first CHORD (ie figure) 
		   * object in the FIGURES list starting at h.
		   * Then move h on over other CHORDs (ie figures) if needed to
		   * use up the duration of the mud->object.
		   * If the list h has run out, abort since we don't know how 
		   * much is missing or why 
		   */
		  h =  (objnode *) (((chord *) mud->object)->figure); 
		  (((chord *) mud->object)->figure) = next_figure (h);
		  if (h)
		    h =
		      use_up_ticks (h,
				    mud->basic_durinticks -
				    ((DenemoObject *) h->data)->
				    basic_durinticks);
		}		/* if h */

	      if (((chord *) mud->object)->figure == NULL)	/* not yet fixed up this CHORD in BASS */
		{
		  lyerror
		    ("Figures are incomplete for Bass - delete the figures or complete them!");
		}
	      h = h ? h->next : NULL;
	    }			/* if mud is CHORD */
	}			/* end of for each DenemoObject in the measure */
    }				/* for each measure */
}

/* create the list of measurewidths for si, add measures to short staffs and
   run set_initial_staffcontexts() to setup the clef etc in the staff structures
   Fill in any figured bass staff that is short of figures with blank figures.
 */
static gint
fixup_measure_widths_and_contexts (DenemoScore * si)
{
  GList *g = si->thescore;
  DenemoStaff *curstaffstruct = staffstruct (g);
  int i, num_measures = 0;
  /* find num_measures of longest staff */
  for (g = si->thescore; g; g = g->next)
    {
      curstaffstruct = staffstruct (g);
      i = g_list_length (curstaffstruct->measures);
      if (num_measures < i)
	num_measures = i;
    }
  /* add measures to short staffs */
  for (g = si->thescore; g; g = g->next)
    {
      curstaffstruct = staffstruct (g);
      i = g_list_length (curstaffstruct->measures);
      while ((num_measures - i++) > 0)
	curstaffstruct->measures =
	  g_list_append (curstaffstruct->measures, NULL);
    }
/*	if(g_list_length(si->measurewidths)) lyerror("si->measurewidths should be zero at this point");*/
  g = si->thescore;
  curstaffstruct = staffstruct (g);
  i = g_list_length (curstaffstruct->measures);
  while (i--)
    si->measurewidths =
      g_list_append (si->measurewidths, GINT_TO_POINTER (si->measurewidth));
  if (si->has_figures)
    fill_in_figures (si->has_figures->main_staff,
		     si->has_figures->related_staff);
  if(set_initial_staffcontexts (si))
	return -1;
  find_leftmost_allcontexts (si);
  return 0;
}


/* these scheme identifiers are difficult to track down in lilypond's source
tree: \major and \minor in particular I haven't found. There are a group in
lilypond/ly/script-init.ly, and clearly the file being parsed could define
additional ones, in general we just need to recognize them and pass on the
user_string, except where denemo has been equipped to represent them graphically.
In this last case we store a token to be returned as the lyval->type, the only
case where this is not the same as the token the lexer returns.
*/

static void
insert_scm (int type, gchar * str)
{
  nodegstr *n = (nodegstr *) g_malloc0 (sizeof (nodegstr));
  n->type = type;
  n->gstr = g_string_new (str);
  g_hash_table_insert (scm_identifiers, (gpointer) str, (gpointer) n);
}


static void
initialize_scm_identifiers (void)
{
  if (scm_identifiers)
    return;
  scm_identifiers = g_hash_table_new (g_str_hash, g_str_equal);
  insert_scm (MUSICMODE, "major");
  insert_scm (MUSICMODE, "minor");

  insert_scm (TONEOPTION, "thumb");
  insert_scm (TONEOPTION, "accent");
  insert_scm (TONEOPTION, "marcato");
  insert_scm (TONEOPTION, "staccatissimo");
  insert_scm (TONEOPTION, "portato");
  insert_scm (TONEOPTION, "fermata");
  insert_scm (TONEOPTION, "stopped");
  insert_scm (TONEOPTION, "staccato");
  insert_scm (TONEOPTION, "tenuto");
  insert_scm (TONEOPTION, "upbow");
  insert_scm (TONEOPTION, "downbow");
  insert_scm (TONEOPTION, "lheel");
  insert_scm (TONEOPTION, "rheel");
  insert_scm (TONEOPTION, "ltoe");
  insert_scm (TONEOPTION, "rtoe");
  insert_scm (TONEOPTION, "turn");
  insert_scm (TONEOPTION, "open");
  insert_scm (TONEOPTION, "flageolet");
  insert_scm (TONEOPTION, "reverseturn");
  insert_scm (TONEOPTION, "trill");
  insert_scm (TONEOPTION, "prall");
  insert_scm (TONEOPTION, "mordent");
  insert_scm (TONEOPTION, "upmordent");
  insert_scm (TONEOPTION, "downmordent");
  insert_scm (TONEOPTION, "prallprall");
  insert_scm (TONEOPTION, "prallup");
  insert_scm (TONEOPTION, "pralldown");
  insert_scm (TONEOPTION, "lineprall");
  insert_scm (TONEOPTION, "prallmordent");
  insert_scm (TONEOPTION, "upprall");
  insert_scm (TONEOPTION, "downprall");
  insert_scm (TONEOPTION, "segno");
  insert_scm (TONEOPTION, "coda");



  insert_scm (DYNAMICMARK, "ppp");
  insert_scm (DYNAMICMARK, "pp");
  insert_scm (DYNAMICMARK, "p");
  insert_scm (DYNAMICMARK, "mp");
  insert_scm (DYNAMICMARK, "mf");
  insert_scm (DYNAMICMARK, "f");
  insert_scm (DYNAMICMARK, "ff");
  insert_scm (DYNAMICMARK, "fff");
  insert_scm (DYNAMICMARK, "fff");
  insert_scm (DYNAMICMARK, "fp");
  insert_scm (DYNAMICMARK, "sf");
  insert_scm (DYNAMICMARK, "sff");
  insert_scm (DYNAMICMARK, "sp");
  insert_scm (DYNAMICMARK, "spp");
  insert_scm (DYNAMICMARK, "sfz");
  insert_scm (DYNAMICMARK, "rfz");

  insert_scm (DYNAMICMARK, "cr");
  insert_scm (DYNAMICMARK, "rc");

  insert_scm (DYNAMICMARK, "decr");
  insert_scm (DYNAMICMARK, "rced");



}


/* set parser_error_linenum
   set parser_error_message, linenum .
   if EDITOR environment variable not set
   show a dialog giving TEXT and and offering to exit application
   or return (for editing the whole file in gui).  */

void
parser_error (gchar * text, int line_number)
{

  GtkWidget *label;
  GtkWidget *editbutton;
  GtkWidget *exitbutton;
  if( (parser_error_message == NULL) ) {
        parser_error_message = strdup(text);
  	parser_error_linenum = line_number;
  }

  if (!getenv ("EDITOR"))
    {
      if (!parser_error_dialog)
	{
	/*warningdialog("Unable to load this file - quitting");*/
	return(-1);
	}
    }
  else
    {
      g_print ("\nAssociated message: %s at line %d\n", text, line_number);

    }

}



/* called by lexer at EOF */
void set_trailing_white_space (gchar *trailing) {
	trailing_white_space = g_strdup (trailing);
}

static void attach_trailing_white_space (GList *top) {
	GList *g = g_list_last (top);
	u_str(g) = g_strconcat(u_str(g), trailing_white_space,NULL);
	g_free(trailing_white_space);
	trailing_white_space = NULL;
}

char *
header_str(char *key)
{
	nodeglist *x;
	char *pt = NULL;
	int n;

         x = (nodeglist *) g_hash_table_lookup (name_value_pairs, key);
	if (x)
	{
	    pt = u_str(x->branch);
	    if (*pt == '"') pt++;
	    n = strlen(pt) - 1;
	    if (*(pt+n) == '"') *(pt+n) = 0;
	}
	return(pt);
}
static void
score_prop_from_lily (DenemoGUI *gui)
{
DenemoScore *si	= gui->si;	
    char *pt;
    GList *scm;

    if ((pt = header_str("midi_tempo")))
    {
	si->tempo = atoi(pt);
    }
    if ((pt = header_str("lilyversion")))
    {
	gui->lilycontrol.lilyversion = g_string_new(pt);
    }
    if (findtok (lily_file, HEADER))
    {
#if 0
	if ((pt = header_str ("title")))
	    g_string_assign (si->headerinfo.title, pt);
	if ((pt = header_str ("subtitle")))
	    g_string_assign (si->headerinfo.subtitle, pt);
	if ((pt = header_str ("poet")))
	    g_string_assign (si->headerinfo.poet, pt);
	if ((pt = header_str ("composer")))
	    g_string_assign (si->headerinfo.composer, pt);
	if ((pt = header_str ("meter")))
	    g_string_assign (si->headerinfo.meter, pt);
	if ((pt = header_str ("opus")))
	    g_string_assign (si->headerinfo.opus, pt);
	if ((pt = header_str ("arranger")))
	    g_string_assign (si->headerinfo.arranger, pt);
	if ((pt = header_str ("instrument")))
	    g_string_assign (si->headerinfo.instrument, pt);
	if ((pt = header_str ("dedication")))
	    g_string_assign (si->headerinfo.dedication, pt);
	if ((pt = header_str ("piece")))
	    g_string_assign (si->headerinfo.piece, pt);
	if ((pt = header_str ("head")))
	    g_string_assign (si->headerinfo.head, pt);
	if ((pt = header_str ("copyright")))
	    g_string_assign (si->headerinfo.copyright, pt);
	if ((pt = header_str ("footer")))
	    g_string_assign (si->headerinfo.footer, pt);
	if ((pt = header_str ("tagline")))
	    g_string_assign (si->headerinfo.tagline, pt);
#else
//FIXME create tagged directives for this information
#endif


    }
    for(scm = findtok (lily_file, SCM_T); scm ; scm = scm->next)
    {
	if ((pt = strstr (u_str (scm), "set-global-staff-size")))
	{
		int font;
		if (sscanf (pt+21, " %d", &font) == 1)
		{
			gui->lilycontrol.staffsize = g_string_new(g_strdup_printf("%d", font));
		}
		else
			g_warning("%s no font", pt);
	}
	if ((pt = strstr (u_str (scm), "set-default-paper-size")))
	{
	    char *tmp;
	    char *pt2 = strchr(pt, '\"');
	    if (pt2)
	    {
		tmp = g_strdup(pt2+1);
		pt2 = strchr(tmp, '\"');
		if (pt2)
		{
		    *pt2 = 0;
 	//	    g_print ("Paper size %s\n", tmp);
                    g_string_assign (gui->lilycontrol.papersize, tmp);
		    *tmp = 0;
                    g_free (tmp);
		}
	    }
	    if (!pt2)
		g_warning("%s paper size error", pt);
	}
    }
}


/* from denemo's easylyparser.y
   note that this function generates a list
   of DenemoScore structures (one for each \score{} block
   in the lilypond file, returning the current one (as set in
   si->theFile->current_scoreblock). This rather clumsy
   arrangement is historical from when si was the root data
   structure: see denemo.h	
 */
int
lyinput (gchar * filename, DenemoGUI *gui)
{
  FILE *lyin;
  GList *score_block_list = NULL;
  initialize_scm_identifiers ();
  name_value_pairs = g_hash_table_new (g_str_hash, g_str_equal);/* FIXME memory leak */
  default_duration_.t1.a = 2;
  default_duration_.t1.b = 0;
  
  init_crescendo_state();
  DenemoScore *si = gui->si;

 //init_score (si, gui);
//	si->thescore = NULL;
  while (1)
    {				/* keep trying to open the file */
      if ((lyin = fopen (filename, "r")) == NULL)
	{
	  fprintf (stderr, "Cannot open the file: %s\n", filename);
	  return -1;
	}
      else
	{			/* file is opened */
	/* any old lily parse tree (ie si->lily_file) is already demolished */
	  lily_file = NULL;	
	  parser_error_dialog = NULL;
	  parser_error_message = NULL;
	  // Calling init_score here would cause piece in header to set to "Movement 2"
  	  si->measurewidths = NULL;
	  si->thescore = NULL;
	  

	  /* in case we are re-entering via reload after error */
	  lyrestart (lyin);	

	  lylineno = 1;		/* not done by lexer for some reason */
	  push_note_state ();

	  while (!feof (lyin))
	    {
	      lyparse ();
	    }
	 
	    {
	      GList *score = findtok (lily_file, SCORE);
	      if (score)
		if (create_score_from_lily (si, br (score)) == 0)
		  {
		    GList *top;
		    score_prop_from_lily(gui);
		    if(fixup_measure_widths_and_contexts (si))
			goto error;
		    while (score && score->next)
		      {
			score = findtok (score->next, SCORE);
			if (score)
			  {
	                    insert_movement_after(NULL, NULL);
			    DenemoScore *nextsi =
			      Denemo.gui->si;
			    init_score (nextsi, gui);
		 	    create_score_from_lily (nextsi, br (score));
		            if(fixup_measure_widths_and_contexts (nextsi))
				goto error;
			    
			  }
		      }

		    return 0;
		  }
	    }

	}
      reset_initial_lexer_state ();
error:
    //  if(si->thescore==NULL) {
	deletescore(NULL, Denemo.gui);
        open_user_default_template(REPLACE_SCORE);
    //  }	
      if (lily_file == NULL)
		return 1;

        lyerror ("File load failed\n");
        return -1;     
    }				/* forever */
  lyerror ("File load failed\n");
  return -1;			/* there is no handler for this yet - never has been! */
}
#ifdef YYPRINT

static gchar *
type_name(gint type)
{
	gint i;
	for(i=0; yytoknum[i] != type; i++)
	{
	    if (i > YYNTOKENS)
		return("");
	}
	return yytname[i];
}

#endif

