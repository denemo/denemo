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
     MUSICMODE = 265,
     TONEOPTION = 266,
     DYNAMICMARK = 267,
     AUTOCHANGE = 268,
     ALIAS = 269,
     APPLY = 270,
     ARPEGGIO = 271,
     DYNAMICSCRIPT = 272,
     ACCEPTS = 273,
     ALTERNATIVE = 274,
     BAR = 275,
     BREAK = 276,
     BREATHE = 277,
     CHORDMODIFIERS = 278,
     CHORDS = 279,
     CHAR_T = 280,
     CLEF_ = 281,
     CM_T = 282,
     CONSISTS = 283,
     SEQUENTIAL = 284,
     SIMULTANEOUS = 285,
     GROBDESCRIPTIONS = 286,
     CONSISTSEND = 287,
     DENIES = 288,
     DURATION = 289,
     EXTENDER = 290,
     FIGURES = 291,
     FIGURE_OPEN = 292,
     FIGURE_CLOSE = 293,
     FIGURE_BRACKET_CLOSE = 294,
     FIGURE_BRACKET_OPEN = 295,
     GLISSANDO = 296,
     GRACE = 297,
     HEADER = 298,
     HYPHEN = 299,
     IN_T = 300,
     INVALID = 301,
     KEY = 302,
     LYRICS = 303,
     MARK = 304,
     MARKUP = 305,
     MULTI_MEASURE_REST = 306,
     MIDI = 307,
     MM_T = 308,
     PITCH = 309,
     DEFAULT = 310,
     NAME = 311,
     PITCHNAMES = 312,
     NOTES = 313,
     PAPER = 314,
     PARTIAL_ = 315,
     PENALTY = 316,
     PROPERTY = 317,
     OVERRIDE = 318,
     SET = 319,
     REVERT = 320,
     PT_T = 321,
     RELATIVE = 322,
     REMOVE = 323,
     REPEAT = 324,
     ADDLYRICS = 325,
     PARTCOMBINE = 326,
     SCORE = 327,
     SCRIPT = 328,
     SKIP = 329,
     SPANREQUEST = 330,
     STYLESHEET = 331,
     COMMANDSPANREQUEST = 332,
     TEMPO = 333,
     OUTPUTPROPERTY = 334,
     TIME_T = 335,
     TIMES = 336,
     TRANSLATOR = 337,
     TRANSPOSE = 338,
     TYPE = 339,
     UNSET = 340,
     CONTEXT = 341,
     LAYOUT = 342,
     LYRICSTO = 343,
     LYRICMODE = 344,
     NEWCONTEXT = 345,
     LILYVERSION = 346,
     DRUM_PITCH = 347,
     MUSIC_FUNCTION = 348,
     REST = 349,
     DOUBLE_ANGLE_CLOSE = 350,
     DOUBLE_ANGLE_OPEN = 351,
     E_CHAR = 352,
     E_EXCLAMATION = 353,
     E_SMALLER = 354,
     E_BIGGER = 355,
     E_OPEN = 356,
     E_CLOSE = 357,
     E_LEFTSQUARE = 358,
     E_RIGHTSQUARE = 359,
     E_TILDE = 360,
     E_BACKSLASH = 361,
     CHORD_BASS = 362,
     CHORD_COLON = 363,
     CHORD_MINUS = 364,
     CHORD_CARET = 365,
     FIGURE_SPACE = 366,
     DIGIT = 367,
     NOTENAME_PITCH = 368,
     TONICNAME_PITCH = 369,
     CHORDMODIFIER_PITCH = 370,
     DURATION_IDENTIFIER = 371,
     FRACTION = 372,
     IDENTIFIER = 373,
     SCORE_IDENTIFIER = 374,
     MUSIC_OUTPUT_DEF_IDENTIFIER = 375,
     NUMBER_IDENTIFIER = 376,
     REQUEST_IDENTIFIER = 377,
     MUSIC_IDENTIFIER = 378,
     TRANSLATOR_IDENTIFIER = 379,
     STRING_IDENTIFIER = 380,
     SCM_IDENTIFIER = 381,
     RESTNAME = 382,
     SKIPNAME = 383,
     STRING_ = 384,
     SCM_T = 385,
     UNSIGNED = 386,
     REAL = 387,
     UNARY_MINUS = 388
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
#define MUSICMODE 265
#define TONEOPTION 266
#define DYNAMICMARK 267
#define AUTOCHANGE 268
#define ALIAS 269
#define APPLY 270
#define ARPEGGIO 271
#define DYNAMICSCRIPT 272
#define ACCEPTS 273
#define ALTERNATIVE 274
#define BAR 275
#define BREAK 276
#define BREATHE 277
#define CHORDMODIFIERS 278
#define CHORDS 279
#define CHAR_T 280
#define CLEF_ 281
#define CM_T 282
#define CONSISTS 283
#define SEQUENTIAL 284
#define SIMULTANEOUS 285
#define GROBDESCRIPTIONS 286
#define CONSISTSEND 287
#define DENIES 288
#define DURATION 289
#define EXTENDER 290
#define FIGURES 291
#define FIGURE_OPEN 292
#define FIGURE_CLOSE 293
#define FIGURE_BRACKET_CLOSE 294
#define FIGURE_BRACKET_OPEN 295
#define GLISSANDO 296
#define GRACE 297
#define HEADER 298
#define HYPHEN 299
#define IN_T 300
#define INVALID 301
#define KEY 302
#define LYRICS 303
#define MARK 304
#define MARKUP 305
#define MULTI_MEASURE_REST 306
#define MIDI 307
#define MM_T 308
#define PITCH 309
#define DEFAULT 310
#define NAME 311
#define PITCHNAMES 312
#define NOTES 313
#define PAPER 314
#define PARTIAL_ 315
#define PENALTY 316
#define PROPERTY 317
#define OVERRIDE 318
#define SET 319
#define REVERT 320
#define PT_T 321
#define RELATIVE 322
#define REMOVE 323
#define REPEAT 324
#define ADDLYRICS 325
#define PARTCOMBINE 326
#define SCORE 327
#define SCRIPT 328
#define SKIP 329
#define SPANREQUEST 330
#define STYLESHEET 331
#define COMMANDSPANREQUEST 332
#define TEMPO 333
#define OUTPUTPROPERTY 334
#define TIME_T 335
#define TIMES 336
#define TRANSLATOR 337
#define TRANSPOSE 338
#define TYPE 339
#define UNSET 340
#define CONTEXT 341
#define LAYOUT 342
#define LYRICSTO 343
#define LYRICMODE 344
#define NEWCONTEXT 345
#define LILYVERSION 346
#define DRUM_PITCH 347
#define MUSIC_FUNCTION 348
#define REST 349
#define DOUBLE_ANGLE_CLOSE 350
#define DOUBLE_ANGLE_OPEN 351
#define E_CHAR 352
#define E_EXCLAMATION 353
#define E_SMALLER 354
#define E_BIGGER 355
#define E_OPEN 356
#define E_CLOSE 357
#define E_LEFTSQUARE 358
#define E_RIGHTSQUARE 359
#define E_TILDE 360
#define E_BACKSLASH 361
#define CHORD_BASS 362
#define CHORD_COLON 363
#define CHORD_MINUS 364
#define CHORD_CARET 365
#define FIGURE_SPACE 366
#define DIGIT 367
#define NOTENAME_PITCH 368
#define TONICNAME_PITCH 369
#define CHORDMODIFIER_PITCH 370
#define DURATION_IDENTIFIER 371
#define FRACTION 372
#define IDENTIFIER 373
#define SCORE_IDENTIFIER 374
#define MUSIC_OUTPUT_DEF_IDENTIFIER 375
#define NUMBER_IDENTIFIER 376
#define REQUEST_IDENTIFIER 377
#define MUSIC_IDENTIFIER 378
#define TRANSLATOR_IDENTIFIER 379
#define STRING_IDENTIFIER 380
#define SCM_IDENTIFIER 381
#define RESTNAME 382
#define SKIPNAME 383
#define STRING_ 384
#define SCM_T 385
#define UNSIGNED 386
#define REAL 387
#define UNARY_MINUS 388




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
#line 585 "lyparser.tab.c"
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
#line 610 "lyparser.tab.c"

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
#define YYLAST   1574

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  157
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  99
/* YYNRULES -- Number of rules.  */
#define YYNRULES  268
/* YYNRULES -- Number of states.  */
#define YYNSTATES  409

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   388

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   127,     2,     2,     2,     2,     2,   128,
     120,   121,   119,   152,   129,   151,   125,   118,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   156,     2,
     114,   112,   116,   126,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   123,     2,   124,   154,   155,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   113,   117,   115,   122,     2,     2,     2,
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
     105,   106,   107,   108,   109,   110,   111,   130,   131,   132,
     133,   134,   135,   136,   137,   138,   139,   140,   141,   142,
     143,   144,   145,   146,   147,   148,   149,   150,   153
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
     164,   166,   172,   174,   176,   178,   182,   186,   189,   195,
     199,   203,   205,   207,   209,   214,   218,   222,   223,   227,
     228,   232,   233,   237,   238,   242,   243,   247,   249,   251,
     253,   257,   261,   265,   270,   275,   282,   288,   295,   302,
     309,   315,   319,   326,   328,   330,   332,   333,   337,   339,
     341,   345,   347,   349,   353,   354,   357,   359,   365,   368,
     370,   372,   374,   376,   378,   380,   383,   386,   389,   392,
     394,   396,   398,   400,   402,   404,   406,   408,   410,   414,
     417,   420,   423,   425,   428,   432,   433,   436,   438,   441,
     444,   446,   448,   449,   451,   454,   457,   459,   462,   464,
     467,   469,   472,   475,   477,   480,   483,   485,   487,   490,
     493,   495,   497,   499,   501,   503,   505,   507,   509,   511,
     513,   515,   517,   519,   521,   523,   525,   527,   529,   531,
     533,   535,   537,   539,   541,   543,   545,   547,   549,   551,
     553,   554,   556,   558,   561,   564,   566,   570,   574,   576,
     580,   581,   584,   586,   589,   590,   592,   599,   602,   605,
     608,   611,   613,   615,   617,   624,   625,   628,   630,   634,
     635,   638,   639,   642,   643,   646,   648,   650,   653,   655,
     658,   661,   665,   669,   671,   673,   677,   681,   685,   688,
     690,   692,   694,   696,   699,   702,   705,   708,   711,   713,
     715,   717,   720,   722,   724,   728,   729,   732,   733
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     158,     0,    -1,    -1,   158,   159,    -1,   158,   166,    -1,
     158,     1,    -1,   158,    46,    -1,   162,    -1,   161,    -1,
     165,    -1,   170,    -1,   172,    -1,   160,    -1,   148,    -1,
     144,    -1,    23,   163,    -1,    57,   163,    -1,   160,    -1,
      -1,   164,   166,    -1,    43,   113,   164,   115,    -1,    91,
     147,    -1,   147,   112,   167,    -1,   170,    -1,   172,    -1,
     168,    -1,   177,    -1,   209,    -1,   218,    -1,   247,    -1,
     253,    -1,   160,    -1,    82,   113,   169,   115,    -1,   142,
      -1,    72,   113,   171,   115,    -1,   177,    -1,   137,    -1,
     171,   165,    -1,   171,   172,    -1,   171,     1,    -1,   173,
     115,    -1,    52,   113,   174,    -1,    59,   113,    -1,    87,
     113,    -1,   173,     1,    -1,    -1,   175,    -1,    78,   230,
     112,   251,    -1,    -1,   176,   177,    -1,   176,     1,    -1,
     182,    -1,   183,    -1,    -1,    19,   113,   176,   115,    -1,
      69,   253,   251,   177,   178,    -1,    29,   113,   176,   115,
      -1,   113,   176,   115,    -1,    30,   113,   176,   115,    -1,
     237,   176,   238,    -1,   196,    -1,    79,   160,   160,   112,
     160,    -1,   141,    -1,   193,    -1,   192,    -1,    86,   147,
     177,    -1,    13,   147,   177,    -1,    42,   177,    -1,    86,
     253,   112,   253,   177,    -1,    90,   147,   177,    -1,    81,
     232,   177,    -1,   179,    -1,   181,    -1,   180,    -1,    83,
     216,   216,   177,    -1,    83,   215,   177,    -1,    15,   160,
     177,    -1,    -1,    58,   184,   177,    -1,    -1,    36,   185,
     177,    -1,    -1,    24,   186,   177,    -1,    -1,    48,   187,
     177,    -1,    -1,    89,   188,   177,    -1,   189,    -1,   190,
      -1,   191,    -1,    67,   227,   177,    -1,    70,   177,   177,
      -1,    88,   147,   177,    -1,    71,   147,   177,   177,    -1,
      82,   147,   112,   147,    -1,    62,   147,   125,   147,   112,
     194,    -1,    62,   147,   125,   147,    85,    -1,    64,   147,
     125,   147,   112,   160,    -1,    64,   147,   125,   147,   112,
     147,    -1,    63,   147,   125,   147,   112,   160,    -1,    63,
     147,   160,   112,   160,    -1,    65,   147,   160,    -1,    62,
     147,   125,   147,    65,   160,    -1,   253,    -1,   252,    -1,
     160,    -1,    -1,   195,   236,   208,    -1,   204,    -1,   197,
      -1,   200,   229,   208,    -1,   114,    -1,   116,    -1,   198,
     201,   199,    -1,    -1,   201,   203,    -1,    93,    -1,   216,
     254,   255,   211,   208,    -1,    92,   208,    -1,   202,    -1,
     205,    -1,   103,    -1,   104,    -1,   106,    -1,   117,    -1,
      20,   147,    -1,    60,   228,    -1,    26,   147,    -1,    80,
     232,    -1,    21,    -1,   206,    -1,   207,    -1,   219,    -1,
     220,    -1,   123,    -1,   124,    -1,    22,    -1,   105,    -1,
      77,   252,   147,    -1,    49,    55,    -1,    49,   194,    -1,
      74,   228,    -1,   175,    -1,    47,    55,    -1,    47,   131,
      10,    -1,    -1,   208,   209,    -1,   221,    -1,   226,   210,
      -1,   226,   221,    -1,   224,    -1,   225,    -1,    -1,   112,
      -1,   112,   213,    -1,   112,   212,    -1,   128,    -1,   212,
     128,    -1,   129,    -1,   213,   129,    -1,   131,    -1,   131,
     212,    -1,   131,   213,    -1,   132,    -1,   132,   212,    -1,
     132,   213,    -1,   214,    -1,   217,    -1,    54,   160,    -1,
      34,   160,    -1,    35,    -1,    44,    -1,   222,    -1,    12,
      -1,    11,    -1,   234,    -1,   120,    -1,   121,    -1,   122,
      -1,    99,    -1,   100,    -1,    98,    -1,   101,    -1,    50,
      -1,   223,    -1,   253,    -1,   130,    -1,   154,    -1,   152,
      -1,   151,    -1,   117,    -1,   116,    -1,   125,    -1,   155,
      -1,   155,    -1,   154,    -1,   151,    -1,   214,    -1,   231,
      -1,   218,    -1,    -1,   231,    -1,   218,    -1,   251,   233,
      -1,   134,   233,    -1,   230,    -1,   231,   119,   251,    -1,
     231,   119,   135,    -1,   135,    -1,   149,   118,   149,    -1,
      -1,   233,   125,    -1,   156,    -1,   156,   251,    -1,    -1,
      94,    -1,   216,   254,   255,   211,   229,   235,    -1,   145,
     229,    -1,   146,   229,    -1,    51,   229,    -1,   147,   229,
      -1,   239,    -1,    96,    -1,    95,    -1,   215,   229,   240,
     242,   243,   244,    -1,    -1,   108,   241,    -1,   245,    -1,
     241,   125,   245,    -1,    -1,   110,   241,    -1,    -1,   118,
     215,    -1,    -1,   107,   215,    -1,   246,    -1,   133,    -1,
     133,   246,    -1,   251,    -1,   251,   152,    -1,   251,   109,
      -1,   247,   152,   248,    -1,   247,   151,   248,    -1,   248,
      -1,   249,    -1,   249,   119,   249,    -1,   249,   118,   249,
      -1,   120,   247,   121,    -1,   151,   249,    -1,   250,    -1,
     149,    -1,   150,    -1,   139,    -1,   150,    27,    -1,   150,
      66,    -1,   150,    45,    -1,   150,    53,    -1,   150,    25,
      -1,   149,    -1,   130,    -1,   250,    -1,   151,   252,    -1,
     147,    -1,   143,    -1,   253,   152,   253,    -1,    -1,   254,
     127,    -1,    -1,   255,   126,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   453,   453,   454,   462,   470,   473,   479,   490,   493,
     496,   499,   502,   509,   513,   522,   528,   532,   538,   541,
     551,   560,   575,   610,   613,   616,   619,   622,   625,   629,
     632,   639,   646,   653,   661,   672,   676,   679,   683,   687,
     697,   707,   711,   715,   719,   725,   728,   734,   748,   751,
     767,   773,   774,   781,   784,   796,   818,   826,   838,   847,
     863,   864,   890,   900,   909,   916,   937,   953,   964,   987,
    1009,  1022,  1025,  1028,  1031,  1043,  1056,  1061,  1060,  1070,
    1069,  1078,  1077,  1093,  1092,  1102,  1101,  1109,  1112,  1115,
    1121,  1136,  1141,  1165,  1183,  1200,  1207,  1214,  1220,  1227,
    1234,  1241,  1248,  1259,  1261,  1263,  1266,  1270,  1277,  1280,
    1286,  1297,  1300,  1304,  1316,  1317,  1323,  1333,  1362,  1377,
    1383,  1386,  1401,  1416,  1424,  1428,  1434,  1443,  1449,  1455,
    1461,  1462,  1466,  1469,  1472,  1476,  1482,  1489,  1499,  1510,
    1518,  1526,  1541,  1545,  1553,  1567,  1570,  1580,  1583,  1589,
    1599,  1602,  1608,  1609,  1610,  1611,  1616,  1620,  1628,  1632,
    1640,  1647,  1655,  1670,  1676,  1684,  1697,  1700,  1706,  1719,
    1733,  1744,  1755,  1758,  1763,  1768,  1774,  1778,  1782,  1786,
    1790,  1794,  1798,  1810,  1815,  1831,  1836,  1851,  1857,  1863,
    1869,  1875,  1881,  1887,  1896,  1897,  1898,  1903,  1909,  1912,
    1921,  1924,  1929,  1939,  1950,  1964,  1969,  1979,  1995,  1996,
    2005,  2006,  2019,  2022,  2089,  2090,  2096,  2138,  2145,  2152,
    2180,  2187,  2199,  2202,  2207,  2216,  2222,  2231,  2234,  2243,
    2249,  2256,  2262,  2271,  2277,  2286,  2292,  2298,  2309,  2320,
    2332,  2349,  2355,  2361,  2365,  2371,  2377,  2386,  2389,  2395,
    2400,  2406,  2412,  2418,  2424,  2430,  2436,  2442,  2452,  2455,
    2461,  2475,  2482,  2485,  2491,  2501,  2502,  2515,  2516
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "DENEMO_MEASURES", "TEXT",
  "staffcontext", "voicecontext", "lyricscontext", "figuredbasscontext",
  "endcontext", "MUSICMODE", "TONEOPTION", "DYNAMICMARK", "AUTOCHANGE",
  "ALIAS", "APPLY", "ARPEGGIO", "DYNAMICSCRIPT", "ACCEPTS", "ALTERNATIVE",
  "BAR", "BREAK", "BREATHE", "CHORDMODIFIERS", "CHORDS", "CHAR_T", "CLEF_",
  "CM_T", "CONSISTS", "SEQUENTIAL", "SIMULTANEOUS", "GROBDESCRIPTIONS",
  "CONSISTSEND", "DENIES", "DURATION", "EXTENDER", "FIGURES",
  "FIGURE_OPEN", "FIGURE_CLOSE", "FIGURE_BRACKET_CLOSE",
  "FIGURE_BRACKET_OPEN", "GLISSANDO", "GRACE", "HEADER", "HYPHEN", "IN_T",
  "INVALID", "KEY", "LYRICS", "MARK", "MARKUP", "MULTI_MEASURE_REST",
  "MIDI", "MM_T", "PITCH", "DEFAULT", "NAME", "PITCHNAMES", "NOTES",
  "PAPER", "PARTIAL_", "PENALTY", "PROPERTY", "OVERRIDE", "SET", "REVERT",
  "PT_T", "RELATIVE", "REMOVE", "REPEAT", "ADDLYRICS", "PARTCOMBINE",
  "SCORE", "SCRIPT", "SKIP", "SPANREQUEST", "STYLESHEET",
  "COMMANDSPANREQUEST", "TEMPO", "OUTPUTPROPERTY", "TIME_T", "TIMES",
  "TRANSLATOR", "TRANSPOSE", "TYPE", "UNSET", "CONTEXT", "LAYOUT",
  "LYRICSTO", "LYRICMODE", "NEWCONTEXT", "LILYVERSION", "DRUM_PITCH",
  "MUSIC_FUNCTION", "REST", "\">>\"", "\"<<\"", "E_CHAR", "E_EXCLAMATION",
  "E_SMALLER", "E_BIGGER", "E_OPEN", "E_CLOSE", "E_LEFTSQUARE",
  "E_RIGHTSQUARE", "E_TILDE", "E_BACKSLASH", "CHORD_BASS", "CHORD_COLON",
  "CHORD_MINUS", "CHORD_CARET", "FIGURE_SPACE", "'='", "'{'", "'<'", "'}'",
  "'>'", "'|'", "'/'", "'*'", "'('", "')'", "'~'", "'['", "']'", "'.'",
  "'?'", "'!'", "'''", "','", "DIGIT", "NOTENAME_PITCH", "TONICNAME_PITCH",
  "CHORDMODIFIER_PITCH", "DURATION_IDENTIFIER", "FRACTION", "IDENTIFIER",
  "SCORE_IDENTIFIER", "MUSIC_OUTPUT_DEF_IDENTIFIER", "NUMBER_IDENTIFIER",
  "REQUEST_IDENTIFIER", "MUSIC_IDENTIFIER", "TRANSLATOR_IDENTIFIER",
  "STRING_IDENTIFIER", "SCM_IDENTIFIER", "RESTNAME", "SKIPNAME", "STRING_",
  "SCM_T", "UNSIGNED", "REAL", "'-'", "'+'", "UNARY_MINUS", "'^'", "'_'",
  "':'", "$accept", "lilypond", "toplevel_expression", "embedded_scm",
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
     365,   366,    61,   123,    60,   125,    62,   124,    47,    42,
      40,    41,   126,    91,    93,    46,    63,    33,    39,    44,
     367,   368,   369,   370,   371,   372,   373,   374,   375,   376,
     377,   378,   379,   380,   381,   382,   383,   384,   385,   386,
     387,    45,    43,   388,    94,    95,    58
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,   157,   158,   158,   158,   158,   158,   159,   159,   159,
     159,   159,   159,   160,   160,   161,   162,   163,   164,   164,
     165,   165,   166,   167,   167,   167,   167,   167,   167,   167,
     167,   167,   168,   169,   170,   171,   171,   171,   171,   171,
     172,   173,   173,   173,   173,   174,   174,   175,   176,   176,
     176,   177,   177,   178,   178,   179,   180,   180,   181,   181,
     182,   182,   182,   182,   182,   183,   183,   183,   183,   183,
     183,   183,   183,   183,   183,   183,   183,   184,   183,   185,
     183,   186,   183,   187,   183,   188,   183,   183,   183,   183,
     189,   190,   190,   191,   192,   193,   193,   193,   193,   193,
     193,   193,   193,   194,   194,   194,   195,   196,   196,   196,
     197,   198,   199,   200,   201,   201,   202,   203,   203,   203,
     204,   204,   204,   204,   204,   204,   204,   204,   204,   204,
     205,   205,   206,   206,   206,   206,   206,   206,   207,   207,
     207,   207,   207,   207,   207,   208,   208,   209,   209,   209,
     210,   210,   211,   211,   211,   211,   212,   212,   213,   213,
     214,   214,   214,   215,   215,   215,   216,   216,   217,   218,
     219,   220,   221,   221,   221,   221,   222,   222,   222,   222,
     222,   222,   222,   223,   224,   224,   224,   225,   225,   225,
     225,   225,   225,   225,   226,   226,   226,   227,   228,   228,
     229,   229,   229,   230,   230,   231,   231,   231,   232,   232,
     233,   233,   234,   234,   235,   235,   236,   236,   236,   236,
     236,   236,   237,   238,   239,   240,   240,   241,   241,   242,
     242,   243,   243,   244,   244,   245,   245,   245,   246,   246,
     246,   247,   247,   247,   248,   248,   248,   249,   249,   249,
     250,   250,   250,   250,   250,   250,   250,   250,   251,   251,
     252,   252,   253,   253,   253,   254,   254,   255,   255
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
       1,     5,     1,     1,     1,     3,     3,     2,     5,     3,
       3,     1,     1,     1,     4,     3,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     1,     1,     1,
       3,     3,     3,     4,     4,     6,     5,     6,     6,     6,
       5,     3,     6,     1,     1,     1,     0,     3,     1,     1,
       3,     1,     1,     3,     0,     2,     1,     5,     2,     1,
       1,     1,     1,     1,     1,     2,     2,     2,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     2,
       2,     2,     1,     2,     3,     0,     2,     1,     2,     2,
       1,     1,     0,     1,     2,     2,     1,     2,     1,     2,
       1,     2,     2,     1,     2,     2,     1,     1,     2,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     1,     1,     2,     2,     1,     3,     3,     1,     3,
       0,     2,     1,     2,     0,     1,     6,     2,     2,     2,
       2,     1,     1,     1,     6,     0,     2,     1,     3,     0,
       2,     0,     2,     0,     2,     1,     1,     2,     1,     2,
       2,     3,     3,     1,     1,     3,     3,     3,     2,     1,
       1,     1,     1,     2,     2,     2,     2,     2,     1,     1,
       1,     2,     1,     1,     3,     0,     2,     0,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     1,     5,     0,     0,     6,     0,     0,     0,
       0,     0,     0,    14,     0,    13,     3,    12,     8,     7,
       9,     4,    10,    11,     0,    17,    15,    18,    45,    16,
      42,   106,    43,    21,   106,    44,    40,     0,     0,    41,
      46,     0,     0,     0,   129,   136,    81,     0,     0,     0,
     170,    79,   106,   171,     0,    83,     0,    77,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    85,     0,   222,   121,
     122,   137,   123,    48,   111,   124,   134,   135,    36,    62,
       0,   142,    35,    71,    73,    72,    51,    52,    87,    88,
      89,    64,    63,     0,    60,   109,   114,   200,   108,   120,
     130,   131,   132,   133,    48,   174,   173,     0,     0,   181,
     179,   180,   182,   176,   177,   178,   252,   263,   262,   250,
     251,   196,   195,   194,   212,    31,    22,    25,    23,    24,
      26,    27,    28,   147,   172,     0,   175,    29,   243,   244,
     249,    30,    20,    19,   259,   210,   258,     0,   210,   106,
     106,   125,   106,   127,    48,    48,   106,    67,   143,     0,
     106,   139,     0,   105,   140,   260,   104,   103,   106,   199,
     126,   205,   198,     0,     0,     0,     0,   160,   197,   106,
       0,   106,   106,   141,     0,     0,   208,     0,   128,   106,
       0,     0,   163,   166,   106,     0,   167,   106,     0,   106,
     106,   106,     0,    39,    34,    37,    38,   200,   200,   200,
     200,   200,   265,   145,   221,     0,   202,   145,   201,     0,
     169,     0,     0,     0,     0,   257,   253,   255,   256,   254,
     248,   213,   183,   191,   190,   176,   192,   186,   189,   188,
     187,   193,   148,   149,   184,   150,   151,   185,     0,     0,
       0,     0,     0,   204,     0,   203,    66,    76,    82,     0,
       0,    80,   144,    84,   261,    78,     0,     0,     0,     0,
       0,   101,   156,   158,   161,   162,    90,   106,    91,   106,
     138,     0,     0,    70,     0,   168,   164,   165,    75,   106,
      65,     0,    92,    86,    69,    50,    57,    49,   219,   217,
     218,   220,   225,   267,   107,   145,   116,   112,   113,   119,
     115,   265,   110,   223,    59,    33,     0,   247,   242,   241,
     246,   245,   264,   211,    47,    56,    58,   207,   206,     0,
       0,     0,     0,   157,   159,    53,    93,     0,   209,    94,
      74,   106,     0,   229,   266,   152,   196,   146,   118,   267,
      32,     0,    96,     0,     0,   100,     0,     0,    55,    61,
      68,   236,   226,   227,   235,   238,     0,   231,   153,   268,
     200,   152,   102,    95,    99,    98,    97,    48,   237,     0,
     240,   239,   230,     0,   233,   155,   154,   214,   145,     0,
     228,   232,     0,   224,   215,   216,   117,    54,   234
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,    16,    25,    18,    19,    26,    37,    20,    21,
     136,   137,   326,    22,    90,    23,    24,    39,    91,   212,
     307,   368,    93,    94,    95,    96,    97,   178,   166,   162,
     170,   210,    98,    99,   100,   101,   102,   174,   103,   104,
     105,   106,   318,   107,   225,   319,   320,   108,   109,   110,
     111,   314,   357,   252,   380,   284,   285,   203,   204,   205,
     206,   226,   112,   113,   143,   144,   254,   255,   256,   145,
     189,   180,   227,   181,   228,   198,   263,   146,   405,   223,
     114,   324,   224,   353,   372,   377,   394,   403,   373,   374,
     234,   148,   149,   150,   158,   176,   177,   313,   355
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -221
static const yytype_int16 yypact[] =
{
    -221,    19,  -221,  -221,   -51,   -55,  -221,   -49,   -51,   -38,
       0,    33,     6,  -221,   -20,  -221,  -221,  -221,  -221,  -221,
    -221,  -221,  -221,  -221,    16,  -221,  -221,  -221,    93,  -221,
    -221,  1319,  -221,  -221,   961,  -221,  -221,   -65,    92,  -221,
    -221,    26,   -51,    37,  -221,  -221,  -221,    44,    79,    82,
    -221,  -221,  1433,  -221,   -37,  -221,    59,  -221,    11,    49,
      52,    68,    69,    86,   -35,  1433,    78,    11,    88,   -51,
      -7,    -7,    81,   -28,    25,    83,  -221,    84,  -221,  -221,
    -221,  -221,  -221,  -221,  -221,  -221,  -221,  -221,  -221,  -221,
     106,  -221,  -221,  -221,  -221,  -221,  -221,  -221,  -221,  -221,
    -221,  -221,  -221,    73,  -221,  -221,  -221,    11,  -221,  -221,
    -221,  -221,  -221,  -221,  -221,  -221,  -221,   -51,   -77,  -221,
    -221,  -221,  -221,    62,  -221,  -221,  -221,  -221,  -221,  -221,
      24,   -24,  -221,  -221,   -44,  -221,  -221,  -221,  -221,  -221,
    -221,  -221,  -221,  -221,  -221,   164,  -221,   -99,  -221,    15,
    -221,    80,  -221,  -221,  -221,  -221,  -221,   121,  -221,  1433,
    1433,  -221,  1433,  -221,  -221,  -221,  1433,  -221,  -221,   224,
    1433,  -221,    88,  -221,  -221,  -221,  -221,    80,  1433,  -221,
    -221,  -221,   116,   128,   -59,   129,   -51,    60,  -221,  1433,
    -105,  1433,  1433,  -221,    99,   -51,  -221,   137,  -221,  1433,
     144,   -51,    60,  -221,  1433,   -22,  -221,  1105,   -72,  1433,
    1433,  1433,   346,  -221,  -221,  -221,  -221,    11,    11,    11,
      11,    11,  -221,  -221,  -221,   -33,  -221,  -221,   116,   475,
    -221,   119,    62,    62,   -78,  -221,  -221,  -221,  -221,  -221,
    -221,  -221,  -221,  -221,  -221,  -221,  -221,  -221,  -221,  -221,
    -221,  -221,  -221,  -221,  -221,  -221,  -221,    80,    62,    62,
      62,    62,   -35,   133,   -44,   133,  -221,  -221,  -221,   589,
     718,  -221,  -221,  -221,  -221,  -221,    94,   120,   123,   154,
     124,  -221,  -221,  -221,   140,   143,  -221,  1433,  -221,  1433,
    -221,   161,   125,  -221,   130,  -221,   140,   143,  -221,  1433,
    -221,   -35,  -221,  -221,  -221,  -221,  -221,  -221,  -221,  -221,
    -221,  -221,   168,   151,     1,  -221,  -221,  -221,  -221,  -221,
    -221,  -221,     1,  -221,  -221,  -221,   167,  -221,  -221,  -221,
    -221,  -221,  -221,  -221,  -221,  -221,  -221,  -221,  -221,   -31,
     171,   -51,   175,  -221,  -221,   269,  -221,   -51,  -221,  -221,
    -221,  1212,    21,   180,  -221,    38,  -221,  -221,     1,   151,
    -221,   -51,  -221,   101,   -51,  -221,   -60,   178,  -221,  -221,
    -221,   -44,   170,  -221,  -221,   -85,    21,   174,    60,  -221,
      11,    38,  -221,  -221,  -221,  -221,  -221,  -221,  -221,    21,
    -221,  -221,   170,   165,   189,   140,   143,   204,  -221,   847,
    -221,  -221,   165,  -221,  -221,  -221,     1,  -221,  -221
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -221,  -221,  -221,    -1,  -221,  -221,   291,  -221,   210,   264,
    -221,  -221,  -221,   270,  -221,   -11,  -221,  -221,   275,  -108,
     -30,  -221,  -221,  -221,  -221,  -221,  -221,  -221,  -221,  -221,
    -221,  -221,  -221,  -221,  -221,  -221,  -221,   -58,  -221,  -221,
    -221,  -221,  -221,  -221,  -221,  -221,  -221,  -221,  -221,  -221,
    -221,  -220,   272,  -221,   -73,  -192,  -191,   247,  -100,   -88,
    -221,     5,  -221,  -221,   169,  -221,  -221,  -221,  -221,  -221,
    -221,   245,  -190,   279,   -21,   242,   166,  -221,  -221,  -221,
    -221,  -221,  -221,  -221,   -53,  -221,  -221,  -221,   -67,   -46,
     292,  -115,  -122,   -54,  -129,   -52,   -26,     7,   -32
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -263
static const yytype_int16 yytable[] =
{
      17,    92,   175,   221,   140,   241,   229,   322,   151,   240,
     296,   297,   115,   116,   175,   222,   194,    35,   168,     2,
       3,   201,   167,   139,   390,   154,   201,   308,   309,   310,
     311,   312,   201,   135,   361,   191,   231,   182,   190,   142,
     301,   160,     4,   327,   156,   117,   182,   262,   208,   235,
     152,   236,   258,   259,   362,   173,   269,   270,    27,   315,
     316,   287,     5,   179,    28,     6,   278,   391,   195,   237,
     200,     7,   179,   258,   259,    30,     8,   238,     9,   216,
     262,   363,    14,   317,    13,    13,   154,   385,    15,    15,
     239,    10,    34,    13,   169,   358,   232,    15,   187,   119,
     120,   121,   122,   187,   202,   156,    11,   213,   127,   187,
      12,   240,   128,    31,   171,   126,   230,   299,   175,   257,
     274,   245,   124,   125,   217,   129,   130,   201,   196,   266,
     267,    36,   268,   260,   261,   334,   271,   321,   330,   331,
     273,   154,   197,   328,   329,   155,    32,   338,   275,     5,
     378,   154,   356,    33,   371,   132,   133,   134,     7,   286,
     156,   288,   289,    13,   379,     9,    14,    15,   127,   293,
     156,    38,   207,   159,   298,   115,   116,   300,   406,   302,
     303,   304,   232,   279,   161,   281,   395,   396,   282,   283,
     397,   163,   164,    11,   291,   165,   183,    12,   126,   184,
     295,   126,   127,    13,   187,   202,   128,    15,   129,   130,
     172,   129,   130,   233,   242,   185,   186,   187,   218,   219,
     220,   214,   154,   375,   154,   192,   155,   126,   200,   337,
     209,   211,   262,   264,   272,   276,   332,   129,   130,   172,
     126,   156,   375,   156,   127,    13,   290,   375,   128,    15,
     129,   130,   172,   277,   280,   292,   294,   345,   333,   346,
     375,   325,   119,   120,   121,   122,   341,   339,   343,   350,
     340,   342,   344,   347,   348,   351,   352,   349,   354,   399,
     243,   244,   360,   364,   245,   124,   125,   366,   367,   246,
     376,   387,   393,   401,   247,   389,   402,   202,   404,    29,
     215,   153,   408,    40,   138,   383,   141,   127,   398,   175,
     188,   128,   193,   199,   253,   248,   249,   157,   250,   251,
     134,   370,   400,   392,   265,   388,   147,   381,   359,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     365,     0,     0,     0,     0,     0,   369,   305,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    41,
     382,    42,   173,   384,     0,   386,    43,    44,    45,     0,
      46,     0,    47,     0,     0,    48,    49,     0,     0,     0,
       0,    50,    51,     0,     0,     0,     0,     0,    52,     0,
      53,     0,     0,    54,    55,    56,     0,  -106,     0,     0,
    -106,     0,     0,     0,    57,     0,    58,     0,    59,    60,
      61,    62,     0,    63,     0,    64,    65,    66,     0,     0,
      67,     0,     0,    68,    38,    69,    70,    71,    72,    73,
       0,     0,    74,     0,    75,    76,    77,     0,     0,     0,
       0,     0,    78,     0,     0,     0,     0,     0,     0,    79,
      80,    81,    82,     0,     0,     0,     0,     0,     0,    83,
      84,   306,     0,    85,     0,     0,     0,     0,     0,    86,
      87,     0,     0,     0,     0,     0,   305,  -106,  -106,     0,
       0,     0,     0,     0,     0,     0,     0,    89,    41,     0,
      42,  -106,  -106,  -106,     0,    43,    44,    45,     0,    46,
       0,    47,     0,     0,    48,    49,     0,     0,     0,     0,
      50,    51,     0,     0,     0,     0,     0,    52,     0,    53,
       0,     0,    54,    55,    56,     0,  -106,     0,     0,  -106,
       0,     0,     0,    57,     0,    58,     0,    59,    60,    61,
      62,     0,    63,     0,    64,    65,    66,     0,     0,    67,
       0,     0,    68,    38,    69,    70,    71,    72,    73,     0,
       0,    74,     0,    75,    76,    77,     0,     0,     0,     0,
     323,    78,     0,     0,     0,     0,     0,     0,    79,    80,
      81,    82,     0,     0,     0,     0,     0,     0,    83,    84,
     305,     0,    85,     0,     0,     0,     0,     0,    86,    87,
       0,     0,    41,     0,    42,     0,  -106,  -106,     0,    43,
      44,    45,     0,    46,     0,    47,    89,     0,    48,    49,
    -106,  -106,  -106,     0,    50,    51,     0,     0,     0,     0,
       0,    52,     0,    53,     0,     0,    54,    55,    56,     0,
    -106,     0,     0,  -106,     0,     0,     0,    57,     0,    58,
       0,    59,    60,    61,    62,     0,    63,     0,    64,    65,
      66,     0,     0,    67,     0,     0,    68,    38,    69,    70,
      71,    72,    73,     0,     0,    74,     0,    75,    76,    77,
       0,     0,     0,     0,     0,    78,     0,     0,     0,     0,
       0,     0,    79,    80,    81,    82,     0,     0,     0,     0,
       0,     0,    83,    84,   335,     0,    85,     0,     0,     0,
       0,     0,    86,    87,     0,     0,     0,     0,     0,   305,
    -106,  -106,     0,     0,     0,     0,     0,     0,     0,     0,
      89,    41,     0,    42,  -106,  -106,  -106,     0,    43,    44,
      45,     0,    46,     0,    47,     0,     0,    48,    49,     0,
       0,     0,     0,    50,    51,     0,     0,     0,     0,     0,
      52,     0,    53,     0,     0,    54,    55,    56,     0,  -106,
       0,     0,  -106,     0,     0,     0,    57,     0,    58,     0,
      59,    60,    61,    62,     0,    63,     0,    64,    65,    66,
       0,     0,    67,     0,     0,    68,    38,    69,    70,    71,
      72,    73,     0,     0,    74,     0,    75,    76,    77,     0,
       0,     0,     0,     0,    78,     0,     0,     0,     0,     0,
       0,    79,    80,    81,    82,     0,     0,     0,     0,     0,
       0,    83,    84,   336,     0,    85,     0,     0,     0,     0,
       0,    86,    87,     0,     0,     0,     0,     0,   305,  -106,
    -106,     0,     0,     0,     0,     0,     0,     0,     0,    89,
      41,     0,    42,  -106,  -106,  -106,     0,    43,    44,    45,
       0,    46,     0,    47,     0,     0,    48,    49,     0,     0,
       0,     0,    50,    51,     0,     0,     0,     0,     0,    52,
       0,    53,     0,     0,    54,    55,    56,     0,  -106,     0,
       0,  -106,     0,     0,     0,    57,     0,    58,     0,    59,
      60,    61,    62,     0,    63,     0,    64,    65,    66,     0,
       0,    67,     0,     0,    68,    38,    69,    70,    71,    72,
      73,     0,     0,    74,     0,    75,    76,    77,     0,     0,
       0,     0,     0,    78,     0,     0,     0,     0,     0,     0,
      79,    80,    81,    82,     0,     0,     0,     0,     0,     0,
      83,    84,   407,     0,    85,     0,     0,     0,     0,     0,
      86,    87,   115,   116,    41,     0,    42,     0,  -106,  -106,
       0,    43,    44,    45,     0,    46,     0,    47,    89,     0,
      48,    49,  -106,  -106,  -106,   117,    50,    51,     0,     0,
       0,     0,     0,    52,     0,    53,     0,     0,    54,    55,
      56,     0,     0,     7,     0,     0,     0,     0,     0,    57,
       9,    58,     0,    59,    60,    61,    62,     0,    63,     0,
      64,    65,    66,    10,     0,    67,     0,     0,    68,    38,
      69,    70,    71,   118,    73,     0,     0,    74,    11,    75,
      76,    77,     0,     0,     0,     0,     0,    78,     0,   119,
     120,   121,   122,     0,    79,    80,    81,    82,     0,     0,
       0,     0,     0,     0,    83,    84,     0,     0,    85,     0,
       0,   123,   124,   125,    86,    87,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     126,     0,    89,     0,   127,    13,     0,     0,   128,    15,
     129,   130,   131,     0,     0,   132,   133,   134,    41,     0,
      42,     0,     0,     0,     0,    43,    44,    45,     0,    46,
       0,    47,     0,     0,    48,    49,     0,     0,     0,     0,
      50,    51,     0,     0,     0,     0,     0,    52,     0,    53,
       0,     0,    54,    55,    56,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,    58,     0,    59,    60,    61,
      62,     0,    63,     0,    64,    65,    66,     0,     0,    67,
       0,     0,    68,    38,    69,    70,    71,    72,    73,     0,
       0,    74,     0,    75,    76,    77,     0,     0,     0,     0,
       0,    78,     0,     0,     0,     0,     0,     0,    79,    80,
      81,    82,     0,     0,     0,     0,     0,  -262,    83,    84,
       0,     0,    85,     0,     0,    41,     0,    42,    86,    87,
       0,     0,    43,    44,    45,     0,    46,     0,    47,     0,
       0,    48,    49,     0,     0,     0,    89,    50,    51,     0,
       0,     0,     0,     0,    52,     0,    53,  -262,     0,    54,
      55,    56,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,    58,     0,    59,    60,    61,    62,     0,    63,
       0,    64,    65,    66,     0,     0,    67,     0,     0,    68,
      38,    69,    70,    71,    72,    73,     0,     0,    74,     0,
      75,    76,    77,     0,     0,     0,     0,     0,    78,     0,
       0,     0,     0,     0,     0,    79,    80,    81,    82,     0,
       0,     0,     0,     0,     0,    83,    84,     0,     0,    85,
       0,     0,    41,     0,    42,    86,    87,     0,     0,    43,
      44,    45,     0,    46,     0,    47,     0,     0,    48,    49,
       0,     0,     0,    89,    50,    51,     0,     0,     0,     0,
       0,    52,     0,    53,   262,     0,    54,    55,    56,     0,
       0,     0,     0,     0,     0,     0,     0,    57,     0,    58,
       0,    59,    60,    61,    62,     0,    63,     0,    64,    65,
      66,     0,     0,    67,     0,     0,    68,    38,    69,    70,
      71,    72,    73,     0,     0,    74,     0,    75,    76,    77,
       0,     0,     0,     0,     0,    78,     0,     0,     0,     0,
       0,     0,    79,    80,    81,    82,     0,     0,     0,     0,
       0,     0,    83,    84,     0,     0,    85,     0,     0,     0,
       0,     0,    86,    87,     0,     0,    41,     0,    42,     0,
       0,     0,     0,    43,    44,    45,    88,    46,     0,    47,
      89,     0,    48,    49,     0,     0,     0,     0,    50,    51,
       0,     0,     0,     0,     0,    52,     0,    53,     0,     0,
      54,    55,    56,     0,     0,     0,     0,     0,     0,     0,
       0,    57,     0,    58,     0,    59,    60,    61,    62,     0,
      63,     0,    64,    65,    66,     0,     0,    67,     0,     0,
      68,    38,    69,    70,    71,    72,    73,     0,     0,    74,
       0,    75,    76,    77,     0,     0,     0,     0,     0,    78,
       0,     0,     0,     0,     0,     0,    79,    80,    81,    82,
       0,     0,     0,     0,     0,     0,    83,    84,     0,     0,
      85,     0,     0,     0,     0,     0,    86,    87,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    89
};

static const yytype_int16 yycheck[] =
{
       1,    31,    56,   103,    34,   134,   114,   227,    34,   131,
     202,   202,    11,    12,    68,   103,    68,     1,    55,     0,
       1,    54,    52,    34,   109,   130,    54,   217,   218,   219,
     220,   221,    54,    34,    65,    65,   113,    58,    64,    34,
     112,    42,    23,   121,   149,    34,    67,   152,    74,    25,
     115,    27,   151,   152,    85,    56,   164,   165,   113,    92,
      93,   190,    43,    58,   113,    46,   125,   152,    69,    45,
     147,    52,    67,   151,   152,   113,    57,    53,    59,    90,
     152,   112,   147,   116,   144,   144,   130,   147,   148,   148,
      66,    72,   112,   144,   131,   315,   120,   148,   131,    98,
      99,   100,   101,   131,   132,   149,    87,     1,   143,   131,
      91,   233,   147,   113,    55,   139,   117,   205,   172,   145,
     172,   120,   121,   122,    51,   149,   150,    54,   135,   159,
     160,   115,   162,   118,   119,   264,   166,   225,   260,   261,
     170,   130,   149,   258,   259,   134,   113,   276,   178,    43,
     112,   130,   151,   147,   133,   154,   155,   156,    52,   189,
     149,   191,   192,   144,   126,    59,   147,   148,   143,   199,
     149,    78,   147,   147,   204,    11,    12,   207,   398,   209,
     210,   211,   120,   184,   147,   186,   378,   378,   128,   129,
     380,   147,   113,    87,   195,   113,   147,    91,   139,   147,
     201,   139,   143,   144,   131,   132,   147,   148,   149,   150,
     151,   149,   150,   151,    50,   147,   147,   131,   145,   146,
     147,   115,   130,   352,   130,   147,   134,   139,   147,   135,
     147,   147,   152,   112,    10,   119,   262,   149,   150,   151,
     139,   149,   371,   149,   143,   144,   147,   376,   147,   148,
     149,   150,   151,   125,   125,   118,   112,   287,   125,   289,
     389,   142,    98,    99,   100,   101,   112,   147,   128,   299,
     147,   147,   129,   112,   149,   301,   108,   147,   127,   387,
     116,   117,   115,   112,   120,   121,   122,   112,    19,   125,
     110,   113,   118,   393,   130,   125,   107,   132,    94,     8,
      90,    37,   402,    28,    34,   363,    34,   143,   381,   363,
      63,   147,    67,    71,   145,   151,   152,    38,   154,   155,
     156,   351,   389,   376,   158,   371,    34,   359,   321,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     341,    -1,    -1,    -1,    -1,    -1,   347,     1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
     361,    15,   363,   364,    -1,   366,    20,    21,    22,    -1,
      24,    -1,    26,    -1,    -1,    29,    30,    -1,    -1,    -1,
      -1,    35,    36,    -1,    -1,    -1,    -1,    -1,    42,    -1,
      44,    -1,    -1,    47,    48,    49,    -1,    51,    -1,    -1,
      54,    -1,    -1,    -1,    58,    -1,    60,    -1,    62,    63,
      64,    65,    -1,    67,    -1,    69,    70,    71,    -1,    -1,
      74,    -1,    -1,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    86,    -1,    88,    89,    90,    -1,    -1,    -1,
      -1,    -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,   105,   106,    -1,    -1,    -1,    -1,    -1,    -1,   113,
     114,   115,    -1,   117,    -1,    -1,    -1,    -1,    -1,   123,
     124,    -1,    -1,    -1,    -1,    -1,     1,   131,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    13,    -1,
      15,   145,   146,   147,    -1,    20,    21,    22,    -1,    24,
      -1,    26,    -1,    -1,    29,    30,    -1,    -1,    -1,    -1,
      35,    36,    -1,    -1,    -1,    -1,    -1,    42,    -1,    44,
      -1,    -1,    47,    48,    49,    -1,    51,    -1,    -1,    54,
      -1,    -1,    -1,    58,    -1,    60,    -1,    62,    63,    64,
      65,    -1,    67,    -1,    69,    70,    71,    -1,    -1,    74,
      -1,    -1,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    86,    -1,    88,    89,    90,    -1,    -1,    -1,    -1,
      95,    96,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
     105,   106,    -1,    -1,    -1,    -1,    -1,    -1,   113,   114,
       1,    -1,   117,    -1,    -1,    -1,    -1,    -1,   123,   124,
      -1,    -1,    13,    -1,    15,    -1,   131,   132,    -1,    20,
      21,    22,    -1,    24,    -1,    26,   141,    -1,    29,    30,
     145,   146,   147,    -1,    35,    36,    -1,    -1,    -1,    -1,
      -1,    42,    -1,    44,    -1,    -1,    47,    48,    49,    -1,
      51,    -1,    -1,    54,    -1,    -1,    -1,    58,    -1,    60,
      -1,    62,    63,    64,    65,    -1,    67,    -1,    69,    70,
      71,    -1,    -1,    74,    -1,    -1,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    86,    -1,    88,    89,    90,
      -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,   105,   106,    -1,    -1,    -1,    -1,
      -1,    -1,   113,   114,   115,    -1,   117,    -1,    -1,    -1,
      -1,    -1,   123,   124,    -1,    -1,    -1,    -1,    -1,     1,
     131,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    13,    -1,    15,   145,   146,   147,    -1,    20,    21,
      22,    -1,    24,    -1,    26,    -1,    -1,    29,    30,    -1,
      -1,    -1,    -1,    35,    36,    -1,    -1,    -1,    -1,    -1,
      42,    -1,    44,    -1,    -1,    47,    48,    49,    -1,    51,
      -1,    -1,    54,    -1,    -1,    -1,    58,    -1,    60,    -1,
      62,    63,    64,    65,    -1,    67,    -1,    69,    70,    71,
      -1,    -1,    74,    -1,    -1,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    -1,    88,    89,    90,    -1,
      -1,    -1,    -1,    -1,    96,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,   105,   106,    -1,    -1,    -1,    -1,    -1,
      -1,   113,   114,   115,    -1,   117,    -1,    -1,    -1,    -1,
      -1,   123,   124,    -1,    -1,    -1,    -1,    -1,     1,   131,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      13,    -1,    15,   145,   146,   147,    -1,    20,    21,    22,
      -1,    24,    -1,    26,    -1,    -1,    29,    30,    -1,    -1,
      -1,    -1,    35,    36,    -1,    -1,    -1,    -1,    -1,    42,
      -1,    44,    -1,    -1,    47,    48,    49,    -1,    51,    -1,
      -1,    54,    -1,    -1,    -1,    58,    -1,    60,    -1,    62,
      63,    64,    65,    -1,    67,    -1,    69,    70,    71,    -1,
      -1,    74,    -1,    -1,    77,    78,    79,    80,    81,    82,
      83,    -1,    -1,    86,    -1,    88,    89,    90,    -1,    -1,
      -1,    -1,    -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,   105,   106,    -1,    -1,    -1,    -1,    -1,    -1,
     113,   114,   115,    -1,   117,    -1,    -1,    -1,    -1,    -1,
     123,   124,    11,    12,    13,    -1,    15,    -1,   131,   132,
      -1,    20,    21,    22,    -1,    24,    -1,    26,   141,    -1,
      29,    30,   145,   146,   147,    34,    35,    36,    -1,    -1,
      -1,    -1,    -1,    42,    -1,    44,    -1,    -1,    47,    48,
      49,    -1,    -1,    52,    -1,    -1,    -1,    -1,    -1,    58,
      59,    60,    -1,    62,    63,    64,    65,    -1,    67,    -1,
      69,    70,    71,    72,    -1,    74,    -1,    -1,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    86,    87,    88,
      89,    90,    -1,    -1,    -1,    -1,    -1,    96,    -1,    98,
      99,   100,   101,    -1,   103,   104,   105,   106,    -1,    -1,
      -1,    -1,    -1,    -1,   113,   114,    -1,    -1,   117,    -1,
      -1,   120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     139,    -1,   141,    -1,   143,   144,    -1,    -1,   147,   148,
     149,   150,   151,    -1,    -1,   154,   155,   156,    13,    -1,
      15,    -1,    -1,    -1,    -1,    20,    21,    22,    -1,    24,
      -1,    26,    -1,    -1,    29,    30,    -1,    -1,    -1,    -1,
      35,    36,    -1,    -1,    -1,    -1,    -1,    42,    -1,    44,
      -1,    -1,    47,    48,    49,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    58,    -1,    60,    -1,    62,    63,    64,
      65,    -1,    67,    -1,    69,    70,    71,    -1,    -1,    74,
      -1,    -1,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    86,    -1,    88,    89,    90,    -1,    -1,    -1,    -1,
      -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
     105,   106,    -1,    -1,    -1,    -1,    -1,   112,   113,   114,
      -1,    -1,   117,    -1,    -1,    13,    -1,    15,   123,   124,
      -1,    -1,    20,    21,    22,    -1,    24,    -1,    26,    -1,
      -1,    29,    30,    -1,    -1,    -1,   141,    35,    36,    -1,
      -1,    -1,    -1,    -1,    42,    -1,    44,   152,    -1,    47,
      48,    49,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      58,    -1,    60,    -1,    62,    63,    64,    65,    -1,    67,
      -1,    69,    70,    71,    -1,    -1,    74,    -1,    -1,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    86,    -1,
      88,    89,    90,    -1,    -1,    -1,    -1,    -1,    96,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,   105,   106,    -1,
      -1,    -1,    -1,    -1,    -1,   113,   114,    -1,    -1,   117,
      -1,    -1,    13,    -1,    15,   123,   124,    -1,    -1,    20,
      21,    22,    -1,    24,    -1,    26,    -1,    -1,    29,    30,
      -1,    -1,    -1,   141,    35,    36,    -1,    -1,    -1,    -1,
      -1,    42,    -1,    44,   152,    -1,    47,    48,    49,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    58,    -1,    60,
      -1,    62,    63,    64,    65,    -1,    67,    -1,    69,    70,
      71,    -1,    -1,    74,    -1,    -1,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    86,    -1,    88,    89,    90,
      -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,   105,   106,    -1,    -1,    -1,    -1,
      -1,    -1,   113,   114,    -1,    -1,   117,    -1,    -1,    -1,
      -1,    -1,   123,   124,    -1,    -1,    13,    -1,    15,    -1,
      -1,    -1,    -1,    20,    21,    22,   137,    24,    -1,    26,
     141,    -1,    29,    30,    -1,    -1,    -1,    -1,    35,    36,
      -1,    -1,    -1,    -1,    -1,    42,    -1,    44,    -1,    -1,
      47,    48,    49,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    58,    -1,    60,    -1,    62,    63,    64,    65,    -1,
      67,    -1,    69,    70,    71,    -1,    -1,    74,    -1,    -1,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    86,
      -1,    88,    89,    90,    -1,    -1,    -1,    -1,    -1,    96,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,   105,   106,
      -1,    -1,    -1,    -1,    -1,    -1,   113,   114,    -1,    -1,
     117,    -1,    -1,    -1,    -1,    -1,   123,   124,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,   158,     0,     1,    23,    43,    46,    52,    57,    59,
      72,    87,    91,   144,   147,   148,   159,   160,   161,   162,
     165,   166,   170,   172,   173,   160,   163,   113,   113,   163,
     113,   113,   113,   147,   112,     1,   115,   164,    78,   174,
     175,    13,    15,    20,    21,    22,    24,    26,    29,    30,
      35,    36,    42,    44,    47,    48,    49,    58,    60,    62,
      63,    64,    65,    67,    69,    70,    71,    74,    77,    79,
      80,    81,    82,    83,    86,    88,    89,    90,    96,   103,
     104,   105,   106,   113,   114,   117,   123,   124,   137,   141,
     171,   175,   177,   179,   180,   181,   182,   183,   189,   190,
     191,   192,   193,   195,   196,   197,   198,   200,   204,   205,
     206,   207,   219,   220,   237,    11,    12,    34,    82,    98,
      99,   100,   101,   120,   121,   122,   139,   143,   147,   149,
     150,   151,   154,   155,   156,   160,   167,   168,   170,   172,
     177,   209,   218,   221,   222,   226,   234,   247,   248,   249,
     250,   253,   115,   166,   130,   134,   149,   230,   251,   147,
     160,   147,   186,   147,   113,   113,   185,   177,    55,   131,
     187,    55,   151,   160,   194,   250,   252,   253,   184,   218,
     228,   230,   231,   147,   147,   147,   147,   131,   214,   227,
     253,   177,   147,   228,   252,   160,   135,   149,   232,   232,
     147,    54,   132,   214,   215,   216,   217,   147,   253,   147,
     188,   147,   176,     1,   115,   165,   172,    51,   145,   146,
     147,   215,   216,   236,   239,   201,   218,   229,   231,   176,
     160,   113,   120,   151,   247,    25,    27,    45,    53,    66,
     249,   251,    50,   116,   117,   120,   125,   130,   151,   152,
     154,   155,   210,   221,   223,   224,   225,   253,   151,   152,
     118,   119,   152,   233,   112,   233,   177,   177,   177,   176,
     176,   177,    10,   177,   252,   177,   119,   125,   125,   160,
     125,   160,   128,   129,   212,   213,   177,   251,   177,   177,
     147,   160,   118,   177,   112,   160,   212,   213,   177,   216,
     177,   112,   177,   177,   177,     1,   115,   177,   229,   229,
     229,   229,   229,   254,   208,    92,    93,   116,   199,   202,
     203,   216,   208,    95,   238,   142,   169,   121,   248,   248,
     249,   249,   253,   125,   251,   115,   115,   135,   251,   147,
     147,   112,   147,   128,   129,   177,   177,   112,   149,   147,
     177,   253,   108,   240,   127,   255,   151,   209,   208,   254,
     115,    65,    85,   112,   112,   160,   112,    19,   178,   160,
     177,   133,   241,   245,   246,   251,   110,   242,   112,   126,
     211,   255,   160,   194,   160,   147,   160,   113,   246,   125,
     109,   152,   241,   118,   243,   212,   213,   229,   211,   176,
     245,   215,   107,   244,    94,   235,   208,   115,   215
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
#line 454 "./lyparser.y"
    { 
		if(lily_file)  {
		 	GList *ret;
			ret = g_list_concat (lily_file, (yyvsp[(2) - (2)].scm));
		} else {
		lily_file = (yyvsp[(2) - (2)].scm);
		}
	;}
    break;

  case 4:
#line 462 "./lyparser.y"
    { 
		if(lily_file)  {
		 	GList *ret;
			ret = g_list_concat(lily_file, (yyvsp[(2) - (2)].scm));
		} else {
		lily_file = (yyvsp[(2) - (2)].scm);
		}
	;}
    break;

  case 5:
#line 470 "./lyparser.y"
    {
		error_level_  = 1;
	;}
    break;

  case 6:
#line 473 "./lyparser.y"
    {
		error_level_  = 1;
	;}
    break;

  case 7:
#line 480 "./lyparser.y"
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
#line 490 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 9:
#line 493 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 10:
#line 496 "./lyparser.y"
    {
		(yyval.scm) =  (yyvsp[(1) - (1)].scm);/* add this score to the root data list lily_file */
	;}
    break;

  case 11:
#line 499 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 12:
#line 502 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 13:
#line 509 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (1)].generic));
	(yyval.scm) = g_list_append(NULL,n);
	;}
    break;

  case 14:
#line 513 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (1)].gstr));
	((nodegstr*)n)->gstr = (yyvsp[(1) - (1)].gstr).gstr;
	(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 15:
#line 522 "./lyparser.y"
    {  (yyval.scm) = (yyvsp[(2) - (2)].scm); 
/*	intercept this at lexical level*/
	;}
    break;

  case 16:
#line 528 "./lyparser.y"
    {  (yyval.scm) = (yyvsp[(2) - (2)].scm); ;}
    break;

  case 17:
#line 532 "./lyparser.y"
    {
	(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 18:
#line 538 "./lyparser.y"
    {
	(yyval.scm) = NULL;
	;}
    break;

  case 19:
#line 541 "./lyparser.y"
    { 
		if((yyvsp[(1) - (2)].scm)) {
			(yyval.scm) = g_list_concat((yyvsp[(1) - (2)].scm), (yyvsp[(2) - (2)].scm));
			}
		else 
			(yyval.scm) = (yyvsp[(2) - (2)].scm);
	;}
    break;

  case 20:
#line 551 "./lyparser.y"
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
#line 561 "./lyparser.y"
    {
		MALLOC_NODE(n,(yyvsp[(1) - (2)].generic));
	    g_free(n->user_string);
	    n->user_string = (yyvsp[(2) - (2)].gstr).gstr->str;
	set_identifier("lilyversion", typed_glist (g_list_append(NULL,n), STRING_IDENTIFIER));
	(yyval.scm) = NULL;
	;}
    break;

  case 22:
#line 575 "./lyparser.y"
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
#line 610 "./lyparser.y"
    { /* I don't think this can ever get used, once defined! */
		(yyval.branch) = typed_glist ((yyvsp[(1) - (1)].scm), SCORE_IDENTIFIER);
	;}
    break;

  case 24:
#line 613 "./lyparser.y"
    {
		(yyval.branch) = typed_glist ((yyvsp[(1) - (1)].scm), MUSIC_OUTPUT_DEF_IDENTIFIER);
	;}
    break;

  case 25:
#line 616 "./lyparser.y"
    {
		(yyval.branch) = typed_glist (g_list_append(NULL,(yyvsp[(1) - (1)].scm)), TRANSLATOR_IDENTIFIER);
	;}
    break;

  case 26:
#line 619 "./lyparser.y"
    {
		(yyval.branch) = typed_glist ((yyvsp[(1) - (1)].scm), MUSIC_IDENTIFIER);
	;}
    break;

  case 27:
#line 622 "./lyparser.y"
    {
		(yyval.branch) = typed_glist ((yyvsp[(1) - (1)].scm), REQUEST_IDENTIFIER);
	;}
    break;

  case 28:
#line 625 "./lyparser.y"
    {
		MALLOC_NODE(n,(yyvsp[(1) - (1)].f));
		(yyval.branch) = typed_glist (g_list_append(NULL,n), DURATION_IDENTIFIER);
	;}
    break;

  case 29:
#line 629 "./lyparser.y"
    {
		(yyval.branch) = typed_glist ((yyvsp[(1) - (1)].scm), STRING_IDENTIFIER);
	;}
    break;

  case 30:
#line 632 "./lyparser.y"
    {
		nodegeneric x;
		x.type = STRING_IDENTIFIER;
		x.user_string = strdup((yyvsp[(1) - (1)].gstr).gstr->str);
		MALLOC_NODE(n,x);
		(yyval.branch) = typed_glist (g_list_append(NULL,n), STRING_IDENTIFIER);
	;}
    break;

  case 31:
#line 639 "./lyparser.y"
    {
		(yyval.branch) = typed_glist ((yyvsp[(1) - (1)].scm), SCM_IDENTIFIER);
	;}
    break;

  case 32:
#line 647 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(3) - (4)].scm);
	;}
    break;

  case 33:
#line 653 "./lyparser.y"
    {
	;}
    break;

  case 34:
#line 661 "./lyparser.y"
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
#line 672 "./lyparser.y"
    {

		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 36:
#line 676 "./lyparser.y"
    {
		(yyval.scm) = g_list_append(NULL, (yyvsp[(1) - (1)].id).id);
	;}
    break;

  case 37:
#line 679 "./lyparser.y"
    {
	lyerror ("parser should have caught this");
		/*intercept this at lexical level*/	
	;}
    break;

  case 38:
#line 683 "./lyparser.y"
    {
	//lyerror ("parser should have caught this");
		/*intercept this at lexical level*/
	;}
    break;

  case 39:
#line 687 "./lyparser.y"
    {
	lyerror("score_body error");
	;}
    break;

  case 40:
#line 697 "./lyparser.y"
    {
		MALLOC_NODE(n2, (yyvsp[(2) - (2)].generic))
		(yyval.scm) = g_list_append((yyvsp[(1) - (2)].scm), n2);

/*		THIS-> lexer_-> scopes_.pop ();*/
	;}
    break;

  case 41:
#line 707 "./lyparser.y"
    {
		set_identifier("midi_tempo", typed_glist ((yyvsp[(3) - (3)].scm), STRING_IDENTIFIER));
		(yyval.scm) = (yyvsp[(3) - (3)].scm);
	;}
    break;

  case 42:
#line 711 "./lyparser.y"
    {
		/* caught by lexer - does not occur*/
		lyerror ("parser should have caught this");
	;}
    break;

  case 43:
#line 715 "./lyparser.y"
    {
		/* caught by lexer - does not occur*/
		lyerror ("parser should have caught this");
	;}
    break;

  case 44:
#line 719 "./lyparser.y"
    {
		lyerror("music_output_def_body error");
		;}
    break;

  case 45:
#line 725 "./lyparser.y"
    {
		(yyval.scm) = NULL;
	;}
    break;

  case 46:
#line 728 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 47:
#line 734 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(4) - (4)].i));
	((nodei*)n)->i = (yyvsp[(4) - (4)].i).i;
	(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 48:
#line 748 "./lyparser.y"
    {
		(yyval.scm) = NULL;
	;}
    break;

  case 49:
#line 751 "./lyparser.y"
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
#line 767 "./lyparser.y"
    {
	;}
    break;

  case 52:
#line 775 "./lyparser.y"
    {
			(yyval.scm) = (yyvsp[(1) - (1)].scm);
		;}
    break;

  case 53:
#line 781 "./lyparser.y"
    {
			(yyval.scm) = NULL;
	;}
    break;

  case 54:
#line 784 "./lyparser.y"
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
#line 797 "./lyparser.y"
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
#line 818 "./lyparser.y"
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
#line 826 "./lyparser.y"
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
#line 838 "./lyparser.y"
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
#line 847 "./lyparser.y"
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
#line 863 "./lyparser.y"
    { (yyval.scm) = (yyvsp[(1) - (1)].scm); ;}
    break;

  case 61:
#line 864 "./lyparser.y"
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

  case 62:
#line 890 "./lyparser.y"
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

  case 63:
#line 900 "./lyparser.y"
    {

		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	/* FIXME - we really don't want to put all these nodes into
	   the music, as denemo will have to go over them -
	   amalgamate the strings into a TEXT node */
		

	;}
    break;

  case 64:
#line 909 "./lyparser.y"
    {
	LATER_MESSAGE((yyloc).first_line);
	;}
    break;

  case 65:
#line 916 "./lyparser.y"
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

  case 66:
#line 937 "./lyparser.y"
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

  case 67:
#line 953 "./lyparser.y"
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

  case 68:
#line 964 "./lyparser.y"
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

  case 69:
#line 987 "./lyparser.y"
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

  case 70:
#line 1011 "./lyparser.y"
    {DenemoObject *tupopen, *tupclose;
		tupopen = newtupopen ((yyvsp[(2) - (3)].t).t.a, (yyvsp[(2) - (3)].t).t.b);
		tupclose = newtupclose ();
		g_assert(ntype((yyvsp[(3) - (3)].scm))==SEQUENTIAL);
		
		tupopen->user_string = g_strconcat((yyvsp[(1) - (3)].generic).user_string, 
						   (yyvsp[(2) - (3)].t).user_string, u_str((yyvsp[(3) - (3)].scm)),
						   NULL);
		tupclose->user_string = u_post_str((yyvsp[(3) - (3)].scm));
		(yyval.scm) = g_list_append(g_list_prepend (br((yyvsp[(3) - (3)].scm)), tupopen), tupclose);
	;}
    break;

  case 71:
#line 1022 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 72:
#line 1025 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 73:
#line 1028 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 74:
#line 1031 "./lyparser.y"
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

  case 75:
#line 1043 "./lyparser.y"
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

  case 76:
#line 1056 "./lyparser.y"
    {
		u_str((yyvsp[(2) - (3)].scm)) = g_strconcat((yyvsp[(1) - (3)].generic).user_string, u_str((yyvsp[(2) - (3)].scm)), NULL);
		(yyval.scm) = g_list_concat((yyvsp[(2) - (3)].scm), (yyvsp[(3) - (3)].scm));
	;}
    break;

  case 77:
#line 1061 "./lyparser.y"
    { push_note_state (); ;}
    break;

  case 78:
#line 1064 "./lyparser.y"
    { 
		MALLOC_NODE(n1, (yyvsp[(1) - (3)].generic));
		(yyval.scm) = g_list_prepend((yyvsp[(3) - (3)].scm), n1);
		pop_state();
		;}
    break;

  case 79:
#line 1070 "./lyparser.y"
    { push_figuredbass_state (); ;}
    break;

  case 80:
#line 1072 "./lyparser.y"
    {
		MALLOC_NODE(n1, (yyvsp[(1) - (3)].generic));
		(yyval.scm) = g_list_prepend((yyvsp[(3) - (3)].scm), n1);
		pop_state();
	;}
    break;

  case 81:
#line 1078 "./lyparser.y"
    { push_chord_state (); ;}
    break;

  case 82:
#line 1080 "./lyparser.y"
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

  case 83:
#line 1093 "./lyparser.y"
    { push_lyric_state (); ;}
    break;

  case 84:
#line 1095 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
		MALLOC_NODE(n, (yyvsp[(1) - (3)].generic))
		(yyval.scm) = g_list_prepend((yyvsp[(3) - (3)].scm), n);
		pop_state();
	;}
    break;

  case 85:
#line 1102 "./lyparser.y"
    { push_lyric_state (); ;}
    break;

  case 86:
#line 1104 "./lyparser.y"
    {
		MALLOC_NODE(n, (yyvsp[(1) - (3)].generic))
		(yyval.scm) = g_list_prepend((yyvsp[(3) - (3)].scm), n);
		pop_state();
	;}
    break;

  case 87:
#line 1109 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 88:
#line 1112 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 89:
#line 1115 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 90:
#line 1121 "./lyparser.y"
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

  case 91:
#line 1136 "./lyparser.y"
    {
		MALLOC_NODE(n, (yyvsp[(1) - (3)].generic));
		((nodeglist*)n)->branch = g_list_append(g_list_append(NULL, g_list_append(NULL, (yyvsp[(2) - (3)].scm))), g_list_append(NULL, (yyvsp[(3) - (3)].scm))); /* ADDLYRICS is a branch containing two GLists */
		(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 92:
#line 1141 "./lyparser.y"
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

  case 93:
#line 1165 "./lyparser.y"
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

  case 94:
#line 1183 "./lyparser.y"
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

  case 95:
#line 1200 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (6)].generic))
	 n->user_string = g_strconcat((yyvsp[(1) - (6)].generic).user_string, (yyvsp[(2) - (6)].gstr).user_string, 
				      ".", (yyvsp[(4) - (6)].gstr).user_string, 
				      "=", u_str((yyvsp[(6) - (6)].scm)), NULL);
	(yyval.scm) = g_list_append(NULL, n); /* FIXME memory leak*/
	;}
    break;

  case 96:
#line 1207 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (5)].generic))
	 n->user_string = g_strconcat((yyvsp[(1) - (5)].generic).user_string, (yyvsp[(2) - (5)].gstr).user_string, 
				      (yyvsp[(3) - (5)].i).user_string, (yyvsp[(4) - (5)].gstr).user_string, 
				      (yyvsp[(5) - (5)].generic).user_string, NULL);
	(yyval.scm) = g_list_append(NULL, n); /* FIXME memory leak*/
	;}
    break;

  case 97:
#line 1214 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (6)].generic))
	n->user_string = g_strconcat((yyvsp[(1) - (6)].generic).user_string, (yyvsp[(2) - (6)].gstr).user_string, 
				     (yyvsp[(3) - (6)].i).user_string, (yyvsp[(4) - (6)].gstr).user_string, (yyvsp[(5) - (6)].generic).user_string, u_str((yyvsp[(6) - (6)].scm)), NULL);
	(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 98:
#line 1220 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (6)].generic))
	n->user_string = g_strconcat((yyvsp[(1) - (6)].generic).user_string, (yyvsp[(2) - (6)].gstr).user_string, 
				     (yyvsp[(3) - (6)].i).user_string, (yyvsp[(4) - (6)].gstr).user_string, 
				(yyvsp[(5) - (6)].generic).user_string, (yyvsp[(6) - (6)].gstr).user_string, NULL);
	(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 99:
#line 1227 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (6)].generic));
	n->user_string = g_strconcat((yyvsp[(1) - (6)].generic).user_string, (yyvsp[(2) - (6)].gstr).user_string, 
				     (yyvsp[(3) - (6)].i).user_string, (yyvsp[(4) - (6)].gstr).user_string, 
				     (yyvsp[(5) - (6)].generic).user_string, u_str((yyvsp[(6) - (6)].scm)), NULL);
	(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 100:
#line 1234 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (5)].generic));
	n->user_string = g_strconcat((yyvsp[(1) - (5)].generic).user_string, (yyvsp[(2) - (5)].gstr).user_string, 
				     u_str ((yyvsp[(3) - (5)].scm)), (yyvsp[(4) - (5)].generic).user_string, 
				     u_str ((yyvsp[(5) - (5)].scm)), NULL);
	(yyval.scm) = g_list_append (NULL, n);
        ;}
    break;

  case 101:
#line 1241 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (3)].generic));
	n->user_string = g_strconcat((yyvsp[(1) - (3)].generic).user_string, (yyvsp[(2) - (3)].gstr).user_string, 
				     u_str ((yyvsp[(3) - (3)].scm)), NULL);
	(yyval.scm) = g_list_append (NULL, n);
        ;}
    break;

  case 102:
#line 1248 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (6)].generic));
	n->user_string = g_strconcat((yyvsp[(1) - (6)].generic).user_string, (yyvsp[(2) - (6)].gstr).user_string, 
				     (yyvsp[(3) - (6)].i).user_string, (yyvsp[(4) - (6)].gstr).user_string, 
				     (yyvsp[(5) - (6)].generic).user_string, u_str ((yyvsp[(6) - (6)].scm)), NULL);
	(yyval.scm) = g_list_append (NULL, n);
	;}
    break;

  case 103:
#line 1259 "./lyparser.y"
    { MALLOC_NODE(n, (yyvsp[(1) - (1)].gstr));(yyval.scm) = g_list_append (NULL, n); 
			/*FIXME copy value */;}
    break;

  case 104:
#line 1261 "./lyparser.y"
    { MALLOC_NODE(n, (yyvsp[(1) - (1)].i));(yyval.scm) = g_list_append (NULL, n);
			  /*FIXME copy value */ ;}
    break;

  case 105:
#line 1263 "./lyparser.y"
    { (yyval.scm) = (yyvsp[(1) - (1)].scm); ;}
    break;

  case 107:
#line 1270 "./lyparser.y"
    {
/* things like start cresc, simple_element end cresc */

	set_post_events ((DenemoObject *) ((yyvsp[(2) - (3)].scm)->data), u_str ((yyvsp[(2) - (3)].scm)), (yyvsp[(3) - (3)].scm));
			
	(yyval.scm) = (yyvsp[(2) - (3)].scm);/* FIXME memory leak */
	;}
    break;

  case 108:
#line 1277 "./lyparser.y"
    {
	(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 109:
#line 1280 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 110:
#line 1287 "./lyparser.y"
    {
	    GList *firstchord = br ((yyvsp[(1) - (3)].scm));
	    if (firstchord && firstchord->data)
		changedur ((DenemoObject *)(firstchord->data), (yyvsp[(2) - (3)].f).t1.a, (yyvsp[(2) - (3)].f).t1.b);
	    set_post_events ((DenemoObject *) ((yyvsp[(1) - (3)].scm)->data), u_str ((yyvsp[(1) - (3)].scm)), (yyvsp[(3) - (3)].scm));
	    (yyval.scm) = (yyvsp[(1) - (3)].scm);
	;}
    break;

  case 113:
#line 1305 "./lyparser.y"
    {
	    nodegeneric*n1 = (nodegeneric*)g_malloc0(sizeof(nodegeneric));
	    n1->user_string = "<";	
	    ((nodeglist*)n1)->post_user_string = ">";
	    ((nodeglist*)n1)->type = SIMULTANEOUS;
	    ((nodeglist*)n1)->branch = (yyvsp[(2) - (3)].scm);
	    (yyval.scm) = g_list_prepend(NULL, n1);
	;}
    break;

  case 114:
#line 1316 "./lyparser.y"
    { (yyval.scm) = NULL; ;}
    break;

  case 115:
#line 1317 "./lyparser.y"
    {
		(yyval.scm) = g_list_concat ((yyvsp[(1) - (2)].scm), (yyvsp[(2) - (2)].scm));
	;}
    break;

  case 116:
#line 1323 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
                (yyval.scm) = scm_list_2 ((yyvsp[(1) - (1)].generic), make_input ((yyloc)));
#endif
        ;}
    break;

  case 117:
#line 1333 "./lyparser.y"
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

  case 118:
#line 1362 "./lyparser.y"
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

  case 119:
#line 1377 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
        ;}
    break;

  case 120:
#line 1383 "./lyparser.y"
    {
	(yyval.scm) = (yyvsp[(1) - (1)].scm);
;}
    break;

  case 121:
#line 1386 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		Span_req *l = new Span_req;
		l->set_span_dir (START);
		l->set_mus_property ("span-type", scm_makfrom0str ("ligature"));
		l->set_spot (THIS->here_input ());

		(yyval.scm) = new Request_chord (SCM_EOL);
		(yyval.scm)->set_mus_property ("elements", gh_cons (l, SCM_EOL));
  	  scm_gc_unprotect_object (l->self_scm ());
		(yyval.scm)->set_spot (THIS->here_input ());
#endif
	;}
    break;

  case 122:
#line 1401 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		Span_req *l = new Span_req;
		l->set_span_dir (STOP);
		l->set_mus_property ("span-type", scm_makfrom0str ("ligature"));
		l->set_spot (THIS->here_input ());

		(yyval.scm) = new Request_chord (SCM_EOL);
		(yyval.scm)->set_mus_property ("elements", gh_cons (l, SCM_EOL));
		(yyval.scm)->set_spot (THIS->here_input ());
	  scm_gc_unprotect_object (l->self_scm ());
#endif
	;}
    break;

  case 123:
#line 1416 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		(yyval.scm) = new Music (gh_list (gh_cons (ly_symbol2scm ("name"), ly_symbol2scm ("separator")), SCM_UNDEFINED));
		(yyval.scm)->set_spot (THIS->here_input ());
#endif
	;}
    break;

  case 124:
#line 1424 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (1)].generic));
	(yyval.scm) = g_list_append(NULL, n); /* this node used to be used by denemo to split the glist into measures */
	;}
    break;

  case 125:
#line 1428 "./lyparser.y"
    {
			MALLOC_NODE(n, (yyvsp[(1) - (2)].generic));

		  n->user_string = g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].gstr).user_string, NULL);/* FIXME memory leaks */
		  (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 126:
#line 1434 "./lyparser.y"
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

  case 127:
#line 1443 "./lyparser.y"
    {
  		DenemoObject *mud = dnm_newclefobj (cleftypefromname((yyvsp[(2) - (2)].gstr).gstr->str));
		mud->user_string = g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].gstr).user_string,NULL);
		/* FIXME memory leaks on strings concatenated */
		(yyval.scm) = g_list_append(NULL,mud);
	;}
    break;

  case 128:
#line 1449 "./lyparser.y"
    {
  		DenemoObject *mud = dnm_newtimesigobj ((yyvsp[(2) - (2)].t).t.a, (yyvsp[(2) - (2)].t).t.b);
		mud->user_string = g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].t).user_string,NULL);
		/* FIXME memory leaks on strings concatenated */
		(yyval.scm) = g_list_append(NULL,mud);
	;}
    break;

  case 129:
#line 1455 "./lyparser.y"
    { /* ignore */
		(yyval.scm) = NULL;
	;}
    break;

  case 130:
#line 1461 "./lyparser.y"
    { (yyval.scm) = (yyvsp[(1) - (1)].scm); ;}
    break;

  case 131:
#line 1462 "./lyparser.y"
    { (yyval.scm) = (yyvsp[(1) - (1)].scm); ;}
    break;

  case 132:
#line 1466 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 133:
#line 1469 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 134:
#line 1472 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
	(yyval.scm) = g_list_append(NULL, n);/* FIXME denemo should know about this */
	;}
    break;

  case 135:
#line 1476 "./lyparser.y"
    {	
	MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
	(yyval.scm) = g_list_append(NULL, n);/* FIXME denemo should know about this */


	;}
    break;

  case 136:
#line 1482 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		(yyval.scm) = new Breathing_sign_req;
#endif
	;}
    break;

  case 137:
#line 1489 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		(yyval.scm) = new Porrectus_req;
#endif
	;}
    break;

  case 138:
#line 1499 "./lyparser.y"
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

  case 139:
#line 1510 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		Mark_req * m = new Mark_req;
		(yyval.scm) = m;
#endif
	;}
    break;

  case 140:
#line 1518 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		Mark_req *m = new Mark_req;
		m->set_mus_property ("label", (yyvsp[(2) - (2)].scm));
		(yyval.scm) = m;
#endif
	;}
    break;

  case 141:
#line 1526 "./lyparser.y"
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

  case 142:
#line 1541 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
//		$$ = $1;
	;}
    break;

  case 143:
#line 1545 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		Key_change_req *key= new Key_change_req;
		(yyval.scm) = key;
#endif
	;}
    break;

  case 144:
#line 1553 "./lyparser.y"
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

  case 145:
#line 1567 "./lyparser.y"
    {
	(yyval.scm) = NULL;
	;}
    break;

  case 146:
#line 1570 "./lyparser.y"
    {
		if((yyvsp[(1) - (2)].scm)) {
			(yyval.scm) = g_list_concat((yyvsp[(1) - (2)].scm), (yyvsp[(2) - (2)].scm));
			}
		else 
			(yyval.scm) = (yyvsp[(2) - (2)].scm);
	;}
    break;

  case 147:
#line 1580 "./lyparser.y"
    {
                (yyval.scm) = (yyvsp[(1) - (1)].scm);
        ;}
    break;

  case 148:
#line 1583 "./lyparser.y"
    {
	   /* script_dir is an integer saying whether up down or centred
	      denemo doesn't understand this yet */
	    u_str((yyvsp[(2) - (2)].scm)) = g_strconcat ( (yyvsp[(1) - (2)].i).user_string, u_str((yyvsp[(2) - (2)].scm)), NULL);
	    (yyval.scm) = (yyvsp[(2) - (2)].scm);
        ;}
    break;

  case 149:
#line 1589 "./lyparser.y"
    {
	   /* script_dir is an integer saying whether up down or centred
	      denemo doesn't understand this yet */
	    u_str((yyvsp[(2) - (2)].scm)) = g_strconcat ( (yyvsp[(1) - (2)].i).user_string, u_str((yyvsp[(2) - (2)].scm)), NULL);
	    (yyval.scm) = (yyvsp[(2) - (2)].scm);
        ;}
    break;

  case 150:
#line 1599 "./lyparser.y"
    {
                (yyval.scm) = (yyvsp[(1) - (1)].scm);
        ;}
    break;

  case 151:
#line 1602 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
        ;}
    break;

  case 152:
#line 1608 "./lyparser.y"
    {  ;}
    break;

  case 153:
#line 1609 "./lyparser.y"
    { (yyval.i).i = 0; ;}
    break;

  case 154:
#line 1610 "./lyparser.y"
    { (yyval.i) = (yyvsp[(2) - (2)].i); ;}
    break;

  case 155:
#line 1611 "./lyparser.y"
    { (yyval.i) = (yyvsp[(2) - (2)].i); ;}
    break;

  case 156:
#line 1616 "./lyparser.y"
    {
		(yyvsp[(1) - (1)].i).i = 1;
		(yyval.i) = (yyvsp[(1) - (1)].i);
	;}
    break;

  case 157:
#line 1620 "./lyparser.y"
    {
		(yyvsp[(1) - (2)].i).i ++;
		(yyvsp[(1) - (2)].i).user_string = g_strconcat((yyvsp[(1) - (2)].i).user_string,(yyvsp[(2) - (2)].i).user_string, NULL);/*FIXME memory leak */
		(yyval.i) = (yyvsp[(1) - (2)].i);
	;}
    break;

  case 158:
#line 1628 "./lyparser.y"
    {
		(yyvsp[(1) - (1)].i).i = 1;
		(yyval.i) = (yyvsp[(1) - (1)].i);	
	;}
    break;

  case 159:
#line 1632 "./lyparser.y"
    {
		(yyvsp[(1) - (2)].i).i ++ ;
		(yyvsp[(1) - (2)].i).user_string = g_strconcat((yyvsp[(1) - (2)].i).user_string,(yyvsp[(2) - (2)].i).user_string, NULL);/*FIXME memory leak */
		(yyval.i) = (yyvsp[(1) - (2)].i);
	;}
    break;

  case 160:
#line 1640 "./lyparser.y"
    {
		char notename = 'a' + (yyvsp[(1) - (1)].t).t.a;
		int enshift = (yyvsp[(1) - (1)].t).t.b;
		(yyval.n).user_string = (yyvsp[(1) - (1)].t).user_string;
		(yyval.n).n.enshift = enshift;
		(yyval.n).n.mid_c_offset = pitchtomid_c_offset (notename, 0);		
	;}
    break;

  case 161:
#line 1647 "./lyparser.y"
    {
		char notename = 'a' + (yyvsp[(1) - (2)].t).t.a;
		int enshift = (yyvsp[(1) - (2)].t).t.b;
		int sups=(yyvsp[(2) - (2)].i).i;
		(yyval.n).user_string = g_strconcat((yyvsp[(1) - (2)].t).user_string, (yyvsp[(2) - (2)].i).user_string, NULL);
		(yyval.n).n.enshift = enshift;
		(yyval.n).n.mid_c_offset = pitchtomid_c_offset (notename, sups);
	;}
    break;

  case 162:
#line 1655 "./lyparser.y"
    {
		char notename = 'a' + (yyvsp[(1) - (2)].t).t.a;
		int enshift = (yyvsp[(1) - (2)].t).t.b;
		int subs = -(yyvsp[(2) - (2)].i).i;
		(yyval.n).user_string = g_strconcat((yyvsp[(1) - (2)].t).user_string, (yyvsp[(2) - (2)].i).user_string, NULL);
		(yyval.n).n.enshift = enshift;
		(yyval.n).n.mid_c_offset = pitchtomid_c_offset (notename, subs);
	;}
    break;

  case 163:
#line 1670 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.n) = (yyvsp[(1) - (1)].t);
#endif
	;}
    break;

  case 164:
#line 1676 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		Pitch p = *unsmob_pitch ((yyvsp[(1) - (2)].t));
		p.octave_ +=  (yyvsp[(2) - (2)].i);
		(yyval.n) = p.smobbed_copy ();
#endif
	;}
    break;

  case 165:
#line 1684 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		Pitch p =* unsmob_pitch ((yyvsp[(1) - (2)].t));

		p.octave_ +=  -(yyvsp[(2) - (2)].i);
		(yyval.n) = p.smobbed_copy ();

#endif
	;}
    break;

  case 166:
#line 1697 "./lyparser.y"
    {
		(yyval.n) = (yyvsp[(1) - (1)].n);
	;}
    break;

  case 167:
#line 1700 "./lyparser.y"
    {
		(yyval.n) = (yyvsp[(1) - (1)].n);
	;}
    break;

  case 168:
#line 1706 "./lyparser.y"
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

  case 169:
#line 1719 "./lyparser.y"
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

  case 170:
#line 1733 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		if (!THIS->lexer_->lyric_state_b ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));
		(yyval.scm) = new Extender_req;
#endif
	;}
    break;

  case 171:
#line 1744 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		if (!THIS->lexer_->lyric_state_b ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));
		(yyval.scm) = new Hyphen_req;
#endif
	;}
    break;

  case 172:
#line 1755 "./lyparser.y"
    {
              (yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 173:
#line 1758 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].gstr));
	    ((nodegstr*)n)->gstr = (yyvsp[(1) - (1)].gstr).gstr;
	    (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 174:
#line 1763 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].gstr));
	    ((nodegstr*)n)->gstr = (yyvsp[(1) - (1)].gstr).gstr;
	    (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 175:
#line 1768 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
	;}
    break;

  case 176:
#line 1774 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 177:
#line 1778 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 178:
#line 1782 "./lyparser.y"
    {	/* tie */
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 179:
#line 1786 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 180:
#line 1790 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 181:
#line 1794 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 182:
#line 1798 "./lyparser.y"
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

  case 183:
#line 1810 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
	;}
    break;

  case 184:
#line 1815 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
                Music *t = MY_MAKE_MUSIC ("TextScriptEvent");
                t->set_property ("text", (yyvsp[(1) - (1)].scm));
                t->set_spot ((yyloc));
                (yyval.scm) = t;
#endif
        ;}
    break;

  case 185:
#line 1831 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].gstr));
	    ((nodegstr*)n)->gstr = (yyvsp[(1) - (1)].gstr).gstr;
	    (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 186:
#line 1836 "./lyparser.y"
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

  case 187:
#line 1851 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Hat");
#endif
	;}
    break;

  case 188:
#line 1857 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Plus");
#endif
	;}
    break;

  case 189:
#line 1863 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Dash");
#endif
	;}
    break;

  case 190:
#line 1869 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Bar");
#endif
	;}
    break;

  case 191:
#line 1875 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Larger");
#endif
	;}
    break;

  case 192:
#line 1881 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Dot");
#endif
	;}
    break;

  case 193:
#line 1887 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Underscore");
#endif
	;}
    break;

  case 194:
#line 1896 "./lyparser.y"
    { /* $$ = DOWN; */ ;}
    break;

  case 195:
#line 1897 "./lyparser.y"
    {  /* $$ = UP; */ ;}
    break;

  case 196:
#line 1898 "./lyparser.y"
    {  /* $$ = CENTER; */ ;}
    break;

  case 197:
#line 1903 "./lyparser.y"
    {
		(yyval.n) = (yyvsp[(1) - (1)].n);
	;}
    break;

  case 198:
#line 1909 "./lyparser.y"
    {
	(yyval.f) = (yyvsp[(1) - (1)].f);
	;}
    break;

  case 199:
#line 1912 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.f) = (yyvsp[(1) - (1)].f);
#endif
	;}
    break;

  case 200:
#line 1921 "./lyparser.y"
    {
	(yyval.f) = default_duration_;
	;}
    break;

  case 201:
#line 1924 "./lyparser.y"
    {
		(yyval.f) = (yyvsp[(1) - (1)].f);
		 default_duration_.t1.a = (yyvsp[(1) - (1)].f).t1.a;
		 default_duration_.t1.b = (yyvsp[(1) - (1)].f).t1.b;
	;}
    break;

  case 202:
#line 1929 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.f) = (yyvsp[(1) - (1)].f);
		THIS->default_duration_ = *unsmob_duration ((yyval.f));
#endif
	;}
    break;

  case 203:
#line 1939 "./lyparser.y"
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

  case 204:
#line 1950 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		Duration *d =unsmob_duration ((yyvsp[(1) - (2)].id));
		Duration k (d->duration_log (),d->dot_count () + (yyvsp[(2) - (2)].i));
		(yyval.f) = k.smobbed_copy ();
#endif
	;}
    break;

  case 205:
#line 1964 "./lyparser.y"
    { /* note 4 integers are used for these */
		(yyvsp[(1) - (1)].f).t2.a = 1;
		(yyvsp[(1) - (1)].f).t2.b = 1;
		(yyval.f) = (yyvsp[(1) - (1)].f);
	;}
    break;

  case 206:
#line 1969 "./lyparser.y"
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

  case 207:
#line 1979 "./lyparser.y"
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

  case 208:
#line 1995 "./lyparser.y"
    { (yyval.t) = (yyvsp[(1) - (1)].t); ;}
    break;

  case 209:
#line 1996 "./lyparser.y"
    {

		(yyval.t).user_string =  g_strconcat((yyvsp[(1) - (3)].i).user_string, (yyvsp[(2) - (3)].generic).user_string, (yyvsp[(3) - (3)].i).user_string, NULL);
		(yyval.t).t.a = (yyvsp[(1) - (3)].i).i;
		(yyval.t).t.b = (yyvsp[(3) - (3)].i).i;
	;}
    break;

  case 210:
#line 2005 "./lyparser.y"
    { (yyval.i).i = 0; ;}
    break;

  case 211:
#line 2006 "./lyparser.y"
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

  case 212:
#line 2019 "./lyparser.y"
    {
		(yyval.i).i = 0;
	;}
    break;

  case 213:
#line 2022 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		if (!is_duration_b ((yyvsp[(2) - (2)].i)))
			THIS->parser_error (_f ("not a duration: %d", (yyvsp[(2) - (2)].i)));
		(yyval.i) = (yyvsp[(2) - (2)].i);
#endif
	;}
    break;

  case 214:
#line 2089 "./lyparser.y"
    { (yyval.i).i = 0; ;}
    break;

  case 215:
#line 2090 "./lyparser.y"
    { (yyval.i).i = 1; ;}
    break;

  case 216:
#line 2096 "./lyparser.y"
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

  case 217:
#line 2138 "./lyparser.y"
    {
	/* denemo wants a chord with no notes */
	DenemoObject *mud = newchord( (yyvsp[(2) - (2)].f).t1.a, (yyvsp[(2) - (2)].f).t1.b,0);

	mud->user_string = g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].f).user_string, NULL);
	(yyval.scm) = g_list_append(NULL,mud); 
	;}
    break;

  case 218:
#line 2145 "./lyparser.y"
    {
	/* denemo wants a chord with no notes */
	DenemoObject *mud = newchord( (yyvsp[(2) - (2)].f).t1.a, (yyvsp[(2) - (2)].f).t1.b,0);
	mud->type = SKIPNAME;
	mud->user_string = g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].f).user_string, NULL);
	(yyval.scm) = g_list_append(NULL,mud); 
	;}
    break;

  case 219:
#line 2152 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		Input i = THIS->pop_spot ();

		Skip_req * sk = new Skip_req;
		sk->set_mus_property ("duration", (yyvsp[(2) - (2)].f));
		Span_req *sp1 = new Span_req;
		Span_req *sp2 = new Span_req;
		sp1-> set_span_dir ( START);
		sp2-> set_span_dir ( STOP);
		SCM r = scm_makfrom0str ("rest");
		sp1->set_mus_property ("span-type", r);
		sp2->set_mus_property ("span-type", r);

		Request_chord * rqc1 = new Request_chord (SCM_EOL);
		rqc1->set_mus_property ("elements", scm_list_n (sp1, SCM_UNDEFINED));
		Request_chord * rqc2 = new Request_chord (SCM_EOL);
		rqc2->set_mus_property ("elements", scm_list_n (sk, SCM_UNDEFINED));
		Request_chord * rqc3 = new Request_chord (SCM_EOL);
		rqc3->set_mus_property ("elements", scm_list_n (sp2, SCM_UNDEFINED));

		SCM ms = scm_list_n (rqc1, rqc2, rqc3, SCM_UNDEFINED);

		(yyval.scm) = new Sequential_music (SCM_EOL);
		(yyval.scm)->set_mus_property ("elements", ms);
#endif
	;}
    break;

  case 220:
#line 2180 "./lyparser.y"
    {
		DenemoObject *mud = newlyric((yyvsp[(2) - (2)].f).t1.a, (yyvsp[(2) - (2)].f).t1.b, (yyvsp[(1) - (2)].gstr).gstr->str);
		mud->user_string = (yyvsp[(1) - (2)].gstr).user_string;
	        mud->type = LYRICS;
		stradd((*mud),(yyvsp[(2) - (2)].f));
		(yyval.scm) = g_list_append(NULL,mud);
	;}
    break;

  case 221:
#line 2187 "./lyparser.y"
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

  case 224:
#line 2207 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
                (yyval.music) = Chord::get_chord ((yyvsp[(1) - (6)].n), (yyvsp[(3) - (6)].scm), (yyvsp[(4) - (6)].scm), (yyvsp[(5) - (6)].scm), (yyvsp[(6) - (6)].scm), (yyvsp[(2) - (6)].f));
		(yyval.music)->set_spot (THIS->here_input ());
#endif
        ;}
    break;

  case 225:
#line 2216 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = SCM_EOL;
#endif
	;}
    break;

  case 226:
#line 2222 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(2) - (2)].scm);
#endif
	;}
    break;

  case 227:
#line 2231 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 228:
#line 2234 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_append2 ((yyval.scm), (yyvsp[(3) - (3)].scm));
#endif
	;}
    break;

  case 229:
#line 2243 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = SCM_EOL;
#endif
	;}
    break;

  case 230:
#line 2249 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(2) - (2)].scm);
	;}
    break;

  case 231:
#line 2256 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = SCM_EOL;
#endif
	;}
    break;

  case 232:
#line 2262 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(2) - (2)].n);
#endif
	;}
    break;

  case 233:
#line 2271 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = SCM_EOL;
#endif
	;}
    break;

  case 234:
#line 2277 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(2) - (2)].n);
#endif
	;}
    break;

  case 235:
#line 2286 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_cons ((yyvsp[(1) - (1)].scm), SCM_EOL);
#endif
	;}
    break;

  case 236:
#line 2292 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_cons (unsmob_pitch ((yyvsp[(1) - (1)].t))->smobbed_copy (), SCM_EOL);
#endif
	;}
    break;

  case 237:
#line 2298 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
	 /* Ugh. */
		(yyval.scm) = scm_list_n (unsmob_pitch ((yyvsp[(1) - (2)].t))->smobbed_copy (),
			(yyvsp[(2) - (2)].scm), SCM_UNDEFINED);
#endif
	;}
    break;

  case 238:
#line 2309 "./lyparser.y"
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

  case 239:
#line 2320 "./lyparser.y"
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

  case 240:
#line 2332 "./lyparser.y"
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

  case 241:
#line 2349 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_sum ((yyvsp[(1) - (3)].scm), (yyvsp[(3) - (3)].scm));
#endif
	;}
    break;

  case 242:
#line 2355 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_difference ((yyvsp[(1) - (3)].scm), (yyvsp[(3) - (3)].scm));
#endif
	;}
    break;

  case 244:
#line 2365 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
#endif
	;}
    break;

  case 245:
#line 2371 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_product ((yyvsp[(1) - (3)].scm), (yyvsp[(3) - (3)].scm));
#endif
	;}
    break;

  case 246:
#line 2377 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_divide ((yyvsp[(1) - (3)].scm), (yyvsp[(3) - (3)].scm));
#endif
	;}
    break;

  case 247:
#line 2386 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(2) - (3)].scm);
	;}
    break;

  case 248:
#line 2389 "./lyparser.y"
    { /* %prec UNARY_MINUS */
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_difference ((yyvsp[(2) - (2)].scm), SCM_UNDEFINED);
#endif
	;}
    break;

  case 250:
#line 2400 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_int2scm ((yyvsp[(1) - (1)].i));
#endif
	;}
    break;

  case 251:
#line 2406 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(1) - (1)].r);
#endif
	;}
    break;

  case 252:
#line 2412 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(1) - (1)].id);
#endif
	;}
    break;

  case 253:
#line 2418 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_double2scm (gh_scm2double ((yyvsp[(1) - (2)].r)) CM );
#endif
	;}
    break;

  case 254:
#line 2424 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_double2scm (gh_scm2double ((yyvsp[(1) - (2)].r)) PT);
#endif
	;}
    break;

  case 255:
#line 2430 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_double2scm (gh_scm2double ((yyvsp[(1) - (2)].r)) INCH);
#endif
	;}
    break;

  case 256:
#line 2436 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_double2scm (gh_scm2double ((yyvsp[(1) - (2)].r)) MM);
#endif
	;}
    break;

  case 257:
#line 2442 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_double2scm (gh_scm2double ((yyvsp[(1) - (2)].r)) CHAR);
#endif
	;}
    break;

  case 258:
#line 2452 "./lyparser.y"
    {
			(yyval.i) = (yyvsp[(1) - (1)].i);
	;}
    break;

  case 259:
#line 2455 "./lyparser.y"
    {
		(yyval.i) = (yyvsp[(1) - (1)].i);
	;}
    break;

  case 260:
#line 2461 "./lyparser.y"
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

  case 261:
#line 2475 "./lyparser.y"
    {
		(yyval.i).i = -(yyvsp[(2) - (2)].i).i;
	;}
    break;

  case 262:
#line 2482 "./lyparser.y"
    {
		(yyval.gstr) = (yyvsp[(1) - (1)].gstr);
	;}
    break;

  case 263:
#line 2485 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.gstr) = (yyvsp[(1) - (1)].id);
#endif
	;}
    break;

  case 264:
#line 2491 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.gstr) = scm_string_append (scm_list_n ((yyvsp[(1) - (3)].gstr), (yyvsp[(3) - (3)].gstr), SCM_UNDEFINED));
#endif
	;}
    break;

  case 265:
#line 2501 "./lyparser.y"
    { (yyval.i).i = 0; ;}
    break;

  case 266:
#line 2502 "./lyparser.y"
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

  case 267:
#line 2515 "./lyparser.y"
    { (yyval.i).i = 0; ;}
    break;

  case 268:
#line 2516 "./lyparser.y"
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
#line 5211 "lyparser.tab.c"
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


#line 2529 "./lyparser.y"


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
  g_assert (h == NULL);
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
static void
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
  set_initial_staffcontexts (si);
  find_leftmost_allcontexts (si);
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
    }
    for(scm = findtok (lily_file, SCM_T); scm ; scm = scm->next)
    {
	if ((pt = strstr (u_str (scm), "set-global-staff-size")))
	{
		int font;
		if (sscanf (pt+21, " %d", &font) == 1)
		{
			gui->lilycontrol.fontsize = font;
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
	  /* free_score (si); CHANGE THIS TO FREE ALL THE si in 
	     si->thefile->currentDenemoScore list... */
	  

	  /* in case we are re-entering via reload after error */
	  lyrestart (lyin);	

	  lylineno = 1;		/* not done by lexer for some reason */
	  push_note_state ();

	  while (!feof (lyin))
	    {
	      lyparse ();
 if (parser_error_message) { 
	warningdialog("Unable to cope with this file");
	return -1;
}
	    }
	  if (parser_error_message == NULL)
	    {
	      GList *score = findtok (lily_file, SCORE);
	      if (score)
		if (create_score_from_lily (si, br (score)) == 0)
		  {
		    GList *top;
		    nodemin *n = (nodemin *) g_malloc0 (sizeof (nodemin));
		    score_prop_from_lily(gui);

		    while (score && score->next)
		      {
			score = findtok (score->next, SCORE);
			if (score)
			  {
			    DenemoScore *nextsi =
			      (DenemoScore *) g_malloc0 (sizeof (DenemoScore));
			    init_score (nextsi, gui);
			    create_score_from_lily (nextsi, br (score));
			    score_block_list =
			      g_list_append (score_block_list, nextsi);
			  }
		      }

		    n->type = TEXT;
		    /* must be g_free-able pointer */
		    n->user_string = g_strdup ("");	
		    top = g_list_append (NULL, n);
		    /* simplify the tree into TEXT and DENEMO_MEASURES nodes */
#ifdef LILYEDIT
		    lily_write_out (top, lily_file, TO_NODE);	
#endif
		    attach_trailing_white_space (top);
		    si->lily_file = top;
		    fixup_measure_widths_and_contexts (si);
#ifdef LILYEDIT
		    /* write out the text to editable display */
		    create_text_display (gui);	
#if GTK_MAJOR_VERSION > 1
/* so that the following toggle starts the scorearea display */
		    gtk_widget_hide (si->scorearea);	
		    toggle_top_node (NULL, gui);
/* present denemo's graphical window first */
		    gtk_window_present ((GtkWindow *) gui->window);	
#endif

		    if (g_list_length (score_block_list) > 0)
		      {
			GList *g;
			DenemoScore *nextsi;
			for (g = score_block_list; g; g = g->next)
			  {
			    nextsi = (DenemoScore *) g->data;
			    fixup_measure_widths_and_contexts (nextsi);
			    nextsi->window = si->window;
			    nextsi->scorearea = si->scorearea;
			    nextsi->pixmap = si->pixmap;
			    nextsi->vadjustment = si->vadjustment;
			    nextsi->vscrollbar = si->vscrollbar;
			    nextsi->hadjustment = si->hadjustment;
			    nextsi->hscrollbar = si->hscrollbar;
			    nextsi->menubar = si->menubar;
			    nextsi->textbuffer = si->textbuffer;
			    nextsi->textwindow = si->textwindow;
			    nextsi->textview = si->textview;
			    nextsi->musicdatabutton = si->musicdatabutton;
			    nextsi->sigid = si->sigid;
			    nextsi->curlilynode = si->curlilynode;
			    nextsi->lily_file = si->lily_file;
			    nextsi->prefs = si->prefs;
/* certain fields are not really score specific - they are file specific - share these */

			   
/* but we have no way of sharing has_changed yet FIXmME */

			  }

			/* append a copy of the score block si (the one that the display is hardwired to)
			   to the list of score_blocks, so that the display can be cycled around by copying
			   from score_block_list to si  */
			nextsi = (DenemoScore *) g_malloc0 (sizeof (DenemoScore));
			memcpy (nextsi, si, sizeof (DenemoScore));
			score_block_list =
			  g_list_append (score_block_list, nextsi);
			si->scoreblocks = score_block_list;
		      }

#endif // LILYEDIT


		    return 0;
		  }
	    }			/* if successful lily parse */
	    else
	    {
		fprintf(stderr, "Parse Error Line %d: %s\n", 
		    parser_error_linenum, parser_error_message);
		return(1);
	    }

	}
      reset_initial_lexer_state ();
      g_assert (lily_file);
#ifdef LILYEDIT
      if (!getenv ("EDITOR"))
	{
	  long len;
	  gchar *filecontents;
	  fseek (lyin, 0L, SEEK_END);
	  len = ftell (lyin);
	  filecontents = (gchar *) g_malloc (len + 1);	/* +1 for NULL terminator */
	  rewind (lyin);
	  fread (filecontents, len, 1, lyin);
	  *(filecontents + len) = '\0';
	  fclose (lyin);
	  u_str (lily_file) = filecontents;
	  lily_file->next = NULL;
	  si->lily_file = lily_file;
	  si->curlilynode = lily_file;
	  if (si->textwindow)
	    lily_text_change (si);
	  else
	    create_text_display (si);
	  gui->filename = g_string_new (filename);
	  gtk_widget_set_sensitive (si->textview, TRUE);
	  gtk_widget_set_sensitive (si->scorearea, FALSE);

	  /* put dialog on top - it would be nice to use gtk_dialog_run() but  how do you use it? */
	  if (parser_error_dialog)
	    gtk_window_present ((GtkWindow *) parser_error_dialog);
	  return -1;
	}
      else
	{			/* use external editor */
	  GString *cmd;
	  int ret;
	  fclose (lyin);
	  cmd = g_string_new (getenv ("EDITOR"));
	  g_string_append_printf (cmd, " +%d %s", parser_error_linenum,
				  filename);
	  g_warning
	    ("Using environment variable $EDITOR calling system with %s\n",
	     cmd->str);
	  ret = system (cmd->str);
#if 0
	  if (WIFIGNALED (ret) &&
	      (WTERMSIG (ret) == SIGINT || WTERMSIG (ret) == SIGQUIT))
	    break;
#endif
	  if (ret)
	    break;
	}
#endif //LILYEDIT
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

