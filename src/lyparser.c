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
#define YYLAST   1602

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  158
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  99
/* YYNRULES -- Number of rules.  */
#define YYNRULES  270
/* YYNRULES -- Number of states.  */
#define YYNSTATES  411

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
     194,   200,   204,   208,   210,   212,   214,   219,   223,   227,
     228,   232,   233,   237,   238,   242,   243,   247,   248,   252,
     254,   256,   258,   262,   266,   270,   275,   280,   287,   293,
     300,   307,   314,   320,   324,   331,   333,   335,   337,   338,
     342,   344,   346,   350,   352,   354,   358,   359,   362,   364,
     370,   373,   375,   377,   379,   381,   383,   385,   388,   391,
     394,   397,   399,   401,   403,   405,   407,   409,   411,   413,
     415,   419,   422,   425,   428,   430,   433,   437,   438,   441,
     443,   446,   449,   451,   453,   454,   456,   459,   462,   464,
     467,   469,   472,   474,   477,   480,   482,   485,   488,   490,
     492,   495,   498,   500,   502,   504,   506,   508,   510,   512,
     514,   516,   518,   520,   522,   524,   526,   528,   530,   532,
     534,   536,   538,   540,   542,   544,   546,   548,   550,   552,
     554,   556,   558,   559,   561,   563,   566,   569,   571,   575,
     579,   581,   585,   586,   589,   591,   594,   595,   597,   604,
     607,   610,   613,   616,   618,   620,   622,   629,   630,   633,
     635,   639,   640,   643,   644,   647,   648,   651,   653,   655,
     658,   660,   663,   666,   670,   674,   676,   678,   682,   686,
     690,   693,   695,   697,   699,   701,   704,   707,   710,   713,
     716,   718,   720,   722,   725,   727,   729,   733,   734,   737,
     738
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
      91,   148,   178,    -1,    82,   233,   178,    -1,   180,    -1,
     182,    -1,   181,    -1,    84,   217,   217,   178,    -1,    84,
     216,   178,    -1,    16,   161,   178,    -1,    -1,    59,   185,
     178,    -1,    -1,    37,   186,   178,    -1,    -1,    25,   187,
     178,    -1,    -1,    49,   188,   178,    -1,    -1,    90,   189,
     178,    -1,   190,    -1,   191,    -1,   192,    -1,    68,   228,
     178,    -1,    71,   178,   178,    -1,    89,   148,   178,    -1,
      72,   148,   178,   178,    -1,    83,   148,   113,   148,    -1,
      63,   148,   126,   148,   113,   195,    -1,    63,   148,   126,
     148,    86,    -1,    65,   148,   126,   148,   113,   161,    -1,
      65,   148,   126,   148,   113,   148,    -1,    64,   148,   126,
     148,   113,   161,    -1,    64,   148,   161,   113,   161,    -1,
      66,   148,   161,    -1,    63,   148,   126,   148,    66,   161,
      -1,   254,    -1,   253,    -1,   161,    -1,    -1,   196,   237,
     209,    -1,   205,    -1,   198,    -1,   201,   230,   209,    -1,
     115,    -1,   117,    -1,   199,   202,   200,    -1,    -1,   202,
     204,    -1,    94,    -1,   217,   255,   256,   212,   209,    -1,
      93,   209,    -1,   203,    -1,   206,    -1,   104,    -1,   105,
      -1,   107,    -1,   118,    -1,    21,   148,    -1,    61,   229,
      -1,    27,   148,    -1,    81,   233,    -1,    22,    -1,   207,
      -1,   208,    -1,   220,    -1,   221,    -1,   124,    -1,   125,
      -1,    23,    -1,   106,    -1,    78,   253,   148,    -1,    50,
      56,    -1,    50,   195,    -1,    75,   229,    -1,   176,    -1,
      48,    56,    -1,    48,   132,    11,    -1,    -1,   209,   210,
      -1,   222,    -1,   227,   211,    -1,   227,   222,    -1,   225,
      -1,   226,    -1,    -1,   113,    -1,   113,   214,    -1,   113,
     213,    -1,   129,    -1,   213,   129,    -1,   130,    -1,   214,
     130,    -1,   132,    -1,   132,   213,    -1,   132,   214,    -1,
     133,    -1,   133,   213,    -1,   133,   214,    -1,   215,    -1,
     218,    -1,    55,   161,    -1,    35,   161,    -1,    36,    -1,
      45,    -1,   223,    -1,    13,    -1,    12,    -1,   235,    -1,
     121,    -1,   122,    -1,   123,    -1,   100,    -1,   101,    -1,
      99,    -1,   102,    -1,    51,    -1,   224,    -1,   254,    -1,
     131,    -1,   155,    -1,   153,    -1,   152,    -1,   118,    -1,
     117,    -1,   126,    -1,   156,    -1,   156,    -1,   155,    -1,
     152,    -1,   215,    -1,   232,    -1,   219,    -1,    -1,   232,
      -1,   219,    -1,   252,   234,    -1,   135,   234,    -1,   231,
      -1,   232,   120,   252,    -1,   232,   120,   136,    -1,   136,
      -1,   150,   119,   150,    -1,    -1,   234,   126,    -1,   157,
      -1,   157,   252,    -1,    -1,    95,    -1,   217,   255,   256,
     212,   230,   236,    -1,   146,   230,    -1,   147,   230,    -1,
      52,   230,    -1,   148,   230,    -1,   240,    -1,    97,    -1,
      96,    -1,   216,   230,   241,   243,   244,   245,    -1,    -1,
     109,   242,    -1,   246,    -1,   242,   126,   246,    -1,    -1,
     111,   242,    -1,    -1,   119,   216,    -1,    -1,   108,   216,
      -1,   247,    -1,   134,    -1,   134,   247,    -1,   252,    -1,
     252,   153,    -1,   252,   110,    -1,   248,   153,   249,    -1,
     248,   152,   249,    -1,   249,    -1,   250,    -1,   250,   120,
     250,    -1,   250,   119,   250,    -1,   121,   248,   122,    -1,
     152,   250,    -1,   251,    -1,   150,    -1,   151,    -1,   140,
      -1,   151,    28,    -1,   151,    67,    -1,   151,    46,    -1,
     151,    54,    -1,   151,    26,    -1,   150,    -1,   131,    -1,
     251,    -1,   152,   253,    -1,   148,    -1,   144,    -1,   254,
     153,   254,    -1,    -1,   255,   128,    -1,    -1,   256,   127,
      -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   455,   455,   456,   464,   471,   474,   480,   491,   494,
     497,   500,   503,   510,   514,   523,   529,   533,   539,   542,
     552,   561,   576,   611,   614,   617,   620,   623,   626,   630,
     633,   640,   647,   654,   662,   673,   677,   680,   685,   690,
     701,   711,   715,   719,   723,   729,   732,   738,   752,   755,
     771,   777,   778,   785,   788,   800,   822,   830,   842,   851,
     867,   868,   872,   876,   902,   912,   921,   928,   949,   965,
     976,   999,  1021,  1034,  1037,  1040,  1043,  1055,  1068,  1073,
    1072,  1082,  1081,  1090,  1089,  1105,  1104,  1114,  1113,  1121,
    1124,  1127,  1133,  1148,  1153,  1177,  1195,  1212,  1219,  1226,
    1232,  1239,  1246,  1253,  1260,  1271,  1273,  1275,  1278,  1282,
    1289,  1292,  1298,  1309,  1312,  1316,  1328,  1329,  1335,  1345,
    1374,  1389,  1395,  1398,  1413,  1428,  1436,  1440,  1448,  1457,
    1463,  1469,  1475,  1476,  1480,  1483,  1486,  1490,  1496,  1503,
    1513,  1524,  1532,  1540,  1555,  1559,  1567,  1581,  1584,  1594,
    1597,  1603,  1613,  1616,  1622,  1623,  1624,  1625,  1630,  1634,
    1642,  1646,  1654,  1661,  1669,  1684,  1690,  1698,  1711,  1714,
    1720,  1733,  1747,  1758,  1769,  1772,  1777,  1782,  1788,  1792,
    1796,  1800,  1804,  1808,  1812,  1824,  1829,  1845,  1850,  1865,
    1871,  1877,  1883,  1889,  1895,  1901,  1910,  1911,  1912,  1917,
    1923,  1926,  1935,  1938,  1943,  1953,  1964,  1978,  1983,  1993,
    2009,  2010,  2019,  2020,  2033,  2036,  2103,  2104,  2110,  2152,
    2159,  2166,  2174,  2181,  2193,  2196,  2201,  2210,  2216,  2225,
    2228,  2237,  2243,  2250,  2256,  2265,  2271,  2280,  2286,  2292,
    2303,  2314,  2326,  2343,  2349,  2355,  2359,  2365,  2371,  2380,
    2383,  2389,  2394,  2400,  2406,  2412,  2418,  2424,  2430,  2436,
    2446,  2449,  2455,  2469,  2476,  2479,  2485,  2495,  2496,  2509,
    2510
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
     184,   184,   184,   184,   184,   184,   184,   184,   184,   185,
     184,   186,   184,   187,   184,   188,   184,   189,   184,   184,
     184,   184,   190,   191,   191,   192,   193,   194,   194,   194,
     194,   194,   194,   194,   194,   195,   195,   195,   196,   197,
     197,   197,   198,   199,   200,   201,   202,   202,   203,   204,
     204,   204,   205,   205,   205,   205,   205,   205,   205,   205,
     205,   205,   206,   206,   207,   207,   207,   207,   207,   207,
     208,   208,   208,   208,   208,   208,   208,   209,   209,   210,
     210,   210,   211,   211,   212,   212,   212,   212,   213,   213,
     214,   214,   215,   215,   215,   216,   216,   216,   217,   217,
     218,   219,   220,   221,   222,   222,   222,   222,   223,   223,
     223,   223,   223,   223,   223,   224,   225,   225,   225,   226,
     226,   226,   226,   226,   226,   226,   227,   227,   227,   228,
     229,   229,   230,   230,   230,   231,   231,   232,   232,   232,
     233,   233,   234,   234,   235,   235,   236,   236,   237,   237,
     237,   237,   237,   237,   238,   239,   240,   241,   241,   242,
     242,   243,   243,   244,   244,   245,   245,   246,   246,   246,
     247,   247,   247,   248,   248,   248,   249,   249,   249,   250,
     250,   250,   251,   251,   251,   251,   251,   251,   251,   251,
     252,   252,   253,   253,   254,   254,   254,   255,   255,   256,
     256
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
       5,     3,     3,     1,     1,     1,     4,     3,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     1,
       1,     1,     3,     3,     3,     4,     4,     6,     5,     6,
       6,     6,     5,     3,     6,     1,     1,     1,     0,     3,
       1,     1,     3,     1,     1,     3,     0,     2,     1,     5,
       2,     1,     1,     1,     1,     1,     1,     2,     2,     2,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     2,     2,     2,     1,     2,     3,     0,     2,     1,
       2,     2,     1,     1,     0,     1,     2,     2,     1,     2,
       1,     2,     1,     2,     2,     1,     2,     2,     1,     1,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     1,     2,     2,     1,     3,     3,
       1,     3,     0,     2,     1,     2,     0,     1,     6,     2,
       2,     2,     2,     1,     1,     1,     6,     0,     2,     1,
       3,     0,     2,     0,     2,     0,     2,     1,     1,     2,
       1,     2,     2,     3,     3,     1,     1,     3,     3,     3,
       2,     1,     1,     1,     1,     2,     2,     2,     2,     2,
       1,     1,     1,     2,     1,     1,     3,     0,     2,     0,
       2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     1,     5,     0,     0,     6,     0,     0,     0,
       0,     0,     0,    14,     0,    13,     3,    12,     8,     7,
       9,     4,    10,    11,     0,    17,    15,    18,    45,    16,
      42,   108,    43,    21,   108,    44,    40,     0,     0,    41,
      46,    61,     0,     0,     0,   131,   138,    83,     0,     0,
       0,   172,    81,   108,   173,     0,    85,     0,    79,     0,
       0,     0,     0,     0,     0,     0,   108,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    87,     0,   224,
     123,   124,   139,   125,    48,   113,   126,   136,   137,    36,
      64,     0,   144,    35,    73,    75,    74,    51,    52,    89,
      90,    91,    66,    65,     0,    60,   111,   116,   202,   110,
     122,   132,   133,   134,   135,    48,   176,   175,     0,     0,
     183,   181,   182,   184,   178,   179,   180,   254,   265,   264,
     252,   253,   198,   197,   196,   214,    31,    22,    25,    23,
      24,    26,    27,    28,   149,   174,     0,   177,    29,   245,
     246,   251,    30,    20,    19,   261,   212,   260,     0,   212,
      62,   108,   108,   127,   108,   129,    48,    48,   108,    69,
     145,     0,   108,   141,     0,   107,   142,   262,   106,   105,
     108,   201,   128,   207,   200,     0,     0,     0,     0,   162,
     199,   108,     0,   108,   108,   143,     0,     0,   210,     0,
     130,   108,     0,     0,   165,   168,   108,     0,   169,   108,
       0,   108,   108,   108,     0,    39,    34,    37,    38,   202,
     202,   202,   202,   202,   267,   147,   223,     0,   204,   147,
     203,     0,   171,     0,     0,     0,     0,   259,   255,   257,
     258,   256,   250,   215,   185,   193,   192,   178,   194,   188,
     191,   190,   189,   195,   150,   151,   186,   152,   153,   187,
       0,     0,     0,     0,     0,   206,     0,   205,    68,    78,
      84,     0,     0,    82,   146,    86,   263,    80,     0,     0,
       0,     0,     0,   103,   158,   160,   163,   164,    92,   108,
      93,   108,   140,     0,     0,    72,     0,   170,   166,   167,
      77,   108,    67,     0,    94,    88,    71,    50,    57,    49,
     221,   219,   220,   222,   227,   269,   109,   147,   118,   114,
     115,   121,   117,   267,   112,   225,    59,    33,     0,   249,
     244,   243,   248,   247,   266,   213,    47,    56,    58,   209,
     208,     0,     0,     0,     0,   159,   161,    53,    95,     0,
     211,    96,    76,   108,     0,   231,   268,   154,   198,   148,
     120,   269,    32,     0,    98,     0,     0,   102,     0,     0,
      55,    63,    70,   238,   228,   229,   237,   240,     0,   233,
     155,   270,   202,   154,   104,    97,   101,   100,    99,    48,
     239,     0,   242,   241,   232,     0,   235,   157,   156,   216,
     147,     0,   230,   234,     0,   226,   217,   218,   119,    54,
     236
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,    16,    25,    18,    19,    26,    37,    20,    21,
     137,   138,   328,    22,    91,    23,    24,    39,    92,   214,
     309,   370,    94,    95,    96,    97,    98,   180,   168,   164,
     172,   212,    99,   100,   101,   102,   103,   176,   104,   105,
     106,   107,   320,   108,   227,   321,   322,   109,   110,   111,
     112,   316,   359,   254,   382,   286,   287,   205,   206,   207,
     208,   228,   113,   114,   144,   145,   256,   257,   258,   146,
     191,   182,   229,   183,   230,   200,   265,   147,   407,   225,
     115,   326,   226,   355,   374,   379,   396,   405,   375,   376,
     236,   149,   150,   151,   159,   178,   179,   315,   357
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -223
static const yytype_int16 yypact[] =
{
    -223,    24,  -223,  -223,  -102,   -76,  -223,     1,  -102,     9,
      11,    25,   -89,  -223,    49,  -223,  -223,  -223,  -223,  -223,
    -223,  -223,  -223,  -223,    14,  -223,  -223,  -223,    72,  -223,
    -223,  1342,  -223,  -223,   347,  -223,  -223,   -87,   -85,  -223,
    -223,    17,    22,  -102,    27,  -223,  -223,  -223,    29,    77,
      83,  -223,  -223,  1460,  -223,   -25,  -223,   118,  -223,   -22,
      50,    51,    52,    59,    80,   -61,  1460,    70,   -22,    16,
    -102,   -99,   -99,    73,   -33,    69,    74,  -223,    75,  -223,
    -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,
    -223,    10,  -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,
    -223,  -223,  -223,  -223,   149,  -223,  -223,  -223,   -22,  -223,
    -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,  -102,   -74,
    -223,  -223,  -223,  -223,     3,  -223,  -223,  -223,  -223,  -223,
    -223,    34,    -3,  -223,  -223,   -17,  -223,  -223,  -223,  -223,
    -223,  -223,  -223,  -223,  -223,  -223,    93,  -223,   -60,  -223,
      38,  -223,    67,  -223,  -223,  -223,  -223,  -223,   112,  -223,
    -223,  1460,  1460,  -223,  1460,  -223,  -223,  -223,  1460,  -223,
    -223,   215,  1460,  -223,    16,  -223,  -223,  -223,  -223,    67,
    1460,  -223,  -223,  -223,   107,   103,   -59,   104,  -102,    30,
    -223,  1460,    53,  1460,  1460,  -223,    88,  -102,  -223,   120,
    -223,  1460,   127,  -102,    30,  -223,  1460,   -28,  -223,  1128,
     -72,  1460,  1460,  1460,   499,  -223,  -223,  -223,  -223,   -22,
     -22,   -22,   -22,   -22,  -223,  -223,  -223,   -21,  -223,  -223,
     107,   628,  -223,    99,     3,     3,   -77,  -223,  -223,  -223,
    -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,
    -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,    67,
       3,     3,     3,     3,   -61,   105,   -17,   105,  -223,  -223,
    -223,   757,   886,  -223,  -223,  -223,  -223,  -223,    -9,    95,
      96,   138,   109,  -223,  -223,  -223,   124,   130,  -223,  1460,
    -223,  1460,  -223,   151,   122,  -223,   125,  -223,   124,   130,
    -223,  1460,  -223,   -61,  -223,  -223,  -223,  -223,  -223,  -223,
    -223,  -223,  -223,  -223,   165,   147,   133,  -223,  -223,  -223,
    -223,  -223,  -223,  -223,   133,  -223,  -223,  -223,   160,  -223,
    -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,
    -223,   -34,   166,  -102,   170,  -223,  -223,   258,  -223,  -102,
    -223,  -223,  -223,  1235,   -40,   173,  -223,   -24,  -223,  -223,
     133,   147,  -223,  -102,  -223,   164,  -102,  -223,    60,   172,
    -223,  -223,  -223,   -17,   161,  -223,  -223,   -75,   -40,   174,
      30,  -223,   -22,   -24,  -223,  -223,  -223,  -223,  -223,  -223,
    -223,   -40,  -223,  -223,   161,   158,   184,   124,   130,   203,
    -223,  1015,  -223,  -223,   158,  -223,  -223,  -223,   133,  -223,
    -223
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -223,  -223,  -223,    -1,  -223,  -223,   291,  -223,   209,   264,
    -223,  -223,  -223,   268,  -223,    -6,  -223,  -223,   277,  -109,
     -30,  -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,
    -223,  -223,  -223,  -223,  -223,  -223,  -223,   -58,  -223,  -223,
    -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,  -223,
    -223,  -222,   272,  -223,   -66,  -194,  -192,   247,  -101,   -78,
    -223,    -4,  -223,  -223,   175,  -223,  -223,  -223,  -223,  -223,
    -223,   250,  -202,   281,   -15,   248,   163,  -223,  -223,  -223,
    -223,  -223,  -223,  -223,   -54,  -223,  -223,  -223,   -65,   -48,
     293,   -71,  -127,   -55,  -126,   -53,   -26,     5,   -32
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -265
static const yytype_int16 yytable[] =
{
      17,    93,   177,   223,   141,   242,   231,   324,   152,   243,
     298,   215,   299,   118,   177,    35,   196,   310,   311,   312,
     313,   314,   203,   169,     2,     3,   224,   203,   140,   153,
     143,   170,   363,   136,   203,   392,   193,   198,    27,   192,
     233,   303,   162,    13,   184,   329,   155,    15,     4,   210,
     156,   199,   364,   184,     5,   181,   175,   271,   272,    33,
     237,    14,   238,     7,   181,   157,   289,   280,     5,   197,
       9,     6,   317,   318,   202,   260,   261,     7,   393,   365,
     239,   264,     8,   128,     9,   218,    13,   129,   240,   380,
      15,   155,   260,   261,   373,   360,   319,    10,    11,   189,
     204,   241,    12,   381,   189,   116,   117,   171,   242,   155,
     157,   189,    11,   156,   155,    28,    12,   232,   234,   177,
     259,   276,   155,    30,   234,    31,   216,   339,   157,   301,
      36,   268,   269,   157,   270,   332,   333,   127,   273,    32,
     336,   157,   275,   127,   244,   116,   117,   130,   131,   323,
     277,    38,   340,   130,   131,   235,   127,   262,   263,   284,
     285,   288,    34,   290,   291,   160,   130,   131,   174,    13,
     161,   295,    14,    15,   173,   163,   300,   165,   408,   302,
     399,   304,   305,   306,   155,   281,   397,   283,   398,   330,
     331,   166,   120,   121,   122,   123,   293,   167,   185,   186,
     187,   219,   297,   157,   203,    13,   264,   188,   387,    15,
     245,   246,   189,   128,   247,   125,   126,   209,   194,   248,
     264,   202,   211,   213,   249,   266,   274,   278,   377,   279,
     282,   335,   120,   121,   122,   123,   292,   128,   334,   294,
     296,   129,   327,   341,   342,   250,   251,   377,   252,   253,
     135,   343,   377,   345,   247,   125,   126,   344,   127,   347,
     346,   348,   128,    13,   349,   377,   129,    15,   130,   131,
     174,   352,   350,   351,   354,   356,   362,   353,   369,   366,
     401,   189,   204,   368,   378,   358,   389,   391,   133,   134,
     135,   204,   404,   395,   403,   220,   221,   222,   406,    29,
     217,   154,   139,   410,   127,    40,   142,   385,   128,    13,
     177,   190,   129,    15,   130,   131,   174,   400,   195,   158,
     201,   255,   267,   372,   394,   390,   402,   148,   361,   383,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   367,     0,     0,     0,     0,     0,   371,     0,
       0,     0,     0,     0,     0,     0,     0,    41,     0,   116,
     117,    42,   384,    43,   175,   386,     0,   388,    44,    45,
      46,     0,    47,     0,    48,     0,     0,    49,    50,     0,
       0,     0,   118,    51,    52,     0,     0,     0,     0,     0,
      53,     0,    54,     0,     0,    55,    56,    57,     0,     0,
       7,     0,     0,     0,     0,     0,    58,     9,    59,     0,
      60,    61,    62,    63,     0,    64,     0,    65,    66,    67,
      10,     0,    68,     0,     0,    69,    38,    70,    71,    72,
     119,    74,     0,     0,    75,    11,    76,    77,    78,     0,
       0,     0,     0,     0,    79,     0,   120,   121,   122,   123,
       0,    80,    81,    82,    83,     0,     0,     0,     0,     0,
       0,    84,    85,     0,     0,    86,     0,     0,   124,   125,
     126,    87,    88,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   127,     0,    90,
       0,   128,    13,     0,     0,   129,    15,   130,   131,   132,
     307,     0,   133,   134,   135,     0,     0,     0,     0,    41,
       0,     0,     0,    42,     0,    43,     0,     0,     0,     0,
      44,    45,    46,     0,    47,     0,    48,     0,     0,    49,
      50,     0,     0,     0,     0,    51,    52,     0,     0,     0,
       0,     0,    53,     0,    54,     0,     0,    55,    56,    57,
       0,  -108,     0,     0,  -108,     0,     0,     0,    58,     0,
      59,     0,    60,    61,    62,    63,     0,    64,     0,    65,
      66,    67,     0,     0,    68,     0,     0,    69,    38,    70,
      71,    72,    73,    74,     0,     0,    75,     0,    76,    77,
      78,     0,     0,     0,     0,     0,    79,     0,     0,     0,
       0,     0,     0,    80,    81,    82,    83,     0,     0,     0,
       0,     0,     0,    84,    85,   308,     0,    86,     0,     0,
       0,     0,     0,    87,    88,     0,     0,     0,     0,   307,
       0,  -108,  -108,     0,     0,     0,     0,     0,    41,     0,
       0,    90,    42,     0,    43,  -108,  -108,  -108,     0,    44,
      45,    46,     0,    47,     0,    48,     0,     0,    49,    50,
       0,     0,     0,     0,    51,    52,     0,     0,     0,     0,
       0,    53,     0,    54,     0,     0,    55,    56,    57,     0,
    -108,     0,     0,  -108,     0,     0,     0,    58,     0,    59,
       0,    60,    61,    62,    63,     0,    64,     0,    65,    66,
      67,     0,     0,    68,     0,     0,    69,    38,    70,    71,
      72,    73,    74,     0,     0,    75,     0,    76,    77,    78,
       0,     0,     0,     0,   325,    79,     0,     0,     0,     0,
       0,     0,    80,    81,    82,    83,     0,     0,     0,     0,
       0,     0,    84,    85,     0,     0,    86,     0,     0,     0,
       0,     0,    87,    88,     0,     0,     0,     0,   307,     0,
    -108,  -108,     0,     0,     0,     0,     0,    41,     0,     0,
      90,    42,     0,    43,  -108,  -108,  -108,     0,    44,    45,
      46,     0,    47,     0,    48,     0,     0,    49,    50,     0,
       0,     0,     0,    51,    52,     0,     0,     0,     0,     0,
      53,     0,    54,     0,     0,    55,    56,    57,     0,  -108,
       0,     0,  -108,     0,     0,     0,    58,     0,    59,     0,
      60,    61,    62,    63,     0,    64,     0,    65,    66,    67,
       0,     0,    68,     0,     0,    69,    38,    70,    71,    72,
      73,    74,     0,     0,    75,     0,    76,    77,    78,     0,
       0,     0,     0,     0,    79,     0,     0,     0,     0,     0,
       0,    80,    81,    82,    83,     0,     0,     0,     0,     0,
       0,    84,    85,   337,     0,    86,     0,     0,     0,     0,
       0,    87,    88,     0,     0,     0,     0,   307,     0,  -108,
    -108,     0,     0,     0,     0,     0,    41,     0,     0,    90,
      42,     0,    43,  -108,  -108,  -108,     0,    44,    45,    46,
       0,    47,     0,    48,     0,     0,    49,    50,     0,     0,
       0,     0,    51,    52,     0,     0,     0,     0,     0,    53,
       0,    54,     0,     0,    55,    56,    57,     0,  -108,     0,
       0,  -108,     0,     0,     0,    58,     0,    59,     0,    60,
      61,    62,    63,     0,    64,     0,    65,    66,    67,     0,
       0,    68,     0,     0,    69,    38,    70,    71,    72,    73,
      74,     0,     0,    75,     0,    76,    77,    78,     0,     0,
       0,     0,     0,    79,     0,     0,     0,     0,     0,     0,
      80,    81,    82,    83,     0,     0,     0,     0,     0,     0,
      84,    85,   338,     0,    86,     0,     0,     0,     0,     0,
      87,    88,     0,     0,     0,     0,   307,     0,  -108,  -108,
       0,     0,     0,     0,     0,    41,     0,     0,    90,    42,
       0,    43,  -108,  -108,  -108,     0,    44,    45,    46,     0,
      47,     0,    48,     0,     0,    49,    50,     0,     0,     0,
       0,    51,    52,     0,     0,     0,     0,     0,    53,     0,
      54,     0,     0,    55,    56,    57,     0,  -108,     0,     0,
    -108,     0,     0,     0,    58,     0,    59,     0,    60,    61,
      62,    63,     0,    64,     0,    65,    66,    67,     0,     0,
      68,     0,     0,    69,    38,    70,    71,    72,    73,    74,
       0,     0,    75,     0,    76,    77,    78,     0,     0,     0,
       0,     0,    79,     0,     0,     0,     0,     0,     0,    80,
      81,    82,    83,     0,     0,     0,     0,     0,     0,    84,
      85,   409,     0,    86,     0,     0,     0,     0,    41,    87,
      88,     0,    42,     0,    43,     0,     0,  -108,  -108,    44,
      45,    46,     0,    47,     0,    48,     0,    90,    49,    50,
       0,  -108,  -108,  -108,    51,    52,     0,     0,     0,     0,
       0,    53,     0,    54,     0,     0,    55,    56,    57,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,    59,
       0,    60,    61,    62,    63,     0,    64,     0,    65,    66,
      67,     0,     0,    68,     0,     0,    69,    38,    70,    71,
      72,    73,    74,     0,     0,    75,     0,    76,    77,    78,
       0,     0,     0,     0,     0,    79,     0,     0,     0,     0,
       0,     0,    80,    81,    82,    83,     0,     0,     0,     0,
       0,  -264,    84,    85,     0,    41,    86,     0,     0,    42,
       0,    43,    87,    88,     0,     0,    44,    45,    46,     0,
      47,     0,    48,     0,     0,    49,    50,     0,     0,     0,
      90,    51,    52,     0,     0,     0,     0,     0,    53,     0,
      54,  -264,     0,    55,    56,    57,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,    59,     0,    60,    61,
      62,    63,     0,    64,     0,    65,    66,    67,     0,     0,
      68,     0,     0,    69,    38,    70,    71,    72,    73,    74,
       0,     0,    75,     0,    76,    77,    78,     0,     0,     0,
       0,     0,    79,     0,     0,     0,     0,     0,     0,    80,
      81,    82,    83,     0,     0,     0,     0,     0,     0,    84,
      85,     0,    41,    86,     0,     0,    42,     0,    43,    87,
      88,     0,     0,    44,    45,    46,     0,    47,     0,    48,
       0,     0,    49,    50,     0,     0,     0,    90,    51,    52,
       0,     0,     0,     0,     0,    53,     0,    54,   264,     0,
      55,    56,    57,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,    59,     0,    60,    61,    62,    63,     0,
      64,     0,    65,    66,    67,     0,     0,    68,     0,     0,
      69,    38,    70,    71,    72,    73,    74,     0,     0,    75,
       0,    76,    77,    78,     0,     0,     0,     0,     0,    79,
       0,     0,     0,     0,     0,     0,    80,    81,    82,    83,
       0,     0,     0,     0,     0,     0,    84,    85,     0,     0,
      86,     0,     0,     0,     0,     0,    87,    88,     0,     0,
      41,     0,     0,     0,    42,     0,    43,     0,     0,     0,
      89,    44,    45,    46,    90,    47,     0,    48,     0,     0,
      49,    50,     0,     0,     0,     0,    51,    52,     0,     0,
       0,     0,     0,    53,     0,    54,     0,     0,    55,    56,
      57,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,    59,     0,    60,    61,    62,    63,     0,    64,     0,
      65,    66,    67,     0,     0,    68,     0,     0,    69,    38,
      70,    71,    72,    73,    74,     0,     0,    75,     0,    76,
      77,    78,     0,     0,     0,     0,     0,    79,     0,     0,
       0,     0,     0,     0,    80,    81,    82,    83,     0,     0,
       0,     0,     0,     0,    84,    85,     0,     0,    86,     0,
       0,     0,     0,     0,    87,    88,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    90
};

static const yytype_int16 yycheck[] =
{
       1,    31,    57,   104,    34,   132,   115,   229,    34,   135,
     204,     1,   204,    35,    69,     1,    69,   219,   220,   221,
     222,   223,    55,    53,     0,     1,   104,    55,    34,   116,
      34,    56,    66,    34,    55,   110,    66,   136,   114,    65,
     114,   113,    43,   145,    59,   122,   131,   149,    24,    75,
     135,   150,    86,    68,    44,    59,    57,   166,   167,   148,
      26,   148,    28,    53,    68,   150,   192,   126,    44,    70,
      60,    47,    93,    94,   148,   152,   153,    53,   153,   113,
      46,   153,    58,   144,    60,    91,   145,   148,    54,   113,
     149,   131,   152,   153,   134,   317,   117,    73,    88,   132,
     133,    67,    92,   127,   132,    12,    13,   132,   235,   131,
     150,   132,    88,   135,   131,   114,    92,   118,   121,   174,
     146,   174,   131,   114,   121,   114,   116,   136,   150,   207,
     116,   161,   162,   150,   164,   262,   263,   140,   168,   114,
     266,   150,   172,   140,    51,    12,    13,   150,   151,   227,
     180,    79,   278,   150,   151,   152,   140,   119,   120,   129,
     130,   191,   113,   193,   194,   148,   150,   151,   152,   145,
     148,   201,   148,   149,    56,   148,   206,   148,   400,   209,
     382,   211,   212,   213,   131,   186,   380,   188,   380,   260,
     261,   114,    99,   100,   101,   102,   197,   114,   148,   148,
     148,    52,   203,   150,    55,   145,   153,   148,   148,   149,
     117,   118,   132,   144,   121,   122,   123,   148,   148,   126,
     153,   148,   148,   148,   131,   113,    11,   120,   354,   126,
     126,   126,    99,   100,   101,   102,   148,   144,   264,   119,
     113,   148,   143,   148,   148,   152,   153,   373,   155,   156,
     157,   113,   378,   129,   121,   122,   123,   148,   140,   289,
     130,   291,   144,   145,   113,   391,   148,   149,   150,   151,
     152,   301,   150,   148,   109,   128,   116,   303,    20,   113,
     389,   132,   133,   113,   111,   152,   114,   126,   155,   156,
     157,   133,   108,   119,   395,   146,   147,   148,    95,     8,
      91,    37,    34,   404,   140,    28,    34,   365,   144,   145,
     365,    64,   148,   149,   150,   151,   152,   383,    68,    38,
      72,   146,   159,   353,   378,   373,   391,    34,   323,   361,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   343,    -1,    -1,    -1,    -1,    -1,   349,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    14,   363,    16,   365,   366,    -1,   368,    21,    22,
      23,    -1,    25,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    35,    36,    37,    -1,    -1,    -1,    -1,    -1,
      43,    -1,    45,    -1,    -1,    48,    49,    50,    -1,    -1,
      53,    -1,    -1,    -1,    -1,    -1,    59,    60,    61,    -1,
      63,    64,    65,    66,    -1,    68,    -1,    70,    71,    72,
      73,    -1,    75,    -1,    -1,    78,    79,    80,    81,    82,
      83,    84,    -1,    -1,    87,    88,    89,    90,    91,    -1,
      -1,    -1,    -1,    -1,    97,    -1,    99,   100,   101,   102,
      -1,   104,   105,   106,   107,    -1,    -1,    -1,    -1,    -1,
      -1,   114,   115,    -1,    -1,   118,    -1,    -1,   121,   122,
     123,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   140,    -1,   142,
      -1,   144,   145,    -1,    -1,   148,   149,   150,   151,   152,
       1,    -1,   155,   156,   157,    -1,    -1,    -1,    -1,    10,
      -1,    -1,    -1,    14,    -1,    16,    -1,    -1,    -1,    -1,
      21,    22,    23,    -1,    25,    -1,    27,    -1,    -1,    30,
      31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    -1,
      -1,    -1,    43,    -1,    45,    -1,    -1,    48,    49,    50,
      -1,    52,    -1,    -1,    55,    -1,    -1,    -1,    59,    -1,
      61,    -1,    63,    64,    65,    66,    -1,    68,    -1,    70,
      71,    72,    -1,    -1,    75,    -1,    -1,    78,    79,    80,
      81,    82,    83,    84,    -1,    -1,    87,    -1,    89,    90,
      91,    -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,   106,   107,    -1,    -1,    -1,
      -1,    -1,    -1,   114,   115,   116,    -1,   118,    -1,    -1,
      -1,    -1,    -1,   124,   125,    -1,    -1,    -1,    -1,     1,
      -1,   132,   133,    -1,    -1,    -1,    -1,    -1,    10,    -1,
      -1,   142,    14,    -1,    16,   146,   147,   148,    -1,    21,
      22,    23,    -1,    25,    -1,    27,    -1,    -1,    30,    31,
      -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    -1,    -1,
      -1,    43,    -1,    45,    -1,    -1,    48,    49,    50,    -1,
      52,    -1,    -1,    55,    -1,    -1,    -1,    59,    -1,    61,
      -1,    63,    64,    65,    66,    -1,    68,    -1,    70,    71,
      72,    -1,    -1,    75,    -1,    -1,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    87,    -1,    89,    90,    91,
      -1,    -1,    -1,    -1,    96,    97,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,   114,   115,    -1,    -1,   118,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,   106,   107,    -1,    -1,    -1,    -1,    -1,
      -1,   114,   115,   116,    -1,   118,    -1,    -1,    -1,    -1,
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
     115,   116,    -1,   118,    -1,    -1,    -1,    -1,    10,   124,
     125,    -1,    14,    -1,    16,    -1,    -1,   132,   133,    21,
      22,    23,    -1,    25,    -1,    27,    -1,   142,    30,    31,
      -1,   146,   147,   148,    36,    37,    -1,    -1,    -1,    -1,
      -1,    43,    -1,    45,    -1,    -1,    48,    49,    50,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    59,    -1,    61,
      -1,    63,    64,    65,    66,    -1,    68,    -1,    70,    71,
      72,    -1,    -1,    75,    -1,    -1,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    87,    -1,    89,    90,    91,
      -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,   106,   107,    -1,    -1,    -1,    -1,
      -1,   113,   114,   115,    -1,    10,   118,    -1,    -1,    14,
      -1,    16,   124,   125,    -1,    -1,    21,    22,    23,    -1,
      25,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
     142,    36,    37,    -1,    -1,    -1,    -1,    -1,    43,    -1,
      45,   153,    -1,    48,    49,    50,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    59,    -1,    61,    -1,    63,    64,
      65,    66,    -1,    68,    -1,    70,    71,    72,    -1,    -1,
      75,    -1,    -1,    78,    79,    80,    81,    82,    83,    84,
      -1,    -1,    87,    -1,    89,    90,    91,    -1,    -1,    -1,
      -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,   114,
     115,    -1,    10,   118,    -1,    -1,    14,    -1,    16,   124,
     125,    -1,    -1,    21,    22,    23,    -1,    25,    -1,    27,
      -1,    -1,    30,    31,    -1,    -1,    -1,   142,    36,    37,
      -1,    -1,    -1,    -1,    -1,    43,    -1,    45,   153,    -1,
      48,    49,    50,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    59,    -1,    61,    -1,    63,    64,    65,    66,    -1,
      68,    -1,    70,    71,    72,    -1,    -1,    75,    -1,    -1,
      78,    79,    80,    81,    82,    83,    84,    -1,    -1,    87,
      -1,    89,    90,    91,    -1,    -1,    -1,    -1,    -1,    97,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,   106,   107,
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,
     118,    -1,    -1,    -1,    -1,    -1,   124,   125,    -1,    -1,
      10,    -1,    -1,    -1,    14,    -1,    16,    -1,    -1,    -1,
     138,    21,    22,    23,   142,    25,    -1,    27,    -1,    -1,
      30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,
      -1,    -1,    -1,    43,    -1,    45,    -1,    -1,    48,    49,
      50,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    59,
      -1,    61,    -1,    63,    64,    65,    66,    -1,    68,    -1,
      70,    71,    72,    -1,    -1,    75,    -1,    -1,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    87,    -1,    89,
      90,    91,    -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,   106,   107,    -1,    -1,
      -1,    -1,    -1,    -1,   114,   115,    -1,    -1,   118,    -1,
      -1,    -1,    -1,    -1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142
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
     254,   148,   189,   148,   177,     1,   116,   166,   173,    52,
     146,   147,   148,   216,   217,   237,   240,   202,   219,   230,
     232,   177,   161,   114,   121,   152,   248,    26,    28,    46,
      54,    67,   250,   252,    51,   117,   118,   121,   126,   131,
     152,   153,   155,   156,   211,   222,   224,   225,   226,   254,
     152,   153,   119,   120,   153,   234,   113,   234,   178,   178,
     178,   177,   177,   178,    11,   178,   253,   178,   120,   126,
     126,   161,   126,   161,   129,   130,   213,   214,   178,   252,
     178,   178,   148,   161,   119,   178,   113,   161,   213,   214,
     178,   217,   178,   113,   178,   178,   178,     1,   116,   178,
     230,   230,   230,   230,   230,   255,   209,    93,    94,   117,
     200,   203,   204,   217,   209,    96,   239,   143,   170,   122,
     249,   249,   250,   250,   254,   126,   252,   116,   116,   136,
     252,   148,   148,   113,   148,   129,   130,   178,   178,   113,
     150,   148,   178,   254,   109,   241,   128,   256,   152,   210,
     209,   255,   116,    66,    86,   113,   113,   161,   113,    20,
     179,   161,   178,   134,   242,   246,   247,   252,   111,   243,
     113,   127,   212,   256,   161,   195,   161,   148,   161,   114,
     247,   126,   110,   153,   242,   119,   244,   213,   214,   230,
     212,   177,   246,   216,   108,   245,    95,   236,   209,   116,
     216
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
		MALLOC_NODE(n2, (yyvsp[(2) - (2)].generic))
		(yyval.scm) = g_list_append((yyvsp[(1) - (2)].scm), n2);

/*		THIS-> lexer_-> scopes_.pop ();*/
	;}
    break;

  case 41:
#line 711 "./lyparser.y"
    {
		set_identifier("midi_tempo", typed_glist ((yyvsp[(3) - (3)].scm), STRING_IDENTIFIER));
		(yyval.scm) = (yyvsp[(3) - (3)].scm);
	;}
    break;

  case 42:
#line 715 "./lyparser.y"
    {
		/* caught by lexer - does not occur*/
		lyerror ("parser should have caught this-paper");
	;}
    break;

  case 43:
#line 719 "./lyparser.y"
    {
		/* caught by lexer - does not occur*/
		lyerror ("parser should have caught this - layout");
	;}
    break;

  case 44:
#line 723 "./lyparser.y"
    {
		lyerror("music_output_def_body error");
		;}
    break;

  case 45:
#line 729 "./lyparser.y"
    {
		(yyval.scm) = NULL;
	;}
    break;

  case 46:
#line 732 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 47:
#line 738 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(4) - (4)].i));
	((nodei*)n)->i = (yyvsp[(4) - (4)].i).i;
	(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 48:
#line 752 "./lyparser.y"
    {
		(yyval.scm) = NULL;
	;}
    break;

  case 49:
#line 755 "./lyparser.y"
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
#line 771 "./lyparser.y"
    {
	;}
    break;

  case 52:
#line 779 "./lyparser.y"
    {
			(yyval.scm) = (yyvsp[(1) - (1)].scm);
		;}
    break;

  case 53:
#line 785 "./lyparser.y"
    {
			(yyval.scm) = NULL;
	;}
    break;

  case 54:
#line 788 "./lyparser.y"
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
#line 801 "./lyparser.y"
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
#line 822 "./lyparser.y"
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
#line 830 "./lyparser.y"
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
#line 842 "./lyparser.y"
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
#line 851 "./lyparser.y"
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
#line 867 "./lyparser.y"
    { (yyval.scm) = (yyvsp[(1) - (1)].scm); ;}
    break;

  case 61:
#line 868 "./lyparser.y"
    {
		DenemoObject *mud = lily_directive_new ((yyvsp[(1) - (1)].gstr).user_string);		
		(yyval.scm) = g_list_append(NULL,mud);
	;}
    break;

  case 62:
#line 872 "./lyparser.y"
    {
		DenemoObject *mud = lily_directive_new (g_strconcat((yyvsp[(1) - (2)].gstr).user_string, (yyvsp[(2) - (2)].gstr).user_string));	
		(yyval.scm) = g_list_append(NULL,mud);
	;}
    break;

  case 63:
#line 876 "./lyparser.y"
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
#line 902 "./lyparser.y"
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
#line 912 "./lyparser.y"
    {

		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	/* FIXME - we really don't want to put all these nodes into
	   the music, as denemo will have to go over them -
	   amalgamate the strings into a TEXT node */
		

	;}
    break;

  case 66:
#line 921 "./lyparser.y"
    {
	LATER_MESSAGE((yyloc).first_line);
	;}
    break;

  case 67:
#line 928 "./lyparser.y"
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
#line 949 "./lyparser.y"
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
#line 965 "./lyparser.y"
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
#line 976 "./lyparser.y"
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
#line 999 "./lyparser.y"
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

  case 72:
#line 1023 "./lyparser.y"
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

  case 73:
#line 1034 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 74:
#line 1037 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 75:
#line 1040 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 76:
#line 1043 "./lyparser.y"
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

  case 77:
#line 1055 "./lyparser.y"
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

  case 78:
#line 1068 "./lyparser.y"
    {
		u_str((yyvsp[(2) - (3)].scm)) = g_strconcat((yyvsp[(1) - (3)].generic).user_string, u_str((yyvsp[(2) - (3)].scm)), NULL);
		(yyval.scm) = g_list_concat((yyvsp[(2) - (3)].scm), (yyvsp[(3) - (3)].scm));
	;}
    break;

  case 79:
#line 1073 "./lyparser.y"
    { push_note_state (); ;}
    break;

  case 80:
#line 1076 "./lyparser.y"
    { 
		MALLOC_NODE(n1, (yyvsp[(1) - (3)].generic));
		(yyval.scm) = g_list_prepend((yyvsp[(3) - (3)].scm), n1);
		pop_state();
		;}
    break;

  case 81:
#line 1082 "./lyparser.y"
    { push_figuredbass_state (); ;}
    break;

  case 82:
#line 1084 "./lyparser.y"
    {
		MALLOC_NODE(n1, (yyvsp[(1) - (3)].generic));
		(yyval.scm) = g_list_prepend((yyvsp[(3) - (3)].scm), n1);
		pop_state();
	;}
    break;

  case 83:
#line 1090 "./lyparser.y"
    { push_chord_state (); ;}
    break;

  case 84:
#line 1092 "./lyparser.y"
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

  case 85:
#line 1105 "./lyparser.y"
    { push_lyric_state (); ;}
    break;

  case 86:
#line 1107 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
		MALLOC_NODE(n, (yyvsp[(1) - (3)].generic))
		(yyval.scm) = g_list_prepend((yyvsp[(3) - (3)].scm), n);
		pop_state();
	;}
    break;

  case 87:
#line 1114 "./lyparser.y"
    { push_lyric_state (); ;}
    break;

  case 88:
#line 1116 "./lyparser.y"
    {
		MALLOC_NODE(n, (yyvsp[(1) - (3)].generic))
		(yyval.scm) = g_list_prepend((yyvsp[(3) - (3)].scm), n);
		pop_state();
	;}
    break;

  case 89:
#line 1121 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 90:
#line 1124 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 91:
#line 1127 "./lyparser.y"
    { 
		(yyval.scm) = (yyvsp[(1) - (1)].scm); 
	;}
    break;

  case 92:
#line 1133 "./lyparser.y"
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

  case 93:
#line 1148 "./lyparser.y"
    {
		MALLOC_NODE(n, (yyvsp[(1) - (3)].generic));
		((nodeglist*)n)->branch = g_list_append(g_list_append(NULL, g_list_append(NULL, (yyvsp[(2) - (3)].scm))), g_list_append(NULL, (yyvsp[(3) - (3)].scm))); /* ADDLYRICS is a branch containing two GLists */
		(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 94:
#line 1153 "./lyparser.y"
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

  case 95:
#line 1177 "./lyparser.y"
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

  case 96:
#line 1195 "./lyparser.y"
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

  case 97:
#line 1212 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (6)].generic))
	 n->user_string = g_strconcat((yyvsp[(1) - (6)].generic).user_string, (yyvsp[(2) - (6)].gstr).user_string, 
				      ".", (yyvsp[(4) - (6)].gstr).user_string, 
				      "=", u_str((yyvsp[(6) - (6)].scm)), NULL);
	(yyval.scm) = g_list_append(NULL, n); /* FIXME memory leak*/
	;}
    break;

  case 98:
#line 1219 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (5)].generic))
	 n->user_string = g_strconcat((yyvsp[(1) - (5)].generic).user_string, (yyvsp[(2) - (5)].gstr).user_string, 
				      (yyvsp[(3) - (5)].i).user_string, (yyvsp[(4) - (5)].gstr).user_string, 
				      (yyvsp[(5) - (5)].generic).user_string, NULL);
	(yyval.scm) = g_list_append(NULL, n); /* FIXME memory leak*/
	;}
    break;

  case 99:
#line 1226 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (6)].generic))
	n->user_string = g_strconcat((yyvsp[(1) - (6)].generic).user_string, (yyvsp[(2) - (6)].gstr).user_string, 
				     (yyvsp[(3) - (6)].i).user_string, (yyvsp[(4) - (6)].gstr).user_string, (yyvsp[(5) - (6)].generic).user_string, u_str((yyvsp[(6) - (6)].scm)), NULL);
	(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 100:
#line 1232 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (6)].generic))
	n->user_string = g_strconcat((yyvsp[(1) - (6)].generic).user_string, (yyvsp[(2) - (6)].gstr).user_string, 
				     (yyvsp[(3) - (6)].i).user_string, (yyvsp[(4) - (6)].gstr).user_string, 
				(yyvsp[(5) - (6)].generic).user_string, (yyvsp[(6) - (6)].gstr).user_string, NULL);
	(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 101:
#line 1239 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (6)].generic));
	n->user_string = g_strconcat((yyvsp[(1) - (6)].generic).user_string, (yyvsp[(2) - (6)].gstr).user_string, 
				     (yyvsp[(3) - (6)].i).user_string, (yyvsp[(4) - (6)].gstr).user_string, 
				     (yyvsp[(5) - (6)].generic).user_string, u_str((yyvsp[(6) - (6)].scm)), NULL);
	(yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 102:
#line 1246 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (5)].generic));
	n->user_string = g_strconcat((yyvsp[(1) - (5)].generic).user_string, (yyvsp[(2) - (5)].gstr).user_string, 
				     u_str ((yyvsp[(3) - (5)].scm)), (yyvsp[(4) - (5)].generic).user_string, 
				     u_str ((yyvsp[(5) - (5)].scm)), NULL);
	(yyval.scm) = g_list_append (NULL, n);
        ;}
    break;

  case 103:
#line 1253 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (3)].generic));
	n->user_string = g_strconcat((yyvsp[(1) - (3)].generic).user_string, (yyvsp[(2) - (3)].gstr).user_string, 
				     u_str ((yyvsp[(3) - (3)].scm)), NULL);
	(yyval.scm) = g_list_append (NULL, n);
        ;}
    break;

  case 104:
#line 1260 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (6)].generic));
	n->user_string = g_strconcat((yyvsp[(1) - (6)].generic).user_string, (yyvsp[(2) - (6)].gstr).user_string, 
				     (yyvsp[(3) - (6)].i).user_string, (yyvsp[(4) - (6)].gstr).user_string, 
				     (yyvsp[(5) - (6)].generic).user_string, u_str ((yyvsp[(6) - (6)].scm)), NULL);
	(yyval.scm) = g_list_append (NULL, n);
	;}
    break;

  case 105:
#line 1271 "./lyparser.y"
    { MALLOC_NODE(n, (yyvsp[(1) - (1)].gstr));(yyval.scm) = g_list_append (NULL, n); 
			/*FIXME copy value */;}
    break;

  case 106:
#line 1273 "./lyparser.y"
    { MALLOC_NODE(n, (yyvsp[(1) - (1)].i));(yyval.scm) = g_list_append (NULL, n);
			  /*FIXME copy value */ ;}
    break;

  case 107:
#line 1275 "./lyparser.y"
    { (yyval.scm) = (yyvsp[(1) - (1)].scm); ;}
    break;

  case 109:
#line 1282 "./lyparser.y"
    {
/* things like start cresc, simple_element end cresc */

	set_post_events ((DenemoObject *) ((yyvsp[(2) - (3)].scm)->data), u_str ((yyvsp[(2) - (3)].scm)), (yyvsp[(3) - (3)].scm));
			
	(yyval.scm) = (yyvsp[(2) - (3)].scm);/* FIXME memory leak */
	;}
    break;

  case 110:
#line 1289 "./lyparser.y"
    {
	(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 111:
#line 1292 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 112:
#line 1299 "./lyparser.y"
    {
	    GList *firstchord = br ((yyvsp[(1) - (3)].scm));
	    if (firstchord && firstchord->data)
		changedur ((DenemoObject *)(firstchord->data), (yyvsp[(2) - (3)].f).t1.a, (yyvsp[(2) - (3)].f).t1.b);
	    set_post_events ((DenemoObject *) ((yyvsp[(1) - (3)].scm)->data), u_str ((yyvsp[(1) - (3)].scm)), (yyvsp[(3) - (3)].scm));
	    (yyval.scm) = (yyvsp[(1) - (3)].scm);
	;}
    break;

  case 115:
#line 1317 "./lyparser.y"
    {
	    nodegeneric*n1 = (nodegeneric*)g_malloc0(sizeof(nodegeneric));
	    n1->user_string = "<";	
	    ((nodeglist*)n1)->post_user_string = ">";
	    ((nodeglist*)n1)->type = SIMULTANEOUS;
	    ((nodeglist*)n1)->branch = (yyvsp[(2) - (3)].scm);
	    (yyval.scm) = g_list_prepend(NULL, n1);
	;}
    break;

  case 116:
#line 1328 "./lyparser.y"
    { (yyval.scm) = NULL; ;}
    break;

  case 117:
#line 1329 "./lyparser.y"
    {
		(yyval.scm) = g_list_concat ((yyvsp[(1) - (2)].scm), (yyvsp[(2) - (2)].scm));
	;}
    break;

  case 118:
#line 1335 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
                (yyval.scm) = scm_list_2 ((yyvsp[(1) - (1)].generic), make_input ((yyloc)));
#endif
        ;}
    break;

  case 119:
#line 1345 "./lyparser.y"
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

  case 120:
#line 1374 "./lyparser.y"
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

  case 121:
#line 1389 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
        ;}
    break;

  case 122:
#line 1395 "./lyparser.y"
    {
	(yyval.scm) = (yyvsp[(1) - (1)].scm);
;}
    break;

  case 123:
#line 1398 "./lyparser.y"
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

  case 124:
#line 1413 "./lyparser.y"
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

  case 125:
#line 1428 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		(yyval.scm) = new Music (gh_list (gh_cons (ly_symbol2scm ("name"), ly_symbol2scm ("separator")), SCM_UNDEFINED));
		(yyval.scm)->set_spot (THIS->here_input ());
#endif
	;}
    break;

  case 126:
#line 1436 "./lyparser.y"
    {
	MALLOC_NODE(n, (yyvsp[(1) - (1)].generic));
	(yyval.scm) = g_list_append(NULL, n); /* this node used to be used by denemo to split the glist into measures */
	;}
    break;

  case 127:
#line 1440 "./lyparser.y"
    {
	                 DenemoObject *mud = lily_directive_new (g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].gstr).user_string, NULL));
		         (yyval.scm) = g_list_append(NULL,mud);

			//MALLOC_NODE(n, $1);
		  //n->user_string = g_strconcat($1.user_string, $2.user_string, NULL);/* FIXME memory leaks */
		 // $$ = g_list_append(NULL, n);
	;}
    break;

  case 128:
#line 1448 "./lyparser.y"
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

  case 129:
#line 1457 "./lyparser.y"
    {
  		DenemoObject *mud = dnm_newclefobj (cleftypefromname((yyvsp[(2) - (2)].gstr).gstr->str));
		mud->user_string = g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].gstr).user_string,NULL);
		/* FIXME memory leaks on strings concatenated */
		(yyval.scm) = g_list_append(NULL,mud);
	;}
    break;

  case 130:
#line 1463 "./lyparser.y"
    {
  		DenemoObject *mud = dnm_newtimesigobj ((yyvsp[(2) - (2)].t).t.a, (yyvsp[(2) - (2)].t).t.b);
		mud->user_string = g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].t).user_string,NULL);
		/* FIXME memory leaks on strings concatenated */
		(yyval.scm) = g_list_append(NULL,mud);
	;}
    break;

  case 131:
#line 1469 "./lyparser.y"
    { /* ignore */
		(yyval.scm) = NULL;
	;}
    break;

  case 132:
#line 1475 "./lyparser.y"
    { (yyval.scm) = (yyvsp[(1) - (1)].scm); ;}
    break;

  case 133:
#line 1476 "./lyparser.y"
    { (yyval.scm) = (yyvsp[(1) - (1)].scm); ;}
    break;

  case 134:
#line 1480 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
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
	MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
	(yyval.scm) = g_list_append(NULL, n);/* FIXME denemo should know about this */
	;}
    break;

  case 137:
#line 1490 "./lyparser.y"
    {	
	MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
	(yyval.scm) = g_list_append(NULL, n);/* FIXME denemo should know about this */


	;}
    break;

  case 138:
#line 1496 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		(yyval.scm) = new Breathing_sign_req;
#endif
	;}
    break;

  case 139:
#line 1503 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		(yyval.scm) = new Porrectus_req;
#endif
	;}
    break;

  case 140:
#line 1513 "./lyparser.y"
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

  case 141:
#line 1524 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		Mark_req * m = new Mark_req;
		(yyval.scm) = m;
#endif
	;}
    break;

  case 142:
#line 1532 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		Mark_req *m = new Mark_req;
		m->set_mus_property ("label", (yyvsp[(2) - (2)].scm));
		(yyval.scm) = m;
#endif
	;}
    break;

  case 143:
#line 1540 "./lyparser.y"
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

  case 144:
#line 1555 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
//		$$ = $1;
	;}
    break;

  case 145:
#line 1559 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER

		Key_change_req *key= new Key_change_req;
		(yyval.scm) = key;
#endif
	;}
    break;

  case 146:
#line 1567 "./lyparser.y"
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

  case 147:
#line 1581 "./lyparser.y"
    {
	(yyval.scm) = NULL;
	;}
    break;

  case 148:
#line 1584 "./lyparser.y"
    {
		if((yyvsp[(1) - (2)].scm)) {
			(yyval.scm) = g_list_concat((yyvsp[(1) - (2)].scm), (yyvsp[(2) - (2)].scm));
			}
		else 
			(yyval.scm) = (yyvsp[(2) - (2)].scm);
	;}
    break;

  case 149:
#line 1594 "./lyparser.y"
    {
                (yyval.scm) = (yyvsp[(1) - (1)].scm);
        ;}
    break;

  case 150:
#line 1597 "./lyparser.y"
    {
	   /* script_dir is an integer saying whether up down or centred
	      denemo doesn't understand this yet */
	    u_str((yyvsp[(2) - (2)].scm)) = g_strconcat ( (yyvsp[(1) - (2)].i).user_string, u_str((yyvsp[(2) - (2)].scm)), NULL);
	    (yyval.scm) = (yyvsp[(2) - (2)].scm);
        ;}
    break;

  case 151:
#line 1603 "./lyparser.y"
    {
	   /* script_dir is an integer saying whether up down or centred
	      denemo doesn't understand this yet */
	    u_str((yyvsp[(2) - (2)].scm)) = g_strconcat ( (yyvsp[(1) - (2)].i).user_string, u_str((yyvsp[(2) - (2)].scm)), NULL);
	    (yyval.scm) = (yyvsp[(2) - (2)].scm);
        ;}
    break;

  case 152:
#line 1613 "./lyparser.y"
    {
                (yyval.scm) = (yyvsp[(1) - (1)].scm);
        ;}
    break;

  case 153:
#line 1616 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
        ;}
    break;

  case 154:
#line 1622 "./lyparser.y"
    {  ;}
    break;

  case 155:
#line 1623 "./lyparser.y"
    { (yyval.i).i = 0; ;}
    break;

  case 156:
#line 1624 "./lyparser.y"
    { (yyval.i) = (yyvsp[(2) - (2)].i); ;}
    break;

  case 157:
#line 1625 "./lyparser.y"
    { (yyval.i) = (yyvsp[(2) - (2)].i); ;}
    break;

  case 158:
#line 1630 "./lyparser.y"
    {
		(yyvsp[(1) - (1)].i).i = 1;
		(yyval.i) = (yyvsp[(1) - (1)].i);
	;}
    break;

  case 159:
#line 1634 "./lyparser.y"
    {
		(yyvsp[(1) - (2)].i).i ++;
		(yyvsp[(1) - (2)].i).user_string = g_strconcat((yyvsp[(1) - (2)].i).user_string,(yyvsp[(2) - (2)].i).user_string, NULL);/*FIXME memory leak */
		(yyval.i) = (yyvsp[(1) - (2)].i);
	;}
    break;

  case 160:
#line 1642 "./lyparser.y"
    {
		(yyvsp[(1) - (1)].i).i = 1;
		(yyval.i) = (yyvsp[(1) - (1)].i);	
	;}
    break;

  case 161:
#line 1646 "./lyparser.y"
    {
		(yyvsp[(1) - (2)].i).i ++ ;
		(yyvsp[(1) - (2)].i).user_string = g_strconcat((yyvsp[(1) - (2)].i).user_string,(yyvsp[(2) - (2)].i).user_string, NULL);/*FIXME memory leak */
		(yyval.i) = (yyvsp[(1) - (2)].i);
	;}
    break;

  case 162:
#line 1654 "./lyparser.y"
    {
		char notename = 'a' + (yyvsp[(1) - (1)].t).t.a;
		int enshift = (yyvsp[(1) - (1)].t).t.b;
		(yyval.n).user_string = (yyvsp[(1) - (1)].t).user_string;
		(yyval.n).n.enshift = enshift;
		(yyval.n).n.mid_c_offset = pitchtomid_c_offset (notename, 0);		
	;}
    break;

  case 163:
#line 1661 "./lyparser.y"
    {
		char notename = 'a' + (yyvsp[(1) - (2)].t).t.a;
		int enshift = (yyvsp[(1) - (2)].t).t.b;
		int sups=(yyvsp[(2) - (2)].i).i;
		(yyval.n).user_string = g_strconcat((yyvsp[(1) - (2)].t).user_string, (yyvsp[(2) - (2)].i).user_string, NULL);
		(yyval.n).n.enshift = enshift;
		(yyval.n).n.mid_c_offset = pitchtomid_c_offset (notename, sups);
	;}
    break;

  case 164:
#line 1669 "./lyparser.y"
    {
		char notename = 'a' + (yyvsp[(1) - (2)].t).t.a;
		int enshift = (yyvsp[(1) - (2)].t).t.b;
		int subs = -(yyvsp[(2) - (2)].i).i;
		(yyval.n).user_string = g_strconcat((yyvsp[(1) - (2)].t).user_string, (yyvsp[(2) - (2)].i).user_string, NULL);
		(yyval.n).n.enshift = enshift;
		(yyval.n).n.mid_c_offset = pitchtomid_c_offset (notename, subs);
	;}
    break;

  case 165:
#line 1684 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.n) = (yyvsp[(1) - (1)].t);
#endif
	;}
    break;

  case 166:
#line 1690 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		Pitch p = *unsmob_pitch ((yyvsp[(1) - (2)].t));
		p.octave_ +=  (yyvsp[(2) - (2)].i);
		(yyval.n) = p.smobbed_copy ();
#endif
	;}
    break;

  case 167:
#line 1698 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		Pitch p =* unsmob_pitch ((yyvsp[(1) - (2)].t));

		p.octave_ +=  -(yyvsp[(2) - (2)].i);
		(yyval.n) = p.smobbed_copy ();

#endif
	;}
    break;

  case 168:
#line 1711 "./lyparser.y"
    {
		(yyval.n) = (yyvsp[(1) - (1)].n);
	;}
    break;

  case 169:
#line 1714 "./lyparser.y"
    {
		(yyval.n) = (yyvsp[(1) - (1)].n);
	;}
    break;

  case 170:
#line 1720 "./lyparser.y"
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

  case 171:
#line 1733 "./lyparser.y"
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

  case 172:
#line 1747 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		if (!THIS->lexer_->lyric_state_b ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));
		(yyval.scm) = new Extender_req;
#endif
	;}
    break;

  case 173:
#line 1758 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		if (!THIS->lexer_->lyric_state_b ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));
		(yyval.scm) = new Hyphen_req;
#endif
	;}
    break;

  case 174:
#line 1769 "./lyparser.y"
    {
              (yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 175:
#line 1772 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].gstr));
	    ((nodegstr*)n)->gstr = (yyvsp[(1) - (1)].gstr).gstr;
	    (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 176:
#line 1777 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].gstr));
	    ((nodegstr*)n)->gstr = (yyvsp[(1) - (1)].gstr).gstr;
	    (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 177:
#line 1782 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
	;}
    break;

  case 178:
#line 1788 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 179:
#line 1792 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 180:
#line 1796 "./lyparser.y"
    {	/* tie */
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 181:
#line 1800 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 182:
#line 1804 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 183:
#line 1808 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].generic))
            (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 184:
#line 1812 "./lyparser.y"
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

  case 185:
#line 1824 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
	;}
    break;

  case 186:
#line 1829 "./lyparser.y"
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

  case 187:
#line 1845 "./lyparser.y"
    {
	    MALLOC_NODE(n, (yyvsp[(1) - (1)].gstr));
	    ((nodegstr*)n)->gstr = (yyvsp[(1) - (1)].gstr).gstr;
	    (yyval.scm) = g_list_append(NULL, n);
	;}
    break;

  case 188:
#line 1850 "./lyparser.y"
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

  case 189:
#line 1865 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Hat");
#endif
	;}
    break;

  case 190:
#line 1871 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Plus");
#endif
	;}
    break;

  case 191:
#line 1877 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Dash");
#endif
	;}
    break;

  case 192:
#line 1883 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Bar");
#endif
	;}
    break;

  case 193:
#line 1889 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Larger");
#endif
	;}
    break;

  case 194:
#line 1895 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Dot");
#endif
	;}
    break;

  case 195:
#line 1901 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_makfrom0str ("Underscore");
#endif
	;}
    break;

  case 196:
#line 1910 "./lyparser.y"
    { /* $$ = DOWN; */ ;}
    break;

  case 197:
#line 1911 "./lyparser.y"
    {  /* $$ = UP; */ ;}
    break;

  case 198:
#line 1912 "./lyparser.y"
    {  /* $$ = CENTER; */ ;}
    break;

  case 199:
#line 1917 "./lyparser.y"
    {
		(yyval.n) = (yyvsp[(1) - (1)].n);
	;}
    break;

  case 200:
#line 1923 "./lyparser.y"
    {
	(yyval.f) = (yyvsp[(1) - (1)].f);
	;}
    break;

  case 201:
#line 1926 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.f) = (yyvsp[(1) - (1)].f);
#endif
	;}
    break;

  case 202:
#line 1935 "./lyparser.y"
    {
	(yyval.f) = default_duration_;
	;}
    break;

  case 203:
#line 1938 "./lyparser.y"
    {
		(yyval.f) = (yyvsp[(1) - (1)].f);
		 default_duration_.t1.a = (yyvsp[(1) - (1)].f).t1.a;
		 default_duration_.t1.b = (yyvsp[(1) - (1)].f).t1.b;
	;}
    break;

  case 204:
#line 1943 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.f) = (yyvsp[(1) - (1)].f);
		THIS->default_duration_ = *unsmob_duration ((yyval.f));
#endif
	;}
    break;

  case 205:
#line 1953 "./lyparser.y"
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

  case 206:
#line 1964 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		Duration *d =unsmob_duration ((yyvsp[(1) - (2)].id));
		Duration k (d->duration_log (),d->dot_count () + (yyvsp[(2) - (2)].i));
		(yyval.f) = k.smobbed_copy ();
#endif
	;}
    break;

  case 207:
#line 1978 "./lyparser.y"
    { /* note 4 integers are used for these */
		(yyvsp[(1) - (1)].f).t2.a = 1;
		(yyvsp[(1) - (1)].f).t2.b = 1;
		(yyval.f) = (yyvsp[(1) - (1)].f);
	;}
    break;

  case 208:
#line 1983 "./lyparser.y"
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

  case 209:
#line 1993 "./lyparser.y"
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

  case 210:
#line 2009 "./lyparser.y"
    { (yyval.t) = (yyvsp[(1) - (1)].t); ;}
    break;

  case 211:
#line 2010 "./lyparser.y"
    {

		(yyval.t).user_string =  g_strconcat((yyvsp[(1) - (3)].i).user_string, (yyvsp[(2) - (3)].generic).user_string, (yyvsp[(3) - (3)].i).user_string, NULL);
		(yyval.t).t.a = (yyvsp[(1) - (3)].i).i;
		(yyval.t).t.b = (yyvsp[(3) - (3)].i).i;
	;}
    break;

  case 212:
#line 2019 "./lyparser.y"
    { (yyval.i).i = 0; ;}
    break;

  case 213:
#line 2020 "./lyparser.y"
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

  case 214:
#line 2033 "./lyparser.y"
    {
		(yyval.i).i = 0;
	;}
    break;

  case 215:
#line 2036 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		if (!is_duration_b ((yyvsp[(2) - (2)].i)))
			THIS->parser_error (_f ("not a duration: %d", (yyvsp[(2) - (2)].i)));
		(yyval.i) = (yyvsp[(2) - (2)].i);
#endif
	;}
    break;

  case 216:
#line 2103 "./lyparser.y"
    { (yyval.i).i = 0; ;}
    break;

  case 217:
#line 2104 "./lyparser.y"
    { (yyval.i).i = 1; ;}
    break;

  case 218:
#line 2110 "./lyparser.y"
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

  case 219:
#line 2152 "./lyparser.y"
    {
	/* denemo wants a chord with no notes */
	DenemoObject *mud = newchord( (yyvsp[(2) - (2)].f).t1.a, (yyvsp[(2) - (2)].f).t1.b,0);

	mud->user_string = g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].f).user_string, NULL);
	(yyval.scm) = g_list_append(NULL,mud); 
	;}
    break;

  case 220:
#line 2159 "./lyparser.y"
    {
	/* denemo wants a chord with no notes */
	DenemoObject *mud = newchord( (yyvsp[(2) - (2)].f).t1.a, (yyvsp[(2) - (2)].f).t1.b,0);
	mud->type = SKIPNAME;
	mud->user_string = g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].f).user_string, NULL);
	(yyval.scm) = g_list_append(NULL,mud); 
	;}
    break;

  case 221:
#line 2166 "./lyparser.y"
    {
	/* treat as skip for the moment */
	DenemoObject *mud = newchord( (yyvsp[(2) - (2)].f).t1.a, (yyvsp[(2) - (2)].f).t1.b,0);
	mud->type = SKIPNAME;
	mud->user_string = g_strconcat((yyvsp[(1) - (2)].generic).user_string, (yyvsp[(2) - (2)].f).user_string, NULL);
	(yyval.scm) = g_list_append(NULL,mud); 

	;}
    break;

  case 222:
#line 2174 "./lyparser.y"
    {
		DenemoObject *mud = newlyric((yyvsp[(2) - (2)].f).t1.a, (yyvsp[(2) - (2)].f).t1.b, (yyvsp[(1) - (2)].gstr).gstr->str);
		mud->user_string = (yyvsp[(1) - (2)].gstr).user_string;
	        mud->type = LYRICS;
		stradd((*mud),(yyvsp[(2) - (2)].f));
		(yyval.scm) = g_list_append(NULL,mud);
	;}
    break;

  case 223:
#line 2181 "./lyparser.y"
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

  case 226:
#line 2201 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
                (yyval.music) = Chord::get_chord ((yyvsp[(1) - (6)].n), (yyvsp[(3) - (6)].scm), (yyvsp[(4) - (6)].scm), (yyvsp[(5) - (6)].scm), (yyvsp[(6) - (6)].scm), (yyvsp[(2) - (6)].f));
		(yyval.music)->set_spot (THIS->here_input ());
#endif
        ;}
    break;

  case 227:
#line 2210 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = SCM_EOL;
#endif
	;}
    break;

  case 228:
#line 2216 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(2) - (2)].scm);
#endif
	;}
    break;

  case 229:
#line 2225 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
	;}
    break;

  case 230:
#line 2228 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_append2 ((yyval.scm), (yyvsp[(3) - (3)].scm));
#endif
	;}
    break;

  case 231:
#line 2237 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = SCM_EOL;
#endif
	;}
    break;

  case 232:
#line 2243 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(2) - (2)].scm);
	;}
    break;

  case 233:
#line 2250 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = SCM_EOL;
#endif
	;}
    break;

  case 234:
#line 2256 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(2) - (2)].n);
#endif
	;}
    break;

  case 235:
#line 2265 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = SCM_EOL;
#endif
	;}
    break;

  case 236:
#line 2271 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(2) - (2)].n);
#endif
	;}
    break;

  case 237:
#line 2280 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_cons ((yyvsp[(1) - (1)].scm), SCM_EOL);
#endif
	;}
    break;

  case 238:
#line 2286 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_cons (unsmob_pitch ((yyvsp[(1) - (1)].t))->smobbed_copy (), SCM_EOL);
#endif
	;}
    break;

  case 239:
#line 2292 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
	 /* Ugh. */
		(yyval.scm) = scm_list_n (unsmob_pitch ((yyvsp[(1) - (2)].t))->smobbed_copy (),
			(yyvsp[(2) - (2)].scm), SCM_UNDEFINED);
#endif
	;}
    break;

  case 240:
#line 2303 "./lyparser.y"
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

  case 241:
#line 2314 "./lyparser.y"
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

  case 242:
#line 2326 "./lyparser.y"
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

  case 243:
#line 2343 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_sum ((yyvsp[(1) - (3)].scm), (yyvsp[(3) - (3)].scm));
#endif
	;}
    break;

  case 244:
#line 2349 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_difference ((yyvsp[(1) - (3)].scm), (yyvsp[(3) - (3)].scm));
#endif
	;}
    break;

  case 246:
#line 2359 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(1) - (1)].scm);
#endif
	;}
    break;

  case 247:
#line 2365 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_product ((yyvsp[(1) - (3)].scm), (yyvsp[(3) - (3)].scm));
#endif
	;}
    break;

  case 248:
#line 2371 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_divide ((yyvsp[(1) - (3)].scm), (yyvsp[(3) - (3)].scm));
#endif
	;}
    break;

  case 249:
#line 2380 "./lyparser.y"
    {
		(yyval.scm) = (yyvsp[(2) - (3)].scm);
	;}
    break;

  case 250:
#line 2383 "./lyparser.y"
    { /* %prec UNARY_MINUS */
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = scm_difference ((yyvsp[(2) - (2)].scm), SCM_UNDEFINED);
#endif
	;}
    break;

  case 252:
#line 2394 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_int2scm ((yyvsp[(1) - (1)].i));
#endif
	;}
    break;

  case 253:
#line 2400 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(1) - (1)].r);
#endif
	;}
    break;

  case 254:
#line 2406 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = (yyvsp[(1) - (1)].id);
#endif
	;}
    break;

  case 255:
#line 2412 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_double2scm (gh_scm2double ((yyvsp[(1) - (2)].r)) CM );
#endif
	;}
    break;

  case 256:
#line 2418 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_double2scm (gh_scm2double ((yyvsp[(1) - (2)].r)) PT);
#endif
	;}
    break;

  case 257:
#line 2424 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_double2scm (gh_scm2double ((yyvsp[(1) - (2)].r)) INCH);
#endif
	;}
    break;

  case 258:
#line 2430 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_double2scm (gh_scm2double ((yyvsp[(1) - (2)].r)) MM);
#endif
	;}
    break;

  case 259:
#line 2436 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.scm) = gh_double2scm (gh_scm2double ((yyvsp[(1) - (2)].r)) CHAR);
#endif
	;}
    break;

  case 260:
#line 2446 "./lyparser.y"
    {
			(yyval.i) = (yyvsp[(1) - (1)].i);
	;}
    break;

  case 261:
#line 2449 "./lyparser.y"
    {
		(yyval.i) = (yyvsp[(1) - (1)].i);
	;}
    break;

  case 262:
#line 2455 "./lyparser.y"
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

  case 263:
#line 2469 "./lyparser.y"
    {
		(yyval.i).i = -(yyvsp[(2) - (2)].i).i;
	;}
    break;

  case 264:
#line 2476 "./lyparser.y"
    {
		(yyval.gstr) = (yyvsp[(1) - (1)].gstr);
	;}
    break;

  case 265:
#line 2479 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.gstr) = (yyvsp[(1) - (1)].id);
#endif
	;}
    break;

  case 266:
#line 2485 "./lyparser.y"
    {
LATER_MESSAGE((yyloc).first_line);
#ifdef LATER
		(yyval.gstr) = scm_string_append (scm_list_n ((yyvsp[(1) - (3)].gstr), (yyvsp[(3) - (3)].gstr), SCM_UNDEFINED));
#endif
	;}
    break;

  case 267:
#line 2495 "./lyparser.y"
    { (yyval.i).i = 0; ;}
    break;

  case 268:
#line 2496 "./lyparser.y"
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

  case 269:
#line 2509 "./lyparser.y"
    { (yyval.i).i = 0; ;}
    break;

  case 270:
#line 2510 "./lyparser.y"
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
#line 5228 "lyparser.tab.c"
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


#line 2523 "./lyparser.y"


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
	  /* free_score (si); CHANGE THIS TO FREE ALL THE si in 
	     si->thefile->currentDenemoScore list... */

#if 1
	  // Calling init_score here would cause piece in header to set to "Movement 2"
  	  si->measurewidths = NULL;
	  si->thescore = NULL;
#endif	  

	  /* in case we are re-entering via reload after error */
	  lyrestart (lyin);	

	  lylineno = 1;		/* not done by lexer for some reason */
	  push_note_state ();

	  while (!feof (lyin))
	    {
	      lyparse ();
	    }
	 if(1)
	    {
	      GList *score = findtok (lily_file, SCORE);
	      if (score)
		if (create_score_from_lily (si, br (score)) == 0)
		  {
		    GList *top;
		    score_prop_from_lily(gui);
		    fixup_measure_widths_and_contexts (si);
		    while (score && score->next)
		      {
			score = findtok (score->next, SCORE);
			if (score)
			  {
	                    insert_movement_after(NULL);
			    DenemoScore *nextsi =
			      Denemo.gui->si;
			    init_score (nextsi, gui);
		 	    create_score_from_lily (nextsi, br (score));
		            fixup_measure_widths_and_contexts (nextsi);
			    
			  }
		      }

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

