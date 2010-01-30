
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 203 "./lyparser.y"

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



/* Line 1676 of yacc.c  */
#line 208 "lyparser.tab.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE lylval;

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

extern YYLTYPE lylloc;

