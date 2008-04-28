/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

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
/* Line 1489 of yacc.c.  */
#line 335 "lyparser.tab.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
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
