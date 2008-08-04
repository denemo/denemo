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
#line 337 "lyparser.tab.h"
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
