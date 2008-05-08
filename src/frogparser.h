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
     STAFFNAME = 258,
     TIE = 259,
     END = 260,
     FLOAT = 261,
     NOTENAME = 262,
     DURATION = 263,
     RESTTYPE = 264,
     DIRECTION = 265,
     SLURPOSITION = 266,
     NUMPOSITION = 267,
     KEYTYPE = 268,
     NUM = 269,
     BRACKET = 270,
     CLEFTYPE = 271,
     ORNAMENT = 272,
     FULL = 273,
     PARTIAL_ = 274,
     MODE = 275,
     BARTYPE = 276,
     TEMPOTERM = 277,
     TRANSTEMPO = 278,
     DYN = 279,
     ACCIDENTAL = 280,
     ACCENT = 281,
     NOTEHEAD = 282,
     STYLE = 283,
     CURVESHAPE = 284,
     SYSTEM = 285,
     STAFFGROUP = 286,
     STAFF = 287,
     ENDSTAFFGROUP = 288,
     TUPLET = 289,
     TUP_END = 290,
     SLURTYPE = 291,
     HAIRPINTYPE = 292
   };
#endif
/* Tokens.  */
#define STAFFNAME 258
#define TIE 259
#define END 260
#define FLOAT 261
#define NOTENAME 262
#define DURATION 263
#define RESTTYPE 264
#define DIRECTION 265
#define SLURPOSITION 266
#define NUMPOSITION 267
#define KEYTYPE 268
#define NUM 269
#define BRACKET 270
#define CLEFTYPE 271
#define ORNAMENT 272
#define FULL 273
#define PARTIAL_ 274
#define MODE 275
#define BARTYPE 276
#define TEMPOTERM 277
#define TRANSTEMPO 278
#define DYN 279
#define ACCIDENTAL 280
#define ACCENT 281
#define NOTEHEAD 282
#define STYLE 283
#define CURVESHAPE 284
#define SYSTEM 285
#define STAFFGROUP 286
#define STAFF 287
#define ENDSTAFFGROUP 288
#define TUPLET 289
#define TUP_END 290
#define SLURTYPE 291
#define HAIRPINTYPE 292




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 63 "./frogparser.y"
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
/* Line 1489 of yacc.c.  */
#line 144 "frogparser.tab.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE froglval;

