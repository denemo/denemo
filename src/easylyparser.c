
/*  A Bison parser, made from ./easylyparser.y
    by GNU Bison version 1.28  */

#define YYBISON 1		/* Identify Bison output.  */

#define yyparse lyparse
#define yylex lylex
#define yyerror lyerror
#define yylval lylval
#define yychar lychar
#define yydebug lydebug
#define yynerrs lynerrs
#define	STAFFBEGIN	257
#define	STAFFEND	258
#define	TIMESIGTOK	259
#define	KEYSIGTOK	260
#define	CLEFTOK	261
#define	INSTRUMENT	262
#define	BEGINCHORD	263
#define	ENDCHORD	264
#define	TONETYPE	265
#define	ENSHIFT	266
#define	OCTIND	267
#define	BASEDURATION	268
#define	NUMDOTS	269
#define	MINOR	270
#define	MAJOR	271
#define	BLINE	272
#define	BEGINTUPLET	273
#define	ENDTUPLET	274
#define	STEMDIRECTIVETOK	275
#define	RELOADDIRECTIVE	276
#define	DYNAMICTOK	277
#define	SLUR_START	278
#define	TIE	279
#define	STARTGRACE	280
#define	ENDGRACE	281
#define	LYTONEOPTION	282
#define	SLUR_END	283

#line 8 "./easylyparser.y"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include "calculatepositions.h"
#include "chordops.h"
#include "contexts.h"
#include "lyparserfuncs.h"
#include "measureops.h"
#include "objops.h"
#include "scoreops.h"
#include "staffops.h"
#include "tupletops.h"
#include "graceops.h"

int lylex ();
gint lyerror (gchar *);

struct scoreinfo *lysi;
FILE *lyin;			/* Global filehandle */
mudelaobject *newmudelaobj = NULL;
struct twoints prevduration;
enum clefs currentclef;
staff *curstaffstruct;



#line 37 "./easylyparser.y"
typedef union
{
  gint a;
  gchar c;
  gchar *strval;
  struct twoints t;
}
YYSTYPE;
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		75
#define	YYFLAG		-32768
#define	YYNTBASE	30

#define YYTRANSLATE(x) ((unsigned)(x) <= 283 ? yytranslate[x] : 55)

static const char yytranslate[] = { 0,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 1, 3, 4, 5, 6,
  7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
  17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
  27, 28, 29
};

#if YYDEBUG != 0
static const short yyprhs[] = { 0,
  0, 1, 4, 5, 14, 15, 17, 18, 20, 21,
  26, 29, 30, 33, 35, 37, 39, 41, 43, 45,
  47, 49, 52, 54, 57, 62, 63, 65, 66, 68,
  69, 71, 72, 77, 78, 81, 82, 85, 91, 96,
  101, 106, 110, 114, 118, 121, 122, 124, 127, 129,
  130, 135, 136
};

static const short yyrhs[] = { -1,
  31, 30, 0, 0, 3, 34, 33, 5, 39, 7,
  32, 35, 0, 0, 8, 0, 0, 22, 0, 0,
  37, 18, 36, 35, 0, 37, 4, 0, 0, 38,
  37, 0, 40, 0, 7, 0, 39, 0, 5, 0,
  51, 0, 53, 0, 21, 0, 23, 0, 6, 16,
  0, 6, 0, 6, 17, 0, 41, 44, 42, 43,
  0, 0, 29, 0, 0, 24, 0, 0, 25, 0,
  0, 9, 45, 47, 10, 0, 0, 46, 48, 0,
  0, 48, 47, 0, 11, 12, 13, 50, 49, 0,
  11, 13, 50, 49, 0, 11, 12, 50, 49, 0,
  11, 12, 13, 49, 0, 11, 12, 49, 0, 11,
  13, 49, 0, 11, 50, 49, 0, 11, 49, 0,
  0, 28, 0, 14, 15, 0, 14, 0, 0, 19,
  52, 37, 20, 0, 0, 26, 54, 37, 27, 0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
  84, 85, 88, 104, 106, 107, 110, 111, 114, 121,
  121, 124, 125, 128, 129, 135, 143, 149, 150, 151,
  157, 175, 176, 177, 180, 189, 190, 193, 194, 197,
  198, 201, 205, 206, 213, 215, 216, 219, 224, 229,
  234, 238, 242, 246, 252, 258, 259, 265, 268, 273,
  277, 284, 288
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char *const yytname[] =
  { "$", "error", "$undefined.", "STAFFBEGIN",
  "STAFFEND", "TIMESIGTOK", "KEYSIGTOK", "CLEFTOK", "INSTRUMENT",
  "BEGINCHORD", "ENDCHORD",
  "TONETYPE", "ENSHIFT", "OCTIND", "BASEDURATION", "NUMDOTS", "MINOR",
  "MAJOR", "BLINE",
  "BEGINTUPLET", "ENDTUPLET", "STEMDIRECTIVETOK", "RELOADDIRECTIVE",
  "DYNAMICTOK",
  "SLUR_START", "TIE", "STARTGRACE", "ENDGRACE", "LYTONEOPTION", "SLUR_END",
  "staffs",
  "staff", "@1", "instrument", "reloaddirective", "measures", "@2",
  "mudelaobjects",
  "mudelaobject", "keysig", "chordandassoc", "slur_end_option",
  "slur_begin_option",
  "tie_option", "chord", "@3", "@4", "tones", "tone", "tone_option",
  "duration", "tuplet",
  "@5", "grace", "@6", NULL
};
#endif

static const short yyr1[] = { 0,
  30, 30, 32, 31, 33, 33, 34, 34, 36, 35,
  35, 37, 37, 38, 38, 38, 38, 38, 38, 38,
  38, 39, 39, 39, 40, 41, 41, 42, 42, 43,
  43, 45, 44, 46, 44, 47, 47, 48, 48, 48,
  48, 48, 48, 48, 48, 49, 49, 50, 50, 52,
  51, 54, 53
};

static const short yyr2[] = { 0,
  0, 2, 0, 8, 0, 1, 0, 1, 0, 4,
  2, 0, 2, 1, 1, 1, 1, 1, 1, 1,
  1, 2, 1, 2, 4, 0, 1, 0, 1, 0,
  1, 0, 4, 0, 2, 0, 2, 5, 4, 4,
  4, 3, 3, 3, 2, 0, 1, 2, 1, 0,
  4, 0, 4
};

static const short yydefact[] = { 1,
  7, 1, 8, 5, 2, 6, 0, 0, 23, 0,
  22, 24, 3, 12, 17, 15, 50, 20, 21, 52,
  27, 4, 0, 12, 16, 14, 34, 18, 19, 26,
  26, 11, 9, 13, 32, 28, 0, 0, 0, 12,
  36, 29, 30, 46, 35, 51, 53, 10, 0, 36,
  31, 25, 46, 46, 49, 47, 45, 46, 33, 37,
  46, 42, 46, 43, 46, 48, 44, 41, 46, 40,
  39, 38, 0, 0, 0
};

static const short yydefgoto[] = { 5,
  2, 14, 7, 4, 22, 40, 23, 24, 25, 26,
  27, 43, 52, 36, 41, 37, 49, 50, 57, 58,
  28, 30, 29, 31
};

static const short yypact[] = { 12,
  -2, 12, -32768, 15, -32768, -32768, 23, 24, -7, 36,
  -32768, -32768, -32768, -5, -32768, -32768, -32768, -32768, -32768, -32768,
  -32768, -32768, -1, -5, -32768, -32768, 22, -32768, -32768, 6,
  31, -32768, -32768, -32768, -32768, 18, 34, 26, 21, -5,
  34, -32768, 28, 49, -32768, -32768, -32768, -32768, 46, 34,
  -32768, -32768, -6, -9, 52, -32768, -32768, 40, -32768, -32768,
  -9, -32768, 40, -32768, 40, -32768, -32768, -32768, 40, -32768,
  -32768, -32768, 64, 69, -32768
};

static const short yypgoto[] = { 70,
  -32768, -32768, -32768, -32768, 32, -32768, 35, -32768, 63, -32768,
  -32768, -32768, -32768, -32768, -32768, -32768, 25, 37, -14, -20,
  -32768, -32768, -32768, -32768
};


#define	YYLAST		77


static const short yytable[] = { 15,
  9, 16, 32, -26, 55, -26, 61, 55, 11, 12,
  15, 9, 16, 17, 1, 18, 33, 19, 56, 3,
  20, 56, 6, 21, 17, -12, 18, 8, 19, 9,
  35, 20, 63, 65, 21, 15, 9, 16, 62, 64,
  69, 42, 13, 67, 44, 46, 68, 47, 70, 17,
  71, 18, 51, 19, 72, 59, 20, -12, 34, 21,
  53, 54, 55, 74, 38, 39, 66, 56, 75, 73,
  10, 48, 0, 45, 60, 0, 56
};

static const short yycheck[] = { 5,
  6, 7, 4, 9, 14, 11, 13, 14, 16, 17,
  5, 6, 7, 19, 3, 21, 18, 23, 28, 22,
  26, 28, 8, 29, 19, 20, 21, 5, 23, 6,
  9, 26, 53, 54, 29, 5, 6, 7, 53, 54,
  61, 24, 7, 58, 11, 20, 61, 27, 63, 19,
  65, 21, 25, 23, 69, 10, 26, 27, 24, 29,
  12, 13, 14, 0, 30, 31, 15, 28, 0, 0,
  8, 40, -1, 37, 50, -1, 28
};

/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/share/bison/bison.simple"
/* This file comes from bison-1.28.  */

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

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
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

#ifndef YYSTACK_USE_ALLOCA
#ifdef alloca
#define YYSTACK_USE_ALLOCA
#else /* alloca not defined */
#ifdef __GNUC__
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi) || (defined (__sun) && defined (__i386))
#define YYSTACK_USE_ALLOCA
#include <alloca.h>
#else /* not sparc */
/* We think this test detects Watcom and Microsoft C.  */
/* This used to test MSDOS, but that is a bad idea
   since that symbol is in the user namespace.  */
#if (defined (_MSDOS) || defined (_MSDOS_)) && !defined (__TURBOC__)
#if 0				/* No need for malloc.h, which pollutes the namespace;
				   instead, just don't use alloca.  */
#include <malloc.h>
#endif
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
/* I don't know what this was needed for, but it pollutes the namespace.
   So I turned it off.   rms, 2 May 1997.  */
/* #include <malloc.h>  */
#pragma alloca
#define YYSTACK_USE_ALLOCA
#else /* not MSDOS, or __TURBOC__, or _AIX */
#if 0
#ifdef __hpux			/* haible@ilog.fr says this works for HPUX 9.05 and up,
				   and on HPUX 10.  Eventually we can turn this on.  */
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#endif /* __hpux */
#endif
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc */
#endif /* not GNU C */
#endif /* alloca not defined */
#endif /* YYSTACK_USE_ALLOCA not defined */

#ifdef YYSTACK_USE_ALLOCA
#define YYSTACK_ALLOC alloca
#else
#define YYSTACK_ALLOC malloc
#endif

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int yychar;			/*  the lookahead symbol                */
YYSTYPE yylval;			/*  the semantic value of the           */
				/*  lookahead symbol                    */

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead     */
				/*  symbol                              */
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace     */
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Define __yy_memcpy.  Note that the size argument
   should be passed with type unsigned int, because that is what the non-GCC
   definitions require.  With GCC, __builtin_memcpy takes an arg
   of type size_t, but it can handle unsigned int.  */

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else /* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (to, from, count)
     char *to;
     char *from;
     unsigned int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (char *to, char *from, unsigned int count)
{
  register char *t = to;
  register char *f = from;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 217 "/usr/share/bison/bison.simple"

/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
#ifdef YYPARSE_PARAM
int yyparse (void *);
#else
int yyparse (void);
#endif
#endif

int
yyparse (YYPARSE_PARAM_ARG)
  YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;		/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short yyssa[YYINITDEPTH];	/*  the state stack                     */
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack            */

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack                  */
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;
  int yyfree_stacks = 0;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return         */
  /*  semantic values from the action     */
  /*  routines                            */

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf (stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
         the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
         but that might be undefined if yyoverflow is a macro.  */
      yyoverflow ("parser stack overflow",
		  &yyss1, size * sizeof (*yyssp),
		  &yyvs1, size * sizeof (*yyvsp),
		  &yyls1, size * sizeof (*yylsp), &yystacksize);
#else
      yyoverflow ("parser stack overflow",
		  &yyss1, size * sizeof (*yyssp),
		  &yyvs1, size * sizeof (*yyvsp), &yystacksize);
#endif

      yyss = yyss1;
      yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror ("parser stack overflow");
	  if (yyfree_stacks)
	    {
	      free (yyss);
	      free (yyvs);
#ifdef YYLSP_NEEDED
	      free (yyls);
#endif
	    }
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
#ifndef YYSTACK_USE_ALLOCA
      yyfree_stacks = 1;
#endif
      yyss = (short *) YYSTACK_ALLOC (yystacksize * sizeof (*yyssp));
      __yy_memcpy ((char *) yyss, (char *) yyss1,
		   size * (unsigned int) sizeof (*yyssp));
      yyvs = (YYSTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *) yyvs, (char *) yyvs1,
		   size * (unsigned int) sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *) yyls, (char *) yyls1,
		   size * (unsigned int) sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf (stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf (stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf (stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf (stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE (yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
     New state is final state => don't bother to shift,
     just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf (stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1 - yylen];	/* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ", yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn)
    {

    case 3:
#line 88 "./easylyparser.y"
      {
	lysi->currentstaffnum++;
	newstaff (lysi, ADDFROMLOAD, NULL);
	lysi->currentstaff = g_list_last (lysi->thescore);
	curstaffstruct = (staff *) lysi->currentstaff->data;
	setstaffname (lysi, yyvsp[-5].strval);
	if (yyvsp[-4].a)
	  curstaffstruct->voicenumber = 2;
	set_clef (lysi, yyvsp[0].strval);
	currentclef = (enum clefs) curstaffstruct->sclef;
	set_key (lysi, yyvsp[-1].t);
	set_time (lysi, yyvsp[-2].t);
	find_leftmost_staffcontext ((staff *) lysi->currentstaff->data, lysi);
	g_string_assign (curstaffstruct->midi_instrument, yyvsp[-3].strval);
	g_free (yyvsp[-3].strval);
	;
	break;
      }
    case 5:
#line 106 "./easylyparser.y"
      {
	yyval.strval = g_strdup ("acoustic grand");
	break;
      }
    case 6:
#line 107 "./easylyparser.y"
      {
	yyval.strval = yyvsp[0].strval;
	break;
      }
    case 7:
#line 110 "./easylyparser.y"
      {
	yyval.a = FALSE;
	break;
      }
    case 8:
#line 111 "./easylyparser.y"
      {
	yyval.a = TRUE;
	break;
      }
    case 9:
#line 114 "./easylyparser.y"
      {
	if (!lysi->currentmeasure->next)
	  lysi->currentmeasure =
	    dnm_addmeasures (lysi, lysi->currentmeasurenum, 1);
	else
	  lysi->currentmeasure = lysi->currentmeasure->next;
	lysi->currentmeasurenum++;
	;
	break;
      }
    case 15:
#line 129 "./easylyparser.y"
      {
	currentclef = cleftypefromname (yyvsp[0].strval);
	newmudelaobj = newclefobj (currentclef);
	lysi->currentmeasure->data =
	  g_list_append ((objnode *) lysi->currentmeasure->data,
			 newmudelaobj);
	;
	break;
      }
    case 16:
#line 135 "./easylyparser.y"
      {
	if (yyvsp[0].t.b)
	  newmudelaobj = dnm_newkeyobj (yyvsp[0].t.a - 3, yyvsp[0].t.b);
	else
	  newmudelaobj = dnm_newkeyobj (yyvsp[0].t.a, yyvsp[0].t.b);
	lysi->currentmeasure->data =
	  g_list_append ((objnode *) lysi->currentmeasure->data,
			 newmudelaobj);
	;
	break;
      }
    case 17:
#line 143 "./easylyparser.y"
      {
	newmudelaobj = newtimesigobj (yyvsp[0].t.a, yyvsp[0].t.b);
	lysi->currentmeasure->data =
	  g_list_append ((objnode *) lysi->currentmeasure->data,
			 newmudelaobj);
	;
	break;
      }
    case 20:
#line 151 "./easylyparser.y"
      {
	newmudelaobj = stem_directive_new ((enum stemdirections) yyvsp[0].a);
	lysi->currentmeasure->data =
	  g_list_append ((objnode *) lysi->currentmeasure->data,
			 newmudelaobj);
	;
	break;
      }
    case 21:
#line 157 "./easylyparser.y"
      {
	GString *dynamic = g_string_new (yyvsp[0].strval);

	mudelaobject *curobj =
	  (mudelaobject *) (lysi->currentobject ? lysi->currentobject->
			    data : NULL);

	if (curobj->type == CHORD && curobj)
	  {
	    ((chord *) curobj->object)->dynamics =
	      g_list_append (((chord *) curobj->object)->dynamics, dynamic);
	  }
	/*newmudelaobj = dynamic_new ($1);
	   lysi->currentmeasure->data = 
	   g_list_append ((objnode *)lysi->currentmeasure->data,
	   newmudelaobj); */
	g_free (yyvsp[0].strval);
	;
	break;
      }
    case 22:
#line 175 "./easylyparser.y"
      {
	yyval.t = twointer (keynametonumber (yyvsp[-1].strval), TRUE);
	break;
      }
    case 23:
#line 176 "./easylyparser.y"
      {
	yyval.t = twointer (keynametonumber (yyvsp[0].strval), FALSE);
	break;
      }
    case 24:
#line 177 "./easylyparser.y"
      {
	yyval.t = twointer (keynametonumber (yyvsp[-1].strval), FALSE);
	break;
      }
    case 25:
#line 181 "./easylyparser.y"
      {
	((chord *) newmudelaobj->object)->slur_end_p = yyvsp[-3].a;
	((chord *) newmudelaobj->object)->slur_begin_p = yyvsp[-1].a;
	((chord *) newmudelaobj->object)->is_tied = yyvsp[0].a;

	;
	break;
      }
    case 26:
#line 189 "./easylyparser.y"
      {
	yyval.a = FALSE;
	break;
      }
    case 27:
#line 190 "./easylyparser.y"
      {
	yyval.a = TRUE;
	break;
      }
    case 28:
#line 193 "./easylyparser.y"
      {
	yyval.a = FALSE;
	break;
      }
    case 29:
#line 194 "./easylyparser.y"
      {
	yyval.a = TRUE;
	break;
      }
    case 30:
#line 197 "./easylyparser.y"
      {
	yyval.a = FALSE;
	break;
      }
    case 31:
#line 198 "./easylyparser.y"
      {
	yyval.a = TRUE;
	break;
      }
    case 32:
#line 201 "./easylyparser.y"
      {
	newmudelaobj = newchord (0, 0, 0);
	lysi->currentmeasure->data =
	  g_list_append ((objnode *) lysi->currentmeasure->data,
			 newmudelaobj);
	;
	break;
      }
    case 34:
#line 206 "./easylyparser.y"
      {
	newmudelaobj = newchord (0, 0, 0);
	lysi->currentmeasure->data =
	  g_list_append ((objnode *) lysi->currentmeasure->data,
			 newmudelaobj);

	;
	break;
      }
    case 38:
#line 219 "./easylyparser.y"
      {
	addtonewrapper (newmudelaobj, yyvsp[-4].c, yyvsp[-3].a, yyvsp[-2].a,
			yyvsp[-1].t, currentclef);
	set_tone_option (newmudelaobj, yyvsp[0].strval);
	prevduration = yyvsp[-1].t;
	;
	break;
      }
    case 39:
#line 224 "./easylyparser.y"
      {
	addtonewrapper (newmudelaobj, yyvsp[-3].c, 0, yyvsp[-2].a,
			yyvsp[-1].t, currentclef);
	set_tone_option (newmudelaobj, yyvsp[0].strval);
	prevduration = yyvsp[-1].t;
	;
	break;
      }
    case 40:
#line 229 "./easylyparser.y"
      {
	addtonewrapper (newmudelaobj, yyvsp[-3].c, yyvsp[-2].a, 0,
			yyvsp[-1].t, currentclef);
	set_tone_option (newmudelaobj, yyvsp[0].strval);
	prevduration = yyvsp[-1].t;
	;
	break;
      }
    case 41:
#line 234 "./easylyparser.y"
      {
	addtonewrapper (newmudelaobj, yyvsp[-3].c, yyvsp[-2].a, yyvsp[-1].a,
			prevduration, currentclef);
	set_tone_option (newmudelaobj, yyvsp[0].strval);
	;
	break;
      }
    case 42:
#line 238 "./easylyparser.y"
      {
	addtonewrapper (newmudelaobj, yyvsp[-2].c, yyvsp[-1].a, 0,
			prevduration, currentclef);
	set_tone_option (newmudelaobj, yyvsp[0].strval);
	;
	break;
      }
    case 43:
#line 242 "./easylyparser.y"
      {
	addtonewrapper (newmudelaobj, yyvsp[-2].c, 0, yyvsp[-1].a,
			prevduration, currentclef);
	set_tone_option (newmudelaobj, yyvsp[0].strval);
	;
	break;
      }
    case 44:
#line 246 "./easylyparser.y"
      {
	addtonewrapper (newmudelaobj, yyvsp[-2].c, 0, 0, yyvsp[-1].t,
			currentclef);
	set_tone_option (newmudelaobj, yyvsp[0].strval);

	prevduration = yyvsp[-1].t;
	;
	break;
      }
    case 45:
#line 252 "./easylyparser.y"
      {
	addtonewrapper (newmudelaobj, yyvsp[-1].c, 0, 0, prevduration,
			currentclef);
	set_tone_option (newmudelaobj, yyvsp[0].strval);
	;
	break;
      }
    case 46:
#line 258 "./easylyparser.y"
      {
	yyval.strval = "";;
	break;
      }
    case 47:
#line 259 "./easylyparser.y"
      {
	yyval.strval = yyvsp[0].strval;
	;
	break;
      }
    case 48:
#line 265 "./easylyparser.y"
      {
	yyval.t = twointer (mutointernalduration (yyvsp[-1].a), yyvsp[0].a);
	;
	break;
      }
    case 49:
#line 268 "./easylyparser.y"
      {
	yyval.t = twointer (mutointernalduration (yyvsp[0].a), 0);
	;
	break;
      }
    case 50:
#line 273 "./easylyparser.y"
      {
	newmudelaobj = newtupopen (yyvsp[0].t.a, yyvsp[0].t.b);
	lysi->currentmeasure->data =
	  g_list_append ((objnode *) lysi->currentmeasure->data,
			 newmudelaobj);
	;
	break;
      }
    case 51:
#line 277 "./easylyparser.y"
      {
	newmudelaobj = newtupclose ();
	lysi->currentmeasure->data =
	  g_list_append ((objnode *) lysi->currentmeasure->data,
			 newmudelaobj);
	;
	break;
      }
    case 52:
#line 284 "./easylyparser.y"
      {
	newmudelaobj = newgracestart ();
	lysi->currentmeasure->data =
	  g_list_append ((objnode *) lysi->currentmeasure->data,
			 newmudelaobj);
	;
	break;
      }
    case 53:
#line 288 "./easylyparser.y"
      {
	newmudelaobj = newgraceend ();
	lysi->currentmeasure->data =
	  g_list_append ((objnode *) lysi->currentmeasure->data,
			 newmudelaobj);
	;
	break;
      }
    }
  /* the action file gets copied in in place of this dollarsign */
#line 543 "/usr/share/bison/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp - 1)->last_line;
      yylsp->last_column = (yylsp - 1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp + yylen - 1)->last_line;
      yylsp->last_column = (yylsp + yylen - 1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:			/* here on detecting error */

  if (!yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof (yytname) / sizeof (char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen (yytname[x]) + 15, count++;
	  msg = (char *) malloc (size + 15);
	  if (msg != 0)
	    {
	      strcpy (msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof (yytname) / sizeof (char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat (msg, count == 0 ? ", expecting `" : " or `");
			strcat (msg, yytname[x]);
			strcat (msg, "'");
			count++;
		      }
		}
	      yyerror (msg);
	      free (msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror ("parse error");
    }

  goto yyerrlab1;
yyerrlab1:			/* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf (stderr, "Discarding token %d (%s).\n", yychar,
		 yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:			/* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];	/* If its default is to accept any token, ok.  Otherwise pop it. */
  if (yyn)
    goto yydefault;
#endif

yyerrpop:			/* pop the current state because it cannot handle the error token */

  if (yyssp == yyss)
    YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf (stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;

yyacceptlab:
  /* YYACCEPT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 0;

yyabortlab:
  /* YYABORT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 1;
}

#line 294 "./easylyparser.y"


/* Both of the otherwise infinite loops in these functions are exited when the
 * lylex returns (i.e., my ad-hoc lexer finds a token) */

int
lyinput (gchar * filename, struct scoreinfo *si)
{
  if ((lyin = fopen (filename, "r")) == NULL)
    {
      fprintf (stderr, "Cannot open the file: %s\n", filename);
      return -1;
    }
  else
    {
      lysi = si;
      free_score (lysi);
      lysi->currentstaffnum = 0;
      while (!feof (lyin))
	{
	  lyparse ();
	}
      fclose (lyin);
      return 0;
    }
}

/* This is an ad-hoc lexer. It really needs to be rewritten in
 * some other way - probably with the help of flex, though I
 * was hoping to avoid this. The problem is it's very much a house
 * of cards right now, and doesn't handle standard mudela block-comments
 * correctly (though it does one-liners properly) */

int
lylex ()
{
  gchar scannedchars[2];
  gchar lotsachars[100];
  gchar thrownaway[5];		/* Nothing is done with the values assigned to
				 * this variable - it's just used to get the
				 * desired return value out of scanf */
  gchar getced;
  static gboolean instaff = FALSE;
  static gboolean tuplet_mode = FALSE;
#if DEBUG
  g_print ("file position %ld\n", ftell (lyin));
#endif
  if ((getced = getc (lyin)) == EOF)
    return 0;
  else
    ungetc (getced, lyin);
  if (!instaff)
    {
      while ((getced = getc (lyin)) != EOF)
	{
	  if (getced == '%')	/* a comment */
	    while ((getced = getc (lyin)) != '\n' && getced != EOF)
	      ;
	  else
	    {
	      ungetc (getced, lyin);
	      if (fscanf (lyin, "%s = \\notes%*[ 0-9A-Za-z=$\\]%1[{]",
			  lotsachars, thrownaway) > 1)
		/* That fscanf is really convoluted so that it'll
		 * match both pre- and post-0.5.3 staffbegins */
		{
		  instaff = TRUE;
		  lylval.strval = g_strdup (lotsachars);
#ifdef DEBUG
		  g_print ("found staffbegin\n");
#endif
		  return STAFFBEGIN;
		}
	      if (fscanf (lyin, "\\tempo %d = %d", &lylval.t.a, &lylval.t.b)
		  == 2)
		{
#ifdef DEBUG
		  g_print ("found tempo\n");
#endif
		  lysi->tempo = lylval.t.b;
		}
	    }
	}
      return 0;
    }				/* End if !instaff */
  else				/* instaff */
    {
      /* The following fscanfs have more hacks in them than you can
       * shake a stick at */

      /* The main hack to notice is that no two scanfs executed one after
       * the other will have a common prefix. This includes leading
       * whitespace */
      while ((getced = getc (lyin)) != EOF)
	{
	  ungetc (getced, lyin);
#if DEBUG
	  g_print ("first char %c\n", getced);
#endif

	  if (fscanf (lyin, "i%1[s]", thrownaway))
	    {			/* sharp */
#ifdef DEBUG
	      g_print ("found enshift\n");
#endif
	      if (fscanf (lyin, "i%1[s]", thrownaway))	/* Double sharp */
		lylval.a = 2;
	      else
		lylval.a = 1;
	      return ENSHIFT;
	    }
	  if (fscanf (lyin, "%1[e]", scannedchars))
	    {			/* either an e or a flat */
	      if (fscanf (lyin, "%1[s]", thrownaway))
		{		/* flat */
#ifdef DEBUG
		  g_print ("found enshift\n");
#endif
		  if (fscanf (lyin, "e%1[s]", thrownaway))	/* Double flat */
		    lylval.a = -2;
		  else
		    lylval.a = -1;
		  return ENSHIFT;
		}
	      else
		{
#ifdef DEBUG
		  g_print ("found tonetype\n");
#endif
		  lylval.c = scannedchars[0];
		  return TONETYPE;
		}
	    }
	  else if (fscanf (lyin, "%[\']%n", thrownaway, &lylval.a))
	    {
#ifdef DEBUG
	      g_print ("found octind\n");
#endif
	      return OCTIND;
	    }
	  else if (fscanf (lyin, "%[,]%n", thrownaway, &lylval.a))
	    {
#ifdef DEBUG
	      g_print ("found octind\n");
#endif
	      lylval.a = -lylval.a;
	      return OCTIND;
	    }
	  else if (fscanf (lyin, "%[0-9]", lotsachars))
	    {
#ifdef DEBUG
	      g_print ("found baseduration\n");
#endif
	      lylval.a = atoi (lotsachars);
	      return BASEDURATION;
	    }
	  else if (fscanf (lyin, "%[.]%n", thrownaway, &lylval.a))
	    {
#ifdef DEBUG
	      g_print ("found numdots\n");
#endif
	      return NUMDOTS;
	    }

	  /* Note that the next fscanf will eat up any leading whitespace that
	   * there might have been; all the fscanf's preceding this one
	   * were specifically looking for an absence of leading whitespace */
	  else if (fscanf (lyin, " %1[\\]", thrownaway))
	    {
	      if (fscanf (lyin, "tim%1[e]", thrownaway))
		{
		  if (fscanf (lyin, " %d/%d", &lylval.t.a, &lylval.t.b) == 2)
		    {
#ifdef DEBUG
		      g_print ("found timesig\n");
#endif
		      return TIMESIGTOK;
		    }
		  else if (fscanf (lyin, "s %d/%d", &lylval.t.a, &lylval.t.b)
			   == 2)
		    {
#ifdef DEBUG
		      g_print ("found tupopen\n");
#endif
		      tuplet_mode = TRUE;
		      return BEGINTUPLET;
		    }
		}
	      else if (fscanf (lyin, "grace {"))
		{
/*#ifdef DEBUG*/
		  g_print ("found grace\n");
/*#endif*/
		  return STARTGRACE;
		}
	      else if (fscanf (lyin, "key %[a-gis]", lotsachars))
		{
#ifdef DEBUG
		  g_print ("found keysig\n");
#endif
		  lylval.strval = g_strdup (lotsachars);
		  return KEYSIGTOK;
		}
	      else if (fscanf (lyin, "%1[m]", thrownaway))
		{
		  if (fscanf (lyin, "ino%1[r]", thrownaway))
		    {
#ifdef DEBUG
		      g_print ("found minor\n");
#endif
		      return MINOR;
		    }
		  else if (fscanf (lyin, "ajo%1[r]", thrownaway))
		    {
#ifdef DEBUG
		      g_print ("found major\n");
#endif
		      return MAJOR;
		    }
		}
	      else if (fscanf (lyin, "clef %[^\n]", lotsachars))
		{
#ifdef DEBUG
		  g_print ("found clef\n");
#endif
		  lylval.strval = g_strdup (lotsachars);
		  return CLEFTOK;
		}
	      else if (fscanf (lyin, "propert%1[y] ", thrownaway))
		{
		  if (fscanf (lyin, "Staff.midiInstrument = \"%[^\"]\"",
			      lotsachars))
		    {
#ifdef DEBUG
		      g_print ("found instrument\n");
#endif
		      lylval.strval = g_strdup (lotsachars);
		      return INSTRUMENT;
		    }
		  else if (fscanf (lyin, "Voice.noteHeadStyle = #\'%s",
				   lotsachars))
		    {
		      /* Recognize this, but don't return anything;
		       * this is just a placeholder. */
#ifdef DEBUG
		      g_print ("found noteheadstyle directive\n");
#endif
		    }
		}
	      else if (fscanf (lyin, "stem%s", lotsachars))
		{
#ifdef DEBUG
		  g_print ("found stemming directive");
#endif
		  if (g_strcasecmp (lotsachars, "down") == 0)
		    lylval.a = DENEMO_STEMDOWN;
		  else if (g_strcasecmp (lotsachars, "up") == 0)
		    lylval.a = DENEMO_STEMUP;
		  else
		    lylval.a = DENEMO_STEMBOTH;
		  return STEMDIRECTIVETOK;
		}
	      else if (fscanf (lyin, "bar \"|.%1[\"]", thrownaway))
		{
#ifdef DEBUG
		  g_print ("found staffend\n");
#endif
		  instaff = FALSE;
		  return STAFFEND;
		}
	    }
	  else if (fscanf (lyin, "s1*%d/%d", &lylval.t.a, &lylval.t.b) == 2)
	    {
	      /* ignore full measure rests */
	    }
	  else if (fscanf (lyin, "%1[<]", thrownaway))
	    {
#ifdef DEBUG
	      g_print ("found beginchord\n");
#endif
	      return BEGINCHORD;
	    }
	  else if (fscanf (lyin, "%1[>]", thrownaway))
	    {
#ifdef DEBUG
	      g_print ("found endchord \n");
#endif
	      return ENDCHORD;
	    }
	  else if (fscanf (lyin, "%1[a-gr]", scannedchars))
	    {
#ifdef DEBUG
	      g_print ("found tonetype\n");
#endif
	      lylval.c = scannedchars[0];
	      return TONETYPE;
	    }
	  else if (fscanf (lyin, "%1[~]", thrownaway))
	    {			/* Tie indicator */
#ifdef DEBUG
	      g_print ("found tie\n");
#endif
	      return TIE;
	    }
	  else if (fscanf (lyin, "%1[(]", thrownaway))
	    {			/* Slur indicator */
#ifdef DEBUG
	      g_print ("found slur\n");
#endif
	      return SLUR_START;
	    }
	  else if (fscanf (lyin, "%1[)]", thrownaway))
	    {			/* Slur indicator */
#ifdef DEBUG
	      g_print ("found end slur\n");
#endif
	      return SLUR_END;
	    }
	  else if (fscanf (lyin, "%1[|]", thrownaway))
	    {			/* Barline marker */
#ifdef DEBUG
	      g_print ("found barline\n");
#endif
	      return BLINE;
	    }
	  else if (fscanf (lyin, "%1[}]", thrownaway))
	    {			/* Close bracket - end of tuplet group */

/*#ifdef DEBUG*/
	      if (tuplet_mode)
		g_print ("found tupclose\n");
	      else
		g_print ("found grace end\n");
/*#endif*/
	      if (tuplet_mode)
		{
		  tuplet_mode = FALSE;
		  return ENDTUPLET;
		}
	      else
		return ENDGRACE;
	    }
	  else if (fscanf (lyin, "-\\%[A-Za-z]", lotsachars))
	    {
	      if (lotsachars[0] == 'p' ||
		  !strcmp (lotsachars, "mf") ||
		  !strcmp (lotsachars, "mfp") ||
		  !strcmp (lotsachars, "mp") ||
		  !strcmp (lotsachars, "f") ||
		  !strcmp (lotsachars, "ff") ||
		  !strcmp (lotsachars, "fff") ||
		  !strcmp (lotsachars, "sf") ||
		  !strcmp (lotsachars, "sfp") ||
		  !strcmp (lotsachars, "sfz") ||
		  !strcmp (lotsachars, "cr") ||
		  !strcmp (lotsachars, "rc") ||
		  !strcmp (lotsachars, "dr") || !strcmp (lotsachars, "rd"))
		{
		  lylval.strval = g_strdup (lotsachars);
#ifdef DEBUG
		  g_print ("found dynamic %s\n", lotsachars);
#endif
		  return DYNAMICTOK;

		}
	      else
		{
		  lylval.strval = g_strdup (lotsachars);
#ifdef DEBUG
		  g_print ("found lytoneoption %s\n", lotsachars);
#endif
		  return LYTONEOPTION;
		}
	    }
	  else
	    {
	      getced = fgetc (lyin);
	      /* Shift to the next character and hope it's more interesting */
	      if (getced == '%')
		{		/* a comment */
		  getced = getc (lyin);
		  if (getced == '!')
		    {
#ifdef DEBUG
		      g_print ("found reloaddirective\n");
#endif
		      while ((getced = getc (lyin)) != '\n' && getced != EOF)
			;
		      return RELOADDIRECTIVE;
		    }
		  else
		    {
		      ungetc (getced, lyin);
		      while ((getced = getc (lyin)) != '\n' && getced != EOF)
			;
		    }
		}
	    }
	}
      return 0;
    }				/* End else */
}

gint
lyerror (char *s)
{
  fprintf (stderr, "%s\n", s);
  return 1;
}
