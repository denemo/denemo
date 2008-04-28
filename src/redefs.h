/* redefs.h */

/* Redefinitions for frog format parser */

#ifndef REDEFS_H
#define REDEFS_H

#define    yymaxdepth frogmaxdepth
#define    yyparse frogparse
/*#define    yylex   froglex*/
/*#define    yyerror frogerror*/
#define    yylval  froglval
#define    yychar  frogchar
#define    yydebug frogdebug
#define    yypact  frogpact
#define    yyr1    frogr1
#define    yyr2    frogr2
#define    yydef   frogdef
#define    yychk   frogchk
#define    yypgo   frogpgo
#define    yyact   frogact
#define    yyexca  frogexca
#define yyerrflag frogerrflag
#define yynerrs    frognerrs
#define    yyps    frogps
#define    yypv    frogpv
#define    yysfrogs
#define    yy_yys  frogs
#define    yystate frogstate
#define    yytmp   frogtmp
#define    yyvfrogv
#define    yy_yyv  frogv
#define    yyval   frogval
#define    yylloc  froglloc
#define yyreds     frogreds
#define yytoks     frogtoks
#define yylhs      froglhs
#define yylen froglen
#define yydefred frogdefred
#define yydgoto    frogdgoto
#define yysindex frogsindex
#define yyrindex frogrindex
#define yygindex froggindex
#define yytable frogtable
#define yycheck frogcheck
#define yyname   frogname
#define yyrule   frogrule

#endif
