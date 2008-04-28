%{
/*
	lexer.ll -- implement the Flex lexer
	source file format ??
	(c) 2000, 2001 Adam Tee <eenajt@electeng.leeds.ac.uk>
	(c) 2000, 2001 University of Leeds	


*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "redefs.h"
#include "frogdefs.h"
#include "frogparser.h"
#undef yyunput
#undef yy_top_state
#define yylval froglval
int i=0;

int errcount = 0;


%}

%option stack
%option never-interactive
%option noyywrap
%x	system
%x 	staffgroup
%x	staff
%x	musicalobject
%x	note
%x      tuplet

A		[a-zA-Z]
AA		{A}|_
N		[0-9]
AN		{AA}|{N}
UNSIGNED	{N}+
INT		-?{UNSIGNED}
FLOAT		({INT}\.{UNSIGNED})
WORD		{A}{AN}*
WHITE		[ \t\f\r]
EXEC		[,;/=]
NOTETYPE	[a-g][#b]*		
KEYSIG		[A-G][#b]?("min"|"maj")
DIR		"up"|"down"|"left"|"right"|"null"
BAR		"simple_line"|"double_line"|"open_repeat"|"close_repeat"
CLEF		"treble"|"soprano"|"bass"|"alto"|"tenor"|"G"|"F"|"E"|"C"|"G8"
INDICATION	"fermata"|"spiccato"|"tenuto"|"mezzo-staccato"|"ubow"|"dbow"|"staccato"
TIES		"tie"
ACCENTS		"accent"|"normal"|"hat"
ORNAMENTS	"trill"|"turn"|"mordent"|"glissando"|"gliss"|"inverted turn"
ACCIDENTALS	"#"|"##"|"b"|"bb"|"n"
DYNAMICS	[p]*|[l]*|m[fpz]*|s[fpz]*|"cr"|"rc"|"fp"
TEMPO		"adagio"|"andante"|"moderato"|"allegro"|"presto"|"prestissimo"|"largo"|"tempo primo"|"tempo I"
TRANSTEMPO	"rall"|"accel"|"rit"|"rallentando"|"accelerando"|"ritenuto"
MODES		"lydian"|"mixolydian"|"phrygian"
END             "end"
NOTEHEADTYPE    "cross"|"harmonic"|"diamond"
SLUR            "slur_begin"|"slur_end"
CURVE           "arc"
HAIRPIN         "cresc_begin"|"cresc_end"|"dim_begin"|"dim_end"
%%

<INITIAL>"{"	{
#ifdef DEBUG
printf("Debugging Info enabled\n");
#endif
	yylval.character = yytext[0];
#ifdef DEBUG
	printf("In INITIAL");
	printf("%s\n", yytext);
#endif
	yy_push_state(system);
	return SYSTEM;
}

<system>{
	{WORD}	{
		strcpy(yylval.string, yytext); 
#ifdef DEBUG
printf("%s\n", yytext);
#endif	
		return BRACKET; 	
	}
	
	"["	{
	yylval.character = yytext[0];
#ifdef DEBUG
printf("In system\n");
printf("%s\n", yytext);
#endif
	yy_push_state(staffgroup);
	return STAFFGROUP;
	}
	"}"	{
		/*yylval.character = yytext[0];
		return SYSTEM;*/
		yy_pop_state();
	}	
}

<staffgroup>{
	"("	{	
		yylval.character = yytext[0];
#ifdef DEBUG
printf("IN staffgroup\n");
printf("%s\n", yytext);
#endif
		yy_push_state(staff);
#ifdef DEBUG
printf("going to Staff state");
#endif
		return STAFF;
	}
	{WHITE}+ 	{ }
	"\n" {
		errcount++;
	}	
}

<staff,staffgroup>"]"{WHITE}*"\n"	{
		yylval.character = yytext[0];
			 	
		yy_pop_state();	
		return ENDSTAFFGROUP;	
}

<staff>"(" {
               yylval.character = yytext[0];
               return STAFF;
}

<staff>{
	{WORD}     {
                strcpy(yylval.string,yytext);
                return STAFFNAME;
        }
        {INT}	{
		yylval.number = atoi(yytext);
#ifdef DEBUG
printf("%d\n", atoi(yytext));
#endif
		return NUM;
	}
	{WHITE}*"\n"	{
		errcount++;
		yy_push_state(musicalobject);
#ifdef DEBUG
printf("Going to Musical Object State\n");
#endif		
	}
	")"	{
#ifdef DEBUG
printf("Leaving Staff state");
#endif
	/*	yylval.character = yytext[0];
		
		yy_pop_state();
		return ENDSTAFF;*/
	}
        
}

<musicalobject>{
	{INT} 	{
		yylval.number = atoi(yytext);
#ifdef DEBUG
printf("%d\n", atoi(yytext));
#endif
		return NUM;
	}
	{EXEC}	{
		return yytext[0];
	 }
	{FLOAT}	{		
		yylval.f = atof(yytext);
		
#ifdef DEBUG
printf("Float value is %f\n", atof(yytext));
#endif		
		return FLOAT;
	}
	{NOTETYPE} {
		strncpy(yylval.string ,yytext,3);
		yy_push_state(note);
#ifdef DEBUG
printf("Going to note state\n");
printf("%s\n", yytext); 
#endif	
		return NOTENAME;
	}
       	{KEYSIG}	{
		strcpy(yylval.string, yytext);
		return KEYTYPE;
	}
	{DYNAMICS}	{
		strcpy(yylval.string, yytext);
		return DYN;
	}
	{BAR}	{
		strcpy(yylval.string, yytext);
		return BARTYPE;
	}
	{CLEF}	{
                strcpy(yylval.string, yytext);
		return CLEFTYPE;
		/*if(yytext[0] == 'G' || yytext[0] == 'F' || yytext[0] == 'E'
		   || yytext[0] == 'C')
		{
			yylval.character = yytext[0];
			return CLEFTYPE;
		}*/
		/*else
		{
			yylval.string == yytext;
			return CLEFTYPE;
		}*/
	}
	{TEMPO}	{
		strcpy(yylval.string, yytext);
		return TEMPOTERM;
	}
	{TRANSTEMPO}	{
		strcpy(yylval.string, yytext);
		return TRANSTEMPO;
	}
        {SLUR}  {
                strcpy(yylval.string, yytext);
                return SLURTYPE;
        }
        {HAIRPIN} {
                strcpy(yylval.string, yytext);
                return HAIRPINTYPE;
        }
        {CURVE} {
                strcpy(yylval.string, yytext);
                return CURVESHAPE;
        }
	{WORD}	{
		
#ifdef DEBUG
printf("found word\n");
#endif		
		if(yytext[0] == 'r')
		{
			yylval.character = yytext[0];
			return RESTTYPE;
		}
                else if(yytext[0] == 't')
                {
                        yylval.character = yytext[0];
                        yy_push_state(tuplet);
                        return TUPLET;
                }
	}
        

	")"{WHITE}*"\n" 	{
		errcount++;	
#ifdef DEBUG
printf("Leaving musical object");
#endif
		/*yylval.character = yytext[0];
		return ENDSTAFF;*/
		yy_pop_state();
		yy_pop_state();		
	}
       	{WHITE}+ { }
	"\n" {
		errcount++;
	}
	
} 	 

<note>{
	{FLOAT}	{
		yylval.f = atof(yytext);
		return FLOAT;
	}
	
	{INT} {
		yylval.number = atoi(yytext);
		return NUM;
	}	
	{DIR}	{
		strcpy(yylval.string, yytext);
#ifdef DEBUG
printf("Return DIRECTION=%s\n", yylval.string);
#endif	
		return DIRECTION;
	}
	{INDICATION}	{
		
		strcpy(yylval.string, yytext);
#ifdef DEBUG
printf("%s\n",yylval.string);
#endif		
		return STYLE;
	}	
	{TIES}	{
		strcpy(yylval.string, yytext);
		return TIE;
	}
	"\n" {
		errcount++;
#ifdef DEBUG
printf("Back musicalobject state\n");
#endif
		yy_pop_state();
	}
	{ORNAMENTS}	{
		strcpy(yylval.string, yytext);
		return ORNAMENT;
	}
	{ACCENTS}	{
		strcpy(yylval.string, yytext);
		return ACCENT;
	}
	{ACCIDENTALS}	{
		strcpy(yylval.string, yytext);
		return ACCIDENTAL;
	}
        {NOTEHEADTYPE}  {
                strcpy(yylval.string, yytext);
                return NOTEHEAD;
        }
	{WHITE}+	{ }
	{EXEC} 	{ return yytext[0]; }
        
		
}
<tuplet>{
        {FLOAT} {
                yylval.f = atof(yytext);
                return FLOAT;
        }
     	{INT} {
		yylval.number = atoi(yytext);
		return NUM;
	}
        {END} {
                strcpy(yylval.string, yytext);
                return TUP_END;
        }
        "\n" {
            errcount++;
            yy_pop_state();
        }

}

{WHITE}*"\n" {
	errcount++;
}
%%















