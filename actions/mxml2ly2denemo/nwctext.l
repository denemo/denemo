;Macros, substitue as {macro}
digit [0-9]
letter [a-zA-Z]
alphanum [a-zA-Z0-9\-]
whitespace [ 	] ;space and tab!
newline [^.] ;hehe... covers windows and linux CR \n as linefeed
sep	\|
durations Whole|Half|4th|8th|16th|32nd|64th|128th|256th
clefs Treble|Bass|Alto|Percussion|Tenor

; Das klappt alles so nicht. Ich muss Duration und so alles nochmal aufspalten in Werte und das dann im Parser zusammensetzen!
; Möglichst viel eindeutig machen. Die Notendurations sind strings, also alle von hand eingeben. was ist mit punktiert?
; Position muss errechnet werden zwischen der zahl aus der quelle sowie dem clef. Also clef kriegt eine absolute zahl und dann wird addiert, oder substrahiert. Der clef markiertja ne linie

; Es muss zwei Versionen geben. Eine der Beam und Stems und Barlines und son kram ignoriert und eine version, die das alles mitbeachtet.

%%
;Expressions
!NoteWorthyComposer\(.*\)	(yycontinue)
!NoteWorthyComposer-End		(yycontinue)
{sep}Note					(mtoken 'NEXTISNOTE yytext)
{sep}Dur:					(mtoken 'DURATION yytext)
{sep}Pos:					(mtoken 'POSITION yytext)


;Metainfo
\|SongInfo.*{newline}		(yycontinue)
;\|Title:					(mtoken 'TITLE yytext)

{durations}					(mtoken 'DURATIONVALUE yytext)
,Dotted						(mtoken 'DOTTED yytext)
,DblDotted					(mtoken 'DOUBLEDOTTED yytext)

Opts:Stem=Up				(yycontinue)
Opts:Stem=Down				(yycontinue)
Beam=First					(yycontinue)
Beam=End					(yycontinue)
Beam						(yycontinue)

\|Clef\|Type:				(mtoken 'CLEF yytext)
{clefs}						(mtoken 'CLEFVALUE yytext)
OctaveShift:Octave\ Up		(mtoken 'CLEF8UP yytext)
OctaveShift:Octave\ Down	(mtoken 'CLEF8DOWN yytext)



\|Bar{newline}				(yycontinue)
\|Bar\|Style:Double			(mtoken 'BAR-DOUBLE yytext)

\|AddStaff					(mtoken 'STAFF yytext)
\|Name:						(mtoken 'STAFFNAME yytext)
\|Label:					(mtoken 'STAFFLABEL yytext)


[0-9\-]+						(mtoken 'NUMBER yytext)
[#bnvx]							(mtoken 'ALT yytext)
\"({alphanum}|{whitespace})+\"	(mtoken 'STRING yytext)
{digit}							(mtoken 'INTEGER yytext)
{letter}						(mtoken 'LETTER yytext)
\"								(mtoken 'DBLQUOTE yytext)
({whitespace}|{newline})		(mtoken 'WHITESPACE yytext)
<<ERROR>>						(mtoken 'ERROR	yytext)
<<EOF>>							(make-lexical-token '*eoi* #f #f)

