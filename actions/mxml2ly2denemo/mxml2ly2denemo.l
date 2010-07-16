; A comment
digit [0-9]
letter [a-zA-Z]
whitespace [ ] ;space and tab!
tab [	]
newline \n

%%

%.*{newline}				(mtoken 'COMMENT yytext)
{digit}						(mtoken 'INTEGER yytext)
{letter}					(mtoken 'LETTER yytext)
;{letter}{digit}			(mtoken 'NOTE yytext)
{tab}						(mtoken 'TAB yycontinue)
({whitespace}|{newline})	(yycontinue)
<<EOF>>						(make-lexical-token '*eoi* #f #f)
