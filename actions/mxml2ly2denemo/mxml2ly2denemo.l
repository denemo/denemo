; A comment
digit [0-9]
letter [a-zA-Z]
whitespace [ \t\r]
newline \n

%%

;%.*{newline}				(cons 'COMMENT yytext)
{digit}						(cons 'INTEGER yytext)	
;{letter}					(cons 'LETTER yytext)	
;{letter}{digit}			(cons 'NOTE yytext)
({whitespace}|{newline})
<<EOF>>						'*eoi*
