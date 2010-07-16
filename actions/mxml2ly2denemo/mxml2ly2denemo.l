; A comment
digit [0-9]
letter [a-zA-Z]
whitespace [ \t\r]
newline \n

%%

;%.*{newline}				(cons 'COMMENT yytext)
{digit}						(make-lexical-token 'INTEGER (make-source-location "*stdin*" 0 0 -1 -1) yytext)	
;{letter}					(cons 'LETTER yytext)	
;{letter}{digit}			(cons 'NOTE yytext)
({whitespace}|{newline})
<<EOF>>						'*eoi*
