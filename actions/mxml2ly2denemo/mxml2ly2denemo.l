; A comment
digit [0-9]
letter [a-zA-Z]
whitespace [ \t\r]
newline \n

%%

%.*{newline}				(cons 'comment yytext)
note						(cons 'string yytext)	
{digit}						(cons 'integer yytext)	
{letter}					(cons 'letter yytext)	
{letter}{digit}				(cons 'note yytext)
({whitespace}|{newline})
<<EOF>>						'*eof*
