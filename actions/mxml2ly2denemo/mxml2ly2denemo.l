; A comment
digit [0-9]
letter [a-zA-Z]
whitespace [ \t\r]
newline \n

%%

%.*{newline}				(mtoken 'COMMENT yytext)
{digit}						(mtoken 'INTEGER yytext)
{letter}					(mtoken 'LETTER yytext)
{letter}{digit}				(mtoken 'NOTE yytext)
({whitespace}|{newline})
<<EOF>>						'*eoi*
