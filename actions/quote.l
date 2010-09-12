A					[a-zA-Z\200-\377]
AA					{A}|_
N					[0-9]
AN					{AA}|{N}
ANY_CHAR			(.|\n)
PUNCT				[?!:'`]
ACCENT				\\[`'"^]
NATIONAL			[\001-\006\021-\027\031\036]
TEX					{AA}|-|{PUNCT}|{ACCENT}|{NATIONAL}
WORD				{A}{AN}*
DASHED_WORD			{A}({AN}|-)*
DASHED_KEY_WORD		\\{DASHED_WORD}
	
ALPHAWORD			{A}+
DIGIT				{N}
UNSIGNED			{N}+
E_UNSIGNED			\\{N}+
FRACTION			{N}+\/{N}+
INT					-?{UNSIGNED}
REAL				({INT}\.{N}*)|(-?\.{N}+)
KEYWORD				\\{WORD}
NEWLINE				[^.] ; windows and linux/unix
WHITE				[ 	] ;space and tab!
HORIZONTALWHITE		[ 	]
BLACK				[^ \n\t\f\r] ;TODO: needs fix
RESTNAME			[rs]
NOTECOMMAND			\\{A}+
MARKUPCOMMAND		\\({A}|[-_])+
LYRICS				({AA}|{TEX})[^0-9 \t\n\r\f]*
ESCAPED				[nt\\'"]
EXTENDER			__
HYPHEN				--
BOM_UTF8			\357\273\277

%%
;\\{ESCAPED}	(
;static char
;escaped_char (char c)
;{
;  switch (c)
;    {
;    case 'n':
;      return '\n';
;    case 't':
;      return '\t';
;    case '\'':
;    case '\"':
;    case '\\':
;      return c;
;    }
;  return 0;
;}
;		(lyimport::quote-append (escaped_char (string-ref yytext 1)))



[^\\"]+	
		(lyimport::quote-append yytext)	(lyimport::multilexer)
	
\"	
		(lyimport::pop_state)
		(lyimport::mtoken 'STRING lyimport::quoted_string)
.
		(lyimport::quote-append yytext)	(lyimport::multilexer)
	

<<ERROR>>					(lyimport::mtoken 'ERROR	yytext)
<<EOF>>						(make-lexical-token '*eoi* #f #f)
