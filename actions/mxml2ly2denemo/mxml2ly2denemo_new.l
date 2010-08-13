notename			c|cis|ces|cisis|ceses|d|dis|des|disis|deses|e|eis|es|ees|eisis|eses|eeses|f|fis|fes|fisis|feses|g|gis|ges|gisis|geses|a|ais|as|aes|aisis|aeses|ases|b|bis|bes|bisis|beses|h|his|hes|hisis|heses
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

{BOM_UTF8}/.* 		(yycontinue)
;original bom_utf8/.*
;  if (this->lexloc_->line_number () != 1 || this->lexloc_->column_number () != 0)
;    {
;      LexerError (_ ("stray UTF-8 BOM encountered").c_str ());
;      exit (1);
;    }
;  if (be_verbose_global)
;     message (_ ("Skipping UTF-8 BOM"));


\\version{WHITE}*.*{NEWLINE} (lyimport::mtoken 'ERROR	yytext) ; skip version, originally its a state switch yy_push_state (version)
%.*{NEWLINE}				(yycontinue)
\\sourcefilename{WHITE}*	(lyimport::mtoken 'ERROR	yytext) ; yy_push_state (sourcefilename)
\\sourcefileline{WHITE}*	(lyimport::mtoken 'ERROR	yytext) ; yy_push_state (sourcefileline)
\\maininput         		(lyimport::mtoken 'ERROR	yytext) ; if (!is_main_input_) (block: start_main_input (); is_main_input_ = true; ) ELSE	error (_ ("\\maininput not allowed outside init files"));
\\include           		(lyimport::mtoken 'ERROR	yytext) ; yy_push_state (incl)

{RESTNAME}					(lyimport::mtoken 'RESTNAME yytext)
R							(lyimport::mtoken 'MULTI_MEASURE_REST yytext)	
<<							(lyimport::mtoken 'DOUBLE_ANGLE_OPEN yytext)	
>>							(lyimport::mtoken 'DOUBLE_ANGLE_CLOSE yytext)	
<							(lyimport::mtoken 'ANGLE_OPEN yytext)	
>							(lyimport::mtoken 'ANGLE_CLOSE yytext)	
{ALPHAWORD}					(lyimport::scan_bare_word yytext)	
{NOTECOMMAND}				(lyimport::scan_escaped_word (string-trim yytext #\\))	
{FRACTION}					(lyimport::scan_fraction yytext)	;		yylval.scm =  scan_fraction (YYText ()); return FRACTION;
{DIGIT}						(lyimport::mtoken 'DIGIT yytext)
{UNSIGNED}					(lyimport::mtoken 'UNSIGNED yytext)	
{E_UNSIGNED}				(lyimport::mtoken 'E_UNSIGNED yytext)	

;{DASHED_WORD}				(lyimport::scan_bare_word yytext)
;{DASHED_KEY_WORD}			(lyimport::scan_escaped_word yytext)	

;{WORD}						(lyimport::scan_bare_word yytext)
;{KEYWORD}					(lyimport::scan_escaped_word yytext)

;{REAL}						(lyimport::mtoken 'REAL yytext)

\{							(lyimport::mtoken '{ yytext)
\}							(lyimport::mtoken '} yytext)

\*							(lyimport::mtoken 'STAR yytext)
:							(lyimport::mtoken ': yytext)
=							(lyimport::mtoken 'EQUAL yytext)
\.							(lyimport::mtoken 'DOT yytext)
!							(lyimport::mtoken 'EXCLAMATIONMARK yytext)
\?							(lyimport::mtoken 'QUESTIONMARK yytext)
						

\\.							(case (string->char yytext)
								((#\>) (lyimport::mtoken 'E_ANGLE_CLOSE yytext))
								((#\<) (lyimport::mtoken 'E_ANGLE_OPEN yytext))
								((#\!) (lyimport::mtoken 'E_EXCLAMATION yytext))
								((#\() (lyimport::mtoken 'E_OPEN yytext))
								((#\)) (lyimport::mtoken 'E_CLOSE yytext))
								((#\[) (lyimport::mtoken 'E_BRACKET_OPEN yytext))
								((#\+) (lyimport::mtoken 'E_PLUS yytext))
								((#\]) (lyimport::mtoken 'E_TILDE yytext))
								((#\\) (lyimport::mtoken 'E_BACKSLASH yytext))
								(else  (lyimport::mtoken 'E_CHAR yytext)))


,							(lyimport::mtoken 'SUB_QUOTE yytext)
'							(lyimport::mtoken 'SUP_QUOTE yytext)	
\+							(lyimport::mtoken 'PLUS yytext)
({WHITE}|{NEWLINE})			(yycontinue);(lyimport::mtoken 'WHITESPACE yytext)
.							(lyimport::mtoken 'ANYTHING yytext) ; in lilypond there is no token name
<<ERROR>>					(lyimport::mtoken 'ERROR	yytext)
<<EOF>>						(make-lexical-token '*eoi* #f #f)
