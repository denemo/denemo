; A comment
digit [0-9]
letter [a-zA-Z]
notename c|cis|ces|cisis|ceses|d|dis|des|disis|deses|e|eis|es|ees|eisis|eses|eeses|f|fis|fes|fisis|feses|g|gis|ges|gisis|geses|a|ais|as|aes|aisis|aeses|ases|b|bis|bes|bisis|beses|h|his|hes|hisis|heses
whitespace [ 	] ;space and tab!
newline \n

%%

;%{[.\n]*%}					(mtoken 'MULTILINECOMMENT yytext)
\"[^\".]*\"					(mtoken 'QUOTEDSTRING yytext)
%.*{newline}				(mtoken 'COMMENT yytext)
{notename}					(mtoken 'NOTE yytext)
{digit}						(mtoken 'INTEGER yytext)
{letter}					(mtoken 'LETTER yytext)
\.+							(mtoken 'DOT yytext)
\"							(mtoken 'DBLQUOTE yytext)
[',]+						(mtoken 'OCTAVESIGN yytext)
({whitespace}|{newline})	(mtoken 'WHITESPACE yytext)
<<EOF>>						(make-lexical-token '*eoi* #f #f)
