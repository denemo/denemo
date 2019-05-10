;
; Table generated from the file /home/rshann/local/share/denemo/actions/quote.l by SILex 1.0
;

(define quote-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       						(make-lexical-token '*eoi* #f #f)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         					(lyimport::mtoken 'ERROR	yytext)
       ))
   (vector
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
		(lyimport::quote-append yytext)	(lyimport::multilexer)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
		(lyimport::pop_state)
		(lyimport::mtoken 'STRING lyimport::quoted_string)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
		(lyimport::quote-append yytext)	(lyimport::multilexer)
        )))
   'decision-trees
   0
   0
   '#((34 (= 10 4 3) (92 (35 2 3) (93 1 3))) err err (35 (34 4 err) (= 92
    err 4)) (35 (34 4 err) (= 92 err 4)))
   '#((#f . #f) (2 . 2) (1 . 1) (0 . 0) (0 . 0))))
