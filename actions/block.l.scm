;
; Table generated from the file /home/rshann/local/share/denemo/actions/block.l by SILex 1.0
;

(define block-table
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
		(lyimport::block-append yytext)	(lyimport::multilexer)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
   	        (set! lyimport::brace_count (+ lyimport::brace_count 1))
                (lyimport::block-append yytext)	(lyimport::multilexer)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                (set! lyimport::brace_count (- lyimport::brace_count 1))
                (if (zero? lyimport::brace_count)
                    (begin
	           	  (lyimport::pop_state)
		          (lyimport::mtoken 'BLOCK lyimport::block_string))
                    (begin
                          (lyimport::block-append yytext)	
                          (lyimport::multilexer)))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
		(lyimport::quote-append yytext)	(lyimport::multilexer)
        )))
   'decision-trees
   0
   0
   '#((123 (= 10 4 3) (125 (124 2 3) (126 1 3))) err err (124 (123 4 err)
    (= 125 err 4)) (124 (123 4 err) (= 125 err 4)))
   '#((#f . #f) (2 . 2) (1 . 1) (0 . 0) (0 . 0))))
