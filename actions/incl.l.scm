;
; Table generated from the file /home/rshann/local/share/denemo/actions/incl.l by SILex 1.0
;

(define incl-table
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
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                  (yycontinue)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
          	 (yycontinue);backup rule
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
       	         (yycontinue)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                   (yycontinue)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
         (yycontinue)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        	(begin (lyimport::push-port (substring yytext 1 (- (string-length yytext) 1))) (lyimport::pop_state) (lyimport::multilexer))
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
        	(lyimport::mtoken 'ERROR "end quote missing")						
        )))
   'decision-trees
   0
   0
   '#((33 (10 (9 err 2) (32 err 2)) (35 (34 err 1) (= 37 3 err))) (= 34 4
    1) (10 (9 err 2) (= 32 2 err)) (114 (= 10 5 6) (123 (115 5 6) (124 err
    6))) err err (11 (10 7 9) (= 114 8 7)) (11 (10 7 9) (= 114 8 7)) (11
    (10 7 9) (= 114 8 7)) err)
   '#((#f . #f) (6 . 6) (4 . 4) (#f . #f) (5 . 5) (2 . 2) (1 . 1) (3 . 3)
    (0 . 0) (0 . 0))))
