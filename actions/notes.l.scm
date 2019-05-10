;
; Table generated from the file /home/rshann/local/share/denemo/actions/notes.l by SILex 1.0
;

(define notes-table
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
;original bom_utf8/.*
;  if (this->lexloc_->line_number () != 1 || this->lexloc_->column_number () != 0)
;    {
;      LexerError (_ ("stray UTF-8 BOM encountered").c_str ());
;      exit (1);
;    }
;  if (be_verbose_global)
;     message (_ ("Skipping UTF-8 BOM"));
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
  		(lyimport::start_quote) (lyimport::multilexer)  ;cannot use yycontinue as that calls this lexer
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                          (lyimport::start_block) (lyimport::multilexer)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                           (lyimport::start_block) (lyimport::multilexer)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                           (lyimport::start_block) (lyimport::multilexer)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                         (lyimport::start_block) (lyimport::multilexer) 
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                           (lyimport::start_block)  (lyimport::mtoken 'MARKUP ((record-accessor lexical-token 'value)(lyimport::multilexer)))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                             (lyimport::mtoken 'VERSION	yytext) ; in lilypond it is a state switch yy_push_state (version)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
            				(yycontinue)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                        	(lyimport::mtoken 'ERROR	yytext) ; yy_push_state (sourcefilename)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                        	(lyimport::mtoken 'ERROR	yytext) ; yy_push_state (sourcefileline)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                    		(lyimport::mtoken 'ERROR	yytext) ; if (!is_main_input_) (block: start_main_input (); is_main_input_ = true; ) ELSE	error (_ ("\\maininput not allowed outside init files"));
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                    	  (lyimport::start_incl) (display "include state started")(lyimport::multilexer)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
          					(lyimport::mtoken 'RESTNAME yytext)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                                                (begin  
                                                   (lyimport::mtoken 'SCM_TOKEN (read (make-soft-port
                                                             (vector #f #f #f  (lambda () (lexer-getc)) #f) "r"))))
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                              (lyimport::start_block) (lyimport::multilexer) 
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                              (lyimport::start_block)  (lyimport::multilexer)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
 							(lyimport::mtoken 'MULTI_MEASURE_REST yytext)	
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
  							(lyimport::mtoken 'DOUBLE_ANGLE_OPEN yytext)	
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
  							(lyimport::mtoken 'DOUBLE_ANGLE_CLOSE yytext)	
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
 							(lyimport::mtoken 'ANGLE_OPEN yytext)	
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
 							(lyimport::mtoken 'ANGLE_CLOSE yytext)	
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
           					(lyimport::scan_bare_word yytext)	
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
             				(lyimport::scan_escaped_word (string-trim yytext #\\))	
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
          					(lyimport::scan_fraction yytext)	;		yylval.scm =  scan_fraction (YYText ()); return FRACTION;
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
       						(lyimport::mtoken 'DIGIT yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
          					(lyimport::mtoken 'UNSIGNED yytext)	
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
            				(lyimport::mtoken 'E_UNSIGNED yytext)	
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
  							(lyimport::mtoken '{ yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
  							(lyimport::mtoken '} yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
  							(lyimport::mtoken 'STAR yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
 							(lyimport::mtoken ': yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
 							(lyimport::mtoken 'EQUAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
  							(lyimport::mtoken 'DOT yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
 							(lyimport::mtoken 'EXCLAMATIONMARK yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
  							(lyimport::mtoken 'QUESTIONMARK yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
  							(lyimport::mtoken 'PIPE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
 							(lyimport::mtoken 'SLASH yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
  							(lyimport::mtoken 'CARET yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
 							(lyimport::mtoken 'UNDERSCORE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
 							(lyimport::mtoken 'HYPHEN yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
 							(lyimport::mtoken 'TILDE yytext) ; This may be the wrong place. But the lexer did not detect E_TILDE with the block \\. below
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
  							(lyimport::mtoken 'BRACKET_OPEN yytext) ; This is a custom lexer rule. In Lilypond these are used directly as strings in the parser.
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
  							(lyimport::mtoken 'BRACKET_CLOSE yytext) ; This is a custom lexer rule. In Lilypond these are used directly as strings in the parser.
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
           (lyimport::mtoken 'OPEN yytext); for slurs
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
           (lyimport::mtoken 'CLOSE yytext); for slurs						
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
   							(case (string-ref yytext 0) ; not sure for what this is. maybe scheme escaped parts in lilypond?
								((#\>) (lyimport::mtoken 'E_ANGLE_CLOSE yytext))
								((#\<) (lyimport::mtoken 'E_ANGLE_OPEN yytext))
								((#\!) (lyimport::mtoken 'E_EXCLAMATION yytext))
								((#\() (lyimport::mtoken 'E_OPEN yytext))
								((#\)) (lyimport::mtoken 'E_CLOSE yytext))
								((#\[) (lyimport::mtoken 'E_BRACKET_OPEN yytext))
								((#\+) (lyimport::mtoken 'E_PLUS yytext))
								((#\]) (lyimport::mtoken 'E_BRACKET_CLOSE yytext))								
								((#\~) (lyimport::mtoken 'E_TILDE yytext))
								((#\\) (lyimport::mtoken 'E_BACKSLASH yytext))
								(else  (lyimport::mtoken 'E_CHAR yytext)))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
 							(lyimport::mtoken 'SUB_QUOTE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
 							(lyimport::mtoken 'SUP_QUOTE yytext)	
;\+							(lyimport::mtoken 'PLUS yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
  							(lyimport::mtoken 'STRING yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                   			(yycontinue);(lyimport::mtoken 'WHITESPACE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
 							(lyimport::mtoken 'ANYTHING yytext) ; in lilypond there is no token name
;\"							(lyimport::mtoken 'DBLQUOTE yytext) ;; This is a custom Denemo token
        )))
   'decision-trees
   0
   0
   '#((62 (41 (35 (11 (10 1 2) (33 1 (34 17 33))) (38 (36 29 (37 1 31)) (39
    1 (40 4 7)))) (47 (44 (42 6 (43 21 3)) (45 5 (46 11 18))) (59 (48 14
    (58 24 20)) (60 1 (61 27 19))))) (97 (91 (65 (63 26 (64 16 1)) (= 82 28
    25)) (94 (92 9 (93 32 8)) (95 13 (96 12 1)))) (126 (123 (114 25 (116 30
    25)) (124 23 (125 15 22))) (357 (127 10 (200 1 25)) (358 34 (378 25
    1)))))) err err err err err err err err err err err err err err err err
    err err err err err err err (48 (47 err 36) (58 35 err)) (97 (65 err
    (91 37 err)) (200 (123 37 err) (378 37 err))) (= 62 38 err) (= 60 39
    err) (97 (65 err (91 37 err)) (200 (123 37 err) (378 37 err))) err (97
    (65 err (91 37 err)) (200 (123 37 err) (378 37 err))) (11 (10 41 42) (=
    46 40 41)) (106 (91 (48 (= 10 err 43) (58 44 (65 43 45))) (100 (97 43
    (99 45 46)) (104 45 (105 51 47)))) (115 (110 (108 45 (109 52 50)) (=
    112 53 45)) (119 (116 48 (118 45 49)) (200 (123 45 43) (378 45 43)))))
    err (123 (91 (65 err 37) (97 err 37)) (273 (200 err 37) (274 54 (378 37
    err)))) (48 (47 err 36) (58 35 err)) (48 err (58 55 err)) (97 (65 err
    (91 37 err)) (200 (123 37 err) (378 37 err))) err err (11 (10 41 42) (=
    46 40 41)) (11 (10 41 42) (= 46 40 41)) err err (48 err (58 56 err))
    (97 (65 err (91 57 err)) (200 (123 57 err) (378 57 err))) (104 (91 (65
    err 57) (97 err 57)) (123 (105 58 57) (200 err (378 57 err)))) (110 (91
    (65 err 57) (97 err 57)) (123 (111 59 57) (200 err (378 57 err)))) (111
    (91 (65 err 57) (97 err 57)) (123 (112 60 57) (200 err (378 57 err))))
    (101 (91 (65 err 57) (97 err 57)) (123 (102 61 57) (200 err (378 57
    err)))) (105 (91 (65 err 57) (97 err (98 62 57))) (123 (106 63 57) (200
    err (378 57 err)))) (101 (91 (65 err 57) (97 err 57)) (123 (102 64 57)
    (200 err (378 57 err)))) (121 (91 (65 err 57) (97 err (98 66 57))) (123
    (122 65 57) (200 err (378 57 err)))) (98 (91 (65 err 57) (97 err 67))
    (200 (123 57 err) (378 57 err))) (123 (91 (65 err 37) (97 err 37)) (277
    (200 err 37) (278 68 (378 37 err)))) (48 err (58 55 err)) (48 err (58
    56 err)) (97 (65 err (91 57 err)) (200 (123 57 err) (378 57 err))) (111
    (91 (65 err 57) (97 err 57)) (123 (112 69 57) (200 err (378 57 err))))
    (99 (91 (65 err 57) (97 err 57)) (123 (100 70 57) (200 err (378 57
    err)))) (117 (91 (65 err 57) (97 err 57)) (123 (118 71 57) (200 err
    (378 57 err)))) (114 (91 (65 err 57) (97 err 57)) (123 (115 72 57) (200
    err (378 57 err)))) (106 (91 (65 err 57) (97 err (105 57 73))) (123 (=
    114 74 57) (200 err (378 57 err)))) (100 (91 (65 err 57) (97 err 57))
    (123 (101 75 57) (200 err (378 57 err)))) (98 (91 (65 err 57) (97 err
    76)) (200 (123 57 err) (378 57 err))) (114 (91 (65 err 57) (97 err 57))
    (123 (115 77 57) (200 err (378 57 err)))) (121 (91 (65 err 57) (97 err
    57)) (123 (122 78 57) (200 err (378 57 err)))) (112 (91 (65 err 57) (97
    err 57)) (123 (113 79 57) (200 err (378 57 err)))) (91 (48 (47 err 80)
    (65 err 37)) (123 (97 err 37) (200 err (378 37 err)))) (114 (91 (65 err
    57) (97 err 57)) (123 (115 81 57) (200 err (378 57 err)))) (108 (91 (65
    err 57) (97 err 57)) (123 (109 82 57) (200 err (378 57 err)))) (114 (91
    (65 err 57) (97 err 57)) (123 (115 83 57) (200 err (378 57 err)))) (115
    (91 (65 err 57) (97 err 57)) (123 (116 84 57) (200 err (378 57 err))))
    (110 (91 (65 err 57) (97 err 57)) (123 (111 85 57) (200 err (378 57
    err)))) (107 (91 (65 err 57) (97 err 57)) (123 (108 86 57) (200 err
    (378 57 err)))) (105 (91 (65 err 57) (97 err 57)) (123 (106 87 57) (200
    err (378 57 err)))) (100 (91 (65 err 57) (97 err 57)) (123 (101 88 57)
    (200 err (378 57 err)))) (105 (91 (65 err 57) (97 err 57)) (123 (106 89
    57) (200 err (378 57 err)))) (111 (91 (65 err 57) (97 err 57)) (123
    (112 90 57) (200 err (378 57 err)))) (101 (91 (65 err 57) (97 err 57))
    (123 (102 91 57) (200 err (378 57 err)))) (= 10 err 80) (100 (91 (65
    err 57) (97 err 57)) (123 (101 92 57) (200 err (378 57 err)))) (117 (91
    (65 err 57) (97 err 57)) (123 (118 93 57) (200 err (378 57 err)))) (99
    (91 (65 err 57) (97 err 57)) (123 (100 94 57) (200 err (378 57 err))))
    (105 (91 (65 err 57) (97 err 57)) (123 (106 95 57) (200 err (378 57
    err)))) (105 (91 (65 err 57) (97 err 57)) (123 (106 96 57) (200 err
    (378 57 err)))) (117 (91 (65 err 57) (97 err 57)) (123 (118 97 57) (200
    err (378 57 err)))) (91 (32 (= 9 98 err) (33 98 (65 err 57))) (124 (97
    err (123 57 99)) (200 err (378 57 err)))) (101 (91 (65 err 57) (97 err
    57)) (123 (102 100 57) (200 err (378 57 err)))) (99 (91 (65 err 57) (97
    err 57)) (123 (100 101 57) (200 err (378 57 err)))) (117 (91 (65 err
    57) (97 err 57)) (123 (118 102 57) (200 err (378 57 err)))) (114 (91
    (65 err 57) (97 err 57)) (123 (115 103 57) (200 err (378 57 err))))
    (109 (91 (65 err 57) (97 err 57)) (123 (110 104 57) (200 err (378 57
    err)))) (100 (91 (65 err 57) (97 err 57)) (123 (101 105 57) (200 err
    (378 57 err)))) (101 (91 (65 err 57) (97 err 57)) (123 (102 106 57)
    (200 err (378 57 err)))) (111 (91 (65 err 57) (97 err 57)) (123 (112
    107 57) (200 err (378 57 err)))) (110 (91 (65 err 57) (97 err 57)) (123
    (111 108 57) (200 err (378 57 err)))) (112 (91 (65 err 57) (97 err 57))
    (123 (113 109 57) (200 err (378 57 err)))) (32 (= 9 98 err) (123 (33 98
    err) (124 99 err))) err (114 (91 (65 err 57) (97 err 57)) (123 (115 110
    57) (200 err (378 57 err)))) (109 (91 (65 err 57) (97 err 57)) (123
    (110 111 57) (200 err (378 57 err)))) (116 (91 (65 err 57) (97 err 57))
    (123 (117 112 57) (200 err (378 57 err)))) (91 (32 (= 9 113 err) (33
    113 (65 err 57))) (124 (97 err (123 57 114)) (200 err (378 57 err))))
    (111 (91 (65 err 57) (97 err 57)) (123 (112 115 57) (200 err (378 57
    err)))) (101 (91 (65 err 57) (97 err 57)) (123 (102 116 57) (200 err
    (378 57 err)))) (102 (91 (65 err 57) (97 err 57)) (123 (103 117 57)
    (200 err (378 57 err)))) (110 (91 (65 err 57) (97 err 57)) (123 (111
    118 57) (200 err (378 57 err)))) (112 (91 (65 err 57) (97 err 57)) (123
    (113 119 57) (200 err (378 57 err)))) (91 (32 (= 9 120 err) (33 120 (65
    err 57))) (124 (97 err (123 57 121)) (200 err (378 57 err)))) (91 (32
    (= 9 122 err) (33 122 (65 err 57))) (124 (97 err (123 57 123)) (200 err
    (378 57 err)))) (111 (91 (65 err 57) (97 err 57)) (123 (112 124 57)
    (200 err (378 57 err)))) (91 (32 (= 9 125 err) (33 125 (65 err 57)))
    (124 (97 err (123 57 126)) (200 err (378 57 err)))) (32 (= 9 113 err)
    (123 (33 113 err) (124 114 err))) err (100 (91 (65 err 57) (97 err 57))
    (123 (101 127 57) (200 err (378 57 err)))) (97 (65 err (91 57 err))
    (200 (123 57 err) (378 57 err))) (105 (91 (65 err 57) (97 err 57)) (123
    (106 128 57) (200 err (378 57 err)))) (47 (11 (9 131 (10 133 132)) (33
    (32 131 133) (46 131 129))) (97 (65 131 (91 130 131)) (200 (123 130
    131) (378 130 131)))) (117 (91 (65 err 57) (97 err 57)) (123 (118 134
    57) (200 err (378 57 err)))) (32 (= 9 120 err) (123 (33 120 err) (124
    121 err))) err (32 (= 9 122 err) (123 (33 122 err) (124 123 err))) err
    (100 (91 (65 err 57) (97 err 57)) (123 (101 135 57) (200 err (378 57
    err)))) (32 (= 9 125 err) (123 (33 125 err) (124 126 err))) err (101
    (91 (65 err 57) (97 err 57)) (123 (102 136 57) (200 err (378 57 err))))
    (108 (91 (65 err 57) (97 err 57)) (123 (109 137 57) (200 err (378 57
    err)))) (11 (10 131 132) (= 46 129 131)) (65 (11 (10 131 132) (= 46 129
    131)) (123 (91 130 (97 131 130)) (200 131 (378 130 131)))) (11 (10 131
    132) (= 46 129 131)) err (32 (10 (9 131 133) (11 132 131)) (46 (33 133
    131) (47 129 131))) (116 (91 (65 err 57) (97 err 57)) (123 (117 138 57)
    (200 err (378 57 err)))) (101 (91 (65 err 57) (97 err 57)) (123 (102
    139 57) (200 err (378 57 err)))) (91 (32 (= 9 140 err) (33 140 (65 err
    57))) (124 (97 err (123 57 141)) (200 err (378 57 err)))) (101 (91 (65
    err 57) (97 err 57)) (123 (102 142 57) (200 err (378 57 err)))) (97 (65
    err (91 57 err)) (200 (123 57 err) (378 57 err))) (91 (32 (= 9 143 err)
    (33 143 (65 err 57))) (124 (97 err (123 57 144)) (200 err (378 57
    err)))) (32 (= 9 140 err) (123 (33 140 err) (124 141 err))) err (109
    (91 (65 err 57) (97 err (108 57 145))) (123 (= 110 146 57) (200 err
    (378 57 err)))) (32 (= 9 143 err) (123 (33 143 err) (124 144 err))) err
    (105 (91 (65 err 57) (97 err 57)) (123 (106 147 57) (200 err (378 57
    err)))) (98 (91 (65 err 57) (97 err 148)) (200 (123 57 err) (378 57
    err))) (110 (91 (65 err 57) (97 err 57)) (123 (111 149 57) (200 err
    (378 57 err)))) (109 (91 (65 err 57) (97 err 57)) (123 (110 150 57)
    (200 err (378 57 err)))) (101 (91 (65 err 57) (97 err 57)) (123 (102
    151 57) (200 err (378 57 err)))) (101 (91 (65 err 57) (97 err 57)) (123
    (102 152 57) (200 err (378 57 err)))) (65 (10 (9 err 153) (= 32 153
    err)) (123 (91 57 (97 err 57)) (200 err (378 57 err)))) (65 (10 (9 err
    154) (= 32 154 err)) (123 (91 57 (97 err 57)) (200 err (378 57 err))))
    (10 (9 err 153) (= 32 153 err)) (10 (9 err 154) (= 32 154 err)))
   '#((#f . #f) (50 . 50) (50 . 50) (49 . 49) (48 . 48) (47 . 47) (45 . 45)
    (44 . 44) (43 . 43) (42 . 42) (41 . 41) (40 . 40) (39 . 39) (38 . 38)
    (37 . 37) (36 . 36) (35 . 35) (34 . 34) (33 . 33) (32 . 32) (31 . 31)
    (30 . 30) (29 . 29) (28 . 28) (25 . 25) (22 . 22) (21 . 21) (20 . 20)
    (17 . 17) (14 . 14) (13 . 13) (50 . 50) (50 . 50) (1 . 1) (22 . 22) (26
    . 26) (#f . #f) (22 . 22) (19 . 19) (18 . 18) (#f . #f) (8 . 8) (8 . 8)
    (46 . 46) (27 . 27) (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23)
    (23 . 23) (23 . 23) (23 . 23) (23 . 23) (22 . 22) (24 . 24) (27 . 27)
    (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23)
    (23 . 23) (23 . 23) (23 . 23) (23 . 23) (22 . 22) (23 . 23) (23 . 23)
    (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23)
    (23 . 23) (23 . 23) (0 . 0) (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23
    . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 .
    23) (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23) (#f . #f) (5 . 5)
    (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23)
    (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23) (#f . #f)
    (2 . 2) (23 . 23) (12 . 12) (23 . 23) (23 . 23) (23 . 23) (#f . #f) (6
    . 6) (#f . #f) (4 . 4) (23 . 23) (#f . #f) (3 . 3) (23 . 23) (23 . 23)
    (#f . #f) (7 . 7) (7 . 7) (7 . 7) (7 . 7) (23 . 23) (23 . 23) (23 . 23)
    (23 . 23) (11 . 11) (23 . 23) (#f . #f) (16 . 16) (23 . 23) (#f . #f)
    (15 . 15) (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23) (23 . 23)
    (10 . 10) (9 . 9) (10 . 10) (9 . 9))))
