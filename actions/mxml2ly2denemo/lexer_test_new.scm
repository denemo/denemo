;; MusicXML to Lilypond to Denemo
;; A Lexer / Parser to import ly-converted mxml files into Denemo
;; By Richard Shann and Nils Gey, July / August 2010
;; Usage of SILex and LALR-scm 
;;;;;;;;;;;;;;;;;;;;;;;;

;;TOC
;; Libs
;; init input port
;; load and init lexer
;; lalr-parser definition
;; execute the parser
;; cleaning up (input port...)
;;;;;

;; Libs
(load "lalr.scm")
(load "silex.scm")

;; Input Port
(set-current-input-port (open-input-file "input_dummy.txt"))

;; Lexer and helper-functions that return TOKEN indirectly

; List of Notenames
(define lyimport::list_of_notenames
	(list "c" "cis" "ces" "cisis" "ceses" "d" "dis" "des" "disis" "deses" "e" "eis" "es" "ees" "eisis" "eses" "eeses" "f" "fis" "fes" "fisis" "feses" "g" "gis" "ges" "gisis" "geses" "a" "ais" "as" "aes" "aisis" "aeses" "ases" "b" "bis" "bes" "bisis" "beses" "h" "his" "hes" "hisis" "heses")
)

;"Magical Token". Wrapper to make returning a token easier, without all the positions and input ports
(define (lyimport::mtoken symbol value) 
	(make-lexical-token symbol (make-source-location (current-input-port) (lexer-get-line) (lexer-get-column) (lexer-get-offset) -1) value)
)


(define (lyimport::scan_escaped_word yytext)
	(cond
		((string-ci=? "score" yytext) (lyimport::mtoken 'SCORE yytext))
				
		(else (begin (display (string-append "error: Unknown Keyword: " yytext " (Line: "(number->string (lexer-get-line)) " Column: " (number->string (lexer-get-column)) ")\n")) 'ERROR)
		)
		
	)
)

(define (lyimport::scan_bare_word yytext)
	;Helper function to test if the string matches any string of a list of strings, the notenames.
	(define (notename_pitch? yytext)
		(let loop ((counter 0))			
			(cond 
			  ((> (+ counter 1) (length lyimport::list_of_notenames)) #f)
			  ((and (<= counter (length lyimport::list_of_notenames)) (string-ci=? yytext (list-ref lyimport::list_of_notenames counter))) #t)
			  (else (loop (+ counter 1)))
			)
		)
	)

	(cond 
	    ((notename_pitch? yytext) (lyimport::mtoken 'NOTENAME_PITCH yytext))
		(else (lyimport::mtoken 'STRING yytext))
	)
)

;Generate a loadable, standalone lexer file from the .l syntax file.
(lex "mxml2ly2denemo_new.l" "mxml2ly2denemo.l.scm" 'counters 'all) ; Oh no!! The generated scm file has comments in the language of the devil!
(load "mxml2ly2denemo.l.scm")

(lexer-init 'port (current-input-port)) 


;;;;;;; Parser Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Create a list to store notes
(define notelist (list #t))

;Helper to print out a value with a custom description, for console output

(define (display-combo string value)
	(display string)
	
	(display ": ")
	(display value)
	(newline)
) 

(define mxml2ly2denemo-parser

  (lalr-parser
   ;; --- token definitions
   (NOTENAME_PITCH WHITESPACE { } ERROR SCORE SUP_QUOTE SUB_QUOTE PLUS EQUAL STRING DIGIT STAR DURATION_IDENTIFIER DOT FRACTION UNSIGNED EXCLAMATIONMARK QUESTIONMARK REST RESTNAME
   )
		;Problems:
		;DURATION_IDENTIFIER is returned in Lily_lexer::try_special_identifiers (SCM *destination, SCM sid)

 (lilypond 
		   ()								 : ""
		   (lilypond toplevel_expression)	 : (display-combo "toplevel_expression" $2)
		   (lilypond assignment)			 : (display-combo "assignment" $2)
		   ;(lilypond error)				 : #f ;PARSER->error_level_ = 1;
		   ;(lilypond INVALID)				 : #f ;PARSER->error_level_ = 1;
 )
	
 (toplevel_expression
			(score_block)				: $1
			(composite_music)			: $1
			;(WHITESPACE)				: #f			
			(ERROR)						: (display-combo "toplevel error" $1) 			
 )	
 
 (assignment_id
	(STRING)						: $1
	;(LYRICS_STRING)				: $1
 )
 
 (assignment
	(assignment_id EQUAL identifier_init)  : (list $1 $3) ;maybe a hashtable? ;PARSER->lexer_->set_identifier ($1, $3);
	;(assignment_id property_path EQUAL identifier_init) : #t ; see next two lines for original actions
		;SCM path = scm_cons (scm_string_to_symbol ($1), $2);
		;PARSER->lexer_->set_identifier (path, $4);	
 )
 
 (identifier_init
	(score_block) : $1	
	(music) : $1
		; Hack: Create event-chord around standalone events.
		;   Prevents the identifier from being interpreted as a post-event. */
		;Music *mus = unsmob_music ($1);
		;bool is_event = mus &&
		;	(scm_memq (ly_symbol2scm ("event"), mus->get_property ("types"))
		;		!= SCM_BOOL_F);
		;if (!is_event)
		;	$$ = $1;
		;else
		;	$$ = MAKE_SYNTAX ("event-chord", @$, scm_list_1 ($1));
	
	;| post_event {
	;	$$ = $1;
	;}
	;| number_expression {
 	;	$$ = $1;
	;}
	(string) : $1
	(DIGIT): $1 	;$$ = scm_from_int ($1);
	)
 
 (string
	(STRING)				: $1
	;(STRING_IDENTIFIER) 	: $1
	;(string PLUS string) 	: (string-append $1 $3)
 )
 
 (composite_music	
	(grouped_music_list)			: $1
 )
 
 (grouped_music_list
	(sequential_music)				: $1
 )	
 
 (sequential_music
	(  { music_list }  )			: $2
 )
 
 (music_list
	(music_list music)				: (begin (append! notelist (list $2)) notelist) 
	(music)							: (begin (append! notelist (list $1)) notelist)
 ) 
 
 (music
	(simple_music)					: $1
	(composite_music)				: $1 ; for {c { d e } } constructions
 )
 
 (simple_music
	(event_chord)					: $1		
 )
 
 (event_chord
	(simple_chord_elements)			: $1
 )
 
 (score_block
		(SCORE { score_body }) 		: $3		
 )
 
 (score_body
		(music)						: $1
 )
 

 
 (optional_rest
	() : ""
	(REST) : $1
 )
 
 (simple_element
	(pitch exclamations questions octave_check optional_notemode_duration optional_rest) : (string-append $1 $2 $3 $4 $5 $6)  
	(RESTNAME optional_notemode_duration) : (string-append $1 $2)		
 )
 
 (simple_chord_elements
	(simple_element)				: $1
 )
 
 
 (optional_notemode_duration
	() 								: ""
	(multiplied_duration) 			: $1
 )
	
 (duration_length
	(multiplied_duration) 			: $1		
 )
	
 (multiplied_duration
	(steno_duration) : $1
	(multiplied_duration STAR bare_unsigned)  	: $3 ;	$$ = unsmob_duration ($$)->compressed ( $3) .smobbed_copy ();
	(multiplied_duration STAR FRACTION) 		: $3 ;	Rational  m (scm_to_int (scm_car ($3)), scm_to_int (scm_cdr ($3))); 		$$ = unsmob_duration ($$)->compressed (m).smobbed_copy ();
 )
	
 (steno_duration
	(bare_unsigned dots) : (string-append $1 $2) ; original lilypond had a check here if there is really a duration before the dots		
	;(DURATION_IDENTIFIER dots) : (string-append $1 $2) 
 )
	
 (bare_unsigned
 	(UNSIGNED) : $1
	(DIGIT) : $1		
  )

 (unsigned_number
	(bare_unsigned)     : $1 
	;(NUMBER_IDENTIFIER) : $1
 )

 (exclamations
	() : ""
	(exclamations EXCLAMATIONMARK) : (string-append $1 $2)
 )

 (questions
	() : ""
	(questions QUESTIONMARK) : (string-append $1 $2)
 )

	
 (dots
	() : ""
	(dots DOT) : (string-append $1 $2)	
  ) 
 
 (pitch
	(steno_pitch)					: $1
)
 
 (octave_check
	() : "" 
	(EQUAL) : "" ; { $$ = scm_from_int (0); )
	(EQUAL sub_quotes) : $2 ;{ $$ = scm_from_int (-$2); )
	(EQUAL sup_quotes) : $2
  )
 
 (sup_quotes
	(SUP_QUOTE)						: $1
	(sup_quotes SUP_QUOTE)			: (string-append $1 $2)
 )
 
 (sub_quotes
	(SUB_QUOTE)						: $1
	(sub_quotes SUB_QUOTE)			: (string-append $1 $2)
 )
 
 (steno_pitch
	(NOTENAME_PITCH)				: $1
 	(NOTENAME_PITCH sup_quotes)		: (string-append $1 $2)
	(NOTENAME_PITCH sub_quotes)		: (string-append $1 $2)
 )
 
  )
)

; Just to get this out of my way... I don't wanted to make errors anyway! (real function later)
(define (displayerror arg1 arg2)
		(display arg1)
		(display arg2)(newline)
)

(mxml2ly2denemo-parser lexer displayerror)

;; Close input port
(close (current-input-port)) 
