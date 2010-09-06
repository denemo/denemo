;; An reminder error. Stops the program and reminds to implement. Only if "Halt on error" is specified"
(define (lyimport::error yytext)
	(if lyimport::halt_on_error
		(begin (display (string-append "error: Not implemented yet: " yytext " (Line: "(number->string (lexer-get-line)) " Column: " (number->string (lexer-get-column)) ")\n"))
			   (exit))
		#t
	)
)

; If the parser throws an uncatched error it needs this function.
(define (displayerror arg1 arg2)
		(display arg1)
		(display arg2)(newline)
)



;;;;;;; Parser Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mxml2ly2denemo-parser

  (lalr-parser
   ;; --- token definitions
   (NOTENAME_PITCH WHITESPACE { } ERROR SCORE SUP_QUOTE SUB_QUOTE PLUS EQUAL STRING DIGIT STAR DURATION_IDENTIFIER CONTEXT CONTEXT_MOD_IDENTIFIER DOT FRACTION UNSIGNED EXCLAMATIONMARK QUESTIONMARK REST RESTNAME DENEMODIRECTIVE MULTI_MEASURE_REST E_UNSIGNED DOUBLE_ANGLE_CLOSE DOUBLE_ANGLE_OPEN ALTERNATIVE SEQUENTIAL SIMULTANEOUS TIME_T NEWCONTEXT WITH CHANGE REPEAT MUSIC_IDENTIFIER SKIP E_BRACKET_OPEN E_BRACKET_CLOSE E_BACKSLASH PIPE PARTIAL SLASH MARK  E_TILDE DEFAULT TEMPO KEY
    SCM_IDENTIFIER SCM_TOKEN REAL NUMBER_IDENTIFIER COTEXT_MOD_IDENTIFIER QUOTED_CHAR CLEF
   )

 (lilypond 
		   ()								 : '()  
		   (lilypond toplevel_expression)	 : (append $1 $2) 
		   (lilypond assignment)			 : '() ;(cons 'MUSIC_ASSIGNMENT #t) 
		   ;(lilypond error)				 : #f ;PARSER->error_level_ = 1;
		   ;(lilypond INVALID)				 : #f ;PARSER->error_level_ = 1;
		   
 )
	
 (toplevel_expression
			(score_block)				: (list (cons 'x_MOVEMENT $1) )
			(composite_music)			:  (begin (format #t "reached toplevel as composite music  ~a~%" $1) (list (cons 'x_MOVEMENT $1) ))
			(ERROR)						: (display (string-append "toplevel error: " $1)) 			
 )	
 
 (embedded_scm
	(SCM_TOKEN) : $1 
	(SCM_IDENTIFIER) : $1
 )
 
 (assignment_id
	(STRING)						: $1
	;(LYRICS_STRING)				: $1
 )
 
 (assignment
	(assignment_id EQUAL identifier_init)  : (lyimport::as-create $1 $3)  
	;(assignment_id property_path EQUAL identifier_init) : #t ; see next two lines for original actions
		;SCM path = scm_cons (scm_string_to_symbol ($1), $2);
		;PARSER->lexer_->set_identifier (path, $4);	
 )
 
 (identifier_init
	(score_block) :  (cons 'SCORE_IDENTIFIER (list $1))	
	(music) :  (cons 'MUSIC_IDENTIFIER  (list $1))
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
	
	(post_event) : $1
	
	;| number_expression {
 	;	$$ = $1;
	;}
	(string) : $1
	(DIGIT): $1 	;$$ = scm_from_int ($1);
	)

 (simple_music ;; a pair
	(event_chord)					: (begin (format #t "now simple_music ~a~%" $1) $1)	
    (MUSIC_IDENTIFIER)				: $1
    ;(music_property_def)			: $1
	(context_change)				: $1
		
 )
	
 (context_modification
        ;WITH { PARSER->lexer_->push_initial_state (); } '{' context_mod_list '}'
        ;{
        ;        PARSER->lexer_->pop_state ();
        ;        $$ = $4;
        ;}        
        ( WITH CONTEXT_MOD_IDENTIFIER ) : $2       
        ( CONTEXT_MOD_IDENTIFIER ) 		: $1
        
  )
        
	
 (optional_context_mod
        () : ""
        (context_modification) : $1      
  )
       
 
 (composite_music	;;; a list
	(prefix_composite_music) 	: $1
	(grouped_music_list)		: (begin (format #t "reached composite music  ~a~%" $1) $1)
	
 )
 
 (grouped_music_list ;;; a list
	(simultaneous_music)			: (begin (format #t "reached simultaneous rule of grouped music list  ~a is pair?~a ~%" $1 (pair? $1)) (list $1) )
	(sequential_music)			: (begin (format #t "reached sequential rule of grouped music list  ~a~%" $1) (list $1))
 )
 
 (optional_id
	() : ""
	(EQUAL simple_string) : $2
  )
	

 
 (prefix_composite_music    ;;; a list
;	generic_prefix_music_scm {
;		$$ = run_music_function (PARSER, $1);
;	}
	(CONTEXT simple_string optional_id optional_context_mod music) :  (cons (cons 'CONTEXT (list $2 $3 $4)) $5)
;         {       Context_mod *ctxmod = unsmob_context_mod ($4);
;                SCM mods = SCM_EOL;
;                if (ctxmod)
;                        mods = ctxmod->get_mods ();
;		$$ = MAKE_SYNTAX ("context-specification", @$, $2, $3, $5, mods, SCM_BOOL_F);
;	}
	(NEWCONTEXT simple_string optional_id optional_context_mod music) :(begin (format #t "newcontext is ~a~%" $2)   (cons (cons 'NEWCONTEXT (list $2  $3 $4)) $5))
;   {            Context_mod *ctxmod = unsmob_context_mod ($4);
;                SCM mods = SCM_EOL;
;                if (ctxmod)
;                        mods = ctxmod->get_mods ();
;		$$ = MAKE_SYNTAX ("context-specification", @$, $2, $3, $5, mods, SCM_BOOL_T);
;	}
;
;	| TIMES fraction music {
;                $$ = MAKE_SYNTAX ("time-scaled-music", @$, $2, $3);
;	}
;	| repeated_music		{ $$ = $1; }
;	| TRANSPOSE pitch_also_in_chords pitch_also_in_chords music {
;		Pitch from = *unsmob_pitch ($2);
;		Pitch to = *unsmob_pitch ($3);
;		SCM pitch = pitch_interval (from, to).smobbed_copy ();
;		$$ = MAKE_SYNTAX ("transpose-music", @$, pitch, $4);
;	}
;	| mode_changing_head grouped_music_list {
;		if ($1 == ly_symbol2scm ("chords"))
;		{
;		  $$ = MAKE_SYNTAX ("unrelativable-music", @$, $2);
;		}
;		else
;		{
;		  $$ = $2;
;		}
;		PARSER->lexer_->pop_state ();
;	}
;	| mode_changing_head_with_context optional_context_mod grouped_music_list {
;                Context_mod *ctxmod = unsmob_context_mod ($2);
;                SCM mods = SCM_EOL;
;                if (ctxmod)
;                        mods = ctxmod->get_mods ();
;		$$ = MAKE_SYNTAX ("context-specification", @$, $1, SCM_EOL, $3, mods, SCM_BOOL_T);
;		if ($1 == ly_symbol2scm ("ChordNames"))
;		{
;		  $$ = MAKE_SYNTAX ("unrelativable-music", @$, $$);
;		}
;		PARSER->lexer_->pop_state ();
;	}
;	| relative_music	{ $$ = $1; }
;	| re_rhythmed_music	{ $$ = $1; }
;	;
  )
 
  (context_change
	(CHANGE STRING EQUAL STRING) : (string-append $1 $2 $3 $4)  ;		$$ = MAKE_SYNTAX ("context-change", @$, scm_string_to_symbol ($2), $4);
  )
	 
 (music_list ;; a list
	(music_list music)	: (append $1 $2) ;;;
	(music)				: $1
 ) 
 
 (music ;; a list
	(simple_music)					: (list $1)
	(composite_music)				: (begin (format #t "~%music as composite_music with value ~a~%" (car (list-ref $1 0)))
             
;;this comes in as either this
;;music as composite_music with value (NEWCONTEXT Staff  )

;;or this...
;;.........with value (x_SEQUENTIAL (x_CHORD

;;output 
                                                   ;;(if (eqv? 'NEWCONTEXT (car (list-ref $1 0)))
;;						       (cons 'x_NEWCONTEXT  $1)
;;						       (cons 'x_COMPOSITE_MUSIC $1))) ; for {c { d e } } constructions
$1)
	
 )
 
 (alternative_music
	() : ""
	(ALTERNATIVE { music_list }) : $3
  )

 (repeated_music
	(REPEAT simple_string unsigned_number music alternative_music) : (list $1 $2 $3 $4 $5)	;	$$ = MAKE_SYNTAX ("repeat", @$, $2, $3, $4, $5);
	
 )
	
 (sequential_music ;;; a pair
	( SEQUENTIAL { music_list } )   : (cons 'x_SEQUENTIAL $3)
	(  { music_list }  )			: (begin (format #t "Sequential this is a ~a~%" (list? $2)) 
    (pretty-print $2)
 (cons 'x_SEQUENTIAL $2))
 )

  (simultaneous_music  ;;; a pair
	(SIMULTANEOUS { music_list }):  (cons 'x_SIMULTANEOUS $3) ;	$$ = MAKE_SYNTAX ("simultaneous-music", @$, scm_car ($3));
	(DOUBLE_ANGLE_OPEN music_list DOUBLE_ANGLE_CLOSE) : (begin (format #t "PARALLEL this is a ~a~%" (list? $2)) (pretty-print $2) (display "finished\n\n")  (pretty-print (cons 'x_Display $2)) (cons 'x_SIMULTANEOUS $2)) ;		$$ = MAKE_SYNTAX ("simultaneous-music", @$, scm_car ($2));	
  )
 
 
 (string
	(STRING)				: $1
	;(STRING_IDENTIFIER) 	: $1
	;(string PLUS string) 	: (string-append $1 $3)
 )
 
 (simple_string
   (STRING) 			 : $1
   ;(LYRICS_STRING)	 : $1	
	;(STRING_IDENTIFIER) : $1
  ) 
 
 (scalar
   (string) : $1
   ;(LYRICS_STRING) : $1 
    (bare_number) : $1  
    (embedded_scm) : $1 
	;(full_markup) : $1 
	(DIGIT) : $1 
  )
 
 (event_chord ;; a pair
	(simple_chord_elements post_events)			: (begin (format #t "Parser reached ~a~%" $1) (cons 'x_CHORD (cons $1 $2)))
	(MULTI_MEASURE_REST optional_notemode_duration post_events)  : (cons 'x_MMREST (cons $2 $3))
	
	;| CHORD_REPETITION optional_notemode_duration post_events {
	;	Input i;
	;	i.set_location (@1, @3);
	;	$$ = MAKE_SYNTAX ("repetition-chord", i,
	;			  PARSER->lexer_->chord_repetition_.last_chord_,
	;			  PARSER->lexer_->chord_repetition_.repetition_function_,
	;			  $2, scm_reverse_x ($3, SCM_EOL));
	
	(command_element) : $1
	
	; note chord elements are memorized into
	;   PARSER->lexer_->chord_repetition_ so that the chord repetition
	;   mechanism copy them when a chord repetition symbol is found
	;(note_chord_element) : $1	;	PARSER->lexer_->chord_repetition_.last_chord_ = $$;
 )
 
 (score_block
		(SCORE { score_body }) 		: $3
 )
 
 (score_body
		(music)						: $1
 )
 
 (tempo_event
	(TEMPO steno_duration EQUAL bare_unsigned) : (lyimport::error "TEMPO dur = number")  ;	$$ = MAKE_SYNTAX ("tempo", @$, SCM_BOOL_F, $2, scm_int2num ($4));
	(TEMPO string steno_duration EQUAL bare_unsigned) : (lyimport::error "TEMPO strin dur = number") ; $$ = MAKE_SYNTAX ("tempo", @$, make_simple_markup($2), $3, scm_int2num ($5));
	;(TEMPO full_markup steno_duration EQUAL bare_unsigned) : "" ;	$$ = MAKE_SYNTAX ("tempo", @$, $2, $3, scm_int2num ($5));
	(TEMPO string) : "" ;	$$ = MAKE_SYNTAX ("tempoText", @$, make_simple_markup($2) );
	;(TEMPO full_markup) : "" ; $$ = MAKE_SYNTAX ("tempoText", @$, $2 );
  ) 

 
 (optional_rest
	() : ""
	(REST) : $1
 )
 
 (simple_element
	(pitch exclamations questions octave_check optional_notemode_duration optional_rest) : (cons 'x_NOTE (list  $1 $2 $3 $4 $5 $6))
	(RESTNAME optional_notemode_duration) : (cons 'x_REST (list $1 $2))	
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
	(multiplied_duration STAR bare_unsigned)  	: (string-append $1 $2 $3);	$$ = unsmob_duration ($$)->compressed ( $3) .smobbed_copy ();
	(multiplied_duration STAR FRACTION) 		: (string-append $1 $2 $3) ;	Rational  m (scm_to_int (scm_car ($3)), scm_to_int (scm_cdr ($3))); 		$$ = unsmob_duration ($$)->compressed (m).smobbed_copy ();
 )
	
 (steno_duration
	(bare_unsigned dots) : (string-append $1 $2) ; original lilypond had a check here if there is really a duration before the dots		
	;(DURATION_IDENTIFIER dots) : (string-append $1 $2) 
 )
	
 (bare_number
	(UNSIGNED) : $1
	(REAL) : $1
	(NUMBER_IDENTIFIER) : $1
	(REAL NUMBER_IDENTIFIER) :  (lyimport::error "REAL NUMBER_IDENTIFIER") ;	$$ = scm_from_double (scm_to_double ($1) *scm_to_double ($2));
	(UNSIGNED NUMBER_IDENTIFIER) : (lyimport::error "UNSIGNED NUMBER_IDENTIFIER")  ;	$$ = scm_from_double ($1 *scm_to_double ($2));
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

 (fraction
	(FRACTION) : $1 
	(UNSIGNED SLASH UNSIGNED) : (cons $1 $3) ; $$ = scm_cons (scm_from_int ($1), scm_from_int ($3));
  )
	
 (dots
	() : ""
	(dots DOT) : (string-append $1 $2)	
  ) 
 
 (pitch
	(steno_pitch)					: $1
 )
 
 (command_element
	(command_event) : $1
	(SKIP duration_length) : (string-append $1 $2)    ;	$$ = MAKE_SYNTAX ("skip-music", @$, $2);
	(E_BRACKET_OPEN) : (lyimport::error "E_BRACKET_OPEN") ;	Music *m = MY_MAKE_MUSIC ("LigatureEvent", @$); m->set_property ("span-direction", scm_from_int (START)); 	$$ = m->unprotect();
	(E_BRACKET_CLOSE) : (lyimport::error "E_BRACKET_CLOSE") ; Music *m = MY_MAKE_MUSIC ("LigatureEvent", @$); m->set_property ("span-direction", scm_from_int (STOP));	$$ = m->unprotect ();
	(E_BACKSLASH) : (lyimport::error "E_BACKSLASH") ; $$ = MAKE_SYNTAX ("voice-separator", @$, SCM_UNDEFINED);
	(PIPE)		: (cons 'x_BARLINE $1) ; look in parser.yy 
	(PARTIAL duration_length): (lyimport::error "PARTIAL") ;		Moment m = - unsmob_duration ($2)->get_length (); 		$$ = MAKE_SYNTAX ("property-operation", @$, SCM_BOOL_F, ly_symbol2scm ("Timing"), ly_symbol2scm ("PropertySet"), ly_symbol2scm ("measurePosition"), m.smobbed_copy ()); 	$$ = MAKE_SYNTAX ("context-specification", @$, ly_symbol2scm ("Score"), SCM_BOOL_F, $$, SCM_EOL, SCM_BOOL_F);
	(TIME_T fraction) : (cons 'x_TIME $2) ; SCM proc = ly_lily_module_constant ("make-time-signature-set"); $$ = scm_apply_2   (proc, scm_car ($2), scm_cdr ($2), SCM_EOL);
	(MARK scalar) : (lyimport::error "MARK scalar") ; SCM proc = ly_lily_module_constant ("make-mark-set"); 	$$ = scm_call_1 (proc, $2);
 )

 (command_event
	(E_TILDE) : (lyimport::error "E_TILDE") ; $$ = MY_MAKE_MUSIC ("PesOrFlexaEvent", @$)->unprotect ();
	(MARK DEFAULT) : (lyimport::error "MARK DEFAULT") ; 	  {
						;Music *m = MY_MAKE_MUSIC ("MarkEvent", @$);
						;$$ = m->unprotect ();
	(tempo_event) :  $1
	(KEY DEFAULT) : (cons 'x_KEY (cons $2 #f)) ;Music *key = MY_MAKE_MUSIC ("KeyChangeEvent", @$);
					   ;$$ = key->unprotect ();
	(KEY NOTENAME_PITCH SCM_IDENTIFIER)	: (cons 'x_KEY  (cons $2 $3))
		;Music *key = MY_MAKE_MUSIC ("KeyChangeEvent", @$);
		;if (scm_ilength ($3) > 0)
		;{
		;	key->set_property ("pitch-alist", $3);
		;	key->set_property ("tonic", Pitch (0, 0, 0).smobbed_copy ());
		;	key->transpose (* unsmob_pitch ($2));
		;} else {
		;	PARSER->parser_error (@3, _ ("second argument must be pitch list"));
		;}

		;$$ = key->unprotect ();
		
	;;;;;;;THESE ARE CUSTOM EVENTS DONE BY DENEMO AND NOT ORIGINAL LILYPOND;;;;;;;;;;
	;;;;;;;;;;
	(CLEF STRING) : (cons 'x_CLEF $2)
	
 )

 (post_events
	() : ""
	(post_events post_event) : (string-append $1 $2)
 )

 (post_event
  ;many things are missing here
  (string_number_event) : $1 
 )
  
 (string_number_event
	(E_UNSIGNED) : $1
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
