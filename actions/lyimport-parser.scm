;; An reminder error. Stops the program and reminds to implement. Only if "Halt on error" is specified"
(define (lyimport::error yytext)
(display (string-append "error: Not implemented yet: " yytext " (Line: "(number->string (lexer-get-line)) " Column: " (number->string (lexer-get-column)) ")\n"))
	(if lyimport::halt_on_error
		(begin (display (string-append "error: Not implemented yet: " yytext " (Line: "(number->string (lexer-get-line)) " Column: " (number->string (lexer-get-column)) ")\n"))
			   (exit))
		yytext
	)
)
(define (lyimport::warning yytext)
	(format #t "Not fully implemented yet: ~a Line: ~a Column: ~a~%~%"
			   
		yytext  (lexer-get-line)  (lexer-get-column)
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
   (


OPEN
CLOSE
;;;;;from parser.yy

 ACCEPTS 

 ADDLYRICS 

 ALIAS 

 ALTERNATIVE 

 BOOK 

 BOOKPART 

 CHANGE 

 CHORDMODE 

 CHORDS 

 CONSISTS 

 CONTEXT 

 DEFAULT 

 DEFAULTCHILD 

 DENIES 

 DESCRIPTION 

 DRUMMODE 

 DRUMS 

 FIGUREMODE 

 FIGURES 

 GROBDESCRIPTIONS 

 HEADER 

 INVALID 

 KEY 

 LAYOUT 

 LYRICMODE 

 LYRICS 

 LYRICSTO 

 MARK 

 MARKUP 

 MARKUPLINES 

 MIDI 

 NAME 

 NOTEMODE 

 OCTAVE 

 ONCE 

 OVERRIDE 

 PAPER 

 PARTIAL 

 RELATIVE 

 REMOVE 

 REPEAT 

 REST 

 REVERT 

 SCORE 

 SEQUENTIAL 

 SET 

 SIMULTANEOUS 

 SKIP 

 TEMPO 

 TIMES 

 TRANSPOSE 

 TYPE 

 UNSET 

 WITH 


BOOK_IDENTIFIER

CHORDMODIFIER_PITCH

CHORD_MODIFIER

CHORD_REPETITION

CONTEXT_DEF_IDENTIFIER

CONTEXT_MOD_IDENTIFIER

DRUM_PITCH

DURATION_IDENTIFIER

EVENT_IDENTIFIER

FRACTION

LYRICS_STRING

LYRIC_MARKUP_IDENTIFIER

MARKUP_FUNCTION

MARKUP_LIST_FUNCTION

MARKUP_IDENTIFIER

MUSIC_FUNCTION

MUSIC_IDENTIFIER

NOTENAME_PITCH

NUMBER_IDENTIFIER

OUTPUT_DEF_IDENTIFIER

REAL

RESTNAME

SCM_IDENTIFIER

SCM_TOKEN

SCORE_IDENTIFIER

STRING

STRING_IDENTIFIER

TONICNAME_PITCH


;/* Keyword token exceptions.  */
TIME_T
NEWCONTEXT

;/* Other string tokens.  */
CHORD_BASS
CHORD_CARET
CHORD_COLON
CHORD_MINUS
CHORD_SLASH
ANGLE_OPEN
ANGLE_CLOSE
DOUBLE_ANGLE_OPEN
DOUBLE_ANGLE_CLOSE
E_BACKSLASH
E_ANGLE_CLOSE
E_CHAR
E_CLOSE
E_EXCLAMATION
E_BRACKET_OPEN
E_OPEN
E_BRACKET_CLOSE
E_ANGLE_OPEN
E_PLUS
E_TILDE
EXTENDER

;;;;;;; Denemo Specials
GRACE
BLOCK
CLEF
BAR
;DBLQUOTE
FERMATA
APPLY_CONTEXT

;;;;;;; Nils tokens for Denemo
WHITESPACE { } ERROR VERSION

SUP_QUOTE SUB_QUOTE PLUS EQUAL BRACKET_OPEN BRACKET_CLOSE TILDE

DIGIT STAR 
DOT 

UNSIGNED EXCLAMATIONMARK QUESTIONMARK 

DENEMODIRECTIVE MULTI_MEASURE_REST E_UNSIGNED 

PIPE 

SLASH 

;;; continuing - substituting literal chars for token names
CARET
UNDERSCORE
HYPHEN




   )

 (lilypond 
		   ()								 : '()  
		   (lilypond toplevel_expression)	 : (append $1 $2) 
		   (lilypond assignment)			 : '() ;(cons 'MUSIC_ASSIGNMENT #t) 
		   ;(lilypond error)				 : #f ;PARSER->error_level_ = 1;
		   ;(lilypond INVALID)				 : #f ;PARSER->error_level_ = 1;
		   
 )
	
 (toplevel_expression
  (lilypond_header)                     : '()
  (score_block)				: (list (cons 'x_MOVEMENT $1) )
  (composite_music)			:  (begin ;(format #t "reached toplevel as composite music  ~a~%" $1) 
                                                   (list (cons 'x_MOVEMENT $1) ))
  (output_def)                          : '()
  (VERSION)                             : '()
  (ERROR)				: (begin (display (string-append "FIXME should not happen:toplevel error: " $1)) '()) 			
  )	
 
 (embedded_scm
	(SCM_TOKEN) : $1 
	(SCM_IDENTIFIER) : $1
 )
 (lilypond_header_body  
  ()                                    : '() )

 (lilypond_header
  (HEADER { lilypond_header_body })   : '() ) 
 

 (assignment_id
	(STRING)						: $1
	;(LYRICS_STRING)				: $1
 )
 
 (assignment
	(assignment_id EQUAL identifier_init)  : (lyimport::as-create $1 $3)  
	;(assignment_id property_path EQUAL identifier_init) : #t ; see next two lines for original actions
		;SCM path = scm_cons (scm_string_to_symbol ($1), $2);
		;PARSER->lexer_->set_identifier (path, $4);
	(embedded_scm) : '()
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
	(embedded_scm) : $1
	(DIGIT): $1 	;$$ = scm_from_int ($1);
	(BLOCK): (cons 'MUSIC_IDENTIFIER (cons 'x_SEQUENTIAL '()))  ;;Added to discard chordmode and lyricmode
	)

 (score_block
		(SCORE { score_body }) 		: $3
 )
 
 (score_body
                (music)						: $1
		(score_body output_def)        : $1
		(score_body lilypond_header)   : $1
  )
 (output_def
                (BLOCK)                : '()
                ;;(output_def_body } ) : '()
)
(output_def_head
                (LAYOUT ) : '()  ;;; see get_layout (PARSER);
)

(output_def_head_with_mode_switch
	(output_def_head) : '()
)
(output_def_body
	(output_def_head_with_mode_switch { ) : '()
)

 (tempo_event
	(TEMPO steno_duration EQUAL bare_unsigned) : (cons 'x_TEMPO $2);(lyimport::error "TEMPO dur = number")  ;	$$ = MAKE_SYNTAX ("tempo", @$, SCM_BOOL_F, $2, scm_from_int ($4));
	(TEMPO string steno_duration EQUAL bare_unsigned) : (lyimport::error "TEMPO strin dur = number") ; $$ = MAKE_SYNTAX ("tempo", @$, make_simple_markup($2), $3, scm_from_int ($5));
	;(TEMPO full_markup steno_duration EQUAL bare_unsigned) : "" ;	$$ = MAKE_SYNTAX ("tempo", @$, $2, $3, scm_from_int ($5));
	(TEMPO string) : "" ;	$$ = MAKE_SYNTAX ("tempoText", @$, make_simple_markup($2) );
	;(TEMPO full_markup) : "" ; $$ = MAKE_SYNTAX ("tempoText", @$, $2 );
  ) 

;;; note in LilyPond's parser the music_list is built in reverse for efficiency
;;; hence the complex rule for building the list with $$ appearing on the lhs of an assignment
 (music_list ;; a list
        ()                      : '()
	(music_list music)	: (append $1 $2) ;;;
	;(music)				: $1  ;;;where does this come from?
	(music_list embedded_scm) : $1 ;;;ignore embedded scheme for now, indeed it appears to be ignored by the LilyPond parser as there is no explicit setting of $$, which defaults to $1
 ) 
 
 (music ;; a list
	(simple_music)					: (list $1)
	(composite_music)				: (begin ;(format #t "~%music as composite_music with value ~a~%"  (list-ref $1 0))
							    $1)
	;;(LYRICMODE music) :  (list (cons 'x_LILYPOND " %Lyrics Omitted\n")) ;;discard lyrics
	;;(CHORDMODE music) :  (list (cons 'x_LILYPOND " %Chords Omitted\n")) ;;discard chords
 )
 
 (alternative_music
	() : '()
	(ALTERNATIVE { music_list }) : $3
  )

 (repeated_music
	(REPEAT simple_string unsigned_number music alternative_music) : (begin
									   ;(format #t "repeat ~a ~a\n\n" (list? (cons (list 'x_REPEAT $2 $3 $4)  $5)) (cons (list 'x_REPEAT $2 $3 $4)  $5))
									   (list (list 'x_REPEAT $2 $3 $4  $5)))	;represent a repeat as a pair the second element holding any alternative music endings
	
 )
	
 (sequential_music ;;; a pair
	( SEQUENTIAL { music_list } )   : (cons 'x_SEQUENTIAL $3)
	(  { music_list }  )			: (begin ;(format #t "Sequential this is a ~a~%" (list? $2)) 
						    (pretty-print $2)
						    (cons 'x_SEQUENTIAL $2))
	)

  (simultaneous_music  ;;; a pair
	(SIMULTANEOUS { music_list }):  (cons 'x_SIMULTANEOUS $3) ;	$$ = MAKE_SYNTAX ("simultaneous-music", @$, scm_car ($3));
	(DOUBLE_ANGLE_OPEN music_list DOUBLE_ANGLE_CLOSE) : (begin 
							      ;(format #t "PARALLEL this is a ~a~%" (list? $2)) (pretty-print $2) (display "finished\n\n")  (pretty-print (cons 'x_Display $2))
							      (cons 'x_SIMULTANEOUS $2)) ;		$$ = MAKE_SYNTAX ("simultaneous-music", @$, scm_car ($2));	
  )
 
 
 (simple_music ;; a pair
	(event_chord)					: (begin ;(format #t "now simple_music ~a~%" $1)
								 $1)	
    (MUSIC_IDENTIFIER)				: $1
    (music_property_def)			: $1
    (context_change)				: $1
    ;;(STRING) : $1 ;;;this would be illegal syntax in music, but allowed in lyrics mode. This line causes a heap of shift/reduce conflicts, it is not sure that these resolve ok.
    ;;(CONTEXT APPLY_CONTEXT SCM_TOKEN) :  ???? so as to parse this ...
    ;;\context Score \applyContext #(set-bar-number-visibility 2)	
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
	(grouped_music_list)		: (begin ;(format #t "reached composite music  ~a~%" $1) 
						 $1)
	
 )
 
 (grouped_music_list ;;; a list
	(simultaneous_music)			: (begin ;(format #t "reached simultaneous rule of grouped music list  ~a is pair?~a ~%" $1 (pair? $1))
							 (list $1) )
	(sequential_music)			: (begin ;(format #t "reached sequential rule of grouped music list  ~a~%" $1)
							 (list $1))
 )
 
 (optional_id
	() : ""
	(EQUAL simple_string) : $2
  )
	

 
 (prefix_composite_music    ;;; a list
;	generic_prefix_music_scm {
;		$$ = run_music_function (PARSER, $1);
;	}
; I think things like \grace have become music functions, and so handled by the generic_prefix_music_scm rule above as we are not doing them...

        (GRACE music) : (list (cons 'x_GRACE  $2)) ;;;Denemo substitute for music function.



	(CONTEXT simple_string optional_id optional_context_mod music) :  (cons (cons 'CONTEXT (list $2 $3 $4)) $5)
;         {       Context_mod *ctxmod = unsmob_context_mod ($4);
;                SCM mods = SCM_EOL;
;                if (ctxmod)
;                        mods = ctxmod->get_mods ();
;		$$ = MAKE_SYNTAX ("context-specification", @$, $2, $3, $5, mods, SCM_BOOL_F);
;	}
	(NEWCONTEXT simple_string optional_id optional_context_mod music) :(begin ;(format #t "newcontext is ~a~%" $2)
										  (cons (cons 'NEWCONTEXT (list $2  $3 $4)) $5))
	(LYRICSTO simple_string optional_id optional_context_mod music) :(begin ;(format #t "newcontext is ~a~%" $2)
										  (cons (cons 'NEWCONTEXT (list $2  $3 $4)) $5))
;   {            Context_mod *ctxmod = unsmob_context_mod ($4);
;                SCM mods = SCM_EOL;
;                if (ctxmod)
;                        mods = ctxmod->get_mods ();
;		$$ = MAKE_SYNTAX ("context-specification", @$, $2, $3, $5, mods, SCM_BOOL_T);
;	}
;
	(TIMES fraction music) : (list (cons 'TIMES (list $2 $3)))
	(repeated_music) : $1 ;		{ $$ = $1; }
	(TRANSPOSE pitch_also_in_chords pitch_also_in_chords music) : $4 ;; ignore for now
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
	(relative_music) : $1
;	| re_rhythmed_music	{ $$ = $1; }
;	;
  )
 


;; mode_changing_head:
;; mode_changing_head_with_context:



 (relative_music
  (RELATIVE absolute_pitch music) : (list (cons 'x_RELATIVE $2) $3)
  (RELATIVE composite_music) :  (list (cons 'x_RELATIVE "c'") $2) ; not sure about this FIXME
  ;;	Pitch middle_c (0, 0, 0);
  ;;	$$ = make_music_relative (middle_c, $2, @$);
  )


;; new_lyrics:
;; re_rhythmed_music:

  (context_change             
	(CHANGE STRING EQUAL STRING) : (cons 'x_CHANGE (string-append $2 $3 $4))  ;		$$ = MAKE_SYNTAX ("context-change", @$, scm_string_to_symbol ($2), $4);
  )

  (property_path
  (SCM_TOKEN) : $1
)
;; property_operation:
;; context_def_mod:
;; context_mod:
;; context_prop_spec:
 (context_prop_spec
	(simple_string) : $1
	(simple_string DOT simple_string) : (string-append $1 "." $3) 
 )

 (simple_music_property_def
	(OVERRIDE context_prop_spec property_path EQUAL scalar) :  (cons 'x_OVERRIDE (cons (cons $2 $3) $5))
	(REVERT context_prop_spec embedded_scm) : (lyimport::error "x_REVERT"); (cons 'x_REVERT (cons $2 $3))
	(APPLY_CONTEXT embedded_scm) : (cons 'x_APPLY_CONTEXT $2)
	(SET context_prop_spec EQUAL scalar) :  (cons 'x_SET (cons $2 $4))
	(UNSET context_prop_spec) :  (lyimport::error "x_UNSET"); (cons 'x_UNSET $2)
)

(music_property_def
       (simple_music_property_def) : $1
       (ONCE simple_music_property_def) : $2
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
    (full_markup) : $1 
    (DIGIT) : $1 

  )
 


;; string:
;; simple_string:
;; scalar:



 (event_chord ;; a pair
	(simple_chord_elements post_events)			: (begin ;(format #t "Post event!!! ~a~%" $2)
									 (cons 'x_CHORD (cons $1 $2)))

	
	;| CHORD_REPETITION optional_notemode_duration post_events {
	;	Input i;
	;	i.set_location (@1, @3);
	;	$$ = MAKE_SYNTAX ("repetition-chord", i,
	;			  PARSER->lexer_->chord_repetition_.last_chord_,
	;			  PARSER->lexer_->chord_repetition_.repetition_function_,
	;			  $2, scm_reverse_x ($3, SCM_EOL));
	(MULTI_MEASURE_REST optional_notemode_duration post_events)  : (cons 'x_MMREST (cons $2 $3))
	
	(command_element) : $1
	
	; note chord elements are memorized into
	;   PARSER->lexer_->chord_repetition_ so that the chord repetition
	;   mechanism copy them when a chord repetition symbol is found
	(note_chord_element) : $1	;	PARSER->lexer_->chord_repetition_.last_chord_ = $$;
 )






(note_chord_element
	(chord_body optional_notemode_duration post_events) : (cons 'x_REALCHORD (cons (cons $1 $2) $3))
 )

(chord_body
	(ANGLE_OPEN chord_body_elements ANGLE_CLOSE) :  $2
)

(chord_body_elements
	() : '()
	(chord_body_element chord_body_elements) : (cons $1 $2) ;;;(list (cons 'x_CHORD $1) $2)
)

(chord_body_element
	(pitch exclamations questions octave_check post_events) : (cons 'x_CHORD (cons (cons 'x_NOTE (list $1 $2 $3 $4)) $5))
)

;; music_function_identifier_musicless_prefix:
;; music_function_chord_body:
;; music_function_event:


 (command_element
	(command_event) : $1
	(SKIP duration_length) : (cons 'x_SKIP $2)    ;	$$ = MAKE_SYNTAX ("skip-music", @$, $2);
	;(BRACKET_OPEN) : (cons 'x_BRACKET_OPEN $1) ;	Music *m = MY_MAKE_MUSIC ("LigatureEvent", @$); m->set_property ("span-direction", scm_from_int (START)); 	$$ = m->unprotect();
	;(BRACKET_CLOSE) : (cons 'x_BRACKET_CLOSE $1) ; Music *m = MY_MAKE_MUSIC ("LigatureEvent", @$); m->set_property ("span-direction", scm_from_int (STOP));	$$ = m->unprotect ();
	(E_BACKSLASH) : (lyimport::error "E_BACKSLASH") ; $$ = MAKE_SYNTAX ("voice-separator", @$, SCM_UNDEFINED);
	(PIPE)		: (cons 'x_BARLINE $1) ; look in parser.yy 
	(PARTIAL duration_length): (cons 'x_PARTIAL (cons (list-ref $2 0) (list-ref $2 1))) ; we get a list here: (duration numberofdots) Moment m = - unsmob_duration ($2)->get_length (); 		$$ = MAKE_SYNTAX ("property-operation", @$, SCM_BOOL_F, ly_symbol2scm ("Timing"), ly_symbol2scm ("PropertySet"), ly_symbol2scm ("measurePosition"), m.smobbed_copy ()); 	$$ = MAKE_SYNTAX ("context-specification", @$, ly_symbol2scm ("Score"), SCM_BOOL_F, $$, SCM_EOL, SCM_BOOL_F);
	(TIME_T fraction) : (cons 'x_TIME $2) ; SCM proc = ly_lily_module_constant ("make-time-signature-set"); $$ = scm_apply_2   (proc, scm_car ($2), scm_cdr ($2), SCM_EOL);
	(MARK scalar) :  (cons 'x_LILYPOND (string-append "\\mark \\markup {" $2 "}"))   ;  (cons 'x_LILYPOND "\\mark \\markup {???}") ;(lyimport::error "MARK scalar") ; SCM proc = ly_lily_module_constant ("make-mark-set"); 	$$ = scm_call_1 (proc, $2);

	(DENEMODIRECTIVE) : (cons 'x_LILYPOND $1)
 )

(command_event
	(TILDE) : (cons 'x_TIE $1) ; $$ = MY_MAKE_MUSIC ("PesOrFlexaEvent", @$)->unprotect ();
	(MARK DEFAULT) : (begin (cons 'x_LILYPOND "\\mark \\default")); 	  {
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
	;;;;;;;;;; in LilyPond these are music-functions which scan_escaped_word looks up the number and type of parameters for, and then pushes these onto the lexer input
	(CLEF STRING) : (cons 'x_CLEF $2)
	(BAR STRING )    : (begin ;(format #t "got bar ~a~%~%" $2) 
				  (cons 'x_BARLINE $2)) 
 )


 (post_events
	() : ""
	(post_events post_event) : (string-append $1 $2)
 )

 (post_event
  ;many things are missing here
  (OPEN)   : $1
  (CLOSE)   : $1
  (BRACKET_OPEN)   : $1
  (BRACKET_CLOSE)  : $1
  (MARKUP) : $1
  (script_dir direction_reqd_event) :  (cond ((equal? $1 'UP) (string-append "^" $2)) ;!!!!!!!! take the \" out of here and require the direction_reqd_event to have them already, then you can have \markup here via gen_text_def and it won't get surrounded by quotes...
					     ((equal? $1 'DOWN) (string-append "_" $2))
					     ((equal? $1 'CENTER) (string-append "-" $2)))
  (string_number_event) : $1 
  (FERMATA) : $1


 )
  

 (string_number_event
	(E_UNSIGNED) : $1
 )


;; direction_less_char:
;; direction_less_event:

 (direction_reqd_event
	(gen_text_def) :     $1;
;; 	}
;; 	| script_abbreviation {
;; 		SCM s = PARSER->lexer_->lookup_identifier ("dash" + ly_scm2string ($1));
;; 		Music *a = MY_MAKE_MUSIC ("ArticulationEvent", @$);
;; 		if (scm_is_string (s))
;; 			a->set_property ("articulation-type", s);
;; 		else PARSER->parser_error (@1, _ ("expecting string as script definition"));
;; 		$$ = a->unprotect ();
;; 	}
)

 (octave_check
	() : "" 
	(EQUAL) : "" ; { $$ = scm_from_int (0); )
	(EQUAL sub_quotes) : $2 ;{ $$ = scm_from_int (-$2); )
	(EQUAL sup_quotes) : $2
  )
 

 (sup_quotes ;;;; a integer
	(SUP_QUOTE)						: 1
	(sup_quotes SUP_QUOTE)			: (+ 1 $1)
 )
 
 (sub_quotes ;;;; a integer
	(SUB_QUOTE)						: -1
	(sub_quotes SUB_QUOTE)			: (- $1 1)
 )
 
 (steno_pitch ;;; a pair
	(NOTENAME_PITCH)				: (cons $1 0)
 	(NOTENAME_PITCH sup_quotes)		: (cons $1 $2)
	(NOTENAME_PITCH sub_quotes)		: (cons $1 $2)
 )


(steno_tonic_pitch
	(TONICNAME_PITCH) : $1
	(TONICNAME_PITCH sup_quotes) : (string-append $1 $2)
	(TONICNAME_PITCH sub_quotes) : (string-append $1 $2)
)




(pitch
	(steno_pitch)					: $1
 )


 (pitch_also_in_chords
	(pitch) : $1
	(steno_tonic_pitch) : $1
 )
 (gen_text_def
        (MARKUP) : (string-append "\\markup {" $1 "}")
	(full_markup) : $1
 	(string) : (string-append "\"" $1 "\"")
 	(FERMATA) : $1
;;	 {
;; 		Music *t = MY_MAKE_MUSIC ("TextScriptEvent", @$);
;; 		t->set_property ("text",
;; 			make_simple_markup ($1));
;; 		$$ = t->unprotect ();
;; 	}
;; 	| DIGIT {
;; 		Music *t = MY_MAKE_MUSIC ("FingeringEvent", @$);
;; 		t->set_property ("digit", scm_from_int ($1));
;; 		$$ = t->unprotect ();
;; 	}
)

;script_abbreviation:

 (script_dir     
	(UNDERSCORE) :   'DOWN
	(CARET) :  'UP 
	(HYPHEN) : 'CENTER
 )
 
 (absolute_pitch
 (steno_pitch) :  $1;
 )

	
 (duration_length
	(multiplied_duration) 			: $1		
 )
	
(optional_notemode_duration
	() 								: ""
	(multiplied_duration) 			: $1
 )



	
 (steno_duration
	(bare_unsigned dots) : (list (string->number $1) $2) ; original lilypond had a check here if there is really a duration before the dots		
	;(DURATION_IDENTIFIER dots) : (string-append $1 $2) 
 )



 (multiplied_duration
	(steno_duration) : $1
	(multiplied_duration STAR bare_unsigned)  	: (list $1 $2 $3);	$$ = unsmob_duration ($$)->compressed ( $3) .smobbed_copy ();
	(multiplied_duration STAR FRACTION) 		: (list $1 $2 $3) ;	Rational  m (scm_to_int (scm_car ($3)), scm_to_int (scm_cdr ($3))); 		$$ = unsmob_duration ($$)->compressed (m).smobbed_copy ();
 )








 (fraction
	(FRACTION) : $1 
	(UNSIGNED SLASH UNSIGNED) : (cons $1 $3) ; $$ = scm_cons (scm_from_int ($1), scm_from_int ($3));
  )
	
 (dots
	() : 0
	(dots DOT) : (+ $1 1)	
  ) 
 


;; tremolo_type:
;; bass_number:
;; figured_bass_alteration:
;; bass_figure:
;; figured_bass_modification:
;; br_bass_figure:
;; figure_list:
;; figure_spec:

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

;; lyric_markup:
;; 	LYRIC_MARKUP_IDENTIFIER {
;; 		$$ = $1;
;; 	}
;; 	| LYRIC_MARKUP
;; 		{ PARSER->lexer_->push_markup_state (); }
;; 	markup_top {
;; 		$$ = $3;
;; 		PARSER->lexer_->pop_state ();
;; 	}


;; full_markup_list:
;; 	MARKUPLINES
;; 		{ PARSER->lexer_->push_markup_state (); }
;; 	markup_list {
;; 		$$ = $3;
;; 		PARSER->lexer_->pop_state ();
;; 	}



(full_markup
        (MARKUP) : $1 ;;; FIXME this needs to be (cons 'x_MARKUP $1) and then something in the parser to turn it into a denemo directive

;; 	MARKUP_IDENTIFIER {
;; 		$$ = $1;
;; 	}
	(MARKUP markup_top) : (string-append "\\"$1 " \"" $2 "\"")

;;; will need a markup lexer... AND it has a mid rule action!!!!!!!!!!! FIXME
;; 		{ PARSER->lexer_->push_markup_state (); }
;; 	markup_top {
;; 		$$ = $3;
;; 		PARSER->lexer_->pop_state ();
;; 	}
)

 (markup_top
 	(markup_list) : $1; {
;; 		$$ = scm_list_2 (ly_lily_module_constant ("line-markup"),  $1);
;; 	}
;; 	| markup_head_1_list simple_markup	{
;; 		$$ = scm_car (scm_call_2 (ly_lily_module_constant ("map-markup-command-list"), $1, scm_list_1 ($2)));
;; 	}
	(simple_markup) : $1
  )
  (markup_list
	;(markup_composed_list) : $1
	(markup_braced_list) : $1 ; {
)
;; 		$$ = $1;
;; 	}
;; 	| markup_command_list {
;; 		$$ = scm_list_1 ($1);
;; 	}
;; 	;

;; markup_composed_list:
;; 	markup_head_1_list markup_braced_list {
;; 		$$ = scm_call_2 (ly_lily_module_constant ("map-markup-command-list"), $1, $2);

;; 	}
;; 	;
 (markup_braced_list
	( { markup_braced_list_body }) : $2
)

 (markup_braced_list_body
	() : '() ;/* empty */	{  $$ = SCM_EOL; }
	(markup_braced_list_body markup) : (list $1 $2)
	(markup_braced_list_body markup_list) : (append $1 $2)
)
;; markup_command_list:
;; 	MARKUP_LIST_FUNCTION markup_command_list_arguments {
;; 	  $$ = scm_cons ($1, scm_reverse_x($2, SCM_EOL));
;; 	}
;; 	;

;; markup_command_basic_arguments:
;; 	EXPECT_MARKUP_LIST markup_command_list_arguments markup_list {
;; 	  $$ = scm_cons ($3, $2);
;; 	}
;; 	| EXPECT_SCM markup_command_list_arguments embedded_scm {
;; 	  $$ = scm_cons ($3, $2);
;; 	}
;; 	| EXPECT_NO_MORE_ARGS {
;; 	  $$ = SCM_EOL;
;; 	}
;; 	;
;; markup_command_list_arguments:
;; 	markup_command_basic_arguments { $$ = $1; }
;; 	| EXPECT_MARKUP markup_command_list_arguments markup {
;; 	  $$ = scm_cons ($3, $2);
;; 	}
;; 	;

;; markup_head_1_item:
;; 	MARKUP_FUNCTION EXPECT_MARKUP markup_command_list_arguments {
;; 	  $$ = scm_cons ($1, scm_reverse_x ($3, SCM_EOL));
;; 	}
;; 	;

;; markup_head_1_list:
;; 	markup_head_1_item	{
;; 		$$ = scm_list_1 ($1);
;; 	}
;; 	| markup_head_1_list markup_head_1_item	{
;; 		$$ = scm_cons ($2, $1);
;; 	}
;; 	;


(simple_markup
	(STRING) : $1 
;;{
;; 		$$ = make_simple_markup ($1);
;; 	}
;; 	| MARKUP_IDENTIFIER {
;; 		$$ = $1;
;; 	}
;; 	| LYRIC_MARKUP_IDENTIFIER {
;; 		$$ = $1;
;; 	}
;; 	| STRING_IDENTIFIER {
;; 		$$ = $1;
;; 	}
;; 	| SCORE {
;; 		SCM nn = PARSER->lexer_->lookup_identifier ("pitchnames");
;; 		PARSER->lexer_->push_note_state (alist_to_hashq (nn));
;; 	} '{' score_body '}' {
;; 		Score * sc = $4;
;; 		$$ = scm_list_2 (ly_lily_module_constant ("score-markup"), sc->self_scm ());
;; 		sc->unprotect ();
;; 		PARSER->lexer_->pop_state ();
;; 	}
;; 	| MARKUP_FUNCTION markup_command_basic_arguments {
;; 		$$ = scm_cons ($1, scm_reverse_x ($2, SCM_EOL));
;; 	}
)
 (markup
;; 	markup_head_1_list simple_markup	{
;; 		SCM mapper = ly_lily_module_constant ("map-markup-command-list");
;; 		$$ = scm_car (scm_call_2 (mapper, $1, scm_list_1 ($2)));
;; 	}
	(simple_markup) :  $1;
)


 	



 

	
 
 
  )
)
