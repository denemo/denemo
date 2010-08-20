;; MusicXML to Lilypond to Denemo
;; A Lexer / Parser to import ly-converted mxml files into Denemo or other formats by modifying the lyimport:: functions
;; By Richard Shann and Nils Gey, July / August 2010
;; Usage of SILex and LALR-scm 
; This file is part of Denemo, http://www.denemo.org
;
;  Its based on Lilyponds parser.yy: 
;		Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>
;								 Jan Nieuwenhuizen <janneke@gnu.org>
;								 
; Denemo is free software: you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation, either version 3 of the License, or
;  (at your option) any later version.
;
;  Denemo is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with Denemo.  If not, see <http://www.gnu.org/licenses/>.
;;
;;TOC
;; Libs, 
;; init input port
;; Initialize lists and tables
;; Prepare lyimport:: functions
;; load and init lexer
;; lalr-parser definition
;; execute the parser
;; cleaning up (input port...)
;;;;;

;; Libs
(load "lalr.scm")
(load "silex.scm")

;; Input Port
(set-current-input-port (open-input-file "mytest.ly"))

;; Lexer and helper-functions that return TOKEN indirectly

;; Lists to save music and create the final output
(define current_notelist '())
(define final_list (list 'x_LIST ))


; List of Notenames
(define lyimport::list_of_notenames
	(list "c" "cis" "ces" "cisis" "ceses" "d" "dis" "des" "disis" "deses" "e" "eis" "es" "ees" "eisis" "eses" "eeses" "f" "fis" "fes" "fisis" "feses" "g" "gis" "ges" "gisis" "geses" "a" "ais" "as" "aes" "aisis" "aeses" "ases" "b" "bis" "bes" "bisis" "beses" "h" "his" "hes" "hisis" "heses")
)

; List of Special Identifiers
 (define lyimport::list_of_special_scm_identifiers
	(list "major" "minor" "dorian" "phrygian" "lydian" "mixolydian")
 )  

;Blank Table of Assignments
(define lyimport::AssignmentTable (make-hash-table))
	;(hashq-set! lyimport::AssignmentTable 'name_of_assignment_var_as_symbol value_of_assignment) ; Template



; Assignment functions. Wants a string as type and something of your own choice (for example a music list)
(define (lyimport::as-create key pair-type-and-value)
		(hashq-set! lyimport::AssignmentTable (string->symbol key) pair-type-and-value)
)

(define (lyimport::as-type key)
		(car (hashq-ref lyimport::AssignmentTable (string->symbol key)))
		
)

(define (lyimport::as-value key)
    	(cdr (hashq-ref lyimport::AssignmentTable (string->symbol key)))
)	


(define (lyimport::as-eval key)		
	    (lyimport::mtoken 'MUSIC_IDENTIFIER (lyimport::as-value key))
)


;Lilyponds "try_special_identifiers" is a part of scan_escaped_words in Denemo and needs this function:
 (define (lyimport::try_special_identifiers_scm? yytext)
 	
 	;Helper function to test if the string matches any string of a list of strings, the scm identifiers.
	(define (scm_identifier?)
		(let loop ((counter 0))			
			(cond 
			  ((> (+ counter 1) (length lyimport::list_of_special_scm_identifiers)) #f)
			  ((and (<= counter (length lyimport::list_of_special_scm_identifiers)) (string-ci=? yytext (list-ref lyimport::list_of_special_scm_identifiers counter))) #t)
			  (else (loop (+ counter 1)))
			)
		)
	)

	(scm_identifier?)
)


;"Magical Token". Wrapper to make returning a token easier, without all the positions and input ports
(define (lyimport::mtoken symbol value) 
	(make-lexical-token symbol (make-source-location (current-input-port) (lexer-get-line) (lexer-get-column) (lexer-get-offset) -1) value)
)	

(define (lyimport::error yytext)
	(display (string-append "error: Not implemented yet: " yytext " (Line: "(number->string (lexer-get-line)) " Column: " (number->string (lexer-get-column)) ")\n"))
	(exit)
)


(define (lyimport::scan_escaped_word yytext)

	(cond
		((string-ci=? "score" yytext) (lyimport::mtoken 'SCORE yytext))
				
		; Converted from Denemo
		((string-ci=? "alias" yytext) (lyimport::mtoken 'ALIAS yytext))
		((string-ci=? "apply" yytext) (lyimport::mtoken 'APPLY yytext))
		((string-ci=? "arpeggio" yytext) (lyimport::mtoken 'ARPEGGIO yytext))
		((string-ci=? "autochange" yytext) (lyimport::mtoken 'AUTOCHANGE yytext))
		((string-ci=? "spanrequest" yytext) (lyimport::mtoken 'SPANREQUEST yytext))
		((string-ci=? "commandspanrequest" yytext) (lyimport::mtoken 'COMMANDSPANREQUEST yytext))
		((string-ci=? "simultaneous" yytext) (lyimport::mtoken 'SIMULTANEOUS yytext))
		((string-ci=? "sequential" yytext) (lyimport::mtoken 'SEQUENTIAL yytext))
		((string-ci=? "accepts" yytext) (lyimport::mtoken 'ACCEPTS}, yytext))
		((string-ci=? "alternative" yytext) (lyimport::mtoken 'ALTERNATIVE yytext))
		((string-ci=? "bar" yytext) (lyimport::mtoken 'BAR yytext))
		((string-ci=? "breathe" yytext) (lyimport::mtoken 'BREATHE yytext))
		((string-ci=? "break" yytext) (lyimport::mtoken 'BREAK yytext))
		((string-ci=? "char" yytext) (lyimport::mtoken 'CHAR_T yytext))
		((string-ci=? "chordmodifiers" yytext) (lyimport::mtoken 'CHORDMODIFIERS yytext))
		((string-ci=? "chords" yytext) (lyimport::mtoken 'CHORDS yytext))
		((string-ci=? "clef" yytext) (lyimport::mtoken 'CLEF yytext))
		((string-ci=? "cm" yytext) (lyimport::mtoken 'CM_T yytext))
		((string-ci=? "consists" yytext) (lyimport::mtoken 'CONSISTS yytext))
		((string-ci=? "consistsend" yytext) (lyimport::mtoken 'CONSISTSEND yytext))
		((string-ci=? "context" yytext) (lyimport::mtoken 'CONTEXT yytext))
		((string-ci=? "default" yytext) (lyimport::mtoken 'DEFAULT yytext))
		((string-ci=? "denies" yytext) (lyimport::mtoken 'DENIES yytext))
		((string-ci=? "duration" yytext) (lyimport::mtoken 'DURATION yytext))
		((string-ci=? "dynamicscript" yytext) (lyimport::mtoken 'DYNAMICSCRIPT yytext))
		((string-ci=? "grobdescriptions" yytext) (lyimport::mtoken 'GROBDESCRIPTIONS yytext))
		((string-ci=? "figures" yytext) (lyimport::mtoken 'FIGURES yytext))
		((string-ci=? "grace" yytext) (lyimport::mtoken 'GRACE yytext))
		((string-ci=? "glissando" yytext) (lyimport::mtoken 'GLISSANDO yytext))
		((string-ci=? "header" yytext) (lyimport::mtoken 'HEADER yytext))
		((string-ci=? "in" yytext) (lyimport::mtoken 'IN_T yytext))
		((string-ci=? "key" yytext) (lyimport::mtoken 'KEY yytext))
		((string-ci=? "mark" yytext) (lyimport::mtoken 'MARK yytext))
		((string-ci=? "new" yytext) (lyimport::mtoken 'NEWCONTEXT yytext))
		((string-ci=? "pitch" yytext) (lyimport::mtoken 'PITCH yytext))
		((string-ci=? "time" yytext) (lyimport::mtoken 'TIME_T yytext))
		((string-ci=? "times" yytext) (lyimport::mtoken 'TIMES yytext))
		((string-ci=? "layout" yytext) (lyimport::mtoken 'LAYOUT yytext))
		((string-ci=? "lyricmode" yytext) (lyimport::mtoken 'LYRICMODE yytext))
		((string-ci=? "lyrics" yytext) (lyimport::mtoken 'LYRICS yytext))
		((string-ci=? "lyricsto" yytext) (lyimport::mtoken 'LYRICSTO yytext))
		((string-ci=? "midi" yytext) (lyimport::mtoken 'MIDI yytext))
		((string-ci=? "mm" yytext) (lyimport::mtoken 'MM_T yytext))
		((string-ci=? "name" yytext) (lyimport::mtoken 'NAME yytext))
		((string-ci=? "pitchnames" yytext) (lyimport::mtoken 'PITCHNAMES yytext))
		((string-ci=? "notes" yytext) (lyimport::mtoken 'NOTES yytext))
		((string-ci=? "outputproperty" yytext) (lyimport::mtoken 'OUTPUTPROPERTY yytext))
		((string-ci=? "override" yytext) (lyimport::mtoken 'OVERRIDE yytext))
		((string-ci=? "set" yytext) (lyimport::mtoken 'SET yytext))
		((string-ci=? "rest" yytext) (lyimport::mtoken 'REST yytext))
		((string-ci=? "revert" yytext) (lyimport::mtoken 'REVERT yytext))
		((string-ci=? "partial" yytext) (lyimport::mtoken 'PARTIAL yytext))
		((string-ci=? "paper" yytext) (lyimport::mtoken 'PAPER yytext))
		((string-ci=? "penalty" yytext) (lyimport::mtoken 'PENALTY yytext))
		((string-ci=? "property" yytext) (lyimport::mtoken 'PROPERTY yytext))
		((string-ci=? "pt" yytext) (lyimport::mtoken 'PT_T yytext))
		((string-ci=? "relative" yytext) (lyimport::mtoken 'RELATIVE yytext))
		((string-ci=? "remove" yytext) (lyimport::mtoken 'REMOVE yytext))
		((string-ci=? "repeat" yytext) (lyimport::mtoken 'REPEAT yytext))
		((string-ci=? "addlyrics" yytext) (lyimport::mtoken 'ADDLYRICS yytext))
		((string-ci=? "partcombine" yytext) (lyimport::mtoken 'PARTCOMBINE yytext))
		((string-ci=? "score" yytext) (lyimport::mtoken 'SCORE yytext))
		((string-ci=? "script" yytext) (lyimport::mtoken 'SCRIPT yytext))
		((string-ci=? "stylesheet" yytext) (lyimport::mtoken 'STYLESHEET yytext))
		((string-ci=? "skip" yytext) (lyimport::mtoken 'SKIP yytext))
		((string-ci=? "tempo" yytext) (lyimport::mtoken 'TEMPO yytext))
		((string-ci=? "translator" yytext) (lyimport::mtoken 'TRANSLATOR yytext))
		((string-ci=? "transpose" yytext) (lyimport::mtoken 'TRANSPOSE yytext))
		((string-ci=? "type" yytext) (lyimport::mtoken 'TYPE yytext))
		((string-ci=? "unset" yytext) (lyimport::mtoken 'UNSET yytext))
		((string-ci=? "version" yytext) (lyimport::mtoken 'LILYVERSION yytext))

		;From parser.yy %token TOKEN "\\keyword"
		((string-ci=? "change" yytext) (lyimport::mtoken 'CHANGE yytext))
		((string-ci=? "with" yytext) (lyimport::mtoken 'WITH yytext))		
		((string-ci=? "book" yytext) (lyimport::mtoken 'BOOK yytext))
		((string-ci=? "bookpart" yytext) (lyimport::mtoken 'BOOKPART yytext))
		((string-ci=? "chordmode" yytext) (lyimport::mtoken 'CHORDMODE yytext))
		((string-ci=? "defaultchild" yytext) (lyimport::mtoken 'DEFAULTCHILD yytext))
		((string-ci=? "description" yytext) (lyimport::mtoken 'DESCRIPTION yytext))
		((string-ci=? "drummode" yytext) (lyimport::mtoken 'DRUMMODE yytext))
		((string-ci=? "drums" yytext) (lyimport::mtoken 'DRUMS yytext))
		((string-ci=? "figuremode" yytext) (lyimport::mtoken 'FIGUREMODE yytext))
		((string-ci=? "invalid" yytext) (lyimport::mtoken 'INVALID yytext))
		((string-ci=? "markup" yytext) (lyimport::mtoken 'MARKUP yytext))
		((string-ci=? "markuplines" yytext) (lyimport::mtoken 'MARKUPLINES yytext))
		((string-ci=? "notemode" yytext) (lyimport::mtoken 'NOTEMODE yytext))
		((string-ci=? "octave" yytext) (lyimport::mtoken 'OCTAVE yytext))
		
		; Denemo specific
		((string-ci=? "barNumberCheck" yytext) (lyimport::mtoken 'DENEMODIRECTIVE yytext))
		
        ;If its not a known keyword its probably a user assignment:
        ((hashq-ref lyimport::AssignmentTable (string->symbol yytext)) (lyimport::as-eval yytext))
        
        ;Next test for an try_special_identifiers
		((lyimport::try_special_identifiers_scm? yytext) (lyimport::mtoken 'SCM_IDENTIFIER yytext))
        
        ;If its not a keyword, assignment or special identifier then its wrong				
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

;		((string-ci=? "" yytext) (lyimport::mtoken ' yytext))				
		(else (lyimport::mtoken 'STRING yytext))
		
	)
)

(define (lyimport::scan_fraction yytext)
	(lyimport::mtoken 'FRACTION yytext)
	
	
	;{
	;ssize i = frac.find ('/');
	;string left = frac.substr (0, i);
	;string right = frac.substr (i + 1, (frac.length () - i + 1));

	;int n = String_convert::dec2int (left);
	;int d = String_convert::dec2int (right);
	;return scm_cons (scm_from_int (n), scm_from_int (d));
    ;}

)

;Generate a loadable, standalone lexer file from the .l syntax file.
(lex "mxml2ly2denemo_new.l" "mxml2ly2denemo.l.scm" 'counters 'all) 
(load "mxml2ly2denemo.l.scm")

(lexer-init 'port (current-input-port)) 


;;;;;;; Parser Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
   (NOTENAME_PITCH WHITESPACE { } ERROR SCORE SUP_QUOTE SUB_QUOTE PLUS EQUAL STRING DIGIT STAR DURATION_IDENTIFIER CONTEXT CONTEXT_MOD_IDENTIFIER DOT FRACTION UNSIGNED EXCLAMATIONMARK QUESTIONMARK REST RESTNAME DENEMODIRECTIVE MULTI_MEASURE_REST E_UNSIGNED DOUBLE_ANGLE_CLOSE DOUBLE_ANGLE_OPEN ALTERNATIVE SEQUENTIAL SIMULTANEOUS TIME_T NEWCONTEXT WITH CHANGE REPEAT MUSIC_IDENTIFIER SKIP E_BRACKET_OPEN E_BRACKET_CLOSE E_BACKSLASH PIPE PARTIAL SLASH MARK  E_TILDE DEFAULT TEMPO KEY
    SCM_IDENTIFIER SCM_TOKEN REAL NUMBER_IDENTIFIER COTEXT_MOD_IDENTIFIER QUOTED_CHAR CLEF
   )

 (lilypond 
		   ()								 : ""  
		   (lilypond toplevel_expression)	 : (append! final_list $2) ; (display-combo "toplevel_expression" $2)
		   (lilypond assignment)			 : (cons 'MUSIC_ASSIGNMENT #t) 
		   ;(lilypond error)				 : #f ;PARSER->error_level_ = 1;
		   ;(lilypond INVALID)				 : #f ;PARSER->error_level_ = 1;
		   
 )
	
 (toplevel_expression
			(score_block)				: $1
			(composite_music)			: $1			
			(ERROR)						: (display-combo "toplevel error" $1) 			
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
	(score_block) : (begin (display "Found scoreblock as ident while creating assignment: ") (display $1)(newline)  (cons 'SCORE_IDENTIFIER (cons 'x_LIST $1))	)
	(music) : (begin (display "Found music as ident while creating assignment: ") (display $1)(newline) (cons 'MUSIC_IDENTIFIER (cons 'x_LIST $1)))
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

 (simple_music
	(event_chord)					: $1	
    (MUSIC_IDENTIFIER)				: (begin (display "Music_Ident was called by a keyword: ")(display $1)(newline) $1) 
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
       
 
 (composite_music	
	(prefix_composite_music) 	: (begin (display "prefix: ") (display $1)(newline) $1)
	(grouped_music_list)		: $1
	
 )
 
 (grouped_music_list
	(simultaneous_music)			: $1
	(sequential_music)				: $1
 )	
 
 (optional_id
	() : ""
	(EQUAL simple_string) : $2
  )
	

 
 (prefix_composite_music
;	generic_prefix_music_scm {
;		$$ = run_music_function (PARSER, $1);
;	}
	(CONTEXT simple_string optional_id optional_context_mod music) : $5 
;         {       Context_mod *ctxmod = unsmob_context_mod ($4);
;                SCM mods = SCM_EOL;
;                if (ctxmod)
;                        mods = ctxmod->get_mods ();
;		$$ = MAKE_SYNTAX ("context-specification", @$, $2, $3, $5, mods, SCM_BOOL_F);
;	}
	(NEWCONTEXT simple_string optional_id optional_context_mod music) :  $5
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
	 
 (music_list
	(music_list music)				: (begin (append! current_notelist (list $2)) current_notelist) 
	(music)							: (begin (set! current_notelist (list $1)) current_notelist)
 ) 
 
 (music
	(simple_music)					: $1
	(composite_music)				: $1 ; for {c { d e } } constructions
	
 )
 
 (alternative_music
	() : ""
	(ALTERNATIVE { music_list }) : $3
  )

 (repeated_music
	(REPEAT simple_string unsigned_number music alternative_music) : (list $1 $2 $3 $4 $5)	;	$$ = MAKE_SYNTAX ("repeat", @$, $2, $3, $4, $5);
	
 )
	
 (sequential_music
	( SEQUENTIAL { music_list } )   : $3
	(  { music_list }  )			: $2
 )

  (simultaneous_music
	(SIMULTANEOUS { music_list }): $3 ;	$$ = MAKE_SYNTAX ("simultaneous-music", @$, scm_car ($3));
	(DOUBLE_ANGLE_OPEN music_list DOUBLE_ANGLE_CLOSE) : $2 ;		$$ = MAKE_SYNTAX ("simultaneous-music", @$, scm_car ($2));	
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
 
 (event_chord
	(simple_chord_elements post_events)			: (cons 'x_CHORD (string-append $1 $2))
	(MULTI_MEASURE_REST optional_notemode_duration post_events)  : (cons 'x_MMREST (string-append $2 $3))
	
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
	(pitch exclamations questions octave_check optional_notemode_duration optional_rest) : (string-append  $1 $2 $3 $4 $5 $6)
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
	(REAL NUMBER_IDENTIFIER) :  "" ;	$$ = scm_from_double (scm_to_double ($1) *scm_to_double ($2));
	(UNSIGNED NUMBER_IDENTIFIER) : ""  ;	$$ = scm_from_double ($1 *scm_to_double ($2));
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
	(CLEF QUOTED_CHAR) : (cons 'x_CLEF $2)
	
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

; Just to get this out of my way... I don't wanted to make errors anyway! (real function later)
(define (displayerror arg1 arg2)
		(display arg1)
		(display arg2)(newline)
)


(mxml2ly2denemo-parser lexer displayerror)

(newline)
(display ":::::::: Parser Finished ::::::::::")(newline)
(newline)

(display "Hash tables / assignments found: ")(newline)
(display lyimport::AssignmentTable)(newline)
;(pretty-print (hash-map->list cons lyimport::AssignmentTable))(newline)

(newline)
(display "============= Here is the final list =============")(newline)
(display "============= ====================== =============")(newline)
(pretty-print final_list)(newline)
(display "============= ====================== =============")(newline)

(newline)
;; Close input port
(close (current-input-port)) 


;Notes
;Clef (cons 'x_CLEF (cons pitch keyword) where pitch can be a pitch or default and keyword is optional
