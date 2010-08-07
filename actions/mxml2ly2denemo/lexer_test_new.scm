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

;; Lexer
(define (mtoken symbol value) 
	(make-lexical-token symbol (make-source-location (current-input-port) (lexer-get-line) (lexer-get-column) (lexer-get-offset) -1) value)
)

(lex "mxml2ly2denemo_new.l" "mxml2ly2denemo.l.scm" 'counters 'all) ; Oh no!! The generated scm file has comments in the language of the devil!
(load "mxml2ly2denemo.l.scm")
(lexer-init 'port (current-input-port)) 


;; Parser Definition

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
   (NOTENAME_PITCH WHITESPACE { } ERROR)

 (lilypond (lilypond toplevel_expression) : #t
		   (toplevel_expression) : #t)
	
 (toplevel_expression
			(composite_music)			: (display-combo "Note" $1)		
			;(NOTENAME_PITCH)			: (display-combo "Note" $1)		
			(WHITESPACE)				: #f
			;({)						: (display "{")
			;(})						: (display "}")
			(ERROR)						: (display-combo "errorr" $1)
 
 )
 
 (composite_music
	(grouped_music_list)			: $1
 )
 
 (grouped_music_list
	(sequential_music)				: $1
 )	
 
 (sequential_music
	({ music_list })				: $1
 )
 
 (music_list
	(music_list music)				: $1
 )
 
 (music
	(simple_music)					: $1
	(composite_music)				: $1
 )
 
 (simple_music
	(event_chord)					: $1
 )
 
 (event_chord
	(simple_chord_element)			: $1
 )
 
 (simple_chord_element
	(simple_element)				: $1
 )
 
 (simple_element
	(pitch)							: $1
 )
 
 (pitch
	(steno_pitch)					: $1
 )
 
 (steno_pitch
	(NOTENAME_PITCH)				: $1
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
