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

(lex "mxml2ly2denemo.l" "mxml2ly2denemo.l.scm" 'counters 'all) ; Oh no!! The generated scm file has comments in the language of the devil!
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
   (INTEGER LETTER NOTE QUOTEDSTRING DBLQUOTE COMMENT MULTILINECOMMENT OCTAVESIGN WHITESPACE DOT)
	
	(commands (commands command) : #t
			  (command) 	     : #t)
	(command 
			(note)			: (display-combo "Note" $1)
			(comment)		: (display-combo "Comment" $1)
			(directive)		: (display-combo "Directive" $1)
			(DBLQUOTE)		: $1
			(INTEGER)		: $1
			(LETTER)		: $1
			(WHITESPACE)	: #f			
	)
	
	(note ;maybe there is already a "insert lilypond" command, then notes can be defined as regular expression in one piece. But what about \relative or not?
			(dottednote)					: $1
			(NOTE)							: $1
			(NOTE INTEGER)					: (string-append $1 $2)
			(NOTE OCTAVESIGN)				: (string-append $1 $2)
			(NOTE OCTAVESIGN INTEGER)		: (string-append $1 $2 $3)
			;(NOTE DOT)						: (string-append $1 $2)
			;(NOTE INTEGER DOT)				: (string-append $1 $2 $3)
			;(NOTE OCTAVESIGN DOT)			: (string-append $1 $2 $3)
			;(NOTE OCTAVESIGN INTEGER DOT)	: (string-append $1 $2 $3 $4)

	)
	
	(dottednote
			(note DOT) : (string-append $1 $2) 
	) 
	
	(directive
			(QUOTEDSTRING) : $1
	)
	
	(comment
			(COMMENT)	: $1
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
