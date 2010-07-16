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
(define mxml2ly2denemo-parser
  (lalr-parser
   ;; --- token definitions
   (INTEGER LETTER TAB)
	
	(commands (commands command) : #t
			  (command) 	     : #t)
	(command 
			(INTEGER)	: (begin (display $1) (display "no_te\n"))
			(LETTER)	: (begin (display $1) (display "re_st\n"))
			(TAB)		: (begin (display $1) (display "Tab\n")))
			

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
