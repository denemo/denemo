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
(lex "mxml2ly2denemo.l" "mxml2ly2denemo.l.scm") ; Oh no!! The generated scm file has comments in the language of the devil!
(load "mxml2ly2denemo.l.scm")
(lexer-init 'port (current-input-port)) 



;; Parser Definition
(define mxml2ly2denemo-parser
  (lalr-parser

   ;; --- token definitions
   (INTEGER )

   
   (e (number)	 : $1)
   (number (INTEGER) : #t)	

))

; Just to get this out of my way... I don't wanted to make errors anyway! (real function later)
(define (displayerror arg1 arg2)
		(display arg1)
		;(display two)
		(display arg2)(newline)
)
(display
(mxml2ly2denemo-parser lexer displayerror)
)

;; Close input port
(close 0) 
