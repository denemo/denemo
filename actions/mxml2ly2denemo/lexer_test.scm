;; Libs
(load "lalr.scm")
(load "silex.scm")

;; Input Port
(set-current-input-port (open-input-file "input_dummy.txt"))

;; Lexer
(lex "mxml2ly2denemo.l" "mxml2ly2denemo.l.scm") ; Oh no!! The generated scm file has comments in the language of the devil!
(load "mxml2ly2denemo.l.scm")
(lexer-init 'port (current-input-port)) 

;; Quick test
(let loop ()
  (let ((c (lexer)))
	(if (not (eqv? c '*eof*))
		(begin (display c)(newline)(loop))
		#f)))


;; Close the input port
(close 0) 
