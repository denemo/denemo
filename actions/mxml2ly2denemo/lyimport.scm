;; Lilypond to Denemo
;; A Lexer / Parser to import ly files into Denemo or other formats
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


;; Libs
(load "lalr.scm")
(load "silex.scm")
(load "multilex.scm")
(load "lyimport-lexer.scm") ; Helper functions for the lexer
(load "lyimport-parser.scm") ; Helper functions and parser rules
(load "lyimport-todenemo.scm") ; Bindings to convert to Denemo


(define lyimport::state (list 'notes))

(define (lyimport::multilexer)
  (format #t "lexer state ~a ~a~%" lyimport::state (car lyimport::state))
  (cond
   ((eqv? (car lyimport::state) 'notes)
    (lyimport::noteslexer))
   ((eqv? (car lyimport::state) 'quote)
    (lyimport::quotelexer))
   (else
    (display "no lexer"))))


;; Options
(define lyimport::create_lexer_each_time #t) ; Switch to decide if the lexer gets rebuild everytime or the existing file gets used. Default #t
(define lyimport::halt_on_error #t) ; Switch to decide if a catched error stops the program and gives a reminder or silently goes on, creating a wrong output. Default #t

 

;Blank Table of Assignments. Every assignment (block of notes and other events) is stored as one entry in the hash table.
(define lyimport::AssignmentTable (make-hash-table))
	;(hashq-set! lyimport::AssignmentTable 'name_of_assignment_var_as_symbol value_of_assignment) ; Template


; Assignment functions. Wants a string as type and something of your own choice (for example a music list)
; Three more functions to get the values are defined and used in lyimport-lexer.scm
(define (lyimport::as-create key pair-type-and-value)
		(hashq-set! lyimport::AssignmentTable (string->symbol key) pair-type-and-value)
)

;;;;;;;;;;;;;;;;;;;SILEX LEXER;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define lexer-get-line #f)
(define lexer-get-column #f)
(define lexer-get-offset #f)
(define lexer-getc #f)
(define lexer-ungetc #f)
(define lyimport::noteslexer #f)
(define lyimport::quotelexer #f)

;If needed: Generate a loadable, standalone lexer file from the .l syntax file 
(if (and (file-exists? "notes.l.scm") (not lyimport::create_lexer_each_time))
	(display "Using existing lexer file\n")
	(begin ; The user wants a new generation or the file does not exist yet.
	  (lex-tables "notes.l" "notes-table"  "notes.l.scm"  'counters 'all)))
;;;FIXME further lexers here e.g. for quoted strings, lilypond blocks we don't need to examine etc.
(if (and (file-exists? "quote.l.scm") (not lyimport::create_lexer_each_time))
	(display "Using existing lexer file\n")
	(begin ; The user wants a new generation or the file does not exist yet.
	  (lex-tables "quote.l" "quote-table"  "quote.l.scm"  'counters 'all)))




(define lexer-port (open-input-file "mytest.ly"))
(define IS (lexer-make-IS 'port lexer-port  'all))
(set! lexer-get-line (lexer-get-func-line IS))
(set! lexer-get-column (lexer-get-func-column IS))
(set! lexer-get-offset (lexer-get-func-offset IS))
(set! lexer-getc (lexer-get-func-getc IS))
(set! lexer-ungetc (lexer-get-func-ungetc IS))


;;;FIXME load any further lexers created above.
(load "notes.l.scm")
(set! lyimport::noteslexer (lexer-make-lexer notes-table IS))
		   

(load "quote.l.scm")
(set! lyimport::quotelexer (lexer-make-lexer quote-table IS))
		   





;;;;;;;;;;;;;;;;;;;LALR-SCM PARSER;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Run the parser. It wants the lexer and a function to display the uncatched errors. Automatically runs on the current input port.
(newline)
(display ":::::::: Parser Start ::::::::::")(newline)
(define final_list (mxml2ly2denemo-parser lyimport::multilexer displayerror))

(newline)
(display ":::::::: Parser Finished ::::::::::")(newline)
;(newline)

;(display "Hash tables / assignments found: ")(newline)
;(display lyimport::AssignmentTable)(newline)
;(pretty-print (hash-map->list cons lyimport::AssignmentTable))(newline)

(newline)
(display "============= Here is the final list =============")(newline)
(display "============= ====================== =============")(newline)
(pretty-print final_list)(newline)
;;(display (list-ref final_list 3))(newline)
(display "============= ====================== =============")(newline)
(newline)

;; Close input port
(close (current-input-port))

(lyimport::convert_to_denemo final_list)
