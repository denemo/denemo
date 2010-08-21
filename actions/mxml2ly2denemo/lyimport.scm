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
(load "lyimport-lexer.scm") ; Helper functions for the lexer
(load "lyimport-parser.scm") ; Helper functions and parser rules
(load "lyimport-todenemo.scm") ; Bindings to convert to Denemo

;; Input Port
(set-current-input-port (open-input-file "mytest.ly"))

;; Options
(define lyimport::create_lexer_each_time #t) ; Switch to decide if the lexer gets rebuild everytime or the existing file gets used. Default #t
(define lyimport::halt_on_error #t) ; Switch to decide if a catched error stops the program and gives a reminder or silently goes on, creating a wrong output. Default #t

;; Lists to save music and create the final output
(define current_notelist '())
(define final_list (list 'x_LIST ))
  

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

;If needed: Generate a loadable, standalone lexer file from the .l syntax file 
(if (and (file-exists? "mxml2ly2denemo.l.scm") (not lyimport::create_lexer_each_time))
	(display "Using existing lexer file\n")
	(begin (lex "mxml2ly2denemo_new.l" "mxml2ly2denemo.l.scm" 'counters 'all) ; The user wants a new generation or the file does not exist yet.
		   (display "New lexer file generated\n"))
)

;Load generated file
(load "mxml2ly2denemo.l.scm")

;Start the lexer on the current input port which is the file to convert. Generates a lexer as var "lexer"
(lexer-init 'port (current-input-port)) 


;;;;;;;;;;;;;;;;;;;LALR-SCM PARSER;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Run the parser. It wants the lexer and a function to display the uncatched errors. Automatically runs on the current input port.
(newline)
(display ":::::::: Parser Start ::::::::::")(newline)
(mxml2ly2denemo-parser lexer displayerror)

(newline)
(display ":::::::: Parser Finished ::::::::::")(newline)
;(newline)

;(display "Hash tables / assignments found: ")(newline)
;(display lyimport::AssignmentTable)(newline)
;(pretty-print (hash-map->list cons lyimport::AssignmentTable))(newline)

;(newline)
;(display "============= Here is the final list =============")(newline)
;(display "============= ====================== =============")(newline)
;(pretty-print final_list)(newline)
;(display (list-ref final_list 3))(newline)
;(display "============= ====================== =============")(newline)
;(newline)

;; Close input port
(close (current-input-port))

(lyimport::convert_to_denemo final_list)
