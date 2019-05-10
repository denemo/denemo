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

(define lyimport::filename "mytest.ly")
(define lyimport::pathname "./")

(define lyimport::state #f) ;;;;; stack of lexer states
(define lyimport::AssignmentTable #f)
(define lyimport::create_lexer_each_time #f) 
(define lyimport::halt_on_error #f)
(define lexer-get-line #f)
(define lexer-get-column #f)
(define lexer-get-offset #f)
(define lexer-getc #f)
(define lexer-ungetc #f)
(define lexer-port #f)

(define lyimport::noteslexer #f)
(define lyimport::quotelexer #f)
(define lyimport::blocklexer #f)
(define lyimport::incllexer #f)

;; Libs
(load "lalr.scm")
(load "silex.scm")
(load "multilex.scm")
(load "lyimport-lexer.scm") ; Helper functions for the lexer
(load "lyimport-parser.scm") ; Helper functions and parser rules
(load "lyimport-todenemo.scm") ; conversion of parse tree to Denemo script

(define (lyimport::multilexer)
;;; the lexing procedure itself
(let ((token #f))
					;(format #t "lexer state ~a ~a~%" lyimport::state (car lyimport::state))
  (set! token
  (cond
   ((eqv? (car lyimport::state) 'notes)
    (lyimport::noteslexer))
   ((eqv? (car lyimport::state) 'quote)
    (lyimport::quotelexer))
   ((eqv? (car lyimport::state) 'block)
    (lyimport::blocklexer))
   ((eqv? (car lyimport::state) 'incl)
    (lyimport::incllexer))
   (else
    (display "no lexer"))))
  ;(format #t "Got token ~a~%" token)
  token))

(if (defined? 'Denemo)
    (begin
      (set! lyimport::create_lexer_each_time #f) ;
      (set! lyimport::halt_on_error #f) ; on  error do not stop
      )
#!
;needs rewriting - define-once is not defined outside 'Denemo
    (begin
      ;; Options when not running inside denemo. Have to define some values which denemo.scm expects to exist
      (set! lyimport::create_lexer_each_time #t) ; Switch to decide if the lexer gets rebuilt everytime or the existing file gets used. Default #t if interactive
      (set! lyimport::halt_on_error #t) ; Switch to decide if a caught error stops the program and gives a reminder or silently goes on, potentially creating a wrong output. Default #t if interactive
     (define-once (d-SetBreve) #f)
     (define-once (d-SetLonga) #f)
     (define-once (d-SetMaxima) #f)
     (define-once (d-Set0) #f)
     (define-once (d-Set1) #f)
     (define-once (d-Set2) #f)
     (define-once (d-Set3) #f)
     (define-once (d-Set4) #f)
     (define-once (d-Set5) #f)
     (define-once (d-Set6) #f)
     (define-once (d-Set7) #f)
     (define-once (Denemo-Note0) #f)
     (define-once (Denemo-Note1) #f)
     (define-once (Denemo-Note2) #f)
     (define-once (Denemo-Note3) #f)
     (define-once (Denemo-Note4) #f)
     (define-once (Denemo-Note5) #f)
     (define-once (Denemo-Note6) #f)
     (define-once (Denemo-Note7) #f)
     (define-once (Denemo-Note8) #f)



      
      )
!#
    )


(define (lyimport::sysdirectory name)
  (if (defined? 'Denemo)
      (string-append DENEMO_ACTIONS_DIR name)
      name))
(define (lyimport::localdirectory name)
  (if (defined? 'Denemo)
      (string-append DENEMO_LOCAL_ACTIONS_DIR name)
      name))

					; Assignment functions. Wants a string as type and something of your own choice (for example a music list)
					; Three more functions to get the values are defined and used in lyimport-lexer.scm FIXME move this function there too????
(define (lyimport::as-create key pair-type-and-value)
  (hashq-set! lyimport::AssignmentTable (string->symbol key) pair-type-and-value)
  )

;;;;;;;;;;;;;;;;;;;SILEX LEXER;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#!
;Guile 2.n does not succeed in generating these - instead we include them with the system
;If needed: Generate a loadable, standalone lexer file from the .l syntax file 
(if (and (file-exists? (lyimport::localdirectory "notes.l.scm")) (not lyimport::create_lexer_each_time))
	(display "Using existing lexer file\n")
	(begin ; The user wants a new generation or the file does not exist yet.
	  (lex-tables  (lyimport::sysdirectory "notes.l") "notes-table"   (lyimport::localdirectory "notes.l.scm")  'counters 'all)))
;;;FIXME further lexers here e.g. for quoted strings, lilypond blocks we don't need to examine etc.
(if (and (file-exists? (lyimport::localdirectory "quote.l.scm")) (not lyimport::create_lexer_each_time))
    (display "Using existing lexer file\n")
    (begin ; The user wants a new generation or the file does not exist yet.
      (lex-tables (lyimport::sysdirectory "quote.l") "quote-table"  (lyimport::localdirectory "quote.l.scm")  'counters 'all)))

(if (and (file-exists? (lyimport::localdirectory "block.l.scm")) (not lyimport::create_lexer_each_time))
    (display "Using existing lexer file\n")
    (begin ; The user wants a new generation or the file does not exist yet.
      (lex-tables (lyimport::sysdirectory "block.l") "block-table"  (lyimport::localdirectory "block.l.scm")  'counters 'all)))

(if (and (file-exists? (lyimport::localdirectory "incl.l.scm")) (not lyimport::create_lexer_each_time))
    (display "Using existing lexer file\n")
    (begin ; The user wants a new generation or the file does not exist yet.
      (lex-tables (lyimport::sysdirectory "incl.l") "incl-table"  (lyimport::localdirectory "incl.l.scm")  'counters 'all)))
!#


(define lyimport::input-ports #f);; a stack of include files

(define (lyimport::push-port includefile)
  (set! lyimport::input-ports (cons  (open-input-file (string-append lyimport::pathname includefile)) lyimport::input-ports)))
  
(define (lyimport::lexer-proc)
  (let ((c (read-char (car lyimport::input-ports))))
;(format #t "The lexer has gotten ~a from ~a~%~%~%" c (car lyimport::input-ports))
    (if (char? c)
	c
	(begin
	  (close-port (car lyimport::input-ports))
	  (set! lyimport::input-ports (cdr lyimport::input-ports))
	  (if (null? lyimport::input-ports)
	      c
	      (lyimport::lexer-proc))))))
		

(define (lyimport::import)
					;(format #t "Starting lyimport now ~%~%~%")
  (if (defined? 'Denemo)
      (set! lyimport::input-ports (list (open-input-file (string-append lyimport::pathname lyimport::filename))))
      (set! lyimport::input-ports (list (open-input-file  "mytest.ly"))))


  (let ((IS (lexer-make-IS 'procedure lyimport::lexer-proc  'all)))
    (set! lexer-get-line (lexer-get-func-line IS))
    (set! lexer-get-column (lexer-get-func-column IS))
    (set! lexer-get-offset (lexer-get-func-offset IS))
    (set! lexer-getc (lexer-get-func-getc IS))
    (set! lexer-ungetc (lexer-get-func-ungetc IS))
    
    
    
    (load (lyimport::sysdirectory "notes.l.scm"))
    (set! lyimport::noteslexer (lexer-make-lexer notes-table IS))
    
    
    (load (lyimport::sysdirectory "quote.l.scm"))
    (set! lyimport::quotelexer (lexer-make-lexer quote-table IS))
    
    (load (lyimport::sysdirectory "block.l.scm"))
    (set! lyimport::blocklexer (lexer-make-lexer block-table IS))

    (load (lyimport::sysdirectory "incl.l.scm"))
    (set! lyimport::incllexer (lexer-make-lexer incl-table IS))
    )



;Blank the table of Assignments. Every assignment (block of notes and other events) is stored as one entry in the hash table.
(set! lyimport::AssignmentTable (make-hash-table))
					;(hashq-set! lyimport::AssignmentTable 'name_of_assignment_var_as_symbol value_of_assignment) ; Template

(set! lyimport::state (list 'notes)) 

;;;;;;;;;;;;;;;;;;;LALR-SCM PARSER;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

					; Run the parser. It wants the lexer and a function to display the uncatched errors. Automatically runs on the current input port.
(newline)
(display ":::::::: Parser Start ::::::::::")(newline)
(let ((final_list (mxml2ly2denemo-parser lyimport::multilexer displayerror)))

  (newline)
  (display ":::::::: Parser Finished ::::::::::")(newline)
  
  (newline)
  (display "============= Here is the final list =============")(newline)
  (display "============= ====================== =============")(newline)
  (pretty-print final_list)(newline)
  (display "============= ====================== =============")(newline)
  (newline)
  
  ;; Close input port
  (close (current-input-port))
  
  (lyimport::convert_to_denemo final_list)))

(if (not (defined? 'Denemo))
    (lyimport::import))
