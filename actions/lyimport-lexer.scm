;"Magical Token". Wrapper to make returning a token easier, without all the positions and input ports
(define (lyimport::mtoken symbol value) 
	(make-lexical-token symbol (make-source-location (current-input-port) (lexer-get-line) (lexer-get-column) (lexer-get-offset) -1) value)
)	

(define (lyimport::pop_state)
					;(format #t "Now popping the state ~a becomes ~a~%~%" lyimport::state (cdr lyimport::state))
  (set! lyimport::state (cdr lyimport::state)))
;Accumulator for a string in double quotes (used by quote state of lexer)
(define lyimport::quoted_string "")
(define (lyimport::start_quote)
  (set! lyimport::quoted_string "")
;;(format #t "Now pushing the state ~a becomes ~a~%~%" lyimport::state  (cons 'quote lyimport::state))
  (set! lyimport::state (cons 'quote lyimport::state)))
(define (lyimport::quote-append str)
      (set! lyimport::quoted_string (string-append lyimport::quoted_string str)))


(define lyimport::block_string "")
(define lyimport::brace_count 0)
(define (lyimport::start_block)
  (set! lyimport::block_string "")
  (set! lyimport::brace_count 1)
;;(format #t "Now pushing the state ~a becomes ~a~%~%" lyimport::state  (cons 'block lyimport::state))
  (set! lyimport::state (cons 'block lyimport::state)))
(define (lyimport::block-append str)
      (set! lyimport::block_string (string-append lyimport::block_string str)))
     


(define (lyimport::start_incl)
  (set! lyimport::state (cons 'incl lyimport::state)))


; List of Notenames
(define lyimport::list_of_notenames
	(list "c" "cis" "ces" "cisis" "ceses" "d" "dis" "des" "disis" "deses" "e" "eis" "es" "ees" "eisis" "eses" "eeses" "f" "fis" "fes" "fisis" "feses" "g" "gis" "ges" "gisis" "geses" "a" "ais" "as" "aes" "aisis" "aeses" "ases" "b" "bis" "bes" "bisis" "beses" "h" "his" "hes" "hisis" "heses")
)

; List of Special Identifiers
 (define lyimport::list_of_special_scm_identifiers
	(list "major" "minor" "dorian" "phrygian" "lydian" "mixolydian")
 )


;Lilyponds "try_special_identifiers" is a part of scan_escaped_words in Denemo and needs this function:
(define (lyimport::try_special_identifiers_scm? yytext)
 	;Helper function to test if the string matches any string of a list of strings, the scm identifiers.
		(let loop ((counter 0))			
			(cond 
			  ((> (+ counter 1) (length lyimport::list_of_special_scm_identifiers)) #f)
			  ((and (<= counter (length lyimport::list_of_special_scm_identifiers)) (string-ci=? yytext (list-ref lyimport::list_of_special_scm_identifiers counter))) #t)
			  (else (loop (+ counter 1)))
			)
		)
)


;Three functions to handle assignments. The creation function is in the main file and its used in the parser. The lexer only looks for already existing assignments, triggered with \user-created-keyword-through-assignment
(define (lyimport::as-type key)
		(car (hashq-ref lyimport::AssignmentTable (string->symbol key)))
		
)

(define (lyimport::as-value key)
    	(cdr (hashq-ref lyimport::AssignmentTable (string->symbol key)))
    	
)	


(define (lyimport::as-eval key)		
	    (lyimport::mtoken 'MUSIC_IDENTIFIER (lyimport::as-value key))
)


;Any time the lexer gets a \keyword is looks up if its a fixed expression, a identifier or an assignment (or wrong)
(define (lyimport::scan_escaped_word yytext)
	(cond
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
	;;;	((string-ci=? "break" yytext) (lyimport::mtoken 'BREAK yytext))
		((string-ci=? "break" yytext) (lyimport::multilexer));;denemo special ignore break

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
		((string-ci=? "fermata" yytext) (lyimport::mtoken 'FERMATA yytext))
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
		((string-ci=? "applyContext" yytext) (lyimport::mtoken 'APPLY_CONTEXT yytext))
		; Denemo specific
		;; we have to swallow # and an embedded scheme integer following barNumberCheck 
		((string-ci=? "barNumberCheck" yytext)

		    (begin (let loop ((c #f)) (set! c (lexer-getc)) (if (char-whitespace? c) (loop c)))   (read (make-soft-port (vector #f #f #f  (lambda () (lexer-getc)) #f) "r"))(lyimport::multilexer)))
		
		
        ;If its not a known keyword its probably a user assignment:
        ((hashq-ref lyimport::AssignmentTable (string->symbol yytext)) (lyimport::as-eval yytext))
        
        ;Next test for an try_special_identifiers
		((lyimport::try_special_identifiers_scm? yytext) (lyimport::mtoken 'SCM_IDENTIFIER yytext))
        
        ;If its not a keyword, assignment or special identifier then its wrong				
	;	(else (begin (display (string-append "error: Unknown word: " yytext " (Line: "(number->string (lexer-get-line)) " Column: " (number->string ;(lexer-get-column)) ")\n"))(lyimport::multilexer))
	;	)
	      (else (lyimport::multilexer)
		;(else (lyimport::mtoken 'DENEMODIRECTIVE (string-append "\\" yytext))
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
