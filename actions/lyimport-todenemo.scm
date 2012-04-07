(define lyimport::movement #f)
(define lyimport::staff 'init) ;;;;;; no staff context so far, because denemo always creates a staff and voice by default, the 'init state indicates that we have these, but we have not yet seen an instruction to create them.
(define lyimport::voice 'init)
(define lyimport::notes #t) ;;; #f once notes have been inserted in the current context
(define lyimport::in-grace #f)
(define lyimport::relative #f)


(define (lyimport::convert_to_denemo list_from_parser)
  

(define (octave blips)
  (cond ((zero? blips) "")
	((> blips 0) (string-append "'" (octave (- blips 1))))
	(else (string-append "," (octave (+ blips 1))))))

(define (octave-shifts blips)
  (if (zero? blips)
      ""
  (string-append "(d-ShiftCursor " (number->string (* blips 7)) ")")))




  (define (notename2 note)
    (string-append (car note) (octave (cdr note))))    

  (define (notename note)
    ;(format #t "the note ~a~%"  (cadr note))
   ; (caadr note)
    (notename2 (cadr note))
    )

  (define (relative-add-note-to-chord note)
    ;(format #t "Chord note ~a \n~a\nThe end\n" note (car (cadadr note)))
    (string-append "(d-MoveTo" (string (char-upcase (string-ref (car (cadadr note))  0))) ")" (octave-shifts (cdr (cadadr note)))  "(d-AddNoteToChord)" (do-accidental (cadr note))
     )
    )

  (define (add-notes-to-chord extra-chordnote)
    ;(format #t "entered addnotes to chord with  ~a list ~a~%" extra-chordnote (cadr extra-chordnote))
    (set! lyimport::notes #f)
    (if lyimport::relative
	(relative-add-note-to-chord extra-chordnote)
	(string-append "(d-InsertNoteInChord \"" (notename (cadr extra-chordnote)) "\")") ))

  (define (start-chord chord-note)
    ;(format #t "entered start chord with list chord-list ~a~%" chord-note)
    (set! lyimport::notes #f)
      (do-note (cadr chord-note)))

  (define (do-clef theclef)
    (if lyimport::notes 
	(string-append "(d-InitialClef \"" theclef "\")")
	(string-append "(d-InsertClef \"" theclef "\")")))
  
  (define (do-tie cdr)	
	"(d-ToggleTie)") ; this only works because the command works in an appending position on the left note and we can be sure there is none in front of us during importing.
	
  (define (do-bracket-open cdr)	
		"(d-StartBeam)") ; this only works because the command works in an appending position on the left note and we can be sure there is none in front of us during importing.		
	
  (define (do-bracket-close cdr)	
		"(d-EndBeam)") ; this only works because the command works in an appending position on the left note and we can be sure there is none in front of us during importing.
		  
  (define (do-time thetime)
    (if lyimport::notes        
	(string-append "(d-InitialTimeSig \"" thetime "\")")
	(string-append "(d-InsertTimeSig \"" thetime "\")")))
  

(define (translate-key keyname)
  (if (= 1 (string-length keyname))
      keyname
      (if (equal? (string-ref keyname 1) #\e)
	  (string-append (string (string-ref keyname 0)) " flat")
	  (string-append (string (string-ref keyname 0)) " sharp"))))
      

  (define (do-key thekey type)
    (set! thekey (translate-key thekey))
    (if lyimport::notes        
	(string-append "(d-InitialKey \"" thekey " " type  "\")")
	(string-append "(d-InsertKey \"" thekey " " type  "\")")))
  
  (define (do-partial current_object)
	(define lilyticks (number->string (duration::CalculateTicksWithDots (duration::lilypond->ticks (car current_object)) (cdr current_object))))
	(define lilypondstring (string-append (number->string (car current_object)) (string-concatenate (make-list (cdr current_object) "."))))
	(string-append "
		(define MaxTicks (* 1536 (GetPrevailingTimeSig #t)))
		(StandAloneDirectiveProto (cons \"Upbeat\" \"\\\\partial " lilypondstring "\")  #f)
		(d-SetDurationInTicks (- MaxTicks "lilyticks"))
		(d-MoveCursorRight)		
	")) ; string-append end
 
   (define (do-set current_object)
	(disp "Set found: " current_object)
	(case (string->symbol (car current_object))
		((Staff.instrumentName) (string-append "(AttachDirective \"staff\" \"postfix\" \"InstrumentName\"  \"\\\\set Staff.instrumentName = \\\""(cdr current_object)"\\\"\" DENEMO_OVERRIDE_GRAPHIC)")) ; what a string madness...
		((Staff.shortInstrumentName) (string-append "(AttachDirective \"staff\" \"postfix\" \"ShortInstrumentName\"  \"\\\\set Staff.shortInstrumentName = \\\""(cdr current_object)"\\\"\" DENEMO_OVERRIDE_GRAPHIC)")) ; what a string madness...
   ))
 
 
  
  (define (do-movement)
    (set! lyimport::notes #t)
    (set! lyimport::staff 'init)
    (set! lyimport::voice 'init)
    (if lyimport::movement
	"\n(d-AddMovement)\n"
	"\n;;new movement not needed here\n"))
  
  (define (do-context thecontext)
    ;(format #t "the context is ~a and ~a~%" thecontext lyimport::staff)

    (cond
     ((equal? "Staff" thecontext)  (if lyimport::staff
								(begin (set! lyimport::notes #t) (set! lyimport::staff #f) (set! lyimport::voice 'init) "(d-AddLast)(d-MoveToBeginning)")
								(begin "")))
     ((equal? "Voice" thecontext)  (if lyimport::voice
								(begin (set! lyimport::notes #t) (set! lyimport::voice #f) "(d-AddVoice)(d-MoveToBeginning)")
								(begin "")))
     ((equal? "PianoStaff" thecontext) ";ignoring PianoStaff\n")
     (else "%context not handled\n")
     ))

  
  (define (do-newcontext thecontext)
    ;(format #t "the context is ~a and ~a~%" thecontext lyimport::staff)
    (cond
     ((equal? "Staff" thecontext) 
      (if (eq? lyimport::staff 'init)
	  (set! lyimport::staff #f)
	  (set! lyimport::staff #t)) 
      (do-context thecontext))
							
     ((equal? "Voice" thecontext)
      (if (eq? lyimport::voice 'init)
	  (set! lyimport::voice #f)
	  (set! lyimport::voice #t))
      (do-context thecontext))							
     ((equal? "PianoStaff" thecontext) ";ignoring PianoStaff\n")
     (else ";context not handled\n")
     ))

  
  (define (do-duration thedur)
    (if (equal? thedur "")
	""
	(begin
	  (string-append
	   (cond ((equal? 1 (list-ref thedur 0)) "(d-Set0)")
		 ((equal? 2  (list-ref thedur 0)) "(d-Set1)")
		 ((equal? 4  (list-ref thedur 0)) "(d-Set2)")
		 ((equal? 8  (list-ref thedur 0)) "(d-Set3)")
		 ((equal? 16  (list-ref thedur 0)) "(d-Set4)")
		 ((equal? 32  (list-ref thedur 0)) "(d-Set5)")
		 ((equal? 64  (list-ref thedur 0)) "(d-Set6)")
		 ((equal? 128  (list-ref thedur 0)) "(d-Set7)")
		 (else ""))
	  
	   ))))


  (define (do-duration-relative thedur)
    (if (equal? thedur "")
	";unknown rest\n\n"
	(begin
	  (string-append
	   (cond ((equal? 1 (list-ref thedur 0)) "(d-PutRest 0)")
		 ((equal? 2  (list-ref thedur 0)) "(d-PutRest 1)")
		 ((equal? 4  (list-ref thedur 0)) "(d-PutRest 2)")
		 ((equal? 8  (list-ref thedur 0)) "(d-PutRest 3)")
		 ((equal? 16  (list-ref thedur 0)) "(d-PutRest 4)")
		 ((equal? 32  (list-ref thedur 0)) "(d-PutRest 5)")
		 ((equal? 64  (list-ref thedur 0)) "(d-PutRest 6)")
		 ((equal? 128  (list-ref thedur 0)) "(d-PutRest 7)")
		 (else ""))
	  
	   ))))




  (define (do-dots thedur)
;(format #t "do-dots gets a duration of form ~a~%" thedur)
    (if (equal? thedur "")
	""
	(let ((adot "(d-AddDot)") (numdots (list-ref thedur 1)))
	   (if (> numdots 0)
	       (xsubstring adot (string-length adot) (* (+ numdots 1) (string-length adot)))
	       "")
	   )))

(define (do-accidental thenote)
;(format #t "using ~a\n\n" (notename thenote))
  (if lyimport::relative
       (string-append "(d-SetAccidental \"" (substring/shared (notename thenote) 1) "\")")
       ""))


 (define (do-relative-note anote)
   ;(format #t "relative note ~a" anote)
   (let ((out (string-append "(d-" (string (char-upcase (string-ref (car anote) 0))) ")")))
     (string-append (octave-shifts (cdr anote)) out)
   ))
  
(define (do-note thenote)
;(format #t "Doing note with ~a\n\n" lyimport::staff)
(let ((context ""))
 (set! context (string-append (do-context "Staff") (do-context "Voice")))
   ;(format #t "And so context ~a\n\n" context)
  (if lyimport::relative
      (string-append context (do-relative-note (cadr thenote)) (do-accidental thenote))
      (string-append context "(d-InsertC)(d-PutNoteName \"" (notename thenote) "\")"  (if lyimport::in-grace "(d-ToggleGrace)" "")) 
      )))


 (define (create-note current_object)
  ;  (let ((context (if lyimport::staff "" (string-append (do-context "Staff") (do-context "Voice")))))
    (set! lyimport::notes #f)
    (cond 
     ((eqv? (car  current_object) 'x_CHORD)		(begin   (string-join (map create-note (list (cadr current_object)))   )   )
      )
     ((eqv? (car current_object) 'x_NOTE)          (begin 
					;(format #t "note is ~a~%" (cdr current_object))
						     (string-append (do-note current_object)))
      )
     ((eqv? (car current_object) 'x_REST)          (begin (string-append ";(d-Insert" (notename current_object) ") rest omitted\n")))
     
     (else "(d-WarningDialog \"%unhandled note type\n\")")   
     )
;)
)
(define (do-repeat theobject)
  ;(format #t "Repeat with alternative ~a\n\n"  theobject )
  (string-append (if lyimport::notes "" "(d-RepeatStart)") (string-join (map loop-through (list-ref theobject 3)))   (do-alternative (car (list-tail theobject 4)))))

(define (do-alternative theobject)
;(format #t "alternative ~a\n\n"  (length theobject) )
;;; Note treatment of alternative with just one is here by repeating the music - this could fail in relative, it should issue a 1-2 time bar
(cond ((= (length theobject) 1) 
       (string-append "(d-OpenFirstTimeBar)" (string-join (map loop-through  (list-tail (list-ref theobject 0) 1))) "(d-EndVolta)(d-RepeatEnd)(d-OpenSecondTimeBar)"  (string-join (map loop-through (list-tail (list-ref theobject 0) 1))) "(d-EndVolta)"  ))
      ((= (length theobject) 2)
       (string-append "(d-OpenFirstTimeBar)" (string-join (map loop-through  (list-tail (list-ref theobject 0) 1))) "(d-EndVolta)(d-RepeatEnd)(d-OpenSecondTimeBar)"    (string-join (map loop-through (list-tail (list-ref theobject 1) 1))) "(d-EndVolta)"  ))
      (else ";Repeats over two not handled\n")))



(define (loop-through-simult current_object)
;;   (set! lyimport::staff 'init)
;;   (set! lyimport::voice 'init)
;;   (set! lyimport::notes #t)
;; (format #t "New context for ~a\n\n" current_object)
(let ((temp (loop-through current_object)))
  (set! lyimport::staff #t)
  (set! lyimport::voice #t)
  (set! lyimport::notes #t)
  temp))

  (define (do-simultaneous theobject)
;(pretty-print theobject)
    (cond 
     ((eqv? 'x_SEQUENTIAL  (car (list-ref theobject 0)))
      (string-append ";;;implementing the {}\n" (string-join (map loop-through-simult theobject))))
   
     
     
     
     ((eqv? 'NEWCONTEXT  (car (list-ref   theobject 0)))
      (string-append ";;;FIXME A new context?\n"  "(d-AddLast)(d-MoveToBeginning)\n" (string-join (map loop-through (list-tail theobject 1)))))
     ((eqv? 'x_CHORD (caar theobject))
      (begin 
	
	(string-append (start-chord (car theobject))  (string-join (map add-notes-to-chord (list-tail   theobject 1))))))
     (else (begin 
					;(format #t "~%~%recursive handle ~a items  ~a~%~%~%~%" (length  (list-ref theobject 0)) (list-ref theobject 0))
	     (string-join (map loop-through theobject))))))


  (define (loop-through current_object)
    ;(format #t "~% ------------ ~% current object ~a which is list?~a~%pair?~a~%" current_object (list? current_object)  (pair? current_object))
    ;; (if (eqv?  current_object 'x_COMPOSITE_MUSIC)  
    ;;	 " "

;;;;;; first tokens
    

;;;;;;; Next pairs that are lists



    (if (list? current_object)
	(begin
	  (cond
	   ((eqv? (car current_object) 'x_MOVEMENT)	  (let ((temp #f))
;(format #t "the movement has tail ~a~%~%" (list-tail current_object 1) )
							    (set! temp 
							      (string-append  (do-movement)  (string-join (map loop-through (list-tail current_object 1)))))
							    (set! lyimport::movement #t)
							    temp
							    ))
	   
	   ((or (eqv? (car current_object) 'NEWCONTEXT)  (eqv? (car current_object) 'CONTEXT))    (do-newcontext (cadr current_object)));        "(d-AddLast)")
	   
	   ((eqv? (car current_object) 'x_SEQUENTIAL)		(begin
								  ;(format #t "the sequential list has ~a~% ~%"  (cdr current_object))
								  (string-join (map loop-through (cdr current_object)))))
	   

	   ((eqv? (car current_object) 'x_REPEAT)	(do-repeat current_object))

	  ;; ((eqv? (car current_object) 'x_ALTERNATIVE)	(do-alternative current_object))


	   ((eqv? (car current_object) 'x_SIMULTANEOUS)	         (do-simultaneous (cdr current_object)))
	   
	   ((eqv? (car current_object) 'x_COMPOSITE_MUSIC)       (begin 
								   ;(format #t "hoping to process composite for ~a~%" (list-tail (cdr current_object) 0))
								   (string-join (map loop-through (list-tail (cdr current_object) 0)))))
	    ((eqv? (car current_object) 'x_GRACE)                 (let ((temp #f))
								    ;(format #t "grace ~a~%"  (list-tail current_object 1))
								  (set! lyimport::in-grace #t) (set! temp (string-join (map loop-through (list-tail current_object 1)))) (set! lyimport::in-grace #f) temp))
	   ((eqv? (car current_object) 'TIMES)                 (begin
								 ;(format #t "Tuplet ~a~%"  (list-tail current_object 2))
								 (string-append "(d-StartTriplet)(d-SetTuplet \"" (list-ref current_object 1) "\") " (string-join (map loop-through (list-tail current_object 2))) " (d-EndTuplet)")))




	   ((eqv? (car current_object) 'x_RELATIVE)      (begin (
								 format #t "\n\nhandling relative ~a FIXME what pitch???\n\n" current_object)
								(set! lyimport::relative #t)
								(string-join (map loop-through (list-tail current_object 1)))))

	   (else
	    (begin 
	      ;(format #t "handled ~a by recursion through list~%" current_object)   
		   (string-join (map loop-through current_object))))
	   ))  ;;;;; end of current_object is a list
	(begin
	  ;(format #t "treating the pair case ~a~%~%" (car current_object))
	  (cond
	   ((eqv? (car current_object) 'x_CHORD) (let () 
						   	;postfix section
						   (define postfix " ") ; build a chain of postfixes
						   (if (string-contains (cdr (cdr current_object)) "fermata")
								(set! postfix (string-append postfix "(d-ToggleFermata) ")))
						   ;(format #t "~%~%~%hoping to process a note next for ~a~%" (list (cadr current_object)))
						    (if (equal? (cdr (cdr current_object)) "(")
							(set! postfix (string-append postfix "(d-ToggleBeginSlur) ")))
						    (if (equal? (cdr (cdr current_object)) ")")
							(set! postfix (string-append postfix "(d-ToggleEndSlur) ")))
						   (if (eqv? (caadr current_object) 'x_REST) 
						       (let ((thedur #f))
							 (set! lyimport::notes #f)
							 (set! thedur  (list-ref (cadr current_object) 2))
							 ;(format #t "dur is ~a~%" (car thedur))
							 (if (number? (car thedur))
							     (string-append (do-duration-relative thedur) (do-dots thedur))
							     
							       (let loop ((count  (string->number (list-ref thedur 2))))
								 ;(format #t "Looping ~a~%" count)
								 (if (not (integer? count)) ";Cannot handle a fraction duration as multiplier\n"
								 (if (zero? count) ""
								     (string-append ;(do-duration (car thedur)) 
								       (do-duration-relative (car thedur)) (do-dots (car thedur)) (loop (- count 1)) postfix ))))));;;; end of if a rest
							 



						  (string-append (do-duration (list-ref (cadr current_object) 5)) " "  (string-join (map create-note (list (cadr current_object)))) " "  (do-dots (list-ref (cadr current_object) 5)) postfix)
						  )))

	   ((eqv? (car current_object) 'x_TIE) (begin  (do-tie (cdr current_object))))
	   ((eqv? (car current_object) 'x_BRACKET_OPEN) (begin  (do-bracket-open (cdr current_object))))
	   ((eqv? (car current_object) 'x_BRACKET_CLOSE) (begin  (do-bracket-close (cdr current_object))))
	   ((eqv? (car current_object) 'x_CLEF) (begin  (do-clef (cdr current_object))))
	   ((eqv? (car current_object) 'x_TIME) (begin (do-time (cdr current_object))))
	   ((eqv? (car current_object) 'x_KEY) (begin (do-key  (cadr current_object) (cddr current_object))))
	   ((eqv? (car current_object) 'x_PARTIAL) (begin (do-partial (cdr current_object))))				
	   ;((eqv? (car current_object) 'x_OVERRIDE) (begin  (do-tie (cdr current_object))))
   	   ;((eqv? (car current_object) 'x_REVERT) (begin  (do-tie (cdr current_object))))
   	   ((eqv? (car current_object) 'x_SET) (begin  (do-set (cdr current_object))))	
   	   ;((eqv? (car current_object) 'x_UNSET) (begin  (do-tie (cdr current_object))))	
	   
	   
	   ((eqv? (car current_object) 'x_REALCHORD) (let () 
;(format #t "hoping to process the chord for ~a~%" (caadr current_object))
   ;postfix section
	  (define postfix " ") ; build a chain of postfixes
	  (if (string-contains (cdr (cdr current_object)) "fermata")
		(set! postfix (string-append postfix "(d-ToggleFermata) ")))
	
  (string-append (do-duration (cdadr current_object)) " "   (start-chord (caaadr current_object))  (string-join (map add-notes-to-chord (list-tail   (caadr current_object) 1)))
 "(d-CursorToNote (GetLowestNote))" postfix)))
 
;;;;(string-join (map loop-through (caadr current_object)))
	   ((eqv? (car current_object) 'x_BARLINE) (begin 
			(cond 
				((equal? (cdr current_object) "|") "#t") ; do nothing
				((equal? (cdr current_object) "||") "(d-DoubleBarline)")
				((equal? (cdr current_object) "|.") "(d-ClosingBarline)")
				((equal? (cdr current_object) ":|:") "(d-RepeatEndStart)")
				((equal? (cdr current_object) "|:") "(d-RepeatStart)")
				((equal? (cdr current_object) ":|") "(d-RepeatEnd)")
				(else (string-append "(d-DirectivePut-standalone-postfix \"Barline\" \"\\\\bar \\\"" (cdr current_object) "\\\"\")"))
				)))
			
	   ((eqv? (car current_object) 'x_MMREST) "(d-InsertWholeMeasureRest)")
	   ((eqv? (car current_object) 'x_CHANGE) ";Context Change ignored\n")
	   ((eqv? (car current_object) 'x_RELATIVE) (begin
						      ;(format #t "Working with Relative music  ~a~%"  current_object)						      
						      (set! lyimport::relative (cdr current_object))

						       (string-append "(d-CursorToNote \"" (notename2 (cdr current_object)) "\");Translated from relative music\n")))
	   ((eqv? (car current_object) 'x_LILYPOND)     (string-append "(d-PutDirective-standalone \"" (scheme-escape (cdr current_object)) "\" \"" (scheme-escape (cdr current_object)) "\")\n"))                                        

	   (else (begin (format #t "Not handled~%~%") (pretty-print current_object) ";Syntax Ignored\n"))					  
	   ))))
  
(if (not (defined? 'Denemo))
  (begin (load "denemo.scm")(format #t "~%;;;Final Denemo Script~%+++++++++++++++++++++++++++~% ~a~%;;;End of Denemo Script++++++++++++++++++++++++++++~%" 
(string-join (map loop-through list_from_parser))))
(string-join (map loop-through list_from_parser))))
