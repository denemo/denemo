(define lyimport::movement #f)
(define lyimport::staff #f)
(define lyimport::voice #f)
(define lyimport::notes #f)
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
  (string-append "(Navigation::CursorUpN " (number->string (* blips 7)) ")")))




  (define (notename2 note)
    ;(string (char-upcase (string-ref (list-ref note 1) 0))))
 ; (list-ref note 1)
    ;(format #t "the note2 ~a~%"  note)
    (string-append (car note) (octave (cdr note)))
    )

  (define (notename note)
    ;(format #t "the note ~a~%"  (cadr note))
   ; (caadr note)
    (notename2 (cadr note))
    )

  (define (relative-add-note-to-chord note)
    ;(format #t "Chord note ~a \n~a\nThe end\n" note (car (cadadr note)))
    (string-append "(d-MoveTo" (string (char-upcase (string-ref (car (cadadr note))  0))) ")" (octave-shifts (cdr (cadadr note)))  "(d-AddNoteToChord)")
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
  
  
  (define (do-time thetime)
    (if lyimport::notes        
	(string-append "(d-InitialTimeSig \"" thetime "\")")
	(string-append "(d-InsertTimeSig \"" thetime "\")")))
  
  (define (do-key thekey type)
    (if lyimport::notes        
	(string-append "(d-InitialKey \"" thekey " " type  "\")")
	(string-append "(d-InsertKey \"" thekey " " type  "\")")))
  
  
  (define (do-movement)
    (set! lyimport::notes #t)
    (set! lyimport::staff #f)
    (set! lyimport::voice #f)
    (if lyimport::movement
	"\n(d-AddMovement)\n"
	"\n;;new movement not needed here\n"))
  
  (define (do-context thecontext)
    ;(format #t "the context is ~a and ~a~%" thecontext lyimport::staff)

    (cond
     ((equal? "Staff" (car thecontext)) (set! lyimport::notes #t) (if lyimport::staff
								      (begin (set! lyimport::voice #f) "(d-AddLast)")
								      (begin (set! lyimport::staff #t) "")))
     ((equal? "Voice" (car thecontext)) (set! lyimport::notes #t) (if lyimport::voice
								      "(d-AddVoice)"
								      (begin (set! lyimport::voice #t) "")))
     ((equal? "PianoStaff" (car thecontext)) ";ignoring PianoStaff\n")
     (else "%context not handled\n")
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
  (define (do-dots thedur)
;(format #t "do-dots gets a duration of form ~a~%" thedur)
    (if (equal? thedur "")
	""
	(let ((adot "(d-AddDot)") (numdots (list-ref thedur 1)))
	   (if (> numdots 0)
	       (xsubstring adot (string-length adot) (* (+ numdots 1) (string-length adot)))
	       "")
	   )))


 (define (do-relative-note anote)
   ;(format #t "relative note ~a" anote)
   (let ((out (string-append "(d-" (string (char-upcase (string-ref (car anote) 0))) ")")))
     (string-append (octave-shifts (cdr anote)) out)
   ))
  
(define (do-note thenote)
  (if lyimport::relative
      (do-relative-note (cadr thenote))
      (string-append "(d-InsertC)(d-PutNoteName \"" (notename thenote) "\")" (if lyimport::in-grace "(d-ToggleGrace)" ""))

      
      ))
  
  
  (define (create-note current_object)
    (set! lyimport::notes #f)
    (cond 
     ((eqv? (car  current_object) 'x_CHORD)		(begin   (string-join (map create-note (list (cadr current_object)))))
      )
     ((eqv? (car current_object) 'x_NOTE)          (begin 
					;(format #t "note is ~a~%" (cdr current_object))
						     (do-note current_object))
      )
     ((eqv? (car current_object) 'x_REST)          (begin (string-append ";(d-Insert" (notename current_object) ") rest omitted\n")))
     
     (else "(d-WarningDialog \"%unhandled note type\n\")")   
     ))

  (define (loop-through current_object)
    ;(format #t "~% ------------ ~% current object ~a which is list?~a~%pair?~a~%" current_object (list? current_object)  (pair? current_object))
    ;; (if (eqv?  current_object 'x_COMPOSITE_MUSIC)  
    ;;	 " "
;;;;;;; First pairs that are lists
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
	   
	   ((or (eqv? (car current_object) 'NEWCONTEXT)  (eqv? (car current_object) 'CONTEXT))    (do-context (cdr current_object)));        "(d-AddLast)")
	   
	   ((eqv? (car current_object) 'x_SEQUENTIAL)		(begin
								  ;(format #t "the sequential list has ~a~% ~%" (list? (cdr current_object)))
								  (string-join (map loop-through (cdr current_object)))))
	   
	   ;;((eqv? (car current_object) 'x_REALCHORD)	(exit))


	   ((eqv? (car current_object) 'x_SIMULTANEOUS)		(begin
								  ;(format #t "the parallel list has ~a~% which has list-ref cadr ~a~%" (cdr current_object)   (list-ref   (cdr current_object) 0))
								  ;(pretty-print  (list-tail (cdr current_object) 1))
								  (cond 
								   ((eqv? 'x_SEQUENTIAL  (car (list-ref (cdr current_object) 0)))
								    (begin
								      ;(format #t "Seq:About to process ~a~%"  (cdr current_object))
									  (string-append ";;;ignoring the {}\n" (string-join (map loop-through (cdr current_object))))							  
								      ))
								   
								   
								   
								   ((eqv? 'NEWCONTEXT  (car (list-ref   (cdr current_object) 0)))
								    (string-append ";;;A new context?\n"  "(d-AddLast)\n" (string-join (map loop-through (list-tail (cdr current_object) 1)))))
								   ((eqv? 'x_CHORD (caadr current_object))
								    (begin 
								      	      
								      (string-append (start-chord (cadr current_object))  (string-join (map add-notes-to-chord (list-tail   (cdr current_object) 1))))))
								   (else (begin 
					;(format #t "~%~%recursive handle ~a items  ~a~%~%~%~%" (length  (list-ref (cdr current_object) 0)) (list-ref (cdr current_object) 0))
									   (string-join (map loop-through (cdr current_object)))))
								   )))
	   
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
	   ((eqv? (car current_object) 'x_CHORD) (begin 
						   ;(format #t "~%~%~%hoping to process a note next for ~a~%" (list (cadr current_object))) 
						   (if (eqv? (caadr current_object) 'x_REST) 
						       (let ((thedur #f))
							 (set! lyimport::notes #f)
							 (set! thedur  (list-ref (cadr current_object) 2))
							 (format #t "dur is ~a~%" (number? (car thedur)))
							 (if (number? (car thedur))
							     (string-append (do-duration thedur) " (d-EnterRest) " (do-dots thedur))
							     
							       (let loop ((count  (string->number (list-ref thedur 2))))
								 (format #t "Looping ~a~%" count)
								 (if (not (integer? count)) ";Cannot handle a fraction duration as multiplier\n"
								 (if (zero? count) ""
								     (string-append (do-duration (car thedur)) " (d-EnterRest) " (do-dots (car thedur)) (loop (- count 1)) ))))
))
							 



						  (string-append (do-duration (list-ref (cadr current_object) 5)) " "  (string-join (map create-note (list (cadr current_object)))) " "   (do-dots (list-ref (cadr current_object) 5)))
						  )))


	   ((eqv? (car current_object) 'x_CLEF) (begin  (do-clef (cdr current_object))))
	   ((eqv? (car current_object) 'x_TIME) (begin (do-time (cdr current_object))))
	   ((eqv? (car current_object) 'x_KEY) (begin (do-key  (cadr current_object) (cddr current_object))))
						

	   ((eqv? (car current_object) 'x_REALCHORD) (begin 
;(format #t "hoping to process the chord for ~a~%" (caadr current_object))
 
 (string-append (do-duration (cdadr current_object)) " "   (start-chord (caaadr current_object))  (string-join (map add-notes-to-chord (list-tail   (caadr current_object) 1))))))
;;;;(string-join (map loop-through (caadr current_object)))
	   ((eqv? (car current_object) 'x_BARLINE) (begin (string-append "(d-DirectivePut-standalone-postfix \"Barline\" \"\\\\bar \\\"" (cdr current_object) "\\\"\")")))
	   ((eqv? (car current_object) 'x_MMREST) "(d-InsertWholeMeasureRest)")
	   ((eqv? (car current_object) 'x_CHANGE) ";Context Change ignored\n")
	   ((eqv? (car current_object) 'x_RELATIVE) (begin
						      (format #t "Working with Relative music  ~a~%"  current_object)						      
						      (set! lyimport::relative (cdr current_object))

						       (string-append "(d-CursorToNote \"" (notename2 (cdr current_object)) "\");Translated from relative music\n")))
	                                            

	   (else (begin (format #t "Not handled~%~%") (pretty-print current_object) ";Syntax Ignored\n"))					  
	   ))))
  
(if (not (defined? 'Denemo))
  (format #t "~%;;;Final Denemo Script~%+++++++++++++++++++++++++++~% ~a~%;;;End of Denemo Script++++++++++++++++++++++++++++~%" 
(string-join (map loop-through list_from_parser)))
(string-join (map loop-through list_from_parser))))
