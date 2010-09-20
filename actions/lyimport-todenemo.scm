(define lyimport::movement #f)
(define lyimport::staff #f)
(define lyimport::voice #f)
(define lyimport::notes #f)
(define lyimport::in-grace #f)


(define (lyimport::convert_to_denemo list_from_parser)
  
  (define (notename note)
    ;(string (char-upcase (string-ref (list-ref note 1) 0))))
  (list-ref note 1))


  (define (add-notes-to-chord extra-chordnote)
					;(format #t "entered addnotes to chord with  ~a list ~a~%" extra-chordnote (cadr extra-chordnote))
    (set! lyimport::notes #f)
    (string-append "(d-InsertNoteInChord \"" (notename (cadr extra-chordnote)) "\")") )

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
    (if (equal? thedur "")
	""
	(let ((adot "(d-AddDot)") (numdots (list-ref thedur 1)))
	   (if (> numdots 0)
	       (xsubstring adot (string-length adot) (* (+ numdots 1) (string-length adot)))
	       "")
	   )))

  
  (define (do-note thenote)
    (string-append "(d-InsertC)(d-PutNoteName \"" (notename thenote) "\")" (if lyimport::in-grace "(d-ToggleGrace)" "")))
  
  
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
	   ((eqv? (car current_object) 'x_MOVEMENT)	  (let ()
;;; (format #t "the movement has ~a~%~%" (cdr current_object) )
							    (define temp 
							      (string-append  (do-movement)  (string-join (map loop-through (list-tail current_object 1)))))
							    (set! lyimport::movement #t)
							    temp
							    ))
	   
	   ((or (eqv? (car current_object) 'NEWCONTEXT)  (eqv? (car current_object) 'CONTEXT))    (do-context (cdr current_object)));        "(d-AddLast)")
	   
	   ((eqv? (car current_object) 'x_SEQUENTIAL)		(begin
								  ;(format #t "the sequential list has ~a~% ~%" (cdr current_object))
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
								    (format #t "grace ~a~%"  (list-tail current_object 1))
								  (set! lyimport::in-grace #t) (set! temp (string-join (map loop-through (list-tail current_object 1)))) (set! lyimport::in-grace #f) temp))
	   ((eqv? (car current_object) 'TIMES)                 (begin
								 ;(format #t "Tuplet ~a~%"  (list-tail current_object 2))
								 (string-append "(d-StartTriplet)(d-SetTuplet \"" (list-ref current_object 1) "\") " (string-join (map loop-through (list-tail current_object 2))) " (d-EndTuplet)")))
	  

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
						       (begin
							     (string-append (do-duration (list-ref (cadr current_object) 2)) " (d-EnterRest) "   (do-dots (list-ref (cadr current_object) 2))))
							 



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

	   (else (begin (format #t "Not handled~%~%") (pretty-print current_object) "NO HOPE"))					  
	   ))))
  
(if (not (defined? 'Denemo))
  (format #t "~%;;;Final Denemo Script~%+++++++++++++++++++++++++++~% ~a~%;;;End of Denemo Script++++++++++++++++++++++++++++~%" 
(string-join (map loop-through list_from_parser)))
(string-join (map loop-through list_from_parser))))
