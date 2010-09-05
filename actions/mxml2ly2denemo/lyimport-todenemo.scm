


(define (lyimport::convert_to_denemo list_from_parser)

  (define (notename note)
    (string (char-upcase (string-ref (list-ref note 1) 0))))


  (define (add-notes-to-chord extra-chordnote)
 (format #t "entered addnotes to chord with  ~a list ~a~%" extra-chordnote (cadr extra-chordnote))
    (string-append "(d-Add" (notename (cadr extra-chordnote)) ")")
    )

  (define (start-chord chord-list)
    (format #t "entered start chord with list ref cadr chord-list ~a~%" (cadr (list-ref (cdr chord-list) 0)))
      (string-append "(d-Insert" (notename (cadr (list-ref (cdr chord-list) 0))) ")"))


  (define (create-note current_object)
   (cond 
     ((eqv? (car  current_object) 'x_CHORD)		(begin   (string-join (map create-note (list (cadr current_object)))))
       )
     ((eqv? (car current_object) 'x_NOTE)          (begin (string-append "(d-Insert" (notename current_object) ")")))
     (else "(d-WarningDialog \"%unhandled note type\n\")")   
    ))

  (define (loop-through current_object)
(format #t "~% ------------ ~% current object ~a which is list?~a~%pair?~a~%" current_object (list? current_object)  (pair? current_object))

     (if (eqv?  current_object 'x_COMPOSITE_MUSIC)  
	 " "
;;;;;;; First pairs that are lists
     (if (list? current_object)
	 (begin
	   (cond
	   ((eqv? (car current_object) 'x_MOVEMENT)	(begin
						       (format #t "the movement has ~a~% which is list? ~a~%" (cdr current_object) (list? (cdr current_object)))
						       (string-append  "(d-NewMovement)" (string-join (map loop-through (list-tail current_object 1))))))

	   ((eqv? (car current_object) 'NEWCONTEXT)             "(d-AddLast)")

	   ((eqv? (car current_object) 'x_SEQUENTIAL)		(begin
							 (format #t "the sequential list has ~a~% which HAS cadr ~a~%" (cdr current_object) (cadr current_object))
							 (string-join (map loop-through (cdr current_object)))))

	   ((eqv? (car current_object) 'x_SIMULTANEOUS)		(begin
								  ;; (format #t "the parallel list has ~a~% which has list-ref cadr ~a~%" (cdr current_object)   (list-ref   (cdr current_object) 0) )
								  ;;(pretty-print  (list-tail (cdr current_object) 1))
;;;FIXME the NEWCONTEXT can also appear at the top level, leading to repeated code. Also we are not looking to see what the new context is - just assuming Staff.
								  (if (eqv? 'NEWCONTEXT  (car (list-ref   (cdr current_object) 0)))
								      (string-append ";;;A new context?\n"  "(d-AddLast)\n" (string-join (map loop-through (list-tail (cdr current_object) 1))))
								      (string-append (start-chord current_object)  (string-join (map add-notes-to-chord (list-tail   (cdr current_object) 1))))
								      )))
	   
	   ((eqv? (car current_object) 'x_COMPOSITE_MUSIC)       (begin (format #t "hoping to process composite for ~a~%" (list-tail (cdr current_object) 0))   (string-join (map loop-through (list-tail (cdr current_object) 0)))))
	   
	   
	   (else
	    (begin (format #t "Not handled ~a~%" current_object) "recursive")   
	    (string-join (map loop-through current_object))
	   )
	   ))
	 (begin
    (cond

  




      ((eqv? (car current_object) 'x_CHORD)		(begin (format #t "hoping to process a note next for ~a~%" (list (cadr current_object)))   (string-join (map create-note (list (cadr current_object))))))




       (else (pretty-print current_object))      
						  
      )))))
  
(format #t "~%Final result ~a~%" (string-join (map loop-through list_from_parser)))
  )
