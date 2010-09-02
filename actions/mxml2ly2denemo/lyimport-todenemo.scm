


(define (lyimport::convert_to_denemo list_from_parser)

  (define (create-chord current_object)
   (cond 
     ((eqv? (car  current_object) 'x_CHORD)		(begin   (string-join (map create-chord (list (cadr current_object)))))
       )
     ((eqv? (car current_object) 'x_NOTE)          (begin (string-append "(d-AddNote" (list-ref (cdr current_object) 0) ")")))
     (else "(d-WarningDialog \"%unhandled chord type\n\")")   
    ))


  (define (create-note current_object)
   (cond 
     ((eqv? (car  current_object) 'x_CHORD)		(begin   (string-join (map create-note (list (cadr current_object)))))
       )
     ((eqv? (car current_object) 'x_NOTE)          (begin (string-append "(d-" (list-ref (cdr current_object) 0) ")")))
     (else "(d-WarningDialog \"%unhandled note type\n\")")   
    ))




  (define (loop-through current_object)
(format #t "~% ------------ ~% current object ~a which is list?~a~%pair?~a~%" current_object (list? current_object)  (pair? current_object))
    (cond 
     ((eqv? (car current_object) 'x_MOVEMENT)	(begin
						       (format #t "the movement has ~a~% which is list? ~a~%" (cdr current_object) (list? (cdr current_object)))
							     (string-append  "(d-NewMovement)" (string-join (map loop-through (list-tail current_object 1))))))


      ((eqv? (car current_object) 'x_SEQUENTIAL)		(begin
							 (format #t "the sequential list has ~a~% which is list? ~a~%" (cdr current_object) (list? (cdr current_object)))
							(string-join (map loop-through (cdr current_object)))))


      ((eqv? (car current_object) 'x_SIMULTANEOUS)		(begin
							 (format #t "the parallel list has ~a~% which is list? ~a~%" (cdr current_object) (list? (cdr current_object)))
							(string-join (map create-chord (cdr current_object)))))


      ((eqv? (car current_object) 'x_CHORD)		(begin (format #t "hoping to process a note next for ~a~%" (list (cadr current_object)))   (string-join (map create-note (list (cadr current_object)))))

       )
       (else (pretty-print current_object))       
						  
      )
    )
  
(format #t "~%Final result ~a~%" (string-join (map loop-through list_from_parser)))
  )
