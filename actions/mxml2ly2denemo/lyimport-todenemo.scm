(define the-script "")


(define (lyimport::convert_to_denemo list_from_parser)
  (define (loop-through current_object)
(format #t "~% ------------ ~% current object ~a which is list?~a~%pair?~a~%" current_object (list? current_object)  (pair? current_object))
    (cond 
     ((eqv? (car current_object) 'x_MOVEMENT)	(begin (set! the-script (string-append the-script "(d-NewMovement)"))
						       (format #t "the movement has ~a~% which is list? ~a~%" (cdr current_object) (list? (cdr current_object)))
							     (for-each loop-through (list-tail current_object 1))))


      ((eqv? (car current_object) 'x_SEQUENTIAL)		(begin
							 (format #t "the sequential list has ~a~% which is list? ~a~%" (cdr current_object) (list? (cdr current_object)))
							  (for-each loop-through (cdr current_object))))
      ((eqv? (car current_object) 'x_CHORD)		(begin (format #t "hoping to process a note next for ~a~%" (list (cadr current_object)))   (for-each loop-through (list (cadr current_object))       ))

       )
      
      ((eqv? (car current_object) 'x_NOTE)          (begin (format #t "process a note ~a~%" (list? (cdr current_object)) ) (set! the-script (string-append the-script "(d-" (list-ref (cdr current_object) 0) ")"))))
       (else (pretty-print current_object))       
						  
      )
    )
  (for-each loop-through list_from_parser)
(format #t "~%Final result ~a~%" the-script)
  )
