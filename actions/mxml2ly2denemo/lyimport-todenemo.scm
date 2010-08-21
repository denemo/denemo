(define (lyimport::convert_to_denemo list_from_parser)
	(define (eval_to_denemo current_object)
		(cond 
		((eqv? (car current_object) 'x_MOVEMENT)	(loop-through current_object))
		((eqv? (car current_object) 'x_LIST)		(loop-through current_object))
		((eqv? (car current_object) 'x_BARLINE)		#f)
		(else (pretty-print current_object))
		)		
	)

	(define (loop-through current_object)
		(define list_length (length current_object))
		
		; Now loop through the first layer of the list, each time eval_to_denemo the entry. nested lists will be handled in eval_to_denemo. 
		(let loop ((counter 1))
			(display counter)(display ": ")
			(if (>= counter list_length)
				(display "The End\n.")
				(begin ; Here begins the real action
					(eval_to_denemo (list-ref current_object counter))
					(loop (+ counter 1))
				)
			)
		)
	)
	

(loop-through list_from_parser)	

)
