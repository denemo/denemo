(define (lyimport::convert_to_denemo list_from_parser)
	(define list_length (length list_from_parser))

	(define (eval_to_denemo current_object)
		(cond 
		
		(else (pretty-print current_object))
		)
		
	)

	; Loop through the first layer of the list, each time eval_to_denemo the entry. nested lists will be handled in eval_to_denemo. 
	(let loop ((counter 0))
		(display counter)(display ": ")
		(if (>= counter list_length)
			(display "The End.")
		
			(begin ; Here begins the real action
				(eval_to_denemo (list-ref list_from_parser counter))
				(loop (+ counter 1))
			)
		)
	)
)
