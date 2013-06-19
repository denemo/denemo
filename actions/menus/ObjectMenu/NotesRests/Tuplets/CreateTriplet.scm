;;;CreateTriplet
(if  (and (MidiInput?) (Appending?))
	(begin  
	    (d-MoveCursorLeft)
	    (if (Music?)
   		(let ((duration (d-GetNoteBaseDuration)))
			(d-StartTriplet)
			(d-MoveCursorRight)
  			(eval-string (string-append "(d-" (number->string duration) ")"))
        		(d-SetNonprinting)
   			(eval-string (string-append "(d-" (number->string duration) ")"))
        		(d-SetNonprinting)  			
			(d-EndTuplet))
		 (begin
		 	(d-MoveCursorRight)
		 	(d-ToggleTripleting))))
	(d-ToggleTripleting))
