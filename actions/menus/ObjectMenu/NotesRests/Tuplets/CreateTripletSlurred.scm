;;;CreateTripletSlurred
(if (and (MidiInput?) (Appending?))
	(begin
	    (d-MoveCursorLeft)
	    (if (Music?)
   		(let ((duration (d-GetNoteBaseDuration)))
			(d-StartTriplet)
			(d-MoveCursorRight)
			(d-ToggleBeginSlur)
  			(eval-string (string-append "(d-" (number->string duration) ")"))
        		(d-SetNonprinting)
   			(eval-string (string-append "(d-" (number->string duration) ")"))
        		(d-SetNonprinting)    
        		(d-ToggleEndSlur)		
			(d-EndTuplet))
		 (begin
		 	(d-MoveCursorRight)
		 	(d-ToggleTripleting))))
	(d-ToggleTripleting))
