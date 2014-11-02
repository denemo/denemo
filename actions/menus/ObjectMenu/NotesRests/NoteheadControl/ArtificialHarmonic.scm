;;;;;;ArtificialHarmonic
(let ((tag "ArtificialHarmonic")
        (params ArtificialHarmonic::params))
    (if (or (equal? params "edit") (d-Directive-note? tag))
    	   (let ((choice (RadioBoxMenu (cons (_ "Help") 'help) (cons (_ "Delete") 'delete))))
    	   	(case choice
    	   		((help)
    	   			(d-InfoDialog (_ "This note will typeset with a diamond notehead, to indicate a harmonic\nYou can sharpen or flatten this note using the standard commands.")))
    	   		((delete)
    	   			(d-RemoveNoteFromChord))))
    	   (begin
    	   	(if (or (Appending?) (EmptyMeasure?))
		    	   (let ((steps (d-GetUserInput (_ "Artificial Harmonic") (_ "Give interval above stopped note") "4")))	
			    (if (and steps (string->number steps))
				(begin
				    (set! steps (1- (string->number steps)))
				    (if (and (> steps 0) (< steps 20))
					(begin
					    (d-InsertOneNote)
					    (d-MoveCursorLeft)
					    (d-Chordize)
					    (let loop ((count steps))
						(if (positive? count)
						    (begin
						        (d-CursorUp)
						        (loop (1- count)))))))))))
		(if (Singlenote?)
			  (begin
			  	 (d-AddNoteToChord)
				  (d-DirectivePut-note-postfix tag "\\harmonic ")
				  (d-DirectivePut-note-override tag DENEMO_OVERRIDE_GRAPHIC)
				  (d-DirectivePut-note-gx tag -10)
				  (d-DirectivePut-note-graphic tag "\n\nemmentaler\n36"))))))