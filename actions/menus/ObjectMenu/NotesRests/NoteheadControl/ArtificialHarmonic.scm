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
			  	 (d-AddNoteToChord)
				  (d-DirectivePut-note-postfix tag "\\harmonic ")
				  (d-DirectivePut-note-override tag DENEMO_OVERRIDE_GRAPHIC)
				  (d-DirectivePut-note-gx tag -10)
				  (d-DirectivePut-note-graphic tag "\n\nemmentaler\n36"))))