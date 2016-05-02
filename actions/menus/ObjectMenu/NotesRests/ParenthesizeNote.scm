;;; ParenthesizeNote
 (let ((tag  "ParenthesizeNote"))
 (if (Note?)
 	(begin
	   (if (d-Directive-note? tag)
	 	(let ((choice (RadioBoxMenu 
		        (cons (_ "Object Inspector") 'help) 
		        (cons (_ "Delete") 'delete))))
		    (case choice
		        ((help)
		           (d-DisplayCurrentObject))
		          ((delete)
		            (d-DirectiveDelete-note tag)))) 
	 	(begin
			(d-DirectivePut-note-prefix tag "\\parenthesize ")
			(d-DirectivePut-note-display tag "()")))
	  (d-SetSaved #f))
      (begin
      	(if (Music?)
      		(d-ParenthesizeChord)
      		(begin
      			(d-WarningDialog (_ "Applies to Note, Chord or Rest")))))))
 