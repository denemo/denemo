;;;SmallNotehead
(let ((tag "SmallNotehead"))
	(if (d-Directive-note? tag)
		(d-DirectiveDelete-note tag)
		(begin
		(d-DirectivePut-note-display tag "Small")
		
		(d-DirectivePut-note-prefix tag "\\small ")))
	(d-SetSaved #f))

