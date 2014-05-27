;;;Dim
(if (Music?)
	(let ((tag "Dim"))
	(if (d-Directive-chord? tag)
		(d-DirectiveDelete-chord tag)
		(begin
			(d-DirectivePut-chord-postfix tag  "\\dim")
			(d-DirectivePut-chord-display tag  "dim.")))
	(d-SetSaved #f)))
;;;End of scheme script