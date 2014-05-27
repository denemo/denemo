;;;Cresc
(if (Music?)
	(let ((tag "Cresc"))
	(if (d-Directive-chord? tag)
		(d-DirectiveDelete-chord tag)
		(begin
			(d-DirectivePut-chord-postfix tag  "\\cresc")
			(d-DirectivePut-chord-display tag  "cresc.")))
	(d-SetSaved #f)))
;;;End of scheme script