
	(let ((tag "Void"))
(cond
	((Music?)
		(if (d-Directive-chord? tag)
			(d-DirectiveDelete-chord tag)
			(begin
				(d-DirectivePut-chord-prefix tag "\\void ")
				(d-DirectivePut-chord-display tag "Void")))
		(d-SetSaved #f))
	((d-Directive-standalone?)
		(if (d-Directive-standalone? tag)
			(d-DirectiveDelete-standalone tag)
			(begin
				(d-DirectivePut-standalone tag)
				(d-DirectivePut-standalone-prefix tag "\\void ")
				(d-DirectivePut-standalone-display tag "Void")))
		(d-SetSaved #f))
	((Clef?)
		(if (d-Directive-clef? tag)
			(d-DirectiveDelete-clef tag)
			(begin
				(d-DirectivePut-clef-prefix tag "\\void ")
				(d-DirectivePut-clef-display tag "Void")))
		(d-SetSaved #f))
	((Timesignature?)
		(if (d-Directive-timesig? tag)
			(d-DirectiveDelete-timesig tag)
			(begin
				(d-DirectivePut-timesig-prefix tag "\\void ")
				(d-DirectivePut-timesig-display tag "Void")))
		(d-SetSaved #f))
	((Keysignature?)
		(if (d-Directive-keysig? tag)
			(d-DirectiveDelete-keysig tag)
			(begin
				(d-DirectivePut-keysig-prefix tag "\\void ")
				(d-DirectivePut-keysig-display tag "Void")))
		(d-SetSaved #f))))
