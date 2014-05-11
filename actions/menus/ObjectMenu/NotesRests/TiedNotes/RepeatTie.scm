;;RepeatTie
(let ((tag "RepeatTie"))
	(if (Note?)
	   (begin
		(if (d-Directive-chord? tag)
			(d-DirectiveDelete-chord tag)	
			(begin
				(d-DirectivePut-chord-postfix tag "\\repeatTie")
				(d-DirectivePut-chord-graphic tag "\n⌣
				Denemo
				30")))
		(d-RefreshDisplay)
		(d-SetSaved #f))))
