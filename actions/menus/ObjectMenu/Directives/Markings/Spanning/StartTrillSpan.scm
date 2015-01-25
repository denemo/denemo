;;;StartTrillSpan
(let ((tag "StartTrillSpan"))
 (if (d-Directive-standalone? tag)
	(d-DirectiveDelete-standalone tag)
	(begin
		(if (d-MoveCursorLeft)
			(if (d-Directive-standalone? tag)
			  (d-DirectiveDelete-standalone tag)
				(d-MoveCursorRight)))
	(StandAloneDirectiveProto (cons "StartTrillSpan" "\\startTrillSpan") #f LG-Prall)
	(d-DirectivePut-standalone-gx tag 10)
	(d-DirectivePut-standalone-grob tag tag)
	(d-MoveCursorRight)))
(d-RefreshDisplay)
(d-SetSaved #f))
		
