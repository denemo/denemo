;;;StartTrillSpan
(let ((tag "StopTrillSpan"))
 (if (d-Directive-standalone? tag)
	(d-DirectiveDelete-standalone tag)
	(begin
		(if (d-MoveCursorLeft)
			(if (d-Directive-standalone? tag)
			  (d-DirectiveDelete-standalone tag)
				(d-MoveCursorRight)))
	(StandAloneDirectiveProto (cons "StopTrillSpan" "\\stopTrillSpan") #f LG-UpPrall)
	(d-DirectivePut-standalone-gx tag 10)
	(d-DirectivePut-standalone-grob tag tag)
	(d-MoveCursorRight)))
(d-RefreshDisplay)
(d-SetSaved #f))
		
