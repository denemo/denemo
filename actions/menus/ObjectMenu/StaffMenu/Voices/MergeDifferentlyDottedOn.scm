;;;MergeDifferentlyDottedOn
(let ((tag "MergeDifferentlyDotted"))
 (if (d-Directive-standalone? tag)
	(d-DirectiveDelete-standalone tag)
	(begin
		(if (d-MoveCursorLeft)
			(if (d-Directive-standalone? tag)
			  (d-DirectiveDelete-standalone tag)
				(d-MoveCursorRight)))
	(StandAloneDirectiveProto (cons tag "\\mergeDifferentlyDottedOn ") #f "\n÷\nDenemo\n48")
	(d-DirectivePut-standalone-gy tag -44)
	(d-DirectivePut-standalone-grob tag tag)
	(d-MoveCursorRight)))
(d-RefreshDisplay)
(d-SetSaved #f))
		
