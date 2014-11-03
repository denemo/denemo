;;;Caesura
(let ((tag "Caesura"))
 (if (d-Directive-standalone? tag)
	(d-DirectiveDelete-standalone tag)
	(begin
		(if (d-MoveCursorLeft)
			(if (d-Directive-standalone? tag)
			  (d-DirectiveDelete-standalone tag)
				(d-MoveCursorRight)))
	(StandAloneDirectiveProto (cons tag "\\once \\override BreathingSign.text= \\markup \\musicglyph #\"scripts.caesura.straight\" \\breathe") #f "\n//\nDenemo\n24")
	(d-DirectivePut-standalone-gy tag -44)
	(d-DirectivePut-standalone-grob tag "BreathingSign")
	(d-MoveCursorRight)))
(d-RefreshDisplay)
(d-SetSaved #f))
		
