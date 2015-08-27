;;;MultiMeasureRestNumber
(let ((tag "MultiMeasureRestNumber"))
	(if (d-MoveCursorLeft)
		(if (not (d-Directive-standalone? tag))
			(d-MoveCursorRight)))
	 (if (d-Directive-standalone? tag)
		(d-EditSimilar)
		(begin
			(StandAloneDirectiveProto (cons tag "\\once \\override MultiMeasureRestNumber #'transparent = ##t ") #f "\nNx\nDenemo\n48")
			(d-DirectivePut-standalone-gy tag -44)
			(d-DirectivePut-standalone-grob tag tag)
			(d-RefreshDisplay)
			(d-SetSaved #f)
			(d-MoveCursorRight))))
	
		
