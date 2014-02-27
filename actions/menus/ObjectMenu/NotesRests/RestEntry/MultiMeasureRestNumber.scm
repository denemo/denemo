;;;MultiMeasureRestNumber
(let ((tag "MultiMeasureRestNumber"))
 (if (d-Directive-standalone? tag)
	(d-EditSimilar)
	(begin
		(if (d-MoveCursorLeft)
			(if (d-Directive-standalone? tag)
			  (d-EditSimilar)
				(d-MoveCursorRight)))
	(StandAloneDirectiveProto (cons tag "\\once \\override MultiMeasureRestNumber #'transparent = ##t ") #f "\nNx\nDenemo\n48")
	(d-DirectivePut-standalone-gy tag -44)
	(d-DirectivePut-standalone-grob tag tag)
	(d-MoveCursorRight)))
(d-RefreshDisplay)
(d-SetSaved #f))
		
