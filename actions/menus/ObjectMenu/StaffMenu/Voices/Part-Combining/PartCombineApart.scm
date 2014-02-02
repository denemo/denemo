;;;partcombineApart
(let ((tag "PartCombineApart")(help  (_ "This directive instructs the LilyPond typesetter not to combine notes from the voices when part combining has been set..\nUse with the Part Combine command.\nYou can delete the directive as any other object.")))
 (if (d-Directive-standalone? tag)
	(d-InfoDialog help)
	(begin
		(if (d-MoveCursorLeft)
			(if (d-Directive-standalone? tag)
			 	 (d-InfoDialog help)
				(d-MoveCursorRight)))
	(StandAloneDirectiveProto (cons tag "\\partcombineApart") #f "\npX\nDenemo\n48")
	(d-DirectivePut-standalone-gy tag -44)
	(d-DirectivePut-standalone-grob tag tag)
	(d-MoveCursorRight)))
(d-RefreshDisplay)
(d-SetSaved #f))
		
