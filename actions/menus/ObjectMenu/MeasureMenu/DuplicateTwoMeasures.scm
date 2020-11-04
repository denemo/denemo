;;;DuplicateTwoMeasures
(if (d-MeasureLeft)
	(begin
		(d-PushClipboard)
		(d-UnsetMark)
		(d-MeasureRight)
		(if (d-MeasureRight)
			(d-CursorLeft)
			(while (d-CursorRight)))
		(d-Copy)
		(if (d-MoveToMeasureRight)
			(if (not (None?))
				(d-InsertMeasureBefore)); create empty measure	
			(begin ; End of staff
				(d-AppendMeasureAllStaffs)
				(d-MoveToMeasureRight)))
		(d-Paste)
		(d-PopClipboard)))
		
(d-PlayMidiNote  72 127 9 100)
(d-OneShotTimer 200 "(d-PlayMidiNote  72 127 9 100)")
