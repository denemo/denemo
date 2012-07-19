(if (None?)
	(d-SetMark)
	(begin
		(d-PushPosition)
		(d-UnsetMark)
		(GoToMeasureBeginning)
		(RepeatProcWhileTest d-CursorRight (lambda () (not (Appending?))))
		(d-PopPosition)))