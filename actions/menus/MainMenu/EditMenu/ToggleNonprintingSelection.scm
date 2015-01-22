;;;SetSelectionNonprinting
(d-PushPosition)
(d-GoToMark)
(let loop()
	(if (d-IsInSelection)
		(d-SetNonprinting (not (d-GetNonprinting))))
	(if (d-MoveCursorRight)
		(loop)))
(d-PopPosition)