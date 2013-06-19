;;;Dottify
(if (Appending?)
	(d-MoveCursorLeft))
(if (Note?)
	(begin
		(d-Diminish)
		(d-SetMark)
		(d-Copy)
		(d-AddDot)
		(d-MoveCursorRight)
		(d-Paste)
		(d-MoveCursorLeft)
		(d-Diminish)
		(d-MoveCursorRight)))
