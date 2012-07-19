(if (and (not (d-MarkStatus)) (not (Appending?)))
	(d-SetMark)
	(d-CursorRight))