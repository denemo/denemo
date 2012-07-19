(if (and (not (d-MarkStatus)) (not (Appending?)))
	(begin (d-MoveCursorLeft) (d-SetMark))
	(d-CursorLeft))