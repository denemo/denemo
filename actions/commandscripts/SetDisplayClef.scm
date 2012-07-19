;;;SetDisplayClef
(let ((type (d-GetType)))
(if (not (string=? type "CLEF"))
	(begin
		(d-InsertClef)
		(d-MoveCursorLeft)))
(d-SetNonprinting (not (d-GetNonprinting)))
(d-SetSaved #f))
