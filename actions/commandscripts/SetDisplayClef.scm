;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;SetDisplayClef
(let ((type (d-GetType)))
(if (not (string=? type "CLEF"))
	(begin
		(d-InsertClef)
		(d-MoveCursorLeft)))
(d-SetNonprinting (not (d-GetNonprinting)))
(d-SetSaved #f))
