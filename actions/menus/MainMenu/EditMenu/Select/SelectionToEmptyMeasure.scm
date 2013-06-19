;;;SelectionToEmptyMeasure
(if (not (None?))
(begin
	(d-CursorRight)
	(let gotoEnd ()
 	(if  (and (d-CursorRight) (not (None?)))
		(gotoEnd)))
	(d-RefreshDisplay)))