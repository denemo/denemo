;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;SelectionToEmptyMeasure
(if (not (None?))
(begin
	(d-CursorRight)
	(let gotoEnd ()
 	(if  (and (d-CursorRight) (not (None?)))
		(gotoEnd)))
	(d-RefreshDisplay)))