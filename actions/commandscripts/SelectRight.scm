;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
(if (and (not (d-MarkStatus)) (not (Appending?)))
	(d-SetMark)
	(d-CursorRight))