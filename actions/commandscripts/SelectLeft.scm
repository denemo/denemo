;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
(if (and (not (d-MarkStatus)) (not (Appending?)))
	(begin (d-MoveCursorLeft) (d-SetMark))
	(d-CursorLeft))