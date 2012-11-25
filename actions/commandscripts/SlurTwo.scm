;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;SlurTwo
(if (Music?)
(begin
	(d-ToggleBeginSlur)
	(if (d-NextChord)
		(d-ToggleEndSlur)
		(d-ToggleBeginSlur))
	(d-NextChord)))
