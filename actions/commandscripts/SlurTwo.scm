;;;SlurTwo
(if (Music?)
(begin
	(d-ToggleBeginSlur)
	(if (d-NextChord)
		(d-ToggleEndSlur)
		(d-ToggleBeginSlur))
	(d-NextChord)))
